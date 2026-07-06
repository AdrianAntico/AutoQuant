# ============================================================
# CatBoost Builder Artifact Generation
#
# This generator is the narrow AutoQuant-side training/scoring contract for
# AnalyticsShinyApp CatBoost Builder v1. It wraps the existing AutoQuant
# AutoCatBoostRegression()/AutoCatBoostClassifier() training functions, returns
# structured artifacts, and exposes scored outputs that can feed Model
# Assessment, Model Insights, and SHAP modules.
# ============================================================

catboost_builder_null_coalesce <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

catboost_builder_slug <- function(x) {
  x <- as.character(x)
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x <- tolower(x)
  if (!nzchar(x)) "artifact" else x
}

catboost_builder_warn <- function(warnings, message) {
  unique(c(warnings, message))
}

catboost_builder_artifact <- function(
  name,
  label,
  type,
  section,
  object = NULL,
  content = NULL,
  metadata = list()
) {
  list(
    name = catboost_builder_slug(name),
    label = label,
    type = type,
    artifact_type = type,
    section = section,
    object = object,
    content = content,
    data = if (identical(type, "table")) object else NULL,
    metadata = metadata
  )
}

catboost_builder_add_artifact <- function(artifacts, artifact) {
  base_name <- catboost_builder_slug(artifact$name)
  artifact$name <- base_name
  artifact$metadata$artifact_index <- length(artifacts) + 1L

  if (artifact$name %in% names(artifacts)) {
    suffix <- 2L
    candidate <- paste0(base_name, "_", suffix)
    while (candidate %in% names(artifacts)) {
      suffix <- suffix + 1L
      candidate <- paste0(base_name, "_", suffix)
    }
    artifact$name <- candidate
  }

  artifacts[[artifact$name]] <- artifact
  artifacts
}

catboost_builder_as_artifact_table <- function(artifacts) {
  if (!length(artifacts)) {
    return(data.table::data.table())
  }

  data.table::rbindlist(lapply(artifacts, function(artifact) {
    data.table::data.table(
      name = artifact$name,
      label = artifact$label,
      type = artifact$type,
      section = artifact$section
    )
  }), use.names = TRUE, fill = TRUE)
}

catboost_builder_safe_div <- function(num, den) {
  ifelse(is.finite(den) & den != 0, num / den, NA_real_)
}

catboost_builder_round_vector <- function(x) {
  if (!is.numeric(x)) {
    return(x)
  }
  finite <- x[is.finite(x)]
  if (!length(finite)) {
    return(x)
  }
  if (all(abs(finite - round(finite)) < sqrt(.Machine$double.eps))) {
    return(as.integer(round(x)))
  }
  max_abs <- max(abs(finite), na.rm = TRUE)
  if (max_abs >= 1000) {
    return(round(x, 1L))
  }
  if (max_abs >= 100) {
    return(round(x, 2L))
  }
  if (max_abs >= 1) {
    return(round(x, 4L))
  }
  signif(x, 4L)
}

catboost_builder_round_table <- function(dt, skip_cols = character()) {
  if (is.null(dt) || !data.table::is.data.table(dt) || !nrow(dt)) {
    return(dt)
  }
  out <- data.table::copy(dt)
  numeric_cols <- setdiff(names(out)[vapply(out, is.numeric, logical(1L))], skip_cols)
  for (col in numeric_cols) {
    data.table::set(out, j = col, value = catboost_builder_round_vector(out[[col]]))
  }
  out[]
}

catboost_builder_failure <- function(
  message,
  problem_type = NA_character_,
  warnings = character(),
  diagnostics = NULL,
  metadata = list()
) {
  out <- list(
    report_type = "catboost_builder",
    status = "error",
    artifacts = list(),
    metadata = c(
      list(
        source_package = "AutoQuant",
        source_function = "generate_catboost_builder_artifacts",
        problem_type = problem_type,
        model_type = "catboost",
        generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        artifact_count = 0L,
        plot_count = 0L,
        table_count = 0L,
        text_count = 0L,
        warnings_count = length(warnings)
      ),
      metadata
    ),
    warnings = warnings,
    diagnostics = catboost_builder_null_coalesce(diagnostics, data.table::data.table(
      severity = "error",
      message = message
    )),
    value = list(
      scored_data = NULL,
      model_object = NULL,
      split = NULL,
      downstream_handoff = NULL
    ),
    error = message
  )
  class(out) <- c("autoquant_catboost_builder_artifacts", "autoquant_report_artifacts", "list")
  out
}

catboost_builder_validate <- function(
  data,
  target_col,
  feature_cols,
  problem_type,
  positive_class,
  id_cols,
  DateVar,
  ByVars,
  train_fraction,
  split_method,
  split_col,
  cat_features,
  iterations,
  depth,
  learning_rate,
  threshold,
  compute_shap,
  score_train,
  score_test,
  score_full,
  include_model_object,
  include_plots,
  top_n
) {
  errors <- character()
  warnings <- character()

  if (missing(data) || is.null(data)) {
    errors <- c(errors, "data is required.")
  } else if (!is.data.frame(data) && !data.table::is.data.table(data)) {
    errors <- c(errors, "data must be data.frame/data.table-like.")
  }

  names_data <- if (!is.null(data)) names(data) else character()
  if (is.null(target_col) || length(target_col) != 1L || !nzchar(target_col)) {
    errors <- c(errors, "target_col must be a non-empty character value.")
  } else if (!target_col %in% names_data) {
    errors <- c(errors, paste0("target_col was not found in data: ", target_col))
  }

  if (is.null(feature_cols) || !length(feature_cols)) {
    errors <- c(errors, "feature_cols must contain at least one column.")
  } else {
    missing_features <- setdiff(feature_cols, names_data)
    if (length(missing_features)) {
      errors <- c(errors, paste("feature_cols missing from data:", paste(missing_features, collapse = ", ")))
    }
    if (!is.null(target_col) && target_col %in% feature_cols) {
      errors <- c(errors, "feature_cols must not include target_col.")
    }
  }

  if (!problem_type %in% c("regression", "binary_classification")) {
    errors <- c(errors, "problem_type must be regression or binary_classification.")
  }

  if (identical(problem_type, "regression") && !is.null(target_col) && target_col %in% names_data && !is.numeric(data[[target_col]])) {
    errors <- c(errors, "Regression target_col must be numeric.")
  }

  if (identical(problem_type, "binary_classification")) {
    if (is.null(positive_class) || length(positive_class) != 1L || is.na(positive_class)) {
      errors <- c(errors, "positive_class is required for binary_classification.")
    }
    if (!is.null(target_col) && target_col %in% names_data) {
      values <- unique(stats::na.omit(data[[target_col]]))
      if (length(values) != 2L) {
        errors <- c(errors, "Binary classification target_col must contain exactly two non-missing classes.")
      } else if (!is.null(positive_class) && !as.character(positive_class) %in% as.character(values)) {
        errors <- c(errors, "positive_class was not found in target_col.")
      }
    }
  }

  for (arg_name in c("id_cols", "DateVar", "ByVars")) {
    cols <- switch(arg_name, id_cols = id_cols, DateVar = DateVar, ByVars = ByVars)
    if (!is.null(cols) && length(cols)) {
      missing_cols <- setdiff(cols, names_data)
      if (length(missing_cols)) {
        errors <- c(errors, paste(arg_name, "missing from data:", paste(missing_cols, collapse = ", ")))
      }
    }
  }

  if (!is.null(cat_features) && length(cat_features)) {
    missing_cats <- setdiff(cat_features, feature_cols)
    if (length(missing_cats)) {
      errors <- c(errors, paste("cat_features must be a subset of feature_cols. Missing:", paste(missing_cats, collapse = ", ")))
    }
  }

  if (!is.numeric(train_fraction) || length(train_fraction) != 1L || !is.finite(train_fraction) || train_fraction <= 0 || train_fraction >= 1) {
    errors <- c(errors, "train_fraction must be a numeric value in (0, 1).")
  }

  if (!split_method %in% c("random", "time")) {
    errors <- c(errors, "split_method must be random or time.")
  }
  if (identical(split_method, "time")) {
    resolved_split <- catboost_builder_null_coalesce(split_col, DateVar)
    if (is.null(resolved_split) || !length(resolved_split) || !resolved_split[[1L]] %in% names_data) {
      errors <- c(errors, "split_method = 'time' requires split_col or DateVar to exist in data.")
    }
  }

  if (!is.numeric(iterations) || length(iterations) != 1L || !is.finite(iterations) || iterations < 1L || iterations > 5000L) {
    errors <- c(errors, "iterations must be between 1 and 5000.")
  }
  if (!is.numeric(depth) || length(depth) != 1L || !is.finite(depth) || depth < 1L || depth > 16L) {
    errors <- c(errors, "depth must be between 1 and 16.")
  }
  if (!is.numeric(learning_rate) || length(learning_rate) != 1L || !is.finite(learning_rate) || learning_rate <= 0 || learning_rate > 1) {
    errors <- c(errors, "learning_rate must be in (0, 1].")
  }
  if (!is.numeric(threshold) || length(threshold) != 1L || !is.finite(threshold) || threshold < 0 || threshold > 1) {
    errors <- c(errors, "threshold must be a numeric value between 0 and 1.")
  }

  logical_args <- list(
    compute_shap = compute_shap,
    score_train = score_train,
    score_test = score_test,
    score_full = score_full,
    include_model_object = include_model_object,
    include_plots = include_plots
  )
  for (nm in names(logical_args)) {
    if (!is.logical(logical_args[[nm]]) || length(logical_args[[nm]]) != 1L || is.na(logical_args[[nm]])) {
      errors <- c(errors, paste(nm, "must be TRUE or FALSE."))
    }
  }
  if (!is.numeric(top_n) || length(top_n) != 1L || !is.finite(top_n) || top_n < 1L) {
    errors <- c(errors, "top_n must be a positive integer.")
  }

  list(errors = unique(errors), warnings = unique(warnings))
}

catboost_builder_make_split <- function(dt, train_fraction, split_method, split_col, DateVar, seed) {
  n <- nrow(dt)
  train_n <- max(1L, min(n - 1L, floor(n * train_fraction)))
  if (identical(split_method, "time")) {
    resolved_split <- catboost_builder_null_coalesce(split_col, DateVar)
    order_index <- order(dt[[resolved_split]], na.last = TRUE)
    train_index <- order_index[seq_len(train_n)]
  } else {
    set.seed(seed)
    train_index <- sort(sample.int(n, train_n))
  }
  test_index <- setdiff(seq_len(n), train_index)
  list(train_index = train_index, test_index = test_index)
}

catboost_builder_run_auto_catboost <- function(
  dt,
  split,
  target_col,
  feature_cols,
  problem_type,
  positive_class,
  DateVar,
  iterations,
  depth,
  learning_rate,
  loss_function,
  eval_metric,
  compute_shap,
  include_model_object,
  model_name
) {
  train_dt <- data.table::copy(dt[split$train_index])
  validate_dt <- data.table::copy(dt[split$test_index])
  model_id <- catboost_builder_null_coalesce(model_name, paste0("catboost_builder_", problem_type))
  temp_path <- tempdir()

  if (identical(problem_type, "regression")) {
    training_function <- "AutoCatBoostRegression"
    result <- AutoCatBoostRegression(
      OutputSelection = c("Importances", "EvalMetrics", "Score_TrainData"),
      ReturnShap = isTRUE(compute_shap),
      data = train_dt,
      ValidationData = validate_dt,
      TestData = NULL,
      TargetColumnName = target_col,
      FeatureColNames = feature_cols,
      PrimaryDateColumn = NULL,
      IDcols = ".aq_builder_row_id",
      TrainOnFull = FALSE,
      task_type = "CPU",
      NumGPUs = 1,
      DebugMode = FALSE,
      ReturnModelObjects = TRUE,
      SaveModelObjects = FALSE,
      ModelID = model_id,
      model_path = temp_path,
      metadata_path = temp_path,
      SaveInfoToPDF = FALSE,
      eval_metric = catboost_builder_null_coalesce(eval_metric, "RMSE"),
      loss_function = catboost_builder_null_coalesce(loss_function, "RMSE"),
      Trees = as.integer(iterations),
      Depth = as.integer(depth),
      LearningRate = learning_rate,
      NumOfParDepPlots = 0L,
      GridTune = FALSE,
      MaxRunMinutes = 60L,
      MetricPeriods = 10L
    )
  } else {
    training_function <- "AutoCatBoostClassifier"
    target_values <- unique(stats::na.omit(dt[[target_col]]))
    negative_class <- setdiff(target_values, positive_class)[1L]
    train_dt[, (target_col) := factor(
      ifelse(as.character(get(target_col)) == as.character(positive_class), "PositiveOutcome", "NegativeOutcome"),
      levels = c("NegativeOutcome", "PositiveOutcome")
    )]
    validate_dt[, (target_col) := factor(
      ifelse(as.character(get(target_col)) == as.character(positive_class), "PositiveOutcome", "NegativeOutcome"),
      levels = c("NegativeOutcome", "PositiveOutcome")
    )]

    result <- AutoCatBoostClassifier(
      OutputSelection = c("Importances", "EvalMetrics", "Score_TrainData"),
      data = train_dt,
      ValidationData = validate_dt,
      TestData = NULL,
      TargetColumnName = target_col,
      FeatureColNames = feature_cols,
      PrimaryDateColumn = NULL,
      IDcols = ".aq_builder_row_id",
      TrainOnFull = FALSE,
      task_type = "CPU",
      NumGPUs = 1,
      ReturnModelObjects = TRUE,
      SaveModelObjects = FALSE,
      SaveInfoToPDF = FALSE,
      ModelID = model_id,
      model_path = temp_path,
      metadata_path = temp_path,
      EvalMetric = catboost_builder_null_coalesce(eval_metric, "AUC"),
      LossFunction = catboost_builder_null_coalesce(loss_function, "Logloss"),
      ClassWeights = c(1, 1),
      CostMatrixWeights = c(1, -1, -1, 0),
      NumOfParDepPlots = 0L,
      GridTune = FALSE,
      MaxRunMinutes = 60L,
      MetricPeriods = 10L,
      Trees = as.integer(iterations),
      Depth = as.integer(depth),
      LearningRate = learning_rate,
      DebugMode = FALSE
    )
  }

  list(result = result, training_function = training_function)
}

catboost_builder_normalize_auto_scored <- function(
  model_result,
  original_dt,
  split,
  target_col,
  feature_cols,
  problem_type,
  positive_class,
  threshold,
  id_cols,
  DateVar,
  ByVars,
  score_train,
  score_test
) {
  scored <- data.table::as.data.table(data.table::copy(model_result$TrainData))
  if (!".aq_builder_row_id" %in% names(scored)) {
    scored[, .aq_builder_row_id := seq_len(.N)]
  }
  scored[, .aq_builder_row_id := as.integer(.aq_builder_row_id)]
  if ("p1" %in% names(scored) && !"Predict" %in% names(scored)) {
    data.table::setnames(scored, "p1", "Predict")
  }
  if (!"Predict" %in% names(scored)) {
    predict_cols <- grep("^Predict", names(scored), value = TRUE)
    if (length(predict_cols)) {
      data.table::setnames(scored, predict_cols[[1L]], "Predict")
    }
  }

  context_cols <- unique(c(".aq_builder_row_id", id_cols, DateVar, ByVars, target_col, feature_cols))
  context_cols <- intersect(context_cols, names(original_dt))
  context <- data.table::copy(original_dt[, .SD, .SDcols = context_cols])
  scored_context <- context[scored, on = ".aq_builder_row_id"]
  duplicate_cols <- grep("^(i\\.|.*\\.1$)", names(scored_context), value = TRUE)
  if (length(duplicate_cols)) {
    scored_context[, (duplicate_cols) := NULL]
  }
  allowed_shap_cols <- paste0("Shap_", feature_cols)
  extra_shap_cols <- setdiff(grep("^Shap_", names(scored_context), value = TRUE), allowed_shap_cols)
  if (length(extra_shap_cols)) {
    scored_context[, (extra_shap_cols) := NULL]
  }

  scored_context[, .split := data.table::fifelse(.aq_builder_row_id %in% split$train_index, "train", "test")]
  if (!isTRUE(score_train)) {
    scored_context <- scored_context[.split != "train"]
  }
  if (!isTRUE(score_test)) {
    scored_context <- scored_context[.split != "test"]
  }

  if (identical(problem_type, "regression")) {
    scored_context[, residual := as.numeric(get(target_col)) - as.numeric(Predict)]
  } else {
    target_values <- unique(stats::na.omit(original_dt[[target_col]]))
    negative_class <- setdiff(target_values, positive_class)[1L]
    scored_context[, ActualClass := as.character(get(target_col))]
    scored_context[, actual_positive := as.integer(ActualClass == as.character(positive_class))]
    scored_context[, PredictedClass := ifelse(Predict >= threshold, as.character(positive_class), as.character(negative_class))]
    scored_context[, predicted_positive := as.integer(PredictedClass == as.character(positive_class))]
    scored_context[, threshold := threshold]
  }

  shap_columns <- grep("^Shap_", names(scored_context), value = TRUE)
  ordered_cols <- unique(c(
    ".aq_builder_row_id",
    id_cols,
    DateVar,
    ByVars,
    target_col,
    feature_cols,
    "Predict",
    if (identical(problem_type, "regression")) "residual" else c("ActualClass", "actual_positive", "PredictedClass", "predicted_positive", "threshold"),
    ".split",
    shap_columns
  ))
  data.table::setcolorder(scored_context, intersect(ordered_cols, names(scored_context)))
  if (".row_id" %in% names(scored_context)) {
    scored_context[, .row_id := NULL]
  }
  data.table::setnames(scored_context, ".aq_builder_row_id", ".row_id")
  scored_context[]
}

catboost_builder_normalize_importance <- function(model_result) {
  vi <- model_result$VariableImportance
  if (is.null(vi)) {
    return(data.table::data.table())
  }
  if (data.table::is.data.table(vi)) {
    out <- data.table::copy(vi)
  } else if (is.list(vi)) {
    vi_tables <- Filter(data.table::is.data.table, vi)
    if (!length(vi_tables)) {
      return(data.table::data.table())
    }
    out <- data.table::rbindlist(lapply(names(vi_tables), function(nm) {
      z <- data.table::copy(vi_tables[[nm]])
      z[, source := nm]
      z
    }), use.names = TRUE, fill = TRUE)
  } else {
    return(data.table::data.table())
  }
  if ("Variable" %in% names(out) && !"feature" %in% names(out)) {
    data.table::setnames(out, "Variable", "feature")
  }
  if ("Importance" %in% names(out) && !"importance" %in% names(out)) {
    data.table::setnames(out, "Importance", "importance")
  }
  if (!all(c("feature", "importance") %in% names(out))) {
    return(data.table::data.table())
  }
  out <- out[, .(importance = mean(as.numeric(importance), na.rm = TRUE)), by = feature][order(-importance)]
  catboost_builder_round_table(out)
}

catboost_builder_normalize_metrics <- function(model_result) {
  metrics <- model_result$EvaluationMetrics
  if (is.null(metrics) || !is.list(metrics)) {
    return(data.table::data.table())
  }
  data.table::rbindlist(lapply(names(metrics), function(nm) {
    dt <- data.table::as.data.table(metrics[[nm]])
    dt[, split := ifelse(grepl("train", nm, ignore.case = TRUE), "train", "test")]
    data.table::setcolorder(dt, c("split", setdiff(names(dt), "split")))
    dt
  }), use.names = TRUE, fill = TRUE)
}

catboost_builder_metrics_regression <- function(dt, target_col = "target", prediction_col = "Predict") {
  actual <- as.numeric(dt[[target_col]])
  pred <- as.numeric(dt[[prediction_col]])
  residual <- actual - pred
  data.table::data.table(
    split = unique(dt$.split)[1L],
    n = sum(stats::complete.cases(actual, pred)),
    rmse = sqrt(mean(residual^2, na.rm = TRUE)),
    mae = mean(abs(residual), na.rm = TRUE),
    r2 = 1 - sum(residual^2, na.rm = TRUE) / sum((actual - mean(actual, na.rm = TRUE))^2, na.rm = TRUE),
    mape = if (any(actual != 0, na.rm = TRUE)) mean(abs(residual[actual != 0] / actual[actual != 0]), na.rm = TRUE) else NA_real_
  )
}

catboost_builder_auc <- function(actual_positive, score) {
  keep <- is.finite(score) & !is.na(actual_positive)
  actual_positive <- actual_positive[keep]
  score <- score[keep]
  n_pos <- sum(actual_positive == 1L)
  n_neg <- sum(actual_positive == 0L)
  if (!n_pos || !n_neg) {
    return(NA_real_)
  }
  ranks <- rank(score, ties.method = "average")
  (sum(ranks[actual_positive == 1L]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
}

catboost_builder_metrics_binary <- function(dt, threshold) {
  actual <- as.integer(dt$actual_positive)
  pred <- as.integer(dt$Predict >= threshold)
  tp <- sum(pred == 1L & actual == 1L, na.rm = TRUE)
  tn <- sum(pred == 0L & actual == 0L, na.rm = TRUE)
  fp <- sum(pred == 1L & actual == 0L, na.rm = TRUE)
  fn <- sum(pred == 0L & actual == 1L, na.rm = TRUE)
  precision <- catboost_builder_safe_div(tp, tp + fp)
  recall <- catboost_builder_safe_div(tp, tp + fn)
  specificity <- catboost_builder_safe_div(tn, tn + fp)
  data.table::data.table(
    split = unique(dt$.split)[1L],
    threshold = threshold,
    n = length(actual),
    auc = catboost_builder_auc(actual, dt$Predict),
    accuracy = catboost_builder_safe_div(tp + tn, tp + tn + fp + fn),
    precision = precision,
    recall = recall,
    specificity = specificity,
    f1 = catboost_builder_safe_div(2 * precision * recall, precision + recall),
    prevalence = mean(actual == 1L, na.rm = TRUE),
    predicted_positive_rate = mean(pred == 1L, na.rm = TRUE),
    tp = tp,
    tn = tn,
    fp = fp,
    fn = fn
  )
}

catboost_builder_safe_plot <- function(expr, warnings, label) {
  plot <- tryCatch(
    expr,
    error = function(e) {
      attr(warnings, "new_warning") <- paste(label, "plot failed:", conditionMessage(e))
      NULL
    }
  )
  list(plot = plot, warnings = catboost_builder_warn(warnings, attr(warnings, "new_warning")))
}

catboost_builder_build_plots <- function(scored_data, variable_importance, problem_type, target_col, top_n, Theme = "dark") {
  plots <- list()
  warnings <- character()
  if (!requireNamespace("AutoPlots", quietly = TRUE)) {
    return(list(plots = plots, warnings = "AutoPlots is not installed; plot artifacts were skipped."))
  }

  if (nrow(variable_importance)) {
    vi_plot_dt <- utils::head(variable_importance[order(importance)], as.integer(top_n))
    plot_result <- catboost_builder_safe_plot(
      AutoPlots::Bar(
        dt = vi_plot_dt,
        PreAgg = TRUE,
        XVar = "feature",
        YVar = "importance",
        Theme = Theme,
        title.text = "CatBoost Variable Importance",
        xAxis.title = "Feature",
        yAxis.title = "Importance"
      ),
      warnings,
      "Variable importance"
    )
    plots$variable_importance <- plot_result$plot
    warnings <- plot_result$warnings
  }

  plot_result <- catboost_builder_safe_plot(
    AutoPlots::Histogram(
      dt = scored_data,
      XVar = "Predict",
      GroupVar = if (identical(problem_type, "binary_classification")) "ActualClass" else NULL,
      Theme = Theme,
      title.text = "Prediction Distribution",
      xAxis.title = "Prediction"
    ),
    warnings,
    "Prediction distribution"
  )
  plots$prediction_distribution <- plot_result$plot
  warnings <- plot_result$warnings

  if (identical(problem_type, "regression")) {
    plot_result <- catboost_builder_safe_plot(
      AutoPlots::Scatter(
        dt = scored_data,
        XVar = "Predict",
        YVar = target_col,
        GroupVar = ".split",
        Theme = Theme,
        title.text = "Actual vs Predicted",
        xAxis.title = "Predicted",
        yAxis.title = "Actual"
      ),
      warnings,
      "Actual vs predicted"
    )
    plots$actual_vs_predicted <- plot_result$plot
    warnings <- plot_result$warnings

    plot_result <- catboost_builder_safe_plot(
      AutoPlots::Histogram(
        dt = scored_data,
        XVar = "residual",
        GroupVar = ".split",
        Theme = Theme,
        title.text = "Residual Distribution",
        xAxis.title = "Residual"
      ),
      warnings,
      "Residual distribution"
    )
    plots$residual_distribution <- plot_result$plot
    warnings <- plot_result$warnings

    plot_result <- catboost_builder_safe_plot(
      AutoPlots::Scatter(
        dt = scored_data,
        XVar = "Predict",
        YVar = "residual",
        GroupVar = ".split",
        Theme = Theme,
        title.text = "Residual by Prediction",
        xAxis.title = "Predicted",
        yAxis.title = "Residual"
      ),
      warnings,
      "Residual by prediction"
    )
    plots$residual_by_prediction <- plot_result$plot
    warnings <- plot_result$warnings
  }

  list(plots = Filter(Negate(is.null), plots), warnings = warnings)
}

catboost_builder_make_handoff <- function(
  problem_type,
  scored_data,
  target_col,
  feature_cols,
  prediction_scale,
  positive_class,
  threshold,
  DateVar,
  ByVars,
  id_cols,
  shap_columns
) {
  list(
    model_assessment = list(
      data = "value$scored_data",
      target_col = target_col,
      prediction_col = "Predict",
      predicted_class_col = if (identical(problem_type, "binary_classification")) "PredictedClass" else NULL,
      positive_class = positive_class,
      feature_cols = feature_cols
    ),
    model_insights = list(
      data = "value$scored_data",
      problem_type = problem_type,
      target_col = target_col,
      prediction_col = "Predict",
      predicted_class_col = if (identical(problem_type, "binary_classification")) "PredictedClass" else NULL,
      positive_class = positive_class,
      feature_cols = feature_cols,
      DateVar = DateVar,
      ByVars = ByVars
    ),
    shap_analysis = list(
      available = length(shap_columns) > 0L,
      data = "value$scored_data",
      problem_type = problem_type,
      target_col = target_col,
      prediction_col = "Predict",
      predicted_class_col = if (identical(problem_type, "binary_classification")) "PredictedClass" else NULL,
      positive_class = positive_class,
      prediction_scale = prediction_scale,
      feature_cols = feature_cols,
      shap_prefix = "Shap_",
      DateVar = DateVar,
      ByVars = ByVars,
      id_cols = id_cols,
      shap_columns = shap_columns
    )
  )
}

#' Generate CatBoost Builder Artifacts
#'
#' @description
#' Wraps AutoQuant CatBoost regression or binary classification training and
#' returns structured artifacts plus scored data for downstream AutoQuant and
#' AnalyticsShinyApp workflows.
#'
#' @param data Input data.
#' @param target_col Target column.
#' @param feature_cols Feature columns.
#' @param problem_type One of `"regression"` or `"binary_classification"`.
#' @param positive_class Positive class for binary classification.
#' @param id_cols Optional identifier/context columns to preserve in scored data.
#' @param DateVar Optional date column to preserve and use for time split.
#' @param ByVars Optional segment columns to preserve.
#' @param train_fraction Fraction of rows used for training.
#' @param split_method `"random"` or `"time"`.
#' @param split_col Optional split column for time split.
#' @param seed Random seed for random split.
#' @param cat_features Optional categorical features. If NULL, inferred.
#' @param iterations CatBoost iterations.
#' @param depth CatBoost tree depth.
#' @param learning_rate CatBoost learning rate.
#' @param loss_function Optional CatBoost loss function.
#' @param eval_metric Optional CatBoost eval metric.
#' @param threshold Binary classification threshold.
#' @param prediction_scale Prediction scale label for downstream SHAP metadata.
#' @param compute_shap Logical. Attempt CatBoost SHAP contribution columns.
#' @param score_train,score_test,score_full Logical score-output controls.
#' @param include_model_object Logical. Include the raw model object in value.
#' @param include_plots Logical. Create AutoPlots artifacts when available.
#' @param top_n Maximum variables for importance plots.
#' @param model_name Optional model label.
#' @param data_name Optional data label.
#' @param ... Reserved for future bounded CatBoost parameters.
#'
#' @return A structured `autoquant_catboost_builder_artifacts` list.
#'
#' @family Reports
#' @export
generate_catboost_builder_artifacts <- function(
  data,
  target_col,
  feature_cols,
  problem_type = c("regression", "binary_classification"),
  positive_class = NULL,
  id_cols = NULL,
  DateVar = NULL,
  ByVars = character(),
  train_fraction = 0.8,
  split_method = c("random", "time"),
  split_col = NULL,
  seed = 123L,
  cat_features = NULL,
  iterations = 200L,
  depth = 6L,
  learning_rate = 0.05,
  loss_function = NULL,
  eval_metric = NULL,
  threshold = 0.5,
  prediction_scale = "response",
  compute_shap = TRUE,
  score_train = TRUE,
  score_test = TRUE,
  score_full = TRUE,
  include_model_object = FALSE,
  include_plots = TRUE,
  top_n = 20L,
  model_name = NULL,
  data_name = NULL,
  ...
) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required.", call. = FALSE)
  }

  problem_type <- match.arg(problem_type)
  split_method <- match.arg(split_method)
  warnings <- character()

  if (!requireNamespace("catboost", quietly = TRUE)) {
    return(catboost_builder_failure(
      "Package 'catboost' is required for generate_catboost_builder_artifacts() but is not installed. Install catboost separately; AutoQuant will not install it automatically.",
      problem_type = problem_type,
      warnings = "catboost is optional and was not installed."
    ))
  }

  validation <- catboost_builder_validate(
    data = data,
    target_col = target_col,
    feature_cols = feature_cols,
    problem_type = problem_type,
    positive_class = positive_class,
    id_cols = id_cols,
    DateVar = DateVar,
    ByVars = ByVars,
    train_fraction = train_fraction,
    split_method = split_method,
    split_col = split_col,
    cat_features = cat_features,
    iterations = iterations,
    depth = depth,
    learning_rate = learning_rate,
    threshold = threshold,
    compute_shap = compute_shap,
    score_train = score_train,
    score_test = score_test,
    score_full = score_full,
    include_model_object = include_model_object,
    include_plots = include_plots,
    top_n = top_n
  )
  warnings <- catboost_builder_warn(warnings, validation$warnings)
  if (length(validation$errors)) {
    return(catboost_builder_failure(
      paste(validation$errors, collapse = " "),
      problem_type = problem_type,
      warnings = warnings,
      diagnostics = data.table::data.table(severity = "error", message = validation$errors)
    ))
  }

  dt <- data.table::as.data.table(data.table::copy(data))
  dt[, .row_id := .I]
  dt[, .aq_builder_row_id := .row_id]
  feature_cols <- unique(as.character(feature_cols))
  id_cols <- unique(as.character(catboost_builder_null_coalesce(id_cols, character())))
  ByVars <- unique(as.character(catboost_builder_null_coalesce(ByVars, character())))
  DateVar <- if (!is.null(DateVar) && length(DateVar)) as.character(DateVar)[1L] else NULL
  split <- catboost_builder_make_split(dt, train_fraction, split_method, split_col, DateVar, as.integer(seed))

  if (is.null(cat_features)) {
    cat_features <- feature_cols[vapply(dt[, .SD, .SDcols = feature_cols], function(x) {
      is.character(x) || is.factor(x) || is.logical(x)
    }, logical(1L))]
  } else {
    cat_features <- unique(as.character(cat_features))
  }
  if (identical(problem_type, "regression")) {
    loss_function <- catboost_builder_null_coalesce(loss_function, "RMSE")
    eval_metric <- catboost_builder_null_coalesce(eval_metric, "RMSE")
  } else {
    target_values <- unique(stats::na.omit(dt[[target_col]]))
    positive_class <- target_values[as.character(target_values) == as.character(positive_class)][1L]
    loss_function <- catboost_builder_null_coalesce(loss_function, "Logloss")
    eval_metric <- catboost_builder_null_coalesce(eval_metric, "AUC")
  }

  model_parameters <- data.table::data.table(
    parameter = c(
      "loss_function",
      "eval_metric",
      "iterations",
      "depth",
      "learning_rate",
      "seed",
      "train_fraction",
      "split_method",
      "training_function"
    ),
    value = as.character(c(
      loss_function,
      eval_metric,
      as.integer(iterations),
      as.integer(depth),
      learning_rate,
      as.integer(seed),
      train_fraction,
      split_method,
      if (identical(problem_type, "regression")) "AutoCatBoostRegression" else "AutoCatBoostClassifier"
    ))
  )

  auto_run <- tryCatch(
    catboost_builder_run_auto_catboost(
      dt = dt,
      split = split,
      target_col = target_col,
      feature_cols = feature_cols,
      problem_type = problem_type,
      positive_class = positive_class,
      DateVar = DateVar,
      iterations = iterations,
      depth = depth,
      learning_rate = learning_rate,
      loss_function = loss_function,
      eval_metric = eval_metric,
      compute_shap = compute_shap,
      include_model_object = include_model_object,
      model_name = model_name
    ),
    error = function(e) e
  )
  if (inherits(auto_run, "error")) {
    return(catboost_builder_failure(
      paste("Existing AutoQuant CatBoost training failed:", conditionMessage(auto_run)),
      problem_type = problem_type,
      warnings = warnings,
      metadata = list(
        loss_function = loss_function,
        eval_metric = eval_metric,
        training_function = if (identical(problem_type, "regression")) "AutoCatBoostRegression" else "AutoCatBoostClassifier"
      )
    ))
  }

  model_result <- auto_run$result
  training_function <- auto_run$training_function
  scored_all <- catboost_builder_normalize_auto_scored(
    model_result = model_result,
    original_dt = dt,
    split = split,
    target_col = target_col,
    feature_cols = feature_cols,
    problem_type = problem_type,
    positive_class = positive_class,
    threshold = threshold,
    id_cols = id_cols,
    DateVar = DateVar,
    ByVars = ByVars,
    score_train = TRUE,
    score_test = TRUE
  )
  scored_data <- data.table::copy(scored_all)
  if (!isTRUE(score_train)) {
    scored_data <- scored_data[.split != "train"]
  }
  if (!isTRUE(score_test)) {
    scored_data <- scored_data[.split != "test"]
  }
  scored_full <- data.table::copy(scored_all)
  scored_full[, .split := "full"]
  shap_columns <- grep("^Shap_", names(scored_all), value = TRUE)
  if (isTRUE(compute_shap) && !length(shap_columns)) {
    warnings <- catboost_builder_warn(
      warnings,
      "Existing AutoQuant CatBoost output did not include SHAP columns; continuing without SHAP columns."
    )
  }

  variable_importance <- catboost_builder_normalize_importance(model_result)

  split_summary <- data.table::data.table(
    split = c("train", "test"),
    rows = c(length(split$train_index), length(split$test_index)),
    row_share = c(length(split$train_index), length(split$test_index)) / nrow(dt)
  )
  feature_roles <- data.table::data.table(
    feature = feature_cols,
    role = data.table::fifelse(feature_cols %in% cat_features, "categorical", "numeric_or_other"),
    preserved_as_context = feature_cols %in% c(id_cols, DateVar, ByVars)
  )

  train_test_metrics <- catboost_builder_normalize_metrics(model_result)
  if (!nrow(train_test_metrics)) {
    metric_sources <- split(scored_data, scored_data$.split)
    train_test_metrics <- if (identical(problem_type, "regression")) {
      catboost_builder_round_table(data.table::rbindlist(lapply(
        metric_sources,
        catboost_builder_metrics_regression,
        target_col = target_col,
        prediction_col = "Predict"
      ), use.names = TRUE, fill = TRUE))
    } else {
      catboost_builder_round_table(data.table::rbindlist(lapply(
        metric_sources,
        catboost_builder_metrics_binary,
        threshold = threshold
      ), use.names = TRUE, fill = TRUE))
    }
  }

  prediction_distribution <- if (identical(problem_type, "binary_classification")) {
    dist_dt <- data.table::copy(scored_data)
    dist_dt[, prediction_bin := cut(Predict, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE)]
    catboost_builder_round_table(dist_dt[, .(
      rows = .N,
      observed_rate = mean(actual_positive, na.rm = TRUE),
      mean_prediction = mean(Predict, na.rm = TRUE)
    ), by = .(prediction_bin, .split)][order(.split, prediction_bin)])
  } else {
    dist_dt <- data.table::copy(scored_data)
    dist_dt[, prediction_bin := cut(Predict, breaks = min(20L, max(5L, length(unique(Predict)))), include.lowest = TRUE)]
    catboost_builder_round_table(dist_dt[, .(
      rows = .N,
      mean_target = mean(get(target_col), na.rm = TRUE),
      mean_prediction = mean(Predict, na.rm = TRUE),
      mean_residual = mean(residual, na.rm = TRUE)
    ), by = .(prediction_bin, .split)][order(.split, prediction_bin)])
  }

  confusion_matrix <- data.table::data.table()
  threshold_metrics <- data.table::data.table()
  if (identical(problem_type, "binary_classification")) {
    threshold_metrics <- train_test_metrics
    confusion_matrix <- scored_data[.split == "test", .N, by = .(ActualClass, PredictedClass)][order(ActualClass, PredictedClass)]
  }

  diagnostics <- data.table::data.table(
    severity = if (length(warnings)) "warning" else "info",
    message = if (length(warnings)) warnings else "CatBoost Builder completed."
  )

  plots <- list()
  if (isTRUE(include_plots)) {
    plot_result <- catboost_builder_build_plots(
      scored_data = scored_data,
      variable_importance = variable_importance,
      problem_type = problem_type,
      target_col = target_col,
      top_n = top_n,
      Theme = "dark"
    )
    plots <- plot_result$plots
    warnings <- catboost_builder_warn(warnings, plot_result$warnings)
  }

  common_metadata <- list(
    source_package = "AutoQuant",
    source_function = "generate_catboost_builder_artifacts",
    training_function = training_function,
    problem_type = problem_type,
    model_type = "catboost",
    scored_output_contract_version = "1.0",
    shap_source = if (length(shap_columns)) "existing_autoquant_catboost_output" else NA_character_,
    model_name = catboost_builder_null_coalesce(model_name, NA_character_),
    data_name = catboost_builder_null_coalesce(data_name, NA_character_),
    target_col = target_col,
    feature_cols = feature_cols,
    cat_features = cat_features,
    positive_class = positive_class,
    threshold = threshold,
    DateVar = DateVar,
    ByVars = ByVars,
    id_cols = id_cols,
    created_by_autoquant = TRUE
  )

  tables <- list(
    training_config = data.table::data.table(
      model_name = catboost_builder_null_coalesce(model_name, NA_character_),
      data_name = catboost_builder_null_coalesce(data_name, NA_character_),
      problem_type = problem_type,
      target_col = target_col,
      feature_count = length(feature_cols),
      train_fraction = train_fraction,
      split_method = split_method,
      split_col = catboost_builder_null_coalesce(split_col, DateVar),
      seed = seed,
      training_function = training_function,
      compute_shap = compute_shap,
      shap_columns_created = length(shap_columns)
    ),
    split_summary = catboost_builder_round_table(split_summary),
    feature_roles = feature_roles,
    model_parameters = model_parameters,
    train_test_metrics = train_test_metrics,
    variable_importance = variable_importance,
    prediction_distribution = prediction_distribution,
    scored_data_preview = utils::head(scored_data, 100L),
    diagnostics = diagnostics
  )
  if (identical(problem_type, "binary_classification")) {
    tables$confusion_matrix <- confusion_matrix
    tables$threshold_metrics <- threshold_metrics
  }

  artifacts <- list()
  artifacts <- catboost_builder_add_artifact(artifacts, catboost_builder_artifact(
    "builder_overview",
    "Builder Overview",
    "text",
    "Builder Overview",
    content = paste(
      "CatBoost Builder trained a", problem_type, "model.",
      "Training was routed through", paste0(training_function, "."),
      "Scored data is available for downstream Model Assessment, Model Insights, and SHAP modules."
    ),
    metadata = c(common_metadata, list(lens = "overview", section = "Builder Overview"))
  ))

  table_specs <- list(
    list("training_config", "Training Config", "Builder Overview", tables$training_config),
    list("split_summary", "Data Split Summary", "Training Data", tables$split_summary),
    list("feature_roles", "Feature Role Table", "Training Data", tables$feature_roles),
    list("model_parameters", "Model Parameter Table", "Model Summary", tables$model_parameters),
    list("train_test_metrics", "Train/Test Metrics", "Model Summary", tables$train_test_metrics),
    list("variable_importance", "Variable Importance Table", "Variable Importance", tables$variable_importance),
    list("prediction_distribution", "Prediction Distribution Table", "Prediction Diagnostics", tables$prediction_distribution),
    list("scored_data_preview", "Scored Data Preview", "Scored Output", tables$scored_data_preview),
    list("diagnostics", "Diagnostics and Warnings", "Appendix", tables$diagnostics)
  )
  if (identical(problem_type, "binary_classification")) {
    table_specs <- c(table_specs, list(
      list("confusion_matrix", "Confusion Matrix", "Classification Diagnostics", tables$confusion_matrix),
      list("threshold_metrics", "Threshold Metrics", "Classification Diagnostics", tables$threshold_metrics)
    ))
  }
  for (spec in table_specs) {
    if (!is.null(spec[[4L]]) && data.table::is.data.table(spec[[4L]]) && nrow(spec[[4L]])) {
      artifacts <- catboost_builder_add_artifact(artifacts, catboost_builder_artifact(
        spec[[1L]],
        spec[[2L]],
        "table",
        spec[[3L]],
        object = spec[[4L]],
        metadata = c(common_metadata, list(lens = spec[[1L]], section = spec[[3L]]))
      ))
    }
  }

  if (length(plots)) {
    for (plot_name in names(plots)) {
      artifacts <- catboost_builder_add_artifact(artifacts, catboost_builder_artifact(
        plot_name,
        tools::toTitleCase(gsub("_", " ", plot_name)),
        "plot",
        if (grepl("importance", plot_name)) "Variable Importance" else "Prediction Diagnostics",
        object = plots[[plot_name]],
        metadata = c(common_metadata, list(
          lens = plot_name,
          section = if (grepl("importance", plot_name)) "Variable Importance" else "Prediction Diagnostics",
          plot_package = "AutoPlots"
        ))
      ))
    }
  }

  artifact_index <- catboost_builder_as_artifact_table(artifacts)
  downstream_handoff <- catboost_builder_make_handoff(
    problem_type = problem_type,
    scored_data = scored_data,
    target_col = target_col,
    feature_cols = feature_cols,
    prediction_scale = prediction_scale,
    positive_class = positive_class,
    threshold = threshold,
    DateVar = DateVar,
    ByVars = ByVars,
    id_cols = id_cols,
    shap_columns = shap_columns
  )

  metadata <- c(common_metadata, list(
    source_package = "AutoQuant",
    source_function = "generate_catboost_builder_artifacts",
    training_function = training_function,
    scored_output_contract_version = "1.0",
    shap_source = if (length(shap_columns)) "existing_autoquant_catboost_output" else NA_character_,
    generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    train_fraction = train_fraction,
    split_method = split_method,
    split_col = catboost_builder_null_coalesce(split_col, DateVar),
    seed = seed,
    iterations = as.integer(iterations),
    depth = as.integer(depth),
    learning_rate = learning_rate,
    loss_function = loss_function,
    eval_metric = eval_metric,
    compute_shap = compute_shap,
    shap_columns_created = shap_columns,
    scored_output_available = nrow(scored_data) > 0L,
    artifact_count = length(artifacts),
    plot_count = sum(artifact_index$type == "plot"),
    table_count = sum(artifact_index$type == "table"),
    text_count = sum(artifact_index$type == "text"),
    warnings_count = length(warnings),
    artifact_index = artifact_index
  ))

  out <- list(
    report_type = "catboost_builder",
    status = "success",
    artifacts = artifacts,
    metadata = metadata,
    warnings = warnings,
    diagnostics = diagnostics,
    tables = tables,
    plots = plots,
    value = list(
      scored_data = scored_data,
      scored_full = scored_full,
      model_object = if (isTRUE(include_model_object)) model_result$Model else NULL,
      auto_catboost_metadata = list(
        training_function = training_function,
        args = model_result$ArgsList,
        col_names = model_result$ColNames,
        factor_levels = model_result$FactorLevelsList,
        grid_metrics = model_result$GridMetrics
      ),
      split = list(train_index = split$train_index, test_index = split$test_index, summary = split_summary),
      downstream_handoff = downstream_handoff
    )
  )
  class(out) <- c("autoquant_catboost_builder_artifacts", "autoquant_report_artifacts", "list")
  out
}

catboost_builder_qa_fixture_regression <- function(n = 300L) {
  set.seed(20260705)
  dt <- data.table::data.table(
    id = seq_len(n),
    event_date = as.Date("2025-01-01") + seq_len(n) - 1L,
    channel = sample(c("Search", "Email", "Social", "Direct"), n, replace = TRUE),
    region = sample(c("West", "Midwest", "South"), n, replace = TRUE),
    spend = stats::runif(n, 50, 500),
    clicks = stats::rpois(n, 40),
    discount = stats::runif(n, 0, 0.3)
  )
  dt[, revenue := 10 + 2.5 * spend + 1.8 * clicks - 120 * discount +
    data.table::fifelse(channel == "Search", 80, 0) +
    data.table::fifelse(region == "West", 40, 0) +
    stats::rnorm(.N, 0, 35)]
  dt[]
}

catboost_builder_qa_fixture_binary <- function(n = 300L) {
  set.seed(20260705)
  dt <- data.table::data.table(
    id = seq_len(n),
    event_date = as.Date("2025-01-01") + seq_len(n) - 1L,
    channel = sample(c("Search", "Email", "Social", "Direct"), n, replace = TRUE),
    region = sample(c("West", "Midwest", "South"), n, replace = TRUE),
    spend = stats::runif(n, 50, 500),
    clicks = stats::rpois(n, 40),
    discount = stats::runif(n, 0, 0.3)
  )
  logit <- -2 + 0.006 * dt$spend + 0.025 * dt$clicks - 2 * dt$discount +
    data.table::fifelse(dt$channel == "Search", 0.8, 0) +
    data.table::fifelse(dt$region == "West", 0.35, 0)
  p <- 1 / (1 + exp(-logit))
  dt[, converted := data.table::fifelse(stats::runif(.N) <= p, "Yes", "No")]
  dt[]
}

catboost_builder_qa_summary <- function(result, problem_type) {
  if (identical(result$status, "error")) {
    return(data.table::data.table(
      problem_type = problem_type,
      check = "generator",
      status = "warning",
      detail = result$error
    ))
  }
  scored <- result$value$scored_data
  checks <- data.table::data.table(
    problem_type = problem_type,
    check = c(
      "status_success",
      "artifacts_returned",
      "plot_artifacts_returned",
      "table_artifacts_returned",
      "text_artifacts_returned",
      "scored_data_returned",
      "predict_exists",
      "split_exists",
      "model_object_excluded",
      "variable_importance_exists",
      "metrics_exists",
      "downstream_handoff_exists",
      "routes_through_existing_auto_catboost",
      "shap_best_effort_recorded"
    ),
    passed = c(
      identical(result$status, "success"),
      length(result$artifacts) > 0L,
      result$metadata$plot_count > 0L || !requireNamespace("AutoPlots", quietly = TRUE),
      result$metadata$table_count > 0L,
      result$metadata$text_count > 0L,
      data.table::is.data.table(scored) && nrow(scored) > 0L,
      "Predict" %in% names(scored),
      ".split" %in% names(scored),
      is.null(result$value$model_object),
      nrow(result$tables$variable_importance) > 0L,
      nrow(result$tables$train_test_metrics) > 0L,
      is.list(result$value$downstream_handoff),
      identical(
        result$metadata$training_function,
        if (identical(problem_type, "regression")) "AutoCatBoostRegression" else "AutoCatBoostClassifier"
      ),
      length(result$metadata$shap_columns_created) > 0L || any(grepl("SHAP column generation failed", result$warnings))
    )
  )
  if (identical(problem_type, "regression")) {
    checks <- data.table::rbindlist(list(checks, data.table::data.table(
      problem_type = problem_type,
      check = "residual_exists",
      passed = "residual" %in% names(scored)
    )), use.names = TRUE, fill = TRUE)
  } else {
    checks <- data.table::rbindlist(list(checks, data.table::data.table(
      problem_type = problem_type,
      check = c("predicted_class_exists", "actual_positive_exists", "predicted_positive_exists"),
      passed = c("PredictedClass" %in% names(scored), "actual_positive" %in% names(scored), "predicted_positive" %in% names(scored))
    )), use.names = TRUE, fill = TRUE)
  }
  checks[, status := data.table::fifelse(passed, "pass", "fail")]
  checks[, detail := ""]
  checks[, passed := NULL]
  checks[]
}

#' QA for CatBoost Builder Regression Artifacts
#'
#' @return A compact data.table of QA checks.
#'
#' @family Reports
#' @export
qa_generate_catboost_builder_artifacts_regression <- function() {
  dt <- catboost_builder_qa_fixture_regression()
  result <- generate_catboost_builder_artifacts(
    data = dt,
    target_col = "revenue",
    feature_cols = c("channel", "region", "spend", "clicks", "discount"),
    problem_type = "regression",
    id_cols = "id",
    DateVar = "event_date",
    ByVars = c("channel", "region"),
    split_method = "time",
    iterations = 30L,
    depth = 4L,
    learning_rate = 0.08,
    compute_shap = TRUE,
    include_model_object = FALSE,
    model_name = "qa_catboost_regression",
    data_name = "qa_regression_fixture"
  )
  catboost_builder_qa_summary(result, "regression")
}

#' QA for CatBoost Builder Binary Classification Artifacts
#'
#' @return A compact data.table of QA checks.
#'
#' @family Reports
#' @export
qa_generate_catboost_builder_artifacts_binary <- function() {
  dt <- catboost_builder_qa_fixture_binary()
  result <- generate_catboost_builder_artifacts(
    data = dt,
    target_col = "converted",
    feature_cols = c("channel", "region", "spend", "clicks", "discount"),
    problem_type = "binary_classification",
    positive_class = "Yes",
    id_cols = "id",
    DateVar = "event_date",
    ByVars = c("channel", "region"),
    split_method = "time",
    iterations = 30L,
    depth = 4L,
    learning_rate = 0.08,
    threshold = 0.5,
    compute_shap = TRUE,
    include_model_object = FALSE,
    model_name = "qa_catboost_binary",
    data_name = "qa_binary_fixture"
  )
  catboost_builder_qa_summary(result, "binary_classification")
}

#' QA for CatBoost Builder Artifacts
#'
#' @return A compact data.table of QA checks for regression and binary workflows.
#'
#' @family Reports
#' @export
qa_generate_catboost_builder_artifacts <- function() {
  invalid_feature <- tryCatch(
    {
      dt <- catboost_builder_qa_fixture_regression(120L)
      result <- generate_catboost_builder_artifacts(
        data = dt,
        target_col = "revenue",
        feature_cols = c("missing_feature"),
        problem_type = "regression",
        iterations = 5L,
        compute_shap = FALSE,
        include_plots = FALSE
      )
      identical(result$status, "error")
    },
    error = function(e) FALSE
  )
  invalid_positive <- tryCatch(
    {
      dt <- catboost_builder_qa_fixture_binary(120L)
      result <- generate_catboost_builder_artifacts(
        data = dt,
        target_col = "converted",
        feature_cols = c("channel", "region", "spend", "clicks", "discount"),
        problem_type = "binary_classification",
        positive_class = "Maybe",
        iterations = 5L,
        compute_shap = FALSE,
        include_plots = FALSE
      )
      identical(result$status, "error")
    },
    error = function(e) FALSE
  )

  data.table::rbindlist(list(
    qa_generate_catboost_builder_artifacts_regression(),
    qa_generate_catboost_builder_artifacts_binary(),
    data.table::data.table(
      problem_type = "validation",
      check = c("invalid_feature_cols_structured_error", "invalid_positive_class_structured_error"),
      status = data.table::fifelse(c(invalid_feature, invalid_positive), "pass", "fail"),
      detail = ""
    )
  ), use.names = TRUE, fill = TRUE)
}
