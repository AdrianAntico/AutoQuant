# ============================================================
# Binary Classification Model Insights Artifact Generation
# ============================================================

binary_model_insights_null_coalesce <- function(x, y) {
  if (is.null(x)) y else x
}

binary_model_insights_slug <- function(x) {
  x <- as.character(x)
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x <- tolower(x)
  if (!nzchar(x)) "artifact" else x
}

binary_model_insights_safe_div <- function(num, den) {
  ifelse(is.finite(den) & den != 0, num / den, NA_real_)
}

binary_model_insights_metric_name <- function(x) {
  x <- gsub("[^A-Za-z0-9]+", "", as.character(x))
  tolower(x)
}

binary_model_insights_allowed_optimize_metrics <- function() {
  c(
    Utility = "utility_total",
    Accuracy = "accuracy",
    BalancedAccuracy = "balanced_accuracy",
    F1 = "f1_score",
    FBeta = "f_beta_score",
    MatthewsCorrelation = "matthews_correlation",
    YoudensJ = "youdens_j",
    CohenKappa = "cohen_kappa"
  )
}

binary_model_insights_resolve_optimize_metric <- function(OptimizeMetric) {
  allowed <- binary_model_insights_allowed_optimize_metrics()
  key <- binary_model_insights_metric_name(OptimizeMetric)
  allowed_key <- binary_model_insights_metric_name(names(allowed))
  match_index <- match(key, allowed_key)
  if (is.na(match_index)) {
    stop(
      paste(
        "Unsupported OptimizeMetric:",
        OptimizeMetric,
        "Allowed values are:",
        paste(names(allowed), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  unname(allowed[[match_index]])
}

binary_model_insights_artifact <- function(
  name,
  label,
  type,
  section,
  object,
  metadata = list()
) {
  list(
    name = name,
    label = label,
    type = type,
    section = section,
    object = object,
    metadata = metadata
  )
}

binary_model_insights_add_artifact <- function(artifacts, artifact) {
  base_name <- binary_model_insights_slug(artifact$name)
  artifact$name <- base_name
  artifact$metadata$artifact_index <- length(artifacts) + 1L
  artifact$metadata$source_package <- binary_model_insights_null_coalesce(artifact$metadata$source_package, "AutoQuant")
  artifact$metadata$source_function <- binary_model_insights_null_coalesce(
    artifact$metadata$source_function,
    "generate_binary_classification_model_insights_artifacts"
  )
  artifact$metadata$problem_type <- binary_model_insights_null_coalesce(artifact$metadata$problem_type, "binary_classification")
  artifact$metadata$source_module <- binary_model_insights_null_coalesce(
    artifact$metadata$source_module,
    "autoquant_binary_model_insights"
  )
  artifact$metadata$original_name <- binary_model_insights_null_coalesce(artifact$metadata$original_name, artifact$name)
  artifact$metadata$original_section <- binary_model_insights_null_coalesce(artifact$metadata$original_section, artifact$section)
  artifact$metadata$normalized_section <- binary_model_insights_null_coalesce(artifact$metadata$normalized_section, artifact$section)
  artifact$metadata$created_by_module <- TRUE
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

binary_model_insights_as_artifact_table <- function(artifacts) {
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

binary_model_insights_resolve_positive_class <- function(y, PositiveClass = NULL) {
  values <- unique(stats::na.omit(y))
  if (length(values) != 2L) {
    stop("TargetColumnName must contain exactly two non-missing classes.", call. = FALSE)
  }

  if (!is.null(PositiveClass)) {
    if (!PositiveClass %in% values && !as.character(PositiveClass) %in% as.character(values)) {
      stop("PositiveClass was not found in the target column.", call. = FALSE)
    }
    return(values[as.character(values) == as.character(PositiveClass)][1L])
  }

  if (is.numeric(values) || is.integer(values) || is.logical(values)) {
    return(sort(values, na.last = TRUE, decreasing = TRUE)[1L])
  }

  sort(as.character(values), decreasing = TRUE)[1L]
}

binary_model_insights_extract_inputs <- function(
  TrainDataInclude = FALSE,
  FeatureColumnNames = NULL,
  SampleSize = 100000L,
  ModelObject = NULL,
  ModelID = NULL,
  SourcePath = NULL,
  OutputPath = NULL,
  PredictionColumnName = NULL,
  TargetColumnName = NULL,
  PositiveClass = NULL,
  Theme = "dark",
  ...
) {
  extra <- list(...)
  TrainData <- extra$TrainData
  TestData <- extra$TestData

  if (!is.null(ModelObject)) {
    TrainData <- binary_model_insights_null_coalesce(TrainData, ModelObject[["TrainData"]])
    TestData <- binary_model_insights_null_coalesce(TestData, ModelObject[["TestData"]])
    args <- ModelObject[["ArgsList"]]
    TargetColumnName <- binary_model_insights_null_coalesce(TargetColumnName, args[["TargetColumnName"]])
    PredictionColumnName <- binary_model_insights_null_coalesce(PredictionColumnName, args[["PredictionColumnName"]])
    PredictionColumnName <- binary_model_insights_null_coalesce(PredictionColumnName, "p1")
    PositiveClass <- binary_model_insights_null_coalesce(PositiveClass, args[["PositiveClass"]])
    PositiveClass <- binary_model_insights_null_coalesce(PositiveClass, args[["TargetLevels"]][1L])
    ModelID <- binary_model_insights_null_coalesce(ModelID, args[["ModelID"]])
    if (is.null(FeatureColumnNames)) {
      FeatureColumnNames <- ModelObject[["ColNames"]][[1L]]
    }
  }

  if (is.null(TestData) && !is.null(TrainData)) {
    TestData <- TrainData
  }

  if (is.null(TestData) && !is.null(SourcePath) && !is.null(ModelID)) {
    candidates <- file.path(
      SourcePath,
      c(
        paste0(ModelID, "_ValidationData.csv"),
        paste0(ModelID, "_TestData.csv"),
        "ValidationData.csv",
        "TestData.csv"
      )
    )
    existing <- candidates[file.exists(candidates)]
    if (length(existing)) {
      TestData <- data.table::fread(existing[[1L]])
    }
  }

  if (is.null(TestData)) {
    stop("ModelObject, TestData, TrainData, or SourcePath/ModelID must provide scoring data.", call. = FALSE)
  }

  TestData <- data.table::as.data.table(TestData)
  TrainData <- if (isTRUE(TrainDataInclude) && !is.null(TrainData)) data.table::as.data.table(TrainData) else NULL

  if (is.null(PredictionColumnName) || !nzchar(PredictionColumnName)) {
    PredictionColumnName <- if ("p1" %in% names(TestData)) "p1" else "Predict"
  }
  if (!PredictionColumnName %in% names(TestData)) {
    stop("PredictionColumnName was not found in scoring data.", call. = FALSE)
  }
  if (!is.numeric(TestData[[PredictionColumnName]])) {
    stop("PredictionColumnName must be numeric.", call. = FALSE)
  }
  if (is.null(TargetColumnName) || !TargetColumnName %in% names(TestData)) {
    stop("TargetColumnName was not found in scoring data.", call. = FALSE)
  }

  PositiveClass <- binary_model_insights_resolve_positive_class(TestData[[TargetColumnName]], PositiveClass)
  NegativeClass <- setdiff(unique(stats::na.omit(TestData[[TargetColumnName]])), PositiveClass)[1L]

  available_names <- unique(c(names(TestData), names(TrainData)))
  if (is.null(FeatureColumnNames)) {
    FeatureColumnNames <- setdiff(available_names, c(TargetColumnName, PredictionColumnName))
  }
  FeatureColumnNames <- intersect(as.character(FeatureColumnNames), available_names)

  if (!is.null(SampleSize) && is.finite(SampleSize) && nrow(TestData) > SampleSize) {
    TestData <- TestData[sample.int(nrow(TestData), SampleSize)]
  }
  if (!is.null(TrainData) && !is.null(SampleSize) && is.finite(SampleSize) && nrow(TrainData) > SampleSize) {
    TrainData <- TrainData[sample.int(nrow(TrainData), SampleSize)]
  }

  list(
    TrainData = TrainData,
    TestData = TestData,
    TargetColumnName = TargetColumnName,
    PredictionColumnName = PredictionColumnName,
    PositiveClass = PositiveClass,
    NegativeClass = NegativeClass,
    FeatureColumnNames = FeatureColumnNames,
    ModelID = ModelID,
    SourcePath = SourcePath,
    OutputPath = OutputPath,
    Theme = Theme,
    metadata = list(
      report_type = "Binary Classification Model Insights",
      artifact_type = "binary_classification_model_insights",
      created_at = as.character(Sys.time()),
      model_id = ModelID,
      source_path = SourcePath,
      output_path = OutputPath,
      target_column = TargetColumnName,
      prediction_column = PredictionColumnName,
      positive_class = as.character(PositiveClass),
      negative_class = as.character(NegativeClass),
      feature_columns = FeatureColumnNames,
      n_features = length(FeatureColumnNames),
      train_data_include = isTRUE(TrainDataInclude),
      train_rows = if (!is.null(TrainData)) nrow(TrainData) else 0L,
      test_rows = nrow(TestData),
      theme = Theme,
      autoquant_version = tryCatch(as.character(utils::packageVersion("AutoQuant")), error = function(e) NA_character_),
      autoplots_version = tryCatch(as.character(utils::packageVersion("AutoPlots")), error = function(e) NA_character_)
    )
  )
}

binary_model_insights_threshold_metrics <- function(
  data,
  TargetColumnName,
  PredictionColumnName,
  PositiveClass,
  Thresholds,
  UtilityTP,
  UtilityTN,
  UtilityFP,
  UtilityFN,
  Beta = 1
) {
  dt <- data.table::as.data.table(data)[, .(
    actual = get(TargetColumnName),
    prediction = suppressWarnings(as.numeric(get(PredictionColumnName)))
  )]
  dt <- dt[is.finite(prediction) & !is.na(actual)]
  if (!nrow(dt)) {
    return(data.table::data.table())
  }

  actual_pos <- as.logical(as.character(dt$actual) == as.character(PositiveClass))
  total <- length(actual_pos)
  prevalence <- mean(actual_pos)

  out <- data.table::rbindlist(lapply(Thresholds, function(threshold) {
    predicted_pos <- dt$prediction >= threshold
    TP <- sum(predicted_pos & actual_pos)
    FP <- sum(predicted_pos & !actual_pos)
    TN <- sum(!predicted_pos & !actual_pos)
    FN <- sum(!predicted_pos & actual_pos)
    precision <- binary_model_insights_safe_div(TP, TP + FP)
    recall <- binary_model_insights_safe_div(TP, TP + FN)
    specificity <- binary_model_insights_safe_div(TN, TN + FP)
    accuracy <- binary_model_insights_safe_div(TP + TN, total)
    f_beta <- binary_model_insights_safe_div((1 + Beta^2) * precision * recall, (Beta^2 * precision) + recall)
    f1 <- binary_model_insights_safe_div(2 * precision * recall, precision + recall)
    mcc_denom <- sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
    expected_accuracy <- (
      binary_model_insights_safe_div((TP + FP) * (TP + FN), total) +
        binary_model_insights_safe_div((FN + TN) * (FP + TN), total)
    ) / total
    utility_total <- UtilityTP * TP + UtilityTN * TN + UtilityFP * FP + UtilityFN * FN
    data.table::data.table(
      threshold = threshold,
      TP = TP,
      FP = FP,
      TN = TN,
      FN = FN,
      predicted_positive_count = TP + FP,
      predicted_positive_rate = binary_model_insights_safe_div(TP + FP, total),
      precision = precision,
      recall = recall,
      specificity = specificity,
      false_positive_rate = binary_model_insights_safe_div(FP, FP + TN),
      false_negative_rate = binary_model_insights_safe_div(FN, FN + TP),
      accuracy = accuracy,
      balanced_accuracy = mean(c(recall, specificity), na.rm = TRUE),
      f1_score = f1,
      f_beta_score = f_beta,
      matthews_correlation = ifelse(is.finite(mcc_denom) && mcc_denom > 0, ((TP * TN) - (FP * FN)) / mcc_denom, NA_real_),
      youdens_j = recall + specificity - 1,
      cohen_kappa = binary_model_insights_safe_div(accuracy - expected_accuracy, 1 - expected_accuracy),
      utility_total = utility_total,
      utility_per_observation = binary_model_insights_safe_div(utility_total, total),
      prevalence = prevalence
    )
  }))

  out[]
}

binary_model_insights_confusion_matrix <- function(threshold_metrics_row) {
  data.table::data.table(
    Actual = c("Positive", "Positive", "Negative", "Negative"),
    Predicted = c("Positive", "Negative", "Positive", "Negative"),
    Count = c(
      threshold_metrics_row$TP,
      threshold_metrics_row$FN,
      threshold_metrics_row$FP,
      threshold_metrics_row$TN
    )
  )
}

binary_model_insights_build_threshold_optimizer_plot <- function(metrics, default_threshold, optimized_threshold, optimize_column) {
  plot_metrics <- c("utility_per_observation", "accuracy", "balanced_accuracy", "f1_score", "matthews_correlation")
  plot_metrics <- intersect(plot_metrics, names(metrics))
  long <- data.table::melt(
    metrics[, c("threshold", plot_metrics), with = FALSE],
    id.vars = "threshold",
    variable.name = "metric",
    value.name = "value"
  )
  value_range <- range(long$value, na.rm = TRUE)
  marker_rows <- data.table::data.table(
    threshold = c(default_threshold, default_threshold, optimized_threshold, optimized_threshold),
    metric = c("Default Threshold", "Default Threshold", "Optimized Threshold", "Optimized Threshold"),
    value = c(value_range[1L], value_range[2L], value_range[1L], value_range[2L])
  )
  long <- data.table::rbindlist(list(long, marker_rows), use.names = TRUE, fill = TRUE)

  AutoPlots::Line(
    dt = long,
    PreAgg = TRUE,
    XVar = "threshold",
    YVar = "value",
    GroupVar = "metric",
    Theme = "dark",
    title.text = "Threshold Optimizer",
    title.subtext = paste("Optimized by", optimize_column),
    xAxis.title = "Threshold",
    yAxis.title = "Metric Value"
  )
}

binary_model_insights_build_roc_plot <- function(scored) {
  roc_dt <- data.table::copy(scored[order(-prediction)])
  positives <- sum(roc_dt$actual_binary == 1L)
  negatives <- sum(roc_dt$actual_binary == 0L)
  roc_dt[, true_positive_rate := cumsum(actual_binary == 1L) / positives]
  roc_dt[, false_positive_rate := cumsum(actual_binary == 0L) / negatives]
  roc_plot_dt <- data.table::rbindlist(list(
    data.table::data.table(false_positive_rate = 0, true_positive_rate = 0),
    roc_dt[, .(false_positive_rate, true_positive_rate)],
    data.table::data.table(false_positive_rate = 1, true_positive_rate = 1)
  ), use.names = TRUE)

  AutoPlots::Line(
    dt = roc_plot_dt,
    PreAgg = TRUE,
    XVar = "false_positive_rate",
    YVar = "true_positive_rate",
    Theme = "dark",
    title.text = "ROC Curve",
    xAxis.title = "False Positive Rate",
    yAxis.title = "True Positive Rate"
  )
}

binary_model_insights_build_pr_plot <- function(metrics) {
  AutoPlots::Line(
    dt = metrics[!is.na(recall) & !is.na(precision)],
    PreAgg = TRUE,
    XVar = "recall",
    YVar = "precision",
    Theme = "dark",
    title.text = "Precision-Recall Curve",
    xAxis.title = "Recall",
    yAxis.title = "Precision"
  )
}

binary_model_insights_build_calibration_table <- function(scored, bins = 20L) {
  dt <- data.table::copy(scored)
  dt[, calibration_bin := cut(
    prediction,
    breaks = unique(stats::quantile(prediction, probs = seq(0, 1, length.out = bins + 1L), na.rm = TRUE)),
    include.lowest = TRUE,
    labels = FALSE
  )]
  dt[!is.na(calibration_bin), .(
    n = .N,
    mean_prediction = mean(prediction, na.rm = TRUE),
    observed_rate = mean(actual_binary, na.rm = TRUE),
    calibration_error = mean(actual_binary, na.rm = TRUE) - mean(prediction, na.rm = TRUE)
  ), by = calibration_bin][order(calibration_bin)]
}

binary_model_insights_build_calibration_plot <- function(calibration_table) {
  if (!nrow(calibration_table)) {
    return(NULL)
  }

  AutoPlots::Line(
    dt = calibration_table,
    PreAgg = TRUE,
    XVar = "mean_prediction",
    YVar = "observed_rate",
    Theme = "dark",
    title.text = "Calibration Plot",
    xAxis.title = "Mean Predicted Probability",
    yAxis.title = "Observed Positive Rate"
  )
}

binary_model_insights_build_gains_table <- function(scored, bins = 20L) {
  dt <- data.table::copy(scored)
  data.table::setorder(dt, -prediction)
  dt[, row_id := .I]
  dt[, bin := pmin(bins, ceiling(row_id / .N * bins))]
  total_pos <- sum(dt$actual_binary)
  dt[, .(
    n = .N,
    positives = sum(actual_binary),
    mean_prediction = mean(prediction)
  ), by = bin][
    order(bin)
  ][
    , `:=`(
      population_rate = cumsum(n) / sum(n),
      cumulative_gain = cumsum(positives) / total_pos,
      lift = binary_model_insights_safe_div(cumsum(positives) / cumsum(n), total_pos / sum(n))
    )
  ][]
}

binary_model_insights_build_gains_plot <- function(gains_table) {
  AutoPlots::Line(
    dt = gains_table,
    PreAgg = TRUE,
    XVar = "population_rate",
    YVar = "cumulative_gain",
    Theme = "dark",
    title.text = "Gains Plot",
    xAxis.title = "Population Share",
    yAxis.title = "Cumulative Positive Capture"
  )
}

binary_model_insights_build_lift_plot <- function(gains_table) {
  AutoPlots::Line(
    dt = gains_table,
    PreAgg = TRUE,
    XVar = "population_rate",
    YVar = "lift",
    Theme = "dark",
    title.text = "Lift Plot",
    xAxis.title = "Population Share",
    yAxis.title = "Lift"
  )
}

binary_model_insights_round_table <- function(dt, digits = 4L) {
  dt <- data.table::as.data.table(dt)
  numeric_cols <- names(dt)[vapply(dt, is.numeric, logical(1L))]
  for (col in numeric_cols) {
    data.table::set(dt, j = col, value = round(dt[[col]], digits))
  }
  dt[]
}

binary_model_insights_build_prediction_distribution <- function(scored, bins = 20L) {
  dt <- data.table::copy(scored)
  breaks <- seq(0, 1, length.out = bins + 1L)
  dt[, prediction_bin := cut(
    prediction,
    breaks = breaks,
    include.lowest = TRUE,
    dig.lab = 4L
  )]
  out <- dt[!is.na(prediction_bin), .(
    n = .N,
    positive_count = sum(actual_binary == 1L, na.rm = TRUE),
    observed_rate = mean(actual_binary, na.rm = TRUE),
    mean_prediction = mean(prediction, na.rm = TRUE)
  ), by = prediction_bin][order(prediction_bin)]
  binary_model_insights_round_table(out)
}

binary_model_insights_build_prediction_distribution_plot <- function(prediction_distribution) {
  if (!nrow(prediction_distribution)) {
    return(NULL)
  }

  AutoPlots::Bar(
    dt = prediction_distribution,
    PreAgg = TRUE,
    XVar = "prediction_bin",
    YVar = "n",
    Theme = "dark",
    title.text = "Prediction Distribution",
    xAxis.title = "Predicted Probability Bin",
    yAxis.title = "Rows"
  )
}

binary_model_insights_date_bucket <- function(x, aggregation = "month") {
  date_values <- as.Date(x)
  aggregation <- tolower(binary_model_insights_null_coalesce(aggregation, "month"))
  if (identical(aggregation, "day")) {
    return(date_values)
  }
  if (identical(aggregation, "week")) {
    return(date_values - as.integer(format(date_values, "%u")) + 1L)
  }
  if (identical(aggregation, "quarter")) {
    month <- as.integer(format(date_values, "%m"))
    quarter_month <- ((month - 1L) %/% 3L) * 3L + 1L
    return(as.Date(sprintf("%s-%02d-01", format(date_values, "%Y"), quarter_month)))
  }
  as.Date(sprintf("%s-%s-01", format(date_values, "%Y"), format(date_values, "%m")))
}

binary_model_insights_build_segment_diagnostics <- function(scored, ByVars = character(), threshold = 0.5, max_levels = 25L) {
  ByVars <- intersect(unique(as.character(ByVars)), names(scored))
  if (!length(ByVars)) {
    return(data.table::data.table())
  }

  out <- data.table::rbindlist(lapply(ByVars, function(by_var) {
    segment_dt <- data.table::copy(scored)
    segment_dt[, segment := as.character(get(by_var))]
    segment_dt[is.na(segment) | !nzchar(segment), segment := "(Missing)"]
    segment_dt[, predicted_binary := as.integer(prediction >= threshold)]
    segment_summary <- segment_dt[, .(
      n = .N,
      positive_count = sum(actual_binary == 1L, na.rm = TRUE),
      predicted_positive_count = sum(predicted_binary == 1L, na.rm = TRUE),
      observed_rate = mean(actual_binary, na.rm = TRUE),
      mean_prediction = mean(prediction, na.rm = TRUE),
      precision = binary_model_insights_safe_div(sum(predicted_binary == 1L & actual_binary == 1L), sum(predicted_binary == 1L)),
      recall = binary_model_insights_safe_div(sum(predicted_binary == 1L & actual_binary == 1L), sum(actual_binary == 1L)),
      false_positive_rate = binary_model_insights_safe_div(sum(predicted_binary == 1L & actual_binary == 0L), sum(actual_binary == 0L))
    ), by = segment][order(-n)]
    segment_summary <- segment_summary[seq_len(min(.N, max_levels))]
    segment_summary[, by_var := by_var]
    data.table::setcolorder(segment_summary, c("by_var", "segment", setdiff(names(segment_summary), c("by_var", "segment"))))
    segment_summary
  }), use.names = TRUE, fill = TRUE)

  binary_model_insights_round_table(out)
}

binary_model_insights_build_segment_plot <- function(segment_diagnostics) {
  if (!nrow(segment_diagnostics)) {
    return(NULL)
  }

  plot_dt <- data.table::melt(
    segment_diagnostics[, .(by_var, segment, observed_rate, mean_prediction)],
    id.vars = c("by_var", "segment"),
    variable.name = "metric",
    value.name = "value"
  )

  AutoPlots::Bar(
    dt = plot_dt,
    PreAgg = TRUE,
    XVar = "segment",
    YVar = "value",
    GroupVar = "metric",
    FacetLevels = unique(plot_dt$by_var),
    Theme = "dark",
    title.text = "Segment Diagnostics",
    xAxis.title = "Segment",
    yAxis.title = "Rate / Mean Prediction"
  )
}

binary_model_insights_build_time_diagnostics <- function(scored, DateVar = NULL, date_aggregation = "month", threshold = 0.5) {
  if (is.null(DateVar) || !nzchar(DateVar) || !DateVar %in% names(scored)) {
    return(data.table::data.table())
  }

  dt <- data.table::copy(scored)
  dt[, time_period := binary_model_insights_date_bucket(get(DateVar), date_aggregation)]
  dt <- dt[!is.na(time_period)]
  if (!nrow(dt)) {
    return(data.table::data.table())
  }
  dt[, predicted_binary := as.integer(prediction >= threshold)]
  out <- dt[, .(
    n = .N,
    positive_count = sum(actual_binary == 1L, na.rm = TRUE),
    predicted_positive_count = sum(predicted_binary == 1L, na.rm = TRUE),
    observed_rate = mean(actual_binary, na.rm = TRUE),
    mean_prediction = mean(prediction, na.rm = TRUE),
    precision = binary_model_insights_safe_div(sum(predicted_binary == 1L & actual_binary == 1L), sum(predicted_binary == 1L)),
    recall = binary_model_insights_safe_div(sum(predicted_binary == 1L & actual_binary == 1L), sum(actual_binary == 1L))
  ), by = time_period][order(time_period)]
  binary_model_insights_round_table(out)
}

binary_model_insights_build_time_plot <- function(time_diagnostics) {
  if (!nrow(time_diagnostics)) {
    return(NULL)
  }

  plot_dt <- data.table::melt(
    time_diagnostics[, .(time_period, observed_rate, mean_prediction)],
    id.vars = "time_period",
    variable.name = "metric",
    value.name = "value"
  )

  AutoPlots::Line(
    dt = plot_dt,
    PreAgg = TRUE,
    XVar = "time_period",
    YVar = "value",
    GroupVar = "metric",
    Theme = "dark",
    title.text = "Time Diagnostics",
    xAxis.title = "Time Period",
    yAxis.title = "Rate / Mean Prediction"
  )
}

binary_model_insights_feature_summary <- function(scored, feature_cols, max_features = 25L) {
  numeric_features <- feature_cols[vapply(feature_cols, function(col) is.numeric(scored[[col]]) || is.integer(scored[[col]]), logical(1L))]
  if (!length(numeric_features)) {
    return(data.table::data.table())
  }

  out <- data.table::rbindlist(lapply(numeric_features, function(col) {
    data.table::data.table(
      feature = col,
      correlation_with_prediction = suppressWarnings(stats::cor(scored[[col]], scored$prediction, use = "complete.obs")),
      correlation_with_target = suppressWarnings(stats::cor(scored[[col]], scored$actual_binary, use = "complete.obs"))
    )
  }), fill = TRUE)
  out[, importance_proxy := abs(correlation_with_prediction)]
  out[order(-importance_proxy)][seq_len(min(.N, max_features))]
}

binary_model_insights_build_feature_plots <- function(scored, feature_summary, target_col, prediction_col, max_features = 5L) {
  plots <- list()
  if (!nrow(feature_summary)) {
    return(plots)
  }

  for (feature in utils::head(feature_summary$feature, max_features)) {
    feature_dt <- data.table::copy(scored)
    if (is.numeric(feature_dt[[feature]]) || is.integer(feature_dt[[feature]])) {
      feature_dt[, feature_bin := cut(
        get(feature),
        breaks = unique(stats::quantile(get(feature), probs = seq(0, 1, length.out = 11L), na.rm = TRUE)),
        include.lowest = TRUE,
        labels = FALSE
      )]
      pdp <- feature_dt[!is.na(feature_bin), .(
        feature_value = mean(get(feature), na.rm = TRUE),
        observed_rate = mean(actual_binary, na.rm = TRUE),
        mean_prediction = mean(prediction, na.rm = TRUE)
      ), by = feature_bin][order(feature_bin)]
      plots[[paste0(feature, "_line")]] <- AutoPlots::PartialDependence.Line(
        dt = scored,
        XVar = feature,
        YVar = "actual_binary",
        ZVar = "prediction",
        Theme = "dark",
        title.text = paste("Partial Dependence Line:", feature),
        xAxis.title = feature,
        yAxis.title = "Actual / Predicted"
      )
      plots[[paste0(feature, "_box")]] <- AutoPlots::PartialDependence.Box(
        dt = scored,
        XVar = feature,
        YVar = "actual_binary",
        ZVar = "prediction",
        Theme = "dark",
        title.text = paste("Partial Dependence Box:", feature),
        xAxis.title = feature,
        yAxis.title = "Target - Predicted"
      )
    } else {
      pdp <- feature_dt[, .(
        n = .N,
        observed_rate = mean(actual_binary, na.rm = TRUE),
        mean_prediction = mean(prediction, na.rm = TRUE)
      ), by = feature][order(-n)][seq_len(min(.N, 20L))]
      pdp[, metric := "Observed Rate"]
      plots[[paste0(feature, "_heatmap")]] <- AutoPlots::HeatMap(
        dt = pdp,
        PreAgg = TRUE,
        XVar = feature,
        YVar = "metric",
        ZVar = "observed_rate",
        Theme = "dark",
        title.text = paste("Partial Dependence Heatmap:", feature),
        xAxis.title = feature,
        yAxis.title = NULL
      )
    }
  }

  plots
}

binary_model_insights_validate_artifacts <- function(artifacts) {
  required <- c("name", "label", "type", "section", "object")
  all(vapply(artifacts, function(artifact) all(required %in% names(artifact)), logical(1L))) &&
    !anyDuplicated(vapply(artifacts, function(artifact) artifact$name, character(1))) &&
    all(nzchar(vapply(artifacts, function(artifact) artifact$label, character(1)))) &&
    all(nzchar(vapply(artifacts, function(artifact) artifact$section, character(1))))
}

#' Generate Binary Classification Model Insights Artifacts
#'
#' Generates a structured artifact set for binary classification model insights.
#' The output is intended for report rendering, Shiny display, export workflows,
#' and downstream artifact orchestration. It includes model overview artifacts,
#' classification metrics, threshold diagnostics, ROC/PR diagnostics,
#' calibration, lift/gains, feature importance proxies, and lightweight partial
#' dependence diagnostics. SHAP and multiclass support are intentionally out of
#' scope for this generator.
#'
#' @param TrainDataInclude Logical. Include train data when available.
#' @param FeatureColumnNames Optional feature columns.
#' @param SampleSize Maximum rows sampled from train/test data.
#' @param data Optional scored/model-output data. When supplied, this is the
#'   preferred artifact-generator-first path and should contain target and
#'   prediction columns.
#' @param target_col,prediction_col,predicted_class_col Modern aliases for the
#'   target, probability, and optional predicted class columns.
#' @param positive_class Modern alias for `PositiveClass`.
#' @param feature_cols Modern alias for `FeatureColumnNames`.
#' @param model_name,data_name Optional labels stored in report metadata.
#' @param DateVar Optional date column used for time diagnostics.
#' @param date_aggregation One of day, week, month, or quarter.
#' @param ByVars Optional segment columns used for segment diagnostics.
#' @param max_segment_levels Maximum segment levels retained per ByVar.
#' @param ModelObject Optional AutoQuant model object.
#' @param ModelID Optional model identifier.
#' @param SourcePath Optional source directory for model outputs.
#' @param OutputPath Optional output path retained in metadata.
#' @param PredictionColumnName Numeric prediction/probability column.
#' @param TargetColumnName Binary target column.
#' @param PositiveClass Optional positive class. If NULL, resolved from target values.
#' @param Threshold Default operating threshold.
#' @param OptimizeMetric Metric used to select the optimized threshold. Allowed
#'   values are Utility, Accuracy, BalancedAccuracy, F1, FBeta,
#'   MatthewsCorrelation, YoudensJ, and CohenKappa.
#' @param UtilityTP,UtilityTN,UtilityFP,UtilityFN Utility weights for threshold optimization.
#' @param Beta Positive beta value for F-beta.
#' @param Theme Plot theme.
#' @param ... Optional TrainData and TestData inputs.
#'
#' @return A named list with `artifacts` and `metadata`, plus grouped `tables`,
#' `plots`, `layout`, and `context` entries.
#'
#' @family Reports
#' @export
generate_binary_classification_model_insights_artifacts <- function(
  data = NULL,
  target_col = NULL,
  prediction_col = NULL,
  predicted_class_col = NULL,
  positive_class = NULL,
  feature_cols = NULL,
  model_name = NULL,
  data_name = NULL,
  DateVar = NULL,
  date_aggregation = "month",
  ByVars = character(),
  max_segment_levels = 25L,
  TrainDataInclude = FALSE,
  FeatureColumnNames = NULL,
  SampleSize = 100000L,
  ModelObject = NULL,
  ModelID = NULL,
  SourcePath = NULL,
  OutputPath = NULL,
  PredictionColumnName = NULL,
  TargetColumnName = NULL,
  PositiveClass = NULL,
  Threshold = 0.5,
  OptimizeMetric = "Utility",
  UtilityTP = 1,
  UtilityTN = 0,
  UtilityFP = -1,
  UtilityFN = -5,
  Beta = 1,
  Theme = "dark",
  ...
) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required.", call. = FALSE)
  }
  if (!requireNamespace("AutoPlots", quietly = TRUE)) {
    stop("Package 'AutoPlots' is required.", call. = FALSE)
  }

  if (!is.numeric(Threshold) || length(Threshold) != 1L || !is.finite(Threshold) || Threshold < 0 || Threshold > 1) {
    stop("Threshold must be a numeric value between 0 and 1.", call. = FALSE)
  }
  utility_values <- c(UtilityTP, UtilityTN, UtilityFP, UtilityFN)
  if (!all(is.numeric(utility_values)) || any(!is.finite(utility_values))) {
    stop("UtilityTP, UtilityTN, UtilityFP, and UtilityFN must be finite numeric values.", call. = FALSE)
  }
  if (!is.numeric(Beta) || length(Beta) != 1L || !is.finite(Beta) || Beta <= 0) {
    stop("Beta must be a positive numeric value.", call. = FALSE)
  }
  if (!is.numeric(SampleSize) || length(SampleSize) != 1L || !is.finite(SampleSize) || SampleSize <= 0) {
    stop("SampleSize must be a positive numeric value.", call. = FALSE)
  }
  if (!is.numeric(max_segment_levels) || length(max_segment_levels) != 1L || !is.finite(max_segment_levels) || max_segment_levels <= 0) {
    stop("max_segment_levels must be a positive numeric value.", call. = FALSE)
  }

  if (!is.null(data)) {
    extra <- list(...)
    if (is.null(extra$TestData)) {
      extra$TestData <- data
    }
    if (is.null(TargetColumnName)) {
      TargetColumnName <- target_col
    }
    if (is.null(PredictionColumnName)) {
      PredictionColumnName <- prediction_col
    }
    if (is.null(PositiveClass)) {
      PositiveClass <- positive_class
    }
    if (is.null(FeatureColumnNames)) {
      FeatureColumnNames <- feature_cols
    }
    if (is.null(ModelID)) {
      ModelID <- model_name
    }
  } else {
    extra <- list(...)
  }

  optimize_column <- binary_model_insights_resolve_optimize_metric(OptimizeMetric)
  inputs <- binary_model_insights_extract_inputs(
    TrainDataInclude = TrainDataInclude,
    FeatureColumnNames = FeatureColumnNames,
    SampleSize = as.integer(SampleSize),
    ModelObject = ModelObject,
    ModelID = ModelID,
    SourcePath = SourcePath,
    OutputPath = OutputPath,
    PredictionColumnName = PredictionColumnName,
    TargetColumnName = TargetColumnName,
    PositiveClass = PositiveClass,
    Theme = Theme,
    TrainData = extra$TrainData,
    TestData = extra$TestData
  )

  scored <- data.table::copy(inputs$TestData)
  scored[, prediction := as.numeric(get(inputs$PredictionColumnName))]
  scored[, actual_binary := as.integer(as.character(get(inputs$TargetColumnName)) == as.character(inputs$PositiveClass))]
  if (!is.null(predicted_class_col) && predicted_class_col %in% names(scored)) {
    scored[, predicted_class := as.character(get(predicted_class_col))]
  } else {
    scored[, predicted_class := ifelse(prediction >= Threshold, as.character(inputs$PositiveClass), as.character(inputs$NegativeClass))]
  }
  scored <- scored[is.finite(prediction) & !is.na(actual_binary)]

  thresholds <- sort(unique(c(seq(0, 1, by = 0.01), Threshold)))
  threshold_metrics <- binary_model_insights_threshold_metrics(
    scored,
    TargetColumnName = "actual_binary",
    PredictionColumnName = "prediction",
    PositiveClass = 1L,
    Thresholds = thresholds,
    UtilityTP = UtilityTP,
    UtilityTN = UtilityTN,
    UtilityFP = UtilityFP,
    UtilityFN = UtilityFN,
    Beta = Beta
  )

  best_row <- threshold_metrics[which.max(get(optimize_column))]
  default_row <- threshold_metrics[which.min(abs(threshold - Threshold))]
  selected_summary <- data.table::data.table(
    default_threshold = Threshold,
    optimized_threshold = best_row$threshold,
    optimize_metric = OptimizeMetric,
    optimized_metric_value = best_row[[optimize_column]],
    utility_tp = UtilityTP,
    utility_tn = UtilityTN,
    utility_fp = UtilityFP,
    utility_fn = UtilityFN,
    optimized_precision = best_row$precision,
    optimized_recall = best_row$recall,
    optimized_specificity = best_row$specificity,
    optimized_accuracy = best_row$accuracy,
    optimized_balanced_accuracy = best_row$balanced_accuracy,
    optimized_f1_score = best_row$f1_score,
    optimized_matthews_correlation = best_row$matthews_correlation,
    default_precision = default_row$precision,
    default_recall = default_row$recall,
    default_specificity = default_row$specificity,
    default_accuracy = default_row$accuracy,
    default_balanced_accuracy = default_row$balanced_accuracy,
    default_f1_score = default_row$f1_score,
    default_matthews_correlation = default_row$matthews_correlation
  )
  classification_metrics <- data.table::rbindlist(list(
    data.table::data.table(
      threshold_type = "default",
      threshold = Threshold,
      precision = default_row$precision,
      recall = default_row$recall,
      specificity = default_row$specificity,
      accuracy = default_row$accuracy,
      balanced_accuracy = default_row$balanced_accuracy,
      f1_score = default_row$f1_score,
      f_beta_score = default_row$f_beta_score,
      matthews_correlation = default_row$matthews_correlation,
      youdens_j = default_row$youdens_j,
      cohen_kappa = default_row$cohen_kappa,
      utility_total = default_row$utility_total,
      utility_per_observation = default_row$utility_per_observation
    ),
    data.table::data.table(
      threshold_type = "optimized",
      threshold = best_row$threshold,
      precision = best_row$precision,
      recall = best_row$recall,
      specificity = best_row$specificity,
      accuracy = best_row$accuracy,
      balanced_accuracy = best_row$balanced_accuracy,
      f1_score = best_row$f1_score,
      f_beta_score = best_row$f_beta_score,
      matthews_correlation = best_row$matthews_correlation,
      youdens_j = best_row$youdens_j,
      cohen_kappa = best_row$cohen_kappa,
      utility_total = best_row$utility_total,
      utility_per_observation = best_row$utility_per_observation
    )
  ), use.names = TRUE, fill = TRUE)
  confusion <- binary_model_insights_confusion_matrix(best_row)
  calibration_table <- binary_model_insights_build_calibration_table(scored)
  gains_table <- binary_model_insights_build_gains_table(scored)
  feature_summary <- binary_model_insights_feature_summary(scored, inputs$FeatureColumnNames)
  prediction_distribution <- binary_model_insights_build_prediction_distribution(scored)
  segment_diagnostics <- binary_model_insights_build_segment_diagnostics(
    scored,
    ByVars = ByVars,
    threshold = best_row$threshold,
    max_levels = as.integer(max_segment_levels)
  )
  time_diagnostics <- binary_model_insights_build_time_diagnostics(
    scored,
    DateVar = DateVar,
    date_aggregation = date_aggregation,
    threshold = best_row$threshold
  )

  tables <- list(
    model_overview = data.table::data.table(
      model_name = binary_model_insights_null_coalesce(
        binary_model_insights_null_coalesce(inputs$ModelID, model_name),
        NA_character_
      ),
      data_name = binary_model_insights_null_coalesce(data_name, NA_character_),
      target_column = inputs$TargetColumnName,
      prediction_column = inputs$PredictionColumnName,
      predicted_class_column = binary_model_insights_null_coalesce(predicted_class_col, NA_character_),
      positive_class = as.character(inputs$PositiveClass),
      negative_class = as.character(inputs$NegativeClass),
      rows = nrow(scored),
      features = length(inputs$FeatureColumnNames),
      date_var = binary_model_insights_null_coalesce(DateVar, NA_character_),
      date_aggregation = binary_model_insights_null_coalesce(date_aggregation, NA_character_),
      by_vars = paste(ByVars, collapse = ", ")
    ),
    classification_metrics = classification_metrics,
    threshold_metrics = threshold_metrics,
    selected_threshold_summary = selected_summary,
    optimized_confusion_matrix = confusion,
    calibration = calibration_table,
    gains = gains_table,
    prediction_distribution = prediction_distribution,
    feature_importance_proxy = feature_summary,
    segment_diagnostics = segment_diagnostics,
    time_diagnostics = time_diagnostics
  )

  plots <- list(
    threshold_optimizer = binary_model_insights_build_threshold_optimizer_plot(
      threshold_metrics,
      default_threshold = Threshold,
      optimized_threshold = best_row$threshold,
      optimize_column = optimize_column
    ),
    roc_curve = binary_model_insights_build_roc_plot(scored),
    precision_recall_curve = binary_model_insights_build_pr_plot(threshold_metrics),
    calibration_plot = binary_model_insights_build_calibration_plot(calibration_table),
    gains_plot = binary_model_insights_build_gains_plot(gains_table),
    lift_plot = binary_model_insights_build_lift_plot(gains_table),
    prediction_distribution = binary_model_insights_build_prediction_distribution_plot(prediction_distribution),
    segment_diagnostics = binary_model_insights_build_segment_plot(segment_diagnostics),
    time_diagnostics = binary_model_insights_build_time_plot(time_diagnostics)
  )
  plots <- Filter(Negate(is.null), plots)
  feature_plots <- binary_model_insights_build_feature_plots(
    scored,
    feature_summary,
    target_col = inputs$TargetColumnName,
    prediction_col = inputs$PredictionColumnName
  )

  artifacts <- list()
  common_metadata <- list(
    source_package = "AutoQuant",
    source_function = "generate_binary_classification_model_insights_artifacts",
    problem_type = "binary_classification",
    model_name = binary_model_insights_null_coalesce(
      binary_model_insights_null_coalesce(inputs$ModelID, model_name),
      NA_character_
    ),
    data_name = binary_model_insights_null_coalesce(data_name, NA_character_),
    source_path = inputs$SourcePath,
    target_column = inputs$TargetColumnName,
    prediction_column = inputs$PredictionColumnName,
    predicted_class_column = predicted_class_col,
    positive_class = as.character(inputs$PositiveClass),
    DateVar = DateVar,
    date_aggregation = date_aggregation,
    ByVars = ByVars,
    optimized_threshold = best_row$threshold,
    default_threshold = Threshold
  )

  artifact_specs <- list(
    list("model_overview", "Model Overview", "table", "Model Overview", tables$model_overview),
    list("classification_metrics", "Classification Metrics", "table", "Classification Metrics", tables$classification_metrics),
    list("threshold_metrics", "Threshold Metrics", "table", "Threshold Diagnostics", tables$threshold_metrics),
    list("threshold_optimizer", "Threshold Optimizer", "plot", "Threshold Diagnostics", plots$threshold_optimizer),
    list("selected_threshold_summary", "Selected Threshold Summary", "table", "Threshold Diagnostics", tables$selected_threshold_summary),
    list("optimized_confusion_matrix", "Optimized Confusion Matrix", "table", "Threshold Diagnostics", tables$optimized_confusion_matrix),
    list("roc_curve", "ROC Curve", "plot", "ROC / PR Analysis", plots$roc_curve),
    list("precision_recall_curve", "Precision-Recall Curve", "plot", "ROC / PR Analysis", plots$precision_recall_curve),
    list("calibration_table", "Calibration Table", "table", "Calibration", tables$calibration),
    list("calibration_plot", "Calibration Plot", "plot", "Calibration", plots$calibration_plot),
    list("gains_table", "Gains Table", "table", "Lift / Gains", tables$gains),
    list("gains_plot", "Gains Plot", "plot", "Lift / Gains", plots$gains_plot),
    list("lift_plot", "Lift Plot", "plot", "Lift / Gains", plots$lift_plot),
    list("prediction_distribution_table", "Prediction Distribution Table", "table", "Prediction Distribution", tables$prediction_distribution),
    list("prediction_distribution", "Prediction Distribution", "plot", "Prediction Distribution", plots$prediction_distribution),
    list("feature_importance_proxy", "Feature Importance Proxy", "table", "Feature Effects", tables$feature_importance_proxy),
    list("segment_diagnostics_table", "Segment Diagnostics Table", "table", "Segment Diagnostics", tables$segment_diagnostics),
    list("segment_diagnostics", "Segment Diagnostics", "plot", "Segment Diagnostics", plots$segment_diagnostics),
    list("time_diagnostics_table", "Time Diagnostics Table", "table", "Time Diagnostics", tables$time_diagnostics),
    list("time_diagnostics", "Time Diagnostics", "plot", "Time Diagnostics", plots$time_diagnostics)
  )

  for (spec in artifact_specs) {
    if (!is.null(spec[[5L]]) && (!data.table::is.data.table(spec[[5L]]) || nrow(spec[[5L]]))) {
      artifacts <- binary_model_insights_add_artifact(
        artifacts,
        binary_model_insights_artifact(spec[[1L]], spec[[2L]], spec[[3L]], spec[[4L]], spec[[5L]], common_metadata)
      )
    }
  }

  if (length(feature_plots)) {
    for (plot_name in names(feature_plots)) {
      feature_label <- gsub("_", " ", plot_name)
      artifacts <- binary_model_insights_add_artifact(
        artifacts,
        binary_model_insights_artifact(
          paste0("partial_dependence_", plot_name),
          tools::toTitleCase(paste("Partial Dependence", feature_label)),
          "plot",
          "Feature Effects",
          feature_plots[[plot_name]],
          common_metadata
        )
      )
    }
  }

  artifact_index <- binary_model_insights_as_artifact_table(artifacts)
  metadata <- c(
    inputs$metadata,
    list(
      source_package = "AutoQuant",
      source_function = "generate_binary_classification_model_insights_artifacts",
      problem_type = "binary_classification",
      model_name = binary_model_insights_null_coalesce(
        binary_model_insights_null_coalesce(inputs$ModelID, model_name),
        NA_character_
      ),
      data_name = binary_model_insights_null_coalesce(data_name, NA_character_),
      target_col = inputs$TargetColumnName,
      prediction_col = inputs$PredictionColumnName,
      predicted_class_col = predicted_class_col,
      positive_class = as.character(inputs$PositiveClass),
      DateVar = DateVar,
      date_aggregation = date_aggregation,
      ByVars = ByVars,
      feature_cols = inputs$FeatureColumnNames,
      selected_features = inputs$FeatureColumnNames,
      generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      threshold = Threshold,
      optimize_metric = OptimizeMetric,
      optimized_threshold = best_row$threshold,
      utility = list(TP = UtilityTP, TN = UtilityTN, FP = UtilityFP, FN = UtilityFN),
      beta = Beta,
      artifact_count = length(artifacts),
      plot_count = sum(artifact_index$type == "plot"),
      table_count = sum(artifact_index$type == "table"),
      text_count = sum(artifact_index$type == "text"),
      warnings_count = 0L,
      artifact_index = artifact_index
    )
  )

  out <- list(
    report_type = "binary_classification_model_insights",
    artifacts = artifacts,
    metadata = metadata,
    tables = tables,
    plots = c(plots, list(feature_effects = feature_plots)),
    layout = list(
      sections = c(
        "Model Overview",
        "Classification Metrics",
        "Threshold Diagnostics",
        "ROC / PR Analysis",
        "Calibration",
        "Lift / Gains",
        "Prediction Distribution",
        "Feature Effects",
        "Segment Diagnostics",
        "Time Diagnostics",
        "Appendix"
      )
    ),
    context = list(
      ArtifactInstruction = paste(
        "Use artifacts for rendering and export.",
        "Threshold diagnostics include all thresholds from 0 to 1.",
        "Counts and predicted-positive rates are diagnostic columns, not optimization metrics.",
        "SHAP and multiclass artifacts are intentionally excluded."
      )
    )
  )

  class(out) <- c(
    "autoquant_binary_classification_model_insights_artifacts",
    "autoquant_report_artifacts",
    "list"
  )

  out
}

#' QA for Binary Classification Model Insights Artifacts
#'
#' @return A compact data.table of QA checks.
#'
#' @family Reports
#' @export
qa_generate_binary_classification_model_insights_artifacts <- function() {
  set.seed(42)
  n <- 250L
  data <- data.table::data.table(
    event_date = as.Date("2026-01-01") + sample(0:120, n, replace = TRUE),
    x1 = stats::rnorm(n),
    x2 = stats::runif(n),
    segment = sample(c("A", "B", "C"), n, replace = TRUE)
  )
  logit <- -0.4 + 1.2 * data$x1 - 0.7 * data$x2 + ifelse(data$segment == "A", 0.4, 0)
  prob <- 1 / (1 + exp(-logit))
  data[, target := stats::rbinom(.N, 1L, prob)]
  data[, p1 := pmin(pmax(prob + stats::rnorm(.N, 0, 0.08), 0.001), 0.999)]

  result <- tryCatch(
    generate_binary_classification_model_insights_artifacts(
      data = data,
      target_col = "target",
      prediction_col = "p1",
      positive_class = 1L,
      feature_cols = c("x1", "x2", "segment"),
      DateVar = "event_date",
      ByVars = "segment",
      model_name = "qa_binary_model",
      data_name = "qa_scored_data",
      OptimizeMetric = "Utility",
      Theme = "dark"
    ),
    error = function(e) e
  )

  rejected <- tryCatch(
    {
      generate_binary_classification_model_insights_artifacts(
        TestData = data,
        TargetColumnName = "target",
        PredictionColumnName = "p1",
        PositiveClass = 1L,
        OptimizeMetric = "TP"
      )
      FALSE
    },
    error = function(e) TRUE
  )

  artifacts <- if (inherits(result, "error")) list() else result$artifacts
  artifact_names <- names(artifacts)
  artifact_labels <- vapply(artifacts, function(x) if (is.null(x$label)) "" else x$label, character(1L))
  artifact_sections <- vapply(artifacts, function(x) if (is.null(x$section)) "" else x$section, character(1L))
  artifact_types <- vapply(artifacts, function(x) if (is.null(x$type)) "" else x$type, character(1L))
  required_artifacts <- c(
    "threshold_metrics",
    "threshold_optimizer",
    "selected_threshold_summary",
    "optimized_confusion_matrix",
    "prediction_distribution",
    "segment_diagnostics",
    "time_diagnostics"
  )

  data.table::data.table(
    check = c(
      "generator_runs",
      "artifacts_list_exists",
      "has_table_artifact",
      "has_plot_artifact",
      "threshold_required_artifacts",
      "rejects_component_optimize_metric",
      "artifact_contract",
      "no_duplicate_names",
      "labels_non_empty",
      "sections_non_empty"
    ),
    status = c(
      if (!inherits(result, "error")) "success" else "error",
      if (length(artifacts)) "success" else "error",
      if (any(artifact_types == "table")) "success" else "error",
      if (any(artifact_types == "plot")) "success" else "error",
      if (all(required_artifacts %in% artifact_names)) "success" else "error",
      if (isTRUE(rejected)) "success" else "error",
      if (length(artifacts) && binary_model_insights_validate_artifacts(artifacts)) "success" else "error",
      if (!anyDuplicated(artifact_names)) "success" else "error",
      if (length(artifact_labels) && all(nzchar(artifact_labels))) "success" else "error",
      if (length(artifact_sections) && all(nzchar(artifact_sections))) "success" else "error"
    ),
    message = c(
      if (!inherits(result, "error")) "Generator completed." else conditionMessage(result),
      paste("Artifacts:", length(artifacts)),
      paste("Table artifacts:", sum(artifact_types == "table")),
      paste("Plot artifacts:", sum(artifact_types == "plot")),
      paste("Required threshold artifacts present:", all(required_artifacts %in% artifact_names)),
      "Component counts are rejected as OptimizeMetric.",
      "Every artifact includes name, label, type, section, object, and metadata.",
      "Artifact names are unique.",
      "Artifact labels are non-empty.",
      "Artifact sections are non-empty."
    )
  )
}
