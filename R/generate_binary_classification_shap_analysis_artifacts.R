# ============================================================
# Binary Classification SHAP Analysis Artifact Generation
#
# This generator consumes precomputed Shap_ columns returned by AutoQuant
# modeling/scoring outputs. It does not compute SHAP values, call models,
# call predict(), or invoke any SHAP backend package.
# ============================================================

binary_shap_sections <- function() {
  c(
    "SHAP Overview",
    "Global Importance",
    "Threshold Context",
    "Class Balance / Outcome Context",
    "Interaction Importance",
    "Single Feature Effects",
    "SHAP Dependence",
    "Segment Effects",
    "Time Effects",
    "Local Explanations",
    "Appendix"
  )
}

binary_shap_artifact <- function(
  name,
  label,
  type,
  section,
  object = NULL,
  content = NULL,
  metadata = list()
) {
  regression_shap_artifact(
    name = name,
    label = label,
    type = type,
    section = section,
    object = object,
    content = content,
    metadata = metadata
  )
}

binary_shap_add_artifact <- function(artifacts, artifact) {
  regression_shap_add_artifact(artifacts, artifact)
}

aq_binary_shap_empty_result <- function(message, warnings = character(), diagnostics = list()) {
  generated_at <- Sys.time()
  diagnostic_table <- if (length(diagnostics)) {
    data.table::data.table(
      diagnostic = names(diagnostics),
      value = vapply(diagnostics, function(x) paste(x, collapse = ", "), character(1L))
    )
  } else {
    data.table::data.table(diagnostic = character(), value = character())
  }
  artifacts <- list(
    shap_diagnostics = binary_shap_artifact(
      name = "shap_diagnostics",
      label = "Binary SHAP Diagnostics",
      type = "table",
      section = "Appendix",
      object = diagnostic_table,
      metadata = list(
        source_package = "AutoQuant",
        source_function = "generate_binary_classification_shap_analysis_artifacts",
        problem_type = "binary_classification",
        lens = "diagnostics",
        original_section = "Appendix",
        normalized_section = "Appendix",
        artifact_index = 1L,
        shap_source = "precomputed_columns",
        exact_shap_interaction_values = FALSE,
        created_by_autoquant = TRUE
      )
    )
  )
  list(
    artifacts = artifacts,
    metadata = list(
      source_package = "AutoQuant",
      source_function = "generate_binary_classification_shap_analysis_artifacts",
      problem_type = "binary_classification",
      generated_at = generated_at,
      artifact_count = length(artifacts),
      plot_count = 0L,
      table_count = 1L,
      text_count = 0L,
      warnings_count = length(warnings),
      shap_source = "precomputed_columns",
      exact_shap_interaction_values = FALSE
    ),
    warnings = warnings,
    diagnostics = diagnostics,
    code = "generate_binary_classification_shap_analysis_artifacts(data = scoring_data_with_precomputed_shap_columns)"
  )
}

aq_binary_shap_normalize_scale <- function(prediction_scale, warnings) {
  scale <- tolower(as.character(prediction_scale[[1L]]))
  if (!scale %in% c("probability", "logit", "margin", "unknown")) {
    warnings <- regression_shap_warn(warnings, "prediction_scale must be probability, logit, margin, or unknown; using unknown.")
    scale <- "unknown"
  }
  list(value = scale, warnings = warnings)
}

aq_binary_shap_actual_positive <- function(data, target_col, positive_class) {
  if (is.null(target_col) || is.null(positive_class) || !target_col %in% names(data)) {
    return(NULL)
  }
  as.character(data[[target_col]]) == as.character(positive_class)
}

aq_binary_shap_threshold_band <- function(prediction, threshold = 0.5, threshold_bands = NULL) {
  if (!is.null(threshold_bands)) {
    return(as.character(threshold_bands))
  }
  margin <- 0.1
  data.table::fifelse(
    prediction < threshold - margin,
    "below threshold",
    data.table::fifelse(prediction > threshold + margin, "above threshold", "near threshold")
  )
}

aq_binary_shap_class_balance <- function(data, target_col, prediction_col, positive_class) {
  if (is.null(target_col) || is.null(positive_class) || !target_col %in% names(data)) {
    return(data.table::data.table())
  }
  dt <- data.table::data.table(
    target = as.character(data[[target_col]]),
    actual_positive = as.character(data[[target_col]]) == as.character(positive_class)
  )
  if (!is.null(prediction_col) && prediction_col %in% names(data) && is.numeric(data[[prediction_col]])) {
    dt[, prediction := data[[prediction_col]]]
  }
  out <- dt[, .(
    n = .N,
    positive_rate = mean(actual_positive, na.rm = TRUE),
    mean_prediction = if ("prediction" %in% names(.SD)) mean(prediction, na.rm = TRUE) else NA_real_
  ), by = target]
  data.table::setorderv(out, "n", order = -1L)
  out[]
}

aq_binary_shap_class_feature_importance <- function(data, column_map, target_col, prediction_col, positive_class) {
  actual_positive <- aq_binary_shap_actual_positive(data, target_col, positive_class)
  if (is.null(actual_positive)) {
    return(data.table::data.table())
  }
  valid <- column_map[included == TRUE]
  rows <- lapply(seq_len(nrow(valid)), function(i) {
    values <- data[[valid$shap_col[[i]]]]
    dt <- data.table::data.table(
      feature = valid$feature[[i]],
      shap_col = valid$shap_col[[i]],
      target = as.character(data[[target_col]]),
      actual_positive = actual_positive,
      shap_value = values
    )
    if (!is.null(prediction_col) && prediction_col %in% names(data) && is.numeric(data[[prediction_col]])) {
      dt[, prediction := data[[prediction_col]]]
    }
    dt[, .(
      mean_abs_shap = mean(abs(shap_value), na.rm = TRUE),
      mean_shap = mean(shap_value, na.rm = TRUE),
      median_shap = stats::median(shap_value, na.rm = TRUE),
      mean_prediction = if ("prediction" %in% names(.SD)) mean(prediction, na.rm = TRUE) else NA_real_,
      positive_rate = mean(actual_positive, na.rm = TRUE),
      n = .N
    ), by = .(target, actual_positive, feature, shap_col)]
  })
  out <- data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
  data.table::setorderv(out, c("target", "mean_abs_shap"), order = c(1L, -1L), na.last = TRUE)
  out[, rank_within_class := seq_len(.N), by = target]
  out[]
}

aq_binary_shap_threshold_context <- function(
  data,
  column_map,
  prediction_col,
  target_col = NULL,
  positive_class = NULL,
  threshold = 0.5,
  threshold_bands = NULL,
  prediction_scale = "probability"
) {
  if (is.null(prediction_col) || !prediction_col %in% names(data) || !is.numeric(data[[prediction_col]])) {
    return(data.table::data.table())
  }
  if (!identical(prediction_scale, "probability") && is.null(threshold_bands)) {
    return(data.table::data.table())
  }
  prediction <- data[[prediction_col]]
  bands <- aq_binary_shap_threshold_band(prediction, threshold, threshold_bands)
  actual_positive <- aq_binary_shap_actual_positive(data, target_col, positive_class)
  valid <- column_map[included == TRUE]
  rows <- lapply(seq_len(nrow(valid)), function(i) {
    values <- data[[valid$shap_col[[i]]]]
    dt <- data.table::data.table(
      threshold = threshold,
      prediction_band = bands,
      feature = valid$feature[[i]],
      shap_col = valid$shap_col[[i]],
      shap_value = values,
      prediction = prediction,
      predicted_positive = prediction >= threshold
    )
    if (!is.null(actual_positive)) {
      dt[, actual_positive := actual_positive]
    }
    dt[, .(
      mean_abs_shap = mean(abs(shap_value), na.rm = TRUE),
      mean_shap = mean(shap_value, na.rm = TRUE),
      median_shap = stats::median(shap_value, na.rm = TRUE),
      n = .N,
      positive_rate = if ("actual_positive" %in% names(.SD)) mean(actual_positive, na.rm = TRUE) else NA_real_,
      predicted_positive_rate = mean(predicted_positive, na.rm = TRUE),
      mean_prediction = mean(prediction, na.rm = TRUE)
    ), by = .(threshold, prediction_band, feature, shap_col)]
  })
  out <- data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
  data.table::setorderv(out, c("prediction_band", "mean_abs_shap"), order = c(1L, -1L), na.last = TRUE)
  out[, rank_within_band := seq_len(.N), by = prediction_band]
  out[]
}

aq_binary_shap_add_binary_context <- function(dt, data, row_col, target_col, prediction_col, positive_class, threshold, id_cols = character()) {
  if (!nrow(dt) || !row_col %in% names(dt)) {
    return(dt)
  }
  row_id <- dt[[row_col]]
  if (!is.null(target_col) && target_col %in% names(data)) {
    dt[, target := data[[target_col]][row_id]]
    if (!is.null(positive_class)) {
      dt[, actual_positive := as.character(target) == as.character(positive_class)]
    }
  }
  if (!is.null(prediction_col) && prediction_col %in% names(data) && is.numeric(data[[prediction_col]])) {
    dt[, prediction := data[[prediction_col]][row_id]]
    dt[, predicted_positive := prediction >= threshold]
  }
  for (col in intersect(id_cols, names(data))) {
    dt[, (col) := data[[col]][row_id]]
  }
  dt[]
}

aq_binary_shap_local_direction <- function(x) {
  data.table::fifelse(
    x > 0,
    "toward_positive_class",
    data.table::fifelse(x < 0, "away_from_positive_class", "neutral")
  )
}

aq_create_binary_shap_plot_artifact <- function(
  name,
  label,
  section,
  plot_type,
  lens,
  plot_object,
  metadata
) {
  binary_shap_artifact(
    name = name,
    label = label,
    type = "plot",
    section = section,
    object = plot_object,
    metadata = c(metadata, list(
      plot_package = "AutoPlots",
      plot_type = plot_type,
      lens = lens
    ))
  )
}

#' Generate Binary Classification SHAP Analysis Artifacts
#'
#' Summarize precomputed row-level binary classification SHAP columns into
#' structured artifacts. This function consumes columns whose names start with
#' `shap_prefix` and maps each one back to its source feature by stripping the
#' prefix. It does not compute SHAP values, call `predict()`, require a model
#' object, or use a SHAP backend package.
#'
#' @param data A data.table-like object containing precomputed `Shap_` columns.
#' @param target_col Optional actual class column.
#' @param prediction_col Optional predicted positive-class score/probability column. Defaults to `Predict` when present.
#' @param predicted_class_col Optional predicted class column.
#' @param positive_class Positive class label. Required for class-aware interpretation when `target_col` is supplied.
#' @param feature_cols Optional source features used to filter detected SHAP columns.
#' @param shap_prefix Prefix for SHAP contribution columns.
#' @param id_cols Optional ID/context columns to carry into dependence/local artifacts.
#' @param model_name Optional model name for overview metadata.
#' @param data_name Optional data name for overview metadata.
#' @param DateVar Optional date column for time effects.
#' @param date_aggregation One of `day`, `week`, or `month`.
#' @param ByVars Optional segment variables for segment effects.
#' @param selected_features Optional feature list for effect/dependence/local views.
#' @param local_row_ids Optional 1-based row indexes for local explanations.
#' @param threshold Probability threshold for threshold-context artifacts.
#' @param threshold_bands Optional precomputed threshold band vector.
#' @param top_n Number of top features used for display-oriented artifacts.
#' @param max_dependence_rows Maximum source rows per selected feature in dependence output.
#' @param max_segment_levels Maximum segment levels to keep per ByVar.
#' @param max_byvars Maximum ByVars to use.
#' @param include_dependence Include dependence artifacts.
#' @param include_segments Include segment effect artifacts.
#' @param include_time Include time effect artifacts.
#' @param include_local Include local explanation artifacts.
#' @param include_interactions Include binned/leveled SHAP interaction diagnostics from precomputed `Shap_` columns.
#' @param include_threshold_context Include threshold-context artifacts when prediction scores are available.
#' @param include_class_balance Include class/outcome context artifacts when target and positive class are available.
#' @param include_plots Include AutoPlots-backed plot artifacts when AutoPlots is installed.
#' @param max_feature_effect_plots Maximum single-feature effect plots.
#' @param max_dependence_plots Maximum dependence plots.
#' @param max_segment_plots Maximum segment plots.
#' @param max_time_plots Maximum time plots.
#' @param max_local_plots Maximum local explanation plots.
#' @param max_interaction_pairs Maximum candidate interaction pairs to score.
#' @param max_interaction_surface_plots Maximum two-way surface heatmap plots to create.
#' @param numeric_interaction_bins Maximum numeric bins for interaction diagnostics.
#' @param max_interaction_levels Maximum categorical/date levels for interaction diagnostics.
#' @param min_interaction_cell_n Minimum cell count before an interaction surface cell is flagged sparse.
#' @param interaction_stat Statistic for interaction surface scoring. One of `mean_abs_shap` or `mean_shap`.
#' @param interaction_score_stat Candidate interaction score statistic.
#' @param prediction_scale One of `probability`, `logit`, `margin`, or `unknown`.
#' @param ... Compatibility arguments are ignored. Model, prediction function, sampling, and SHAP backend arguments are not used.
#'
#' @return A structured list with `artifacts`, `metadata`, `warnings`, `diagnostics`, and `code`.
#'
#' @details
#' The input data must already contain precomputed SHAP contribution columns.
#' For example, `Shap_Impressions` maps to source feature `Impressions`.
#' `positive_class` and `prediction_scale` make the binary interpretation
#' explicit. Threshold artifacts are created only when compatible prediction
#' scores are available. Interaction diagnostics use binned/leveled ordinary
#' SHAP columns and source variables; they are not exact pairwise SHAP
#' interaction values. Multiclass SHAP is deferred.
#'
#' @export
generate_binary_classification_shap_analysis_artifacts <- function(
  data,
  target_col = NULL,
  prediction_col = NULL,
  predicted_class_col = NULL,
  positive_class = NULL,
  feature_cols = NULL,
  shap_prefix = "Shap_",
  id_cols = NULL,
  model_name = NULL,
  data_name = NULL,
  DateVar = NULL,
  date_aggregation = "month",
  ByVars = character(),
  selected_features = NULL,
  local_row_ids = integer(),
  threshold = 0.5,
  threshold_bands = NULL,
  top_n = 20L,
  max_dependence_rows = 5000L,
  max_segment_levels = 20L,
  max_byvars = 3L,
  include_dependence = TRUE,
  include_segments = TRUE,
  include_time = TRUE,
  include_local = FALSE,
  include_interactions = TRUE,
  include_threshold_context = TRUE,
  include_class_balance = TRUE,
  include_plots = TRUE,
  max_feature_effect_plots = 5L,
  max_dependence_plots = 5L,
  max_segment_plots = 5L,
  max_time_plots = 5L,
  max_local_plots = 5L,
  max_interaction_pairs = 20L,
  max_interaction_surface_plots = 10L,
  numeric_interaction_bins = 5L,
  max_interaction_levels = 12L,
  min_interaction_cell_n = 5L,
  interaction_stat = "mean_abs_shap",
  interaction_score_stat = "weighted_mad",
  collapse_rare_levels = TRUE,
  prediction_scale = c("probability", "logit", "margin", "unknown"),
  plot_top_n = NULL,
  auto_plots_theme = NULL,
  plot_width = NULL,
  plot_height = NULL,
  ...
) {
  warnings <- character()
  generated_at <- Sys.time()
  extra_args <- list(...)
  if (length(intersect(names(extra_args), c("model", "predict_function", "prediction_function", "background_n", "sample_n", "nsim", "shap_backend")))) {
    warnings <- regression_shap_warn(
      warnings,
      "Model, prediction, sampling, and SHAP backend arguments are ignored. This generator only summarizes precomputed Shap_ columns."
    )
  }

  if (!data.table::is.data.table(data)) {
    data <- tryCatch(data.table::as.data.table(data), error = function(e) NULL)
  }
  if (!data.table::is.data.table(data)) {
    return(aq_binary_shap_empty_result(
      "data must be data.table-like.",
      warnings = regression_shap_warn(warnings, "data must be data.table-like."),
      diagnostics = list(error = "invalid_data")
    ))
  }
  data <- data.table::copy(data)

  scale_result <- aq_binary_shap_normalize_scale(prediction_scale, warnings)
  prediction_scale <- scale_result$value
  warnings <- scale_result$warnings

  if (!is.character(shap_prefix) || length(shap_prefix) != 1L || !nzchar(shap_prefix)) {
    return(aq_binary_shap_empty_result(
      "shap_prefix must be a non-empty character value.",
      warnings = regression_shap_warn(warnings, "shap_prefix must be a non-empty character value."),
      diagnostics = list(error = "invalid_shap_prefix")
    ))
  }

  date_aggregation <- tolower(as.character(date_aggregation))
  if (!date_aggregation %in% c("day", "week", "month")) {
    warnings <- regression_shap_warn(warnings, "date_aggregation must be day, week, or month; using month.")
    date_aggregation <- "month"
  }

  for (limit in c("top_n", "max_dependence_rows", "max_segment_levels", "max_byvars", "max_interaction_pairs", "max_interaction_surface_plots", "numeric_interaction_bins", "max_interaction_levels", "min_interaction_cell_n", "max_feature_effect_plots", "max_dependence_plots", "max_segment_plots", "max_time_plots", "max_local_plots")) {
    checked <- regression_shap_positive_int(
      get(limit),
      default = switch(
        limit,
        top_n = 20L,
        max_dependence_rows = 5000L,
        max_segment_levels = 20L,
        max_byvars = 3L,
        max_interaction_pairs = 20L,
        max_interaction_surface_plots = 10L,
        numeric_interaction_bins = 5L,
        max_interaction_levels = 12L,
        min_interaction_cell_n = 5L,
        max_feature_effect_plots = 5L,
        max_dependence_plots = 5L,
        max_segment_plots = 5L,
        max_time_plots = 5L,
        max_local_plots = 5L
      ),
      name = limit,
      warnings = warnings
    )
    assign(limit, checked$value)
    warnings <- checked$warnings
  }
  if (is.null(plot_top_n)) {
    plot_top_n <- top_n
  } else {
    checked <- regression_shap_positive_int(plot_top_n, default = top_n, name = "plot_top_n", warnings = warnings)
    plot_top_n <- checked$value
    warnings <- checked$warnings
  }

  threshold <- suppressWarnings(as.numeric(threshold[[1L]]))
  if (is.na(threshold)) {
    warnings <- regression_shap_warn(warnings, "threshold must be numeric; using 0.5.")
    threshold <- 0.5
  }

  prediction_col <- aq_infer_prediction_col(data, prediction_col)
  if (!is.null(prediction_col) && !prediction_col %in% names(data)) {
    warnings <- regression_shap_warn(warnings, paste("prediction_col was not found and will be omitted:", prediction_col))
    prediction_col <- NULL
  }
  if (!is.null(target_col) && !target_col %in% names(data)) {
    warnings <- regression_shap_warn(warnings, paste("target_col was not found and will be omitted:", target_col))
    target_col <- NULL
  }
  if (!is.null(predicted_class_col) && !predicted_class_col %in% names(data)) {
    warnings <- regression_shap_warn(warnings, paste("predicted_class_col was not found and will be omitted:", predicted_class_col))
    predicted_class_col <- NULL
  }
  if (!is.null(target_col) && (is.null(positive_class) || !nzchar(as.character(positive_class)))) {
    return(aq_binary_shap_empty_result(
      "positive_class is required when target_col is supplied for binary SHAP interpretation.",
      warnings = regression_shap_warn(warnings, "positive_class is required when target_col is supplied for binary SHAP interpretation."),
      diagnostics = list(error = "missing_positive_class", target_col = target_col)
    ))
  }
  if (!is.null(target_col) && !is.null(positive_class)) {
    target_values <- unique(stats::na.omit(as.character(data[[target_col]])))
    if (length(target_values) > 2L) {
      warnings <- regression_shap_warn(warnings, paste("target_col has more than two observed classes:", paste(target_values, collapse = ", ")))
    }
    if (!as.character(positive_class) %in% target_values) {
      warnings <- regression_shap_warn(warnings, paste("positive_class was not observed in target_col:", positive_class))
    }
  }
  if (!is.null(prediction_col) && identical(prediction_scale, "probability")) {
    p <- data[[prediction_col]]
    valid_p <- p[is.finite(p)]
    if (length(valid_p) && mean(valid_p >= 0 & valid_p <= 1, na.rm = TRUE) < 0.95) {
      warnings <- regression_shap_warn(warnings, "prediction_scale was probability but prediction_col values are not mostly in [0, 1]; using unknown scale.")
      prediction_scale <- "unknown"
    }
  }

  id_cols <- as.character(regression_shap_null_coalesce(id_cols, character()))
  invalid_id_cols <- setdiff(id_cols, names(data))
  if (length(invalid_id_cols)) {
    warnings <- regression_shap_warn(warnings, paste("id_cols not found and omitted:", paste(invalid_id_cols, collapse = ", ")))
  }
  id_cols <- intersect(id_cols, names(data))

  ByVars <- as.character(regression_shap_null_coalesce(ByVars, character()))
  invalid_byvars <- setdiff(ByVars, names(data))
  if (length(invalid_byvars)) {
    warnings <- regression_shap_warn(warnings, paste("ByVars not found and omitted:", paste(invalid_byvars, collapse = ", ")))
  }
  ByVars <- intersect(ByVars, names(data))
  if (length(ByVars) > max_byvars) {
    warnings <- regression_shap_warn(warnings, paste("ByVars capped to max_byvars:", max_byvars))
    ByVars <- head(ByVars, max_byvars)
  }

  requested_DateVar <- DateVar
  DateVar <- aq_infer_date_col(data, DateVar)
  if (!is.null(requested_DateVar) && !identical(requested_DateVar, DateVar)) {
    if (is.null(DateVar)) {
      warnings <- regression_shap_warn(warnings, paste("DateVar not found and time effects will be omitted:", requested_DateVar))
    } else {
      warnings <- regression_shap_warn(warnings, paste("DateVar", requested_DateVar, "was not found exactly; using", DateVar, "for time effects."))
    }
  } else if (is.null(requested_DateVar) && !is.null(DateVar)) {
    warnings <- regression_shap_warn(warnings, paste("DateVar was inferred for time effects:", DateVar))
  }

  column_map <- aq_detect_shap_columns(data, shap_prefix = shap_prefix, feature_cols = feature_cols)
  if (!nrow(column_map) || !any(column_map$included)) {
    warnings <- regression_shap_warn(warnings, "No valid numeric SHAP columns were found.")
    return(aq_binary_shap_empty_result(
      "No valid numeric SHAP columns were found.",
      warnings = warnings,
      diagnostics = list(
        n_rows = nrow(data),
        n_columns = ncol(data),
        shap_prefix = shap_prefix,
        shap_cols_detected = names(data)[startsWith(names(data), shap_prefix)]
      )
    ))
  }
  if (any(!column_map$shap_is_numeric)) {
    warnings <- regression_shap_warn(warnings, "Non-numeric SHAP columns were dropped.")
  }
  if (any(!column_map$source_col_exists & column_map$included)) {
    warnings <- regression_shap_warn(warnings, "Some SHAP features do not have matching source columns; dependence/effect views skip those features.")
  }

  global_importance <- aq_summarize_shap_global_importance(data, column_map)
  categorical_level_importance <- aq_summarize_shap_categorical_level_importance(
    data,
    column_map,
    features = NULL,
    max_levels = max_segment_levels
  )
  selected_features <- as.character(regression_shap_null_coalesce(
    selected_features,
    global_importance$feature[seq_len(min(top_n, nrow(global_importance)))]
  ))
  missing_selected <- setdiff(selected_features, column_map[included == TRUE, feature])
  if (length(missing_selected)) {
    warnings <- regression_shap_warn(warnings, paste("selected_features did not map to valid SHAP columns and were omitted:", paste(missing_selected, collapse = ", ")))
  }
  selected_features <- intersect(selected_features, column_map[included == TRUE, feature])

  full_metadata <- list(
    source_package = "AutoQuant",
    source_function = "generate_binary_classification_shap_analysis_artifacts",
    problem_type = "binary_classification",
    model_name = model_name,
    data_name = data_name,
    target_col = target_col,
    prediction_col = prediction_col,
    predicted_class_col = predicted_class_col,
    positive_class = positive_class,
    prediction_scale = prediction_scale,
    threshold = threshold,
    feature_cols = feature_cols,
    shap_prefix = shap_prefix,
    shap_cols = column_map[included == TRUE, shap_col],
    DateVar = DateVar,
    date_aggregation = date_aggregation,
    ByVars = ByVars,
    id_cols = id_cols,
    selected_features = selected_features,
    top_n = top_n,
    max_dependence_rows = max_dependence_rows,
    max_segment_levels = max_segment_levels,
    max_interaction_pairs = max_interaction_pairs,
    max_interaction_surface_plots = max_interaction_surface_plots,
    numeric_interaction_bins = numeric_interaction_bins,
    max_interaction_levels = max_interaction_levels,
    min_interaction_cell_n = min_interaction_cell_n,
    interaction_stat = interaction_stat,
    interaction_score_stat = interaction_score_stat,
    generated_at = generated_at,
    shap_source = "precomputed_columns",
    shap_column_prefix = shap_prefix,
    interaction_method = "binned_level_combination_shap_diagnostic",
    exact_shap_interaction_values = FALSE
  )
  artifact_metadata <- function(lens, section, index) {
    c(full_metadata, list(
      lens = lens,
      original_section = section,
      normalized_section = section,
      artifact_index = index,
      created_by_autoquant = TRUE
    ))
  }

  actual_positive <- aq_binary_shap_actual_positive(data, target_col, positive_class)
  predicted_positive <- if (!is.null(prediction_col) && prediction_col %in% names(data) && is.numeric(data[[prediction_col]])) {
    data[[prediction_col]] >= threshold
  } else {
    NULL
  }

  config_table <- data.table::data.table(
    setting = c(
      "n_rows", "n_columns", "n_shap_columns_detected",
      "n_valid_numeric_shap_columns", "n_features_with_source_columns",
      "target_col", "prediction_col", "predicted_class_col",
      "positive_class", "prediction_scale", "threshold",
      "DateVar", "date_aggregation", "ByVars", "id_cols",
      "selected_features", "top_n", "warnings_count"
    ),
    value = c(
      nrow(data), ncol(data), nrow(column_map),
      nrow(column_map[included == TRUE]),
      nrow(column_map[included == TRUE & source_col_exists == TRUE]),
      regression_shap_null_coalesce(target_col, ""),
      regression_shap_null_coalesce(prediction_col, ""),
      regression_shap_null_coalesce(predicted_class_col, ""),
      regression_shap_null_coalesce(positive_class, ""),
      prediction_scale,
      threshold,
      regression_shap_null_coalesce(DateVar, ""),
      date_aggregation,
      paste(ByVars, collapse = ", "),
      paste(id_cols, collapse = ", "),
      paste(selected_features, collapse = ", "),
      top_n,
      length(warnings)
    )
  )

  overview <- paste(
    "Binary Classification SHAP Analysis",
    paste("Model:", regression_shap_null_coalesce(model_name, "not supplied")),
    paste("Data:", regression_shap_null_coalesce(data_name, "not supplied")),
    paste("Rows:", nrow(data)),
    paste("Valid SHAP columns:", nrow(column_map[included == TRUE])),
    paste("Mapped source features:", nrow(column_map[included == TRUE & source_col_exists == TRUE])),
    paste("Target column:", regression_shap_null_coalesce(target_col, "not supplied")),
    paste("Prediction column:", regression_shap_null_coalesce(prediction_col, "not supplied")),
    paste("Predicted class column:", regression_shap_null_coalesce(predicted_class_col, "not supplied")),
    paste("Positive class:", regression_shap_null_coalesce(positive_class, "not supplied")),
    paste("Prediction scale:", prediction_scale),
    paste("Threshold:", threshold),
    paste("DateVar/date aggregation:", paste(c(regression_shap_null_coalesce(DateVar, "not supplied"), date_aggregation), collapse = " / ")),
    paste("ByVars:", if (length(ByVars)) paste(ByVars, collapse = ", ") else "none"),
    paste("Top N:", top_n),
    "SHAP values were precomputed upstream. This generator only summarizes existing SHAP contribution columns.",
    if (!is.null(positive_class)) "SHAP values are interpreted as contributions toward or away from the positive class under the upstream modeling contract." else "Positive-class interpretation is limited because positive_class was not supplied.",
    if (identical(prediction_scale, "unknown")) "Prediction scale is unknown; signed SHAP interpretation should be treated cautiously." else paste("Signed SHAP values are reported on the supplied prediction scale:", prediction_scale),
    sep = "\n"
  )

  artifacts <- list()
  artifacts <- binary_shap_add_artifact(artifacts, binary_shap_artifact("shap_overview", "Binary SHAP Overview", "text", "SHAP Overview", content = overview, metadata = artifact_metadata("overview", "SHAP Overview", 1L)))
  artifacts <- binary_shap_add_artifact(artifacts, binary_shap_artifact("shap_diagnostics_config", "Binary SHAP Diagnostics / Config", "table", "Appendix", object = config_table, metadata = artifact_metadata("diagnostics", "Appendix", 2L)))
  artifacts <- binary_shap_add_artifact(artifacts, binary_shap_artifact("shap_column_map", "Binary SHAP Column Map", "table", "Appendix", object = column_map, metadata = artifact_metadata("column_map", "Appendix", 3L)))
  artifacts <- binary_shap_add_artifact(artifacts, binary_shap_artifact("global_importance_table", "Binary Global SHAP Importance", "table", "Global Importance", object = aq_smart_round_dt(global_importance, skip_cols = c("rank", "n")), metadata = artifact_metadata("global_importance", "Global Importance", 4L)))
  if (nrow(categorical_level_importance)) {
    artifacts <- binary_shap_add_artifact(artifacts, binary_shap_artifact("categorical_level_importance_table", "Categorical / Binned Numeric Level SHAP Importance", "table", "Global Importance", object = aq_smart_round_dt(categorical_level_importance, skip_cols = c("rank", "n")), metadata = artifact_metadata("categorical_level_importance", "Global Importance", 5L)))
  }

  if (isTRUE(include_plots)) {
    global_plot_data <- data.table::copy(global_importance[rank <= plot_top_n])
    data.table::setorderv(global_plot_data, "mean_abs_shap", order = 1L, na.last = TRUE)
    global_plot_data <- aq_apply_plot_category_order(global_plot_data, "feature")
    global_plot_data <- aq_smart_round_dt(global_plot_data, skip_cols = c("rank", "n"))
    global_plot <- aq_safe_create_shap_plot(
      "bar",
      aq_create_shap_bar_plot(
        dt = global_plot_data,
        XVar = "feature",
        YVar = "mean_abs_shap",
        title = "Mean Absolute SHAP Toward Positive Class",
        auto_plots_theme = auto_plots_theme,
        plot_width = plot_width,
        plot_height = plot_height
      )
    )
    if (!is.null(global_plot$object)) {
      global_plot$object <- aq_style_shap_plot(global_plot$object, horizontal = TRUE, x_axis_title = "")
      artifacts <- binary_shap_add_artifact(artifacts, aq_create_binary_shap_plot_artifact("global_importance_plot", "Binary Global SHAP Importance Plot", "Global Importance", "bar", "global_importance", global_plot$object, artifact_metadata("global_importance", "Global Importance", 6L)))
    } else {
      warnings <- regression_shap_warn(warnings, global_plot$warning)
    }

    shap_distribution <- aq_regression_shap_long_values(data, column_map, global_importance[rank <= plot_top_n, feature], max_rows = max_dependence_rows)
    if (nrow(shap_distribution)) {
      shap_distribution <- aq_order_for_flipped_box(shap_distribution, "feature", "shap_value")
      distribution_plot <- aq_safe_create_shap_plot(
        "box",
        aq_create_shap_box_plot(
          dt = shap_distribution,
          XVar = "feature",
          YVar = "shap_value",
          title = "Binary SHAP Distribution by Feature",
          auto_plots_theme = auto_plots_theme,
          plot_width = plot_width,
          plot_height = plot_height
        )
      )
      if (!is.null(distribution_plot$object)) {
        distribution_plot$object <- aq_style_shap_plot(distribution_plot$object, horizontal = TRUE, x_axis_title = "")
        artifacts <- binary_shap_add_artifact(artifacts, aq_create_binary_shap_plot_artifact("shap_distribution_plot", "Binary SHAP Distribution Plot", "Global Importance", "box", "shap_distribution", distribution_plot$object, artifact_metadata("shap_distribution", "Global Importance", 7L)))
      } else {
        warnings <- regression_shap_warn(warnings, distribution_plot$warning)
      }
    }

    if (nrow(categorical_level_importance)) {
      categorical_plot_data <- data.table::copy(categorical_level_importance[rank <= plot_top_n])
      data.table::setorderv(categorical_plot_data, "mean_abs_shap", order = 1L, na.last = TRUE)
      categorical_plot_data <- aq_apply_plot_category_order(categorical_plot_data, "feature_level")
      categorical_plot_data <- aq_smart_round_dt(categorical_plot_data, skip_cols = c("rank", "n"))
      categorical_bar_plot <- aq_safe_create_shap_plot(
        "bar",
        aq_create_shap_bar_plot(
          dt = categorical_plot_data,
          XVar = "feature_level",
          YVar = "mean_abs_shap",
          title = "Categorical / Binned Numeric Level Mean Absolute SHAP",
          auto_plots_theme = auto_plots_theme,
          plot_width = plot_width,
          plot_height = plot_height
        )
      )
      if (!is.null(categorical_bar_plot$object)) {
        categorical_bar_plot$object <- aq_style_shap_plot(categorical_bar_plot$object, horizontal = TRUE, x_axis_title = "")
        artifacts <- binary_shap_add_artifact(artifacts, aq_create_binary_shap_plot_artifact("categorical_level_importance_plot", "Categorical / Binned Numeric Level SHAP Importance Plot", "Global Importance", "bar", "categorical_level_importance", categorical_bar_plot$object, artifact_metadata("categorical_level_importance", "Global Importance", 8L)))
      } else {
        warnings <- regression_shap_warn(warnings, categorical_bar_plot$warning)
      }

      categorical_box_data <- aq_regression_shap_categorical_level_values(data, column_map, categorical_level_importance, max_rows = max_dependence_rows, top_n = plot_top_n)
      if (nrow(categorical_box_data)) {
        categorical_box_data <- aq_order_for_flipped_box(categorical_box_data, "feature_level", "shap_value")
        categorical_box_plot <- aq_safe_create_shap_plot(
          "box",
          aq_create_shap_box_plot(
            dt = categorical_box_data,
            XVar = "feature_level",
            YVar = "shap_value",
            title = "Categorical / Binned Numeric Level SHAP Distribution",
            auto_plots_theme = auto_plots_theme,
            plot_width = plot_width,
            plot_height = plot_height
          )
        )
        if (!is.null(categorical_box_plot$object)) {
          categorical_box_plot$object <- aq_style_shap_plot(categorical_box_plot$object, horizontal = TRUE, x_axis_title = "")
          artifacts <- binary_shap_add_artifact(artifacts, aq_create_binary_shap_plot_artifact("categorical_level_distribution_plot", "Categorical / Binned Numeric Level SHAP Distribution Plot", "Global Importance", "box", "categorical_level_importance", categorical_box_plot$object, artifact_metadata("categorical_level_importance", "Global Importance", 9L)))
        } else {
          warnings <- regression_shap_warn(warnings, categorical_box_plot$warning)
        }
      }
    }
  }

  effects <- aq_summarize_shap_single_feature_effects(data, column_map, selected_features, prediction_col, target_col, max_levels = max_segment_levels)
  if (nrow(effects) && !is.null(actual_positive)) {
    effects[, positive_rate := mean(actual_positive), by = .(feature, feature_value_or_bin)]
  }
  artifacts <- binary_shap_add_artifact(artifacts, binary_shap_artifact("single_feature_effects_table", "Binary Single Feature SHAP Effects", "table", "Single Feature Effects", object = aq_smart_round_dt(effects, skip_cols = "n"), metadata = artifact_metadata("single_feature_effects", "Single Feature Effects", 10L)))
  if (isTRUE(include_plots) && nrow(effects)) {
    effect_features <- head(intersect(global_importance$feature, unique(effects$feature)), max_feature_effect_plots)
    for (feature_name in effect_features) {
      effect_plot_data <- data.table::copy(effects[feature == feature_name])
      source_is_numeric <- feature_name %in% names(data) && (is.numeric(data[[feature_name]]) || is.integer(data[[feature_name]]))
      if (source_is_numeric) {
        sort_col <- if ("bin_order" %in% names(effect_plot_data)) "bin_order" else "feature_value_or_bin"
        data.table::setorderv(effect_plot_data, sort_col, order = 1L, na.last = TRUE)
        effect_plot_data <- aq_apply_plot_category_order(effect_plot_data, "feature_value_or_bin")
      } else {
        data.table::setorderv(effect_plot_data, "mean_shap", order = 1L, na.last = TRUE)
        effect_plot_data <- aq_apply_plot_category_order(effect_plot_data, "feature_value_or_bin")
      }
      effect_plot_data <- aq_smart_round_dt(effect_plot_data, skip_cols = "n")
      plot_result <- aq_safe_create_shap_plot(
        if (source_is_numeric) "line" else "bar",
        if (source_is_numeric) {
          aq_create_shap_line_plot(effect_plot_data, "feature_value_or_bin", "mean_shap", title = paste("Binary SHAP Effect:", feature_name), auto_plots_theme = auto_plots_theme, plot_width = plot_width, plot_height = plot_height, x_axis_label_rotate = 45L)
        } else {
          aq_create_shap_bar_plot(effect_plot_data, "feature_value_or_bin", "mean_shap", title = paste("Binary SHAP Effect:", feature_name), auto_plots_theme = auto_plots_theme, plot_width = plot_width, plot_height = plot_height)
        }
      )
      if (!is.null(plot_result$object)) {
        plot_result$object <- aq_style_shap_plot(plot_result$object, horizontal = !source_is_numeric, rotate_x = source_is_numeric, x_axis_title = feature_name)
        artifacts <- binary_shap_add_artifact(artifacts, aq_create_binary_shap_plot_artifact(paste0("single_feature_effect_", regression_shap_slug(feature_name), "_plot"), paste("Binary SHAP Effect:", feature_name), "Single Feature Effects", if (source_is_numeric) "line" else "bar", "single_feature_effects", plot_result$object, c(artifact_metadata("single_feature_effects", "Single Feature Effects", 11L), list(feature = feature_name))))
      } else {
        warnings <- regression_shap_warn(warnings, plot_result$warning)
      }
    }
  }

  if (isTRUE(include_dependence)) {
    dependence <- aq_summarize_shap_dependence(data, column_map, selected_features, prediction_col, target_col, id_cols, ByVars, DateVar, max_dependence_rows)
    dependence <- aq_binary_shap_add_binary_context(dependence, data, "row_id", target_col, prediction_col, positive_class, threshold, id_cols)
    artifacts <- binary_shap_add_artifact(artifacts, binary_shap_artifact("shap_dependence_table", "Binary SHAP Dependence", "table", "SHAP Dependence", object = aq_smart_round_dt(dependence, skip_cols = "row_id"), metadata = artifact_metadata("shap_dependence", "SHAP Dependence", 12L)))
    if (isTRUE(include_plots) && nrow(dependence)) {
      dependence_features <- head(intersect(global_importance$feature, unique(dependence$feature)), max_dependence_plots)
      for (feature_name in dependence_features) {
        shap_col <- column_map[feature == feature_name, shap_col][[1L]]
        row_index <- seq_len(nrow(data))
        if (nrow(data) > max_dependence_rows) {
          set.seed(123L)
          row_index <- sort(sample(row_index, max_dependence_rows))
        }
        source_is_numeric <- feature_name %in% names(data) && (is.numeric(data[[feature_name]]) || is.integer(data[[feature_name]]))
        plot_data <- data.table::data.table(feature_value = data[[feature_name]][row_index], shap_value = data[[shap_col]][row_index])
        if (!source_is_numeric) {
          plot_data[, feature_value := as.character(feature_value)]
          plot_data <- aq_order_for_flipped_box(plot_data, "feature_value", "shap_value")
        }
        plot_result <- aq_safe_create_shap_plot(
          if (source_is_numeric) "scatter" else "box",
          if (source_is_numeric) {
            aq_create_shap_scatter_plot(plot_data, "feature_value", "shap_value", GroupVar = NULL, title = paste("Binary SHAP Dependence:", feature_name), auto_plots_theme = auto_plots_theme, plot_width = plot_width, plot_height = plot_height)
          } else {
            aq_create_shap_box_plot(plot_data, "feature_value", "shap_value", GroupVar = NULL, title = paste("Binary SHAP Dependence:", feature_name), auto_plots_theme = auto_plots_theme, plot_width = plot_width, plot_height = plot_height)
          }
        )
        if (!is.null(plot_result$object)) {
          plot_result$object <- aq_style_shap_plot(plot_result$object, horizontal = !source_is_numeric, x_axis_title = feature_name)
          artifacts <- binary_shap_add_artifact(artifacts, aq_create_binary_shap_plot_artifact(paste0("shap_dependence_", regression_shap_slug(feature_name), "_plot"), paste("Binary SHAP Dependence:", feature_name), "SHAP Dependence", if (source_is_numeric) "scatter" else "box", "shap_dependence", plot_result$object, c(artifact_metadata("shap_dependence", "SHAP Dependence", 13L), list(feature = feature_name, x_axis_source_column = feature_name, x_axis = "feature_value", group_var = NULL, ByVars_context_available = ByVars))))
        } else {
          warnings <- regression_shap_warn(warnings, plot_result$warning)
        }
      }
    }
  }

  if (isTRUE(include_segments) && length(ByVars)) {
    segments <- aq_summarize_shap_segments(data, column_map, ByVars, max_byvars, max_segment_levels)
    if (nrow(segments) && !is.null(actual_positive)) {
      segment_context <- data.table::data.table(actual_positive = actual_positive)
      for (byvar in ByVars) segment_context[, (byvar) := as.character(data[[byvar]])]
      if (!is.null(prediction_col) && prediction_col %in% names(data) && is.numeric(data[[prediction_col]])) segment_context[, prediction := data[[prediction_col]]]
      context_rows <- lapply(ByVars, function(byvar) {
        segment_context[, .(
          ByVar = byvar,
          segment = get(byvar),
          positive_rate = mean(actual_positive, na.rm = TRUE),
          mean_prediction = if ("prediction" %in% names(.SD)) mean(prediction, na.rm = TRUE) else NA_real_
        ), by = byvar][, (byvar) := NULL]
      })
      segment_rates <- data.table::rbindlist(context_rows, use.names = TRUE, fill = TRUE)
      segments <- merge(segments, segment_rates, by = c("ByVar", "segment"), all.x = TRUE)
    }
    artifacts <- binary_shap_add_artifact(artifacts, binary_shap_artifact("segment_effects_table", "Binary Segment SHAP Effects", "table", "Segment Effects", object = aq_smart_round_dt(segments, skip_cols = c("rank_within_segment", "n")), metadata = artifact_metadata("segment_effects", "Segment Effects", 14L)))
    if (isTRUE(include_plots) && nrow(segments)) {
      segment_byvars <- head(unique(segments$ByVar), max_segment_plots)
      for (byvar in segment_byvars) {
        segment_plot_data <- data.table::copy(segments[ByVar == byvar & rank_within_segment <= plot_top_n])
        segment_plot_data <- aq_smart_round_dt(segment_plot_data, skip_cols = c("rank_within_segment", "n"))
        segment_n_y_levels <- data.table::uniqueN(segment_plot_data$feature, na.rm = TRUE)
        segment_plot_height <- aq_shap_heatmap_height(segment_n_y_levels)
        heatmap_result <- aq_safe_create_shap_plot(
          "heatmap",
          aq_create_shap_heatmap_plot(segment_plot_data, "segment", "feature", "mean_shap", title = paste("Binary Segment Mean SHAP Heatmap:", byvar), auto_plots_theme = auto_plots_theme, plot_width = plot_width, plot_height = segment_plot_height)
        )
        if (!is.null(heatmap_result$object)) {
          heatmap_result$object <- aq_style_shap_plot(heatmap_result$object, rotate_x = TRUE, x_axis_title = byvar, y_axis_title = "")
          artifacts <- binary_shap_add_artifact(artifacts, aq_create_binary_shap_plot_artifact(paste0("segment_effects_", regression_shap_slug(byvar), "_heatmap"), paste("Binary Segment Mean SHAP Heatmap:", byvar), "Segment Effects", "heatmap", "segment_effects", heatmap_result$object, c(artifact_metadata("segment_effects", "Segment Effects", 15L), list(ByVar = byvar, heatmap_value = "mean_shap", heatmap_value_description = "Signed mean SHAP by feature and segment", n_y_levels = segment_n_y_levels, plot_height = segment_plot_height))))
        } else {
          warnings <- regression_shap_warn(warnings, heatmap_result$warning)
        }
      }
    }
  } else if (isTRUE(include_segments)) {
    warnings <- regression_shap_warn(warnings, "Segment effects skipped because no valid ByVars were supplied.")
  }

  if (isTRUE(include_time) && !is.null(DateVar)) {
    time_effects <- tryCatch(aq_summarize_shap_time(data, column_map, DateVar, date_aggregation), error = function(e) {
      warnings <<- regression_shap_warn(warnings, paste("Time effects skipped:", conditionMessage(e)))
      data.table::data.table()
    })
    if (nrow(time_effects) && !is.null(actual_positive)) {
      period_context <- data.table::data.table(period = aq_shap_period(data[[DateVar]], date_aggregation), actual_positive = actual_positive)
      if (!is.null(prediction_col) && prediction_col %in% names(data) && is.numeric(data[[prediction_col]])) period_context[, prediction := data[[prediction_col]]]
      period_rates <- period_context[, .(
        positive_rate = mean(actual_positive, na.rm = TRUE),
        mean_prediction = if ("prediction" %in% names(.SD)) mean(prediction, na.rm = TRUE) else NA_real_
      ), by = period]
      time_effects <- merge(time_effects, period_rates, by = "period", all.x = TRUE)
    }
    artifacts <- binary_shap_add_artifact(artifacts, binary_shap_artifact("time_effects_table", "Binary Time SHAP Effects", "table", "Time Effects", object = aq_smart_round_dt(time_effects, skip_cols = c("rank_within_period", "n")), metadata = artifact_metadata("time_effects", "Time Effects", 16L)))
    if (isTRUE(include_plots) && nrow(time_effects)) {
      time_plot_data <- data.table::copy(time_effects[rank_within_period <= plot_top_n])
      time_plot_data[, period := as.Date(period)]
      time_plot_data <- aq_smart_round_dt(time_plot_data, skip_cols = c("rank_within_period", "n"))
      line_result <- aq_safe_create_shap_plot("line", aq_create_shap_line_plot(time_plot_data, "period", "mean_abs_shap", GroupVar = "feature", title = "Binary Time SHAP Effects", auto_plots_theme = auto_plots_theme, plot_width = plot_width, plot_height = plot_height, x_axis_label_rotate = 45L))
      if (!is.null(line_result$object)) {
        line_result$object <- aq_style_shap_plot(line_result$object, rotate_x = TRUE, x_axis_title = DateVar)
        artifacts <- binary_shap_add_artifact(artifacts, aq_create_binary_shap_plot_artifact("time_effects_line_plot", "Binary Time SHAP Effects Plot", "Time Effects", "line", "time_effects", line_result$object, artifact_metadata("time_effects", "Time Effects", 17L)))
      } else {
        warnings <- regression_shap_warn(warnings, line_result$warning)
      }
      time_n_y_levels <- data.table::uniqueN(time_plot_data$feature, na.rm = TRUE)
      time_plot_height <- aq_shap_heatmap_height(time_n_y_levels)
      heatmap_result <- aq_safe_create_shap_plot("heatmap", aq_create_shap_heatmap_plot(time_plot_data, "period", "feature", "mean_abs_shap", title = "Binary Time SHAP Effects Heatmap", auto_plots_theme = auto_plots_theme, plot_width = plot_width, plot_height = time_plot_height))
      if (!is.null(heatmap_result$object)) {
        heatmap_result$object <- aq_style_shap_plot(heatmap_result$object, rotate_x = TRUE, x_axis_title = DateVar, y_axis_title = "")
        artifacts <- binary_shap_add_artifact(artifacts, aq_create_binary_shap_plot_artifact("time_effects_heatmap", "Binary Time SHAP Effects Heatmap", "Time Effects", "heatmap", "time_effects", heatmap_result$object, c(artifact_metadata("time_effects", "Time Effects", 18L), list(n_y_levels = time_n_y_levels, plot_height = time_plot_height))))
      } else {
        warnings <- regression_shap_warn(warnings, heatmap_result$warning)
      }
    }
  } else if (isTRUE(include_time)) {
    warnings <- regression_shap_warn(warnings, "Time effects skipped because no valid DateVar was supplied.")
  }

  if (isTRUE(include_threshold_context)) {
    threshold_context <- aq_binary_shap_threshold_context(data, column_map, prediction_col, target_col, positive_class, threshold, threshold_bands, prediction_scale)
    if (nrow(threshold_context)) {
      artifacts <- binary_shap_add_artifact(artifacts, binary_shap_artifact("threshold_context_table", "Binary Threshold SHAP Context", "table", "Threshold Context", object = aq_smart_round_dt(threshold_context, skip_cols = c("rank_within_band", "n")), metadata = artifact_metadata("threshold_context", "Threshold Context", 19L)))
      if (isTRUE(include_plots)) {
        threshold_plot_data <- data.table::copy(threshold_context[rank_within_band <= plot_top_n])
        data.table::setorderv(threshold_plot_data, c("prediction_band", "mean_abs_shap"), order = c(1L, 1L), na.last = TRUE)
        threshold_plot <- aq_safe_create_shap_plot("bar", aq_create_shap_bar_plot(threshold_plot_data, "feature", "mean_abs_shap", GroupVar = "prediction_band", title = "Binary SHAP Importance by Threshold Band", auto_plots_theme = auto_plots_theme, plot_width = plot_width, plot_height = plot_height))
        if (!is.null(threshold_plot$object)) {
          threshold_plot$object <- aq_style_shap_plot(threshold_plot$object, horizontal = TRUE, x_axis_title = "")
          artifacts <- binary_shap_add_artifact(artifacts, aq_create_binary_shap_plot_artifact("threshold_context_plot", "Binary Threshold SHAP Context Plot", "Threshold Context", "bar", "threshold_context", threshold_plot$object, artifact_metadata("threshold_context", "Threshold Context", 20L)))
        } else {
          warnings <- regression_shap_warn(warnings, threshold_plot$warning)
        }
      }
    } else {
      warnings <- regression_shap_warn(warnings, "Threshold context skipped because prediction scores were unavailable or incompatible with the prediction scale.")
    }
  }

  if (isTRUE(include_class_balance)) {
    class_balance <- aq_binary_shap_class_balance(data, target_col, prediction_col, positive_class)
    class_feature_importance <- aq_binary_shap_class_feature_importance(data, column_map, target_col, prediction_col, positive_class)
    if (nrow(class_balance)) {
      artifacts <- binary_shap_add_artifact(artifacts, binary_shap_artifact("class_balance_table", "Class Balance / Outcome Context", "table", "Class Balance / Outcome Context", object = aq_smart_round_dt(class_balance, skip_cols = "n"), metadata = artifact_metadata("class_balance", "Class Balance / Outcome Context", 21L)))
    }
    if (nrow(class_feature_importance)) {
      artifacts <- binary_shap_add_artifact(artifacts, binary_shap_artifact("class_feature_importance_table", "Class-Specific SHAP Importance", "table", "Class Balance / Outcome Context", object = aq_smart_round_dt(class_feature_importance, skip_cols = c("rank_within_class", "n")), metadata = artifact_metadata("class_balance", "Class Balance / Outcome Context", 22L)))
      if (isTRUE(include_plots)) {
        class_plot_data <- data.table::copy(class_feature_importance[rank_within_class <= plot_top_n])
        data.table::setorderv(class_plot_data, c("target", "mean_abs_shap"), order = c(1L, 1L), na.last = TRUE)
        class_plot <- aq_safe_create_shap_plot("bar", aq_create_shap_bar_plot(class_plot_data, "feature", "mean_abs_shap", GroupVar = "target", title = "Class-Specific Mean Absolute SHAP", auto_plots_theme = auto_plots_theme, plot_width = plot_width, plot_height = plot_height))
        if (!is.null(class_plot$object)) {
          class_plot$object <- aq_style_shap_plot(class_plot$object, horizontal = TRUE, x_axis_title = "")
          artifacts <- binary_shap_add_artifact(artifacts, aq_create_binary_shap_plot_artifact("class_feature_importance_plot", "Class-Specific SHAP Importance Plot", "Class Balance / Outcome Context", "bar", "class_balance", class_plot$object, artifact_metadata("class_balance", "Class Balance / Outcome Context", 23L)))
        } else {
          warnings <- regression_shap_warn(warnings, class_plot$warning)
        }
      }
    }
  }

  if (isTRUE(include_local)) {
    local_row_ids <- suppressWarnings(as.integer(local_row_ids))
    invalid_rows <- local_row_ids[is.na(local_row_ids) | local_row_ids < 1L | local_row_ids > nrow(data)]
    if (length(invalid_rows)) {
      warnings <- regression_shap_warn(warnings, "Invalid local_row_ids were dropped.")
    }
    local <- aq_summarize_shap_local(data, column_map, local_row_ids[!is.na(local_row_ids)], prediction_col, target_col, id_cols)
    if (nrow(local)) {
      local <- aq_binary_shap_add_binary_context(local, data, "row_id", target_col, prediction_col, positive_class, threshold, id_cols)
      local[, direction := aq_binary_shap_local_direction(shap_value)]
    }
    artifacts <- binary_shap_add_artifact(artifacts, binary_shap_artifact("local_explanations_table", "Binary Local SHAP Explanations", "table", "Local Explanations", object = aq_smart_round_dt(local, skip_cols = c("row_id", "contribution_rank")), metadata = artifact_metadata("local_explanations", "Local Explanations", 24L)))
    if (isTRUE(include_plots) && nrow(local)) {
      local_rows <- head(unique(local$row_id), max_local_plots)
      for (row_id in local_rows) {
        local_plot_data <- data.table::copy(local[local[["row_id"]] == row_id][contribution_rank <= plot_top_n])
        local_plot_data[, abs_shap_for_plot_order := abs(shap_value)]
        data.table::setorderv(local_plot_data, "abs_shap_for_plot_order", order = 1L, na.last = TRUE)
        local_plot_data[, abs_shap_for_plot_order := NULL]
        local_plot_data <- aq_apply_plot_category_order(local_plot_data, "feature")
        local_plot_data <- aq_smart_round_dt(local_plot_data, skip_cols = c("row_id", "contribution_rank"))
        plot_result <- aq_safe_create_shap_plot("bar", aq_create_shap_bar_plot(local_plot_data, "feature", "shap_value", title = paste("Local Binary SHAP Contributions: Row", row_id), auto_plots_theme = auto_plots_theme, plot_width = plot_width, plot_height = plot_height))
        if (!is.null(plot_result$object)) {
          plot_result$object <- aq_style_shap_plot(plot_result$object, horizontal = TRUE, x_axis_title = "")
          artifacts <- binary_shap_add_artifact(artifacts, aq_create_binary_shap_plot_artifact(paste0("local_explanations_row_", row_id, "_plot"), paste("Local Binary SHAP Contributions: Row", row_id), "Local Explanations", "bar", "local_explanations", plot_result$object, c(artifact_metadata("local_explanations", "Local Explanations", 25L), list(row_id = row_id))))
        } else {
          warnings <- regression_shap_warn(warnings, plot_result$warning)
        }
      }
    }
  }

  if (isTRUE(include_interactions)) {
    interactions <- aq_summarize_shap_interactions(
      data = data,
      column_map = column_map,
      global_importance = global_importance,
      selected_features = selected_features,
      interaction_pairs = list(),
      max_interaction_pairs = max_interaction_pairs,
      numeric_interaction_bins = numeric_interaction_bins,
      max_interaction_levels = max_interaction_levels,
      min_interaction_cell_n = min_interaction_cell_n,
      interaction_stat = interaction_stat,
      interaction_score_stat = interaction_score_stat,
      collapse_rare_levels = collapse_rare_levels,
      target_col = target_col,
      prediction_col = prediction_col,
      ByVars = ByVars,
      DateVar = DateVar,
      date_aggregation = date_aggregation,
      id_cols = id_cols
    )
    warnings <- unique(c(warnings, interactions$warnings))
    interaction_text <- paste(
      "Binary SHAP interaction diagnostics",
      "These diagnostics use precomputed ordinary Shap_ columns and existing source feature columns.",
      "Numeric variables are binned and categorical/date variables use bounded levels.",
      "Two-way surfaces place the SHAP-attributed feature's actual value/bin on one axis and the interaction lens feature's actual value/bin on the other axis.",
      "Pairwise diagnostics are canonical unordered pairs; A x B and B x A are treated as the same analytical object unless a future diagnostic explicitly marks itself directional.",
      "Heatmap values are signed mean SHAP for the attributed feature across those actual value/bin combinations.",
      "Scores are candidate interaction diagnostics based on binned level-combination heterogeneity, not exact SHAP interaction values.",
      "Exact pairwise SHAP interaction values require upstream interaction-specific output.",
      sep = "\n"
    )
    interaction_meta <- c(artifact_metadata("interaction_diagnostics", "Interaction Importance", 26L), list(
      interaction_type = "binned_level_combination",
      shap_source = "precomputed_columns",
      exact_shap_interaction_values = FALSE,
      numeric_interaction_bins = numeric_interaction_bins,
      max_interaction_levels = max_interaction_levels,
      min_interaction_cell_n = min_interaction_cell_n,
      interaction_stat = interaction_stat,
      score_stat = interaction_score_stat,
      directional = FALSE,
      target_col = target_col,
      prediction_col = prediction_col,
      positive_class = positive_class,
      prediction_scale = prediction_scale,
      DateVar = DateVar,
      date_aggregation = date_aggregation,
      ByVars = ByVars
    ))
    artifacts <- binary_shap_add_artifact(artifacts, binary_shap_artifact("interaction_methodology", "Binary SHAP Interaction Diagnostics Methodology", "text", "Interaction Importance", content = interaction_text, metadata = interaction_meta))
    if (nrow(interactions$scores)) {
      artifacts <- binary_shap_add_artifact(artifacts, binary_shap_artifact("candidate_interaction_ranking_table", "Binary Candidate Interaction Ranking", "table", "Interaction Importance", object = interactions$scores, metadata = interaction_meta))
      if (nrow(interactions$surfaces)) {
        artifacts <- binary_shap_add_artifact(artifacts, binary_shap_artifact("two_way_shap_surface_table", "Binary Two-Way SHAP Surface Table", "table", "Interaction Importance", object = interactions$surfaces, metadata = interaction_meta))
      }
      if (isTRUE(include_plots)) {
        interaction_plot_data <- data.table::copy(interactions$scores[rank <= plot_top_n])
        data.table::setorderv(interaction_plot_data, "score", order = 1L, na.last = TRUE)
        ranking_plot <- aq_safe_create_shap_plot("bar", aq_create_shap_bar_plot(interaction_plot_data, "pair_label", "score", title = "Binary Candidate SHAP Interaction Diagnostic Score", auto_plots_theme = auto_plots_theme, plot_width = plot_width, plot_height = plot_height))
        if (!is.null(ranking_plot$object)) {
          ranking_plot$object <- aq_style_shap_plot(ranking_plot$object, horizontal = TRUE, x_axis_title = "")
          artifacts <- binary_shap_add_artifact(artifacts, aq_create_binary_shap_plot_artifact("candidate_interaction_ranking_plot", "Binary Candidate Interaction Ranking Plot", "Interaction Importance", "bar", "interaction_diagnostics", ranking_plot$object, interaction_meta))
        } else {
          warnings <- regression_shap_warn(warnings, ranking_plot$warning)
        }
        top_pair_scores <- data.table::copy(interactions$scores)
        top_pair_scores[, plot_priority := data.table::fcase(interaction_feature_role == "ByVar", 1L, interaction_feature_role == "DateVar_period", 2L, interaction_feature_role == "selected_feature", 3L, default = 4L)]
        data.table::setorderv(top_pair_scores, c("plot_priority", "rank"), order = c(1L, 1L), na.last = TRUE)
        top_pairs <- head(top_pair_scores$pair_label, max_interaction_surface_plots)
        for (current_pair_label in top_pairs) {
          surface_plot_data <- data.table::copy(interactions$surfaces[pair_label == current_pair_label & sparse_cell == FALSE])
          if (!nrow(surface_plot_data)) next
          surface_pair_key <- surface_plot_data$interaction_pair_key[[1L]]
          surface_feature_x <- surface_plot_data$feature_x[[1L]]
          surface_feature_y <- surface_plot_data$feature_y[[1L]]
          shap_axis <- surface_plot_data$shap_feature[[1L]]
          interaction_axis <- surface_plot_data$interaction_feature[[1L]]
          heatmap_axis <- "mean_shap"
          if (identical(shap_axis, interaction_axis)) interaction_axis <- paste0(interaction_axis, "_interaction")
          data.table::setorderv(surface_plot_data, c("shap_feature_level_order", "interaction_feature_level_order"), order = c(1L, 1L), na.last = TRUE)
          surface_plot_data[, (shap_axis) := factor(as.character(shap_feature_actual_value_bin), levels = unique(as.character(shap_feature_actual_value_bin)), ordered = TRUE)]
          surface_plot_data[, (interaction_axis) := factor(as.character(interaction_feature_actual_value_bin), levels = unique(as.character(interaction_feature_actual_value_bin)), ordered = TRUE)]
          surface_plot_data[, (heatmap_axis) := heatmap_value]
          surface_title <- paste("Binary Mean SHAP Surface:", surface_plot_data$shap_feature[[1L]], "by", surface_plot_data$interaction_feature[[1L]])
          surface_n_y_levels <- data.table::uniqueN(surface_plot_data[[interaction_axis]], na.rm = TRUE)
          surface_plot_height <- aq_shap_heatmap_height(surface_n_y_levels)
          heatmap_result <- aq_safe_create_shap_plot("heatmap", aq_create_shap_heatmap_plot(surface_plot_data, shap_axis, interaction_axis, heatmap_axis, title = surface_title, auto_plots_theme = auto_plots_theme, plot_width = plot_width, plot_height = surface_plot_height))
          if (!is.null(heatmap_result$object)) {
            heatmap_result$object <- aq_style_shap_plot(heatmap_result$object, rotate_x = TRUE)
            artifacts <- binary_shap_add_artifact(artifacts, aq_create_binary_shap_plot_artifact(paste0("two_way_shap_surface_", regression_shap_slug(current_pair_label), "_heatmap"), surface_title, "Interaction Importance", "heatmap", "interaction_diagnostics", heatmap_result$object, c(interaction_meta, list(
              pair_label = current_pair_label,
              feature_x = surface_feature_x,
              feature_y = surface_feature_y,
              interaction_pair_key = surface_pair_key,
              directional = FALSE,
              x_axis = shap_axis,
              y_axis = interaction_axis,
              heatmap_value = "mean_shap",
              heatmap_value_description = surface_plot_data$heatmap_value_description[[1L]],
              n_y_levels = surface_n_y_levels,
              plot_height = surface_plot_height
            ))))
          } else {
            warnings <- regression_shap_warn(warnings, heatmap_result$warning)
          }
        }
      }
    } else {
      warnings <- regression_shap_warn(warnings, "Interaction diagnostics skipped because no valid candidate pairs were available.")
    }
  }

  types <- vapply(artifacts, function(x) regression_shap_null_coalesce(regression_shap_null_coalesce(x$type, x$artifact_type), ""), character(1L))
  metadata <- c(full_metadata, list(
    artifact_count = length(artifacts),
    plot_count = sum(types == "plot"),
    table_count = sum(types == "table"),
    text_count = sum(types == "text"),
    warnings_count = length(warnings),
    interaction_diagnostics_available = "candidate_interaction_ranking_table" %in% names(artifacts),
    interaction_pair_count = if ("candidate_interaction_ranking_table" %in% names(artifacts)) nrow(artifacts$candidate_interaction_ranking_table$object) else 0L,
    interaction_surface_count = if ("two_way_shap_surface_table" %in% names(artifacts)) length(unique(artifacts$two_way_shap_surface_table$object$pair_label)) else 0L,
    interaction_method = "binned_level_combination_shap_diagnostic",
    exact_shap_interaction_values = FALSE
  ))

  list(
    artifacts = artifacts,
    metadata = metadata,
    warnings = warnings,
    diagnostics = list(
      column_map = column_map,
      valid_features = column_map[included == TRUE, feature],
      skipped_features = column_map[included == FALSE, feature]
    ),
    code = "generate_binary_classification_shap_analysis_artifacts(data = scoring_data_with_precomputed_shap_columns)"
  )
}

qa_generate_binary_classification_shap_analysis_artifacts <- function() {
  set.seed(123)
  n <- 300
  x1 <- stats::runif(n, 0, 100)
  x2 <- stats::runif(n, 0, 50)
  seg <- sample(c("A", "B", "C"), n, TRUE)
  date <- as.Date("2024-01-01") + sample(0:180, n, TRUE)
  eta <- -1 + 0.025 * x1 + 0.03 * x2 + ifelse(seg == "A", 0.7, ifelse(seg == "B", -0.2, 0))
  p <- 1 / (1 + exp(-eta))
  y <- ifelse(stats::runif(n) < p, "Yes", "No")

  dt <- data.table::data.table(
    Target = y,
    Predict = p,
    PredictedClass = ifelse(p >= 0.5, "Yes", "No"),
    Independent_Variable1 = x1,
    Independent_Variable2 = x2,
    Factor_1 = seg,
    IDCol_1 = seq_len(n),
    IDCol_2 = sample(1000:9999, n, TRUE),
    Date = date,
    Shap_Independent_Variable1 = 0.01 * x1 + ifelse(x2 > 25, 0.15, -0.05) + ifelse(seg == "A", 0.08, 0) + stats::rnorm(n, 0, 0.02),
    Shap_Independent_Variable2 = 0.01 * x2 + ifelse(x1 > 50, 0.1, -0.04) + stats::rnorm(n, 0, 0.02),
    Shap_Factor_1 = ifelse(seg == "A", 0.15, ifelse(seg == "B", -0.05, 0.02)) + stats::rnorm(n, 0, 0.02),
    Shap_Bad = as.character(seq_len(n))
  )

  result <- generate_binary_classification_shap_analysis_artifacts(
    data = dt,
    target_col = "Target",
    prediction_col = "Predict",
    predicted_class_col = "PredictedClass",
    positive_class = "Yes",
    prediction_scale = "probability",
    threshold = 0.5,
    DateVar = "Date",
    date_aggregation = "month",
    ByVars = "Factor_1",
    id_cols = c("IDCol_1", "IDCol_2"),
    selected_features = c("Independent_Variable1", "Independent_Variable2", "Factor_1"),
    local_row_ids = c(1L, 2L),
    top_n = 3,
    include_dependence = TRUE,
    include_segments = TRUE,
    include_time = TRUE,
    include_local = TRUE,
    include_interactions = TRUE,
    include_threshold_context = TRUE,
    include_class_balance = TRUE,
    include_plots = TRUE,
    numeric_interaction_bins = 5,
    max_interaction_pairs = 10,
    max_interaction_surface_plots = 5,
    min_interaction_cell_n = 5
  )
  no_shap <- generate_binary_classification_shap_analysis_artifacts(data = dt[, !startsWith(names(dt), "Shap_")], positive_class = "Yes")
  missing_positive <- generate_binary_classification_shap_analysis_artifacts(data = dt, target_col = "Target")
  bad_date <- generate_binary_classification_shap_analysis_artifacts(data = dt, target_col = "Target", positive_class = "Yes", DateVar = "MissingDate")
  bad_by <- generate_binary_classification_shap_analysis_artifacts(data = dt, target_col = "Target", positive_class = "Yes", ByVars = "MissingBy")
  bad_probability <- data.table::copy(dt)
  bad_probability[, Predict := Predict * 10]
  bad_probability_result <- generate_binary_classification_shap_analysis_artifacts(data = bad_probability, target_col = "Target", prediction_col = "Predict", positive_class = "Yes", prediction_scale = "probability")
  regression_qa <- qa_generate_regression_shap_analysis_artifacts()

  artifact_names <- names(result$artifacts)
  artifact_sections <- vapply(result$artifacts, function(x) regression_shap_null_coalesce(x$section, ""), character(1L))
  artifact_labels <- vapply(result$artifacts, function(x) regression_shap_null_coalesce(x$label, ""), character(1L))
  artifact_types <- vapply(result$artifacts, function(x) regression_shap_null_coalesce(x$type, ""), character(1L))
  plot_metadata_packages <- vapply(result$artifacts[artifact_types == "plot"], function(x) regression_shap_null_coalesce(x$metadata$plot_package, ""), character(1L))
  interaction_artifacts <- result$artifacts[grepl("interaction|surface", names(result$artifacts))]
  exact_flags <- vapply(interaction_artifacts, function(x) identical(x$metadata$exact_shap_interaction_values, FALSE), logical(1L))
  surface_table <- result$artifacts$two_way_shap_surface_table$object
  local_table <- result$artifacts$local_explanations_table$object

  data.table::data.table(
    check = c(
      "structured_result",
      "artifacts_returned",
      "overview_text",
      "diagnostics_table",
      "column_map_table",
      "global_importance_table",
      "global_importance_plot",
      "shap_distribution_plot",
      "single_feature_effects_table",
      "dependence_table",
      "segment_table",
      "time_table",
      "threshold_context_table",
      "threshold_context_plot",
      "class_balance_artifact",
      "local_table",
      "local_plot",
      "interaction_methodology",
      "interaction_ranking_table",
      "interaction_surface_table",
      "interaction_surface_plot",
      "labels_non_empty",
      "sections_non_empty",
      "metadata_problem_type",
      "metadata_source_function",
      "metadata_positive_class",
      "metadata_prediction_scale",
      "metadata_shap_source",
      "metadata_not_exact_interactions",
      "id_cols_not_features",
      "target_prediction_not_interaction_candidates",
      "no_shap_columns_structured",
      "missing_positive_class_structured",
      "invalid_date_warns",
      "invalid_byvars_warn",
      "probability_validation_warns",
      "plot_metadata_package",
      "local_direction",
      "regression_shap_qa_still_passes"
    ),
    status = c(
      if (is.list(result) && all(c("artifacts", "metadata", "warnings", "diagnostics", "code") %in% names(result))) "success" else "error",
      if (length(result$artifacts) >= 12L) "success" else "error",
      if ("shap_overview" %in% artifact_names) "success" else "error",
      if ("shap_diagnostics_config" %in% artifact_names) "success" else "error",
      if ("shap_column_map" %in% artifact_names) "success" else "error",
      if ("global_importance_table" %in% artifact_names) "success" else "error",
      if (!aq_autoplots_available() || "global_importance_plot" %in% artifact_names) "success" else "error",
      if (!aq_autoplots_available() || "shap_distribution_plot" %in% artifact_names) "success" else "error",
      if ("single_feature_effects_table" %in% artifact_names) "success" else "error",
      if ("shap_dependence_table" %in% artifact_names) "success" else "error",
      if ("segment_effects_table" %in% artifact_names) "success" else "error",
      if ("time_effects_table" %in% artifact_names) "success" else "error",
      if ("threshold_context_table" %in% artifact_names) "success" else "error",
      if (!aq_autoplots_available() || "threshold_context_plot" %in% artifact_names) "success" else "error",
      if ("class_balance_table" %in% artifact_names && "class_feature_importance_table" %in% artifact_names) "success" else "error",
      if ("local_explanations_table" %in% artifact_names) "success" else "error",
      if (!aq_autoplots_available() || any(grepl("^local_explanations_row_.*_plot$", artifact_names))) "success" else "error",
      if ("interaction_methodology" %in% artifact_names) "success" else "error",
      if ("candidate_interaction_ranking_table" %in% artifact_names) "success" else "error",
      if ("two_way_shap_surface_table" %in% artifact_names) "success" else "error",
      if (!aq_autoplots_available() || any(grepl("^two_way_shap_surface_.*_heatmap$", artifact_names))) "success" else "error",
      if (all(nzchar(artifact_labels))) "success" else "error",
      if (all(nzchar(artifact_sections))) "success" else "error",
      if (identical(result$metadata$problem_type, "binary_classification")) "success" else "error",
      if (identical(result$metadata$source_function, "generate_binary_classification_shap_analysis_artifacts")) "success" else "error",
      if (identical(result$metadata$positive_class, "Yes")) "success" else "error",
      if (identical(result$metadata$prediction_scale, "probability")) "success" else "error",
      if (identical(result$metadata$shap_source, "precomputed_columns")) "success" else "error",
      if (identical(result$metadata$exact_shap_interaction_values, FALSE) && all(exact_flags)) "success" else "error",
      if (!any(result$diagnostics$column_map$feature %in% c("IDCol_1", "IDCol_2"))) "success" else "error",
      if (!any(result$artifacts$candidate_interaction_ranking_table$object$interaction_feature %in% c("Target", "Predict"))) "success" else "error",
      if (is.list(no_shap) && length(no_shap$warnings)) "success" else "error",
      if (is.list(missing_positive) && length(missing_positive$warnings)) "success" else "error",
      if (length(bad_date$warnings)) "success" else "error",
      if (length(bad_by$warnings)) "success" else "error",
      if (identical(bad_probability_result$metadata$prediction_scale, "unknown") && length(bad_probability_result$warnings)) "success" else "error",
      if (!aq_autoplots_available() || all(plot_metadata_packages == "AutoPlots")) "success" else "error",
      if ("direction" %in% names(local_table) && all(local_table$direction %in% c("toward_positive_class", "away_from_positive_class", "neutral"))) "success" else "error",
      if (!any(regression_qa$status == "error")) "success" else "error"
    ),
    message = c(
      "Result has structured fields.",
      "Binary SHAP artifacts were returned.",
      "Overview text artifact exists.",
      "Diagnostics/config table exists.",
      "SHAP column map table exists.",
      "Global importance table exists.",
      "Global importance plot exists when AutoPlots is available.",
      "SHAP distribution plot exists when AutoPlots is available.",
      "Single feature effects table exists.",
      "Dependence table exists.",
      "Segment table exists.",
      "Time table exists.",
      "Threshold context table exists.",
      "Threshold context plot exists when AutoPlots is available.",
      "Class balance/outcome context artifacts exist.",
      "Local explanation table exists.",
      "Local contribution plot exists when AutoPlots is available.",
      "Interaction methodology text exists.",
      "Interaction ranking table exists.",
      "Two-way SHAP surface table exists.",
      "Two-way SHAP surface plot exists when AutoPlots is available.",
      "Artifact labels are non-empty.",
      "Artifact sections are non-empty.",
      "Problem type metadata is binary_classification.",
      "Source function metadata is correct.",
      "Positive class metadata is explicit.",
      "Prediction scale metadata is explicit.",
      "SHAP source metadata is precomputed_columns.",
      "Interaction diagnostics are explicitly not exact SHAP interaction values.",
      "ID columns were not treated as SHAP features.",
      "Target and prediction columns were not interaction candidates by default.",
      "No-SHAP input returns structured warning output.",
      "Missing positive_class with target_col returns structured warning output.",
      "Invalid DateVar warns without crashing.",
      "Invalid ByVars warn without crashing.",
      "Probability-scale validation warns and uses unknown when values are incompatible.",
      "Plot artifact metadata records AutoPlots.",
      "Local explanation directions are positive-class oriented.",
      "Regression SHAP QA still passes."
    )
  )
}
