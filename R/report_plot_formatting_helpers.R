# Shared report plot formatting helpers.
#
# AutoPlots owns visual theme defaults. These helpers only prepare report data
# for semantic display behavior before AutoPlots creates the widget.

aq_report_sort_for_flipped_bar <- function(dt, value_col, category_col = NULL) {
  if (is.null(dt) || !data.table::is.data.table(dt) || !nrow(dt) || !value_col %in% names(dt)) {
    return(dt)
  }

  out <- data.table::copy(dt)
  data.table::setorderv(out, value_col, order = 1L, na.last = TRUE)

  if (!is.null(category_col) && category_col %in% names(out)) {
    out[, (category_col) := factor(as.character(get(category_col)), levels = unique(as.character(get(category_col))), ordered = TRUE)]
  }

  out[]
}

aq_report_unique_axis_levels <- function(dt, axis_col) {
  if (is.null(dt) || !data.table::is.data.table(dt) || is.null(axis_col) || !axis_col %in% names(dt)) {
    return(0L)
  }

  as.integer(data.table::uniqueN(dt[[axis_col]], na.rm = TRUE))
}

aq_report_dense_axis_tooltip <- function(
  dt,
  XVar,
  YVar = NULL,
  max_x_levels = 20L,
  max_y_levels = 14L
) {
  n_x <- aq_report_unique_axis_levels(dt, XVar)
  n_y <- aq_report_unique_axis_levels(dt, YVar)
  if (n_x > max_x_levels || n_y > max_y_levels) FALSE else NULL
}

qa_report_flipped_bar_ordering_guideline <- function() {
  dt <- data.table::data.table(
    label = c("A", "B", "C"),
    value = c(1, 3, 2)
  )
  out <- aq_report_sort_for_flipped_bar(dt, "value", "label")

  data.table::data.table(
    check = "flipped_bar_ascending_plot_data",
    status = if (identical(as.character(out$label), c("A", "C", "B")) && identical(tail(as.character(out$label), 1L), "B")) "success" else "error",
    message = "Ascending plot-data order leaves the largest value last before coordinate flip, so it appears at the top after flipping."
  )
}

aq_report_read_source <- function(paths) {
  existing <- paths[file.exists(paths)]
  if (!length(existing)) {
    return("")
  }

  paste(vapply(existing, function(path) paste(readLines(path, warn = FALSE), collapse = "\n"), character(1L)), collapse = "\n")
}

qa_report_axis_formatting_guidelines <- function() {
  paths <- file.path(getwd(), c(
    "R/eda_artifact_generation.R",
    "R/target_model_readiness_artifacts.R",
    "R/generate_regression_model_insights_artifacts.R",
    "R/generate_binary_classification_model_insights_artifacts.R",
    "R/generate_regression_shap_analysis_artifacts.R",
    "R/generate_binary_classification_shap_analysis_artifacts.R"
  ))
  source <- aq_report_read_source(paths)

  data.table::rbindlist(list(
    data.table::data.table(
      check = "no_report_side_axislabel_object_overrides",
      status = if (!grepl("axisLabel\\s*=\\s*list|e_[xy]_axis\\([^\\n]*axisLabel\\s*=", source)) "success" else "error",
      message = "Modern report generators do not inject report-side axisLabel style objects."
    ),
    data.table::data.table(
      check = "autoplots_x_axis_rotation_preserved",
      status = if (grepl("xAxis\\.axisLabel\\.rotate", source)) "success" else "error",
      message = "Readable x-axis rotation is passed through AutoPlots xAxis.axisLabel.rotate."
    ),
    qa_report_flipped_bar_ordering_guideline()
  ), fill = TRUE)
}

qa_report_numeric_formatting_guidelines <- function() {
  if (exists("qa_regression_shap_formatting_ordering", mode = "function")) {
    shap_checks <- qa_regression_shap_formatting_ordering()
    shap_numeric <- shap_checks[check %in% c("table_rounding_applied", "box_tooltips_disabled", "heatmap_tooltips_disabled_when_crowded")]
  } else {
    shap_numeric <- data.table::data.table()
  }

  data.table::rbindlist(list(
    shap_numeric,
    data.table::data.table(
      check = "shared_numeric_report_checks_available",
      status = "success",
      message = "Shared report numeric display checks are available for rounded tables and tooltip behavior."
    )
  ), fill = TRUE)
}

qa_report_bin_ordering_guidelines <- function() {
  if (exists("qa_regression_shap_formatting_ordering", mode = "function")) {
    shap_checks <- qa_regression_shap_formatting_ordering()
    shap_bins <- shap_checks[check %in% c("numeric_bins_ordered_factor", "cut_bins_ordered_factor", "interaction_bins_ordered_factor", "plot_category_order_preserved")]
  } else {
    shap_bins <- data.table::data.table()
  }

  data.table::rbindlist(list(
    shap_bins,
    qa_report_flipped_bar_ordering_guideline()
  ), fill = TRUE)
}

qa_report_heatmap_axis_guidelines <- function() {
  paths <- file.path(getwd(), c(
    "R/eda_artifact_generation.R",
    "R/generate_regression_model_insights_artifacts.R",
    "R/generate_binary_classification_model_insights_artifacts.R",
    "R/generate_regression_shap_analysis_artifacts.R"
  ))
  source <- aq_report_read_source(paths)

  data.table::data.table(
    check = c(
      "heatmap_x_axis_rotation_preserved",
      "heatmap_tooltip_crowding_rule_present"
    ),
    status = c(
      if (grepl("xAxis\\.axisLabel\\.rotate", source)) "success" else "error",
      if (grepl("aq_report_dense_axis_tooltip", source, fixed = TRUE) || grepl("aq_shap_dense_axis_tooltip", source, fixed = TRUE)) "success" else "error"
    ),
    message = c(
      "Heatmap x-axis labels use AutoPlots-native rotation where needed.",
      "Heatmap tooltips can be disabled when axis labels are crowded."
    )
  )
}
