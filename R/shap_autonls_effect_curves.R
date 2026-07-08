# ============================================================
# Optional AutoNLS-backed SHAP effect curve artifacts
#
# This bridge is intentionally optional. AutoQuant keeps accepting
# original-scale SHAP source data and only calls AutoNLS when requested.
# ============================================================

aq_shap_effect_curve_empty <- function() {
  list(
    values = data.table::data.table(),
    diagnostics = data.table::data.table(),
    summary = data.table::data.table(),
    warnings = character()
  )
}

aq_shap_effect_curve_available <- function(backend, force_unavailable = FALSE) {
  backend <- match.arg(as.character(backend)[1L], c("none", "autonls"))
  if (identical(backend, "none")) {
    return(list(available = FALSE, reason = "backend_none"))
  }
  if (isTRUE(force_unavailable) || !requireNamespace("AutoNLS", quietly = TRUE)) {
    return(list(available = FALSE, reason = "autonls_not_installed"))
  }
  list(available = TRUE, reason = "available")
}

aq_shap_effect_curve_models <- function(effect_curve_models) {
  models <- as.character(effect_curve_models)
  models <- models[nzchar(models)]
  if (!length(models)) models <- "stable"

  if (length(models) == 1L && models %in% c("stable", "experimental", "all")) {
    return(list(models = "all", model_status = models))
  }
  list(models = models, model_status = "all")
}

aq_shap_effect_curve_scalar <- function(x, default = NA_character_) {
  if (is.null(x) || length(x) == 0L) return(default)
  out <- x[[1L]]
  if (is.null(out) || length(out) == 0L || is.na(out)) return(default)
  as.character(out)
}

aq_shap_effect_curve_numeric <- function(x, default = NA_real_) {
  if (is.null(x) || length(x) == 0L) return(default)
  out <- suppressWarnings(as.numeric(x[[1L]]))
  if (is.na(out)) default else out
}

aq_shap_effect_curve_artifacts_from_fit <- function(fit) {
  if (is.list(fit) && all(c("curve_values", "curve_diagnostics", "selected_model") %in% names(fit))) {
    return(fit)
  }
  if (is.list(fit) && !is.null(fit$artifacts) && is.function(fit$artifacts)) {
    return(fit$artifacts())
  }
  if (requireNamespace("AutoNLS", quietly = TRUE) && exists("generate_autonls_artifacts", envir = asNamespace("AutoNLS"), inherits = FALSE)) {
    return(get("generate_autonls_artifacts", envir = asNamespace("AutoNLS"))(fit))
  }
  stop("AutoNLS fit did not expose vNext artifacts.", call. = FALSE)
}

aq_shap_effect_curve_fit <- function(
  dependence_data,
  effect_curve_models = "stable",
  validation_fraction = 0.2,
  effect_curve_theme = NULL,
  autonls_runner = NULL
) {
  if (!is.null(autonls_runner)) {
    return(autonls_runner(dependence_data))
  }
  model_args <- aq_shap_effect_curve_models(effect_curve_models)
  AutoNLS::AutoNLS(
    data = dependence_data,
    x = "feature_value",
    y = "shap_value",
    models = model_args$models,
    model_status = model_args$model_status,
    validation_fraction = validation_fraction,
    scale_x = TRUE,
    scale_y = TRUE,
    start_strategy = "family_transformed",
    theme = effect_curve_theme
  )
}

aq_shap_effect_curve_normalize <- function(feature_name, artifacts, status = "success", reason = "") {
  curve_values <- data.table::as.data.table(regression_shap_null_coalesce(artifacts$curve_values, data.table::data.table()))
  selected_model <- data.table::as.data.table(regression_shap_null_coalesce(artifacts$selected_model, data.table::data.table()))
  model_confidence <- data.table::as.data.table(regression_shap_null_coalesce(artifacts$model_confidence, data.table::data.table()))
  fit_warnings <- data.table::as.data.table(regression_shap_null_coalesce(artifacts$fit_warnings, data.table::data.table()))
  fit_recommendations <- data.table::as.data.table(regression_shap_null_coalesce(artifacts$fit_recommendations, data.table::data.table()))
  curve_diagnostics <- data.table::as.data.table(regression_shap_null_coalesce(artifacts$curve_diagnostics, data.table::data.table()))

  selected <- aq_shap_effect_curve_scalar(selected_model$model_name, "")
  if (!nzchar(selected)) selected <- aq_shap_effect_curve_scalar(curve_values$model_name, "")
  family <- aq_shap_effect_curve_scalar(selected_model$family, "")
  if (!nzchar(family)) family <- aq_shap_effect_curve_scalar(curve_values$family, "")
  confidence <- aq_shap_effect_curve_numeric(model_confidence$confidence, NA_real_)
  if (is.na(confidence)) confidence <- aq_shap_effect_curve_numeric(model_confidence$model_confidence, NA_real_)
  warning_count <- as.integer(nrow(fit_warnings))
  if (identical(status, "success") && !is.na(confidence) && confidence < 0.25) {
    status <- "skipped"
    reason <- "low_model_confidence"
    curve_values <- data.table::data.table()
  }

  if (nrow(curve_values) && "is_best_model" %in% names(curve_values)) {
    best_values <- curve_values[is_best_model == TRUE]
    if (nrow(best_values)) curve_values <- best_values
  } else if (nrow(curve_values) && nzchar(selected) && "model_name" %in% names(curve_values)) {
    selected_values <- curve_values[model_name == selected]
    if (nrow(selected_values)) curve_values <- selected_values
  }

  value_cols <- names(curve_values)
  x_col <- intersect(c("x", "feature_value", "x_original"), value_cols)[1L]
  y_col <- intersect(c("y_hat", "prediction", "fit", "shap_curve_value"), value_cols)[1L]
  lower_col <- intersect(c("lower", "lower_bound", "interval_lower"), value_cols)[1L]
  upper_col <- intersect(c("upper", "upper_bound", "interval_upper"), value_cols)[1L]

  values <- data.table::data.table()
  if (nrow(curve_values) && !is.na(x_col) && !is.na(y_col)) {
    values <- data.table::data.table(
      feature = feature_name,
      feature_value = curve_values[[x_col]],
      shap_curve_value = curve_values[[y_col]],
      lower = if (!is.na(lower_col)) curve_values[[lower_col]] else NA_real_,
      upper = if (!is.na(upper_col)) curve_values[[upper_col]] else NA_real_,
      derivative = if ("derivative" %in% value_cols) curve_values[["derivative"]] else NA_real_,
      elasticity = if ("elasticity" %in% value_cols) curve_values[["elasticity"]] else NA_real_,
      selected_model = selected,
      curve_family = family,
      confidence = confidence,
      derivative_available = "derivative" %in% value_cols && any(is.finite(suppressWarnings(as.numeric(curve_values[["derivative"]])))),
      elasticity_available = "elasticity" %in% value_cols && any(is.finite(suppressWarnings(as.numeric(curve_values[["elasticity"]])))),
      warning_count = warning_count,
      backend = "AutoNLS",
      predictions_original_scale = if ("original_scale" %in% value_cols) all(curve_values[["original_scale"]] == TRUE, na.rm = TRUE) else TRUE
    )
  }

  summary <- data.table::data.table(
    feature = feature_name,
    selected_model = selected,
    curve_family = family,
    confidence = confidence,
    derivative_available = nrow(values) > 0L && any(values$derivative_available),
    elasticity_available = nrow(values) > 0L && any(values$elasticity_available),
    warning_count = warning_count,
    backend = "AutoNLS",
    status = status,
    reason = reason,
    predictions_original_scale = if (nrow(values)) all(values$predictions_original_scale == TRUE, na.rm = TRUE) else TRUE,
    recommendation_count = as.integer(nrow(fit_recommendations))
  )

  diagnostics <- data.table::data.table(
    feature = feature_name,
    backend = "AutoNLS",
    status = status,
    reason = reason,
    selected_model = selected,
    curve_family = family,
    confidence = confidence,
    metric = "warning_count",
    value = as.character(warning_count),
    warning_count = warning_count,
    predictions_original_scale = summary$predictions_original_scale
  )
  if (nrow(curve_diagnostics)) {
    diag_names <- names(curve_diagnostics)
    metric_col <- intersect(c("metric", "diagnostic", "name", "check"), diag_names)[1L]
    value_col <- intersect(c("value", "status", "message", "result"), diag_names)[1L]
    extra <- data.table::data.table(
      feature = feature_name,
      backend = "AutoNLS",
      status = status,
      reason = reason,
      selected_model = selected,
      curve_family = family,
      confidence = confidence,
      metric = if (!is.na(metric_col)) as.character(curve_diagnostics[[metric_col]]) else paste0("curve_diagnostic_", seq_len(nrow(curve_diagnostics))),
      value = if (!is.na(value_col)) as.character(curve_diagnostics[[value_col]]) else "",
      warning_count = warning_count,
      predictions_original_scale = summary$predictions_original_scale
    )
    diagnostics <- data.table::rbindlist(list(diagnostics, extra), fill = TRUE)
  }

  list(values = values, diagnostics = diagnostics, summary = summary)
}

aq_generate_shap_effect_curve_artifacts <- function(
  data,
  column_map,
  global_importance,
  selected_features = NULL,
  include_effect_curves = TRUE,
  effect_curve_backend = c("none", "autonls"),
  effect_curve_models = "stable",
  effect_curve_sample_size = 50000L,
  effect_curve_max_features = 20L,
  effect_curve_validation_fraction = 0.2,
  effect_curve_theme = NULL,
  autonls_runner = NULL,
  force_unavailable = FALSE
) {
  out <- aq_shap_effect_curve_empty()
  if (!isTRUE(include_effect_curves)) return(out)

  backend <- match.arg(as.character(effect_curve_backend)[1L], c("none", "autonls"))
  available <- aq_shap_effect_curve_available(backend, force_unavailable = force_unavailable)
  if (!available$available && is.null(autonls_runner)) {
    if (!identical(available$reason, "backend_none")) {
      out$diagnostics <- data.table::data.table(
        feature = NA_character_,
        backend = "AutoNLS",
        status = "skipped",
        reason = available$reason,
        selected_model = "",
        curve_family = "",
        confidence = NA_real_,
        metric = "backend_available",
        value = "FALSE",
        warning_count = 0L,
        predictions_original_scale = TRUE
      )
      out$warnings <- paste("AutoNLS effect curves skipped:", available$reason)
    }
    return(out)
  }

  effect_curve_sample_size <- regression_shap_positive_int(effect_curve_sample_size, 50000L, "effect_curve_sample_size", character())$value
  effect_curve_max_features <- regression_shap_positive_int(effect_curve_max_features, 20L, "effect_curve_max_features", character())$value
  effect_curve_validation_fraction <- suppressWarnings(as.numeric(effect_curve_validation_fraction[1L]))
  if (is.na(effect_curve_validation_fraction)) effect_curve_validation_fraction <- 0.2
  effect_curve_validation_fraction <- max(0, min(0.5, effect_curve_validation_fraction))

  candidate_features <- intersect(selected_features, column_map[included == TRUE & source_col_exists == TRUE, feature])
  if (!length(candidate_features)) {
    candidate_features <- intersect(global_importance$feature, column_map[included == TRUE & source_col_exists == TRUE, feature])
  }
  numeric_features <- candidate_features[vapply(candidate_features, function(feature_name) {
    feature_name %in% names(data) && is.numeric(data[[feature_name]])
  }, logical(1L))]
  numeric_features <- head(numeric_features, effect_curve_max_features)
  if (!length(numeric_features)) {
    out$diagnostics <- data.table::data.table(
      feature = NA_character_,
      backend = "AutoNLS",
      status = "skipped",
      reason = "no_numeric_shap_dependence_features",
      selected_model = "",
      curve_family = "",
      confidence = NA_real_,
      metric = "eligible_feature_count",
      value = "0",
      warning_count = 0L,
      predictions_original_scale = TRUE
    )
    return(out)
  }

  values <- list()
  diagnostics <- list()
  summaries <- list()
  fit_warnings <- character()
  for (feature_name in numeric_features) {
    shap_col <- column_map[feature == feature_name, shap_col][[1L]]
    dependence_data <- data.table::data.table(
      feature_value = suppressWarnings(as.numeric(data[[feature_name]])),
      shap_value = suppressWarnings(as.numeric(data[[shap_col]]))
    )
    dependence_data <- dependence_data[is.finite(feature_value) & is.finite(shap_value)]
    if (nrow(dependence_data) < 10L || data.table::uniqueN(dependence_data$feature_value) < 4L) {
      normalized <- aq_shap_effect_curve_normalize(feature_name, list(), status = "skipped", reason = "insufficient_numeric_rows")
    } else {
      if (nrow(dependence_data) > effect_curve_sample_size) {
        set.seed(123L)
        dependence_data <- dependence_data[sort(sample(seq_len(.N), effect_curve_sample_size))]
      }
      normalized <- tryCatch(
        {
          fit <- aq_shap_effect_curve_fit(
            dependence_data = dependence_data,
            effect_curve_models = effect_curve_models,
            validation_fraction = effect_curve_validation_fraction,
            effect_curve_theme = effect_curve_theme,
            autonls_runner = autonls_runner
          )
          aq_shap_effect_curve_normalize(feature_name, aq_shap_effect_curve_artifacts_from_fit(fit))
        },
        error = function(e) {
          msg <- conditionMessage(e)
          fit_warnings <<- unique(c(fit_warnings, paste("AutoNLS effect curve failed for", feature_name, ":", msg)))
          aq_shap_effect_curve_normalize(feature_name, list(), status = "failed", reason = msg)
        }
      )
    }
    values[[feature_name]] <- normalized$values
    diagnostics[[feature_name]] <- normalized$diagnostics
    summaries[[feature_name]] <- normalized$summary
  }

  out$values <- data.table::rbindlist(values, fill = TRUE)
  out$diagnostics <- data.table::rbindlist(diagnostics, fill = TRUE)
  out$summary <- data.table::rbindlist(summaries, fill = TRUE)
  out$warnings <- fit_warnings
  out
}

#' QA for optional AutoNLS-backed SHAP effect curve artifacts
#'
#' @return A data.table with QA checks and statuses.
#' @export
qa_shap_autonls_backend <- function() {
  set.seed(42L)
  n <- 80L
  dt <- data.table::data.table(
    Spend = seq(1, 100, length.out = n),
    Noise = rnorm(n),
    Shap_Spend = 2 + 8 * seq(1, 100, length.out = n) / (20 + seq(1, 100, length.out = n)) + rnorm(n, sd = 0.05),
    Shap_Noise = rnorm(n)
  )
  column_map <- data.table::data.table(
    shap_col = c("Shap_Spend", "Shap_Noise"),
    feature = c("Spend", "Noise"),
    included = TRUE,
    source_col_exists = TRUE
  )
  global_importance <- data.table::data.table(
    feature = c("Spend", "Noise"),
    rank = 1:2,
    mean_abs_shap = c(3, 1)
  )
  stub_runner <- function(dependence_data) {
    grid <- data.table::data.table(
      x = seq(min(dependence_data$feature_value), max(dependence_data$feature_value), length.out = 25L)
    )
    grid[, `:=`(y_hat = 2 + 8 * x / (20 + x))]
    grid[, `:=`(
      lower = y_hat - 0.1,
      upper = y_hat + 0.1,
      derivative = 160 / ((20 + x)^2),
      model_name = "Hill",
      family = "saturation",
      is_best_model = TRUE,
      original_scale = TRUE
    )]
    grid[, `:=`(elasticity = derivative * x / pmax(abs(y_hat), .Machine$double.eps))]
    list(
      curve_values = grid,
      curve_diagnostics = data.table::data.table(metric = "predictions_original_scale", value = "TRUE"),
      selected_model = data.table::data.table(model_name = "Hill", family = "saturation"),
      model_confidence = data.table::data.table(confidence = 0.91),
      fit_warnings = data.table::data.table(),
      fit_recommendations = data.table::data.table(recommendation = "ok")
    )
  }

  none_result <- aq_generate_shap_effect_curve_artifacts(dt, column_map, global_importance, include_effect_curves = TRUE, effect_curve_backend = "none")
  unavailable_result <- aq_generate_shap_effect_curve_artifacts(dt, column_map, global_importance, include_effect_curves = TRUE, effect_curve_backend = "autonls", force_unavailable = TRUE)
  stub_result <- aq_generate_shap_effect_curve_artifacts(dt, column_map, global_importance, selected_features = "Spend", include_effect_curves = TRUE, effect_curve_backend = "autonls", autonls_runner = stub_runner)
  low_confidence_runner <- function(dependence_data) {
    out <- stub_runner(dependence_data)
    out$model_confidence$confidence <- 0.10
    out
  }
  low_confidence_result <- aq_generate_shap_effect_curve_artifacts(dt, column_map, global_importance, selected_features = "Spend", include_effect_curves = TRUE, effect_curve_backend = "autonls", autonls_runner = low_confidence_runner)
  failure_result <- aq_generate_shap_effect_curve_artifacts(dt, column_map, global_importance, selected_features = "Spend", include_effect_curves = TRUE, effect_curve_backend = "autonls", autonls_runner = function(x) stop("intentional failure", call. = FALSE))

  regression_result <- generate_regression_shap_analysis_artifacts(dt, include_plots = FALSE, include_effect_curves = TRUE, effect_curve_backend = "autonls", effect_curve_max_features = 1L, effect_curve_sample_size = 60L, autonls_runner = stub_runner)
  binary_dt <- data.table::copy(dt)
  binary_dt[, `:=`(Target = rep(c("Yes", "No"), length.out = .N))]
  binary_result <- generate_binary_classification_shap_analysis_artifacts(binary_dt, target_col = "Target", positive_class = "Yes", include_plots = FALSE, include_effect_curves = TRUE, effect_curve_backend = "autonls", effect_curve_max_features = 1L, effect_curve_sample_size = 60L, autonls_runner = stub_runner)

  checks <- data.table::data.table(
    check = c(
      "backend_none_no_artifacts",
      "autonls_unavailable_diagnostic",
      "autonls_stub_values",
      "autonls_stub_summary_fields",
      "autonls_derivative_elasticity_available",
      "autonls_original_scale_predictions",
      "autonls_low_confidence_skips_values",
      "autonls_failure_diagnostic",
      "regression_generator_artifacts",
      "binary_generator_artifacts"
    ),
    status = c(
      if (!nrow(none_result$values) && !nrow(none_result$summary)) "success" else "error",
      if (nrow(unavailable_result$diagnostics) && unavailable_result$diagnostics$reason[1L] == "autonls_not_installed") "success" else "error",
      if (nrow(stub_result$values) > 0L) "success" else "error",
      if (all(c("feature", "selected_model", "curve_family", "confidence", "derivative_available", "elasticity_available", "warning_count", "backend") %in% names(stub_result$summary))) "success" else "error",
      if (isTRUE(stub_result$summary$derivative_available[1L]) && isTRUE(stub_result$summary$elasticity_available[1L])) "success" else "error",
      if (isTRUE(stub_result$summary$predictions_original_scale[1L])) "success" else "error",
      if (!nrow(low_confidence_result$values) && low_confidence_result$summary$status[1L] == "skipped" && low_confidence_result$summary$reason[1L] == "low_model_confidence") "success" else "error",
      if (nrow(failure_result$diagnostics) && failure_result$summary$status[1L] == "failed") "success" else "error",
      if (all(c("shap_effect_curve_values", "shap_effect_curve_diagnostics", "shap_effect_curve_summary") %in% names(regression_result$artifacts))) "success" else "error",
      if (all(c("shap_effect_curve_values", "shap_effect_curve_diagnostics", "shap_effect_curve_summary") %in% names(binary_result$artifacts))) "success" else "error"
    ),
    detail = c(
      "Backend none leaves effect-curve artifacts empty.",
      "Unavailable AutoNLS returns diagnostics without failing SHAP.",
      "Stub AutoNLS runner produces original-scale curve values.",
      "Summary contains required AutoQuant SHAP effect-curve fields.",
      "Derivative and elasticity availability are propagated.",
      "Final predictions are marked original-scale.",
      "Low-confidence AutoNLS results skip curve values and keep diagnostics.",
      "AutoNLS fit failures are diagnostics, not SHAP failures.",
      "Regression SHAP generator emits optional effect-curve artifacts.",
      "Binary SHAP generator emits optional effect-curve artifacts."
    )
  )
  checks
}
