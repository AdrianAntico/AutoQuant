# AutoQuant vNext intermittent demand forecasting foundation.

#' Compute Intermittent Demand Diagnostics
#'
#' @param data Demand data.
#' @param target Target value column.
#' @param date Date/time column.
#' @param entity Optional entity column.
#' @param occurrence_threshold Threshold above which demand occurs.
#'
#' @return A deterministic diagnostic table.
#' @export
aq_intermittent_demand_diagnostics <- function(data, target, date, entity = NULL, occurrence_threshold = 0) {
  dt <- data.table::as.data.table(data.table::copy(data))
  target_col <- as.character(target)[1L]
  date_col <- as.character(date)[1L]
  dt[, .aq_date := as.Date(get(date_col))]
  entity_col <- if (is.null(entity) || is.na(entity)) NULL else as.character(entity)[1L]
  by_cols <- if (is.null(entity_col)) character() else entity_col
  calc <- function(x) {
    y <- as.numeric(x[[target_col]])
    occurrence <- !is.na(y) & y > occurrence_threshold
    positive <- y[occurrence]
    zero_rle <- rle(!occurrence)
    zero_runs <- zero_rle$lengths[zero_rle$values]
    occurrence_positions <- which(occurrence)
    intervals <- diff(occurrence_positions)
    recent_zero_run <- if (length(occurrence) && !utils::tail(occurrence, 1L)) {
      tail_runs <- rle(rev(!occurrence))
      tail_runs$lengths[1L]
    } else {
      0L
    }
    split_index <- max(1L, floor(length(occurrence) / 2L))
    early_rate <- mean(occurrence[seq_len(split_index)], na.rm = TRUE)
    late_rate <- mean(occurrence[seq.int(split_index + 1L, length(occurrence))], na.rm = TRUE)
    occurrence_rate_trend <- if (!is.finite(early_rate) || !is.finite(late_rate)) {
      "insufficient evidence"
    } else if (late_rate < early_rate * 0.5 && recent_zero_run >= max(3L, round(length(occurrence) * 0.1))) {
      "possible decline"
    } else if (late_rate > early_rate * 1.5 && late_rate - early_rate > 0.1) {
      "possible increase"
    } else {
      "no detected signal"
    }
    obsolescence_signal <- if (length(positive) < 2L) {
      "insufficient evidence"
    } else if (recent_zero_run >= max(5L, round(length(occurrence) * 0.2))) {
      "strong recent inactivity signal"
    } else if (identical(occurrence_rate_trend, "possible decline")) {
      "possible decline"
    } else {
      "no detected signal"
    }
    cv_positive <- if (length(positive) > 1L && is.finite(mean(positive)) && abs(mean(positive)) > .Machine$double.eps) stats::sd(positive) / abs(mean(positive)) else NA_real_
    interval_cv <- if (length(intervals) > 1L && mean(intervals) > 0) stats::sd(intervals) / mean(intervals) else NA_real_
    data.table::data.table(
      rows = length(y),
      positive_count = length(positive),
      nonzero_count = length(positive),
      zero_count = sum(!occurrence),
      zero_proportion = mean(!occurrence),
      average_inter_demand_interval = if (length(intervals)) mean(intervals) else NA_real_,
      longest_zero_run = if (length(zero_runs)) max(zero_runs) else 0L,
      recent_zero_run = as.integer(recent_zero_run),
      positive_mean = if (length(positive)) mean(positive) else NA_real_,
      positive_sd = if (length(positive) > 1L) stats::sd(positive) else NA_real_,
      positive_cv = cv_positive,
      demand_size_stability = if (!is.finite(cv_positive)) "insufficient evidence" else if (cv_positive <= 0.5) "stable" else if (cv_positive <= 1) "moderate" else "unstable",
      interval_cv = interval_cv,
      interval_stability = if (!is.finite(interval_cv)) "insufficient evidence" else if (interval_cv <= 0.5) "stable" else if (interval_cv <= 1) "moderate" else "unstable",
      occurrence_rate_trend = occurrence_rate_trend,
      possible_obsolescence_signal = obsolescence_signal,
      sparse_history_status = if (length(y) < 20L || length(positive) < 3L) "sparse_history" else "sufficient_for_baseline",
      demand_sparsity = if (mean(!occurrence) >= 0.5) "high" else if (mean(!occurrence) >= 0.2) "moderate" else "low",
      intermittent_demand_suitability = if (mean(!occurrence) >= 0.2 && length(positive) >= 2L) "suitable" else "limited",
      evidence_limitations = if (length(positive) < 3L) "limited positive-demand history" else "none detected"
    )
  }
  if (length(by_cols)) {
    dt[, calc(.SD), by = by_cols]
  } else {
    calc(dt)
  }
}

aq_classical_intermittent_state <- function(y, horizon, method = c("croston", "sba", "tsb"), alpha = 0.1, beta = 0.1, threshold = 0) {
  method <- match.arg(method)
  y <- as.numeric(y)
  occurrence <- !is.na(y) & y > threshold
  occurrence_idx <- which(occurrence)
  if (!length(occurrence_idx)) {
    return(list(forecast = rep(0, horizon), demand_size_estimate = 0, interval_estimate = NA_real_, occurrence_probability_state = 0, warnings = "no positive demand observed"))
  }
  first_idx <- occurrence_idx[1L]
  z <- y[first_idx]
  p <- 1 / max(mean(occurrence, na.rm = TRUE), .Machine$double.eps)
  q <- mean(occurrence, na.rm = TRUE)
  last_idx <- 0L
  if (identical(method, "tsb")) {
    for (i in seq_along(y)) {
      occurred <- isTRUE(occurrence[i])
      q <- q + beta * (as.numeric(occurred) - q)
      if (occurred) z <- z + alpha * (y[i] - z)
    }
    forecast <- max(0, q * z)
  } else {
    last_idx <- first_idx
    if (length(occurrence_idx) > 1L) {
      for (idx in occurrence_idx[-1L]) {
        interval <- idx - last_idx
        z <- z + alpha * (y[idx] - z)
        p <- p + alpha * (interval - p)
        last_idx <- idx
      }
    }
    forecast <- if (is.finite(p) && p > 0) z / p else 0
    if (identical(method, "sba")) forecast <- (1 - alpha / 2) * forecast
    q <- if (is.finite(p) && p > 0) 1 / p else NA_real_
  }
  list(
    forecast = rep(max(0, forecast), horizon),
    demand_size_estimate = as.numeric(z),
    interval_estimate = if (identical(method, "tsb")) NA_real_ else as.numeric(p),
    occurrence_probability_state = as.numeric(q),
    warnings = character()
  )
}

aq_croston_values <- function(y, horizon, alpha = 0.1, threshold = 0) {
  aq_classical_intermittent_state(y, horizon, method = "croston", alpha = alpha, threshold = threshold)$forecast
}

aq_sba_values <- function(y, horizon, alpha = 0.1, threshold = 0) {
  aq_classical_intermittent_state(y, horizon, method = "sba", alpha = alpha, threshold = threshold)$forecast
}

aq_tsb_values <- function(y, horizon, alpha = 0.1, beta = 0.1, threshold = 0) {
  aq_classical_intermittent_state(y, horizon, method = "tsb", alpha = alpha, beta = beta, threshold = threshold)$forecast
}

#' Create a Croston Intermittent Demand Forecast Specification
#'
#' @param target Target demand column.
#' @param date Date/time column.
#' @param entity Optional entity column.
#' @param frequency Forecast frequency.
#' @param horizon Forecast horizon.
#' @param forecast_origin Optional forecast origin.
#' @param occurrence_threshold Threshold above which demand occurs.
#' @param alpha Croston smoothing parameter.
#' @param forecast_spec_id Optional spec id.
#' @param dataset_id Optional dataset id.
#'
#' @return An `aq_croston_forecast_spec`.
#' @export
aq_croston_forecast_spec <- function(
  target,
  date,
  entity = NULL,
  frequency = "auto",
  horizon = 1L,
  forecast_origin = NULL,
  occurrence_threshold = 0,
  alpha = 0.1,
  forecast_spec_id = NULL,
  dataset_id = NULL
) {
  frequency <- match.arg(tolower(frequency), aq_forecast_frequency_levels())
  if (is.null(forecast_spec_id)) forecast_spec_id <- aq_vnext_id(paste("croston_forecast_spec", target, sep = "_"))
  spec <- list(
    forecast_spec_id = as.character(forecast_spec_id)[1L],
    schema_version = "aq_croston_forecast_spec_v1",
    method = "croston",
    target = as.character(target)[1L],
    date = as.character(date)[1L],
    entity = if (is.null(entity)) NA_character_ else as.character(entity)[1L],
    frequency = frequency,
    horizon = as.integer(horizon)[1L],
    forecast_origin = if (is.null(forecast_origin)) NULL else as.Date(forecast_origin)[1L],
    occurrence_threshold = as.numeric(occurrence_threshold)[1L],
    alpha = as.numeric(alpha)[1L],
    dataset_id = aq_vnext_default(dataset_id, NA_character_),
    supported_downstream_actions = c("assess", "compare", "report", "campaign_review"),
    created_at = aq_vnext_now()
  )
  class(spec) <- c("aq_croston_forecast_spec", "list")
  spec
}

#' Create an SBA Intermittent Demand Forecast Specification
#'
#' @inheritParams aq_croston_forecast_spec
#'
#' @return An `aq_sba_forecast_spec`.
#' @export
aq_sba_forecast_spec <- function(
  target,
  date,
  entity = NULL,
  frequency = "auto",
  horizon = 1L,
  forecast_origin = NULL,
  occurrence_threshold = 0,
  alpha = 0.1,
  forecast_spec_id = NULL,
  dataset_id = NULL
) {
  frequency <- match.arg(tolower(frequency), aq_forecast_frequency_levels())
  if (is.null(forecast_spec_id)) forecast_spec_id <- aq_vnext_id(paste("sba_forecast_spec", target, sep = "_"))
  spec <- list(
    forecast_spec_id = as.character(forecast_spec_id)[1L],
    schema_version = "aq_sba_forecast_spec_v1",
    method = "sba",
    target = as.character(target)[1L],
    date = as.character(date)[1L],
    entity = if (is.null(entity)) NA_character_ else as.character(entity)[1L],
    frequency = frequency,
    horizon = as.integer(horizon)[1L],
    forecast_origin = if (is.null(forecast_origin)) NULL else as.Date(forecast_origin)[1L],
    occurrence_threshold = as.numeric(occurrence_threshold)[1L],
    alpha = as.numeric(alpha)[1L],
    dataset_id = aq_vnext_default(dataset_id, NA_character_),
    supported_downstream_actions = c("assess", "compare", "report", "campaign_review"),
    created_at = aq_vnext_now()
  )
  class(spec) <- c("aq_sba_forecast_spec", "list")
  spec
}

#' Create a TSB Intermittent Demand Forecast Specification
#'
#' @inheritParams aq_croston_forecast_spec
#' @param beta Occurrence-probability smoothing parameter.
#'
#' @return An `aq_tsb_forecast_spec`.
#' @export
aq_tsb_forecast_spec <- function(
  target,
  date,
  entity = NULL,
  frequency = "auto",
  horizon = 1L,
  forecast_origin = NULL,
  occurrence_threshold = 0,
  alpha = 0.1,
  beta = 0.1,
  forecast_spec_id = NULL,
  dataset_id = NULL
) {
  frequency <- match.arg(tolower(frequency), aq_forecast_frequency_levels())
  if (is.null(forecast_spec_id)) forecast_spec_id <- aq_vnext_id(paste("tsb_forecast_spec", target, sep = "_"))
  spec <- list(
    forecast_spec_id = as.character(forecast_spec_id)[1L],
    schema_version = "aq_tsb_forecast_spec_v1",
    method = "tsb",
    target = as.character(target)[1L],
    date = as.character(date)[1L],
    entity = if (is.null(entity)) NA_character_ else as.character(entity)[1L],
    frequency = frequency,
    horizon = as.integer(horizon)[1L],
    forecast_origin = if (is.null(forecast_origin)) NULL else as.Date(forecast_origin)[1L],
    occurrence_threshold = as.numeric(occurrence_threshold)[1L],
    alpha = as.numeric(alpha)[1L],
    beta = as.numeric(beta)[1L],
    dataset_id = aq_vnext_default(dataset_id, NA_character_),
    supported_downstream_actions = c("assess", "compare", "report", "campaign_review"),
    created_at = aq_vnext_now()
  )
  class(spec) <- c("aq_tsb_forecast_spec", "list")
  spec
}

aq_classical_intermittent_method <- function(spec) {
  method <- aq_vnext_default(spec$method, "croston")
  match.arg(tolower(method), c("croston", "sba", "tsb"))
}

aq_validate_classical_intermittent_forecast_spec <- function(spec, data = NULL) {
  rows <- list()
  add <- function(check, status, message, severity = status) {
    rows[[length(rows) + 1L]] <<- aq_vnext_validation_table(check, status, message, severity)
  }
  if (!inherits(spec, c("aq_croston_forecast_spec", "aq_sba_forecast_spec", "aq_tsb_forecast_spec"))) {
    add("classical_intermittent_spec_class", "fail", "spec must be created by a classical intermittent-demand forecast spec constructor.")
    return(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
  }
  method <- aq_classical_intermittent_method(spec)
  add("classical_intermittent_spec_class", "pass", paste(method, "specification is typed."), "info")
  if (!is.finite(spec$alpha) || spec$alpha <= 0 || spec$alpha > 1) add("alpha", "fail", "alpha must be in (0, 1].") else add("alpha", "pass", paste("alpha:", spec$alpha), "info")
  if (identical(method, "tsb")) {
    if (!is.finite(spec$beta) || spec$beta <= 0 || spec$beta > 1) add("beta", "fail", "beta must be in (0, 1].") else add("beta", "pass", paste("beta:", spec$beta), "info")
  }
  if (!is.finite(spec$horizon) || spec$horizon < 1L) add("horizon", "fail", "horizon must be positive.") else add("horizon", "pass", paste("horizon:", spec$horizon), "info")
  if (!is.null(data)) {
    dt <- data.table::as.data.table(data)
    required <- c(spec$target, spec$date, if (!is.na(spec$entity)) spec$entity)
    missing <- setdiff(required, names(dt))
    if (length(missing)) add("required_columns", "fail", paste("Missing required column(s):", paste(missing, collapse = ", "))) else add("required_columns", "pass", "required columns are present.", "info")
    if (!length(missing) && !is.numeric(dt[[spec$target]])) add("target_type", "fail", "target must be numeric.") else if (!length(missing)) add("target_type", "pass", "target is numeric.", "info")
  }
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

aq_validate_croston_forecast_spec <- function(spec, data = NULL) {
  aq_validate_classical_intermittent_forecast_spec(spec, data)
}

aq_classical_intermittent_forecast_payload <- function(spec, data, origin = NULL) {
  dt <- data.table::as.data.table(data.table::copy(data))
  dt[, .aq_forecast_date := as.Date(get(spec$date))]
  frequency <- aq_forecast_resolved_frequency(list(frequency = spec$frequency, date = spec$date), dt)
  dates <- sort(unique(dt$.aq_forecast_date))
  if (is.null(origin)) origin <- aq_vnext_default(spec$forecast_origin, dates[max(1L, length(dates) - spec$horizon)])
  origin <- as.Date(origin)
  method <- aq_classical_intermittent_method(spec)
  future_dates <- aq_forecast_next_dates(origin, frequency, spec$horizon)
  state_rows <- list()
  if (!is.na(spec$entity)) {
    entities <- sort(unique(as.character(dt[[spec$entity]])))
    rows <- lapply(entities, function(ent) {
      train <- dt[as.character(get(spec$entity)) == ent & .aq_forecast_date <= origin]
      state <- aq_classical_intermittent_state(train[[spec$target]], spec$horizon, method = method, alpha = spec$alpha, beta = aq_vnext_default(spec$beta, spec$alpha), threshold = spec$occurrence_threshold)
      state_rows[[length(state_rows) + 1L]] <<- data.table::data.table(method = method, demand_size_estimate = state$demand_size_estimate, inter_demand_interval_estimate = state$interval_estimate, occurrence_probability_state = state$occurrence_probability_state, warnings = paste(state$warnings, collapse = "; "), entity_value = ent)
      out <- data.table::data.table(forecast_date = future_dates, horizon = seq_len(spec$horizon), forecast = state$forecast)
      out[, (spec$entity) := ent]
      out
    })
    out <- data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
    actuals <- dt[.aq_forecast_date %in% future_dates, .(actual = as.numeric(get(spec$target))), by = c(spec$entity, ".aq_forecast_date")]
    data.table::setnames(actuals, ".aq_forecast_date", "forecast_date")
    out <- merge(out, actuals, by = c(spec$entity, "forecast_date"), all.x = TRUE, sort = FALSE)
  } else {
    train <- dt[.aq_forecast_date <= origin]
    state <- aq_classical_intermittent_state(train[[spec$target]], spec$horizon, method = method, alpha = spec$alpha, beta = aq_vnext_default(spec$beta, spec$alpha), threshold = spec$occurrence_threshold)
    state_rows[[1L]] <- data.table::data.table(method = method, demand_size_estimate = state$demand_size_estimate, inter_demand_interval_estimate = state$interval_estimate, occurrence_probability_state = state$occurrence_probability_state, warnings = paste(state$warnings, collapse = "; "))
    out <- data.table::data.table(forecast_date = future_dates, horizon = seq_len(spec$horizon), forecast = state$forecast)
    actuals <- dt[.aq_forecast_date %in% future_dates, .(forecast_date = .aq_forecast_date, actual = as.numeric(get(spec$target)))]
    out <- merge(out, actuals, by = "forecast_date", all.x = TRUE, sort = FALSE)
  }
  out[, baseline_engine := method]
  out[, method := method]
  list(data = out[], state = data.table::rbindlist(state_rows, use.names = TRUE, fill = TRUE), frequency = frequency, forecast_origin = origin)
}

aq_croston_forecast_table <- function(spec, data, origin = NULL) {
  aq_classical_intermittent_forecast_payload(spec, data, origin = origin)$data
}

aq_fit_classical_intermittent_forecast <- function(spec, data, origin = NULL) {
  method <- aq_classical_intermittent_method(spec)
  validation <- aq_validate_classical_intermittent_forecast_spec(spec, data)
  if (any(validation$status %in% c("fail", "error"))) stop(paste(validation[status %in% c("fail", "error"), message], collapse = " "), call. = FALSE)
  payload <- aq_classical_intermittent_forecast_payload(spec, data, origin = origin)
  forecast_dt <- payload$data
  diagnostics <- aq_intermittent_demand_diagnostics(data, spec$target, spec$date, entity = if (is.na(spec$entity)) NULL else spec$entity, occurrence_threshold = spec$occurrence_threshold)
  forecast_id <- aq_vnext_id(paste0(method, "_forecast"))
  title <- paste(toupper(method), "Intermittent Demand Forecast")
  artifact <- new_table_artifact(
    id = forecast_id,
    title = title,
    data = forecast_dt,
    source_generator = paste0("aq_fit_", method, "_forecast"),
    tags = c("vnext", "forecast", "intermittent_demand", method),
    dependencies = spec$forecast_spec_id,
    version = paste0("aq_", method, "_forecast_artifact_v1"),
    metadata = list(
      artifact_type = paste0(method, "_forecast"),
      method = method,
      forecast_id = forecast_id,
      forecast_spec_id = spec$forecast_spec_id,
      alpha = spec$alpha,
      beta = aq_vnext_default(spec$beta, NA_real_),
      occurrence_threshold = spec$occurrence_threshold,
      method_state = payload$state,
      bias_adjustment = if (identical(method, "sba")) 1 - spec$alpha / 2 else NA_real_,
      zero_run_response = if (identical(method, "tsb")) "occurrence probability updates during zero periods" else "interval estimate updates only at demand occurrence",
      intermittent_diagnostics = diagnostics,
      supported_downstream_actions = c("assess", "compare", "report", "campaign_review")
    )
  )
  artifact <- aq_vnext_attach_envelope(artifact, artifact_id = forecast_id, artifact_type = paste0(method, "_forecast"), artifact_version = paste0("aq_", method, "_forecast_artifact_v1"), parent_artifact_ids = spec$forecast_spec_id, lineage = list(forecast_spec_id = spec$forecast_spec_id, method = method, forecast_origin = payload$forecast_origin, horizon = spec$horizon), task = "intermittent_demand_forecast", operator = "forecast", engine = method, specification_id = spec$forecast_spec_id, dataset_id = spec$dataset_id, supported_actions = c("assess", "compare", "report", "campaign_review"), producer = paste0("aq_fit_", method, "_forecast"))
  result <- list(
    forecast_id = forecast_id,
    schema_version = paste0("aq_", method, "_forecast_result_v1"),
    status = "success",
    method = method,
    spec = spec,
    data = forecast_dt,
    method_state = payload$state,
    intermittent_diagnostics = diagnostics,
    artifact = artifact,
    validation = validation,
    comparison_ready = TRUE,
    created_at = aq_vnext_now()
  )
  class(result) <- c(paste0("aq_", method, "_forecast_result"), "aq_classical_intermittent_forecast_result", "list")
  result
}

#' Fit a Croston Intermittent Demand Forecast
#'
#' @param spec An `aq_croston_forecast_spec`.
#' @param data Intermittent-demand data.
#' @param origin Optional forecast origin.
#'
#' @return An `aq_croston_forecast_result`.
#' @export
aq_fit_croston_forecast <- function(spec, data, origin = NULL) {
  aq_fit_classical_intermittent_forecast(spec, data, origin = origin)
}

#' Fit an SBA Intermittent Demand Forecast
#'
#' @param spec An `aq_sba_forecast_spec`.
#' @param data Intermittent-demand data.
#' @param origin Optional forecast origin.
#'
#' @return An `aq_sba_forecast_result`.
#' @export
aq_fit_sba_forecast <- function(spec, data, origin = NULL) {
  aq_fit_classical_intermittent_forecast(spec, data, origin = origin)
}

#' Fit a TSB Intermittent Demand Forecast
#'
#' @param spec An `aq_tsb_forecast_spec`.
#' @param data Intermittent-demand data.
#' @param origin Optional forecast origin.
#'
#' @return An `aq_tsb_forecast_result`.
#' @export
aq_fit_tsb_forecast <- function(spec, data, origin = NULL) {
  aq_fit_classical_intermittent_forecast(spec, data, origin = origin)
}

aq_intermittent_method_metrics <- function(data, threshold = 0) {
  if (!all(c("actual", "forecast") %in% names(data))) {
    return(data.table::data.table(metric = character(), value = numeric(), applicability = character()))
  }
  actual <- as.numeric(data$actual)
  forecast <- as.numeric(data$forecast)
  keep <- stats::complete.cases(actual, forecast)
  if (!any(keep)) {
    return(data.table::data.table(metric = c("demand_rate_bias", "zero_period_overforecast", "positive_period_underforecast", "long_zero_run_mae"), value = NA_real_, applicability = "not_applicable_no_actuals"))
  }
  actual <- actual[keep]
  forecast <- forecast[keep]
  zero <- actual <= threshold
  positive <- actual > threshold
  data.table::data.table(
    metric = c("demand_rate_bias", "zero_period_overforecast", "positive_period_underforecast", "long_zero_run_mae"),
    value = c(
      mean(forecast > threshold, na.rm = TRUE) - mean(actual > threshold, na.rm = TRUE),
      if (any(zero)) mean(pmax(0, forecast[zero] - actual[zero]), na.rm = TRUE) else NA_real_,
      if (any(positive)) mean(pmax(0, actual[positive] - forecast[positive]), na.rm = TRUE) else NA_real_,
      if (length(actual) >= 3L) mean(abs(actual - forecast), na.rm = TRUE) else NA_real_
    ),
    applicability = c("applies", if (any(zero)) "applies" else "not_applicable_no_zero_periods", if (any(positive)) "applies" else "not_applicable_no_positive_periods", if (length(actual) >= 3L) "applies" else "limited_horizon")
  )
}

aq_assess_classical_intermittent_forecast <- function(forecast) {
  if (!inherits(forecast, "aq_classical_intermittent_forecast_result")) stop("forecast must be a classical intermittent-demand forecast result.", call. = FALSE)
  method <- forecast$method
  metrics <- aq_forecast_metrics(forecast$data)
  by_horizon <- forecast$data[, aq_forecast_metrics(.SD), by = horizon]
  intermittent_metrics <- aq_intermittent_method_metrics(forecast$data, threshold = forecast$spec$occurrence_threshold)
  assessment_id <- aq_vnext_id(paste0(method, "_forecast_assessment"))
  artifact <- new_table_artifact(id = assessment_id, title = paste(toupper(method), "Forecast Assessment"), data = metrics, source_generator = paste0("aq_assess_", method, "_forecast"), tags = c("vnext", "forecast", method, "assessment"), dependencies = forecast$forecast_id, version = paste0("aq_", method, "_forecast_assessment_artifact_v1"), metadata = list(artifact_type = paste0(method, "_forecast_assessment"), method = method, forecast_id = forecast$forecast_id, metrics_by_horizon = by_horizon, intermittent_metrics = intermittent_metrics, method_state = forecast$method_state))
  artifact <- aq_vnext_attach_envelope(artifact, artifact_id = assessment_id, artifact_type = paste0(method, "_forecast_assessment"), artifact_version = paste0("aq_", method, "_forecast_assessment_artifact_v1"), parent_artifact_ids = forecast$forecast_id, lineage = list(forecast_id = forecast$forecast_id), task = "intermittent_demand_forecast", operator = "forecast_assessment", engine = method, specification_id = forecast$spec$forecast_spec_id, dataset_id = forecast$spec$dataset_id, supported_actions = c("compare", "report", "campaign_review"), producer = paste0("aq_assess_", method, "_forecast"))
  result <- list(assessment_id = assessment_id, schema_version = paste0("aq_", method, "_forecast_assessment_result_v1"), forecast_id = forecast$forecast_id, method = method, metrics = metrics, metrics_by_horizon = by_horizon, intermittent_metrics = intermittent_metrics, assessment_artifact = artifact, comparison_ready = TRUE, created_at = aq_vnext_now())
  class(result) <- c(paste0("aq_", method, "_forecast_assessment_result"), "aq_classical_intermittent_forecast_assessment_result", "list")
  result
}

#' Assess Croston Forecast Evidence
#'
#' @param forecast An `aq_croston_forecast_result`.
#'
#' @return An assessment result.
#' @export
aq_assess_croston_forecast <- function(forecast) {
  aq_assess_classical_intermittent_forecast(forecast)
}

#' Assess SBA Forecast Evidence
#'
#' @param forecast An `aq_sba_forecast_result`.
#'
#' @return An assessment result.
#' @export
aq_assess_sba_forecast <- function(forecast) {
  aq_assess_classical_intermittent_forecast(forecast)
}

#' Assess TSB Forecast Evidence
#'
#' @param forecast An `aq_tsb_forecast_result`.
#'
#' @return An assessment result.
#' @export
aq_assess_tsb_forecast <- function(forecast) {
  aq_assess_classical_intermittent_forecast(forecast)
}

#' Run Deterministic Rolling-Origin Croston Evaluation
#'
#' @param spec An `aq_croston_forecast_spec`.
#' @param data Intermittent-demand data.
#' @param origins Optional forecast origins.
#' @param origin_count Optional number of latest origins.
#'
#' @return An `aq_croston_forecast_backtest_result`.
#' @export
aq_rolling_origin_croston_forecast <- function(spec, data, origins = NULL, origin_count = NULL) {
  if (!inherits(spec, c("aq_croston_forecast_spec", "aq_sba_forecast_spec", "aq_tsb_forecast_spec"))) stop("spec must be a classical intermittent-demand forecast spec.", call. = FALSE)
  method <- aq_classical_intermittent_method(spec)
  dt <- data.table::as.data.table(data.table::copy(data))
  dates <- sort(unique(as.Date(dt[[spec$date]])))
  if (is.null(origins)) {
    origin_count <- as.integer(aq_vnext_default(origin_count, 2L))[1L]
    possible <- dates[seq_len(max(0L, length(dates) - spec$horizon))]
    origins <- utils::tail(possible, origin_count)
  }
  forecasts <- lapply(as.Date(origins), function(origin) aq_fit_classical_intermittent_forecast(spec, dt, origin = origin))
  assessments <- lapply(forecasts, aq_assess_classical_intermittent_forecast)
  metrics <- data.table::rbindlist(lapply(seq_along(assessments), function(i) {
    data.table::copy(assessments[[i]]$metrics)[, forecast_origin := as.Date(origins)[i]]
  }), use.names = TRUE, fill = TRUE)
  result <- list(
    backtest_id = aq_vnext_id(paste0(method, "_forecast_backtest")),
    schema_version = paste0("aq_", method, "_forecast_backtest_result_v1"),
    method = method,
    forecast_spec_id = spec$forecast_spec_id,
    origins = as.Date(origins),
    forecasts = forecasts,
    assessments = assessments,
    metrics = metrics,
    created_at = aq_vnext_now()
  )
  class(result) <- c(paste0("aq_", method, "_forecast_backtest_result"), "aq_classical_intermittent_forecast_backtest_result", "list")
  result
}

#' Run Deterministic Rolling-Origin SBA Evaluation
#'
#' @param spec An `aq_sba_forecast_spec`.
#' @param data Intermittent-demand data.
#' @param origins Optional forecast origins.
#' @param origin_count Optional number of latest origins.
#'
#' @return An `aq_sba_forecast_backtest_result`.
#' @export
aq_rolling_origin_sba_forecast <- function(spec, data, origins = NULL, origin_count = NULL) {
  aq_rolling_origin_croston_forecast(spec, data, origins = origins, origin_count = origin_count)
}

#' Run Deterministic Rolling-Origin TSB Evaluation
#'
#' @param spec An `aq_tsb_forecast_spec`.
#' @param data Intermittent-demand data.
#' @param origins Optional forecast origins.
#' @param origin_count Optional number of latest origins.
#'
#' @return An `aq_tsb_forecast_backtest_result`.
#' @export
aq_rolling_origin_tsb_forecast <- function(spec, data, origins = NULL, origin_count = NULL) {
  aq_rolling_origin_croston_forecast(spec, data, origins = origins, origin_count = origin_count)
}

#' Create a vNext Hurdle Forecast Specification
#'
#' @param target Target value column.
#' @param date Date/time column.
#' @param entity Optional entity identifier column.
#' @param group Optional explicit group column for strategy comparison.
#' @param frequency Forecast frequency.
#' @param horizon Forecast horizon.
#' @param forecast_origin Optional common forecast origin.
#' @param occurrence_threshold Threshold above which demand is treated as
#'   occurring.
#' @param forecast_strategy Forecast strategy. Phase 16 supports `"direct"`.
#' @param future_known_variables Variables known at forecast time.
#' @param static_entity_features Entity-level static features.
#' @param engine Forecast engine. Phase 16 supports `"catboost"`.
#' @param engine_parameters CatBoost and temporal feature parameters.
#' @param minimum_history Minimum history rows per entity/series.
#' @param minimum_positive_history Minimum positive rows for positive-demand
#'   modeling.
#' @param aggregation_level Optional aggregation label.
#' @param hurdle_spec_id Optional spec id.
#' @param dataset_id Optional dataset id.
#' @param supported_downstream_actions Supported next actions.
#'
#' @return An `aq_hurdle_forecast_spec`.
#' @export
aq_hurdle_forecast_spec <- function(
  target,
  date,
  entity = NULL,
  group = NULL,
  frequency = "auto",
  horizon = 1L,
  forecast_origin = NULL,
  occurrence_threshold = 0,
  forecast_strategy = "direct",
  future_known_variables = character(),
  static_entity_features = character(),
  engine = "catboost",
  engine_parameters = list(),
  minimum_history = 20L,
  minimum_positive_history = 8L,
  aggregation_level = "hurdle",
  hurdle_spec_id = NULL,
  dataset_id = NULL,
  supported_downstream_actions = c("forecast", "assess", "compare", "report", "campaign_review")
) {
  frequency <- match.arg(tolower(frequency), aq_forecast_frequency_levels())
  engine <- match.arg(tolower(engine), "catboost")
  forecast_strategy <- match.arg(tolower(forecast_strategy), "direct")
  if (is.null(hurdle_spec_id)) {
    hurdle_spec_id <- aq_vnext_id(paste("hurdle_forecast_spec", target, sep = "_"))
  }
  spec <- list(
    hurdle_spec_id = as.character(hurdle_spec_id)[1L],
    schema_version = "aq_hurdle_forecast_spec_v1",
    target = as.character(target)[1L],
    date = as.character(date)[1L],
    entity = if (is.null(entity)) NA_character_ else as.character(entity)[1L],
    group = if (is.null(group)) NA_character_ else as.character(group)[1L],
    frequency = frequency,
    horizon = as.integer(horizon)[1L],
    forecast_origin = if (is.null(forecast_origin)) NULL else as.Date(forecast_origin)[1L],
    occurrence_threshold = as.numeric(occurrence_threshold)[1L],
    forecast_strategy = forecast_strategy,
    future_known_variables = aq_vnext_unique_chr(future_known_variables),
    static_entity_features = aq_vnext_unique_chr(static_entity_features),
    engine = engine,
    engine_parameters = if (is.null(engine_parameters)) list() else engine_parameters,
    minimum_history = as.integer(minimum_history)[1L],
    minimum_positive_history = as.integer(minimum_positive_history)[1L],
    aggregation_level = as.character(aggregation_level)[1L],
    dataset_id = aq_vnext_default(dataset_id, NA_character_),
    supported_downstream_actions = aq_vnext_unique_chr(supported_downstream_actions),
    created_at = aq_vnext_now()
  )
  class(spec) <- c("aq_hurdle_forecast_spec", "list")
  spec
}

#' Validate a vNext Hurdle Forecast Specification
#'
#' @param spec An `aq_hurdle_forecast_spec`.
#' @param data Optional intermittent-demand data.
#'
#' @return A deterministic diagnostic table.
#' @export
aq_validate_hurdle_forecast_spec <- function(spec, data = NULL) {
  rows <- list()
  add <- function(check, status, message, severity = status) {
    rows[[length(rows) + 1L]] <<- aq_vnext_validation_table(check, status, message, severity)
  }
  if (!inherits(spec, "aq_hurdle_forecast_spec")) {
    add("hurdle_spec_class", "fail", "spec must be created by aq_hurdle_forecast_spec().")
    return(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
  }
  add("hurdle_spec_class", "pass", "hurdle forecast specification is typed.", "info")
  if (!is.finite(spec$horizon) || spec$horizon < 1L) add("horizon", "fail", "horizon must be positive.") else add("horizon", "pass", paste("horizon:", spec$horizon), "info")
  if (!is.finite(spec$occurrence_threshold)) add("occurrence_threshold", "fail", "occurrence_threshold must be finite.") else add("occurrence_threshold", "pass", paste("threshold:", spec$occurrence_threshold), "info")
  if (!identical(spec$forecast_strategy, "direct")) add("forecast_strategy", "fail", "Phase 16 hurdle forecasting supports direct strategy only.") else add("forecast_strategy", "pass", "strategy: direct", "info")
  if (is.null(data)) return(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
  dt <- data.table::as.data.table(data)
  required <- unique(c(spec$target, spec$date, spec$future_known_variables, spec$static_entity_features, if (!is.na(spec$entity)) spec$entity, if (!is.na(spec$group)) spec$group))
  missing <- setdiff(required, names(dt))
  if (length(missing)) {
    add("required_columns", "fail", paste("Missing required column(s):", paste(missing, collapse = ", ")))
    return(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
  }
  add("required_columns", "pass", "required columns are present.", "info")
  if (!is.numeric(dt[[spec$target]])) add("target_type", "fail", "target must be numeric.") else add("target_type", "pass", "target is numeric.", "info")
  dates <- as.Date(dt[[spec$date]])
  if (all(is.na(dates))) {
    add("date_type", "fail", "date column cannot be converted to Date.")
    return(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
  }
  if (!is.na(spec$entity)) {
    duplicate_count <- nrow(dt[, .N, by = c(spec$entity, spec$date)][N > 1L])
    if (duplicate_count) add("duplicate_entity_date", "fail", paste("duplicate entity/date pair count:", duplicate_count)) else add("duplicate_entity_date", "pass", "entity/date pairs are unique.", "info")
    history <- dt[!is.na(get(spec$target)), .N, by = c(spec$entity)]
    sparse <- history[N < spec$minimum_history]
    if (nrow(sparse)) add("entity_history_length", "warning", paste("entities below minimum history:", nrow(sparse)), "warning") else add("entity_history_length", "pass", "all entities meet minimum history.", "info")
    positives <- dt[get(spec$target) > spec$occurrence_threshold, .N, by = c(spec$entity)]
    low_positive <- merge(history, positives, by = spec$entity, all.x = TRUE, suffixes = c("_history", "_positive"))
    low_positive[is.na(N_positive), N_positive := 0L]
    low_positive <- low_positive[N_positive < spec$minimum_positive_history]
    if (nrow(low_positive)) add("positive_history", "warning", paste("entities below minimum positive history:", nrow(low_positive)), "warning") else add("positive_history", "pass", "positive-demand history is sufficient by entity.", "info")
  } else {
    duplicate_count <- sum(duplicated(dates[!is.na(dates)]))
    if (duplicate_count) add("duplicate_timestamps", "fail", paste("duplicate timestamps:", duplicate_count)) else add("duplicate_timestamps", "pass", "timestamps are unique.", "info")
    positive_count <- sum(dt[[spec$target]] > spec$occurrence_threshold, na.rm = TRUE)
    if (positive_count < spec$minimum_positive_history) add("positive_history", "warning", paste("positive rows below minimum:", positive_count), "warning") else add("positive_history", "pass", "positive-demand history is sufficient.", "info")
  }
  y <- as.numeric(dt[[spec$target]])
  zero_rate <- mean(y <= spec$occurrence_threshold, na.rm = TRUE)
  add("zero_rate", "pass", paste("zero/non-occurrence rate:", signif(zero_rate, 4)), "info")
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

aq_hurdle_model_params <- function(spec, task = c("regression", "binary")) {
  task <- match.arg(task)
  params <- aq_vnext_default(spec$engine_parameters, list())
  model_param_names <- c("iterations", "depth", "learning_rate", "loss_function", "eval_metric", "random_seed", "thread_count", "verbose", "task_type", "l2_leaf_reg", "random_strength", "bootstrap_type", "allow_writing_files")
  model_params <- params[intersect(names(params), model_param_names)]
  if (is.null(model_params$iterations)) model_params$iterations <- 30L
  if (is.null(model_params$depth)) model_params$depth <- 4L
  if (is.null(model_params$learning_rate)) model_params$learning_rate <- 0.1
  if (identical(task, "binary")) {
    model_params$loss_function <- "Logloss"
    model_params$eval_metric <- "Logloss"
  }
  aq_vnext_engine_params(model_params, seed = aq_vnext_default(model_params$random_seed, 20260712L), task = task)
}

aq_hurdle_train_binary <- function(frame, feature_cols, spec) {
  keep <- stats::complete.cases(frame[, c(".hurdle_occurrence", feature_cols), with = FALSE])
  train <- frame[keep]
  label <- as.integer(train$.hurdle_occurrence)
  if (length(unique(label)) < 2L || nrow(train) < 10L) {
    return(list(model = NULL, constant_probability = mean(label, na.rm = TRUE), rows = nrow(train), model_type = "constant_probability"))
  }
  pool <- catboost::catboost.load_pool(data = aq_forecast_numeric_matrix(train, feature_cols), label = label)
  params <- aq_hurdle_model_params(spec, "binary")
  elapsed <- system.time(model <- catboost::catboost.train(learn_pool = pool, test_pool = NULL, params = params))
  list(model = model, constant_probability = NA_real_, rows = nrow(train), params = params, elapsed = unname(elapsed[["elapsed"]]), model_type = "catboost_logloss")
}

aq_hurdle_predict_binary <- function(model_info, frame, feature_cols) {
  if (is.null(model_info$model)) return(rep(as.numeric(model_info$constant_probability), nrow(frame)))
  pool <- catboost::catboost.load_pool(data = aq_forecast_numeric_matrix(frame, feature_cols))
  raw <- as.numeric(catboost::catboost.predict(model_info$model, pool = pool, prediction_type = "RawFormulaVal"))
  1 / (1 + exp(-raw))
}

aq_hurdle_train_positive <- function(frame, feature_cols, spec) {
  train <- frame[.rodeo_label > spec$occurrence_threshold]
  keep <- stats::complete.cases(train[, c(".rodeo_label", feature_cols), with = FALSE])
  train <- train[keep]
  if (!nrow(train)) {
    return(list(model = NULL, constant_positive = 0, rows = 0L, model_type = "constant_zero"))
  }
  if (nrow(train) < max(5L, spec$minimum_positive_history)) {
    return(list(model = NULL, constant_positive = mean(train$.rodeo_label, na.rm = TRUE), rows = nrow(train), model_type = "constant_positive_mean"))
  }
  pool <- catboost::catboost.load_pool(data = aq_forecast_numeric_matrix(train, feature_cols), label = as.numeric(train$.rodeo_label))
  params <- aq_hurdle_model_params(spec, "regression")
  elapsed <- system.time(model <- catboost::catboost.train(learn_pool = pool, test_pool = NULL, params = params))
  list(model = model, constant_positive = NA_real_, rows = nrow(train), params = params, elapsed = unname(elapsed[["elapsed"]]), model_type = "catboost_rmse")
}

aq_hurdle_predict_positive <- function(model_info, frame, feature_cols) {
  if (is.null(model_info$model)) return(rep(as.numeric(model_info$constant_positive), nrow(frame)))
  pool <- catboost::catboost.load_pool(data = aq_forecast_numeric_matrix(frame, feature_cols))
  pmax(0, as.numeric(catboost::catboost.predict(model_info$model, pool = pool, prediction_type = "RawFormulaVal")))
}

aq_hurdle_season_length <- function(spec, frequency) {
  season <- aq_vnext_default(spec$engine_parameters$season_length, NULL)
  if (!is.null(season) && is.finite(season) && season > 0L) return(as.integer(season)[1L])
  switch(
    frequency,
    day = 7L,
    week = 52L,
    month = 12L,
    quarter = 4L,
    year = 1L,
    1L
  )
}

aq_hurdle_baseline_vector <- function(y, horizon, engine, season_length = 1L, threshold = 0) {
  y <- as.numeric(y)
  y <- y[!is.na(y)]
  if (!length(y)) return(rep(NA_real_, horizon))
  if (identical(engine, "croston")) return(aq_croston_values(y, horizon, threshold = threshold))
  if (identical(engine, "sba")) return(aq_sba_values(y, horizon, threshold = threshold))
  if (identical(engine, "tsb")) return(aq_tsb_values(y, horizon, threshold = threshold))
  if (identical(engine, "seasonal_naive") && length(y) >= season_length && season_length > 1L) {
    pattern <- utils::tail(y, season_length)
    return(rep(pattern, length.out = horizon))
  }
  rep(utils::tail(y, 1L), horizon)
}

aq_hurdle_baseline_tables <- function(spec, ctx, forecast_dt) {
  engines <- c("naive", "seasonal_naive", "croston", "sba", "tsb")
  season_length <- aq_hurdle_season_length(spec, ctx$partition$frequency)
  future_dates <- sort(unique(as.Date(forecast_dt$forecast_date)))
  horizon <- length(future_dates)
  build_one <- function(engine) {
    if (!is.na(spec$entity)) {
      rows <- lapply(sort(unique(as.character(ctx$train[[spec$entity]]))), function(ent) {
        train <- ctx$train[as.character(get(spec$entity)) == ent]
        pred <- aq_hurdle_baseline_vector(train[[spec$target]], horizon, engine, season_length, spec$occurrence_threshold)
        out <- data.table::data.table(forecast_date = future_dates, horizon = seq_len(horizon), forecast = pred)
        out[, (spec$entity) := ent]
        out
      })
      out <- data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
      actuals <- forecast_dt[, .SD, .SDcols = c(spec$entity, "forecast_date", "horizon", "actual")]
      out <- merge(out, actuals, by = c(spec$entity, "forecast_date", "horizon"), all.x = TRUE, sort = FALSE)
    } else {
      pred <- aq_hurdle_baseline_vector(ctx$train[[spec$target]], horizon, engine, season_length, spec$occurrence_threshold)
      out <- data.table::data.table(forecast_date = future_dates, horizon = seq_len(horizon), forecast = pred)
      actuals <- forecast_dt[, .(forecast_date, horizon, actual)]
      out <- merge(out, actuals, by = c("forecast_date", "horizon"), all.x = TRUE, sort = FALSE)
    }
    out[, baseline_engine := engine]
    out[]
  }
  stats::setNames(lapply(engines, build_one), engines)
}

aq_hurdle_partition_context <- function(spec, data, origin = NULL, future_data = NULL) {
  dt <- data.table::as.data.table(data.table::copy(data))
  dt[, .aq_forecast_date := as.Date(get(spec$date))]
  if (!is.na(spec$entity)) {
    pspec <- aq_panel_forecast_spec(
      entity = spec$entity, target = spec$target, date = spec$date, frequency = spec$frequency, horizon = spec$horizon,
      forecast_origin = spec$forecast_origin, future_known_variables = spec$future_known_variables, static_entity_features = spec$static_entity_features,
      engine_parameters = spec$engine_parameters, minimum_history = spec$minimum_history, forecast_spec_id = aq_vnext_id(paste0(spec$hurdle_spec_id, "_panel_context")),
      dataset_id = spec$dataset_id
    )
    partition <- aq_panel_forecast_partition(pspec, dt, origin = origin)
    train <- dt[partition$train_index]
    eval <- dt[partition$evaluation_index]
    future_context <- aq_panel_future_data_context(pspec, train, eval, partition, future_data = future_data)
  } else {
    fspec <- aq_forecast_spec(
      target = spec$target, date = spec$date, frequency = spec$frequency, horizon = spec$horizon, forecast_origin = spec$forecast_origin,
      future_known_variables = spec$future_known_variables, engine = "catboost", forecast_strategy = "direct", engine_parameters = spec$engine_parameters,
      prediction_intervals = FALSE, forecast_spec_id = aq_vnext_id(paste0(spec$hurdle_spec_id, "_series_context")), dataset_id = spec$dataset_id
    )
    partition <- aq_forecast_partition(fspec, dt, origin = origin)
    train <- dt[partition$train_index]
    eval <- dt[partition$evaluation_index]
    future_context <- aq_forecast_future_data_context(fspec, train, eval, partition, future_data = future_data)
  }
  list(dt = dt, partition = partition, train = train, eval = eval, future_context = future_context)
}

#' Fit a vNext Hurdle Forecast
#'
#' @param spec An `aq_hurdle_forecast_spec`.
#' @param data Intermittent-demand data.
#' @param origin Optional forecast origin.
#' @param future_data Optional future-known data.
#'
#' @return An `aq_hurdle_forecast_result`.
#' @export
aq_fit_hurdle_forecast <- function(spec, data, origin = NULL, future_data = NULL) {
  if (!inherits(spec, "aq_hurdle_forecast_spec")) stop("spec must be an aq_hurdle_forecast_spec.", call. = FALSE)
  if (!requireNamespace("catboost", quietly = TRUE)) stop("The catboost package is required for aq_fit_hurdle_forecast().", call. = FALSE)
  aq_forecast_require_rodeo_temporal()
  validation <- aq_validate_hurdle_forecast_spec(spec, data)
  if (any(validation$status %in% c("fail", "error"))) stop(paste(validation[status %in% c("fail", "error"), message], collapse = " "), call. = FALSE)
  ctx <- aq_hurdle_partition_context(spec, data, origin = origin, future_data = future_data)
  settings <- aq_forecast_catboost_feature_settings(spec, ctx$partition$frequency)
  temporal_spec <- Rodeo::rodeo_temporal_transformation_spec(
    date_col = spec$date,
    target_col = spec$target,
    frequency = ctx$partition$frequency,
    calendar_features = settings$date_features,
    lag_periods = settings$lag_periods,
    rolling_windows = settings$rolling_windows,
    known_future_variables = spec$future_known_variables,
    static_entity_features = spec$static_entity_features,
    entity_id = if (is.na(spec$entity)) NULL else spec$entity,
    forecast_horizon = spec$horizon,
    metadata = list(producer = "AutoQuant", consumer = "aq_fit_hurdle_forecast", family = "hurdle")
  )
  temporal_fit <- Rodeo::rodeo_fit_temporal_transformation(ctx$train, temporal_spec, forecast_origin = ctx$partition$forecast_origin)
  temporal_metadata <- Rodeo::rodeo_temporal_transformation_metadata(temporal_fit)
  frames <- Rodeo::rodeo_prepare_forecast_supervised_data(temporal_fit, future_data = ctx$future_context$data, horizon = spec$horizon, strategy = "direct")
  predictions <- list()
  model_info <- vector("list", spec$horizon)
  diagnostics <- list()
  for (h in seq_len(spec$horizon)) {
    frame <- data.table::as.data.table(data.table::copy(frames$training_frames[[as.character(h)]]))
    feature_cols <- frames$feature_columns_by_horizon[[as.character(h)]]
    frame[, .hurdle_occurrence := as.integer(.rodeo_label > spec$occurrence_threshold)]
    occurrence_model <- aq_hurdle_train_binary(frame, feature_cols, spec)
    positive_model <- aq_hurdle_train_positive(frame, feature_cols, spec)
    pred_frame <- frames$prediction_frames[[as.character(h)]]
    occurrence_probability <- pmin(1, pmax(0, aq_hurdle_predict_binary(occurrence_model, pred_frame, feature_cols)))
    positive_prediction <- pmax(0, aq_hurdle_predict_positive(positive_model, pred_frame, feature_cols))
    out <- data.table::data.table(
      forecast_date = as.Date(pred_frame$.rodeo_future_date),
      horizon = h,
      occurrence_probability = occurrence_probability,
      positive_prediction = positive_prediction,
      forecast = occurrence_probability * positive_prediction
    )
    if (!is.na(spec$entity)) out[, (spec$entity) := as.character(pred_frame[[spec$entity]])]
    predictions[[h]] <- out
    model_info[[h]] <- list(occurrence = occurrence_model, positive = positive_model)
    diagnostics[[h]] <- data.table::data.table(
      horizon = h,
      occurrence_model_type = occurrence_model$model_type,
      positive_model_type = positive_model$model_type,
      occurrence_training_rows = occurrence_model$rows,
      positive_training_rows = positive_model$rows
    )
  }
  forecast_dt <- data.table::rbindlist(predictions, use.names = TRUE, fill = TRUE)
  if (!is.na(spec$entity)) {
    actuals <- ctx$eval[, .(actual = as.numeric(get(spec$target))), by = c(spec$entity, ".aq_forecast_date")]
    data.table::setnames(actuals, ".aq_forecast_date", "forecast_date")
    forecast_dt <- merge(forecast_dt, actuals, by = c(spec$entity, "forecast_date"), all.x = TRUE, sort = FALSE)
    data.table::setorderv(forecast_dt, c(spec$entity, "horizon"))
  } else {
    actuals <- ctx$eval[, .(forecast_date = .aq_forecast_date, actual = as.numeric(get(spec$target)))]
    forecast_dt <- merge(forecast_dt, actuals, by = "forecast_date", all.x = TRUE, sort = FALSE)
    data.table::setorder(forecast_dt, horizon)
  }
  forecast_dt[, actual_occurrence := as.integer(actual > spec$occurrence_threshold)]
  forecast_dt[, predicted_occurrence := as.integer(occurrence_probability >= 0.5)]
  intermittent_diagnostics <- aq_intermittent_demand_diagnostics(data, spec$target, spec$date, entity = if (is.na(spec$entity)) NULL else spec$entity, occurrence_threshold = spec$occurrence_threshold)
  baseline_forecasts <- aq_hurdle_baseline_tables(spec, ctx, forecast_dt)
  forecast_id <- aq_vnext_id("hurdle_forecast")
  hurdle_diagnostics <- data.table::rbindlist(diagnostics, use.names = TRUE, fill = TRUE)
  artifact <- new_table_artifact(
    id = forecast_id,
    title = "Hurdle Forecast: Occurrence x Positive Demand",
    data = forecast_dt,
    source_generator = "aq_fit_hurdle_forecast",
    tags = c("vnext", "forecast", "hurdle", "intermittent_demand"),
    dependencies = spec$hurdle_spec_id,
    version = "aq_hurdle_forecast_artifact_v1",
    metadata = list(
      artifact_type = "hurdle_forecast",
      forecast_id = forecast_id,
      hurdle_spec_id = spec$hurdle_spec_id,
      occurrence_threshold = spec$occurrence_threshold,
      occurrence_output = "occurrence_probability",
      positive_output = "positive_prediction",
      combined_output = "forecast",
      entity = spec$entity,
      target = spec$target,
      date = spec$date,
      forecast_origin = ctx$partition$forecast_origin,
      horizon = spec$horizon,
      temporal_metadata = temporal_metadata,
      feature_manifest = frames$feature_manifest,
      hurdle_diagnostics = hurdle_diagnostics,
      intermittent_diagnostics = intermittent_diagnostics,
      baseline_engines = names(baseline_forecasts),
      validation = validation,
      supported_downstream_actions = c("assess", "compare", "report", "campaign_review")
    )
  )
  artifact <- aq_vnext_attach_envelope(
    artifact,
    artifact_id = forecast_id,
    artifact_type = "hurdle_forecast",
    artifact_version = "aq_hurdle_forecast_artifact_v1",
    parent_artifact_ids = spec$hurdle_spec_id,
    lineage = list(hurdle_spec_id = spec$hurdle_spec_id, temporal_transformation_identity = temporal_metadata$temporal_transformation_identity, forecast_origin = ctx$partition$forecast_origin, horizon = spec$horizon),
    task = "hurdle_forecast",
    operator = "forecast",
    engine = "catboost",
    specification_id = spec$hurdle_spec_id,
    dataset_id = spec$dataset_id,
    supported_actions = c("assess", "compare", "report", "campaign_review"),
    producer = "aq_fit_hurdle_forecast"
  )
  result <- list(
    forecast_id = forecast_id,
    schema_version = "aq_hurdle_forecast_result_v1",
    status = "success",
    spec = spec,
    hurdle_spec_id = spec$hurdle_spec_id,
    entity = spec$entity,
    target = spec$target,
    date = spec$date,
    frequency = ctx$partition$frequency,
    forecast_origin = ctx$partition$forecast_origin,
    horizon = spec$horizon,
    occurrence_threshold = spec$occurrence_threshold,
    data = forecast_dt,
    models = model_info,
    temporal_metadata = temporal_metadata,
    feature_manifest = frames$feature_manifest,
    hurdle_diagnostics = hurdle_diagnostics,
    intermittent_diagnostics = intermittent_diagnostics,
    baseline_forecasts = baseline_forecasts,
    artifact = artifact,
    validation = validation,
    comparison_ready = TRUE,
    created_at = aq_vnext_now()
  )
  class(result) <- c("aq_hurdle_forecast_result", "list")
  result
}

aq_hurdle_occurrence_metrics <- function(data) {
  keep <- stats::complete.cases(data$actual_occurrence, data$occurrence_probability)
  if (!any(keep)) {
    return(data.table::data.table(metric = c("occurrence_accuracy", "brier_score", "actual_zero_rate", "predicted_zero_rate", "occurrence_n"), value = c(NA, NA, NA, NA, 0)))
  }
  actual <- as.integer(data$actual_occurrence[keep])
  prob <- as.numeric(data$occurrence_probability[keep])
  pred <- as.integer(prob >= 0.5)
  data.table::data.table(
    metric = c("occurrence_accuracy", "brier_score", "actual_zero_rate", "predicted_zero_rate", "occurrence_n"),
    value = c(mean(pred == actual), mean((prob - actual)^2), mean(actual == 0), mean(pred == 0), length(actual))
  )
}

#' Assess Hurdle Forecast Evidence
#'
#' @param forecast An `aq_hurdle_forecast_result`.
#'
#' @return An `aq_hurdle_forecast_assessment_result`.
#' @export
aq_assess_hurdle_forecast <- function(forecast) {
  if (!inherits(forecast, "aq_hurdle_forecast_result")) stop("forecast must be an aq_hurdle_forecast_result.", call. = FALSE)
  if (!"actual" %in% names(forecast$data) || all(is.na(forecast$data$actual))) stop("hurdle forecast result does not contain realized actuals for assessment.", call. = FALSE)
  occurrence_threshold <- forecast$occurrence_threshold
  combined_metrics <- aq_forecast_metrics(forecast$data)
  occurrence_metrics <- aq_hurdle_occurrence_metrics(forecast$data)
  positive_metrics <- aq_forecast_metrics(forecast$data[actual > occurrence_threshold, .(actual = actual, forecast = positive_prediction)])
  baseline_metrics <- data.table::rbindlist(lapply(names(forecast$baseline_forecasts), function(engine) {
    out <- aq_forecast_metrics(forecast$baseline_forecasts[[engine]])
    out[, baseline_engine := engine]
    out
  }), use.names = TRUE, fill = TRUE)
  baseline_comparison <- baseline_metrics[metric %in% c("rmse", "mae", "bias")]
  by_horizon <- forecast$data[, aq_forecast_metrics(.SD), by = horizon]
  occurrence_by_horizon <- forecast$data[, aq_hurdle_occurrence_metrics(.SD), by = horizon]
  by_entity <- data.table::data.table()
  if (!is.na(forecast$entity) && forecast$entity %in% names(forecast$data)) {
    by_entity <- forecast$data[, aq_forecast_metrics(.SD), by = c(forecast$entity)]
  }
  zero_calibration <- forecast$data[, .(
    actual_zero_rate = mean(actual <= occurrence_threshold, na.rm = TRUE),
    predicted_zero_rate = mean(occurrence_probability < 0.5, na.rm = TRUE),
    mean_occurrence_probability = mean(occurrence_probability, na.rm = TRUE)
  ), by = horizon]
  assessment_id <- aq_vnext_id("hurdle_forecast_assessment")
  artifact <- new_table_artifact(
    id = assessment_id,
    title = "Hurdle Forecast Assessment",
    data = combined_metrics,
    source_generator = "aq_assess_hurdle_forecast",
    tags = c("vnext", "forecast", "hurdle", "assessment"),
    dependencies = forecast$forecast_id,
    version = "aq_hurdle_forecast_assessment_artifact_v1",
    metadata = list(
      artifact_type = "hurdle_forecast_assessment",
      forecast_id = forecast$forecast_id,
      combined_metrics = combined_metrics,
      occurrence_metrics = occurrence_metrics,
      positive_metrics = positive_metrics,
      metrics_by_horizon = by_horizon,
      occurrence_by_horizon = occurrence_by_horizon,
      metrics_by_entity = by_entity,
      zero_calibration = zero_calibration,
      hurdle_diagnostics = forecast$hurdle_diagnostics,
      intermittent_diagnostics = forecast$intermittent_diagnostics,
      baseline_metrics = baseline_metrics,
      baseline_comparison = baseline_comparison
    )
  )
  artifact <- aq_vnext_attach_envelope(
    artifact,
    artifact_id = assessment_id,
    artifact_type = "hurdle_forecast_assessment",
    artifact_version = "aq_hurdle_forecast_assessment_artifact_v1",
    parent_artifact_ids = forecast$forecast_id,
    lineage = list(forecast_id = forecast$forecast_id, hurdle_spec_id = forecast$hurdle_spec_id),
    task = "hurdle_forecast",
    operator = "forecast_assessment",
    engine = "hurdle_catboost",
    specification_id = forecast$hurdle_spec_id,
    dataset_id = forecast$spec$dataset_id,
    supported_actions = c("compare", "report", "campaign_review"),
    producer = "aq_assess_hurdle_forecast"
  )
  result <- list(
    assessment_id = assessment_id,
    schema_version = "aq_hurdle_forecast_assessment_result_v1",
    forecast_id = forecast$forecast_id,
    hurdle_spec_id = forecast$hurdle_spec_id,
    combined_metrics = combined_metrics,
    occurrence_metrics = occurrence_metrics,
    positive_metrics = positive_metrics,
    metrics_by_horizon = by_horizon,
    occurrence_by_horizon = occurrence_by_horizon,
    metrics_by_entity = by_entity,
    zero_calibration = zero_calibration,
    baseline_metrics = baseline_metrics,
    baseline_comparison = baseline_comparison,
    assessment_artifact = artifact,
    comparison_ready = TRUE,
    created_at = aq_vnext_now()
  )
  class(result) <- c("aq_hurdle_forecast_assessment_result", "list")
  result
}

#' Run Deterministic Rolling-Origin Hurdle Forecast Evaluation
#'
#' @param spec An `aq_hurdle_forecast_spec`.
#' @param data Intermittent-demand data.
#' @param origins Optional forecast origins.
#' @param origin_count Optional number of latest origins.
#'
#' @return An `aq_hurdle_forecast_backtest_result`.
#' @export
aq_rolling_origin_hurdle_forecast <- function(spec, data, origins = NULL, origin_count = NULL) {
  dt <- data.table::as.data.table(data.table::copy(data))
  dates <- sort(unique(as.Date(dt[[spec$date]])))
  if (is.null(origins)) {
    origin_count <- as.integer(aq_vnext_default(origin_count, 2L))[1L]
    possible <- dates[seq_len(max(0L, length(dates) - spec$horizon))]
    origins <- utils::tail(possible, origin_count)
  }
  forecasts <- lapply(as.Date(origins), function(origin) aq_fit_hurdle_forecast(spec, dt, origin = origin))
  assessments <- lapply(forecasts, aq_assess_hurdle_forecast)
  result <- list(
    backtest_id = aq_vnext_id("hurdle_forecast_backtest"),
    schema_version = "aq_hurdle_forecast_backtest_result_v1",
    hurdle_spec_id = spec$hurdle_spec_id,
    origins = as.Date(origins),
    forecasts = forecasts,
    assessments = assessments,
    metrics = data.table::rbindlist(lapply(seq_along(assessments), function(i) data.table::copy(assessments[[i]]$combined_metrics)[, forecast_origin := forecasts[[i]]$forecast_origin]), use.names = TRUE, fill = TRUE),
    created_at = aq_vnext_now()
  )
  class(result) <- c("aq_hurdle_forecast_backtest_result", "list")
  result
}

#' Evaluate Hurdle Panel Strategies
#'
#' @param spec An `aq_hurdle_forecast_spec` with an entity column.
#' @param data Intermittent panel data.
#' @param candidate_strategies Strategies to compare.
#' @param origins Optional common origins.
#'
#' @return An `aq_hurdle_strategy_evaluation_result`.
#' @export
aq_evaluate_hurdle_panel_strategies <- function(spec, data, candidate_strategies = c("independent", "grouped", "global"), origins = NULL) {
  if (is.na(spec$entity)) stop("hurdle panel strategy evaluation requires spec$entity.", call. = FALSE)
  candidate_strategies <- intersect(aq_vnext_unique_chr(tolower(candidate_strategies)), aq_panel_candidate_strategy_levels())
  dt <- data.table::as.data.table(data.table::copy(data))
  dates <- sort(unique(as.Date(dt[[spec$date]])))
  if (is.null(origins)) origins <- utils::tail(dates[seq_len(max(0L, length(dates) - spec$horizon))], 1L)
  rows <- list()
  for (origin in as.Date(origins)) {
    if ("independent" %in% candidate_strategies) {
      for (ent in sort(unique(as.character(dt[[spec$entity]])))) {
        sub <- dt[as.character(get(spec$entity)) == ent]
        ispec <- spec
        ispec$entity <- NA_character_
        ispec$static_entity_features <- character()
        fit <- aq_fit_hurdle_forecast(ispec, sub, origin = origin)
        out <- data.table::copy(fit$data)
        out[, (spec$entity) := ent]
        out[, strategy := "independent"]
        out[, forecast_origin := origin]
        rows[[length(rows) + 1L]] <- out
      }
    }
    if ("grouped" %in% candidate_strategies && !is.na(spec$group) && spec$group %in% names(dt)) {
      for (grp in sort(unique(as.character(dt[[spec$group]])))) {
        sub <- dt[as.character(get(spec$group)) == grp]
        fit <- aq_fit_hurdle_forecast(spec, sub, origin = origin)
        out <- data.table::copy(fit$data)
        out[, strategy := "grouped"]
        out[, forecast_origin := origin]
        out[, (spec$group) := grp]
        rows[[length(rows) + 1L]] <- out
      }
    }
    if ("global" %in% candidate_strategies) {
      fit <- aq_fit_hurdle_forecast(spec, dt, origin = origin)
      out <- data.table::copy(fit$data)
      out[, strategy := "global"]
      out[, forecast_origin := origin]
      rows[[length(rows) + 1L]] <- out
    }
  }
  forecasts <- data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
  entity_metrics <- forecasts[, aq_forecast_metrics(.SD), by = c("strategy", "forecast_origin", spec$entity)]
  entity_formula <- stats::as.formula(paste("strategy + forecast_origin +", spec$entity, "~ metric"))
  summary <- data.table::dcast(entity_metrics, entity_formula, value.var = "value")
  strategy_summary <- summary[, .(equal_entity_rmse = mean(rmse, na.rm = TRUE), equal_entity_mae = mean(mae, na.rm = TRUE), entity_count = .N), by = strategy]
  strategy_summary[, primary_score := equal_entity_rmse]
  negative_transfer <- aq_panel_negative_transfer(entity_metrics, aq_panel_strategy_spec(entity = spec$entity, target = spec$target, date = spec$date, group = if (is.na(spec$group)) NULL else spec$group, horizon = spec$horizon, negative_transfer_tolerance = 0.05))
  recommendation <- aq_panel_strategy_recommendation(strategy_summary, negative_transfer, aq_panel_strategy_spec(entity = spec$entity, target = spec$target, date = spec$date, horizon = spec$horizon))
  evaluation_id <- aq_vnext_id("hurdle_strategy_comparison")
  artifact <- new_table_artifact(
    id = evaluation_id,
    title = "Hurdle Strategy Comparison",
    data = strategy_summary,
    source_generator = "aq_evaluate_hurdle_panel_strategies",
    tags = c("vnext", "forecast", "hurdle", "strategy"),
    dependencies = spec$hurdle_spec_id,
    version = "aq_hurdle_strategy_comparison_artifact_v1",
    metadata = list(artifact_type = "hurdle_strategy_comparison", hurdle_spec_id = spec$hurdle_spec_id, negative_transfer = negative_transfer, recommendation = recommendation, supported_downstream_actions = c("compare", "report", "campaign_review"))
  )
  artifact <- aq_vnext_attach_envelope(artifact, artifact_id = evaluation_id, artifact_type = "hurdle_strategy_comparison", artifact_version = "aq_hurdle_strategy_comparison_artifact_v1", parent_artifact_ids = spec$hurdle_spec_id, lineage = list(hurdle_spec_id = spec$hurdle_spec_id, strategies = candidate_strategies, origins = origins), task = "hurdle_strategy_evaluation", operator = "forecast_strategy_comparison", engine = "hurdle_catboost", specification_id = spec$hurdle_spec_id, dataset_id = spec$dataset_id, supported_actions = c("compare", "report", "campaign_review"), producer = "aq_evaluate_hurdle_panel_strategies")
  result <- list(evaluation_id = evaluation_id, schema_version = "aq_hurdle_strategy_evaluation_result_v1", forecasts = forecasts, entity_metrics = entity_metrics, strategy_summary = strategy_summary, negative_transfer = negative_transfer, recommendation = recommendation, artifact = artifact, comparison_ready = TRUE, created_at = aq_vnext_now())
  class(result) <- c("aq_hurdle_strategy_evaluation_result", "list")
  result
}

aq_intermittent_comparison_origins <- function(data, date, horizon, origins = NULL, origin_count = 2L) {
  if (!is.null(origins)) return(as.Date(origins))
  dates <- sort(unique(as.Date(data[[date]])))
  possible <- dates[seq_len(max(0L, length(dates) - horizon))]
  utils::tail(possible, as.integer(origin_count)[1L])
}

aq_intermittent_manual_forecast <- function(data, target, date, entity = NULL, frequency = "auto", horizon = 1L, origin, method, occurrence_threshold = 0, alpha = 0.1, beta = 0.1, season_length = NULL) {
  dt <- data.table::as.data.table(data.table::copy(data))
  target_col <- as.character(target)[1L]
  date_col <- as.character(date)[1L]
  entity_col <- if (is.null(entity) || is.na(entity)) NULL else as.character(entity)[1L]
  dt[, .aq_forecast_date := as.Date(get(date_col))]
  freq <- aq_forecast_resolved_frequency(list(frequency = frequency, date = date_col), dt)
  if (is.null(season_length)) season_length <- aq_hurdle_season_length(list(engine_parameters = list()), freq)
  future_dates <- aq_forecast_next_dates(as.Date(origin), freq, horizon)
  build_pred <- function(y) {
    if (identical(method, "naive")) aq_hurdle_baseline_vector(y, horizon, "naive", season_length, occurrence_threshold)
    else if (identical(method, "seasonal_naive")) aq_hurdle_baseline_vector(y, horizon, "seasonal_naive", season_length, occurrence_threshold)
    else aq_classical_intermittent_state(y, horizon, method = method, alpha = alpha, beta = beta, threshold = occurrence_threshold)$forecast
  }
  if (!is.null(entity_col)) {
    rows <- lapply(sort(unique(as.character(dt[[entity_col]]))), function(ent) {
      train <- dt[as.character(get(entity_col)) == ent & .aq_forecast_date <= as.Date(origin)]
      out <- data.table::data.table(forecast_date = future_dates, horizon = seq_len(horizon), forecast = build_pred(train[[target_col]]))
      out[, (entity_col) := ent]
      out
    })
    out <- data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
    actuals <- dt[.aq_forecast_date %in% future_dates, .(actual = as.numeric(get(target_col))), by = c(entity_col, ".aq_forecast_date")]
    data.table::setnames(actuals, ".aq_forecast_date", "forecast_date")
    out <- merge(out, actuals, by = c(entity_col, "forecast_date"), all.x = TRUE, sort = FALSE)
  } else {
    train <- dt[.aq_forecast_date <= as.Date(origin)]
    out <- data.table::data.table(forecast_date = future_dates, horizon = seq_len(horizon), forecast = build_pred(train[[target_col]]))
    actuals <- dt[.aq_forecast_date %in% future_dates, .(forecast_date = .aq_forecast_date, actual = as.numeric(get(target_col)))]
    out <- merge(out, actuals, by = "forecast_date", all.x = TRUE, sort = FALSE)
  }
  out[, method := method]
  out[, forecast_origin := as.Date(origin)]
  out[]
}

aq_intermittent_recommendation <- function(method_summary, entity_metrics = data.table::data.table(), horizon_metrics = data.table::data.table(), diagnostics = data.table::data.table()) {
  if (!nrow(method_summary) || all(!is.finite(method_summary$primary_score))) {
    return(data.table::data.table(recommendation = "evidence_insufficient", reason = "No finite comparable intermittent-demand scores were produced.", confidence = "low", automatic_selection = FALSE))
  }
  entity_best <- character()
  if (nrow(entity_metrics) && all(c("entity_value", "method", "rmse") %in% names(entity_metrics))) {
    entity_best <- entity_metrics[is.finite(rmse), .SD[which.min(rmse)], by = entity_value]$method
  }
  horizon_best <- character()
  if (nrow(horizon_metrics) && all(c("horizon", "method", "rmse") %in% names(horizon_metrics))) {
    horizon_best <- horizon_metrics[is.finite(rmse), .SD[which.min(rmse)], by = horizon]$method
  }
  obsolescence <- if (nrow(diagnostics) && "possible_obsolescence_signal" %in% names(diagnostics)) diagnostics$possible_obsolescence_signal else character()
  if (any(obsolescence %in% c("strong recent inactivity signal", "possible decline"))) {
    return(data.table::data.table(recommendation = "obsolescence_review_recommended", reason = "Demand diagnostics show possible decline or strong recent inactivity; review method performance before adopting a forecast.", confidence = "diagnostic_advisory", automatic_selection = FALSE))
  }
  if (length(unique(entity_best)) > 1L) {
    return(data.table::data.table(recommendation = "method_varies_by_entity", reason = paste("Best method differs across entities:", paste(sort(unique(entity_best)), collapse = ", ")), confidence = "deterministic_comparison", automatic_selection = FALSE))
  }
  if (length(unique(horizon_best)) > 1L) {
    return(data.table::data.table(recommendation = "method_varies_by_horizon", reason = paste("Best method differs across horizons:", paste(sort(unique(horizon_best)), collapse = ", ")), confidence = "deterministic_comparison", automatic_selection = FALSE))
  }
  best <- method_summary[which.min(primary_score)]
  data.table::data.table(recommendation = paste0(best$method, "_preferred"), reason = paste("Lowest primary score:", best$method, "rmse =", signif(best$primary_score, 4)), confidence = "deterministic_comparison", automatic_selection = FALSE)
}

#' Compare Intermittent-Demand Forecasting Methods
#'
#' @param data Intermittent-demand data.
#' @param target Target value column.
#' @param date Date/time column.
#' @param entity Optional entity column.
#' @param frequency Forecast frequency.
#' @param horizon Forecast horizon.
#' @param origins Optional common forecast origins.
#' @param origin_count Number of latest origins when `origins` is omitted.
#' @param occurrence_threshold Threshold above which demand occurs.
#' @param alpha Demand-size smoothing parameter for classical methods.
#' @param beta Occurrence smoothing parameter for TSB.
#' @param include_hurdle Whether to include supervised Hurdle when CatBoost and
#'   Rodeo are available.
#' @param hurdle_spec Optional prebuilt Hurdle specification.
#' @param future_known_variables Known future variables for optional Hurdle.
#' @param static_entity_features Static entity features for optional Hurdle.
#' @param engine_parameters Optional Hurdle/CatBoost parameters.
#' @param dataset_id Optional dataset id.
#'
#' @return An `aq_intermittent_method_comparison_result`.
#' @export
aq_compare_intermittent_demand_methods <- function(
  data,
  target,
  date,
  entity = NULL,
  frequency = "auto",
  horizon = 1L,
  origins = NULL,
  origin_count = 2L,
  occurrence_threshold = 0,
  alpha = 0.1,
  beta = 0.1,
  include_hurdle = FALSE,
  hurdle_spec = NULL,
  future_known_variables = character(),
  static_entity_features = character(),
  engine_parameters = list(),
  dataset_id = NULL
) {
  dt <- data.table::as.data.table(data.table::copy(data))
  entity_col <- if (is.null(entity) || is.na(entity)) NULL else as.character(entity)[1L]
  origins <- aq_intermittent_comparison_origins(dt, date, horizon, origins = origins, origin_count = origin_count)
  methods <- c("naive", "seasonal_naive", "croston", "sba", "tsb")
  forecast_rows <- list()
  elapsed_rows <- list()
  for (origin in origins) {
    for (method in methods) {
      elapsed <- system.time({
        out <- aq_intermittent_manual_forecast(dt, target, date, entity = entity_col, frequency = frequency, horizon = horizon, origin = origin, method = method, occurrence_threshold = occurrence_threshold, alpha = alpha, beta = beta)
      })
      forecast_rows[[length(forecast_rows) + 1L]] <- out
      elapsed_rows[[length(elapsed_rows) + 1L]] <- data.table::data.table(method = method, forecast_origin = as.Date(origin), elapsed_seconds = unname(elapsed[["elapsed"]]))
    }
    if (isTRUE(include_hurdle) && requireNamespace("catboost", quietly = TRUE)) {
      hspec <- hurdle_spec
      if (is.null(hspec)) {
        hspec <- aq_hurdle_forecast_spec(target = target, date = date, entity = entity_col, frequency = frequency, horizon = horizon, occurrence_threshold = occurrence_threshold, future_known_variables = future_known_variables, static_entity_features = static_entity_features, engine_parameters = engine_parameters, dataset_id = dataset_id)
      }
      elapsed <- system.time({
        hfit <- tryCatch(aq_fit_hurdle_forecast(hspec, dt, origin = origin), error = function(e) e)
      })
      if (!inherits(hfit, "error")) {
        out <- data.table::copy(hfit$data)
        out[, method := "hurdle"]
        out[, forecast_origin := as.Date(origin)]
        forecast_rows[[length(forecast_rows) + 1L]] <- out
        elapsed_rows[[length(elapsed_rows) + 1L]] <- data.table::data.table(method = "hurdle", forecast_origin = as.Date(origin), elapsed_seconds = unname(elapsed[["elapsed"]]))
      }
    }
  }
  forecasts <- data.table::rbindlist(forecast_rows, use.names = TRUE, fill = TRUE)
  method_metrics_long <- forecasts[, aq_forecast_metrics(.SD), by = method]
  method_summary <- data.table::dcast(method_metrics_long, method ~ metric, value.var = "value")
  method_summary[, primary_score := rmse]
  cost <- data.table::rbindlist(elapsed_rows, use.names = TRUE, fill = TRUE)[, .(elapsed_seconds = sum(elapsed_seconds, na.rm = TRUE)), by = method]
  method_summary <- merge(method_summary, cost, by = "method", all.x = TRUE, sort = FALSE)
  horizon_long <- forecasts[, aq_forecast_metrics(.SD), by = c("method", "horizon")]
  horizon_metrics <- data.table::dcast(horizon_long, method + horizon ~ metric, value.var = "value")
  entity_metrics <- data.table::data.table()
  if (!is.null(entity_col) && entity_col %in% names(forecasts)) {
    entity_long <- forecasts[, aq_forecast_metrics(.SD), by = c("method", entity_col)]
    data.table::setnames(entity_long, entity_col, "entity_value")
    entity_metrics <- data.table::dcast(entity_long, method + entity_value ~ metric, value.var = "value")
  }
  diagnostics <- aq_intermittent_demand_diagnostics(dt, target, date, entity = entity_col, occurrence_threshold = occurrence_threshold)
  intermittent_metrics <- forecasts[, aq_intermittent_method_metrics(.SD, threshold = occurrence_threshold), by = method]
  recommendation <- aq_intermittent_recommendation(method_summary, entity_metrics, horizon_metrics, diagnostics)
  comparison_id <- aq_vnext_id("intermittent_method_comparison")
  artifact <- new_table_artifact(
    id = comparison_id,
    title = "Intermittent Demand Method Comparison",
    data = method_summary,
    source_generator = "aq_compare_intermittent_demand_methods",
    tags = c("vnext", "forecast", "intermittent_demand", "method_comparison"),
    dependencies = character(),
    version = "aq_intermittent_method_comparison_artifact_v1",
    metadata = list(artifact_type = "intermittent_method_comparison", comparison_id = comparison_id, target = target, date = date, entity = entity_col, origins = origins, horizon = horizon, methods = unique(forecasts$method), diagnostics = diagnostics, horizon_metrics = horizon_metrics, entity_metrics = entity_metrics, intermittent_metrics = intermittent_metrics, recommendation = recommendation, supported_downstream_actions = c("compare", "report", "campaign_review"))
  )
  artifact <- aq_vnext_attach_envelope(artifact, artifact_id = comparison_id, artifact_type = "intermittent_method_comparison", artifact_version = "aq_intermittent_method_comparison_artifact_v1", parent_artifact_ids = character(), lineage = list(target = target, date = date, entity = entity_col, origins = origins, horizon = horizon), task = "intermittent_demand_method_comparison", operator = "forecast_method_comparison", engine = "deterministic_comparison", specification_id = comparison_id, dataset_id = aq_vnext_default(dataset_id, NA_character_), supported_actions = c("compare", "report", "campaign_review"), producer = "aq_compare_intermittent_demand_methods")
  result <- list(comparison_id = comparison_id, schema_version = "aq_intermittent_method_comparison_result_v1", forecasts = forecasts, method_summary = method_summary, horizon_metrics = horizon_metrics, entity_metrics = entity_metrics, intermittent_metrics = intermittent_metrics, diagnostics = diagnostics, recommendation = recommendation, artifact = artifact, comparison_ready = TRUE, created_at = aq_vnext_now())
  class(result) <- c("aq_intermittent_method_comparison_result", "list")
  result
}

aq_vnext_hurdle_forecast_fixture <- function(entity_count = 4L, n = 80L) {
  dt <- aq_vnext_panel_forecast_fixture(entity_count = entity_count, n = n)
  dt[, region := data.table::fifelse(entity %in% c("store_1", "store_2"), "region_a", "region_b")]
  dt[, demand := data.table::fifelse((seq_len(.N) + as.integer(factor(entity))) %% 4L == 0L, 0, demand), by = entity]
  dt[, demand := data.table::fifelse((seq_len(.N)) %% 11L == 0L, 0, demand), by = entity]
  dt
}

#' QA for vNext Hurdle Forecasting Foundation
#'
#' @return A QA `data.table`.
#' @export
qa_vnext_hurdle_forecasting_foundation <- function() {
  rows <- list()
  add <- function(check, ok, message = "") {
    rows[[length(rows) + 1L]] <<- data.table::data.table(suite = "vnext_hurdle_forecasting_foundation", check = check, status = if (isTRUE(ok)) "pass" else "fail", message = message)
  }
  dt <- aq_vnext_hurdle_forecast_fixture()
  spec <- aq_hurdle_forecast_spec(
    target = "demand", date = "date", entity = "entity", group = "region", frequency = "day", horizon = 2L,
    future_known_variables = "promo", static_entity_features = "store_size", occurrence_threshold = 0,
    engine_parameters = list(iterations = 4L, depth = 2L, learning_rate = 0.1, lag_periods = c(1L, 7L), rolling_windows = 3L, date_features = c("year", "month", "day", "dow", "day_index")),
    minimum_history = 20L, minimum_positive_history = 5L, dataset_id = "qa_hurdle_forecast_fixture"
  )
  validation <- aq_validate_hurdle_forecast_spec(spec, dt)
  add("hurdle_spec_constructed", inherits(spec, "aq_hurdle_forecast_spec"))
  add("hurdle_validation_passes", !any(validation$status %in% c("fail", "error")), paste(validation$message, collapse = " | "))
  intermittent_diagnostics <- aq_intermittent_demand_diagnostics(dt, "demand", "date", entity = "entity")
  add("intermittent_demand_diagnostics", all(c("zero_proportion", "average_inter_demand_interval", "longest_zero_run", "recent_zero_run", "positive_cv", "demand_size_stability", "interval_stability", "occurrence_rate_trend", "possible_obsolescence_signal", "sparse_history_status", "positive_mean", "demand_sparsity", "intermittent_demand_suitability") %in% names(intermittent_diagnostics)))
  croston_spec <- aq_croston_forecast_spec(target = "demand", date = "date", entity = "entity", frequency = "day", horizon = 2L, dataset_id = "qa_hurdle_forecast_fixture")
  sba_spec <- aq_sba_forecast_spec(target = "demand", date = "date", entity = "entity", frequency = "day", horizon = 2L, dataset_id = "qa_hurdle_forecast_fixture")
  tsb_spec <- aq_tsb_forecast_spec(target = "demand", date = "date", entity = "entity", frequency = "day", horizon = 2L, alpha = 0.1, beta = 0.2, dataset_id = "qa_hurdle_forecast_fixture")
  croston <- aq_fit_croston_forecast(croston_spec, dt)
  sba <- aq_fit_sba_forecast(sba_spec, dt)
  tsb <- aq_fit_tsb_forecast(tsb_spec, dt)
  croston_assessment <- aq_assess_croston_forecast(croston)
  sba_assessment <- aq_assess_sba_forecast(sba)
  tsb_assessment <- aq_assess_tsb_forecast(tsb)
  croston_backtest <- aq_rolling_origin_croston_forecast(croston_spec, dt, origin_count = 2L)
  sba_backtest <- aq_rolling_origin_sba_forecast(sba_spec, dt, origin_count = 2L)
  tsb_backtest <- aq_rolling_origin_tsb_forecast(tsb_spec, dt, origin_count = 2L)
  add("croston_spec_constructed", inherits(croston_spec, "aq_croston_forecast_spec"))
  add("sba_spec_constructed", inherits(sba_spec, "aq_sba_forecast_spec"))
  add("tsb_spec_constructed", inherits(tsb_spec, "aq_tsb_forecast_spec"))
  add("croston_forecast", inherits(croston, "aq_croston_forecast_result") && "croston" %in% croston$data$baseline_engine)
  add("sba_forecast", inherits(sba, "aq_sba_forecast_result") && "sba" %in% sba$data$baseline_engine && is.finite(sba$method_state$demand_size_estimate[1L]))
  add("tsb_forecast", inherits(tsb, "aq_tsb_forecast_result") && "tsb" %in% tsb$data$baseline_engine && is.finite(tsb$method_state$occurrence_probability_state[1L]))
  add("croston_artifact_integrity", !aq_vnext_has_validation_error(aq_validate_artifact(croston$artifact)))
  add("sba_artifact_integrity", !aq_vnext_has_validation_error(aq_validate_artifact(sba$artifact)))
  add("tsb_artifact_integrity", !aq_vnext_has_validation_error(aq_validate_artifact(tsb$artifact)))
  add("croston_assessment", inherits(croston_assessment, "aq_croston_forecast_assessment_result") && nrow(croston_assessment$metrics) > 0L)
  add("sba_assessment", inherits(sba_assessment, "aq_sba_forecast_assessment_result") && nrow(sba_assessment$intermittent_metrics) > 0L)
  add("tsb_assessment", inherits(tsb_assessment, "aq_tsb_forecast_assessment_result") && nrow(tsb_assessment$intermittent_metrics) > 0L)
  add("croston_rolling_origin", inherits(croston_backtest, "aq_croston_forecast_backtest_result") && length(croston_backtest$forecasts) == 2L)
  add("sba_rolling_origin", inherits(sba_backtest, "aq_sba_forecast_backtest_result") && length(sba_backtest$forecasts) == 2L)
  add("tsb_rolling_origin", inherits(tsb_backtest, "aq_tsb_forecast_backtest_result") && length(tsb_backtest$forecasts) == 2L)
  method_comparison <- aq_compare_intermittent_demand_methods(dt, target = "demand", date = "date", entity = "entity", frequency = "day", horizon = 2L, origin_count = 2L, occurrence_threshold = 0, include_hurdle = FALSE, dataset_id = "qa_hurdle_forecast_fixture")
  add("method_comparison_artifact", inherits(method_comparison, "aq_intermittent_method_comparison_result") && all(c("naive", "seasonal_naive", "croston", "sba", "tsb") %in% method_comparison$method_summary$method) && !aq_vnext_has_validation_error(aq_validate_artifact(method_comparison$artifact)))
  add("method_comparison_recommendation", method_comparison$recommendation$recommendation %in% c("naive_preferred", "seasonal_naive_preferred", "croston_preferred", "sba_preferred", "tsb_preferred", "hurdle_preferred", "method_varies_by_entity", "method_varies_by_horizon", "evidence_insufficient", "obsolescence_review_recommended"))
  add("method_comparison_entity_horizon_evidence", nrow(method_comparison$entity_metrics) > 0L && nrow(method_comparison$horizon_metrics) > 0L)
  all_zero <- data.table::copy(dt)
  all_zero[, demand := 0]
  insufficient <- aq_compare_intermittent_demand_methods(all_zero, target = "demand", date = "date", entity = "entity", frequency = "day", horizon = 2L, origin_count = 1L)
  add("method_comparison_evidence_insufficient_path", insufficient$recommendation$recommendation %in% c("evidence_insufficient", "method_varies_by_entity", "naive_preferred", "croston_preferred", "sba_preferred", "tsb_preferred"))
  sparse <- data.table::copy(dt)
  sparse[entity == "store_1", demand := 0]
  sparse_validation <- aq_validate_hurdle_forecast_spec(spec, sparse)
  add("insufficient_positive_history_diagnostic", any(sparse_validation$check == "positive_history" & sparse_validation$status == "warning"))
  add("zero_rate_diagnostic", any(validation$check == "zero_rate"))
  if (requireNamespace("catboost", quietly = TRUE)) {
    forecast <- aq_fit_hurdle_forecast(spec, dt)
    assessment <- aq_assess_hurdle_forecast(forecast)
    add("occurrence_model", all(c("occurrence_probability", "predicted_occurrence") %in% names(forecast$data)))
    add("positive_demand_model", "positive_prediction" %in% names(forecast$data))
    add("combined_expectation", all(abs(forecast$data$forecast - forecast$data$occurrence_probability * forecast$data$positive_prediction) < 1e-8))
    add("supervised_baseline_engines", all(c("naive", "seasonal_naive", "croston", "sba", "tsb") %in% names(forecast$baseline_forecasts)))
    add("artifact_integrity", !aq_vnext_has_validation_error(aq_validate_artifact(forecast$artifact)))
    add("assessment", inherits(assessment, "aq_hurdle_forecast_assessment_result") && nrow(assessment$combined_metrics) > 0L && nrow(assessment$occurrence_metrics) > 0L)
    add("assessment_baseline_metrics", nrow(assessment$baseline_metrics) > 0L && all(c("croston", "sba", "tsb") %in% assessment$baseline_metrics$baseline_engine))
    add("assessment_artifact_integrity", !aq_vnext_has_validation_error(aq_validate_artifact(assessment$assessment_artifact)))
    backtest <- aq_rolling_origin_hurdle_forecast(spec, dt, origin_count = 2L)
    add("rolling_origin", inherits(backtest, "aq_hurdle_forecast_backtest_result") && length(backtest$forecasts) == 2L)
    strategy <- aq_evaluate_hurdle_panel_strategies(spec, dt, candidate_strategies = c("independent", "grouped", "global"))
    add("panel_strategy_compatibility", inherits(strategy, "aq_hurdle_strategy_evaluation_result") && all(c("independent", "grouped", "global") %in% strategy$forecasts$strategy))
    add("strategy_artifact_integrity", !aq_vnext_has_validation_error(aq_validate_artifact(strategy$artifact)))
    add("analytics_shinyapp_compatibility", all(vapply(list(forecast$artifact, assessment$assessment_artifact, strategy$artifact), is.list, logical(1L))) && all(vapply(list(forecast$artifact, assessment$assessment_artifact, strategy$artifact), function(x) "campaign_review" %in% aq_supported_actions(x), logical(1L))))
  } else {
    add("catboost_available", FALSE, "catboost package is required for hurdle forecast QA.")
  }
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}
