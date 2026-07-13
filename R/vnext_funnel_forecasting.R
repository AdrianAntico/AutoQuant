# AutoQuant vNext funnel forecasting foundation.

aq_funnel_strategy_levels <- function() {
  c("stage", "transition")
}

aq_funnel_make_transitions <- function(stages) {
  stages <- aq_vnext_clean_chr(stages)
  if (length(stages) < 2L) {
    return(data.table::data.table(
      transition_id = character(),
      from_stage = character(),
      to_stage = character(),
      transition_order = integer()
    ))
  }
  data.table::data.table(
    transition_id = paste(stages[-length(stages)], stages[-1L], sep = "_to_"),
    from_stage = stages[-length(stages)],
    to_stage = stages[-1L],
    transition_order = seq_len(length(stages) - 1L)
  )
}

#' Create a vNext Funnel Forecast Specification
#'
#' Defines a deterministic funnel forecasting contract. A funnel is represented
#' as ordered stages, explicit adjacent transitions, cohort identity, calendar
#' date, and optional stage/maturity fields.
#'
#' @param stages Ordered stage identifiers.
#' @param stage Stage identifier column.
#' @param value Numeric stage volume column.
#' @param date Calendar observation date column.
#' @param cohort Cohort identifier column.
#' @param stage_date Optional stage date column.
#' @param maturity Optional maturity/age column.
#' @param frequency Calendar frequency.
#' @param horizon Forecast horizon.
#' @param forecast_origin Optional forecast origin date.
#' @param strategy Funnel strategy. Supports `"stage"` and `"transition"`.
#' @param known_future_variables Optional known future variables.
#' @param forecast_spec_id Optional specification identifier.
#' @param dataset_id Optional dataset identifier.
#' @param supported_downstream_actions Supported downstream actions.
#'
#' @return An `aq_funnel_forecast_spec`.
#' @export
aq_funnel_forecast_spec <- function(
  stages,
  stage = "stage",
  value = "value",
  date = "date",
  cohort = "cohort",
  stage_date = NULL,
  maturity = NULL,
  frequency = "auto",
  horizon = 1L,
  forecast_origin = NULL,
  strategy = "stage",
  known_future_variables = character(),
  forecast_spec_id = NULL,
  dataset_id = NULL,
  supported_downstream_actions = c("forecast", "assess", "compare", "report", "campaign_review")
) {
  frequency <- match.arg(tolower(frequency), aq_forecast_frequency_levels())
  strategy <- match.arg(tolower(strategy), aq_funnel_strategy_levels())
  stages <- aq_vnext_clean_chr(stages)
  horizon <- as.integer(horizon)[1L]
  if (is.null(forecast_spec_id)) {
    forecast_spec_id <- aq_vnext_id(paste("funnel_forecast_spec", strategy, paste(stages, collapse = "_"), sep = "_"))
  }
  spec <- list(
    forecast_spec_id = as.character(forecast_spec_id)[1L],
    schema_version = "aq_funnel_forecast_spec_v1",
    stages = stages,
    stage = as.character(stage)[1L],
    value = as.character(value)[1L],
    date = as.character(date)[1L],
    cohort = as.character(cohort)[1L],
    stage_date = if (is.null(stage_date)) NA_character_ else as.character(stage_date)[1L],
    maturity = if (is.null(maturity)) NA_character_ else as.character(maturity)[1L],
    frequency = frequency,
    horizon = horizon,
    forecast_origin = if (is.null(forecast_origin)) NULL else as.Date(forecast_origin)[1L],
    strategy = strategy,
    transitions = aq_funnel_make_transitions(stages),
    known_future_variables = aq_vnext_unique_chr(known_future_variables),
    dataset_id = aq_vnext_default(dataset_id, NA_character_),
    supported_downstream_actions = aq_vnext_unique_chr(supported_downstream_actions),
    created_at = aq_vnext_now()
  )
  class(spec) <- c("aq_funnel_forecast_spec", "list")
  spec
}

#' Validate a vNext Funnel Forecast Specification
#'
#' @param spec An `aq_funnel_forecast_spec`.
#' @param data Optional long-form funnel data.
#'
#' @return A deterministic diagnostic table.
#' @export
aq_validate_funnel_forecast_spec <- function(spec, data = NULL) {
  rows <- list()
  add <- function(check, status, message, severity = status) {
    rows[[length(rows) + 1L]] <<- aq_vnext_validation_table(check, status, message, severity)
  }
  if (!inherits(spec, "aq_funnel_forecast_spec")) {
    add("funnel_spec_class", "fail", "spec must be created by aq_funnel_forecast_spec().")
    return(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
  }
  add("funnel_spec_class", "pass", "funnel specification is typed.", "info")
  if (length(spec$stages) < 2L) {
    add("stage_count", "fail", "funnel requires at least two ordered stages.")
  } else {
    add("stage_count", "pass", paste("stages:", length(spec$stages)), "info")
  }
  if (anyDuplicated(spec$stages)) {
    add("duplicate_stages", "fail", "stages must be unique.")
  } else {
    add("duplicate_stages", "pass", "stage identifiers are unique.", "info")
  }
  if (nrow(spec$transitions) != max(length(spec$stages) - 1L, 0L)) {
    add("transition_structure", "fail", "adjacent transition structure is invalid.")
  } else {
    add("transition_structure", "pass", "adjacent transitions are explicit.", "info")
  }
  if (!is.finite(spec$horizon) || spec$horizon < 1L) {
    add("horizon", "fail", "horizon must be a positive integer.")
  } else {
    add("horizon", "pass", paste("horizon:", spec$horizon), "info")
  }
  if (!spec$strategy %in% aq_funnel_strategy_levels()) {
    add("strategy", "fail", paste("unsupported funnel strategy:", spec$strategy))
  } else {
    add("strategy", "pass", paste("strategy:", spec$strategy), "info")
  }

  if (!is.null(data)) {
    dt <- data.table::as.data.table(data.table::copy(data))
    required <- c(spec$stage, spec$value, spec$date, spec$cohort)
    optional <- c(spec$stage_date, spec$maturity, spec$known_future_variables)
    optional <- optional[!is.na(optional) & nzchar(optional)]
    missing_required <- setdiff(required, names(dt))
    if (length(missing_required)) {
      add("required_columns", "fail", paste("missing required columns:", paste(missing_required, collapse = ", ")))
      return(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
    }
    add("required_columns", "pass", "required columns exist.", "info")
    missing_optional <- setdiff(optional, names(dt))
    if (length(missing_optional)) {
      add("optional_columns", "warning", paste("missing optional columns:", paste(missing_optional, collapse = ", ")))
    } else {
      add("optional_columns", "pass", "optional declared columns exist.", "info")
    }
    data_stages <- unique(as.character(dt[[spec$stage]]))
    missing_stages <- setdiff(spec$stages, data_stages)
    extra_stages <- setdiff(data_stages, spec$stages)
    if (length(missing_stages)) {
      add("missing_stages", "fail", paste("data is missing stages:", paste(missing_stages, collapse = ", ")))
    } else {
      add("missing_stages", "pass", "all declared stages appear in data.", "info")
    }
    if (length(extra_stages)) {
      add("unsupported_stages", "warning", paste("data contains stages outside the specification:", paste(extra_stages, collapse = ", ")))
    } else {
      add("unsupported_stages", "pass", "data stages match the specification.", "info")
    }
    numeric_value <- suppressWarnings(as.numeric(dt[[spec$value]]))
    if (any(is.na(numeric_value) & !is.na(dt[[spec$value]]))) {
      add("numeric_value", "fail", "value column must be numeric or numeric-coercible.")
    } else {
      add("numeric_value", "pass", "value column is numeric.", "info")
    }
    if (any(numeric_value < 0, na.rm = TRUE)) {
      add("negative_values", "fail", "funnel stage volumes must be nonnegative.")
    } else {
      add("negative_values", "pass", "stage volumes are nonnegative.", "info")
    }
    dt[, .aq_date := as.Date(get(spec$date))]
    if (any(is.na(dt$.aq_date))) {
      add("date_parse", "fail", "date column contains values that cannot be parsed as Date.")
    } else {
      add("date_parse", "pass", "date column parses as Date.", "info")
    }
    duplicate_keys <- dt[, .N, by = c(spec$cohort, spec$date, spec$stage)][N > 1L]
    if (nrow(duplicate_keys)) {
      add("duplicate_stage_observations", "fail", "cohort/date/stage observations must be unique.")
    } else {
      add("duplicate_stage_observations", "pass", "cohort/date/stage observations are unique.", "info")
    }
    if (!is.na(spec$stage_date) && spec$stage_date %in% names(dt)) {
      stage_dates <- as.Date(dt[[spec$stage_date]])
      if (any(stage_dates < dt$.aq_date, na.rm = TRUE)) {
        add("stage_date_order", "warning", "some stage dates precede calendar observation dates; review maturity semantics.")
      } else {
        add("stage_date_order", "pass", "stage dates do not precede calendar dates.", "info")
      }
    }
    if (!is.na(spec$maturity) && spec$maturity %in% names(dt)) {
      maturity_values <- suppressWarnings(as.numeric(dt[[spec$maturity]]))
      if (any(maturity_values < 0, na.rm = TRUE)) {
        add("maturity_nonnegative", "fail", "maturity values must be nonnegative.")
      } else {
        add("maturity_nonnegative", "pass", "maturity values are nonnegative.", "info")
      }
    } else {
      add("maturity_available", "warning", "no explicit maturity column supplied; maturity will be derived from cohort/date ordering.")
    }
    if (!is.null(spec$forecast_origin)) {
      future_rows <- dt[.aq_date > spec$forecast_origin]
      if (nrow(future_rows)) {
        add("future_leakage", "warning", "data contains rows after forecast_origin; they are used only as actuals for assessment.")
      } else {
        add("future_leakage", "pass", "no post-origin rows detected.", "info")
      }
    }
  }
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

aq_funnel_prepare_data <- function(spec, data) {
  dt <- data.table::as.data.table(data.table::copy(data))
  dt[, .aq_stage := factor(as.character(get(spec$stage)), levels = spec$stages, ordered = TRUE)]
  dt[, .aq_stage_chr := as.character(.aq_stage)]
  dt[, .aq_stage_order := as.integer(.aq_stage)]
  dt[, .aq_value := as.numeric(get(spec$value))]
  dt[, .aq_date := as.Date(get(spec$date))]
  dt[, .aq_cohort := as.character(get(spec$cohort))]
  if (!is.na(spec$stage_date) && spec$stage_date %in% names(dt)) {
    dt[, .aq_stage_date := as.Date(get(spec$stage_date))]
  } else {
    dt[, .aq_stage_date := .aq_date]
  }
  if (!is.na(spec$maturity) && spec$maturity %in% names(dt)) {
    dt[, .aq_maturity := as.numeric(get(spec$maturity))]
  } else {
    data.table::setorder(dt, .aq_cohort, .aq_date, .aq_stage_order)
    dt[, .aq_maturity := as.numeric(.aq_date - min(.aq_date, na.rm = TRUE)), by = .aq_cohort]
  }
  dt[!is.na(.aq_stage)]
}

aq_funnel_origin <- function(spec, dt) {
  if (!is.null(spec$forecast_origin)) {
    return(as.Date(spec$forecast_origin)[1L])
  }
  max(dt$.aq_date, na.rm = TRUE)
}

aq_funnel_transition_observations <- function(spec, dt, origin = NULL) {
  origin <- aq_vnext_default(origin, max(dt$.aq_date, na.rm = TRUE))
  hist <- dt[.aq_date <= as.Date(origin)]
  wide <- data.table::dcast(
    hist,
    .aq_cohort + .aq_date ~ .aq_stage_chr,
    value.var = ".aq_value",
    fun.aggregate = sum,
    fill = NA_real_
  )
  maturity <- hist[, .(maturity = max(.aq_maturity, na.rm = TRUE)), by = .(.aq_cohort, .aq_date)]
  maturity[!is.finite(maturity), maturity := NA_real_]
  wide <- maturity[wide, on = c(".aq_cohort", ".aq_date")]
  rows <- lapply(seq_len(nrow(spec$transitions)), function(i) {
    tr <- spec$transitions[i]
    from <- tr$from_stage
    to <- tr$to_stage
    if (!all(c(from, to) %in% names(wide))) {
      return(data.table::data.table())
    }
    out <- wide[, .(
      cohort = .aq_cohort,
      date = .aq_date,
      maturity = maturity,
      transition_id = tr$transition_id,
      from_stage = from,
      to_stage = to,
      from_volume = get(from),
      to_volume = get(to)
    )]
    out[, conversion_rate := data.table::fifelse(from_volume > 0, to_volume / from_volume, NA_real_)]
    out[, conversion_rate := pmin(pmax(conversion_rate, 0), 1)]
    out
  })
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

aq_funnel_stage_forecast_table <- function(spec, dt, origin) {
  future_dates <- aq_forecast_next_dates(origin, spec$frequency, spec$horizon)
  hist <- dt[.aq_date <= origin]
  actuals <- dt[.aq_date > origin & .aq_date %in% future_dates]
  last_rows <- hist[order(.aq_date), .SD[.N], by = c(".aq_cohort", ".aq_stage_chr", ".aq_stage_order")]
  forecast_grid <- data.table::CJ(.aq_cohort = unique(hist$.aq_cohort), horizon = seq_len(spec$horizon))
  forecast_grid[, forecast_date := future_dates[horizon]]
  forecast_grid <- forecast_grid[last_rows, on = ".aq_cohort", allow.cartesian = TRUE]
  forecast_grid[, `:=`(
    stage = .aq_stage_chr,
    stage_order = .aq_stage_order,
    forecast = .aq_value,
    maturity = .aq_maturity + horizon,
    forecast_strategy = "stage"
  )]
  actual_lookup <- actuals[, .(actual = sum(.aq_value, na.rm = TRUE)), by = c(".aq_cohort", ".aq_stage_chr", ".aq_date")]
  forecast_grid <- actual_lookup[forecast_grid, on = c(".aq_cohort", ".aq_stage_chr", ".aq_date" = "forecast_date")]
  data.table::setnames(forecast_grid, ".aq_cohort", "cohort")
  forecast_grid[, transition_id := NA_character_]
  forecast_grid[, conversion_rate := NA_real_]
  forecast_grid[, .(cohort, forecast_date = .aq_date, horizon, stage, stage_order, transition_id, forecast, actual, conversion_rate, maturity, forecast_strategy)]
}

aq_funnel_transition_forecast_table <- function(spec, dt, origin) {
  future_dates <- aq_forecast_next_dates(origin, spec$frequency, spec$horizon)
  hist <- dt[.aq_date <= origin]
  actuals <- dt[.aq_date > origin & .aq_date %in% future_dates]
  first_stage <- spec$stages[1L]
  last_base <- hist[.aq_stage_chr == first_stage][order(.aq_date), .SD[.N], by = .aq_cohort]
  rates <- aq_funnel_transition_observations(spec, dt, origin = origin)
  rate_summary <- rates[, .(
    conversion_rate = mean(conversion_rate, na.rm = TRUE),
    conversion_sd = stats::sd(conversion_rate, na.rm = TRUE),
    observations = sum(!is.na(conversion_rate))
  ), by = .(transition_id, from_stage, to_stage)]
  rate_summary[!is.finite(conversion_rate), conversion_rate := 0]
  rows <- list()
  for (cohort_value in unique(hist$.aq_cohort)) {
    base <- last_base[.aq_cohort == cohort_value]
    if (!nrow(base)) next
    for (h in seq_len(spec$horizon)) {
      current_volume <- as.numeric(base$.aq_value[1L])
      maturity_value <- as.numeric(base$.aq_maturity[1L]) + h
      rows[[length(rows) + 1L]] <- data.table::data.table(
        cohort = cohort_value,
        forecast_date = future_dates[h],
        horizon = h,
        stage = first_stage,
        stage_order = 1L,
        transition_id = NA_character_,
        forecast = current_volume,
        conversion_rate = NA_real_,
        maturity = maturity_value,
        forecast_strategy = "transition"
      )
      for (i in seq_len(nrow(spec$transitions))) {
        tr <- spec$transitions[i]
        rate <- rate_summary[transition_id == tr$transition_id, conversion_rate]
        if (!length(rate) || !is.finite(rate)) rate <- 0
        current_volume <- current_volume * rate
        rows[[length(rows) + 1L]] <- data.table::data.table(
          cohort = cohort_value,
          forecast_date = future_dates[h],
          horizon = h,
          stage = tr$to_stage,
          stage_order = i + 1L,
          transition_id = tr$transition_id,
          forecast = current_volume,
          conversion_rate = rate,
          maturity = maturity_value,
          forecast_strategy = "transition"
        )
      }
    }
  }
  out <- data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
  actual_lookup <- actuals[, .(actual = sum(.aq_value, na.rm = TRUE)), by = c(".aq_cohort", ".aq_stage_chr", ".aq_date")]
  out <- actual_lookup[out, on = c(".aq_cohort" = "cohort", ".aq_stage_chr" = "stage", ".aq_date" = "forecast_date")]
  data.table::setnames(out, ".aq_date", "forecast_date")
  out[, .(cohort = .aq_cohort, forecast_date, horizon, stage = .aq_stage_chr, stage_order, transition_id, forecast, actual, conversion_rate, maturity, forecast_strategy)]
}

aq_funnel_maturity_summary <- function(spec, dt, origin) {
  hist <- dt[.aq_date <= origin]
  hist[, .(
    min_maturity = min(.aq_maturity, na.rm = TRUE),
    max_maturity = max(.aq_maturity, na.rm = TRUE),
    latest_observed_date = max(.aq_date, na.rm = TRUE),
    observed_stage_count = data.table::uniqueN(.aq_stage_chr),
    latest_stage = .aq_stage_chr[which.max(.aq_stage_order)]
  ), by = .aq_cohort][, cohort := .aq_cohort][, .aq_cohort := NULL][]
}

aq_funnel_transition_diagnostics <- function(spec, dt, origin) {
  transitions <- aq_funnel_transition_observations(spec, dt, origin = origin)
  if (!nrow(transitions)) {
    return(data.table::data.table(
      transition_id = spec$transitions$transition_id,
      from_stage = spec$transitions$from_stage,
      to_stage = spec$transitions$to_stage,
      observations = 0L,
      mean_conversion = NA_real_,
      conversion_stability = "insufficient evidence",
      warnings = "no comparable transition observations"
    ))
  }
  transitions[, {
    valid <- conversion_rate[is.finite(conversion_rate)]
    obs <- length(valid)
    sd_value <- if (obs > 1L) stats::sd(valid) else NA_real_
    .(
      observations = obs,
      mean_conversion = if (obs) mean(valid) else NA_real_,
      conversion_sd = sd_value,
      min_conversion = if (obs) min(valid) else NA_real_,
      max_conversion = if (obs) max(valid) else NA_real_,
      conversion_stability = dplyr_like_fcase(
        obs < 3L, "insufficient evidence",
        is.finite(sd_value) && sd_value > 0.2, "unstable",
        default = "stable"
      ),
      warnings = if (any(conversion_rate > 1, na.rm = TRUE)) "conversion exceeded one before clipping" else NA_character_
    )
  }, by = .(transition_id, from_stage, to_stage)]
}

dplyr_like_fcase <- function(..., default = NA_character_) {
  args <- list(...)
  n <- length(args)
  if (n %% 2L != 0L) stop("conditions and values must be paired.", call. = FALSE)
  out <- default
  for (i in seq(1L, n, by = 2L)) {
    if (isTRUE(args[[i]])) {
      out <- args[[i + 1L]]
      break
    }
  }
  out
}

#' Fit a vNext Funnel Forecast
#'
#' @param spec An `aq_funnel_forecast_spec`.
#' @param data Long-form funnel data.
#' @param origin Optional forecast origin.
#'
#' @return An `aq_funnel_forecast_result`.
#' @export
aq_fit_funnel_forecast <- function(spec, data, origin = NULL) {
  validation <- aq_validate_funnel_forecast_spec(spec, data)
  if (aq_vnext_has_validation_error(validation)) {
    stop(paste(validation[status %in% c("fail", "error"), message], collapse = " "), call. = FALSE)
  }
  dt <- aq_funnel_prepare_data(spec, data)
  origin <- as.Date(aq_vnext_default(origin, aq_funnel_origin(spec, dt)))
  if (identical(spec$frequency, "auto")) {
    detected <- aq_forecast_detect_frequency(dt$.aq_date)
    frequency <- if (detected$frequency %in% aq_forecast_frequency_levels()) detected$frequency else "day"
  } else {
    frequency <- spec$frequency
  }
  spec$frequency <- frequency
  forecast_dt <- if (identical(spec$strategy, "stage")) {
    aq_funnel_stage_forecast_table(spec, dt, origin)
  } else {
    aq_funnel_transition_forecast_table(spec, dt, origin)
  }
  forecast_id <- aq_vnext_id(paste0("funnel_forecast_", spec$strategy))
  maturity_summary <- aq_funnel_maturity_summary(spec, dt, origin)
  transition_diagnostics <- aq_funnel_transition_diagnostics(spec, dt, origin)
  stage_summary <- dt[.aq_date <= origin, .(
    observations = .N,
    latest_value = .SD[which.max(.aq_date), .aq_value],
    mean_value = mean(.aq_value, na.rm = TRUE),
    max_value = max(.aq_value, na.rm = TRUE)
  ), by = .(.aq_stage_chr, .aq_stage_order)]
  data.table::setnames(stage_summary, c(".aq_stage_chr", ".aq_stage_order"), c("stage", "stage_order"))
  artifact <- new_table_artifact(
    id = forecast_id,
    title = paste("Funnel Forecast:", tools::toTitleCase(spec$strategy), "Strategy"),
    data = forecast_dt,
    source_generator = "aq_fit_funnel_forecast",
    tags = c("vnext", "forecast", "funnel", spec$strategy),
    dependencies = character(),
    version = "aq_funnel_forecast_artifact_v1",
    metadata = list(
      artifact_type = "funnel_forecast",
      forecast_id = forecast_id,
      forecast_spec_id = spec$forecast_spec_id,
      stages = spec$stages,
      transitions = spec$transitions,
      strategy = spec$strategy,
      forecast_origin = origin,
      horizon = spec$horizon,
      maturity_summary = maturity_summary,
      transition_diagnostics = transition_diagnostics,
      stage_summary = stage_summary,
      supported_downstream_actions = spec$supported_downstream_actions
    )
  )
  artifact <- aq_vnext_attach_envelope(
    artifact,
    artifact_id = forecast_id,
    artifact_type = "funnel_forecast",
    artifact_version = "aq_funnel_forecast_artifact_v1",
    lineage = list(
      forecast_id = forecast_id,
      forecast_spec_id = spec$forecast_spec_id,
      stages = spec$stages,
      transitions = spec$transitions,
      strategy = spec$strategy,
      forecast_origin = origin,
      horizon = spec$horizon
    ),
    task = "funnel_forecast",
    operator = "forecast",
    engine = paste0("deterministic_", spec$strategy),
    specification_id = spec$forecast_spec_id,
    dataset_id = spec$dataset_id,
    supported_actions = spec$supported_downstream_actions,
    producer = "aq_fit_funnel_forecast"
  )
  result <- list(
    forecast_id = forecast_id,
    schema_version = "aq_funnel_forecast_result_v1",
    task = "funnel_forecast",
    strategy = spec$strategy,
    forecast_spec_id = spec$forecast_spec_id,
    spec = spec,
    forecast_origin = origin,
    horizon = spec$horizon,
    data = forecast_dt,
    stage_summary = stage_summary,
    transition_diagnostics = transition_diagnostics,
    maturity_summary = maturity_summary,
    artifact = artifact,
    comparison_ready = TRUE,
    warnings = validation[status == "warning", message],
    created_at = aq_vnext_now()
  )
  class(result) <- c("aq_funnel_forecast_result", "list")
  result
}

aq_funnel_conversion_accuracy <- function(data) {
  dt <- data.table::as.data.table(data.table::copy(data))
  wide <- data.table::dcast(dt, cohort + forecast_date + horizon ~ stage, value.var = c("actual", "forecast"), fun.aggregate = sum, fill = NA_real_)
  stage_names <- unique(dt$stage[order(dt$stage_order)])
  if (length(stage_names) < 2L) {
    return(data.table::data.table())
  }
  rows <- lapply(seq_len(length(stage_names) - 1L), function(i) {
    from <- stage_names[i]
    to <- stage_names[i + 1L]
    afrom <- paste0("actual_", from)
    ato <- paste0("actual_", to)
    ffrom <- paste0("forecast_", from)
    fto <- paste0("forecast_", to)
    if (!all(c(afrom, ato, ffrom, fto) %in% names(wide))) return(data.table::data.table())
    tmp <- wide[, .(
      actual_conversion = data.table::fifelse(get(afrom) > 0, get(ato) / get(afrom), NA_real_),
      forecast_conversion = data.table::fifelse(get(ffrom) > 0, get(fto) / get(ffrom), NA_real_)
    )]
    tmp[, transition_id := paste(from, to, sep = "_to_")]
    tmp[, .(
      transition_id = transition_id[1L],
      conversion_mae = mean(abs(actual_conversion - forecast_conversion), na.rm = TRUE),
      conversion_bias = mean(actual_conversion - forecast_conversion, na.rm = TRUE),
      n = sum(stats::complete.cases(actual_conversion, forecast_conversion))
    )]
  })
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

#' Assess Funnel Forecast Evidence
#'
#' @param forecast An `aq_funnel_forecast_result`.
#'
#' @return An `aq_funnel_forecast_assessment_result`.
#' @export
aq_assess_funnel_forecast <- function(forecast) {
  if (!inherits(forecast, "aq_funnel_forecast_result")) {
    stop("forecast must be an aq_funnel_forecast_result.", call. = FALSE)
  }
  if (!"actual" %in% names(forecast$data) || all(is.na(forecast$data$actual))) {
    stop("funnel forecast result does not contain realized actuals for assessment.", call. = FALSE)
  }
  metrics <- aq_forecast_metrics(forecast$data)
  by_stage_long <- forecast$data[, aq_forecast_metrics(.SD), by = .(stage, stage_order)]
  by_stage <- data.table::dcast(by_stage_long, stage + stage_order ~ metric, value.var = "value")
  by_horizon_long <- forecast$data[, aq_forecast_metrics(.SD), by = horizon]
  by_horizon <- data.table::dcast(by_horizon_long, horizon ~ metric, value.var = "value")
  final_stage <- tail(forecast$spec$stages, 1L)
  aggregate_outcome <- aq_forecast_metrics(forecast$data[stage == final_stage])
  conversion_accuracy <- aq_funnel_conversion_accuracy(forecast$data)
  maturity_accuracy <- forecast$data[, .(
    maturity_n = sum(!is.na(maturity)),
    mean_maturity = mean(maturity, na.rm = TRUE),
    maturity_coverage = mean(!is.na(maturity))
  ), by = horizon]
  assessment_id <- aq_vnext_id("funnel_forecast_assessment")
  artifact <- new_table_artifact(
    id = assessment_id,
    title = "Funnel Forecast Assessment",
    data = by_stage,
    source_generator = "aq_assess_funnel_forecast",
    tags = c("vnext", "forecast", "funnel", "assessment"),
    dependencies = forecast$forecast_id,
    version = "aq_funnel_forecast_assessment_artifact_v1",
    metadata = list(
      artifact_type = "funnel_forecast_assessment",
      forecast_id = forecast$forecast_id,
      forecast_spec_id = forecast$forecast_spec_id,
      strategy = forecast$strategy,
      metrics = metrics,
      metrics_by_stage = by_stage,
      metrics_by_horizon = by_horizon,
      conversion_accuracy = conversion_accuracy,
      aggregate_outcome_accuracy = aggregate_outcome,
      maturity_accuracy = maturity_accuracy,
      supported_downstream_actions = c("compare", "report", "campaign_review", "knowledge_promotion")
    )
  )
  artifact <- aq_vnext_attach_envelope(
    artifact,
    artifact_id = assessment_id,
    artifact_type = "funnel_forecast_assessment",
    artifact_version = "aq_funnel_forecast_assessment_artifact_v1",
    parent_artifact_ids = forecast$forecast_id,
    lineage = list(
      forecast_id = forecast$forecast_id,
      forecast_spec_id = forecast$forecast_spec_id,
      strategy = forecast$strategy,
      stages = forecast$spec$stages
    ),
    task = "funnel_forecast",
    operator = "forecast_assessment",
    engine = paste0("deterministic_", forecast$strategy),
    specification_id = forecast$forecast_spec_id,
    dataset_id = forecast$spec$dataset_id,
    supported_actions = c("compare", "report", "campaign_review", "knowledge_promotion"),
    producer = "aq_assess_funnel_forecast"
  )
  result <- list(
    assessment_id = assessment_id,
    schema_version = "aq_funnel_forecast_assessment_result_v1",
    forecast_id = forecast$forecast_id,
    forecast_spec_id = forecast$forecast_spec_id,
    strategy = forecast$strategy,
    metrics = metrics,
    metrics_by_stage = by_stage,
    metrics_by_horizon = by_horizon,
    conversion_accuracy = conversion_accuracy,
    aggregate_outcome_accuracy = aggregate_outcome,
    maturity_accuracy = maturity_accuracy,
    assessment_artifact = artifact,
    comparison_ready = TRUE,
    created_at = aq_vnext_now()
  )
  class(result) <- c("aq_funnel_forecast_assessment_result", "list")
  result
}

#' Compare Funnel Forecast Strategies
#'
#' @param spec An `aq_funnel_forecast_spec`.
#' @param data Long-form funnel data.
#' @param origin Optional forecast origin.
#'
#' @return An `aq_funnel_strategy_comparison_result`.
#' @export
aq_compare_funnel_strategies <- function(spec, data, origin = NULL) {
  if (!inherits(spec, "aq_funnel_forecast_spec")) {
    stop("spec must be an aq_funnel_forecast_spec.", call. = FALSE)
  }
  stage_spec <- spec
  transition_spec <- spec
  stage_spec$strategy <- "stage"
  transition_spec$strategy <- "transition"
  stage_forecast <- aq_fit_funnel_forecast(stage_spec, data, origin = origin)
  transition_forecast <- aq_fit_funnel_forecast(transition_spec, data, origin = origin)
  forecasts <- list(stage = stage_forecast, transition = transition_forecast)
  assessments <- lapply(forecasts, function(x) {
    tryCatch(aq_assess_funnel_forecast(x), error = function(e) e)
  })
  summary_rows <- lapply(names(forecasts), function(strategy) {
    assessment <- assessments[[strategy]]
    if (inherits(assessment, "error")) {
      data.table::data.table(strategy = strategy, rmse = NA_real_, mae = NA_real_, bias = NA_real_, assessed = FALSE, error = conditionMessage(assessment))
    } else {
      wide <- data.table::dcast(assessment$metrics, . ~ metric, value.var = "value")
      wide[, . := NULL]
      wide[, `:=`(strategy = strategy, assessed = TRUE, error = NA_character_)]
      wide
    }
  })
  strategy_summary <- data.table::rbindlist(summary_rows, use.names = TRUE, fill = TRUE)
  data.table::setcolorder(strategy_summary, c("strategy", setdiff(names(strategy_summary), "strategy")))
  recommendation <- if (!any(isTRUE(strategy_summary$assessed))) {
    "evidence_insufficient"
  } else {
    assessed <- strategy_summary[assessed == TRUE & is.finite(rmse)]
    if (!nrow(assessed)) "evidence_insufficient" else paste0(assessed[which.min(rmse), strategy], "_strategy_lower_rmse")
  }
  comparison_id <- aq_vnext_id("funnel_strategy_comparison")
  artifact <- new_table_artifact(
    id = comparison_id,
    title = "Funnel Strategy Comparison",
    data = strategy_summary,
    source_generator = "aq_compare_funnel_strategies",
    tags = c("vnext", "forecast", "funnel", "strategy_comparison"),
    dependencies = vapply(forecasts, `[[`, character(1L), "forecast_id"),
    version = "aq_funnel_strategy_comparison_artifact_v1",
    metadata = list(
      artifact_type = "funnel_strategy_comparison",
      comparison_id = comparison_id,
      forecast_spec_id = spec$forecast_spec_id,
      strategy_summary = strategy_summary,
      recommendation = recommendation,
      supported_downstream_actions = c("compare", "report", "campaign_review")
    )
  )
  artifact <- aq_vnext_attach_envelope(
    artifact,
    artifact_id = comparison_id,
    artifact_type = "funnel_strategy_comparison",
    artifact_version = "aq_funnel_strategy_comparison_artifact_v1",
    parent_artifact_ids = vapply(forecasts, `[[`, character(1L), "forecast_id"),
    lineage = list(forecast_spec_id = spec$forecast_spec_id, strategies = names(forecasts)),
    task = "funnel_forecast",
    operator = "strategy_comparison",
    engine = "deterministic_comparison",
    specification_id = spec$forecast_spec_id,
    dataset_id = spec$dataset_id,
    supported_actions = c("compare", "report", "campaign_review"),
    producer = "aq_compare_funnel_strategies"
  )
  result <- list(
    comparison_id = comparison_id,
    schema_version = "aq_funnel_strategy_comparison_result_v1",
    forecast_spec_id = spec$forecast_spec_id,
    forecasts = forecasts,
    assessments = assessments,
    strategy_summary = strategy_summary,
    recommendation = recommendation,
    artifact = artifact,
    comparison_ready = TRUE,
    created_at = aq_vnext_now()
  )
  class(result) <- c("aq_funnel_strategy_comparison_result", "list")
  result
}

aq_vnext_funnel_forecast_fixture <- function() {
  cohorts <- paste0("cohort_", sprintf("%02d", 1:8))
  stages <- c("prospects", "applications", "qualified", "enrollments")
  dates <- seq.Date(as.Date("2026-01-01"), by = "week", length.out = length(cohorts))
  rows <- list()
  for (i in seq_along(cohorts)) {
    base <- 120 + i * 8
    rates <- c(1, 0.62 + i * 0.005, 0.72 - i * 0.003, 0.45 + i * 0.004)
    volume <- base
    for (j in seq_along(stages)) {
      if (j > 1L) volume <- volume * rates[j]
      rows[[length(rows) + 1L]] <- data.table::data.table(
        cohort = cohorts[i],
        date = dates[i],
        stage_date = dates[i] + (j - 1L) * 7L,
        maturity = j - 1L,
        stage = stages[j],
        volume = round(volume)
      )
    }
  }
  extra <- data.table::copy(data.table::rbindlist(rows))[date >= dates[7]]
  extra[, date := date + 14L]
  extra[, maturity := maturity + 2L]
  extra[, volume := round(volume * 1.04)]
  data.table::rbindlist(list(data.table::rbindlist(rows), extra), use.names = TRUE, fill = TRUE)
}

#' QA for vNext Funnel Forecasting Foundation
#'
#' @return A data.table of deterministic QA checks.
#' @export
qa_vnext_funnel_forecasting_foundation <- function() {
  rows <- list()
  add <- function(check, ok, message = "") {
    rows[[length(rows) + 1L]] <<- data.table::data.table(
      suite = "vnext_funnel_forecasting_foundation",
      check = check,
      status = if (isTRUE(ok)) "pass" else "fail",
      message = message
    )
  }
  dt <- aq_vnext_funnel_forecast_fixture()
  stages <- c("prospects", "applications", "qualified", "enrollments")
  spec <- aq_funnel_forecast_spec(
    stages = stages,
    stage = "stage",
    value = "volume",
    date = "date",
    cohort = "cohort",
    stage_date = "stage_date",
    maturity = "maturity",
    frequency = "week",
    horizon = 2,
    forecast_origin = as.Date("2026-02-12"),
    strategy = "stage",
    dataset_id = "qa_funnel_fixture"
  )
  validation <- aq_validate_funnel_forecast_spec(spec, dt)
  add("funnel_spec_constructed", inherits(spec, "aq_funnel_forecast_spec"))
  add("funnel_validation_passes", !aq_vnext_has_validation_error(validation), paste(validation$message, collapse = " | "))
  dup_spec <- aq_funnel_forecast_spec(stages = c("a", "a"), stage = "stage", value = "volume", date = "date", cohort = "cohort")
  dup_validation <- aq_validate_funnel_forecast_spec(dup_spec)
  add("duplicate_stage_validation", any(dup_validation$check == "duplicate_stages" & dup_validation$status == "fail"))
  bad_dt <- data.table::copy(dt)
  bad_dt[1L, volume := -1]
  bad_validation <- aq_validate_funnel_forecast_spec(spec, bad_dt)
  add("negative_volume_validation", any(bad_validation$check == "negative_values" & bad_validation$status == "fail"))
  stage_fit <- aq_fit_funnel_forecast(spec, dt)
  add("stage_forecast_result", inherits(stage_fit, "aq_funnel_forecast_result") && nrow(stage_fit$data) > 0L && identical(stage_fit$strategy, "stage"))
  add("stage_artifact_integrity", !aq_vnext_has_validation_error(aq_validate_artifact(stage_fit$artifact)) && identical(aq_artifact_envelope(stage_fit$artifact)$task, "funnel_forecast"))
  transition_spec <- spec
  transition_spec$strategy <- "transition"
  transition_fit <- aq_fit_funnel_forecast(transition_spec, dt)
  add("transition_forecast_result", inherits(transition_fit, "aq_funnel_forecast_result") && nrow(transition_fit$transition_diagnostics) == length(stages) - 1L)
  add("transition_evidence_observed", all(transition_fit$transition_diagnostics$observations > 0L) && all(is.finite(transition_fit$transition_diagnostics$mean_conversion)))
  add("maturity_summary", nrow(stage_fit$maturity_summary) > 0L && all(c("min_maturity", "max_maturity") %in% names(stage_fit$maturity_summary)))
  stage_assessment <- aq_assess_funnel_forecast(stage_fit)
  transition_assessment <- aq_assess_funnel_forecast(transition_fit)
  add("stage_assessment", inherits(stage_assessment, "aq_funnel_forecast_assessment_result") && nrow(stage_assessment$metrics_by_stage) > 0L)
  add("transition_assessment", inherits(transition_assessment, "aq_funnel_forecast_assessment_result") && nrow(transition_assessment$conversion_accuracy) > 0L)
  comparison <- aq_compare_funnel_strategies(spec, dt)
  add("strategy_comparison", inherits(comparison, "aq_funnel_strategy_comparison_result") && all(c("stage", "transition") %in% comparison$strategy_summary$strategy))
  add("comparison_artifact_integrity", !aq_vnext_has_validation_error(aq_validate_artifact(comparison$artifact)) && identical(aq_artifact_envelope(comparison$artifact)$operator, "strategy_comparison"))
  app_like <- list(stage_fit$artifact, stage_assessment$assessment_artifact, comparison$artifact)
  add("analytics_shiny_app_compatibility", all(vapply(app_like, function(x) inherits(x, "aq_artifact") && !is.null(x$artifact_envelope), logical(1L))))
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}
