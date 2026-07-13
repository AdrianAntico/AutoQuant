# AutoQuant vNext time-series forecasting foundation.

aq_forecast_frequency_levels <- function() {
  c("auto", "day", "week", "month", "quarter", "year")
}

aq_forecast_engine_levels <- function() {
  c("naive", "seasonal_naive", "ets", "arima", "catboost")
}

aq_forecast_strategy_levels <- function() {
  c("direct", "recursive")
}

aq_forecast_window_levels <- function() {
  c("expanding", "fixed")
}

aq_forecast_metric_levels <- function() {
  c("rmse", "mae", "mape", "smape", "bias")
}

aq_forecast_future_regressor_policy_levels <- function() {
  c("require_complete", "diagnostic_only")
}

aq_forecast_engine_supports_intervals <- function(engine) {
  engine %in% c("ets", "arima")
}

aq_forecast_engine_supports_xreg <- function(engine) {
  engine %in% c("arima", "catboost")
}

aq_forecast_detect_frequency <- function(dates) {
  dates <- sort(unique(as.Date(dates[!is.na(dates)])))
  if (length(dates) < 3L) {
    return(list(frequency = NA_character_, interval = NA_real_, confidence = "insufficient_history"))
  }
  diffs <- as.numeric(diff(dates))
  median_diff <- stats::median(diffs, na.rm = TRUE)
  frequency <- if (isTRUE(abs(median_diff - 1) <= 0.1)) {
    "day"
  } else if (isTRUE(abs(median_diff - 7) <= 0.5)) {
    "week"
  } else if (isTRUE(median_diff >= 27 && median_diff <= 32)) {
    "month"
  } else if (isTRUE(median_diff >= 80 && median_diff <= 100)) {
    "quarter"
  } else if (isTRUE(median_diff >= 360 && median_diff <= 370)) {
    "year"
  } else {
    "irregular"
  }
  list(
    frequency = frequency,
    interval = median_diff,
    confidence = if (identical(frequency, "irregular")) "low" else "deterministic"
  )
}

aq_forecast_season_length <- function(frequency, season_length = NULL) {
  if (!is.null(season_length)) {
    return(as.integer(season_length)[1L])
  }
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

aq_forecast_next_dates <- function(origin, frequency, horizon) {
  origin <- as.Date(origin)
  horizon <- as.integer(horizon)[1L]
  if (!is.finite(horizon) || horizon < 1L) {
    return(as.Date(character()))
  }
  if (identical(frequency, "week")) {
    return(origin + 7L * seq_len(horizon))
  }
  if (identical(frequency, "month")) {
    return(seq(origin, by = "month", length.out = horizon + 1L)[-1L])
  }
  if (identical(frequency, "quarter")) {
    return(seq(origin, by = "quarter", length.out = horizon + 1L)[-1L])
  }
  if (identical(frequency, "year")) {
    return(seq(origin, by = "year", length.out = horizon + 1L)[-1L])
  }
  origin + seq_len(horizon)
}

aq_forecast_expected_dates <- function(min_date, max_date, frequency) {
  min_date <- as.Date(min_date)
  max_date <- as.Date(max_date)
  if (identical(frequency, "week")) {
    return(seq(min_date, max_date, by = "week"))
  }
  if (identical(frequency, "month")) {
    return(seq(min_date, max_date, by = "month"))
  }
  if (identical(frequency, "quarter")) {
    return(seq(min_date, max_date, by = "quarter"))
  }
  if (identical(frequency, "year")) {
    return(seq(min_date, max_date, by = "year"))
  }
  seq(min_date, max_date, by = "day")
}

#' Create a vNext Forecast Specification
#'
#' @param target Target value column.
#' @param date Date/time column.
#' @param frequency Series frequency. Supports `"auto"`, `"day"`, `"week"`,
#'   `"month"`, `"quarter"`, and `"year"`.
#' @param horizon Forecast horizon in periods.
#' @param forecast_origin Optional forecast origin date.
#' @param future_known_variables Variables known at forecast time.
#' @param future_unknown_variables Variables not known at forecast time.
#' @param engine Forecast engine. Supports `"naive"`, `"seasonal_naive"`,
#'   `"ets"`, `"arima"`, and `"catboost"`.
#' @param forecast_strategy Forecast strategy for CatBoost. Supports `"direct"`
#'   and `"recursive"`.
#' @param metrics Forecast metrics.
#' @param engine_parameters Optional deterministic engine parameter list. ETS
#'   supports `method`, `alpha`, `beta`, and `gamma`. ARIMA supports `order`,
#'   `seasonal_order`, and `include_mean`.
#' @param prediction_intervals Whether to request prediction interval evidence.
#' @param confidence_level Prediction interval confidence level.
#' @param future_regressor_policy Validation policy for known future regressors.
#'   Supports `"require_complete"` and `"diagnostic_only"`.
#' @param rolling_validation Whether rolling-origin validation is expected.
#' @param rolling_origins Number of rolling origins for QA/backtesting helpers.
#' @param rolling_window Rolling training window policy. Supports `"expanding"`
#'   and `"fixed"`.
#' @param training_window Optional fixed training-window length.
#' @param season_length Optional seasonal lag length.
#' @param aggregation_level Optional aggregation label.
#' @param forecast_spec_id Optional specification id.
#' @param dataset_id Optional dataset id.
#' @param supported_downstream_actions Supported next actions.
#'
#' @return An `aq_forecast_spec` object.
#' @export
aq_forecast_spec <- function(
  target,
  date,
  frequency = "auto",
  horizon = 1L,
  forecast_origin = NULL,
  future_known_variables = character(),
  future_unknown_variables = character(),
  engine = "naive",
  forecast_strategy = "direct",
  metrics = c("rmse", "mae", "mape", "smape", "bias"),
  engine_parameters = list(),
  prediction_intervals = TRUE,
  confidence_level = 0.95,
  future_regressor_policy = "require_complete",
  rolling_validation = TRUE,
  rolling_origins = 3L,
  rolling_window = "expanding",
  training_window = NULL,
  season_length = NULL,
  aggregation_level = NULL,
  forecast_spec_id = NULL,
  dataset_id = NULL,
  supported_downstream_actions = c("forecast", "assess", "compare", "report", "campaign_review")
) {
  frequency <- match.arg(tolower(frequency), aq_forecast_frequency_levels())
  engine <- match.arg(tolower(engine), aq_forecast_engine_levels())
  forecast_strategy <- match.arg(tolower(forecast_strategy), aq_forecast_strategy_levels())
  future_regressor_policy <- match.arg(tolower(future_regressor_policy), aq_forecast_future_regressor_policy_levels())
  rolling_window <- match.arg(tolower(rolling_window), aq_forecast_window_levels())
  horizon <- as.integer(horizon)[1L]
  confidence_level <- as.numeric(confidence_level)[1L]
  if (is.null(forecast_spec_id)) {
    forecast_spec_id <- aq_vnext_id(paste("forecast_spec", engine, target, sep = "_"))
  }
  spec <- list(
    forecast_spec_id = as.character(forecast_spec_id)[1L],
    schema_version = "aq_forecast_spec_v1",
    target = as.character(target)[1L],
    date = as.character(date)[1L],
    frequency = frequency,
    horizon = horizon,
    forecast_origin = if (is.null(forecast_origin)) NULL else as.Date(forecast_origin)[1L],
    future_known_variables = aq_vnext_unique_chr(future_known_variables),
    future_unknown_variables = aq_vnext_unique_chr(future_unknown_variables),
    engine = engine,
    forecast_strategy = forecast_strategy,
    engine_parameters = if (is.null(engine_parameters)) list() else engine_parameters,
    prediction_intervals = isTRUE(prediction_intervals),
    confidence_level = confidence_level,
    future_regressor_policy = future_regressor_policy,
    metrics = intersect(aq_vnext_unique_chr(metrics), aq_forecast_metric_levels()),
    rolling_validation = isTRUE(rolling_validation),
    rolling_origins = as.integer(rolling_origins)[1L],
    rolling_window = rolling_window,
    training_window = if (is.null(training_window)) NULL else as.integer(training_window)[1L],
    season_length = if (is.null(season_length)) NULL else as.integer(season_length)[1L],
    aggregation_level = if (is.null(aggregation_level)) NA_character_ else as.character(aggregation_level)[1L],
    dataset_id = aq_vnext_default(dataset_id, NA_character_),
    supported_downstream_actions = aq_vnext_unique_chr(supported_downstream_actions),
    created_at = aq_vnext_now()
  )
  class(spec) <- c("aq_forecast_spec", "list")
  spec
}

#' Validate a vNext Forecast Specification
#'
#' @param spec An `aq_forecast_spec`.
#' @param data Optional time-series data.
#'
#' @return A `data.table` of deterministic validation diagnostics.
#' @export
aq_validate_forecast_spec <- function(spec, data = NULL) {
  rows <- list()
  add <- function(check, status, message, severity = status) {
    rows[[length(rows) + 1L]] <<- aq_vnext_validation_table(check, status, message, severity)
  }
  if (!inherits(spec, "aq_forecast_spec")) {
    add("forecast_spec_class", "fail", "spec must be created by aq_forecast_spec().")
    return(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
  }
  add("forecast_spec_class", "pass", "forecast specification is typed.", "info")
  if (!is.finite(spec$horizon) || spec$horizon < 1L) {
    add("horizon", "fail", "forecast horizon must be a positive integer.")
  } else {
    add("horizon", "pass", paste("horizon:", spec$horizon), "info")
  }
  if (!spec$engine %in% aq_forecast_engine_levels()) {
    add("engine_supported", "fail", paste("unsupported forecast engine:", spec$engine))
  } else {
    add("engine_supported", "pass", paste("engine:", spec$engine), "info")
  }
  if (!spec$forecast_strategy %in% aq_forecast_strategy_levels()) {
    add("forecast_strategy", "fail", paste("unsupported forecast strategy:", spec$forecast_strategy))
  } else if (identical(spec$engine, "catboost")) {
    add("forecast_strategy", "pass", paste("CatBoost forecast strategy:", spec$forecast_strategy), "info")
  } else {
    add("forecast_strategy", "pass", "forecast strategy is recorded for compatibility.", "info")
  }
  if (!is.finite(spec$confidence_level) || spec$confidence_level <= 0 || spec$confidence_level >= 1) {
    add("confidence_level", "fail", "confidence_level must be a scalar value between 0 and 1.")
  } else {
    add("confidence_level", "pass", paste("confidence level:", spec$confidence_level), "info")
  }
  if (isTRUE(spec$prediction_intervals) && !aq_forecast_engine_supports_intervals(spec$engine)) {
    add("interval_support", "warning", paste("prediction intervals are not supported by engine:", spec$engine), "warning")
  } else if (isTRUE(spec$prediction_intervals)) {
    add("interval_support", "pass", paste("prediction intervals requested for engine:", spec$engine), "info")
  } else {
    add("interval_support", "pass", "prediction intervals were not requested.", "info")
  }
  if (length(spec$future_known_variables) && !aq_forecast_engine_supports_xreg(spec$engine)) {
    add("future_regressor_support", "warning", paste("known future regressors are recorded but not used by engine:", spec$engine), "warning")
  } else if (length(spec$future_known_variables)) {
    add("future_regressor_support", "pass", paste("known future regressors supported by engine:", spec$engine), "info")
  } else {
    add("future_regressor_support", "pass", "no known future regressors declared.", "info")
  }
  if (length(intersect(spec$future_known_variables, spec$future_unknown_variables))) {
    add("future_variable_overlap", "fail", "future known and future unknown variables overlap.")
  } else {
    add("future_variable_overlap", "pass", "future variable roles do not overlap.", "info")
  }
  if (is.null(data)) {
    return(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
  }
  dt <- data.table::as.data.table(data)
  missing_cols <- setdiff(c(spec$target, spec$date, spec$future_known_variables, spec$future_unknown_variables), names(dt))
  if (length(missing_cols)) {
    add("required_columns", "fail", paste("Missing required column(s):", paste(missing_cols, collapse = ", ")))
    return(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
  }
  add("required_columns", "pass", "required columns are present.", "info")
  if (!inherits(dt[[spec$date]], c("Date", "POSIXct", "POSIXt"))) {
    converted <- suppressWarnings(as.Date(dt[[spec$date]]))
    if (all(is.na(converted))) {
      add("date_type", "fail", "date column cannot be converted to Date.")
      return(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
    }
  }
  dates <- as.Date(dt[[spec$date]])
  duplicate_count <- sum(duplicated(dates[!is.na(dates)]))
  if (duplicate_count) {
    add("duplicate_timestamps", "fail", paste("Duplicate timestamp rows:", duplicate_count))
  } else {
    add("duplicate_timestamps", "pass", "timestamps are unique.", "info")
  }
  if (!is.numeric(dt[[spec$target]])) {
    add("target_type", "fail", "forecast target must be numeric.")
  } else {
    add("target_type", "pass", "forecast target is numeric.", "info")
  }
  detection <- aq_forecast_detect_frequency(dates)
  resolved_frequency <- if (identical(spec$frequency, "auto")) detection$frequency else spec$frequency
  if (is.na(resolved_frequency) || identical(resolved_frequency, "irregular")) {
    add("frequency_detection", "warning", paste("Frequency is irregular or unavailable; median interval:", detection$interval), "warning")
  } else {
    add("frequency_detection", "pass", paste("frequency:", resolved_frequency), "info")
  }
  if (!identical(spec$frequency, "auto") && !is.na(detection$frequency) && !identical(detection$frequency, "irregular") && !identical(spec$frequency, detection$frequency)) {
    add("frequency_override", "warning", paste("frequency override differs from detected frequency:", detection$frequency), "warning")
  } else {
    add("frequency_override", "pass", "frequency override is compatible or automatic.", "info")
  }
  if (!is.na(resolved_frequency) && !identical(resolved_frequency, "irregular")) {
    expected <- aq_forecast_expected_dates(min(dates, na.rm = TRUE), max(dates, na.rm = TRUE), resolved_frequency)
    missing_timestamps <- setdiff(expected, unique(dates))
    if (length(missing_timestamps)) {
      add("missing_timestamps", "warning", paste("Missing timestamp periods:", length(missing_timestamps)), "warning")
    } else {
      add("missing_timestamps", "pass", "no missing timestamp periods detected.", "info")
    }
  }
  engine_parameter_diagnostics <- aq_forecast_engine_parameter_diagnostics(spec, resolved_frequency)
  rows <- c(rows, split(engine_parameter_diagnostics, seq_len(nrow(engine_parameter_diagnostics))))
  min_history <- aq_forecast_engine_min_history(spec, resolved_frequency)
  if (sum(!is.na(dt[[spec$target]])) < min_history) {
    add("insufficient_history", "fail", paste("Insufficient non-missing history. Required at least", min_history, "rows."))
  } else {
    add("insufficient_history", "pass", "sufficient history is available.", "info")
  }
  if (!is.null(spec$forecast_origin) && spec$forecast_origin < min(dates, na.rm = TRUE)) {
    add("forecast_origin", "fail", "forecast origin is before the available history.")
  } else {
    add("forecast_origin", "pass", "forecast origin is compatible with available history.", "info")
  }
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

aq_forecast_resolved_frequency <- function(spec, data) {
  if (!identical(spec$frequency, "auto")) {
    return(spec$frequency)
  }
  detected <- aq_forecast_detect_frequency(as.Date(data[[spec$date]]))$frequency
  if (is.na(detected) || identical(detected, "irregular")) "day" else detected
}

aq_forecast_engine_min_history <- function(spec, frequency) {
  season_length <- aq_forecast_season_length(frequency, spec$season_length)
  if (identical(spec$engine, "seasonal_naive")) {
    return(spec$horizon + season_length)
  }
  if (identical(spec$engine, "ets")) {
    return(max(6L, if (season_length > 1L) 2L * season_length else 6L))
  }
  if (identical(spec$engine, "arima")) {
    params <- aq_vnext_default(spec$engine_parameters, list())
    order <- as.integer(aq_vnext_default(params$order, c(1L, 0L, 0L)))
    seasonal_order <- as.integer(aq_vnext_default(params$seasonal_order, c(0L, 0L, 0L)))
    return(max(10L, sum(order, seasonal_order, na.rm = TRUE) + 5L))
  }
  if (identical(spec$engine, "catboost")) {
    params <- aq_vnext_default(spec$engine_parameters, list())
    lag_periods <- as.integer(aq_vnext_default(params$lag_periods, c(1L, season_length)))
    rolling_windows <- as.integer(aq_vnext_default(params$rolling_windows, season_length))
    max_lag <- max(c(lag_periods, rolling_windows, spec$horizon), na.rm = TRUE)
    return(max(20L, max_lag + spec$horizon + 5L))
  }
  2L
}

aq_forecast_engine_parameter_diagnostics <- function(spec, frequency) {
  rows <- list()
  add <- function(check, status, message, severity = status) {
    rows[[length(rows) + 1L]] <<- aq_vnext_validation_table(check, status, message, severity)
  }
  params <- aq_vnext_default(spec$engine_parameters, list())
  if (identical(spec$engine, "ets")) {
    method <- as.character(aq_vnext_default(params$method, "simple"))[1L]
    if (!method %in% c("simple", "trend", "seasonal")) {
      add("ets_method", "fail", "ETS method must be one of: simple, trend, seasonal.")
    } else if (identical(method, "seasonal") && aq_forecast_season_length(frequency, spec$season_length) <= 1L) {
      add("ets_method", "fail", "seasonal ETS requires a seasonal frequency or explicit season_length greater than 1.")
    } else {
      add("ets_method", "pass", paste("ETS method:", method), "info")
    }
    numeric_params <- intersect(names(params), c("alpha", "beta", "gamma"))
    bad <- numeric_params[!vapply(params[numeric_params], function(x) is.numeric(x) && length(x) == 1L && is.finite(x) && x >= 0 && x <= 1, logical(1L))]
    if (length(bad)) {
      add("ets_smoothing_parameters", "fail", paste("ETS smoothing parameters must be scalar values in [0, 1]:", paste(bad, collapse = ", ")))
    } else {
      add("ets_smoothing_parameters", "pass", "ETS smoothing parameters are compatible.", "info")
    }
  } else if (identical(spec$engine, "arima")) {
    order <- aq_vnext_default(params$order, c(1L, 0L, 0L))
    seasonal_order <- aq_vnext_default(params$seasonal_order, c(0L, 0L, 0L))
    valid_order <- function(x) length(x) == 3L && all(is.finite(as.numeric(x))) && all(as.integer(x) >= 0)
    if (!valid_order(order)) {
      add("arima_order", "fail", "ARIMA order must be a non-negative integer vector of length 3.")
    } else {
      add("arima_order", "pass", paste("ARIMA order:", paste(as.integer(order), collapse = ",")), "info")
    }
    if (!valid_order(seasonal_order)) {
      add("arima_seasonal_order", "fail", "ARIMA seasonal_order must be a non-negative integer vector of length 3.")
    } else {
      add("arima_seasonal_order", "pass", paste("ARIMA seasonal order:", paste(as.integer(seasonal_order), collapse = ",")), "info")
    }
  } else if (identical(spec$engine, "catboost")) {
    supported <- c(
      "iterations", "depth", "learning_rate", "loss_function", "eval_metric",
      "random_seed", "thread_count", "verbose", "task_type", "l2_leaf_reg",
      "random_strength", "bootstrap_type", "allow_writing_files", "lag_periods",
      "rolling_windows", "date_features"
    )
    unknown <- setdiff(names(params), supported)
    if (length(unknown)) {
      add("catboost_parameters", "fail", paste("unsupported CatBoost forecast parameter(s):", paste(unknown, collapse = ", ")))
    } else {
      add("catboost_parameters", "pass", "CatBoost forecast parameters are recognized.", "info")
    }
    positive_integer_param <- function(name) {
      value <- params[[name]]
      is.null(value) || (is.numeric(value) && length(value) >= 1L && all(is.finite(value)) && all(as.integer(value) >= 1L))
    }
    if (!positive_integer_param("lag_periods")) {
      add("catboost_lag_periods", "fail", "CatBoost lag_periods must be positive integers.")
    } else {
      add("catboost_lag_periods", "pass", "CatBoost lag periods are compatible.", "info")
    }
    if (!positive_integer_param("rolling_windows")) {
      add("catboost_rolling_windows", "fail", "CatBoost rolling_windows must be positive integers.")
    } else {
      add("catboost_rolling_windows", "pass", "CatBoost rolling windows are compatible.", "info")
    }
    date_features <- aq_vnext_unique_chr(aq_vnext_default(params$date_features, c("year", "month", "day", "dow", "day_index")))
    bad_date_features <- setdiff(date_features, c("year", "month", "day", "dow", "week", "quarter", "is_weekend", "day_index"))
    if (length(bad_date_features)) {
      add("catboost_date_features", "fail", paste("unsupported CatBoost date feature(s):", paste(bad_date_features, collapse = ", ")))
    } else {
      add("catboost_date_features", "pass", paste("CatBoost date features:", paste(date_features, collapse = ", ")), "info")
    }
  } else {
    add("engine_parameters", "pass", "no engine-specific parameters required.", "info")
  }
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

#' Create a vNext Forecast Partition
#'
#' @param spec An `aq_forecast_spec`.
#' @param data Time-series data.
#' @param origin Optional forecast origin override.
#'
#' @return An `aq_forecast_partition` object.
#' @export
aq_forecast_partition <- function(spec, data, origin = NULL) {
  if (!inherits(spec, "aq_forecast_spec")) {
    stop("spec must be an aq_forecast_spec.", call. = FALSE)
  }
  dt <- data.table::as.data.table(data.table::copy(data))
  dt[, .aq_forecast_date := as.Date(get(spec$date))]
  data.table::setorder(dt, .aq_forecast_date)
  unique_dates <- sort(unique(dt$.aq_forecast_date))
  if (is.null(origin)) {
    origin <- aq_vnext_default(spec$forecast_origin, unique_dates[max(1L, length(unique_dates) - spec$horizon)])
  }
  origin <- as.Date(origin)[1L]
  frequency <- aq_forecast_resolved_frequency(spec, dt)
  future_dates <- aq_forecast_next_dates(origin, frequency, spec$horizon)
  train_idx <- which(dt$.aq_forecast_date <= origin)
  if (!is.null(spec$training_window) && identical(spec$rolling_window, "fixed") && length(train_idx) > spec$training_window) {
    train_idx <- tail(train_idx, spec$training_window)
  }
  eval_idx <- which(dt$.aq_forecast_date %in% future_dates)
  partition <- list(
    partition_id = aq_vnext_id("forecast_partition"),
    schema_version = "aq_forecast_partition_v1",
    forecast_origin = origin,
    horizon = spec$horizon,
    frequency = frequency,
    future_dates = future_dates,
    train_index = train_idx,
    evaluation_index = eval_idx,
    training_window = spec$training_window,
    rolling_window = spec$rolling_window,
    summary = data.table::data.table(
      segment = c("training", "evaluation"),
      rows = c(length(train_idx), length(eval_idx)),
      start_date = c(min(dt$.aq_forecast_date[train_idx], na.rm = TRUE), if (length(eval_idx)) min(dt$.aq_forecast_date[eval_idx], na.rm = TRUE) else as.Date(NA)),
      end_date = c(max(dt$.aq_forecast_date[train_idx], na.rm = TRUE), if (length(eval_idx)) max(dt$.aq_forecast_date[eval_idx], na.rm = TRUE) else as.Date(NA))
    ),
    created_at = aq_vnext_now()
  )
  class(partition) <- c("aq_forecast_partition", "list")
  partition
}

aq_forecast_baseline_values <- function(train, spec, frequency) {
  y <- as.numeric(train[[spec$target]])
  y <- y[!is.na(y)]
  if (!length(y)) {
    stop("training data contains no non-missing target values.", call. = FALSE)
  }
  if (identical(spec$engine, "naive")) {
    return(rep(utils::tail(y, 1L), spec$horizon))
  }
  season_length <- aq_forecast_season_length(frequency, spec$season_length)
  if (length(y) < season_length) {
    return(rep(utils::tail(y, 1L), spec$horizon))
  }
  seasonal_values <- utils::tail(y, season_length)
  rep(seasonal_values, length.out = spec$horizon)
}

aq_forecast_ts_frequency <- function(frequency, season_length = NULL) {
  max(1L, aq_forecast_season_length(frequency, season_length))
}

aq_forecast_residual_diagnostics <- function(residuals) {
  residuals <- as.numeric(residuals)
  residuals <- residuals[is.finite(residuals)]
  if (!length(residuals)) {
    return(list(n = 0L, mean = NA_real_, sd = NA_real_, ljung_box_p_value = NA_real_))
  }
  lag <- min(10L, max(1L, length(residuals) - 1L))
  lb <- tryCatch(stats::Box.test(residuals, lag = lag, type = "Ljung-Box"), error = function(e) NULL)
  list(
    n = length(residuals),
    mean = mean(residuals, na.rm = TRUE),
    sd = stats::sd(residuals, na.rm = TRUE),
    ljung_box_lag = lag,
    ljung_box_p_value = if (is.null(lb)) NA_real_ else unname(lb$p.value)
  )
}

aq_forecast_model_ic <- function(model, n) {
  aic <- suppressWarnings(tryCatch(stats::AIC(model), error = function(e) NA_real_))
  bic <- suppressWarnings(tryCatch(stats::BIC(model), error = function(e) NA_real_))
  k <- suppressWarnings(tryCatch(length(stats::coef(model)), error = function(e) NA_integer_))
  aicc <- if (is.finite(aic) && is.finite(k) && is.finite(n) && n > k + 1L) {
    aic + (2 * k * (k + 1)) / (n - k - 1)
  } else {
    NA_real_
  }
  list(aic = unname(aic), aicc = unname(aicc), bic = unname(bic))
}

aq_forecast_with_warnings <- function(expr) {
  warnings <- character()
  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = unique(warnings))
}

aq_forecast_future_data_context <- function(spec, train, eval, partition, future_data = NULL) {
  rows <- list()
  add <- function(check, status, message, severity = status) {
    rows[[length(rows) + 1L]] <<- aq_vnext_validation_table(check, status, message, severity)
  }
  using <- if (is.null(future_data) && nrow(eval)) "evaluation_window" else if (!is.null(future_data)) "future_data" else "none"
  future_dt <- if (identical(using, "evaluation_window")) {
    data.table::copy(eval)
  } else if (identical(using, "future_data")) {
    data.table::as.data.table(data.table::copy(future_data))
  } else {
    data.table::data.table()
  }
  if (nrow(future_dt) && spec$date %in% names(future_dt)) {
    future_dt[, .aq_forecast_date := as.Date(get(spec$date))]
    future_dt <- future_dt[.aq_forecast_date %in% partition$future_dates]
    data.table::setorder(future_dt, .aq_forecast_date)
  } else if (nrow(future_dt)) {
    future_dt <- head(future_dt, spec$horizon)
    future_dt[, .aq_forecast_date := partition$future_dates[seq_len(.N)]]
  }
  if (!length(spec$future_known_variables)) {
    add("future_regressor_declaration", "pass", "no known future regressors declared.", "info")
    return(list(
      data = future_dt,
      diagnostics = data.table::rbindlist(rows, use.names = TRUE, fill = TRUE),
      xreg_train = NULL,
      xreg_future = NULL,
      metadata = list(
        future_data_source = using,
        future_known_variables = character(),
        future_rows = nrow(future_dt),
        xreg_used = FALSE,
        xreg_supported = aq_forecast_engine_supports_xreg(spec$engine)
      )
    ))
  }
  add("future_regressor_declaration", "pass", paste("known future regressors:", paste(spec$future_known_variables, collapse = ", ")), "info")
  if (!aq_forecast_engine_supports_xreg(spec$engine)) {
    add("future_regressor_engine_support", "warning", paste("engine does not use known future regressors:", spec$engine), "warning")
    return(list(
      data = future_dt,
      diagnostics = data.table::rbindlist(rows, use.names = TRUE, fill = TRUE),
      xreg_train = NULL,
      xreg_future = NULL,
      metadata = list(
        future_data_source = using,
        future_known_variables = spec$future_known_variables,
        future_rows = nrow(future_dt),
        xreg_used = FALSE,
        xreg_supported = FALSE,
        unsupported_reason = paste("engine does not use known future regressors:", spec$engine)
      )
    ))
  }
  missing_train <- setdiff(spec$future_known_variables, names(train))
  missing_future <- setdiff(spec$future_known_variables, names(future_dt))
  if (length(missing_train)) {
    add("future_regressor_training_schema", "fail", paste("training data is missing known-future regressor(s):", paste(missing_train, collapse = ", ")))
  } else {
    add("future_regressor_training_schema", "pass", "training regressor schema is present.", "info")
  }
  if (length(missing_future)) {
    add("future_regressor_future_schema", "fail", paste("future data is missing known-future regressor(s):", paste(missing_future, collapse = ", ")))
  } else {
    add("future_regressor_future_schema", "pass", "future regressor schema is present.", "info")
  }
  if (nrow(future_dt) != spec$horizon) {
    add("future_regressor_horizon", "fail", paste("future data rows must match forecast horizon. Expected", spec$horizon, "got", nrow(future_dt)))
  } else {
    add("future_regressor_horizon", "pass", "future data horizon matches forecast horizon.", "info")
  }
  if (!length(missing_train) && !length(missing_future) && nrow(future_dt)) {
    non_numeric <- spec$future_known_variables[
      !vapply(spec$future_known_variables, function(x) is.numeric(train[[x]]) && is.numeric(future_dt[[x]]), logical(1L))
    ]
    if (length(non_numeric)) {
      add("future_regressor_type", "fail", paste("known-future regressors must be numeric for this engine:", paste(non_numeric, collapse = ", ")))
    } else {
      add("future_regressor_type", "pass", "known-future regressors are numeric.", "info")
    }
    missing_values <- spec$future_known_variables[
      vapply(spec$future_known_variables, function(x) any(is.na(future_dt[[x]])), logical(1L))
    ]
    if (length(missing_values)) {
      add("future_regressor_missing_values", "fail", paste("future regressors contain missing values:", paste(missing_values, collapse = ", ")))
    } else {
      add("future_regressor_missing_values", "pass", "future regressors are complete.", "info")
    }
  }
  diagnostics <- data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
  if (identical(spec$future_regressor_policy, "diagnostic_only") && any(diagnostics$status %in% c("fail", "error"))) {
    diagnostics[status %in% c("fail", "error"), `:=`(status = "warning", severity = "warning")]
  }
  xreg_ready <- !any(diagnostics$status %in% c("fail", "error")) && nrow(future_dt) == spec$horizon
  cols <- spec$future_known_variables
  list(
    data = future_dt,
    diagnostics = diagnostics,
    xreg_train = if (xreg_ready) as.matrix(train[, ..cols]) else NULL,
    xreg_future = if (xreg_ready) as.matrix(future_dt[, ..cols]) else NULL,
    metadata = list(
      future_data_source = using,
      future_known_variables = spec$future_known_variables,
      future_rows = nrow(future_dt),
      xreg_used = xreg_ready,
      xreg_supported = TRUE,
      future_regressor_policy = spec$future_regressor_policy,
      schema_columns = names(future_dt)
    )
  )
}

aq_forecast_interval_columns <- function(prediction, se, confidence_level, method) {
  prediction <- as.numeric(prediction)
  se <- as.numeric(se)
  z <- stats::qnorm((1 + confidence_level) / 2)
  data.table::data.table(
    lower_interval = prediction - z * se,
    upper_interval = prediction + z * se,
    confidence_level = confidence_level,
    interval_available = TRUE,
    interval_method = method,
    unsupported_interval_reason = NA_character_
  )
}

aq_forecast_no_interval_columns <- function(horizon, confidence_level, reason) {
  data.table::data.table(
    lower_interval = rep(NA_real_, horizon),
    upper_interval = rep(NA_real_, horizon),
    confidence_level = confidence_level,
    interval_available = FALSE,
    interval_method = NA_character_,
    unsupported_interval_reason = reason
  )
}

aq_forecast_fit_ets <- function(train, spec, frequency, xreg = NULL) {
  params <- aq_vnext_default(spec$engine_parameters, list())
  method <- as.character(aq_vnext_default(params$method, "simple"))[1L]
  y <- as.numeric(train[[spec$target]])
  y <- y[!is.na(y)]
  ts_y <- stats::ts(y, frequency = aq_forecast_ts_frequency(frequency, spec$season_length))
  alpha <- params$alpha
  beta <- params$beta
  gamma <- params$gamma
  beta_arg <- if (identical(method, "simple")) FALSE else beta
  gamma_arg <- if (identical(method, "seasonal")) gamma else FALSE
  elapsed <- system.time({
    fit <- aq_forecast_with_warnings(stats::HoltWinters(ts_y, alpha = alpha, beta = beta_arg, gamma = gamma_arg))
  })
  model <- fit$value
  if (isTRUE(spec$prediction_intervals)) {
    prediction_raw <- stats::predict(model, n.ahead = spec$horizon, prediction.interval = TRUE, level = spec$confidence_level)
    prediction <- as.numeric(prediction_raw[, "fit"])
    intervals <- data.table::data.table(
      lower_interval = as.numeric(prediction_raw[, "lwr"]),
      upper_interval = as.numeric(prediction_raw[, "upr"]),
      confidence_level = spec$confidence_level,
      interval_available = TRUE,
      interval_method = "stats::HoltWinters prediction.interval",
      unsupported_interval_reason = NA_character_
    )
  } else {
    prediction <- as.numeric(stats::predict(model, n.ahead = spec$horizon))
    intervals <- aq_forecast_no_interval_columns(spec$horizon, spec$confidence_level, "prediction intervals were not requested")
  }
  residuals <- as.numeric(model$x - model$fitted[, "xhat"])
  ic <- aq_forecast_model_ic(model, length(y))
  list(
    prediction = prediction,
    intervals = intervals,
    model = model,
    diagnostics = list(
      engine = "ets",
      method = method,
      model_form = paste0("HoltWinters-", method),
      season_length = aq_forecast_season_length(frequency, spec$season_length),
      aic = ic$aic,
      aicc = ic$aicc,
      bic = ic$bic,
      residual_diagnostics = aq_forecast_residual_diagnostics(residuals),
      convergence = "completed",
      warnings = fit$warnings,
      interval_available = any(intervals$interval_available),
      interval_method = unique(intervals$interval_method[!is.na(intervals$interval_method)]),
      training_duration_seconds = unname(elapsed[["elapsed"]])
    )
  )
}

aq_forecast_fit_arima <- function(train, spec, frequency, xreg = NULL) {
  params <- aq_vnext_default(spec$engine_parameters, list())
  order <- as.integer(aq_vnext_default(params$order, c(1L, 0L, 0L)))
  seasonal_order <- as.integer(aq_vnext_default(params$seasonal_order, c(0L, 0L, 0L)))
  include_mean <- isTRUE(aq_vnext_default(params$include_mean, TRUE))
  y <- as.numeric(train[[spec$target]])
  y <- y[!is.na(y)]
  ts_y <- stats::ts(y, frequency = aq_forecast_ts_frequency(frequency, spec$season_length))
  seasonal <- list(order = seasonal_order, period = aq_forecast_season_length(frequency, spec$season_length))
  elapsed <- system.time({
    fit <- aq_forecast_with_warnings(stats::arima(
      ts_y,
      order = order,
      seasonal = seasonal,
      xreg = xreg$train,
      include.mean = include_mean,
      method = "ML"
    ))
  })
  model <- fit$value
  prediction_raw <- stats::predict(model, n.ahead = spec$horizon, newxreg = xreg$future)
  prediction <- as.numeric(prediction_raw$pred)
  intervals <- if (isTRUE(spec$prediction_intervals)) {
    aq_forecast_interval_columns(prediction, as.numeric(prediction_raw$se), spec$confidence_level, "stats::predict.Arima normal approximation")
  } else {
    aq_forecast_no_interval_columns(spec$horizon, spec$confidence_level, "prediction intervals were not requested")
  }
  ic <- aq_forecast_model_ic(model, length(y))
  list(
    prediction = prediction,
    intervals = intervals,
    model = model,
    diagnostics = list(
      engine = "arima",
      order = order,
      seasonal_order = seasonal_order,
      seasonal_period = seasonal$period,
      include_mean = include_mean,
      aic = ic$aic,
      aicc = ic$aicc,
      bic = ic$bic,
      residual_diagnostics = aq_forecast_residual_diagnostics(stats::residuals(model)),
      convergence = if (!is.null(model$code) && identical(model$code, 0L)) "converged" else "completed",
      warnings = fit$warnings,
      interval_available = any(intervals$interval_available),
      interval_method = unique(intervals$interval_method[!is.na(intervals$interval_method)]),
      xreg_used = !is.null(xreg$train),
      training_duration_seconds = unname(elapsed[["elapsed"]])
    )
  )
}

aq_forecast_catboost_feature_settings <- function(spec, frequency) {
  params <- aq_vnext_default(spec$engine_parameters, list())
  season_length <- aq_forecast_season_length(frequency, spec$season_length)
  lag_periods <- unique(as.integer(aq_vnext_default(params$lag_periods, c(1L, season_length))))
  lag_periods <- lag_periods[is.finite(lag_periods) & lag_periods > 0L]
  if (!length(lag_periods)) {
    lag_periods <- 1L
  }
  rolling_windows <- unique(as.integer(aq_vnext_default(params$rolling_windows, if (season_length > 1L) season_length else 3L)))
  rolling_windows <- rolling_windows[is.finite(rolling_windows) & rolling_windows > 0L]
  date_features <- aq_vnext_unique_chr(aq_vnext_default(params$date_features, c("year", "month", "day", "dow", "day_index")))
  list(
    lag_periods = lag_periods,
    rolling_windows = rolling_windows,
    date_features = date_features,
    season_length = season_length
  )
}

aq_forecast_rodeo_temporal_available <- function() {
  if (!requireNamespace("Rodeo", quietly = TRUE)) return(FALSE)
  required <- c(
    "rodeo_temporal_transformation_spec",
    "rodeo_fit_temporal_transformation",
    "rodeo_prepare_forecast_supervised_data",
    "rodeo_temporal_prediction_frame",
    "rodeo_temporal_transformation_metadata"
  )
  all(required %in% getNamespaceExports("Rodeo"))
}

aq_forecast_require_rodeo_temporal <- function() {
  if (!aq_forecast_rodeo_temporal_available()) {
    stop(
      "CatBoost forecasting requires Rodeo's temporal transformation contract. Rebuild/install Rodeo with the Phase 12 temporal API before fitting engine = 'catboost'.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

aq_forecast_rodeo_temporal_spec <- function(spec, frequency, settings) {
  Rodeo::rodeo_temporal_transformation_spec(
    date_col = spec$date,
    target_col = spec$target,
    frequency = frequency,
    calendar_features = settings$date_features,
    lag_periods = settings$lag_periods,
    rolling_windows = settings$rolling_windows,
    rolling_stats = "mean",
    known_future_variables = spec$future_known_variables,
    forecast_horizon = spec$horizon,
    metadata = list(
      producer = "AutoQuant",
      consumer = "aq_fit_forecast",
      engine = "catboost",
      forecast_strategy = spec$forecast_strategy
    )
  )
}

aq_forecast_rodeo_future_data <- function(spec, partition, future_context) {
  future_dt <- data.table::as.data.table(data.table::copy(aq_vnext_default(future_context$data, data.table::data.table())))
  if (!nrow(future_dt)) {
    future_dt <- data.table::data.table(.aq_forecast_date = partition$future_dates)
  }
  if (!spec$date %in% names(future_dt)) {
    if (".aq_forecast_date" %in% names(future_dt)) {
      future_dt[[spec$date]] <- as.Date(future_dt$.aq_forecast_date)
    } else {
      future_dt[[spec$date]] <- partition$future_dates[seq_len(min(nrow(future_dt), length(partition$future_dates)))]
    }
  }
  future_dt <- future_dt[seq_len(min(nrow(future_dt), spec$horizon))]
  if (nrow(future_dt) < spec$horizon) {
    missing_dates <- partition$future_dates[(nrow(future_dt) + 1L):spec$horizon]
    add <- data.table::data.table(.aq_forecast_date = missing_dates)
    add[[spec$date]] <- missing_dates
    future_dt <- data.table::rbindlist(list(future_dt, add), use.names = TRUE, fill = TRUE)
  }
  future_dt[]
}

aq_forecast_date_feature_frame <- function(dates, date_features, reference_start) {
  dates <- as.Date(dates)
  d <- as.POSIXlt(dates)
  out <- data.table::data.table()
  if ("year" %in% date_features) {
    out[, date_year := d$year + 1900L]
  }
  if ("month" %in% date_features) {
    out[, date_month := d$mon + 1L]
  }
  if ("day" %in% date_features) {
    out[, date_day := d$mday]
  }
  if ("dow" %in% date_features) {
    out[, date_dow := d$wday]
  }
  if ("week" %in% date_features) {
    out[, date_week := as.integer(strftime(dates, "%U"))]
  }
  if ("quarter" %in% date_features) {
    out[, date_quarter := floor((d$mon) / 3L) + 1L]
  }
  if ("is_weekend" %in% date_features) {
    out[, date_is_weekend := as.integer(d$wday %in% c(0L, 6L))]
  }
  if ("day_index" %in% date_features) {
    out[, date_day_index := as.numeric(dates - as.Date(reference_start))]
  }
  out[]
}

aq_forecast_origin_feature_frame <- function(train, spec, settings) {
  y <- as.numeric(train[[spec$target]])
  out <- data.table::data.table(.aq_origin_date = as.Date(train$.aq_forecast_date))
  for (lag in settings$lag_periods) {
    out[[paste0("target_lag_", lag)]] <- data.table::shift(y, n = lag, type = "lag")
  }
  shifted_y <- data.table::shift(y, n = 1L, type = "lag")
  for (window in settings$rolling_windows) {
    out[[paste0("target_roll_mean_", window)]] <- data.table::frollmean(shifted_y, n = window, align = "right", fill = NA_real_)
  }
  out[]
}

aq_forecast_catboost_model_params <- function(spec) {
  params <- aq_vnext_default(spec$engine_parameters, list())
  model_param_names <- c(
    "iterations", "depth", "learning_rate", "loss_function", "eval_metric",
    "random_seed", "thread_count", "verbose", "task_type", "l2_leaf_reg",
    "random_strength", "bootstrap_type", "allow_writing_files"
  )
  model_params <- params[intersect(names(params), model_param_names)]
  if (is.null(model_params$iterations)) {
    model_params$iterations <- 30L
  }
  if (is.null(model_params$depth)) {
    model_params$depth <- 4L
  }
  if (is.null(model_params$learning_rate)) {
    model_params$learning_rate <- 0.1
  }
  aq_vnext_engine_params(model_params, seed = aq_vnext_default(model_params$random_seed, 20260712L), task = "regression")
}

aq_forecast_numeric_matrix <- function(frame, feature_cols) {
  x <- data.table::copy(frame[, ..feature_cols])
  for (col in feature_cols) {
    x[[col]] <- as.numeric(x[[col]])
  }
  as.matrix(x)
}

aq_forecast_catboost_train_one <- function(train_frame, label_col, feature_cols, spec) {
  keep <- stats::complete.cases(train_frame[, c(label_col, feature_cols), with = FALSE])
  train_frame <- train_frame[keep]
  if (nrow(train_frame) < 10L) {
    stop("CatBoost forecast training frame has insufficient complete rows after supervised feature preparation.", call. = FALSE)
  }
  x <- aq_forecast_numeric_matrix(train_frame, feature_cols)
  label <- as.numeric(train_frame[[label_col]])
  pool <- catboost::catboost.load_pool(data = x, label = label)
  params <- aq_forecast_catboost_model_params(spec)
  elapsed <- system.time({
    model <- catboost::catboost.train(learn_pool = pool, test_pool = NULL, params = params)
  })
  feature_importance <- tryCatch({
    importance <- as.numeric(catboost::catboost.get_feature_importance(model, pool = pool))
    data.table::data.table(feature = feature_cols, importance = importance)
  }, error = function(e) {
    data.table::data.table(feature = feature_cols, importance = NA_real_, error = conditionMessage(e))
  })
  list(
    model = model,
    pool = pool,
    feature_importance = feature_importance,
    rows = nrow(train_frame),
    params = params,
    elapsed = unname(elapsed[["elapsed"]])
  )
}

aq_forecast_catboost_predict_one <- function(model, feature_row, feature_cols) {
  pool <- catboost::catboost.load_pool(data = aq_forecast_numeric_matrix(feature_row, feature_cols))
  as.numeric(catboost::catboost.predict(model, pool = pool, prediction_type = "RawFormulaVal"))[1L]
}

aq_forecast_catboost_direct <- function(train, spec, partition, future_context, temporal_fit, future_data) {
  frames <- Rodeo::rodeo_prepare_forecast_supervised_data(
    temporal_fit,
    future_data = future_data,
    horizon = spec$horizon,
    strategy = "direct"
  )
  predictions <- numeric(spec$horizon)
  models <- vector("list", spec$horizon)
  feature_importance <- list()
  feature_cols_by_horizon <- list()
  feature_rows <- list()
  for (h in seq_len(spec$horizon)) {
    frame <- frames$training_frames[[as.character(h)]]
    feature_cols <- frames$feature_columns_by_horizon[[as.character(h)]]
    trained <- aq_forecast_catboost_train_one(frame, ".rodeo_label", feature_cols, spec)
    pred_row <- frames$prediction_frames[[as.character(h)]]
    predictions[h] <- aq_forecast_catboost_predict_one(trained$model, pred_row, feature_cols)
    models[[h]] <- trained$model
    feature_importance[[h]] <- data.table::copy(trained$feature_importance)[, horizon := h]
    feature_cols_by_horizon[[h]] <- feature_cols
    feature_rows[[h]] <- trained$rows
  }
  list(
    prediction = predictions,
    model = list(strategy = "direct", models = models),
    feature_importance = data.table::rbindlist(feature_importance, use.names = TRUE, fill = TRUE),
    feature_cols_by_horizon = feature_cols_by_horizon,
    training_rows_by_horizon = unlist(feature_rows, use.names = FALSE),
    rodeo_supervised_frames = frames
  )
}

aq_forecast_catboost_recursive <- function(train, spec, partition, future_context, temporal_fit, future_data) {
  frames <- Rodeo::rodeo_prepare_forecast_supervised_data(
    temporal_fit,
    future_data = future_data,
    horizon = spec$horizon,
    strategy = "recursive"
  )
  frame <- frames$training_frame
  feature_cols <- frames$feature_columns
  trained <- aq_forecast_catboost_train_one(frame, ".rodeo_label", feature_cols, spec)
  history <- as.numeric(train[[spec$target]])
  history <- history[!is.na(history)]
  predictions <- numeric(spec$horizon)
  for (h in seq_len(spec$horizon)) {
    row <- Rodeo::rodeo_temporal_prediction_frame(temporal_fit, future_data[h], history_values = history)
    predictions[h] <- aq_forecast_catboost_predict_one(trained$model, row, feature_cols)
    history <- c(history, predictions[h])
  }
  list(
    prediction = predictions,
    model = list(strategy = "recursive", model = trained$model),
    feature_importance = trained$feature_importance,
    feature_cols_by_horizon = list(recursive = feature_cols),
    training_rows_by_horizon = trained$rows,
    rodeo_supervised_frames = frames
  )
}

aq_forecast_catboost_lineage <- function(spec, settings, prepared, temporal_fit, temporal_metadata) {
  feature_cols <- unique(unlist(prepared$feature_cols_by_horizon, use.names = FALSE))
  list(
    prepared_feature_id = temporal_metadata$prepared_temporal_dataset_identity,
    rodeo_transformation_id = temporal_metadata$temporal_transformation_identity,
    rodeo_available = TRUE,
    rodeo_contract = list(
      owner = "Rodeo",
      transformation_scope = c("temporal_supervised_forecast_features", "date_features", "lag_features", "rolling_features", "known_future_variables"),
      temporal_contract_available = TRUE,
      temporal_specification_identity = temporal_metadata$temporal_specification_identity,
      temporal_transformation_identity = temporal_metadata$temporal_transformation_identity,
      replay_status = temporal_metadata$replay_status
    ),
    feature_manifest = data.table::copy(temporal_metadata$feature_manifest)[feature %in% feature_cols][, forecast_strategy := spec$forecast_strategy],
    lag_periods = settings$lag_periods,
    rolling_windows = settings$rolling_windows,
    date_features = settings$date_features,
    future_known_variables = spec$future_known_variables
  )
}

aq_forecast_fit_catboost <- function(train, spec, frequency, partition, future_context) {
  if (!requireNamespace("catboost", quietly = TRUE)) {
    stop("The catboost package is required for aq_fit_forecast() with engine = 'catboost'.", call. = FALSE)
  }
  aq_forecast_require_rodeo_temporal()
  settings <- aq_forecast_catboost_feature_settings(spec, frequency)
  temporal_spec <- aq_forecast_rodeo_temporal_spec(spec, frequency, settings)
  temporal_fit <- Rodeo::rodeo_fit_temporal_transformation(
    train,
    temporal_spec,
    forecast_origin = max(train$.aq_forecast_date, na.rm = TRUE)
  )
  temporal_metadata <- Rodeo::rodeo_temporal_transformation_metadata(temporal_fit)
  future_data <- aq_forecast_rodeo_future_data(spec, partition, future_context)
  elapsed <- system.time({
    prepared <- if (identical(spec$forecast_strategy, "recursive")) {
      aq_forecast_catboost_recursive(train, spec, partition, future_context, temporal_fit, future_data)
    } else {
      aq_forecast_catboost_direct(train, spec, partition, future_context, temporal_fit, future_data)
    }
  })
  lineage <- aq_forecast_catboost_lineage(spec, settings, prepared, temporal_fit, temporal_metadata)
  intervals <- aq_forecast_no_interval_columns(
    spec$horizon,
    spec$confidence_level,
    "CatBoost deterministic point forecasts do not provide native prediction intervals in this contract."
  )
  list(
    prediction = prepared$prediction,
    intervals = intervals,
    model = prepared$model,
    diagnostics = list(
      engine = "catboost",
      forecast_strategy = spec$forecast_strategy,
      convergence = "completed",
      warnings = character(),
      interval_available = FALSE,
      unsupported_interval_reason = unique(intervals$unsupported_interval_reason),
      xreg_used = length(spec$future_known_variables) > 0L,
      feature_importance_available = nrow(prepared$feature_importance) > 0L && any(is.finite(prepared$feature_importance$importance)),
      training_rows_by_horizon = prepared$training_rows_by_horizon,
      temporal_replay_status = temporal_metadata$replay_status,
      training_duration_seconds = unname(elapsed[["elapsed"]])
    ),
    catboost_metadata = list(
      forecast_strategy = spec$forecast_strategy,
      prepared_feature_identity = lineage$prepared_feature_id,
      rodeo_transformation_identity = lineage$rodeo_transformation_id,
      temporal_specification_identity = temporal_metadata$temporal_specification_identity,
      temporal_transformation_identity = temporal_metadata$temporal_transformation_identity,
      prepared_temporal_dataset_identity = temporal_metadata$prepared_temporal_dataset_identity,
      temporal_replay_status = temporal_metadata$replay_status,
      temporal_diagnostics = temporal_metadata$diagnostics,
      catboost_model_identity = aq_vnext_id(paste0("catboost_forecast_model_", spec$forecast_strategy)),
      feature_manifest = lineage$feature_manifest,
      feature_importance = prepared$feature_importance,
      feature_importance_available = nrow(prepared$feature_importance) > 0L && any(is.finite(prepared$feature_importance$importance)),
      prepared_feature_lineage = lineage
    )
  )
}

aq_forecast_fit_engine <- function(train, spec, frequency, xreg = NULL) {
  if (spec$engine %in% c("naive", "seasonal_naive")) {
    interval_reason <- if (isTRUE(spec$prediction_intervals)) {
      paste("prediction intervals are not supported by engine:", spec$engine)
    } else {
      "prediction intervals were not requested"
    }
    return(list(
      prediction = aq_forecast_baseline_values(train, spec, frequency),
      intervals = aq_forecast_no_interval_columns(spec$horizon, spec$confidence_level, interval_reason),
      model = NULL,
      diagnostics = list(
        engine = spec$engine,
        model_form = spec$engine,
        season_length = if (identical(spec$engine, "seasonal_naive")) aq_forecast_season_length(frequency, spec$season_length) else NA_integer_,
        convergence = "deterministic_baseline",
        warnings = character(),
        interval_available = FALSE,
        unsupported_interval_reason = interval_reason,
        training_duration_seconds = 0
      )
    ))
  }
  if (identical(spec$engine, "ets")) {
    return(aq_forecast_fit_ets(train, spec, frequency, xreg = xreg))
  }
  if (identical(spec$engine, "arima")) {
    return(aq_forecast_fit_arima(train, spec, frequency, xreg = xreg))
  }
  if (identical(spec$engine, "catboost")) {
    return(aq_forecast_fit_catboost(train, spec, frequency, xreg$partition, xreg$future_context))
  }
  stop(paste("unsupported forecast engine:", spec$engine), call. = FALSE)
}

aq_forecast_baseline_tables <- function(train, forecast_dt, spec, frequency) {
  engines <- c("naive", "seasonal_naive")
  if (identical(frequency, "year")) {
    engines <- "naive"
  }
  if (identical(spec$engine, "catboost")) {
    engines <- unique(c(engines, "ets", "arima"))
  }
  out <- lapply(engines, function(engine) {
    baseline_spec <- spec
    baseline_spec$engine <- engine
    baseline_spec$forecast_strategy <- "direct"
    baseline_spec$future_known_variables <- character()
    baseline_spec$future_unknown_variables <- character()
    baseline_spec$prediction_intervals <- FALSE
    baseline_spec$engine_parameters <- if (identical(engine, "ets")) {
      list(method = "simple")
    } else if (identical(engine, "arima")) {
      list(order = c(1L, 0L, 0L), seasonal_order = c(0L, 0L, 0L), include_mean = TRUE)
    } else {
      list()
    }
    preds <- if (engine %in% c("naive", "seasonal_naive")) {
      aq_forecast_baseline_values(train, baseline_spec, frequency)
    } else {
      tryCatch(
        aq_forecast_fit_engine(train, baseline_spec, frequency, xreg = list(train = NULL, future = NULL))$prediction,
        error = function(e) rep(NA_real_, spec$horizon)
      )
    }
    table <- data.table::copy(forecast_dt)
    table[, `:=`(forecast = as.numeric(preds), baseline_engine = engine)]
    if ("lower_interval" %in% names(table)) {
      table[, `:=`(
        lower_interval = NA_real_,
        upper_interval = NA_real_,
        interval_available = FALSE,
        interval_method = NA_character_,
        unsupported_interval_reason = paste("baseline comparison intervals are not produced for engine:", engine)
      )]
    }
    table
  })
  names(out) <- engines
  out
}

#' Execute a vNext Forecast
#'
#' @param spec An `aq_forecast_spec`.
#' @param data Time-series data.
#' @param origin Optional forecast origin override.
#' @param future_data Optional future data containing known-future regressors.
#'
#' @return An `aq_forecast_result`.
#' @export
aq_fit_forecast <- function(spec, data, origin = NULL, future_data = NULL) {
  validation <- aq_validate_forecast_spec(spec, data)
  if (aq_vnext_has_validation_error(validation)) {
    stop(paste(validation[status %in% c("fail", "error"), message], collapse = " "), call. = FALSE)
  }
  dt <- data.table::as.data.table(data.table::copy(data))
  dt[, .aq_forecast_date := as.Date(get(spec$date))]
  data.table::setorder(dt, .aq_forecast_date)
  partition <- aq_forecast_partition(spec, dt, origin = origin)
  train <- dt[partition$train_index]
  eval <- dt[partition$evaluation_index]
  future_context <- aq_forecast_future_data_context(spec, train, eval, partition, future_data = future_data)
  if (aq_vnext_has_validation_error(future_context$diagnostics)) {
    stop(paste(future_context$diagnostics[status %in% c("fail", "error"), message], collapse = " "), call. = FALSE)
  }
  engine_fit <- aq_forecast_fit_engine(
    train,
    spec,
    partition$frequency,
    xreg = list(
      train = future_context$xreg_train,
      future = future_context$xreg_future,
      partition = partition,
      future_context = future_context
    )
  )
  forecast_dt <- data.table::data.table(
    forecast_date = partition$future_dates,
    horizon = seq_len(spec$horizon),
    forecast = as.numeric(engine_fit$prediction),
    actual = NA_real_
  )
  forecast_dt <- data.table::as.data.table(cbind(forecast_dt, engine_fit$intervals))
  if (nrow(eval)) {
    actuals <- eval[, .(forecast_date = .aq_forecast_date, actual = as.numeric(get(spec$target)))]
    forecast_dt <- merge(forecast_dt[, !"actual"], actuals, by = "forecast_date", all.x = TRUE, sort = FALSE)
    data.table::setorder(forecast_dt, horizon)
  }
  baseline_forecasts <- if (spec$engine %in% c("ets", "arima", "catboost")) {
    aq_forecast_baseline_tables(train, forecast_dt, spec, partition$frequency)
  } else {
    list()
  }
  forecast_id <- aq_vnext_id(paste0("forecast_", spec$engine))
  artifact <- new_table_artifact(
    id = forecast_id,
    title = paste("Forecast:", spec$engine),
    data = forecast_dt,
    source_generator = "aq_fit_forecast",
    tags = c("vnext", "forecast", spec$engine),
    dependencies = spec$forecast_spec_id,
    version = "aq_forecast_artifact_v1",
    metadata = list(
      artifact_type = paste0("time_series_forecast_", spec$engine),
      forecast_id = forecast_id,
      forecast_spec_id = spec$forecast_spec_id,
      partition_id = partition$partition_id,
      target = spec$target,
      date = spec$date,
      engine = spec$engine,
      forecast_strategy = spec$forecast_strategy,
      frequency = partition$frequency,
      forecast_origin = partition$forecast_origin,
      horizon = spec$horizon,
      prediction_intervals_requested = isTRUE(spec$prediction_intervals),
      interval_available = any(forecast_dt$interval_available),
      confidence_level = spec$confidence_level,
      interval_method = unique(forecast_dt$interval_method[!is.na(forecast_dt$interval_method)]),
      unsupported_interval_reason = unique(forecast_dt$unsupported_interval_reason[!is.na(forecast_dt$unsupported_interval_reason)]),
      future_regressor_metadata = future_context$metadata,
      future_regressor_diagnostics = future_context$diagnostics,
      engine_diagnostics = engine_fit$diagnostics,
      catboost_metadata = aq_vnext_default(engine_fit$catboost_metadata, NULL),
      baseline_engines = names(baseline_forecasts),
      supported_downstream_actions = c("assess", "compare", "report", "campaign_review")
    )
  )
  artifact <- aq_vnext_attach_envelope(
    artifact,
    artifact_id = forecast_id,
    artifact_type = paste0("time_series_forecast_", spec$engine),
    artifact_version = "aq_forecast_artifact_v1",
    parent_artifact_ids = spec$forecast_spec_id,
    lineage = list(
      forecast_spec_id = spec$forecast_spec_id,
      partition_id = partition$partition_id,
      forecast_origin = partition$forecast_origin,
      horizon = spec$horizon,
      forecast_strategy = spec$forecast_strategy,
      frequency = partition$frequency,
      training_rows = length(partition$train_index),
      evaluation_rows = length(partition$evaluation_index),
      prediction_intervals_requested = isTRUE(spec$prediction_intervals),
      interval_available = any(forecast_dt$interval_available),
      confidence_level = spec$confidence_level,
      future_regressor_metadata = future_context$metadata,
      engine_diagnostics = engine_fit$diagnostics,
      catboost_metadata = aq_vnext_default(engine_fit$catboost_metadata, NULL),
      baseline_engines = names(baseline_forecasts)
    ),
    task = "time_series_forecast",
    operator = "forecast",
    engine = spec$engine,
    specification_id = spec$forecast_spec_id,
    dataset_id = spec$dataset_id,
    supported_actions = c("assess", "compare", "report", "campaign_review"),
    producer = "aq_fit_forecast"
  )
  result <- list(
    forecast_id = forecast_id,
    status = "success",
    schema_version = "aq_forecast_result_v1",
    spec = spec,
    forecast_spec_id = spec$forecast_spec_id,
    partition = partition,
    partition_id = partition$partition_id,
    task = "time_series_forecast",
    engine = spec$engine,
    forecast_strategy = spec$forecast_strategy,
    target = spec$target,
    date = spec$date,
    frequency = partition$frequency,
    forecast_origin = partition$forecast_origin,
    horizon = spec$horizon,
    data = forecast_dt,
    model = engine_fit$model,
    engine_diagnostics = engine_fit$diagnostics,
    catboost_metadata = aq_vnext_default(engine_fit$catboost_metadata, NULL),
    future_regressor_metadata = future_context$metadata,
    future_regressor_diagnostics = future_context$diagnostics,
    prediction_intervals_requested = isTRUE(spec$prediction_intervals),
    interval_available = any(forecast_dt$interval_available),
    interval_method = unique(forecast_dt$interval_method[!is.na(forecast_dt$interval_method)]),
    confidence_level = spec$confidence_level,
    baseline_forecasts = baseline_forecasts,
    artifact = artifact,
    validation = validation,
    comparison_ready = TRUE,
    warnings = unique(c(validation[status == "warning", message], future_context$diagnostics[status == "warning", message], engine_fit$diagnostics$warnings)),
    supported_downstream_actions = c("assess", "compare", "report", "campaign_review"),
    created_at = aq_vnext_now()
  )
  class(result) <- c("aq_forecast_result", "list")
  result
}

aq_forecast_metrics <- function(data) {
  actual <- as.numeric(data$actual)
  forecast <- as.numeric(data$forecast)
  keep <- stats::complete.cases(actual, forecast)
  actual <- actual[keep]
  forecast <- forecast[keep]
  residual <- actual - forecast
  safe_pct <- function(num, den) ifelse(abs(den) > .Machine$double.eps, num / den, NA_real_)
  data.table::data.table(
    metric = c("rmse", "mae", "mape", "smape", "bias", "n"),
    value = c(
      sqrt(mean(residual^2, na.rm = TRUE)),
      mean(abs(residual), na.rm = TRUE),
      mean(abs(safe_pct(residual, actual)), na.rm = TRUE),
      mean(2 * abs(residual) / pmax(abs(actual) + abs(forecast), .Machine$double.eps), na.rm = TRUE),
      mean(residual, na.rm = TRUE),
      length(residual)
    )
  )
}

aq_forecast_interval_metrics <- function(data) {
  required <- c("actual", "lower_interval", "upper_interval", "interval_available")
  interval_available <- as.logical(data$interval_available)
  interval_available[is.na(interval_available)] <- FALSE
  if (!all(required %in% names(data)) || !any(interval_available)) {
    return(data.table::data.table(
      metric = c("interval_coverage", "average_interval_width", "interval_n"),
      value = c(NA_real_, NA_real_, 0)
    ))
  }
  actual <- as.numeric(data$actual)
  lower <- as.numeric(data$lower_interval)
  upper <- as.numeric(data$upper_interval)
  keep <- stats::complete.cases(actual, lower, upper) & interval_available
  if (!any(keep)) {
    return(data.table::data.table(
      metric = c("interval_coverage", "average_interval_width", "interval_n"),
      value = c(NA_real_, NA_real_, 0)
    ))
  }
  data.table::data.table(
    metric = c("interval_coverage", "average_interval_width", "interval_n"),
    value = c(
      mean(actual[keep] >= lower[keep] & actual[keep] <= upper[keep], na.rm = TRUE),
      mean(upper[keep] - lower[keep], na.rm = TRUE),
      sum(keep)
    )
  )
}

#' Assess vNext Forecast Evidence
#'
#' @param forecast An `aq_forecast_result`.
#'
#' @return An `aq_forecast_assessment_result`.
#' @export
aq_assess_forecast <- function(forecast) {
  if (!inherits(forecast, "aq_forecast_result")) {
    stop("forecast must be an aq_forecast_result.", call. = FALSE)
  }
  if (!"actual" %in% names(forecast$data) || all(is.na(forecast$data$actual))) {
    stop("forecast result does not contain realized actuals for assessment.", call. = FALSE)
  }
  metrics <- aq_forecast_metrics(forecast$data)
  by_horizon <- forecast$data[, aq_forecast_metrics(.SD), by = horizon]
  interval_metrics <- aq_forecast_interval_metrics(forecast$data)
  interval_metrics_by_horizon <- forecast$data[, aq_forecast_interval_metrics(.SD), by = horizon]
  baseline_metrics <- data.table::data.table()
  baseline_comparison <- data.table::data.table()
  if (length(forecast$baseline_forecasts)) {
    baseline_metrics <- data.table::rbindlist(lapply(names(forecast$baseline_forecasts), function(engine) {
      out <- aq_forecast_metrics(forecast$baseline_forecasts[[engine]])
      out[, baseline_engine := engine]
      out
    }), use.names = TRUE, fill = TRUE)
    primary <- data.table::copy(metrics)
    primary[, baseline_engine := forecast$engine]
    comparison_metrics <- data.table::rbindlist(list(primary, baseline_metrics), use.names = TRUE, fill = TRUE)
    baseline_comparison <- data.table::dcast(
      comparison_metrics[metric %in% c("rmse", "mae", "mape", "smape", "bias")],
      metric ~ baseline_engine,
      value.var = "value"
    )
  }
  assessment_id <- aq_vnext_id("forecast_assessment")
  artifact <- list(
    artifact_id = assessment_id,
    artifact_type = "time_series_forecast_assessment",
    schema_version = "aq_forecast_assessment_artifact_v1",
    forecast_id = forecast$forecast_id,
    forecast_spec_id = forecast$forecast_spec_id,
    partition_id = forecast$partition_id,
    engine = forecast$engine,
    forecast_origin = forecast$forecast_origin,
    horizon = forecast$horizon,
    metrics = metrics,
    metrics_by_horizon = by_horizon,
    interval_metrics = interval_metrics,
    interval_metrics_by_horizon = interval_metrics_by_horizon,
    interval_available = isTRUE(forecast$interval_available),
    confidence_level = forecast$confidence_level,
    baseline_metrics = baseline_metrics,
    baseline_comparison = baseline_comparison,
    comparison_ready = TRUE,
    supported_downstream_actions = c("compare", "report", "campaign_review", "knowledge_promotion"),
    created_at = aq_vnext_now()
  )
  artifact <- aq_vnext_attach_envelope(
    artifact,
    parent_artifact_ids = forecast$forecast_id,
    lineage = list(
      forecast_id = forecast$forecast_id,
      forecast_spec_id = forecast$forecast_spec_id,
      partition_id = forecast$partition_id,
      forecast_origin = forecast$forecast_origin,
      horizon = forecast$horizon,
      interval_available = isTRUE(forecast$interval_available),
      confidence_level = forecast$confidence_level,
      compared_baselines = if ("baseline_engine" %in% names(baseline_metrics)) unique(baseline_metrics$baseline_engine) else character()
    ),
    task = "time_series_forecast",
    operator = "forecast_assessment",
    engine = forecast$engine,
    specification_id = forecast$forecast_spec_id,
    dataset_id = forecast$spec$dataset_id,
    supported_actions = c("compare", "report", "campaign_review", "knowledge_promotion"),
    producer = "aq_assess_forecast"
  )
  class(artifact) <- c("aq_forecast_assessment_artifact", "aq_result_artifact", "list")
  result <- list(
    assessment_id = assessment_id,
    status = "success",
    schema_version = "aq_forecast_assessment_result_v1",
    forecast_id = forecast$forecast_id,
    forecast_spec_id = forecast$forecast_spec_id,
    partition_id = forecast$partition_id,
    task = "time_series_forecast",
    engine = forecast$engine,
    metrics = metrics,
    metrics_by_horizon = by_horizon,
    interval_metrics = interval_metrics,
    interval_metrics_by_horizon = interval_metrics_by_horizon,
    interval_available = isTRUE(forecast$interval_available),
    confidence_level = forecast$confidence_level,
    baseline_metrics = baseline_metrics,
    baseline_comparison = baseline_comparison,
    assessment_artifact = artifact,
    comparison_ready = TRUE,
    warnings = character(),
    created_at = aq_vnext_now()
  )
  class(result) <- c("aq_forecast_assessment_result", "list")
  result
}

#' Run Deterministic Rolling-Origin Forecast Evaluation
#'
#' @param spec An `aq_forecast_spec`.
#' @param data Time-series data.
#' @param origins Optional forecast origin dates.
#' @param origin_count Optional number of origins when `origins` is omitted.
#'
#' @return An `aq_forecast_backtest_result`.
#' @export
aq_rolling_origin_forecast <- function(spec, data, origins = NULL, origin_count = NULL) {
  if (!inherits(spec, "aq_forecast_spec")) {
    stop("spec must be an aq_forecast_spec.", call. = FALSE)
  }
  dt <- data.table::as.data.table(data.table::copy(data))
  dt[, .aq_forecast_date := as.Date(get(spec$date))]
  data.table::setorder(dt, .aq_forecast_date)
  unique_dates <- sort(unique(dt$.aq_forecast_date))
  if (is.null(origin_count)) {
    origin_count <- spec$rolling_origins
  }
  if (is.null(origins)) {
    candidate_max <- length(unique_dates) - spec$horizon
    candidate_min <- max(2L, candidate_max - as.integer(origin_count)[1L] + 1L)
    origins <- unique_dates[seq(candidate_min, candidate_max)]
  }
  forecasts <- lapply(as.Date(origins), function(origin) aq_fit_forecast(spec, dt, origin = origin))
  assessments <- lapply(forecasts, aq_assess_forecast)
  forecast_table <- data.table::rbindlist(lapply(forecasts, function(x) {
    out <- data.table::copy(x$data)
    out[, `:=`(forecast_id = x$forecast_id, forecast_origin = x$forecast_origin, engine = x$engine)]
    out
  }), use.names = TRUE, fill = TRUE)
  metric_table <- data.table::rbindlist(lapply(seq_along(assessments), function(i) {
    out <- data.table::copy(assessments[[i]]$metrics)
    out[, `:=`(assessment_id = assessments[[i]]$assessment_id, forecast_id = forecasts[[i]]$forecast_id, forecast_origin = forecasts[[i]]$forecast_origin)]
    out
  }), use.names = TRUE, fill = TRUE)
  backtest_id <- aq_vnext_id("forecast_backtest")
  artifact <- new_table_artifact(
    id = backtest_id,
    title = paste("Rolling-Origin Forecast Backtest:", spec$engine),
    data = metric_table,
    source_generator = "aq_rolling_origin_forecast",
    tags = c("vnext", "forecast", "backtest", spec$engine),
    dependencies = vapply(forecasts, `[[`, character(1L), "forecast_id"),
    version = "aq_forecast_backtest_artifact_v1",
    metadata = list(
      artifact_type = "time_series_forecast_backtest",
      backtest_id = backtest_id,
      forecast_spec_id = spec$forecast_spec_id,
      engine = spec$engine,
      origin_count = length(forecasts),
      supported_downstream_actions = c("compare", "report", "campaign_review")
    )
  )
  artifact <- aq_vnext_attach_envelope(
    artifact,
    artifact_id = backtest_id,
    artifact_type = "time_series_forecast_backtest",
    artifact_version = "aq_forecast_backtest_artifact_v1",
    parent_artifact_ids = vapply(forecasts, `[[`, character(1L), "forecast_id"),
    lineage = list(
      forecast_spec_id = spec$forecast_spec_id,
      forecast_origins = as.character(as.Date(origins)),
      horizon = spec$horizon,
      rolling_window = spec$rolling_window
    ),
    task = "time_series_forecast",
    operator = "rolling_origin_backtest",
    engine = spec$engine,
    specification_id = spec$forecast_spec_id,
    dataset_id = spec$dataset_id,
    supported_actions = c("compare", "report", "campaign_review"),
    producer = "aq_rolling_origin_forecast"
  )
  result <- list(
    backtest_id = backtest_id,
    status = "success",
    schema_version = "aq_forecast_backtest_result_v1",
    forecast_spec_id = spec$forecast_spec_id,
    task = "time_series_forecast",
    engine = spec$engine,
    origins = as.Date(origins),
    forecasts = forecasts,
    assessments = assessments,
    forecast_table = forecast_table,
    metrics = metric_table,
    artifact = artifact,
    comparison_ready = TRUE,
    created_at = aq_vnext_now()
  )
  class(result) <- c("aq_forecast_backtest_result", "list")
  result
}

aq_vnext_forecast_fixture <- function(n = 120L) {
  set.seed(20260712)
  dt <- data.table::data.table(
    date = as.Date("2024-01-01") + seq_len(n) - 1L,
    promo = sample(c(0, 1), n, replace = TRUE, prob = c(0.8, 0.2))
  )
  dt[, demand := 100 + 0.4 * seq_len(.N) + 12 * sin(seq_len(.N) / 7 * 2 * pi) + 8 * promo + stats::rnorm(.N, 0, 4)]
  dt[]
}

#' QA for vNext Forecasting Foundation
#'
#' @return A `data.table` of deterministic QA checks.
#' @export
qa_vnext_forecasting_foundation <- function() {
  rows <- list()
  add <- function(check, passed, message = "") {
    rows[[length(rows) + 1L]] <<- data.table::data.table(
      suite = "vnext_forecasting_foundation",
      check = check,
      status = if (isTRUE(passed)) "pass" else "fail",
      message = message
    )
  }
  dt <- aq_vnext_forecast_fixture()
  spec <- aq_forecast_spec(
    target = "demand",
    date = "date",
    frequency = "auto",
    horizon = 7L,
    engine = "naive",
    future_known_variables = "promo",
    rolling_origins = 3L,
    dataset_id = "qa_forecast_fixture"
  )
  validation <- aq_validate_forecast_spec(spec, dt)
  add("forecast_spec_constructed", inherits(spec, "aq_forecast_spec"))
  add("temporal_validation_passes", !aq_vnext_has_validation_error(validation), paste(validation$message, collapse = " | "))
  add("unsupported_interval_warning", any(validation$check == "interval_support" & validation$status == "warning"))
  duplicate_dt <- data.table::copy(dt)
  duplicate_dt[2L, date := duplicate_dt[1L, date]]
  duplicate_validation <- aq_validate_forecast_spec(spec, duplicate_dt)
  add("duplicate_timestamps_detected", any(duplicate_validation$check == "duplicate_timestamps" & duplicate_validation$status == "fail"))
  missing_dt <- dt[-10L]
  missing_validation <- aq_validate_forecast_spec(spec, missing_dt)
  add("missing_timestamps_reported", any(missing_validation$check == "missing_timestamps" & missing_validation$status == "warning"))
  partition <- aq_forecast_partition(spec, dt)
  add("forecast_partition_constructed", inherits(partition, "aq_forecast_partition") && length(partition$future_dates) == spec$horizon)
  forecast <- aq_fit_forecast(spec, dt)
  add("naive_forecast_result", inherits(forecast, "aq_forecast_result") && nrow(forecast$data) == spec$horizon)
  add("forecast_artifact_enveloped", inherits(aq_artifact_envelope(forecast$artifact), "aq_artifact_envelope") &&
    identical(aq_artifact_envelope(forecast$artifact)$operator, "forecast"))
  assessment <- aq_assess_forecast(forecast)
  add("forecast_assessment_result", inherits(assessment, "aq_forecast_assessment_result") && all(c("rmse", "mae", "mape", "smape", "bias") %in% assessment$metrics$metric))
  add("assessment_artifact_validates", !aq_vnext_has_validation_error(aq_validate_artifact(assessment$assessment_artifact)))
  seasonal_spec <- aq_forecast_spec(
    target = "demand",
    date = "date",
    frequency = "day",
    horizon = 7L,
    engine = "seasonal_naive",
    season_length = 7L,
    rolling_origins = 3L,
    dataset_id = "qa_forecast_fixture"
  )
  seasonal_forecast <- aq_fit_forecast(seasonal_spec, dt)
  add("seasonal_naive_forecast_result", inherits(seasonal_forecast, "aq_forecast_result") && nrow(seasonal_forecast$data) == 7L)
  backtest <- aq_rolling_origin_forecast(seasonal_spec, dt, origin_count = 3L)
  add("rolling_origin_backtest", inherits(backtest, "aq_forecast_backtest_result") && length(backtest$forecasts) == 3L && nrow(backtest$metrics) > 0L)
  ets_spec <- aq_forecast_spec(
    target = "demand",
    date = "date",
    frequency = "day",
    horizon = 7L,
    engine = "ets",
    engine_parameters = list(method = "simple"),
    rolling_origins = 2L,
    dataset_id = "qa_forecast_fixture"
  )
  ets_validation <- aq_validate_forecast_spec(ets_spec, dt)
  add("ets_specification", inherits(ets_spec, "aq_forecast_spec") && identical(ets_spec$engine, "ets"))
  add("ets_validation", !aq_vnext_has_validation_error(ets_validation), paste(ets_validation$message, collapse = " | "))
  ets_forecast <- aq_fit_forecast(ets_spec, dt)
  ets_assessment <- aq_assess_forecast(ets_forecast)
  add("ets_forecast_result", inherits(ets_forecast, "aq_forecast_result") && nrow(ets_forecast$data) == 7L)
  add("ets_engine_diagnostics", all(c("model_form", "aic", "aicc", "bic", "residual_diagnostics", "training_duration_seconds") %in% names(ets_forecast$engine_diagnostics)))
  add("ets_prediction_intervals", isTRUE(ets_forecast$interval_available) && all(c("lower_interval", "upper_interval", "confidence_level") %in% names(ets_forecast$data)))
  add("interval_assessment", all(c("interval_coverage", "average_interval_width", "interval_n") %in% ets_assessment$interval_metrics$metric))
  add("ets_baseline_comparison", nrow(ets_assessment$baseline_metrics) > 0L && nrow(ets_assessment$baseline_comparison) > 0L)
  arima_spec <- aq_forecast_spec(
    target = "demand",
    date = "date",
    frequency = "day",
    horizon = 7L,
    engine = "arima",
    engine_parameters = list(order = c(1L, 0L, 0L), seasonal_order = c(0L, 0L, 0L), include_mean = TRUE),
    future_known_variables = "promo",
    rolling_origins = 2L,
    dataset_id = "qa_forecast_fixture"
  )
  arima_validation <- aq_validate_forecast_spec(arima_spec, dt)
  add("arima_specification", inherits(arima_spec, "aq_forecast_spec") && identical(arima_spec$engine, "arima"))
  add("arima_validation", !aq_vnext_has_validation_error(arima_validation), paste(arima_validation$message, collapse = " | "))
  arima_forecast <- aq_fit_forecast(arima_spec, dt)
  arima_assessment <- aq_assess_forecast(arima_forecast)
  add("arima_forecast_result", inherits(arima_forecast, "aq_forecast_result") && nrow(arima_forecast$data) == 7L)
  add("arima_engine_diagnostics", all(c("order", "seasonal_order", "aic", "aicc", "bic", "residual_diagnostics", "training_duration_seconds") %in% names(arima_forecast$engine_diagnostics)))
  add("arima_known_future_regressors", isTRUE(arima_forecast$future_regressor_metadata$xreg_used) && identical(arima_forecast$future_regressor_metadata$future_known_variables, "promo"))
  add("arima_prediction_intervals", isTRUE(arima_forecast$interval_available) && all(is.finite(arima_forecast$data$lower_interval)))
  add("arima_baseline_comparison", nrow(arima_assessment$baseline_metrics) > 0L && nrow(arima_assessment$baseline_comparison) > 0L)
  arima_backtest <- aq_rolling_origin_forecast(arima_spec, dt, origin_count = 2L)
  add("statistical_rolling_origin_backtest", inherits(arima_backtest, "aq_forecast_backtest_result") && length(arima_backtest$forecasts) == 2L)
  future_missing <- tryCatch(aq_fit_forecast(arima_spec, dt, origin = max(dt$date), future_data = data.table::data.table(date = aq_forecast_next_dates(max(dt$date), "day", 7L))), error = function(e) e)
  add("missing_future_regressor_rejected", inherits(future_missing, "error"))
  add("catboost_engine_registered", "catboost" %in% aq_forecast_engine_levels())
  add("forecast_strategy_registered", all(c("direct", "recursive") %in% aq_forecast_strategy_levels()))
  if (requireNamespace("catboost", quietly = TRUE)) {
    catboost_params <- list(
      iterations = 5L,
      depth = 2L,
      learning_rate = 0.1,
      lag_periods = c(1L, 7L),
      rolling_windows = c(3L, 7L),
      date_features = c("year", "month", "day", "dow", "day_index")
    )
    catboost_direct_spec <- aq_forecast_spec(
      target = "demand",
      date = "date",
      frequency = "day",
      horizon = 3L,
      engine = "catboost",
      forecast_strategy = "direct",
      future_known_variables = "promo",
      engine_parameters = catboost_params,
      rolling_origins = 2L,
      dataset_id = "qa_forecast_fixture"
    )
    catboost_validation <- aq_validate_forecast_spec(catboost_direct_spec, dt)
    add("catboost_validation", !aq_vnext_has_validation_error(catboost_validation), paste(catboost_validation$message, collapse = " | "))
    catboost_forecast <- aq_fit_forecast(catboost_direct_spec, dt)
    catboost_assessment <- aq_assess_forecast(catboost_forecast)
    add("catboost_direct_forecast_result", inherits(catboost_forecast, "aq_forecast_result") && nrow(catboost_forecast$data) == 3L)
    add("catboost_unsupported_intervals", !isTRUE(catboost_forecast$interval_available) &&
      any(grepl("CatBoost deterministic point forecasts", catboost_forecast$data$unsupported_interval_reason, fixed = TRUE)))
    add("catboost_future_regressors", isTRUE(catboost_forecast$future_regressor_metadata$xreg_used) &&
      identical(catboost_forecast$future_regressor_metadata$future_known_variables, "promo"))
    add("catboost_metadata_lineage", is.list(catboost_forecast$catboost_metadata) &&
      all(c("prepared_feature_identity", "rodeo_transformation_identity", "catboost_model_identity", "feature_manifest") %in% names(catboost_forecast$catboost_metadata)))
    add("catboost_rodeo_temporal_contract", isTRUE(aq_forecast_rodeo_temporal_available()) &&
      identical(catboost_forecast$catboost_metadata$temporal_replay_status, "ready") &&
      all(c("temporal_specification_identity", "temporal_transformation_identity", "prepared_temporal_dataset_identity", "temporal_diagnostics") %in% names(catboost_forecast$catboost_metadata)))
    add("catboost_temporal_metadata_artifact", all(c("temporal_replay_status", "temporal_specification_identity", "prepared_temporal_dataset_identity") %in% names(catboost_forecast$artifact$metadata$catboost_metadata)))
    add("catboost_feature_manifest", all(c("target_lag", "target_rolling_stat", "calendar", "future_known_variable") %in% catboost_forecast$catboost_metadata$feature_manifest$source))
    add("catboost_feature_importance", isTRUE(catboost_forecast$catboost_metadata$feature_importance_available) &&
      nrow(catboost_forecast$catboost_metadata$feature_importance) > 0L)
    add("catboost_challenger_baselines", all(c("naive", "seasonal_naive", "ets", "arima") %in% names(catboost_forecast$baseline_forecasts)))
    add("catboost_assessment_comparison", nrow(catboost_assessment$baseline_metrics) > 0L && nrow(catboost_assessment$baseline_comparison) > 0L)
    catboost_direct_backtest <- aq_rolling_origin_forecast(catboost_direct_spec, dt, origin_count = 2L)
    add("catboost_direct_rolling_origin", inherits(catboost_direct_backtest, "aq_forecast_backtest_result") && length(catboost_direct_backtest$forecasts) == 2L)
    catboost_recursive_spec <- catboost_direct_spec
    catboost_recursive_spec$forecast_strategy <- "recursive"
    catboost_recursive_spec$forecast_spec_id <- aq_vnext_id("forecast_spec_catboost_recursive")
    catboost_recursive_forecast <- aq_fit_forecast(catboost_recursive_spec, dt)
    catboost_recursive_backtest <- aq_rolling_origin_forecast(catboost_recursive_spec, dt, origin_count = 2L)
    add("catboost_recursive_forecast_result", inherits(catboost_recursive_forecast, "aq_forecast_result") &&
      identical(catboost_recursive_forecast$engine_diagnostics$forecast_strategy, "recursive"))
    add("catboost_recursive_rolling_origin", inherits(catboost_recursive_backtest, "aq_forecast_backtest_result") && length(catboost_recursive_backtest$forecasts) == 2L)
  } else {
    add("catboost_available", FALSE, "catboost package is required for CatBoost forecast QA.")
  }
  bad_arima <- arima_spec
  bad_arima$engine_parameters$order <- c(1L, 0L)
  bad_arima_validation <- aq_validate_forecast_spec(bad_arima, dt)
  add("engine_parameter_validation", any(bad_arima_validation$check == "arima_order" & bad_arima_validation$status == "fail"))
  serialized <- unserialize(serialize(list(forecast = forecast$artifact, assessment = assessment$assessment_artifact, backtest = backtest$artifact), NULL))
  add("forecast_artifact_serialization", all(vapply(serialized, function(x) !aq_vnext_has_validation_error(aq_validate_artifact(x)), logical(1L))))
  add("comparison_ready", isTRUE(forecast$comparison_ready) && isTRUE(assessment$comparison_ready) && isTRUE(backtest$comparison_ready))
  app_like <- list(forecast$artifact, assessment$assessment_artifact, backtest$artifact)
  add("analytics_shinyapp_compatibility", all(vapply(app_like, is.list, logical(1L))) &&
    all(vapply(app_like, function(x) "campaign_review" %in% aq_supported_actions(x), logical(1L))))
  add("future_operator_contract", identical(aq_artifact_envelope(forecast$artifact)$task, "time_series_forecast") &&
    all(c("artifact_id", "lineage", "supported_actions", "consumer_expectations") %in% names(aq_artifact_envelope(forecast$artifact))))
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}
