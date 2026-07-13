# AutoQuant vNext multi-target forecasting foundation.

aq_multitarget_strategy_levels <- function() {
  c("independent", "shared_workflow", "cross_target_features")
}

aq_multitarget_engine_levels <- function() {
  c("naive", "seasonal_naive", "catboost")
}

aq_multitarget_target_predictor_table <- function(spec) {
  rows <- lapply(spec$targets, function(target) {
    data.table::data.table(
      target = target,
      shared_predictors = paste(spec$shared_predictors, collapse = ", "),
      target_specific_predictors = paste(aq_vnext_unique_chr(spec$target_specific_predictors[[target]]), collapse = ", "),
      known_future_variables = paste(spec$known_future_variables, collapse = ", ")
    )
  })
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

#' Create a vNext Multi-Target Forecast Specification
#'
#' @param targets Character vector of numeric target columns.
#' @param date Shared date/time column.
#' @param frequency Series frequency. Supports `"auto"`, `"day"`, `"week"`,
#'   `"month"`, `"quarter"`, and `"year"`.
#' @param horizon Forecast horizon in periods.
#' @param forecast_origin Optional forecast origin date.
#' @param known_future_variables Variables known at forecast time.
#' @param shared_predictors Predictors shared across targets.
#' @param target_specific_predictors Optional named list of target-specific
#'   predictors.
#' @param strategy Multi-target strategy. Supports `"independent"`,
#'   `"shared_workflow"`, and `"cross_target_features"`.
#' @param engine Deterministic engine. Supports `"naive"`, `"seasonal_naive"`,
#'   and supervised `"catboost"`.
#' @param target_weights Optional named numeric target weights for aggregate
#'   assessment evidence.
#' @param season_length Optional seasonal lag length.
#' @param cross_target_feature_policy Cross-target feature policy. Supports
#'   `"none"`, `"lags"`, and `"lags_rolls"`.
#' @param shared_target_lags Positive lag periods applied to supervised
#'   target-history features.
#' @param target_specific_lags Optional named list of target-specific lag
#'   periods. These override `shared_target_lags` for named targets.
#' @param shared_rolling_windows Optional rolling mean windows applied to
#'   supervised cross-target feature preparation.
#' @param target_relationship_metadata Optional descriptive metadata about
#'   target relationships. This is recorded as context only.
#' @param engine_parameters Optional engine parameter list for CatBoost.
#' @param forecast_spec_id Optional specification id.
#' @param dataset_id Optional dataset id.
#' @param supported_downstream_actions Supported next actions.
#'
#' @return An `aq_multitarget_forecast_spec` object.
#' @export
aq_multitarget_forecast_spec <- function(
  targets,
  date,
  frequency = "auto",
  horizon = 1L,
  forecast_origin = NULL,
  known_future_variables = character(),
  shared_predictors = character(),
  target_specific_predictors = list(),
  strategy = "independent",
  engine = "naive",
  target_weights = NULL,
  season_length = NULL,
  cross_target_feature_policy = "none",
  shared_target_lags = 1L,
  target_specific_lags = list(),
  shared_rolling_windows = integer(),
  target_relationship_metadata = list(),
  engine_parameters = list(),
  forecast_spec_id = NULL,
  dataset_id = NULL,
  supported_downstream_actions = c("forecast", "assess", "compare", "report", "campaign_review")
) {
  targets <- as.character(targets)
  targets <- targets[!is.na(targets) & nzchar(targets)]
  frequency <- match.arg(tolower(frequency), aq_forecast_frequency_levels())
  strategy <- match.arg(tolower(strategy), aq_multitarget_strategy_levels())
  engine <- match.arg(tolower(engine), aq_multitarget_engine_levels())
  cross_target_feature_policy <- match.arg(tolower(cross_target_feature_policy), c("none", "lags", "lags_rolls"))
  horizon <- as.integer(horizon)[1L]
  if (is.null(forecast_spec_id)) {
    forecast_spec_id <- aq_vnext_id(paste("multitarget_forecast_spec", engine, strategy, paste(targets, collapse = "_"), sep = "_"))
  }
  target_specific_predictors <- aq_vnext_default(target_specific_predictors, list())
  if (length(target_specific_predictors) && is.null(names(target_specific_predictors))) {
    names(target_specific_predictors) <- rep("", length(target_specific_predictors))
  }
  target_specific_lags <- aq_vnext_default(target_specific_lags, list())
  if (length(target_specific_lags) && is.null(names(target_specific_lags))) {
    names(target_specific_lags) <- rep("", length(target_specific_lags))
  }
  shared_target_lags <- unique(as.integer(shared_target_lags))
  shared_target_lags <- shared_target_lags[is.finite(shared_target_lags) & shared_target_lags > 0L]
  if (!length(shared_target_lags)) shared_target_lags <- 1L
  shared_rolling_windows <- unique(as.integer(shared_rolling_windows))
  shared_rolling_windows <- shared_rolling_windows[is.finite(shared_rolling_windows) & shared_rolling_windows > 0L]
  target_weights <- aq_vnext_default(target_weights, stats::setNames(rep(1, length(targets)), targets))
  if (is.null(names(target_weights))) {
    names(target_weights) <- targets[seq_along(target_weights)]
  }
  spec <- list(
    forecast_spec_id = as.character(forecast_spec_id)[1L],
    schema_version = "aq_multitarget_forecast_spec_v1",
    targets = targets,
    date = as.character(date)[1L],
    frequency = frequency,
    horizon = horizon,
    forecast_origin = if (is.null(forecast_origin)) NULL else as.Date(forecast_origin)[1L],
    known_future_variables = aq_vnext_unique_chr(known_future_variables),
    shared_predictors = aq_vnext_unique_chr(shared_predictors),
    target_specific_predictors = target_specific_predictors,
    strategy = strategy,
    engine = engine,
    target_weights = target_weights,
    season_length = if (is.null(season_length)) NULL else as.integer(season_length)[1L],
    cross_target_feature_policy = cross_target_feature_policy,
    shared_target_lags = shared_target_lags,
    target_specific_lags = target_specific_lags,
    shared_rolling_windows = shared_rolling_windows,
    target_relationship_metadata = if (is.null(target_relationship_metadata)) list() else target_relationship_metadata,
    engine_parameters = if (is.null(engine_parameters)) list() else engine_parameters,
    dataset_id = aq_vnext_default(dataset_id, NA_character_),
    supported_downstream_actions = aq_vnext_unique_chr(supported_downstream_actions),
    created_at = aq_vnext_now()
  )
  class(spec) <- c("aq_multitarget_forecast_spec", "list")
  spec
}

#' Validate a vNext Multi-Target Forecast Specification
#'
#' @param spec An `aq_multitarget_forecast_spec`.
#' @param data Optional wide multi-target time-series data.
#' @param future_data Optional future data for known future variables.
#'
#' @return A `data.table` of deterministic validation diagnostics.
#' @export
aq_validate_multitarget_forecast_spec <- function(spec, data = NULL, future_data = NULL) {
  rows <- list()
  add <- function(check, status, message, severity = status) {
    rows[[length(rows) + 1L]] <<- aq_vnext_validation_table(check, status, message, severity)
  }
  if (!inherits(spec, "aq_multitarget_forecast_spec")) {
    add("multitarget_forecast_spec_class", "fail", "spec must be created by aq_multitarget_forecast_spec().")
    return(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
  }
  add("multitarget_forecast_spec_class", "pass", "multi-target forecast specification is typed.", "info")
  if (length(spec$targets) < 2L) add("target_count", "fail", "multi-target forecasting requires at least two targets.") else add("target_count", "pass", paste("targets:", length(spec$targets)), "info")
  if (anyDuplicated(spec$targets)) add("duplicate_targets", "fail", "targets must be unique.") else add("duplicate_targets", "pass", "targets are unique.", "info")
  if (!is.finite(spec$horizon) || spec$horizon < 1L) add("horizon", "fail", "forecast horizon must be positive.") else add("horizon", "pass", paste("horizon:", spec$horizon), "info")
  if (!spec$strategy %in% aq_multitarget_strategy_levels()) add("strategy_supported", "fail", paste("unsupported multi-target strategy:", spec$strategy)) else add("strategy_supported", "pass", paste("strategy:", spec$strategy), "info")
  if (!spec$engine %in% aq_multitarget_engine_levels()) add("engine_supported", "fail", paste("unsupported deterministic engine:", spec$engine)) else add("engine_supported", "pass", paste("engine:", spec$engine), "info")
  if (identical(spec$strategy, "cross_target_features") && !identical(spec$engine, "catboost")) add("cross_target_engine", "fail", "cross_target_features requires engine = 'catboost'.") else add("cross_target_engine", "pass", "strategy and engine are compatible.", "info")
  if (identical(spec$engine, "catboost")) add("catboost_strategy", "pass", "CatBoost supervised multi-target forecasting uses direct horizon models.", "info")
  if (!spec$cross_target_feature_policy %in% c("none", "lags", "lags_rolls")) add("cross_target_feature_policy", "fail", "cross_target_feature_policy must be none, lags, or lags_rolls.") else add("cross_target_feature_policy", "pass", paste("cross-target feature policy:", spec$cross_target_feature_policy), "info")
  bad_lags <- spec$shared_target_lags[!is.finite(spec$shared_target_lags) | spec$shared_target_lags < 1L]
  if (length(bad_lags)) add("shared_target_lags", "fail", "shared_target_lags must contain positive integers.") else add("shared_target_lags", "pass", paste("shared target lags:", paste(spec$shared_target_lags, collapse = ", ")), "info")
  bad_windows <- spec$shared_rolling_windows[!is.finite(spec$shared_rolling_windows) | spec$shared_rolling_windows < 1L]
  if (length(bad_windows)) add("shared_rolling_windows", "fail", "shared_rolling_windows must contain positive integers.") else add("shared_rolling_windows", "pass", paste("shared rolling windows:", paste(spec$shared_rolling_windows, collapse = ", ")), "info")
  target_specific_lag_names <- names(spec$target_specific_lags)
  unknown_lag_targets <- setdiff(target_specific_lag_names[nzchar(target_specific_lag_names)], spec$targets)
  if (length(unknown_lag_targets)) add("target_specific_lag_names", "fail", paste("target-specific lags supplied for unknown target(s):", paste(unknown_lag_targets, collapse = ", "))) else add("target_specific_lag_names", "pass", "target-specific lag declarations map to known targets.", "info")
  target_specific_names <- names(spec$target_specific_predictors)
  unknown_specific <- setdiff(target_specific_names[nzchar(target_specific_names)], spec$targets)
  if (length(unknown_specific)) add("target_specific_predictor_names", "fail", paste("target-specific predictors supplied for unknown target(s):", paste(unknown_specific, collapse = ", "))) else add("target_specific_predictor_names", "pass", "target-specific predictor declarations map to known targets.", "info")
  predictor_overlap <- intersect(unique(c(spec$known_future_variables, spec$shared_predictors, unlist(spec$target_specific_predictors, use.names = FALSE))), spec$targets)
  if (length(predictor_overlap)) add("future_leakage_predictors", "fail", paste("predictors cannot include target columns:", paste(predictor_overlap, collapse = ", "))) else add("future_leakage_predictors", "pass", "declared predictors do not include target columns.", "info")
  add("same_period_target_leakage", "pass", "cross-target features are shifted to use strictly prior target history.", "info")
  add("future_target_leakage", "pass", "future target values are not accepted as known future variables.", "info")
  add("forecast_origin_leakage", "pass", "training labels and features are restricted to information available at the forecast origin.", "info")
  add("recursive_leakage", "pass", "supervised multi-target CatBoost uses direct horizon models; recursive cross-target updates are not used.", "info")
  if (is.null(data)) return(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
  dt <- data.table::as.data.table(data)
  required <- unique(c(spec$date, spec$targets, spec$known_future_variables, spec$shared_predictors, unlist(spec$target_specific_predictors, use.names = FALSE)))
  missing <- setdiff(required, names(dt))
  if (length(missing)) {
    add("required_columns", "fail", paste("Missing required column(s):", paste(missing, collapse = ", ")))
    return(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
  }
  add("required_columns", "pass", "required columns are present.", "info")
  dates <- as.Date(dt[[spec$date]])
  if (all(is.na(dates))) {
    add("date_type", "fail", "date column cannot be converted to Date.")
    return(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
  }
  duplicate_dates <- sum(duplicated(dates[!is.na(dates)]))
  if (duplicate_dates) add("duplicate_date_target_combinations", "fail", paste("wide multi-target data contains duplicate date rows:", duplicate_dates)) else add("duplicate_date_target_combinations", "pass", "date/target combinations are unique in wide data.", "info")
  non_numeric_targets <- spec$targets[!vapply(spec$targets, function(x) is.numeric(dt[[x]]), logical(1L))]
  if (length(non_numeric_targets)) add("target_types", "fail", paste("targets must be numeric:", paste(non_numeric_targets, collapse = ", "))) else add("target_types", "pass", "all targets are numeric.", "info")
  detection <- aq_forecast_detect_frequency(dates)
  resolved_frequency <- if (identical(spec$frequency, "auto")) detection$frequency else spec$frequency
  if (is.na(resolved_frequency) || identical(resolved_frequency, "irregular")) add("shared_timeline", "warning", paste("shared timeline is irregular or unavailable; median interval:", detection$interval), "warning") else add("shared_timeline", "pass", paste("shared frequency:", resolved_frequency), "info")
  if (!is.null(spec$forecast_origin) && spec$forecast_origin < min(dates, na.rm = TRUE)) add("forecast_origin", "fail", "forecast origin is before available history.") else add("forecast_origin", "pass", "forecast origin is compatible.", "info")
  min_history <- spec$horizon + aq_forecast_season_length(if (identical(spec$frequency, "auto")) aq_vnext_default(resolved_frequency, "day") else spec$frequency, spec$season_length)
  short_targets <- spec$targets[vapply(spec$targets, function(x) sum(!is.na(dt[[x]])) < min_history, logical(1L))]
  if (length(short_targets)) add("target_history_length", "warning", paste("targets below recommended deterministic history:", paste(short_targets, collapse = ", ")), "warning") else add("target_history_length", "pass", "all targets have sufficient deterministic history.", "info")
  if (!is.null(future_data) && length(spec$known_future_variables)) {
    fdt <- data.table::as.data.table(future_data)
    future_missing <- setdiff(c(spec$date, spec$known_future_variables), names(fdt))
    if (length(future_missing)) add("future_schema", "fail", paste("future data missing column(s):", paste(future_missing, collapse = ", "))) else add("future_schema", "pass", "future known-variable schema is present.", "info")
  } else if (length(spec$known_future_variables)) {
    add("future_schema", "warning", "known future variables are declared but future_data was not supplied; deterministic baselines record them as context only.", "warning")
  }
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

aq_multitarget_resolved_frequency <- function(spec, data) {
  if (!identical(spec$frequency, "auto")) return(spec$frequency)
  detected <- aq_forecast_detect_frequency(as.Date(data[[spec$date]]))$frequency
  if (is.na(detected) || identical(detected, "irregular")) "day" else detected
}

aq_multitarget_partition <- function(spec, data, origin = NULL) {
  dt <- data.table::as.data.table(data.table::copy(data))
  dt[, .aq_forecast_date := as.Date(get(spec$date))]
  data.table::setorder(dt, .aq_forecast_date)
  frequency <- aq_multitarget_resolved_frequency(spec, dt)
  unique_dates <- sort(unique(dt$.aq_forecast_date))
  if (is.null(origin)) origin <- aq_vnext_default(spec$forecast_origin, unique_dates[max(1L, length(unique_dates) - spec$horizon)])
  origin <- as.Date(origin)[1L]
  future_dates <- aq_forecast_next_dates(origin, frequency, spec$horizon)
  list(
    partition_id = aq_vnext_id(paste0("multitarget_forecast_partition_", spec$forecast_spec_id)),
    forecast_origin = origin,
    frequency = frequency,
    horizon = spec$horizon,
    future_dates = future_dates,
    train_index = which(dt$.aq_forecast_date <= origin),
    evaluation_index = which(dt$.aq_forecast_date %in% future_dates),
    summary = data.table::data.table(
      target_count = length(spec$targets),
      training_rows = sum(dt$.aq_forecast_date <= origin),
      evaluation_rows = sum(dt$.aq_forecast_date %in% future_dates),
      forecast_origin = origin,
      horizon = spec$horizon
    )
  )
}

aq_multitarget_rodeo_metadata <- function(spec, train, partition) {
  if (!requireNamespace("Rodeo", quietly = TRUE)) {
    return(list(status = "unavailable", reason = "Rodeo package is not installed."))
  }
  out <- lapply(spec$targets, function(target) {
    temporal_spec <- Rodeo::rodeo_temporal_transformation_spec(
      date_col = spec$date,
      target_col = target,
      frequency = partition$frequency,
      calendar_features = c("year", "month", "day", "dow", "day_index"),
      lag_periods = aq_forecast_season_length(partition$frequency, spec$season_length),
      rolling_windows = integer(),
      known_future_variables = spec$known_future_variables,
      forecast_horizon = spec$horizon,
      metadata = list(producer = "AutoQuant", consumer = "aq_fit_multitarget_forecast", strategy = spec$strategy)
    )
    fit <- Rodeo::rodeo_fit_temporal_transformation(train, temporal_spec, forecast_origin = partition$forecast_origin)
    meta <- Rodeo::rodeo_temporal_transformation_metadata(fit)
    meta$target <- target
    meta
  })
  names(out) <- spec$targets
  list(status = "ready", target_metadata = out)
}

aq_multitarget_cross_target_evidence <- function(spec, dt, forecast_dt, partition, feature_importance = NULL, preparation_records = NULL, diagnostics = NULL) {
  train <- dt[.aq_forecast_date <= partition$forecast_origin]
  target_summary <- data.table::rbindlist(lapply(spec$targets, function(target) {
    y <- as.numeric(train[[target]])
    data.table::data.table(
      target = target,
      observations = sum(!is.na(y)),
      mean_actual_history = mean(y, na.rm = TRUE),
      sd_actual_history = stats::sd(y, na.rm = TRUE),
      latest_actual = utils::tail(y[!is.na(y)], 1L)
    )
  }), use.names = TRUE, fill = TRUE)
  target_correlations <- data.table::data.table()
  if (length(spec$targets) > 1L) {
    pairs <- utils::combn(spec$targets, 2L, simplify = FALSE)
    target_correlations <- data.table::rbindlist(lapply(pairs, function(pair) {
      data.table::data.table(
        target_a = pair[1L],
        target_b = pair[2L],
        correlation = suppressWarnings(stats::cor(train[[pair[1L]]], train[[pair[2L]]], use = "complete.obs")),
        complete_rows = sum(stats::complete.cases(train[[pair[1L]]], train[[pair[2L]]]))
      )
    }), use.names = TRUE, fill = TRUE)
  }
  forecast_divergence <- forecast_dt[, .(
    target_count = data.table::uniqueN(target),
    mean_forecast = mean(forecast, na.rm = TRUE),
    sd_forecast = stats::sd(forecast, na.rm = TRUE),
    min_forecast = min(forecast, na.rm = TRUE),
    max_forecast = max(forecast, na.rm = TRUE),
    forecast_range = max(forecast, na.rm = TRUE) - min(forecast, na.rm = TRUE)
  ), by = .(forecast_date, horizon)]
  forecast_divergence[!is.finite(sd_forecast), sd_forecast := NA_real_]
  list(
    target_identities = data.table::data.table(target = spec$targets, target_weight = as.numeric(spec$target_weights[spec$targets])),
    target_summary = target_summary,
    target_correlations = target_correlations,
    predictor_usage = aq_multitarget_target_predictor_table(spec),
    forecast_divergence = forecast_divergence,
    cross_target_feature_importance = if (is.null(feature_importance)) data.table::data.table() else data.table::copy(feature_importance),
    target_contribution_summary = aq_multitarget_target_contribution_summary(feature_importance),
    shared_predictor_diagnostics = aq_multitarget_shared_predictor_diagnostics(spec, feature_importance),
    cross_target_diagnostics = if (is.null(diagnostics)) data.table::data.table() else data.table::copy(diagnostics),
    cross_target_preparation_identity = if (is.null(preparation_records)) character() else vapply(preparation_records, function(x) x$preparation_identity, character(1L)),
    cross_target_feature_policy = spec$cross_target_feature_policy,
    target_relationship_metadata = spec$target_relationship_metadata,
    shared_predictor_count = length(spec$shared_predictors),
    target_specific_predictor_count = length(unique(unlist(spec$target_specific_predictors, use.names = FALSE)))
  )
}

aq_multitarget_require_supervised <- function() {
  if (!requireNamespace("catboost", quietly = TRUE)) {
    stop("The catboost package is required for supervised multi-target forecasting.", call. = FALSE)
  }
  if (!requireNamespace("Rodeo", quietly = TRUE) ||
      !"rodeo_prepare_cross_target_features" %in% getNamespaceExports("Rodeo")) {
    stop("Supervised multi-target forecasting requires Rodeo::rodeo_prepare_cross_target_features(). Rebuild/install Rodeo with the Phase 20 temporal API.", call. = FALSE)
  }
  invisible(TRUE)
}

aq_multitarget_target_lags <- function(spec, target) {
  lags <- spec$target_specific_lags[[target]]
  if (is.null(lags)) lags <- spec$shared_target_lags
  lags <- unique(as.integer(lags))
  lags <- lags[is.finite(lags) & lags > 0L]
  if (!length(lags)) lags <- 1L
  lags
}

aq_multitarget_future_frame <- function(spec, partition, future_data = NULL, dt = NULL) {
  future <- if (!is.null(future_data)) {
    data.table::as.data.table(data.table::copy(future_data))
  } else if (!is.null(dt)) {
    data.table::copy(dt[.aq_forecast_date %in% partition$future_dates])
  } else {
    data.table::data.table()
  }
  if (!nrow(future)) {
    future <- data.table::data.table(.aq_forecast_date = partition$future_dates)
  }
  if (!spec$date %in% names(future)) {
    if (".aq_forecast_date" %in% names(future)) {
      future[[spec$date]] <- as.Date(future$.aq_forecast_date)
    } else {
      future[[spec$date]] <- partition$future_dates[seq_len(min(nrow(future), length(partition$future_dates)))]
    }
  }
  future <- future[seq_len(min(nrow(future), spec$horizon))]
  if (nrow(future) < spec$horizon) {
    missing_dates <- partition$future_dates[(nrow(future) + 1L):spec$horizon]
    add <- data.table::data.table(.aq_forecast_date = missing_dates)
    add[[spec$date]] <- missing_dates
    future <- data.table::rbindlist(list(future, add), use.names = TRUE, fill = TRUE)
  }
  future[]
}

aq_multitarget_fill_features <- function(train_frame, pred_frame, feature_cols) {
  train_out <- data.table::copy(train_frame)
  pred_out <- data.table::copy(pred_frame)
  for (col in feature_cols) {
    train_out[[col]] <- as.numeric(train_out[[col]])
    pred_out[[col]] <- as.numeric(pred_out[[col]])
    fill <- suppressWarnings(stats::median(train_out[[col]], na.rm = TRUE))
    if (!is.finite(fill)) fill <- 0
    train_out[is.na(get(col)), (col) := fill]
    pred_out[is.na(get(col)), (col) := fill]
  }
  list(train = train_out, pred = pred_out)
}

aq_multitarget_feature_role <- function(feature, target) {
  if (grepl("^future_", feature)) return("known_future")
  if (grepl("^date_", feature)) return("calendar")
  if (!grepl("^cross_target_", feature)) return("other")
  source_target <- sub("^cross_target_", "", feature)
  source_target <- sub("_(lag|roll_mean)_.*$", "", source_target)
  if (identical(source_target, target)) "current_target_history" else "cross_target_other"
}

aq_multitarget_source_target <- function(feature) {
  if (!grepl("^cross_target_", feature)) return(NA_character_)
  source_target <- sub("^cross_target_", "", feature)
  sub("_(lag|roll_mean)_.*$", "", source_target)
}

aq_multitarget_supervised_fit <- function(spec, dt, partition, future_data = NULL) {
  aq_multitarget_require_supervised()
  future_frame <- aq_multitarget_future_frame(spec, partition, future_data = future_data, dt = dt)
  label_table <- dt[, .(.aq_forecast_date, .aq_row_index = seq_len(.N))]
  target_forecasts <- list()
  models <- list()
  feature_importance <- list()
  preparation_records <- list()
  diagnostics <- list()
  for (target in spec$targets) {
    prep_targets <- if (identical(spec$strategy, "cross_target_features")) spec$targets else target
    lag_periods <- aq_multitarget_target_lags(spec, target)
    rolling_windows <- if (identical(spec$cross_target_feature_policy, "lags_rolls")) spec$shared_rolling_windows else integer()
    prep <- Rodeo::rodeo_prepare_cross_target_features(
      data = dt,
      date_col = spec$date,
      target_cols = prep_targets,
      forecast_origin = partition$forecast_origin,
      lag_periods = lag_periods,
      rolling_windows = rolling_windows,
      known_future_variables = spec$known_future_variables,
      future_data = future_frame[, intersect(c(spec$date, spec$known_future_variables), names(future_frame)), with = FALSE]
    )
    feature_cols <- prep$feature_columns
    if (!length(feature_cols)) stop("Rodeo cross-target preparation produced no supervised feature columns.", call. = FALSE)
    target_rows <- list()
    target_models <- list()
    preparation_records[[target]] <- list(
      target = target,
      preparation_id = prep$preparation_id,
      preparation_identity = prep$metadata$preparation_identity,
      target_cols = prep$target_cols,
      feature_manifest = prep$feature_manifest,
      diagnostics = prep$diagnostics
    )
    diagnostics[[target]] <- prep$diagnostics[, target := target][]
    for (h in seq_len(spec$horizon)) {
      train_frame <- data.table::copy(prep$training_features)
      train_frame <- label_table[train_frame, on = c(".aq_forecast_date" = ".rodeo_cross_target_date")]
      train_frame[, .aq_label_index := .aq_row_index + h]
      labels <- dt[, .(.aq_label_index = seq_len(.N), .rodeo_label = as.numeric(get(target)), .rodeo_label_date = .aq_forecast_date)]
      train_frame <- labels[train_frame, on = ".aq_label_index"]
      train_frame <- train_frame[.rodeo_label_date <= partition$forecast_origin]
      pred_frame <- prep$future_features[.rodeo_cross_target_date == partition$future_dates[h]]
      if (!nrow(pred_frame)) pred_frame <- prep$future_features[min(h, nrow(prep$future_features))]
      filled <- aq_multitarget_fill_features(train_frame, pred_frame, feature_cols)
      target_spec <- aq_forecast_spec(
        target = target,
        date = spec$date,
        frequency = partition$frequency,
        horizon = spec$horizon,
        forecast_origin = partition$forecast_origin,
        engine = "catboost",
        forecast_strategy = "direct",
        future_known_variables = spec$known_future_variables,
        engine_parameters = spec$engine_parameters,
        prediction_intervals = FALSE,
        season_length = spec$season_length,
        dataset_id = spec$dataset_id
      )
      trained <- aq_forecast_catboost_train_one(filled$train, ".rodeo_label", feature_cols, target_spec)
      pred <- aq_forecast_catboost_predict_one(trained$model, filled$pred, feature_cols)
      actual <- dt[.aq_forecast_date == partition$future_dates[h], as.numeric(get(target))]
      target_rows[[h]] <- data.table::data.table(
        target = target,
        forecast_date = partition$future_dates[h],
        horizon = h,
        forecast = as.numeric(pred),
        actual = if (length(actual)) actual[1L] else NA_real_
      )
      imp <- data.table::copy(trained$feature_importance)
      imp[, `:=`(
        target = target,
        horizon = h,
        source_target = vapply(feature, aq_multitarget_source_target, character(1L)),
        feature_role = vapply(feature, aq_multitarget_feature_role, character(1L), target = target),
        preparation_id = prep$preparation_id
      )]
      feature_importance[[length(feature_importance) + 1L]] <- imp
      target_models[[as.character(h)]] <- trained$model
    }
    models[[target]] <- target_models
    target_forecasts[[target]] <- data.table::rbindlist(target_rows, use.names = TRUE, fill = TRUE)
  }
  list(
    forecast = data.table::rbindlist(target_forecasts, use.names = TRUE, fill = TRUE),
    model = list(strategy = spec$strategy, engine = "catboost", target_models = models),
    feature_importance = data.table::rbindlist(feature_importance, use.names = TRUE, fill = TRUE),
    preparation_records = preparation_records,
    diagnostics = data.table::rbindlist(diagnostics, use.names = TRUE, fill = TRUE)
  )
}

aq_multitarget_target_contribution_summary <- function(feature_importance) {
  if (is.null(feature_importance) || !nrow(feature_importance)) {
    return(data.table::data.table())
  }
  feature_importance[feature_role == "cross_target_other", .(
    total_importance = sum(importance, na.rm = TRUE),
    mean_importance = mean(importance, na.rm = TRUE),
    feature_count = .N
  ), by = .(target, source_target)]
}

aq_multitarget_shared_predictor_diagnostics <- function(spec, feature_importance) {
  if (is.null(feature_importance) || !nrow(feature_importance)) {
    return(data.table::data.table())
  }
  feature_importance[, .(
    total_importance = sum(importance, na.rm = TRUE),
    mean_importance = mean(importance, na.rm = TRUE),
    feature_count = .N
  ), by = .(feature_role)]
}

#' Fit a vNext Multi-Target Forecast
#'
#' @param spec An `aq_multitarget_forecast_spec`.
#' @param data Wide multi-target time-series data.
#' @param origin Optional forecast origin override.
#' @param future_data Optional future known-variable data.
#'
#' @return An `aq_multitarget_forecast_result`.
#' @export
aq_fit_multitarget_forecast <- function(spec, data, origin = NULL, future_data = NULL) {
  validation <- aq_validate_multitarget_forecast_spec(spec, data, future_data = future_data)
  if (aq_vnext_has_validation_error(validation)) stop(paste(validation[status %in% c("fail", "error"), message], collapse = " "), call. = FALSE)
  dt <- data.table::as.data.table(data.table::copy(data))
  dt[, .aq_forecast_date := as.Date(get(spec$date))]
  partition <- aq_multitarget_partition(spec, dt, origin = origin)
  train <- dt[partition$train_index]
  eval <- dt[partition$evaluation_index]
  rodeo_metadata <- aq_multitarget_rodeo_metadata(spec, train, partition)
  supervised_fit <- NULL
  if (identical(spec$engine, "catboost")) {
    supervised_fit <- aq_multitarget_supervised_fit(spec, dt, partition, future_data = future_data)
    forecast_dt <- supervised_fit$forecast
  } else {
    target_forecasts <- lapply(spec$targets, function(target) {
      target_spec <- aq_forecast_spec(
        target = target,
        date = spec$date,
        frequency = partition$frequency,
        horizon = spec$horizon,
        forecast_origin = partition$forecast_origin,
        engine = spec$engine,
        prediction_intervals = FALSE,
        season_length = spec$season_length,
        dataset_id = spec$dataset_id
      )
      pred <- aq_forecast_baseline_values(train, target_spec, partition$frequency)
      out <- data.table::data.table(
        target = target,
        forecast_date = partition$future_dates,
        horizon = seq_len(spec$horizon),
        forecast = as.numeric(pred)
      )
      if (nrow(eval)) {
        actuals <- eval[, .(forecast_date = .aq_forecast_date, actual = as.numeric(get(target)))]
        out <- merge(out, actuals, by = "forecast_date", all.x = TRUE, sort = FALSE)
        data.table::setorder(out, horizon)
      } else {
        out[, actual := NA_real_]
      }
      out
    })
    forecast_dt <- data.table::rbindlist(target_forecasts, use.names = TRUE, fill = TRUE)
  }
  forecast_dt[, forecast_strategy := spec$strategy]
  cross_target_evidence <- aq_multitarget_cross_target_evidence(
    spec,
    dt,
    forecast_dt,
    partition,
    feature_importance = supervised_fit$feature_importance,
    preparation_records = supervised_fit$preparation_records,
    diagnostics = supervised_fit$diagnostics
  )
  forecast_id <- aq_vnext_id(paste0("multitarget_forecast_", spec$strategy))
  artifact <- new_table_artifact(
    id = forecast_id,
    title = paste("Multi-Target Forecast:", tools::toTitleCase(gsub("_", " ", spec$strategy))),
    data = forecast_dt,
    source_generator = "aq_fit_multitarget_forecast",
    tags = c("vnext", "forecast", "multi_target", spec$strategy, spec$engine),
    dependencies = spec$forecast_spec_id,
    version = "aq_multitarget_forecast_artifact_v1",
    metadata = list(
      artifact_type = "multi_target_forecast",
      forecast_id = forecast_id,
      forecast_spec_id = spec$forecast_spec_id,
      partition_id = partition$partition_id,
      targets = spec$targets,
      target_count = length(spec$targets),
      date = spec$date,
      engine = spec$engine,
      strategy = spec$strategy,
      frequency = partition$frequency,
      forecast_origin = partition$forecast_origin,
      horizon = spec$horizon,
      rodeo_temporal_metadata = rodeo_metadata,
      cross_target_feature_identity = cross_target_evidence$cross_target_preparation_identity,
      cross_target_preparation_identity = cross_target_evidence$cross_target_preparation_identity,
      target_relationship_metadata = spec$target_relationship_metadata,
      cross_target_diagnostics = cross_target_evidence$cross_target_diagnostics,
      supervised_metadata = if (is.null(supervised_fit)) NULL else list(
        feature_importance_available = nrow(supervised_fit$feature_importance) > 0L,
        feature_importance = supervised_fit$feature_importance,
        preparation_records = supervised_fit$preparation_records
      ),
      cross_target_evidence = cross_target_evidence,
      supported_downstream_actions = c("assess", "compare", "report", "campaign_review")
    )
  )
  artifact <- aq_vnext_attach_envelope(
    artifact,
    artifact_id = forecast_id,
    artifact_type = "multi_target_forecast",
    artifact_version = "aq_multitarget_forecast_artifact_v1",
    parent_artifact_ids = spec$forecast_spec_id,
    lineage = list(
      forecast_spec_id = spec$forecast_spec_id,
      partition_id = partition$partition_id,
      targets = spec$targets,
      strategy = spec$strategy,
      engine = spec$engine,
      forecast_origin = partition$forecast_origin,
      horizon = spec$horizon,
      rodeo_temporal_metadata = rodeo_metadata,
      cross_target_preparation_identity = cross_target_evidence$cross_target_preparation_identity,
      target_relationship_metadata = spec$target_relationship_metadata
    ),
    task = "multi_target_forecast",
    operator = "forecast",
    engine = spec$engine,
    specification_id = spec$forecast_spec_id,
    dataset_id = spec$dataset_id,
    supported_actions = c("assess", "compare", "report", "campaign_review"),
    producer = "aq_fit_multitarget_forecast"
  )
  result <- list(
    forecast_id = forecast_id,
    status = "success",
    schema_version = "aq_multitarget_forecast_result_v1",
    spec = spec,
    forecast_spec_id = spec$forecast_spec_id,
    partition = partition,
    partition_id = partition$partition_id,
    task = "multi_target_forecast",
    engine = spec$engine,
    strategy = spec$strategy,
    targets = spec$targets,
    target_count = length(spec$targets),
    date = spec$date,
    frequency = partition$frequency,
    forecast_origin = partition$forecast_origin,
    horizon = spec$horizon,
    data = forecast_dt,
    model = if (is.null(supervised_fit)) NULL else supervised_fit$model,
    feature_importance = if (is.null(supervised_fit)) data.table::data.table() else supervised_fit$feature_importance,
    cross_target_preparation_records = if (is.null(supervised_fit)) list() else supervised_fit$preparation_records,
    cross_target_diagnostics = if (is.null(supervised_fit)) data.table::data.table() else supervised_fit$diagnostics,
    cross_target_evidence = cross_target_evidence,
    rodeo_temporal_metadata = rodeo_metadata,
    artifact = artifact,
    validation = validation,
    warnings = validation[status == "warning", message],
    comparison_ready = TRUE,
    supported_downstream_actions = c("assess", "compare", "report", "campaign_review"),
    created_at = aq_vnext_now()
  )
  class(result) <- c("aq_multitarget_forecast_result", "list")
  result
}

aq_multitarget_weighted_metrics <- function(metrics_by_target, spec) {
  if (!nrow(metrics_by_target)) return(data.table::data.table())
  weights <- data.table::data.table(target = spec$targets, target_weight = as.numeric(spec$target_weights[spec$targets]))
  weights[!is.finite(target_weight), target_weight := 1]
  dt <- merge(metrics_by_target, weights, by = "target", all.x = TRUE, sort = FALSE)
  dt[, .(value = stats::weighted.mean(value, target_weight, na.rm = TRUE)), by = metric]
}

#' Assess Multi-Target Forecast Evidence
#'
#' @param forecast An `aq_multitarget_forecast_result`.
#'
#' @return An `aq_multitarget_forecast_assessment_result`.
#' @export
aq_assess_multitarget_forecast <- function(forecast) {
  if (!inherits(forecast, "aq_multitarget_forecast_result")) stop("forecast must be an aq_multitarget_forecast_result.", call. = FALSE)
  if (!"actual" %in% names(forecast$data) || all(is.na(forecast$data$actual))) stop("multi-target forecast result does not contain realized actuals for assessment.", call. = FALSE)
  metrics <- aq_forecast_metrics(forecast$data)
  metrics_by_target <- forecast$data[, aq_forecast_metrics(.SD), by = target]
  metrics_by_horizon <- forecast$data[, aq_forecast_metrics(.SD), by = horizon]
  metrics_by_target_horizon <- forecast$data[, aq_forecast_metrics(.SD), by = .(target, horizon)]
  weighted_metrics <- aq_multitarget_weighted_metrics(metrics_by_target, forecast$spec)
  feature_usefulness <- if (!is.null(forecast$feature_importance) && nrow(forecast$feature_importance)) {
    forecast$feature_importance[, .(
      total_importance = sum(importance, na.rm = TRUE),
      mean_importance = mean(importance, na.rm = TRUE),
      feature_count = .N
    ), by = .(target, feature_role)]
  } else {
    data.table::data.table()
  }
  target_summary <- forecast$data[, .(
    rows = .N,
    assessed_rows = sum(stats::complete.cases(actual, forecast)),
    mean_actual = mean(actual, na.rm = TRUE),
    mean_forecast = mean(forecast, na.rm = TRUE),
    forecast_bias = mean(actual - forecast, na.rm = TRUE)
  ), by = target]
  assessment_id <- aq_vnext_id("multitarget_forecast_assessment")
  artifact <- new_table_artifact(
    id = assessment_id,
    title = "Multi-Target Forecast Assessment",
    data = metrics_by_target,
    source_generator = "aq_assess_multitarget_forecast",
    tags = c("vnext", "forecast", "multi_target", "assessment"),
    dependencies = forecast$forecast_id,
    version = "aq_multitarget_forecast_assessment_artifact_v1",
    metadata = list(
      artifact_type = "multi_target_forecast_assessment",
      forecast_id = forecast$forecast_id,
      forecast_spec_id = forecast$forecast_spec_id,
      aggregate_metrics = metrics,
      weighted_metrics = weighted_metrics,
      metrics_by_horizon = metrics_by_horizon,
      metrics_by_target_horizon = metrics_by_target_horizon,
      target_summary = target_summary,
      feature_usefulness_summary = feature_usefulness,
      cross_target_evidence = forecast$cross_target_evidence
    )
  )
  artifact <- aq_vnext_attach_envelope(
    artifact,
    artifact_id = assessment_id,
    artifact_type = "multi_target_forecast_assessment",
    artifact_version = "aq_multitarget_forecast_assessment_artifact_v1",
    parent_artifact_ids = forecast$forecast_id,
    lineage = list(forecast_id = forecast$forecast_id, forecast_spec_id = forecast$forecast_spec_id, partition_id = forecast$partition_id, targets = forecast$targets),
    task = "multi_target_forecast",
    operator = "forecast_assessment",
    engine = forecast$engine,
    specification_id = forecast$forecast_spec_id,
    dataset_id = forecast$spec$dataset_id,
    supported_actions = c("compare", "report", "campaign_review"),
    producer = "aq_assess_multitarget_forecast"
  )
  result <- list(
    assessment_id = assessment_id,
    schema_version = "aq_multitarget_forecast_assessment_result_v1",
    forecast_id = forecast$forecast_id,
    forecast_spec_id = forecast$forecast_spec_id,
    metrics = metrics,
    weighted_metrics = weighted_metrics,
    metrics_by_target = metrics_by_target,
    metrics_by_horizon = metrics_by_horizon,
    metrics_by_target_horizon = metrics_by_target_horizon,
    target_summary = target_summary,
    feature_usefulness_summary = feature_usefulness,
    cross_target_evidence = forecast$cross_target_evidence,
    assessment_artifact = artifact,
    comparison_ready = TRUE,
    created_at = aq_vnext_now()
  )
  class(result) <- c("aq_multitarget_forecast_assessment_result", "list")
  result
}

#' Compare Multi-Target Forecast Strategies
#'
#' @param spec An `aq_multitarget_forecast_spec`.
#' @param data Wide multi-target time-series data.
#' @param strategies Strategies to compare.
#' @param origin Optional forecast origin override.
#' @param future_data Optional future known-variable data.
#'
#' @return An `aq_multitarget_strategy_comparison_result`.
#' @export
aq_compare_multitarget_strategies <- function(
  spec,
  data,
  strategies = c("independent", "shared_workflow", "cross_target_features"),
  origin = NULL,
  future_data = NULL
) {
  strategies <- intersect(aq_vnext_unique_chr(strategies), aq_multitarget_strategy_levels())
  if (!length(strategies)) stop("at least one supported strategy is required.", call. = FALSE)
  forecasts <- list()
  assessments <- list()
  summary_rows <- lapply(strategies, function(strategy) {
    strategy_spec <- spec
    strategy_spec$strategy <- strategy
    fit <- tryCatch(aq_fit_multitarget_forecast(strategy_spec, data, origin = origin, future_data = future_data), error = function(e) e)
    if (inherits(fit, "error")) {
      return(data.table::data.table(strategy = strategy, rmse = NA_real_, mae = NA_real_, bias = NA_real_, assessed = FALSE, error = conditionMessage(fit)))
    }
    forecasts[[strategy]] <<- fit
    assessment <- tryCatch(aq_assess_multitarget_forecast(fit), error = function(e) e)
    if (inherits(assessment, "error")) {
      return(data.table::data.table(strategy = strategy, rmse = NA_real_, mae = NA_real_, bias = NA_real_, assessed = FALSE, error = conditionMessage(assessment)))
    }
    assessments[[strategy]] <<- assessment
    wide <- data.table::dcast(assessment$weighted_metrics, . ~ metric, value.var = "value")
    wide[, . := NULL]
    wide[, `:=`(strategy = strategy, assessed = TRUE, error = NA_character_)]
    wide
  })
  strategy_summary <- data.table::rbindlist(summary_rows, use.names = TRUE, fill = TRUE)
  data.table::setcolorder(strategy_summary, c("strategy", setdiff(names(strategy_summary), "strategy")))
  target_winners <- data.table::data.table()
  horizon_winners <- data.table::data.table()
  negative_transfer <- data.table::data.table()
  if (length(assessments)) {
    target_metrics <- data.table::rbindlist(lapply(names(assessments), function(strategy) {
      out <- data.table::copy(assessments[[strategy]]$metrics_by_target)
      out[, strategy := strategy]
      out
    }), use.names = TRUE, fill = TRUE)
    target_rmse <- target_metrics[metric == "rmse" & is.finite(value)]
    if (nrow(target_rmse)) {
      target_winners <- target_rmse[, .SD[which.min(value)], by = target][, .(target, winning_strategy = strategy, rmse = value)]
    }
    horizon_metrics <- data.table::rbindlist(lapply(names(assessments), function(strategy) {
      out <- data.table::copy(assessments[[strategy]]$metrics_by_horizon)
      out[, strategy := strategy]
      out
    }), use.names = TRUE, fill = TRUE)
    horizon_rmse <- horizon_metrics[metric == "rmse" & is.finite(value)]
    if (nrow(horizon_rmse)) {
      horizon_winners <- horizon_rmse[, .SD[which.min(value)], by = horizon][, .(horizon, winning_strategy = strategy, rmse = value)]
    }
    if (all(c("independent", "cross_target_features") %in% names(assessments))) {
      independent_rmse <- assessments$independent$metrics_by_target[metric == "rmse", .(target, independent_rmse = value)]
      cross_rmse <- assessments$cross_target_features$metrics_by_target[metric == "rmse", .(target, cross_target_rmse = value)]
      negative_transfer <- merge(independent_rmse, cross_rmse, by = "target", all = TRUE)
      negative_transfer[, `:=`(
        rmse_delta = cross_target_rmse - independent_rmse,
        negative_transfer = is.finite(cross_target_rmse) & is.finite(independent_rmse) & cross_target_rmse > independent_rmse
      )]
    }
  }
  recommendation <- if (!any(isTRUE(strategy_summary$assessed))) {
    "evidence_insufficient"
  } else {
    assessed <- strategy_summary[assessed == TRUE & is.finite(rmse)]
    if (!nrow(assessed)) {
      "evidence_insufficient"
    } else if (nrow(target_winners) && data.table::uniqueN(target_winners$winning_strategy) > 1L) {
      "strategy_varies_by_target"
    } else if (nrow(horizon_winners) && data.table::uniqueN(horizon_winners$winning_strategy) > 1L) {
      "strategy_varies_by_horizon"
    } else {
      best <- assessed[which.min(rmse), strategy]
      switch(
        best,
        independent = "independent_preferred",
        shared_workflow = "shared_workflow_preferred",
        cross_target_features = "cross_target_preferred",
        "evidence_insufficient"
      )
    }
  }
  comparison_id <- aq_vnext_id("multitarget_strategy_comparison")
  artifact <- new_table_artifact(
    id = comparison_id,
    title = "Multi-Target Strategy Comparison",
    data = strategy_summary,
    source_generator = "aq_compare_multitarget_strategies",
    tags = c("vnext", "forecast", "multi_target", "strategy_comparison"),
    dependencies = vapply(forecasts, `[[`, character(1L), "forecast_id"),
    version = "aq_multitarget_strategy_comparison_artifact_v1",
    metadata = list(
      artifact_type = "multi_target_strategy_comparison",
      comparison_id = comparison_id,
      forecast_spec_id = spec$forecast_spec_id,
      strategies = strategies,
      recommendation = recommendation,
      target_winners = target_winners,
      horizon_winners = horizon_winners,
      negative_transfer = negative_transfer,
      assessment_ids = vapply(assessments, `[[`, character(1L), "assessment_id")
    )
  )
  artifact <- aq_vnext_attach_envelope(
    artifact,
    artifact_id = comparison_id,
    artifact_type = "multi_target_strategy_comparison",
    artifact_version = "aq_multitarget_strategy_comparison_artifact_v1",
    parent_artifact_ids = vapply(forecasts, `[[`, character(1L), "forecast_id"),
    lineage = list(forecast_spec_id = spec$forecast_spec_id, strategies = strategies, targets = spec$targets),
    task = "multi_target_forecast",
    operator = "forecast_strategy_comparison",
    engine = spec$engine,
    specification_id = spec$forecast_spec_id,
    dataset_id = spec$dataset_id,
    supported_actions = c("compare", "report", "campaign_review"),
    producer = "aq_compare_multitarget_strategies"
  )
  result <- list(
    comparison_id = comparison_id,
    schema_version = "aq_multitarget_strategy_comparison_result_v1",
    forecast_spec_id = spec$forecast_spec_id,
    strategies = strategies,
    strategy_summary = strategy_summary,
    recommendation = recommendation,
    target_winners = target_winners,
    horizon_winners = horizon_winners,
    negative_transfer = negative_transfer,
    forecasts = forecasts,
    assessments = assessments,
    artifact = artifact,
    comparison_ready = TRUE,
    created_at = aq_vnext_now()
  )
  class(result) <- c("aq_multitarget_strategy_comparison_result", "list")
  result
}

aq_vnext_multitarget_forecast_fixture <- function(n = 36L) {
  dates <- seq.Date(as.Date("2025-01-01"), by = "week", length.out = n)
  idx <- seq_len(n)
  data.table::data.table(
    date = dates,
    leads = 100 + idx * 2 + sin(idx / 3) * 8,
    applications = 60 + idx * 1.4 + sin(idx / 3 + 0.3) * 5,
    enrollments = 28 + idx * 0.8 + sin(idx / 3 + 0.7) * 3,
    spend = 500 + idx * 5,
    promo = rep(c(0, 1, 0, 0), length.out = n)
  )
}

#' QA for vNext Multi-Target Forecasting Foundation
#'
#' @return A data.table of deterministic QA checks.
#' @export
qa_vnext_multitarget_forecasting_foundation <- function() {
  rows <- list()
  add <- function(check, ok, message = "") {
    rows[[length(rows) + 1L]] <<- data.table::data.table(
      suite = "vnext_multitarget_forecasting_foundation",
      check = check,
      status = if (isTRUE(ok)) "pass" else "fail",
      message = message
    )
  }
  dt <- aq_vnext_multitarget_forecast_fixture()
  targets <- c("leads", "applications", "enrollments")
  spec <- aq_multitarget_forecast_spec(
    targets = targets,
    date = "date",
    frequency = "week",
    horizon = 3L,
    forecast_origin = max(dt$date) - 21L,
    known_future_variables = "promo",
    shared_predictors = "spend",
    target_specific_predictors = list(enrollments = "promo"),
    strategy = "independent",
    engine = "seasonal_naive",
    season_length = 4L,
    dataset_id = "qa_multitarget_fixture"
  )
  validation <- aq_validate_multitarget_forecast_spec(spec, dt)
  add("multitarget_spec_constructed", inherits(spec, "aq_multitarget_forecast_spec"))
  add("multitarget_validation_passes", !aq_vnext_has_validation_error(validation), paste(validation$message, collapse = " | "))
  duplicate_spec <- aq_multitarget_forecast_spec(targets = c("a", "a"), date = "date")
  duplicate_validation <- aq_validate_multitarget_forecast_spec(duplicate_spec)
  add("duplicate_targets_detected", any(duplicate_validation$check == "target_count" & duplicate_validation$status == "fail") || any(duplicate_validation$check == "duplicate_targets" & duplicate_validation$status == "fail"))
  bad_dt <- data.table::copy(dt)
  bad_dt[, leads := as.character(leads)]
  bad_validation <- aq_validate_multitarget_forecast_spec(spec, bad_dt)
  add("non_numeric_target_detected", any(bad_validation$check == "target_types" & bad_validation$status == "fail"))
  duplicate_data_validation <- aq_validate_multitarget_forecast_spec(spec, data.table::rbindlist(list(dt, dt[1])))
  add("duplicate_date_target_detected", any(duplicate_data_validation$check == "duplicate_date_target_combinations" & duplicate_data_validation$status == "fail"))
  independent_fit <- aq_fit_multitarget_forecast(spec, dt)
  add("independent_forecast_result", inherits(independent_fit, "aq_multitarget_forecast_result") && nrow(independent_fit$data) == length(targets) * spec$horizon)
  add("cross_target_evidence", nrow(independent_fit$cross_target_evidence$target_correlations) == 3L && nrow(independent_fit$cross_target_evidence$forecast_divergence) == spec$horizon)
  add("multitarget_artifact_integrity", !aq_vnext_has_validation_error(aq_validate_artifact(independent_fit$artifact)) && identical(aq_artifact_envelope(independent_fit$artifact)$task, "multi_target_forecast"))
  shared_spec <- spec
  shared_spec$strategy <- "shared_workflow"
  shared_fit <- aq_fit_multitarget_forecast(shared_spec, dt)
  add("shared_workflow_forecast_result", inherits(shared_fit, "aq_multitarget_forecast_result") && identical(shared_fit$strategy, "shared_workflow"))
  assessment <- aq_assess_multitarget_forecast(independent_fit)
  add("multitarget_assessment", inherits(assessment, "aq_multitarget_forecast_assessment_result") && nrow(assessment$metrics_by_target) > 0L && nrow(assessment$weighted_metrics) > 0L)
  comparison <- aq_compare_multitarget_strategies(spec, dt)
  add("multitarget_strategy_comparison", inherits(comparison, "aq_multitarget_strategy_comparison_result") && all(c("independent", "shared_workflow") %in% comparison$strategy_summary$strategy))
  add("comparison_artifact_integrity", !aq_vnext_has_validation_error(aq_validate_artifact(comparison$artifact)) && identical(aq_artifact_envelope(comparison$artifact)$operator, "forecast_strategy_comparison"))
  app_like <- list(independent_fit$artifact, assessment$assessment_artifact, comparison$artifact)
  add("analytics_shinyapp_compatibility", all(vapply(app_like, function(x) inherits(x, "aq_artifact") && !is.null(x$artifact_envelope) && "campaign_review" %in% aq_supported_actions(x), logical(1L))))
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

#' QA for vNext Supervised Cross-Target Forecasting
#'
#' @return A data.table of deterministic QA checks.
#' @export
qa_vnext_multitarget_supervised_forecasting <- function() {
  rows <- list()
  add <- function(check, ok, message = "") {
    rows[[length(rows) + 1L]] <<- data.table::data.table(
      suite = "vnext_multitarget_supervised_forecasting",
      check = check,
      status = if (isTRUE(ok)) "pass" else "fail",
      message = message
    )
  }
  if (!requireNamespace("catboost", quietly = TRUE)) {
    add("catboost_available", FALSE, "catboost package is required for supervised multi-target QA.")
    return(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
  }
  if (!requireNamespace("Rodeo", quietly = TRUE) ||
      !"rodeo_prepare_cross_target_features" %in% getNamespaceExports("Rodeo")) {
    add("rodeo_cross_target_available", FALSE, "Rodeo cross-target feature helper is required for supervised multi-target QA.")
    return(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
  }
  dt <- aq_vnext_multitarget_forecast_fixture(52L)
  targets <- c("leads", "applications", "enrollments")
  params <- list(iterations = 5L, depth = 2L, learning_rate = 0.1, random_seed = 20260712L)
  spec <- aq_multitarget_forecast_spec(
    targets = targets,
    date = "date",
    frequency = "week",
    horizon = 3L,
    forecast_origin = max(dt$date) - 21L,
    known_future_variables = "promo",
    shared_predictors = "spend",
    target_specific_predictors = list(enrollments = "promo"),
    strategy = "cross_target_features",
    engine = "catboost",
    cross_target_feature_policy = "lags_rolls",
    shared_target_lags = c(1L, 2L),
    target_specific_lags = list(enrollments = c(1L, 3L)),
    shared_rolling_windows = 3L,
    target_relationship_metadata = list(source = "qa_fixture", causal_claim = FALSE),
    engine_parameters = params,
    dataset_id = "qa_multitarget_supervised_fixture"
  )
  validation <- aq_validate_multitarget_forecast_spec(spec, dt)
  add("cross_target_spec_constructed", inherits(spec, "aq_multitarget_forecast_spec") && identical(spec$strategy, "cross_target_features"))
  add("cross_target_validation_passes", !aq_vnext_has_validation_error(validation), paste(validation$message, collapse = " | "))
  add("leakage_diagnostics_declared", all(c("same_period_target_leakage", "future_target_leakage", "forecast_origin_leakage", "recursive_leakage") %in% validation$check))
  prep <- Rodeo::rodeo_prepare_cross_target_features(
    dt,
    date_col = "date",
    target_cols = targets,
    forecast_origin = spec$forecast_origin,
    lag_periods = c(1L, 2L),
    rolling_windows = 3L,
    known_future_variables = "promo",
    future_data = dt[date > spec$forecast_origin, .(date, promo)]
  )
  add("rodeo_cross_target_preparation", inherits(prep, "rodeo_cross_target_features") && all(c("cross_target_leads_lag_1", "cross_target_applications_lag_1", "date_dow") %in% prep$feature_columns))
  add("rodeo_leakage_policy", all(prep$diagnostics$status == "pass") && any(prep$feature_manifest$leakage_policy == "strictly_prior_to_prediction_date"))
  independent_spec <- spec
  independent_spec$strategy <- "independent"
  shared_spec <- spec
  shared_spec$strategy <- "shared_workflow"
  cross_fit <- aq_fit_multitarget_forecast(spec, dt)
  independent_fit <- aq_fit_multitarget_forecast(independent_spec, dt)
  shared_fit <- aq_fit_multitarget_forecast(shared_spec, dt)
  add("independent_catboost_forecast", inherits(independent_fit, "aq_multitarget_forecast_result") && identical(independent_fit$engine, "catboost"))
  add("shared_workflow_catboost_forecast", inherits(shared_fit, "aq_multitarget_forecast_result") && identical(shared_fit$strategy, "shared_workflow"))
  add("cross_target_catboost_forecast", inherits(cross_fit, "aq_multitarget_forecast_result") && nrow(cross_fit$data) == length(targets) * spec$horizon)
  add("cross_target_feature_importance", nrow(cross_fit$feature_importance) > 0L && "cross_target_other" %in% cross_fit$feature_importance$feature_role)
  add("cross_target_evidence_tables", nrow(cross_fit$cross_target_evidence$target_contribution_summary) > 0L && nrow(cross_fit$cross_target_evidence$cross_target_diagnostics) > 0L)
  assessment <- aq_assess_multitarget_forecast(cross_fit)
  add("cross_target_assessment", inherits(assessment, "aq_multitarget_forecast_assessment_result") && nrow(assessment$feature_usefulness_summary) > 0L)
  comparison <- aq_compare_multitarget_strategies(spec, dt)
  add("three_strategy_comparison", inherits(comparison, "aq_multitarget_strategy_comparison_result") && all(c("independent", "shared_workflow", "cross_target_features") %in% comparison$strategy_summary$strategy))
  add("phase20_recommendation_label", comparison$recommendation %in% c("independent_preferred", "shared_workflow_preferred", "cross_target_preferred", "strategy_varies_by_target", "strategy_varies_by_horizon", "evidence_insufficient"))
  add("negative_transfer_table", all(c("target", "independent_rmse", "cross_target_rmse", "rmse_delta", "negative_transfer") %in% names(comparison$negative_transfer)))
  add("artifact_integrity", !aq_vnext_has_validation_error(aq_validate_artifact(cross_fit$artifact)) && !aq_vnext_has_validation_error(aq_validate_artifact(comparison$artifact)))
  app_like <- list(cross_fit$artifact, assessment$assessment_artifact, comparison$artifact)
  add("analytics_shinyapp_compatibility", all(vapply(app_like, function(x) inherits(x, "aq_artifact") && !is.null(x$artifact_envelope) && "campaign_review" %in% aq_supported_actions(x), logical(1L))))
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}
