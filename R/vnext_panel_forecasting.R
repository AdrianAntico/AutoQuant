# AutoQuant vNext global panel forecasting foundation.

#' Create a vNext Panel Forecast Specification
#'
#' @param entity Entity identifier column.
#' @param target Target value column.
#' @param date Date/time column.
#' @param frequency Panel frequency. Supports `"auto"`, `"day"`, `"week"`,
#'   `"month"`, `"quarter"`, and `"year"`.
#' @param horizon Forecast horizon in periods per entity.
#' @param forecast_origin Optional common forecast origin.
#' @param future_known_variables Variables known at forecast time.
#' @param static_entity_features Entity-level static features.
#' @param engine Forecast engine. Phase 13 supports `"catboost"`.
#' @param forecast_strategy Forecast strategy. Supports `"direct"` and
#'   `"recursive"`.
#' @param engine_parameters Optional CatBoost and temporal feature parameters.
#' @param minimum_history Minimum non-missing target rows required per entity.
#' @param prediction_intervals Whether to request prediction intervals. CatBoost
#'   panel forecasts record unsupported interval diagnostics.
#' @param confidence_level Prediction interval confidence level.
#' @param rolling_origins Number of rolling origins for QA/backtesting helpers.
#' @param aggregation_level Optional aggregation label.
#' @param forecast_spec_id Optional specification id.
#' @param dataset_id Optional dataset id.
#' @param supported_downstream_actions Supported next actions.
#'
#' @return An `aq_panel_forecast_spec` object.
#' @export
aq_panel_forecast_spec <- function(
  entity,
  target,
  date,
  frequency = "auto",
  horizon = 1L,
  forecast_origin = NULL,
  future_known_variables = character(),
  static_entity_features = character(),
  engine = "catboost",
  forecast_strategy = "direct",
  engine_parameters = list(),
  minimum_history = 20L,
  prediction_intervals = FALSE,
  confidence_level = 0.95,
  rolling_origins = 3L,
  aggregation_level = "panel",
  forecast_spec_id = NULL,
  dataset_id = NULL,
  supported_downstream_actions = c("forecast", "assess", "compare", "report", "campaign_review")
) {
  frequency <- match.arg(tolower(frequency), aq_forecast_frequency_levels())
  engine <- match.arg(tolower(engine), "catboost")
  forecast_strategy <- match.arg(tolower(forecast_strategy), aq_forecast_strategy_levels())
  horizon <- as.integer(horizon)[1L]
  minimum_history <- as.integer(minimum_history)[1L]
  if (is.null(forecast_spec_id)) {
    forecast_spec_id <- aq_vnext_id(paste("panel_forecast_spec", engine, entity, target, sep = "_"))
  }
  spec <- list(
    forecast_spec_id = as.character(forecast_spec_id)[1L],
    schema_version = "aq_panel_forecast_spec_v1",
    entity = as.character(entity)[1L],
    target = as.character(target)[1L],
    date = as.character(date)[1L],
    frequency = frequency,
    horizon = horizon,
    forecast_origin = if (is.null(forecast_origin)) NULL else as.Date(forecast_origin)[1L],
    future_known_variables = aq_vnext_unique_chr(future_known_variables),
    static_entity_features = aq_vnext_unique_chr(static_entity_features),
    engine = engine,
    forecast_strategy = forecast_strategy,
    engine_parameters = if (is.null(engine_parameters)) list() else engine_parameters,
    minimum_history = minimum_history,
    prediction_intervals = isTRUE(prediction_intervals),
    confidence_level = as.numeric(confidence_level)[1L],
    rolling_origins = as.integer(rolling_origins)[1L],
    aggregation_level = as.character(aggregation_level)[1L],
    dataset_id = aq_vnext_default(dataset_id, NA_character_),
    supported_downstream_actions = aq_vnext_unique_chr(supported_downstream_actions),
    created_at = aq_vnext_now()
  )
  class(spec) <- c("aq_panel_forecast_spec", "list")
  spec
}

#' Validate a vNext Panel Forecast Specification
#'
#' @param spec An `aq_panel_forecast_spec`.
#' @param data Optional panel time-series data.
#' @param future_data Optional future known data.
#'
#' @return A `data.table` of deterministic validation diagnostics.
#' @export
aq_validate_panel_forecast_spec <- function(spec, data = NULL, future_data = NULL) {
  rows <- list()
  add <- function(check, status, message, severity = status) {
    rows[[length(rows) + 1L]] <<- aq_vnext_validation_table(check, status, message, severity)
  }
  if (!inherits(spec, "aq_panel_forecast_spec")) {
    add("panel_forecast_spec_class", "fail", "spec must be created by aq_panel_forecast_spec().")
    return(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
  }
  add("panel_forecast_spec_class", "pass", "panel forecast specification is typed.", "info")
  if (!is.finite(spec$horizon) || spec$horizon < 1L) add("horizon", "fail", "forecast horizon must be positive.") else add("horizon", "pass", paste("horizon:", spec$horizon), "info")
  if (!identical(spec$engine, "catboost")) add("engine_supported", "fail", "Phase 13 panel forecasting supports engine = 'catboost'.") else add("engine_supported", "pass", "engine: catboost", "info")
  if (!spec$forecast_strategy %in% aq_forecast_strategy_levels()) add("forecast_strategy", "fail", "unsupported forecast strategy.") else add("forecast_strategy", "pass", paste("strategy:", spec$forecast_strategy), "info")
  if (is.null(data)) return(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))

  dt <- data.table::as.data.table(data)
  required <- unique(c(spec$entity, spec$target, spec$date, spec$future_known_variables, spec$static_entity_features))
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
  duplicate_count <- nrow(dt[, .N, by = c(spec$entity, spec$date)][N > 1L])
  if (duplicate_count > 0L) add("duplicate_entity_date", "fail", paste("duplicate entity/date pair count:", duplicate_count)) else add("duplicate_entity_date", "pass", "entity/date pairs are unique.", "info")
  history <- dt[!is.na(get(spec$target)), .N, by = c(spec$entity)]
  sparse <- history[N < spec$minimum_history]
  if (nrow(sparse)) add("entity_history_length", "warning", paste("entities below minimum history:", nrow(sparse)), "warning") else add("entity_history_length", "pass", "all entities meet minimum history.", "info")
  if (length(spec$static_entity_features)) {
    inconsistent <- unlist(dt[, lapply(.SD, data.table::uniqueN), by = c(spec$entity), .SDcols = spec$static_entity_features][, lapply(.SD, function(x) any(x > 1L)), .SDcols = spec$static_entity_features], use.names = FALSE)
    if (any(inconsistent)) add("static_entity_consistency", "warning", "one or more static entity features vary within entity.", "warning") else add("static_entity_consistency", "pass", "static entity features are stable by entity.", "info")
  }
  if (!is.null(future_data)) {
    fdt <- data.table::as.data.table(future_data)
    future_required <- unique(c(spec$entity, spec$date, spec$future_known_variables, spec$static_entity_features))
    future_missing <- setdiff(future_required, names(fdt))
    if (length(future_missing)) add("future_schema", "fail", paste("future data missing column(s):", paste(future_missing, collapse = ", "))) else add("future_schema", "pass", "future schema is present.", "info")
    train_entities <- sort(unique(as.character(dt[[spec$entity]])))
    future_entities <- sort(unique(as.character(fdt[[spec$entity]])))
    new_entities <- setdiff(future_entities, train_entities)
    missing_entities <- setdiff(train_entities, future_entities)
    if (length(new_entities)) add("cold_start_new_entities", "warning", paste("new future entities:", length(new_entities)), "warning") else add("cold_start_new_entities", "pass", "no new future entities detected.", "info")
    if (length(missing_entities)) add("future_entity_coverage", "warning", paste("training entities missing from future data:", length(missing_entities)), "warning") else add("future_entity_coverage", "pass", "future data covers training entities.", "info")
  }
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

#' Create an Entity-Aware Panel Forecast Partition
#'
#' @export
aq_panel_forecast_partition <- function(spec, data, origin = NULL) {
  if (!inherits(spec, "aq_panel_forecast_spec")) stop("spec must be an aq_panel_forecast_spec.", call. = FALSE)
  dt <- data.table::as.data.table(data)
  dt[, .aq_forecast_date := as.Date(get(spec$date))]
  data.table::setorderv(dt, c(spec$entity, ".aq_forecast_date"))
  frequency <- aq_forecast_resolved_frequency(list(frequency = spec$frequency, date = spec$date), dt)
  origin <- if (is.null(origin)) spec$forecast_origin else as.Date(origin)[1L]
  if (is.null(origin)) {
    unique_dates <- sort(unique(dt$.aq_forecast_date))
    origin <- unique_dates[max(1L, length(unique_dates) - spec$horizon)]
  }
  origin <- as.Date(origin)
  future_dates <- aq_forecast_next_dates(origin, frequency, spec$horizon)
  train_index <- which(dt$.aq_forecast_date <= origin)
  eval_index <- which(dt$.aq_forecast_date %in% future_dates)
  partition <- list(
    partition_id = aq_vnext_id(paste0("panel_forecast_partition_", spec$forecast_spec_id)),
    schema_version = "aq_panel_forecast_partition_v1",
    forecast_origin = origin,
    horizon = spec$horizon,
    frequency = frequency,
    future_dates = future_dates,
    entities = sort(unique(as.character(dt[[spec$entity]]))),
    train_index = train_index,
    evaluation_index = eval_index,
    summary = data.table::data.table(
      entity_count = length(unique(dt[[spec$entity]])),
      training_rows = length(train_index),
      evaluation_rows = length(eval_index),
      forecast_origin = origin,
      horizon = spec$horizon
    )
  )
  class(partition) <- c("aq_panel_forecast_partition", "list")
  partition
}

aq_panel_future_data_context <- function(spec, train, eval, partition, future_data = NULL) {
  future <- if (!is.null(future_data)) data.table::as.data.table(data.table::copy(future_data)) else data.table::copy(eval)
  if (!nrow(future)) {
    future <- data.table::CJ(.entity_tmp = partition$entities, .date_tmp = partition$future_dates)
    data.table::setnames(future, c(".entity_tmp", ".date_tmp"), c(spec$entity, spec$date))
  }
  if (!spec$date %in% names(future)) future[[spec$date]] <- future$.aq_forecast_date
  future[, .aq_forecast_date := as.Date(get(spec$date))]
  data.table::setorderv(future, c(spec$entity, ".aq_forecast_date"))
  future[, .rodeo_panel_horizon := seq_len(.N), by = c(spec$entity)]
  validation <- aq_validate_panel_forecast_spec(spec, train, future_data = future)
  list(data = future, diagnostics = validation, metadata = list(future_rows = nrow(future), future_entities = length(unique(future[[spec$entity]]))))
}

aq_fit_panel_catboost_direct <- function(train, spec, partition, future_context, temporal_fit) {
  frames <- Rodeo::rodeo_prepare_forecast_supervised_data(temporal_fit, future_data = future_context$data, horizon = spec$horizon, strategy = "direct")
  predictions <- list()
  models <- vector("list", spec$horizon)
  feature_importance <- list()
  rows_by_horizon <- integer(spec$horizon)
  for (h in seq_len(spec$horizon)) {
    frame <- frames$training_frames[[as.character(h)]]
    feature_cols <- frames$feature_columns_by_horizon[[as.character(h)]]
    trained <- aq_forecast_catboost_train_one(frame, ".rodeo_label", feature_cols, spec)
    pred_frame <- frames$prediction_frames[[as.character(h)]]
    pred <- as.numeric(catboost::catboost.predict(trained$model, pool = catboost::catboost.load_pool(data = aq_forecast_numeric_matrix(pred_frame, feature_cols)), prediction_type = "RawFormulaVal"))
    predictions[[h]] <- data.table::data.table(
      entity = as.character(pred_frame[[spec$entity]]),
      forecast_date = as.Date(pred_frame$.rodeo_future_date),
      horizon = h,
      forecast = pred
    )
    data.table::setnames(predictions[[h]], "entity", spec$entity)
    models[[h]] <- trained$model
    feature_importance[[h]] <- data.table::copy(trained$feature_importance)[, horizon := h]
    rows_by_horizon[[h]] <- trained$rows
  }
  list(
    prediction = data.table::rbindlist(predictions, use.names = TRUE, fill = TRUE),
    model = list(strategy = "direct", models = models),
    feature_importance = data.table::rbindlist(feature_importance, use.names = TRUE, fill = TRUE),
    training_rows_by_horizon = rows_by_horizon,
    feature_manifest = frames$feature_manifest
  )
}

aq_fit_panel_catboost_recursive <- function(train, spec, partition, future_context, temporal_fit) {
  frames <- Rodeo::rodeo_prepare_forecast_supervised_data(temporal_fit, future_data = future_context$data, horizon = spec$horizon, strategy = "recursive")
  trained <- aq_forecast_catboost_train_one(frames$training_frame, ".rodeo_label", frames$feature_columns, spec)
  histories <- split(as.numeric(train[[spec$target]]), as.character(train[[spec$entity]]))
  predictions <- list()
  for (h in seq_len(spec$horizon)) {
    future_h <- future_context$data[, .SD[.rodeo_panel_horizon == h], by = c(spec$entity)]
    rows <- lapply(seq_len(nrow(future_h)), function(i) {
      ent <- as.character(future_h[[spec$entity]][i])
      row <- Rodeo::rodeo_temporal_prediction_frame(temporal_fit, future_h[i], history_values = histories[[ent]])
      row[[spec$entity]] <- ent
      row[[spec$date]] <- future_h[[spec$date]][i]
      row
    })
    pred_frame <- data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
    pred <- as.numeric(catboost::catboost.predict(trained$model, pool = catboost::catboost.load_pool(data = aq_forecast_numeric_matrix(pred_frame, frames$feature_columns)), prediction_type = "RawFormulaVal"))
    out <- data.table::data.table(entity = pred_frame[[spec$entity]], forecast_date = as.Date(pred_frame$.rodeo_future_date), horizon = h, forecast = pred)
    data.table::setnames(out, "entity", spec$entity)
    predictions[[h]] <- out
    for (i in seq_along(pred)) {
      ent <- as.character(out[[spec$entity]][i])
      histories[[ent]] <- c(histories[[ent]], pred[[i]])
    }
  }
  list(
    prediction = data.table::rbindlist(predictions, use.names = TRUE, fill = TRUE),
    model = list(strategy = "recursive", model = trained$model),
    feature_importance = trained$feature_importance,
    training_rows_by_horizon = trained$rows,
    feature_manifest = frames$feature_manifest
  )
}

#' Fit a Global Panel Forecast
#'
#' @export
aq_fit_panel_forecast <- function(spec, data, origin = NULL, future_data = NULL) {
  if (!inherits(spec, "aq_panel_forecast_spec")) stop("spec must be an aq_panel_forecast_spec.", call. = FALSE)
  if (!requireNamespace("catboost", quietly = TRUE)) stop("The catboost package is required for aq_fit_panel_forecast().", call. = FALSE)
  aq_forecast_require_rodeo_temporal()
  validation <- aq_validate_panel_forecast_spec(spec, data, future_data = future_data)
  if (aq_vnext_has_validation_error(validation)) stop(paste(validation[status %in% c("fail", "error"), message], collapse = " "), call. = FALSE)
  dt <- data.table::as.data.table(data.table::copy(data))
  dt[, .aq_forecast_date := as.Date(get(spec$date))]
  partition <- aq_panel_forecast_partition(spec, dt, origin = origin)
  train <- dt[partition$train_index]
  eval <- dt[partition$evaluation_index]
  future_context <- aq_panel_future_data_context(spec, train, eval, partition, future_data = future_data)
  if (aq_vnext_has_validation_error(future_context$diagnostics)) stop(paste(future_context$diagnostics[status %in% c("fail", "error"), message], collapse = " "), call. = FALSE)
  settings <- aq_forecast_catboost_feature_settings(spec, partition$frequency)
  temporal_spec <- Rodeo::rodeo_temporal_transformation_spec(
    date_col = spec$date,
    target_col = spec$target,
    frequency = partition$frequency,
    calendar_features = settings$date_features,
    lag_periods = settings$lag_periods,
    rolling_windows = settings$rolling_windows,
    known_future_variables = spec$future_known_variables,
    static_entity_features = spec$static_entity_features,
    entity_id = spec$entity,
    forecast_horizon = spec$horizon,
    metadata = list(producer = "AutoQuant", consumer = "aq_fit_panel_forecast", engine = "catboost")
  )
  temporal_fit <- Rodeo::rodeo_fit_temporal_transformation(train, temporal_spec, forecast_origin = partition$forecast_origin)
  temporal_metadata <- Rodeo::rodeo_temporal_transformation_metadata(temporal_fit)
  elapsed <- system.time({
    prepared <- if (identical(spec$forecast_strategy, "recursive")) {
      aq_fit_panel_catboost_recursive(train, spec, partition, future_context, temporal_fit)
    } else {
      aq_fit_panel_catboost_direct(train, spec, partition, future_context, temporal_fit)
    }
  })
  forecast_dt <- prepared$prediction
  actuals <- eval[, .(actual = as.numeric(get(spec$target))), by = c(spec$entity, ".aq_forecast_date")]
  data.table::setnames(actuals, ".aq_forecast_date", "forecast_date")
  forecast_dt <- merge(forecast_dt, actuals, by = c(spec$entity, "forecast_date"), all.x = TRUE, sort = FALSE)
  data.table::setorderv(forecast_dt, c(spec$entity, "horizon"))
  intervals <- aq_forecast_no_interval_columns(nrow(forecast_dt), spec$confidence_level, "Global CatBoost panel forecasts do not provide native prediction intervals in this contract.")
  forecast_dt <- data.table::as.data.table(cbind(forecast_dt, intervals))
  forecast_id <- aq_vnext_id("panel_forecast_catboost")
  artifact <- new_table_artifact(
    id = forecast_id,
    title = "Panel Forecast: Global CatBoost",
    data = forecast_dt,
    source_generator = "aq_fit_panel_forecast",
    tags = c("vnext", "forecast", "panel", "catboost"),
    dependencies = spec$forecast_spec_id,
    version = "aq_panel_forecast_artifact_v1",
    metadata = list(
      artifact_type = "panel_forecast_catboost",
      forecast_id = forecast_id,
      forecast_spec_id = spec$forecast_spec_id,
      partition_id = partition$partition_id,
      entity = spec$entity,
      entity_count = length(partition$entities),
      target = spec$target,
      date = spec$date,
      engine = spec$engine,
      forecast_strategy = spec$forecast_strategy,
      frequency = partition$frequency,
      forecast_origin = partition$forecast_origin,
      horizon = spec$horizon,
      temporal_metadata = temporal_metadata,
      feature_manifest = prepared$feature_manifest,
      feature_importance = prepared$feature_importance,
      future_context = future_context$metadata,
      cold_start_diagnostics = future_context$diagnostics[check %in% c("cold_start_new_entities", "entity_history_length", "future_entity_coverage")],
      engine_diagnostics = list(
        engine = "catboost",
        forecast_strategy = spec$forecast_strategy,
        convergence = "completed",
        global_model = TRUE,
        training_rows_by_horizon = prepared$training_rows_by_horizon,
        training_duration_seconds = unname(elapsed[["elapsed"]])
      ),
      supported_downstream_actions = c("assess", "compare", "report", "campaign_review")
    )
  )
  artifact <- aq_vnext_attach_envelope(
    artifact,
    artifact_id = forecast_id,
    artifact_type = "panel_forecast_catboost",
    artifact_version = "aq_panel_forecast_artifact_v1",
    parent_artifact_ids = spec$forecast_spec_id,
    lineage = list(
      forecast_spec_id = spec$forecast_spec_id,
      partition_id = partition$partition_id,
      temporal_transformation_identity = temporal_metadata$temporal_transformation_identity,
      prepared_temporal_dataset_identity = temporal_metadata$prepared_temporal_dataset_identity,
      entity_count = length(partition$entities),
      forecast_origin = partition$forecast_origin,
      horizon = spec$horizon
    ),
    task = "panel_forecast",
    operator = "forecast",
    engine = "catboost",
    specification_id = spec$forecast_spec_id,
    dataset_id = spec$dataset_id,
    supported_actions = c("assess", "compare", "report", "campaign_review"),
    producer = "aq_fit_panel_forecast"
  )
  result <- list(
    forecast_id = forecast_id,
    status = "success",
    schema_version = "aq_panel_forecast_result_v1",
    spec = spec,
    forecast_spec_id = spec$forecast_spec_id,
    partition = partition,
    partition_id = partition$partition_id,
    task = "panel_forecast",
    engine = "catboost",
    forecast_strategy = spec$forecast_strategy,
    entity = spec$entity,
    target = spec$target,
    date = spec$date,
    frequency = partition$frequency,
    forecast_origin = partition$forecast_origin,
    horizon = spec$horizon,
    data = forecast_dt,
    model = prepared$model,
    temporal_metadata = temporal_metadata,
    feature_manifest = prepared$feature_manifest,
    feature_importance = prepared$feature_importance,
    future_context = future_context,
    engine_diagnostics = artifact$metadata$engine_diagnostics,
    artifact = artifact,
    validation = validation,
    comparison_ready = TRUE,
    created_at = aq_vnext_now()
  )
  class(result) <- c("aq_panel_forecast_result", "list")
  result
}

#' Assess Panel Forecast Evidence
#'
#' @export
aq_assess_panel_forecast <- function(forecast) {
  if (!inherits(forecast, "aq_panel_forecast_result")) stop("forecast must be an aq_panel_forecast_result.", call. = FALSE)
  if (!"actual" %in% names(forecast$data) || all(is.na(forecast$data$actual))) stop("panel forecast result does not contain realized actuals for assessment.", call. = FALSE)
  entity_col <- forecast$entity
  metrics <- aq_forecast_metrics(forecast$data)
  by_entity <- forecast$data[, aq_forecast_metrics(.SD), by = c(entity_col)]
  by_horizon <- forecast$data[, aq_forecast_metrics(.SD), by = horizon]
  by_entity_horizon <- forecast$data[, aq_forecast_metrics(.SD), by = c(entity_col, "horizon")]
  entity_summary <- forecast$data[, .(rows = .N, assessed_rows = sum(stats::complete.cases(actual, forecast)), mean_actual = mean(actual, na.rm = TRUE), mean_forecast = mean(forecast, na.rm = TRUE)), by = c(entity_col)]
  assessment_id <- aq_vnext_id("panel_forecast_assessment")
  artifact <- new_table_artifact(
    id = assessment_id,
    title = "Panel Forecast Assessment",
    data = by_entity,
    source_generator = "aq_assess_panel_forecast",
    tags = c("vnext", "forecast", "panel", "assessment"),
    dependencies = forecast$forecast_id,
    version = "aq_panel_forecast_assessment_artifact_v1",
    metadata = list(
      artifact_type = "panel_forecast_assessment",
      forecast_id = forecast$forecast_id,
      aggregate_metrics = metrics,
      metrics_by_horizon = by_horizon,
      metrics_by_entity_horizon = by_entity_horizon,
      entity_summary = entity_summary
    )
  )
  artifact <- aq_vnext_attach_envelope(
    artifact,
    artifact_id = assessment_id,
    artifact_type = "panel_forecast_assessment",
    artifact_version = "aq_panel_forecast_assessment_artifact_v1",
    parent_artifact_ids = forecast$forecast_id,
    lineage = list(forecast_id = forecast$forecast_id, forecast_spec_id = forecast$forecast_spec_id, partition_id = forecast$partition_id),
    task = "panel_forecast",
    operator = "forecast_assessment",
    engine = forecast$engine,
    specification_id = forecast$forecast_spec_id,
    dataset_id = forecast$spec$dataset_id,
    supported_actions = c("compare", "report", "campaign_review"),
    producer = "aq_assess_panel_forecast"
  )
  result <- list(
    assessment_id = assessment_id,
    schema_version = "aq_panel_forecast_assessment_result_v1",
    forecast_id = forecast$forecast_id,
    forecast_spec_id = forecast$forecast_spec_id,
    partition_id = forecast$partition_id,
    metrics = metrics,
    metrics_by_entity = by_entity,
    metrics_by_horizon = by_horizon,
    metrics_by_entity_horizon = by_entity_horizon,
    entity_summary = entity_summary,
    assessment_artifact = artifact,
    comparison_ready = TRUE,
    created_at = aq_vnext_now()
  )
  class(result) <- c("aq_panel_forecast_assessment_result", "list")
  result
}

#' Run Deterministic Rolling-Origin Panel Forecast Evaluation
#'
#' @export
aq_rolling_origin_panel_forecast <- function(spec, data, origins = NULL, origin_count = NULL) {
  if (!inherits(spec, "aq_panel_forecast_spec")) stop("spec must be an aq_panel_forecast_spec.", call. = FALSE)
  dt <- data.table::as.data.table(data.table::copy(data))
  dt[, .aq_forecast_date := as.Date(get(spec$date))]
  unique_dates <- sort(unique(dt$.aq_forecast_date))
  if (is.null(origins)) {
    origin_count <- as.integer(aq_vnext_default(origin_count, spec$rolling_origins))[1L]
    possible <- unique_dates[seq_len(max(0L, length(unique_dates) - spec$horizon))]
    origins <- utils::tail(possible, origin_count)
  }
  forecasts <- lapply(as.Date(origins), function(origin) aq_fit_panel_forecast(spec, dt, origin = origin))
  assessments <- lapply(forecasts, aq_assess_panel_forecast)
  result <- list(
    backtest_id = aq_vnext_id("panel_forecast_backtest"),
    schema_version = "aq_panel_forecast_backtest_result_v1",
    forecast_spec_id = spec$forecast_spec_id,
    origins = as.Date(origins),
    forecasts = forecasts,
    assessments = assessments,
    metrics = data.table::rbindlist(lapply(seq_along(assessments), function(i) data.table::copy(assessments[[i]]$metrics)[, forecast_origin := forecasts[[i]]$forecast_origin]), use.names = TRUE, fill = TRUE),
    created_at = aq_vnext_now()
  )
  class(result) <- c("aq_panel_forecast_backtest_result", "list")
  result
}

aq_vnext_panel_forecast_fixture <- function(entity_count = 4L, n = 70L) {
  dates <- as.Date("2024-01-01") + seq_len(n) - 1L
  out <- data.table::rbindlist(lapply(seq_len(entity_count), function(i) {
    data.table::data.table(
      entity = paste0("store_", i),
      date = dates,
      demand = 50 + i * 20 + seq_len(n) * (0.15 + i * 0.02) + sin(seq_len(n) / 5) * 4 + rep(c(0, 8), length.out = n),
      promo = rep(c(0, 1, 0, 0), length.out = n),
      store_size = 100 + i * 50
    )
  }))
  out[, demand := as.numeric(demand)]
  out
}

#' QA for vNext Panel Forecasting Foundation
#'
#' @export
qa_vnext_panel_forecasting_foundation <- function() {
  rows <- list()
  add <- function(check, ok, message = "") {
    rows[[length(rows) + 1L]] <<- data.table::data.table(
      suite = "vnext_panel_forecasting_foundation",
      check = check,
      status = if (isTRUE(ok)) "pass" else "fail",
      message = message
    )
  }
  dt <- aq_vnext_panel_forecast_fixture()
  spec <- aq_panel_forecast_spec(
    entity = "entity",
    target = "demand",
    date = "date",
    frequency = "day",
    horizon = 3L,
    future_known_variables = "promo",
    static_entity_features = "store_size",
    forecast_strategy = "direct",
    engine_parameters = list(iterations = 5L, depth = 2L, learning_rate = 0.1, lag_periods = c(1L, 7L), rolling_windows = 3L, date_features = c("year", "month", "day", "dow", "day_index")),
    minimum_history = 20L,
    dataset_id = "qa_panel_forecast_fixture"
  )
  validation <- aq_validate_panel_forecast_spec(spec, dt)
  duplicate_validation <- aq_validate_panel_forecast_spec(spec, rbind(dt, dt[1]))
  partition <- aq_panel_forecast_partition(spec, dt)
  add("panel_spec_constructed", inherits(spec, "aq_panel_forecast_spec"))
  add("panel_validation_passes", !aq_vnext_has_validation_error(validation), paste(validation$message, collapse = " | "))
  add("duplicate_entity_date_detected", any(duplicate_validation$check == "duplicate_entity_date" & duplicate_validation$status == "fail"))
  add("entity_partition_constructed", inherits(partition, "aq_panel_forecast_partition") && length(partition$entities) == 4L)
  if (requireNamespace("catboost", quietly = TRUE)) {
    forecast <- aq_fit_panel_forecast(spec, dt)
    assessment <- aq_assess_panel_forecast(forecast)
    add("global_catboost_direct_forecast", inherits(forecast, "aq_panel_forecast_result") && nrow(forecast$data) == 12L)
    add("panel_artifact_integrity", !aq_vnext_has_validation_error(aq_validate_artifact(forecast$artifact)))
    add("rodeo_panel_replay_metadata", identical(forecast$temporal_metadata$replay_status, "ready") && "entity_identity" %in% forecast$feature_manifest$source)
    add("entity_assessment", inherits(assessment, "aq_panel_forecast_assessment_result") && nrow(assessment$metrics_by_entity) > 0L)
    add("horizon_assessment", data.table::uniqueN(assessment$metrics_by_horizon$horizon) == 3L)
    recursive_spec <- spec
    recursive_spec$forecast_strategy <- "recursive"
    recursive_spec$forecast_spec_id <- aq_vnext_id("panel_forecast_spec_recursive")
    recursive_forecast <- aq_fit_panel_forecast(recursive_spec, dt)
    add("global_catboost_recursive_forecast", inherits(recursive_forecast, "aq_panel_forecast_result") && identical(recursive_forecast$forecast_strategy, "recursive"))
    backtest <- aq_rolling_origin_panel_forecast(spec, dt, origin_count = 2L)
    add("panel_rolling_origin", inherits(backtest, "aq_panel_forecast_backtest_result") && length(backtest$forecasts) == 2L)
    serialized <- unserialize(serialize(list(forecast = forecast$artifact, assessment = assessment$assessment_artifact), NULL))
    add("panel_bundle_compatibility", all(vapply(serialized, function(x) !aq_vnext_has_validation_error(aq_validate_artifact(x)), logical(1L))))
    app_like <- list(forecast$artifact, assessment$assessment_artifact)
    add("analytics_shinyapp_compatibility", all(vapply(app_like, is.list, logical(1L))) && all(vapply(app_like, function(x) "campaign_review" %in% aq_supported_actions(x), logical(1L))))
  } else {
    add("catboost_available", FALSE, "catboost package is required for panel forecast QA.")
  }
  sparse <- data.table::copy(dt)[entity == "store_1" & date <= min(date) + 5]
  sparse <- data.table::rbindlist(list(sparse, dt[entity != "store_1"]))
  sparse_validation <- aq_validate_panel_forecast_spec(spec, sparse)
  add("cold_start_sparse_history_diagnostic", any(sparse_validation$check == "entity_history_length" & sparse_validation$status == "warning"))
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}
