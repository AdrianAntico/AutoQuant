# AutoQuant vNext panel strategy selection and negative transfer evidence.

aq_panel_candidate_strategy_levels <- function() {
  c("independent", "grouped", "global")
}

aq_panel_strategy_recommendation_levels <- function() {
  c("independent_preferred", "grouped_preferred", "global_preferred", "hybrid_worth_investigating", "evidence_insufficient")
}

#' Create a vNext Panel Strategy Specification
#'
#' @param entity Entity identifier column.
#' @param target Target value column.
#' @param date Date/time column.
#' @param group Optional explicit group column for grouped forecasting.
#' @param candidate_strategies Strategies to compare.
#' @param frequency Panel frequency.
#' @param horizon Forecast horizon.
#' @param forecast_origin Optional common forecast origin.
#' @param future_known_variables Variables known at forecast time.
#' @param static_entity_features Entity-level static features.
#' @param engine Forecast engine. Phase 15 uses `"catboost"`.
#' @param forecast_strategy CatBoost forecast strategy.
#' @param engine_parameters Engine parameters passed to forecast candidates.
#' @param comparison_metrics Metrics used for comparison.
#' @param evaluation_horizons Optional horizons to emphasize.
#' @param entity_weighting Entity weighting policy.
#' @param cold_start_policy Cold-start policy label.
#' @param minimum_history Minimum history required per entity.
#' @param rolling_origins Number of rolling origins.
#' @param negative_transfer_tolerance Relative degradation tolerated before
#'   flagging negative transfer.
#' @param strategy_spec_id Optional spec id.
#' @param dataset_id Optional dataset id.
#' @param supported_downstream_actions Supported next actions.
#'
#' @return An `aq_panel_strategy_spec`.
#' @export
aq_panel_strategy_spec <- function(
  entity,
  target,
  date,
  group = NULL,
  candidate_strategies = c("independent", "grouped", "global"),
  frequency = "auto",
  horizon = 1L,
  forecast_origin = NULL,
  future_known_variables = character(),
  static_entity_features = character(),
  engine = "catboost",
  forecast_strategy = "direct",
  engine_parameters = list(),
  comparison_metrics = c("rmse", "mae", "bias"),
  evaluation_horizons = NULL,
  entity_weighting = c("equal_entity", "row_count"),
  cold_start_policy = c("diagnostic_only", "exclude"),
  minimum_history = 20L,
  rolling_origins = 2L,
  negative_transfer_tolerance = 0.05,
  strategy_spec_id = NULL,
  dataset_id = NULL,
  supported_downstream_actions = c("compare", "recommend", "report", "campaign_review")
) {
  frequency <- match.arg(tolower(frequency), aq_forecast_frequency_levels())
  engine <- match.arg(tolower(engine), "catboost")
  forecast_strategy <- match.arg(tolower(forecast_strategy), aq_forecast_strategy_levels())
  entity_weighting <- match.arg(tolower(entity_weighting), c("equal_entity", "row_count"))
  cold_start_policy <- match.arg(tolower(cold_start_policy), c("diagnostic_only", "exclude"))
  candidate_strategies <- intersect(aq_vnext_unique_chr(tolower(candidate_strategies)), aq_panel_candidate_strategy_levels())
  if (!length(candidate_strategies)) candidate_strategies <- c("independent", "global")
  horizon <- as.integer(horizon)[1L]
  if (is.null(strategy_spec_id)) {
    strategy_spec_id <- aq_vnext_id(paste("panel_strategy_spec", entity, target, sep = "_"))
  }
  spec <- list(
    strategy_spec_id = as.character(strategy_spec_id)[1L],
    schema_version = "aq_panel_strategy_spec_v1",
    entity = as.character(entity)[1L],
    target = as.character(target)[1L],
    date = as.character(date)[1L],
    group = if (is.null(group)) NA_character_ else as.character(group)[1L],
    candidate_strategies = candidate_strategies,
    frequency = frequency,
    horizon = horizon,
    forecast_origin = if (is.null(forecast_origin)) NULL else as.Date(forecast_origin)[1L],
    future_known_variables = aq_vnext_unique_chr(future_known_variables),
    static_entity_features = aq_vnext_unique_chr(static_entity_features),
    engine = engine,
    forecast_strategy = forecast_strategy,
    engine_parameters = if (is.null(engine_parameters)) list() else engine_parameters,
    comparison_metrics = intersect(aq_vnext_unique_chr(comparison_metrics), aq_forecast_metric_levels()),
    evaluation_horizons = if (is.null(evaluation_horizons)) seq_len(horizon) else as.integer(evaluation_horizons),
    entity_weighting = entity_weighting,
    cold_start_policy = cold_start_policy,
    minimum_history = as.integer(minimum_history)[1L],
    rolling_origins = as.integer(rolling_origins)[1L],
    negative_transfer_tolerance = as.numeric(negative_transfer_tolerance)[1L],
    dataset_id = aq_vnext_default(dataset_id, NA_character_),
    supported_downstream_actions = aq_vnext_unique_chr(supported_downstream_actions),
    created_at = aq_vnext_now()
  )
  class(spec) <- c("aq_panel_strategy_spec", "list")
  spec
}

#' Validate a vNext Panel Strategy Specification
#'
#' @param spec An `aq_panel_strategy_spec`.
#' @param data Optional panel data.
#'
#' @return A deterministic diagnostic table.
#' @export
aq_validate_panel_strategy_spec <- function(spec, data = NULL) {
  rows <- list()
  add <- function(check, status, message, severity = status) {
    rows[[length(rows) + 1L]] <<- aq_vnext_validation_table(check, status, message, severity)
  }
  if (!inherits(spec, "aq_panel_strategy_spec")) {
    add("panel_strategy_spec_class", "fail", "spec must be created by aq_panel_strategy_spec().")
    return(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
  }
  add("panel_strategy_spec_class", "pass", "panel strategy specification is typed.", "info")
  if (!is.finite(spec$horizon) || spec$horizon < 1L) add("horizon", "fail", "horizon must be positive.") else add("horizon", "pass", paste("horizon:", spec$horizon), "info")
  if (!all(spec$candidate_strategies %in% aq_panel_candidate_strategy_levels())) add("candidate_strategies", "fail", "unsupported candidate strategy.") else add("candidate_strategies", "pass", paste(spec$candidate_strategies, collapse = ", "), "info")
  if ("grouped" %in% spec$candidate_strategies && (is.na(spec$group) || !nzchar(spec$group))) add("grouped_strategy_group", "fail", "grouped strategy requires an explicit group column.") else add("grouped_strategy_group", "pass", "grouped strategy configuration is compatible.", "info")
  if (is.null(data)) return(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
  dt <- data.table::as.data.table(data)
  required <- unique(c(spec$entity, spec$target, spec$date, spec$future_known_variables, spec$static_entity_features, if (!is.na(spec$group)) spec$group))
  missing <- setdiff(required, names(dt))
  if (length(missing)) {
    add("required_columns", "fail", paste("Missing required column(s):", paste(missing, collapse = ", ")))
    return(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
  }
  add("required_columns", "pass", "required columns are present.", "info")
  if (!is.numeric(dt[[spec$target]])) add("target_type", "fail", "target must be numeric.") else add("target_type", "pass", "target is numeric.", "info")
  duplicate_count <- nrow(dt[, .N, by = c(spec$entity, spec$date)][N > 1L])
  if (duplicate_count) add("duplicate_entity_date", "fail", paste("duplicate entity/date pair count:", duplicate_count)) else add("duplicate_entity_date", "pass", "entity/date pairs are unique.", "info")
  history <- dt[!is.na(get(spec$target)), .N, by = c(spec$entity)]
  sparse <- history[N < spec$minimum_history]
  if (nrow(sparse)) add("entity_history_length", "warning", paste("entities below minimum history:", nrow(sparse)), "warning") else add("entity_history_length", "pass", "all entities meet minimum history.", "info")
  if ("grouped" %in% spec$candidate_strategies && !is.na(spec$group)) {
    group_count <- data.table::uniqueN(dt[[spec$group]])
    if (group_count < 1L) add("group_count", "fail", "grouped strategy requires at least one group.") else add("group_count", "pass", paste("groups:", group_count), "info")
  }
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

aq_panel_strategy_origins <- function(spec, data, origins = NULL) {
  if (!is.null(origins)) return(as.Date(origins))
  dt <- data.table::as.data.table(data)
  dates <- sort(unique(as.Date(dt[[spec$date]])))
  possible <- dates[seq_len(max(0L, length(dates) - spec$horizon))]
  if (!is.null(spec$forecast_origin)) return(as.Date(spec$forecast_origin))
  utils::tail(possible, spec$rolling_origins)
}

aq_panel_strategy_series_diagnostics <- function(spec, data) {
  dt <- data.table::as.data.table(data.table::copy(data))
  dt[, .aq_date := as.Date(get(spec$date))]
  out <- dt[, {
    y <- as.numeric(get(spec$target))
    missingness <- mean(is.na(y))
    y_non_missing <- y[!is.na(y)]
    volatility <- if (length(y_non_missing) > 1L) stats::sd(y_non_missing, na.rm = TRUE) / max(abs(mean(y_non_missing, na.rm = TRUE)), .Machine$double.eps) else NA_real_
    diffs <- diff(y_non_missing)
    stability <- if (length(diffs) > 1L) stats::sd(diffs, na.rm = TRUE) else NA_real_
    data.table::data.table(
      history_length = length(y_non_missing),
      missingness = missingness,
      volatility = volatility,
      forecast_stability = stability,
      cold_start_risk = length(y_non_missing) < spec$minimum_history
    )
  }, by = c(spec$entity)]
  if (!is.na(spec$group) && spec$group %in% names(dt)) {
    groups <- unique(dt[, c(spec$entity, spec$group), with = FALSE])
    out <- merge(out, groups, by = spec$entity, all.x = TRUE, sort = FALSE)
  }
  out[]
}

aq_panel_strategy_single_spec <- function(spec, suffix, origin = NULL) {
  aq_forecast_spec(
    target = spec$target,
    date = spec$date,
    frequency = spec$frequency,
    horizon = spec$horizon,
    forecast_origin = origin,
    future_known_variables = spec$future_known_variables,
    engine = spec$engine,
    forecast_strategy = spec$forecast_strategy,
    engine_parameters = spec$engine_parameters,
    prediction_intervals = FALSE,
    future_regressor_policy = "require_complete",
    rolling_origins = spec$rolling_origins,
    aggregation_level = suffix,
    forecast_spec_id = aq_vnext_id(paste0(spec$strategy_spec_id, "_", suffix)),
    dataset_id = spec$dataset_id
  )
}

aq_panel_strategy_panel_spec <- function(spec, suffix, origin = NULL) {
  aq_panel_forecast_spec(
    entity = spec$entity,
    target = spec$target,
    date = spec$date,
    frequency = spec$frequency,
    horizon = spec$horizon,
    forecast_origin = origin,
    future_known_variables = spec$future_known_variables,
    static_entity_features = spec$static_entity_features,
    engine = spec$engine,
    forecast_strategy = spec$forecast_strategy,
    engine_parameters = spec$engine_parameters,
    minimum_history = spec$minimum_history,
    prediction_intervals = FALSE,
    aggregation_level = suffix,
    forecast_spec_id = aq_vnext_id(paste0(spec$strategy_spec_id, "_", suffix)),
    dataset_id = spec$dataset_id
  )
}

aq_panel_strategy_run_independent <- function(spec, data, origin) {
  dt <- data.table::as.data.table(data.table::copy(data))
  entities <- sort(unique(as.character(dt[[spec$entity]])))
  rows <- lapply(entities, function(entity_value) {
    entity_data <- dt[as.character(get(spec$entity)) == entity_value]
    fspec <- aq_panel_strategy_single_spec(spec, paste0("independent_", entity_value), origin = origin)
    forecast <- aq_fit_forecast(fspec, entity_data, origin = origin)
    out <- data.table::copy(forecast$data)
    out[, (spec$entity) := entity_value]
    out[, strategy := "independent"]
    out[, forecast_origin := as.Date(origin)]
    out
  })
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

aq_panel_strategy_run_global <- function(spec, data, origin) {
  pspec <- aq_panel_strategy_panel_spec(spec, "global", origin = origin)
  forecast <- aq_fit_panel_forecast(pspec, data, origin = origin)
  out <- data.table::copy(forecast$data)
  out[, strategy := "global"]
  out[, forecast_origin := as.Date(origin)]
  out
}

aq_panel_strategy_run_grouped <- function(spec, data, origin) {
  if (is.na(spec$group) || !nzchar(spec$group)) {
    return(data.table::data.table())
  }
  dt <- data.table::as.data.table(data.table::copy(data))
  groups <- sort(unique(as.character(dt[[spec$group]])))
  rows <- lapply(groups, function(group_value) {
    group_data <- dt[as.character(get(spec$group)) == group_value]
    if (data.table::uniqueN(group_data[[spec$entity]]) < 1L) return(data.table::data.table())
    pspec <- aq_panel_strategy_panel_spec(spec, paste0("grouped_", group_value), origin = origin)
    forecast <- aq_fit_panel_forecast(pspec, group_data, origin = origin)
    out <- data.table::copy(forecast$data)
    out[, strategy := "grouped"]
    out[, forecast_origin := as.Date(origin)]
    out[, (spec$group) := group_value]
    out
  })
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

aq_panel_strategy_metrics_by_entity <- function(forecasts, spec) {
  if (!nrow(forecasts)) return(data.table::data.table())
  forecasts[horizon %in% spec$evaluation_horizons, aq_forecast_metrics(.SD), by = c("strategy", "forecast_origin", spec$entity)]
}

aq_panel_strategy_summary <- function(forecasts, entity_metrics, spec, diagnostics) {
  if (!nrow(forecasts)) return(data.table::data.table())
  aggregate <- forecasts[horizon %in% spec$evaluation_horizons, aq_forecast_metrics(.SD), by = strategy]
  aggregate <- data.table::dcast(aggregate, strategy ~ metric, value.var = "value")
  entity_formula <- stats::as.formula(paste("strategy + forecast_origin +", spec$entity, "~ metric"))
  entity_wide <- data.table::dcast(entity_metrics, entity_formula, value.var = "value")
  entity_score <- entity_wide[, .(
    entity_count = .N,
    equal_entity_rmse = mean(rmse, na.rm = TRUE),
    equal_entity_mae = mean(mae, na.rm = TRUE),
    bias_mean = mean(bias, na.rm = TRUE),
    entity_rmse_sd = stats::sd(rmse, na.rm = TRUE)
  ), by = strategy]
  out <- merge(aggregate, entity_score, by = "strategy", all = TRUE, sort = FALSE)
  if (identical(spec$entity_weighting, "row_count")) {
    out[, primary_score := rmse]
  } else {
    out[, primary_score := equal_entity_rmse]
  }
  out[, cold_start_entity_count := sum(diagnostics$cold_start_risk, na.rm = TRUE)]
  out[]
}

aq_panel_negative_transfer <- function(entity_metrics, spec) {
  if (!nrow(entity_metrics)) return(data.table::data.table())
  tolerance <- spec$negative_transfer_tolerance
  entity_formula <- stats::as.formula(paste("forecast_origin +", spec$entity, "~ strategy"))
  wide <- data.table::dcast(entity_metrics[metric == "rmse"], entity_formula, value.var = "value")
  rows <- list()
  if (all(c("independent", "global") %in% names(wide))) {
    tmp <- wide[is.finite(independent) & is.finite(global)]
    tmp[, relative_degradation := (global - independent) / pmax(independent, .Machine$double.eps)]
    tmp[, diagnostic := "global_vs_independent"]
    tmp[, status := ifelse(relative_degradation > tolerance, "negative_transfer", "ok")]
    rows[["global"]] <- tmp[, .SD, .SDcols = c("forecast_origin", spec$entity, "diagnostic", "status", "independent", "global", "relative_degradation")]
  }
  if (all(c("independent", "grouped") %in% names(wide))) {
    tmp <- wide[is.finite(independent) & is.finite(grouped)]
    tmp[, relative_degradation := (grouped - independent) / pmax(independent, .Machine$double.eps)]
    tmp[, diagnostic := "grouped_vs_independent"]
    tmp[, status := ifelse(relative_degradation > tolerance, "negative_transfer", "ok")]
    rows[["grouped"]] <- tmp[, .SD, .SDcols = c("forecast_origin", spec$entity, "diagnostic", "status", "independent", "grouped", "relative_degradation")]
  }
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

aq_panel_strategy_recommendation <- function(summary, negative_transfer, spec) {
  if (!nrow(summary) || all(!is.finite(summary$primary_score))) {
    recommendation <- "evidence_insufficient"
    reason <- "No finite comparable strategy scores were produced."
  } else {
    best <- summary[which.min(primary_score)]
    degraded_entities <- if (nrow(negative_transfer)) unique(negative_transfer[status == "negative_transfer", get(spec$entity)]) else character()
    if (length(degraded_entities) && best$strategy %in% c("global", "grouped")) {
      recommendation <- "hybrid_worth_investigating"
      reason <- paste("Best aggregate strategy is", best$strategy, "but", length(degraded_entities), "entity/entities show negative transfer.")
    } else {
      recommendation <- paste0(best$strategy, "_preferred")
      reason <- paste("Lowest primary score:", best$strategy, "=", signif(best$primary_score, 4))
    }
  }
  data.table::data.table(
    recommendation = recommendation,
    reason = reason,
    confidence = if (identical(recommendation, "evidence_insufficient")) "low" else "deterministic_comparison",
    automatic_selection = FALSE,
    next_action = if (identical(recommendation, "hybrid_worth_investigating")) "Investigate degraded entities before pooling." else "Review strategy comparison evidence before choosing a production strategy."
  )
}

#' Evaluate Panel Forecasting Strategies
#'
#' @param spec An `aq_panel_strategy_spec`.
#' @param data Panel time-series data.
#' @param origins Optional common forecast origins.
#'
#' @return An `aq_panel_strategy_evaluation_result`.
#' @export
aq_evaluate_panel_strategies <- function(spec, data, origins = NULL) {
  if (!inherits(spec, "aq_panel_strategy_spec")) stop("spec must be an aq_panel_strategy_spec.", call. = FALSE)
  validation <- aq_validate_panel_strategy_spec(spec, data)
  if (aq_vnext_has_validation_error(validation)) stop(paste(validation[status %in% c("fail", "error"), message], collapse = " "), call. = FALSE)
  origins <- aq_panel_strategy_origins(spec, data, origins = origins)
  diagnostics <- aq_panel_strategy_series_diagnostics(spec, data)
  elapsed <- system.time({
    forecast_rows <- lapply(origins, function(origin) {
      candidates <- list()
      if ("independent" %in% spec$candidate_strategies) candidates$independent <- aq_panel_strategy_run_independent(spec, data, origin)
      if ("grouped" %in% spec$candidate_strategies) candidates$grouped <- aq_panel_strategy_run_grouped(spec, data, origin)
      if ("global" %in% spec$candidate_strategies) candidates$global <- aq_panel_strategy_run_global(spec, data, origin)
      data.table::rbindlist(candidates, use.names = TRUE, fill = TRUE)
    })
  })
  forecasts <- data.table::rbindlist(forecast_rows, use.names = TRUE, fill = TRUE)
  entity_metrics <- aq_panel_strategy_metrics_by_entity(forecasts, spec)
  strategy_summary <- aq_panel_strategy_summary(forecasts, entity_metrics, spec, diagnostics)
  negative_transfer <- aq_panel_negative_transfer(entity_metrics, spec)
  recommendation <- aq_panel_strategy_recommendation(strategy_summary, negative_transfer, spec)
  comparison_id <- aq_vnext_id("panel_strategy_comparison")
  artifact <- new_table_artifact(
    id = comparison_id,
    title = "Panel Strategy Comparison",
    data = strategy_summary,
    source_generator = "aq_evaluate_panel_strategies",
    tags = c("vnext", "forecast", "panel", "strategy", "negative_transfer"),
    dependencies = spec$strategy_spec_id,
    version = "aq_panel_strategy_comparison_artifact_v1",
    metadata = list(
      artifact_type = "panel_strategy_comparison",
      strategy_spec_id = spec$strategy_spec_id,
      candidate_strategies = spec$candidate_strategies,
      origins = origins,
      horizon = spec$horizon,
      comparison_metrics = spec$comparison_metrics,
      entity_weighting = spec$entity_weighting,
      strategy_recommendation = recommendation,
      negative_transfer = negative_transfer,
      series_diagnostics = diagnostics,
      training_efficiency = list(duration_seconds = unname(elapsed[["elapsed"]]), origin_count = length(origins)),
      supported_downstream_actions = c("compare", "report", "campaign_review")
    )
  )
  artifact <- aq_vnext_attach_envelope(
    artifact,
    artifact_id = comparison_id,
    artifact_type = "panel_strategy_comparison",
    artifact_version = "aq_panel_strategy_comparison_artifact_v1",
    parent_artifact_ids = spec$strategy_spec_id,
    lineage = list(strategy_spec_id = spec$strategy_spec_id, origins = origins, strategies = spec$candidate_strategies),
    task = "panel_strategy_evaluation",
    operator = "forecast_strategy_comparison",
    engine = spec$engine,
    specification_id = spec$strategy_spec_id,
    dataset_id = spec$dataset_id,
    supported_actions = c("compare", "report", "campaign_review"),
    producer = "aq_evaluate_panel_strategies"
  )
  result <- list(
    evaluation_id = comparison_id,
    schema_version = "aq_panel_strategy_evaluation_result_v1",
    status = "success",
    strategy_spec_id = spec$strategy_spec_id,
    spec = spec,
    origins = origins,
    forecasts = forecasts,
    entity_metrics = entity_metrics,
    strategy_summary = strategy_summary,
    negative_transfer = negative_transfer,
    series_diagnostics = diagnostics,
    recommendation = recommendation,
    artifact = artifact,
    validation = validation,
    comparison_ready = TRUE,
    created_at = aq_vnext_now()
  )
  class(result) <- c("aq_panel_strategy_evaluation_result", "list")
  result
}

aq_vnext_panel_strategy_fixture <- function() {
  dt <- aq_vnext_panel_forecast_fixture(entity_count = 4L, n = 75L)
  dt[, region := data.table::fifelse(entity %in% c("store_1", "store_2"), "region_a", "region_b")]
  dt[entity == "store_4", demand := demand + rep(c(35, -25, 5, -15, 20), length.out = .N)]
  dt
}

#' QA for vNext Panel Strategy Selection
#'
#' @return A QA `data.table`.
#' @export
qa_vnext_panel_strategy_selection <- function() {
  rows <- list()
  add <- function(check, ok, message = "") {
    rows[[length(rows) + 1L]] <<- data.table::data.table(
      suite = "vnext_panel_strategy_selection",
      check = check,
      status = if (isTRUE(ok)) "pass" else "fail",
      message = message
    )
  }
  dt <- aq_vnext_panel_strategy_fixture()
  spec <- aq_panel_strategy_spec(
    entity = "entity",
    target = "demand",
    date = "date",
    group = "region",
    candidate_strategies = c("independent", "grouped", "global"),
    frequency = "day",
    horizon = 2L,
    future_known_variables = "promo",
    static_entity_features = "store_size",
    forecast_strategy = "direct",
    engine_parameters = list(iterations = 4L, depth = 2L, learning_rate = 0.1, lag_periods = c(1L, 7L), rolling_windows = 3L, date_features = c("year", "month", "day", "dow", "day_index")),
    minimum_history = 20L,
    rolling_origins = 1L,
    dataset_id = "qa_panel_strategy_fixture"
  )
  validation <- aq_validate_panel_strategy_spec(spec, dt)
  add("panel_strategy_spec_constructed", inherits(spec, "aq_panel_strategy_spec"))
  add("panel_strategy_validation_passes", !aq_vnext_has_validation_error(validation), paste(validation$message, collapse = " | "))
  diagnostics <- aq_panel_strategy_series_diagnostics(spec, dt)
  add("series_diagnostics", nrow(diagnostics) == data.table::uniqueN(dt$entity) && all(c("history_length", "volatility", "cold_start_risk") %in% names(diagnostics)))
  bad_group <- spec
  bad_group$group <- NA_character_
  bad_validation <- aq_validate_panel_strategy_spec(bad_group, dt)
  add("grouped_requires_explicit_group", any(bad_validation$check == "grouped_strategy_group" & bad_validation$status == "fail"))
  if (requireNamespace("catboost", quietly = TRUE)) {
    evaluation <- aq_evaluate_panel_strategies(spec, dt)
    add("independent_strategy", "independent" %in% evaluation$forecasts$strategy)
    add("grouped_strategy", "grouped" %in% evaluation$forecasts$strategy)
    add("global_strategy", "global" %in% evaluation$forecasts$strategy)
    add("strategy_comparison", nrow(evaluation$strategy_summary) >= 3L && "primary_score" %in% names(evaluation$strategy_summary))
    add("negative_transfer_diagnostics", all(c("diagnostic", "status", "relative_degradation") %in% names(evaluation$negative_transfer)))
    add("strategy_recommendation", evaluation$recommendation$recommendation %in% aq_panel_strategy_recommendation_levels())
    add("comparison_artifact_integrity", !aq_vnext_has_validation_error(aq_validate_artifact(evaluation$artifact)))
    add("analytics_shinyapp_compatibility", is.list(evaluation$artifact) && "campaign_review" %in% aq_supported_actions(evaluation$artifact))
    serialized <- unserialize(serialize(evaluation$artifact, NULL))
    add("strategy_bundle_compatibility", !aq_vnext_has_validation_error(aq_validate_artifact(serialized)))
  } else {
    add("catboost_available", FALSE, "catboost package is required for panel strategy QA.")
  }
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}
