# AutoQuant vNext forecasting capability discovery and strategy planning.

aq_forecasting_planner_operator_levels <- function() {
  c(
    "single_series_statistical",
    "catboost_supervised",
    "panel",
    "hierarchy",
    "intermittent_demand",
    "funnel",
    "multi_target",
    "cross_target_features"
  )
}

aq_forecasting_carma_mechanism_inventory <- function() {
  data.table::data.table(
    mechanism = c(
      "differencing",
      "trend_terms",
      "anomaly_treatment",
      "observation_weighting",
      "target_transformation",
      "temporal_lag_roll_features",
      "calendar_holiday_features",
      "fourier_seasonality",
      "cross_target_features",
      "recursive_forecast_state_update",
      "bounded_model_search",
      "forecast_diagnostics"
    ),
    historical_name = c(
      "Difference; CarmaDifferencing; antidiff",
      "TimeTrendVariable; TimeTrend",
      "AnomalyDetection; tstat_high; tstat_low",
      "TimeWeights; Weights",
      "TargetTransformation; Methods; AutoTransformationCreate",
      "Lags; MA_Periods; SD_Periods; Skew_Periods; Kurt_Periods; Quantile_Periods",
      "CalendarVariables; HolidayVariable; HolidayLags; HolidayMovingAverages",
      "FourierTerms; CarmaFourier",
      "AutoCatBoostVectorCARMA; shared target histories",
      "CarmaScore; UpdateFeatures; CarmaRollingStatsUpdate",
      "GridTune; PassInGrid; ModelCount; MaxRunsWithoutNewWinner; MaxRunMinutes",
      "EvalMetric; feature importances; horizon/group error summaries"
    ),
    source = c(
      "R/AutoCatBoostCARMA.R; R/AutoCatBoostVectorCARMA.R; R/AutoCatBoostHurdleCARMA.R",
      "R/AutoCatBoostCARMA.R; R/AutoCatBoostVectorCARMA.R; R/AutoCatBoostHurdleCARMA.R",
      "R/AutoCatBoostCARMA.R; R/AutoCatBoostVectorCARMA.R; R/AutoCatBoostHurdleCARMA.R",
      "R/AutoCatBoostCARMA.R; R/AutoCatBoostVectorCARMA.R; R/AutoCatBoostHurdleCARMA.R",
      "R/AutoCatBoostCARMA.R; R/AutoCatBoostVectorCARMA.R; R/AutoCatBoostHurdleCARMA.R; Rodeo numeric transforms",
      "R/AutoCatBoostCARMA.R; R/AutoCatBoostVectorCARMA.R; R/CARMA-HelperFunctions.R; Rodeo cross-row helpers",
      "R/AutoCatBoostCARMA.R; R/AutoCatBoostFunnel.R; Rodeo calendar helpers",
      "R/AutoCatBoostCARMA.R; R/AutoCatBoostVectorCARMA.R; R/CARMA-HelperFunctions.R",
      "R/AutoCatBoostVectorCARMA.R; docs/vnext_cross_target_forecasting.md",
      "R/AutoCatBoostCARMA.R; R/AutoCatBoostHurdleCARMA.R; R/CARMA-HelperFunctions.R",
      "R/AutoCatBoostCARMA.R; R/AutoCatBoostVectorCARMA.R; R/AutoCatBoostFunnel.R",
      "R/AutoCatBoostCARMA.R; R/AutoCatBoostVectorCARMA.R; README.md historical CARMA examples"
    ),
    analytical_purpose = c(
      "Model change rather than level when level series are nonstationary, then reconstruct original-scale forecasts.",
      "Expose elapsed-time structure that tree models may not extrapolate from raw dates alone.",
      "Separate bad observations or exceptional shocks from stable temporal signal.",
      "Bias fitting toward recent or business-important observations.",
      "Stabilize variance, respect positivity, and improve loss behavior while preserving original-scale interpretation.",
      "Expose autoregressive memory, local averages, volatility, shape, and tail behavior.",
      "Represent calendar, holiday, and event timing effects available before forecast time.",
      "Represent seasonal periodicity compactly, including group-specific seasonality.",
      "Test whether prior history of one target improves another target.",
      "Keep future lag and rolling features coherent as forecasts are generated step by step.",
      "Constrain model and feature search to bounded experiments instead of open-ended AutoML.",
      "Return evidence by horizon, group, target, feature, and transformation path."
    ),
    leakage_risk = c(
      "High if future levels are used for reintegration or if differencing crosses forecast origin.",
      "Low, but trend extrapolation can overstate stability after regime breaks.",
      "High if anomaly thresholds are fit using future rows or if exceptional events are silently removed.",
      "Medium if weights use future outcome information or optimize business value outside validation.",
      "Medium if transforms are fit on scoring/future rows; inversion may bias original-scale metrics.",
      "High unless lag and rolling windows use strictly prior rows within sorted groups.",
      "Medium if future calendar variables are not truly known at forecast time.",
      "Low when based only on dates/groups, but group-specific fitting needs replay metadata.",
      "High for contemporaneous or future target values; safe only with prior target histories.",
      "High if recursive updates accidentally use realized future targets.",
      "Medium if search observes holdout repeatedly without recording the selection process.",
      "Low, but diagnostics can be misleading if computed on transformed scale only."
    ),
    modern_owner = c(
      "Rodeo + AutoQuant",
      "Rodeo",
      "Rodeo + assessment layer",
      "AutoQuant + assessment layer",
      "Rodeo + AutoQuant",
      "Rodeo",
      "Rodeo",
      "Rodeo",
      "Rodeo + AutoQuant",
      "Rodeo + AutoQuant",
      "AutoQuant campaign/experiment layer",
      "AutoQuant assessment layer"
    ),
    vnext_status = c(
      "partially represented",
      "partially represented",
      "missing",
      "missing",
      "partially represented",
      "partially represented",
      "partially represented",
      "missing",
      "partially represented",
      "partially represented",
      "partially represented",
      "partially represented"
    ),
    recommended_future_experiment = c(
      "Compare level modeling, differenced modeling, and transformed+differenced modeling with reintegration error.",
      "Compare no trend, global trend, entity trend, and trend-break diagnostics.",
      "Compare flagged-only, down-weighted, clipped, and untouched anomaly policies.",
      "Compare unweighted, recency-weighted, entity-weighted, and business-value-weighted fits.",
      "Compare original-scale, log/log1p, Box-Cox/Yeo-Johnson, and bias-corrected inversions.",
      "Compare bounded lag/rolling windows by horizon and entity density.",
      "Compare calendar-only, holiday-count, holiday-lag, and holiday-rolling features.",
      "Compare Fourier seasonal features against calendar and seasonal naive baselines.",
      "Compare independent, shared workflow, and cross-target feature strategies with negative-transfer diagnostics.",
      "Compare direct and recursive strategies where recursive state can be replayed safely.",
      "Convert legacy grid concepts into evidence-guided model-tuning hypotheses with frozen-baseline challengers.",
      "Expand forecast artifacts with horizon stability, residual, feature usefulness, and reconstruction diagnostics."
    ),
    tuning_layer = c(
      "feature_tuning",
      "feature_tuning",
      "feature_tuning",
      "feature_tuning",
      "feature_tuning",
      "feature_tuning",
      "feature_tuning",
      "feature_tuning",
      "feature_tuning",
      "strategy_tuning",
      "model_tuning",
      "assessment"
    ),
    future_capability = c(
      "Forecast Feature Tuning",
      "Forecast Feature Tuning",
      "Forecast Feature Tuning",
      "Forecast Feature Tuning",
      "Forecast Feature Tuning",
      "Forecast Feature Tuning",
      "Forecast Feature Tuning",
      "Forecast Feature Tuning",
      "Forecast Feature Tuning",
      "Forecast Strategy Tuning",
      "Evidence-Guided Model Tuning",
      "Forecast Assessment Evidence"
    )
  )
}

aq_forecasting_planner_date_candidates <- function(dt, date = NULL) {
  if (!is.null(date) && length(date) && date %in% names(dt)) return(as.character(date)[1L])
  date_like <- names(dt)[vapply(dt, inherits, logical(1L), what = c("Date", "POSIXct", "POSIXt"))]
  if (length(date_like)) return(date_like[1L])
  name_like <- grep("date|ds|time|timestamp", names(dt), ignore.case = TRUE, value = TRUE)
  parseable <- name_like[vapply(name_like, function(col) {
    vals <- suppressWarnings(as.Date(dt[[col]]))
    mean(!is.na(vals)) >= 0.8
  }, logical(1L))]
  if (length(parseable)) parseable[1L] else NA_character_
}

aq_forecasting_planner_target_candidates <- function(dt, target = NULL, exclude = character()) {
  if (!is.null(target) && length(target)) {
    return(intersect(aq_vnext_unique_chr(target), names(dt)))
  }
  numeric_cols <- names(dt)[vapply(dt, is.numeric, logical(1L))]
  setdiff(numeric_cols, exclude)
}

aq_forecasting_planner_entity_candidate <- function(dt, entity = NULL, exclude = character()) {
  if (!is.null(entity) && length(entity) && entity %in% names(dt)) return(as.character(entity)[1L])
  candidates <- intersect(c("entity", "id", "group", "store", "region", "market", "segment", "customer_segment"), names(dt))
  candidates <- setdiff(candidates, exclude)
  candidates <- candidates[vapply(candidates, function(col) data.table::uniqueN(dt[[col]]) > 1L, logical(1L))]
  if (length(candidates)) candidates[1L] else NA_character_
}

aq_forecasting_planner_schema <- function(dt) {
  data.table::data.table(
    column = names(dt),
    class = vapply(dt, function(x) paste(class(x), collapse = "/"), character(1L)),
    missing = vapply(dt, function(x) sum(is.na(x)), integer(1L)),
    missing_rate = vapply(dt, function(x) mean(is.na(x)), numeric(1L)),
    unique_values = vapply(dt, data.table::uniqueN, integer(1L)),
    numeric = vapply(dt, is.numeric, logical(1L))
  )
}

aq_forecasting_planner_target_summary <- function(dt, targets) {
  if (!length(targets)) return(data.table::data.table())
  data.table::rbindlist(lapply(targets, function(target) {
    y <- as.numeric(dt[[target]])
    data.table::data.table(
      target = target,
      observations = sum(!is.na(y)),
      missing = sum(is.na(y)),
      missing_rate = mean(is.na(y)),
      zero_count = sum(y == 0, na.rm = TRUE),
      zero_proportion = mean(y == 0, na.rm = TRUE),
      positive_count = sum(y > 0, na.rm = TRUE),
      mean = mean(y, na.rm = TRUE),
      sd = stats::sd(y, na.rm = TRUE)
    )
  }), use.names = TRUE, fill = TRUE)
}

aq_forecasting_planner_add_operator <- function(rows, operator, status, reason, required_evidence, missing_evidence = character(), priority = "recommended") {
  rows[[length(rows) + 1L]] <- data.table::data.table(
    operator = operator,
    status = status,
    reason = reason,
    required_evidence = paste(aq_vnext_unique_chr(required_evidence), collapse = "; "),
    missing_evidence = paste(aq_vnext_unique_chr(missing_evidence), collapse = "; "),
    priority = priority
  )
  rows
}

#' Discover vNext Forecasting Capabilities
#'
#' Inspects a forecasting problem and records which forecasting families are
#' supported, unsupported, or uncertain. Discovery is deterministic and does not
#' fit models.
#'
#' @param data Input data.
#' @param target Optional target column or columns.
#' @param date Optional date column.
#' @param entity Optional entity/group column for panel forecasting.
#' @param hierarchy Optional hierarchy column names or hierarchy metadata.
#' @param stages Optional ordered funnel stage names.
#' @param stage Optional funnel stage column.
#' @param value Optional funnel value column.
#' @param known_future_variables Optional variables known at forecast time.
#' @param horizon Forecast horizon used for planning diagnostics.
#' @param forecast_origin Optional forecast origin.
#' @param dataset_id Optional dataset id.
#'
#' @return An `aq_forecasting_capability_discovery` object.
#' @export
aq_discover_forecasting_capabilities <- function(
  data,
  target = NULL,
  date = NULL,
  entity = NULL,
  hierarchy = NULL,
  stages = NULL,
  stage = NULL,
  value = NULL,
  known_future_variables = character(),
  horizon = 1L,
  forecast_origin = NULL,
  dataset_id = NULL
) {
  dt <- data.table::as.data.table(data.table::copy(data))
  horizon <- as.integer(horizon)[1L]
  if (!is.finite(horizon) || horizon < 1L) horizon <- 1L
  known_future_variables <- aq_vnext_unique_chr(known_future_variables)
  date_col <- aq_forecasting_planner_date_candidates(dt, date)
  stage_col <- if (!is.null(stage) && length(stage) && stage %in% names(dt)) as.character(stage)[1L] else NA_character_
  value_col <- if (!is.null(value) && length(value) && value %in% names(dt)) as.character(value)[1L] else NA_character_
  entity_col <- aq_forecasting_planner_entity_candidate(dt, entity, exclude = c(date_col, stage_col, value_col))
  targets <- aq_forecasting_planner_target_candidates(dt, target, exclude = c(date_col, entity_col, stage_col, value_col, known_future_variables))
  schema <- aq_forecasting_planner_schema(dt)
  target_summary <- aq_forecasting_planner_target_summary(dt, targets)
  dates <- if (!is.na(date_col)) suppressWarnings(as.Date(dt[[date_col]])) else as.Date(character())
  frequency <- if (length(dates) && any(!is.na(dates))) aq_forecast_detect_frequency(dates) else list(frequency = NA_character_, interval = NA_real_, confidence = "missing_date")
  origin <- if (!is.null(forecast_origin)) as.Date(forecast_origin)[1L] else if (length(dates) && any(!is.na(dates))) max(dates, na.rm = TRUE) else as.Date(NA)
  history_rows <- if (length(dates) && !is.na(origin)) sum(dates <= origin, na.rm = TRUE) else nrow(dt)
  entity_count <- if (!is.na(entity_col)) data.table::uniqueN(dt[[entity_col]]) else 0L
  hierarchy_cols <- intersect(aq_vnext_unique_chr(unlist(hierarchy, use.names = FALSE)), names(dt))
  stage_count <- if (!is.na(stage_col)) data.table::uniqueN(dt[[stage_col]]) else length(aq_vnext_unique_chr(stages))
  has_date <- !is.na(date_col)
  has_target <- length(targets) >= 1L
  has_multiple_targets <- length(targets) >= 2L
  has_entity <- !is.na(entity_col) && entity_count > 1L
  has_hierarchy <- length(hierarchy_cols) > 0L
  has_funnel <- !is.na(stage_col) && !is.na(value_col) && stage_count >= 2L
  missing_future_vars <- setdiff(known_future_variables, names(dt))
  max_zero_prop <- if (nrow(target_summary)) max(target_summary$zero_proportion, na.rm = TRUE) else NA_real_
  min_positive <- if (nrow(target_summary)) min(target_summary$positive_count, na.rm = TRUE) else NA_integer_
  rows <- list()
  common_required <- c("date column", "numeric target", "forecast horizon")
  if (has_date && has_target && history_rows >= horizon + 4L) {
    rows <- aq_forecasting_planner_add_operator(rows, "single_series_statistical", "supported", "Date, numeric target, and enough history are available.", c(common_required, "history"))
    rows <- aq_forecasting_planner_add_operator(rows, "catboost_supervised", "supported", "Supervised forecasting can be considered with temporal features and known future variables where available.", c(common_required, "Rodeo temporal preparation", "CatBoost"))
  } else {
    rows <- aq_forecasting_planner_add_operator(rows, "single_series_statistical", "unsupported", "Single-series forecasting requires date, numeric target, and sufficient history.", common_required, c(if (!has_date) "date column", if (!has_target) "numeric target", if (history_rows < horizon + 4L) "sufficient history"), "blocked")
    rows <- aq_forecasting_planner_add_operator(rows, "catboost_supervised", "uncertain", "Supervised forecasting requires temporal feature evidence and sufficient training rows.", c(common_required, "Rodeo temporal preparation", "CatBoost"), c(if (!has_date) "date column", if (!has_target) "numeric target", if (history_rows < 12L) "training history"), "optional")
  }
  if (has_entity && has_date && has_target) {
    rows <- aq_forecasting_planner_add_operator(rows, "panel", "supported", "Entity/date structure is present.", c("entity column", "date column", "numeric target", "entity history diagnostics"))
  } else {
    rows <- aq_forecasting_planner_add_operator(rows, "panel", "unsupported", "Panel forecasting requires explicit entity/date/target structure.", c("entity column", "date column", "numeric target"), c(if (!has_entity) "entity column", if (!has_date) "date column", if (!has_target) "numeric target"), "blocked")
  }
  if (has_entity && has_hierarchy && has_date && has_target) {
    rows <- aq_forecasting_planner_add_operator(rows, "hierarchy", "supported", "Hierarchy columns are available above panel entities.", c("entity column", "hierarchy columns", "panel forecasts", "reconciliation rule"))
  } else {
    rows <- aq_forecasting_planner_add_operator(rows, "hierarchy", if (has_entity) "uncertain" else "unsupported", "Hierarchical forecasting needs known aggregation relationships.", c("entity column", "hierarchy columns", "panel forecast evidence"), c(if (!has_entity) "entity column", if (!has_hierarchy) "hierarchy metadata"), "optional")
  }
  if (has_target && is.finite(max_zero_prop) && max_zero_prop >= 0.3 && min_positive >= 5L) {
    rows <- aq_forecasting_planner_add_operator(rows, "intermittent_demand", "supported", "Targets show zero inflation with enough positive observations.", c("zero proportion", "positive-demand count", "Croston/SBA/TSB baselines"))
  } else if (has_target && is.finite(max_zero_prop) && max_zero_prop > 0) {
    rows <- aq_forecasting_planner_add_operator(rows, "intermittent_demand", "uncertain", "Some zeros exist but intermittent-demand suitability is not decisive.", c("zero proportion", "positive-demand count"), c(if (is.finite(min_positive) && min_positive < 5L) "sufficient positive demand"), "optional")
  } else {
    rows <- aq_forecasting_planner_add_operator(rows, "intermittent_demand", "unsupported", "No meaningful zero inflation was detected.", c("zero-inflated target"), "zero inflation", "blocked")
  }
  if (has_funnel) {
    rows <- aq_forecasting_planner_add_operator(rows, "funnel", "supported", "Stage and value columns with at least two stages are available.", c("stage column", "value column", "ordered stages", "cohort/date evidence"))
  } else {
    rows <- aq_forecasting_planner_add_operator(rows, "funnel", "unsupported", "Funnel forecasting requires stage and value structure.", c("stage column", "value column", "ordered stages"), c(if (is.na(stage_col)) "stage column", if (is.na(value_col)) "value column", if (stage_count < 2L) "at least two stages"), "blocked")
  }
  if (has_multiple_targets && has_date) {
    rows <- aq_forecasting_planner_add_operator(rows, "multi_target", "supported", "Multiple numeric targets share a timeline.", c("multiple targets", "shared date", "target-level assessment"))
    rows <- aq_forecasting_planner_add_operator(rows, "cross_target_features", if (history_rows >= 12L) "supported" else "uncertain", "Cross-target features can test whether prior target history helps other targets.", c("multiple targets", "Rodeo cross-target preparation", "CatBoost", "negative-transfer assessment"), c(if (history_rows < 12L) "more history"), "recommended")
  } else {
    rows <- aq_forecasting_planner_add_operator(rows, "multi_target", "unsupported", "Multi-target forecasting requires at least two numeric targets on a shared timeline.", c("multiple numeric targets", "shared date"), c(if (!has_multiple_targets) "multiple targets", if (!has_date) "date column"), "blocked")
    rows <- aq_forecasting_planner_add_operator(rows, "cross_target_features", "unsupported", "Cross-target learning requires multi-target structure.", c("multiple numeric targets", "shared date", "Rodeo cross-target preparation"), c(if (!has_multiple_targets) "multiple targets", if (!has_date) "date column"), "blocked")
  }
  operators <- data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
  diagnostics <- data.table::data.table(
    check = c("date", "targets", "entity", "hierarchy", "funnel", "known_future_variables", "history"),
    status = c(
      if (has_date) "pass" else "warning",
      if (has_target) "pass" else "fail",
      if (has_entity) "pass" else "info",
      if (has_hierarchy) "pass" else "info",
      if (has_funnel) "pass" else "info",
      if (!length(missing_future_vars)) "pass" else "warning",
      if (history_rows >= horizon + 4L) "pass" else "warning"
    ),
    message = c(
      if (has_date) paste("date column:", date_col) else "No date column was detected.",
      if (has_target) paste("target columns:", paste(targets, collapse = ", ")) else "No numeric target columns were detected.",
      if (has_entity) paste("entity column:", entity_col, "entities:", entity_count) else "No panel entity column was detected.",
      if (has_hierarchy) paste("hierarchy columns:", paste(hierarchy_cols, collapse = ", ")) else "No hierarchy metadata was supplied.",
      if (has_funnel) paste("funnel stages:", stage_count) else "No funnel stage/value structure was detected.",
      if (!length(missing_future_vars)) "Known future variables are present or not declared." else paste("Missing known future variables:", paste(missing_future_vars, collapse = ", ")),
      paste("history rows at or before origin:", history_rows)
    )
  )
  result <- list(
    discovery_id = aq_vnext_id("forecasting_capability_discovery"),
    schema_version = "aq_forecasting_capability_discovery_v1",
    dataset_id = aq_vnext_default(dataset_id, NA_character_),
    date = date_col,
    targets = targets,
    entity = entity_col,
    hierarchy = hierarchy_cols,
    stage = stage_col,
    value = value_col,
    stages = aq_vnext_unique_chr(stages),
    known_future_variables = known_future_variables,
    horizon = horizon,
    forecast_origin = origin,
    frequency = frequency,
    schema = schema,
    target_summary = target_summary,
    operators = operators,
    diagnostics = diagnostics,
    mechanism_inventory = aq_forecasting_carma_mechanism_inventory(),
    supported_operators = operators[status == "supported", operator],
    unsupported_operators = operators[status == "unsupported", operator],
    uncertain_operators = operators[status == "uncertain", operator],
    created_at = aq_vnext_now()
  )
  class(result) <- c("aq_forecasting_capability_discovery", "list")
  result
}

aq_forecasting_planner_baselines <- function(discovery) {
  ops <- discovery$supported_operators
  rows <- list(data.table::data.table(operator = "single_series_statistical", baseline = c("naive", "seasonal_naive"), mandatory = TRUE, reason = "Foundational deterministic forecast baselines."))
  if ("single_series_statistical" %in% ops) {
    rows[[length(rows) + 1L]] <- data.table::data.table(operator = "single_series_statistical", baseline = c("ets", "arima"), mandatory = FALSE, reason = "Statistical challengers where history supports them.")
  }
  if ("intermittent_demand" %in% ops) {
    rows[[length(rows) + 1L]] <- data.table::data.table(operator = "intermittent_demand", baseline = c("naive", "seasonal_naive", "croston", "sba", "tsb"), mandatory = TRUE, reason = "Intermittent-demand methods require demand-specific baselines.")
  }
  if ("panel" %in% ops) {
    rows[[length(rows) + 1L]] <- data.table::data.table(operator = "panel", baseline = c("independent_panel", "global_panel"), mandatory = TRUE, reason = "Panel strategy evidence should compare independent and pooled behavior.")
  }
  if ("hierarchy" %in% ops) {
    rows[[length(rows) + 1L]] <- data.table::data.table(operator = "hierarchy", baseline = "bottom_up_reconciliation", mandatory = TRUE, reason = "Hierarchy evidence should start with deterministic bottom-up reconciliation.")
  }
  if ("funnel" %in% ops) {
    rows[[length(rows) + 1L]] <- data.table::data.table(operator = "funnel", baseline = c("stage_forecast", "transition_forecast"), mandatory = TRUE, reason = "Funnel evidence should compare stage and transition views.")
  }
  if ("multi_target" %in% ops) {
    rows[[length(rows) + 1L]] <- data.table::data.table(operator = "multi_target", baseline = c("independent", "shared_workflow"), mandatory = TRUE, reason = "Multi-target evidence must preserve independent target baselines.")
  }
  if ("cross_target_features" %in% ops) {
    rows[[length(rows) + 1L]] <- data.table::data.table(operator = "cross_target_features", baseline = c("independent", "shared_workflow", "cross_target_features"), mandatory = TRUE, reason = "Cross-target learning must be compared against non-cross-target strategies.")
  }
  unique(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
}

aq_forecasting_planner_experiments <- function(discovery, max_experiments = 5L) {
  supported <- discovery$operators[status == "supported"]
  priority <- c("cross_target_features", "multi_target", "intermittent_demand", "funnel", "hierarchy", "panel", "catboost_supervised", "single_series_statistical")
  supported <- supported[order(match(operator, priority), operator)]
  max_experiments <- as.integer(max_experiments)[1L]
  if (!is.finite(max_experiments) || max_experiments < 1L) max_experiments <- 1L
  selected <- supported[seq_len(min(nrow(supported), max_experiments))]
  experiments <- selected[, .(
    experiment_id = paste0("forecast_experiment_", seq_len(.N)),
    operator,
    purpose = data.table::fcase(
      operator == "intermittent_demand", "Test whether zero-inflated demand methods reduce forecast error.",
      operator == "funnel", "Compare stage-volume and transition-rate views of the funnel.",
      operator == "cross_target_features", "Test whether prior target history improves other target forecasts.",
      operator == "multi_target", "Preserve target-level evidence under a shared forecasting workflow.",
      operator == "hierarchy", "Test deterministic reconciliation above panel forecasts.",
      operator == "panel", "Compare independent and global entity forecasting.",
      operator == "catboost_supervised", "Test supervised temporal features against deterministic baselines.",
      default = "Establish deterministic statistical baseline evidence."
    ),
    expected_comparison = data.table::fcase(
      operator == "intermittent_demand", "naive vs seasonal_naive vs Croston/SBA/TSB",
      operator == "funnel", "stage vs transition",
      operator == "cross_target_features", "independent vs shared_workflow vs cross_target_features",
      operator == "multi_target", "independent vs shared_workflow",
      operator == "hierarchy", "unreconciled panel vs reconciled hierarchy",
      operator == "panel", "independent vs global",
      operator == "catboost_supervised", "naive/seasonal naive/statistical vs CatBoost",
      default = "naive vs seasonal naive"
    )
  )]
  experiments[]
}

#' Plan vNext Forecasting Strategy
#'
#' Converts capability discovery into a deterministic analytical plan. The
#' planner recommends what to test; it does not execute models.
#'
#' @param discovery Optional `aq_forecasting_capability_discovery`.
#' @param data Optional data used when discovery is not supplied.
#' @param max_experiments Maximum number of recommended experiments.
#' @param ... Arguments passed to `aq_discover_forecasting_capabilities()` when
#'   `discovery` is not supplied.
#'
#' @return An `aq_forecasting_strategy_plan`.
#' @export
aq_plan_forecasting_strategy <- function(discovery = NULL, data = NULL, max_experiments = 5L, ...) {
  if (is.null(discovery)) {
    if (is.null(data)) stop("Either discovery or data must be supplied.", call. = FALSE)
    discovery <- aq_discover_forecasting_capabilities(data = data, ...)
  }
  if (!inherits(discovery, "aq_forecasting_capability_discovery")) {
    stop("discovery must be returned by aq_discover_forecasting_capabilities().", call. = FALSE)
  }
  baselines <- aq_forecasting_planner_baselines(discovery)
  experiments <- aq_forecasting_planner_experiments(discovery, max_experiments = max_experiments)
  recommendations <- discovery$operators[status %in% c("supported", "uncertain"), .(
    operator,
    recommendation = ifelse(status == "supported", "consider", "collect_more_evidence"),
    why = reason,
    required_evidence,
    missing_evidence,
    limitations = data.table::fcase(
      operator == "cross_target_features", "Descriptive target relationships only; no causal interpretation.",
      operator == "hierarchy", "Requires trustworthy aggregation relationships.",
      operator == "funnel", "Requires meaningful ordered stages and maturity/cohort interpretation.",
      operator == "intermittent_demand", "Requires enough positive demand to assess occurrence and magnitude behavior.",
      default = "Plan is advisory and does not execute models."
    ),
    recommended_validation = data.table::fcase(
      operator == "cross_target_features", "Compare independent, shared_workflow, and cross_target_features with negative-transfer diagnostics.",
      operator == "panel", "Use rolling-origin panel strategy comparison.",
      operator == "hierarchy", "Assess reconciled forecasts against unreconciled panel forecasts.",
      operator == "funnel", "Compare stage and transition strategies.",
      operator == "intermittent_demand", "Compare naive, seasonal naive, Croston, SBA, TSB, and optional Hurdle.",
      default = "Use rolling-origin validation where practical."
    ),
    supported_downstream_actions = "forecast; assess; compare; report; campaign_review"
  )]
  missing_evidence <- discovery$operators[nzchar(missing_evidence), .(operator, missing_evidence, status)]
  plan_id <- aq_vnext_id("forecasting_strategy_plan")
  artifact <- new_table_artifact(
    id = plan_id,
    title = "Forecasting Strategy Plan",
    data = recommendations,
    source_generator = "aq_plan_forecasting_strategy",
    tags = c("vnext", "forecast", "planning", "capability_discovery"),
    dependencies = discovery$discovery_id,
    version = "aq_forecasting_strategy_plan_artifact_v1",
    metadata = list(
      artifact_type = "forecast_planning_artifact",
      plan_id = plan_id,
      discovery_id = discovery$discovery_id,
      candidate_operators = discovery$operators,
      required_baselines = baselines,
      experiment_set = experiments,
      missing_evidence = missing_evidence,
      diagnostics = discovery$diagnostics,
      carma_mechanism_inventory = discovery$mechanism_inventory,
      supported_actions = c("forecast", "assess", "compare", "report", "campaign_review")
    )
  )
  artifact <- aq_vnext_attach_envelope(
    artifact,
    artifact_id = plan_id,
    artifact_type = "forecast_planning_artifact",
    artifact_version = "aq_forecasting_strategy_plan_artifact_v1",
    parent_artifact_ids = discovery$discovery_id,
    lineage = list(
      discovery_id = discovery$discovery_id,
      dataset_id = discovery$dataset_id,
      date = discovery$date,
      targets = discovery$targets,
      horizon = discovery$horizon,
      forecast_origin = discovery$forecast_origin
    ),
    task = "forecast_strategy_planning",
    operator = "forecast_planning",
    engine = "deterministic_planner",
    specification_id = discovery$discovery_id,
    dataset_id = discovery$dataset_id,
    supported_actions = c("forecast", "assess", "compare", "report", "campaign_review"),
    producer = "aq_plan_forecasting_strategy"
  )
  result <- list(
    plan_id = plan_id,
    schema_version = "aq_forecasting_strategy_plan_v1",
    discovery = discovery,
    recommendations = recommendations,
    required_baselines = baselines,
    experiment_set = experiments,
    missing_evidence = missing_evidence,
    limitations = unique(recommendations$limitations),
    artifact = artifact,
    comparison_ready = TRUE,
    created_at = aq_vnext_now()
  )
  class(result) <- c("aq_forecasting_strategy_plan", "list")
  result
}

aq_vnext_forecasting_planner_fixture <- function() {
  dt <- aq_vnext_multitarget_forecast_fixture(52L)
  dt[, region := rep(c("north", "south"), length.out = .N)]
  dt[seq(1L, .N, by = 5L), enrollments := 0]
  dt
}

#' QA for vNext Forecasting Planning
#'
#' @return A data.table of deterministic QA checks.
#' @export
qa_vnext_forecasting_planning <- function() {
  rows <- list()
  add <- function(check, ok, message = "") {
    rows[[length(rows) + 1L]] <<- data.table::data.table(
      suite = "vnext_forecasting_planning",
      check = check,
      status = if (isTRUE(ok)) "pass" else "fail",
      message = message
    )
  }
  dt <- aq_vnext_forecasting_planner_fixture()
  discovery <- aq_discover_forecasting_capabilities(
    dt,
    target = c("leads", "applications", "enrollments"),
    date = "date",
    hierarchy = "region",
    known_future_variables = "promo",
    horizon = 3L,
    forecast_origin = max(dt$date) - 21L,
    dataset_id = "qa_forecasting_planner"
  )
  add("discovery_constructed", inherits(discovery, "aq_forecasting_capability_discovery"))
  add("operators_classified", all(c("multi_target", "cross_target_features", "single_series_statistical") %in% discovery$supported_operators))
  add("missing_evidence_detected", "hierarchy" %in% discovery$uncertain_operators || "hierarchy" %in% discovery$supported_operators)
  add("target_summary", nrow(discovery$target_summary) == 3L && "zero_proportion" %in% names(discovery$target_summary))
  add("diagnostics_present", all(c("date", "targets", "known_future_variables") %in% discovery$diagnostics$check))
  add("carma_mechanism_inventory", is.data.frame(discovery$mechanism_inventory) && all(c("differencing", "anomaly_treatment", "observation_weighting", "cross_target_features") %in% discovery$mechanism_inventory$mechanism))
  plan <- aq_plan_forecasting_strategy(discovery)
  add("plan_constructed", inherits(plan, "aq_forecasting_strategy_plan"))
  add("planning_artifact", inherits(plan$artifact, "aq_artifact") && identical(plan$artifact$metadata$artifact_type, "forecast_planning_artifact"))
  add("artifact_integrity", !aq_vnext_has_validation_error(aq_validate_artifact(plan$artifact)) && identical(aq_artifact_envelope(plan$artifact)$operator, "forecast_planning"))
  add("baselines_recommended", all(c("naive", "seasonal_naive") %in% plan$required_baselines$baseline))
  add("cross_target_experiment", any(plan$experiment_set$operator == "cross_target_features") && any(plan$required_baselines$operator == "cross_target_features"))
  add("planning_artifact_mechanisms", is.data.frame(plan$artifact$metadata$carma_mechanism_inventory) && any(plan$artifact$metadata$carma_mechanism_inventory$vnext_status == "missing"))
  sparse <- dt[1:5]
  sparse_discovery <- aq_discover_forecasting_capabilities(sparse, target = "leads", date = "date", horizon = 8L)
  add("insufficient_history_warning", any(sparse_discovery$diagnostics$check == "history" & sparse_discovery$diagnostics$status == "warning"))
  no_funnel <- aq_discover_forecasting_capabilities(dt, target = "leads", date = "date")
  add("unsupported_funnel_recorded", any(no_funnel$operators$operator == "funnel" & no_funnel$operators$status == "unsupported"))
  app_like <- list(plan$artifact)
  add("analytics_shinyapp_compatibility", all(vapply(app_like, function(x) inherits(x, "aq_artifact") && !is.null(x$artifact_envelope) && "campaign_review" %in% aq_supported_actions(x), logical(1L))))
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}
