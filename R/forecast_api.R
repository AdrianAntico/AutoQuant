aq_forecast_api_call <- function(fun, arguments, extra = list()) {
  supplied <- c(arguments, extra)
  supplied <- supplied[!vapply(supplied, is.null, logical(1L))]
  accepted <- names(formals(fun))
  if (!"..." %in% accepted) supplied <- supplied[names(supplied) %in% accepted]
  do.call(fun, supplied)
}

#' Fit a Forecast at Full Method Strength
#'
#' `forecast_fit()` is the preferred expert entry point for AutoQuant
#' forecasting. It selects a forecasting family without hiding its important
#' controls. Additional family-specific parameters pass through `...` to the
#' native specification constructor.
#'
#' @param data A data frame or `data.table` containing training observations.
#' @param target Outcome column for ordinary, panel, hurdle, and intermittent
#'   forecasts. For funnel forecasts this is the value column.
#' @param date Time-index column.
#' @param method Forecast family: ordinary, panel, multitarget, hurdle, funnel,
#'   Croston, SBA, or TSB.
#' @param targets Outcome columns for a multi-target forecast.
#' @param entity Optional entity column for panel, hurdle, or intermittent data.
#' @param stages Ordered funnel stage values.
#' @param frequency Forecast frequency or `"auto"`.
#' @param horizon Positive forecast horizon.
#' @param origin Optional forecast origin.
#' @param future_data Optional future covariate data.
#' @param future_known_variables Variables known over the forecast horizon.
#' @param engine Native forecasting engine.
#' @param strategy Forecast strategy. Consequential strategy choices remain
#'   explicit and are not elected silently.
#' @param engine_parameters Named native engine controls. CatBoost parameters
#'   are validated without reducing the native control surface.
#' @param prediction_intervals Whether supported families should return
#'   prediction intervals.
#' @param confidence_level Requested interval confidence level.
#' @param rolling_origins Number of default rolling validation origins.
#' @param ... Family-specific expert parameters passed to the selected native
#'   specification constructor.
#' @return A native fitted forecast result retaining its method-specific class,
#'   specification, fitted state, forecasts, diagnostics, and provenance.
#' @examples
#' d <- data.table::data.table(
#'   date = as.Date("2024-01-01") + 0:59,
#'   value = 10 + 0.1 * (0:59) + sin((0:59) / 4)
#' )
#' fit <- forecast_fit(d, target = "value", date = "date",
#'   method = "ordinary", engine = "naive", horizon = 5)
#' fit$data
#' @export
forecast_fit <- function(
    data, target = NULL, date, method = "ordinary", targets = NULL,
    entity = NULL, stages = NULL, frequency = "auto", horizon = 1L,
    origin = NULL, future_data = NULL, future_known_variables = character(),
    engine = NULL, strategy = NULL, engine_parameters = list(),
    prediction_intervals = TRUE, confidence_level = 0.95,
    rolling_origins = 3L, ...) {
  method <- match.arg(tolower(method), c(
    "ordinary", "panel", "multitarget", "hurdle", "funnel",
    "croston", "sba", "tsb"))
  extra <- list(...)
  common <- list(target = target, date = date, frequency = frequency,
    horizon = horizon, forecast_origin = origin,
    future_known_variables = future_known_variables,
    engine = engine, forecast_strategy = strategy,
    engine_parameters = engine_parameters,
    prediction_intervals = prediction_intervals,
    confidence_level = confidence_level, rolling_origins = rolling_origins)
  if (identical(method, "ordinary")) {
    common$engine <- engine %||% "naive"
    common$forecast_strategy <- strategy %||% "direct"
    spec <- aq_forecast_api_call(aq_forecast_spec, common, extra)
    return(aq_fit_forecast(spec, data, origin = origin, future_data = future_data))
  }
  if (identical(method, "panel")) {
    common$entity <- entity
    common$engine <- engine %||% "catboost"
    common$forecast_strategy <- strategy %||% "direct"
    spec <- aq_forecast_api_call(aq_panel_forecast_spec, common, extra)
    return(aq_fit_panel_forecast(spec, data, origin = origin,
      future_data = future_data))
  }
  if (identical(method, "multitarget")) {
    args <- common
    args$target <- NULL
    args$targets <- targets %||% target
    args$known_future_variables <- future_known_variables
    args$strategy <- strategy %||% "independent"
    args$engine <- engine %||% "naive"
    spec <- aq_forecast_api_call(aq_multitarget_forecast_spec, args, extra)
    return(aq_fit_multitarget_forecast(spec, data, origin = origin,
      future_data = future_data))
  }
  if (identical(method, "hurdle")) {
    common$entity <- entity
    common$engine <- engine %||% "catboost"
    common$forecast_strategy <- strategy %||% "direct"
    spec <- aq_forecast_api_call(aq_hurdle_forecast_spec, common, extra)
    return(aq_fit_hurdle_forecast(spec, data, origin = origin,
      future_data = future_data))
  }
  if (identical(method, "funnel")) {
    args <- list(stages = stages, value = target, date = date,
      frequency = frequency, horizon = horizon, forecast_origin = origin,
      strategy = strategy %||% "stage",
      known_future_variables = future_known_variables,
      engine = engine %||% "catboost",
      engine_parameters = engine_parameters)
    spec <- aq_forecast_api_call(aq_funnel_forecast_spec, args, extra)
    return(aq_fit_funnel_forecast(spec, data, origin = origin))
  }
  constructor <- switch(method, croston = aq_croston_forecast_spec,
    sba = aq_sba_forecast_spec, tsb = aq_tsb_forecast_spec)
  fitter <- switch(method, croston = aq_fit_croston_forecast,
    sba = aq_fit_sba_forecast, tsb = aq_fit_tsb_forecast)
  spec <- aq_forecast_api_call(constructor, list(target = target, date = date,
    entity = entity, frequency = frequency, horizon = horizon,
    forecast_origin = origin), extra)
  fitter(spec, data, origin = origin)
}

#' Backtest a Forecast Without Changing Its Method
#' @param spec A native AutoQuant forecast specification.
#' @param data Historical observations.
#' @param origins Optional explicit forecast origins.
#' @param origin_count Optional number of trailing origins.
#' @return The native rolling-origin result for the supplied method.
#' @export
forecast_backtest <- function(spec, data, origins = NULL, origin_count = NULL) {
  if (inherits(spec, "aq_forecast_spec"))
    return(aq_rolling_origin_forecast(spec, data, origins, origin_count))
  if (inherits(spec, "aq_panel_forecast_spec"))
    return(aq_rolling_origin_panel_forecast(spec, data, origins, origin_count))
  if (inherits(spec, "aq_hurdle_forecast_spec"))
    return(aq_rolling_origin_hurdle_forecast(spec, data, origins, origin_count))
  if (inherits(spec, "aq_multitarget_forecast_spec")) {
    if (!exists("aq_rolling_origin_multitarget_forecast", mode = "function", inherits = TRUE)) {
      stop("No public rolling-origin implementation exists for multi-target specifications.",
        call. = FALSE)
    }
    return(aq_rolling_origin_multitarget_forecast(spec, data, origins, origin_count))
  }
  if (inherits(spec, "aq_funnel_forecast_spec")) {
    if (!exists("aq_rolling_origin_funnel_forecast", mode = "function", inherits = TRUE)) {
      stop("No public rolling-origin implementation exists for funnel specifications.",
        call. = FALSE)
    }
    return(aq_rolling_origin_funnel_forecast(spec, data, origins, origin_count))
  }
  if (inherits(spec, "aq_croston_forecast_spec"))
    return(aq_rolling_origin_croston_forecast(spec, data, origins, origin_count))
  if (inherits(spec, "aq_sba_forecast_spec"))
    return(aq_rolling_origin_sba_forecast(spec, data, origins, origin_count))
  if (inherits(spec, "aq_tsb_forecast_spec"))
    return(aq_rolling_origin_tsb_forecast(spec, data, origins, origin_count))
  stop("No public rolling-origin implementation exists for this specification.",
    call. = FALSE)
}

#' Diagnose a Fitted Forecast
#' @param object A fitted AutoQuant forecast result.
#' @return The native method-specific assessment object.
#' @export
forecast_diagnose <- function(object) {
  if (inherits(object, "aq_panel_forecast_result")) return(aq_assess_panel_forecast(object))
  if (inherits(object, "aq_multitarget_forecast_result")) return(aq_assess_multitarget_forecast(object))
  if (inherits(object, "aq_hurdle_forecast_result")) return(aq_assess_hurdle_forecast(object))
  if (inherits(object, "aq_funnel_forecast_result")) return(aq_assess_funnel_forecast(object))
  if (inherits(object, "aq_croston_forecast_result")) return(aq_assess_croston_forecast(object))
  if (inherits(object, "aq_sba_forecast_result")) return(aq_assess_sba_forecast(object))
  if (inherits(object, "aq_tsb_forecast_result")) return(aq_assess_tsb_forecast(object))
  if (inherits(object, "aq_forecast_result")) return(aq_assess_forecast(object))
  stop("No public forecast diagnostic implementation exists for this object.",
    call. = FALSE)
}

#' Reconcile Forecasts Across a Hierarchy
#' @param base Base forecasts containing node, horizon, and forecast columns.
#' @param summing_matrix Hierarchical summing matrix.
#' @param node_order Node order corresponding to the matrix.
#' @param method Reconciliation method: OLS, WLS, or MinT.
#' @param covariance Optional forecast-error covariance matrix.
#' @param variances Optional forecast-error variances.
#' @return A coherent hierarchical forecast result.
#' @export
forecast_reconcile <- function(base, summing_matrix, node_order,
    method = c("mint", "wls", "ols"), covariance = NULL, variances = NULL) {
  aq_reconcile_forecast_advanced(base = base, summing_matrix = summing_matrix,
    node_order = node_order, method = match.arg(method),
    covariance = covariance, variances = variances)
}

#' Qualify the compact expert forecasting API
#' @return A compact table of dispatch and capability checks.
#' @export
qa_forecast_public_api <- function() {
  add <- function(check, passed, detail = "") data.table::data.table(
    check = check, passed = isTRUE(passed), detail = as.character(detail)[1L])
  d <- data.table::data.table(
    date = as.Date("2024-01-01") + 0:79,
    value = 12 + 0.08 * (0:79) + sin((0:79) / 5))
  ordinary <- forecast_fit(d, "value", "date", method = "ordinary",
    engine = "naive", horizon = 4L)
  intermittent <- data.table::copy(d)
  intermittent[, value := ifelse(seq_len(.N) %% 5L == 0L,
    seq_len(.N) / 10, 0)]
  croston <- forecast_fit(intermittent, "value", "date",
    method = "croston", horizon = 4L, alpha = 0.2)
  diagnosed <- forecast_diagnose(ordinary)
  bad <- tryCatch(forecast_backtest(list(), d), error = function(e) e)
  out <- data.table::rbindlist(list(
    add("ordinary_native_result", inherits(ordinary, "aq_forecast_result")),
    add("intermittent_native_result", inherits(croston,
      "aq_croston_forecast_result")),
    add("diagnostic_dispatch", inherits(diagnosed,
      "aq_forecast_assessment_result")),
    add("unsupported_lifecycle_fails", inherits(bad, "error")),
    add("expert_controls_explicit", all(c("engine", "strategy",
      "engine_parameters", "prediction_intervals", "confidence_level") %in%
      names(formals(forecast_fit)))),
    add("advanced_reconciliation_reachable", identical(
      eval(formals(forecast_reconcile)$method)[1L], "mint")),
    add("native_gbdt_engines", all(c("catboost", "lightgbm", "xgboost") %in%
      aq_forecast_engine_levels())),
    add("funnel_forwards_engine_parameters",
      all(c("engine", "engine_parameters") %in% names(formals(aq_funnel_forecast_spec)))),
    add("catboost_has_time_default", {
      spec <- aq_forecast_spec("value", "date", engine = "catboost",
        engine_parameters = list(boosting_type = "Ordered",
          monotone_constraints = c(1, 0, -1), langevin = TRUE))
      val <- aq_validate_forecast_spec(spec)
      !any(val$status %in% c("fail", "error")) &&
        isTRUE(aq_forecast_catboost_model_params(spec)$has_time)
    }),
    add("forecast_engine_control_exported",
      is.function(forecast_engine_control) &&
        identical(eval(formals(forecast_engine_control)$engine)[1L], "catboost"))
  ))
  attr(out, "passed") <- all(out$passed)
  out
}

#' Native GBDT forecast engine controls
#'
#' Flat, documented production defaults for CatBoost, LightGBM, and XGBoost.
#' Unknown names fail closed. Temporal CatBoost defaults preserve row order.
#'
#' @param engine `"catboost"`, `"lightgbm"`, or `"xgboost"`.
#' @param ... Native engine parameters from the corresponding allowlist.
#' @return A named list for `engine_parameters` in [forecast_fit()].
#' @export
forecast_engine_control <- function(engine = c("catboost", "lightgbm", "xgboost"),
    ...) {
  engine <- match.arg(engine)
  supplied <- list(...)
  if (identical(engine, "lightgbm")) return(aq_vnext_lightgbm_params(supplied))
  if (identical(engine, "xgboost")) return(aq_vnext_xgboost_params(supplied))
  aq_vnext_engine_params(supplied, task = "regression")
}
