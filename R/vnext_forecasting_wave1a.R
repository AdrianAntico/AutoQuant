# Forecasting Reference Upgrade Wave 1A engine adapters.
# The public contract remains aq_forecast_spec()/aq_fit_forecast(); this file
# owns only strong package-backed implementations and their native evidence.

aq_forecast_require_forecast <- function(engine) {
  if (!requireNamespace("forecast", quietly = TRUE)) {
    stop("The forecast package is required for the ", engine,
      " Wave 1A forecasting engine.", call. = FALSE)
  }
  invisible(TRUE)
}

aq_forecast_native_intervals <- function(object, horizon, confidence_level,
    method) {
  level <- 100 * confidence_level
  predicted <- forecast::forecast(object, h = horizon, level = level)
  lower <- if (is.null(predicted$lower)) rep(NA_real_, horizon) else
    as.numeric(predicted$lower[, 1L])
  upper <- if (is.null(predicted$upper)) rep(NA_real_, horizon) else
    as.numeric(predicted$upper[, 1L])
  list(
    prediction = as.numeric(predicted$mean),
    intervals = data.table::data.table(
      lower_interval = lower, upper_interval = upper,
      confidence_level = confidence_level,
      interval_available = is.finite(lower) & is.finite(upper),
      interval_method = method, unsupported_interval_reason = NA_character_),
    forecast = predicted)
}

aq_forecast_fit_ets <- function(train, spec, frequency, xreg = NULL) {
  aq_forecast_require_forecast("ETS")
  params <- aq_vnext_default(spec$engine_parameters, list())
  y <- as.numeric(train[[spec$target]])
  y <- y[!is.na(y)]
  ts_y <- stats::ts(y, frequency = aq_forecast_ts_frequency(frequency,
    spec$season_length))
  model_form <- as.character(aq_vnext_default(params$model, "ZZZ"))[1L]
  damped <- aq_vnext_default(params$damped, NULL)
  fit_call <- aq_forecast_with_warnings(forecast::ets(ts_y, model = model_form,
    damped = damped, opt.crit = "lik"))
  model <- fit_call$value
  pred <- if (isTRUE(spec$prediction_intervals))
    aq_forecast_native_intervals(model, spec$horizon, spec$confidence_level,
      "forecast::ets state-space predictive intervals") else list(
        prediction = as.numeric(forecast::forecast(model, h = spec$horizon)$mean),
        intervals = aq_forecast_no_interval_columns(spec$horizon,
          spec$confidence_level, "prediction intervals were not requested"))
  components <- strsplit(model$method, "", fixed = TRUE)[[1L]]
  list(prediction = pred$prediction, intervals = pred$intervals, model = model,
    diagnostics = list(engine = "ets", method = model$method,
      model_form = model$method,
      error = components[1L], trend = components[2L], seasonality = components[3L],
      damped = isTRUE(model$damped), smoothing_parameters = as.list(model$par),
      initial_state = as.numeric(model$states[1L, ]),
      aic = unname(model$aic), aicc = unname(model$aicc), bic = unname(model$bic),
      residual_diagnostics = aq_forecast_residual_diagnostics(stats::residuals(model)),
      convergence = "completed", warnings = fit_call$warnings,
      interval_available = any(pred$intervals$interval_available),
      interval_method = unique(pred$intervals$interval_method),
      training_duration_seconds = NA_real_))
}

aq_forecast_fit_arima <- function(train, spec, frequency, xreg = NULL) {
  aq_forecast_require_forecast("ARIMA")
  params <- aq_vnext_default(spec$engine_parameters, list())
  y <- as.numeric(train[[spec$target]])
  y <- y[!is.na(y)]
  ts_y <- stats::ts(y, frequency = aq_forecast_ts_frequency(frequency,
    spec$season_length))
  automatic <- isTRUE(aq_vnext_default(params$automatic, is.null(params$order)))
  fit_call <- aq_forecast_with_warnings(if (automatic) forecast::auto.arima(
    ts_y, xreg = xreg$train, seasonal = TRUE, stepwise =
      isTRUE(aq_vnext_default(params$stepwise, TRUE)), approximation =
      isTRUE(aq_vnext_default(params$approximation, FALSE)), ic = "aicc") else
    forecast::Arima(ts_y, order = as.integer(params$order), seasonal = list(
      order = as.integer(aq_vnext_default(params$seasonal_order, c(0L, 0L, 0L))),
      period = aq_forecast_season_length(frequency, spec$season_length)),
      xreg = xreg$train, include.mean = isTRUE(aq_vnext_default(params$include_mean, TRUE)),
      method = "ML"))
  model <- fit_call$value
  predicted <- forecast::forecast(model, h = spec$horizon,
    xreg = xreg$future, level = 100 * spec$confidence_level)
  intervals <- if (isTRUE(spec$prediction_intervals)) data.table::data.table(
    lower_interval = as.numeric(predicted$lower[, 1L]),
    upper_interval = as.numeric(predicted$upper[, 1L]),
    confidence_level = spec$confidence_level, interval_available = TRUE,
    interval_method = "forecast::Arima predictive intervals",
    unsupported_interval_reason = NA_character_) else
      aq_forecast_no_interval_columns(spec$horizon, spec$confidence_level,
        "prediction intervals were not requested")
  arma <- model$arma
  list(prediction = as.numeric(predicted$mean), intervals = intervals, model = model,
    diagnostics = list(engine = "arima", selection = if (automatic) "auto_aicc" else "explicit",
      order = c(arma[1L], arma[6L], arma[2L]),
      seasonal_order = c(arma[3L], arma[7L], arma[4L]), seasonal_period = arma[5L],
      coefficients = stats::coef(model), coefficient_variance = model$var.coef,
      innovation_variance = model$sigma2, aic = model$aic, aicc = model$aicc,
      bic = model$bic, residual_diagnostics =
        aq_forecast_residual_diagnostics(stats::residuals(model)),
      roots = list(ar = if (arma[1L] > 0L) polyroot(c(1, -stats::coef(model)[seq_len(arma[1L])])) else complex(),
        ma = if (arma[2L] > 0L) polyroot(c(1, stats::coef(model)[arma[1L] + seq_len(arma[2L])])) else complex()),
      xreg_used = !is.null(xreg$train), convergence = "completed",
      warnings = fit_call$warnings, interval_available = any(intervals$interval_available),
      interval_method = unique(intervals$interval_method),
      training_duration_seconds = NA_real_))
}

aq_forecast_fit_tbats <- function(train, spec, frequency, xreg = NULL) {
  aq_forecast_require_forecast("TBATS")
  params <- aq_vnext_default(spec$engine_parameters, list())
  y <- as.numeric(train[[spec$target]])
  y <- y[!is.na(y)]
  periods <- as.numeric(aq_vnext_default(params$seasonal_periods,
    aq_forecast_season_length(frequency, spec$season_length)))
  ts_y <- forecast::msts(y, seasonal.periods = periods)
  fit_call <- aq_forecast_with_warnings(forecast::tbats(ts_y,
    use.box.cox = aq_vnext_default(params$use_box_cox, NULL),
    use.trend = aq_vnext_default(params$use_trend, NULL),
    use.damped.trend = aq_vnext_default(params$use_damped_trend, NULL),
    use.arma.errors = isTRUE(aq_vnext_default(params$use_arma_errors, TRUE)),
    biasadj = isTRUE(aq_vnext_default(params$biasadj, FALSE))))
  model <- fit_call$value
  pred <- aq_forecast_native_intervals(model, spec$horizon,
    spec$confidence_level, "forecast::tbats simulated predictive intervals")
  components <- model$components
  list(prediction = pred$prediction,
    intervals = if (isTRUE(spec$prediction_intervals)) pred$intervals else
      aq_forecast_no_interval_columns(spec$horizon, spec$confidence_level,
        "prediction intervals were not requested"), model = model,
    diagnostics = list(engine = "tbats", seasonal_periods = periods,
      box_cox_lambda = model$lambda, trend = isTRUE(components[1L]),
      damped_trend = isTRUE(components[2L]), arma_orders = components[3:4],
      fourier_harmonics = components[-seq_len(min(4L, length(components)))],
      initial_state = model$x0, fitted_values = as.numeric(model$fitted.values),
      residual_diagnostics = aq_forecast_residual_diagnostics(stats::residuals(model)),
      convergence = "completed", warnings = fit_call$warnings,
      interval_available = any(pred$intervals$interval_available),
      interval_method = unique(pred$intervals$interval_method),
      training_duration_seconds = NA_real_))
}
