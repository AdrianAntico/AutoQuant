# Forecasting Reference Upgrade Wave 1B operators. These functions extend the
# canonical forecast contracts; they do not create a second forecasting system.

aq_forecast_fingerprint <- function(...) {
  paste0("aq_", digest::digest(list(...), algo = "sha256", serialize = TRUE))
}

aq_forecast_fit_theta <- function(train, spec, frequency, xreg = NULL) {
  aq_forecast_require_forecast("Theta")
  y <- stats::ts(as.numeric(train[[spec$target]]), frequency =
    aq_forecast_ts_frequency(frequency, spec$season_length))
  fit_call <- aq_forecast_with_warnings(forecast::thetaf(y,
    h = spec$horizon, level = 100 * spec$confidence_level))
  predicted <- fit_call$value
  intervals <- if (isTRUE(spec$prediction_intervals)) data.table::data.table(
    lower_interval = as.numeric(predicted$lower[, 1L]),
    upper_interval = as.numeric(predicted$upper[, 1L]),
    confidence_level = spec$confidence_level, interval_available = TRUE,
    interval_method = "forecast::thetaf predictive intervals",
    unsupported_interval_reason = NA_character_) else
      aq_forecast_no_interval_columns(spec$horizon, spec$confidence_level,
        "prediction intervals were not requested")
  model <- predicted$model
  list(prediction = as.numeric(predicted$mean), intervals = intervals,
    model = model, diagnostics = list(engine = "theta",
      theta_variant = "optimized_dynamic_theta",
      alpha = model$alpha, drift = unname(model$drift), sigma2 = model$sigma2,
      seasonal_component = as.numeric(model$seas_component),
      residual_diagnostics = aq_forecast_residual_diagnostics(model$residuals),
      convergence = "completed", warnings = fit_call$warnings,
      interval_available = any(intervals$interval_available),
      interval_method = unique(intervals$interval_method),
      training_duration_seconds = NA_real_))
}

aq_forecast_fit_arfima <- function(train, spec, frequency, xreg = NULL) {
  aq_forecast_require_forecast("ARFIMA")
  y <- stats::ts(as.numeric(train[[spec$target]]), frequency =
    aq_forecast_ts_frequency(frequency, spec$season_length))
  fit_call <- aq_forecast_with_warnings(forecast::arfima(y))
  model <- fit_call$value
  predicted <- forecast::forecast(model, h = spec$horizon,
    level = 100 * spec$confidence_level)
  intervals <- if (isTRUE(spec$prediction_intervals)) data.table::data.table(
    lower_interval = as.numeric(predicted$lower[, 1L]),
    upper_interval = as.numeric(predicted$upper[, 1L]),
    confidence_level = spec$confidence_level, interval_available = TRUE,
    interval_method = "forecast::arfima predictive intervals",
    unsupported_interval_reason = NA_character_) else
      aq_forecast_no_interval_columns(spec$horizon, spec$confidence_level,
        "prediction intervals were not requested")
  list(prediction = as.numeric(predicted$mean), intervals = intervals,
    model = model, diagnostics = list(engine = "arfima",
      fractional_differencing = unname(model$d), ar = model$ar, ma = model$ma,
      innovation_variance = model$sigma2,
      long_memory_evidence = list(d = unname(model$d),
        interpretation = "fractional integration parameter"),
      residual_diagnostics = aq_forecast_residual_diagnostics(model$residuals),
      convergence = "completed", warnings = fit_call$warnings,
      interval_available = any(intervals$interval_available),
      interval_method = unique(intervals$interval_method),
      training_duration_seconds = NA_real_))
}

#' Decompose Multiple Seasonalities with MSTL
#'
#' @param values Numeric observations.
#' @param seasonal_periods Seasonal periods.
#' @param robust Whether STL fitting is robust.
#' @return A typed decomposition with component and evidence tables.
#' @export
aq_mstl_decompose <- function(values, seasonal_periods, robust = TRUE) {
  aq_forecast_require_forecast("MSTL")
  values <- as.numeric(values)
  periods <- sort(unique(as.integer(seasonal_periods)))
  periods <- periods[is.finite(periods) & periods > 1L]
  if (length(values) < 2L * max(periods, 1L))
    stop("MSTL requires at least two observations per longest seasonal period.",
      call. = FALSE)
  fit <- forecast::mstl(forecast::msts(values, seasonal.periods = periods),
    robust = isTRUE(robust))
  components <- data.table::as.data.table(as.data.frame(fit))
  components[, observation := seq_len(.N)]
  data.table::setcolorder(components, c("observation",
    setdiff(names(components), "observation")))
  out <- list(schema_version = "aq_mstl_decomposition_v1",
    decomposition_id = aq_vnext_id("mstl_decomposition"),
    seasonal_periods = periods, robust = isTRUE(robust),
    components = components,
    evidence = list(trend = "Trend", remainder = "Remainder",
      seasonal_components = grep("Season", names(components), value = TRUE),
      component_identity_preserved = TRUE,
      downstream_forecast_required = TRUE))
  class(out) <- c("aq_mstl_decomposition", "list")
  out
}

#' Score Probabilistic Forecast Evidence
#'
#' @param actual Realized observations.
#' @param quantiles Long table containing `observation`, `probability`, and
#'   `forecast`.
#' @param intervals Optional interval table with lower, upper, and nominal level.
#' @return Typed proper-score and calibration evidence.
#' @export
aq_score_probabilistic_forecast <- function(actual, quantiles,
    intervals = NULL) {
  actual <- as.numeric(actual)
  q <- data.table::as.data.table(data.table::copy(quantiles))
  required <- c("observation", "probability", "forecast")
  if (!all(required %in% names(q)))
    stop("quantiles must contain observation, probability, and forecast.",
      call. = FALSE)
  if (any(q$probability <= 0 | q$probability >= 1 | !is.finite(q$forecast)))
    stop("Quantile probabilities and forecasts must be finite and valid.",
      call. = FALSE)
  if (any(q$observation < 1L | q$observation > length(actual)))
    stop("Quantile observation identities must index supplied actuals.",
      call. = FALSE)
  q[, actual := actual[observation]]
  q[, pinball_loss := pmax(probability * (actual - forecast),
    (probability - 1) * (actual - forecast))]
  pinball <- q[, .(value = mean(pinball_loss), n = .N), by = probability]
  interval_scores <- data.table::data.table()
  calibration <- data.table::data.table()
  if (!is.null(intervals)) {
    z <- data.table::as.data.table(data.table::copy(intervals))
    need <- c("observation", "lower", "upper", "nominal_coverage")
    if (!all(need %in% names(z)))
      stop("intervals must contain observation, lower, upper, and nominal_coverage.",
        call. = FALSE)
    if (any(z$observation < 1L | z$observation > length(actual)) ||
        any(!is.finite(z$lower) | !is.finite(z$upper) |
          z$lower > z$upper | z$nominal_coverage <= 0 |
          z$nominal_coverage >= 1))
      stop("Interval bounds, coverage, and observation identities must be valid.",
        call. = FALSE)
    z[, actual := actual[observation]]
    z[, alpha := 1 - nominal_coverage]
    z[, interval_score := upper - lower +
      2 / alpha * (lower - actual) * (actual < lower) +
      2 / alpha * (actual - upper) * (actual > upper)]
    medians <- q[abs(probability - 0.5) < sqrt(.Machine$double.eps),
      .(observation, median_forecast = forecast)]
    z <- merge(z, medians, by = "observation", all.x = TRUE, sort = FALSE)
    z[, weighted_interval_score := ifelse(is.finite(median_forecast),
      (0.5 * abs(actual - median_forecast) + alpha / 2 * interval_score) /
        1.5, NA_real_)]
    interval_scores <- z[, .(wis = mean(weighted_interval_score,
      na.rm = TRUE), interval_score = mean(interval_score),
      average_width = mean(upper - lower), n = .N), by = nominal_coverage]
    calibration <- z[, .(empirical_coverage = mean(actual >= lower & actual <= upper),
      nominal_coverage = nominal_coverage[1L], average_width = mean(upper-lower)),
      by = nominal_coverage]
  }
  out <- list(schema_version = "aq_probabilistic_forecast_evidence_v1",
    evaluation_id = aq_vnext_id("probabilistic_forecast_evaluation"),
    pinball = pinball, interval_scores = interval_scores,
    calibration = calibration,
    unsupported_scores = c(crps = "requires samples or a full predictive distribution",
      log_score = "requires a proper predictive density"))
  class(out) <- c("aq_probabilistic_forecast_evidence", "list")
  out
}

#' Derive Split-Conformal Forecast Intervals
#'
#' @param point_forecast Point forecasts.
#' @param calibration_residuals Strictly historical calibration residuals.
#' @param coverage Nominal coverage.
#' @param horizon Optional horizon identity for horizon-specific calibration.
#' @return Separate conformal interval evidence; native uncertainty is untouched.
#' @export
aq_conformal_forecast_intervals <- function(point_forecast,
    calibration_residuals, coverage = 0.9, horizon = NULL) {
  point_forecast <- as.numeric(point_forecast)
  residuals <- abs(as.numeric(calibration_residuals))
  residuals <- residuals[is.finite(residuals)]
  if (!length(residuals) || !is.finite(coverage) || coverage <= 0 || coverage >= 1)
    stop("Conformal intervals require historical residuals and valid coverage.",
      call. = FALSE)
  radius <- as.numeric(stats::quantile(residuals, probs = coverage,
    type = 1, names = FALSE))
  out <- data.table::data.table(horizon = aq_vnext_default(horizon,
      seq_along(point_forecast)),
    forecast = point_forecast, lower_conformal = point_forecast - radius,
    upper_conformal = point_forecast + radius, nominal_coverage = coverage,
    calibration_n = length(residuals), calibration_radius = radius,
    uncertainty_origin = "split_conformal_historical_residuals")
  attr(out, "assumptions") <- c("exchangeable calibration residuals",
    "calibration data precedes forecast origin")
  out
}

aq_forecast_pseudoinverse <- function(x, tolerance = sqrt(.Machine$double.eps)) {
  x <- as.matrix(x)
  decomposition <- svd(x)
  if (!length(decomposition$d))
    stop("Cannot invert an empty reconciliation matrix.", call. = FALSE)
  cutoff <- max(decomposition$d) * max(dim(x)) * tolerance
  inverse_values <- ifelse(decomposition$d > cutoff, 1 / decomposition$d, 0)
  decomposition$v %*% (inverse_values * t(decomposition$u))
}

aq_forecast_reconciliation_projection <- function(summing_matrix,
    method = c("ols", "wls", "mint"), covariance = NULL,
    variances = NULL) {
  method <- match.arg(method)
  S <- as.matrix(summing_matrix)
  if (!is.numeric(S) || nrow(S) < ncol(S) || qr(S)$rank < ncol(S))
    stop("summing_matrix must be numeric and full column rank.", call. = FALSE)
  n <- nrow(S)
  W <- if (identical(method, "ols")) diag(n) else if (identical(method, "wls")) {
    if (is.null(variances) || length(variances) != n || any(variances <= 0))
      stop("WLS requires one positive base-error variance per hierarchy node.",
        call. = FALSE)
    diag(as.numeric(variances), n)
  } else {
    if (is.null(covariance) || !all(dim(covariance) == c(n, n)))
      stop("MinT requires a square base-error covariance matrix.", call. = FALSE)
    as.matrix(covariance)
  }
  Winv <- aq_forecast_pseudoinverse(W)
  middle <- crossprod(S, Winv %*% S)
  solve_middle <- aq_forecast_pseudoinverse(middle)
  S %*% solve_middle %*% crossprod(S, Winv)
}

#' Reconcile Hierarchical Forecasts without Mutating Base Forecasts
#'
#' @param base Long base forecast table with node, horizon, and forecast columns.
#' @param summing_matrix Hierarchy summing matrix; rows match `node_order`.
#' @param node_order Hierarchy-node order.
#' @param method OLS, WLS, or MinT.
#' @param covariance Optional MinT covariance.
#' @param variances Optional WLS variances.
#' @return Base and reconciled forecasts with coherence evidence.
#' @export
aq_reconcile_forecast_advanced <- function(base, summing_matrix, node_order,
    method = c("ols", "wls", "mint"), covariance = NULL,
    variances = NULL) {
  method <- match.arg(method)
  base <- data.table::as.data.table(data.table::copy(base))
  if (!all(c("node", "horizon", "forecast") %in% names(base)))
    stop("base must contain node, horizon, and forecast.", call. = FALSE)
  if (!setequal(unique(base$node), node_order))
    stop("Base nodes must match node_order exactly.", call. = FALSE)
  P <- aq_forecast_reconciliation_projection(summing_matrix, method,
    covariance, variances)
  reconciled <- base[, {
    ordered <- .SD[match(node_order, node)]
    adjusted <- as.numeric(P %*% ordered$forecast)
    .(node = node_order, base_forecast = ordered$forecast,
      reconciled_forecast = adjusted,
      adjustment = adjusted - ordered$forecast)
  }, by = horizon]
  S <- as.matrix(summing_matrix)
  coherent <- reconciled[, {
    values <- reconciled_forecast[match(node_order, node)]
    bottom <- values[seq.int(nrow(S)-ncol(S)+1L, nrow(S))]
    .(coherence_error = max(abs(values - as.numeric(S %*% bottom))))
  }, by = horizon]
  out <- list(schema_version = "aq_forecast_reconciliation_v2",
    reconciliation_id = aq_vnext_id(paste0("forecast_reconciliation_", method)),
    method = method, node_order = node_order, summing_matrix = S,
    base_forecast = base, reconciled_forecast = reconciled,
    evidence = list(covariance_assumption = if (method == "ols") "identity" else
      if (method == "wls") "diagonal_base_error_variance" else "supplied_base_error_covariance",
      coherence = coherent, adjustment_magnitude =
        reconciled[, .(mean_absolute_adjustment = mean(abs(adjustment)),
          maximum_absolute_adjustment = max(abs(adjustment))), by = horizon],
      reconciliation_fingerprint = aq_forecast_fingerprint(method, S,
        reconciled)))
  class(out) <- c("aq_forecast_reconciliation", "list")
  out
}

#' Combine Governed Forecast Components
#'
#' @param components Long table with component_id, horizon, and forecast.
#' @param method Mean, median, inverse-error, validation-weighted, or robust.
#' @param validation_errors Optional component/horizon error evidence.
#' @param component_revisions Named component revision identities.
#' @return Combined forecast with full component provenance.
#' @export
aq_combine_forecasts <- function(components,
    method = c("mean", "median", "inverse_error", "validation_weighted", "robust"),
    validation_errors = NULL, component_revisions = NULL) {
  method <- match.arg(method)
  x <- data.table::as.data.table(data.table::copy(components))
  if (!all(c("component_id", "horizon", "forecast") %in% names(x)) ||
      any(!is.finite(x$forecast)))
    stop("components must contain finite component_id/horizon/forecast evidence.",
      call. = FALSE)
  if (anyDuplicated(x[, .(component_id, horizon)]))
    stop("Each component may contribute once per horizon.", call. = FALSE)
  errors <- data.table::as.data.table(data.table::copy(aq_vnext_default(
    validation_errors, data.table::data.table())))
  if (method %in% c("inverse_error", "validation_weighted") &&
      !all(c("component_id", "error") %in% names(errors)))
    stop("Weighted combinations require validation error evidence.",
      call. = FALSE)
  weights <- x[, .(component_id, horizon)]
  if (method %in% c("mean", "median", "robust")) {
    weights[, weight := 1 / .N, by = horizon]
  } else {
    weights <- merge(weights, errors, by = intersect(c("component_id", "horizon"),
      names(errors)), all.x = TRUE)
    if (any(!is.finite(weights$error) | weights$error < 0))
      stop("Validation errors must be finite and non-negative.", call. = FALSE)
    weights[, raw_weight := 1 / pmax(error, sqrt(.Machine$double.eps))]
    weights[, weight := raw_weight / sum(raw_weight), by = horizon]
  }
  joined <- merge(x, weights[, .(component_id, horizon, weight)],
    by = c("component_id", "horizon"))
  combined <- if (method == "median") joined[, .(forecast = stats::median(forecast)),
    by = horizon] else if (method == "robust") joined[, .(forecast = mean(forecast,
      trim = 0.1)), by = horizon] else joined[, .(forecast = sum(weight * forecast)),
        by = horizon]
  out <- list(schema_version = "aq_forecast_combination_v1",
    combination_id = aq_vnext_id(paste0("forecast_combination_", method)),
    method = method, forecast = combined, components = x, weights = weights,
    component_revisions = aq_vnext_default(component_revisions, list()),
    evaluation_evidence = errors,
    combination_fingerprint = aq_forecast_fingerprint(method,
      sort(unique(x$component_id)),
      weights[order(horizon, component_id),
        .(component_id, horizon, weight)], combined))
  class(out) <- c("aq_forecast_combination", "list")
  out
}

#' Declare Governed Panel Parallelism
#'
#' @param series_count Number of independent series.
#' @param requested_workers Requested outer workers.
#' @param method_threads Threads consumed inside each method fit.
#' @param available_cores Optional detected core count.
#' @return A bounded resource policy preventing nested oversubscription.
#' @export
aq_forecast_parallel_policy <- function(series_count, requested_workers = 1L,
    method_threads = 1L, available_cores = parallel::detectCores()) {
  cores <- max(1L, as.integer(available_cores)[1L])
  inner <- max(1L, as.integer(method_threads)[1L])
  outer_max <- max(1L, cores %/% inner)
  outer <- min(max(1L, as.integer(requested_workers)[1L]),
    max(1L, as.integer(series_count)[1L]), outer_max)
  list(outer_series_workers = outer, inner_method_threads = inner,
    available_cores = cores, maximum_concurrent_threads = outer * inner,
    nested_oversubscription_prevented = outer * inner <= cores)
}

#' Run a Bounded Forecast Method Tournament
#'
#' @param regimes Named list of controlled data tables.
#' @param engines Candidate engines.
#' @param horizon Evaluation horizon.
#' @param origins Rolling-origin count.
#' @param frequency Forecast frequency.
#' @return Deterministic method/regime evidence and Pareto summaries.
#' @export
aq_forecast_method_tournament <- function(regimes, engines, horizon = 6L,
    origins = 3L, frequency = "month") {
  rows <- list()
  for (regime in names(regimes)) for (engine in engines) {
    spec <- aq_forecast_spec("value", "date", frequency = frequency,
      horizon = horizon, engine = engine, rolling_origins = origins,
      dataset_id = paste0("controlled:", regime))
    started <- proc.time()[[3L]]
    result <- tryCatch(aq_rolling_origin_forecast(spec, regimes[[regime]],
      origin_count = origins), error = identity)
    elapsed <- proc.time()[[3L]] - started
    if (inherits(result, "error")) rows[[length(rows)+1L]] <-
      data.table::data.table(regime = regime, engine = engine,
        status = "failed", rmse = NA_real_, mae = NA_real_,
        elapsed_seconds = elapsed, failure = conditionMessage(result)) else {
      metrics <- result$metrics[, .(value = mean(value, na.rm = TRUE)), by = metric]
      rows[[length(rows)+1L]] <- data.table::data.table(regime = regime,
        engine = engine, status = "succeeded",
        rmse = metrics[metric == "rmse", value],
        mae = metrics[metric == "mae", value], elapsed_seconds = elapsed,
        failure = NA_character_)
    }
  }
  evidence <- data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
  evidence[, rank_rmse := data.table::frank(rmse, ties.method = "min",
    na.last = "keep"), by = regime]
  list(schema_version = "aq_forecast_method_tournament_v1",
    tournament_id = aq_vnext_id("forecast_method_tournament"),
    evidence = evidence,
    envelopes = evidence[status == "succeeded",
      .SD[which.min(rmse)], by = regime],
    no_universal_winner = data.table::uniqueN(
      evidence[status == "succeeded", engine[which.min(rmse)], by = regime]$V1) > 1L)
}

#' Qualify Forecasting Reference Upgrade Wave 1B
#'
#' Runs a bounded deterministic contract pack for accepted Wave 1B methods and
#' non-fitting forecast operators.
#'
#' @return A data table of named qualification checks.
#' @export
qa_vnext_forecasting_wave1b <- function() {
  checks <- list()
  add <- function(check, passed, detail = "") checks[[length(checks) + 1L]] <<-
    data.table::data.table(check = check, passed = isTRUE(passed),
      detail = as.character(detail))
  dates <- seq(as.Date("2018-01-01"), by = "month", length.out = 96L)
  fixture <- data.table::data.table(date = dates,
    value = 40 + 0.3 * seq_along(dates) +
      6 * sin(2 * pi * seq_along(dates) / 12))
  for (engine in c("theta", "arfima")) {
    spec <- aq_forecast_spec("value", "date", frequency = "month",
      horizon = 6L, engine = engine, rolling_origins = 2L,
      dataset_id = "qa_wave1b")
    fit <- aq_fit_forecast(spec, fixture)
    restored <- unserialize(serialize(fit$model, NULL))
    replay <- forecast::forecast(restored, h = 3L)
    add(paste0(engine, "_fit"), inherits(fit, "aq_forecast_result") &&
      all(is.finite(fit$data$forecast)))
    add(paste0(engine, "_native_evidence"),
      identical(fit$engine_diagnostics$engine, engine) &&
        is.list(fit$engine_diagnostics$residual_diagnostics))
    add(paste0(engine, "_restart_application"),
      length(replay$mean) == 3L && all(is.finite(replay$mean)))
  }
  decomposition <- aq_mstl_decompose(
    20 + sin(2 * pi * seq_len(240L) / 12) +
      cos(2 * pi * seq_len(240L) / 24), c(12L, 24L))
  add("mstl_decomposition", nrow(decomposition$components) == 240L &&
    length(decomposition$evidence$seasonal_components) == 2L)
  quantiles <- data.table::CJ(observation = seq_len(4L),
    probability = c(0.1, 0.5, 0.9))
  quantiles[, forecast := rep(c(10, 11, 12, 13), each = 3L) +
    rep(c(-1, 0, 1), 4L)]
  intervals <- data.table::data.table(observation = seq_len(4L),
    lower = c(8, 9, 10, 11), upper = c(12, 13, 14, 15),
    nominal_coverage = 0.8)
  probability <- aq_score_probabilistic_forecast(c(10, 11, 12, 13),
    quantiles, intervals)
  add("probabilistic_scores", nrow(probability$pinball) == 3L &&
    all(c("wis", "average_width") %in% names(probability$interval_scores)))
  add("unsupported_density_scores_honest",
    all(c("crps", "log_score") %in% names(probability$unsupported_scores)))
  conformal <- aq_conformal_forecast_intervals(c(10, 11),
    c(-1, 0.5, 2, -0.25), 0.9)
  add("conformal_separate_uncertainty", nrow(conformal) == 2L &&
    all(conformal$uncertainty_origin ==
      "split_conformal_historical_residuals"))
  S <- rbind(c(1, 1), c(1, 0), c(0, 1))
  base <- data.table::CJ(horizon = 1:2, node = c("total", "a", "b"))
  base[, forecast := c(12, 5, 6, 21, 9, 10)]
  for (method in c("ols", "wls", "mint")) {
    args <- list(base = base, summing_matrix = S,
      node_order = c("total", "a", "b"), method = method)
    if (identical(method, "wls")) args$variances <- c(2, 1, 1)
    if (identical(method, "mint")) args$covariance <- diag(c(2, 1, 1))
    reconciled <- do.call(aq_reconcile_forecast_advanced, args)
    add(paste0("reconciliation_", method),
      max(reconciled$evidence$coherence$coherence_error) < 1e-8 &&
        identical(reconciled$base_forecast, base))
  }
  components <- data.table::CJ(component_id = c("theta", "arima"),
    horizon = 1:3)
  components[, forecast := c(10, 11, 12, 9, 12, 13)]
  combined <- aq_combine_forecasts(components, "inverse_error",
    data.table::data.table(component_id = c("theta", "arima"),
      error = c(1, 2)), component_revisions = list(theta = "fmr_theta",
        arima = "fmr_arima"))
  add("forecast_combination", nrow(combined$forecast) == 3L &&
    abs(combined$weights[, sum(weight), by = horizon]$V1[1L] - 1) < 1e-12)
  add("combination_component_provenance",
    identical(names(combined$component_revisions), c("theta", "arima")))
  policy <- aq_forecast_parallel_policy(100L, 8L, 2L, 8L)
  add("bounded_panel_parallelism", policy$outer_series_workers == 4L &&
    policy$maximum_concurrent_threads == 8L &&
    isTRUE(policy$nested_oversubscription_prevented))
  result <- data.table::rbindlist(checks, use.names = TRUE, fill = TRUE)
  attr(result, "passed") <- all(result$passed)
  result
}
