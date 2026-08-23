#' Qualify the expert CatBoost forecasting control surface
#'
#' Confirms that production defaults retain CatBoost's serious tree budget and
#' that important native temporal, probabilistic, structural, stochastic, and
#' compute controls reach the engine unchanged.
#' @return A compact data table of qualification checks.
#' @export
qa_forecast_catboost_controls <- function() {
  add <- function(check, passed, detail = "") data.table::data.table(
    check = check, passed = isTRUE(passed), detail = as.character(detail)[1L])
  base <- aq_vnext_engine_params()
  expert <- list(
    iterations = 1750L,
    depth = 8L,
    learning_rate = 0.025,
    loss_function = "MultiQuantile:alpha=0.1,0.5,0.9",
    has_time = TRUE,
    boosting_type = "Ordered",
    grow_policy = "Depthwise",
    min_data_in_leaf = 25L,
    rsm = 0.8,
    monotone_constraints = c(1, 0, -1),
    langevin = TRUE,
    diffusion_temperature = 5000,
    bootstrap_type = "MVS",
    max_ctr_complexity = 3L,
    task_type = "GPU",
    devices = "0:1",
    early_stopping_rounds = 75L
  )
  spec <- list(engine_parameters = c(expert, list(
    lag_periods = c(1L, 7L), rolling_windows = 7L)))
  passed <- aq_forecast_catboost_model_params(spec)
  unknown <- try(aq_vnext_engine_params(list(not_a_catboost_parameter = 1)),
    silent = TRUE)
  out <- data.table::rbindlist(list(
    add("production_tree_budget", identical(base$iterations, 1000L) &&
      identical(base$depth, 6L) && is.null(base$learning_rate)),
    add("has_time_default_true", isTRUE(base$has_time)),
    add("temporal_controls", identical(passed$has_time, TRUE) &&
      identical(passed$boosting_type, "Ordered")),
    add("probabilistic_objective", identical(passed$loss_function,
      expert$loss_function)),
    add("structural_controls", identical(passed$grow_policy, "Depthwise") &&
      identical(passed$monotone_constraints, c(1, 0, -1))),
    add("stochastic_controls", identical(passed$langevin, TRUE) &&
      identical(passed$diffusion_temperature, 5000)),
    add("categorical_controls", identical(passed$max_ctr_complexity, 3L)),
    add("compute_controls", identical(passed$task_type, "GPU") &&
      identical(passed$devices, "0:1")),
    add("orchestration_not_forwarded", !any(c("lag_periods", "rolling_windows") %in%
      names(passed))),
    add("unknown_controls_fail_closed", inherits(unknown, "try-error"))
  ))
  attr(out, "passed") <- all(out$passed)
  out
}

#' Qualify native execution of advanced CatBoost forecast controls
#'
#' Runs nonlinear regression with the production default budget, ordered
#' temporal boosting, and a multi-quantile objective when CatBoost is present.
#' @return A compact data table with execution checks and timing evidence.
#' @export
qa_forecast_catboost_engine_execution <- function() {
  add <- function(check, passed, detail = "") data.table::data.table(
    check = check, passed = isTRUE(passed), detail = as.character(detail)[1L])
  if (!requireNamespace("catboost", quietly = TRUE)) {
    out <- add("catboost_dependency", FALSE,
      "catboost is unavailable; native execution was not falsely qualified.")
    attr(out, "passed") <- FALSE
    attr(out, "status") <- "dependency_missing"
    return(out)
  }
  set.seed(4201L)
  x <- matrix(stats::rnorm(3000L), nrow = 600L, ncol = 5L)
  y <- sin(2 * x[, 1L]) + x[, 2L]^2 + 0.5 * x[, 3L] * x[, 4L] +
    stats::rnorm(600L, sd = 0.15)
  pool <- catboost::catboost.load_pool(x, label = y)
  elapsed <- system.time({
    production_model <- catboost::catboost.train(pool,
      params = aq_vnext_engine_params())
  })[["elapsed"]]
  production_prediction <- as.numeric(catboost::catboost.predict(
    production_model, pool))
  production_rmse <- sqrt(mean((production_prediction - y)^2))

  temporal_model <- catboost::catboost.train(pool, params =
    aq_vnext_engine_params(list(iterations = 80L, has_time = TRUE,
      boosting_type = "Ordered")))
  temporal_prediction <- as.numeric(catboost::catboost.predict(temporal_model,
    pool))

  quantile_loss <- "MultiQuantile:alpha=0.1,0.5,0.9"
  quantile_model <- catboost::catboost.train(pool, params =
    aq_vnext_engine_params(list(iterations = 80L,
      loss_function = quantile_loss, eval_metric = quantile_loss)))
  quantile_prediction <- catboost::catboost.predict(quantile_model, pool,
    prediction_type = "RawFormulaVal")

  out <- data.table::rbindlist(list(
    add("production_default_executes", all(is.finite(production_prediction)),
      sprintf("iterations=1000 depth=6 elapsed=%.3fs", elapsed)),
    add("nonlinear_fit_not_stump", is.finite(production_rmse) &&
      production_rmse < stats::sd(y) * 0.45,
      sprintf("rmse=%.5f target_sd=%.5f", production_rmse, stats::sd(y))),
    add("ordered_temporal_executes", all(is.finite(temporal_prediction))),
    add("multiquantile_executes", is.matrix(quantile_prediction) &&
      ncol(quantile_prediction) == 3L && all(is.finite(quantile_prediction)),
      paste(dim(quantile_prediction), collapse = "x"))
  ))
  attr(out, "passed") <- all(out$passed)
  attr(out, "status") <- if (all(out$passed)) "passed" else "failed"
  out
}
