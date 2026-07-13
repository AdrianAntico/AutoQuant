# AutoQuant vNext multi-target forecasting quick start

library(data.table)
library(AutoQuant)

dates <- seq.Date(as.Date("2025-01-01"), by = "week", length.out = 36)
idx <- seq_along(dates)

dt <- data.table(
  date = dates,
  leads = 100 + idx * 2 + sin(idx / 3) * 8,
  applications = 60 + idx * 1.4 + sin(idx / 3 + 0.3) * 5,
  enrollments = 28 + idx * 0.8 + sin(idx / 3 + 0.7) * 3,
  spend = 500 + idx * 5,
  promo = rep(c(0, 1, 0, 0), length.out = length(idx))
)

spec <- aq_multitarget_forecast_spec(
  targets = c("leads", "applications", "enrollments"),
  date = "date",
  frequency = "week",
  horizon = 3,
  forecast_origin = max(dt$date) - 21,
  known_future_variables = "promo",
  shared_predictors = "spend",
  target_specific_predictors = list(enrollments = "promo"),
  strategy = "independent",
  engine = "seasonal_naive",
  season_length = 4,
  dataset_id = "vnext_multitarget_example"
)

validation <- aq_validate_multitarget_forecast_spec(spec, dt)

independent_forecast <- aq_fit_multitarget_forecast(spec, dt)
independent_assessment <- aq_assess_multitarget_forecast(independent_forecast)

shared_spec <- spec
shared_spec$strategy <- "shared_workflow"
shared_forecast <- aq_fit_multitarget_forecast(shared_spec, dt)
shared_assessment <- aq_assess_multitarget_forecast(shared_forecast)

comparison <- aq_compare_multitarget_strategies(spec, dt)

stopifnot(
  !any(validation$status %in% c("fail", "error")),
  inherits(independent_forecast, "aq_multitarget_forecast_result"),
  inherits(independent_assessment, "aq_multitarget_forecast_assessment_result"),
  inherits(shared_forecast, "aq_multitarget_forecast_result"),
  inherits(shared_assessment, "aq_multitarget_forecast_assessment_result"),
  inherits(comparison, "aq_multitarget_strategy_comparison_result")
)

list(
  validation = validation,
  independent_forecast = independent_forecast,
  independent_assessment = independent_assessment,
  shared_forecast = shared_forecast,
  shared_assessment = shared_assessment,
  comparison = comparison
)
