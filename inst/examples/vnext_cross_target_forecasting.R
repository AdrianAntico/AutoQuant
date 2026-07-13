# AutoQuant vNext cross-target feature forecasting quick start

library(data.table)
library(AutoQuant)

dates <- seq.Date(as.Date("2025-01-01"), by = "week", length.out = 52)
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
  horizon = 2,
  forecast_origin = max(dt$date) - 14,
  known_future_variables = "promo",
  shared_predictors = "spend",
  target_specific_predictors = list(enrollments = "promo"),
  strategy = "cross_target_features",
  engine = "catboost",
  cross_target_feature_policy = "lags_rolls",
  shared_target_lags = c(1, 2),
  shared_rolling_windows = 3,
  target_relationship_metadata = list(source = "example", causal_claim = FALSE),
  engine_parameters = list(iterations = 3, depth = 2, learning_rate = 0.1),
  dataset_id = "vnext_cross_target_example"
)

validation <- aq_validate_multitarget_forecast_spec(spec, dt)
forecast <- aq_fit_multitarget_forecast(spec, dt)
assessment <- aq_assess_multitarget_forecast(forecast)
comparison <- aq_compare_multitarget_strategies(spec, dt)

stopifnot(
  !any(validation$status %in% c("fail", "error")),
  inherits(forecast, "aq_multitarget_forecast_result"),
  inherits(assessment, "aq_multitarget_forecast_assessment_result"),
  inherits(comparison, "aq_multitarget_strategy_comparison_result"),
  "cross_target_other" %in% forecast$feature_importance$feature_role
)

list(
  validation = validation,
  forecast = forecast,
  assessment = assessment,
  comparison = comparison
)
