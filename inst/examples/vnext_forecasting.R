# AutoQuant vNext forecasting quick start

library(data.table)
library(AutoQuant)

dates <- seq.Date(as.Date("2025-01-01"), by = "day", length.out = 90)
dt <- data.table(
  ds = dates,
  demand = 50 + seq_along(dates) * 0.15 + 8 * sin(seq_along(dates) / 7) + rnorm(length(dates), 0, 2)
)

naive_spec <- aq_forecast_spec(
  target = "demand",
  date = "ds",
  frequency = "day",
  horizon = 7,
  engine = "naive",
  prediction_intervals = TRUE,
  dataset_id = "vnext_forecast_example"
)

seasonal_spec <- aq_forecast_spec(
  target = "demand",
  date = "ds",
  frequency = "day",
  horizon = 7,
  engine = "seasonal_naive",
  season_length = 7,
  prediction_intervals = TRUE,
  dataset_id = "vnext_forecast_example"
)

naive_fit <- aq_fit_forecast(naive_spec, dt)
seasonal_fit <- aq_fit_forecast(seasonal_spec, dt)
naive_assessment <- aq_assess_forecast(naive_fit)
rolling <- aq_rolling_origin_forecast(naive_spec, dt, origins = as.Date(c("2025-03-01", "2025-03-08")))

stopifnot(
  inherits(naive_fit, "aq_forecast_result"),
  inherits(seasonal_fit, "aq_forecast_result"),
  inherits(naive_assessment, "aq_forecast_assessment_result"),
  inherits(rolling, "aq_forecast_backtest_result")
)

list(
  naive_fit = naive_fit,
  seasonal_fit = seasonal_fit,
  naive_assessment = naive_assessment,
  rolling = rolling
)
