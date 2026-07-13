library(AutoQuant)
library(data.table)

set.seed(20260712)
history <- data.table(
  date = as.Date("2025-01-01") + 0:129,
  promo = sample(c(0, 1), 130, replace = TRUE, prob = c(0.8, 0.2))
)
history[, demand := 100 + 0.4 * seq_len(.N) + 12 * sin(seq_len(.N) / 7 * 2 * pi) + 8 * promo + rnorm(.N, 0, 4)]

discovery <- aq_discover_forecasting_capabilities(
  history,
  target = "demand",
  date = "date",
  known_future_variables = "promo",
  horizon = 3,
  forecast_origin = max(history$date) - 3,
  dataset_id = "forecast_experiment_example"
)

plan <- aq_plan_forecasting_strategy(discovery)

experiment <- aq_forecast_experiment_spec(
  planning_result = plan,
  experiment_type = "model",
  baseline = "naive",
  challenger = "ets",
  target = "demand",
  date = "date",
  horizon = 3,
  forecast_origin = max(history$date) - 3,
  hypothesis = "ETS may reduce out-of-sample forecast error relative to a frozen naive baseline."
)

result <- aq_run_forecast_experiment(experiment, history)
result$learning
result$artifact$metadata$comparison

campaign <- aq_run_forecast_experiment_campaign(list(experiment), history)
campaign$summary
