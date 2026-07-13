library(data.table)
library(AutoQuant)

set.seed(21)

dates <- seq.Date(as.Date("2026-01-01"), by = "week", length.out = 52)
planning_data <- data.table(
  date = dates,
  region = rep(c("north", "south"), length.out = length(dates)),
  promo = as.integer(seq_along(dates) %% 6 == 0)
)
planning_data[, leads := round(120 + seq_len(.N) * 1.4 + 12 * promo + rnorm(.N, 0, 6))]
planning_data[, applications := round(leads * 0.42 + rnorm(.N, 0, 5))]
planning_data[, enrollments := round(applications * 0.28 + 4 * promo + rnorm(.N, 0, 3))]
planning_data[seq(1L, .N, by = 8L), enrollments := 0]

discovery <- aq_discover_forecasting_capabilities(
  planning_data,
  target = c("leads", "applications", "enrollments"),
  date = "date",
  hierarchy = "region",
  known_future_variables = "promo",
  horizon = 3,
  forecast_origin = max(planning_data$date) - 21,
  dataset_id = "forecasting_planning_example"
)

plan <- aq_plan_forecasting_strategy(discovery)

stopifnot(inherits(discovery, "aq_forecasting_capability_discovery"))
stopifnot(inherits(plan, "aq_forecasting_strategy_plan"))
stopifnot(inherits(plan$artifact, "aq_artifact"))
stopifnot(any(plan$recommendations$operator == "cross_target_features"))
stopifnot(any(plan$artifact$metadata$carma_mechanism_inventory$tuning_layer == "feature_tuning"))

list(
  supported_operators = discovery$supported_operators,
  recommendations = plan$recommendations,
  required_baselines = plan$required_baselines,
  experiment_set = plan$experiment_set,
  planning_artifact = plan$artifact
)
