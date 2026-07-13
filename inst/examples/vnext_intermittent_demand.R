# AutoQuant vNext intermittent-demand forecasting quick start

library(data.table)
library(AutoQuant)

dates <- seq.Date(as.Date("2025-01-01"), by = "day", length.out = 100)
dt <- data.table(
  ds = rep(dates, 2),
  sku = rep(c("SKU_A", "SKU_B"), each = length(dates))
)
dt[, demand := {
  interval <- if (sku[1] == "SKU_A") 5L else 9L
  sizes <- if (sku[1] == "SKU_A") c(3, 5, 8) else c(2, 4, 7)
  fifelse(seq_len(.N) %% interval == 0, sample(sizes, .N, TRUE), 0)
}, by = sku]

diagnostics <- aq_intermittent_demand_diagnostics(dt[sku == "SKU_A"], target = "demand", date = "ds")

croston_spec <- aq_croston_forecast_spec(
  target = "demand",
  date = "ds",
  entity = "sku",
  frequency = "day",
  horizon = 7,
  alpha = 0.2,
  dataset_id = "vnext_intermittent_example"
)

sba_spec <- aq_sba_forecast_spec(
  target = "demand",
  date = "ds",
  entity = "sku",
  frequency = "day",
  horizon = 7,
  alpha = 0.2,
  dataset_id = "vnext_intermittent_example"
)

tsb_spec <- aq_tsb_forecast_spec(
  target = "demand",
  date = "ds",
  entity = "sku",
  frequency = "day",
  horizon = 7,
  alpha = 0.2,
  beta = 0.2,
  dataset_id = "vnext_intermittent_example"
)

croston_fit <- aq_fit_croston_forecast(croston_spec, dt)
sba_fit <- aq_fit_sba_forecast(sba_spec, dt)
tsb_fit <- aq_fit_tsb_forecast(tsb_spec, dt)

comparison <- aq_compare_intermittent_demand_methods(
  data = dt,
  target = "demand",
  date = "ds",
  entity = "sku",
  frequency = "day",
  horizon = 3,
  origins = as.Date(c("2025-03-01", "2025-03-08")),
  alpha = 0.2,
  beta = 0.2,
  include_hurdle = FALSE,
  dataset_id = "vnext_intermittent_comparison_example"
)

stopifnot(
  nrow(diagnostics) > 0,
  inherits(croston_fit, "aq_croston_forecast_result"),
  inherits(sba_fit, "aq_sba_forecast_result"),
  inherits(tsb_fit, "aq_tsb_forecast_result"),
  inherits(comparison, "aq_intermittent_method_comparison_result")
)

list(
  diagnostics = diagnostics,
  croston_fit = croston_fit,
  sba_fit = sba_fit,
  tsb_fit = tsb_fit,
  comparison = comparison
)
