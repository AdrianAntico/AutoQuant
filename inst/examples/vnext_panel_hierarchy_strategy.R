# AutoQuant vNext panel, hierarchy, and strategy contract quick start

library(data.table)
library(AutoQuant)

dates <- seq.Date(as.Date("2025-01-01"), by = "day", length.out = 45)
dt <- CJ(store = c("A", "B", "C"), ds = dates)
dt[, region := fifelse(store %in% c("A", "B"), "North", "South")]
dt[, sales := 20 + fifelse(region == "North", 5, 0) + as.integer(ds - min(ds)) * 0.1 + rnorm(.N, 0, 1)]

panel_spec <- aq_panel_forecast_spec(
  entity = "store",
  target = "sales",
  date = "ds",
  frequency = "day",
  horizon = 3,
  minimum_history = 20,
  engine_parameters = list(iterations = 5, depth = 2, learning_rate = 0.15),
  dataset_id = "vnext_panel_example"
)

strategy_spec <- aq_panel_strategy_spec(
  entity = "store",
  target = "sales",
  date = "ds",
  group = "region",
  candidate_strategies = c("independent", "grouped", "global"),
  frequency = "day",
  horizon = 2,
  minimum_history = 20,
  rolling_origins = 1,
  engine_parameters = list(iterations = 5, depth = 2, learning_rate = 0.15),
  dataset_id = "vnext_panel_strategy_example"
)

hierarchy <- data.table(
  entity = c("All", "North", "South", "A", "B", "C"),
  parent = c(NA, "All", "All", "North", "North", "South"),
  level = c("total", "region", "region", "store", "store", "store")
)

hierarchy_spec <- aq_hierarchy_spec(hierarchy)

panel_validation <- aq_validate_panel_forecast_spec(panel_spec, dt)
strategy_validation <- aq_validate_panel_strategy_spec(strategy_spec, dt)
hierarchy_validation <- aq_validate_hierarchy_spec(hierarchy_spec)

stopifnot(
  inherits(panel_spec, "aq_panel_forecast_spec"),
  inherits(strategy_spec, "aq_panel_strategy_spec"),
  inherits(hierarchy_spec, "aq_hierarchy_spec"),
  !any(panel_validation$status %in% c("fail", "error")),
  !any(strategy_validation$status %in% c("fail", "error")),
  !any(hierarchy_validation$status %in% c("fail", "error"))
)

list(
  panel_spec = panel_spec,
  strategy_spec = strategy_spec,
  hierarchy_spec = hierarchy_spec,
  panel_validation = panel_validation,
  strategy_validation = strategy_validation,
  hierarchy_validation = hierarchy_validation
)
