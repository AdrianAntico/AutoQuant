# AutoQuant vNext funnel forecasting quick start

library(data.table)
library(AutoQuant)

cohorts <- paste0("cohort_", sprintf("%02d", 1:8))
stages <- c("prospects", "applications", "qualified", "enrollments")
dates <- seq.Date(as.Date("2026-01-01"), by = "week", length.out = length(cohorts))
rows <- list()

for (i in seq_along(cohorts)) {
  base <- 120 + i * 8
  rates <- c(1, 0.62 + i * 0.005, 0.72 - i * 0.003, 0.45 + i * 0.004)
  volume <- base
  for (j in seq_along(stages)) {
    if (j > 1L) volume <- volume * rates[j]
    rows[[length(rows) + 1L]] <- data.table(
      cohort = cohorts[i],
      date = dates[i],
      stage_date = dates[i] + (j - 1L) * 7L,
      maturity = j - 1L,
      stage = stages[j],
      volume = round(volume)
    )
  }
}

dt <- rbindlist(rows, use.names = TRUE)
future_actuals <- copy(dt[date >= dates[7]])
future_actuals[, date := date + 14L]
future_actuals[, maturity := maturity + 2L]
future_actuals[, volume := round(volume * 1.04)]
dt <- rbindlist(list(dt, future_actuals), use.names = TRUE)

spec <- aq_funnel_forecast_spec(
  stages = stages,
  stage = "stage",
  value = "volume",
  date = "date",
  cohort = "cohort",
  stage_date = "stage_date",
  maturity = "maturity",
  frequency = "week",
  horizon = 2,
  forecast_origin = as.Date("2026-02-12"),
  strategy = "stage",
  dataset_id = "vnext_funnel_example"
)

validation <- aq_validate_funnel_forecast_spec(spec, dt)

stage_forecast <- aq_fit_funnel_forecast(spec, dt)
stage_assessment <- aq_assess_funnel_forecast(stage_forecast)

transition_spec <- spec
transition_spec$strategy <- "transition"
transition_forecast <- aq_fit_funnel_forecast(transition_spec, dt)
transition_assessment <- aq_assess_funnel_forecast(transition_forecast)

comparison <- aq_compare_funnel_strategies(spec, dt)

stopifnot(
  !any(validation$status %in% c("fail", "error")),
  inherits(stage_forecast, "aq_funnel_forecast_result"),
  inherits(stage_assessment, "aq_funnel_forecast_assessment_result"),
  inherits(transition_forecast, "aq_funnel_forecast_result"),
  inherits(transition_assessment, "aq_funnel_forecast_assessment_result"),
  inherits(comparison, "aq_funnel_strategy_comparison_result")
)

list(
  validation = validation,
  stage_forecast = stage_forecast,
  stage_assessment = stage_assessment,
  transition_forecast = transition_forecast,
  transition_assessment = transition_assessment,
  comparison = comparison
)
