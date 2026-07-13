library(data.table)
library(AutoQuant)

causal_question <- aq_causal_question(list(
  causal_question_id = "cq_budget_revenue",
  decision_context_id = "decision_budget",
  exposure = "budget",
  outcome = "revenue",
  population = "eligible markets",
  unit_of_analysis = "market-week",
  time_zero = "campaign start",
  treatment_window = "four weeks",
  outcome_window = "six weeks",
  comparison_condition = "current budget",
  intervention_definition = "increase eligible-market budget from current range to approved test range",
  estimand = "ATE",
  effect_scale = "incremental revenue",
  target_population = "eligible markets"
))

causal_context <- aq_causal_context(
  causal_question,
  assumptions = c("intervention controllable", "measurement available")
)

experiment_question <- aq_experiment_question(list(
  experiment_question_id = "eq_budget_test",
  causal_question_id = "cq_budget_revenue",
  decision_context_id = "decision_budget",
  hypothesis = "Increasing budget raises revenue.",
  null_claim = "No revenue lift.",
  alternative_claim = "Revenue lift is positive.",
  treatment = "approved budget increase",
  comparison = "current budget",
  estimand = "ATE",
  assignment_population = "eligible markets",
  expected_mechanism = "More qualified reach creates more revenue.",
  primary_outcome = "revenue",
  guardrails = "cpa,customer_quality",
  decision_rule = "Approve rollout if lift exceeds cost and guardrails hold.",
  authority = "marketing_approval",
  coverage = "eligible_markets"
), causal_context)

design <- aq_experiment_design_spec(
  experiment_question,
  design_type = "stratified_randomized",
  assignment_unit = "market",
  analysis_unit = "market-week",
  stratification_variables = "region",
  pre_period = "eight weeks",
  treatment_period = "four weeks"
)

eligible_units <- data.table(
  unit_id = paste0("m", 1:40),
  region = rep(c("East", "West"), each = 20),
  baseline_revenue = seq(80, 119)
)

assignment <- aq_assignment_plan(design, eligible_units, strata_cols = "region")
balance <- aq_assess_assignment_balance(assignment, eligible_units, baseline_cols = "baseline_revenue")
power <- aq_power_plan(outcome_type = "continuous", baseline_sd = 12, minimum_detectable_effect = 5)
timing <- aq_experiment_timing_plan(treatment_duration_days = 28, outcome_maturation_days = 14, reporting_delay_days = 7, pre_period_days = 56)
measurement <- aq_measurement_plan(
  primary_outcome = "revenue",
  guardrails = c("cpa", "customer_quality"),
  exposure_verification = "spend delivery logs",
  compliance_measure = "budget adherence",
  treatment_receipt = "delivered spend",
  data_source = "analytics warehouse",
  owner = "analytics"
)
threats <- aq_validity_threat_register(design_spec = design, measurement_plan = measurement)
interference <- aq_interference_plan("geographic_spillover", design)
info <- aq_experiment_information_value("high", "high", experiment_cost = 1000, duration_days = 49, reversibility = TRUE)
gate <- aq_experiment_gate_assessment(
  experiment_question,
  design,
  assignment,
  power,
  measurement,
  threats,
  authority_approved = TRUE,
  coverage_approved = TRUE,
  information_value = info
)

artifact <- aq_experiment_plan_artifact(
  experiment_question,
  design,
  assignment,
  power,
  timing,
  measurement,
  threats,
  interference,
  gate,
  info
)

print(experiment_question$validation)
print(design$validation)
print(balance)
print(gate)
print(artifact$metadata)
