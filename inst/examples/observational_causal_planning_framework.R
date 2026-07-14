library(AutoQuant)
library(data.table)

study <- aq_observational_study(data.table(
  observational_study_id = "obs_discount",
  decision_context_id = "decision_discount",
  causal_question_id = "cq_discount_revenue",
  estimand_id = "estimand_ate",
  study_title = "Discount eligibility observational study",
  treatment = "discount eligible",
  comparison_condition = "not discount eligible",
  unit_of_analysis = "customer",
  population = "active customers",
  eligibility = "active before score date",
  treatment_assignment_time = "eligibility score date",
  treatment_window = "30 days",
  outcome_window = "60 days after assignment",
  baseline_window = "90 days before assignment",
  index_date = "2026-01-01",
  data_cutoff = "2026-04-01",
  organizational_scope = "growth",
  authority = "analytics",
  coverage = "example",
  status = "draft"
))

target_trial <- aq_target_trial_spec(
  study,
  eligibility_criteria = "active before score date",
  treatment_strategies = c("eligible for discount", "not eligible for discount"),
  assignment_time = "score date",
  follow_up = "60 days",
  outcome = "revenue",
  causal_contrast = "ATE",
  estimand = "ATE"
)

assignment <- aq_observational_assignment_mechanism(
  mechanism_type = "eligibility_threshold",
  decision_process = "score-based eligibility rule",
  assignment_inputs = c("prior_spend", "region"),
  timing = "pre_treatment",
  confidence = "moderate"
)

treatment <- aq_observational_treatment_definition(
  treatment_name = "discount eligible",
  comparison_condition = "not discount eligible"
)

adjustment <- aq_observational_adjustment_spec(
  approved_confounders = c("prior_spend", "region"),
  optional_precision_variables = "tenure",
  excluded_mediators = "post_discount_clicks",
  excluded_colliders = "observed_purchase"
)

timing <- aq_assess_observational_temporal_eligibility(
  adjustment,
  data.table(
    variable = c("prior_spend", "region", "tenure", "post_discount_clicks", "observed_purchase"),
    timing_status = c("pre_treatment", "baseline", "baseline stale", "post_treatment", "post_treatment")
  )
)

variation <- aq_assess_observational_variation(treated_count = 120, comparison_count = 160)
assignment_model <- aq_observational_assignment_model_diagnostics(seq(0.12, 0.88, length.out = 100))
overlap <- aq_assess_observational_overlap(assignment_model = assignment_model)

balance_data <- data.table(
  treat = rep(c(0, 1), each = 60),
  prior_spend = c(rnorm(60, 10, 2), rnorm(60, 11, 2)),
  region = rep(c("A", "B"), 60)
)
balance <- aq_assess_observational_balance(balance_data, "treat", c("prior_spend", "region"))

selection <- aq_assess_observational_selection_missingness(data.table(
  threat = "outcome missingness",
  timing = "post_treatment",
  severity = "moderate",
  evidence = "source-system review",
  recommendation = "review outcome-observation process"
))

unmeasured <- aq_unmeasured_confounding_risk_register(data.table(
  factor = "manager discretion",
  confidence = "high",
  decision_consequence = "high uncertainty"
))

falsification <- aq_observational_falsification_plan(data.table(
  test_type = "placebo_period",
  rationale = "pre-treatment pseudo-effect",
  expected_relationship = "null",
  required_data = "pre-period outcomes",
  interpretation = "detects residual bias",
  limitations = "does not prove causality"
))

designs <- aq_observational_design_eligibility(
  study, variation, overlap, adjustment, target_trial,
  evidence = list(pre_period_available = TRUE, negative_control_available = TRUE)
)

plan <- aq_plan_observational_causal_analysis(
  study, target_trial, assignment, treatment, adjustment, timing,
  variation, overlap, balance, selection, unmeasured, falsification, designs
)

readiness <- aq_assess_observational_estimation_readiness(
  plan, overlap, adjustment, selection, unmeasured, falsification
)

artifact <- aq_observational_causal_planning_artifact(study, plan, readiness)

readiness
artifact$metadata$prohibited_claims
