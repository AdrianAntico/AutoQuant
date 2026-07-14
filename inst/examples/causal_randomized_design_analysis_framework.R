library(AutoQuant)
library(data.table)

set.seed(20260713)
n <- 80L

completed <- aq_completed_experiment(list(
  completed_experiment_id = "ce_depth_example",
  experiment_plan_artifact_id = "plan_depth_example",
  decision_context_id = "decision_depth_example",
  causal_question_id = "cq_depth_example",
  estimand_id = "estimand_depth_example",
  design_version = "v1",
  assignment_version = "v1",
  experiment_status = "completed",
  actual_start_date = "2026-01-01",
  actual_end_date = "2026-02-01",
  data_cutoff_date = "2026-02-15",
  execution_owner = "analytics"
))

assignment <- aq_assignment_evidence(data.table(
  unit_id = paste0("u", seq_len(n)),
  planned_arm = rep(c("control", "treatment"), each = n / 2),
  realized_assigned_arm = rep(c("control", "treatment"), each = n / 2),
  block = rep(c("b1", "b2"), times = n / 2),
  cluster = rep(paste0("geo", 1:8), each = 10)
))

outcomes <- aq_outcome_evidence(data.table(
  unit_id = paste0("u", seq_len(n)),
  outcome_id = "revenue",
  value = c(rnorm(n / 2, 10, 2), rnorm(n / 2, 13, 2)),
  outcome_role = "primary"
))

baseline <- data.table(
  unit_id = paste0("u", seq_len(n)),
  baseline_y = rnorm(n),
  pre_revenue = rnorm(n, 9, 2),
  block = rep(c("b1", "b2"), times = n / 2),
  cluster = rep(paste0("geo", 1:8), each = 10)
)

missingness <- aq_assess_missingness_attrition(assignment, outcomes)
reconciliation <- aq_reconcile_experiment_execution(completed, assignment_evidence = assignment, outcome_evidence = outcomes)
integrity <- aq_assess_randomization_integrity(assignment)
fidelity <- aq_assess_treatment_fidelity(assignment)
interference <- aq_assess_interference_spillover()
guardrails <- aq_assess_guardrails(aq_outcome_evidence(data.table(
  unit_id = paste0("u", seq_len(n)),
  outcome_id = "cost_guardrail",
  value = 0,
  outcome_role = "guardrail"
)))
estimand <- aq_assess_estimand_preservation(completed, reconciliation, integrity, fidelity, missingness, interference, outcomes)
readiness <- aq_assess_experiment_analysis_readiness(completed, assignment, outcomes, reconciliation, integrity, fidelity, missingness, estimand, guardrails)

evidence <- list(
  completed = completed,
  assignment = assignment,
  outcomes = outcomes,
  missingness = missingness,
  reconciliation = reconciliation,
  integrity = integrity,
  fidelity = fidelity,
  interference = interference,
  guardrails = guardrails,
  estimand = estimand,
  readiness = readiness
)

planned <- aq_planned_analysis_record(
  completed,
  readiness,
  outcome_variables = "revenue",
  baseline_covariates = "baseline_y,pre_revenue"
)

itt_spec <- aq_randomized_itt_spec(
  completed_experiment_id = "ce_depth_example",
  experiment_plan_artifact_id = "plan_depth_example",
  causal_question_id = "cq_depth_example",
  estimand_id = "estimand_depth_example",
  treatment_arm = "treatment",
  comparison_arm = "control",
  outcome = "revenue",
  outcome_type = "continuous",
  baseline_covariates = c("baseline_y", "pre_revenue"),
  blocking_variables = "block",
  cluster_variable = "cluster",
  standard_error_method = "cluster",
  minimum_meaningful_effect = 1
)

itt_result <- aq_estimate_randomized_itt(itt_spec, evidence, baseline, planned)

design_spec <- aq_randomized_design_analysis_spec(
  itt_analysis_id = itt_spec$analysis_id,
  design_type = "blocked_randomized",
  analysis_modes = c("unadjusted", "ancova", "cuped", "blocked", "randomization_inference"),
  block_fields = "block",
  cluster_unit = "cluster",
  pre_period_fields = "pre_revenue",
  material_benefit = 1,
  material_harm = -1,
  multiplicity_policy = "holm"
)

depth <- aq_analyze_randomized_design_depth(itt_result, design_spec)
report <- aq_randomized_causal_effect_report(itt_result, depth)

depth$robustness_matrix
report$sections
