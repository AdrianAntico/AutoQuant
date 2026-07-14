library(data.table)
library(AutoQuant)

set.seed(20260713)

n <- 80L
completed <- aq_completed_experiment(list(
  completed_experiment_id = "ce_itt_example",
  experiment_plan_artifact_id = "plan_itt_example",
  decision_context_id = "decision_itt_example",
  causal_question_id = "cq_itt_example",
  estimand_id = "estimand_itt_example",
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
  realized_assigned_arm = rep(c("control", "treatment"), each = n / 2)
))

delivery <- aq_treatment_delivery_evidence(data.table(
  unit_id = paste0("u", seq_len(n)),
  delivered_condition = rep(c("control", "treatment"), each = n / 2),
  delivery_status = "delivered"
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
  market_cluster = rep(paste0("geo", 1:8), each = 10)
)

reconciliation <- aq_reconcile_experiment_execution(
  completed,
  assignment_evidence = assignment,
  delivery_evidence = delivery,
  outcome_evidence = outcomes
)
integrity <- aq_assess_randomization_integrity(assignment)
missingness <- aq_assess_missingness_attrition(assignment, outcomes)
fidelity <- aq_assess_treatment_fidelity(assignment, delivery)
interference <- aq_assess_interference_spillover()
estimand <- aq_assess_estimand_preservation(
  completed, reconciliation, integrity, fidelity, missingness, interference, outcomes
)
guardrails <- aq_assess_guardrails(
  aq_outcome_evidence(data.table(
    unit_id = paste0("u", seq_len(n)),
    outcome_id = "cost_guardrail",
    value = 0,
    outcome_role = "guardrail"
  ))
)
readiness <- aq_assess_experiment_analysis_readiness(
  completed, assignment, outcomes, reconciliation, integrity, fidelity, missingness, estimand, guardrails
)

completed_evidence <- list(
  completed = completed,
  assignment = assignment,
  delivery = delivery,
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
  baseline_covariates = "baseline_y"
)

spec <- aq_randomized_itt_spec(
  completed_experiment_id = "ce_itt_example",
  experiment_plan_artifact_id = "plan_itt_example",
  causal_question_id = "cq_itt_example",
  estimand_id = "estimand_itt_example",
  treatment_arm = "treatment",
  comparison_arm = "control",
  outcome = "revenue",
  outcome_type = "continuous",
  baseline_covariates = "baseline_y",
  cluster_variable = "market_cluster",
  standard_error_method = "cluster",
  minimum_meaningful_effect = 1
)

gate <- aq_validate_randomized_itt_readiness(
  spec,
  completed_evidence,
  planned_analysis = planned,
  baseline_data = baseline
)
print(gate)

result <- aq_estimate_randomized_itt(
  spec,
  completed_evidence,
  baseline_data = baseline,
  planned_analysis = planned
)
print(result$primary_estimate)
print(result$materiality)

artifact <- aq_randomized_itt_effect_artifact(result)
reviewed <- aq_review_randomized_itt_result(result, reviewer = "analyst", approve = TRUE)
seeds <- aq_randomized_itt_campaign_seeds(result)

print(class(artifact))
print(reviewed$status)
print(seeds)
