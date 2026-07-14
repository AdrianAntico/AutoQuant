library(data.table)
library(AutoQuant)

completed <- aq_completed_experiment(list(
  completed_experiment_id = "ce_budget_test",
  experiment_plan_artifact_id = "aq_experiment_plan_budget_test",
  decision_context_id = "decision_budget",
  causal_question_id = "cq_budget_revenue",
  estimand_id = "estimand_budget_itt",
  design_version = "v1",
  assignment_version = "v1",
  experiment_status = "completed",
  actual_start_date = "2026-01-01",
  actual_end_date = "2026-02-01",
  data_cutoff_date = "2026-02-15",
  execution_owner = "analytics"
))

assignment <- aq_assignment_evidence(data.table(
  unit_id = paste0("market_", 1:12),
  planned_arm = rep(c("control", "treatment"), each = 6),
  realized_assigned_arm = rep(c("control", "treatment"), each = 6)
))

delivery <- aq_treatment_delivery_evidence(data.table(
  unit_id = paste0("market_", 1:12),
  delivered_condition = rep(c("control", "treatment"), each = 6),
  delivery_status = "delivered"
))

outcomes <- aq_outcome_evidence(data.table(
  unit_id = paste0("market_", 1:12),
  outcome_id = "revenue",
  value = seq(100, 155, by = 5),
  outcome_role = "primary"
))

guardrails <- aq_assess_guardrails(aq_outcome_evidence(data.table(
  unit_id = paste0("market_", 1:12),
  outcome_id = "cpa_guardrail",
  value = 0,
  outcome_role = "guardrail"
)))

missingness <- aq_assess_missingness_attrition(assignment, outcomes)
reconciliation <- aq_reconcile_experiment_execution(
  completed,
  assignment_evidence = assignment,
  delivery_evidence = delivery,
  outcome_evidence = outcomes
)
integrity <- aq_assess_randomization_integrity(assignment, c("control", "treatment"))
fidelity <- aq_assess_treatment_fidelity(assignment, delivery)
interference <- aq_assess_interference_spillover()
estimand <- aq_assess_estimand_preservation(
  completed,
  reconciliation,
  integrity,
  fidelity,
  missingness,
  interference,
  outcomes
)
readiness <- aq_assess_experiment_analysis_readiness(
  completed,
  assignment,
  outcomes,
  reconciliation,
  integrity,
  fidelity,
  missingness,
  estimand,
  guardrails
)

planned <- aq_planned_analysis_record(
  completed,
  readiness,
  outcome_variables = "revenue"
)

artifact <- aq_completed_experiment_evidence_artifact(
  completed,
  assignment,
  delivery,
  outcome_evidence = outcomes,
  missingness = missingness,
  reconciliation = reconciliation,
  integrity = integrity,
  fidelity = fidelity,
  interference = interference,
  guardrails = guardrails,
  estimand_preservation = estimand,
  readiness = readiness,
  planned_analysis = planned
)

print(readiness)
print(artifact$metadata)
