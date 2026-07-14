library(AutoQuant)

workflow <- aq_decision_workflow(
  workflow_id = "workflow_pilot_budget",
  decision_context_id = "decision_next_quarter_budget",
  decision_version = "v1",
  valuation_artifact_id = "decision_valuation_artifact_1",
  recommendation_id = "recommendation_pilot",
  selected_alternative = "pilot",
  workflow_type = "pilot_approval",
  authority_tier = "manager",
  review_deadline = as.character(Sys.Date() + 7)
)

readiness <- aq_assess_decision_review_readiness(workflow)

evidence_package <- aq_decision_evidence_package(
  workflow,
  evidence_refs = c("decision_valuation_artifact_1", "randomized_itt_artifact_1")
)

request <- aq_decision_review_request(list(
  workflow_id = workflow$workflow_id,
  review_type = "financial",
  reviewer_role = "finance_owner",
  evidence_package_id = evidence_package$id,
  due_date = as.character(Sys.Date() + 5),
  required_response = "endorse_or_request_revision"
))

review <- aq_decision_review(list(
  workflow_id = workflow$workflow_id,
  review_request_id = request$review_request_id[[1]],
  reviewer = "Finance Owner",
  role = "financial",
  findings = "Pilot budget is acceptable if spend is monitored weekly.",
  conditions = "Weekly spend monitoring",
  status = "endorse_with_conditions",
  confidence = 0.8
))

approval <- aq_decision_approval(list(
  workflow_id = workflow$workflow_id,
  approver = "Budget Manager",
  authority_basis = "marketing_budget_authority",
  approved_alternative = "pilot",
  approved_budget = 100,
  authority_magnitude = 200,
  approved_timing = "next quarter",
  conditions = "Weekly spend monitoring",
  monitoring_requirements = "KPI and spend review",
  expiration = as.character(Sys.Date() + 30),
  status = "conditionally_approved"
))

approval_validation <- aq_validate_decision_approval(approval, workflow)

condition <- aq_decision_condition(list(
  workflow_id = workflow$workflow_id,
  source_id = approval$approval_id[[1]],
  condition_type = "monitoring",
  description = "Weekly spend monitoring",
  owner = "Analyst",
  due_date = as.character(Sys.Date() + 7),
  status = "open"
))

plan <- aq_decision_implementation_plan(list(
  workflow_id = workflow$workflow_id,
  implementation_id = "impl_pilot_budget",
  approved_decision = approval$approval_id[[1]],
  selected_alternative = "pilot",
  levers = "paid_search_budget",
  current_values = "100",
  approved_target_values = "110",
  budget = 100,
  measurement_plan = "Weekly KPI and guardrail monitoring",
  rollback_plan = "Return to baseline budget",
  status = "planned"
))

actual <- aq_record_decision_implementation(list(
  workflow_id = workflow$workflow_id,
  implementation_id = "impl_pilot_budget",
  actual_lever_settings = "paid_search_budget=110",
  actual_cost = 105,
  deviations = "",
  operational_owner = "Marketing Ops",
  evidence_quality = "observed"
))

reconciliation <- aq_reconcile_decision_implementation(plan, actual)

monitoring <- aq_decision_monitoring_plan(list(
  workflow_id = workflow$workflow_id,
  metric = "incremental_revenue",
  cadence = "weekly",
  threshold = 0,
  owner = "Analyst",
  escalation_rule = "Escalate if revenue or guardrail threshold fails"
))

quality <- aq_assess_decision_quality(workflow, readiness, review, approval, reconciliation)

realized <- aq_realized_value_review(
  workflow_id = workflow$workflow_id,
  expected_value = 200,
  realized_value = 180,
  expected_cost = 100,
  realized_cost = 105,
  maturation_status = "preliminary"
)

followups <- aq_decision_followup_candidates(workflow, quality, realized, reconciliation)

artifact <- aq_decision_workflow_artifact(
  workflow,
  readiness,
  review,
  approval,
  plan,
  reconciliation,
  monitoring,
  quality,
  realized,
  followups
)

qa <- qa_decision_workflow_framework()

stopifnot(
  any(readiness$readiness_state == "ready_for_review"),
  !any(approval_validation$status == "fail"),
  artifact$artifact_envelope$artifact_type == "decision_workflow_artifact",
  !any(qa$status == "fail")
)
