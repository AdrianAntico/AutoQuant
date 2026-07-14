# AutoQuant deterministic decision-workflow framework.

aq_dw_chr <- function(x, default = NA_character_) {
  if (is.null(x) || !length(x) || is.na(x[[1]])) return(default)
  out <- as.character(x[[1]])
  if (!nzchar(out)) default else out
}

aq_dw_num <- function(x, default = NA_real_) {
  out <- suppressWarnings(as.numeric(x[[1]] %||% default))
  if (!is.finite(out)) default else out
}

aq_dw_bool <- function(x, default = FALSE) {
  if (is.null(x) || !length(x) || is.na(x[[1]])) return(default)
  isTRUE(x[[1]]) || tolower(as.character(x[[1]])) %in% c("true", "yes", "1")
}

aq_dw_fill_chr <- function(x, default) {
  out <- as.character(x)
  out[is.na(out) | !nzchar(out)] <- default
  out
}

aq_dw_parse_chr <- function(x) {
  if (is.null(x) || !length(x)) return(character())
  x <- paste(as.character(x), collapse = ",")
  x <- trimws(strsplit(x, ",", fixed = TRUE)[[1]])
  x[nzchar(x)]
}

aq_dw_dt <- function(x, id_col = NULL) {
  if (is.null(x)) return(data.table::data.table())
  out <- if (is.data.frame(x)) {
    data.table::as.data.table(x)
  } else {
    data.table::rbindlist(lapply(if (!is.null(names(x)) && any(nzchar(names(x)))) list(x) else x, function(record) data.table::as.data.table(as.list(record))), use.names = TRUE, fill = TRUE)
  }
  if (!is.null(id_col) && !id_col %in% names(out) && nrow(out)) stop("records must include ", id_col, ".", call. = FALSE)
  out
}

aq_dw_add_missing <- function(dt, cols, default = NA) {
  for (col in setdiff(cols, names(dt))) dt[, (col) := default]
  dt
}

aq_decision_workflow_stages <- function() {
  c(
    "draft", "evidence_gathering", "ready_for_review", "under_review",
    "revision_requested", "awaiting_approval", "approved", "conditionally_approved",
    "rejected", "deferred", "escalated", "implementation_planned",
    "implementing", "implemented", "monitoring", "outcome_review_due",
    "outcome_reviewed", "retained", "expanded", "modified", "contracted",
    "abandoned", "superseded", "closed"
  )
}

aq_decision_workflow_allowed_transitions <- function() {
  data.table::data.table(
    from = c(
      "draft", "draft", "evidence_gathering", "ready_for_review", "under_review",
      "under_review", "under_review", "revision_requested", "awaiting_approval",
      "awaiting_approval", "awaiting_approval", "approved", "conditionally_approved",
      "implementation_planned", "implementing", "implemented", "monitoring",
      "outcome_review_due", "outcome_reviewed", "outcome_reviewed", "outcome_reviewed",
      "outcome_reviewed", "outcome_reviewed", "deferred", "escalated"
    ),
    to = c(
      "evidence_gathering", "ready_for_review", "ready_for_review", "under_review",
      "revision_requested", "awaiting_approval", "escalated", "ready_for_review",
      "approved", "conditionally_approved", "rejected", "implementation_planned",
      "implementation_planned", "implementing", "implemented", "monitoring",
      "outcome_review_due", "outcome_reviewed", "retained", "expanded",
      "modified", "contracted", "abandoned", "closed", "awaiting_approval"
    )
  )
}

aq_decision_workflow_transition_allowed <- function(from, to) {
  if (identical(from, to)) return(TRUE)
  trans <- aq_decision_workflow_allowed_transitions()
  any(trans$from == from & trans$to == to)
}

aq_decision_workflow_templates <- function() {
  data.table::data.table(
    workflow_type = c("advisory_only", "analyst_review", "manager_approval", "functional_executive_approval", "cross_functional_approval", "enterprise_escalation", "pilot_approval", "emergency_expedited_review"),
    required_stages = c(
      "review",
      "analytical_review",
      "analytical_review,approval",
      "analytical_review,business_review,approval",
      "analytical_review,financial_review,operational_review,approval",
      "executive_review,risk_review,approval",
      "analytical_review,implementation_feasibility,approval,monitoring",
      "expedited_review,approval,post_review"
    ),
    minimum_authority_tier = c("advisory", "analyst", "manager", "functional_executive", "cross_functional", "enterprise", "manager", "emergency_delegate")
  )
}

#' Create a Decision Workflow
#' @export
aq_decision_workflow <- function(
  workflow_id = NULL,
  decision_context_id,
  decision_version = NA_character_,
  valuation_artifact_id = NA_character_,
  recommendation_id = NA_character_,
  selected_alternative = NA_character_,
  workflow_type = "manager_approval",
  risk_tier = "medium",
  authority_tier = "manager",
  required_stages = NULL,
  current_stage = "draft",
  stage_owners = character(),
  required_approvals = character(),
  review_deadline = NA_character_,
  conditions = character(),
  evidence_cutoff = NA_character_,
  status = "active",
  supported_actions = c("assess_readiness", "freeze_evidence_package", "request_review", "record_review", "record_approval", "plan_implementation", "record_implementation", "monitor", "review_outcome", "close")
) {
  templates <- aq_decision_workflow_templates()
  workflow_type_value <- workflow_type
  template <- templates[workflow_type == workflow_type_value]
  if (is.null(required_stages)) required_stages <- aq_dw_parse_chr(template$required_stages[[1]] %||% "review,approval")
  out <- list(
    schema_version = "aq_decision_workflow_v1",
    workflow_id = workflow_id %||% aq_vnext_id("decision_workflow"),
    decision_context_id = decision_context_id,
    decision_version = decision_version,
    valuation_artifact_id = valuation_artifact_id,
    recommendation_id = recommendation_id,
    selected_alternative = selected_alternative,
    workflow_type = workflow_type,
    risk_tier = risk_tier,
    authority_tier = authority_tier,
    required_stages = aq_vnext_unique_chr(required_stages),
    current_stage = current_stage,
    stage_owners = aq_vnext_unique_chr(stage_owners),
    required_approvals = aq_vnext_unique_chr(required_approvals),
    review_deadline = review_deadline,
    conditions = aq_vnext_unique_chr(conditions),
    evidence_cutoff = evidence_cutoff,
    status = status,
    supported_actions = aq_vnext_unique_chr(supported_actions),
    created_at = aq_vnext_now()
  )
  class(out) <- c("aq_decision_workflow", "list")
  out$validation <- aq_validate_decision_workflow(out)
  out
}

#' Validate a Decision Workflow
#' @export
aq_validate_decision_workflow <- function(workflow) {
  rows <- list()
  add <- function(check, status, reason, recommendation = NA_character_) {
    rows[[length(rows) + 1L]] <<- data.table::data.table(check, status, severity = status, reason, recommendation)
  }
  if (!is.list(workflow) || is.null(workflow$workflow_id)) {
    add("workflow_object", "fail", "workflow must be created by aq_decision_workflow().", "Create a typed workflow.")
    return(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
  }
  if (!nzchar(aq_dw_chr(workflow$decision_context_id, ""))) add("decision_context_id", "fail", "decision_context_id is required.", "Link workflow to an authored decision.")
  if (!workflow$current_stage %in% aq_decision_workflow_stages()) add("current_stage", "fail", "current_stage is not part of the governed lifecycle.", "Use a supported workflow stage.")
  if (!length(workflow$required_stages)) add("required_stages", "warning", "No required stages are declared.", "Use a proportional workflow template.")
  if (!nzchar(aq_dw_chr(workflow$valuation_artifact_id, ""))) add("valuation_artifact_id", "warning", "No valuation artifact is linked.", "Link the reviewed valuation evidence package.")
  if (!nzchar(aq_dw_chr(workflow$selected_alternative, ""))) add("selected_alternative", "warning", "No selected alternative is linked.", "Preserve which alternative is being governed.")
  if (!length(rows)) add("workflow_valid", "pass", "Decision workflow is structurally valid.")
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

#' Assess Decision Review Readiness
#' @export
aq_assess_decision_review_readiness <- function(workflow, valuation = NULL, evidence_package = NULL, authority = NULL, blockers = NULL, as_of = Sys.Date()) {
  rows <- list()
  add <- function(check, status, reason, required_action = NA_character_) {
    rows[[length(rows) + 1L]] <<- data.table::data.table(check, status, reason, required_action)
  }
  validation <- aq_validate_decision_workflow(workflow)
  if (any(validation$status == "fail")) add("workflow_structure", "blocked", paste(validation$reason[validation$status == "fail"], collapse = "; "), "Repair workflow structure.")
  if (!nzchar(aq_dw_chr(workflow$decision_context_id, ""))) add("decision_context", "structurally_incomplete", "Decision context is missing.", "Author a decision context.")
  if (!nzchar(aq_dw_chr(workflow$selected_alternative, ""))) add("selected_alternative", "structurally_incomplete", "Selected alternative is missing.", "Select the alternative under review.")
  if (!nzchar(aq_dw_chr(workflow$valuation_artifact_id, "")) && is.null(valuation)) add("valuation", "valuation_stale", "No current valuation evidence is linked.", "Generate or link a valuation artifact.")
  if (isTRUE(authority$incomplete %||% FALSE) || !nzchar(aq_dw_chr(workflow$authority_tier, ""))) add("authority", "authority_incomplete", "Approval authority is incomplete.", "Define authority tier and reviewer rights.")
  if (length(blockers %||% character())) add("blockers", "blocked", paste(blockers, collapse = "; "), "Resolve blockers before review.")
  deadline <- suppressWarnings(as.Date(workflow$review_deadline))
  if (!is.na(deadline) && deadline < as.Date(as_of)) add("review_deadline", "expired", "Review deadline has passed.", "Refresh deadline or close the workflow.")
  if (!length(rows)) add("ready", "ready_for_review", "Decision package is ready for governed review.", "Freeze evidence package and request review.")
  state <- if (any(vapply(rows, function(x) x$status[[1]] %in% c("blocked", "expired"), logical(1)))) {
    "blocked"
  } else if (any(vapply(rows, function(x) x$status[[1]] %in% c("valuation_stale", "authority_incomplete", "structurally_incomplete"), logical(1)))) {
    rows[[which(vapply(rows, function(x) x$status[[1]] %in% c("valuation_stale", "authority_incomplete", "structurally_incomplete"), logical(1)))[1L]]]$status[[1]]
  } else {
    "ready_for_review"
  }
  out <- data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
  out[, readiness_state := state]
  out[]
}

#' Create a Frozen Decision Evidence Package
#' @export
aq_decision_evidence_package <- function(workflow, valuation = NULL, evidence_refs = character(), recommendation = NULL, artifact_versions = NULL, package_id = NULL) {
  package_id <- package_id %||% paste0("decision_evidence_package_", workflow$workflow_id)
  payload <- list(
    workflow_id = workflow$workflow_id,
    decision_context_id = workflow$decision_context_id,
    decision_version = workflow$decision_version,
    valuation_artifact_id = workflow$valuation_artifact_id,
    valuation_summary = valuation %||% list(),
    recommendation = recommendation %||% list(),
    evidence_refs = aq_vnext_unique_chr(evidence_refs),
    artifact_versions = artifact_versions %||% list(),
    evidence_cutoff = workflow$evidence_cutoff,
    frozen_at = aq_vnext_now()
  )
  artifact <- new_metadata_artifact(
    id = package_id,
    title = "Decision Evidence Package",
    values = payload,
    description = "Frozen version-specific evidence package for governed review and approval.",
    tags = c("decision_workflow", "evidence_package", "governed_review"),
    source_generator = "aq_decision_evidence_package",
    metadata = list(
      artifact_id = package_id,
      artifact_type = "decision_evidence_package_artifact",
      workflow_id = workflow$workflow_id,
      decision_context_id = workflow$decision_context_id,
      evidence_cutoff = workflow$evidence_cutoff,
      created_at = aq_vnext_now()
    )
  )
  aq_vnext_attach_envelope(
    artifact,
    artifact_id = package_id,
    artifact_type = "decision_evidence_package_artifact",
    artifact_version = "aq_decision_evidence_package_artifact_v1",
    parent_artifact_ids = aq_vnext_unique_chr(c(workflow$decision_context_id, workflow$valuation_artifact_id, evidence_refs)),
    lineage = list(workflow_id = workflow$workflow_id, decision_context_id = workflow$decision_context_id),
    task = "decision_workflow",
    operator = "freeze_evidence_package",
    engine = "base_r",
    specification_id = workflow$workflow_id,
    supported_actions = c("request_review", "approve", "audit"),
    producer = "aq_decision_evidence_package"
  )
}

#' Create Decision Review Requests
#' @export
aq_decision_review_request <- function(requests) {
  out <- aq_dw_dt(requests, "workflow_id")
  if (!nrow(out)) return(data.table::data.table())
  out <- aq_dw_add_missing(out, c("review_request_id", "review_type", "reviewer", "reviewer_role", "evidence_package_id", "requested_date", "due_date", "questions", "decision_rights", "required_response", "escalation_rule", "status"))
  out[, review_request_id := aq_dw_fill_chr(review_request_id, paste0("review_request_", seq_len(.N)))]
  out[, review_type := aq_dw_fill_chr(review_type, "analytical")]
  out[, status := aq_dw_fill_chr(status, "requested")]
  out[]
}

#' Record Decision Reviews
#' @export
aq_decision_review <- function(reviews) {
  out <- aq_dw_dt(reviews, "workflow_id")
  if (!nrow(out)) return(data.table::data.table())
  out <- aq_dw_add_missing(out, c("review_id", "review_request_id", "reviewer", "role", "review_date", "reviewed_decision_version", "reviewed_valuation_version", "reviewed_evidence_cutoff", "findings", "concerns", "conditions", "missing_evidence", "recommendation", "confidence", "approval_authority", "status", "expiration_date"))
  out[, review_id := aq_dw_fill_chr(review_id, paste0("review_", seq_len(.N)))]
  out[, status := aq_dw_fill_chr(status, "endorse")]
  out[, confidence := suppressWarnings(as.numeric(confidence))]
  out[]
}

#' Create Decision Approval Records
#' @export
aq_decision_approval <- function(approvals) {
  out <- aq_dw_dt(approvals, "workflow_id")
  if (!nrow(out)) return(data.table::data.table())
  out <- aq_dw_add_missing(out, c("approval_id", "approver", "authority_basis", "approved_decision_version", "approved_alternative", "approved_lever_settings", "approved_budget", "approved_timing", "conditions", "guardrails", "monitoring_requirements", "expiration", "review_date", "escalation_requirements", "override_rationale", "status", "authority_scope", "authority_domain", "authority_magnitude", "authority_risk_tier"))
  out[, approval_id := aq_dw_fill_chr(approval_id, paste0("approval_", seq_len(.N)))]
  out[, status := aq_dw_fill_chr(status, "approved")]
  out[, approved_budget := suppressWarnings(as.numeric(approved_budget))]
  out[, authority_magnitude := suppressWarnings(as.numeric(authority_magnitude))]
  out[]
}

#' Validate Decision Approval Authority
#' @export
aq_validate_decision_approval <- function(approval, workflow = NULL) {
  approval <- aq_decision_approval(approval)
  rows <- list()
  add <- function(approval_id, check, status, reason, recommendation = NA_character_) {
    rows[[length(rows) + 1L]] <<- data.table::data.table(approval_id, check, status, reason, recommendation)
  }
  if (!nrow(approval)) return(data.table::data.table())
  for (i in seq_len(nrow(approval))) {
    row <- approval[i]
    id <- row$approval_id[[1]]
    if (!nzchar(row$approver %||% "")) add(id, "approver", "fail", "Approver is missing.", "Record an accountable approver.")
    if (!nzchar(row$authority_basis %||% "")) add(id, "authority_basis", "warning", "Authority basis is not documented.", "Link authority record or policy basis.")
    if (is.finite(row$approved_budget) && is.finite(row$authority_magnitude) && row$approved_budget > row$authority_magnitude) add(id, "magnitude", "fail", "Approved budget exceeds authority magnitude.", "Escalate to a higher authority tier.")
    if (!is.null(workflow) && nzchar(workflow$selected_alternative %||% "") && nzchar(row$approved_alternative %||% "") && !identical(row$approved_alternative, workflow$selected_alternative)) add(id, "alternative", "fail", "Approved alternative differs from workflow selected alternative.", "Revise workflow or approval.")
    if ((row$status %||% "") %in% c("approved", "conditionally_approved") && !nzchar(row$approved_alternative %||% "")) add(id, "approved_alternative", "fail", "Approval does not identify an approved alternative.", "Record the approved alternative.")
  }
  if (!length(rows)) rows[[1L]] <- data.table::data.table(approval_id = NA_character_, check = "approval_valid", status = "pass", reason = "Approval is structurally valid.", recommendation = NA_character_)
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

#' Create Approval Conditions
#' @export
aq_decision_condition <- function(conditions) {
  out <- aq_dw_dt(conditions, "workflow_id")
  if (!nrow(out)) return(data.table::data.table())
  out <- aq_dw_add_missing(out, c("condition_id", "source_id", "condition_type", "description", "owner", "due_date", "evidence", "status", "breach_severity", "resolution"))
  out[, condition_id := aq_dw_fill_chr(condition_id, paste0("condition_", seq_len(.N)))]
  out[, status := aq_dw_fill_chr(status, "open")]
  out[]
}

#' Create Decision Implementation Plans
#' @export
aq_decision_implementation_plan <- function(plans) {
  out <- aq_dw_dt(plans, "workflow_id")
  if (!nrow(out)) return(data.table::data.table())
  out <- aq_dw_add_missing(out, c("implementation_id", "approved_decision", "selected_alternative", "tactics", "levers", "current_values", "approved_target_values", "implementation_sequence", "owners", "start_date", "expected_completion_date", "milestones", "dependencies", "budget", "effort", "capacity", "risk_mitigations", "guardrails", "measurement_plan", "rollback_plan", "option_exercise_points", "status"))
  out[, implementation_id := aq_dw_fill_chr(implementation_id, paste0("implementation_", seq_len(.N)))]
  out[, status := aq_dw_fill_chr(status, "planned")]
  out[, budget := suppressWarnings(as.numeric(budget))]
  out[]
}

#' Record Realized Decision Implementation
#' @export
aq_record_decision_implementation <- function(records) {
  out <- aq_dw_dt(records, "workflow_id")
  if (!nrow(out)) return(data.table::data.table())
  out <- aq_dw_add_missing(out, c("implementation_evidence_id", "implementation_id", "actual_lever_settings", "actual_timing", "actual_cost", "actual_population", "actual_geography", "actual_duration", "actual_treatment_intensity", "milestones_completed", "deviations", "failures", "delays", "operational_owner", "source", "evidence_quality"))
  out[, implementation_evidence_id := aq_dw_fill_chr(implementation_evidence_id, paste0("implementation_evidence_", seq_len(.N)))]
  out[, actual_cost := suppressWarnings(as.numeric(actual_cost))]
  out[]
}

#' Reconcile Approved and Realized Implementation
#' @export
aq_reconcile_decision_implementation <- function(plan, implementation) {
  plan <- aq_decision_implementation_plan(plan)
  implementation <- aq_record_decision_implementation(implementation)
  if (!nrow(plan) || !nrow(implementation)) return(data.table::data.table(reconciliation_state = "missing_plan_or_implementation"))
  out <- merge(implementation, plan[, .(workflow_id, implementation_id, budget, expected_completion_date, approved_target_values, guardrails)], by = c("workflow_id", "implementation_id"), all.x = TRUE, suffixes = c("_actual", "_plan"))
  out[, cost_variance := actual_cost - budget]
  out[, deviation_state := "immaterial"]
  out[is.finite(cost_variance) & abs(cost_variance) > pmax(abs(budget) * 0.1, 1, na.rm = TRUE), deviation_state := "material"]
  out[nzchar(deviations %||% "") | nzchar(failures %||% ""), deviation_state := "outcome_relevant"]
  out[grepl("authority", deviations %||% "", ignore.case = TRUE), deviation_state := "authority_breach"]
  out[grepl("guardrail", deviations %||% "", ignore.case = TRUE), deviation_state := "guardrail_breach"]
  out[, escalation_required := deviation_state %in% c("material", "outcome_relevant", "authority_breach", "guardrail_breach")]
  out[]
}

#' Create Decision Monitoring Plans
#' @export
aq_decision_monitoring_plan <- function(monitors) {
  out <- aq_dw_dt(monitors, "workflow_id")
  if (!nrow(out)) return(data.table::data.table())
  out <- aq_dw_add_missing(out, c("monitoring_id", "metric", "source", "cadence", "threshold", "owner", "escalation_rule", "missing_data_rule", "expected_maturation", "status"))
  out[, monitoring_id := aq_dw_fill_chr(monitoring_id, paste0("monitoring_", seq_len(.N)))]
  out[, status := aq_dw_fill_chr(status, "active")]
  out[, threshold := suppressWarnings(as.numeric(threshold))]
  out[]
}

#' Assess Decision Quality
#' @export
aq_assess_decision_quality <- function(workflow, readiness = NULL, reviews = NULL, approvals = NULL, reconciliation = NULL, outcomes = NULL) {
  readiness <- readiness %||% aq_assess_decision_review_readiness(workflow)
  reviews <- aq_decision_review(reviews)
  approvals <- aq_decision_approval(approvals)
  reconciliation <- data.table::as.data.table(reconciliation %||% data.table::data.table())
  score <- 100
  reasons <- character()
  if (!identical(unique(readiness$readiness_state)[1], "ready_for_review")) { score <- score - 20; reasons <- c(reasons, "readiness_gaps") }
  if (!nrow(reviews)) { score <- score - 15; reasons <- c(reasons, "missing_review") }
  if (!nrow(approvals)) { score <- score - 15; reasons <- c(reasons, "missing_approval") }
  if (nrow(reconciliation) && any(reconciliation$escalation_required %||% FALSE, na.rm = TRUE)) { score <- score - 20; reasons <- c(reasons, "implementation_deviation") }
  if (!length(reasons)) reasons <- "complete_governed_process"
  state <- if (score >= 85) "high_quality_process" else if (score >= 65) "adequate_process" else if ("implementation_deviation" %in% reasons) "implementation_limited" else if ("missing_approval" %in% reasons) "authority_limited" else "material_process_gaps"
  data.table::data.table(
    workflow_id = workflow$workflow_id,
    decision_context_id = workflow$decision_context_id,
    process_quality_score = score,
    decision_quality_state = state,
    reasons = paste(reasons, collapse = ", "),
    assessed_at = aq_vnext_now()
  )
}

#' Review Realized Decision Value
#' @export
aq_realized_value_review <- function(workflow_id, expected_value, realized_value, expected_cost = NA_real_, realized_cost = NA_real_, expected_timing = NA_real_, realized_timing = NA_real_, maturation_status = "mature", notes = NA_character_) {
  expected_value <- aq_dw_num(expected_value)
  realized_value <- aq_dw_num(realized_value)
  expected_cost <- aq_dw_num(expected_cost)
  realized_cost <- aq_dw_num(realized_cost)
  data.table::data.table(
    realized_value_review_id = aq_vnext_id("realized_value_review"),
    workflow_id,
    expected_value,
    realized_value,
    value_variance = realized_value - expected_value,
    expected_cost,
    realized_cost,
    cost_variance = realized_cost - expected_cost,
    expected_timing = aq_dw_num(expected_timing),
    realized_timing = aq_dw_num(realized_timing),
    maturation_status,
    notes,
    reviewed_at = aq_vnext_now()
  )
}

#' Generate Follow-Up Decision Candidates
#' @export
aq_decision_followup_candidates <- function(workflow, quality = NULL, realized_value = NULL, reconciliation = NULL) {
  quality <- quality %||% aq_assess_decision_quality(workflow)
  realized_value <- data.table::as.data.table(realized_value %||% data.table::data.table())
  reconciliation <- data.table::as.data.table(reconciliation %||% data.table::data.table())
  rows <- list()
  add <- function(candidate_type, reason, priority = "medium") rows[[length(rows) + 1L]] <<- data.table::data.table(workflow_id = workflow$workflow_id, candidate_id = aq_vnext_id("decision_followup"), candidate_type, reason, priority)
  if (nrow(reconciliation) && any(reconciliation$deviation_state %in% c("authority_breach", "guardrail_breach"), na.rm = TRUE)) add("escalate", "Implementation deviation breached authority or guardrail.", "high")
  if (nrow(realized_value) && is.finite(realized_value$value_variance[[1]]) && realized_value$value_variance[[1]] > 0) add("expand", "Realized value exceeded expectation.", "medium")
  if (nrow(realized_value) && is.finite(realized_value$value_variance[[1]]) && realized_value$value_variance[[1]] < 0) add("modify_or_contract", "Realized value trailed expectation.", "medium")
  if ((quality$decision_quality_state[[1]] %||% "") %in% c("high_quality_process", "adequate_process") && !length(rows)) add("retain", "Decision process and available outcomes support retaining the current action.", "low")
  if (!length(rows)) add("gather_more_evidence", "Insufficient workflow evidence for a final follow-up.", "medium")
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

#' Create Decision Workflow Campaign Seeds
#' @export
aq_decision_workflow_campaign_seeds <- function(readiness = NULL, reconciliation = NULL, realized_value = NULL, followups = NULL) {
  rows <- list()
  add <- function(seed_type, reason, reference = NA_character_) rows[[length(rows) + 1L]] <<- data.table::data.table(seed_type, reason, reference)
  readiness <- data.table::as.data.table(readiness %||% data.table::data.table())
  reconciliation <- data.table::as.data.table(reconciliation %||% data.table::data.table())
  realized_value <- data.table::as.data.table(realized_value %||% data.table::data.table())
  followups <- data.table::as.data.table(followups %||% data.table::data.table())
  if (nrow(readiness) && any(!readiness$readiness_state %in% "ready_for_review")) add("collect_missing_review_evidence", "Review readiness has unresolved gaps.", paste(unique(readiness$readiness_state), collapse = ", "))
  if (nrow(reconciliation) && any(reconciliation$escalation_required %||% FALSE, na.rm = TRUE)) add("investigate_implementation_deviation", "Realized implementation deviated materially from plan.", paste(unique(reconciliation$deviation_state), collapse = ", "))
  if (nrow(realized_value) && is.finite(realized_value$value_variance[[1]]) && abs(realized_value$value_variance[[1]]) > 0) add("investigate_valuation_miss", "Expected and realized value differ.", realized_value$realized_value_review_id[[1]])
  if (nrow(followups)) add("prepare_followup_decision", "Follow-up decision candidates exist.", paste(unique(followups$candidate_type), collapse = ", "))
  if (!length(rows)) return(data.table::data.table(seed_type = character(), reason = character(), reference = character()))
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

#' Assess Workflow Staleness
#' @export
aq_decision_workflow_staleness <- function(change_type, material = TRUE) {
  if (!isTRUE(material)) {
    action <- "no_action"
  } else if (change_type %in% c("valuation_changed", "alternative_changed", "authority_changed", "guardrail_changed")) {
    action <- "reapproval"
  } else if (change_type %in% c("causal_estimate_updated", "forecast_updated", "evidence_package_changed")) {
    action <- "partial_rereview"
  } else if (change_type %in% c("deadline_passed", "option_expired", "approval_expired")) {
    action <- "escalation"
  } else {
    action <- "reviewer_acknowledgement"
  }
  data.table::data.table(change_type, material = isTRUE(material), required_action = action)
}

#' Create a Canonical Decision Workflow Artifact
#' @export
aq_decision_workflow_artifact <- function(workflow, readiness = NULL, reviews = NULL, approvals = NULL, implementation_plan = NULL, reconciliation = NULL, monitoring = NULL, quality = NULL, realized_value = NULL, followups = NULL, artifact_id = NULL) {
  artifact_id <- artifact_id %||% paste0("decision_workflow_artifact_", workflow$workflow_id)
  payload <- list(
    workflow = workflow,
    readiness = readiness %||% aq_assess_decision_review_readiness(workflow),
    reviews = aq_decision_review(reviews),
    approvals = aq_decision_approval(approvals),
    implementation_plan = aq_decision_implementation_plan(implementation_plan),
    reconciliation = data.table::as.data.table(reconciliation %||% data.table::data.table()),
    monitoring = aq_decision_monitoring_plan(monitoring),
    quality = quality %||% aq_assess_decision_quality(workflow),
    realized_value = data.table::as.data.table(realized_value %||% data.table::data.table()),
    followups = data.table::as.data.table(followups %||% data.table::data.table())
  )
  artifact <- new_metadata_artifact(
    id = artifact_id,
    title = "Decision Workflow",
    values = payload,
    description = "Governed review, approval, implementation, monitoring, realized value, and follow-through evidence.",
    tags = c("decision_workflow", "governance", "implementation_tracking"),
    source_generator = "aq_decision_workflow_artifact",
    metadata = list(
      artifact_id = artifact_id,
      artifact_type = "decision_workflow_artifact",
      workflow_id = workflow$workflow_id,
      decision_context_id = workflow$decision_context_id,
      current_stage = workflow$current_stage,
      created_at = aq_vnext_now()
    )
  )
  aq_vnext_attach_envelope(
    artifact,
    artifact_id = artifact_id,
    artifact_type = "decision_workflow_artifact",
    artifact_version = "aq_decision_workflow_artifact_v1",
    parent_artifact_ids = aq_vnext_unique_chr(c(workflow$decision_context_id, workflow$valuation_artifact_id)),
    lineage = list(workflow_id = workflow$workflow_id, decision_context_id = workflow$decision_context_id),
    task = "decision_workflow",
    operator = "deterministic_governed_decision_workflow",
    engine = "base_r",
    specification_id = workflow$workflow_id,
    supported_actions = c("audit", "review", "monitor", "calibrate", "generate_followup_candidate"),
    producer = "aq_decision_workflow_artifact"
  )
}

#' QA for Decision Workflow Intelligence
#' @export
qa_decision_workflow_framework <- function() {
  rows <- list()
  add <- function(check, ok, message) rows[[length(rows) + 1L]] <<- data.table::data.table(suite = "decision_workflow_framework", check, status = if (isTRUE(ok)) "pass" else "fail", message)
  workflow <- aq_decision_workflow(
    workflow_id = "workflow_1",
    decision_context_id = "decision_1",
    decision_version = "v1",
    valuation_artifact_id = "valuation_artifact_1",
    recommendation_id = "rec_1",
    selected_alternative = "pilot",
    workflow_type = "pilot_approval",
    authority_tier = "manager",
    review_deadline = as.character(Sys.Date() + 7)
  )
  add("workflow_validates", any(workflow$validation$status == "pass"), "Workflow contract validates.")
  add("transition_validation", aq_decision_workflow_transition_allowed("draft", "ready_for_review") && !aq_decision_workflow_transition_allowed("draft", "implemented"), "State transitions are deterministic.")
  readiness <- aq_assess_decision_review_readiness(workflow)
  add("readiness_ready", unique(readiness$readiness_state)[1] == "ready_for_review", "Review readiness can be assessed.")
  package <- aq_decision_evidence_package(workflow, evidence_refs = c("valuation_artifact_1", "itt_artifact_1"))
  add("evidence_package_artifact", identical(package$artifact_envelope$artifact_type, "decision_evidence_package_artifact"), "Frozen evidence package is reconstructable.")
  request <- aq_decision_review_request(list(workflow_id = workflow$workflow_id, review_type = "financial", reviewer_role = "finance"))
  review <- aq_decision_review(list(workflow_id = workflow$workflow_id, review_request_id = request$review_request_id[[1]], reviewer = "finance_owner", role = "financial", status = "endorse_with_conditions", conditions = "monitor spend"))
  add("reviews_distinct", all(c("review_type", "review_request_id") %in% names(request)) && review$status[[1]] == "endorse_with_conditions", "Review request and review records remain distinct.")
  approval <- aq_decision_approval(list(workflow_id = workflow$workflow_id, approver = "manager", authority_basis = "authority_1", approved_alternative = "pilot", approved_budget = 100, authority_magnitude = 200, status = "conditionally_approved", conditions = "monitor spend"))
  approval_validation <- aq_validate_decision_approval(approval, workflow)
  add("approval_authority", !any(approval_validation$status == "fail"), "Approval authority validates by explicit scope fields.")
  condition <- aq_decision_condition(list(workflow_id = workflow$workflow_id, source_id = approval$approval_id[[1]], condition_type = "monitoring", description = "Monitor spend", owner = "analyst", status = "open"))
  add("conditions_persist", nrow(condition) == 1L && condition$status[[1]] == "open", "Approval conditions remain first-class.")
  plan <- aq_decision_implementation_plan(list(workflow_id = workflow$workflow_id, implementation_id = "impl_1", approved_decision = approval$approval_id[[1]], selected_alternative = "pilot", budget = 100, approved_target_values = "lever=110", expected_completion_date = as.character(Sys.Date() + 14), rollback_plan = "return to baseline"))
  actual <- aq_record_decision_implementation(list(workflow_id = workflow$workflow_id, implementation_id = "impl_1", actual_cost = 130, deviations = "minor delay", operational_owner = "ops", evidence_quality = "observed"))
  reconciliation <- aq_reconcile_decision_implementation(plan, actual)
  add("implementation_separate", nrow(reconciliation) == 1L && "deviation_state" %in% names(reconciliation), "Realized implementation is reconciled separately from approval.")
  monitoring <- aq_decision_monitoring_plan(list(workflow_id = workflow$workflow_id, metric = "revenue", cadence = "weekly", threshold = 0, owner = "analyst", escalation_rule = "notify owner"))
  add("monitoring_attached", nrow(monitoring) == 1L && monitoring$status[[1]] == "active", "Monitoring remains attached to the decision.")
  quality <- aq_assess_decision_quality(workflow, readiness, review, approval, reconciliation)
  add("decision_quality_process", quality$decision_quality_state[[1]] %in% c("high_quality_process", "adequate_process", "implementation_limited", "material_process_gaps"), "Decision quality is process-oriented.")
  value <- aq_realized_value_review(workflow$workflow_id, expected_value = 200, realized_value = 150, expected_cost = 100, realized_cost = 130)
  add("realized_value_compares", is.finite(value$value_variance[[1]]) && is.finite(value$cost_variance[[1]]), "Expected and realized value are compared.")
  followups <- aq_decision_followup_candidates(workflow, quality, value, reconciliation)
  add("followup_candidates", nrow(followups) > 0L, "Follow-up decisions are proposed without action.")
  stale <- aq_decision_workflow_staleness("valuation_changed", material = TRUE)
  add("staleness_proportional", stale$required_action[[1]] == "reapproval", "Material changes stale downstream stages proportionally.")
  seeds <- aq_decision_workflow_campaign_seeds(readiness, reconciliation, value, followups)
  add("campaign_seeds", data.table::is.data.table(seeds), "Workflow produces campaign seed candidates.")
  artifact <- aq_decision_workflow_artifact(workflow, readiness, review, approval, plan, reconciliation, monitoring, quality, value, followups)
  add("workflow_artifact", identical(artifact$artifact_envelope$artifact_type, "decision_workflow_artifact"), "Workflow registers as canonical artifact.")
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}
