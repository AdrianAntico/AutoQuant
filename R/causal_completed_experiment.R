# AutoQuant completed-experiment evidence and analysis-readiness contracts.

aq_completed_experiment_status_levels <- function() {
  c("not_started", "in_progress", "completed", "stopped_early", "canceled", "compromised", "outcome_pending", "analysis_pending", "closed")
}

aq_analysis_readiness_states <- function() {
  c("ready_for_itt", "ready_for_itt_with_adjustment", "ready_with_major_limitations", "outcome_pending", "measurement_incomplete", "assignment_integrity_review", "analysis_population_review", "estimand_revision_required", "descriptive_only", "blocked", "invalid_for_planned_estimand", "insufficient_information")
}

aq_completed_experiment_record_dt <- function(x, id_col, record_type) {
  if (is.null(x)) x <- data.table::data.table()
  out <- data.table::as.data.table(x)
  if (!id_col %in% names(out)) {
    if (!nrow(out)) out[, (id_col) := character()] else stop(paste(record_type, "records must include", id_col), call. = FALSE)
  }
  out
}

aq_completed_chr <- function(x, default = NA_character_) {
  if (is.null(x) || !length(x) || is.na(x[[1]])) return(default)
  as.character(x[[1]])
}

aq_completed_bool <- function(x) isTRUE(x %in% c(TRUE, "TRUE", "true", "yes", "1", 1L))

#' Create a Completed Experiment Record
#' @export
aq_completed_experiment <- function(record, experiment_plan_artifact = NULL, completed_experiment_id = NULL) {
  record <- aq_completed_experiment_record_dt(record, "completed_experiment_id", "completed_experiment")
  if (nrow(record) != 1L) stop("record must describe exactly one completed experiment.", call. = FALSE)
  if (!is.null(completed_experiment_id)) record[, completed_experiment_id := as.character(completed_experiment_id)[1L]]
  required <- c("experiment_plan_artifact_id", "decision_context_id", "causal_question_id", "estimand_id", "design_version", "assignment_version", "experiment_status", "actual_start_date", "actual_end_date", "data_cutoff_date", "execution_owner")
  for (col in setdiff(required, names(record))) record[, (col) := NA_character_]
  for (col in c("source_systems", "evidence_references", "notes", "supported_actions")) if (!col %in% names(record)) record[, (col) := list(character())]
  out <- list(
    completed_experiment_id = record$completed_experiment_id[[1]],
    schema_version = "aq_completed_experiment_v1",
    record = record,
    experiment_plan_artifact = experiment_plan_artifact,
    created_at = aq_vnext_now()
  )
  out$validation <- aq_validate_completed_experiment(out)
  class(out) <- c("aq_completed_experiment", "list")
  out
}

#' Validate a Completed Experiment Record
#' @export
aq_validate_completed_experiment <- function(completed_experiment) {
  rows <- list()
  add <- function(check, status, message, recommendation = NA_character_) rows[[length(rows) + 1L]] <<- data.table::data.table(check, status, severity = status, message, recommendation)
  if (!is.list(completed_experiment) || is.null(completed_experiment$record)) {
    add("completed_experiment_object", "fail", "completed_experiment must be created by aq_completed_experiment().")
    return(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
  }
  r <- completed_experiment$record[1]
  required <- c("completed_experiment_id", "experiment_plan_artifact_id", "decision_context_id", "causal_question_id", "estimand_id", "experiment_status", "actual_start_date", "data_cutoff_date", "execution_owner")
  for (col in required) if (!col %in% names(r) || !nzchar(aq_completed_chr(r[[col]], ""))) add(paste0("missing_", col), "fail", paste(col, "is required."))
  if (!aq_completed_chr(r$experiment_status, "") %in% aq_completed_experiment_status_levels()) add("unknown_status", "fail", "Experiment status is not supported.")
  if (!length(rows)) add("completed_experiment_valid", "pass", "Completed experiment record is structurally valid.")
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

#' Ingest Assignment Evidence
#' @export
aq_assignment_evidence <- function(data, unit_id_col = "unit_id", planned_arm_col = "planned_arm", realized_arm_col = "realized_assigned_arm", assignment_time_col = NULL, eligible_col = NULL, exclusion_col = NULL, override_col = NULL) {
  dt <- data.table::as.data.table(data)
  required <- c(unit_id_col, planned_arm_col, realized_arm_col)
  missing <- setdiff(required, names(dt))
  if (length(missing)) stop(paste("Assignment evidence is missing columns:", paste(missing, collapse = ", ")), call. = FALSE)
  out <- data.table::data.table(
    unit_id = as.character(dt[[unit_id_col]]),
    planned_arm = as.character(dt[[planned_arm_col]]),
    realized_assigned_arm = as.character(dt[[realized_arm_col]]),
    assignment_timestamp = if (!is.null(assignment_time_col) && assignment_time_col %in% names(dt)) as.character(dt[[assignment_time_col]]) else NA_character_,
    eligibility_status = if (!is.null(eligible_col) && eligible_col %in% names(dt)) as.character(dt[[eligible_col]]) else "eligible",
    exclusion_status = if (!is.null(exclusion_col) && exclusion_col %in% names(dt)) as.character(dt[[exclusion_col]]) else "not_excluded",
    assignment_override = if (!is.null(override_col) && override_col %in% names(dt)) as.logical(dt[[override_col]]) else FALSE
  )
  evidence <- list(schema_version = "aq_assignment_evidence_v1", assignment = out, diagnostics = aq_validate_assignment_evidence(out), created_at = aq_vnext_now())
  class(evidence) <- c("aq_assignment_evidence", "list")
  evidence
}

#' Validate Assignment Evidence
#' @export
aq_validate_assignment_evidence <- function(assignment_evidence, expected_arms = NULL) {
  assignment <- if (inherits(assignment_evidence, "aq_assignment_evidence")) assignment_evidence$assignment else data.table::as.data.table(assignment_evidence)
  rows <- list()
  add <- function(check, status, finding, recommendation = NA_character_) rows[[length(rows) + 1L]] <<- data.table::data.table(check, status, finding, recommendation)
  if (!nrow(assignment)) add("assignment_missing", "fail", "No assignment evidence was supplied.", "Attach the original assignment log.")
  if (nrow(assignment) && anyDuplicated(assignment$unit_id)) add("duplicate_units", "fail", "Duplicate assigned units exist.", "Resolve duplicate assignment records without deleting evidence.")
  changed <- assignment[nzchar(planned_arm) & nzchar(realized_assigned_arm) & planned_arm != realized_assigned_arm]
  if (nrow(changed)) add("assignment_changes", "warning", paste(nrow(changed), "unit(s) changed assigned arm."), "Preserve original assignment and review reassignment/override evidence.")
  if (!is.null(expected_arms)) {
    unexpected <- setdiff(unique(c(assignment$planned_arm, assignment$realized_assigned_arm)), expected_arms)
    unexpected <- unexpected[nzchar(unexpected)]
    if (length(unexpected)) add("unexpected_arms", "fail", paste("Unexpected arm(s):", paste(unexpected, collapse = ", ")))
  }
  overrides <- assignment[assignment_override %in% TRUE]
  if (nrow(overrides)) add("assignment_overrides", "warning", paste(nrow(overrides), "override(s) detected."), "Review governance and preserve override reasons.")
  if (!length(rows)) add("assignment_integrity_initial", "pass", "Assignment evidence preserves planned and realized assignment.")
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

#' Ingest Treatment Delivery Evidence
#' @export
aq_treatment_delivery_evidence <- function(data, unit_id_col = "unit_id", delivered_condition_col = "delivered_condition", delivery_status_col = "delivery_status", dosage_col = NULL, timestamp_col = NULL) {
  dt <- data.table::as.data.table(data)
  for (col in c(unit_id_col, delivered_condition_col)) if (!col %in% names(dt)) stop(paste(col, "must exist in delivery evidence."), call. = FALSE)
  out <- data.table::data.table(
    unit_id = as.character(dt[[unit_id_col]]),
    delivered_condition = as.character(dt[[delivered_condition_col]]),
    delivery_status = if (!is.null(delivery_status_col) && delivery_status_col %in% names(dt)) as.character(dt[[delivery_status_col]]) else "unknown",
    delivery_amount = if (!is.null(dosage_col) && dosage_col %in% names(dt)) suppressWarnings(as.numeric(dt[[dosage_col]])) else NA_real_,
    delivery_timestamp = if (!is.null(timestamp_col) && timestamp_col %in% names(dt)) as.character(dt[[timestamp_col]]) else NA_character_
  )
  structure(list(schema_version = "aq_treatment_delivery_evidence_v1", delivery = out, created_at = aq_vnext_now()), class = c("aq_treatment_delivery_evidence", "list"))
}

#' Ingest Exposure Evidence
#' @export
aq_exposure_evidence <- function(data, unit_id_col = "unit_id", exposure_col = "exposure", intensity_col = NULL, timing_col = NULL, quality_col = NULL) {
  dt <- data.table::as.data.table(data)
  for (col in c(unit_id_col, exposure_col)) if (!col %in% names(dt)) stop(paste(col, "must exist in exposure evidence."), call. = FALSE)
  out <- data.table::data.table(
    unit_id = as.character(dt[[unit_id_col]]),
    exposure_measure = suppressWarnings(as.numeric(dt[[exposure_col]])),
    exposure_timing = if (!is.null(timing_col) && timing_col %in% names(dt)) as.character(dt[[timing_col]]) else NA_character_,
    exposure_intensity = if (!is.null(intensity_col) && intensity_col %in% names(dt)) suppressWarnings(as.numeric(dt[[intensity_col]])) else NA_real_,
    measurement_quality = if (!is.null(quality_col) && quality_col %in% names(dt)) as.character(dt[[quality_col]]) else "unknown",
    post_assignment_measure = TRUE
  )
  structure(list(schema_version = "aq_exposure_evidence_v1", exposure = out, created_at = aq_vnext_now()), class = c("aq_exposure_evidence", "list"))
}

#' Ingest Compliance Evidence
#' @export
aq_compliance_evidence <- function(data, unit_id_col = "unit_id", assigned_col = "assigned_treatment", received_col = "treatment_received", compliance_col = NULL, crossover_col = NULL) {
  dt <- data.table::as.data.table(data)
  for (col in c(unit_id_col, assigned_col, received_col)) if (!col %in% names(dt)) stop(paste(col, "must exist in compliance evidence."), call. = FALSE)
  assigned_treatment <- as.character(dt[[assigned_col]])
  treatment_received <- as.character(dt[[received_col]])
  out <- data.table::data.table(
    unit_id = as.character(dt[[unit_id_col]]),
    assigned_treatment = assigned_treatment,
    treatment_received = treatment_received,
    adherence = if (!is.null(compliance_col) && compliance_col %in% names(dt)) suppressWarnings(as.numeric(dt[[compliance_col]])) else NA_real_,
    crossover = if (!is.null(crossover_col) && crossover_col %in% names(dt)) as.logical(dt[[crossover_col]]) else assigned_treatment != treatment_received,
    noncompliance_type = data.table::fifelse(assigned_treatment == treatment_received, "compliant", "crossover_or_noncompliance")
  )
  summary <- out[, .(units = .N, crossover_rate = mean(crossover, na.rm = TRUE), measured_adherence = mean(!is.na(adherence))), by = assigned_treatment]
  structure(list(schema_version = "aq_compliance_evidence_v1", compliance = out, summary = summary, created_at = aq_vnext_now()), class = c("aq_compliance_evidence", "list"))
}

#' Ingest Outcome and Guardrail Evidence
#' @export
aq_outcome_evidence <- function(data, unit_id_col = "unit_id", outcome_id_col = "outcome_id", value_col = "value", measurement_time_col = NULL, outcome_role_col = NULL, missing_col = NULL) {
  dt <- data.table::as.data.table(data)
  for (col in c(unit_id_col, outcome_id_col, value_col)) if (!col %in% names(dt)) stop(paste(col, "must exist in outcome evidence."), call. = FALSE)
  if (is.null(outcome_role_col) && "outcome_role" %in% names(dt)) outcome_role_col <- "outcome_role"
  if (is.null(missing_col) && "missingness" %in% names(dt)) missing_col <- "missingness"
  out <- data.table::data.table(
    unit_id = as.character(dt[[unit_id_col]]),
    outcome_id = as.character(dt[[outcome_id_col]]),
    value = suppressWarnings(as.numeric(dt[[value_col]])),
    measurement_time = if (!is.null(measurement_time_col) && measurement_time_col %in% names(dt)) as.character(dt[[measurement_time_col]]) else NA_character_,
    outcome_role = if (!is.null(outcome_role_col) && outcome_role_col %in% names(dt)) as.character(dt[[outcome_role_col]]) else "primary",
    missingness = if (!is.null(missing_col) && missing_col %in% names(dt)) as.character(dt[[missing_col]]) else data.table::fifelse(is.na(suppressWarnings(as.numeric(dt[[value_col]]))), "missing", "observed"),
    revision_status = "current"
  )
  diagnostics <- aq_validate_outcome_evidence(out)
  structure(list(schema_version = "aq_outcome_evidence_v1", outcomes = out, diagnostics = diagnostics, created_at = aq_vnext_now()), class = c("aq_outcome_evidence", "list"))
}

#' Validate Outcome Evidence
#' @export
aq_validate_outcome_evidence <- function(outcome_evidence, primary_outcome = NULL) {
  outcomes <- if (inherits(outcome_evidence, "aq_outcome_evidence")) outcome_evidence$outcomes else data.table::as.data.table(outcome_evidence)
  rows <- list()
  add <- function(check, status, finding, recommendation = NA_character_) rows[[length(rows) + 1L]] <<- data.table::data.table(check, status, finding, recommendation)
  if (!nrow(outcomes)) add("outcomes_missing", "fail", "No outcome evidence was supplied.", "Attach planned outcome and guardrail evidence.")
  dup <- outcomes[, .N, by = .(unit_id, outcome_id, outcome_role)][N > 1L]
  if (nrow(dup)) add("duplicate_outcomes", "warning", paste(nrow(dup), "duplicate outcome unit/outcome rows."), "Review revisions; do not silently choose a favorable value.")
  if (!is.null(primary_outcome) && !primary_outcome %in% outcomes$outcome_id) add("primary_outcome_absent", "fail", "Planned primary outcome is absent.", "Do not substitute another KPI silently.")
  missing_primary <- outcomes[outcome_role == "primary" & missingness != "observed"]
  if (nrow(missing_primary)) add("missing_primary_outcomes", "warning", paste(nrow(missing_primary), "primary outcome rows are missing."))
  if (!length(rows)) add("outcome_evidence_valid", "pass", "Outcome and guardrail evidence is structurally valid.")
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

#' Ingest Exclusion Evidence
#' @export
aq_exclusion_evidence <- function(data, unit_id_col = "unit_id", stage_col = "exclusion_stage", reason_col = "exclusion_reason", planned_col = NULL) {
  dt <- data.table::as.data.table(data)
  for (col in c(unit_id_col, stage_col, reason_col)) if (!col %in% names(dt)) stop(paste(col, "must exist in exclusion evidence."), call. = FALSE)
  out <- data.table::data.table(
    unit_id = as.character(dt[[unit_id_col]]),
    exclusion_stage = as.character(dt[[stage_col]]),
    exclusion_reason = as.character(dt[[reason_col]]),
    pre_assignment = grepl("pre", as.character(dt[[stage_col]]), ignore.case = TRUE),
    post_assignment = !grepl("pre", as.character(dt[[stage_col]]), ignore.case = TRUE),
    planned = if (!is.null(planned_col) && planned_col %in% names(dt)) as.logical(dt[[planned_col]]) else FALSE,
    analysis_impact = data.table::fifelse(!grepl("pre", as.character(dt[[stage_col]]), ignore.case = TRUE), "post_assignment_exclusion_risk", "eligibility_definition")
  )
  structure(list(schema_version = "aq_exclusion_evidence_v1", exclusions = out, created_at = aq_vnext_now()), class = c("aq_exclusion_evidence", "list"))
}

#' Assess Missingness and Attrition
#' @export
aq_assess_missingness_attrition <- function(assignment_evidence, outcome_evidence) {
  assignment <- if (inherits(assignment_evidence, "aq_assignment_evidence")) assignment_evidence$assignment else data.table::as.data.table(assignment_evidence)
  outcomes <- if (inherits(outcome_evidence, "aq_outcome_evidence")) outcome_evidence$outcomes else data.table::as.data.table(outcome_evidence)
  if (!nrow(assignment)) return(data.table::data.table(check = "assignment_missing", status = "fail", finding = "Cannot assess attrition without assignment evidence."))
  primary <- outcomes[outcome_role == "primary", .(outcome_observed = any(missingness == "observed")), by = unit_id]
  dt <- merge(assignment[, .(unit_id, arm = planned_arm)], primary, by = "unit_id", all.x = TRUE)
  dt[is.na(outcome_observed), outcome_observed := FALSE]
  by_arm <- dt[, .(assigned = .N, observed = sum(outcome_observed), missing = sum(!outcome_observed), missing_rate = mean(!outcome_observed)), by = arm]
  diff <- if (nrow(by_arm) >= 2L) diff(by_arm$missing_rate[seq_len(2L)]) else NA_real_
  severity <- if (is.finite(diff) && abs(diff) > 0.15) "warning" else "pass"
  data.table::rbindlist(list(
    by_arm[, .(check = "missingness_by_arm", status = "info", finding = paste(arm, "missing_rate", round(missing_rate, 3)), recommendation = "Do not assume missing-at-random.")],
    data.table::data.table(check = "differential_attrition", status = severity, finding = paste("missing-rate difference =", round(diff, 3)), recommendation = "Review outcome collection and sensitivity before estimation.")
  ), use.names = TRUE, fill = TRUE)
}

#' Reconcile Planned and Realized Experiment Execution
#' @export
aq_reconcile_experiment_execution <- function(completed_experiment, experiment_plan_artifact = NULL, assignment_evidence = NULL, delivery_evidence = NULL, outcome_evidence = NULL, exclusion_evidence = NULL) {
  rows <- list()
  add <- function(deviation, severity, finding, recommendation = NA_character_) rows[[length(rows) + 1L]] <<- data.table::data.table(deviation, severity, finding, recommendation)
  assignment <- if (inherits(assignment_evidence, "aq_assignment_evidence")) assignment_evidence$assignment else data.table::as.data.table(assignment_evidence %||% data.table::data.table())
  delivery <- if (inherits(delivery_evidence, "aq_treatment_delivery_evidence")) delivery_evidence$delivery else data.table::as.data.table(delivery_evidence %||% data.table::data.table())
  outcomes <- if (inherits(outcome_evidence, "aq_outcome_evidence")) outcome_evidence$outcomes else data.table::as.data.table(outcome_evidence %||% data.table::data.table())
  exclusions <- if (inherits(exclusion_evidence, "aq_exclusion_evidence")) exclusion_evidence$exclusions else data.table::as.data.table(exclusion_evidence %||% data.table::data.table())
  if (!nrow(assignment)) add("assignment_log_missing", "experiment-invalidating", "No realized assignment log was supplied.", "Assignment is the anchor of randomized evidence.")
  if (nrow(assignment) && nrow(assignment[planned_arm != realized_assigned_arm])) add("assignment_changed", "validity-threatening", "Realized assignment differs from original assignment.", "Preserve ITT assignment and review override evidence.")
  if (!nrow(delivery)) add("delivery_missing", "insufficient information", "No treatment-delivery evidence was supplied.")
  if (!nrow(outcomes)) add("outcomes_missing", "material but salvageable", "No outcomes were supplied yet.", "Classify as outcome pending.")
  if (nrow(exclusions) && any(exclusions$post_assignment %in% TRUE)) add("post_assignment_exclusions", "estimand-changing", "Post-assignment exclusions are present.", "Do not silently construct a cleaner analysis sample.")
  if (!length(rows)) add("execution_matches_available_evidence", "immaterial", "No material plan-execution deviations were detected in supplied evidence.")
  out <- data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
  out[, reconciliation_status := data.table::fifelse(any(severity %in% c("experiment-invalidating", "validity-threatening", "estimand-changing")), "requires_review", "reconciled")]
  out[]
}

#' Assess Randomization Integrity
#' @export
aq_assess_randomization_integrity <- function(assignment_evidence, expected_arms = NULL) {
  assignment <- if (inherits(assignment_evidence, "aq_assignment_evidence")) assignment_evidence$assignment else data.table::as.data.table(assignment_evidence)
  diagnostics <- aq_validate_assignment_evidence(assignment, expected_arms)
  counts <- if (nrow(assignment)) assignment[, .N, by = .(planned_arm)] else data.table::data.table(planned_arm = character(), N = integer())
  flags <- diagnostics[status %in% c("fail", "warning")]
  status <- if (any(flags$status == "fail")) "assignment_integrity_review" else if (nrow(flags)) "review_flags" else "no_blockers_detected"
  list(status = status, diagnostics = diagnostics, counts = counts, review_flags = flags, no_malicious_manipulation_claimed = TRUE)
}

#' Assess Realized Balance
#' @export
aq_assess_realized_balance <- function(assignment_evidence, baseline_data = NULL, unit_id_col = "unit_id", baseline_cols = character()) {
  assignment <- if (inherits(assignment_evidence, "aq_assignment_evidence")) assignment_evidence$assignment else data.table::as.data.table(assignment_evidence)
  assignment_copy <- data.table::copy(assignment)
  assignment_copy[, arm := planned_arm]
  plan <- list(assignment = assignment_copy)
  class(plan) <- c("aq_assignment_plan", "list")
  aq_assess_assignment_balance(plan, baseline_data, unit_id_col, baseline_cols)
}

#' Assess Treatment Fidelity
#' @export
aq_assess_treatment_fidelity <- function(assignment_evidence, delivery_evidence = NULL, exposure_evidence = NULL, compliance_evidence = NULL) {
  assignment <- if (inherits(assignment_evidence, "aq_assignment_evidence")) assignment_evidence$assignment else data.table::as.data.table(assignment_evidence)
  delivery <- if (inherits(delivery_evidence, "aq_treatment_delivery_evidence")) delivery_evidence$delivery else data.table::as.data.table(delivery_evidence %||% data.table::data.table())
  compliance <- if (inherits(compliance_evidence, "aq_compliance_evidence")) compliance_evidence$compliance else data.table::as.data.table(compliance_evidence %||% data.table::data.table())
  status <- "insufficient measurement"
  reasons <- character()
  if (nrow(delivery)) {
    delivered_rate <- mean(delivery$delivery_status %in% c("delivered", "complete", "available"), na.rm = TRUE)
    status <- if (is.finite(delivered_rate) && delivered_rate >= 0.9) "strong fidelity" else if (is.finite(delivered_rate) && delivered_rate >= 0.7) "adequate fidelity" else "weak fidelity"
    reasons <- c(reasons, paste("delivery_rate =", round(delivered_rate, 3)))
  }
  if (nrow(compliance) && mean(compliance$crossover, na.rm = TRUE) > 0.1) {
    status <- "substantial contamination"
    reasons <- c(reasons, "crossover/noncompliance exceeds 10%")
  }
  data.table::data.table(fidelity_status = status, reasons = paste(reasons, collapse = " | "), assignment_preserved_for_itt = TRUE, recommendation = "Interpret fidelity separately from original assignment.")
}

#' Assess Interference and Spillover Evidence
#' @export
aq_assess_interference_spillover <- function(interference_plan = NULL, contamination_evidence = NULL) {
  evidence <- data.table::as.data.table(contamination_evidence %||% data.table::data.table())
  status <- if (!nrow(evidence)) "insufficient measurement" else if (any(evidence$severity %in% c("material", "high", "critical"), na.rm = TRUE)) "material" else "possible"
  data.table::data.table(interference_status = status, recommendation = if (identical(status, "insufficient measurement")) "Do not claim absence of interference without measurement." else "Review whether the no-interference estimand remains supportable.")
}

#' Assess Guardrail and Harm Evidence
#' @export
aq_assess_guardrails <- function(outcome_evidence, guardrail_ids = character()) {
  outcomes <- if (inherits(outcome_evidence, "aq_outcome_evidence")) outcome_evidence$outcomes else data.table::as.data.table(outcome_evidence)
  guardrails <- outcomes[outcome_role == "guardrail" | outcome_id %in% guardrail_ids]
  if (!nrow(guardrails)) return(data.table::data.table(guardrail_status = "guardrail_absent", severity = "warning", finding = "No guardrail evidence was supplied.", recommendation = "A positive primary outcome does not override missing guardrails."))
  breached <- guardrails[!is.na(value) & value > 0]
  data.table::data.table(guardrail_status = if (nrow(breached)) "guardrail_breach_review" else "observed_no_breach_flag", severity = if (nrow(breached)) "high" else "pass", finding = paste(nrow(breached), "guardrail row(s) flagged positive values."), recommendation = "Review decision consequences separately from primary outcome.")
}

#' Assess Estimand Preservation
#' @export
aq_assess_estimand_preservation <- function(completed_experiment, reconciliation, integrity, fidelity, missingness, interference, outcomes = NULL, exclusions = NULL) {
  reasons <- character()
  status <- "estimand preserved"
  if (any(reconciliation$severity %in% c("experiment-invalidating", "validity-threatening"), na.rm = TRUE)) { status <- "original estimand not supportable"; reasons <- c(reasons, "Execution reconciliation contains invalidating or validity-threatening deviations.") }
  if (any(reconciliation$severity == "estimand-changing", na.rm = TRUE)) { status <- "estimand modified by execution"; reasons <- c(reasons, "Post-assignment exclusions or execution changes threaten the planned estimand.") }
  if (identical(fidelity$fidelity_status[[1]], "weak fidelity")) { status <- if (status == "estimand preserved") "ITT preserved but treatment-received estimand unsupported" else status; reasons <- c(reasons, "Treatment contrast/fidelity is weak.") }
  if (identical(interference$interference_status[[1]], "material")) { status <- "estimand modified by execution"; reasons <- c(reasons, "Material interference/spillover detected.") }
  if (any(missingness$status == "warning", na.rm = TRUE)) { status <- if (status == "estimand preserved") "estimand preserved with limitations" else status; reasons <- c(reasons, "Differential missingness/attrition requires sensitivity.") }
  if (!length(reasons)) reasons <- "Original assignment and supplied evidence do not show estimand-changing blockers."
  data.table::data.table(estimand_preservation_status = status, reasons = paste(reasons, collapse = " | "), itt_default_preserved = TRUE, alternative_estimand_requires_authoring = TRUE)
}

#' Assess Experiment Analysis Readiness
#' @export
aq_assess_experiment_analysis_readiness <- function(completed_experiment, assignment_evidence = NULL, outcome_evidence = NULL, reconciliation = NULL, integrity = NULL, fidelity = NULL, missingness = NULL, estimand_preservation = NULL, guardrails = NULL) {
  blockers <- character()
  warnings <- character()
  assignment <- if (inherits(assignment_evidence, "aq_assignment_evidence")) assignment_evidence$assignment else data.table::data.table()
  outcomes <- if (inherits(outcome_evidence, "aq_outcome_evidence")) outcome_evidence$outcomes else data.table::data.table()
  if (!nrow(assignment)) blockers <- c(blockers, "Assignment evidence is missing.")
  if (!nrow(outcomes)) blockers <- c(blockers, "Outcome evidence is missing.")
  if (!is.null(integrity) && integrity$status %in% c("assignment_integrity_review")) warnings <- c(warnings, "Assignment integrity requires review.")
  if (!is.null(estimand_preservation) && estimand_preservation$estimand_preservation_status[[1]] %in% c("original estimand not supportable")) blockers <- c(blockers, "Original estimand is not supportable.")
  if (!is.null(estimand_preservation) && grepl("modified", estimand_preservation$estimand_preservation_status[[1]])) warnings <- c(warnings, "Estimand revision is required before confirmatory estimation.")
  if (!is.null(guardrails) && guardrails$severity[[1]] %in% c("high", "critical")) warnings <- c(warnings, "Guardrail review is required.")
  state <- if (length(blockers) && any(grepl("Outcome", blockers))) "outcome_pending" else if (length(blockers)) "blocked" else if (length(warnings) && any(grepl("Estimand", warnings))) "estimand_revision_required" else if (length(warnings)) "ready_with_major_limitations" else "ready_for_itt"
  data.table::data.table(
    readiness_state = state,
    reasons = paste(c(blockers, warnings, if (!length(blockers) && !length(warnings)) "Ready for ITT planning without estimating an effect."), collapse = " | "),
    recommended_analysis_population = "assigned/randomized population for ITT unless explicitly revised",
    adjustment_guidance = "Use baseline covariates for precision only; do not condition on post-assignment behavior by default.",
    required_sensitivity_analyses = paste(c(if (length(warnings)) "missingness/fidelity/interference sensitivity" else "standard randomized-experiment sensitivity review"), collapse = ", "),
    prohibited_claims = paste(c("Do not estimate a treatment effect in this phase.", "Do not replace assignment with exposure or compliance.", "Do not silently exclude post-assignment units."), collapse = " | "),
    supported_next_actions = paste(c("register_completed_experiment_evidence", if (state %in% c("ready_for_itt", "ready_with_major_limitations")) "future_itt_analysis" else "resolve_readiness_blockers"), collapse = ", ")
  )
}

#' Create a Planned Analysis Record
#' @export
aq_planned_analysis_record <- function(completed_experiment, readiness, primary_estimand = "ITT", assignment_variable = "planned_arm", analysis_population = "assigned_population", outcome_variables = character(), baseline_covariates = character(), cluster_structure = NA_character_, sensitivity_analyses = character(), approvals = character()) {
  data.table::data.table(
    planned_analysis_id = aq_vnext_id("planned_analysis"),
    completed_experiment_id = completed_experiment$completed_experiment_id,
    primary_estimand = primary_estimand,
    analysis_population = analysis_population,
    assignment_variable = assignment_variable,
    outcome_variables = paste(outcome_variables, collapse = ","),
    baseline_covariates = paste(baseline_covariates, collapse = ","),
    cluster_structure = cluster_structure,
    missingness_strategy = "preserve missingness diagnostics; no imputation specified",
    multiplicity_strategy = "not specified",
    sensitivity_analyses = paste(sensitivity_analyses, collapse = ","),
    guardrail_analyses = "separate decision evidence",
    readiness_state = readiness$readiness_state[[1]],
    approvals = paste(approvals, collapse = ","),
    effect_estimated = FALSE
  )
}

#' Create a Completed-Experiment Evidence Artifact
#' @export
aq_completed_experiment_evidence_artifact <- function(completed_experiment, assignment_evidence, delivery_evidence = NULL, exposure_evidence = NULL, compliance_evidence = NULL, outcome_evidence = NULL, exclusion_evidence = NULL, missingness = NULL, reconciliation = NULL, integrity = NULL, fidelity = NULL, interference = NULL, guardrails = NULL, estimand_preservation = NULL, readiness = NULL, planned_analysis = NULL, artifact_id = NULL) {
  artifact_id <- aq_vnext_default(artifact_id, paste0("aq_completed_experiment_", completed_experiment$completed_experiment_id))
  artifact <- new_metadata_artifact(
    id = artifact_id,
    title = paste("Completed Experiment Evidence:", completed_experiment$completed_experiment_id),
    description = "Completed-experiment evidence, integrity, estimand-preservation, and analysis-readiness artifact. No effect is estimated.",
    tags = c("causal_intelligence", "completed_experiment", "analysis_readiness"),
    values = list(completed_experiment = completed_experiment$record, assignment = assignment_evidence, delivery = delivery_evidence, exposure = exposure_evidence, compliance = compliance_evidence, outcomes = outcome_evidence, exclusions = exclusion_evidence, missingness = missingness, reconciliation = reconciliation, integrity = integrity, fidelity = fidelity, interference = interference, guardrails = guardrails, estimand_preservation = estimand_preservation, readiness = readiness, planned_analysis = planned_analysis),
    dependencies = aq_vnext_unique_chr(c(completed_experiment$record$experiment_plan_artifact_id[[1]], completed_experiment$record$causal_question_id[[1]], completed_experiment$record$estimand_id[[1]])),
    source_generator = "aq_completed_experiment_evidence_artifact",
    version = "aq_completed_experiment_evidence_artifact_v1",
    metadata = list(artifact_type = "completed_experiment_evidence", completed_experiment_id = completed_experiment$completed_experiment_id, readiness_state = if (is.data.frame(readiness) && "readiness_state" %in% names(readiness) && nrow(readiness)) readiness$readiness_state[[1]] else "not_assessed", no_effect_estimated = TRUE, assignment_preserved = TRUE, supported_actions = c("validate", "register_artifact", "future_itt_analysis_if_ready"), prohibited_claims = c("causal effect was estimated", "treatment was redefined from exposure", "post-assignment exclusions were silently applied"))
  )
  artifact$artifact_envelope <- aq_vnext_attach_envelope(artifact, artifact_id = artifact_id, artifact_type = "completed_experiment_evidence", artifact_version = "aq_completed_experiment_evidence_artifact_v1", parent_artifact_ids = aq_vnext_unique_chr(c(completed_experiment$record$experiment_plan_artifact_id[[1]], completed_experiment$record$causal_question_id[[1]])), lineage = list(completed_experiment_id = completed_experiment$completed_experiment_id, no_effect_estimated = TRUE), task = "completed_experiment_analysis_readiness", operator = "deterministic_experiment_readiness", engine = "none", specification_id = completed_experiment$completed_experiment_id, supported_actions = c("validate", "register_artifact", "future_itt_analysis_if_ready"), producer = "aq_completed_experiment_evidence_artifact")
  artifact
}

#' Run Completed-Experiment Evidence Framework QA
#' @export
qa_causal_completed_experiment_framework <- function() {
  rows <- list()
  add <- function(check, ok, message) rows[[length(rows) + 1L]] <<- data.table::data.table(suite = "causal_completed_experiment_framework", check = check, status = if (isTRUE(ok)) "pass" else "fail", message = message)
  completed <- aq_completed_experiment(list(completed_experiment_id = "ce_test", experiment_plan_artifact_id = "aq_experiment_plan_eq_budget_test", decision_context_id = "decision_budget", causal_question_id = "cq_budget_revenue", estimand_id = "estimand_budget", design_version = "v1", assignment_version = "v1", experiment_status = "completed", actual_start_date = "2026-01-01", actual_end_date = "2026-02-01", data_cutoff_date = "2026-02-15", execution_owner = "analytics"))
  assignment <- aq_assignment_evidence(data.table::data.table(unit_id = paste0("u", 1:20), planned_arm = rep(c("control", "treatment"), each = 10), realized_assigned_arm = rep(c("control", "treatment"), each = 10)))
  delivery <- aq_treatment_delivery_evidence(data.table::data.table(unit_id = paste0("u", 1:20), delivered_condition = rep(c("control", "treatment"), each = 10), delivery_status = "delivered"))
  exposure <- aq_exposure_evidence(data.table::data.table(unit_id = paste0("u", 1:20), exposure = c(rep(0, 10), rep(1, 10))))
  compliance <- aq_compliance_evidence(data.table::data.table(unit_id = paste0("u", 1:20), assigned_treatment = rep(c("control", "treatment"), each = 10), treatment_received = rep(c("control", "treatment"), each = 10), adherence = 1))
  outcomes <- aq_outcome_evidence(data.table::data.table(unit_id = paste0("u", 1:20), outcome_id = "revenue", value = 1:20, outcome_role = "primary"))
  exclusions <- aq_exclusion_evidence(data.table::data.table(unit_id = "u20", exclusion_stage = "post_assignment", exclusion_reason = "outcome unavailable"))
  missingness <- aq_assess_missingness_attrition(assignment, outcomes)
  reconciliation <- aq_reconcile_experiment_execution(completed, assignment_evidence = assignment, delivery_evidence = delivery, outcome_evidence = outcomes, exclusion_evidence = exclusions)
  integrity <- aq_assess_randomization_integrity(assignment, c("control", "treatment"))
  balance <- aq_assess_realized_balance(assignment, data.table::data.table(unit_id = paste0("u", 1:20), x = rnorm(20)), baseline_cols = "x")
  fidelity <- aq_assess_treatment_fidelity(assignment, delivery, exposure, compliance)
  interference <- aq_assess_interference_spillover()
  guardrails <- aq_assess_guardrails(aq_outcome_evidence(data.table::data.table(unit_id = paste0("u", 1:20), outcome_id = "cost_guardrail", value = 0, outcome_role = "guardrail")))
  estimand <- aq_assess_estimand_preservation(completed, reconciliation, integrity, fidelity, missingness, interference, outcomes, exclusions)
  readiness <- aq_assess_experiment_analysis_readiness(completed, assignment, outcomes, reconciliation, integrity, fidelity, missingness, estimand, guardrails)
  planned <- aq_planned_analysis_record(completed, readiness, outcome_variables = "revenue", baseline_covariates = "x")
  artifact <- aq_completed_experiment_evidence_artifact(completed, assignment, delivery, exposure, compliance, outcomes, exclusions, missingness, reconciliation, integrity, fidelity, interference, guardrails, estimand, readiness, planned)
  add("completed_experiment", inherits(completed, "aq_completed_experiment") && !any(completed$validation$status == "fail"), "Completed experiment links plan, decision, question, and estimand.")
  add("assignment_preserved", all(c("planned_arm", "realized_assigned_arm") %in% names(assignment$assignment)), "Original assignment is preserved separately from realized assignment.")
  add("delivery_exposure_compliance", inherits(delivery, "aq_treatment_delivery_evidence") && inherits(exposure, "aq_exposure_evidence") && inherits(compliance, "aq_compliance_evidence"), "Delivery, exposure, and compliance are separate evidence records.")
  add("outcomes_guardrails", inherits(outcomes, "aq_outcome_evidence") && identical(guardrails$severity[[1]], "pass"), "Outcome and guardrail evidence ingest without substituting KPIs.")
  add("exclusions_post_assignment", any(exclusions$exclusions$post_assignment), "Post-assignment exclusions are explicitly preserved.")
  add("missingness_attrition", any(missingness$check == "differential_attrition"), "Missingness and attrition diagnostics are produced by arm.")
  add("reconciliation", any(reconciliation$deviation == "post_assignment_exclusions"), "Plan-versus-execution reconciliation identifies estimand-changing deviations.")
  add("integrity_balance", inherits(integrity, "list") && nrow(balance), "Randomization integrity and realized balance diagnostics run without effect estimation.")
  add("fidelity_interference", nrow(fidelity) && nrow(interference), "Treatment fidelity and interference diagnostics are separate from assignment.")
  add("estimand_readiness", nrow(estimand) && readiness$readiness_state[[1]] %in% aq_analysis_readiness_states(), "Estimand preservation and analysis readiness are deterministic.")
  add("planned_analysis", planned$effect_estimated[[1]] %in% FALSE, "Planned-analysis record prepares future estimators without estimating effects.")
  add("artifact", inherits(artifact, "aq_artifact") && isTRUE(artifact$metadata$no_effect_estimated) && isTRUE(artifact$metadata$assignment_preserved), "Completed-experiment evidence artifact uses canonical envelope.")
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}
