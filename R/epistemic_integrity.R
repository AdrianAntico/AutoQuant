# AutoQuant portable Epistemic Integrity contracts.

aq_epi_chr <- function(x, default = NA_character_) {
  if (is.null(x) || !length(x) || is.na(x[[1]])) return(default)
  out <- as.character(x[[1]])
  if (!nzchar(out)) default else out
}

aq_epi_bool <- function(x, default = FALSE) {
  if (is.null(x) || !length(x) || is.na(x[[1]])) return(default)
  isTRUE(x[[1]]) || tolower(as.character(x[[1]])) %in% c("true", "yes", "1")
}

aq_epi_dt <- function(x, id_col = NULL) {
  if (is.null(x)) return(data.table::data.table())
  out <- if (is.data.frame(x)) {
    data.table::as.data.table(x)
  } else {
    data.table::rbindlist(lapply(if (!is.null(names(x)) && any(nzchar(names(x)))) list(x) else x, function(record) data.table::as.data.table(as.list(record))), use.names = TRUE, fill = TRUE)
  }
  if (!is.null(id_col) && !id_col %in% names(out) && nrow(out)) stop("records must include ", id_col, ".", call. = FALSE)
  out
}

aq_epi_add_missing <- function(dt, cols, default = NA) {
  for (col in setdiff(cols, names(dt))) dt[, (col) := default]
  dt
}

aq_epi_fill_chr <- function(x, default) {
  out <- as.character(x)
  out[is.na(out) | !nzchar(out)] <- default
  out
}

#' Epistemic Integrity Risk Registry
#' @export
aq_epistemic_risk_registry <- function() {
  data.table::data.table(
    risk_code = c(
      "post_result_population_change",
      "metric_or_outcome_switching",
      "favorable_time_window_change",
      "undocumented_exclusion",
      "robustness_cherry_picking",
      "contradictory_evidence_omission",
      "causal_language_overreach",
      "estimand_drift",
      "claim_strength_beyond_evidence",
      "authority_requested_analytical_change",
      "missing_independent_review",
      "unsupported_narrative_strengthening"
    ),
    risk_family = c(
      "scope_change", "outcome_change", "window_change", "population_change",
      "robustness_selection", "evidence_omission", "causal_overclaim",
      "estimand_change", "claim_governance", "human_intervention",
      "review_gap", "claim_governance"
    ),
    finding_type = c(
      "reasoning_risk", "reasoning_risk", "reasoning_risk", "reasoning_risk",
      "reasoning_risk", "reasoning_risk", "claim_risk", "causal_risk",
      "claim_risk", "intervention_risk", "review_risk", "claim_risk"
    ),
    default_severity = c("high", "high", "medium", "high", "medium", "high", "critical", "critical", "critical", "medium", "high", "high"),
    deterministic_detector = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    description = c(
      "The analyzed population or eligibility definition changed after result awareness.",
      "The primary metric, outcome, or success definition changed after result awareness.",
      "The reported time window changed toward a more favorable interpretation.",
      "Rows, segments, channels, or observations were excluded without documented rationale.",
      "Only favorable robustness checks were surfaced while other checks were omitted.",
      "Material contradictory evidence was available but omitted from the claim package.",
      "A claim uses causal language beyond the available causal evidence.",
      "The estimand, target population, treatment, comparator, or outcome drifted.",
      "The stated claim strength exceeds the evidence grade.",
      "A person with authority requested an analytical change that affects interpretation.",
      "A required independent review is missing.",
      "Narrative wording was strengthened beyond the supported evidence."
    )
  )
}

#' Claim Strength Levels
#' @export
aq_epistemic_claim_strength_levels <- function() {
  data.table::data.table(
    claim_strength = c("none", "descriptive", "associational", "predictive", "causal_planning", "causal_estimated", "decision_ready"),
    rank = 0:6,
    description = c(
      "No analytical claim is supported.",
      "The claim describes observed data or artifact metadata.",
      "The claim describes an association without causal interpretation.",
      "The claim describes predictive evidence or forecast behavior.",
      "The claim describes a causal design, estimand, or identification plan only.",
      "The claim is supported by completed causal estimation evidence.",
      "The claim is supported by sufficient evidence for a governed decision context."
    )
  )
}

#' Epistemic Finding Definitions
#' @export
aq_epistemic_finding_definitions <- function() {
  registry <- aq_epistemic_risk_registry()
  registry[, .(
    finding_code = risk_code,
    finding_type,
    severity = default_severity,
    status_values = "not_detected, detected, adjudicated_valid, adjudicated_not_material, false_positive, superseded",
    required_fields = "finding_id, finding_code, severity, evidence_refs, review_status",
    default_recommendation = "Preserve provenance, require review, and constrain downstream claim wording."
  )]
}

#' Create Epistemic Intervention Events
#' @export
aq_epistemic_intervention_event <- function(events) {
  out <- aq_epi_dt(events)
  if (!nrow(out)) return(data.table::data.table())
  out <- aq_epi_add_missing(out, c(
    "intervention_id", "project_id", "artifact_id", "decision_context_id",
    "actor_id", "actor_role", "event_type", "event_timing",
    "requested_change", "before_value", "after_value", "reason_provided",
    "source_ref", "authority_context", "result_awareness", "review_status",
    "created_at"
  ))
  out[, intervention_id := aq_epi_fill_chr(intervention_id, paste0("intervention_", seq_len(.N)))]
  out[, event_type := aq_epi_fill_chr(event_type, "unspecified_intervention")]
  out[, event_timing := aq_epi_fill_chr(event_timing, "unknown")]
  out[, actor_role := aq_epi_fill_chr(actor_role, "unknown")]
  out[, review_status := aq_epi_fill_chr(review_status, "not_reviewed")]
  out[, result_awareness := vapply(result_awareness, aq_epi_bool, logical(1L))]
  out[, created_at := aq_epi_fill_chr(created_at, aq_vnext_now())]
  out[]
}

#' Create Epistemic Claim Records
#' @export
aq_epistemic_claim_record <- function(claims) {
  out <- aq_epi_dt(claims)
  if (!nrow(out)) return(data.table::data.table())
  out <- aq_epi_add_missing(out, c(
    "claim_id", "artifact_id", "claim_text", "claim_type", "claim_strength",
    "evidence_strength", "evidence_refs", "causal_language", "decision_context_id",
    "estimand_id", "review_status", "created_at"
  ))
  out[, claim_id := aq_epi_fill_chr(claim_id, paste0("claim_", seq_len(.N)))]
  out[, claim_type := aq_epi_fill_chr(claim_type, "analytical_claim")]
  out[, claim_strength := aq_epi_fill_chr(claim_strength, "descriptive")]
  out[, evidence_strength := aq_epi_fill_chr(evidence_strength, "descriptive")]
  out[, causal_language := vapply(causal_language, aq_epi_bool, logical(1L))]
  out[, review_status := aq_epi_fill_chr(review_status, "not_reviewed")]
  out[, created_at := aq_epi_fill_chr(created_at, aq_vnext_now())]
  out[]
}

aq_epi_strength_rank <- function(x) {
  levels <- aq_epistemic_claim_strength_levels()
  idx <- match(x, levels$claim_strength)
  out <- levels$rank[idx]
  out[is.na(out)] <- 0L
  out
}

#' Detect Epistemic Integrity Findings
#' @export
aq_detect_epistemic_findings <- function(events = NULL, claims = NULL, evidence = NULL, review_requirements = NULL) {
  events <- aq_epistemic_intervention_event(events)
  claims <- aq_epistemic_claim_record(claims)
  evidence <- aq_epi_dt(evidence)
  review_requirements <- aq_epi_dt(review_requirements)
  rows <- list()
  add <- function(finding_code, source_id, evidence_refs = character(), severity = NA_character_, reason = NA_character_) {
    def <- aq_epistemic_risk_registry()[risk_code == finding_code][1]
    rows[[length(rows) + 1L]] <<- data.table::data.table(
      finding_id = paste0("finding_", finding_code, "_", length(rows) + 1L),
      finding_code = finding_code,
      finding_type = def$finding_type %||% "reasoning_risk",
      severity = severity %||% def$default_severity %||% "medium",
      source_id = source_id,
      evidence_refs = paste(evidence_refs, collapse = ","),
      reason = reason %||% def$description %||% finding_code,
      status = "detected",
      review_status = "requires_review",
      created_at = aq_vnext_now()
    )
  }
  if (nrow(events)) {
    for (i in seq_len(nrow(events))) {
      ev <- events[i]
      text <- tolower(paste(ev$event_type, ev$requested_change, ev$reason_provided, ev$before_value, ev$after_value, collapse = " "))
      if (isTRUE(ev$result_awareness) && grepl("population|eligib|segment|cohort", text)) add("post_result_population_change", ev$intervention_id, ev$artifact_id, reason = "Population or eligibility changed after result awareness.")
      if (isTRUE(ev$result_awareness) && grepl("metric|outcome|kpi|success", text)) add("metric_or_outcome_switching", ev$intervention_id, ev$artifact_id, reason = "Metric or outcome changed after result awareness.")
      if (isTRUE(ev$result_awareness) && grepl("window|period|date|time", text)) add("favorable_time_window_change", ev$intervention_id, ev$artifact_id, reason = "Time window changed after result awareness.")
      if (grepl("exclude|remove|drop|filter", text) && !nzchar(aq_epi_chr(ev$reason_provided, ""))) add("undocumented_exclusion", ev$intervention_id, ev$artifact_id, reason = "Exclusion-like change lacks documented rationale.")
      if (isTRUE(ev$result_awareness) && nzchar(aq_epi_chr(ev$authority_context, ""))) add("authority_requested_analytical_change", ev$intervention_id, ev$artifact_id, reason = "Authority-linked analytical intervention occurred after result awareness.")
    }
  }
  if (nrow(claims)) {
    claims[, claim_rank := aq_epi_strength_rank(claim_strength)]
    claims[, evidence_rank := aq_epi_strength_rank(evidence_strength)]
    for (i in seq_len(nrow(claims))) {
      cl <- claims[i]
      if (isTRUE(cl$causal_language) && cl$evidence_strength %in% c("none", "descriptive", "associational", "predictive", "causal_planning")) add("causal_language_overreach", cl$claim_id, cl$evidence_refs, reason = "Claim uses causal wording without completed causal estimation evidence.")
      if (cl$claim_rank > cl$evidence_rank) add("claim_strength_beyond_evidence", cl$claim_id, cl$evidence_refs, reason = "Claim strength exceeds evidence strength.")
      if (grepl("\\bproves\\b|\\bguarantees\\b|\\bwill definitely\\b", tolower(cl$claim_text %||% ""))) add("unsupported_narrative_strengthening", cl$claim_id, cl$evidence_refs, reason = "Narrative uses certainty wording that requires stronger evidence.")
      if (nzchar(aq_epi_chr(cl$estimand_id, "")) && cl$claim_strength %in% c("causal_estimated", "decision_ready") && cl$evidence_strength == "causal_planning") add("estimand_drift", cl$claim_id, cl$estimand_id, reason = "Claim advances beyond a planning-level estimand.")
    }
  }
  if (nrow(evidence) && all(c("evidence_id", "contradicts_claim_id", "material") %in% names(evidence))) {
    omitted <- evidence[!is.na(contradicts_claim_id) & vapply(material, aq_epi_bool, logical(1L))]
    if (nrow(omitted)) {
      for (i in seq_len(nrow(omitted))) add("contradictory_evidence_omission", omitted$contradicts_claim_id[[i]], omitted$evidence_id[[i]], reason = "Material contradictory evidence must be carried into claim governance.")
    }
  }
  if (nrow(review_requirements) && all(c("object_id", "required_review", "completed_review") %in% names(review_requirements))) {
    missing <- review_requirements[vapply(required_review, aq_epi_bool, logical(1L)) & !vapply(completed_review, aq_epi_bool, logical(1L))]
    if (nrow(missing)) {
      for (i in seq_len(nrow(missing))) add("missing_independent_review", missing$object_id[[i]], missing$object_id[[i]], reason = "Required independent review has not been completed.")
    }
  }
  if (!length(rows)) return(data.table::data.table())
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

#' Assess Claim-to-Evidence Governance
#' @export
aq_assess_epistemic_claims <- function(claims, evidence = NULL, findings = NULL) {
  claims <- aq_epistemic_claim_record(claims)
  findings <- aq_epi_dt(findings)
  if (!nrow(claims)) return(data.table::data.table())
  claims[, claim_rank := aq_epi_strength_rank(claim_strength)]
  claims[, evidence_rank := aq_epi_strength_rank(evidence_strength)]
  claims[, overclaim := claim_rank > evidence_rank]
  claims[, support_status := data.table::fifelse(overclaim, "overclaimed", data.table::fifelse(evidence_strength %in% c("none", "missing"), "unsupported", "supported_with_limits"))]
  claims[, allowed_strength := aq_epistemic_claim_strength_levels()$claim_strength[pmax(1L, evidence_rank + 1L)]]
  risk_findings <- if (nrow(findings) && "source_id" %in% names(findings)) findings[source_id %in% claims$claim_id] else data.table::data.table()
  claims[, review_required := overclaim | claim_strength %in% c("causal_estimated", "decision_ready") | claim_id %in% (risk_findings$source_id %||% character())]
  claims[, permitted_wording := data.table::fifelse(overclaim, paste("May state evidence supports at most", allowed_strength), "May state claim with explicit evidence limits.")]
  claims[, prohibited_wording := data.table::fifelse(overclaim, "Do not state the stronger claim until evidence is upgraded or reviewed.", "Do not omit evidence limits or contradictory findings.")]
  claims[, .(claim_id, claim_text, claim_strength, evidence_strength, allowed_strength, support_status, overclaim, review_required, permitted_wording, prohibited_wording)]
}

#' Create Epistemic Quality Gates
#' @export
aq_epistemic_quality_gates <- function(findings = NULL, claim_assessment = NULL) {
  findings <- aq_epi_dt(findings)
  claim_assessment <- aq_epi_dt(claim_assessment)
  rows <- list()
  add <- function(gate_id, status, severity, reason, recommendation) rows[[length(rows) + 1L]] <<- data.table::data.table(gate_id, status, severity, reason, recommendation)
  if (nrow(findings) && any(findings$severity %in% c("critical"))) add("critical_epistemic_findings", "block", "critical", "Critical epistemic findings require adjudication.", "Adjudicate findings before decision-ready claims.")
  if (nrow(findings) && any(findings$severity %in% c("high"))) add("high_epistemic_findings", "warn", "high", "High-severity epistemic findings are present.", "Carry caveats and require review.")
  if (nrow(claim_assessment) && any(vapply(claim_assessment$overclaim, aq_epi_bool, logical(1L)))) add("claim_strength_gate", "block", "critical", "At least one claim exceeds evidence strength.", "Downgrade wording or obtain stronger evidence.")
  if (!length(rows)) add("epistemic_integrity_clear", "pass", "low", "No blocking epistemic integrity gate was detected.", "Proceed with normal caveats.")
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

#' Create Epistemic Adjudication Records
#' @export
aq_epistemic_adjudication <- function(adjudications) {
  out <- aq_epi_dt(adjudications)
  if (!nrow(out)) return(data.table::data.table())
  out <- aq_epi_add_missing(out, c("adjudication_id", "finding_id", "adjudicator_id", "adjudicator_role", "adjudication_state", "decision", "rationale", "created_at"))
  out[, adjudication_id := aq_epi_fill_chr(adjudication_id, paste0("adjudication_", seq_len(.N)))]
  out[, adjudication_state := aq_epi_fill_chr(adjudication_state, "pending_review")]
  out[, created_at := aq_epi_fill_chr(created_at, aq_vnext_now())]
  out[]
}

#' Create an Epistemic Integrity Artifact
#' @export
aq_epistemic_integrity_artifact <- function(events = NULL, claims = NULL, evidence = NULL, review_requirements = NULL, adjudications = NULL, artifact_id = NULL) {
  events <- aq_epistemic_intervention_event(events)
  claims <- aq_epistemic_claim_record(claims)
  findings <- aq_detect_epistemic_findings(events, claims, evidence, review_requirements)
  assessments <- aq_assess_epistemic_claims(claims, evidence, findings)
  gates <- aq_epistemic_quality_gates(findings, assessments)
  adjudications <- aq_epistemic_adjudication(adjudications)
  artifact_id <- artifact_id %||% aq_vnext_id("epistemic_integrity_artifact")
  artifact <- new_metadata_artifact(
    id = artifact_id,
    title = "Epistemic Integrity Assessment",
    values = list(events = events, claims = claims, findings = findings, claim_assessment = assessments, quality_gates = gates, adjudications = adjudications),
    description = "Portable epistemic integrity findings, claim governance, gates, and adjudication state.",
    tags = c("epistemic_integrity", "claim_governance", "quality_gate"),
    source_generator = "aq_epistemic_integrity_artifact",
    metadata = list(
      artifact_id = artifact_id,
      artifact_type = "epistemic_integrity_artifact",
      finding_count = nrow(findings),
      blocking_gate_count = sum(gates$status == "block"),
      created_at = aq_vnext_now()
    )
  )
  aq_vnext_attach_envelope(
    artifact,
    artifact_id = artifact_id,
    artifact_type = "epistemic_integrity_artifact",
    artifact_version = "aq_epistemic_integrity_artifact_v1",
    lineage = list(source = "epistemic_integrity_contracts"),
    task = "epistemic_integrity",
    operator = "aq_epistemic_integrity_artifact",
    engine = "base_r",
    specification_id = artifact_id,
    supported_actions = c("review_findings", "adjudicate", "downgrade_claim", "request_independent_review", "register_artifact"),
    producer = "aq_epistemic_integrity_artifact"
  )
}

#' QA for Epistemic Integrity Contracts
#' @export
qa_epistemic_integrity_contracts <- function() {
  events <- aq_epistemic_intervention_event(data.table::data.table(
    intervention_id = "ev1",
    artifact_id = "artifact_1",
    actor_id = "leader_1",
    actor_role = "business_owner",
    event_type = "metric_change",
    requested_change = "Change primary metric after seeing results",
    result_awareness = TRUE,
    authority_context = "decision_owner"
  ))
  claims <- aq_epistemic_claim_record(data.table::data.table(
    claim_id = "claim_1",
    artifact_id = "artifact_1",
    claim_text = "The tactic proves revenue will definitely increase.",
    claim_strength = "decision_ready",
    evidence_strength = "associational",
    causal_language = TRUE,
    evidence_refs = "artifact_1"
  ))
  evidence <- data.table::data.table(evidence_id = "ev_contra", contradicts_claim_id = "claim_1", material = TRUE)
  reviews <- data.table::data.table(object_id = "claim_1", required_review = TRUE, completed_review = FALSE)
  findings <- aq_detect_epistemic_findings(events, claims, evidence, reviews)
  assessment <- aq_assess_epistemic_claims(claims, evidence, findings)
  gates <- aq_epistemic_quality_gates(findings, assessment)
  artifact <- aq_epistemic_integrity_artifact(events, claims, evidence, reviews)
  data.table::data.table(
    check = c("risk_registry", "events", "claims", "findings", "claim_assessment", "quality_gates", "artifact_envelope", "supported_actions"),
    status = c(
      if (nrow(aq_epistemic_risk_registry()) >= 12L) "success" else "error",
      if (nrow(events) == 1L && events$result_awareness[[1]]) "success" else "error",
      if (nrow(claims) == 1L && claims$causal_language[[1]]) "success" else "error",
      if (nrow(findings) >= 4L && "causal_language_overreach" %in% findings$finding_code) "success" else "error",
      if (nrow(assessment) == 1L && assessment$overclaim[[1]]) "success" else "error",
      if (any(gates$status == "block")) "success" else "error",
      if (identical(artifact$artifact_envelope$artifact_type, "epistemic_integrity_artifact")) "success" else "error",
      if ("adjudicate" %in% artifact$artifact_envelope$supported_actions) "success" else "error"
    ),
    message = c(
      "Risk registry includes initial deterministic finding families.",
      "Human intervention provenance is structured.",
      "Claim records preserve strength, evidence strength, and causal wording.",
      "Deterministic findings are generated for observable epistemic risks.",
      "Claim-to-evidence assessment detects overclaiming.",
      "Quality gates block critical epistemic risks.",
      "Epistemic integrity artifact uses the canonical artifact envelope.",
      "Artifact exposes review/adjudication actions, not autonomous execution."
    )
  )
}
