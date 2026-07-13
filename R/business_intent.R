# AutoQuant canonical business-intent and lever-management framework.

aq_business_intent_supported_actions <- function() {
  c(
    "validate_business_intent",
    "assess_alignment",
    "assess_explore_exploit",
    "report",
    "campaign_review",
    "experiment_planning",
    "recommendation_review",
    "decision_review",
    "optimization_planning",
    "knowledge_promotion"
  )
}

aq_business_intent_object_specs <- function() {
  data.table::data.table(
    object_type = c("mission", "objective", "strategy", "tactic", "lever", "kpi", "guardrail", "constraint", "risk", "assumption", "recommendation", "decision", "authority", "coverage"),
    id_col = c("mission_id", "objective_id", "strategy_id", "tactic_id", "lever_id", "kpi_id", "guardrail_id", "constraint_id", "risk_id", "assumption_id", "recommendation_id", "decision_id", "authority_id", "coverage_id")
  )
}

aq_business_intent_empty_dt <- function(id_col) {
  out <- data.table::data.table()
  out[, (id_col) := character()]
  out
}

aq_business_intent_as_dt <- function(x, id_col, object_type) {
  if (is.null(x)) return(aq_business_intent_empty_dt(id_col))
  if (is.data.frame(x)) {
    out <- data.table::as.data.table(x)
  } else if (is.list(x)) {
    records <- if (!is.null(names(x)) && any(nzchar(names(x)))) {
      list(x)
    } else {
      x
    }
    out <- data.table::rbindlist(lapply(records, function(record) {
      record <- as.list(record)
      data.table::as.data.table(lapply(record, function(value) list(value)))
    }), use.names = TRUE, fill = TRUE)
  } else {
    stop(object_type, " records must be a data.frame, list of records, or NULL.", call. = FALSE)
  }
  if (!id_col %in% names(out)) {
    stop(object_type, " records must include ", id_col, ".", call. = FALSE)
  }
  out[, (id_col) := as.character(get(id_col))]
  out[, object_type := object_type]
  if (!"schema_version" %in% names(out)) out[, schema_version := paste0("aq_business_", object_type, "_v1")]
  if (!"status" %in% names(out)) out[, status := "active"]
  if (!"confidence" %in% names(out)) out[, confidence := NA_real_]
  if (!"evidence_refs" %in% names(out)) out[, evidence_refs := list(character())]
  out
}

aq_business_intent_records <- function(...) {
  inputs <- list(...)
  specs <- aq_business_intent_object_specs()
  out <- list()
  for (i in seq_len(nrow(specs))) {
    type <- specs$object_type[i]
    out[[type]] <- aq_business_intent_as_dt(inputs[[type]], specs$id_col[i], type)
  }
  out
}

aq_business_intent_collect_ids <- function(records, type) {
  specs <- aq_business_intent_object_specs()
  id_col <- specs[object_type == type, id_col]
  if (!length(id_col) || !id_col %in% names(records[[type]])) return(character())
  aq_vnext_unique_chr(records[[type]][[id_col]])
}

aq_business_flat_chr <- function(x) {
  aq_vnext_unique_chr(unlist(x, recursive = TRUE, use.names = FALSE))
}

aq_business_intent_relationship_rows <- function(from_type, from_id, relationship, to_type, to_id, evidence = NA_character_) {
  from_id <- aq_vnext_unique_chr(from_id)
  to_id <- aq_vnext_unique_chr(to_id)
  if (!length(from_id) || !length(to_id)) {
    return(data.table::data.table(from_type = character(), from_id = character(), relationship = character(), to_type = character(), to_id = character(), evidence = character()))
  }
  data.table::CJ(from_id = from_id, to_id = to_id, sorted = FALSE)[
    ,
    `:=`(
      from_type = from_type,
      relationship = relationship,
      to_type = to_type,
      evidence = as.character(evidence)[1L]
    )
  ][, .(from_type, from_id, relationship, to_type, to_id, evidence)]
}

aq_business_intent_list_col <- function(dt, col) {
  if (!col %in% names(dt) || !nrow(dt)) return(vector("list", nrow(dt)))
  lapply(dt[[col]], aq_business_flat_chr)
}

aq_business_intent_infer_relationships <- function(records) {
  rows <- list()
  add_refs <- function(dt, from_type, from_id_col, ref_col, relationship, to_type) {
    if (!nrow(dt) || !ref_col %in% names(dt)) return()
    refs <- aq_business_intent_list_col(dt, ref_col)
    for (i in seq_len(nrow(dt))) {
      rows[[length(rows) + 1L]] <<- aq_business_intent_relationship_rows(from_type, dt[[from_id_col]][i], relationship, to_type, refs[[i]])
    }
  }
  add_refs(records$objective, "objective", "objective_id", "mission_id", "contained_by", "mission")
  add_refs(records$strategy, "strategy", "strategy_id", "objective_id", "supports", "objective")
  add_refs(records$tactic, "tactic", "tactic_id", "strategy_id", "executes", "strategy")
  add_refs(records$lever, "lever", "lever_id", "tactic_id", "operationalizes", "tactic")
  add_refs(records$lever, "lever", "lever_id", "related_variables", "represented_by", "variable")
  add_refs(records$kpi, "kpi", "kpi_id", "objective_id", "measures", "objective")
  add_refs(records$kpi, "kpi", "kpi_id", "source_variable", "sourced_from", "variable")
  add_refs(records$guardrail, "guardrail", "guardrail_id", "tactic_id", "limits", "tactic")
  add_refs(records$constraint, "constraint", "constraint_id", "lever_id", "bounds", "lever")
  add_refs(records$risk, "risk", "risk_id", "objective_id", "threatens", "objective")
  add_refs(records$risk, "risk", "risk_id", "tactic_id", "threatens", "tactic")
  add_refs(records$assumption, "assumption", "assumption_id", "strategy_id", "supports", "strategy")
  add_refs(records$assumption, "assumption", "assumption_id", "tactic_id", "supports", "tactic")
  add_refs(records$assumption, "assumption", "assumption_id", "lever_id", "supports", "lever")
  add_refs(records$recommendation, "recommendation", "recommendation_id", "lever_id", "proposes_action_on", "lever")
  add_refs(records$decision, "decision", "decision_id", "recommendation_id", "decides", "recommendation")
  if (!length(rows)) return(data.table::data.table(from_type = character(), from_id = character(), relationship = character(), to_type = character(), to_id = character(), evidence = character()))
  unique(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
}

aq_business_intent_normalize_relationships <- function(relationships) {
  required <- c("from_type", "from_id", "relationship", "to_type", "to_id")
  if (is.null(relationships)) {
    return(data.table::data.table(from_type = character(), from_id = character(), relationship = character(), to_type = character(), to_id = character(), evidence = character()))
  }
  out <- data.table::as.data.table(relationships)
  missing <- setdiff(required, names(out))
  if (length(missing)) stop("relationships must include: ", paste(required, collapse = ", "), call. = FALSE)
  if (!"evidence" %in% names(out)) out[, evidence := NA_character_]
  out[, (required) := lapply(.SD, as.character), .SDcols = required]
  out
}

aq_business_intent_validation_row <- function(check, status, message, object_type = NA_character_, object_id = NA_character_, severity = status, recommendation = NA_character_) {
  data.table::data.table(
    check = check,
    status = status,
    severity = severity,
    object_type = object_type,
    object_id = object_id,
    message = message,
    recommendation = recommendation
  )
}

aq_business_intent_ref_check <- function(rows, records, from_type, ref_col, to_type, check, message) {
  dt <- records[[from_type]]
  if (!nrow(dt) || !ref_col %in% names(dt)) return(rows)
  from_id_col <- aq_business_intent_object_specs()[object_type == from_type, id_col]
  valid <- aq_business_intent_collect_ids(records, to_type)
  refs <- aq_business_intent_list_col(dt, ref_col)
  for (i in seq_len(nrow(dt))) {
    missing <- setdiff(refs[[i]], valid)
    if (length(missing)) {
      rows[[length(rows) + 1L]] <- aq_business_intent_validation_row(check, "fail", paste(message, paste(missing, collapse = ", ")), from_type, dt[[from_id_col]][i], "fail", "Create the referenced object or remove the invalid reference.")
    }
  }
  rows
}

aq_business_intent_semantics_variables <- function(variable_semantics) {
  if (is.null(variable_semantics)) return(character())
  aq_vnext_unique_chr(variable_semantics$variables)
}

aq_business_intent_semantics_mappings <- function(variable_semantics) {
  if (is.null(variable_semantics) || is.null(variable_semantics$mappings)) {
    return(data.table::data.table(variable = character(), dimension = character(), semantic_type = character()))
  }
  data.table::as.data.table(variable_semantics$mappings)
}

aq_business_intent_row_has <- function(row, col, value) {
  if (!col %in% names(row)) return(FALSE)
  values <- aq_business_flat_chr(row[[col]])
  value %in% values
}

#' Create a Canonical Business Intent Object
#'
#' @description
#' Creates the Phase 2 business-intent contract. The object represents mission,
#' objectives, strategies, tactics, levers, KPIs, guardrails, constraints,
#' risks, assumptions, recommendations, decisions, authority, coverage, and
#' deterministic relationships without requiring a graph database.
#'
#' @param missions,objectives,strategies,tactics,levers,kpis,guardrails,constraints,risks,assumptions,recommendations,decisions,authority,coverage Data frames or lists of typed records.
#' @param relationships Optional relationship table. Inferred relationships are
#'   added from typed reference columns.
#' @param variable_semantics Optional `aq_variable_semantics` object.
#' @param intent_id Optional business-intent id.
#' @param supported_downstream_actions Supported downstream actions.
#'
#' @return An `aq_business_intent` object.
#' @export
aq_business_intent <- function(
  missions = NULL,
  objectives = NULL,
  strategies = NULL,
  tactics = NULL,
  levers = NULL,
  kpis = NULL,
  guardrails = NULL,
  constraints = NULL,
  risks = NULL,
  assumptions = NULL,
  recommendations = NULL,
  decisions = NULL,
  authority = NULL,
  coverage = NULL,
  relationships = NULL,
  variable_semantics = NULL,
  intent_id = NULL,
  supported_downstream_actions = aq_business_intent_supported_actions()
) {
  records <- aq_business_intent_records(
    mission = missions,
    objective = objectives,
    strategy = strategies,
    tactic = tactics,
    lever = levers,
    kpi = kpis,
    guardrail = guardrails,
    constraint = constraints,
    risk = risks,
    assumption = assumptions,
    recommendation = recommendations,
    decision = decisions,
    authority = authority,
    coverage = coverage
  )
  explicit_relationships <- aq_business_intent_normalize_relationships(relationships)
  inferred_relationships <- aq_business_intent_infer_relationships(records)
  relationships <- unique(data.table::rbindlist(list(explicit_relationships, inferred_relationships), use.names = TRUE, fill = TRUE))
  intent <- list(
    intent_id = aq_vnext_default(intent_id, aq_vnext_id("business_intent")),
    schema_version = "aq_business_intent_v1",
    records = records,
    relationships = relationships,
    variable_semantics = variable_semantics,
    supported_downstream_actions = aq_vnext_unique_chr(supported_downstream_actions),
    created_at = aq_vnext_now()
  )
  intent$alignment <- aq_assess_business_alignment(intent)
  intent$explore_exploit <- aq_assess_explore_exploit(intent)
  intent$validation <- aq_validate_business_intent(intent)
  class(intent) <- c("aq_business_intent", "list")
  intent
}

#' Validate Business Intent
#'
#' @param intent An `aq_business_intent` object.
#'
#' @return A data.table of deterministic validation diagnostics.
#' @export
aq_validate_business_intent <- function(intent) {
  rows <- list()
  add <- function(check, status, message, object_type = NA_character_, object_id = NA_character_, severity = status, recommendation = NA_character_) {
    rows[[length(rows) + 1L]] <<- aq_business_intent_validation_row(check, status, message, object_type, object_id, severity, recommendation)
  }
  if (!is.list(intent) || is.null(intent$records) || is.null(intent$relationships)) {
    add("business_intent_object", "fail", "intent must be created by aq_business_intent().")
    return(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
  }
  specs <- aq_business_intent_object_specs()
  add("business_intent_object", "pass", "business intent object is structured.", severity = "info")
  for (i in seq_len(nrow(specs))) {
    type <- specs$object_type[i]
    id_col <- specs$id_col[i]
    dt <- intent$records[[type]]
    if (is.null(dt) || !id_col %in% names(dt)) {
      add("record_contract", "fail", paste(type, "records are missing", id_col), type)
      next
    }
    ids <- aq_vnext_unique_chr(dt[[id_col]])
    bad <- ids[!nzchar(ids)]
    if (length(bad) || any(is.na(dt[[id_col]]))) {
      add("record_ids", "fail", paste(type, "records require non-empty ids."), type)
    } else {
      add("record_ids", "pass", paste(type, "id contract is valid."), type, severity = "info")
    }
    dup <- dt[, .N, by = id_col][N > 1L]
    if (nrow(dup)) {
      for (id in dup[[id_col]]) add("duplicate_record_id", "fail", paste("duplicate", type, "id."), type, id)
    }
  }
  records <- intent$records
  rows <- aq_business_intent_ref_check(rows, records, "objective", "mission_id", "mission", "objective_mission_reference", "objective references missing mission:")
  rows <- aq_business_intent_ref_check(rows, records, "strategy", "objective_id", "objective", "strategy_objective_reference", "strategy references missing objective:")
  rows <- aq_business_intent_ref_check(rows, records, "tactic", "strategy_id", "strategy", "tactic_strategy_reference", "tactic references missing strategy:")
  rows <- aq_business_intent_ref_check(rows, records, "lever", "tactic_id", "tactic", "lever_tactic_reference", "lever references missing tactic:")
  rows <- aq_business_intent_ref_check(rows, records, "kpi", "objective_id", "objective", "kpi_objective_reference", "KPI references missing objective:")
  rows <- aq_business_intent_ref_check(rows, records, "guardrail", "tactic_id", "tactic", "guardrail_tactic_reference", "guardrail references missing tactic:")
  rows <- aq_business_intent_ref_check(rows, records, "constraint", "lever_id", "lever", "constraint_lever_reference", "constraint references missing lever:")
  rows <- aq_business_intent_ref_check(rows, records, "risk", "objective_id", "objective", "risk_objective_reference", "risk references missing objective:")
  rows <- aq_business_intent_ref_check(rows, records, "assumption", "strategy_id", "strategy", "assumption_strategy_reference", "assumption references missing strategy:")
  rows <- aq_business_intent_ref_check(rows, records, "recommendation", "lever_id", "lever", "recommendation_lever_reference", "recommendation references missing lever:")
  rows <- aq_business_intent_ref_check(rows, records, "decision", "recommendation_id", "recommendation", "decision_recommendation_reference", "decision references missing recommendation:")

  semantic_variables <- aq_business_intent_semantics_variables(intent$variable_semantics)
  if (nrow(records$lever) && "related_variables" %in% names(records$lever) && length(semantic_variables)) {
    refs <- aq_business_intent_list_col(records$lever, "related_variables")
    for (i in seq_len(nrow(records$lever))) {
      missing <- setdiff(refs[[i]], semantic_variables)
      if (length(missing)) add("lever_variable_semantics_reference", "fail", paste("lever variables missing from variable semantics:", paste(missing, collapse = ", ")), "lever", records$lever$lever_id[i], recommendation = "Add variable semantics or remove the invalid lever-variable mapping.")
    }
  }
  if (nrow(records$kpi) && "source_variable" %in% names(records$kpi) && length(semantic_variables)) {
    refs <- aq_business_intent_list_col(records$kpi, "source_variable")
    for (i in seq_len(nrow(records$kpi))) {
      missing <- setdiff(refs[[i]], semantic_variables)
      if (length(missing)) add("kpi_variable_semantics_reference", "fail", paste("KPI variables missing from variable semantics:", paste(missing, collapse = ", ")), "kpi", records$kpi$kpi_id[i], recommendation = "Add variable semantics for KPI source variables.")
    }
  }
  mappings <- aq_business_intent_semantics_mappings(intent$variable_semantics)
  if (nrow(records$lever) && nrow(mappings)) {
    for (i in seq_len(nrow(records$lever))) {
      lever_vars <- aq_business_flat_chr(records$lever$related_variables[i])
      eligible <- aq_business_bool(records$lever$optimization_eligible[i])
      if (eligible && length(lever_vars)) {
        opt_vars <- mappings[variable %in% lever_vars & dimension %in% c("decision", "operational_eligibility") & semantic_type == "optimization_eligible", variable]
        if (!length(opt_vars)) add("optimization_eligible_lever_semantics", "warning", "optimization-eligible lever has no optimization-eligible variable semantics.", "lever", records$lever$lever_id[i], "warning", "Mark at least one related variable as optimization eligible or revise lever eligibility.")
      }
      control_vars <- mappings[variable %in% lever_vars & dimension %in% c("operational", "operational_eligibility") & semantic_type %in% c("control", "adjustment_only", "non_actionable"), variable]
      if (length(control_vars) && eligible) add("control_variable_not_silent_lever", "warning", paste("lever maps to variables also marked as controls/non-actionable:", paste(unique(control_vars), collapse = ", ")), "lever", records$lever$lever_id[i], "warning", "Review whether the mapping represents source, exposure, or contextual control.")
    }
  }
  if (!any(vapply(rows, function(x) identical(x$status, "fail"), logical(1L)))) {
    add("validation_summary", "pass", "business intent validation completed without failures.", severity = "info")
  }
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

aq_business_alignment_issue <- function(issue_type, severity, affected_type, affected_id, evidence, recommendation, required_human_input = FALSE) {
  data.table::data.table(
    alignment_status = if (severity %in% c("critical", "high")) "misaligned" else "needs_review",
    issue_type = issue_type,
    severity = severity,
    affected_type = affected_type,
    affected_id = affected_id,
    evidence = evidence,
    recommendation = recommendation,
    required_human_input = isTRUE(required_human_input)
  )
}

#' Assess Strategy, Tactic, and Measurement Alignment
#'
#' @param intent An `aq_business_intent` object or compatible list.
#'
#' @return A data.table of alignment findings.
#' @export
aq_assess_business_alignment <- function(intent) {
  records <- intent$records
  issues <- list()
  if (is.null(records)) return(data.table::data.table())
  if (nrow(records$objective)) {
    strategy_objectives <- if ("objective_id" %in% names(records$strategy)) aq_business_flat_chr(records$strategy$objective_id) else character()
    kpi_objectives <- if ("objective_id" %in% names(records$kpi)) aq_business_flat_chr(records$kpi$objective_id) else character()
    for (id in records$objective$objective_id) {
      if (!id %in% strategy_objectives) issues[[length(issues) + 1L]] <- aq_business_alignment_issue("objective_has_no_strategy", "high", "objective", id, "No strategy references this objective.", "Define or link a strategy before treating the objective as operationalized.", TRUE)
      if (!id %in% kpi_objectives) issues[[length(issues) + 1L]] <- aq_business_alignment_issue("objective_has_no_kpi", "high", "objective", id, "No KPI measures this objective.", "Define a KPI or document why the objective is qualitative.", TRUE)
    }
  }
  if (nrow(records$strategy)) {
    tactic_strategies <- if ("strategy_id" %in% names(records$tactic)) aq_business_flat_chr(records$tactic$strategy_id) else character()
    for (id in records$strategy$strategy_id) {
      if (!id %in% tactic_strategies) issues[[length(issues) + 1L]] <- aq_business_alignment_issue("strategy_has_no_tactic", "high", "strategy", id, "No tactic executes this strategy.", "Define tactics that operationalize the strategic thesis.", TRUE)
    }
  }
  if (nrow(records$tactic)) {
    lever_tactics <- if ("tactic_id" %in% names(records$lever)) aq_business_flat_chr(records$lever$tactic_id) else character()
    guardrail_tactics <- if ("tactic_id" %in% names(records$guardrail)) aq_business_flat_chr(records$guardrail$tactic_id) else character()
    for (id in records$tactic$tactic_id) {
      if (!id %in% lever_tactics) issues[[length(issues) + 1L]] <- aq_business_alignment_issue("tactic_has_no_lever", "high", "tactic", id, "No lever operationalizes this tactic.", "Map the tactic to one or more controllable levers.", TRUE)
      if (!id %in% guardrail_tactics) issues[[length(issues) + 1L]] <- aq_business_alignment_issue("tactic_has_no_guardrail", "medium", "tactic", id, "No guardrail limits this tactic.", "Define guardrails where downside risk matters.", FALSE)
    }
  }
  if (nrow(records$lever)) {
    mappings <- aq_business_intent_semantics_mappings(intent$variable_semantics)
    for (i in seq_len(nrow(records$lever))) {
      id <- records$lever$lever_id[i]
      vars <- aq_business_flat_chr(records$lever$related_variables[i])
      if (!length(vars)) issues[[length(issues) + 1L]] <- aq_business_alignment_issue("lever_has_no_variable_mapping", "high", "lever", id, "Lever is not represented by any variables.", "Map the lever to source, planned, realized, cost, exposure, or engineered variables.", TRUE)
      if (aq_business_bool(records$lever$optimization_eligible[i]) && !aq_business_bool(records$lever$execution_eligible[i]) && !aq_business_bool(records$lever$approval_required[i])) {
        issues[[length(issues) + 1L]] <- aq_business_alignment_issue("optimization_without_authority", "high", "lever", id, "Lever is optimization eligible but neither execution eligible nor approval gated.", "Add authority/approval constraints or remove optimization eligibility.", TRUE)
      }
      if (nrow(mappings) && length(vars)) {
        guardrail_vars <- mappings[variable %in% vars & dimension %in% c("business_role", "decision") & semantic_type == "guardrail", variable]
        if (length(guardrail_vars) && aq_business_bool(records$lever$optimization_eligible[i])) {
          issues[[length(issues) + 1L]] <- aq_business_alignment_issue("guardrail_variable_optimized", "high", "lever", id, paste("Guardrail variable(s) mapped to optimization lever:", paste(unique(guardrail_vars), collapse = ", ")), "Separate guardrail measurement from controllable lever representation.", TRUE)
        }
      }
    }
  }
  if (nrow(records$assumption)) {
    for (i in seq_len(nrow(records$assumption))) {
      if (!"evidence_status" %in% names(records$assumption) || records$assumption$evidence_status[i] %in% c(NA, "missing", "unsupported", "unknown")) {
        issues[[length(issues) + 1L]] <- aq_business_alignment_issue("unsupported_assumption", "medium", "assumption", records$assumption$assumption_id[i], "Assumption lacks supporting evidence.", "Prioritize evidence collection or an experiment if the assumption is decision-critical.", FALSE)
      }
    }
  }
  if (!length(issues)) {
    return(data.table::data.table(alignment_status = "aligned", issue_type = "none", severity = "info", affected_type = NA_character_, affected_id = NA_character_, evidence = "No structural alignment issues detected.", recommendation = "Continue monitoring evidence and assumptions.", required_human_input = FALSE))
  }
  data.table::rbindlist(issues, use.names = TRUE, fill = TRUE)
}

aq_business_num <- function(x, default = NA_real_) {
  if (is.null(x) || !length(x)) return(default)
  x <- suppressWarnings(as.numeric(unlist(x, recursive = TRUE, use.names = FALSE))[1L])
  if (is.finite(x)) x else default
}

aq_business_bool <- function(x, default = FALSE) {
  if (is.null(x) || !length(x) || is.na(x[1L])) return(default)
  x <- unlist(x, recursive = TRUE, use.names = FALSE)
  if (!length(x) || is.na(x[1L])) return(default)
  isTRUE(x[1L]) || identical(tolower(as.character(x[1L])), "true")
}

aq_business_explore_state <- function(evidence_strength, uncertainty, economic_importance, reversible, experiment_eligible, authority, measurement_quality, risk = NA_real_) {
  evidence_strength <- aq_business_num(evidence_strength, NA_real_)
  uncertainty <- aq_business_num(uncertainty, NA_real_)
  economic_importance <- aq_business_num(economic_importance, NA_real_)
  measurement_quality <- aq_business_num(measurement_quality, 0.5)
  risk <- aq_business_num(risk, 0.5)
  reversible <- aq_business_bool(reversible, FALSE)
  experiment_eligible <- aq_business_bool(experiment_eligible, FALSE)
  authority <- aq_business_bool(authority, FALSE)
  if (!is.finite(evidence_strength) || !is.finite(uncertainty) || !is.finite(economic_importance)) {
    return(list(state = "insufficient_information", reasons = "Evidence strength, uncertainty, or economic importance is missing."))
  }
  if (risk >= 0.8 && !authority) return(list(state = "escalate", reasons = "High risk and insufficient authority."))
  if (measurement_quality < 0.4) return(list(state = "collect_more_evidence", reasons = "Measurement quality is too weak for action."))
  if (evidence_strength >= 0.75 && uncertainty <= 0.35 && authority) return(list(state = if (risk >= 0.6) "exploit_with_monitoring" else "exploit", reasons = "Evidence is strong, uncertainty is low, and authority is present."))
  if (uncertainty >= 0.6 && economic_importance >= 0.6 && experiment_eligible && reversible) return(list(state = "explore", reasons = "Uncertainty and economic importance are high, and experimentation is feasible."))
  if (uncertainty >= 0.6 && economic_importance < 0.4) return(list(state = "hold", reasons = "Uncertainty is high but business importance is low."))
  if (!authority) return(list(state = "escalate", reasons = "Action requires authority or approval."))
  list(state = "collect_more_evidence", reasons = "Evidence is not yet sufficient for exploitation.")
}

#' Assess Explore/Exploit Status
#'
#' @param intent An `aq_business_intent` object or compatible list.
#'
#' @return A data.table with deterministic explore/exploit classifications.
#' @export
aq_assess_explore_exploit <- function(intent) {
  records <- intent$records
  rows <- list()
  if (is.null(records)) return(data.table::data.table())
  if (nrow(records$lever)) {
    for (i in seq_len(nrow(records$lever))) {
      row <- records$lever[i]
      state <- aq_business_explore_state(
        row$evidence_strength,
        row$uncertainty,
        row$economic_importance,
        row$reversibility,
        row$experiment_eligible,
        aq_business_bool(row$execution_eligible, FALSE) || aq_business_bool(row$approval_required, FALSE),
        row$measurement_quality,
        row$risk
      )
      rows[[length(rows) + 1L]] <- data.table::data.table(
        object_type = "lever",
        object_id = row$lever_id,
        context_id = aq_vnext_default(row$context_id, NA_character_),
        validated_range = aq_vnext_default(row$validated_range, NA_character_),
        explore_exploit_state = state$state,
        reasons = state$reasons,
        recommended_next_action = switch(state$state,
          explore = "Design a bounded experiment.",
          exploit = "Use the lever within the validated range.",
          exploit_with_monitoring = "Use the lever with guardrail monitoring.",
          hold = "Do not spend evidence budget yet.",
          collect_more_evidence = "Collect measurement or support evidence.",
          escalate = "Escalate for authority or risk review.",
          constrain = "Constrain the lever.",
          retire = "Retire the lever.",
          insufficient_information = "Complete lever evidence metadata.",
          "Review manually."
        )
      )
    }
  }
  if (nrow(records$assumption)) {
    for (i in seq_len(nrow(records$assumption))) {
      row <- records$assumption[i]
      state <- aq_business_explore_state(
        row$evidence_strength,
        row$unresolved_uncertainty,
        row$economic_importance,
        TRUE,
        row$experiment_eligible,
        TRUE,
        row$measurement_quality,
        row$risk
      )
      rows[[length(rows) + 1L]] <- data.table::data.table(
        object_type = "assumption",
        object_id = row$assumption_id,
        context_id = aq_vnext_default(row$context_id, NA_character_),
        validated_range = NA_character_,
        explore_exploit_state = state$state,
        reasons = state$reasons,
        recommended_next_action = if (identical(state$state, "explore")) "Test the assumption." else "Review assumption evidence."
      )
    }
  }
  if (!length(rows)) return(data.table::data.table(object_type = character(), object_id = character(), context_id = character(), validated_range = character(), explore_exploit_state = character(), reasons = character(), recommended_next_action = character()))
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

aq_business_intent_flatten_records <- function(records) {
  data.table::rbindlist(lapply(names(records), function(type) {
    dt <- data.table::copy(records[[type]])
    if (!nrow(dt)) return(NULL)
    id_col <- aq_business_intent_object_specs()[object_type == type, id_col]
    data.table::data.table(
      object_type = type,
      object_id = dt[[id_col]],
      title = if ("title" %in% names(dt)) vapply(dt$title, function(x) aq_vnext_default(aq_business_flat_chr(x)[1L], NA_character_), character(1L)) else if ("name" %in% names(dt)) vapply(dt$name, function(x) aq_vnext_default(aq_business_flat_chr(x)[1L], NA_character_), character(1L)) else dt[[id_col]],
      status = if ("status" %in% names(dt)) vapply(dt$status, function(x) aq_vnext_default(aq_business_flat_chr(x)[1L], NA_character_), character(1L)) else NA_character_,
      confidence = if ("confidence" %in% names(dt)) vapply(dt$confidence, aq_business_num, numeric(1L)) else NA_real_
    )
  }), use.names = TRUE, fill = TRUE)
}

#' Create a Business Intent Artifact
#'
#' @param intent An `aq_business_intent` object.
#' @param artifact_id Optional artifact id.
#'
#' @return A canonical business-intent artifact.
#' @export
aq_business_intent_artifact <- function(intent, artifact_id = NULL) {
  if (!inherits(intent, "aq_business_intent")) stop("intent must be created by aq_business_intent().", call. = FALSE)
  validation <- aq_validate_business_intent(intent)
  alignment <- aq_assess_business_alignment(intent)
  explore_exploit <- aq_assess_explore_exploit(intent)
  warnings <- c(validation[status %in% c("warning", "fail", "error"), message], alignment[severity %in% c("critical", "high"), evidence])
  artifact_id <- aq_vnext_default(artifact_id, aq_vnext_id("business_intent_artifact"))
  artifact <- new_table_artifact(
    id = artifact_id,
    title = "Business Intent and Lever Portfolio",
    data = aq_business_intent_flatten_records(intent$records),
    source_generator = "aq_business_intent_artifact",
    tags = c("vnext", "business_intent", "levers", "semantics"),
    dependencies = aq_vnext_default(intent$variable_semantics$semantics_id, character()),
    version = "aq_business_intent_artifact_v1",
    metadata = list(
      artifact_type = "business_intent_artifact",
      intent_id = intent$intent_id,
      schema_version = intent$schema_version,
      records = intent$records,
      relationships = intent$relationships,
      validation = validation,
      alignment = alignment,
      explore_exploit = explore_exploit,
      variable_semantics_id = aq_vnext_default(intent$variable_semantics$semantics_id, NA_character_),
      warnings = warnings,
      supported_actions = intent$supported_downstream_actions
    )
  )
  artifact <- aq_vnext_attach_envelope(
    artifact,
    artifact_id = artifact_id,
    artifact_type = "business_intent_artifact",
    artifact_version = "aq_business_intent_artifact_v1",
    parent_artifact_ids = aq_vnext_default(intent$variable_semantics$semantics_id, character()),
    lineage = list(intent_id = intent$intent_id, relationship_count = nrow(intent$relationships), variable_semantics_id = aq_vnext_default(intent$variable_semantics$semantics_id, NA_character_)),
    task = "business_intent",
    operator = "business_intent_specification",
    engine = "deterministic_business_intent_validator",
    specification_id = intent$intent_id,
    warnings = warnings,
    supported_actions = intent$supported_downstream_actions,
    producer = "aq_business_intent_artifact"
  )
  artifact
}

aq_business_intent_fixture <- function() {
  semantics <- aq_variable_semantics(
    variables = c("revenue", "paid_search_spend", "planned_paid_search_spend", "applications", "inventory_cap", "region"),
    business_role = list(revenue = c("objective_metric", "measurement_kpi"), paid_search_spend = c("tactic_lever", "business_lever", "cost"), planned_paid_search_spend = c("tactic_lever", "business_lever"), applications = "measurement_kpi", inventory_cap = c("constraint", "guardrail", "risk_indicator"), region = "contextual_control"),
    operational_eligibility = list(paid_search_spend = c("controllable", "optimization_eligible", "experiment_eligible"), planned_paid_search_spend = c("controllable", "scenario_only"), inventory_cap = "non_controllable", region = "adjustment_only"),
    analytical_role = list(revenue = "target", paid_search_spend = "predictor", planned_paid_search_spend = "known_future_regressor", applications = "target", inventory_cap = "predictor", region = "entity_variable"),
    causal_role = list(paid_search_spend = "exposure", revenue = "outcome", applications = "outcome", inventory_cap = "moderator", region = "confounder_candidate"),
    decision = list(revenue = "primary_kpi", paid_search_spend = "optimization_eligible", inventory_cap = "guardrail"),
    confidence = list(revenue = 0.9, paid_search_spend = 0.8, planned_paid_search_spend = 0.85, applications = 0.8, inventory_cap = 0.75, region = 0.7),
    business_context_refs = list(mission_id = "mission_growth", business_objective_id = "objective_revenue_growth", strategy_id = "strategy_qualified_demand", tactic_id = "tactic_paid_search", lever_id = "lever_paid_search_budget", risk_id = "risk_capacity", decision_id = "decision_budget_shift"),
    dataset_id = "business_intent_fixture"
  )
  aq_business_intent(
    missions = list(mission_id = "mission_growth", title = "Sustainable Growth", description = "Grow while preserving operational quality.", owner = "Executive Team"),
    objectives = list(objective_id = "objective_revenue_growth", mission_id = "mission_growth", title = "Increase Revenue", owner = "CMO", organizational_scope = "function", time_horizon = "quarter", priority = "high", desired_direction = "increase", primary_kpis = list("kpi_revenue"), guardrails = list("guardrail_capacity"), constraints = list("constraint_budget"), risk_tolerance = "medium", strategic_importance = "high", confidence = 0.75),
    strategies = list(strategy_id = "strategy_qualified_demand", objective_id = "objective_revenue_growth", title = "Increase Qualified Demand", strategic_thesis = "Increase qualified paid demand to improve revenue.", intended_mechanism = "Increase awareness and intent among eligible prospects.", tactics = list("tactic_paid_search"), assumptions = list("assumption_search_capacity"), owner = "Growth Lead", organizational_scope = "marketing", confidence = 0.7),
    tactics = list(tactic_id = "tactic_paid_search", strategy_id = "strategy_qualified_demand", title = "Expand Paid Search Acquisition", operational_description = "Adjust paid-search budget and target ROAS.", associated_levers = list("lever_paid_search_budget"), expected_mechanism = "More qualified search demand creates more applications.", controllability = "partially_controllable", reversibility = TRUE, experiment_eligible = TRUE, optimization_eligible = TRUE, authority_requirements = "approval_required"),
    levers = list(lever_id = "lever_paid_search_budget", name = "Paid Search Budget", tactic_id = "tactic_paid_search", strategy_id = "strategy_qualified_demand", related_variables = list(c("paid_search_spend", "planned_paid_search_spend")), controllability = "fully_controllable", current_value = 100000, permitted_range = "50000-150000", validated_range = "80000-120000", action_granularity = "weekly", adjustment_cadence = "weekly", implementation_delay = "1 week", effect_delay = "1-3 weeks", reversibility = TRUE, cost = "media spend", evidence_strength = 0.82, uncertainty = 0.25, economic_importance = 0.9, measurement_quality = 0.8, risk = 0.35, optimization_eligible = TRUE, experiment_eligible = TRUE, recommendation_eligible = TRUE, simulation_eligible = TRUE, execution_eligible = FALSE, approval_required = TRUE, lifecycle_status = "exploitable"),
    kpis = list(kpi_id = "kpi_revenue", objective_id = "objective_revenue_growth", strategy_id = "strategy_qualified_demand", tactic_id = "tactic_paid_search", source_variable = "revenue", definition = "Total revenue", unit = "USD", aggregation = "sum", time_window = "weekly", target_value = 1200000, baseline_value = 1000000, role = "primary", confidence = 0.9),
    guardrails = list(guardrail_id = "guardrail_capacity", tactic_id = "tactic_paid_search", source_variable = "inventory_cap", definition = "Do not exceed available capacity.", measured = TRUE),
    constraints = list(constraint_id = "constraint_budget", lever_id = "lever_paid_search_budget", definition = "Budget cannot exceed approved weekly range.", lower_bound = 50000, upper_bound = 150000),
    risks = list(risk_id = "risk_capacity", objective_id = "objective_revenue_growth", tactic_id = "tactic_paid_search", title = "Capacity Risk", severity = "medium", evidence_refs = list("guardrail_capacity")),
    assumptions = list(assumption_id = "assumption_search_capacity", statement = "Search capacity can absorb incremental qualified demand.", strategy_id = "strategy_qualified_demand", tactic_id = "tactic_paid_search", lever_id = "lever_paid_search_budget", type = "mechanism", evidence_status = "partial", confidence = 0.65, testability = "testable", experiment_eligible = TRUE, evidence_strength = 0.45, unresolved_uncertainty = 0.65, economic_importance = 0.85, measurement_quality = 0.75),
    recommendations = list(recommendation_id = "recommendation_budget_shift", lever_id = "lever_paid_search_budget", proposed_action = "Increase within validated range with monitoring.", status = "proposed"),
    decisions = list(decision_id = "decision_budget_shift", recommendation_id = "recommendation_budget_shift", decision = "awaiting_approval", owner = "CMO"),
    authority = list(authority_id = "authority_marketing_advisory", organizational_scope = "function", decision_domain = "marketing", authority_level = "recommend", budget_threshold = 25000, approval_required = TRUE, required_approver = "CMO"),
    coverage = list(coverage_id = "coverage_marketing_only", represented_domains = list("marketing"), missing_domains = list(c("finance", "operations")), authority_levels = list("observe", "diagnose", "recommend"), limitation = "Optimized within represented marketing levers and supplied enterprise constraints; not an enterprise-wide allocation recommendation."),
    variable_semantics = semantics,
    intent_id = "business_intent_fixture"
  )
}

#' QA for Business Intent and Lever Management
#'
#' @return A data.table of deterministic QA checks.
#' @export
qa_business_intent_framework <- function() {
  rows <- list()
  add <- function(check, ok, message = "") {
    rows[[length(rows) + 1L]] <<- data.table::data.table(
      suite = "business_intent_framework",
      check = check,
      status = if (isTRUE(ok)) "pass" else "fail",
      message = message
    )
  }
  intent <- aq_business_intent_fixture()
  add("business_intent_object", inherits(intent, "aq_business_intent") && length(intent$records) >= 10L)
  add("distinct_contracts", all(c("mission", "objective", "strategy", "tactic", "lever", "kpi", "guardrail", "constraint", "risk", "assumption", "recommendation", "decision", "authority", "coverage") %in% names(intent$records)))
  add("lever_variable_mapping", "lever_paid_search_budget" %in% intent$records$lever$lever_id && length(aq_business_flat_chr(intent$records$lever$related_variables)) >= 2L)
  add("relationships_inferred", nrow(intent$relationships) > 10L && all(c("supports", "executes", "operationalizes", "represented_by") %in% intent$relationships$relationship))
  add("variable_semantics_preserved", inherits(intent$variable_semantics, "aq_variable_semantics") && any(intent$variable_semantics$mappings$semantic_type == "business_lever"))
  validation <- aq_validate_business_intent(intent)
  add("validation_passes", !any(validation$status %in% c("fail", "error")), paste(validation$message, collapse = " | "))
  alignment <- aq_assess_business_alignment(intent)
  add("alignment_runs", nrow(alignment) > 0L && all(c("alignment_status", "issue_type", "severity", "recommendation") %in% names(alignment)))
  explore <- aq_assess_explore_exploit(intent)
  add("explore_exploit_runs", all(c("exploit", "explore") %in% explore$explore_exploit_state))
  add("authority_distinct_from_capability", !aq_business_bool(intent$records$lever$execution_eligible[1]) && aq_business_bool(intent$records$lever$approval_required[1]) && "recommend" %in% aq_business_flat_chr(intent$records$authority$authority_level[1]))
  add("partial_coverage_recorded", "operations" %in% aq_business_flat_chr(intent$records$coverage$missing_domains))
  bad <- aq_business_intent(
    missions = list(mission_id = "m1", title = "Mission"),
    objectives = list(objective_id = "o1", mission_id = "m1", title = "Objective"),
    strategies = list(strategy_id = "s1", objective_id = "missing_objective", title = "Bad Strategy"),
    tactics = list(tactic_id = "t1", strategy_id = "s1", title = "Tactic"),
    levers = list(lever_id = "l1", tactic_id = "t1", related_variables = list("x"), optimization_eligible = TRUE, execution_eligible = FALSE, approval_required = FALSE),
    kpis = list(kpi_id = "k1", objective_id = "missing_objective", source_variable = "missing"),
    variable_semantics = aq_variable_semantics(variables = "x", business_role = list(x = "business_lever"), decision = list(x = "optimization_eligible"))
  )
  bad_validation <- aq_validate_business_intent(bad)
  bad_alignment <- aq_assess_business_alignment(bad)
  add("invalid_references_detected", any(bad_validation$status == "fail"))
  add("misalignment_detected", any(bad_alignment$issue_type %in% c("objective_has_no_kpi", "optimization_without_authority")))
  artifact <- aq_business_intent_artifact(intent)
  add("artifact_constructed", inherits(artifact, "aq_artifact") && identical(artifact$metadata$artifact_type, "business_intent_artifact"))
  add("artifact_integrity", !aq_vnext_has_validation_error(aq_validate_artifact(artifact)) && identical(aq_artifact_envelope(artifact)$operator, "business_intent_specification"))
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}
