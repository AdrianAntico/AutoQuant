# AutoQuant canonical causal-intelligence planning framework.

aq_causal_supported_actions <- function() {
  c(
    "validate_causal_question",
    "validate_estimand",
    "validate_causal_context",
    "inspect_graph",
    "inspect_adjustment_guidance",
    "assess_identification",
    "assess_design_eligibility",
    "plan_causal_investigation",
    "register_causal_planning_artifact",
    "campaign_review",
    "genai_context",
    "report"
  )
}

aq_causal_empty <- function(cols) {
  out <- data.table::as.data.table(stats::setNames(rep(list(character()), length(cols)), cols))
  out[0]
}

aq_causal_chr1 <- function(x, default = NA_character_) {
  value <- aq_business_flat_chr(x)
  aq_vnext_default(value[1L], default)
}

aq_causal_bool <- function(x) {
  isTRUE(x) || identical(tolower(as.character(x %||% "")), "true")
}

aq_causal_record_dt <- function(x, id_col, object_type) {
  out <- aq_business_intent_as_dt(x, id_col, object_type)
  scalar_id_cols <- names(out)[grepl("(^|_)id$", names(out)) | names(out) %in% c("variable", "source_variable", "destination_variable", "exposure", "outcome")]
  for (col in scalar_id_cols) {
    out[, (col) := vapply(get(col), function(value) aq_vnext_default(aq_business_flat_chr(value)[1L], NA_character_), character(1L))]
  }
  out
}

aq_causal_question_estimand_levels <- function() {
  c(
    "ATE", "ATT", "ATC", "CATE", "policy_effect", "intention_to_treat",
    "treatment_on_treated", "controlled_direct_effect", "natural_direct_effect",
    "indirect_effect", "longitudinal_cumulative_effect", "dose_response",
    "incremental_effect", "dynamic_regime_value"
  )
}

aq_causal_role_levels <- function() {
  c(
    "exposure", "treatment_assignment", "outcome", "confounder_candidate",
    "mediator_candidate", "collider_candidate", "effect_modifier",
    "instrument_candidate", "selection_variable", "prognostic_precision_variable",
    "exposure_predictor", "proxy", "surrogate_outcome", "latent_variable_reference",
    "missingness_indicator", "censoring_variable", "compliance_uptake",
    "competing_exposure", "spillover_interference_variable", "state_variable",
    "constraint", "unknown"
  )
}

aq_causal_timing_levels <- function() {
  c("baseline", "pre_treatment", "contemporaneous", "post_treatment", "time_varying", "lagged", "future_known", "future_unknown", "unknown")
}

aq_causal_relationship_levels <- function() {
  c("causes", "may_cause", "mediates", "modifies_effect_of", "determines_treatment_assignment", "determines_selection", "measures", "proxies", "constrains", "enables", "creates_spillover_to")
}

aq_causal_directed_relationships <- function() {
  c("causes", "may_cause", "mediates", "determines_treatment_assignment", "determines_selection", "creates_spillover_to")
}

aq_causal_design_catalog <- function() {
  data.table::data.table(
    design_id = c(
      "randomized_ab", "cluster_randomization", "stratified_block_randomization", "geo_experiment", "switchback", "stepped_wedge", "factorial_design",
      "difference_in_differences", "event_study", "synthetic_control", "interrupted_time_series", "regression_discontinuity", "instrumental_variables",
      "matching", "weighting", "doubly_robust", "mediation_analysis", "heterogeneous_treatment_effects", "structural_equation_modeling",
      "marginal_structural_models", "g_computation", "dynamic_treatment_regimes", "contextual_bandits", "reinforcement_learning"
    ),
    design_family = c(rep("experimental", 7), rep("quasi_experimental", 9), rep("mechanism_heterogeneity", 3), rep("longitudinal_policy", 5)),
    required_evidence = c(
      "random assignment and outcome observability",
      "cluster assignment and enough clusters",
      "blocking variables and randomized assignment",
      "market geography, treatment variation, and spillover controls",
      "reversible treatment timing and stable periods",
      "staggered rollout with comparable units",
      "multiple interventions and factorial assignment",
      "treated and comparison units with pre-period history",
      "event timing and pre-trend diagnostics",
      "treated unit, donor pool, and long pre-period",
      "clear interruption date and sufficient pre/post observations",
      "assignment threshold and local continuity",
      "candidate instrument satisfying relevance and exclusion",
      "pre-treatment covariates and overlap",
      "pre-treatment covariates and overlap",
      "outcome, treatment, covariates, and model diagnostics",
      "mediator measurement and direct-effect estimand",
      "effect modifiers and sufficient variation",
      "asserted structural equations and measurement assumptions",
      "time-varying treatment/confounding and weights",
      "longitudinal treatment/outcome/covariate process",
      "sequential decision policy and longitudinal outcomes",
      "online policy, reward, and exploration protocol",
      "state, action, reward, transition, and safety constraints"
    )
  )
}

#' Create a Canonical Causal Question
#'
#' @param question Causal question record or list.
#' @param decision_context Optional `aq_decision_context`.
#' @param estimand Optional estimand object or estimand id/type.
#' @param causal_question_id Optional question id.
#' @param supported_downstream_actions Supported downstream actions.
#'
#' @return An `aq_causal_question` object.
#' @export
aq_causal_question <- function(
  question,
  decision_context = NULL,
  estimand = NULL,
  causal_question_id = NULL,
  supported_downstream_actions = aq_causal_supported_actions()
) {
  question <- aq_causal_record_dt(question, "causal_question_id", "causal_question")
  if (nrow(question) != 1L) stop("question must describe exactly one causal question.", call. = FALSE)
  if (!is.null(causal_question_id)) question[, causal_question_id := as.character(causal_question_id)[1L]]
  required <- c("decision_context_id", "exposure", "outcome", "population", "unit_of_analysis", "time_zero", "treatment_window", "outcome_window", "comparison_condition", "intervention_definition", "estimand", "effect_scale", "target_population")
  for (col in setdiff(required, names(question))) question[, (col) := NA_character_]
  if (is.null(estimand)) {
    estimand <- aq_estimand(
      estimand_id = paste0(question$causal_question_id[[1]], "_estimand"),
      estimand_type = aq_vnext_default(question$estimand[[1]], "ATE"),
      treatment_contrast = question$comparison_condition[[1]],
      outcome = question$outcome[[1]],
      population = question$target_population[[1]],
      time_horizon = question$outcome_window[[1]],
      effect_scale = question$effect_scale[[1]],
      interpretation = paste("Effect of", question$intervention_definition[[1]], "versus", question$comparison_condition[[1]])
    )
  }
  out <- list(
    causal_question_id = question$causal_question_id[[1]],
    schema_version = "aq_causal_question_v1",
    question = question,
    decision_context = decision_context,
    estimand = estimand,
    supported_downstream_actions = aq_vnext_unique_chr(supported_downstream_actions),
    created_at = aq_vnext_now()
  )
  out$validation <- aq_validate_causal_question(out)
  class(out) <- c("aq_causal_question", "list")
  out
}

#' Validate a Causal Question
#'
#' @param causal_question An `aq_causal_question`.
#'
#' @return A data.table of deterministic diagnostics.
#' @export
aq_validate_causal_question <- function(causal_question) {
  rows <- list()
  add <- function(check, status, message, severity = status, recommendation = NA_character_) {
    rows[[length(rows) + 1L]] <<- data.table::data.table(check = check, status = status, severity = severity, message = message, recommendation = recommendation)
  }
  if (!is.list(causal_question) || is.null(causal_question$question)) {
    add("causal_question_object", "fail", "causal_question must be created by aq_causal_question().")
    return(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
  }
  q <- causal_question$question[1]
  required <- c("causal_question_id", "decision_context_id", "exposure", "outcome", "population", "unit_of_analysis", "time_zero", "treatment_window", "outcome_window", "comparison_condition", "intervention_definition", "estimand", "effect_scale", "target_population")
  for (col in required) {
    if (!col %in% names(q) || !nzchar(aq_causal_chr1(q[[col]], ""))) add(paste0("missing_", col), "fail", paste(col, "is required."), recommendation = paste("Author", col, "before causal planning."))
  }
  vague_terms <- c("marketing", "budget", "campaign", "model", "it")
  intervention <- tolower(aq_causal_chr1(q$intervention_definition, ""))
  comparison <- tolower(aq_causal_chr1(q$comparison_condition, ""))
  if (intervention %in% vague_terms || nchar(intervention) < 12L) add("imprecise_intervention", "fail", "Intervention definition is too vague for causal planning.", recommendation = "State the lever level, change, eligibility, timing, and implementation rule.")
  if (comparison %in% vague_terms || nchar(comparison) < 4L) add("imprecise_comparison", "fail", "Comparison condition is too vague.", recommendation = "Define baseline/current policy, no treatment, or explicit alternative setting.")
  if (identical(aq_causal_chr1(q$exposure), aq_causal_chr1(q$outcome))) add("exposure_equals_outcome", "fail", "Exposure and outcome cannot be the same variable.")
  unknown_estimand <- setdiff(aq_causal_chr1(q$estimand), aq_causal_question_estimand_levels())
  if (length(unknown_estimand)) add("unknown_estimand", "warning", paste("Estimand is not in the supported planning vocabulary:", paste(unknown_estimand, collapse = ", ")), recommendation = "Use a known estimand type or mark it as custom in limitations.")
  if (!length(rows)) add("causal_question_valid", "pass", "Causal question has the minimum structure needed for planning.", "pass", "Proceed to role assignment and identification planning.")
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

#' Create an Estimand Contract
#'
#' @return An `aq_estimand` object.
#' @export
aq_estimand <- function(
  estimand_id = NULL,
  estimand_type = "ATE",
  treatment_contrast,
  outcome,
  population,
  time_horizon,
  effect_scale = "difference",
  conditioning_variables = character(),
  intervention_policy = NA_character_,
  censoring_policy = NA_character_,
  interpretation = NA_character_,
  assumptions = character(),
  limitations = character(),
  implementation_status = "planning_only"
) {
  out <- list(
    estimand_id = aq_vnext_default(estimand_id, aq_vnext_id("estimand")),
    schema_version = "aq_estimand_v1",
    estimand_type = as.character(estimand_type)[1L],
    treatment_contrast = as.character(treatment_contrast)[1L],
    outcome = as.character(outcome)[1L],
    population = as.character(population)[1L],
    time_horizon = as.character(time_horizon)[1L],
    effect_scale = as.character(effect_scale)[1L],
    conditioning_variables = aq_vnext_unique_chr(conditioning_variables),
    intervention_policy = aq_vnext_default(intervention_policy, NA_character_),
    censoring_policy = aq_vnext_default(censoring_policy, NA_character_),
    interpretation = aq_vnext_default(interpretation, NA_character_),
    assumptions = aq_vnext_unique_chr(assumptions),
    limitations = aq_vnext_unique_chr(c(limitations, "This contract represents the target effect only; it does not imply estimability or estimation support.")),
    implementation_status = implementation_status,
    created_at = aq_vnext_now()
  )
  out$validation <- aq_validate_estimand(out)
  class(out) <- c("aq_estimand", "list")
  out
}

#' Validate an Estimand
#'
#' @param estimand An `aq_estimand`.
#' @export
aq_validate_estimand <- function(estimand) {
  rows <- list()
  add <- function(check, status, message, severity = status, recommendation = NA_character_) {
    rows[[length(rows) + 1L]] <<- data.table::data.table(check = check, status = status, severity = severity, message = message, recommendation = recommendation)
  }
  if (!is.list(estimand) || is.null(estimand$estimand_type)) {
    add("estimand_object", "fail", "estimand must be created by aq_estimand().")
    return(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
  }
  for (field in c("treatment_contrast", "outcome", "population", "time_horizon", "effect_scale")) {
    if (!nzchar(aq_causal_chr1(estimand[[field]], ""))) add(paste0("missing_", field), "fail", paste(field, "is required."))
  }
  if (!estimand$estimand_type %in% aq_causal_question_estimand_levels()) add("unknown_estimand_type", "warning", "Estimand type is custom or unsupported by the planning vocabulary.")
  if (!identical(estimand$implementation_status, "planning_only")) add("estimation_claim", "warning", "Estimand implementation status should remain planning_only in Causal Intelligence Phase 1.", recommendation = "Do not imply effect estimation support.")
  if (!length(rows)) add("estimand_valid", "pass", "Estimand is represented without claiming estimation support.")
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

#' Create Question-Relative Causal Variable Roles
#'
#' @param roles Role records.
#' @return A data.table.
#' @export
aq_causal_variable_roles <- function(roles) {
  out <- aq_causal_record_dt(roles, "role_id", "causal_variable_role")
  required <- c("causal_question_id", "variable", "role", "timing", "role_confidence", "evidence_basis", "assigned_by", "rationale", "related_exposure", "related_outcome", "adjustment_eligibility", "unresolved_conflict")
  for (col in setdiff(required, names(out))) out[, (col) := NA]
  scalar_cols <- intersect(c("causal_question_id", "variable", "role", "timing", "evidence_basis", "assigned_by", "rationale", "related_exposure", "related_outcome", "adjustment_eligibility", "unresolved_conflict"), names(out))
  for (col in scalar_cols) out[, (col) := vapply(get(col), aq_causal_chr1, character(1L))]
  out
}

#' Create Question-Scoped Causal Relationships
#'
#' @param relationships Relationship records.
#' @return A data.table.
#' @export
aq_causal_relationships <- function(relationships) {
  out <- aq_causal_record_dt(relationships, "relationship_id", "causal_relationship")
  required <- c("causal_question_id", "source_variable", "destination_variable", "relationship_type", "direction", "timing", "sign", "confidence", "evidence_source", "human_rationale", "contradiction_status", "status")
  for (col in setdiff(required, names(out))) out[, (col) := NA]
  scalar_cols <- intersect(c("causal_question_id", "source_variable", "destination_variable", "relationship_type", "direction", "timing", "sign", "evidence_source", "human_rationale", "contradiction_status", "status"), names(out))
  for (col in scalar_cols) out[, (col) := vapply(get(col), aq_causal_chr1, character(1L))]
  out[, relationship_type := aq_vnext_default(relationship_type, "may_cause")]
  out[, direction := aq_vnext_default(direction, "directed")]
  out[, status := aq_vnext_default(status, "active")]
  out
}

aq_causal_directed_edges <- function(relationships) {
  if (!nrow(relationships)) return(data.table::data.table(from = character(), to = character(), relationship_id = character(), relationship_type = character()))
  relationships[status %in% c("active", NA_character_) & relationship_type %in% aq_causal_directed_relationships(), .(from = source_variable, to = destination_variable, relationship_id, relationship_type)]
}

aq_causal_reachable <- function(edges, start, end = NULL, blocked = character()) {
  if (!nrow(edges) || !length(start)) return(character())
  visited <- character()
  frontier <- setdiff(start, blocked)
  while (length(frontier)) {
    node <- frontier[1L]
    frontier <- frontier[-1L]
    if (node %in% visited || node %in% blocked) next
    visited <- c(visited, node)
    if (!is.null(end) && node %in% end) return(visited)
    next_nodes <- edges[from == node, to]
    frontier <- unique(c(frontier, setdiff(next_nodes, c(visited, blocked))))
  }
  visited
}

aq_causal_has_path <- function(edges, from, to, blocked = character()) {
  if (!nrow(edges) || !length(from) || !length(to)) return(FALSE)
  any(to %in% aq_causal_reachable(edges, from, blocked = blocked))
}

aq_causal_has_cycle <- function(edges) {
  if (!nrow(edges)) return(FALSE)
  for (i in seq_len(nrow(edges))) {
    remaining_edges <- edges[-i]
    if (aq_causal_has_path(remaining_edges, edges$to[[i]], edges$from[[i]])) return(TRUE)
  }
  FALSE
}

aq_causal_ancestors <- function(edges, node) {
  rev_edges <- data.table::copy(edges)[, .(from = to, to = from)]
  setdiff(aq_causal_reachable(rev_edges, node), node)
}

aq_causal_descendants <- function(edges, node) {
  setdiff(aq_causal_reachable(edges, node), node)
}

aq_causal_role_vars <- function(roles, role_values) {
  if (!nrow(roles)) return(character())
  unique(roles[role %in% role_values, variable])
}

#' Create a Causal Context
#'
#' @return An `aq_causal_context` object.
#' @export
aq_causal_context <- function(
  causal_question,
  estimand = NULL,
  roles = NULL,
  relationships = NULL,
  variable_semantics = NULL,
  temporal_ordering = NULL,
  assumptions = character(),
  selection_mechanism = NA_character_,
  measurement_mechanism = NA_character_,
  authority = NA_character_,
  coverage = NA_character_,
  candidate_designs = NULL,
  causal_context_id = NULL
) {
  if (!inherits(causal_question, "aq_causal_question")) stop("causal_question must be created by aq_causal_question().", call. = FALSE)
  estimand <- aq_vnext_default(estimand, causal_question$estimand)
  roles <- if (is.null(roles)) {
    aq_causal_variable_roles(data.table::data.table(role_id = character()))
  } else {
    aq_causal_variable_roles(roles)
  }
  relationships <- if (is.null(relationships)) {
    aq_causal_relationships(data.table::data.table(relationship_id = character()))
  } else {
    aq_causal_relationships(relationships)
  }
  out <- list(
    causal_context_id = aq_vnext_default(causal_context_id, aq_vnext_id("causal_context")),
    schema_version = "aq_causal_context_v1",
    causal_question = causal_question,
    estimand = estimand,
    roles = roles,
    relationships = relationships,
    variable_semantics = variable_semantics,
    temporal_ordering = temporal_ordering,
    assumptions = aq_vnext_unique_chr(assumptions),
    selection_mechanism = aq_vnext_default(selection_mechanism, NA_character_),
    measurement_mechanism = aq_vnext_default(measurement_mechanism, NA_character_),
    authority = aq_vnext_default(authority, causal_question$question$authority[[1]] %||% NA_character_),
    coverage = aq_vnext_default(coverage, causal_question$question$organizational_coverage[[1]] %||% NA_character_),
    candidate_designs = aq_vnext_default(candidate_designs, aq_causal_design_catalog()$design_id),
    created_at = aq_vnext_now()
  )
  out$graph_diagnostics <- aq_validate_causal_graph(out)
  out$adjustment_guidance <- aq_adjustment_guidance(out)
  out$identification <- aq_assess_identification(out)
  out$design_eligibility <- aq_assess_causal_designs(out)
  out$validation <- aq_validate_causal_context(out)
  class(out) <- c("aq_causal_context", "list")
  out
}

#' Validate a Causal Graph Structurally
#'
#' @param causal_context An `aq_causal_context`.
#' @export
aq_validate_causal_graph <- function(causal_context) {
  rows <- list()
  add <- function(check, status, message, severity = status, object_id = NA_character_, recommendation = NA_character_) {
    rows[[length(rows) + 1L]] <<- data.table::data.table(check = check, status = status, severity = severity, object_id = object_id, message = message, recommendation = recommendation)
  }
  roles <- causal_context$roles
  rel <- causal_context$relationships
  edges <- aq_causal_directed_edges(rel)
  q <- causal_context$causal_question$question[1]
  exposure <- aq_causal_chr1(q$exposure)
  outcome <- aq_causal_chr1(q$outcome)
  role_vars <- unique(roles$variable %||% character())
  nodes <- unique(c(role_vars, exposure, outcome))
  if (nrow(rel)) {
    unknown <- setdiff(unique(c(rel$source_variable, rel$destination_variable)), nodes)
    if (length(unknown)) add("unknown_node_reference", "fail", paste("Graph references unknown node(s):", paste(unknown, collapse = ", ")), "fail", recommendation = "Add role records for every graph node or remove invalid relationships.")
    duplicates <- rel[, .N, by = .(source_variable, destination_variable, relationship_type)][N > 1L]
    if (nrow(duplicates)) add("duplicate_edges", "fail", "Duplicate causal relationships exist.", "fail", recommendation = "Keep one active relationship per directed assumption.")
    self <- rel[source_variable == destination_variable]
    if (nrow(self)) add("self_loop", "fail", "Self-loop relationships are not valid causal graph assumptions.", "fail", paste(self$relationship_id, collapse = ", "))
  }
  if (nrow(edges) && aq_causal_has_cycle(edges)) add("cycle_detected", "fail", "Directed causal assumptions contain a cycle.", "fail", recommendation = "Retire or revise at least one directed edge.")
  if (nrow(edges) && !aq_causal_has_path(edges, exposure, outcome)) add("disconnected_exposure_outcome", "warning", "No directed causal path from exposure to outcome is asserted.", "warning", recommendation = "Clarify the assumed mechanism or mark the question as association-only.")
  timing <- if (nrow(roles)) stats::setNames(roles$timing, roles$variable) else character()
  exposure_timing <- aq_vnext_default(unname(timing[exposure][1L]), "")
  outcome_timing <- aq_vnext_default(unname(timing[outcome][1L]), "")
  if (exposure_timing %in% c("post_treatment", "future_unknown")) add("exposure_timing_concern", "fail", "Exposure is labeled as occurring after treatment/future unknown.", "fail")
  if (outcome_timing %in% c("baseline", "pre_treatment")) add("outcome_timing_concern", "fail", "Outcome is labeled baseline/pre-treatment.", "fail")
  mediators <- roles[role == "mediator_candidate"]
  if (nrow(mediators) && any(mediators$timing %in% c("baseline", "pre_treatment"), na.rm = TRUE)) add("mediator_before_treatment", "warning", "A mediator candidate is marked baseline/pre-treatment.", "warning", recommendation = "Review timing or role assignment.")
  instruments <- roles[role == "instrument_candidate", variable]
  for (z in instruments) {
    if (aq_causal_has_path(edges, z, outcome, blocked = exposure)) add("instrument_direct_path_to_outcome", "warning", paste("Instrument candidate has a directed path to outcome not through exposure:", z), "warning", z, "Do not use as an instrument without exclusion justification.")
  }
  if (outcome %in% roles[grepl("adjust", adjustment_eligibility %||% "", ignore.case = TRUE), variable]) add("outcome_adjustment", "fail", "Outcome is marked as an adjustment variable.", "fail", outcome)
  if (!length(rows)) add("graph_structurally_valid", "pass", "No structural graph blockers were detected.", "pass", recommendation = "Structural validity does not prove substantive correctness.")
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

#' Produce Question-Relative Adjustment Guidance
#'
#' @param causal_context An `aq_causal_context`.
#' @export
aq_adjustment_guidance <- function(causal_context) {
  roles <- causal_context$roles
  edges <- aq_causal_directed_edges(causal_context$relationships)
  q <- causal_context$causal_question$question[1]
  exposure <- aq_causal_chr1(q$exposure)
  outcome <- aq_causal_chr1(q$outcome)
  variables <- unique(c(roles$variable %||% character(), exposure, outcome, edges$from, edges$to))
  if (!length(variables)) return(data.table::data.table())
  ancestors_exposure <- aq_causal_ancestors(edges, exposure)
  ancestors_outcome <- aq_causal_ancestors(edges, outcome)
  descendants_exposure <- aq_causal_descendants(edges, exposure)
  descendants_colliders <- unique(unlist(lapply(aq_causal_role_vars(roles, "collider_candidate"), function(x) aq_causal_descendants(edges, x)), use.names = FALSE))
  guidance <- lapply(variables, function(variable) {
    current_variable <- variable
    var_roles <- roles[variable == current_variable, role]
    timing <- aq_vnext_default(roles[variable == current_variable, timing][1L], "unknown")
    decision <- "insufficient information"
    reason <- "No question-relative causal role has been confirmed."
    if (variable == exposure) {
      decision <- "do not adjust"
      reason <- "This is the exposure/intervention."
    } else if (variable == outcome) {
      decision <- "do not adjust"
      reason <- "This is the outcome."
    } else if ("collider_candidate" %in% var_roles || variable %in% descendants_colliders) {
      decision <- "collider risk"
      reason <- "Variable is a collider candidate or descendant of a collider."
    } else if ("mediator_candidate" %in% var_roles) {
      decision <- "mediator--depends on estimand"
      reason <- "Mediator adjustment changes the estimand from total effect toward direct effect."
    } else if ("instrument_candidate" %in% var_roles) {
      decision <- "instrument candidate--not ordinary adjustment"
      reason <- "Instrument candidates require separate assumptions and should not be routine controls."
    } else if (timing %in% c("post_treatment", "future_unknown") || variable %in% descendants_exposure) {
      decision <- "post-treatment risk"
      reason <- "Variable is post-treatment or downstream of exposure."
    } else if ("selection_variable" %in% var_roles) {
      decision <- "selection model candidate"
      reason <- "Selection variables may require design or selection modeling."
    } else if ("confounder_candidate" %in% var_roles || (variable %in% ancestors_exposure && variable %in% ancestors_outcome)) {
      decision <- "required adjustment candidate"
      reason <- "Variable is a candidate common cause of exposure and outcome."
    } else if ("prognostic_precision_variable" %in% var_roles || variable %in% ancestors_outcome) {
      decision <- "optional precision adjustment"
      reason <- "Variable appears prognostic for outcome without clear exposure ancestry."
    }
    data.table::data.table(variable = variable, roles = paste(unique(var_roles), collapse = ", "), timing = timing, adjustment_guidance = decision, reason = reason)
  })
  data.table::rbindlist(guidance, use.names = TRUE, fill = TRUE)
}

#' Assess Identification Plausibility
#'
#' @param causal_context An `aq_causal_context`.
#' @export
aq_assess_identification <- function(causal_context) {
  graph <- aq_validate_causal_graph(causal_context)
  guidance <- aq_adjustment_guidance(causal_context)
  q <- causal_context$causal_question$question[1]
  roles <- causal_context$roles
  assumptions <- aq_business_flat_chr(causal_context$assumptions)
  status <- "identifiable under stated assumptions"
  severity <- "warning"
  reasons <- character()
  add_reason <- function(x) reasons <<- unique(c(reasons, x))
  if (any(graph$status == "fail")) {
    status <- "conflicting causal structure"
    severity <- "fail"
    add_reason("Structural graph diagnostics contain failures.")
  }
  if (!nrow(roles) || !any(roles$role == "confounder_candidate")) {
    status <- if (identical(status, "conflicting causal structure")) status else "insufficient information"
    add_reason("No candidate confounder structure has been authored.")
  }
  if (any(guidance$adjustment_guidance %in% c("collider risk", "post-treatment risk"), na.rm = TRUE)) add_reason("Dangerous adjustment variables are present and must be excluded or justified.")
  if (!length(assumptions)) add_reason("No identification assumptions have been stated.")
  if (any(roles$role == "spillover_interference_variable", na.rm = TRUE)) {
    status <- if (identical(status, "conflicting causal structure")) status else "interference concern"
    add_reason("Potential spillover/interference variable is present.")
  }
  if (any(roles$role == "selection_variable", na.rm = TRUE)) {
    status <- if (status %in% c("conflicting causal structure", "interference concern")) status else "selection-bias concern"
    add_reason("Selection mechanism requires explicit handling.")
  }
  positive_randomization <- any(grepl("random", assumptions, ignore.case = TRUE)) &&
    !any(grepl("\\b(no|not|without)\\s+random", assumptions, ignore.case = TRUE))
  if (positive_randomization) {
    status <- if (identical(status, "conflicting causal structure")) status else "plausibly identifiable"
    severity <- if (identical(status, "plausibly identifiable")) "pass" else severity
    add_reason("Randomized assignment is asserted as evidence/assumption.")
  }
  if (status %in% c("insufficient information", "selection-bias concern", "interference concern") && !any(grepl("experiment|random", assumptions, ignore.case = TRUE))) {
    add_reason("Only associational analysis is supportable until design assumptions are strengthened.")
  }
  data.table::data.table(
    causal_question_id = causal_context$causal_question$causal_question_id,
    identification_status = status,
    severity = severity,
    reasons = paste(reasons, collapse = " | "),
    prohibited_claims = paste(c("Do not claim a causal effect estimate.", "Do not convert predictive importance into causal importance.", if (status != "plausibly identifiable") "Do not make definitive causal recommendations from current evidence."), collapse = " | "),
    recommended_next_step = if (status %in% c("plausibly identifiable", "identifiable under stated assumptions")) "Run diagnostics and consider a bounded estimator in a future phase." else "Clarify graph, assumptions, timing, overlap, or design; consider an experiment."
  )
}

#' Assess Causal Design Eligibility
#'
#' @param causal_context An `aq_causal_context`.
#' @export
aq_assess_causal_designs <- function(causal_context) {
  catalog <- aq_causal_design_catalog()
  roles <- causal_context$roles
  assumptions <- tolower(paste(aq_business_flat_chr(causal_context$assumptions), collapse = " "))
  q <- causal_context$causal_question$question[1]
  has_pre <- any(roles$timing %in% c("baseline", "pre_treatment"), na.rm = TRUE)
  has_instrument <- any(roles$role == "instrument_candidate", na.rm = TRUE)
  has_mediator <- any(roles$role == "mediator_candidate", na.rm = TRUE)
  has_modifier <- any(roles$role == "effect_modifier", na.rm = TRUE)
  randomized <- grepl("random", assumptions)
  pre_period <- grepl("pre.?period|pretrend|pre-trend|historical", assumptions)
  threshold <- grepl("threshold|cutoff|discontinuity", assumptions)
  staggered <- grepl("stagger|rollout|adoption", assumptions)
  geo <- grepl("geo|market|region", tolower(paste(q$population, q$unit_of_analysis, assumptions, collapse = " ")))
  catalog[, `:=`(eligible = "uncertain", observed_evidence = "", missing_evidence = required_evidence, major_validity_threats = "Requires assumption review.", recommended_priority = "medium")]
  catalog[design_id %in% c("randomized_ab", "cluster_randomization", "stratified_block_randomization", "factorial_design") & randomized, `:=`(eligible = "eligible", observed_evidence = "Randomized assignment asserted.", missing_evidence = "Design-specific sample size and compliance diagnostics.", major_validity_threats = "Noncompliance, attrition, interference.", recommended_priority = "high")]
  catalog[design_id == "geo_experiment" & geo, `:=`(eligible = "uncertain", observed_evidence = "Geography/market context present.", missing_evidence = "Market eligibility, spillover, power, and assignment plan.", major_validity_threats = "Spillovers and market imbalance.", recommended_priority = "high")]
  catalog[design_id %in% c("difference_in_differences", "event_study") & pre_period & staggered, `:=`(eligible = "eligible", observed_evidence = "Pre-period and staggered adoption are asserted.", missing_evidence = "Pre-trend diagnostics and comparison group.", major_validity_threats = "Parallel trends violation.", recommended_priority = "high")]
  catalog[design_id == "synthetic_control" & pre_period, `:=`(eligible = "uncertain", observed_evidence = "Historical pre-period asserted.", missing_evidence = "Donor pool and treated unit definition.", major_validity_threats = "Poor pre-period fit.", recommended_priority = "medium")]
  catalog[design_id == "interrupted_time_series" & pre_period, `:=`(eligible = "uncertain", observed_evidence = "Pre-period asserted.", missing_evidence = "Clear interruption and enough post observations.", major_validity_threats = "Concurrent shocks.", recommended_priority = "medium")]
  catalog[design_id == "regression_discontinuity" & threshold, `:=`(eligible = "eligible", observed_evidence = "Threshold/cutoff asserted.", missing_evidence = "Running variable, bandwidth, manipulation checks.", major_validity_threats = "Sorting around threshold.", recommended_priority = "high")]
  catalog[design_id == "instrumental_variables" & has_instrument, `:=`(eligible = "uncertain", observed_evidence = "Instrument candidate role exists.", missing_evidence = "Relevance, exclusion, and monotonicity evidence.", major_validity_threats = "Invalid exclusion restriction.", recommended_priority = "medium")]
  catalog[design_id %in% c("matching", "weighting", "doubly_robust") & has_pre, `:=`(eligible = "uncertain", observed_evidence = "Pre-treatment covariates exist.", missing_evidence = "Overlap and complete confounder set.", major_validity_threats = "Unmeasured confounding and poor overlap.", recommended_priority = "medium")]
  catalog[design_id == "mediation_analysis" & has_mediator, `:=`(eligible = "uncertain", observed_evidence = "Mediator candidate exists.", missing_evidence = "Direct-effect estimand and mediator timing.", major_validity_threats = "Mediator-outcome confounding.", recommended_priority = "low")]
  catalog[design_id == "heterogeneous_treatment_effects" & has_modifier, `:=`(eligible = "uncertain", observed_evidence = "Effect modifier exists.", missing_evidence = "Sufficient variation and pre-specified groups.", major_validity_threats = "Multiple testing and subgroup instability.", recommended_priority = "medium")]
  catalog[design_family == "longitudinal_policy", `:=`(eligible = "uncertain", missing_evidence = paste(required_evidence, "plus future estimator support"), major_validity_threats = "Time-varying confounding and policy feedback.", recommended_priority = "low")]
  catalog[!design_id %in% (causal_context$candidate_designs %||% catalog$design_id), eligible := "ineligible"]
  catalog[]
}

#' Validate a Causal Context
#'
#' @param causal_context An `aq_causal_context`.
#' @export
aq_validate_causal_context <- function(causal_context) {
  rows <- list()
  add <- function(check, status, message, severity = status, recommendation = NA_character_) {
    rows[[length(rows) + 1L]] <<- data.table::data.table(check = check, status = status, severity = severity, message = message, recommendation = recommendation)
  }
  if (!inherits(causal_context$causal_question, "aq_causal_question")) add("causal_question_link", "fail", "Causal context must link a causal question.")
  if (!inherits(causal_context$estimand, "aq_estimand")) add("estimand_link", "fail", "Causal context must link an estimand.")
  qv <- aq_validate_causal_question(causal_context$causal_question)
  ev <- aq_validate_estimand(causal_context$estimand)
  gv <- aq_validate_causal_graph(causal_context)
  if (any(qv$status == "fail")) add("causal_question_validity", "fail", "Causal question has blocking diagnostics.")
  if (any(ev$status == "fail")) add("estimand_validity", "fail", "Estimand has blocking diagnostics.")
  if (any(gv$status == "fail")) add("graph_validity", "fail", "Causal graph has structural failures.")
  roles <- causal_context$roles
  if (!nrow(roles)) add("roles_exist", "fail", "No question-relative variable roles have been assigned.", recommendation = "Assign exposure, outcome, and candidate causes before identification.")
  if (!any(roles$role == "exposure")) add("exposure_role", "warning", "No variable role is explicitly marked exposure.")
  if (!any(roles$role == "outcome")) add("outcome_role", "warning", "No variable role is explicitly marked outcome.")
  bad_roles <- setdiff(roles$role, aq_causal_role_levels())
  if (length(bad_roles)) add("unknown_roles", "warning", paste("Unknown causal role(s):", paste(bad_roles, collapse = ", ")))
  bad_timing <- setdiff(roles$timing, c(aq_causal_timing_levels(), NA_character_))
  if (length(bad_timing)) add("unknown_timing", "warning", paste("Unknown timing value(s):", paste(bad_timing, collapse = ", ")))
  if (!length(aq_business_flat_chr(causal_context$assumptions))) add("missing_assumptions", "warning", "No causal assumptions have been stated.")
  if (!length(rows)) add("causal_context_valid", "pass", "Causal context is structurally ready for identification planning.")
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

#' Plan the Smallest Useful Causal Investigation
#'
#' @param causal_context An `aq_causal_context`.
#' @export
aq_plan_causal_investigation <- function(causal_context) {
  identification <- aq_assess_identification(causal_context)
  designs <- aq_assess_causal_designs(causal_context)
  graph <- aq_validate_causal_graph(causal_context)
  adjustment <- aq_adjustment_guidance(causal_context)
  priority_designs <- designs[eligible %in% c("eligible", "uncertain")][order(factor(recommended_priority, levels = c("high", "medium", "low")))]
  selected <- utils::head(priority_designs$design_id, 3L)
  missing <- unique(c(
    if (any(graph$status == "fail")) "Repair structural graph failures.",
    if (any(adjustment$adjustment_guidance == "insufficient information")) "Resolve unknown variable roles and timing.",
    if (identical(identification$identification_status[[1]], "insufficient information")) "Author candidate confounders, assumptions, and evidence of treatment variation.",
    priority_designs$missing_evidence[seq_len(min(3L, nrow(priority_designs)))]
  ))
  plan <- list(
    plan_id = aq_vnext_id("causal_investigation_plan"),
    schema_version = "aq_causal_investigation_plan_v1",
    causal_question_id = causal_context$causal_question$causal_question_id,
    estimand_id = causal_context$estimand$estimand_id,
    identification_status = identification$identification_status[[1]],
    required_graph_clarification = graph[status != "pass"],
    mandatory_diagnostics = c("structural_graph_validation", "role_timing_review", "adjustment_safety_review", "identification_assessment", "design_eligibility_review"),
    eligible_methods = selected,
    mandatory_baselines = c("association_only_baseline", "descriptive_treatment_outcome_summary"),
    sensitivity_analyses = c("unmeasured_confounding_sensitivity", "overlap_sensitivity", "timing_sensitivity"),
    missing_evidence = missing,
    experiment_opportunity = any(designs[design_family == "experimental" & eligible %in% c("eligible", "uncertain"), .N] > 0L),
    prohibited_claims = aq_business_flat_chr(identification$prohibited_claims),
    stopping_conditions = c("causal question is precise", "estimand is explicit", "graph blockers are resolved", "identification status is no worse than identifiable under stated assumptions or experiment is recommended"),
    supported_next_actions = c("clarify_graph", "collect_missing_evidence", "design_experiment", "run_future_estimator_only_after_identification")
  )
  class(plan) <- c("aq_causal_investigation_plan", "list")
  plan
}

#' Create a Causal Planning Artifact
#'
#' @param causal_context An `aq_causal_context`.
#' @param plan Optional causal investigation plan.
#' @param artifact_id Optional artifact id.
#' @export
aq_causal_planning_artifact <- function(causal_context, plan = NULL, artifact_id = NULL) {
  plan <- aq_vnext_default(plan, aq_plan_causal_investigation(causal_context))
  artifact_id <- aq_vnext_default(artifact_id, aq_vnext_id("causal_planning_artifact"))
  artifact <- new_metadata_artifact(
    id = artifact_id,
    title = paste("Causal Planning:", causal_context$causal_question$question$causal_question_id[[1]]),
    description = "Causal question, estimand, role, graph, adjustment, identification, design eligibility, and investigation planning artifact. No effect is estimated.",
    tags = c("vnext", "causal_intelligence", "identification_planning"),
    dependencies = aq_vnext_unique_chr(c(causal_context$causal_question$causal_question_id, causal_context$estimand$estimand_id)),
    source_generator = "aq_causal_planning_artifact",
    version = "aq_causal_planning_artifact_v1",
    metadata = list(
      artifact_type = "causal_planning_artifact",
      causal_question_id = causal_context$causal_question$causal_question_id,
      estimand_id = causal_context$estimand$estimand_id,
      identification_status = causal_context$identification$identification_status[[1]],
      supported_actions = aq_causal_supported_actions(),
      no_effect_estimated = TRUE,
      prohibited_claims = plan$prohibited_claims
    )
  )
  artifact$causal_question <- causal_context$causal_question$question
  artifact$estimand <- causal_context$estimand
  artifact$roles <- causal_context$roles
  artifact$graph <- causal_context$relationships
  artifact$graph_diagnostics <- causal_context$graph_diagnostics
  artifact$adjustment_guidance <- causal_context$adjustment_guidance
  artifact$identification <- causal_context$identification
  artifact$eligible_designs <- causal_context$design_eligibility
  artifact$investigation_plan <- plan
  artifact$artifact_envelope <- aq_vnext_attach_envelope(
    artifact,
    artifact_id = artifact_id,
    artifact_type = "causal_planning_artifact",
    artifact_version = "aq_causal_planning_artifact_v1",
    parent_artifact_ids = aq_vnext_unique_chr(c(causal_context$causal_question$question$decision_context_id[[1]], causal_context$causal_question$causal_question_id, causal_context$estimand$estimand_id)),
    lineage = list(
      causal_question_id = causal_context$causal_question$causal_question_id,
      estimand_id = causal_context$estimand$estimand_id,
      decision_context_id = causal_context$causal_question$question$decision_context_id[[1]],
      no_effect_estimated = TRUE
    ),
    task = "causal_identification_planning",
    operator = "deterministic_causal_planner",
    engine = "none",
    specification_id = causal_context$causal_context_id,
    supported_actions = aq_causal_supported_actions(),
    producer = "aq_causal_planning_artifact"
  )
  artifact
}

qa_causal_intelligence_framework <- function() {
  rows <- list()
  add <- function(check, ok, message) {
    rows[[length(rows) + 1L]] <<- data.table::data.table(suite = "causal_intelligence_framework", check = check, status = if (isTRUE(ok)) "pass" else "fail", message = message)
  }
  semantics <- aq_variable_semantics(
    variables = c("tv_spend", "revenue", "market_size", "brand_search", "region", "assignment_score", "seasonality"),
    business_role = list(tv_spend = "tactic_lever", revenue = "measurement_kpi", market_size = "contextual_control"),
    operational_eligibility = list(tv_spend = "controllable"),
    causal_role = list(tv_spend = "exposure", revenue = "outcome", market_size = "confounder_candidate")
  )
  decision <- aq_decision_context(
    context = list(decision_context_id = "decision_tv_budget", decision_question = "Should eligible markets increase weekly TV spend next quarter?", objective_ids = "objective_growth"),
    alternatives = list(
      list(alternative_id = "current_policy", name = "Current TV spend", alternative_type = "do_nothing", baseline = TRUE),
      list(alternative_id = "increase_tv", name = "Increase TV spend within eligible markets", alternative_type = "pilot", baseline = FALSE, affected_levers = "tv_spend")
    ),
    variable_semantics = semantics
  )
  question <- aq_causal_question(
    question = list(
      causal_question_id = "cq_tv_revenue",
      decision_context_id = decision$decision_context_id,
      business_objective = "objective_growth",
      lever_id = "tv_spend",
      exposure = "tv_spend",
      exposure_representation = "weekly spend level",
      outcome = "revenue",
      outcome_representation = "weekly revenue",
      population = "eligible markets",
      unit_of_analysis = "market-week",
      time_zero = "campaign start",
      treatment_window = "next quarter",
      outcome_window = "same quarter and four-week lag",
      comparison_condition = "current weekly TV spend range",
      intervention_definition = "increase weekly TV spend from current market range to proposed eligible-market range",
      estimand = "ATE",
      effect_scale = "incremental revenue difference",
      target_population = "eligible markets"
    ),
    decision_context = decision
  )
  roles <- aq_causal_variable_roles(list(
    list(role_id = "role_x", causal_question_id = "cq_tv_revenue", variable = "tv_spend", role = "exposure", timing = "time_varying", role_confidence = 0.9),
    list(role_id = "role_y", causal_question_id = "cq_tv_revenue", variable = "revenue", role = "outcome", timing = "post_treatment", role_confidence = 0.9),
    list(role_id = "role_c", causal_question_id = "cq_tv_revenue", variable = "market_size", role = "confounder_candidate", timing = "baseline", role_confidence = 0.7),
    list(role_id = "role_m", causal_question_id = "cq_tv_revenue", variable = "brand_search", role = "mediator_candidate", timing = "post_treatment", role_confidence = 0.6),
    list(role_id = "role_z", causal_question_id = "cq_tv_revenue", variable = "assignment_score", role = "instrument_candidate", timing = "pre_treatment", role_confidence = 0.4),
    list(role_id = "role_s", causal_question_id = "cq_tv_revenue", variable = "region", role = "selection_variable", timing = "baseline", role_confidence = 0.5),
    list(role_id = "role_p", causal_question_id = "cq_tv_revenue", variable = "seasonality", role = "prognostic_precision_variable", timing = "pre_treatment", role_confidence = 0.8)
  ))
  relationships <- aq_causal_relationships(list(
    list(relationship_id = "edge_cx", causal_question_id = "cq_tv_revenue", source_variable = "market_size", destination_variable = "tv_spend", relationship_type = "causes", confidence = 0.7),
    list(relationship_id = "edge_cy", causal_question_id = "cq_tv_revenue", source_variable = "market_size", destination_variable = "revenue", relationship_type = "causes", confidence = 0.7),
    list(relationship_id = "edge_xy", causal_question_id = "cq_tv_revenue", source_variable = "tv_spend", destination_variable = "revenue", relationship_type = "may_cause", confidence = 0.5),
    list(relationship_id = "edge_xm", causal_question_id = "cq_tv_revenue", source_variable = "tv_spend", destination_variable = "brand_search", relationship_type = "causes", confidence = 0.5),
    list(relationship_id = "edge_my", causal_question_id = "cq_tv_revenue", source_variable = "brand_search", destination_variable = "revenue", relationship_type = "causes", confidence = 0.5)
  ))
  context <- aq_causal_context(
    causal_question = question,
    roles = roles,
    relationships = relationships,
    variable_semantics = semantics,
    assumptions = c("observational treatment variation exists", "historical pre-period is available", "no random assignment is currently asserted")
  )
  plan <- aq_plan_causal_investigation(context)
  artifact <- aq_causal_planning_artifact(context, plan)
  bad_cycle <- aq_causal_context(question, roles = roles, relationships = rbind(relationships, aq_causal_relationships(list(list(relationship_id = "edge_yc", causal_question_id = "cq_tv_revenue", source_variable = "revenue", destination_variable = "market_size", relationship_type = "causes")))), assumptions = "cycle test")
  add("causal_question", inherits(question, "aq_causal_question") && !any(question$validation$status == "fail"), "Precise causal question contract validates.")
  add("estimand_contract", inherits(question$estimand, "aq_estimand") && identical(question$estimand$implementation_status, "planning_only"), "Estimand is explicit and planning-only.")
  add("question_relative_roles", nrow(context$roles) >= 7L && all(c("confounder_candidate", "mediator_candidate", "instrument_candidate", "selection_variable", "prognostic_precision_variable") %in% context$roles$role), "Question-relative causal roles preserve uncertainty and multiple role families.")
  add("graph_diagnostics", any(bad_cycle$graph_diagnostics$check == "cycle_detected") && any(context$graph_diagnostics$check == "graph_structurally_valid"), "DAG diagnostics detect cycles and validate bounded acyclic graphs.")
  add("adjustment_guidance", all(c("required adjustment candidate", "mediator--depends on estimand", "instrument candidate--not ordinary adjustment") %in% context$adjustment_guidance$adjustment_guidance), "Adjustment guidance distinguishes confounders, mediators, and instruments.")
  add("identification_assessment", context$identification$identification_status[[1]] %in% c("identifiable under stated assumptions", "selection-bias concern", "insufficient information"), "Identification assessment remains deterministic and assumption-aware.")
  add("design_eligibility", all(c("difference_in_differences", "matching", "weighting", "doubly_robust") %in% context$design_eligibility$design_id), "Experimental and quasi-experimental design eligibility is represented.")
  add("investigation_plan", inherits(plan, "aq_causal_investigation_plan") && length(plan$prohibited_claims) > 0L && length(plan$supported_next_actions) > 0L, "Investigation plan recommends next actions and prohibited claims.")
  add("planning_artifact", inherits(artifact, "aq_artifact") && isTRUE(artifact$metadata$no_effect_estimated), "Causal planning artifact uses canonical artifact envelope and makes no effect-estimation claim.")
  add("predictive_not_causal", !any(grepl("feature importance", unlist(context), ignore.case = TRUE)), "Framework does not convert predictive importance into causal importance.")
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}
