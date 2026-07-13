# AutoQuant canonical variable semantics framework.

aq_variable_semantic_taxonomy <- function() {
  data.table::rbindlist(list(
    data.table::data.table(
      dimension = "business_role",
      semantic_type = c("objective_metric", "strategy_indicator", "tactic_lever", "business_lever", "operational_driver", "contextual_control", "system_state", "constraint", "guardrail", "risk_indicator", "cost", "utility", "measurement_kpi", "unknown"),
      description = c(
        "Metric directly tied to a business objective.",
        "Variable indicating strategy state, execution, or alignment.",
        "Variable representing a tactic or tactical lever.",
        "Variable representing a lever that can affect business outcomes.",
        "Operational driver with business interpretation.",
        "Contextual variable used to interpret or control business evidence.",
        "Variable describing business or operational system state.",
        "Business or operational constraint.",
        "Metric or condition used as a guardrail.",
        "Variable representing risk, risk exposure, or risk state.",
        "Cost variable.",
        "Utility, value, payoff, or preference variable.",
        "Measurement or KPI variable.",
        "Business role is unknown."
      )
    ),
    data.table::data.table(
      dimension = "operational_eligibility",
      semantic_type = c("controllable", "non_controllable", "optimization_eligible", "scenario_only", "experiment_eligible", "reporting_only", "adjustment_only", "unknown"),
      description = c(
        "Variable can be controlled by the decision maker.",
        "Variable cannot be controlled by the decision maker.",
        "Variable may be changed by an optimizer or recommendation system.",
        "Variable is only used for scenarios.",
        "Variable can be deliberately tested in an experiment.",
        "Variable is for reporting, not intervention.",
        "Variable can be adjusted for, but should not be treated as a lever.",
        "Operational eligibility is unknown."
      )
    ),
    data.table::data.table(
      dimension = "analytical_role",
      semantic_type = c("target", "predictor", "known_future_regressor", "derived_feature", "identifier", "hierarchy_variable", "entity_variable", "unknown"),
      description = c(
        "Analytical target.",
        "Predictive or explanatory input.",
        "Regressor known in future scoring or forecasting periods.",
        "Feature derived from one or more source variables.",
        "Stable row, entity, or record identity.",
        "Hierarchy or aggregation variable.",
        "Panel or entity variable.",
        "Analytical role is unknown."
      )
    ),
    data.table::data.table(
      dimension = "causal_role",
      semantic_type = c("exposure", "outcome", "confounder_candidate", "mediator_candidate", "collider_candidate", "moderator", "instrument_candidate", "selection_variable", "unknown"),
      description = c(
        "Treatment, intervention, exposure, or decision variable.",
        "Causal outcome.",
        "Candidate confounder requiring validation.",
        "Candidate mediator requiring validation.",
        "Candidate collider requiring validation.",
        "Variable modifying the effect of exposure.",
        "Candidate instrument requiring validation.",
        "Variable governing sample selection.",
        "Causal role is unknown."
      )
    ),
    data.table::data.table(
      dimension = "operational",
      semantic_type = c("outcome", "driver", "control", "constraint", "state", "identifier", "scenario_variable", "optimization_candidate", "non_actionable", "candidate_driver", "unknown"),
      description = c(
        "Business or analytical outcome.",
        "Variable believed to influence an outcome.",
        "Variable used to control, adjust, stratify, or condition an analysis.",
        "Variable representing an operational boundary or hard limit.",
        "Variable describing current system state.",
        "Stable row, entity, or record identity.",
        "Variable used for scenarios or what-if analysis.",
        "Variable eligible for optimization or recommendation.",
        "Variable not actionable by the decision maker.",
        "Possible driver requiring validation.",
        "Operational role is unknown."
      )
    ),
    data.table::data.table(
      dimension = "temporal",
      semantic_type = c("baseline", "time_varying", "future_known", "future_unknown", "lagged", "post_treatment", "unknown"),
      description = c(
        "Baseline or pre-period value.",
        "Variable changes over time.",
        "Future values are known at prediction or forecast time.",
        "Future values are not known at prediction or forecast time.",
        "Variable is a lagged value.",
        "Variable may occur after treatment, exposure, or decision.",
        "Temporal role is unknown."
      )
    ),
    data.table::data.table(
      dimension = "causal",
      semantic_type = c("exposure", "outcome", "mediator", "confounder", "candidate_confounder", "collider", "instrument", "moderator", "selection_variable", "proxy", "latent_construct", "possible_confounder", "unknown_causal_role"),
      description = c(
        "Treatment, intervention, exposure, or decision variable.",
        "Causal outcome.",
        "Variable on a possible causal pathway.",
        "Common cause of exposure and outcome.",
        "Potential confounder requiring validation.",
        "Common effect that may induce bias if conditioned on.",
        "Variable affecting exposure but not outcome except through exposure.",
        "Variable modifying the effect of exposure.",
        "Variable governing sample selection.",
        "Observed proxy for another construct.",
        "Unobserved construct represented indirectly.",
        "Possible confounder requiring validation.",
        "Causal role is unknown."
      )
    ),
    data.table::data.table(
      dimension = "forecasting",
      semantic_type = c("target", "shared_predictor", "target_specific_predictor", "known_future_regressor", "intermittent_demand_feature", "funnel_stage", "cross_target_feature", "hierarchy_variable", "entity_variable", "unknown"),
      description = c(
        "Forecast target.",
        "Predictor shared across targets.",
        "Predictor tied to a specific target.",
        "Regressor known for future periods.",
        "Feature relevant to intermittent-demand behavior.",
        "Funnel stage variable.",
        "Feature derived from another target's prior history.",
        "Hierarchy or aggregation variable.",
        "Panel or entity variable.",
        "Forecasting role is unknown."
      )
    ),
    data.table::data.table(
      dimension = "measurement",
      semantic_type = c("observed", "derived", "engineered", "aggregated", "external", "estimated", "scenario", "missingness_indicator", "unknown"),
      description = c(
        "Directly observed field.",
        "Derived from other variables.",
        "Engineered feature.",
        "Aggregate or rolled-up measure.",
        "External data source.",
        "Estimated rather than directly observed.",
        "Scenario input or simulated value.",
        "Indicator of missingness.",
        "Measurement role is unknown."
      )
    ),
    data.table::data.table(
      dimension = "decision",
      semantic_type = c("optimization_eligible", "optimization_prohibited", "experiment_candidate", "guardrail", "primary_kpi", "secondary_kpi", "business_constraint", "unknown"),
      description = c(
        "Variable may be changed by an optimizer or recommendation system.",
        "Variable must not be changed by optimization.",
        "Variable is a candidate for experiment or intervention.",
        "Guardrail metric or condition.",
        "Primary decision metric.",
        "Secondary decision metric.",
        "Business constraint.",
        "Decision role is unknown."
      )
    )
  ), use.names = TRUE, fill = TRUE)
}

aq_variable_semantics_supported_actions <- function() {
  c(
    "validate_semantics",
    "feature_engineering",
    "modeling",
    "forecasting",
    "report",
    "campaign_review",
    "experiment_planning",
    "optimization_planning",
    "causal_planning",
    "knowledge_promotion"
  )
}

aq_variable_semantics_dimension_levels <- function() {
  unique(aq_variable_semantic_taxonomy()$dimension)
}

aq_variable_semantics_normalize_dimension <- function(values, variables) {
  variables <- aq_vnext_unique_chr(variables)
  empty <- stats::setNames(vector("list", length(variables)), variables)
  if (is.null(values)) return(empty)
  if (is.data.frame(values)) {
    dt <- data.table::as.data.table(values)
    if (!all(c("variable", "semantic_type") %in% names(dt))) {
      stop("dimension data.frames must include variable and semantic_type columns.", call. = FALSE)
    }
    out <- empty
    for (var in variables) out[[var]] <- aq_vnext_unique_chr(dt[variable == var, semantic_type])
    return(out)
  }
  if (is.list(values) && !is.data.frame(values)) {
    out <- empty
    if (is.null(names(values)) || !any(nzchar(names(values)))) {
      if (length(variables) == 1L) {
        out[[variables[1L]]] <- aq_vnext_unique_chr(unlist(values, use.names = FALSE))
        return(out)
      }
      stop("unnamed list semantic dimensions are only supported for one variable.", call. = FALSE)
    }
    for (var in intersect(names(values), variables)) {
      out[[var]] <- aq_vnext_unique_chr(unlist(values[[var]], use.names = FALSE))
    }
    return(out)
  }
  values <- aq_vnext_unique_chr(values)
  stats::setNames(rep(list(values), length(variables)), variables)
}

aq_variable_semantics_confidence <- function(confidence, variables) {
  variables <- aq_vnext_unique_chr(variables)
  if (is.null(confidence)) confidence <- 1
  if (is.list(confidence) && !is.null(names(confidence))) {
    out <- vapply(variables, function(var) as.numeric(aq_vnext_default(confidence[[var]], 1))[1L], numeric(1L))
  } else {
    confidence <- as.numeric(confidence)
    if (length(confidence) == 1L) confidence <- rep(confidence, length(variables))
    out <- stats::setNames(confidence[seq_along(variables)], variables)
  }
  out[!is.finite(out)] <- NA_real_
  out
}

aq_variable_semantics_evidence <- function(evidence_source, variables) {
  variables <- aq_vnext_unique_chr(variables)
  if (is.null(evidence_source)) evidence_source <- "human_annotation"
  if (is.list(evidence_source) && !is.null(names(evidence_source))) {
    out <- vapply(variables, function(var) as.character(aq_vnext_default(evidence_source[[var]], "human_annotation"))[1L], character(1L))
  } else {
    evidence_source <- as.character(evidence_source)
    if (length(evidence_source) == 1L) evidence_source <- rep(evidence_source, length(variables))
    out <- stats::setNames(evidence_source[seq_along(variables)], variables)
  }
  out
}

aq_variable_semantics_make_mappings <- function(variables, dimensions, confidence, evidence_source) {
  rows <- list()
  for (dimension in names(dimensions)) {
    assignments <- dimensions[[dimension]]
    for (variable in names(assignments)) {
      semantic_types <- aq_vnext_unique_chr(assignments[[variable]])
      if (!length(semantic_types)) next
      rows[[length(rows) + 1L]] <- data.table::data.table(
        variable = variable,
        dimension = dimension,
        semantic_type = semantic_types,
        confidence = as.numeric(confidence[[variable]])[1L],
        evidence_source = as.character(evidence_source[[variable]])[1L]
      )
    }
  }
  if (!length(rows)) {
    return(data.table::data.table(variable = character(), dimension = character(), semantic_type = character(), confidence = numeric(), evidence_source = character()))
  }
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

#' Create Canonical Variable Semantics
#'
#' @description
#' Creates a typed semantic object describing what variables mean across
#' business-role, operational-eligibility, analytical-role, causal-role,
#' operational, temporal, causal, forecasting, measurement, and decision
#' dimensions. Variables may have multiple semantic assignments and uncertainty
#' is represented through confidence and candidate/unknown semantic types.
#'
#' @param variables Character vector of variable names.
#' @param operational Operational semantic assignments.
#' @param temporal Temporal semantic assignments.
#' @param causal Causal semantic assignments.
#' @param forecasting Forecasting semantic assignments.
#' @param measurement Measurement semantic assignments.
#' @param decision Decision semantic assignments.
#' @param business_role Business-role semantic assignments.
#' @param operational_eligibility Operational-eligibility semantic assignments.
#' @param analytical_role Analytical-role semantic assignments.
#' @param causal_role Context-relative causal-role semantic assignments.
#' @param confidence Scalar, named vector, or named list of confidence values.
#' @param evidence_source Scalar, named vector, or named list of evidence
#'   sources.
#' @param human_annotations Optional human annotation list.
#' @param llm_suggestions Optional LLM suggestion list.
#' @param dataset_id Optional dataset id.
#' @param business_context_refs Optional references to future business-intent
#'   artifacts. This phase records ids only; it does not define those schemas.
#' @param semantics_id Optional semantic object id.
#' @param supported_downstream_actions Supported downstream actions.
#'
#' @return An `aq_variable_semantics` object.
#' @export
aq_variable_semantics <- function(
  variables,
  operational = NULL,
  temporal = NULL,
  causal = NULL,
  forecasting = NULL,
  measurement = NULL,
  decision = NULL,
  business_role = NULL,
  operational_eligibility = NULL,
  analytical_role = NULL,
  causal_role = NULL,
  confidence = 1,
  evidence_source = "human_annotation",
  human_annotations = list(),
  llm_suggestions = list(),
  dataset_id = NULL,
  business_context_refs = list(),
  semantics_id = NULL,
  supported_downstream_actions = aq_variable_semantics_supported_actions()
) {
  variables <- aq_vnext_unique_chr(variables)
  if (!length(variables)) stop("variables must contain at least one variable name.", call. = FALSE)
  confidence <- aq_variable_semantics_confidence(confidence, variables)
  evidence_source <- aq_variable_semantics_evidence(evidence_source, variables)
  dimensions <- list(
    operational = aq_variable_semantics_normalize_dimension(operational, variables),
    temporal = aq_variable_semantics_normalize_dimension(temporal, variables),
    causal = aq_variable_semantics_normalize_dimension(causal, variables),
    forecasting = aq_variable_semantics_normalize_dimension(forecasting, variables),
    measurement = aq_variable_semantics_normalize_dimension(measurement, variables),
    decision = aq_variable_semantics_normalize_dimension(decision, variables),
    business_role = aq_variable_semantics_normalize_dimension(business_role, variables),
    operational_eligibility = aq_variable_semantics_normalize_dimension(operational_eligibility, variables),
    analytical_role = aq_variable_semantics_normalize_dimension(analytical_role, variables),
    causal_role = aq_variable_semantics_normalize_dimension(causal_role, variables)
  )
  mappings <- aq_variable_semantics_make_mappings(variables, dimensions, confidence, evidence_source)
  semantics <- list(
    semantics_id = aq_vnext_default(semantics_id, aq_vnext_id("variable_semantics")),
    schema_version = "aq_variable_semantics_v1",
    dataset_id = aq_vnext_default(dataset_id, NA_character_),
    variables = variables,
    mappings = mappings,
    taxonomy = aq_variable_semantic_taxonomy(),
    confidence = confidence,
    evidence_source = evidence_source,
    human_annotations = human_annotations,
    llm_suggestions = llm_suggestions,
    business_context_refs = business_context_refs,
    supported_downstream_actions = aq_vnext_unique_chr(supported_downstream_actions),
    created_at = aq_vnext_now()
  )
  semantics$validation <- aq_validate_variable_semantics(semantics)
  class(semantics) <- c("aq_variable_semantics", "list")
  semantics
}

aq_variable_semantics_validation_row <- function(check, status, message, variable = NA_character_, dimension = NA_character_, semantic_type = NA_character_, severity = status) {
  data.table::data.table(
    check = check,
    status = status,
    severity = severity,
    variable = variable,
    dimension = dimension,
    semantic_type = semantic_type,
    message = message
  )
}

aq_variable_semantics_add_pair_diagnostics <- function(rows, mappings, dimension, a, b, check, status, message) {
  dimension_name <- as.character(dimension)[1L]
  vars <- mappings[dimension == dimension_name & semantic_type %in% c(a, b), .N, by = variable][N >= 2L, variable]
  for (var in vars) {
    rows[[length(rows) + 1L]] <- aq_variable_semantics_validation_row(check, status, message, var, dimension, paste(a, b, sep = " + "), status)
  }
  rows
}

#' Validate Canonical Variable Semantics
#'
#' @param semantics An `aq_variable_semantics` object.
#'
#' @return A data.table of validation diagnostics.
#' @export
aq_validate_variable_semantics <- function(semantics) {
  rows <- list()
  add <- function(check, status, message, variable = NA_character_, dimension = NA_character_, semantic_type = NA_character_, severity = status) {
    rows[[length(rows) + 1L]] <<- aq_variable_semantics_validation_row(check, status, message, variable, dimension, semantic_type, severity)
  }
  if (!is.list(semantics) || is.null(semantics$mappings)) {
    add("semantics_object", "fail", "semantics must be created by aq_variable_semantics().")
    return(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
  }
  mappings <- data.table::as.data.table(semantics$mappings)
  taxonomy <- aq_variable_semantic_taxonomy()
  variables <- aq_vnext_unique_chr(semantics$variables)
  add("semantics_object", "pass", "semantic object is structured.", severity = "info")
  if (!length(variables)) {
    add("variables", "fail", "at least one variable is required.")
  } else {
    add("variables", "pass", paste("variables:", length(variables)), severity = "info")
  }
  required_cols <- c("variable", "dimension", "semantic_type", "confidence", "evidence_source")
  missing_cols <- setdiff(required_cols, names(mappings))
  if (length(missing_cols)) {
    add("mapping_columns", "fail", paste("missing mapping columns:", paste(missing_cols, collapse = ", ")))
    return(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
  }
  invalid_refs <- setdiff(mappings$variable, variables)
  if (length(invalid_refs)) {
    for (var in invalid_refs) add("invalid_variable_reference", "fail", "mapping references variable not declared in variables.", var)
  } else {
    add("invalid_variable_reference", "pass", "all mappings reference declared variables.", severity = "info")
  }
  invalid_dimensions <- setdiff(mappings$dimension, taxonomy$dimension)
  if (length(invalid_dimensions)) {
    for (dimension in invalid_dimensions) add("unknown_dimension", "fail", "unknown semantic dimension.", dimension = dimension)
  } else {
    add("known_dimensions", "pass", "all semantic dimensions are known.", severity = "info")
  }
  known_pairs <- unique(taxonomy[, .(dimension, semantic_type)])
  invalid_types <- mappings[!known_pairs, on = c("dimension", "semantic_type")]
  if (nrow(invalid_types)) {
    for (i in seq_len(nrow(invalid_types))) add("unknown_semantic_type", "fail", "unknown semantic type for dimension.", invalid_types$variable[i], invalid_types$dimension[i], invalid_types$semantic_type[i])
  } else {
    add("known_semantic_types", "pass", "all semantic types are in the canonical taxonomy.", severity = "info")
  }
  duplicates <- mappings[, .N, by = .(variable, dimension, semantic_type)][N > 1L]
  if (nrow(duplicates)) {
    for (i in seq_len(nrow(duplicates))) add("duplicate_semantic_assignment", "fail", "duplicate semantic assignment.", duplicates$variable[i], duplicates$dimension[i], duplicates$semantic_type[i])
  } else {
    add("duplicate_semantic_assignment", "pass", "no duplicate semantic assignments.", severity = "info")
  }
  bad_conf <- mappings[!is.finite(confidence) | confidence < 0 | confidence > 1]
  if (nrow(bad_conf)) {
    for (i in seq_len(nrow(bad_conf))) add("confidence_range", "fail", "confidence must be between 0 and 1.", bad_conf$variable[i], bad_conf$dimension[i], bad_conf$semantic_type[i])
  } else {
    add("confidence_range", "pass", "confidence values are between 0 and 1.", severity = "info")
  }
  missing_evidence <- mappings[is.na(evidence_source) | !nzchar(evidence_source)]
  if (nrow(missing_evidence)) {
    for (i in seq_len(nrow(missing_evidence))) add("evidence_source", "warning", "semantic assignment is missing evidence source.", missing_evidence$variable[i], missing_evidence$dimension[i], missing_evidence$semantic_type[i], "warning")
  } else {
    add("evidence_source", "pass", "semantic assignments include evidence source.", severity = "info")
  }
  rows <- aq_variable_semantics_add_pair_diagnostics(rows, mappings, "temporal", "future_known", "future_unknown", "contradictory_temporal_semantics", "fail", "variable cannot be both future_known and future_unknown in the same semantic context.")
  rows <- aq_variable_semantics_add_pair_diagnostics(rows, mappings, "decision", "optimization_eligible", "optimization_prohibited", "contradictory_decision_semantics", "fail", "variable cannot be both optimization eligible and optimization prohibited.")
  rows <- aq_variable_semantics_add_pair_diagnostics(rows, mappings, "operational_eligibility", "controllable", "non_controllable", "contradictory_operational_eligibility", "fail", "variable cannot be both controllable and non-controllable in the same decision context.")
  rows <- aq_variable_semantics_add_pair_diagnostics(rows, mappings, "operational_eligibility", "optimization_eligible", "reporting_only", "contradictory_optimization_reporting_semantics", "warning", "variable is both optimization eligible and reporting only; review context.")
  rows <- aq_variable_semantics_add_pair_diagnostics(rows, mappings, "operational", "optimization_candidate", "non_actionable", "contradictory_operational_semantics", "warning", "variable is both optimization candidate and non-actionable; review decision context.")
  rows <- aq_variable_semantics_add_pair_diagnostics(rows, mappings, "forecasting", "target", "known_future_regressor", "contradictory_forecasting_semantics", "warning", "forecast target is also marked known future regressor; review leakage risk.")
  rows <- aq_variable_semantics_add_pair_diagnostics(rows, mappings, "analytical_role", "target", "known_future_regressor", "contradictory_analytical_role_semantics", "warning", "analytical target is also marked known future regressor; review leakage risk.")
  if (!length(rows)) {
    add("validation", "pass", "semantic validation completed.", severity = "info")
  }
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

#' Create a Variable Semantics Artifact
#'
#' @param semantics An `aq_variable_semantics` object.
#' @param artifact_id Optional artifact id.
#'
#' @return A canonical `variable_semantics_artifact`.
#' @export
aq_variable_semantics_artifact <- function(semantics, artifact_id = NULL) {
  if (!inherits(semantics, "aq_variable_semantics")) {
    stop("semantics must be created by aq_variable_semantics().", call. = FALSE)
  }
  validation <- aq_validate_variable_semantics(semantics)
  warnings <- validation[status %in% c("warning", "fail", "error"), message]
  artifact_id <- aq_vnext_default(artifact_id, aq_vnext_id("variable_semantics_artifact"))
  artifact <- new_table_artifact(
    id = artifact_id,
    title = "Variable Semantics",
    data = semantics$mappings,
    source_generator = "aq_variable_semantics_artifact",
    tags = c("vnext", "semantics", "variables"),
    dependencies = character(),
    version = "aq_variable_semantics_artifact_v1",
    metadata = list(
      artifact_type = "variable_semantics_artifact",
      semantics_id = semantics$semantics_id,
      schema_version = semantics$schema_version,
      variables = semantics$variables,
      taxonomy = semantics$taxonomy,
      validation = validation,
      confidence = semantics$confidence,
      evidence_source = semantics$evidence_source,
      human_annotations = semantics$human_annotations,
      llm_suggestions = semantics$llm_suggestions,
      business_context_refs = semantics$business_context_refs,
      warnings = warnings,
      supported_actions = semantics$supported_downstream_actions
    )
  )
  artifact <- aq_vnext_attach_envelope(
    artifact,
    artifact_id = artifact_id,
    artifact_type = "variable_semantics_artifact",
    artifact_version = "aq_variable_semantics_artifact_v1",
    parent_artifact_ids = character(),
    lineage = list(
      semantics_id = semantics$semantics_id,
      dataset_id = semantics$dataset_id,
      business_context_refs = semantics$business_context_refs,
      variables = semantics$variables,
      validation_status = if (any(validation$status %in% c("fail", "error"))) "has_failures" else if (any(validation$status == "warning")) "has_warnings" else "pass"
    ),
    task = "variable_semantics",
    operator = "semantic_specification",
    engine = "deterministic_semantic_validator",
    specification_id = semantics$semantics_id,
    dataset_id = semantics$dataset_id,
    warnings = warnings,
    supported_actions = semantics$supported_downstream_actions,
    producer = "aq_variable_semantics_artifact"
  )
  artifact
}

#' QA for Canonical Variable Semantics
#'
#' @return A data.table of deterministic QA checks.
#' @export
qa_variable_semantics_framework <- function() {
  rows <- list()
  add <- function(check, ok, message = "") {
    rows[[length(rows) + 1L]] <<- data.table::data.table(
      suite = "variable_semantics_framework",
      check = check,
      status = if (isTRUE(ok)) "pass" else "fail",
      message = message
    )
  }
  semantics <- aq_variable_semantics(
    variables = c("revenue", "spend", "region", "promo", "inventory_cap"),
    operational = list(revenue = "outcome", spend = c("driver", "optimization_candidate"), region = "control", promo = "scenario_variable", inventory_cap = "constraint"),
    temporal = list(revenue = "future_unknown", spend = "time_varying", promo = "future_known"),
    causal = list(spend = c("exposure", "candidate_confounder"), revenue = "outcome", region = "possible_confounder"),
    forecasting = list(revenue = "target", spend = "shared_predictor", promo = "known_future_regressor", region = "entity_variable"),
    measurement = list(revenue = "observed", spend = "observed", promo = "external", inventory_cap = "scenario"),
    decision = list(revenue = "primary_kpi", spend = "optimization_eligible", inventory_cap = "business_constraint"),
    business_role = list(revenue = c("objective_metric", "measurement_kpi"), spend = c("tactic_lever", "business_lever"), region = "contextual_control", promo = "strategy_indicator", inventory_cap = c("constraint", "guardrail", "risk_indicator")),
    operational_eligibility = list(spend = c("controllable", "optimization_eligible", "experiment_eligible"), promo = "scenario_only", region = "adjustment_only", inventory_cap = "non_controllable"),
    analytical_role = list(revenue = "target", spend = "predictor", promo = "known_future_regressor", region = "entity_variable", inventory_cap = "predictor"),
    causal_role = list(spend = "exposure", revenue = "outcome", region = "confounder_candidate", promo = "moderator"),
    confidence = list(revenue = 0.95, spend = 0.8, region = 0.7, promo = 0.9, inventory_cap = 0.85),
    evidence_source = "human_annotation",
    human_annotations = list(author = "qa"),
    llm_suggestions = list(status = "not_used"),
    business_context_refs = list(
      mission_id = "future_mission_qa",
      business_objective_id = "future_objective_revenue_growth",
      strategy_id = "future_strategy_media_efficiency",
      tactic_id = "future_tactic_spend_allocation",
      lever_id = "future_lever_paid_media_spend",
      risk_id = "future_risk_inventory_capacity",
      decision_id = "future_decision_budget_shift"
    ),
    dataset_id = "qa_semantics"
  )
  add("semantic_object", inherits(semantics, "aq_variable_semantics") && nrow(semantics$mappings) > 0L)
  add("taxonomy_dimensions", all(c("business_role", "operational_eligibility", "analytical_role", "causal_role", "operational", "temporal", "causal", "forecasting", "measurement", "decision") %in% semantics$taxonomy$dimension))
  add("business_context_refs", all(c("mission_id", "business_objective_id", "strategy_id", "tactic_id", "lever_id", "risk_id", "decision_id") %in% names(semantics$business_context_refs)))
  validation <- aq_validate_variable_semantics(semantics)
  add("validation_passes", !any(validation$status %in% c("fail", "error")))
  add("confidence_recorded", all(semantics$mappings$confidence >= 0 & semantics$mappings$confidence <= 1))
  artifact <- aq_variable_semantics_artifact(semantics)
  add("artifact_constructed", inherits(artifact, "aq_artifact") && identical(artifact$metadata$artifact_type, "variable_semantics_artifact"))
  add("artifact_integrity", !aq_vnext_has_validation_error(aq_validate_artifact(artifact)) && identical(aq_artifact_envelope(artifact)$operator, "semantic_specification"))
  bad <- aq_variable_semantics(
    variables = "x",
    temporal = list(x = c("future_known", "future_unknown")),
    decision = list(x = c("optimization_eligible", "optimization_prohibited")),
    operational_eligibility = list(x = c("controllable", "non_controllable")),
    confidence = 1,
    evidence_source = "qa_negative_case"
  )
  bad_validation <- aq_validate_variable_semantics(bad)
  add("contradictions_detected", all(c("contradictory_temporal_semantics", "contradictory_decision_semantics", "contradictory_operational_eligibility") %in% bad_validation$check))
  duplicate <- data.table::copy(semantics)
  duplicate$mappings <- data.table::rbindlist(list(semantics$mappings, semantics$mappings[1]), use.names = TRUE, fill = TRUE)
  add("duplicates_detected", any(aq_validate_variable_semantics(duplicate)$check == "duplicate_semantic_assignment"))
  unknown <- data.table::copy(semantics)
  unknown$mappings <- data.table::rbindlist(list(semantics$mappings, data.table::data.table(variable = "spend", dimension = "causal", semantic_type = "definitely_magic", confidence = 0.2, evidence_source = "qa")), use.names = TRUE, fill = TRUE)
  add("unknown_types_detected", any(aq_validate_variable_semantics(unknown)$check == "unknown_semantic_type"))
  add("analytics_shinyapp_compatibility", inherits(artifact, "aq_artifact") && !is.null(artifact$artifact_envelope) && "campaign_review" %in% aq_supported_actions(artifact))
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}
