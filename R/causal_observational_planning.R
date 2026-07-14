# AutoQuant observational causal-study planning and readiness contracts.

aq_observational_assignment_types <- function() {
  c(
    "deterministic_rule", "eligibility_threshold", "discretionary_assignment",
    "self_selection", "resource_constrained_allocation", "geographic_rollout",
    "staggered_adoption", "historical_policy", "provider_preference",
    "customer_choice", "unknown"
  )
}

aq_observational_readiness_states <- function() {
  c(
    "ready_for_design_implementation", "ready_with_strong_assumptions",
    "overlap_remediation_required", "adjustment_set_unresolved",
    "treatment_timing_unresolved", "selection_review_required",
    "sensitivity_plan_required", "descriptive_only", "experiment_preferred",
    "blocked", "unidentified", "insufficient_information"
  )
}

aq_observational_chr <- function(x, default = NA_character_) {
  if (is.null(x) || !length(x) || is.na(x[[1]])) return(default)
  value <- as.character(x[[1]])
  if (!nzchar(value)) default else value
}

aq_observational_num <- function(x, default = NA_real_) {
  value <- suppressWarnings(as.numeric(x))[1L]
  if (!is.finite(value)) default else value
}

aq_observational_vec <- function(x) {
  if (is.null(x)) return(character())
  if (length(x) == 1L && is.character(x) && grepl(",", x, fixed = TRUE)) {
    x <- strsplit(x, ",", fixed = TRUE)[[1L]]
  }
  aq_vnext_unique_chr(trimws(as.character(x[nzchar(as.character(x))])))
}

aq_observational_add_row <- function(rows, check, status, message, severity = status, recommendation = NA_character_) {
  rows[[length(rows) + 1L]] <- data.table::data.table(
    check = check,
    status = status,
    severity = severity,
    message = message,
    recommendation = recommendation
  )
  rows
}

aq_observational_record <- function(x, id_col, object_type) {
  if (is.null(x)) x <- data.table::data.table()
  out <- data.table::as.data.table(x)
  if (!id_col %in% names(out)) {
    if (!nrow(out)) out[, (id_col) := character()] else out[, (id_col) := aq_vnext_id(object_type)]
  }
  out
}

#' Create an Observational Causal Study Contract
#'
#' @param study Study record.
#' @param causal_question Optional causal question object.
#' @param estimand Optional estimand object.
#' @param study_id Optional ID override.
#'
#' @return An `aq_observational_study` object.
#' @export
aq_observational_study <- function(study, causal_question = NULL, estimand = NULL, study_id = NULL) {
  study <- aq_observational_record(study, "observational_study_id", "observational_study")
  if (nrow(study) != 1L) stop("study must describe exactly one observational study.", call. = FALSE)
  if (!is.null(study_id)) study[, observational_study_id := as.character(study_id)[1L]]
  required <- c(
    "decision_context_id", "causal_question_id", "estimand_id", "study_title",
    "treatment", "comparison_condition", "unit_of_analysis", "population",
    "eligibility", "treatment_assignment_time", "treatment_window",
    "outcome_window", "baseline_window", "index_date", "data_cutoff",
    "organizational_scope", "authority", "coverage", "status"
  )
  for (col in setdiff(required, names(study))) study[, (col) := NA_character_]
  for (col in c("treatment_levels", "evidence_references", "supported_actions", "limitations")) {
    if (!col %in% names(study)) study[, (col) := list(character())]
  }
  out <- list(
    observational_study_id = study$observational_study_id[[1]],
    schema_version = "aq_observational_study_v1",
    study = study,
    causal_question = causal_question,
    estimand = estimand,
    created_at = aq_vnext_now()
  )
  out$validation <- aq_validate_observational_study(out)
  class(out) <- c("aq_observational_study", "list")
  out
}

#' Validate an Observational Causal Study Contract
#' @export
aq_validate_observational_study <- function(study) {
  rows <- list()
  if (!is.list(study) || is.null(study$study)) {
    rows <- aq_observational_add_row(rows, "observational_study_object", "fail", "study must be created by aq_observational_study().")
    return(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
  }
  s <- study$study[1]
  required <- c("observational_study_id", "decision_context_id", "causal_question_id", "estimand_id", "treatment", "comparison_condition", "unit_of_analysis", "population", "eligibility", "treatment_assignment_time", "outcome_window", "baseline_window")
  for (col in required) {
    if (!col %in% names(s) || !nzchar(aq_observational_chr(s[[col]], ""))) {
      rows <- aq_observational_add_row(rows, paste0("missing_", col), "fail", paste(col, "is required."), recommendation = paste("Author", col, "before observational planning."))
    }
  }
  if (identical(aq_observational_chr(s$treatment, ""), aq_observational_chr(s$comparison_condition, ""))) {
    rows <- aq_observational_add_row(rows, "treatment_equals_comparison", "fail", "Treatment and comparison must be distinct.")
  }
  if (!nzchar(aq_observational_chr(s$treatment_assignment_time, ""))) {
    rows <- aq_observational_add_row(rows, "time_zero_missing", "fail", "A defensible treatment-assignment anchor is required.", recommendation = "Define when treatment eligibility or receipt became knowable.")
  }
  if (!length(rows)) {
    rows <- aq_observational_add_row(rows, "observational_study_valid", "pass", "Observational study has the minimum structure needed for target-trial planning.")
  }
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

#' Define Observational Treatment and Comparison
#' @export
aq_observational_treatment_definition <- function(
  treatment_name,
  comparison_condition,
  treatment_type = "binary_treatment",
  treatment_threshold = NA_character_,
  grace_period = NA_character_,
  persistence = NA_character_,
  crossover = "not_assessed",
  discontinuation = "not_assessed",
  repeated_treatment = "not_assessed",
  contamination_risk = "unknown",
  measurement_quality = "unknown",
  notes = character()
) {
  out <- list(
    schema_version = "aq_observational_treatment_definition_v1",
    treatment_name = as.character(treatment_name)[1L],
    comparison_condition = as.character(comparison_condition)[1L],
    treatment_type = as.character(treatment_type)[1L],
    treatment_threshold = aq_observational_chr(treatment_threshold),
    grace_period = aq_observational_chr(grace_period),
    persistence = aq_observational_chr(persistence),
    crossover = aq_observational_chr(crossover, "not_assessed"),
    discontinuation = aq_observational_chr(discontinuation, "not_assessed"),
    repeated_treatment = aq_observational_chr(repeated_treatment, "not_assessed"),
    contamination_risk = aq_observational_chr(contamination_risk, "unknown"),
    measurement_quality = aq_observational_chr(measurement_quality, "unknown"),
    notes = aq_observational_vec(notes),
    no_effect_estimated = TRUE
  )
  class(out) <- c("aq_observational_treatment_definition", "list")
  out
}

#' Document an Observational Treatment Assignment Mechanism
#' @export
aq_observational_assignment_mechanism <- function(
  mechanism_type = "unknown",
  decision_process = NA_character_,
  assignment_inputs = character(),
  timing = NA_character_,
  known_rules = character(),
  exceptions = character(),
  override_behavior = "unknown",
  availability_constraints = character(),
  documentation_source = character(),
  confidence = "unknown",
  unresolved_factors = character(),
  likely_unmeasured_influences = character(),
  identification_implications = character()
) {
  mechanism_type <- aq_observational_chr(mechanism_type, "unknown")
  if (!mechanism_type %in% aq_observational_assignment_types()) mechanism_type <- "unknown"
  out <- list(
    schema_version = "aq_observational_assignment_mechanism_v1",
    mechanism_type = mechanism_type,
    decision_process = aq_observational_chr(decision_process),
    assignment_inputs = aq_observational_vec(assignment_inputs),
    timing = aq_observational_chr(timing),
    known_rules = aq_observational_vec(known_rules),
    exceptions = aq_observational_vec(exceptions),
    override_behavior = aq_observational_chr(override_behavior, "unknown"),
    availability_constraints = aq_observational_vec(availability_constraints),
    documentation_source = aq_observational_vec(documentation_source),
    confidence = aq_observational_chr(confidence, "unknown"),
    unresolved_factors = aq_observational_vec(unresolved_factors),
    likely_unmeasured_influences = aq_observational_vec(likely_unmeasured_influences),
    identification_implications = aq_observational_vec(identification_implications),
    no_effect_estimated = TRUE
  )
  class(out) <- c("aq_observational_assignment_mechanism", "list")
  out
}

#' Create a Target-Trial Emulation Specification
#' @export
aq_target_trial_spec <- function(
  study,
  eligibility_criteria,
  treatment_strategies,
  assignment_time,
  follow_up,
  outcome,
  causal_contrast,
  analysis_plan = "observational_design_readiness_only",
  censoring = "not_assessed",
  adherence = "not_assessed",
  estimand = NA_character_,
  target_trial_id = NULL
) {
  out <- list(
    target_trial_id = aq_vnext_default(target_trial_id, aq_vnext_id("target_trial")),
    schema_version = "aq_target_trial_spec_v1",
    observational_study_id = if (inherits(study, "aq_observational_study")) study$observational_study_id else aq_observational_chr(study),
    eligibility_criteria = aq_observational_vec(eligibility_criteria),
    treatment_strategies = aq_observational_vec(treatment_strategies),
    assignment_time = aq_observational_chr(assignment_time),
    follow_up = aq_observational_chr(follow_up),
    outcome = aq_observational_chr(outcome),
    causal_contrast = aq_observational_chr(causal_contrast),
    analysis_plan = aq_observational_chr(analysis_plan, "observational_design_readiness_only"),
    censoring = aq_observational_chr(censoring, "not_assessed"),
    adherence = aq_observational_chr(adherence, "not_assessed"),
    estimand = aq_observational_chr(estimand),
    no_effect_estimated = TRUE
  )
  out$validation <- aq_validate_target_trial_spec(out)
  class(out) <- c("aq_target_trial_spec", "list")
  out
}

#' Validate a Target-Trial Emulation Specification
#' @export
aq_validate_target_trial_spec <- function(spec) {
  rows <- list()
  required <- c("eligibility_criteria", "treatment_strategies", "assignment_time", "follow_up", "outcome", "causal_contrast")
  for (field in required) {
    value <- spec[[field]]
    missing <- if (is.character(value)) !length(value) || !nzchar(value[[1]]) else is.null(value) || !length(value)
    if (missing) rows <- aq_observational_add_row(rows, paste0("missing_", field), "fail", paste(field, "is required."))
  }
  lower_join <- tolower(paste(unlist(spec), collapse = " "))
  if (grepl("after outcome|post outcome|after follow", lower_join)) rows <- aq_observational_add_row(rows, "immortal_time_risk", "warning", "Specification text suggests possible immortal-time or post-outcome treatment definition.", recommendation = "Confirm treatment assignment time occurs before follow-up.")
  if (!length(rows)) rows <- aq_observational_add_row(rows, "target_trial_spec_valid", "pass", "Target-trial specification is structurally clear.")
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

#' Create an Observational Adjustment Set Contract
#' @export
aq_observational_adjustment_spec <- function(
  approved_confounders = character(),
  required_baseline_variables = character(),
  optional_precision_variables = character(),
  assignment_predictors = character(),
  effect_modifiers = character(),
  excluded_mediators = character(),
  excluded_colliders = character(),
  excluded_post_treatment = character(),
  instruments = character(),
  proxy_variables = character(),
  measurement_quality = list(),
  missingness = list(),
  evidence_basis = character(),
  graph_version = NA_character_,
  human_approval = FALSE,
  unresolved_conflicts = character(),
  adjustment_spec_id = NULL
) {
  out <- list(
    adjustment_spec_id = aq_vnext_default(adjustment_spec_id, aq_vnext_id("observational_adjustment")),
    schema_version = "aq_observational_adjustment_spec_v1",
    approved_confounders = aq_observational_vec(approved_confounders),
    required_baseline_variables = aq_observational_vec(required_baseline_variables),
    optional_precision_variables = aq_observational_vec(optional_precision_variables),
    assignment_predictors = aq_observational_vec(assignment_predictors),
    effect_modifiers = aq_observational_vec(effect_modifiers),
    excluded_mediators = aq_observational_vec(excluded_mediators),
    excluded_colliders = aq_observational_vec(excluded_colliders),
    excluded_post_treatment = aq_observational_vec(excluded_post_treatment),
    instruments = aq_observational_vec(instruments),
    proxy_variables = aq_observational_vec(proxy_variables),
    measurement_quality = measurement_quality,
    missingness = missingness,
    evidence_basis = aq_observational_vec(evidence_basis),
    graph_version = aq_observational_chr(graph_version),
    human_approval = isTRUE(human_approval),
    unresolved_conflicts = aq_observational_vec(unresolved_conflicts),
    no_automatic_confounder_selection = TRUE,
    no_effect_estimated = TRUE
  )
  class(out) <- c("aq_observational_adjustment_spec", "list")
  out
}

#' Assess Adjustment Variable Timing Eligibility
#' @export
aq_assess_observational_temporal_eligibility <- function(adjustment_spec, variable_timing = NULL) {
  vars <- aq_vnext_unique_chr(c(
    adjustment_spec$approved_confounders,
    adjustment_spec$required_baseline_variables,
    adjustment_spec$optional_precision_variables,
    adjustment_spec$assignment_predictors,
    adjustment_spec$effect_modifiers,
    adjustment_spec$excluded_mediators,
    adjustment_spec$excluded_colliders,
    adjustment_spec$excluded_post_treatment,
    adjustment_spec$instruments,
    adjustment_spec$proxy_variables
  ))
  timing <- data.table::as.data.table(variable_timing %||% data.table::data.table(variable = vars, timing_status = "timing unavailable"))
  if (!"variable" %in% names(timing)) timing[, variable := vars]
  if (!"timing_status" %in% names(timing)) timing[, timing_status := "timing unavailable"]
  out <- data.table::data.table(variable = vars)
  out <- merge(out, timing, by = "variable", all.x = TRUE)
  out[is.na(timing_status), timing_status := "timing unavailable"]
  out[, role := data.table::fcase(
    variable %in% adjustment_spec$excluded_mediators, "excluded_mediator",
    variable %in% adjustment_spec$excluded_colliders, "excluded_collider",
    variable %in% adjustment_spec$excluded_post_treatment, "excluded_post_treatment",
    variable %in% adjustment_spec$optional_precision_variables, "precision_variable",
    variable %in% adjustment_spec$approved_confounders, "approved_confounder",
    variable %in% adjustment_spec$assignment_predictors, "assignment_predictor",
    default = "candidate"
  )]
  out[, temporal_classification := data.table::fcase(
    grepl("post|future|leak", tolower(timing_status)), "invalid for adjustment",
    grepl("contemporaneous|ambiguous", tolower(timing_status)), "contemporaneous or ambiguous",
    grepl("stale", tolower(timing_status)), "baseline but stale",
    grepl("baseline|pre", tolower(timing_status)), "valid pre-treatment",
    role %in% c("excluded_mediator", "excluded_collider", "excluded_post_treatment"), "invalid for adjustment",
    default = "timing unavailable"
  )]
  out[, adjustment_eligible := temporal_classification %in% c("valid pre-treatment", "baseline but stale") & !role %in% c("excluded_mediator", "excluded_collider", "excluded_post_treatment")]
  out[, recommendation := data.table::fifelse(adjustment_eligible, "May be used only under the approved adjustment specification.", "Do not adjust until timing/role is resolved.")]
  out[]
}

#' Assess Observational Data Sufficiency and Treatment Variation
#' @export
aq_assess_observational_variation <- function(
  treated_count,
  comparison_count,
  outcome_count = NA_integer_,
  clusters = NA_integer_,
  treatment_prevalence = NULL,
  repeated_treatment = FALSE,
  rare_outcome = FALSE,
  sparse_strata = FALSE,
  baseline_history_coverage = NA_real_,
  eligible_population_coverage = NA_real_
) {
  treated_count <- as.integer(treated_count)[1L]
  comparison_count <- as.integer(comparison_count)[1L]
  total <- treated_count + comparison_count
  prevalence <- aq_observational_num(treatment_prevalence, if (total > 0) treated_count / total else NA_real_)
  state <- "adequate"
  reasons <- character()
  if (!is.finite(total) || total <= 0L) {
    state <- "insufficient information"; reasons <- c(reasons, "No analyzable units were supplied.")
  } else if (treated_count == 0L || comparison_count == 0L) {
    state <- "structurally unsupported"; reasons <- c(reasons, "Both treated and comparison units are required.")
  } else if (treated_count < 20L || comparison_count < 20L || prevalence < 0.02 || prevalence > 0.98) {
    state <- "sparse"; reasons <- c(reasons, "Treatment/comparison variation is sparse or extremely imbalanced.")
  } else if (treated_count < 50L || comparison_count < 50L || isTRUE(sparse_strata) || isTRUE(rare_outcome)) {
    state <- "limited"; reasons <- c(reasons, "Variation exists but may be limited for credible design implementation.")
  }
  if (!length(reasons)) reasons <- "Treatment and comparison variation is present."
  data.table::data.table(
    metric = c("treated_count", "comparison_count", "treatment_prevalence", "outcome_count", "clusters", "baseline_history_coverage", "eligible_population_coverage"),
    value = as.character(c(treated_count, comparison_count, round(prevalence, 4), outcome_count, clusters, baseline_history_coverage, eligible_population_coverage)),
    variation_state = state,
    reason = paste(reasons, collapse = " "),
    repeated_treatment = isTRUE(repeated_treatment),
    rare_outcome = isTRUE(rare_outcome),
    sparse_strata = isTRUE(sparse_strata)
  )
}

#' Create Treatment-Assignment Model Diagnostics for Design Only
#' @export
aq_observational_assignment_model_diagnostics <- function(
  predicted_treatment_probability,
  method = "logistic_regression",
  approved_pre_treatment_variables = character(),
  training_population = NA_character_,
  calibration = NA_character_,
  discrimination = NA_real_,
  warnings = character(),
  model_version = "assignment_diagnostic_v1"
) {
  p <- suppressWarnings(as.numeric(predicted_treatment_probability))
  p <- p[is.finite(p)]
  p <- p[p >= 0 & p <= 1]
  diagnostics <- data.table::data.table(
    metric = c("units_scored", "min_probability", "p05_probability", "median_probability", "p95_probability", "max_probability", "extreme_probability_share"),
    value = c(length(p), if (length(p)) min(p) else NA_real_, if (length(p)) stats::quantile(p, .05, names = FALSE) else NA_real_, if (length(p)) stats::median(p) else NA_real_, if (length(p)) stats::quantile(p, .95, names = FALSE) else NA_real_, if (length(p)) max(p) else NA_real_, if (length(p)) mean(p < .05 | p > .95) else NA_real_)
  )
  out <- list(
    schema_version = "aq_observational_assignment_model_diagnostics_v1",
    method = aq_observational_chr(method, "logistic_regression"),
    approved_pre_treatment_variables = aq_observational_vec(approved_pre_treatment_variables),
    training_population = aq_observational_chr(training_population),
    predicted_treatment_probability = p,
    calibration = aq_observational_chr(calibration),
    discrimination = aq_observational_num(discrimination),
    diagnostics = diagnostics,
    warnings = aq_observational_vec(warnings),
    model_version = aq_observational_chr(model_version, "assignment_diagnostic_v1"),
    model_purpose = "overlap_and_assignment_diagnostics_only",
    no_effect_estimated = TRUE
  )
  class(out) <- c("aq_observational_assignment_model_diagnostics", "list")
  out
}

#' Assess Observational Overlap and Positivity
#' @export
aq_assess_observational_overlap <- function(probabilities = NULL, assignment_model = NULL, subgroup = NULL) {
  p <- if (inherits(assignment_model, "aq_observational_assignment_model_diagnostics")) assignment_model$predicted_treatment_probability else probabilities
  p <- suppressWarnings(as.numeric(p))
  p <- p[is.finite(p) & p >= 0 & p <= 1]
  state <- "insufficient information"
  reason <- "No valid treatment probabilities or support diagnostics were supplied."
  if (length(p)) {
    extreme <- mean(p < .05 | p > .95)
    weak <- mean(p < .1 | p > .9)
    if (min(p) > .1 && max(p) < .9 && weak < .05) {
      state <- "strong overlap"; reason <- "Predicted treatment probabilities stay away from extreme support regions."
    } else if (min(p) > .05 && max(p) < .95 && weak < .2) {
      state <- "adequate overlap"; reason <- "Some weak support exists, but most units remain in common-support regions."
    } else if (extreme < .35) {
      state <- "limited overlap"; reason <- "Substantial units approach extreme probability regions."
    } else if (extreme < .75) {
      state <- "severe positivity concern"; reason <- "Extreme treatment probabilities suggest limited credible comparisons."
    } else {
      state <- "no credible support"; reason <- "Most units have near-deterministic treatment assignment."
    }
  }
  data.table::data.table(
    overlap_state = state,
    units = length(p),
    min_probability = if (length(p)) min(p) else NA_real_,
    max_probability = if (length(p)) max(p) else NA_real_,
    extreme_probability_share = if (length(p)) mean(p < .05 | p > .95) else NA_real_,
    subgroup = aq_observational_chr(subgroup, "overall"),
    reason = reason,
    recommendation = if (state %in% c("severe positivity concern", "no credible support")) "Narrow target population, revise estimand, collect better comparison data, or prefer an experiment." else "Use overlap as design evidence, not an effect estimate.",
    no_effect_estimated = TRUE
  )
}

#' Assess Baseline Covariate Balance
#' @export
aq_assess_observational_balance <- function(data, treatment, covariates) {
  dt <- data.table::as.data.table(data)
  covariates <- aq_observational_vec(covariates)
  if (!treatment %in% names(dt)) stop("treatment must exist in data.", call. = FALSE)
  groups <- unique(as.character(dt[[treatment]]))
  groups <- groups[nzchar(groups)]
  if (length(groups) < 2L) {
    return(data.table::data.table(variable = covariates, balance_metric = "unavailable", difference = NA_real_, standardized_difference = NA_real_, severity = "no comparison variation", no_p_value_dependence = TRUE))
  }
  g1 <- groups[[1L]]
  g2 <- groups[[2L]]
  rows <- lapply(covariates, function(v) {
    if (!v %in% names(dt)) {
      return(data.table::data.table(variable = v, balance_metric = "missing_variable", difference = NA_real_, standardized_difference = NA_real_, severity = "unavailable", recommendation = "Attach baseline covariate before balance assessment."))
    }
    x1 <- dt[as.character(get(treatment)) == g1, get(v)]
    x2 <- dt[as.character(get(treatment)) == g2, get(v)]
    if (is.numeric(x1) || is.integer(x1)) {
      m1 <- mean(as.numeric(x1), na.rm = TRUE); m2 <- mean(as.numeric(x2), na.rm = TRUE)
      sd1 <- stats::sd(as.numeric(x1), na.rm = TRUE); sd2 <- stats::sd(as.numeric(x2), na.rm = TRUE)
      pooled <- sqrt((sd1^2 + sd2^2) / 2)
      smd <- if (is.finite(pooled) && pooled > 0) (m1 - m2) / pooled else NA_real_
      sev <- if (!is.finite(smd)) "unavailable" else if (abs(smd) < .1) "balanced" else if (abs(smd) < .25) "moderate imbalance" else "imbalanced"
      data.table::data.table(variable = v, balance_metric = "standardized_mean_difference", difference = m1 - m2, standardized_difference = smd, severity = sev, recommendation = "Balance is a measured-covariate diagnostic, not proof of ignorability.")
    } else {
      tab <- table(as.character(dt[[treatment]]), as.character(dt[[v]]))
      props <- prop.table(tab, 1)
      diff <- if (all(c(g1, g2) %in% rownames(props))) max(abs(props[g1, ] - props[g2, ]), na.rm = TRUE) else NA_real_
      sev <- if (!is.finite(diff)) "unavailable" else if (diff < .05) "balanced" else if (diff < .15) "moderate imbalance" else "imbalanced"
      data.table::data.table(variable = v, balance_metric = "max_categorical_proportion_difference", difference = diff, standardized_difference = NA_real_, severity = sev, recommendation = "Inspect categories with large distribution differences.")
    }
  })
  out <- data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
  out[, no_p_value_dependence := TRUE]
  out[]
}

#' Assess Selection, Attrition, and Missingness Threats
#' @export
aq_assess_observational_selection_missingness <- function(records) {
  dt <- data.table::as.data.table(records)
  if (!nrow(dt)) return(data.table::data.table(threat = "missing_selection_assessment", state = "insufficient_information", recommendation = "Document dataset selection, treatment observation, outcome observation, censoring, attrition, and missing confounders."))
  required <- c("threat", "timing", "severity", "evidence", "recommendation")
  for (col in setdiff(required, names(dt))) dt[, (col) := NA_character_]
  dt[, state := data.table::fcase(
    grepl("post", tolower(timing)) | grepl("collider|outcome", tolower(threat)), "selection_review_required",
    grepl("high|critical", tolower(severity)), "selection_review_required",
    grepl("unknown|missing", tolower(severity)), "insufficient_information",
    default = "documented"
  )]
  dt[]
}

#' Create an Unmeasured-Confounding Risk Register
#' @export
aq_unmeasured_confounding_risk_register <- function(risks) {
  dt <- data.table::as.data.table(risks)
  if (!nrow(dt)) dt <- data.table::data.table(factor = "unmeasured assignment influences", direction_of_bias = "unknown", treatment_assignment_relevance = "unknown", outcome_relevance = "unknown", proxies = "", negative_control_opportunities = "", sensitivity_required = TRUE, confidence = "unknown", decision_consequence = "causal claim should remain qualified")
  required <- c("factor", "direction_of_bias", "treatment_assignment_relevance", "outcome_relevance", "proxies", "negative_control_opportunities", "sensitivity_required", "confidence", "decision_consequence")
  for (col in setdiff(required, names(dt))) dt[, (col) := NA_character_]
  dt[, risk_state := data.table::fcase(
    grepl("critical", tolower(confidence)) | grepl("critical", tolower(decision_consequence)), "critical unidentified threat",
    grepl("high", tolower(confidence)) | grepl("high", tolower(decision_consequence)), "high concern",
    grepl("moderate", tolower(confidence)), "moderate concern",
    grepl("low", tolower(confidence)), "low concern under stated assumptions",
    default = "unknown"
  )]
  dt[, recommendation := data.table::fifelse(risk_state %in% c("high concern", "critical unidentified threat", "unknown"), "Plan sensitivity analysis, negative controls, new data collection, or experiment.", "Preserve assumption and revisit after diagnostics.")]
  dt[]
}

#' Plan Negative Controls and Falsification Checks
#' @export
aq_observational_falsification_plan <- function(tests) {
  dt <- data.table::as.data.table(tests)
  if (!nrow(dt)) return(data.table::data.table(test_type = "missing_falsification_plan", rationale = "No falsification plan supplied.", expected_relationship = "null", required_data = NA_character_, interpretation = "Passing falsification tests would not prove causality.", limitations = "Missing plan blocks high-confidence observational claims."))
  required <- c("test_type", "rationale", "expected_relationship", "required_data", "interpretation", "limitations")
  for (col in setdiff(required, names(dt))) dt[, (col) := NA_character_]
  dt[]
}

#' Assess Candidate Observational Design Eligibility
#' @export
aq_observational_design_eligibility <- function(
  study,
  variation = NULL,
  overlap = NULL,
  adjustment = NULL,
  target_trial = NULL,
  evidence = list()
) {
  variation_state <- if (is.data.frame(variation) && "variation_state" %in% names(variation) && nrow(variation)) variation$variation_state[[1L]] else "insufficient information"
  overlap_state <- if (is.data.frame(overlap) && "overlap_state" %in% names(overlap) && nrow(overlap)) overlap$overlap_state[[1L]] else "insufficient information"
  has_adjustment <- inherits(adjustment, "aq_observational_adjustment_spec") && length(adjustment$approved_confounders)
  designs <- c("covariate_adjustment", "matching", "propensity_score_matching", "inverse_probability_weighting", "overlap_weighting", "regression_adjustment", "doubly_robust", "difference_in_differences", "event_study", "interrupted_time_series", "synthetic_control", "regression_discontinuity", "instrumental_variables", "negative_controls")
  data.table::rbindlist(lapply(designs, function(design) {
    status <- "potentially eligible"
    reasons <- character()
    if (variation_state %in% c("structurally unsupported", "insufficient information")) {
      status <- "ineligible"; reasons <- c(reasons, "Treatment/comparison variation is insufficient.")
    }
    if (design %in% c("matching", "propensity_score_matching", "inverse_probability_weighting", "overlap_weighting", "doubly_robust") && overlap_state %in% c("severe positivity concern", "no credible support", "insufficient information")) {
      status <- if (overlap_state == "insufficient information") "required data missing" else "ineligible"
      reasons <- c(reasons, "Overlap/positivity does not currently support this design.")
    }
    if (design %in% c("covariate_adjustment", "regression_adjustment", "doubly_robust") && !has_adjustment) {
      status <- "required data missing"; reasons <- c(reasons, "Approved pre-treatment confounder set is missing.")
    }
    if (design %in% c("difference_in_differences", "event_study") && !isTRUE(evidence$pre_period_available %||% FALSE)) {
      status <- "required data missing"; reasons <- c(reasons, "Pre-period outcome history is required.")
    }
    if (design == "synthetic_control" && !isTRUE(evidence$donor_pool_available %||% FALSE)) {
      status <- "required data missing"; reasons <- c(reasons, "Donor pool evidence is required.")
    }
    if (design == "regression_discontinuity" && !isTRUE(evidence$running_variable_cutoff %||% FALSE)) {
      status <- "required data missing"; reasons <- c(reasons, "Running variable and cutoff evidence are required.")
    }
    if (design == "instrumental_variables" && !isTRUE(evidence$candidate_instrument %||% FALSE)) {
      status <- "required data missing"; reasons <- c(reasons, "Candidate instrument evidence is required.")
    }
    if (design == "negative_controls" && !isTRUE(evidence$negative_control_available %||% FALSE)) {
      status <- "potentially eligible"; reasons <- c(reasons, "Negative-control candidates should be authored before estimation.")
    }
    if (!length(reasons)) {
      status <- if (design %in% c("covariate_adjustment", "matching", "overlap_weighting") && overlap_state %in% c("strong overlap", "adequate overlap") && has_adjustment) "eligible" else status
      reasons <- "Eligible subject to stated assumptions and mandatory diagnostics."
    }
    data.table::data.table(
      design_family = design,
      eligibility = status,
      required_data = paste(reasons, collapse = " "),
      required_assumptions = "Measured-confounding, temporal ordering, positivity, stable measurement, and no unsupported interference as applicable.",
      major_threats = if (status == "eligible") "unmeasured confounding and model specification remain possible" else paste(reasons, collapse = " "),
      target_estimand_compatibility = "requires design-specific review",
      recommended_priority = data.table::fifelse(status == "eligible", "primary_candidate", data.table::fifelse(status == "potentially eligible", "secondary_candidate", "defer")),
      no_effect_estimated = TRUE
    )
  }), use.names = TRUE, fill = TRUE)
}

#' Plan an Observational Causal Analysis Without Estimating Effects
#' @export
aq_plan_observational_causal_analysis <- function(
  study,
  target_trial,
  assignment_mechanism,
  treatment_definition,
  adjustment_spec,
  temporal_eligibility,
  variation,
  overlap,
  balance,
  selection_missingness,
  unmeasured_risk,
  falsification_plan,
  design_eligibility = NULL,
  plan_id = NULL
) {
  if (is.null(design_eligibility)) {
    design_eligibility <- aq_observational_design_eligibility(study, variation, overlap, adjustment_spec, target_trial)
  }
  eligible <- design_eligibility[eligibility %in% c("eligible", "potentially eligible")]
  primary <- if (nrow(eligible)) eligible$design_family[[1L]] else "none"
  blockers <- c()
  if (nrow(design_eligibility[eligibility == "ineligible"]) == nrow(design_eligibility)) blockers <- c(blockers, "No observational design is currently eligible.")
  if (assignment_mechanism$mechanism_type == "unknown") blockers <- c(blockers, "Treatment assignment mechanism is unknown.")
  if (is.data.frame(overlap) && nrow(overlap) && overlap$overlap_state[[1L]] %in% c("severe positivity concern", "no credible support")) blockers <- c(blockers, "Overlap/positivity is inadequate.")
  if (any(!temporal_eligibility$adjustment_eligible & temporal_eligibility$role %in% c("approved_confounder", "assignment_predictor"))) blockers <- c(blockers, "Some proposed adjustment variables are not valid pre-treatment variables.")
  out <- list(
    plan_id = aq_vnext_default(plan_id, aq_vnext_id("observational_analysis_plan")),
    schema_version = "aq_observational_analysis_plan_v1",
    observational_study_id = study$observational_study_id,
    target_trial_id = target_trial$target_trial_id,
    target_estimand = target_trial$estimand,
    recommended_primary_design = primary,
    candidate_estimator_family = if (primary == "none") "none" else paste0(primary, "_future_estimator"),
    mandatory_diagnostics = c("temporal eligibility", "treatment variation", "baseline balance", "overlap", "selection/missingness", "unmeasured confounding", "falsification/sensitivity"),
    required_adjustment_set = adjustment_spec$approved_confounders,
    overlap_policy = if (is.data.frame(overlap) && nrow(overlap)) overlap$overlap_state[[1L]] else "not_assessed",
    missingness_plan = if (is.data.frame(selection_missingness) && nrow(selection_missingness)) paste(unique(selection_missingness$state), collapse = ", ") else "not_assessed",
    falsification_tests = if (is.data.frame(falsification_plan) && nrow(falsification_plan)) falsification_plan$test_type else character(),
    prohibited_variables = aq_vnext_unique_chr(c(adjustment_spec$excluded_mediators, adjustment_spec$excluded_colliders, adjustment_spec$excluded_post_treatment)),
    prohibited_claims = c("observational data prove randomization", "ignorability is established automatically", "balance proves no unmeasured confounding", "treatment effect was estimated in this phase"),
    permitted_claims = c("observational design readiness was assessed", "estimation remains conditional on explicit assumptions", "negative readiness is valid decision evidence"),
    stopping_conditions = blockers,
    experiment_alternative = if (length(blockers)) "recommended when blockers cannot be resolved" else "retain as benchmark if decision criticality is high",
    no_effect_estimated = TRUE,
    created_at = aq_vnext_now()
  )
  class(out) <- c("aq_observational_analysis_plan", "list")
  out
}

#' Assess Observational Estimation Readiness
#' @export
aq_assess_observational_estimation_readiness <- function(plan, overlap = NULL, adjustment_spec = NULL, selection_missingness = NULL, unmeasured_risk = NULL, falsification_plan = NULL) {
  reasons <- character()
  state <- "ready_for_design_implementation"
  if (length(plan$stopping_conditions)) {
    state <- "blocked"; reasons <- c(reasons, plan$stopping_conditions)
  }
  overlap_state <- if (is.data.frame(overlap) && nrow(overlap)) overlap$overlap_state[[1L]] else plan$overlap_policy
  if (overlap_state %in% c("severe positivity concern", "no credible support")) {
    state <- if (overlap_state == "no credible support") "experiment_preferred" else "overlap_remediation_required"
    reasons <- c(reasons, paste("Overlap state:", overlap_state))
  }
  if (inherits(adjustment_spec, "aq_observational_adjustment_spec") && !length(adjustment_spec$approved_confounders)) {
    state <- "adjustment_set_unresolved"; reasons <- c(reasons, "No approved confounder set exists.")
  }
  if (is.data.frame(selection_missingness) && any(selection_missingness$state == "selection_review_required")) {
    state <- if (state == "ready_for_design_implementation") "selection_review_required" else state
    reasons <- c(reasons, "Selection or missingness requires review.")
  }
  if (is.data.frame(unmeasured_risk) && any(unmeasured_risk$risk_state %in% c("high concern", "critical unidentified threat", "unknown"))) {
    state <- if (state == "ready_for_design_implementation") "sensitivity_plan_required" else state
    reasons <- c(reasons, "Unmeasured-confounding risk requires sensitivity planning.")
  }
  if (is.data.frame(falsification_plan) && any(falsification_plan$test_type == "missing_falsification_plan")) {
    state <- if (state == "ready_for_design_implementation") "sensitivity_plan_required" else state
    reasons <- c(reasons, "Falsification plan is missing.")
  }
  if (!length(reasons)) reasons <- "Planning evidence supports implementing the selected observational design under explicit assumptions."
  data.table::data.table(
    readiness_state = state,
    reasons = paste(unique(reasons), collapse = " | "),
    required_human_approvals = paste(c("causal design owner", "domain owner"), collapse = ", "),
    supported_next_actions = paste(c("freeze_design", "prepare_future_estimator", "register_planning_artifact", "report_limitations"), collapse = ", "),
    prohibited_claims = paste(plan$prohibited_claims, collapse = " | "),
    no_effect_estimated = TRUE
  )
}

#' Create a Canonical Observational Causal Planning Artifact
#' @export
aq_observational_causal_planning_artifact <- function(study, plan, readiness, artifact_id = NULL) {
  artifact_id <- aq_vnext_default(artifact_id, aq_vnext_id("observational_causal_planning_artifact"))
  artifact <- new_metadata_artifact(
    id = artifact_id,
    title = paste("Observational Causal Planning:", study$study$study_title[[1]] %||% study$observational_study_id),
    metadata = list(
      artifact_type = "observational_causal_planning_artifact",
      observational_study_id = study$observational_study_id,
      causal_question_id = study$study$causal_question_id[[1]],
      estimand_id = study$study$estimand_id[[1]],
      plan_id = plan$plan_id,
      readiness_state = if (is.data.frame(readiness) && nrow(readiness)) readiness$readiness_state[[1]] else "not_assessed",
      permitted_claims = plan$permitted_claims,
      prohibited_claims = plan$prohibited_claims,
      no_effect_estimated = TRUE,
      no_ignorability_claim = TRUE,
      no_automatic_confounder_selection = TRUE,
      supported_actions = c("review_plan", "register_artifact", "prepare_future_estimator", "recommend_experiment")
    ),
    source_generator = "aq_observational_causal_planning_artifact",
    version = "aq_observational_causal_planning_artifact_v1"
  )
  artifact$artifact_envelope <- aq_vnext_attach_envelope(
    artifact,
    artifact_id = artifact_id,
    artifact_type = "observational_causal_planning_artifact",
    artifact_version = "aq_observational_causal_planning_artifact_v1",
    parent_artifact_ids = aq_vnext_unique_chr(c(study$study$decision_context_id[[1]], study$study$causal_question_id[[1]], study$study$estimand_id[[1]])),
    lineage = list(observational_study_id = study$observational_study_id, plan_id = plan$plan_id, no_effect_estimated = TRUE),
    task = "observational_causal_design_readiness",
    operator = "deterministic_observational_planning",
    engine = "none",
    specification_id = plan$plan_id,
    supported_actions = c("review_plan", "register_artifact", "prepare_future_estimator", "recommend_experiment"),
    producer = "aq_observational_causal_planning_artifact"
  )
  artifact
}

#' Deterministic QA for Observational Causal Planning
#' @export
qa_observational_causal_planning <- function() {
  checks <- list()
  add <- function(check, ok, message) {
    checks[[length(checks) + 1L]] <<- data.table::data.table(
      check = check,
      status = if (isTRUE(ok)) "success" else "error",
      message = message
    )
  }
  study <- aq_observational_study(data.table::data.table(
    observational_study_id = "obs_demo",
    decision_context_id = "decision_demo",
    causal_question_id = "cq_demo",
    estimand_id = "estimand_ate",
    study_title = "Discount eligibility observational study",
    treatment = "discount eligible",
    comparison_condition = "not discount eligible",
    unit_of_analysis = "customer",
    population = "eligible customers",
    eligibility = "active customers before assignment",
    treatment_assignment_time = "eligibility scoring date",
    treatment_window = "30 days",
    outcome_window = "60 days after assignment",
    baseline_window = "90 days before assignment",
    index_date = "2026-01-01",
    data_cutoff = "2026-04-01",
    organizational_scope = "growth",
    authority = "analytics",
    coverage = "qa",
    status = "draft"
  ))
  add("study_contract", inherits(study, "aq_observational_study") && !any(study$validation$status == "fail"), "Study context is typed and temporally anchored.")
  assignment <- aq_observational_assignment_mechanism("eligibility_threshold", decision_process = "score-based eligibility rule", assignment_inputs = c("prior_spend", "region"), timing = "pre_treatment", confidence = "moderate")
  treatment <- aq_observational_treatment_definition("discount eligible", "not discount eligible")
  trial <- aq_target_trial_spec(study, "active before score", c("eligible", "not eligible"), "score date", "60 days", "revenue", "ATE", estimand = "ATE")
  add("target_trial", inherits(trial, "aq_target_trial_spec") && !any(trial$validation$status == "fail"), "Target-trial spec is represented.")
  adj <- aq_observational_adjustment_spec(approved_confounders = c("prior_spend", "region"), optional_precision_variables = "tenure", excluded_mediators = "post_discount_clicks", excluded_colliders = "observed_purchase")
  timing <- aq_assess_observational_temporal_eligibility(adj, data.table::data.table(variable = c("prior_spend", "region", "tenure", "post_discount_clicks", "observed_purchase"), timing_status = c("pre_treatment", "baseline", "baseline stale", "post_treatment", "post_treatment")))
  add("temporal_eligibility", all(timing[variable %in% c("post_discount_clicks", "observed_purchase")]$adjustment_eligible == FALSE), "Mediators, colliders, and post-treatment variables are rejected.")
  variation <- aq_assess_observational_variation(120, 160, outcome_count = 250, clusters = 12)
  add("treatment_variation", variation$variation_state[[1]] %in% c("adequate", "limited"), "Treatment variation is assessed without relying on row count alone.")
  probs <- c(seq(.12, .88, length.out = 100), .98, .02)
  assignment_model <- aq_observational_assignment_model_diagnostics(probs, approved_pre_treatment_variables = c("prior_spend", "region"))
  overlap <- aq_assess_observational_overlap(assignment_model = assignment_model)
  add("assignment_model_no_effect", isTRUE(assignment_model$no_effect_estimated) && "extreme_probability_share" %in% assignment_model$diagnostics$metric, "Assignment model is bounded to overlap diagnostics.")
  set.seed(1)
  balance_data <- data.table::data.table(treat = rep(c(0, 1), each = 60), prior_spend = c(rnorm(60, 10, 2), rnorm(60, 11, 2)), region = rep(c("A", "B"), 60))
  balance <- aq_assess_observational_balance(balance_data, "treat", c("prior_spend", "region"))
  add("balance_no_p_values", all(balance$no_p_value_dependence), "Balance uses SMD/distribution differences, not significance-test dependence.")
  selection <- aq_assess_observational_selection_missingness(data.table::data.table(threat = "outcome missingness", timing = "post_treatment", severity = "moderate", evidence = "qa", recommendation = "review"))
  unmeasured <- aq_unmeasured_confounding_risk_register(data.table::data.table(factor = "manager discretion", confidence = "high", decision_consequence = "high uncertainty"))
  falsification <- aq_observational_falsification_plan(data.table::data.table(test_type = "placebo_period", rationale = "pre-treatment pseudo-effect", expected_relationship = "null", required_data = "pre-period outcomes", interpretation = "detects bias", limitations = "not proof"))
  designs <- aq_observational_design_eligibility(study, variation, overlap, adj, trial, evidence = list(pre_period_available = TRUE, negative_control_available = TRUE))
  add("design_eligibility", all(c("matching", "inverse_probability_weighting", "difference_in_differences", "synthetic_control", "regression_discontinuity", "instrumental_variables", "negative_controls") %in% designs$design_family), "Major observational design families are assessed without estimating effects.")
  plan <- aq_plan_observational_causal_analysis(study, trial, assignment, treatment, adj, timing, variation, overlap, balance, selection, unmeasured, falsification, designs)
  readiness <- aq_assess_observational_estimation_readiness(plan, overlap, adj, selection, unmeasured, falsification)
  add("readiness_gate", readiness$readiness_state[[1]] %in% aq_observational_readiness_states() && isTRUE(readiness$no_effect_estimated[[1]]), "Readiness gate returns a governed state and no effect estimate.")
  artifact <- aq_observational_causal_planning_artifact(study, plan, readiness)
  add("planning_artifact", inherits(artifact, "aq_artifact") && identical(artifact$metadata$artifact_type, "observational_causal_planning_artifact") && isTRUE(artifact$metadata$no_effect_estimated), "Canonical planning artifact preserves no-effect and no-ignorability claims.")
  add("no_effect_estimator_exported", !exists("aq_estimate_observational_effect", mode = "function"), "No observational effect estimator was introduced.")
  data.table::rbindlist(checks, use.names = TRUE, fill = TRUE)
}
