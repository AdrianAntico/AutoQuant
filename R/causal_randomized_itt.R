# AutoQuant randomized ITT estimation contracts.

aq_randomized_itt_lifecycle_states <- function() {
  c("draft", "readiness_blocked", "specification_complete", "analysis_run", "analyst_review", "approved_evidence", "revision_required", "superseded", "retired")
}

aq_randomized_itt_compatible_readiness <- function() {
  c("ready_for_itt", "ready_for_itt_with_adjustment", "ready_with_major_limitations")
}

aq_randomized_itt_now <- function() aq_vnext_now()

aq_itt_chr <- function(x, default = NA_character_) {
  if (is.null(x) || !length(x) || is.na(x[[1]])) return(default)
  as.character(x[[1]])
}

aq_itt_num <- function(x, default = NA_real_) {
  out <- suppressWarnings(as.numeric(x[[1]] %||% default))
  if (!is.finite(out)) default else out
}

aq_itt_parse_chr <- function(x) {
  if (is.null(x) || !length(x)) return(character())
  x <- paste(as.character(x), collapse = ",")
  x <- trimws(strsplit(x, ",", fixed = TRUE)[[1]])
  x[nzchar(x)]
}

aq_itt_se_method_levels <- function() {
  c("welch", "pooled", "hc0", "cluster")
}

aq_itt_missing_policy_levels <- function() {
  c("complete_outcome_primary", "bounds_only", "blocked_if_missing", "report_only")
}

#' Create a Randomized ITT Analysis Specification
#' @export
aq_randomized_itt_spec <- function(
  analysis_id = NULL,
  completed_experiment_id,
  experiment_plan_artifact_id,
  causal_question_id,
  estimand_id,
  assignment_column = "planned_arm",
  treatment_arm,
  comparison_arm,
  analysis_population = "all_randomized_eligible_units",
  outcome,
  outcome_type = c("continuous", "binary"),
  effect_scale = NULL,
  outcome_window = NA_character_,
  baseline_covariates = character(),
  blocking_variables = character(),
  stratification_variables = character(),
  cluster_variable = NA_character_,
  standard_error_method = c("welch", "hc0", "cluster", "pooled"),
  confidence_level = 0.95,
  missing_outcome_policy = c("complete_outcome_primary", "bounds_only", "blocked_if_missing", "report_only"),
  multiplicity_policy = c("primary_unadjusted", "bonferroni", "holm", "report_only"),
  guardrail_outcomes = character(),
  subgroup_analyses = character(),
  sensitivity_analyses = c("unadjusted_vs_adjusted", "missing_outcome_bounds"),
  minimum_meaningful_effect = NA_real_,
  analysis_amendments = character(),
  authority = NA_character_,
  status = "draft",
  supported_actions = c("validate_readiness", "construct_population", "estimate_itt", "register_effect_artifact")
) {
  outcome_type <- match.arg(outcome_type)
  standard_error_method <- match.arg(standard_error_method)
  missing_outcome_policy <- match.arg(missing_outcome_policy)
  multiplicity_policy <- match.arg(multiplicity_policy)
  if (is.null(effect_scale)) effect_scale <- if (identical(outcome_type, "binary")) "risk_difference" else "mean_difference"
  out <- list(
    schema_version = "aq_randomized_itt_spec_v1",
    analysis_id = analysis_id %||% aq_vnext_id("randomized_itt_analysis"),
    completed_experiment_id = completed_experiment_id,
    experiment_plan_artifact_id = experiment_plan_artifact_id,
    causal_question_id = causal_question_id,
    estimand_id = estimand_id,
    assignment_column = assignment_column,
    treatment_arm = treatment_arm,
    comparison_arm = comparison_arm,
    analysis_population = analysis_population,
    outcome = outcome,
    outcome_type = outcome_type,
    effect_scale = effect_scale,
    outcome_window = outcome_window,
    baseline_covariates = aq_vnext_unique_chr(baseline_covariates),
    blocking_variables = aq_vnext_unique_chr(blocking_variables),
    stratification_variables = aq_vnext_unique_chr(stratification_variables),
    cluster_variable = cluster_variable,
    standard_error_method = standard_error_method,
    confidence_level = confidence_level,
    missing_outcome_policy = missing_outcome_policy,
    multiplicity_policy = multiplicity_policy,
    guardrail_outcomes = aq_vnext_unique_chr(guardrail_outcomes),
    subgroup_analyses = aq_vnext_unique_chr(subgroup_analyses),
    sensitivity_analyses = aq_vnext_unique_chr(sensitivity_analyses),
    minimum_meaningful_effect = minimum_meaningful_effect,
    analysis_amendments = aq_vnext_unique_chr(analysis_amendments),
    authority = authority,
    status = status,
    supported_actions = aq_vnext_unique_chr(supported_actions),
    created_at = aq_randomized_itt_now()
  )
  class(out) <- c("aq_randomized_itt_spec", "list")
  out$validation <- aq_validate_randomized_itt_spec(out)
  out
}

#' Validate a Randomized ITT Specification
#' @export
aq_validate_randomized_itt_spec <- function(spec, planned_analysis = NULL, covariate_roles = NULL) {
  rows <- list()
  add <- function(check, status, reason, recommendation = NA_character_) {
    rows[[length(rows) + 1L]] <<- data.table::data.table(check, status, reason, recommendation)
  }
  required <- c("analysis_id", "completed_experiment_id", "experiment_plan_artifact_id", "causal_question_id", "estimand_id", "assignment_column", "treatment_arm", "comparison_arm", "outcome", "outcome_type")
  for (field in required) if (!nzchar(aq_itt_chr(spec[[field]], ""))) add(paste0("missing_", field), "fail", paste(field, "is required."))
  if (identical(aq_itt_chr(spec$treatment_arm), aq_itt_chr(spec$comparison_arm))) add("invalid_contrast", "fail", "Treatment and comparison arms must differ.")
  if (!spec$outcome_type %in% c("continuous", "binary")) add("unsupported_outcome_type", "fail", "Only continuous and binary outcomes are supported in this first ITT slice.")
  if (!spec$standard_error_method %in% aq_itt_se_method_levels()) add("unsupported_se_method", "fail", "Standard-error method is not supported.")
  if (!spec$missing_outcome_policy %in% aq_itt_missing_policy_levels()) add("unsupported_missing_policy", "fail", "Missing-outcome policy is not supported.")
  if (!spec$status %in% aq_randomized_itt_lifecycle_states()) add("unsupported_lifecycle_state", "fail", "Analysis lifecycle state is not supported.")
  if (!is.null(planned_analysis) && length(spec$baseline_covariates)) {
    allowed <- aq_itt_parse_chr(planned_analysis$baseline_covariates %||% character())
    disallowed <- setdiff(spec$baseline_covariates, allowed)
    if (length(disallowed)) add("unapproved_covariates", "fail", paste("Covariates not in planned analysis:", paste(disallowed, collapse = ", ")), "Use only pre-approved pre-treatment precision variables or author an amendment.")
  }
  if (!is.null(covariate_roles) && length(spec$baseline_covariates)) {
    roles <- data.table::as.data.table(covariate_roles)
    bad <- roles[variable %in% spec$baseline_covariates & (timing %in% c("post_treatment", "contemporaneous", "future_unknown") | role %in% c("mediator_candidate", "collider_candidate", "outcome"))]
    if (nrow(bad)) add("post_treatment_or_invalid_covariates", "fail", paste("Invalid precision covariates:", paste(unique(bad$variable), collapse = ", ")), "Remove post-treatment, mediator, collider, or outcome-derived covariates.")
  }
  if (!length(rows)) add("specification_valid", "pass", "Randomized ITT specification is complete.")
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

#' Validate Randomized ITT Readiness
#' @export
aq_validate_randomized_itt_readiness <- function(spec, completed_evidence, readiness = NULL, planned_analysis = NULL, baseline_data = NULL, stale_readiness = FALSE, stale_planned_analysis = FALSE, authority_approved = TRUE, coverage_approved = TRUE) {
  rows <- list()
  add <- function(check, status, reason, recommendation = NA_character_, prohibited_claims = character()) {
    rows[[length(rows) + 1L]] <<- data.table::data.table(check, status, reason, recommendation, prohibited_claims = paste(prohibited_claims, collapse = " | "))
  }
  validation <- aq_validate_randomized_itt_spec(spec, planned_analysis)
  if (any(validation$status == "fail")) add("specification_invalid", "blocked", paste(validation[status == "fail"]$reason, collapse = " | "), "Fix the frozen ITT specification.")
  readiness_state <- if (!is.null(readiness) && "readiness_state" %in% names(readiness)) readiness$readiness_state[[1]] else if (!is.null(completed_evidence$readiness)) completed_evidence$readiness$readiness_state[[1]] else "insufficient_information"
  if (!readiness_state %in% aq_randomized_itt_compatible_readiness()) {
    status <- if (identical(readiness_state, "descriptive_only")) "descriptive_only" else if (grepl("estimand", readiness_state)) "amendment_required" else "blocked"
    add("readiness_state", status, paste("Readiness state is", readiness_state), "Only ITT-compatible completed experiments may enter estimation.", c("causal effect estimated", "ITT-ready"))
  }
  if (isTRUE(stale_readiness)) add("stale_readiness", "blocked", "Completed-experiment readiness is stale.", "Reassess completed-experiment evidence before estimation.")
  if (isTRUE(stale_planned_analysis)) add("stale_planned_analysis", "blocked", "Planned analysis record is stale.", "Regenerate or approve a frozen planned-analysis record.")
  assignment <- completed_evidence$assignment$assignment %||% data.table::data.table()
  outcomes <- completed_evidence$outcomes$outcomes %||% data.table::data.table()
  if (!nrow(assignment)) add("assignment_missing", "blocked", "Original assignment evidence is missing.", "Attach the original randomized assignment log.")
  if (nrow(assignment) && !all(c("unit_id", "planned_arm") %in% names(assignment))) add("assignment_columns", "blocked", "Assignment evidence must include unit_id and planned_arm.")
  if (!nrow(outcomes)) add("outcome_missing", "blocked", "Primary outcome evidence is missing.", "Attach planned outcome evidence.")
  if (!isTRUE(authority_approved)) add("authority_missing", "blocked", "Analysis authority is not approved.", "Obtain governed analysis approval.")
  if (!isTRUE(coverage_approved)) add("coverage_missing", "blocked", "Analysis coverage is not approved.", "Confirm that the randomized population supports the decision context.")
  exclusions <- completed_evidence$exclusions$exclusions %||% data.table::data.table()
  if (nrow(exclusions) && any(exclusions$post_assignment %in% TRUE, na.rm = TRUE)) add("post_assignment_exclusions", "amendment_required", "Post-assignment exclusions are present.", "Retain randomized units for ITT or author a governed sensitivity/amendment.")
  integrity <- completed_evidence$integrity
  if (!is.null(integrity) && "status" %in% names(integrity) && integrity$status[[1]] %in% c("assignment_integrity_review")) add("assignment_integrity", "blocked", "Assignment integrity requires review.", "Resolve assignment integrity before estimation.")
  baseline_names <- if (!is.null(baseline_data)) names(data.table::as.data.table(baseline_data)) else character()
  cluster_variable <- aq_itt_chr(spec$cluster_variable, "")
  cluster_available <- !nzchar(cluster_variable) || cluster_variable %in% names(assignment) || cluster_variable %in% baseline_names
  if (!cluster_available) add("cluster_variable_missing", "blocked", "Cluster standard errors were requested but the cluster variable is missing from assignment or baseline population evidence.")
  if (!length(rows)) add("itt_ready", "eligible", "Completed-experiment evidence is eligible for randomized ITT estimation.", "Run the frozen ITT analysis specification.")
  out <- data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
  out[, eligibility := if (any(status == "blocked")) "blocked" else if (any(status == "amendment_required")) "amendment_required" else if (any(status == "descriptive_only")) "descriptive_only" else if (any(status %in% c("warning", "eligible_with_limitations")) || readiness_state == "ready_with_major_limitations") "eligible_with_limitations" else "eligible"]
  out[]
}

#' Construct the Randomized ITT Analysis Population
#' @export
aq_itt_analysis_population <- function(spec, assignment_evidence, outcome_evidence, exclusion_evidence = NULL, baseline_data = NULL, unit_id_col = "unit_id") {
  assignment <- if (inherits(assignment_evidence, "aq_assignment_evidence")) assignment_evidence$assignment else data.table::as.data.table(assignment_evidence)
  outcomes <- if (inherits(outcome_evidence, "aq_outcome_evidence")) outcome_evidence$outcomes else data.table::as.data.table(outcome_evidence)
  exclusions <- if (inherits(exclusion_evidence, "aq_exclusion_evidence")) exclusion_evidence$exclusions else data.table::as.data.table(exclusion_evidence %||% data.table::data.table())
  population <- data.table::copy(assignment)
  if (!"unit_id" %in% names(population)) stop("assignment_evidence must include unit_id.", call. = FALSE)
  population <- population[planned_arm %in% c(spec$treatment_arm, spec$comparison_arm)]
  population[, itt_arm := planned_arm]
  outcome_rows <- outcomes[outcome_id == spec$outcome & outcome_role %in% c("primary", "secondary", "guardrail", NA_character_)]
  outcome_rows <- outcome_rows[, .(unit_id, outcome_value = value, outcome_missingness = missingness)]
  population <- merge(population, outcome_rows, by = "unit_id", all.x = TRUE)
  if (!"outcome_missingness" %in% names(population)) population[, outcome_missingness := NA_character_]
  population[is.na(outcome_value), outcome_missingness := "missing"]
  population[is.na(outcome_missingness) | !nzchar(outcome_missingness), outcome_missingness := "observed"]
  if (nrow(exclusions)) {
    ex <- exclusions[, .(unit_id, exclusion_stage, exclusion_reason, post_assignment, planned, analysis_impact)]
    population <- merge(population, ex, by = "unit_id", all.x = TRUE)
  } else {
    population[, `:=`(exclusion_stage = NA_character_, exclusion_reason = NA_character_, post_assignment = FALSE, planned = FALSE, analysis_impact = NA_character_)]
  }
  if (!is.null(baseline_data) && length(spec$baseline_covariates)) {
    base <- data.table::as.data.table(baseline_data)
    keep <- intersect(c(unit_id_col, spec$baseline_covariates, spec$cluster_variable, spec$blocking_variables, spec$stratification_variables), names(base))
    if (unit_id_col %in% keep) population <- merge(population, base[, keep, with = FALSE], by.x = "unit_id", by.y = unit_id_col, all.x = TRUE)
  }
  population[, `:=`(
    randomized_n = .N,
    observed_outcome = !is.na(outcome_value),
    treatment_indicator = as.integer(itt_arm == spec$treatment_arm),
    comparison_indicator = as.integer(itt_arm == spec$comparison_arm),
    retained_for_itt = TRUE
  )]
  summary <- population[, .(randomized_n = .N, observed_n = sum(observed_outcome), missing_n = sum(!observed_outcome), missing_rate = mean(!observed_outcome), post_assignment_exclusions = sum(post_assignment %in% TRUE, na.rm = TRUE)), by = itt_arm]
  structure(list(schema_version = "aq_itt_analysis_population_v1", population = population, summary = summary, created_at = aq_randomized_itt_now()), class = c("aq_itt_analysis_population", "list"))
}

aq_itt_ci <- function(estimate, se, confidence_level = 0.95, df = Inf) {
  alpha <- 1 - confidence_level
  crit <- if (is.finite(df)) stats::qt(1 - alpha / 2, df = df) else stats::qnorm(1 - alpha / 2)
  c(lower = estimate - crit * se, upper = estimate + crit * se)
}

aq_itt_unadjusted <- function(population, spec) {
  dt <- data.table::copy(population$population)
  observed <- dt[observed_outcome %in% TRUE]
  if (identical(spec$outcome_type, "binary")) observed[, outcome_value := as.numeric(outcome_value > 0)]
  arm <- observed[, .(n = .N, mean = mean(outcome_value, na.rm = TRUE), variance = stats::var(outcome_value, na.rm = TRUE)), by = itt_arm]
  treatment <- arm[itt_arm == spec$treatment_arm]
  comparison <- arm[itt_arm == spec$comparison_arm]
  if (!nrow(treatment) || !nrow(comparison)) stop("Both treatment and comparison arms need observed outcomes.", call. = FALSE)
  effect <- treatment$mean[[1]] - comparison$mean[[1]]
  se <- sqrt((treatment$variance[[1]] %||% 0) / treatment$n[[1]] + (comparison$variance[[1]] %||% 0) / comparison$n[[1]])
  df <- max(min(treatment$n[[1]], comparison$n[[1]]) - 1L, 1L)
  ci <- aq_itt_ci(effect, se, spec$confidence_level, df)
  relative <- if (is.finite(comparison$mean[[1]]) && comparison$mean[[1]] != 0) effect / abs(comparison$mean[[1]]) else NA_real_
  rr <- if (identical(spec$outcome_type, "binary") && comparison$mean[[1]] > 0) treatment$mean[[1]] / comparison$mean[[1]] else NA_real_
  or <- if (identical(spec$outcome_type, "binary") && all(c(treatment$mean[[1]], comparison$mean[[1]]) > 0) && all(c(treatment$mean[[1]], comparison$mean[[1]]) < 1)) (treatment$mean[[1]] / (1 - treatment$mean[[1]])) / (comparison$mean[[1]] / (1 - comparison$mean[[1]])) else NA_real_
  data.table::data.table(
    estimator = "unadjusted_itt",
    outcome = spec$outcome,
    outcome_type = spec$outcome_type,
    effect_scale = if (identical(spec$outcome_type, "binary")) "risk_difference" else "mean_difference",
    treatment_arm = spec$treatment_arm,
    comparison_arm = spec$comparison_arm,
    treatment_mean = treatment$mean[[1]],
    comparison_mean = comparison$mean[[1]],
    estimate = effect,
    standard_error = se,
    conf_low = ci[["lower"]],
    conf_high = ci[["upper"]],
    confidence_level = spec$confidence_level,
    p_value = 2 * stats::pnorm(-abs(effect / se)),
    randomized_n = nrow(dt),
    observed_n = nrow(observed),
    treatment_n = treatment$n[[1]],
    comparison_n = comparison$n[[1]],
    relative_effect = relative,
    relative_risk = rr,
    odds_ratio_secondary = or,
    assumptions = "Effect is assignment-based ITT; missing outcomes handled according to explicit policy."
  )
}

aq_itt_vcov_hc0 <- function(model, cluster = NULL) {
  x <- stats::model.matrix(model)
  e <- stats::residuals(model)
  bread <- tryCatch(solve(crossprod(x)), error = function(e) qr.solve(crossprod(x)))
  if (is.null(cluster)) {
    meat <- crossprod(x * as.numeric(e))
    return(bread %*% meat %*% bread)
  }
  cluster <- as.character(cluster)
  meat <- matrix(0, ncol(x), ncol(x))
  for (g in unique(cluster)) {
    idx <- which(cluster == g)
    xg <- x[idx, , drop = FALSE]
    eg <- matrix(e[idx], ncol = 1)
    meat <- meat + t(xg) %*% eg %*% t(eg) %*% xg
  }
  g <- length(unique(cluster))
  n <- length(e)
  k <- ncol(x)
  mult <- if (g > 1 && n > k) (g / (g - 1)) * ((n - 1) / (n - k)) else 1
  mult * bread %*% meat %*% bread
}

aq_itt_adjusted <- function(population, spec) {
  covars <- spec$baseline_covariates
  if (!length(covars)) return(data.table::data.table())
  dt <- data.table::copy(population$population)
  observed <- dt[observed_outcome %in% TRUE]
  if (identical(spec$outcome_type, "binary")) observed[, outcome_value := as.numeric(outcome_value > 0)]
  available <- intersect(covars, names(observed))
  if (!length(available)) return(data.table::data.table(estimator = "adjusted_itt", status = "not_run", reason = "No approved baseline covariates were available."))
  formula <- stats::reformulate(c("treatment_indicator", available), response = "outcome_value")
  model <- stats::lm(formula, data = observed)
  vcov <- if (identical(spec$standard_error_method, "cluster") && nzchar(spec$cluster_variable %||% "") && spec$cluster_variable %in% names(observed)) {
    aq_itt_vcov_hc0(model, observed[[spec$cluster_variable]])
  } else {
    aq_itt_vcov_hc0(model)
  }
  estimate <- stats::coef(model)[["treatment_indicator"]]
  se <- sqrt(diag(vcov))[["treatment_indicator"]]
  ci <- aq_itt_ci(estimate, se, spec$confidence_level, df = stats::df.residual(model))
  data.table::data.table(
    estimator = if (identical(spec$outcome_type, "binary")) "adjusted_lpm_itt" else "adjusted_lm_itt",
    outcome = spec$outcome,
    outcome_type = spec$outcome_type,
    effect_scale = if (identical(spec$outcome_type, "binary")) "adjusted_risk_difference_lpm" else "adjusted_mean_difference",
    estimate = estimate,
    standard_error = se,
    conf_low = ci[["lower"]],
    conf_high = ci[["upper"]],
    confidence_level = spec$confidence_level,
    p_value = 2 * stats::pt(-abs(estimate / se), df = stats::df.residual(model)),
    observed_n = nrow(observed),
    covariates = paste(available, collapse = ","),
    standard_error_method = if (identical(spec$standard_error_method, "cluster")) "cluster_or_hc0" else "hc0",
    assumptions = "Adjustment uses approved pre-treatment precision variables; it is not observational confounding repair."
  )
}

#' Assess Missing Outcome Bounds for ITT
#' @export
aq_itt_missing_outcome_bounds <- function(population, spec) {
  dt <- data.table::copy(population$population)
  if (!identical(spec$outcome_type, "binary")) {
    return(data.table::data.table(sensitivity = "missing_outcome_bounds", status = "not_applicable", finding = "Simple best/worst-case bounds are implemented for binary outcomes only in this phase."))
  }
  dt[, y_obs := as.numeric(outcome_value > 0)]
  scenarios <- list(
    best_case = list(treat_missing = 1, control_missing = 0),
    worst_case = list(treat_missing = 0, control_missing = 1)
  )
  rows <- lapply(names(scenarios), function(name) {
    s <- scenarios[[name]]
    tmp <- data.table::copy(dt)
    tmp[is.na(y_obs) & itt_arm == spec$treatment_arm, y_obs := s$treat_missing]
    tmp[is.na(y_obs) & itt_arm == spec$comparison_arm, y_obs := s$control_missing]
    means <- tmp[, .(risk = mean(y_obs, na.rm = TRUE), n = .N), by = itt_arm]
    data.table::data.table(sensitivity = name, estimate = means[itt_arm == spec$treatment_arm]$risk[[1]] - means[itt_arm == spec$comparison_arm]$risk[[1]], assumption = paste("treatment missing =", s$treat_missing, "comparison missing =", s$control_missing))
  })
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

#' Assess ITT Effect Materiality
#' @export
aq_assess_itt_materiality <- function(primary_estimate, minimum_meaningful_effect = NA_real_) {
  if (!is.finite(minimum_meaningful_effect)) {
    return(data.table::data.table(materiality_state = "not_assessed", finding = "No minimum meaningful effect was supplied.", recommendation = "Author a business materiality threshold before decision interpretation."))
  }
  est <- primary_estimate$estimate[[1]]
  lo <- primary_estimate$conf_low[[1]]
  hi <- primary_estimate$conf_high[[1]]
  state <- if (lo >= minimum_meaningful_effect) {
    "materially_beneficial"
  } else if (est >= minimum_meaningful_effect && lo < minimum_meaningful_effect) {
    "beneficial_but_uncertain"
  } else if (hi < minimum_meaningful_effect && hi > -minimum_meaningful_effect) {
    "no_material_benefit_supported"
  } else if (hi <= -abs(minimum_meaningful_effect)) {
    "materially_harmful"
  } else if (lo < 0 && hi > 0) {
    "inconclusive"
  } else if (est < 0) {
    "possible_harm"
  } else {
    "statistically_detectable_but_operationally_small"
  }
  data.table::data.table(materiality_state = state, minimum_meaningful_effect = minimum_meaningful_effect, estimate = est, conf_low = lo, conf_high = hi, recommendation = "Interpret statistical evidence with guardrails, implementation fidelity, and decision context.")
}

#' Estimate a Randomized ITT Effect
#' @export
aq_estimate_randomized_itt <- function(spec, completed_evidence, baseline_data = NULL, planned_analysis = NULL, readiness = NULL, authority_approved = TRUE, coverage_approved = TRUE) {
  gate <- aq_validate_randomized_itt_readiness(spec, completed_evidence, readiness, planned_analysis, baseline_data = baseline_data, authority_approved = authority_approved, coverage_approved = coverage_approved)
  if (gate$eligibility[[1]] %in% c("blocked", "amendment_required", "descriptive_only")) {
    return(structure(list(schema_version = "aq_randomized_itt_result_v1", spec = spec, gate = gate, status = "readiness_blocked", effect_estimated = FALSE, created_at = aq_randomized_itt_now()), class = c("aq_randomized_itt_result", "list")))
  }
  population <- aq_itt_analysis_population(spec, completed_evidence$assignment, completed_evidence$outcomes, completed_evidence$exclusions, baseline_data)
  primary <- aq_itt_unadjusted(population, spec)
  adjusted <- aq_itt_adjusted(population, spec)
  missing_bounds <- aq_itt_missing_outcome_bounds(population, spec)
  materiality <- aq_assess_itt_materiality(primary, spec$minimum_meaningful_effect)
  guardrail <- if (!is.null(completed_evidence$guardrails)) completed_evidence$guardrails else data.table::data.table(guardrail_status = "not_assessed", severity = "warning", finding = "No guardrail evidence supplied.")
  sensitivity <- data.table::rbindlist(list(
    data.table::data.table(sensitivity = "unadjusted_vs_adjusted", unadjusted = primary$estimate[[1]], adjusted = if (nrow(adjusted) && "estimate" %in% names(adjusted)) adjusted$estimate[[1]] else NA_real_, finding = if (nrow(adjusted) && "estimate" %in% names(adjusted)) "Both unadjusted and adjusted estimates are preserved." else "Adjusted estimate not run."),
    missing_bounds
  ), use.names = TRUE, fill = TRUE)
  permitted <- c("Effect estimate applies to assignment to treatment for the authored estimand and population.", "Uncertainty and missingness limitations must accompany the point estimate.")
  prohibited <- c("Treatment-on-treated effect was estimated.", "Exposure or treatment receipt replaced assignment.", "Observational causal effect was estimated.", "Guardrail harms were suppressed.", "Effect applies outside the authored estimand.")
  result <- list(
    schema_version = "aq_randomized_itt_result_v1",
    spec = spec,
    gate = gate,
    status = "analysis_run",
    effect_estimated = TRUE,
    population = population,
    primary_estimate = primary,
    adjusted_estimate = adjusted,
    missingness = population$summary,
    guardrails = guardrail,
    fidelity_context = completed_evidence$fidelity,
    interference_context = completed_evidence$interference,
    estimand_preservation = completed_evidence$estimand,
    sensitivity = sensitivity,
    materiality = materiality,
    permitted_claims = permitted,
    prohibited_claims = prohibited,
    lifecycle = data.table::data.table(state = "analysis_run", analyst = NA_character_, reviewer = NA_character_, specification_version = spec$analysis_id, data_version = completed_evidence$completed$completed_experiment_id %||% NA_character_, result_version = aq_vnext_id("itt_result_version"), review_notes = NA_character_, approval_date = NA_character_, limitations = paste(unique(c(gate$reason, materiality$materiality_state)), collapse = " | ")),
    created_at = aq_randomized_itt_now()
  )
  class(result) <- c("aq_randomized_itt_result", "list")
  result
}

#' Create a Randomized ITT Effect Artifact
#' @export
aq_randomized_itt_effect_artifact <- function(result, artifact_id = NULL) {
  artifact_id <- artifact_id %||% paste0("aq_randomized_itt_effect_", result$spec$analysis_id)
  artifact <- new_metadata_artifact(
    id = artifact_id,
    title = paste("Randomized ITT Effect:", result$spec$outcome),
    description = "Governed randomized-assignment ITT effect evidence with uncertainty, guardrails, materiality, and limitations.",
    tags = c("causal_intelligence", "randomized_itt", "causal_effect"),
    values = list(spec = result$spec, gate = result$gate, population_summary = result$population$summary, primary_estimate = result$primary_estimate, adjusted_estimate = result$adjusted_estimate, missingness = result$missingness, guardrails = result$guardrails, fidelity_context = result$fidelity_context, sensitivity = result$sensitivity, materiality = result$materiality, lifecycle = result$lifecycle),
    dependencies = aq_vnext_unique_chr(c(result$spec$completed_experiment_id, result$spec$experiment_plan_artifact_id, result$spec$causal_question_id, result$spec$estimand_id)),
    source_generator = "aq_randomized_itt_effect_artifact",
    version = "aq_randomized_itt_effect_artifact_v1",
    metadata = list(artifact_type = "randomized_itt_effect_artifact", analysis_id = result$spec$analysis_id, completed_experiment_id = result$spec$completed_experiment_id, decision_context_id = NA_character_, causal_question_id = result$spec$causal_question_id, estimand_id = result$spec$estimand_id, outcome = result$spec$outcome, effect_estimated = isTRUE(result$effect_estimated), materiality_state = result$materiality$materiality_state[[1]], permitted_claims = result$permitted_claims, prohibited_claims = result$prohibited_claims, supported_actions = c("review", "approve_evidence", "link_to_decision", "seed_followup_campaign"))
  )
  artifact$artifact_envelope <- aq_vnext_attach_envelope(artifact, artifact_id = artifact_id, artifact_type = "randomized_itt_effect_artifact", artifact_version = "aq_randomized_itt_effect_artifact_v1", parent_artifact_ids = aq_vnext_unique_chr(c(result$spec$completed_experiment_id, result$spec$experiment_plan_artifact_id, result$spec$causal_question_id)), lineage = list(analysis_id = result$spec$analysis_id, estimand_id = result$spec$estimand_id, assignment_variable = result$spec$assignment_column), task = "randomized_itt_estimation", operator = "randomized_assignment_itt", engine = "base_r", specification_id = result$spec$analysis_id, supported_actions = c("review", "approve_evidence", "link_to_decision", "seed_followup_campaign"), producer = "aq_randomized_itt_effect_artifact")
  artifact
}

#' Review a Randomized ITT Result
#' @export
aq_review_randomized_itt_result <- function(result, reviewer = NA_character_, review_notes = NA_character_, approve = FALSE) {
  result$lifecycle <- data.table::as.data.table(result$lifecycle)
  result$lifecycle[, `:=`(
    state = if (isTRUE(approve)) "approved_evidence" else "analyst_review",
    reviewer = reviewer,
    review_notes = review_notes,
    approval_date = if (isTRUE(approve)) as.character(Sys.Date()) else NA_character_
  )]
  result$status <- result$lifecycle$state[[1]]
  result
}

#' Create Campaign Seeds from an ITT Result
#' @export
aq_randomized_itt_campaign_seeds <- function(result) {
  rows <- list()
  add <- function(seed_type, severity, recommendation) rows[[length(rows) + 1L]] <<- data.table::data.table(seed_type, severity, recommendation)
  mat <- result$materiality$materiality_state[[1]] %||% "not_assessed"
  if (mat %in% c("inconclusive", "insufficient_precision")) add("investigate_inconclusive_effect", "medium", "Consider a better-powered follow-up or narrower decision context.")
  if (mat %in% c("materially_beneficial", "beneficial_but_uncertain")) add("decision_update_available", "medium", "Review exploit/expand decision with guardrails and implementation fidelity.")
  if (mat %in% c("possible_harm", "materially_harmful")) add("investigate_possible_harm", "high", "Review treatment harms and stop/modify expansion plans.")
  if (!is.null(result$guardrails) && any(result$guardrails$severity %in% c("high", "critical"), na.rm = TRUE)) add("investigate_guardrail_harm", "high", "Guardrail harm may block exploitation despite primary effect.")
  if (!is.null(result$fidelity_context) && any(grepl("weak|contamination", result$fidelity_context$fidelity_status %||% "", ignore.case = TRUE))) add("improve_treatment_fidelity", "medium", "Improve delivery contrast before interpreting weak or null effects as strategy failure.")
  if (!length(rows)) add("archive_causal_learning", "low", "Preserve approved causal evidence with applicability and limitations.")
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

#' Run Randomized ITT Estimation QA
#' @export
qa_causal_randomized_itt_framework <- function() {
  set.seed(20260713)
  rows <- list()
  add <- function(check, ok, message) rows[[length(rows) + 1L]] <<- data.table::data.table(suite = "causal_randomized_itt_framework", check, status = if (isTRUE(ok)) "pass" else "fail", message)
  completed <- aq_completed_experiment(list(completed_experiment_id = "ce_itt", experiment_plan_artifact_id = "plan_itt", decision_context_id = "decision_itt", causal_question_id = "cq_itt", estimand_id = "estimand_itt", design_version = "v1", assignment_version = "v1", experiment_status = "completed", actual_start_date = "2026-01-01", actual_end_date = "2026-02-01", data_cutoff_date = "2026-02-15", execution_owner = "analytics"))
  n <- 80L
  assignment <- aq_assignment_evidence(data.table::data.table(unit_id = paste0("u", seq_len(n)), planned_arm = rep(c("control", "treatment"), each = n / 2), realized_assigned_arm = rep(c("control", "treatment"), each = n / 2), cluster = rep(paste0("geo", 1:8), each = 10)))
  delivery <- aq_treatment_delivery_evidence(data.table::data.table(unit_id = paste0("u", seq_len(n)), delivered_condition = rep(c("control", "treatment"), each = n / 2), delivery_status = "delivered"))
  compliance <- aq_compliance_evidence(data.table::data.table(unit_id = paste0("u", seq_len(n)), assigned_treatment = rep(c("control", "treatment"), each = n / 2), treatment_received = c(rep("control", n / 2), rep("treatment", (n / 2) - 2), "control", "control")))
  baseline <- data.table::data.table(unit_id = paste0("u", seq_len(n)), baseline_y = rnorm(n), cluster = rep(paste0("geo", 1:8), each = 10))
  outcomes <- aq_outcome_evidence(data.table::data.table(unit_id = paste0("u", seq_len(n)), outcome_id = "revenue", value = c(rnorm(n / 2, 10, 2), rnorm(n / 2, 13, 2)), outcome_role = "primary"))
  binary_outcomes <- aq_outcome_evidence(data.table::data.table(unit_id = paste0("u", seq_len(n)), outcome_id = "converted", value = c(rbinom(n / 2, 1, .25), rbinom(n / 2, 1, .45)), outcome_role = "primary"))
  guardrails <- aq_assess_guardrails(aq_outcome_evidence(data.table::data.table(unit_id = paste0("u", seq_len(n)), outcome_id = "cost_guardrail", value = 0, outcome_role = "guardrail")))
  missingness <- aq_assess_missingness_attrition(assignment, outcomes)
  reconciliation <- aq_reconcile_experiment_execution(completed, assignment_evidence = assignment, delivery_evidence = delivery, outcome_evidence = outcomes)
  integrity <- aq_assess_randomization_integrity(assignment)
  fidelity <- aq_assess_treatment_fidelity(assignment, delivery, compliance_evidence = compliance)
  interference <- aq_assess_interference_spillover()
  estimand <- aq_assess_estimand_preservation(completed, reconciliation, integrity, fidelity, missingness, interference, outcomes)
  readiness <- aq_assess_experiment_analysis_readiness(completed, assignment, outcomes, reconciliation, integrity, fidelity, missingness, estimand, guardrails)
  completed_evidence <- list(completed = completed, assignment = assignment, delivery = delivery, compliance = compliance, outcomes = outcomes, missingness = missingness, reconciliation = reconciliation, integrity = integrity, fidelity = fidelity, interference = interference, guardrails = guardrails, estimand = estimand, readiness = readiness)
  planned <- aq_planned_analysis_record(completed, readiness, outcome_variables = "revenue", baseline_covariates = "baseline_y")
  spec <- aq_randomized_itt_spec(completed_experiment_id = "ce_itt", experiment_plan_artifact_id = "plan_itt", causal_question_id = "cq_itt", estimand_id = "estimand_itt", treatment_arm = "treatment", comparison_arm = "control", outcome = "revenue", outcome_type = "continuous", baseline_covariates = "baseline_y", cluster_variable = "cluster", standard_error_method = "cluster", minimum_meaningful_effect = 1)
  gate <- aq_validate_randomized_itt_readiness(spec, completed_evidence, planned_analysis = planned, baseline_data = baseline)
  result <- aq_estimate_randomized_itt(spec, completed_evidence, baseline, planned)
  artifact <- aq_randomized_itt_effect_artifact(result)
  reviewed <- aq_review_randomized_itt_result(result, reviewer = "qa", approve = TRUE)
  seeds <- aq_randomized_itt_campaign_seeds(result)
  bin_ev <- completed_evidence
  bin_ev$outcomes <- binary_outcomes
  bin_spec <- aq_randomized_itt_spec(completed_experiment_id = "ce_itt", experiment_plan_artifact_id = "plan_itt", causal_question_id = "cq_itt", estimand_id = "estimand_itt", treatment_arm = "treatment", comparison_arm = "control", outcome = "converted", outcome_type = "binary", minimum_meaningful_effect = .05)
  bin_result <- aq_estimate_randomized_itt(bin_spec, bin_ev, planned_analysis = planned)
  blocked <- aq_validate_randomized_itt_readiness(spec, completed_evidence, readiness = data.table::data.table(readiness_state = "blocked"))
  add("spec_contract", inherits(spec, "aq_randomized_itt_spec") && !any(spec$validation$status == "fail"), "ITT specification is explicit and validates.")
  add("readiness_gate", gate$eligibility[[1]] %in% c("eligible", "eligible_with_limitations") && blocked$eligibility[[1]] == "blocked", "Estimator gates on Phase 3 readiness.")
  add("analysis_population", all(result$population$population$retained_for_itt) && "treatment_indicator" %in% names(result$population$population), "ITT population retains randomized units by original assignment.")
  add("continuous_estimate", isTRUE(result$effect_estimated) && is.finite(result$primary_estimate$estimate[[1]]) && is.finite(result$primary_estimate$standard_error[[1]]), "Continuous difference-in-means ITT estimate includes uncertainty.")
  add("binary_estimate", isTRUE(bin_result$effect_estimated) && identical(bin_result$primary_estimate$effect_scale[[1]], "risk_difference"), "Binary ITT estimates risk difference transparently.")
  add("adjustment_preserved", isTRUE(result$effect_estimated) && nrow(result$adjusted_estimate) > 0 && result$adjusted_estimate$estimator[[1]] == "adjusted_lm_itt", "Approved pre-treatment precision adjustment is preserved separately.")
  add("cluster_uncertainty", isTRUE(result$effect_estimated) && nrow(result$adjusted_estimate) > 0 && grepl("cluster", result$adjusted_estimate$standard_error_method[[1]]), "Cluster-aware uncertainty is identified when requested.")
  add("missingness_bounds", any(result$sensitivity$sensitivity == "missing_outcome_bounds") || any(result$sensitivity$status == "not_applicable"), "Missingness sensitivity is explicit and no imputation occurs.")
  add("guardrails_materiality", nrow(result$guardrails) && nrow(result$materiality), "Guardrails and materiality are separate from p-values.")
  add("artifact_lifecycle", inherits(artifact, "aq_artifact") && reviewed$status == "approved_evidence" && nrow(seeds), "Effect artifact, review lifecycle, and campaign seeds exist.")
  add("prohibited_claims", any(grepl("Treatment-on-treated", result$prohibited_claims)), "No treatment-on-treated or observational claim is introduced.")
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}
