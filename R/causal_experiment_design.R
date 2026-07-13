# AutoQuant governed experiment-design planning for Causal Intelligence Phase 2.

aq_experiment_supported_designs <- function() {
  c(
    "individual_randomized_ab",
    "stratified_randomized",
    "blocked_randomized",
    "cluster_randomized",
    "geographic_randomized",
    "switchback",
    "stepped_wedge",
    "factorial",
    "randomized_encouragement"
  )
}

aq_experiment_status_levels <- function() {
  c(
    "draft", "design_incomplete", "feasibility_review", "causally_ready",
    "operational_review", "authority_review", "measurement_ready",
    "approval_required", "approved", "blocked", "superseded", "canceled"
  )
}

aq_experiment_design_catalog <- function() {
  data.table::data.table(
    design_type = aq_experiment_supported_designs(),
    design_family = c("randomized", "randomized", "randomized", "clustered", "clustered", "temporal", "rollout", "multi_factor", "encouragement"),
    typical_assignment_unit = c("individual", "individual", "block_member", "cluster", "geography", "unit_period", "cluster_period", "unit", "individual"),
    common_estimand = c("ATE/ITT", "ATE/ITT", "ATE/ITT", "cluster-level ITT", "market-level ITT", "period policy effect", "rollout ITT", "main effects/interactions", "encouragement ITT"),
    primary_risks = c(
      "noncompliance, contamination",
      "sparse strata, noncompliance",
      "block imbalance, operational complexity",
      "few clusters, within-cluster interference",
      "spillover, few geographies, market heterogeneity",
      "carryover, seasonality, treatment reversibility",
      "calendar confounding, rollout constraints",
      "multiple testing, interaction power",
      "weak encouragement, exclusion restriction"
    )
  )
}

#' Create an Experiment Question
#'
#' @param question Experiment question record or list.
#' @param causal_context Optional `aq_causal_context`.
#' @param experiment_question_id Optional ID override.
#'
#' @return An `aq_experiment_question` object.
#' @export
aq_experiment_question <- function(question, causal_context = NULL, experiment_question_id = NULL) {
  question <- aq_causal_record_dt(question, "experiment_question_id", "experiment_question")
  if (nrow(question) != 1L) stop("question must describe exactly one experiment question.", call. = FALSE)
  if (!is.null(experiment_question_id)) question[, experiment_question_id := as.character(experiment_question_id)[1L]]
  required <- c(
    "causal_question_id", "decision_context_id", "hypothesis", "null_claim",
    "alternative_claim", "treatment", "comparison", "estimand", "assignment_population",
    "expected_mechanism", "primary_outcome", "decision_rule", "authority", "coverage"
  )
  for (col in setdiff(required, names(question))) question[, (col) := NA_character_]
  for (col in c("secondary_outcomes", "guardrails", "evidence_references", "limitations")) {
    if (!col %in% names(question)) question[, (col) := list(character())]
  }
  out <- list(
    experiment_question_id = question$experiment_question_id[[1]],
    schema_version = "aq_experiment_question_v1",
    question = question,
    causal_context = causal_context,
    created_at = aq_vnext_now()
  )
  out$validation <- aq_validate_experiment_question(out)
  class(out) <- c("aq_experiment_question", "list")
  out
}

#' Validate an Experiment Question
#' @export
aq_validate_experiment_question <- function(experiment_question) {
  rows <- list()
  add <- function(check, status, message, severity = status, recommendation = NA_character_) {
    rows[[length(rows) + 1L]] <<- data.table::data.table(check = check, status = status, severity = severity, message = message, recommendation = recommendation)
  }
  if (!is.list(experiment_question) || is.null(experiment_question$question)) {
    add("experiment_question_object", "fail", "experiment_question must be created by aq_experiment_question().")
    return(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
  }
  q <- experiment_question$question[1]
  required <- c("experiment_question_id", "causal_question_id", "decision_context_id", "hypothesis", "treatment", "comparison", "estimand", "assignment_population", "primary_outcome", "decision_rule", "authority", "coverage")
  for (col in required) {
    if (!col %in% names(q) || !nzchar(aq_causal_chr1(q[[col]], ""))) add(paste0("missing_", col), "fail", paste(col, "is required."), recommendation = paste("Author", col, "before experiment design."))
  }
  if (identical(aq_causal_chr1(q$treatment), aq_causal_chr1(q$comparison))) add("treatment_equals_comparison", "fail", "Treatment and comparison cannot be identical.")
  if (!nzchar(aq_causal_chr1(q$guardrails, ""))) add("guardrails_missing", "warning", "No guardrails are attached.", "warning", "Add safety, cost, fairness, quality, customer, or operational guardrails.")
  if (!length(rows)) add("experiment_question_valid", "pass", "Experiment question is decision-linked and ready for design specification.", "pass")
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

#' Create an Experimental Design Specification
#' @export
aq_experiment_design_spec <- function(
  experiment_question,
  design_type,
  assignment_unit,
  analysis_unit,
  treatment_delivery_unit = assignment_unit,
  cluster_unit = NA_character_,
  blocking_variables = character(),
  stratification_variables = character(),
  number_of_arms = 2L,
  allocation_ratios = c(control = 0.5, treatment = 0.5),
  rollout_schedule = NA_character_,
  washout_period = NA_character_,
  pre_period = NA_character_,
  treatment_period = NA_character_,
  follow_up_period = NA_character_,
  randomization_method = "deterministic_pseudo_random",
  seed = 20260713L,
  exposure_rules = NA_character_,
  compliance_expectations = NA_character_,
  contamination_risks = character(),
  interference_assumptions = "not yet assessed",
  planned_estimator_family = "future_intention_to_treat",
  supported_estimands = character(),
  validity_limitations = character(),
  design_id = NULL
) {
  if (!inherits(experiment_question, "aq_experiment_question")) stop("experiment_question must be created by aq_experiment_question().", call. = FALSE)
  out <- list(
    design_id = aq_vnext_default(design_id, aq_vnext_id("experiment_design")),
    schema_version = "aq_experiment_design_spec_v1",
    experiment_question_id = experiment_question$experiment_question_id,
    causal_question_id = experiment_question$question$causal_question_id[[1]],
    design_type = as.character(design_type)[1L],
    assignment_unit = as.character(assignment_unit)[1L],
    analysis_unit = as.character(analysis_unit)[1L],
    treatment_delivery_unit = as.character(treatment_delivery_unit)[1L],
    cluster_unit = aq_vnext_default(cluster_unit, NA_character_),
    blocking_variables = aq_vnext_unique_chr(blocking_variables),
    stratification_variables = aq_vnext_unique_chr(stratification_variables),
    number_of_arms = as.integer(number_of_arms)[1L],
    allocation_ratios = allocation_ratios,
    rollout_schedule = aq_vnext_default(rollout_schedule, NA_character_),
    washout_period = aq_vnext_default(washout_period, NA_character_),
    pre_period = aq_vnext_default(pre_period, NA_character_),
    treatment_period = aq_vnext_default(treatment_period, NA_character_),
    follow_up_period = aq_vnext_default(follow_up_period, NA_character_),
    randomization_method = randomization_method,
    seed = as.integer(seed)[1L],
    exposure_rules = aq_vnext_default(exposure_rules, NA_character_),
    compliance_expectations = aq_vnext_default(compliance_expectations, NA_character_),
    contamination_risks = aq_vnext_unique_chr(contamination_risks),
    interference_assumptions = aq_vnext_default(interference_assumptions, "not yet assessed"),
    planned_estimator_family = planned_estimator_family,
    supported_estimands = aq_vnext_unique_chr(supported_estimands),
    validity_limitations = aq_vnext_unique_chr(validity_limitations),
    created_at = aq_vnext_now()
  )
  out$validation <- aq_validate_experiment_design_spec(out, experiment_question)
  class(out) <- c("aq_experiment_design_spec", "list")
  out
}

#' Validate an Experimental Design Specification
#' @export
aq_validate_experiment_design_spec <- function(design_spec, experiment_question = NULL) {
  rows <- list()
  add <- function(check, status, message, severity = status, recommendation = NA_character_) {
    rows[[length(rows) + 1L]] <<- data.table::data.table(check = check, status = status, severity = severity, message = message, recommendation = recommendation)
  }
  if (!design_spec$design_type %in% aq_experiment_supported_designs()) add("unsupported_design_type", "fail", paste("Unsupported design type:", design_spec$design_type))
  for (field in c("assignment_unit", "analysis_unit", "treatment_delivery_unit")) {
    if (!nzchar(aq_causal_chr1(design_spec[[field]], ""))) add(paste0("missing_", field), "fail", paste(field, "is required."))
  }
  if (design_spec$number_of_arms < 2L) add("too_few_arms", "fail", "At least two arms are required.")
  ratios <- suppressWarnings(as.numeric(design_spec$allocation_ratios))
  if (!length(ratios) || any(!is.finite(ratios)) || any(ratios <= 0) || abs(sum(ratios) - 1) > 0.02) add("allocation_ratios", "fail", "Allocation ratios must be positive and sum to approximately 1.")
  if (design_spec$design_type %in% c("cluster_randomized", "geographic_randomized", "stepped_wedge") && !nzchar(aq_causal_chr1(design_spec$cluster_unit, ""))) add("cluster_unit_required", "fail", "Cluster/geographic/rollout designs require a cluster unit.")
  if (design_spec$design_type == "switchback" && !nzchar(aq_causal_chr1(design_spec$washout_period, ""))) add("washout_missing", "warning", "Switchback design has no washout period.", "warning", "Add washout/carryover justification.")
  if (length(design_spec$contamination_risks)) add("contamination_declared", "warning", "Contamination risks are declared and must be mitigated.", "warning")
  if (!length(rows)) add("design_spec_valid", "pass", "Design specification is structurally valid.", "pass")
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

#' Assess Experimental Design Readiness
#' @export
aq_assess_experiment_design_eligibility <- function(experiment_question, causal_context = NULL, candidate_designs = aq_experiment_supported_designs(), evidence = list()) {
  rows <- lapply(candidate_designs, function(design) {
    reasons <- character()
    status <- "ready with conditions"
    if (!isTRUE(evidence$intervention_controllable %||% FALSE)) {
      status <- "feasibility study required"
      reasons <- c(reasons, "Intervention controllability has not been confirmed.")
    }
    if (!isTRUE(evidence$assignment_feasible %||% FALSE)) {
      status <- "feasibility study required"
      reasons <- c(reasons, "Assignment feasibility has not been confirmed.")
    }
    if (design %in% c("cluster_randomized", "geographic_randomized", "stepped_wedge") && !isTRUE(evidence$clusters_available %||% FALSE)) {
      status <- "insufficient information"
      reasons <- c(reasons, "Cluster/geography availability is missing.")
    }
    if (design == "switchback" && !isTRUE(evidence$treatment_reversible %||% FALSE)) {
      status <- "operationally blocked"
      reasons <- c(reasons, "Switchback requires reversible treatment delivery.")
    }
    if (design == "randomized_encouragement" && !isTRUE(evidence$encouragement_possible %||% FALSE)) {
      status <- "insufficient information"
      reasons <- c(reasons, "Encouragement delivery is not established.")
    }
    if (isTRUE(evidence$high_spillover_risk %||% FALSE) && design %in% c("individual_randomized_ab", "stratified_randomized", "blocked_randomized")) {
      status <- "causally inappropriate"
      reasons <- c(reasons, "High spillover risk conflicts with individual assignment.")
    }
    if (isTRUE(evidence$authority_available %||% FALSE) && isTRUE(evidence$measurement_ready %||% FALSE) && status == "ready with conditions") {
      status <- "ready"
      reasons <- c(reasons, "Authority and measurement readiness are currently satisfied.")
    }
    if (!length(reasons)) reasons <- "Ready subject to authored assumptions."
    data.table::data.table(
      design_type = design,
      readiness = status,
      reasons = paste(reasons, collapse = " | "),
      recommendation = switch(status,
        ready = "Proceed to experiment specification and approval gate.",
        `ready with conditions` = "Complete stated conditions before approval.",
        `feasibility study required` = "Collect operational feasibility evidence.",
        `operationally blocked` = "Do not submit for approval until operational blocker is resolved.",
        `causally inappropriate` = "Choose a design that matches interference and assignment structure.",
        `insufficient information` = "Author missing design evidence.",
        "Review design assumptions."
      )
    )
  })
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

#' Create a Deterministic Assignment Plan
#' @export
aq_assignment_plan <- function(
  design_spec,
  eligible_units = NULL,
  unit_id_col = "unit_id",
  arms = c("control", "treatment"),
  allocation_ratios = design_spec$allocation_ratios,
  strata_cols = design_spec$stratification_variables,
  block_cols = design_spec$blocking_variables,
  cluster_col = design_spec$cluster_unit,
  exclusion_col = NULL,
  assignment_version = "v1",
  seed = design_spec$seed
) {
  if (!inherits(design_spec, "aq_experiment_design_spec")) stop("design_spec must be created by aq_experiment_design_spec().", call. = FALSE)
  eligible_units <- if (is.null(eligible_units)) {
    data.table::data.table(unit_id = character())
  } else {
    data.table::as.data.table(eligible_units)
  }
  assignment <- data.table::data.table()
  warnings <- character()
  if (nrow(eligible_units)) {
    if (!unit_id_col %in% names(eligible_units)) stop("unit_id_col must exist in eligible_units.", call. = FALSE)
    units <- data.table::copy(eligible_units)
    excluded <- if (!is.null(exclusion_col) && exclusion_col %in% names(units)) as.logical(units[[exclusion_col]]) else rep(FALSE, nrow(units))
    units[, .assignment_row := seq_len(.N)]
    assignable <- units[!excluded]
    if (anyDuplicated(assignable[[unit_id_col]])) warnings <- c(warnings, "Duplicate assignable unit IDs were detected.")
    set.seed(as.integer(seed)[1L])
    ratios <- as.numeric(allocation_ratios)
    ratios <- ratios / sum(ratios)
    arm_names <- names(allocation_ratios) %||% arms
    if (length(arm_names) != length(ratios)) arm_names <- arms[seq_along(ratios)]
    assignable[, .rand := stats::runif(.N)]
    if (length(strata_cols) && all(strata_cols %in% names(assignable))) {
      assignable[, arm := {
        n <- .N
        idx <- order(.rand)
        cuts <- cumsum(round(ratios * n))
        cuts[length(cuts)] <- n
        out <- rep(arm_names, pmax(diff(c(0, cuts)), 0))
        out <- out[seq_len(n)]
        assigned <- character(n)
        assigned[idx] <- out
        assigned
      }, by = strata_cols]
    } else {
      if (length(strata_cols)) warnings <- c(warnings, "Requested strata columns were not all available; assignment used global allocation.")
      n <- nrow(assignable)
      idx <- order(assignable$.rand)
      cuts <- cumsum(round(ratios * n))
      cuts[length(cuts)] <- n
      out <- rep(arm_names, pmax(diff(c(0, cuts)), 0))[seq_len(n)]
      assignable[, arm := NA_character_]
      assignable[idx, arm := out]
    }
    assignment <- assignable[, .(
      unit_id = as.character(get(unit_id_col)),
      arm = arm,
      block = if (length(block_cols) && block_cols[[1]] %in% names(assignable)) as.character(get(block_cols[[1]])) else NA_character_,
      stratum = if (length(strata_cols) && strata_cols[[1]] %in% names(assignable)) as.character(get(strata_cols[[1]])) else NA_character_,
      cluster = if (nzchar(aq_causal_chr1(cluster_col, "")) && cluster_col %in% names(assignable)) as.character(get(cluster_col)) else NA_character_,
      assignment_reason = "deterministic pseudo-random proposal"
    )]
    excluded_units <- units[excluded, .(
      unit_id = as.character(get(unit_id_col)),
      arm = "excluded",
      block = NA_character_,
      stratum = NA_character_,
      cluster = NA_character_,
      assignment_reason = "excluded by eligibility rule"
    )]
    assignment <- data.table::rbindlist(list(assignment, excluded_units), use.names = TRUE, fill = TRUE)
  }
  out <- list(
    assignment_plan_id = aq_vnext_id("assignment_plan"),
    schema_version = "aq_assignment_plan_v1",
    design_id = design_spec$design_id,
    assignment_version = assignment_version,
    seed = as.integer(seed)[1L],
    arms = aq_vnext_unique_chr(arms),
    allocation_ratios = allocation_ratios,
    unit_id_col = unit_id_col,
    strata_cols = aq_vnext_unique_chr(strata_cols),
    block_cols = aq_vnext_unique_chr(block_cols),
    cluster_col = aq_vnext_default(cluster_col, NA_character_),
    assignment = assignment[, assignment_timestamp := aq_vnext_now()],
    warnings = aq_vnext_unique_chr(warnings),
    created_at = aq_vnext_now()
  )
  class(out) <- c("aq_assignment_plan", "list")
  out
}

#' Assess Assignment Balance
#' @export
aq_assess_assignment_balance <- function(assignment_plan, baseline_data = NULL, unit_id_col = "unit_id", baseline_cols = character()) {
  assignment <- data.table::copy(assignment_plan$assignment)
  if (!nrow(assignment)) {
    return(data.table::data.table(check = "assignment_empty", status = "warning", finding = "No assignment table was generated.", recommendation = "Supply eligible units before balance assessment."))
  }
  rows <- list()
  add <- function(check, status, finding, recommendation = NA_character_) rows[[length(rows) + 1L]] <<- data.table::data.table(check, status, finding, recommendation)
  counts <- assignment[, .N, by = arm]
  add("treatment_counts", "info", paste(paste(counts$arm, counts$N, sep = "="), collapse = ", "), "Review allocation counts before approval.")
  active <- counts[arm != "excluded"]
  if (nrow(active) && min(active$N) == 0L) add("empty_arm", "error", "At least one active arm has no assigned units.", "Revise allocation or eligible population.")
  if ("stratum" %in% names(assignment) && any(nzchar(assignment$stratum %||% ""), na.rm = TRUE)) {
    sparse <- assignment[arm != "excluded", .N, by = .(stratum, arm)][N < 2L]
    if (nrow(sparse)) add("sparse_strata", "warning", paste(nrow(sparse), "sparse stratum-arm cells detected."), "Collapse strata or increase eligible units.")
  }
  if (!is.null(baseline_data) && length(baseline_cols)) {
    dt <- merge(assignment[arm != "excluded"], data.table::as.data.table(baseline_data), by = unit_id_col, all.x = TRUE)
    for (col in intersect(baseline_cols, names(dt))) {
      if (is.numeric(dt[[col]]) && length(unique(dt$arm)) == 2L) {
        by_arm <- dt[, .(mean = mean(get(col), na.rm = TRUE), sd = stats::sd(get(col), na.rm = TRUE), n = .N), by = arm]
        pooled <- sqrt(mean(by_arm$sd^2, na.rm = TRUE))
        smd <- if (is.finite(pooled) && pooled > 0) diff(by_arm$mean[seq_len(2L)]) / pooled else NA_real_
        add(paste0("baseline_balance_", col), if (!is.na(smd) && abs(smd) > 0.25) "warning" else "pass", paste("standardized difference =", round(smd, 3)), "Use only pre-treatment variables for balance checks.")
      }
    }
  }
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

#' Create a Power and Precision Plan
#' @export
aq_power_plan <- function(
  outcome_type = c("continuous", "binary"),
  baseline_mean = NA_real_,
  baseline_sd = NA_real_,
  baseline_rate = NA_real_,
  minimum_detectable_effect = NA_real_,
  alpha = 0.05,
  target_power = 0.8,
  allocation_ratio = 1,
  total_n = NA_integer_,
  clusters = NA_integer_,
  intraclass_correlation = NA_real_,
  attrition_allowance = 0,
  design_effect = NA_real_,
  repeated_measure_r2 = NA_real_,
  assumptions_source = "user_supplied"
) {
  outcome_type <- match.arg(outcome_type)
  missing <- character()
  if (!is.finite(minimum_detectable_effect)) missing <- c(missing, "minimum_detectable_effect")
  if (outcome_type == "continuous" && !is.finite(baseline_sd)) missing <- c(missing, "baseline_sd")
  if (outcome_type == "binary" && !is.finite(baseline_rate)) missing <- c(missing, "baseline_rate")
  estimate <- NA_real_
  status <- "missing assumptions"
  if (!length(missing)) {
    z_alpha <- stats::qnorm(1 - alpha / 2)
    z_power <- stats::qnorm(target_power)
    if (outcome_type == "continuous") {
      estimate <- 2 * ((z_alpha + z_power) * baseline_sd / minimum_detectable_effect)^2
    } else {
      p <- baseline_rate
      estimate <- 2 * ((z_alpha + z_power)^2 * p * (1 - p)) / (minimum_detectable_effect^2)
    }
    if (is.finite(repeated_measure_r2)) estimate <- estimate * max(0.05, 1 - repeated_measure_r2)
    if (is.finite(design_effect)) estimate <- estimate * design_effect
    if (is.finite(intraclass_correlation) && is.finite(clusters) && clusters > 1) estimate <- estimate * (1 + (max(total_n %||% estimate, estimate) / clusters - 1) * intraclass_correlation)
    estimate <- ceiling(estimate / max(1 - attrition_allowance, 0.01))
    status <- "calculated"
  }
  out <- list(
    power_plan_id = aq_vnext_id("power_plan"),
    schema_version = "aq_power_plan_v1",
    outcome_type = outcome_type,
    baseline_mean = baseline_mean,
    baseline_sd = baseline_sd,
    baseline_rate = baseline_rate,
    minimum_detectable_effect = minimum_detectable_effect,
    alpha = alpha,
    target_power = target_power,
    allocation_ratio = allocation_ratio,
    total_n = total_n,
    clusters = clusters,
    intraclass_correlation = intraclass_correlation,
    attrition_allowance = attrition_allowance,
    design_effect = design_effect,
    repeated_measure_r2 = repeated_measure_r2,
    required_total_n = estimate,
    required_information = missing,
    status = status,
    assumptions_source = assumptions_source,
    warning = if (length(missing)) "Power was not calculated because required assumptions are missing." else "Power planning is approximate and does not guarantee validity.",
    created_at = aq_vnext_now()
  )
  class(out) <- c("aq_power_plan", "list")
  out
}

#' Create a Timing Plan
#' @export
aq_experiment_timing_plan <- function(
  earliest_start = Sys.Date(),
  treatment_duration_days = NA_integer_,
  outcome_maturation_days = 0L,
  reporting_delay_days = 0L,
  washout_days = 0L,
  pre_period_days = 0L,
  follow_up_days = 0L,
  decision_deadline = NA,
  seasonality_notes = character(),
  blackout_periods = character(),
  known_events = character()
) {
  start <- as.Date(earliest_start)
  total_days <- sum(suppressWarnings(as.integer(c(treatment_duration_days, outcome_maturation_days, reporting_delay_days, washout_days, pre_period_days, follow_up_days))), na.rm = TRUE)
  ready <- start + total_days
  conflicts <- character()
  if (!is.na(as.Date(decision_deadline)) && ready > as.Date(decision_deadline)) conflicts <- c(conflicts, "Evidence-ready date is after the decision deadline.")
  if (length(blackout_periods)) conflicts <- c(conflicts, "Operational blackout periods require scheduling review.")
  if (length(seasonality_notes)) conflicts <- c(conflicts, "Seasonality notes require interpretation guardrails.")
  data.table::data.table(
    timing_plan_id = aq_vnext_id("timing_plan"),
    earliest_feasible_start = as.character(start),
    estimated_evidence_ready_date = as.character(ready),
    total_calendar_days = total_days,
    timing_conflicts = paste(conflicts, collapse = " | "),
    seasonality_notes = paste(seasonality_notes, collapse = " | "),
    blackout_periods = paste(blackout_periods, collapse = " | "),
    known_events = paste(known_events, collapse = " | ")
  )
}

#' Create a Measurement Plan
#' @export
aq_measurement_plan <- function(
  primary_outcome,
  secondary_outcomes = character(),
  guardrails = character(),
  exposure_verification = NA_character_,
  compliance_measure = NA_character_,
  treatment_receipt = NA_character_,
  manipulation_checks = character(),
  data_source,
  numerator = NA_character_,
  denominator = NA_character_,
  aggregation = NA_character_,
  measurement_cadence = NA_character_,
  outcome_window = NA_character_,
  missingness_handling = NA_character_,
  censoring = NA_character_,
  data_latency = NA_character_,
  quality_threshold = NA_character_,
  owner = NA_character_,
  source_of_truth_status = "unknown"
) {
  data.table::data.table(
    measurement_plan_id = aq_vnext_id("measurement_plan"),
    primary_outcome = primary_outcome,
    secondary_outcomes = paste(aq_vnext_unique_chr(secondary_outcomes), collapse = ","),
    guardrails = paste(aq_vnext_unique_chr(guardrails), collapse = ","),
    exposure_verification = exposure_verification,
    compliance_measure = compliance_measure,
    treatment_receipt = treatment_receipt,
    manipulation_checks = paste(aq_vnext_unique_chr(manipulation_checks), collapse = ","),
    data_source = data_source,
    numerator = numerator,
    denominator = denominator,
    aggregation = aggregation,
    measurement_cadence = measurement_cadence,
    outcome_window = outcome_window,
    missingness_handling = missingness_handling,
    censoring = censoring,
    data_latency = data_latency,
    quality_threshold = quality_threshold,
    owner = owner,
    source_of_truth_status = source_of_truth_status,
    assignment_exposure_compliance_distinguished = all(nzchar(c(exposure_verification, compliance_measure, treatment_receipt)))
  )
}

#' Build a Validity-Threat Register
#' @export
aq_validity_threat_register <- function(threats = NULL, design_spec = NULL, measurement_plan = NULL) {
  base <- data.table::data.table(
    threat_id = c("assignment_failure", "noncompliance", "contamination", "interference", "carryover", "attrition", "multiple_testing", "insufficient_precision", "weak_treatment_representation", "kpi_mismatch"),
    validity_family = c(rep("internal", 6), rep("statistical", 2), rep("construct", 2)),
    threat = c("Assignment failure", "Noncompliance", "Contamination", "Interference/spillover", "Carryover", "Attrition", "Multiple testing", "Insufficient precision", "Weak treatment representation", "KPI mismatch"),
    severity = "needs_review",
    evidence = NA_character_,
    mitigation = NA_character_,
    owner = NA_character_,
    unresolved = TRUE
  )
  if (!is.null(threats)) {
    supplied <- data.table::as.data.table(threats)
    base <- data.table::rbindlist(list(base, supplied), use.names = TRUE, fill = TRUE)
  }
  if (!is.null(design_spec) && design_spec$design_type == "switchback") {
    base[threat_id == "carryover", `:=`(severity = "high", evidence = "Switchback design selected.", mitigation = "Specify washout and carryover checks.")]
  }
  if (!is.null(design_spec) && length(design_spec$contamination_risks)) {
    base[threat_id == "contamination", `:=`(severity = "high", evidence = paste(design_spec$contamination_risks, collapse = ", "), mitigation = "Add separation, cluster design, or exposure monitoring.")]
  }
  if (!is.null(measurement_plan) && !isTRUE(measurement_plan$assignment_exposure_compliance_distinguished[[1]])) {
    base[threat_id == "noncompliance", `:=`(severity = "high", evidence = "Exposure/compliance/treatment receipt are not fully distinguished.", mitigation = "Add treatment receipt and compliance measures.")]
  }
  unique(base, by = "threat_id")
}

#' Assess Interference and Spillover
#' @export
aq_interference_plan <- function(mode = c("no_interference_assumed", "within_cluster_allowed", "cross_cluster_risk", "geographic_spillover", "network_spillover", "channel_contamination", "treatment_substitution", "competitive_response"), design_spec = NULL) {
  mode <- match.arg(mode)
  recommendation <- switch(mode,
    no_interference_assumed = "Document why interference is not expected; do not assume silently.",
    within_cluster_allowed = "Prefer cluster assignment and cluster-aware analysis.",
    cross_cluster_risk = "Consider geographic/cluster separation or spillover estimand.",
    geographic_spillover = "Use buffer regions, geo separation, and exposure measurement.",
    network_spillover = "Measure network exposure or redesign assignment clusters.",
    channel_contamination = "Separate channels or measure cross-channel exposure.",
    treatment_substitution = "Measure substitution behavior and guardrails.",
    competitive_response = "Track external response and limit transfer claims."
  )
  data.table::data.table(interference_mode = mode, recommendation = recommendation, sutva_assumed = identical(mode, "no_interference_assumed"))
}

#' Assess Experiment Approval Gates
#' @export
aq_experiment_gate_assessment <- function(experiment_question, design_spec, assignment_plan = NULL, power_plan = NULL, measurement_plan = NULL, validity_threats = NULL, authority_approved = FALSE, coverage_approved = FALSE, information_value = NULL) {
  blockers <- character()
  warnings <- character()
  if (any(experiment_question$validation$status == "fail")) blockers <- c(blockers, "Experiment question is incomplete.")
  if (any(design_spec$validation$status == "fail")) blockers <- c(blockers, "Design specification has structural failures.")
  if (is.null(measurement_plan) || !nrow(measurement_plan)) blockers <- c(blockers, "Measurement plan is missing.")
  if (is.null(power_plan) || identical(power_plan$status, "missing assumptions")) warnings <- c(warnings, "Power assumptions are incomplete.")
  if (!is.null(validity_threats) && any(validity_threats$severity %in% c("high", "critical"), na.rm = TRUE)) warnings <- c(warnings, "High validity threats require mitigation.")
  if (!isTRUE(authority_approved)) blockers <- c(blockers, "Authority approval is missing.")
  if (!isTRUE(coverage_approved)) warnings <- c(warnings, "Coverage approval or organizational scope review is missing.")
  gate <- if (length(blockers)) "approval_required" else if (length(warnings)) "operational_review" else "approved"
  data.table::data.table(
    gate_status = gate,
    blockers = paste(blockers, collapse = " | "),
    warnings = paste(warnings, collapse = " | "),
    execution_ready = FALSE,
    recommendation = if (identical(gate, "approved")) "Plan is approved as a design artifact only; treatment execution remains out of scope." else "Resolve blockers and warnings before execution planning."
  )
}

#' Assess Experiment Information Value
#' @export
aq_experiment_information_value <- function(decision_sensitivity = "unknown", lever_importance = "unknown", experiment_cost = NA_real_, duration_days = NA_integer_, reversibility = NA, optionality_created = character(), opportunity_cost = NA_real_) {
  score <- 0L
  score <- score + if (decision_sensitivity == "high") 3L else if (decision_sensitivity == "medium") 2L else if (decision_sensitivity == "low") 1L else 0L
  score <- score + if (lever_importance == "high") 3L else if (lever_importance == "medium") 2L else if (lever_importance == "low") 1L else 0L
  if (isTRUE(reversibility)) score <- score + 1L
  if (length(optionality_created)) score <- score + 1L
  if (isTRUE(is.finite(experiment_cost)) && experiment_cost > 100000) score <- score - 1L
  if (isTRUE(is.finite(duration_days)) && duration_days > 90) score <- score - 1L
  outcome <- if (score >= 6L) "high-value exploration" else if (score >= 4L) "useful but not urgent" else if (score >= 2L) "exploit current evidence instead" else "insufficient information"
  data.table::data.table(
    information_value_status = outcome,
    score = score,
    decision_sensitivity = decision_sensitivity,
    lever_importance = lever_importance,
    experiment_cost = experiment_cost,
    duration_days = duration_days,
    reversibility = reversibility,
    optionality_created = paste(optionality_created, collapse = ","),
    opportunity_cost = opportunity_cost,
    recommendation = switch(outcome,
      `high-value exploration` = "Experiment planning is worth continuing subject to governance.",
      `useful but not urgent` = "Experiment may be useful after cheaper evidence is reviewed.",
      `exploit current evidence instead` = "Remaining uncertainty may not justify experiment cost.",
      "Author missing value-of-information inputs."
    )
  )
}

#' Create a Canonical Experiment Plan Artifact
#' @export
aq_experiment_plan_artifact <- function(
  experiment_question,
  design_spec,
  assignment_plan = NULL,
  power_plan = NULL,
  timing_plan = NULL,
  measurement_plan = NULL,
  validity_threats = NULL,
  interference_plan = NULL,
  gate_assessment = NULL,
  information_value = NULL,
  artifact_id = NULL
) {
  artifact_id <- aq_vnext_default(artifact_id, paste0("aq_experiment_plan_", experiment_question$experiment_question_id))
  artifact <- new_metadata_artifact(
    id = artifact_id,
    title = paste("Governed Experiment Plan:", experiment_question$experiment_question_id),
    description = paste("Experiment plan for", experiment_question$experiment_question_id, "using", design_spec$design_type),
    tags = c("causal_intelligence", "experiment_design", "planning_only"),
    values = list(
      experiment_question = experiment_question$question,
      design_spec = design_spec,
      assignment_plan = assignment_plan,
      power_plan = power_plan,
      timing_plan = timing_plan,
      measurement_plan = measurement_plan,
      validity_threats = validity_threats,
      interference_plan = interference_plan,
      gate_assessment = gate_assessment,
      information_value = information_value
    ),
    dependencies = aq_vnext_unique_chr(c(experiment_question$experiment_question_id, experiment_question$question$causal_question_id[[1]], design_spec$design_id)),
    source_generator = "aq_experiment_plan_artifact",
    version = "aq_experiment_plan_artifact_v1",
    metadata = list(
      artifact_type = "causal_experiment_plan",
      experiment_question_id = experiment_question$experiment_question_id,
      causal_question_id = experiment_question$question$causal_question_id[[1]],
      decision_context_id = experiment_question$question$decision_context_id[[1]],
      design_type = design_spec$design_type,
      gate_status = if (!is.null(gate_assessment)) gate_assessment$gate_status[[1]] else "draft",
      no_treatment_executed = TRUE,
      no_effect_estimated = TRUE,
      supported_actions = c("validate", "submit_for_approval", "register_artifact", "future_analyze_completed_experiment"),
      prohibited_claims = c("treatment was executed", "causal effect was estimated", "experiment result is known")
    )
  )
  artifact$artifact_envelope <- aq_vnext_attach_envelope(
    artifact,
    artifact_id = artifact_id,
    artifact_type = "causal_experiment_plan",
    artifact_version = "aq_experiment_plan_artifact_v1",
    parent_artifact_ids = aq_vnext_unique_chr(c(experiment_question$question$decision_context_id[[1]], experiment_question$question$causal_question_id[[1]], experiment_question$experiment_question_id)),
    lineage = list(
      experiment_question_id = experiment_question$experiment_question_id,
      causal_question_id = experiment_question$question$causal_question_id[[1]],
      design_id = design_spec$design_id,
      no_treatment_executed = TRUE,
      no_effect_estimated = TRUE
    ),
    task = "governed_experiment_design_planning",
    operator = "deterministic_experiment_planner",
    engine = "none",
    specification_id = design_spec$design_id,
    supported_actions = c("validate", "submit_for_approval", "register_artifact"),
    producer = "aq_experiment_plan_artifact"
  )
  artifact
}

qa_causal_experiment_design_framework <- function() {
  rows <- list()
  add <- function(check, ok, message) rows[[length(rows) + 1L]] <<- data.table::data.table(suite = "causal_experiment_design_framework", check = check, status = if (isTRUE(ok)) "pass" else "fail", message = message)
  cq <- aq_causal_question(list(
    causal_question_id = "cq_budget_revenue",
    decision_context_id = "decision_budget",
    exposure = "budget",
    outcome = "revenue",
    population = "eligible markets",
    unit_of_analysis = "market-week",
    time_zero = "campaign start",
    treatment_window = "four weeks",
    outcome_window = "six weeks",
    comparison_condition = "current budget",
    intervention_definition = "increase eligible-market budget from current range to approved test range",
    estimand = "ATE",
    effect_scale = "incremental revenue",
    target_population = "eligible markets"
  ))
  context <- aq_causal_context(cq, assumptions = c("intervention controllable", "measurement available"))
  eq <- aq_experiment_question(list(
    experiment_question_id = "eq_budget_test",
    causal_question_id = "cq_budget_revenue",
    decision_context_id = "decision_budget",
    hypothesis = "Increasing budget raises revenue.",
    null_claim = "No revenue lift.",
    alternative_claim = "Revenue lift is positive.",
    treatment = "approved budget increase",
    comparison = "current budget",
    estimand = "ATE",
    assignment_population = "eligible markets",
    expected_mechanism = "More qualified reach creates more revenue.",
    primary_outcome = "revenue",
    guardrails = "cpa,customer_quality",
    decision_rule = "Approve rollout if lift exceeds cost and guardrails hold.",
    authority = "marketing_approval",
    coverage = "eligible_markets"
  ), context)
  design <- aq_experiment_design_spec(eq, "stratified_randomized", assignment_unit = "market", analysis_unit = "market-week", stratification_variables = "region", treatment_period = "four weeks", pre_period = "eight weeks")
  units <- data.table::data.table(unit_id = paste0("m", 1:40), region = rep(c("East", "West"), each = 20), baseline_revenue = rnorm(40, 100, 10))
  assignment <- aq_assignment_plan(design, units, strata_cols = "region")
  balance <- aq_assess_assignment_balance(assignment, units, baseline_cols = "baseline_revenue")
  power_missing <- aq_power_plan(outcome_type = "continuous", minimum_detectable_effect = 5)
  power <- aq_power_plan(outcome_type = "continuous", baseline_sd = 12, minimum_detectable_effect = 5)
  timing <- aq_experiment_timing_plan(treatment_duration_days = 28, outcome_maturation_days = 14, reporting_delay_days = 7, pre_period_days = 56, decision_deadline = Sys.Date() + 120)
  measurement <- aq_measurement_plan("revenue", guardrails = c("cpa", "customer_quality"), exposure_verification = "spend delivery logs", compliance_measure = "budget adherence", treatment_receipt = "delivered spend", data_source = "analytics warehouse", owner = "analytics")
  threats <- aq_validity_threat_register(design_spec = design, measurement_plan = measurement)
  interference <- aq_interference_plan("geographic_spillover", design)
  eligibility <- aq_assess_experiment_design_eligibility(eq, context, evidence = list(intervention_controllable = TRUE, assignment_feasible = TRUE, authority_available = TRUE, measurement_ready = TRUE, clusters_available = TRUE, treatment_reversible = TRUE, encouragement_possible = TRUE))
  info <- aq_experiment_information_value("high", "high", experiment_cost = 1000, duration_days = 49, reversibility = TRUE, optionality_created = "rollout_or_stop")
  gate <- aq_experiment_gate_assessment(eq, design, assignment, power, measurement, threats, authority_approved = TRUE, coverage_approved = TRUE, information_value = info)
  artifact <- aq_experiment_plan_artifact(eq, design, assignment, power, timing, measurement, threats, interference, gate, info)
  add("experiment_question", inherits(eq, "aq_experiment_question") && !any(eq$validation$status == "fail"), "Decision-linked experiment question validates.")
  add("design_spec", inherits(design, "aq_experiment_design_spec") && !any(design$validation$status == "fail"), "Design spec distinguishes assignment, delivery, and analysis units.")
  add("design_eligibility", all(aq_experiment_supported_designs() %in% eligibility$design_type), "Design eligibility covers supported design families.")
  add("assignment_deterministic", identical(assignment$assignment$arm, aq_assignment_plan(design, units, strata_cols = "region")$assignment$arm), "Assignment proposals are deterministic under the same seed.")
  add("balance_diagnostics", any(balance$check == "treatment_counts"), "Balance diagnostics include treatment counts and baseline checks.")
  add("power_missing_inputs", identical(power_missing$status, "missing assumptions") && "baseline_sd" %in% power_missing$required_information, "Power planning reports missing inputs instead of fabricating them.")
  add("power_calculation", identical(power$status, "calculated") && is.finite(power$required_total_n), "Power planning calculates only when required assumptions exist.")
  add("measurement_distinctions", isTRUE(measurement$assignment_exposure_compliance_distinguished[[1]]), "Measurement plan distinguishes assignment, exposure, compliance, treatment receipt, outcomes, and guardrails.")
  add("validity_threats", all(c("interference", "noncompliance", "multiple_testing") %in% threats$threat_id), "Validity register covers internal, statistical, and construct threats.")
  add("interference_first_class", identical(interference$interference_mode[[1]], "geographic_spillover"), "Interference and spillover are explicit planning objects.")
  add("approval_gates", !isTRUE(gate$execution_ready) && gate$gate_status[[1]] %in% aq_experiment_status_levels(), "Approval gates never imply treatment execution readiness.")
  add("information_value", identical(info$information_value_status[[1]], "high-value exploration"), "Information value links experiment planning to explore/exploit relevance.")
  add("plan_artifact", inherits(artifact, "aq_artifact") && isTRUE(artifact$metadata$no_treatment_executed) && isTRUE(artifact$metadata$no_effect_estimated), "Experiment plan artifact preserves lineage and prohibits effect claims.")
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}
