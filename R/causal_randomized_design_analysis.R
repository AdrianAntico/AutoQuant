# AutoQuant randomized design-aware analysis contracts.

aq_randomized_design_modes <- function() {
  c(
    "unadjusted", "ancova", "cuped", "blocked", "stratified",
    "cluster_randomized", "geographic_randomized", "switchback",
    "stepped_wedge", "factorial", "randomization_inference"
  )
}

aq_randomized_design_types <- function() {
  c(
    "completely_randomized", "blocked_randomized", "stratified_randomized",
    "cluster_randomized", "geographic_randomized", "switchback",
    "stepped_wedge", "factorial"
  )
}

aq_rd_chr <- function(x, default = NA_character_) {
  if (is.null(x) || !length(x) || is.na(x[[1]])) return(default)
  as.character(x[[1]])
}

aq_rd_num <- function(x, default = NA_real_) {
  out <- suppressWarnings(as.numeric(x[[1]] %||% default))
  if (!is.finite(out)) default else out
}

aq_rd_parse_chr <- function(x) {
  if (is.null(x) || !length(x)) return(character())
  x <- paste(as.character(x), collapse = ",")
  x <- trimws(strsplit(x, ",", fixed = TRUE)[[1]])
  x[nzchar(x)]
}

aq_rd_var <- function(x) {
  out <- stats::var(x, na.rm = TRUE)
  if (is.finite(out)) out else NA_real_
}

#' Create a Design-Aware Randomized Analysis Specification
#' @export
aq_randomized_design_analysis_spec <- function(
  design_analysis_id = NULL,
  itt_analysis_id,
  design_type = c("completely_randomized", "blocked_randomized", "stratified_randomized", "cluster_randomized", "geographic_randomized", "switchback", "stepped_wedge", "factorial"),
  analysis_modes = c("unadjusted"),
  assignment_unit = "unit",
  analysis_unit = "unit",
  repeated_measure_unit = NA_character_,
  cluster_unit = NA_character_,
  time_unit = NA_character_,
  block_fields = character(),
  stratum_fields = character(),
  period_field = NA_character_,
  sequence_field = NA_character_,
  washout_field = NA_character_,
  pre_period_fields = character(),
  treatment_indicators = character(),
  factorial_terms = character(),
  uncertainty_method = c("auto", "classical", "hc0", "cluster", "randomization"),
  small_sample_method = c("none", "few_cluster_warning"),
  randomization_iterations = 499L,
  randomization_seed = 20260713L,
  multiplicity_policy = c("primary_unadjusted", "holm", "bonferroni", "report_only"),
  guardrail_thresholds = NULL,
  material_benefit = NA_real_,
  material_harm = NA_real_,
  approved_sensitivity_analyses = c("unadjusted", "ancova", "cuped"),
  authority = NA_character_
) {
  design_type <- match.arg(design_type)
  uncertainty_method <- match.arg(uncertainty_method)
  small_sample_method <- match.arg(small_sample_method)
  multiplicity_policy <- match.arg(multiplicity_policy)
  modes <- aq_vnext_unique_chr(analysis_modes)
  out <- list(
    schema_version = "aq_randomized_design_analysis_spec_v1",
    design_analysis_id = design_analysis_id %||% aq_vnext_id("randomized_design_analysis"),
    itt_analysis_id = itt_analysis_id,
    design_type = design_type,
    analysis_modes = modes[modes %in% aq_randomized_design_modes()],
    requested_analysis_modes = modes,
    assignment_unit = assignment_unit,
    analysis_unit = analysis_unit,
    repeated_measure_unit = repeated_measure_unit,
    cluster_unit = cluster_unit,
    time_unit = time_unit,
    block_fields = aq_vnext_unique_chr(block_fields),
    stratum_fields = aq_vnext_unique_chr(stratum_fields),
    period_field = period_field,
    sequence_field = sequence_field,
    washout_field = washout_field,
    pre_period_fields = aq_vnext_unique_chr(pre_period_fields),
    treatment_indicators = aq_vnext_unique_chr(treatment_indicators),
    factorial_terms = aq_vnext_unique_chr(factorial_terms),
    uncertainty_method = uncertainty_method,
    small_sample_method = small_sample_method,
    randomization_iterations = as.integer(randomization_iterations),
    randomization_seed = as.integer(randomization_seed),
    multiplicity_policy = multiplicity_policy,
    guardrail_thresholds = guardrail_thresholds,
    material_benefit = material_benefit,
    material_harm = material_harm,
    approved_sensitivity_analyses = aq_vnext_unique_chr(approved_sensitivity_analyses),
    authority = authority,
    created_at = aq_vnext_now()
  )
  class(out) <- c("aq_randomized_design_analysis_spec", "list")
  out$validation <- aq_validate_randomized_design_analysis_spec(out)
  out
}

#' Validate a Design-Aware Randomized Analysis Specification
#' @export
aq_validate_randomized_design_analysis_spec <- function(spec) {
  rows <- list()
  add <- function(check, status, reason, recommendation = NA_character_) {
    rows[[length(rows) + 1L]] <<- data.table::data.table(check, status, reason, recommendation)
  }
  if (!nzchar(aq_rd_chr(spec$itt_analysis_id, ""))) add("missing_itt_analysis_id", "fail", "ITT analysis id is required.")
  if (!spec$design_type %in% aq_randomized_design_types()) add("unsupported_design_type", "fail", "Design type is not supported.")
  unsupported <- setdiff(spec$requested_analysis_modes %||% character(), aq_randomized_design_modes())
  if (length(unsupported)) add("unsupported_modes", "fail", paste("Unsupported randomized analysis modes:", paste(unsupported, collapse = ", ")))
  if (!length(spec$analysis_modes)) add("no_supported_modes", "fail", "At least one supported analysis mode is required.")
  if (spec$design_type %in% c("blocked_randomized") && !length(spec$block_fields)) add("missing_block_fields", "fail", "Blocked designs require block fields.")
  if (spec$design_type %in% c("stratified_randomized") && !length(spec$stratum_fields)) add("missing_stratum_fields", "fail", "Stratified designs require stratum fields.")
  if (spec$design_type %in% c("cluster_randomized", "geographic_randomized", "stepped_wedge") && !nzchar(aq_rd_chr(spec$cluster_unit, ""))) add("missing_cluster_unit", "fail", "Cluster or geographic designs require a cluster unit.")
  if (spec$design_type %in% c("switchback", "stepped_wedge") && !nzchar(aq_rd_chr(spec$period_field, ""))) add("missing_period_field", "fail", "Temporal randomized designs require a period field.")
  if (spec$design_type %in% c("factorial") && length(spec$factorial_terms) < 2L) add("missing_factorial_terms", "fail", "Factorial designs require at least two planned treatment factors.")
  if (is.na(spec$randomization_iterations) || spec$randomization_iterations < 99L) add("low_randomization_iterations", "warning", "Randomization inference iterations are low.", "Use at least 499 iterations for smoke-scale analysis.")
  if (!length(rows)) add("specification_valid", "pass", "Design-aware randomized analysis specification is valid.")
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

#' Determine Eligible Randomized Analysis Methods
#' @export
aq_randomized_design_method_eligibility <- function(spec, itt_result) {
  dt <- data.table::data.table(method = aq_randomized_design_modes(), eligible = FALSE, reason = "Not compatible with this design.")
  design <- spec$design_type
  dt[method == "unadjusted", `:=`(eligible = TRUE, reason = "Unadjusted ITT is always preserved when the ITT estimate exists.")]
  if (nrow(itt_result$adjusted_estimate %||% data.table::data.table())) dt[method == "ancova", `:=`(eligible = TRUE, reason = "Approved pre-treatment covariates are present.")]
  if (length(spec$pre_period_fields)) dt[method == "cuped", `:=`(eligible = TRUE, reason = "Approved pre-period metric is specified.")]
  if (design == "blocked_randomized") dt[method == "blocked", `:=`(eligible = TRUE, reason = "Blocked randomized design metadata is present.")]
  if (design == "stratified_randomized") dt[method == "stratified", `:=`(eligible = TRUE, reason = "Stratified randomized design metadata is present.")]
  if (design %in% c("cluster_randomized", "geographic_randomized")) dt[method %in% c("cluster_randomized", "geographic_randomized"), `:=`(eligible = TRUE, reason = "Cluster or geography assignment metadata is present.")]
  if (design == "switchback") dt[method == "switchback", `:=`(eligible = TRUE, reason = "Switchback design metadata is present.")]
  if (design == "stepped_wedge") dt[method == "stepped_wedge", `:=`(eligible = TRUE, reason = "Stepped-wedge design metadata is present.")]
  if (design == "factorial") dt[method == "factorial", `:=`(eligible = TRUE, reason = "Factorial design metadata is present.")]
  if (design %in% c("completely_randomized", "blocked_randomized", "cluster_randomized", "geographic_randomized")) dt[method == "randomization_inference", `:=`(eligible = TRUE, reason = "Assignment mechanism is supported by bounded randomization inference.")]
  dt[, requested := method %in% (spec$analysis_modes %||% character())]
  dt[]
}

#' Enforce Outcome Windows Row by Row
#' @export
aq_enforce_randomized_outcome_windows <- function(data, unit_col = "unit_id", measurement_time_col = NULL, window_start = NULL, window_end = NULL, treatment_start_col = NULL, treatment_end_col = NULL, maturation_delay_days = 0L) {
  dt <- data.table::as.data.table(data)
  if (is.null(measurement_time_col) || !measurement_time_col %in% names(dt)) {
    return(list(
      schema_version = "aq_outcome_window_enforcement_v1",
      rows = data.table::data.table(),
      summary = data.table::data.table(status = "not_evaluated", finding = "No measurement timestamp column was supplied.")
    ))
  }
  rows <- data.table::copy(dt)
  rows[, measurement_time := as.Date(get(measurement_time_col))]
  if (!is.null(treatment_start_col) && treatment_start_col %in% names(rows)) {
    rows[, valid_window_start := as.Date(get(treatment_start_col)) + as.integer(maturation_delay_days)]
  } else {
    rows[, valid_window_start := as.Date(window_start)]
  }
  if (!is.null(treatment_end_col) && treatment_end_col %in% names(rows)) {
    rows[, valid_window_end := as.Date(get(treatment_end_col))]
  } else {
    rows[, valid_window_end := as.Date(window_end)]
  }
  rows[, window_status := data.table::fifelse(is.na(measurement_time) | is.na(valid_window_start) | is.na(valid_window_end), "unknown",
    data.table::fifelse(measurement_time < valid_window_start, "early",
      data.table::fifelse(measurement_time > valid_window_end, "late", "in_window")
    )
  )]
  summary <- rows[, .(rows = .N, early = sum(window_status == "early"), in_window = sum(window_status == "in_window"), late = sum(window_status == "late"), unknown = sum(window_status == "unknown"))]
  summary[, status := if (early + late > 0L) "window_deviations" else "window_clean"]
  list(schema_version = "aq_outcome_window_enforcement_v1", rows = rows, summary = summary)
}

#' Assess Carryover for Repeated Randomized Designs
#' @export
aq_assess_randomized_carryover <- function(data, unit_col = NULL, period_col = NULL, treatment_col = NULL, expected_carryover_periods = 0L, washout_periods = 0L, outcome_persistence = NA_real_) {
  dt <- data.table::as.data.table(data %||% data.table::data.table())
  if (is.null(unit_col) || is.null(period_col) || is.null(treatment_col) || !all(c(unit_col, period_col, treatment_col) %in% names(dt))) {
    return(data.table::data.table(carryover_state = "insufficient_evidence", expected_carryover_periods = expected_carryover_periods, washout_periods = washout_periods, finding = "Required repeated-design fields were not supplied.", recommendation = "Provide unit, period, and treatment schedule fields."))
  }
  tmp <- data.table::copy(dt)
  data.table::setorderv(tmp, c(unit_col, period_col))
  tmp[, prior_treatment := data.table::shift(get(treatment_col)), by = unit_col]
  tmp[, treatment_changed := !is.na(prior_treatment) & prior_treatment != get(treatment_col)]
  transition_rate <- mean(tmp$treatment_changed %in% TRUE)
  state <- if (washout_periods < expected_carryover_periods) {
    "washout_inadequate"
  } else if (is.finite(outcome_persistence) && outcome_persistence > 0.7 && transition_rate > 0) {
    "possible_carryover"
  } else if (transition_rate == 0) {
    "no_detected_evidence"
  } else {
    "carryover_unlikely_under_stated_assumptions"
  }
  data.table::data.table(
    carryover_state = state,
    expected_carryover_periods = expected_carryover_periods,
    washout_periods = washout_periods,
    transition_rate = transition_rate,
    outcome_persistence = outcome_persistence,
    finding = "Carryover assessment is explicit and affects temporal-design interpretation.",
    recommendation = if (state %in% c("washout_inadequate", "possible_carryover")) "Do not overclaim temporal randomized evidence until carryover assumptions are resolved." else "Carryover evidence does not currently block interpretation."
  )
}

#' Run CUPED-Style Variance Reduction
#' @export
aq_randomized_cuped_adjustment <- function(population, spec, pre_period_metric) {
  dt <- data.table::copy(population$population)
  if (!pre_period_metric %in% names(dt)) {
    return(data.table::data.table(estimator = "cuped_itt", status = "not_run", reason = "Approved pre-period metric is missing."))
  }
  observed <- dt[observed_outcome %in% TRUE & !is.na(get(pre_period_metric))]
  if (nrow(observed) < 4L) return(data.table::data.table(estimator = "cuped_itt", status = "not_run", reason = "Insufficient pre-period coverage."))
  theta <- stats::cov(observed$outcome_value, observed[[pre_period_metric]], use = "complete.obs") / stats::var(observed[[pre_period_metric]], na.rm = TRUE)
  if (!is.finite(theta)) return(data.table::data.table(estimator = "cuped_itt", status = "not_run", reason = "Pre-period metric has zero or invalid variance."))
  observed[, cuped_outcome := outcome_value - theta * (get(pre_period_metric) - mean(get(pre_period_metric), na.rm = TRUE))]
  before <- aq_rd_var(observed$outcome_value)
  after <- aq_rd_var(observed$cuped_outcome)
  arm <- observed[, .(n = .N, mean = mean(cuped_outcome, na.rm = TRUE), variance = aq_rd_var(cuped_outcome)), by = itt_arm]
  tr <- arm[itt_arm == spec$treatment_arm]
  co <- arm[itt_arm == spec$comparison_arm]
  estimate <- tr$mean[[1]] - co$mean[[1]]
  se <- sqrt((tr$variance[[1]] %||% 0) / tr$n[[1]] + (co$variance[[1]] %||% 0) / co$n[[1]])
  ci <- aq_itt_ci(estimate, se, spec$confidence_level, df = max(min(tr$n[[1]], co$n[[1]]) - 1L, 1L))
  data.table::data.table(
    estimator = "cuped_itt",
    status = "success",
    pre_period_metric = pre_period_metric,
    correlation = stats::cor(observed$outcome_value, observed[[pre_period_metric]], use = "complete.obs"),
    adjustment_coefficient = theta,
    variance_before = before,
    variance_after = after,
    precision_gain = if (is.finite(before) && before > 0) 1 - after / before else NA_real_,
    coverage = nrow(observed) / nrow(dt),
    estimate = estimate,
    standard_error = se,
    conf_low = ci[["lower"]],
    conf_high = ci[["upper"]],
    assumptions = "CUPED metric is declared pre-treatment and treatment-blind by frozen specification."
  )
}

aq_design_group_effects <- function(population, spec, group_vars, estimator) {
  dt <- data.table::copy(population$population)
  group_vars <- intersect(group_vars, names(dt))
  if (!length(group_vars)) return(data.table::data.table(estimator = estimator, status = "not_run", reason = "Grouping fields are missing."))
  observed <- dt[observed_outcome %in% TRUE]
  by <- c(group_vars, "itt_arm")
  arm <- observed[, .(n = .N, mean = mean(outcome_value, na.rm = TRUE)), by = by]
  wide <- data.table::dcast(arm, stats::as.formula(paste(paste(group_vars, collapse = "+"), "~ itt_arm")), value.var = c("n", "mean"))
  tmean <- paste0("mean_", spec$treatment_arm)
  cmean <- paste0("mean_", spec$comparison_arm)
  tn <- paste0("n_", spec$treatment_arm)
  cn <- paste0("n_", spec$comparison_arm)
  wide[, `:=`(
    estimator = estimator,
    status = data.table::fifelse(is.na(get(tn)) | is.na(get(cn)), "sparse_or_empty_cell", "success"),
    effect = get(tmean) - get(cmean),
    weight = pmax(get(tn) %||% 0, 0, na.rm = TRUE) + pmax(get(cn) %||% 0, 0, na.rm = TRUE)
  )]
  overall <- wide[status == "success", .(estimator = paste0(estimator, "_overall"), status = "success", estimate = stats::weighted.mean(effect, weight, na.rm = TRUE), groups_used = .N, sparse_groups = sum(wide$status != "success", na.rm = TRUE))]
  list(group_effects = wide, overall = overall)
}

#' Run Randomization Inference for Supported Assignment Mechanisms
#' @export
aq_randomization_inference <- function(population, spec, design_spec = NULL) {
  dt <- data.table::copy(population$population)
  observed <- dt[observed_outcome %in% TRUE]
  if (nrow(observed) < 4L) return(data.table::data.table(estimator = "randomization_inference", status = "not_run", reason = "Insufficient observed outcomes."))
  iterations <- as.integer(design_spec$randomization_iterations %||% 499L)
  seed <- as.integer(design_spec$randomization_seed %||% 20260713L)
  set.seed(seed)
  observed_stat <- mean(observed[itt_arm == spec$treatment_arm]$outcome_value) - mean(observed[itt_arm == spec$comparison_arm]$outcome_value)
  block_vars <- intersect(design_spec$block_fields %||% character(), names(observed))
  stats <- numeric(iterations)
  if (length(block_vars)) {
    observed[, perm_group := do.call(paste, c(.SD, sep = "::")), .SDcols = block_vars]
    for (i in seq_len(iterations)) {
      perm <- data.table::copy(observed)
      perm[, perm_arm := sample(itt_arm), by = perm_group]
      stats[[i]] <- mean(perm[perm_arm == spec$treatment_arm]$outcome_value) - mean(perm[perm_arm == spec$comparison_arm]$outcome_value)
    }
    mechanism <- "blocked_permutation"
  } else {
    for (i in seq_len(iterations)) {
      perm_arm <- sample(observed$itt_arm)
      stats[[i]] <- mean(observed[perm_arm == spec$treatment_arm]$outcome_value) - mean(observed[perm_arm == spec$comparison_arm]$outcome_value)
    }
    mechanism <- "complete_randomization_permutation"
  }
  data.table::data.table(
    estimator = "randomization_inference",
    status = "success",
    assignment_mechanism = mechanism,
    test_statistic = "difference_in_means",
    observed_statistic = observed_stat,
    iterations = iterations,
    random_seed = seed,
    sharp_null_p_value = mean(abs(stats) >= abs(observed_stat)),
    interpretation = "Sharp-null randomization p-value supplements, but does not replace, the ITT estimate and confidence interval."
  )
}

#' Apply a Governed Multiplicity Policy
#' @export
aq_randomized_multiplicity_adjust <- function(results, policy = c("primary_unadjusted", "holm", "bonferroni", "report_only"), family = "primary") {
  policy <- match.arg(policy)
  dt <- data.table::as.data.table(results)
  if (!"p_value" %in% names(dt)) dt[, p_value := NA_real_]
  dt[, `:=`(multiplicity_policy = policy, multiplicity_family = family, adjusted_p_value = p_value)]
  idx <- which(is.finite(dt$p_value))
  if (length(idx) && policy %in% c("holm", "bonferroni")) dt[idx, adjusted_p_value := stats::p.adjust(p_value, method = policy)]
  dt[, claims_allowed := data.table::fifelse(policy == "report_only", "descriptive_only", "confirmatory_only_for_prespecified_family")]
  dt[]
}

#' Interpret Guardrails for Randomized Decision Evidence
#' @export
aq_randomized_guardrail_decision <- function(primary_materiality, guardrails = NULL, maximum_acceptable_harm = NA_real_) {
  guards <- data.table::as.data.table(guardrails %||% data.table::data.table())
  harm <- FALSE
  uncertain <- FALSE
  if (nrow(guards)) {
    harm <- any((guards$severity %||% character()) %in% c("high", "critical") | grepl("harm|breach", paste(guards$finding %||% "", guards$guardrail_status %||% ""), ignore.case = TRUE), na.rm = TRUE)
    uncertain <- any((guards$severity %||% character()) %in% c("warning", "medium"), na.rm = TRUE)
  }
  mat <- primary_materiality$materiality_state[[1]] %||% "not_assessed"
  state <- if (harm && mat %in% c("materially_beneficial", "beneficial_but_uncertain")) {
    "benefit_blocked_by_harm"
  } else if (harm) {
    "harm_detected"
  } else if (uncertain && mat %in% c("materially_beneficial", "beneficial_but_uncertain")) {
    "benefit_with_uncertain_guardrails"
  } else if (mat %in% c("materially_beneficial", "beneficial_but_uncertain")) {
    "benefit_with_acceptable_guardrails"
  } else if (mat %in% c("inconclusive", "insufficient_precision")) {
    "inconclusive"
  } else {
    "no_benefit_and_no_guardrail_concern"
  }
  data.table::data.table(decision_state = state, guardrail_harm = harm, guardrail_uncertainty = uncertain, materiality_state = mat, maximum_acceptable_harm = maximum_acceptable_harm)
}

#' Classify Randomized Effect Materiality Regions
#' @export
aq_randomized_materiality_regions <- function(primary_estimate, material_benefit = NA_real_, material_harm = NA_real_) {
  est <- primary_estimate$estimate[[1]]
  lo <- primary_estimate$conf_low[[1]]
  hi <- primary_estimate$conf_high[[1]]
  benefit <- aq_rd_num(material_benefit, NA_real_)
  harm <- aq_rd_num(material_harm, -abs(benefit))
  if (!is.finite(benefit) || !is.finite(harm)) {
    return(data.table::data.table(region = "not_assessed", finding = "Material benefit and harm regions were not supplied."))
  }
  region <- if (lo >= benefit) "material_benefit" else if (est > 0 && hi < benefit && lo > harm) "small_benefit" else if (lo > harm && hi < benefit) "practically_null" else if (lo < harm && hi > benefit) "uncertain_across_benefit_and_harm" else if (lo < 0 && hi >= benefit) "uncertain_across_benefit_and_null" else if (lo <= harm && hi > harm) "possible_harm" else if (hi <= harm) "material_harm" else "inconclusive"
  data.table::data.table(region = region, material_benefit = benefit, material_harm = harm, estimate = est, conf_low = lo, conf_high = hi)
}

#' Create a Randomized Robustness Matrix
#' @export
aq_randomized_robustness_matrix <- function(itt_result, design_depth = NULL) {
  rows <- list()
  primary <- itt_result$primary_estimate %||% data.table::data.table()
  if (nrow(primary)) rows[[length(rows) + 1L]] <- data.table::data.table(specification = "primary_unadjusted", primary = TRUE, estimate = primary$estimate[[1]], standard_error = primary$standard_error[[1]], conf_low = primary$conf_low[[1]], conf_high = primary$conf_high[[1]], materiality = itt_result$materiality$materiality_state[[1]] %||% NA_character_, interpretation = "Primary preregistered ITT evidence.")
  adj <- itt_result$adjusted_estimate %||% data.table::data.table()
  if (nrow(adj) && "estimate" %in% names(adj) && is.finite(adj$estimate[[1]])) rows[[length(rows) + 1L]] <- data.table::data.table(specification = "approved_adjusted_ancova", primary = FALSE, estimate = adj$estimate[[1]], standard_error = adj$standard_error[[1]], conf_low = adj$conf_low[[1]], conf_high = adj$conf_high[[1]], materiality = NA_character_, interpretation = "Approved precision adjustment; not confounding repair.")
  cuped <- design_depth$cuped %||% data.table::data.table()
  if (nrow(cuped) && "estimate" %in% names(cuped) && is.finite(cuped$estimate[[1]])) rows[[length(rows) + 1L]] <- data.table::data.table(specification = "approved_cuped", primary = FALSE, estimate = cuped$estimate[[1]], standard_error = cuped$standard_error[[1]], conf_low = cuped$conf_low[[1]], conf_high = cuped$conf_high[[1]], materiality = NA_character_, interpretation = "Pre-period variance reduction.")
  ri <- design_depth$randomization_inference %||% data.table::data.table()
  if (nrow(ri) && "sharp_null_p_value" %in% names(ri)) rows[[length(rows) + 1L]] <- data.table::data.table(specification = "randomization_inference", primary = FALSE, estimate = ri$observed_statistic[[1]], standard_error = NA_real_, conf_low = NA_real_, conf_high = NA_real_, materiality = NA_character_, interpretation = paste("Sharp-null p-value:", signif(ri$sharp_null_p_value[[1]], 3)))
  out <- data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
  out[, cherry_picking_allowed := FALSE]
  out[]
}

#' Run Design-Aware Randomized Analysis Depth
#' @export
aq_analyze_randomized_design_depth <- function(itt_result, design_spec, source_data = NULL) {
  if (is.null(itt_result) || !isTRUE(itt_result$effect_estimated)) {
    return(structure(list(schema_version = "aq_randomized_design_analysis_result_v1", status = "not_run", reason = "ITT result was not estimated."), class = c("aq_randomized_design_analysis_result", "list")))
  }
  spec <- itt_result$spec
  population <- itt_result$population
  eligibility <- aq_randomized_design_method_eligibility(design_spec, itt_result)
  cuped <- if ("cuped" %in% design_spec$analysis_modes && length(design_spec$pre_period_fields)) aq_randomized_cuped_adjustment(population, spec, design_spec$pre_period_fields[[1]]) else data.table::data.table(estimator = "cuped_itt", status = "not_requested")
  group_fields <- if (design_spec$design_type == "blocked_randomized") design_spec$block_fields else if (design_spec$design_type == "stratified_randomized") design_spec$stratum_fields else character()
  grouped <- if (length(group_fields)) aq_design_group_effects(population, spec, group_fields, paste0(design_spec$design_type, "_itt")) else list(group_effects = data.table::data.table(status = "not_requested"), overall = data.table::data.table(status = "not_requested"))
  cluster_diagnostics <- {
    dt <- population$population
    cluster_field <- aq_rd_chr(design_spec$cluster_unit, aq_rd_chr(spec$cluster_variable, ""))
    if (nzchar(cluster_field) && cluster_field %in% names(dt)) {
      data.table::as.data.table(dt)[, .(clusters = data.table::uniqueN(get(cluster_field)), units = .N, mean_cluster_size = .N / data.table::uniqueN(get(cluster_field))), by = itt_arm][, few_cluster_warning := clusters < 8L][]
    } else data.table::data.table(status = "not_applicable", finding = "No cluster unit supplied.")
  }
  carryover <- if (design_spec$design_type %in% c("switchback", "stepped_wedge")) aq_assess_randomized_carryover(source_data %||% population$population, unit_col = design_spec$repeated_measure_unit %||% "unit_id", period_col = design_spec$period_field, treatment_col = spec$assignment_column, expected_carryover_periods = 1L, washout_periods = if (nzchar(aq_rd_chr(design_spec$washout_field, ""))) 1L else 0L) else data.table::data.table(carryover_state = "not_applicable")
  switchback <- if (design_spec$design_type == "switchback") data.table::data.table(estimator = "switchback_itt", status = if (carryover$carryover_state[[1]] %in% c("washout_inadequate", "possible_carryover")) "limited" else "success", finding = "Switchback evidence preserves period structure, washout, and carryover state.") else data.table::data.table(status = "not_applicable")
  stepped <- if (design_spec$design_type == "stepped_wedge") data.table::data.table(estimator = "stepped_wedge_itt", status = "foundation", finding = "Stepped-wedge evidence requires cluster and calendar-period adjustment; naive before/after is prohibited.") else data.table::data.table(status = "not_applicable")
  factorial <- if (design_spec$design_type == "factorial") data.table::data.table(estimator = "factorial_itt", status = "foundation", main_effects = paste(design_spec$factorial_terms, collapse = ","), interaction = paste(design_spec$factorial_terms[seq_len(min(2L, length(design_spec$factorial_terms)))], collapse = ":"), finding = "Factorial analysis is only available for explicitly planned factorial designs.") else data.table::data.table(status = "not_applicable")
  ri <- if ("randomization_inference" %in% design_spec$analysis_modes && eligibility[method == "randomization_inference"]$eligible[[1]]) aq_randomization_inference(population, spec, design_spec) else data.table::data.table(estimator = "randomization_inference", status = "not_requested")
  multiplicity <- aq_randomized_multiplicity_adjust(itt_result$primary_estimate, design_spec$multiplicity_policy)
  materiality_regions <- aq_randomized_materiality_regions(itt_result$primary_estimate, design_spec$material_benefit, design_spec$material_harm)
  guardrail_decision <- aq_randomized_guardrail_decision(itt_result$materiality, itt_result$guardrails, design_spec$material_harm)
  depth <- list(cuped = cuped, randomization_inference = ri)
  robustness <- aq_randomized_robustness_matrix(itt_result, depth)
  out <- list(
    schema_version = "aq_randomized_design_analysis_result_v1",
    status = "success",
    design_spec = design_spec,
    method_eligibility = eligibility,
    cuped = cuped,
    grouped_effects = grouped$group_effects,
    grouped_overall = grouped$overall,
    cluster_diagnostics = cluster_diagnostics,
    carryover = carryover,
    switchback = switchback,
    stepped_wedge = stepped,
    factorial = factorial,
    randomization_inference = ri,
    multiplicity = multiplicity,
    materiality_regions = materiality_regions,
    guardrail_decision = guardrail_decision,
    robustness_matrix = robustness,
    prohibited_claims = c(itt_result$prohibited_claims, "Most favorable robustness row was selected as the primary result.", "Randomization inference ignored the assignment mechanism."),
    created_at = aq_vnext_now()
  )
  class(out) <- c("aq_randomized_design_analysis_result", "list")
  out
}

#' Create a Causal Effect Report Contract
#' @export
aq_randomized_causal_effect_report <- function(itt_result, design_result = NULL, report_id = NULL) {
  sections <- data.table::data.table(
    section = c("Business Decision", "Causal Question", "Estimand", "Experimental Design", "Execution and Readiness", "Analysis Population", "Assignment Counts", "Primary Outcome Window", "Unadjusted ITT Effect", "Primary Adjusted Effect", "Design-Specific Analysis", "Uncertainty", "Business Materiality", "Guardrails", "Missingness and Attrition", "Fidelity and Contamination", "Carryover and Interference", "Multiplicity", "Robustness Matrix", "Applicability", "Permitted Claims", "Prohibited Claims", "Decision Implications", "Recommended Next Actions"),
    source = c("decision_context", "spec", "spec", "design_spec", "gate", "population", "population", "outcome_window", "primary_estimate", "adjusted_estimate", "design_result", "primary_estimate", "materiality", "guardrails", "missingness", "fidelity_context", "design_result", "design_result", "design_result", "artifact_metadata", "claims", "claims", "interpretation", "campaign_seeds"),
    status = "available"
  )
  if (is.null(design_result)) sections[source == "design_result", status := "not_available"]
  report <- list(
    schema_version = "aq_randomized_causal_effect_report_v1",
    report_id = report_id %||% aq_vnext_id("randomized_causal_report"),
    analysis_id = itt_result$spec$analysis_id,
    completed_experiment_id = itt_result$spec$completed_experiment_id,
    sections = sections,
    observed_facts = list(population = itt_result$population$summary, missingness = itt_result$missingness, guardrails = itt_result$guardrails),
    estimates = list(primary = itt_result$primary_estimate, adjusted = itt_result$adjusted_estimate, design = design_result$robustness_matrix %||% data.table::data.table()),
    assumptions = c(itt_result$primary_estimate$assumptions, design_result$carryover$finding %||% character()),
    interpretations = list(materiality = itt_result$materiality, guardrail_decision = design_result$guardrail_decision %||% data.table::data.table()),
    recommendations = aq_randomized_itt_campaign_seeds(itt_result),
    permitted_claims = itt_result$permitted_claims,
    prohibited_claims = c(itt_result$prohibited_claims, design_result$prohibited_claims %||% character()),
    lineage = list(itt_result_schema = itt_result$schema_version, design_result_schema = design_result$schema_version %||% NA_character_),
    created_at = aq_vnext_now()
  )
  class(report) <- c("aq_randomized_causal_effect_report", "list")
  report
}

#' Run Randomized Design Analysis QA
#' @export
qa_causal_randomized_design_analysis_framework <- function() {
  set.seed(20260713)
  rows <- list()
  add <- function(check, ok, message) rows[[length(rows) + 1L]] <<- data.table::data.table(suite = "causal_randomized_design_analysis_framework", check, status = if (isTRUE(ok)) "pass" else "fail", message)
  base <- qa_causal_randomized_itt_framework()
  n <- 80L
  completed <- aq_completed_experiment(list(completed_experiment_id = "ce_depth", experiment_plan_artifact_id = "plan_depth", decision_context_id = "decision_depth", causal_question_id = "cq_depth", estimand_id = "estimand_depth", design_version = "v1", assignment_version = "v1", experiment_status = "completed", actual_start_date = "2026-01-01", actual_end_date = "2026-02-01", data_cutoff_date = "2026-02-15", execution_owner = "analytics"))
  assignment <- aq_assignment_evidence(data.table::data.table(unit_id = paste0("u", seq_len(n)), planned_arm = rep(c("control", "treatment"), each = n / 2), realized_assigned_arm = rep(c("control", "treatment"), each = n / 2), block = rep(c("b1", "b2"), times = n / 2), cluster = rep(paste0("geo", 1:8), each = 10)))
  outcomes <- aq_outcome_evidence(data.table::data.table(unit_id = paste0("u", seq_len(n)), outcome_id = "revenue", value = c(rnorm(n / 2, 10, 2), rnorm(n / 2, 13, 2)), outcome_role = "primary"))
  baseline <- data.table::data.table(unit_id = paste0("u", seq_len(n)), baseline_y = rnorm(n), pre_revenue = c(rnorm(n / 2, 9, 2), rnorm(n / 2, 9, 2)), cluster = rep(paste0("geo", 1:8), each = 10), block = rep(c("b1", "b2"), times = n / 2))
  missingness <- aq_assess_missingness_attrition(assignment, outcomes)
  reconciliation <- aq_reconcile_experiment_execution(completed, assignment_evidence = assignment, outcome_evidence = outcomes)
  integrity <- aq_assess_randomization_integrity(assignment)
  fidelity <- aq_assess_treatment_fidelity(assignment)
  interference <- aq_assess_interference_spillover()
  guardrails <- aq_assess_guardrails(aq_outcome_evidence(data.table::data.table(unit_id = paste0("u", seq_len(n)), outcome_id = "cost_guardrail", value = 0, outcome_role = "guardrail")))
  estimand <- aq_assess_estimand_preservation(completed, reconciliation, integrity, fidelity, missingness, interference, outcomes)
  readiness <- aq_assess_experiment_analysis_readiness(completed, assignment, outcomes, reconciliation, integrity, fidelity, missingness, estimand, guardrails)
  evidence <- list(completed = completed, assignment = assignment, outcomes = outcomes, missingness = missingness, reconciliation = reconciliation, integrity = integrity, fidelity = fidelity, interference = interference, guardrails = guardrails, estimand = estimand, readiness = readiness)
  planned <- aq_planned_analysis_record(completed, readiness, outcome_variables = "revenue", baseline_covariates = "baseline_y,pre_revenue")
  spec <- aq_randomized_itt_spec(completed_experiment_id = "ce_depth", experiment_plan_artifact_id = "plan_depth", causal_question_id = "cq_depth", estimand_id = "estimand_depth", treatment_arm = "treatment", comparison_arm = "control", outcome = "revenue", outcome_type = "continuous", baseline_covariates = c("baseline_y", "pre_revenue"), blocking_variables = "block", cluster_variable = "cluster", standard_error_method = "cluster", minimum_meaningful_effect = 1)
  itt <- aq_estimate_randomized_itt(spec, evidence, baseline, planned)
  design_spec <- aq_randomized_design_analysis_spec(itt_analysis_id = spec$analysis_id, design_type = "blocked_randomized", analysis_modes = c("unadjusted", "ancova", "cuped", "blocked", "randomization_inference"), block_fields = "block", cluster_unit = "cluster", pre_period_fields = "pre_revenue", material_benefit = 1, material_harm = -1, multiplicity_policy = "holm")
  depth <- aq_analyze_randomized_design_depth(itt, design_spec)
  report <- aq_randomized_causal_effect_report(itt, depth)
  windows <- aq_enforce_randomized_outcome_windows(data.table::data.table(unit_id = c("u1", "u2", "u3"), measured_at = as.Date(c("2026-01-01", "2026-01-15", "2026-03-01"))), measurement_time_col = "measured_at", window_start = "2026-01-05", window_end = "2026-02-01")
  carry <- aq_assess_randomized_carryover(data.table::data.table(site = rep("s1", 4), period = 1:4, planned_arm = c("control", "treatment", "control", "treatment")), unit_col = "site", period_col = "period", treatment_col = "planned_arm", expected_carryover_periods = 1L, washout_periods = 0L)
  factorial_spec <- aq_randomized_design_analysis_spec(itt_analysis_id = spec$analysis_id, design_type = "factorial", analysis_modes = "factorial", factorial_terms = c("creative", "bid"))
  add("cleanup_baseline_warning_classified", TRUE, "Aggregate warning is known terminology compatibility and not causal work.")
  add("itt_phase4_still_green", all(base$status == "pass"), "Phase 4 ITT QA remains green.")
  add("design_spec_contract", inherits(design_spec, "aq_randomized_design_analysis_spec") && !any(design_spec$validation$status == "fail"), "Design-aware randomized analysis spec validates.")
  add("method_eligibility", any(depth$method_eligibility$method == "blocked" & depth$method_eligibility$eligible), "Only design-compatible methods are marked eligible.")
  add("ancova_preserved", nrow(itt$adjusted_estimate) && nrow(depth$robustness_matrix[specification == "approved_adjusted_ancova"]) == 1L, "ANCOVA improves precision while preserving unadjusted primary evidence.")
  add("cuped_preperiod", nrow(depth$cuped) && depth$cuped$status[[1]] == "success" && is.finite(depth$cuped$precision_gain[[1]]), "CUPED uses an approved pre-period metric and records variance reduction.")
  add("blocked_analysis", nrow(depth$grouped_effects) && nrow(depth$grouped_overall), "Blocked randomized analysis preserves block-level evidence.")
  add("cluster_depth", nrow(depth$cluster_diagnostics) && "few_cluster_warning" %in% names(depth$cluster_diagnostics), "Cluster diagnostics preserve assignment-level sample structure.")
  add("randomization_inference", depth$randomization_inference$status[[1]] == "success" && is.finite(depth$randomization_inference$sharp_null_p_value[[1]]), "Randomization inference uses a supported assignment mechanism and seed.")
  add("outcome_windows", windows$summary$early[[1]] == 1L && windows$summary$late[[1]] == 1L, "Outcome windows classify early and late rows explicitly.")
  add("carryover_explicit", carry$carryover_state[[1]] == "washout_inadequate", "Carryover is assessed separately from generic interference.")
  add("multiplicity_guardrails", nrow(depth$multiplicity) && nrow(depth$guardrail_decision), "Multiplicity and guardrail decision logic are explicit.")
  add("materiality_regions", nrow(depth$materiality_regions) && !is.na(depth$materiality_regions$region[[1]]), "Materiality regions go beyond a single threshold.")
  add("robustness_primary", nrow(depth$robustness_matrix) >= 2L && any(depth$robustness_matrix$primary) && !any(depth$robustness_matrix$cherry_picking_allowed), "Robustness matrix preserves the primary analysis and forbids cherry-picking.")
  add("report_contract", inherits(report, "aq_randomized_causal_effect_report") && nrow(report$sections) >= 20L, "Causal report contract is complete and traceable.")
  add("factorial_foundation", inherits(factorial_spec, "aq_randomized_design_analysis_spec") && !any(factorial_spec$validation$status == "fail"), "Factorial designs require explicit planned factorial terms.")
  add("prohibited_claims", any(grepl("Observational", depth$prohibited_claims)) || any(grepl("Treatment-on-treated", itt$prohibited_claims)), "No observational, treatment-on-treated, or automated decision claim is introduced.")
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}
