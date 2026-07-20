# AutoQuant governed classic Difference-in-Differences contracts.

aq_did_vec <- function(x) {
  if (is.null(x)) return(character())
  if (length(x) == 1L && is.character(x) && grepl(",", x, fixed = TRUE)) x <- strsplit(x, ",", fixed = TRUE)[[1L]]
  aq_vnext_unique_chr(trimws(as.character(x[nzchar(as.character(x))])))
}

aq_did_hash <- function(...) {
  raw <- paste(utils::capture.output(str(list(...), give.attr = FALSE)), collapse = "\n")
  paste0("did_design_", as.integer(abs(sum(utf8ToInt(raw)) %% 1000000000L)))
}

aq_did_time_value <- function(x) {
  if (inherits(x, "Date")) return(as.numeric(x))
  if (inherits(x, "POSIXt")) return(as.numeric(as.Date(x)))
  if (is.character(x)) {
    d <- suppressWarnings(as.Date(x))
    if (all(!is.na(d))) return(as.numeric(d))
  }
  suppressWarnings(as.numeric(x))
}

aq_did_intervention_value <- function(x, reference) {
  if (inherits(reference, "Date") || inherits(reference, "POSIXt")) return(as.numeric(as.Date(x)))
  if (is.character(reference)) {
    d <- suppressWarnings(as.Date(reference))
    if (all(!is.na(d))) return(as.numeric(as.Date(x)))
  }
  suppressWarnings(as.numeric(x)[1L])
}

aq_did_binary <- function(x, treated_level = 1, comparison_level = 0) {
  if (is.logical(x)) x <- as.integer(x)
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) {
    out <- ifelse(x == as.character(treated_level), 1L, ifelse(x == as.character(comparison_level), 0L, NA_integer_))
  } else {
    out <- ifelse(x == treated_level, 1L, ifelse(x == comparison_level, 0L, NA_integer_))
  }
  as.integer(out)
}

aq_did_cluster_se <- function(fit, cluster) {
  if (is.null(cluster) || !length(cluster)) return(summary(fit)$coefficients[, "Std. Error"])
  x <- stats::model.matrix(fit)
  u <- stats::residuals(fit)
  keep <- stats::complete.cases(x, u, cluster)
  x <- x[keep, , drop = FALSE]
  u <- u[keep]
  cluster <- as.character(cluster[keep])
  xtx_inv <- tryCatch(solve(crossprod(x)), error = function(e) NULL)
  if (is.null(xtx_inv)) return(summary(fit)$coefficients[, "Std. Error"])
  meat <- matrix(0, ncol(x), ncol(x))
  for (g in unique(cluster)) {
    idx <- cluster == g
    xu <- crossprod(x[idx, , drop = FALSE], u[idx])
    meat <- meat + xu %*% t(xu)
  }
  g <- length(unique(cluster))
  n <- nrow(x)
  k <- ncol(x)
  scale <- if (g > 1 && n > k) (g / (g - 1)) * ((n - 1) / (n - k)) else 1
  sqrt(diag(scale * xtx_inv %*% meat %*% xtx_inv))
}

#' Create a Governed Difference-in-Differences Analysis Specification
#' @export
aq_did_analysis_spec <- function(
  planning_artifact = NULL,
  plan = NULL,
  readiness = NULL,
  target_trial = NULL,
  treatment_col,
  outcome_col,
  time_col,
  intervention_time,
  unit_col = NULL,
  cluster_col = NULL,
  treatment_level = 1,
  comparison_level = 0,
  estimand = "ATT",
  pre_period = NULL,
  post_period = NULL,
  assumptions = c("parallel_trends", "no_anticipation", "consistency", "stable_composition", "no_spillover", "no_interference", "stable_measurement", "correct_treatment_timing"),
  approved = TRUE,
  spec_id = NULL
) {
  readiness_state <- if (is.data.frame(readiness) && nrow(readiness) && "readiness_state" %in% names(readiness)) readiness$readiness_state[[1]] else "not_supplied"
  spec <- list(
    schema_version = "aq_did_analysis_spec_v1",
    spec_id = spec_id %||% aq_vnext_id("did_analysis_spec"),
    planning_artifact_id = planning_artifact$artifact_id %||% planning_artifact$id %||% NA_character_,
    plan_id = plan$plan_id %||% NA_character_,
    target_trial_id = target_trial$target_trial_id %||% NA_character_,
    readiness_state = readiness_state,
    readiness = readiness,
    treatment_col = as.character(treatment_col)[1L],
    outcome_col = as.character(outcome_col)[1L],
    time_col = as.character(time_col)[1L],
    intervention_time = intervention_time,
    unit_col = unit_col %||% NA_character_,
    cluster_col = cluster_col %||% unit_col %||% NA_character_,
    treatment_level = treatment_level,
    comparison_level = comparison_level,
    estimand = if (estimand %in% c("ATE", "ATT")) estimand else "ATT",
    pre_period = pre_period,
    post_period = post_period,
    assumptions = aq_did_vec(assumptions),
    frozen = TRUE,
    approved = isTRUE(approved),
    frozen_design_hash = aq_did_hash(plan, readiness, target_trial, treatment_col, outcome_col, time_col, intervention_time, unit_col, cluster_col, estimand),
    no_staggered_adoption = TRUE,
    no_event_study = TRUE,
    no_generalized_twfe = TRUE,
    no_design_mutation_after_estimation = TRUE,
    permitted_claims = c(
      "Classic two-group Difference-in-Differences estimate is conditional on the frozen design and explicit assumptions.",
      "Parallel trends are diagnostically assessed, not proven.",
      "Composition and timing diagnostics must accompany the estimate."
    ),
    prohibited_claims = c(
      "Parallel trends were proven.",
      "The estimate supports staggered-adoption, event-study, synthetic-control, or generalized TWFE claims.",
      "The comparison group was optimized after seeing the effect.",
      "The estimate applies outside the frozen population, timing, treatment, comparison, outcome, or estimand."
    ),
    created_at = aq_vnext_now()
  )
  spec$validation <- aq_validate_did_analysis_spec(spec)
  class(spec) <- c("aq_did_analysis_spec", "list")
  spec
}

#' Validate a Difference-in-Differences Analysis Specification
#' @export
aq_validate_did_analysis_spec <- function(spec) {
  rows <- list()
  add <- function(check, ok, message, recommendation = NA_character_) {
    rows[[length(rows) + 1L]] <<- data.table::data.table(check, status = if (isTRUE(ok)) "pass" else "fail", severity = if (isTRUE(ok)) "info" else "error", message, recommendation)
  }
  add("approved_design", isTRUE(spec$approved), "DiD design is explicitly approved.", "Approve or revise the DiD design.")
  add("frozen_design", isTRUE(spec$frozen) && nzchar(spec$frozen_design_hash %||% ""), "Frozen DiD design hash is preserved.", "Freeze the DiD design before estimation.")
  add("phase1_readiness", spec$readiness_state %in% aq_obs_allowed_estimation_states(), paste("Phase 1 readiness:", spec$readiness_state), "Resolve observational readiness blockers first.")
  add("required_columns", all(nzchar(c(spec$treatment_col, spec$outcome_col, spec$time_col))), "Treatment, outcome, and time columns are specified.")
  add("intervention_time", !is.null(spec$intervention_time) && length(spec$intervention_time) == 1L && nzchar(as.character(spec$intervention_time)), "Intervention timing is specified.")
  add("supported_scope", isTRUE(spec$no_staggered_adoption) && isTRUE(spec$no_event_study) && isTRUE(spec$no_generalized_twfe), "Scope is classic two-group DiD only.")
  data.table::rbindlist(rows)
}

aq_did_prepare_data <- function(data, spec) {
  dt <- data.table::as.data.table(data)
  required <- c(spec$treatment_col, spec$outcome_col, spec$time_col)
  missing <- setdiff(required, names(dt))
  if (length(missing)) stop("Required DiD columns are missing: ", paste(missing, collapse = ", "), call. = FALSE)
  dt <- data.table::copy(dt)
  dt[, .did_treated := aq_did_binary(get(spec$treatment_col), spec$treatment_level, spec$comparison_level)]
  if (anyNA(dt$.did_treated) || length(unique(dt$.did_treated)) != 2L) stop("DiD treatment group must be binary.", call. = FALSE)
  dt[, .did_time_value := aq_did_time_value(get(spec$time_col))]
  intervention <- aq_did_intervention_value(spec$intervention_time, dt[[spec$time_col]])
  dt[, .did_post := as.integer(.did_time_value >= intervention)]
  dt[, .did_outcome := as.numeric(get(spec$outcome_col))]
  if (anyNA(dt$.did_time_value) || anyNA(dt$.did_outcome)) stop("DiD time and outcome columns must be usable.", call. = FALSE)
  dt
}

#' Assess Difference-in-Differences Readiness
#' @export
aq_did_readiness <- function(data, spec) {
  gate <- aq_validate_did_analysis_spec(spec)
  if (any(gate$status == "fail")) {
    return(data.table::data.table(readiness_state = "blocked", reasons = paste(gate[status == "fail"]$message, collapse = " | "), supported_next_actions = "revise_design", did_effect_estimated = FALSE))
  }
  dt <- tryCatch(aq_did_prepare_data(data, spec), error = function(e) e)
  if (inherits(dt, "error")) return(data.table::data.table(readiness_state = "blocked", reasons = conditionMessage(dt), supported_next_actions = "revise_data_mapping", did_effect_estimated = FALSE))
  reasons <- character()
  state <- "ready"
  cells <- dt[, .N, by = .(.did_treated, .did_post)]
  if (nrow(cells) < 4L || any(cells$N < 2L)) {
    state <- "blocked"; reasons <- c(reasons, "Treatment and comparison groups require pre and post observations.")
  }
  if (!is.na(spec$unit_col) && nzchar(spec$unit_col) && spec$unit_col %in% names(dt)) {
    unit_span <- dt[, .(has_pre = any(.did_post == 0L), has_post = any(.did_post == 1L), treated_values = data.table::uniqueN(.did_treated)), by = c(spec$unit_col)]
    if (any(unit_span$treated_values > 1L)) {
      state <- "blocked"; reasons <- c(reasons, "Treatment reversal or changing treatment-group membership detected.")
    }
    if (mean(unit_span$has_pre & unit_span$has_post) < 0.7) {
      state <- if (identical(state, "ready")) "ready_with_assumptions" else state
      reasons <- c(reasons, "Some units do not appear in both pre and post periods.")
    }
  }
  if (data.table::uniqueN(dt[.did_post == 0L][[spec$time_col]]) < 2L) {
    state <- "blocked"; reasons <- c(reasons, "At least two pre-period time points are required for trend diagnostics.")
  }
  if (data.table::uniqueN(dt[.did_post == 1L][[spec$time_col]]) < 1L) {
    state <- "blocked"; reasons <- c(reasons, "At least one post-period time point is required.")
  }
  if (!length(reasons)) reasons <- "DiD data support pre-period diagnostics and classic two-group estimation under explicit assumptions."
  data.table::data.table(
    readiness_state = state,
    reasons = paste(unique(reasons), collapse = " | "),
    supported_next_actions = if (state == "blocked") "revise_design_or_data" else "freeze_did_design, run_pre_period_diagnostics, estimate_did",
    did_effect_estimated = FALSE
  )
}

#' Compute Difference-in-Differences Pre-Period Diagnostics
#' @export
aq_did_pre_period_diagnostics <- function(data, spec) {
  dt <- aq_did_prepare_data(data, spec)
  pre <- dt[.did_post == 0L]
  trend <- dt[, .(
    n = .N,
    mean_outcome = mean(.did_outcome, na.rm = TRUE),
    variance = stats::var(.did_outcome, na.rm = TRUE),
    missing_outcome_share = mean(is.na(.did_outcome))
  ), by = .(.did_treated, time = get(spec$time_col), time_value = .did_time_value)]
  slopes <- pre[, {
    fit <- if (.N >= 2L && data.table::uniqueN(.did_time_value) >= 2L) stats::lm(.did_outcome ~ .did_time_value) else NULL
    data.table::data.table(slope = if (is.null(fit)) NA_real_ else stats::coef(fit)[[".did_time_value"]], mean_outcome = mean(.did_outcome, na.rm = TRUE), n = .N)
  }, by = .did_treated]
  slope_diff <- diff(slopes[order(.did_treated)]$slope)
  data.table::setnames(slopes, ".did_treated", "treated_group")
  list(
    trend_data = trend,
    slope_summary = slopes,
    summary = data.table::data.table(pre_period_rows = nrow(pre), pre_time_points = data.table::uniqueN(pre[[spec$time_col]]), slope_difference = if (length(slope_diff)) slope_diff[[1]] else NA_real_, effect_estimated = FALSE)
  )
}

#' Assess Difference-in-Differences Parallel-Trends Support
#' @export
aq_did_parallel_trends <- function(data, spec, diagnostics = aq_did_pre_period_diagnostics(data, spec)) {
  dt <- aq_did_prepare_data(data, spec)
  pre <- dt[.did_post == 0L]
  pre_times <- sort(unique(pre$.did_time_value))
  placebo <- if (length(pre_times) >= 3L) pre_times[ceiling(length(pre_times) / 2)] else NA_real_
  placebo_status <- "not_available"
  placebo_interaction <- NA_real_
  placebo_p <- NA_real_
  if (is.finite(placebo)) {
    pre[, .placebo_post := as.integer(.did_time_value >= placebo)]
    fit <- stats::lm(.did_outcome ~ .did_treated * .placebo_post, data = pre)
    co <- summary(fit)$coefficients
    term <- ".did_treated:.placebo_post"
    if (term %in% rownames(co)) {
      placebo_interaction <- co[term, "Estimate"]
      placebo_p <- co[term, "Pr(>|t|)"]
      placebo_status <- if (is.finite(placebo_p) && placebo_p < 0.1) "placebo_difference_detected" else "no_large_placebo_difference"
    }
  }
  sd_pre <- stats::sd(pre$.did_outcome, na.rm = TRUE)
  slope_diff <- diagnostics$summary$slope_difference[[1]]
  scaled <- abs(slope_diff) / ifelse(is.finite(sd_pre) && sd_pre > 0, sd_pre, 1)
  classification <- if (!is.finite(scaled)) "unknown" else if (scaled <= 0.02 && !identical(placebo_status, "placebo_difference_detected")) "strong_support" else if (scaled <= 0.08) "moderate_support" else if (scaled <= 0.15) "weak_support" else "unsupported"
  data.table::data.table(
    parallel_trends_support = classification,
    slope_difference = slope_diff,
    scaled_slope_difference = scaled,
    placebo_interaction = placebo_interaction,
    placebo_p_value = placebo_p,
    placebo_status = placebo_status,
    claim = "Parallel trends are diagnostically assessed, not proven.",
    recommendation = if (classification %in% c("unsupported", "unknown")) "Do not treat DiD as decision-grade without stronger design evidence." else "Preserve diagnostics and limitations with the estimate."
  )
}

#' Assess Difference-in-Differences Composition Stability
#' @export
aq_did_composition_stability <- function(data, spec) {
  dt <- aq_did_prepare_data(data, spec)
  if (is.na(spec$unit_col) || !nzchar(spec$unit_col) || !spec$unit_col %in% names(dt)) {
    return(data.table::data.table(composition_state = "unknown", entry_share = NA_real_, exit_share = NA_real_, recommendation = "Supply a stable unit column to assess composition."))
  }
  by_unit <- dt[, .(treated = max(.did_treated), has_pre = any(.did_post == 0L), has_post = any(.did_post == 1L), missing_share = mean(is.na(.did_outcome))), by = c(spec$unit_col)]
  entry_share <- mean(!by_unit$has_pre & by_unit$has_post)
  exit_share <- mean(by_unit$has_pre & !by_unit$has_post)
  drift <- max(entry_share, exit_share, mean(by_unit$missing_share > 0.2))
  state <- if (drift <= 0.05) "stable" else if (drift <= 0.2) "marginal" else "unstable"
  data.table::data.table(composition_state = state, entry_share = entry_share, exit_share = exit_share, high_missing_unit_share = mean(by_unit$missing_share > 0.2), recommendation = if (state == "unstable") "Review entry, exit, and eligibility drift before interpreting DiD." else "Preserve composition diagnostics with the estimate.")
}

#' Estimate a Governed Classic Difference-in-Differences Effect
#' @export
aq_estimate_did_effect <- function(data, spec) {
  phase1 <- aq_validate_did_analysis_spec(spec)
  readiness <- aq_did_readiness(data, spec)
  if (any(phase1$status == "fail") || readiness$readiness_state[[1]] == "blocked") {
    return(structure(list(schema_version = "aq_did_effect_result_v1", status = "readiness_blocked", effect_estimated = FALSE, spec = spec, readiness = readiness, gate = phase1, created_at = aq_vnext_now()), class = c("aq_did_effect_result", "list")))
  }
  diagnostics <- aq_did_pre_period_diagnostics(data, spec)
  parallel <- aq_did_parallel_trends(data, spec, diagnostics)
  composition <- aq_did_composition_stability(data, spec)
  if (parallel$parallel_trends_support[[1]] %in% c("unsupported", "unknown") || composition$composition_state[[1]] == "unstable") {
    status <- "diagnostic_blocked"
    effect_estimated <- FALSE
    estimate <- data.table::data.table()
  } else {
    dt <- aq_did_prepare_data(data, spec)
    fit <- stats::lm(.did_outcome ~ .did_treated * .did_post, data = dt)
    co <- stats::coef(fit)
    term <- ".did_treated:.did_post"
    se <- aq_did_cluster_se(fit, if (!is.na(spec$cluster_col) && nzchar(spec$cluster_col) && spec$cluster_col %in% names(dt)) dt[[spec$cluster_col]] else NULL)
    estimate_value <- unname(co[[term]])
    se_value <- unname(se[[term]])
    estimate <- data.table::data.table(
      estimand = spec$estimand,
      estimate = estimate_value,
      std_error = se_value,
      conf_low = estimate_value - 1.96 * se_value,
      conf_high = estimate_value + 1.96 * se_value,
      method = "classic_two_group_did",
      variance = se_value^2,
      cluster_col = spec$cluster_col %||% NA_character_,
      status = "estimated"
    )
    status <- "estimated_requires_review"
    effect_estimated <- TRUE
  }
  sensitivity <- aq_did_sensitivity_summary(spec, diagnostics, parallel, composition, estimate)
  result <- list(
    schema_version = "aq_did_effect_result_v1",
    status = status,
    effect_estimated = effect_estimated,
    spec = spec,
    gate = phase1,
    readiness = readiness,
    pre_period_diagnostics = diagnostics,
    parallel_trends = parallel,
    composition = composition,
    primary_estimate = estimate,
    sensitivity = sensitivity,
    assumptions = data.table::data.table(assumption = spec$assumptions, status = "explicit_assumption", limitation = "Must be defended from design evidence; not proven by estimation."),
    claim_governance = data.table::data.table(permitted_claims = paste(spec$permitted_claims, collapse = " | "), prohibited_claims = paste(spec$prohibited_claims, collapse = " | "), requires_human_review = TRUE, no_generalized_did = TRUE),
    permitted_claims = spec$permitted_claims,
    prohibited_claims = spec$prohibited_claims,
    created_at = aq_vnext_now()
  )
  class(result) <- c("aq_did_effect_result", "list")
  result
}

#' Summarize Difference-in-Differences Sensitivity Obligations
#' @export
aq_did_sensitivity_summary <- function(spec, diagnostics, parallel, composition, estimate = data.table::data.table()) {
  data.table::data.table(
    sensitivity = c("alternative_pre_period", "alternative_comparison", "placebo_intervention_date", "alternative_outcome_window"),
    status = c(
      "recommended",
      "recommended_if_available",
      parallel$placebo_status[[1]],
      "recommended"
    ),
    recommendation = c(
      "Re-run with a defensible pre-period variant before high-stakes use.",
      "Compare against a pre-specified alternative comparison group if one exists.",
      "Use placebo timing as a diagnostic; do not use it to fish for acceptable results.",
      "Confirm the outcome window was specified before inspecting effects."
    )
  )
}

#' Create a Governed Difference-in-Differences Effect Artifact
#' @export
aq_did_effect_artifact <- function(result, artifact_id = NULL) {
  artifact_id <- artifact_id %||% paste0("aq_did_effect_", result$spec$spec_id)
  est <- if (is.data.frame(result$primary_estimate) && nrow(result$primary_estimate)) result$primary_estimate$estimate[[1]] else NA_real_
  artifact <- new_metadata_artifact(
    id = artifact_id,
    title = paste("Difference-in-Differences Effect:", result$spec$outcome_col),
    description = "Governed classic two-group Difference-in-Differences evidence with readiness, pre-period diagnostics, parallel-trends assessment, composition stability, sensitivity, and claim governance.",
    tags = c("causal_intelligence", "observational", "difference_in_differences"),
    values = list(spec = result$spec, readiness = result$readiness, pre_period = result$pre_period_diagnostics, parallel_trends = result$parallel_trends, composition = result$composition, estimate = result$primary_estimate, sensitivity = result$sensitivity, claims = result$claim_governance),
    dependencies = aq_vnext_unique_chr(c(result$spec$planning_artifact_id, result$spec$plan_id, result$spec$target_trial_id)),
    source_generator = "aq_did_effect_artifact",
    version = "aq_did_effect_artifact_v1",
    metadata = list(
      artifact_type = "did_effect_artifact",
      spec_id = result$spec$spec_id,
      plan_id = result$spec$plan_id,
      frozen_design_hash = result$spec$frozen_design_hash,
      estimand = result$spec$estimand,
      outcome = result$spec$outcome_col,
      intervention_time = as.character(result$spec$intervention_time),
      effect_estimated = isTRUE(result$effect_estimated),
      estimate = est,
      parallel_trends_support = result$parallel_trends$parallel_trends_support[[1]],
      composition_state = result$composition$composition_state[[1]],
      requires_human_review = TRUE,
      prohibited_claims = result$prohibited_claims,
      supported_actions = c("review", "approve_evidence", "link_to_decision", "recommend_experiment")
    )
  )
  aq_vnext_attach_envelope(
    artifact,
    artifact_id = artifact_id,
    artifact_type = "did_effect_artifact",
    artifact_version = "aq_did_effect_artifact_v1",
    parent_artifact_ids = aq_vnext_unique_chr(c(result$spec$planning_artifact_id, result$spec$plan_id, result$spec$target_trial_id)),
    lineage = list(spec_id = result$spec$spec_id, plan_id = result$spec$plan_id, frozen_design_hash = result$spec$frozen_design_hash),
    task = "observational_difference_in_differences",
    operator = "classic_two_group_did_after_governed_design",
    engine = "base_r",
    specification_id = result$spec$spec_id,
    supported_actions = c("review", "approve_evidence", "link_to_decision", "recommend_experiment"),
    producer = "aq_did_effect_artifact"
  )
}

#' Create a Difference-in-Differences Review Report Object
#' @export
aq_did_effect_report <- function(result) {
  list(
    schema_version = "aq_did_effect_report_v1",
    title = paste("Difference-in-Differences Review:", result$spec$outcome_col),
    question = result$spec$target_trial_id,
    target_trial = result$spec$target_trial_id,
    treatment = result$spec$treatment_col,
    comparison = result$spec$comparison_level,
    intervention = as.character(result$spec$intervention_time),
    pre_period = result$pre_period_diagnostics$summary,
    parallel_trends = result$parallel_trends,
    composition = result$composition,
    estimate = result$primary_estimate,
    sensitivity = result$sensitivity,
    assumptions = result$assumptions,
    permitted_claims = result$permitted_claims,
    prohibited_claims = result$prohibited_claims,
    limitations = "Classic two-group DiD only; no staggered adoption, event study, generalized TWFE, synthetic DiD, or automatic comparison selection."
  )
}

#' Deterministic QA for Governed Difference-in-Differences
#' @export
qa_observational_did_estimation <- function() {
  checks <- list()
  add <- function(check, ok, message) checks[[length(checks) + 1L]] <<- data.table::data.table(check, status = if (isTRUE(ok)) "success" else "error", message)
  set.seed(123)
  units <- paste0("u", seq_len(80))
  times <- seq_len(8)
  dt <- data.table::CJ(unit = units, time = times)
  dt[, treated_group := as.integer(as.integer(sub("u", "", unit)) <= 40L)]
  dt[, post := as.integer(time >= 5L)]
  unit_effects <- stats::rnorm(80)
  dt[, unit_fe := unit_effects[match(unit, units)]]
  dt[, y := 3 + unit_fe + 0.3 * time + 1.2 * treated_group * post + stats::rnorm(.N, 0, 0.4)]
  readiness <- data.table::data.table(readiness_state = "ready_for_design_implementation", reasons = "qa approved", no_effect_estimated = TRUE)
  spec <- aq_did_analysis_spec(plan = list(plan_id = "obs_plan_did_qa"), readiness = readiness, target_trial = list(target_trial_id = "target_trial_did_qa"), treatment_col = "treated_group", outcome_col = "y", time_col = "time", intervention_time = 5, unit_col = "unit", cluster_col = "unit", estimand = "ATT", approved = TRUE)
  blocked <- aq_did_analysis_spec(plan = list(plan_id = "obs_plan_did_qa"), readiness = data.table::data.table(readiness_state = "blocked"), target_trial = list(target_trial_id = "target_trial_did_qa"), treatment_col = "treated_group", outcome_col = "y", time_col = "time", intervention_time = 5, unit_col = "unit", approved = TRUE)
  read <- aq_did_readiness(dt, spec)
  diag <- aq_did_pre_period_diagnostics(dt, spec)
  par <- aq_did_parallel_trends(dt, spec, diag)
  comp <- aq_did_composition_stability(dt, spec)
  result <- aq_estimate_did_effect(dt, spec)
  blocked_result <- aq_estimate_did_effect(dt, blocked)
  artifact <- aq_did_effect_artifact(result)
  report <- aq_did_effect_report(result)
  add("approved_only_gate", identical(blocked_result$status, "readiness_blocked"), "Blocked Phase 1 readiness prevents DiD estimation.")
  add("intervention_timing_frozen", isTRUE(spec$frozen) && nzchar(spec$frozen_design_hash) && identical(spec$intervention_time, 5), "Intervention timing is frozen in the spec.")
  add("readiness", read$readiness_state[[1]] %in% c("ready", "ready_with_assumptions"), "DiD readiness passes for balanced two-group panel.")
  add("pre_period_diagnostics", nrow(diag$trend_data) > 0L && isFALSE(isTRUE(diag$summary$effect_estimated[[1]])), "Pre-period diagnostics are preserved before estimation.")
  add("parallel_trends_diagnostic", par$parallel_trends_support[[1]] %in% c("strong_support", "moderate_support", "weak_support") && grepl("not proven", par$claim[[1]]), "Parallel trends are diagnostic, not proof.")
  add("composition_stability", comp$composition_state[[1]] == "stable", "Composition stability is assessed.")
  add("deterministic_estimator", isTRUE(result$effect_estimated) && nrow(result$primary_estimate) == 1L, "Classic two-group DiD estimate is produced.")
  add("assumptions_explicit", nrow(result$assumptions) >= 8L, "DiD assumptions remain explicit.")
  add("claim_governance", any(grepl("Parallel trends were proven", result$prohibited_claims)), "Claim governance prohibits overreach.")
  add("no_generalized_did", isTRUE(spec$no_event_study) && isTRUE(spec$no_generalized_twfe), "No event-study or generalized DiD was introduced.")
  add("artifact_contract", identical(artifact$metadata$artifact_type, "did_effect_artifact") && identical(artifact$artifact_envelope$operator, "classic_two_group_did_after_governed_design"), "DiD effect artifact uses canonical envelope.")
  add("report_contract", identical(report$schema_version, "aq_did_effect_report_v1") && nrow(report$parallel_trends) == 1L, "DiD report contract includes diagnostics and claims.")
  data.table::rbindlist(checks)
}
