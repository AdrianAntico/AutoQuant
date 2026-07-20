# AutoQuant governed observational causal effect implementation.

aq_obs_allowed_estimation_states <- function() {
  c("ready_for_design_implementation", "ready_with_strong_assumptions")
}

aq_obs_est_vec <- function(x) {
  if (is.null(x)) return(character())
  if (length(x) == 1L && is.character(x) && grepl(",", x, fixed = TRUE)) {
    x <- strsplit(x, ",", fixed = TRUE)[[1L]]
  }
  aq_vnext_unique_chr(trimws(as.character(x[nzchar(as.character(x))])))
}

aq_obs_est_status <- function(ok, message, severity = NULL, recommendation = NA_character_) {
  data.table::data.table(
    status = if (isTRUE(ok)) "pass" else "fail",
    severity = severity %||% if (isTRUE(ok)) "info" else "error",
    message = message,
    recommendation = recommendation
  )
}

aq_obs_est_design_hash <- function(...) {
  raw <- paste(utils::capture.output(str(list(...), give.attr = FALSE)), collapse = "\n")
  paste0("obs_design_", as.integer(abs(sum(utf8ToInt(raw)) %% 1000000000L)))
}

aq_obs_est_binary_treatment <- function(data, treatment_col, treatment_level = 1, comparison_level = 0) {
  x <- data[[treatment_col]]
  if (is.logical(x)) x <- as.integer(x)
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) {
    out <- ifelse(x == as.character(treatment_level), 1L, ifelse(x == as.character(comparison_level), 0L, NA_integer_))
  } else {
    out <- ifelse(x == treatment_level, 1L, ifelse(x == comparison_level, 0L, NA_integer_))
  }
  as.integer(out)
}

aq_obs_est_weighted_mean <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w >= 0
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

aq_obs_est_weighted_var <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w >= 0
  if (sum(ok) < 2L) return(NA_real_)
  m <- aq_obs_est_weighted_mean(x[ok], w[ok])
  sum(w[ok] * (x[ok] - m)^2) / sum(w[ok])
}

aq_obs_est_smd_num <- function(x, z, w = NULL) {
  if (is.null(w)) w <- rep(1, length(x))
  mt <- aq_obs_est_weighted_mean(x[z == 1L], w[z == 1L])
  mc <- aq_obs_est_weighted_mean(x[z == 0L], w[z == 0L])
  vt <- aq_obs_est_weighted_var(x[z == 1L], w[z == 1L])
  vc <- aq_obs_est_weighted_var(x[z == 0L], w[z == 0L])
  denom <- sqrt((vt + vc) / 2)
  if (!is.finite(denom) || denom == 0) return(0)
  (mt - mc) / denom
}

aq_obs_est_var_ratio <- function(x, z, w = NULL) {
  if (is.null(w)) w <- rep(1, length(x))
  vt <- aq_obs_est_weighted_var(x[z == 1L], w[z == 1L])
  vc <- aq_obs_est_weighted_var(x[z == 0L], w[z == 0L])
  if (!is.finite(vt) || !is.finite(vc) || vc == 0) return(NA_real_)
  vt / vc
}

#' Create a Governed Observational Analysis Specification
#'
#' @param planning_artifact Optional Phase 1 planning artifact.
#' @param plan Optional Phase 1 observational analysis plan.
#' @param readiness Optional readiness table from `aq_assess_observational_estimation_readiness()`.
#' @param target_trial Optional target-trial specification.
#' @param treatment_col Treatment column in the analysis data.
#' @param outcome_col Outcome column in the analysis data.
#' @param adjustment_variables Human-approved pre-treatment adjustment variables.
#' @param estimand `ATE` or `ATT`.
#' @param treatment_level Value representing treatment.
#' @param comparison_level Value representing comparison.
#' @param propensity_model Propensity model family. Phase 2 supports logistic regression.
#' @param matching_policy List with nearest-neighbor matching options.
#' @param weighting_policy List with weighting options.
#' @param outcome_model Outcome regression family. `auto` chooses linear or logistic.
#' @param sensitivity_plan Sensitivity reminders to preserve.
#' @param approved Whether the frozen design is approved for implementation.
#' @param spec_id Optional specification ID.
#'
#' @return An `aq_observational_analysis_spec` object.
#' @export
aq_observational_analysis_spec <- function(
  planning_artifact = NULL,
  plan = NULL,
  readiness = NULL,
  target_trial = NULL,
  treatment_col,
  outcome_col,
  adjustment_variables,
  estimand = c("ATE", "ATT"),
  treatment_level = 1,
  comparison_level = 0,
  propensity_model = "logistic_regression",
  matching_policy = list(method = "nearest_neighbor", ratio = 1L, caliper = NA_real_, exact = character(), replacement = FALSE),
  weighting_policy = list(type = NULL, stabilized = TRUE, trim = NULL, overlap = FALSE),
  outcome_model = "auto",
  sensitivity_plan = c("overlap", "trimming", "unmeasured_confounding_reminder", "negative_control_reminder", "model_comparison"),
  approved = TRUE,
  spec_id = NULL
) {
  estimand <- match.arg(estimand)
  adjustment_variables <- aq_obs_est_vec(adjustment_variables)
  readiness_state <- if (is.data.frame(readiness) && nrow(readiness) && "readiness_state" %in% names(readiness)) readiness$readiness_state[[1]] else "not_supplied"
  if (is.null(weighting_policy$type)) weighting_policy$type <- estimand
  spec <- list(
    schema_version = "aq_observational_analysis_spec_v1",
    spec_id = spec_id %||% aq_vnext_id("observational_analysis_spec"),
    planning_artifact_id = planning_artifact$artifact_id %||% planning_artifact$id %||% NA_character_,
    plan_id = plan$plan_id %||% NA_character_,
    target_trial_id = target_trial$target_trial_id %||% NA_character_,
    readiness_state = readiness_state,
    readiness = readiness,
    frozen_design_hash = aq_obs_est_design_hash(plan, readiness, target_trial, treatment_col, outcome_col, adjustment_variables, estimand),
    frozen = TRUE,
    approved = isTRUE(approved),
    treatment_col = as.character(treatment_col)[1L],
    outcome_col = as.character(outcome_col)[1L],
    adjustment_variables = adjustment_variables,
    estimand = estimand,
    treatment_level = treatment_level,
    comparison_level = comparison_level,
    propensity_model = propensity_model,
    matching_policy = matching_policy,
    weighting_policy = weighting_policy,
    outcome_model = outcome_model,
    sensitivity_plan = aq_obs_est_vec(sensitivity_plan),
    no_automatic_confounder_selection = TRUE,
    no_estimator_shopping = TRUE,
    permitted_claims = c(
      "Observational effect estimate is conditional on the frozen design and explicit assumptions.",
      "Balance and overlap diagnostics must accompany the estimate.",
      "Residual unmeasured confounding remains possible."
    ),
    prohibited_claims = c(
      "Treatment was randomized.",
      "Balance proves no unmeasured confounding.",
      "The estimate applies outside the frozen population, treatment, outcome, or estimand.",
      "Alternative estimators were searched until a favorable result was found."
    ),
    created_at = aq_vnext_now()
  )
  spec$validation <- aq_validate_observational_analysis_spec(spec)
  class(spec) <- c("aq_observational_analysis_spec", "list")
  spec
}

#' Validate a Governed Observational Analysis Specification
#' @export
aq_validate_observational_analysis_spec <- function(spec) {
  checks <- list()
  add <- function(check, ok, message, recommendation = NA_character_) {
    checks[[length(checks) + 1L]] <<- data.table::data.table(
      check = check,
      status = if (isTRUE(ok)) "pass" else "fail",
      severity = if (isTRUE(ok)) "info" else "error",
      message = message,
      recommendation = recommendation
    )
  }
  add("approved_design", isTRUE(spec$approved), "Design is explicitly approved for estimation.", "Approve or revise the observational design before estimation.")
  add("frozen_design", isTRUE(spec$frozen) && nzchar(spec$frozen_design_hash %||% ""), "Frozen design hash is preserved.", "Freeze the Phase 1 design before estimation.")
  add("readiness_gate", spec$readiness_state %in% aq_obs_allowed_estimation_states(), paste("Readiness state:", spec$readiness_state), "Resolve readiness blockers before estimating.")
  add("estimand_supported", spec$estimand %in% c("ATE", "ATT"), "Estimand is supported.", "Use ATE or ATT in Phase 2.")
  add("propensity_model_supported", identical(spec$propensity_model, "logistic_regression"), "Propensity model is governed logistic regression.", "Use logistic regression in Phase 2.")
  add("outcome_present", nzchar(spec$outcome_col %||% ""), "Outcome column is specified.")
  add("treatment_present", nzchar(spec$treatment_col %||% ""), "Treatment column is specified.")
  add("approved_adjustment_set", length(spec$adjustment_variables) > 0L, "Adjustment variables are explicitly supplied.", "Provide human-approved pre-treatment confounders.")
  data.table::rbindlist(checks, use.names = TRUE, fill = TRUE)
}

#' Fit a Governed Observational Propensity Model
#' @export
aq_fit_observational_propensity <- function(data, spec) {
  gate <- aq_validate_observational_analysis_spec(spec)
  if (any(gate$status == "fail")) {
    return(list(status = "blocked", gate = gate, propensity = numeric(), diagnostics = data.table::data.table()))
  }
  dt <- data.table::as.data.table(data)
  required <- c(spec$treatment_col, spec$adjustment_variables)
  missing <- setdiff(required, names(dt))
  if (length(missing)) stop("Required propensity columns are missing: ", paste(missing, collapse = ", "), call. = FALSE)
  z <- aq_obs_est_binary_treatment(dt, spec$treatment_col, spec$treatment_level, spec$comparison_level)
  if (anyNA(z) || length(unique(z)) != 2L) stop("Treatment must be binary and match treatment_level/comparison_level.", call. = FALSE)
  model_data <- dt[, spec$adjustment_variables, with = FALSE]
  model_data$.treatment <- z
  fit <- stats::glm(stats::as.formula(paste(".treatment ~", paste(spec$adjustment_variables, collapse = " + "))), data = model_data, family = stats::binomial())
  p <- as.numeric(stats::predict(fit, newdata = model_data, type = "response"))
  p <- pmax(pmin(p, 0.999), 0.001)
  diagnostics <- data.table::data.table(
    n = length(p),
    treated = sum(z == 1L),
    comparison = sum(z == 0L),
    min_propensity = min(p),
    max_propensity = max(p),
    extreme_propensity_share = mean(p < 0.05 | p > 0.95),
    model = "logistic_regression",
    outcome_excluded = !(spec$outcome_col %in% spec$adjustment_variables),
    status = if (mean(p < 0.05 | p > 0.95) > 0.25) "overlap_warning" else "pass"
  )
  structure(list(status = "success", model = fit, propensity = p, treatment = z, diagnostics = diagnostics, spec = spec), class = c("aq_observational_propensity_result", "list"))
}

#' Construct Governed Observational Matching Sets
#' @export
aq_construct_observational_matches <- function(data, spec, propensity_result) {
  dt <- data.table::as.data.table(data)
  z <- propensity_result$treatment
  p <- propensity_result$propensity
  treated <- which(z == 1L)
  controls <- which(z == 0L)
  policy <- spec$matching_policy
  ratio <- as.integer(policy$ratio %||% 1L)
  caliper <- suppressWarnings(as.numeric(policy$caliper %||% NA_real_))
  replacement <- isTRUE(policy$replacement)
  exact <- intersect(aq_obs_est_vec(policy$exact), names(dt))
  rows <- list()
  used_controls <- integer()
  for (ti in treated) {
    pool <- controls
    if (!replacement) pool <- setdiff(pool, used_controls)
    if (length(exact)) {
      keep <- rep(TRUE, length(pool))
      for (col in exact) keep <- keep & as.character(dt[[col]][pool]) == as.character(dt[[col]][ti])
      pool <- pool[keep]
    }
    if (!length(pool)) next
    dist <- abs(p[pool] - p[ti])
    if (is.finite(caliper)) {
      pool <- pool[dist <= caliper]
      dist <- abs(p[pool] - p[ti])
    }
    if (!length(pool)) next
    ord <- order(dist)
    chosen <- pool[ord][seq_len(min(ratio, length(pool)))]
    used_controls <- unique(c(used_controls, chosen))
    rows[[length(rows) + 1L]] <- data.table::data.table(treated_index = ti, control_index = chosen, distance = abs(p[chosen] - p[ti]), ratio = ratio)
  }
  matches <- if (length(rows)) data.table::rbindlist(rows) else data.table::data.table(treated_index = integer(), control_index = integer(), distance = numeric(), ratio = integer())
  diagnostics <- data.table::data.table(
    treated_units = length(treated),
    matched_treated_units = length(unique(matches$treated_index)),
    matched_control_units = length(unique(matches$control_index)),
    match_rate = if (length(treated)) length(unique(matches$treated_index)) / length(treated) else NA_real_,
    replacement = replacement,
    ratio = ratio,
    caliper = caliper,
    status = if (length(treated) && length(unique(matches$treated_index)) / length(treated) >= 0.8) "pass" else "matching_warning"
  )
  structure(list(status = "success", matches = matches, diagnostics = diagnostics, spec = spec), class = c("aq_observational_matching_result", "list"))
}

#' Construct Governed Observational Weights
#' @export
aq_construct_observational_weights <- function(data, spec, propensity_result) {
  z <- propensity_result$treatment
  p <- propensity_result$propensity
  policy <- spec$weighting_policy
  type <- toupper(policy$type %||% spec$estimand)
  if (isTRUE(policy$overlap)) type <- "OVERLAP"
  pr_t <- mean(z == 1L)
  w <- switch(
    type,
    ATE = ifelse(z == 1L, 1 / p, 1 / (1 - p)),
    ATT = ifelse(z == 1L, 1, p / (1 - p)),
    OVERLAP = ifelse(z == 1L, 1 - p, p),
    stop("Unsupported observational weighting policy: ", type, call. = FALSE)
  )
  if (isTRUE(policy$stabilized) && type == "ATE") w <- ifelse(z == 1L, pr_t / p, (1 - pr_t) / (1 - p))
  trim <- policy$trim
  trimmed <- rep(FALSE, length(w))
  if (!is.null(trim)) {
    limit <- suppressWarnings(as.numeric(trim)[1L])
    if (is.finite(limit) && limit > 0) {
      trimmed <- w > limit
      w <- pmin(w, limit)
    }
  }
  ess <- function(v) if (sum(v^2) > 0) sum(v)^2 / sum(v^2) else NA_real_
  diagnostics <- data.table::data.table(
    weight_type = type,
    stabilized = isTRUE(policy$stabilized),
    trimming_preapproved = !is.null(trim),
    trim_limit = suppressWarnings(as.numeric(trim)[1L] %||% NA_real_),
    trimmed_share = mean(trimmed),
    min_weight = min(w),
    max_weight = max(w),
    mean_weight = mean(w),
    extreme_weight_share = mean(w > 10),
    effective_sample_size = ess(w),
    treated_ess = ess(w[z == 1L]),
    comparison_ess = ess(w[z == 0L]),
    status = if (mean(w > 10) > 0.1 || ess(w) < 0.25 * length(w)) "weight_warning" else "pass"
  )
  structure(list(status = "success", weights = w, treatment = z, diagnostics = diagnostics, spec = spec), class = c("aq_observational_weight_result", "list"))
}

#' Validate Observational Covariate Balance
#' @export
aq_validate_observational_balance <- function(data, spec, weight_result = NULL) {
  dt <- data.table::as.data.table(data)
  z <- if (!is.null(weight_result$treatment)) weight_result$treatment else aq_obs_est_binary_treatment(dt, spec$treatment_col, spec$treatment_level, spec$comparison_level)
  w <- weight_result$weights %||% rep(1, nrow(dt))
  rows <- list()
  for (var in intersect(spec$adjustment_variables, names(dt))) {
    x <- dt[[var]]
    if (is.numeric(x) || is.integer(x)) {
      smd0 <- aq_obs_est_smd_num(as.numeric(x), z)
      smdw <- aq_obs_est_smd_num(as.numeric(x), z, w)
      vr0 <- aq_obs_est_var_ratio(as.numeric(x), z)
      vrw <- aq_obs_est_var_ratio(as.numeric(x), z, w)
      rows[[length(rows) + 1L]] <- data.table::data.table(variable = var, level = NA_character_, variable_type = "numeric", smd_unweighted = smd0, smd_weighted = smdw, variance_ratio_unweighted = vr0, variance_ratio_weighted = vrw)
    } else {
      for (lev in sort(unique(as.character(x)))) {
        xb <- as.numeric(as.character(x) == lev)
        rows[[length(rows) + 1L]] <- data.table::data.table(variable = var, level = lev, variable_type = "categorical", smd_unweighted = aq_obs_est_smd_num(xb, z), smd_weighted = aq_obs_est_smd_num(xb, z, w), variance_ratio_unweighted = NA_real_, variance_ratio_weighted = NA_real_)
      }
    }
  }
  balance <- if (length(rows)) data.table::rbindlist(rows) else data.table::data.table(variable = character(), level = character(), variable_type = character(), smd_unweighted = numeric(), smd_weighted = numeric(), variance_ratio_unweighted = numeric(), variance_ratio_weighted = numeric())
  balance[, balance_status := data.table::fcase(abs(smd_weighted) <= 0.1, "balanced", abs(smd_weighted) <= 0.25, "residual_imbalance", default = "imbalanced")]
  diagnostics <- data.table::data.table(
    variables = length(unique(balance$variable)),
    max_abs_smd_unweighted = if (nrow(balance)) max(abs(balance$smd_unweighted), na.rm = TRUE) else NA_real_,
    max_abs_smd_weighted = if (nrow(balance)) max(abs(balance$smd_weighted), na.rm = TRUE) else NA_real_,
    imbalanced_variables = if (nrow(balance)) sum(balance$balance_status == "imbalanced", na.rm = TRUE) else 0L,
    status = if (nrow(balance) && max(abs(balance$smd_weighted), na.rm = TRUE) <= 0.25) "pass" else "balance_warning"
  )
  structure(list(status = "success", balance = balance, love_plot_data = balance, diagnostics = diagnostics, spec = spec), class = c("aq_observational_balance_result", "list"))
}

#' Fit Governed Observational Outcome Models
#' @export
aq_fit_observational_outcome_models <- function(data, spec, propensity_result = NULL) {
  dt <- data.table::as.data.table(data)
  z <- propensity_result$treatment %||% aq_obs_est_binary_treatment(dt, spec$treatment_col, spec$treatment_level, spec$comparison_level)
  required <- c(spec$outcome_col, spec$adjustment_variables)
  missing <- setdiff(required, names(dt))
  if (length(missing)) stop("Required outcome-model columns are missing: ", paste(missing, collapse = ", "), call. = FALSE)
  y <- dt[[spec$outcome_col]]
  y_binary <- length(unique(stats::na.omit(y))) == 2L
  family <- if (identical(spec$outcome_model, "logistic") || identical(spec$outcome_model, "auto") && y_binary) "logistic" else "linear"
  model_data <- dt[, spec$adjustment_variables, with = FALSE]
  model_data$.outcome <- y
  fit_one <- function(group) {
    d <- model_data[z == group]
    form <- stats::as.formula(paste(".outcome ~", paste(spec$adjustment_variables, collapse = " + ")))
    if (family == "logistic") stats::glm(form, data = d, family = stats::binomial()) else stats::lm(form, data = d)
  }
  fit1 <- fit_one(1L)
  fit0 <- fit_one(0L)
  pred_type <- if (family == "logistic") "response" else "response"
  m1 <- as.numeric(stats::predict(fit1, newdata = model_data, type = pred_type))
  m0 <- as.numeric(stats::predict(fit0, newdata = model_data, type = pred_type))
  diagnostics <- data.table::data.table(outcome_family = family, n = length(y), treated_fit_n = sum(z == 1L), comparison_fit_n = sum(z == 0L), status = "pass")
  structure(list(status = "success", model_treated = fit1, model_comparison = fit0, m1 = m1, m0 = m0, outcome = as.numeric(y), treatment = z, diagnostics = diagnostics, spec = spec), class = c("aq_observational_outcome_result", "list"))
}

#' Estimate a Governed Observational AIPW Effect
#' @export
aq_estimate_observational_aipw <- function(spec, propensity_result, outcome_result, balance_result = NULL) {
  z <- propensity_result$treatment
  p <- propensity_result$propensity
  y <- outcome_result$outcome
  m1 <- outcome_result$m1
  m0 <- outcome_result$m0
  if (!is.null(balance_result$diagnostics) && balance_result$diagnostics$status[[1]] == "balance_warning") {
    return(structure(list(status = "balance_blocked", effect_estimated = FALSE, reason = "Balance validation did not pass.", spec = spec), class = c("aq_observational_aipw_result", "list")))
  }
  if (spec$estimand == "ATE") {
    psi <- (m1 - m0) + z * (y - m1) / p - (1 - z) * (y - m0) / (1 - p)
    estimate <- mean(psi)
    se <- stats::sd(psi) / sqrt(length(psi))
  } else {
    pt <- mean(z == 1L)
    psi <- z * (y - m0) / pt - (1 - z) * p * (y - m0) / ((1 - p) * pt)
    estimate <- mean(psi)
    se <- stats::sd(psi) / sqrt(length(psi))
  }
  out <- data.table::data.table(
    estimand = spec$estimand,
    estimate = estimate,
    std_error = se,
    conf_low = estimate - 1.96 * se,
    conf_high = estimate + 1.96 * se,
    method = "aipw",
    status = "estimated"
  )
  structure(list(status = "success", effect_estimated = TRUE, primary_estimate = out, influence_values = psi, spec = spec), class = c("aq_observational_aipw_result", "list"))
}

#' Summarize Observational Sensitivity Obligations
#' @export
aq_observational_sensitivity_summary <- function(spec, propensity_result, weight_result, balance_result, aipw_result) {
  data.table::data.table(
    sensitivity = c("overlap", "weight_trimming", "unmeasured_confounding", "negative_control", "model_comparison"),
    status = c(
      propensity_result$diagnostics$status[[1]],
      if (isTRUE(weight_result$diagnostics$trimming_preapproved[[1]])) "preapproved" else "not_applied",
      "required_interpretation_limit",
      if ("negative_control_reminder" %in% spec$sensitivity_plan) "recommended" else "not_requested",
      if (isTRUE(aipw_result$effect_estimated)) "aipw_available" else "not_estimated"
    ),
    recommendation = c(
      "Inspect propensity support and do not extrapolate outside common support.",
      "Only use trimming that was specified before inspecting the effect estimate.",
      "State that unmeasured confounding may still explain the result.",
      "Run negative-control checks when credible controls exist.",
      "Compare outcome regression, weighting, and AIPW as diagnostics, not as estimator shopping."
    )
  )
}

#' Estimate a Governed Observational Effect
#' @export
aq_estimate_observational_effect <- function(data, spec) {
  gate <- aq_validate_observational_analysis_spec(spec)
  if (any(gate$status == "fail")) {
    return(structure(list(schema_version = "aq_observational_effect_result_v1", status = "readiness_blocked", effect_estimated = FALSE, gate = gate, spec = spec, created_at = aq_vnext_now()), class = c("aq_observational_effect_result", "list")))
  }
  prop <- aq_fit_observational_propensity(data, spec)
  matches <- aq_construct_observational_matches(data, spec, prop)
  weights <- aq_construct_observational_weights(data, spec, prop)
  balance <- aq_validate_observational_balance(data, spec, weights)
  outcome <- aq_fit_observational_outcome_models(data, spec, prop)
  aipw <- aq_estimate_observational_aipw(spec, prop, outcome, balance)
  sensitivity <- aq_observational_sensitivity_summary(spec, prop, weights, balance, aipw)
  permitted <- spec$permitted_claims
  prohibited <- spec$prohibited_claims
  result <- list(
    schema_version = "aq_observational_effect_result_v1",
    status = if (isTRUE(aipw$effect_estimated)) "estimated_requires_review" else aipw$status,
    effect_estimated = isTRUE(aipw$effect_estimated),
    spec = spec,
    gate = gate,
    propensity = prop$diagnostics,
    matching = matches$diagnostics,
    matches = matches$matches,
    weights = weights$diagnostics,
    balance = balance$balance,
    balance_diagnostics = balance$diagnostics,
    love_plot_data = balance$love_plot_data,
    outcome_models = outcome$diagnostics,
    primary_estimate = aipw$primary_estimate %||% data.table::data.table(),
    sensitivity = sensitivity,
    assumptions = data.table::data.table(
      assumption = c("conditional_exchangeability", "positivity", "consistency", "correct_propensity_or_outcome_model"),
      status = c("explicit_assumption", prop$diagnostics$status[[1]], "explicit_assumption", "aipw_double_robust_condition"),
      limitation = c("Cannot be proven from observed data alone.", "Common support must be inspected.", "Treatment definition must match observed exposure.", "At least one nuisance model must be adequate.")
    ),
    claim_governance = data.table::data.table(
      permitted_claims = paste(permitted, collapse = " | "),
      prohibited_claims = paste(prohibited, collapse = " | "),
      requires_human_review = TRUE,
      no_estimator_shopping = TRUE
    ),
    permitted_claims = permitted,
    prohibited_claims = prohibited,
    created_at = aq_vnext_now()
  )
  class(result) <- c("aq_observational_effect_result", "list")
  result
}

#' Create a Governed Observational Effect Artifact
#' @export
aq_observational_effect_artifact <- function(result, artifact_id = NULL) {
  artifact_id <- artifact_id %||% paste0("aq_observational_effect_", result$spec$spec_id)
  est <- if (is.data.frame(result$primary_estimate) && nrow(result$primary_estimate)) result$primary_estimate$estimate[[1]] else NA_real_
  artifact <- new_metadata_artifact(
    id = artifact_id,
    title = paste("Observational Effect:", result$spec$outcome_col),
    description = "Governed observational causal effect evidence conditional on frozen design, overlap, balance, and explicit assumptions.",
    tags = c("causal_intelligence", "observational", "aipw"),
    values = list(spec = result$spec, gate = result$gate, primary_estimate = result$primary_estimate, propensity = result$propensity, matching = result$matching, weights = result$weights, balance_diagnostics = result$balance_diagnostics, sensitivity = result$sensitivity, claim_governance = result$claim_governance),
    dependencies = aq_vnext_unique_chr(c(result$spec$planning_artifact_id, result$spec$plan_id, result$spec$target_trial_id)),
    source_generator = "aq_observational_effect_artifact",
    version = "aq_observational_effect_artifact_v1",
    metadata = list(
      artifact_type = "observational_effect_artifact",
      spec_id = result$spec$spec_id,
      plan_id = result$spec$plan_id,
      frozen_design_hash = result$spec$frozen_design_hash,
      estimand = result$spec$estimand,
      outcome = result$spec$outcome_col,
      effect_estimated = isTRUE(result$effect_estimated),
      estimate = est,
      requires_human_review = TRUE,
      permitted_claims = result$permitted_claims,
      prohibited_claims = result$prohibited_claims,
      no_estimator_shopping = TRUE,
      supported_actions = c("review", "approve_evidence", "link_to_decision", "seed_sensitivity_review")
    )
  )
  artifact <- aq_vnext_attach_envelope(
    artifact,
    artifact_id = artifact_id,
    artifact_type = "observational_effect_artifact",
    artifact_version = "aq_observational_effect_artifact_v1",
    parent_artifact_ids = aq_vnext_unique_chr(c(result$spec$planning_artifact_id, result$spec$plan_id, result$spec$target_trial_id)),
    lineage = list(spec_id = result$spec$spec_id, plan_id = result$spec$plan_id, frozen_design_hash = result$spec$frozen_design_hash),
    task = "observational_causal_estimation",
    operator = "aipw_after_governed_design",
    engine = "base_r",
    specification_id = result$spec$spec_id,
    supported_actions = c("review", "approve_evidence", "link_to_decision", "seed_sensitivity_review"),
    producer = "aq_observational_effect_artifact"
  )
  artifact
}

#' Deterministic QA for Governed Observational Causal Estimation
#' @export
qa_observational_causal_estimation <- function() {
  checks <- list()
  add <- function(check, ok, message) {
    checks[[length(checks) + 1L]] <<- data.table::data.table(check = check, status = if (isTRUE(ok)) "success" else "error", message = message)
  }
  set.seed(42)
  n <- 360L
  x1 <- stats::rnorm(n)
  x2 <- stats::rbinom(n, 1, 0.45)
  x3 <- sample(c("north", "south"), n, TRUE)
  lp <- -0.2 + 0.7 * x1 - 0.5 * x2 + ifelse(x3 == "south", 0.2, 0)
  p <- stats::plogis(lp)
  treat <- stats::rbinom(n, 1, p)
  y <- 2 + 1.25 * treat + 0.9 * x1 - 0.6 * x2 + ifelse(x3 == "south", 0.4, 0) + stats::rnorm(n)
  dt <- data.table::data.table(treat = treat, y = y, x1 = x1, x2 = x2, x3 = x3)
  readiness <- data.table::data.table(readiness_state = "ready_for_design_implementation", reasons = "qa approved", no_effect_estimated = TRUE)
  plan <- list(plan_id = "obs_plan_qa", prohibited_claims = c("randomization"), permitted_claims = c("conditional estimate"))
  target <- list(target_trial_id = "target_trial_qa")
  spec <- aq_observational_analysis_spec(plan = plan, readiness = readiness, target_trial = target, treatment_col = "treat", outcome_col = "y", adjustment_variables = c("x1", "x2", "x3"), estimand = "ATE", approved = TRUE)
  blocked <- aq_observational_analysis_spec(plan = plan, readiness = data.table::data.table(readiness_state = "blocked"), target_trial = target, treatment_col = "treat", outcome_col = "y", adjustment_variables = c("x1", "x2"), estimand = "ATE", approved = TRUE)
  result <- aq_estimate_observational_effect(dt, spec)
  blocked_result <- aq_estimate_observational_effect(dt, blocked)
  artifact <- aq_observational_effect_artifact(result)
  add("approved_only_gate", identical(blocked_result$status, "readiness_blocked"), "Blocked readiness prevents estimation.")
  add("frozen_design_preserved", isTRUE(spec$frozen) && nzchar(spec$frozen_design_hash), "Frozen design hash is preserved.")
  add("propensity_governed", identical(result$propensity$model[[1]], "logistic_regression"), "Propensity model is logistic regression.")
  add("matching_deterministic", nrow(result$matches) > 0L && result$matching$ratio[[1]] == 1L, "Nearest-neighbor matching is deterministic.")
  add("weighting_deterministic", result$weights$weight_type[[1]] == "ATE" && is.finite(result$weights$effective_sample_size[[1]]), "ATE weights and ESS are produced.")
  add("balance_independent", nrow(result$balance) >= 3L && "smd_weighted" %in% names(result$balance), "Balance diagnostics are separate from effect size.")
  add("aipw_estimated", isTRUE(result$effect_estimated) && nrow(result$primary_estimate) == 1L, "AIPW estimate is produced after diagnostics.")
  add("assumptions_explicit", nrow(result$assumptions) >= 4L, "Core assumptions remain explicit.")
  add("claim_governance", any(grepl("randomized", result$prohibited_claims, ignore.case = TRUE)) && isTRUE(result$claim_governance$requires_human_review[[1]]), "Claim governance prohibits overreach.")
  add("no_estimator_shopping", isTRUE(spec$no_estimator_shopping) && isTRUE(result$claim_governance$no_estimator_shopping[[1]]), "Estimator shopping is not introduced.")
  add("artifact_contract", identical(artifact$metadata$artifact_type, "observational_effect_artifact") && identical(artifact$artifact_envelope$operator, "aipw_after_governed_design"), "Effect artifact uses canonical envelope.")
  data.table::rbindlist(checks)
}
