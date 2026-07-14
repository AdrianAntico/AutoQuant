# AutoQuant deterministic decision-valuation framework.

aq_dv_source_types <- function() {
  c(
    "directly_observed", "experimentally_estimated", "causally_estimated",
    "predictively_modeled", "forecast", "scenario_assumption",
    "expert_judgment", "imported_financial_input", "llm_suggestion",
    "missing", "unsupported"
  )
}

aq_dv_chr <- function(x, default = NA_character_) {
  if (is.null(x) || !length(x) || is.na(x[[1]])) return(default)
  as.character(x[[1]])
}

aq_dv_num <- function(x, default = NA_real_) {
  out <- suppressWarnings(as.numeric(x[[1]] %||% default))
  if (!is.finite(out)) default else out
}

aq_dv_bool <- function(x, default = FALSE) {
  if (is.null(x) || !length(x) || is.na(x[[1]])) return(default)
  isTRUE(x[[1]]) || tolower(as.character(x[[1]])) %in% c("true", "yes", "1")
}

aq_dv_parse_chr <- function(x) {
  if (is.null(x) || !length(x)) return(character())
  x <- paste(as.character(x), collapse = ",")
  x <- trimws(strsplit(x, ",", fixed = TRUE)[[1]])
  x[nzchar(x)]
}

aq_dv_fill_chr <- function(x, default) {
  out <- as.character(x)
  out[is.na(out) | !nzchar(out)] <- default
  out
}

aq_dv_dt <- function(x, id_col = NULL) {
  if (is.null(x)) return(data.table::data.table())
  out <- if (is.data.frame(x)) data.table::as.data.table(x) else data.table::rbindlist(lapply(if (!is.null(names(x)) && any(nzchar(names(x)))) list(x) else x, function(record) data.table::as.data.table(as.list(record))), use.names = TRUE, fill = TRUE)
  if (!is.null(id_col) && !id_col %in% names(out) && nrow(out)) stop("records must include ", id_col, ".", call. = FALSE)
  out
}

aq_dv_add_missing <- function(dt, cols, default = NA) {
  for (col in setdiff(cols, names(dt))) dt[, (col) := default]
  dt
}

aq_dv_validation_row <- function(check, status, reason, severity = status, recommendation = NA_character_) {
  data.table::data.table(check, status, severity, reason, recommendation)
}

#' Create a Decision Valuation Context
#' @export
aq_decision_valuation_context <- function(
  valuation_context_id = NULL,
  decision_context_id,
  decision_version = NA_character_,
  alternatives_included,
  baseline_alternative = "baseline",
  objective_refs = character(),
  strategy_refs = character(),
  tactic_refs = character(),
  lever_refs = character(),
  valuation_date = Sys.Date(),
  time_horizon_periods = 1L,
  period_unit = "period",
  currency = "USD",
  discount_rate = 0,
  authority = NA_character_,
  coverage = NA_character_,
  evidence_cutoff = NA_character_,
  valuation_policy = "deterministic_transparent",
  status = "draft",
  supported_actions = c("validate", "value_alternatives", "assess_thresholds", "register_artifact", "link_to_decision")
) {
  out <- list(
    schema_version = "aq_decision_valuation_context_v1",
    valuation_context_id = valuation_context_id %||% aq_vnext_id("decision_valuation"),
    decision_context_id = decision_context_id,
    decision_version = decision_version,
    alternatives_included = aq_vnext_unique_chr(alternatives_included),
    baseline_alternative = baseline_alternative,
    objective_refs = aq_vnext_unique_chr(objective_refs),
    strategy_refs = aq_vnext_unique_chr(strategy_refs),
    tactic_refs = aq_vnext_unique_chr(tactic_refs),
    lever_refs = aq_vnext_unique_chr(lever_refs),
    valuation_date = as.character(valuation_date),
    time_horizon_periods = as.integer(time_horizon_periods),
    period_unit = period_unit,
    currency = currency,
    discount_rate = aq_dv_num(discount_rate, 0),
    authority = authority,
    coverage = coverage,
    evidence_cutoff = evidence_cutoff,
    valuation_policy = valuation_policy,
    status = status,
    supported_actions = aq_vnext_unique_chr(supported_actions),
    created_at = aq_vnext_now()
  )
  class(out) <- c("aq_decision_valuation_context", "list")
  out$validation <- aq_validate_decision_valuation_context(out)
  out
}

#' Validate a Decision Valuation Context
#' @export
aq_validate_decision_valuation_context <- function(context) {
  rows <- list()
  add <- function(check, status, reason, severity = status, recommendation = NA_character_) {
    rows[[length(rows) + 1L]] <<- aq_dv_validation_row(check, status, reason, severity, recommendation)
  }
  if (!is.list(context) || is.null(context$valuation_context_id)) {
    add("context_object", "fail", "context must be created by aq_decision_valuation_context().", recommendation = "Create a typed valuation context.")
    return(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
  }
  if (!nzchar(aq_dv_chr(context$decision_context_id, ""))) add("decision_context_id", "fail", "decision_context_id is required.", recommendation = "Link valuation to an authored decision context.")
  if (!length(context$alternatives_included)) add("alternatives_included", "fail", "At least one alternative must be included.", recommendation = "Include baseline and candidate alternatives.")
  if (!nzchar(aq_dv_chr(context$baseline_alternative, ""))) add("baseline_alternative", "fail", "A baseline/current-policy alternative is required.", recommendation = "Set baseline_alternative.")
  if (!context$baseline_alternative %in% context$alternatives_included) add("baseline_included", "warning", "The baseline is not listed in alternatives_included.", recommendation = "Include baseline in valuation comparisons.")
  if (is.na(context$time_horizon_periods) || context$time_horizon_periods < 1L) add("time_horizon", "fail", "time_horizon_periods must be positive.", recommendation = "Use a positive deterministic horizon.")
  if (!is.finite(context$discount_rate) || context$discount_rate < -0.99) add("discount_rate", "fail", "discount_rate is invalid.", recommendation = "Use a finite period discount rate.")
  if (!nzchar(aq_dv_chr(context$authority, ""))) add("authority", "warning", "Authority is not linked.", recommendation = "Link authority before recommending execution.")
  if (!nzchar(aq_dv_chr(context$coverage, ""))) add("coverage", "warning", "Coverage is not linked.", recommendation = "Link organizational coverage.")
  if (!length(rows)) add("context_valid", "pass", "Decision valuation context is structurally valid.", "pass")
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

#' Create Alternative Cash-Flow Records
#' @export
aq_alternative_cash_flows <- function(cash_flows) {
  out <- aq_dv_dt(cash_flows, "alternative_id")
  if (!nrow(out)) return(data.table::data.table(alternative_id = character(), cash_flow_id = character()))
  out <- aq_dv_add_missing(out, c("cash_flow_id", "cash_flow_type", "amount", "period", "scenario", "source_type", "evidence_reference", "confidence", "uncertainty_reference", "limitation"))
  out[, cash_flow_id := aq_dv_fill_chr(cash_flow_id, paste0("cash_flow_", seq_len(.N)))]
  out[, cash_flow_type := aq_dv_fill_chr(cash_flow_type, "benefit")]
  out[, amount := suppressWarnings(as.numeric(amount))]
  out[, period := suppressWarnings(as.integer(aq_vnext_default(period, 0L)))]
  out[, scenario := aq_dv_fill_chr(scenario, "base")]
  out[, source_type := ifelse(aq_dv_fill_chr(source_type, "missing") %in% aq_dv_source_types(), aq_dv_fill_chr(source_type, "missing"), "unsupported")]
  out[, confidence := suppressWarnings(as.numeric(confidence))]
  out[, missing_input := is.na(amount) | source_type %in% c("missing", "unsupported")]
  out[]
}

#' Create Evidence-to-Impact Mapping Records
#' @export
aq_evidence_impact_mapping <- function(mappings) {
  out <- aq_dv_dt(mappings, "alternative_id")
  if (!nrow(out)) return(data.table::data.table(alternative_id = character(), mapping_id = character()))
  required <- c(
    "mapping_id", "source_artifact_id", "evidence_type", "estimand_or_prediction",
    "effect_scale", "effect_value", "affected_population", "exposure_level",
    "duration_periods", "unit_value", "capacity_limit", "guardrail_status",
    "operating_range_min", "operating_range_max", "proposed_value",
    "scenario", "source_type", "confidence", "assumptions", "validity_range",
    "applicability_limitations"
  )
  out <- aq_dv_add_missing(out, required)
  out[, mapping_id := aq_dv_fill_chr(mapping_id, paste0("impact_mapping_", seq_len(.N)))]
  out[, evidence_type := aq_dv_fill_chr(evidence_type, "unknown")]
  out[, source_type := ifelse(aq_dv_fill_chr(source_type, "missing") %in% aq_dv_source_types(), aq_dv_fill_chr(source_type, "missing"), "unsupported")]
  for (col in c("effect_value", "affected_population", "exposure_level", "duration_periods", "unit_value", "capacity_limit", "operating_range_min", "operating_range_max", "proposed_value", "confidence")) {
    out[, (col) := suppressWarnings(as.numeric(get(col)))]
  }
  out[, scenario := aq_dv_fill_chr(scenario, "base")]
  out[, guardrail_status := aq_dv_fill_chr(guardrail_status, "not_checked")]
  out[]
}

#' Validate Evidence-to-Impact Mappings
#' @export
aq_validate_evidence_impact_mapping <- function(mapping) {
  mapping <- data.table::as.data.table(mapping)
  rows <- list()
  add <- function(mapping_id, check, status, reason, recommendation = NA_character_) {
    rows[[length(rows) + 1L]] <<- data.table::data.table(mapping_id, check, status, reason, recommendation)
  }
  if (!nrow(mapping)) {
    return(data.table::data.table(mapping_id = character(), check = character(), status = character(), reason = character(), recommendation = character()))
  }
  for (i in seq_len(nrow(mapping))) {
    row <- mapping[i]
    id <- row$mapping_id[[1]]
    if (row$source_type[[1]] %in% c("missing", "unsupported")) add(id, "source_type", "warning", "Evidence source is missing or unsupported.", "Do not treat this mapping as quantified evidence.")
    if (any(is.na(c(row$effect_value, row$affected_population, row$duration_periods, row$unit_value)))) add(id, "translation_inputs", "warning", "One or more translation inputs are missing.", "Keep financial impact missing until effect, scale, duration, and unit economics are known.")
    if (is.finite(row$capacity_limit) && is.finite(row$affected_population) && row$affected_population > row$capacity_limit) add(id, "capacity_limit", "warning", "Affected population exceeds capacity limit.", "Cap expected impact or add capacity evidence.")
    if (is.finite(row$proposed_value) && is.finite(row$operating_range_min) && row$proposed_value < row$operating_range_min) add(id, "operating_range", "warning", "Proposed value is below validated operating range.", "Treat as extrapolation or pilot.")
    if (is.finite(row$proposed_value) && is.finite(row$operating_range_max) && row$proposed_value > row$operating_range_max) add(id, "operating_range", "warning", "Proposed value is above validated operating range.", "Treat as extrapolation or pilot.")
    if (row$guardrail_status[[1]] %in% c("failed", "harmful", "blocked")) add(id, "guardrail", "fail", "Guardrail evidence blocks the mapping.", "Do not recommend exploitation until guardrail is resolved.")
  }
  if (!length(rows)) {
    rows[[1L]] <- data.table::data.table(mapping_id = NA_character_, check = "mapping_valid", status = "pass", reason = "Mappings are structurally usable.", recommendation = NA_character_)
  }
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

aq_translate_evidence_to_cash_flows <- function(mapping) {
  mapping <- data.table::as.data.table(mapping)
  if (!nrow(mapping)) return(aq_alternative_cash_flows(NULL))
  out <- data.table::copy(mapping)
  out[, effective_population := affected_population]
  out[is.finite(capacity_limit) & is.finite(affected_population), effective_population := pmin(affected_population, capacity_limit)]
  out[, amount := effect_value * effective_population * aq_vnext_default(duration_periods, 1) * unit_value]
  out[!is.finite(amount) | source_type %in% c("missing", "unsupported"), amount := NA_real_]
  aq_alternative_cash_flows(out[, .(
    cash_flow_id = paste0("impact_", mapping_id),
    alternative_id,
    cash_flow_type = data.table::fifelse(amount >= 0, "benefit", "harm"),
    amount,
    period = 0L,
    scenario,
    source_type,
    evidence_reference = source_artifact_id,
    confidence,
    uncertainty_reference = estimand_or_prediction,
    limitation = paste(na.omit(c(applicability_limitations, validity_range)), collapse = " | ")
  )])
}

#' Create Explicit Decision Scenarios
#' @export
aq_decision_valuation_scenarios <- function(scenarios) {
  out <- aq_dv_dt(scenarios, "scenario")
  if (!nrow(out)) return(data.table::data.table(scenario = "base", probability = NA_real_, description = "Base scenario", probability_source = "not_assigned"))
  out <- aq_dv_add_missing(out, c("probability", "description", "probability_source"))
  out[, probability := suppressWarnings(as.numeric(probability))]
  out[, probability_source := aq_dv_fill_chr(probability_source, "not_assigned")]
  out[]
}

#' Assess Alternative Economics
#' @export
aq_assess_alternative_economics <- function(context, cash_flows = NULL, impact_mappings = NULL, scenarios = NULL) {
  cash <- aq_alternative_cash_flows(cash_flows)
  impact <- aq_evidence_impact_mapping(impact_mappings)
  translated <- aq_translate_evidence_to_cash_flows(impact)
  all_cash <- data.table::rbindlist(list(cash, translated), use.names = TRUE, fill = TRUE)
  scenarios <- aq_decision_valuation_scenarios(scenarios)
  alternatives <- aq_vnext_unique_chr(c(context$alternatives_included, all_cash$alternative_id))
  if (!length(alternatives)) alternatives <- context$baseline_alternative
  grid <- data.table::CJ(alternative_id = alternatives, scenario = unique(c("base", scenarios$scenario, all_cash$scenario)), sorted = FALSE)
  if (!nrow(all_cash)) {
    grid[, `:=`(gross_benefit = NA_real_, total_cost = NA_real_, net_benefit = NA_real_, roi = NA_real_, payback_period = NA_integer_, npv = NA_real_, missing_inputs = TRUE, evidence_source_status = "missing")]
    return(grid[])
  }
  all_cash[, amount_signed := amount]
  all_cash[cash_flow_type %in% c("cost", "investment", "implementation_cost", "opportunity_cost"), amount_signed := -abs(amount)]
  all_cash[cash_flow_type %in% c("harm", "downside"), amount_signed := -abs(amount)]
  all_cash[cash_flow_type %in% c("benefit", "revenue", "savings", "upside"), amount_signed := abs(amount)]
  all_cash[, discounted_amount := amount_signed / ((1 + context$discount_rate) ^ pmax(period, 0L))]
  by_alt <- all_cash[, .(
    gross_benefit = sum(pmax(amount_signed, 0), na.rm = TRUE),
    total_cost = abs(sum(pmin(amount_signed, 0), na.rm = TRUE)),
    net_benefit = if (all(is.na(amount_signed))) NA_real_ else sum(amount_signed, na.rm = TRUE),
    npv = if (all(is.na(discounted_amount))) NA_real_ else sum(discounted_amount, na.rm = TRUE),
    missing_inputs = any(missing_input) || any(is.na(amount)),
    evidence_source_status = paste(sort(unique(source_type)), collapse = ", ")
  ), by = .(alternative_id, scenario)]
  by_alt[, roi := data.table::fifelse(total_cost > 0, net_benefit / total_cost, NA_real_)]
  cumulative <- all_cash[order(period), .(payback_period = {
    cs <- cumsum(aq_vnext_default(amount_signed, 0))
    hit <- which(cs >= 0 & seq_along(cs) > 1L)
    if (length(hit)) period[hit[1L]] else NA_integer_
  }), by = .(alternative_id, scenario)]
  out <- merge(grid, by_alt, by = c("alternative_id", "scenario"), all.x = TRUE)
  out <- merge(out, cumulative, by = c("alternative_id", "scenario"), all.x = TRUE)
  out[is.na(missing_inputs), missing_inputs := TRUE]
  out[is.na(evidence_source_status), evidence_source_status := "missing"]
  out[, baseline := alternative_id == context$baseline_alternative]
  base <- out[baseline == TRUE, .(scenario, baseline_net_benefit = net_benefit, baseline_npv = npv)]
  out <- merge(out, base, by = "scenario", all.x = TRUE)
  out[, incremental_net_benefit := net_benefit - baseline_net_benefit]
  out[, incremental_npv := npv - baseline_npv]
  out[]
}

#' Run Deterministic Sensitivity Checks
#' @export
aq_decision_valuation_sensitivity <- function(economics, assumptions) {
  economics <- data.table::as.data.table(economics)
  assumptions <- aq_dv_dt(assumptions, "assumption_id")
  if (!nrow(assumptions)) return(data.table::data.table(assumption_id = character(), sensitivity_state = character()))
  assumptions <- aq_dv_add_missing(assumptions, c("alternative_id", "current_value", "low_value", "high_value", "break_even_value", "metric", "notes"))
  for (col in c("current_value", "low_value", "high_value", "break_even_value")) assumptions[, (col) := suppressWarnings(as.numeric(get(col)))]
  current <- economics[, .(alternative_id, scenario, net_benefit, npv, roi, missing_inputs)]
  out <- merge(assumptions, current, by = "alternative_id", all.x = TRUE, allow.cartesian = TRUE)
  out[, distance_to_break_even := break_even_value - current_value]
  out[, sensitivity_state := data.table::fifelse(is.na(break_even_value) | is.na(current_value), "unknown",
    data.table::fifelse(abs(distance_to_break_even) <= abs(current_value) * 0.1, "near_break_even", "not_near_break_even")
  )]
  out[]
}

#' Create Decision Action Thresholds
#' @export
aq_decision_action_thresholds <- function(thresholds) {
  out <- aq_dv_dt(thresholds, "threshold_id")
  if (!nrow(out)) return(data.table::data.table(threshold_id = character()))
  out <- aq_dv_add_missing(out, c("alternative_id", "metric", "operator", "value", "threshold_type", "authority_required", "guardrail_blocking", "recommendation_if_met", "recommendation_if_not_met"))
  out[, value := suppressWarnings(as.numeric(value))]
  out[, threshold_type := aq_dv_fill_chr(threshold_type, "minimum_action_condition")]
  out[, operator := aq_dv_fill_chr(operator, ">=")]
  out[, guardrail_blocking := vapply(guardrail_blocking, aq_dv_bool, logical(1L))]
  out[]
}

aq_dv_compare <- function(x, op, y) {
  if (is.na(x) || is.na(y)) return(NA)
  switch(op, ">=" = x >= y, ">" = x > y, "<=" = x <= y, "<" = x < y, "==" = x == y, "!=" = x != y, NA)
}

#' Assess Decision Thresholds
#' @export
aq_assess_decision_thresholds <- function(economics, thresholds) {
  economics <- data.table::as.data.table(economics)
  thresholds <- aq_decision_action_thresholds(thresholds)
  if (!nrow(thresholds)) {
    return(data.table::data.table(
      threshold_id = character(),
      alternative_id = character(),
      metric = character(),
      metric_value = numeric(),
      operator = character(),
      threshold_value = numeric(),
      threshold_status = character(),
      guardrail_blocking = logical(),
      authority_required = character(),
      recommendation = character()
    ))
  }
  rows <- lapply(seq_len(nrow(thresholds)), function(i) {
    th <- thresholds[i]
    econ <- economics[alternative_id == th$alternative_id]
    if (!nrow(econ)) econ <- economics[1]
    metric_value <- if (nrow(econ) && th$metric %in% names(econ)) econ[[th$metric]][[1]] else NA_real_
    met <- aq_dv_compare(metric_value, th$operator[[1]], th$value[[1]])
    data.table::data.table(
      threshold_id = th$threshold_id,
      alternative_id = th$alternative_id,
      metric = th$metric,
      metric_value = metric_value,
      operator = th$operator,
      threshold_value = th$value,
      threshold_status = if (is.na(met)) "unknown" else if (isTRUE(met)) "met" else "not_met",
      guardrail_blocking = th$guardrail_blocking,
      authority_required = th$authority_required,
      recommendation = if (isTRUE(met)) th$recommendation_if_met else th$recommendation_if_not_met
    )
  })
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

#' Assess Evidence-to-Decision Information Value
#' @export
aq_assess_decision_information_value <- function(uncertainties = NULL, sensitivity = NULL, experiment_cost = NA_real_, reversibility = NA) {
  uncertainty <- aq_dv_dt(uncertainties)
  sensitivity <- aq_dv_dt(sensitivity)
  high_uncertainty <- nrow(uncertainty) && any(as.character(uncertainty$magnitude %||% "") %in% c("high", "unknown"), na.rm = TRUE)
  high_sensitivity <- nrow(sensitivity) && any(sensitivity$sensitivity_state %in% c("near_break_even", "unknown"), na.rm = TRUE)
  cost <- aq_dv_num(experiment_cost)
  reversible <- aq_dv_bool(reversibility, default = NA)
  state <- if (high_uncertainty && high_sensitivity && (is.na(cost) || cost <= 0)) {
    "collect_more_evidence"
  } else if (high_uncertainty && high_sensitivity && isTRUE(reversible)) {
    "pilot_or_stage"
  } else if (high_uncertainty && high_sensitivity) {
    "defer_or_reduce_commitment"
  } else {
    "information_value_low"
  }
  data.table::data.table(
    information_value_state = state,
    high_uncertainty = high_uncertainty,
    high_sensitivity = high_sensitivity,
    experiment_cost = cost,
    reversible = reversible,
    recommendation = switch(state,
      collect_more_evidence = "Learning appears valuable before commitment.",
      pilot_or_stage = "Use a bounded reversible action to learn.",
      defer_or_reduce_commitment = "Avoid irreversible full commitment until uncertainty is reduced.",
      "Existing evidence may be sufficient for deterministic threshold review."
    )
  )
}

#' Assess Decision Optionality for Valuation
#' @export
aq_assess_decision_option_value <- function(optionality = NULL) {
  if (is.null(optionality)) return(data.table::data.table(alternative_id = character(), option_value_state = character()))
  optionality <- data.table::as.data.table(optionality)
  if (!nrow(optionality)) return(data.table::data.table(alternative_id = character(), option_value_state = character()))
  opt <- aq_decision_optionality(optionality)
  opt[, option_value_state := data.table::fifelse(option_type %in% c("learn", "stage", "defer", "expand", "growth", "compound") & vapply(reversibility, aq_dv_bool, logical(1L)), "positive_option_value",
    data.table::fifelse(lengths(lapply(options_foreclosed, aq_dv_parse_chr)) > 0L, "negative_option_value", "unquantified_option_value")
  )]
  opt[, .(option_value_state = paste(sort(unique(option_value_state)), collapse = ", "), options_created = paste(unlist(lapply(future_decisions_enabled, aq_dv_parse_chr)), collapse = ", "), options_foreclosed = paste(unlist(lapply(options_foreclosed, aq_dv_parse_chr)), collapse = ", ")), by = alternative_id]
}

#' Assess Effort and Capacity Requirements
#' @export
aq_assess_decision_effort <- function(effort = NULL) {
  out <- aq_dv_dt(effort, "alternative_id")
  if (!nrow(out)) return(data.table::data.table(alternative_id = character(), effort_state = character()))
  out <- aq_dv_add_missing(out, c("implementation_hours", "capacity_available_hours", "calendar_duration", "burden_level", "owner"))
  out[, implementation_hours := suppressWarnings(as.numeric(implementation_hours))]
  out[, capacity_available_hours := suppressWarnings(as.numeric(capacity_available_hours))]
  out[, capacity_gap := implementation_hours - capacity_available_hours]
  out[, effort_state := data.table::fifelse(is.finite(capacity_gap) & capacity_gap > 0, "capacity_constrained",
    data.table::fifelse(as.character(burden_level) %in% c("high", "severe"), "high_burden", "feasible")
  )]
  out[]
}

#' Assess Downside and Guardrail Risk
#' @export
aq_assess_decision_risk <- function(risks = NULL) {
  out <- aq_dv_dt(risks, "alternative_id")
  if (!nrow(out)) return(data.table::data.table(alternative_id = character(), risk_state = character()))
  out <- aq_dv_add_missing(out, c("risk_id", "risk_type", "downside_amount", "likelihood_label", "guardrail_status", "mitigation"))
  out[, downside_amount := suppressWarnings(as.numeric(downside_amount))]
  out[, risk_state := data.table::fifelse(as.character(guardrail_status) %in% c("failed", "blocked", "harmful"), "blocked_by_guardrail",
    data.table::fifelse(is.finite(downside_amount) & downside_amount < 0, "downside_exposure", "monitored")
  )]
  out[]
}

#' Assess Multiple Decision Criteria
#' @export
aq_multi_criteria_assessment <- function(criteria_values) {
  out <- aq_dv_dt(criteria_values, "alternative_id")
  if (!nrow(out)) return(data.table::data.table(alternative_id = character(), criteria_status = character()))
  out <- aq_dv_add_missing(out, c("criterion_id", "criterion_name", "value", "direction", "weight", "hard_constraint", "constraint_met", "source_type"))
  out[, value := suppressWarnings(as.numeric(value))]
  out[, weight := suppressWarnings(as.numeric(weight))]
  out[, hard_constraint := vapply(hard_constraint, aq_dv_bool, logical(1L))]
  out[, constraint_met := vapply(constraint_met, function(x) aq_dv_bool(x, default = NA), logical(1L))]
  out[, missing_qualitative_evidence := is.na(value) | source_type %in% c("missing", "unsupported")]
  summary <- out[, .(
    criteria = paste(criterion_id, collapse = ", "),
    weighted_value = if (all(is.na(value * weight))) NA_real_ else sum(value * aq_vnext_default(weight, 1), na.rm = TRUE),
    hard_constraint_failure = any(hard_constraint & !constraint_met, na.rm = TRUE),
    missing_qualitative_evidence = any(missing_qualitative_evidence)
  ), by = alternative_id]
  summary[, criteria_status := data.table::fifelse(hard_constraint_failure, "blocked", data.table::fifelse(missing_qualitative_evidence, "partial", "assessed"))]
  summary[]
}

#' Identify Dominated and Non-Dominated Alternatives
#' @export
aq_alternative_dominance <- function(economics, criteria = NULL, risks = NULL) {
  econ <- data.table::as.data.table(economics)
  crit <- aq_multi_criteria_assessment(criteria)
  risk <- aq_assess_decision_risk(risks)
  if (!nrow(crit)) {
    crit <- data.table::data.table(alternative_id = character(), criteria_status = character(), hard_constraint_failure = logical(), missing_qualitative_evidence = logical())
  }
  if (!nrow(risk)) {
    risk <- data.table::data.table(alternative_id = character(), risk_state = character())
  }
  out <- merge(econ, crit[, .(alternative_id, criteria_status, hard_constraint_failure, missing_qualitative_evidence)], by = "alternative_id", all.x = TRUE)
  out <- merge(out, unique(risk[, .(alternative_id, risk_state)]), by = "alternative_id", all.x = TRUE)
  out[, dominated := FALSE]
  for (i in seq_len(nrow(out))) {
    candidate <- out[i]
    comparable <- out[scenario == candidate$scenario & alternative_id != candidate$alternative_id & !is.na(npv) & !missing_inputs]
    out$dominated[i] <- nrow(comparable[npv >= candidate$npv & net_benefit >= candidate$net_benefit]) > 0L && (isTRUE(candidate$missing_inputs) || any(comparable$npv > candidate$npv))
  }
  out[, dominance_state := data.table::fifelse(risk_state == "blocked_by_guardrail" | hard_constraint_failure, "blocked",
    data.table::fifelse(dominated, "dominated", data.table::fifelse(missing_qualitative_evidence | missing_inputs, "incompletely_ordered", "non_dominated"))
  )]
  out[]
}

#' Create a Governed Decision-Valuation Recommendation
#' @export
aq_governed_decision_valuation_recommendation <- function(context, economics, thresholds = NULL, optionality = NULL, effort = NULL, risks = NULL, criteria = NULL, information_value = NULL) {
  dominance <- aq_alternative_dominance(economics, criteria, risks)
  threshold_status <- aq_assess_decision_thresholds(economics, thresholds)
  option_value <- aq_assess_decision_option_value(optionality)
  effort_state <- aq_assess_decision_effort(effort)
  info <- information_value %||% aq_assess_decision_information_value()
  out <- merge(dominance, option_value, by = "alternative_id", all.x = TRUE)
  out <- merge(out, unique(effort_state[, .(alternative_id, effort_state)]), by = "alternative_id", all.x = TRUE)
  blocked_thresholds <- threshold_status[guardrail_blocking == TRUE | threshold_status == "not_met", unique(alternative_id)]
  out[, recommendation_category := "insufficient_information"]
  out[baseline == TRUE & (is.na(incremental_npv) | incremental_npv >= 0), recommendation_category := "retain_baseline"]
  out[dominance_state == "non_dominated" & !alternative_id %in% blocked_thresholds & !isTRUE(missing_inputs) & incremental_npv > 0, recommendation_category := "proceed"]
  out[dominance_state %in% c("incompletely_ordered") | missing_inputs == TRUE, recommendation_category := "collect_more_evidence"]
  out[option_value_state %in% c("positive_option_value") & recommendation_category %in% c("collect_more_evidence", "insufficient_information"), recommendation_category := "pilot_or_stage"]
  out[effort_state %in% c("capacity_constrained", "high_burden") & recommendation_category == "proceed", recommendation_category := "stage_or_escalate_capacity"]
  out[dominance_state == "blocked" | alternative_id %in% blocked_thresholds, recommendation_category := "reject_or_escalate"]
  if (is.null(context$authority) || !nzchar(aq_dv_chr(context$authority, ""))) out[recommendation_category %in% c("proceed", "pilot_or_stage", "stage_or_escalate_capacity"), recommendation_category := "escalate_authority"]
  out[, information_value_state := info$information_value_state[[1]]]
  out[, recommendation_reason := paste(
    "Economics:", aq_vnext_default(dominance_state, "unknown"),
    "| sources:", aq_vnext_default(evidence_source_status, "missing"),
    "| information:", info$information_value_state[[1]]
  )]
  out[order(match(recommendation_category, c("proceed", "pilot_or_stage", "stage_or_escalate_capacity", "retain_baseline", "collect_more_evidence", "escalate_authority", "reject_or_escalate", "insufficient_information")))]
}

#' Create a Canonical Decision Valuation Artifact
#' @export
aq_decision_valuation_artifact <- function(context, economics, recommendation, validations = NULL, artifact_id = NULL) {
  artifact_id <- artifact_id %||% paste0("decision_valuation_artifact_", context$valuation_context_id)
  payload <- list(
    context = context,
    economics = data.table::as.data.table(economics),
    recommendation = data.table::as.data.table(recommendation),
    validations = validations %||% list()
  )
  artifact <- new_metadata_artifact(
    id = artifact_id,
    title = "Decision Valuation",
    values = payload,
    description = "Deterministic alternative economics, evidence translation, thresholds, and governed recommendation.",
    tags = c("decision_valuation", "semantic_intelligence", "evidence_to_decision"),
    source_generator = "aq_decision_valuation_artifact",
    metadata = list(
      artifact_id = artifact_id,
      artifact_type = "decision_valuation_artifact",
      decision_context_id = context$decision_context_id,
      valuation_context_id = context$valuation_context_id,
      baseline_alternative = context$baseline_alternative,
      currency = context$currency,
      created_at = aq_vnext_now()
    )
  )
  artifact <- aq_vnext_attach_envelope(
    artifact,
    artifact_id = artifact_id,
    artifact_type = "decision_valuation_artifact",
    artifact_version = "aq_decision_valuation_artifact_v1",
    parent_artifact_ids = aq_vnext_unique_chr(c(context$decision_context_id)),
    lineage = list(decision_context_id = context$decision_context_id, valuation_context_id = context$valuation_context_id),
    task = "decision_valuation",
    operator = "deterministic_decision_valuation",
    engine = "base_r",
    specification_id = context$valuation_context_id,
    supported_actions = c("review", "link_to_decision", "register_artifact", "calibrate_outcomes"),
    producer = "aq_decision_valuation_artifact"
  )
  artifact
}

#' Create Decision Valuation Campaign Seeds
#' @export
aq_decision_valuation_campaign_seeds <- function(recommendation) {
  rec <- data.table::as.data.table(recommendation)
  if (!nrow(rec)) return(data.table::data.table(seed_type = character(), alternative_id = character(), reason = character()))
  rec[recommendation_category %in% c("collect_more_evidence", "pilot_or_stage", "stage_or_escalate_capacity", "escalate_authority", "reject_or_escalate"),
    .(seed_type = recommendation_category, alternative_id, reason = recommendation_reason)
  ]
}

#' Record Valuation Calibration Against Realized Outcomes
#' @export
aq_decision_valuation_calibration_record <- function(valuation_artifact_id, alternative_id, expected_value, realized_value, outcome_source = "outcome_review", notes = NA_character_) {
  expected_value <- aq_dv_num(expected_value)
  realized_value <- aq_dv_num(realized_value)
  data.table::data.table(
    calibration_id = aq_vnext_id("decision_valuation_calibration"),
    valuation_artifact_id,
    alternative_id,
    expected_value,
    realized_value,
    variance = realized_value - expected_value,
    outcome_source,
    notes,
    created_at = aq_vnext_now()
  )
}

#' QA for Decision Valuation Intelligence
#' @export
qa_decision_valuation_framework <- function() {
  rows <- list()
  add <- function(check, ok, message) rows[[length(rows) + 1L]] <<- data.table::data.table(suite = "decision_valuation_framework", check, status = if (isTRUE(ok)) "pass" else "fail", message)
  ctx <- aq_decision_valuation_context(
    decision_context_id = "decision_creative_test",
    alternatives_included = c("baseline", "pilot", "full"),
    baseline_alternative = "baseline",
    objective_refs = "objective_growth",
    strategy_refs = "strategy_creative_learning",
    tactic_refs = "tactic_paid_media",
    lever_refs = "lever_creative_mix",
    time_horizon_periods = 3L,
    discount_rate = 0.01,
    authority = "authority_marketing",
    coverage = "coverage_paid_media"
  )
  add("context_validates", any(ctx$validation$status == "pass"), "Valuation context validates.")
  cash <- aq_alternative_cash_flows(list(
    list(cash_flow_id = "base", alternative_id = "baseline", cash_flow_type = "benefit", amount = 1000, period = 0, scenario = "base", source_type = "directly_observed"),
    list(cash_flow_id = "pilot_cost", alternative_id = "pilot", cash_flow_type = "cost", amount = 200, period = 0, scenario = "base", source_type = "imported_financial_input"),
    list(cash_flow_id = "full_missing", alternative_id = "full", cash_flow_type = "benefit", amount = NA, period = 0, scenario = "base", source_type = "missing")
  ))
  add("cash_flows_preserve_missing", any(cash$alternative_id == "full" & cash$missing_input), "Cash flows preserve missing source inputs.")
  mapping <- aq_evidence_impact_mapping(list(
    list(mapping_id = "m1", source_artifact_id = "itt_1", alternative_id = "pilot", evidence_type = "randomized_itt", estimand_or_prediction = "ATE", effect_scale = "incremental_conversions", effect_value = 0.05, affected_population = 10000, duration_periods = 1, unit_value = 10, source_type = "causally_estimated", confidence = 0.8, operating_range_min = 0, operating_range_max = 100, proposed_value = 50),
    list(mapping_id = "m2", source_artifact_id = "forecast_1", alternative_id = "full", evidence_type = "forecast", effect_scale = "incremental_revenue", effect_value = NA, affected_population = 10000, duration_periods = 2, unit_value = 10, source_type = "forecast")
  ))
  val <- aq_validate_evidence_impact_mapping(mapping)
  add("impact_mapping_warns_missing", any(val$mapping_id == "m2" & val$status == "warning"), "Impact validation warns instead of inventing missing values.")
  econ <- aq_assess_alternative_economics(ctx, cash, mapping)
  add("economics_computed", all(c("net_benefit", "npv", "roi", "incremental_npv") %in% names(econ)) && nrow(econ) >= 3L, "Alternative economics are transparent.")
  add("causal_translation_keeps_source", any(grepl("causally_estimated", econ$evidence_source_status, fixed = TRUE)), "Causal evidence source status is retained.")
  scen <- aq_decision_valuation_scenarios(list(list(scenario = "downside", probability = NA, description = "No probability assigned")))
  add("scenarios_do_not_require_probabilities", is.na(scen$probability[[1]]), "Scenario contract allows explicit scenarios without invented probabilities.")
  sens <- aq_decision_valuation_sensitivity(econ, list(list(assumption_id = "unit_value", alternative_id = "pilot", current_value = 10, break_even_value = 9, metric = "npv")))
  add("sensitivity_break_even", any(sens$sensitivity_state == "near_break_even"), "Sensitivity identifies break-even assumptions.")
  thresholds <- aq_decision_action_thresholds(list(list(threshold_id = "min_npv", alternative_id = "pilot", metric = "incremental_npv", operator = ">=", value = 0, recommendation_if_met = "consider_pilot", recommendation_if_not_met = "retain_baseline")))
  th <- aq_assess_decision_thresholds(econ, thresholds)
  add("thresholds_assessed", any(th$threshold_status %in% c("met", "not_met", "unknown")), "Action thresholds connect metrics to decision conditions.")
  info <- aq_assess_decision_information_value(list(list(magnitude = "high")), sens, experiment_cost = 100, reversibility = TRUE)
  add("information_value", info$information_value_state[[1]] %in% c("pilot_or_stage", "collect_more_evidence"), "Information value influences explore/exploit guidance.")
  opt <- aq_decision_optionality(list(list(optionality_id = "opt_pilot", alternative_id = "pilot", option_type = "learn", reversibility = TRUE, future_decisions_enabled = "expand,abandon", options_foreclosed = "")))
  opt_assess <- aq_assess_decision_option_value(opt)
  add("optionality_assessed", any(opt_assess$option_value_state == "positive_option_value"), "Optionality creates/forecloses future choices.")
  effort <- aq_assess_decision_effort(list(list(alternative_id = "pilot", implementation_hours = 40, capacity_available_hours = 20, burden_level = "medium")))
  add("effort_separate_from_cost", any(effort$effort_state == "capacity_constrained"), "Effort and capacity are assessed separately from cost.")
  risk <- aq_assess_decision_risk(list(list(alternative_id = "full", risk_id = "guardrail", guardrail_status = "blocked", downside_amount = -1000)))
  add("guardrail_blocks", any(risk$risk_state == "blocked_by_guardrail"), "Guardrail risk can block attractive economics.")
  criteria <- aq_multi_criteria_assessment(list(list(alternative_id = "pilot", criterion_id = "strategic_fit", value = NA, source_type = "missing")))
  add("criteria_preserve_partial_order", any(criteria$criteria_status == "partial"), "Incomplete qualitative evidence remains partial.")
  rec <- aq_governed_decision_valuation_recommendation(ctx, econ, thresholds, opt, effort, risk, criteria, info)
  add("recommendation_governed", all(c("recommendation_category", "recommendation_reason") %in% names(rec)), "Governed recommendation is structured and explainable.")
  add("baseline_can_be_recommended", any(rec$alternative_id == "baseline"), "Baseline remains an explicit alternative.")
  art <- aq_decision_valuation_artifact(ctx, econ, rec, validations = list(mapping = val, thresholds = th))
  add("artifact_canonical", identical(art$artifact_envelope$artifact_type, "decision_valuation_artifact"), "Decision valuation registers as canonical artifact.")
  seeds <- aq_decision_valuation_campaign_seeds(rec)
  add("campaign_seeds", data.table::is.data.table(seeds), "Valuation can seed governed follow-up campaigns.")
  cal <- aq_decision_valuation_calibration_record("artifact_1", "pilot", 100, 80)
  add("calibration_records_variance", is.finite(cal$variance[[1]]), "Realized outcomes can calibrate future assumptions.")
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}
