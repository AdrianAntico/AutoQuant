library(AutoQuant)

context <- aq_decision_valuation_context(
  decision_context_id = "decision_creative_attributes",
  alternatives_included = c("baseline", "pilot", "full"),
  baseline_alternative = "baseline",
  objective_refs = "objective_growth",
  strategy_refs = "strategy_creative_learning",
  tactic_refs = "tactic_paid_social",
  lever_refs = "lever_creative_mix",
  time_horizon_periods = 2,
  discount_rate = 0.01,
  authority = "authority_marketing",
  coverage = "coverage_paid_media"
)

cash_flows <- aq_alternative_cash_flows(list(
  list(
    cash_flow_id = "baseline_value",
    alternative_id = "baseline",
    cash_flow_type = "benefit",
    amount = 10000,
    period = 0,
    scenario = "base",
    source_type = "directly_observed"
  ),
  list(
    cash_flow_id = "pilot_cost",
    alternative_id = "pilot",
    cash_flow_type = "cost",
    amount = 1200,
    period = 0,
    scenario = "base",
    source_type = "imported_financial_input"
  )
))

impact_mapping <- aq_evidence_impact_mapping(list(
  list(
    mapping_id = "creative_itt_translation",
    source_artifact_id = "randomized_itt_effect_creative_a",
    alternative_id = "pilot",
    evidence_type = "randomized_itt",
    estimand_or_prediction = "ATE",
    effect_scale = "incremental conversions per exposure",
    effect_value = 0.04,
    affected_population = 50000,
    duration_periods = 1,
    unit_value = 8,
    source_type = "causally_estimated",
    confidence = 0.8,
    operating_range_min = 0,
    operating_range_max = 100,
    proposed_value = 50
  )
))

economics <- aq_assess_alternative_economics(
  context,
  cash_flows = cash_flows,
  impact_mappings = impact_mapping
)

thresholds <- aq_decision_action_thresholds(list(
  list(
    threshold_id = "minimum_incremental_npv",
    alternative_id = "pilot",
    metric = "incremental_npv",
    operator = ">=",
    value = 0,
    recommendation_if_met = "consider_pilot",
    recommendation_if_not_met = "retain_baseline"
  )
))

sensitivity <- aq_decision_valuation_sensitivity(
  economics,
  list(
    list(
      assumption_id = "unit_value",
      alternative_id = "pilot",
      current_value = 8,
      break_even_value = 5,
      metric = "incremental_npv"
    )
  )
)

information_value <- aq_assess_decision_information_value(
  uncertainties = list(list(magnitude = "medium")),
  sensitivity = sensitivity,
  experiment_cost = 1200,
  reversibility = TRUE
)

recommendation <- aq_governed_decision_valuation_recommendation(
  context,
  economics,
  thresholds = thresholds,
  optionality = aq_decision_optionality(list(
    list(
      optionality_id = "pilot_learning_option",
      alternative_id = "pilot",
      option_type = "learn",
      reversibility = TRUE,
      future_decisions_enabled = "expand,abandon"
    )
  )),
  effort = list(list(alternative_id = "pilot", implementation_hours = 40, capacity_available_hours = 80, burden_level = "medium")),
  risks = list(list(alternative_id = "pilot", risk_id = "guardrail", guardrail_status = "passed")),
  information_value = information_value
)

artifact <- aq_decision_valuation_artifact(context, economics, recommendation)
print(economics)
print(recommendation[, c("alternative_id", "recommendation_category", "recommendation_reason")])
print(artifact$artifact_envelope$artifact_type)

qa <- qa_decision_valuation_framework()
stopifnot(!any(qa$status %in% c("fail", "error")))
