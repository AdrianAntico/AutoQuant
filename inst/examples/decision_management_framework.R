library(AutoQuant)

semantics <- aq_variable_semantics(
  variables = c("paid_search_budget", "revenue"),
  business_role = c(
    paid_search_budget = "tactic_lever",
    revenue = "objective_metric"
  ),
  operational_eligibility = c(
    paid_search_budget = "controllable",
    revenue = "reporting_only"
  ),
  analytical_role = c(
    paid_search_budget = "known_future_regressor",
    revenue = "target"
  ),
  causal_role = c(
    paid_search_budget = "exposure",
    revenue = "outcome"
  ),
  business_context_refs = list(
    lever_id = "lever_paid_search_budget",
    kpi_id = "kpi_revenue"
  )
)

intent <- aq_business_intent(
  missions = list(mission_id = "mission_growth", title = "Grow efficiently"),
  objectives = list(objective_id = "objective_revenue_growth", mission_id = "mission_growth", title = "Increase revenue"),
  strategies = list(strategy_id = "strategy_qualified_demand", objective_id = "objective_revenue_growth", title = "Increase qualified demand"),
  tactics = list(tactic_id = "tactic_paid_search", strategy_id = "strategy_qualified_demand", title = "Scale paid search"),
  levers = list(
    lever_id = "lever_paid_search_budget",
    tactic_id = "tactic_paid_search",
    title = "Paid-search budget",
    related_variables = list("paid_search_budget"),
    controllability = "direct",
    execution_eligible = FALSE,
    approval_required = TRUE,
    validated_range = "100000-120000"
  ),
  kpis = list(
    kpi_id = "kpi_revenue",
    objective_id = "objective_revenue_growth",
    title = "Revenue",
    source_variable = "revenue"
  ),
  authority = list(authority_id = "authority_marketing_advisory", authority_level = "recommend"),
  coverage = list(coverage_id = "coverage_marketing_only", covered_domains = list("marketing"), missing_domains = list("operations")),
  variable_semantics = semantics
)

decision <- aq_decision_context(
  context = list(
    decision_context_id = "decision_paid_search_budget",
    title = "Next-quarter paid-search budget",
    decision_question = "Should paid-search budget remain flat, increase within the validated range, or be piloted above the validated range?",
    related_objectives = list("objective_revenue_growth"),
    related_strategies = list("strategy_qualified_demand"),
    related_tactics = list("tactic_paid_search"),
    related_levers = list("lever_paid_search_budget"),
    authority = "authority_marketing_advisory",
    coverage = "coverage_marketing_only"
  ),
  alternatives = list(
    list(alternative_id = "alt_baseline", name = "Keep current budget", alternative_type = "do_nothing", baseline = TRUE),
    list(alternative_id = "alt_validated_increase", name = "Increase within validated range", alternative_type = "partial_implementation", baseline = FALSE, authority_compatible = TRUE, scope_compatible = TRUE),
    list(alternative_id = "alt_pilot", name = "Pilot above validated range", alternative_type = "pilot", baseline = FALSE, authority_compatible = FALSE, scope_compatible = TRUE)
  ),
  criteria = list(
    list(criterion_id = "criterion_value", name = "Expected net value", direction = "maximize", weight = 0.5),
    list(criterion_id = "criterion_authority", name = "Authority compatibility", hard_constraint = TRUE)
  ),
  financial_impacts = list(
    list(financial_id = "fin_baseline", alternative_id = "alt_baseline", recurring_cost = 100000, expected_benefit = 110000, opportunity_cost = 10000, source_type = "observed"),
    list(financial_id = "fin_validated", alternative_id = "alt_validated_increase", recurring_cost = 115000, expected_benefit = 140000, source_type = "modeled"),
    list(financial_id = "fin_pilot", alternative_id = "alt_pilot", initial_cost = 5000, recurring_cost = 140000, expected_benefit = 160000, downside_estimate = -35000, source_type = "scenario_assumption")
  ),
  uncertainties = list(
    list(uncertainty_id = "unc_pilot_transfer", alternative_id = "alt_pilot", uncertainty_category = "transfer_uncertainty", reducibility = "reducible", decision_sensitivity = "high", candidate_experiment = "bounded pilot")
  ),
  optionality = list(
    list(optionality_id = "opt_pilot_learning", alternative_id = "alt_pilot", option_type = "learn", future_decisions_enabled = list(c("expand", "abandon")), reversibility = TRUE, confidence = 0.6)
  ),
  business_intent = intent,
  variable_semantics = semantics
)

validation <- aq_validate_decision_context(decision)
alternative_assessment <- aq_assess_decision_alternatives(decision)
optionality_assessment <- aq_assess_decision_optionality(decision)
artifact <- aq_decision_context_artifact(decision)

stopifnot(!any(validation$status %in% c("fail", "error")))
stopifnot(nrow(alternative_assessment) == 3)
stopifnot(nrow(optionality_assessment) == 1)
stopifnot(inherits(artifact, "aq_artifact"))
