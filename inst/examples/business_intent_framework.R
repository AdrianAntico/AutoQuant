library(AutoQuant)

semantics <- aq_variable_semantics(
  variables = c(
    "revenue",
    "paid_search_spend",
    "planned_paid_search_spend",
    "applications",
    "inventory_cap",
    "region"
  ),
  business_role = list(
    revenue = c("objective_metric", "measurement_kpi"),
    paid_search_spend = c("tactic_lever", "business_lever", "cost"),
    planned_paid_search_spend = c("tactic_lever", "business_lever"),
    applications = "measurement_kpi",
    inventory_cap = c("constraint", "guardrail", "risk_indicator"),
    region = "contextual_control"
  ),
  operational_eligibility = list(
    paid_search_spend = c("controllable", "optimization_eligible", "experiment_eligible"),
    planned_paid_search_spend = c("controllable", "scenario_only"),
    inventory_cap = "non_controllable",
    region = "adjustment_only"
  ),
  analytical_role = list(
    revenue = "target",
    paid_search_spend = "predictor",
    planned_paid_search_spend = "known_future_regressor",
    applications = "target",
    inventory_cap = "predictor",
    region = "entity_variable"
  ),
  causal_role = list(
    paid_search_spend = "exposure",
    revenue = "outcome",
    applications = "outcome",
    inventory_cap = "moderator",
    region = "confounder_candidate"
  ),
  decision = list(
    revenue = "primary_kpi",
    paid_search_spend = "optimization_eligible",
    inventory_cap = "guardrail"
  ),
  confidence = list(
    revenue = 0.9,
    paid_search_spend = 0.8,
    planned_paid_search_spend = 0.85,
    applications = 0.8,
    inventory_cap = 0.75,
    region = 0.7
  ),
  business_context_refs = list(
    mission_id = "mission_growth",
    business_objective_id = "objective_revenue_growth",
    strategy_id = "strategy_qualified_demand",
    tactic_id = "tactic_paid_search",
    lever_id = "lever_paid_search_budget",
    risk_id = "risk_capacity",
    decision_id = "decision_budget_shift"
  ),
  dataset_id = "business_intent_example"
)

intent <- aq_business_intent(
  missions = list(
    mission_id = "mission_growth",
    title = "Sustainable Growth",
    description = "Grow while preserving operational quality.",
    owner = "Executive Team"
  ),
  objectives = list(
    objective_id = "objective_revenue_growth",
    mission_id = "mission_growth",
    title = "Increase Revenue",
    owner = "CMO",
    organizational_scope = "function",
    time_horizon = "quarter",
    priority = "high",
    desired_direction = "increase",
    primary_kpis = list("kpi_revenue"),
    guardrails = list("guardrail_capacity"),
    constraints = list("constraint_budget"),
    risk_tolerance = "medium",
    strategic_importance = "high",
    confidence = 0.75
  ),
  strategies = list(
    strategy_id = "strategy_qualified_demand",
    objective_id = "objective_revenue_growth",
    title = "Increase Qualified Demand",
    strategic_thesis = "Increase qualified paid demand to improve revenue.",
    intended_mechanism = "Increase awareness and intent among eligible prospects.",
    tactics = list("tactic_paid_search"),
    assumptions = list("assumption_search_capacity"),
    owner = "Growth Lead",
    organizational_scope = "marketing",
    confidence = 0.7
  ),
  tactics = list(
    tactic_id = "tactic_paid_search",
    strategy_id = "strategy_qualified_demand",
    title = "Expand Paid Search Acquisition",
    operational_description = "Adjust paid-search budget and target ROAS.",
    associated_levers = list("lever_paid_search_budget"),
    expected_mechanism = "More qualified search demand creates more applications.",
    controllability = "partially_controllable",
    reversibility = TRUE,
    experiment_eligible = TRUE,
    optimization_eligible = TRUE,
    authority_requirements = "approval_required"
  ),
  levers = list(
    lever_id = "lever_paid_search_budget",
    name = "Paid Search Budget",
    tactic_id = "tactic_paid_search",
    strategy_id = "strategy_qualified_demand",
    related_variables = list(c("paid_search_spend", "planned_paid_search_spend")),
    controllability = "fully_controllable",
    current_value = 100000,
    permitted_range = "50000-150000",
    validated_range = "80000-120000",
    action_granularity = "weekly",
    adjustment_cadence = "weekly",
    implementation_delay = "1 week",
    effect_delay = "1-3 weeks",
    reversibility = TRUE,
    cost = "media spend",
    evidence_strength = 0.82,
    uncertainty = 0.25,
    economic_importance = 0.9,
    measurement_quality = 0.8,
    risk = 0.35,
    optimization_eligible = TRUE,
    experiment_eligible = TRUE,
    recommendation_eligible = TRUE,
    simulation_eligible = TRUE,
    execution_eligible = FALSE,
    approval_required = TRUE,
    lifecycle_status = "exploitable"
  ),
  kpis = list(
    kpi_id = "kpi_revenue",
    objective_id = "objective_revenue_growth",
    strategy_id = "strategy_qualified_demand",
    tactic_id = "tactic_paid_search",
    source_variable = "revenue",
    definition = "Total revenue",
    unit = "USD",
    aggregation = "sum",
    time_window = "weekly",
    target_value = 1200000,
    baseline_value = 1000000,
    role = "primary",
    confidence = 0.9
  ),
  guardrails = list(
    guardrail_id = "guardrail_capacity",
    tactic_id = "tactic_paid_search",
    source_variable = "inventory_cap",
    definition = "Do not exceed available capacity.",
    measured = TRUE
  ),
  constraints = list(
    constraint_id = "constraint_budget",
    lever_id = "lever_paid_search_budget",
    definition = "Budget cannot exceed approved weekly range.",
    lower_bound = 50000,
    upper_bound = 150000
  ),
  risks = list(
    risk_id = "risk_capacity",
    objective_id = "objective_revenue_growth",
    tactic_id = "tactic_paid_search",
    title = "Capacity Risk",
    severity = "medium",
    evidence_refs = list("guardrail_capacity")
  ),
  assumptions = list(
    assumption_id = "assumption_search_capacity",
    statement = "Search capacity can absorb incremental qualified demand.",
    strategy_id = "strategy_qualified_demand",
    tactic_id = "tactic_paid_search",
    lever_id = "lever_paid_search_budget",
    type = "mechanism",
    evidence_status = "partial",
    confidence = 0.65,
    testability = "testable",
    experiment_eligible = TRUE,
    evidence_strength = 0.45,
    unresolved_uncertainty = 0.65,
    economic_importance = 0.85,
    measurement_quality = 0.75
  ),
  recommendations = list(
    recommendation_id = "recommendation_budget_shift",
    lever_id = "lever_paid_search_budget",
    proposed_action = "Increase within validated range with monitoring.",
    status = "proposed"
  ),
  decisions = list(
    decision_id = "decision_budget_shift",
    recommendation_id = "recommendation_budget_shift",
    decision = "awaiting_approval",
    owner = "CMO"
  ),
  authority = list(
    authority_id = "authority_marketing_advisory",
    organizational_scope = "function",
    decision_domain = "marketing",
    authority_level = "recommend",
    budget_threshold = 25000,
    approval_required = TRUE,
    required_approver = "CMO"
  ),
  coverage = list(
    coverage_id = "coverage_marketing_only",
    represented_domains = list("marketing"),
    missing_domains = list(c("finance", "operations")),
    authority_levels = list(c("observe", "diagnose", "recommend")),
    limitation = "Optimized within represented marketing levers and supplied enterprise constraints; not an enterprise-wide allocation recommendation."
  ),
  variable_semantics = semantics,
  intent_id = "business_intent_example"
)

validation <- aq_validate_business_intent(intent)
alignment <- aq_assess_business_alignment(intent)
explore_exploit <- aq_assess_explore_exploit(intent)
artifact <- aq_business_intent_artifact(intent)

validation
alignment
explore_exploit
artifact$metadata$relationships
artifact$metadata$supported_actions
