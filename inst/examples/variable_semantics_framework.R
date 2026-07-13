library(AutoQuant)

semantics <- aq_variable_semantics(
  variables = c("revenue", "spend", "region", "promo", "inventory_cap"),
  operational = list(
    revenue = "outcome",
    spend = c("driver", "optimization_candidate"),
    region = "control",
    promo = "scenario_variable",
    inventory_cap = "constraint"
  ),
  temporal = list(
    revenue = "future_unknown",
    spend = "time_varying",
    promo = "future_known"
  ),
  causal = list(
    spend = "exposure",
    region = "possible_confounder",
    revenue = "outcome"
  ),
  forecasting = list(
    revenue = "target",
    spend = "shared_predictor",
    promo = "known_future_regressor",
    region = "entity_variable"
  ),
  measurement = list(
    revenue = "observed",
    spend = "observed",
    promo = "external",
    inventory_cap = "scenario"
  ),
  decision = list(
    revenue = "primary_kpi",
    spend = "optimization_eligible",
    inventory_cap = "business_constraint"
  ),
  business_role = list(
    revenue = c("objective_metric", "measurement_kpi"),
    spend = c("tactic_lever", "business_lever"),
    region = "contextual_control",
    promo = "strategy_indicator",
    inventory_cap = c("constraint", "guardrail", "risk_indicator")
  ),
  operational_eligibility = list(
    spend = c("controllable", "optimization_eligible", "experiment_eligible"),
    promo = "scenario_only",
    region = "adjustment_only",
    inventory_cap = "non_controllable"
  ),
  analytical_role = list(
    revenue = "target",
    spend = "predictor",
    promo = "known_future_regressor",
    region = "entity_variable",
    inventory_cap = "predictor"
  ),
  causal_role = list(
    spend = "exposure",
    revenue = "outcome",
    region = "confounder_candidate",
    promo = "moderator"
  ),
  confidence = list(
    revenue = 0.95,
    spend = 0.8,
    region = 0.7,
    promo = 0.9,
    inventory_cap = 0.85
  ),
  evidence_source = "human_annotation",
  business_context_refs = list(
    mission_id = "mission_growth",
    business_objective_id = "objective_revenue_growth",
    strategy_id = "strategy_media_efficiency",
    tactic_id = "tactic_budget_allocation",
    lever_id = "lever_paid_media_spend",
    risk_id = "risk_inventory_capacity",
    decision_id = "decision_budget_shift"
  ),
  dataset_id = "semantic_example"
)

validation <- aq_validate_variable_semantics(semantics)
artifact <- aq_variable_semantics_artifact(semantics)

semantics$mappings
validation
artifact$metadata$validation
