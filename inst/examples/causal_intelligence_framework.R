library(AutoQuant)

semantics <- aq_variable_semantics(
  variables = c("tv_spend", "revenue", "market_size", "brand_search"),
  business_role = list(
    tv_spend = "tactic_lever",
    revenue = "measurement_kpi",
    market_size = "contextual_control"
  ),
  operational_eligibility = list(tv_spend = "controllable"),
  causal_role = list(
    tv_spend = "exposure",
    revenue = "outcome",
    market_size = "confounder_candidate"
  )
)

decision <- aq_decision_context(
  context = list(
    decision_context_id = "decision_tv_budget",
    decision_question = "Should eligible markets increase weekly TV spend next quarter?"
  ),
  alternatives = list(
    list(alternative_id = "current_policy", name = "Current TV spend", baseline = TRUE),
    list(alternative_id = "increase_tv", name = "Increase TV spend", affected_levers = "tv_spend")
  ),
  variable_semantics = semantics
)

question <- aq_causal_question(
  question = list(
    causal_question_id = "cq_tv_revenue",
    decision_context_id = decision$decision_context_id,
    exposure = "tv_spend",
    outcome = "revenue",
    population = "eligible markets",
    unit_of_analysis = "market-week",
    time_zero = "campaign start",
    treatment_window = "next quarter",
    outcome_window = "same quarter and four-week lag",
    comparison_condition = "current weekly TV spend range",
    intervention_definition = "increase weekly TV spend from current market range to proposed eligible-market range",
    estimand = "ATE",
    effect_scale = "incremental revenue difference",
    target_population = "eligible markets"
  ),
  decision_context = decision
)

roles <- aq_causal_variable_roles(list(
  list(role_id = "role_x", causal_question_id = "cq_tv_revenue", variable = "tv_spend", role = "exposure", timing = "time_varying"),
  list(role_id = "role_y", causal_question_id = "cq_tv_revenue", variable = "revenue", role = "outcome", timing = "post_treatment"),
  list(role_id = "role_c", causal_question_id = "cq_tv_revenue", variable = "market_size", role = "confounder_candidate", timing = "baseline"),
  list(role_id = "role_m", causal_question_id = "cq_tv_revenue", variable = "brand_search", role = "mediator_candidate", timing = "post_treatment")
))

relationships <- aq_causal_relationships(list(
  list(relationship_id = "edge_cx", causal_question_id = "cq_tv_revenue", source_variable = "market_size", destination_variable = "tv_spend", relationship_type = "causes"),
  list(relationship_id = "edge_cy", causal_question_id = "cq_tv_revenue", source_variable = "market_size", destination_variable = "revenue", relationship_type = "causes"),
  list(relationship_id = "edge_xy", causal_question_id = "cq_tv_revenue", source_variable = "tv_spend", destination_variable = "revenue", relationship_type = "may_cause"),
  list(relationship_id = "edge_xm", causal_question_id = "cq_tv_revenue", source_variable = "tv_spend", destination_variable = "brand_search", relationship_type = "causes"),
  list(relationship_id = "edge_my", causal_question_id = "cq_tv_revenue", source_variable = "brand_search", destination_variable = "revenue", relationship_type = "causes")
))

context <- aq_causal_context(
  causal_question = question,
  roles = roles,
  relationships = relationships,
  variable_semantics = semantics,
  assumptions = c("observational treatment variation exists", "historical pre-period is available")
)

plan <- aq_plan_causal_investigation(context)
artifact <- aq_causal_planning_artifact(context, plan)

context$identification
context$adjustment_guidance
artifact$metadata
