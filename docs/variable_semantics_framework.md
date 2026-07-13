# Canonical Variable Semantics Framework

Status: Semantic Intelligence Phase 1 implemented.

Variables are no longer treated only as columns. They can now be represented as
analytical objects with explicit meaning. This semantic layer is intended to be
shared by feature engineering, modeling, forecasting, reporting, optimization,
experiment planning, campaign reasoning, GenAI guidance, and future autonomous
analytical systems.

This phase establishes the shared language. It does not rewrite existing
operators, implement causal estimators, implement optimization, or introduce
AutoML.

## Business-Intent Alignment

The semantic framework is designed to sit downstream of a future business
intent layer. That layer is not implemented in Phase 1, but variable semantics
can already reference future intent artifacts through `business_context_refs`.

The intended future trace is:

```text
mission
-> business objective
-> strategy
-> tactic
-> measurement/KPI
-> variable semantics
-> analytical operator
-> evidence
-> recommendation
-> decision
-> strategy validation or revision
```

This matters because the same variable can mean different things under
different objectives, strategies, tactics, causal questions, or decision
contexts. A variable may simultaneously be a business tactic, a controllable
driver, a forecasting regressor, a candidate causal exposure, and an
optimization variable. These roles are deliberately not collapsed into one
field.

## Public API

- `aq_variable_semantics()`
- `aq_validate_variable_semantics()`
- `aq_variable_semantics_artifact()`
- `qa_variable_semantics_framework()`

## Philosophy

Variables are not merely predictors. They may be outcomes, controls,
constraints, future-known regressors, possible confounders, identifiers,
optimization candidates, guardrails, business levers, risk indicators, or
unknowns. Meaning influences the analytical choices a system should make.

Semantic knowledge should be explicit, inspectable, uncertain when necessary,
and reusable across operators.

## Orthogonal Semantic Dimensions

The framework does not force a variable into exactly one category. A variable
may have multiple assignments across multiple dimensions.

| Dimension | Examples |
| --- | --- |
| Business role | objective metric, strategy indicator, tactic lever, business lever, operational driver, contextual control, system state, constraint, guardrail, risk indicator, cost, utility, measurement/KPI |
| Operational eligibility | controllable, non-controllable, optimization eligible, scenario only, experiment eligible, reporting only, adjustment only |
| Analytical role | target, predictor, known-future regressor, derived feature, identifier, hierarchy/entity variable |
| Causal role | exposure, outcome, confounder candidate, mediator candidate, collider candidate, moderator, instrument candidate, selection variable, unknown |
| Operational | outcome, driver, control, constraint, state, identifier, scenario variable, optimization candidate, non-actionable |
| Temporal | baseline, time varying, future known, future unknown, lagged, post-treatment |
| Causal | exposure, outcome, mediator, confounder, candidate confounder, collider, instrument, moderator, selection variable, proxy, latent construct, unknown causal role |
| Forecasting | target, shared predictor, target-specific predictor, known future regressor, intermittent-demand feature, funnel stage, cross-target feature, hierarchy variable, entity variable |
| Measurement | observed, derived, engineered, aggregated, external, estimated, scenario, missingness indicator |
| Decision | optimization eligible, optimization prohibited, experiment candidate, guardrail, primary KPI, secondary KPI, business constraint |

Uncertainty is supported through confidence values and semantic labels such as
`candidate_driver`, `possible_confounder`, and `unknown`.

## Semantic Object

`aq_variable_semantics()` creates an `aq_variable_semantics` object containing:

- semantic id
- dataset id
- variable identities
- semantic mappings
- canonical taxonomy
- confidence
- evidence source
- human annotations
- LLM suggestions
- business context references
- deterministic validation
- supported downstream actions

The API is deliberately flat. Assignments can be supplied as named lists,
simple vectors, or data frames with `variable` and `semantic_type` columns.

## Validation

`aq_validate_variable_semantics()` returns deterministic diagnostics for:

- missing required metadata
- invalid variable references
- unknown dimensions
- unknown semantic types
- duplicate semantic assignments
- invalid confidence values
- missing evidence sources
- contradictory assignments

Examples of contradictory assignments:

- `future_known` and `future_unknown`
- `optimization_eligible` and `optimization_prohibited`
- `controllable` and `non_controllable`
- `optimization_candidate` and `non_actionable`
- forecasting `target` and `known_future_regressor`

Some contradictions are failures. Others are warnings because domain context may
make them explainable but still worth review.

## Canonical Semantic Artifact

`aq_variable_semantics_artifact()` creates a canonical
`variable_semantics_artifact` with:

- semantic mappings as the primary table
- taxonomy
- validation diagnostics
- confidence
- evidence sources
- annotations
- business context references
- warnings
- canonical artifact envelope
- supported actions

The artifact exposes `campaign_review`, `experiment_planning`,
`feature_engineering`, `forecasting`, `report`, `optimization_planning`,
`causal_planning`, and `knowledge_promotion` as downstream actions.

## Operator Integration Roadmap

| Subsystem | Current usage | Future semantic usage | Priority | Migration plan |
| --- | --- | --- | --- | --- |
| Rodeo | Applies deterministic feature transformations from explicit specs. | Use temporal, measurement, and decision semantics to recommend safe transformations, future-known handling, lag eligibility, missingness indicators, and leakage diagnostics. | High | Accept optional semantic artifact in feature-prep planning; do not change fit/apply replay contract. |
| AutoQuant supervised modeling | Uses target/features and validation specs. | Use operational, causal, and decision semantics to separate outcomes, drivers, controls, leakage risks, guardrails, and non-actionable variables. | High | Add optional semantics input to future specs; preserve current feature lists. |
| Forecasting | Uses target/date/entity/future-regressor parameters. | Use forecasting and temporal semantics to infer targets, known-future regressors, entity variables, hierarchy variables, funnel stages, and cross-target feature eligibility. | High | Let planning consume semantic artifacts before operator execution. |
| Scoring | Scores against fitted schemas and attached outcomes. | Use semantics to distinguish predictions, outcomes, guardrails, scenario variables, and post-treatment fields. | Medium | Add semantic references to scoring artifacts later. |
| Canonical artifacts | Preserve artifact identity, lineage, and supported actions. | Attach semantic artifacts as reusable evidence for reports, campaigns, GenAI, and knowledge promotion. | High | Consume `variable_semantics_artifact` without changing artifact envelope. |
| Campaigns | Evaluate bounded hypotheses and preserve learning evidence. | Use semantics to decide which hypotheses are meaningful, safe, actionable, or causally risky. | High | Campaign planners should read semantics before generating experiment specs. |
| Reports | Render tables, diagnostics, and recommendations. | Group variables into Drivers, Controls, State Variables, Constraints, Candidate Confounders, Mediators, Optimization Variables, Unknown Semantics. | Medium | Add optional semantic report sections; avoid breaking existing reports. |
| Feature tuning | Planned through evidence-guided experiments. | Use semantics to decide whether differencing, trend terms, anomalies, lags, rolling windows, weights, and cross-target features are appropriate. | High | Treat semantics as planning evidence, not hidden transformation behavior. |
| Model tuning | Treated as bounded analytical hypotheses. | Use semantics to constrain engine/loss/regularization/threshold experiments and prevent optimizing forbidden variables. | Medium | Add semantics to future campaign specs. |
| Strategy tuning | Compares independent, grouped, global, funnel, hurdle, cross-target, etc. | Use entity, hierarchy, target, stage, and causal role semantics to propose coherent strategy experiments. | High | Feed semantic artifact into forecasting capability planning. |
| Optimization | Not implemented. | Require optimization eligibility, constraints, guardrails, and KPI semantics before optimization planning. | Future | Implement only after semantic guardrails are mature. |
| Future causal operators | Not implemented. | Require exposure, outcome, confounder, mediator, collider, moderator, and instrument semantics. | Future | Semantics are prerequisite, not proof of causality. |
| Future MMM | Not implemented. | Require outcome, media drivers, controls, seasonality, constraints, saturation candidates, and decision variables. | Future | Use semantics to avoid mixing controls and decision levers. |
| Future reinforcement learning | Historical code exists, not vNext. | Require state, action, reward/KPI, constraints, guardrails, and transition timing semantics. | Future | Do not implement until semantic and decision contracts are stronger. |

## Business-Intent Roadmap

Future phases should explicitly connect semantics to:

- strategy/tactic alignment
- KPI and measurement mapping
- objective-to-evidence lineage
- strategy assumption testing
- tactic effectiveness experiments
- optimization against allowed tactics and constraints
- executive reporting by objective, strategy, and tactic
- automatic detection of strategy/tactic/measurement misalignment

Phase 1 only records reference fields such as mission, business objective,
strategy, tactic, initiative, process, KPI, lever, guardrail, constraint, risk,
decision, recommendation, experiment, and evidence ids. It does not invent full
schemas for those objects.

## Model-Family Assessment

| Model family | Semantic requirements | Already represented | Missing |
| --- | --- | --- | --- |
| SEM | latent constructs, indicators, causal paths, measurement roles | latent construct, proxy, observed/estimated | structural path grammar and identification checks |
| Bayesian Networks | nodes, candidate causal roles, priors, constraints | causal roles, confidence, evidence source | graph structure and conditional independence evidence |
| DAG estimators | exposure, outcome, confounders, colliders, instruments | causal semantic taxonomy | graph validation, adjustment-set logic |
| Uplift models | treatment/exposure, outcome, moderators, guardrails | exposure, outcome, moderator, primary KPI | treatment assignment mechanism and policy constraints |
| Dynamic treatment models | time-varying exposures/outcomes, post-treatment variables | temporal and causal dimensions | sequential exchangeability and treatment history contracts |
| MMM | outcome, media drivers, controls, constraints, scenario variables | operational, decision, temporal dimensions | saturation/carryover semantics and budget constraints |
| State-space models | observed states, latent states, time-varying variables | state, latent construct, time varying | state transition structure |
| Reinforcement learning | state, action, reward, constraints, guardrails | state, optimization candidate, KPI, guardrail | action space, transition policy, safety constraints |

## Reporting Roadmap

Future reports should be able to include semantic sections:

- Drivers
- Controls
- State variables
- Constraints
- Candidate confounders
- Potential mediators
- Optimization variables
- Unknown semantics
- Leakage and post-treatment caveats
- Decision guardrails

This phase creates the artifact these sections can consume. It does not rewrite
existing reports.

## Feature Engineering Roadmap

Semantics should eventually influence:

- differencing only for variables where change is meaningful
- trend representation for time-varying outcomes or drivers
- anomaly treatment for observed operational measures
- lag and rolling windows for strictly prior-history variables
- weighting where decision or business importance is explicit
- cross-target features only when target relationships are stated as empirical hypotheses
- future regressors only when variables are marked `future_known`
- missingness indicators when measurement semantics suggest meaningful missingness

## Experiment Planning Roadmap

Semantic artifacts should eventually help planners choose:

- strategy experiments that match entity/hierarchy/stage/target semantics
- feature experiments that match temporal and measurement semantics
- model experiments that respect constraints and guardrails
- causal experiments only when exposure/outcome/confounder semantics exist
- optimization experiments only when variables are explicitly eligible

The planner should not rediscover semantic assumptions independently. It should
consume the semantic artifact as shared knowledge.

## Example

```r
semantics <- aq_variable_semantics(
  variables = c("revenue", "spend", "region", "promo", "inventory_cap"),
  business_role = list(
    revenue = "objective_metric",
    spend = c("tactic_lever", "business_lever"),
    inventory_cap = "risk_indicator"
  ),
  operational_eligibility = list(spend = c("controllable", "optimization_eligible")),
  analytical_role = list(revenue = "target", promo = "known_future_regressor"),
  causal_role = list(spend = "exposure", region = "confounder_candidate"),
  operational = list(revenue = "outcome", spend = "driver", region = "control"),
  temporal = list(revenue = "future_unknown", promo = "future_known"),
  causal = list(spend = "exposure", region = "possible_confounder"),
  forecasting = list(revenue = "target", promo = "known_future_regressor"),
  decision = list(revenue = "primary_kpi", spend = "optimization_eligible"),
  confidence = list(revenue = 0.95, spend = 0.8, region = 0.7, promo = 0.9),
  business_context_refs = list(
    mission_id = "mission_growth",
    business_objective_id = "objective_revenue_growth",
    strategy_id = "strategy_media_efficiency",
    tactic_id = "tactic_budget_allocation",
    lever_id = "lever_paid_media_spend",
    risk_id = "risk_inventory_capacity"
  )
)

validation <- aq_validate_variable_semantics(semantics)
artifact <- aq_variable_semantics_artifact(semantics)
```

## Validation

```r
qa_variable_semantics_framework()
qa_autoquant_package()
```

## Completion Criterion

The semantic framework can become the shared bridge between organizational
intent and analytical evidence without redesigning the core variable contract
later.
