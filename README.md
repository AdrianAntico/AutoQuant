![Version: 1.0.0](https://img.shields.io/static/v1?label=Version&message=1.0.0&color=blue&?style=plastic)
![Build: Passing](https://img.shields.io/static/v1?label=Build&message=passing&color=brightgreen)
[![Maintenance](https://img.shields.io/badge/Maintained%3F-yes-green.svg)](https://GitHub.com/Naereen/StrapDown.js/graphs/commit-activity)
[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg?style=default)](http://makeapullrequest.com)
[![GitHub Stars](https://img.shields.io/github/stars/AdrianAntico/AutoQuant.svg?style=social)](https://github.com/AdrianAntico/AutoQuant)

<img src='https://raw.githubusercontent.com/AdrianAntico/AutoQuant/master/Images/AutoQuant.PNG' align='center' width='1000' />


## AutoQuant Reference Manual

![AutoQuant Reference Manual](https://github.com/AdrianAntico/AutoQuant/tree/master/vignette)

Companion Packages:
- ![Quantico](https://github.com/AdrianAntico/Quantico)
- ![Rodeo](https://github.com/AdrianAntico/Rodeo)
- ![AutoPlots](https://github.com/AdrianAntico/AutoPlots)

Table of Contents
- [Background](#background)
- [Installation](#installation)

Documentation + Code Examples
- [Supervised Learning](#supervised-learning-)
- [Model Scoring](#model-scoring-)
- [Model Evaluation](#model-evaluation-)
- [Panel Data Forecasting](#panel-data-forecasting-)
- [Time Series Forecasting](#time-series-forecasting-)

Architecture + Redesign Notes
- [AutoQuant vNext Archaeology And Design](docs/autoquant_vnext_archaeology_and_design.md) describes proposed next-generation contracts for supervised learning, scoring, forecasting, panel forecasting, typed artifacts, Rodeo ownership boundaries, AnalyticsShinyApp integration, and H2O retirement strategy. This is design documentation, not an implemented API manual.
- [AutoQuant vNext CatBoost Supervised Learning](docs/vnext_catboost_regression.md) documents the first implemented vNext vertical slices: CatBoost regression and binary classification specs, deterministic fit, prediction artifacts, canonical scoring artifacts, delayed outcome attachment, realized assessment, bounded monitoring evidence, threshold policy, and installed-package QA.
- [AutoQuant vNext Forecasting Foundation](docs/vnext_forecasting_foundation.md) documents naive, seasonal naive, ETS, ARIMA, CatBoost forecasting, panel forecasting, hierarchy reconciliation, panel strategy comparison, intermittent-demand forecasting, and rolling-origin evidence.
- [AutoQuant vNext Intermittent Demand Forecasting](docs/vnext_intermittent_demand_forecasting.md) documents Croston, SBA, TSB, supervised Hurdle, diagnostics, method comparison, and advisory selection evidence.
- [AutoQuant vNext Funnel Forecasting Foundation](docs/vnext_funnel_forecasting_foundation.md) documents funnel specification, explicit transitions, maturity evidence, stage forecasting, transition forecasting, assessment, and strategy comparison.
- [AutoQuant vNext Multi-Target Forecasting Foundation](docs/vnext_multitarget_forecasting_foundation.md) documents multi-target specification, shared temporal preparation evidence, target-level forecasts, cross-target evidence, assessment, and strategy comparison.
- [AutoQuant vNext Cross-Target Forecasting](docs/vnext_cross_target_forecasting.md) documents supervised CatBoost cross-target feature learning, Rodeo-owned leakage-safe feature preparation, negative-transfer evidence, and advisory strategy comparison.
- [AutoQuant vNext Forecasting Capability Planning](docs/vnext_forecasting_planning.md) documents deterministic forecasting capability discovery, evidence-guided strategy planning, planning artifacts, and the historical CatBoost CARMA mechanism inventory for future feature-tuning experiments.
- [Causal Intelligence Philosophy](docs/causal_intelligence_philosophy.md) explains why causal planning is separate from prediction, association, and effect estimation.
- [Causal Intelligence Framework](docs/causal_intelligence_framework.md) documents Phase 1 causal questions, estimands, question-relative roles, graph diagnostics, adjustment guidance, identification planning, design eligibility, and causal planning artifacts.
- [Causal Experiment Design Framework](docs/causal_experiment_design_framework.md) documents Phase 2 governed experiment questions, design specs, deterministic assignment proposals, balance diagnostics, power/timing/measurement plans, validity threats, approval gates, information value, and experiment plan artifacts.
- [Causal Completed-Experiment Evidence Framework](docs/causal_completed_experiment_framework.md) documents Phase 3 completed-experiment evidence ingestion, assignment preservation, execution reconciliation, integrity/fidelity diagnostics, estimand preservation, and analysis-readiness classification without estimating effects.
- [Causal Randomized ITT Estimation Framework](docs/causal_randomized_itt_framework.md) documents Phase 4 governed randomized intent-to-treat estimation, readiness gating, uncertainty, guardrails, materiality, sensitivity, effect artifacts, lifecycle review, and prohibited claims.
- [Causal Randomized Design Analysis Framework](docs/causal_randomized_design_analysis_framework.md) documents Phase 5 randomized-analysis depth, governed variance reduction, design-aware method eligibility, outcome windows, carryover, multiplicity, guardrail decisions, robustness, and causal report contracts.
- [Observational Causal Planning Framework](docs/observational_causal_planning_framework.md) documents observational study design, target-trial thinking, assignment-mechanism evidence, temporal eligibility, overlap, balance, selection, unmeasured-confounding risk, estimator-family readiness, and planning artifacts without estimating observational treatment effects.

<br>

## AutoQuant vNext Cookbook

The vNext API is the artifact-first redesign path. It is additive and does not remove the legacy AutoQuant wrappers. The examples below are copy-paste oriented and correspond to validated scripts in `inst/examples/`.

### vNext Quick Start

<details>Click to expand

```r
library(data.table)
library(AutoQuant)

dt <- data.table(
  id = 1:80,
  spend = runif(80, 10, 100),
  clicks = rpois(80, 40),
  channel = sample(c("Search", "Social", "Email"), 80, TRUE)
)
dt[, revenue := 25 + 1.8 * spend + 0.7 * clicks + rnorm(.N, 0, 5)]

spec <- aq_model_spec(
  task = "regression",
  target = "revenue",
  features = c("spend", "clicks", "channel"),
  engine_params = list(iterations = 10, depth = 2),
  dataset_id = "example_training_data"
)

fit <- aq_fit_model(spec, dt)
score <- aq_score_model(fit, dt[1:10], row_id_cols = "id", outcome_col = "revenue")
```

Validated script: `inst/examples/vnext_supervised_learning.R`

### Causal Intelligence Planning

Causal Intelligence Phase 1 is planning-only. It helps define the causal question, estimand, roles, graph assumptions, adjustment guidance, identification concerns, candidate designs, and a canonical planning artifact. It does not estimate treatment effects.

```r
question <- aq_causal_question(
  question = list(
    causal_question_id = "cq_tv_revenue",
    decision_context_id = "decision_tv_budget",
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
  )
)
```

Validated script: `inst/examples/causal_intelligence_framework.R`

### Causal Randomized ITT Estimation

Causal Intelligence Phase 4 is the first estimator slice. It runs only randomized intent-to-treat analyses after completed-experiment evidence is classified as ITT-compatible. It does not estimate treatment-on-treated effects, observational effects, matching, IV, DiD, mediation, synthetic controls, causal forests, or optimization.

```r
spec <- aq_randomized_itt_spec(
  completed_experiment_id = "ce_itt",
  experiment_plan_artifact_id = "plan_itt",
  causal_question_id = "cq_itt",
  estimand_id = "estimand_itt",
  treatment_arm = "treatment",
  comparison_arm = "control",
  outcome = "revenue",
  outcome_type = "continuous",
  baseline_covariates = "baseline_y",
  minimum_meaningful_effect = 1
)

gate <- aq_validate_randomized_itt_readiness(
  spec,
  completed_evidence,
  planned_analysis = planned_analysis_record,
  baseline_data = baseline_data
)

result <- aq_estimate_randomized_itt(
  spec,
  completed_evidence,
  baseline_data = baseline_data,
  planned_analysis = planned_analysis_record
)
artifact <- aq_randomized_itt_effect_artifact(result)
```

Validated script: `inst/examples/causal_randomized_itt_framework.R`

### Causal Randomized Design Analysis

Causal Intelligence Phase 5 deepens randomized evidence after ITT estimation. It preserves the unadjusted assignment effect while adding design-aware analysis, governed ANCOVA, CUPED-style variance reduction, blocked/stratified evidence, cluster diagnostics, randomization inference, multiplicity, guardrail decisions, robustness, and a causal-effect report contract.

```r
design_spec <- aq_randomized_design_analysis_spec(
  itt_analysis_id = result$spec$analysis_id,
  design_type = "blocked_randomized",
  analysis_modes = c("unadjusted", "ancova", "cuped", "blocked", "randomization_inference"),
  block_fields = "block",
  cluster_unit = "market",
  pre_period_fields = "pre_revenue",
  material_benefit = 1,
  material_harm = -1,
  multiplicity_policy = "holm"
)

depth <- aq_analyze_randomized_design_depth(result, design_spec)
report <- aq_randomized_causal_effect_report(result, depth)
```

Validated script: `inst/examples/causal_randomized_design_analysis_framework.R`

### Causal Experiment Design Planning

Causal Experiment Design Phase 2 extends causal planning into governed experiment-design artifacts. It does not execute treatment and does not estimate causal effects.

```r
experiment_question <- aq_experiment_question(list(
  experiment_question_id = "eq_budget_test",
  causal_question_id = "cq_budget_revenue",
  decision_context_id = "decision_budget",
  hypothesis = "Increasing budget raises revenue.",
  null_claim = "No revenue lift.",
  alternative_claim = "Revenue lift is positive.",
  treatment = "approved budget increase",
  comparison = "current budget",
  estimand = "ATE",
  assignment_population = "eligible markets",
  expected_mechanism = "More qualified reach creates more revenue.",
  primary_outcome = "revenue",
  guardrails = "cpa,customer_quality",
  decision_rule = "Approve rollout if lift exceeds cost and guardrails hold.",
  authority = "marketing_approval",
  coverage = "eligible_markets"
), causal_context)

design <- aq_experiment_design_spec(
  experiment_question,
  design_type = "stratified_randomized",
  assignment_unit = "market",
  analysis_unit = "market-week",
  stratification_variables = "region"
)
```

Validated script: `inst/examples/causal_experiment_design_framework.R`

### Causal Completed-Experiment Evidence

Causal Intelligence Phase 3 ingests completed or in-progress experiment evidence and classifies analysis readiness. It preserves original assignment as the default ITT anchor, records treatment delivery, exposure, compliance, outcomes, guardrails, exclusions, and plan-versus-execution deviations, then creates a completed-experiment evidence artifact. It does not estimate treatment effects.

```r
completed <- aq_completed_experiment(list(
  completed_experiment_id = "ce_budget_test",
  experiment_plan_artifact_id = "aq_experiment_plan_budget_test",
  decision_context_id = "decision_budget",
  causal_question_id = "cq_budget_revenue",
  estimand_id = "estimand_budget_itt",
  design_version = "v1",
  assignment_version = "v1",
  experiment_status = "completed",
  actual_start_date = "2026-01-01",
  actual_end_date = "2026-02-01",
  data_cutoff_date = "2026-02-15",
  execution_owner = "analytics"
))

assignment <- aq_assignment_evidence(data.table::data.table(
  unit_id = paste0("market_", 1:12),
  planned_arm = rep(c("control", "treatment"), each = 6),
  realized_assigned_arm = rep(c("control", "treatment"), each = 6)
))

outcomes <- aq_outcome_evidence(data.table::data.table(
  unit_id = paste0("market_", 1:12),
  outcome_id = "revenue",
  value = seq(100, 155, by = 5),
  outcome_role = "primary"
))

readiness <- aq_assess_experiment_analysis_readiness(
  completed,
  assignment_evidence = assignment,
  outcome_evidence = outcomes
)
```

Validated script: `inst/examples/causal_completed_experiment_framework.R`

### Supervised Learning

Use `aq_model_spec()` to create an explicit supervised model contract, then `aq_fit_model()` to train. Current vNext supervised operators support CatBoost regression and binary classification. The returned fit includes the model, fit metrics, partition evidence, model metadata, and canonical artifacts.

```r
binary_spec <- aq_model_spec(
  task = "binary",
  target = "converted",
  features = c("spend", "clicks", "channel"),
  positive_class = "yes",
  engine_params = list(iterations = 10, depth = 2)
)

binary_fit <- aq_fit_model(binary_spec, training_data)
```

### Scoring and Realized Outcomes

Use `aq_score_model()` for canonical scoring artifacts. If outcomes are available at scoring time, pass `outcome_col`. Delayed outcomes can be attached later through the vNext scoring lifecycle helpers described in `docs/vnext_catboost_regression.md`.

```r
scored <- aq_score_model(
  fit,
  new_data,
  row_id_cols = "customer_id",
  outcome_col = "actual_outcome",
  dataset_id = "campaign_scoring_population"
)
```

### Model Bundles

Use model bundles when the same fitted model must be reloaded for reproducible inference.

```r
bundle <- aq_save_model_bundle(fit, "exports/model_bundle", overwrite = TRUE)
loaded <- aq_load_model_bundle("exports/model_bundle")
rescored <- aq_score_model(loaded, new_data, row_id_cols = "customer_id")
```

### Canonical Analytical Artifacts

vNext results carry canonical artifacts so downstream apps, reports, campaigns, and agents can consume evidence without reverse-engineering return objects. See `docs/canonical_analytical_artifacts.md` and `inst/examples/artifact_schema_example.R`.

### Canonical Variable Semantics

Use `aq_variable_semantics()` to describe what variables mean across business
role, operational eligibility, analytical role, causal role, operational,
temporal, forecasting, measurement, and decision dimensions. Variable semantics
are shared analytical knowledge for future feature engineering, modeling,
forecasting, reporting, campaign reasoning, optimization planning, risk review,
and GenAI guidance. The semantic artifact records confidence, evidence source,
business-context references for future objectives, strategies, tactics, levers,
KPIs, risks, and decisions, validation diagnostics, and supported downstream
actions.

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

Validated script: `inst/examples/variable_semantics_framework.R`

### Business Intent and Lever Management

Use `aq_business_intent()` to describe the organizational layer above
variables: mission, objective, strategy, tactic, lever, KPI, guardrail,
constraint, risk, assumption, recommendation, decision, authority, and
coverage. This contract keeps organizational intent separate from variable
semantics while linking them through deterministic relationships and
variable-to-lever mappings.

The framework can assess structural strategy/tactic/measurement alignment,
classify levers and assumptions as explore/exploit candidates, distinguish
capability from authority, and create a canonical `business_intent_artifact`.
It does not implement optimization, causal estimation, MMM, reinforcement
learning, or automatic strategy changes.

```r
semantics <- aq_variable_semantics(
  variables = c("revenue", "paid_search_spend", "planned_paid_search_spend", "inventory_cap"),
  business_role = list(
    revenue = c("objective_metric", "measurement_kpi"),
    paid_search_spend = c("tactic_lever", "business_lever", "cost"),
    planned_paid_search_spend = c("tactic_lever", "business_lever"),
    inventory_cap = c("constraint", "guardrail", "risk_indicator")
  ),
  operational_eligibility = list(
    paid_search_spend = c("controllable", "optimization_eligible", "experiment_eligible"),
    planned_paid_search_spend = c("controllable", "scenario_only"),
    inventory_cap = "non_controllable"
  ),
  analytical_role = list(
    revenue = "target",
    paid_search_spend = "predictor",
    planned_paid_search_spend = "known_future_regressor",
    inventory_cap = "predictor"
  ),
  causal_role = list(
    paid_search_spend = "exposure",
    revenue = "outcome",
    inventory_cap = "moderator"
  ),
  decision = list(
    revenue = "primary_kpi",
    paid_search_spend = "optimization_eligible",
    inventory_cap = "guardrail"
  )
)

intent <- aq_business_intent(
  missions = list(
    mission_id = "mission_growth",
    title = "Sustainable Growth"
  ),
  objectives = list(
    objective_id = "objective_revenue_growth",
    mission_id = "mission_growth",
    title = "Increase Revenue",
    primary_kpis = list("kpi_revenue")
  ),
  strategies = list(
    strategy_id = "strategy_qualified_demand",
    objective_id = "objective_revenue_growth",
    title = "Increase Qualified Demand",
    intended_mechanism = "Increase awareness and intent among eligible prospects."
  ),
  tactics = list(
    tactic_id = "tactic_paid_search",
    strategy_id = "strategy_qualified_demand",
    title = "Expand Paid Search Acquisition"
  ),
  levers = list(
    lever_id = "lever_paid_search_budget",
    tactic_id = "tactic_paid_search",
    related_variables = list(c("paid_search_spend", "planned_paid_search_spend")),
    evidence_strength = 0.82,
    uncertainty = 0.25,
    economic_importance = 0.9,
    measurement_quality = 0.8,
    reversibility = TRUE,
    experiment_eligible = TRUE,
    optimization_eligible = TRUE,
    execution_eligible = FALSE,
    approval_required = TRUE,
    validated_range = "80000-120000"
  ),
  kpis = list(
    kpi_id = "kpi_revenue",
    objective_id = "objective_revenue_growth",
    source_variable = "revenue",
    role = "primary"
  ),
  guardrails = list(
    guardrail_id = "guardrail_capacity",
    tactic_id = "tactic_paid_search",
    source_variable = "inventory_cap",
    measured = TRUE
  ),
  constraints = list(
    constraint_id = "constraint_budget",
    lever_id = "lever_paid_search_budget",
    lower_bound = 50000,
    upper_bound = 150000
  ),
  assumptions = list(
    assumption_id = "assumption_search_capacity",
    strategy_id = "strategy_qualified_demand",
    tactic_id = "tactic_paid_search",
    lever_id = "lever_paid_search_budget",
    statement = "Search capacity can absorb incremental qualified demand.",
    evidence_status = "partial",
    experiment_eligible = TRUE,
    evidence_strength = 0.45,
    unresolved_uncertainty = 0.65,
    economic_importance = 0.85,
    measurement_quality = 0.75
  ),
  authority = list(
    authority_id = "authority_marketing_advisory",
    authority_level = "recommend",
    approval_required = TRUE,
    required_approver = "CMO"
  ),
  coverage = list(
    coverage_id = "coverage_marketing_only",
    represented_domains = list("marketing"),
    missing_domains = list(c("finance", "operations")),
    limitation = "Marketing scope only; not an enterprise-wide allocation recommendation."
  ),
  variable_semantics = semantics
)

validation <- aq_validate_business_intent(intent)
alignment <- aq_assess_business_alignment(intent)
explore_exploit <- aq_assess_explore_exploit(intent)
artifact <- aq_business_intent_artifact(intent)
```

Validated script: `inst/examples/business_intent_framework.R`

Focused QA:

```r
qa_business_intent_framework()
```

### Decision Management

Use `aq_decision_context()` when analysis needs to support a governed decision
rather than merely describe model output. Decision contexts preserve the decision
question, baseline/current-policy alternative, candidate alternatives, criteria,
financial impact, uncertainty, optionality, recommendation, selected decision,
and outcome follow-up.

Use `aq_decision_alternative()` and `aq_decision_optionality()` directly when
building those record tables before assembling the full context.

The framework is deterministic and human-governed. It can assess alternatives,
flag authority escalation, identify reducible uncertainty, and produce a
canonical `decision_context_artifact`; it does not execute actions or optimize
budgets automatically.

```r
decision <- aq_decision_context(
  context = list(
    decision_context_id = "decision_paid_search_budget",
    decision_question = "Should paid-search budget remain flat, increase within the validated range, or be piloted above the validated range?",
    related_objectives = list("objective_revenue_growth"),
    related_tactics = list("tactic_paid_search"),
    related_levers = list("lever_paid_search_budget"),
    authority = "authority_marketing_advisory",
    coverage = "coverage_marketing_only"
  ),
  alternatives = list(
    list(alternative_id = "alt_baseline", name = "Keep current budget", alternative_type = "do_nothing", baseline = TRUE),
    list(alternative_id = "alt_validated_increase", name = "Increase within validated range", alternative_type = "partial_implementation", authority_compatible = TRUE),
    list(alternative_id = "alt_pilot", name = "Pilot above validated range", alternative_type = "pilot", authority_compatible = FALSE)
  ),
  financial_impacts = list(
    list(financial_id = "fin_baseline", alternative_id = "alt_baseline", recurring_cost = 100000, expected_benefit = 110000),
    list(financial_id = "fin_validated", alternative_id = "alt_validated_increase", recurring_cost = 115000, expected_benefit = 140000),
    list(financial_id = "fin_pilot", alternative_id = "alt_pilot", recurring_cost = 140000, expected_benefit = 160000, downside_estimate = -35000)
  ),
  uncertainties = list(
    list(uncertainty_id = "unc_pilot_transfer", alternative_id = "alt_pilot", reducibility = "reducible", decision_sensitivity = "high")
  ),
  optionality = list(
    list(optionality_id = "opt_pilot_learning", alternative_id = "alt_pilot", option_type = "learn", future_decisions_enabled = list(c("expand", "abandon")), reversibility = TRUE)
  ),
  business_intent = intent,
  variable_semantics = semantics
)

validation <- aq_validate_decision_context(decision)
alternative_assessment <- aq_assess_decision_alternatives(decision)
optionality_assessment <- aq_assess_decision_optionality(decision)
artifact <- aq_decision_context_artifact(decision)
```

Validated script: `inst/examples/decision_management_framework.R`

Focused QA:

```r
qa_decision_management_framework()
```

### Decision Valuation Intelligence

Use `aq_decision_valuation_context()` after alternatives have been authored and
evidence exists that may change the economic interpretation of those
alternatives. The valuation layer compares alternatives against a baseline,
preserves whether inputs are observed, causal, predictive, forecast, assumed,
imported, missing, or unsupported, and computes transparent economics such as
net benefit, ROI, payback, NPV, incremental NPV, thresholds, and governed
recommendation states.

Decision valuation does not approve, optimize, allocate budget, or execute. It
creates auditable valuation evidence.

```r
valuation <- aq_decision_valuation_context(
  decision_context_id = "decision_paid_search_budget",
  alternatives_included = c("alt_baseline", "alt_pilot"),
  baseline_alternative = "alt_baseline",
  objective_refs = "objective_revenue_growth",
  strategy_refs = "strategy_qualified_demand",
  tactic_refs = "tactic_paid_search",
  lever_refs = "lever_paid_search_budget",
  authority = "authority_marketing_advisory",
  coverage = "coverage_marketing_only",
  time_horizon_periods = 1,
  discount_rate = 0.01
)

impact <- aq_evidence_impact_mapping(list(
  list(
    mapping_id = "itt_to_margin",
    source_artifact_id = "randomized_itt_effect_artifact",
    alternative_id = "alt_pilot",
    evidence_type = "randomized_itt",
    estimand_or_prediction = "ATE",
    effect_value = 0.04,
    affected_population = 50000,
    duration_periods = 1,
    unit_value = 8,
    source_type = "causally_estimated"
  )
))

economics <- aq_assess_alternative_economics(valuation, impact_mappings = impact)
recommendation <- aq_governed_decision_valuation_recommendation(valuation, economics)
artifact <- aq_decision_valuation_artifact(valuation, economics, recommendation)
```

Validated script: `inst/examples/decision_valuation_framework.R`

Focused QA:

```r
qa_decision_valuation_framework()
```

### Decision Workflow Intelligence

Use `aq_decision_workflow()` after valuation has produced a governed
recommendation and the organization needs durable human review, approval,
implementation tracking, monitoring, and realized-value follow-through.

The workflow layer preserves that recommendation, decision, implementation, and
outcome are separate facts. It can assess review readiness, freeze an evidence
package, record reviews and approvals, attach conditions, reconcile realized
implementation against an approved plan, compare expected and realized value,
and generate follow-up decision candidates. It does not approve, execute, or
replace organizational authority.

```r
workflow <- aq_decision_workflow(
  workflow_id = "workflow_paid_search_pilot",
  decision_context_id = "decision_paid_search_budget",
  valuation_artifact_id = artifact$id,
  recommendation_id = "recommendation_paid_search_pilot",
  selected_alternative = "alt_pilot",
  workflow_type = "pilot_approval",
  authority_tier = "manager"
)

readiness <- aq_assess_decision_review_readiness(workflow)
evidence_package <- aq_decision_evidence_package(
  workflow,
  evidence_refs = c(artifact$id, "randomized_itt_effect_artifact")
)

review <- aq_decision_review(list(
  workflow_id = workflow$workflow_id,
  reviewer = "Finance Owner",
  role = "financial",
  status = "endorse_with_conditions",
  conditions = "Monitor weekly spend and guardrails."
))

approval <- aq_decision_approval(list(
  workflow_id = workflow$workflow_id,
  approver = "Budget Manager",
  authority_basis = "marketing_budget_authority",
  approved_alternative = "alt_pilot",
  approved_budget = 100,
  authority_magnitude = 200,
  status = "conditionally_approved"
))

plan <- aq_decision_implementation_plan(list(
  workflow_id = workflow$workflow_id,
  implementation_id = "impl_paid_search_pilot",
  selected_alternative = "alt_pilot",
  approved_target_values = "paid_search_budget=110",
  budget = 100
))

actual <- aq_record_decision_implementation(list(
  workflow_id = workflow$workflow_id,
  implementation_id = "impl_paid_search_pilot",
  actual_cost = 105,
  actual_lever_settings = "paid_search_budget=110"
))

reconciliation <- aq_reconcile_decision_implementation(plan, actual)
quality <- aq_assess_decision_quality(workflow, readiness, review, approval, reconciliation)
```

Validated script: `inst/examples/decision_workflow_framework.R`

Focused QA:

```r
qa_decision_workflow_framework()
```

### Decision Lifecycle and Organizational Memory

Use `aq_review_decision()` after a governed decision has an observable outcome.
The review records expected outcome, realized outcome, actual value, variance,
execution state, assumption status, lessons learned, strategy implications,
lever implications, and future recommendations. This turns decisions into
durable organizational memory instead of one-time recommendations.

Use `aq_decision_timeline()` to reconstruct the deterministic lifecycle:
context, alternatives, recommendations, selected decision, outcome follow-up,
and learning review.

Use `aq_decision_learning_summary()` to summarize whether outcome evidence
validated the decision pattern, produced partial learning, or created negative
evidence. Use `aq_decision_memory_artifact()` to package the timeline and
learning summary as a canonical artifact for collectors, reports, GenAI bounded
context, and future knowledge promotion.

```r
review <- aq_review_decision(
  decision,
  decision_id = "decision_budget_pending",
  realized_outcome = "Validated response inside delegated range.",
  actual_value = 36000,
  lessons_learned = "Validated range remains reusable.",
  assumption_status = "held"
)

timeline <- aq_decision_timeline(decision, review)
learning <- aq_decision_learning_summary(decision, review)
memory_artifact <- aq_decision_memory_artifact(decision, review)
```

Validated script: `inst/examples/decision_lifecycle_and_memory.R`

Focused QA:

```r
qa_decision_lifecycle_framework()
```

### Time-Series Forecasting

Use `aq_forecast_spec()` and `aq_fit_forecast()` for deterministic single-series forecasting. Current engines include `naive`, `seasonal_naive`, `ets`, `arima`, and `catboost`.

```r
forecast_spec <- aq_forecast_spec(
  target = "demand",
  date = "ds",
  frequency = "day",
  horizon = 14,
  engine = "seasonal_naive",
  season_length = 7,
  prediction_intervals = TRUE
)

forecast <- aq_fit_forecast(forecast_spec, demand_history)
assessment <- aq_assess_forecast(forecast)
```

Validated script: `inst/examples/vnext_forecasting.R`

### CatBoost Forecasting

CatBoost forecasting uses the same `aq_forecast_spec()` contract with `engine = "catboost"`. Rodeo owns temporal transformation replay; AutoQuant owns model fitting, forecast artifacts, and assessment evidence.

```r
catboost_forecast_spec <- aq_forecast_spec(
  target = "sales",
  date = "ds",
  frequency = "day",
  horizon = 7,
  engine = "catboost",
  forecast_strategy = "direct",
  future_known_variables = c("promotion", "holiday"),
  engine_parameters = list(iterations = 25, depth = 3)
)
```

### Panel Forecasting

Use `aq_panel_forecast_spec()` when the same forecast target appears across multiple entities. The first vNext panel operator is a global CatBoost forecast with entity-aware evidence.

```r
panel_spec <- aq_panel_forecast_spec(
  entity = "store",
  target = "sales",
  date = "ds",
  frequency = "day",
  horizon = 7,
  engine_parameters = list(iterations = 25, depth = 3)
)
```

Validated contract script: `inst/examples/vnext_panel_hierarchy_strategy.R`

### Hierarchical Forecasting

Use `aq_hierarchy_spec()` and `aq_reconcile_hierarchical_forecast()` when entity forecasts must reconcile to parent totals. Current vNext reconciliation is deterministic bottom-up.

```r
hierarchy_spec <- aq_hierarchy_spec(hierarchy_table)
reconciled <- aq_reconcile_hierarchical_forecast(panel_forecast, hierarchy_spec)
```

### Panel Strategy Comparison

Use `aq_panel_strategy_spec()` and `aq_evaluate_panel_strategies()` to compare independent, grouped, and global panel approaches. The recommendation is advisory evidence, not automatic production selection.

```r
strategy_spec <- aq_panel_strategy_spec(
  entity = "store",
  group = "region",
  target = "sales",
  date = "ds",
  candidate_strategies = c("independent", "grouped", "global"),
  horizon = 7
)
```

### Intermittent-Demand Forecasting

Intermittent demand is a problem family with classical and supervised operators:

- Classical: `aq_croston_forecast_spec()`, `aq_sba_forecast_spec()`, `aq_tsb_forecast_spec()`
- Supervised: `aq_hurdle_forecast_spec()`
- Comparison: `aq_compare_intermittent_demand_methods()`

```r
diagnostics <- aq_intermittent_demand_diagnostics(history, target = "demand", date = "ds")

croston <- aq_fit_croston_forecast(
  aq_croston_forecast_spec(target = "demand", date = "ds", horizon = 7),
  history
)

sba <- aq_fit_sba_forecast(
  aq_sba_forecast_spec(target = "demand", date = "ds", horizon = 7),
  history
)

tsb <- aq_fit_tsb_forecast(
  aq_tsb_forecast_spec(target = "demand", date = "ds", horizon = 7, beta = 0.2),
  history
)

comparison <- aq_compare_intermittent_demand_methods(
  data = history,
  target = "demand",
  date = "ds",
  horizon = 3,
  include_hurdle = FALSE
)
```

Validated script: `inst/examples/vnext_intermittent_demand.R`

### Funnel Forecasting

Funnel forecasting treats a process as ordered stages and explicit adjacent
transitions. It is different from forecasting unrelated columns.

```r
funnel_spec <- aq_funnel_forecast_spec(
  stages = c("prospects", "applications", "qualified", "enrollments"),
  stage = "stage",
  value = "volume",
  date = "date",
  cohort = "cohort",
  stage_date = "stage_date",
  maturity = "maturity",
  frequency = "week",
  horizon = 2,
  forecast_origin = as.Date("2026-02-12"),
  strategy = "stage"
)

validation <- aq_validate_funnel_forecast_spec(funnel_spec, funnel_data)
stage_forecast <- aq_fit_funnel_forecast(funnel_spec, funnel_data)
stage_assessment <- aq_assess_funnel_forecast(stage_forecast)

transition_spec <- funnel_spec
transition_spec$strategy <- "transition"
transition_forecast <- aq_fit_funnel_forecast(transition_spec, funnel_data)

comparison <- aq_compare_funnel_strategies(funnel_spec, funnel_data)
```

Validated script: `inst/examples/vnext_funnel_forecasting.R`

### Multi-Target Forecasting

Multi-target forecasting treats several related targets as a shared analytical
problem while preserving target-level evidence. Phase 19 supports deterministic
independent and shared-workflow strategies; it does not implement VAR, VARMAX,
state-space multivariate models, deep learning, or target-causality modeling.

```r
multitarget_spec <- aq_multitarget_forecast_spec(
  targets = c("leads", "applications", "enrollments"),
  date = "date",
  frequency = "week",
  horizon = 3,
  forecast_origin = max(multitarget_data$date) - 21,
  known_future_variables = "promo",
  shared_predictors = "spend",
  target_specific_predictors = list(enrollments = "promo"),
  strategy = "independent",
  engine = "seasonal_naive",
  season_length = 4
)

validation <- aq_validate_multitarget_forecast_spec(multitarget_spec, multitarget_data)
forecast <- aq_fit_multitarget_forecast(multitarget_spec, multitarget_data)
assessment <- aq_assess_multitarget_forecast(forecast)

shared_spec <- multitarget_spec
shared_spec$strategy <- "shared_workflow"
shared_forecast <- aq_fit_multitarget_forecast(shared_spec, multitarget_data)

comparison <- aq_compare_multitarget_strategies(multitarget_spec, multitarget_data)
```

Validated script: `inst/examples/vnext_multitarget_forecasting.R`

### Cross-Target Feature Forecasting

Cross-target feature forecasting asks whether prior information from one target
helps forecast another target. AutoQuant treats this as an empirical
hypothesis, not a default assumption. Rodeo owns deterministic cross-target
lag, rolling, calendar, and known-future feature preparation. AutoQuant fits
CatBoost direct horizon models and compares independent, shared-workflow, and
cross-target strategies using realized forecast evidence.

```r
cross_target_spec <- aq_multitarget_forecast_spec(
  targets = c("leads", "applications", "enrollments"),
  date = "date",
  frequency = "week",
  horizon = 2,
  forecast_origin = max(multitarget_data$date) - 14,
  known_future_variables = "promo",
  shared_predictors = "spend",
  target_specific_predictors = list(enrollments = "promo"),
  strategy = "cross_target_features",
  engine = "catboost",
  cross_target_feature_policy = "lags_rolls",
  shared_target_lags = c(1, 2),
  shared_rolling_windows = 3,
  target_relationship_metadata = list(source = "analyst_hypothesis", causal_claim = FALSE),
  engine_parameters = list(iterations = 5, depth = 2, learning_rate = 0.1)
)

validation <- aq_validate_multitarget_forecast_spec(cross_target_spec, multitarget_data)
forecast <- aq_fit_multitarget_forecast(cross_target_spec, multitarget_data)
assessment <- aq_assess_multitarget_forecast(forecast)
comparison <- aq_compare_multitarget_strategies(cross_target_spec, multitarget_data)
```

Validated script: `inst/examples/vnext_cross_target_forecasting.R`

### Forecasting Capability Planning

Use `aq_discover_forecasting_capabilities()` and
`aq_plan_forecasting_strategy()` before running broad forecasting experiments.
The planner inspects schema, targets, date/entity structure, hierarchy, funnel
shape, zero inflation, known-future variables, supported operators, missing
evidence, and historical CARMA feature-tuning mechanisms. It returns a
canonical `forecast_planning_artifact`; it does not execute models.

```r
discovery <- aq_discover_forecasting_capabilities(
  multitarget_data,
  target = c("leads", "applications", "enrollments"),
  date = "date",
  hierarchy = "region",
  known_future_variables = "promo",
  horizon = 3
)

plan <- aq_plan_forecasting_strategy(discovery)

plan$recommendations
plan$required_baselines
plan$experiment_set
plan$artifact$metadata$carma_mechanism_inventory
```

Validated script: `inst/examples/vnext_forecasting_planning.R`

### Forecasting Experiment Campaigns

Use `aq_forecast_experiment_spec()` after forecasting planning to run one
bounded, evidence-guided experiment. The experiment preserves a frozen
baseline, executes one deterministic challenger, assesses out-of-sample
evidence, and returns a canonical `forecast_experiment_artifact`. Failed or
unsuccessful challengers are preserved as negative evidence. No challenger is
adopted automatically.

```r
experiment <- aq_forecast_experiment_spec(
  planning_result = plan,
  experiment_type = "model",
  baseline = "naive",
  challenger = "ets",
  target = "demand",
  date = "date",
  horizon = 3,
  hypothesis = "ETS may reduce forecast error relative to a frozen naive baseline."
)

result <- aq_run_forecast_experiment(experiment, history)

result$learning
result$artifact

campaign <- aq_run_forecast_experiment_campaign(list(experiment), history)
campaign$summary
```

Validated script: `inst/examples/vnext_forecasting_experiment_campaigns.R`

### Package QA

Run the installed package QA entry point after installation or before integrating with AnalyticsShinyApp:

```r
qa <- AutoQuant::qa_autoquant_package()
qa[, .N, by = status]
qa[status != "pass"]

cross_target_qa <- AutoQuant::qa_vnext_multitarget_supervised_forecasting()
planning_qa <- AutoQuant::qa_vnext_forecasting_planning()
experiment_qa <- AutoQuant::qa_vnext_forecasting_experiment_campaigns()
semantics_qa <- AutoQuant::qa_variable_semantics_framework()
business_intent_qa <- AutoQuant::qa_business_intent_framework()
decision_qa <- AutoQuant::qa_decision_management_framework()
decision_lifecycle_qa <- AutoQuant::qa_decision_lifecycle_framework()
```

The vNext QA includes supervised learning, scoring, model bundles, artifact contracts, canonical variable semantics, business intent, decision management, decision lifecycle memory, forecasting, panel forecasting, hierarchy reconciliation, panel strategy comparison, intermittent-demand operators, funnel forecasting, multi-target forecasting, cross-target feature forecasting, forecasting capability planning, governed forecasting experiment campaigns, and README/example coverage.

### Legacy API Status

The legacy AutoQuant functions remain available for compatibility. vNext is the preferred path for new artifact-first work because it separates specifications, fitting, scoring, assessment, artifacts, and downstream evidence consumption. H2O remains historical/compatibility-oriented for vNext planning rather than the default redesign engine.

### Detailed Architecture Documents

- `docs/autoquant_vnext_archaeology_and_design.md`
- `docs/vnext_catboost_regression.md`
- `docs/canonical_analytical_artifacts.md`
- `docs/variable_semantics_framework.md`
- `docs/business_intent_framework.md`
- `docs/decision_management_philosophy.md`
- `docs/decision_management_framework.md`
- `docs/decision_lifecycle_and_memory.md`
- `docs/vnext_forecasting_foundation.md`
- `docs/vnext_forecasting_planning.md`
- `docs/vnext_forecasting_experiment_campaigns.md`
- `docs/vnext_panel_forecasting_foundation.md`
- `docs/vnext_hierarchical_forecasting_foundation.md`
- `docs/vnext_panel_strategy_selection.md`
- `docs/vnext_intermittent_demand_forecasting.md`
- `docs/vnext_funnel_forecasting_foundation.md`
- `docs/vnext_multitarget_forecasting_foundation.md`
- `docs/vnext_cross_target_forecasting.md`

</details>

<br>

## Background

<details><summary>Expand to view content</summary>
<p>

> Automated Machine Learning - In my view, AutoML should consist of functions to help make professional model development and operationalization more efficient. The functions in this package are there to help no matter which part of the ML lifecycle you are working on. The functions in this package have been tested across a variety of industries and have consistently outperformed competing methods. 

### Package Details
> Supervised Learning - Currently, I'm utilizing CatBoost, LightGBM, XGBoost, and H2O for all of the automated Machine Learning related functions. GPU's can be utilized with CatBoost, LightGBM, and XGBoost, while those and the H2O models can all utilize 100% of CPU. Multi-armed bandit grid tuning is available for CatBoost, LightGBM, and XGBoost models, which utilize the concept of randomized probability matching, which is detailed in the R pacakge "bandit". My choice of included ML algorithms in the package is based on previous success when compared against other algorithms on real world use cases, the additional utilities these packages offer aside from accurate predictions, their ability to work on big data, and the fact that they're available in both R and Python which makes managing multiple languages a little more seamless in a professional setting.

> Documentation - Each exported function in the package has a help file and can be viewed in your RStudio session, e.g. <code>?Rodeo::ModelDataPrep</code>. Many of them come with examples coded up in the help files (at the bottom) that you can run to get a feel for how to set the parameters. There's also a listing of exported functions by category with code examples at the bottom of this readme. You can also jump into the R folder here to dig into the source code. 

> Overall process: Typically, I go to the warehouse to get all of my base features and then I run through all the relevant feature engineering functions in this package. Personally, I set up templates for features engineering, model training optimization, and model scoring (including feature engineering for scoring). I collect all relevant metdata in a list that is shared across templates and as a result, I never have to touch the model scoring template, which makes operationalize and maintenace a breeze. I can simply list out the columns of interest, which feature engineering functions I want to utilize, and then I simply kick off some command line scripts and everything else is automatically managed.

</p>
</details>

<br>

## Installation

The Description File is designed to require only the minimum number of packages to install AutoQuant. However, in order to utilize most of the functions in the package, you'll have to install additional libraries. I set it up this way on purpose. You don't need to install every single possible dependency if you are only interested in using a few of the functions. For example, if you only want to use CatBoost then install the catboost package and forget about the h2o, xgboost, and lightgbm packages. This is one of the primary benefits of not hosting an R package on cran, as they require dependencies to be part of the Imports section on the Description File, which subsequently requires users to have all dependencies installed in order to install the package.

The minimal set of packages that need to be installed are below. The full list can be found by expanding the section (Expand to view content).
* bit64
* data.table
* doParallel
* foreach
* lubridate
* timeDate

```r

# Core pacakges
if(!("data.table" %in% rownames(installed.packages()))) install.packages("data.table"); print("data.table")
if(!("collapse" %in% rownames(installed.packages()))) install.packages("collapse"); print("collapse")
if(!("bit64" %in% rownames(installed.packages()))) install.packages("bit64"); print("bit64")
if(!("devtools" %in% rownames(installed.packages()))) install.packages("devtools"); print("devtools")
if(!("doParallel" %in% rownames(installed.packages()))) install.packages("doParallel"); print("doParallel")
if(!("foreach" %in% rownames(installed.packages()))) install.packages("foreach"); print("foreach")
if(!("lubridate" %in% rownames(installed.packages()))) install.packages("lubridate"); print("lubridate")
if(!("timeDate" %in% rownames(installed.packages()))) install.packages("timeDate"); print("timeDate")

# AutoQuant
devtools::install_github('AdrianAntico/AutoQuant', upgrade = FALSE, dependencies = FALSE, force = TRUE)
```

<details><summary>Additional Packages to Install</summary>
<p>

#### Install ALL R package dependencies for all functions: 
XGBoost and LightGBM can be used with GPU. However, their installation is much more involved than CatBoost, which comes with GPU capabilities simply by installing their package. The installation instructions for them below is for the CPU version only. Refer to each's home page for instructions for installing for GPU. 
 
```r
# Install Dependencies----
if(!("devtools" %in% rownames(installed.packages()))) install.packages("devtools"); print("devtools")

# Core pacakges
if(!("data.table" %in% rownames(installed.packages()))) install.packages("data.table"); print("data.table")
if(!("collapse" %in% rownames(installed.packages()))) install.packages("collapse"); print("collapse")
if(!("bit64" %in% rownames(installed.packages()))) install.packages("bit64"); print("bit64")
if(!("devtools" %in% rownames(installed.packages()))) install.packages("devtools"); print("devtools")
if(!("doParallel" %in% rownames(installed.packages()))) install.packages("doParallel"); print("doParallel")
if(!("foreach" %in% rownames(installed.packages()))) install.packages("foreach"); print("foreach")
if(!("lubridate" %in% rownames(installed.packages()))) install.packages("lubridate"); print("lubridate")
if(!("timeDate" %in% rownames(installed.packages()))) install.packages("timeDate"); print("timeDate")

# Additional dependencies for specific use cases
if(!("combinat" %in% rownames(installed.packages()))) install.packages("combinat"); print("combinat")
if(!("DBI" %in% rownames(installed.packages()))) install.packages("DBI"); print("DBI")
if(!("e1071" %in% rownames(installed.packages()))) install.packages("e1071"); print("e1071")
if(!("fBasics" %in% rownames(installed.packages()))) install.packages("fBasics"); print("fBasics")
if(!("forecast" %in% rownames(installed.packages()))) install.packages("forecast"); print("forecast")
if(!("fpp" %in% rownames(installed.packages()))) install.packages("fpp"); print("fpp")
if(!("ggplot2" %in% rownames(installed.packages()))) install.packages("ggplot2"); print("ggplot2")
if(!("gridExtra" %in% rownames(installed.packages()))) install.packages("gridExtra"); print("gridExtra")
if(!("itertools" %in% rownames(installed.packages()))) install.packages("itertools"); print("itertools")
if(!("MLmetrics" %in% rownames(installed.packages()))) install.packages("MLmetrics"); print("MLmetrics")
if(!("nortest" %in% rownames(installed.packages()))) install.packages("nortest"); print("nortest")
if(!("pROC" %in% rownames(installed.packages()))) install.packages("pROC"); print("pROC")
if(!("RColorBrewer" %in% rownames(installed.packages()))) install.packages("RColorBrewer"); print("RColorBrewer")
if(!("recommenderlab" %in% rownames(installed.packages()))) install.packages("recommenderlab"); print("recommenderlab")
if(!("RPostgres" %in% rownames(installed.packages()))) install.packages("RPostgres"); print("RPostgres")
if(!("Rfast" %in% rownames(installed.packages()))) install.packages("Rfast"); print("Rfast")
if(!("scatterplot3d" %in% rownames(installed.packages()))) install.packages("scatterplot3d"); print("scatterplot3d")
if(!("stringr" %in% rownames(installed.packages()))) install.packages("stringr"); print("stringr")
if(!("tsoutliers" %in% rownames(installed.packages()))) install.packages("tsoutliers"); print("tsoutliers")
if(!("xgboost" %in% rownames(installed.packages()))) install.packages("xgboost"); print("xgboost")
if(!("lightgbm" %in% rownames(installed.packages()))) install.packages("lightgbm"); print("lightgbm")
if(!("regmedint" %in% rownames(installed.packages()))) install.packages("regmedint"); print("regmedint")
for(pkg in c("RCurl","jsonlite")) if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
install.packages("h2o", type = "source", repos = (c("http://h2o-release.s3.amazonaws.com/h2o/latest_stable_R")))
devtools::install_github('catboost/catboost', subdir = 'catboost/R-package')

# Dependencies for ML Reports
if(!("reactable" %in% rownames(installed.packages()))) install.packages("reactable"); print("reactable")
devtools::install_github('AdrianAntico/prettydoc', upgrade = FALSE, dependencies = FALSE, force = TRUE)

# And lastly, AutoQuant
devtools::install_github('AdrianAntico/AutoQuant', upgrade = FALSE, dependencies = FALSE, force = TRUE)
```

Reactable-backed report tables support text exclusion filters. In a text column filter, use:

- `Impressions` to keep rows containing `Impressions`
- `!Impressions` to exclude rows containing `Impressions`
- `-Impressions` to exclude rows containing `Impressions`

Text matching is case-insensitive. Numeric and date columns keep their standard reactable filtering behavior.

#### Installation Troubleshooting 
The most common issue some users are having when trying to install <code>AutoQuant</code> is the installation of the <code>catboost</code> package dependency. Since <code>catboost</code> is not on CRAN it can only be installed through GitHub. To install <code>catboost</code> without error (and consequently install <code>AutoQuant</code> without error), try running this line of code first, then restart your R session, then re-run the 2-step installation process above. (<a href="https://github.com/catboost/catboost/issues/612" target="_blank">Reference</a>):
If you're still having trouble submit an issue and I'll work with you to get it installed.

```r
# Method for on premise servers
options(devtools.install.args = c("--no-multiarch", "--no-test-load"))
install.packages("https://github.com/catboost/catboost/releases/download/<version>/catboost-R-Windows-<version>.tgz", repos = NULL, type = "source", INSTALL_opts = c("--no-multiarch", "--no-test-load"))

# Method for azure machine learning Designer pipelines

## catboost
install.packages("https://github.com/catboost/catboost/releases/download/<version>/catboost-R-Windows-<version>.tgz", repos = NULL, type = "source", INSTALL_opts = c("--no-multiarch", "--no-test-load"))

## AutoQuant
install.packages("https://github.com/AdrianAntico/AutoQuant/archive/refs/tags/<version>.tar.gz", repos = NULL, type = "source", INSTALL_opts = c("--no-multiarch", "--no-test-load"))
```
 

</p>
</details>

<br>

# Code Examples

## EDA <img src="https://raw.githubusercontent.com/AdrianAntico/AutoQuant/master/Images/EDA.png" align="right" width="80" />

<details><summary>Expand to view content</summary>
<p>

<br>

```r
# ============================================================
# Fake EDA Report QA Data for AutoQuant::EDAReport()
# ============================================================

set.seed(8675309)

library(data.table)
library(AutoQuant)

# ----------------------------
# 1. Create fake data
# ----------------------------

n <- 5000

dates <- seq.Date(
  from = as.Date("2023-01-01"),
  to   = as.Date("2025-12-31"),
  by   = "day"
)

dt <- data.table(
  id = seq_len(n),

  # Date / trend fields
  event_date = sample(dates, n, replace = TRUE),

  # Grouping fields
  channel = sample(
    c("Search", "Social", "Email", "Direct", "Affiliate"),
    n,
    replace = TRUE,
    prob = c(0.34, 0.24, 0.16, 0.18, 0.08)
  ),

  region = sample(
    c("West", "South", "Midwest", "Northeast"),
    n,
    replace = TRUE,
    prob = c(0.35, 0.28, 0.20, 0.17)
  ),

  customer_segment = sample(
    c("Budget", "Standard", "Premium", "Enterprise"),
    n,
    replace = TRUE,
    prob = c(0.35, 0.40, 0.20, 0.05)
  )
)

# Date features for synthetic signal
dt[, month_num := as.integer(format(event_date, "%m"))]
dt[, year_num  := as.integer(format(event_date, "%Y"))]
dt[, day_index := as.integer(event_date - min(event_date))]

# Base effects
channel_effect <- c(
  Search    = 1.20,
  Social    = 0.85,
  Email     = 1.35,
  Direct    = 1.00,
  Affiliate = 0.65
)

segment_effect <- c(
  Budget     = 0.70,
  Standard   = 1.00,
  Premium    = 1.55,
  Enterprise = 2.40
)

region_effect <- c(
  West      = 1.10,
  South     = 0.95,
  Midwest   = 0.85,
  Northeast = 1.05
)

# Seasonality and trend
dt[, seasonality := 1 + 0.25 * sin(2 * pi * month_num / 12)]
dt[, trend       := 1 + day_index / max(day_index) * 0.35]

# Numeric variables with structure
dt[, impressions := round(
  rgamma(.N, shape = 8, scale = 1200) *
    channel_effect[channel] *
    seasonality *
    trend
)]

dt[, clicks := round(
  impressions * pmin(
    pmax(rnorm(.N, mean = 0.035, sd = 0.012), 0.002),
    0.12
  )
)]

dt[, spend := round(
  impressions * runif(.N, 0.008, 0.025) *
    channel_effect[channel] *
    region_effect[region],
  2
)]

dt[, conversions := rpois(
  .N,
  lambda = pmax(
    clicks *
      runif(.N, 0.025, 0.12) *
      segment_effect[customer_segment] *
      region_effect[region],
    0.01
  )
)]

dt[, revenue := round(
  conversions *
    rgamma(.N, shape = 4, scale = 90) *
    segment_effect[customer_segment] *
    runif(.N, 0.75, 1.35),
  2
)]

dt[, ctr := clicks / pmax(impressions, 1)]
dt[, cvr := conversions / pmax(clicks, 1)]
dt[, cpc := spend / pmax(clicks, 1)]
dt[, roas := revenue / pmax(spend, 1)]

# Some less-behaved variables for QA
dt[, noise_normal := rnorm(.N, mean = 100, sd = 15)]
dt[, skewed_score := rgamma(.N, shape = 2, scale = 20)]
dt[, binary_flag := rbinom(.N, size = 1, prob = 0.28)]

# Deliberate correlated variables
dt[, spend_lag_proxy := spend * runif(.N, 0.85, 1.15) + rnorm(.N, 0, 25)]
dt[, revenue_proxy   := revenue * runif(.N, 0.90, 1.10) + rnorm(.N, 0, 100)]

# Deliberate outliers
outlier_rows <- sample(seq_len(n), size = 35)
dt[outlier_rows, spend := spend * runif(.N, 4, 9)]
dt[outlier_rows, revenue := revenue * runif(.N, 3, 7)]
dt[outlier_rows, impressions := round(impressions * runif(.N, 2, 5))]

# Deliberate missingness
dt[sample(.N, 150), revenue := NA_real_]
dt[sample(.N, 120), cpc := NA_real_]
dt[sample(.N, 90), customer_segment := NA_character_]
dt[sample(.N, 80), ctr := NA_real_]

# Deliberate zero-heavy variable
dt[, zero_heavy_metric := fifelse(
  runif(.N) < 0.72,
  0,
  round(rgamma(.N, shape = 2, scale = 15), 2)
)]

# Clean helper columns if you do not want them in report
# Keeping them can also help QA univariate behavior.
# dt[, c("month_num", "year_num", "day_index", "seasonality", "trend") := NULL]


# ----------------------------
# 2. Define report inputs
# ----------------------------

UnivariateVars <- c(
  "impressions",
  "clicks",
  "spend",
  "conversions",
  "revenue",
  "ctr",
  "cvr",
  "cpc",
  "roas",
  "noise_normal",
  "skewed_score",
  "binary_flag",
  "zero_heavy_metric",
  "channel",
  "region",
  "customer_segment"
)

CorrVars <- c(
  "impressions",
  "clicks",
  "spend",
  "spend_lag_proxy",
  "conversions",
  "revenue",
  "revenue_proxy",
  "ctr",
  "cvr",
  "cpc",
  "roas",
  "noise_normal",
  "skewed_score",
  "zero_heavy_metric"
)

TrendVars <- c(
  "impressions",
  "clicks",
  "spend",
  "conversions",
  "revenue",
  "ctr",
  "cvr",
  "cpc",
  "roas"
)

OutputPath <- getwd()


# ----------------------------
# 3. Full report: all options
# ----------------------------

eda_artifacts <- generate_eda_artifacts(
  data = dt,
  DataName = "EDA Data",
  UnivariateVars = UnivariateVars,
  CorrVars = CorrVars,
  TrendVars = TrendVars,
  TrendDateVar = "event_date",
  TrendGroupVar = "channel",
  Theme = "dark",
  OutputPath = "eda_artifacts",
  ExportPNG = FALSE
)

# Generate Report
EDAReport(
  artifacts = eda_artifacts,
  DataName = "AutoQuant Fake Marketing QA Dataset",
  OutputPath = OutputPath,
  Theme = "dark"
)
```

</p>
</details>

<br>

## Model Readiness <img src="https://raw.githubusercontent.com/AdrianAntico/AutoQuant/master/Images/Model Readiness Logo.png" align="right" width="80" />

<details><summary>Expand to view content</summary>
<p>

<br>

```r
# ============================================================
# Fake EDA Report QA Data for AutoQuant::EDAReport()
# ============================================================

set.seed(8675309)

library(data.table)
library(AutoQuant)

# ----------------------------
# 1. Create fake data
# ----------------------------

n <- 5000

dates <- seq.Date(
  from = as.Date("2023-01-01"),
  to   = as.Date("2025-12-31"),
  by   = "day"
)

dt <- data.table(
  id = seq_len(n),

  # Date / trend fields
  event_date = sample(dates, n, replace = TRUE),

  # Grouping fields
  channel = sample(
    c("Search", "Social", "Email", "Direct", "Affiliate"),
    n,
    replace = TRUE,
    prob = c(0.34, 0.24, 0.16, 0.18, 0.08)
  ),

  region = sample(
    c("West", "South", "Midwest", "Northeast"),
    n,
    replace = TRUE,
    prob = c(0.35, 0.28, 0.20, 0.17)
  ),

  customer_segment = sample(
    c("Budget", "Standard", "Premium", "Enterprise"),
    n,
    replace = TRUE,
    prob = c(0.35, 0.40, 0.20, 0.05)
  )
)

# Date features for synthetic signal
dt[, month_num := as.integer(format(event_date, "%m"))]
dt[, year_num  := as.integer(format(event_date, "%Y"))]
dt[, day_index := as.integer(event_date - min(event_date))]

# Base effects
channel_effect <- c(
  Search    = 1.20,
  Social    = 0.85,
  Email     = 1.35,
  Direct    = 1.00,
  Affiliate = 0.65
)

segment_effect <- c(
  Budget     = 0.70,
  Standard   = 1.00,
  Premium    = 1.55,
  Enterprise = 2.40
)

region_effect <- c(
  West      = 1.10,
  South     = 0.95,
  Midwest   = 0.85,
  Northeast = 1.05
)

# Seasonality and trend
dt[, seasonality := 1 + 0.25 * sin(2 * pi * month_num / 12)]
dt[, trend       := 1 + day_index / max(day_index) * 0.35]

# Numeric variables with structure
dt[, impressions := round(
  rgamma(.N, shape = 8, scale = 1200) *
    channel_effect[channel] *
    seasonality *
    trend
)]

dt[, clicks := round(
  impressions * pmin(
    pmax(rnorm(.N, mean = 0.035, sd = 0.012), 0.002),
    0.12
  )
)]

dt[, spend := round(
  impressions * runif(.N, 0.008, 0.025) *
    channel_effect[channel] *
    region_effect[region],
  2
)]

dt[, conversions := rpois(
  .N,
  lambda = pmax(
    clicks *
      runif(.N, 0.025, 0.12) *
      segment_effect[customer_segment] *
      region_effect[region],
    0.01
  )
)]

dt[, revenue := round(
  conversions *
    rgamma(.N, shape = 4, scale = 90) *
    segment_effect[customer_segment] *
    runif(.N, 0.75, 1.35),
  2
)]

dt[, ctr := clicks / pmax(impressions, 1)]
dt[, cvr := conversions / pmax(clicks, 1)]
dt[, cpc := spend / pmax(clicks, 1)]
dt[, roas := revenue / pmax(spend, 1)]

# Some less-behaved variables for QA
dt[, noise_normal := rnorm(.N, mean = 100, sd = 15)]
dt[, skewed_score := rgamma(.N, shape = 2, scale = 20)]
dt[, binary_flag := rbinom(.N, size = 1, prob = 0.28)]

# Deliberate correlated variables
dt[, spend_lag_proxy := spend * runif(.N, 0.85, 1.15) + rnorm(.N, 0, 25)]
dt[, revenue_proxy   := revenue * runif(.N, 0.90, 1.10) + rnorm(.N, 0, 100)]

# Deliberate outliers
outlier_rows <- sample(seq_len(n), size = 35)
dt[outlier_rows, spend := spend * runif(.N, 4, 9)]
dt[outlier_rows, revenue := revenue * runif(.N, 3, 7)]
dt[outlier_rows, impressions := round(impressions * runif(.N, 2, 5))]

# Deliberate missingness
dt[sample(.N, 150), revenue := NA_real_]
dt[sample(.N, 120), cpc := NA_real_]
dt[sample(.N, 90), customer_segment := NA_character_]
dt[sample(.N, 80), ctr := NA_real_]

# Deliberate zero-heavy variable
dt[, zero_heavy_metric := fifelse(
  runif(.N) < 0.72,
  0,
  round(rgamma(.N, shape = 2, scale = 15), 2)
)]

# Clean helper columns if you do not want them in report
# Keeping them can also help QA univariate behavior.
# dt[, c("month_num", "year_num", "day_index", "seasonality", "trend") := NULL]


# ----------------------------
# 2. Define report inputs
# ----------------------------

UnivariateVars <- c(
  "impressions",
  "clicks",
  "spend",
  "conversions",
  "revenue",
  "ctr",
  "cvr",
  "cpc",
  "roas",
  "noise_normal",
  "skewed_score",
  "binary_flag",
  "zero_heavy_metric",
  "channel",
  "region",
  "customer_segment"
)

CorrVars <- c(
  "impressions",
  "clicks",
  "spend",
  "spend_lag_proxy",
  "conversions",
  "revenue",
  "revenue_proxy",
  "ctr",
  "cvr",
  "cpc",
  "roas",
  "noise_normal",
  "skewed_score",
  "zero_heavy_metric"
)

TrendVars <- c(
  "impressions",
  "clicks",
  "spend",
  "conversions",
  "revenue",
  "ctr",
  "cvr",
  "cpc",
  "roas"
)

OutputPath <- getwd()


# ----------------------------
# 3. Full report: all options
# ----------------------------

model_readiness_artifacts <- generate_model_assessment_artifacts(
  data = dt,
  DataName = "Modeling Data",
  TargetVar = "conversions",
  TrendDateVar = "event_date",
  TrendGroupVar = c("channel", "market"),
  Theme = "dark",
  OutputPath = "model_readiness_artifacts",
  ExportPNG = FALSE
)

TargetAnalysisReport(
  artifacts = model_readiness_artifacts,
  OutputPath = OutputPath
)

```

</p>
</details>

<br>


## Supervised Learning <img src="https://raw.githubusercontent.com/AdrianAntico/AutoQuant/master/Images/SupervisedLearningImage.png" align="right" width="80" />

<details><summary>Expand to view content</summary>
<p>

<br>

### CatBoost Builder Artifact Generator

`AutoCatBoostRegression()` and `AutoCatBoostClassifier()` remain the core AutoQuant CatBoost training functions. `generate_catboost_builder_artifacts()` is the artifact-first wrapper for app/report workflows: it routes training through those existing functions, standardizes scored output with `Predict` and `.split`, and preserves `Shap_` contribution columns when the wrapped AutoCatBoost output includes them. The scored output is designed to feed Model Assessment, Model Insights, and SHAP Analysis; it does not run those downstream modules automatically. AnalyticsShinyApp should call `generate_catboost_builder_artifacts()`, not the lower-level training functions directly.

```r
library(data.table)

set.seed(42)
n <- 300L
dt <- data.table(
  id = seq_len(n),
  event_date = as.Date("2025-01-01") + seq_len(n) - 1L,
  channel = sample(c("Search", "Email", "Social", "Direct"), n, replace = TRUE),
  region = sample(c("West", "Midwest", "South"), n, replace = TRUE),
  spend = runif(n, 50, 500),
  clicks = rpois(n, 40),
  discount = runif(n, 0, 0.3)
)

dt[, revenue := 10 + 2.5 * spend + 1.8 * clicks - 120 * discount +
  fifelse(channel == "Search", 80, 0) +
  fifelse(region == "West", 40, 0) +
  rnorm(.N, 0, 35)]

builder <- AutoQuant::generate_catboost_builder_artifacts(
  data = dt,
  target_col = "revenue",
  feature_cols = c("channel", "region", "spend", "clicks", "discount"),
  problem_type = "regression",
  id_cols = "id",
  DateVar = "event_date",
  ByVars = c("channel", "region"),
  split_method = "time",
  iterations = 30L,
  depth = 4L,
  learning_rate = 0.08,
  compute_shap = TRUE
)

builder$metadata$artifact_index
builder$value$scored_data
builder$value$downstream_handoff
```

### Modern Report API Contract

Modern AutoQuant reports are artifact renderers. Artifact generators own analytical parameters; report functions own rendering parameters.

Preferred generator-first workflow:

```r
reg_artifacts <- AutoQuant::generate_regression_shap_analysis_artifacts(
  data = scored_data,
  target_col = "Target",
  prediction_col = "Predict",
  DateVar = "Date",
  ByVars = "Segment"
)

AutoQuant::RegressionShapAnalysisReport(
  artifact_result = reg_artifacts,
  OutputPath = "reports",
  OutputFile = "regression_shap.html"
)
```

Convenience wrapper workflow:

```r
AutoQuant::RegressionShapAnalysisReport(
  data = scored_data,
  OutputPath = "reports",
  OutputFile = "regression_shap.html",
  target_col = "Target",
  prediction_col = "Predict",
  DateVar = "Date",
  ByVars = "Segment"
)
```

The same pattern applies to `EDAReport()`, `TargetAnalysisReport()`, `RegressionModelInsightsReport()`, `BinaryClassificationModelInsightsReport()`, `RegressionShapAnalysisReport()`, and `BinaryClassificationShapAnalysisReport()`. `ModelInsightsReport()` is legacy compatibility only.

See `docs/report_api_contract.md`.

### Optional AutoNLS SHAP effect curves

SHAP generators can optionally use AutoNLS vNext as an internal curve-fitting backend for numeric dependence data:

```r
reg_artifacts <- AutoQuant::generate_regression_shap_analysis_artifacts(
  data = scored_data,
  target_col = "Target",
  prediction_col = "Predict",
  effect_curve_backend = "autonls",
  effect_curve_models = "stable",
  effect_curve_sample_size = 50000,
  effect_curve_max_features = 20,
  effect_curve_validation_fraction = 0.20
)
```

AutoNLS is optional. The default `effect_curve_backend = "none"` keeps existing SHAP workflows unchanged and does not require AutoNLS to be installed.

AutoQuant accepts original-scale feature and SHAP contribution columns. AutoNLS may use internal scaling, log/log1p starts, or family-specific transformed initialization for optimizer stability, but returned curve values, predictions, derivatives, and elasticities remain on the original scale. If fitting is unavailable or unsuitable, AutoQuant returns effect-curve diagnostics and continues generating SHAP artifacts.

### Typed Artifact Schema Framework

Future AutoQuant generators should return reusable typed artifacts that can power reports, dashboards, Shiny apps, APIs, and LLM agents without recomputing analytical work.

The first typed artifact layer supports:

- `table`
- `plot`
- `diagnostic`
- `finding`
- `warning`
- `metadata`
- `computation_graph`
- `display_plan`
- `quality_gate`

Small example:

```r
importance <- data.table::data.table(
  feature = c("Spend", "Clicks"),
  mean_abs_contribution = c(0.42, 0.18)
)

table_artifact <- AutoQuant::new_table_artifact(
  id = "tbl_global_importance",
  title = "Global Importance",
  data = importance,
  source_generator = "example_generator"
)

finding_artifact <- AutoQuant::new_finding_artifact(
  id = "finding_top_driver",
  title = "Top Driver",
  claim = "Spend is the strongest driver in this example result.",
  evidence_ids = "tbl_global_importance",
  confidence = 0.8,
  source_generator = "example_generator"
)

AutoQuant::validate_artifact_collection(list(table_artifact, finding_artifact))
```

See `docs/contracts/artifact_schema_contract.md` and `inst/examples/artifact_schema_example.R`.

### Regression

<details><summary>click to expand</summary>
<p>

<details><summary>Regression Description</summary>
<p>
  
The Auto_Regression() models handle a multitude of tasks. In order:
1. Convert your data to data.table format for faster processing
2. Transform your target variable using the best normalization method based on the <code>AutoTransformationCreate()</code> function
3. Create train, validation, and test data, utilizing the <code>AutoDataPartition()</code> function, if you didn't supply those directly to the function
4. Consoldate columns that are used for modeling and what metadata you want returned in your test data with predictions
5. Dichotomize categorical variables (for <code>AutoXGBoostRegression()</code>) and save the factor levels for scoring in a way that guarentees consistency across training, validation, and test data sets, utilizing the <code>DummifyDT()</code> function
6. Save the final modeling column names for reference
7. Handles the data conversion to the appropriate modeling type, such as CatBoost, H2O, and XGBoost
8. Multi-armed bandit hyperparameter tuning using randomized probability matching, if you choose to grid tune
9. Loop through the grid-tuning process, building N models
10. Collect the evaluation metrics for each grid tune run
11. Identify the best model of the set of models built in the grid tuning search
12. Save the hyperparameters from the winning grid tuned model
13. Build the final model based on the best model from the grid tuning model search (I remove each model after evaluation metrics are generated in the grid tune to avoid memory overflow)
14. Back-transform your predictions based on the best transformation used earlier in the process
15. Collect evaluation metrics based on performance on test data (based on back-transformed data)
16. Store the final predictions with the associated test data and other columns you want included in that set
17. Save your transformation metadata for recreating them in a scoring process
18. Build out and save an Evaluation Calibration Line Plot and Evaluation Calibration Box-Plot, using the <code>EvalPlot()</code> function
19. Generate and save Variable Importance
20. Generate and save Partital Dependence Calibration Line Plots and Partital Dependence Calibration Box-Plots, using the <code>ParDepPlots()</code> function
21. Return all the objects generated in a named list for immediate use and evaluation
 
</p>
</details>

<details><summary>CatBoost Example</summary>
<p>

```r
# Create some dummy correlated data
data <- AutoQuant::FakeDataGenerator(
  Correlation = 0.85,
  N = 10000,
  ID = 2,
  ZIP = 0,
  AddDate = FALSE,
  Classification = FALSE,
  MultiClass = FALSE)

# Run function
TestModel <- AutoQuant::AutoCatBoostRegression(

  # GPU or CPU and the number of available GPUs
  TrainOnFull = FALSE,
  task_type = 'GPU',
  NumGPUs = 1,
  DebugMode = FALSE,

  # Metadata args
  OutputSelection = c('Importances', 'EvalPlots', 'EvalMetrics', 'Score_TrainData'),
  ModelID = 'Test_Model_1',
  model_path = normalizePath('./'),
  metadata_path = normalizePath('./'),
  SaveModelObjects = FALSE,
  SaveInfoToPDF = FALSE,
  ReturnModelObjects = TRUE,

  # Data args
  data = data,
  ValidationData = NULL,
  TestData = NULL,
  TargetColumnName = 'Adrian',
  FeatureColNames = names(data)[!names(data) %in% c('IDcol_1', 'IDcol_2','Adrian')],
  PrimaryDateColumn = NULL,
  WeightsColumnName = NULL,
  IDcols = c('IDcol_1','IDcol_2'),
  TransformNumericColumns = 'Adrian',
  Methods = c('BoxCox', 'Asinh', 'Asin', 'Log',
    'LogPlus1', 'Sqrt', 'Logit'),

  # Model evaluation
  eval_metric = 'RMSE',
  eval_metric_value = 1.5,
  loss_function = 'RMSE',
  loss_function_value = 1.5,
  MetricPeriods = 10L,
  NumOfParDepPlots = ncol(data)-1L-2L,

  # Grid tuning args
  PassInGrid = NULL,
  GridTune = FALSE,
  MaxModelsInGrid = 30L,
  MaxRunsWithoutNewWinner = 20L,
  MaxRunMinutes = 60*60,
  BaselineComparison = 'default',

  # ML args
  langevin = FALSE,
  diffusion_temperature = 10000,
  Trees = 1000,
  Depth = 9,
  L2_Leaf_Reg = NULL,
  RandomStrength = 1,
  BorderCount = 128,
  LearningRate = NULL,
  RSM = 1,
  BootStrapType = NULL,
  GrowPolicy = 'SymmetricTree',
  model_size_reg = 0.5,
  feature_border_type = 'GreedyLogSum',
  sampling_unit = 'Object',
  subsample = NULL,
  score_function = 'Cosine',
  min_data_in_leaf = 1)
  
# Legacy compatibility training report.
# New regression/binary workflows should use the artifact-first report functions.
AutoQuant::ModelInsightsReport(
  TrainDataInclude = TRUE,
  FeatureColumnNames = names(data)[!names(data) %in% c('IDcol_1', 'IDcol_2','Adrian')],
  SampleSize = 1e+05,
  ModelObject = TestModel,
  ModelID = "Test_Model_1",
  OutputPath = getwd()
)
```
 
</p>
</details>

<details><summary>XGBoost Example</summary>
<p>

```r
# Create some dummy correlated data
data <- AutoQuant::FakeDataGenerator(
  Correlation = 0.85,
  N = 1000,
  ID = 2,
  ZIP = 0,
  AddDate = FALSE,
  Classification = FALSE,
  MultiClass = FALSE)

# Run function
TestModel <- AutoQuant::AutoXGBoostRegression(
  
  # GPU or CPU
  TreeMethod = 'hist',
  NThreads = parallel::detectCores(),
  LossFunction = 'reg:squarederror',
  
  # Metadata args
  OutputSelection = c('Importances', 'EvalPlots', 'EvalMetrics', 'Score_TrainData'),
  model_path = normalizePath("./"),
  metadata_path = NULL,
  ModelID = "Test_Model_1",
  EncodingMethod = "binary",
  ReturnFactorLevels = TRUE,
  ReturnModelObjects = TRUE,
  SaveModelObjects = FALSE,
  SaveInfoToPDF = FALSE,
  DebugMode = FALSE,
  
  # Data args
  data = data,
  TrainOnFull = FALSE,
  ValidationData = NULL,
  TestData = NULL,
  TargetColumnName = "Adrian",
  FeatureColNames = names(data)[!names(data) %in% c('IDcol_1','IDcol_2','Adrian')],
  PrimaryDateColumn = NULL,
  WeightsColumnName = NULL,
  IDcols = c('IDcol_1','IDcol_2'),
  TransformNumericColumns = 'Adrian',
  Methods = c('Asinh','Asin','Log','LogPlus1','Sqrt','Logit'),
  
  # Model evaluation args
  eval_metric = 'rmse',
  NumOfParDepPlots = 3L,
  
  # Grid tuning args
  PassInGrid = NULL,
  GridTune = FALSE,
  grid_eval_metric = 'r2',
  BaselineComparison = 'default',
  MaxModelsInGrid = 10L,
  MaxRunsWithoutNewWinner = 20L,
  MaxRunMinutes = 24L*60L,
  Verbose = 1L,
  
  # ML args
  Trees = 50L,
  eta = 0.05,
  max_depth = 4L,
  min_child_weight = 1.0,
  subsample = 0.55,
  colsample_bytree = 0.55)
```

</p>
</details>

<details><summary>LightGBM Example</summary>
<p>

```r
# Create some dummy correlated data
data <- AutoQuant::FakeDataGenerator(
  Correlation = 0.85,
  N = 1000,
  ID = 2,
  ZIP = 0,
  AddDate = FALSE,
  Classification = FALSE,
  MultiClass = FALSE)

# Run function
TestModel <- AutoQuant::AutoLightGBMRegression(

  # Metadata args
  OutputSelection = c('Importances','EvalPlots','EvalMetrics','Score_TrainData'),
  model_path = normalizePath('./'),
  metadata_path = NULL,
  ModelID = 'Test_Model_1',
  NumOfParDepPlots = 3L,
  EncodingMethod = 'credibility',
  ReturnFactorLevels = TRUE,
  ReturnModelObjects = TRUE,
  SaveModelObjects = FALSE,
  SaveInfoToPDF = FALSE,
  DebugMode = FALSE,

  # Data args
  data = data,
  TrainOnFull = FALSE,
  ValidationData = NULL,
  TestData = NULL,
  TargetColumnName = 'Adrian',
  FeatureColNames = names(data)[!names(data) %in% c('IDcol_1', 'IDcol_2','Adrian')],
  PrimaryDateColumn = NULL,
  WeightsColumnName = NULL,
  IDcols = c('IDcol_1','IDcol_2'),
  TransformNumericColumns = NULL,
  Methods = c('Asinh','Asin','Log','LogPlus1','Sqrt','Logit'),

  # Grid parameters
  GridTune = FALSE,
  grid_eval_metric = 'r2',
  BaselineComparison = 'default',
  MaxModelsInGrid = 10L,
  MaxRunsWithoutNewWinner = 20L,
  MaxRunMinutes = 24L*60L,
  PassInGrid = NULL,

  # Core parameters
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#core-parameters
  input_model = NULL, # continue training a model that is stored to file
  task = 'train',
  device_type = 'CPU',
  NThreads = parallel::detectCores() / 2,
  objective = 'regression',
  metric = 'rmse',
  boosting = 'gbdt',
  LinearTree = FALSE,
  Trees = 50L,
  eta = NULL,
  num_leaves = 31,
  deterministic = TRUE,

  # Learning Parameters
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#learning-control-parameters
  force_col_wise = FALSE,
  force_row_wise = FALSE,
  max_depth = NULL,
  min_data_in_leaf = 20,
  min_sum_hessian_in_leaf = 0.001,
  bagging_freq = 0,
  bagging_fraction = 1.0,
  feature_fraction = 1.0,
  feature_fraction_bynode = 1.0,
  extra_trees = FALSE,
  early_stopping_round = 10,
  first_metric_only = TRUE,
  max_delta_step = 0.0,
  lambda_l1 = 0.0,
  lambda_l2 = 0.0,
  linear_lambda = 0.0,
  min_gain_to_split = 0,
  drop_rate_dart = 0.10,
  max_drop_dart = 50,
  skip_drop_dart = 0.50,
  uniform_drop_dart = FALSE,
  top_rate_goss = FALSE,
  other_rate_goss = FALSE,
  monotone_constraints = NULL,
  monotone_constraints_method = 'advanced',
  monotone_penalty = 0.0,
  forcedsplits_filename = NULL, # use for AutoStack option; .json file
  refit_decay_rate = 0.90,
  path_smooth = 0.0,

  # IO Dataset Parameters
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#io-parameters
  max_bin = 255,
  min_data_in_bin = 3,
  data_random_seed = 1,
  is_enable_sparse = TRUE,
  enable_bundle = TRUE,
  use_missing = TRUE,
  zero_as_missing = FALSE,
  two_round = FALSE,

  # Convert Parameters
  convert_model = NULL,
  convert_model_language = 'cpp',

  # Objective Parameters
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#objective-parameters
  boost_from_average = TRUE,
  alpha = 0.90,
  fair_c = 1.0,
  poisson_max_delta_step = 0.70,
  tweedie_variance_power = 1.5,
  lambdarank_truncation_level = 30,

  # Metric Parameters (metric is in Core)
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#metric-parameters
  is_provide_training_metric = TRUE,
  eval_at = c(1,2,3,4,5),

  # Network Parameters
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#network-parameters
  num_machines = 1,

  # GPU Parameters
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#gpu-parameters
  gpu_platform_id = -1,
  gpu_device_id = -1,
  gpu_use_dp = TRUE,
  num_gpu = 1)
```

</p>
</details> 

<details><summary>H2O-GBM Example</summary>
<p>

```r
# Create some dummy correlated data
data <- AutoQuant::FakeDataGenerator(
  Correlation = 0.85,
  N = 1000,
  ID = 2,
  ZIP = 0,
  AddDate = FALSE,
  Classification = FALSE,
  MultiClass = FALSE)

# Run function
TestModel <- AutoQuant::AutoH2oGBMRegression(
  
  # Compute management
  MaxMem = {gc();paste0(as.character(floor(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE)) / 1000000)),"G")},
  NThreads = max(1, parallel::detectCores()-2),
  H2OShutdown = TRUE,
  H2OStartUp = TRUE,
  IfSaveModel = "mojo",
  
  # Model evaluation
  NumOfParDepPlots = 3,
  
  # Metadata arguments:
  model_path = normalizePath("./"),
  metadata_path = file.path(normalizePath("./")),
  ModelID = "FirstModel",
  ReturnModelObjects = TRUE,
  SaveModelObjects = FALSE,
  SaveInfoToPDF = FALSE,
  
  # Data arguments
  data = data,
  TrainOnFull = FALSE,
  ValidationData = NULL,
  TestData = NULL,
  TargetColumnName = 'Adrian',
  FeatureColNames = names(data)[!names(data) %in% c('IDcol_1','IDcol_2','Adrian')],
  WeightsColumn = NULL,
  TransformNumericColumns = NULL,
  Methods = c('Asinh','Asin','Log','LogPlus1','Sqrt','Logit'),
  
  # ML grid tuning args
  GridTune = FALSE,
  GridStrategy = "Cartesian",
  MaxRuntimeSecs = 60*60*24,
  StoppingRounds = 10,
  MaxModelsInGrid = 2,
  
  # Model args
  Trees = 50,
  LearnRate = 0.10,
  LearnRateAnnealing = 1,
  eval_metric = "RMSE",
  Alpha = NULL,
  Distribution = "poisson",
  MaxDepth = 20,
  SampleRate = 0.632,
  ColSampleRate = 1,
  ColSampleRatePerTree = 1,
  ColSampleRatePerTreeLevel  = 1,
  MinRows = 1,
  NBins = 20,
  NBinsCats = 1024,
  NBinsTopLevel = 1024,
  HistogramType = "AUTO",
  CategoricalEncoding = "AUTO")
```
 
</p>
</details>

<details><summary>H2O-DRF Example</summary>
<p>
 
```r
# Create some dummy correlated data
data <- AutoQuant::FakeDataGenerator(
  Correlation = 0.85,
  N = 1000,
  ID = 2,
  ZIP = 0,
  AddDate = FALSE,
  Classification = FALSE,
  MultiClass = FALSE)

# Run function
TestModel <- AutoQuant::AutoH2oDRFRegression(
  
  # Compute management
  MaxMem = {gc();paste0(as.character(floor(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE)) / 1000000)),"G")},
  NThreads = max(1L, parallel::detectCores() - 2L),
  H2OShutdown = TRUE,
  H2OStartUp = TRUE,
  IfSaveModel = "mojo",
  
  # Model evaluation:
  eval_metric = "RMSE",
  NumOfParDepPlots = 3,
  
  # Metadata arguments:
  model_path = normalizePath("./"),
  metadata_path = NULL,
  ModelID = "FirstModel",
  ReturnModelObjects = TRUE,
  SaveModelObjects = FALSE,
  SaveInfoToPDF = FALSE,
  
  # Data Args
  data = data,
  TrainOnFull = FALSE,
  ValidationData = NULL,
  TestData = NULL,
  TargetColumnName = "Adrian",
  FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
  WeightsColumn = NULL,
  TransformNumericColumns = NULL,
  Methods = c("Asinh", "Asin", "Log", "LogPlus1", "Sqrt", "Logit"),
  
  # Grid Tuning Args
  GridStrategy = "Cartesian",
  GridTune = FALSE,
  MaxModelsInGrid = 10,
  MaxRuntimeSecs = 60*60*24,
  StoppingRounds = 10,
  
  # ML Args
  Trees = 50,
  MaxDepth = 20,
  SampleRate = 0.632,
  MTries = -1,
  ColSampleRatePerTree = 1,
  ColSampleRatePerTreeLevel = 1,
  MinRows = 1,
  NBins = 20,
  NBinsCats = 1024,
  NBinsTopLevel = 1024,
  HistogramType = "AUTO",
  CategoricalEncoding = "AUTO")
```
 
</p>
</details>

<details><summary>H2O-GLM Example</summary>
<p>
 
```r
# Create some dummy correlated data
data <- AutoQuant::FakeDataGenerator(
  Correlation = 0.85,
  N = 1000,
  ID = 2,
  ZIP = 0,
  AddDate = FALSE,
  Classification = FALSE,
  MultiClass = FALSE)

# Run function
TestModel <- AutoQuant::AutoH2oGLMRegression(
  
  # Compute management
  MaxMem = {gc();paste0(as.character(floor(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE)) / 1000000)),"G")},
  NThreads = max(1, parallel::detectCores()-2),
  H2OShutdown = TRUE,
  H2OStartUp = TRUE,
  IfSaveModel = "mojo",
  
  # Model evaluation:
  eval_metric = "RMSE",
  NumOfParDepPlots = 3,
  
  # Metadata arguments:
  model_path = NULL,
  metadata_path = NULL,
  ModelID = "FirstModel",
  ReturnModelObjects = TRUE,
  SaveModelObjects = FALSE,
  SaveInfoToPDF = FALSE,
  
  # Data arguments:
  data = data,
  TrainOnFull = FALSE,
  ValidationData = NULL,
  TestData = NULL,
  TargetColumnName = "Adrian",
  FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
  RandomColNumbers = NULL,
  InteractionColNumbers = NULL,
  WeightsColumn = NULL,
  TransformNumericColumns = NULL,
  Methods = c("Asinh", "Asin", "Log", "LogPlus1", "Sqrt", "Logit"),
  
  # Model args
  GridTune = FALSE,
  GridStrategy = "Cartesian",
  StoppingRounds = 10,
  MaxRunTimeSecs = 3600 * 24 * 7,
  MaxModelsInGrid = 10,
  Distribution = "gaussian",
  Link = "identity",
  TweedieLinkPower = NULL,
  TweedieVariancePower = NULL,
  RandomDistribution = NULL,
  RandomLink = NULL,
  Solver = "AUTO",
  Alpha = NULL,
  Lambda = NULL,
  LambdaSearch = FALSE,
  NLambdas = -1,
  Standardize = TRUE,
  RemoveCollinearColumns = FALSE,
  InterceptInclude = TRUE,
  NonNegativeCoefficients = FALSE)
```
 
</p>
</details>

<details><summary>H2O-AutoML Example</summary>
<p>
 
```r
# Create some dummy correlated data with numeric and categorical features
data <- AutoQuant::FakeDataGenerator(
  Correlation = 0.85,
  N = 1000,
  ID = 2,
  ZIP = 0,
  AddDate = FALSE,
  Classification = FALSE,
  MultiClass = FALSE)

# Run function
TestModel <- AutoQuant::AutoH2oMLRegression(

  # Compute management
  MaxMem = "32G",
  NThreads = max(1, parallel::detectCores()-2),
  H2OShutdown = TRUE,
  IfSaveModel = "mojo",

  # Model evaluation
  eval_metric = "RMSE",
  NumOfParDepPlots = 3,

  # Metadata arguments
  model_path = NULL,
  metadata_path = NULL,
  ModelID = "FirstModel",
  ReturnModelObjects = TRUE,
  SaveModelObjects = FALSE,

  # Data arguments
  TrainOnFull = FALSE,
  ValidationData = NULL,
  TestData = NULL,
  TargetColumnName = "Adrian",
  FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
  TransformNumericColumns = NULL,
  Methods = c("Asinh", "Asin", "Log", "LogPlus1", "Logit"),

  # Model args
  GridTune = FALSE,
  ExcludeAlgos = NULL,
  Trees = 50,
  MaxModelsInGrid = 10)
```

</p>
</details> 

<details><summary>H2O-GAM Example</summary>
<p>
 
```r
# Create some dummy correlated data
data <- AutoQuant::FakeDataGenerator(
  Correlation = 0.85,
  N = 1000,
  ID = 2,
  ZIP = 0,
  AddDate = FALSE,
  Classification = FALSE,
  MultiClass = FALSE)

# Define GAM Columns to use - up to 9 are allowed
GamCols <- names(which(unlist(lapply(data, is.numeric))))
GamCols <- GamCols[!GamCols %in% c("Adrian","IDcol_1","IDcol_2")]
GamCols <- GamCols[1L:(min(9L,length(GamCols)))]

# Run function
TestModel <- AutoQuant::AutoH2oGAMRegression(
  
  # Compute management
  MaxMem = {gc();paste0(as.character(floor(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE)) / 1000000)),"G")},
  NThreads = max(1, parallel::detectCores()-2),
  H2OShutdown = TRUE,
  H2OStartUp = TRUE,
  IfSaveModel = "mojo",
  
  # Model evaluation:
  eval_metric = "RMSE",
  NumOfParDepPlots = 3,
  
  # Metadata arguments:
  model_path = NULL,
  metadata_path = NULL,
  ModelID = "FirstModel",
  ReturnModelObjects = TRUE,
  SaveModelObjects = FALSE,
  SaveInfoToPDF = FALSE,
  
  # Data arguments:
  data = data,
  TrainOnFull = FALSE,
  ValidationData = NULL,
  TestData = NULL,
  TargetColumnName = "Adrian",
  FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
  InteractionColNumbers = NULL,
  WeightsColumn = NULL,
  GamColNames = GamCols,
  TransformNumericColumns = NULL,
  Methods = c("Asinh", "Asin", "Log", "LogPlus1", "Sqrt", "Logit"),
  
  # Model args
  num_knots = NULL,
  keep_gam_cols = TRUE,
  GridTune = FALSE,
  GridStrategy = "Cartesian",
  StoppingRounds = 10,
  MaxRunTimeSecs = 3600 * 24 * 7,
  MaxModelsInGrid = 10,
  Distribution = "gaussian",
  Link = "Family_Default",
  TweedieLinkPower = NULL,
  TweedieVariancePower = NULL,
  Solver = "AUTO",
  Alpha = NULL,
  Lambda = NULL,
  LambdaSearch = FALSE,
  NLambdas = -1,
  Standardize = TRUE,
  RemoveCollinearColumns = FALSE,
  InterceptInclude = TRUE,
  NonNegativeCoefficients = FALSE)
```
 
</p>
</details> 

</p>
</details>

<br>

### Binary Classification

<details><summary>click to expand</summary>
<p>

<details><summary>Classification Description</summary>
<p>
  
The Auto_Classifier() models handle a multitude of tasks. In order:
1. Convert your data to data.table format for faster processing
2. Create train, validation, and test data if you didn't supply those directly to the function
3. Consoldate columns that are used for modeling and what is to be kept for data returned
4. Dichotomize categorical variables (for AutoXGBoostRegression) and save the factor levels for scoring in a way that guarentees consistency across training, validation, and test data sets
5. Saves the final column names for modeling to a csv for later reference
6. Handles the data conversion to the appropriate type, based on model type (CatBoost, H2O, and XGBoost)
7. Multi-armed bandit hyperparameter tuning using randomized probability matching, if you choose to grid tune
8. Build the grid tuned models
9. Collect the evaluation metrics for each grid tune run
10. Identify the best model of the set of models built in the grid tuning setup
11. Save the hyperparameters from the winning grid tuned model
12. Build the final model based on the best model from the grid tuning model search
13. Collect evaluation metrics based on performance on test data
14. Store the final predictions with the associated test data and other columns you want included in that set
15. Build out and save an Evaluation Calibration Line Plot
16. Build out and save an ROC plot with the top 5 models used in grid-tuning (includes the winning model)
17. Generate and save Variable Importance data
18. Generate and save Partital Dependence Calibration Line Plots
19. Return all the objects generated in a named list for immediate use

</p>
</details>

<details><summary>CatBoost Example</summary>
<p>

```r
# Create some dummy correlated data
data <- AutoQuant::FakeDataGenerator(
  Correlation = 0.85,
  N = 10000,
  ID = 2,
  ZIP = 0,
  AddDate = FALSE,
  Classification = TRUE,
  MultiClass = FALSE)

# Run function
TestModel <- AutoQuant::AutoCatBoostClassifier(

  # GPU or CPU and the number of available GPUs
  task_type = 'GPU',
  NumGPUs = 1,
  TrainOnFull = FALSE,
  DebugMode = FALSE,

  # Metadata args
  OutputSelection = c('Score_TrainData', 'Importance', 'EvalPlots', 'Metrics', 'PDF'),
  ModelID = 'Test_Model_1',
  model_path = normalizePath('./'),
  metadata_path = normalizePath('./'),
  SaveModelObjects = FALSE,
  ReturnModelObjects = TRUE,
  SaveInfoToPDF = FALSE,

  # Data args
  data = data,
  ValidationData = NULL,
  TestData = NULL,
  TargetColumnName = 'Adrian',
  FeatureColNames = names(data)[!names(data) %in% c('IDcol_1','IDcol_2','Adrian')],
  PrimaryDateColumn = NULL,
  WeightsColumnName = NULL,
  IDcols = c('IDcol_1','IDcol_2'),

  # Evaluation args
  ClassWeights = c(1L,1L),
  CostMatrixWeights = c(1,0,0,1),
  EvalMetric = 'AUC',
  grid_eval_metric = 'MCC',
  LossFunction = 'Logloss',
  MetricPeriods = 10L,
  NumOfParDepPlots = ncol(data)-1L-2L,

  # Grid tuning args
  PassInGrid = NULL,
  GridTune = FALSE,
  MaxModelsInGrid = 30L,
  MaxRunsWithoutNewWinner = 20L,
  MaxRunMinutes = 24L*60L,
  BaselineComparison = 'default',

  # ML args
  Trees = 1000,
  Depth = 9,
  LearningRate = NULL,
  L2_Leaf_Reg = NULL,
  model_size_reg = 0.5,
  langevin = FALSE,
  diffusion_temperature = 10000,
  RandomStrength = 1,
  BorderCount = 128,
  RSM = 1,
  BootStrapType = 'Bayesian',
  GrowPolicy = 'SymmetricTree',
  feature_border_type = 'GreedyLogSum',
  sampling_unit = 'Object',
  subsample = NULL,
  score_function = 'Cosine',
  min_data_in_leaf = 1)

# Legacy compatibility training report.
# New regression/binary workflows should use the artifact-first report functions.
AutoQuant::ModelInsightsReport(
  TrainDataInclude = TRUE,
  FeatureColumnNames = names(data)[!names(data) %in% c('IDcol_1', 'IDcol_2','Adrian')],
  SampleSize = 1e+05,
  ModelObject = TestModel,
  ModelID = "Test_Model_1",
  OutputPath = getwd()
)
```

</p>
</details>

<details><summary>XGBoost Example</summary>
<p>
 
```r
# Create some dummy correlated data
data <- AutoQuant::FakeDataGenerator(
  Correlation = 0.85,
  N = 1000L,
  ID = 2L,
  ZIP = 0L,
  AddDate = FALSE,
  Classification = TRUE,
  MultiClass = FALSE)

# Run function
TestModel <- AutoQuant::AutoXGBoostClassifier(

  # GPU or CPU
  TreeMethod = "hist",
  NThreads = parallel::detectCores(),

  # Metadata args
  OutputSelection = c("Importances", "EvalPlots", "EvalMetrics", "PDFs", "Score_TrainData"),
  model_path = normalizePath("./"),
  metadata_path = NULL,
  ModelID = "Test_Model_1",
  EncodingMethod = "binary",
  ReturnFactorLevels = TRUE,
  ReturnModelObjects = TRUE,
  SaveModelObjects = FALSE,
  SaveInfoToPDF = FALSE,

  # Data args
  data = data,
  TrainOnFull = FALSE,
  ValidationData = NULL,
  TestData = NULL,
  TargetColumnName = "Adrian",
  FeatureColNames = names(data)[!names(data) %in%
    c("IDcol_1", "IDcol_2","Adrian")],
  WeightsColumnName = NULL,
  IDcols = c("IDcol_1","IDcol_2"),

  # Model evaluation
  LossFunction = 'reg:logistic',
  CostMatrixWeights = c(1,0,0,1),
  eval_metric = "auc",
  grid_eval_metric = "MCC",
  NumOfParDepPlots = 3L,

  # Grid tuning args
  PassInGrid = NULL,
  GridTune = FALSE,
  BaselineComparison = "default",
  MaxModelsInGrid = 10L,
  MaxRunsWithoutNewWinner = 20L,
  MaxRunMinutes = 24L*60L,
  Verbose = 1L,

  # ML args
  Trees = 500L,
  eta = 0.30,
  max_depth = 9L,
  min_child_weight = 1.0,
  subsample = 1,
  colsample_bytree = 1,
  DebugMode = FALSE)
```

</p>
</details> 

<details><summary>LightGBM Example</summary>
<p>
 
```r
# Create some dummy correlated data
data <- AutoQuant::FakeDataGenerator(
  Correlation = 0.85,
  N = 1000,
  ID = 2,
  ZIP = 0,
  AddDate = FALSE,
  Classification = FALSE,
  MultiClass = FALSE)

# Run function
TestModel <- AutoQuant::AutoLightGBMClassifier(

  # Metadata args
  OutputSelection = c("Importances","EvalPlots","EvalMetrics","Score_TrainData"),
  model_path = normalizePath("./"),
  metadata_path = NULL,
  ModelID = "Test_Model_1",
  NumOfParDepPlots = 3L,
  EncodingMethod = "credibility",
  ReturnFactorLevels = TRUE,
  ReturnModelObjects = TRUE,
  SaveModelObjects = FALSE,
  SaveInfoToPDF = FALSE,
  DebugMode = FALSE,

  # Data args
  data = data,
  TrainOnFull = FALSE,
  ValidationData = NULL,
  TestData = NULL,
  TargetColumnName = "Adrian",
  FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
  PrimaryDateColumn = NULL,
  WeightsColumnName = NULL,
  IDcols = c("IDcol_1","IDcol_2"),

  # Grid parameters
  GridTune = FALSE,
  grid_eval_metric = 'Utility',
  BaselineComparison = 'default',
  MaxModelsInGrid = 10L,
  MaxRunsWithoutNewWinner = 20L,
  MaxRunMinutes = 24L*60L,
  PassInGrid = NULL,

  # Core parameters
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#core-parameters
  input_model = NULL, # continue training a model that is stored to file
  task = "train",
  device_type = 'CPU',
  NThreads = parallel::detectCores() / 2,
  objective = 'binary',
  metric = 'binary_logloss',
  boosting = 'gbdt',
  LinearTree = FALSE,
  Trees = 50L,
  eta = NULL,
  num_leaves = 31,
  deterministic = TRUE,

  # Learning Parameters
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#learning-control-parameters
  force_col_wise = FALSE,
  force_row_wise = FALSE,
  max_depth = NULL,
  min_data_in_leaf = 20,
  min_sum_hessian_in_leaf = 0.001,
  bagging_freq = 0,
  bagging_fraction = 1.0,
  feature_fraction = 1.0,
  feature_fraction_bynode = 1.0,
  extra_trees = FALSE,
  early_stopping_round = 10,
  first_metric_only = TRUE,
  max_delta_step = 0.0,
  lambda_l1 = 0.0,
  lambda_l2 = 0.0,
  linear_lambda = 0.0,
  min_gain_to_split = 0,
  drop_rate_dart = 0.10,
  max_drop_dart = 50,
  skip_drop_dart = 0.50,
  uniform_drop_dart = FALSE,
  top_rate_goss = FALSE,
  other_rate_goss = FALSE,
  monotone_constraints = NULL,
  monotone_constraints_method = "advanced",
  monotone_penalty = 0.0,
  forcedsplits_filename = NULL, # use for AutoStack option; .json file
  refit_decay_rate = 0.90,
  path_smooth = 0.0,

  # IO Dataset Parameters
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#io-parameters
  max_bin = 255,
  min_data_in_bin = 3,
  data_random_seed = 1,
  is_enable_sparse = TRUE,
  enable_bundle = TRUE,
  use_missing = TRUE,
  zero_as_missing = FALSE,
  two_round = FALSE,

  # Convert Parameters
  convert_model = NULL,
  convert_model_language = "cpp",

  # Objective Parameters
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#objective-parameters
  boost_from_average = TRUE,
  is_unbalance = FALSE,
  scale_pos_weight = 1.0,

  # Metric Parameters (metric is in Core)
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#metric-parameters
  is_provide_training_metric = TRUE,
  eval_at = c(1,2,3,4,5),

  # Network Parameters
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#network-parameters
  num_machines = 1,

  # GPU Parameters
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#gpu-parameters
  gpu_platform_id = -1,
  gpu_device_id = -1,
  gpu_use_dp = TRUE,
  num_gpu = 1)
```

</p>
</details> 

<details><summary>H2O-GBM Example</summary>
<p>

```r
# Create some dummy correlated data
data <- AutoQuant::FakeDataGenerator(
  Correlation = 0.85,
  N = 1000L,
  ID = 2L,
  ZIP = 0L,
  AddDate = FALSE,
  Classification = TRUE,
  MultiClass = FALSE)

TestModel <- AutoQuant::AutoH2oGBMClassifier(
  
  # Compute management
  MaxMem = {gc();paste0(as.character(floor(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE)) / 1000000)),"G")},
  NThreads = max(1, parallel::detectCores()-2),
  H2OShutdown = TRUE,
  H2OStartUp = TRUE,
  IfSaveModel = "mojo",
  
  # Model evaluation
  NumOfParDepPlots = 3,
  
  # Metadata arguments:
  model_path = normalizePath("./"),
  metadata_path = file.path(normalizePath("./")),
  ModelID = "FirstModel",
  ReturnModelObjects = TRUE,
  SaveModelObjects = FALSE,
  SaveInfoToPDF = FALSE,
  
  # Data arguments
  data = data,
  TrainOnFull = FALSE,
  ValidationData = NULL,
  TestData = NULL,
  TargetColumnName = "Adrian",
  FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
  WeightsColumn = NULL,
  
  # ML grid tuning args
  GridTune = FALSE,
  GridStrategy = "Cartesian",
  MaxRuntimeSecs = 60*60*24,
  StoppingRounds = 10,
  MaxModelsInGrid = 2,
  
  # Model args
  Trees = 50,
  LearnRate = 0.10,
  LearnRateAnnealing = 1,
  eval_metric = "auc",
  Distribution = "bernoulli",
  MaxDepth = 20,
  SampleRate = 0.632,
  ColSampleRate = 1,
  ColSampleRatePerTree = 1,
  ColSampleRatePerTreeLevel  = 1,
  MinRows = 1,
  NBins = 20,
  NBinsCats = 1024,
  NBinsTopLevel = 1024,
  HistogramType = "AUTO",
  CategoricalEncoding = "AUTO")
```

</p>
</details> 

<details><summary>H2O-DRF Example</summary>
<p>
 
```r
# Create some dummy correlated data
data <- AutoQuant::FakeDataGenerator(
  Correlation = 0.85,
  N = 1000L,
  ID = 2L,
  ZIP = 0L,
  AddDate = FALSE,
  Classification = TRUE,
  MultiClass = FALSE)

TestModel <- AutoQuant::AutoH2oDRFClassifier(
  
  # Compute management
  MaxMem = {gc();paste0(as.character(floor(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE)) / 1000000)),"G")},
  NThreads = max(1L, parallel::detectCores() - 2L),
  IfSaveModel = "mojo",
  H2OShutdown = FALSE,
  H2OStartUp = TRUE,
  
  # Metadata arguments:
  eval_metric = "auc",
  NumOfParDepPlots = 3L,
  
  # Data arguments:
  model_path = normalizePath("./"),
  metadata_path = NULL,
  ModelID = "FirstModel",
  ReturnModelObjects = TRUE,
  SaveModelObjects = FALSE,
  SaveInfoToPDF = FALSE,
  
  # Model evaluation:
  data,
  TrainOnFull = FALSE,
  ValidationData = NULL,
  TestData = NULL,
  TargetColumnName = "Adrian",
  FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2", "Adrian")],
  WeightsColumn = NULL,
  
  # Grid Tuning Args
  GridStrategy = "Cartesian",
  GridTune = FALSE,
  MaxModelsInGrid = 10,
  MaxRuntimeSecs = 60*60*24,
  StoppingRounds = 10,
  
  # Model args
  Trees = 50L,
  MaxDepth = 20,
  SampleRate = 0.632,
  MTries = -1,
  ColSampleRatePerTree = 1,
  ColSampleRatePerTreeLevel = 1,
  MinRows = 1,
  NBins = 20,
  NBinsCats = 1024,
  NBinsTopLevel = 1024,
  HistogramType = "AUTO",
  CategoricalEncoding = "AUTO")
```
 
</p>
</details> 

<details><summary>H2O-GLM Example</summary>
<p>
 
```r
# Create some dummy correlated data with numeric and categorical features
data <- AutoQuant::FakeDataGenerator(
  Correlation = 0.85,
  N = 1000L,
  ID = 2L,
  ZIP = 0L,
  AddDate = FALSE,
  Classification = TRUE,
  MultiClass = FALSE)

# Run function
TestModel <- AutoQuant::AutoH2oGLMClassifier(
  
  # Compute management
  MaxMem = {gc();paste0(as.character(floor(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE)) / 1000000)),"G")},
  NThreads = max(1, parallel::detectCores()-2),
  H2OShutdown = TRUE,
  H2OStartUp = TRUE,
  IfSaveModel = "mojo",
  
  # Model evaluation args
  eval_metric = "auc",
  NumOfParDepPlots = 3,
  
  # Metadata args
  model_path = NULL,
  metadata_path = NULL,
  ModelID = "FirstModel",
  ReturnModelObjects = TRUE,
  SaveModelObjects = FALSE,
  SaveInfoToPDF = FALSE,
  
  # Data args
  data = data,
  TrainOnFull = FALSE,
  ValidationData = NULL,
  TestData = NULL,
  TargetColumnName = "Adrian",
  FeatureColNames = names(data)[!names(data) %in%
                                  c("IDcol_1", "IDcol_2","Adrian")],
  RandomColNumbers = NULL,
  InteractionColNumbers = NULL,
  WeightsColumn = NULL,
  
  # ML args
  GridTune = FALSE,
  GridStrategy = "Cartesian",
  StoppingRounds = 10,
  MaxRunTimeSecs = 3600 * 24 * 7,
  MaxModelsInGrid = 10,
  Distribution = "binomial",
  Link = "logit",
  RandomDistribution = NULL,
  RandomLink = NULL,
  Solver = "AUTO",
  Alpha = NULL,
  Lambda = NULL,
  LambdaSearch = FALSE,
  NLambdas = -1,
  Standardize = TRUE,
  RemoveCollinearColumns = FALSE,
  InterceptInclude = TRUE,
  NonNegativeCoefficients = FALSE)
```
 
</p>
</details> 

<details><summary>H2O-AutoML Example</summary>
<p>
 
```r
# Create some dummy correlated data with numeric and categorical features
data <- AutoQuant::FakeDataGenerator(
  Correlation = 0.85, 
  N = 1000L, 
  ID = 2L, 
  ZIP = 0L, 
  AddDate = FALSE, 
  Classification = TRUE, 
  MultiClass = FALSE)

TestModel <- AutoQuant::AutoH2oMLClassifier(
   data,
   TrainOnFull = FALSE,
   ValidationData = NULL,
   TestData = NULL,
   TargetColumnName = "Adrian",
   FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
   ExcludeAlgos = NULL,
   eval_metric = "auc",
   Trees = 50,
   MaxMem = "32G",
   NThreads = max(1, parallel::detectCores()-2),
   MaxModelsInGrid = 10,
   model_path = normalizePath("./"),
   metadata_path = file.path(normalizePath("./"), "MetaData"),
   ModelID = "FirstModel",
   NumOfParDepPlots = 3,
   ReturnModelObjects = TRUE,
   SaveModelObjects = FALSE,
   IfSaveModel = "mojo",
   H2OShutdown = FALSE,
   HurdleModel = FALSE)
```
 
</p>
</details> 

<details><summary>H2O-GAM Example</summary>
<p>
 
```r
# Create some dummy correlated data
data <- AutoQuant::FakeDataGenerator(
  Correlation = 0.85,
  N = 1000,
  ID = 2,
  ZIP = 0,
  AddDate = FALSE,
  Classification = TRUE,
  MultiClass = FALSE)

# Define GAM Columns to use - up to 9 are allowed
GamCols <- names(which(unlist(lapply(data, is.numeric))))
GamCols <- GamCols[!GamCols %in% c("Adrian","IDcol_1","IDcol_2")]
GamCols <- GamCols[1L:(min(9L,length(GamCols)))]

# Run function
TestModel <- AutoQuant::AutoH2oGAMClassifier(

  # Compute management
  MaxMem = {gc();paste0(as.character(floor(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE)) / 1000000)),"G")},
  NThreads = max(1, parallel::detectCores()-2),
  H2OShutdown = TRUE,
  H2OStartUp = TRUE,
  IfSaveModel = "mojo",

  # Model evaluation:
  eval_metric = "auc",
  NumOfParDepPlots = 3,

  # Metadata arguments:
  model_path = NULL,
  metadata_path = NULL,
  ModelID = "FirstModel",
  ReturnModelObjects = TRUE,
  SaveModelObjects = FALSE,
  SaveInfoToPDF = FALSE,

  # Data arguments:
  data = data,
  TrainOnFull = FALSE,
  ValidationData = NULL,
  TestData = NULL,
  TargetColumnName = "Adrian",
  FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
  WeightsColumn = NULL,
  GamColNames = GamCols,

  # ML args
  num_knots = NULL,
  keep_gam_cols = TRUE,
  GridTune = FALSE,
  GridStrategy = "Cartesian",
  StoppingRounds = 10,
  MaxRunTimeSecs = 3600 * 24 * 7,
  MaxModelsInGrid = 10,
  Distribution = "binomial",
  Link = "logit",
  Solver = "AUTO",
  Alpha = NULL,
  Lambda = NULL,
  LambdaSearch = FALSE,
  NLambdas = -1,
  Standardize = TRUE,
  RemoveCollinearColumns = FALSE,
  InterceptInclude = TRUE,
  NonNegativeCoefficients = FALSE)
```
 
</p>
</details>

</p>
</details>

<br>

### MultiClass Classification

<details><summary>click to expand</summary>
<p>

<details><summary>MultiClass Description</summary>
<p>
  
The Auto_MultiClass() models handle a multitude of tasks. In order:
1. Convert your data to data.table format for faster processing
2. Create train, validation, and test data if you didn't supply those directly to the function
3. Consoldate columns that are used for modeling and what is to be kept for data returned
4. Dichotomize categorical variables (for AutoXGBoostRegression) and save the factor levels for scoring in a way that guarentees consistency across training, validation, and test data sets
5. Saves the final column names for modeling to a csv for later reference
6. Ensures the target levels are consistent across train, validate, and test sets and save the levels to file
7. Handles the data conversion to the appropriate type, based on model type (CatBoost, H2O, and XGBoost)
8. Multi-armed bandit hyperparameter tuning using randomized probability matching, if you choose to grid tune
9. Build the grid tuned models
10. Collect the evaluation metrics for each grid tune run
11. Identify the best model of the set of models built in the grid tuning setup
12. Save the hyperparameters from the winning grid tuned model
13. Build the final model based on the best model from the grid tuning model search
14. Collect evaluation metrics based on performance on test data
15. Store the final predictions with the associated test data and other columns you want included in that set
16. Generate and save Variable Importance data
17. Return all the objects generated in a named list for immediate use

</p>
</details>

<details><summary>CatBoost Example</summary>
<p>

```r
# Create some dummy correlated data
data <- AutoQuant::FakeDataGenerator(
  Correlation = 0.85,
  N = 10000L,
  ID = 2L,
  ZIP = 0L,
  AddDate = FALSE,
  Classification = FALSE,
  MultiClass = TRUE)

# Run function
TestModel <- AutoQuant::AutoCatBoostMultiClass(
  
  # GPU or CPU and the number of available GPUs
  task_type = 'GPU',
  NumGPUs = 1,
  TrainOnFull = FALSE,
  DebugMode = FALSE,
  
  # Metadata args
  OutputSelection = c('Importances', 'EvalPlots', 'EvalMetrics', 'Score_TrainData'),
  ModelID = 'Test_Model_1',
  model_path = normalizePath('./'),
  metadata_path = normalizePath('./'),
  SaveModelObjects = FALSE,
  ReturnModelObjects = TRUE,
  
  # Data args
  data = data,
  ValidationData = NULL,
  TestData = NULL,
  TargetColumnName = 'Adrian',
  FeatureColNames = names(data)[!names(data) %in%
                                  c('IDcol_1', 'IDcol_2','Adrian')],
  PrimaryDateColumn = NULL,
  WeightsColumnName = NULL,
  ClassWeights = c(1L,1L,1L,1L,1L),
  IDcols = c('IDcol_1','IDcol_2'),
  
  # Model evaluation
  eval_metric = 'MCC',
  loss_function = 'MultiClassOneVsAll',
  grid_eval_metric = 'Accuracy',
  MetricPeriods = 10L,
  NumOfParDepPlots = 3,
  
  # Grid tuning args
  PassInGrid = NULL,
  GridTune = TRUE,
  MaxModelsInGrid = 30L,
  MaxRunsWithoutNewWinner = 20L,
  MaxRunMinutes = 24L*60L,
  BaselineComparison = 'default',
  
  # ML args
  langevin = FALSE,
  diffusion_temperature = 10000,
  Trees = seq(100L, 500L, 50L),
  Depth = seq(4L, 8L, 1L),
  LearningRate = seq(0.01,0.10,0.01),
  L2_Leaf_Reg = seq(1.0, 10.0, 1.0),
  RandomStrength = 1,
  BorderCount = 254,
  RSM = c(0.80, 0.85, 0.90, 0.95, 1.0),
  BootStrapType = c('Bayesian', 'Bernoulli', 'Poisson', 'MVS', 'No'),
  GrowPolicy = c('SymmetricTree', 'Depthwise', 'Lossguide'),
  model_size_reg = 0.5,
  feature_border_type = 'GreedyLogSum',
  sampling_unit = 'Object',
  subsample = NULL,
  score_function = 'Cosine',
  min_data_in_leaf = 1)

# Legacy compatibility training report.
# New regression/binary workflows should use the artifact-first report functions.
AutoQuant::ModelInsightsReport(
  TrainDataInclude = TRUE,
  FeatureColumnNames = names(data)[!names(data) %in% c('IDcol_1', 'IDcol_2','Adrian')],
  SampleSize = 1e+05,
  ModelObject = TestModel,
  ModelID = "Test_Model_1",
  OutputPath = getwd()
)
```

</p>
</details>

<details><summary>XGBoost Example</summary>
<p>

```r
# Create some dummy correlated data
data <- AutoQuant::FakeDataGenerator(
  Correlation = 0.85,
  N = 1000L,
  ID = 2L,
  ZIP = 0L,
  AddDate = FALSE,
  Classification = FALSE,
  MultiClass = TRUE)

# Run function
TestModel <- AutoQuant::AutoXGBoostMultiClass(
  
  # GPU or CPU
  TreeMethod = "hist",
  NThreads = parallel::detectCores(),
  
  # Metadata args
  OutputSelection = c("Importances", "EvalPlots", "EvalMetrics", "PDFs", "Score_TrainData"),
  model_path = normalizePath("./"),
  metadata_path = normalizePath("./"),
  ModelID = "Test_Model_1",
  EncodingMethod = "binary",
  ReturnFactorLevels = TRUE,
  ReturnModelObjects = TRUE,
  SaveModelObjects = FALSE,
  
  # Data args
  data = data,
  TrainOnFull = FALSE,
  ValidationData = NULL,
  TestData = NULL,
  TargetColumnName = "Adrian",
  FeatureColNames = names(data)[!names(data) %in%
                                  c("IDcol_1", "IDcol_2","Adrian")],
  WeightsColumnName = NULL,
  IDcols = c("IDcol_1","IDcol_2"),
  
  # Model evaluation args
  eval_metric = "merror",
  LossFunction = 'multi:softprob',
  grid_eval_metric = "accuracy",
  NumOfParDepPlots = 3L,
  
  # Grid tuning args
  PassInGrid = NULL,
  GridTune = FALSE,
  BaselineComparison = "default",
  MaxModelsInGrid = 10L,
  MaxRunsWithoutNewWinner = 20L,
  MaxRunMinutes = 24L*60L,
  Verbose = 1L,
  DebugMode = FALSE,
  
  # ML args
  Trees = 50L,
  eta = 0.05,
  max_depth = 4L,
  min_child_weight = 1.0,
  subsample = 0.55,
  colsample_bytree = 0.55)
```

</p>
</details>

<details><summary>LightGBM Example</summary>
<p>

```r
# Create some dummy correlated data
data <- AutoQuant::FakeDataGenerator(
  Correlation = 0.85,
  N = 1000,
  ID = 2,
  ZIP = 0,
  AddDate = FALSE,
  Classification = FALSE,
  MultiClass = FALSE)

# Run function
TestModel <- AutoQuant::AutoLightGBMMultiClass(

  # Metadata args
  OutputSelection = c("Importances","EvalPlots","EvalMetrics","Score_TrainData"),
  model_path = normalizePath("./"),
  metadata_path = NULL,
  ModelID = "Test_Model_1",
  NumOfParDepPlots = 3L,
  EncodingMethod = "credibility",
  ReturnFactorLevels = TRUE,
  ReturnModelObjects = TRUE,
  SaveModelObjects = FALSE,
  SaveInfoToPDF = FALSE,
  DebugMode = FALSE,

  # Data args
  data = data,
  TrainOnFull = FALSE,
  ValidationData = NULL,
  TestData = NULL,
  TargetColumnName = "Adrian",
  FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
  PrimaryDateColumn = NULL,
  WeightsColumnName = NULL,
  IDcols = c("IDcol_1","IDcol_2"),

  # Grid parameters
  GridTune = FALSE,
  grid_eval_metric = 'microauc',
  BaselineComparison = 'default',
  MaxModelsInGrid = 10L,
  MaxRunsWithoutNewWinner = 20L,
  MaxRunMinutes = 24L*60L,
  PassInGrid = NULL,

  # Core parameters
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#core-parameters
  input_model = NULL, # continue training a model that is stored to file
  task = "train",
  device_type = 'CPU',
  NThreads = parallel::detectCores() / 2,
  objective = 'multiclass',
  multi_error_top_k = 1,
  metric = 'multi_logloss',
  boosting = 'gbdt',
  LinearTree = FALSE,
  Trees = 50L,
  eta = NULL,
  num_leaves = 31,
  deterministic = TRUE,

  # Learning Parameters
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#learning-control-parameters
  force_col_wise = FALSE,
  force_row_wise = FALSE,
  max_depth = NULL,
  min_data_in_leaf = 20,
  min_sum_hessian_in_leaf = 0.001,
  bagging_freq = 0,
  bagging_fraction = 1.0,
  feature_fraction = 1.0,
  feature_fraction_bynode = 1.0,
  extra_trees = FALSE,
  early_stopping_round = 10,
  first_metric_only = TRUE,
  max_delta_step = 0.0,
  lambda_l1 = 0.0,
  lambda_l2 = 0.0,
  linear_lambda = 0.0,
  min_gain_to_split = 0,
  drop_rate_dart = 0.10,
  max_drop_dart = 50,
  skip_drop_dart = 0.50,
  uniform_drop_dart = FALSE,
  top_rate_goss = FALSE,
  other_rate_goss = FALSE,
  monotone_constraints = NULL,
  monotone_constraints_method = "advanced",
  monotone_penalty = 0.0,
  forcedsplits_filename = NULL, # use for AutoStack option; .json file
  refit_decay_rate = 0.90,
  path_smooth = 0.0,

  # IO Dataset Parameters
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#io-parameters
  max_bin = 255,
  min_data_in_bin = 3,
  data_random_seed = 1,
  is_enable_sparse = TRUE,
  enable_bundle = TRUE,
  use_missing = TRUE,
  zero_as_missing = FALSE,
  two_round = FALSE,

  # Convert Parameters
  convert_model = NULL,
  convert_model_language = "cpp",

  # Objective Parameters
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#objective-parameters
  boost_from_average = TRUE,
  is_unbalance = FALSE,
  scale_pos_weight = 1.0,

  # Metric Parameters (metric is in Core)
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#metric-parameters
  is_provide_training_metric = TRUE,
  eval_at = c(1,2,3,4,5),

  # Network Parameters
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#network-parameters
  num_machines = 1,

  # GPU Parameters
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#gpu-parameters
  gpu_platform_id = -1,
  gpu_device_id = -1,
  gpu_use_dp = TRUE,
  num_gpu = 1)
```

</p>
</details>

<details><summary>H2O-GBM Example</summary>
<p>

```r
# Create some dummy correlated data
data <- AutoQuant::FakeDataGenerator(
  Correlation = 0.85,
  N = 1000,
  ID = 2,
  ZIP = 0,
  AddDate = FALSE,
  Classification = FALSE,
  MultiClass = TRUE)

# Run function
TestModel <- AutoQuant::AutoH2oGBMMultiClass(
   data,
   TrainOnFull = FALSE,
   ValidationData = NULL,
   TestData = NULL,
   TargetColumnName = "Adrian",
   FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
   WeightsColumn = NULL,
   eval_metric = "logloss",
   MaxMem = {gc();paste0(as.character(floor(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE)) / 1000000)),"G")},
   NThreads = max(1, parallel::detectCores()-2),
   model_path = normalizePath("./"),
   metadata_path = file.path(normalizePath("./")),
   ModelID = "FirstModel",
   ReturnModelObjects = TRUE,
   SaveModelObjects = FALSE,
   IfSaveModel = "mojo",
   H2OShutdown = TRUE,
   H2OStartUp = TRUE,

   # Model args
   GridTune = FALSE,
   GridStrategy = "Cartesian",
   MaxRuntimeSecs = 60*60*24,
   StoppingRounds = 10,
   MaxModelsInGrid = 2,
   Trees = 50,
   LearnRate = 0.10,
   LearnRateAnnealing = 1,
   eval_metric = "RMSE",
   Distribution = "multinomial",
   MaxDepth = 20,
   SampleRate = 0.632,
   ColSampleRate = 1,
   ColSampleRatePerTree = 1,
   ColSampleRatePerTreeLevel  = 1,
   MinRows = 1,
   NBins = 20,
   NBinsCats = 1024,
   NBinsTopLevel = 1024,
   HistogramType = "AUTO",
   CategoricalEncoding = "AUTO")
```

</p>
</details>

<details><summary>H2O-DRF Example</summary>
<p>

```r
# Create some dummy correlated data
data <- AutoQuant::FakeDataGenerator(
  Correlation = 0.85,
  N = 1000L,
  ID = 2L,
  ZIP = 0L,
  AddDate = FALSE,
  Classification = FALSE,
  MultiClass = TRUE)

# Run function
TestModel <- AutoQuant::AutoH2oDRFMultiClass(
  data,
  TrainOnFull = FALSE,
  ValidationData = NULL,
  TestData = NULL,
  TargetColumnName = "Adrian",
  FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
  WeightsColumn = NULL,
  eval_metric = "logloss",
  MaxMem = {gc();paste0(as.character(floor(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE)) / 1000000)),"G")},
  NThreads = max(1, parallel::detectCores()-2),
  model_path = normalizePath("./"),
  metadata_path = file.path(normalizePath("./")),
  ModelID = "FirstModel",
  ReturnModelObjects = TRUE,
  SaveModelObjects = FALSE,
  IfSaveModel = "mojo",
  H2OShutdown = FALSE,
  H2OStartUp = TRUE,

  # Grid Tuning Args
  GridStrategy = "Cartesian",
  GridTune = FALSE,
  MaxModelsInGrid = 10,
  MaxRuntimeSecs = 60*60*24,
  StoppingRounds = 10,

  # ML args
  Trees = 50,
  MaxDepth = 20,
  SampleRate = 0.632,
  MTries = -1,
  ColSampleRatePerTree = 1,
  ColSampleRatePerTreeLevel = 1,
  MinRows = 1,
  NBins = 20,
  NBinsCats = 1024,
  NBinsTopLevel = 1024,
  HistogramType = "AUTO",
  CategoricalEncoding = "AUTO")
```

</p>
</details>

<details><summary>H2O-GLM Example</summary>
<p>

```r
# Create some dummy correlated data with numeric and categorical features
data <- AutoQuant::FakeDataGenerator(
  Correlation = 0.85,
  N = 1000L,
  ID = 2L,
  ZIP = 0L,
  AddDate = FALSE,
  Classification = FALSE,
  MultiClass = TRUE)

# Run function
TestModel <- AutoQuant::AutoH2oGLMMultiClass(
  
  # Compute management
  MaxMem = {gc();paste0(as.character(floor(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE)) / 1000000)),"G")},
  NThreads = max(1, parallel::detectCores()-2),
  H2OShutdown = TRUE,
  H2OStartUp = TRUE,
  IfSaveModel = "mojo",
  
  # Model evaluation:
  eval_metric = "logloss",
  NumOfParDepPlots = 3,
  
  # Metadata arguments:
  model_path = NULL,
  metadata_path = NULL,
  ModelID = "FirstModel",
  ReturnModelObjects = TRUE,
  SaveModelObjects = FALSE,
  SaveInfoToPDF = FALSE,
  
  # Data arguments:
  data = data,
  TrainOnFull = FALSE,
  ValidationData = NULL,
  TestData = NULL,
  TargetColumnName = "Adrian",
  FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
  RandomColNumbers = NULL,
  InteractionColNumbers = NULL,
  WeightsColumn = NULL,
  
  # Model args
  GridTune = FALSE,
  GridStrategy = "Cartesian",
  StoppingRounds = 10,
  MaxRunTimeSecs = 3600 * 24 * 7,
  MaxModelsInGrid = 10,
  Distribution = "multinomial",
  Link = "family_default",
  RandomDistribution = NULL,
  RandomLink = NULL,
  Solver = "AUTO",
  Alpha = NULL,
  Lambda = NULL,
  LambdaSearch = FALSE,
  NLambdas = -1,
  Standardize = TRUE,
  RemoveCollinearColumns = FALSE,
  InterceptInclude = TRUE,
  NonNegativeCoefficients = FALSE)
```

</p>
</details>

<details><summary>H2O-AutoML Example</summary>
<p>

```r
# Create some dummy correlated data with numeric and categorical features
data <- AutoQuant::FakeDataGenerator(Correlation = 0.85, N = 1000, ID = 2, ZIP = 0, AddDate = FALSE, Classification = FALSE, MultiClass = TRUE)

# Run function
TestModel <- AutoQuant::AutoH2oMLMultiClass(
   data,
   TrainOnFull = FALSE,
   ValidationData = NULL,
   TestData = NULL,
   TargetColumnName = "Adrian",
   FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
   ExcludeAlgos = NULL,
   eval_metric = "logloss",
   Trees = 50,
   MaxMem = "32G",
   NThreads = max(1, parallel::detectCores()-2),
   MaxModelsInGrid = 10,
   model_path = normalizePath("./"),
   metadata_path = file.path(normalizePath("./"), "MetaData"),
   ModelID = "FirstModel",
   ReturnModelObjects = TRUE,
   SaveModelObjects = FALSE,
   IfSaveModel = "mojo",
   H2OShutdown = FALSE,
   HurdleModel = FALSE)
```

</p>
</details>

<details><summary>H2O-GAM Example</summary>
<p>

```r
# Create some dummy correlated data with numeric and categorical features
data <- AutoQuant::FakeDataGenerator(
  Correlation = 0.85,
  N = 1000L,
  ID = 2L,
  ZIP = 0L,
  AddDate = FALSE,
  Classification = FALSE,
  MultiClass = TRUE)

# Define GAM Columns to use - up to 9 are allowed
GamCols <- names(which(unlist(lapply(data, is.numeric))))
GamCols <- GamCols[!GamCols %in% c("Adrian","IDcol_1","IDcol_2")]
GamCols <- GamCols[1L:(min(9L,length(GamCols)))]

# Run function
TestModel <- AutoQuant::AutoH2oGAMMultiClass(
  data,
  TrainOnFull = FALSE,
  ValidationData = NULL,
  TestData = NULL,
  TargetColumnName = "Adrian",
  FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
  WeightsColumn = NULL,
  GamColNames = GamCols,
  eval_metric = "logloss",
  MaxMem = {gc();paste0(as.character(floor(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE)) / 1000000)),"G")},
  NThreads = max(1, parallel::detectCores()-2),
  model_path = normalizePath("./"),
  metadata_path = NULL,
  ModelID = "FirstModel",
  ReturnModelObjects = TRUE,
  SaveModelObjects = FALSE,
  IfSaveModel = "mojo",
  H2OShutdown = FALSE,
  H2OStartUp = TRUE,
  
  # ML args
  num_knots = NULL,
  keep_gam_cols = TRUE,
  GridTune = FALSE,
  GridStrategy = "Cartesian",
  StoppingRounds = 10,
  MaxRunTimeSecs = 3600 * 24 * 7,
  MaxModelsInGrid = 10,
  Distribution = "multinomial",
  Link = "Family_Default",
  Solver = "AUTO",
  Alpha = NULL,
  Lambda = NULL,
  LambdaSearch = FALSE,
  NLambdas = -1,
  Standardize = TRUE,
  RemoveCollinearColumns = FALSE,
  InterceptInclude = TRUE,
  NonNegativeCoefficients = FALSE)
```

</p>
</details>

</p>
</details>



</p>
</details>



</p>
</details>

<br>

## Model Scoring <img src="https://raw.githubusercontent.com/AdrianAntico/AutoQuant/master/Images/ModelScoringImage.png" align="right" width="80" />
<details><summary>Expand to view content</summary>
<p>



<details><summary>Scoring Description</summary>
<p>

<code>AutoCatBoostScoring()</code> is an automated scoring function that compliments the AutoCatBoost__() model training functions. This function requires you to supply features for scoring. It will run ModelDataPrep() to prepare your features for catboost data conversion and scoring. It will also handle and transformations and back-transformations if you utilized that feature in the regression training case.

<code>AutoXGBoostScoring()</code> is an automated scoring function that compliments the AutoXGBoost__() model training functions. This function requires you to supply features for scoring. It will run ModelDataPrep() and the CategoricalEncoding() functions to prepare your features for xgboost data conversion and scoring. It will also handle and transformations and back-transformations if you utilized that feature in the regression training case.

<code>AutoLightGBMScoring()</code> is an automated scoring function that compliments the AutoLightGBM__() model training functions. This function requires you to supply features for scoring. It will run ModelDataPrep() and the CategoricalEncoding() functions to prepare your features for lightgbm data conversion and scoring. It will also handle and transformations and back-transformations if you utilized that feature in the regression training case.

<code>AutoH2OMLScoring()</code> is an automated scoring function that compliments the AutoH2oGBM__() and AutoH2oDRF__() model training functions. This function requires you to supply features for scoring. It will run ModelDataPrep()to prepare your features for H2O data conversion and scoring. It will also handle transformations and back-transformations if you utilized that feature in the regression training case and didn't do it yourself before hand.


</p>
</details>



<details><summary>AutoCatBoost__() Examples</summary>
<p>

<details><summary>AutoCatBoostRegression() Scoring Example</summary>
<p>

```r
# Create some dummy correlated data
data <- AutoQuant::FakeDataGenerator(
  Correlation = 0.85,
  N = 10000,
  ID = 2,
  ZIP = 0,
  AddDate = FALSE,
  Classification = FALSE,
  MultiClass = FALSE)

# Copy data
data1 <- data.table::copy(data)

# Feature Colnames
Features <- names(data1)[!names(data1) %in% c("IDcol_1", "IDcol_2","DateTime","Adrian")]

# Run function
TestModel <- AutoQuant::AutoCatBoostRegression(
  
  # GPU or CPU and the number of available GPUs
  TrainOnFull = FALSE,
  task_type = 'CPU',
  NumGPUs = 1,
  DebugMode = FALSE,
  
  # Metadata args
  OutputSelection = c('Importances','EvalPlots','EvalMetrics','Score_TrainData'),
  ModelID = 'Test_Model_1',
  model_path = getwd(),
  metadata_path = getwd(),
  SaveModelObjects = FALSE,
  SaveInfoToPDF = FALSE,
  ReturnModelObjects = TRUE,
  
  # Data args
  data = data1,
  ValidationData = NULL,
  TestData = NULL,
  TargetColumnName = 'Adrian',
  FeatureColNames = Features,
  PrimaryDateColumn = NULL,
  WeightsColumnName = NULL,
  IDcols = c('IDcol_1','IDcol_2'),
  TransformNumericColumns = 'Adrian',
  Methods = c('Asinh','Asin','Log','LogPlus1','Sqrt','Logit'),
  
  # Model evaluation
  eval_metric = 'RMSE',
  eval_metric_value = 1.5,
  loss_function = 'RMSE',
  loss_function_value = 1.5,
  MetricPeriods = 10L,
  NumOfParDepPlots = ncol(data1)-1L-2L,
  
  # Grid tuning args
  PassInGrid = NULL,
  GridTune = FALSE,
  MaxModelsInGrid = 30L,
  MaxRunsWithoutNewWinner = 20L,
  MaxRunMinutes = 60*60,
  BaselineComparison = 'default',
  
  # ML args
  langevin = FALSE,
  diffusion_temperature = 10000,
  Trees = 1000,
  Depth = 9,
  L2_Leaf_Reg = NULL,
  RandomStrength = 1,
  BorderCount = 128,
  LearningRate = NULL,
  RSM = 1,
  BootStrapType = NULL,
  GrowPolicy = 'SymmetricTree',
  model_size_reg = 0.5,
  feature_border_type = 'GreedyLogSum',
  sampling_unit = 'Object',
  subsample = NULL,
  score_function = 'Cosine',
  min_data_in_leaf = 1)


# Legacy compatibility insights report.
# New regression/binary workflows should use the artifact-first report functions.
AutoQuant::ModelInsightsReport(
  TrainDataInclude = TRUE,
  FeatureColumnNames = Features,
  SampleSize = 100000,
  ModelObject = ModelObject,
  ModelID = 'Test_Model_1',
  OutputPath = getwd())


# Score data
Preds <- AutoQuant::AutoCatBoostScoring(
  TargetType = 'regression',
  ScoringData = data,
  FeatureColumnNames = Features,
  FactorLevelsList = TestModel$FactorLevelsList,
  IDcols = c('IDcol_1','IDcol_2'),
  OneHot = FALSE,
  ReturnShapValues = TRUE,
  ModelObject = TestModel$Model,
  ModelPath = NULL,
  ModelID = 'Test_Model_1',
  ReturnFeatures = TRUE,
  MultiClassTargetLevels = NULL,
  TransformNumeric = FALSE,
  BackTransNumeric = FALSE,
  TargetColumnName = NULL,
  TransformationObject = NULL,
  TransID = NULL,
  TransPath = NULL,
  MDP_Impute = FALSE,
  MDP_CharToFactor = TRUE,
  MDP_RemoveDates = TRUE,
  MDP_MissFactor = '0',
  MDP_MissNum = -1,
  RemoveModel = FALSE)
```

</p>
</details>

<details><summary>AutoCatBoostClassifier() Scoring Example</summary>
<p>

```r
# Refresh data
data <- AutoQuant::FakeDataGenerator(
  Correlation = 0.85,
  N = 25000L,
  ID = 2L,
  AddWeightsColumn = TRUE,
  ZIP = 0L,
  AddDate = TRUE,
  Classification = TRUE,
  MultiClass = FALSE)

# Copy data (used for scoring below``)
data1 <- data.table::copy(data)

# Partition Data
Sets <- Rodeo::AutoDataPartition(
  data = data,
  NumDataSets = 3,
  Ratios = c(0.7,0.2,0.1),
  PartitionType = "random",
  StratifyColumnNames = "Adrian",
  TimeColumnName = NULL)
TTrainData <- Sets$TrainData
VValidationData <- Sets$ValidationData
TTestData <- Sets$TestData
rm(Sets)

# Feature Colnames
Features <- names(TTrainData)[!names(TTrainData) %in% c("IDcol_1", "IDcol_2","DateTime","Adrian")]

# AutoCatBoostClassifier
TestModel <- AutoQuant::AutoCatBoostClassifier(
  
  # GPU or CPU and the number of available GPUs
  task_type = "CPU",
  NumGPUs = 1,
  
  # Metadata arguments
  OutputSelection = c("Importances", "EvalPlots", "EvalMetrics", "Score_TrainData"),
  ModelID = "Test_Model_1",
  model_path = normalizePath("./"),
  metadata_path = normalizePath("./"),
  SaveModelObjects = FALSE,
  ReturnModelObjects = TRUE,
  SaveInfoToPDF = FALSE,
  
  # Data arguments
  data = TTrainData,
  TrainOnFull = FALSE,
  ValidationData = VValidationData,
  TestData = TTestData,
  TargetColumnName = "Adrian",
  FeatureColNames = Features,
  PrimaryDateColumn = "DateTime",
  WeightsColumnName = "Weights",
  ClassWeights = c(1L,1L),
  IDcols = c("IDcol_1","IDcol_2","DateTime"),
  
  # Model evaluation
  CostMatrixWeights = c(2,0,0,1),
  EvalMetric = "MCC",
  LossFunction = "Logloss",
  grid_eval_metric = "Utility",
  MetricPeriods = 10L,
  NumOfParDepPlots = 3,
  
  # Grid tuning arguments
  PassInGrid = NULL,
  GridTune = FALSE,
  MaxModelsInGrid = 30L,
  MaxRunsWithoutNewWinner = 20L,
  MaxRunMinutes = 24L*60L,
  BaselineComparison = "default",
  
  # ML args
  Trees = 100L,
  Depth = 4L,
  LearningRate = NULL,
  L2_Leaf_Reg = NULL,
  RandomStrength = 1,
  BorderCount = 128,
  RSM = 0.80,
  BootStrapType = "Bayesian",
  GrowPolicy = "SymmetricTree",
  langevin = FALSE,
  diffusion_temperature = 10000,
  model_size_reg = 0.5,
  feature_border_type = "GreedyLogSum",
  sampling_unit = "Object",
  subsample = NULL,
  score_function = "Cosine",
  min_data_in_leaf = 1,
  DebugMode = TRUE)


# Legacy compatibility insights report.
# New binary workflows should use BinaryClassificationModelInsightsReport().
AutoQuant::ModelInsightsReport(
  TrainDataInclude = TRUE,
  FeatureColumnNames = Features,
  SampleSize = 100000,
  ModelObject = ModelObject,
  ModelID = 'Test_Model_1',
  OutputPath = getwd())


# Score data
Preds <- AutoQuant::AutoCatBoostScoring(
  TargetType = 'classifier',
  ScoringData = data,
  FeatureColumnNames = Features,
  FactorLevelsList = TestModel$FactorLevelsList,
  IDcols = c("IDcol_1","IDcol_2","DateTime"),
  OneHot = FALSE,
  ReturnShapValues = TRUE,
  ModelObject = TestModel$Model,
  ModelPath = NULL,
  ModelID = 'Test_Model_1',
  ReturnFeatures = TRUE,
  MultiClassTargetLevels = NULL,
  TransformNumeric = FALSE,
  BackTransNumeric = FALSE,
  TargetColumnName = NULL,
  TransformationObject = NULL,
  TransID = NULL,
  TransPath = NULL,
  MDP_Impute = TRUE,
  MDP_CharToFactor = TRUE,
  MDP_RemoveDates = TRUE,
  MDP_MissFactor = '0',
  MDP_MissNum = -1,
  RemoveModel = FALSE)
```

</p>
</details>

<details><summary>AutoCatBoostMultiClasss() Scoring Example</summary>
<p>

```r
# Refresh data
data <- AutoQuant::FakeDataGenerator(
  Correlation = 0.85,
  N = 25000L,
  ID = 2L,
  AddWeightsColumn = TRUE,
  ZIP = 0L,
  AddDate = TRUE,
  Classification = FALSE,
  MultiClass = TRUE)

# Copy data (used for scoring below``)
data1 <- data.table::copy(data)

# Partition Data
Sets <- Rodeo::AutoDataPartition(
  data = data,
  NumDataSets = 3,
  Ratios = c(0.7,0.2,0.1),
  PartitionType = "random",
  StratifyColumnNames = "Adrian",
  TimeColumnName = NULL)
TTrainData <- Sets$TrainData
VValidationData <- Sets$ValidationData
TTestData <- Sets$TestData
rm(Sets)

# Feature Colnames
Features <- names(TTrainData)[!names(TTrainData) %in% c("IDcol_1", "IDcol_2","Adrian","DateTime")]

# Run function
TestModel <- AutoQuant::AutoCatBoostMultiClass(
  
  # GPU or CPU and the number of available GPUs
  task_type = "GPU",
  NumGPUs = 1,
  
  # Metadata arguments
  OutputSelection = c("Importances", "EvalPlots", "EvalMetrics", "Score_TrainData"),
  ModelID = "Test_Model_1",
  model_path = normalizePath("./"),
  metadata_path = normalizePath("./"),
  SaveModelObjects = FALSE,
  ReturnModelObjects = TRUE,
  
  # Data arguments
  data = TTrainData,
  TrainOnFull = FALSE,
  ValidationData = VValidationData,
  TestData = TTestData,
  TargetColumnName = "Adrian",
  FeatureColNames = Features,
  PrimaryDateColumn = "DateTime",
  WeightsColumnName = "Weights",
  ClassWeights = c(1L,1L,1L,1L,1L),
  IDcols = c("IDcol_1","IDcol_2","DateTime"),
  
  # Model evaluation
  eval_metric = "MCC",
  loss_function = "MultiClassOneVsAll",
  grid_eval_metric = "Accuracy",
  MetricPeriods = 10L,
  
  # Grid tuning arguments
  PassInGrid = NULL,
  GridTune = FALSE,
  MaxModelsInGrid = 30L,
  MaxRunsWithoutNewWinner = 20L,
  MaxRunMinutes = 24L*60L,
  BaselineComparison = "default",
  
  # ML args
  Trees = 100L,
  Depth = 4L,
  LearningRate = 0.01,
  L2_Leaf_Reg = 1.0,
  RandomStrength = 1,
  BorderCount = 128,
  langevin = FALSE,
  diffusion_temperature = 10000,
  RSM = 0.80,
  BootStrapType = "Bayesian",
  GrowPolicy = "SymmetricTree",
  model_size_reg = 0.5,
  feature_border_type = "GreedyLogSum",
  sampling_unit = "Group",
  subsample = NULL,
  score_function = "Cosine",
  min_data_in_leaf = 1,
  DebugMode = TRUE)


# Legacy compatibility insights report.
# Multiclass remains here until a multiclass artifact-first replacement exists.
AutoQuant::ModelInsightsReport(
  TrainDataInclude = TRUE,
  FeatureColumnNames = Features,
  SampleSize = 100000,
  ModelObject = ModelObject,
  ModelID = 'Test_Model_1',
  OutputPath = getwd())


# Score data
Preds <- AutoQuant::AutoCatBoostScoring(
  TargetType = 'multiclass',
  ScoringData = data,
  FeatureColumnNames = Features,
  FactorLevelsList = TestModel$FactorLevelsList,
  IDcols = c("IDcol_1","IDcol_2","DateTime"),
  OneHot = FALSE,
  ReturnShapValues = FALSE,
  ModelObject = TestModel$Model,
  ModelPath = NULL,
  ModelID = 'Test_Model_1',
  ReturnFeatures = TRUE,
  MultiClassTargetLevels = TestModel$TargetLevels,
  TransformNumeric = FALSE,
  BackTransNumeric = FALSE,
  TargetColumnName = NULL,
  TransformationObject = NULL,
  TransID = NULL,
  TransPath = NULL,
  MDP_Impute = TRUE,
  MDP_CharToFactor = TRUE,
  MDP_RemoveDates = TRUE,
  MDP_MissFactor = '0',
  MDP_MissNum = -1,
  RemoveModel = FALSE)
```

</p>
</details>

</p>
</details>



<details><summary>AutoLightGBM__() Examples</summary>
<p>

<details><summary>AutoLightGBMRegression() Scoring Example</summary>
<p>

```r
# Refresh data
data <- AutoQuant::FakeDataGenerator(
  Correlation = 0.85,
  N = 25000L,
  ID = 2L,
  AddWeightsColumn = TRUE,
  ZIP = 0L,
  AddDate = TRUE,
  Classification = FALSE,
  MultiClass = FALSE)

# Partition Data
Sets <- Rodeo::AutoDataPartition(
  data = data,
  NumDataSets = 3,
  Ratios = c(0.7,0.2,0.1),
  PartitionType = "random",
  StratifyColumnNames = "Adrian",
  TimeColumnName = NULL)
TTrainData <- Sets$TrainData
VValidationData <- Sets$ValidationData
TTestData <- Sets$TestData
rm(Sets)

# Run function
TestModel <- AutoQuant::AutoLightGBMRegression(
  
  # GPU or CPU
  NThreads = parallel::detectCores(),
  
  # Metadata args
  OutputSelection = c("Importances","EvalPlots","EvalMetrics","Score_TrainData"),
  model_path = getwd(),
  metadata_path = getwd(),
  ModelID = "Test_Model_1",
  NumOfParDepPlots = 3L,
  EncodingMethod = "credibility",
  ReturnFactorLevels = TRUE,
  ReturnModelObjects = TRUE,
  SaveModelObjects = TRUE,
  SaveInfoToPDF = FALSE,
  DebugMode = TRUE,
  
  # Data args
  data = TTrainData,
  TrainOnFull = FALSE,
  ValidationData = VValidationData,
  TestData = TTestData,
  TargetColumnName = "Adrian",
  FeatureColNames = names(TTrainData)[!names(TTrainData) %in% c("IDcol_1", "IDcol_2","DateTime","Adrian")],
  PrimaryDateColumn = "DateTime",
  WeightsColumnName = "Weights",
  IDcols = c("IDcol_1","IDcol_2","DateTime"),
  TransformNumericColumns = NULL,
  Methods = c("Asinh","Asin","Log","LogPlus1","Sqrt","Logit"),
  
  # Grid parameters
  GridTune = FALSE,
  grid_eval_metric = "r2",
  BaselineComparison = "default",
  MaxModelsInGrid = 10L,
  MaxRunsWithoutNewWinner = 20L,
  MaxRunMinutes = 24L*60L,
  PassInGrid = NULL,
  
  # Core parameters
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#core-parameters
  input_model = NULL, # continue training a model that is stored to file
  task = "train",
  device_type = "CPU",
  objective = 'regression',
  metric = "rmse",
  boosting = "gbdt",
  LinearTree = FALSE,
  Trees = 50L,
  eta = NULL,
  num_leaves = 31,
  deterministic = TRUE,
  
  # Learning Parameters
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#learning-control-parameters
  force_col_wise = FALSE,
  force_row_wise = FALSE,
  max_depth = 6,
  min_data_in_leaf = 20,
  min_sum_hessian_in_leaf = 0.001,
  bagging_freq = 1.0,
  bagging_fraction = 1.0,
  feature_fraction = 1.0,
  feature_fraction_bynode = 1.0,
  lambda_l1 = 0.0,
  lambda_l2 = 0.0,
  extra_trees = FALSE,
  early_stopping_round = 10,
  first_metric_only = TRUE,
  max_delta_step = 0.0,
  linear_lambda = 0.0,
  min_gain_to_split = 0,
  drop_rate_dart = 0.10,
  max_drop_dart = 50,
  skip_drop_dart = 0.50,
  uniform_drop_dart = FALSE,
  top_rate_goss = FALSE,
  other_rate_goss = FALSE,
  monotone_constraints = NULL,
  monotone_constraints_method = "advanced",
  monotone_penalty = 0.0,
  forcedsplits_filename = NULL, # use for AutoStack option; .json file
  refit_decay_rate = 0.90,
  path_smooth = 0.0,
  
  # IO Dataset Parameters
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#io-parameters
  max_bin = 255,
  min_data_in_bin = 3,
  data_random_seed = 1,
  is_enable_sparse = TRUE,
  enable_bundle = TRUE,
  use_missing = TRUE,
  zero_as_missing = FALSE,
  two_round = FALSE,
  
  # Convert Parameters
  convert_model = NULL,
  convert_model_language = "cpp",
  
  # Objective Parameters
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#objective-parameters
  boost_from_average = TRUE,
  alpha = 0.90,
  fair_c = 1.0,
  poisson_max_delta_step = 0.70,
  tweedie_variance_power = 1.5,
  lambdarank_truncation_level = 30,
  
  # Metric Parameters (metric is in Core)
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#metric-parameters
  is_provide_training_metric = TRUE,
  eval_at = c(1,2,3,4,5),
  
  # Network Parameters
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#network-parameters
  num_machines = 1,
  
  # GPU Parameters
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#gpu-parameters
  gpu_platform_id = -1,
  gpu_device_id = -1,
  gpu_use_dp = TRUE,
  num_gpu = 1)

# Outcome
ModelID = "Test_Model_1"
colnames <- data.table::fread(file = file.path(getwd(), paste0(ModelID, "_ColNames.csv")))
Preds <- AutoQuant::AutoLightGBMScoring(
  TargetType = "regression",
  ScoringData = TTestData,
  ReturnShapValues = FALSE,
  FeatureColumnNames = colnames[[1L]],
  IDcols = c("IDcol_1","IDcol_2"),
  EncodingMethod = "credibility",
  FactorLevelsList = NULL,
  TargetLevels = NULL,
  ModelObject = NULL,
  ModelPath = getwd(),
  ModelID = "Test_Model_1",
  ReturnFeatures = TRUE,
  TransformNumeric = FALSE,
  BackTransNumeric = FALSE,
  TargetColumnName = NULL,
  TransformationObject = NULL,
  TransID = NULL,
  TransPath = NULL,
  MDP_Impute = TRUE,
  MDP_CharToFactor = TRUE,
  MDP_RemoveDates = TRUE,
  MDP_MissFactor = "0",
  MDP_MissNum = -1)
```

</p>
</details>

<details><summary>AutoLightGBMClassifier() Scoring Example</summary>
<p>

```r
# Refresh data
data <- AutoQuant::FakeDataGenerator(
  Correlation = 0.85,
  N = 25000L,
  ID = 2L,
  AddWeightsColumn = TRUE,
  ZIP = 0L,
  AddDate = TRUE,
  Classification = TRUE,
  MultiClass = FALSE)

# Partition Data
Sets <- Rodeo::AutoDataPartition(
  data = data,
  NumDataSets = 3,
  Ratios = c(0.7,0.2,0.1),
  PartitionType = "random",
  StratifyColumnNames = "Adrian",
  TimeColumnName = NULL)
TTrainData <- Sets$TrainData
VValidationData <- Sets$ValidationData
TTestData <- Sets$TestData
rm(Sets)

# Run function
TestModel <- AutoQuant::AutoLightGBMClassifier(
  
  # Multithreading
  NThreads = parallel::detectCores(),
  
  # Metadata args
  OutputSelection = c("Importances","EvalPlots","EvalMetrics","Score_TrainData"),
  model_path = normalizePath("./"),
  metadata_path = NULL,
  ModelID = "Test_Model_1",
  NumOfParDepPlots = 3L,
  EncodingMethod = "credibility",
  ReturnFactorLevels = TRUE,
  ReturnModelObjects = TRUE,
  SaveModelObjects = TRUE,
  SaveInfoToPDF = FALSE,
  DebugMode = TRUE,
  
  # Data args
  data = TTrainData,
  TrainOnFull = FALSE,
  ValidationData = VValidationData,
  TestData = TTestData,
  TargetColumnName = "Adrian",
  FeatureColNames = names(TTrainData)[!names(TTrainData) %in% c("IDcol_1", "IDcol_2","DateTime","Adrian")],
  PrimaryDateColumn = NULL,
  WeightsColumnName = "Weights",
  IDcols = c("IDcol_1","IDcol_2","DateTime"),
  CostMatrixWeights = c(1,0,0,1),
  
  # Grid parameters
  GridTune = FALSE,
  grid_eval_metric = "Utility",
  BaselineComparison = "default",
  MaxModelsInGrid = 10L,
  MaxRunsWithoutNewWinner = 20L,
  MaxRunMinutes = 24L*60L,
  PassInGrid = NULL,
  
  # Core parameters
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#core-parameters
  input_model = NULL, # continue training a model that is stored to file
  task = "train",
  device_type = "CPU",
  objective = 'binary',
  metric = 'binary_logloss',
  boosting = "gbdt",
  LinearTree = FALSE,
  Trees = 50L,
  eta = NULL,
  num_leaves = 31,
  deterministic = TRUE,
  
  # Learning Parameters
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#learning-control-parameters
  force_col_wise = FALSE,
  force_row_wise = FALSE,
  max_depth = 6,
  min_data_in_leaf = 20,
  min_sum_hessian_in_leaf = 0.001,
  bagging_freq = 1.0,
  bagging_fraction = 1.0,
  feature_fraction = 1.0,
  feature_fraction_bynode = 1.0,
  lambda_l1 = 0.0,
  lambda_l2 = 0.0,
  extra_trees = FALSE,
  early_stopping_round = 10,
  first_metric_only = TRUE,
  max_delta_step = 0.0,
  linear_lambda = 0.0,
  min_gain_to_split = 0,
  drop_rate_dart = 0.10,
  max_drop_dart = 50,
  skip_drop_dart = 0.50,
  uniform_drop_dart = FALSE,
  top_rate_goss = FALSE,
  other_rate_goss = FALSE,
  monotone_constraints = NULL,
  monotone_constraints_method = 'advanced',
  monotone_penalty = 0.0,
  forcedsplits_filename = NULL, # use for AutoStack option; .json file
  refit_decay_rate = 0.90,
  path_smooth = 0.0,
  
  # IO Dataset Parameters
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#io-parameters
  max_bin = 255,
  min_data_in_bin = 3,
  data_random_seed = 1,
  is_enable_sparse = TRUE,
  enable_bundle = TRUE,
  use_missing = TRUE,
  zero_as_missing = FALSE,
  two_round = FALSE,
  
  # Convert Parameters
  convert_model = NULL,
  convert_model_language = "cpp",
  
  # Objective Parameters
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#objective-parameters
  boost_from_average = TRUE,
  is_unbalance = FALSE,
  scale_pos_weight = 1.0,
  
  # Metric Parameters (metric is in Core)
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#metric-parameters
  is_provide_training_metric = TRUE,
  eval_at = c(1,2,3,4,5),
  
  # Network Parameters
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#network-parameters
  num_machines = 1,
  
  # GPU Parameters
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#gpu-parameters
  gpu_platform_id = -1,
  gpu_device_id = -1,
  gpu_use_dp = TRUE,
  num_gpu = 1)

# Outcome
ModelID = "Test_Model_1"
colnames <- data.table::fread(file = file.path(getwd(), paste0(ModelID, "_ColNames.csv")))
Preds <- AutoQuant::AutoLightGBMScoring(
  TargetType = "classification",
  ScoringData = TTestData,
  ReturnShapValues = FALSE,
  FeatureColumnNames = colnames[[1L]],
  IDcols = c("IDcol_1","IDcol_2","DateTime"),
  EncodingMethod = "credibility",
  FactorLevelsList = NULL,
  TargetLevels = NULL,
  ModelObject = NULL,
  ModelPath = getwd(),
  ModelID = "Test_Model_1",
  ReturnFeatures = TRUE,
  TransformNumeric = FALSE,
  BackTransNumeric = FALSE,
  TargetColumnName = NULL,
  TransformationObject = NULL,
  TransID = NULL,
  TransPath = NULL,
  MDP_Impute = TRUE,
  MDP_CharToFactor = TRUE,
  MDP_RemoveDates = TRUE,
  MDP_MissFactor = "0",
  MDP_MissNum = -1)
```

</p>
</details>

<details><summary>AutoLightGBMMultiClasss() Scoring Example</summary>
<p>

```r
# Refresh data
data <- AutoQuant::FakeDataGenerator(
  Correlation = 0.85,
  N = 25000L,
  ID = 2L,
  AddWeightsColumn = TRUE,
  ZIP = 0L,
  AddDate = TRUE,
  Classification = FALSE,
  MultiClass = TRUE)

# Partition Data
Sets <- Rodeo::AutoDataPartition(
  data = data,
  NumDataSets = 3,
  Ratios = c(0.7,0.2,0.1),
  PartitionType = "random",
  StratifyColumnNames = "Adrian",
  TimeColumnName = NULL)
TTrainData <- Sets$TrainData
VValidationData <- Sets$ValidationData
TTestData <- Sets$TestData
rm(Sets)

# Run function
TestModel <- AutoQuant::AutoLightGBMMultiClass(
  
  # GPU or CPU
  NThreads = parallel::detectCores(),
  
  # Metadata args
  OutputSelection = c("Importances","EvalPlots","EvalMetrics","Score_TrainData"),
  model_path = normalizePath("./"),
  metadata_path = NULL,
  ModelID = "Test_Model_1",
  NumOfParDepPlots = 3L,
  EncodingMethod = "credibility",
  ReturnFactorLevels = TRUE,
  ReturnModelObjects = TRUE,
  SaveModelObjects = TRUE,
  SaveInfoToPDF = FALSE,
  DebugMode = TRUE,
  
  # Data args
  data = TTrainData,
  TrainOnFull = FALSE,
  ValidationData = VValidationData,
  TestData = TTestData,
  TargetColumnName = "Adrian",
  FeatureColNames = names(TTrainData)[!names(TTrainData) %in% c("IDcol_1","IDcol_2","DateTime","Adrian")],
  PrimaryDateColumn = NULL,
  WeightsColumnName = "Weights",
  IDcols = c("IDcol_1","IDcol_2",'DateTime'),
  CostMatrixWeights = c(1,0,0,1),
  
  # Grid parameters
  GridTune = FALSE,
  grid_eval_metric = "microauc",
  BaselineComparison = "default",
  MaxModelsInGrid = 10L,
  MaxRunsWithoutNewWinner = 20L,
  MaxRunMinutes = 24L*60L,
  PassInGrid = NULL,
  
  # Core parameters
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#core-parameters
  input_model = NULL, # continue training a model that is stored to file
  task = "train",
  device_type = "CPU",
  objective = 'multiclass',
  multi_error_top_k = 1,
  metric = 'multiclass_logloss',
  boosting = "gbdt",
  LinearTree = FALSE,
  Trees = 50L,
  eta = NULL,
  num_leaves = 31,
  deterministic = TRUE,
  
  # Learning Parameters
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#learning-control-parameters
  force_col_wise = FALSE,
  force_row_wise = FALSE,
  max_depth = 6,
  min_data_in_leaf = 20,
  min_sum_hessian_in_leaf = 0.001,
  bagging_freq = 1.0,
  bagging_fraction = 1.0,
  feature_fraction = 1.0,
  feature_fraction_bynode = 1.0,
  lambda_l1 = 0.0,
  lambda_l2 = 0.0,
  extra_trees = FALSE,
  early_stopping_round = 10,
  first_metric_only = TRUE,
  max_delta_step = 0.0,
  linear_lambda = 0.0,
  min_gain_to_split = 0,
  drop_rate_dart = 0.10,
  max_drop_dart = 50,
  skip_drop_dart = 0.50,
  uniform_drop_dart = FALSE,
  top_rate_goss = FALSE,
  other_rate_goss = FALSE,
  monotone_constraints = NULL,
  monotone_constraints_method = 'advanced',
  monotone_penalty = 0.0,
  forcedsplits_filename = NULL, # use for AutoStack option; .json file
  refit_decay_rate = 0.90,
  path_smooth = 0.0,
  
  # IO Dataset Parameters
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#io-parameters
  max_bin = 255,
  min_data_in_bin = 3,
  data_random_seed = 1,
  is_enable_sparse = TRUE,
  enable_bundle = TRUE,
  use_missing = TRUE,
  zero_as_missing = FALSE,
  two_round = FALSE,
  
  # Convert Parameters
  convert_model = NULL,
  convert_model_language = "cpp",
  
  # Objective Parameters
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#objective-parameters
  boost_from_average = TRUE,
  is_unbalance = FALSE,
  scale_pos_weight = 1.0,
  
  # Metric Parameters (metric is in Core)
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#metric-parameters
  is_provide_training_metric = TRUE,
  eval_at = c(1,2,3,4,5),
  
  # Network Parameters
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#network-parameters
  num_machines = 1,
  
  # GPU Parameters
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#gpu-parameters
  gpu_platform_id = -1,
  gpu_device_id = -1,
  gpu_use_dp = TRUE,
  num_gpu = 1)

# Outcome
ModelID = "Test_Model_1"
colnames <- data.table::fread(file = file.path(getwd(), paste0(ModelID, "_ColNames.csv")))
Preds <- AutoQuant::AutoLightGBMScoring(
  TargetType = "multiclass",
  ScoringData = TTestData,
  ReturnShapValues = FALSE,
  FeatureColumnNames = colnames[[1L]],
  IDcols = c("IDcol_1","IDcol_2","DateTime"),
  EncodingMethod = "credibility",
  FactorLevelsList = NULL,
  TargetLevels = NULL,
  ModelObject = NULL,
  ModelPath = getwd(),
  ModelID = "Test_Model_1",
  ReturnFeatures = TRUE,
  TransformNumeric = FALSE,
  BackTransNumeric = FALSE,
  TargetColumnName = NULL,
  TransformationObject = NULL,
  TransID = NULL,
  TransPath = NULL,
  MDP_Impute = TRUE,
  MDP_CharToFactor = TRUE,
  MDP_RemoveDates = TRUE,
  MDP_MissFactor = "0",
  MDP_MissNum = -1)
```

</p>
</details>



</p>
</details>


<details><summary>AutoXGBoost__() Examples</summary>
<p>

<details><summary>AutoXGBoostRegression() Scoring Example</summary>
<p>

```r
# Refresh data
data <- AutoQuant::FakeDataGenerator(
  Correlation = 0.85,
  N = 25000L,
  ID = 2L,
  FactorCount = 3,
  AddWeightsColumn = TRUE,
  ZIP = 0L,
  AddDate = TRUE,
  Classification = FALSE,
  MultiClass = FALSE)

# Copy data
data1 <- data.table::copy(data)

# Partition Data
Sets <- Rodeo::AutoDataPartition(
  data = data1,
  NumDataSets = 3,
  Ratios = c(0.7,0.2,0.1),
  PartitionType = "random",
  StratifyColumnNames = "Adrian",
  TimeColumnName = NULL)
TTrainData <- Sets$TrainData
VValidationData <- Sets$ValidationData
TTestData <- Sets$TestData
rm(Sets)

# Run function
TestModel <- AutoQuant::AutoXGBoostRegression(
  
  # GPU or CPU
  TreeMethod = "hist",
  NThreads = parallel::detectCores(),
  LossFunction = 'reg:squarederror',
  
  # Metadata arguments
  model_path = normalizePath("./"),
  metadata_path = NULL,
  ModelID = "Test_Model_1",
  EncodingMethod = "credibility",
  ReturnFactorLevels = TRUE,
  ReturnModelObjects = TRUE,
  SaveModelObjects = TRUE,
  DebugMode = TRUE,
  
  # Data arguments
  data = TTrainData,
  TrainOnFull = FALSE,
  ValidationData = VValidationData,
  TestData = TTestData,
  TargetColumnName = "Adrian",
  FeatureColNames = names(TTrainData)[!names(TTrainData) %in% c("IDcol_1", "IDcol_2","DateTime","Adrian")],
  WeightsColumnName = "Weights",
  IDcols = c("IDcol_1","IDcol_2","DateTime"),
  TransformNumericColumns = NULL,
  Methods = c("Asinh", "Asin", "Log", "LogPlus1", "Sqrt", "Logit"),
  
  # Model evaluation
  eval_metric = "rmse",
  NumOfParDepPlots = 3L,
  
  # Grid tuning arguments
  PassInGrid = NULL,
  GridTune = FALSE,
  grid_eval_metric = "r2",
  BaselineComparison = "default",
  MaxModelsInGrid = 10L,
  MaxRunsWithoutNewWinner = 20L,
  MaxRunMinutes = 24L*60L,
  Verbose = 1L,
  SaveInfoToPDF = TRUE,
  
  # ML args
  Trees = 50L,
  eta = 0.05,
  max_depth = 4L,
  min_child_weight = 1.0,
  subsample = 0.55,
  colsample_bytree = 0.55)

# Score model
Preds <- AutoQuant::AutoXGBoostScoring(
  TargetType = "regression",
  ScoringData = data,
  ReturnShapValues = FALSE,
  FeatureColumnNames = names(TTrainData)[!names(TTrainData) %in% c("IDcol_1", "IDcol_2","DateTime","Adrian")],
  IDcols = c("IDcol_1","IDcol_2","DateTime"),
  EncodingMethod = "credibility",
  FactorLevelsList = TestModel$FactorLevelsList,
  TargetLevels = NULL,
  ModelObject = TestModel$Model,
  ModelPath = "home",
  ModelID = "ModelTest",
  ReturnFeatures = TRUE,
  TransformNumeric = FALSE,
  BackTransNumeric = FALSE,
  TargetColumnName = NULL,
  TransformationObject = NULL,
  TransID = NULL,
  TransPath = NULL,
  MDP_Impute = TRUE,
  MDP_CharToFactor = TRUE,
  MDP_RemoveDates = TRUE,
  MDP_MissFactor = "0",
  MDP_MissNum = -1)
```

</p>
</details>

<details><summary>AutoXGBoostClassifier() Scoring Example</summary>
<p>

```r
# Refresh data
data <- AutoQuant::FakeDataGenerator(
  Correlation = 0.85,
  N = 25000L,
  ID = 2L,
  AddWeightsColumn = TRUE,
  ZIP = 0L,
  AddDate = TRUE,
  Classification = TRUE,
  MultiClass = FALSE)

# Partition Data
Sets <- Rodeo::AutoDataPartition(
  data = data,
  NumDataSets = 3,
  Ratios = c(0.7,0.2,0.1),
  PartitionType = "random",
  StratifyColumnNames = "Adrian",
  TimeColumnName = NULL)
TTrainData <- Sets$TrainData
VValidationData <- Sets$ValidationData
TTestData <- Sets$TestData
rm(Sets)

# Run function
TestModel <- AutoQuant::AutoXGBoostClassifier(
  
  # GPU or CPU
  TreeMethod = "hist",
  NThreads = parallel::detectCores(),
  
  # Metadata arguments
  OutputSelection = c("Importances", "EvalPlots", "EvalMetrics", "Score_TrainData"),
  model_path = normalizePath("./"),
  metadata_path = NULL,
  ModelID = "Test_Model_1",
  EncodingMethod = "credibility",
  ReturnFactorLevels = TRUE,
  ReturnModelObjects = TRUE,
  SaveModelObjects = TRUE,
  SaveInfoToPDF = TRUE,
  DebugMode = TRUE,
  
  # Data arguments
  data = TTrainData,
  TrainOnFull = FALSE,
  ValidationData = VValidationData,
  TestData = TTestData,
  TargetColumnName = "Adrian",
  FeatureColNames = names(TTrainData)[!names(TTrainData) %in% c("IDcol_1", "IDcol_2","DateTime","Adrian")],
  WeightsColumnName = "Weights",
  IDcols = c("IDcol_1","IDcol_2","DateTime"),
  
  # Model evaluation
  LossFunction = 'reg:logistic',
  eval_metric = "auc",
  grid_eval_metric = "MCC",
  CostMatrixWeights = c(1,0,0,1),
  NumOfParDepPlots = 3L,
  
  # Grid tuning arguments
  PassInGrid = NULL,
  GridTune = FALSE,
  BaselineComparison = "default",
  MaxModelsInGrid = 10L,
  MaxRunsWithoutNewWinner = 20L,
  MaxRunMinutes = 24L*60L,
  Verbose = 1L,
  
  # ML Args
  Trees = 50L,
  eta = 0.05,
  max_depth = 4L,
  min_child_weight = 1.0,
  subsample = 0.55,
  colsample_bytree = 0.55)

# Score model
Preds <- AutoQuant::AutoXGBoostScoring(
  TargetType = "classifier",
  ScoringData = data,
  ReturnShapValues = FALSE,
  FeatureColumnNames = names(TTrainData)[!names(TTrainData) %in% c("IDcol_1", "IDcol_2","DateTime","Adrian")],
  IDcols = c("IDcol_1","IDcol_2","DateTime"),
  EncodingMethod = "credibility",
  FactorLevelsList = TestModel$FactorLevelsList,
  TargetLevels = NULL,
  ModelObject = TestModel$Model,
  ModelPath = "home",
  ModelID = "ModelTest",
  ReturnFeatures = TRUE,
  TransformNumeric = FALSE,
  BackTransNumeric = FALSE,
  TargetColumnName = NULL,
  TransformationObject = NULL,
  TransID = NULL,
  TransPath = NULL,
  MDP_Impute = TRUE,
  MDP_CharToFactor = TRUE,
  MDP_RemoveDates = TRUE,
  MDP_MissFactor = "0",
  MDP_MissNum = -1)
```

</p>
</details>

<details><summary>AutoXGBoostMultiClasss() Scoring Example</summary>
<p>

```r
# Refresh data
data <- AutoQuant::FakeDataGenerator(
  Correlation = 0.85,
  N = 25000L,
  ID = 2L,
  AddWeightsColumn = TRUE,
  ZIP = 0L,
  AddDate = TRUE,
  Classification = FALSE,
  MultiClass = TRUE)

# Partition Data
Sets <- Rodeo::AutoDataPartition(
  data = data,
  NumDataSets = 3,
  Ratios = c(0.7,0.2,0.1),
  PartitionType = "random",
  StratifyColumnNames = "Adrian",
  TimeColumnName = NULL)
TTrainData <- Sets$TrainData
VValidationData <- Sets$ValidationData
TTestData <- Sets$TestData
rm(Sets)

# Run function
TestModel <- AutoQuant::AutoXGBoostMultiClass(
  
  # GPU or CPU
  TreeMethod = "hist",
  NThreads = parallel::detectCores(),
  
  # Metadata arguments
  OutputSelection = c("Importances", "EvalPlots", "EvalMetrics", "Score_TrainData"),
  model_path = normalizePath("./"),
  metadata_path = normalizePath("./"),
  ModelID = "Test_Model_1",
  ReturnFactorLevels = TRUE,
  ReturnModelObjects = TRUE,
  SaveModelObjects = FALSE,
  EncodingMethod = "credibility",
  DebugMode = TRUE,
  
  # Data arguments
  data = TTrainData,
  TrainOnFull = FALSE,
  ValidationData = VValidationData,
  TestData = TTestData,
  TargetColumnName = "Adrian",
  FeatureColNames = names(TTrainData)[!names(TTrainData) %in% c("IDcol_1", "IDcol_2","DateTime","Adrian")],
  IDcols = c("IDcol_1","IDcol_2","DateTime"),
  
  # Model evaluation
  eval_metric = "merror",
  LossFunction = 'multi:softprob',
  grid_eval_metric = "accuracy",
  NumOfParDepPlots = 3L,
  
  # Grid tuning arguments
  PassInGrid = NULL,
  GridTune = FALSE,
  BaselineComparison = "default",
  MaxModelsInGrid = 10L,
  MaxRunsWithoutNewWinner = 20L,
  MaxRunMinutes = 24L*60L,
  Verbose = 1L,
  
  # ML Args
  Trees = 50L,
  eta = 0.05,
  max_depth = 4L,
  min_child_weight = 1.0,
  subsample = 0.55,
  colsample_bytree = 0.55)

# Score model
Preds <- AutoQuant::AutoXGBoostScoring(
  TargetType = "multiclass",
  ScoringData = data,
  ReturnShapValues = FALSE,
  FeatureColumnNames = names(TTrainData)[!names(TTrainData) %in% c("IDcol_1", "IDcol_2","DateTime","Adrian")],
  IDcols = c("IDcol_1","IDcol_2","DateTime"),
  EncodingMethod = "credibility",
  FactorLevelsList = TestModel$FactorLevelsList,
  TargetLevels = TestModel$TargetLevels,
  ModelObject = TestModel$Model,
  ModelPath = NULL,
  ModelID = "ModelTest",
  ReturnFeatures = TRUE,
  TransformNumeric = FALSE,
  BackTransNumeric = FALSE,
  TargetColumnName = NULL,
  TransformationObject = NULL,
  TransID = NULL,
  TransPath = NULL,
  MDP_Impute = TRUE,
  MDP_CharToFactor = TRUE,
  MDP_RemoveDates = TRUE,
  MDP_MissFactor = "0",
  MDP_MissNum = -1)
```

</p>
</details>


</p>
</details>



</p>
</details>

<br>

## Model Evaluation <img src="https://raw.githubusercontent.com/AdrianAntico/AutoQuant/master/Images/ModelEvaluationImage.png" align="right" width="80" />
<details><summary>Expand to view content</summary>
<p>

`ModelInsightsReport()` is a legacy compatibility wrapper for older supervised-learning examples. New regression workflows should use `generate_regression_model_insights_artifacts()` plus `RegressionModelInsightsReport()`. New binary classification workflows should use `generate_binary_classification_model_insights_artifacts()` plus `BinaryClassificationModelInsightsReport()`. Multiclass remains on the legacy wrapper until a multiclass artifact generator/report pair exists.

<img src="https://github.com/AdrianAntico/AutoQuant/blob/master/Images/MLReports.png?raw=true" align="center" width="800" />

<details><summary>RegressionModelInsightsReport() Example</summary>
<p>

```r
set.seed(8675309)

library(data.table)
library(AutoQuant)

# ----------------------------
# 1. Create fake data
# ----------------------------

n <- 5000

dates <- seq.Date(
  from = as.Date("2023-01-01"),
  to   = as.Date("2025-12-31"),
  by   = "day"
)

dt <- data.table(
  id = seq_len(n),

  # Date / trend fields
  event_date = sample(dates, n, replace = TRUE),

  # Grouping fields
  channel = sample(
    c("Search", "Social", "Email", "Direct", "Affiliate"),
    n,
    replace = TRUE,
    prob = c(0.34, 0.24, 0.16, 0.18, 0.08)
  ),

  region = sample(
    c("West", "South", "Midwest", "Northeast"),
    n,
    replace = TRUE,
    prob = c(0.35, 0.28, 0.20, 0.17)
  ),

  customer_segment = sample(
    c("Budget", "Standard", "Premium", "Enterprise"),
    n,
    replace = TRUE,
    prob = c(0.35, 0.40, 0.20, 0.05)
  )
)

# Date features for synthetic signal
dt[, month_num := as.integer(format(event_date, "%m"))]
dt[, year_num  := as.integer(format(event_date, "%Y"))]
dt[, day_index := as.integer(event_date - min(event_date))]

# Base effects
channel_effect <- c(
  Search    = 1.20,
  Social    = 0.85,
  Email     = 1.35,
  Direct    = 1.00,
  Affiliate = 0.65
)

segment_effect <- c(
  Budget     = 0.70,
  Standard   = 1.00,
  Premium    = 1.55,
  Enterprise = 2.40
)

region_effect <- c(
  West      = 1.10,
  South     = 0.95,
  Midwest   = 0.85,
  Northeast = 1.05
)

# Seasonality and trend
dt[, seasonality := 1 + 0.25 * sin(2 * pi * month_num / 12)]
dt[, trend       := 1 + day_index / max(day_index) * 0.35]

# Numeric variables with structure
dt[, impressions := round(
  rgamma(.N, shape = 8, scale = 1200) *
    channel_effect[channel] *
    seasonality *
    trend
)]

dt[, clicks := round(
  impressions * pmin(
    pmax(rnorm(.N, mean = 0.035, sd = 0.012), 0.002),
    0.12
  )
)]

dt[, spend := round(
  impressions * runif(.N, 0.008, 0.025) *
    channel_effect[channel] *
    region_effect[region],
  2
)]

dt[, conversions := rpois(
  .N,
  lambda = pmax(
    clicks *
      runif(.N, 0.025, 0.12) *
      segment_effect[customer_segment] *
      region_effect[region],
    0.01
  )
)]

dt[, revenue := round(
  conversions *
    rgamma(.N, shape = 4, scale = 90) *
    segment_effect[customer_segment] *
    runif(.N, 0.75, 1.35),
  2
)]

dt[, ctr := clicks / pmax(impressions, 1)]
dt[, cvr := conversions / pmax(clicks, 1)]
dt[, cpc := spend / pmax(clicks, 1)]
dt[, roas := revenue / pmax(spend, 1)]

# Some less-behaved variables for QA
dt[, noise_normal := rnorm(.N, mean = 100, sd = 15)]
dt[, skewed_score := rgamma(.N, shape = 2, scale = 20)]
dt[, binary_flag := rbinom(.N, size = 1, prob = 0.28)]

# Deliberate correlated variables
dt[, spend_lag_proxy := spend * runif(.N, 0.85, 1.15) + rnorm(.N, 0, 25)]
dt[, revenue_proxy   := revenue * runif(.N, 0.90, 1.10) + rnorm(.N, 0, 100)]

# Deliberate outliers
outlier_rows <- sample(seq_len(n), size = 35)
dt[outlier_rows, spend := spend * runif(.N, 4, 9)]
dt[outlier_rows, revenue := revenue * runif(.N, 3, 7)]
dt[outlier_rows, impressions := round(impressions * runif(.N, 2, 5))]

# Deliberate missingness
dt[sample(.N, 150), revenue := NA_real_]
dt[sample(.N, 120), cpc := NA_real_]
dt[sample(.N, 90), customer_segment := NA_character_]
dt[sample(.N, 80), ctr := NA_real_]

# Deliberate zero-heavy variable
dt[, zero_heavy_metric := fifelse(
  runif(.N) < 0.72,
  0,
  round(rgamma(.N, shape = 2, scale = 15), 2)
)]

# Clean helper columns if you do not want them in report
# Keeping them can also help QA univariate behavior.
# dt[, c("month_num", "year_num", "day_index", "seasonality", "trend") := NULL]


# ----------------------------
# 2. Define report inputs
# ----------------------------

UnivariateVars <- c(
  "impressions",
  "clicks",
  "spend",
  "conversions",
  "revenue",
  "ctr",
  "cvr",
  "cpc",
  "roas",
  "noise_normal",
  "skewed_score",
  "binary_flag",
  "zero_heavy_metric",
  "channel",
  "region",
  "customer_segment"
)

CorrVars <- c(
  "impressions",
  "clicks",
  "spend",
  "spend_lag_proxy",
  "conversions",
  "revenue",
  "revenue_proxy",
  "ctr",
  "cvr",
  "cpc",
  "roas",
  "noise_normal",
  "skewed_score",
  "zero_heavy_metric"
)

TrendVars <- c(
  "impressions",
  "clicks",
  "spend",
  "conversions",
  "revenue",
  "ctr",
  "cvr",
  "cpc",
  "roas"
)

OutputPath <- getwd()


# ----------------------------
# 4. Build Catboost model
# ----------------------------

target <- "conversions"
features <- c(
  "impressions", "clicks", "spend",
  "seasonality", "trend",
  "channel", "region", "customer_segment"
)

id_cols <- setdiff(names(dt), c(target, features))

# Run function
ModelObject <- AutoQuant::AutoCatBoostRegression(

  # GPU or CPU and the number of available GPUs
  task_type = 'GPU',
  NumGPUs = 1,
  NumOfParDepPlots = 0,

  # Metadata args
  OutputSelection = c('Importances','EvalMetrics','Score_TrainData'),
  ModelID = 'Test_Model_1',
  model_path = getwd(),
  metadata_path = getwd(),
  ReturnModelObjects = TRUE,

  # Data args
  data = dt,
  TargetColumnName = "conversions",
  FeatureColNames = features,
  IDcols = id_cols,
  TransformNumericColumns = NULL,
  Methods = c('Asinh','Asin','Log','LogPlus1','Sqrt','Logit'),
  Trees = 200
)

# ----------------------------
# 5. Run artifact generator
# ----------------------------

ModelInsightsArtifacts <- generate_regression_model_insights_artifacts(

  # Model object from AutoCatBoostRegression()
  ModelObject = ModelObject,

  # Important for CatBoost-specific importance / interaction handling
  Algo = "catboost",

  # Explicitly pass these so the generator does not have to infer
  TargetColumnName = target,
  PredictionColumnName = "Predict",
  FeatureColumnNames = features,

  # Good candidates for segment and Simpson's Paradox / by-variable checks
  SegmentVars = c("channel", "region", "customer_segment"),
  ByVars = c("channel", "region", "customer_segment"),

  # Optional, only use this if one of your columns is truly a date/time var
  DateVar = NULL,

  # Optional ID columns retained in top-error tables
  IDColumn = if (length(id_cols) > 0L) id_cols[1L] else NULL,

  # Visual theme
  Theme = "dark",

  # First run: keep everything dynamic, no exports
  OutputPath = NULL
)

ReportPath <- RegressionModelInsightsReport(
  artifacts = ModelInsightsArtifacts,
  OutputPath = getwd(),
  OutputFile = "Regression_ModelInsights_Report.html",
  Quiet = FALSE
)

```

</p>
</details>

<details><summary>Regression SHAP Artifact Generator Example</summary>
<p>

`generate_regression_shap_analysis_artifacts()` consumes precomputed `Shap_` columns from AutoQuant modeling/scoring outputs. It does not compute SHAP values, call `predict()`, require a model object, or use a SHAP backend package.

```r
library(data.table)
library(AutoQuant)

set.seed(123)

# Purpose-built regression QA dataset
n <- 15000L

dt <- data.table(
  IDcol_1 = seq_len(n),
  IDcol_2 = sample(100000:999999, n, TRUE),
  Date = as.Date("2023-01-01") + sample(0:540, n, TRUE),
  
  Channel = sample(c("Search", "Social", "Email", "Direct", "Affiliate"), n, TRUE,
                   prob = c(0.30, 0.24, 0.18, 0.18, 0.10)),
  Region = sample(c("West", "Midwest", "South", "Northeast"), n, TRUE),
  CustomerTier = sample(c("Bronze", "Silver", "Gold", "Platinum"), n, TRUE,
                        prob = c(0.42, 0.30, 0.20, 0.08)),
  
  Spend = round(rgamma(n, shape = 4, scale = 60), 2),
  Clicks = rpois(n, lambda = 40),
  Impressions = rpois(n, lambda = 1800),
  TenureMonths = sample(1:72, n, TRUE),
  DiscountRate = pmin(pmax(rbeta(n, 2, 8), 0), 0.75),
  CompetitorIndex = rnorm(n, 100, 15),
  Seasonality = sin(2 * pi * as.integer(format(as.Date("2023-01-01") + sample(0:540, n, TRUE), "%j")) / 365)
)

# Add correlated / engineered features
dt[, CTR := Clicks / pmax(Impressions, 1)]
dt[, SpendPerClick := Spend / pmax(Clicks, 1)]
dt[, IsHolidaySeason := fifelse(format(Date, "%m") %in% c("11", "12"), "Yes", "No")]

# Segment effects
dt[, ChannelEffect := fifelse(Channel == "Search", 45,
                              fifelse(Channel == "Social", 25,
                                      fifelse(Channel == "Email", 18,
                                              fifelse(Channel == "Affiliate", -10, 5))))]

dt[, RegionEffect := fifelse(Region == "West", 20,
                             fifelse(Region == "South", -12,
                                     fifelse(Region == "Northeast", 8, 0)))]

dt[, TierEffect := fifelse(CustomerTier == "Platinum", 80,
                           fifelse(CustomerTier == "Gold", 45,
                                   fifelse(CustomerTier == "Silver", 20, 0)))]

# Explicit nonlinear and interaction-like signal for model to learn
dt[, Spend_Channel_Interaction := fifelse(Channel == "Search", Spend * 0.12,
                                          fifelse(Channel == "Social", Spend * 0.08,
                                                  fifelse(Channel == "Affiliate", -Spend * 0.04, Spend * 0.03)))]

dt[, Discount_Tier_Interaction := fifelse(CustomerTier %in% c("Gold", "Platinum"),
                                          DiscountRate * 130,
                                          -DiscountRate * 35)]

dt[, Time_Channel_Interaction := fifelse(Channel == "Email", Seasonality * 35,
                                         fifelse(Channel == "Search", Seasonality * 22, Seasonality * 8))]

# Target
dt[, Adrian :=
     120 +
     0.55 * Spend +
     1.80 * Clicks +
     0.015 * Impressions +
     18 * log1p(TenureMonths) -
     65 * DiscountRate -
     0.30 * CompetitorIndex +
     ChannelEffect +
     RegionEffect +
     TierEffect +
     Spend_Channel_Interaction +
     Discount_Tier_Interaction +
     Time_Channel_Interaction +
     rnorm(.N, 0, 35)
]

# Keep target positive-ish for transforms if desired
dt[, Adrian := pmax(Adrian, 1)]

FeatureCols <- setdiff(
  names(dt),
  c(
    "IDcol_1", "IDcol_2", "Adrian",
    "ChannelEffect", "RegionEffect", "TierEffect"
  )
)

# Run model with SHAP
TestModel <- AutoQuant::AutoCatBoostRegression(
  ReturnShap = TRUE,
  
  TrainOnFull = FALSE,
  task_type = "GPU",
  NumGPUs = 1,
  DebugMode = FALSE,
  
  OutputSelection = c(
    "Importances",
    "EvalPlots",
    "EvalMetrics",
    "Score_TrainData"
  ),
  ModelID = "Regression_SHAP_QA_Model",
  model_path = normalizePath("./"),
  metadata_path = normalizePath("./"),
  SaveModelObjects = FALSE,
  SaveInfoToPDF = FALSE,
  ReturnModelObjects = TRUE,
  
  data = dt,
  ValidationData = NULL,
  TestData = NULL,
  TargetColumnName = "Adrian",
  FeatureColNames = FeatureCols,
  PrimaryDateColumn = "Date",
  WeightsColumnName = NULL,
  IDcols = c("IDcol_1", "IDcol_2"),
  TransformNumericColumns = "Adrian",
  Methods = c("BoxCox", "Asinh", "Asin", "Log", "LogPlus1", "Sqrt", "Logit"),
  
  eval_metric = "RMSE",
  eval_metric_value = 1.5,
  loss_function = "RMSE",
  loss_function_value = 1.5,
  MetricPeriods = 10L,
  NumOfParDepPlots = min(length(FeatureCols), 20L),
  
  PassInGrid = NULL,
  GridTune = FALSE,
  MaxModelsInGrid = 30L,
  MaxRunsWithoutNewWinner = 20L,
  MaxRunMinutes = 60 * 60,
  BaselineComparison = "default",
  
  langevin = FALSE,
  diffusion_temperature = 10000,
  Trees = 700,
  Depth = 8,
  L2_Leaf_Reg = NULL,
  RandomStrength = 1,
  BorderCount = 128,
  LearningRate = NULL,
  RSM = 1,
  BootStrapType = NULL,
  GrowPolicy = "SymmetricTree",
  model_size_reg = 0.5,
  feature_border_type = "GreedyLogSum",
  sampling_unit = "Object",
  subsample = NULL,
  score_function = "Cosine",
  min_data_in_leaf = 1
)

scored_dt <- data.table::rbindlist(
  list(TestModel$TrainData, TestModel$TestData),
  use.names = TRUE,
  fill = TRUE
)

# Generate all currently supported Regression SHAP artifacts
ShapArtifacts <- AutoQuant::generate_regression_shap_analysis_artifacts(
  data = scored_dt,
  target_col = "Adrian",
  prediction_col = "Predict",
  feature_cols = FeatureCols,
  shap_prefix = "Shap_",
  
  id_cols = c("IDcol_1", "IDcol_2"),
  model_name = "Regression_SHAP_QA_Model",
  data_name = "Purpose-built SHAP QA dataset",
  
  DateVar = "Date",
  date_aggregation = "month",
  
  ByVars = c("Channel", "Region", "CustomerTier", "IsHolidaySeason"),
  
  selected_features = c(
    "Spend",
    "Clicks",
    "Impressions",
    "CTR",
    "SpendPerClick",
    "TenureMonths",
    "DiscountRate",
    "CompetitorIndex",
    "Seasonality",
    "Channel",
    "Region",
    "CustomerTier",
    "IsHolidaySeason",
    "Spend_Channel_Interaction",
    "Discount_Tier_Interaction",
    "Time_Channel_Interaction"
  ),
  
  local_row_ids = c(1L, 25L, 100L, 500L, 1000L),
  
  top_n = 25L,
  max_dependence_rows = 7000L,
  max_segment_levels = 20L,
  max_byvars = 4L,
  
  include_dependence = TRUE,
  include_segments = TRUE,
  include_time = TRUE,
  include_local = TRUE,
  include_interactions = TRUE,
  
  include_plots = TRUE,
  max_feature_effect_plots = 12L,
  max_dependence_plots = 12L,
  max_segment_plots = 4L,
  max_time_plots = 4L,
  max_local_plots = 5L,
  plot_top_n = 15L
)

# Quick artifact inventory
artifact_inventory <- rbindlist(lapply(names(ShapArtifacts$artifacts), function(nm) {
  a <- ShapArtifacts$artifacts[[nm]]
  data.table(
    artifact = nm,
    label = a$label,
    type = a$type,
    section = a$section,
    lens = a$metadata$lens %||% NA_character_,
    plot_type = a$metadata$plot_type %||% NA_character_
  )
}), fill = TRUE)

print(artifact_inventory)

# Preferred: render the existing artifact result without recomputing artifacts
reg_artifacts <- ShapArtifacts
ReportPath <- AutoQuant::RegressionShapAnalysisReport(
  artifact_result = reg_artifacts,
  OutputPath = normalizePath("./"),
  OutputFile = "regression_shap_analysis_full_qa_report.html",
  Title = "Regression SHAP Analysis Full QA Report",
  Open = TRUE,
  Quiet = FALSE
)

# Convenience wrapper: pass analytical arguments through ...
ConvenienceReportPath <- AutoQuant::RegressionShapAnalysisReport(
  data = scored_dt,
  OutputPath = normalizePath("./"),
  OutputFile = "regression_shap_analysis_convenience_report.html",
  Title = "Regression SHAP Analysis Convenience Report",
  target_col = "Adrian",
  prediction_col = "Predict",
  feature_cols = FeatureCols,
  id_cols = c("IDcol_1", "IDcol_2"),
  model_name = "Regression_SHAP_QA_Model",
  data_name = "Purpose-built SHAP QA dataset",
  
  DateVar = "Date",
  date_aggregation = "month",
  ByVars = c("Channel", "Region", "CustomerTier", "IsHolidaySeason"),
  selected_features = FeatureCols,
  local_row_ids = c(1L, 25L, 100L, 500L, 1000L),
  top_n = 25L,
  
  include_dependence = TRUE,
  include_segments = TRUE,
  include_time = TRUE,
  include_local = TRUE,
  include_interactions = TRUE,

  Open = TRUE,
  Quiet = FALSE
)
```

The generator-first workflow is preferred: use `generate_regression_shap_analysis_artifacts()` for analytical options, then pass the result to `RegressionShapAnalysisReport(artifact_result = reg_artifacts, ...)`. The report function is a renderer. It can still call the generator as a convenience wrapper by passing `data` plus analytical arguments through `...`, but `artifact_result` mode avoids recomputing artifacts.

The `Shap_` prefix maps each contribution column to the source model variable by stripping the prefix, such as `Shap_Impressions` -> `Impressions`. ID, segment, date, target, and prediction columns may be present, but they are not treated as SHAP features unless they have matching `Shap_` columns. When `include_plots = TRUE`, plot artifacts are created with AutoPlots high-level functions. Segment heatmaps use signed mean SHAP. Interaction diagnostics are optional binned/leveled candidate interaction surfaces from ordinary `Shap_` columns; axes are actual source feature value bins/levels and heatmap values are signed mean SHAP for the attributed feature. Pairwise interaction diagnostics are canonical unordered pairs, so A x B and B x A are treated as the same non-directional analytical object. They are not exact SHAP interaction value decompositions.

Missing or insufficient interaction inputs do not fail SHAP generation. When interaction analysis is requested but candidate pair columns, source feature columns, usable SHAP columns, rows, or unique value combinations are unavailable, AutoQuant emits an `interaction_diagnostics` table with `status`, `reason_code`, `reason`, `severity`, required/available columns, and a recommendation, then skips ranking, surface, and heatmap artifacts. Effect-curve generation is independent of interaction analysis; effect-curves-only runs do not attempt interaction generation.

</p>
</details>

<details><summary>Binary Classification Model Insights Artifact Generator Example</summary>
<p>

`generate_binary_classification_model_insights_artifacts()` consumes scored binary classification output data. It does not require a full model object. The generator creates structured plot/table artifacts for threshold diagnostics, ROC / PR, calibration, lift/gains, prediction distribution, feature effects, segment diagnostics, and time diagnostics. Native report rendering should use the artifact result through `BinaryClassificationModelInsightsReport()`.

```r
library(data.table)
library(AutoQuant)

set.seed(321)
n <- 1000L

dt <- data.table(
  Date = as.Date("2025-01-01") + sample(0:240, n, TRUE),
  Channel = sample(c("Direct", "Search", "Social", "Email"), n, TRUE),
  Region = sample(c("West", "Midwest", "South", "Northeast"), n, TRUE),
  Spend = rgamma(n, shape = 5, scale = 40),
  Clicks = rpois(n, lambda = 60),
  CustomerTier = sample(c("Bronze", "Silver", "Gold", "Platinum"), n, TRUE)
)

dt[, score_linear :=
  -1.2 +
  0.008 * Spend +
  0.015 * Clicks +
  fifelse(Channel == "Search", 0.55, 0) +
  fifelse(CustomerTier == "Platinum", 0.7, 0) +
  fifelse(Region == "West", 0.25, 0)
]
dt[, Predict := 1 / (1 + exp(-score_linear))]
dt[, Target := fifelse(runif(.N) <= Predict, "Yes", "No")]
dt[, PredictedClass := fifelse(Predict >= 0.5, "Yes", "No")]

BinaryModelInsightsArtifacts <- AutoQuant::generate_binary_classification_model_insights_artifacts(
  data = dt,
  target_col = "Target",
  prediction_col = "Predict",
  predicted_class_col = "PredictedClass",
  positive_class = "Yes",
  feature_cols = c("Spend", "Clicks", "Channel", "Region", "CustomerTier"),
  model_name = "Binary_Model_Insights_QA_Model",
  data_name = "Binary scored QA data",
  DateVar = "Date",
  date_aggregation = "month",
  ByVars = c("Channel", "Region", "CustomerTier"),
  Threshold = 0.5,
  OptimizeMetric = "F1"
)

print(BinaryModelInsightsArtifacts$metadata$artifact_index)

ReportPath <- AutoQuant::BinaryClassificationModelInsightsReport(
  artifacts = BinaryModelInsightsArtifacts,
  OutputPath = normalizePath("./"),
  OutputFile = "binary_classification_model_insights_report.html",
  Quiet = FALSE
)
```

</p>
</details>

<details><summary>Binary Classification SHAP Artifact Generator Example</summary>
<p>

`generate_binary_classification_shap_analysis_artifacts()` consumes precomputed `Shap_` columns from binary classification modeling/scoring outputs. It does not compute SHAP values, call `predict()`, require a model object, or use a SHAP backend package. `positive_class` and `prediction_scale` are explicit so the report can describe whether SHAP values are contributions toward the positive class on probability, logit, margin, or unknown scale.

```r
library(data.table)
library(AutoQuant)

set.seed(123)
n <- 300L
x1 <- runif(n, 0, 100)
x2 <- runif(n, 0, 50)
seg <- sample(c("A", "B", "C"), n, TRUE)
date <- as.Date("2024-01-01") + sample(0:180, n, TRUE)
eta <- -1 + 0.025 * x1 + 0.03 * x2 + ifelse(seg == "A", 0.7, ifelse(seg == "B", -0.2, 0))
p <- 1 / (1 + exp(-eta))

dt <- data.table(
  Target = ifelse(runif(n) < p, "Yes", "No"),
  Predict = p,
  PredictedClass = ifelse(p >= 0.5, "Yes", "No"),
  Independent_Variable1 = x1,
  Independent_Variable2 = x2,
  Factor_1 = seg,
  IDCol_1 = seq_len(n),
  IDCol_2 = sample(1000:9999, n, TRUE),
  Date = date,
  Shap_Independent_Variable1 = 0.01 * x1 + ifelse(x2 > 25, 0.15, -0.05) + ifelse(seg == "A", 0.08, 0) + rnorm(n, 0, 0.02),
  Shap_Independent_Variable2 = 0.01 * x2 + ifelse(x1 > 50, 0.1, -0.04) + rnorm(n, 0, 0.02),
  Shap_Factor_1 = ifelse(seg == "A", 0.15, ifelse(seg == "B", -0.05, 0.02)) + rnorm(n, 0, 0.02)
)

BinaryShapArtifacts <- AutoQuant::generate_binary_classification_shap_analysis_artifacts(
  data = dt,
  target_col = "Target",
  prediction_col = "Predict",
  predicted_class_col = "PredictedClass",
  positive_class = "Yes",
  prediction_scale = "probability",
  threshold = 0.5,
  DateVar = "Date",
  date_aggregation = "month",
  ByVars = "Factor_1",
  id_cols = c("IDCol_1", "IDCol_2"),
  selected_features = c("Independent_Variable1", "Independent_Variable2", "Factor_1"),
  local_row_ids = c(1L, 2L),
  top_n = 3L,
  include_dependence = TRUE,
  include_segments = TRUE,
  include_time = TRUE,
  include_local = TRUE,
  include_interactions = TRUE,
  include_threshold_context = TRUE,
  include_class_balance = TRUE,
  include_plots = TRUE
)

or_na <- function(x) if (is.null(x)) NA_character_ else x
artifact_inventory <- rbindlist(lapply(names(BinaryShapArtifacts$artifacts), function(nm) {
  a <- BinaryShapArtifacts$artifacts[[nm]]
  data.table(
    artifact = nm,
    label = a$label,
    type = a$type,
    section = a$section,
    lens = or_na(a$metadata$lens),
    plot_type = or_na(a$metadata$plot_type)
  )
}), fill = TRUE)

print(artifact_inventory)

# Preferred: render the existing artifact result without recomputing artifacts
binary_artifacts <- BinaryShapArtifacts
BinaryShapReportPath <- AutoQuant::BinaryClassificationShapAnalysisReport(
  artifact_result = binary_artifacts,
  OutputPath = normalizePath("./"),
  OutputFile = "binary_classification_shap_analysis_full_qa_report.html",
  Title = "Binary Classification SHAP Analysis Full QA Report",
  Open = TRUE,
  Quiet = FALSE
)

# Convenience wrapper: pass analytical arguments through ...
BinaryShapConvenienceReportPath <- AutoQuant::BinaryClassificationShapAnalysisReport(
  data = dt,
  OutputPath = normalizePath("./"),
  OutputFile = "binary_classification_shap_analysis_convenience_report.html",
  Title = "Binary Classification SHAP Analysis Convenience Report",
  target_col = "Target",
  prediction_col = "Predict",
  predicted_class_col = "PredictedClass",
  positive_class = "Yes",
  prediction_scale = "probability",
  threshold = 0.5,

  DateVar = "Date",
  date_aggregation = "month",
  ByVars = "Factor_1",
  id_cols = c("IDCol_1", "IDCol_2"),
  selected_features = c("Independent_Variable1", "Independent_Variable2", "Factor_1"),
  local_row_ids = c(1L, 2L),
  top_n = 3L,

  include_dependence = TRUE,
  include_segments = TRUE,
  include_time = TRUE,
  include_local = TRUE,
  include_interactions = TRUE,
  include_threshold_context = TRUE,
  include_class_balance = TRUE,

  Open = TRUE,
  Quiet = FALSE
)
```

The generator-first workflow is preferred: use `generate_binary_classification_shap_analysis_artifacts()` for analytical options, then pass the result to `BinaryClassificationShapAnalysisReport(artifact_result = binary_artifacts, ...)`. The report function is a renderer. It can still call the generator as a convenience wrapper by passing `data` plus analytical arguments through `...`, including `positive_class`, `prediction_scale`, `threshold`, and `predicted_class_col`, but `artifact_result` mode avoids recomputing artifacts.

Binary SHAP artifacts include positive-class overview text, diagnostics/config tables, global importance, categorical/binned numeric level importance, dependence, segment, time, threshold context, class balance/outcome context, local explanations, and binned/leveled interaction diagnostics. Interaction surfaces use actual source feature bins/levels on the axes and signed mean SHAP as the heatmap value. Pairwise interaction diagnostics are canonical unordered pairs, so A x B and B x A are treated as the same non-directional analytical object. They are not exact pairwise SHAP interaction decompositions. Multiclass SHAP is deferred.

</p>
</details>


<details><summary>BinaryClassificationModelInsightsReport() Example</summary>
<p>

```r
library(data.table)
library(AutoQuant)

set.seed(321)
n <- 1000L

scored_dt <- data.table(
  Date = as.Date("2025-01-01") + sample(0:240, n, TRUE),
  Channel = sample(c("Direct", "Search", "Social", "Email"), n, TRUE),
  Region = sample(c("West", "Midwest", "South", "Northeast"), n, TRUE),
  Spend = rgamma(n, shape = 5, scale = 40),
  Clicks = rpois(n, lambda = 60),
  CustomerTier = sample(c("Bronze", "Silver", "Gold", "Platinum"), n, TRUE)
)

scored_dt[, score_linear :=
  -1.2 +
  0.008 * Spend +
  0.015 * Clicks +
  fifelse(Channel == "Search", 0.55, 0) +
  fifelse(CustomerTier == "Platinum", 0.7, 0) +
  fifelse(Region == "West", 0.25, 0)
]
scored_dt[, Predict := 1 / (1 + exp(-score_linear))]
scored_dt[, Target := fifelse(runif(.N) <= Predict, "Yes", "No")]
scored_dt[, PredictedClass := fifelse(Predict >= 0.5, "Yes", "No")]

BinaryArtifacts <- AutoQuant::generate_binary_classification_model_insights_artifacts(
  data = scored_dt,
  target_col = "Target",
  prediction_col = "Predict",
  predicted_class_col = "PredictedClass",
  positive_class = "Yes",
  feature_cols = c("Spend", "Clicks", "Channel", "Region", "CustomerTier"),
  model_name = "Binary_Model_Insights_QA_Model",
  data_name = "Binary scored QA data",
  DateVar = "Date",
  date_aggregation = "month",
  ByVars = c("Channel", "Region", "CustomerTier"),
  Threshold = 0.5,
  OptimizeMetric = "F1"
)

ReportPath <- AutoQuant::BinaryClassificationModelInsightsReport(
  artifacts = BinaryArtifacts,
  OutputPath = normalizePath("./"),
  OutputFile = "binary_classification_model_insights_report.html",
  Quiet = FALSE
)
```

</p>
</details>



<details><summary>Legacy MultiClass ModelInsightsReport() Example</summary>
<p>

```r
# Create some dummy correlated data
data <- AutoQuant::FakeDataGenerator(
  Correlation = 0.85,
  N = 10000L,
  ID = 2L,
  ZIP = 0L,
  AddDate = FALSE,
  Classification = FALSE,
  MultiClass = TRUE)

# Copy data
data1 <- data.table::copy(data)

# Feature Colnames
Features <- c(names(data1)[!names(data1) %in% c('IDcol_1','IDcol_2','Adrian')])

# Run function
ModelObject <- AutoQuant::AutoCatBoostMultiClass(
  
  # GPU or CPU and the number of available GPUs
  task_type = 'GPU',
  NumGPUs = 1,
  NumOfParDepPlots = length(Features),
  
  # Metadata args
  OutputSelection = c('Importances', 'EvalPlots', 'EvalMetrics', 'Score_TrainData'),
  ModelID = 'Test_Model_1',
  model_path = getwd(),
  metadata_path = getwd(),
  ReturnModelObjects = TRUE,
  
  # Data args
  data = data,
  TargetColumnName = 'Adrian',
  FeatureColNames = Features,
  IDcols = c('IDcol_1','IDcol_2'))

# Legacy compatibility multiclass report.
# Replace once a multiclass artifact-first report exists.
AutoQuant::ModelInsightsReport(
  TrainDataInclude = TRUE,
  FeatureColumnNames = Features,
  SampleSize = 100000,
  ModelObject = ModelObject,
  ModelID = 'Test_Model_1',
  OutputPath = getwd())
```

</p>
</details>


</p>
</details>

<br>

## Panel Data Forecasting <img src="https://raw.githubusercontent.com/AdrianAntico/AutoQuant/master/Images/AutoCARMA2.png" align="right" width="80" />
<details><summary>Expand to view content</summary>
<p>


<details><summary>Code Example: AutoCatBoostCARMA()</summary>
<p>
 
```r
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Out-of-Sample Feature + Grid Tuning of AutoQuant::AutoCatBoostCARMA()
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Set up your output file path for saving results as a .csv
Path <- "C:/YourPathHere"

# Run on GPU or CPU (some options in the grid tuning force usage of CPU for some runs)
TaskType = "GPU"

# Define number of CPU threads to allow data.table to utilize
data.table::setDTthreads(percent = max(1L, parallel::detectCores()-2L))

# Load data
data <- data <- data.table::fread("https://www.dropbox.com/s/2str3ek4f4cheqi/walmart_train.csv?dl=1")

# Ensure series have no missing dates (also remove series with more than 25% missing values)
data <- Rodeo::TimeSeriesFill(
  data,
  DateColumnName = "Date",
  GroupVariables = c("Store","Dept"),
  TimeUnit = "weeks",
  FillType = "maxmax",
  MaxMissingPercent = 0.25,
  SimpleImpute = TRUE)

# Set negative numbers to 0
data <- data[, Weekly_Sales := data.table::fifelse(Weekly_Sales < 0, 0, Weekly_Sales)]

# Remove IsHoliday column
data[, IsHoliday := NULL]

# Create xregs (this is the include the categorical variables instead of utilizing only the interaction of them)
xregs <- data[, .SD, .SDcols = c("Date", "Store", "Dept")]

# Change data types
data[, ":=" (Store = as.character(Store), Dept = as.character(Dept))]
xregs[, ":=" (Store = as.character(Store), Dept = as.character(Dept))]

# Subset data so we have an out of time sample
data1 <- data.table::copy(data[, ID := 1L:.N, by = c("Store","Dept")][ID <= 125L][, ID := NULL])
data[, ID := NULL]

# Define values for SplitRatios and FCWindow Args
N1 <- data1[, .N, by = c("Store","Dept")][1L, N]
N2 <- xregs[, .N, by = c("Store","Dept")][1L, N]

# Setup Grid Tuning & Feature Tuning data.table using a cross join of vectors
Tuning <- data.table::CJ(
  TimeWeights = c("None",0.999),
  MaxTimeGroups = c("weeks","months"),
  TargetTransformation = c("TRUE","FALSE"),
  Difference = c("TRUE","FALSE"),
  HoldoutTrain = c(6,18),
  Langevin = c("TRUE","FALSE"),
  NTrees = c(2500,5000),
  Depth = c(6,9),
  RandomStrength = c(0.75,1),
  L2_Leaf_Reg = c(3.0,4.0),
  RSM = c(0.75,"NULL"),
  GrowPolicy = c("SymmetricTree","Lossguide","Depthwise"),
  BootStrapType = c("Bayesian","MVS","No"))

# Remove options that are not compatible with GPU (skip over this otherwise)
Tuning <- Tuning[Langevin == "TRUE" | (Langevin == "FALSE" & RSM == "NULL" & BootStrapType %in% c("Bayesian","No"))]

# Randomize order of Tuning data.table
Tuning <- Tuning[order(runif(.N))]

# Load grid results and remove rows that have already been tested
if(file.exists(file.path(Path, "Walmart_CARMA_Metrics.csv"))) {
  Metrics <- data.table::fread(file.path(Path, "Walmart_CARMA_Metrics.csv"))
  temp <- data.table::rbindlist(list(Metrics,Tuning), fill = TRUE)
  temp <- unique(temp, by = c(4:(ncol(temp)-1)))
  Tuning <- temp[is.na(RunTime)][, .SD, .SDcols = names(Tuning)]
  rm(Metrics,temp)
}

# Define the total number of runs
TotalRuns <- Tuning[,.N]

# Kick off feature + grid tuning
for(Run in seq_len(TotalRuns)) {

  # Print run number
  for(zz in seq_len(100)) print(Run)

  # Use fresh data for each run
  xregs_new <- data.table::copy(xregs)
  data_new <- data.table::copy(data1)

  # Timer start
  StartTime <- Sys.time()

  # Run carma system
  CatBoostResults <- AutoQuant::AutoCatBoostCARMA(

    # data args
    data = data_new,
    TimeWeights = if(Tuning[Run, TimeWeights] == "None") NULL else as.numeric(Tuning[Run, TimeWeights]),
    TargetColumnName = "Weekly_Sales",
    DateColumnName = "Date",
    HierarchGroups = NULL,
    GroupVariables = c("Store","Dept"),
    TimeUnit = "weeks",
    TimeGroups = if(Tuning[Run, MaxTimeGroups] == "weeks") "weeks" else if(Tuning[Run, MaxTimeGroups] == "months") c("weeks","months") else c("weeks","months","quarters"),

    # Production args
    TrainOnFull = TRUE,
    SplitRatios = c(1 - Tuning[Run, HoldoutTrain] / N2, Tuning[Run, HoldoutTrain] / N2),
    PartitionType = "random",
    FC_Periods = N2-N1,
    TaskType = TaskType,
    NumGPU = 1,
    Timer = TRUE,
    DebugMode = TRUE,

    # Target variable transformations
    TargetTransformation = as.logical(Tuning[Run, TargetTransformation]),
    Methods = c('Asinh','Log','LogPlus1','Sqrt'),
    Difference = as.logical(Tuning[Run, Difference]),
    NonNegativePred = TRUE,
    RoundPreds = FALSE,

    # Calendar-related features
    CalendarVariables = c("week","wom","month","quarter"),
    HolidayVariable = c("USPublicHolidays"),
    HolidayLookback = NULL,
    HolidayLags = c(1,2,3),
    HolidayMovingAverages = c(2,3),

    # Lags, moving averages, and other rolling stats
    Lags = if(Tuning[Run, MaxTimeGroups] == "weeks") c(1,2,3,4,5,8,9,12,13,51,52,53) else if(Tuning[Run, MaxTimeGroups] == "months") list("weeks" = c(1,2,3,4,5,8,9,12,13,51,52,53), "months" = c(1,2,6,12)) else list("weeks" = c(1,2,3,4,5,8,9,12,13,51,52,53), "months" = c(1,2,6,12), "quarters" = c(1,2,3,4)),
    MA_Periods = if(Tuning[Run, MaxTimeGroups] == "weeks") c(2,3,4,5,8,9,12,13,51,52,53) else if(Tuning[Run, MaxTimeGroups] == "months") list("weeks" = c(2,3,4,5,8,9,12,13,51,52,53), "months" = c(2,6,12)) else list("weeks" = c(2,3,4,5,8,9,12,13,51,52,53), "months" = c(2,6,12), "quarters" = c(2,3,4)),
    SD_Periods = NULL,
    Skew_Periods = NULL,
    Kurt_Periods = NULL,
    Quantile_Periods = NULL,
    Quantiles_Selected = NULL,

    # Bonus features
    AnomalyDetection = NULL,
    XREGS = xregs_new,
    FourierTerms = 0,
    TimeTrendVariable = TRUE,
    ZeroPadSeries = NULL,
    DataTruncate = FALSE,

    # ML grid tuning args
    GridTune = FALSE,
    PassInGrid = NULL,
    ModelCount = 5,
    MaxRunsWithoutNewWinner = 50,
    MaxRunMinutes = 60*60,

    # ML evaluation output
    PDFOutputPath = NULL,
    SaveDataPath = NULL,
    NumOfParDepPlots = 0L,

    # ML loss functions
    EvalMetric = "RMSE",
    EvalMetricValue = 1,
    LossFunction = "RMSE",
    LossFunctionValue = 1,

    # ML tuning args
    NTrees = Tuning[Run, NTrees],
    Depth = Tuning[Run, Depth],
    L2_Leaf_Reg = Tuning[Run, L2_Leaf_Reg],
    LearningRate = 0.03,
    Langevin = as.logical(Tuning[Run, Langevin]),
    DiffusionTemperature = 10000,
    RandomStrength = Tuning[Run, RandomStrength],
    BorderCount = 254,
    RSM = if(Tuning[Run, RSM] == "NULL") NULL else as.numeric(Tuning[Run, RSM]),
    GrowPolicy = Tuning[Run, GrowPolicy],
    BootStrapType = Tuning[Run, BootStrapType],
    ModelSizeReg = 0.5,
    FeatureBorderType = "GreedyLogSum",
    SamplingUnit = "Group",
    SubSample = NULL,
    ScoreFunction = "Cosine",
    MinDataInLeaf = 1)

  # Timer End
  EndTime <- Sys.time()

  # Prepare data for evaluation
  Results <- CatBoostResults$Forecast
  data.table::setnames(Results, "Weekly_Sales", "bla")
  Results <- merge(Results, data, by = c("Store","Dept","Date"), all = FALSE)
  Results <- Results[is.na(bla)][, bla := NULL]

  # Create totals and subtotals
  Results <- data.table::groupingsets(
    x = Results,
    j = list(Predictions = sum(Predictions), Weekly_Sales = sum(Weekly_Sales)),
    by = c("Date", "Store", "Dept"),
    sets = list(c("Date", "Store", "Dept"), c("Store", "Dept"), "Store", "Dept", "Date"))
  
  # Fill NAs with "Total" for totals and subtotals
  for(cols in c("Store","Dept")) Results[, eval(cols) := data.table::fifelse(is.na(get(cols)), "Total", get(cols))]

  # Add error measures
  Results[, Weekly_MAE := abs(Weekly_Sales - Predictions)]
  Results[, Weekly_MAPE := Weekly_MAE / Weekly_Sales]

  # Weekly results
  Weekly_MAPE <- Results[, list(Weekly_MAPE = mean(Weekly_MAPE)), by = list(Store,Dept)]

  # Monthly results
  temp <- data.table::copy(Results)
  temp <- temp[, Date := lubridate::floor_date(Date, unit = "months")]
  temp <- temp[, lapply(.SD, sum), by = c("Date","Store","Dept"), .SDcols = c("Predictions", "Weekly_Sales")]
  temp[, Monthly_MAE := abs(Weekly_Sales - Predictions)]
  temp[, Monthly_MAPE := Monthly_MAE / Weekly_Sales]
  Monthly_MAPE <- temp[, list(Monthly_MAPE = mean(Monthly_MAPE)), by = list(Store,Dept)]

  # Collect metrics for Total (feel free to switch to something else or no filter at all)
  Metrics <- data.table::data.table(
    RunNumber = Run,
    Total_Weekly_MAPE = Weekly_MAPE[Store == "Total" & Dept == "Total", Weekly_MAPE],
    Total_Monthly_MAPE = Monthly_MAPE[Store == "Total" & Dept == "Total", Monthly_MAPE],
    Tuning[Run],
    RunTime = EndTime - StartTime)

  # Append to file (not overwrite)
  data.table::fwrite(Metrics, file = file.path(Path, "Walmart_CARMA_Metrics.csv"), append = TRUE)
  
  # Remove objects (clear space before new runs)
  rm(CatBoostResults, Results, temp, Weekly_MAE, Weekly_MAPE, Monthly_MAE, Monthly_MAPE)

  # Garbage collection because of GPU
  gc()
}
```

</p>
</details>


<details><summary>Code Example: AutoXGBoostCARMA()</summary>
<p>

```r
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# XGBoost Version ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Load data
data <- data.table::fread("https://www.dropbox.com/s/2str3ek4f4cheqi/walmart_train.csv?dl=1")

# Ensure series have no missing dates (also remove series with more than 25% missing values)
data <- Rodeo::TimeSeriesFill(
  data,
  DateColumnName = "Date",
  GroupVariables = c("Store","Dept"),
  TimeUnit = "weeks",
  FillType = "maxmax",
  MaxMissingPercent = 0.25,
  SimpleImpute = TRUE)

# Set negative numbers to 0
data <- data[, Weekly_Sales := data.table::fifelse(Weekly_Sales < 0, 0, Weekly_Sales)]

# Remove IsHoliday column
data[, IsHoliday := NULL]

# Create xregs (this is the include the categorical variables instead of utilizing only the interaction of them)
xregs <- data[, .SD, .SDcols = c("Date", "Store", "Dept")]

# Change data types
data[, ":=" (Store = as.character(Store), Dept = as.character(Dept))]
xregs[, ":=" (Store = as.character(Store), Dept = as.character(Dept))]

 # Build forecast
XGBoostResults <- AutoXGBoostCARMA(

  # Data Artifacts
  data = data,
  NonNegativePred = FALSE,
  RoundPreds = FALSE,
  TargetColumnName = "Weekly_Sales",
  DateColumnName = "Date",
  HierarchGroups = NULL,
  GroupVariables = c("Store","Dept"),
  TimeUnit = "weeks",
  TimeGroups = c("weeks","months"),

  # Data Wrangling Features
  ZeroPadSeries = NULL,
  DataTruncate = FALSE,
  SplitRatios = c(1 - 10 / 138, 10 / 138),
  PartitionType = "timeseries",
  AnomalyDetection = NULL,
  EncodingMethod = "binary",

  # Productionize
  FC_Periods = 0,
  TrainOnFull = FALSE,
  NThreads = 8,
  Timer = TRUE,
  DebugMode = FALSE,
  SaveDataPath = NULL,
  PDFOutputPath = NULL,

  # Target Transformations
  TargetTransformation = TRUE,
  Methods = c("Asinh", "Asin", "Log", "LogPlus1", "Sqrt", "Logit"),
  Difference = FALSE,

  # Features
  Lags = list("weeks" = seq(1L, 10L, 1L), "months" = seq(1L, 5L, 1L)),
  MA_Periods = list("weeks" = seq(5L, 20L, 5L), "months" = seq(2L, 10L, 2L)),
  SD_Periods = NULL,
  Skew_Periods = NULL,
  Kurt_Periods = NULL,
  Quantile_Periods = NULL,
  Quantiles_Selected = c("q5","q95"),
  XREGS = xregs,
  FourierTerms = 4,
  CalendarVariables = c("week", "wom", "month", "quarter"),
  HolidayVariable = c("USPublicHolidays","EasterGroup","ChristmasGroup","OtherEcclesticalFeasts"),
  HolidayLookback = NULL,
  HolidayLags = 1,
  HolidayMovingAverages = 1:2,
  TimeTrendVariable = TRUE,

  # ML eval args
  TreeMethod = "hist",
  EvalMetric = "RMSE",
  LossFunction = 'reg:squarederror',

  # ML grid tuning
  GridTune = FALSE,
  ModelCount = 5,
  MaxRunsWithoutNewWinner = 20L,
  MaxRunMinutes = 24L*60L,

  # ML args
  NTrees = 300,
  LearningRate = 0.3,
  MaxDepth = 9L,
  MinChildWeight = 1.0,
  SubSample = 1.0,
  ColSampleByTree = 1.0)
```

</p>
</details>

<details><summary>Code Example: AutoLightGBMCARMA()</summary>
<p>

```r
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# LightGBM Version ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Load data
data <- data.table::fread('https://www.dropbox.com/s/2str3ek4f4cheqi/walmart_train.csv?dl=1')

# Ensure series have no missing dates (also remove series with more than 25% missing values)
data <- Rodeo::TimeSeriesFill(
  data,
  DateColumnName = 'Date',
  GroupVariables = c('Store','Dept'),
  TimeUnit = 'weeks',
  FillType = 'maxmax',
  MaxMissingPercent = 0.25,
  SimpleImpute = TRUE)

# Set negative numbers to 0
data <- data[, Weekly_Sales := data.table::fifelse(Weekly_Sales < 0, 0, Weekly_Sales)]

# Remove IsHoliday column
data[, IsHoliday := NULL]

# Create xregs (this is the include the categorical variables instead of utilizing only the interaction of them)
xregs <- data[, .SD, .SDcols = c('Date', 'Store', 'Dept')]

# Change data types
data[, ':=' (Store = as.character(Store), Dept = as.character(Dept))]
xregs[, ':=' (Store = as.character(Store), Dept = as.character(Dept))]

# Build forecast
Results <- AutoLightGBMCARMA(

  # Data Artifacts
  data = data,
  NonNegativePred = FALSE,
  RoundPreds = FALSE,
  TargetColumnName = 'Weekly_Sales',
  DateColumnName = 'Date',
  HierarchGroups = NULL,
  GroupVariables = c('Store','Dept'),
  TimeUnit = 'weeks',
  TimeGroups = c('weeks','months'),

  # Data Wrangling Features
  EncodingMethod = 'binary',
  ZeroPadSeries = NULL,
  DataTruncate = FALSE,
  SplitRatios = c(1 - 10 / 138, 10 / 138),
  PartitionType = 'timeseries',
  AnomalyDetection = NULL,

  # Productionize
  FC_Periods = 0,
  TrainOnFull = FALSE,
  NThreads = 8,
  Timer = TRUE,
  DebugMode = FALSE,
  SaveDataPath = NULL,
  PDFOutputPath = NULL,

  # Target Transformations
  TargetTransformation = TRUE,
  Methods = c('Asinh', 'Asin', 'Log', 'LogPlus1', 'Sqrt', 'Logit'),
  Difference = FALSE,

  # Features
  Lags = list('weeks' = seq(1L, 10L, 1L), 'months' = seq(1L, 5L, 1L)),
  MA_Periods = list('weeks' = seq(5L, 20L, 5L), 'months' = seq(2L, 10L, 2L)),
  SD_Periods = NULL,
  Skew_Periods = NULL,
  Kurt_Periods = NULL,
  Quantile_Periods = NULL,
  Quantiles_Selected = c('q5','q95'),
  XREGS = xregs,
  FourierTerms = 4,
  CalendarVariables = c('week', 'wom', 'month', 'quarter'),
  HolidayVariable = c('USPublicHolidays','EasterGroup','ChristmasGroup','OtherEcclesticalFeasts'),
  HolidayLookback = NULL,
  HolidayLags = 1,
  HolidayMovingAverages = 1:2,
  TimeTrendVariable = TRUE,

  # ML eval args
  TreeMethod = 'hist',
  EvalMetric = 'RMSE',
  LossFunction = 'reg:squarederror',

  # Grid tuning args
  GridTune = FALSE,
  GridEvalMetric = 'mae',
  ModelCount = 30L,
  MaxRunsWithoutNewWinner = 20L,
  MaxRunMinutes = 24L*60L,

  # LightGBM Args
  Device_Type = TaskType,
  LossFunction = 'regression',
  EvalMetric = 'MAE',
  Input_Model = NULL,
  Task = 'train',
  Boosting = 'gbdt',
  LinearTree = FALSE,
  Trees = 1000,
  ETA = 0.10,
  Num_Leaves = 31,
  Deterministic = TRUE,

  # Learning Parameters
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#learning-control-parameters
  Force_Col_Wise = FALSE,
  Force_Row_Wise = FALSE,
  Max_Depth = 6,
  Min_Data_In_Leaf = 20,
  Min_Sum_Hessian_In_Leaf = 0.001,
  Bagging_Freq = 1.0,
  Bagging_Fraction = 1.0,
  Feature_Fraction = 1.0,
  Feature_Fraction_Bynode = 1.0,
  Lambda_L1 = 0.0,
  Lambda_L2 = 0.0,
  Extra_Trees = FALSE,
  Early_Stopping_Round = 10,
  First_Metric_Only = TRUE,
  Max_Delta_Step = 0.0,
  Linear_Lambda = 0.0,
  Min_Gain_To_Split = 0,
  Drop_Rate_Dart = 0.10,
  Max_Drop_Dart = 50,
  Skip_Drop_Dart = 0.50,
  Uniform_Drop_Dart = FALSE,
  Top_Rate_Goss = FALSE,
  Other_Rate_Goss = FALSE,
  Monotone_Constraints = NULL,
  Monotone_Constraints_Method = 'advanced',
  Monotone_Penalty = 0.0,
  Forcedsplits_Filename = NULL, # use for AutoStack option; .json file
  Refit_Decay_Rate = 0.90,
  Path_Smooth = 0.0,

  # IO Dataset Parameters
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#io-parameters
  Max_Bin = 255,
  Min_Data_In_Bin = 3,
  Data_Random_Seed = 1,
  Is_Enable_Sparse = TRUE,
  Enable_Bundle = TRUE,
  Use_Missing = TRUE,
  Zero_As_Missing = FALSE,
  Two_Round = FALSE,

  # Convert Parameters
  Convert_Model = NULL,
  Convert_Model_Language = 'cpp',

  # Objective Parameters
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#objective-parameters
  Boost_From_Average = TRUE,
  Alpha = 0.90,
  Fair_C = 1.0,
  Poisson_Max_Delta_Step = 0.70,
  Tweedie_Variance_Power = 1.5,
  Lambdarank_Truncation_Level = 30,

  # Metric Parameters (metric is in Core)
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#metric-parameters
  Is_Provide_Training_Metric = TRUE,
  Eval_At = c(1,2,3,4,5),

  # Network Parameters
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#network-parameters
  Num_Machines = 1,

  # GPU Parameters
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#gpu-parameters
  Gpu_Platform_Id = -1,
  Gpu_Device_Id = -1,
  Gpu_Use_Dp = TRUE,
  Num_Gpu = 1)
```

</p>
</details>

<details><summary>Code Example: AutoH2OCARMA()</summary>
<p>

```r
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# H2O Version ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Load data
data <- data.table::fread("https://www.dropbox.com/s/2str3ek4f4cheqi/walmart_train.csv?dl=1")

# Ensure series have no missing dates (also remove series with more than 25% missing values)
data <- Rodeo::TimeSeriesFill(
  data,
  DateColumnName = "Date",
  GroupVariables = c("Store","Dept"),
  TimeUnit = "weeks",
  FillType = "maxmax",
  MaxMissingPercent = 0.25,
  SimpleImpute = TRUE)

# Set negative numbers to 0
data <- data[, Weekly_Sales := data.table::fifelse(Weekly_Sales < 0, 0, Weekly_Sales)]

# Remove IsHoliday column
data[, IsHoliday := NULL]

# Create xregs (this is the include the categorical variables instead of utilizing only the interaction of them)
xregs <- data[, .SD, .SDcols = c("Date", "Store", "Dept")]

# Change data types
data[, ":=" (Store = as.character(Store), Dept = as.character(Dept))]
xregs[, ":=" (Store = as.character(Store), Dept = as.character(Dept))]

# Build forecast
Results <- AutoQuant::AutoH2OCARMA(

  # Data Artifacts
  AlgoType = "drf",
  ExcludeAlgos = NULL,
  data = data,
  TargetColumnName = "Weekly_Sales",
  DateColumnName = "Date",
  HierarchGroups = NULL,
  GroupVariables = c("Dept"),
  TimeUnit = "week",
  TimeGroups = c("weeks","months"),

  # Data Wrangling Features
  SplitRatios = c(1 - 10 / 138, 10 / 138),
  PartitionType = "random",

  # Production args
  FC_Periods = 4L,
  TrainOnFull = FALSE,
  MaxMem = {gc();paste0(as.character(floor(max(32, as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE)) -32) / 1000000)),"G")},
  NThreads = parallel::detectCores(),
  PDFOutputPath = NULL,
  SaveDataPath = NULL,
  Timer = TRUE,
  DebugMode = TRUE,

  # Target Transformations
  TargetTransformation = FALSE,
  Methods = c("Asinh", "Asin", "Log", "LogPlus1", "Sqrt", "Logit"),
  Difference = FALSE,
  NonNegativePred = FALSE,
  RoundPreds = FALSE,

  # Calendar features
  CalendarVariables = c("week", "wom", "month", "quarter", "year"),
  HolidayVariable = c("USPublicHolidays","EasterGroup",
    "ChristmasGroup","OtherEcclesticalFeasts"),
  HolidayLookback = NULL,
  HolidayLags = 1:7,
  HolidayMovingAverages = 2:7,
  TimeTrendVariable = TRUE,

  # Time series features
  Lags = list("weeks" = c(1:4), "months" = c(1:3)),
  MA_Periods = list("weeks" = c(2:8), "months" = c(6:12)),
  SD_Periods = NULL,
  Skew_Periods = NULL,
  Kurt_Periods = NULL,
  Quantile_Periods = NULL,
  Quantiles_Selected = NULL,

  # Bonus Features
  XREGS = NULL,
  FourierTerms = 2L,
  AnomalyDetection = NULL,
  ZeroPadSeries = NULL,
  DataTruncate = FALSE,

  # ML evaluation args
  EvalMetric = "RMSE",
  NumOfParDepPlots = 0L,

  # ML grid tuning args
  GridTune = FALSE,
  GridStrategy = "Cartesian",
  ModelCount = 5,
  MaxRuntimeSecs = 60*60*24,
  StoppingRounds = 10,

  # ML Args
  NTrees = 1000L,
  MaxDepth = 20,
  SampleRate = 0.632,
  MTries = -1,
  ColSampleRatePerTree = 1,
  ColSampleRatePerTreeLevel  = 1,
  MinRows = 1,
  NBins = 20,
  NBinsCats = 1024,
  NBinsTopLevel = 1024,
  HistogramType = "AUTO",
  CategoricalEncoding = "AUTO",
  RandomColNumbers = NULL,
  InteractionColNumbers = NULL,
  WeightsColumn = NULL,

  # ML args
  Distribution = "gaussian",
  Link = "identity",
  RandomDistribution = NULL,
  RandomLink = NULL,
  Solver = "AUTO",
  Alpha = NULL,
  Lambda = NULL,
  LambdaSearch = FALSE,
  NLambdas = -1,
  Standardize = TRUE,
  RemoveCollinearColumns = FALSE,
  InterceptInclude = TRUE,
  NonNegativeCoefficients = FALSE)

```

</p>
</details>

</p>
</details>

<br>

## Time Series Forecasting <img src="https://raw.githubusercontent.com/AdrianAntico/AutoQuant/master/Images/TimeSeriesImage2.png" align="right" width="80" />
<details><summary>Expand to view content</summary>
<p>


<details><summary>Time Series Forecasting Description</summary>
<p>

There are three sets of functions for single series traditional time series model forecasting. The first set includes the AutoBanditSarima() and AutoBanditNNet() functions. These two offer the most robust fitting strategies. The utilize a multi-armed-bandit to help narrow the search space of available parameter settings. The next batch includes the AutoTBATS(), AutoETS(), and the AutoArfima() functions. These don't utilze the bandit framework. Rather, they run through a near exhaustive search through all their possible settings. Both the bandit set and the non-bandit set utilize parallelism to burn through as many models as possible for a fixed amount of time. 

* Bandit: AutoBanditSarima
* Bandit: AutoBanditNNet
* Exhaustive: AutoTBATS
* Exhaustive: AutoETS
* Exhaustive: AutoArfima

</p>
</details>

<details><summary>AutoBanditSarima() Example</summary>
<p>
 
```r
# Build model
data <- AutoQuant::FakeDataGenerator(Correlation = 0.82, TimeSeries = TRUE, TimeSeriesTimeAgg = "1min")

# Run system
Output <- AutoQuant::AutoBanditSarima(
  data = data,
  SaveFile = NULL,
  ByDataType = FALSE,
  TargetVariableName = "Weekly_Sales",
  DateColumnName = "Date",
  TimeAggLevel = "1min",
  EvaluationMetric = "MAE",
  NumHoldOutPeriods = 12L,
  NumFCPeriods = 16L,
  MaxLags = 10L,
  MaxSeasonalLags = 0L,
  MaxMovingAverages = 3L,
  MaxSeasonalMovingAverages = 0L,
  MaxFourierPairs = 2L,
  TrainWeighting = 0.50,
  MaxConsecutiveFails = 50L,
  MaxNumberModels = 100L,
  MaxRunTimeMinutes = 10L,
  NumberCores = 12,
  DebugMode = FALSE)

# View output
Output$ForecastPlot
Output$ErrorLagMA2x2
Output$Forecast
Output$PerformanceGrid
```

</p>
</details>

<details><summary>AutoBanditNNet() Example</summary>
<p>
 
```r
# Build model
data <- AutoQuant::FakeDataGenerator(Correlation = 0.82, TimeSeries = TRUE, TimeSeriesTimeAgg = "1min")

# Run system
Output <- AutoQuant::AutoBanditNNet(
  data = data,
  TargetVariableName = "Weekly_Sales",
  DateColumnName = "Date",
  TimeAggLevel = "1min",
  EvaluationMetric = "MAE",
  NumHoldOutPeriods = 12L,
  NumFCPeriods = 16L,
  MaxLags = 10L,
  MaxSeasonalLags = 0L,
  MaxFourierPairs = 2L,
  TrainWeighting = 0.50,
  MaxConsecutiveFails = 50L,
  MaxNumberModels = 100L,
  MaxRunTimeMinutes = 10L,
  NumberCores = 12)
  
# View output
Output$Forecast
Output$PerformanceGrid
```

</p>
</details>


<details><summary>AutoTBATS() Example</summary>
<p>
 
```r
# Build model
data <- AutoQuant::FakeDataGenerator(Correlation = 0.82, TimeSeries = TRUE, TimeSeriesTimeAgg = "1min")

# Run system
Output <- AutoQuant::AutoTBATS(
  data = data,
  FilePath = getwd(),
  TargetVariableName = "Weekly_Sales",
  DateColumnName = "Date",
  TimeAggLevel = "1min",
  EvaluationMetric = "MAE",
  NumHoldOutPeriods = 12L,
  NumFCPeriods = 16L,
  MaxLags = 10L,
  MaxMovingAverages = 5,
  MaxSeasonalPeriods = 1,
  TrainWeighting = 0.50,
  MaxConsecutiveFails = 50L,
  MaxNumberModels = 100L,
  MaxRunTimeMinutes = 10L,
  NumberCores = 12)

# View output
Output$Forecast
Output$PerformanceGrid
```

</p>
</details>


<details><summary>AutoETS() Example</summary>
<p>
 
```r
# Build model
data <- AutoQuant::FakeDataGenerator(Correlation = 0.82, TimeSeries = TRUE, TimeSeriesTimeAgg = "1min")

# Run system
Output <- AutoQuant::AutoETS(
  data = data,
  FilePath = getwd(),
  TargetVariableName = "Weekly_Sales",
  DateColumnName = "Date",
  TimeAggLevel = "1min",
  EvaluationMetric = "MAE",
  NumHoldOutPeriods = 12L,
  NumFCPeriods = 16L,
  TrainWeighting = 0.50,
  MaxConsecutiveFails = 50L,
  MaxNumberModels = 100L,
  MaxRunTimeMinutes = 10L,
  NumberCores = 12)

# View output
Output$Forecast
Output$PerformanceGrid
```

</p>
</details>


<details><summary>AutoArfima() Example</summary>
<p>
 
```r
# Build model
data <- AutoQuant::FakeDataGenerator(Correlation = 0.82, TimeSeries = TRUE, TimeSeriesTimeAgg = "1min")

# Run system
Output <- AutoQuant::AutoArfima(
  data = data,
  FilePath = getwd(),
  TargetVariableName = "Weekly_Sales",
  DateColumnName = "Date",
  TimeAggLevel = "1min",
  EvaluationMetric = "MAE",
  NumHoldOutPeriods = 12L,
  NumFCPeriods = 16L,
  TrainWeighting = 0.50,
  MaxLags = 5, 
  MaxMovingAverages = 5,
  MaxConsecutiveFails = 50L,
  MaxNumberModels = 100L,
  MaxRunTimeMinutes = 10L,
  NumberCores = 12)

# View output
Output$Forecast
Output$PerformanceGrid
```

</p>
</details>


</p>
</details>


</p>
</details>


</p>
</details>
