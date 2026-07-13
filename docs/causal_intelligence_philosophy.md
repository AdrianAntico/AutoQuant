# Causal Intelligence Philosophy

AutoQuant Causal Intelligence starts from a conservative premise:

Predictive evidence is not causal evidence.

Feature importance, SHAP dependence, model gain, correlation, lift, calibration, and forecasting accuracy can all be useful analytical evidence. None of them alone answers what would happen under an intervention. Causal Intelligence therefore treats causal reasoning as a planning discipline before it becomes an estimation discipline.

Phase 1 does not estimate effects. It creates the contracts needed to ask a causal question precisely, define the target estimand, assign question-relative variable roles, record causal assumptions, inspect identification blockers, and generate a planning artifact.

Phase 2 keeps the same boundary and moves from causal question planning into governed experiment design. It creates experiment questions, design specifications, deterministic assignment proposals, measurement plans, validity-threat registers, approval gates, information-value assessments, and experiment plan artifacts. It still does not execute treatment and still does not estimate causal effects.

## Governing Rules

1. Estimand before estimator.
2. Identification before computation.
3. Question-relative causal roles before adjustment.
4. Deterministic diagnostics before probabilistic explanation.
5. Planning artifacts before effect claims.
6. Experiment design before experiment execution.
7. Authority, coverage, and measurement readiness before approval.

## Why Planning Comes First

Analytical systems often blur three different statements:

- A variable predicts an outcome.
- A variable is associated with an outcome.
- Intervening on a variable would change an outcome.

The third statement requires a causal question, an intervention definition, a comparison condition, an estimand, a target population, timing, variable roles, assumptions, and a defensible design. Without these, effect estimates can appear precise while answering an undefined question.

The Phase 1 framework is intentionally strict. It records what is known, what is assumed, what remains missing, and which designs appear eligible. It also records prohibited claims so downstream software does not accidentally convert causal planning into causal proof.

The Phase 2 framework is strict in the same way. An assignment plan is a proposed randomization artifact, not delivered treatment. A power plan is an assumption record, not a guarantee. An approval gate can approve a design artifact while still recording `execution_ready = FALSE`, because operational treatment execution is outside the framework.

## Relationship to Business Intent

Causal questions should ultimately trace back to authored decision contexts, business objectives, strategies, tactics, levers, KPIs, constraints, and alternatives. The same variable may be a tactic lever, a controllable driver, a forecasting regressor, a candidate causal exposure, and an optimization variable. These roles are not interchangeable.

AutoQuant separates:

- global variable semantics,
- decision-relative business meaning,
- question-relative causal role,
- estimator or design eligibility.

That separation allows the system to preserve business intent while avoiding causal overclaiming.

## Phase 1 Boundary

Supported:

- causal question contracts,
- estimand contracts,
- question-relative variable roles,
- directed relationship records,
- graph diagnostics,
- adjustment guidance,
- identification assessment,
- design eligibility planning,
- investigation plans,
- canonical causal planning artifacts.

Not supported:

- treatment effect estimation,
- automatic DAG discovery,
- automatic confounder truth claims,
- causal effect recommendations,
- adjustment set optimization,
- causal model fitting.

The output of Phase 1 is a plan, not an answer.

## Phase 2 Boundary

Supported:

- experiment question contracts,
- experiment design specifications,
- deterministic assignment proposals,
- assignment balance diagnostics,
- power and precision plans,
- timing plans,
- measurement plans,
- validity threat registers,
- interference and spillover planning,
- authority and coverage gates,
- information-value assessments,
- canonical experiment plan artifacts.

Not supported:

- treatment execution,
- exposure delivery,
- completed-experiment analysis,
- causal effect estimation,
- autonomous experiment approval.

The output of Phase 2 is a governed design artifact, not an experiment result.
