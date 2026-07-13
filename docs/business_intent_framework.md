# Business Intent and Lever Management Framework

Status: Semantic Intelligence Phase 2 implemented.

Semantic Intelligence Phase 1 made variables explicit analytical objects.
Phase 2 adds the business-intent layer above variable semantics. The purpose is
to represent what the organization is trying to accomplish, how it intends to
accomplish it, which levers it can manipulate, which variables represent those
levers, and what authority exists to recommend or act.

This phase is philosophy-first and contract-first. It does not implement
enterprise optimization, causal estimation, MMM, reinforcement learning,
contextual bandits, autonomous executive decision-making, or a graph database.

## Governing Chain

The intended long-term chain is:

```text
Mission
-> Business objective
-> Strategy
-> Tactic
-> Lever
-> KPI / guardrail / constraint / risk
-> Variable semantics
-> Analytical operator
-> Evidence
-> Experiment
-> Recommendation
-> Decision
-> Realized outcome
-> Strategy validation or revision
```

The implemented contract preserves this chain through typed records,
deterministic relationships, alignment findings, explore/exploit assessments,
authority metadata, and a canonical artifact.

## Public API

- `aq_business_intent()`
- `aq_validate_business_intent()`
- `aq_assess_business_alignment()`
- `aq_assess_explore_exploit()`
- `aq_business_intent_artifact()`
- `qa_business_intent_framework()`

The API intentionally avoids exporting one constructor per noun. The smallest
coherent public contract is a typed business-intent object containing records
for the business nouns and relationship tables connecting them.

## Core Distinctions

| Concept | Meaning | Not interchangeable with |
| --- | --- | --- |
| Mission | Durable organizational purpose. | Objective or KPI |
| Objective | Desired business outcome, sometimes quantitative and sometimes qualitative. | Strategy or tactic |
| Strategy | Thesis for how an objective will be achieved. | Tactic or model |
| Tactic | Operational activity that instantiates a strategy. | Lever |
| Lever | Organizational degree of freedom that can be manipulated or simulated. | Dataset column |
| Variable | Data representation of an outcome, context, lever, KPI, state, exposure, or engineered feature. | Business lever |
| KPI | Measurement of progress, status, guardrail, or diagnosis. | Objective itself |
| Guardrail | Metric or condition limiting action. | Primary optimization target |
| Constraint | Boundary on an action or feasible range. | Predictor |
| Risk | Threat to an objective, strategy, tactic, lever, or decision. | Uncertainty alone |
| Assumption | Belief required for strategy, tactic, model, or recommendation validity. | Evidence |
| Recommendation | Proposed action, usually on a lever. | Decision |
| Decision | Acceptance, rejection, modification, delay, or escalation of a recommendation. | Recommendation |

A tactic may operate through many levers. A lever may map to many source and
engineered variables. A variable may retain separate business, operational,
analytical, and causal roles from Phase 1.

## Business-Intent Records

`aq_business_intent()` accepts data frames or named lists for:

- missions
- objectives
- strategies
- tactics
- levers
- KPIs
- guardrails
- constraints
- risks
- assumptions
- recommendations
- decisions
- authority
- coverage

Each record type has a deterministic ID column such as `mission_id`,
`objective_id`, `strategy_id`, `tactic_id`, `lever_id`, or `kpi_id`.
Additional fields are preserved rather than narrowed. This allows the contract
to grow without forcing all consumers to understand every optional field.

## Relationships

Relationships are represented as deterministic tables, not a graph database.
The framework infers relationships from reference fields such as:

- objective -> mission
- strategy -> objective
- tactic -> strategy
- lever -> tactic
- lever -> variable
- KPI -> objective
- KPI -> variable
- guardrail -> tactic
- constraint -> lever
- risk -> objective or tactic
- assumption -> strategy, tactic, or lever
- recommendation -> lever
- decision -> recommendation

Explicit relationship tables can be supplied when needed.

## Lever Contract

A lever is an organizational degree of freedom. It may map to:

- source variables
- planned values
- realized values
- cost representations
- exposure representations
- engineered features
- policy settings
- thresholds
- allocations
- external operator inputs

The contract preserves:

- identity and ownership
- related strategy and tactic
- related variables
- controllability
- permitted and validated ranges
- action cadence
- implementation and effect delay
- reversibility
- cost and marginal cost
- evidence strength
- uncertainty
- experiment history
- observational, predictive, causal, and transfer evidence
- optimization, experiment, simulation, recommendation, and execution eligibility
- approval and escalation requirements
- lifecycle status

A lever is operationally actionable. Its causal effect may still be uncertain.
The framework does not infer causal effect from business actionability.

## Variable-to-Lever Mapping

Variable semantics remain separate from business levers.

Example: a paid-search budget lever may map to:

- planned spend
- realized spend
- impressions
- reach
- frequency
- adstocked exposure
- saturation-transformed exposure
- marginal cost

These are related representations, not interchangeable meanings. The Phase 2
validation checks whether lever variables exist in the Phase 1 variable
semantics object when one is supplied.

## KPI, Guardrail, Constraint, and Risk

KPI records preserve measurement definition, source variable, objective,
strategy/tactic references, unit, aggregation, target, baseline, role,
confidence, and limitations.

Guardrails limit action. Constraints bound feasible actions. Risks threaten
objectives, tactics, strategies, decisions, or lever changes. They are stored as
separate record families so future reporting and optimization cannot quietly
treat guardrails as primary outcomes or constraints as ordinary predictors.

## Assumptions

Assumptions are first-class because exploration begins with uncertainty.
Assumptions may reference objectives, strategies, tactics, levers, or models
and preserve evidence status, confidence, testability, experiment eligibility,
economic importance, and decision consequence if wrong.

## Explore / Exploit Assessment

`aq_assess_explore_exploit()` provides a deterministic, explainable
classification for levers and assumptions:

- `explore`
- `exploit`
- `exploit_with_monitoring`
- `hold`
- `collect_more_evidence`
- `escalate`
- `insufficient_information`

The assessment uses explicit fields such as evidence strength, uncertainty,
economic importance, reversibility, experiment eligibility, authority,
measurement quality, and risk. It is not reinforcement learning and does not
allocate resources.

Explore/exploit status can vary by context and validated range. A lever can be
exploitable inside a supported range and exploratory outside it.

## Authority and Partial Coverage

The framework distinguishes capability from permission. Authority records can
represent:

- observe
- explain
- diagnose
- simulate
- recommend
- design experiment
- request approval
- execute approved experiment
- execute within bounded policy
- coordinate across domains

Coverage records preserve represented domains, missing domains, authority
levels, and deterministic limitations. A marketing-only deployment can state
that a recommendation is optimized within represented marketing levers and
supplied enterprise constraints, not enterprise-wide allocation.

## Alignment Assessment

`aq_assess_business_alignment()` deterministically detects structural
inconsistencies such as:

- objective has no strategy
- objective has no KPI
- strategy has no tactic
- tactic has no lever
- tactic has no guardrail
- lever has no variable mapping
- optimization-eligible lever lacks authority or approval gating
- guardrail variable is being optimized as a lever
- assumption lacks evidence

The assessment returns issue type, severity, affected object, evidence,
recommendation, and whether human input is required.

## Canonical Artifact

`aq_business_intent_artifact()` creates a `business_intent_artifact` using the
existing AutoQuant canonical artifact envelope. The artifact contains:

- mission/objective/strategy/tactic/lever records
- KPI, guardrail, constraint, risk, assumption, recommendation, decision records
- authority and coverage records
- relationships
- validation diagnostics
- alignment findings
- explore/exploit assessments
- variable semantics reference
- supported actions

No new artifact philosophy is introduced.

## Semantic Integration

Phase 2 validates compatibility with Phase 1 variable semantics:

- referenced lever variables can be checked against semantic variables
- KPI source variables can be checked against semantic variables
- optimization-eligible levers can be checked for optimization-eligible variable semantics
- control and adjustment variables are not silently promoted to levers
- guardrails remain distinct from primary optimization targets

Business roles do not globally assign causal roles. Context-relative causal
semantics remain explicit.

## Downstream Integration Roadmap

| Subsystem | Future consumption |
| --- | --- |
| Rodeo | semantic feature generation, feature-role inheritance, safe interactions, control/lever transformation boundaries, constraint-aware preparation |
| AutoQuant supervised learning | role-aware specs, controllable vs non-controllable importance, causal interpretation warnings |
| Forecasting | planned lever values, known-future controls, scenario variables, conditional forecasts, lever-specific experiments |
| Scoring and monitoring | lever range monitoring, driver vs context drift, realized decision outcomes |
| Reporting | objective, strategy, tactic, lever, controls, states, risks, assumptions, evidence, decisions, outcomes |
| Campaigns | assumption testing, lever calibration, strategy validation, exploration vs exploitation, knowledge promotion |
| Optimization | authorized eligible levers only, explicit constraints, validated ranges, cost, uncertainty, authority |
| GenAI | bounded business context, authority, unsupported claims, escalation, missing information |
| Future causal methods | exposure, outcome, confounders, mediators, modifiers, timing, estimands, identification assumptions |
| Future experiment design | lever, hypothesis, estimand, assignment unit, guardrails, feasibility, authority, value of information |

## Model-Family Gap Assessment

| Family | Business question answered | Required semantics | Priority | Deferred because |
| --- | --- | --- | --- | --- |
| Causal regression / doubly robust | What is the effect of a lever under assumptions? | exposure, outcome, confounders, timing, estimand | High | needs causal design contract |
| Uplift / HTE | Who benefits from a tactic or lever? | treatment, outcome, moderators, guardrails | Medium | needs assignment and policy semantics |
| Difference-in-differences | Did a policy/tactic change alter outcomes? | treated group, control group, time, outcome | Medium | needs panel intervention design |
| Synthetic control | What happened versus constructed counterfactual? | unit, donor pool, intervention, outcome | Medium | needs donor eligibility rules |
| Interrupted time series | Did a change shift level/trend? | intervention time, outcome, controls | Medium | needs intervention artifact |
| Regression discontinuity | What is local threshold effect? | running variable, threshold, outcome | Low | narrow design requirements |
| Instrumental variables | What effect is identified by an instrument? | exposure, outcome, instrument, exclusion assumptions | Low | strong identification burden |
| Mediation analysis | How does a strategy mechanism operate? | exposure, mediator, outcome, timing | Medium | needs mechanism and mediator contract |
| SEM | How do latent constructs and paths relate? | latent constructs, indicators, paths | Low | needs measurement model contract |
| Bayesian networks | What dependency graph is plausible? | nodes, candidate causal roles, priors | Low | needs graph structure and assumptions |
| State-space / latent-state | What hidden state drives outcomes? | observed state, latent state, transitions | Medium | needs state contract |
| MMM | How should media levers be calibrated? | media levers, controls, saturation, carryover, constraints | High | needs MMM-specific evidence contracts |
| Constrained optimization | Which authorized lever values maximize utility? | levers, objective, constraints, guardrails, authority | High | needs optimizer and approval contract |
| Contextual bandits | Which action should be explored online? | action, context, reward, guardrails | Future | needs online experimentation governance |
| Reinforcement learning | What policy maximizes long-run reward? | state, action, reward, transitions, safety | Future | high governance and deployment risk |
| Decision analysis | Which decision has highest expected utility? | utility, uncertainty, alternatives, costs | Medium | needs probability and utility elicitation |
| Portfolio allocation | How should resources move across domains? | enterprise objective, levers, constraints, authority | Future | requires cross-domain coverage |

The decision rule remains: add a model family only when it answers a materially
different organizational question or enables a decision current operators
cannot credibly support.

## Reporting and Executive Breakout Design

Future reports should support scope-specific views for analyst, channel owner,
functional leader, CMO, CFO, COO, CEO, board, and executive summary audiences.
Likely sections include:

- mission and objectives
- strategy and tactic status
- lever portfolio
- exploitable levers
- exploration candidates
- controls and contextual conditions
- constraints and guardrails
- key risks and assumptions
- evidence strength
- active experiments
- decisions awaiting approval
- decisions outside authority
- misalignment
- recommended next actions

## Strategy and Tactic Adjustment Lifecycle

Future phases should support evidence-backed transitions:

- retain strategy
- strengthen strategy
- narrow applicability
- modify tactic
- expand tactic
- constrain tactic
- test assumption
- suspend tactic
- retire tactic
- escalate decision
- revise KPI
- revise objective
- collect missing evidence

Phase 2 creates the record structure and deterministic recommendation language.
It does not automatically revise strategy.

## Validation

```r
qa_business_intent_framework()
qa_autoquant_package()
```

## Remaining Limitations

- No enterprise optimizer: requires authorized lever, constraint, utility, and approval contracts to mature first.
- No causal estimator: requires estimand and design contracts.
- No MMM: requires media-specific carryover/saturation contracts.
- No automatic strategy adjustment: requires decision outcome and governance lifecycle.
- No graph database: deterministic relationship tables are sufficient for Phase 2.
- No downstream operator rewrites: this phase establishes contracts and compatibility.

## Phase 3 Recommendation

The safest highest-leverage Phase 3 is to integrate business intent into
planning and reporting consumers without changing modeling behavior:

1. expose business intent artifacts in AnalyticsShinyApp,
2. show alignment and explore/exploit findings,
3. allow report sections by objective, strategy, tactic, and lever,
4. let campaign planning consume assumptions and lever uncertainty,
5. keep optimization and causal estimation deferred until evidence contracts are stronger.
