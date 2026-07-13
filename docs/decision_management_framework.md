# Decision Management Framework

Semantic Intelligence Phase 3 adds canonical decision-management contracts to
AutoQuant. The framework links business intent and variable semantics to
decision contexts, alternatives, criteria, financial impact, uncertainty,
optionality, recommendations, decision records, and outcome follow-up.

The central object is `aq_decision_context()`.

## Contract Summary

`aq_decision_context()` contains:

- `context`: the decision question, owner, authority, scope, deadline, and
  relationship to mission/objective/strategy/tactic/lever records.
- `alternatives`: the candidate actions, including a baseline/current-policy
  alternative.
- `criteria`: explicit evaluation criteria and hard constraints.
- `financial_impacts`: expected value, cost, upside, downside, opportunity cost,
  source type, and confidence.
- `uncertainties`: structured uncertainty evidence, including reducibility and
  decision sensitivity.
- `optionality`: future choices created, preserved, constrained, or foreclosed.
- `recommendations`: proposed action, viable alternatives, evidence basis, and
  required approvals.
- `decisions`: selected alternative, approver, decision date, and review date.
- `outcomes`: follow-up records for realized outcomes and learning.
- `business_intent`: optional `aq_business_intent()` object.
- `variable_semantics`: optional `aq_variable_semantics()` object.

The decision context is intentionally richer than a recommendation. It preserves
what was considered, why it was considered, what was uncertain, and what should
be reviewed later.

## Public API

Core creation and validation:

- `aq_decision_context()`
- `aq_decision_alternative()`
- `aq_decision_optionality()`
- `aq_validate_decision_context()`

Assessment helpers:

- `aq_assess_decision_alternatives()`
- `aq_assess_decision_optionality()`

Artifact creation:

- `aq_decision_context_artifact()`

QA:

- `qa_decision_management_framework()`

## Alternative Assessment

`aq_assess_decision_alternatives()` produces deterministic alternative-level
evidence. It currently records:

- financial net benefit
- simple ROI
- downside and upside estimates
- uncertainty count
- high-sensitivity uncertainty count
- reducible uncertainty count
- optionality category
- authority failure
- hard-constraint failure
- coverage limitation
- missing information
- dominated status
- recommendation category
- required human judgment
- supported next actions

The function does not collapse all tradeoffs into a single total score. A future
decision UI or GenAI layer may summarize tradeoffs, but the canonical object
preserves the individual evidence dimensions.

## Optionality Assessment

`aq_assess_decision_optionality()` categorizes optionality evidence as:

- `high_positive_optionality`
- `moderate_positive_optionality`
- `negative_optionality`
- `material_options_foreclosed`
- `insufficient_information`

Optionality is treated as analytical evidence because it affects whether a user
should explore, exploit, defer, stage, expand, abandon, or collect more evidence.
In other words, optionality is one of the explicit inputs to explore/exploit
decision framing.

## Decision Artifacts

`aq_decision_context_artifact()` emits a canonical table artifact whose primary
data is the alternative assessment. Its metadata includes the full context,
alternatives, criteria, financial impacts, uncertainties, optionality,
recommendations, decisions, outcomes, validation diagnostics, business-intent
id, variable-semantics id, and supported downstream actions.

This allows downstream systems to use the decision artifact without rebuilding
decision logic:

- Analytics Workstation project collector
- campaign review
- GenAI context routing
- reporting
- knowledge promotion
- future outcome learning

## Relationship to Business Intent

Business intent describes mission, objective, strategy, tactic, lever, KPI,
guardrail, risk, authority, coverage, and assumptions. Decision management uses
those records as context for deciding what action to take.

The relationship is:

```
Business intent
-> Variable semantics
-> Analytical evidence
-> Decision context
-> Alternatives
-> Recommendation
-> Decision record
-> Outcome follow-up
```

Phase 3 does not implement a full optimization engine. It records the decision
contract that such an engine would need to respect.

## Application Integration Roadmap

Analytics Workstation should expose Semantic Intelligence as a decision workbench
rather than as isolated technical records. The smallest coherent interface should
show:

- active business objective
- current decision question
- alternatives and baseline
- authority / coverage status
- financial evidence
- uncertainty and optionality evidence
- recommended next action
- downstream artifacts

Mission Control should surface signals such as:

- decision context missing
- no baseline alternative
- high uncertainty
- authority escalation required
- outcome follow-up due

GenAI context should receive decision artifacts as structured evidence, not as
raw model output.

## AutoPlots QA Classification

The Phase 3 cross-repository validation includes an AutoPlots installed-package
surface check. If `qa_autoplots_package()` is not exported by the installed
AutoPlots package, this should be classified as an AutoPlots package QA surface
gap rather than an AutoQuant decision-management failure. AutoQuant decision
artifacts rely on canonical table artifacts and do not require AutoPlots-specific
rendering for the core contract.

## Current Boundaries

The framework intentionally does not implement:

- autonomous action execution
- budget optimization
- stochastic decision trees
- portfolio allocation
- full causal decision proof
- learned routing
- automatic outcome learning

Those capabilities can build on the decision contract later.
