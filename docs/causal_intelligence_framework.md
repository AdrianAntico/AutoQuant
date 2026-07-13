# Causal Intelligence Framework

This document describes the implemented Phase 1 Causal Intelligence contracts in AutoQuant.

Phase 2 governed experiment-design planning is documented separately in [Causal Experiment Design Framework](causal_experiment_design_framework.md). Phase 2 consumes Phase 1 causal questions and produces experiment-design artifacts without treatment execution or effect estimation.

## Lifecycle

```text
Authored decision
-> lever/intervention
-> outcome
-> causal question
-> estimand
-> causal context
-> variable roles
-> graph relationships
-> identification planning
-> adjustment guidance
-> design eligibility
-> missing evidence
-> planning artifact
```

The framework is planning-only. It does not fit causal estimators and does not claim causal effects.

## Core Objects

### Causal Question

Created by `aq_causal_question()`.

Required fields include:

- `decision_context_id`
- `exposure`
- `outcome`
- `population`
- `unit_of_analysis`
- `time_zero`
- `treatment_window`
- `outcome_window`
- `comparison_condition`
- `intervention_definition`
- `estimand`
- `effect_scale`
- `target_population`

The validator rejects vague intervention/comparison definitions and exposure/outcome collisions.

### Estimand

Created by `aq_estimand()`.

The estimand records the target effect, not the estimator:

- estimand type,
- treatment contrast,
- outcome,
- population,
- time horizon,
- effect scale,
- conditioning variables,
- assumptions,
- limitations,
- `implementation_status = "planning_only"`.

### Causal Variable Roles

Created by `aq_causal_variable_roles()`.

Roles are question-relative, not global facts. Supported roles include:

- exposure,
- outcome,
- confounder candidate,
- mediator candidate,
- collider candidate,
- effect modifier,
- instrument candidate,
- selection variable,
- prognostic precision variable,
- spillover/interference variable,
- state variable,
- unknown.

### Causal Relationships

Created by `aq_causal_relationships()`.

Relationships describe directed assumptions such as `causes`, `may_cause`, `mediates`, `determines_treatment_assignment`, and `determines_selection`. They are not learned automatically in Phase 1.

### Causal Context

Created by `aq_causal_context()`.

The context combines the question, estimand, roles, relationships, assumptions, semantic references, and candidate designs. It automatically attaches:

- graph diagnostics,
- adjustment guidance,
- identification assessment,
- design eligibility.

## Diagnostics

### Graph Diagnostics

`aq_validate_causal_graph()` checks for:

- unknown node references,
- duplicate edges,
- self loops,
- cycles,
- disconnected exposure/outcome mechanisms,
- timing concerns,
- instrument exclusion warnings,
- invalid outcome adjustment.

### Adjustment Guidance

`aq_adjustment_guidance()` classifies variables as:

- required adjustment candidate,
- optional precision adjustment,
- mediator--depends on estimand,
- instrument candidate--not ordinary adjustment,
- collider risk,
- post-treatment risk,
- selection model candidate,
- do not adjust,
- insufficient information.

This is guidance, not a mathematically complete adjustment-set optimizer.

### Identification Assessment

`aq_assess_identification()` reports planning statuses such as:

- plausibly identifiable,
- identifiable under stated assumptions,
- selection-bias concern,
- interference concern,
- insufficient information,
- conflicting causal structure.

The result is deterministic and assumption-aware.

### Design Eligibility

`aq_assess_causal_designs()` evaluates candidate designs against authored evidence and assumptions. Designs include randomized experiments, geo experiments, switchbacks, difference-in-differences, event studies, synthetic controls, regression discontinuity, instruments, matching, weighting, doubly robust designs, mediation, heterogeneity, longitudinal policy designs, contextual bandits, and reinforcement learning.

Eligibility is advisory. It does not create an estimator.

## Planning Artifact

`aq_causal_planning_artifact()` creates a canonical artifact containing:

- causal question,
- estimand,
- roles,
- graph relationships,
- diagnostics,
- adjustment guidance,
- identification assessment,
- eligible designs,
- investigation plan,
- prohibited claims.

The artifact metadata includes `no_effect_estimated = TRUE`.

## QA

Run:

```r
devtools::load_all(".")
qa_causal_intelligence_framework()
```

The QA verifies causal question validation, estimand contracts, role families, DAG diagnostics, adjustment guidance, identification assessment, design eligibility, planning artifacts, and the predictive-vs-causal boundary.
