# Causal Experiment Design Framework

AutoQuant Causal Intelligence Phase 2 adds governed experiment-design planning.

The framework is design-before-estimator. It creates experiment questions, design specifications, assignment proposals, measurement plans, validity-threat registers, approval gates, information-value assessments, and canonical plan artifacts. It does not execute treatments and does not estimate causal effects.

## Lifecycle

```text
Causal question
-> experiment question
-> design specification
-> assignment proposal
-> balance diagnostics
-> power and precision plan
-> timing plan
-> measurement plan
-> validity-threat register
-> interference plan
-> approval gate
-> information-value assessment
-> experiment plan artifact
```

The result is a governed plan. It is not an experiment result.

## Core Objects

### Experiment Question

Created by `aq_experiment_question()`.

The question links the experiment back to a causal question and decision context. It records:

- hypothesis,
- null and alternative claims,
- treatment,
- comparison,
- estimand,
- assignment population,
- expected mechanism,
- primary outcome,
- guardrails,
- decision rule,
- authority,
- coverage.

The validator blocks incomplete questions and warns when guardrails are missing.

### Design Specification

Created by `aq_experiment_design_spec()`.

The design specification separates:

- assignment unit,
- treatment delivery unit,
- analysis unit,
- cluster unit,
- blocking variables,
- stratification variables,
- allocation ratios,
- rollout or washout assumptions,
- exposure rules,
- compliance expectations,
- contamination and interference assumptions.

Supported design families include:

- individual randomized A/B,
- stratified randomized,
- blocked randomized,
- cluster randomized,
- geographic randomized,
- switchback,
- stepped wedge,
- factorial,
- randomized encouragement.

### Assignment Plan

Created by `aq_assignment_plan()`.

The assignment plan is deterministic for the same eligible population, seed, strata, blocks, arms, and allocation ratios. It is a proposal only. It does not execute treatment.

### Balance Diagnostics

Created by `aq_assess_assignment_balance()`.

Diagnostics include treatment counts, sparse strata warnings, and standardized baseline differences for supplied pre-treatment variables.

### Power and Timing Plans

`aq_power_plan()` calculates approximate sample requirements only when required assumptions are supplied. Missing assumptions are recorded rather than fabricated.

`aq_experiment_timing_plan()` records treatment duration, pre-period, washout, maturation, reporting delay, decision deadline, and scheduling conflicts.

### Measurement Plan

Created by `aq_measurement_plan()`.

The measurement contract distinguishes:

- assignment,
- exposure verification,
- compliance,
- treatment receipt,
- primary outcome,
- secondary outcomes,
- guardrails,
- source of truth,
- data quality.

### Validity and Interference

`aq_validity_threat_register()` records internal, statistical, and construct threats such as assignment failure, noncompliance, contamination, interference, carryover, attrition, insufficient precision, weak treatment representation, and KPI mismatch.

`aq_interference_plan()` makes spillover and SUTVA assumptions explicit instead of silent.

### Approval Gate

`aq_experiment_gate_assessment()` blocks or escalates incomplete plans. Even an approved plan has `execution_ready = FALSE` because treatment execution is out of scope for Phase 2.

### Information Value

`aq_experiment_information_value()` records whether the experiment appears worth continuing based on decision sensitivity, lever importance, cost, duration, reversibility, and optionality.

## Canonical Artifact

`aq_experiment_plan_artifact()` creates a canonical metadata artifact with:

- experiment question,
- design specification,
- assignment plan,
- power plan,
- timing plan,
- measurement plan,
- validity threats,
- interference plan,
- gate assessment,
- information value,
- lineage,
- prohibited claims.

The artifact explicitly records:

- `no_treatment_executed = TRUE`,
- `no_effect_estimated = TRUE`.

## Phase Boundary

Supported:

- experiment-design planning,
- deterministic assignment proposals,
- balance diagnostics,
- power and timing assumptions,
- measurement readiness,
- validity threat planning,
- authority and coverage gates,
- plan artifacts.

Not supported:

- treatment execution,
- exposure delivery,
- causal effect estimation,
- completed-experiment analysis,
- autonomous approval,
- automatic randomization in production systems.

Future phases may add completed-experiment analysis after design artifacts, assignments, exposure logs, outcomes, compliance data, and approval records exist.
