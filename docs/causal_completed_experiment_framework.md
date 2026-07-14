# Causal Completed-Experiment Evidence Framework

Phase 3 records what happened after a governed experiment was run. It is an evidence-ingestion and analysis-readiness layer, not a treatment executor and not a causal estimator.

The framework preserves the separation between:

- planned assignment
- realized assignment
- treatment delivery
- exposure
- compliance
- outcomes
- guardrails
- exclusions
- estimand preservation
- analysis readiness

The default estimand posture is intent-to-treat. Treatment-received evidence may be recorded, but it must not replace original assignment unless a new estimand is explicitly authored.

## Contracts

Use `aq_completed_experiment()` to link a completed or in-progress execution record back to the approved experiment plan, decision context, causal question, estimand, design version, and assignment version.

Use the evidence helpers to ingest source evidence:

- `aq_assignment_evidence()`
- `aq_treatment_delivery_evidence()`
- `aq_exposure_evidence()`
- `aq_compliance_evidence()`
- `aq_outcome_evidence()`
- `aq_exclusion_evidence()`

Use readiness helpers to classify the evidence without estimating effects:

- `aq_reconcile_experiment_execution()`
- `aq_assess_randomization_integrity()`
- `aq_assess_realized_balance()`
- `aq_assess_missingness_attrition()`
- `aq_assess_treatment_fidelity()`
- `aq_assess_interference_spillover()`
- `aq_assess_guardrails()`
- `aq_assess_estimand_preservation()`
- `aq_assess_experiment_analysis_readiness()`

Use `aq_planned_analysis_record()` to create a handoff record for a future estimator. The record states what can be estimated later, which assignment variable is preserved, and that no effect has been estimated.

Use `aq_completed_experiment_evidence_artifact()` to package the evidence into a canonical artifact for downstream collectors, applications, and future campaign reasoning.

## Readiness States

`aq_analysis_readiness_states()` defines the deterministic readiness states:

- `ready_for_itt`
- `ready_for_itt_with_adjustment`
- `ready_with_major_limitations`
- `outcome_pending`
- `measurement_incomplete`
- `assignment_integrity_review`
- `analysis_population_review`
- `estimand_revision_required`
- `descriptive_only`
- `blocked`
- `invalid_for_planned_estimand`
- `insufficient_information`

Readiness is an evidence classification. It is not an effect estimate, a hypothesis test, or a launch recommendation.

## Guardrails

Guardrails are decision evidence. A favorable primary outcome does not override missing or breached guardrails. The framework records guardrail evidence separately so future decision workflows can reason about benefit and risk without collapsing both into one metric.

## Exclusions

Exclusions are preserved with stage and analysis impact. Post-assignment exclusions are estimand-relevant and trigger review instead of being silently applied.

## Validation

Run:

```r
qa_causal_completed_experiment_framework()
```

Validated script: `inst/examples/causal_completed_experiment_framework.R`
