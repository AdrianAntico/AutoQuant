# Governed Observational Causal Estimation Framework

AutoQuant now supports the first conservative observational effect-estimation slice.

This estimator consumes the Phase 1 observational planning artifacts. It does not replace target-trial thinking, assignment documentation, adjustment planning, temporal validation, overlap review, or readiness gates.

## Supported Scope

- Binary treatment only.
- ATE and ATT only.
- Continuous and binary outcomes.
- Logistic-regression propensity model.
- Nearest-neighbor matching diagnostics.
- Deterministic ATE, ATT, and overlap-style weights.
- Standardized mean difference and variance-ratio balance checks.
- Linear or logistic outcome regression.
- AIPW as the only doubly robust estimator.

## Governance

Estimation requires:

- approved design;
- frozen design hash;
- readiness state of `ready_for_design_implementation` or `ready_with_strong_assumptions`;
- explicit treatment and outcome columns;
- explicit human-approved adjustment variables.

The framework does not automatically select confounders, search estimators, or relax readiness failures.

## Claim Controls

Permitted claims are conditional on the frozen design and explicit assumptions.

Prohibited claims include:

- treatment was randomized;
- balance proves no unmeasured confounding;
- the estimate applies outside the frozen population, treatment, outcome, or estimand;
- alternative estimators were searched until a favorable result was found.

## Artifact Contract

`aq_observational_effect_artifact()` creates canonical effect evidence with:

- specification and frozen design hash;
- propensity diagnostics;
- matching diagnostics;
- weight diagnostics and effective sample size;
- balance diagnostics and love-plot data;
- primary AIPW estimate;
- sensitivity reminders;
- assumptions;
- permitted and prohibited claims.

The artifact is evidence for human review. It is not an automatic decision.
