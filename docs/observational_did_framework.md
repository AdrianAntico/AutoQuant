# Governed Difference-in-Differences Framework

AutoQuant supports a conservative classic two-group Difference-in-Differences estimator.

This is not a generalized DiD framework. It intentionally excludes staggered adoption, event studies, synthetic DiD, synthetic control, matrix completion, cohort estimators, and generalized TWFE.

## Required Sequence

Decision -> Target Trial -> Phase 1 Observational Readiness -> DiD Readiness -> Frozen DiD Design -> Pre-Period Diagnostics -> Parallel-Trends Assessment -> Composition Stability -> Governed DiD Estimate -> Sensitivity -> DiD Effect Artifact.

## Supported Scope

- Binary treated group.
- Untreated comparison group.
- Known intervention date.
- No treatment reversal.
- Continuous and binary outcomes through a classic linear DiD contrast.
- Optional cluster-robust standard errors.
- ATT-oriented interpretation.

## Diagnostics

The framework preserves:

- group means over time;
- pre-period slopes;
- slope differences;
- placebo pre-period regression;
- trend support classification;
- composition entry and exit;
- missingness and coverage diagnostics.

Parallel trends are never treated as proven. They are classified as strong support, moderate support, weak support, unsupported, or unknown.

## Claim Governance

Permitted claims are conditional on the frozen design and explicit assumptions.

Prohibited claims include:

- parallel trends were proven;
- the result supports staggered-adoption, event-study, synthetic-control, or generalized TWFE claims;
- the comparison group was optimized after seeing the effect;
- the result applies outside the frozen population, timing, treatment, comparison, outcome, or estimand.

## Artifact Contract

`aq_did_effect_artifact()` creates canonical time-based causal evidence with readiness, frozen design hash, pre-period diagnostics, parallel-trends evidence, composition stability, estimate, sensitivity reminders, assumptions, and claim governance.
