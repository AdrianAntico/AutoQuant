# Observational Causal Planning Framework

AutoQuant now supports observational causal-study planning before any observational effect estimator exists.

The framework answers whether an observational causal analysis can credibly be attempted. It does not estimate treatment effects, does not declare ignorability, and does not automatically select confounders from predictive features.

## Lifecycle

Decision context -> causal question -> observational study -> target-trial specification -> assignment mechanism -> treatment/comparison definition -> adjustment set -> temporal eligibility -> treatment variation -> balance -> overlap -> selection/missingness -> unmeasured-confounding risk -> falsification plan -> estimator eligibility -> readiness -> planning artifact.

## Core Contracts

- `aq_observational_study()` records population, unit, treatment, comparison, timing, windows, authority, coverage, and lineage.
- `aq_target_trial_spec()` describes the hypothetical experiment the observational data are attempting to emulate.
- `aq_observational_assignment_mechanism()` records why some units received treatment and others did not.
- `aq_observational_treatment_definition()` separates treatment, comparison, thresholds, persistence, crossover, and contamination risks.
- `aq_observational_adjustment_spec()` records approved confounder candidates and prohibited mediators, colliders, and post-treatment variables.
- `aq_assess_observational_temporal_eligibility()` validates whether variables are pre-treatment, ambiguous, post-treatment, or invalid for adjustment.
- `aq_assess_observational_variation()` checks treatment/comparison support rather than equating row count with causal support.
- `aq_observational_assignment_model_diagnostics()` records treatment-probability diagnostics for design only.
- `aq_assess_observational_overlap()` classifies overlap and positivity support.
- `aq_assess_observational_balance()` reports standardized mean differences and categorical distribution differences without p-value dependence.
- `aq_observational_design_eligibility()` assesses readiness for matching, weighting, doubly robust, DiD, synthetic control, RD, IV, and negative controls.
- `aq_assess_observational_estimation_readiness()` returns a governed readiness state.
- `aq_observational_causal_planning_artifact()` registers canonical planning evidence.

## Prohibited Claims

The planning artifact explicitly prohibits claims that:

- observational data prove randomization;
- ignorability is established automatically;
- balance proves there is no unmeasured confounding;
- this phase estimated an observational treatment effect.

## Negative Readiness

`experiment_preferred`, `blocked`, `unidentified`, and `descriptive_only` are valid outputs. A refusal to estimate can be decision evidence when overlap, timing, assignment documentation, adjustment, or selection evidence is inadequate.

## Future Estimator Phases

This phase intentionally leaves matching effects, IPW effects, doubly robust estimation, DiD estimation, synthetic control, RD, IV, mediation, and causal forests unimplemented. The first estimator should be selected from actual readiness evidence, not from an API wish list.
