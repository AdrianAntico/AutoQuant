# Causal Randomized ITT Estimation Framework

Phase 4 is the first causal estimation slice in AutoQuant Causal Intelligence.

It supports conservative randomized intent-to-treat estimation only. The estimator is allowed to run only after Phase 3 completed-experiment evidence has been ingested and classified as ITT-compatible. The framework preserves original assignment, planned estimands, guardrails, materiality thresholds, missingness sensitivity, and review lifecycle metadata.

## Boundary

Supported:

- randomized completed-experiment ITT readiness gating,
- frozen ITT analysis specifications,
- randomized analysis population construction from original assignment,
- continuous difference-in-means ITT estimates,
- binary risk-difference ITT estimates,
- secondary binary relative-risk and odds-ratio summaries,
- approved pre-treatment precision adjustment,
- HC0 and cluster-aware uncertainty when cluster identifiers are available,
- explicit missing-outcome sensitivity for binary outcomes,
- materiality assessment separate from p-values,
- guardrail preservation,
- canonical effect artifacts,
- governed review lifecycle states,
- campaign seed suggestions for inconclusive, harmful, or limited evidence.

Not supported:

- observational treatment-effect estimation,
- treatment-on-treated or CACE/TOT estimation,
- instrumental variables,
- propensity scores,
- matching,
- difference-in-differences,
- synthetic controls,
- mediation,
- causal forests,
- adaptive experiments,
- inventory or business optimization,
- autonomous decisioning.

## Core Contracts

Use `aq_randomized_itt_spec()` to author the frozen estimator request. The spec links the completed experiment, causal question, estimand, assignment contrast, outcome, outcome type, approved covariates, uncertainty method, missing-outcome policy, sensitivity plan, and minimum meaningful effect.

Use `aq_validate_randomized_itt_readiness()` before estimation. It rejects blocked, descriptive-only, stale, unapproved, or estimand-incompatible evidence. Cluster identifiers may be supplied by assignment evidence or baseline population evidence.

Use `aq_estimate_randomized_itt()` to run the estimator. It returns:

- the readiness gate,
- the retained randomized analysis population,
- the primary unadjusted ITT estimate,
- optional adjusted precision estimate,
- missingness and guardrail evidence,
- missing-outcome sensitivity,
- materiality evidence,
- permitted and prohibited claims,
- lifecycle state.

Use `aq_randomized_itt_effect_artifact()` to package the result as canonical causal evidence.

Use `aq_review_randomized_itt_result()` to approve or reject the effect artifact for downstream decision evidence.

## Interpretation Rules

The primary estimate is assignment to treatment versus assignment to comparison. It is not treatment received. Compliance, delivery, exposure, and fidelity evidence are diagnostic context unless a later explicitly-authored estimand supports a different estimator.

Statistical significance is not decision readiness. The result must preserve:

- confidence intervals,
- minimum meaningful effect,
- guardrail evidence,
- missingness sensitivity,
- implementation fidelity,
- prohibited claims.

Null and inconclusive results are still valuable evidence. They may reduce uncertainty, reveal implementation problems, or seed follow-up investigations.

## Validation

Run:

```r
qa_causal_randomized_itt_framework()
```

The QA verifies specification validation, Phase 3 readiness gating, randomized population retention, continuous and binary estimates, adjustment preservation, cluster-aware uncertainty, missingness sensitivity, guardrails, materiality, artifacts, lifecycle review, campaign seeds, and prohibited claims.
