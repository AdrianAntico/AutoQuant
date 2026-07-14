# Causal Intelligence Phase 5: Randomized Design Analysis

Phase 5 deepens randomized causal evidence without broadening into observational causal estimation.

The governing rule remains:

```text
randomized assignment
-> completed-experiment readiness
-> frozen ITT specification
-> design-aware randomized analysis
-> uncertainty, guardrails, robustness, and reporting
-> decision learning
```

## Scope

The framework supports a bounded randomized-analysis depth contract:

- governed ANCOVA through approved pre-treatment covariates;
- CUPED-style variance reduction through an approved pre-period metric;
- blocked and stratified randomized evidence summaries;
- cluster and geographic assignment diagnostics;
- conservative switchback and stepped-wedge foundations with explicit carryover evidence;
- explicit factorial design foundation for planned two-factor designs;
- bounded randomization inference for supported assignment mechanisms;
- multiplicity policy metadata;
- guardrail decision logic;
- materiality regions;
- a robustness matrix that preserves the primary analysis;
- a causal-effect report contract.

The primary effect remains assignment-based ITT. Variance reduction does not change the estimand.

## Key Functions

- `aq_randomized_design_analysis_spec()`
- `aq_validate_randomized_design_analysis_spec()`
- `aq_randomized_design_method_eligibility()`
- `aq_randomized_cuped_adjustment()`
- `aq_randomization_inference()`
- `aq_enforce_randomized_outcome_windows()`
- `aq_assess_randomized_carryover()`
- `aq_randomized_multiplicity_adjust()`
- `aq_randomized_guardrail_decision()`
- `aq_randomized_materiality_regions()`
- `aq_randomized_robustness_matrix()`
- `aq_analyze_randomized_design_depth()`
- `aq_randomized_causal_effect_report()`
- `qa_causal_randomized_design_analysis_framework()`

## Deliberate Non-Goals

Phase 5 does not implement:

- treatment-on-treated;
- CACE or IV;
- propensity scores;
- matching;
- observational weighting;
- difference-in-differences;
- synthetic control;
- mediation;
- causal forests;
- adaptive experimentation;
- automatic subgroup discovery;
- autonomous decisions.

## Build Artifact Policy

Validation builds should be created in isolated validation directories. The tracked `AutoQuant_1.0.1.tar.gz` is legacy/release repository content and should not be regenerated in-place during routine QA.

## Aggregate Warning Classification

The remaining AnalyticsShinyApp aggregate warning is `module_terminology_consistency`. It is classified as a known terminology compatibility warning because historical documentation intentionally preserves the legacy `autoquant_model_assessment` name while the canonical app-facing pre-model module is `autoquant_model_readiness`.
