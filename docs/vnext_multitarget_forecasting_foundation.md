# AutoQuant vNext Multi-Target Forecasting Foundation

Status: Phase 19 implemented. Phase 20 extends this family with supervised
cross-target feature learning documented in
`docs/vnext_cross_target_forecasting.md`.

Multi-target forecasting is now a first-class forecasting problem family in
AutoQuant vNext. It extends the existing forecasting architecture rather than
introducing a separate multivariate modeling philosophy.

This phase does not implement VAR, VARMAX, multivariate state-space models,
deep learning, target clustering, target-causality modeling, deployment, or
AutoML.

## Philosophy

Multiple targets are not merely several unrelated single-series forecasts.
They are related forecasting objectives that may share:

- a common timeline
- a common forecast horizon
- known future variables
- shared predictors
- preparation lineage
- cross-target evidence

At the same time, each target remains analytically independent unless a future
operator explicitly models joint target behavior. Phase 19 preserves both
target independence and cross-target context.

## Public API

Implemented:

- `aq_multitarget_forecast_spec()`
- `aq_validate_multitarget_forecast_spec()`
- `aq_fit_multitarget_forecast()`
- `aq_assess_multitarget_forecast()`
- `aq_compare_multitarget_strategies()`
- `qa_vnext_multitarget_forecasting_foundation()`

## Specification

`aq_multitarget_forecast_spec()` defines a flat contract:

- `targets`
- `date`
- `frequency`
- `horizon`
- `forecast_origin`
- `known_future_variables`
- `shared_predictors`
- `target_specific_predictors`
- `strategy`
- `engine`
- `target_weights`
- `season_length`
- `dataset_id`
- `supported_downstream_actions`

The expected data shape is wide: one row per timestamp and one numeric column
per target. This keeps the initial contract simple and makes duplicate
date/target validation deterministic.

## Validation

Validation records deterministic diagnostics for:

- duplicate targets
- missing targets
- non-numeric targets
- missing date column
- duplicate date/target combinations
- irregular shared timeline
- forecast origin compatibility
- insufficient target history
- future leakage through target columns declared as predictors
- unknown target-specific predictor mappings
- future known-variable schema

Validation returns diagnostics rather than relying on hidden model errors.

## Forecast Strategies

Phase 19 supports deterministic strategies:

### Independent Targets

Each target is forecast independently under the shared multi-target
specification. This is the conservative baseline and remains the clearest
interpretation when targets have different behavior.

### Shared Workflow

Targets share timeline validation, partitioning, preparation intent, and
artifact evidence. Forecast execution remains target-wise and deterministic.
This is not joint probabilistic multivariate forecasting.

Supported deterministic engines are:

- `naive`
- `seasonal_naive`

Phase 20 additionally supports `engine = "catboost"` for supervised
multi-target forecasting and `strategy = "cross_target_features"` for
Rodeo-prepared cross-target feature learning.

These engines establish the problem-family contract without taking on the risk
of advanced multivariate modeling too early.

## Rodeo Integration

Rodeo remains the owner of temporal preparation contracts. Phase 19 records
Rodeo temporal metadata per target where Rodeo is available. This gives future
supervised or shared-feature operators a stable lineage path without duplicating
temporal feature engineering inside AutoQuant.

## Cross-Target Evidence

`aq_fit_multitarget_forecast()` produces descriptive cross-target evidence:

- target identities
- target weights
- historical target summaries
- pairwise target correlations
- shared predictor usage
- target-specific predictor usage
- forecast divergence by horizon

This evidence is descriptive. It does not assert causal target interaction.

## Artifacts

Forecast results produce canonical table artifacts with vNext envelopes:

- task: `multi_target_forecast`
- operator: `forecast`
- artifact type: `multi_target_forecast`
- target identities
- shared specification
- forecast horizon
- forecast origin
- Rodeo temporal metadata where available
- cross-target evidence
- supported downstream actions

These artifacts are intended to integrate naturally with AnalyticsShinyApp,
campaigns, reports, and future experimentation.

## Assessment

`aq_assess_multitarget_forecast()` produces:

- aggregate metrics
- per-target metrics
- per-horizon metrics
- per-target/per-horizon metrics
- weighted metrics
- target comparison summaries
- cross-target evidence

Weighted metrics are advisory evidence. They do not replace target-level
diagnostics.

## Comparison

`aq_compare_multitarget_strategies()` compares:

- `independent`
- `shared_workflow`

The recommendation is advisory and based on weighted RMSE when realized actuals
are available. This is not automatic production strategy selection.

## Historical Capability Intent

The restored historical `AutoCatBoostVectorCARMA()` implementation showed that
AutoQuant previously aimed to support:

- multiple target columns
- shared date and group structure
- shared external regressors
- temporal feature generation
- target transformations
- target-level forecast outputs
- model evaluation over multiple related outputs
- reportable forecast evidence

## Modern Architectural Placement

The modern successor is not the historical Vector API. The successor is the
artifact-first multi-target forecasting family:

```text
Forecasting
    -> Single Series
    -> Panel
    -> Hierarchy
    -> Intermittent Demand
    -> Funnel
    -> Multi-Target
```

The historical implementation is treated as evidence of analytical intent, not
as an API or architecture to preserve.

## Retired Historical Assumptions

Phase 19 intentionally does not preserve:

- giant engine-specific wrappers
- hidden feature construction inside a monolithic function
- implicit file writing
- CatBoost MultiRMSE as the only multi-target path
- GPU/CPU tuning complexity
- grid tuning
- target transformation search
- direct report generation inside the modeling function

## Future Specialization Opportunities

Future phases may add:

- target-specific model families
- target relationship diagnostics
- target grouping
- joint residual analysis
- VAR or state-space operators
- target-aware campaign investigations

Those future operators should inherit this specification, artifact, assessment,
comparison, and evidence contract.
