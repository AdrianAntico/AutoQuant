# AutoQuant vNext Cross-Target Forecasting

Status: Phase 20 implemented. Phase 21 adds deterministic forecasting
capability planning that treats cross-target features as a feature-tuning
hypothesis to test, not as an automatic strategy default.

Cross-target forecasting extends the multi-target forecasting family with
supervised CatBoost models that can use leakage-safe prior information from
other targets. It does not introduce a separate forecasting architecture.

This phase does not implement VAR, VARMAX, multivariate state-space models,
deep learning, target clustering, AutoML, deployment, or target-causality
modeling.

## Philosophy

Cross-target information is a hypothesis. Some targets may help forecast other
targets. Some targets may add noise. Some may help at short horizons and hurt at
longer horizons.

AutoQuant therefore treats cross-target features as evidence to test rather
than a modeling default. The system should be able to conclude either:

- these targets help each other
- these targets should remain independent
- the answer varies by target
- the answer varies by horizon
- the available evidence is insufficient

The implementation is descriptive and empirical. It does not claim causal
target influence.

## Public API

Phase 20 extends the existing multi-target API:

- `aq_multitarget_forecast_spec()`
- `aq_validate_multitarget_forecast_spec()`
- `aq_fit_multitarget_forecast()`
- `aq_assess_multitarget_forecast()`
- `aq_compare_multitarget_strategies()`
- `qa_vnext_multitarget_supervised_forecasting()`

No separate cross-target fit function is introduced. Cross-target learning is a
strategy within multi-target forecasting.

## Specification Extensions

`aq_multitarget_forecast_spec()` now supports:

- `strategy = "cross_target_features"`
- `engine = "catboost"`
- `cross_target_feature_policy`
- `shared_target_lags`
- `target_specific_lags`
- `shared_rolling_windows`
- `target_relationship_metadata`
- `engine_parameters`

The public API remains flat. Target relationship metadata is recorded as
context only and should not be interpreted as causal evidence.

## Rodeo Ownership Boundary

Cross-target feature preparation belongs to Rodeo.

AutoQuant calls:

```r
Rodeo::rodeo_prepare_cross_target_features()
```

Rodeo produces:

- cross-target lag features
- cross-target rolling mean features
- deterministic calendar features
- known future variable features
- feature manifests
- leakage diagnostics
- preparation identities

AutoQuant consumes the prepared features and fits supervised forecasts. It does
not duplicate cross-target feature engineering logic.

## Leakage Policy

The leakage policy is explicit:

- same-period target leakage is prevented by using only rows with dates before
  the feature row
- future target leakage is prevented because future target values are not
  required and are not used
- forecast-origin leakage is prevented by restricting training labels and
  features to information available at the forecast origin
- recursive leakage is prevented because supervised multi-target CatBoost uses
  direct horizon models in this contract

Validation records these checks so downstream systems can inspect the policy.

## Forecast Strategies

`aq_compare_multitarget_strategies()` compares:

- `independent`
- `shared_workflow`
- `cross_target_features`

For `engine = "catboost"`, all three are supervised direct-horizon workflows.
The cross-target strategy differs because Rodeo prepares all target histories as
candidate features for each target model.

## Cross-Target Evidence

Forecast results include:

- cross-target feature importance
- target contribution summaries
- shared predictor diagnostics
- target divergence
- cross-target preparation identities
- leakage diagnostics
- target relationship metadata

Assessment results add feature usefulness summaries by target and feature role.

Strategy comparison adds negative-transfer evidence by comparing cross-target
RMSE against independent RMSE for each target.

## Advisory Recommendations

Recommendations remain advisory. Supported labels are:

- `independent_preferred`
- `shared_workflow_preferred`
- `cross_target_preferred`
- `strategy_varies_by_target`
- `strategy_varies_by_horizon`
- `evidence_insufficient`

No strategy is selected automatically.

## Canonical Artifact Integration

Cross-target forecasts remain standard vNext artifacts:

- task: `multi_target_forecast`
- operator: `forecast`
- artifact type: `multi_target_forecast`
- supported actions: `assess`, `compare`, `report`, `campaign_review`

Metadata records cross-target preparation identity, diagnostics, target
relationship metadata, feature importance, and evidence tables.

## Historical Archaeology

The historical Vector implementation suggested that AutoQuant should support
multiple targets, shared regressors, target-level forecast evidence, and a
large family of candidate feature mechanisms. Phase 20 preserves the
cross-target analytical intent without preserving the historical API. Phase 21
adds a mechanism inventory that classifies cross-target feature construction as
future `feature_tuning`, because it is a competing feature hypothesis rather
than a permanent forecasting operator.

The modern contract differs in three ways:

- Rodeo owns deterministic temporal and cross-target feature preparation
- AutoQuant owns forecasting, assessment, and canonical evidence
- cross-target learning is compared against independent supervised baselines

## Current Limitations

- Only CatBoost supervised direct-horizon models are supported for
  cross-target features.
- Cross-target relationships are descriptive, not causal.
- No automatic target clustering or feature selection is implemented.
- Shared workflow and independent workflow are intentionally conservative.
- Prediction intervals are not provided by this CatBoost contract.

## Validation

Phase 20 QA is provided by:

```r
qa_vnext_multitarget_supervised_forecasting()
```

The package-level QA includes this suite through:

```r
qa_autoquant_package()
```
