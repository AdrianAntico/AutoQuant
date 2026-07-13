# AutoQuant vNext Forecasting Foundation

Status: Phase 21 implemented for deterministic forecasting foundations with single-series naive, seasonal naive, ETS, ARIMA, CatBoost supervised forecasting, Rodeo-owned temporal transformation replay, global CatBoost panel forecasting, deterministic hierarchical reconciliation, panel strategy selection, negative-transfer diagnostics, expanded intermittent-demand forecasting, funnel forecasting, multi-target forecasting, supervised cross-target feature forecasting, prediction interval evidence, known-future-regressor validation, challenger-baseline comparison, and forecasting capability planning.

This document establishes the shared forecasting language that future forecasting engines should inherit. ETS, ARIMA, CatBoost, global CatBoost panel forecasting, deterministic hierarchy reconciliation, panel strategy comparison, Croston/SBA/TSB intermittent-demand forecasting, supervised Hurdle intermittent-demand forecasting, intermittent-demand method comparison, deterministic funnel forecasting, deterministic multi-target forecasting, supervised cross-target feature forecasting, and deterministic forecasting strategy planning are implemented inside shared specification, artifact, assessment, comparison, or planning contracts. Prediction intervals and known future regressors are first-class forecast evidence. This phase does not implement Prophet, GAM forecasting, VAR, VARMAX, multivariate state-space models, inventory optimization, optimization-based reconciliation, feature tuning, model tuning, deployment, or AutoML.

## Lifecycle

```text
forecast specification
-> temporal validation
-> forecast partition
-> Rodeo temporal transformation contract where temporal feature preparation is required
-> forecast engine
-> forecast strategy
-> prepared feature lineage
-> prediction interval evidence
-> known-future-regressor evidence
-> forecast artifact
-> forecast assessment
-> rolling-origin backtest
-> canonical analytical evidence
-> hierarchical reconciliation where requested
-> panel strategy comparison where requested
-> intermittent-demand diagnostics where requested
-> funnel maturity and transition evidence where requested
-> multi-target cross-target evidence where requested
-> forecasting capability discovery and strategy planning where requested
```

Forecasting is treated as a first-class analytical operator rather than a specialized scoring wrapper. The core concepts are explicit:

- forecast date column
- target column
- frequency
- forecast horizon
- forecast origin
- future known variables
- future unknown variables
- future regressor policy
- forecast strategy
- prepared feature lineage
- temporal transformation identity
- temporal replay status
- prediction intervals
- confidence level
- temporal validation
- training window
- evaluation window
- rolling-origin backtesting
- forecast artifact lineage
- hierarchy identity
- reconciliation diagnostics
- strategy recommendation
- negative transfer diagnostics
- intermittent-demand diagnostics
- funnel transition diagnostics
- cross-target evidence
- forecasting capability discovery
- forecasting strategy planning
- feature-tuning mechanism inventory

## Public API

Implemented:

- `aq_forecast_spec()`
- `aq_validate_forecast_spec()`
- `aq_forecast_partition()`
- `aq_fit_forecast()`
- `aq_assess_forecast()`
- `aq_rolling_origin_forecast()`
- `qa_vnext_forecasting_foundation()`
- `aq_discover_forecasting_capabilities()`
- `aq_plan_forecasting_strategy()`
- `qa_vnext_forecasting_planning()`

The installed-package QA entry point `qa_autoquant_package()` includes `qa_vnext_forecasting_foundation()` and `qa_vnext_forecasting_planning()`.

## Forecast Specification

`aq_forecast_spec()` defines:

- `target`
- `date`
- `frequency`
- `horizon`
- `forecast_origin`
- `future_known_variables`
- `future_unknown_variables`
- `engine`
- `forecast_strategy`
- `engine_parameters`
- `prediction_intervals`
- `confidence_level`
- `future_regressor_policy`
- `metrics`
- `rolling_validation`
- `rolling_origins`
- `rolling_window`
- `training_window`
- `season_length`
- `aggregation_level`
- `dataset_id`
- `supported_downstream_actions`

The current single-series engines are:

- `naive`
- `seasonal_naive`
- `ets`
- `arima`
- `catboost`

`forecast_strategy` is used by CatBoost and currently supports:

- `direct`
- `recursive`

No multi-output strategy is implemented. Direct CatBoost trains one point-forecast model per horizon. Recursive CatBoost trains a one-step model and rolls predictions forward deterministically.

Multi-target forecasting uses additive public APIs:

- `aq_multitarget_forecast_spec()`
- `aq_validate_multitarget_forecast_spec()`
- `aq_fit_multitarget_forecast()`
- `aq_assess_multitarget_forecast()`
- `aq_compare_multitarget_strategies()`

Phase 19 multi-target forecasting supports deterministic `naive` and
`seasonal_naive` engines. Phase 20 adds supervised `catboost` multi-target
forecasting and `strategy = "cross_target_features"`. It compares independent,
shared-workflow, and cross-target strategies while preserving target-level
evidence, descriptive cross-target context, and negative-transfer diagnostics.
It does not implement joint probabilistic multivariate forecasting or target
causality.

Panel forecasting uses additive public APIs:

- `aq_panel_forecast_spec()`
- `aq_validate_panel_forecast_spec()`
- `aq_panel_forecast_partition()`
- `aq_fit_panel_forecast()`
- `aq_assess_panel_forecast()`
- `aq_rolling_origin_panel_forecast()`
- `aq_hierarchy_spec()`
- `aq_validate_hierarchy_spec()`
- `aq_reconcile_hierarchical_forecast()`
- `aq_assess_hierarchical_forecast()`
- `aq_rolling_origin_hierarchical_forecast()`
- `qa_vnext_hierarchical_forecasting_foundation()`
- `aq_panel_strategy_spec()`
- `aq_validate_panel_strategy_spec()`
- `aq_evaluate_panel_strategies()`
- `qa_vnext_panel_strategy_selection()`
- `aq_intermittent_demand_diagnostics()`
- `aq_croston_forecast_spec()`
- `aq_fit_croston_forecast()`
- `aq_assess_croston_forecast()`
- `aq_rolling_origin_croston_forecast()`
- `aq_sba_forecast_spec()`
- `aq_fit_sba_forecast()`
- `aq_assess_sba_forecast()`
- `aq_rolling_origin_sba_forecast()`
- `aq_tsb_forecast_spec()`
- `aq_fit_tsb_forecast()`
- `aq_assess_tsb_forecast()`
- `aq_rolling_origin_tsb_forecast()`
- `aq_hurdle_forecast_spec()`
- `aq_validate_hurdle_forecast_spec()`
- `aq_fit_hurdle_forecast()`
- `aq_assess_hurdle_forecast()`
- `aq_rolling_origin_hurdle_forecast()`
- `aq_evaluate_hurdle_panel_strategies()`
- `aq_compare_intermittent_demand_methods()`
- `aq_funnel_forecast_spec()`
- `aq_validate_funnel_forecast_spec()`
- `aq_fit_funnel_forecast()`
- `aq_assess_funnel_forecast()`
- `aq_compare_funnel_strategies()`
- `qa_vnext_funnel_forecasting_foundation()`
- `qa_vnext_hurdle_forecasting_foundation()`

Panel forecasting currently supports global CatBoost models only. See
`docs/vnext_panel_forecasting_foundation.md`.

Hierarchical forecasting currently supports deterministic bottom-up
reconciliation above panel forecasts only. See
`docs/vnext_hierarchical_forecasting_foundation.md`.

Panel strategy selection compares independent, grouped, and global pooled
forecasting strategies without automatically selecting one. See
`docs/vnext_panel_strategy_selection.md`.

Intermittent demand forecasting now contains two sibling operator families:

- classical intermittent-demand forecasting through `aq_fit_croston_forecast()`,
  `aq_fit_sba_forecast()`, and `aq_fit_tsb_forecast()`
- supervised intermittent-demand forecasting through `aq_fit_hurdle_forecast()`
- evidence-based intermittent-demand method comparison through
  `aq_compare_intermittent_demand_methods()`

Croston estimates demand size and inter-demand interval directly. SBA applies
a Croston-style bias adjustment. TSB updates occurrence probability and
positive-demand size as separate states. Hurdle forecasting estimates
occurrence probability and positive-demand magnitude from supervised temporal
features, then combines them into expected demand. These are intentionally
different approaches to the same problem family. Neither is forced to imitate
the other.
See `docs/vnext_intermittent_demand_forecasting.md`.

Funnel forecasting now treats ordered stage processes as a first-class
forecasting family:

- stage forecasting preserves each stage as its own volume evidence
- transition forecasting estimates adjacent conversion evidence and propagates
  stage volume downstream
- maturity is represented explicitly when available and conservatively derived
  when not supplied
- `aq_compare_funnel_strategies()` compares stage and transition strategies
  without automatically selecting a production path

See `docs/vnext_funnel_forecasting_foundation.md`.

## Temporal Validation

`aq_validate_forecast_spec()` returns deterministic diagnostics for:

- specification class
- horizon
- overlapping future-known and future-unknown variables
- required columns
- date conversion
- duplicate timestamps
- target type
- frequency detection
- frequency override compatibility
- missing timestamp periods
- sufficient history
- forecast origin compatibility
- engine support
- engine parameter compatibility
- interval support
- confidence level
- known-future-regressor support

Frequency detection currently covers:

- daily
- weekly
- monthly
- quarterly
- yearly
- irregular

The validation is conservative. It records warnings for irregular or missing periods rather than silently assuming the user intended them.

## Forecast Partition

`aq_forecast_partition()` creates an explicit partition object containing:

- partition id
- forecast origin
- horizon
- resolved frequency
- future dates
- training row index
- evaluation row index
- training window policy
- summary table

The partition id becomes part of artifact lineage. Forecasting should not rely on implicit row slicing.

## Baseline Engines

`aq_fit_forecast()` supports:

- naive forecast: repeat the most recent observed target value
- seasonal naive forecast: repeat values from the most recent seasonal cycle

If the seasonal naive engine does not have enough seasonal history, the current implementation falls back to the most recent observed value. Advanced model selection and optimization are intentionally not implemented.

## Statistical Engines

Phase 9 adds two statistical engines:

- ETS through base R `stats::HoltWinters()`
- ARIMA through base R `stats::arima()`

The implementation intentionally avoids a new hard dependency on the historical `forecast` package. Historical AutoQuant functions may still call `forecast::` at runtime, but the vNext forecasting foundation must validate and install in a clean environment where that package is not guaranteed to exist.

ETS and ARIMA own only model fitting, forecast generation, and engine diagnostics. The forecasting framework continues to own specification, temporal validation, partitioning, artifacts, assessment, comparison readiness, and rolling-origin evaluation.

### ETS Parameters

`aq_forecast_spec(engine = "ets", engine_parameters = list(...))` supports:

- `method`: one of `simple`, `trend`, or `seasonal`
- `alpha`: optional smoothing parameter in `[0, 1]`
- `beta`: optional smoothing parameter in `[0, 1]`
- `gamma`: optional smoothing parameter in `[0, 1]`

The default is simple exponential smoothing. Seasonal ETS requires a seasonal frequency or explicit `season_length > 1`.

### ARIMA Parameters

`aq_forecast_spec(engine = "arima", engine_parameters = list(...))` supports:

- `order`: non-negative integer vector of length 3, default `c(1, 0, 0)`
- `seasonal_order`: non-negative integer vector of length 3, default `c(0, 0, 0)`
- `include_mean`: logical, default `TRUE`

No automatic order search is implemented. The caller supplies deterministic ARIMA parameters.

## Engine Diagnostics

Statistical forecast results and artifact metadata preserve engine-specific diagnostics:

- engine
- model form or order
- season length or seasonal period
- AIC
- AICc
- BIC
- residual diagnostics
- convergence status
- warnings
- training duration

Diagnostics are nested inside the common forecast artifact envelope so consumers can inspect them without needing engine-specific artifact families.

## Rodeo Temporal Transformation Boundary

Phase 12 moves deterministic temporal feature preparation into Rodeo.
AutoQuant no longer owns CatBoost forecasting feature construction. AutoQuant
still owns forecast orchestration, engine fitting, artifacts, assessment, and
backtesting, but temporal preparation is delegated to Rodeo's structured
temporal contract:

- `Rodeo::rodeo_temporal_transformation_spec()`
- `Rodeo::rodeo_fit_temporal_transformation()`
- `Rodeo::rodeo_prepare_forecast_supervised_data()`
- `Rodeo::rodeo_temporal_prediction_frame()`
- `Rodeo::rodeo_temporal_transformation_metadata()`

The lifecycle is:

```text
raw temporal data
-> Rodeo temporal transformation spec
-> fitted temporal transformation
-> prepared temporal forecast frames
-> AutoQuant forecast engine
-> forecast artifact
-> forecast assessment
```

The user-facing AutoQuant API remains unchanged. The Rodeo contract is an
internal orchestration dependency for deterministic temporal preparation and
replay.

ETS and ARIMA continue to consume the common forecast partition and
known-future-regressor contracts. They do not require lag or rolling feature
generation beyond their xreg inputs, so they preserve the common artifact
contract without fabricating unused supervised features.

## CatBoost Supervised Forecasting

Phase 11 added `engine = "catboost"` to `aq_fit_forecast()` without introducing a separate forecasting API. Phase 12 moves its deterministic temporal preparation into Rodeo.

CatBoost forecasting supports:

- `forecast_strategy = "direct"`
- `forecast_strategy = "recursive"`
- Rodeo-prepared calendar/date features
- Rodeo-prepared target lags
- Rodeo-prepared shifted rolling target means
- Rodeo-aligned known future regressors
- prepared temporal feature lineage
- feature manifests
- feature importance metadata where CatBoost exposes it
- rolling-origin validation through `aq_rolling_origin_forecast()`

The CatBoost engine is intentionally narrow. It produces deterministic point forecasts only. It does not implement:

- multi-output forecasting
- automatic model selection
- hyperparameter tuning
- probabilistic intervals
- vector and funnel forecasting
- deployment

CatBoost-specific feature controls are supplied through `engine_parameters`:

- `lag_periods`
- `rolling_windows`
- `date_features`
- standard CatBoost parameters such as `iterations`, `depth`, `learning_rate`, `random_seed`, `thread_count`, and `verbose`

The default CatBoost feature set includes short target history, seasonal target history where the frequency implies one, rolling target means, and compact date features. Rodeo constructs the supervised feature frames and returns a feature manifest recording whether each feature came from target lags, target rolling statistics, calendar encoding, or known future regressors.

Direct strategy creates one Rodeo-prepared supervised training frame per horizon. Recursive strategy creates a Rodeo-prepared one-step supervised training frame and asks Rodeo to generate each subsequent prediction row from the updated history values. Both strategies use the existing forecast partition and assessment contracts.

CatBoost forecast metadata includes:

- forecast strategy
- prepared feature identity
- Rodeo transformation identity
- temporal specification identity
- temporal transformation identity
- prepared temporal dataset identity
- temporal replay status
- temporal diagnostics
- CatBoost model identity
- feature manifest
- feature importance availability
- future-known-regressor usage
- unsupported interval reason

Every CatBoost forecast is compared against naive, seasonal naive when applicable, ETS, and ARIMA where those challenger engines can be fit. These are challenger baselines, not automatic model selection.

## Prediction Intervals

Prediction intervals are treated as analytical evidence, not incidental engine output.

`aq_forecast_spec()` supports:

- `prediction_intervals = TRUE/FALSE`
- `confidence_level`

Forecast artifacts include:

- `lower_interval`
- `upper_interval`
- `confidence_level`
- `interval_available`
- `interval_method`
- `unsupported_interval_reason`

Current interval behavior:

- ETS uses `stats::predict.HoltWinters(..., prediction.interval = TRUE)`.
- ARIMA uses `stats::predict.Arima()` standard errors with a normal approximation.
- Naive and seasonal naive engines report unsupported interval diagnostics and do not fabricate intervals.
- CatBoost reports unsupported interval diagnostics and does not fabricate intervals.

If intervals are requested for an unsupported engine, validation records a warning and the forecast artifact records why intervals are unavailable.

## Known Future Regressors

Known future regressors are declared through `future_known_variables`. The architecture distinguishes:

- known future variables
- unknown future variables
- historical-only target/date history

`aq_fit_forecast()` accepts optional `future_data`. Future data is validated for:

- required future regressor columns
- forecast horizon length
- missing future values
- numeric type compatibility for ARIMA xreg
- date alignment when a date column is supplied

ARIMA currently consumes known future regressors through `stats::arima(..., xreg = ...)` and `stats::predict.Arima(..., newxreg = ...)`.

CatBoost consumes known future regressors as supervised features after validating that the future horizon contains complete numeric values.

ETS, naive, and seasonal naive record known-future-regressor metadata but do not consume xreg. Unsupported usage is visible as diagnostics rather than silently ignored.

## Forecast Artifacts

Forecast results include a table artifact with:

- forecast date
- horizon
- forecast
- actual where evaluation data exists

The artifact carries the canonical analytical artifact envelope:

- artifact id
- artifact type
- parent artifact ids
- lineage
- task = `time_series_forecast`
- operator = `forecast`
- engine
- specification id
- dataset id
- supported actions

This allows AnalyticsShinyApp, campaigns, reports, and future operators to reason over forecast evidence without engine-specific logic.

## Forecast Assessment

`aq_assess_forecast()` computes:

- RMSE
- MAE
- MAPE
- SMAPE
- bias
- row count

It also produces metrics by horizon. Assessment requires realized actuals in the forecast result.

For ETS and ARIMA, `aq_fit_forecast()` also carries naive and seasonal naive baseline forecast tables when applicable. For CatBoost, the forecast result carries naive, seasonal naive when applicable, ETS, and ARIMA challenger forecast tables where possible. `aq_assess_forecast()` summarizes those baselines in `baseline_metrics` and `baseline_comparison`. This keeps baseline comparison inside assessment evidence without implementing automatic model selection.

Forecast assessment also includes deterministic interval evidence when realized actuals exist:

- `interval_metrics`
- `interval_metrics_by_horizon`
- interval coverage
- average interval width
- interval row count

This is not probabilistic calibration or Bayesian forecasting. It is a deterministic evaluation of whether realized outcomes fell within generated intervals.

## Rolling-Origin Backtesting

`aq_rolling_origin_forecast()` runs deterministic rolling-origin evaluation.

Supported behavior:

- explicit origins
- derived latest N origins
- expanding window
- fixed training window through `training_window`
- repeated forecast execution
- assessment per origin
- backtest artifact with canonical envelope

This is the reproducibility foundation for future challenger forecast engines.

Phase 11 verifies that naive, seasonal naive, ETS, ARIMA, and CatBoost all use this same rolling-origin framework. ARIMA and CatBoost with known future regressors use the evaluation window as future-known data during backtests. No engine-specific rolling framework is introduced.

## Rodeo Boundary

Rodeo should own reusable feature transformation capabilities such as:

- calendar features
- date features
- lag features
- rolling statistics
- future feature preparation

Phase 11 keeps the ownership boundary explicit. AutoQuant owns forecast specification, partitioning, supervised framing, model fitting, assessment, and artifact lineage. Rodeo owns reusable temporal transformation contracts.

The currently installed Rodeo public surface exposes legacy temporal helpers such as `CalendarVariables`, `AutoLagRollStats`, and `AutoLagRollStatsScoring`. The structured `rodeo_transformation_spec()` temporal fit/apply contract is not assumed to be installed in downstream environments. CatBoost forecast artifacts therefore record Rodeo transformation identity and transformation scope while AutoQuant performs the minimal deterministic supervised framing required for single-series direct and recursive forecasting. This is a compatibility bridge, not a new feature-engineering ownership claim.

Future Rodeo work should expose a structured temporal fit/apply contract for lag, rolling, date, and known-future-variable preparation. When that exists, the CatBoost forecast engine should delegate those transformations and preserve the same metadata fields.

## Bundle Compatibility

Forecast artifacts serialize cleanly and use the canonical artifact envelope. CatBoost forecast model objects remain inside the forecast result for immediate use, while durable portable forecast bundles remain future work. Future forecasting bundles should use the same envelope and lineage principles as vNext model bundles.

## Known Limitations

Not implemented:

- Prophet
- GAM forecasting
- top-down reconciliation
- middle-out reconciliation
- optimization-based reconciliation
- full Vector forecasting
- Funnel forecasting
- Hurdle forecasting
- advanced exogenous-variable modeling beyond ARIMA known-future regressors
- automatic future regressor generation
- automatic feature engineering
- automatic clustering
- automatic strategy selection
- forecast optimization
- forecast deployment
- prediction interval optimization
- probabilistic calibration
- Bayesian forecasting
- probabilistic prediction intervals
- model registry

The goal through Phase 15 is to make uncertainty, known-future information, supervised forecasting features, panel entity evidence, hierarchy reconciliation, strategy selection, negative-transfer diagnostics, and challenger baselines first-class evidence. Future engines should inherit this uncertainty, future-data, feature-lineage, panel, hierarchy, strategy, and assessment contract rather than inventing their own artifact shapes.
