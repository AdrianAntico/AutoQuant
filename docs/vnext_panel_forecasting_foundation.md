# AutoQuant vNext Panel Forecasting Foundation

Status: Phase 13 implemented as an additive global panel forecasting
foundation.

Panel forecasting is treated as another forecasting operator, not a separate
forecasting philosophy. The architecture now scales from one series to many
related series while preserving the same evidence, artifact, assessment, and
Rodeo temporal replay contracts.

## Lifecycle

```text
panel specification
-> entity-aware temporal validation
-> entity-aware partition
-> Rodeo deterministic temporal preparation
-> prepared panel dataset
-> global CatBoost model
-> panel forecast artifact
-> panel assessment
-> canonical analytical evidence
```

## Public API

Implemented:

- `aq_panel_forecast_spec()`
- `aq_validate_panel_forecast_spec()`
- `aq_panel_forecast_partition()`
- `aq_fit_panel_forecast()`
- `aq_assess_panel_forecast()`
- `aq_rolling_origin_panel_forecast()`
- `qa_vnext_panel_forecasting_foundation()`

The installed-package QA entry point `qa_autoquant_package()` includes
`qa_vnext_panel_forecasting_foundation()`.

## Panel Specification

`aq_panel_forecast_spec()` keeps the public API flat:

- `entity`
- `target`
- `date`
- `frequency`
- `horizon`
- `forecast_origin`
- `future_known_variables`
- `static_entity_features`
- `engine`
- `forecast_strategy`
- `engine_parameters`
- `minimum_history`
- `prediction_intervals`
- `confidence_level`
- `rolling_origins`
- `aggregation_level`
- `dataset_id`
- `supported_downstream_actions`

Phase 13 supports `engine = "catboost"` and the existing direct and recursive
forecast strategies. It does not implement per-entity model loops, vector
forecasting, funnel forecasting, hurdle forecasting, hierarchical forecasting,
Prophet, tuning, AutoML, deployment, reconciliation, or probabilistic
intervals.

## Entity Validation

`aq_validate_panel_forecast_spec()` returns deterministic diagnostics for:

- entity column presence
- required date and target columns
- duplicate entity/date pairs
- numeric target
- minimum entity history
- static entity feature consistency
- future data schema
- new future entities
- future entity coverage

Sparse entities and new entities are surfaced as cold-start diagnostics. Phase
13 does not implement a cold-start model.

## Entity-Aware Partition

`aq_panel_forecast_partition()` creates a common panel origin and horizon while
preserving entity identity. The partition records:

- partition id
- forecast origin
- resolved frequency
- future dates
- entity set
- training row index
- evaluation row index
- summary table

The partition id becomes part of artifact lineage.

## Rodeo Integration

Rodeo owns deterministic temporal preparation. AutoQuant calls Rodeo for:

- entity-aware lags
- entity-aware rolling statistics
- entity identity encoding
- calendar features
- known future variable alignment
- static entity feature exposure
- prepared feature manifests
- deterministic replay metadata

AutoQuant fits CatBoost on Rodeo-prepared panel frames. It does not duplicate
lag, rolling, calendar, future-known-variable, or static-entity preparation
logic.

## Global CatBoost Forecasting

Panel forecasting uses one pooled CatBoost model per horizon for direct
strategy, or one pooled one-step model for recursive strategy. The model learns
from all entities together while preserving entity-specific forecast rows and
assessment evidence.

This is not "run one forecast per entity." It is one analytical problem with
many related entities and shared learning.

## Panel Forecast Artifact

`aq_fit_panel_forecast()` produces a canonical table artifact containing:

- entity id
- forecast date
- horizon
- forecast
- actual when available
- unsupported interval diagnostics

Artifact metadata records:

- entity count
- forecast origin
- horizon
- partition id
- temporal specification identity
- temporal transformation identity
- prepared temporal dataset identity
- feature manifest
- feature importance
- cold-start diagnostics
- engine diagnostics
- supported downstream actions

## Assessment

`aq_assess_panel_forecast()` computes:

- aggregate metrics
- metrics by entity
- metrics by horizon
- metrics by entity and horizon
- entity summaries

The metrics use the shared forecast metric implementation:

- RMSE
- MAE
- MAPE
- SMAPE
- bias
- n

This keeps panel assessment compatible with existing forecast evidence while
making entity failures inspectable.

## Rolling-Origin Evaluation

`aq_rolling_origin_panel_forecast()` reuses the same panel specification and
fit/assess functions across multiple origins. It validates that direct global
CatBoost panel forecasting works through the existing rolling-origin pattern.

## Historical Placement

Historical Vector, Funnel, Hurdle, CARMA, and hierarchical forecasting code
contains important analytical ideas: shared temporal features, xregs,
hierarchy/group context, recursive forecast updates, nonnegative forecasts,
intermittent demand logic, and entity-level diagnostics. Phase 13 does not
preserve those APIs. Those families should emerge later as specializations of
the panel forecasting architecture.

## QA

`qa_vnext_panel_forecasting_foundation()` verifies:

- panel specification
- deterministic validation
- duplicate entity/date detection
- entity-aware partition
- Rodeo panel replay metadata
- global CatBoost direct strategy
- global CatBoost recursive strategy
- cold-start sparse-history diagnostics
- aggregate, horizon, and entity assessment
- artifact integrity
- serialization compatibility
- AnalyticsShinyApp compatibility
