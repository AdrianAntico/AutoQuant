# AutoQuant vNext Intermittent Demand Forecasting

Status: Phase 17 implemented the expanded intermittent-demand forecasting family.

Intermittent demand is now modeled as a forecasting problem family rather than
as only a supervised Hurdle algorithm. The current architecture is:

```text
Forecasting
-> Intermittent Demand
   -> Classical
      -> Croston
      -> SBA
      -> TSB
   -> Supervised
      -> Hurdle occurrence x positive demand
   -> Evidence-Based Method Comparison
      -> naive
      -> seasonal naive
      -> Croston
      -> SBA
      -> TSB
      -> supervised Hurdle when requested and available
```

This distinction matters. Croston and Hurdle solve related analytical problems
with different assumptions:

- Croston estimates positive demand size and inter-demand interval from a
  single intermittent series.
- SBA applies a deterministic small-sample bias adjustment to Croston-style
  demand-size and inter-demand interval estimates.
- TSB tracks occurrence probability and positive-demand size as separate
  exponential smoothing states, making it more responsive to recent zero runs.
- Hurdle estimates demand occurrence probability and positive-demand magnitude
  from supervised temporal features, then combines them into expected demand.

Neither operator is forced to imitate the other. Both inherit the canonical
forecasting contracts for specification, artifact lineage, assessment,
rolling-origin evidence, supported actions, and campaign compatibility.

## Public API

Diagnostics:

- `aq_intermittent_demand_diagnostics()`

Classical Croston:

- `aq_croston_forecast_spec()`
- `aq_fit_croston_forecast()`
- `aq_assess_croston_forecast()`
- `aq_rolling_origin_croston_forecast()`

Classical SBA:

- `aq_sba_forecast_spec()`
- `aq_fit_sba_forecast()`
- `aq_assess_sba_forecast()`
- `aq_rolling_origin_sba_forecast()`

Classical TSB:

- `aq_tsb_forecast_spec()`
- `aq_fit_tsb_forecast()`
- `aq_assess_tsb_forecast()`
- `aq_rolling_origin_tsb_forecast()`

Supervised Hurdle:

- `aq_hurdle_forecast_spec()`
- `aq_validate_hurdle_forecast_spec()`
- `aq_fit_hurdle_forecast()`
- `aq_assess_hurdle_forecast()`
- `aq_rolling_origin_hurdle_forecast()`
- `aq_evaluate_hurdle_panel_strategies()`
- `qa_vnext_hurdle_forecasting_foundation()`

Method comparison:

- `aq_compare_intermittent_demand_methods()`

## Diagnostics

`aq_intermittent_demand_diagnostics()` records deterministic suitability
evidence:

- row count
- positive demand count / nonzero count
- zero or non-occurrence count
- zero proportion
- average inter-demand interval
- longest zero run
- recent zero run
- positive-demand mean
- positive-demand standard deviation
- coefficient of variation of positive demand
- demand-size stability
- interval stability
- trend in occurrence rate
- possible obsolescence signal
- sparse-history status
- demand sparsity
- intermittent-demand suitability
- evidence limitations

Hurdle assessment additionally records:

- occurrence accuracy
- Brier score
- actual zero rate
- predicted zero rate
- mean occurrence probability
- positive-demand error
- combined expected-demand error

## Croston

Croston is the deterministic classical baseline for single-series or
entity-by-entity intermittent demand. It does not require Rodeo temporal
feature preparation and does not create supervised occurrence/magnitude
models. Its artifact records:

- forecast specification id
- alpha smoothing parameter
- occurrence threshold
- intermittent-demand diagnostics
- forecast table
- canonical artifact envelope

Croston is also used as a baseline for supervised Hurdle forecasts whenever
the Hurdle forecast can produce comparable future dates.

## SBA

SBA is implemented as a classical intermittent-demand operator that shares the
Croston state model and applies the standard smoothing-parameter bias
adjustment. It is intentionally separate from Croston because comparison
evidence should be able to determine whether the adjusted estimate improves
assessment metrics on a particular history.

## TSB

TSB is implemented as a distinct classical intermittent-demand operator. It
updates an occurrence-probability state at every time point and updates the
positive-demand size state only on observed positive demand. This makes TSB
more sensitive to recent runs of zero demand than Croston or SBA. The artifact
metadata records the occurrence-probability state, demand-size state,
zero-run-response behavior, and intermittent-demand diagnostics.

## Supervised Hurdle

Supervised Hurdle forecasting uses Rodeo's temporal transformation contract to
prepare deterministic forecast frames. For each horizon it fits:

- an occurrence model for `target > occurrence_threshold`
- a positive-demand model using only positive-demand rows

The final expected-demand forecast is:

```text
expected demand = occurrence probability * positive-demand prediction
```

The forecast artifact preserves all three outputs:

- `occurrence_probability`
- `positive_prediction`
- `forecast`

The assessment compares the combined forecast against realized actuals and
also stores occurrence and positive-demand diagnostics separately.

## Baselines

Every supervised Hurdle result records deterministic baselines:

- naive
- seasonal naive
- Croston
- SBA
- TSB

These baselines are evidence, not automatic model selection. They allow
campaigns, reports, and future guidance systems to inspect whether the
supervised intermittent-demand model is earning its additional complexity.

## Method Comparison

`aq_compare_intermittent_demand_methods()` evaluates intermittent-demand
operators on common rolling-origin evidence. The comparison currently covers:

- naive
- seasonal naive
- Croston
- SBA
- TSB
- supervised Hurdle when `include_hurdle = TRUE` and the required supervised
  dependencies are available

The result includes forecast rows, method-level metrics, horizon-level metrics,
entity-level metrics where applicable, intermittent-demand diagnostics,
intermittent-specific metrics, and a canonical comparison artifact.

Recommendations are deliberately advisory:

- `naive_preferred`
- `seasonal_naive_preferred`
- `croston_preferred`
- `sba_preferred`
- `tsb_preferred`
- `hurdle_preferred`
- `method_varies_by_entity`
- `method_varies_by_horizon`
- `evidence_insufficient`
- `obsolescence_review_recommended`

The comparison does not implement automatic routing, clustering, inventory
optimization, or hybrid production selection. It produces evidence that a
human, report, campaign, or future governed recommendation layer can inspect.

## Panel Strategy Compatibility

`aq_evaluate_hurdle_panel_strategies()` evaluates independent, grouped, and
global supervised Hurdle strategies using the same advisory strategy-selection
contract introduced for panel forecasting. The recommendation remains
advisory and does not automatically choose a production model.

## Historical Archaeology

The restored historical Hurdle code encoded real intermittent-demand intent:

- explicit occurrence and magnitude decomposition
- zero-heavy demand processes
- recursive forecasting concerns
- calendar and lag feature construction
- model-scoring separation

Phases 16 and 17 preserve that intent while reorganizing the architecture around the
problem family. Historical Hurdle wrappers are not treated as the only correct
solution. Croston establishes the classical baseline; supervised Hurdle
establishes the first feature-aware operator; SBA, TSB, and method comparison
make the intermittent-demand family testable rather than assumed.

## Limitations

Not implemented in Phase 17:

- probabilistic intermittent-demand forecasting
- inventory optimization
- safety stock
- service-level optimization
- deployment
- automatic model selection or production routing
- Hurdle strategy auto-selection
- advanced positive-demand distribution modeling

The scope is intentionally limited to establishing intermittent demand as a
first-class forecasting problem family inside the existing AutoQuant vNext
artifact, assessment, rolling-origin, and campaign evidence contracts.
