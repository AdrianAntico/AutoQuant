# AutoQuant vNext Panel Strategy Selection

Status: Phase 15 implemented as an additive vNext forecasting contract.

Panel forecasting is now treated as an analytical hypothesis rather than a
default. AutoQuant can compare independent, grouped, and global pooled
forecasting strategies under common origins, horizons, metrics, artifacts, and
campaign evidence.

The result is advisory. AutoQuant does not automatically select a production
strategy.

## Public API

Phase 15 adds:

- `aq_panel_strategy_spec()`
- `aq_validate_panel_strategy_spec()`
- `aq_evaluate_panel_strategies()`
- `qa_vnext_panel_strategy_selection()`

The public API remains flat. Strategy evaluation is configured with entity,
target, date, optional group, candidate strategies, horizon, future-known
variables, static entity features, engine parameters, comparison metrics,
entity weighting, cold-start policy, and negative-transfer tolerance.

## Candidate Strategies

### Independent

Independent forecasting fits one single-series forecast per entity using the
existing `aq_forecast_spec()` and `aq_fit_forecast()` contracts. It is the
baseline for determining whether pooling helps or hurts.

### Grouped

Grouped forecasting requires an explicit group column. Phase 15 does not infer
groups or cluster series. For each group, AutoQuant fits a pooled panel forecast
using the existing `aq_fit_panel_forecast()` contract on the group subset.

### Global

Global forecasting reuses the existing pooled panel forecasting implementation.
No global panel redesign was introduced.

## Strategy Comparison

`aq_evaluate_panel_strategies()` evaluates all requested candidates across the
same forecast origins and horizons. It produces:

- forecast rows by strategy
- entity metrics
- aggregate strategy metrics
- equal-entity weighted scores
- row-count weighted scores where requested
- training efficiency metadata
- strategy recommendation evidence

The primary score defaults to equal-entity RMSE so large entities do not hide
consistent degradation in smaller entities.

## Negative Transfer

Negative transfer diagnostics compare pooled strategies against independent
entity forecasts. A strategy is flagged when its entity-level RMSE degrades
beyond the configured tolerance.

Diagnostics identify:

- global vs independent degradation
- grouped vs independent degradation
- affected entities
- forecast origin
- relative degradation

This evidence does not automatically reject pooled models. It explains where
shared learning may be harmful.

## Series Diagnostics

Series diagnostics include:

- history length
- missingness
- volatility
- forecast stability
- cold-start risk
- explicit group membership where available

These diagnostics are designed to explain strategy recommendations and support
future campaign questions such as:

- Should we pool these entities?
- Which entities behave differently?
- Which entities deserve separate experiments?
- Which groups are natural candidates for shared models?

## Recommendations

Recommendations are deterministic and advisory:

- `independent_preferred`
- `grouped_preferred`
- `global_preferred`
- `hybrid_worth_investigating`
- `evidence_insufficient`

If a pooled strategy has the best aggregate score but entity-level negative
transfer is detected, AutoQuant recommends `hybrid_worth_investigating`.

## Artifacts

`aq_evaluate_panel_strategies()` creates a canonical table artifact:

- artifact type: `panel_strategy_comparison`
- lineage: strategy specification and origins
- payload: strategy summary
- metadata: negative transfer, series diagnostics, recommendation, training
  efficiency
- supported actions: compare, report, campaign review

This artifact is compatible with the existing AutoQuant artifact envelope and
AnalyticsShinyApp artifact normalization.

## Intentional Gaps

Not implemented:

- automatic clustering
- automatic strategy selection
- Funnel forecasting
- Hurdle forecasting
- Vector forecasting
- hierarchical optimization
- AutoML
- deployment

The purpose of Phase 15 is evidence-driven recommendation, not automation.
