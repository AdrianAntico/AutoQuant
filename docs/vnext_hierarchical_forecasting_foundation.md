# AutoQuant vNext Hierarchical Forecasting Foundation

Status: Phase 14 implemented as an additive vNext forecasting contract.

AutoQuant now treats hierarchy as a deterministic forecasting operator above
panel forecasting. The hierarchy layer does not replace panel forecasting and
does not create a second forecasting philosophy. The lifecycle is:

```text
hierarchy specification
-> entity-aware temporal preparation
-> global panel forecast
-> deterministic reconciliation
-> hierarchy assessment
-> canonical analytical evidence
```

Rodeo continues to own deterministic temporal preparation. AutoQuant owns
hierarchy specification, validation, aggregation, reconciliation, artifacts, and
assessment.

## Public API

Phase 14 adds:

- `aq_hierarchy_spec()`
- `aq_validate_hierarchy_spec()`
- `aq_reconcile_hierarchical_forecast()`
- `aq_assess_hierarchical_forecast()`
- `aq_rolling_origin_hierarchical_forecast()`
- `qa_vnext_hierarchical_forecasting_foundation()`

The API is intentionally flat. Hierarchies are represented as node tables with
entity id, parent id, optional level, and optional weights. AutoQuant does not
require users to construct nested tree objects.

## Hierarchy Specification

`aq_hierarchy_spec()` records:

- entity id column
- parent id column
- hierarchy level column
- root id
- aggregation method
- optional weight column
- hierarchy version
- supported downstream actions

The hierarchy table is the canonical structural input. It is preserved in
metadata so later artifacts can explain which hierarchy produced them.

## Validation

`aq_validate_hierarchy_spec()` returns deterministic diagnostics for:

- required columns
- duplicate entity ids
- missing entity ids
- missing parents / orphan nodes
- root count
- expected root identity
- self-parent relationships
- cycles
- aggregation/weight validity

Invalid hierarchies are rejected before reconciliation.

## Aggregation and Reconciliation

Phase 14 supports deterministic bottom-up reconciliation.

Bottom-up reconciliation:

- treats panel forecast entities as leaves
- aggregates descendant leaf forecasts into every ancestor
- preserves bottom-level forecasts unchanged
- records diagnostics instead of hiding aggregate adjustments

Supported aggregation methods are:

- `sum`
- `mean`
- `weighted`

`sum` is the default and primary production path for additive business
hierarchies such as regions, products, stores, channels, cohorts, and
organizational groups.

Top-down, middle-out, and optimization-based reconciliation are intentionally
not implemented in this phase.

## Artifacts

`aq_reconcile_hierarchical_forecast()` returns an
`aq_hierarchical_forecast_result` and creates a canonical table artifact with:

- hierarchy identity
- panel forecast identity
- parent identity
- hierarchy level
- aggregation method
- reconciliation method
- reconciliation status
- reconciliation diagnostics
- supported downstream actions

These artifacts are compatible with the canonical AutoQuant artifact envelope
and AnalyticsShinyApp artifact normalization.

## Assessment

`aq_assess_hierarchical_forecast()` extends the existing forecast assessment
pattern with:

- aggregate metrics
- metrics by entity
- metrics by hierarchy level
- metrics by horizon
- aggregate consistency summaries
- reconciliation diagnostics

The assessment remains deterministic and does not require a separate assessment
framework.

## Rolling Origin

`aq_rolling_origin_hierarchical_forecast()` reuses
`aq_rolling_origin_panel_forecast()`. Each panel forecast is reconciled through
the same hierarchy contract, then assessed with the hierarchy assessment helper.
No second rolling-origin framework was introduced.

## Historical Placement

The restored Vector, Funnel, Hurdle, and hierarchical/CARMA files contain
valuable structural ideas:

- shared temporal evidence across related series
- group and hierarchy awareness
- future-known base measures
- conversion/funnel-style staged outcomes
- intermittent-demand occurrence and magnitude logic

Phase 14 does not preserve those historical APIs. Instead, it establishes a
modern structural foundation:

```text
single series
-> panel
-> hierarchy
-> future specializations
```

Future Vector, Funnel, and Hurdle forecasting should specialize this hierarchy
and panel foundation rather than implement independent structural frameworks.

## QA

`qa_vnext_hierarchical_forecasting_foundation()` verifies:

- hierarchy specification
- hierarchy validation
- duplicate detection
- orphan detection
- cycle detection
- multiple-root detection
- bottom-up reconciliation
- aggregate constraints
- artifact integrity
- aggregate metrics
- entity metrics
- assessment artifact integrity
- rolling-origin compatibility
- bundle compatibility
- AnalyticsShinyApp compatibility

## Intentional Gaps

Not implemented:

- Funnel forecasting
- Hurdle forecasting
- full Vector forecasting
- Prophet
- tuning / AutoML
- optimization-based reconciliation
- top-down reconciliation
- middle-out reconciliation
- deployment

These are future specializations, not Phase 14 requirements.
