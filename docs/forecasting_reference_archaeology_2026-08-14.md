# AutoQuant Forecasting Foundation Archaeology and Boundaries (2026-08-14)

## Scope
- Read only AutoQuant forecasting implementations and retained CARMA/Hurdle/Funnel/Vector/Funnel references.
- Preserve historical behavior as evidence; do not preserve historical API shape.
- Bound by current request: do not alter legacy CARMA/Hurdle/Funnel/Vector/Funnel internals.

## Archaeological findings
- Existing vNext forecasting contract already provides: specification/validation/partitioning, Naive, Seasonal Naive, ETS, ARIMA, CatBoost, panel/hierarchy/intermittent/funnel foundations, and rolling-origin evaluation.
- Historical CARMA wrappers encode useful ideas (differencing/replay, recursive feature state, target interaction, regime handling) but are monolithic and not aligned with typed artifact governance.
- Current uncommitted work adds Wave 1A/1B operators: broader statistical engine family (`tbats`, `theta`, `arfima`) and non-fitting forecast operators (`mstl`, probabilistic scoring, conformal intervals, reconciliation, combination, tournament/policy).
- A bounded reuse policy is not yet unified across all forecast and panel paths; this checkpoint should add operators without adding hidden engine selection.

## Architectural rule derived
Forecasting must remain a single shared operator contract (`aq_forecast_spec` + `aq_fit_forecast`) with a bounded set of engines and shared artifact schema, while legacy monolithic forecasting families remain in archive/compatibility mode.

## Boundary decisions
- **Preserve unchanged:** CARMA/Hurdle/Funnel internals, existing legacy wrappers, and existing panel/funnel/hurdle modules.
- **Integrate:** Wave 1 engines/operators as first-class extensions to shared forecasting contract.
- **Exclude:** any automatic selection across engines/models in this slice.

## Bounded foundation slice
1. Add/confirm deterministic support for additional single-series engines.
2. Add governed non-fitting operators that produce typed evidence (MSTL/uncertainty/reconciliation/combination/tournament/policy).
3. Keep one replay/lineage envelope shared with existing forecast artifacts.
4. Keep uncertainty and prediction evidence explicit and non-overlapping with unsupported-engine behavior.
