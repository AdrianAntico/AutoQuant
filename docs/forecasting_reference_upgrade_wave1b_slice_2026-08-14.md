# Forecasting Reference Upgrade 1B Slice Plan (2026-08-14)

## Slice objective
Implement and prove the bounded Wave 1B operator slice without disturbing legacy forecasting families.

## Concrete implementation targets
1. Consolidate new engines into the canonical forecast path (`aq_fit_forecast`) and keep engine ownership singular.
2. Add bounded non-fitting forecast operators as reusable utilities:
   - `aq_mstl_decompose`
   - `aq_score_probabilistic_forecast`
   - `aq_conformal_forecast_intervals`
   - `aq_reconcile_forecast_advanced`
   - `aq_combine_forecasts`
   - `aq_forecast_parallel_policy`
   - `aq_forecast_method_tournament`
3. Export these operators and attach focused manpages.
4. Preserve legacy monoliths untouched.
5. Validate through bounded contract (`qa_vnext_forecasting_wave1b`) and existing core forecast foundation QA.

## Acceptance criteria (slice)
- Same `aq_forecast_spec`/`aq_fit_forecast` lifecycle.
- Same artifact envelope shape for all fitted forecast outputs.
- Explicitly represented uncertainty and interval provenance.
- Repeated invocation determinism for restart/no-refit scenarios and replay.
- No architectural coupling to CARMA/Hurdle/Funnel internals.

## Failure handling
- If any operator conflicts with existing engine contracts, adjust the minimal operator input contract and keep the shared forecast envelope unchanged.
- If historical behavior conflicts with typed evidence expectations, retain it in legacy paths and implement a typed vNext translation only in this slice.
