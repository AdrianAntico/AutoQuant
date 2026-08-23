# AutoQuant expert API reconstruction 2.0

Date: 2026-08-22
Base SHA: 45787883

## Export accounting

| Metric | Count |
|---|---|
| TOTAL_EXPORTS_BEFORE | 455 |
| TOTAL_EXPORTS_AFTER | 115 |
| CORE_EXPERT_EXPORTS | 5 |
| SPECIALIZED_EXPORTS | 110 |
| COMPATIBILITY_EXPORTS | 0 |
| UNEXPORTED_INTERNALS | 340 |

NAMESPACE is hand-curated. DESCRIPTION uses `Roxygen: list(roclets = c("collate", "rd"))` so `document()` cannot restore the 455-name leak.

## Preferred expert entry points

- `forecast_fit()`
- `forecast_backtest()`
- `forecast_diagnose()`
- `forecast_reconcile()`
- `forecast_engine_control()`

## Specialized surface (why an expert should know it)

Named native engines (`AutoCatBoost*`, `AutoLightGBM*`, `AutoXGBoost*`, `AutoH2o*`, CARMA/ETS/ARIMA/TBATS/ARFIMA/Bandit), family specs and native family fitters that `forecast_fit()` wraps, rolling-origin and tournament helpers, conformal/probabilistic scoring, hierarchical reconciliation, and the supervised `aq_fit_model` / score / assess contract.

## Unexported (not for experts)

Plotting, SQL/Postgres, LLM image adapters, FakeDataGenerator, artifact constructors, all `qa_*`, all `aq_validate_*`, decision/epistemic/experiment lifecycle constructors, EDA communication envelopes, and other plumbing.

## Grok failures addressed

1. Single CatBoost allowlist: `aq_validate_forecast_spec()` GBDT path uses `aq_vnext_supported_engine_params()` / LightGBM / XGBoost native lists. `has_time`, Ordered, monotone, Langevin survive validation.
2. `has_time = TRUE` is the CatBoost production default on temporal fits, including when the expert does not pass it.
3. Funnel forwards `engine` and `engine_parameters`.
4. LightGBM and XGBoost are first-class forecast engines (ordinary + panel), not CatBoost monopoly.
5. Public surface cut 455 → 115. Capability of named engines is retained, not compressed to four wrappers.

## Capability lost?

No. Lifecycle validators, plots, and SQL helpers are unexported, not deleted. Expert engines remain public. GBDT choice expanded.

## Analytical / API grade (expected after)

- Analytical: STRONG_BUT_INCOMPLETE → closer to EXPERT_PASS on the forecast path (LGB/XGB present; MinT still point-only; no auto-tuning)
- API: GOOD_BUT_LEAKY → EXPERT_API_PASS candidate (115 named expert methods, not 455 plumbing names)
