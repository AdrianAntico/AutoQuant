# Forecasting Reference Upgrade 1A/1B Decision Record (2026-08-14)

## Decision
Implement Wave 1 expansion as governed operator increments, not a new forecasting stack.

### Why this design
- Existing forecasting contract already enforces deterministic partitioning, validation, lineage, artifacts, assessment, and backtesting.
- Legacy CARMA/Funnel/Hurdle modules contain valuable feature hypotheses but are not a sustainable primary contract.
- Minimal-scope extension reduces migration risk while materially increasing capability.

### Canonical ownership split
- **AutoQuant**: forecast orchestration (`spec`, `validation`, `partition`, `fit`, `assessment`, `backtest`, capability contracts, comparison surfaces).
- **Rodeo**: reusable deterministic temporal feature preparation for forecasting models that require it.
- **Historical modules**: kept as behavioral references only; not in this checkpoint.

### Inclusion criteria for Wave 1 operators
- Must return bounded typed evidence.
- Must preserve/reuse existing artifact envelope and reproducibility semantics.
- Must never silently retarget an existing workflow binding.
- Must be deterministic under identical input and spec.

### Exclusions
- No automatic engine/model selection.
- No broad feature-tuning or tuning infrastructure expansion in this checkpoint.
- No replacement of legacy Hurdle/Funnel/Vector/CARMA public behavior.

### Boundaries introduced
1. Engine taxonomy extension (`tbats`, `theta`, `arfima`) inside `aq_fit_forecast` via explicit `engine`.
2. Deterministic uncertainty/operator utilities:
   - decomposition, probabilistic scoring, conformal width calibration,
   - reconciliation, forecast combination, policy computation.
3. Focused qualification path via `qa_vnext_forecasting_wave1b`.
