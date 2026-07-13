# AutoQuant vNext Forecasting Capability Planning

Status: Phase 22 implemented. Phase 21 introduced deterministic capability
planning. Phase 22 adds the governed experiment campaign handoff through
`aq_forecast_experiment_spec()`, `aq_run_forecast_experiment()`, and
`aq_run_forecast_experiment_campaign()`.

Forecasting capability planning is a deterministic discovery and planning
layer. It inspects the available data and returns a planning artifact that says
which forecasting strategy families appear supported, unsupported, or
uncertain. It does not fit models, tune parameters, execute AutoML, or select a
production strategy.

The public entry points are:

- `aq_discover_forecasting_capabilities()`
- `aq_plan_forecasting_strategy()`
- `qa_vnext_forecasting_planning()`

## Purpose

Forecasting now contains many possible families:

- single-series statistical forecasting
- CatBoost supervised forecasting
- panel forecasting
- hierarchical forecasting
- intermittent-demand forecasting
- funnel forecasting
- multi-target forecasting
- cross-target feature forecasting

The planner helps a downstream workflow decide what evidence should be
collected next. It returns recommendations, missing evidence, mandatory
baselines, and a smallest useful experiment set. The recommendations are
advisory. They are not automatic model execution.

## Discovery Output

`aq_discover_forecasting_capabilities()` records:

- schema summary
- detected date column
- target candidates
- entity/group candidate
- hierarchy metadata
- funnel stage/value structure
- known-future-variable availability
- target summaries
- zero inflation diagnostics
- supported operators
- unsupported operators
- uncertain operators
- required evidence
- missing evidence
- CARMA mechanism inventory

The CARMA mechanism inventory is included because the restored historical
CatBoost CARMA and Vector CARMA implementations contain valuable analytical
ideas. Phase 21 preserves those ideas as evidence, not as instructions to
recreate the historical wrappers.

## Strategy Planning Output

`aq_plan_forecasting_strategy()` converts discovery into:

- operator recommendations
- reasons
- required evidence
- missing evidence
- limitations
- recommended validation
- mandatory baselines
- smallest useful experiment set
- a canonical `forecast_planning_artifact`

The planning artifact supports downstream actions:

- forecast
- assess
- compare
- report
- campaign_review

Phase 22 consumes this planning artifact as evidence for one bounded
experiment. The planner still does not execute models. The experiment campaign
layer owns frozen-baseline challenger execution, learning assessment, negative
evidence, and advisory recommendations.

## Strategy Tuning, Feature Tuning, and Model Tuning

The historical CARMA family bundled many concepts into one large function.
Phase 21 separates them:

| Layer | Examples | Phase 21 behavior |
| --- | --- | --- |
| Strategy tuning | independent, grouped, pooled, hierarchy, hurdle, funnel, multi-target | represented as operator/family recommendations |
| Feature tuning | differencing, trends, anomalies, weights, lags, rolling windows, transforms, cross-target features, regime features | inventoried as future feature-tuning hypotheses |
| Model tuning | engine choice, loss function, complexity, regularization, weighting behavior, constraints, calibration, threshold policy, training window, bounded hyperparameter changes | recorded as future evidence-guided model-tuning hypotheses |

Feature-tuning mechanisms are not embedded permanently into every forecasting
operator. They are competing feature hypotheses that should eventually be
tested through bounded evidence-driven experiments.

Model tuning follows the same principle. Historical grid-search behavior is not
treated as a brute-force optimization pattern to reproduce. Engine choices,
losses, regularization, training windows, and bounded hyperparameter changes are
analytical hypotheses. Each should be compared against a frozen baseline, scored
with deterministic evidence, and either adopted or rejected through governed
learning.

The common evidence loop is:

```text
evidence
-> hypothesis
-> deterministic challenger
-> frozen-baseline evaluation
-> learning assessment
-> governed adoption or rejection
```

## CatBoost Vector/CARMA Mechanism Inventory

| Mechanism | Historical names | Purpose | Leakage risk | Modern owner | vNext status | Future capability |
| --- | --- | --- | --- | --- | --- | --- |
| Differencing | `Difference`, `CarmaDifferencing`, `antidiff` | Model change rather than level for nonstationary series, then reconstruct original-scale forecasts. | High if future levels are used for reintegration or if differencing crosses the forecast origin. | Rodeo + AutoQuant | partially represented | Forecast Feature Tuning |
| Trend handling | `TimeTrendVariable`, `TimeTrend` | Expose elapsed-time structure that tree models may not extrapolate from raw dates alone. | Low, but trend extrapolation can overstate stability after regime breaks. | Rodeo | partially represented | Forecast Feature Tuning |
| Anomaly treatment | `AnomalyDetection`, `tstat_high`, `tstat_low` | Separate bad observations or exceptional shocks from stable temporal signal. | High if thresholds are fit using future rows or if events are silently removed. | Rodeo + assessment layer | missing | Forecast Feature Tuning |
| Observation weighting | `TimeWeights`, `Weights` | Bias fitting toward recent or business-important observations. | Medium if weights use future outcome information or optimize business value outside validation. | AutoQuant + assessment layer | missing | Forecast Feature Tuning |
| Target transformations | `TargetTransformation`, `Methods`, `AutoTransformationCreate` | Stabilize variance, respect positivity, and improve loss behavior while preserving original-scale interpretation. | Medium if transforms are fit on future rows; inversion can bias original-scale metrics. | Rodeo + AutoQuant | partially represented | Forecast Feature Tuning |
| Lag and rolling features | `Lags`, `MA_Periods`, `SD_Periods`, `Skew_Periods`, `Kurt_Periods`, `Quantile_Periods` | Expose autoregressive memory, local averages, volatility, shape, and tail behavior. | High unless windows use strictly prior rows within sorted groups. | Rodeo | partially represented | Forecast Feature Tuning |
| Calendar and holiday features | `CalendarVariables`, `HolidayVariable`, `HolidayLags`, `HolidayMovingAverages` | Represent calendar, holiday, and event timing effects available before forecast time. | Medium if future event variables are not truly known. | Rodeo | partially represented | Forecast Feature Tuning |
| Fourier seasonality | `FourierTerms`, `CarmaFourier` | Represent periodicity compactly, including group-specific seasonality. | Low when based only on dates/groups; replay metadata still matters. | Rodeo | missing | Forecast Feature Tuning |
| Cross-target features | `AutoCatBoostVectorCARMA`, shared target histories | Test whether prior history of one target improves another target. | High for contemporaneous or future target values. | Rodeo + AutoQuant | partially represented | Forecast Feature Tuning |
| Recursive state update | `CarmaScore`, `UpdateFeatures`, `CarmaRollingStatsUpdate` | Keep future lag and rolling features coherent as forecasts are generated step by step. | High if recursive updates accidentally use realized future targets. | Rodeo + AutoQuant | partially represented | Forecast Strategy Tuning |
| Bounded model search | `GridTune`, `PassInGrid`, `ModelCount`, `MaxRunsWithoutNewWinner` | Explore bounded model-choice, loss, complexity, regularization, and training-window hypotheses. | Medium if validation is repeatedly observed without recorded selection evidence. | AutoQuant campaign/experiment layer | partially represented | Evidence-Guided Model Tuning |
| Forecast diagnostics | `EvalMetric`, importance, group/horizon metrics | Return evidence by horizon, group, target, feature, and transformation path. | Low, but transformed-scale-only diagnostics can mislead. | AutoQuant assessment layer | partially represented | Forecast Assessment Evidence |

## Gap Summary

Already represented correctly:

- deterministic operator/family contracts
- single-series, panel, hierarchy, intermittent-demand, funnel, multi-target,
  and cross-target strategy surfaces
- canonical artifacts and supported downstream actions
- Rodeo ownership of temporal and cross-target preparation
- negative-transfer evidence for cross-target strategies

Represented only superficially:

- differencing and original-scale reconstruction
- trend representation
- target transformations and inversion evidence
- recursive state replay
- feature usefulness diagnostics by horizon

Valuable mechanisms currently missing:

- anomaly policy experiments
- observation weighting experiments
- Fourier seasonal features
- regime indicators and break diagnostics
- formal feature-tuning evidence contracts

Belongs primarily in Rodeo:

- lag policies
- rolling windows
- calendar/holiday/Fourier features
- deterministic feature manifests
- feature fit/apply replay
- leakage diagnostics for feature construction

Belongs primarily in AutoQuant:

- strategy planning
- baseline comparison
- assessment evidence
- model/campaign experiment orchestration
- strategy and feature recommendation artifacts

Should become planner recommendations:

- collect hierarchy metadata
- test cross-target features only against independent and shared baselines
- test intermittent-demand methods only when sparsity evidence supports them
- record missing feature-tuning evidence before claiming the landscape is
  complete

Implemented campaign experiment handoff:

- forecast planning artifact
- one bounded hypothesis
- frozen baseline
- deterministic challenger
- out-of-sample comparison
- learning assessment
- advisory recommendation
- canonical `forecast_experiment_artifact`

Should become future campaign experiments:

- differencing versus level modeling
- trend feature variants
- anomaly treatment variants
- lag and rolling-window policies
- target transformations
- cross-target feature policies
- weighted versus unweighted training

Should be retired:

- monolithic engine-specific CARMA wrappers as the primary API
- implicit file writes
- hidden feature construction inside AutoQuant forecasting functions
- unbounded grid tuning as default behavior
- model tuning framed as brute-force AutoML rather than evidence-guided
  challenger testing
- automatic strategy selection without evidence

## Example

```r
discovery <- aq_discover_forecasting_capabilities(
  data = forecasting_data,
  target = c("leads", "applications", "enrollments"),
  date = "date",
  hierarchy = "region",
  known_future_variables = "promo",
  horizon = 3
)

plan <- aq_plan_forecasting_strategy(discovery)

plan$recommendations
plan$required_baselines
plan$experiment_set
plan$artifact
```

## Validation

Phase 21 QA is provided by:

```r
qa_vnext_forecasting_planning()
```

Phase 22 experiment QA is provided by:

```r
qa_vnext_forecasting_experiment_campaigns()
```

The installed package QA includes this suite through:

```r
qa_autoquant_package()
```
