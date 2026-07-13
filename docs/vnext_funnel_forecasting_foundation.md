# AutoQuant vNext Funnel Forecasting Foundation

Status: Phase 18 implemented as an additive vNext forecasting contract.

Funnel forecasting is treated as a structured transition process rather than a
set of unrelated target forecasts. A funnel preserves separate evidence for:

- stage volume
- adjacent transition probability
- cohort identity
- calendar timing
- stage timing
- maturity
- cumulative downstream outcome

The implementation does not reproduce the historical Funnel CARMA wrappers. It
extracts their analytical intent and places it inside the existing vNext
forecasting, artifact, assessment, comparison, and campaign evidence contracts.

## Public API

- `aq_funnel_forecast_spec()`
- `aq_validate_funnel_forecast_spec()`
- `aq_fit_funnel_forecast()`
- `aq_assess_funnel_forecast()`
- `aq_compare_funnel_strategies()`
- `qa_vnext_funnel_forecasting_foundation()`

## Data Shape

Phase 18 expects long-form funnel data:

| column role | meaning |
| --- | --- |
| cohort | cohort, campaign, batch, or entry group |
| date | calendar observation date |
| stage | ordered stage identifier |
| value | nonnegative stage volume |
| stage_date | optional date associated with the stage |
| maturity | optional cohort/stage maturity measure |

Stages are supplied explicitly in the specification. Adjacent transitions are
derived only from the ordered stage list. The implementation does not infer
hidden transitions.

## Stage Philosophy

Stage forecasting preserves stage identity and forecasts each stage volume as
its own deterministic evidence row. This is useful when the analyst wants to
inspect whether each stage is stable on its own.

Current Phase 18 behavior uses a deterministic continuation baseline from the
latest observed stage value for each cohort. It is intentionally simple. Future
supervised or statistical stage models can replace the baseline without
changing the artifact contract.

## Transition Philosophy

Transition forecasting treats the funnel as a process. It estimates adjacent
conversion rates from observed transitions and propagates the first-stage
volume downstream through the explicit transition chain.

This makes conversion evidence first-class:

```text
stage A volume
-> A_to_B conversion
-> stage B volume
-> B_to_C conversion
-> stage C volume
```

No automatic strategy selection is performed.

## Maturity

Maturity is recorded explicitly when a maturity column is supplied. If no
maturity column exists, Phase 18 derives a conservative maturity proxy from
cohort/date ordering. Maturity evidence is stored in forecast artifacts and
assessment results so future campaigns can ask whether uncertainty is caused by
immature cohorts rather than poor stage or transition estimates.

## Artifacts

`aq_fit_funnel_forecast()` creates a canonical table artifact with:

- stage identity
- transition identity
- cohort identity
- maturity
- forecast horizon
- forecast date
- forecast volume
- realized actuals where present
- transition diagnostics
- stage summary
- supported downstream actions

The artifact is wrapped with the canonical vNext artifact envelope using
`task = "funnel_forecast"`.

## Assessment

`aq_assess_funnel_forecast()` computes:

- aggregate forecast metrics
- stage-level accuracy
- horizon-level accuracy
- conversion accuracy
- aggregate final-stage outcome accuracy
- maturity coverage

Assessment remains deterministic and artifact-backed.

## Strategy Comparison

`aq_compare_funnel_strategies()` compares:

- stage forecasting
- transition forecasting

The recommendation is advisory only. Phase 18 does not implement automatic
strategy selection, optimization, or deployment.

## Historical Capability Intent

The restored historical Funnel implementation showed several durable ideas:

- funnels depend on a base forward-looking measure
- downstream outcomes are often modeled as conversion rates
- cohort period / maturity is central to interpretation
- calendar date and cohort date are different concepts
- forward-looking base volumes may be known while downstream conversions are
  unknown
- recursive scoring extends cohorts through maturity periods
- reporting should separate base volumes, conversion, and final outcomes

## Modern Architectural Placement

Modern vNext placement:

```text
Forecasting
-> Panel / hierarchy / strategy evidence where needed
-> Funnel forecasting
   -> stage strategy
   -> transition strategy
   -> maturity evidence
   -> canonical funnel artifacts
   -> deterministic assessment
   -> strategy comparison
```

Funnel forecasting is now a first-class forecasting family that reuses the
existing artifact and assessment philosophy.

## Retired Implementation Assumptions

Phase 18 does not preserve:

- the historical CARMA public API shape
- giant engine-specific wrappers as the main interface
- direct file writing as a required side effect
- grid tuning
- transformation-heavy wrapper internals
- model-specific scoring loops
- automatic production selection

## Limitations

Not implemented in Phase 18:

- AutoML
- optimization-based transition selection
- inventory or capacity optimization
- deployment
- vector forecasting
- advanced hierarchy optimization
- supervised CatBoost funnel models
- automatic transition discovery
- causal interpretation of transition failures

The foundation is deliberately deterministic so future model-based operators
can inherit the same specification, artifact, assessment, and comparison
contracts.
