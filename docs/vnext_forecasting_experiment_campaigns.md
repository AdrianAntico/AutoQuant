# AutoQuant vNext Forecasting Experiment Campaigns

Status: Phase 22 implemented.

Forecasting experiment campaigns connect deterministic forecasting planning to
bounded empirical learning. The planner proposes what evidence is worth
collecting next. The experiment layer evaluates one frozen baseline against one
deterministic challenger and records the result as canonical evidence.

This is not AutoML. It does not perform broad search, Bayesian optimization,
evolutionary search, automatic adoption, or unlimited feature enumeration.

## Public API

- `aq_forecast_experiment_spec()`
- `aq_run_forecast_experiment()`
- `aq_run_forecast_experiment_campaign()`
- `qa_vnext_forecasting_experiment_campaigns()`

## Philosophy

Forecast experiments answer:

```text
What should we test next?
```

They do not answer:

```text
What else can we search?
```

Every experiment should exist because planning evidence suggests it may reduce
uncertainty. The experiment result may be positive, negative, or inconclusive.
All three outcomes are useful evidence.

## Lifecycle

```text
forecast planning artifact
-> one bounded hypothesis
-> frozen baseline
-> deterministic challenger
-> out-of-sample comparison
-> learning assessment
-> advisory recommendation
-> canonical experiment evidence
```

## Experiment Specification

`aq_forecast_experiment_spec()` records:

- experiment id
- experiment type: `strategy`, `feature`, or `model`
- planning artifact id
- baseline identity
- challenger identity
- target/date/horizon/origin
- hypothesis
- expected evidence
- acceptance criteria
- rejection criteria
- supported downstream actions

The public API remains flat. Baselines and challengers can be supplied as simple
ids or small lists. The specification records whether the candidate belongs to a
known strategy, feature, or model family.

## Supported Experiment Families

Phase 22 supports specification of three governed families:

| Family | Supported candidate ids |
| --- | --- |
| Strategy | `independent`, `grouped`, `global`, `cross_target`, `direct`, `recursive` |
| Feature | CARMA mechanism inventory entries with `tuning_layer == "feature_tuning"` |
| Model | `naive`, `seasonal_naive`, `ets`, `arima`, `catboost`, `croston`, `sba`, `tsb`, `hurdle` |

Execution in Phase 22 focuses on the first deterministic vertical slice:
single-series model experiments through the existing `aq_forecast_spec()`,
`aq_fit_forecast()`, and `aq_assess_forecast()` operators. Feature and strategy
hypotheses are first-class governed specifications but are not expanded into
combinatorial searches.

## Frozen Baselines

Every run preserves:

- baseline candidate id
- challenger candidate id
- forecast origin
- evaluation window
- forecast ids
- assessment ids
- artifact lineage

The challenger never mutates the baseline. A precomputed baseline can be
supplied to `aq_run_forecast_experiment()` when a workflow already owns a frozen
baseline artifact.

## Learning Assessment

The learning table records:

- primary metric
- baseline value
- challenger value
- relative improvement
- outcome
- recommendation
- reason
- whether uncertainty decreased
- warnings

Supported outcomes include:

- `accepted`
- `rejected`
- `inconclusive`
- `negative_evidence`
- `partial_improvement`
- `problem_specific_improvement`

Supported recommendations include:

- `adopt_challenger`
- `retain_baseline`
- `run_feature_experiment`
- `run_strategy_experiment`
- `run_model_experiment`
- `collect_more_evidence`
- `stop`

Recommendations are advisory. Phase 22 never adopts a challenger automatically.

## Negative Evidence

Failed experiments are preserved. If the challenger cannot run, cannot be
assessed, or degrades the primary metric, the experiment still returns a
canonical `forecast_experiment_artifact`. This prevents unsuccessful ideas from
disappearing and allows future campaigns, knowledge systems, and transfer
layers to learn what did not work.

## Campaigns

`aq_run_forecast_experiment_campaign()` runs a bounded list of already-specified
experiments. It does not branch into search trees. Each experiment is still
evaluated as:

```text
one hypothesis -> one challenger -> one learning assessment
```

The campaign artifact summarizes experiment status, outcomes,
recommendations, and warnings for downstream consumers.

## AnalyticsShinyApp Compatibility

Experiment and campaign artifacts use canonical artifact envelopes and expose
`campaign_review` in supported actions. They are designed to be consumed by
AnalyticsShinyApp, reports, Mission Control, and future governed campaign
surfaces without reading engine-specific forecast objects.

## Example

```r
discovery <- aq_discover_forecasting_capabilities(
  data = history,
  target = "demand",
  date = "date",
  horizon = 7
)

plan <- aq_plan_forecasting_strategy(discovery)

experiment <- aq_forecast_experiment_spec(
  planning_result = plan,
  experiment_type = "model",
  baseline = "naive",
  challenger = "ets",
  target = "demand",
  date = "date",
  horizon = 7,
  hypothesis = "ETS may reduce out-of-sample error relative to naive."
)

result <- aq_run_forecast_experiment(experiment, history)

result$learning
result$artifact
```

## Validation

Phase 22 QA is provided by:

```r
qa_vnext_forecasting_experiment_campaigns()
```

The installed package QA includes this suite through:

```r
qa_autoquant_package()
```
