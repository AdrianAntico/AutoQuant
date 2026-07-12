# AutoQuant vNext CatBoost Supervised Learning

Status: Phase 2 regression vertical slice implemented. Phase 3 binary classification vertical slice implemented. Phase 4 canonical scoring, delayed outcome attachment, realized assessment, and bounded monitoring foundation implemented.

This document describes the first implemented AutoQuant vNext supervised-learning operator paths. The scope is intentionally narrow: CatBoost regression and CatBoost binary classification only.

The goal is to prove the new lifecycle contract without recreating the historical monolithic wrapper architecture.

```text
model spec
-> deterministic fit
-> prediction artifact
-> task-specific assessment artifacts
-> scoring specification
-> scoring artifact
-> delayed outcome attachment
-> realized scoring assessment
-> bounded monitoring evidence
-> comparison-ready outputs
```

## Public API

The Phase 2 API is:

- `aq_partition_spec()`
- `aq_threshold_policy()`
- `aq_model_spec()`
- `aq_validate_model_spec()`
- `aq_fit_model()`
- `aq_predict_model()`
- `aq_scoring_spec()`
- `aq_score_model()`
- `aq_apply_threshold_policy()`
- `aq_attach_outcomes()`
- `aq_assess_scoring()`
- `aq_monitor_scoring()`
- `aq_assess_model()`
- `aq_model_operator_capabilities()`
- `qa_vnext_catboost_regression()`
- `qa_vnext_catboost_binary()`
- `qa_vnext_scoring_lifecycle()`

The installed-package QA entry point `qa_autoquant_package()` includes the vNext CatBoost regression and binary QA suites.

## Supported Scope

Implemented:

- supervised regression
- supervised binary classification
- CatBoost engine
- random and time-based train/validation partition specifications
- direct feature consumption from supplied data
- validation of target, feature columns, feature types, partition settings, threshold policy, class shape, and supported engine parameters
- deterministic fit through `catboost::catboost.train()`
- prediction on training, validation, all fit data, or new data
- regression prediction artifacts with `Predict`, optional residuals, target metadata, model id, fit id, and dataset id
- binary prediction artifacts with `PositiveProbability`, `Predict`, threshold metadata, positive/negative class metadata, model id, fit id, and dataset id
- regression metrics: RMSE, MAE, R2, mean residual, row count
- binary metrics: log loss, AUC, accuracy, precision, recall, F1, specificity, sensitivity
- binary confusion matrix
- calibration and residual tables
- binary probability diagnostics and deterministic calibration summaries
- optional subgroup metrics
- typed fit, prediction, and assessment artifacts
- canonical scoring specifications and scoring artifacts for new populations
- row identity preservation or explicit generated `.aq_row_id`
- scoring-data validation for missing features, feature type compatibility, extra-column policy, duplicate row identities, target leakage warnings, missingness warnings, and unseen categorical-level warnings
- binary threshold-policy reapplication against existing probability evidence without model refitting or repeated probability scoring
- delayed outcome attachment by row identity without mutating historical prediction probabilities
- realized scoring assessment through the existing regression and binary metric engines
- bounded deterministic monitoring evidence covering schema diagnostics, scoring population size, prediction/probability distributions, predicted class distribution, outcome distribution, and compatible baseline summaries
- serialization round-trip QA for artifact payloads
- AnalyticsShinyApp-consumable list/table artifact shapes

Not implemented in this slice:

- multiclass
- tuning/challenger search
- SHAP/contribution generation
- report rendering
- H2O/XGBoost/LightGBM adapters
- AutoPlots rendering
- Rodeo feature engineering or transformation fitting
- model object persistence policy beyond the returned in-memory fit result
- production deployment
- automated retraining
- threshold optimization
- alert infrastructure
- probabilistic drift detection

## Example

```r
library(data.table)
library(AutoQuant)

dt <- data.table(
  channel = sample(c("Search", "Email", "Social"), 300, TRUE),
  spend = runif(300, 50, 500),
  clicks = rpois(300, 40)
)
dt[, revenue := 10 + 2.5 * spend + 1.8 * clicks +
  fifelse(channel == "Search", 80, 0) + rnorm(.N, 0, 35)]

spec <- aq_model_spec(
  target = "revenue",
  features = c("channel", "spend", "clicks"),
  partition = aq_partition_spec(method = "random", train_fraction = 0.8),
  engine_params = list(iterations = 25L, depth = 4L, learning_rate = 0.08)
)

validation <- aq_validate_model_spec(spec, dt)
fit <- aq_fit_model(spec, dt)
pred <- aq_predict_model(fit, dataset = "validation")
assessment <- aq_assess_model(fit, pred, by = "channel")
```

Binary classification uses the same lifecycle with an explicit threshold policy:

```r
policy <- aq_threshold_policy(threshold = 0.45, positive_class = "yes")

binary_spec <- aq_model_spec(
  task = "binary",
  target = "converted",
  features = c("channel", "spend", "clicks"),
  threshold_policy = policy,
  engine_params = list(iterations = 25L, depth = 4L, learning_rate = 0.08)
)

binary_fit <- aq_fit_model(binary_spec, binary_data)
binary_pred <- aq_predict_model(binary_fit, dataset = "validation")
binary_assessment <- aq_assess_model(binary_fit, binary_pred, by = "channel")
```

## Canonical Scoring Lifecycle

`aq_predict_model()` remains a compatibility-oriented convenience for scoring training, validation, all fit data, or ad hoc new data. Phase 4 introduces `aq_score_model()` as the canonical vNext scoring lifecycle for new scoring populations.

The scoring lifecycle is:

```r
score <- aq_score_model(
  fit,
  new_data = scoring_data,
  row_id_cols = "customer_id",
  dataset_id = "campaign_scoring_2026_07"
)

score_with_outcomes <- aq_attach_outcomes(
  score,
  outcomes = realized_data,
  outcome_col = "actual_revenue"
)

realized <- aq_assess_scoring(score_with_outcomes)
monitoring <- aq_monitor_scoring(score_with_outcomes)
```

For binary classification, probability and decision evidence are intentionally separate:

```text
PositiveProbability
-> threshold policy
-> Predict
```

This allows a new threshold policy to be applied without refitting the model and without repeating probability scoring:

```r
new_policy <- aq_threshold_policy(
  threshold = 0.65,
  positive_class = "yes",
  negative_class = "no"
)

rethresholded <- aq_apply_threshold_policy(score, new_policy)
```

The scoring artifact stores row identity, model lineage, feature manifest, schema diagnostics, prediction columns, threshold policy metadata, outcome status, warnings, and supported next actions. It preserves scored predictions and compact evidence; it does not embed an unnecessary copy of the full scoring dataset.

## Monitoring Foundation

`aq_monitor_scoring()` returns bounded deterministic monitoring evidence. It is deliberately conservative. It can summarize schema diagnostics, prediction distributions, binary probability and predicted-class distributions, outcome availability, outcome distributions, and compatible baseline summaries.

It does not implement automated alerts, deployment monitoring, retraining, probabilistic drift engines, or background jobs.

## Historical Scoring Migration

The restored historical scoring functions performed several valuable jobs:

| Historical path | Analytical job | vNext Phase 4 coverage | Classification | Remaining gap |
| --- | --- | --- | --- | --- |
| `AutoH2OMLScoring()` | Score new data with H2O model objects/MOJOs, optional H2O startup/shutdown, model-data preparation, optional inverse transformations | vNext scores CatBoost fit artifacts through `aq_score_model()` and preserves row/model lineage | Compatibility-only for H2O; partially replaced analytically | H2O engine and MOJO support are intentionally not implemented |
| `AutoCatBoostScoring()` | Score CatBoost models, optionally return features/SHAP, manage model-data preparation and transformations | vNext covers CatBoost regression/binary scoring, threshold decisions, delayed outcomes, realized assessment, monitoring evidence | Partially replaced | SHAP generation and Rodeo transformation replay remain outside this phase |
| `AutoXGBoostScoring()` | Score XGBoost models with dummification, transformations, multiclass, optional SHAP | vNext scoring contract replaces the lifecycle shape, not the XGBoost engine | Compatibility-only | XGBoost, multiclass, SHAP, and transformation application are future slices |
| `AutoLightGBMScoring()` | Score LightGBM models with feature prep, transformations, multiclass, optional SHAP | vNext scoring contract replaces the lifecycle shape, not the LightGBM engine | Compatibility-only | LightGBM, multiclass, SHAP, and transformation application are future slices |

The migration implication is that vNext should preserve the valuable lifecycle jobs: validate scoring data, score new populations, preserve row/model lineage, support delayed outcomes, assess realized performance, and produce monitoring evidence. It should not preserve H2O-era engine startup/shutdown behavior, broad side effects, or monolithic scoring wrappers.

## Artifact Contract

`aq_fit_model()` returns an `aq_fit_result` containing:

- `spec`
- in-memory CatBoost model object
- partition summary
- feature schema
- training metadata
- validation diagnostics
- `fit_artifact`

`fit_artifact` is a lightweight typed artifact with class:

```r
c("aq_supervised_fit_artifact", "aq_result_artifact", "list")
```

`aq_predict_model()` returns an `aq_prediction_result` containing:

- scored data
- `Predict`
- `PositiveProbability` for binary classification
- optional `residual`
- threshold policy metadata for binary classification
- prediction metadata
- `new_table_artifact()` table artifact

`aq_assess_model()` returns an `aq_assessment_result` containing:

- metrics table
- residual table
- binary confusion matrix
- calibration table
- binary probability diagnostics table
- optional subgroup metrics table
- table artifacts for assessment outputs
- assessment artifact with class:

```r
c("aq_regression_assessment_artifact", "aq_result_artifact", "list")
```

For binary classification, the assessment artifact has class:

```r
c("aq_binary_assessment_artifact", "aq_result_artifact", "list")
```

`aq_score_model()` returns an `aq_scoring_result` containing:

- `scoring_spec`
- row identity metadata
- compact scored evidence
- `Predict`
- `PositiveProbability` for binary classification
- threshold decision history for binary classification
- scoring schema diagnostics
- warnings
- `new_table_artifact()` table artifact
- model, fit, dataset, and feature lineage
- outcome attachment status

`aq_attach_outcomes()` returns an updated `aq_scoring_result` with an `aq_outcome_attachment_artifact`.

`aq_monitor_scoring()` returns an `aq_scoring_monitoring_result` with an `aq_scoring_monitoring_artifact`.

These artifacts are designed to be consumed by AnalyticsShinyApp, collector workflows, GenAI context packaging, future campaign operators, and comparison/report layers without requiring a report render during model fitting or scoring.

## Rodeo Boundary

This slice does not duplicate feature engineering.

AutoQuant consumes the columns provided in `features`. It validates whether they exist and whether their types are compatible with the current CatBoost regression path. Learned transformations, scoring-safe feature preparation, date expansion, categorical policies beyond CatBoost-compatible factor handling, and broader fit/apply transformation lineage remain Rodeo responsibilities.

Future integration should allow a Rodeo preparation artifact or transform spec reference to appear in the model spec and fit lineage. That is intentionally outside Phase 2.

## Optional Dependency Behavior

`catboost` remains an optional dependency. AutoQuant does not install it automatically.

If `catboost` is unavailable, `aq_fit_model()` fails with a clear message. The app/package can still load, and non-CatBoost functionality remains available.

## Migration Notes

The existing `generate_catboost_builder_artifacts()` contract remains the AnalyticsShinyApp-facing transitional path. Phase 2 and Phase 3 do not replace it.

The new vNext path exists beside the legacy builder so the contract can be validated before downstream consumers migrate. A later phase may refactor the builder internals to call vNext operators while preserving its current public shape.
