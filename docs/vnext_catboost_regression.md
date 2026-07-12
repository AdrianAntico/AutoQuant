# AutoQuant vNext CatBoost Supervised Learning

Status: Phase 2 regression vertical slice implemented. Phase 3 binary classification vertical slice implemented. Phase 4 canonical scoring, delayed outcome attachment, realized assessment, and bounded monitoring foundation implemented. Phase 5 implements Rodeo transformation fit/replay for training and scoring. Phase 6 implements portable model bundles for save/load/validate and reproducible inference. Phase 7 implements the canonical analytical artifact envelope, relationship extraction, supported-action inspection, and deterministic artifact validation.

This document describes the first implemented AutoQuant vNext supervised-learning operator paths. The scope is intentionally narrow: CatBoost regression and CatBoost binary classification only.

The goal is to prove the new lifecycle contract without recreating the historical monolithic wrapper architecture.

```text
model spec
-> optional Rodeo transformation spec
-> deterministic fit
-> fitted transformation replay
-> canonical analytical artifact envelope
-> portable model bundle
-> save / load
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
- `aq_save_model_bundle()`
- `aq_load_model_bundle()`
- `aq_validate_model_bundle()`
- `aq_artifact_envelope()`
- `aq_artifact_relationships()`
- `aq_supported_actions()`
- `aq_validate_artifact()`
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
- `qa_vnext_rodeo_transformation_replay()`
- `qa_vnext_model_bundle()`
- `qa_vnext_artifact_framework()`
- `qa_vnext_scoring_lifecycle()`

The installed-package QA entry point `qa_autoquant_package()` includes the vNext CatBoost regression, binary, Rodeo transformation replay, model bundle, canonical artifact framework, and scoring lifecycle QA suites.

## Supported Scope

Implemented:

- supervised regression
- supervised binary classification
- CatBoost engine
- random and time-based train/validation partition specifications
- direct feature consumption from supplied data when no transformation spec is supplied
- optional Rodeo transformation specification references in `aq_model_spec()`
- fitted Rodeo transformation state learned during `aq_fit_model()`
- prepared training and validation data created by replaying the fitted Rodeo transformation
- serialized fitted transformation state in the fit result
- model artifacts that record transformation spec id, fitted transformation id, transformation metadata, replay requirement, prepared feature manifest, and prepared dataset identities
- portable model bundles containing the model specification, model object, fit metadata, feature manifest, compact categorical level manifest, serialized Rodeo transformation, raw/prepared schema fingerprints, lineage, warnings, versions, and supported actions
- metadata-only bundle inspection without loading the model object
- bundle validation for required fields, version compatibility, model availability, transformation availability, feature manifest coverage, and supported actions
- reloaded bundle scoring through the same `aq_score_model()` path
- raw scoring data replay through the fitted Rodeo transformation before scoring
- transformation replay diagnostics, warnings, schema fingerprints, and lineage in scoring artifacts
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
- canonical analytical artifact envelopes for vNext fit, prediction, scoring, assessment, monitoring, outcome attachment, and model bundle artifacts
- deterministic artifact relationship extraction through parent artifact ids
- supported downstream action inspection through `aq_supported_actions()`
- artifact validation for required envelope metadata, version, lineage shape, supported action normalization, and consumer expectations
- canonical scoring specifications and scoring artifacts for new populations
- row identity preservation or explicit generated `.aq_row_id`
- scoring-data validation for missing transformation inputs, missing prepared features, feature type compatibility, extra-column policy, duplicate row identities, target leakage warnings, missingness warnings, and unseen categorical-level warnings
- binary threshold-policy reapplication against existing probability evidence without model refitting or repeated probability scoring
- delayed outcome attachment by row identity without mutating historical prediction probabilities
- realized scoring assessment through the existing regression and binary metric engines
- bounded deterministic monitoring evidence covering schema diagnostics, transformation replay compatibility, scoring population size, prediction/probability distributions, predicted class distribution, outcome distribution, and compatible baseline summaries
- serialization round-trip QA for artifact payloads
- AnalyticsShinyApp-consumable list/table artifact shapes

Not implemented in this slice:

- multiclass
- tuning/challenger search
- SHAP/contribution generation
- report rendering
- H2O/XGBoost/LightGBM adapters
- AutoPlots rendering
- production deployment
- automated retraining
- threshold optimization
- alert infrastructure
- probabilistic drift detection
- model registry, remote serving, REST APIs, cloud storage, or deployment

## Canonical Analytical Artifact Framework

Phase 7 introduces a common envelope for analytical artifacts. The envelope is intentionally a compact explicit contract rather than a generic object framework.

Every canonical vNext artifact can expose:

- `artifact_id`
- `artifact_type`
- `artifact_version`
- `envelope_version`
- `parent_artifact_ids`
- `lineage`
- `task`
- `operator`
- `engine`
- `specification_id`
- `dataset_id`
- `prepared_dataset_id`
- `transformation_id`
- `model_id`
- `campaign_references`
- `warnings`
- `supported_actions`
- `producer`
- `consumer_expectations`
- `created_at`

Use:

```r
envelope <- aq_artifact_envelope(artifact)
relationships <- aq_artifact_relationships(artifact)
actions <- aq_supported_actions(artifact)
diagnostics <- aq_validate_artifact(artifact)
```

The relationship model is deterministic and list-based. It does not require a graph database. Typical lifecycle edges are represented through `parent_artifact_ids`:

```text
model specification
-> fit artifact
-> prediction or scoring artifact
-> outcome attachment
-> realized assessment
-> monitoring evidence
-> model bundle
```

The envelope is designed for:

- AnalyticsShinyApp consumers that should reason over artifact metadata rather than engine-specific model classes
- campaign systems that need stable identity, lineage, and supported downstream actions
- future operators such as forecasting, panel forecasting, MMM, and causal modeling

Future operators should extend the artifact family by supplying the same envelope fields and task/operator-specific payloads. They should not invent a new result philosophy unless the canonical envelope is genuinely insufficient.

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

## Rodeo Transformation Replay

Phase 5 makes fitted transformations part of the model contract. A model specification may reference a deterministic `Rodeo::rodeo_transformation_spec()`. AutoQuant validates that reference, fits the Rodeo transformation on the training split, serializes the fitted transformation, replays it into training and validation data, and then fits CatBoost on the prepared features.

Scoring uses the same fitted transformation state. `aq_score_model()` accepts raw scoring data, validates it against the fitted Rodeo transformation schema, replays the transformation, validates the prepared feature manifest, and only then scores the model.

AutoQuant does not implement transformation engines. Rodeo owns transformation semantics, learned state, schema validation, serialization, and apply behavior. AutoQuant orchestrates that contract and preserves lineage.

Example with generated date features:

```r
date_spec <- Rodeo::rodeo_transformation_spec(
  "date_features",
  input_columns = "event_date",
  parameters = list(features = c("year", "month"))
)

spec <- aq_model_spec(
  target = "revenue",
  features = c("channel", "spend", "clicks", "event_date_year", "event_date_month"),
  partition = aq_partition_spec(method = "time", split_col = "event_date"),
  transformation_spec = date_spec,
  engine_params = list(iterations = 25L, depth = 4L, learning_rate = 0.08)
)

fit <- aq_fit_model(spec, raw_training_data)

score <- aq_score_model(
  fit,
  new_data = raw_scoring_data,
  row_id_cols = "customer_id"
)
```

Replay behavior is deterministic:

- the transformation is fitted during training, not scoring
- scoring never refits or relearns transformation state
- missing required raw transformation inputs fail before scoring
- prepared feature schema is validated after replay
- row identity and delayed outcome attachment continue to operate on the scoring artifact
- monitoring records transformation compatibility and replay status

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

The scoring artifact stores row identity, model lineage, raw and prepared dataset fingerprints, prepared feature manifest, transformation replay status, schema diagnostics, prediction columns, threshold policy metadata, outcome status, warnings, and supported next actions. It preserves scored predictions and compact evidence; it does not embed an unnecessary copy of the full scoring dataset.

## Portable Model Bundles

Phase 6 introduces the canonical vNext model bundle. A bundle is a portable analytical asset that preserves enough state to score new raw data without reconstructing the original development session.

```r
manifest <- aq_save_model_bundle(
  fit,
  path = "models/campaign_response_bundle",
  overwrite = TRUE
)

metadata <- aq_load_model_bundle(
  "models/campaign_response_bundle",
  metadata_only = TRUE
)

bundle <- aq_load_model_bundle("models/campaign_response_bundle")
diagnostics <- aq_validate_model_bundle(bundle)

score <- aq_score_model(
  bundle,
  new_data = raw_scoring_data,
  row_id_cols = "customer_id"
)
```

The bundle contains:

- model specification
- fit metadata
- task and engine
- model object
- feature schema and compact categorical level manifest
- partition identity
- fit artifact
- serialized fitted Rodeo transformation when replay is required
- transformation lineage
- raw and prepared schema fingerprints
- training metadata
- warnings
- bundle id, bundle version, model id, fit id, specification id, and supported actions

The saved bundle path contains:

- `model_bundle.rds`: full scoring-capable bundle
- `metadata.rds`: lightweight metadata for inspection without loading the model object

If a `.rds` file path is supplied, AutoQuant saves the full bundle to that file and writes a sibling `*_metadata.rds` file.

Bundles intentionally do not embed copies of the raw training, prepared training, raw validation, or prepared validation rows. They preserve feature schemas, compact categorical level manifests, schema fingerprints, transformation lineage, and training metadata instead.

`aq_load_model_bundle()` returns an object with classes:

```r
c("aq_model_bundle", "aq_fit_result", "list")
```

That means the bundle can be passed directly to `aq_score_model()`. Reloaded scoring follows the same path as in-session scoring:

```text
raw scoring data
-> fitted Rodeo transformation replay when required
-> prepared feature validation
-> CatBoost prediction
-> scoring artifact
```

No refitting, relearning, registry lookup, remote serving, deployment layer, or hidden global state is involved.

## Monitoring Foundation

`aq_monitor_scoring()` returns bounded deterministic monitoring evidence. It is deliberately conservative. It can summarize schema diagnostics, transformation replay compatibility, prediction distributions, binary probability and predicted-class distributions, outcome availability, outcome distributions, and compatible baseline summaries.

It does not implement automated alerts, deployment monitoring, retraining, probabilistic drift engines, or background jobs.

## Historical Scoring Migration

The restored historical scoring functions performed several valuable jobs:

| Historical path | Analytical job | vNext Phase 4 coverage | Classification | Remaining gap |
| --- | --- | --- | --- | --- |
| `AutoH2OMLScoring()` | Score new data with H2O model objects/MOJOs, optional H2O startup/shutdown, model-data preparation, optional inverse transformations | vNext scores CatBoost fit artifacts through `aq_score_model()` and preserves row/model lineage | Compatibility-only for H2O; partially replaced analytically | H2O engine and MOJO support are intentionally not implemented |
| `AutoCatBoostScoring()` | Score CatBoost models, optionally return features/SHAP, manage model-data preparation and transformations | vNext covers CatBoost regression/binary scoring, fitted Rodeo transformation replay, threshold decisions, delayed outcomes, realized assessment, monitoring evidence | Partially replaced | SHAP generation remains outside this phase |
| `AutoXGBoostScoring()` | Score XGBoost models with dummification, transformations, multiclass, optional SHAP | vNext scoring contract replaces the lifecycle shape, not the XGBoost engine | Compatibility-only | XGBoost, multiclass, SHAP, and transformation application are future slices |
| `AutoLightGBMScoring()` | Score LightGBM models with feature prep, transformations, multiclass, optional SHAP | vNext scoring contract replaces the lifecycle shape, not the LightGBM engine | Compatibility-only | LightGBM, multiclass, SHAP, and transformation application are future slices |

The migration implication is that vNext should preserve the valuable lifecycle jobs: validate scoring data, score new populations, preserve row/model lineage, support delayed outcomes, assess realized performance, and produce monitoring evidence. It should not preserve H2O-era engine startup/shutdown behavior, broad side effects, or monolithic scoring wrappers.

## Artifact Contract

`aq_fit_model()` returns an `aq_fit_result` containing:

- `spec`
- in-memory CatBoost model object
- partition summary
- feature schema
- optional Rodeo transformation spec
- optional fitted Rodeo transformation
- serialized fitted transformation state
- transformation lineage
- raw and prepared schema fingerprints
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
- transformation replay diagnostics when the model requires replay
- warnings
- `new_table_artifact()` table artifact
- model, fit, dataset, and feature lineage
- outcome attachment status

`aq_attach_outcomes()` returns an updated `aq_scoring_result` with an `aq_outcome_attachment_artifact`.

`aq_monitor_scoring()` returns an `aq_scoring_monitoring_result` with an `aq_scoring_monitoring_artifact`.

These artifacts are designed to be consumed by AnalyticsShinyApp, collector workflows, GenAI context packaging, future campaign operators, and comparison/report layers without requiring a report render during model fitting or scoring.

## Rodeo Boundary

This slice does not duplicate feature engineering.

AutoQuant consumes a Rodeo transformation specification when supplied, calls Rodeo to fit and apply it, and preserves the resulting fitted transformation state as model lineage. Learned transformations, scoring-safe feature preparation, date expansion, categorical policies, schema validation, and fitted transformation serialization remain Rodeo responsibilities.

AutoQuant is responsible for enforcing that scoring replays the fitted state rather than refitting it.

## Optional Dependency Behavior

`catboost` remains an optional dependency. AutoQuant does not install it automatically.

If `catboost` is unavailable, `aq_fit_model()` fails with a clear message. The app/package can still load, and non-CatBoost functionality remains available.

## Migration Notes

The existing `generate_catboost_builder_artifacts()` contract remains the AnalyticsShinyApp-facing transitional path. Phase 2 and Phase 3 do not replace it.

The new vNext path exists beside the legacy builder so the contract can be validated before downstream consumers migrate. A later phase may refactor the builder internals to call vNext operators while preserving its current public shape.
