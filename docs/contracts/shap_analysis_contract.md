# Prediction Surface / Contribution Analysis Contract

## Scope

AutoQuant Prediction Surface / Contribution Analysis is a reusable analytical framework for understanding how model predictions change across variables, observations, interactions, groups, and time.

SHAP is the first contribution backend. It is not the conceptual center.

The framework must support:

- static reports
- dashboards
- Shiny applications
- APIs
- scheduled monitoring
- LLM agents
- future AutoQuant analytical modules

Reports are renderers only. They must never recompute analytical artifacts.

This contract covers the long-term framework plus the first convenience wrappers:

- `PredictionSurfaceAnalysis()`
- `RegressionShapAnalysis()`
- `BinaryClassificationShapAnalysis()`

Current transitional generator names may remain while the implementation migrates:

- `generate_regression_shap_analysis_artifacts()`
- `generate_binary_classification_shap_analysis_artifacts()`

Report wrappers follow the standard rendering API:

```r
Report(
  artifact_result = NULL,
  data = NULL,
  OutputPath = getwd(),
  OutputFile = NULL,
  Title = NULL,
  Subtitle = NULL,
  Theme = "dark",
  Open = FALSE,
  Quiet = TRUE,
  ...
)
```

## 1. User Intent

### Core Intent

The user wants to understand the model prediction surface:

- Which variables materially influence predictions?
- How does each variable influence predictions?
- Are effects linear, nonlinear, monotonic, thresholded, saturated, unstable, or segment-specific?
- Which feature values increase or decrease predictions?
- Which observations are driven by which variables?
- Which feature interactions materially change model behavior?
- Do effects differ across groups, time windows, prediction bands, or error bands?
- Are explanations stable enough to trust?
- Are explanations changing between reference data and current data?
- Are there warnings, pitfalls, or misleading interpretations?
- What should the analyst investigate next?

### Regression Intent

For regression, the user wants to understand contributions to a continuous prediction:

- prediction magnitude
- signed contribution direction and size
- residual and error context
- over-prediction and under-prediction drivers
- decomposition from baseline to prediction
- drift in prediction-surface behavior over groups and time

### Binary Classification Intent

For binary classification, the user wants to understand contributions toward the positive class:

- positive-class probability, margin, or link-scale prediction
- threshold behavior
- class-specific contribution patterns
- true-positive, true-negative, false-positive, and false-negative drivers
- near-threshold observations
- decomposition toward or away from the positive class

### Non-Intent

The user is not asking for a fixed list of SHAP plots. The user is asking for a complete, reusable, and trustworthy understanding of prediction behavior.

## 2. Public API

### Core API Proposal

The long-term public API should expose the general framework:

```r
result <- PredictionSurfaceAnalysis(
  data,
  target = NULL,
  prediction = NULL,
  contributions = NULL,
  problem_type = c("regression", "binary"),
  id = NULL,
  time = NULL,
  group = NULL,
  features = NULL,
  reference_data = NULL,
  method = "shap",
  ...
)
```

Parameter meanings:

- `data`: scored data containing targets, predictions, features, and contribution columns or backend inputs.
- `target`: observed outcome column.
- `prediction`: prediction column.
- `contributions`: contribution matrix, contribution column names, prefix, or backend-specific contribution source.
- `problem_type`: prediction problem type.
- `id`: optional row/entity identifier columns.
- `time`: optional date/time column used for time and drift lenses.
- `group`: optional grouping columns used as analysis lenses.
- `features`: optional source feature columns; when `NULL`, infer from contribution metadata.
- `reference_data`: optional baseline/reference data for drift and stability comparisons.
- `method`: contribution backend, initially `"shap"`.
- `...`: advanced analytical controls owned by the generator.

### Convenience Wrappers

SHAP wrappers remain preferred copy/paste entry points for the first backend:

```r
reg_result <- RegressionShapAnalysis(
  data = scored_data,
  target = "Target",
  prediction = "Predict",
  contributions = "Shap_",
  id = NULL,
  time = NULL,
  group = NULL,
  features = NULL,
  reference_data = NULL
)
```

```r
bin_result <- BinaryClassificationShapAnalysis(
  data = scored_data,
  target = "Target",
  prediction = "Predict",
  contributions = "Shap_",
  predicted_class = "PredictedClass",
  positive_class = "Yes",
  threshold = 0.5,
  prediction_scale = "probability",
  id = NULL,
  time = NULL,
  group = NULL,
  features = NULL,
  reference_data = NULL
)
```

### Report API

Reports render existing artifact results:

```r
RegressionShapAnalysisReport(
  artifact_result = reg_result,
  OutputPath = "reports",
  OutputFile = "regression_shap.html"
)
```

Reports may call the generator as a convenience wrapper:

```r
RegressionShapAnalysisReport(
  data = scored_data,
  OutputPath = "reports",
  OutputFile = "regression_shap.html",
  target = "Target",
  prediction = "Predict",
  contributions = "Shap_",
  time = "Date",
  group = "Segment"
)
```

### API Rules

- Use flat, explicit parameters for common workflows.
- Keep analytical parameters in generators.
- Keep rendering parameters in reports.
- Do not expose one public helper per plot, table, or diagnostic.
- Do not expose backend implementation details unless the user must decide.
- Keep legacy names such as `target_col`, `prediction_col`, `DateVar`, and `ByVars` as internal mappings during transition.
- SHAP-specific wrappers must call the general contribution framework where possible.
- New contribution backends must fit `PredictionSurfaceAnalysis()` without API sprawl.

## 3. Artifact Schema

### Top-Level Result

The generator returns one structured result:

```r
list(
  status = "success",
  analysis_type = "prediction_surface",
  method = "shap",
  problem_type = "regression",
  metadata = list(),
  inputs = list(),
  schemas = list(),
  tables = list(),
  plots = list(),
  texts = list(),
  findings = list(),
  judgments = list(),
  quality_gates = list(),
  theory_layer = list(),
  empirical_layer = list(),
  artifacts = list(),
  display_plan = list(),
  report_plans = list(),
  computation_graph = list(),
  cache = list(),
  warnings = character(),
  code = character()
)
```

### Required Top-Level Metadata

`metadata` must include:

- `analysis_id`
- `analysis_type`
- `method`
- `backend`
- `problem_type`
- `generated_at`
- `source_package`
- `source_function`
- `data_name`
- `model_name`
- `target`
- `prediction`
- `prediction_scale`
- `contributions_source`
- `positive_class` for binary
- `threshold` for binary
- `id`
- `time`
- `group`
- `features`
- `reference_data_name`
- `row_count`
- `feature_count`
- `contribution_count`
- `artifact_count`
- `table_count`
- `plot_count`
- `finding_count`
- `judgment_count`
- `quality_gate_count`

### Required Reusable Schemas

The following objects are required first-class reusable artifacts. Reports, dashboards, APIs, Shiny apps, and agents must consume these objects instead of reverse-engineering plots.

#### `contribution_matrix`

Authoritative row-feature contribution object.

Required fields:

- `row_id`
- `feature_id`
- `feature_name`
- `contribution_value`
- `contribution_abs`
- `contribution_sign`
- `prediction`
- `target` when available
- `baseline_value` when available
- `prediction_scale`
- `method`
- `backend`
- `problem_type`
- `data_split` when available
- `time_value` when available
- `group_values` when available

Rules:

- One row per observation-feature contribution.
- Sparse representations are allowed but must expose the same logical schema.
- This is the source for global importance, local explanations, grouped effects, time effects, and drift.

#### `feature_semantics`

Canonical mapping between model contribution columns and source features.

Required fields:

- `feature_id`
- `raw_feature`
- `model_feature`
- `contribution_column`
- `feature_type`
- `semantic_type`
- `source_role`
- `transformation`
- `encoding`
- `level`
- `bin_id`
- `bin_lower`
- `bin_upper`
- `bin_label`
- `grouped_level`
- `parent_feature`
- `display_label`
- `order`

Must handle:

- raw features
- engineered features
- one-hot encoded features
- grouped categorical levels
- binned numeric values
- transformed variables
- interaction features
- model-only features with no raw source column

Rules:

- Numeric bins must be ordered by numeric bounds, not alphabetically.
- One-hot or encoded levels must map back to the source feature.
- Grouped categorical levels must preserve original level membership.
- Plots and tables must use `display_label` and `order` from this object where available.

#### `baseline_reference`

Contract for reconstructing predictions from contributions.

Required fields:

- `baseline_value`
- `baseline_scale`
- `prediction_scale`
- `link_function`
- `inverse_link_function`
- `class_reference` for classification
- `reference_population`
- `reference_data_name`
- `method`
- `backend`
- `available`
- `limitations`

Rules:

- On additive scale:
  `baseline_value + sum(contributions) = prediction`
- For link-scale models:
  `inverse_link(baseline_value + sum(contributions)) = prediction`
- If exact reconstruction is impossible, the result must include a quality gate explaining why.
- Binary reports must state whether contributions are on probability, margin, logit, or another scale.

#### `analysis_lens`

Reusable slicing lens for grouped/time/drift/error/prediction-band analyses.

Required fields:

- `lens_id`
- `lens_type`
- `source_column`
- `level`
- `label`
- `filter_expression`
- `n_rows`
- `coverage`
- `min_time`
- `max_time`
- `is_reference`
- `is_current`
- `sparse`
- `warnings`

Standard lens types:

- `global`
- `group`
- `time`
- `reference_current`
- `prediction_band`
- `error_band`
- `outcome_group`
- `threshold_band`

Rules:

- Lenses must not contaminate feature axes.
- A feature-dependence plot for feature A must use A values or A bins on the feature axis.
- Group/time/outcome lenses may filter, facet, color, or summarize only when explicitly requested by the artifact role.

#### `computation_graph`

Directed dependency graph for computed quantities and artifacts.

Required fields:

- `node_id`
- `node_type`
- `artifact_id`
- `depends_on`
- `cache_key`
- `compute_function`
- `input_fingerprint`
- `parameter_fingerprint`
- `created_at`
- `reused`
- `status`
- `duration_seconds`

Rules:

- Reports must never recompute graph nodes.
- Plots must depend on authoritative tables.
- Text summaries and findings must depend on authoritative tables and quality gates.
- Cache keys must include data fingerprint, method, problem type, feature semantics version, lens definition, and relevant analytical parameters.

#### `finding`

Structured analytical insight.

Required fields:

- `id`
- `type`
- `severity`
- `confidence`
- `title`
- `interpretation`
- `evidence_artifact_ids`
- `evidence_table_ids`
- `affected_features`
- `affected_lenses`
- `caveats`
- `recommended_follow_up`
- `created_by`

Standard finding types:

- `global_driver`
- `interaction`
- `nonlinear_effect`
- `local_driver`
- `drift`
- `instability`
- `quality_warning`
- `threshold_behavior`
- `error_driver`
- `interpretation_risk`

Rules:

- Reports should lead with findings.
- LLM agents should consume findings before raw plots.
- Findings must be traceable to evidence artifacts.
- Confidence must reflect data volume, stability, and quality gates.

#### `quality_gate`

Structured validation and trust object.

Required fields:

- `gate_id`
- `type`
- `status`
- `severity`
- `message`
- `affected_artifact_ids`
- `affected_features`
- `affected_lenses`
- `diagnostic_value`
- `threshold`
- `recommended_action`

Standard statuses:

- `pass`
- `warning`
- `fail`
- `not_applicable`

Standard gate types:

- `schema`
- `prediction_reconstruction`
- `missing_contributions`
- `invalid_contributions`
- `scale_mismatch`
- `sparse_group`
- `high_cardinality`
- `unstable_importance`
- `unstable_interaction`
- `drift_detected`
- `binary_threshold_mismatch`

#### `display_plan`

Renderer-independent organization plan.

Required fields:

- `plan_id`
- `label`
- `audience`
- `layout_type`
- `sections`
- `artifact_ids`
- `finding_ids`
- `quality_gate_ids`
- `default_open_sections`
- `appendix_artifact_ids`
- `priority_order`

Rules:

- Reports use `display_plan`.
- Dashboards may use `display_plan`.
- AnalyticsShinyApp report plans may adapt `display_plan`.
- `display_plan` references artifacts and findings by ID only.

#### `theory_layer`

Structured assumptions and context used to judge whether empirical patterns are plausible, limited, or misleading.

Required fields:

- `theory_id`
- `problem_context`
- `model_context`
- `prediction_scale_meaning`
- `link_function_meaning`
- `expected_relationships`
- `domain_constraints`
- `non_causal_boundaries`
- `plausible_interpretations`
- `implausible_interpretations`
- `known_risks`
- `assumptions`
- `source`

Known risks should include, where applicable:

- leakage
- sparse groups
- confounding
- collider behavior
- unstable groups
- extrapolation
- target leakage through engineered features
- scale/link-function misunderstanding
- reference/current population mismatch

Rules:

- The theory layer may be supplied by the user, inferred from metadata, or generated from AutoQuant diagnostics.
- Inferred theory must be marked as inferred.
- Domain constraints supplied by users must be preserved and referenced by judgments.
- The theory layer cannot override empirical evidence, but it can reduce confidence, add caveats, or block over-interpretation.
- Causal claims require explicit causal theory plus causal evidence. Contribution evidence alone is insufficient.

#### `empirical_layer`

Structured observed evidence used to support, weaken, or reject analytical judgments.

Required fields:

- `evidence_id`
- `evidence_type`
- `artifact_id`
- `table_id`
- `feature_id`
- `lens_id`
- `statistic`
- `estimate`
- `uncertainty`
- `sample_size`
- `coverage`
- `diagnostic_status`
- `stability_status`
- `counter_evidence_ids`
- `limitations`

Empirical evidence should cover:

- observed contribution patterns
- importance rankings
- dependence behavior
- interactions
- grouped behavior
- time behavior
- drift
- stability
- uncertainty
- diagnostics
- reconstruction checks
- quality gate results

Rules:

- Every empirical evidence record must be traceable to an artifact, table, diagnostic, or quality gate.
- Evidence must preserve uncertainty and limitations.
- Conflicting evidence must be represented, not silently dropped.
- Evidence records are reusable by reports, findings, GenAI summaries, adjudication workflows, and QA.

#### `judgment`

Structured analytical belief derived from theory plus empirical evidence.

Required fields:

- `judgment_id`
- `claim`
- `claim_type`
- `theoretical_basis`
- `empirical_evidence`
- `confidence`
- `severity`
- `importance`
- `caveats`
- `assumptions`
- `falsification_tests`
- `evidence_strength`
- `counter_evidence`
- `escalation_path`
- `recommended_follow_up`
- `status`

Standard claim types:

- `descriptive`
- `predictive`
- `causal`
- `operational`
- `diagnostic`
- `hypothesis`
- `theory_only`
- `insufficient_evidence`

Standard statuses:

- `supported`
- `weakly_supported`
- `conflicted`
- `unsupported`
- `insufficient_evidence`
- `blocked_by_quality_gate`

Rules:

- Do not emit strong findings without empirical support.
- Do not emit strong findings that violate theoretical constraints without caveats.
- Distinguish descriptive, predictive, causal, operational, and diagnostic claims.
- Contribution findings are descriptive or predictive, not causal, unless additional causal evidence is supplied.
- Confidence decreases when evidence conflicts, sample sizes are small, diagnostics fail, instability is high, or assumptions are weak.
- Findings must include counter-evidence when present.
- Every judgment must be traceable to evidence IDs.
- Prefer `insufficient_evidence` over false certainty.
- Reports and summaries should be generated from judgments, not directly from raw plots.

### Standard Artifact Object

Each displayable artifact must have:

```r
list(
  artifact_id = "aq_rshap_global_importance_table",
  artifact_type = "table",
  artifact_role = "global_importance",
  label = "Global Contribution Importance",
  source_module = "prediction_surface_analysis",
  section = "Global Importance",
  order = 10L,
  visible = TRUE,
  status = "ready",
  object = NULL,
  content = NULL,
  config = list(),
  code = NULL,
  metadata = list()
)
```

Required artifact metadata:

- `analysis_id`
- `method`
- `backend`
- `problem_type`
- `artifact_role`
- `feature_id`
- `feature_pair_key`
- `feature_x`
- `feature_y`
- `lens_id`
- `statistic`
- `scale`
- `n_rows_used`
- `n_missing`
- `n_distinct_levels`
- `binning_method`
- `bin_count`
- `quality_gate_ids`
- `finding_ids`

### Artifact IDs

Recommended prefixes:

- regression SHAP convenience wrapper: `aq_rshap_`
- binary SHAP convenience wrapper: `aq_bshap_`
- general prediction surface analysis: `aq_psa_`

Artifact IDs must be deterministic for the same analysis role, feature, lens, and method.

## 4. Computation Plan

### Compute Once, Reuse Everywhere

The generator computes authoritative intermediate objects once. All tables, plots, findings, text, display plans, reports, dashboards, and APIs reuse those objects.

### Required Computation Stages

1. Input resolution
   - resolve target, prediction, contributions, features, id, time, group, reference data
   - compute data fingerprints
   - classify feature types
   - build `feature_semantics`

2. Contribution normalization
   - build `contribution_matrix`
   - validate contribution scale
   - validate row alignment
   - validate feature-contribution mapping
   - resolve sparse or wide contribution formats

3. Baseline and reconstruction
   - build `baseline_reference`
   - compute contribution sums
   - validate reconstruction:
     `baseline + sum(contributions) = prediction`
   - handle link-scale or approximate reconstruction

4. Core global summaries
   - signed mean contribution
   - mean absolute contribution
   - median absolute contribution
   - contribution standard deviation
   - positive/negative contribution share
   - normalized contribution share
   - rank and concentration metrics

5. Feature effects
   - numeric bins
   - categorical levels
   - encoded-level rollups
   - grouped categorical rollups
   - transformed-variable rollups
   - nonlinear and monotonicity summaries

6. Dependence
   - single-feature contribution distributions
   - outlier contribution rows
   - prediction/target context
   - optional lens summaries without axis contamination

7. Interactions
   - canonical unordered feature-pair keys
   - candidate interaction ranking
   - interaction surfaces
   - sparse-cell diagnostics
   - interaction stability
   - exact interaction decomposition only when backend supports it

8. Lenses
   - global lens
   - group lenses
   - time lenses
   - reference/current lenses
   - error or residual lenses for regression
   - outcome and threshold lenses for binary

9. Drift and stability
   - reference vs current contribution drift
   - time-window drift
   - group drift
   - importance rank drift
   - effect shape drift
   - bootstrap/subsample stability where requested

10. Local explanations
    - row-level contribution vectors
    - top positive/negative local drivers
    - local reconstruction
    - local context and nearest-lens summaries

11. Findings and quality gates
    - build quality gates from diagnostics
    - build findings from authoritative tables
    - attach evidence artifact IDs
    - assign severity and confidence

12. Epistemic judgment
    - build theory layer
    - build empirical layer
    - combine evidence and assumptions into structured judgments
    - attach counter-evidence
    - define falsification tests
    - block unsupported causal language
    - flag high-impact low-confidence claims for deep inspection

13. Display plan
    - create executive-first display plan
    - assign appendix artifacts
    - order details by analytical value

### Cache Contract

Each computed stage must define:

- `cache_key`
- `input_fingerprint`
- `parameter_fingerprint`
- `dependency_keys`
- `output_schema_version`

Cache keys must change when:

- data changes
- reference data changes
- contribution backend changes
- feature semantics change
- problem type changes
- prediction scale changes
- binning parameters change
- lens definitions change
- interaction settings change

Reports must use cached/generated artifacts only. Report rendering has no permission to recompute analytical quantities.

## 5. Output Inventory

### Executive Overview

Artifacts:

- executive findings table
- executive judgments table
- quality gate summary
- model/prediction-scale summary
- analysis inventory
- top global drivers
- top interactions
- top drift/stability warnings

### Data and Contribution Diagnostics

Artifacts:

- schema summary
- contribution availability table
- feature semantics table
- baseline reference table
- reconstruction diagnostics
- missing/unused contribution table
- invalid/infinite contribution diagnostics
- high-cardinality and sparse-lens diagnostics

### Global Importance

Artifacts:

- global importance table
- signed contribution ranking
- absolute contribution ranking
- contribution concentration summary
- importance stability table
- global importance plots
- contribution distribution plots

### Feature Effects

Artifacts:

- feature effect table by value/bin/level
- encoded-level rollup table
- numeric bin effect table
- nonlinear effect summary
- monotonicity and threshold summary
- feature effect plots

### Dependence

Artifacts:

- single-feature dependence table
- binned dependence table
- outlier contribution table
- dependence plots

Rules:

- Dependence artifacts are single-feature artifacts.
- Feature A dependence uses A values or A bins and A contributions.
- Lens values can filter/facet/color only when explicitly represented in metadata.

### Interaction Importance

Artifacts:

- interaction ranking table
- interaction surface table
- sparse-cell diagnostic table
- interaction stability table
- interaction heatmaps

Rules:

- Unordered pairs must be deduplicated.
- Heatmap axes must be source feature values/bins/levels.
- Heatmap cell values must state the statistic, such as signed mean contribution.
- Candidate interactions from ordinary contribution columns must not be labeled as exact SHAP interaction values.

### Grouped Effects

Artifacts:

- importance by group
- feature effects by group
- group contrast table
- segment-specific findings
- segment heatmaps and summaries

### Time Effects

Artifacts:

- importance over time
- effect shape over time
- top driver changes over time
- time coverage table
- time drift findings

### Drift and Stability

Artifacts:

- reference/current drift table
- time-window drift table
- group drift table
- bootstrap/subsample stability table
- rank stability table
- drift/stability findings

### Local Explanations

Artifacts:

- selected-row contribution table
- top positive local drivers
- top negative local drivers
- local reconstruction diagnostics
- local decomposition plot

### Regression Context

Artifacts:

- residual-lens contribution table
- over-prediction driver summary
- under-prediction driver summary
- high-error cohort explanations
- residual contribution plots

### Binary Context

Artifacts:

- outcome-group contribution table
- true-positive/true-negative/false-positive/false-negative driver summaries
- threshold-adjacent explanation table
- threshold behavior findings
- probability/link-scale diagnostics

### Interpretation Guidance

Artifacts:

- non-causal interpretation warning
- scale interpretation guidance
- sparse group caveats
- instability caveats
- recommended follow-up table
- theory layer summary
- empirical evidence summary
- judgment table
- counter-evidence table
- falsification test table

## 6. Epistemic Judgment System

### Purpose

AutoQuant artifacts should support justified analytical beliefs, not merely outputs.

The system must:

- compute evidence
- compress evidence into findings
- preserve uncertainty
- test claims
- distinguish claim types
- make smart judgments without overstating what the evidence supports

### Theory Layer

The theory layer captures what should be true, what might be true, and what should not be claimed without additional evidence.

It includes:

- analytical assumptions
- model/problem context
- expected relationships
- scale and link-function meaning
- plausible interpretations
- implausible interpretations
- non-causal boundaries
- domain constraints where supplied
- known risks such as leakage, sparsity, confounding, collider behavior, unstable groups, and extrapolation

Examples:

- A binary probability model must not be interpreted on the same scale as a logit-margin model.
- A grouped effect based on 12 rows is weak evidence even if the effect size is large.
- A contribution pattern is not causal evidence.
- A feature suspected of leakage should not be promoted as an operational driver without caveats.

### Empirical Layer

The empirical layer captures what was observed.

It includes:

- contribution patterns
- importance rankings
- dependence behavior
- interaction evidence
- grouped behavior
- time behavior
- drift
- stability
- uncertainty
- diagnostics
- reconstruction checks
- quality gate results
- evidence IDs

Evidence can support, weaken, contradict, or fail to address a claim.

### Judgment Objects

A judgment is a structured analytical claim with evidence and caveats:

```r
list(
  judgment_id = "judgment_top_driver_spend",
  claim = "Spend is the strongest global driver of predicted revenue.",
  claim_type = "predictive",
  theoretical_basis = list("higher spend is expected to increase sales opportunity"),
  empirical_evidence = c("evidence_global_importance_spend", "evidence_stability_spend"),
  confidence = 0.86,
  severity = "medium",
  importance = "high",
  caveats = c("descriptive/predictive, not causal"),
  assumptions = c("prediction scale is continuous revenue"),
  falsification_tests = c("check rank stability under bootstrap", "compare reference/current periods"),
  evidence_strength = "strong",
  counter_evidence = character(),
  escalation_path = NULL,
  recommended_follow_up = c("inspect grouped effects by channel"),
  status = "supported"
)
```

### Judgment Rules

- Do not emit strong findings without empirical support.
- Do not emit strong findings that violate theoretical constraints without caveats.
- Distinguish descriptive, predictive, causal, operational, and diagnostic claims.
- SHAP or contribution findings are descriptive or predictive, not causal, unless additional causal evidence is supplied.
- Confidence must decrease when evidence conflicts.
- Confidence must decrease when sample sizes are small.
- Confidence must decrease when diagnostics fail.
- Confidence must decrease when instability is high.
- Confidence must decrease when assumptions are weak or inferred.
- Findings must include counter-evidence when present.
- Every judgment must be traceable to evidence IDs.
- The system should prefer `insufficient_evidence` over false certainty.
- Operational recommendations require stronger evidence than descriptive summaries.
- Causal recommendations require explicit causal evidence and must be blocked otherwise.

### GenAI Usage Rules

GenAI may consume judgments but must not invent evidence.

Modes:

- Weighted mode may use strong prior judgments to prioritize summaries and recommendations.
- Exploratory mode should surface competing plausible judgments and unresolved hypotheses.
- Adjudication mode should compare judgments by theory, evidence, counter-evidence, assumptions, and falsification tests.
- Deep inspection should be triggered when a judgment is high-impact but low-confidence.

Rules:

- GenAI summaries must resolve to structured judgments.
- GenAI must preserve caveats and counter-evidence.
- GenAI must not upgrade descriptive/predictive claims into causal claims.
- GenAI must not hide quality gate failures.
- GenAI should ask for or recommend additional evidence when judgments are conflicted or insufficient.

### Judgment Outputs

Standard judgment artifacts:

- executive judgments table
- judgment evidence map
- theory layer summary
- empirical evidence summary
- counter-evidence table
- falsification test table
- high-impact low-confidence claims table
- insufficient-evidence claims table
- GenAI-safe judgment bundle

## 7. Report Rendering

### Report Role

Reports render artifact results for human attention. They do not compute.

### Progressive Report Design

Reports must start with:

1. executive findings
2. executive judgments
3. quality gates
4. most important global effects
5. most important interactions
6. drift and stability warnings

Detailed tables and plots follow after the user understands what matters.

### Recommended Report Order

1. Executive findings
2. Executive judgments
3. Quality gates and interpretation warnings
4. Global importance
5. Interaction importance
6. Drift and stability
7. Feature effects
8. Dependence
9. Grouped effects
10. Time effects
11. Local explanations
12. Prediction decomposition
13. Regression or binary context
14. Appendix

### Rendering Rules

- Use `display_plan` to choose order and visibility.
- Render findings before raw artifact inventories.
- Render judgments before raw artifact inventories.
- Use high-value summary tables before dense plots.
- Move dense details to appendix.
- Make quality gates visible near affected outputs.
- Use AutoPlots display helpers for plot groups.
- Use shared table helpers for filtering, exclusion filtering, and numeric formatting.
- Use artifact metadata for default row/card sizing.
- Reports must accept missing optional sections and show clear empty states.
- Reports must not mutate artifact results.

## 8. QA Contract

### API Invariants

Must verify:

- `PredictionSurfaceAnalysis()` accepts the minimal API.
- SHAP wrappers call the shared contribution framework.
- reports render supplied `artifact_result`.
- reports do not call generators when `artifact_result` is supplied.
- `data + ...` report convenience path calls the matching generator.
- render arguments are not forwarded to analytical generators.
- legacy argument names map to canonical names during transition.

### Schema Invariants

Must verify:

- top-level fields exist.
- required reusable schemas exist.
- `contribution_matrix` has required logical columns.
- `feature_semantics` maps every contribution to a source or model feature.
- `baseline_reference` declares reconstruction availability.
- `analysis_lens` objects are valid and non-overlapping where required.
- `computation_graph` contains dependencies and cache keys.
- every `finding` references evidence artifacts.
- every `judgment` references evidence or is explicitly marked as theory-only or insufficient evidence.
- every `quality_gate` has status, severity, and recommended action.
- `display_plan` references existing artifacts and findings only.

### Analytical Invariants

Must verify:

- prediction reconstruction passes within tolerance or produces a quality gate.
- contribution columns map correctly to features.
- missing contribution columns are reported.
- unused contribution columns are reported.
- numeric bins are ordered by numeric bounds.
- categorical levels do not leak into unrelated feature axes.
- group/time/reference lenses do not contaminate feature-dependence axes.
- unordered interaction pairs are deduplicated.
- interaction heatmap values use the documented statistic.
- binary thresholds and predicted classes are consistent or warned.
- probability/logit/margin scale is explicit.
- high-cardinality and sparse-lens outputs warn correctly.

### Epistemic Judgment QA

Must verify:

- every judgment has empirical evidence or is explicitly marked as `hypothesis`, `theory_only`, or `insufficient_evidence`.
- high-confidence judgments require passing diagnostics.
- causal language is blocked unless causal evidence is explicitly present.
- counter-evidence cannot be silently dropped.
- confidence values are consistent with diagnostics.
- confidence decreases when sample sizes are small.
- confidence decreases when stability diagnostics fail.
- confidence decreases when evidence conflicts.
- summaries resolve to structured judgments.
- GenAI-safe bundles preserve evidence IDs, caveats, and counter-evidence.
- high-impact low-confidence judgments trigger deep inspection.
- unsupported claims are not promoted to executive findings.

### Stability and Drift QA

Must verify:

- bootstrap/subsample stability returns deterministic schema.
- importance stability is computed or marked unavailable.
- interaction stability is computed or marked unavailable.
- grouped stability respects sparse group warnings.
- time stability handles missing periods.
- reference/current drift requires compatible schemas.
- reference/current drift reports feature semantics differences.

### Edge Cases

Must test:

- no target column
- no baseline value
- missing prediction values
- infinite contribution values
- sparse contribution matrix
- high-cardinality categorical features
- one-hot encoded features
- grouped categorical levels
- transformed variables
- many features
- many groups
- small groups
- missing time windows
- binary class imbalance
- predictions outside probability range
- threshold near 0 or 1
- reference data with missing features
- current data with new categorical levels

### Report QA

Must verify:

- Rmd params match wrapper params.
- template lookup works installed and in development.
- artifact_result mode avoids recomputation.
- every section renders or shows a meaningful empty state.
- findings appear before detailed artifacts.
- quality gates appear near affected sections.
- plot/table formatting remains readable.
- report rendering works without optional sections.
- Pandoc absence does not create false package QA failures when wrapper/template checks pass.

## 9. Extension Path

### Contribution Backends

Future methods must plug into the same framework:

- SHAP
- model-native contribution methods
- permutation contribution summaries
- gradient-based contribution methods
- exact interaction SHAP
- approximate interaction methods
- counterfactual sensitivity methods
- future explanation backends

Backend-specific details belong in backend adapters, not public report APIs.

### Future Problem Types

The framework should extend to:

- multiclass classification
- survival models
- forecasting
- uplift / treatment effect models
- ranking models
- anomaly detection

New problem types must reuse:

- contribution matrix
- feature semantics
- baseline/reference contract where applicable
- analysis lenses
- computation graph
- findings
- judgments
- quality gates
- display plan

### Future Lenses

Additional lenses should extend `analysis_lens`, not create new report-specific systems:

- cohort
- geography
- customer segment
- time period
- prediction band
- residual/error band
- actual outcome group
- threshold band
- monitoring window
- model version

### Future Consumers

Artifacts must remain implementation-independent for:

- static HTML reports
- dashboards
- AnalyticsShinyApp
- APIs
- scheduled monitoring
- LLM summaries
- automated report generation
- future analytical modules

### Extension Rules

- Add authoritative tables before plots.
- Derive plots, text, and findings from authoritative tables.
- Add new artifact roles only when they answer a distinct analytical question.
- Prefer adding a lens over creating a separate module.
- Prefer adding a backend adapter over changing public APIs.
- Add QA invariants before exposing new capability.
- Do not add public helper functions for each plot or table.
- Preserve theory, evidence, judgment, and counter-evidence objects when adding new methods.

## Implementation Readiness Checklist

Before implementation starts, the team must define:

- canonical schema versions
- reconstruction tolerance
- default binning rules
- default stability sampling rules
- default reference/current comparison rules
- severity levels for quality gates
- confidence scoring for findings
- confidence scoring for judgments
- claim type taxonomy
- causal language blocking rules
- counter-evidence rules
- GenAI judgment consumption rules
- cache key structure
- display plan defaults
- minimum QA fixtures

## Design Standard

The framework is complete only when it gives an expert analyst enough reusable evidence to understand, trust, question, and communicate model prediction behavior.

It is not complete because it has many plots.

It is not complete because it supports SHAP.

It is complete when additional outputs would provide little incremental understanding, every computed quantity can be reused beyond a single report, and every important claim remains traceable to theory, evidence, caveats, and counter-evidence.
