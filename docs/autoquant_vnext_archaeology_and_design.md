# AutoQuant vNext Archaeology And Design

Status: Phase 1 architecture/design complete. Phase 2 implemented the first narrow vertical slice: CatBoost regression model specs, deterministic fit, prediction artifacts, regression assessment artifacts, and installed-package QA. Phase 3 extends the same contract to CatBoost binary classification with threshold policy, probability prediction, confusion matrix, binary metrics, calibration summaries, and installed-package QA. Phase 4 implements the canonical scoring lifecycle with `aq_scoring_spec()`, `aq_score_model()`, `aq_apply_threshold_policy()`, `aq_attach_outcomes()`, `aq_assess_scoring()`, and `aq_monitor_scoring()` for regression and binary CatBoost fit artifacts. Phase 5 integrates Rodeo fitted transformation replay so raw training and scoring data can share the same deterministic prepared-feature contract. Phase 6 adds portable model bundles through `aq_save_model_bundle()`, `aq_load_model_bundle()`, and `aq_validate_model_bundle()`. Phase 7 adds the canonical analytical artifact envelope, relationship extraction, supported-action inspection, and deterministic artifact validation through `aq_artifact_envelope()`, `aq_artifact_relationships()`, `aq_supported_actions()`, and `aq_validate_artifact()`. Phase 8 adds the time-series forecasting foundation through `aq_forecast_spec()`, `aq_validate_forecast_spec()`, `aq_forecast_partition()`, `aq_fit_forecast()`, `aq_assess_forecast()`, and `aq_rolling_origin_forecast()`. Phase 9 adds ETS and ARIMA statistical forecasting engines through the same forecasting contract without introducing new artifact or assessment families. Phase 10 adds prediction interval evidence, interval assessment, and known-future-regressor validation/ARIMA xreg support to the same forecast artifact contract. Phase 11 adds CatBoost supervised forecasting with direct and recursive strategies, prepared feature lineage, feature manifests, known-future-regressor features, feature importance metadata, and challenger comparisons against naive, seasonal naive, ETS, and ARIMA where possible. See `docs/vnext_catboost_regression.md` and `docs/vnext_forecasting_foundation.md` for the implemented API surface.

This document treats the restored supervised learning, scoring, panel forecasting, time-series forecasting, helper, report, and artifact code as historical evidence. It is not a specification for re-creating the old architecture.

## Phase 18 Funnel Forecasting Note

Phase 18 establishes Funnel forecasting as a first-class vNext forecasting
family. The restored historical Funnel implementations are treated as
archaeological evidence: they show that funnels combine base volumes,
conversion rates, cohort periods, calendar dates, cohort dates, maturity, and
recursive downstream outcome forecasting. The modern implementation does not
preserve the historical CARMA wrapper shape.

Implemented:

- `aq_funnel_forecast_spec()`
- `aq_validate_funnel_forecast_spec()`
- `aq_fit_funnel_forecast()`
- `aq_assess_funnel_forecast()`
- `aq_compare_funnel_strategies()`
- `qa_vnext_funnel_forecasting_foundation()`
- explicit ordered stages and adjacent transitions
- deterministic stage forecasting
- deterministic transition forecasting
- maturity evidence
- stage, transition, horizon, aggregate outcome, and conversion assessment
- canonical funnel artifacts and strategy comparison artifacts

Modern placement:

```text
Forecasting
-> Funnel
   -> stage volume evidence
   -> transition conversion evidence
   -> maturity evidence
   -> assessment
   -> strategy comparison
```

Retired assumptions:

- giant engine-specific Funnel wrappers as the primary public API
- mandatory grid tuning
- direct file-writing side effects
- hidden transition inference
- automatic strategy selection
- deployment or optimization concerns inside the forecasting foundation

The important architectural outcome is that forecasting a process is now
represented differently from forecasting a variable while still using the same
canonical evidence philosophy.

The purpose is to extract analytical capabilities, preserve hard-won modeling lessons, discard accidental implementation shape, and define the system AutoQuant should become.

## Phase 2 And Phase 3 Implementation Note

The first implemented vNext operator paths are intentionally narrow:

```text
aq_model_spec()
-> optional Rodeo transformation spec
-> aq_fit_model()
-> fitted Rodeo transformation replay
-> aq_save_model_bundle()
-> aq_load_model_bundle()
-> aq_artifact_envelope()
-> aq_artifact_relationships()
-> aq_supported_actions()
-> aq_validate_artifact()
-> aq_predict_model()
-> aq_assess_model()
-> aq_score_model()
-> aq_attach_outcomes()
-> aq_assess_scoring()
-> aq_monitor_scoring()
```

Scope delivered:

- CatBoost regression only
- CatBoost binary classification only
- `aq_partition_spec()` for random and time partitions
- `aq_threshold_policy()` for explicit binary decision thresholds
- `aq_validate_model_spec()` for deterministic contract validation
- typed fit, prediction, and assessment artifacts
- comparison-ready regression and binary assessment outputs
- binary probability outputs, predicted classes, confusion matrix, probability diagnostics, and calibration summaries
- optional Rodeo transformation fit/replay lineage for training and scoring
- portable model bundle save/load/validate for reproducible inference
- canonical analytical artifact envelopes for vNext fit, prediction, scoring, outcome attachment, assessment, monitoring, and bundle artifacts
- deterministic artifact relationships through parent artifact ids
- normalized supported downstream actions for app, campaign, and future-operator consumers
- AnalyticsShinyApp-consumable artifact payload shapes
- `qa_vnext_catboost_regression()` and `qa_vnext_catboost_binary()` integrated into `qa_autoquant_package()`
- `qa_vnext_scoring_lifecycle()` integrated into `qa_autoquant_package()`
- `qa_vnext_rodeo_transformation_replay()` integrated into `qa_autoquant_package()`
- `qa_vnext_model_bundle()` integrated into `qa_autoquant_package()`
- `qa_vnext_artifact_framework()` integrated into `qa_autoquant_package()`

Scope deliberately not delivered:

- multiclass
- H2O/XGBoost/LightGBM adapters
- SHAP/contribution generation
- tuning/challenger search
- report rendering
- Rodeo transformation-engine duplication
- replacement of `generate_catboost_builder_artifacts()`

The existing CatBoost Builder remains the transitional AnalyticsShinyApp-facing contract. The vNext functions sit beside it until downstream consumers are ready to migrate.

## Phase 8 Forecasting Foundation Note

The first implemented vNext forecasting path is intentionally limited to deterministic baseline forecasting:

```text
aq_forecast_spec()
-> aq_validate_forecast_spec()
-> aq_forecast_partition()
-> aq_fit_forecast()
-> aq_assess_forecast()
-> aq_rolling_origin_forecast()
```

Scope delivered:

- univariate time-series specification
- date/frequency/horizon/origin concepts
- future-known and future-unknown variable declarations
- deterministic temporal validation
- duplicate timestamp detection
- missing timestamp warnings
- frequency detection and override diagnostics
- explicit forecast partitions
- naive baseline forecast
- seasonal naive baseline forecast
- forecast artifacts with canonical analytical envelopes
- forecast assessment artifacts
- RMSE, MAE, MAPE, SMAPE, bias, and metrics by horizon
- deterministic rolling-origin backtesting
- comparison-ready forecast evidence
- AnalyticsShinyApp-compatible list/table artifact shapes
- `qa_vnext_forecasting_foundation()` integrated into `qa_autoquant_package()`

Scope deliberately not delivered:

- ARIMA
- ETS
- Prophet
- CatBoost forecasting
- GAM forecasting
- panel forecasting
- exogenous-variable modeling
- automatic forecast optimization
- probabilistic intervals
- forecasting bundles
- forecast deployment

The point of Phase 8 is to make forecasting a first-class deterministic operator within the artifact ecosystem. Future engines should extend this contract rather than inventing separate forecast result shapes.

## Phase 9 Statistical Forecasting Engine Note

Phase 9 validates that the forecasting contract can support engine diversity without architectural divergence. ETS and ARIMA are implemented as operators inside `aq_fit_forecast()`:

```text
aq_forecast_spec(engine = "ets" | "arima")
-> aq_validate_forecast_spec()
-> aq_forecast_partition()
-> aq_fit_forecast()
-> aq_assess_forecast()
-> aq_rolling_origin_forecast()
```

The forecasting architecture still owns specification, partitioning, artifacts, assessment, lineage, comparison readiness, and rolling-origin evaluation. Engines own only fitting, forecast generation, and diagnostics.

Implemented:

- ETS via base R `stats::HoltWinters()`
- ARIMA via base R `stats::arima()`
- deterministic engine parameter validation
- engine diagnostics: model form/order, AIC, AICc, BIC, residual diagnostics, convergence, warnings, and training duration
- baseline comparison against naive and seasonal naive where applicable
- rolling-origin compatibility for all four engines

Not implemented:

- automatic ETS/ARIMA model search
- automatic engine selection
- prediction intervals
- exogenous regressors
- model bundles for forecasts
- forecast deployment

The important outcome is that `naive`, `seasonal_naive`, `ets`, and `arima` now feel like forecast operators, not separate systems.

## Phase 10 Forecast Uncertainty and Future Regressor Note

Phase 10 completes the core univariate forecasting evidence model by making uncertainty and known future information first-class.

Implemented:

- prediction interval request and confidence level fields on `aq_forecast_spec()`
- interval availability, method, confidence level, lower/upper interval columns, and unsupported-interval reasons on forecast artifacts
- deterministic interval assessment: coverage, average interval width, and interval counts
- interval metrics by horizon
- future-known-regressor validation for horizon length, schema, missing values, and numeric type compatibility
- ARIMA xreg support using caller-supplied `future_known_variables`
- rolling-origin compatibility for ARIMA with known future regressors

Not implemented:

- fabricated intervals for unsupported engines
- interval optimization
- probabilistic calibration
- Bayesian forecasting
- automatic future feature generation
- panel, funnel, hurdle, or vector forecasting

The important architectural outcome is that forecasts now communicate point expectations, uncertainty availability, uncertainty limitations, and future-information requirements through the same canonical artifact envelope.

## Phase 11 CatBoost Forecasting Note

Phase 11 adds CatBoost as a supervised forecasting engine inside the existing univariate forecast contract.

Implemented:

- `engine = "catboost"` on `aq_forecast_spec()`
- explicit `forecast_strategy = "direct"` and `forecast_strategy = "recursive"`
- deterministic target lag, rolling target mean, calendar/date, and known-future-regressor supervised features
- prepared feature identity, Rodeo transformation identity, CatBoost model identity, and feature manifest metadata
- feature importance metadata where CatBoost exposes it
- unsupported interval metadata rather than fabricated CatBoost prediction intervals
- challenger comparison against naive, seasonal naive when applicable, ETS, and ARIMA where possible
- direct and recursive rolling-origin validation through the existing backtest path

The ownership boundary is deliberately narrow. AutoQuant owns forecast orchestration, supervised framing, CatBoost fitting, assessment, artifacts, and comparison. Rodeo remains the owner of reusable deterministic temporal transformation contracts. The current implementation records Rodeo transformation identity and scope because the installed public Rodeo surface exposes legacy temporal helpers but not yet a complete structured temporal fit/apply contract for this forecasting use case.

Not implemented:

- multi-output CatBoost forecasting
- tuning or AutoML
- CatBoost prediction intervals
- panel/global forecasting
- deployment or monitoring

The important architectural outcome is that supervised machine-learning forecasts now produce the same canonical evidence as statistical forecasts while preserving feature lineage and model identity.

## Phase 12 Rodeo Temporal Transformation Note

Phase 12 moves deterministic temporal feature preparation for supervised
forecasting into Rodeo.

Implemented:

- Rodeo temporal transformation spec, fit, apply, schema validation, supervised
  forecast frame preparation, recursive prediction rows, metadata, and QA
- AutoQuant CatBoost forecasting now consumes Rodeo-prepared temporal frames
  instead of constructing lags, rolling means, calendar features, and
  future-known-variable alignment internally
- CatBoost forecast artifacts now record temporal specification identity,
  temporal transformation identity, prepared temporal dataset identity, replay
  status, temporal diagnostics, and Rodeo feature manifests
- direct and recursive CatBoost forecasting preserve the existing AutoQuant API
  while delegating deterministic temporal preparation to Rodeo

The ownership boundary is now:

- Rodeo owns deterministic temporal preparation and replay.
- AutoQuant owns forecasting orchestration, engine fitting, artifacts,
  assessment, and rolling-origin validation.

Not implemented:

- panel forecasting
- new forecast engines
- tuning or AutoML
- deployment
- a separate forecast bundle subsystem

The important architectural outcome is that temporal feature engineering is no
longer duplicated in AutoQuant. Forecast engines consume prepared temporal data
through a reusable fit/apply contract.

## Phase 13 Global Panel Forecasting Note

Phase 13 establishes global panel forecasting as a specialization of the shared
forecasting architecture.

Implemented:

- `aq_panel_forecast_spec()`
- `aq_validate_panel_forecast_spec()`
- `aq_panel_forecast_partition()`
- `aq_fit_panel_forecast()`
- `aq_assess_panel_forecast()`
- `aq_rolling_origin_panel_forecast()`
- `qa_vnext_panel_forecasting_foundation()`
- Rodeo entity-aware temporal replay enhancements for grouped labels, grouped
  future-variable alignment, entity identity encoding, static entity features,
  and multi-entity future prediction frames
- global pooled CatBoost forecasting for direct and recursive strategies
- panel forecast artifacts with entity identity, temporal transformation
  identity, prepared temporal dataset identity, feature manifest, cold-start
  diagnostics, feature importance, and campaign-compatible supported actions
- panel assessment with aggregate metrics, metrics by entity, metrics by
  horizon, metrics by entity/horizon, and entity summaries

Historical capability archaeology:

- Vector/CARMA forecasting encoded the idea that many related series can share
  temporal features, xregs, rolling moments, and recursive updates.
- Funnel forecasting encoded staged demand/supply or conversion-style outcome
  thinking.
- Hurdle forecasting encoded intermittent-demand and two-stage occurrence plus
  magnitude ideas.
- Hierarchical groups encoded aggregation and entity-context problems that
  future reconciliation work can revisit.

Modern placement:

- Global panel forecasting is the shared architectural home.
- Vector forecasting should become a multivariate/panel specialization.
- Funnel forecasting should become a staged panel outcome specialization.
- Intermittent-demand forecasting should be treated as a problem family.
  Classical Croston and supervised Hurdle forecasting are sibling operators:
  Croston estimates demand size and inter-demand interval directly, while
  Hurdle decomposes occurrence probability and positive-demand magnitude.
- Hierarchical forecasting should become a reconciliation layer above panel
  forecasts, not a separate low-level feature-engineering system.

Not implemented:

- Vector forecasting
- Funnel forecasting
- SBA/TSB intermittent-demand variants were implemented in Phase 17 as
  classical sibling operators to Croston, with method comparison evidence
  across naive, seasonal naive, Croston, SBA, TSB, and optional supervised
  Hurdle.
- top-down, middle-out, and optimization-based reconciliation
- per-series model loops
- Prophet, tuning, AutoML, or deployment

The important architectural outcome is that future forecasting families now
have an obvious home: specializations of global panel forecasting that preserve
the same artifact, assessment, Rodeo replay, and campaign evidence contracts.

## Phase 14 Hierarchical Forecasting Note

Phase 14 promotes hierarchy from a historical option inside older forecasting
wrappers into an explicit deterministic forecasting operator above panel
forecasting.

Implemented:

- `aq_hierarchy_spec()`
- `aq_validate_hierarchy_spec()`
- `aq_reconcile_hierarchical_forecast()`
- `aq_assess_hierarchical_forecast()`
- `aq_rolling_origin_hierarchical_forecast()`
- `qa_vnext_hierarchical_forecasting_foundation()`
- flat hierarchy node tables with entity id, parent id, level, root identity,
  aggregation method, and hierarchy version
- deterministic validation for duplicates, missing parents, multiple roots,
  self-parent relationships, and cycles
- conservative bottom-up reconciliation from panel leaf forecasts
- hierarchy artifacts with parent identity, level identity, reconciliation
  status, reconciliation diagnostics, and campaign-compatible supported actions
- hierarchy assessment with aggregate metrics, metrics by entity, metrics by
  level, metrics by horizon, and aggregate consistency summaries

Historical capability archaeology:

- `AutoCatBoostVectorCARMA()` and related CARMA code contained hierarchy/group
  checks, hierarchical Fourier generation, and grouped recursive scoring
  patterns. These are evidence that related series need explicit structure, but
  they are not a stable modern API.
- Funnel forecasting contained base-measure and conversion-measure ideas that
  naturally depend on group, cohort, and aggregate constraints. Funnel should
  later specialize the panel/hierarchy foundation rather than rebuild its own
  grouping layer.
- Hurdle forecasting contained intermittent-demand occurrence/magnitude logic.
  Hurdle should later specialize the entity outcome process while still using
  the same hierarchy and reconciliation evidence where aggregate constraints
  matter.

Modern placement:

- Single-series forecasting owns specification, partitioning, engine fitting,
  artifact lineage, assessment, and rolling-origin evaluation.
- Panel forecasting adds entity-aware temporal preparation and pooled global
  forecasting.
- Hierarchical forecasting adds deterministic parent-child structure and
  reconciliation above panel forecasts.
- Vector, Funnel, and Hurdle forecasting should become future specialized
  operators inside this structure, not separate structural frameworks.

Not implemented:

- Funnel forecasting
- Hurdle forecasting
- full Vector forecasting
- top-down reconciliation
- middle-out reconciliation
- optimization-based reconciliation
- Prophet, tuning, AutoML, or deployment

## Phase 15 Panel Strategy Selection Note

Phase 15 changes the interpretation of panel forecasting. Pooling is no longer
treated as an architectural preference. It is an analytical hypothesis that
must earn its place through deterministic comparison evidence.

Implemented:

- `aq_panel_strategy_spec()`
- `aq_validate_panel_strategy_spec()`
- `aq_evaluate_panel_strategies()`
- `qa_vnext_panel_strategy_selection()`
- independent strategy evaluation using existing single-series forecast
  contracts
- grouped strategy evaluation using explicit group columns and pooled panel
  forecasts per group
- global strategy evaluation using the existing pooled panel forecast contract
- equal-entity strategy comparison
- negative-transfer diagnostics against independent entity forecasts
- advisory recommendations such as `independent_preferred`,
  `grouped_preferred`, `global_preferred`, `hybrid_worth_investigating`, and
  `evidence_insufficient`

Historical capability archaeology:

- Vector/CARMA forecasting assumed that many series could benefit from shared
  temporal evidence. Phase 15 makes that assumption testable by comparing
  independent, grouped, and global strategies.
- Funnel forecasting often contains natural groups such as cohort, market,
  product family, or channel. Future Funnel work should use explicit grouped
  strategy diagnostics rather than hard-coding one pooled structure.
- Hurdle forecasting may benefit from pooling occurrence behavior while keeping
  some magnitude series separate. Phase 15 preserves this as future hybrid
  evidence instead of forcing global or independent modeling.

Modern placement:

- Independent forecasts establish the no-pooling baseline.
- Grouped forecasts test explicitly supplied business structure.
- Global forecasts test full pooled learning.
- Negative-transfer diagnostics identify entities where pooling appears to
  hurt.
- Recommendations remain advisory and do not automatically select a model.

Not implemented:

- automatic clustering
- automatic strategy selection
- Funnel forecasting
- Hurdle forecasting
- Vector forecasting
- hierarchical optimization
- AutoML or deployment

## Executive Summary

AutoQuant historically compressed an enormous amount of analytical labor into user-facing automation wrappers. Functions such as `AutoCatBoostRegression()`, `AutoCatBoostClassifier()`, `AutoXGBoostRegression()`, `AutoLightGBMRegression()`, `AutoH2oGBMRegression()`, `AutoCatBoostCARMA()`, `AutoBanditSarima()`, and their siblings did not merely fit models. They performed data preparation, partitioning, tuning, model fitting, validation, scoring, back-transformation, metric calculation, plot generation, report generation, file writing, and sometimes model cleanup.

That historical design solved a real problem: analysts needed end-to-end productivity in environments where reproducible modeling pipelines, feature engineering contracts, artifact systems, and app-level orchestration did not yet exist.

The vNext design should not preserve that shape. The core lesson is not "one giant function per algorithm." The core lesson is that AutoQuant has accumulated valuable analytical operators:

- supervised model fitting
- model scoring
- metric computation
- threshold optimization
- prediction surface analysis
- variable importance
- model diagnostics
- hyperparameter exploration
- single-series forecasting
- panel forecasting
- recursive forecast generation
- model comparison
- artifact generation
- report rendering

AutoQuant vNext should expose these as deterministic, typed, composable operators that produce reusable artifacts. Reports, apps, campaigns, and agents should consume those artifacts rather than re-running computation.

The desired direction is:

```text
specification
-> fit / score / forecast
-> typed result artifacts
-> assessment
-> comparison
-> reports and apps consume artifacts
```

The historical direction was often:

```text
one wrapper
-> prepare data
-> tune
-> fit
-> score
-> evaluate
-> plot
-> write files
-> maybe return objects
-> maybe render reports
```

The first path is the future.

## Cross-Repository Impact Plan

The AnalyticsShinyApp cross-repository impact planner was run from R 4.5.2 using the implemented API:

```r
plan <- cross_repo_impact_plan(list(
  summary = "Redesign AutoQuant supervised learning, scoring, panel forecasting, and time-series forecasting APIs and result contracts",
  repositories = c("AutoQuant", "AnalyticsShinyApp", "Rodeo", "AutoPlots"),
  category = "public_api_breaking"
))
```

The generated plan classified the redesign as a `public_api_breaking` change with `cross-system` blast radius.

Planner summary:

| Field | Result |
| --- | --- |
| Change category | `public_api_breaking` |
| Blast radius | `cross-system` |
| Affected repositories | AutoQuant, AnalyticsShinyApp, Rodeo, AutoPlots |
| Implementation order | Rodeo -> AutoPlots -> AutoQuant -> AnalyticsShinyApp |
| Package rebuild order | Rodeo -> AutoPlots -> AutoQuant |
| Required validation | `source('app.R')` -> `git diff --check` -> `AutoQuant::qa_autoquant_package()` -> `Rodeo::qa_rodeo_package()` -> `AutoPlots::qa_autoplots_package()` -> `cross_repo_validate(mode = 'full')` |
| Migration guidance | Update provider contract first, regenerate metadata, rebuild/install provider, update consumers, then run full cross-repo validation. |
| Remaining uncertainty | Static analysis identifies declared contracts only; runtime-only or undocumented dependencies may still exist. |

Capabilities affected:

- `workflow_orchestration` owned by AnalyticsShinyApp
- `artifact_model_and_collector` owned by AnalyticsShinyApp
- `feature_transformations` owned by Rodeo
- `model_preparation` owned by Rodeo
- `analytical_artifact_generation` owned by AutoQuant
- `model_training_and_scoring` owned by AutoQuant
- `visualization_rendering` owned by AutoPlots
- `genai_action_layer` owned by AnalyticsShinyApp
- `campaigns_and_remediation` owned by AnalyticsShinyApp
- `cross_repo_validation` owned by AnalyticsShinyApp

Declared contracts affected:

- `app_to_rodeo_feature_preparation`: Rodeo -> AnalyticsShinyApp
- `app_to_autoquant_artifact_generators`: AutoQuant -> AnalyticsShinyApp
- `app_to_autoplots_rendering`: AutoPlots -> AnalyticsShinyApp

The current declared AutoQuant app-facing contract is still the artifact-generator surface:

- `generate_eda_artifacts()`
- `generate_model_assessment_artifacts()`
- `generate_regression_model_insights_artifacts()`
- `generate_binary_classification_model_insights_artifacts()`
- `generate_regression_shap_analysis_artifacts()`
- `generate_binary_classification_shap_analysis_artifacts()`
- `generate_catboost_builder_artifacts()`

Those functions are the compatibility and migration boundary. vNext internals should be introduced beneath or beside them before AnalyticsShinyApp is asked to consume new contracts directly.

## Archaeological Findings

### What The Historical Code Got Right

The restored implementation contains years of analytical judgment:

- Model families were chosen because they performed well on real business data: CatBoost, XGBoost, LightGBM, H2O, classical time-series models, and CARMA-style panel forecasting.
- Modeling workflows preserved the idea that train/validation/test separation matters.
- Scoring was treated as a first-class operational concern, not an afterthought.
- Feature identity, final modeling columns, factor levels, and transformation metadata were treated as necessary for scoring reproducibility.
- Hyperparameter search used multi-armed-bandit/randomized probability matching concepts to avoid naive exhaustive search.
- Classification utilities included threshold-sensitive metrics, utility/cost weighting, confusion matrices, ROC, lift, gains, and two-threshold utility surfaces.
- Regression utilities included residual plots, calibration/evaluation plots, partial-dependence calibration, back-transformation, and error summaries.
- SHAP and contribution behavior were recognized as separate from ordinary evaluation.
- Panel forecasting treated recursive future feature creation, lag rolling statistics, hierarchy, cohorts, and holdout periods as first-class concerns.
- Single-series forecasting recognized that backtesting, transformation, differencing, Fourier features, and ensemble candidates matter.
- File outputs and reports existed because users needed durable artifacts even before there was a formal artifact model.

These ideas should be preserved.

### What The Historical Code Should Not Define

The historical implementation also reflects constraints that should not shape vNext:

- Giant public functions with dozens or hundreds of parameters.
- One function owning data prep, modeling, scoring, assessment, plotting, report rendering, and persistence.
- Side effects such as writing model objects, plots, CSVs, and PDFs during core computation.
- Public APIs exposing engine-specific parameter surfaces directly.
- Repeated implementations of partitioning, transformation, factor-level handling, and scoring preparation across engines.
- Multiple engines acting as parallel architecture copies rather than adapters.
- Report generation tightly coupled to modeling wrappers.
- Dependency-heavy engines remaining public solely because they existed historically.
- H2O lifecycle management inside modeling functions.
- Feature engineering duplicated in AutoQuant that now belongs in Rodeo.
- Plotting utilities inside AutoQuant that now belong in AutoPlots, except where retained as legacy compatibility or report-specific adapters.

These shapes were useful historically. They are accidental architecture now.

## Capability Inventory

| Capability | Historical implementation | Analytical value | Weakness / debt | Future owner | Future API direction | Classification |
| --- | --- | --- | --- | --- | --- | --- |
| CatBoost supervised regression | `AutoCatBoostRegression()`, `CatBoostHelpers.R`, `generate_catboost_builder_artifacts()` | Strong default tree model, categorical handling, SHAP output, GPU option, feature importance | Giant wrapper, mixed fitting/scoring/plots/files, many raw engine args | AutoQuant | `aq_fit_model()` through CatBoost engine adapter; `generate_catboost_builder_artifacts()` remains transitional app contract | Retain and redesign |
| CatBoost binary classification | `AutoCatBoostClassifier()` | Strong categorical classifier, cost/utility metrics, threshold-sensitive evaluation | Same wrapper debt; classification scale/threshold concerns mixed with fitting | AutoQuant | shared supervised fit + binary assessment + scoring artifacts | Retain and redesign |
| CatBoost multiclass | `AutoCatBoostMultiClass()` | Useful capability, but less integrated with modern artifact-first binary/regression path | No vNext multiclass artifact/report contract yet | AutoQuant | defer until multiclass typed artifacts exist | Redesign later |
| XGBoost supervised learning | `AutoXGBoost*`, `XGBoostHelpers.R` | Valuable alternative engine, mature boosting, SHAP-like validation support | Requires duplicated preprocessing; weaker categorical handling; package dependency burden | AutoQuant engine adapter, with Rodeo prep | optional engine adapter behind shared supervised contract | Retain as adapter candidate |
| LightGBM supervised learning | `AutoLightGBM*`, `LightGBMHelpers.R` | Fast engine, GPU/large-data potential, custom SHAP helpers | Large raw parameter surface; install/runtime complexity | AutoQuant engine adapter | optional engine adapter after CatBoost contract stabilizes | Retain as adapter candidate |
| H2O supervised learning | `AutoH2o*`, `H2OHelpers.R` | Historical breadth: GBM, DRF, GLM, GAM, AutoML, MOJO scoring | Heavy Java/runtime lifecycle, H2O cluster management, less aligned with local deterministic contract | Possibly separate optional adapter package or deferred AutoQuant adapter | not first-class vNext until clear need | Defer / possibly retire |
| Hurdle models | `Auto*HurdleModel*` | Important for zero-inflated/semi-continuous targets | Specialized, likely duplicated engine scaffolding | AutoQuant after core fit/scoring contracts | `aq_fit_hurdle_model()` later, with two-stage artifacts | Redesign later |
| Scoring | `AutoCatBoostScoring()`, `AutoXGBoostScoring()`, `AutoLightGBMScoring()`, `AutoH2OMLScoring()` | Operational scoring, transformation/factor-level reuse, prediction output | Engine-specific scoring wrappers; schema validation not standardized | AutoQuant for scoring; Rodeo for feature transform apply | `aq_score_model(fit, new_data, prepared_spec = NULL)` | Retain and redesign |
| Model metrics | `ModelMetrics.R` | Regression metrics, binary threshold metrics, two-threshold utility surface, confusion matrices | Some functions save files or expect historical object names | AutoQuant | pure metric operators returning typed tables | Retain |
| Model assessment plots | `ModelEvaluationPlots.R` | Calibration, residuals, ROC, lift/gains, PD calibration, SHAP shaping | Plotting and assessment mixed; should derive from tables and use AutoPlots | AutoQuant tables, AutoPlots rendering | assessment artifact generators | Retain analytical ideas, redesign rendering boundary |
| Model insights reports | `ModelInsights.R`, modern report API docs | Report rendering already moving toward artifact-first | Legacy `ModelInsightsReport()` remains broad | AutoQuant for reports, artifacts first | render supplied artifact results; no recomputation | Retain modern path, deprecate legacy shape |
| Prediction surface / SHAP | `generate_*_shap_analysis_artifacts.R`, `shap_autonls_effect_curves.R` | Strong modern contract: contribution analysis, diagnostics, optional AutoNLS effect curves | Transitional names still SHAP-specific | AutoQuant | `PredictionSurfaceAnalysis()` with SHAP wrappers | Retain and evolve |
| CatBoost Builder artifacts | `generate_catboost_builder_artifacts.R` | Current bridge from old CatBoost wrappers to app artifacts/scored outputs | Transitional wrapper over legacy fit functions | AutoQuant | replace internals with vNext supervised operators later | Retain transitional |
| EDA / Model Readiness | `eda_artifact_generation.R`, `target_model_readiness_artifacts.R` | Artifact-first direction, app integration, sidecars | naming and legacy generator internals still imperfect | AutoQuant | keep as artifact producers | Retain |
| Single-series forecasting | `AutomatedTimeSeriesModels.R`, `EconometricsFunctions.R` | Bandit search, transformations, backtesting, Fourier, ARIMA/NNET/TBATS/ETS/ARFIMA | Monolithic outputs; many external deps; not typed artifacts | AutoQuant | `aq_fit_forecast_model()`, `aq_backtest_forecast()`, `aq_forecast()` | Redesign |
| Panel forecasting / CARMA | `Auto*CARMA.R`, `CARMA-HelperFunctions.R` | Rich global/per-series forecasting, lags, recursive features, hierarchy, rolling stats | Feature engineering deeply embedded; engine-specific copies; side effects | AutoQuant plus Rodeo | AutoQuant owns forecast modeling/recursive orchestration; Rodeo owns deterministic lag/calendar/rolling specs | Retain and redesign |
| Funnel / cohort forecasting | `Auto*Funnel.R` | Specialized conversion/cohort dynamics, base measure + conversion measure, cohort/calendar feature families | Highly domain-specific and complex | AutoQuant optional forecasting module; Rodeo feature specs | later specialized panel/cohort contract | Redesign later |
| SQL/system helpers | `SQL_Functions.R`, `SystemFunctions.R` | Historical utility convenience | Not core analytical engine; unrelated dependencies | outside AutoQuant or legacy compatibility | remove from vNext public surface | Retire/deprecate |
| Plot functions | `PlotFunctions.R`, `RmarkdownFunctions.R` | Historical visualization/report helpers | AutoPlots now owns production rendering | AutoPlots, except report adapters | retain only compatibility or move | Deprecate/migrate |
| Causal mediation | `CausalMediation.R` | Potentially valuable causal analysis | Separate domain, not part of core supervised/forecast vNext | future causal module/package | defer until causal architecture exists | Defer |

## H2O Retirement Strategy

H2O should be treated as legacy and undesired by default for vNext. This is a design posture, not an immediate code removal.

Historical H2O value:

- provided broad model family coverage through GBM, DRF, GLM, GAM, AutoML, and ensembles
- supported historical automation when native R model interfaces were less standardized
- offered MOJO-style scoring and clustered runtime behavior
- gave AutoQuant a way to present "many engines" through one package

Why it should not shape vNext:

- Java and cluster lifecycle management are orthogonal to deterministic analytical operators
- local package installation and CI validation become harder
- runtime state is less transparent than ordinary R package model objects
- H2O-specific setup leaks into user workflows
- many H2O use cases can be replaced by CatBoost, XGBoost, LightGBM, glmnet, mgcv, or base statistical models
- preserving H2O because it exists would reproduce the old architecture's engine-first bias

Recommended classification:

| Historical H2O area | Analytical value | Replacement candidate | Recommendation |
| --- | --- | --- | --- |
| H2O GBM / DRF regression and classification | tree-based supervised modeling | CatBoost first, XGBoost/LightGBM second | retire from first-class vNext; compatibility only if real consumer demand exists |
| H2O GLM / GAM | linear and smooth statistical models | `glm`, `glmnet`, `mgcv` | replace with lighter native/statistical engines |
| H2O AutoML | broad challenger search | bounded challenger experiments using explicit engine specs | replace; do not introduce opaque AutoML as core |
| H2O MOJO scoring | portable scoring | vNext score artifact plus explicit model serialization policy | defer unless deployment target requires MOJO |
| H2O hurdle wrappers | zero-inflated/two-stage workflows | CatBoost/glmnet/two-stage AutoQuant hurdle contract | redesign later without H2O dependency |

Smallest human decision required: whether H2O should receive a long-lived compatibility isolation layer or be marked as historical/retired once vNext CatBoost and native statistical alternatives exist.

## Capability Ownership Map

| Capability | AutoQuant | Rodeo | AnalyticsShinyApp | AutoPlots |
| --- | --- | --- | --- | --- |
| Feature transformations | consumes prepared features; declares model feature requirements | owns fit/apply transformation specs, learned preprocessing state, scoring-safe transforms | configures and displays preparation workflow | no |
| Model preparation / partitioning | consumes partitions for model fitting; may request problem-specific splits | owns deterministic split/fold preparation where reusable | owns workflow state and user handoff | no |
| Supervised fitting | owns model specs, engine adapters, fitted model artifacts | no | launches modules, stores artifacts, no fitting internals | no |
| Scoring | owns prediction generation and score artifacts | applies feature transforms needed before scoring | launches scoring, stores results | no |
| Assessment | owns metric tables, diagnostics, quality gates | no | displays and routes artifacts | renders plots from artifact tables |
| Prediction surface / SHAP | owns contribution analysis and typed artifacts | no, except feature lineage context | configures and inspects artifacts | renders plots |
| Forecasting | owns forecasting specs, fits, backtests, forecasts | owns lag/calendar/rolling feature fit/apply specs where deterministic | orchestrates workflow and artifact collection | renders forecast plots |
| Reports | owns package-level report renderers from artifact results | no | owns app report plans / collector | owns chart rendering |
| Campaign integration | produces deterministic candidate outputs and comparison artifacts | executes governed feature experiments | owns campaign orchestration, audit ledger, remediation | no |

Boundary principle:

```text
Rodeo prepares reproducible data.
AutoQuant computes analytical evidence.
AutoPlots renders production visuals.
AnalyticsShinyApp orchestrates projects, workflows, artifacts, campaigns, and delivery.
```

## Supervised Learning Redesign

### Canonical Responsibilities

AutoQuant supervised learning should own:

- problem specification
- engine selection and engine adapter dispatch
- model fit
- model metadata
- hyperparameter search records
- scored train/validation/test outputs
- model assessment artifacts
- model comparison artifacts
- prediction surface hooks when contribution values are produced
- lineage from input data/prepared data to fit and scored outputs

It should not own:

- generic feature engineering
- app state
- app workflow sequencing
- report plan layout
- final project persistence
- AutoPlots internals

### Proposed Public API Shape

The future API should be small and boring:

```r
spec <- aq_model_spec(
  problem_type = "regression",
  engine = "catboost",
  target = "revenue",
  features = c("spend", "clicks", "channel"),
  id = "id",
  time = "event_date",
  weight = NULL
)

fit <- aq_fit_model(
  spec,
  data = prepared_data,
  validation = validation_spec,
  tuning = tuning_spec,
  return_contributions = TRUE
)

scores <- aq_score_model(
  fit,
  new_data = prepared_scoring_data
)

assessment <- aq_assess_model(
  fit = fit,
  scores = scores,
  assessment_spec = aq_assessment_spec()
)

comparison <- aq_compare_models(
  baseline = fit_a,
  challenger = fit_b,
  metric = "rmse"
)
```

Convenience wrappers may exist, but they should assemble these operators rather than hide the lifecycle.

### Supported Model Families

Phase 1 recommendation:

1. CatBoost first-class.
2. Native statistical engines for transparent baselines and interpretable alternatives:
   - `glm` for ordinary regression and classification baselines
   - `glmnet` for regularized linear models
   - `mgcv` for smooth nonlinear effects where justified
3. XGBoost and LightGBM as adapter candidates after the core contract is stable.
4. H2O deferred for compatibility isolation or retirement; it should not be first-class.
5. Hurdle and multiclass deferred until typed result artifacts for those problem types exist.

This is not an argument that H2O, multiclass, or hurdle models are analytically useless. It is an argument that they should not delay the core vNext contract.

### Fit Versus Tuning

The core fit contract should mean: fit one specified model configuration.

Bounded tuning and challenger experiments are separate operators. They should produce search-history artifacts, candidate-result artifacts, and comparison artifacts. They should not be hidden inside `aq_fit_model()` unless the user explicitly supplies a tuning/challenger specification.

Recommended separation:

```r
fit <- aq_fit_model(spec, data, partition)

experiment <- aq_run_model_experiment(
  spec,
  data,
  partition,
  search = aq_tuning_spec(max_trials = 25, strategy = "random"),
  challengers = list(...)
)
```

This distinction matters for reproducibility. A fit artifact says what model was fit. An experiment artifact says what search process was run and why the selected model won.

### Internal Contracts

Supervised learning should define internal, engine-neutral objects:

- `model_spec`
- `engine_spec`
- `validation_spec`
- `tuning_spec`
- `fit_result`
- `score_result`
- `assessment_result`
- `model_comparison_result`

Each object should carry:

- schema version
- source data fingerprint
- feature lineage
- target/prediction metadata
- engine metadata
- warnings
- diagnostics
- artifact IDs

### Result Artifacts

Supervised learning should emit typed artifacts:

- `fit_metadata`
- `model_spec`
- `engine_spec`
- `training_schema`
- `feature_lineage`
- `partition_summary`
- `tuning_search_history`
- `winning_hyperparameters`
- `fit_diagnostics`
- `scored_output`
- `prediction_summary`
- `model_assessment`
- `variable_importance`
- `contribution_output`
- `model_warnings`
- `model_quality_gates`

### Non-Goals

- No AutoML platform.
- No generic workflow engine.
- No hidden report rendering during fit.
- No automatic downstream SHAP/model insights without explicit call.
- No permanent commitment to every historical engine.

### Migration Strategy

The existing `AutoCatBoostRegression()` and `AutoCatBoostClassifier()` can remain as compatibility wrappers while vNext operators emerge. `generate_catboost_builder_artifacts()` should stay the AnalyticsShinyApp-facing transitional contract until it can call vNext operators internally.

The old engine-specific wrappers should eventually become:

- compatibility functions
- examples of parameter mapping
- test fixtures for behavioral parity where useful
- not the architecture

## Scoring Redesign

Historical scoring contains several distinct concerns:

- prediction generation
- classification thresholding
- probability/link-scale metadata
- calibration
- evaluation against outcomes
- monitoring/drift
- schema validation
- transform/factor-level reuse
- comparison against prior scores or baseline models

These should not all live in one scoring function.

### Future Scoring Contracts

```r
score <- aq_score_model(
  fit,
  new_data,
  transform_state = NULL,
  output_type = c("prediction", "probability", "class", "contribution"),
  strict_schema = TRUE
)
```

Scoring returns typed artifacts:

- `score_schema_validation`
- `scoring_input_summary`
- `prediction_output`
- `classification_output`
- `contribution_output`
- `score_lineage`
- `score_warnings`

Assessment remains separate:

```r
assessment <- aq_assess_scores(
  scores,
  target = "actual",
  problem_type = "binary",
  threshold = 0.5
)
```

Monitoring remains separate:

```r
monitoring <- aq_monitor_scores(
  current_scores,
  reference_scores,
  drift_spec = aq_drift_spec()
)
```

### What Belongs In Scoring

- schema validation
- transform-state compatibility check
- model prediction
- prediction scale declaration
- optional contribution generation only when the fit/engine supports it
- scoring lineage

### What Belongs Elsewhere

- calibration: assessment/monitoring
- threshold optimization: assessment
- drift: monitoring
- comparison: comparison layer
- reports: renderers
- feature preparation: Rodeo

### Threshold Policies

Threshold choice should become a first-class artifact, not a hidden numeric argument.

A threshold policy should record:

- policy ID
- model ID
- score artifact ID
- target outcome
- positive class
- threshold value or threshold rule
- objective optimized, if any
- constraints such as minimum precision or maximum false-positive rate
- business utility/cost matrix, if supplied
- validation partition used
- metrics at the selected threshold
- alternate threshold candidates
- warnings and limitations
- downstream decision semantics

This preserves the distinction between a probability model and a decision policy. A binary classifier can be stable while the threshold policy changes because the business cost of decisions changes.

## Time-Series Forecasting Redesign

### Historical Capabilities

The single-series forecasting code includes:

- bandit SARIMA search
- bandit NNETAR search
- TBATS, ETS, ARFIMA search
- transformations and differencing
- Fourier feature generation
- backtesting
- parallel model exploration
- forecast and performance grids
- final rebuild of best available model
- lag/error moving-average plots

### Future Contract

```r
forecast_spec <- aq_forecast_spec(
  target = "value",
  time = "date",
  frequency = "auto",
  horizon = 52,
  engines = c("arima", "ets", "nnetar"),
  transformations = c("none", "log1p", "boxcox"),
  backtesting = aq_backtest_spec(windows = 4)
)

fit <- aq_fit_forecast_model(data, forecast_spec)
backtest <- aq_backtest_forecast(fit)
forecast <- aq_forecast(fit, horizon = 52)
assessment <- aq_assess_forecast(backtest, forecast)
```

### Forecast Artifacts

- `forecast_spec`
- `time_series_schema`
- `transformation_summary`
- `backtest_plan`
- `candidate_model_grid`
- `search_history`
- `selected_model`
- `backtest_predictions`
- `forecast_values`
- `forecast_intervals`
- `forecast_assessment`
- `forecast_diagnostics`
- `forecast_quality_gates`

### Engines

First-class support should begin conservatively:

- ARIMA/SARIMA
- ETS
- possibly NNETAR after contract stabilization

TBATS/ARFIMA can remain adapter candidates. They are valuable but dependency-heavy and should not define the initial vNext shape.

## Panel Forecasting Redesign

Panel forecasting is not simply "time-series with groups." The historical CARMA code recognizes this correctly.

### Historical Capabilities To Preserve

- global models across panels
- group/hierarchy awareness
- time and group partitions
- calendar features
- Fourier features
- lag features
- rolling means, standard deviations, skew, kurtosis, quantiles
- recursive future feature updating
- horizon construction
- forward-looking exogenous data
- hierarchy checks
- per-series evaluation and aggregate evaluation
- funnel/cohort forecasting

### Ownership Split

Rodeo should own deterministic feature transforms:

- calendar features
- lag specs
- rolling statistic specs
- differencing specs
- categorical interaction specs
- fit/apply transform state

AutoQuant should own forecasting:

- panel forecast specification
- recursive forecasting logic
- model fit
- backtesting
- forecast generation
- panel assessment
- forecast comparison
- forecast artifacts

AnalyticsShinyApp should own:

- workflow configuration
- project state
- artifact collection
- campaign orchestration
- user-facing review and approval

### Future Panel API

```r
panel_spec <- aq_panel_forecast_spec(
  target = "sales",
  time = "date",
  group = c("store", "dept"),
  horizon = 12,
  model_strategy = c("global", "per_series", "hybrid"),
  feature_plan = rodeo_feature_plan,
  recursive = TRUE
)

fit <- aq_fit_panel_forecast(data, panel_spec)
backtest <- aq_backtest_panel_forecast(fit)
forecast <- aq_panel_forecast(fit, future_data = xreg_data)
assessment <- aq_assess_panel_forecast(backtest, by = c("group", "horizon"))
```

### Panel Artifacts

- `panel_forecast_spec`
- `panel_schema`
- `hierarchy_summary`
- `feature_plan_reference`
- `recursive_feature_plan`
- `backtest_windows`
- `panel_fit_metadata`
- `forecast_values`
- `forecast_by_group`
- `forecast_by_horizon`
- `panel_assessment`
- `series_failure_diagnostics`
- `hierarchy_quality_gates`
- `forecast_comparison`

## Shared Partition And Resampling Design

Partitioning is a cross-cutting contract. The historical system duplicated partition logic across supervised learning, scoring, forecasting, panel forecasting, and reports. vNext should define partition identity once and let model-family operators consume it.

The partition contract should support:

- random train/validation/test splits
- stratified splits
- grouped splits
- temporal holdouts
- rolling-origin backtests
- panel/entity-aware splits
- cohort-aware splits
- cross-validation folds
- reusable fixed partitions for baseline/challenger comparison

Required fields:

- `partition_id`
- `partition_type`
- `created_at`
- `seed`
- `row_id_col`
- `group_cols`
- `time_col`
- `target_col`
- `leakage_constraints`
- `partition_labels`
- `fold_labels`, when applicable
- `train_rows`
- `validation_rows`
- `test_rows`
- `holdout_start`
- `holdout_end`
- `reuse_semantics`
- `warnings`

Ownership should be shared but explicit:

- Rodeo owns reusable deterministic partition creation when it is part of model preparation or feature experiments.
- AutoQuant consumes partition artifacts and may request task-specific validation checks.
- AnalyticsShinyApp records the selected partition in project state and routes it through artifacts.

AutoQuant should not silently create a new partition during model fit if a project-level partition already exists. Silent repartitioning breaks comparison, campaign learning, and reproducibility.

## Shared Assessment And Comparison Design

Assessment should use a common envelope with task-specific bodies.

Common assessment envelope:

- `assessment_id`
- `assessment_type`
- `task_family`
- `model_id`
- `score_or_forecast_id`
- `dataset_id`
- `partition_id`
- `created_at`
- `metric_summary`
- `primary_metric`
- `primary_metric_direction`
- `warnings`
- `diagnostics`
- `quality_gates`
- `supported_next_actions`

Task-specific bodies:

- regression: residuals, RMSE/MAE/MAPE/R-squared, calibration, subgroup residuals
- binary classification: ROC, PR, lift, gains, confusion matrices, threshold metrics, calibration
- multiclass: class-wise metrics, macro/micro metrics, confusion matrix, calibration where available
- time-series: horizon metrics, rolling-origin metrics, bias, interval coverage, baseline comparison
- panel forecasting: metrics by horizon/entity/exposure/cohort, sparse-history diagnostics, aggregate reconciliation

Comparison should answer:

- what was compared
- whether comparison is fair
- what improved
- what regressed
- whether the difference is practically meaningful
- what evidence remains uncertain
- what actions are supported next

Comparison artifact fields:

- `comparison_id`
- `baseline_artifact_id`
- `challenger_artifact_id`
- `comparison_type`
- `shared_partition_id`
- `metric_deltas`
- `primary_decision`
- `decision_rationale`
- `regressions`
- `improvements`
- `uncertainties`
- `warnings`
- `recommended_next_action`

The app and campaign layer should be able to discover comparison outcomes without reading engine-specific model objects.

## Rodeo Integration Boundaries

Rodeo should absorb or own reusable feature/prep behaviors that historically appeared inside AutoQuant:

- numeric transformations
- learned transformations for scoring
- categorical encoding/rare-level handling
- date/calendar feature extraction
- lag features
- rolling statistics
- differencing and reverse differencing where modeled as reusable transforms
- train/validation/test partition plans
- cross-validation/fold plans
- feature lineage and transform manifests

AutoQuant should request, consume, and preserve Rodeo state. It should not reimplement feature transformation logic when Rodeo can express it.

AutoQuant may still own model-specific derived data when it is not a general-purpose transformation. For example:

- CatBoost Pool construction
- engine-native categorical feature indexes
- prediction contribution matrix assembly
- forecast recursion control flow
- model-specific missingness diagnostics

## AnalyticsShinyApp Integration Boundaries

AnalyticsShinyApp should see AutoQuant as a deterministic analytical engine, not a box of side-effect wrappers.

Preferred app-facing pattern:

```text
user config
-> app validates config
-> AutoQuant generator/operator returns service-like result and typed artifacts
-> app normalizes/submits artifacts to collector
-> reports consume artifacts
-> campaigns reason over artifacts and comparisons
```

AutoQuant should expose:

- deterministic operators
- stable artifact schemas
- package QA
- warnings and diagnostics
- no app-state mutation

AnalyticsShinyApp should not call:

- internal helper files
- engine-specific legacy wrappers directly when artifact generators exist
- raw model fitting functions without an artifact contract

## Result Artifact Philosophy

AutoQuant vNext artifacts should make analytical work durable, inspectable, comparable, and reusable.

Typed artifact families:

| Artifact family | Purpose |
| --- | --- |
| `fit` | fitted model identity, engine, parameters, training schema, lineage |
| `prediction` | row-level predictions, scales, classes, scores, schema validation |
| `score` | scoring run metadata, input validation, prediction distributions |
| `assessment` | metrics, confusion matrices, calibration, residuals, lift/gains |
| `forecast` | forecast values, intervals, horizon metadata, future input lineage |
| `panel_forecast` | group/horizon forecasts, hierarchy diagnostics, recursive feature lineage |
| `comparison` | baseline/challenger comparisons, metric deltas, decision gates |
| `diagnostic` | warnings, assumptions, failures, coverage, sparse groups |
| `model_metadata` | model ID, problem type, engine, target, features, training context |
| `lineage` | source data, prepared data, transform state, model, score, assessment dependencies |

Artifacts should have stable IDs, schema versions, source function, creation time, dependencies, warnings, and diagnostics.

### Typed Result Artifact Contract

For every typed result artifact, the design expectation is:

| Field family | Required content |
| --- | --- |
| Identity | artifact ID, artifact type, schema version, creation timestamp |
| Source | source function/operator, package version, engine version where applicable |
| Inputs | source dataset ID, prepared dataset ID, partition ID, feature manifest ID, transform state ID |
| Task | task family, target, prediction scale, grouping/time fields where applicable |
| Payload | typed result body: model metadata, predictions, metrics, forecasts, diagnostics, or comparison |
| Lineage | dependencies, upstream artifacts, downstream-safe references |
| Quality | warnings, errors, assumptions, validation status, completeness |
| Serialization | whether object is serializable, sidecar path references, object-size policy |
| Consumers | supported downstream operators and render targets |

Serialization must be explicit. Fitted model objects may be too large or unsafe to embed directly in every artifact. vNext should distinguish:

- lightweight metadata artifacts
- row-level table sidecars
- optional model-object references
- exportable model bundles
- app project references

The artifact contract should not require every consumer to load a trained model object to understand what happened.

## Public API Philosophy

The new API should optimize:

- clarity
- flatness
- discoverability
- deterministic execution
- agent operability
- reproducibility
- typed outputs

Rules:

- Prefer a few lifecycle functions over many giant wrappers.
- Prefer explicit specs over raw engine parameter explosions.
- Keep common arguments flat.
- Put advanced engine-specific options behind `engine_options` only after the main contract is stable.
- Do not hide scoring or assessment inside fitting.
- Do not write files unless an explicit export/render function is called.
- Do not render reports during computation.
- Do not recompute artifacts during rendering.
- Preserve compatibility wrappers only as transition aids.

## Model Operator Capability Metadata

AutoQuant should expose simple capability metadata for its operators. This is not a generic plugin framework. It is a discoverability contract for apps, QA, and future agents.

A model operator descriptor should include:

- operator ID
- display name
- task family
- supported problem types
- supported engines
- required inputs
- optional inputs
- supported partition types
- supported metrics
- supported explanations
- scoring capability
- assessment capability
- comparison capability
- forecast capability, where relevant
- known limitations
- optional dependency requirements
- validation function

Example sketch:

```r
aq_model_operator_capabilities()
```

This would allow AnalyticsShinyApp and governed actions to ask what AutoQuant can do before attempting execution.

## Agent Operability

The vNext API should be easy for an agent to invoke safely because the same properties help humans and QA.

Agents should be able to determine:

- available task families
- available engines
- required columns
- supported partitions
- supported metrics
- valid explanation methods
- unsupported combinations
- optional dependency state
- expected artifacts
- valid next actions

This should be achieved through explicit specs, validators, descriptors, and typed artifacts. It should not rely on prompting an LLM to infer arguments from historical function signatures.

Agent-operable does not mean autonomous by default. It means contracts are inspectable enough that a governed execution layer can validate intent before running deterministic operators.

## Migration Strategy

| Historical capability | Recommendation | Reason |
| --- | --- | --- |
| `generate_*_artifacts()` modern generators | retain | Already align with artifact-first architecture. |
| `Regression*Report()`, `Binary*Report()`, `EDAReport()`, `TargetAnalysisReport()` modern wrappers | retain | Already separate generation from rendering. |
| `AutoCatBoostRegression()` / `AutoCatBoostClassifier()` | redesign internally, keep transitional compatibility | High-value engine and app bridge exists. |
| `AutoXGBoost*` / `AutoLightGBM*` | redesign as optional engine adapters | Valuable engines, but not first implementation target. |
| `AutoH2o*` | defer / possibly retire from first-class vNext | Heavy runtime and dependency model. |
| multiclass wrappers | defer until multiclass artifact contract | Avoid repeating old wrapper pattern. |
| hurdle wrappers | defer until two-stage/hurdle artifact contract | Specialized but valuable. |
| scoring wrappers | redesign as shared scoring contract | Operational value is high, but current shape is engine-specific. |
| single-series forecasting | redesign | Strong ideas, old output shape. |
| CARMA / panel forecasting | redesign with Rodeo feature boundary | Very valuable; needs cleaner ownership. |
| funnel/cohort forecasting | redesign later | Valuable but specialized. |
| SQL/system utilities | retire/deprecate from core API | Not part of analytical engine. |
| historical plot helpers | migrate/deprecate in favor of AutoPlots | AutoPlots owns production rendering. |
| report helper internals | keep internal | Needed for rendering, not user API. |

## QA Philosophy

QA must validate contracts, not old implementation quirks.

Required QA families:

- `qa_autoquant_package()` remains the public installed-package QA entry point.
- supervised spec QA
- engine adapter QA
- fit result schema QA
- scoring schema QA
- assessment schema QA
- contribution/prediction surface QA
- forecast spec QA
- backtest QA
- panel forecast QA
- artifact collection QA
- lineage QA
- report rendering contract QA
- AnalyticsShinyApp integration QA
- Rodeo integration QA
- campaign comparison QA

Deterministic fixtures should be small and synthetic. Engine-specific heavy QA should be optional and clearly classified as dependency/runtime-sensitive.

QA should distinguish:

- missing optional engine dependency
- invalid input
- contract failure
- model failure
- artifact validation failure
- report rendering failure
- app integration failure

Warnings are acceptable when structured. Silent failures are not.

## Technical Debt Discovered

- Public API surface is historically too broad.
- Many exported functions are engine-specific wrappers rather than lifecycle contracts.
- Reports, plots, persistence, and computation were historically coupled.
- Feature engineering and model prep are duplicated across engines and should move toward Rodeo.
- H2O requires runtime management that does not fit the local deterministic operator contract cleanly.
- Model scoring is fragmented across engines.
- Forecasting code contains valuable logic but lacks typed artifact contracts.
- Plotting helpers overlap with AutoPlots ownership.
- SQL/system utilities broaden AutoQuant beyond analytical engine responsibilities.
- Legacy function names still reflect `RemixAutoML` history in examples/docs.

## Architectural Lessons Extracted

1. End-to-end productivity matters, but lifecycle stages must now be explicit.
2. Scoring reproducibility requires feature lineage, transform state, model schema, and prediction scale.
3. Hyperparameter search history is evidence, not noise.
4. Threshold choice is a decision artifact, not just a model metric.
5. Forecast recursion is a distinct capability that deserves its own contract.
6. Panel forecasting needs hierarchy, group, and horizon diagnostics.
7. Model fit, scoring, assessment, prediction surface, and report rendering should be separate operators.
8. Reports are consumers, not computational authorities.
9. Feature engineering should be owned once, by Rodeo, then consumed everywhere.
10. Agent operability requires typed artifacts, deterministic contracts, and explicit warnings.

## Recommended Implementation Sequence

1. Define vNext object schemas:
   - `model_spec`
   - `fit_result`
   - `score_result`
   - `assessment_result`
   - `forecast_result`
   - `panel_forecast_result`
   - `comparison_result`

2. Implement minimal CatBoost supervised vertical slice:
   - regression first
   - binary second
   - no report rendering inside fit
   - typed fit/scoring/assessment artifacts

3. Refactor `generate_catboost_builder_artifacts()` to call vNext internals while preserving its current public contract.

4. Implement shared scoring contract:
   - schema validation
   - prediction output
   - prediction scale metadata
   - contribution output when available

5. Implement assessment contract:
   - regression metrics
   - binary metrics
   - threshold diagnostics
   - calibration/lift/gains
   - typed artifacts

6. Integrate Rodeo:
   - consume prepared data and feature lineage
   - preserve transform state references
   - avoid feature engineering duplication

7. Add model comparison artifacts:
   - baseline/challenger comparisons
   - metric deltas
   - quality gates
   - campaign-ready recommendations

8. Redesign single-series forecasting:
   - forecast specs
   - backtest artifacts
   - forecast artifacts

9. Redesign panel forecasting:
   - panel specs
   - recursive feature plan boundary
   - group/horizon assessment

10. Decide engine adapter roadmap:
    - CatBoost first
    - XGBoost/LightGBM second
    - H2O only with explicit runtime decision

11. Deprecation pass:
   - mark old wrappers as compatibility
   - move docs toward vNext operators
   - keep historical examples as migration references, not primary docs

## Vertical Slice Plan

| Slice | Capability delivered | Repositories affected | Contracts introduced | Consumers migrated | QA required | Stopping criterion |
| --- | --- | --- | --- | --- | --- | --- |
| 1. Partition and artifact envelope | shared IDs, lineage, basic result envelope | Rodeo, AutoQuant, AnalyticsShinyApp | partition/result metadata | none initially | package QA, artifact schema QA, cross-repo fast validation | artifacts can reference prepared data and partitions deterministically |
| 2. CatBoost regression | first vNext supervised fit/score/assessment path | AutoQuant, AnalyticsShinyApp, AutoPlots | model spec, fit result, score result, regression assessment | internal CatBoost Builder prototype only | AutoQuant package QA, builder integration QA | old and new evidence can be compared on synthetic regression fixture |
| 3. CatBoost binary | binary fit/score/threshold/assessment | AutoQuant, AnalyticsShinyApp, AutoPlots | threshold policy, binary assessment | CatBoost Builder binary path | binary assessment QA, threshold QA | binary scoring and threshold artifacts are durable |
| 4. Shared scoring | batch prediction as standalone lifecycle | AutoQuant, Rodeo, AnalyticsShinyApp | scoring result, schema diagnostics | scoring workflows | scoring QA, schema mismatch QA | scoring works without hidden refit |
| 5. Assessment and comparison | model evidence and challenger decision artifacts | AutoQuant, AnalyticsShinyApp | assessment envelope, comparison artifact | campaign comparison consumers | comparison QA, campaign integration QA | campaigns can read model improvement evidence |
| 6. Baseline time-series | defensible naive/ETS/ARIMA path | AutoQuant, AutoPlots, AnalyticsShinyApp | forecast spec, backtest, forecast artifact | future time-series module | forecast QA | baseline forecast artifacts exist with rolling-origin evidence |
| 7. Panel forecasting | global panel forecast with Rodeo temporal features | Rodeo, AutoQuant, AnalyticsShinyApp | panel spec, temporal feature contract, panel assessment | future panel module | panel QA, leakage QA | horizon/entity assessment is reliable |
| 8. Engine expansion | XGBoost/LightGBM/glmnet/mgcv adapters | AutoQuant, AnalyticsShinyApp | engine descriptors | selected app/model builder consumers | adapter QA | adapters prove common contract without leaking engine shapes |

This order differs slightly from a pure AutoQuant-first sequence because the impact planner shows Rodeo and AutoPlots should be stable before AnalyticsShinyApp migration. AutoQuant can still prototype internally, but consumer migration should follow provider contract readiness.

## Capabilities To Preserve

- multi-armed-bandit/randomized search ideas
- train/validation/test lifecycle
- model scoring operational metadata
- transformation metadata for scoring
- final modeling column preservation
- variable importance and contribution outputs
- threshold and utility-aware binary evaluation
- regression calibration/residual diagnostics
- lift/gains/ROC/PR style outputs
- partial-dependence calibration concept
- forecasting backtests and performance grids
- CARMA recursive panel feature updating
- group/hierarchy diagnostics
- cohort/funnel forecasting as a later specialized capability

## Capabilities To Retire Or Remove From Core

- SQL/database utilities as core AutoQuant API
- general plotting APIs owned better by AutoPlots
- H2O as mandatory first-class vNext engine
- report rendering as side effect of fit
- file output as side effect of fit
- giant raw engine parameter surfaces as primary user API
- source-package install instructions as part of modern analytical API docs

## Open Questions

- Should H2O remain in AutoQuant, become an optional extension, or be retired from active vNext development?
- Should hurdle models be part of core supervised vNext or a separate specialized module?
- What is the minimum CatBoost vNext slice that can replace `generate_catboost_builder_artifacts()` internals without breaking AnalyticsShinyApp?
- How much of CARMA feature generation should be expressed in Rodeo before panel forecasting vNext begins?
- Should single-series forecasting support classical models first, or should panel/global forecasting take priority because it is more differentiated?
- What artifact schema should represent fitted model objects without forcing unsafe serialization into app/project artifacts?

## Unresolved Decision Register

| Decision | Evidence reviewed | Alternatives | Recommendation | Implementation consequence | Smallest human decision required |
| --- | --- | --- | --- | --- | --- |
| H2O future | historical H2O wrappers, current dependency friction, app contract does not require H2O exports | retire, isolate compatibility, keep first-class | retire from vNext first-class; isolate only if real consumer demand appears | vNext design uses CatBoost/native/statistical engines instead | confirm whether any external user depends on H2O wrappers |
| First non-CatBoost engine | historical XGBoost/LightGBM/H2O breadth, current app CatBoost Builder focus | XGBoost, LightGBM, glmnet, mgcv | add glm/glmnet baseline before another boosting engine; then LightGBM/XGBoost | better baselines and lower dependency risk | choose whether interpretability baselines outrank boosting breadth |
| Forecast priority | historical single-series and CARMA/panel strength | single-series first, panel first | baseline single-series first for contract simplicity, panel second for differentiation | reduces recursion complexity before panel work | confirm whether near-term product value favors panel forecasting |
| Partition ownership | historical duplication, Rodeo model prep ownership, AutoQuant assessment needs | AutoQuant owns, Rodeo owns, shared | Rodeo creates reusable partitions; AutoQuant validates/consumes | avoids duplicate split logic | approve Rodeo as partition source of truth for prepared modeling data |
| Model object serialization | historical file writes, artifact model prefers references | embed model, sidecar model bundle, metadata only | metadata artifact plus optional model bundle reference | avoids bloated artifacts | define acceptable model bundle storage policy |
| Multiclass scope | historical CatBoost multiclass exists but app contracts focus regression/binary | immediate support, defer | defer until typed multiclass assessment exists | avoids partial unsupported artifacts | decide if near-term users require multiclass |
| Hurdle scope | many historical hurdle wrappers restored | core, specialized later | specialized later after two-stage artifact design | avoids wrapper explosion | identify whether zero-inflated targets are near-term critical |
| Prophet support | not central in current restored primary contracts | include, optional, omit | omit unless strong evidence appears | avoids dependency and expectation creep | decide only when forecasting roadmap starts |

## Final Assessment Answers

### 1. Which historical AutoQuant capabilities remain genuinely valuable?

The valuable capabilities are supervised model fitting, scoring, threshold-sensitive binary evaluation, regression diagnostics, variable importance, contribution/SHAP outputs, prediction surface analysis, model comparison, single-series forecasting, panel forecasting, recursive forecast feature generation, backtesting, utility-aware classification, and durable report/artifact generation.

The most valuable historical insight is not any individual wrapper. It is the recognition that analysts need the full lifecycle: prepare, fit, score, assess, explain, compare, preserve, and report.

### 2. Which should be retired?

Core SQL/system utilities, general plotting utilities that overlap AutoPlots, report rendering as a side effect of fitting, file writing as a side effect of model training, giant engine-specific public wrappers as primary APIs, and H2O as a default first-class vNext dependency should be retired or moved to compatibility/historical status.

### 3. Which H2O-dependent behaviors need replacement?

H2O GBM/DRF behavior should be replaced by CatBoost and later XGBoost/LightGBM adapters. H2O GLM/GAM behavior should be replaced by native `glm`, `glmnet`, and `mgcv` where needed. H2O AutoML should be replaced by bounded explicit challenger experiments. H2O MOJO scoring should be deferred until a real deployment target requires it. H2O hurdle behavior should be replaced by a future two-stage/hurdle contract.

### 4. What should the canonical supervised-learning API become?

The canonical API should be spec driven:

```r
spec <- aq_model_spec(...)
fit <- aq_fit_model(spec, data, partition)
scores <- aq_score_model(fit, new_data)
assessment <- aq_assess_model(fit, scores)
comparison <- aq_compare_models(baseline, challenger)
```

Tuning and challenger searches should be explicit experiment operators, not hidden fit behavior.

### 5. What should the canonical scoring API become?

The canonical scoring API should be:

```r
score <- aq_score_model(
  fit,
  new_data,
  output_type = c("prediction", "probability", "class", "contribution"),
  strict_schema = TRUE
)
```

It should return typed scoring artifacts with row identity, model identity, prediction scale, schema validation, transformation compatibility, warnings, and lineage. Threshold policies, outcome attachment, assessment, drift monitoring, and export should remain separate.

### 6. What should the canonical time-series API become?

The canonical time-series API should separate specification, fitting, backtesting, forecasting, and assessment:

```r
forecast_spec <- aq_forecast_spec(...)
fit <- aq_fit_forecast_model(data, forecast_spec)
backtest <- aq_backtest_forecast(fit)
forecast <- aq_forecast(fit, horizon)
assessment <- aq_assess_forecast(backtest, forecast)
```

It must support defensible baselines before advanced engines.

### 7. What should the canonical panel-forecasting API become?

Panel forecasting should use:

```r
panel_spec <- aq_panel_forecast_spec(...)
fit <- aq_fit_panel_forecast(data, panel_spec)
backtest <- aq_backtest_panel_forecast(fit)
forecast <- aq_panel_forecast(fit, future_data)
assessment <- aq_assess_panel_forecast(backtest)
```

It should explicitly model group/entity, horizon, recursive/direct strategy, feature-plan reference, and panel-specific diagnostics.

### 8. What transformations must Rodeo own?

Rodeo should own missing-value handling, categorical handling, scaling/clipping/transforms, learned fit/apply preprocessing state, feature manifests, date features, lags, rolling features, group features, cohort/maturity features, partition/fold creation, and scoring-time transformation compatibility.

### 9. What artifacts should AnalyticsShinyApp consume?

AnalyticsShinyApp should consume finalized typed artifacts: fit metadata, model spec, score results, threshold policies, assessment artifacts, comparison artifacts, forecast artifacts, panel assessment artifacts, diagnostics, warnings, feature manifests, lineage, and report-ready plot/table/narrative artifacts.

It should not consume raw engine-specific internals unless there is a deliberate low-level inspection mode.

### 10. What is the safest high-value first implementation slice?

The safest high-value slice is CatBoost regression vNext behind or beside `generate_catboost_builder_artifacts()`: model spec, deterministic fit, score artifact, regression assessment artifact, and comparison against the legacy builder output on small synthetic fixtures.

This delivers immediate app value while preserving the existing AnalyticsShinyApp boundary.

### 11. What should not be implemented yet?

Do not implement H2O vNext, full multiclass, hurdle models, panel forecasting, generic AutoML, generic plugin systems, generic workflow engines, Prophet support, arbitrary engine registries, or broad report rewrites before the core supervised/scoring/artifact contracts are proven.

### 12. Does the proposed architecture preserve analytical knowledge without preserving accidental historical design?

Yes. The design preserves the analytical lessons: scoring reproducibility, threshold decisions, tuning history, residual diagnostics, contribution evidence, forecast backtesting, panel recursion, and durable reporting. It rejects accidental shapes: giant wrappers, hidden side effects, engine-specific public surfaces, duplicated feature engineering, H2O runtime coupling, and report-time recomputation.

## Completion Criterion For Phase 1

The restored code should now be understood as evidence, not as destiny.

AutoQuant vNext should preserve the analytical intelligence of the historical implementation while moving toward deterministic operators, typed artifacts, explicit lineage, separated rendering, Rodeo-owned preparation, AnalyticsShinyApp-owned orchestration, and campaign-ready comparison.

## Phase 19 Multi-Target Forecasting Note

The restored `AutoCatBoostVectorCARMA()` implementation shows that AutoQuant
historically aimed to forecast multiple related target columns under one shared
temporal workflow. Useful intent recovered from the historical Vector code
includes:

- multiple numeric target columns
- shared date and optional group structure
- shared external regressors
- target-aware temporal feature construction
- target transformations
- multi-output evaluation ideas
- reportable forecast evidence

The modern architectural placement is not the historical Vector wrapper. The
successor is `aq_multitarget_forecast_spec()` plus deterministic independent and
shared-workflow strategies, target artifacts, cross-target evidence, assessment,
and strategy comparison.

Retired assumptions include:

- monolithic CatBoost Vector CARMA wrappers
- CatBoost MultiRMSE as the only valid multi-target path
- hidden temporal feature construction inside AutoQuant
- grid tuning as part of the foundation contract
- automatic target transformation search
- direct report generation from the modeling function
- implicit file writing and side effects

Future specialization opportunities include supervised shared-feature
multi-target forecasting, target relationship diagnostics, target grouping,
VAR/state-space operators, and campaign investigations over target-specific
failures. These should inherit the vNext artifact and evidence contracts rather
than reviving the historical API.

## Phase 20 Cross-Target Feature Forecasting Note

Phase 20 implements the first supervised successor to the historical Vector
intent without reviving the historical Vector API. The recovered idea is that
related targets may contain useful prior information for one another. The modern
implementation treats that as an empirical hypothesis.

Implemented behavior:

- `aq_multitarget_forecast_spec()` now accepts `strategy =
  "cross_target_features"` with `engine = "catboost"`.
- Rodeo owns deterministic cross-target lag, rolling, calendar, and
  known-future feature preparation through
  `Rodeo::rodeo_prepare_cross_target_features()`.
- AutoQuant fits target-specific CatBoost direct horizon models and preserves
  feature importance, preparation identity, leakage diagnostics, and target
  relationship metadata inside canonical artifacts.
- `aq_compare_multitarget_strategies()` compares `independent`,
  `shared_workflow`, and `cross_target_features` and records negative-transfer
  evidence.

This phase intentionally does not implement VAR, VARMAX, multivariate
state-space models, deep learning, target clustering, AutoML, deployment, or
target causality. Cross-target learning remains another source of forecasting
evidence, not a separate forecasting philosophy.
