# Report API Contract

## Principle

Artifact generators own analytical parameters.

Report functions own rendering parameters.

This keeps AutoQuant workflows copy/paste friendly and prevents report wrappers from becoming duplicated analytical APIs.

## Render Targets

Artifact generators and report renderers serve separate render targets.

Human report targets, including R Markdown and HTML reports, must continue to render interactive widgets and existing layouts. LLM collector targets may add PNG sidecars, metadata, captions, and compact context, but must not replace the human-facing widget object.

`ExportPNG = TRUE` means an additional LLM/static representation is requested. It must not change the human report artifact. The expected lifecycle is:

```text
production visualization object
  -> human report: existing interactive widget
  -> LLM collector/sidecar: production screenshot plus metadata
```

Both renderings originate from the same production AutoPlots object.

## Preferred Workflow

Generate artifacts first:

```r
artifact_result <- generate_regression_shap_analysis_artifacts(
  data = scored_data,
  target_col = "Target",
  prediction_col = "Predict",
  DateVar = "Date",
  ByVars = "Segment"
)
```

Render the artifacts:

```r
RegressionShapAnalysisReport(
  artifact_result = artifact_result,
  OutputPath = "reports",
  OutputFile = "regression_shap.html"
)
```

## Convenience Workflow

Report functions may call the matching generator through `data + ...`:

```r
RegressionShapAnalysisReport(
  data = scored_data,
  OutputPath = "reports",
  OutputFile = "regression_shap.html",
  target_col = "Target",
  prediction_col = "Predict",
  DateVar = "Date",
  ByVars = "Segment"
)
```

This is a convenience wrapper. The analytical arguments still belong to the generator.

## Standard Report Signature

Modern report functions should follow this shape:

```r
SomeReport <- function(
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

Report functions may support `SelfContained`, `Clean`, `Envir`, `TemplatePath`, `RmdFile`, and `Package` through `...` when needed by the rendering system.

## Behavior

If `artifact_result` is supplied:

- render it directly
- do not call the artifact generator
- ignore analytical arguments in `...`
- avoid recomputation
- render optional SHAP AutoNLS effect-curve artifacts when they are present

If `artifact_result` is `NULL`:

- require `data` or another generator-supported data input
- call the matching `generate_*_artifacts()` function
- pass analytical arguments through `...`
- render the generated artifacts

Optional SHAP effect-curve controls such as `effect_curve_backend = "autonls"` belong to the SHAP artifact generators. Reports consume `shap_effect_curve_values`, `shap_effect_curve_diagnostics`, and `shap_effect_curve_summary`; they must not refit AutoNLS curves while rendering an existing `artifact_result`.

## Modern Report Wrappers

Current modern wrappers should follow this contract:

- `EDAReport()`
- `TargetAnalysisReport()` during the Model Readiness naming transition
- `RegressionModelInsightsReport()`
- `BinaryClassificationModelInsightsReport()`
- `RegressionShapAnalysisReport()`
- `BinaryClassificationShapAnalysisReport()`

`ModelInsightsReport()` is legacy compatibility only and should not shape new report APIs.

## QA Invariants

Report API QA should verify:

- report signatures use the standard shape
- artifact-result mode reuses supplied artifacts
- `data + ...` mode calls the matching generator
- render arguments are not forwarded to generators
- analytical arguments pass through `...`
- Rmd templates are discoverable
- render params match Rmd YAML declarations
- Pandoc absence does not create false failures when template/wrapper validation passes
