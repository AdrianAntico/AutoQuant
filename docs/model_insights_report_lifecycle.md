# ModelInsightsReport Lifecycle

`ModelInsightsReport()` is a legacy compatibility wrapper. It should remain available for older examples and existing user code, but new problem-specific workflows should use artifact-first generators and renderers.

Preferred paths:

- Regression: `generate_regression_model_insights_artifacts()` -> `RegressionModelInsightsReport()`
- Binary classification: `generate_binary_classification_model_insights_artifacts()` -> `BinaryClassificationModelInsightsReport()`

## Reference Audit

| Location | Classification | Rationale / Next Action |
|---|---|---|
| `R/ModelInsights.R` roxygen and function body | keep as legacy compatibility | Runtime compatibility wrapper remains exported for existing user code. Docs label it as legacy and point to problem-specific report functions. |
| `man/ModelInsightsReport.Rd` | keep as legacy compatibility | Generated lifecycle note mirrors `R/ModelInsights.R`. |
| `README.md` early regression training snippets | keep as legacy compatibility | These are older training-report examples tied to full model objects. Modern regression example already uses `generate_regression_model_insights_artifacts()` and `RegressionModelInsightsReport()`. |
| `README.md` older binary classification training snippet | keep as legacy compatibility | Modern binary example now uses `generate_binary_classification_model_insights_artifacts()` and `BinaryClassificationModelInsightsReport()`. |
| `README.md` multiclass example | blocked until replacement exists | There is no multiclass artifact-first model-insights generator/report pair yet. Do not replace until that path exists. |
| `R/AutoCatBoostScoring.R` roxygen example | keep as legacy compatibility | Historical scoring example. Replace only when the surrounding example is rewritten around the artifact-first report path. |
| `man/AutoCatBoostScoring.Rd` | keep as legacy compatibility | Generated from `R/AutoCatBoostScoring.R`; inherits same classification. |
| `AnalyticsShinyApp` adapters | replace now / already replaced | App adapters should never call `ModelInsightsReport()`. They use artifact generators and standard app artifacts; native report renderers are optional export paths only. |

## Rules

- Do not add new `ModelInsightsReport()` examples for regression or binary classification.
- Do not replace multiclass usage until a multiclass artifact generator and renderer exist.
- Do not make app integrations depend on RMarkdown report renderers.
- Keep artifact generators as the source of truth for app ingestion, report plans, and downstream display.
