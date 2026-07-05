\name{RegressionShapAnalysisReport}
\alias{RegressionShapAnalysisReport}
\title{Render Regression SHAP Analysis Report}
\usage{
RegressionShapAnalysisReport(
  data,
  output_dir = getwd(),
  output_file = NULL,
  title = "Regression SHAP Analysis Report",
  subtitle = NULL,
  target_col = NULL,
  prediction_col = NULL,
  feature_cols = NULL,
  shap_prefix = "Shap_",
  id_cols = NULL,
  model_name = NULL,
  data_name = NULL,
  DateVar = NULL,
  date_aggregation = "month",
  ByVars = character(),
  selected_features = NULL,
  local_row_ids = integer(),
  top_n = 20L,
  max_dependence_rows = 5000L,
  max_segment_levels = 20L,
  max_byvars = 3L,
  include_dependence = TRUE,
  include_segments = TRUE,
  include_time = TRUE,
  include_local = FALSE,
  include_interactions = FALSE,
  prediction_scale = "response",
  artifact_result = NULL,
  open = FALSE,
  quiet = TRUE,
  self_contained = TRUE,
  RmdFile = "Regression_SHAP_Analysis_Report.Rmd",
  Package = "AutoQuant",
  TemplatePath = NULL,
  Clean = TRUE,
  Envir = NULL,
  ...
)
}
\arguments{
\item{data}{A data.table containing precomputed \code{Shap_} columns. Required when \code{artifact_result} is NULL.}
\item{output_dir}{Directory where the rendered HTML report should be written.}
\item{output_file}{Output HTML filename.}
\item{title}{Report title.}
\item{subtitle}{Optional report subtitle.}
\item{target_col}{Optional target/actual column.}
\item{prediction_col}{Optional prediction column. Defaults to \code{Predict} when present.}
\item{feature_cols}{Optional source features used to filter detected SHAP columns.}
\item{shap_prefix}{Prefix for SHAP contribution columns.}
\item{id_cols}{Optional ID/context columns.}
\item{model_name}{Optional model name.}
\item{data_name}{Optional data name.}
\item{DateVar}{Optional date column.}
\item{date_aggregation}{One of \code{day}, \code{week}, or \code{month}.}
\item{ByVars}{Optional segment variables.}
\item{selected_features}{Optional feature list for effect/dependence/local views.}
\item{local_row_ids}{Optional 1-based row indexes for local explanations.}
\item{top_n}{Number of top features used for display-oriented artifacts.}
\item{max_dependence_rows}{Maximum source rows per selected feature in dependence output.}
\item{max_segment_levels}{Maximum segment levels to keep per ByVar.}
\item{max_byvars}{Maximum number of ByVars to use.}
\item{include_dependence}{Whether to include dependence artifacts.}
\item{include_segments}{Whether to include segment artifacts.}
\item{include_time}{Whether to include time artifacts.}
\item{include_local}{Whether to include local explanation artifacts.}
\item{include_interactions}{Whether to include binned/leveled SHAP interaction diagnostics from precomputed \code{Shap_} columns.}
\item{prediction_scale}{Prediction scale label, usually \code{response}.}
\item{artifact_result}{Optional precomputed result returned by \code{generate_regression_shap_analysis_artifacts()}.}
\item{open}{If TRUE, open the rendered report in a browser.}
\item{quiet}{Passed to \code{rmarkdown::render()}.}
\item{self_contained}{Passed to \code{rmarkdown::render()} through output options.}
\item{RmdFile}{RMarkdown filename located in \code{inst/r-markdowns}.}
\item{Package}{Package name used to locate installed RMarkdown files.}
\item{TemplatePath}{Optional explicit path to the RMarkdown file. If supplied, this takes precedence over the package/system.file lookup.}
\item{Clean}{Passed to \code{rmarkdown::render()}.}
\item{Envir}{Optional render environment. If NULL, a clean child environment of \code{.GlobalEnv} is created.}
\item{...}{Additional arguments passed to \code{generate_regression_shap_analysis_artifacts()}.}
}
\description{
Renders a standalone AutoQuant-native Regression SHAP Analysis HTML report from
structured artifacts. If \code{artifact_result} is not supplied, this function
calls \code{generate_regression_shap_analysis_artifacts()}.
}
\details{
Input data must contain precomputed \code{Shap_} columns. This report function
does not compute SHAP values, call \code{predict()}, require a model object, or
use a SHAP backend package. Binary and multiclass SHAP reports are deferred.
Plot artifacts are AutoPlots-backed and the native report follows the same
artifact renderer visual pattern used by existing AutoQuant reports.
Interaction diagnostics in this report are candidate binned/leveled SHAP
surfaces from ordinary \code{Shap_} columns, not exact pairwise SHAP interaction
values. Exact interaction decomposition requires upstream interaction-specific
outputs.
}
\value{
Invisibly returns the rendered HTML report path.
}
\examples{
set.seed(123)
df <- data.table::data.table(
  y = rnorm(20),
  Predict = rnorm(20),
  x1 = runif(20),
  x2 = rnorm(20),
  Shap_x1 = rnorm(20, 0, 0.2),
  Shap_x2 = rnorm(20, 0, 0.1)
)
path <- RegressionShapAnalysisReport(
  data = df,
  output_dir = tempdir(),
  output_file = "regression_shap_report.html",
  quiet = TRUE
)
}
