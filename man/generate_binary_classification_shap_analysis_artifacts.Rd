\name{generate_binary_classification_shap_analysis_artifacts}
\alias{generate_binary_classification_shap_analysis_artifacts}
\title{Generate Binary Classification SHAP Analysis Artifacts}
\usage{
generate_binary_classification_shap_analysis_artifacts(
  data,
  target_col = NULL,
  prediction_col = NULL,
  predicted_class_col = NULL,
  positive_class = NULL,
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
  threshold = 0.5,
  threshold_bands = NULL,
  top_n = 20L,
  max_dependence_rows = 5000L,
  max_segment_levels = 20L,
  max_byvars = 3L,
  include_dependence = TRUE,
  include_segments = TRUE,
  include_time = TRUE,
  include_local = FALSE,
  include_interactions = TRUE,
  include_threshold_context = TRUE,
  include_class_balance = TRUE,
  include_plots = TRUE,
  max_feature_effect_plots = 5L,
  max_dependence_plots = 5L,
  max_segment_plots = 5L,
  max_time_plots = 5L,
  max_local_plots = 5L,
  max_interaction_pairs = 20L,
  max_interaction_surface_plots = 10L,
  numeric_interaction_bins = 5L,
  max_interaction_levels = 12L,
  min_interaction_cell_n = 5L,
  interaction_stat = "mean_abs_shap",
  interaction_score_stat = "weighted_mad",
  collapse_rare_levels = TRUE,
  prediction_scale = c("probability", "logit", "margin", "unknown"),
  plot_top_n = NULL,
  auto_plots_theme = NULL,
  plot_width = NULL,
  plot_height = NULL,
  ...
)
}
\arguments{
\item{data}{A data.table-like object containing precomputed \code{Shap_} columns.}
\item{target_col}{Optional actual class column.}
\item{prediction_col}{Optional predicted positive-class score/probability column. Defaults to \code{Predict} when present.}
\item{predicted_class_col}{Optional predicted class column.}
\item{positive_class}{Positive class label. Required for class-aware interpretation when \code{target_col} is supplied.}
\item{feature_cols}{Optional source feature names used to filter detected SHAP columns.}
\item{shap_prefix}{Prefix for SHAP contribution columns.}
\item{id_cols}{Optional ID/context columns to carry into dependence/local artifacts.}
\item{model_name}{Optional model name for overview metadata.}
\item{data_name}{Optional data name for overview metadata.}
\item{DateVar}{Optional date column for time effects.}
\item{date_aggregation}{One of \code{day}, \code{week}, or \code{month}.}
\item{ByVars}{Optional segment variables for segment effects.}
\item{selected_features}{Optional feature list for effect/dependence/local views.}
\item{local_row_ids}{Optional 1-based row indexes for local explanations.}
\item{threshold}{Probability threshold for threshold-context artifacts.}
\item{threshold_bands}{Optional precomputed threshold band vector.}
\item{top_n}{Number of top features used for display-oriented artifacts.}
\item{max_dependence_rows}{Maximum source rows per selected feature in dependence output.}
\item{max_segment_levels}{Maximum segment levels to keep per ByVar.}
\item{max_byvars}{Maximum number of ByVars to use.}
\item{include_dependence}{Whether to include dependence artifacts.}
\item{include_segments}{Whether to include segment effect artifacts.}
\item{include_time}{Whether to include time effect artifacts.}
\item{include_local}{Whether to include local explanation artifacts.}
\item{include_interactions}{Whether to include binned/leveled SHAP interaction diagnostics from precomputed \code{Shap_} columns.}
\item{include_threshold_context}{Whether to include threshold-context artifacts when prediction scores are available.}
\item{include_class_balance}{Whether to include class/outcome context artifacts when target and positive class are available.}
\item{include_plots}{Whether to include AutoPlots-backed plot artifacts when AutoPlots is installed.}
\item{max_feature_effect_plots}{Maximum number of single-feature effect plots.}
\item{max_dependence_plots}{Maximum number of dependence plots.}
\item{max_segment_plots}{Maximum number of segment plots.}
\item{max_time_plots}{Maximum number of time plots.}
\item{max_local_plots}{Maximum number of local explanation plots.}
\item{max_interaction_pairs}{Maximum candidate interaction pairs to score.}
\item{max_interaction_surface_plots}{Maximum two-way surface heatmap plots to create.}
\item{numeric_interaction_bins}{Maximum numeric bins for interaction diagnostics.}
\item{max_interaction_levels}{Maximum categorical/date levels for interaction diagnostics.}
\item{min_interaction_cell_n}{Minimum cell count before an interaction surface cell is flagged sparse.}
\item{interaction_stat}{Statistic for interaction surface scoring. One of \code{mean_abs_shap} or \code{mean_shap}.}
\item{interaction_score_stat}{Label for the interaction score method.}
\item{collapse_rare_levels}{Collapse rare/high-cardinality levels to \code{other}.}
\item{prediction_scale}{One of \code{probability}, \code{logit}, \code{margin}, or \code{unknown}.}
\item{plot_top_n}{Optional top feature count for plot artifacts. Defaults to \code{top_n}.}
\item{auto_plots_theme}{Optional AutoPlots theme.}
\item{plot_width}{Optional AutoPlots plot width.}
\item{plot_height}{Optional AutoPlots plot height.}
\item{...}{Ignored compatibility arguments. SHAP computation is not performed here.}
}
\description{
Summarizes precomputed row-level binary classification SHAP columns into
structured text, table, and AutoPlots-backed plot artifacts. Columns whose
names start with \code{shap_prefix} are mapped back to source features by
stripping the prefix.
}
\details{
This function does not compute SHAP values, call \code{predict()}, require a
model object, or use a SHAP backend package. AutoQuant modeling/scoring
functions are responsible for producing SHAP values upstream when requested.

\code{positive_class} and \code{prediction_scale} make the binary
interpretation explicit. Probability threshold-context artifacts are created
when compatible prediction scores are supplied. If probability values are not
mostly in \eqn{[0, 1]}, the generator warns and uses cautious unknown-scale
metadata.

Interaction diagnostics are candidate interaction diagnostics derived from
ordinary precomputed \code{Shap_} columns. Numeric variables are binned,
categorical/date variables use bounded levels, and two-way surfaces aggregate
signed mean SHAP over actual source feature level/bin combinations. These are
not exact pairwise SHAP interaction values. Multiclass SHAP is deferred.
}
\value{
A structured list with \code{artifacts}, \code{metadata}, \code{warnings},
\code{diagnostics}, and \code{code}.
}
\examples{
set.seed(123)
n <- 50
x1 <- runif(n)
p <- plogis(-0.5 + x1)
dt <- data.table::data.table(
  Target = ifelse(runif(n) < p, "Yes", "No"),
  Predict = p,
  x1 = x1,
  Shap_x1 = rnorm(n, 0, 0.1)
)
artifacts <- generate_binary_classification_shap_analysis_artifacts(
  data = dt,
  target_col = "Target",
  prediction_col = "Predict",
  positive_class = "Yes",
  prediction_scale = "probability",
  top_n = 1,
  include_plots = FALSE
)
}
