\name{generate_regression_shap_analysis_artifacts}
\alias{generate_regression_shap_analysis_artifacts}
\title{Generate Regression SHAP Analysis Artifacts}
\usage{
generate_regression_shap_analysis_artifacts(
  data,
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
  interaction_pairs = list(),
  max_interaction_pairs = 20L,
  max_interaction_surface_plots = 10L,
  numeric_interaction_bins = 5L,
  max_interaction_levels = 12L,
  min_interaction_cell_n = 5L,
  interaction_stat = "mean_abs_shap",
  interaction_score_stat = "weighted_mad",
  collapse_rare_levels = TRUE,
  include_plots = TRUE,
  max_feature_effect_plots = 5L,
  max_dependence_plots = 5L,
  max_segment_plots = 5L,
  max_time_plots = 5L,
  max_local_plots = 5L,
  plot_top_n = NULL,
  auto_plots_theme = NULL,
  plot_width = NULL,
  plot_height = NULL,
  prediction_scale = "response",
  ...
)
}
\arguments{
\item{data}{A data.table containing precomputed SHAP contribution columns.}
\item{target_col}{Optional target/actual column.}
\item{prediction_col}{Optional prediction column. Defaults to \code{Predict} when present.}
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
\item{top_n}{Number of top features used for display-oriented artifacts.}
\item{max_dependence_rows}{Maximum source rows per selected feature in dependence output.}
\item{max_segment_levels}{Maximum segment levels to keep per ByVar.}
\item{max_byvars}{Maximum number of ByVars to use.}
\item{include_dependence}{Whether to include dependence table artifacts.}
\item{include_segments}{Whether to include segment effect table artifacts.}
\item{include_time}{Whether to include time effect table artifacts.}
\item{include_local}{Whether to include local explanation table artifacts.}
\item{include_interactions}{Whether to include binned/leveled SHAP interaction diagnostics from precomputed \code{Shap_} columns.}

\item{interaction_pairs}{Optional list of ordered candidate pairs. Each pair should be \code{c(shap_feature, interaction_feature)}.}

\item{max_interaction_pairs}{Maximum candidate interaction pairs to score.}

\item{max_interaction_surface_plots}{Maximum two-way surface heatmap plots to create.}

\item{numeric_interaction_bins}{Maximum numeric bins for interaction diagnostics.}

\item{max_interaction_levels}{Maximum categorical/date levels for interaction diagnostics.}

\item{min_interaction_cell_n}{Minimum cell count before an interaction surface cell is flagged sparse.}

\item{interaction_stat}{Statistic for interaction surface scoring. One of \code{mean_abs_shap} or \code{mean_shap}.}

\item{interaction_score_stat}{Label for the interaction score method.}

\item{collapse_rare_levels}{Collapse rare/high-cardinality levels to \code{other}.}

\item{include_plots}{Whether to include AutoPlots-backed plot artifacts when AutoPlots is installed.}
\item{max_feature_effect_plots}{Maximum number of single-feature effect plots to create.}
\item{max_dependence_plots}{Maximum number of dependence plots to create.}
\item{max_segment_plots}{Maximum number of segment plots to create.}
\item{max_time_plots}{Maximum number of time plots to create.}
\item{max_local_plots}{Maximum number of local explanation plots to create.}
\item{plot_top_n}{Optional top feature count for plot artifacts. Defaults to \code{top_n}.}
\item{auto_plots_theme}{Optional AutoPlots theme.}
\item{plot_width}{Optional AutoPlots plot width.}
\item{plot_height}{Optional AutoPlots plot height.}
\item{prediction_scale}{Prediction scale label, usually \code{response}.}
\item{...}{Ignored compatibility arguments. SHAP computation is not performed here.}
}
\description{
Summarizes precomputed row-level regression SHAP columns into structured
artifacts. Columns whose names start with \code{shap_prefix} are mapped back
to source features by stripping the prefix, for example
\code{Shap_Impressions} maps to \code{Impressions}.
}
\details{
This function does not compute SHAP values, call \code{predict()}, require a
model object, or use a SHAP backend package. AutoQuant modeling/scoring
functions are responsible for producing SHAP values upstream when requested.
This artifact generator summarizes those existing \code{Shap_} columns into
overview, diagnostic, global importance, categorical/binned-numeric level,
single-feature effect, dependence, segment, time, local, and interaction
diagnostic artifacts.

Interaction diagnostics are candidate interaction diagnostics derived from
ordinary precomputed \code{Shap_} columns. Numeric variables are binned,
categorical variables use bounded levels, and two-way surfaces aggregate SHAP
contributions over actual source feature level/bin combinations. Surface axes
are actual feature value bins/levels and are labeled with the pair variables.
Heatmap values are signed mean SHAP for the attributed feature. Segment heatmaps use signed mean SHAP by feature and
segment. These diagnostics are not exact pairwise SHAP interaction values;
exact interaction decomposition requires upstream interaction-specific outputs.

When \code{include_plots = TRUE} and AutoPlots is installed, plot artifacts are
created with AutoPlots high-level plotting functions. If AutoPlots is not
available, table and text artifacts are still returned and plot creation is
reported as a warning.
}
\value{
A structured list with \code{artifacts}, \code{metadata}, \code{warnings},
\code{diagnostics}, and \code{code}.
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
artifacts <- generate_regression_shap_analysis_artifacts(
  data = df,
  target_col = "y",
  prediction_col = "Predict",
  top_n = 2,
  include_plots = TRUE
)
}
