\name{generate_eda_communication_envelope}
\alias{generate_eda_communication_envelope}
\alias{eda_characterize_dataset}
\alias{eda_applicability_plan}
\alias{generate_target_communication_envelope}
\title{Applicability-driven EDA and target communication envelopes}
\description{
Canonical analytical envelopes for EDA and target-aware analysis.
HTML/RMarkdown is not the analytical contract. Leakage/drift math remains
owned by \code{generate_model_assessment_artifacts}.
}
\usage{
eda_characterize_dataset(data, target_var = NULL, date_var = NULL,
  entity_var = NULL)
eda_applicability_plan(profile, max_pairwise_cols = 30L,
  max_cluster_rows = 4000L)
generate_eda_communication_envelope(data, DataName = NULL, TargetVar = NULL,
  DateVar = NULL, EntityVar = NULL, max_rows = 50000L,
  max_pairwise_cols = 30L)
generate_target_communication_envelope(data, TargetVar, TrendDateVar = NULL,
  TrendGroupVar = NULL, DataName = NULL, RunGAMDiagnostics = FALSE)
}
