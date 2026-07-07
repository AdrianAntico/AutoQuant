# AutoQuant typed artifact schema example

library(data.table)
library(AutoQuant)

importance <- data.table(
  feature = c("Spend", "Clicks", "Channel"),
  mean_abs_contribution = c(0.42, 0.18, 0.11)
)

table_artifact <- new_table_artifact(
  id = "tbl_global_importance",
  title = "Global Importance",
  data = importance,
  description = "Authoritative global importance table.",
  tags = c("importance", "table"),
  source_generator = "example_generator"
)

plot_artifact <- new_plot_artifact(
  id = "plt_global_importance",
  title = "Global Importance Plot",
  object = NULL,
  description = "Renderer-specific plot object would be stored here.",
  tags = c("importance", "plot"),
  dependencies = "tbl_global_importance",
  source_generator = "example_generator"
)

finding_artifact <- new_finding_artifact(
  id = "finding_top_driver",
  title = "Top Driver",
  claim = "Spend is the strongest driver in this example result.",
  evidence_ids = "tbl_global_importance",
  confidence = 0.8,
  caveats = "Example fixture only.",
  recommended_follow_up = "Inspect feature effects for Spend.",
  source_generator = "example_generator"
)

quality_gate <- new_quality_gate_artifact(
  id = "gate_schema",
  title = "Schema Gate",
  gate_type = "schema",
  status = "pass",
  severity = "info",
  message = "The example artifact schema is valid.",
  source_generator = "example_generator"
)

display_plan <- new_display_plan_artifact(
  id = "display_plan",
  title = "Example Display Plan",
  sections = list(
    list(
      section_id = "overview",
      title = "Overview",
      artifact_ids = c("finding_top_driver", "tbl_global_importance", "plt_global_importance")
    )
  ),
  artifact_ids = c("finding_top_driver", "tbl_global_importance", "plt_global_importance"),
  source_generator = "example_generator"
)

artifacts <- list(
  table_artifact,
  plot_artifact,
  finding_artifact,
  quality_gate,
  display_plan
)

validate_artifact_collection(artifacts)
