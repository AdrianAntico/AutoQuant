# AutoQuant vNext usage documentation QA.

#' QA for vNext README Cookbook and Example Scripts
#'
#' Verifies that the vNext README cookbook, documentation links, and copy-paste
#' examples remain aligned with the implemented public API.
#'
#' @return A data.table of QA checks.
#' @export
qa_vnext_usage_documentation <- function() {
  rows <- list()
  add <- function(check, ok, message) {
    rows[[length(rows) + 1L]] <<- data.table::data.table(
      check = check,
      status = if (isTRUE(ok)) "pass" else "fail",
      message = message
    )
  }

  root <- getwd()
  readme_path <- file.path(root, "README.md")
  if (!file.exists(readme_path)) {
    package_readme <- system.file("README.md", package = "AutoQuant")
    if (nzchar(package_readme)) readme_path <- package_readme
  }
  readme <- if (file.exists(readme_path)) paste(readLines(readme_path, warn = FALSE), collapse = "\n") else ""

  required_sections <- c(
    "AutoQuant vNext Cookbook",
    "vNext Quick Start",
    "Supervised Learning",
    "Scoring and Realized Outcomes",
    "Model Bundles",
    "Canonical Analytical Artifacts",
    "Canonical Variable Semantics",
    "Time-Series Forecasting",
    "CatBoost Forecasting",
    "Panel Forecasting",
    "Hierarchical Forecasting",
    "Panel Strategy Comparison",
    "Intermittent-Demand Forecasting",
    "Funnel Forecasting",
    "Multi-Target Forecasting",
    "Cross-Target Feature Forecasting",
    "Forecasting Capability Planning",
    "Forecasting Experiment Campaigns",
    "Package QA",
    "Legacy API Status",
    "Detailed Architecture Documents"
  )
  for (section in required_sections) {
    add(
      paste0("readme_section_", gsub("[^a-z0-9]+", "_", tolower(section))),
      grepl(section, readme, fixed = TRUE),
      paste("README documents:", section)
    )
  }

  implemented_names <- c(
    "aq_model_spec",
    "aq_fit_model",
    "aq_score_model",
    "aq_save_model_bundle",
    "aq_load_model_bundle",
    "aq_variable_semantics",
    "aq_validate_variable_semantics",
    "aq_variable_semantics_artifact",
    "aq_forecast_spec",
    "aq_fit_forecast",
    "aq_panel_forecast_spec",
    "aq_hierarchy_spec",
    "aq_panel_strategy_spec",
    "aq_croston_forecast_spec",
    "aq_sba_forecast_spec",
    "aq_tsb_forecast_spec",
    "aq_compare_intermittent_demand_methods",
    "aq_funnel_forecast_spec",
    "aq_fit_funnel_forecast",
    "aq_assess_funnel_forecast",
    "aq_compare_funnel_strategies",
    "aq_multitarget_forecast_spec",
    "aq_fit_multitarget_forecast",
    "aq_assess_multitarget_forecast",
    "aq_compare_multitarget_strategies",
    "aq_discover_forecasting_capabilities",
    "aq_plan_forecasting_strategy",
    "aq_forecast_experiment_spec",
    "aq_run_forecast_experiment",
    "aq_run_forecast_experiment_campaign",
    "qa_vnext_forecasting_planning",
    "qa_vnext_forecasting_experiment_campaigns",
    "qa_variable_semantics_framework",
    "qa_vnext_multitarget_supervised_forecasting"
  )
  for (fn in implemented_names) {
    add(paste0("readme_mentions_", fn), grepl(fn, readme, fixed = TRUE), paste("README mentions", fn))
  }

  docs_to_check <- c(
    file.path("docs", "variable_semantics_framework.md"),
    file.path("docs", "vnext_forecasting_foundation.md"),
    file.path("docs", "vnext_intermittent_demand_forecasting.md"),
    file.path("docs", "vnext_funnel_forecasting_foundation.md"),
    file.path("docs", "vnext_multitarget_forecasting_foundation.md"),
    file.path("docs", "vnext_cross_target_forecasting.md"),
    file.path("docs", "vnext_forecasting_planning.md"),
    file.path("docs", "vnext_forecasting_experiment_campaigns.md"),
    file.path("docs", "autoquant_vnext_archaeology_and_design.md")
  )
  docs_text <- paste(vapply(docs_to_check[file.exists(docs_to_check)], function(path) {
    paste(readLines(path, warn = FALSE), collapse = "\n")
  }, character(1L)), collapse = "\n")
  add("docs_mention_sba", grepl("SBA", docs_text, fixed = TRUE), "vNext docs mention SBA intermittent-demand support.")
  add("docs_mention_variable_semantics", grepl("aq_variable_semantics", docs_text, fixed = TRUE) && grepl("variable_semantics_artifact", docs_text, fixed = TRUE), "vNext docs mention canonical variable semantics.")
  add("docs_mention_business_intent_semantics", grepl("mission", docs_text, fixed = TRUE) && grepl("business objective", docs_text, fixed = TRUE) && grepl("strategy/tactic alignment", docs_text, fixed = TRUE), "vNext docs mention business-intent semantic alignment.")
  add("docs_mention_lever_risk_semantics", grepl("business_lever", docs_text, fixed = TRUE) && grepl("risk_indicator", docs_text, fixed = TRUE) && grepl("lever_id", docs_text, fixed = TRUE) && grepl("risk_id", docs_text, fixed = TRUE), "vNext docs mention future lever and risk semantic references.")
  add("docs_mention_tsb", grepl("TSB", docs_text, fixed = TRUE), "vNext docs mention TSB intermittent-demand support.")
  add("docs_mention_method_comparison", grepl("aq_compare_intermittent_demand_methods", docs_text, fixed = TRUE), "vNext docs mention method comparison.")
  add("docs_mention_funnel_forecasting", grepl("aq_funnel_forecast_spec", docs_text, fixed = TRUE), "vNext docs mention funnel forecasting.")
  add("docs_mention_multitarget_forecasting", grepl("aq_multitarget_forecast_spec", docs_text, fixed = TRUE), "vNext docs mention multi-target forecasting.")
  add("docs_mention_cross_target_forecasting", grepl("cross_target_features", docs_text, fixed = TRUE), "vNext docs mention cross-target feature forecasting.")
  add("docs_mention_forecasting_planning", grepl("aq_plan_forecasting_strategy", docs_text, fixed = TRUE), "vNext docs mention forecasting planning.")
  add("docs_mention_forecasting_experiment_campaigns", grepl("aq_forecast_experiment_spec", docs_text, fixed = TRUE) && grepl("negative evidence", docs_text, fixed = TRUE), "vNext docs mention governed forecasting experiment campaigns.")
  add("docs_mention_feature_tuning", grepl("feature_tuning", docs_text, fixed = TRUE), "vNext docs classify historical mechanisms as future feature tuning candidates.")
  add("docs_mention_model_tuning_hypotheses", grepl("Evidence-Guided Model Tuning", docs_text, fixed = TRUE) && grepl("frozen-baseline evaluation", docs_text, fixed = TRUE), "vNext docs classify model tuning as evidence-guided challenger hypotheses.")

  example_names <- c(
    "artifact_schema_example.R",
    "variable_semantics_framework.R",
    "vnext_supervised_learning.R",
    "vnext_forecasting.R",
    "vnext_intermittent_demand.R",
    "vnext_panel_hierarchy_strategy.R",
    "vnext_funnel_forecasting.R",
    "vnext_multitarget_forecasting.R",
    "vnext_cross_target_forecasting.R",
    "vnext_forecasting_planning.R",
    "vnext_forecasting_experiment_campaigns.R"
  )
  source_dir <- file.path(root, "inst", "examples")
  package_dir <- system.file("examples", package = "AutoQuant")
  example_dir <- if (dir.exists(source_dir)) source_dir else package_dir

  for (example in example_names) {
    example_path <- file.path(example_dir, example)
    add(paste0("example_exists_", tools::file_path_sans_ext(example)), file.exists(example_path), paste("example exists:", example))
    if (file.exists(example_path)) {
      ok <- TRUE
      msg <- paste("example runs:", example)
      tryCatch(
        {
          sys.source(example_path, envir = new.env(parent = .GlobalEnv))
        },
        error = function(e) {
          ok <<- FALSE
          msg <<- paste("example failed:", example, conditionMessage(e))
        }
      )
      add(paste0("example_runs_", tools::file_path_sans_ext(example)), ok, msg)
    }
  }

  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}
