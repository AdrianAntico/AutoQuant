#' Run AutoQuant installed-package QA
#'
#' @description
#' Runs the stable installed-package QA contract for AutoQuant. This public
#' entry point coordinates installed QA for artifact schemas, SHAP contracts,
#' model insights, and CatBoost artifact generation while allowing selected
#' implementation-specific QA helpers to remain internal.
#'
#' @return A data.table with QA check rows and normalized status values.
#'
#' @examples
#' \dontrun{
#' qa_autoquant_package()
#' }
#'
#' @export
qa_autoquant_package <- function() {
  internal_regression_shap <- get(
    "qa_generate_regression_shap_analysis_artifacts",
    envir = asNamespace("AutoQuant"),
    inherits = FALSE
  )

  qa_results <- list(
    artifact_schema_framework = qa_artifact_schema_framework(),
    shap_interaction_guards = qa_shap_interaction_guards(),
    shap_autonls_backend = qa_shap_autonls_backend(),
    regression_shap_analysis_artifacts = internal_regression_shap(),
    binary_model_insights_artifacts = qa_generate_binary_classification_model_insights_artifacts(),
    catboost_builder_artifacts = qa_generate_catboost_builder_artifacts(),
    vnext_catboost_regression = qa_vnext_catboost_regression(),
    vnext_catboost_binary = qa_vnext_catboost_binary(),
    variable_semantics_framework = qa_variable_semantics_framework(),
    business_intent_framework = qa_business_intent_framework(),
    decision_management_framework = qa_decision_management_framework(),
    decision_lifecycle_framework = qa_decision_lifecycle_framework(),
    decision_valuation_framework = qa_decision_valuation_framework(),
    decision_workflow_framework = qa_decision_workflow_framework(),
    epistemic_integrity_contracts = qa_epistemic_integrity_contracts(),
    causal_intelligence_framework = qa_causal_intelligence_framework(),
    causal_experiment_design_framework = qa_causal_experiment_design_framework(),
    causal_completed_experiment_framework = qa_causal_completed_experiment_framework(),
    causal_randomized_itt_framework = qa_causal_randomized_itt_framework(),
    causal_randomized_design_analysis_framework = qa_causal_randomized_design_analysis_framework(),
    observational_causal_planning = qa_observational_causal_planning(),
    observational_causal_estimation = qa_observational_causal_estimation(),
    observational_did_estimation = qa_observational_did_estimation(),
    vnext_rodeo_transformation_replay = qa_vnext_rodeo_transformation_replay(),
    vnext_model_bundle = qa_vnext_model_bundle(),
    vnext_artifact_framework = qa_vnext_artifact_framework(),
    vnext_scoring_lifecycle = qa_vnext_scoring_lifecycle(),
    vnext_forecasting_foundation = qa_vnext_forecasting_foundation(),
    vnext_panel_forecasting_foundation = qa_vnext_panel_forecasting_foundation(),
    vnext_hierarchical_forecasting_foundation = qa_vnext_hierarchical_forecasting_foundation(),
    vnext_panel_strategy_selection = qa_vnext_panel_strategy_selection(),
    vnext_hurdle_forecasting_foundation = qa_vnext_hurdle_forecasting_foundation(),
    vnext_funnel_forecasting_foundation = qa_vnext_funnel_forecasting_foundation(),
    vnext_multitarget_forecasting_foundation = qa_vnext_multitarget_forecasting_foundation(),
    vnext_multitarget_supervised_forecasting = qa_vnext_multitarget_supervised_forecasting(),
    vnext_forecasting_planning = qa_vnext_forecasting_planning(),
    vnext_forecasting_experiment_campaigns = qa_vnext_forecasting_experiment_campaigns(),
    vnext_usage_documentation = qa_vnext_usage_documentation()
  )

  rows <- lapply(names(qa_results), function(name) {
    result <- qa_results[[name]]
    if (is.data.frame(result)) {
      result <- data.table::as.data.table(result)
      result[, suite := name]
      if (!"message" %in% names(result)) {
        result[, message := ""]
      }
      return(result)
    }
    data.table::data.table(
      suite = name,
      check = name,
      status = "success",
      message = paste(utils::capture.output(str(result)), collapse = " ")
    )
  })

  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}
