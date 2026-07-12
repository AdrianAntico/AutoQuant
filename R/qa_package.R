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
    catboost_builder_artifacts = qa_generate_catboost_builder_artifacts()
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
