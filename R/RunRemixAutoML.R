#' @title RunRemixAutoML
#'
#' @description GUI for running RemixAutoML functions. Time series evaluation and forecasting, machine learning, etc.
#'
#' @author Adrian Antico
#'
#' @family GUI
#'
#' @noRd
RunRemixAutoML <- function() {
  appDir <- system.file("shiny-apps", "PanelForecasting", package = "RemixAutoML")
  if(appDir == "") stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}

#' @title FeatureEngineering
#'
#' @description Feature Engineering Script
#'
#' @author Adrian Antico
#'
#' @family GUI ML
#'
#' @noRd
FeatureEngineering <- function() {
  source(file.path(system.file("shiny-apps", "myapp", package = "RemixAutoML"), "Feature_Engineering.R"))
}
