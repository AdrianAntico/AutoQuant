#' @noRd
UniqueLevels <- function(input,
                         data,
                         n,
                         GroupVariables) {
  tryCatch({
    c(unique(data[[eval(input[['GroupVars']][[n]])]]))}, error = function(x) {
      tryCatch({
        c(unique(data[[eval(GroupVariables[[n]])]]))}, error = function(x) NULL)})
}

#' @title App_TimeSeriesPlots
#'
#' @description Simple shiny app for viewing time series plots with box plots or line plots. You can use up to 3 categorical variables to filter by and one additional variable to filter as a bonus. You'll need to have shiny, shinyWidgets, htmltools
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param data Source data.table
#' @param FeatureNames Character vector of feature column names
#' @param GroupVariables Character vector of group column names
#' @param FilterVariable Variable to use to filter data before plotting
#' @param DateName Character scalar of the date column name
#' @param AppWidth Width of boxes
#' @param Debug FALSE
#'
#' @examples
#' \dontrun{
#' # Pull Data
#' data <- data.table::fread(system.file('tests/QA_DataSets/ThreeGroup-FC-Walmart-XREG3.csv', package = "RemixAutoML"))
#' data[, Date := as.Date(Date)]
#'
#' # Run App
#' RemixAutoML::App_TimeSeriesPlots(
#'   data,
#'   FeatureNames = names(data)[5L:ncol(data)],
#'   GroupVariables = names(data)[seq_len(3L)],
#'   FilterVariable = 'XREG1',
#'   DateName = 'Date',
#'   AppWidth = 9L,
#'   Debug = TRUE)
#'
#' # FeatureNames = names(data)[5L:ncol(data)]
#' # GroupVariables = names(data)[seq_len(3L)]
#' # FilterVariable = 'XREG1'
#' # DateName = 'Date'
#' # Debug = TRUE
#' }
#'
#' @export
App_TimeSeriesPlots <- function(data,
                                FeatureNames = NULL,
                                GroupVariables = NULL,
                                FilterVariable = NULL,
                                DateName = NULL,
                                AppWidth = 9L,
                                Debug = FALSE) {

  # Pass args to shiny app
  shiny::shinyOptions(
    data = data,
    FeatureNames = FeatureNames,
    GroupVariables = GroupVariables,
    FilterVariable = FilterVariable,
    DateName = DateName,
    AppWidth = AppWidth,
    Debug = Debug)

  # Run shiny app
  shiny::shinyAppDir(appDir = system.file("shiny-apps", "BoxPlotTS", package = 'RemixAutoML'))
}
