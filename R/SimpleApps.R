#' @noRd
UniqueLevels <- function(input, data, n, GroupVariables) {
  tryCatch({
    c(unique(data[[eval(input[['GroupVars']][[n]])]]))}, error = function(x) {
      tryCatch({
        c(unique(data[[eval(GroupVariables[[n]])]]))}, error = function(x) NULL)})
}

#' @title App_BoxPlotsOverTime
#'
#' @description Simple shiny app for viewing boxplots over time. You can use up to 3 categorical variables to filter by and one additional variable to filter as a bonus.
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param data Source data.table
#' @param FeatureNames Character vector of feature column names
#' @param GroupVariables Character vector of group column names
#' @param FilterVariable Variable to use to filter data before plotting
#' @param DateName Character scalar of the date column name
#' @param Title Title for plot
#' @param SubTitle SubTitle for plot
#' @param Debug FALSE
#'
#' @examples
#' \dontrun{
#' # Pull Data
#' data <- data.table::fread(system.file('tests/QA_DataSets/ThreeGroup-FC-Walmart-XREG3.csv', package = "RemixAutoML"))
#' data[, Date := as.Date(Date)]
#'
#' # Run App
#' RemixAutoML::App_BoxPlotsOverTime(
#'   data,
#'   FeatureNames = names(data)[5L:ncol(data)],
#'   GroupVariables = names(data)[seq_len(3L)],
#'   FilterVariable = 'XREG1',
#'   DateName = 'Date',
#'   DateBreaks = '14 day',
#'   DateLabels = '%Y %b %d',
#'   Title = 'Time Series Distribution',
#'   SubTitle = 'Dist. by Day',
#'   Debug = TRUE)
#'
#' # FeatureNames = names(data)[5L:ncol(data)]
#' # GroupVariables = names(data)[seq_len(3L)]
#' # FilterVariable = 'XREG1'
#' # DateName = 'Date'
#' # DateBreaks = '1 day'
#' # DateLabels = '%Y %b %d'
#' # Title = 'Time Series Distribution'
#' # SubTitle = 'Dist. by Day'
#' # Debug = TRUE
#' }
#'
#' @export
App_BoxPlotsOverTime <- function(data,
                                 FeatureNames = NULL,
                                 GroupVariables = NULL,
                                 FilterVariable = NULL,
                                 DateName = NULL,
                                 DateBreaks = '1 day',
                                 DateLabels = '%b %d',
                                 Title = NULL,
                                 SubTitle = NULL,
                                 Debug = FALSE) {

  # Pass args to shiny app
  shiny::shinyOptions(data = data,
                      FeatureNames = FeatureNames,
                      GroupVariables = GroupVariables,
                      FilterVariable = FilterVariable,
                      DateName = DateName,
                      DateBreaks = DateBreaks,
                      DateLabels = DateLabels,
                      Title = Title,
                      SubTitle = SubTitle,
                      Debug = Debug)

  # Run shiny app
  shiny::shinyAppDir(appDir = system.file("shiny-apps", "BoxPlotTS", package = 'RemixAutoML'))
}
