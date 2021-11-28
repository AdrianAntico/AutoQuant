#' @title App_TimeSeriesPlots
#'
#' @description Simple shiny app for viewing time series plots with box plots, viloin plots, or line plots. You can use up to 3 categorical variables to filter by and one additional variable to filter as a bonus. You'll need to have shiny, shinyWidgets, htmltools
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param data Source data.table
#' @param FeatureNames Character vector of feature column names
#' @param GroupVariables Character vector of group column names
#' @param FilterVariable Variable to use to filter data before plotting
#' @param DateName Character scalar of the date column name
#' @param AppTitle Defaults to 'Time Series Plotting'
#' @param AppWidth Width of boxes
#' @param Box1Color Choose from 'red', 'yellow', 'aqua', 'blue', 'light-blue', 'green', 'navy', 'teal', 'olive', 'lime', 'orange', 'fuchsia', 'purple', 'maroon', 'black'
#' @param Box2Color Choose from 'red', 'yellow', 'aqua', 'blue', 'light-blue', 'green', 'navy', 'teal', 'olive', 'lime', 'orange', 'fuchsia', 'purple', 'maroon', 'black'
#' @param Box3Color Choose from 'red', 'yellow', 'aqua', 'blue', 'light-blue', 'green', 'navy', 'teal', 'olive', 'lime', 'orange', 'fuchsia', 'purple', 'maroon', 'black'
#' @param CreatePlotButtonColor Choose from 'default', 'primary', 'warning', 'danger', 'success', 'royal'
#' @param UpdatePlotButtonColor Choose from 'default', 'primary', 'warning', 'danger', 'success', 'royal'
#' @param ResetPlotButtonColor Choose from 'default', 'primary', 'warning', 'danger', 'success', 'royal'
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
#'   AppTitle = 'Time Series Plotting',
#'   AppWidth = 12L,
#'   LogoWidth = '1000px',
#'   LogoHeight = '100px',
#'   Box1Color = 'navy',
#'   Box2Color = 'purple',
#'   Box3Color = 'aqua',
#'   CreatePlotButtonColor = 'default',
#'   UpdatePlotButtonColor = 'default',
#'   ResetPlotButtonColor = 'default',
#'   Debug = TRUE)
#'
#' # FeatureNames = names(data)[5L:ncol(data)]
#' # GroupVariables = names(data)[seq_len(3L)]
#' # FilterVariable = 'XREG1'
#' # DateName = 'Date'
#' # Debug = TRUE
#' # AppWidth = 12L,
#' # LogoWidth = '1000px',
#' # LogoHeight = '100px',
#' # Box1Color = 'navy',
#' # Box2Color = 'purple',
#' # Box3Color = 'aqua',
#' # CreatePlotButtonColor = 'default',
#' # UpdatePlotButtonColor = 'default',
#' # ResetPlotButtonColor = 'default',
#' # Debug = TRUE
#' }
#'
#' @export
App_TimeSeriesPlots <- function(data,
                                FeatureNames = NULL,
                                GroupVariables = NULL,
                                FilterVariable = NULL,
                                DateName = NULL,
                                AppTitle = 'Time Series Plotting',
                                AppWidth = 12L,
                                LogoWidth = '1000px',
                                LogoHeight = '100px',
                                Box1Color = 'navy',
                                Box2Color = 'purple',
                                Box3Color = 'aqua',
                                CreatePlotButtonColor = 'default',
                                UpdatePlotButtonColor = 'default',
                                ResetPlotButtonColor = 'default',
                                Debug = FALSE) {

  # Pass args to shiny app
  shiny::shinyOptions(
    data = data,
    FeatureNames = FeatureNames,
    GroupVariables = GroupVariables,
    FilterVariable = FilterVariable,
    DateName = DateName,
    AppTitle = AppTitle,
    AppWidth = AppWidth,
    LogoWidth = LogoWidth,
    LogoHeight = LogoHeight,
    Box1Color = 'navy',
    Box2Color = 'purple',
    Box3Color = 'aqua',
    CreatePlotButtonColor = 'default',
    UpdatePlotButtonColor = 'default',
    ResetPlotButtonColor = 'default',
    Debug = Debug)

  # Run shiny app
  shiny::shinyAppDir(appDir = system.file("shiny-apps", "TimeSeriesPlotting", package = 'RemixAutoML'))
}
