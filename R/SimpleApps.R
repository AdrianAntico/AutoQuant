#' @title Apps_Plotting
#'
#' @description Simple shiny app for viewing time series plots with box plots or line plots. You can use up to 3 categorical variables to filter by and one additional variable to filter as a bonus. You'll need to have shiny, shinyWidgets, htmltools
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param data Source data.table
#' @param XVariable Starter column name for x-variable. Not needed if x-axis is the DateName column
#' @param YVariable Starter column name for y-variable
#' @param DateName Starter column name for date-variable
#' @param GroupVariables Starter column name for group-variables
#' @param FilterVariable Starter column name for filter-variable
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
#' RemixAutoML::Apps_Plotting(
#'   data,
#'   XVariable = 'Date',
#'   YVariable = 'XREG1',
#'   DateName = 'Date',
#'   FilterVariable = 'XREG1',
#'   GroupVariables = names(data)[seq_len(3L)],
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
#' # XVariable = 'Date'
#' # YVariable = 'XREG1'
#' # DateName = 'Date'
#' # GroupVariables = names(data)[seq_len(3L)]
#' # FilterVariable = 'XREG1'
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
AppsPlotting <- function(data,
                         XVariable = NULL,
                         YVariable = NULL,
                         DateName = NULL,
                         GroupVariables = NULL,
                         FilterVariable = NULL,
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
    XVariable = XVariable,
    YVariable = YVariable,
    DateName = DateName,
    GroupVariables = GroupVariables,
    FilterVariable = FilterVariable,
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
