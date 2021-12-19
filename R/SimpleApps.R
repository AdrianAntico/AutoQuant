#' @title AppsPlotting
#'
#' @description Simple shiny app for viewing time series plots with box plots or line plots. You can use up to 3 categorical variables to filter by and one additional variable to filter as a bonus. You'll need to have shiny, shinyWidgets, htmltools
#'
#' @author Adrian Antico
#' @family GUI
#'
#' @param data Source data.table
#' @param XVariable Starter column name for x-variable. Not needed if x-axis is the DateName column
#' @param YVariable Starter column name for y-variable
#' @param DateName Starter column name for date-variable
#' @param GroupVariables Starter column name for group-variables
#' @param FilterVariable Starter column name for filter-variable
#' @param ModelOutputList ML output from RemixAutoML Auto___() SL functions
#' @param HeaderColor 'black', 'blue', 'purple', 'green', 'red', 'yellow'
#' @param AppWidth Width of boxes
#' @param GroupVarsBoxColor Choose from 'red', 'yellow', 'aqua', 'blue', 'light-blue', 'green', 'navy', 'teal', 'olive', 'lime', 'orange', 'fuchsia', 'purple', 'maroon', 'black'
#' @param VarsBoxColor Choose from 'red', 'yellow', 'aqua', 'blue', 'light-blue', 'green', 'navy', 'teal', 'olive', 'lime', 'orange', 'fuchsia', 'purple', 'maroon', 'black'
#' @param FilterBoxColor Choose from 'red', 'yellow', 'aqua', 'blue', 'light-blue', 'green', 'navy', 'teal', 'olive', 'lime', 'orange', 'fuchsia', 'purple', 'maroon', 'black'
#' @param PlotBoxColor Choose from 'red', 'yellow', 'aqua', 'blue', 'light-blue', 'green', 'navy', 'teal', 'olive', 'lime', 'orange', 'fuchsia', 'purple', 'maroon', 'black'
#' @param CreatePlotButtonColor Choose from 'default', 'primary', 'warning', 'danger', 'success', 'royal'
#' @param UpdatePlotButtonColor Choose from 'default', 'primary', 'warning', 'danger', 'success', 'royal'
#' @param ResetPlotButtonColor Choose from 'default', 'primary', 'warning', 'danger', 'success', 'royal'
#' @param Browser FALSE
#' @param Docker FALSE
#' @param UserName_Password_DT NULL. In order to enforce authentication, supply a data.table with columns 'UserName' which contains the names of your users and 'Password' which contains the acceptable passwords. E.g. data.table::data.table(UserName = c('Adrian Antico', 'Guest'), Password = c('Password1', 'Password2')). Case sensitivity applies.
#' @param Debug FALSE
#'
#' @examples
#' \dontrun{
#' # Pull Data
#' data <- data.table::fread(system.file('tests/QA_DataSets/ThreeGroup-FC-Walmart-XREG3.csv', package = "RemixAutoML"))
#' data[, Date := as.Date(Date)]
#'
#' # Run App
#' RemixAutoML::AppsPlotting(
#'   data,
#'   XVariable = NULL,
#'   YVariable = 'XREG1',
#'   DateName = 'Date',
#'   GroupVariables = names(data)[seq_len(3L)],
#'   FilterVariable = 'XREG1',
#'   ModelOutputList = NULL,
#'   HeaderColor = 'black',
#'   AppWidth = 12L,
#'   LogoWidth = '1000px',
#'   LogoHeight = '100px',
#'   GroupVarsBoxColor = 'navy',
#'   VarsBoxColor = 'purple',
#'   FilterBoxColor = 'blue',
#'   PlotBoxColor = 'aqua',
#'   CreatePlotButtonColor = 'default',
#'   UpdatePlotButtonColor = 'default',
#'   ResetPlotButtonColor = 'default',
#'   Docker = FALSE,
#'   Browser = FALSE,
#'   UserName_Password_DT = NULL,
#'   Debug = FALSE)
#'
#' # XVariable = 'Date'
#' # YVariable = 'XREG1'
#' # DateName = 'Date'
#' # GroupVariables = names(data)[seq_len(3L)]
#' # FilterVariable = 'XREG1'
#' # ModelOutputList = NULL
#' # Debug = TRUE
#' # HeaderColor = 'black'
#' # AppWidth = 12L
#' # LogoWidth = '1000px'
#' # LogoHeight = '100px'
#' # GroupVarsBoxColor = 'navy'
#' # VarsBoxColor = 'purple'
#' # FilterBoxColor = 'blue'
#' # PlotBoxColor = 'aqua'
#' # CreatePlotButtonColor = 'default'
#' # UpdatePlotButtonColor = 'default'
#' # ResetPlotButtonColor = 'default'
#' # Docker = FALSE
#' # Browser = FALSE
#' # UserName_Password_DT = NULL
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
                         ModelOutputList = NULL,
                         HeaderColor = 'black',
                         AppWidth = 12L,
                         LogoWidth = '1000px',
                         LogoHeight = '100px',
                         GroupVarsBoxColor = 'navy',
                         VarsBoxColor = 'purple',
                         FilterBoxColor = 'blue',
                         PlotBoxColor = 'aqua',
                         CreatePlotButtonColor = 'primary',
                         UpdatePlotButtonColor = 'primary',
                         ResetPlotButtonColor = 'primary',
                         Browser = FALSE,
                         Docker = FALSE,
                         UserName_Password_DT = NULL,
                         Debug = FALSE) {

  # Pass args to shiny app
  shiny::shinyOptions(
    data = data,
    XVariable = XVariable,
    YVariable = YVariable,
    DateName = DateName,
    GroupVariables = GroupVariables,
    FilterVariable = FilterVariable,
    ModelOutputList = ModelOutputList,
    HeaderColor = HeaderColor,
    AppWidth = AppWidth,
    LogoWidth = LogoWidth,
    LogoHeight = LogoHeight,
    GroupVarsBoxColor = GroupVarsBoxColor,
    VarsBoxColor = VarsBoxColor,
    FilterBoxColor = FilterBoxColor,
    PlotBoxColor = PlotBoxColor,
    CreatePlotButtonColor = 'default',
    UpdatePlotButtonColor = 'default',
    ResetPlotButtonColor = 'default',
    UserName_Password_DT = UserName_Password_DT,
    Debug = Debug)

  # Run shiny app
  if(!Docker) {
    shiny::shinyAppDir(appDir = system.file('shiny-apps', 'Insights', package = 'RemixAutoML'))
  } else {
    shiny::runApp(appDir = system.file('shiny-apps', 'Insights', package = 'RemixAutoML'), display.mode = "normal", launch.browser = TRUE)
  }
}

#' @title RunRemixAutoML
#'
#' @description GUI for running RemixAutoML functions. Time series evaluation and forecasting, machine learning, etc.
#'
#' @author Adrian Antico
#'
#' @family GUI
#'
#' @export
RunRemixAutoML <- function(data = NULL, ModelOutput = NULL, TargetName = NULL, PredictName = NULL, DateName = NULL, Debug = FALSE) {

  # Pass args to shiny app
  shiny::shinyOptions(
    data = data,
    ModelOutput = ModelOutput,
    TargetName = TargetName,
    PredictName = PredictName,
    DateName = DateName,
    Debug = Debug)
  shiny::shinyAppDir(appDir = system.file('shiny-apps', 'PanelForecasting', package = 'RemixAutoML'))
  #shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}

#' @title FeatureEngineering
#'
#' @description Feature Engineering Script
#'
#' @author Adrian Antico
#'
#' @family GUI
#'
#' @noRd
FeatureEngineering <- function() {
  source(file.path(system.file("shiny-apps", "myapp", package = "RemixAutoML"), "Feature_Engineering.R"))
}
