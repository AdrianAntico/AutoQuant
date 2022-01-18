# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# Environment Setup                    ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
StartEnv <- as.list(environment())
library(data.table)
data.table::setDTthreads(threads = max(1L, parallel::detectCores()-1L))
options(shiny.maxRequestSize = 100000000*1024^2)
options(scipen = 999)

PlotNamesLookup <- list()
PlotNamesLookup[['Test_EvaluationPlot']] <- c('CalibrationPlot_Test')
PlotNamesLookup[['Train_EvaluationPlot']] <- c('Calibration_Train')
PlotNamesLookup[['Train_EvaluationBoxPlot']] <- c('CalibrationBox_Train')
PlotNamesLookup[['Test_EvaluationBoxPlot']] <- c('CalibrationBox_Test')
PlotNamesLookup[['Train_ParDepPlots']] <- c('PartialDep_Train')
PlotNamesLookup[['Test_ParDepPlots']] <- c('PartialDep_Test')
PlotNamesLookup[['Test_ParDepBoxPlots']] <- c('PartialDepBox_Test')
PlotNamesLookup[['Train_ParDepBoxPlots']] <- c('PartialDepBox_Train')
PlotNamesLookup[['Test_ResidualsHistogram']] <- c('ResidualsHist_Test')
PlotNamesLookup[['Train_ResidualsHistogram']] <- c('ResidualsHist_Train')
PlotNamesLookup[['Test_ScatterPlot']] <- c('Scatter_Test')
PlotNamesLookup[['Train_ScatterPlot']] <- c('Scatter_Train')
PlotNamesLookup[['Test_CopulaPlot']] <- c('Copula_Test')
PlotNamesLookup[['Train_CopulaPlot']] <- c('Copula_Train')
PlotNamesLookup[['Test_VariableImportance']] <- c('VarImp_Test')
PlotNamesLookup[['Validation_VariableImportance']] <- c('VarImp_Validation')
PlotNamesLookup[['Train_VariableImportance']] <- c('VarImp_Train')
PlotNamesLookup[['Test_GainsPlot']] <- c('Gains_Test')
PlotNamesLookup[['Train_GainsPlot']] <- c('Gains_Train')
PlotNamesLookup[['Test_LiftPlot']] <- c('Lift_Test')
PlotNamesLookup[['Train_LiftPlot']] <- c('Lift_Train')
PlotNamesLookup[['Test_ROC_Plot']] <- c('ROC_Test')
PlotNamesLookup[['Train_ROC_Plot']] <- c('ROC_Train')
PlotNamesLookup[['Scatter']] <- c('Scatter')
PlotNamesLookup[['Copula']] <- c('Copula')
PlotNamesLookup[['Line']] <- c('Line')
PlotNamesLookup[['Bar']] <- c('Bar')
PlotNamesLookup[['BoxPlot']] <- c('Box')
PlotNamesLookup[['ViolinPlot']] <- c('Violin')
PlotNamesLookup[['Histogram']] <- c('Hist')

PlotNamesLookup[['CalibrationPlot_Test']] <- c('Test_EvaluationPlot')
PlotNamesLookup[['Calibration_Train']] <- c('Train_EvaluationPlot')
PlotNamesLookup[['CalibrationBox_Train']] <- c('Train_EvaluationBoxPlot')
PlotNamesLookup[['CalibrationBox_Test']] <- c('Test_EvaluationBoxPlot')
PlotNamesLookup[['PartialDep_Train']] <- c('Train_ParDepPlots')
PlotNamesLookup[['PartialDep_Test']] <- c('Test_ParDepPlots')
PlotNamesLookup[['PartialDepBox_Test']] <- c('Test_ParDepBoxPlots')
PlotNamesLookup[['PartialDepBox_Train']] <- c('Train_ParDepBoxPlots')
PlotNamesLookup[['ResidualsHist_Test']] <- c('Test_ResidualsHistogram')
PlotNamesLookup[['ResidualsHist_Train']] <- c('Train_ResidualsHistogram')
PlotNamesLookup[['Scatter_Test']] <- c('Test_ScatterPlot')
PlotNamesLookup[['Scatter_Train']] <- c('Train_ScatterPlot')
PlotNamesLookup[['Copula_Test']] <- c('Test_CopulaPlot')
PlotNamesLookup[['Copula_Train']] <- c('Train_CopulaPlot')
PlotNamesLookup[['VarImp_Test']] <- c('Test_VariableImportance')
PlotNamesLookup[['VarImp_Validation']] <- c('Validation_VariableImportance')
PlotNamesLookup[['VarImp_Train']] <- c('Train_VariableImportance')
PlotNamesLookup[['Gains_Test']] <- c('Test_GainsPlot')
PlotNamesLookup[['Gains_Train']] <- c('Train_GainsPlot')
PlotNamesLookup[['Lift_Test']] <- c('Test_LiftPlot')
PlotNamesLookup[['Lift_Train']] <- c('Train_LiftPlot')
PlotNamesLookup[['ROC_Test']] <- c('Test_ROC_Plot')
PlotNamesLookup[['ROC_Train']] <- c('Train_ROC_Plot')
PlotNamesLookup[['Scatter']] <- c('Scatter')
PlotNamesLookup[['Copula']] <- c('Copula')
PlotNamesLookup[['Line']] <- c('Line')
PlotNamesLookup[['Bar']] <- c('Bar')
PlotNamesLookup[['Box']] <- c('BoxPlot')
PlotNamesLookup[['Violin']] <- c('ViolinPlot')
PlotNamesLookup[['Hist']] <- c('Histogram')
PlotNamesLookup[['CorrMatrix']] <- c('CorrMatrix')
PlotNamesLookup[['ShapelyVarImp']] <- c('ShapelyVarImp')

# Initialize Data
data <- NULL

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# Passthrough Args                     ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

# Meta Data related
BlobStorageURL <- shiny::getShinyOption('BlobStorageURL', default = NULL)
IFrameLocation <- shiny::getShinyOption('IFrameLocation', default = NULL)
PlotObjectHome <- shiny::getShinyOption('PlotObjectHome', default = NULL)

# App and Plot related
HeaderColor <- shiny::getShinyOption('HeaderColor')
AppWidth <- shiny::getShinyOption('AppWidth')
LogoWidth <- shiny::getShinyOption('LogoWidth')
LogoHeight <- shiny::getShinyOption('LogoHeight')
GroupVarsBoxColor <- shiny::getShinyOption('GroupVarsBoxColor')
VarsBoxColor <- shiny::getShinyOption('VarsBoxColor')
FilterBoxColor <- shiny::getShinyOption('FilterBoxColor')
PlotBoxColor <- shiny::getShinyOption('PlotBoxColor')
CreatePlotButtonColor <- shiny::getShinyOption('CreatePlotButtonColor')
UpdatePlotButtonColor <- shiny::getShinyOption('UpdatePlotButtonColor')
ResetPlotButtonColor <- shiny::getShinyOption('ResetPlotButtonColor')
H4Color <- shiny::getShinyOption('H4Color')
H3Color <- shiny::getShinyOption('H3Color')
AppTextColor <- shiny::getShinyOption('AppTextColor')
Debug <- shiny::getShinyOption('Debug')

# Usernames and Passwords
UserName_Password_DT <- shiny::getShinyOption(name = 'UserName_Password_DT', default = NULL)
if(!is.null(UserName_Password_DT) && 'UserName' %in% names(UserName_Password_DT) && 'Password' %in% names(UserName_Password_DT)) {
  Credentials <- UserName_Password_DT
} else {
  Credentials <- data.table::data.table(
    UserName = c('Guest'),
    Password = c('Password'))
}

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# UI Code                              ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
ui <- shinydashboard::dashboardPage(

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Header                               ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  title = 'Tablows',

  # Top of page color
  skin = HeaderColor,

  # App Header
  shinydashboard::dashboardHeader(
    htmltools::tags$li(class = "dropdown",
                       htmltools::tags$style(".main-header {max-height: 57px}"),
                       htmltools::tags$style(".main-header .logo {height: 57px;}"),
                       htmltools::tags$style(".sidebar-toggle {height: 20px; padding-top: 1px !important;}"),
                       htmltools::tags$style(".navbar {min-height:55px !important}")),
    titleWidth = 190,
    title = htmltools::HTML(
      "
      <div style = 'vertical-align:middle'>
      <img src='NewPackageLogo.png' align = 'center' height = '54px'></img>
      </div>
      ")),

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Side Bar Menu                        ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  shinydashboard::dashboardSidebar(

    # Needed for authentication-related activities
    shinyjs::useShinyjs(),

    # Sidebar menu
    shinydashboard::sidebarMenu(

      # Sidebar for switching pages only (no other buttons will work. Ensures authentically)
      id = "sidebar",
      tags$head(tags$style(".inactiveLink {
                            pointer-events: none;
                           cursor: default;
                           }")),

      # -- ADD SPACE
      RemixAutoML::BlankRow(AppWidth),

      # Home Page
      shinydashboard::menuItem(text="Login", tabName="Login", icon=shiny::icon("fort-awesome"), selected = TRUE),

      # -- ADD SPACE
      RemixAutoML::BlankRow(AppWidth),

      # Home Page
      shinydashboard::menuItem(text="Import Data", tabName='LoadDataPage', icon=shiny::icon("fort-awesome")),

      # -- ADD SPACE
      RemixAutoML::BlankRow(AppWidth),

      # Home Page
      shinydashboard::menuItem(text="Create Plots", tabName='Plotter', icon=shiny::icon("fort-awesome")),

      # -- ADD SPACE
      RemixAutoML::BlankRow(AppWidth),

      # Home Page
      shinydashboard::menuItem(text="Print Code", tabName='CodePrint', icon=shiny::icon("fort-awesome")))),

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # DashboardBody                        ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  shinydashboard::dashboardBody(

    # ----

    # ----

    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    # TabItems                             ----
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    shinydashboard::tabItems(


      # ----

      # ----

      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
      # Login Page                           ----
      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
      shinydashboard::tabItem(
        selected = TRUE,
        tabName = 'Login',
        shiny::fluidRow(
          shiny::column(
            width = AppWidth, shinyjs::useShinyjs(),
            shinydashboard::box(
              title = htmltools::tagList(shiny::icon('filter', lib = 'font-awesome'), 'Login Panel'),
              solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
              background = GroupVarsBoxColor, width = AppWidth,
              shiny::selectInput(
                inputId = "UserName",
                label =  "Select from Names",
                choices = Credentials[['UserName']],
                selected = 'UserName'),
              shiny::textInput(
                inputId = "Password",
                label =  "Input Password",
                value = if(Credentials[UserName == 'Adrian Antico', .N] > 0) Credentials$Password[[1L]] else 'Password')))),

        # Add Space
        RemixAutoML::BlankRow(AppWidth),

        # Button to login and go to Load Data
        shiny::fluidRow(
          shiny::column(
            width = 3L, shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = 'Check_Credentials',
              label = 'Check Credentials',
              icon = shiny::icon('chevron-right', lib = 'font-awesome'),
              style = 'gradient',
              color = 'royal')))),

      # ----

      # ----

      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
      # Load Data Page                       ----
      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
      shinydashboard::tabItem(
        tabName = 'LoadDataPage',

        # Loaders
        shinydashboard::box(
          title = htmltools::tagList(shiny::icon('filter', lib = 'font-awesome'), 'Loading Objects'),
          width=AppWidth, solidHeader=TRUE, collapsible=TRUE, collapsed=FALSE, background='purple',
          RemixAutoML::BlankRow(AppWidth),
          shiny::fileInput(
            inputId = 'DataLoad',
            label =  'Choose CSV File',
            accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
          shiny::textInput(
            inputId = 'blob',
            label = 'Azure Blob Storage File Name',
            value = NULL,
            placeholder = 'Load'),
          RemixAutoML::BlankRow(AppWidth),
          shiny::fileInput(
            inputId = "ModelObjectLoad",
            label =  "RemixAutoML .Rdata Model Output List")),

        # Add Space
        RemixAutoML::BlankRow(AppWidth),

        # Go to Plotter
        shiny::fluidRow(
          shiny::column(
            width = 3L, shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(inputId='LoadDataButton', label='Load Data', icon=shiny::icon('chevron-right', lib='font-awesome'), style='gradient', color='royal'))),

        # Add Space
        RemixAutoML::BlankRow(AppWidth),
        RemixAutoML::BlankRow(AppWidth),

        # Azure Blob Data
        shinydashboard::box(
          title = htmltools::tagList(shiny::icon('filter', lib = 'font-awesome'), 'Azure Blob Storage iframe'),
          width=9L, solidHeader=TRUE, collapsible=TRUE, collapsed=TRUE, background='aqua',
          shiny::fluidRow(
            shiny::column(
              width = 9L,
              shiny::htmlOutput('IFrame'))))),

      # ----

      # ----

      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
      # Plotter Page                         ----
      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
      shinydashboard::tabItem(

        # -- TAB REFERENCE VALUE
        tabName = "Plotter",

        # ----

        # ----

        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
        # Plot Inputs                          ----
        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

        # Add Space
        RemixAutoML::BlankRow(AppWidth),

        # Plot Type
        shinydashboard::box(
          title = htmltools::tagList(shiny::icon('chart-bar', lib = 'font-awesome'), tags$b('Core Inputs')),
          Footer = 'Dragula Input available from the esquisse package',
          solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE, background = 'navy', width = AppWidth,

          # Plotting Variables
          shiny::fluidRow(
            width=12L,

            # Plot 1
            shiny::column(
              width = 2L,

              # Plot 1-2 Core Variables in DropDowns
              tags$h4(tags$b('Plot 1')),
              shinyWidgets::dropdown(
                right = FALSE, animate = TRUE, circle = FALSE, tooltip = FALSE, status = "info", width = LogoWidth,
                tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Core Variables for Plot 1')),
                RemixAutoML::BlankRow(AppWidth),

                # PlotType Selection
                shiny::fluidRow(
                  width=AppWidth,
                  shiny::column(12L, shiny::uiOutput('Plot1'))),

                # Y-Variable
                shiny::fluidRow(
                  width=AppWidth,
                  shiny::column(12L, shiny::uiOutput('YVar1'))),

                # X-Variable
                shiny::fluidRow(
                  width=AppWidth,
                  shiny::column(12L, shiny::uiOutput('XVar1'))),

                # Group-Variables
                shiny::fluidRow(
                  width=AppWidth,
                  shiny::column(12L, shiny::uiOutput('GroupVars1'))),

                # Group-Levels
                shiny::fluidRow(
                  width=AppWidth,
                  shiny::column(4L, shiny::conditionalPanel(width = 3L, condition = "length(input['GroupVars1']) >= 1", shiny::uiOutput('Levels_1_1'))),
                  shiny::column(4L, shiny::conditionalPanel(width = 3L, condition = "length(input['GroupVars1']) >= 1", shiny::uiOutput('Levels_1_2'))),
                  shiny::column(4L, shiny::conditionalPanel(width = 3L, condition = "length(input['GroupVars1']) >= 1", shiny::uiOutput('Levels_1_3')))),

                # By-Variables
                shiny::fluidRow(width=AppWidth, shiny::column(12L, shiny::uiOutput('FacetVar_1_1'))),
                RemixAutoML::BlankRow(AppWidth),
                shiny::fluidRow(width=AppWidth, shiny::column(12L, shiny::uiOutput('FacetVar_1_2'))),
                RemixAutoML::BlankRow(AppWidth),
                shiny::fluidRow(width=AppWidth, shiny::column(12L, shiny::uiOutput('SizeVar1'))),

                # Correlation-Variables
                shiny::fluidRow(
                  width = AppWidth,
                  shiny::column(6L, shiny::uiOutput('CorVariables1'))),

                # Scoring Variables
                shiny::fluidRow(
                  width = AppWidth,
                  shiny::column(6L, shiny::uiOutput('ScoreVar1'))),

                # Partial Dependence Plot Variable
                shiny::fluidRow(
                  width = AppWidth,
                  shiny::column(6L, shiny::uiOutput('PDP_Variable1'))))),


            # Plot 2
            shiny::column(
              width = 2L,

              # Core Variables
              tags$h4(tags$b('Plot 2')),
              shinyWidgets::dropdown(
                right = FALSE, animate = TRUE, circle = FALSE, tooltip = FALSE, status = "info", width = LogoWidth,
                tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Core Variables for Plot 2')),
                RemixAutoML::BlankRow(AppWidth),

                # PlotType Selection
                shiny::fluidRow(
                  width=AppWidth,
                  shiny::column(12L, shiny::uiOutput('Plot2'))),

                # Y-Variable
                shiny::fluidRow(
                  width=AppWidth,
                  shiny::column(12L, shiny::uiOutput('YVar2'))),

                # X-Variable
                shiny::fluidRow(
                  width=AppWidth,
                  shiny::column(12L, shiny::uiOutput('XVar2'))),

                # Group-Variables
                shiny::fluidRow(
                  width=AppWidth,
                  shiny::column(12L, shiny::uiOutput('GroupVars2'))),

                # Group-Levels
                shiny::fluidRow(
                  width=AppWidth,
                  shiny::column(4L, shiny::conditionalPanel(width = 3L, condition = "length(input['GroupVars2']) >= 1", shiny::uiOutput('Levels_2_1'))),
                  shiny::column(4L, shiny::conditionalPanel(width = 3L, condition = "length(input['GroupVars2']) >= 1", shiny::uiOutput('Levels_2_2'))),
                  shiny::column(4L, shiny::conditionalPanel(width = 3L, condition = "length(input['GroupVars2']) >= 1", shiny::uiOutput('Levels_2_3')))),

                # By-Variables
                shiny::fluidRow(width=AppWidth, shiny::column(12L, shiny::uiOutput('FacetVar_2_1'))),
                RemixAutoML::BlankRow(AppWidth),
                shiny::fluidRow(width=AppWidth, shiny::column(12L, shiny::uiOutput('FacetVar_2_2'))),
                RemixAutoML::BlankRow(AppWidth),
                shiny::fluidRow(width=AppWidth, shiny::column(12L, shiny::uiOutput('SizeVar2'))),

                # Correlation-Variables
                shiny::fluidRow(
                  width = AppWidth,
                  shiny::column(6L, shiny::uiOutput('CorVariables2'))),

                # Scoring Variables
                shiny::fluidRow(
                  width = AppWidth,
                  shiny::column(6L, shiny::uiOutput('ScoreVar2'))),

                # Partial Dependence Plot Variable
                shiny::fluidRow(
                  width = AppWidth,
                  shiny::column(6L, shiny::uiOutput('PDP_Variable2'))))),


            # Plot 3
            shiny::column(
              width = 2L,

              # Core Variables
              tags$h4(tags$b('Plot 3')),
              shinyWidgets::dropdown(
                right = FALSE, animate = TRUE, circle = FALSE, tooltip = FALSE, status = "info", width = LogoWidth,
                tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Core Variables for Plot 3')),
                RemixAutoML::BlankRow(AppWidth),

                # PlotType Selection
                shiny::fluidRow(
                  width=AppWidth,
                  shiny::column(12L, shiny::uiOutput('Plot3'))),

                # Y-Variable
                shiny::fluidRow(
                  width=AppWidth,
                  shiny::column(12L, shiny::uiOutput('YVar3'))),

                # X-Variable
                shiny::fluidRow(
                  width=AppWidth,
                  shiny::column(12L, shiny::uiOutput('XVar3'))),

                # Group-Variables
                shiny::fluidRow(
                  width=AppWidth,
                  shiny::column(12L, shiny::uiOutput('GroupVars3'))),

                # Group-Levels
                shiny::fluidRow(
                  width=AppWidth,
                  shiny::column(4L, shiny::conditionalPanel(width = 3L, condition = "length(input['GroupVars3']) >= 1", shiny::uiOutput('Levels_3_1'))),
                  shiny::column(4L, shiny::conditionalPanel(width = 3L, condition = "length(input['GroupVars3']) >= 1", shiny::uiOutput('Levels_3_2'))),
                  shiny::column(4L, shiny::conditionalPanel(width = 3L, condition = "length(input['GroupVars3']) >= 1", shiny::uiOutput('Levels_3_3')))),

                # By-Variables
                shiny::fluidRow(width=AppWidth, shiny::column(12L, shiny::uiOutput('FacetVar_3_1'))),
                RemixAutoML::BlankRow(AppWidth),
                shiny::fluidRow(width=AppWidth, shiny::column(12L, shiny::uiOutput('FacetVar_3_2'))),
                RemixAutoML::BlankRow(AppWidth),
                shiny::fluidRow(width=AppWidth, shiny::column(12L, shiny::uiOutput('SizeVar3'))),

                # Correlation-Variables
                shiny::fluidRow(
                  width = AppWidth,
                  shiny::column(6L, shiny::uiOutput('CorVariables3'))),

                # Scoring Variables
                shiny::fluidRow(
                  width = AppWidth,
                  shiny::column(6L, shiny::uiOutput('ScoreVar3'))),

                # Partial Dependence Plot Variable
                shiny::fluidRow(
                  width = AppWidth,
                  shiny::column(6L, shiny::uiOutput('PDP_Variable3'))))),



            # Plot 4
            shiny::column(
              width = 2L,

              # Core Variables
              tags$h4(tags$b('Plot 4')),
              shinyWidgets::dropdown(
                right = FALSE, animate = TRUE, circle = FALSE, tooltip = FALSE, status = "info", width = LogoWidth,
                tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Core Variables for Plot 4')),
                RemixAutoML::BlankRow(AppWidth),

                # PlotType Selection
                shiny::fluidRow(
                  width=AppWidth,
                  shiny::column(12L, shiny::uiOutput('Plot4'))),

                # Y-Variable
                shiny::fluidRow(
                  width=AppWidth,
                  shiny::column(12L, shiny::uiOutput('YVar4'))),

                # X-Variable
                shiny::fluidRow(
                  width=AppWidth,
                  shiny::column(12L, shiny::uiOutput('XVar4'))),

                # Group-Variables
                shiny::fluidRow(
                  width=AppWidth,
                  shiny::column(12L, shiny::uiOutput('GroupVars4'))),

                # Group-Levels
                shiny::fluidRow(
                  width=AppWidth,
                  shiny::column(4L, shiny::conditionalPanel(width = 3L, condition = "length(input['GroupVars4']) >= 1", shiny::uiOutput('Levels_4_1'))),
                  shiny::column(4L, shiny::conditionalPanel(width = 3L, condition = "length(input['GroupVars4']) >= 1", shiny::uiOutput('Levels_4_2'))),
                  shiny::column(4L, shiny::conditionalPanel(width = 3L, condition = "length(input['GroupVars4']) >= 1", shiny::uiOutput('Levels_4_3')))),

                # By-Variables
                shiny::fluidRow(width=AppWidth, shiny::column(12L, shiny::uiOutput('FacetVar_4_1'))),
                RemixAutoML::BlankRow(AppWidth),
                shiny::fluidRow(width=AppWidth, shiny::column(12L, shiny::uiOutput('FacetVar_4_2'))),
                RemixAutoML::BlankRow(AppWidth),
                shiny::fluidRow(width=AppWidth, shiny::column(12L, shiny::uiOutput('SizeVar4'))),

                # Correlation-Variables
                shiny::fluidRow(
                  width = AppWidth,
                  shiny::column(6L, shiny::uiOutput('CorVariables4'))),

                # Scoring Variables
                shiny::fluidRow(
                  width = AppWidth,
                  shiny::column(6L, shiny::uiOutput('ScoreVar4'))),

                # Partial Dependence Plot Variable
                shiny::fluidRow(
                  width = AppWidth,
                  shiny::column(6L, shiny::uiOutput('PDP_Variable4')))))

          ), # End of fluid row for the plot variable dropdowns

          # Dragula Input
          shiny::fluidRow(
            width = AppWidth,
            shiny::column(12L, shiny::uiOutput('PlotTypeDragula')))

          ), # End of Box

        # ----

        # ----

        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
        # Plotting Adjustments                 ----
        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

        # Add Space
        RemixAutoML::BlankRow(AppWidth),

        # Plot Type
        shinydashboard::box(
          title = htmltools::tagList(shiny::icon('filter', lib = 'font-awesome'), tags$b('Plot Enhancements')),
          solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE, background = 'blue', width = AppWidth,

          # All on same row
          shiny::fluidRow(

            # Create Plot!
            shiny::column(
              width = 3L, shinyjs::useShinyjs(),
              tags$h4(tags$b('~ Build Plot')), # tags$h4(tags$span(style='color: blue;', 'Build Plot')),
              shinyWidgets::actionBttn(inputId='TrendPlotExecute', label='Build Plot', style='gradient', color='royal')), # eval(CreatePlotButtonColor)

            # Reset Theme!
            # shiny::column(
            #   width = 2L, shinyjs::useShinyjs(),
            #   tags$h4(tags$span(style='color: blue;', 'Reset Plot')),
            #   shinyWidgets::actionBttn(inputId='ResetPlotThemeElements', label='Reset', icon=shiny::icon('chevron-right', lib = 'font-awesome'), style='gradient', color='default')), # color=eval(ResetPlotButtonColor)))),

            # Global Settings
            shiny::column(
              width = 2L,
              tags$h4(tags$b('Global Settings')),
              shinyWidgets::dropdown(
                right = FALSE, animate = TRUE, circle = FALSE, tooltip = FALSE, status = "warning", width = LogoWidth,
                tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Global Settings')),
                RemixAutoML::BlankRow(AppWidth),
                shiny::fluidRow(
                  width = AppWidth,
                  shiny::column(3L, shiny::uiOutput('AutoGridHorizontal'))),
                RemixAutoML::BlankRow(AppWidth),
                shiny::fluidRow(width = AppWidth, shiny::column(3L, shiny::uiOutput('PlotHeight'))),
                RemixAutoML::BlankRow(AppWidth),
                shiny::fluidRow(width = AppWidth, shiny::column(3L, shiny::uiOutput('PlotWidth'))),

                )), # End of global settings

            # # Plot Labels (col 3)
            # shiny::column(
            #   width = 1L,
            #   tags$h4('Formatting'),
            #   shinyWidgets::dropdown(
            #     right = FALSE, animate = TRUE, circle = FALSE, tooltip = FALSE, status = "primary", icon = icon("gear"), width = LogoWidth,
            #     tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Plot Formatting')),
            #     tags$h4(tags$span(style=paste0('color: ', H4Color, ';'),'For color options, see Plot Colors')),
            #     RemixAutoML::BlankRow(AppWidth),
            #     shiny::fluidRow(
            #       width = AppWidth,
            #       shiny::column(3L, shiny::uiOutput('YAxisLabel1')),
            #       shiny::column(3L, shiny::uiOutput('YAxisLabel2')),
            #       shiny::column(3L, shiny::uiOutput('YAxisLabel3')),
            #       shiny::column(3L, shiny::uiOutput('YAxisLabel4'))),
            #     shiny::fluidRow(
            #       width = AppWidth,
            #       shiny::column(3L, shiny::uiOutput('XAxisLabel1')),
            #       shiny::column(3L, shiny::uiOutput('XAxisLabel2')),
            #       shiny::column(3L, shiny::uiOutput('XAxisLabel3')),
            #       shiny::column(3L, shiny::uiOutput('XAxisLabel4'))),
            #     shiny::fluidRow(
            #       width = AppWidth,
            #       shiny::column(3L, shiny::uiOutput('Title1')),
            #       shiny::column(3L, shiny::uiOutput('Title2')),
            #       shiny::column(3L, shiny::uiOutput('Title3')),
            #       shiny::column(3L, shiny::uiOutput('Title4'))),
            #     shiny::fluidRow(
            #       width = AppWidth,
            #       shiny::column(3L, shiny::uiOutput('SubTitle1')),
            #       shiny::column(3L, shiny::uiOutput('SubTitle2')),
            #       shiny::column(3L, shiny::uiOutput('SubTitle3')),
            #       shiny::column(3L, shiny::uiOutput('SubTitle4')))
            #
            #
            #     )), # column end



            # Plot Format
            shiny::column(
              width = 2L,
              tags$h4(tags$b('Plot Formatting')),
              shinyWidgets::dropdown(
                right = FALSE, animate = TRUE, circle = FALSE, tooltip = FALSE, status = "success", inputId = "Plot-Formatting-Parent", width = LogoWidth,
                tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Plot Enhancements')),
                tags$h4(tags$span(style=paste0('color: ', H4Color, ';'),'Additional MetaData Selection for Plot Enhancements')),
                RemixAutoML::BlankRow(AppWidth),

                # Plot Formatting
                shiny::fluidRow(

                  # Plot Axis Limits
                  shiny::column(
                    width = 3L,
                    tags$h4(tags$b(tags$span(style=paste0('color: ', H3Color, ';'), 'Axis Limits'))),
                    shinyWidgets::dropdown(
                      right = FALSE, animate = TRUE, circle = FALSE, tooltip = FALSE, status = "info", inputId = "Axis-Limits", width = LogoWidth,
                      tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Plot Limits')),
                      tags$h4(tags$span(style=paste0('color: ', H4Color, ';'),'Alter the min and max limits for the Y and X Axes')),
                      RemixAutoML::BlankRow(AppWidth),
                      shiny::fluidRow(
                        width = AppWidth,
                        shiny::column(3L, shiny::uiOutput('YLimMin1')),
                        shiny::column(3L, shiny::uiOutput('YLimMax1')),
                        shiny::column(3L, shiny::uiOutput('XLimMin1')),
                        shiny::column(3L, shiny::uiOutput('XLimMax1'))),
                      shiny::fluidRow(
                        width = AppWidth,
                        shiny::column(3L, shiny::uiOutput('YLimMin2')),
                        shiny::column(3L, shiny::uiOutput('YLimMax2')),
                        shiny::column(3L, shiny::uiOutput('XLimMin2')),
                        shiny::column(3L, shiny::uiOutput('XLimMax2'))),
                      shiny::fluidRow(
                        width = AppWidth,
                        shiny::column(3L, shiny::uiOutput('YLimMin3')),
                        shiny::column(3L, shiny::uiOutput('YLimMax3')),
                        shiny::column(3L, shiny::uiOutput('XLimMin3')),
                        shiny::column(3L, shiny::uiOutput('XLimMax3'))),
                      shiny::fluidRow(
                        width = AppWidth,
                        shiny::column(3L, shiny::uiOutput('YLimMin4')),
                        shiny::column(3L, shiny::uiOutput('YLimMax4')),
                        shiny::column(3L, shiny::uiOutput('XLimMin4')),
                        shiny::column(3L, shiny::uiOutput('XLimMax4'))))), # column end

                  # Plot Formatting
                  shiny::column(
                    width = 3L,
                    tags$h4(tags$b(tags$span(style=paste0('color: ', H3Color, ';'), 'Plot Structure'))),
                    shinyWidgets::dropdown(
                      right = TRUE, animate = TRUE, circle = FALSE, tooltip = FALSE, status = "info", inputId = "Plot-Structure", width = LogoWidth,
                      tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Plot Formatting')),
                      tags$h4(tags$span(style=paste0('color: ', H4Color, ';'),'For color options, see Plot Colors')),
                      RemixAutoML::BlankRow(AppWidth),
                      shiny::fluidRow(
                        width = AppWidth,
                        shiny::column(3L, shiny::uiOutput('SampleSize1')),
                        shiny::column(3L, shiny::uiOutput('SampleSize2')),
                        shiny::column(3L, shiny::uiOutput('SampleSize3')),
                        shiny::column(3L, shiny::uiOutput('SampleSize4'))),
                      shiny::fluidRow(
                        width = AppWidth,
                        shiny::column(3L, shiny::uiOutput('AngleY1')),
                        shiny::column(3L, shiny::uiOutput('AngleY2')),
                        shiny::column(3L, shiny::uiOutput('AngleY3')),
                        shiny::column(3L, shiny::uiOutput('AngleY4'))),
                      shiny::fluidRow(
                        width = AppWidth,
                        shiny::column(3L, shiny::uiOutput('AngleX1')),
                        shiny::column(3L, shiny::uiOutput('AngleX2')),
                        shiny::column(3L, shiny::uiOutput('AngleX3')),
                        shiny::column(3L, shiny::uiOutput('AngleX4'))),
                      shiny::fluidRow(
                        width = AppWidth,
                        shiny::column(3L, shiny::uiOutput('YTicks1')),
                        shiny::column(3L, shiny::uiOutput('YTicks2')),
                        shiny::column(3L, shiny::uiOutput('YTicks3')),
                        shiny::column(3L, shiny::uiOutput('YTicks4'))),
                      shiny::fluidRow(
                        width = AppWidth,
                        shiny::column(3L, shiny::uiOutput('XTicks1')),
                        shiny::column(3L, shiny::uiOutput('XTicks2')),
                        shiny::column(3L, shiny::uiOutput('XTicks3')),
                        shiny::column(3L, shiny::uiOutput('XTicks4'))),
                      shiny::fluidRow(
                        shiny::column(3L, shiny::uiOutput('TextSize1')),
                        shiny::column(3L, shiny::uiOutput('TextSize2')),
                        shiny::column(3L, shiny::uiOutput('TextSize3')),
                        shiny::column(3L, shiny::uiOutput('TextSize4'))),
                      shiny::fluidRow(
                        shiny::column(3L, shiny::uiOutput('OutlierSize1')),
                        shiny::column(3L, shiny::uiOutput('OutlierSize2')),
                        shiny::column(3L, shiny::uiOutput('OutlierSize3')),
                        shiny::column(3L, shiny::uiOutput('OutlierSize4'))),
                      shiny::fluidRow(
                        shiny::column(3L, shiny::uiOutput('LegendPosition1')),
                        shiny::column(3L, shiny::uiOutput('LegendPosition2')),
                        shiny::column(3L, shiny::uiOutput('LegendPosition3')),
                        shiny::column(3L, shiny::uiOutput('LegendPosition4'))),
                      shiny::fluidRow(
                        shiny::column(3L, shiny::uiOutput('LegendLineType1')),
                        shiny::column(3L, shiny::uiOutput('LegendLineType2')),
                        shiny::column(3L, shiny::uiOutput('LegendLineType3')),
                        shiny::column(3L, shiny::uiOutput('LegendLineType4'))),

                      )), # column end and dropdown end

                  # Plot Colors (col 3)
                  shiny::column(
                    width = 3L,
                    tags$h4(tags$b(tags$span(style=paste0('color: ', H3Color, ';'), 'Plot Colors'))),
                    shinyWidgets::dropdown(
                      right = TRUE, animate = TRUE, circle = FALSE, tooltip = FALSE, status = "info", inputId = "Plot-Colors", width = LogoWidth,
                      tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Plot Coloring')),
                      RemixAutoML::BlankRow(AppWidth),
                      shiny::fluidRow(
                        width = AppWidth,
                        shiny::column(3L, shiny::uiOutput('BackGroundColor1')),
                        shiny::column(3L, shiny::uiOutput('BackGroundColor2')),
                        shiny::column(3L, shiny::uiOutput('BackGroundColor3')),
                        shiny::column(3L, shiny::uiOutput('BackGroundColor4'))),
                      shiny::fluidRow(
                        width = AppWidth,
                        shiny::column(3L, shiny::uiOutput('SubTitleColor1')),
                        shiny::column(3L, shiny::uiOutput('SubTitleColor2')),
                        shiny::column(3L, shiny::uiOutput('SubTitleColor3')),
                        shiny::column(3L, shiny::uiOutput('SubTitleColor4'))),
                      shiny::fluidRow(
                        width = AppWidth,
                        shiny::column(3L, shiny::uiOutput('ChartColor1')),
                        shiny::column(3L, shiny::uiOutput('ChartColor2')),
                        shiny::column(3L, shiny::uiOutput('ChartColor3')),
                        shiny::column(3L, shiny::uiOutput('ChartColor4'))),
                      shiny::fluidRow(
                        width = AppWidth,
                        shiny::column(3L, shiny::uiOutput('BorderColor1')),
                        shiny::column(3L, shiny::uiOutput('BorderColor2')),
                        shiny::column(3L, shiny::uiOutput('BorderColor3')),
                        shiny::column(3L, shiny::uiOutput('BorderColor4'))),
                      shiny::fluidRow(
                        width = AppWidth,
                        shiny::column(3L, shiny::uiOutput('TextColor1')),
                        shiny::column(3L, shiny::uiOutput('TextColor2')),
                        shiny::column(3L, shiny::uiOutput('TextColor3')),
                        shiny::column(3L, shiny::uiOutput('TextColor4'))),
                      shiny::fluidRow(
                        width = AppWidth,
                        shiny::column(3L, shiny::uiOutput('FillColor1')),
                        shiny::column(3L, shiny::uiOutput('FillColor2')),
                        shiny::column(3L, shiny::uiOutput('FillColor3')),
                        shiny::column(3L, shiny::uiOutput('FillColor4'))),
                      shiny::fluidRow(
                        width = AppWidth,
                        shiny::column(3L, shiny::uiOutput('OutlierColor1')),
                        shiny::column(3L, shiny::uiOutput('OutlierColor2')),
                        shiny::column(3L, shiny::uiOutput('OutlierColor3')),
                        shiny::column(3L, shiny::uiOutput('OutlierColor4'))),
                      shiny::fluidRow(
                        width = AppWidth,
                        shiny::column(3L, shiny::uiOutput('GridColor1')),
                        shiny::column(3L, shiny::uiOutput('GridColor2')),
                        shiny::column(3L, shiny::uiOutput('GridColor3')),
                        shiny::column(3L, shiny::uiOutput('GridColor4')))

                      ))), # column end

                # Add Space
                RemixAutoML::BlankRow(AppWidth),

                # Plot Enhancements
                shiny::fluidRow(

                  # Gam Fitting
                  shiny::column(
                    width = 3L,
                    tags$h4(tags$b(tags$span(style=paste0('color: ', H3Color, ';'), 'GAM Line'))),
                    shinyWidgets::dropdown(
                      right = FALSE, animate = TRUE, circle = FALSE, tooltip = FALSE, status = "warning", inputId = "GamRegressionLine", width = LogoWidth,
                      tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'GAM Regression Lines')),
                      tags$h4(tags$span(style=paste0('color: ', H4Color, ';'),'Add a generalized additive model regression fit to the data')),
                      RemixAutoML::BlankRow(AppWidth),
                      shiny::fluidRow(
                        width = AppWidth,
                        shiny::column(3L, shiny::uiOutput('GamFitScatter1')),
                        shiny::column(3L, shiny::uiOutput('GamFitScatter2')),
                        shiny::column(3L, shiny::uiOutput('GamFitScatter3')),
                        shiny::column(3L, shiny::uiOutput('GamFitScatter4'))))),

                  # Histogram bins
                  shiny::column(
                    width = 3L,
                    tags$h4(tags$span(style=paste0('color: ', H3Color, ';'), tags$b('Hist Bins'))),
                    shinyWidgets::dropdown(
                      right = FALSE, animate = TRUE, circle = FALSE, tooltip = FALSE, status = "warning", inputId = "Histograms-Bins", width = LogoWidth,
                      tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Histogram # Bins')),
                      tags$h4(tags$span(style=paste0('color: ', H4Color, ';'),'Choose the number of bins for the histogram')),
                      RemixAutoML::BlankRow(AppWidth),
                      shiny::fluidRow(
                        width = AppWidth,
                        shiny::column(3L, shiny::uiOutput('NumberBins1')),
                        shiny::column(3L, shiny::uiOutput('NumberBins2')),
                        shiny::column(3L, shiny::uiOutput('NumberBins3')),
                        shiny::column(3L, shiny::uiOutput('NumberBins4'))))),

                  # Percentile Bins
                  shiny::column(
                    width = 3L,
                    tags$h4(tags$b(tags$span(style=paste0('color: ', H3Color, ';'), 'Eval Bins'))),
                    shinyWidgets::dropdown(
                      right = TRUE, animate = TRUE, circle = FALSE, tooltip = FALSE, status = "warning", inputId = "Percentile-Bins", width = LogoWidth,
                      tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Calibration Plot Bins')),
                      tags$h4(tags$span(style=paste0('color: ', H4Color, ';'),'Choose the number of bins for the relevant model output plots')),
                      RemixAutoML::BlankRow(AppWidth),
                      shiny::fluidRow(
                        width = AppWidth,
                        shiny::column(3L, shiny::uiOutput('Percentile_Buckets1')),
                        shiny::column(3L, shiny::uiOutput('Percentile_Buckets2')),
                        shiny::column(3L, shiny::uiOutput('Percentile_Buckets3')),
                        shiny::column(3L, shiny::uiOutput('Percentile_Buckets4'))))),

                  # Shapely Aggregation Method
                  shiny::column(
                    width = 3L,
                    tags$h4(tags$span(style=paste0('color: ', H3Color, ';'), tags$b('Shapley Agg'))),
                    shinyWidgets::dropdown(
                      right = TRUE, animate = TRUE, circle = FALSE, tooltip = FALSE, status = "warning", inputId = "Shapely-Agg", width = LogoWidth,
                      tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Calibration Plot Bins')),
                      tags$h4(tags$span(style=paste0('color: ', H4Color, ';'),'Choose the number of bins for the relevant model output plots')),
                      RemixAutoML::BlankRow(AppWidth),
                      shiny::fluidRow(
                        width = AppWidth,
                        shiny::column(3L, shiny::uiOutput('ShapAggMethod1')),
                        shiny::column(3L, shiny::uiOutput('ShapAggMethod2')),
                        shiny::column(3L, shiny::uiOutput('ShapAggMethod3')),
                        shiny::column(3L, shiny::uiOutput('ShapAggMethod4')))))

                  ))), # end of column, end of dropdown, end of column, end of single fluid row



            # Filters for Subsetting Data or Model Data
            shiny::column(
              width = 1L,
              tags$h4(tags$b('Data Filters')),
              shinyWidgets::dropdown(
                right = TRUE, animate = TRUE, circle = FALSE, tooltip = FALSE, status = "success", width = LogoWidth,
                tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Filter Variables, Logic, and Values')),
                tags$h4(tags$span(style=paste0('color: ', H4Color, ';'),'Filters for subsetting data')),
                RemixAutoML::BlankRow(12L),

                # One fluid row for this dropdown
                shiny::fluidRow(

                  # Plot 1 Filter Variables
                  shiny::column(
                    width = 3L,
                    tags$h4(tags$b(tags$span(style=paste0('color: ', H3Color, ';'), 'Filters 1'))),
                    shinyWidgets::dropdown(
                      right = FALSE, animate = TRUE, circle = FALSE, tooltip = FALSE, status = "success", inputId = "Filters-P1", width = LogoWidth,
                      tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Filters for Plot 1')),
                      tags$h4(tags$span(style=paste0('color: ', H4Color, ';'),'Subset data using up to four variables')),
                      RemixAutoML::BlankRow(AppWidth),
                      shiny::fluidRow(
                        width = 12L,
                        shiny::column(3L, shiny::uiOutput('FilterVariable_1_1')),
                        shiny::column(3L, shiny::uiOutput('FilterVariable_1_2')),
                        shiny::column(3L, shiny::uiOutput('FilterVariable_1_3')),
                        shiny::column(3L, shiny::uiOutput('FilterVariable_1_4'))),
                      shiny::fluidRow(
                        width = 13L,
                        shiny::column(3L, shiny::uiOutput('FilterLogic_1_1')),
                        shiny::column(3L, shiny::uiOutput('FilterLogic_1_2')),
                        shiny::column(3L, shiny::uiOutput('FilterLogic_1_3')),
                        shiny::column(3L, shiny::uiOutput('FilterLogic_1_4'))),
                      shiny::fluidRow(
                        width = 13L,
                        shiny::column(3L, shiny::uiOutput('FilterValue_1_1_1')),
                        shiny::column(3L, shiny::uiOutput('FilterValue_1_2_1')),
                        shiny::column(3L, shiny::uiOutput('FilterValue_1_3_1')),
                        shiny::column(3L, shiny::uiOutput('FilterValue_1_4_1'))),
                      shiny::fluidRow(
                        width = 13L,
                        shiny::column(3L, shiny::uiOutput('FilterValue_1_1_2')),
                        shiny::column(3L, shiny::uiOutput('FilterValue_1_2_2')),
                        shiny::column(3L, shiny::uiOutput('FilterValue_1_3_2')),
                        shiny::column(3L, shiny::uiOutput('FilterValue_1_4_2'))))),


                  # Plot 2 Filter Variables
                  shiny::column(
                    width = 3L,
                    tags$h4(tags$b(tags$span(style=paste0('color: ', H3Color, ';'), 'Filters 2'))),
                    shinyWidgets::dropdown(
                      right = FALSE, animate = TRUE, circle = FALSE, tooltip = FALSE, status = "success", inputId = "Filters-P2", width = LogoWidth,
                      tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Filters for Plot 2')),
                      tags$h4(tags$span(style=paste0('color: ', H4Color, ';'),'Subset data using up to four variables')),
                      RemixAutoML::BlankRow(AppWidth),
                      shiny::fluidRow(
                        width = AppWidth,
                        shiny::column(3L, shiny::uiOutput('FilterVariable_2_1')),
                        shiny::column(3L, shiny::uiOutput('FilterVariable_2_2')),
                        shiny::column(3L, shiny::uiOutput('FilterVariable_2_3')),
                        shiny::column(3L, shiny::uiOutput('FilterVariable_2_4'))),
                      shiny::fluidRow(
                        width = AppWidth,
                        shiny::column(3L, shiny::uiOutput('FilterLogic_2_1')),
                        shiny::column(3L, shiny::uiOutput('FilterLogic_2_2')),
                        shiny::column(3L, shiny::uiOutput('FilterLogic_2_3')),
                        shiny::column(3L, shiny::uiOutput('FilterLogic_2_4'))),
                      shiny::fluidRow(
                        width = AppWidth,
                        shiny::column(3L, shiny::uiOutput('FilterValue_2_1_1')),
                        shiny::column(3L, shiny::uiOutput('FilterValue_2_2_1')),
                        shiny::column(3L, shiny::uiOutput('FilterValue_2_3_1')),
                        shiny::column(3L, shiny::uiOutput('FilterValue_2_4_1'))),
                      shiny::fluidRow(
                        width = AppWidth,
                        shiny::column(3L, shiny::uiOutput('FilterValue_2_1_2')),
                        shiny::column(3L, shiny::uiOutput('FilterValue_2_2_2')),
                        shiny::column(3L, shiny::uiOutput('FilterValue_2_3_2')),
                        shiny::column(3L, shiny::uiOutput('FilterValue_2_4_2'))))), # column end

                  # Plot 3 Filter Variables
                  shiny::column(
                    width = 3L,
                    tags$h4(tags$b(tags$span(style=paste0('color: ', H3Color, ';'), 'Filters 3'))),
                    shinyWidgets::dropdown(
                      right = FALSE, animate = TRUE, circle = FALSE, tooltip = FALSE, status = "success", inputId = "Filters-P3", width = LogoWidth,
                      tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Filters for Plot 3')),
                      tags$h4(tags$span(style=paste0('color: ', H4Color, ';'),'Subset data using up to four variables')),
                      RemixAutoML::BlankRow(AppWidth),
                      shiny::fluidRow(
                        width = AppWidth,
                        shiny::column(3L, shiny::uiOutput('FilterVariable_3_1')),
                        shiny::column(3L, shiny::uiOutput('FilterVariable_3_2')),
                        shiny::column(3L, shiny::uiOutput('FilterVariable_3_3')),
                        shiny::column(3L, shiny::uiOutput('FilterVariable_3_4'))),
                      shiny::fluidRow(
                        width = AppWidth,
                        shiny::column(3L, shiny::uiOutput('FilterLogic_3_1')),
                        shiny::column(3L, shiny::uiOutput('FilterLogic_3_2')),
                        shiny::column(3L, shiny::uiOutput('FilterLogic_3_3')),
                        shiny::column(3L, shiny::uiOutput('FilterLogic_3_4'))),
                      shiny::fluidRow(
                        width = AppWidth,
                        shiny::column(3L, shiny::uiOutput('FilterValue_3_1_1')),
                        shiny::column(3L, shiny::uiOutput('FilterValue_3_2_1')),
                        shiny::column(3L, shiny::uiOutput('FilterValue_3_3_1')),
                        shiny::column(3L, shiny::uiOutput('FilterValue_3_4_1'))),
                      shiny::fluidRow(
                        width = AppWidth,
                        shiny::column(3L, shiny::uiOutput('FilterValue_3_1_2')),
                        shiny::column(3L, shiny::uiOutput('FilterValue_3_2_2')),
                        shiny::column(3L, shiny::uiOutput('FilterValue_3_3_2')),
                        shiny::column(3L, shiny::uiOutput('FilterValue_3_4_2'))))), # column end

                  # Plot 4 Filter Variables
                  shiny::column(
                    width = 3L,
                    tags$h4(tags$b(tags$span(style=paste0('color: ', H3Color, ';'), 'Filters 4'))),
                    shinyWidgets::dropdown(
                      right = TRUE, animate = TRUE, circle = FALSE, tooltip = FALSE, status = "success", inputId = "Filters-P4", width = LogoWidth,
                      tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Filters for Plot 4')),
                      tags$h4(tags$span(style=paste0('color: ', H4Color, ';'),'Subset data using up to four variables')),
                      RemixAutoML::BlankRow(AppWidth),
                      shiny::fluidRow(
                        width = AppWidth,
                        shiny::column(3L, shiny::uiOutput('FilterVariable_4_1')),
                        shiny::column(3L, shiny::uiOutput('FilterVariable_4_2')),
                        shiny::column(3L, shiny::uiOutput('FilterVariable_4_3')),
                        shiny::column(3L, shiny::uiOutput('FilterVariable_4_4'))),
                      shiny::fluidRow(
                        width = AppWidth,
                        shiny::column(3L, shiny::uiOutput('FilterLogic_4_1')),
                        shiny::column(3L, shiny::uiOutput('FilterLogic_4_2')),
                        shiny::column(3L, shiny::uiOutput('FilterLogic_4_3')),
                        shiny::column(3L, shiny::uiOutput('FilterLogic_4_4'))),
                      shiny::fluidRow(
                        width = AppWidth,
                        shiny::column(3L, shiny::uiOutput('FilterValue_4_1_1')),
                        shiny::column(3L, shiny::uiOutput('FilterValue_4_2_1')),
                        shiny::column(3L, shiny::uiOutput('FilterValue_4_3_1')),
                        shiny::column(3L, shiny::uiOutput('FilterValue_4_4_1'))),
                      shiny::fluidRow(
                        width = AppWidth,
                        shiny::column(3L, shiny::uiOutput('FilterValue_4_1_2')),
                        shiny::column(3L, shiny::uiOutput('FilterValue_4_2_2')),
                        shiny::column(3L, shiny::uiOutput('FilterValue_4_3_2')),
                        shiny::column(3L, shiny::uiOutput('FilterValue_4_4_2'))))) # column end


                  )  # fluidrow end
                )   # dropdown end
              )),  # Column end  and fluidrow end before plot display

          # Add extra space at bottom of box; acts like a border
          RemixAutoML::BlankRow(AppWidth),

        ), # box end

        # ----

        # ----

        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
        # Show Plot                            ----
        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

        # Add Space
        RemixAutoML::BlankRow(AppWidth),
        shiny::fluidRow(shiny::column(width = AppWidth, shiny::plotOutput('Trend')))),

      # ----

      # ----

      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
      # Code Print                           ----
      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
      shinydashboard::tabItem(

        # -- TAB REFERENCE VALUE
        tabName = "CodePrint",

        # Print Code!
        shiny::fluidRow(
          shiny::column(
            width = 3L, shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = 'PrintCodeButton',
              label = 'Print Code',
              icon = shiny::icon('chevron-right', lib = 'font-awesome'),
              style = 'gradient',
              color = eval(CreatePlotButtonColor)))),

        # Add Space
        RemixAutoML::BlankRow(AppWidth),

        # Print Code
        shiny::fluidRow(shiny::column(width = AppWidth, shiny::htmlOutput('PrintCode'))))

      ) # Close tab items
    ) # Close dashboard body
  ) # finishes up UI

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# Server Code                          ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
server <- function(input, output, session) {

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Enable Page Load                     ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  # Button to disable / enable Data Load Page
  shinyjs::addCssClass(selector = "a[data-value='LoadDataPage']", class = "inactiveLink")
  shinyjs::addCssClass(selector = "a[data-value='Plotter']", class = "inactiveLink")
  shinyjs::addCssClass(selector = "a[data-value='CodePrint']", class = "inactiveLink")

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Login and Page Navigation            ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  # Inputs
  UserName <- shiny::reactive({input$UserName})
  Password <- shiny::reactive({input$Password})

  # Login
  shiny::observeEvent(eventExpr = input$Check_Credentials, {
    if(UserName() %in% Credentials$UserName && Password() %in% Credentials[UserName == eval(UserName())]$Password) {
      shinyjs::removeCssClass(selector = "a[data-value='LoadDataPage']", class = "inactiveLink")
      shinyjs::removeCssClass(selector = "a[data-value='Plotter']", class = "inactiveLink")
      shinyjs::removeCssClass(selector = "a[data-value='CodePrint']", class = "inactiveLink")
      shinyWidgets::sendSweetAlert(session, title = NULL, text = 'Success', type = NULL, btn_labels = "success", btn_colors = "green", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {
      shinyjs::addCssClass(selector = "a[data-value='LoadDataPage']", class = "inactiveLink")
      shinyjs::addCssClass(selector = "a[data-value='Plotter']", class = "inactiveLink")
      shinyjs::addCssClass(selector = "a[data-value='CodePrint']", class = "inactiveLink")
      shinyWidgets::sendSweetAlert(session, title = NULL, text = 'Username and / or password is incorrect', type = NULL, btn_labels = "error", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  })

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Load Data and Initialize Vars        ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  # IFrame holder of C# app
  output$IFrame <- renderUI({
    tags$iframe(src=IFrameLocation, style='width:60vw;height:50vh;')
  })

  # Load data event
  shiny::observeEvent(eventExpr = input$LoadDataButton, {

    # ----

    # ----

    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    # Load Data Sets and Rdata             ----
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

    # Local data loading
    if(Debug) print('data check 1')
    CodeCollection <- list()
    data <<- RemixAutoML::ReactiveLoadCSV(Infile = input[['DataLoad']], ProjectList = NULL, DateUpdateName = NULL, RemoveObjects = NULL, Debug = Debug)
    print("::::::::::::::::::::::::::::::::::: DATA IS LOADED NOW ::::::::::::::::::::::::::::::::::::::::::")
    print(data)

    # Load ModelOutputList
    if(Debug) print('data check 2')
    inFile1 <- tryCatch({input[['ModelObjectLoad']]}, error = function(x) NULL)
    if(Debug) print(inFile1)
    pp <- c(as.list(environment()))
    if(!is.null(inFile1)) {
      if(Debug) print('loading .Rdata')
      e <- new.env()
      name <- load(inFile1[['datapath']], e)
      if(Debug) print('store ModelOutputList globally')
      ModelOutputList <<- e[[name]]
      if(Debug) print(class(ModelOutputList))
      if(Debug) print(length(ModelOutputList))
      if(Debug) print(length(ModelOutputList$PlotList))
      if(Debug) print(names(ModelOutputList$PlotList))
    } else {
      if(Debug) print('ModelOutputList not loaded')
      ModelOutputList <<- NULL
    }

    # Azure data loading
    if(Debug) print(input[['blob']])
    inFile2 <- tryCatch({input[['blob']]}, error = function(x) NULL)
    if(Debug) print(inFile2)
    if(length(inFile2) != 0 && inFile2 != "Load" && inFile2 != "") {
      if(Debug) {print('data check 3'); print(input[['blob']])}
      if(!is.null(inFile2)) {
        if(grepl(pattern = '.csv', x = inFile2)) {
          download.file(url=paste0(BlobStorageURL, inFile2), destfile = file.path(system.file(package = 'RemixAutoML'), 'tests/CSVs/data.csv'))
          data <<- data.table::fread(file.path(system.file(package = 'RemixAutoML'), 'tests/CSVs/data.csv'))
        } else {
          download.file(url=paste0(BlobStorageURL, inFile2), destfile = file.path(system.file(package = 'RemixAutoML'), 'tests/CSVs/data.Rdata'))
          e <- new.env()
          name <- load(file.path(system.file(package = 'RemixAutoML'), 'tests/CSVs/data.Rdata'), e)
          ModelOutputList <<- e[[name]]
        }
      }
    }
    CodeCollection <<- CodeCollection

    # ----

    # ----

    # EXACT COPIES EXCEPT FILTERS          ----

    # EXACT COPIES EXCEPT FILTERS          ----

    # EXACT COPIES EXCEPT FILTERS          ----

    # ----

    # ----

    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    # Variables                            ----
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

    # YVars
    if(Debug) print("Here a")
    output$YVar1 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'YVar1', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Y-Variable 1'), Choices = c('None', names(data)), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here b")
    output$YVar2 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'YVar2', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Y-Variable 2'), Choices = c('None', names(data)), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here c")
    output$YVar3 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'YVar3', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Y-Variable 3'), Choices = c('None', names(data)), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here d")
    output$YVar4 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'YVar4', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Y-Variable 4'), Choices = c('None', names(data)), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    # 'X-Variables'
    if(Debug) print("Here e")
    output$XVar1 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'XVar1', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'X-Variable 1'), Choices = c('None', names(data)), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here f")
    output$XVar2 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'XVar2', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'X-Variable 2'), Choices = c('None', names(data)), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here g")
    output$XVar3 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'XVar3', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'X-Variable 3'), Choices = c('None', names(data)), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here h")
    output$XVar4 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'XVar4', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'X-Variable 4'), Choices = c('None', names(data)), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    # 'Score-Variables'
    if(Debug) print("Here i")
    output$ScoreVar1 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'ScoreVar1', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Scoring-Variable 1'), Choices = c('None', names(data)), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here j")
    output$ScoreVar2 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'ScoreVar2', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Scoring-Variable 2'), Choices = c('None', names(data)), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here k")
    output$ScoreVar3 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'ScoreVar3', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Scoring-Variable 3'), Choices = c('None', names(data)), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here l")
    output$ScoreVar4 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'ScoreVar4', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Scoring-Variable 4'), Choices = c('None', names(data)), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    # 'Date Variables'
    if(Debug) print("Here m")
    output$DateVar1 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'DateVar1', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Date-Variable 1'), Choices = c('None', names(data)), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here n")
    output$DateVar2 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'DateVar2', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Date-Variable 2'), Choices = c('None', names(data)), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here o")
    output$DateVar3 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'DateVar3', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Date-Variable 3'), Choices = c('None', names(data)), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here p")
    output$DateVar4 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'DateVar4', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Date-Variable 4'), Choices = c('None', names(data)), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here q")

    # Reactives References
    YVar1 <- shiny::reactive({shiny::req(input[['YVar1']])})
    YVar2 <- shiny::reactive({shiny::req(input[['YVar2']])})
    YVar3 <- shiny::reactive({shiny::req(input[['YVar3']])})
    YVar4 <- shiny::reactive({shiny::req(input[['YVar4']])})
    XVar1 <- shiny::reactive({shiny::req(input[['XVar1']])})
    XVar2 <- shiny::reactive({shiny::req(input[['XVar2']])})
    XVar3 <- shiny::reactive({shiny::req(input[['XVar3']])})
    XVar4 <- shiny::reactive({shiny::req(input[['XVar4']])})
    ScoreVar1 <- shiny::reactive({shiny::req(input[['ScoreVar1']])})
    ScoreVar2 <- shiny::reactive({shiny::req(input[['ScoreVar2']])})
    ScoreVar3 <- shiny::reactive({shiny::req(input[['ScoreVar3']])})
    ScoreVar4 <- shiny::reactive({shiny::req(input[['ScoreVar4']])})
    DateVar1 <- shiny::reactive({shiny::req(input[['DateVar1']])})
    DateVar2 <- shiny::reactive({shiny::req(input[['DateVar2']])})
    DateVar3 <- shiny::reactive({shiny::req(input[['DateVar3']])})
    DateVar4 <- shiny::reactive({shiny::req(input[['DateVar4']])})

    if(Debug) print("Here r")

    # ----

    # ----

    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    # Plotting MetaData                    ----
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

    if(Debug) print("Auto Size Grid")

    # Auto SCaling of Plot Grid: doubles the size in the event of more than 1 plot
    output$AutoGridHorizontal <-  shiny::renderUI({
      shinyWidgets::materialSwitch(inputId = "AutoGridHorizontal", label = tags$span(style='color: blue;', 'Auto Grid Scale'), status = "danger", value = TRUE, inline = TRUE, width = '100%')
    })

    if(Debug) print("Here qq")

    output$Plot1 <- shiny::renderUI({
      if(exists('ModelOutputList') && length(ModelOutputList) != 0) {
        bla <- names(ModelOutputList$PlotList)
        if(!is.null(ModelOutputList$TrainData) && !is.null(ModelOutputList$TestData)) {
          ModelData <<- data.table::rbindlist(list(ModelOutputList$TrainData, ModelOutputList$TestData))
        } else if(is.null(ModelOutputList$TrainData) && !is.null(ModelOutputList$TestData)) {
          ModelData <<- ModelOutputList$TestData
        } else {
          ModelData <<- NULL
        }
      } else {
        bla <- NULL
        ModelData <<- NULL
      }
      x <- RemixAutoML:::AvailableAppInsightsPlots(x = bla, PlotNamesLookup = PlotNamesLookup)
      RemixAutoML::SelectizeInput(InputID = 'Plot1', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Plot 1'), Choices = c(x), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$Plot2 <- shiny::renderUI({
      if(exists('ModelOutputList') && length(ModelOutputList) != 0 && exists('ModelData') && length(ModelData) != 0L) {
        bla <- names(ModelOutputList$PlotList)
      } else if(exists('ModelOutputList') && length(ModelOutputList) != 0 && !exists('ModelData')) {
        bla <- names(ModelOutputList$PlotList)
        if(!is.null(ModelOutputList$TrainData) && !is.null(ModelOutputList$TestData)) {
          ModelData <<- data.table::rbindlist(list(ModelOutputList$TrainData, ModelOutputList$TestData))
        } else if(is.null(ModelOutputList$TrainData) && !is.null(ModelOutputList$TestData)) {
          ModelData <<- ModelOutputList$TestData
        } else {
          ModelData <<- NULL
        }
      } else {
        bla <- NULL
        ModelData <<- NULL
      }
      x <- RemixAutoML:::AvailableAppInsightsPlots(x = bla, PlotNamesLookup = PlotNamesLookup)
      RemixAutoML::SelectizeInput(InputID = 'Plot2', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Plot 2'), Choices = c(x), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$Plot3 <- shiny::renderUI({
      if(exists('ModelOutputList') && length(ModelOutputList) != 0 && exists('ModelData') && length(ModelData) != 0L) {
        bla <- names(ModelOutputList$PlotList)
      } else if(exists('ModelOutputList') && length(ModelOutputList) != 0 && !exists('ModelData')) {
        bla <- names(ModelOutputList$PlotList)
        if(!is.null(ModelOutputList$TrainData) && !is.null(ModelOutputList$TestData)) {
          ModelData <<- data.table::rbindlist(list(ModelOutputList$TrainData, ModelOutputList$TestData))
        } else if(is.null(ModelOutputList$TrainData) && !is.null(ModelOutputList$TestData)) {
          ModelData <<- ModelOutputList$TestData
        } else {
          ModelData <<- NULL
        }
      } else {
        bla <- NULL
        ModelData <<- NULL
      }
      x <- RemixAutoML:::AvailableAppInsightsPlots(x = bla, PlotNamesLookup = PlotNamesLookup)
      RemixAutoML::SelectizeInput(InputID = 'Plot3', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Plot 3'), Choices = c(x), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$Plot4 <- shiny::renderUI({
      if(exists('ModelOutputList') && length(ModelOutputList) != 0 && exists('ModelData') && length(ModelData) != 0L) {
        bla <- names(ModelOutputList$PlotList)
      } else if(exists('ModelOutputList') && length(ModelOutputList) != 0 && !exists('ModelData')) {
        bla <- names(ModelOutputList$PlotList)
        if(!is.null(ModelOutputList$TrainData) && !is.null(ModelOutputList$TestData)) {
          ModelData <<- data.table::rbindlist(list(ModelOutputList$TrainData, ModelOutputList$TestData))
        } else if(is.null(ModelOutputList$TrainData) && !is.null(ModelOutputList$TestData)) {
          ModelData <<- ModelOutputList$TestData
        } else {
          ModelData <<- NULL
        }
      } else {
        bla <- NULL
        ModelData <<- NULL
      }
      x <- RemixAutoML:::AvailableAppInsightsPlots(x = bla, PlotNamesLookup = PlotNamesLookup)
      RemixAutoML::SelectizeInput(InputID = 'Plot4', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Plot 4'), Choices = c(x), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    # Dragula for PlotType
    output$PlotTypeDragula <- shiny::renderUI({
      dragheight <- '75px'
      esquisse::dragulaInput(
        height = dragheight,
        replace = TRUE,
        inputId = 'PlotTypeDragula',
        label = 'Drag and Drop Plot Types',
        sourceLabel = 'Plots',
        targetsLabels = c('UpperLeftPlot', 'BottomLeftPlot', 'UpperRightPlot', 'BottomRightPlot'),
        #style="color: #fff",# ; background-color: #e95420; border-color: #c34113; border-radius: 10px; border-width: 2px",
        choices = c('Plot1', 'Plot2', 'Plot3', 'Plot4')
        #,
        # choiceNames = , choiceValues = , selected = , status = , replace = , copySource = , badge = , ncolSource = , ncolGrid = , dragulaOpts = , boxStyle = , width = "100%", height = "100%"
      )
    })

    if(Debug) print("Here rr")

    # UI Plot Options
    output$YLimMin1 <- shiny::renderUI({
      RemixAutoML::TextInput(InputID = 'YLimMin1', Label = tags$span(style='color: blue;', 'Y Min Limit 1'), Value = NULL, Placeholder = 'Insert a number')
    })
    output$YLimMax1 <- shiny::renderUI({
      RemixAutoML::TextInput(InputID = 'YLimMax1', Label = tags$span(style='color: blue;', 'Y Max Limit 1'), Value = NULL, Placeholder = 'Insert a number')
    })
    output$YLimMin2 <- shiny::renderUI({
      RemixAutoML::TextInput(InputID = 'YLimMin2', Label = tags$span(style='color: blue;', 'Y Min Limit 2'), Value = NULL, Placeholder = 'Insert a number')
    })
    output$YLimMax2 <- shiny::renderUI({
      RemixAutoML::TextInput(InputID = 'YLimMax2', Label = tags$span(style='color: blue;', 'Y Max Limit 2'), Value = NULL, Placeholder = 'Insert a number')
    })
    output$YLimMin3 <- shiny::renderUI({
      RemixAutoML::TextInput(InputID = 'YLimMin3', Label = tags$span(style='color: blue;', 'Y Min Limit 3'), Value = NULL, Placeholder = 'Insert a number')
    })
    output$YLimMax3 <- shiny::renderUI({
      RemixAutoML::TextInput(InputID = 'YLimMax3', Label = tags$span(style='color: blue;', 'Y Max Limit 3'), Value = NULL, Placeholder = 'Insert a number')
    })
    output$YLimMin4 <- shiny::renderUI({
      RemixAutoML::TextInput(InputID = 'YLimMin4', Label = tags$span(style='color: blue;', 'Y Min Limit 4'), Value = NULL, Placeholder = 'Insert a number')
    })
    output$YLimMax4 <- shiny::renderUI({
      RemixAutoML::TextInput(InputID = 'YLimMax4', Label = tags$span(style='color: blue;', 'Y Max Limit 4'), Value = NULL, Placeholder = 'Insert a number')
    })
    output$XLimMin1 <- shiny::renderUI({
      RemixAutoML::TextInput(InputID = 'XLimMin1', Label = tags$span(style='color: blue;', 'X Min Limit 1'), Value = NULL, Placeholder = 'Insert a number')
    })
    output$XLimMax1 <- shiny::renderUI({
      RemixAutoML::TextInput(InputID = 'XLimMax1', Label = tags$span(style='color: blue;', 'X Max Limit 1'), Value = NULL, Placeholder = 'Insert a number')
    })
    output$XLimMin2 <- shiny::renderUI({
      RemixAutoML::TextInput(InputID = 'XLimMin2', Label = tags$span(style='color: blue;', 'X Min Limit 2'), Value = NULL, Placeholder = 'Insert a number')
    })
    output$XLimMax2 <- shiny::renderUI({
      RemixAutoML::TextInput(InputID = 'XLimMax2', Label = tags$span(style='color: blue;', 'X Max Limit 2'), Value = NULL, Placeholder = 'Insert a number')
    })
    output$XLimMin3 <- shiny::renderUI({
      RemixAutoML::TextInput(InputID = 'XLimMin3', Label = tags$span(style='color: blue;', 'X Min Limit 3'), Value = NULL, Placeholder = 'Insert a number')
    })
    output$XLimMax3 <- shiny::renderUI({
      RemixAutoML::TextInput(InputID = 'XLimMax3', Label = tags$span(style='color: blue;', 'X Max Limit 3'), Value = NULL, Placeholder = 'Insert a number')
    })
    output$XLimMin4 <- shiny::renderUI({
      RemixAutoML::TextInput(InputID = 'XLimMin4', Label = tags$span(style='color: blue;', 'X Min Limit 4'), Value = NULL, Placeholder = 'Insert a number')
    })
    output$XLimMax4 <- shiny::renderUI({
      RemixAutoML::TextInput(InputID = 'XLimMax4', Label = tags$span(style='color: blue;', 'X Max Limit 4'), Value = NULL, Placeholder = 'Insert a number')
    })

    # NumberGroupsDisplay
    output$NumberGroupsDisplay1 <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID = 'NumberGroupsDisplay1', Label = tags$span(style='color: blue;', 'Dispay N Levels'), Step = 1L, Value = 5L, Min = 1L, Max = 100L)
    })
    output$NumberGroupsDisplay2 <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID = 'NumberGroupsDisplay2', Label = tags$span(style='color: blue;', 'Dispay N Levels'), Step = 1L, Value = 5L, Min = 1L, Max = 100L)
    })
    output$NumberGroupsDisplay3 <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID = 'NumberGroupsDisplay3', Label = tags$span(style='color: blue;', 'Dispay N Levels'), Step = 1L, Value = 5L, Min = 1L, Max = 100L)
    })
    output$NumberGroupsDisplay4 <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID = 'NumberGroupsDisplay4', Label = tags$span(style='color: blue;', 'Dispay N Levels'), Step = 1L, Value = 5L, Min = 1L, Max = 100L)
    })

    if(Debug) print("Here ss")
    output$PlotWidth <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID = "PlotWidth", Label=tags$span(style='color: blue;', 'Plot Width'), Step = 50, Min = 800, Max = 1800, Value = 1600)
    })

    if(Debug) print("Here tt")

    output$PlotHeight <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID = "PlotHeight", Label=tags$span(style='color: blue;', 'Plot Height'), Step = 25, Min = 350, Max = 350*10, Value = 500)
    })

    # Correlation Variables
    if(Debug) print("Here uu")
    output$CorVariables1 <- shiny::renderUI({
      if(Debug) {
        print('::::::::::::::::::::::::::::::::::: THIS IS THE NEW PLACE TO LOOK :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::')
        print(!is.null(data) && any(RemixAutoML:::ColTypes(data) %in% c('numeric','integer')))
        print(!is.null(data))
        print(RemixAutoML:::ColTypes(data))
        print(names(data)[which(RemixAutoML:::ColTypes(data) %in% c('numeric','integer'))])
      }
      if(!is.null(data) && any(RemixAutoML:::ColTypes(data) %in% c('numeric','integer'))) {
        cor_choices <- names(data)[which(RemixAutoML:::ColTypes(data) %in% c('numeric','integer'))]
      } else {
        cor_choices <- NULL
      }
      RemixAutoML::SelectizeInput(InputID='CorVariables1', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Corr Variables 1'), Choices=c(cor_choices), SelectedDefault=NULL, Size=10, SelectedText="count > 1", Multiple=TRUE, ActionBox=TRUE)
    })

    if(Debug) print("Here vv")
    output$CorVariables2 <- shiny::renderUI({
      if(!is.null(data) && any(RemixAutoML:::ColTypes(data) %in% c('numeric','integer'))) {
        cor_choices <- names(data)[which(RemixAutoML:::ColTypes(data) %in% c('numeric','integer'))]
      } else {
        cor_choices <- NULL
      }
      RemixAutoML::SelectizeInput(InputID='CorVariables2', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Corr Variables 2'), Choices=c(cor_choices), SelectedDefault=NULL, Size=10, SelectedText="count > 1", Multiple=TRUE, ActionBox=TRUE)
    })

    if(Debug) print("Here ww")
    output$CorVariables3 <- shiny::renderUI({
      if(!is.null(data) && any(RemixAutoML:::ColTypes(data) %in% c('numeric','integer'))) {
        cor_choices <- names(data)[which(RemixAutoML:::ColTypes(data) %in% c('numeric','integer'))]
      } else {
        cor_choices <- NULL
      }
      RemixAutoML::SelectizeInput(InputID='CorVariables3', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Corr Variables 3'), Choices=c(cor_choices), SelectedDefault=NULL, Size=10, SelectedText="count > 1", Multiple=TRUE, ActionBox=TRUE)
    })

    if(Debug) print("Here xx")
    output$CorVariables4 <- shiny::renderUI({
      if(!is.null(data) && any(RemixAutoML:::ColTypes(data) %in% c('numeric','integer'))) {
        cor_choices <- names(data)[which(RemixAutoML:::ColTypes(data) %in% c('numeric','integer'))]
      } else {
        cor_choices <- NULL
      }
      RemixAutoML::SelectizeInput(InputID='CorVariables4', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Corr Variables 4'), Choices=c(cor_choices), SelectedDefault=NULL, Size=10, SelectedText="count > 1", Multiple=TRUE, ActionBox=TRUE)
    })

    # PDP Variables
    if(Debug) print("Here yy")
    output$PDP_Variable1 <- shiny::renderUI({
      pdp <- RemixAutoML:::PDPVar(ModelOutputList)
      RemixAutoML::SelectizeInput(InputID='PDP_Variable1', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'PDP Variable 1'), Choices=pdp$Names, SelectedDefault=pdp$Default, Size=10, SelectedText="count > 1", Multiple=FALSE, ActionBox=TRUE)
    })

    if(Debug) print("Here vv")
    output$PDP_Variable2 <- shiny::renderUI({
      pdp <- RemixAutoML:::PDPVar(ModelOutputList)
      RemixAutoML::SelectizeInput(InputID='PDP_Variable2', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'PDP Variable 2'), Choices=pdp$Names, SelectedDefault=pdp$Default, Size=10, SelectedText="count > 1", Multiple=FALSE, ActionBox=TRUE)
    })

    if(Debug) print("Here ww")
    output$PDP_Variable3 <- shiny::renderUI({
      pdp <- RemixAutoML:::PDPVar(ModelOutputList)
      RemixAutoML::SelectizeInput(InputID='PDP_Variable3', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'PDP Variable 3'), Choices=pdp$Names, SelectedDefault=pdp$Default, Size=10, SelectedText="count > 1", Multiple=FALSE, ActionBox=TRUE)
    })

    if(Debug) print("Here xx")
    output$PDP_Variable4 <- shiny::renderUI({
      pdp <- RemixAutoML:::PDPVar(ModelOutputList)
      RemixAutoML::SelectizeInput(InputID='PDP_Variable4', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'PDP Variable 4'), Choices=pdp$Names, SelectedDefault=pdp$Default, Size=10, SelectedText="count > 1", Multiple=FALSE, ActionBox=TRUE)
    })

    # Percentile Buckets
    if(Debug) print("Here yy")
    output$Percentile_Buckets1 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID='Percentile_Buckets1', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Percentile Buckets 1'), Choices=1:100, SelectedDefault=20, Size=10, SelectedText="count > 1", Multiple=FALSE, ActionBox=TRUE)
    })

    if(Debug) print("Here zz")
    output$Percentile_Buckets2 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID='Percentile_Buckets2', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Percentile Buckets 2'), Choices=1:100, SelectedDefault=20, Size=10, SelectedText= "count > 1", Multiple=FALSE, ActionBox=TRUE)
    })

    if(Debug) print("Here aaa")
    output$Percentile_Buckets3 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID='Percentile_Buckets3', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Percentile Buckets 3'), Choices=1:100, SelectedDefault=20, Size=10, SelectedText= "count > 1", Multiple=FALSE, ActionBox=TRUE)
    })

    if(Debug) print("Here bbb")
    output$Percentile_Buckets4 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID='Percentile_Buckets4', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Percentile Buckets 4'), Choices=1:100, SelectedDefault=20, Size=10, SelectedText= "count > 1", Multiple=FALSE, ActionBox=TRUE)
    })

    # Shapely Agg Method
    if(Debug) print("Here ccc")
    output$ShapAggMethod1 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID='ShapAggMethod1', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Shap Agg Method 1'), Choices=c('mean','absmean','meanabs','sd','median','absmedian','medianabs'), SelectedDefault='mean', Size=10, SelectedText= "count > 1", Multiple=FALSE, ActionBox=TRUE)
    })
    output$ShapAggMethod2 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID='ShapAggMethod2', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Shap Agg Method 2'), Choices=c('mean','absmean','meanabs','sd','median','absmedian','medianabs'), SelectedDefault='mean', Size=10, SelectedText= "count > 1", Multiple=FALSE, ActionBox=TRUE)
    })
    output$ShapAggMethod3 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID='ShapAggMethod3', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Shap Agg Method 3'), Choices=c('mean','absmean','meanabs','sd','median','absmedian','medianabs'), SelectedDefault='mean', Size=10, SelectedText= "count > 1", Multiple=FALSE, ActionBox=TRUE)
    })
    output$ShapAggMethod4 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID='ShapAggMethod4', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Shap Agg Method 4'), Choices=c('mean','absmean','meanabs','sd','median','absmedian','medianabs'), SelectedDefault='mean', Size=10, SelectedText= "count > 1", Multiple=FALSE, ActionBox=TRUE)
    })

    # Add GAM Fit to Plot
    if(Debug) print("Here cccasdfasdf")
    output$GamFitScatter1 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID='GamFitScatter1', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Fit Gam on Scatter or Copula 1'), Choices=c('TRUE','FALSE'), SelectedDefault=FALSE, Multiple=FALSE, ActionBox=TRUE)
    })

    if(Debug) print("Here ddd")
    output$GamFitScatter2 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID='GamFitScatter2', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Fit Gam on Scatter or Copula 2'), Choices=c('TRUE','FALSE'), SelectedDefault=FALSE, Multiple=FALSE, ActionBox=TRUE)
    })

    if(Debug) print("Here eee")
    output$GamFitScatter3 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID='GamFitScatter3', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Fit Gam on Scatter or Copula 3'), Choices=c('TRUE','FALSE'), SelectedDefault=FALSE, Multiple=FALSE, ActionBox=TRUE)
    })

    if(Debug) print("Here fff")
    output$GamFitScatter4 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID='GamFitScatter4', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Fit Gam on Scatter or Copula 4'), Choices=c('TRUE','FALSE'), SelectedDefault=FALSE, Multiple=FALSE, ActionBox=TRUE)
    })

    # Histogram Bins
    if(Debug) print("Here ggg")
    output$NumberBins1 <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID='NumberBins1', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'# of Bins for Histogram 1'), Min=1, Max=1000, Step=5, Value=30)
    })

    if(Debug) print("Here hhh")
    output$NumberBins2 <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID='NumberBins2', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'# of Bins for Histogram 2'), Min=1, Max=1000, Step=5, Value=30)
    })

    if(Debug) print("Here iii")
    output$NumberBins3 <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID='NumberBins3', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'# of Bins for Histogram 3'), Min=1, Max=1000, Step=5, Value=30)
    })

    if(Debug) print("Here jjj")
    output$NumberBins4 <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID='NumberBins4', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'# of Bins for Histogram 4'), Min=1, Max=1000, Step=5, Value=30)
    })

    # YTicks Values (NULL is whats handled by RemixAutoML:::YTicks())
    if(Debug) print("Here kkk")
    output$YTicks1 <- shiny::renderUI({
      yy <- tryCatch({YVar1()}, error = function(x) 'None')
      if(Debug) print(RemixAutoML:::XTicks(data, xvar=yy))
      if(yy %in% names(data)) temp <- data else temp <- ModelData
      RemixAutoML::SelectizeInput(InputID = 'YTicks1', Label = tags$span(style='color: blue;', 'Y-Axis 1 Ticks'), Choices = RemixAutoML:::YTicks(temp, yvar = yy), SelectedDefault = 'Default', Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })

    if(Debug) print("Here lll")
    output$YTicks2 <- shiny::renderUI({
      yy <- tryCatch({YVar2()}, error = function(x) 'None')
      if(Debug) print(RemixAutoML:::XTicks(data, xvar=yy))
      if(yy %in% names(data)) temp <- data else temp <- ModelData
      RemixAutoML::SelectizeInput(InputID = 'YTicks2', Label = tags$span(style='color: blue;', 'Y-Axis 2 Ticks'), Choices = RemixAutoML:::YTicks(temp, yvar = yy), SelectedDefault = 'Default', Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })

    if(Debug) print("Here mmm")
    output$YTicks3 <- shiny::renderUI({
      yy <- tryCatch({YVar3()}, error = function(x) 'None')
      if(Debug) print(RemixAutoML:::XTicks(data, xvar=yy))
      if(yy %in% names(data)) temp <- data else temp <- ModelData
      RemixAutoML::SelectizeInput(InputID = 'YTicks3', Label = tags$span(style='color: blue;', 'Y-Axis 3 Ticks'), Choices = RemixAutoML:::YTicks(temp, yvar = yy), SelectedDefault = 'Default', Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })

    if(Debug) print("Here nnn")
    output$YTicks4 <- shiny::renderUI({
      yy <- tryCatch({YVar4()}, error = function(x) 'None')
      if(Debug) print(RemixAutoML:::XTicks(data, xvar=yy))
      if(yy %in% names(data)) temp <- data else temp <- ModelData
      RemixAutoML::SelectizeInput(InputID = 'YTicks4', Label = tags$span(style='color: blue;', 'Y-Axis 4 Ticks'), Choices = RemixAutoML:::YTicks(temp, yvar = yy), SelectedDefault = 'Default', Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })

    # XTicks Values ('None' is whats handled by RemixAutoML:::XTicks())
    if(Debug) print("Here ooo")
    output$XTicks1 <- shiny::renderUI({
      xx <- tryCatch({XVar1()}, error = function(x) 'None')
      dd <- tryCatch({DateVar1()}, error = function(x) 'None')
      if(Debug) print(RemixAutoML:::XTicks(data, xvar=xx,datevar=dd))
      if(xx %in% names(data)) temp <- data else temp <- ModelData
      RemixAutoML::SelectizeInput(InputID = 'XTicks1', Label = tags$span(style='color: blue;', 'X-Axis 1 Ticks'), Choices = RemixAutoML:::XTicks(temp, xvar=xx,datevar=dd), SelectedDefault = 'Default', Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })

    if(Debug) print("Here ppp")
    output$XTicks2 <- shiny::renderUI({
      xx <- tryCatch({XVar2()}, error = function(x) 'None')
      dd <- tryCatch({DateVar2()}, error = function(x) 'None')
      if(Debug) print(RemixAutoML:::XTicks(data, xvar=xx,datevar=dd))
      if(xx %in% names(data)) temp <- data else temp <- ModelData
      RemixAutoML::SelectizeInput(InputID = 'XTicks2', Label = tags$span(style='color: blue;', 'X-Axis 2 Ticks'), Choices = RemixAutoML:::XTicks(temp, xvar=xx,datevar=dd), SelectedDefault = 'Default', Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })

    if(Debug) print("Here qqq")
    output$XTicks3 <- shiny::renderUI({
      xx <- tryCatch({XVar3()}, error = function(x) 'None')
      dd <- tryCatch({DateVar3()}, error = function(x) 'None')
      if(Debug) print(RemixAutoML:::XTicks(data, xvar=xx,datevar=dd))
      if(xx %in% names(data)) temp <- data else temp <- ModelData
      RemixAutoML::SelectizeInput(InputID = 'XTicks3', Label = tags$span(style='color: blue;', 'X-Axis 3 Ticks'), Choices = RemixAutoML:::XTicks(temp, xvar=xx,datevar=dd), SelectedDefault = 'Default', Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })

    if(Debug) print("Here rrr")
    output$XTicks4 <- shiny::renderUI({
      xx <- tryCatch({XVar4()}, error = function(x) 'None')
      dd <- tryCatch({DateVar4()}, error = function(x) 'None')
      if(Debug) print(RemixAutoML:::XTicks(data, xvar=xx,datevar=dd))
      if(xx %in% names(data)) temp <- data else temp <- ModelData
      RemixAutoML::SelectizeInput(InputID = 'XTicks4', Label = tags$span(style='color: blue;', 'X-Axis 4 Ticks'), Choices = RemixAutoML:::XTicks(temp, xvar=xx,datevar=dd), SelectedDefault = 'Default', Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })

    # Other values
    if(Debug) print("Here sss")
    output$SampleSize <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID = 'SampleSize', Label = tags$span(style='color: blue;', 'Sample size for plotting'), Step = 50000, Min = 0, Max = 1000000, Value = 100000)
    })

    if(Debug) print("Here sssa")
    output$AngleY1 <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID = 'AngleY1', Label = tags$span(style='color: blue;', 'Y-axis text angle'), Step = 5, Min = 0, Max = 360, Value = 0)
    })
    output$AngleY2 <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID = 'AngleY2', Label = tags$span(style='color: blue;', 'Y-axis text angle'), Step = 5, Min = 0, Max = 360, Value = 0)
    })
    output$AngleY3 <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID = 'AngleY3', Label = tags$span(style='color: blue;', 'Y-axis text angle'), Step = 5, Min = 0, Max = 360, Value = 0)
    })
    output$AngleY4 <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID = 'AngleY4', Label = tags$span(style='color: blue;', 'Y-axis text angle'), Step = 5, Min = 0, Max = 360, Value = 0)
    })

    if(Debug) print("Here ttt")
    output$AngleX1 <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID = 'AngleX1', Label = tags$span(style='color: blue;', 'X-axis text angle'), Step = 5, Min = 0, Max = 360, Value = 90)
    })
    output$AngleX2 <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID = 'AngleX2', Label = tags$span(style='color: blue;', 'X-axis text angle'), Step = 5, Min = 0, Max = 360, Value = 90)
    })
    output$AngleX3 <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID = 'AngleX3', Label = tags$span(style='color: blue;', 'X-axis text angle'), Step = 5, Min = 0, Max = 360, Value = 90)
    })
    output$AngleX4 <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID = 'AngleX4', Label = tags$span(style='color: blue;', 'X-axis text angle'), Step = 5, Min = 0, Max = 360, Value = 90)
    })

    if(Debug) print("Here uuu")
    output$TextSize1 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'TextSize1', Label = tags$span(style='color: blue;', 'Text Size'),Choices = c(as.character(seq(1,100,1))), SelectedDefault = '14', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TextSize2 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'TextSize2', Label = tags$span(style='color: blue;', 'Text Size'),Choices = c(as.character(seq(1,100,1))), SelectedDefault = '14', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TextSize3 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'TextSize3', Label = tags$span(style='color: blue;', 'Text Size'),Choices = c(as.character(seq(1,100,1))), SelectedDefault = '14', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TextSize4 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'TextSize4', Label = tags$span(style='color: blue;', 'Text Size'),Choices = c(as.character(seq(1,100,1))), SelectedDefault = '14', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here vvv")
    output$OutlierSize1 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'OutlierSize1', Label = tags$span(style='color: blue;', 'Outlier Size'), Choices = c(seq(0.01,2,0.01)), SelectedDefault = 0.01, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$OutlierSize2 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'OutlierSize2', Label = tags$span(style='color: blue;', 'Outlier Size'), Choices = c(seq(0.01,2,0.01)), SelectedDefault = 0.01, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$OutlierSize3 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'OutlierSize3', Label = tags$span(style='color: blue;', 'Outlier Size'), Choices = c(seq(0.01,2,0.01)), SelectedDefault = 0.01, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$OutlierSize4 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'OutlierSize4', Label = tags$span(style='color: blue;', 'Outlier Size'), Choices = c(seq(0.01,2,0.01)), SelectedDefault = 0.01, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here www")
    output$LegendPosition1 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'LegendPosition1', Label = tags$span(style='color: blue;', 'Legend Position'), Choices = c('bottom','left','right','top','none'), SelectedDefault = 'bottom', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$LegendPosition2 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'LegendPosition2', Label = tags$span(style='color: blue;', 'Legend Position'), Choices = c('bottom','left','right','top','none'), SelectedDefault = 'bottom', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$LegendPosition3 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'LegendPosition3', Label = tags$span(style='color: blue;', 'Legend Position'), Choices = c('bottom','left','right','top','none'), SelectedDefault = 'bottom', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$LegendPosition4 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'LegendPosition4', Label = tags$span(style='color: blue;', 'Legend Position'), Choices = c('bottom','left','right','top','none'), SelectedDefault = 'bottom', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here xxx")
    output$LegendBorderSize1 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'LegendBorderSize1', Label = tags$span(style='color: blue;', 'Legend Border Size'), Choices = c(as.character(seq(0.01,2,0.01))), SelectedDefault = as.character(0.01), Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$LegendBorderSize2 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'LegendBorderSize2', Label = tags$span(style='color: blue;', 'Legend Border Size'), Choices = c(as.character(seq(0.01,2,0.01))), SelectedDefault = as.character(0.01), Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$LegendBorderSize3 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'LegendBorderSize3', Label = tags$span(style='color: blue;', 'Legend Border Size'), Choices = c(as.character(seq(0.01,2,0.01))), SelectedDefault = as.character(0.01), Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$LegendBorderSize4 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'LegendBorderSize4', Label = tags$span(style='color: blue;', 'Legend Border Size'), Choices = c(as.character(seq(0.01,2,0.01))), SelectedDefault = as.character(0.01), Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here yyy")
    output$LegendLineType1 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'LegendLineType1', Label = tags$span(style='color: blue;', 'Legend Border Type'), Choices = c('solid','blank','dashed','dotdash','dotted','longlash','twodash'), SelectedDefault = 'solid', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$LegendLineType2 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'LegendLineType2', Label = tags$span(style='color: blue;', 'Legend Border Type'), Choices = c('solid','blank','dashed','dotdash','dotted','longlash','twodash'), SelectedDefault = 'solid', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$LegendLineType3 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'LegendLineType3', Label = tags$span(style='color: blue;', 'Legend Border Type'), Choices = c('solid','blank','dashed','dotdash','dotted','longlash','twodash'), SelectedDefault = 'solid', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$LegendLineType4 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'LegendLineType4', Label = tags$span(style='color: blue;', 'Legend Border Type'), Choices = c('solid','blank','dashed','dotdash','dotted','longlash','twodash'), SelectedDefault = 'solid', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    # Color boxes
    if(Debug) print("Here zzz")
    output$TextColor1 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'TextColor1', Label = tags$span(style='color: blue;', 'Text Color'), Choices = grDevices::colors(), SelectedDefault = 'darkblue', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TextColor2 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'TextColor2', Label = tags$span(style='color: blue;', 'Text Color'), Choices = grDevices::colors(), SelectedDefault = 'darkblue', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TextColor3 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'TextColor3', Label = tags$span(style='color: blue;', 'Text Color'), Choices = grDevices::colors(), SelectedDefault = 'darkblue', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TextColor4 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'TextColor4', Label = tags$span(style='color: blue;', 'Text Color'), Choices = grDevices::colors(), SelectedDefault = 'darkblue', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here aaaa")
    output$ChartColor1 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'ChartColor1', Label = tags$span(style='color: blue;', 'Chart Color'), Choices = grDevices::colors(), SelectedDefault = 'lightsteelblue1', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$ChartColor2 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'ChartColor2', Label = tags$span(style='color: blue;', 'Chart Color'), Choices = grDevices::colors(), SelectedDefault = 'lightsteelblue1', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$ChartColor3 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'ChartColor3', Label = tags$span(style='color: blue;', 'Chart Color'), Choices = grDevices::colors(), SelectedDefault = 'lightsteelblue1', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$ChartColor4 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'ChartColor4', Label = tags$span(style='color: blue;', 'Chart Color'), Choices = grDevices::colors(), SelectedDefault = 'lightsteelblue1', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here bbbb")
    output$GridColor1 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'GridColor1', Label = tags$span(style='color: blue;', 'Grid Lines Color'), Choices = grDevices::colors(), SelectedDefault = 'white', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$GridColor2 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'GridColor2', Label = tags$span(style='color: blue;', 'Grid Lines Color'), Choices = grDevices::colors(), SelectedDefault = 'white', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$GridColor3 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'GridColor3', Label = tags$span(style='color: blue;', 'Grid Lines Color'), Choices = grDevices::colors(), SelectedDefault = 'white', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$GridColor4 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'GridColor4', Label = tags$span(style='color: blue;', 'Grid Lines Color'), Choices = grDevices::colors(), SelectedDefault = 'white', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here cccc")
    output$BackGroundColor1 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'BackGroundColor1', Label = tags$span(style='color: blue;', 'Background Color'), Choices = grDevices::colors(), SelectedDefault = 'gray95', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$BackGroundColor2 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'BackGroundColor2', Label = tags$span(style='color: blue;', 'Background Color'), Choices = grDevices::colors(), SelectedDefault = 'gray95', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$BackGroundColor3 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'BackGroundColor3', Label = tags$span(style='color: blue;', 'Background Color'), Choices = grDevices::colors(), SelectedDefault = 'gray95', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$BackGroundColor4 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'BackGroundColor4', Label = tags$span(style='color: blue;', 'Background Color'), Choices = grDevices::colors(), SelectedDefault = 'gray95', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here dddd")
    output$BorderColor1 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'BorderColor1', Label = tags$span(style='color: blue;', 'Border Color'), Choices = grDevices::colors(), SelectedDefault = 'darkblue', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$BorderColor2 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'BorderColor2', Label = tags$span(style='color: blue;', 'Border Color'), Choices = grDevices::colors(), SelectedDefault = 'darkblue', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$BorderColor3 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'BorderColor3', Label = tags$span(style='color: blue;', 'Border Color'), Choices = grDevices::colors(), SelectedDefault = 'darkblue', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$BorderColor4 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'BorderColor4', Label = tags$span(style='color: blue;', 'Border Color'), Choices = grDevices::colors(), SelectedDefault = 'darkblue', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here eeee")
    output$OutlierColor1 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'OutlierColor1', Label = tags$span(style='color: blue;', 'Outlier Color'), Choices = grDevices::colors(), SelectedDefault = 'blue', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$OutlierColor2 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'OutlierColor2', Label = tags$span(style='color: blue;', 'Outlier Color'), Choices = grDevices::colors(), SelectedDefault = 'blue', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$OutlierColor3 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'OutlierColor3', Label = tags$span(style='color: blue;', 'Outlier Color'), Choices = grDevices::colors(), SelectedDefault = 'blue', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$OutlierColor4 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'OutlierColor4', Label = tags$span(style='color: blue;', 'Outlier Color'), Choices = grDevices::colors(), SelectedDefault = 'blue', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here ffff")
    output$FillColor1 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'FillColor1', Label = tags$span(style='color: blue;', 'BoxPlot Fill Color'), Choices = grDevices::colors(), SelectedDefault = 'gray', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$FillColor2 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'FillColor2', Label = tags$span(style='color: blue;', 'BoxPlot Fill Color'), Choices = grDevices::colors(), SelectedDefault = 'gray', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$FillColor3 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'FillColor3', Label = tags$span(style='color: blue;', 'BoxPlot Fill Color'), Choices = grDevices::colors(), SelectedDefault = 'gray', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$FillColor4 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'FillColor4', Label = tags$span(style='color: blue;', 'BoxPlot Fill Color'), Choices = grDevices::colors(), SelectedDefault = 'gray', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here gggg")
    output$SubTitleColor1 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'SubTitleColor1', Label = tags$span(style='color: blue;', 'Subtitle Color'), Choices = grDevices::colors(), SelectedDefault = 'blue', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$SubTitleColor2 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'SubTitleColor2', Label = tags$span(style='color: blue;', 'Subtitle Color'), Choices = grDevices::colors(), SelectedDefault = 'blue', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$SubTitleColor3 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'SubTitleColor3', Label = tags$span(style='color: blue;', 'Subtitle Color'), Choices = grDevices::colors(), SelectedDefault = 'blue', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$SubTitleColor4 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'SubTitleColor4', Label = tags$span(style='color: blue;', 'Subtitle Color'), Choices = grDevices::colors(), SelectedDefault = 'blue', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    # ----

    # ----

    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    # Group Variables                      ----
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

    if(Debug) print("Here hhhh")

    # Select GroupVars
    output$GroupVars1 <- shiny::renderUI({
      if(Debug) print('SelectizeInput GroupVars1')
      RemixAutoML::SelectizeInput(InputID='GroupVars1', Label=tags$span(style='color: blue;', 'Select Group Variables 1'), Choices=c('None',names(data)), SelectedDefault='None', SelectedText='count > 1', Multiple=TRUE, ActionBox=TRUE)
    })

    if(Debug) print("Here iiii")

    output$GroupVars2 <- shiny::renderUI({
      if(Debug) print('SelectizeInput GroupVars2')
      RemixAutoML::SelectizeInput(InputID='GroupVars2', Label=tags$span(style='color: blue;', 'Select Group Variables 2'), Choices=c('None',names(data)), SelectedDefault='None', SelectedText='count > 1', Multiple=TRUE, ActionBox=TRUE)
    })

    if(Debug) print("Here jjjj")

    output$GroupVars3 <- shiny::renderUI({
      if(Debug) print('SelectizeInput GroupVars3')
      RemixAutoML::SelectizeInput(InputID='GroupVars3', Label=tags$span(style='color: blue;', 'Select Group Variables 3'), Choices=c('None',names(data)), SelectedDefault='None', SelectedText='count > 1', Multiple=TRUE, ActionBox=TRUE)
    })

    if(Debug) print("Here kkkk")

    output$GroupVars4 <- shiny::renderUI({
      if(Debug) print('SelectizeInput GroupVars4')
      RemixAutoML::SelectizeInput(InputID='GroupVars4', Label=tags$span(style='color: blue;', 'Select Group Variables 4'), Choices=c('None',names(data)), SelectedDefault='None', SelectedText='count > 1', Multiple=TRUE, ActionBox=TRUE)
    })

    if(Debug) print("Here llll")

    # Reactive Group Variables
    SelectedGroups1 <- shiny::reactive({
      RemixAutoML::ReturnParam(input, VarName = 'GroupVars1', Default = 'None', Switch = TRUE, Type = 'character')
    })

    if(Debug) print("Here mmmm")

    SelectedGroups2 <- shiny::reactive({
      RemixAutoML::ReturnParam(input, VarName = 'GroupVars2', Default = 'None', Switch = TRUE, Type = 'character')
    })

    if(Debug) print("Here nnnn")

    SelectedGroups3 <- shiny::reactive({
      RemixAutoML::ReturnParam(input, VarName = 'GroupVars3', Default = 'None', Switch = TRUE, Type = 'character')
    })

    if(Debug) print("Here oooo")

    SelectedGroups4 <- shiny::reactive({
      RemixAutoML::ReturnParam(input, VarName = 'GroupVars4', Default = 'None', Switch = TRUE, Type = 'character')
    })

    if(Debug) print("Here pppp")

    # Group Levels
    output$Levels_1_1 <- shiny::renderUI({
      sgs <- RemixAutoML:::LevelValues(SelectedGroups1()); if(Debug) print('PickerInput_GetLevels 1_1')
      RemixAutoML::PickerInput_GetLevels2(input, DataExist=exists('data'), InputID='Levels_1_1', InputID2=sgs, Choices=RemixAutoML::UniqueLevels(input, data, 1L, GroupVars=sgs), NumGroupVar=1L, Size=9, SelectedText='count > 1', Multiple=TRUE, ActionBox=TRUE, SelectedDefault=NULL)
    })

    if(Debug) print("Here qqqq")

    output$Levels_1_2 <- shiny::renderUI({
      sgs <- RemixAutoML:::LevelValues(SelectedGroups1()); if(Debug) print('PickerInput_GetLevels 1_2')
      RemixAutoML::PickerInput_GetLevels2(input, DataExist=exists('data'), InputID='Levels_1_2', InputID2=sgs, Choices=RemixAutoML::UniqueLevels(input, data, 2L, GroupVars=sgs), NumGroupVar=2L, Size=9, SelectedText='count > 1', Multiple=TRUE, ActionBox=TRUE, SelectedDefault=NULL)
    })

    if(Debug) print("Here rrrr")

    output$Levels_1_3 <- shiny::renderUI({
      sgs <- RemixAutoML:::LevelValues(SelectedGroups1()); if(Debug) print('PickerInput_GetLevels 1_3')
      RemixAutoML::PickerInput_GetLevels2(input, DataExist=exists('data'), InputID='Levels_1_3', InputID2=sgs, Choices=RemixAutoML::UniqueLevels(input, data, 3L, GroupVars=sgs), NumGroupVar=3L, Size=9, SelectedText="count > 1", Multiple=TRUE, ActionBox=TRUE, SelectedDefault=NULL)
    })

    if(Debug) print("Here ssss")

    output$Levels_2_1 <- shiny::renderUI({
      sgs <- RemixAutoML:::LevelValues(SelectedGroups2()); if(Debug) print('PickerInput_GetLevels 2_1')
      RemixAutoML::PickerInput_GetLevels2(input, DataExist=exists('data'), InputID='Levels_2_1', InputID2=sgs, Choices=RemixAutoML::UniqueLevels(input, data, 1L, GroupVars=sgs), NumGroupVar=1L, Size=9, SelectedText='count > 1', Multiple=TRUE, ActionBox=TRUE, SelectedDefault=NULL)
    })

    if(Debug) print("Here tttt")

    output$Levels_2_2 <- shiny::renderUI({
      sgs <- RemixAutoML:::LevelValues(SelectedGroups2()); if(Debug) print('PickerInput_GetLevels 2_2')
      RemixAutoML::PickerInput_GetLevels2(input, DataExist=exists('data'), InputID='Levels_2_2', InputID2=sgs, Choices=RemixAutoML::UniqueLevels(input, data, 2L, GroupVars=sgs), NumGroupVar=2L, Size=9, SelectedText='count > 1', Multiple=TRUE, ActionBox=TRUE, SelectedDefault=NULL)
    })

    if(Debug) print("Here uuuu")

    output$Levels_2_3 <- shiny::renderUI({
      sgs <- RemixAutoML:::LevelValues(SelectedGroups2()); if(Debug) print('PickerInput_GetLevels 2_3')
      RemixAutoML::PickerInput_GetLevels2(input, DataExist=exists('data'), InputID='Levels_2_3', InputID2=sgs, Choices=RemixAutoML::UniqueLevels(input, data, 3L, GroupVars=sgs), NumGroupVar=3L, Size=9, SelectedText="count > 1", Multiple=TRUE, ActionBox=TRUE, SelectedDefault=NULL)
    })

    if(Debug) print("Here vvvv")

    output$Levels_3_1 <- shiny::renderUI({
      sgs <- RemixAutoML:::LevelValues(SelectedGroups3()); if(Debug) print('PickerInput_GetLevels 3_1')
      RemixAutoML::PickerInput_GetLevels2(input, DataExist=exists('data'), InputID='Levels_3_1', InputID2=sgs, Choices=RemixAutoML::UniqueLevels(input, data, 1L, GroupVars=sgs), NumGroupVar=1L, Size=9, SelectedText='count > 1', Multiple=TRUE, ActionBox=TRUE, SelectedDefault=NULL)
    })

    if(Debug) print("Here wwww")

    output$Levels_3_2 <- shiny::renderUI({
      sgs <- RemixAutoML:::LevelValues(SelectedGroups3()); if(Debug) print('PickerInput_GetLevels 3_2')
      RemixAutoML::PickerInput_GetLevels2(input, DataExist=exists('data'), InputID='Levels_3_2', InputID2=sgs, Choices=RemixAutoML::UniqueLevels(input, data, 2L, GroupVars=sgs), NumGroupVar=2L, Size=9, SelectedText='count > 1', Multiple=TRUE, ActionBox=TRUE, SelectedDefault=NULL)
    })

    if(Debug) print("Here xxxx")

    output$Levels_3_3 <- shiny::renderUI({
      sgs <- RemixAutoML:::LevelValues(SelectedGroups3()); if(Debug) print('PickerInput_GetLevels 3_3')
      RemixAutoML::PickerInput_GetLevels2(input, DataExist=exists('data'), InputID='Levels_3_3', InputID2=sgs, Choices=RemixAutoML::UniqueLevels(input, data, 3L, GroupVars=sgs), NumGroupVar=3L, Size=9, SelectedText="count > 1", Multiple=TRUE, ActionBox=TRUE, SelectedDefault=NULL)
    })

    if(Debug) print("Here yyyy")

    output$Levels_4_1 <- shiny::renderUI({
      sgs <- RemixAutoML:::LevelValues(SelectedGroups4()); if(Debug) print('PickerInput_GetLevels 4_1')
      RemixAutoML::PickerInput_GetLevels2(input, DataExist=exists('data'), InputID='Levels_4_1', InputID2=sgs, Choices=RemixAutoML::UniqueLevels(input, data, 1L, GroupVars=sgs), NumGroupVar=1L, Size=9, SelectedText='count > 1', Multiple=TRUE, ActionBox=TRUE, SelectedDefault=NULL)
    })

    if(Debug) print("Here zzzz")

    output$Levels_4_2 <- shiny::renderUI({
      sgs <- RemixAutoML:::LevelValues(SelectedGroups4()); if(Debug) print('PickerInput_GetLevels 4_2')
      RemixAutoML::PickerInput_GetLevels2(input, DataExist=exists('data'), InputID='Levels_4_2', InputID2=sgs, Choices=RemixAutoML::UniqueLevels(input, data, 2L, GroupVars=sgs), NumGroupVar=2L, Size=9, SelectedText='count > 1', Multiple=TRUE, ActionBox=TRUE, SelectedDefault=NULL)
    })

    if(Debug) print("Here aaaaa")

    output$Levels_4_3 <- shiny::renderUI({
      sgs <- RemixAutoML:::LevelValues(SelectedGroups4()); if(Debug) print('PickerInput_GetLevels 4_3')
      RemixAutoML::PickerInput_GetLevels2(input, DataExist=exists('data'), InputID='Levels_4_3', InputID2=sgs, Choices=RemixAutoML::UniqueLevels(input, data, 3L, GroupVars=sgs), NumGroupVar=3L, Size=9, SelectedText="count > 1", Multiple=TRUE, ActionBox=TRUE, SelectedDefault=NULL)
    })

    if(Debug) print("Here bbbbb")

    # Faceting
    output$FacetVar_1_1 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID='FacetVar_1_1', Label = tags$span(style='color: blue;', 'Facet Variable 1 1'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here ccccc")

    output$FacetVar_1_2 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID='FacetVar_1_2', Label = tags$span(style='color: blue;', 'Facet Variable 1 2'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here ddddd")

    output$FacetVar_2_1 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID='FacetVar_2_1', Label = tags$span(style='color: blue;', 'Facet Variable 2 1'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here eeeee")

    output$FacetVar_2_2 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID='FacetVar_2_2', Label = tags$span(style='color: blue;', 'Facet Variable 2 2'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here fffff")

    output$FacetVar_3_1 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID='FacetVar_3_1', Label = tags$span(style='color: blue;', 'Facet Variable 3 1'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here ggggg")

    output$FacetVar_3_2 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID='FacetVar_3_2', Label = tags$span(style='color: blue;', 'Facet Variable 3 2'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here hhhhh")

    output$FacetVar_4_1 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID='FacetVar_4_1', Label = tags$span(style='color: blue;', 'Facet Variable 4 1'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here iiiii")

    output$FacetVar_4_2 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID='FacetVar_4_2', Label = tags$span(style='color: blue;', 'Facet Variable 4 2'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here jjjjj")

    # Sizing
    output$SizeVar1 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'SizeVar1', Label = tags$span(style='color: blue;', 'Size Variable 1'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here kkkkk")

    output$SizeVar2 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'SizeVar2', Label = tags$span(style='color: blue;', 'Size Variable 2'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here lllll")

    output$SizeVar3 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'SizeVar3', Label = tags$span(style='color: blue;', 'Size Variable 3'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here mmmmm")

    output$SizeVar4 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'SizeVar4', Label = tags$span(style='color: blue;', 'Size Variable 4'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    # ----

    # ----

    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    # Filter Variables                     ----
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

    # Filter Variables
    output$FilterVariable_1_1 <- shiny::renderUI({
      if(!is.null(ModelData)) {
        ModelVars <- paste0('ModelVar-', names(ModelData))
      } else {
        ModelVars <- NULL
      }
      shiny::selectInput(inputId='FilterVariable_1_1', label = tags$span(style='color: blue;', 'Filter Variable 1 1'), choices=c('None', names(data), ModelVars), selected='None')
    })

    if(Debug) print("Here nnnnn")

    output$FilterVariable_1_2 <- shiny::renderUI({
      if(!is.null(ModelData)) {
        ModelVars <- paste0('ModelVar-', names(ModelData))
      } else {
        ModelVars <- NULL
      }
      shiny::selectInput(inputId='FilterVariable_1_2', label = tags$span(style='color: blue;', 'Filter Variable 1 2'), choices=c('None', names(data), ModelVars), selected='None')
    })

    if(Debug) print("Here ooooo")

    output$FilterVariable_1_3 <- shiny::renderUI({
      if(!is.null(ModelData)) {
        ModelVars <- paste0('ModelVar-', names(ModelData))
      } else {
        ModelVars <- NULL
      }
      shiny::selectInput(inputId='FilterVariable_1_3', label = tags$span(style='color: blue;', 'Filter Variable 1 3'), choices=c('None', names(data), ModelVars), selected='None')
    })

    if(Debug) print("Here ppppp")

    output$FilterVariable_1_4 <- shiny::renderUI({
      if(!is.null(ModelData)) {
        ModelVars <- paste0('ModelVar-', names(ModelData))
      } else {
        ModelVars <- NULL
      }
      shiny::selectInput(inputId='FilterVariable_1_4', label = tags$span(style='color: blue;', 'Filter Variable 1 4'), choices=c('None', names(data), ModelVars), selected='None')
    })

    if(Debug) print("Here qqqqq")

    output$FilterVariable_2_1 <- shiny::renderUI({
      if(!is.null(ModelData)) {
        ModelVars <- paste0('ModelVar-', names(ModelData))
      } else {
        ModelVars <- NULL
      }
      shiny::selectInput(inputId='FilterVariable_2_1', label = tags$span(style='color: blue;', 'Filter Variable 2 1'), choices=c('None', names(data), ModelVars), selected='None')
    })

    if(Debug) print("Here rrrrr")

    output$FilterVariable_2_2 <- shiny::renderUI({
      if(!is.null(ModelData)) {
        ModelVars <- paste0('ModelVar-', names(ModelData))
      } else {
        ModelVars <- NULL
      }
      shiny::selectInput(inputId='FilterVariable_2_2', label = tags$span(style='color: blue;', 'Filter Variable 2 2'), choices=c('None', names(data), ModelVars), selected='None')
    })

    if(Debug) print("Here sssss")

    output$FilterVariable_2_3 <- shiny::renderUI({
      if(!is.null(ModelData)) {
        ModelVars <- paste0('ModelVar-', names(ModelData))
      } else {
        ModelVars <- NULL
      }
      shiny::selectInput(inputId='FilterVariable_2_3', label = tags$span(style='color: blue;', 'Filter Variable 2 3'), choices=c('None', names(data), ModelVars), selected='None')
    })

    if(Debug) print("Here ttttt")

    output$FilterVariable_2_4 <- shiny::renderUI({
      if(!is.null(ModelData)) {
        ModelVars <- paste0('ModelVar-', names(ModelData))
      } else {
        ModelVars <- NULL
      }
      shiny::selectInput(inputId='FilterVariable_2_4', label = tags$span(style='color: blue;', 'Filter Variable 2 4'), choices=c('None', names(data), ModelVars), selected='None')
    })

    if(Debug) print("Here uuuuu")

    output$FilterVariable_3_1 <- shiny::renderUI({
      if(!is.null(ModelData)) {
        ModelVars <- paste0('ModelVar-', names(ModelData))
      } else {
        ModelVars <- NULL
      }
      shiny::selectInput(inputId='FilterVariable_3_1', label = tags$span(style='color: blue;', 'Filter Variable 3 1'), choices=c('None', names(data), ModelVars), selected='None')
    })

    if(Debug) print("Here vvvvv")

    output$FilterVariable_3_2 <- shiny::renderUI({
      if(!is.null(ModelData)) {
        ModelVars <- paste0('ModelVar-', names(ModelData))
      } else {
        ModelVars <- NULL
      }
      shiny::selectInput(inputId='FilterVariable_3_2', label = tags$span(style='color: blue;', 'Filter Variable 3 2'), choices=c('None', names(data), ModelVars), selected='None')
    })

    if(Debug) print("Here wwwww")

    output$FilterVariable_3_3 <- shiny::renderUI({
      if(!is.null(ModelData)) {
        ModelVars <- paste0('ModelVar-', names(ModelData))
      } else {
        ModelVars <- NULL
      }
      shiny::selectInput(inputId='FilterVariable_3_3', label = tags$span(style='color: blue;', 'Filter Variable 3 3'), choices=c('None', names(data), ModelVars), selected='None')
    })

    if(Debug) print("Here xxxxx")

    output$FilterVariable_3_4 <- shiny::renderUI({
      if(!is.null(ModelData)) {
        ModelVars <- paste0('ModelVar-', names(ModelData))
      } else {
        ModelVars <- NULL
      }
      shiny::selectInput(inputId='FilterVariable_3_4', label = tags$span(style='color: blue;', 'Filter Variable 3 4'), choices=c('None', names(data), ModelVars), selected='None')
    })

    if(Debug) print("Here yyyyy")

    output$FilterVariable_4_1 <- shiny::renderUI({
      if(!is.null(ModelData)) {
        ModelVars <- paste0('ModelVar-', names(ModelData))
      } else {
        ModelVars <- NULL
      }
      shiny::selectInput(inputId='FilterVariable_4_1', label = tags$span(style='color: blue;', 'Filter Variable 4 1'), choices=c('None', names(data), ModelVars), selected='None')
    })

    if(Debug) print("Here zzzzz")

    output$FilterVariable_4_2 <- shiny::renderUI({
      if(!is.null(ModelData)) {
        ModelVars <- paste0('ModelVar-', names(ModelData))
      } else {
        ModelVars <- NULL
      }
      shiny::selectInput(inputId='FilterVariable_4_2', label = tags$span(style='color: blue;', 'Filter Variable 4 2'), choices=c('None', names(data), ModelVars), selected='None')
    })

    if(Debug) print("Here aaaaaa")

    output$FilterVariable_4_3 <- shiny::renderUI({
      if(!is.null(ModelData)) {
        ModelVars <- paste0('ModelVar-', names(ModelData))
      } else {
        ModelVars <- NULL
      }
      shiny::selectInput(inputId='FilterVariable_4_3', label = tags$span(style='color: blue;', 'Filter Variable 4 3'), choices=c('None', names(data), ModelVars), selected='None')
    })

    if(Debug) print("Here bbbbbb")

    output$FilterVariable_4_4 <- shiny::renderUI({
      if(!is.null(ModelData)) {
        ModelVars <- paste0('ModelVar-', names(ModelData))
      } else {
        ModelVars <- NULL
      }
      shiny::selectInput(inputId='FilterVariable_4_4', label = tags$span(style='color: blue;', 'Filter Variable 4 4'), choices=c('None', names(data), ModelVars), selected='None')
    })

    # Reactives References
    FilterVariable_1_1 <- shiny::reactive({shiny::req(input[['FilterVariable_1_1']])})
    FilterVariable_1_2 <- shiny::reactive({shiny::req(input[['FilterVariable_1_2']])})
    FilterVariable_1_3 <- shiny::reactive({shiny::req(input[['FilterVariable_1_3']])})
    FilterVariable_1_4 <- shiny::reactive({shiny::req(input[['FilterVariable_1_4']])})
    FilterVariable_2_1 <- shiny::reactive({shiny::req(input[['FilterVariable_2_1']])})
    FilterVariable_2_2 <- shiny::reactive({shiny::req(input[['FilterVariable_2_2']])})
    FilterVariable_2_3 <- shiny::reactive({shiny::req(input[['FilterVariable_2_3']])})
    FilterVariable_2_4 <- shiny::reactive({shiny::req(input[['FilterVariable_2_4']])})
    FilterVariable_3_1 <- shiny::reactive({shiny::req(input[['FilterVariable_3_1']])})
    FilterVariable_3_2 <- shiny::reactive({shiny::req(input[['FilterVariable_3_2']])})
    FilterVariable_3_3 <- shiny::reactive({shiny::req(input[['FilterVariable_3_3']])})
    FilterVariable_3_4 <- shiny::reactive({shiny::req(input[['FilterVariable_3_4']])})
    FilterVariable_4_1 <- shiny::reactive({shiny::req(input[['FilterVariable_4_1']])})
    FilterVariable_4_2 <- shiny::reactive({shiny::req(input[['FilterVariable_4_2']])})
    FilterVariable_4_3 <- shiny::reactive({shiny::req(input[['FilterVariable_4_3']])})
    FilterVariable_4_4 <- shiny::reactive({shiny::req(input[['FilterVariable_4_4']])})

    # ----

    # ----

    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    # Filter Logic                         ----
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

    if(Debug) print("Here cccccc")

    # Filter Logics
    output$FilterLogic_1_1 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_1_1', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_1_1()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })

    if(Debug) print("Here dddddd")

    output$FilterLogic_1_2 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_1_2', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_1_2()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })

    if(Debug) print("Here eeeeee")

    output$FilterLogic_1_3 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_1_3', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_1_3()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })

    if(Debug) print("Here ffffff")

    output$FilterLogic_1_4 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_1_4', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_1_4()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })

    if(Debug) print("Here gggggg")

    output$FilterLogic_2_1 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_2_1', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_2_1()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })

    if(Debug) print("Here hhhhhh")

    output$FilterLogic_2_2 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_2_2', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_2_2()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })

    if(Debug) print("Here iiiiii")

    output$FilterLogic_2_3 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_2_3', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_2_3()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })

    if(Debug) print("Here jjjjjj")

    output$FilterLogic_2_4 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_2_4', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_2_4()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })

    if(Debug) print("Here kkkkkk")

    output$FilterLogic_3_1 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_3_1', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_3_1()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })

    if(Debug) print("Here llllll")

    output$FilterLogic_3_2 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_3_2', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_3_2()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })

    if(Debug) print("Here mmmmmm")

    output$FilterLogic_3_3 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_3_3', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_3_3()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })

    if(Debug) print("Here nnnnnn")

    output$FilterLogic_3_4 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_3_4', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_3_4()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })

    if(Debug) print("Here oooooo")

    output$FilterLogic_4_1 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_4_1', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_4_1()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })

    if(Debug) print("Here pppppp")

    output$FilterLogic_4_2 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_4_2', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_4_2()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })

    if(Debug) print("Here qqqqqq")

    output$FilterLogic_4_3 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_4_3', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_4_3()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })

    if(Debug) print("Here rrrrrr")

    output$FilterLogic_4_4 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_4_4', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_4_4()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })

    # ----

    # ----

    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    # Filter Values     DONT OVERWRITE     ----
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

    if(Debug) print("Here ssssss")

    # Filter Values
    #
    # 1_1_1 means Plot 1, Filter Var 1, Filter Value min
    # 1_1_2 means Plot 1, Filter Var 1, filter value max
    #
    # Plot 1
    output$FilterValue_1_1_1 <- shiny::renderUI({
      if(Debug) {print('AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA'); print('WHY IS FilterVariable_1_1 coming back NULL?'); print(input[['FilterVariable_1_1']]); print(data[1:3])}
      if(Debug) {print(tryCatch({input[['FilterVariable_1_1']]}, error = function(x) NULL)); print(tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_1_1']])$ChoiceInput}, error = function(x) NULL))}
      params <- list(data, VarName=tryCatch({input[['FilterVariable_1_1']]}, error = function(x) NULL), type=1)
      choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_1_1']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_1_1']])
      if(length(choices) == 0) {
        if(length(ModelData) != 0) {
          if(Debug) print("HERE SIR")
          vname <- tryCatch({stringr::str_remove(string = input[['FilterVariable_1_1']], pattern = 'ModelVar-')}, error = function(x) 'None')
          params <- list(data=ModelData, VarName=vname, type=1)
          choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
          Mult <- RemixAutoML:::GetFilterValueMultiple(ModelData, VarName=vname)
        }
      }
      if(Debug) {print(choices); print(do.call(RemixAutoML::GetFilterValueLabel, params))}
      RemixAutoML::SelectizeInput(Multiple=Mult, InputID='FilterValue_1_1_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })

    if(Debug) print("Here tttttt")

    output$FilterValue_1_1_2 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_1_1']]}, error = function(x) NULL), type=2)
      choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_1_1']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data=data, VarName=input[['FilterVariable_1_1']])
      if(length(choices) == 0) {
        if(length(ModelData) != 0) {
          vname <- tryCatch({stringr::str_remove(string = input[['FilterVariable_1_1']], pattern = 'ModelVar-')}, error = function(x) 'None')
          params <- list(data=ModelData, VarName=vname, type=2)
          choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
          Mult <- RemixAutoML:::GetFilterValueMultiple(ModelData, VarName=vname)
        }
      }
      if(Debug) {print(choices); print(do.call(RemixAutoML::GetFilterValueLabel, params))}
      RemixAutoML::SelectizeInput(Multiple=Mult, InputID='FilterValue_1_1_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })

    if(Debug) print("Here uuuuuu")

    output$FilterValue_1_2_1 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_1_2']]}, error = function(x) NULL), type=1)
      choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_1_2']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_1_2']])
      if(length(choices) == 0) {
        if(length(ModelData) != 0) {
          if(Debug) print("HERE SIR")
          vname <- tryCatch({stringr::str_remove(string = input[['FilterVariable_1_2']], pattern = 'ModelVar-')}, error = function(x) 'None')
          params <- list(data=ModelData, VarName=vname, type=1)
          choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
          Mult <- RemixAutoML:::GetFilterValueMultiple(ModelData, VarName=vname)
        }
      }
      if(Debug) {print(choices); print(do.call(RemixAutoML::GetFilterValueLabel, params))}
      RemixAutoML::SelectizeInput(Multiple=Mult, InputID='FilterValue_1_2_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })

    if(Debug) print("Here vvvvvv")

    output$FilterValue_1_2_2 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_1_2']]}, error = function(x) NULL), type=2)
      choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_1_2']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_1_2']])
      if(length(choices) == 0) {
        if(length(ModelData) != 0) {
          vname <- tryCatch({stringr::str_remove(string = input[['FilterVariable_1_2']], pattern = 'ModelVar-')}, error = function(x) 'None')
          params <- list(data=ModelData, VarName=vname, type=2)
          choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
          Mult <- RemixAutoML:::GetFilterValueMultiple(ModelData, VarName=vname)
        }
      }
      if(Debug) {print(choices); print(do.call(RemixAutoML::GetFilterValueLabel, params))}
      RemixAutoML::SelectizeInput(Multiple=Mult, InputID='FilterValue_1_2_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })

    if(Debug) print("Here wwwwww")

    output$FilterValue_1_3_1 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_1_3']]}, error = function(x) NULL), type=1)
      choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_1_3']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_1_3']])
      if(length(choices) == 0) {
        if(length(ModelData) != 0) {
          vname <- tryCatch({stringr::str_remove(input[['FilterVariable_1_3']], 'ModelVar-')}, error = function(x) NULL)
          params <- list(data=ModelData, VarName=vname, type=1)
          choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
          Mult <- RemixAutoML:::GetFilterValueMultiple(ModelData, VarName=vname)
        }
      }
      if(Debug) print(choices)
      if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_1_3_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })

    if(Debug) print("Here xxxxxx")

    output$FilterValue_1_3_2 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_1_3']]}, error = function(x) NULL), type=2)
      choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_1_3']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_1_3']])
      if(length(choices) == 0) {
        if(length(ModelData) != 0) {
          vname <- tryCatch({stringr::str_remove(input[['FilterVariable_1_3']], 'ModelVar-')}, error = function(x) NULL)
          params <- list(data=ModelData, VarName=vname, type=2)
          choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
          Mult <- RemixAutoML:::GetFilterValueMultiple(ModelData, VarName=vname)
        }
      }
      if(Debug) print(choices)
      if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_1_3_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })

    if(Debug) print("Here yyyyyy")

    output$FilterValue_1_4_1 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_1_4']]}, error = function(x) NULL), type=1)
      choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_1_4']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_1_4']])
      if(length(choices) == 0) {
        if(length(ModelData) != 0) {
          vname <- tryCatch({stringr::str_remove(input[['FilterVariable_1_4']], 'ModelVar-')}, error = function(x) NULL)
          params <- list(data=ModelData, VarName=vname, type=1)
          choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
          Mult <- RemixAutoML:::GetFilterValueMultiple(ModelData, VarName=vname)
        }
      }
      if(Debug) print(choices)
      if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_1_4_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })

    if(Debug) print("Here zzzzzz")

    output$FilterValue_1_4_2 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_1_4']]}, error = function(x) NULL), type=2)
      choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_1_4']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_1_4']])
      if(length(choices) == 0) {
        if(length(ModelData) != 0) {
          vname <- tryCatch({stringr::str_remove(input[['FilterVariable_1_4']], 'ModelVar-')}, error = function(x) NULL)
          params <- list(data=ModelData, VarName=vname, type=2)
          choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
          Mult <- RemixAutoML:::GetFilterValueMultiple(ModelData, VarName=vname)
        }
      }
      if(Debug) print(choices)
      if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
      RemixAutoML::SelectizeInput(Multiple=Mult, InputID='FilterValue_1_4_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })

    if(Debug) print("Here aaaaaaa")

    # Plot 2
    output$FilterValue_2_1_1 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_2_1']]}, error = function(x) NULL), type=1)
      choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_2_1']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_2_1']])
      if(length(choices) == 0) {
        if(length(ModelData) != 0) {
          vname <- tryCatch({stringr::str_remove(input[['FilterVariable_2_1']], 'ModelVar-')}, error = function(x) NULL)
          params <- list(data=ModelData, VarName=vname, type=1)
          choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
          Mult <- RemixAutoML:::GetFilterValueMultiple(ModelData, VarName=vname)
        }
      }
      if(Debug) print(choices)
      if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_2_1_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })

    if(Debug) print("Here bbbbbbb")

    output$FilterValue_2_1_2 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_2_1']]}, error = function(x) NULL), type=2)
      choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_2_1']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_2_1']])
      if(length(choices) == 0) {
        if(length(ModelData) != 0) {
          vname <- tryCatch({stringr::str_remove(input[['FilterVariable_2_1']], 'ModelVar-')}, error = function(x) NULL)
          params <- list(data=ModelData, VarName=vname, type=2)
          choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
          Mult <- RemixAutoML:::GetFilterValueMultiple(ModelData, VarName=vname)
        }
      }
      if(Debug) print(choices)
      if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_2_1_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })

    if(Debug) print("Here ccccccc")

    output$FilterValue_2_2_1 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_2_2']]}, error = function(x) NULL), type=1)
      choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_2_2']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_2_2']])
      if(length(choices) == 0) {
        if(length(ModelData) != 0) {
          vname <- tryCatch({stringr::str_remove(input[['FilterVariable_2_2']], 'ModelVar-')}, error = function(x) NULL)
          params <- list(data=ModelData, VarName=vname, type=1)
          choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
          Mult <- RemixAutoML:::GetFilterValueMultiple(ModelData, VarName=vname)
        }
      }
      if(Debug) print(choices)
      if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_2_2_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })

    if(Debug) print("Here ddddddd")

    output$FilterValue_2_2_2 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_2_2']]}, error = function(x) NULL), type=2)
      choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_2_2']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_2_2']])
      if(length(choices) == 0) {
        if(length(ModelData) != 0) {
          vname <- tryCatch({stringr::str_remove(input[['FilterVariable_2_2']], 'ModelVar-')}, error = function(x) NULL)
          params <- list(data=ModelData, VarName=vname, type=2)
          choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
          Mult <- RemixAutoML:::GetFilterValueMultiple(ModelData, VarName=vname)
        }
      }
      if(Debug) print(choices)
      if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_2_2_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })

    if(Debug) print("Here eeeeeee")

    output$FilterValue_2_3_1 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_2_3']]}, error = function(x) NULL), type=1)
      choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_2_3']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_2_3']])
      if(length(choices) == 0) {
        if(length(ModelData) != 0) {
          vname <- tryCatch({stringr::str_remove(input[['FilterVariable_2_3']], 'ModelVar-')}, error = function(x) NULL)
          params <- list(data=ModelData, VarName=vname, type=1)
          choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
          Mult <- RemixAutoML:::GetFilterValueMultiple(ModelData, VarName=vname)
        }
      }
      if(Debug) print(choices)
      if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_2_3_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })

    if(Debug) print("Here fffffff")

    output$FilterValue_2_3_2 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_2_3']]}, error = function(x) NULL), type=2)
      choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_2_3']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_2_3']])
      if(length(choices) == 0) {
        if(length(ModelData) != 0) {
          vname <- tryCatch({stringr::str_remove(input[['FilterVariable_2_3']], 'ModelVar-')}, error = function(x) NULL)
          params <- list(data=ModelData, VarName=vname, type=2)
          choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
          Mult <- RemixAutoML:::GetFilterValueMultiple(ModelData, VarName=vname)
        }
      }
      if(Debug) print(choices)
      if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_2_3_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })

    if(Debug) print("Here ggggggg")

    output$FilterValue_2_4_1 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_2_4']]}, error = function(x) NULL), type=1)
      choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_2_4']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_2_4']])
      if(length(choices) == 0) {
        if(length(ModelData) != 0) {
          vname <- tryCatch({stringr::str_remove(input[['FilterVariable_2_4']], 'ModelVar-')}, error = function(x) NULL)
          params <- list(data=ModelData, VarName=vname, type=1)
          choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
          Mult <- RemixAutoML:::GetFilterValueMultiple(ModelData, VarName=vname)
        }
      }
      if(Debug) print(choices)
      if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_2_4_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })

    if(Debug) print("Here hhhhhhh")

    output$FilterValue_2_4_2 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_2_4']]}, error = function(x) NULL), type=2)
      choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_2_4']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_2_4']])
      if(length(choices) == 0) {
        if(length(ModelData) != 0) {
          vname <- tryCatch({stringr::str_remove(input[['FilterVariable_2_4']], 'ModelVar-')}, error = function(x) NULL)
          params <- list(data=ModelData, VarName=vname, type=2)
          choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
          Mult <- RemixAutoML:::GetFilterValueMultiple(ModelData, VarName=vname)
        }
      }
      if(Debug) print(choices)
      if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_2_4_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })

    if(Debug) print("Here qqqqqqq")

    # Plot 3
    output$FilterValue_3_1_1 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_3_1']]}, error = function(x) NULL), type=1)
      choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_3_1']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_3_1']])
      if(length(choices) == 0) {
        if(length(ModelData) != 0) {
          vname <- tryCatch({stringr::str_remove(input[['FilterVariable_3_1']], 'ModelVar-')}, error = function(x) NULL)
          params <- list(data=ModelData, VarName=vname, type=1)
          choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
          Mult <- RemixAutoML:::GetFilterValueMultiple(ModelData, VarName=vname)
        }
      }
      if(Debug) print(choices)
      if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_3_1_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })

    if(Debug) print("Here rrrrrrr")

    output$FilterValue_3_1_2 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_3_1']]}, error = function(x) NULL), type=2)
      choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_3_1']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_3_1']])
      if(length(choices) == 0) {
        if(length(ModelData) != 0) {
          vname <- tryCatch({stringr::str_remove(input[['FilterVariable_3_1']], 'ModelVar-')}, error = function(x) NULL)
          params <- list(data=ModelData, VarName=vname, type=2)
          choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
          Mult <- RemixAutoML:::GetFilterValueMultiple(ModelData, VarName=vname)
        }
      }
      if(Debug) print(choices)
      if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_3_1_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })

    if(Debug) print("Here sssssss")

    output$FilterValue_3_2_1 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_3_2']]}, error = function(x) NULL), type=1)
      choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_3_2']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_3_2']])
      if(length(choices) == 0) {
        if(length(ModelData) != 0) {
          vname <- tryCatch({stringr::str_remove(input[['FilterVariable_3_2']], 'ModelVar-')}, error = function(x) NULL)
          params <- list(data=ModelData, VarName=vname, type=1)
          choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
          Mult <- RemixAutoML:::GetFilterValueMultiple(ModelData, VarName=vname)
        }
      }
      if(Debug) print(choices)
      if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_3_2_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })

    if(Debug) print("Here ttttttt")

    output$FilterValue_3_2_2 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_3_2']]}, error = function(x) NULL), type=2)
      choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_3_2']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_3_2']])
      if(length(choices) == 0) {
        if(length(ModelData) != 0) {
          vname <- tryCatch({stringr::str_remove(input[['FilterVariable_3_2']], 'ModelVar-')}, error = function(x) NULL)
          params <- list(data=ModelData, VarName=vname, type=2)
          choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
          Mult <- RemixAutoML:::GetFilterValueMultiple(ModelData, VarName=vname)
        }
      }
      if(Debug) print(choices)
      if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_3_2_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })

    if(Debug) print("Here uuuuuuu")

    output$FilterValue_3_3_1 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_3_3']]}, error = function(x) NULL), type=1)
      choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_3_3']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_3_3']])
      if(length(choices) == 0) {
        if(length(ModelData) != 0) {
          vname <- tryCatch({stringr::str_remove(input[['FilterVariable_3_3']], 'ModelVar-')}, error = function(x) NULL)
          params <- list(data=ModelData, VarName=vname, type=1)
          choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
          Mult <- RemixAutoML:::GetFilterValueMultiple(ModelData, VarName=vname)
        }
      }
      if(Debug) print(choices)
      if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_3_3_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })

    if(Debug) print("Here vvvvvvv")

    output$FilterValue_3_3_2 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_3_3']]}, error = function(x) NULL), type=2)
      choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_3_3']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_3_3']])
      if(length(choices) == 0) {
        if(length(ModelData) != 0) {
          vname <- tryCatch({stringr::str_remove(input[['FilterVariable_3_3']], 'ModelVar-')}, error = function(x) NULL)
          params <- list(data=ModelData, VarName=vname, type=2)
          choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
          Mult <- RemixAutoML:::GetFilterValueMultiple(ModelData, VarName=vname)
        }
      }
      if(Debug) print(choices)
      if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_3_3_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })

    if(Debug) print("Here wwwwwww")

    output$FilterValue_3_4_1 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_3_4']]}, error = function(x) NULL), type=1)
      choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_3_4']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_3_4']])
      if(length(choices) == 0) {
        if(length(ModelData) != 0) {
          vname <- tryCatch({stringr::str_remove(input[['FilterVariable_3_4']], 'ModelVar-')}, error = function(x) NULL)
          params <- list(data=ModelData, VarName=vname, type=1)
          choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
          Mult <- RemixAutoML:::GetFilterValueMultiple(ModelData, VarName=vname)
        }
      }
      if(Debug) print(choices)
      if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_3_4_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })

    if(Debug) print("Here xxxxxxx")

    output$FilterValue_3_4_2 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_3_4']]}, error = function(x) NULL), type=2)
      choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_3_4']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_3_4']])
      if(length(choices) == 0) {
        if(length(ModelData) != 0) {
          vname <- tryCatch({stringr::str_remove(input[['FilterVariable_3_4']], 'ModelVar-')}, error = function(x) NULL)
          params <- list(data=ModelData, VarName=vname, type=2)
          choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
          Mult <- RemixAutoML:::GetFilterValueMultiple(ModelData, VarName=vname)
        }
      }
      if(Debug) print(choices)
      if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_3_4_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })

    if(Debug) print("Here yyyyyyy")

    # Plot 4
    output$FilterValue_4_1_1 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_4_1']]}, error = function(x) NULL), type=1)
      choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_4_1']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_4_1']])
      if(length(choices) == 0) {
        if(length(ModelData) != 0) {
          vname <- tryCatch({stringr::str_remove(input[['FilterVariable_4_1']], 'ModelVar-')}, error = function(x) NULL)
          params <- list(data=ModelData, VarName=vname, type=1)
          choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
          Mult <- RemixAutoML:::GetFilterValueMultiple(ModelData, VarName=vname)
        }
      }
      if(Debug) print(choices)
      if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_4_1_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })

    if(Debug) print("Here zzzzzzz")

    output$FilterValue_4_1_2 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_4_1']]}, error = function(x) NULL), type=2)
      choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_4_1']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_4_1']])
      if(length(choices) == 0) {
        if(length(ModelData) != 0) {
          vname <- tryCatch({stringr::str_remove(input[['FilterVariable_4_1']], 'ModelVar-')}, error = function(x) NULL)
          params <- list(data=ModelData, VarName=vname, type=2)
          choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
          Mult <- RemixAutoML:::GetFilterValueMultiple(ModelData, VarName=vname)
        }
      }
      if(Debug) print(choices)
      if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_4_1_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })

    if(Debug) print("Here aaaaaaaa")

    output$FilterValue_4_2_1 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_4_2']]}, error = function(x) NULL), type=1)
      choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_4_2']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_4_2']])
      if(length(choices) == 0) {
        if(length(ModelData) != 0) {
          vname <- tryCatch({stringr::str_remove(input[['FilterVariable_4_2']], 'ModelVar-')}, error = function(x) NULL)
          params <- list(data=ModelData, VarName=vname, type=1)
          choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
          Mult <- RemixAutoML:::GetFilterValueMultiple(ModelData, VarName=vname)
        }
      }
      if(Debug) print(choices)
      if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_4_2_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })

    if(Debug) print("Here bbbbbbbb")

    output$FilterValue_4_2_2 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_4_2']]}, error = function(x) NULL), type=2)
      choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_4_2']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_4_2']])
      if(length(choices) == 0) {
        if(length(ModelData) != 0) {
          vname <- tryCatch({stringr::str_remove(input[['FilterVariable_4_2']], 'ModelVar-')}, error = function(x) NULL)
          params <- list(data=ModelData, VarName=vname, type=2)
          choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
          Mult <- RemixAutoML:::GetFilterValueMultiple(ModelData, VarName=vname)
        }
      }
      if(Debug) print(choices)
      if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_4_2_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })

    if(Debug) print("Here cccccccc")

    output$FilterValue_4_3_1 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_4_3']]}, error = function(x) NULL), type=1)
      choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_4_3']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_4_3']])
      if(length(choices) == 0) {
        if(length(ModelData) != 0) {
          vname <- tryCatch({stringr::str_remove(input[['FilterVariable_4_3']], 'ModelVar-')}, error = function(x) NULL)
          params <- list(data=ModelData, VarName=vname, type=1)
          choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
          Mult <- RemixAutoML:::GetFilterValueMultiple(ModelData, VarName=vname)
        }
      }
      if(Debug) print(choices)
      if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_4_3_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })

    if(Debug) print("Here dddddddd")

    output$FilterValue_4_3_2 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_4_3']]}, error = function(x) NULL), type=2)
      choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_4_3']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_4_3']])
      if(length(choices) == 0) {
        if(length(ModelData) != 0) {
          vname <- tryCatch({stringr::str_remove(input[['FilterVariable_4_3']], 'ModelVar-')}, error = function(x) NULL)
          params <- list(data=ModelData, VarName=vname, type=2)
          choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
          Mult <- RemixAutoML:::GetFilterValueMultiple(ModelData, VarName=vname)
        }
      }
      if(Debug) print(choices)
      if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_4_3_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })

    if(Debug) print("Here eeeeeeee")

    output$FilterValue_4_4_1 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_4_4']]}, error = function(x) NULL), type=1)
      choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_4_4']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_4_4']])
      if(length(choices) == 0) {
        if(length(ModelData) != 0) {
          vname <- tryCatch({stringr::str_remove(input[['FilterVariable_4_4']], 'ModelVar-')}, error = function(x) NULL)
          params <- list(data=ModelData, VarName=vname, type=1)
          choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
          Mult <- RemixAutoML:::GetFilterValueMultiple(ModelData, VarName=vname)
        }
      }
      if(Debug) print(choices)
      if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_4_4_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })

    if(Debug) print("Here ffffffff")

    output$FilterValue_4_4_2 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_4_4']]}, error = function(x) NULL), type=2)
      choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_4_4']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_4_4']])
      if(length(choices) == 0) {
        if(length(ModelData) != 0) {
          vname <- tryCatch({stringr::str_remove(input[['FilterVariable_4_4']], 'ModelVar-')}, error = function(x) NULL)
          params <- list(data=ModelData, VarName=vname, type=2)
          choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
          Mult <- RemixAutoML:::GetFilterValueMultiple(ModelData, VarName=vname)
        }
      }
      if(Debug) print(choices)
      if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_4_4_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })

    # ----

    # ----

    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    # Sweet Alert                          ----
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    if(Debug) print("Here gggggggg")
    if(is.null(data) && is.null(ModelOutputList)) {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = 'Data was not loaded', type = NULL, btn_labels = "error", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = 'Success', type = NULL, btn_labels = "success", btn_colors = "green", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }

    # ----

    # ----

  })

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Variables                            ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  if(Debug) print("Here a")

  print(':::::::: DATA NULL TESTING 1 ::::::::')
  print(data)

  # YVars
  output$YVar1 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'YVar1', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Y-Variable 1'), Choices = c('None', names(data)), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  print(':::::::: DATA NULL TESTING 2 ::::::::')
  print(data)

  if(Debug) print("Here b")

  output$YVar2 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'YVar2', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Y-Variable 2'), Choices = c('None', names(data)), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here c")

  output$YVar3 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'YVar3', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Y-Variable 3'), Choices = c('None', names(data)), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here d")

  output$YVar4 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'YVar4', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Y-Variable 4'), Choices = c('None', names(data)), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here e")

  # 'X-Variables'
  output$XVar1 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'XVar1', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'X-Variable 1'), Choices = c('None', names(data)), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here f")

  output$XVar2 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'XVar2', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'X-Variable 2'), Choices = c('None', names(data)), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here g")

  output$XVar3 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'XVar3', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'X-Variable 3'), Choices = c('None', names(data)), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here h")

  output$XVar4 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'XVar4', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'X-Variable 4'), Choices = c('None', names(data)), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here i")

  # 'Score-Variables'
  output$ScoreVar1 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'ScoreVar1', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Scoring-Variable 1'), Choices = c('None', names(data)), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here j")

  output$ScoreVar2 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'ScoreVar2', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Scoring-Variable 2'), Choices = c('None', names(data)), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here k")

  output$ScoreVar3 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'ScoreVar3', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Scoring-Variable 3'), Choices = c('None', names(data)), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here l")

  output$ScoreVar4 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'ScoreVar4', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Scoring-Variable 4'), Choices = c('None', names(data)), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here m")

  # 'Date Variables'
  output$DateVar1 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'DateVar1', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Date-Variable 1'), Choices = c('None', names(data)), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here n")

  output$DateVar2 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'DateVar2', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Date-Variable 2'), Choices = c('None', names(data)), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here o")

  output$DateVar3 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'DateVar3', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Date-Variable 3'), Choices = c('None', names(data)), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here p")

  output$DateVar4 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'DateVar4', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Date-Variable 4'), Choices = c('None', names(data)), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here q")

  # Reactives References
  YVar1 <- shiny::reactive({shiny::req(input[['YVar1']])})
  YVar2 <- shiny::reactive({shiny::req(input[['YVar2']])})
  YVar3 <- shiny::reactive({shiny::req(input[['YVar3']])})
  YVar4 <- shiny::reactive({shiny::req(input[['YVar4']])})
  XVar1 <- shiny::reactive({shiny::req(input[['XVar1']])})
  XVar2 <- shiny::reactive({shiny::req(input[['XVar2']])})
  XVar3 <- shiny::reactive({shiny::req(input[['XVar3']])})
  XVar4 <- shiny::reactive({shiny::req(input[['XVar4']])})
  ScoreVar1 <- shiny::reactive({shiny::req(input[['ScoreVar1']])})
  ScoreVar2 <- shiny::reactive({shiny::req(input[['ScoreVar2']])})
  ScoreVar3 <- shiny::reactive({shiny::req(input[['ScoreVar3']])})
  ScoreVar4 <- shiny::reactive({shiny::req(input[['ScoreVar4']])})
  DateVar1 <- shiny::reactive({shiny::req(input[['DateVar1']])})
  DateVar2 <- shiny::reactive({shiny::req(input[['DateVar2']])})
  DateVar3 <- shiny::reactive({shiny::req(input[['DateVar3']])})
  DateVar4 <- shiny::reactive({shiny::req(input[['DateVar4']])})

  if(Debug) print("Here r")

  print(':::::::: DATA NULL TESTING 3 ::::::::')
  print(data)

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Plotting MetaData                    ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  if(Debug) print("Auto Size Grid")

  # Auto SCaling of Plot Grid: doubles the size in the event of more than 1 plot
  output$AutoGridHorizontal <-  shiny::renderUI({
    shinyWidgets::materialSwitch(inputId = "AutoGridHorizontal", label = tags$span(style='color: blue;', 'Auto Grid Scale'), status = "danger", value = TRUE, inline = TRUE, width = '100%')
  })

  if(Debug) print("Here qq")

  output$Plot1 <- shiny::renderUI({
    if(exists('ModelOutputList') && length(ModelOutputList) != 0) {
      bla <- names(ModelOutputList$PlotList)
      if(!is.null(ModelOutputList$TrainData) && !is.null(ModelOutputList$TestData)) {
        ModelData <<- data.table::rbindlist(list(ModelOutputList$TrainData, ModelOutputList$TestData))
      } else if(is.null(ModelOutputList$TrainData) && !is.null(ModelOutputList$TestData)) {
        ModelData <<- ModelOutputList$TestData
      } else {
        ModelData <<- NULL
      }
    } else {
      bla <- NULL
      ModelData <<- NULL
    }
    x <- RemixAutoML:::AvailableAppInsightsPlots(x = bla, PlotNamesLookup = PlotNamesLookup)
    RemixAutoML::SelectizeInput(InputID = 'Plot1', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Plot 1'), Choices = c(x), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  output$Plot2 <- shiny::renderUI({
    if(exists('ModelOutputList') && length(ModelOutputList) != 0 && exists('ModelData') && length(ModelData) != 0L) {
      bla <- names(ModelOutputList$PlotList)
    } else if(exists('ModelOutputList') && length(ModelOutputList) != 0 && !exists('ModelData')) {
      bla <- names(ModelOutputList$PlotList)
      if(!is.null(ModelOutputList$TrainData) && !is.null(ModelOutputList$TestData)) {
        ModelData <<- data.table::rbindlist(list(ModelOutputList$TrainData, ModelOutputList$TestData))
      } else if(is.null(ModelOutputList$TrainData) && !is.null(ModelOutputList$TestData)) {
        ModelData <<- ModelOutputList$TestData
      } else {
        ModelData <<- NULL
      }
    } else {
      bla <- NULL
      ModelData <<- NULL
    }
    x <- RemixAutoML:::AvailableAppInsightsPlots(x = bla, PlotNamesLookup = PlotNamesLookup)
    RemixAutoML::SelectizeInput(InputID = 'Plot2', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Plot 2'), Choices = c(x), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  output$Plot3 <- shiny::renderUI({
    if(exists('ModelOutputList') && length(ModelOutputList) != 0 && exists('ModelData') && length(ModelData) != 0L) {
      bla <- names(ModelOutputList$PlotList)
    } else if(exists('ModelOutputList') && length(ModelOutputList) != 0 && !exists('ModelData')) {
      bla <- names(ModelOutputList$PlotList)
      if(!is.null(ModelOutputList$TrainData) && !is.null(ModelOutputList$TestData)) {
        ModelData <<- data.table::rbindlist(list(ModelOutputList$TrainData, ModelOutputList$TestData))
      } else if(is.null(ModelOutputList$TrainData) && !is.null(ModelOutputList$TestData)) {
        ModelData <<- ModelOutputList$TestData
      } else {
        ModelData <<- NULL
      }
    } else {
      bla <- NULL
      ModelData <<- NULL
    }
    x <- RemixAutoML:::AvailableAppInsightsPlots(x = bla, PlotNamesLookup = PlotNamesLookup)
    RemixAutoML::SelectizeInput(InputID = 'Plot3', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Plot 3'), Choices = c(x), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  output$Plot4 <- shiny::renderUI({
    if(exists('ModelOutputList') && length(ModelOutputList) != 0 && exists('ModelData') && length(ModelData) != 0L) {
      bla <- names(ModelOutputList$PlotList)
    } else if(exists('ModelOutputList') && length(ModelOutputList) != 0 && !exists('ModelData')) {
      bla <- names(ModelOutputList$PlotList)
      if(!is.null(ModelOutputList$TrainData) && !is.null(ModelOutputList$TestData)) {
        ModelData <<- data.table::rbindlist(list(ModelOutputList$TrainData, ModelOutputList$TestData))
      } else if(is.null(ModelOutputList$TrainData) && !is.null(ModelOutputList$TestData)) {
        ModelData <<- ModelOutputList$TestData
      } else {
        ModelData <<- NULL
      }
    } else {
      bla <- NULL
      ModelData <<- NULL
    }
    x <- RemixAutoML:::AvailableAppInsightsPlots(x = bla, PlotNamesLookup = PlotNamesLookup)
    RemixAutoML::SelectizeInput(InputID = 'Plot4', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Plot 4'), Choices = c(x), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  # Dragula for PlotType
  output$PlotTypeDragula <- shiny::renderUI({
    dragheight <- '75px'
    esquisse::dragulaInput(
      height = dragheight,
      replace = TRUE,
      inputId = 'PlotTypeDragula',
      label = 'Drag and Drop Plot Types',
      sourceLabel = 'Plots',
      targetsLabels = c('UpperLeftPlot', 'BottomLeftPlot', 'UpperRightPlot', 'BottomRightPlot'),
      #style="color: #fff",# ; background-color: #e95420; border-color: #c34113; border-radius: 10px; border-width: 2px",
      choices = c('Plot1', 'Plot2', 'Plot3', 'Plot4')
      #,
      # choiceNames = , choiceValues = , selected = , status = , replace = , copySource = , badge = , ncolSource = , ncolGrid = , dragulaOpts = , boxStyle = , width = "100%", height = "100%"
    )
  })

  print(':::::::: DATA NULL TESTING 6 ::::::::')
  print(data)

  if(Debug) print("Here rr")

  # UI Plot Options
  output$YLimMin1 <- shiny::renderUI({
    RemixAutoML::TextInput(InputID = 'YLimMin1', Label = tags$span(style='color: blue;', 'Y Min Limit 1'), Value = NULL, Placeholder = 'Insert a number')
  })
  output$YLimMax1 <- shiny::renderUI({
    RemixAutoML::TextInput(InputID = 'YLimMax1', Label = tags$span(style='color: blue;', 'Y Max Limit 1'), Value = NULL, Placeholder = 'Insert a number')
  })
  output$YLimMin2 <- shiny::renderUI({
    RemixAutoML::TextInput(InputID = 'YLimMin2', Label = tags$span(style='color: blue;', 'Y Min Limit 2'), Value = NULL, Placeholder = 'Insert a number')
  })
  output$YLimMax2 <- shiny::renderUI({
    RemixAutoML::TextInput(InputID = 'YLimMax2', Label = tags$span(style='color: blue;', 'Y Max Limit 2'), Value = NULL, Placeholder = 'Insert a number')
  })
  output$YLimMin3 <- shiny::renderUI({
    RemixAutoML::TextInput(InputID = 'YLimMin3', Label = tags$span(style='color: blue;', 'Y Min Limit 3'), Value = NULL, Placeholder = 'Insert a number')
  })
  output$YLimMax3 <- shiny::renderUI({
    RemixAutoML::TextInput(InputID = 'YLimMax3', Label = tags$span(style='color: blue;', 'Y Max Limit 3'), Value = NULL, Placeholder = 'Insert a number')
  })
  output$YLimMin4 <- shiny::renderUI({
    RemixAutoML::TextInput(InputID = 'YLimMin4', Label = tags$span(style='color: blue;', 'Y Min Limit 4'), Value = NULL, Placeholder = 'Insert a number')
  })
  output$YLimMax4 <- shiny::renderUI({
    RemixAutoML::TextInput(InputID = 'YLimMax4', Label = tags$span(style='color: blue;', 'Y Max Limit 4'), Value = NULL, Placeholder = 'Insert a number')
  })
  output$XLimMin1 <- shiny::renderUI({
    RemixAutoML::TextInput(InputID = 'XLimMin1', Label = tags$span(style='color: blue;', 'X Min Limit 1'), Value = NULL, Placeholder = 'Insert a number')
  })
  output$XLimMax1 <- shiny::renderUI({
    RemixAutoML::TextInput(InputID = 'XLimMax1', Label = tags$span(style='color: blue;', 'X Max Limit 1'), Value = NULL, Placeholder = 'Insert a number')
  })
  output$XLimMin2 <- shiny::renderUI({
    RemixAutoML::TextInput(InputID = 'XLimMin2', Label = tags$span(style='color: blue;', 'X Min Limit 2'), Value = NULL, Placeholder = 'Insert a number')
  })
  output$XLimMax2 <- shiny::renderUI({
    RemixAutoML::TextInput(InputID = 'XLimMax2', Label = tags$span(style='color: blue;', 'X Max Limit 2'), Value = NULL, Placeholder = 'Insert a number')
  })
  output$XLimMin3 <- shiny::renderUI({
    RemixAutoML::TextInput(InputID = 'XLimMin3', Label = tags$span(style='color: blue;', 'X Min Limit 3'), Value = NULL, Placeholder = 'Insert a number')
  })
  output$XLimMax3 <- shiny::renderUI({
    RemixAutoML::TextInput(InputID = 'XLimMax3', Label = tags$span(style='color: blue;', 'X Max Limit 3'), Value = NULL, Placeholder = 'Insert a number')
  })
  output$XLimMin4 <- shiny::renderUI({
    RemixAutoML::TextInput(InputID = 'XLimMin4', Label = tags$span(style='color: blue;', 'X Min Limit 4'), Value = NULL, Placeholder = 'Insert a number')
  })
  output$XLimMax4 <- shiny::renderUI({
    RemixAutoML::TextInput(InputID = 'XLimMax4', Label = tags$span(style='color: blue;', 'X Max Limit 4'), Value = NULL, Placeholder = 'Insert a number')
  })

  print(':::::::: DATA NULL TESTING 7 ::::::::')
  print(data)

  # NumberGroupsDisplay
  output$NumberGroupsDisplay1 <- shiny::renderUI({
    RemixAutoML::NumericInput(InputID = 'NumberGroupsDisplay1', Label = tags$span(style='color: blue;', 'Dispay N Levels'), Step = 1L, Value = 5L, Min = 1L, Max = 100L)
  })
  output$NumberGroupsDisplay2 <- shiny::renderUI({
    RemixAutoML::NumericInput(InputID = 'NumberGroupsDisplay2', Label = tags$span(style='color: blue;', 'Dispay N Levels'), Step = 1L, Value = 5L, Min = 1L, Max = 100L)
  })
  output$NumberGroupsDisplay3 <- shiny::renderUI({
    RemixAutoML::NumericInput(InputID = 'NumberGroupsDisplay3', Label = tags$span(style='color: blue;', 'Dispay N Levels'), Step = 1L, Value = 5L, Min = 1L, Max = 100L)
  })
  output$NumberGroupsDisplay4 <- shiny::renderUI({
    RemixAutoML::NumericInput(InputID = 'NumberGroupsDisplay4', Label = tags$span(style='color: blue;', 'Dispay N Levels'), Step = 1L, Value = 5L, Min = 1L, Max = 100L)
  })

  # Global Setting
  if(Debug) print("Here ss")
  output$PlotWidth <- shiny::renderUI({
    RemixAutoML::NumericInput(InputID = "PlotWidth", Label=tags$span(style='color: blue;', 'Plot Width'), Step = 50, Min = 800, Max = 1800, Value = 1600)
  })
  if(Debug) print("Here tt")
  output$PlotHeight <- shiny::renderUI({
    RemixAutoML::NumericInput(InputID = "PlotHeight", Label=tags$span(style='color: blue;', 'Plot Height'), Step = 25, Min = 350, Max = 350*10, Value = 500)
  })

  print(':::::::: DATA NULL TESTING 7.1 ::::::::')
  print(data)


  # Correlation Variables
  if(Debug) print("Here uu")
  output$CorVariables1 <- shiny::renderUI({
    if(Debug) {
      print('::::::::::::::::::::::::::::::::::: THIS IS THE NEW PLACE TO LOOK :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::')
      print(!is.null(data) && any(RemixAutoML:::ColTypes(data) %in% c('numeric','integer')))
      print(!is.null(data))
      print(RemixAutoML:::ColTypes(data))
      print(names(data)[which(RemixAutoML:::ColTypes(data) %in% c('numeric','integer'))])
    }
    if(!is.null(data) && any(RemixAutoML:::ColTypes(data) %in% c('numeric','integer'))) {
      cor_choices <- names(data)[which(RemixAutoML:::ColTypes(data) %in% c('numeric','integer'))]
    } else {
      cor_choices <- NULL
    }
    RemixAutoML::SelectizeInput(InputID='CorVariables1', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Corr Variables 1'), Choices=c(cor_choices), SelectedDefault=NULL, Size=10, SelectedText="count > 1", Multiple=TRUE, ActionBox=TRUE)
  })

  if(Debug) print("Here vv")
  output$CorVariables2 <- shiny::renderUI({
    if(!is.null(data) && any(RemixAutoML:::ColTypes(data) %in% c('numeric','integer'))) {
      cor_choices <- names(data)[which(RemixAutoML:::ColTypes(data) %in% c('numeric','integer'))]
    } else {
      cor_choices <- NULL
    }
    RemixAutoML::SelectizeInput(InputID='CorVariables2', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Corr Variables 2'), Choices=c(cor_choices), SelectedDefault=NULL, Size=10, SelectedText="count > 1", Multiple=TRUE, ActionBox=TRUE)
  })

  if(Debug) print("Here ww")
  output$CorVariables3 <- shiny::renderUI({
    if(!is.null(data) && any(RemixAutoML:::ColTypes(data) %in% c('numeric','integer'))) {
      cor_choices <- names(data)[which(RemixAutoML:::ColTypes(data) %in% c('numeric','integer'))]
    } else {
      cor_choices <- NULL
    }
    RemixAutoML::SelectizeInput(InputID='CorVariables3', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Corr Variables 3'), Choices=c(cor_choices), SelectedDefault=NULL, Size=10, SelectedText="count > 1", Multiple=TRUE, ActionBox=TRUE)
  })

  if(Debug) print("Here xx")
  output$CorVariables4 <- shiny::renderUI({
    if(!is.null(data) && any(RemixAutoML:::ColTypes(data) %in% c('numeric','integer'))) {
      cor_choices <- names(data)[which(RemixAutoML:::ColTypes(data) %in% c('numeric','integer'))]
    } else {
      cor_choices <- NULL
    }
    RemixAutoML::SelectizeInput(InputID='CorVariables4', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Corr Variables 4'), Choices=c(cor_choices), SelectedDefault=NULL, Size=10, SelectedText="count > 1", Multiple=TRUE, ActionBox=TRUE)
  })

  if(Debug) print("Here yy")
  print(':::::::: DATA NULL TESTING 7.2 ::::::::')
  print(data)

  # PDP Variables
  if(Debug) print("Here yy")
  output$PDP_Variable1 <- shiny::renderUI({
    pdp <- RemixAutoML:::PDPVar(ModelOutputList)
    RemixAutoML::SelectizeInput(InputID='PDP_Variable1', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'PDP Variable 1'), Choices=pdp$Names, SelectedDefault=pdp$Default, Size=10, SelectedText="count > 1", Multiple=FALSE, ActionBox=TRUE)
  })

  if(Debug) print("Here vv")
  output$PDP_Variable2 <- shiny::renderUI({
    pdp <- RemixAutoML:::PDPVar(ModelOutputList)
    RemixAutoML::SelectizeInput(InputID='PDP_Variable2', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'PDP Variable 2'), Choices=pdp$Names, SelectedDefault=pdp$Default, Size=10, SelectedText="count > 1", Multiple=FALSE, ActionBox=TRUE)
  })

  if(Debug) print("Here ww")
  output$PDP_Variable3 <- shiny::renderUI({
    pdp <- RemixAutoML:::PDPVar(ModelOutputList)
    RemixAutoML::SelectizeInput(InputID='PDP_Variable3', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'PDP Variable 3'), Choices=pdp$Names, SelectedDefault=pdp$Default, Size=10, SelectedText="count > 1", Multiple=FALSE, ActionBox=TRUE)
  })

  if(Debug) print("Here xx")
  output$PDP_Variable4 <- shiny::renderUI({
    pdp <- RemixAutoML:::PDPVar(ModelOutputList)
    RemixAutoML::SelectizeInput(InputID='PDP_Variable4', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'PDP Variable 4'), Choices=pdp$Names, SelectedDefault=pdp$Default, Size=10, SelectedText="count > 1", Multiple=FALSE, ActionBox=TRUE)
  })

  if(Debug) print("Here yy")
  print(':::::::: DATA NULL TESTING 8 ::::::::')
  print(data)

  # Shapely Agg Method
  output$ShapAggMethod1 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID='ShapAggMethod1', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Shap Agg Method 1'), Choices=c('mean','absmean','meanabs','sd','median','absmedian','medianabs'), SelectedDefault='mean', Size=10, SelectedText= "count > 1", Multiple=FALSE, ActionBox=TRUE)
  })
  output$ShapAggMethod2 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID='ShapAggMethod2', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Shap Agg Method 2'), Choices=c('mean','absmean','meanabs','sd','median','absmedian','medianabs'), SelectedDefault='mean', Size=10, SelectedText= "count > 1", Multiple=FALSE, ActionBox=TRUE)
  })
  output$ShapAggMethod3 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID='ShapAggMethod3', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Shap Agg Method 3'), Choices=c('mean','absmean','meanabs','sd','median','absmedian','medianabs'), SelectedDefault='mean', Size=10, SelectedText= "count > 1", Multiple=FALSE, ActionBox=TRUE)
  })
  output$ShapAggMethod4 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID='ShapAggMethod4', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Shap Agg Method 4'), Choices=c('mean','absmean','meanabs','sd','median','absmedian','medianabs'), SelectedDefault='mean', Size=10, SelectedText= "count > 1", Multiple=FALSE, ActionBox=TRUE)
  })

  # Percentile Buckets
  if(Debug) print("Here cccasdfa")
  output$Percentile_Buckets1 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID='Percentile_Buckets1', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Percentile Buckets 1'), Choices=1:100, SelectedDefault=20, Size=10, SelectedText="count > 1", Multiple=FALSE, ActionBox=TRUE)
  })

  if(Debug) print("Here zz")
  output$Percentile_Buckets2 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID='Percentile_Buckets2', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Percentile Buckets 2'), Choices=1:100, SelectedDefault=20, Size=10, SelectedText= "count > 1", Multiple=FALSE, ActionBox=TRUE)
  })

  if(Debug) print("Here aaa")
  output$Percentile_Buckets3 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID='Percentile_Buckets3', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Percentile Buckets 3'), Choices=1:100, SelectedDefault=20, Size=10, SelectedText= "count > 1", Multiple=FALSE, ActionBox=TRUE)
  })

  if(Debug) print("Here bbb")
  output$Percentile_Buckets4 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID='Percentile_Buckets4', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Percentile Buckets 4'), Choices=1:100, SelectedDefault=20, Size=10, SelectedText= "count > 1", Multiple=FALSE, ActionBox=TRUE)
  })

  # Add GAM Fit to Plot
  if(Debug) print("Here ccc")
  output$GamFitScatter1 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID='GamFitScatter1', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Fit Gam on Scatter or Copula 1'), Choices=c('TRUE','FALSE'), SelectedDefault=FALSE, Multiple=FALSE, ActionBox=TRUE)
  })

  if(Debug) print("Here ddd")
  output$GamFitScatter2 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID='GamFitScatter2', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Fit Gam on Scatter or Copula 2'), Choices=c('TRUE','FALSE'), SelectedDefault=FALSE, Multiple=FALSE, ActionBox=TRUE)
  })

  if(Debug) print("Here eee")
  output$GamFitScatter3 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID='GamFitScatter3', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Fit Gam on Scatter or Copula 3'), Choices=c('TRUE','FALSE'), SelectedDefault=FALSE, Multiple=FALSE, ActionBox=TRUE)
  })

  if(Debug) print("Here fff")
  output$GamFitScatter4 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID='GamFitScatter4', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Fit Gam on Scatter or Copula 4'), Choices=c('TRUE','FALSE'), SelectedDefault=FALSE, Multiple=FALSE, ActionBox=TRUE)
  })

  # Histogram Bins
  if(Debug) print("Here ggg")
  output$NumberBins1 <- shiny::renderUI({
    RemixAutoML::NumericInput(InputID='NumberBins1', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'# of Bins for Histogram 1'), Min=1, Max=1000, Step=5, Value=30)
  })

  if(Debug) print("Here hhh")
  output$NumberBins2 <- shiny::renderUI({
    RemixAutoML::NumericInput(InputID='NumberBins2', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'# of Bins for Histogram 2'), Min=1, Max=1000, Step=5, Value=30)
  })

  if(Debug) print("Here iii")
  output$NumberBins3 <- shiny::renderUI({
    RemixAutoML::NumericInput(InputID='NumberBins3', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'# of Bins for Histogram 3'), Min=1, Max=1000, Step=5, Value=30)
  })

  if(Debug) print("Here jjj")
  output$NumberBins4 <- shiny::renderUI({
    RemixAutoML::NumericInput(InputID='NumberBins4', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'# of Bins for Histogram 4'), Min=1, Max=1000, Step=5, Value=30)
  })

  # YTicks Values (NULL is whats handled by RemixAutoML:::YTicks())
  if(Debug) print("Here kkk")
  output$YTicks1 <- shiny::renderUI({
    yy <- tryCatch({YVar1()}, error = function(x) 'None')
    if(Debug) print(RemixAutoML:::XTicks(data, xvar=yy))
    if(yy %in% names(data)) temp <- data else temp <- ModelData
    RemixAutoML::SelectizeInput(InputID = 'YTicks1', Label = tags$span(style='color: blue;', 'Y-Axis 1 Ticks'), Choices = RemixAutoML:::YTicks(temp, yvar = yy), SelectedDefault = 'Default', Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
  })

  if(Debug) print("Here lll")
  output$YTicks2 <- shiny::renderUI({
    yy <- tryCatch({YVar2()}, error = function(x) 'None')
    if(Debug) print(RemixAutoML:::XTicks(data, xvar=yy))
    if(yy %in% names(data)) temp <- data else temp <- ModelData
    RemixAutoML::SelectizeInput(InputID = 'YTicks2', Label = tags$span(style='color: blue;', 'Y-Axis 2 Ticks'), Choices = RemixAutoML:::YTicks(temp, yvar = yy), SelectedDefault = 'Default', Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
  })

  if(Debug) print("Here mmm")
  output$YTicks3 <- shiny::renderUI({
    yy <- tryCatch({YVar3()}, error = function(x) 'None')
    if(Debug) print(RemixAutoML:::XTicks(data, xvar=yy))
    if(yy %in% names(data)) temp <- data else temp <- ModelData
    RemixAutoML::SelectizeInput(InputID = 'YTicks3', Label = tags$span(style='color: blue;', 'Y-Axis 3 Ticks'), Choices = RemixAutoML:::YTicks(temp, yvar = yy), SelectedDefault = 'Default', Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
  })

  if(Debug) print("Here nnn")
  output$YTicks4 <- shiny::renderUI({
    yy <- tryCatch({YVar4()}, error = function(x) 'None')
    if(Debug) print(RemixAutoML:::XTicks(data, xvar=yy))
    if(yy %in% names(data)) temp <- data else temp <- ModelData
    RemixAutoML::SelectizeInput(InputID = 'YTicks4', Label = tags$span(style='color: blue;', 'Y-Axis 4 Ticks'), Choices = RemixAutoML:::YTicks(temp, yvar = yy), SelectedDefault = 'Default', Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
  })

  # XTicks Values ('None' is whats handled by RemixAutoML:::XTicks())
  if(Debug) print("Here ooo")
  output$XTicks1 <- shiny::renderUI({
    xx <- tryCatch({XVar1()}, error = function(x) 'None')
    dd <- tryCatch({DateVar1()}, error = function(x) 'None')
    if(Debug) print(RemixAutoML:::XTicks(data, xvar=xx,datevar=dd))
    if(xx %in% names(data)) temp <- data else temp <- ModelData
    RemixAutoML::SelectizeInput(InputID = 'XTicks1', Label = tags$span(style='color: blue;', 'X-Axis 1 Ticks'), Choices = RemixAutoML:::XTicks(temp, xvar=xx,datevar=dd), SelectedDefault = 'Default', Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
  })

  if(Debug) print("Here ppp")
  output$XTicks2 <- shiny::renderUI({
    xx <- tryCatch({XVar2()}, error = function(x) 'None')
    dd <- tryCatch({DateVar2()}, error = function(x) 'None')
    if(Debug) print(RemixAutoML:::XTicks(data, xvar=xx,datevar=dd))
    if(xx %in% names(data)) temp <- data else temp <- ModelData
    RemixAutoML::SelectizeInput(InputID = 'XTicks2', Label = tags$span(style='color: blue;', 'X-Axis 2 Ticks'), Choices = RemixAutoML:::XTicks(temp, xvar=xx,datevar=dd), SelectedDefault = 'Default', Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
  })

  if(Debug) print("Here qqq")

  output$XTicks3 <- shiny::renderUI({
    xx <- tryCatch({XVar3()}, error = function(x) 'None')
    dd <- tryCatch({DateVar3()}, error = function(x) 'None')
    if(Debug) print(RemixAutoML:::XTicks(data, xvar=xx,datevar=dd))
    if(xx %in% names(data)) temp <- data else temp <- ModelData
    RemixAutoML::SelectizeInput(InputID = 'XTicks3', Label = tags$span(style='color: blue;', 'X-Axis 3 Ticks'), Choices = RemixAutoML:::XTicks(temp, xvar=xx,datevar=dd), SelectedDefault = 'Default', Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
  })

  if(Debug) print("Here rrr")
  output$XTicks4 <- shiny::renderUI({
    xx <- tryCatch({XVar4()}, error = function(x) 'None')
    dd <- tryCatch({DateVar4()}, error = function(x) 'None')
    if(Debug) print(RemixAutoML:::XTicks(data, xvar=xx,datevar=dd))
    if(xx %in% names(data)) temp <- data else temp <- ModelData
    RemixAutoML::SelectizeInput(InputID = 'XTicks4', Label = tags$span(style='color: blue;', 'X-Axis 4 Ticks'), Choices = RemixAutoML:::XTicks(temp, xvar=xx,datevar=dd), SelectedDefault = 'Default', Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
  })

  # Other values
  if(Debug) print("Here sss")
  output$SampleSize <- shiny::renderUI({
    RemixAutoML::NumericInput(InputID = 'SampleSize', Label = tags$span(style='color: blue;', 'Sample size for plotting'), Step = 50000, Min = 0, Max = 1000000, Value = 100000)
  })

  if(Debug) print("Here sssa")
  output$AngleY1 <- shiny::renderUI({
    RemixAutoML::NumericInput(InputID = 'AngleY1', Label = tags$span(style='color: blue;', 'Y-axis text angle'), Step = 5, Min = 0, Max = 360, Value = 0)
  })
  output$AngleY2 <- shiny::renderUI({
    RemixAutoML::NumericInput(InputID = 'AngleY2', Label = tags$span(style='color: blue;', 'Y-axis text angle'), Step = 5, Min = 0, Max = 360, Value = 0)
  })
  output$AngleY3 <- shiny::renderUI({
    RemixAutoML::NumericInput(InputID = 'AngleY3', Label = tags$span(style='color: blue;', 'Y-axis text angle'), Step = 5, Min = 0, Max = 360, Value = 0)
  })
  output$AngleY4 <- shiny::renderUI({
    RemixAutoML::NumericInput(InputID = 'AngleY4', Label = tags$span(style='color: blue;', 'Y-axis text angle'), Step = 5, Min = 0, Max = 360, Value = 0)
  })

  if(Debug) print("Here ttt")
  output$AngleX1 <- shiny::renderUI({
    RemixAutoML::NumericInput(InputID = 'AngleX1', Label = tags$span(style='color: blue;', 'X-axis text angle'), Step = 5, Min = 0, Max = 360, Value = 90)
  })
  output$AngleX2 <- shiny::renderUI({
    RemixAutoML::NumericInput(InputID = 'AngleX2', Label = tags$span(style='color: blue;', 'X-axis text angle'), Step = 5, Min = 0, Max = 360, Value = 90)
  })
  output$AngleX3 <- shiny::renderUI({
    RemixAutoML::NumericInput(InputID = 'AngleX3', Label = tags$span(style='color: blue;', 'X-axis text angle'), Step = 5, Min = 0, Max = 360, Value = 90)
  })
  output$AngleX4 <- shiny::renderUI({
    RemixAutoML::NumericInput(InputID = 'AngleX4', Label = tags$span(style='color: blue;', 'X-axis text angle'), Step = 5, Min = 0, Max = 360, Value = 90)
  })

  if(Debug) print("Here uuu")
  output$TextSize1 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'TextSize1', Label = tags$span(style='color: blue;', 'Text Size'),Choices = c(as.character(seq(1,100,1))), SelectedDefault = '14', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$TextSize2 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'TextSize2', Label = tags$span(style='color: blue;', 'Text Size'),Choices = c(as.character(seq(1,100,1))), SelectedDefault = '14', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$TextSize3 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'TextSize3', Label = tags$span(style='color: blue;', 'Text Size'),Choices = c(as.character(seq(1,100,1))), SelectedDefault = '14', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$TextSize4 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'TextSize4', Label = tags$span(style='color: blue;', 'Text Size'),Choices = c(as.character(seq(1,100,1))), SelectedDefault = '14', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here vvv")
  output$OutlierSize1 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'OutlierSize1', Label = tags$span(style='color: blue;', 'Outlier Size'), Choices = c(seq(0.01,2,0.01)), SelectedDefault = 0.01, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$OutlierSize2 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'OutlierSize2', Label = tags$span(style='color: blue;', 'Outlier Size'), Choices = c(seq(0.01,2,0.01)), SelectedDefault = 0.01, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$OutlierSize3 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'OutlierSize3', Label = tags$span(style='color: blue;', 'Outlier Size'), Choices = c(seq(0.01,2,0.01)), SelectedDefault = 0.01, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$OutlierSize4 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'OutlierSize4', Label = tags$span(style='color: blue;', 'Outlier Size'), Choices = c(seq(0.01,2,0.01)), SelectedDefault = 0.01, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here www")
  output$LegendPosition1 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'LegendPosition1', Label = tags$span(style='color: blue;', 'Legend Position'), Choices = c('bottom','left','right','top','none'), SelectedDefault = 'bottom', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$LegendPosition2 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'LegendPosition2', Label = tags$span(style='color: blue;', 'Legend Position'), Choices = c('bottom','left','right','top','none'), SelectedDefault = 'bottom', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$LegendPosition3 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'LegendPosition3', Label = tags$span(style='color: blue;', 'Legend Position'), Choices = c('bottom','left','right','top','none'), SelectedDefault = 'bottom', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$LegendPosition4 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'LegendPosition4', Label = tags$span(style='color: blue;', 'Legend Position'), Choices = c('bottom','left','right','top','none'), SelectedDefault = 'bottom', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here xxx")
  output$LegendBorderSize1 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'LegendBorderSize1', Label = tags$span(style='color: blue;', 'Legend Border Size'), Choices = c(as.character(seq(0.01,2,0.01))), SelectedDefault = as.character(0.01), Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$LegendBorderSize2 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'LegendBorderSize2', Label = tags$span(style='color: blue;', 'Legend Border Size'), Choices = c(as.character(seq(0.01,2,0.01))), SelectedDefault = as.character(0.01), Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$LegendBorderSize3 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'LegendBorderSize3', Label = tags$span(style='color: blue;', 'Legend Border Size'), Choices = c(as.character(seq(0.01,2,0.01))), SelectedDefault = as.character(0.01), Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$LegendBorderSize4 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'LegendBorderSize4', Label = tags$span(style='color: blue;', 'Legend Border Size'), Choices = c(as.character(seq(0.01,2,0.01))), SelectedDefault = as.character(0.01), Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here yyy")
  output$LegendLineType1 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'LegendLineType1', Label = tags$span(style='color: blue;', 'Legend Border Type'), Choices = c('solid','blank','dashed','dotdash','dotted','longlash','twodash'), SelectedDefault = 'solid', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$LegendLineType2 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'LegendLineType2', Label = tags$span(style='color: blue;', 'Legend Border Type'), Choices = c('solid','blank','dashed','dotdash','dotted','longlash','twodash'), SelectedDefault = 'solid', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$LegendLineType3 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'LegendLineType3', Label = tags$span(style='color: blue;', 'Legend Border Type'), Choices = c('solid','blank','dashed','dotdash','dotted','longlash','twodash'), SelectedDefault = 'solid', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$LegendLineType4 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'LegendLineType4', Label = tags$span(style='color: blue;', 'Legend Border Type'), Choices = c('solid','blank','dashed','dotdash','dotted','longlash','twodash'), SelectedDefault = 'solid', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  # Color boxes
  if(Debug) print("Here zzz")
  output$TextColor1 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'TextColor1', Label = tags$span(style='color: blue;', 'Text Color'), Choices = grDevices::colors(), SelectedDefault = 'darkblue', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$TextColor2 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'TextColor2', Label = tags$span(style='color: blue;', 'Text Color'), Choices = grDevices::colors(), SelectedDefault = 'darkblue', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$TextColor3 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'TextColor3', Label = tags$span(style='color: blue;', 'Text Color'), Choices = grDevices::colors(), SelectedDefault = 'darkblue', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$TextColor4 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'TextColor4', Label = tags$span(style='color: blue;', 'Text Color'), Choices = grDevices::colors(), SelectedDefault = 'darkblue', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here aaaa")
  output$ChartColor1 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'ChartColor1', Label = tags$span(style='color: blue;', 'Chart Color'), Choices = grDevices::colors(), SelectedDefault = 'lightsteelblue1', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$ChartColor2 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'ChartColor2', Label = tags$span(style='color: blue;', 'Chart Color'), Choices = grDevices::colors(), SelectedDefault = 'lightsteelblue1', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$ChartColor3 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'ChartColor3', Label = tags$span(style='color: blue;', 'Chart Color'), Choices = grDevices::colors(), SelectedDefault = 'lightsteelblue1', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$ChartColor4 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'ChartColor4', Label = tags$span(style='color: blue;', 'Chart Color'), Choices = grDevices::colors(), SelectedDefault = 'lightsteelblue1', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here bbbb")
  output$GridColor1 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'GridColor1', Label = tags$span(style='color: blue;', 'Grid Lines Color'), Choices = grDevices::colors(), SelectedDefault = 'white', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$GridColor2 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'GridColor2', Label = tags$span(style='color: blue;', 'Grid Lines Color'), Choices = grDevices::colors(), SelectedDefault = 'white', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$GridColor3 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'GridColor3', Label = tags$span(style='color: blue;', 'Grid Lines Color'), Choices = grDevices::colors(), SelectedDefault = 'white', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$GridColor4 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'GridColor4', Label = tags$span(style='color: blue;', 'Grid Lines Color'), Choices = grDevices::colors(), SelectedDefault = 'white', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here cccc")
  output$BackGroundColor1 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'BackGroundColor1', Label = tags$span(style='color: blue;', 'Background Color'), Choices = grDevices::colors(), SelectedDefault = 'gray95', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$BackGroundColor2 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'BackGroundColor2', Label = tags$span(style='color: blue;', 'Background Color'), Choices = grDevices::colors(), SelectedDefault = 'gray95', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$BackGroundColor3 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'BackGroundColor3', Label = tags$span(style='color: blue;', 'Background Color'), Choices = grDevices::colors(), SelectedDefault = 'gray95', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$BackGroundColor4 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'BackGroundColor4', Label = tags$span(style='color: blue;', 'Background Color'), Choices = grDevices::colors(), SelectedDefault = 'gray95', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here dddd")
  output$BorderColor1 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'BorderColor1', Label = tags$span(style='color: blue;', 'Border Color'), Choices = grDevices::colors(), SelectedDefault = 'darkblue', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$BorderColor2 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'BorderColor2', Label = tags$span(style='color: blue;', 'Border Color'), Choices = grDevices::colors(), SelectedDefault = 'darkblue', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$BorderColor3 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'BorderColor3', Label = tags$span(style='color: blue;', 'Border Color'), Choices = grDevices::colors(), SelectedDefault = 'darkblue', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$BorderColor4 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'BorderColor4', Label = tags$span(style='color: blue;', 'Border Color'), Choices = grDevices::colors(), SelectedDefault = 'darkblue', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here eeee")
  output$OutlierColor1 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'OutlierColor1', Label = tags$span(style='color: blue;', 'Outlier Color'), Choices = grDevices::colors(), SelectedDefault = 'blue', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$OutlierColor2 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'OutlierColor2', Label = tags$span(style='color: blue;', 'Outlier Color'), Choices = grDevices::colors(), SelectedDefault = 'blue', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$OutlierColor3 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'OutlierColor3', Label = tags$span(style='color: blue;', 'Outlier Color'), Choices = grDevices::colors(), SelectedDefault = 'blue', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$OutlierColor4 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'OutlierColor4', Label = tags$span(style='color: blue;', 'Outlier Color'), Choices = grDevices::colors(), SelectedDefault = 'blue', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here ffff")
  output$FillColor1 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'FillColor1', Label = tags$span(style='color: blue;', 'BoxPlot Fill Color'), Choices = grDevices::colors(), SelectedDefault = 'gray', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$FillColor2 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'FillColor2', Label = tags$span(style='color: blue;', 'BoxPlot Fill Color'), Choices = grDevices::colors(), SelectedDefault = 'gray', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$FillColor3 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'FillColor3', Label = tags$span(style='color: blue;', 'BoxPlot Fill Color'), Choices = grDevices::colors(), SelectedDefault = 'gray', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$FillColor4 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'FillColor4', Label = tags$span(style='color: blue;', 'BoxPlot Fill Color'), Choices = grDevices::colors(), SelectedDefault = 'gray', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here gggg")
  output$SubTitleColor1 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'SubTitleColor1', Label = tags$span(style='color: blue;', 'Subtitle Color'), Choices = grDevices::colors(), SelectedDefault = 'blue', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$SubTitleColor2 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'SubTitleColor2', Label = tags$span(style='color: blue;', 'Subtitle Color'), Choices = grDevices::colors(), SelectedDefault = 'blue', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$SubTitleColor3 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'SubTitleColor3', Label = tags$span(style='color: blue;', 'Subtitle Color'), Choices = grDevices::colors(), SelectedDefault = 'blue', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$SubTitleColor4 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'SubTitleColor4', Label = tags$span(style='color: blue;', 'Subtitle Color'), Choices = grDevices::colors(), SelectedDefault = 'blue', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  print(':::::::: DATA NULL TESTING 9 ::::::::')
  print(data)

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Group Variables                      ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  if(Debug) print("Here hhhh")

  # Select GroupVars
  output$GroupVars1 <- shiny::renderUI({
    if(Debug) print('SelectizeInput GroupVars1')
    RemixAutoML::SelectizeInput(InputID='GroupVars1', Label=tags$span(style='color: blue;', 'Select Group Variables 1'), Choices=c('None',names(data)), SelectedDefault='None', SelectedText='count > 1', Multiple=TRUE, ActionBox=TRUE)
  })

  print(':::::::: DATA NULL TESTING 10 ::::::::')
  print(data)

  if(Debug) print("Here iiii")

  output$GroupVars2 <- shiny::renderUI({
    if(Debug) print('SelectizeInput GroupVars2')
    RemixAutoML::SelectizeInput(InputID='GroupVars2', Label=tags$span(style='color: blue;', 'Select Group Variables 2'), Choices=c('None',names(data)), SelectedDefault='None', SelectedText='count > 1', Multiple=TRUE, ActionBox=TRUE)
  })

  if(Debug) print("Here jjjj")

  output$GroupVars3 <- shiny::renderUI({
    if(Debug) print('SelectizeInput GroupVars3')
    RemixAutoML::SelectizeInput(InputID='GroupVars3', Label=tags$span(style='color: blue;', 'Select Group Variables 3'), Choices=c('None',names(data)), SelectedDefault='None', SelectedText='count > 1', Multiple=TRUE, ActionBox=TRUE)
  })

  if(Debug) print("Here kkkk")

  output$GroupVars4 <- shiny::renderUI({
    if(Debug) print('SelectizeInput GroupVars4')
    RemixAutoML::SelectizeInput(InputID='GroupVars4', Label=tags$span(style='color: blue;', 'Select Group Variables 4'), Choices=c('None',names(data)), SelectedDefault='None', SelectedText='count > 1', Multiple=TRUE, ActionBox=TRUE)
  })

  if(Debug) print("Here llll")

  # Reactive Group Variables
  SelectedGroups1 <- shiny::reactive({
    RemixAutoML::ReturnParam(input, VarName = 'GroupVars1', Default = 'None', Switch = TRUE, Type = 'character')
  })

  if(Debug) print("Here mmmm")

  SelectedGroups2 <- shiny::reactive({
    RemixAutoML::ReturnParam(input, VarName = 'GroupVars2', Default = 'None', Switch = TRUE, Type = 'character')
  })

  if(Debug) print("Here nnnn")

  SelectedGroups3 <- shiny::reactive({
    RemixAutoML::ReturnParam(input, VarName = 'GroupVars3', Default = 'None', Switch = TRUE, Type = 'character')
  })

  if(Debug) print("Here oooo")

  SelectedGroups4 <- shiny::reactive({
    RemixAutoML::ReturnParam(input, VarName = 'GroupVars4', Default = 'None', Switch = TRUE, Type = 'character')
  })

  if(Debug) print("Here pppp")

  # Group Levels
  output$Levels_1_1 <- shiny::renderUI({
    sgs <- RemixAutoML:::LevelValues(SelectedGroups1()); if(Debug) print('PickerInput_GetLevels 1_1')
    RemixAutoML::PickerInput_GetLevels2(input, DataExist=exists('data'), InputID='Levels_1_1', InputID2=sgs, Choices=RemixAutoML::UniqueLevels(input, data, 1L, GroupVars=sgs), NumGroupVar=1L, Size=9, SelectedText='count > 1', Multiple=TRUE, ActionBox=TRUE, SelectedDefault=NULL)
  })

  if(Debug) print("Here qqqq")

  output$Levels_1_2 <- shiny::renderUI({
    sgs <- RemixAutoML:::LevelValues(SelectedGroups1()); if(Debug) print('PickerInput_GetLevels 1_2')
    RemixAutoML::PickerInput_GetLevels2(input, DataExist=exists('data'), InputID='Levels_1_2', InputID2=sgs, Choices=RemixAutoML::UniqueLevels(input, data, 2L, GroupVars=sgs), NumGroupVar=2L, Size=9, SelectedText='count > 1', Multiple=TRUE, ActionBox=TRUE, SelectedDefault=NULL)
  })

  if(Debug) print("Here rrrr")

  output$Levels_1_3 <- shiny::renderUI({
    sgs <- RemixAutoML:::LevelValues(SelectedGroups1()); if(Debug) print('PickerInput_GetLevels 1_3')
    RemixAutoML::PickerInput_GetLevels2(input, DataExist=exists('data'), InputID='Levels_1_3', InputID2=sgs, Choices=RemixAutoML::UniqueLevels(input, data, 3L, GroupVars=sgs), NumGroupVar=3L, Size=9, SelectedText="count > 1", Multiple=TRUE, ActionBox=TRUE, SelectedDefault=NULL)
  })

  if(Debug) print("Here ssss")

  output$Levels_2_1 <- shiny::renderUI({
    sgs <- RemixAutoML:::LevelValues(SelectedGroups2()); if(Debug) print('PickerInput_GetLevels 2_1')
    RemixAutoML::PickerInput_GetLevels2(input, DataExist=exists('data'), InputID='Levels_2_1', InputID2=sgs, Choices=RemixAutoML::UniqueLevels(input, data, 1L, GroupVars=sgs), NumGroupVar=1L, Size=9, SelectedText='count > 1', Multiple=TRUE, ActionBox=TRUE, SelectedDefault=NULL)
  })

  if(Debug) print("Here tttt")

  output$Levels_2_2 <- shiny::renderUI({
    sgs <- RemixAutoML:::LevelValues(SelectedGroups2()); if(Debug) print('PickerInput_GetLevels 2_2')
    RemixAutoML::PickerInput_GetLevels2(input, DataExist=exists('data'), InputID='Levels_2_2', InputID2=sgs, Choices=RemixAutoML::UniqueLevels(input, data, 2L, GroupVars=sgs), NumGroupVar=2L, Size=9, SelectedText='count > 1', Multiple=TRUE, ActionBox=TRUE, SelectedDefault=NULL)
  })

  if(Debug) print("Here uuuu")

  output$Levels_2_3 <- shiny::renderUI({
    sgs <- RemixAutoML:::LevelValues(SelectedGroups2()); if(Debug) print('PickerInput_GetLevels 2_3')
    RemixAutoML::PickerInput_GetLevels2(input, DataExist=exists('data'), InputID='Levels_2_3', InputID2=sgs, Choices=RemixAutoML::UniqueLevels(input, data, 3L, GroupVars=sgs), NumGroupVar=3L, Size=9, SelectedText="count > 1", Multiple=TRUE, ActionBox=TRUE, SelectedDefault=NULL)
  })

  if(Debug) print("Here vvvv")

  output$Levels_3_1 <- shiny::renderUI({
    sgs <- RemixAutoML:::LevelValues(SelectedGroups3()); if(Debug) print('PickerInput_GetLevels 3_1')
    RemixAutoML::PickerInput_GetLevels2(input, DataExist=exists('data'), InputID='Levels_3_1', InputID2=sgs, Choices=RemixAutoML::UniqueLevels(input, data, 1L, GroupVars=sgs), NumGroupVar=1L, Size=9, SelectedText='count > 1', Multiple=TRUE, ActionBox=TRUE, SelectedDefault=NULL)
  })

  if(Debug) print("Here wwww")

  output$Levels_3_2 <- shiny::renderUI({
    sgs <- RemixAutoML:::LevelValues(SelectedGroups3()); if(Debug) print('PickerInput_GetLevels 3_2')
    RemixAutoML::PickerInput_GetLevels2(input, DataExist=exists('data'), InputID='Levels_3_2', InputID2=sgs, Choices=RemixAutoML::UniqueLevels(input, data, 2L, GroupVars=sgs), NumGroupVar=2L, Size=9, SelectedText='count > 1', Multiple=TRUE, ActionBox=TRUE, SelectedDefault=NULL)
  })

  if(Debug) print("Here xxxx")

  output$Levels_3_3 <- shiny::renderUI({
    sgs <- RemixAutoML:::LevelValues(SelectedGroups3()); if(Debug) print('PickerInput_GetLevels 3_3')
    RemixAutoML::PickerInput_GetLevels2(input, DataExist=exists('data'), InputID='Levels_3_3', InputID2=sgs, Choices=RemixAutoML::UniqueLevels(input, data, 3L, GroupVars=sgs), NumGroupVar=3L, Size=9, SelectedText="count > 1", Multiple=TRUE, ActionBox=TRUE, SelectedDefault=NULL)
  })

  if(Debug) print("Here yyyy")

  output$Levels_4_1 <- shiny::renderUI({
    sgs <- RemixAutoML:::LevelValues(SelectedGroups4()); if(Debug) print('PickerInput_GetLevels 4_1')
    RemixAutoML::PickerInput_GetLevels2(input, DataExist=exists('data'), InputID='Levels_4_1', InputID2=sgs, Choices=RemixAutoML::UniqueLevels(input, data, 1L, GroupVars=sgs), NumGroupVar=1L, Size=9, SelectedText='count > 1', Multiple=TRUE, ActionBox=TRUE, SelectedDefault=NULL)
  })

  if(Debug) print("Here zzzz")

  output$Levels_4_2 <- shiny::renderUI({
    sgs <- RemixAutoML:::LevelValues(SelectedGroups4()); if(Debug) print('PickerInput_GetLevels 4_2')
    RemixAutoML::PickerInput_GetLevels2(input, DataExist=exists('data'), InputID='Levels_4_2', InputID2=sgs, Choices=RemixAutoML::UniqueLevels(input, data, 2L, GroupVars=sgs), NumGroupVar=2L, Size=9, SelectedText='count > 1', Multiple=TRUE, ActionBox=TRUE, SelectedDefault=NULL)
  })

  if(Debug) print("Here aaaaa")

  output$Levels_4_3 <- shiny::renderUI({
    sgs <- RemixAutoML:::LevelValues(SelectedGroups4()); if(Debug) print('PickerInput_GetLevels 4_3')
    RemixAutoML::PickerInput_GetLevels2(input, DataExist=exists('data'), InputID='Levels_4_3', InputID2=sgs, Choices=RemixAutoML::UniqueLevels(input, data, 3L, GroupVars=sgs), NumGroupVar=3L, Size=9, SelectedText="count > 1", Multiple=TRUE, ActionBox=TRUE, SelectedDefault=NULL)
  })

  if(Debug) print("Here bbbbb")

  # Faceting
  output$FacetVar_1_1 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID='FacetVar_1_1', Label = tags$span(style='color: blue;', 'Facet Variable 1 1'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here ccccc")

  output$FacetVar_1_2 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID='FacetVar_1_2', Label = tags$span(style='color: blue;', 'Facet Variable 1 2'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here ddddd")

  output$FacetVar_2_1 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID='FacetVar_2_1', Label = tags$span(style='color: blue;', 'Facet Variable 2 1'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here eeeee")

  output$FacetVar_2_2 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID='FacetVar_2_2', Label = tags$span(style='color: blue;', 'Facet Variable 2 2'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here fffff")

  output$FacetVar_3_1 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID='FacetVar_3_1', Label = tags$span(style='color: blue;', 'Facet Variable 3 1'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here ggggg")

  output$FacetVar_3_2 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID='FacetVar_3_2', Label = tags$span(style='color: blue;', 'Facet Variable 3 2'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here hhhhh")

  output$FacetVar_4_1 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID='FacetVar_4_1', Label = tags$span(style='color: blue;', 'Facet Variable 4 1'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here iiiii")

  output$FacetVar_4_2 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID='FacetVar_4_2', Label = tags$span(style='color: blue;', 'Facet Variable 4 2'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here jjjjj")

  # Sizing
  output$SizeVar1 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'SizeVar1', Label = tags$span(style='color: blue;', 'Size Variable 1'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here kkkkk")

  output$SizeVar2 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'SizeVar2', Label = tags$span(style='color: blue;', 'Size Variable 2'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here lllll")

  output$SizeVar3 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'SizeVar3', Label = tags$span(style='color: blue;', 'Size Variable 3'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here mmmmm")

  output$SizeVar4 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'SizeVar4', Label = tags$span(style='color: blue;', 'Size Variable 4'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  print(':::::::: DATA NULL TESTING 11 ::::::::')
  print(data)

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Filter Variables                     ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  # Filter Variables
  output$FilterVariable_1_1 <- shiny::renderUI({
    if(!is.null(ModelData)) {
      ModelVars <- paste0('ModelVar-', names(ModelData))
    } else {
      ModelVars <- NULL
    }
    shiny::selectInput(inputId='FilterVariable_1_1', label = tags$span(style='color: blue;', 'Filter Variable 1 1'), choices=c('None', names(data), ModelVars), selected='None')
  })

  print(':::::::: DATA NULL TESTING 12 ::::::::')
  print(data)

  if(Debug) print("Here nnnnn")

  output$FilterVariable_1_2 <- shiny::renderUI({
    if(!is.null(ModelData)) {
      ModelVars <- paste0('ModelVar-', names(ModelData))
    } else {
      ModelVars <- NULL
    }
    shiny::selectInput(inputId='FilterVariable_1_2', label = tags$span(style='color: blue;', 'Filter Variable 1 2'), choices=c('None', names(data), ModelVars), selected='None')
  })

  if(Debug) print("Here ooooo")

  output$FilterVariable_1_3 <- shiny::renderUI({
    if(!is.null(ModelData)) {
      ModelVars <- paste0('ModelVar-', names(ModelData))
    } else {
      ModelVars <- NULL
    }
    shiny::selectInput(inputId='FilterVariable_1_3', label = tags$span(style='color: blue;', 'Filter Variable 1 3'), choices=c('None', names(data), ModelVars), selected='None')
  })

  if(Debug) print("Here ppppp")

  output$FilterVariable_1_4 <- shiny::renderUI({
    if(!is.null(ModelData)) {
      ModelVars <- paste0('ModelVar-', names(ModelData))
    } else {
      ModelVars <- NULL
    }
    shiny::selectInput(inputId='FilterVariable_1_4', label = tags$span(style='color: blue;', 'Filter Variable 1 4'), choices=c('None', names(data), ModelVars), selected='None')
  })

  if(Debug) print("Here qqqqq")

  output$FilterVariable_2_1 <- shiny::renderUI({
    if(!is.null(ModelData)) {
      ModelVars <- paste0('ModelVar-', names(ModelData))
    } else {
      ModelVars <- NULL
    }
    shiny::selectInput(inputId='FilterVariable_2_1', label = tags$span(style='color: blue;', 'Filter Variable 2 1'), choices=c('None', names(data), ModelVars), selected='None')
  })

  if(Debug) print("Here rrrrr")

  output$FilterVariable_2_2 <- shiny::renderUI({
    if(!is.null(ModelData)) {
      ModelVars <- paste0('ModelVar-', names(ModelData))
    } else {
      ModelVars <- NULL
    }
    shiny::selectInput(inputId='FilterVariable_2_2', label = tags$span(style='color: blue;', 'Filter Variable 2 2'), choices=c('None', names(data), ModelVars), selected='None')
  })

  if(Debug) print("Here sssss")

  output$FilterVariable_2_3 <- shiny::renderUI({
    if(!is.null(ModelData)) {
      ModelVars <- paste0('ModelVar-', names(ModelData))
    } else {
      ModelVars <- NULL
    }
    shiny::selectInput(inputId='FilterVariable_2_3', label = tags$span(style='color: blue;', 'Filter Variable 2 3'), choices=c('None', names(data), ModelVars), selected='None')
  })

  if(Debug) print("Here ttttt")

  output$FilterVariable_2_4 <- shiny::renderUI({
    if(!is.null(ModelData)) {
      ModelVars <- paste0('ModelVar-', names(ModelData))
    } else {
      ModelVars <- NULL
    }
    shiny::selectInput(inputId='FilterVariable_2_4', label = tags$span(style='color: blue;', 'Filter Variable 2 4'), choices=c('None', names(data), ModelVars), selected='None')
  })

  if(Debug) print("Here uuuuu")

  output$FilterVariable_3_1 <- shiny::renderUI({
    if(!is.null(ModelData)) {
      ModelVars <- paste0('ModelVar-', names(ModelData))
    } else {
      ModelVars <- NULL
    }
    shiny::selectInput(inputId='FilterVariable_3_1', label = tags$span(style='color: blue;', 'Filter Variable 3 1'), choices=c('None', names(data), ModelVars), selected='None')
  })

  if(Debug) print("Here vvvvv")

  output$FilterVariable_3_2 <- shiny::renderUI({
    if(!is.null(ModelData)) {
      ModelVars <- paste0('ModelVar-', names(ModelData))
    } else {
      ModelVars <- NULL
    }
    shiny::selectInput(inputId='FilterVariable_3_2', label = tags$span(style='color: blue;', 'Filter Variable 3 2'), choices=c('None', names(data), ModelVars), selected='None')
  })

  if(Debug) print("Here wwwww")

  output$FilterVariable_3_3 <- shiny::renderUI({
    if(!is.null(ModelData)) {
      ModelVars <- paste0('ModelVar-', names(ModelData))
    } else {
      ModelVars <- NULL
    }
    shiny::selectInput(inputId='FilterVariable_3_3', label = tags$span(style='color: blue;', 'Filter Variable 3 3'), choices=c('None', names(data), ModelVars), selected='None')
  })

  if(Debug) print("Here xxxxx")

  output$FilterVariable_3_4 <- shiny::renderUI({
    if(!is.null(ModelData)) {
      ModelVars <- paste0('ModelVar-', names(ModelData))
    } else {
      ModelVars <- NULL
    }
    shiny::selectInput(inputId='FilterVariable_3_4', label = tags$span(style='color: blue;', 'Filter Variable 3 4'), choices=c('None', names(data), ModelVars), selected='None')
  })

  if(Debug) print("Here yyyyy")

  output$FilterVariable_4_1 <- shiny::renderUI({
    if(!is.null(ModelData)) {
      ModelVars <- paste0('ModelVar-', names(ModelData))
    } else {
      ModelVars <- NULL
    }
    shiny::selectInput(inputId='FilterVariable_4_1', label = tags$span(style='color: blue;', 'Filter Variable 4 1'), choices=c('None', names(data), ModelVars), selected='None')
  })

  if(Debug) print("Here zzzzz")

  output$FilterVariable_4_2 <- shiny::renderUI({
    if(!is.null(ModelData)) {
      ModelVars <- paste0('ModelVar-', names(ModelData))
    } else {
      ModelVars <- NULL
    }
    shiny::selectInput(inputId='FilterVariable_4_2', label = tags$span(style='color: blue;', 'Filter Variable 4 2'), choices=c('None', names(data), ModelVars), selected='None')
  })

  if(Debug) print("Here aaaaaa")

  output$FilterVariable_4_3 <- shiny::renderUI({
    if(!is.null(ModelData)) {
      ModelVars <- paste0('ModelVar-', names(ModelData))
    } else {
      ModelVars <- NULL
    }
    shiny::selectInput(inputId='FilterVariable_4_3', label = tags$span(style='color: blue;', 'Filter Variable 4 3'), choices=c('None', names(data), ModelVars), selected='None')
  })

  if(Debug) print("Here bbbbbb")

  output$FilterVariable_4_4 <- shiny::renderUI({
    if(!is.null(ModelData)) {
      ModelVars <- paste0('ModelVar-', names(ModelData))
    } else {
      ModelVars <- NULL
    }
    shiny::selectInput(inputId='FilterVariable_4_4', label = tags$span(style='color: blue;', 'Filter Variable 4 4'), choices=c('None', names(data), ModelVars), selected='None')
  })

  # Reactives References
  FilterVariable_1_1 <- shiny::reactive({shiny::req(input[['FilterVariable_1_1']])})
  FilterVariable_1_2 <- shiny::reactive({shiny::req(input[['FilterVariable_1_2']])})
  FilterVariable_1_3 <- shiny::reactive({shiny::req(input[['FilterVariable_1_3']])})
  FilterVariable_1_4 <- shiny::reactive({shiny::req(input[['FilterVariable_1_4']])})
  FilterVariable_2_1 <- shiny::reactive({shiny::req(input[['FilterVariable_2_1']])})
  FilterVariable_2_2 <- shiny::reactive({shiny::req(input[['FilterVariable_2_2']])})
  FilterVariable_2_3 <- shiny::reactive({shiny::req(input[['FilterVariable_2_3']])})
  FilterVariable_2_4 <- shiny::reactive({shiny::req(input[['FilterVariable_2_4']])})
  FilterVariable_3_1 <- shiny::reactive({shiny::req(input[['FilterVariable_3_1']])})
  FilterVariable_3_2 <- shiny::reactive({shiny::req(input[['FilterVariable_3_2']])})
  FilterVariable_3_3 <- shiny::reactive({shiny::req(input[['FilterVariable_3_3']])})
  FilterVariable_3_4 <- shiny::reactive({shiny::req(input[['FilterVariable_3_4']])})
  FilterVariable_4_1 <- shiny::reactive({shiny::req(input[['FilterVariable_4_1']])})
  FilterVariable_4_2 <- shiny::reactive({shiny::req(input[['FilterVariable_4_2']])})
  FilterVariable_4_3 <- shiny::reactive({shiny::req(input[['FilterVariable_4_3']])})
  FilterVariable_4_4 <- shiny::reactive({shiny::req(input[['FilterVariable_4_4']])})

  print(':::::::: DATA NULL TESTING 13 ::::::::')
  print(data)

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Filter Logic                         ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  if(Debug) print("Here cccccc")

  # Filter Logics
  output$FilterLogic_1_1 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterLogic_1_1', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_1_1()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
  })

  print(':::::::: DATA NULL TESTING 14 ::::::::')
  print(data)

  if(Debug) print("Here dddddd")

  output$FilterLogic_1_2 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterLogic_1_2', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_1_2()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
  })

  if(Debug) print("Here eeeeee")

  output$FilterLogic_1_3 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterLogic_1_3', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_1_3()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
  })

  if(Debug) print("Here ffffff")

  output$FilterLogic_1_4 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterLogic_1_4', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_1_4()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
  })

  if(Debug) print("Here gggggg")

  output$FilterLogic_2_1 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterLogic_2_1', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_2_1()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
  })

  if(Debug) print("Here hhhhhh")

  output$FilterLogic_2_2 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterLogic_2_2', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_2_2()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
  })

  if(Debug) print("Here iiiiii")

  output$FilterLogic_2_3 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterLogic_2_3', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_2_3()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
  })

  if(Debug) print("Here jjjjjj")

  output$FilterLogic_2_4 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterLogic_2_4', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_2_4()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
  })

  if(Debug) print("Here kkkkkk")

  output$FilterLogic_3_1 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterLogic_3_1', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_3_1()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
  })

  if(Debug) print("Here llllll")

  output$FilterLogic_3_2 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterLogic_3_2', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_3_2()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
  })

  if(Debug) print("Here mmmmmm")

  output$FilterLogic_3_3 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterLogic_3_3', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_3_3()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
  })

  if(Debug) print("Here nnnnnn")

  output$FilterLogic_3_4 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterLogic_3_4', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_3_4()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
  })

  if(Debug) print("Here oooooo")

  output$FilterLogic_4_1 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterLogic_4_1', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_4_1()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
  })

  if(Debug) print("Here pppppp")

  output$FilterLogic_4_2 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterLogic_4_2', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_4_2()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
  })

  if(Debug) print("Here qqqqqq")

  output$FilterLogic_4_3 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterLogic_4_3', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_4_3()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
  })

  if(Debug) print("Here rrrrrr")

  output$FilterLogic_4_4 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterLogic_4_4', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_4_4()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
  })

  print(':::::::: DATA NULL TESTING 15 ::::::::')
  print(data)

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Filter Values     DONT OVERWRITE     ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  if(Debug) print("Here ssssss")

  # Filter Values
  #
  # 1_1_1 means Plot 1, Filter Var 1, Filter Value min
  # 1_1_2 means Plot 1, Filter Var 1, filter value max
  #
  # Plot 1
  output$FilterValue_1_1_1 <- shiny::renderUI({
    params <- list(data=data, VarName=tryCatch({input[['FilterVariable_1_1']]}, error = function(x) NULL), type=1)
    choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_1_1']])$ChoiceInput}, error = function(x) NULL)
    if(all(length(choices) == 0, length(choices$MinVal) == 0, length(choices$MaxVal) == 0, length(choices$ChoiceInput) == 0)) {
      if(length(ModelData) != 0) {
        vname <- tryCatch({stringr::str_remove(input[['FilterVariable_1_1']], 'ModelVar-')}, error = function(x) NULL)
        params <- list(data=ModelData, VarName=vname, type=1)
        choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
      }
    }
    if(Debug) print(choices)
    if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_1_1_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
  })

  print(':::::::: DATA NULL TESTING 16 ::::::::')
  print(data)

  if(Debug) print("Here tttttt")

  output$FilterValue_1_1_2 <- shiny::renderUI({
    params <- list(data=data, VarName=tryCatch({input[['FilterVariable_1_1']]}, error = function(x) NULL), type=2)
    choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_1_1']])$ChoiceInput}, error = function(x) NULL)
    if(all(length(choices) == 0, length(choices$MinVal) == 0, length(choices$MaxVal) == 0, length(choices$ChoiceInput) == 0)) {
      if(length(ModelData) != 0) {
        vname <- tryCatch({stringr::str_remove(input[['FilterVariable_1_1']], 'ModelVar-')}, error = function(x) NULL)
        params <- list(data=ModelData, VarName=vname, type=2)
        choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
      }
    }
    if(Debug) print(choices)
    if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_1_1_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
  })

  if(Debug) print("Here uuuuuu")

  output$FilterValue_1_2_1 <- shiny::renderUI({
    params <- list(data=data, VarName=tryCatch({input[['FilterVariable_1_2']]}, error = function(x) NULL), type=1)
    choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_1_2']])$ChoiceInput}, error = function(x) NULL)
    if(all(length(choices) == 0, length(choices$MinVal) == 0, length(choices$MaxVal) == 0, length(choices$ChoiceInput) == 0)) {
      if(length(ModelData) != 0) {
        vname <- tryCatch({stringr::str_remove(input[['FilterVariable_1_2']], 'ModelVar-')}, error = function(x) NULL)
        params <- list(data=ModelData, VarName=vname, type=1)
        choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
      }
    }
    if(Debug) print(choices)
    if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_1_2_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
  })

  if(Debug) print("Here vvvvvv")

  output$FilterValue_1_2_2 <- shiny::renderUI({
    params <- list(data=data, VarName=tryCatch({input[['FilterVariable_1_2']]}, error = function(x) NULL), type=2)
    choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_1_2']])$ChoiceInput}, error = function(x) NULL)
    if(all(length(choices) == 0, length(choices$MinVal) == 0, length(choices$MaxVal) == 0, length(choices$ChoiceInput) == 0)) {
      if(length(ModelData) != 0) {
        vname <- tryCatch({stringr::str_remove(input[['FilterVariable_1_2']], 'ModelVar-')}, error = function(x) NULL)
        params <- list(data=ModelData, VarName=vname, type=2)
        choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
      }
    }
    if(Debug) print(choices)
    if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_1_2_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
  })

  if(Debug) print("Here wwwwww")

  output$FilterValue_1_3_1 <- shiny::renderUI({
    params <- list(data=data, VarName=tryCatch({input[['FilterVariable_1_3']]}, error = function(x) NULL), type=1)
    choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_1_3']])$ChoiceInput}, error = function(x) NULL)
    if(all(length(choices) == 0, length(choices$MinVal) == 0, length(choices$MaxVal) == 0, length(choices$ChoiceInput) == 0)) {
      if(length(ModelData) != 0) {
        vname <- tryCatch({stringr::str_remove(input[['FilterVariable_1_3']], 'ModelVar-')}, error = function(x) NULL)
        params <- list(data=ModelData, VarName=vname, type=1)
        choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
      }
    }
    if(Debug) print(choices)
    if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_1_3_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
  })

  if(Debug) print("Here xxxxxx")

  output$FilterValue_1_3_2 <- shiny::renderUI({
    params <- list(data=data, VarName=tryCatch({input[['FilterVariable_1_3']]}, error = function(x) NULL), type=2)
    choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_1_3']])$ChoiceInput}, error = function(x) NULL)
    if(all(length(choices) == 0, length(choices$MinVal) == 0, length(choices$MaxVal) == 0, length(choices$ChoiceInput) == 0)) {
      if(length(ModelData) != 0) {
        vname <- tryCatch({stringr::str_remove(input[['FilterVariable_1_3']], 'ModelVar-')}, error = function(x) NULL)
        params <- list(data=ModelData, VarName=vname, type=2)
        choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
      }
    }
    if(Debug) print(choices)
    if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_1_3_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
  })

  if(Debug) print("Here yyyyyy")

  output$FilterValue_1_4_1 <- shiny::renderUI({
    params <- list(data=data, VarName=tryCatch({input[['FilterVariable_1_4']]}, error = function(x) NULL), type=1)
    choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_1_4']])$ChoiceInput}, error = function(x) NULL)
    if(all(length(choices) == 0, length(choices$MinVal) == 0, length(choices$MaxVal) == 0, length(choices$ChoiceInput) == 0)) {
      if(length(ModelData) != 0) {
        vname <- tryCatch({stringr::str_remove(input[['FilterVariable_1_4']], 'ModelVar-')}, error = function(x) NULL)
        params <- list(data=ModelData, VarName=vname, type=1)
        choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
      }
    }
    if(Debug) print(choices)
    if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_1_4_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
  })

  if(Debug) print("Here zzzzzz")

  output$FilterValue_1_4_2 <- shiny::renderUI({
    params <- list(data=data, VarName=tryCatch({input[['FilterVariable_1_4']]}, error = function(x) NULL), type=2)
    choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_1_4']])$ChoiceInput}, error = function(x) NULL)
    if(all(length(choices) == 0, length(choices$MinVal) == 0, length(choices$MaxVal) == 0, length(choices$ChoiceInput) == 0)) {
      if(length(ModelData) != 0) {
        vname <- tryCatch({stringr::str_remove(input[['FilterVariable_1_4']], 'ModelVar-')}, error = function(x) NULL)
        params <- list(data=ModelData, VarName=vname, type=2)
        choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
      }
    }
    if(Debug) print(choices)
    if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_1_4_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
  })

  if(Debug) print("Here aaaaaaa")

  # Plot 2
  output$FilterValue_2_1_1 <- shiny::renderUI({
    params <- list(data=data, VarName=tryCatch({input[['FilterVariable_2_1']]}, error = function(x) NULL), type=1)
    choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_2_1']])$ChoiceInput}, error = function(x) NULL)
    if(all(length(choices) == 0, length(choices$MinVal) == 0, length(choices$MaxVal) == 0, length(choices$ChoiceInput) == 0)) {
      if(length(ModelData) != 0) {
        vname <- tryCatch({stringr::str_remove(input[['FilterVariable_2_1']], 'ModelVar-')}, error = function(x) NULL)
        params <- list(data=ModelData, VarName=vname, type=1)
        choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
      }
    }
    if(Debug) print(choices)
    if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_2_1_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
  })

  if(Debug) print("Here bbbbbbb")

  output$FilterValue_2_1_2 <- shiny::renderUI({
    params <- list(data=data, VarName=tryCatch({input[['FilterVariable_2_1']]}, error = function(x) NULL), type=2)
    choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_2_1']])$ChoiceInput}, error = function(x) NULL)
    if(all(length(choices) == 0, length(choices$MinVal) == 0, length(choices$MaxVal) == 0, length(choices$ChoiceInput) == 0)) {
      if(length(ModelData) != 0) {
        vname <- tryCatch({stringr::str_remove(input[['FilterVariable_2_1']], 'ModelVar-')}, error = function(x) NULL)
        params <- list(data=ModelData, VarName=vname, type=2)
        choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
      }
    }
    if(Debug) print(choices)
    if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_2_1_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
  })

  if(Debug) print("Here ccccccc")

  output$FilterValue_2_2_1 <- shiny::renderUI({
    params <- list(data=data, VarName=tryCatch({input[['FilterVariable_2_2']]}, error = function(x) NULL), type=1)
    choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_2_2']])$ChoiceInput}, error = function(x) NULL)
    if(all(length(choices) == 0, length(choices$MinVal) == 0, length(choices$MaxVal) == 0, length(choices$ChoiceInput) == 0)) {
      if(length(ModelData) != 0) {
        vname <- tryCatch({stringr::str_remove(input[['FilterVariable_2_2']], 'ModelVar-')}, error = function(x) NULL)
        params <- list(data=ModelData, VarName=vname, type=1)
        choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
      }
    }
    if(Debug) print(choices)
    if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_2_2_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
  })

  if(Debug) print("Here ddddddd")

  output$FilterValue_2_2_2 <- shiny::renderUI({
    params <- list(data=data, VarName=tryCatch({input[['FilterVariable_2_2']]}, error = function(x) NULL), type=2)
    choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_2_2']])$ChoiceInput}, error = function(x) NULL)
    if(all(length(choices) == 0, length(choices$MinVal) == 0, length(choices$MaxVal) == 0, length(choices$ChoiceInput) == 0)) {
      if(length(ModelData) != 0) {
        vname <- tryCatch({stringr::str_remove(input[['FilterVariable_2_2']], 'ModelVar-')}, error = function(x) NULL)
        params <- list(data=ModelData, VarName=vname, type=2)
        choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
      }
    }
    if(Debug) print(choices)
    if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_2_2_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
  })

  if(Debug) print("Here eeeeeee")

  output$FilterValue_2_3_1 <- shiny::renderUI({
    params <- list(data=data, VarName=tryCatch({input[['FilterVariable_2_3']]}, error = function(x) NULL), type=1)
    choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_2_3']])$ChoiceInput}, error = function(x) NULL)
    if(all(length(choices) == 0, length(choices$MinVal) == 0, length(choices$MaxVal) == 0, length(choices$ChoiceInput) == 0)) {
      if(length(ModelData) != 0) {
        vname <- tryCatch({stringr::str_remove(input[['FilterVariable_2_3']], 'ModelVar-')}, error = function(x) NULL)
        params <- list(data=ModelData, VarName=vname, type=1)
        choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
      }
    }
    if(Debug) print(choices)
    if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_2_3_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
  })

  if(Debug) print("Here fffffff")

  output$FilterValue_2_3_2 <- shiny::renderUI({
    params <- list(data=data, VarName=tryCatch({input[['FilterVariable_2_3']]}, error = function(x) NULL), type=2)
    choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_2_3']])$ChoiceInput}, error = function(x) NULL)
    if(all(length(choices) == 0, length(choices$MinVal) == 0, length(choices$MaxVal) == 0, length(choices$ChoiceInput) == 0)) {
      if(length(ModelData) != 0) {
        vname <- tryCatch({stringr::str_remove(input[['FilterVariable_2_3']], 'ModelVar-')}, error = function(x) NULL)
        params <- list(data=ModelData, VarName=vname, type=2)
        choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
      }
    }
    if(Debug) print(choices)
    if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_2_3_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
  })

  if(Debug) print("Here ggggggg")

  output$FilterValue_2_4_1 <- shiny::renderUI({
    params <- list(data=data, VarName=tryCatch({input[['FilterVariable_2_4']]}, error = function(x) NULL), type=1)
    choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_2_4']])$ChoiceInput}, error = function(x) NULL)
    if(all(length(choices) == 0, length(choices$MinVal) == 0, length(choices$MaxVal) == 0, length(choices$ChoiceInput) == 0)) {
      if(length(ModelData) != 0) {
        vname <- tryCatch({stringr::str_remove(input[['FilterVariable_2_4']], 'ModelVar-')}, error = function(x) NULL)
        params <- list(data=ModelData, VarName=vname, type=1)
        choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
      }
    }
    if(Debug) print(choices)
    if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_2_4_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
  })

  if(Debug) print("Here hhhhhhh")

  output$FilterValue_2_4_2 <- shiny::renderUI({
    params <- list(data=data, VarName=tryCatch({input[['FilterVariable_2_4']]}, error = function(x) NULL), type=2)
    choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_2_4']])$ChoiceInput}, error = function(x) NULL)
    if(all(length(choices) == 0, length(choices$MinVal) == 0, length(choices$MaxVal) == 0, length(choices$ChoiceInput) == 0)) {
      if(length(ModelData) != 0) {
        vname <- tryCatch({stringr::str_remove(input[['FilterVariable_2_4']], 'ModelVar-')}, error = function(x) NULL)
        params <- list(data=ModelData, VarName=vname, type=2)
        choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
      }
    }
    if(Debug) print(choices)
    if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_2_4_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
  })

  if(Debug) print("Here qqqqqqq")

  # Plot 3
  output$FilterValue_3_1_1 <- shiny::renderUI({
    params <- list(data=data, VarName=tryCatch({input[['FilterVariable_3_1']]}, error = function(x) NULL), type=1)
    choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_3_1']])$ChoiceInput}, error = function(x) NULL)
    if(all(length(choices) == 0, length(choices$MinVal) == 0, length(choices$MaxVal) == 0, length(choices$ChoiceInput) == 0)) {
      if(length(ModelData) != 0) {
        vname <- tryCatch({stringr::str_remove(input[['FilterVariable_3_1']], 'ModelVar-')}, error = function(x) NULL)
        params <- list(data=ModelData, VarName=vname, type=1)
        choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
      }
    }
    if(Debug) print(choices)
    if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_3_1_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
  })

  if(Debug) print("Here rrrrrrr")

  output$FilterValue_3_1_2 <- shiny::renderUI({
    params <- list(data=data, VarName=tryCatch({input[['FilterVariable_3_1']]}, error = function(x) NULL), type=2)
    choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_3_1']])$ChoiceInput}, error = function(x) NULL)
    if(all(length(choices) == 0, length(choices$MinVal) == 0, length(choices$MaxVal) == 0, length(choices$ChoiceInput) == 0)) {
      if(length(ModelData) != 0) {
        vname <- tryCatch({stringr::str_remove(input[['FilterVariable_3_1']], 'ModelVar-')}, error = function(x) NULL)
        params <- list(data=ModelData, VarName=vname, type=2)
        choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
      }
    }
    if(Debug) print(choices)
    if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_3_1_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
  })

  if(Debug) print("Here sssssss")

  output$FilterValue_3_2_1 <- shiny::renderUI({
    params <- list(data=data, VarName=tryCatch({input[['FilterVariable_3_2']]}, error = function(x) NULL), type=1)
    choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_3_2']])$ChoiceInput}, error = function(x) NULL)
    if(all(length(choices) == 0, length(choices$MinVal) == 0, length(choices$MaxVal) == 0, length(choices$ChoiceInput) == 0)) {
      if(length(ModelData) != 0) {
        vname <- tryCatch({stringr::str_remove(input[['FilterVariable_3_2']], 'ModelVar-')}, error = function(x) NULL)
        params <- list(data=ModelData, VarName=vname, type=1)
        choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
      }
    }
    if(Debug) print(choices)
    if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_3_2_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
  })

  if(Debug) print("Here ttttttt")

  output$FilterValue_3_2_2 <- shiny::renderUI({
    params <- list(data=data, VarName=tryCatch({input[['FilterVariable_3_2']]}, error = function(x) NULL), type=2)
    choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_3_2']])$ChoiceInput}, error = function(x) NULL)
    if(all(length(choices) == 0, length(choices$MinVal) == 0, length(choices$MaxVal) == 0, length(choices$ChoiceInput) == 0)) {
      if(length(ModelData) != 0) {
        vname <- tryCatch({stringr::str_remove(input[['FilterVariable_3_2']], 'ModelVar-')}, error = function(x) NULL)
        params <- list(data=ModelData, VarName=vname, type=2)
        choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
      }
    }
    if(Debug) print(choices)
    if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_3_2_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
  })

  if(Debug) print("Here uuuuuuu")

  output$FilterValue_3_3_1 <- shiny::renderUI({
    params <- list(data=data, VarName=tryCatch({input[['FilterVariable_3_3']]}, error = function(x) NULL), type=1)
    choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_3_3']])$ChoiceInput}, error = function(x) NULL)
    if(all(length(choices) == 0, length(choices$MinVal) == 0, length(choices$MaxVal) == 0, length(choices$ChoiceInput) == 0)) {
      if(length(ModelData) != 0) {
        vname <- tryCatch({stringr::str_remove(input[['FilterVariable_3_3']], 'ModelVar-')}, error = function(x) NULL)
        params <- list(data=ModelData, VarName=vname, type=1)
        choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
      }
    }
    if(Debug) print(choices)
    if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_3_3_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
  })

  if(Debug) print("Here vvvvvvv")

  output$FilterValue_3_3_2 <- shiny::renderUI({
    params <- list(data=data, VarName=tryCatch({input[['FilterVariable_3_3']]}, error = function(x) NULL), type=2)
    choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_3_3']])$ChoiceInput}, error = function(x) NULL)
    if(all(length(choices) == 0, length(choices$MinVal) == 0, length(choices$MaxVal) == 0, length(choices$ChoiceInput) == 0)) {
      if(length(ModelData) != 0) {
        vname <- tryCatch({stringr::str_remove(input[['FilterVariable_3_3']], 'ModelVar-')}, error = function(x) NULL)
        params <- list(data=ModelData, VarName=vname, type=2)
        choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
      }
    }
    if(Debug) print(choices)
    if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_3_3_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
  })

  if(Debug) print("Here wwwwwww")

  output$FilterValue_3_4_1 <- shiny::renderUI({
    params <- list(data=data, VarName=tryCatch({input[['FilterVariable_3_4']]}, error = function(x) NULL), type=1)
    choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_3_4']])$ChoiceInput}, error = function(x) NULL)
    if(all(length(choices) == 0, length(choices$MinVal) == 0, length(choices$MaxVal) == 0, length(choices$ChoiceInput) == 0)) {
      if(length(ModelData) != 0) {
        vname <- tryCatch({stringr::str_remove(input[['FilterVariable_3_4']], 'ModelVar-')}, error = function(x) NULL)
        params <- list(data=ModelData, VarName=vname, type=1)
        choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
      }
    }
    if(Debug) print(choices)
    if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_3_4_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
  })

  if(Debug) print("Here xxxxxxx")

  output$FilterValue_3_4_2 <- shiny::renderUI({
    params <- list(data=data, VarName=tryCatch({input[['FilterVariable_3_4']]}, error = function(x) NULL), type=2)
    choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_3_4']])$ChoiceInput}, error = function(x) NULL)
    if(all(length(choices) == 0, length(choices$MinVal) == 0, length(choices$MaxVal) == 0, length(choices$ChoiceInput) == 0)) {
      if(length(ModelData) != 0) {
        vname <- tryCatch({stringr::str_remove(input[['FilterVariable_3_4']], 'ModelVar-')}, error = function(x) NULL)
        params <- list(data=ModelData, VarName=vname, type=2)
        choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
      }
    }
    if(Debug) print(choices)
    if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_3_4_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
  })

  if(Debug) print("Here yyyyyyy")

  # Plot 4
  output$FilterValue_4_1_1 <- shiny::renderUI({
    params <- list(data=data, VarName=tryCatch({input[['FilterVariable_4_1']]}, error = function(x) NULL), type=1)
    choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_4_1']])$ChoiceInput}, error = function(x) NULL)
    if(all(length(choices) == 0, length(choices$MinVal) == 0, length(choices$MaxVal) == 0, length(choices$ChoiceInput) == 0)) {
      if(length(ModelData) != 0) {
        vname <- tryCatch({stringr::str_remove(input[['FilterVariable_4_1']], 'ModelVar-')}, error = function(x) NULL)
        params <- list(data=ModelData, VarName=vname, type=1)
        choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
      }
    }
    if(Debug) print(choices)
    if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_4_1_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
  })

  if(Debug) print("Here zzzzzzz")

  output$FilterValue_4_1_2 <- shiny::renderUI({
    params <- list(data=data, VarName=tryCatch({input[['FilterVariable_4_1']]}, error = function(x) NULL), type=2)
    choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_4_1']])$ChoiceInput}, error = function(x) NULL)
    if(all(length(choices) == 0, length(choices$MinVal) == 0, length(choices$MaxVal) == 0, length(choices$ChoiceInput) == 0)) {
      if(length(ModelData) != 0) {
        vname <- tryCatch({stringr::str_remove(input[['FilterVariable_4_1']], 'ModelVar-')}, error = function(x) NULL)
        params <- list(data=ModelData, VarName=vname, type=2)
        choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
      }
    }
    if(Debug) print(choices)
    if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_4_1_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
  })

  if(Debug) print("Here aaaaaaaa")

  output$FilterValue_4_2_1 <- shiny::renderUI({
    params <- list(data=data, VarName=tryCatch({input[['FilterVariable_4_2']]}, error = function(x) NULL), type=1)
    choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_4_2']])$ChoiceInput}, error = function(x) NULL)
    if(all(length(choices) == 0, length(choices$MinVal) == 0, length(choices$MaxVal) == 0, length(choices$ChoiceInput) == 0)) {
      if(length(ModelData) != 0) {
        vname <- tryCatch({stringr::str_remove(input[['FilterVariable_4_2']], 'ModelVar-')}, error = function(x) NULL)
        params <- list(data=ModelData, VarName=vname, type=1)
        choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
      }
    }
    if(Debug) print(choices)
    if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_4_2_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
  })

  if(Debug) print("Here bbbbbbbb")

  output$FilterValue_4_2_2 <- shiny::renderUI({
    params <- list(data=data, VarName=tryCatch({input[['FilterVariable_4_2']]}, error = function(x) NULL), type=2)
    choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_4_2']])$ChoiceInput}, error = function(x) NULL)
    if(all(length(choices) == 0, length(choices$MinVal) == 0, length(choices$MaxVal) == 0, length(choices$ChoiceInput) == 0)) {
      if(length(ModelData) != 0) {
        vname <- tryCatch({stringr::str_remove(input[['FilterVariable_4_2']], 'ModelVar-')}, error = function(x) NULL)
        params <- list(data=ModelData, VarName=vname, type=2)
        choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
      }
    }
    if(Debug) print(choices)
    if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_4_2_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
  })

  if(Debug) print("Here cccccccc")

  output$FilterValue_4_3_1 <- shiny::renderUI({
    params <- list(data=data, VarName=tryCatch({input[['FilterVariable_4_3']]}, error = function(x) NULL), type=1)
    choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_4_3']])$ChoiceInput}, error = function(x) NULL)
    if(all(length(choices) == 0, length(choices$MinVal) == 0, length(choices$MaxVal) == 0, length(choices$ChoiceInput) == 0)) {
      if(length(ModelData) != 0) {
        vname <- tryCatch({stringr::str_remove(input[['FilterVariable_4_3']], 'ModelVar-')}, error = function(x) NULL)
        params <- list(data=ModelData, VarName=vname, type=1)
        choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
      }
    }
    if(Debug) print(choices)
    if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_4_3_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
  })

  if(Debug) print("Here dddddddd")

  output$FilterValue_4_3_2 <- shiny::renderUI({
    params <- list(data=data, VarName=tryCatch({input[['FilterVariable_4_3']]}, error = function(x) NULL), type=2)
    choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_4_3']])$ChoiceInput}, error = function(x) NULL)
    if(all(length(choices) == 0, length(choices$MinVal) == 0, length(choices$MaxVal) == 0, length(choices$ChoiceInput) == 0)) {
      if(length(ModelData) != 0) {
        vname <- tryCatch({stringr::str_remove(input[['FilterVariable_4_3']], 'ModelVar-')}, error = function(x) NULL)
        params <- list(data=ModelData, VarName=vname, type=2)
        choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
      }
    }
    if(Debug) print(choices)
    if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_4_3_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
  })

  if(Debug) print("Here eeeeeeee")

  output$FilterValue_4_4_1 <- shiny::renderUI({
    params <- list(data=data, VarName=tryCatch({input[['FilterVariable_4_4']]}, error = function(x) NULL), type=1)
    choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_4_4']])$ChoiceInput}, error = function(x) NULL)
    if(all(length(choices) == 0, length(choices$MinVal) == 0, length(choices$MaxVal) == 0, length(choices$ChoiceInput) == 0)) {
      if(length(ModelData) != 0) {
        vname <- tryCatch({stringr::str_remove(input[['FilterVariable_4_4']], 'ModelVar-')}, error = function(x) NULL)
        params <- list(data=ModelData, VarName=vname, type=1)
        choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
      }
    }
    if(Debug) print(choices)
    if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_4_4_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
  })

  if(Debug) print("Here ffffffff")

  output$FilterValue_4_4_2 <- shiny::renderUI({
    params <- list(data=data, VarName=tryCatch({input[['FilterVariable_4_4']]}, error = function(x) NULL), type=2)
    choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_4_4']])$ChoiceInput}, error = function(x) NULL)
    if(all(length(choices) == 0, length(choices$MinVal) == 0, length(choices$MaxVal) == 0, length(choices$ChoiceInput) == 0)) {
      if(length(ModelData) != 0) {
        vname <- tryCatch({stringr::str_remove(input[['FilterVariable_4_4']], 'ModelVar-')}, error = function(x) NULL)
        params <- list(data=ModelData, VarName=vname, type=2)
        choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
      }
    }
    if(Debug) print(choices)
    if(Debug) print(do.call(RemixAutoML::GetFilterValueLabel, params))
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_4_4_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
  })

  print(':::::::: DATA NULL TESTING 17 ::::::::')
  print(data)

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Print Code to UI                     ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  shiny::observeEvent(input$PrintCodeButton, {
    if(Debug) print('Print Code UI Begin')
    if(Debug) print(paste0('Check if CodeCollection exists: exists = ', exists('CodeCollection')))
    if(exists('CodeCollection')) {
      output$PrintCode <- shiny::renderPrint({
        # text_code <- c()
        # for(i in seq_along(CodeCollection)) {
        #   text_code <- c(
        #     text_code,
        #     shiny::HTML(paste0('<br><code>', CodeCollection[[i]]), '<br/><code/>'))
        # }
        #shiny::HTML(paste0(unlist(CodeCollection), sep = '<br/><code/>'))
        shiny::HTML(paste0(unlist(CodeCollection), sep = '<br/>'))
        #text_code
      })
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = 'No Code Collected, Yet', type = NULL, btn_labels = "warning", btn_colors = "yellow", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  })

  # ----

  # ----

  # Save Plot to File
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Downloadable csv of selected dataset ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  # shiny::observeEvent(input$SavePlot, {
  #
  #   # Check if plot exists and is valid
  #
  #   # Then Save
  #   output$SavePlot <- shiny::downloadHandler(
  #     filename = function() {
  #       paste(data, ".csv", sep = "")
  #     },
  #     content = function(file) {
  #       data.table::fwrite(datasetInput(), file, row.names = FALSE)
  #     })
  # })

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Create Plot                          ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  shiny::observeEvent(eventExpr = input[['TrendPlotExecute']], {

    # Initallize All Variables so that when referenced it's either
    #  1. NULL
    #  2. The correct type of value
    #  3. The correct type of vector with only valid elements inside
    #  Note: user should know which type is which as some values can be a scalar or a vector
    #        context is clear from initialization of variables

    print(':::::::: DATA NULL TESTING 18 ::::::::')
    print(data)

    # Debug
    if(Debug) for(zzzz in 1:4) print(':: :: CREATE PLOTS :: ::')

    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    # Determine Which Plots to Build       ----
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    if(Debug) print('Here 11: BEGIN PLOTTING LOOP')

    PlotCollectionList <- list()
    CodeCollection <- list()

    # Identify which plots to build
    NumPlots <- c()
    Plot1 <<- RemixAutoML::ReturnParam(xx=tryCatch({input[['Plot1']]}, error=function(x) NULL), Type='character', Default=NULL)
    Plot2 <<- RemixAutoML::ReturnParam(xx=tryCatch({input[['Plot2']]}, error=function(x) NULL), Type='character', Default=NULL)
    Plot3 <<- RemixAutoML::ReturnParam(xx=tryCatch({input[['Plot3']]}, error=function(x) NULL), Type='character', Default=NULL)
    Plot4 <<- RemixAutoML::ReturnParam(xx=tryCatch({input[['Plot4']]}, error=function(x) NULL), Type='character', Default=NULL)
    if(length(PlotObjectHome$Plot_1$Plot_1) != 0) NumPlots[length(NumPlots) + 1L] <- 1
    if(length(PlotObjectHome$Plot_2$Plot_2) != 0) NumPlots[length(NumPlots) + 1L] <- 2
    if(length(PlotObjectHome$Plot_3$Plot_3) != 0) NumPlots[length(NumPlots) + 1L] <- 3
    if(length(PlotObjectHome$Plot_4$Plot_4) != 0) NumPlots[length(NumPlots) + 1L] <- 4

    # PlotType Determination
    if(Debug) {print('Dragula Details'); print(input[['PlotTypeDragula']])}
    ULP <- input[['PlotTypeDragula']][['target']][['UpperLeftPlot']]
    BLP <- input[['PlotTypeDragula']][['target']][['BottomLeftPlot']]
    URP <- input[['PlotTypeDragula']][['target']][['UpperRightPlot']]
    BRP <- input[['PlotTypeDragula']][['target']][['BottomRightPlot']]
    PlotBuilds <- c()
    if(!is.null(ULP) && ULP != "") {
      if(ULP == 'Plot_1') PlotBuilds[length(PlotBuilds) + 1L] <- 1
      if(ULP == 'Plot_2') PlotBuilds[length(PlotBuilds) + 1L] <- 2
      if(ULP == 'Plot_3') PlotBuilds[length(PlotBuilds) + 1L] <- 3
      if(ULP == 'Plot_4') PlotBuilds[length(PlotBuilds) + 1L] <- 4
      print(paste0('UpperLeftPlot = ', ULP))
    }
    if(!is.null(BLP) && BLP != "") {
      if(BLP == 'Plot_1') PlotBuilds[length(PlotBuilds) + 1L] <- 1
      if(BLP == 'Plot_2') PlotBuilds[length(PlotBuilds) + 1L] <- 2
      if(BLP == 'Plot_3') PlotBuilds[length(PlotBuilds) + 1L] <- 3
      if(BLP == 'Plot_4') PlotBuilds[length(PlotBuilds) + 1L] <- 4
      print(paste0('BottomLeftPlot = ', BLP))
    }
    if(!is.null(URP) && URP != "") {
      if(URP == 'Plot_1') PlotBuilds[length(PlotBuilds) + 1L] <- 1
      if(URP == 'Plot_2') PlotBuilds[length(PlotBuilds) + 1L] <- 2
      if(URP == 'Plot_3') PlotBuilds[length(PlotBuilds) + 1L] <- 3
      if(URP == 'Plot_4') PlotBuilds[length(PlotBuilds) + 1L] <- 4
      print(paste0('UpperRightPlot = ', URP))
    }
    if(!is.null(BRP) && BRP != "") {
      if(BRP == 'Plot_1') PlotBuilds[length(PlotBuilds) + 1L] <- 1
      if(BRP == 'Plot_2') PlotBuilds[length(PlotBuilds) + 1L] <- 2
      if(BRP == 'Plot_3') PlotBuilds[length(PlotBuilds) + 1L] <- 3
      if(BRP == 'Plot_4') PlotBuilds[length(PlotBuilds) + 1L] <- 4
      print(paste0('UpperRightPlot = ', BRP))
    }

    # Intersection of both
    PlotRefs <- intersect(NumPlots,PlotBuilds)

    # Global Settings
    PlotObjectHome[['GlobalSettings']][['PlotWidth']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[['PlotWidth']]}, error=function(x) NULL), Type='numeric', Default=1550L)
    PlotObjectHome[['GlobalSettings']][['PlotHeight']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[['PlotHeight']]}, error=function(x) NULL), Type='numeric', Default=500L)

    # Initialize PlotObjectHome List
    for(run in PlotRefs) {

      # Debug
      if(Debug) print('Define PlotObjectHome values for variables :: START ::')

      # MetaData
      PlotObjectHome[[paste0('Plot_', run)]][['DataSource']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('DataSource', run)]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['PlotType']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('PlotType', run)]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['UpdateMethod']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('UpdateMethod', run)]]}, error=function(x) NULL), Type='character', Default=NULL)

      # Data Usage
      PlotObjectHome[[paste0('Plot_', run)]][['SampleSize']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('SampleSize', run)]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['NumberGroupsDisplay']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('NumberGroupsDisplay', run)]]}, error=function(x) NULL), Type='character', Default=NULL)

      # Variable Selection
      PlotObjectHome[[paste0('Plot_', run)]][['YVar']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('YVar', run)]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['YTicks']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('YTicks', run)]]}, error=function(x) NULL), Type='character', Default='Default')
      PlotObjectHome[[paste0('Plot_', run)]][['XVar']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('XVar', run)]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['XTicks']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('XTicks', run)]]}, error=function(x) NULL), Type='character', Default='Default')
      PlotObjectHome[[paste0('Plot_', run)]][['CorVariables']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('CorVariables', run)]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['CorMethod']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('CorMethod', run)]]}, error=function(x) NULL), Type='character', Default='pearson')
      PlotObjectHome[[paste0('Plot_', run)]][['PDP_Variable']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('PDP_Variable', run)]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['ScoreVar']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('ScoreVar', run)]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['GroupVars']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('GroupVars', run)]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['Levels1']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('Levels_',run,'_1')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['Levels2']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('Levels_',run,'_1')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['Levels3']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('Levels_',run,'_1')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['SizeVars']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('SizeVars', run)]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FacetVar1']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('FacetVar1', run)]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FacetVar2']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('FacetVar2', run)]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FilterVar1']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('FilterVar_',run, '_1')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FilterVar2']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('FilterVar_',run, '_2')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FilterVar3']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('FilterVar_',run, '_3')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FilterVar4']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('FilterVar_',run, '_4')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FilterLogic1']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('FilterLogic_',run, '_1')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FilterLogic2']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('FilterLogic_',run, '_2')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FilterLogic3']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('FilterLogic_',run, '_3')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FilterLogic4']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('FilterLogic_',run, '_4')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FilterValue_1_1']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',run, '_1_1')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FilterValue_1_2']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',run, '_1_2')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FilterValue_1_3']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',run, '_2_1')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FilterValue_1_4']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',run, '_2_2')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FilterValue_2_1']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',run, '_3_1')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FilterValue_2_2']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',run, '_3_2')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FilterValue_2_3']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',run, '_4_1')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FilterValue_2_4']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',run, '_4_2')]]}, error=function(x) NULL), Type='character', Default=NULL)

      # Plot Formatting
      PlotObjectHome[[paste0('Plot_', run)]][['AngleY']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('AngleY', run)]]}, error=function(x) NULL), Type='numeric', Default=0L)
      PlotObjectHome[[paste0('Plot_', run)]][['AngleX']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('AngleX', run)]]}, error=function(x) NULL), Type='numeric', Default=90L)
      PlotObjectHome[[paste0('Plot_', run)]][['TextSize']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('TextSize', run)]]}, error=function(x) NULL), Type='numeric', Default=15L)
      PlotObjectHome[[paste0('Plot_', run)]][['OutlierSize']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('OutlierSize', run)]]}, error=function(x) NULL), Type='numeric', Default=0.01)
      PlotObjectHome[[paste0('Plot_', run)]][['LegendPosition']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('LegendPosition', run)]]}, error=function(x) NULL), Type='character', Default='right')
      PlotObjectHome[[paste0('Plot_', run)]][['LegendBorderSize']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('LegendBorderSize', run)]]}, error=function(x) NULL), Type='numeric', Default=0.01)
      PlotObjectHome[[paste0('Plot_', run)]][['LegendLineType']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('LegendLineType', run)]]}, error=function(x) NULL), Type='character', Default='solid')
      PlotObjectHome[[paste0('Plot_', run)]][['TextColor']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('TextColor', run)]]}, error=function(x) NULL), Type='character', Default='darkblue')
      PlotObjectHome[[paste0('Plot_', run)]][['ChartColor']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('ChartColor', run)]]}, error=function(x) NULL), Type='character', Default='lightsteelblue1')
      PlotObjectHome[[paste0('Plot_', run)]][['GridColor']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('GridColor', run)]]}, error=function(x) NULL), Type='character', Default='white')
      PlotObjectHome[[paste0('Plot_', run)]][['BackGroundColor']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('BackGroundColor', run)]]}, error=function(x) NULL), Type='character', Default='gray95')
      PlotObjectHome[[paste0('Plot_', run)]][['BorderColor']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('BorderColor', run)]]}, error=function(x) NULL), Type='character', Default='darkblue')
      PlotObjectHome[[paste0('Plot_', run)]][['OutlierColor']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('OutlierColor', run)]]}, error=function(x) NULL), Type='character', Default='blue')
      PlotObjectHome[[paste0('Plot_', run)]][['FillColor']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('FillColor', run)]]}, error=function(x) NULL), Type='character', Default='gray')
      PlotObjectHome[[paste0('Plot_', run)]][['SubTitleColor']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('SubTitleColor', run)]]}, error=function(x) NULL), Type='character', Default='blue')

      # Plot Extras
      PlotObjectHome[[paste0('Plot_', run)]][['ShapAgg']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('ShapAgg', run)]]}, error=function(x) NULL), Type='character', Default='meanabs')
      PlotObjectHome[[paste0('Plot_', run)]][['GamFitScatter']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('GamFitScatter', run)]]}, error=function(x) NULL), Type='logical', Default=FALSE)
      PlotObjectHome[[paste0('Plot_', run)]][['NumberBins']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('NumberBins', run)]]}, error=function(x) NULL), Type='numeric', Default=30L)
      PlotObjectHome[[paste0('Plot_', run)]][['Percentile_Buckets']] <<- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('Percentile_Buckets', run)]]}, error=function(x) NULL), Type='numeric', Default=20L)

      # Debug
      if(Debug) print('Define PlotObjectHome values for variables :: END ::')
    }

    # ----

    # ----

    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    # Loop Through Plot Builds             ----
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    for(run in PlotRefs) {

      # Code ID
      CodeCollection[[run]] <- run

      # Debugging
      if(run == 1) tryCatch({print(data)}, error = function(x) print('::: run = 1: printing data caused an error :::'))
      if(run == 2) tryCatch({print(data)}, error = function(x) print('::: run = 2: printing data caused an error :::'))
      if(run == 3) tryCatch({print(data)}, error = function(x) print('::: run = 3: printing data caused an error :::'))
      if(run == 4) tryCatch({print(data)}, error = function(x) print('::: run = 4: printing data caused an error :::'))

      # Debug
      if(Debug) print('Define NamedValue Objects for variables :: START ::')

      # Define NamedValue Objects for variables
      if(1 == 1) {

        # Variables as objects
        DataSource <- PlotObjectHome[[paste0('Plot_', run)]][['DataSource']]
        PlotType <- PlotObjectHome[[paste0('Plot_', run)]][['PlotType']]
        UpdateMethod <- PlotObjectHome[[paste0('Plot_', run)]][['UpdateMethod']]

        # Data Usage
        SampleSize <- PlotObjectHome[[paste0('Plot_', run)]][['SampleSize']]
        NumberGroupsDisplay <- PlotObjectHome[[paste0('Plot_', run)]][['NumberGroupsDisplay']]

        # Variable Selection
        YVar <- PlotObjectHome[[paste0('Plot_', run)]][['YVar']]
        YTicks <- PlotObjectHome[[paste0('Plot_', run)]][['YTicks']]
        XVar <- PlotObjectHome[[paste0('Plot_', run)]][['XVar']]
        XTicks <- PlotObjectHome[[paste0('Plot_', run)]][['XTicks']]
        CorVariables <- PlotObjectHome[[paste0('Plot_', run)]][['CorVariables']]
        CorMethod <- PlotObjectHome[[paste0('Plot_', run)]][['CorMethod']]
        PDP_Variable <- PlotObjectHome[[paste0('Plot_', run)]][['PDP_Variable']]
        ScoreVar <- PlotObjectHome[[paste0('Plot_', run)]][['ScoreVar']]
        GroupVars <- PlotObjectHome[[paste0('Plot_', run)]][['GroupVars']]
        Levels1 <- PlotObjectHome[[paste0('Plot_', run)]][['Levels1']]
        Levels2 <- PlotObjectHome[[paste0('Plot_', run)]][['Levels2']]
        Levels3 <- PlotObjectHome[[paste0('Plot_', run)]][['Levels3']]
        SizeVars <- PlotObjectHome[[paste0('Plot_', run)]][['SizeVars']]
        FacetVar1 <- PlotObjectHome[[paste0('Plot_', run)]][['FacetVar1']]
        FacetVar2 <- PlotObjectHome[[paste0('Plot_', run)]][['FacetVar2']]
        FilterVar1 <- PlotObjectHome[[paste0('Plot_', run)]][['FilterVar1']]
        FilterVar2 <- PlotObjectHome[[paste0('Plot_', run)]][['FilterVar2']]
        FilterVar3 <- PlotObjectHome[[paste0('Plot_', run)]][['FilterVar3']]
        FilterVar4 <- PlotObjectHome[[paste0('Plot_', run)]][['FilterVar4']]
        FilterLogic1 <- PlotObjectHome[[paste0('Plot_', run)]][['FilterLogic1']]
        FilterLogic2 <- PlotObjectHome[[paste0('Plot_', run)]][['FilterLogic2']]
        FilterLogic3 <- PlotObjectHome[[paste0('Plot_', run)]][['FilterLogic3']]
        FilterLogic4 <- PlotObjectHome[[paste0('Plot_', run)]][['FilterLogic4']]
        FilterValue_1_1 <- PlotObjectHome[[paste0('Plot_', run)]][['FilterValue_1_1']]
        FilterValue_1_2 <- PlotObjectHome[[paste0('Plot_', run)]][['FilterValue_1_2']]
        FilterValue_1_3 <- PlotObjectHome[[paste0('Plot_', run)]][['FilterValue_1_3']]
        FilterValue_1_4 <- PlotObjectHome[[paste0('Plot_', run)]][['FilterValue_1_4']]
        FilterValue_2_1 <- PlotObjectHome[[paste0('Plot_', run)]][['FilterValue_2_1']]
        FilterValue_2_2 <- PlotObjectHome[[paste0('Plot_', run)]][['FilterValue_2_2']]
        FilterValue_2_3 <- PlotObjectHome[[paste0('Plot_', run)]][['FilterValue_2_3']]
        FilterValue_2_4 <- PlotObjectHome[[paste0('Plot_', run)]][['FilterValue_2_4']]

        # Plot Formatting
        PlotWidth <- PlotObjectHome[[paste0('Plot_', run)]][['PlotWidth']]
        PlotHeight <- PlotObjectHome[[paste0('Plot_', run)]][['PlotHeight']]
        AngleY <- PlotObjectHome[[paste0('Plot_', run)]][['AngleY']]
        AngleX <- PlotObjectHome[[paste0('Plot_', run)]][['AngleX']]
        TextSize <- PlotObjectHome[[paste0('Plot_', run)]][['TextSize']]
        OutlierSize <- PlotObjectHome[[paste0('Plot_', run)]][['OutlierSize']]
        LegendPosition <- PlotObjectHome[[paste0('Plot_', run)]][['LegendPosition']]
        LegendBorderSize <- PlotObjectHome[[paste0('Plot_', run)]][['LegendBorderSize']]
        LegendLineType <- PlotObjectHome[[paste0('Plot_', run)]][['LegendLineType']]
        TextColor <- PlotObjectHome[[paste0('Plot_', run)]][['TextColor']]
        ChartColor <- PlotObjectHome[[paste0('Plot_', run)]][['ChartColor']]
        GridColor <- PlotObjectHome[[paste0('Plot_', run)]][['GridColor']]
        BackGroundColor <- PlotObjectHome[[paste0('Plot_', run)]][['BackGroundColor']]
        BorderColor <- PlotObjectHome[[paste0('Plot_', run)]][['BorderColor']]
        OutlierColor <- PlotObjectHome[[paste0('Plot_', run)]][['OutlierColor']]
        FillColor <- PlotObjectHome[[paste0('Plot_', run)]][['FillColor']]
        SubTitleColor <- PlotObjectHome[[paste0('Plot_', run)]][['SubTitleColor']]

        # Plot Extras
        ShapAgg <- PlotObjectHome[[paste0('Plot_', run)]][['ShapAgg']]
        GamFitScatter <- PlotObjectHome[[paste0('Plot_', run)]][['GamFitScatter']]
        NumberBins <- PlotObjectHome[[paste0('Plot_', run)]][['NumberBins']]
        Percentile_Buckets <- PlotObjectHome[[paste0('Plot_', run)]][['Percentile_Buckets']]

        # Debug
        if(Debug) print('Define NamedValue Objects for variables :: END ::')

      }

      # ----

      # ----

      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
      # Logic Check to Build Plots           ----
      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

      # Convert back to original plottype name
      PlotType <- PlotNamesLookup[[PlotType]]

      # X & Y Variable Checks
      baseplot <- PlotType %in% c('Scatter','Copula','Line','Bar','BoxPlot','ViolinPlot','Histogram')
      if(Debug) print('Here 15.33')

      # Debugging
      if(Debug) {
        print('here yo yo')
        print(CorVariables)
        print(PlotType %in% 'CorrMatrix')
        print(length(CorVariables) == 0 || all(CorVariables %in% "") || all(CorVariables %in% 'None'))
        print('Here 15.4'); print('Args for first if condition :::: ::::'); print(YVar); print(XVar); print(PlotType); print('Args for 2nd if condition :::: ::::')
        print('First IF condition check'); print(YVar); print(XVar); print(baseplot)
        print('Second IF condition check'); print(PlotType)
        if(!is.null(XVar)) print(!any(class(data[[eval(XVar)]]) %in% c('numeric','integer'))) else print('XVar is NULL')
        if(!is.null(YVar)) print(!any(class(data[[eval(YVar)]]) %in% c('numeric','integer'))) else print('YVar is NULL')
      }

      # PLOT LOGIC CHECK:
      if(length(YVar) == 0 && length(XVar) == 0 && baseplot) {
        shinyWidgets::sendSweetAlert(session, title = NULL, text = 'You need to specify additional variables to generate additional plots', type = NULL, btn_labels = "error", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
      } else if(PlotType %chin% c('Scatter','Copula') && !any(class(data[[eval(XVar)]]) %in% c('numeric','integer')) && !any(class(data[[eval(YVar)]]) %in% c('numeric','integer'))) {
        shinyWidgets::sendSweetAlert(session, title = NULL, text = "Y and / or X-Variable needs to be a numeric or integer variable", type = NULL, btn_labels = "error", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
      } else if(PlotType %in% 'CorrMatrix' && length(CorVariables) == 0) {
        shinyWidgets::sendSweetAlert(session, title = NULL, text = "CorVariables needs to be selected to build this plot", type = NULL, btn_labels = "error", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
      } else {

        # Debug
        if(Debug) print('Here 133')

        # ----

        # ----

        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
        # Grouping Variable Management         ----
        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
        if(!(exists('SubsetList') && !is.null(SubsetList[['RunNumber']]) && SubsetList[['RunNumber']] >= 1)) {

          if(Debug) print('Here 14 a')

          SubsetList <- list()
          SubsetList[[paste0('RunNumber', run)]] <- 1L
          SubsetList[[paste0('DataPrep', run)]] <- TRUE

          if(Debug) {print('Here at 14c'); print(run)}

          SubsetList[[paste0('GroupVars', run)]] <- GroupVars
          SubsetList[[paste0('Levels_', run, '_1')]] <- Levels1
          SubsetList[[paste0('Levels_', run, '_2')]] <- Levels2
          SubsetList[[paste0('Levels_', run, '_3')]] <- Levels3
          SubsetList[[paste0('FacetVar_', run, '_1')]] <- FacetVar1
          SubsetList[[paste0('FacetVar_', run, '_2')]] <- FacetVar2
          SubsetList[[paste0('SizeVar', run)]] <- SizeVars

          # Filter Variables
          print('Filter Variables')
          print(FilterVar1)
          print(stringr::str_remove(string = FilterVar1, pattern = 'ModelVar-'))
          SubsetList[[paste0('FilterVariable_', run, '_1')]] <- if(length(FilterVar1) != 0 && FilterVar1 != 'None') stringr::str_remove(string = FilterVar1, pattern = 'ModelVar-') else 'None'
          SubsetList[[paste0('FilterVariable_', run, '_2')]] <- if(length(FilterVar2) != 0 && FilterVar2 != 'None') stringr::str_remove(string = FilterVar2, pattern = 'ModelVar-') else 'None'
          SubsetList[[paste0('FilterVariable_', run, '_3')]] <- if(length(FilterVar3) != 0 && FilterVar3 != 'None') stringr::str_remove(string = FilterVar3, pattern = 'ModelVar-') else 'None'
          SubsetList[[paste0('FilterVariable_', run, '_4')]] <- if(length(FilterVar4) != 0 && FilterVar4 != 'None') stringr::str_remove(string = FilterVar4, pattern = 'ModelVar-') else 'None'

          # Filter Logic
          print('Filter Logic')
          SubsetList[[paste0('FilterLogic_', run, '_1')]] <- FilterLogic1
          SubsetList[[paste0('FilterLogic_', run, '_2')]] <- FilterLogic2
          SubsetList[[paste0('FilterLogic_', run, '_3')]] <- FilterLogic3
          SubsetList[[paste0('FilterLogic_', run, '_3')]] <- FilterLogic4

          # Filter Values
          print('Filter Values')
          SubsetList[[paste0('FilterValue_', run, '_1_1')]] <- FilterValue_1_1
          SubsetList[[paste0('FilterValue_', run, '_1_2')]] <- FilterValue_1_2
          SubsetList[[paste0('FilterValue_', run, '_2_1')]] <- FilterValue_2_1
          SubsetList[[paste0('FilterValue_', run, '_2_2')]] <- FilterValue_2_2
          SubsetList[[paste0('FilterValue_', run, '_3_1')]] <- FilterValue_3_1
          SubsetList[[paste0('FilterValue_', run, '_3_2')]] <- FilterValue_3_2
          SubsetList[[paste0('FilterValue_', run, '_4_1')]] <- FilterValue_4_1
          SubsetList[[paste0('FilterValue_', run, '_4_2')]] <- FilterValue_4_2

          # Store Globally
          assign(x = 'SubsetList', value = SubsetList, envir = .GlobalEnv)

          # Debugging
          if(Debug) {
            print('Filter Values Done')
            print('Filter check here')
            print(SubsetList[[paste0('FilterValue_', run, '_1_1')]])
            print(SubsetList[[paste0('FilterValue_', run, '_1_2')]])
          }

        } else {

          if(Debug) print('Here 14 b')

          # MetaData
          SubsetList[[paste0('RunNumber', run)]] <- RunNumber + 1L
          SubsetList[[paste0('DataPrep', run)]] <- FALSE

          # Group Variables
          if(Debug) print('# Group Variables')
          if(Debug) print('GROUP VARIABLE CHECK HERE :::::::::::::::::::::')
          # if(Debug) {
          #   print(SubsetList[[paste0('GroupVars', run)]])
          #   print(RemixAutoML::ReturnParam(input, VarName=paste0('GroupVars', run), Type='character', Default='None', Switch=TRUE))
          #   print(!all(SubsetList[[paste0('GroupVars', run)]] == RemixAutoML::ReturnParam(input, VarName=paste0('GroupVars', run), Type='character', Default='None', Switch=TRUE)))
          # }

          # Check values
          if(!all(SubsetList[[paste0('GroupVars', run)]] == GroupVars)) {
            if(Debug) print(GroupVars)
            SubsetList[[paste0('GroupVars', run)]] <- RemixAutoML::ReturnParam(input, VarName=paste0('GroupVars', run), Type='character', Default='None', Switch=TRUE)
            SubsetList[[paste0('DataPrep', run)]] <- TRUE
            print('# Levels 1 1')
            print(Level1)
          }

          # Levels.., FacetVar1.., SizeVars
          if(!all(SubsetList[[paste0('Levels_', run, '_1')]] == Level1)) {
            SubsetList[[paste0('Levels_', run, '_1')]] <- Level1; SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('Levels_', run, '_2')]] == Level2)) {
            SubsetList[[paste0('Levels_', run, '_2')]] <- Level2; SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('Levels_', run, '_3')]] == Level3)) {
            SubsetList[[paste0('Levels_', run, '_3')]] <- Level3; SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FacetVar_', run, '_1')]] == FacetVar1)) {
            SubsetList[[paste0('FacetVar_', run, '_1')]] <- FacetVar1; SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FacetVar_', run, '_2')]] == FacetVar2)) {
            SubsetList[[paste0('FacetVar_', run, '_2')]] <- FacetVar2; SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('SizeVar', run)]] == SizeVars)) {
            SubsetList[[paste0('SizeVar', run)]] <- SizeVars; SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }

          # Filter Variables
          if(!all(SubsetList[[paste0('FilterVariable_', run, '_1')]] == FilterVariable1)) {
            SubsetList[[paste0('FilterVariable_', run, '_1')]] <- FilterVariable1; SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterVariable_', run, '_2')]] == FilterVariable2)) {
            SubsetList[[paste0('FilterVariable_', run, '_2')]] <- FilterVariable2; SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterVariable_', run, '_3')]] == FilterVariable3)) {
            SubsetList[[paste0('FilterVariable_', run, '_3')]] <- FilterVariable3; SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterVariable_', run, '_4')]] == FilterVariable4)) {
            SubsetList[[paste0('FilterVariable_', run, '_4')]] <- FilterVariable4; SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }

          # Filter Logic
          if(!all(SubsetList[[paste0('FilterLogic_', run, '_1')]] == FilterLogic1)) {
            SubsetList[[paste0('FilterLogic_', run, '_1')]] <- FilterLogic1; SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterLogic_', run, '_2')]] == FilterLogic2)) {
            SubsetList[[paste0('FilterLogic_', run, '_2')]] <- FilterLogic2; SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterLogic_', run, '_3')]] == FilterLogic3)) {
            SubsetList[[paste0('FilterLogic_', run, '_3')]] <- FilterLogic3; SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterLogic_', run, '_4')]] == FilterLogic4)) {
            SubsetList[[paste0('FilterLogic_', run, '_4')]] <- FilterLogic4; SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }

          # Filter Values
          if(!all(SubsetList[[paste0('FilterValue_', run, '_1_1')]] == FilterValue_1_1)) {
            SubsetList[[paste0('FilterValue_', run, '_1_1')]] <- FilterValue_1_1; SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterValue_', run, '_1_2')]] == FilterValue_1_2)) {
            SubsetList[[paste0('FilterValue_', run, '_1_2')]] <- FilterValue_1_2; SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterValue_', run, '_2_1')]] == FilterValue_2_1)) {
            SubsetList[[paste0('FilterValue_', run, '_2_1')]] <- FilterValue_2_1; SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterValue_', run, '_2_2')]] == FilterValue_2_2)) {
            SubsetList[[paste0('FilterValue_', run, '_2_2')]] <- FilterValue_2_2; SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterValue_', run, '_3_1')]] == FilterValue_3_1)) {
            SubsetList[[paste0('FilterValue_', run, '_3_1')]] <- FilterValue_3_1; SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterValue_', run, '_3_2')]] == FilterValue_3_2)) {
            SubsetList[[paste0('FilterValue_', run, '_3_2')]] <- FilterValue_3_2; SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterValue_', run, '_4_1')]] == FilterValue_4_1)) {
            SubsetList[[paste0('FilterValue_', run, '_4_1')]] <- FilterValue_4_1; SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterValue_', run, '_4_2')]] == FilterValue_4_2)) {
            SubsetList[[paste0('FilterValue_', run, '_4_2')]] <- FilterValue_4_2; SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          assign(x = 'SubsetList', value = SubsetList, envir = .GlobalEnv)
        }

        # Debug
        if(Debug) print('Here 16')

        # ----

        # ----

        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
        # Prepare data for plotting            ----
        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

        # Debugging
        if(Debug) {
          print('Here 17')
          for(zzzz in 1:4) print(':: :: DataPrep :: ::')
        }

        # Filter Data if DataPrep = TRUE
        if(!SubsetList[[paste0('DataPrep', run)]]) {

          # Assign data1
          if(Debug) print('Here 18 a')
          if(!PlotType %in% c('BoxPlot','ViolinPlot','Line','Bar','Scatter','Copula','CorrMatrix','Histogram')) data1 <- ModelData else data1 <- data

        } else {

          # Subset by FilterVariable
          if(Debug) print('Here 23')
          for(i in seq_len(4L)) {
            if(length(FilterVariable1) != 0L) {
              data1 <- RemixAutoML::FilterLogicData(
                FilterLogic    = get(paste0('FilterLogic', i)),
                FilterVariable = get(paste0('FilterVariable', i)),
                FilterValue    = get(paste0('FilterValue_',i,'_1')),
                FilterValue2   = get(paste0('FilterValue_',i,'_2')),
                Debug          = Debug)
              CodeCollection[[run]][[length(CodeCollection[[run]])+1L]] <- paste0("data1 <- RemixAutoML::FilterLogicData(data1, FilterLogic=", RemixAutoML:::CEP(get(paste0('FilterLogic',i))),", FilterVariable=", RemixAutoML:::CEP(get(paste0('FilterVariable',i))),", FilterValue=", RemixAutoML:::CEP(get(paste0('FilterValue_',i,'_1'))),", FilterValue2=", RemixAutoML:::CEP(get(paste0('FilterValue_',i,'_2'))),"))")
            }
          }

          # Debugging
          if(Debug) {
            print(tryCatch({PlotType}, error = function(x) print('PlotType caused an error'))) # Is PlotType coming in as NULL or logical(0) or character(0) etc.?
            print('Checking Levels_1, Levels_2, and Levels_3'); print(SubsetList[[paste0('Levels_', run, '_1')]]); print(SubsetList[[paste0('Levels_', run, '_2')]]); print(SubsetList[[paste0('Levels_', run, '_3')]])
            print('Checking YVar(), XVar(), and DateVar()'); print(tryCatch({YVarList[[paste0('YVar', run)]]}, error = function(x) NULL)); print(tryCatch({XVarList[[paste0('XVar', run)]]}, error = function(x) NULL)); print(tryCatch({DateVarList[[paste0('DateVar', run)]]}, error = function(x) NULL)); print(tryCatch({GroupVars}, error = function(x) NULL))
          }

          # Subset Rows and Columns
          if(PlotType == 'line' && length(XVar) != 0) {
            data1 <- RemixAutoML::PreparePlotData(
              SubsetOnly = FALSE, #if(PlotType %in% c('BoxPlot','ViolinPlot','CorrMatrix','Scatter','Copula','Histogram','Train_ParDepPlots','Test_ParDepPlots','Train_ParDepBoxPlots','Test_ParDepBoxPlots','Test__EvaluationPlot','Train_EvaluationPlot','Test_EvaluationBoxPlot','Train_EvaluationBoxPlot')) TRUE else FALSE,
              data = data1, Aggregate = 'mean', TargetVariable = YVar, DateVariable = XVar,
              GroupVariables = GroupVars,
              G1Levels = Levels1, G2Levels = Levels2, G3Levels = Levels3)
            CodeCollection[[run]][[length(CodeCollection[[run]])+1L]] <- paste0("data1 <- RemixAutoML::PreparePlotData(SubsetOnly = ", FALSE,", data=data1, Aggregate='mean', TargetVariable=", RemixAutoML:::CEP(YVar),", DateVariable=", RemixAutoML:::CEP(XVar), ", GroupVariables=", RemixAutoML:::CEP(GroupVars),", G1Levels=", RemixAutoML:::CEP(Levels1),", G2Levels=", RemixAutoML:::CEP(Levels2),", G3Levels=", RemixAutoML:::CEP(Levels3),")")
          } else {
            keep <- unique(c(YVar, XVar, GroupVars, SizeVars, FacetVar1, FacetVar2, CorVariables, PDP_Variable, ScoreVar))
            data1 <- data1[, .SD, .SDcols = c(Keep)]
            CodeCollection[[run]][[length(CodeCollection[[run]])+1L]] <- paste0('data1 <- data1[, .SD, .SDcols = c(',Keep,')]')
          }

          # Debugging
          if(Debug) print('PreparePlotData done')
        }

        # ----

        # ----

        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
        # Rebuild Logic                        ----
        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

        if(Debug) print('Here 27')

        # Rebuild Model Eval Plots; # Rebuild needs ScoreVar to not be null, # Rebuild if PDP Var in names(dt)
        # Rebuild if Percentile_Buckets changed from default, # Rebuild if Subsetting is desired
        # x4:Rebuild if PDP_Variable not in names of PlotList
        if(Debug) {
          print(data1)
          print(GroupVars)
        }

        # Logic Checks
        x1 <- length(ScoreVar) != 0; if(Debug) print(x1)
        if(Debug) print('Here 27a')
        x2 <- PDP_Variable %in% names(data1); if(Debug) print(PDP_Variable %in% names(data1))
        if(Debug) print('Here 27b')
        x3 <- Percentile_Buckets != 20; if(Debug) print(Percentile_Buckets != 20)
        if(Debug) print('Here 27c')
        x4 <- length(GroupVars) != 0 && (length(Levels1) != 0 || length(Levels2) != 0 || length(Levels3) != 0); if(Debug) print(length(GroupVars) != 0 && (length(Levels1) != 0 || length(Levels2) != 0 || length(Levels3) != 0))
        if(Debug) print('Here 27d')
        x5 <- any(c('Test_ParDepPlots','Train_ParDepPlots','Test_ParDepBoxPlots','Train_ParDepBoxPlots','Test_EvaluationPlot','Train_EvaluationPlot','Test_EvaluationBoxPlot','Train_EvaluationBoxPlot','Test_GainsPlot','Train_GainsPlot','Test_LiftPlot','Train_LiftPlot','Test_ScatterPlot','Train_ScatterPlot','Test_CopulaPlot','Train_CopulaPlot','Test_ResidualsHistogram','Train_ResidualsHistogram') %in% PlotType); if(Debug) print(any(c('Test_ParDepPlots','Train_ParDepPlots','Test_ParDepBoxPlots','Train_ParDepBoxPlots','Test_EvaluationPlot','Train_EvaluationPlot','Test_EvaluationBoxPlot','Train_EvaluationBoxPlot','Test_GainsPlot','Train_GainsPlot','Test_LiftPlot','Train_LiftPlot','Test_ScatterPlot','Train_ScatterPlot','Test_CopulaPlot','Train_CopulaPlot','Test_ResidualsHistogram','Train_ResidualsHistogram') %in% PlotType))
        if(Debug) print('Here 27e')
        Blocker <- !x1 || (!x2 && PlotType %in% c('Test_ParDepPlots','Train_ParDepPlots','Test_ParDepBoxPlots','Train_ParDepBoxPlots')); if(Debug) print(!x1 || (!x2 && PlotType %in% c('Test_ParDepPlots','Train_ParDepPlots','Test_ParDepBoxPlots','Train_ParDepBoxPlots')))
        if(Debug) print('Here 27f')
        if(x5 || x4 || (x3 && PlotType %in% c('Test_ParDepPlots','Train_ParDepPlots','Test_ParDepBoxPlots','Train_ParDepBoxPlots'))) {
          if(Debug) print('Here 27g')
          if(Blocker) Rebuild <- FALSE else Rebuild <- TRUE
        } else {
          if(Debug) print('Here 27h')
          Rebuild <- FALSE
        }
        if(Debug) print(Rebuild)

        # ----

        # ----

        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
        # Define Plots Variables               ----
        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

        if(Debug) print('Here 28')
        if(Debug) {print('Create Plot Object'); print(PlotType)}

        # Debugging
        if(Debug) {print(paste0('PlotType print: ', PlotType)); print(PlotType %chin% c('BoxPlot', 'ViolinPlot', 'Bar', 'Line', 'Scatter', 'Copula',  'CorrMatrix', 'Histogram'))}

        # XVar: If XVar is NULL go to next iteration in Plot Loop
        if(PlotType == 'Line') {
          if(Debug) print('Checking XVar for PlotType == :: Line ::')
          if(length(XVar) == 0) next
        }

        # Debugging
        if(Debug) print('Continuing ::')

        # ----

        # ----

        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
        # Create Plots                         ----
        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
        if(PlotType %chin% c('BoxPlot', 'ViolinPlot', 'Bar', 'Line', 'Scatter', 'Copula', 'Histogram', 'CorrMatrix')) {

          # AutoPlotter()
          if(Debug) print('Run AutoPlotter')
          PlotCollectionList[[paste0('p', run)]] <- RemixAutoML:::AutoPlotter(
            dt = data1,
            PlotType = PlotType,
            SampleSize = SampleSize,
            YVar = YVar,
            XVar = XVar,
            CorrVariables = CorVariables,
            CorrelationMethod = CorMethod,
            ColorVariables = GroupVars,
            SizeVar1 = SizeVars,
            FacetVar1 = FacetVar1,
            FacetVar2 = FacetVar2,
            YTicks = YTicks,
            XTicks = XTicks,
            OutlierSize = OutlierSize,
            OutlierColor = OutlierColor,
            FillColor = FillColor,
            GamFitScatter = GamFitScatter,
            TextSize = TextSize,
            TextColor = TextColor,
            AngleX = AngleX,
            AngleY = AngleY,
            ChartColor = ChartColor,
            BorderColor = BorderColor,
            GridColor = GridColor,
            BackGroundColor = BackGroundColor,
            LegendPosition = LegendPosition,
            LegendBorderSize = LegendBorderSize,
            Debug = Debug)

          # Debugging
          if(Debug) {
            print('Here 29')
            print('paste0(RemixAutoML:::AutoPlotter(dt = data1, PlotType = , ')
            print(RemixAutoML:::CEPP(PlotType))
            print(paste0('YVar = ', RemixAutoML:::CEP(YVar)))
            print(paste0('XVar = ', RemixAutoML:::CEP(XVar)))
            print(paste0('ColorVariables = ', RemixAutoML:::CEP(GroupVars)))
            print(paste0('SizeVar1 = ', RemixAutoML:::CEP(SizeVars)))
            print(paste0('FacetVar1 = ', RemixAutoML:::CEP(FacetVar1)))
            print(paste0('FacetVar2 = ', RemixAutoML:::CEP(FacetVar2)))
            print(paste0('YTicks = ', RemixAutoML:::CEP(YTicks)))
            print(paste0('XTicks = ', RemixAutoML:::CEP(XTicks)))
            print(paste0('OutlierSize = ', RemixAutoML:::CEP(OutlierSize)))
            print(paste0('OutlierColor = ', RemixAutoML:::CEP(OutlierColor)))
            print(paste0('FillColor = ', RemixAutoML:::CEP(FillColor)))
            print(paste0('GamFitScatter = ', RemixAutoML:::CEP(GamFitScatter)))
            print(paste0('TextSize = ', RemixAutoML:::CEP(TextSize)))
            print(paste0('TextColor = ', RemixAutoML:::CEP(TextColor)))
            print(paste0('AngleX = ', RemixAutoML:::CEP(AngleX)))
            print(paste0('AngleY = ', RemixAutoML:::CEP(AngleY)))
            print(paste0('ChartColor = ', RemixAutoML:::CEP(ChartColor)))
            print(paste0('BorderColor = ', RemixAutoML:::CEP(BorderColor)))
            print(paste0('GridColor = ', RemixAutoML:::CEP(GridColor)))
            print(paste0('BackGroundColor = ', RemixAutoML:::CEP(BackGroundColor)))
            print(paste0('LegendBorderSize = ', RemixAutoML:::CEP(as.numeric(LegendBorderSize))))
            print(paste0('LegendPosition = ', RemixAutoML:::CEP(LegendPosition)))
          }

          # Code Collection
          if(Debug) print('Debug Code Collection Start')
          CodeCollection[[run]][[length(CodeCollection[[run]])+1L]] <- paste0(
            "RemixAutoML:::AutoPlotter(dt = data1, PlotType = ", RemixAutoML:::CEP(PlotType),
            ", YVar=", RemixAutoML:::CEP(YVar),
            ", XVar=", RemixAutoML:::CEP(XVar),
            ", ColorVariables=", RemixAutoML:::CEP(GroupVars),
            ", SizeVar1=", RemixAutoML:::CEP(SizeVars),
            ", FacetVar1=", RemixAutoML:::CEP(FacetVar1),
            ", FacetVar2=", RemixAutoML:::CEP(FacetVar2),
            ", YTicks=", RemixAutoML:::CEP(YTicks),
            ", XTicks=", RemixAutoML:::CEP(XTicks),
            ", OutlierSize=", RemixAutoML:::CEP(OutlierSize),
            ", OutlierColor=", RemixAutoML:::CEP(OutlierColor),
            ", FillColor=", RemixAutoML:::CEP(FillColor),
            ", GamFitScatter=", RemixAutoML:::CEP(GamFitScatter),
            ", TextSize=", RemixAutoML:::CEP(TextSize),
            ", TextColor=", RemixAutoML:::CEP(TextColor),
            ", AngleX=", RemixAutoML:::CEP(AngleX),
            ", AngleY=", RemixAutoML:::CEP(AngleY),
            ", ChartColor=", RemixAutoML:::CEP(ChartColor),
            ", BorderColor=", RemixAutoML:::CEP(BorderColor),
            ", GridColor=", RemixAutoML:::CEP(GridColor),
            ", BackGroundColor=", RemixAutoML:::CEP(BackGroundColor), ")")
          CodeCollection <<- CodeCollection
          if(Debug) print(unlist(CodeCollection))

        } else {

          # Debugging
          if(Debug) {
            print(paste0('PDPVar = ', RemixAutoML:::CEP(PDP_Variable)))
            print(class(ModelOutputList))
            print(names(ModelOutputList$PlotList))
          }

          # Build Plot
          PlotCollectionList[[paste0('p', run)]] <- RemixAutoML:::AppModelInsights(
            ModelOutputList,
            dt = data1,
            PlotType = PlotType,
            TargetVar = YVar,
            PredictVar = ScoreVar,
            PDPVar = PDP_Variable,
            DateVar = NULL,
            FacetVar1 = NULL, # facetvar1,
            FacetVar2 = NULL, # facetvar2,
            GamFit = GamFitScatter,
            Buckets = Percentile_Buckets,
            ShapAgg = ShapAgg,
            Rebuild = Rebuild, Debug = Debug)

          # Debugging
          if(Debug) {
            print(ShapAgg)
            print('You are right here and now 1')
            print(class(PlotCollectionList[[paste0('p', 1)]]))
            print(PDP_Variable)
            print(names(ModelOutputList$PlotList$Test_ParDepPlots)[1L])
            print('AppModelInsights finished building. Code Collection next')
            print(length(YVar))
            print(RemixAutoML:::CEP(YVar))
            print(paste0("TargetVar=", if(length(YVar) != 0) RemixAutoML:::CEP(YVar) else 'NULL'))
            print(paste0("PredictVar=", if(length(ScoreVar) != 0) RemixAutoML:::CEP(ScoreVar) else 'NULL'))
            print(paste0("PDPVar=", PDP_Variable))
            print(paste0("GamFit=", RemixAutoML:::CEPP(GamFitScatter)))
            print(paste0("Buckets=", RemixAutoML:::CEP(Percentile_Buckets)))
            print(paste0("Rebuild=", Rebuild))
          }

          # Code Collection
          CodeCollection[[run]][[length(CodeCollection[[run]])+1L]] <- paste0(
            "RemixAutoML:::AppModelInsights(dt=data1, PlotType=", RemixAutoML:::CEP(PlotType),
            ", ModelOutputList=ModelOutputList",
            ", TargetVar=", RemixAutoML:::CEP(YVar),
            ", PredictVar=", RemixAutoML:::CEP(ScoreVar),
            ", PDPVar=", RemixAutoML:::CEP(PDP_Variable),
            ", FacetVar1 = ", if(PlotType %in% 'S__hapleyVarImp') RemixAutoML:::CEP(FacetVar1) else "NULL",
            ", FacetVar2 = ", if(PlotType %in% 'S__hapleyVarImp') RemixAutoML:::CEP(FacetVar2) else "NULL",
            ", GamFit=", RemixAutoML:::CEPP(GamFitScatter),
            ", Buckets=", RemixAutoML:::CEP(Percentile_Buckets),
            ", Rebuild=", Rebuild, ")")

          # Code Collection
          if(length(names(PlotCollectionList)) > 0L) {

            # Update ChartTheme
            PlotCollectionList[[paste0('p', run)]] <- PlotCollectionList[[paste0('p', run)]] + RemixAutoML::ChartTheme(
              Size = TextSize, AngleX = AngleX, AngleY = AngleY, ChartColor = ChartColor,
              BorderColor = BorderColor, TextColor = TextColor, GridColor = GridColor,
              BackGroundColor = BackGroundColor, SubTitleColor = SubTitleColor,
              LegendPosition = if(PlotType %in% c('ShapelyVarImp','Train_VariableImportance','Test_VariableImportance','Validation_VariableImportance','Test_GainsPlot','Train_GainsPlot')) 'none' else LegendPosition,
              LegendBorderSize = if(PlotType %in% c('ShapelyVarImp','Train_VariableImportance','Test_VariableImportance','Validation_VariableImportance','Test_GainsPlot','Train_GainsPlot')) NULL else as.numeric(LegendBorderSize),
              LegendLineType = if(PlotType %in% c('ShapelyVarImp','Train_VariableImportance','Test_VariableImportance','Validation_VariableImportance','Test_GainsPlot','Train_GainsPlot')) NULL else LegendLineType)

            # Code Collection
            CodeCollection[[run]][[length(CodeCollection[[run]])+1L]] <- paste0(
              "RemixAutoML::ChartTheme(Size=", RemixAutoML:::CEP(TextSize),
              ", AngleX=", RemixAutoML:::CEP(AngleX),
              ", AngleY=", RemixAutoML:::CEP(AngleY),
              ", ChartColor=", RemixAutoML:::CEP(ChartColor),
              ", BorderColor=", RemixAutoML:::CEP(BorderColor),
              ", TextColor=", RemixAutoML:::CEP(TextColor),
              ", GridColor=", RemixAutoML:::CEP(GridColor),
              ", BackGroundColor=", RemixAutoML:::CEP(BackGroundColor),
              ", SubTitleColor=", RemixAutoML:::CEP(SubTitleColor),
              ", LegendPosition=", RemixAutoML:::CEP(if(PlotType %in% c('ShapelyVarImp','Train_VariableImportance','Test_VariableImportance','Validation_VariableImportance','Test_GainsPlot','Train_GainsPlot')) 'none' else LegendPosition),
              ", LegendBorderSize=", RemixAutoML:::CEP(if(PlotType %in% c('ShapelyVarImp','Train_VariableImportance','Test_VariableImportance','Validation_VariableImportance','Test_GainsPlot','Train_GainsPlot')) NULL else as.numeric(LegendBorderSize)),
              ", LegendLineType=", RemixAutoML:::CEP(if(PlotType %in% c('ShapelyVarImp','Train_VariableImportance','Test_VariableImportance','Validation_VariableImportance','Test_GainsPlot','Train_GainsPlot')) NULL else LegendBorderSize), ")")
          }
          CodeCollection <<- CodeCollection
        }

        # Store globally
        data1 <<- data1
        PlotCollectionList <<- PlotCollectionList
        CodeCollection <<- CodeCollection

        # ----

        # ----

        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
        # Return Plot to UI                          ----
        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
        if(length(names(PlotCollectionList)) == length(PlotRefs)) {

          # Debugging
          if(Debug) {
            print('Return Plot to UI')
            print(exists("PlotCollectionList"))
            print(class(PlotCollectionList[[paste0('p', 1)]]))
          }

          # Print to UI
          if(exists("PlotCollectionList") && length(names(PlotCollectionList)) != 0) {
            if(Debug) print(RemixAutoML::ReturnParam(xx=tryCatch({input[['AutoGridHorizontal']]}, error=function(x) FALSE), VarName=NULL, Type='logical', Default = TRUE))
            AutoGridHorizontal <- RemixAutoML::ReturnParam(xx=tryCatch({input[['AutoGridHorizontal']]}, error=function(x) FALSE), VarName=NULL, Type='logical', Default = TRUE)
            if(is.null(AutoGridHorizontal)) AutoGridHorizontal <- TRUE
            if(Debug) {
              print('Convert p1 to global env')
              print(paste0('FINAL ERROR =' , AutoGridHorizontal))
              print(paste0('Length of PlotCollectionList = ', length(PlotCollectionList)))
              print(paste0('Names of PlotCollectionList = ', names(PlotCollectionList)))
              print(for(i in names(PlotCollectionList)) paste0('Names of PlotCollectionList = ', class(PlotCollectionList[[i]])))
              print('Create Plot output$Trend')
            }
            CodeCollection[[length(CodeCollection)+1L]] <- 'gridExtra::grid.arrange(gridExtra::arrangeGrob(grobs = PlotCollectionList, as.table = FALSE))'

            # Number of plots
            N <- length(PlotCollectionList)
            if(Debug) print(paste0('Length of N = ', N))

            # Build Plots
            if(N == 1L) {

              if(Debug) print('N == 1L case')
              if(Debug) print(tryCatch({input[['YLimMin1']]}, error = function(x) ""))
              p1 <- PlotCollectionList[['p1']]

              # Update axis limits
              if(Debug) {print(input$YLimMin1);print(input$YLimMax1);print(input$XLimMin1);print(input$XLimMax1)}
              #if(any(length(input[['YLimMin1']])))
              p1 <- RemixAutoML::PlotLimits(
                p1,
                YMin=tryCatch({input[['YLimMin1']]}, error = function(x) ""),
                YMax=tryCatch({input[['YLimMax1']]}, error = function(x) ""),
                XMin=tryCatch({input[['XLimMin1']]}, error = function(x) ""),
                XMax=tryCatch({input[['XLimMax1']]}, error = function(x) ""))

              if(Debug) print(paste0('Class of p1 is :: ', class(p1)))

              # Ouput Plot for 1 single plot request
              output$Trend <- shiny::renderPlot(width = PlotWidth, height = PlotHeight, {
                gridExtra::grid.arrange(p1, ncol=1)
              })

            } else if(N == 2L) {

              if(Debug) print('N == 2L case')
              p1 <- PlotCollectionList[['p1']]
              p2 <- PlotCollectionList[['p2']]
              if(Debug) print(paste0('Class of p1 is :: ', tryCatch({class(p1)}, error = function(x) NULL)))
              if(Debug) print(paste0('Class of p1 is :: ', tryCatch({class(p2)}, error = function(x) NULL)))

              # Update axis limits
              p1 <- RemixAutoML::PlotLimits(
                p1,
                YMin=tryCatch({input[['YLimMin1']]}, error = function(x) ""),
                YMax=tryCatch({input[['YLimMax1']]}, error = function(x) ""),
                XMin=tryCatch({input[['XLimMin1']]}, error = function(x) ""),
                XMax=tryCatch({input[['XLimMax1']]}, error = function(x) ""))
              p2 <- RemixAutoML::PlotLimits(
                p2,
                YMin=tryCatch({input[['YLimMin2']]}, error = function(x) ""),
                YMax=tryCatch({input[['YLimMax2']]}, error = function(x) ""),
                XMin=tryCatch({input[['XLimMin2']]}, error = function(x) ""),
                XMax=tryCatch({input[['XLimMax2']]}, error = function(x) ""))

              # Ouput Plot for 2 plot requests
              if(AutoGridHorizontal) {
                output$Trend <- shiny::renderPlot(width = PlotWidth, height = PlotHeight * 2, {
                  gridExtra::grid.arrange(p1,p2, ncol=1)
                })
              } else {
                output$Trend <- shiny::renderPlot(width = PlotWidth, height = PlotHeight, {
                  gridExtra::grid.arrange(p1,p2, ncol=1)
                })
              }

            } else if(N == 3L) {

              if(Debug) print('N == 3L case')
              p1 <- PlotCollectionList[['p1']]
              p2 <- PlotCollectionList[['p2']]
              p3 <- PlotCollectionList[['p3']]
              if(Debug) print(paste0('Class of p1 is :: ', tryCatch({class(p1)}, error = function(x) NULL)))
              if(Debug) print(paste0('Class of p2 is :: ', tryCatch({class(p2)}, error = function(x) NULL)))
              if(Debug) print(paste0('Class of p3 is :: ', tryCatch({class(p3)}, error = function(x) NULL)))

              # Update axis limits
              p1 <- RemixAutoML::PlotLimits(
                p1,
                YMin=tryCatch({input[['YLimMin1']]}, error = function(x) ""),
                YMax=tryCatch({input[['YLimMax1']]}, error = function(x) ""),
                XMin=tryCatch({input[['XLimMin1']]}, error = function(x) ""),
                XMax=tryCatch({input[['XLimMax1']]}, error = function(x) ""))
              p2 <- RemixAutoML::PlotLimits(
                p2,
                YMin=tryCatch({input[['YLimMin2']]}, error = function(x) ""),
                YMax=tryCatch({input[['YLimMax2']]}, error = function(x) ""),
                XMin=tryCatch({input[['XLimMin2']]}, error = function(x) ""),
                XMax=tryCatch({input[['XLimMax2']]}, error = function(x) ""))
              p3 <- RemixAutoML::PlotLimits(
                p3,
                YMin=tryCatch({input[['YLimMin3']]}, error = function(x) ""),
                YMax=tryCatch({input[['YLimMax3']]}, error = function(x) ""),
                XMin=tryCatch({input[['XLimMin3']]}, error = function(x) ""),
                XMax=tryCatch({input[['XLimMax3']]}, error = function(x) ""))

              # Ouput Plot for 3 plot requests
              if(AutoGridHorizontal) {
                output$Trend <- shiny::renderPlot(width = PlotWidth, height = PlotHeight * 2, {
                  gridExtra::grid.arrange(p1,p3,p2, layout_matrix = rbind(c(1, 2),  # 1 = upper left, 2 = upper right, 3 = bottom left and right
                                                                          c(3, 3)))
                })
              } else {
                output$Trend <- shiny::renderPlot(width = PlotWidth, height = PlotHeight, {
                  gridExtra::grid.arrange(p1,p3,p2, layout_matrix = rbind(c(1, 2),  # 1 = upper left, 2 = upper right, 3 = bottom left and right
                                                                          c(3, 3)))
                })
              }

            } else if(N == 4L) {

              if(Debug) print('N == 4L case')
              p1 <- PlotCollectionList[['p1']]
              p2 <- PlotCollectionList[['p2']]
              p3 <- PlotCollectionList[['p3']]
              p4 <- PlotCollectionList[['p4']]
              if(Debug) print(paste0('Class of p1 is :: ', tryCatch({class(p1)}, error = function(x) NULL)))
              if(Debug) print(paste0('Class of p2 is :: ', tryCatch({class(p2)}, error = function(x) NULL)))
              if(Debug) print(paste0('Class of p3 is :: ', tryCatch({class(p3)}, error = function(x) NULL)))
              if(Debug) print(paste0('Class of p4 is :: ', tryCatch({class(p4)}, error = function(x) NULL)))

              # Update axis limits
              p1 <- RemixAutoML::PlotLimits(
                p1,
                YMin=tryCatch({input[['YLimMin1']]}, error = function(x) ""),
                YMax=tryCatch({input[['YLimMax1']]}, error = function(x) ""),
                XMin=tryCatch({input[['XLimMin1']]}, error = function(x) ""),
                XMax=tryCatch({input[['XLimMax1']]}, error = function(x) ""))
              p2 <- RemixAutoML::PlotLimits(
                p2,
                YMin=tryCatch({input[['YLimMin2']]}, error = function(x) ""),
                YMax=tryCatch({input[['YLimMax2']]}, error = function(x) ""),
                XMin=tryCatch({input[['XLimMin2']]}, error = function(x) ""),
                XMax=tryCatch({input[['XLimMax2']]}, error = function(x) ""))
              p3 <- RemixAutoML::PlotLimits(
                p3,
                YMin=tryCatch({input[['YLimMin3']]}, error = function(x) ""),
                YMax=tryCatch({input[['YLimMax3']]}, error = function(x) ""),
                XMin=tryCatch({input[['XLimMin3']]}, error = function(x) ""),
                XMax=tryCatch({input[['XLimMax3']]}, error = function(x) ""))
              p4 <- RemixAutoML::PlotLimits(
                p4,
                YMin=tryCatch({input[['YLimMin4']]}, error = function(x) ""),
                YMax=tryCatch({input[['YLimMax4']]}, error = function(x) ""),
                XMin=tryCatch({input[['XLimMin4']]}, error = function(x) ""),
                XMax=tryCatch({input[['XLimMax4']]}, error = function(x) ""))

              # Ouput Plot for 4 plot requests
              if(AutoGridHorizontal) {
                output$Trend <- shiny::renderPlot(width = PlotWidth, height = PlotHeight * 2, {
                  gridExtra::grid.arrange(p1,p3,p2,p4, ncol=2)
                })
              } else {
                output$Trend <- shiny::renderPlot(width = PlotWidth, height = PlotHeight, {
                  gridExtra::grid.arrange(p1,p3,p2,p4, ncol=2)
                })
              }

            }

          } else {

            # Send Error Message
            shinyWidgets::sendSweetAlert(session, title = NULL, text = 'Plot could not build. Check for missing variables, such as Date Variables.', type = NULL, btn_labels = "error", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")

          }

        } else {
          # Send Error Message
          if(Debug) print('length(names(PlotCollectionList)) != length(PlotRefs)')
          shinyWidgets::sendSweetAlert(session, title = NULL, text = 'Plot could not build. Check for missing variables, such as Date Variables.', type = NULL, btn_labels = "error", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")

        } # end Plot Build

      }
    }
  })

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Close app after closing browser      ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # shiny::onStop(function() {
  #   cat("Session stopped\n")
  #   EndEnv <- as.list(environment())
  #   ggg <- setdiff(names(EndEnv), names(StartEnv))
  #   for(i in seq_along(ggg)) try(rm(list = ggg[i]))
  # })
  session$onSessionEnded(function() {
    shiny::stopApp()
  })
}

# onStart = function() {
#   cat("Doing application setup\n")
#
#   onStop(function() {
#     cat("Doing application cleanup\n")
#     EndEnv <- as.list(environment())
#     ggg <- setdiff(names(EndEnv), names(StartEnv))
#     for(i in seq_along(ggg)) try(rm(list = ggg[i]))
#   })
# }


# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# Run App                              ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
shiny::shinyApp(ui = ui, server = server)

# ----

# ----
