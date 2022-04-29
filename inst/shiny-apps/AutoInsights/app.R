options(shiny.maxRequestSize = 250000*1024^2)

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# Environment Setup                    ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
StartEnv <- as.list(environment())
library(data.table)
library(shiny)
#library(promises)
#library(future)
#plan(multisession)
data.table::setDTthreads(threads = max(1L, parallel::detectCores()-1L))
options(scipen = 999)

# Input Initialization at specific page clicks for the first time
LoadDataPage_Validate <- TRUE
FE_Validate <- TRUE
FE_Validate_Counter <- 0L
Plotter_PlotDropDown1_Validate <- TRUE

# Display NNN number of records from any given table displayed via DataTable()
NNN <- 1000L

# Temp data
DataList <- list()
DataList[['temp']] <- data.table::data.table(Random1=runif(5L), Random2=runif(5L), Random3=runif(5L), Random4=runif(5L), Random5=runif(5L), Random6=runif(5L))

# Initialize Data
data <- NULL
DataList <- list()

# First pass
InitalizeInputs <- TRUE

# List of Plot Types to choose
AvailablePlots <- c(
  'Histogram','ViolinPlot','BoxPlot','BarPlot','LinePlot','ScatterPlot','CopulaPlot','CorrelationMatrix','HeatMapPlot',
  'ShapelyImportance',
  'PartialDependenceLine', 'PartialDependenceBox',
  'CalibrationPlot', 'CalibrationBoxPlot',
  'ResidualsHistogram', 'ResidualsScatterPlot', 'ResidualsCopulaPlot',
  'VariableImportance',
  'GainsPlot', 'LiftPlot', 'ROCPlot')

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# Passthrough Args                     ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

# Meta Data related
BlobStorageURL <- shiny::getShinyOption('BlobStorageURL', default = NULL)
DockerPathToData <- shiny::getShinyOption('DockerPathToData', default = NULL)
PlotObjectHome <- shiny::getShinyOption('PlotObjectHome', default = NULL)

# App and Plot related
AzureCredsFile <- shiny::getShinyOption('AzureCredsFile')
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

# if(Debug) options(shiny.trace = TRUE)

# Load credentials
if(!is.null(AzureCredsFile)) {
  creds <- data.table::fread(file = file.path(AzureCredsFile, 'AutoPlotterCreds.csv'))
  StorageAccount <- creds[Account == 'StorageAccount', Values]
  Container <- creds[Account == 'Container', Values]
  Key <- creds[Account == 'Key', Values]
} else {
  StorageAccount <- NULL
  Container <- NULL
  Key <- NULL
}

# Local PostGRE Creds
LocalPostGRE_DBNames = c('RemixAutoML','ControlTower')
LocalPostGRE_Host <- 'localhost'
LocalPostGRE_Port <- 5432
LocalPostGRE_User <- 'postgres'
LocalPostGRE_Password <- 'Aa1028#@'


# Initialize a few variables
PlotWidth <- 1500
PlotHeight <- 550
ModelData <- NULL
ModelOutputList <- NULL
data <- NULL

# Usernames and Passwords
UserName_Password_DT <- shiny::getShinyOption(name = 'UserName_Password_DT', default = NULL)
if(!is.null(UserName_Password_DT) && 'UserName' %in% names(UserName_Password_DT) && 'Password' %in% names(UserName_Password_DT)) {
  Credentials <- UserName_Password_DT
} else {
  Credentials <- data.table::data.table(UserName = c('Guest'), Password = c('Password'))
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
  title = 'RemixAutoML',

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
      id = 'sidebar',

      # Login Page
      RemixAutoML:::BlankRow(AppWidth),
      shinydashboard::menuItem(text = 'Login', tabName = 'Login', icon = shiny::icon('sign-in-alt'), selected = TRUE),

      # Load Data Page
      RemixAutoML:::BlankRow(AppWidth),
      shinydashboard::menuItem(text = 'Import Data', tabName = 'LoadDataPage', icon=shiny::icon('database')),

      # Feature Engineering Page
      RemixAutoML:::BlankRow(AppWidth),
      shinydashboard::menuItem(text = 'Feature Engineering', tabName = 'FeatureEngineering', icon = shiny::icon('blender')),

      # Machine Learning Page
      RemixAutoML:::BlankRow(AppWidth),
      shinydashboard::menuItem(text = 'Machine Learning', tabName = 'MachineLearning', icon = shiny::icon('asterisk')),

      # Plotting Page
      RemixAutoML:::BlankRow(AppWidth),
      shinydashboard::menuItem(text = 'Create Plots', tabName = 'Plotter', icon = shiny::icon("chart-line")),

      # Code Print Page
      RemixAutoML:::BlankRow(AppWidth),
      shinydashboard::menuItem(text = 'Print Code', tabName = 'CodePrint', icon = shiny::icon('code')))),

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # DashboardBody                        ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  shinydashboard::dashboardBody(

    # Style Sheet Reference
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),

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

        # Tab MetaData
        selected = TRUE, tabName = 'Login',

        # Box with Username and Password inputs
        RemixAutoML:::LoginInputs(id='LoginBox', BoxStatus='danger', UserNameLabel="Select from Names", PassWordLabel='Input Password', UserNameChoices=Credentials[['UserName']], BoxTitle=NULL, SolidHeader=TRUE, Collapsible=FALSE, AppWidth=AppWidth),

        # Button to login and go to Load Data
        RemixAutoML:::LoginButton(id='LoginButton', Label='Check Credentials', AppWidth=AppWidth, Width=3L, Icon=shiny::icon('chevron-right', lib = 'font-awesome'), Style='gradient', Color='royal')),

      # ----

      # ----

      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
      # Load Data Page                       ----
      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
      shinydashboard::tabItem(

        # Tab MetaData
        tabName = 'LoadDataPage',

        # Load Inputs Box
        RemixAutoML:::LoadDataInputs(id = 'ExternalData', AppWidth=AppWidth, LogoWidth=LogoWidth, SolidHeader=TRUE, BoxTitle=NULL, BoxStatus='danger', DropdownRight=FALSE, DropDownAnimate=TRUE, DropDownStatus='custom'),

        # Button to Load Data
        RemixAutoML:::LoadDataButton(id = 'DataButton', AppWidth = AppWidth)),

      # ----

      # ----

      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
      # Feature Engineering Page             ----
      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
      shinydashboard::tabItem(

        # -- TAB REFERENCE VALUE
        tabName = "FeatureEngineering",

        # Add row
        RemixAutoML:::BlankRow(AppWidth),

        # Box
        shinydashboard::box(
          title = NULL, solidHeader = TRUE, collapsible = FALSE, status = 'danger', width = AppWidth,

          # Add Space
          RemixAutoML:::BlankRow(AppWidth),

          # Feature Engineering DropDowns by Feature Engineering Type
          shiny::fluidRow(
            width=AppWidth,
            RemixAutoML:::FE_WindowingVariables(id   = 'WindowingVariables',   AppWidth=AppWidth, LogoWidth=LogoWidth, ButtonWidth=3L, Align='center', DropDownRight=FALSE, Animate=TRUE, Status='custom', H3Color = 'blue'),
            RemixAutoML:::FE_CategoricalVariables(id = 'CategoricalVariables', AppWidth=AppWidth, LogoWidth=LogoWidth, ButtonWidth=3L, Align='center', DropDownRight=FALSE, Animate=TRUE, Status='custom', H3Color = 'blue'),
            RemixAutoML:::FE_DateVariables(id        = 'CalendarVariables',    AppWidth=AppWidth, LogoWidth=LogoWidth, ButtonWidth=3L, Align='center', DropDownRight=TRUE, Animate=TRUE, Status='custom', H3Color = 'blue'),
            RemixAutoML:::FE_NumericVariables(id     = 'NumericVariables',     AppWidth=AppWidth, LogoWidth=LogoWidth, ButtonWidth=3L, Align='center', DropDownRight=TRUE, Animate=TRUE, Status='custom', H3Color = 'blue')),

          # Add Space
          RemixAutoML:::BlankRow(AppWidth),

          # Other
          shiny::fluidRow(
            width=AppWidth,
            RemixAutoML:::ShinySaveData(id = 'SaveData_CSV_UI',           AppWidth=AppWidth, LogoWidth=LogoWidth, ButtonWidth=3L, Align='center', DropDownRight=FALSE, Animate=TRUE, Status='custom', H3Color = 'blue'),
            RemixAutoML:::FE_DataSets(  id = 'DataSets',                  AppWidth=AppWidth, LogoWidth=LogoWidth, ButtonWidth=3L, Align='center', DropDownRight=FALSE, Animate=TRUE, Status='custom', H3Color = 'blue'),
            RemixAutoML:::FE_DataWrangling(   id = 'GeneralFeatureEngineering', AppWidth=AppWidth, LogoWidth=LogoWidth, ButtonWidth=3L, Align='center', DropDownRight=TRUE,  Animate=TRUE, Status='custom', H3Color = 'blue')),

          # Add Space to act as a bigger boarder for box
          RemixAutoML:::BlankRow(AppWidth),
          RemixAutoML:::BlankRow(AppWidth),

        ), # End of box

        # Box
        shinydashboard::box(
          title = NULL, solidHeader = TRUE, collapsible = FALSE, status = 'danger', width = AppWidth,

          # Tabular Output
          RemixAutoML:::BlankRow(AppWidth),
          shiny::fluidRow(
            width = AppWidth,
            DT::dataTableOutput('FE_DisplayData'))),

          # Add extra row for padding
          RemixAutoML:::BlankRow(AppWidth)

      ), # End of tabItem

      # ----

      # ----

      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
      # Machine Learning Page                ----
      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
      shinydashboard::tabItem(

        # -- TAB REFERENCE VALUE
        tabName = 'MachineLearning',

        # Add row
        RemixAutoML:::BlankRow(AppWidth),

        # Box
        shinydashboard::box(
          title = NULL, solidHeader = TRUE, collapsible = FALSE, status = 'danger', width = AppWidth,

          # CatBoost ML
          RemixAutoML:::BlankRow(AppWidth),
          shiny::fluidRow(
            RemixAutoML:::ML_CatBoost(id = 'CatBoostML', Align = 'center', ButtonWidth = 2L),
            RemixAutoML:::ML_XGBoost( id = 'XGBoostML' , Align = 'center', ButtonWidth = 2L),
            RemixAutoML:::ML_LightGBM( id = 'LightGBMML', Align = 'center', ButtonWidth = 2L)),
          shiny::fluidRow(
            RemixAutoML:::BuildModelsButton(id = 'MLBuildButton')),

          # Add row so box has larger border
          RemixAutoML:::BlankRow(AppWidth),

        ), # End of box

        # # Box
        # shinydashboard::box(
        #   title = NULL, solidHeader = TRUE, collapsible = FALSE, status = 'danger', width = AppWidth,
        #
        #   # Tabular Output
        #   RemixAutoML:::BlankRow(AppWidth),
        #   shiny::fluidRow(
        #     width = AppWidth,
        #     DT::dataTableOutput('PLOT DEFAULTS VS ARGUMENTS'))),

        # Add extra row for padding
        RemixAutoML:::BlankRow(AppWidth)

      ), # End of tabItem

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

        # Box
        shinydashboard::box(
          title = NULL,
          solidHeader = TRUE, collapsible = FALSE, status = 'danger', width = AppWidth,

          # Add Space
          RemixAutoML:::BlankRow(AppWidth),

          # Plot DropDown Buttons and Contents
          shiny::fluidRow(
            width=AppWidth,
            RemixAutoML:::PlotDropDownContents(id='PlotDropDown', PlotNumber=1, AppWidth=AppWidth, LogoWidth=LogoWidth, ButtonWidth=3L, Align='center', DropDownRight=FALSE, Animate=TRUE, Status='custom', H3Color=H3Color),
            RemixAutoML:::PlotDropDownContents(id='PlotDropDown', PlotNumber=2, AppWidth=AppWidth, LogoWidth=LogoWidth, ButtonWidth=3L, Align='center', DropDownRight=FALSE, Animate=TRUE, Status='custom', H3Color=H3Color),
            RemixAutoML:::PlotDropDownContents(id='PlotDropDown', PlotNumber=3, AppWidth=AppWidth, LogoWidth=LogoWidth, ButtonWidth=3L, Align='center', DropDownRight=FALSE, Animate=TRUE, Status='custom', H3Color=H3Color),
            RemixAutoML:::PlotDropDownContents(id='PlotDropDown', PlotNumber=4, AppWidth=AppWidth, LogoWidth=LogoWidth, ButtonWidth=3L, Align='center', DropDownRight=FALSE, Animate=TRUE, Status='custom', H3Color=H3Color)), # end of fluidrow

          # Add Space to act as a bigger boarder for box
          RemixAutoML:::BlankRow(AppWidth)

        ), # End of box

        # Add Space to act as a border, just like the dragula box
        RemixAutoML:::BlankRow(AppWidth),

        # ----

        # ----

        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
        # Box with Dragula and Extra Buttons   ----
        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

        # Box
        shinydashboard::box(
          title = NULL, solidHeader = TRUE, collapsible = FALSE, status = 'danger', width = AppWidth,

          # Plotting Variables
          shiny::fluidRow(
            width=AppWidth,

            # Dragula Boxes
            shiny::column(width = 12L, align = 'center', shiny::uiOutput('PlotTypeDragula')),

            # Create Plot Button
            shiny::column(
              width = 3L, shinyjs::useShinyjs(), align='center', tags$h4(tags$b('~ Build Plot')),
              shinyWidgets::actionBttn(inputId='TrendPlotExecute', label='Build Plot', style='gradient', color='royal')),

            # Global Settings
            # (issue with getting blank space removed) RemixAutoML:::GlobalSettingsContents(id='GlobalSettings', PlotHeight=PlotHeight, PlotWidth=PlotWidth, AppWidth=AppWidth, LogoWidth=LogoWidth, H3Color=H3Color, Right=FALSE, Animate=TRUE, Status='custom'),

            # Formatting DropDown
            shiny::column(
              width = 3L,
              align = 'center',
              tags$h4(tags$b('Formatting')),
              shinyWidgets::dropdown(
                right = FALSE, animate = TRUE, circle = FALSE, tooltip = FALSE, status = "custom", inputId = "Plot-Formatting-Parent", width = LogoWidth,
                tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Plot Enhancements')),
                tags$h4(tags$span(style=paste0('color: ', H4Color, ';'),'Additional MetaData Selection for Plot Enhancements')),
                RemixAutoML:::BlankRow(AppWidth),

                # Plot Formatting
                shiny::fluidRow(
                  RemixAutoML:::AxisLimits(id='AxisLimitsContents', AppWidth=AppWidth, LogoWidth=LogoWidth, H3Color=H3Color, H4Color=H4Color, Right=FALSE, Animate=TRUE, Status='custom'),
                  RemixAutoML:::Formatting(id='FormattingContents', AppWidth=AppWidth, LogoWidth=LogoWidth, H3Color=H3Color, H4Color=H4Color, Right=TRUE, Animate=TRUE, Status='custom'),
                  RemixAutoML:::Coloring(id='ColoringContents', AppWidth=AppWidth, LogoWidth=LogoWidth, H3Color=H3Color, H4Color=H4Color, Right=TRUE, Animate=TRUE, Status='custom')), # fluidrow end

                # Add Space
                RemixAutoML:::BlankRow(AppWidth),

                # Plot Enhancements
                shiny::fluidRow(
                  RemixAutoML:::GamFitting(id='GamFittingContents', AppWidth=AppWidth, LogoWidth=LogoWidth, H3Color=H3Color, H4Color=H4Color, Right=FALSE, Animate=TRUE, Status='custom'),
                  RemixAutoML:::HistBins(id='HistBinsContents', AppWidth=AppWidth, LogoWidth=LogoWidth, H3Color=H3Color, H4Color=H4Color, Right=FALSE, Animate=TRUE, Status='custom'),
                  RemixAutoML:::PercentileBuckets(id='PercentileBucketsContents', AppWidth=AppWidth, LogoWidth=LogoWidth, H3Color=H3Color, H4Color=H4Color, Right=TRUE, Animate=TRUE, Status='custom'),
                  RemixAutoML:::ShapAgg(id='ShapAggContents', AppWidth=AppWidth, LogoWidth=LogoWidth, H3Color=H3Color, H4Color=H4Color, Right=TRUE, Animate=TRUE, Status='custom')),

                # Extra space at bottom of dropdown after filter buttoms
                RemixAutoML:::BlankRow(AppWidth),

              ) # end of dropdown
            ), # end of column plot format dropdown inputs ::::::::

            # Filtering Dropdown
            shiny::column(
              width = 3L,
              align = 'center',
              tags$h4(tags$b('Filters')),
              shinyWidgets::dropdown(
                right = TRUE, animate = TRUE, circle = FALSE, tooltip = FALSE, status = "custom", width = LogoWidth,
                tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Filter Variables, Logic, and Values')),
                tags$h4(tags$span(style=paste0('color: ', H4Color, ';'),'Filters for subsetting data')),
                RemixAutoML:::BlankRow(12L),

                # Filter Variables
                shiny::fluidRow(
                  RemixAutoML:::DataFilters(id='FiltersDropDownContents', PlotNumber=1, Status='custom', AppWidth=AppWidth, LogoWidth=LogoWidth, H3Color=H3Color, H4Color = H4Color, Right=FALSE, Animate=TRUE),
                  RemixAutoML:::DataFilters(id='FiltersDropDownContents', PlotNumber=2, Status='custom', AppWidth=AppWidth, LogoWidth=LogoWidth, H3Color=H3Color, H4Color = H4Color, Right=FALSE, Animate=TRUE),
                  RemixAutoML:::DataFilters(id='FiltersDropDownContents', PlotNumber=3, Status='custom', AppWidth=AppWidth, LogoWidth=LogoWidth, H3Color=H3Color, H4Color = H4Color, Right=FALSE, Animate=TRUE),
                  RemixAutoML:::DataFilters(id='FiltersDropDownContents', PlotNumber=4, Status='custom', AppWidth=AppWidth, LogoWidth=LogoWidth, H3Color=H3Color, H4Color = H4Color, Right=FALSE, Animate=TRUE)), # fluidrow end

                # Extra space at bottom of dropdown after filter buttoms
                RemixAutoML:::BlankRow(AppWidth),

              ) # dropdown end
            ) # column end for filtering inputs ::::::::
          ) # fluidrow end or row ::::
        ), # End of box ::

        # ----

        # ----

        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
        # Show Plot                            ----
        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

        # Add Space
        RemixAutoML:::Plotter(id = 'PlotOutput')),

      # ----

      # ----

      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
      # Code Print                           ----
      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
      shinydashboard::tabItem(

        # -- TAB REFERENCE VALUE
        tabName = "CodePrint",

        # Print Code!
        shiny::fluidRow(shiny::column(width = 3L, shinyjs::useShinyjs(), shinyWidgets::actionBttn(inputId='PrintCodeButton', label='Print Code', icon=shiny::icon('chevron-right', lib = 'font-awesome'), style='gradient', color=eval(CreatePlotButtonColor)))),

        # Print Code
        RemixAutoML:::BlankRow(AppWidth),
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

  print('Server Side Begins')

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  #    Setup Elements                    ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  # Initialize data reactive. Gets overwritten after first data load or when user updates data selected
  Data <<- reactive({DataList[['temp']]})
  CurrentData <- 'temp'

  # Button to disable / enable Data Load Page
  shinyjs::addCssClass(selector = "a[data-value='LoadDataPage']", class = "inactiveLink")
  shinyjs::addCssClass(selector = "a[data-value='Plotter']", class = "inactiveLink")
  shinyjs::addCssClass(selector = "a[data-value='FeatureEngineering']", class = "inactiveLink")
  shinyjs::addCssClass(selector = "a[data-value='MachineLearning']", class = "inactiveLink")
  shinyjs::addCssClass(selector = "a[data-value='CodePrint']", class = "inactiveLink")

  # Inputs
  UserName <- shiny::reactive({input$UserName})
  Password <- shiny::reactive({input$Password})

  # Login
  shiny::observeEvent(input$Check_Credentials, {
    if(UserName() %in% Credentials$UserName && Password() %in% Credentials[UserName == eval(UserName())]$Password) {
      shinyjs::removeCssClass(selector = "a[data-value='LoadDataPage']", class = "inactiveLink")
      shinyjs::removeCssClass(selector = "a[data-value='Plotter']", class = "inactiveLink")
      shinyjs::removeCssClass(selector = "a[data-value='FeatureEngineering']", class = "inactiveLink")
      shinyjs::removeCssClass(selector = "a[data-value='MachineLearning']", class = "inactiveLink")
      shinyjs::removeCssClass(selector = "a[data-value='CodePrint']", class = "inactiveLink")
      shinyWidgets::sendSweetAlert(session, title = NULL, text = 'Success', type = NULL, btn_labels = "success", btn_colors = "green", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {
      shinyjs::addCssClass(selector = "a[data-value='LoadDataPage']", class = "inactiveLink")
      shinyjs::addCssClass(selector = "a[data-value='Plotter']", class = "inactiveLink")
      shinyjs::addCssClass(selector = "a[data-value='FeatureEngineering']", class = "inactiveLink")
      shinyjs::addCssClass(selector = "a[data-value='MachineLearning']", class = "inactiveLink")
      shinyjs::addCssClass(selector = "a[data-value='CodePrint']", class = "inactiveLink")
      shinyWidgets::sendSweetAlert(session, title = NULL, text = 'Username and / or password is incorrect', type = NULL, btn_labels = "error", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  })

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  #    Inputs Load Data                  ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  print('LoadDataPage Initialization')

  # Prepare Data Loaders
  shiny::observeEvent(input$sidebar, {
    if(input$sidebar == 'LoadDataPage') {

      # Local .csv or .txt
      output$TabularData <- shiny::renderUI({
        shiny::fileInput(
          inputId = 'TabularData',
          label = NULL,
          accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
      })

      # Local .Rdata or .rds
      output$ModelObjectLoad <- shiny::renderUI({
        shiny::fileInput(inputId = "ModelObjectLoad", label = NULL)
      })

      # Azure Blob .csv or .txt
      output$AzureBlobStorageTabular <- shiny::renderUI({
        if(Debug) paste0('https://', StorageAccount, '.blob.core.windows.net/', Container)
        BlobStorageURL <- paste0('https://', StorageAccount, '.blob.core.windows.net/', Container)
        assign(x = 'BlobStorageURL', value = BlobStorageURL, envir = .GlobalEnv)
        cont <<- tryCatch({AzureStor::blob_container(BlobStorageURL, key = Key)}, error = function(x) NULL)
        rawfiles <<- tryCatch({AzureStor::list_storage_files(cont, info = 'name')}, error = function(x) NULL)
        if(length(rawfiles) != 0) {
          rawfiles <<- rawfiles[c(which(grepl(pattern = '.csv', x = rawfiles)), which(grepl(pattern = '.Rdata', x = rawfiles)))]
          rawfiles_csv <- rawfiles[which(grepl(pattern = '.csv', x = rawfiles))]
        } else {
          rawfiles_csv <<- NULL
        }
        RemixAutoML:::SelectizeInput(InputID='AzureBlobStorageTabular', Label=tags$span(style=paste0('color: blue;'),'Azure Blob .csv Files'), Choices=rawfiles_csv, SelectedDefault=NULL, Multiple=TRUE, MaxVars=1, CloseAfterSelect=TRUE, Debug=Debug)
      })

      # .Rdata or .rds
      output$AzureBlobStorageRdata <- shiny::renderUI({
        if(length(rawfiles) != 0) {
          rawfiles_rdata <- rawfiles[which(grepl(pattern = '.Rdata', x = rawfiles))]
        } else {
          rawfiles_rdata <- NULL
        }
        RemixAutoML:::SelectizeInput(InputID='AzureBlobStorageRdata', Label=tags$span(style=paste0('color: blue;'),'Azure Blob .Rdata Files'), Choices=rawfiles_rdata, SelectedDefault=NULL, Multiple=TRUE, MaxVars=1, CloseAfterSelect=TRUE, Debug=Debug)
      })

      # Local PostGRE DBNames Available
      output$LocalPostGRE_Database <- shiny::renderUI({
        RemixAutoML:::SelectizeInput(InputID = 'LocalPostGRE_Database', Label = NULL, Choices = LocalPostGRE_DBNames, SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1L, CloseAfterSelect = FALSE, Debug = Debug)
      })
      DBName <- shiny::reactive({tryCatch({input[['LocalPostGRE_Database']]}, error = function(x) NULL)})

      # Local POSTGRE DB
      output$LocalPostGRE <- shiny::renderUI({
        DBNameSelected <- tryCatch({shiny::req(DBName())}, error = function(x) 'RemixAutoML')
        if(DBNameSelected == 'RemixAutoML') {
          x <- RemixAutoML::PostGRE_ListTables(DBName = 'RemixAutoML', Connection = NULL, CloseConnection = TRUE, Host = LocalPostGRE_Host, Port = LocalPostGRE_Port, User = LocalPostGRE_User, Password = LocalPostGRE_Password)$data
        } else if(DBNameSelected == 'ControlTower') {
          x <- RemixAutoML::PostGRE_ListTables(DBName = 'ControlTower', Connection = NULL, CloseConnection = TRUE, Host = LocalPostGRE_Host, Port = LocalPostGRE_Port, User = LocalPostGRE_User, Password = LocalPostGRE_Password)$data
        }
        if(length(x) == 0L) x <- NULL
        RemixAutoML:::SelectizeInput(InputID = 'LocalPostGRE', Label = NULL, Choices = sort(x), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1L, CloseAfterSelect = FALSE, Debug = Debug)
      })
    }
  })

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  #    Initialize Shiny Inputs on Start  ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  shiny::observeEvent(InitalizeInputs, {
    print('App Initialization')
    RemixAutoML:::InitalizeInputs(InitalizeInputs)
    InitalizeInputs <<- FALSE
  })

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # :: Obs Event azure blob storage data ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  shiny::observeEvent(input$LoadAzure, {

    # File Type .csv
    FileName <<- tryCatch({input[['AzureBlobStorageTabular']]}, error = function(x) NULL)
    if(Debug) print(FileName)
    if(length(FileName) != 0 && FileName != "Load" && FileName != "") {
      data
      AzureStor::download_blob(container = cont, src = input[['AzureBlobStorageTabular']], dest = file.path('/inputdata', input[['AzureBlobStorageTabular']]), overwrite=TRUE)
    }

    # File Type .Rdata
    inFile2 <- tryCatch({input[['AzureBlobStorageRdata']]}, error = function(x) NULL)
    if(!is.null(inFile2)) print(inFile2)
    if(length(inFile2) != 0 && inFile2 != "") {
      if(Debug) {print('data check 3')}
      AzureStor::download_blob(container = cont, src = input[['AzureBlobStorageRdata']], dest = file.path('/inputdata', input[['AzureBlobStorageRdata']]), overwrite=TRUE)
    }
  })

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # :: Obs Event Load data               ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  shiny::observeEvent(input$LoadDataButton, {

    # Notify user that data is being loaded
    shiny::showNotification('Data loading has begun')

    # Local data loading
    CodeCollection <- list()
    x <- tryCatch({input[['TabularData']]}, error = function(x) NULL)
    print('TabularData')
    print(x)
    if(length(x) != 0) {
      filename <<- basename(input[['TabularData']][['name']])
      DataList[[filename]] <<- RemixAutoML:::ReactiveLoadCSV(Infile = input[['TabularData']], ProjectList = NULL, DateUpdateName = NULL, RemoveObjects = NULL, Debug = Debug)
      DataList <<- DataList
      CurrentData <<- filename
    }

    # Load ModelOutputList
    x <- tryCatch({input[['ModelObjectLoad']]}, error = function(x) NULL)
    print('ModelObjectLoad')
    print(x)
    if(length(x) != 0L) {
      filename <<- basename(x[['datapath']])
      ModelOutputList <<- readRDS(x[['datapath']])
      if(!is.null(ModelOutputList$TrainData) && !is.null(ModelOutputList$TestData)) {
        DataList[[filename]] <<- data.table::rbindlist(list(ModelOutputList$TrainData, ModelOutputList$TestData), use.names = TRUE, fill = TRUE)
        DataList <<- DataList
        CurrentData <<- filename
      } else if(is.null(ModelOutputList$TrainData) && !is.null(ModelOutputList$TestData)) {
        DataList[[filename]] <<- ModelOutputList$TestData
        DataList <<- DataList
        CurrentData <<- filename
      } else if(!is.null(ModelOutputList$TrainData) && is.null(ModelOutputList$TestData)) {
        DataList[[filename]] <<- ModelOutputList$TrainData
        DataList <<- DataList
        CurrentData <<- filename
      }
    }

    # Azure .csv
    x <- tryCatch({input[['AzureBlobStorageTabular']]}, error = function(x) NULL)
    print('AzureBlobStorageTabular')
    print(x)
    if(length(x) != 0L) {
      filename <<- basename(input$AzureBlobStorageTabular)
      DataList[[filename]] <<- RemixAutoML:::ReactiveLoadCSV(Infile = file.path('/inputdata', input[['AzureBlobStorageTabular']]), ProjectList = NULL, DateUpdateName = NULL, RemoveObjects = NULL, Debug = Debug)
      DataList <<- DataList
      CurrentData <<- filename
    }

    # Load ModelOutputList
    x <- tryCatch({input[['AzureBlobStorageRdata']]}, error = function(x) NULL)
    print('AzureBlobStorageRdata')
    print(x)
    if(length(x) != 0L) {
      filename <- basename(x)
      ModelOutputList <<- readRDS(file.path('/inputdata', x))
      if(!is.null(ModelOutputList$TrainData) && !is.null(ModelOutputList$TestData)) {
        DataList[[filename]] <<- data.table::rbindlist(list(ModelOutputList$TrainData, ModelOutputList$TestData), use.names = TRUE, fill = TRUE)
        DataList <<- DataList
        CurrentData <<- filename
      } else if(is.null(ModelOutputList$TrainData) && !is.null(ModelOutputList$TestData)) {
        DataList[[filename]] <<- ModelOutputList$TestData
        DataList <<- DataList
        CurrentData <<- filename
      } else if(!is.null(ModelOutputList$TrainData) && is.null(ModelOutputList$TestData)) {
        DataList[[filename]] <<- ModelOutputList$TrainData
        DataList <<- DataList
        CurrentData <<- filename
      }
    }

    # Local PostGRE Data
    print(tryCatch({input[['LocalPostGRE']]}, error = function(x) NULL))
    print(tryCatch({input[['LocalPostGRE_Database']]}, error = function(x) NULL))
    LocalPostGRE_TableName <- tryCatch({input[['LocalPostGRE']]}, error = function(x) NULL)
    LocalPostGRE_DBName <- tryCatch({input[['LocalPostGRE_Database']]}, error = function(x) NULL)
    print(LocalPostGRE_TableName)
    print(LocalPostGRE_DBName)
    if(length(LocalPostGRE_DBName) != 0L && length(LocalPostGRE_TableName) != 0L) {
      query <- paste0("SELECT * FROM ", shQuote(LocalPostGRE_TableName), " ;")
      print(query)
      DataList[[LocalPostGRE_TableName]] <- RemixAutoML::PostGRE_Query(
        Query = query,
        Host = LocalPostGRE_Host,
        CloseConnection = TRUE,
        DBName = LocalPostGRE_DBName,
        User = LocalPostGRE_User,
        Port = LocalPostGRE_Port,
        Password = LocalPostGRE_Password)$data
      print('here')
      DataList <<- DataList
      CurrentData <<- LocalPostGRE_TableName
      print(CurrentData)
      print(DataList[[CurrentData]])
    }

    # Initialize
    CodeCollection <<- CodeCollection

    # Sweet Alert
    if(Debug) print("Data was loaded")
    shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "success", btn_colors = "green", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
  })

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  #    Inputs Feature Engineering        ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  # input$DeleteVariablesParams is a shinyWidgets::dropdown inputId
  shiny::observeEvent(input$DeleteVariablesInputs, {
    print('Delete Variables Inputs Dropdown')
    output$DeleteVariables_SelectData <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='DeleteVariables_SelectData', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Choose data set'), Choices = names(DataList), Multiple = TRUE, MaxVars = 1, SelectedDefault = names(DataList)[1L])
    })
    dt <- shiny::reactive({shiny::req(tryCatch({DataList[[input$DeleteVariables_SelectData]]}, error = function(x) DataList[[1L]]))})
    output$DeleteVariables <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='DeleteVariables', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Delete Columns'), Choices = names(dt()), Multiple = TRUE, MaxVars = 1000)
    })
  })

  # input$ConcatColumnsParams is a shinyWidgets::dropdown inputId
  shiny::observeEvent(input$ConcatColumnsInputs, {
    print('Concat Columns Inputs Dropdown')
    output$ConcatColumns_SelectData <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='ConcatColumns_SelectData', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Choose data set'), Choices = names(DataList), Multiple = TRUE, MaxVars = 1, SelectedDefault = names(DataList)[1L])
    })
    dt <- shiny::reactive({shiny::req(tryCatch({DataList[[input$ConcatColumns_SelectData]]}, error = function(x) DataList[[1L]]))})
    output$ConcatColumns <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='ConcatColumns', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Concat Columns'), Choices = names(dt()), Multiple = TRUE, MaxVars = 1000)
    })
  })

  # input$CalendarVariablesInputs is a shinyWidgets::dropdown InputId
  shiny::observeEvent(input$CalendarVariablesInputs, {
    print('Calendar Variables Inputs Dropdown')
    output$CalendarVariables_SelectData <- shiny::renderUI({
      print(names(DataList))
      RemixAutoML:::SelectizeInput(InputID='CalendarVariables_SelectData', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Choose data set'), Choices = names(DataList), Multiple = TRUE, MaxVars = 1, SelectedDefault = names(DataList)[1L])
    })
    dt <- shiny::reactive({shiny::req(tryCatch({DataList[[input$CalendarVariables_SelectData]]}, error = function(x) DataList[[1L]]))})
    output$CalendarVariables_DateVariables <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='CalendarVariables_DateVariables', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Date Columns'), Choices = c(names(dt())), Multiple = TRUE, MaxVars = 1000)
    })
    output$CalendarVariables_TimeUnits <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='CalendarVariables_TimeUnits', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Calendar Variables'), Choices = c('second','minute','hour','wday','mday','yday','week','isoweek','wom','month','quarter','year'), Multiple = TRUE, MaxVars = 15)
    })
  })

  # input$HolidayVariablesInputs is a shinyWidgets::dropdown InputId
  shiny::observeEvent(input$HolidayVariablesInputs, {
    print('Holiday Variables Inputs Dropdown')
    output$HolidayVariables_SelectData <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='HolidayVariables_SelectData', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Choose data set'), Choices = names(DataList), Multiple = TRUE, MaxVars = 1, SelectedDefault = names(DataList)[1L])
    })
    dt <- shiny::reactive({shiny::req(tryCatch({DataList[[input$HolidayVariables_SelectData]]}, error = function(x) DataList[[1L]]))})
    output$HolidayVariables_DateVariables <- shiny::renderUI({
      if(length(dt()) != 0 && length(names(dt())) != 0) bla <- c(names(dt())) else bla <- NULL
      RemixAutoML:::SelectizeInput(InputID='HolidayVariables_DateVariables', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Date Variables'), Choices = c(bla), Multiple = TRUE, MaxVars = 100)
    })
    output$HolidayVariables_HolidayGroups <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='HolidayVariables_HolidayGroups', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Holiday Selection'), Choices = c('USPublicHolidays','EasterGroup','ChristmasGroup','OtherEcclesticalFeasts'), Multiple = TRUE, MaxVars = 10)
    })
    output$HolidayVariables_LookbackDays <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='HolidayVariables_LookbackDays', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Lookback Days'), Choices = c(1:100), Multiple = TRUE, MaxVars = 1, SelectedDefault = 1)
    })
  })

  # input$PercRankInputs is a shinyWidgets::dropdown InputId
  shiny::observeEvent(input$PercRankInputs, {
    print('PercRank Inputs Dropdown')
    output$PercentRank_SelectData <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='PercentRank_SelectData', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Choose data set'), Choices = names(DataList), Multiple = TRUE, MaxVars = 1, SelectedDefault = names(DataList)[1L])
    })
    dt <- shiny::reactive({shiny::req(tryCatch({DataList[[input$PercentRank_SelectData]]}, error = function(x) DataList[[1L]]))})
    output$PercentRank_ColNames <- shiny::renderUI({
      nam <- RemixAutoML:::CEPP(x = names(dt())[which(unlist(lapply(dt(), is.numeric)))], Default = NULL)
      RemixAutoML:::SelectizeInput(InputID='PercentRank_ColNames', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Variable Names'), Choices = nam, Multiple = TRUE, MaxVars = 1000)
    })
    output$PercentRank_GroupVars <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='PercentRank_GroupVars', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'By-Variables'), Choices = c(names(dt())), Multiple = TRUE, MaxVars = 100)
    })
    output$PercentRank_Granularity <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='PercentRank_Granularity', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Precision'), Choices = c(seq(0.001, 0.99, 0.001)), Multiple = TRUE, MaxVars = 1, SelectedDefault = 0.001)
    })
  })

  # input$AutoInteractionInputs is a shinyWidgets::dropdown InputId
  shiny::observeEvent(input$AutoInteractionInputs, {
    print('Auto Interaction Inputs Dropdown')
    output$AutoInteraction_SelectData <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='AutoInteraction_SelectData', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Choose data set'), Choices = names(DataList), Multiple = TRUE, MaxVars = 1, SelectedDefault = names(DataList)[1L])
    })
    dt <- shiny::reactive({shiny::req(tryCatch({DataList[[input$AutoInteraction_SelectData]]}, error = function(x) DataList[[1L]]))})
    output$AutoInteraction_NumericVars <- shiny::renderUI({
      nam <- RemixAutoML:::CEPP(x = names(dt())[which(unlist(lapply(dt(), is.numeric)))], Default = NULL)
      RemixAutoML:::SelectizeInput(InputID='AutoInteraction_NumericVars', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Variable Names'), Choices = nam, Multiple = TRUE, MaxVars = 1000)
    })
    output$AutoInteraction_InteractionDepth <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='AutoInteraction_InteractionDepth', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Interaction Depth'), Choices = c(1:10), Multiple = TRUE, MaxVars = 1, SelectedDefault = 2)
    })
    output$AutoInteraction_Scale <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='AutoInteraction_Scale', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Scale Data'), Choices = c(FALSE,TRUE), Multiple = TRUE, MaxVars = 1, SelectedDefault = TRUE)
    })
    output$AutoInteraction_Center <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='AutoInteraction_Center', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Center Data'), Choices = c(FALSE,TRUE), Multiple = TRUE, MaxVars = 1, SelectedDefault = TRUE)
    })
  })

  # input$ is a shinyWidgets::dropdown InputId
  shiny::observeEvent(input$TransformationInputs, {
    print('Transformation Inputs Dropdown')
    output$AutoTransformationCreate_SelectData <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='AutoTransformationCreate_SelectData', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Choose data set'), Choices = names(DataList), Multiple = TRUE, MaxVars = 1, SelectedDefault = names(DataList)[1L])
    })
    dt <- shiny::reactive({shiny::req(tryCatch({DataList[[input$AutoTransformationCreate_SelectData]]}, error = function(x) DataList[[1L]]))})
    output$AutoTransformationCreate_ColumnNames <- shiny::renderUI({
      nam <- RemixAutoML:::CEPP(x = names(dt())[which(unlist(lapply(dt(), is.numeric)))], Default = NULL)
      RemixAutoML:::SelectizeInput(InputID='AutoTransformationCreate_ColumnNames', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Variable Names'), Choices = nam, Multiple = TRUE, MaxVars = 1000)
    })
    output$AutoTransformationCreate_Methods <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='AutoTransformationCreate_Methods', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Trans Method'), Choices = c('Asinh','Log','LogPlus1','Sqrt','Asin','Logit','BoxCox','YeoJohnson'), Multiple = TRUE, MaxVars = 10)
    })
  })

  # input$ is a shinyWidgets::dropdown InputId
  shiny::observeEvent(input$PartialDummiesInputs, {
    print('Partial Dummies Inputs Dropdown')
    output$DummifyDT_SelectData <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='DummifyDT_SelectData', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Choose data set'), Choices = names(DataList), Multiple = TRUE, MaxVars = 1, SelectedDefault = names(DataList)[1L])
    })
    dt <- shiny::reactive({shiny::req(tryCatch({DataList[[input$DummifyDT_SelectData]]}, error = function(x) DataList[[1L]]))})
    output$DummifyDT_Cols <- shiny::renderUI({
      nam <- RemixAutoML:::CEPP(x = unique(c(names(dt())[which(unlist(lapply(dt(), is.character)))], names(dt())[which(unlist(lapply(dt(), is.factor)))])), Default = NULL)
      RemixAutoML:::SelectizeInput(InputID='DummifyDT_Cols', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Variable Names'), Choices = nam, Multiple = TRUE, MaxVars = 1000)
    })
    output$DummifyDT_TopN <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='DummifyDT_TopN', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'TopN'), Choices = 1:100, Multiple = FALSE, MaxVars = 100)
    })
    output$DummifyDT_KeepBaseCols <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='DummifyDT_KeepBaseCols', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Keep Base Cols'), Choices = c(TRUE, FALSE), Multiple = TRUE, MaxVars = 1)
    })
  })

  # CategoricalEncoding
  shiny::observeEvent(input$CategoricalEncodingInputs, {
    print('Categorical Encoding Inputs Dropdown')
    output$CategoricalEncoding_SelectData <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='CategoricalEncoding_SelectData', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Choose data set'), Choices = names(DataList), Multiple = TRUE, MaxVars = 1, SelectedDefault = names(DataList)[1L], CloseAfterSelect = FALSE)
    })
    dt <- shiny::reactive({shiny::req(tryCatch({DataList[[input$CategoricalEncoding_SelectData]]}, error = function(x) DataList[[1L]]))})
    output$CategoricalEncoding_GroupVariables <- shiny::renderUI({
      nam <- names(dt())
      RemixAutoML:::SelectizeInput(InputID='CategoricalEncoding_GroupVariables', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Group Variables'), Choices = nam, Multiple = TRUE, MaxVars = 100, CloseAfterSelect = FALSE)
    })
    output$CategoricalEncoding_TargetVariable <- shiny::renderUI({
      nam <- names(dt())
      RemixAutoML:::SelectizeInput(InputID='CategoricalEncoding_TargetVariable', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Target Name'), Choices = nam, Multiple = TRUE, MaxVars = 100, CloseAfterSelect = FALSE)
    })
    output$CategoricalEncoding_Method <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='CategoricalEncoding_Method', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Encoding Method'), Choices = c('credibility','target_encoding','m_estimator','woe','poly_encode','backward_difference','helmert'), Multiple = TRUE, MaxVars = 1)
    })
  })

  # input$ is a shinyWidgets::dropdown InputId
  shiny::observeEvent(input$AutoLagRollModeInputs, {
    print('AutoLagRollMode Inputs Dropdown')
    output$AutoLagRollMode_SelectData <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='AutoLagRollMode_SelectData', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Choose data set'), Choices = names(DataList), Multiple = TRUE, MaxVars = 1, SelectedDefault = names(DataList)[1L])
    })
    dt <- shiny::reactive({shiny::req(tryCatch({DataList[[input$AutoLagRollMode_SelectData]]}, error = function(x) DataList[[1L]]))})
    output$AutoLagRollMode_Lags <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='AutoLagRollMode_Lags', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Lags'), Choices = 1:250, Multiple = TRUE, MaxVars = 1000)
    })
    output$AutoLagRollMode_ModePeriods <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='AutoLagRollMode_ModePeriods', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Window Sizes'), Choices = 1:250, Multiple = TRUE, MaxVars = 1000)
    })
    output$AutoLagRollMode_Targets <- shiny::renderUI({
      nam <- RemixAutoML:::CEPP(x = unique(c(names(dt())[which(unlist(lapply(dt(), is.character)))], names(dt())[which(unlist(lapply(dt(), is.factor)))])), Default = NULL)
      RemixAutoML:::SelectizeInput(InputID='AutoLagRollMode_Targets', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Target Variables'), Choices = nam, Multiple = TRUE, MaxVars = 1000)
    })
    output$AutoLagRollMode_GroupingVars <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='AutoLagRollMode_GroupingVars', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'By-Variables'), Choices = names(dt()), Multiple = TRUE, MaxVars = 100)
    })
    output$AutoLagRollMode_SortDateName <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='AutoLagRollMode_SortDateName', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Date Variable'), Choices = names(dt()), Multiple = TRUE, MaxVars = 1)
    })
    output$AutoLagRollMode_WindowingLag <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='AutoLagRollMode_WindowingLag', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Window Lag'), Choices = c(FALSE,TRUE), Multiple = TRUE, MaxVars = 1, SelectedDefault = TRUE)
    })
  })

  # AutoLagRollStats
  shiny::observeEvent(input$AutoLagRollStatsInputs, {
    print('AutoLagRollStats Inputs Dropdown')
    output$AutoLagRollStats_SelectData <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='AutoLagRollStats_SelectData', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Choose data set'), Choices = names(DataList), Multiple = TRUE, MaxVars = 1, SelectedDefault = names(DataList)[1L])
    })
    dt <- shiny::reactive({shiny::req(tryCatch({DataList[[input$AutoLagRollStats_SelectData]]}, error = function(x) DataList[[1L]]))})
    output$AutoLagRollStats_Targets <- shiny::renderUI({
      nam <- RemixAutoML:::CEPP(x = names(dt())[which(unlist(lapply(dt(), is.numeric)))], Default = NULL)
      RemixAutoML:::SelectizeInput(InputID='AutoLagRollStats_Targets', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Target Variables'), Choices = nam, Multiple = TRUE, MaxVars = 1000)
    })
    output$AutoLagRollStats_GroupVars <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='AutoLagRollStats_GroupVars', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'By-Variables'), Choices = names(dt()), Multiple = TRUE, MaxVars = 100)
    })
    output$AutoLagRollStats_DateColumn <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='AutoLagRollStats_DateColumn', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Date Variable'), Choices = names(dt()), Multiple = TRUE, MaxVars = 1)
    })
    output$AutoLagRollStats_TimeUnits <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='AutoLagRollStats_TimeUnits', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Time Aggregation'), Choices = c('raw','hour','day','week','month','quarter','year'), Multiple = TRUE, MaxVars = 100000)
    })
    output$AutoLagRollStats_RollOnLag1 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='AutoLagRollStats_RollOnLag1', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Roll On Lag'), Choices = c(FALSE,TRUE), Multiple = TRUE, MaxVars = 1, SelectedDefault = TRUE)
    })
    output$AutoLagRollStats_Lags <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='AutoLagRollStats_Lags', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Lags'), Choices = c(1:250), Multiple = TRUE, MaxVars = 1000, SelectedDefault = 1)
    })
    output$AutoLagRollStats_MA_RollWindows <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='AutoLagRollStats_MA_RollWindows', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Rolling Mean'), Choices = c(2:250), Multiple = TRUE, MaxVars = 1000)
    })
    output$AutoLagRollStats_SD_RollWindows <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='AutoLagRollStats_SD_RollWindows', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Rolling StDev'), Choices = c(3:250), Multiple = TRUE, MaxVars = 1000)
    })
    output$AutoLagRollStats_Skew_RollWindows <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='AutoLagRollStats_Skew_RollWindows', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Rolling Skew'), Choices = c(4:250), Multiple = TRUE, MaxVars = 1000)
    })
    output$AutoLagRollStats_Kurt_RollWindows <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='AutoLagRollStats_Kurt_RollWindows', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Rolling Kurt'), Choices = c(5:250), Multiple = TRUE, MaxVars = 1000)
    })
    output$AutoLagRollStats_Quantile_RollWindows <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='AutoLagRollStats_Quantile_RollWindows', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Rolling Percentile'), Choices = c(5:250), Multiple = TRUE, MaxVars = 1000)
    })
    output$AutoLagRollStats_Quantiles_Selected <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='AutoLagRollStats_Quantiles_Selected', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Select Percentiles'), Choices = c('q5','q10','q15','q20','q25','q30','q35','q40','q45','q50','q55','q60','q65','q70','q75','q80','q85','q90','q95'), Multiple = TRUE, MaxVars = 100)
    })
  })

  # AutoDiffLagN
  shiny::observeEvent(input$AutoDiffInputs, {
    print('AutoDiff Inputs Dropdown')
    output$AutoDiffLagN_SelectData <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='AutoDiffLagN_SelectData', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Choose data set'), Choices = names(DataList), Multiple = TRUE, MaxVars = 1, SelectedDefault = names(DataList)[1L])
    })
    dt <- shiny::reactive({shiny::req(tryCatch({DataList[[input$AutoDiffLagN_SelectData]]}, error = function(x) DataList[[1L]]))})
    output$AutoDiffLagN_DateVariable <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='AutoDiffLagN_DateVariable', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Date Variable'), Choices = names(dt()), Multiple = TRUE, MaxVars = 1)
    })
    output$AutoDiffLagN_GroupVariables <- shiny::renderUI({
      #nam <- RemixAutoML:::CEPP(x = unique(c(names(dt())[which(unlist(lapply(dt(), is.character)))], names(dt())[which(unlist(lapply(dt(), is.factor)))])), Default = NULL)
      RemixAutoML:::SelectizeInput(InputID='AutoDiffLagN_GroupVariables', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'By-Variables'), Choices = names(dt()), Multiple = TRUE, MaxVars = 100)
    })
    output$AutoDiffLagN_DiffVariables <- shiny::renderUI({
      #nam <- RemixAutoML:::CEPP(x = names(dt())[which(unlist(lapply(dt(), is.numeric)))], Default = NULL)
      RemixAutoML:::SelectizeInput(InputID='AutoDiffLagN_DiffVariables', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Numeric Diff Variables'), Choices = names(dt()), Multiple = TRUE, MaxVars = 100)
    })
    output$AutoDiffLagN_DiffDateVariables <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='AutoDiffLagN_DiffDateVariables', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Date Diff Variables'), Choices = names(dt()), Multiple = TRUE, MaxVars = 1)
    })
    output$AutoDiffLagN_DiffGroupVariables <- shiny::renderUI({
      #nam <- RemixAutoML:::CEPP(x = unique(c(names(dt())[which(unlist(lapply(dt(), is.character)))], names(dt())[which(unlist(lapply(dt(), is.factor)))])), Default = NULL)
      RemixAutoML:::SelectizeInput(InputID='AutoDiffLagN_DiffGroupVariables', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Group Diff Variables'), Choices = names(dt()), Multiple = TRUE, MaxVars = 100)
    })
    output$AutoDiffLagN_NLag1 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='AutoDiffLagN_NLag1', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Base Period'), Choices = c(0:100), Multiple = TRUE, MaxVars = 1, SelectedDefault = 0)
    })
    output$AutoDiffLagN_NLag2 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='AutoDiffLagN_NLag2', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Lookback Period'), Choices = c(1:100), Multiple = TRUE, MaxVars = 1, SelectedDefault = 1)
    })
  })

  # ModelDataPrep
  shiny::observeEvent(input$ModelDataPrepInputs, {
    print('Data Prep Inputs Dropdown')
    output$ModelDataPrep_SelectData <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='ModelDataPrep_SelectData', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Choose data set'), Choices = names(DataList), Multiple = TRUE, MaxVars = 1, SelectedDefault = names(DataList)[1L])
    })
    dt <- shiny::reactive({shiny::req(tryCatch({DataList[[input$ModelDataPrep_SelectData]]}, error = function(x) DataList[[1L]]))})
    output$ModelDataPrep_IgnoreCols <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='ModelDataPrep_IgnoreCols', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Skip Columns'), Choices = names(dt()), Multiple = TRUE, MaxVars = 1000)
    })
    output$ModelDataPrep_CharToFactor <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='ModelDataPrep_CharToFactor', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Char to Factor'), Choices = c(FALSE,TRUE), Multiple = TRUE, MaxVars = 1, SelectedDefault = FALSE)
    })
    output$ModelDataPrep_FactorToChar <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='ModelDataPrep_FactorToChar', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Factor to Char'), Choices = c(FALSE,TRUE), Multiple = TRUE, MaxVars = 1, SelectedDefault = FALSE)
    })
    output$ModelDataPrep_DateToChar <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='ModelDataPrep_DateToChar', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Date to Char'), Choices = c(FALSE,TRUE), Multiple = TRUE, MaxVars = 1, SelectedDefault = FALSE)
    })
    output$ModelDataPrep_IDateConversion <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='ModelDataPrep_IDateConversion', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'IDate to Date'), Choices = c(FALSE,TRUE), Multiple = TRUE, MaxVars = 1, SelectedDefault = FALSE)
    })
    output$ModelDataPrep_RemoveDates <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='ModelDataPrep_RemoveDates', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Remove Dates'), Choices = c(FALSE,TRUE), Multiple = TRUE, MaxVars = 1, SelectedDefault = FALSE)
    })
    output$ModelDataPrep_IntToNumeric <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='ModelDataPrep_IntToNumeric', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Integer to Numeric'), Choices = c(FALSE,TRUE), Multiple = TRUE, MaxVars = 1, SelectedDefault = FALSE)
    })
    output$ModelDataPrep_LogicalToBinary <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='ModelDataPrep_LogicalToBinary', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Logical to Binary'), Choices = c(FALSE,TRUE), Multiple = TRUE, MaxVars = 1, SelectedDefault = FALSE)
    })
    output$ModelDataPrep_MissFactor <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='ModelDataPrep_MissFactor', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Character Impute'), Choices = c('0','missing','NULL'), Multiple = TRUE, MaxVars = 1, SelectedDefault = '0')
    })
    output$ModelDataPrep_MissNum <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='ModelDataPrep_MissNum', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Numeric Impute'), Choices = c(-1, 0, -999, 999, 0.001, -0.001), Multiple = TRUE, MaxVars = 1, SelectedDefault = -1)
    })
  })

  # AutoDataPartition
  shiny::observeEvent(input$AutoDataPartitionInputs, {
    print('Partition Data Inputs Dropdown')
    output$AutoDataPartition_SelectData <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='AutoDataPartition_SelectData', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Choose data set'), Choices = names(DataList), Multiple = TRUE, MaxVars = 1, SelectedDefault = names(DataList)[1L])
    })
    dt <- shiny::reactive({shiny::req(tryCatch({DataList[[input$AutoDataPartition_SelectData]]}, error = function(x) DataList[[1L]]))})
    output$AutoDataPartition_NumDataSets <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='AutoDataPartition_NumDataSets', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Choose data set'), Choices = c(2,3), Multiple = TRUE, MaxVars = 1, SelectedDefault = 3)
    })
    output$AutoDataPartition_Ratios_Train <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='AutoDataPartition_Ratios_Train', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Train %'), Choices = c(seq(0.50, 0.95, 0.05)), Multiple = TRUE, MaxVars = 1, SelectedDefault = c(0.70))
    })
    output$AutoDataPartition_Ratios_Validation <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='AutoDataPartition_Ratios_Validation', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Validation %'), Choices = c(seq(0.0, 0.50, 0.05)), Multiple = TRUE, MaxVars = 1, SelectedDefault = c(0.20))
    })
    output$AutoDataPartition_Ratios_Test <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='AutoDataPartition_Ratios_Test', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Test %'), Choices = c(seq(0.0, 0.50, 0.05)), Multiple = TRUE, MaxVars = 1, SelectedDefault = c(0.10))
    })
    output$AutoDataPartition_PartitionType <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='AutoDataPartition_PartitionType', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Partition Type'), Choices = c('random', 'time'), Multiple = TRUE, MaxVars = 1, SelectedDefault = 'random')
    })
    output$AutoDataPartition_StratifyColumnNames <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='AutoDataPartition_StratifyColumnNames', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Numeric Impute'), Choices = c(names(dt())), Multiple = TRUE, MaxVars = 10, SelectedDefault = NULL)
    })
    output$AutoDataPartition_TimeColumnName <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='AutoDataPartition_TimeColumnName', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Numeric Impute'), Choices = c(names(dt())), Multiple = TRUE, MaxVars = 1, SelectedDefault = NULL)
    })
  })

  # Save Data
  shiny::observeEvent(input$SaveDataInputs, {
    print('Save Data Inputs Dropdown')
    output$SaveData_SelectData <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='SaveData_SelectData', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Choose data set'), Choices = names(DataList), Multiple = TRUE, MaxVars = 1, SelectedDefault = names(DataList)[1L])
    })
    output$SaveData_SelectDataPostGRE <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='SaveData_SelectDataPostGRE', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Choose data set'), Choices = names(DataList), Multiple = TRUE, MaxVars = 1, SelectedDefault = names(DataList)[1L])
    })
    save_data <- shiny::reactive({shiny::req(tryCatch({DataList[[input$SaveData_SelectData]]}, error = function(x) DataList[[1L]]))})
    output$SaveData_CSV <- shiny::downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        data.table::fwrite(x = shiny::isolate(save_data()), file = file.path(file))
      })

    # Get data
    output$SaveData_DataBaseName <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='SaveData_DataBaseName', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'PostGRE Database Name'), Choices = c(LocalPostGRE_DBNames), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1L, CloseAfterSelect = FALSE, Debug = Debug)
    })
    output$SaveData_TableName <- shiny::renderUI({
      RemixAutoML:::TextInput(InputID='SaveData_TableName', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'PostGRE Table Name'), Value = paste0('TEMP_', substr(x = as.character(round(runif(1),5)), start = 3, nchar(as.character(round(runif(1),5))))), Placeholder = NULL)
    })
  })

  # PostGRE Save Data
  shiny::observeEvent(input$PostGRE_Push, {

    # Data Name
    TableName <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['SaveData_TableName']]}, error=function(x) NULL), Type='character', Default=NULL)

    # Database Name
    DataBaseName <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['SaveData_DataBaseName']]}, error=function(x) NULL), Type='character', Default=NULL)

    # Data
    postgre_data <- tryCatch({shiny::req(DataList[[input[['SaveData_SelectDataPostGRE']]]])}, error = function(x) NULL)

    # Push to PostGRE
    if(length(TableName) != 0L && length(DataBaseName) != 0L && data.table::is.data.table(postgre_data)) {
      RemixAutoML::PostGRE_RemoveCreateAppend(
        data = postgre_data,
        DBName = DataBaseName,
        TableName = TableName,
        Host = LocalPostGRE_Host,
        User = LocalPostGRE_User,
        Port = LocalPostGRE_Port,
        Password = LocalPostGRE_PasswordPassword,
        CloseConnection = TRUE,
        CreateSchema = NULL,
        Temporary = FALSE,
        Connection = NULL,
        Append = TRUE)
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = 'success', btn_colors = 'green', html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {
      print('length(TableName) == 0L or length(DataBaseName) == 0L or postgre_data is not a data.table')
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = 'data not sent', btn_colors = 'red', html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  })

  # Initialize DataTable output
  shiny::observeEvent(input$sidebar, {
    if(input$sidebar == 'FeatureEngineering') {

      # Meta
      FE_Validate_Counter <<- FE_Validate_Counter + 1L
      if(Debug) {print(FE_Validate_Counter); print(CurrentData); print(names(DataList)); print(' :: R#2184 error ::'); print(FE_Validate); print('input$sidebar'); print(input$sidebar)}

      # Data
      data <- DataList[[CurrentData]]

      # Display DataTable
      if(FE_Validate_Counter <= 3L) {
        output$FE_DisplayData <- DT::renderDataTable({
          if(Debug) {print('here and data is :'); print(data)}
          RemixAutoML::DataTable(data[seq_len(min(.N, NNN))])
        })
        FE_Validate <<- FALSE
      }
    }
  })

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  #    Inputs CatBoost ML                ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  # CatBoost DropDown
  shiny::observeEvent(input$CatBoost, {

    # Create List
    if(!exists('CatBoost')) CatBoost <- list(Debug = Debug)
    print('CatBoost Target Type')

    # Target Type
    output$CatBoost_TargetType <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = CatBoost, InputName = 'CatBoost_TargetType', ArgName = 'SelectedDefault', Default = 'Regression', Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID = 'CatBoost_TargetType', Label = 'Select Supervised Learning Type', Choices = c('Regression','Binary Classification','MultiClass'), SelectedDefault = selected_default, Multiple = TRUE, MaxVars = 1, Debug = Debug)
    })

    # Create reactive
    TT <- shiny::reactive({input$CatBoost_TargetType})
    TT <<- TT

    # Args Storage
    CatBoost[['CatBoost_TargetType']][['SelectedDefault']][[length(CatBoost[['CatBoost_TargetType']][['SelectedDefault']]) + 1L]] <- input$CatBoost_TargetType
    CatBoost <<- CatBoost
  })

  # CatBoost MetaData Parameters
  shiny::observeEvent(input$Cat_MetaData, {

    # Initialize List
    if(!exists('Cat_MetaData')) Cat_MetaData <- list(Debug = Debug)
    print('Metadata Parameters')

    # Pass In Grid
    output$CatBoost_PassInGrid <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = Cat_MetaData, InputName = 'CatBoost_PassInGrid', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='CatBoost_PassInGrid', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Previous Grid Data'), Choices = names(DataList), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    Cat_MetaData[['CatBoost_PassInGrid']][['SelectedDefault']][[length(Cat_MetaData[['CatBoost_PassInGrid']][['SelectedDefault']]) + 1L]] <- input$CatBoost_PassInGrid
    Cat_MetaData <<- Cat_MetaData

    # Task Type
    output$CatBoost_task_type <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = Cat_MetaData, InputName = 'CatBoost_task_type', ArgName = 'SelectedDefault', Default = 'CPU', Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='CatBoost_task_type', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'GPU or CPU'), Choices = c('GPU','CPU'), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    Cat_MetaData[['CatBoost_task_type']][['SelectedDefault']][[length(Cat_MetaData[['CatBoost_task_type']][['SelectedDefault']]) + 1L]] <- input$CatBoost_task_type
    Cat_MetaData <<- Cat_MetaData

    # Number of GPUs
    output$CatBoost_NumGPUs <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = Cat_MetaData, InputName = 'CatBoost_NumGPUs', ArgName = 'SelectedDefault', Default = 1L, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='CatBoost_NumGPUs', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Number of GPUs'), Choices = seq_len(10L), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    Cat_MetaData[['CatBoost_NumGPUs']][['SelectedDefault']][[length(Cat_MetaData[['CatBoost_NumGPUs']][['SelectedDefault']]) + 1L]] <- input$CatBoost_NumGPUs
    Cat_MetaData <<- Cat_MetaData

    # Train on Full Data
    output$CatBoost_TrainOnFull <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = Cat_MetaData, InputName = 'CatBoost_TrainOnFull', ArgName = 'SelectedDefault', Default = FALSE, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='CatBoost_TrainOnFull', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Train on Full Data'), Choices = c(FALSE, TRUE), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    Cat_MetaData[['CatBoost_TrainOnFull']][['SelectedDefault']][[length(Cat_MetaData[['CatBoost_TrainOnFull']][['SelectedDefault']]) + 1L]] <- input$CatBoost_TrainOnFull
    Cat_MetaData <<- Cat_MetaData

    # Model ID
    output$CatBoost_ModelID <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = Cat_MetaData, InputName = 'CatBoost_ModelID', ArgName = 'SelectedDefault', Default = 'Model1', Debug = Debug)
      RemixAutoML:::TextInput(InputID='CatBoost_ModelID', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Create Model ID'), Value = selected_default, Placeholder = 'Name your model')
    })

    # Args Storage
    Cat_MetaData[['CatBoost_ModelID']][['SelectedDefault']][[length(Cat_MetaData[['CatBoost_ModelID']][['SelectedDefault']]) + 1L]] <- input$CatBoost_ModelID
    Cat_MetaData <<- Cat_MetaData
  })

  # CatBoost Data Parameters
  shiny::observeEvent(input$Cat_DataParameters, {

    # Initialize List
    if(!exists('Cat_DataParameters')) Cat_DataParameters <- list(Debug = Debug)
    print('Data Parameters')

    # Data
    output$CatBoost_data <- shiny::renderUI({
      print('CatBoost Data Parameters 1')
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = Cat_DataParameters, InputName = 'CatBoost_data', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='CatBoost_data', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Select Training Data'), Choices = names(DataList), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    Cat_DataParameters[['CatBoost_data']][['SelectedDefault']][[length(Cat_DataParameters[['CatBoost_data']][['SelectedDefault']]) + 1L]] <- input$CatBoost_data
    Cat_DataParameters <<- Cat_DataParameters

    # Reactive
    ML_dt <- shiny::reactive({tryCatch({DataList[[input$CatBoost_data]]}, error = function(x) DataList[[1L]])})

    # Validation Data
    output$CatBoost_ValidationData <- shiny::renderUI({
      print('CatBoost Data Parameters 2')
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = Cat_DataParameters, InputName = 'CatBoost_ValidationData', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='CatBoost_ValidationData', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Select Validation Data'), Choices = names(DataList), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    Cat_DataParameters[['CatBoost_ValidationData']][['SelectedDefault']][[length(Cat_DataParameters[['CatBoost_ValidationData']][['SelectedDefault']]) + 1L]] <- input$CatBoost_ValidationData
    Cat_DataParameters <<- Cat_DataParameters

    # Test Data
    output$CatBoost_TestData <- shiny::renderUI({
      print('CatBoost Data Parameters 3')
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = Cat_DataParameters, InputName = 'CatBoost_TestData', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='CatBoost_TestData', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Select Test Data'), Choices = names(DataList), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    Cat_DataParameters[['CatBoost_TestData']][['SelectedDefault']][[length(Cat_DataParameters[['CatBoost_TestData']][['SelectedDefault']]) + 1L]] <- input$CatBoost_TestData
    Cat_DataParameters <<- Cat_DataParameters

    # Target Column
    output$CatBoost_TargetColumnName <- shiny::renderUI({
      print('CatBoost Data Parameters 4')
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = Cat_DataParameters, InputName = 'CatBoost_TargetColumnName', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='CatBoost_TargetColumnName', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Select Target'), Choices = names(ML_dt()), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    Cat_DataParameters[['CatBoost_TargetColumnName']][['SelectedDefault']][[length(Cat_DataParameters[['CatBoost_TargetColumnName']][['SelectedDefault']]) + 1L]] <- input$CatBoost_TargetColumnName
    Cat_DataParameters <<- Cat_DataParameters

    # Feature Columns
    output$CatBoost_FeatureColNames <- shiny::renderUI({
      print('CatBoost Data Parameters 5')
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = Cat_DataParameters, InputName = 'CatBoost_FeatureColNames', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='CatBoost_FeatureColNames', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Select Features'), Choices = names(ML_dt()), Multiple = TRUE, MaxVars = 100, SelectedDefault = selected_default)
    })

    # Args Storage
    Cat_DataParameters[['CatBoost_FeatureColNames']][['SelectedDefault']][[length(Cat_DataParameters[['CatBoost_FeatureColNames']][['SelectedDefault']]) + 1L]] <- input$CatBoost_FeatureColNames
    Cat_DataParameters <<- Cat_DataParameters

    # Date Column
    output$CatBoost_PrimaryDateColumn <- shiny::renderUI({
      print('Adrian Antico 6')
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = Cat_DataParameters, InputName = 'CatBoost_PrimaryDateColumn', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='CatBoost_PrimaryDateColumn', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Select Date'), Choices = names(ML_dt()), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    Cat_DataParameters[['CatBoost_PrimaryDateColumn']][['SelectedDefault']][[length(Cat_DataParameters[['CatBoost_PrimaryDateColumn']][['SelectedDefault']]) + 1L]] <- input$CatBoost_PrimaryDateColumn
    Cat_DataParameters <<- Cat_DataParameters

    # Date Column
    output$CatBoost_EncodeMethod <- shiny::renderUI({
      print('Adrian Antico 6')
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = Cat_DataParameters, InputName = 'CatBoost_EncodeMethod', ArgName = 'SelectedDefault', Default = 'credibility', Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='CatBoost_EncodeMethod', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Character Encoding'), Choices = c('credibility','binary','m_estimator','woe','target_encoding','poly_encode','backward_difference','helmert'), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    Cat_DataParameters[['CatBoost_EncodeMethod']][['SelectedDefault']][[length(Cat_DataParameters[['CatBoost_EncodeMethod']][['SelectedDefault']]) + 1L]] <- input$CatBoost_PrimaryDateColumn
    Cat_DataParameters <<- Cat_DataParameters


    # Weigths Column
    output$CatBoost_WeightsColumnName <- shiny::renderUI({
      print('CatBoost Data Parameters 7')
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = Cat_DataParameters, InputName = 'CatBoost_WeightsColumnName', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='CatBoost_WeightsColumnName', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Weights Column'), Choices = names(ML_dt()), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    Cat_DataParameters[['CatBoost_WeightsColumnName']][['SelectedDefault']][[length(Cat_DataParameters[['CatBoost_WeightsColumnName']][['SelectedDefault']]) + 1L]] <- input$CatBoost_WeightsColumnName
    Cat_DataParameters <<- Cat_DataParameters

    # ID Columns
    output$CatBoost_IDcols <- shiny::renderUI({
      print('CatBoost Data Parameters 8')
      gg <- c(input$CatBoost_TargetColumnName, input$CatBoost_FeatureColNames, input$CatBoost_PrimaryDateColumn, input$CatBoost_WeightsColumnName)
      gg <- gg[!is.na(gg)]; hh <- tryCatch({names(ML_dt())}, error = function(x) NULL); choices <- tryCatch({hh[!hh %in% gg]}, error = function(x) NULL)
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = Cat_DataParameters, InputName = 'CatBoost_IDcols', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='CatBoost_IDcols', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'ID Columns'), Choices = choices, Multiple = TRUE, MaxVars = 100, SelectedDefault = selected_default)
    })

    # Args Storage
    Cat_DataParameters[['CatBoost_IDcols']][['SelectedDefault']][[length(Cat_DataParameters[['CatBoost_IDcols']][['SelectedDefault']]) + 1L]] <- input$CatBoost_IDcols
    Cat_DataParameters <<- Cat_DataParameters

    # Transformation Columns
    output$CatBoost_TransformNumericColumns <- shiny::renderUI({
      print('CatBoost Data Parameters 10')
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = Cat_DataParameters, InputName = 'CatBoost_TransformNumericColumns', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='CatBoost_TransformNumericColumns', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Transform Columns'), Choices = names(ML_dt()), Multiple = TRUE, MaxVars = 100, SelectedDefault = selected_default)
    })

    # Args Storage
    Cat_DataParameters[['CatBoost_TransformNumericColumns']][['SelectedDefault']][[length(Cat_DataParameters[['CatBoost_TransformNumericColumns']][['SelectedDefault']]) + 1L]] <- input$CatBoost_TransformNumericColumns
    Cat_DataParameters <<- Cat_DataParameters

    # Transformation Methods
    output$CatBoost_Methods <- shiny::renderUI({
      print('CatBoost Data Parameters 11')
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = Cat_DataParameters, InputName = 'CatBoost_Methods', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='CatBoost_Methods', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Transform Methods'), Choices = c('BoxCox', 'Asinh', 'Log', 'LogPlus1', 'Sqrt', 'Asin', 'Logit'), Multiple = TRUE, MaxVars = 100, SelectedDefault = selected_default)
    })

    # Args Storage
    Cat_DataParameters[['CatBoost_Methods']][['SelectedDefault']][[length(Cat_DataParameters[['CatBoost_Methods']][['SelectedDefault']]) + 1L]] <- input$CatBoost_Methods
    Cat_DataParameters <<- Cat_DataParameters
  })

  # CatBoost Grid Tuning Parameters
  shiny::observeEvent(input$Cat_GridTuningParameters, {

    # Initialize List
    if(!exists('Cat_GridTuningParameters')) Cat_GridTuningParameters <- list(Debug = Debug)
    print('Grid Tuning Parameters')

    # Grid Tune
    output$CatBoost_GridTune <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = Cat_GridTuningParameters, InputName = 'CatBoost_GridTune', ArgName = 'SelectedDefault', Default = FALSE, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='CatBoost_GridTune', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Grid Tune'), Choices = c(FALSE, TRUE), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    Cat_GridTuningParameters[['CatBoost_GridTune']][['SelectedDefault']][[length(Cat_GridTuningParameters[['CatBoost_GridTune']][['SelectedDefault']]) + 1L]] <- input$CatBoost_GridTune
    Cat_GridTuningParameters <<- Cat_GridTuningParameters

    # grid eval metric
    output$CatBoost_grid_eval_metric <- shiny::renderUI({
      out <- RemixAutoML:::CatBoostGridEvalMetricsOptions(TT())
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = Cat_GridTuningParameters, InputName = 'CatBoost_grid_eval_metric', ArgName = 'SelectedDefault', Default = out$Default, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='CatBoost_grid_eval_metric', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Evaluation Metric'), Choices = out$Choices, Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    Cat_GridTuningParameters[['CatBoost_grid_eval_metric']][['SelectedDefault']][[length(Cat_GridTuningParameters[['CatBoost_grid_eval_metric']][['SelectedDefault']]) + 1L]] <- input$CatBoost_grid_eval_metric
    Cat_GridTuningParameters <<- Cat_GridTuningParameters

    # Max Models
    output$CatBoost_MaxModelsInGrid <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = Cat_GridTuningParameters, InputName = 'CatBoost_MaxModelsInGrid', ArgName = 'SelectedDefault', Default = 25, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='CatBoost_MaxModelsInGrid', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Max Models'), Choices = c(seq(5,20,5),seq(25,2500,25)), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    Cat_GridTuningParameters[['CatBoost_MaxModelsInGrid']][['SelectedDefault']][[length(Cat_GridTuningParameters[['CatBoost_MaxModelsInGrid']][['SelectedDefault']]) + 1L]] <- input$CatBoost_MaxModelsInGrid
    Cat_GridTuningParameters <<- Cat_GridTuningParameters

    # Runs without a New Winner
    output$CatBoost_MaxRunsWithoutNewWinner <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = Cat_GridTuningParameters, InputName = 'CatBoost_MaxRunsWithoutNewWinner', ArgName = 'SelectedDefault', Default = 10, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='CatBoost_MaxRunsWithoutNewWinner', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Runs Without Winner'), Choices = c(seq(5,20,5),seq(25,2500,25)), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    Cat_GridTuningParameters[['CatBoost_MaxRunsWithoutNewWinner']][['SelectedDefault']][[length(Cat_GridTuningParameters[['CatBoost_MaxRunsWithoutNewWinner']][['SelectedDefault']]) + 1L]] <- input$CatBoost_MaxRunsWithoutNewWinner
    Cat_GridTuningParameters <<- Cat_GridTuningParameters

    # Max Runtime in Minutes
    output$CatBoost_MaxRunMinutes <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = Cat_GridTuningParameters, InputName = 'CatBoost_MaxRunMinutes', ArgName = 'SelectedDefault', Default = 30, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='CatBoost_MaxRunMinutes', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Max Run Time (mins)'), Choices = c(seq(30,60*24*7,30)), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    Cat_GridTuningParameters[['CatBoost_MaxRunMinutes']][['SelectedDefault']][[length(Cat_GridTuningParameters[['CatBoost_MaxRunMinutes']][['SelectedDefault']]) + 1L]] <- input$CatBoost_MaxRunMinutes
    Cat_GridTuningParameters <<- Cat_GridTuningParameters

    # Baseline Comparison
    output$CatBoost_BaselineComparison <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = Cat_GridTuningParameters, InputName = 'CatBoost_BaselineComparison', ArgName = 'SelectedDefault', Default = 'default', Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='CatBoost_BaselineComparison', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Evaluation Metric'), Choices = c('default','best'), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    Cat_GridTuningParameters[['CatBoost_BaselineComparison']][['SelectedDefault']][[length(Cat_GridTuningParameters[['CatBoost_BaselineComparison']][['SelectedDefault']]) + 1L]] <- input$CatBoost_BaselineComparison
    Cat_GridTuningParameters <<- Cat_GridTuningParameters
  })

  # CatBoost ML Parameters
  shiny::observeEvent(input$Cat_MLParameters, {

    # Initialize List
    if(!exists('Cat_MLParameters')) Cat_MLParameters <- list(Debug = Debug)
    print('ML Parameters')

    # Trees
    output$CatBoost_Trees <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = Cat_MLParameters, InputName = 'CatBoost_Trees', ArgName = 'SelectedDefault', Default = 1000, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='CatBoost_Trees', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Number of Trees'), Choices = c(5:50, seq(75,475,25), seq(500,9500,500), seq(10000, 25000, 1000)), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    Cat_MLParameters[['CatBoost_Trees']][['SelectedDefault']][[length(Cat_MLParameters[['CatBoost_Trees']][['SelectedDefault']]) + 1L]] <- input$CatBoost_Trees
    Cat_MLParameters <<- Cat_MLParameters

    # Depth
    output$CatBoost_Depth <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = Cat_MLParameters, InputName = 'CatBoost_Depth', ArgName = 'SelectedDefault', Default = 8, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='CatBoost_Depth', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Max Tree Depth'), Choices = 4:20, Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    Cat_MLParameters[['CatBoost_Depth']][['SelectedDefault']][[length(Cat_MLParameters[['CatBoost_Depth']][['SelectedDefault']]) + 1L]] <- input$CatBoost_Depth
    Cat_MLParameters <<- Cat_MLParameters

    # Learning Rate
    output$CatBoost_LearningRate <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = Cat_MLParameters, InputName = 'CatBoost_LearningRate', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='CatBoost_LearningRate', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Learning Rate'), Choices = seq(0.01,0.50,0.002), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    Cat_MLParameters[['CatBoost_LearningRate']][['SelectedDefault']][[length(Cat_MLParameters[['CatBoost_LearningRate']][['SelectedDefault']]) + 1L]] <- input$CatBoost_LearningRate
    Cat_MLParameters <<- Cat_MLParameters

    # L2 Regularization
    output$CatBoost_L2_Leaf_Reg <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = Cat_MLParameters, InputName = 'CatBoost_L2_Leaf_Reg', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='CatBoost_L2_Leaf_Reg', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'L2 Leaf Regularize'), Choices = 0:50, Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    Cat_MLParameters[['CatBoost_L2_Leaf_Reg']][['SelectedDefault']][[length(Cat_MLParameters[['CatBoost_L2_Leaf_Reg']][['SelectedDefault']]) + 1L]] <- input$CatBoost_L2_Leaf_Reg
    Cat_MLParameters <<- Cat_MLParameters

    # Model Size Regularization
    output$CatBoost_model_size_reg <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = Cat_MLParameters, InputName = 'CatBoost_model_size_reg', ArgName = 'SelectedDefault', Default = 0.50, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='CatBoost_model_size_reg', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Model Size Regularize'), Choices = seq(0.05,5,0.05), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    Cat_MLParameters[['CatBoost_model_size_reg']][['SelectedDefault']][[length(Cat_MLParameters[['CatBoost_model_size_reg']][['SelectedDefault']]) + 1L]] <- input$CatBoost_model_size_reg
    Cat_MLParameters <<- Cat_MLParameters

    # langevin
    output$CatBoost_langevin <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = Cat_MLParameters, InputName = 'CatBoost_langevin', ArgName = 'SelectedDefault', Default = FALSE, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='CatBoost_langevin', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Langevin Boosting'), Choices = c(FALSE,TRUE), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    Cat_MLParameters[['CatBoost_langevin']][['SelectedDefault']][[length(Cat_MLParameters[['CatBoost_langevin']][['SelectedDefault']]) + 1L]] <- input$CatBoost_langevin
    Cat_MLParameters <<- Cat_MLParameters

    # Diffusion Temperature
    output$CatBoost_diffusion_temperature <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = Cat_MLParameters, InputName = 'CatBoost_diffusion_temperature', ArgName = 'SelectedDefault', Default = 10000, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='CatBoost_diffusion_temperature', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Diffusion Temp'), Choices = c(5000,7500,10000,12500,15000), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    Cat_MLParameters[['CatBoost_diffusion_temperature']][['SelectedDefault']][[length(Cat_MLParameters[['CatBoost_diffusion_temperature']][['SelectedDefault']]) + 1L]] <- input$CatBoost_diffusion_temperature
    Cat_MLParameters <<- Cat_MLParameters

    # Random Strength
    output$CatBoost_RandomStrength <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = Cat_MLParameters, InputName = 'CatBoost_RandomStrength', ArgName = 'SelectedDefault', Default = 1, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='CatBoost_RandomStrength', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Random Strength'), Choices = seq(0.50, 1, 0.05), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    Cat_MLParameters[['CatBoost_RandomStrength']][['SelectedDefault']][[length(Cat_MLParameters[['CatBoost_RandomStrength']][['SelectedDefault']]) + 1L]] <- input$CatBoost_RandomStrength
    Cat_MLParameters <<- Cat_MLParameters

    # Border Count
    output$CatBoost_BorderCount <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = Cat_MLParameters, InputName = 'CatBoost_BorderCount', ArgName = 'SelectedDefault', Default = 256, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='CatBoost_BorderCount', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Border Count'), Choices = seq(32,256,32), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    Cat_MLParameters[['CatBoost_BorderCount']][['SelectedDefault']][[length(Cat_MLParameters[['CatBoost_BorderCount']][['SelectedDefault']]) + 1L]] <- input$CatBoost_BorderCount
    Cat_MLParameters <<- Cat_MLParameters

    # Random Subspace Method
    output$CatBoost_RSM <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = Cat_MLParameters, InputName = 'CatBoost_RSM', ArgName = 'SelectedDefault', Default = 1, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='CatBoost_RSM', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Random Subspace'), Choices = seq(0.01,1,0.01), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    Cat_MLParameters[['CatBoost_RSM']][['SelectedDefault']][[length(Cat_MLParameters[['CatBoost_RSM']][['SelectedDefault']]) + 1L]] <- input$CatBoost_RSM
    Cat_MLParameters <<- Cat_MLParameters

    # Bootstrap Type
    output$CatBoost_BootStrapType <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = Cat_MLParameters, InputName = 'CatBoost_BootStrapType', ArgName = 'SelectedDefault', Default = 'Bayesian', Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='CatBoost_BootStrapType', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Bootstrap Type'), Choices = c('Bayesian', 'Bernoulli', 'Poisson', 'MVS', 'No'), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    Cat_MLParameters[['CatBoost_BootStrapType']][['SelectedDefault']][[length(Cat_MLParameters[['CatBoost_BootStrapType']][['SelectedDefault']]) + 1L]] <- input$CatBoost_BootStrapType
    Cat_MLParameters <<- Cat_MLParameters

    # Grow Policy
    output$CatBoost_GrowPolicy <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = Cat_MLParameters, InputName = 'CatBoost_GrowPolicy', ArgName = 'SelectedDefault', Default = 'SymmetricTree', Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='CatBoost_GrowPolicy', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Border Count'), Choices = c('SymmetricTree', 'Depthwise', 'Lossguide'), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    Cat_MLParameters[['CatBoost_GrowPolicy']][['SelectedDefault']][[length(Cat_MLParameters[['CatBoost_GrowPolicy']][['SelectedDefault']]) + 1L]] <- input$CatBoost_GrowPolicy
    Cat_MLParameters <<- Cat_MLParameters

    # Feature Border Type
    output$CatBoost_feature_border_type <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = Cat_MLParameters, InputName = 'CatBoost_feature_border_type', ArgName = 'SelectedDefault', Default = 'GreedyLogSum', Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='CatBoost_feature_border_type', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Border Count'), Choices = c('GreedyLogSum', 'Median', 'Uniform', 'UniformAndQuantiles', 'MaxLogSum', 'MinEntropy'), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    Cat_MLParameters[['CatBoost_feature_border_type']][['SelectedDefault']][[length(Cat_MLParameters[['CatBoost_feature_border_type']][['SelectedDefault']]) + 1L]] <- input$CatBoost_feature_border_type
    Cat_MLParameters <<- Cat_MLParameters

    # Subsample
    output$CatBoost_subsample <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = Cat_MLParameters, InputName = 'CatBoost_subsample', ArgName = 'SelectedDefault', Default = 1, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='CatBoost_subsample', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Bagging Rate'), Choices = seq(0.50,1,0.01), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    Cat_MLParameters[['CatBoost_subsample']][['SelectedDefault']][[length(Cat_MLParameters[['CatBoost_subsample']][['SelectedDefault']]) + 1L]] <- input$CatBoost_subsample
    Cat_MLParameters <<- Cat_MLParameters

    # Score Function
    output$CatBoost_score_function <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = Cat_MLParameters, InputName = 'CatBoost_score_function', ArgName = 'SelectedDefault', Default = 'Cosine', Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='CatBoost_score_function', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Score Function'), Choices = c('Cosine', 'L2', 'NewtonL2', 'NewtomCosine'), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    Cat_MLParameters[['CatBoost_score_function']][['SelectedDefault']][[length(Cat_MLParameters[['CatBoost_score_function']][['SelectedDefault']]) + 1L]] <- input$CatBoost_score_function
    Cat_MLParameters <<- Cat_MLParameters

    # Min data in leaf
    output$CatBoost_min_data_in_leaf <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = Cat_MLParameters, InputName = 'CatBoost_min_data_in_leaf', ArgName = 'SelectedDefault', Default = 1, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='CatBoost_min_data_in_leaf', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Min Data Leaf'), Choices = 1:20, Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    Cat_MLParameters[['CatBoost_min_data_in_leaf']][['SelectedDefault']][[length(Cat_MLParameters[['CatBoost_min_data_in_leaf']][['SelectedDefault']]) + 1L]] <- input$CatBoost_min_data_in_leaf
    Cat_MLParameters <<- Cat_MLParameters
  })

  # CatBoost Eval Parameters
  shiny::observeEvent(input$Cat_EvalParameters, {

    # Initialize List
    if(!exists('Cat_EvalParameters')) Cat_EvalParameters <- list(Debug = Debug)
    print('Evaluation Parameters')

    # Loss Function
    output$CatBoost_LossFunction <- shiny::renderUI({
      out <- RemixAutoML:::CatBoostLossFunctionOptions(TT())
      print(out)
      print(TT())
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = Cat_EvalParameters, InputName = 'CatBoost_LossFunction', ArgName = 'SelectedDefault', Default = out$Default, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='CatBoost_LossFunction', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Loss Function'), Choices = out$Choices, Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    Cat_EvalParameters[['CatBoost_LossFunction']][['SelectedDefault']][[length(Cat_EvalParameters[['CatBoost_LossFunction']][['SelectedDefault']]) + 1L]] <- input$CatBoost_LossFunction
    Cat_EvalParameters <<- Cat_EvalParameters

    # Evaluation Metric
    output$CatBoost_EvalMetric <- shiny::renderUI({
      out <- RemixAutoML:::CatBoostEvalMetricOptions(TT())
      print(out)
      print(TT())
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = Cat_EvalParameters, InputName = 'CatBoost_EvalMetric', ArgName = 'SelectedDefault', Default = out$Default, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='CatBoost_EvalMetric', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Evaluation Metric'), Choices = out$Choices, Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    Cat_EvalParameters[['CatBoost_EvalMetric']][['SelectedDefault']][[length(Cat_EvalParameters[['CatBoost_EvalMetric']][['SelectedDefault']]) + 1L]] <- input$CatBoost_EvalMetric
    Cat_EvalParameters <<- Cat_EvalParameters

    # Class Weights 0
    output$CatBoost_ClassWeights0 <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = Cat_EvalParameters, InputName = 'CatBoost_ClassWeights0', ArgName = 'SelectedDefault', Default = 1, Debug = Debug)
      RemixAutoML:::TextInput(InputID='CatBoost_ClassWeights0', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Class Weight for 0'), Value = '1', Placeholder = selected_default)
    })

    # Args Storage
    Cat_EvalParameters[['CatBoost_ClassWeights0']][['SelectedDefault']][[length(Cat_EvalParameters[['CatBoost_ClassWeights0']][['SelectedDefault']]) + 1L]] <- as.numeric(input$CatBoost_ClassWeights0)
    Cat_EvalParameters <<- Cat_EvalParameters

    # Class Weights 1
    output$CatBoost_ClassWeights1 <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = Cat_EvalParameters, InputName = 'CatBoost_ClassWeights1', ArgName = 'SelectedDefault', Default = '1', Debug = Debug)
      RemixAutoML:::TextInput(InputID='CatBoost_ClassWeights1', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Class Weight for 1'), Value = '1', Placeholder = selected_default)
    })

    # Args Storage
    Cat_EvalParameters[['CatBoost_ClassWeights1']][['SelectedDefault']][[length(Cat_EvalParameters[['CatBoost_ClassWeights1']][['SelectedDefault']]) + 1L]] <- as.numeric(input$CatBoost_ClassWeights1)
    Cat_EvalParameters <<- Cat_EvalParameters

    # Metric Periods
    output$CatBoost_MetricPeriods <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = Cat_EvalParameters, InputName = 'CatBoost_MetricPeriods', ArgName = 'SelectedDefault', Default = 10, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='CatBoost_MetricPeriods', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Trees between Evals'), Choices = c(1,seq(5,500,5)), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    Cat_EvalParameters[['CatBoost_MetricPeriods']][['SelectedDefault']][[length(Cat_EvalParameters[['CatBoost_MetricPeriods']][['SelectedDefault']]) + 1L]] <- input$CatBoost_MetricPeriods
    Cat_EvalParameters <<- Cat_EvalParameters
  })

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  #    Inputs XGBoost ML                ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  # XGBoost DropDown
  shiny::observeEvent(input$XGBoost, {

    # Create List
    if(!exists('XGBoost')) XGBoost <- list(Debug = Debug)
    print('XGBoost Target Type')

    # Target Type
    output$XGBoost_TargetType <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = XGBoost, InputName = 'XGBoost_TargetType', ArgName = 'SelectedDefault', Default = 'Regression', Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID = 'XGBoost_TargetType', Label = 'Select Supervised Learning Type', Choices = c('Regression','Binary Classification','MultiClass'), SelectedDefault = selected_default, Multiple = TRUE, MaxVars = 1, Debug = Debug)
    })

    # Create reactive
    TTx <- shiny::reactive({input$XGBoost_TargetType})
    TTx <<- TTx

    # Args Storage
    XGBoost[['XGBoost_TargetType']][['SelectedDefault']][[length(XGBoost[['XGBoost_TargetType']][['SelectedDefault']]) + 1L]] <- input$XGBoost_TargetType
    XGBoost <<- XGBoost
  })

  # XGBoost MetaData Parameters
  shiny::observeEvent(input$XGB_MetaData, {

    # Initialize List
    if(!exists('XGB_MetaData')) XGB_MetaData <- list(Debug = Debug)
    print('Metadata Parameters')

    # Number of cpu threads
    output$XGBoost_NThreads <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = XGB_MetaData, InputName = 'XGBoost_NThreads', ArgName = 'SelectedDefault', Default = max(1L, parallel::detectCores() - 2L), Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='XGBoost_NThreads', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'CPU Threads'), Choices = c(-1,1:128), Multiple = TRUE, MaxVars = 1, SelectedDefault = max(1L, parallel::detectCores() - 2L))
    })

    # Args Storage
    XGB_MetaData[['XGBoost_NThreads']][['SelectedDefault']][[length(XGB_MetaData[['XGBoost_NThreads']][['SelectedDefault']]) + 1L]] <- input$XGBoost_NThreads
    XGB_MetaData <<- XGB_MetaData

    # Number of GPUs
    output$XGBoost_NumGPUs <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = XGB_MetaData, InputName = 'XGBoost_NumGPUs', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='XGBoost_NumGPUs', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Number of GPUs'), Choices = c(1:12), Multiple = TRUE, MaxVars = 1, SelectedDefault = 1)
    })

    # Args Storage
    XGB_MetaData[['XGBoost_NumGPUs']][['SelectedDefault']][[length(XGB_MetaData[['XGBoost_NumGPUs']][['SelectedDefault']]) + 1L]] <- input$XGBoost_NumGPUs
    XGB_MetaData <<- XGB_MetaData

    # Train on Full Data
    output$XGBoost_TrainOnFull <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = XGB_MetaData, InputName = 'XGBoost_TrainOnFull', ArgName = 'SelectedDefault', Default = FALSE, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='XGBoost_TrainOnFull', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Train on Full Data'), Choices = c(FALSE, TRUE), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    XGB_MetaData[['XGBoost_TrainOnFull']][['SelectedDefault']][[length(XGB_MetaData[['XGBoost_TrainOnFull']][['SelectedDefault']]) + 1L]] <- input$XGBoost_TrainOnFull
    XGB_MetaData <<- XGB_MetaData

    # Model ID
    output$XGBoost_ModelID <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = XGB_MetaData, InputName = 'XGBoost_ModelID', ArgName = 'SelectedDefault', Default = 'Model1', Debug = Debug)
      RemixAutoML:::TextInput(InputID='XGBoost_ModelID', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Create Model ID'), Value = selected_default, Placeholder = 'Name your model')
    })

    # Args Storage
    XGB_MetaData[['XGBoost_ModelID']][['SelectedDefault']][[length(XGB_MetaData[['XGBoost_ModelID']][['SelectedDefault']]) + 1L]] <- input$XGBoost_ModelID
    XGB_MetaData <<- XGB_MetaData
  })

  # XGBoost Data Parameters
  shiny::observeEvent(input$XGB_DataParameters, {

    # Initialize List
    if(!exists('XGB_DataParameters')) XGB_DataParameters <- list(Debug = Debug)
    print('Data Parameters')

    # Data
    output$XGBoost_data <- shiny::renderUI({
      print('XGBoost Data Parameters 1')
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = XGB_DataParameters, InputName = 'XGBoost_data', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='XGBoost_data', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Select Training Data'), Choices = names(DataList), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    XGB_DataParameters[['XGBoost_data']][['SelectedDefault']][[length(XGB_DataParameters[['XGBoost_data']][['SelectedDefault']]) + 1L]] <- input$XGBoost_data
    XGB_DataParameters <<- XGB_DataParameters

    # Reactive
    XGB_dt <- shiny::reactive({tryCatch({DataList[[input$XGBoost_data]]}, error = function(x) DataList[[1L]])})

    # Validation Data
    output$XGBoost_ValidationData <- shiny::renderUI({
      print('XGBoost Data Parameters 2')
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = XGB_DataParameters, InputName = 'XGBoost_ValidationData', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='XGBoost_ValidationData', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Select Validation Data'), Choices = names(DataList), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    XGB_DataParameters[['XGBoost_ValidationData']][['SelectedDefault']][[length(XGB_DataParameters[['XGBoost_ValidationData']][['SelectedDefault']]) + 1L]] <- input$XGBoost_ValidationData
    XGB_DataParameters <<- XGB_DataParameters

    # Test Data
    output$XGBoost_TestData <- shiny::renderUI({
      print('XGBoost Data Parameters 3')
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = XGB_DataParameters, InputName = 'XGBoost_TestData', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='XGBoost_TestData', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Select Test Data'), Choices = names(DataList), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    XGB_DataParameters[['XGBoost_TestData']][['SelectedDefault']][[length(XGB_DataParameters[['XGBoost_TestData']][['SelectedDefault']]) + 1L]] <- input$XGBoost_TestData
    XGB_DataParameters <<- XGB_DataParameters

    # Target Column
    output$XGBoost_TargetColumnName <- shiny::renderUI({
      print('XGBoost Data Parameters 4')
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = XGB_DataParameters, InputName = 'XGBoost_TargetColumnName', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='XGBoost_TargetColumnName', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Select Target'), Choices = names(XGB_dt()), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    XGB_DataParameters[['XGBoost_TargetColumnName']][['SelectedDefault']][[length(XGB_DataParameters[['XGBoost_TargetColumnName']][['SelectedDefault']]) + 1L]] <- input$XGBoost_TargetColumnName
    XGB_DataParameters <<- XGB_DataParameters

    # Feature Columns
    output$XGBoost_FeatureColNames <- shiny::renderUI({
      print('XGBoost Data Parameters 5')
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = XGB_DataParameters, InputName = 'XGBoost_FeatureColNames', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='XGBoost_FeatureColNames', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Select Features'), Choices = names(XGB_dt()), Multiple = TRUE, MaxVars = 100, SelectedDefault = selected_default)
    })

    # Args Storage
    XGB_DataParameters[['XGBoost_FeatureColNames']][['SelectedDefault']][[length(XGB_DataParameters[['XGBoost_FeatureColNames']][['SelectedDefault']]) + 1L]] <- input$XGBoost_FeatureColNames
    XGB_DataParameters <<- XGB_DataParameters

    # Date Column
    output$XGBoost_PrimaryDateColumn <- shiny::renderUI({
      print('Adrian Antico 6')
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = XGB_DataParameters, InputName = 'XGBoost_PrimaryDateColumn', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='XGBoost_PrimaryDateColumn', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Select Date'), Choices = names(XGB_dt()), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    XGB_DataParameters[['XGBoost_PrimaryDateColumn']][['SelectedDefault']][[length(XGB_DataParameters[['XGBoost_PrimaryDateColumn']][['SelectedDefault']]) + 1L]] <- input$XGBoost_PrimaryDateColumn
    XGB_DataParameters <<- XGB_DataParameters

    # Date Column
    output$XGBoost_EncodeMethod <- shiny::renderUI({
      print('Adrian Antico 6')
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = XGB_DataParameters, InputName = 'XGBoost_EncodeMethod', ArgName = 'SelectedDefault', Default = 'credibility', Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='XGBoost_EncodeMethod', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Character Encoding'), Choices = c('credibility','binary','m_estimator','woe','target_encoding','poly_encode','backward_difference','helmert'), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    XGB_DataParameters[['XGBoost_EncodeMethod']][['SelectedDefault']][[length(XGB_DataParameters[['XGBoost_EncodeMethod']][['SelectedDefault']]) + 1L]] <- input$XGBoost_PrimaryDateColumn
    XGB_DataParameters <<- XGB_DataParameters


    # Weigths Column
    output$XGBoost_WeightsColumnName <- shiny::renderUI({
      print('XGBoost Data Parameters 7')
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = XGB_DataParameters, InputName = 'XGBoost_WeightsColumnName', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='XGBoost_WeightsColumnName', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Weights Column'), Choices = names(XGB_dt()), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    XGB_DataParameters[['XGBoost_WeightsColumnName']][['SelectedDefault']][[length(XGB_DataParameters[['XGBoost_WeightsColumnName']][['SelectedDefault']]) + 1L]] <- input$XGBoost_WeightsColumnName
    XGB_DataParameters <<- XGB_DataParameters

    # ID Columns
    output$XGBoost_IDcols <- shiny::renderUI({
      print('XGBoost Data Parameters 8')
      gg <- c(input$XGBoost_TargetColumnName, input$XGBoost_FeatureColNames, input$XGBoost_PrimaryDateColumn, input$XGBoost_WeightsColumnName)
      gg <- gg[!is.na(gg)]; hh <- tryCatch({names(XGB_dt())}, error = function(x) NULL); choices <- tryCatch({hh[!hh %in% gg]}, error = function(x) NULL)
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = XGB_DataParameters, InputName = 'XGBoost_IDcols', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='XGBoost_IDcols', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'ID Columns'), Choices = choices, Multiple = TRUE, MaxVars = 100, SelectedDefault = selected_default)
    })

    # Args Storage
    XGB_DataParameters[['XGBoost_IDcols']][['SelectedDefault']][[length(XGB_DataParameters[['XGBoost_IDcols']][['SelectedDefault']]) + 1L]] <- input$XGBoost_IDcols
    XGB_DataParameters <<- XGB_DataParameters

    # Transformation Columns
    output$XGBoost_TransformNumericColumns <- shiny::renderUI({
      print('XGBoost Data Parameters 10')
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = XGB_DataParameters, InputName = 'XGBoost_TransformNumericColumns', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='XGBoost_TransformNumericColumns', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Transform Columns'), Choices = names(XGB_dt()), Multiple = TRUE, MaxVars = 100, SelectedDefault = selected_default)
    })

    # Args Storage
    XGB_DataParameters[['XGBoost_TransformNumericColumns']][['SelectedDefault']][[length(XGB_DataParameters[['XGBoost_TransformNumericColumns']][['SelectedDefault']]) + 1L]] <- input$XGBoost_TransformNumericColumns
    XGB_DataParameters <<- XGB_DataParameters

    # Transformation Methods
    output$XGBoost_Methods <- shiny::renderUI({
      print('XGBoost Data Parameters 11')
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = XGB_DataParameters, InputName = 'XGBoost_Methods', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='XGBoost_Methods', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Transform Methods'), Choices = c('BoxCox', 'Asinh', 'Log', 'LogPlus1', 'Sqrt', 'Asin', 'Logit'), Multiple = TRUE, MaxVars = 100, SelectedDefault = selected_default)
    })

    # Args Storage
    XGB_DataParameters[['XGBoost_Methods']][['SelectedDefault']][[length(XGB_DataParameters[['XGBoost_Methods']][['SelectedDefault']]) + 1L]] <- input$XGBoost_Methods
    XGB_DataParameters <<- XGB_DataParameters
  })

  # XGBoost Grid Tuning Parameters
  shiny::observeEvent(input$XGB_GridTuningParameters, {

    # Initialize List
    if(!exists('XG_GridTuningParameters')) XGB_GridTuningParameters <- list(Debug = Debug)
    print('Grid Tuning Parameters')

    # Pass In Grid
    output$XGBoost_PassInGrid <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = XGB_MetaData, InputName = 'XGBoost_PassInGrid', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='XGBoost_PassInGrid', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Previous Grid Data'), Choices = names(DataList), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    XGB_MetaData[['XGBoost_PassInGrid']][['SelectedDefault']][[length(XGB_MetaData[['XGBoost_PassInGrid']][['SelectedDefault']]) + 1L]] <- input$XGBoost_PassInGrid
    XGB_MetaData <<- XGB_MetaData

    # Grid Tune
    output$XGBoost_GridTune <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = XGB_GridTuningParameters, InputName = 'XGBoost_GridTune', ArgName = 'SelectedDefault', Default = FALSE, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='XGBoost_GridTune', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Grid Tune'), Choices = c(FALSE, TRUE), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    XGB_GridTuningParameters[['XGBoost_GridTune']][['SelectedDefault']][[length(XGB_GridTuningParameters[['XGBoost_GridTune']][['SelectedDefault']]) + 1L]] <- input$XGBoost_GridTune
    XGB_GridTuningParameters <<- XGB_GridTuningParameters

    # grid eval metric
    output$XGBoost_grid_eval_metric <- shiny::renderUI({
      out <- RemixAutoML:::CatBoostGridEvalMetricsOptions(TTx())
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = XGB_GridTuningParameters, InputName = 'XGBoost_grid_eval_metric', ArgName = 'SelectedDefault', Default = out$Default, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='XGBoost_grid_eval_metric', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Evaluation Metric'), Choices = out$Choices, Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    XGB_GridTuningParameters[['XGBoost_grid_eval_metric']][['SelectedDefault']][[length(XGB_GridTuningParameters[['XGBoost_grid_eval_metric']][['SelectedDefault']]) + 1L]] <- input$XGBoost_grid_eval_metric
    XGB_GridTuningParameters <<- XGB_GridTuningParameters

    # Max Models
    output$XGBoost_MaxModelsInGrid <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = XGB_GridTuningParameters, InputName = 'XGBoost_MaxModelsInGrid', ArgName = 'SelectedDefault', Default = 25, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='XGBoost_MaxModelsInGrid', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Max Models'), Choices = c(seq(5,20,5),seq(25,2500,25)), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    XGB_GridTuningParameters[['XGBoost_MaxModelsInGrid']][['SelectedDefault']][[length(XGB_GridTuningParameters[['XGBoost_MaxModelsInGrid']][['SelectedDefault']]) + 1L]] <- input$XGBoost_MaxModelsInGrid
    XGB_GridTuningParameters <<- XGB_GridTuningParameters

    # Runs without a New Winner
    output$XGBoost_MaxRunsWithoutNewWinner <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = XGB_GridTuningParameters, InputName = 'XGBoost_MaxRunsWithoutNewWinner', ArgName = 'SelectedDefault', Default = 10, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='XGBoost_MaxRunsWithoutNewWinner', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Runs Without Winner'), Choices = c(seq(5,20,5),seq(25,2500,25)), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    XGB_GridTuningParameters[['XGBoost_MaxRunsWithoutNewWinner']][['SelectedDefault']][[length(XGB_GridTuningParameters[['XGBoost_MaxRunsWithoutNewWinner']][['SelectedDefault']]) + 1L]] <- input$XGBoost_MaxRunsWithoutNewWinner
    XGB_GridTuningParameters <<- XGB_GridTuningParameters

    # Max Runtime in Minutes
    output$XGBoost_MaxRunMinutes <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = XGB_GridTuningParameters, InputName = 'XGBoost_MaxRunMinutes', ArgName = 'SelectedDefault', Default = 30, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='XGBoost_MaxRunMinutes', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Max Run Time (mins)'), Choices = c(seq(30,60*24*7,30)), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    XGB_GridTuningParameters[['XGBoost_MaxRunMinutes']][['SelectedDefault']][[length(XGB_GridTuningParameters[['XGBoost_MaxRunMinutes']][['SelectedDefault']]) + 1L]] <- input$XGBoost_MaxRunMinutes
    XGB_GridTuningParameters <<- XGB_GridTuningParameters

    # Baseline Comparison
    output$XGBoost_BaselineComparison <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = XGB_GridTuningParameters, InputName = 'XGBoost_BaselineComparison', ArgName = 'SelectedDefault', Default = 'default', Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='XGBoost_BaselineComparison', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Evaluation Metric'), Choices = c('default','best'), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    XGB_GridTuningParameters[['XGBoost_BaselineComparison']][['SelectedDefault']][[length(XGB_GridTuningParameters[['XGBoost_BaselineComparison']][['SelectedDefault']]) + 1L]] <- input$XGBoost_BaselineComparison
    XGB_GridTuningParameters <<- XGB_GridTuningParameters
  })

  # XGBoost ML Parameters
  shiny::observeEvent(input$XGB_MLParameters, {

    # Initialize List
    if(!exists('XGB_MLParameters')) XGB_MLParameters <- list(Debug = Debug)
    print('ML Parameters')

    # Trees
    output$XGBoost_Trees <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = XGB_MLParameters, InputName = 'XGBoost_Trees', ArgName = 'SelectedDefault', Default = 1000, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='XGBoost_Trees', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Number of Trees'), Choices = c(5:50, seq(75,475,25), seq(500,9500,500), seq(10000, 25000, 1000)), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    XGB_MLParameters[['XGBoost_Trees']][['SelectedDefault']][[length(XGB_MLParameters[['XGBoost_Trees']][['SelectedDefault']]) + 1L]] <- input$XGBoost_Trees
    XGB_MLParameters <<- XGB_MLParameters

    # Depth
    output$XGBoost_max_depth <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = XGB_MLParameters, InputName = 'XGBoost_max_depth', ArgName = 'SelectedDefault', Default = 8, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='XGBoost_max_depth', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Max Tree Depth'), Choices = 4:20, Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    XGB_MLParameters[['XGBoost_max_depth']][['SelectedDefault']][[length(XGB_MLParameters[['XGBoost_max_depth']][['SelectedDefault']]) + 1L]] <- input$XGBoost_max_depth
    XGB_MLParameters <<- XGB_MLParameters

    # Learning Rate
    output$XGBoost_eta <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = XGB_MLParameters, InputName = 'XGBoost_eta', ArgName = 'SelectedDefault', Default = 0.10, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='XGBoost_eta', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Learning Rate'), Choices = seq(0.01,0.50,0.002), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    XGB_MLParameters[['XGBoost_eta']][['SelectedDefault']][[length(XGB_MLParameters[['XGBoost_eta']][['SelectedDefault']]) + 1L]] <- input$XGBoost_eta
    XGB_MLParameters <<- XGB_MLParameters

    # min_child_weight
    output$XGBoost_min_child_weight <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = XGB_MLParameters, InputName = 'XGBoost_min_child_weight', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='XGBoost_min_child_weight', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Min Child Weight'), Choices = 0:50, Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    XGB_MLParameters[['XGBoost_min_child_weight']][['SelectedDefault']][[length(XGB_MLParameters[['XGBoost_min_child_weight']][['SelectedDefault']]) + 1L]] <- input$XGBoost_min_child_weight
    XGB_MLParameters <<- XGB_MLParameters

    # subsample
    output$XGBoost_subsample <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = XGB_MLParameters, InputName = 'XGBoost_subsample', ArgName = 'SelectedDefault', Default = 1, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='XGBoost_subsample', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Subsample'), Choices = seq(0.05,1,0.05), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    XGB_MLParameters[['XGBoost_subsample']][['SelectedDefault']][[length(XGB_MLParameters[['XGBoost_subsample']][['SelectedDefault']]) + 1L]] <- input$XGBoost_subsample
    XGB_MLParameters <<- XGB_MLParameters

    # colsample by tree
    output$XGBoost_colsample_bytree <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = XGB_MLParameters, InputName = 'XGBoost_colsample_bytree', ArgName = 'SelectedDefault', Default = 1, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='XGBoost_colsample_bytree', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Colsample Rate'), Choices = seq(0.05,1,0.05), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    XGB_MLParameters[['XGBoost_colsample_bytree']][['SelectedDefault']][[length(XGB_MLParameters[['XGBoost_colsample_bytree']][['SelectedDefault']]) + 1L]] <- input$XGBoost_colsample_bytree
    XGB_MLParameters <<- XGB_MLParameters
  })

  # XGBoost Eval Parameters
  shiny::observeEvent(input$XGB_EvalParameters, {

    # Initialize List
    if(!exists('XGB_EvalParameters')) XGB_EvalParameters <- list(Debug = Debug)
    print('Evaluation Parameters')

    # Loss Function
    output$XGBoost_LossFunction <- shiny::renderUI({
      out <- RemixAutoML:::XGBoostLossFunctionOptions(TTx())
      print(out)
      print(TTx())
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = XGB_EvalParameters, InputName = 'XGBoost_LossFunction', ArgName = 'SelectedDefault', Default = out$Default, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='XGBoost_LossFunction', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Loss Function'), Choices = out$Choices, Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    XGB_EvalParameters[['XGBoost_LossFunction']][['SelectedDefault']][[length(XGB_EvalParameters[['XGBoost_LossFunction']][['SelectedDefault']]) + 1L]] <- input$XGBoost_LossFunction
    XGB_EvalParameters <<- XGB_EvalParameters

    # Evaluation Metric
    output$XGBoost_EvalMetric <- shiny::renderUI({
      out <- RemixAutoML:::XGBoostEvalMetricOptions(TTx())
      print(out)
      print(TTx())
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = XGB_EvalParameters, InputName = 'XGBoost_EvalMetric', ArgName = 'SelectedDefault', Default = out$Default, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='XGBoost_EvalMetric', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Evaluation Metric'), Choices = out$Choices, Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    XGB_EvalParameters[['XGBoost_EvalMetric']][['SelectedDefault']][[length(XGB_EvalParameters[['XGBoost_EvalMetric']][['SelectedDefault']]) + 1L]] <- input$XGBoost_EvalMetric
    XGB_EvalParameters <<- XGB_EvalParameters
  })

  # ----

  # ----


  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  #    Inputs Plotting                   ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  # Dragula, globals
  shiny::observeEvent(input$sidebar, {
    if(input$sidebar == 'Plotter') {
      output$PlotEngine <- shiny::renderUI({
        shiny::checkboxGroupInput(inputId = "PlotEngine", label = tags$span(style='color: blue;', 'Plot Engine'),choices = list("plotly" = 1, "ggplot2" = 2), selected = 1)
      })
      output$AutoGridHorizontal <-  shiny::renderUI({
        shiny::checkboxGroupInput(inputId = "AutoGridHorizontal", label = tags$span(style='color: blue;', 'Auto Grid Scale'),choices = list("On" = 1, "Off" = 2), selected = 1)
      })
      output$PlotTypeDragula <- shiny::renderUI({
        dragheight <- '40px'
        tags$style("#display {background: #2d2d2d; border: 10px solid #000000; border-radius: 5px; font-size: 2em; color: white; height: 100px; min-width:200px; text-align: center; padding: 1em; display:table-cell; vertical-align:middle; } #drag-elements { display: block; background-color: #dfdfdf; border-radius: 5px; min-height: 50px; margin: 0 auto; padding: 2em; } #drag-elements > div { text-align: center; float: left; padding: 1em; margin: 0 1em 1em 0; box-shadow: 1px 1px 1px rgba(0, 0, 0, 0.3); border-radius: 100px; border: 2px solid #ececec; background: #F7F7F7; transition: all .5s ease; } #drag-elements > div:active { -webkit-animation: wiggle 0.3s 0s infinite ease-in; animation: wiggle 0.3s 0s infinite ease-in; opacity: .6; border: 2px solid #000; } #drag-elements > div:hover { border: 2px solid gray; background-color: #e5e5e5; } #drop-target { border: 2px dashed #D9D9D9; border-radius: 5px; min-height: 50px; margin: 0 auto; margin-top: 10px; padding: 2em; display: block; text-align: center; } #drop-target > div { transition: all .5s; text-align: center; float: left; padding: 1em; margin: 0 1em 1em 0; box-shadow: 1px 1px 1px rgba(0, 0, 0, 0.3); border-radius: 5px; border: 2px solid skyblue; background: #F7F7F7; transition: all .5s ease; } #drop-target > div:active { -webkit-animation: wiggle 0.3s 0s infinite ease-in; animation: wiggle 0.3s 0s infinite ease-in; opacity: .6; border: 2px solid #000; }")
        esquisse::dragulaInput(
          height = dragheight,
          replace = FALSE,
          inputId = 'PlotTypeDragula',
          label = 'Drop Plot Boxes into Grid',
          sourceLabel = 'Plots',
          targetsLabels = c('TopLeft','LowerLeft','TopRight','LowerRight'),
          boxStyle = shiny::tags$style(HTML(
            "
        /* drag source */
        #container-drag-source {
        	position: relative;
        	padding: 5px 0 5px 0;
        	border-color: #cfcef0;
        	border-radius: 5px;
        	border-style: solid;
        	border-width: 10px;
        	overflow: auto;
        	overflow-x: hidden;
        	font-size: 12px;
        }
        ")),
          choices = c('Plot1','Plot2','Plot3','Plot4')
          #,
          # choiceNames = , choiceValues = , selected = , status = , replace = , copySource = , badge = , ncolSource = , ncolGrid = , dragulaOpts = , boxStyle = , width = "100%", height = "100%"
        )
      })

      output$NumberGroupsDisplay1 <- shiny::renderUI({
        RemixAutoML:::NumericInput(InputID = 'NumberGroupsDisplay1', Label = tags$span(style='color: blue;', 'Dispay N Levels'), Step = 1L, Value = 5L, Min = 1L, Max = 100L)
      })
      output$NumberGroupsDisplay2 <- shiny::renderUI({
        RemixAutoML:::NumericInput(InputID = 'NumberGroupsDisplay2', Label = tags$span(style='color: blue;', 'Dispay N Levels'), Step = 1L, Value = 5L, Min = 1L, Max = 100L)
      })
      output$NumberGroupsDisplay3 <- shiny::renderUI({
        RemixAutoML:::NumericInput(InputID = 'NumberGroupsDisplay3', Label = tags$span(style='color: blue;', 'Dispay N Levels'), Step = 1L, Value = 5L, Min = 1L, Max = 100L)
      })
      output$NumberGroupsDisplay4 <- shiny::renderUI({
        RemixAutoML:::NumericInput(InputID = 'NumberGroupsDisplay4', Label = tags$span(style='color: blue;', 'Dispay N Levels'), Step = 1L, Value = 5L, Min = 1L, Max = 100L)
      })
      output$PlotWidth <- shiny::renderUI({
        RemixAutoML:::NumericInput(InputID = "PlotWidth", Label=tags$span(style='color: blue;', 'Plot Width'), Step = 50, Min = 500, Max = 3500, Value = 1600)
      })
      output$PlotHeight <- shiny::renderUI({
        RemixAutoML:::NumericInput(InputID = "PlotHeight", Label=tags$span(style='color: blue;', 'Plot Height'), Step = 25, Min = 300, Max = 3500, Value = 500)
      })

      # Output blank grapth is list is empty
      output$TrendPlotly <- plotly::renderPlotly({
        plotly::ggplotly(RemixAutoML:::BlankPlot())
      })
    }
  })

  # Plot 1 DropDown
  shiny::observeEvent(input$PlotDropDown1, {

    # Create intra-session tracking list
    if(!exists('PlotDropDown1')) PlotDropDown1 <- list(Debug = Debug)
    print('Plot1_SelectData')

    # Select data
    output$Plot1_SelectData <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown1, InputName = 'Plot1_SelectData', ArgName = 'SelectedDefault', Default = names(DataList)[[1L]], Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='Plot1_SelectData', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Choose data set'), Choices = names(DataList), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    PlotDropDown1[['Plot1_SelectData']][['SelectedDefault']][[length(PlotDropDown1[['Plot1_SelectData']][['SelectedDefault']]) + 1L]] <- input$Plot1_SelectData
    PlotDropDown1 <<- PlotDropDown1

    # Reactives
    dt1 <- shiny::reactive({shiny::req(tryCatch({DataList[[input$Plot1_SelectData]]}, error = function(x) DataList[[1L]]))})
    dt1 <<- dt1

    # Plot Selection + reactives for enabling smart selection for YVar, XVar, etc.
    output$Plot1 <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown1, InputName = 'Plot1', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID = 'Plot1', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Plot Type Selection'), Choices = c(AvailablePlots), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    PlotDropDown1[['Plot1']][['SelectedDefault']][[length(PlotDropDown1[['Plot1']][['SelectedDefault']]) + 1L]] <- input$Plot1
    PlotDropDown1 <<- PlotDropDown1

    # Reactives
    Plot1_react <- shiny::reactive({input[['Plot1']]})

    # Y Variable
    output$YVar1 <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown1, InputName = 'YVar1', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      if('CorrelationMatrix' %in% tryCatch({Plot1_react()}, error = 'none')) {
        choices <- names(dt1())[which(RemixAutoML:::ColTypes(dt1()) %in% c('numeric','integer'))]
        MaxVars <- 30
        if(length(choices) == 0) choices <- NULL
      } else {
        choices <- names(dt1())
        MaxVars <- 1
      }
      RemixAutoML:::SelectizeInput(InputID = 'YVar1', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Y-Variable'), Choices = choices, Multiple = TRUE, MaxVars = MaxVars, SelectedDefault = selected_default)
    })

    # Args Storage
    PlotDropDown1[['YVar1']][['SelectedDefault']][[length(PlotDropDown1[['YVar1']][['SelectedDefault']]) + 1L]] <- input$YVar1
    PlotDropDown1 <<- PlotDropDown1

    # X Variable
    output$XVar1 <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown1, InputName = 'XVar1', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      if('CorrelationMatrix' %in% tryCatch({Plot1_react()}, error = 'none')) {
        MaxVars <- 100
        choices <- NULL
      } else {
        MaxVars <- 1
        choices <- names(dt1())
      }
      RemixAutoML:::SelectizeInput(InputID = 'XVar1', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'X-Variable'), Choices = choices, Multiple = TRUE, MaxVars = MaxVars, SelectedDefault = selected_default)
    })

    # Args Storage
    PlotDropDown1[['XVar1']][['SelectedDefault']][[length(PlotDropDown1[['XVar1']][['SelectedDefault']]) + 1L]] <- input$XVar1
    PlotDropDown1 <<- PlotDropDown1

    # Z Variable
    output$ZVar1 <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown1, InputName = 'ZVar1', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      if('HeatMapPlot' %in% tryCatch({Plot1_react()}, error = 'none')) {
        MaxVars <- 1
        choices <- names(dt1())
      } else {
        MaxVars <- 0
        choices <- NULL
      }
      RemixAutoML:::SelectizeInput(InputID = 'ZVar1', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Z-Variable'), Choices = choices, Multiple = TRUE, MaxVars = MaxVars, SelectedDefault = selected_default)
    })

    # Args Storage
    PlotDropDown1[['ZVar1']][['SelectedDefault']][[length(PlotDropDown1[['ZVar1']][['SelectedDefault']]) + 1L]] <- input$ZVar1
    PlotDropDown1 <<- PlotDropDown1

    # Reactives
    YVar1 <- shiny::reactive({shiny::req(input[['YVar1']])})
    YVar1 <<- YVar1
    XVar1 <- shiny::reactive({shiny::req(input[['XVar1']])})
    XVar1 <<- XVar1

    # Select GroupVars
    output$GroupVars1 <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown1, InputName = 'GroupVars1', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      if('CorrelationMatrix' %in% tryCatch({Plot1_react()}, error = 'none')) {
        choices <- names(dt1())[which(RemixAutoML:::ColTypes(dt1()) %in% c('numeric','integer'))]
        if(length(choices) == 0) choices <- NULL
      } else {
        choices <- names(dt1())
      }
      RemixAutoML:::SelectizeInput(InputID='GroupVars1', Label=tags$span(style='color: blue;', 'Select Group Variables'), Choices=choices, Multiple=TRUE, MaxVars = 3, SelectedDefault = selected_default)
    })

    # Args Storage
    PlotDropDown1[['GroupVars1']][['SelectedDefault']][[length(PlotDropDown1[['GroupVars1']][['SelectedDefault']]) + 1L]] <- input$GroupVars1
    PlotDropDown1 <<- PlotDropDown1

    # Reactives
    SelectedGroups1 <- shiny::reactive({RemixAutoML:::ReturnParam(xx = input[['GroupVars1']], VarName = 'GroupVars1', Default = NULL, Switch = TRUE, Type = 'character')})

    # Group Levels 1
    output$Levels_1_1 <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown1, InputName = 'Levels_1_1', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      sgs <- RemixAutoML:::LevelValues(SelectedGroups1()); if(Debug) {print('PickerInput_GetLevels 1'); print(sgs)}
      if(Debug) {print('Levels_1_1 logic check for data'); print(length(sgs) != 0 && length(dt1()) != 0 && sgs[1L] %in% names(dt1()))}
      if(length(sgs) != 0 && length(dt1()) != 0 && sgs[1L] %in% names(dt1())) {
        if(Debug) print('here for data')
        RemixAutoML:::PickerInput_GetLevels2(DataExist=exists('dt1'), InputID='Levels_1_1', InputID2=sgs, Choices=RemixAutoML:::UniqueLevels(input, dt1(), 1L, GroupVars=sgs), NumGroupVar=1L, Multiple=TRUE, SelectedDefault=selected_default)
      } else {
        RemixAutoML:::PickerInput_GetLevels2(DataExist=exists('aaasdfasdfsdf'), InputID='Levels_1_1', InputID2=NULL, Choices=NULL, NumGroupVar=1L, Multiple=TRUE, SelectedDefault=selected_default)
      }
    })

    # Args Storage
    PlotDropDown1[['Levels_1_1']][['SelectedDefault']][[length(PlotDropDown1[['Levels_1_1']][['SelectedDefault']]) + 1L]] <- input$Levels_1_1
    PlotDropDown1 <<- PlotDropDown1

    # Group Levels 2
    output$Levels_1_2 <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown1, InputName = 'Levels_1_2', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      sgs <- RemixAutoML:::LevelValues(SelectedGroups1()); if(Debug) {print('PickerInput_GetLevels 2'); print(sgs)}
      if(length(sgs) != 0 && length(dt1()) != 0 && sgs[1L] %in% names(dt1())) {
        RemixAutoML:::PickerInput_GetLevels2(DataExist=exists('dt1'), InputID='Levels_1_2', InputID2=sgs, Choices=RemixAutoML:::UniqueLevels(input, dt1(), 2L, GroupVars=sgs), NumGroupVar=2L, Multiple=TRUE, SelectedDefault=selected_default)
      } else {
        RemixAutoML:::PickerInput_GetLevels2(DataExist=exists('aaasdfasdfsdf'), InputID='Levels_1_2', InputID2=NULL, Choices=NULL, NumGroupVar=2L, Multiple=TRUE, SelectedDefault=selected_default)
      }
    })

    # Args Storage
    PlotDropDown1[['Levels_1_2']][['SelectedDefault']][[length(PlotDropDown1[['Levels_1_2']][['SelectedDefault']]) + 1L]] <- input$Levels_1_2
    PlotDropDown1 <<- PlotDropDown1

    # Group Levels 3
    output$Levels_1_3 <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown1, InputName = 'Levels_1_3', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      sgs <- RemixAutoML:::LevelValues(SelectedGroups1()); if(Debug) {print('PickerInput_GetLevels 3'); print(sgs)}
      if(length(sgs) != 0 && length(dt1()) != 0 && sgs[1L] %in% names(dt1())) {
        RemixAutoML:::PickerInput_GetLevels2(DataExist=exists('data'), InputID='Levels_1_3', InputID2=sgs, Choices=RemixAutoML:::UniqueLevels(input, dt1(), 3L, GroupVars=sgs), NumGroupVar=3L, Multiple=TRUE, SelectedDefault=selected_default)
      } else {
        RemixAutoML:::PickerInput_GetLevels2(DataExist=exists('aaasdfasdfsdf'), InputID='Levels_1_3', InputID2=NULL, Choices=NULL, NumGroupVar=3L, Multiple=TRUE, SelectedDefault=selected_default)
      }
    })

    # Args Storage
    PlotDropDown1[['Levels_1_3']][['SelectedDefault']][[length(PlotDropDown1[['Levels_1_3']][['SelectedDefault']]) + 1L]] <- input$Levels_1_3
    PlotDropDown1 <<- PlotDropDown1

    # Facet Var 1
    output$FacetVar_1_1 <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown1, InputName = 'FacetVar_1_1', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='FacetVar_1_1', Label = tags$span(style='color: blue;', 'Facet Variable 1'), Choices = names(dt1()), Multiple = TRUE, MaxVars = 1)
    })

    # Args Storage
    PlotDropDown1[['FacetVar_1_1']][['SelectedDefault']][[length(PlotDropDown1[['FacetVar_1_1']][['SelectedDefault']]) + 1L]] <- input$FacetVar_1_1
    PlotDropDown1 <<- PlotDropDown1

    # Facet Var 2
    output$FacetVar_1_2 <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown1, InputName = 'FacetVar_1_2', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='FacetVar_1_2', Label = tags$span(style='color: blue;', 'Facet Variable 2'), Choices = names(dt1()), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    PlotDropDown1[['FacetVar_1_2']][['SelectedDefault']][[length(PlotDropDown1[['FacetVar_1_2']][['SelectedDefault']]) + 1L]] <- input$FacetVar_1_2
    PlotDropDown1 <<- PlotDropDown1

    # Size Var 1
    output$SizeVar1 <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown1, InputName = 'SizeVar1', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID = 'SizeVar1', Label = tags$span(style='color: blue;', 'Size Variable'), Choices = names(dt1()), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    PlotDropDown1[['SizeVar1']][['SelectedDefault']][[length(PlotDropDown1[['SizeVar1']][['SelectedDefault']]) + 1L]] <- input$SizeVar1
    PlotDropDown1 <<- PlotDropDown1

    # Bar Plot Aggregation Method
    output$BarPlotAgg1 <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown1, InputName = 'BarPlotAgg1', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID = 'BarPlotAgg1', Label = tags$span(style='color: blue;', 'Aggregate Method'), Choices = c('sum','mean','median','sd'), Multiple = FALSE, MaxVars = 1, CloseAfterSelect = TRUE, Debug = FALSE, SelectedDefault = selected_default)
    })

    # Args Storage
    PlotDropDown1[['BarPlotAgg1']][['SelectedDefault']][[length(PlotDropDown1[['BarPlotAgg1']][['SelectedDefault']]) + 1L]] <- input$BarPlotAgg1
    PlotDropDown1 <<- PlotDropDown1

    # MultiClass Level Selection for PDP Model Insight Plot
    output$TargetLevel1 <- shiny::renderUI({
      if(length(YVar1()) != 0L && length(dt1()) != 0) {
        if(!any(class(dt1()[[YVar1()[[1L]]]]) %in% c('numeric','integer'))) vals <- as.character(unique(dt1()[[YVar1()]])) else vals <- NULL
      } else {
        vals <- NULL
      }
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown1, InputName = 'TargetLevel1', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID = 'TargetLevel1', Label = tags$span(style='color: blue;', 'Target Level'), Choices = vals, Multiple = FALSE, MaxVars = 1, CloseAfterSelect = TRUE, Debug = FALSE, SelectedDefault = selected_default)
    })

    # Args Storage
    PlotDropDown1[['TargetLevel1']][['SelectedDefault']][[length(PlotDropDown1[['TargetLevel1']][['SelectedDefault']]) + 1L]] <- input$TargetLevel1
    PlotDropDown1 <<- PlotDropDown1

  })

  # Plot 2 DropDown
  shiny::observeEvent(input$PlotDropDown2, {


    # Create intra-session tracking list
    if(!exists('PlotDropDown2')) PlotDropDown2 <- list(Debug = Debug)
    print('Plot2_SelectData')

    # Select data
    output$Plot2_SelectData <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown2, InputName = 'Plot2_SelectData', ArgName = 'SelectedDefault', Default = names(DataList)[[1L]], Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='Plot2_SelectData', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Choose data set'), Choices = names(DataList), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    PlotDropDown2[['Plot2_SelectData']][['SelectedDefault']][[length(PlotDropDown2[['Plot2_SelectData']][['SelectedDefault']]) + 1L]] <- input$Plot2_SelectData
    PlotDropDown2 <<- PlotDropDown2

    # Reactives
    dt2 <- shiny::reactive({shiny::req(tryCatch({DataList[[input$Plot2_SelectData]]}, error = function(x) DataList[[1L]]))})
    dt2 <<- dt2

    # Plot Selection + reactives for enabling smart selection for YVar, XVar, etc.
    output$Plot2 <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown2, InputName = 'Plot2', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID = 'Plot2', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Plot Type Selection'), Choices = c(AvailablePlots), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    PlotDropDown2[['Plot2']][['SelectedDefault']][[length(PlotDropDown2[['Plot2']][['SelectedDefault']]) + 1L]] <- input$Plot2
    PlotDropDown2 <<- PlotDropDown2

    # Reactives
    Plot2_react <- shiny::reactive({input[['Plot2']]})

    # Y Variable
    output$YVar2 <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown2, InputName = 'YVar2', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      if('CorrelationMatrix' %in% tryCatch({Plot2_react()}, error = 'none')) {
        choices <- names(dt2())[which(RemixAutoML:::ColTypes(dt2()) %in% c('numeric','integer'))]
        MaxVars <- 30
        if(length(choices) == 0) choices <- NULL
      } else {
        choices <- names(dt2())
        MaxVars <- 1
      }
      RemixAutoML:::SelectizeInput(InputID = 'YVar2', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Y-Variable'), Choices = choices, Multiple = TRUE, MaxVars = MaxVars, SelectedDefault = selected_default)
    })

    # Args Storage
    PlotDropDown2[['YVar2']][['SelectedDefault']][[length(PlotDropDown2[['YVar2']][['SelectedDefault']]) + 1L]] <- input$YVar2
    PlotDropDown2 <<- PlotDropDown2

    # X Variable
    output$XVar2 <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown2, InputName = 'XVar2', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      if('CorrelationMatrix' %in% tryCatch({Plot2_react()}, error = 'none')) {
        MaxVars <- 100
        choices <- NULL
      } else {
        MaxVars <- 1
        choices <- names(dt2())
      }
      RemixAutoML:::SelectizeInput(InputID = 'XVar2', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'X-Variable'), Choices = choices, Multiple = TRUE, MaxVars = MaxVars, SelectedDefault = selected_default)
    })

    # Args Storage
    PlotDropDown2[['XVar2']][['SelectedDefault']][[length(PlotDropDown2[['XVar2']][['SelectedDefault']]) + 1L]] <- input$XVar2
    PlotDropDown2 <<- PlotDropDown2

    # Z Variable
    output$ZVar2 <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown2, InputName = 'ZVar2', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      if('HeatMapPlot' %in% tryCatch({Plot2_react()}, error = 'none')) {
        MaxVars <- 1
        choices <- names(dt2())
      } else {
        MaxVars <- 0
        choices <- NULL
      }
      RemixAutoML:::SelectizeInput(InputID = 'ZVar2', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Z-Variable'), Choices = choices, Multiple = TRUE, MaxVars = MaxVars, SelectedDefault = selected_default)
    })

    # Args Storage
    PlotDropDown2[['ZVar2']][['SelectedDefault']][[length(PlotDropDown2[['ZVar2']][['SelectedDefault']]) + 1L]] <- input$ZVar2
    PlotDropDown2 <<- PlotDropDown2

    # Reactives
    YVar2 <- shiny::reactive({shiny::req(input[['YVar2']])})
    YVar2 <<- YVar2
    XVar2 <- shiny::reactive({shiny::req(input[['XVar2']])})
    XVar2 <<- XVar2

    # Select GroupVars
    output$GroupVars2 <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown2, InputName = 'GroupVars2', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      if('CorrelationMatrix' %in% tryCatch({Plot2_react()}, error = 'none')) {
        choices <- names(dt2())[which(RemixAutoML:::ColTypes(dt2()) %in% c('numeric','integer'))]
        if(length(choices) == 0) choices <- NULL
      } else {
        choices <- names(dt2())
      }
      RemixAutoML:::SelectizeInput(InputID='GroupVars2', Label=tags$span(style='color: blue;', 'Select Group Variables'), Choices=choices, Multiple=TRUE, MaxVars = 3, SelectedDefault = selected_default)
    })

    # Args Storage
    PlotDropDown2[['GroupVars2']][['SelectedDefault']][[length(PlotDropDown2[['GroupVars2']][['SelectedDefault']]) + 1L]] <- input$GroupVars2
    PlotDropDown2 <<- PlotDropDown2

    # Reactives
    SelectedGroups2 <- shiny::reactive({RemixAutoML:::ReturnParam(xx = input[['GroupVars2']], VarName = 'GroupVars2', Default = NULL, Switch = TRUE, Type = 'character')})

    # Group Levels 2
    output$Levels_2_1 <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown2, InputName = 'Levels_2_1', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      sgs <- RemixAutoML:::LevelValues(SelectedGroups2()); if(Debug) {print('PickerInput_GetLevels 1'); print(sgs)}
      if(Debug) {print('Levels_2_1 logic check for data'); print(length(sgs) != 0 && length(dt2()) != 0 && sgs[1L] %in% names(dt2()))}
      if(length(sgs) != 0 && length(dt2()) != 0 && sgs[1L] %in% names(dt2())) {
        if(Debug) print('here for data')
        RemixAutoML:::PickerInput_GetLevels2(DataExist=exists('dt2'), InputID='Levels_2_1', InputID2=sgs, Choices=RemixAutoML:::UniqueLevels(input, dt2(), 1L, GroupVars=sgs), NumGroupVar=1L, Multiple=TRUE, SelectedDefault=selected_default)
      } else {
        RemixAutoML:::PickerInput_GetLevels2(DataExist=exists('aaasdfasdfsdf'), InputID='Levels_2_1', InputID2=NULL, Choices=NULL, NumGroupVar=1L, Multiple=TRUE, SelectedDefault=selected_default)
      }
    })

    # Args Storage
    PlotDropDown2[['Levels_2_1']][['SelectedDefault']][[length(PlotDropDown2[['Levels_2_1']][['SelectedDefault']]) + 1L]] <- input$Levels_2_1
    PlotDropDown2 <<- PlotDropDown2

    # Group Levels 2
    output$Levels_2_2 <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown2, InputName = 'Levels_2_2', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      sgs <- RemixAutoML:::LevelValues(SelectedGroups2()); if(Debug) {print('PickerInput_GetLevels 2'); print(sgs)}
      if(length(sgs) != 0 && length(dt2()) != 0 && sgs[1L] %in% names(dt2())) {
        RemixAutoML:::PickerInput_GetLevels2(DataExist=exists('dt2'), InputID='Levels_2_2', InputID2=sgs, Choices=RemixAutoML:::UniqueLevels(input, dt2(), 2L, GroupVars=sgs), NumGroupVar=2L, Multiple=TRUE, SelectedDefault=selected_default)
      } else {
        RemixAutoML:::PickerInput_GetLevels2(DataExist=exists('aaasdfasdfsdf'), InputID='Levels_2_2', InputID2=NULL, Choices=NULL, NumGroupVar=2L, Multiple=TRUE, SelectedDefault=selected_default)
      }
    })

    # Args Storage
    PlotDropDown2[['Levels_2_2']][['SelectedDefault']][[length(PlotDropDown2[['Levels_2_2']][['SelectedDefault']]) + 1L]] <- input$Levels_2_2
    PlotDropDown2 <<- PlotDropDown2

    # Group Levels 3
    output$Levels_2_3 <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown2, InputName = 'Levels_2_3', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      sgs <- RemixAutoML:::LevelValues(SelectedGroups2()); if(Debug) {print('PickerInput_GetLevels 3'); print(sgs)}
      if(length(sgs) != 0 && length(dt2()) != 0 && sgs[1L] %in% names(dt2())) {
        RemixAutoML:::PickerInput_GetLevels2(DataExist=exists('data'), InputID='Levels_2_3', InputID2=sgs, Choices=RemixAutoML:::UniqueLevels(input, dt2(), 3L, GroupVars=sgs), NumGroupVar=3L, Multiple=TRUE, SelectedDefault=selected_default)
      } else {
        RemixAutoML:::PickerInput_GetLevels2(DataExist=exists('aaasdfasdfsdf'), InputID='Levels_2_3', InputID2=NULL, Choices=NULL, NumGroupVar=3L, Multiple=TRUE, SelectedDefault=selected_default)
      }
    })

    # Args Storage
    PlotDropDown2[['Levels_2_3']][['SelectedDefault']][[length(PlotDropDown2[['Levels_2_3']][['SelectedDefault']]) + 1L]] <- input$Levels_2_3
    PlotDropDown2 <<- PlotDropDown2

    # Facet Var 1
    output$FacetVar_2_1 <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown2, InputName = 'FacetVar_2_1', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='FacetVar_2_1', Label = tags$span(style='color: blue;', 'Facet Variable 1'), Choices = names(dt2()), Multiple = TRUE, MaxVars = 1)
    })

    # Args Storage
    PlotDropDown2[['FacetVar_2_1']][['SelectedDefault']][[length(PlotDropDown2[['FacetVar_2_1']][['SelectedDefault']]) + 1L]] <- input$FacetVar_2_1
    PlotDropDown2 <<- PlotDropDown2

    # Facet Var 2
    output$FacetVar_2_2 <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown2, InputName = 'FacetVar_2_2', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='FacetVar_2_2', Label = tags$span(style='color: blue;', 'Facet Variable 2'), Choices = names(dt2()), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    PlotDropDown2[['FacetVar_2_2']][['SelectedDefault']][[length(PlotDropDown2[['FacetVar_2_2']][['SelectedDefault']]) + 1L]] <- input$FacetVar_2_2
    PlotDropDown2 <<- PlotDropDown2

    # Size Var 2
    output$SizeVar2 <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown2, InputName = 'SizeVar2', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID = 'SizeVar2', Label = tags$span(style='color: blue;', 'Size Variable'), Choices = names(dt2()), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    PlotDropDown2[['SizeVar2']][['SelectedDefault']][[length(PlotDropDown2[['SizeVar2']][['SelectedDefault']]) + 1L]] <- input$SizeVar2
    PlotDropDown2 <<- PlotDropDown2

    # Bar Plot Aggregation Method
    output$BarPlotAgg2 <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown2, InputName = 'BarPlotAgg2', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID = 'BarPlotAgg2', Label = tags$span(style='color: blue;', 'Aggregate Method'), Choices = c('sum','mean','median','sd'), Multiple = FALSE, MaxVars = 1, CloseAfterSelect = TRUE, Debug = FALSE, SelectedDefault = selected_default)
    })

    # Args Storage
    PlotDropDown2[['BarPlotAgg2']][['SelectedDefault']][[length(PlotDropDown2[['BarPlotAgg2']][['SelectedDefault']]) + 1L]] <- input$BarPlotAgg2
    PlotDropDown2 <<- PlotDropDown2

    # MultiClass Level Selection for PDP Model Insight Plot
    output$TargetLevel2 <- shiny::renderUI({
      if(length(YVar2()) != 0L && length(dt2()) != 0) {
        if(!any(class(dt2()[[YVar2()[[1L]]]]) %in% c('numeric','integer'))) vals <- as.character(unique(dt2()[[YVar2()]])) else vals <- NULL
      } else {
        vals <- NULL
      }
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown2, InputName = 'TargetLevel2', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID = 'TargetLevel2', Label = tags$span(style='color: blue;', 'Target Level'), Choices = vals, Multiple = FALSE, MaxVars = 1, CloseAfterSelect = TRUE, Debug = FALSE, SelectedDefault = selected_default)
    })

    # Args Storage
    PlotDropDown2[['TargetLevel2']][['SelectedDefault']][[length(PlotDropDown2[['TargetLevel2']][['SelectedDefault']]) + 1L]] <- input$TargetLevel2
    PlotDropDown2 <<- PlotDropDown2

  })

  # Plot 3 DropDown
  shiny::observeEvent(input$PlotDropDown3, {


    # Create intra-session tracking list
    if(!exists('PlotDropDown3')) PlotDropDown3 <- list(Debug = Debug)
    print('Plot3_SelectData')

    # Select data
    output$Plot3_SelectData <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown3, InputName = 'Plot3_SelectData', ArgName = 'SelectedDefault', Default = names(DataList)[[1L]], Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='Plot3_SelectData', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Choose data set'), Choices = names(DataList), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    PlotDropDown3[['Plot3_SelectData']][['SelectedDefault']][[length(PlotDropDown3[['Plot3_SelectData']][['SelectedDefault']]) + 1L]] <- input$Plot3_SelectData
    PlotDropDown3 <<- PlotDropDown3

    # Reactives
    dt3 <- shiny::reactive({shiny::req(tryCatch({DataList[[input$Plot3_SelectData]]}, error = function(x) DataList[[1L]]))})
    dt3 <<- dt3

    # Plot Selection + reactives for enabling smart selection for YVar, XVar, etc.
    output$Plot3 <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown3, InputName = 'Plot3', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID = 'Plot3', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Plot Type Selection'), Choices = c(AvailablePlots), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    PlotDropDown3[['Plot3']][['SelectedDefault']][[length(PlotDropDown3[['Plot3']][['SelectedDefault']]) + 1L]] <- input$Plot3
    PlotDropDown3 <<- PlotDropDown3

    # Reactives
    Plot3_react <- shiny::reactive({input[['Plot3']]})

    # Y Variable
    output$YVar3 <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown3, InputName = 'YVar3', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      if('CorrelationMatrix' %in% tryCatch({Plot3_react()}, error = 'none')) {
        choices <- names(dt3())[which(RemixAutoML:::ColTypes(dt3()) %in% c('numeric','integer'))]
        MaxVars <- 30
        if(length(choices) == 0) choices <- NULL
      } else {
        choices <- names(dt3())
        MaxVars <- 1
      }
      RemixAutoML:::SelectizeInput(InputID = 'YVar3', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Y-Variable'), Choices = choices, Multiple = TRUE, MaxVars = MaxVars, SelectedDefault = selected_default)
    })

    # Args Storage
    PlotDropDown3[['YVar3']][['SelectedDefault']][[length(PlotDropDown3[['YVar3']][['SelectedDefault']]) + 1L]] <- input$YVar3
    PlotDropDown3 <<- PlotDropDown3

    # X Variable
    output$XVar3 <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown3, InputName = 'XVar3', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      if('CorrelationMatrix' %in% tryCatch({Plot3_react()}, error = 'none')) {
        MaxVars <- 100
        choices <- NULL
      } else {
        MaxVars <- 1
        choices <- names(dt3())
      }
      RemixAutoML:::SelectizeInput(InputID = 'XVar3', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'X-Variable'), Choices = choices, Multiple = TRUE, MaxVars = MaxVars, SelectedDefault = selected_default)
    })

    # Args Storage
    PlotDropDown3[['XVar3']][['SelectedDefault']][[length(PlotDropDown3[['XVar3']][['SelectedDefault']]) + 1L]] <- input$XVar3
    PlotDropDown3 <<- PlotDropDown3

    # Z Variable
    output$ZVar3 <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown3, InputName = 'ZVar3', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      if('HeatMapPlot' %in% tryCatch({Plot3_react()}, error = 'none')) {
        MaxVars <- 1
        choices <- names(dt3())
      } else {
        MaxVars <- 0
        choices <- NULL
      }
      RemixAutoML:::SelectizeInput(InputID = 'ZVar3', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Z-Variable'), Choices = choices, Multiple = TRUE, MaxVars = MaxVars, SelectedDefault = selected_default)
    })

    # Args Storage
    PlotDropDown3[['ZVar3']][['SelectedDefault']][[length(PlotDropDown3[['ZVar3']][['SelectedDefault']]) + 1L]] <- input$ZVar3
    PlotDropDown3 <<- PlotDropDown3

    # Reactives
    YVar3 <- shiny::reactive({shiny::req(input[['YVar3']])})
    YVar3 <<- YVar3
    XVar3 <- shiny::reactive({shiny::req(input[['XVar3']])})
    XVar3 <<- XVar3

    # Select GroupVars
    output$GroupVars3 <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown3, InputName = 'GroupVars3', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      if('CorrelationMatrix' %in% tryCatch({Plot3_react()}, error = 'none')) {
        choices <- names(dt3())[which(RemixAutoML:::ColTypes(dt3()) %in% c('numeric','integer'))]
        if(length(choices) == 0) choices <- NULL
      } else {
        choices <- names(dt3())
      }
      RemixAutoML:::SelectizeInput(InputID='GroupVars3', Label=tags$span(style='color: blue;', 'Select Group Variables'), Choices=choices, Multiple=TRUE, MaxVars = 3, SelectedDefault = selected_default)
    })

    # Args Storage
    PlotDropDown3[['GroupVars3']][['SelectedDefault']][[length(PlotDropDown3[['GroupVars3']][['SelectedDefault']]) + 1L]] <- input$GroupVars3
    PlotDropDown3 <<- PlotDropDown3

    # Reactives
    SelectedGroups3 <- shiny::reactive({RemixAutoML:::ReturnParam(xx = input[['GroupVars3']], VarName = 'GroupVars3', Default = NULL, Switch = TRUE, Type = 'character')})

    # Group Levels 3
    output$Levels_3_1 <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown3, InputName = 'Levels_3_1', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      sgs <- RemixAutoML:::LevelValues(SelectedGroups3()); if(Debug) {print('PickerInput_GetLevels 1'); print(sgs)}
      if(Debug) {print('Levels_3_1 logic check for data'); print(length(sgs) != 0 && length(dt3()) != 0 && sgs[1L] %in% names(dt3()))}
      if(length(sgs) != 0 && length(dt3()) != 0 && sgs[1L] %in% names(dt3())) {
        if(Debug) print('here for data')
        RemixAutoML:::PickerInput_GetLevels2(DataExist=exists('dt3'), InputID='Levels_3_1', InputID2=sgs, Choices=RemixAutoML:::UniqueLevels(input, dt3(), 1L, GroupVars=sgs), NumGroupVar=1L, Multiple=TRUE, SelectedDefault=selected_default)
      } else {
        RemixAutoML:::PickerInput_GetLevels2(DataExist=exists('aaasdfasdfsdf'), InputID='Levels_3_1', InputID2=NULL, Choices=NULL, NumGroupVar=1L, Multiple=TRUE, SelectedDefault=selected_default)
      }
    })

    # Args Storage
    PlotDropDown3[['Levels_3_1']][['SelectedDefault']][[length(PlotDropDown3[['Levels_3_1']][['SelectedDefault']]) + 1L]] <- input$Levels_3_1
    PlotDropDown3 <<- PlotDropDown3

    # Group Levels 3
    output$Levels_3_2 <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown3, InputName = 'Levels_3_2', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      sgs <- RemixAutoML:::LevelValues(SelectedGroups3()); if(Debug) {print('PickerInput_GetLevels 2'); print(sgs)}
      if(length(sgs) != 0 && length(dt3()) != 0 && sgs[1L] %in% names(dt3())) {
        RemixAutoML:::PickerInput_GetLevels2(DataExist=exists('dt3'), InputID='Levels_3_2', InputID2=sgs, Choices=RemixAutoML:::UniqueLevels(input, dt3(), 2L, GroupVars=sgs), NumGroupVar=2L, Multiple=TRUE, SelectedDefault=selected_default)
      } else {
        RemixAutoML:::PickerInput_GetLevels2(DataExist=exists('aaasdfasdfsdf'), InputID='Levels_3_2', InputID2=NULL, Choices=NULL, NumGroupVar=2L, Multiple=TRUE, SelectedDefault=selected_default)
      }
    })

    # Args Storage
    PlotDropDown3[['Levels_3_2']][['SelectedDefault']][[length(PlotDropDown3[['Levels_3_2']][['SelectedDefault']]) + 1L]] <- input$Levels_3_2
    PlotDropDown3 <<- PlotDropDown3

    # Group Levels 3
    output$Levels_3_3 <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown3, InputName = 'Levels_3_3', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      sgs <- RemixAutoML:::LevelValues(SelectedGroups3()); if(Debug) {print('PickerInput_GetLevels 3'); print(sgs)}
      if(length(sgs) != 0 && length(dt3()) != 0 && sgs[1L] %in% names(dt3())) {
        RemixAutoML:::PickerInput_GetLevels2(DataExist=exists('data'), InputID='Levels_3_3', InputID2=sgs, Choices=RemixAutoML:::UniqueLevels(input, dt3(), 3L, GroupVars=sgs), NumGroupVar=3L, Multiple=TRUE, SelectedDefault=selected_default)
      } else {
        RemixAutoML:::PickerInput_GetLevels2(DataExist=exists('aaasdfasdfsdf'), InputID='Levels_3_3', InputID2=NULL, Choices=NULL, NumGroupVar=3L, Multiple=TRUE, SelectedDefault=selected_default)
      }
    })

    # Args Storage
    PlotDropDown3[['Levels_3_3']][['SelectedDefault']][[length(PlotDropDown3[['Levels_3_3']][['SelectedDefault']]) + 1L]] <- input$Levels_3_3
    PlotDropDown3 <<- PlotDropDown3

    # Facet Var 1
    output$FacetVar_3_1 <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown3, InputName = 'FacetVar_3_1', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='FacetVar_3_1', Label = tags$span(style='color: blue;', 'Facet Variable 1'), Choices = names(dt3()), Multiple = TRUE, MaxVars = 1)
    })

    # Args Storage
    PlotDropDown3[['FacetVar_3_1']][['SelectedDefault']][[length(PlotDropDown3[['FacetVar_3_1']][['SelectedDefault']]) + 1L]] <- input$FacetVar_3_1
    PlotDropDown3 <<- PlotDropDown3

    # Facet Var 2
    output$FacetVar_3_2 <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown3, InputName = 'FacetVar_3_2', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='FacetVar_3_2', Label = tags$span(style='color: blue;', 'Facet Variable 2'), Choices = names(dt3()), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    PlotDropDown3[['FacetVar_3_2']][['SelectedDefault']][[length(PlotDropDown3[['FacetVar_3_2']][['SelectedDefault']]) + 1L]] <- input$FacetVar_3_2
    PlotDropDown3 <<- PlotDropDown3

    # Size Var 2
    output$SizeVar3 <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown3, InputName = 'SizeVar3', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID = 'SizeVar3', Label = tags$span(style='color: blue;', 'Size Variable'), Choices = names(dt3()), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    PlotDropDown3[['SizeVar3']][['SelectedDefault']][[length(PlotDropDown3[['SizeVar3']][['SelectedDefault']]) + 1L]] <- input$SizeVar3
    PlotDropDown3 <<- PlotDropDown3

    # Bar Plot Aggregation Method
    output$BarPlotAgg3 <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown3, InputName = 'BarPlotAgg3', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID = 'BarPlotAgg3', Label = tags$span(style='color: blue;', 'Aggregate Method'), Choices = c('sum','mean','median','sd'), Multiple = FALSE, MaxVars = 1, CloseAfterSelect = TRUE, Debug = FALSE, SelectedDefault = selected_default)
    })

    # Args Storage
    PlotDropDown3[['BarPlotAgg3']][['SelectedDefault']][[length(PlotDropDown3[['BarPlotAgg3']][['SelectedDefault']]) + 1L]] <- input$BarPlotAgg3
    PlotDropDown3 <<- PlotDropDown3

    # MultiClass Level Selection for PDP Model Insight Plot
    output$TargetLevel3 <- shiny::renderUI({
      if(length(YVar3()) != 0L && length(dt3()) != 0) {
        if(!any(class(dt3()[[YVar3()[[1L]]]]) %in% c('numeric','integer'))) vals <- as.character(unique(dt3()[[YVar3()]])) else vals <- NULL
      } else {
        vals <- NULL
      }
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown3, InputName = 'TargetLevel3', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID = 'TargetLevel3', Label = tags$span(style='color: blue;', 'Target Level'), Choices = vals, Multiple = FALSE, MaxVars = 1, CloseAfterSelect = TRUE, Debug = FALSE, SelectedDefault = selected_default)
    })

    # Args Storage
    PlotDropDown3[['TargetLevel3']][['SelectedDefault']][[length(PlotDropDown3[['TargetLevel3']][['SelectedDefault']]) + 1L]] <- input$TargetLevel3
    PlotDropDown3 <<- PlotDropDown3

  })

  # Plot 4 DropDown
  shiny::observeEvent(input$PlotDropDown4, {


    # Create intra-session tracking list
    if(!exists('PlotDropDown4')) PlotDropDown4 <- list(Debug = Debug)
    print('Plot4_SelectData')

    # Select data
    output$Plot4_SelectData <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown4, InputName = 'Plot4_SelectData', ArgName = 'SelectedDefault', Default = names(DataList)[[1L]], Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='Plot4_SelectData', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Choose data set'), Choices = names(DataList), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    PlotDropDown4[['Plot4_SelectData']][['SelectedDefault']][[length(PlotDropDown4[['Plot4_SelectData']][['SelectedDefault']]) + 1L]] <- input$Plot4_SelectData
    PlotDropDown4 <<- PlotDropDown4

    # Reactives
    dt4 <- shiny::reactive({shiny::req(tryCatch({DataList[[input$Plot4_SelectData]]}, error = function(x) DataList[[1L]]))})
    dt4 <<- dt4

    # Plot Selection + reactives for enabling smart selection for YVar, XVar, etc.
    output$Plot4 <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown4, InputName = 'Plot4', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID = 'Plot4', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Plot Type Selection'), Choices = c(AvailablePlots), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    PlotDropDown4[['Plot4']][['SelectedDefault']][[length(PlotDropDown4[['Plot4']][['SelectedDefault']]) + 1L]] <- input$Plot4
    PlotDropDown4 <<- PlotDropDown4

    # Reactives
    Plot4_react <- shiny::reactive({input[['Plot4']]})

    # Y Variable
    output$YVar4 <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown4, InputName = 'YVar4', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      if('CorrelationMatrix' %in% tryCatch({Plot4_react()}, error = 'none')) {
        choices <- names(dt4())[which(RemixAutoML:::ColTypes(dt4()) %in% c('numeric','integer'))]
        MaxVars <- 30
        if(length(choices) == 0) choices <- NULL
      } else {
        choices <- names(dt4())
        MaxVars <- 1
      }
      RemixAutoML:::SelectizeInput(InputID = 'YVar4', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Y-Variable'), Choices = choices, Multiple = TRUE, MaxVars = MaxVars, SelectedDefault = selected_default)
    })

    # Args Storage
    PlotDropDown4[['YVar4']][['SelectedDefault']][[length(PlotDropDown4[['YVar4']][['SelectedDefault']]) + 1L]] <- input$YVar4
    PlotDropDown4 <<- PlotDropDown4

    # X Variable
    output$XVar4 <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown4, InputName = 'XVar4', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      if('CorrelationMatrix' %in% tryCatch({Plot4_react()}, error = 'none')) {
        MaxVars <- 100
        choices <- NULL
      } else {
        MaxVars <- 1
        choices <- names(dt4())
      }
      RemixAutoML:::SelectizeInput(InputID = 'XVar4', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'X-Variable'), Choices = choices, Multiple = TRUE, MaxVars = MaxVars, SelectedDefault = selected_default)
    })

    # Args Storage
    PlotDropDown4[['XVar4']][['SelectedDefault']][[length(PlotDropDown4[['XVar4']][['SelectedDefault']]) + 1L]] <- input$XVar4
    PlotDropDown4 <<- PlotDropDown4

    # Z Variable
    output$ZVar4 <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown4, InputName = 'ZVar4', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      if('HeatMapPlot' %in% tryCatch({Plot4_react()}, error = 'none')) {
        MaxVars <- 1
        choices <- names(dt4())
      } else {
        MaxVars <- 0
        choices <- NULL
      }
      RemixAutoML:::SelectizeInput(InputID = 'ZVar4', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Z-Variable'), Choices = choices, Multiple = TRUE, MaxVars = MaxVars, SelectedDefault = selected_default)
    })

    # Args Storage
    PlotDropDown4[['ZVar4']][['SelectedDefault']][[length(PlotDropDown4[['ZVar4']][['SelectedDefault']]) + 1L]] <- input$ZVar4
    PlotDropDown4 <<- PlotDropDown4

    # Reactives
    YVar4 <- shiny::reactive({shiny::req(input[['YVar4']])})
    YVar4 <<- YVar4
    XVar4 <- shiny::reactive({shiny::req(input[['XVar4']])})
    XVar4 <<- XVar4

    # Select GroupVars
    output$GroupVars4 <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown4, InputName = 'GroupVars4', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      if('CorrelationMatrix' %in% tryCatch({Plot4_react()}, error = 'none')) {
        choices <- names(dt4())[which(RemixAutoML:::ColTypes(dt4()) %in% c('numeric','integer'))]
        if(length(choices) == 0) choices <- NULL
      } else {
        choices <- names(dt4())
      }
      RemixAutoML:::SelectizeInput(InputID='GroupVars4', Label=tags$span(style='color: blue;', 'Select Group Variables'), Choices=choices, Multiple=TRUE, MaxVars = 3, SelectedDefault = selected_default)
    })

    # Args Storage
    PlotDropDown4[['GroupVars4']][['SelectedDefault']][[length(PlotDropDown4[['GroupVars4']][['SelectedDefault']]) + 1L]] <- input$GroupVars4
    PlotDropDown4 <<- PlotDropDown4

    # Reactives
    SelectedGroups4 <- shiny::reactive({RemixAutoML:::ReturnParam(xx = input[['GroupVars4']], VarName = 'GroupVars4', Default = NULL, Switch = TRUE, Type = 'character')})

    # Group Levels 4
    output$Levels_4_1 <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown4, InputName = 'Levels_4_1', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      sgs <- RemixAutoML:::LevelValues(SelectedGroups4()); if(Debug) {print('PickerInput_GetLevels 1'); print(sgs)}
      if(Debug) {print('Levels_4_1 logic check for data'); print(length(sgs) != 0 && length(dt4()) != 0 && sgs[1L] %in% names(dt4()))}
      if(length(sgs) != 0 && length(dt4()) != 0 && sgs[1L] %in% names(dt4())) {
        if(Debug) print('here for data')
        RemixAutoML:::PickerInput_GetLevels2(DataExist=exists('dt4'), InputID='Levels_4_1', InputID2=sgs, Choices=RemixAutoML:::UniqueLevels(input, dt4(), 1L, GroupVars=sgs), NumGroupVar=1L, Multiple=TRUE, SelectedDefault=selected_default)
      } else {
        RemixAutoML:::PickerInput_GetLevels2(DataExist=exists('aaasdfasdfsdf'), InputID='Levels_4_1', InputID2=NULL, Choices=NULL, NumGroupVar=1L, Multiple=TRUE, SelectedDefault=selected_default)
      }
    })

    # Args Storage
    PlotDropDown4[['Levels_4_1']][['SelectedDefault']][[length(PlotDropDown4[['Levels_4_1']][['SelectedDefault']]) + 1L]] <- input$Levels_4_1
    PlotDropDown4 <<- PlotDropDown4

    # Group Levels 4
    output$Levels_4_2 <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown4, InputName = 'Levels_4_2', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      sgs <- RemixAutoML:::LevelValues(SelectedGroups4()); if(Debug) {print('PickerInput_GetLevels 2'); print(sgs)}
      if(length(sgs) != 0 && length(dt4()) != 0 && sgs[1L] %in% names(dt4())) {
        RemixAutoML:::PickerInput_GetLevels2(DataExist=exists('dt4'), InputID='Levels_4_2', InputID2=sgs, Choices=RemixAutoML:::UniqueLevels(input, dt4(), 2L, GroupVars=sgs), NumGroupVar=2L, Multiple=TRUE, SelectedDefault=selected_default)
      } else {
        RemixAutoML:::PickerInput_GetLevels2(DataExist=exists('aaasdfasdfsdf'), InputID='Levels_4_2', InputID2=NULL, Choices=NULL, NumGroupVar=2L, Multiple=TRUE, SelectedDefault=selected_default)
      }
    })

    # Args Storage
    PlotDropDown4[['Levels_4_2']][['SelectedDefault']][[length(PlotDropDown4[['Levels_4_2']][['SelectedDefault']]) + 1L]] <- input$Levels_4_2
    PlotDropDown4 <<- PlotDropDown4

    # Group Levels 4
    output$Levels_4_3 <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown4, InputName = 'Levels_4_3', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      sgs <- RemixAutoML:::LevelValues(SelectedGroups4()); if(Debug) {print('PickerInput_GetLevels 4'); print(sgs)}
      if(length(sgs) != 0 && length(dt4()) != 0 && sgs[1L] %in% names(dt4())) {
        RemixAutoML:::PickerInput_GetLevels2(DataExist=exists('data'), InputID='Levels_4_3', InputID2=sgs, Choices=RemixAutoML:::UniqueLevels(input, dt4(), 3L, GroupVars=sgs), NumGroupVar=3L, Multiple=TRUE, SelectedDefault=selected_default)
      } else {
        RemixAutoML:::PickerInput_GetLevels2(DataExist=exists('aaasdfasdfsdf'), InputID='Levels_4_3', InputID2=NULL, Choices=NULL, NumGroupVar=3L, Multiple=TRUE, SelectedDefault=selected_default)
      }
    })

    # Args Storage
    PlotDropDown4[['Levels_4_3']][['SelectedDefault']][[length(PlotDropDown4[['Levels_4_3']][['SelectedDefault']]) + 1L]] <- input$Levels_4_3
    PlotDropDown4 <<- PlotDropDown4

    # Facet Var 1
    output$FacetVar_4_1 <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown4, InputName = 'FacetVar_4_1', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='FacetVar_4_1', Label = tags$span(style='color: blue;', 'Facet Variable 1'), Choices = names(dt4()), Multiple = TRUE, MaxVars = 1)
    })

    # Args Storage
    PlotDropDown4[['FacetVar_4_1']][['SelectedDefault']][[length(PlotDropDown4[['FacetVar_4_1']][['SelectedDefault']]) + 1L]] <- input$FacetVar_4_1
    PlotDropDown4 <<- PlotDropDown4

    # Facet Var 2
    output$FacetVar_4_2 <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown4, InputName = 'FacetVar_4_2', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID='FacetVar_4_2', Label = tags$span(style='color: blue;', 'Facet Variable 2'), Choices = names(dt4()), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    PlotDropDown4[['FacetVar_4_2']][['SelectedDefault']][[length(PlotDropDown4[['FacetVar_4_2']][['SelectedDefault']]) + 1L]] <- input$FacetVar_4_2
    PlotDropDown4 <<- PlotDropDown4

    # Size Var 2
    output$SizeVar4 <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown4, InputName = 'SizeVar4', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID = 'SizeVar4', Label = tags$span(style='color: blue;', 'Size Variable'), Choices = names(dt4()), Multiple = TRUE, MaxVars = 1, SelectedDefault = selected_default)
    })

    # Args Storage
    PlotDropDown4[['SizeVar4']][['SelectedDefault']][[length(PlotDropDown4[['SizeVar4']][['SelectedDefault']]) + 1L]] <- input$SizeVar4
    PlotDropDown4 <<- PlotDropDown4

    # Bar Plot Aggregation Method
    output$BarPlotAgg4 <- shiny::renderUI({
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown4, InputName = 'BarPlotAgg4', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID = 'BarPlotAgg4', Label = tags$span(style='color: blue;', 'Aggregate Method'), Choices = c('sum','mean','median','sd'), Multiple = FALSE, MaxVars = 1, CloseAfterSelect = TRUE, Debug = FALSE, SelectedDefault = selected_default)
    })

    # Args Storage
    PlotDropDown4[['BarPlotAgg4']][['SelectedDefault']][[length(PlotDropDown4[['BarPlotAgg4']][['SelectedDefault']]) + 1L]] <- input$BarPlotAgg4
    PlotDropDown4 <<- PlotDropDown4

    # MultiClass Level Selection for PDP Model Insight Plot
    output$TargetLevel4 <- shiny::renderUI({
      if(length(YVar4()) != 0L && length(dt4()) != 0) {
        if(!any(class(dt4()[[YVar4()[[1L]]]]) %in% c('numeric','integer'))) vals <- as.character(unique(dt4()[[YVar4()]])) else vals <- NULL
      } else {
        vals <- NULL
      }
      selected_default <- RemixAutoML:::IntraSessionDefaults(List = PlotDropDown4, InputName = 'TargetLevel4', ArgName = 'SelectedDefault', Default = NULL, Debug = Debug)
      RemixAutoML:::SelectizeInput(InputID = 'TargetLevel4', Label = tags$span(style='color: blue;', 'Target Level'), Choices = vals, Multiple = FALSE, MaxVars = 1, CloseAfterSelect = TRUE, Debug = FALSE, SelectedDefault = selected_default)
    })

    # Args Storage
    PlotDropDown4[['TargetLevel4']][['SelectedDefault']][[length(PlotDropDown4[['TargetLevel4']][['SelectedDefault']]) + 1L]] <- input$TargetLevel4
    PlotDropDown4 <<- PlotDropDown4

  })

  # Axis Limits UI Inputs
  shiny::observeEvent(input$AxisLimitsInputs, {
    output$YLimMin1 <- shiny::renderUI({
      RemixAutoML:::TextInput(InputID = 'YLimMin1', Label = tags$span(style='color: blue;', 'Y Min Limit 1'), Value = NULL, Placeholder = 'Insert a number')
    })
    output$YLimMax1 <- shiny::renderUI({
      RemixAutoML:::TextInput(InputID = 'YLimMax1', Label = tags$span(style='color: blue;', 'Y Max Limit 1'), Value = NULL, Placeholder = 'Insert a number')
    })
    output$YLimMin2 <- shiny::renderUI({
      RemixAutoML:::TextInput(InputID = 'YLimMin2', Label = tags$span(style='color: blue;', 'Y Min Limit 2'), Value = NULL, Placeholder = 'Insert a number')
    })
    output$YLimMax2 <- shiny::renderUI({
      RemixAutoML:::TextInput(InputID = 'YLimMax2', Label = tags$span(style='color: blue;', 'Y Max Limit 2'), Value = NULL, Placeholder = 'Insert a number')
    })
    output$YLimMin3 <- shiny::renderUI({
      RemixAutoML:::TextInput(InputID = 'YLimMin3', Label = tags$span(style='color: blue;', 'Y Min Limit 3'), Value = NULL, Placeholder = 'Insert a number')
    })
    output$YLimMax3 <- shiny::renderUI({
      RemixAutoML:::TextInput(InputID = 'YLimMax3', Label = tags$span(style='color: blue;', 'Y Max Limit 3'), Value = NULL, Placeholder = 'Insert a number')
    })
    output$YLimMin4 <- shiny::renderUI({
      RemixAutoML:::TextInput(InputID = 'YLimMin4', Label = tags$span(style='color: blue;', 'Y Min Limit 4'), Value = NULL, Placeholder = 'Insert a number')
    })
    output$YLimMax4 <- shiny::renderUI({
      RemixAutoML:::TextInput(InputID = 'YLimMax4', Label = tags$span(style='color: blue;', 'Y Max Limit 4'), Value = NULL, Placeholder = 'Insert a number')
    })
    output$XLimMin1 <- shiny::renderUI({
      RemixAutoML:::TextInput(InputID = 'XLimMin1', Label = tags$span(style='color: blue;', 'X Min Limit 1'), Value = NULL, Placeholder = 'Insert a number')
    })
    output$XLimMax1 <- shiny::renderUI({
      RemixAutoML:::TextInput(InputID = 'XLimMax1', Label = tags$span(style='color: blue;', 'X Max Limit 1'), Value = NULL, Placeholder = 'Insert a number')
    })
    output$XLimMin2 <- shiny::renderUI({
      RemixAutoML:::TextInput(InputID = 'XLimMin2', Label = tags$span(style='color: blue;', 'X Min Limit 2'), Value = NULL, Placeholder = 'Insert a number')
    })
    output$XLimMax2 <- shiny::renderUI({
      RemixAutoML:::TextInput(InputID = 'XLimMax2', Label = tags$span(style='color: blue;', 'X Max Limit 2'), Value = NULL, Placeholder = 'Insert a number')
    })
    output$XLimMin3 <- shiny::renderUI({
      RemixAutoML:::TextInput(InputID = 'XLimMin3', Label = tags$span(style='color: blue;', 'X Min Limit 3'), Value = NULL, Placeholder = 'Insert a number')
    })
    output$XLimMax3 <- shiny::renderUI({
      RemixAutoML:::TextInput(InputID = 'XLimMax3', Label = tags$span(style='color: blue;', 'X Max Limit 3'), Value = NULL, Placeholder = 'Insert a number')
    })
    output$XLimMin4 <- shiny::renderUI({
      RemixAutoML:::TextInput(InputID = 'XLimMin4', Label = tags$span(style='color: blue;', 'X Min Limit 4'), Value = NULL, Placeholder = 'Insert a number')
    })
    output$XLimMax4 <- shiny::renderUI({
      RemixAutoML:::TextInput(InputID = 'XLimMax4', Label = tags$span(style='color: blue;', 'X Max Limit 4'), Value = NULL, Placeholder = 'Insert a number')
    })
  })

  # Plot Structure Inputs
  shiny::observeEvent(input$PlotStructureInputs, {

    # Dependency reactive values
    xx1 <- tryCatch({XVar1()}, error = function(x) 'None')
    xx2 <- tryCatch({XVar2()}, error = function(x) 'None')
    xx3 <- tryCatch({XVar3()}, error = function(x) 'None')
    xx4 <- tryCatch({XVar4()}, error = function(x) 'None')
    yy1 <- tryCatch({YVar1()}, error = function(x) 'None')
    yy2 <- tryCatch({YVar2()}, error = function(x) 'None')
    yy3 <- tryCatch({YVar3()}, error = function(x) 'None')
    yy4 <- tryCatch({YVar4()}, error = function(x) 'None')
    dd1 <- tryCatch({dt1()}, error = function(x) NULL)
    dd2 <- tryCatch({dt2()}, error = function(x) NULL)
    dd3 <- tryCatch({dt3()}, error = function(x) NULL)
    dd4 <- tryCatch({dt4()}, error = function(x) NULL)

    # Tick Marks debugging
    if(Debug) {
      print('Tick Marks Inspection start here')
      print(xx1)
      print(xx2)
      print(xx3)
      print(xx4)
      print(yy1)
      print(yy2)
      print(yy3)
      print(yy4)
      print(dd1)
      print(dd2)
      print(dd3)
      print(dd4)
      print('Tick Marks Inspection end here')
    }

    # Y Text Angles
    output$AngleY1 <- shiny::renderUI({
      RemixAutoML:::NumericInput(InputID = 'AngleY1', Label = tags$span(style='color: blue;', 'Plot 1 Y-Axis Text Angle'), Step = 5, Min = 0, Max = 360, Value = 0)
    })
    output$AngleY2 <- shiny::renderUI({
      RemixAutoML:::NumericInput(InputID = 'AngleY2', Label = tags$span(style='color: blue;', 'Plot 2 Y-Axis Text Angle'), Step = 5, Min = 0, Max = 360, Value = 0)
    })
    output$AngleY3 <- shiny::renderUI({
      RemixAutoML:::NumericInput(InputID = 'AngleY3', Label = tags$span(style='color: blue;', 'Plot 3 Y-Axis Text Angle'), Step = 5, Min = 0, Max = 360, Value = 0)
    })
    output$AngleY4 <- shiny::renderUI({
      RemixAutoML:::NumericInput(InputID = 'AngleY4', Label = tags$span(style='color: blue;', 'Plot 4 Y-Axis Text Angle'), Step = 5, Min = 0, Max = 360, Value = 0)
    })

    # X Text Angles
    output$AngleX1 <- shiny::renderUI({
      RemixAutoML:::NumericInput(InputID = 'AngleX1', Label = tags$span(style='color: blue;', 'Plot 1 X-Axis Text Angle'), Step = 5, Min = 0, Max = 360, Value = 90)
    })
    output$AngleX2 <- shiny::renderUI({
      RemixAutoML:::NumericInput(InputID = 'AngleX2', Label = tags$span(style='color: blue;', 'Plot 2 X-Axis Text Angle'), Step = 5, Min = 0, Max = 360, Value = 90)
    })
    output$AngleX3 <- shiny::renderUI({
      RemixAutoML:::NumericInput(InputID = 'AngleX3', Label = tags$span(style='color: blue;', 'Plot 3 X-Axis Text Angle'), Step = 5, Min = 0, Max = 360, Value = 90)
    })
    output$AngleX4 <- shiny::renderUI({
      RemixAutoML:::NumericInput(InputID = 'AngleX4', Label = tags$span(style='color: blue;', 'Plot 4 X-Axis Text Angle'), Step = 5, Min = 0, Max = 360, Value = 90)
    })

    # YTicks Values (NULL is whats handled by RemixAutoML:::YTicks())
    output$YTicks1 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'YTicks1', Label = tags$span(style='color: blue;', 'Plot 1 Y-Axis Ticks'), Choices = RemixAutoML:::YTicks(dd1, yvar = yy1), SelectedDefault = 'Default', Multiple = TRUE)
    })
    output$YTicks2 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'YTicks2', Label = tags$span(style='color: blue;', 'Plot 2 Y-Axis Ticks'), Choices = RemixAutoML:::YTicks(dd2, yvar = yy2), SelectedDefault = 'Default', Multiple = TRUE)
    })
    output$YTicks3 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'YTicks3', Label = tags$span(style='color: blue;', 'Plot 3 Y-Axis Ticks'), Choices = RemixAutoML:::YTicks(dd3, yvar = yy3), SelectedDefault = 'Default', Multiple = TRUE)
    })
    output$YTicks4 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'YTicks4', Label = tags$span(style='color: blue;', 'Plot 4 Y-Axis Ticks'), Choices = RemixAutoML:::YTicks(dd4, yvar = yy4), SelectedDefault = 'Default', Multiple = TRUE)
    })

    # XTicks Values ('None' is whats handled by RemixAutoML:::XTicks())
    output$XTicks1 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'XTicks1', Label = tags$span(style='color: blue;', 'Plot 1 X-Axis Ticks'), Choices = RemixAutoML:::XTicks(dd1, xvar=xx1), SelectedDefault = 'Default', Multiple = TRUE)
    })
    output$XTicks2 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'XTicks2', Label = tags$span(style='color: blue;', 'Plot 2 X-Axis Ticks'), Choices = RemixAutoML:::XTicks(dd2, xvar=xx2), SelectedDefault = 'Default', Multiple = TRUE)
    })
    output$XTicks3 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'XTicks3', Label = tags$span(style='color: blue;', 'Plot 3 X-Axis Ticks'), Choices = RemixAutoML:::XTicks(dd3, xvar=xx3), SelectedDefault = 'Default', Multiple = TRUE)
    })
    output$XTicks4 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'XTicks4', Label = tags$span(style='color: blue;', 'Plot 4 X-Axis Ticks'), Choices = RemixAutoML:::XTicks(dd4, xvar=xx4), SelectedDefault = 'Default', Multiple = TRUE)
    })

    # Text Size
    output$TextSize1 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'TextSize1', Label = tags$span(style='color: blue;', 'Plot 1 Text Size'),Choices = c(as.character(seq(1L,100L,1L))), SelectedDefault = '14', Multiple = FALSE)
    })
    output$TextSize2 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'TextSize2', Label = tags$span(style='color: blue;', 'Plot 2 Text Size'),Choices = c(as.character(seq(1L,100L,1L))), SelectedDefault = '14', Multiple = FALSE)
    })
    output$TextSize3 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'TextSize3', Label = tags$span(style='color: blue;', 'Plot 3 Text Size'),Choices = c(as.character(seq(1L,100L,1L))), SelectedDefault = '14', Multiple = FALSE)
    })
    output$TextSize4 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'TextSize4', Label = tags$span(style='color: blue;', 'Plot 4 Text Size'),Choices = c(as.character(seq(1L,100L,1L))), SelectedDefault = '14', Multiple = FALSE)
    })

    # Outlier Size
    output$OutlierSize1 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'OutlierSize1', Label = tags$span(style='color: blue;', 'Plot 1 Outlier Size'), Choices = c(seq(0.01,2,0.01)), SelectedDefault = 0.01, Multiple = FALSE)
    })
    output$OutlierSize2 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'OutlierSize2', Label = tags$span(style='color: blue;', 'Plot 2 Outlier Size'), Choices = c(seq(0.01,2,0.01)), SelectedDefault = 0.01, Multiple = FALSE)
    })
    output$OutlierSize3 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'OutlierSize3', Label = tags$span(style='color: blue;', 'Plot 3 Outlier Size'), Choices = c(seq(0.01,2,0.01)), SelectedDefault = 0.01, Multiple = FALSE)
    })
    output$OutlierSize4 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'OutlierSize4', Label = tags$span(style='color: blue;', 'Plot 4 Outlier Size'), Choices = c(seq(0.01,2,0.01)), SelectedDefault = 0.01, Multiple = FALSE)
    })

    # Legend Position
    output$LegendPosition1 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'LegendPosition1', Label = tags$span(style='color: blue;', 'Plot 1 Legend Position'), Choices = c('bottom','left','right','top','none'), SelectedDefault = 'bottom', Multiple = FALSE)
    })
    output$LegendPosition2 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'LegendPosition2', Label = tags$span(style='color: blue;', 'Plot 2 Legend Position'), Choices = c('bottom','left','right','top','none'), SelectedDefault = 'bottom', Multiple = FALSE)
    })
    output$LegendPosition3 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'LegendPosition3', Label = tags$span(style='color: blue;', 'Plot 3 Legend Position'), Choices = c('bottom','left','right','top','none'), SelectedDefault = 'bottom', Multiple = FALSE)
    })
    output$LegendPosition4 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'LegendPosition4', Label = tags$span(style='color: blue;', 'Plot 4 Legend Position'), Choices = c('bottom','left','right','top','none'), SelectedDefault = 'bottom', Multiple = FALSE)
    })

    # Legend border size
    output$LegendBorderSize1 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'LegendBorderSize1', Label = tags$span(style='color: blue;', 'Plot 1 Legend Border Size'), Choices = c(as.character(seq(0.01,2,0.01))), SelectedDefault = as.character(0.01), Multiple = FALSE)
    })
    output$LegendBorderSize2 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'LegendBorderSize2', Label = tags$span(style='color: blue;', 'Plot 2 Legend Border Size'), Choices = c(as.character(seq(0.01,2,0.01))), SelectedDefault = as.character(0.01), Multiple = FALSE)
    })
    output$LegendBorderSize3 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'LegendBorderSize3', Label = tags$span(style='color: blue;', 'Plot 3 Legend Border Size'), Choices = c(as.character(seq(0.01,2,0.01))), SelectedDefault = as.character(0.01), Multiple = FALSE)
    })
    output$LegendBorderSize4 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'LegendBorderSize4', Label = tags$span(style='color: blue;', 'Plot 4 Legend Border Size'), Choices = c(as.character(seq(0.01,2,0.01))), SelectedDefault = as.character(0.01), Multiple = FALSE)
    })
  })

  # Plot Coloring Inputs
  shiny::observeEvent(input$PlotColoringInputs, {
    output$TextColor1 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'TextColor1', Label = tags$span(style='color: blue;', 'Plot 1 Text Color'), Choices = grDevices::colors(), SelectedDefault = 'darkblue', Multiple = FALSE)
    })
    output$TextColor2 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'TextColor2', Label = tags$span(style='color: blue;', 'Plot 2 Text Color'), Choices = grDevices::colors(), SelectedDefault = 'darkblue', Multiple = FALSE)
    })
    output$TextColor3 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'TextColor3', Label = tags$span(style='color: blue;', 'Plot 3 Text Color'), Choices = grDevices::colors(), SelectedDefault = 'darkblue', Multiple = FALSE)
    })
    output$TextColor4 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'TextColor4', Label = tags$span(style='color: blue;', 'Plot 4 Text Color'), Choices = grDevices::colors(), SelectedDefault = 'darkblue', Multiple = FALSE)
    })
    output$ChartColor1 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'ChartColor1', Label = tags$span(style='color: blue;', 'Plot 1 Chart Color'), Choices = grDevices::colors(), SelectedDefault = 'aliceblue', Multiple = FALSE)
    })
    output$ChartColor2 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'ChartColor2', Label = tags$span(style='color: blue;', 'Plot 2 Chart Color'), Choices = grDevices::colors(), SelectedDefault = 'aliceblue', Multiple = FALSE)
    })
    output$ChartColor3 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'ChartColor3', Label = tags$span(style='color: blue;', 'Plot 3 Chart Color'), Choices = grDevices::colors(), SelectedDefault = 'aliceblue', Multiple = FALSE)
    })
    output$ChartColor4 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'ChartColor4', Label = tags$span(style='color: blue;', 'Plot 4 Chart Color'), Choices = grDevices::colors(), SelectedDefault = 'aliceblue', Multiple = FALSE)
    })
    output$GridColor1 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'GridColor1', Label = tags$span(style='color: blue;', 'Plot 1 Grid Lines Color'), Choices = grDevices::colors(), SelectedDefault = 'lightsteelblue1', Multiple = FALSE)
    })
    output$GridColor2 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'GridColor2', Label = tags$span(style='color: blue;', 'Plot 2 Grid Lines Color'), Choices = grDevices::colors(), SelectedDefault = 'lightsteelblue1', Multiple = FALSE)
    })
    output$GridColor3 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'GridColor3', Label = tags$span(style='color: blue;', 'Plot 3 Grid Lines Color'), Choices = grDevices::colors(), SelectedDefault = 'lightsteelblue1', Multiple = FALSE)
    })
    output$GridColor4 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'GridColor4', Label = tags$span(style='color: blue;', 'Plot 4 Grid Lines Color'), Choices = grDevices::colors(), SelectedDefault = 'lightsteelblue1', Multiple = FALSE)
    })
    output$BackGroundColor1 <- shiny::renderUI({
      colourpicker::colourInput(inputId = "BackGroundColor1", tags$span(style='color: blue;', 'Plot 1 Background Color'), value = "gray95")
      #RemixAutoML:::SelectizeInput(InputID = 'BackGroundColor1', Label = tags$span(style='color: blue;', 'Plot 1 Background Color'), Choices = grDevices::colors(), SelectedDefault = 'gray95', Multiple = FALSE)
    })
    output$BackGroundColor2 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'BackGroundColor2', Label = tags$span(style='color: blue;', 'Plot 2 Background Color'), Choices = grDevices::colors(), SelectedDefault = 'gray95', Multiple = FALSE)
    })
    output$BackGroundColor3 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'BackGroundColor3', Label = tags$span(style='color: blue;', 'Plot 3 Background Color'), Choices = grDevices::colors(), SelectedDefault = 'gray95', Multiple = FALSE)
    })
    output$BackGroundColor4 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'BackGroundColor4', Label = tags$span(style='color: blue;', 'Plot 4 Background Color'), Choices = grDevices::colors(), SelectedDefault = 'gray95', Multiple = FALSE)
    })
    output$BorderColor1 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'BorderColor1', Label = tags$span(style='color: blue;', 'Plot 1 Border Color'), Choices = grDevices::colors(), SelectedDefault = 'lightsteelblue4', Multiple = FALSE)
    })
    output$BorderColor2 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'BorderColor2', Label = tags$span(style='color: blue;', 'Plot 2 Border Color'), Choices = grDevices::colors(), SelectedDefault = 'lightsteelblue4', Multiple = FALSE)
    })
    output$BorderColor3 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'BorderColor3', Label = tags$span(style='color: blue;', 'Plot 3 Border Color'), Choices = grDevices::colors(), SelectedDefault = 'lightsteelblue4', Multiple = FALSE)
    })
    output$BorderColor4 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'BorderColor4', Label = tags$span(style='color: blue;', 'Plot 4 Border Color'), Choices = grDevices::colors(), SelectedDefault = 'lightsteelblue4', Multiple = FALSE)
    })
    output$OutlierColor1 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'OutlierColor1', Label = tags$span(style='color: blue;', 'Plot 1 Outlier Color'), Choices = grDevices::colors(), SelectedDefault = 'blue', Multiple = FALSE)
    })
    output$OutlierColor2 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'OutlierColor2', Label = tags$span(style='color: blue;', 'Plot 2 Outlier Color'), Choices = grDevices::colors(), SelectedDefault = 'blue', Multiple = FALSE)
    })
    output$OutlierColor3 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'OutlierColor3', Label = tags$span(style='color: blue;', 'Plot 3 Outlier Color'), Choices = grDevices::colors(), SelectedDefault = 'blue', Multiple = FALSE)
    })
    output$OutlierColor4 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'OutlierColor4', Label = tags$span(style='color: blue;', 'Plot 4 Outlier Color'), Choices = grDevices::colors(), SelectedDefault = 'blue', Multiple = FALSE)
    })
    output$FillColor1 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'FillColor1', Label = tags$span(style='color: blue;', 'Plot 1 BoxPlot Fill Color'), Choices = grDevices::colors(), SelectedDefault = 'gray70', Multiple = FALSE)
    })
    output$FillColor2 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'FillColor2', Label = tags$span(style='color: blue;', 'Plot 2 BoxPlot Fill Color'), Choices = grDevices::colors(), SelectedDefault = 'gray70', Multiple = FALSE)
    })
    output$FillColor3 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'FillColor3', Label = tags$span(style='color: blue;', 'Plot 3 BoxPlot Fill Color'), Choices = grDevices::colors(), SelectedDefault = 'gray70', Multiple = FALSE)
    })
    output$FillColor4 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'FillColor4', Label = tags$span(style='color: blue;', 'Plot 4 BoxPlot Fill Color'), Choices = grDevices::colors(), SelectedDefault = 'gray70', Multiple = FALSE)
    })
    output$SubTitleColor1 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'SubTitleColor1', Label = tags$span(style='color: blue;', 'Plot 1 Subtitle Color'), Choices = grDevices::colors(), SelectedDefault = 'blue', Multiple = FALSE)
    })
    output$SubTitleColor2 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'SubTitleColor2', Label = tags$span(style='color: blue;', 'Plot 2 Subtitle Color'), Choices = grDevices::colors(), SelectedDefault = 'blue', Multiple = FALSE)
    })
    output$SubTitleColor3 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'SubTitleColor3', Label = tags$span(style='color: blue;', 'Plot 3 Subtitle Color'), Choices = grDevices::colors(), SelectedDefault = 'blue', Multiple = FALSE)
    })
    output$SubTitleColor4 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'SubTitleColor4', Label = tags$span(style='color: blue;', 'Plot 4 Subtitle Color'), Choices = grDevices::colors(), SelectedDefault = 'blue', Multiple = FALSE)
    })
  })

  # Gam Line Inputs
  shiny::observeEvent(input$GamLineInputs, {
    output$GamFitScatter1 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='GamFitScatter1', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Fit Gam on Scatter or Copula 1'), Choices=c('TRUE','FALSE'), SelectedDefault=FALSE, Multiple=FALSE)
    })
    output$GamFitScatter2 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='GamFitScatter2', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Fit Gam on Scatter or Copula 2'), Choices=c('TRUE','FALSE'), SelectedDefault=FALSE, Multiple=FALSE)
    })
    output$GamFitScatter3 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='GamFitScatter3', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Fit Gam on Scatter or Copula 3'), Choices=c('TRUE','FALSE'), SelectedDefault=FALSE, Multiple=FALSE)
    })
    output$GamFitScatter4 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='GamFitScatter4', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Fit Gam on Scatter or Copula 4'), Choices=c('TRUE','FALSE'), SelectedDefault=FALSE, Multiple=FALSE)
    })
  })

  # Histogram Bins Inputs
  shiny::observeEvent(input$HistBinsInputs, {
    output$NumberBins1 <- shiny::renderUI({
      RemixAutoML:::NumericInput(InputID='NumberBins1', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'# of Bins for Histogram 1'), Min=1, Max=1000, Step=5, Value=30)
    })
    output$NumberBins2 <- shiny::renderUI({
      RemixAutoML:::NumericInput(InputID='NumberBins2', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'# of Bins for Histogram 2'), Min=1, Max=1000, Step=5, Value=30)
    })
    output$NumberBins3 <- shiny::renderUI({
      RemixAutoML:::NumericInput(InputID='NumberBins3', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'# of Bins for Histogram 3'), Min=1, Max=1000, Step=5, Value=30)
    })
    output$NumberBins4 <- shiny::renderUI({
      RemixAutoML:::NumericInput(InputID='NumberBins4', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'# of Bins for Histogram 4'), Min=1, Max=1000, Step=5, Value=30)
    })
  })

  # Percentile Bins Inputs
  shiny::observeEvent(input$PercBinsInputs, {
    output$Percentile_Buckets1 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='Percentile_Buckets1', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Percentile Buckets 1'), Choices=1:100, SelectedDefault=20, Multiple=FALSE)
    })
    output$Percentile_Buckets2 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='Percentile_Buckets2', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Percentile Buckets 2'), Choices=1:100, SelectedDefault=20, Multiple=FALSE)
    })
    output$Percentile_Buckets3 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='Percentile_Buckets3', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Percentile Buckets 3'), Choices=1:100, SelectedDefault=20, Multiple=FALSE)
    })
    output$Percentile_Buckets4 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='Percentile_Buckets4', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Percentile Buckets 4'), Choices=1:100, SelectedDefault=20, Multiple=FALSE)
    })
  })

  # Shapely Aggregation Method Inputs
  shiny::observeEvent(input$ShapAggInputs, {
    output$ShapAggMethod1 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='ShapAggMethod1', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Shap Agg Method 1'), Choices=c('mean','absmean','meanabs','sd','median','absmedian','medianabs'), SelectedDefault='meanabs', Multiple=FALSE)
    })
    output$ShapAggMethod2 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='ShapAggMethod2', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Shap Agg Method 2'), Choices=c('mean','absmean','meanabs','sd','median','absmedian','medianabs'), SelectedDefault='meanabs', Multiple=FALSE)
    })
    output$ShapAggMethod3 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='ShapAggMethod3', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Shap Agg Method 3'), Choices=c('mean','absmean','meanabs','sd','median','absmedian','medianabs'), SelectedDefault='meanabs', Multiple=FALSE)
    })
    output$ShapAggMethod4 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID='ShapAggMethod4', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Shap Agg Method 4'), Choices=c('mean','absmean','meanabs','sd','median','absmedian','medianabs'), SelectedDefault='meanabs', Multiple=FALSE)
    })
  })

  # Filter Variables Plot 1
  shiny::observeEvent(input$FilterPlotInputs1, {

    # Plot 1
    print('Filter1_SelectData')
    ft1 <- shiny::reactive({shiny::req(tryCatch({DataList[[input$Plot1_SelectData]]}, error = function(x) DataList[[1L]]))})

    # Filter Columns
    output$FilterVariable_1_1 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'FilterVariable_1_1', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Filter Variable 1'), Choices = names(ft1()), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
    })
    output$FilterVariable_1_2 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'FilterVariable_1_2', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Filter Variable 2'), Choices = names(ft1()), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
    })
    output$FilterVariable_1_3 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'FilterVariable_1_3', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Filter Variable 3'), Choices = names(ft1()), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
    })
    output$FilterVariable_1_4 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'FilterVariable_1_4', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Filter Variable 4'), Choices = names(ft1()), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
    })

    # Filter Column Reactives
    FilterVariable_1_1 <- shiny::reactive({shiny::req(input[['FilterVariable_1_1']])})
    FilterVariable_1_2 <- shiny::reactive({shiny::req(input[['FilterVariable_1_2']])})
    FilterVariable_1_3 <- shiny::reactive({shiny::req(input[['FilterVariable_1_3']])})
    FilterVariable_1_4 <- shiny::reactive({shiny::req(input[['FilterVariable_1_4']])})

    # Filter Logic
    output$FilterLogic_1_1 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_1_1', selected=RemixAutoML:::FL_Default(ft1(), x=tryCatch({FilterVariable_1_1()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })
    output$FilterLogic_1_2 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_1_2', selected=RemixAutoML:::FL_Default(ft1(), x=tryCatch({FilterVariable_1_2()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })
    output$FilterLogic_1_3 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_1_3', selected=RemixAutoML:::FL_Default(ft1(), x=tryCatch({FilterVariable_1_3()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })
    output$FilterLogic_1_4 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_1_4', selected=RemixAutoML:::FL_Default(ft1(), x=tryCatch({FilterVariable_1_4()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })

    # Filter Values
    output$FilterValue_1_1_1 <- shiny::renderUI({
      params <- list(ft1(), VarName=tryCatch({input[['FilterVariable_1_1']]}, error = function(x) NULL), type=1)
      choices <- tryCatch({RemixAutoML:::KeyVarsInit(ft1(), VarName = input[['FilterVariable_1_1']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(ft1(), VarName=input[['FilterVariable_1_1']])
      RemixAutoML:::SelectizeInput(Multiple=Mult, InputID='FilterValue_1_1_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML:::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })
    output$FilterValue_1_1_2 <- shiny::renderUI({
      params <- list(data=ft1(), VarName=tryCatch({input[['FilterVariable_1_1']]}, error = function(x) NULL), type=2)
      choices <- tryCatch({RemixAutoML:::KeyVarsInit(ft1(), VarName = input[['FilterVariable_1_1']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data=ft1(), VarName=input[['FilterVariable_1_1']])
      RemixAutoML:::SelectizeInput(Multiple=Mult, InputID='FilterValue_1_1_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML:::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })
    output$FilterValue_1_2_1 <- shiny::renderUI({
      params <- list(data=ft1(), VarName=tryCatch({input[['FilterVariable_1_2']]}, error = function(x) NULL), type=1)
      choices <- tryCatch({RemixAutoML:::KeyVarsInit(ft1(), VarName = input[['FilterVariable_1_2']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(ft1(), VarName=input[['FilterVariable_1_2']])
      RemixAutoML:::SelectizeInput(Multiple=Mult, InputID='FilterValue_1_2_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML:::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })
    output$FilterValue_1_2_2 <- shiny::renderUI({
      params <- list(data=ft1(), VarName=tryCatch({input[['FilterVariable_1_2']]}, error = function(x) NULL), type=2)
      choices <- tryCatch({RemixAutoML:::KeyVarsInit(ft1(), VarName = input[['FilterVariable_1_2']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(ft1(), VarName=input[['FilterVariable_1_2']])
      RemixAutoML:::SelectizeInput(Multiple=Mult, InputID='FilterValue_1_2_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML:::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })
    output$FilterValue_1_3_1 <- shiny::renderUI({
      params <- list(data=ft1(), VarName=tryCatch({input[['FilterVariable_1_3']]}, error = function(x) NULL), type=1)
      choices <- tryCatch({RemixAutoML:::KeyVarsInit(ft1(), VarName = input[['FilterVariable_1_3']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(ft1(), VarName=input[['FilterVariable_1_3']])
      RemixAutoML:::SelectizeInput(Multiple = Mult, InputID='FilterValue_1_3_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML:::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })
    output$FilterValue_1_3_2 <- shiny::renderUI({
      params <- list(data=ft1(), VarName=tryCatch({input[['FilterVariable_1_3']]}, error = function(x) NULL), type=2)
      choices <- tryCatch({RemixAutoML:::KeyVarsInit(ft1(), VarName = input[['FilterVariable_1_3']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(ft1(), VarName=input[['FilterVariable_1_3']])
      RemixAutoML:::SelectizeInput(Multiple = Mult, InputID='FilterValue_1_3_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML:::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })
    output$FilterValue_1_4_1 <- shiny::renderUI({
      params <- list(data=ft1(), VarName=tryCatch({input[['FilterVariable_1_4']]}, error = function(x) NULL), type=1)
      choices <- tryCatch({RemixAutoML:::KeyVarsInit(ft1(), VarName = input[['FilterVariable_1_4']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(ft1(), VarName=input[['FilterVariable_1_4']])
      RemixAutoML:::SelectizeInput(Multiple = Mult, InputID='FilterValue_1_4_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML:::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })
    output$FilterValue_1_4_2 <- shiny::renderUI({
      params <- list(data=ft1(), VarName=tryCatch({input[['FilterVariable_1_4']]}, error = function(x) NULL), type=2)
      choices <- tryCatch({RemixAutoML:::KeyVarsInit(ft1(), VarName = input[['FilterVariable_1_4']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(ft1(), VarName=input[['FilterVariable_1_4']])
      RemixAutoML:::SelectizeInput(Multiple=Mult, InputID='FilterValue_1_4_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML:::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })
  })

  # Filter Variables Plot 2
  shiny::observeEvent(input$FilterPlotInputs2, {

    # Plot 2
    print('Filter2_SelectData')
    ft2 <- shiny::reactive({shiny::req(tryCatch({DataList[[input$Plot2_SelectData]]}, error = function(x) DataList[[1L]]))})

    # Plot 2
    # Filter Columns
    output$FilterVariable_2_1 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'FilterVariable_2_1', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Filter Variable 1'), Choices = names(data), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
    })
    output$FilterVariable_2_2 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'FilterVariable_2_2', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Filter Variable 2'), Choices = names(data), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
    })
    output$FilterVariable_2_3 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'FilterVariable_2_3', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Filter Variable 3'), Choices = names(data), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
    })
    output$FilterVariable_2_4 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'FilterVariable_2_4', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Filter Variable 4'), Choices = names(data), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
    })

    # Filter Column Reactives
    FilterVariable_2_1 <- shiny::reactive({shiny::req(input[['FilterVariable_2_1']])})
    FilterVariable_2_2 <- shiny::reactive({shiny::req(input[['FilterVariable_2_2']])})
    FilterVariable_2_3 <- shiny::reactive({shiny::req(input[['FilterVariable_2_3']])})
    FilterVariable_2_4 <- shiny::reactive({shiny::req(input[['FilterVariable_2_4']])})

    # Filter Logic
    output$FilterLogic_2_1 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_2_1', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_2_1()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })
    output$FilterLogic_2_2 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_2_2', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_2_2()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })
    output$FilterLogic_2_3 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_2_3', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_2_3()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })
    output$FilterLogic_2_4 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_2_4', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_2_4()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })

    # Filter Values
    output$FilterValue_2_1_1 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_2_1']]}, error = function(x) NULL), type=1)
      choices <- tryCatch({RemixAutoML:::KeyVarsInit(data, VarName = input[['FilterVariable_2_1']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_2_1']])
      RemixAutoML:::SelectizeInput(Multiple = Mult, InputID='FilterValue_2_1_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML:::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })
    output$FilterValue_2_1_2 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_2_1']]}, error = function(x) NULL), type=2)
      choices <- tryCatch({RemixAutoML:::KeyVarsInit(data, VarName = input[['FilterVariable_2_1']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_2_1']])
      RemixAutoML:::SelectizeInput(Multiple = Mult, InputID='FilterValue_2_1_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML:::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })
    output$FilterValue_2_2_1 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_2_2']]}, error = function(x) NULL), type=1)
      choices <- tryCatch({RemixAutoML:::KeyVarsInit(data, VarName = input[['FilterVariable_2_2']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_2_2']])
      RemixAutoML:::SelectizeInput(Multiple = Mult, InputID='FilterValue_2_2_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML:::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })
    output$FilterValue_2_2_2 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_2_2']]}, error = function(x) NULL), type=2)
      choices <- tryCatch({RemixAutoML:::KeyVarsInit(data, VarName = input[['FilterVariable_2_2']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_2_2']])
      RemixAutoML:::SelectizeInput(Multiple = Mult, InputID='FilterValue_2_2_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML:::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })
    output$FilterValue_2_3_1 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_2_3']]}, error = function(x) NULL), type=1)
      choices <- tryCatch({RemixAutoML:::KeyVarsInit(data, VarName = input[['FilterVariable_2_3']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_2_3']])
      RemixAutoML:::SelectizeInput(Multiple = Mult, InputID='FilterValue_2_3_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML:::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })
    output$FilterValue_2_3_2 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_2_3']]}, error = function(x) NULL), type=2)
      choices <- tryCatch({RemixAutoML:::KeyVarsInit(data, VarName = input[['FilterVariable_2_3']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_2_3']])
      RemixAutoML:::SelectizeInput(Multiple = Mult, InputID='FilterValue_2_3_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML:::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })
    output$FilterValue_2_4_1 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_2_4']]}, error = function(x) NULL), type=1)
      choices <- tryCatch({RemixAutoML:::KeyVarsInit(data, VarName = input[['FilterVariable_2_4']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_2_4']])
      RemixAutoML:::SelectizeInput(Multiple = Mult, InputID='FilterValue_2_4_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML:::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })
    output$FilterValue_2_4_2 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_2_4']]}, error = function(x) NULL), type=2)
      choices <- tryCatch({RemixAutoML:::KeyVarsInit(data, VarName = input[['FilterVariable_2_4']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_2_4']])
      RemixAutoML:::SelectizeInput(Multiple = Mult, InputID='FilterValue_2_4_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML:::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })
  })

  # Filter Variables Plot 3
  shiny::observeEvent(input$FilterPlotInputs3, {

    # Plot 3
    print('Filter3_SelectData')
    ft3 <- shiny::reactive({shiny::req(tryCatch({DataList[[input$Plot3_SelectData]]}, error = function(x) DataList[[1L]]))})

    # Plot 3
    # Filter Columns
    output$FilterVariable_3_1 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'FilterVariable_3_1', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Filter Variable 1'), Choices = names(data), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
    })
    output$FilterVariable_3_2 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'FilterVariable_3_2', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Filter Variable 2'), Choices = names(data), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
    })
    output$FilterVariable_3_3 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'FilterVariable_3_3', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Filter Variable 3'), Choices = names(data), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
    })
    output$FilterVariable_3_4 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'FilterVariable_3_4', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Filter Variable 4'), Choices = names(data), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
    })

    # Filter Column Reactives
    FilterVariable_3_1 <- shiny::reactive({shiny::req(input[['FilterVariable_3_1']])})
    FilterVariable_3_2 <- shiny::reactive({shiny::req(input[['FilterVariable_3_2']])})
    FilterVariable_3_3 <- shiny::reactive({shiny::req(input[['FilterVariable_3_3']])})
    FilterVariable_3_4 <- shiny::reactive({shiny::req(input[['FilterVariable_3_4']])})

    # Filter Logic
    output$FilterLogic_3_1 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_3_1', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_3_1()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })
    output$FilterLogic_3_2 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_3_2', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_3_2()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })
    output$FilterLogic_3_3 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_3_3', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_3_3()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })
    output$FilterLogic_3_4 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_3_4', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_3_4()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })

    # Filter Values
    output$FilterValue_3_1_1 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_3_1']]}, error = function(x) NULL), type=1)
      choices <- tryCatch({RemixAutoML:::KeyVarsInit(data, VarName = input[['FilterVariable_3_1']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_3_1']])
      RemixAutoML:::SelectizeInput(Multiple = Mult, InputID='FilterValue_3_1_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML:::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })
    output$FilterValue_3_1_2 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_3_1']]}, error = function(x) NULL), type=2)
      choices <- tryCatch({RemixAutoML:::KeyVarsInit(data, VarName = input[['FilterVariable_3_1']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_3_1']])
      RemixAutoML:::SelectizeInput(Multiple = Mult, InputID='FilterValue_3_1_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML:::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })
    output$FilterValue_3_2_1 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_3_2']]}, error = function(x) NULL), type=1)
      choices <- tryCatch({RemixAutoML:::KeyVarsInit(data, VarName = input[['FilterVariable_3_2']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_3_2']])
      RemixAutoML:::SelectizeInput(Multiple = Mult, InputID='FilterValue_3_2_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML:::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })
    output$FilterValue_3_2_2 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_3_2']]}, error = function(x) NULL), type=2)
      choices <- tryCatch({RemixAutoML:::KeyVarsInit(data, VarName = input[['FilterVariable_3_2']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_3_2']])
      RemixAutoML:::SelectizeInput(Multiple = Mult, InputID='FilterValue_3_2_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML:::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })
    output$FilterValue_3_3_1 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_3_3']]}, error = function(x) NULL), type=1)
      choices <- tryCatch({RemixAutoML:::KeyVarsInit(data, VarName = input[['FilterVariable_3_3']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_3_3']])
      RemixAutoML:::SelectizeInput(Multiple = Mult, InputID='FilterValue_3_3_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML:::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })
    output$FilterValue_3_3_2 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_3_3']]}, error = function(x) NULL), type=2)
      choices <- tryCatch({RemixAutoML:::KeyVarsInit(data, VarName = input[['FilterVariable_3_3']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_3_3']])
      RemixAutoML:::SelectizeInput(Multiple = Mult, InputID='FilterValue_3_3_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML:::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })
    output$FilterValue_3_4_1 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_3_4']]}, error = function(x) NULL), type=1)
      choices <- tryCatch({RemixAutoML:::KeyVarsInit(data, VarName = input[['FilterVariable_3_4']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_3_4']])
      RemixAutoML:::SelectizeInput(Multiple = Mult, InputID='FilterValue_3_4_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML:::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })
    output$FilterValue_3_4_2 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_3_4']]}, error = function(x) NULL), type=2)
      choices <- tryCatch({RemixAutoML:::KeyVarsInit(data, VarName = input[['FilterVariable_3_4']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_3_4']])
      RemixAutoML:::SelectizeInput(Multiple = Mult, InputID='FilterValue_3_4_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML:::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })

  })

  # Filter Variables Plot 4
  shiny::observeEvent(input$FilterPlotInputs4, {

    # Plot 4
    print('Filter4_SelectData')
    ft4 <- shiny::reactive({shiny::req(tryCatch({DataList[[input$Plot4_SelectData]]}, error = function(x) DataList[[1L]]))})

    # Plot 4
    # Filter Columns
    output$FilterVariable_4_1 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'FilterVariable_4_1', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Filter Variable 1'), Choices = names(data), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
    })
    output$FilterVariable_4_2 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'FilterVariable_4_2', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Filter Variable 2'), Choices = names(data), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
    })
    output$FilterVariable_4_3 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'FilterVariable_4_3', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Filter Variable 3'), Choices = names(data), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
    })
    output$FilterVariable_4_4 <- shiny::renderUI({
      RemixAutoML:::SelectizeInput(InputID = 'FilterVariable_4_4', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Filter Variable 4'), Choices = names(data), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
    })

    # Filter Column Reactives
    FilterVariable_4_1 <- shiny::reactive({shiny::req(input[['FilterVariable_4_1']])})
    FilterVariable_4_2 <- shiny::reactive({shiny::req(input[['FilterVariable_4_2']])})
    FilterVariable_4_3 <- shiny::reactive({shiny::req(input[['FilterVariable_4_3']])})
    FilterVariable_4_4 <- shiny::reactive({shiny::req(input[['FilterVariable_4_4']])})

    # Filter Logic
    output$FilterLogic_4_1 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_4_1', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_4_1()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })
    output$FilterLogic_4_2 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_4_2', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_4_2()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })
    output$FilterLogic_4_3 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_4_3', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_4_3()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })
    output$FilterLogic_4_4 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_4_4', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_4_4()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })

    # Filter Values
    output$FilterValue_4_1_1 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_4_1']]}, error = function(x) NULL), type=1)
      choices <- tryCatch({RemixAutoML:::KeyVarsInit(data, VarName = input[['FilterVariable_4_1']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_4_1']])
      RemixAutoML:::SelectizeInput(Multiple = Mult, InputID='FilterValue_4_1_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML:::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })
    output$FilterValue_4_1_2 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_4_1']]}, error = function(x) NULL), type=2)
      choices <- tryCatch({RemixAutoML:::KeyVarsInit(data, VarName = input[['FilterVariable_4_1']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_4_1']])
      RemixAutoML:::SelectizeInput(Multiple = Mult, InputID='FilterValue_4_1_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML:::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })
    output$FilterValue_4_2_1 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_4_2']]}, error = function(x) NULL), type=1)
      choices <- tryCatch({RemixAutoML:::KeyVarsInit(data, VarName = input[['FilterVariable_4_2']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_4_2']])
      RemixAutoML:::SelectizeInput(Multiple = Mult, InputID='FilterValue_4_2_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML:::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })
    output$FilterValue_4_2_2 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_4_2']]}, error = function(x) NULL), type=2)
      choices <- tryCatch({RemixAutoML:::KeyVarsInit(data, VarName = input[['FilterVariable_4_2']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_4_2']])
      RemixAutoML:::SelectizeInput(Multiple = Mult, InputID='FilterValue_4_2_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML:::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })
    output$FilterValue_4_3_1 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_4_3']]}, error = function(x) NULL), type=1)
      choices <- tryCatch({RemixAutoML:::KeyVarsInit(data, VarName = input[['FilterVariable_4_3']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_4_3']])
      RemixAutoML:::SelectizeInput(Multiple = Mult, InputID='FilterValue_4_3_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML:::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })
    output$FilterValue_4_3_2 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_4_3']]}, error = function(x) NULL), type=2)
      choices <- tryCatch({RemixAutoML:::KeyVarsInit(data, VarName = input[['FilterVariable_4_3']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_4_3']])
      RemixAutoML:::SelectizeInput(Multiple = Mult, InputID='FilterValue_4_3_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML:::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })
    output$FilterValue_4_4_1 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_4_4']]}, error = function(x) NULL), type=1)
      choices <- tryCatch({RemixAutoML:::KeyVarsInit(data, VarName = input[['FilterVariable_4_4']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_4_4']])
      RemixAutoML:::SelectizeInput(Multiple = Mult, InputID='FilterValue_4_4_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML:::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })
    output$FilterValue_4_4_2 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_4_4']]}, error = function(x) NULL), type=2)
      choices <- tryCatch({RemixAutoML:::KeyVarsInit(data, VarName = input[['FilterVariable_4_4']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_4_4']])
      RemixAutoML:::SelectizeInput(Multiple = Mult, InputID='FilterValue_4_4_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML:::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })
  })

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # :: Obs Event Feature Engineering     ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  # Delete Columns
  shiny::observeEvent(input$FeatureEngineeringButton_DeleteFeatures, {
    x <- DataList[[shiny::req(input$DeleteVariables_SelectData)]]
    Cols <- RemixAutoML:::ReturnParam(xx = tryCatch({input$DeleteVariables}, error = function(x) NULL), Type = 'character', Default = NULL, VarName = NULL, Debug = Debug)
    if(length(Cols) != 0) {
      data.table::set(x, j = eval(Cols), value = NULL)
      DataList[[CurrentData]] <<- x
      DataList <<- DataList
      output$FE_DisplayData <- DT::renderDataTable({
        RemixAutoML::DataTable(dt[seq_len(min(.N, NNN))])
      })
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = 'success', btn_colors = 'green', html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  })

  # Concat Columns
  shiny::observeEvent(input$FeatureEngineeringButton_ConcatColumns, {
    Cols <- RemixAutoML:::ReturnParam(xx = tryCatch({input$ConcatColumns}, error = function(x) NULL), Type = 'character', Default = NULL, VarName = NULL, Debug = Debug)
    if(length(Cols) != 0) {
      x <- DataList[[eval(input$DeleteFeatures_SelectData)]]
      x[, paste0(Cols, collapse = '_') := do.call(paste, c(.SD, sep = ' ')), .SDcols = c(Cols)]
      DataList[[eval(input$DeleteFeatures_SelectData)]] <- x
      DataList <<- DataList
      output$FE_DisplayData <- DT::renderDataTable({
        RemixAutoML::DataTable(x[seq_len(min(.N, NNN))])
      })
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = 'success', btn_colors = 'green', html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  })

  # CalendarVariables()
  shiny::observeEvent(input$FeatureEngineeringButton_CalendarVariables, {
    print('FE Calendar Variables')
    CalendarVar_DateVariables <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['CalendarVariables_DateVariables']]}, error=function(x) NULL), VarName = NULL, Type = 'character', Default = NULL, Debug = Debug)
    if(length(CalendarVar_DateVariables) != 0) {
      CalendarVar_TimeUnits <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['CalendarVariables_TimeUnits']]}, error=function(x) NULL), VarName = NULL, Type = 'character', Default = NULL, Debug = Debug)
      x <- DataList[[eval(input$CalendarVariables_SelectData)]]
      x <- RemixAutoML::CreateCalendarVariables(
        data = x,
        DateCols = CalendarVar_DateVariables,
        AsFactor = FALSE,
        TimeUnits = CalendarVar_TimeUnits)
      DataList[[eval(input$CalendarVariables_SelectData)]] <- x
      DataList <<- DataList
      output$FE_DisplayData <- DT::renderDataTable({
        RemixAutoML::DataTable(x[seq_len(min(.N, NNN))])
      })
    }
    shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = 'success', btn_colors = 'green', html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
  })

  # HolidayVariables()
  shiny::observeEvent(input$FeatureEngineeringButton_HolidayVariables, {
    print('FE Holiday Variables')
    HolidayVar_DateVariables <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['HolidayVariables_DateVariables']]}, error=function(x) NULL), VarName = NULL, Type = 'character', Default = NULL, Debug = Debug)
    if(length(HolidayVar_DateVariables) != 0) {
      HolidayVar_HolidayGroups <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['HolidayVariables_HolidayGroups']]}, error=function(x) NULL), VarName = NULL, Type = 'character', Default = NULL, Debug = Debug)
      HolidayVar_LookbackDays <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['HolidayVariables_LookbackDays']]}, error=function(x) NULL), VarName = NULL, Type = 'numeric', Default = NULL, Debug = Debug)
      x <- DataList[[eval(input$HolidayVariables_SelectData)]]
      x <- RemixAutoML::CreateHolidayVariables(
        x,
        DateCols = HolidayVar_DateVariables,
        LookbackDays = HolidayVar_LookbackDays,
        HolidayGroups = HolidayVar_HolidayGroups,
        Holidays = NULL,
        Print = FALSE)
      DataList[[eval(input$HolidayVariables_SelectData)]] <- x
      DataList <<- DataList
      output$FE_DisplayData <- DT::renderDataTable({
        RemixAutoML::DataTable(x[seq_len(min(.N, NNN))])
      })
    }
    shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = 'success', btn_colors = 'green', html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
  })

  # PercentRank()
  shiny::observeEvent(input$FeatureEngineeringButton_PercRank, {
    print('FE Percent Rank')
    PercentRank_ColNames <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['PercentRank_ColNames']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    if(length(PercentRank_ColNames) != 0 && all(PercentRank_ColNames %in% names(DataList[[eval(input$PercRank_SelectData)]]))) {
      PercentRank_GroupVars <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['PercentRank_GroupVars']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      PercentRank_Granularity <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['PercentRank_Granularity']]}, error=function(x) NULL), Type = 'numeric', Default = NULL, Debug = Debug)
      x <- DataList[[eval(input$PercRank_SelectData)]]
      x <- RemixAutoML::PercRank(data = x, ColNames = PercentRank_ColNames, GroupVars = PercentRank_GroupVars, Granularity = PercentRank_Granularity)
      DataList[[eval(input$PercRank_SelectData)]] <- x
      DataList <<- DataList
      output$FE_DisplayData <- DT::renderDataTable({
        RemixAutoML::DataTable(x[seq_len(min(.N, NNN))])
      })
    }
    shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = 'success', btn_colors = 'green', html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
  })

  # AutoInteraction()
  shiny::observeEvent(input$FeatureEngineeringButton_Interaction, {
    print('FE Interaction')
    AutoInteraction_NumericVars <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoInteraction_NumericVars']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    if(length(AutoInteraction_NumericVars) != 0) {
      AutoInteraction_InteractionDepth <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoInteraction_InteractionDepth']]}, error=function(x) NULL), Type = 'numeric', Default = 2, Debug = Debug)
      AutoInteraction_Scale <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoInteraction_Scale']]}, error=function(x) NULL), Type = 'logical', Default = TRUE, Debug = Debug)
      AutoInteraction_Center <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoInteraction_Center']]}, error=function(x) NULL), Type = 'logical', Default = TRUE, Debug = Debug)
      x <- DataList[[eval(input$Interaction_SelectData)]]
      x <- RemixAutoML::AutoInteraction(
        data = x,
        NumericVars = AutoInteraction_NumericVars,
        InteractionDepth = AutoInteraction_InteractionDepth,
        Center = AutoInteraction_Center,
        Scale = AutoInteraction_Scale,
        SkipCols = NULL,
        Scoring = FALSE,
        File = NULL)
      DataList[[eval(input$Interaction_SelectData)]] <- x
      DataList <<- DataList
      output$FE_DisplayData <- DT::renderDataTable({
        RemixAutoML::DataTable(x[seq_len(min(.N, NNN))])
      })
    }
    shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = 'success', btn_colors = 'green', html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
  })

  # AutoTransformationCreate()
  shiny::observeEvent(input$FeatureEngineeringButton_Transformations, {
    print('FE Transformations')
    AutoTransformationCreate_ColumnNames <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoTransformationCreate_ColumnNames']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    if(length(AutoTransformationCreate_ColumnNames) != 0) {
      AutoTransformationCreate_Methods <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoTransformationCreate_Methods']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      x <- DataList[[eval(input$AutoTransformationCreate_SelectData)]]
      x <- RemixAutoML::AutoTransformationCreate(
        data = x,
        ColumnNames = AutoTransformationCreate_ColumnNames,
        Methods = AutoTransformationCreate_Methods,
        Path = NULL,
        TransID = "ModelID",
        SaveOutput = FALSE)$Data
      DataList[[eval(input$AutoTransformationCreate_SelectData)]] <- x
      DataList <<- DataList
      output$FE_DisplayData <- DT::renderDataTable({
        RemixAutoML::DataTable(x[seq_len(min(.N, NNN))])
      })
    }
    shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = 'success', btn_colors = 'green', html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
  })

  # DummifyDT()
  shiny::observeEvent(input$FeatureEngineeringButton_PartialDummies, {
    print('FE Partial Dummies')
    DummifyDT_Cols <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['DummifyDT_Cols']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    if(length(DummifyDT_Cols) != 0) {
      DummifyDT_TopN <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['DummifyDT_TopN']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      DummifyDT_KeepBaseCols <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['DummifyDT_KeepBaseCols']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      x <- DataList[[eval(input$PartialDummies_SelectData)]]
      x <- RemixAutoML::DummifyDT(
        data = x,
        cols = DummifyDT_Cols,
        TopN = DummifyDT_TopN,
        KeepFactorCols = as.logical(DummifyDT_KeepBaseCols),
        OneHot=FALSE, SaveFactorLevels=FALSE, SavePath=NULL, ImportFactorLevels=FALSE, FactorLevelsList=NULL, ClustScore=FALSE, ReturnFactorLevels=FALSE, GroupVar=FALSE)
      DataList[[eval(input$PartialDummies_SelectData)]] <- x
      DataList <<- DataList
      output$FE_DisplayData <- DT::renderDataTable({
        RemixAutoML::DataTable(x[seq_len(min(.N, NNN))])
      })
    }
    shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = 'success', btn_colors = 'green', html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
  })

  # CategoricalEncoding()
  shiny::observeEvent(input$FeatureEngineeringButton_CategoricalEncoding, {
    print('FE Categorical Encoding')
    CategoricalEncoding_GroupVariables <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['CategoricalEncoding_GroupVariables']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    if(length(CategoricalEncoding_GroupVariables) != 0) {
      CategoricalEncoding_TargetVariable <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['CategoricalEncoding_TargetVariable']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      CategoricalEncoding_Method <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['CategoricalEncoding_Method']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      x <- DataList[[eval(input$CategoricalEncoding_SelectData)]]
      x <- RemixAutoML::CategoricalEncoding(
        data = x,
        ML_Type = "classification",
        GroupVariables = CategoricalEncoding_GroupVariables,
        TargetVariable = CategoricalEncoding_TargetVariable,
        Method = CategoricalEncoding_Method,
        SavePath=NULL, Scoring=FALSE, ImputeValueScoring=NULL, ReturnFactorLevelList=TRUE, SupplyFactorLevelList=NULL, KeepOriginalFactors=TRUE)$data
      DataList[[eval(input$CategoricalEncoding_SelectData)]] <- x
      DataList <<- DataList
      output$FE_DisplayData <- DT::renderDataTable({
        RemixAutoML::DataTable(x[seq_len(min(.N, NNN))])
      })
    }
    shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = 'success', btn_colors = 'green', html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
  })

  # AutoLagRollMode()
  shiny::observeEvent(input$FeatureEngineeringButton_AutoLagRollMode, {
    print('FE Auto Lag Roll Mode')
    AutoLagRollMode_Targets <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoLagRollMode_Targets']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    if(length(AutoLagRollMode_Targets) != 0) {
      AutoLagRollMode_Lags <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoLagRollMode_Lags', Type = 'numeric']]}, error=function(x) NULL), Default = NULL, Debug = Debug)
      AutoLagRollMode_ModePeriods <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoLagRollMode_ModePeriods']]}, error=function(x) NULL), Type = 'numeric', Default = NULL, Debug = Debug)
      AutoLagRollMode_GroupingVars <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoLagRollMode_GroupingVars']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      AutoLagRollMode_SortDateName <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoLagRollMode_SortDateName']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      AutoLagRollMode_WindowingLag <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoLagRollMode_WindowingLag']]}, error=function(x) NULL), Type = 'numeric', Default = NULL, Debug = Debug)
      x <- DataList[[eval(input$AutoLagRollMode_SelectData)]]
      x <- RemixAutoML::AutoLagRollMode(
        data = x,
        Lags = AutoLagRollMode_Lags,
        ModePeriods = AutoLagRollMode_ModePeriods,
        Targets = AutoLagRollMode_Targets,
        GroupingVars = AutoLagRollMode_GroupingVars,
        SortDateName = AutoLagRollMode_SortDateName,
        WindowingLag = AutoLagRollMode_WindowingLag,
        Type = c("Lag"),
        SimpleImpute = TRUE)
      DataList[[eval(input$AutoLagRollMode_SelectData)]] <- x
      DataList <<- DataList
      output$FE_DisplayData <- DT::renderDataTable({
        RemixAutoML::DataTable(x[seq_len(min(.N, NNN))])
      })
    }
    shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = 'success', btn_colors = 'green', html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
  })

  # AutoLagRollStats()
  shiny::observeEvent(input$FeatureEngineeringButton_AutoLagRollStats, {
    print('FE Auto Lag Roll Stats')
    AutoLagRollStats_Targets <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoLagRollStats_Targets']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    AutoLagRollStats_DateColumn <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoLagRollStats_DateColumn']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    AutoLagRollStats_TimeUnits <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoLagRollStats_TimeUnits']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    print(AutoLagRollStats_Targets)
    print(AutoLagRollStats_DateColumn)
    print(AutoLagRollStats_TimeUnits)
    if(length(AutoLagRollStats_Targets) != 0 &&
       length(AutoLagRollStats_DateColumn) != 0 &&
       length(AutoLagRollStats_TimeUnits) != 0) {
      x <- DataList[[eval(input$AutoLagRollStats_SelectData)]]
      AutoLagRollStats_GroupVars <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoLagRollStats_GroupVars']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      if(length(AutoLagRollStats_GroupVars) > 1) {
        x[, paste0(AutoLagRollStats_GroupVars, collapse = '_') := do.call(paste, c(.SD, sep = ' ')), .SDcols = c(AutoLagRollStats_GroupVars)]
        AutoLagRollStats_GroupVars <- paste0(AutoLagRollStats_GroupVars, collapse = '_')
      }
      AutoLagRollStats_Lags <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoLagRollStats_Lags']]}, error=function(x) NULL), Type = 'numeric', Default = NULL, Debug = Debug)
      AutoLagRollStats_RollOnLag1 <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoLagRollStats_RollOnLag1']]}, error=function(x) NULL), Type = 'logical', Default = NULL, Debug = Debug)
      AutoLagRollStats_MA_RollWindows <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoLagRollStats_MA_RollWindows']]}, error=function(x) NULL), Type = 'numeric', Default = NULL, Debug = Debug)
      AutoLagRollStats_SD_RollWindows <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoLagRollStats_SD_RollWindows']]}, error=function(x) NULL), Type = 'numeric', Default = NULL, Debug = Debug)
      AutoLagRollStats_Skew_RollWindows <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoLagRollStats_Skew_RollWindows']]}, error=function(x) NULL), Type = 'numeric', Default = NULL, Debug = Debug)
      AutoLagRollStats_Kurt_RollWindows <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoLagRollStats_Kurt_RollWindows']]}, error=function(x) NULL), Type = 'numeric', Default = NULL, Debug = Debug)
      AutoLagRollStats_Quantile_RollWindows <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoLagRollStats_Quantile_RollWindows']]}, error=function(x) NULL), Type = 'numeric', Default = NULL, Debug = Debug)
      AutoLagRollStats_Quantiles_Selected <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoLagRollStats_Quantiles_Selected']]}, error=function(x) NULL), Type = 'numeric', Default = NULL, Debug = Debug)
      x <- RemixAutoML::AutoLagRollStats(
        data                 = x,
        Targets              = AutoLagRollStats_Targets,
        HierarchyGroups      = NULL,
        IndependentGroups    = AutoLagRollStats_GroupVars,
        DateColumn           = AutoLagRollStats_DateColumn,
        TimeUnit             = AutoLagRollStats_TimeUnits,
        TimeUnitAgg          = AutoLagRollStats_TimeUnits,
        TimeGroups           = AutoLagRollStats_TimeUnits,
        TimeBetween          = NULL,
        RollOnLag1           = AutoLagRollStats_RollOnLag1,
        Type                 = "Lag",
        SimpleImpute         = TRUE,
        Lags                 = AutoLagRollStats_Lags,
        MA_RollWindows       = AutoLagRollStats_MA_RollWindows,
        SD_RollWindows       = AutoLagRollStats_SD_RollWindows,
        Skew_RollWindows     = AutoLagRollStats_Skew_RollWindows,
        Kurt_RollWindows     = AutoLagRollStats_Kurt_RollWindows,
        Quantile_RollWindows = AutoLagRollStats_Quantile_RollWindows,
        Quantiles_Selected   = AutoLagRollStats_Quantiles_Selected,
        Debug = Debug)
      if(length(AutoLagRollStats_GroupVars) > 1) {
        data.table::set(x, j = paste0(AutoLagRollStats_GroupVars, collapse = '_'), value = NULL)
      }
      DataList[[eval(input$AutoLagRollStats_SelectData)]] <- x
      DataList <<- DataList
      output$FE_DisplayData <- DT::renderDataTable({
        RemixAutoML::DataTable(x[seq_len(min(.N, NNN))])
      })
    }
    shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = 'success', btn_colors = 'green', html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
  })

  # AutoDiffLagN()
  shiny::observeEvent(input$FeatureEngineeringButton_AutoDiff, {
    print('FE Auto Diff')
    AutoDiffLagN_DateVariable <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoDiffLagN_DateVariable']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    AutoDiffLagN_NLag1 <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoDiffLagN_NLag1']]}, error=function(x) NULL), Type = 'numeric', Default = NULL, Debug = Debug)
    AutoDiffLagN_NLag2 <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoDiffLagN_NLag2']]}, error=function(x) NULL), Type = 'numeric', Default = NULL, Debug = Debug)
    if(length(AutoDiffLagN_DateVariable) != 0 &&
       length(AutoDiffLagN_NLag1) != 0 &&
       length(AutoDiffLagN_NLag2) != 0) {
      AutoDiffLagN_GroupVariables <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoDiffLagN_GroupVariables']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      AutoDiffLagN_DiffVariables <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoDiffLagN_DiffVariables']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      AutoDiffLagN_DiffDateVariables <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoDiffLagN_DiffDateVariables']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      AutoDiffLagN_DiffGroupVariables <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoDiffLagN_DiffGroupVariables']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      x <- DataList[[eval(input$AutoDiffLagN_SelectData)]]
      x <- RemixAutoML::AutoDiffLagN(
        data = x,
        DateVariable = AutoDiffLagN_DateVariable,
        GroupVariables = AutoDiffLagN_GroupVariables,
        DiffVariables = AutoDiffLagN_DiffVariables,
        DiffDateVariables = AutoDiffLagN_DiffDateVariables,
        DiffGroupVariables = AutoDiffLagN_DiffGroupVariables,
        NLag1 = AutoDiffLagN_NLag1,
        NLag2 = AutoDiffLagN_NLag2,
        Sort = FALSE,
        RemoveNA = TRUE)
      DataList[[eval(input$AutoDiffLagN_SelectData)]] <- x
      DataList <<- DataList
      output$FE_DisplayData <- DT::renderDataTable({
        RemixAutoML::DataTable(x[seq_len(min(.N, NNN))])
      })
    }
    shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = 'success', btn_colors = 'green', html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
  })

  # ModelDataPrep()
  shiny::observeEvent(input$FeatureEngineeringButton_ModelDataPrep, {
    print('FE Type Conversion')
    ModelDataPrep_IgnoreCols <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['ModelDataPrep_IgnoreCols']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    ModelDataPrep_CharToFactor <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['ModelDataPrep_CharToFactor']]}, error=function(x) NULL), Type = 'logical', Default = FALSE, Debug = Debug)
    ModelDataPrep_FactorToChar <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['ModelDataPrep_FactorToChar']]}, error=function(x) NULL), Type = 'logical', Default = FALSE, Debug = Debug)
    ModelDataPrep_DateToChar <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['ModelDataPrep_DateToChar']]}, error=function(x) NULL), Type = 'logical', Default = FALSE, Debug = Debug)
    ModelDataPrep_IDateConversion <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['ModelDataPrep_IDateConversion']]}, error=function(x) NULL), Type = 'logical', Default = FALSE, Debug = Debug)
    ModelDataPrep_RemoveDates <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['ModelDataPrep_RemoveDates']]}, error=function(x) NULL), Type = 'logical', Default = FALSE, Debug = Debug)
    ModelDataPrep_IntToNumeric <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['ModelDataPrep_IntToNumeric']]}, error=function(x) NULL), Type = 'logical', Default = FALSE, Debug = Debug)
    ModelDataPrep_LogicalToBinary <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['ModelDataPrep_LogicalToBinary']]}, error=function(x) NULL), Type = 'logical', Default = FALSE, Debug = Debug)
    ModelDataPrep_MissFactor <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['ModelDataPrep_MissFactor']]}, error=function(x) NULL), Type = 'character', Default = FALSE, Debug = Debug)
    ModelDataPrep_MissNum <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['ModelDataPrep_MissNum']]}, error=function(x) NULL), Type = 'numeric', Default = FALSE, Debug = Debug)
    x <- DataList[[eval(input$ModelDataPrep_SelectData)]]
    x <- RemixAutoML::ModelDataPrep(
      x,
      Impute          = FALSE,
      CharToFactor    = ModelDataPrep_CharToFactor,
      FactorToChar    = ModelDataPrep_FactorToChar,
      IntToNumeric    = ModelDataPrep_IntToNumeric,
      LogicalToBinary = ModelDataPrep_LogicalToBinary,
      DateToChar      = ModelDataPrep_DateToChar,
      IDateConversion = ModelDataPrep_IDateConversion,
      RemoveDates     = ModelDataPrep_RemoveDates,
      MissFactor      = ModelDataPrep_MissFactor,
      MissNum         = ModelDataPrep_MissNum,
      IgnoreCols      = ModelDataPrep_IgnoreCols)
    DataList[[eval(input$ModelDataPrep_SelectData)]] <- x
    DataList <<- DataList
    output$FE_DisplayData <- DT::renderDataTable({
      RemixAutoML::DataTable(x[seq_len(min(.N, NNN))])
    })
    shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = 'success', btn_colors = 'green', html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
  })

  # AutoDataPartition()
  shiny::observeEvent(input$FeatureEngineeringButton_AutoDataPartition, {
    print('FE Data Partition')
    AutoDataPartition_NumDataSets <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoDataPartition_NumDataSets']]}, error=function(x) NULL), Type = 'numeric', Default = 3L, Debug = Debug)
    AutoDataPartition_Ratios_Train <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoDataPartition_Ratios_Train']]}, error=function(x) NULL), Type = 'numeric', Default = c(0.70), Debug = Debug)
    AutoDataPartition_Ratios_Validation <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoDataPartition_Ratios_Validation']]}, error=function(x) NULL), Type = 'numeric', Default = c(0.20), Debug = Debug)
    AutoDataPartition_Ratios_Test <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoDataPartition_Ratios_Test']]}, error=function(x) NULL), Type = 'numeric', Default = c(0.10), Debug = Debug)
    AutoDataPartition_PartitionType <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoDataPartition_PartitionType']]}, error=function(x) NULL), Type = 'character', Default = "random", Debug = Debug)
    AutoDataPartition_StratifyColumnNames <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoDataPartition_StratifyColumnNames']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    AutoDataPartition_TimeColumnName <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoDataPartition_TimeColumnName']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    x <- DataList[[eval(input$AutoDataPartition_SelectData)]]
    DataSets <- RemixAutoML::AutoDataPartition(
      x,
      NumDataSets = AutoDataPartition_NumDataSets,
      Ratios = c(AutoDataPartition_Ratios_Train, AutoDataPartition_Ratios_Validation, AutoDataPartition_Ratios_Test),
      PartitionType = AutoDataPartition_PartitionType,
      StratifyColumnNames = AutoDataPartition_StratifyColumnNames,
      TimeColumnName = AutoDataPartition_TimeColumnName)
    DataList[['TrainData']] <- DataSets[['TrainData']]
    DataList[['ValidationData']] <- DataSets[['ValidationData']]
    DataList[['TestData']] <- DataSets[['TestData']]
    DataList <<- DataList
    output$FE_DisplayData <- DT::renderDataTable({
      RemixAutoML::DataTable(x[seq_len(min(.N, NNN))])
    })
    shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = 'success', btn_colors = 'green', html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
  })

  # TODO:
  # Text Variables, Dim Reduction, Clustering, Anomaly Detection

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # :: Obs Event Machine Learning        ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  shiny::observeEvent(input$BuildModels, {

    print(' ::  BuildModels 0  :: ')

    # Model Lists
    CatBoostArgsList <- list()
    XGBoostArgsList <- list()
    LightGBMArgsList <- list()

    # Catboost check
    temp <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_data']]}, error=function(x) NULL), Type='character', Default=NULL)
    if(length(temp) != 0) CatBoostArgsList[['data']] <- DataList[[temp]] else CatBoostArgsList[['data']] <- NULL
    CatBoostArgsList[['TargetColumnName']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_TargetColumnName']]}, error=function(x) NULL), Type='character', Default=NULL)
    CatBoostArgsList[['FeatureColNames']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_FeatureColNames']]}, error=function(x) NULL), Type='character', Default=NULL)
    print("class(CatBoostArgsList[['data']])[[1L]] %in% 'data.table' && length(CatBoostArgsList[['TargetColumnName']]) != 0L && length(CatBoostArgsList[['FeatureColNames']]) != 0L")
    if(class(CatBoostArgsList[['data']])[[1L]] %in% 'data.table' && length(CatBoostArgsList[['TargetColumnName']]) != 0L && length(CatBoostArgsList[['FeatureColNames']]) != 0L) {
      CatBoostRun <- TRUE
    } else {
      CatBoostRun <- FALSE
    }

    print(' ::  BuildModels 1  :: ')

    # CatBoost ML Build
    if(CatBoostRun) {

      # Target Type
      CatBoost_TargetType <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_TargetType']]}, error=function(x) NULL), Type='character', Default='MultiClass')
      print(paste0('CatBoost_TargetType = ', CatBoost_TargetType))

      # Notification of starting
      shiny::showNotification('CatBoost Building has Begun!')

      # CatBoost MetaData Parameters
      CatBoostArgsList[['OutputSelection']] <- c('Importances','EvalMetrics','EvalPlots','Score_TrainData')
      CatBoostArgsList[['task_type']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_task_type']]}, error=function(x) NULL), Type='character', Default='CPU')
      CatBoostArgsList[['NumGPUs']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_NumGPUs']]}, error=function(x) NULL), Type='numeric', Default=1)
      CatBoostArgsList[['TrainOnFull']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_TrainOnFull']]}, error=function(x) NULL), Type='logical', Default=FALSE)
      CatBoostArgsList[['ModelID']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_ModelID']]}, error=function(x) NULL), Type='character', Default='Model_1')
      CatBoostArgsList[['DebugMode']] <- Debug
      CatBoostArgsList[['model_path']] <- getwd()
      CatBoostArgsList[['metadata_path']] <- getwd()
      CatBoostArgsList[['SaveModelObjects']] <- FALSE
      if(CatBoost_TargetType != 'MultiClass') CatBoostArgsList[['SaveInfoToPDF']] <- FALSE
      CatBoostArgsList[['ReturnModelObjects']] <- TRUE
      CatBoostArgsList[['NumOfParDepPlots']] <- 1
      CatBoostArgsList[['sampling_unit']] <- 'Object'

      print(' ::  BuildModels 2  :: ')

      # CatBoost Data Parameters

      # ValidationData
      temp <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_ValidationData']]}, error=function(x) NULL), Type='character', Default=NULL)
      if(Debug) {print("RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_ValidationData']]}, error=function(x) NULL), Type='character', Default=NULL)"); print(temp)}
      if(length(temp) != 0) CatBoostArgsList[['ValidationData']] <- DataList[[temp]] else CatBoostArgsList[['ValidationData']] <- NULL

      print(' ::  BuildModels 3  :: ')

      # TestData
      temp <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_TestData']]}, error=function(x) NULL), Type='character', Default=NULL)
      if(Debug) {print("RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_TestData']]}, error=function(x) NULL), Type='character', Default=NULL)"); print(temp)}
      if(length(temp) != 0) CatBoostArgsList[['TestData']] <- DataList[[temp]] else CatBoostArgsList[['TestData']] <- NULL

      print(' ::  BuildModels 4  :: ')

      CatBoostArgsList[['PrimaryDateColumn']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_PrimaryDateColumn']]}, error=function(x) NULL), Type='character', Default=NULL)
      CatBoostArgsList[['EncodeMethod']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_EncodeMethod']]}, error=function(x) NULL), Type='character', Default='credibility')
      CatBoostArgsList[['WeightsColumnName']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_WeightsColumnName']]}, error=function(x) NULL), Type='character', Default=NULL)
      CatBoostArgsList[['IDcols']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_IDcols']]}, error=function(x) NULL), Type='character', Default=NULL)
      CatBoostArgsList[['TransformNumericColumns']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_TransformNumericColumns']]}, error=function(x) NULL), Type='character', Default=NULL)
      CatBoostArgsList[['Methods']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_Methods']]}, error=function(x) NULL), Type='character', Default=NULL)

      print(' ::  BuildModels 5  :: ')

      # CatBoost Grid Tuning Parameters
      CatBoostArgsList[['PassInGrid']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_PassInGrid']]}, error=function(x) NULL), Type='character', Default=NULL)
      CatBoostArgsList[['GridTune']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_GridTune']]}, error=function(x) NULL), Type='logical', Default=FALSE)
      CatBoostArgsList[['MaxModelsInGrid']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_MaxModelsInGrid']]}, error=function(x) NULL), Type='numeric', Default=30)
      CatBoostArgsList[['MaxRunsWithoutNewWinner']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_MaxRunsWithoutNewWinner']]}, error=function(x) NULL), Type='numeric', Default=15)
      CatBoostArgsList[['MaxRunMinutes']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_MaxRunMinutes']]}, error=function(x) NULL), Type='numeric', Default=30)
      CatBoostArgsList[['BaselineComparison']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_BaselineComparison']]}, error=function(x) NULL), Type='character', Default='default')

      print(' ::  BuildModels 6  :: ')

      # CatBoost ML Parameters
      CatBoostArgsList[['Trees']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_Trees']]}, error=function(x) NULL), Type='numeric', Default=1000)
      CatBoostArgsList[['Depth']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_Depth']]}, error=function(x) NULL), Type='numeric', Default=8)
      CatBoostArgsList[['LearningRate']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_LearningRate']]}, error=function(x) NULL), Type='numeric', Default=NULL)
      CatBoostArgsList[['L2_Leaf_Reg']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_L2_Leaf_Reg']]}, error=function(x) NULL), Type='numeric', Default=NULL)
      CatBoostArgsList[['model_size_reg']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_model_size_reg']]}, error=function(x) NULL), Type='numeric', Default=0.50)
      CatBoostArgsList[['langevin']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_langevin']]}, error=function(x) NULL), Type='logical', Default=FALSE)
      CatBoostArgsList[['diffusion_temperature']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_diffusion_temperature']]}, error=function(x) NULL), Type='numeric', Default=10000)
      CatBoostArgsList[['RandomStrength']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_RandomStrength']]}, error=function(x) NULL), Type='numeric', Default=1)
      CatBoostArgsList[['BorderCount']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_BorderCount']]}, error=function(x) NULL), Type='numeric', Default=256)
      CatBoostArgsList[['RSM']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_RSM']]}, error=function(x) NULL), Type='numeric', Default=1)
      CatBoostArgsList[['BootStrapType']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_BootStrapType']]}, error=function(x) NULL), Type='character', Default='Bayesian')
      CatBoostArgsList[['GrowPolicy']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_GrowPolicy']]}, error=function(x) NULL), Type='character', Default='SymmetricTree')
      CatBoostArgsList[['feature_border_type']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_feature_border_type']]}, error=function(x) NULL), Type='character', Default='GreedyLogSum')
      CatBoostArgsList[['subsample']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_subsample']]}, error=function(x) NULL), Type='numeric', Default=1)
      CatBoostArgsList[['score_function']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_score_function']]}, error=function(x) NULL), Type='character', Default='Cosine')
      CatBoostArgsList[['min_data_in_leaf']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_min_data_in_leaf']]}, error=function(x) NULL), Type='numeric', Default=1)

      print(' ::  BuildModels 7  :: ')

      # CatBoost Eval Parameters
      if(CatBoost_TargetType == 'Regression') {
        print(' ::  BuildModels 8 :: ')
        CatBoostArgsList[['loss_function']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_LossFunction']]}, error=function(x) NULL), Type='character', Default='RMSE')
        CatBoostArgsList[['loss_function_value']] <- 1.5
        CatBoostArgsList[['eval_metric']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_EvalMetric']]}, error=function(x) NULL), Type='character', Default='RMSE')
        CatBoostArgsList[['eval_metric_value']] <- 1.5
        CatBoostArgsList[['grid_eval_metric']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_grid_eval_metric']]}, error=function(x) NULL), Type='character', Default='mse')
        CatBoostArgsList[['MetricPeriods']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_MetricPeriods']]}, error=function(x) NULL), Type='numeric', Default=10)
      } else if(CatBoost_TargetType == 'Binary Classification') {
        print(' ::  BuildModels 8 :: ')
        CatBoostArgsList[['LossFunction']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_LossFunction']]}, error=function(x) NULL), Type='character', Default='Logloss')
        CatBoostArgsList[['EvalMetric']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_EvalMetric']]}, error=function(x) NULL), Type='character', Default='AUC')
        CatBoostArgsList[['grid_eval_metric']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_grid_eval_metric']]}, error=function(x) NULL), Type='character', Default='MCC')
        cw0 <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_ClassWeights0']]}, error=function(x) NULL), Type='numeric', Default=1)
        cw1 <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_ClassWeights1']]}, error=function(x) NULL), Type='numeric', Default=1)
        CatBoostArgsList[['ClassWeights']] <- c(cw0, cw1)
        CatBoostArgsList[['MetricPeriods']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_MetricPeriods']]}, error=function(x) NULL), Type='numeric', Default=10)
      } else if(CatBoost_TargetType == 'MultiClass') {
        print(' ::  BuildModels 8 :: ')
        CatBoostArgsList[['loss_function']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_LossFunction']]}, error=function(x) NULL), Type='character', Default='MultiClassOneVsAll')
        CatBoostArgsList[['eval_metric']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_EvalMetric']]}, error=function(x) NULL), Type='character', Default='MultiClassOneVsAll')
        CatBoostArgsList[['grid_eval_metric']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_grid_eval_metric']]}, error=function(x) NULL), Type='character', Default='microauc')
        CatBoostArgsList[['MetricPeriods']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_MetricPeriods']]}, error=function(x) NULL), Type='numeric', Default=10)
      }

      # Build model
      if(CatBoost_TargetType == 'Regression') {
        print(' ::  BuildModels 9 :: ')
        if(Debug) {print("CatBoostArgsList[['data']]");print(CatBoostArgsList[['data']]);print("CatBoostArgsList[['ValidationData']]");print(CatBoostArgsList[['ValidationData']]);print("CatBoostArgsList[['TestData']]");print(CatBoostArgsList[['TestData']])  }
        ModelOutputList <<- do.call(what = RemixAutoML::AutoCatBoostRegression, args = CatBoostArgsList)
      }
      if(CatBoost_TargetType == 'Binary Classification') {
        print(' ::  BuildModels 9 :: ')
        if(Debug) {print("CatBoostArgsList[['data']]");print(CatBoostArgsList[['data']]);print("CatBoostArgsList[['ValidationData']]");print(CatBoostArgsList[['ValidationData']]);print("CatBoostArgsList[['TestData']]");print(CatBoostArgsList[['TestData']])  }
        ModelOutputList <<- do.call(RemixAutoML::AutoCatBoostClassifier, CatBoostArgsList)
      }
      if(CatBoost_TargetType == 'MultiClass') {
        print(' ::  BuildModels 9 :: ')
        if(Debug) {print("CatBoostArgsList[['data']]");print(CatBoostArgsList[['data']]);print("CatBoostArgsList[['ValidationData']]");print(CatBoostArgsList[['ValidationData']]);print("CatBoostArgsList[['TestData']]");print(CatBoostArgsList[['TestData']])  }
        ModelOutputList <<- do.call(RemixAutoML::AutoCatBoostMultiClass, CatBoostArgsList)
      }

      # Store in DataList
      print(' ::  BuildModels 10 :: ')
      KeyName <- CatBoostArgsList[['ModelID']]
      Output <- RemixAutoML:::ModelDataObjects(ModelOutputList, TT = 'catboost')
      DataList[[paste0(KeyName, '_ScoringData')]] <- Output$ScoringDataCombined
      DataList[[paste0(KeyName, '_Test_VI_Data')]] <- Output$VI_Train
      DataList[[paste0(KeyName, '_Train_VI_Data')]] <- Output$VI_Validation
      DataList[[paste0(KeyName, '_Validation_VI_Data')]] <- Output$VI_Test
      DataList[[paste0(KeyName, '_All_II_Data')]] <- Output$II_Train
      rm(Output); gc()

      # Save output
      print(' ::  BuildModels 11  :: ')
      DataList <<- DataList
      CatBoostArgsList <<- CatBoostArgsList
    }

    # XGBoost check
    temp <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_data']]}, error=function(x) NULL), Type='character', Default=NULL)
    if(length(temp) != 0) XGBoostArgsList[['data']] <- DataList[[temp]] else XGBoostArgsList[['data']] <- NULL
    XGBoostArgsList[['TargetColumnName']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_TargetColumnName']]}, error=function(x) NULL), Type='character', Default=NULL)
    XGBoostArgsList[['FeatureColNames']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_FeatureColNames']]}, error=function(x) NULL), Type='character', Default=NULL)
    print("class(XGBoostArgsList[['data']])[[1L]] %in% 'data.table' && length(XGBoostArgsList[['TargetColumnName']]) != 0L && length(XGBoostArgsList[['FeatureColNames']]) != 0L")
    if(class(XGBoostArgsList[['data']])[[1L]] %in% 'data.table' && length(XGBoostArgsList[['TargetColumnName']]) != 0L && length(XGBoostArgsList[['FeatureColNames']]) != 0L) {
      print("length(XGBoostArgsList[['data']])[[1L]] != 0L && length(XGBoostArgsList[['TargetColumnName']]) != 0L && length(XGBoostArgsList[['FeatureColNames']]) != 0L == TRUE")
      XGBoostRun <- TRUE
    } else {
      print("length(XGBoostArgsList[['data']])[[1L]] != 0L && length(XGBoostArgsList[['TargetColumnName']]) != 0L && length(XGBoostArgsList[['FeatureColNames']]) != 0L == FALSE")
      print(XGBoostArgsList[['data']])
      print(XGBoostArgsList[['TargetColumnName']])
      print(XGBoostArgsList[['FeatureColNames']])
      XGBoostRun <- FALSE
    }

    # XGBoost ML Build
    if(XGBoostRun) {

      print(' ::  BuildModels 0 :: ')

      XGBoost_TargetType <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_TargetType']]}, error=function(x) NULL), Type='character', Default='MultiClass')

      # Notification of starting
      shiny::showNotification('XGBoost Building has Begun!')

      # Constants for now
      XGBoostArgsList[['Verbose']] <- 0L
      XGBoostArgsList[['ReturnModelObjects']] <- TRUE
      XGBoostArgsList[['NumOfParDepPlots']] <- 1L

      # XGBoost MetaData Parameters
      XGBoostArgsList[['DebugMode']] <- Debug
      XGBoostArgsList[['NThreads']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_NThreads']]}, error=function(x) NULL), Type='numeric', Default=-1)
      XGBoostArgsList[['OutputSelection']] <- c('Importances','EvalMetrics','Score_TrainData')
      XGBoostArgsList[['TrainOnFull']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_TrainOnFull']]}, error=function(x) NULL), Type='logical', Default=FALSE)
      XGBoostArgsList[['ModelID']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_ModelID']]}, error=function(x) NULL), Type='character', Default='Model_1')

      print(' ::  BuildModels 1 :: ')

      # XGBoost Data Parameters

      # ValidationData
      temp <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_ValidationData']]}, error=function(x) NULL), Type='character', Default=NULL)
      if(Debug) {print("RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_ValidationData']]}, error=function(x) NULL), Type='character', Default=NULL)"); print(temp)}
      if(length(temp) != 0) XGBoostArgsList[['ValidationData']] <- DataList[[temp]] else XGBoostArgsList[['ValidationData']] <- NULL

      print(' ::  BuildModels 3  :: ')

      # TestData
      temp <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_TestData']]}, error=function(x) NULL), Type='character', Default=NULL)
      if(Debug) {print("RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_TestData']]}, error=function(x) NULL), Type='character', Default=NULL)"); print(temp)}
      if(length(temp) != 0) XGBoostArgsList[['TestData']] <- DataList[[temp]] else XGBoostArgsList[['TestData']] <- NULL

      print(' ::  BuildModels 4  :: ')

      XGBoostArgsList[['WeightsColumnName']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_WeightsColumnName']]}, error=function(x) NULL), Type='character', Default=NULL)
      XGBoostArgsList[['IDcols']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_IDcols']]}, error=function(x) NULL), Type='character', Default=NULL)
      XGBoostArgsList[['EncodingMethod']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_EncodingMethod']]}, error=function(x) NULL), Type='character', Default='credibility')
      XGBoostArgsList[['TransformNumericColumns']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_TransformNumericColumns']]}, error=function(x) NULL), Type='character', Default=NULL)
      XGBoostArgsList[['Methods']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_Methods']]}, error=function(x) NULL), Type='character', Default=NULL)

      print(' ::  BuildModels 5 :: ')

      # XGBoost Grid Tuning Parameters
      XGBoostArgsList[['PassInGrid']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_PassInGrid']]}, error=function(x) NULL), Type='character', Default=NULL)
      XGBoostArgsList[['GridTune']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_GridTune']]}, error=function(x) NULL), Type='logical', Default=FALSE)
      XGBoostArgsList[['MaxModelsInGrid']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_MaxModelsInGrid']]}, error=function(x) NULL), Type='numeric', Default=30)
      XGBoostArgsList[['MaxRunsWithoutNewWinner']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_MaxRunsWithoutNewWinner']]}, error=function(x) NULL), Type='numeric', Default=15)
      XGBoostArgsList[['MaxRunMinutes']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_MaxRunMinutes']]}, error=function(x) NULL), Type='numeric', Default=30)
      XGBoostArgsList[['BaselineComparison']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_BaselineComparison']]}, error=function(x) NULL), Type='character', Default='default')

      print(' ::  BuildModels 6 :: ')

      # XGBoost ML Parameters
      XGBoostArgsList[['Trees']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_Trees']]}, error=function(x) NULL), Type='numeric', Default=1000)
      XGBoostArgsList[['max_depth']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_max_depth']]}, error=function(x) NULL), Type='numeric', Default=8)
      XGBoostArgsList[['eta']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_eta']]}, error=function(x) NULL), Type='numeric', Default=NULL)
      XGBoostArgsList[['min_child_weight']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_min_child_weight']]}, error=function(x) NULL), Type='numeric', Default=NULL)
      XGBoostArgsList[['subsample']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_subsample']]}, error=function(x) NULL), Type='numeric', Default=1)
      XGBoostArgsList[['colsample_bytree']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_colsample_bytree']]}, error=function(x) NULL), Type='numeric', Default=1)

      print(' ::  BuildModels 7 :: ')
      print(XGBoost_TargetType)

      # XGBoost Eval Parameters
      if(XGBoost_TargetType == 'Regression') {
        print(' ::  BuildModels 7.1 :: ')
        XGBoostArgsList[['LossFunction']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_LossFunction']]}, error=function(x) NULL), Type='character', Default='reg:squarederror')
        XGBoostArgsList[['eval_metric']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_EvalMetric']]}, error=function(x) NULL), Type='character', Default='rmse')
        XGBoostArgsList[['grid_eval_metric']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_grid_eval_metric']]}, error=function(x) NULL), Type='character', Default='r2')
      } else if(XGBoost_TargetType == 'Binary Classification') {
        print(' ::  BuildModels 7.2 :: ')
        XGBoostArgsList[['LossFunction']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_LossFunction']]}, error=function(x) NULL), Type='character', Default='binary:logistic')
        XGBoostArgsList[['eval_metric']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_EvalMetric']]}, error=function(x) NULL), Type='character', Default='auc')
        XGBoostArgsList[['grid_eval_metric']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_grid_eval_metric']]}, error=function(x) NULL), Type='character', Default='MCC')
      } else {
        print(' ::  BuildModels 7.3 :: ')
        print(tryCatch({input[['XGBoost_LossFunction']]}, error=function(x) NULL))
        print(RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_LossFunction']]}, error=function(x) NULL), Type='character', Default='multi:softprob'))
        XGBoostArgsList[['LossFunction']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_LossFunction']]}, error=function(x) NULL), Type='character', Default='multi:softprob')
        XGBoostArgsList[['eval_metric']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_EvalMetric']]}, error=function(x) NULL), Type='character', Default='mlogloss')
        XGBoostArgsList[['grid_eval_metric']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_grid_eval_metric']]}, error=function(x) NULL), Type='character', Default='microauc')
      }

      print("RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_TargetType']]}, error=function(x) NULL), Type='character', Default='MultiClass')")

      # Build model
      print(' ::  BuildModels 8 :: ')
      if(XGBoost_TargetType == 'Regression') {
        print(' ::  BuildModels 8.1 :: ')
        XGBoostArgsList[['PrimaryDateColumn']] <- RemixAutoML:::ReturnParam(xx=input[['XGBoost_PrimaryDateColumn']], Type='character', Default=NULL)
        XGBoostArgsList[['SaveInfoToPDF']] <- FALSE
        print(XGBoostArgsList)
        Model <- do.call(RemixAutoML::AutoXGBoostRegression, XGBoostArgsList)
      }
      if(XGBoost_TargetType == 'Binary Classification') {
        print(' ::  BuildModels 8.2 :: ')
        XGBoostArgsList[['CostMatrixWeights']] <- c(0,1,1,0)
        XGBoostArgsList[['SaveInfoToPDF']] <- FALSE
        print(XGBoostArgsList)
        Model <- do.call(RemixAutoML::AutoXGBoostClassifier, XGBoostArgsList)
      }
      if(XGBoost_TargetType == 'MultiClass') {
        print(' ::  BuildModels 8.3 :: ')
        print(XGBoostArgsList)
        Model <- do.call(RemixAutoML::AutoXGBoostMultiClass, XGBoostArgsList)
      }

      print(' ::  BuildModels 9 :: ')

      # Store in DataList
      KeyName <- XGBoostArgsList[['ModelID']]
      Output <- RemixAutoML:::ModelDataObjects(Model, TT = 'xgboost')
      DataList[[paste0(KeyName, '_ScoringData')]] <- Output$ScoringDataCombined
      DataList[[paste0(KeyName, '_Test_VI_Data')]] <- Output$VI_Train

      print(' ::  BuildModels 10 :: ')

      # Save output
      DataList <<- DataList
    }

    # LightGBM check
    temp <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_data']]}, error=function(x) NULL), Type='character', Default=NULL)
    if(length(temp) != 0) LightGBMArgsList[['data']] <- DataList[[temp]] else LightGBMArgsList[['data']] <- NULL
    LightGBMArgsList[['TargetColumnName']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_TargetColumnName']]}, error=function(x) NULL), Type='character', Default=NULL)
    LightGBMArgsList[['FeatureColNames']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_FeatureColNames']]}, error=function(x) NULL), Type='character', Default=NULL)
    if(length(LightGBMArgsList[['data']]) != 0L && length(LightGBMArgsList[['TargetColumnName']]) != 0L && length(LightGBMArgsList[['FeatureColNames']]) != 0L) {
      LightGBMRun <- TRUE
    } else {
      LightGBMRun <- FALSE
    }

    # LightGBM ML Build
    if(LightGBMRun) {

      LightGBM_TargetType <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_TargetType']]}, error=function(x) NULL), Type='character', Default='MultiClass')

      # Notification of starting
      shiny::showNotification('LightGBM Building has Begun!')

      # LightGBM MetaData Parameters
      LightGBMArgsList[['OutputSelection']] <- c('Importances','EvalMetrics','Score_TrainData')
      LightGBMArgsList[['TrainOnFull']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_TrainOnFull']]}, error=function(x) NULL), Type='logical', Default=FALSE)
      LightGBMArgsList[['ModelID']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_ModelID']]}, error=function(x) NULL), Type='character', Default='Model_1')

      # LightGBM Data Parameters
      LightGBMArgsList[['ValidationData']] <- DataList[[RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_ValidationDataSelection']]}, error=function(x) NULL), Type='character', Default=NULL)]]
      LightGBMArgsList[['TestData']] <- DataList[[RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_TestDataSelection']]}, error=function(x) NULL), Type='character', Default=NULL)]]
      LightGBMArgsList[['PrimaryDateColumn']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_PrimaryDateColumn']]}, error=function(x) NULL), Type='character', Default=NULL)
      LightGBMArgsList[['WeightsColumnName']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_WeightsColumnName']]}, error=function(x) NULL), Type='character', Default=NULL)
      LightGBMArgsList[['IDcols']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_IDcols']]}, error=function(x) NULL), Type='character', Default=NULL)
      LightGBMArgsList[['TransformNumericColumns']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_TransformNumericColumns']]}, error=function(x) NULL), Type='character', Default=NULL)
      LightGBMArgsList[['Methods']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_Methods']]}, error=function(x) NULL), Type='character', Default=NULL)

      # LightGBM Grid Tuning Parameters
      LightGBMArgsList[['PassInGrid']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_PassInGrid']]}, error=function(x) NULL), Type='character', Default=NULL)
      LightGBMArgsList[['GridTune']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_GridTune']]}, error=function(x) NULL), Type='logical', Default=FALSE)
      LightGBMArgsList[['MaxModelsInGrid']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_MaxModelsInGrid']]}, error=function(x) NULL), Type='numeric', Default=30)
      LightGBMArgsList[['MaxRunsWithoutNewWinner']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_MaxRunsWithoutNewWinner']]}, error=function(x) NULL), Type='numeric', Default=15)
      LightGBMArgsList[['MaxRunMinutes']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_MaxRunMinutes']]}, error=function(x) NULL), Type='numeric', Default=30)
      LightGBMArgsList[['BaselineComparison']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_BaselineComparison']]}, error=function(x) NULL), Type='character', Default='default')

      # LightGBM ML Parameters
      LightGBMArgsList[['Trees']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGoost_Trees']]}, error=function(x) NULL), Type='numeric', Default=1000)
      LightGBMArgsList[['Depth']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_Depth']]}, error=function(x) NULL), Type='numeric', Default=8)
      LightGBMArgsList[['LearningRate']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_LearningRate']]}, error=function(x) NULL), Type='numeric', Default=NULL)
      LightGBMArgsList[['L2_Leaf_Reg']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_L2_Leaf_Reg']]}, error=function(x) NULL), Type='numeric', Default=NULL)
      LightGBMArgsList[['RSM']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_RSM']]}, error=function(x) NULL), Type='numeric', Default=1)
      LightGBMArgsList[['subsample']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_subsample']]}, error=function(x) NULL), Type='numeric', Default=1)
      LightGBMArgsList[['min_data_in_leaf']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_min_data_in_leaf']]}, error=function(x) NULL), Type='numeric', Default=1)

      # LightGBM Eval Parameters
      if(LightGBM_TargetType == 'Regression') {
        LightGBMArgsList[['loss_function']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_LossFunction']]}, error=function(x) NULL), Type='character', Default='RMSE')
        LightGBMArgsList[['eval_metric']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_EvalMetric']]}, error=function(x) NULL), Type='character', Default='RMSE')
        LightGBMArgsList[['grid_eval_metric']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_grid_eval_metric']]}, error=function(x) NULL), Type='character', Default='mse')
        cw0 <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_ClassWeights0']]}, error=function(x) NULL), Type='numeric', Default=1)
        cw1 <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_ClassWeights1']]}, error=function(x) NULL), Type='numeric', Default=1)
        LightGBMArgsList[['ClassWeights']] <- c(cw0, cw1)
        LightGBMArgsList[['MetricPeriods']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_MetricPeriods']]}, error=function(x) NULL), Type='numeric', Default=10)
      } else if(LightGBM_TargetType == 'Binary Classification') {
        LightGBMArgsList[['LossFunction']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_LossFunction']]}, error=function(x) NULL), Type='character', Default='Logloss')
        LightGBMArgsList[['EvalMetric']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_EvalMetric']]}, error=function(x) NULL), Type='character', Default='AUC')
        LightGBMArgsList[['grid_eval_metric']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_grid_eval_metric']]}, error=function(x) NULL), Type='character', Default='MCC')
        LightGBMArgsList[['MetricPeriods']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_MetricPeriods']]}, error=function(x) NULL), Type='numeric', Default=10)
      } else if(LightGBM_TargetType == 'MultiClass') {
        LightGBMArgsList[['loss_function']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_LossFunction']]}, error=function(x) NULL), Type='character', Default='MultiClassOneVsAll')
        LightGBMArgsList[['eval_metric']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_EvalMetric']]}, error=function(x) NULL), Type='character', Default='MultiClassOneVsAll')
        LightGBMArgsList[['grid_eval_metric']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_grid_eval_metric']]}, error=function(x) NULL), Type='character', Default='Accuracy')
        LightGBMArgsList[['MetricPeriods']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_MetricPeriods']]}, error=function(x) NULL), Type='numeric', Default=10)
      }

      # Build model
      if(LightGBM_TargetType == 'Regression') {
        Model <- do.call(RemixAutoML::AutoLightGBMRegression, LightGBMArgsList)
      }
      if(LightGBM_TargetType == 'Binary Classification') {
        Model <- do.call(RemixAutoML::AutoLightGBMClassifier, LightGBMArgsList)
      }
      if(LightGBM_TargetType == 'MultiClass') {
        Model <- do.call(RemixAutoML::AutoLightGBMMultiClass, LightGBMArgsList)
      }

      # Store in DataList
      KeyName <- LightGBMArgsList[['ModelID']]
      Output <- RemixAutoML:::ModelDataObjects(Model)
      DataList[[paste0(KeyName, '_ScoringData')]] <- Output$ScoringDataCombined
      DataList[[paste0(KeyName, '_Test_VI_Data')]] <- Output$VI_Train

      # Save output
      DataList <<- DataList
    }

    # Finished
    shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = 'success', btn_colors = 'green', html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")

  })

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # :: Obs Event Plotting                ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  shiny::observeEvent(input$TrendPlotExecute, {

    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    # Determine Which Plots to Build       ----
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    PlotCollectionList <- list()
    CodeCollection <- list()

    # Identify which plots to build
    NumPlots <- c()
    Plot1 <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['Plot1']]}, error=function(x) NULL), Type='character', Default=NULL)
    Plot2 <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['Plot2']]}, error=function(x) NULL), Type='character', Default=NULL)
    Plot3 <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['Plot3']]}, error=function(x) NULL), Type='character', Default=NULL)
    Plot4 <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['Plot4']]}, error=function(x) NULL), Type='character', Default=NULL)
    if(length(Plot1) != 0) NumPlots[length(NumPlots) + 1L] <- 1
    if(length(Plot2) != 0) NumPlots[length(NumPlots) + 1L] <- 2
    if(length(Plot3) != 0) NumPlots[length(NumPlots) + 1L] <- 3
    if(length(Plot4) != 0) NumPlots[length(NumPlots) + 1L] <- 4

    # PlotType Determination
    ULP <- input[['PlotTypeDragula']][['target']][['TopLeft']]
    BLP <- input[['PlotTypeDragula']][['target']][['LowerLeft']]
    URP <- input[['PlotTypeDragula']][['target']][['TopRight']]
    BRP <- input[['PlotTypeDragula']][['target']][['LowerRight']]
    PlotBuilds <- c()
    if(!is.null(ULP) && ULP != "") {
      if(ULP == 'Plot1') PlotBuilds[length(PlotBuilds) + 1L] <- 1
      if(ULP == 'Plot2') PlotBuilds[length(PlotBuilds) + 1L] <- 2
      if(ULP == 'Plot3') PlotBuilds[length(PlotBuilds) + 1L] <- 3
      if(ULP == 'Plot4') PlotBuilds[length(PlotBuilds) + 1L] <- 4
    }
    if(!is.null(BLP) && BLP != "") {
      if(BLP == 'Plot1') PlotBuilds[length(PlotBuilds) + 1L] <- 1
      if(BLP == 'Plot2') PlotBuilds[length(PlotBuilds) + 1L] <- 2
      if(BLP == 'Plot3') PlotBuilds[length(PlotBuilds) + 1L] <- 3
      if(BLP == 'Plot4') PlotBuilds[length(PlotBuilds) + 1L] <- 4
    }
    if(!is.null(URP) && URP != "") {
      if(URP == 'Plot1') PlotBuilds[length(PlotBuilds) + 1L] <- 1
      if(URP == 'Plot2') PlotBuilds[length(PlotBuilds) + 1L] <- 2
      if(URP == 'Plot3') PlotBuilds[length(PlotBuilds) + 1L] <- 3
      if(URP == 'Plot4') PlotBuilds[length(PlotBuilds) + 1L] <- 4
    }
    if(!is.null(BRP) && BRP != "") {
      if(BRP == 'Plot1') PlotBuilds[length(PlotBuilds) + 1L] <- 1
      if(BRP == 'Plot2') PlotBuilds[length(PlotBuilds) + 1L] <- 2
      if(BRP == 'Plot3') PlotBuilds[length(PlotBuilds) + 1L] <- 3
      if(BRP == 'Plot4') PlotBuilds[length(PlotBuilds) + 1L] <- 4
    }

    # Intersection of both
    PlotRefs <- intersect(PlotBuilds,NumPlots)
    if(length(PlotRefs) == 0) PlotRefs <- NULL

    # Global Settings
    PlotObjectHome[['GlobalSettings']][['PlotEngine']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['PlotEngine']]}, error=function(x) NULL), Type='character', Default='plotly')
    PlotObjectHome[['GlobalSettings']][['PlotWidth']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['PlotWidth']]}, error=function(x) NULL), Type='numeric', Default=1550L)
    PlotObjectHome[['GlobalSettings']][['PlotHeight']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['PlotHeight']]}, error=function(x) NULL), Type='numeric', Default=500L)

    # Initialize PlotObjectHome List
    for(run in PlotRefs) {
      PlotObjectHome[[paste0('Plot_', run)]][['DataSource']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('DataSource', run)]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['PlotType']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('Plot', run)]]}, error=function(x) NULL), Type='character', Default=NULL, Debug = TRUE)
      PlotObjectHome[[paste0('Plot_', run)]][['UpdateMethod']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('UpdateMethod', run)]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['SampleSize']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('SampleSize', run)]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['NumberGroupsDisplay']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('NumberGroupsDisplay', run)]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['YVar']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('YVar', run)]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['YTicks']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('YTicks', run)]]}, error=function(x) NULL), Type='character', Default='Default')
      PlotObjectHome[[paste0('Plot_', run)]][['XVar']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('XVar', run)]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['XTicks']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('XTicks', run)]]}, error=function(x) NULL), Type='character', Default='Default')
      PlotObjectHome[[paste0('Plot_', run)]][['ZVar']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('ZVar', run)]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['CorMethod']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('CorMethod', run)]]}, error=function(x) NULL), Type='character', Default='pearson')
      PlotObjectHome[[paste0('Plot_', run)]][['ScoreVar']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('ScoreVar', run)]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['GroupVars']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('GroupVars', run)]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['Levels1']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('Levels_',run,'_1')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['Levels2']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('Levels_',run,'_1')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['Levels3']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('Levels_',run,'_1')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['SizeVars']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('SizeVars', run)]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FacetVar1']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('FacetVar_', run, '_1')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FacetVar2']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('FacetVar_', run, '_2')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FilterVar1']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('FilterVariable_',run, '_1')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FilterVar2']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('FilterVariable_',run, '_2')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FilterVar3']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('FilterVariable_',run, '_3')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FilterVar4']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('FilterVariable_',run, '_4')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FilterLogic1']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('FilterLogic_',run, '_1')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FilterLogic2']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('FilterLogic_',run, '_2')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FilterLogic3']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('FilterLogic_',run, '_3')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FilterLogic4']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('FilterLogic_',run, '_4')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FilterValue_1_1']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',run, '_1_1')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FilterValue_1_2']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',run, '_1_2')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FilterValue_1_3']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',run, '_2_1')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FilterValue_1_4']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',run, '_2_2')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FilterValue_2_1']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',run, '_3_1')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FilterValue_2_2']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',run, '_3_2')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FilterValue_2_3']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',run, '_4_1')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FilterValue_2_4']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',run, '_4_2')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['AngleY']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('AngleY', run)]]}, error=function(x) NULL), Type='numeric', Default=0L)
      PlotObjectHome[[paste0('Plot_', run)]][['AngleX']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('AngleX', run)]]}, error=function(x) NULL), Type='numeric', Default=90L)
      PlotObjectHome[[paste0('Plot_', run)]][['TextSize']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('TextSize', run)]]}, error=function(x) NULL), Type='numeric', Default=15L)
      PlotObjectHome[[paste0('Plot_', run)]][['OutlierSize']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('OutlierSize', run)]]}, error=function(x) NULL), Type='numeric', Default=0.01)
      PlotObjectHome[[paste0('Plot_', run)]][['LegendPosition']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('LegendPosition', run)]]}, error=function(x) NULL), Type='character', Default='right')
      PlotObjectHome[[paste0('Plot_', run)]][['LegendBorderSize']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('LegendBorderSize', run)]]}, error=function(x) NULL), Type='numeric', Default=0.01)
      PlotObjectHome[[paste0('Plot_', run)]][['LegendLineType']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('LegendLineType', run)]]}, error=function(x) NULL), Type='character', Default='solid')
      PlotObjectHome[[paste0('Plot_', run)]][['TextColor']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('TextColor', run)]]}, error=function(x) NULL), Type='character', Default='darkblue')
      PlotObjectHome[[paste0('Plot_', run)]][['ChartColor']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('ChartColor', run)]]}, error=function(x) NULL), Type='character', Default='aliceblue')
      PlotObjectHome[[paste0('Plot_', run)]][['GridColor']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('GridColor', run)]]}, error=function(x) NULL), Type='character', Default='lightsteelblue1')
      PlotObjectHome[[paste0('Plot_', run)]][['BackGroundColor']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('BackGroundColor', run)]]}, error=function(x) NULL), Type='character', Default='gray95')
      PlotObjectHome[[paste0('Plot_', run)]][['BorderColor']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('BorderColor', run)]]}, error=function(x) NULL), Type='character', Default='lightsteelblue4')
      PlotObjectHome[[paste0('Plot_', run)]][['OutlierColor']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('OutlierColor', run)]]}, error=function(x) NULL), Type='character', Default='blue')
      PlotObjectHome[[paste0('Plot_', run)]][['FillColor']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('FillColor', run)]]}, error=function(x) NULL), Type='character', Default='gray70')
      PlotObjectHome[[paste0('Plot_', run)]][['SubTitleColor']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('SubTitleColor', run)]]}, error=function(x) NULL), Type='character', Default='blue')
      PlotObjectHome[[paste0('Plot_', run)]][['ShapAgg']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('ShapAggMethod', run)]]}, error=function(x) NULL), Type='character', Default='meanabs')
      PlotObjectHome[[paste0('Plot_', run)]][['GamFitScatter']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('GamFitScatter', run)]]}, error=function(x) NULL), Type='logical', Default=FALSE)
      PlotObjectHome[[paste0('Plot_', run)]][['NumberBins']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('NumberBins', run)]]}, error=function(x) NULL), Type='numeric', Default=30L)
      PlotObjectHome[[paste0('Plot_', run)]][['Percentile_Buckets']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('Percentile_Buckets', run)]]}, error=function(x) NULL), Type='numeric', Default=20L)
      PlotObjectHome[[paste0('Plot_', run)]][['BarPlotAgg']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('BarPlotAgg', run)]]}, error=function(x) NULL), Type='character', Default='mean')
      PlotObjectHome[[paste0('Plot_', run)]][['TargetLevel']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0('TargetLevel', run)]]}, error=function(x) NULL), Type='character', Default=NULL)

      # Assign Globally
      assign(x = 'PlotObjectHome', value = PlotObjectHome, envir = .GlobalEnv)
    }

    # ----

    # ----

    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    # Loop Through Plot Builds             ----
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    for(run in PlotRefs) {

      # Code ID
      CodeCollection[[run]] <- run

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
        ZVar <- PlotObjectHome[[paste0('Plot_', run)]][['ZVar']]
        CorMethod <- PlotObjectHome[[paste0('Plot_', run)]][['CorMethod']]
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
        PlotEngine <<- PlotObjectHome[['GlobalSettings']][['PlotEngine']]
        PlotWidth <<- PlotObjectHome[['GlobalSettings']][['PlotWidth']]
        PlotHeight <<- PlotObjectHome[['GlobalSettings']][['PlotHeight']]
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
        BarPlotAgg <- PlotObjectHome[[paste0('Plot_', run)]][['BarPlotAgg']]
        TargetLevel <- PlotObjectHome[[paste0('Plot_', run)]][['TargetLevel']]
        ShapAgg <- PlotObjectHome[[paste0('Plot_', run)]][['ShapAgg']]
        GamFitScatter <- PlotObjectHome[[paste0('Plot_', run)]][['GamFitScatter']]
        NumberBins <- PlotObjectHome[[paste0('Plot_', run)]][['NumberBins']]
        Percentile_Buckets <- PlotObjectHome[[paste0('Plot_', run)]][['Percentile_Buckets']]
      }

      # ----

      # ----

      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
      # Logic Check to Build Plots           ----
      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

      # Define data
      data1 <- DataList[[input[[paste0('Plot', run, '_SelectData')]]]]

      # For PDP's
      print('ScoreVar')
      print(names(data1))
      if('p1' %in% names(data1)) {
        ScoreVar <- 'p1'
      } else if('Predict' %in% names(data1)) {
        ScoreVar <- 'Predict'
      } else {
        ScoreVar <- NULL
      }

      # PLOT LOGIC CHECK THEN BUILD PLOT:
      if(length(YVar) == 0 && length(XVar) == 0 && PlotType %in% c('BoxPlot','ViolinPlot','BarPlot','LinePlot','ScatterPlot','CopulaPlot','Histogram','CorrelationMatrix')) {
        shinyWidgets::sendSweetAlert(session, title = NULL, text = 'You need to specify additional variables to generate additional plots', type = NULL, btn_labels = "error", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
      } else if(PlotType %in% 'CorrelationMatrix' && length(YVar) == 0) {
        shinyWidgets::sendSweetAlert(session, title = NULL, text = "YVar needs to have at least two variables selected to build this plot", type = NULL, btn_labels = "error", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
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
          SubsetList[[paste0('GroupVars', run)]] <- GroupVars
          SubsetList[[paste0('Levels_', run, '_1')]] <- Levels1
          SubsetList[[paste0('Levels_', run, '_2')]] <- Levels2
          SubsetList[[paste0('Levels_', run, '_3')]] <- Levels3
          SubsetList[[paste0('FacetVar_', run, '_1')]] <- FacetVar1
          SubsetList[[paste0('FacetVar_', run, '_2')]] <- FacetVar2
          SubsetList[[paste0('SizeVar', run)]] <- SizeVars

          # Filter Variables
          SubsetList[[paste0('FilterVariable_', run, '_1')]] <- if(length(FilterVar1) != 0 && FilterVar1 != 'None') stringr::str_remove(string = FilterVar1, pattern = 'ModelVar-') else 'None'
          SubsetList[[paste0('FilterVariable_', run, '_2')]] <- if(length(FilterVar2) != 0 && FilterVar2 != 'None') stringr::str_remove(string = FilterVar2, pattern = 'ModelVar-') else 'None'
          SubsetList[[paste0('FilterVariable_', run, '_3')]] <- if(length(FilterVar3) != 0 && FilterVar3 != 'None') stringr::str_remove(string = FilterVar3, pattern = 'ModelVar-') else 'None'
          SubsetList[[paste0('FilterVariable_', run, '_4')]] <- if(length(FilterVar4) != 0 && FilterVar4 != 'None') stringr::str_remove(string = FilterVar4, pattern = 'ModelVar-') else 'None'

          # Filter Logic
          SubsetList[[paste0('FilterLogic_', run, '_1')]] <- FilterLogic1
          SubsetList[[paste0('FilterLogic_', run, '_2')]] <- FilterLogic2
          SubsetList[[paste0('FilterLogic_', run, '_3')]] <- FilterLogic3
          SubsetList[[paste0('FilterLogic_', run, '_3')]] <- FilterLogic4

          # Filter Values
          SubsetList[[paste0('FilterValue_', run, '_1_1')]] <- FilterValue_1_1
          SubsetList[[paste0('FilterValue_', run, '_1_2')]] <- FilterValue_1_2
          SubsetList[[paste0('FilterValue_', run, '_1_3')]] <- FilterValue_1_3
          SubsetList[[paste0('FilterValue_', run, '_1_4')]] <- FilterValue_1_4
          SubsetList[[paste0('FilterValue_', run, '_2_1')]] <- FilterValue_2_1
          SubsetList[[paste0('FilterValue_', run, '_2_2')]] <- FilterValue_2_2
          SubsetList[[paste0('FilterValue_', run, '_2_3')]] <- FilterValue_2_3
          SubsetList[[paste0('FilterValue_', run, '_2_4')]] <- FilterValue_2_4

          # Store Globally
          assign(x = 'SubsetList', value = SubsetList, envir = .GlobalEnv)

        } else {

          if(Debug) print('Here 14 b')

          # MetaData
          SubsetList[[paste0('RunNumber', run)]] <- RunNumber + 1L
          SubsetList[[paste0('DataPrep', run)]] <- FALSE

          # Group Variables
          if(Debug) print('GROUP VARIABLE CHECK HERE :::::::::::::::::::::')

          # Check values
          if(!all(SubsetList[[paste0('GroupVars', run)]] == GroupVars)) {
            if(Debug) print(GroupVars)
            SubsetList[[paste0('GroupVars', run)]] <- RemixAutoML:::ReturnParam(xx = input[[paste0('GroupVars', run)]], VarName=paste0('GroupVars', run), Type='character', Default='None', Switch=TRUE)
            SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }

          # Levels.., FacetVar1.., SizeVars
          if(!all(SubsetList[[paste0('Levels_', run, '_1')]] == Level1) || length(SubsetList[[paste0('Levels_', run, '_1')]]) > length(Level1)) {
            SubsetList[[paste0('Levels_', run, '_1')]] <- Level1; SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('Levels_', run, '_2')]] == Level2) || length(SubsetList[[paste0('Levels_', run, '_2')]]) > length(Level2)) {
            SubsetList[[paste0('Levels_', run, '_2')]] <- Level2; SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('Levels_', run, '_3')]] == Level3) || length(SubsetList[[paste0('Levels_', run, '_3')]]) > length(Level3)) {
            SubsetList[[paste0('Levels_', run, '_3')]] <- Level3; SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FacetVar_', run, '_1')]] == FacetVar1) || length(SubsetList[[paste0('FacetVar_', run, '_1')]]) > length(FacetVar1)) {
            SubsetList[[paste0('FacetVar_', run, '_1')]] <- FacetVar1; SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FacetVar_', run, '_2')]] == FacetVar2) || length(SubsetList[[paste0('FacetVar_', run, '_2')]]) > length(FacetVar2)) {
            SubsetList[[paste0('FacetVar_', run, '_2')]] <- FacetVar2; SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('SizeVar', run)]] == SizeVars) || length(SubsetList[[paste0('SizeVar', run)]]) > length(SizeVars)) {
            SubsetList[[paste0('SizeVar', run)]] <- SizeVars; SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }

          # Filter Variables
          if(!all(SubsetList[[paste0('FilterVariable_', run, '_1')]] == FilterVar1) || length(SubsetList[[paste0('FilterVariable_', run, '_1')]]) > length(FilterVar1)) {
            SubsetList[[paste0('FilterVariable_', run, '_1')]] <- FilterVar1; SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterVariable_', run, '_2')]] == FilterVar2) || length(SubsetList[[paste0('FilterVariable_', run, '_2')]]) > length(FilterVar2)) {
            SubsetList[[paste0('FilterVariable_', run, '_2')]] <- FilterVar2; SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterVariable_', run, '_3')]] == FilterVar3) || length(SubsetList[[paste0('FilterVariable_', run, '_3')]]) > length(FilterVar3)) {
            SubsetList[[paste0('FilterVariable_', run, '_3')]] <- FilterVar3; SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterVariable_', run, '_4')]] == FilterVar4) || length(SubsetList[[paste0('FilterVariable_', run, '_4')]]) > length(FilterVar4)) {
            SubsetList[[paste0('FilterVariable_', run, '_4')]] <- FilterVariable4; SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }

          # Filter Logic
          if(!all(SubsetList[[paste0('FilterLogic_', run, '_1')]] == FilterLogic1) || length(SubsetList[[paste0('FilterLogic_', run, '_1')]]) > length(FilterLogic1)) {
            SubsetList[[paste0('FilterLogic_', run, '_1')]] <- FilterLogic1; SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterLogic_', run, '_2')]] == FilterLogic2) || length(SubsetList[[paste0('FilterLogic_', run, '_2')]]) > length(FilterLogic2)) {
            SubsetList[[paste0('FilterLogic_', run, '_2')]] <- FilterLogic2; SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterLogic_', run, '_3')]] == FilterLogic3) || length(SubsetList[[paste0('FilterLogic_', run, '_3')]]) > length(FilterLogic3)) {
            SubsetList[[paste0('FilterLogic_', run, '_3')]] <- FilterLogic3; SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterLogic_', run, '_4')]] == FilterLogic4) || length(SubsetList[[paste0('FilterLogic_', run, '_4')]]) > length(FilterLogic4)) {
            SubsetList[[paste0('FilterLogic_', run, '_4')]] <- FilterLogic4; SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }

          # Filter Values
          if(!all(SubsetList[[paste0('FilterValue_', run, '_1_1')]] == FilterValue_1_1) || length(SubsetList[[paste0('FilterValue_', run, '_1_1')]]) > length(FilterLogic_1_1)) {
            SubsetList[[paste0('FilterValue_', run, '_1_1')]] <- FilterValue_1_1; SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterValue_', run, '_1_2')]] == FilterValue_1_2) || length(SubsetList[[paste0('FilterValue_', run, '_1_2')]]) > length(FilterLogic_1_2)) {
            SubsetList[[paste0('FilterValue_', run, '_1_2')]] <- FilterValue_1_2; SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterValue_', run, '_2_1')]] == FilterValue_1_3) || length(SubsetList[[paste0('FilterValue_', run, '_1_3')]]) > length(FilterLogic_1_3)) {
            SubsetList[[paste0('FilterValue_', run, '_2_1')]] <- FilterValue_1_3; SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterValue_', run, '_2_2')]] == FilterValue_1_4) || length(SubsetList[[paste0('FilterValue_', run, '_1_4')]]) > length(FilterLogic_1_4)) {
            SubsetList[[paste0('FilterValue_', run, '_2_2')]] <- FilterValue_1_4; SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterValue_', run, '_3_1')]] == FilterValue_2_1) || length(SubsetList[[paste0('FilterValue_', run, '_2_1')]]) > length(FilterLogic_2_1)) {
            SubsetList[[paste0('FilterValue_', run, '_3_1')]] <- FilterValue_2_1; SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterValue_', run, '_3_2')]] == FilterValue_2_2) || length(SubsetList[[paste0('FilterValue_', run, '_2_2')]]) > length(FilterLogic_2_2)) {
            SubsetList[[paste0('FilterValue_', run, '_3_2')]] <- FilterValue_2_2; SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterValue_', run, '_4_1')]] == FilterValue_2_3) || length(SubsetList[[paste0('FilterValue_', run, '_2_3')]]) > length(FilterLogic_2_3)) {
            SubsetList[[paste0('FilterValue_', run, '_4_1')]] <- FilterValue_2_3; SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterValue_', run, '_4_2')]] == FilterValue_2_4) || length(SubsetList[[paste0('FilterValue_', run, '_2_4')]]) > length(FilterLogic_2_4)) {
            SubsetList[[paste0('FilterValue_', run, '_4_2')]] <- FilterValue_2_4; SubsetList[[paste0('DataPrep', run)]] <- TRUE
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

        # Filter Data if DataPrep = TRUE
        if(SubsetList[[paste0('DataPrep', run)]]) {

          # Subset by FilterVariable
          if(Debug) print('Here 23')
          for(i in seq_len(4L)) {
            if(Debug) print(length(eval(parse(text = paste0('FilterVar', i)))) != 0L)
            if(length(eval(parse(text = paste0('FilterVar', i)))) != 0L) {
              data1 <- RemixAutoML:::FilterLogicData(
                data1,
                FilterLogic    = get(paste0('FilterLogic', i)),
                FilterVariable = get(paste0('FilterVar', i)),
                FilterValue    = get(paste0('FilterValue_',i,'_1')),
                FilterValue2   = get(paste0('FilterValue_',i,'_2')),
                Debug          = Debug)
              CodeCollection[[run]][[length(CodeCollection[[run]])+1L]] <- paste0("data1 <- RemixAutoML:::FilterLogicData(data1, FilterLogic=", RemixAutoML:::CEP(get(paste0('FilterLogic',i))),", FilterVariable=", RemixAutoML:::CEP(get(paste0('FilterVar',i))),", FilterValue=", RemixAutoML:::CEP(get(paste0('FilterValue_',i,'_1'))),", FilterValue2=", RemixAutoML:::CEP(get(paste0('FilterValue_',i,'_2'))),"))")
            }
          }

          # Subset Rows and Columns
          if(PlotType == 'LinePlot' && length(XVar) != 0) {
            data1 <- RemixAutoML:::PreparePlotData(
              SubsetOnly = FALSE,
              data = data1, Aggregate = 'mean', TargetVariable = YVar, DateVariable = XVar,
              GroupVariables = GroupVars,
              G1Levels = Levels1, G2Levels = Levels2, G3Levels = Levels3)
            CodeCollection[[run]][[length(CodeCollection[[run]])+1L]] <- paste0("data1 <- RemixAutoML:::PreparePlotData(SubsetOnly = ", FALSE,", data=data1, Aggregate='mean', TargetVariable=", RemixAutoML:::CEP(YVar),", DateVariable=", RemixAutoML:::CEP(XVar), ", GroupVariables=", RemixAutoML:::CEP(GroupVars),", G1Levels=", RemixAutoML:::CEP(Levels1),", G2Levels=", RemixAutoML:::CEP(Levels2),", G3Levels=", RemixAutoML:::CEP(Levels3),")")

          } else {

            # Debugging
            if(Debug) {print(YVar); print(XVar); print(ScoreVar); print(GroupVars); print(SizeVars); print(FacetVar1); print(FacetVar2)}

            # Subset columns
            if(Debug) print('Subset Columns Here')
            if(!PlotType %in% c('BoxPlot','ViolinPlot','BarPlot','LinePlot','ScatterPlot','CopulaPlot','Histogram','CorrelationMatrix','HeatMapPlot','ShapelyImportance')) {
              if(length(unique(c(XVar))) != 0) {
                Keep <- unique(c(YVar, XVar, ZVar, ScoreVar)); if(Debug) {print(Keep); print(names(data1))}
                data1 <- data1[, .SD, .SDcols = c(Keep)]; if(Debug) print('Subset Columns Here predone')
                CodeCollection[[run]][[length(CodeCollection)+1L]] <- paste0("data1 <- data1[, .SD, .SDcols = c(",RemixAutoML:::ExpandText(Keep),")]"); if(Debug) print('Subset Columns Here done')
              }
            } else if(!PlotType %in% 'ShapelyImportance') {
              if(length(unique(c(YVar, XVar, ZVar, GroupVars, SizeVars, FacetVar1, FacetVar2))) != 0) {
                Keep <- unique(c(YVar, XVar, ZVar, GroupVars, SizeVars, FacetVar1, FacetVar2))
                if(PlotType %in% 'VariableImportance') Keep <- unique(keep, 'Variable','Importance')
                data1 <- data1[, .SD, .SDcols = c(Keep)]; if(Debug) print('Subset Columns Here predone')
                CodeCollection[[run]][[length(CodeCollection)+1L]] <- paste0("data1 <- data1[, .SD, .SDcols = c(",RemixAutoML:::ExpandText(Keep),")]"); if(Debug) print('Subset Columns Here done')
              }
            }
          }
        }

        # ----

        # ----

        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
        # Define Plots Variables               ----
        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

        if(Debug) print('Here 28')

        # XVar: If XVar is NULL go to next iteration in Plot Loop
        if(PlotType == 'LinePlot') {
          if(Debug) print('Checking XVar for PlotType == :: LinePlot ::')
          if(length(XVar) == 0) next
        }

        # ----

        # ----

        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
        # Create Plots                         ----
        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
        if(PlotType %chin% c('BoxPlot','ViolinPlot','BarPlot','LinePlot','ScatterPlot','CopulaPlot','Histogram','CorrelationMatrix','HeatMapPlot')) {

          # AutoPlotter()
          if(Debug) print('Run AutoPlotter')
          PlotCollectionList[[paste0('p', run)]] <- RemixAutoML:::AutoPlotter(
            dt = data1,
            PlotType = PlotType,
            SampleSize = SampleSize,
            YVar = YVar,
            XVar = XVar,
            ZVar = ZVar,
            Bins = NumberBins,
            ColorVariables = GroupVars,
            SizeVar1 = SizeVars,
            FacetVar1 = FacetVar1,
            FacetVar2 = FacetVar2,
            YTicks = YTicks,
            XTicks = XTicks,
            OutlierSize = OutlierSize,
            OutlierColor = OutlierColor,
            FillColor = FillColor,
            BarPlotAggMethod = BarPlotAgg,
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
            print(paste0('ZVar = ', RemixAutoML:::CEP(ZVar)))
            print(paste0('Bins = ', RemixAutoML:::CEP(NumberBins)))
            print(paste0('ColorVariables = ', RemixAutoML:::CEP(GroupVars[[1L]])))
            print(paste0('SizeVar1 = ', RemixAutoML:::CEP(SizeVars)))
            print(paste0('FacetVar1 = ', RemixAutoML:::CEP(FacetVar1)))
            print(paste0('FacetVar2 = ', RemixAutoML:::CEP(FacetVar2)))
            print(paste0('YTicks = ', RemixAutoML:::CEP(YTicks)))
            print(paste0('XTicks = ', RemixAutoML:::CEP(XTicks)))
            print(paste0('OutlierSize = ', RemixAutoML:::CEP(OutlierSize)))
            print(paste0('OutlierColor = ', RemixAutoML:::CEP(OutlierColor)))
            print(paste0('FillColor = ', RemixAutoML:::CEP(FillColor)))
            print(paste0("BarPlotAggMethod = ", RemixAutoML:::CEP(BarPlotAgg)))
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
            ", ZVar=", RemixAutoML:::CEP(ZVar),
            ", Bins=", RemixAutoML:::CEP(NumberBins),
            ", ColorVariables=", RemixAutoML:::CEP(GroupVars[[1L]]),
            ", SizeVar1=", RemixAutoML:::CEP(SizeVars),
            ", FacetVar1=", RemixAutoML:::CEP(FacetVar1),
            ", FacetVar2=", RemixAutoML:::CEP(FacetVar2),
            ", YTicks=", RemixAutoML:::CEP(YTicks),
            ", XTicks=", RemixAutoML:::CEP(XTicks),
            ", OutlierSize=", RemixAutoML:::CEP(OutlierSize),
            ", OutlierColor=", RemixAutoML:::CEP(OutlierColor),
            ", FillColor=", RemixAutoML:::CEP(FillColor),
            ", BarPlotAggMethod= ", RemixAutoML:::CEP(BarPlotAgg),
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

          # Debug
          if(Debug) {
            print('data1: '); print(data1)
            print('PlotType'); print(PlotType)
            print('YVar'); print(YVar)
            print('ScoreVar'); print(ScoreVar)
            print('XVar'); print(XVar)
            print('GamFitSCatter'); print(GamFitScatter)
            print('Percentile_Buckets'); print(Percentile_Buckets)
            print('ShapAgg'); print(ShapAgg)
            print('Debug'); print(Debug)
          }

          # Build Plot
          PlotCollectionList[[paste0('p', run)]] <- RemixAutoML:::AppModelInsights(
            dt = data1,
            PlotType = PlotType,
            TargetVar = YVar,
            TargetLevel = TargetLevel,
            PredictVar = ScoreVar,
            PDPVar = XVar,
            DateVar = NULL,
            FacetVar1 = NULL,
            FacetVar2 = NULL,
            GamFit = GamFitScatter,
            Buckets = Percentile_Buckets,
            ShapAgg = ShapAgg,
            Debug = Debug)

          # Debugging
          if(Debug) {
            print('AppModelInsights finished building. Code Collection next')
            print(data1)
            print(ShapAgg)
            print('You are right here and now 1')
            print(class(PlotCollectionList[[paste0('p', run)]]))
            print(YVar)
            print(length(YVar))
            print(RemixAutoML:::CEP(YVar))
            print(paste0("TargetVar=", if(length(YVar) != 0) RemixAutoML:::CEP(YVar) else 'NULL'))
            print(paste0("PredictVar=", if(length(ScoreVar) != 0) RemixAutoML:::CEP(ScoreVar) else 'NULL'))
            print(paste0("PDPVar=", XVar))
            print(paste0("GamFit=", RemixAutoML:::CEPP(GamFitScatter)))
            print(paste0("Buckets=", RemixAutoML:::CEP(Percentile_Buckets)))
          }

          # Code Collection
          CodeCollection[[run]][[length(CodeCollection[[run]])+1L]] <- paste0(
            "RemixAutoML:::AppModelInsights(dt=data1, PlotType=", RemixAutoML:::CEP(PlotType),
            ", TargetVar=", RemixAutoML:::CEP(YVar),
            ", PredictVar=", RemixAutoML:::CEP(ScoreVar),
            ", PDPVar=", RemixAutoML:::CEP(XVar),
            ", FacetVar1 = ", if(PlotType %in% 'S__hapleyVarImp') RemixAutoML:::CEP(FacetVar1) else "NULL",
            ", FacetVar2 = ", if(PlotType %in% 'S__hapleyVarImp') RemixAutoML:::CEP(FacetVar2) else "NULL",
            ", GamFit=", RemixAutoML:::CEPP(GamFitScatter),
            ", Buckets=", RemixAutoML:::CEP(Percentile_Buckets), ")")

          # Code Collection
          if(length(names(PlotCollectionList)) > 0L) {

            # Update ChartTheme
            print(names(PlotCollectionList))
            print(PlotCollectionList[[paste0('p', run)]]$layers)
            PlotCollectionList[[paste0('p', run)]] <- PlotCollectionList[[paste0('p', run)]] + RemixAutoML::ChartTheme(
              Size = TextSize, AngleX = AngleX, AngleY = AngleY, ChartColor = ChartColor,
              BorderColor = BorderColor, TextColor = TextColor, GridColor = GridColor,
              BackGroundColor = BackGroundColor, SubTitleColor = SubTitleColor,
              LegendPosition = if(PlotType %in% c('ShapelyImportance','VariableImportance','GainsPlot')) 'none' else LegendPosition,
              LegendBorderSize = if(PlotType %in% c('ShapelyImportance','VariableImportance','GainsPlot')) NULL else as.numeric(LegendBorderSize),
              LegendLineType = if(PlotType %in% c('ShapelyImportance','VariableImportance','GainsPlot')) NULL else LegendLineType)

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
              ", LegendPosition=", RemixAutoML:::CEP(if(PlotType %in% c('ShapelyImportance','VariableImportance','GainsPlot')) 'none' else LegendPosition),
              ", LegendBorderSize=", RemixAutoML:::CEP(if(PlotType %in% c('ShapelyImportance','VariableImportance','GainsPlot')) NULL else as.numeric(LegendBorderSize)),
              ", LegendLineType=", RemixAutoML:::CEP(if(PlotType %in% c('ShapelyImportance','VariableImportance','GainsPlot')) NULL else LegendBorderSize), ")")
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

          # Print to UI
          if(exists("PlotCollectionList") && length(names(PlotCollectionList)) != 0) {

            # Number of plots
            N <- length(PlotCollectionList)
            if(Debug) print(paste0('Length of N = ', N))

            # Build Plots
            if(N == 1L) {

              # Store plot object
              p1 <- PlotCollectionList[[paste0('p', PlotRefs[1L])]]

              # Plotly
              output$Trendggplot2 <- NULL
              output$TrendPlotly <- plotly::renderPlotly({
                p1 <<- p1
                RemixAutoML:::PlotlyConversion(p1)
              })

            } else if(N == 2L) {

              # Store plot objects
              p1 <- PlotCollectionList[[paste0('p', PlotRefs[1L])]]
              p2 <- PlotCollectionList[[paste0('p', PlotRefs[2L])]]

              # Output
              output$TrendPlotly <- plotly::renderPlotly({
                p1 <- p1 + ggplot2::labs(title = NULL, subtitle = NULL)
                p2 <- p2 + ggplot2::labs(title = NULL, subtitle = NULL)
                p1 <- RemixAutoML:::PlotlyConversion(p1)
                p2 <- RemixAutoML:::PlotlyConversion(p2)
                plotly::subplot(p1, p2, nrows = 2, shareX = FALSE, shareY = FALSE)
              })

            } else if(N == 3L) {

              # Store plot objects
              p1 <- PlotCollectionList[[paste0('p', PlotRefs[1L])]]
              p2 <- PlotCollectionList[[paste0('p', PlotRefs[2L])]]
              p3 <- PlotCollectionList[[paste0('p', PlotRefs[3L])]]

              # Plotly Output
              output$TrendPlotly <- plotly::renderPlotly({
                shiny::req(PlotEngine == 'plotly')
                p1 <- p1 + ggplot2::labs(title = NULL, subtitle = NULL)
                p2 <- p2 + ggplot2::labs(title = NULL, subtitle = NULL)
                p3 <- p3 + ggplot2::labs(title = NULL, subtitle = NULL)
                p1 <- RemixAutoML:::PlotlyConversion(p1)
                p2 <- RemixAutoML:::PlotlyConversion(p2)
                p3 <- RemixAutoML:::PlotlyConversion(p3)
                plotly::subplot(p1, p2, p3, nrows = 2, shareX = FALSE, shareY = FALSE)
              })

            } else if(N == 4L) {

              # Store plot objects
              p1 <- PlotCollectionList[[paste0('p', PlotRefs[1L])]]
              p2 <- PlotCollectionList[[paste0('p', PlotRefs[2L])]]
              p3 <- PlotCollectionList[[paste0('p', PlotRefs[3L])]]
              p4 <- PlotCollectionList[[paste0('p', PlotRefs[4L])]]

              # Plotly
              output$TrendPlotly <- plotly::renderPlotly({
                shiny::req(PlotEngine == 'plotly')
                p1 <- p1 + ggplot2::labs(title = NULL, subtitle = NULL)
                p2 <- p2 + ggplot2::labs(title = NULL, subtitle = NULL)
                p3 <- p3 + ggplot2::labs(title = NULL, subtitle = NULL)
                p4 <- p4 + ggplot2::labs(title = NULL, subtitle = NULL)
                p1 <- RemixAutoML:::PlotlyConversion(p1)
                p2 <- RemixAutoML:::PlotlyConversion(p2)
                p3 <- RemixAutoML:::PlotlyConversion(p3)
                p4 <- RemixAutoML:::PlotlyConversion(p4)
                plotly::subplot(p1, p2, p3, p4, nrows = 2, shareX = FALSE, shareY = FALSE)
              })

            }

          } else {

            # Empty plot for errors
            output$TrendPlotly <- plotly::renderPlotly({
              plotly::ggplotly(RemixAutoML:::BlankPlot())
            })

            # Send Error Message
            shinyWidgets::sendSweetAlert(session, title = NULL, text = 'Plot could not build. Check for missing variables, such as Date Variables.', type = NULL, btn_labels = "error", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
          }

        } else {

          # Empty plot for errors
          output$TrendPlotly <- plotly::renderPlotly({
            plotly::ggplotly(RemixAutoML:::BlankPlot())
          })

          if(Debug) print('length(names(PlotCollectionList)) == length(PlotRefs) was FALSE')

        } # end Plot Build
      }
    }

    # Output blank grapth is list is empty
    if(length(PlotCollectionList) == 0) {
      output$TrendPlotly <- plotly::renderPlotly({
        plotly::ggplotly(RemixAutoML:::BlankPlot())
      })
    }

  })

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # :: Obs Event Print Code to UI        ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  shiny::observeEvent(input$PrintCodeButton, {
    if(Debug) {print('Print Code UI Begin'); print(paste0('Check if CodeCollection exists: exists = ', exists('CodeCollection')))}
    if(exists('CodeCollection')) {
      output$PrintCode <- shiny::renderPrint({
        shiny::HTML(paste0(unlist(CodeCollection), sep = '<br/>'))
      })
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = 'No Code Collected, Yet', type = NULL, btn_labels = "warning", btn_colors = "yellow", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  })

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  #    Close app after closing browser   ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  session$onSessionEnded(function() {
  suppressWarnings(rm(BlobStorageURL, PlotObjectHome, CodeCollection, data1, PlotCollectionList, SubsetList, rawfiles, cont, ModelOutputList, envir = .GlobalEnv))
    shiny::stopApp()
  })
}

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
#    Run App                           ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
shiny::shinyApp(ui = ui, server = server)

# ----

# ----

# TODO: possibly allow labeling updating ----
# # Plot Labels (col 3)
# shiny::column(
#   width = 1L,
#   tags$h4('Formatting'),
#   shinyWidgets::dropdown(
#     right = FALSE, animate = TRUE, circle = FALSE, tooltip = FALSE, status = "primary", icon = icon("gear"), width = LogoWidth,
#     tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Plot Formatting')),
#     tags$h4(tags$span(style=paste0('color: ', H4Color, ';'),'For color options, see Plot Colors')),
#     RemixAutoML:::BlankRow(AppWidth),
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
#       shiny::column(3L, shiny::uiOutput('SubTitle1')),
#       shiny::column(3L, shiny::uiOutput('SubTitle2')),
#       shiny::column(3L, shiny::uiOutput('SubTitle3')),
#       shiny::column(3L, shiny::uiOutput('SubTitle4')))
#
#
#     )), # column end


# Other values
# output$SampleSize <- shiny::renderUI({
#   RemixAutoML:::NumericInput(InputID = 'SampleSize', Label = tags$span(style='color: blue;', 'Sample size for plotting'), Step = 50000, Min = 0, Max = 1000000, Value = 100000)
# })
# output$LegendLineType1 <- shiny::renderUI({
#   RemixAutoML:::SelectizeInput(InputID = 'LegendLineType1', Label = tags$span(style='color: blue;', 'Legend Border Type'), Choices = c('solid','blank','dashed','dotdash','dotted','longlash','twodash'), SelectedDefault = 'solid', Multiple = FALSE)
# })
# output$LegendLineType2 <- shiny::renderUI({
#   RemixAutoML:::SelectizeInput(InputID = 'LegendLineType2', Label = tags$span(style='color: blue;', 'Legend Border Type'), Choices = c('solid','blank','dashed','dotdash','dotted','longlash','twodash'), SelectedDefault = 'solid', Multiple = FALSE)
# })
# output$LegendLineType3 <- shiny::renderUI({
#   RemixAutoML:::SelectizeInput(InputID = 'LegendLineType3', Label = tags$span(style='color: blue;', 'Legend Border Type'), Choices = c('solid','blank','dashed','dotdash','dotted','longlash','twodash'), SelectedDefault = 'solid', Multiple = FALSE)
# })
# output$LegendLineType4 <- shiny::renderUI({
#   RemixAutoML:::SelectizeInput(InputID = 'LegendLineType4', Label = tags$span(style='color: blue;', 'Legend Border Type'), Choices = c('solid','blank','dashed','dotdash','dotted','longlash','twodash'), SelectedDefault = 'solid', Multiple = FALSE)
# })

# ----

# ----
