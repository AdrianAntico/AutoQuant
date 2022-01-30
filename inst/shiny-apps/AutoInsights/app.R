options(shiny.maxRequestSize = 250000*1024^2)
library(curl)

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# Environment Setup                    ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
StartEnv <- as.list(environment())
library(data.table)
data.table::setDTthreads(threads = max(1L, parallel::detectCores()-1L))
options(scipen = 999)

# For dragula naming - convert to display, convert back to utilize
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
PlotNamesLookup[['ShapleyVarImp']] <- c('ShapleyVarImp')

# Initialize Data
data <- NULL

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
      id = "sidebar",

      # Login Page
      RemixAutoML::BlankRow(AppWidth),
      shinydashboard::menuItem(text="Login", tabName="Login", icon=shiny::icon("sign-in-alt"), selected = TRUE),

      # Load Data Page
      RemixAutoML::BlankRow(AppWidth),
      shinydashboard::menuItem(text="Import Data", tabName='LoadDataPage', icon=shiny::icon("database")),

      # Plotting Page
      RemixAutoML::BlankRow(AppWidth),
      shinydashboard::menuItem(text="Create Plots", tabName='Plotter', icon=shiny::icon("chart-line")),

      # Code Print Page
      RemixAutoML::BlankRow(AppWidth),
      shinydashboard::menuItem(text="Print Code", tabName='CodePrint', icon=shiny::icon("code")))),

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # DashboardBody                        ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  shinydashboard::dashboardBody(

    # Style Sheet Reference ----
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
        RemixAutoML:::LoadDataInputs(id='LoadDataInputs', AppWidth=AppWidth, LogoWidth=LogoWidth, SolidHeader=TRUE, BoxTitle=NULL, BoxStatus='danger', CSV_h4='Local .csv Data', DropdownRight=FALSE, DropDownAnimate=TRUE, DropDownStatus='custom'),

        # Button to Load Data
        RemixAutoML:::LoadDataButton(id = 'DataButton', AppWidth = AppWidth)),

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

        # Box ----
        shinydashboard::box(
          title = NULL,
          solidHeader = TRUE, collapsible = FALSE, status = 'danger', width = AppWidth,

          # Add Space
          RemixAutoML::BlankRow(AppWidth),

          # Plot DropDown Buttons and Contents ----
          shiny::fluidRow(
            width=AppWidth,
            RemixAutoML:::PlotDropDownContents(id='PlotDropDown', PlotNumber=1L, AppWidth=AppWidth, LogoWidth=LogoWidth, ButtonWidth=3L, Align='center', DropDownRight=FALSE, Animate=TRUE, Status='custom', H3Color=H3Color),
            RemixAutoML:::PlotDropDownContents(id='PlotDropDown', PlotNumber=2L, AppWidth=AppWidth, LogoWidth=LogoWidth, ButtonWidth=3L, Align='center', DropDownRight=FALSE, Animate=TRUE, Status='custom', H3Color=H3Color),
            RemixAutoML:::PlotDropDownContents(id='PlotDropDown', PlotNumber=3L, AppWidth=AppWidth, LogoWidth=LogoWidth, ButtonWidth=3L, Align='center', DropDownRight=FALSE, Animate=TRUE, Status='custom', H3Color=H3Color),
            RemixAutoML:::PlotDropDownContents(id='PlotDropDown', PlotNumber=4L, AppWidth=AppWidth, LogoWidth=LogoWidth, ButtonWidth=3L, Align='center', DropDownRight=FALSE, Animate=TRUE, Status='custom', H3Color=H3Color)), # end of fluidrow

          # Add Space to act as a bigger boarder for box
          RemixAutoML::BlankRow(AppWidth),

        ), # End of box

        # Add Space to act as a border, just like the dragula box
        RemixAutoML::BlankRow(AppWidth),

        # ----

        # ----

        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
        # Box with Dragula and Extra Buttons   ----
        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

        # Box ----
        shinydashboard::box(
          title = NULL, solidHeader = TRUE, collapsible = FALSE, status = 'danger', width = AppWidth,

          # Plotting Variables
          shiny::fluidRow(
            width=AppWidth,

            # Dragula Boxes          ----
            shiny::column(width = 12L, align = 'center', shiny::uiOutput('PlotTypeDragula')),

            # Create Plot Button     ----
            shiny::column(
              width = 3L, shinyjs::useShinyjs(), align='center', tags$h4(tags$b('~ Build Plot')),
              shinyWidgets::actionBttn(inputId='TrendPlotExecute', label='Build Plot', style='gradient', color='royal')),

            # Global Settings        ----
            RemixAutoML:::GlobalSettingsContents(id='GlobalSettings', PlotHeight=PlotHeight, PlotWidth=PlotWidth, AppWidth=AppWidth, LogoWidth=LogoWidth, H3Color=H3Color, Right=FALSE, Animate=TRUE, Status='custom'),

            # Formatting DropDown    ----
            shiny::column(
              width = 3L,
              align = 'center',
              tags$h4(tags$b('Formatting')),
              shinyWidgets::dropdown(
                right = FALSE, animate = TRUE, circle = FALSE, tooltip = FALSE, status = "custom", inputId = "Plot-Formatting-Parent", width = LogoWidth,
                tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Plot Enhancements')),
                tags$h4(tags$span(style=paste0('color: ', H4Color, ';'),'Additional MetaData Selection for Plot Enhancements')),
                RemixAutoML::BlankRow(AppWidth),

                # Plot Formatting
                shiny::fluidRow(

                  # Plot Axes Limits ----
                  RemixAutoML:::AxisLimits(id='AxisLimitsContents', AppWidth=AppWidth, LogoWidth=LogoWidth, H3Color=H3Color, H4Color=H4Color, Right=FALSE, Animate=TRUE, Status='custom'),

                  # Plot Formatting  ----
                  RemixAutoML:::Formatting(id='FormattingContents', AppWidth=AppWidth, LogoWidth=LogoWidth, H3Color=H3Color, H4Color=H4Color, Right=FALSE, Animate=TRUE, Status='custom'),

                  # Plot Colors      ----
                  RemixAutoML:::Coloring(id='ColoringContents', AppWidth=AppWidth, LogoWidth=LogoWidth, H3Color=H3Color, H4Color=H4Color, Right=FALSE, Animate=TRUE, Status='custom')), # fluidrow end


                # Add Space
                RemixAutoML::BlankRow(AppWidth),

                # Plot Enhancements
                shiny::fluidRow(

                  # Gam Fitting
                  RemixAutoML:::GamFitting(id='GamFittingContents', AppWidth=AppWidth, LogoWidth=LogoWidth, H3Color=H3Color, H4Color=H4Color, Right=FALSE, Animate=TRUE, Status='custom'),

                  # Histogram bins
                  RemixAutoML:::HistBins(id='HistBinsContents', AppWidth=AppWidth, LogoWidth=LogoWidth, H3Color=H3Color, H4Color=H4Color, Right=FALSE, Animate=TRUE, Status='custom'),

                  # Percentile Bins
                  RemixAutoML:::PercentileBuckets(id='PercentileBucketsContents', AppWidth=AppWidth, LogoWidth=LogoWidth, H3Color=H3Color, H4Color=H4Color, Right=TRUE, Animate=TRUE, Status='custom'),

                  # Shapely Aggregation Method
                  RemixAutoML:::ShapAgg(id='ShapAggContents', AppWidth=AppWidth, LogoWidth=LogoWidth, H3Color=H3Color, H4Color=H4Color, Right=TRUE, Animate=TRUE, Status='custom')) # end fluid row

              ) # end of dropdown
            ), # end of column plot format dropdown inputs ::::::::


            # Filtering Dropdown     ----
            shiny::column(
              width = 3L,
              align = 'center',
              tags$h4(tags$b('Filters')),
              shinyWidgets::dropdown(
                right = TRUE, animate = TRUE, circle = FALSE, tooltip = FALSE, status = "custom", width = LogoWidth,
                tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Filter Variables, Logic, and Values')),
                tags$h4(tags$span(style=paste0('color: ', H4Color, ';'),'Filters for subsetting data')),
                RemixAutoML::BlankRow(12L),

                # Filter Variables ----
                shiny::fluidRow(
                  RemixAutoML:::DataFilters(id='FiltersDropDownContents', PlotNumber=1, Status='custom', AppWidth=AppWidth, LogoWidth=LogoWidth, H3Color=H3Color, H4Color = H4Color, Right=FALSE, Animate=TRUE),
                  RemixAutoML:::DataFilters(id='FiltersDropDownContents', PlotNumber=2, Status='custom', AppWidth=AppWidth, LogoWidth=LogoWidth, H3Color=H3Color, H4Color = H4Color, Right=FALSE, Animate=TRUE),
                  RemixAutoML:::DataFilters(id='FiltersDropDownContents', PlotNumber=3, Status='custom', AppWidth=AppWidth, LogoWidth=LogoWidth, H3Color=H3Color, H4Color = H4Color, Right=FALSE, Animate=TRUE),
                  RemixAutoML:::DataFilters(id='FiltersDropDownContents', PlotNumber=4, Status='custom', AppWidth=AppWidth, LogoWidth=LogoWidth, H3Color=H3Color, H4Color = H4Color, Right=FALSE, Animate=TRUE)), # fluidrow end

                # Extra space at bottom of dropdown after filter buttoms
                RemixAutoML::BlankRow(AppWidth),

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
        shiny::fluidRow(shiny::column(width = 3L, shinyjs::useShinyjs(), shinyWidgets::actionBttn(inputId='PrintCodeButton', label='Print Code', icon=shiny::icon('chevron-right', lib = 'font-awesome'), style='gradient', color=eval(CreatePlotButtonColor)))),

        # Print Code
        RemixAutoML::BlankRow(AppWidth),
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
  output$blob <- shiny::renderUI({
    if(Debug) paste0('https://', StorageAccount, '.blob.core.windows.net/', Container)
    BlobStorageURL <- paste0('https://', StorageAccount, '.blob.core.windows.net/', Container)
    assign(x = 'BlobStorageURL', value = BlobStorageURL, envir = .GlobalEnv)
    cont <<- AzureStor::blob_container(BlobStorageURL, key = Key)
    rawfiles <- AzureStor::list_storage_files(cont, info = 'name')
    rawfiles <<- rawfiles[c(which(grepl(pattern = '.csv', x = rawfiles)), which(grepl(pattern = '.Rdata', x = rawfiles)))]
    RemixAutoML::SelectizeInput(
      InputID = 'blob',
      Label = 'Select Azure .csv File',
      Choices = rawfiles[which(grepl(pattern = '.csv', x = rawfiles))],
      SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1, CloseAfterSelect = TRUE, Debug = Debug)
  })
  output$rdatablob <- shiny::renderUI({
    RemixAutoML::SelectizeInput(
      InputID = 'rdatablob',
      Label = 'Select Azure .Rdata File',
      Choices = rawfiles[which(grepl(pattern = '.Rdata', x = rawfiles))],
      SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1, CloseAfterSelect = TRUE, Debug = Debug)
  })

  # Load Azure Data
  shiny::observeEvent(eventExpr = input$LoadAzure, {

    # csv
    FileName <- tryCatch({input[['blob']]}, error = function(x) NULL)
    if(Debug) print(FileName)
    if(length(FileName) != 0 && FileName != "Load" && FileName != "") {
      data
      AzureStor::download_blob(container = cont, src = input[['blob']], dest = file.path('/inputdata', input[['blob']]), overwrite=TRUE)
    }

    # .Rdata
    inFile2 <- tryCatch({input[['rdatablob']]}, error = function(x) NULL)
    if(!is.null(inFile2)) print(inFile2)
    if(length(inFile2) != 0 && inFile2 != "") {
      if(Debug) {print('data check 3')}
      AzureStor::download_blob(container = cont, src = input[['rdatablob']], dest = file.path('/inputdata', input[['rdatablob']]), overwrite=TRUE)
    }
  })

  # Load data event
  shiny::observeEvent(eventExpr = input$LoadDataButton, {

    # ----

    # ----

    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    # Load Data Sets and Rdata             ----
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

    # Local data loading
    CodeCollection <- list()
    if(!is.null(input[['DataLoad']])) {
      data <<- RemixAutoML::ReactiveLoadCSV(Infile = input[['DataLoad']], ProjectList = NULL, DateUpdateName = NULL, RemoveObjects = NULL, Debug = Debug)
    } else {
      data <<- NULL
    }

    # Load ModelOutputList
    if(Debug) print('data check 2')
    inFile1 <- tryCatch({input[['ModelObjectLoad']]}, error = function(x) NULL)
    if(Debug) print(inFile1[['datapath']])
    if(!is.null(inFile1)) {
      if(Debug) print('loading .Rdata')
      e <- new.env()
      name <- load(inFile1[['datapath']], e)
      if(Debug) print('store ModelOutputList globally')
      ModelOutputList <<- e[[name]]
      if(!is.null(ModelOutputList$TrainData) && !is.null(ModelOutputList$TestData)) {
        ModelData <<- data.table::rbindlist(list(ModelOutputList$TrainData, ModelOutputList$TestData), use.names = TRUE, fill = TRUE)
      } else if(is.null(ModelOutputList$TrainData) && !is.null(ModelOutputList$TestData)) {
        ModelData <<- ModelOutputList$TestData
      } else if(!is.null(ModelOutputList$TrainData) && is.null(ModelOutputList$TestData)) {
        ModelData <<- ModelOutputList$TrainData
      } else {
        ModelData <<- NULL
      }
      if(Debug) print(class(ModelOutputList))
      if(Debug) print(length(ModelOutputList))
      if(Debug) print(length(ModelOutputList$PlotList))
      if(Debug) print(names(ModelOutputList$PlotList))
    }

    # Azure .csv
    if(Debug) {print(input$blob); print(!is.null(input$blob))}
    if(!is.null(input$blob)) {
      data <<- RemixAutoML::ReactiveLoadCSV(Infile = file.path('/inputdata', input[['blob']]), ProjectList = NULL, DateUpdateName = NULL, RemoveObjects = NULL, Debug = Debug)
    }

    # Load ModelOutputList
    inFile1 <- tryCatch({input[['rdatablob']]}, error = function(x) NULL)
    if(Debug) print(inFile1)
    if(!is.null(inFile1)) {
      if(Debug) print('loading .Rdata')
      e <- new.env()
      print(file.exists(file.path('/inputdata', inFile1)))
      name <- load(file.path('/inputdata', inFile1), e)
      print(name)
      if(Debug) print('store ModelOutputList globally')
      ModelOutputList <<- e[[name]]
      if(!is.null(ModelOutputList$TrainData) && !is.null(ModelOutputList$TestData)) {
        ModelData <<- data.table::rbindlist(list(ModelOutputList$TrainData, ModelOutputList$TestData), use.names = TRUE, fill = TRUE)
      } else if(is.null(ModelOutputList$TrainData) && !is.null(ModelOutputList$TestData)) {
        ModelData <<- ModelOutputList$TestData
      } else if(!is.null(ModelOutputList$TrainData) && is.null(ModelOutputList$TestData)) {
        ModelData <<- ModelOutputList$TrainData
      } else {
        ModelData <<- NULL
      }

      if(Debug) print(class(ModelOutputList))
      if(Debug) print(length(ModelOutputList))
      if(Debug) print(length(ModelOutputList$PlotList))
      if(Debug) print(names(ModelOutputList$PlotList))
    } else if(!exists('ModelOutputList')) {
      if(Debug) print('ModelOutputList not loaded')
      ModelOutputList <- NULL
      ModelData <- NULL
    }

    # Initialize
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

    # Plot Selection + reactives for enabling smart selection for YVar, XVar, etc.
    output$Plot1 <- shiny::renderUI({
      if(length(ModelOutputList) != 0 && length(names(ModelOutputList$PlotList)) != 0) bla <- names(ModelOutputList$PlotList) else bla <- NULL
      x <- RemixAutoML:::AvailableAppInsightsPlots(x = bla, PlotNamesLookup = PlotNamesLookup, Debug = Debug)
      if(Debug) {print('Plot1 Charts Available'); print(x)}
      RemixAutoML::SelectizeInput(InputID = 'Plot1', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Plot Type Selection'), Choices = c(x), Multiple = TRUE, MaxVars = 1)
    })
    output$Plot2 <- shiny::renderUI({
      if(length(ModelOutputList) != 0 && length(names(ModelOutputList$PlotList)) != 0) bla <- names(ModelOutputList$PlotList) else bla <- NULL
      x <- RemixAutoML:::AvailableAppInsightsPlots(x = bla, PlotNamesLookup = PlotNamesLookup, Debug = Debug)
      RemixAutoML::SelectizeInput(InputID = 'Plot2', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Plot Type Selection'), Choices = c(x), Multiple = TRUE, MaxVars = 1)
    })
    output$Plot3 <- shiny::renderUI({
      if(length(ModelOutputList) != 0 && length(names(ModelOutputList$PlotList)) != 0) bla <- names(ModelOutputList$PlotList) else bla <- NULL
      x <- RemixAutoML:::AvailableAppInsightsPlots(x = bla, PlotNamesLookup = PlotNamesLookup, Debug = Debug)
      RemixAutoML::SelectizeInput(InputID = 'Plot3', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Plot Type Selection'), Choices = c(x), Multiple = TRUE, MaxVars = 1)
    })
    output$Plot4 <- shiny::renderUI({
      if(length(ModelOutputList) != 0 && length(names(ModelOutputList$PlotList)) != 0) bla <- names(ModelOutputList$PlotList) else bla <- NULL
      x <- RemixAutoML:::AvailableAppInsightsPlots(x = bla, PlotNamesLookup = PlotNamesLookup, Debug = Debug)
      RemixAutoML::SelectizeInput(InputID = 'Plot4', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Plot Type Selection'), Choices = c(x), Multiple = TRUE, MaxVars = 1)
    })

    Plot1_react <- shiny::reactive({input[['Plot1']]})
    Plot2_react <- shiny::reactive({input[['Plot2']]})
    Plot3_react <- shiny::reactive({input[['Plot3']]})
    Plot4_react <- shiny::reactive({input[['Plot4']]})

    # YVars
    output$YVar1 <- shiny::renderUI({
      if('CorrMatrix' %in% tryCatch({Plot1_react()}, error = 'none')) {
        choices <- names(data)[which(RemixAutoML:::ColTypes(data) %in% c('numeric','integer'))]
        MaxVars <- 100
        if(length(choices) == 0) choices <- NULL
      } else {
        choices <- RemixAutoML:::VarNamesDisplay(DataNames=names(data), ModelDataNames=names(ModelData), PlotName=tryCatch({Plot1_react()}, error = function(x) 'Scatter'))
        MaxVars <- 1
      }
      RemixAutoML::SelectizeInput(InputID = 'YVar1', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Y-Variable'), Choices = choices, Multiple = TRUE, MaxVars = MaxVars)
    })
    output$YVar2 <- shiny::renderUI({
      if('CorrMatrix' %in% tryCatch({Plot2_react()}, error = 'none')) {
        MaxVars <- 100
        choices <- names(data)[which(RemixAutoML:::ColTypes(data) %in% c('numeric','integer'))]
        if(length(choices) == 0) choices <- NULL
      } else {
        MaxVars <- 1
        choices <- RemixAutoML:::VarNamesDisplay(DataNames=names(data), ModelDataNames=names(ModelData), PlotName=tryCatch({Plot2_react()}, error = function(x) 'Scatter'))
      }
      RemixAutoML::SelectizeInput(InputID = 'YVar2', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Y-Variable'), Choices = choices, Multiple = TRUE, MaxVars = MaxVars)
    })
    output$YVar3 <- shiny::renderUI({
      if('CorrMatrix' %in% tryCatch({Plot3_react()}, error = 'none')) {
        MaxVars <- 100
        choices <- names(data)[which(RemixAutoML:::ColTypes(data) %in% c('numeric','integer'))]
        if(length(choices) == 0) choices <- NULL
      } else {
        MaxVars <- 1
        choices <- RemixAutoML:::VarNamesDisplay(DataNames=names(data), ModelDataNames=names(ModelData), PlotName=tryCatch({Plot3_react()}, error = function(x) 'Scatter'))
      }
      RemixAutoML::SelectizeInput(InputID = 'YVar3', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Y-Variable'), Choices = choices, Multiple = TRUE, MaxVars = MaxVars)
    })
    output$YVar4 <- shiny::renderUI({
      if('CorrMatrix' %in% tryCatch({Plot4_react()}, error = 'none')) {
        MaxVars <- 100
        choices <- names(data)[which(RemixAutoML:::ColTypes(data) %in% c('numeric','integer'))]
        if(length(choices) == 0) choices <- NULL
      } else {
        MaxVars <- 1
        choices <- RemixAutoML:::VarNamesDisplay(DataNames=names(data), ModelDataNames=names(ModelData), PlotName=tryCatch({Plot4_react()}, error = function(x) 'Scatter'))
      }
      RemixAutoML::SelectizeInput(InputID = 'YVar4', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Y-Variable'), Choices = choices, Multiple = TRUE, MaxVars = MaxVars)
    })

    # 'X-Variables'
    output$XVar1 <- shiny::renderUI({
      if('CorrMatrix' %in% tryCatch({Plot1_react()}, error = 'none')) {
        MaxVars <- 100
        choices <- NULL
      } else {
        MaxVars <- 1
        choices <- RemixAutoML:::VarNamesDisplay(DataNames=names(data), ModelDataNames=names(ModelData), PlotName=tryCatch({Plot1_react()}, error = function(x) 'Scatter'))
      }
      RemixAutoML::SelectizeInput(InputID = 'XVar1', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'X-Variable'), Choices = choices, Multiple = TRUE, MaxVars = MaxVars)
    })
    output$XVar2 <- shiny::renderUI({
      if('CorrMatrix' %in% tryCatch({Plot2_react()}, error = 'none')) {
        MaxVars <- 100
        choices <- NULL
      } else {
        MaxVars <- 1
        choices <- RemixAutoML:::VarNamesDisplay(DataNames=names(data), ModelDataNames=names(ModelData), PlotName=tryCatch({Plot2_react()}, error = function(x) 'Scatter'))
      }
      RemixAutoML::SelectizeInput(InputID = 'XVar2', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'X-Variable'), Choices = choices, Multiple = TRUE, MaxVars = MaxVars)
    })
    output$XVar3 <- shiny::renderUI({
      if('CorrMatrix' %in% tryCatch({Plot3_react()}, error = 'none')) {
        MaxVars <- 100
        choices <- NULL
      } else {
        MaxVars <- 1
        choices <- RemixAutoML:::VarNamesDisplay(DataNames=names(data), ModelDataNames=names(ModelData), PlotName=tryCatch({Plot3_react()}, error = function(x) 'Scatter'))
      }
      RemixAutoML::SelectizeInput(InputID = 'XVar3', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'X-Variable'), Choices = choices, Multiple = TRUE, MaxVars = MaxVars)
    })
    output$XVar4 <- shiny::renderUI({
      if('CorrMatrix' %in% tryCatch({Plot4_react()}, error = 'none')) {
        MaxVars <- 100
        choices <- NULL
      } else {
        MaxVars <- 1
        choices <- RemixAutoML:::VarNamesDisplay(DataNames=names(data), ModelDataNames=names(ModelData), PlotName=tryCatch({Plot4_react()}, error = function(x) 'Scatter'))
      }
      RemixAutoML::SelectizeInput(InputID = 'XVar4', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'X-Variable'), Choices = choices, Multiple = TRUE, MaxVars = MaxVars)
    })

    # Reactives References
    YVar1 <- shiny::reactive({shiny::req(input[['YVar1']])})
    YVar2 <- shiny::reactive({shiny::req(input[['YVar2']])})
    YVar3 <- shiny::reactive({shiny::req(input[['YVar3']])})
    YVar4 <- shiny::reactive({shiny::req(input[['YVar4']])})
    XVar1 <- shiny::reactive({shiny::req(input[['XVar1']])})
    XVar2 <- shiny::reactive({shiny::req(input[['XVar2']])})
    XVar3 <- shiny::reactive({shiny::req(input[['XVar3']])})
    XVar4 <- shiny::reactive({shiny::req(input[['XVar4']])})

    # ----

    # ----

    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    # Plotting MetaData                    ----
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

    # Auto SCaling of Plot Grid: doubles the size in the event of more than 1 plot
    output$AutoGridHorizontal <-  shiny::renderUI({
      shinyWidgets::materialSwitch(inputId = "AutoGridHorizontal", label = tags$span(style='color: blue;', 'Auto Grid Scale'), status = "danger", value = TRUE, inline = TRUE, width = '100%')
    })

    # Dragula for PlotType
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
    output$PlotWidth <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID = "PlotWidth", Label=tags$span(style='color: blue;', 'Plot Width'), Step = 50, Min = 500, Max = 3500, Value = 1600)
    })
    output$PlotHeight <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID = "PlotHeight", Label=tags$span(style='color: blue;', 'Plot Height'), Step = 25, Min = 300, Max = 3500, Value = 500)
    })

    # Percentile Buckets
    output$Percentile_Buckets1 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID='Percentile_Buckets1', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Percentile Buckets 1'), Choices=1:100, SelectedDefault=20, Multiple=FALSE)
    })
    output$Percentile_Buckets2 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID='Percentile_Buckets2', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Percentile Buckets 2'), Choices=1:100, SelectedDefault=20, Multiple=FALSE)
    })
    output$Percentile_Buckets3 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID='Percentile_Buckets3', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Percentile Buckets 3'), Choices=1:100, SelectedDefault=20, Multiple=FALSE)
    })
    output$Percentile_Buckets4 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID='Percentile_Buckets4', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Percentile Buckets 4'), Choices=1:100, SelectedDefault=20, Multiple=FALSE)
    })

    # Shapely Agg Method
    output$ShapAggMethod1 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID='ShapAggMethod1', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Shap Agg Method 1'), Choices=c('mean','absmean','meanabs','sd','median','absmedian','medianabs'), SelectedDefault='mean', Multiple=FALSE)
    })
    output$ShapAggMethod2 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID='ShapAggMethod2', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Shap Agg Method 2'), Choices=c('mean','absmean','meanabs','sd','median','absmedian','medianabs'), SelectedDefault='mean', Multiple=FALSE)
    })
    output$ShapAggMethod3 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID='ShapAggMethod3', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Shap Agg Method 3'), Choices=c('mean','absmean','meanabs','sd','median','absmedian','medianabs'), SelectedDefault='mean', Multiple=FALSE)
    })
    output$ShapAggMethod4 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID='ShapAggMethod4', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Shap Agg Method 4'), Choices=c('mean','absmean','meanabs','sd','median','absmedian','medianabs'), SelectedDefault='mean', Multiple=FALSE)
    })

    # Add GAM Fit to Plot
    output$GamFitScatter1 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID='GamFitScatter1', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Fit Gam on Scatter or Copula 1'), Choices=c('TRUE','FALSE'), SelectedDefault=FALSE, Multiple=FALSE)
    })
    output$GamFitScatter2 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID='GamFitScatter2', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Fit Gam on Scatter or Copula 2'), Choices=c('TRUE','FALSE'), SelectedDefault=FALSE, Multiple=FALSE)
    })
    output$GamFitScatter3 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID='GamFitScatter3', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Fit Gam on Scatter or Copula 3'), Choices=c('TRUE','FALSE'), SelectedDefault=FALSE, Multiple=FALSE)
    })
    output$GamFitScatter4 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID='GamFitScatter4', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Fit Gam on Scatter or Copula 4'), Choices=c('TRUE','FALSE'), SelectedDefault=FALSE, Multiple=FALSE)
    })

    # Histogram Bins
    output$NumberBins1 <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID='NumberBins1', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'# of Bins for Histogram 1'), Min=1, Max=1000, Step=5, Value=30)
    })
    output$NumberBins2 <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID='NumberBins2', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'# of Bins for Histogram 2'), Min=1, Max=1000, Step=5, Value=30)
    })
    output$NumberBins3 <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID='NumberBins3', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'# of Bins for Histogram 3'), Min=1, Max=1000, Step=5, Value=30)
    })
    output$NumberBins4 <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID='NumberBins4', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'# of Bins for Histogram 4'), Min=1, Max=1000, Step=5, Value=30)
    })

    # YTicks Values (NULL is whats handled by RemixAutoML:::YTicks())
    output$YTicks1 <- shiny::renderUI({
      yy <- tryCatch({YVar1()}, error = function(x) 'None')
      if(any(yy %in% names(data))) temp <- data else temp <- ModelData
      RemixAutoML::SelectizeInput(InputID = 'YTicks1', Label = tags$span(style='color: blue;', 'Y-Axis 1 Ticks'), Choices = RemixAutoML:::YTicks(temp, yvar = yy), SelectedDefault = 'Default', Multiple = TRUE)
    })
    output$YTicks2 <- shiny::renderUI({
      yy <- tryCatch({YVar2()}, error = function(x) 'None')
      if(any(yy %in% names(data))) temp <- data else temp <- ModelData
      RemixAutoML::SelectizeInput(InputID = 'YTicks2', Label = tags$span(style='color: blue;', 'Y-Axis 2 Ticks'), Choices = RemixAutoML:::YTicks(temp, yvar = yy), SelectedDefault = 'Default', Multiple = TRUE)
    })
    output$YTicks3 <- shiny::renderUI({
      yy <- tryCatch({YVar3()}, error = function(x) 'None')
      if(any(yy %in% names(data))) temp <- data else temp <- ModelData
      RemixAutoML::SelectizeInput(InputID = 'YTicks3', Label = tags$span(style='color: blue;', 'Y-Axis 3 Ticks'), Choices = RemixAutoML:::YTicks(temp, yvar = yy), SelectedDefault = 'Default', Multiple = TRUE)
    })
    output$YTicks4 <- shiny::renderUI({
      yy <- tryCatch({YVar4()}, error = function(x) 'None')
      if(any(yy %in% names(data))) temp <- data else temp <- ModelData
      RemixAutoML::SelectizeInput(InputID = 'YTicks4', Label = tags$span(style='color: blue;', 'Y-Axis 4 Ticks'), Choices = RemixAutoML:::YTicks(temp, yvar = yy), SelectedDefault = 'Default', Multiple = TRUE)
    })

    # XTicks Values ('None' is whats handled by RemixAutoML:::XTicks())
    output$XTicks1 <- shiny::renderUI({
      xx <- tryCatch({XVar1()}, error = function(x) 'None')
      dd <- tryCatch({DateVar1()}, error = function(x) 'None')
      if(any(xx %in% names(data))) temp <- data else temp <- ModelData
      RemixAutoML::SelectizeInput(InputID = 'XTicks1', Label = tags$span(style='color: blue;', 'X-Axis 1 Ticks'), Choices = RemixAutoML:::XTicks(temp, xvar=xx,datevar=dd), SelectedDefault = 'Default', Multiple = TRUE)
    })
    output$XTicks2 <- shiny::renderUI({
      xx <- tryCatch({XVar2()}, error = function(x) 'None')
      dd <- tryCatch({DateVar2()}, error = function(x) 'None')
      if(any(xx %in% names(data))) temp <- data else temp <- ModelData
      RemixAutoML::SelectizeInput(InputID = 'XTicks2', Label = tags$span(style='color: blue;', 'X-Axis 2 Ticks'), Choices = RemixAutoML:::XTicks(temp, xvar=xx,datevar=dd), SelectedDefault = 'Default', Multiple = TRUE)
    })
    output$XTicks3 <- shiny::renderUI({
      xx <- tryCatch({XVar3()}, error = function(x) 'None')
      dd <- tryCatch({DateVar3()}, error = function(x) 'None')
      if(any(xx %in% names(data))) temp <- data else temp <- ModelData
      RemixAutoML::SelectizeInput(InputID = 'XTicks3', Label = tags$span(style='color: blue;', 'X-Axis 3 Ticks'), Choices = RemixAutoML:::XTicks(temp, xvar=xx,datevar=dd), SelectedDefault = 'Default', Multiple = TRUE)
    })
    output$XTicks4 <- shiny::renderUI({
      xx <- tryCatch({XVar4()}, error = function(x) 'None')
      dd <- tryCatch({DateVar4()}, error = function(x) 'None')
      if(any(xx %in% names(data))) temp <- data else temp <- ModelData
      RemixAutoML::SelectizeInput(InputID = 'XTicks4', Label = tags$span(style='color: blue;', 'X-Axis 4 Ticks'), Choices = RemixAutoML:::XTicks(temp, xvar=xx,datevar=dd), SelectedDefault = 'Default', Multiple = TRUE)
    })

    # Other values
    output$SampleSize <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID = 'SampleSize', Label = tags$span(style='color: blue;', 'Sample size for plotting'), Step = 50000, Min = 0, Max = 1000000, Value = 100000)
    })
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
    output$TextSize1 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'TextSize1', Label = tags$span(style='color: blue;', 'Text Size'),Choices = c(as.character(seq(1,100,1))), SelectedDefault = '14', Multiple = FALSE)
    })
    output$TextSize2 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'TextSize2', Label = tags$span(style='color: blue;', 'Text Size'),Choices = c(as.character(seq(1,100,1))), SelectedDefault = '14', Multiple = FALSE)
    })
    output$TextSize3 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'TextSize3', Label = tags$span(style='color: blue;', 'Text Size'),Choices = c(as.character(seq(1,100,1))), SelectedDefault = '14', Multiple = FALSE)
    })
    output$TextSize4 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'TextSize4', Label = tags$span(style='color: blue;', 'Text Size'),Choices = c(as.character(seq(1,100,1))), SelectedDefault = '14', Multiple = FALSE)
    })
    output$OutlierSize1 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'OutlierSize1', Label = tags$span(style='color: blue;', 'Outlier Size'), Choices = c(seq(0.01,2,0.01)), SelectedDefault = 0.01, Multiple = FALSE)
    })
    output$OutlierSize2 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'OutlierSize2', Label = tags$span(style='color: blue;', 'Outlier Size'), Choices = c(seq(0.01,2,0.01)), SelectedDefault = 0.01, Multiple = FALSE)
    })
    output$OutlierSize3 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'OutlierSize3', Label = tags$span(style='color: blue;', 'Outlier Size'), Choices = c(seq(0.01,2,0.01)), SelectedDefault = 0.01, Multiple = FALSE)
    })
    output$OutlierSize4 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'OutlierSize4', Label = tags$span(style='color: blue;', 'Outlier Size'), Choices = c(seq(0.01,2,0.01)), SelectedDefault = 0.01, Multiple = FALSE)
    })
    output$LegendPosition1 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'LegendPosition1', Label = tags$span(style='color: blue;', 'Legend Position'), Choices = c('bottom','left','right','top','none'), SelectedDefault = 'bottom', Multiple = FALSE)
    })
    output$LegendPosition2 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'LegendPosition2', Label = tags$span(style='color: blue;', 'Legend Position'), Choices = c('bottom','left','right','top','none'), SelectedDefault = 'bottom', Multiple = FALSE)
    })
    output$LegendPosition3 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'LegendPosition3', Label = tags$span(style='color: blue;', 'Legend Position'), Choices = c('bottom','left','right','top','none'), SelectedDefault = 'bottom', Multiple = FALSE)
    })
    output$LegendPosition4 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'LegendPosition4', Label = tags$span(style='color: blue;', 'Legend Position'), Choices = c('bottom','left','right','top','none'), SelectedDefault = 'bottom', Multiple = FALSE)
    })
    output$LegendBorderSize1 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'LegendBorderSize1', Label = tags$span(style='color: blue;', 'Legend Border Size'), Choices = c(as.character(seq(0.01,2,0.01))), SelectedDefault = as.character(0.01), Multiple = FALSE)
    })
    output$LegendBorderSize2 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'LegendBorderSize2', Label = tags$span(style='color: blue;', 'Legend Border Size'), Choices = c(as.character(seq(0.01,2,0.01))), SelectedDefault = as.character(0.01), Multiple = FALSE)
    })
    output$LegendBorderSize3 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'LegendBorderSize3', Label = tags$span(style='color: blue;', 'Legend Border Size'), Choices = c(as.character(seq(0.01,2,0.01))), SelectedDefault = as.character(0.01), Multiple = FALSE)
    })
    output$LegendBorderSize4 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'LegendBorderSize4', Label = tags$span(style='color: blue;', 'Legend Border Size'), Choices = c(as.character(seq(0.01,2,0.01))), SelectedDefault = as.character(0.01), Multiple = FALSE)
    })
    output$LegendLineType1 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'LegendLineType1', Label = tags$span(style='color: blue;', 'Legend Border Type'), Choices = c('solid','blank','dashed','dotdash','dotted','longlash','twodash'), SelectedDefault = 'solid', Multiple = FALSE)
    })
    output$LegendLineType2 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'LegendLineType2', Label = tags$span(style='color: blue;', 'Legend Border Type'), Choices = c('solid','blank','dashed','dotdash','dotted','longlash','twodash'), SelectedDefault = 'solid', Multiple = FALSE)
    })
    output$LegendLineType3 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'LegendLineType3', Label = tags$span(style='color: blue;', 'Legend Border Type'), Choices = c('solid','blank','dashed','dotdash','dotted','longlash','twodash'), SelectedDefault = 'solid', Multiple = FALSE)
    })
    output$LegendLineType4 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'LegendLineType4', Label = tags$span(style='color: blue;', 'Legend Border Type'), Choices = c('solid','blank','dashed','dotdash','dotted','longlash','twodash'), SelectedDefault = 'solid', Multiple = FALSE)
    })

    # Color boxes
    output$TextColor1 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'TextColor1', Label = tags$span(style='color: blue;', 'Text Color'), Choices = grDevices::colors(), SelectedDefault = 'darkblue', Multiple = FALSE)
    })
    output$TextColor2 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'TextColor2', Label = tags$span(style='color: blue;', 'Text Color'), Choices = grDevices::colors(), SelectedDefault = 'darkblue', Multiple = FALSE)
    })
    output$TextColor3 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'TextColor3', Label = tags$span(style='color: blue;', 'Text Color'), Choices = grDevices::colors(), SelectedDefault = 'darkblue', Multiple = FALSE)
    })
    output$TextColor4 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'TextColor4', Label = tags$span(style='color: blue;', 'Text Color'), Choices = grDevices::colors(), SelectedDefault = 'darkblue', Multiple = FALSE)
    })
    output$ChartColor1 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'ChartColor1', Label = tags$span(style='color: blue;', 'Chart Color'), Choices = grDevices::colors(), SelectedDefault = 'aliceblue', Multiple = FALSE)
    })
    output$ChartColor2 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'ChartColor2', Label = tags$span(style='color: blue;', 'Chart Color'), Choices = grDevices::colors(), SelectedDefault = 'aliceblue', Multiple = FALSE)
    })
    output$ChartColor3 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'ChartColor3', Label = tags$span(style='color: blue;', 'Chart Color'), Choices = grDevices::colors(), SelectedDefault = 'aliceblue', Multiple = FALSE)
    })
    output$ChartColor4 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'ChartColor4', Label = tags$span(style='color: blue;', 'Chart Color'), Choices = grDevices::colors(), SelectedDefault = 'aliceblue', Multiple = FALSE)
    })
    output$GridColor1 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'GridColor1', Label = tags$span(style='color: blue;', 'Grid Lines Color'), Choices = grDevices::colors(), SelectedDefault = 'lightsteelblue1', Multiple = FALSE)
    })
    output$GridColor2 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'GridColor2', Label = tags$span(style='color: blue;', 'Grid Lines Color'), Choices = grDevices::colors(), SelectedDefault = 'lightsteelblue1', Multiple = FALSE)
    })
    output$GridColor3 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'GridColor3', Label = tags$span(style='color: blue;', 'Grid Lines Color'), Choices = grDevices::colors(), SelectedDefault = 'lightsteelblue1', Multiple = FALSE)
    })
    output$GridColor4 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'GridColor4', Label = tags$span(style='color: blue;', 'Grid Lines Color'), Choices = grDevices::colors(), SelectedDefault = 'lightsteelblue1', Multiple = FALSE)
    })
    output$BackGroundColor1 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'BackGroundColor1', Label = tags$span(style='color: blue;', 'Background Color'), Choices = grDevices::colors(), SelectedDefault = 'gray95', Multiple = FALSE)
    })
    output$BackGroundColor2 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'BackGroundColor2', Label = tags$span(style='color: blue;', 'Background Color'), Choices = grDevices::colors(), SelectedDefault = 'gray95', Multiple = FALSE)
    })
    output$BackGroundColor3 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'BackGroundColor3', Label = tags$span(style='color: blue;', 'Background Color'), Choices = grDevices::colors(), SelectedDefault = 'gray95', Multiple = FALSE)
    })
    output$BackGroundColor4 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'BackGroundColor4', Label = tags$span(style='color: blue;', 'Background Color'), Choices = grDevices::colors(), SelectedDefault = 'gray95', Multiple = FALSE)
    })
    output$BorderColor1 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'BorderColor1', Label = tags$span(style='color: blue;', 'Border Color'), Choices = grDevices::colors(), SelectedDefault = 'lightsteelblue4', Multiple = FALSE)
    })
    output$BorderColor2 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'BorderColor2', Label = tags$span(style='color: blue;', 'Border Color'), Choices = grDevices::colors(), SelectedDefault = 'lightsteelblue4', Multiple = FALSE)
    })
    output$BorderColor3 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'BorderColor3', Label = tags$span(style='color: blue;', 'Border Color'), Choices = grDevices::colors(), SelectedDefault = 'lightsteelblue4', Multiple = FALSE)
    })
    output$BorderColor4 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'BorderColor4', Label = tags$span(style='color: blue;', 'Border Color'), Choices = grDevices::colors(), SelectedDefault = 'lightsteelblue4', Multiple = FALSE)
    })
    output$OutlierColor1 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'OutlierColor1', Label = tags$span(style='color: blue;', 'Outlier Color'), Choices = grDevices::colors(), SelectedDefault = 'blue', Multiple = FALSE)
    })
    output$OutlierColor2 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'OutlierColor2', Label = tags$span(style='color: blue;', 'Outlier Color'), Choices = grDevices::colors(), SelectedDefault = 'blue', Multiple = FALSE)
    })
    output$OutlierColor3 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'OutlierColor3', Label = tags$span(style='color: blue;', 'Outlier Color'), Choices = grDevices::colors(), SelectedDefault = 'blue', Multiple = FALSE)
    })
    output$OutlierColor4 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'OutlierColor4', Label = tags$span(style='color: blue;', 'Outlier Color'), Choices = grDevices::colors(), SelectedDefault = 'blue', Multiple = FALSE)
    })
    output$FillColor1 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'FillColor1', Label = tags$span(style='color: blue;', 'BoxPlot Fill Color'), Choices = grDevices::colors(), SelectedDefault = 'gray70', Multiple = FALSE)
    })
    output$FillColor2 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'FillColor2', Label = tags$span(style='color: blue;', 'BoxPlot Fill Color'), Choices = grDevices::colors(), SelectedDefault = 'gray70', Multiple = FALSE)
    })
    output$FillColor3 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'FillColor3', Label = tags$span(style='color: blue;', 'BoxPlot Fill Color'), Choices = grDevices::colors(), SelectedDefault = 'gray70', Multiple = FALSE)
    })
    output$FillColor4 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'FillColor4', Label = tags$span(style='color: blue;', 'BoxPlot Fill Color'), Choices = grDevices::colors(), SelectedDefault = 'gray70', Multiple = FALSE)
    })
    output$SubTitleColor1 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'SubTitleColor1', Label = tags$span(style='color: blue;', 'Subtitle Color'), Choices = grDevices::colors(), SelectedDefault = 'blue', Multiple = FALSE)
    })
    output$SubTitleColor2 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'SubTitleColor2', Label = tags$span(style='color: blue;', 'Subtitle Color'), Choices = grDevices::colors(), SelectedDefault = 'blue', Multiple = FALSE)
    })
    output$SubTitleColor3 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'SubTitleColor3', Label = tags$span(style='color: blue;', 'Subtitle Color'), Choices = grDevices::colors(), SelectedDefault = 'blue', Multiple = FALSE)
    })
    output$SubTitleColor4 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID = 'SubTitleColor4', Label = tags$span(style='color: blue;', 'Subtitle Color'), Choices = grDevices::colors(), SelectedDefault = 'blue', Multiple = FALSE)
    })

    # ----

    # ----

    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    # Group Variables                      ----
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

    # Select GroupVars
    output$GroupVars1 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID='GroupVars1', Label=tags$span(style='color: blue;', 'Select Group Variables'), Choices=c(names(data)), SelectedDefault=NULL, Multiple=TRUE, MaxVars = 3, CloseAfterSelect = FALSE)
    })
    output$GroupVars2 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID='GroupVars2', Label=tags$span(style='color: blue;', 'Select Group Variables'), Choices=c(names(data)), SelectedDefault=NULL, Multiple=TRUE, MaxVars = 3, CloseAfterSelect = FALSE)
    })
    output$GroupVars3 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID='GroupVars3', Label=tags$span(style='color: blue;', 'Select Group Variables'), Choices=c(names(data)), SelectedDefault=NULL, Multiple=TRUE, MaxVars = 3, CloseAfterSelect = FALSE)
    })
    output$GroupVars4 <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID='GroupVars4', Label=tags$span(style='color: blue;', 'Select Group Variables'), Choices=c(names(data)), SelectedDefault=NULL, Multiple=TRUE, MaxVars = 3, CloseAfterSelect = FALSE)
    })

    # Reactive Group Variables
    SelectedGroups1 <- shiny::reactive({
      RemixAutoML::ReturnParam(xx = input[['GroupVars1']], VarName = 'GroupVars1', Default = NULL, Switch = TRUE, Type = 'character')
    })
    SelectedGroups2 <- shiny::reactive({
      RemixAutoML::ReturnParam(xx = input[['GroupVars2']], VarName = 'GroupVars2', Default = NULL, Switch = TRUE, Type = 'character')
    })
    SelectedGroups3 <- shiny::reactive({
      RemixAutoML::ReturnParam(xx = input[['GroupVars3']], VarName = 'GroupVars3', Default = NULL, Switch = TRUE, Type = 'character')
    })
    SelectedGroups4 <- shiny::reactive({
      RemixAutoML::ReturnParam(xx = input[['GroupVars4']], VarName = 'GroupVars4', Default = NULL, Switch = TRUE, Type = 'character')
    })

    # Group Levels
    output$Levels_1_1 <- shiny::renderUI({
      sgs <- RemixAutoML:::LevelValues(SelectedGroups1()); if(Debug) print('PickerInput_GetLevels 1')
      RemixAutoML::PickerInput_GetLevels2(DataExist=exists('data'), InputID='Levels_1_1', InputID2=sgs, Choices=RemixAutoML::UniqueLevels(input, data, 1L, GroupVars=sgs), NumGroupVar=1L, Multiple=TRUE, SelectedDefault=NULL)
    })
    output$Levels_1_2 <- shiny::renderUI({
      sgs <- RemixAutoML:::LevelValues(SelectedGroups1()); if(Debug) print('PickerInput_GetLevels 2')
      RemixAutoML::PickerInput_GetLevels2(DataExist=exists('data'), InputID='Levels_1_2', InputID2=sgs, Choices=RemixAutoML::UniqueLevels(input, data, 2L, GroupVars=sgs), NumGroupVar=2L, Multiple=TRUE, SelectedDefault=NULL)
    })
    output$Levels_1_3 <- shiny::renderUI({
      sgs <- RemixAutoML:::LevelValues(SelectedGroups1()); if(Debug) print('PickerInput_GetLevels 3')
      RemixAutoML::PickerInput_GetLevels2(DataExist=exists('data'), InputID='Levels_1_3', InputID2=sgs, Choices=RemixAutoML::UniqueLevels(input, data, 3L, GroupVars=sgs), NumGroupVar=3L, Multiple=TRUE, SelectedDefault=NULL)
    })
    output$Levels_2_1 <- shiny::renderUI({
      sgs <- RemixAutoML:::LevelValues(SelectedGroups2()); if(Debug) print('PickerInput_GetLevels 1')
      RemixAutoML::PickerInput_GetLevels2(DataExist=exists('data'), InputID='Levels_2_1', InputID2=sgs, Choices=RemixAutoML::UniqueLevels(input, data, 1L, GroupVars=sgs), NumGroupVar=1L, Multiple=TRUE, SelectedDefault=NULL)
    })
    output$Levels_2_2 <- shiny::renderUI({
      sgs <- RemixAutoML:::LevelValues(SelectedGroups2()); if(Debug) print('PickerInput_GetLevels 2')
      RemixAutoML::PickerInput_GetLevels2(DataExist=exists('data'), InputID='Levels_2_2', InputID2=sgs, Choices=RemixAutoML::UniqueLevels(input, data, 2L, GroupVars=sgs), NumGroupVar=2L, Multiple=TRUE, SelectedDefault=NULL)
    })
    output$Levels_2_3 <- shiny::renderUI({
      sgs <- RemixAutoML:::LevelValues(SelectedGroups2()); if(Debug) print('PickerInput_GetLevels 3')
      RemixAutoML::PickerInput_GetLevels2(DataExist=exists('data'), InputID='Levels_2_3', InputID2=sgs, Choices=RemixAutoML::UniqueLevels(input, data, 3L, GroupVars=sgs), NumGroupVar=3L, Multiple=TRUE, SelectedDefault=NULL)
    })
    output$Levels_3_1 <- shiny::renderUI({
      sgs <- RemixAutoML:::LevelValues(SelectedGroups3()); if(Debug) print('PickerInput_GetLevels 1')
      RemixAutoML::PickerInput_GetLevels2(DataExist=exists('data'), InputID='Levels_3_1', InputID2=sgs, Choices=RemixAutoML::UniqueLevels(input, data, 1L, GroupVars=sgs), NumGroupVar=1L, Multiple=TRUE, SelectedDefault=NULL)
    })
    output$Levels_3_2 <- shiny::renderUI({
      sgs <- RemixAutoML:::LevelValues(SelectedGroups3()); if(Debug) print('PickerInput_GetLevels 2')
      RemixAutoML::PickerInput_GetLevels2(DataExist=exists('data'), InputID='Levels_3_2', InputID2=sgs, Choices=RemixAutoML::UniqueLevels(input, data, 2L, GroupVars=sgs), NumGroupVar=2L, Multiple=TRUE, SelectedDefault=NULL)
    })
    output$Levels_3_3 <- shiny::renderUI({
      sgs <- RemixAutoML:::LevelValues(SelectedGroups3()); if(Debug) print('PickerInput_GetLevels 3')
      RemixAutoML::PickerInput_GetLevels2(DataExist=exists('data'), InputID='Levels_3_3', InputID2=sgs, Choices=RemixAutoML::UniqueLevels(input, data, 3L, GroupVars=sgs), NumGroupVar=3L, Multiple=TRUE, SelectedDefault=NULL)
    })
    output$Levels_4_1 <- shiny::renderUI({
      sgs <- RemixAutoML:::LevelValues(SelectedGroups4()); if(Debug) print('PickerInput_GetLevels 1')
      RemixAutoML::PickerInput_GetLevels2(DataExist=exists('data'), InputID='Levels_4_1', InputID2=sgs, Choices=RemixAutoML::UniqueLevels(input, data, 1L, GroupVars=sgs), NumGroupVar=1L, Multiple=TRUE, SelectedDefault=NULL)
    })
    output$Levels_4_2 <- shiny::renderUI({
      sgs <- RemixAutoML:::LevelValues(SelectedGroups4()); if(Debug) print('PickerInput_GetLevels 2')
      RemixAutoML::PickerInput_GetLevels2(DataExist=exists('data'), InputID='Levels_4_2', InputID2=sgs, Choices=RemixAutoML::UniqueLevels(input, data, 2L, GroupVars=sgs), NumGroupVar=2L, Multiple=TRUE, SelectedDefault=NULL)
    })
    output$Levels_4_3 <- shiny::renderUI({
      sgs <- RemixAutoML:::LevelValues(SelectedGroups4()); if(Debug) print('PickerInput_GetLevels 3')
      RemixAutoML::PickerInput_GetLevels2(DataExist=exists('data'), InputID='Levels_4_3', InputID2=sgs, Choices=RemixAutoML::UniqueLevels(input, data, 3L, GroupVars=sgs), NumGroupVar=3L, Multiple=TRUE, SelectedDefault=NULL)
    })

    # Faceting
    output$FacetVar_1_1 <- shiny::renderUI({
      if(any(RemixAutoML:::AvailableAppInsightsPlots(x = NULL, PlotNamesLookup = PlotNamesLookup, Debug = Debug) %in% tryCatch({Plot1_react()}, error = function(x) NULL))) {
        ModelVars <- names(data)
      } else {
        ModelVars <- names(ModelData)
      }
      RemixAutoML::SelectizeInput(InputID='FacetVar_1_1', Label = tags$span(style='color: blue;', 'Facet Variable 1'), Choices = c(ModelVars), Multiple = TRUE, MaxVars = 1)
    })
    output$FacetVar_1_2 <- shiny::renderUI({
      if(any(RemixAutoML:::AvailableAppInsightsPlots(x = NULL, PlotNamesLookup = PlotNamesLookup, Debug = Debug) %in% tryCatch({Plot1_react()}, error = function(x) NULL))) {
        ModelVars <- names(data)
      } else {
        ModelVars <- names(ModelData)
      }
      RemixAutoML::SelectizeInput(InputID='FacetVar_1_2', Label = tags$span(style='color: blue;', 'Facet Variable 2'), Choices = c(ModelVars), Multiple = TRUE, MaxVars = 1)
    })
    output$FacetVar_2_1 <- shiny::renderUI({
      if(any(RemixAutoML:::AvailableAppInsightsPlots(x = NULL, PlotNamesLookup = PlotNamesLookup, Debug = Debug) %in% tryCatch({Plot1_react()}, error = function(x) NULL))) {
        ModelVars <- names(data)
      } else {
        ModelVars <- names(ModelData)
      }
      RemixAutoML::SelectizeInput(InputID='FacetVar_2_1', Label = tags$span(style='color: blue;', 'Facet Variable 1'), Choices = c(ModelVars), Multiple = TRUE, MaxVars = 1)
    })
    output$FacetVar_2_2 <- shiny::renderUI({
      if(any(RemixAutoML:::AvailableAppInsightsPlots(x = NULL, PlotNamesLookup = PlotNamesLookup, Debug = Debug) %in% tryCatch({Plot1_react()}, error = function(x) NULL))) {
        ModelVars <- names(data)
      } else {
        ModelVars <- names(ModelData)
      }
      RemixAutoML::SelectizeInput(InputID='FacetVar_2_2', Label = tags$span(style='color: blue;', 'Facet Variable 2'), Choices = c(ModelVars), Multiple = TRUE, MaxVars = 1)
    })
    output$FacetVar_3_1 <- shiny::renderUI({
      if(any(RemixAutoML:::AvailableAppInsightsPlots(x = NULL, PlotNamesLookup = PlotNamesLookup, Debug = Debug) %in% tryCatch({Plot1_react()}, error = function(x) NULL))) {
        ModelVars <- names(data)
      } else {
        ModelVars <- names(ModelData)
      }
      RemixAutoML::SelectizeInput(InputID='FacetVar_3_1', Label = tags$span(style='color: blue;', 'Facet Variable 1'), Choices = c(ModelVars), Multiple = TRUE, MaxVars = 1)
    })
    output$FacetVar_3_2 <- shiny::renderUI({
      if(any(RemixAutoML:::AvailableAppInsightsPlots(x = NULL, PlotNamesLookup = PlotNamesLookup, Debug = Debug) %in% tryCatch({Plot1_react()}, error = function(x) NULL))) {
        ModelVars <- names(data)
      } else {
        ModelVars <- names(ModelData)
      }
      RemixAutoML::SelectizeInput(InputID='FacetVar_3_2', Label = tags$span(style='color: blue;', 'Facet Variable 2'), Choices = c(ModelVars), Multiple = TRUE, MaxVars = 1)
    })
    output$FacetVar_4_1 <- shiny::renderUI({
      if(any(RemixAutoML:::AvailableAppInsightsPlots(x = NULL, PlotNamesLookup = PlotNamesLookup, Debug = Debug) %in% tryCatch({Plot1_react()}, error = function(x) NULL))) {
        ModelVars <- names(data)
      } else {
        ModelVars <- names(ModelData)
      }
      RemixAutoML::SelectizeInput(InputID='FacetVar_4_1', Label = tags$span(style='color: blue;', 'Facet Variable 1'), Choices = c(ModelVars), Multiple = TRUE, MaxVars = 1)
    })
    output$FacetVar_4_2 <- shiny::renderUI({
      if(any(RemixAutoML:::AvailableAppInsightsPlots(x = NULL, PlotNamesLookup = PlotNamesLookup, Debug = Debug) %in% tryCatch({Plot1_react()}, error = function(x) NULL))) {
        ModelVars <- names(data)
      } else {
        ModelVars <- names(ModelData)
      }
      RemixAutoML::SelectizeInput(InputID='FacetVar_4_2', Label = tags$span(style='color: blue;', 'Facet Variable 2'), Choices = c(ModelVars), Multiple = TRUE, MaxVars = 1)
    })

    # Sizing
    output$SizeVar1 <- shiny::renderUI({
      if(any(RemixAutoML:::AvailableAppInsightsPlots(x = NULL, PlotNamesLookup = PlotNamesLookup, Debug = Debug) %in% tryCatch({Plot1_react()}, error = function(x) NULL))) {
        ModelVars <- names(data)
      } else {
        ModelVars <- names(ModelData)
      }
      RemixAutoML::SelectizeInput(InputID = 'SizeVar1', Label = tags$span(style='color: blue;', 'Size Variable'), Choices = c(ModelVars), Multiple = TRUE, MaxVars = 1)
    })
    output$SizeVar2 <- shiny::renderUI({
      if(any(RemixAutoML:::AvailableAppInsightsPlots(x = NULL, PlotNamesLookup = PlotNamesLookup, Debug = Debug) %in% tryCatch({Plot1_react()}, error = function(x) NULL))) {
        ModelVars <- names(data)
      } else {
        ModelVars <- names(ModelData)
      }
      RemixAutoML::SelectizeInput(InputID = 'SizeVar2', Label = tags$span(style='color: blue;', 'Size Variable'), Choices = c(ModelVars), Multiple = TRUE, MaxVars = 1)
    })
    output$SizeVar3 <- shiny::renderUI({
      if(any(RemixAutoML:::AvailableAppInsightsPlots(x = NULL, PlotNamesLookup = PlotNamesLookup, Debug = Debug) %in% tryCatch({Plot1_react()}, error = function(x) NULL))) {
        ModelVars <- names(data)
      } else {
        ModelVars <- names(ModelData)
      }
      RemixAutoML::SelectizeInput(InputID = 'SizeVar3', Label = tags$span(style='color: blue;', 'Size Variable'), Choices = c(ModelVars), Multiple = TRUE, MaxVars = 1)
    })
    output$SizeVar4 <- shiny::renderUI({
      if(any(RemixAutoML:::AvailableAppInsightsPlots(x = NULL, PlotNamesLookup = PlotNamesLookup, Debug = Debug) %in% tryCatch({Plot1_react()}, error = function(x) NULL))) {
        ModelVars <- names(data)
      } else {
        ModelVars <- names(ModelData)
      }
      RemixAutoML::SelectizeInput(InputID = 'SizeVar4', Label = tags$span(style='color: blue;', 'Size Variable'), Choices = c(ModelVars), Multiple = TRUE, MaxVars = 1)
    })

    # ----

    # ----

    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    # Filter Variables                     ----
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

    # Filter Variables
    output$FilterVariable_1_1 <- shiny::renderUI({
      if(any(RemixAutoML:::AvailableAppInsightsPlots(x = NULL, PlotNamesLookup = PlotNamesLookup, Debug = Debug) %in% tryCatch({Plot1_react()}, error = function(x) NULL))) {
        ModelVars <- names(data)
      } else {
        ModelVars <- names(ModelData)
      }
      RemixAutoML::SelectizeInput(InputID = 'FilterVariable_1_1', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Filter Variable 1'), Choices = c(ModelVars), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
    })
    output$FilterVariable_1_2 <- shiny::renderUI({
      if(any(RemixAutoML:::AvailableAppInsightsPlots(x = NULL, PlotNamesLookup = PlotNamesLookup, Debug = Debug) %in% tryCatch({Plot1_react()}, error = function(x) NULL))) {
        ModelVars <- names(data)
      } else {
        ModelVars <- names(ModelData)
      }
      RemixAutoML::SelectizeInput(InputID = 'FilterVariable_1_2', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Filter Variable 2'), Choices = c(ModelVars), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
    })
    output$FilterVariable_1_3 <- shiny::renderUI({
      if(any(RemixAutoML:::AvailableAppInsightsPlots(x = NULL, PlotNamesLookup = PlotNamesLookup, Debug = Debug) %in% tryCatch({Plot1_react()}, error = function(x) NULL))) {
        ModelVars <- names(data)
      } else {
        ModelVars <- names(ModelData)
      }
      RemixAutoML::SelectizeInput(InputID = 'FilterVariable_1_3', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Filter Variable 3'), Choices = c(ModelVars), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
    })
    output$FilterVariable_1_4 <- shiny::renderUI({
      if(any(RemixAutoML:::AvailableAppInsightsPlots(x = NULL, PlotNamesLookup = PlotNamesLookup, Debug = Debug) %in% tryCatch({Plot1_react()}, error = function(x) NULL))) {
        ModelVars <- names(data)
      } else {
        ModelVars <- names(ModelData)
      }
      RemixAutoML::SelectizeInput(InputID = 'FilterVariable_1_4', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Filter Variable 4'), Choices = c(ModelVars), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
    })
    output$FilterVariable_2_1 <- shiny::renderUI({
      if(any(RemixAutoML:::AvailableAppInsightsPlots(x = NULL, PlotNamesLookup = PlotNamesLookup, Debug = Debug) %in% tryCatch({Plot2_react()}, error = function(x) NULL))) {
        ModelVars <- names(data)
      } else {
        ModelVars <- names(ModelData)
      }
      RemixAutoML::SelectizeInput(InputID = 'FilterVariable_2_1', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Filter Variable 1'), Choices = c(ModelVars), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
    })
    output$FilterVariable_2_2 <- shiny::renderUI({
      if(any(RemixAutoML:::AvailableAppInsightsPlots(x = NULL, PlotNamesLookup = PlotNamesLookup, Debug = Debug) %in% tryCatch({Plot2_react()}, error = function(x) NULL))) {
        ModelVars <- names(data)
      } else {
        ModelVars <- names(ModelData)
      }
      RemixAutoML::SelectizeInput(InputID = 'FilterVariable_2_2', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Filter Variable 2'), Choices = c(ModelVars), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
    })
    output$FilterVariable_2_3 <- shiny::renderUI({
      if(any(RemixAutoML:::AvailableAppInsightsPlots(x = NULL, PlotNamesLookup = PlotNamesLookup, Debug = Debug) %in% tryCatch({Plot2_react()}, error = function(x) NULL))) {
        ModelVars <- names(data)
      } else {
        ModelVars <- names(ModelData)
      }
      RemixAutoML::SelectizeInput(InputID = 'FilterVariable_2_3', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Filter Variable 3'), Choices = c(ModelVars), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
    })
    output$FilterVariable_2_4 <- shiny::renderUI({
      if(any(RemixAutoML:::AvailableAppInsightsPlots(x = NULL, PlotNamesLookup = PlotNamesLookup, Debug = Debug) %in% tryCatch({Plot2_react()}, error = function(x) NULL))) {
        ModelVars <- names(data)
      } else {
        ModelVars <- names(ModelData)
      }
      RemixAutoML::SelectizeInput(InputID = 'FilterVariable_2_4', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Filter Variable 4'), Choices = c(ModelVars), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
    })
    output$FilterVariable_3_1 <- shiny::renderUI({
      if(any(RemixAutoML:::AvailableAppInsightsPlots(x = NULL, PlotNamesLookup = PlotNamesLookup, Debug = Debug) %in% tryCatch({Plot3_react()}, error = function(x) NULL))) {
        ModelVars <- names(data)
      } else {
        ModelVars <- names(ModelData)
      }
      RemixAutoML::SelectizeInput(InputID = 'FilterVariable_3_1', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Filter Variable 1'), Choices = c(ModelVars), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
    })
    output$FilterVariable_3_2 <- shiny::renderUI({
      if(any(RemixAutoML:::AvailableAppInsightsPlots(x = NULL, PlotNamesLookup = PlotNamesLookup, Debug = Debug) %in% tryCatch({Plot3_react()}, error = function(x) NULL))) {
        ModelVars <- names(data)
      } else {
        ModelVars <- names(ModelData)
      }
      RemixAutoML::SelectizeInput(InputID = 'FilterVariable_3_2', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Filter Variable 2'), Choices = c(ModelVars), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
    })
    output$FilterVariable_3_3 <- shiny::renderUI({
      if(any(RemixAutoML:::AvailableAppInsightsPlots(x = NULL, PlotNamesLookup = PlotNamesLookup, Debug = Debug) %in% tryCatch({Plot3_react()}, error = function(x) NULL))) {
        ModelVars <- names(data)
      } else {
        ModelVars <- names(ModelData)
      }
      RemixAutoML::SelectizeInput(InputID = 'FilterVariable_3_3', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Filter Variable 3'), Choices = c(ModelVars), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
    })
    output$FilterVariable_3_4 <- shiny::renderUI({
      if(any(RemixAutoML:::AvailableAppInsightsPlots(x = NULL, PlotNamesLookup = PlotNamesLookup, Debug = Debug) %in% tryCatch({Plot3_react()}, error = function(x) NULL))) {
        ModelVars <- names(data)
      } else {
        ModelVars <- names(ModelData)
      }
      RemixAutoML::SelectizeInput(InputID = 'FilterVariable_3_4', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Filter Variable 4'), Choices = c(ModelVars), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
    })
    output$FilterVariable_4_1 <- shiny::renderUI({
      if(any(RemixAutoML:::AvailableAppInsightsPlots(x = NULL, PlotNamesLookup = PlotNamesLookup, Debug = Debug) %in% tryCatch({Plot4_react()}, error = function(x) NULL))) {
        ModelVars <- names(data)
      } else {
        ModelVars <- names(ModelData)
      }
      RemixAutoML::SelectizeInput(InputID = 'FilterVariable_4_1', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Filter Variable 1'), Choices = c(ModelVars), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
    })
    output$FilterVariable_4_2 <- shiny::renderUI({
      if(any(RemixAutoML:::AvailableAppInsightsPlots(x = NULL, PlotNamesLookup = PlotNamesLookup, Debug = Debug) %in% tryCatch({Plot4_react()}, error = function(x) NULL))) {
        ModelVars <- names(data)
      } else {
        ModelVars <- names(ModelData)
      }
      RemixAutoML::SelectizeInput(InputID = 'FilterVariable_4_2', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Filter Variable 2'), Choices = c(ModelVars), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
    })
    output$FilterVariable_4_3 <- shiny::renderUI({
      if(any(RemixAutoML:::AvailableAppInsightsPlots(x = NULL, PlotNamesLookup = PlotNamesLookup, Debug = Debug) %in% tryCatch({Plot4_react()}, error = function(x) NULL))) {
        ModelVars <- names(data)
      } else {
        ModelVars <- names(ModelData)
      }
      RemixAutoML::SelectizeInput(InputID = 'FilterVariable_4_3', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Filter Variable 3'), Choices = c(ModelVars), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
    })
    output$FilterVariable_4_4 <- shiny::renderUI({
      if(any(RemixAutoML:::AvailableAppInsightsPlots(x = NULL, PlotNamesLookup = PlotNamesLookup, Debug = Debug) %in% tryCatch({Plot4_react()}, error = function(x) NULL))) {
        ModelVars <- names(data)
      } else {
        ModelVars <- names(ModelData)
      }
      RemixAutoML::SelectizeInput(InputID = 'FilterVariable_4_4', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Filter Variable 4'), Choices = c(ModelVars), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
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

    # Filter Logics
    output$FilterLogic_1_1 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_1_1', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_1_1()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })
    output$FilterLogic_1_2 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_1_2', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_1_2()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })
    output$FilterLogic_1_3 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_1_3', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_1_3()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })
    output$FilterLogic_1_4 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_1_4', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_1_4()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })
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

    # ----

    # ----

    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    # Filter Values     DONT OVERWRITE     ----
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

    # Plot 1
    output$FilterValue_1_1_1 <- shiny::renderUI({
      params <- list(data, VarName=tryCatch({input[['FilterVariable_1_1']]}, error = function(x) NULL), type=1)
      choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_1_1']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_1_1']])
      if(length(choices) == 0) {
        if(length(ModelData) != 0) {
          vname <- tryCatch({stringr::str_remove(string = input[['FilterVariable_1_1']], pattern = 'ModelVar-')}, error = function(x) 'None')
          params <- list(data=ModelData, VarName=vname, type=1)
          choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
          Mult <- RemixAutoML:::GetFilterValueMultiple(ModelData, VarName=vname)
        }
      }
      RemixAutoML::SelectizeInput(Multiple=Mult, InputID='FilterValue_1_1_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })
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
      RemixAutoML::SelectizeInput(Multiple=Mult, InputID='FilterValue_1_1_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })
    output$FilterValue_1_2_1 <- shiny::renderUI({
      params <- list(data=data, VarName=tryCatch({input[['FilterVariable_1_2']]}, error = function(x) NULL), type=1)
      choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = input[['FilterVariable_1_2']])$ChoiceInput}, error = function(x) NULL)
      Mult <- RemixAutoML:::GetFilterValueMultiple(data, VarName=input[['FilterVariable_1_2']])
      if(length(choices) == 0) {
        if(length(ModelData) != 0) {
          vname <- tryCatch({stringr::str_remove(string = input[['FilterVariable_1_2']], pattern = 'ModelVar-')}, error = function(x) 'None')
          params <- list(data=ModelData, VarName=vname, type=1)
          choices <- tryCatch({RemixAutoML::KeyVarsInit(ModelData, VarName = vname)$ChoiceInput}, error = function(x) NULL)
          Mult <- RemixAutoML:::GetFilterValueMultiple(ModelData, VarName=vname)
        }
      }
      RemixAutoML::SelectizeInput(Multiple=Mult, InputID='FilterValue_1_2_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })
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
      RemixAutoML::SelectizeInput(Multiple=Mult, InputID='FilterValue_1_2_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })
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
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_1_3_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })
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
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_1_3_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })
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
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_1_4_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })
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
      RemixAutoML::SelectizeInput(Multiple=Mult, InputID='FilterValue_1_4_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })

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
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_2_1_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })
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
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_2_1_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })
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
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_2_2_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })
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
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_2_2_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })
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
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_2_3_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })
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
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_2_3_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })
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
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_2_4_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })
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
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_2_4_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })

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
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_3_1_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })
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
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_3_1_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })
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
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_3_2_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })
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
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_3_2_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })
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
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_3_3_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })
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
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_3_3_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })
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
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_3_4_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })
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
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_3_4_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })

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
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_4_1_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })
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
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_4_1_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })
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
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_4_2_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })
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
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_4_2_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })
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
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_4_3_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })
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
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_4_3_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })
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
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_4_4_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })
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
      RemixAutoML::SelectizeInput(Multiple = Mult, InputID='FilterValue_4_4_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })

    # ----

    # ----

    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    # Sweet Alert                          ----
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    if(Debug) print("Here gggggggg")
    shinyWidgets::sendSweetAlert(session, title = NULL, text = 'Success', type = NULL, btn_labels = "success", btn_colors = "green", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")

    # ----

    # ----

  })

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Variables                            ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  # Initialize
  if(!exists('ModelData')) ModelData <- NULL
  if(!exists('ModelOutputList')) ModelOutputList <- NULL

  # YVars
  output$YVar1 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'YVar1', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Y-Variable'), Choices = c(names(data)), Multiple = FALSE, SelectedDefault = NULL, CloseAfterSelect = TRUE)
  })
  output$YVar2 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'YVar2', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Y-Variable'), Choices = c(names(data)), Multiple = FALSE, SelectedDefault = NULL, CloseAfterSelect = TRUE)
  })
  output$YVar3 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'YVar3', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Y-Variable'), Choices = c(names(data)), Multiple = FALSE, SelectedDefault = NULL, CloseAfterSelect = TRUE)
  })
  output$YVar4 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'YVar4', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Y-Variable'), Choices = c(names(data)), Multiple = FALSE, SelectedDefault = NULL, CloseAfterSelect = TRUE)
  })

  # 'X-Variables'
  output$XVar1 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'XVar1', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'X-Variable'), Choices = c(names(data)), Multiple = FALSE, SelectedDefault = NULL, CloseAfterSelect = TRUE)
  })
  output$XVar2 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'XVar2', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'X-Variable'), Choices = c(names(data)), Multiple = FALSE, SelectedDefault = NULL, CloseAfterSelect = TRUE)
  })
  output$XVar3 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'XVar3', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'X-Variable'), Choices = c(names(data)), Multiple = FALSE, SelectedDefault = NULL, CloseAfterSelect = TRUE)
  })
  output$XVar4 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'XVar4', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'X-Variable'), Choices = c(names(data)), Multiple = FALSE, SelectedDefault = NULL, CloseAfterSelect = TRUE)
  })

  # Reactives References
  YVar1 <- shiny::reactive({shiny::req(input[['YVar1']])})
  YVar2 <- shiny::reactive({shiny::req(input[['YVar2']])})
  YVar3 <- shiny::reactive({shiny::req(input[['YVar3']])})
  YVar4 <- shiny::reactive({shiny::req(input[['YVar4']])})
  XVar1 <- shiny::reactive({shiny::req(input[['XVar1']])})
  XVar2 <- shiny::reactive({shiny::req(input[['XVar2']])})
  XVar3 <- shiny::reactive({shiny::req(input[['XVar3']])})
  XVar4 <- shiny::reactive({shiny::req(input[['XVar4']])})

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Plotting MetaData                    ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  # Auto SCaling of Plot Grid: doubles the size in the event of more than 1 plot
  output$AutoGridHorizontal <-  shiny::renderUI({
    shinyWidgets::materialSwitch(inputId = "AutoGridHorizontal", label = tags$span(style='color: blue;', 'Auto Grid Scale'), status = "danger", value = TRUE, inline = TRUE, width = '100%')
  })
  output$Plot1 <- shiny::renderUI({
    if(length(ModelOutputList) != 0 && length(names(ModelOutputList$PlotList)) != 0) bla <- names(ModelOutputList$PlotList) else bla <- NULL
    if(Debug) {print(bla)}
    x <- RemixAutoML:::AvailableAppInsightsPlots(x = bla, PlotNamesLookup = PlotNamesLookup, Debug = Debug)
    if(Debug) {print('Plot1 Charts Available 2nd plot section'); print(x)}
    RemixAutoML::SelectizeInput(InputID = 'Plot1', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Plot Type Selection'), Choices = c(x), Multiple = FALSE, SelectedDefault = NULL, CloseAfterSelect = TRUE)
  })
  output$Plot2 <- shiny::renderUI({
    if(length(ModelOutputList) != 0 && length(names(ModelOutputList$PlotList)) != 0) bla <- names(ModelOutputList$PlotList) else bla <- NULL
    x <- RemixAutoML:::AvailableAppInsightsPlots(x = bla, PlotNamesLookup = PlotNamesLookup, Debug = Debug)
    RemixAutoML::SelectizeInput(InputID = 'Plot2', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Plot Type Selection'), Choices = c(x), Multiple = FALSE, SelectedDefault = NULL, CloseAfterSelect = TRUE)
  })
  output$Plot3 <- shiny::renderUI({
    if(length(ModelOutputList) != 0 && length(names(ModelOutputList$PlotList)) != 0) bla <- names(ModelOutputList$PlotList) else bla <- NULL
    x <- RemixAutoML:::AvailableAppInsightsPlots(x = bla, PlotNamesLookup = PlotNamesLookup, Debug = Debug)
    RemixAutoML::SelectizeInput(InputID = 'Plot3', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Plot Type Selection'), Choices = c(x), Multiple = FALSE, SelectedDefault = NULL, CloseAfterSelect = TRUE)
  })
  output$Plot4 <- shiny::renderUI({
    if(length(ModelOutputList) != 0 && length(names(ModelOutputList$PlotList)) != 0) bla <- names(ModelOutputList$PlotList) else bla <- NULL
    x <- RemixAutoML:::AvailableAppInsightsPlots(x = bla, PlotNamesLookup = PlotNamesLookup, Debug = Debug)
    RemixAutoML::SelectizeInput(InputID = 'Plot4', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Plot Type Selection'), Choices = c(x), Multiple = FALSE, SelectedDefault = NULL, CloseAfterSelect = TRUE)
  })

  # Dragula for PlotType
  output$PlotTypeDragula <- shiny::renderUI({
    dragheight <- '40px'
    tags$style("#display {background: #2d2d2d; border: 10px solid #000000; border-radius: 5px; font-size: 2em; color: white; height: 100px; min-width:200px; text-align: center; padding: 1em; display:table-cell; vertical-align:middle; } #drag-elements { display: block; background-color: #dfdfdf; border-radius: 5px; min-height: 50px; margin: 0 auto; padding: 2em; } #drag-elements > div { text-align: center; float: left; padding: 1em; margin: 0 1em 1em 0; box-shadow: 1px 1px 1px rgba(0, 0, 0, 0.3); border-radius: 100px; border: 2px solid #ececec; background: #F7F7F7; transition: all .5s ease; } #drag-elements > div:active { -webkit-animation: wiggle 0.3s 0s infinite ease-in; animation: wiggle 0.3s 0s infinite ease-in; opacity: .6; border: 2px solid #000; } #drag-elements > div:hover { border: 2px solid gray; background-color: #e5e5e5; } #drop-target { border: 2px dashed #D9D9D9; border-radius: 5px; min-height: 50px; margin: 0 auto; margin-top: 10px; padding: 2em; display: block; text-align: center; } #drop-target > div { transition: all .5s; text-align: center; float: left; padding: 1em; margin: 0 1em 1em 0; box-shadow: 1px 1px 1px rgba(0, 0, 0, 0.3); border-radius: 5px; border: 2px solid skyblue; background: #F7F7F7; transition: all .5s ease; } #drop-target > div:active { -webkit-animation: wiggle 0.3s 0s infinite ease-in; animation: wiggle 0.3s 0s infinite ease-in; opacity: .6; border: 2px solid #000; }")
    esquisse::dragulaInput(
      height = dragheight,
      replace = FALSE,
      inputId = 'PlotTypeDragula',
      label = 'Drag and Drop Plot Types',
      sourceLabel = 'Plots',
      targetsLabels = c('TopLeft', 'LowerLeft', 'TopRight', 'LowerRight'),
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
      choices = c('Plot1', 'Plot2', 'Plot3', 'Plot4'))
  })

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

  # Global Setting
  output$PlotWidth <- shiny::renderUI({
    RemixAutoML::NumericInput(InputID = "PlotWidth", Label=tags$span(style='color: blue;', 'Plot Width'), Step = 50, Min = 500, Max = 3500, Value = 1600)
  })
  output$PlotHeight <- shiny::renderUI({
    RemixAutoML::NumericInput(InputID = "PlotHeight", Label=tags$span(style='color: blue;', 'Plot Height'), Step = 25, Min = 300, Max = 3500, Value = 500)
  })

  # Shapely Agg Method
  output$ShapAggMethod1 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID='ShapAggMethod1', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Shap Agg Method 1'), Choices=c('mean','absmean','meanabs','sd','median','absmedian','medianabs'), SelectedDefault='mean', Multiple=FALSE, CloseAfterSelect = TRUE)
  })
  output$ShapAggMethod2 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID='ShapAggMethod2', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Shap Agg Method 2'), Choices=c('mean','absmean','meanabs','sd','median','absmedian','medianabs'), SelectedDefault='mean', Multiple=FALSE, CloseAfterSelect = TRUE)
  })
  output$ShapAggMethod3 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID='ShapAggMethod3', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Shap Agg Method 3'), Choices=c('mean','absmean','meanabs','sd','median','absmedian','medianabs'), SelectedDefault='mean', Multiple=FALSE, CloseAfterSelect = TRUE)
  })
  output$ShapAggMethod4 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID='ShapAggMethod4', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Shap Agg Method 4'), Choices=c('mean','absmean','meanabs','sd','median','absmedian','medianabs'), SelectedDefault='mean', Multiple=FALSE, CloseAfterSelect = TRUE)
  })

  # Percentile Buckets
  output$Percentile_Buckets1 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID='Percentile_Buckets1', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Percentile Buckets 1'), Choices=1:100, SelectedDefault=20, Multiple=FALSE, CloseAfterSelect = TRUE)
  })
  output$Percentile_Buckets2 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID='Percentile_Buckets2', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Percentile Buckets 2'), Choices=1:100, SelectedDefault=20, Multiple=FALSE, CloseAfterSelect = TRUE)
  })
  output$Percentile_Buckets3 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID='Percentile_Buckets3', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Percentile Buckets 3'), Choices=1:100, SelectedDefault=20, Multiple=FALSE, CloseAfterSelect = TRUE)
  })
  output$Percentile_Buckets4 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID='Percentile_Buckets4', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Percentile Buckets 4'), Choices=1:100, SelectedDefault=20, Multiple=FALSE, CloseAfterSelect = TRUE)
  })

  # Add GAM Fit to Plot
  output$GamFitScatter1 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID='GamFitScatter1', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Fit Gam on Scatter or Copula 1'), Choices=c('TRUE','FALSE'), SelectedDefault=FALSE, Multiple=FALSE, CloseAfterSelect = TRUE)
  })

  output$GamFitScatter2 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID='GamFitScatter2', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Fit Gam on Scatter or Copula 2'), Choices=c('TRUE','FALSE'), SelectedDefault=FALSE, Multiple=FALSE, CloseAfterSelect = TRUE)
  })
  output$GamFitScatter3 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID='GamFitScatter3', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Fit Gam on Scatter or Copula 3'), Choices=c('TRUE','FALSE'), SelectedDefault=FALSE, Multiple=FALSE, CloseAfterSelect = TRUE)
  })
  output$GamFitScatter4 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID='GamFitScatter4', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Fit Gam on Scatter or Copula 4'), Choices=c('TRUE','FALSE'), SelectedDefault=FALSE, Multiple=FALSE, CloseAfterSelect = TRUE)
  })

  # Histogram Bins
  output$NumberBins1 <- shiny::renderUI({
    RemixAutoML::NumericInput(InputID='NumberBins1', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'# of Bins for Histogram 1'), Min=1, Max=1000, Step=5, Value=30)
  })
  output$NumberBins2 <- shiny::renderUI({
    RemixAutoML::NumericInput(InputID='NumberBins2', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'# of Bins for Histogram 2'), Min=1, Max=1000, Step=5, Value=30)
  })
  output$NumberBins3 <- shiny::renderUI({
    RemixAutoML::NumericInput(InputID='NumberBins3', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'# of Bins for Histogram 3'), Min=1, Max=1000, Step=5, Value=30)
  })
  output$NumberBins4 <- shiny::renderUI({
    RemixAutoML::NumericInput(InputID='NumberBins4', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'# of Bins for Histogram 4'), Min=1, Max=1000, Step=5, Value=30)
  })

  # YTicks Values (NULL is whats handled by RemixAutoML:::YTicks())
  output$YTicks1 <- shiny::renderUI({
    yy <- tryCatch({YVar1()}, error = function(x) 'None')
    if(any(yy %in% names(data))) temp <- data else temp <- ModelData
    RemixAutoML::SelectizeInput(InputID = 'YTicks1', Label = tags$span(style='color: blue;', 'Y-Axis 1 Ticks'), Choices = RemixAutoML:::YTicks(temp, yvar = yy), SelectedDefault = 'Default', Multiple = TRUE, CloseAfterSelect = TRUE)
  })
  output$YTicks2 <- shiny::renderUI({
    yy <- tryCatch({YVar2()}, error = function(x) 'None')
    if(any(yy %in% names(data))) temp <- data else temp <- ModelData
    RemixAutoML::SelectizeInput(InputID = 'YTicks2', Label = tags$span(style='color: blue;', 'Y-Axis 2 Ticks'), Choices = RemixAutoML:::YTicks(temp, yvar = yy), SelectedDefault = 'Default', Multiple = TRUE, CloseAfterSelect = TRUE)
  })
  output$YTicks3 <- shiny::renderUI({
    yy <- tryCatch({YVar3()}, error = function(x) 'None')
    if(any(yy %in% names(data))) temp <- data else temp <- ModelData
    RemixAutoML::SelectizeInput(InputID = 'YTicks3', Label = tags$span(style='color: blue;', 'Y-Axis 3 Ticks'), Choices = RemixAutoML:::YTicks(temp, yvar = yy), SelectedDefault = 'Default', Multiple = TRUE, CloseAfterSelect = TRUE)
  })
  output$YTicks4 <- shiny::renderUI({
    yy <- tryCatch({YVar4()}, error = function(x) 'None')
    if(any(yy %in% names(data))) temp <- data else temp <- ModelData
    RemixAutoML::SelectizeInput(InputID = 'YTicks4', Label = tags$span(style='color: blue;', 'Y-Axis 4 Ticks'), Choices = RemixAutoML:::YTicks(temp, yvar = yy), SelectedDefault = 'Default', Multiple = TRUE, CloseAfterSelect = TRUE)
  })

  # XTicks Values ('None' is whats handled by RemixAutoML:::XTicks())
  output$XTicks1 <- shiny::renderUI({
    xx <- tryCatch({XVar1()}, error = function(x) 'None')
    dd <- tryCatch({DateVar1()}, error = function(x) 'None')
    if(any(xx %in% names(data))) temp <- data else temp <- ModelData
    RemixAutoML::SelectizeInput(InputID = 'XTicks1', Label = tags$span(style='color: blue;', 'X-Axis 1 Ticks'), Choices = RemixAutoML:::XTicks(temp, xvar=xx,datevar=dd), SelectedDefault = 'Default', Multiple = TRUE, CloseAfterSelect = TRUE)
  })
  output$XTicks2 <- shiny::renderUI({
    xx <- tryCatch({XVar2()}, error = function(x) 'None')
    dd <- tryCatch({DateVar2()}, error = function(x) 'None')
    if(any(xx %in% names(data))) temp <- data else temp <- ModelData
    RemixAutoML::SelectizeInput(InputID = 'XTicks2', Label = tags$span(style='color: blue;', 'X-Axis 2 Ticks'), Choices = RemixAutoML:::XTicks(temp, xvar=xx,datevar=dd), SelectedDefault = 'Default', Multiple = TRUE, CloseAfterSelect = TRUE)
  })
  output$XTicks3 <- shiny::renderUI({
    xx <- tryCatch({XVar3()}, error = function(x) 'None')
    dd <- tryCatch({DateVar3()}, error = function(x) 'None')
    if(any(xx %in% names(data))) temp <- data else temp <- ModelData
    RemixAutoML::SelectizeInput(InputID = 'XTicks3', Label = tags$span(style='color: blue;', 'X-Axis 3 Ticks'), Choices = RemixAutoML:::XTicks(temp, xvar=xx,datevar=dd), SelectedDefault = 'Default', Multiple = TRUE, CloseAfterSelect = TRUE)
  })
  output$XTicks4 <- shiny::renderUI({
    xx <- tryCatch({XVar4()}, error = function(x) 'None')
    dd <- tryCatch({DateVar4()}, error = function(x) 'None')
    if(any(xx %in% names(data))) temp <- data else temp <- ModelData
    RemixAutoML::SelectizeInput(InputID = 'XTicks4', Label = tags$span(style='color: blue;', 'X-Axis 4 Ticks'), Choices = RemixAutoML:::XTicks(temp, xvar=xx,datevar=dd), SelectedDefault = 'Default', Multiple = TRUE, CloseAfterSelect = TRUE)
  })

  # Other values
  output$SampleSize <- shiny::renderUI({
    RemixAutoML::NumericInput(InputID = 'SampleSize', Label = tags$span(style='color: blue;', 'Sample size for plotting'), Step = 50000, Min = 0, Max = 1000000, Value = 100000)
  })
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
  output$TextSize1 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'TextSize1', Label = tags$span(style='color: blue;', 'Text Size'),Choices = c(as.character(seq(1,100,1))), SelectedDefault = '14', Multiple = FALSE, CloseAfterSelect = TRUE)
  })
  output$TextSize2 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'TextSize2', Label = tags$span(style='color: blue;', 'Text Size'),Choices = c(as.character(seq(1,100,1))), SelectedDefault = '14', Multiple = FALSE, CloseAfterSelect = TRUE)
  })
  output$TextSize3 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'TextSize3', Label = tags$span(style='color: blue;', 'Text Size'),Choices = c(as.character(seq(1,100,1))), SelectedDefault = '14', Multiple = FALSE, CloseAfterSelect = TRUE)
  })
  output$TextSize4 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'TextSize4', Label = tags$span(style='color: blue;', 'Text Size'),Choices = c(as.character(seq(1,100,1))), SelectedDefault = '14', Multiple = FALSE, CloseAfterSelect = TRUE)
  })
  output$OutlierSize1 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'OutlierSize1', Label = tags$span(style='color: blue;', 'Outlier Size'), Choices = c(seq(0.01,2,0.01)), SelectedDefault = 0.01, Multiple = FALSE, CloseAfterSelect = TRUE)
  })
  output$OutlierSize2 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'OutlierSize2', Label = tags$span(style='color: blue;', 'Outlier Size'), Choices = c(seq(0.01,2,0.01)), SelectedDefault = 0.01, Multiple = FALSE, CloseAfterSelect = TRUE)
  })
  output$OutlierSize3 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'OutlierSize3', Label = tags$span(style='color: blue;', 'Outlier Size'), Choices = c(seq(0.01,2,0.01)), SelectedDefault = 0.01, Multiple = FALSE, CloseAfterSelect = TRUE)
  })
  output$OutlierSize4 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'OutlierSize4', Label = tags$span(style='color: blue;', 'Outlier Size'), Choices = c(seq(0.01,2,0.01)), SelectedDefault = 0.01, Multiple = FALSE, CloseAfterSelect = TRUE)
  })
  output$LegendPosition1 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'LegendPosition1', Label = tags$span(style='color: blue;', 'Legend Position'), Choices = c('bottom','left','right','top','none'), SelectedDefault = 'bottom', Multiple = FALSE, CloseAfterSelect = TRUE)
  })
  output$LegendPosition2 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'LegendPosition2', Label = tags$span(style='color: blue;', 'Legend Position'), Choices = c('bottom','left','right','top','none'), SelectedDefault = 'bottom', Multiple = FALSE, CloseAfterSelect = TRUE)
  })
  output$LegendPosition3 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'LegendPosition3', Label = tags$span(style='color: blue;', 'Legend Position'), Choices = c('bottom','left','right','top','none'), SelectedDefault = 'bottom', Multiple = FALSE, CloseAfterSelect = TRUE)
  })
  output$LegendPosition4 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'LegendPosition4', Label = tags$span(style='color: blue;', 'Legend Position'), Choices = c('bottom','left','right','top','none'), SelectedDefault = 'bottom', Multiple = FALSE, CloseAfterSelect = TRUE)
  })
  output$LegendBorderSize1 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'LegendBorderSize1', Label = tags$span(style='color: blue;', 'Legend Border Size'), Choices = c(as.character(seq(0.01,2,0.01))), SelectedDefault = as.character(0.01), Multiple = FALSE, CloseAfterSelect = TRUE)
  })
  output$LegendBorderSize2 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'LegendBorderSize2', Label = tags$span(style='color: blue;', 'Legend Border Size'), Choices = c(as.character(seq(0.01,2,0.01))), SelectedDefault = as.character(0.01), Multiple = FALSE, CloseAfterSelect = TRUE)
  })
  output$LegendBorderSize3 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'LegendBorderSize3', Label = tags$span(style='color: blue;', 'Legend Border Size'), Choices = c(as.character(seq(0.01,2,0.01))), SelectedDefault = as.character(0.01), Multiple = FALSE, CloseAfterSelect = TRUE)
  })
  output$LegendBorderSize4 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'LegendBorderSize4', Label = tags$span(style='color: blue;', 'Legend Border Size'), Choices = c(as.character(seq(0.01,2,0.01))), SelectedDefault = as.character(0.01), Multiple = FALSE, CloseAfterSelect = TRUE)
  })
  output$LegendLineType1 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'LegendLineType1', Label = tags$span(style='color: blue;', 'Legend Border Type'), Choices = c('solid','blank','dashed','dotdash','dotted','longlash','twodash'), SelectedDefault = 'solid', Multiple = FALSE, CloseAfterSelect = TRUE)
  })
  output$LegendLineType2 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'LegendLineType2', Label = tags$span(style='color: blue;', 'Legend Border Type'), Choices = c('solid','blank','dashed','dotdash','dotted','longlash','twodash'), SelectedDefault = 'solid', Multiple = FALSE, CloseAfterSelect = TRUE)
  })
  output$LegendLineType3 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'LegendLineType3', Label = tags$span(style='color: blue;', 'Legend Border Type'), Choices = c('solid','blank','dashed','dotdash','dotted','longlash','twodash'), SelectedDefault = 'solid', Multiple = FALSE, CloseAfterSelect = TRUE)
  })
  output$LegendLineType4 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'LegendLineType4', Label = tags$span(style='color: blue;', 'Legend Border Type'), Choices = c('solid','blank','dashed','dotdash','dotted','longlash','twodash'), SelectedDefault = 'solid', Multiple = FALSE, CloseAfterSelect = TRUE)
  })

  # Color boxes
  output$TextColor1 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'TextColor1', Label = tags$span(style='color: blue;', 'Text Color'), Choices = grDevices::colors(), SelectedDefault = 'darkblue', Multiple = FALSE, CloseAfterSelect = TRUE)
  })
  output$TextColor2 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'TextColor2', Label = tags$span(style='color: blue;', 'Text Color'), Choices = grDevices::colors(), SelectedDefault = 'darkblue', Multiple = FALSE, CloseAfterSelect = TRUE)
  })
  output$TextColor3 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'TextColor3', Label = tags$span(style='color: blue;', 'Text Color'), Choices = grDevices::colors(), SelectedDefault = 'darkblue', Multiple = FALSE, CloseAfterSelect = TRUE)
  })
  output$TextColor4 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'TextColor4', Label = tags$span(style='color: blue;', 'Text Color'), Choices = grDevices::colors(), SelectedDefault = 'darkblue', Multiple = FALSE, CloseAfterSelect = TRUE)
  })
  output$ChartColor1 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'ChartColor1', Label = tags$span(style='color: blue;', 'Chart Color'), Choices = grDevices::colors(), SelectedDefault = 'aliceblue', Multiple = FALSE, CloseAfterSelect = TRUE)
  })
  output$ChartColor2 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'ChartColor2', Label = tags$span(style='color: blue;', 'Chart Color'), Choices = grDevices::colors(), SelectedDefault = 'aliceblue', Multiple = FALSE, CloseAfterSelect = TRUE)
  })
  output$ChartColor3 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'ChartColor3', Label = tags$span(style='color: blue;', 'Chart Color'), Choices = grDevices::colors(), SelectedDefault = 'aliceblue', Multiple = FALSE, CloseAfterSelect = TRUE)
  })
  output$ChartColor4 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'ChartColor4', Label = tags$span(style='color: blue;', 'Chart Color'), Choices = grDevices::colors(), SelectedDefault = 'aliceblue', Multiple = FALSE, CloseAfterSelect = TRUE)
  })
  output$GridColor1 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'GridColor1', Label = tags$span(style='color: blue;', 'Grid Lines Color'), Choices = grDevices::colors(), SelectedDefault = 'lightsteelblue1', Multiple = FALSE, CloseAfterSelect = TRUE)
  })
  output$GridColor2 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'GridColor2', Label = tags$span(style='color: blue;', 'Grid Lines Color'), Choices = grDevices::colors(), SelectedDefault = 'lightsteelblue1', Multiple = FALSE, CloseAfterSelect = TRUE)
  })
  output$GridColor3 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'GridColor3', Label = tags$span(style='color: blue;', 'Grid Lines Color'), Choices = grDevices::colors(), SelectedDefault = 'lightsteelblue1', Multiple = FALSE, CloseAfterSelect = TRUE)
  })
  output$GridColor4 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'GridColor4', Label = tags$span(style='color: blue;', 'Grid Lines Color'), Choices = grDevices::colors(), SelectedDefault = 'lightsteelblue1', Multiple = FALSE, CloseAfterSelect = TRUE)
  })
  output$BackGroundColor1 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'BackGroundColor1', Label = tags$span(style='color: blue;', 'Background Color'), Choices = grDevices::colors(), SelectedDefault = 'gray95', Multiple = FALSE, CloseAfterSelect = TRUE)
  })
  output$BackGroundColor2 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'BackGroundColor2', Label = tags$span(style='color: blue;', 'Background Color'), Choices = grDevices::colors(), SelectedDefault = 'gray95', Multiple = FALSE, CloseAfterSelect = TRUE)
  })
  output$BackGroundColor3 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'BackGroundColor3', Label = tags$span(style='color: blue;', 'Background Color'), Choices = grDevices::colors(), SelectedDefault = 'gray95', Multiple = FALSE, CloseAfterSelect = TRUE)
  })
  output$BackGroundColor4 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'BackGroundColor4', Label = tags$span(style='color: blue;', 'Background Color'), Choices = grDevices::colors(), SelectedDefault = 'gray95', Multiple = FALSE, CloseAfterSelect = TRUE)
  })
  output$BorderColor1 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'BorderColor1', Label = tags$span(style='color: blue;', 'Border Color'), Choices = grDevices::colors(), SelectedDefault = 'lightsteelblue4', Multiple = FALSE, CloseAfterSelect = TRUE)
  })
  output$BorderColor2 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'BorderColor2', Label = tags$span(style='color: blue;', 'Border Color'), Choices = grDevices::colors(), SelectedDefault = 'lightsteelblue4', Multiple = FALSE, CloseAfterSelect = TRUE)
  })
  output$BorderColor3 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'BorderColor3', Label = tags$span(style='color: blue;', 'Border Color'), Choices = grDevices::colors(), SelectedDefault = 'lightsteelblue4', Multiple = FALSE, CloseAfterSelect = TRUE)
  })
  output$BorderColor4 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'BorderColor4', Label = tags$span(style='color: blue;', 'Border Color'), Choices = grDevices::colors(), SelectedDefault = 'lightsteelblue4', Multiple = FALSE, CloseAfterSelect = TRUE)
  })
  output$OutlierColor1 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'OutlierColor1', Label = tags$span(style='color: blue;', 'Outlier Color'), Choices = grDevices::colors(), SelectedDefault = 'blue', Multiple = FALSE, CloseAfterSelect = TRUE)
  })
  output$OutlierColor2 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'OutlierColor2', Label = tags$span(style='color: blue;', 'Outlier Color'), Choices = grDevices::colors(), SelectedDefault = 'blue', Multiple = FALSE, CloseAfterSelect = TRUE)
  })
  output$OutlierColor3 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'OutlierColor3', Label = tags$span(style='color: blue;', 'Outlier Color'), Choices = grDevices::colors(), SelectedDefault = 'blue', Multiple = FALSE, CloseAfterSelect = TRUE)
  })
  output$OutlierColor4 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'OutlierColor4', Label = tags$span(style='color: blue;', 'Outlier Color'), Choices = grDevices::colors(), SelectedDefault = 'blue', Multiple = FALSE, CloseAfterSelect = TRUE)
  })
  output$FillColor1 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'FillColor1', Label = tags$span(style='color: blue;', 'BoxPlot Fill Color'), Choices = grDevices::colors(), SelectedDefault = 'gray70', Multiple = FALSE, CloseAfterSelect = TRUE)
  })
  output$FillColor2 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'FillColor2', Label = tags$span(style='color: blue;', 'BoxPlot Fill Color'), Choices = grDevices::colors(), SelectedDefault = 'gray70', Multiple = FALSE, CloseAfterSelect = TRUE)
  })
  output$FillColor3 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'FillColor3', Label = tags$span(style='color: blue;', 'BoxPlot Fill Color'), Choices = grDevices::colors(), SelectedDefault = 'gray70', Multiple = FALSE, CloseAfterSelect = TRUE)
  })
  output$FillColor4 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'FillColor4', Label = tags$span(style='color: blue;', 'BoxPlot Fill Color'), Choices = grDevices::colors(), SelectedDefault = 'gray70', Multiple = FALSE, CloseAfterSelect = TRUE)
  })
  output$SubTitleColor1 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'SubTitleColor1', Label = tags$span(style='color: blue;', 'Subtitle Color'), Choices = grDevices::colors(), SelectedDefault = 'blue', Multiple = FALSE, CloseAfterSelect = TRUE)
  })
  output$SubTitleColor2 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'SubTitleColor2', Label = tags$span(style='color: blue;', 'Subtitle Color'), Choices = grDevices::colors(), SelectedDefault = 'blue', Multiple = FALSE, CloseAfterSelect = TRUE)
  })
  output$SubTitleColor3 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'SubTitleColor3', Label = tags$span(style='color: blue;', 'Subtitle Color'), Choices = grDevices::colors(), SelectedDefault = 'blue', Multiple = FALSE, CloseAfterSelect = TRUE)
  })
  output$SubTitleColor4 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'SubTitleColor4', Label = tags$span(style='color: blue;', 'Subtitle Color'), Choices = grDevices::colors(), SelectedDefault = 'blue', Multiple = FALSE, CloseAfterSelect = TRUE)
  })

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Group Variables                      ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  # Select GroupVars
  output$GroupVars1 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID='GroupVars1', Label=tags$span(style='color: blue;', 'Select Group Variables'), Choices=c(names(data)), SelectedDefault=NULL, Multiple=TRUE, MaxVars = 3)
  })
  output$GroupVars2 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID='GroupVars2', Label=tags$span(style='color: blue;', 'Select Group Variables'), Choices=c(names(data)), SelectedDefault=NULL, Multiple=TRUE, MaxVars = 3)
  })
  output$GroupVars3 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID='GroupVars3', Label=tags$span(style='color: blue;', 'Select Group Variables'), Choices=c(names(data)), SelectedDefault=NULL, Multiple=TRUE, MaxVars = 3)
  })
  output$GroupVars4 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID='GroupVars4', Label=tags$span(style='color: blue;', 'Select Group Variables'), Choices=c(names(data)), SelectedDefault=NULL, Multiple=TRUE, MaxVars = 3)
  })

  # Reactive Group Variables
  SelectedGroups1 <- shiny::reactive({
    RemixAutoML::ReturnParam(xx = input[['GroupVars1']], VarName = 'GroupVars1', Default = NULL, Switch = TRUE, Type = 'character')
  })
  SelectedGroups2 <- shiny::reactive({
    RemixAutoML::ReturnParam(xx = input[['GroupVars2']], VarName = 'GroupVars2', Default = NULL, Switch = TRUE, Type = 'character')
  })
  SelectedGroups3 <- shiny::reactive({
    RemixAutoML::ReturnParam(xx = input[['GroupVars3']], VarName = 'GroupVars3', Default = NULL, Switch = TRUE, Type = 'character')
  })
  SelectedGroups4 <- shiny::reactive({
    RemixAutoML::ReturnParam(xx = input[['GroupVars4']], VarName = 'GroupVars4', Default = NULL, Switch = TRUE, Type = 'character')
  })

  # Group Levels
  output$Levels_1_1 <- shiny::renderUI({
    sgs <- RemixAutoML:::LevelValues(SelectedGroups1()); if(Debug) print('PickerInput_GetLevels 1')
    RemixAutoML::PickerInput_GetLevels2(DataExist=exists('data'), InputID='Levels_1_1', InputID2=sgs, Choices=RemixAutoML::UniqueLevels(input, data, 1L, GroupVars=sgs), NumGroupVar=1L, Multiple=TRUE, SelectedDefault=NULL)
  })
  output$Levels_1_2 <- shiny::renderUI({
    sgs <- RemixAutoML:::LevelValues(SelectedGroups1()); if(Debug) print('PickerInput_GetLevels 2')
    RemixAutoML::PickerInput_GetLevels2(DataExist=exists('data'), InputID='Levels_1_2', InputID2=sgs, Choices=RemixAutoML::UniqueLevels(input, data, 2L, GroupVars=sgs), NumGroupVar=2L, Multiple=TRUE, SelectedDefault=NULL)
  })
  output$Levels_1_3 <- shiny::renderUI({
    sgs <- RemixAutoML:::LevelValues(SelectedGroups1()); if(Debug) print('PickerInput_GetLevels 3')
    RemixAutoML::PickerInput_GetLevels2(DataExist=exists('data'), InputID='Levels_1_3', InputID2=sgs, Choices=RemixAutoML::UniqueLevels(input, data, 3L, GroupVars=sgs), NumGroupVar=3L, Multiple=TRUE, SelectedDefault=NULL)
  })
  output$Levels_2_1 <- shiny::renderUI({
    sgs <- RemixAutoML:::LevelValues(SelectedGroups2()); if(Debug) print('PickerInput_GetLevels 1')
    RemixAutoML::PickerInput_GetLevels2(DataExist=exists('data'), InputID='Levels_2_1', InputID2=sgs, Choices=RemixAutoML::UniqueLevels(input, data, 1L, GroupVars=sgs), NumGroupVar=1L, Multiple=TRUE, SelectedDefault=NULL)
  })
  output$Levels_2_2 <- shiny::renderUI({
    sgs <- RemixAutoML:::LevelValues(SelectedGroups2()); if(Debug) print('PickerInput_GetLevels 2')
    RemixAutoML::PickerInput_GetLevels2(DataExist=exists('data'), InputID='Levels_2_2', InputID2=sgs, Choices=RemixAutoML::UniqueLevels(input, data, 2L, GroupVars=sgs), NumGroupVar=2L, Multiple=TRUE, SelectedDefault=NULL)
  })
  output$Levels_2_3 <- shiny::renderUI({
    sgs <- RemixAutoML:::LevelValues(SelectedGroups2()); if(Debug) print('PickerInput_GetLevels 3')
    RemixAutoML::PickerInput_GetLevels2(DataExist=exists('data'), InputID='Levels_2_3', InputID2=sgs, Choices=RemixAutoML::UniqueLevels(input, data, 3L, GroupVars=sgs), NumGroupVar=3L, Multiple=TRUE, SelectedDefault=NULL)
  })
  output$Levels_3_1 <- shiny::renderUI({
    sgs <- RemixAutoML:::LevelValues(SelectedGroups3()); if(Debug) print('PickerInput_GetLevels 1')
    RemixAutoML::PickerInput_GetLevels2(DataExist=exists('data'), InputID='Levels_3_1', InputID2=sgs, Choices=RemixAutoML::UniqueLevels(input, data, 1L, GroupVars=sgs), NumGroupVar=1L, Multiple=TRUE, SelectedDefault=NULL)
  })
  output$Levels_3_2 <- shiny::renderUI({
    sgs <- RemixAutoML:::LevelValues(SelectedGroups3()); if(Debug) print('PickerInput_GetLevels 2')
    RemixAutoML::PickerInput_GetLevels2(DataExist=exists('data'), InputID='Levels_3_2', InputID2=sgs, Choices=RemixAutoML::UniqueLevels(input, data, 2L, GroupVars=sgs), NumGroupVar=2L, Multiple=TRUE, SelectedDefault=NULL)
  })
  output$Levels_3_3 <- shiny::renderUI({
    sgs <- RemixAutoML:::LevelValues(SelectedGroups3()); if(Debug) print('PickerInput_GetLevels 3')
    RemixAutoML::PickerInput_GetLevels2(DataExist=exists('data'), InputID='Levels_3_3', InputID2=sgs, Choices=RemixAutoML::UniqueLevels(input, data, 3L, GroupVars=sgs), NumGroupVar=3L, Multiple=TRUE, SelectedDefault=NULL)
  })
  output$Levels_4_1 <- shiny::renderUI({
    sgs <- RemixAutoML:::LevelValues(SelectedGroups4()); if(Debug) print('PickerInput_GetLevels 1')
    RemixAutoML::PickerInput_GetLevels2(DataExist=exists('data'), InputID='Levels_4_1', InputID2=sgs, Choices=RemixAutoML::UniqueLevels(input, data, 1L, GroupVars=sgs), NumGroupVar=1L, Multiple=TRUE, SelectedDefault=NULL)
  })
  output$Levels_4_2 <- shiny::renderUI({
    sgs <- RemixAutoML:::LevelValues(SelectedGroups4()); if(Debug) print('PickerInput_GetLevels 2')
    RemixAutoML::PickerInput_GetLevels2(DataExist=exists('data'), InputID='Levels_4_2', InputID2=sgs, Choices=RemixAutoML::UniqueLevels(input, data, 2L, GroupVars=sgs), NumGroupVar=2L, Multiple=TRUE, SelectedDefault=NULL)
  })
  output$Levels_4_3 <- shiny::renderUI({
    sgs <- RemixAutoML:::LevelValues(SelectedGroups4()); if(Debug) print('PickerInput_GetLevels 3')
    RemixAutoML::PickerInput_GetLevels2(DataExist=exists('data'), InputID='Levels_4_3', InputID2=sgs, Choices=RemixAutoML::UniqueLevels(input, data, 3L, GroupVars=sgs), NumGroupVar=3L, Multiple=TRUE, SelectedDefault=NULL)
  })

  # Faceting
  output$FacetVar_1_1 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID='FacetVar_1_1', Label = tags$span(style='color: blue;', 'Facet Variable 1'), Choices = c(names(data)), Multiple = FALSE)
  })
  output$FacetVar_1_2 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID='FacetVar_1_2', Label = tags$span(style='color: blue;', 'Facet Variable 2'), Choices = c(names(data)), Multiple = FALSE)
  })
  output$FacetVar_2_1 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID='FacetVar_2_1', Label = tags$span(style='color: blue;', 'Facet Variable 1'), Choices = c(names(data)), Multiple = FALSE)
  })
  output$FacetVar_2_2 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID='FacetVar_2_2', Label = tags$span(style='color: blue;', 'Facet Variable 2'), Choices = c(names(data)), Multiple = FALSE)
  })
  output$FacetVar_3_1 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID='FacetVar_3_1', Label = tags$span(style='color: blue;', 'Facet Variable 1'), Choices = c(names(data)), Multiple = FALSE)
  })
  output$FacetVar_3_2 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID='FacetVar_3_2', Label = tags$span(style='color: blue;', 'Facet Variable 2'), Choices = c(names(data)), Multiple = FALSE)
  })
  output$FacetVar_4_1 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID='FacetVar_4_1', Label = tags$span(style='color: blue;', 'Facet Variable 1'), Choices = c(names(data)), Multiple = FALSE)
  })
  output$FacetVar_4_2 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID='FacetVar_4_2', Label = tags$span(style='color: blue;', 'Facet Variable 2'), Choices = c(names(data)), Multiple = FALSE)
  })

  # Sizing
  output$SizeVar1 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'SizeVar1', Label = tags$span(style='color: blue;', 'Size Variable'), Choices = c(names(data)), Multiple = FALSE)
  })
  output$SizeVar2 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'SizeVar2', Label = tags$span(style='color: blue;', 'Size Variable'), Choices = c(names(data)), Multiple = FALSE)
  })
  output$SizeVar3 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'SizeVar3', Label = tags$span(style='color: blue;', 'Size Variable'), Choices = c(names(data)), Multiple = FALSE)
  })
  output$SizeVar4 <- shiny::renderUI({
    RemixAutoML::SelectizeInput(InputID = 'SizeVar4', Label = tags$span(style='color: blue;', 'Size Variable'), Choices = c(names(data)), Multiple = FALSE)
  })

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Filter Variables                     ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  # Filter Variables
  output$FilterVariable_1_1 <- shiny::renderUI({
    if(any(RemixAutoML:::AvailableAppInsightsPlots(x = NULL, PlotNamesLookup = PlotNamesLookup, Debug = Debug) %in% tryCatch({input[['Plot1']]}, error = function(x) NULL))) {
      ModelVars <- names(data)
    } else {
      ModelVars <- names(ModelData)
    }
    RemixAutoML::SelectizeInput(InputID = 'FilterVariable_1_1', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Filter Variable 1'), Choices = c(ModelVars), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
  })
  output$FilterVariable_1_2 <- shiny::renderUI({
    if(any(RemixAutoML:::AvailableAppInsightsPlots(x = NULL, PlotNamesLookup = PlotNamesLookup, Debug = Debug) %in% tryCatch({input[['Plot1']]}, error = function(x) NULL))) {
      ModelVars <- names(data)
    } else {
      ModelVars <- names(ModelData)
    }
    RemixAutoML::SelectizeInput(InputID = 'FilterVariable_1_2', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Filter Variable 2'), Choices = c(ModelVars), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
  })
  output$FilterVariable_1_3 <- shiny::renderUI({
    if(any(RemixAutoML:::AvailableAppInsightsPlots(x = NULL, PlotNamesLookup = PlotNamesLookup, Debug = Debug) %in% tryCatch({input[['Plot1']]}, error = function(x) NULL))) {
      ModelVars <- names(data)
    } else {
      ModelVars <- names(ModelData)
    }
    RemixAutoML::SelectizeInput(InputID = 'FilterVariable_1_3', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Filter Variable 3'), Choices = c(ModelVars), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
  })
  output$FilterVariable_1_4 <- shiny::renderUI({
    if(any(RemixAutoML:::AvailableAppInsightsPlots(x = NULL, PlotNamesLookup = PlotNamesLookup, Debug = Debug) %in% tryCatch({input[['Plot1']]}, error = function(x) NULL))) {
      ModelVars <- names(data)
    } else {
      ModelVars <- names(ModelData)
    }
    RemixAutoML::SelectizeInput(InputID = 'FilterVariable_1_4', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Filter Variable 4'), Choices = c(ModelVars), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
  })
  output$FilterVariable_2_1 <- shiny::renderUI({
    if(any(RemixAutoML:::AvailableAppInsightsPlots(x = NULL, PlotNamesLookup = PlotNamesLookup, Debug = Debug) %in% tryCatch({input[['Plot2']]}, error = function(x) NULL))) {
      ModelVars <- names(data)
    } else {
      ModelVars <- names(ModelData)
    }
    RemixAutoML::SelectizeInput(InputID = 'FilterVariable_2_1', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Filter Variable 1'), Choices = c(ModelVars), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
  })
  output$FilterVariable_2_2 <- shiny::renderUI({
    if(any(RemixAutoML:::AvailableAppInsightsPlots(x = NULL, PlotNamesLookup = PlotNamesLookup, Debug = Debug) %in% tryCatch({input[['Plot2']]}, error = function(x) NULL))) {
      ModelVars <- names(data)
    } else {
      ModelVars <- names(ModelData)
    }
    RemixAutoML::SelectizeInput(InputID = 'FilterVariable_2_2', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Filter Variable 2'), Choices = c(ModelVars), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
  })
  output$FilterVariable_2_3 <- shiny::renderUI({
    if(any(RemixAutoML:::AvailableAppInsightsPlots(x = NULL, PlotNamesLookup = PlotNamesLookup, Debug = Debug) %in% tryCatch({input[['Plot2']]}, error = function(x) NULL))) {
      ModelVars <- names(data)
    } else {
      ModelVars <- names(ModelData)
    }
    RemixAutoML::SelectizeInput(InputID = 'FilterVariable_2_3', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Filter Variable 3'), Choices = c(ModelVars), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
  })
  output$FilterVariable_2_4 <- shiny::renderUI({
    if(any(RemixAutoML:::AvailableAppInsightsPlots(x = NULL, PlotNamesLookup = PlotNamesLookup, Debug = Debug) %in% tryCatch({input[['Plot2']]}, error = function(x) NULL))) {
      ModelVars <- names(data)
    } else {
      ModelVars <- names(ModelData)
    }
    RemixAutoML::SelectizeInput(InputID = 'FilterVariable_2_4', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Filter Variable 4'), Choices = c(ModelVars), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
  })
  output$FilterVariable_3_1 <- shiny::renderUI({
    if(any(RemixAutoML:::AvailableAppInsightsPlots(x = NULL, PlotNamesLookup = PlotNamesLookup, Debug = Debug) %in% tryCatch({input[['Plot3']]}, error = function(x) NULL))) {
      ModelVars <- names(data)
    } else {
      ModelVars <- names(ModelData)
    }
    RemixAutoML::SelectizeInput(InputID = 'FilterVariable_3_1', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Filter Variable 1'), Choices = c(ModelVars), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
  })
  output$FilterVariable_3_2 <- shiny::renderUI({
    if(any(RemixAutoML:::AvailableAppInsightsPlots(x = NULL, PlotNamesLookup = PlotNamesLookup, Debug = Debug) %in% tryCatch({input[['Plot3']]}, error = function(x) NULL))) {
      ModelVars <- names(data)
    } else {
      ModelVars <- names(ModelData)
    }
    RemixAutoML::SelectizeInput(InputID = 'FilterVariable_3_2', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Filter Variable 2'), Choices = c(ModelVars), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
  })
  output$FilterVariable_3_3 <- shiny::renderUI({
    if(any(RemixAutoML:::AvailableAppInsightsPlots(x = NULL, PlotNamesLookup = PlotNamesLookup, Debug = Debug) %in% tryCatch({input[['Plot3']]}, error = function(x) NULL))) {
      ModelVars <- names(data)
    } else {
      ModelVars <- names(ModelData)
    }
    RemixAutoML::SelectizeInput(InputID = 'FilterVariable_3_3', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Filter Variable 3'), Choices = c(ModelVars), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
  })
  output$FilterVariable_3_4 <- shiny::renderUI({
    if(any(RemixAutoML:::AvailableAppInsightsPlots(x = NULL, PlotNamesLookup = PlotNamesLookup, Debug = Debug) %in% tryCatch({input[['Plot3']]}, error = function(x) NULL))) {
      ModelVars <- names(data)
    } else {
      ModelVars <- names(ModelData)
    }
    RemixAutoML::SelectizeInput(InputID = 'FilterVariable_3_4', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Filter Variable 4'), Choices = c(ModelVars), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
  })
  output$FilterVariable_4_1 <- shiny::renderUI({
    if(any(RemixAutoML:::AvailableAppInsightsPlots(x = NULL, PlotNamesLookup = PlotNamesLookup, Debug = Debug) %in% tryCatch({input[['Plot4']]}, error = function(x) NULL))) {
      ModelVars <- names(data)
    } else {
      ModelVars <- names(ModelData)
    }
    RemixAutoML::SelectizeInput(InputID = 'FilterVariable_4_1', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Filter Variable 1'), Choices = c(ModelVars), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
  })
  output$FilterVariable_4_2 <- shiny::renderUI({
    if(any(RemixAutoML:::AvailableAppInsightsPlots(x = NULL, PlotNamesLookup = PlotNamesLookup, Debug = Debug) %in% tryCatch({input[['Plot4']]}, error = function(x) NULL))) {
      ModelVars <- names(data)
    } else {
      ModelVars <- names(ModelData)
    }
    RemixAutoML::SelectizeInput(InputID = 'FilterVariable_4_2', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Filter Variable 2'), Choices = c(ModelVars), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
  })
  output$FilterVariable_4_3 <- shiny::renderUI({
    if(any(RemixAutoML:::AvailableAppInsightsPlots(x = NULL, PlotNamesLookup = PlotNamesLookup, Debug = Debug) %in% tryCatch({input[['Plot4']]}, error = function(x) NULL))) {
      ModelVars <- names(data)
    } else {
      ModelVars <- names(ModelData)
    }
    RemixAutoML::SelectizeInput(InputID = 'FilterVariable_4_3', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Filter Variable 3'), Choices = c(ModelVars), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
  })
  output$FilterVariable_4_4 <- shiny::renderUI({
    if(any(RemixAutoML:::AvailableAppInsightsPlots(x = NULL, PlotNamesLookup = PlotNamesLookup, Debug = Debug) %in% tryCatch({input[['Plot4']]}, error = function(x) NULL))) {
      ModelVars <- names(data)
    } else {
      ModelVars <- names(ModelData)
    }
    RemixAutoML::SelectizeInput(InputID = 'FilterVariable_4_4', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Filter Variable 4'), Choices = c(ModelVars), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
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

  # Filter Logics
  output$FilterLogic_1_1 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterLogic_1_1', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_1_1()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
  })
  output$FilterLogic_1_2 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterLogic_1_2', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_1_2()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
  })
  output$FilterLogic_1_3 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterLogic_1_3', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_1_3()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
  })
  output$FilterLogic_1_4 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterLogic_1_4', selected=RemixAutoML:::FL_Default(data, x=tryCatch({FilterVariable_1_4()}, error = function(x) NULL)), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
  })
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

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Filter Values     DONT OVERWRITE     ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

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
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_1_1_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
  })
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
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_1_1_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
  })
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
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_1_2_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
  })
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
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_1_2_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
  })
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
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_1_3_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
  })
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
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_1_3_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
  })
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
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_1_4_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
  })
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
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_1_4_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
  })

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
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_2_1_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
  })
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
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_2_1_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
  })
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
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_2_2_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
  })
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
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_2_2_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
  })
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
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_2_3_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
  })
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
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_2_3_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
  })
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
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_2_4_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
  })
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
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_2_4_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
  })

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
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_3_1_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
  })
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
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_3_1_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
  })
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
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_3_2_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
  })
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
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_3_2_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
  })
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
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_3_3_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
  })
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
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_3_3_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
  })
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
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_3_4_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
  })
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
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_3_4_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
  })

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
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_4_1_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
  })
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
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_4_1_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
  })
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
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_4_2_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
  })
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
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_4_2_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
  })
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
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_4_3_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
  })
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
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_4_3_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
  })
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
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_4_4_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
  })
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
    RemixAutoML::SelectizeInput(Multiple = FALSE, InputID='FilterValue_4_4_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
  })

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Initialize Plot                      ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  output$Trend <- renderPlot({
    if(!exists('PlotCollectionList')) RemixAutoML:::BlankPlot()
  })

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
        shiny::HTML(paste0(unlist(CodeCollection), sep = '<br/>'))
      })
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = 'No Code Collected, Yet', type = NULL, btn_labels = "warning", btn_colors = "yellow", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  })

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Create Plot                          ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  shiny::observeEvent(eventExpr = input[['TrendPlotExecute']], {

    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    # Determine Which Plots to Build       ----
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    PlotCollectionList <- list()
    CodeCollection <- list()

    # Identify which plots to build
    NumPlots <- c()
    Plot1 <- RemixAutoML::ReturnParam(xx=tryCatch({input[['Plot1']]}, error=function(x) NULL), Type='character', Default=NULL)
    Plot2 <- RemixAutoML::ReturnParam(xx=tryCatch({input[['Plot2']]}, error=function(x) NULL), Type='character', Default=NULL)
    Plot3 <- RemixAutoML::ReturnParam(xx=tryCatch({input[['Plot3']]}, error=function(x) NULL), Type='character', Default=NULL)
    Plot4 <- RemixAutoML::ReturnParam(xx=tryCatch({input[['Plot4']]}, error=function(x) NULL), Type='character', Default=NULL)
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
    PlotObjectHome[['GlobalSettings']][['PlotWidth']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[['PlotWidth']]}, error=function(x) NULL), Type='numeric', Default=1550L)
    PlotObjectHome[['GlobalSettings']][['PlotHeight']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[['PlotHeight']]}, error=function(x) NULL), Type='numeric', Default=500L)

    # Initialize PlotObjectHome List
    for(run in PlotRefs) {
      PlotObjectHome[[paste0('Plot_', run)]][['DataSource']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('DataSource', run)]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['PlotType']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('Plot', run)]]}, error=function(x) NULL), Type='character', Default=NULL, Debug = TRUE)
      PlotObjectHome[[paste0('Plot_', run)]][['UpdateMethod']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('UpdateMethod', run)]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['SampleSize']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('SampleSize', run)]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['NumberGroupsDisplay']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('NumberGroupsDisplay', run)]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['YVar']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('YVar', run)]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['YTicks']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('YTicks', run)]]}, error=function(x) NULL), Type='character', Default='Default')
      PlotObjectHome[[paste0('Plot_', run)]][['XVar']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('XVar', run)]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['XTicks']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('XTicks', run)]]}, error=function(x) NULL), Type='character', Default='Default')
      PlotObjectHome[[paste0('Plot_', run)]][['CorMethod']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('CorMethod', run)]]}, error=function(x) NULL), Type='character', Default='pearson')
      PlotObjectHome[[paste0('Plot_', run)]][['ScoreVar']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('ScoreVar', run)]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['GroupVars']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('GroupVars', run)]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['Levels1']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('Levels_',run,'_1')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['Levels2']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('Levels_',run,'_1')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['Levels3']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('Levels_',run,'_1')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['SizeVars']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('SizeVars', run)]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FacetVar1']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('FacetVar1', run)]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FacetVar2']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('FacetVar2', run)]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FilterVar1']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('FilterVariable_',run, '_1')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FilterVar2']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('FilterVariable_',run, '_2')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FilterVar3']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('FilterVariable_',run, '_3')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FilterVar4']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('FilterVariable_',run, '_4')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FilterLogic1']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('FilterLogic_',run, '_1')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FilterLogic2']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('FilterLogic_',run, '_2')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FilterLogic3']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('FilterLogic_',run, '_3')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FilterLogic4']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('FilterLogic_',run, '_4')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FilterValue_1_1']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',run, '_1_1')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FilterValue_1_2']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',run, '_1_2')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FilterValue_1_3']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',run, '_2_1')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FilterValue_1_4']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',run, '_2_2')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FilterValue_2_1']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',run, '_3_1')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FilterValue_2_2']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',run, '_3_2')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FilterValue_2_3']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',run, '_4_1')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['FilterValue_2_4']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',run, '_4_2')]]}, error=function(x) NULL), Type='character', Default=NULL)
      PlotObjectHome[[paste0('Plot_', run)]][['AngleY']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('AngleY', run)]]}, error=function(x) NULL), Type='numeric', Default=0L)
      PlotObjectHome[[paste0('Plot_', run)]][['AngleX']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('AngleX', run)]]}, error=function(x) NULL), Type='numeric', Default=90L)
      PlotObjectHome[[paste0('Plot_', run)]][['TextSize']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('TextSize', run)]]}, error=function(x) NULL), Type='numeric', Default=15L)
      PlotObjectHome[[paste0('Plot_', run)]][['OutlierSize']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('OutlierSize', run)]]}, error=function(x) NULL), Type='numeric', Default=0.01)
      PlotObjectHome[[paste0('Plot_', run)]][['LegendPosition']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('LegendPosition', run)]]}, error=function(x) NULL), Type='character', Default='right')
      PlotObjectHome[[paste0('Plot_', run)]][['LegendBorderSize']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('LegendBorderSize', run)]]}, error=function(x) NULL), Type='numeric', Default=0.01)
      PlotObjectHome[[paste0('Plot_', run)]][['LegendLineType']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('LegendLineType', run)]]}, error=function(x) NULL), Type='character', Default='solid')
      PlotObjectHome[[paste0('Plot_', run)]][['TextColor']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('TextColor', run)]]}, error=function(x) NULL), Type='character', Default='darkblue')
      PlotObjectHome[[paste0('Plot_', run)]][['ChartColor']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('ChartColor', run)]]}, error=function(x) NULL), Type='character', Default='aliceblue')
      PlotObjectHome[[paste0('Plot_', run)]][['GridColor']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('GridColor', run)]]}, error=function(x) NULL), Type='character', Default='lightsteelblue1')
      PlotObjectHome[[paste0('Plot_', run)]][['BackGroundColor']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('BackGroundColor', run)]]}, error=function(x) NULL), Type='character', Default='gray95')
      PlotObjectHome[[paste0('Plot_', run)]][['BorderColor']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('BorderColor', run)]]}, error=function(x) NULL), Type='character', Default='lightsteelblue4')
      PlotObjectHome[[paste0('Plot_', run)]][['OutlierColor']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('OutlierColor', run)]]}, error=function(x) NULL), Type='character', Default='blue')
      PlotObjectHome[[paste0('Plot_', run)]][['FillColor']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('FillColor', run)]]}, error=function(x) NULL), Type='character', Default='gray70')
      PlotObjectHome[[paste0('Plot_', run)]][['SubTitleColor']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('SubTitleColor', run)]]}, error=function(x) NULL), Type='character', Default='blue')
      PlotObjectHome[[paste0('Plot_', run)]][['ShapAgg']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('ShapAggMethod', run)]]}, error=function(x) NULL), Type='character', Default='meanabs')
      PlotObjectHome[[paste0('Plot_', run)]][['GamFitScatter']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('GamFitScatter', run)]]}, error=function(x) NULL), Type='logical', Default=FALSE)
      PlotObjectHome[[paste0('Plot_', run)]][['NumberBins']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('NumberBins', run)]]}, error=function(x) NULL), Type='numeric', Default=30L)
      PlotObjectHome[[paste0('Plot_', run)]][['Percentile_Buckets']] <- RemixAutoML::ReturnParam(xx=tryCatch({input[[paste0('Percentile_Buckets', run)]]}, error=function(x) NULL), Type='numeric', Default=20L)

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
        PlotWidth <- PlotObjectHome[['GlobalSettings']][['PlotWidth']]
        PlotHeight <- PlotObjectHome[['GlobalSettings']][['PlotHeight']]
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
      }

      # ----

      # ----

      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
      # Logic Check to Build Plots           ----
      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

      # Convert back to original plottype name
      PlotType <- PlotNamesLookup[[eval(PlotType)]]

      # For PDP's
      if(PlotType %in% names(ModelOutputList$PlotList)) {
        if('p1' %in% names(ModelData)) {
          ScoreVar <- 'p1'
        } else {
          ScoreVar <- 'Predict'
        }
      } else {
        ScoreVar <- NULL
      }

      # PLOT LOGIC CHECK:
      if(length(YVar) == 0 && length(XVar) == 0 && PlotType %in% c('Scatter','Copula','Line','Bar','BoxPlot','ViolinPlot','Histogram')) {
        shinyWidgets::sendSweetAlert(session, title = NULL, text = 'You need to specify additional variables to generate additional plots', type = NULL, btn_labels = "error", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
      } else if(PlotType %in% c('Scatter','Copula') && !any(class(data[[eval(XVar)]]) %in% c('numeric','integer')) && !any(class(data[[eval(YVar)]]) %in% c('numeric','integer'))) {
        shinyWidgets::sendSweetAlert(session, title = NULL, text = "Y and / or X-Variable needs to be a numeric or integer variable", type = NULL, btn_labels = "error", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
      } else if(PlotType %in% 'CorrMatrix' && length(YVar) == 0) {
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
            SubsetList[[paste0('GroupVars', run)]] <- RemixAutoML::ReturnParam(xx = input[[paste0('GroupVars', run)]], VarName=paste0('GroupVars', run), Type='character', Default='None', Switch=TRUE)
            SubsetList[[paste0('DataPrep', run)]] <- TRUE
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
          if(!all(SubsetList[[paste0('FilterVariable_', run, '_1')]] == FilterVar1)) {
            SubsetList[[paste0('FilterVariable_', run, '_1')]] <- FilterVar1; SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterVariable_', run, '_2')]] == FilterVar2)) {
            SubsetList[[paste0('FilterVariable_', run, '_2')]] <- FilterVar2; SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterVariable_', run, '_3')]] == FilterVar3)) {
            SubsetList[[paste0('FilterVariable_', run, '_3')]] <- FilterVar3; SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterVariable_', run, '_4')]] == FilterVar4)) {
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
          if(!all(SubsetList[[paste0('FilterValue_', run, '_2_1')]] == FilterValue_1_3)) {
            SubsetList[[paste0('FilterValue_', run, '_2_1')]] <- FilterValue_1_3; SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterValue_', run, '_2_2')]] == FilterValue_1_4)) {
            SubsetList[[paste0('FilterValue_', run, '_2_2')]] <- FilterValue_1_4; SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterValue_', run, '_3_1')]] == FilterValue_2_1)) {
            SubsetList[[paste0('FilterValue_', run, '_3_1')]] <- FilterValue_2_1; SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterValue_', run, '_3_2')]] == FilterValue_2_2)) {
            SubsetList[[paste0('FilterValue_', run, '_3_2')]] <- FilterValue_2_2; SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterValue_', run, '_4_1')]] == FilterValue_2_3)) {
            SubsetList[[paste0('FilterValue_', run, '_4_1')]] <- FilterValue_2_3; SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterValue_', run, '_4_2')]] == FilterValue_2_4)) {
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

        # Define data1
        if(exists('data') && (exists('YVar') && length(YVar) != 0 && YVar %in% names(data)) || (exists('XVar') && length(XVar) != 0 && XVar %in% names(data))) {
          data1 <- data.table::copy(data)
        } else if(exists('ModelData') && (exists('YVar') && length(YVar) != 0 && YVar %in% names(ModelData)) || (exists('XVar') && length(XVar) != 0 && XVar %in% names(ModelData))) {
          data1 <- data.table::copy(ModelData)
        } else if(length(YVar) == 0 && length(XVar) == 0 && !PlotType %in% c('BoxPlot','ViolinPlot','Line','Bar','Scatter','Copula','CorrMatrix','Histogram')) {
          if(!is.null(ModelData)) {
            data1 <- data.table::copy(ModelData)
          } else {
            data1 <- NULL
          }
        } else {
          if(!is.null(data)) {
            data1 <- data.table::copy(data)
          } else {
            data1 <- NULL
          }
        }

        # Filter Data if DataPrep = TRUE
        if(SubsetList[[paste0('DataPrep', run)]]) {

          # Subset by FilterVariable
          if(Debug) print('Here 23')
          for(i in seq_len(4L)) {
            if(length(eval(parse(text = paste0('FilterVar', i)))) != 0L) {
              data1 <- RemixAutoML::FilterLogicData(
                data1,
                FilterLogic    = get(paste0('FilterLogic', i)),
                FilterVariable = get(paste0('FilterVar', i)), # Replaces ModelVar- with "" and returns normally if it isn't there
                FilterValue    = get(paste0('FilterValue_',i,'_1')),
                FilterValue2   = get(paste0('FilterValue_',i,'_2')),
                Debug          = Debug)
              CodeCollection[[run]][[length(CodeCollection[[run]])+1L]] <- paste0("data1 <- RemixAutoML::FilterLogicData(data1, FilterLogic=", RemixAutoML:::CEP(get(paste0('FilterLogic',i))),", FilterVariable=", RemixAutoML:::CEP(get(paste0('FilterVar',i))),", FilterValue=", RemixAutoML:::CEP(get(paste0('FilterValue_',i,'_1'))),", FilterValue2=", RemixAutoML:::CEP(get(paste0('FilterValue_',i,'_2'))),"))")
            }
          }

          # Subset Rows and Columns
          if(PlotType == 'Line' && length(XVar) != 0) {
            data1 <- RemixAutoML::PreparePlotData(
              SubsetOnly = FALSE, #if(PlotType %in% c('BoxPlot','ViolinPlot','CorrMatrix','Scatter','Copula','Histogram','Train_ParDepPlots','Test_ParDepPlots','Train_ParDepBoxPlots','Test_ParDepBoxPlots','Test__EvaluationPlot','Train_EvaluationPlot','Test_EvaluationBoxPlot','Train_EvaluationBoxPlot')) TRUE else FALSE,
              data = data1, Aggregate = 'mean', TargetVariable = YVar, DateVariable = XVar,
              GroupVariables = GroupVars,
              G1Levels = Levels1, G2Levels = Levels2, G3Levels = Levels3)
            CodeCollection[[run]][[length(CodeCollection[[run]])+1L]] <- paste0("data1 <- RemixAutoML::PreparePlotData(SubsetOnly = ", FALSE,", data=data1, Aggregate='mean', TargetVariable=", RemixAutoML:::CEP(YVar),", DateVariable=", RemixAutoML:::CEP(XVar), ", GroupVariables=", RemixAutoML:::CEP(GroupVars),", G1Levels=", RemixAutoML:::CEP(Levels1),", G2Levels=", RemixAutoML:::CEP(Levels2),", G3Levels=", RemixAutoML:::CEP(Levels3),")")

          } else {

            # Debugging
            if(Debug) {print(YVar); print(XVar); print(ScoreVar); print(GroupVars); print(SizeVars); print(FacetVar1); print(FacetVar2)}

            # Subset columns
            if(Debug) print('Subset Columns Here')
            if(!PlotType %in% c('BoxPlot','ViolinPlot','Line','Bar','Scatter','Copula','CorrMatrix','Histogram','ShapleyVarImp')) {
              if(length(unique(c(XVar))) != 0) {
                Keep <- unique(c(YVar, XVar, ScoreVar)); if(Debug) {print(Keep); print(names(data1))}
                data1 <- data1[, .SD, .SDcols = c(Keep)]; if(Debug) print('Subset Columns Here predone')
                CodeCollection[[run]][[length(CodeCollection)+1L]] <- paste0("data1 <- data1[, .SD, .SDcols = c(",RemixAutoML:::ExpandText(Keep),")]"); if(Debug) print('Subset Columns Here done')
              }
            } else if(!PlotType %in% 'ShapleyVarImp') {
              if(length(unique(c(YVar, XVar, GroupVars, SizeVars, FacetVar1, FacetVar2))) != 0) {
                Keep <- unique(c(YVar, XVar, GroupVars, SizeVars, FacetVar1, FacetVar2))
                data1 <- data1[, .SD, .SDcols = c(Keep)]; if(Debug) print('Subset Columns Here predone')
                CodeCollection[[run]][[length(CodeCollection)+1L]] <- paste0("data1 <- data1[, .SD, .SDcols = c(",RemixAutoML:::ExpandText(Keep),")]"); if(Debug) print('Subset Columns Here done')
              }
            }
          }
        }

        # ----

        # ----

        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
        # Rebuild Logic                        ----
        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

        if(Debug) print('Here 27')

        # Logic Check for rebuilding modeling plots
        x2 <- if(length(data1) != 0) any(names(data1) %in% XVar) else FALSE; if(Debug) print(if(length(data1) != 0) {print(data1); any(names(data1) %in% XVar)} else FALSE)
        x3 <- Percentile_Buckets != 20; if(Debug) print(Percentile_Buckets != 20)
        x4 <- length(GroupVars) != 0 && (length(Levels1) != 0 || length(Levels2) != 0 || length(Levels3) != 0); if(Debug) print(length(GroupVars) != 0 && (length(Levels1) != 0 || length(Levels2) != 0 || length(Levels3) != 0))
        x5 <- any(c('Test_ParDepPlots','Train_ParDepPlots','Test_ParDepBoxPlots','Train_ParDepBoxPlots','Test_EvaluationPlot','Train_EvaluationPlot','Test_EvaluationBoxPlot','Train_EvaluationBoxPlot','Test_GainsPlot','Train_GainsPlot','Test_LiftPlot','Train_LiftPlot','Test_ScatterPlot','Train_ScatterPlot','Test_CopulaPlot','Train_CopulaPlot','Test_ResidualsHistogram','Train_ResidualsHistogram') %in% PlotType); if(Debug) print(any(c('Test_ParDepPlots','Train_ParDepPlots','Test_ParDepBoxPlots','Train_ParDepBoxPlots','Test_EvaluationPlot','Train_EvaluationPlot','Test_EvaluationBoxPlot','Train_EvaluationBoxPlot','Test_GainsPlot','Train_GainsPlot','Test_LiftPlot','Train_LiftPlot','Test_ScatterPlot','Train_ScatterPlot','Test_CopulaPlot','Train_CopulaPlot','Test_ResidualsHistogram','Train_ResidualsHistogram') %in% PlotType))

        # if xvar is not in data1, cannot rebuild
        #        && PlotType %in% c('Test_ParDepPlots','Train_ParDepPlots','Test_ParDepBoxPlots','Train_ParDepBoxPlots')
        PDP_Blocker <- !x2; if(Debug) print(!x2 && PlotType %in% c('Test_ParDepPlots','Train_ParDepPlots','Test_ParDepBoxPlots','Train_ParDepBoxPlots'))
        if(x5 || x4 || (x3 && PlotType %in% c('Test_ParDepPlots','Train_ParDepPlots','Test_ParDepBoxPlots','Train_ParDepBoxPlots'))) {
          if(PDP_Blocker) Rebuild <- FALSE else Rebuild <- TRUE
        } else {
          Rebuild <- FALSE
        }

        # ----

        # ----

        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
        # Define Plots Variables               ----
        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

        if(Debug) print('Here 28')

        # XVar: If XVar is NULL go to next iteration in Plot Loop
        if(PlotType == 'Line') {
          if(Debug) print('Checking XVar for PlotType == :: Line ::')
          if(length(XVar) == 0) next
        }

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
            print(paste0('ColorVariables = ', RemixAutoML:::CEP(GroupVars[[1L]])))
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
            ", ColorVariables=", RemixAutoML:::CEP(GroupVars[[1L]]),
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

          # Build Plot
          PlotCollectionList[[paste0('p', run)]] <- RemixAutoML:::AppModelInsights(
            ModelOutputList,
            dt = data1,
            PlotType = PlotType,
            TargetVar = YVar,
            PredictVar = ScoreVar,
            PDPVar = XVar,
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
            print(class(PlotCollectionList[[paste0('p', run)]]))
            print(YVar)
            print(names(ModelOutputList$PlotList$Test_ParDepPlots)[1L])
            print('AppModelInsights finished building. Code Collection next')
            print(length(YVar))
            print(RemixAutoML:::CEP(YVar))
            print(paste0("TargetVar=", if(length(YVar) != 0) RemixAutoML:::CEP(YVar) else 'NULL'))
            print(paste0("PredictVar=", if(length(ScoreVar) != 0) RemixAutoML:::CEP(ScoreVar) else 'NULL'))
            print(paste0("PDPVar=", XVar))
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
            ", PDPVar=", RemixAutoML:::CEP(XVar),
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
              LegendPosition = if(PlotType %in% c('ShapleyVarImp','Train_VariableImportance','Test_VariableImportance','Validation_VariableImportance','Test_GainsPlot','Train_GainsPlot')) 'none' else LegendPosition,
              LegendBorderSize = if(PlotType %in% c('ShapleyVarImp','Train_VariableImportance','Test_VariableImportance','Validation_VariableImportance','Test_GainsPlot','Train_GainsPlot')) NULL else as.numeric(LegendBorderSize),
              LegendLineType = if(PlotType %in% c('ShapleyVarImp','Train_VariableImportance','Test_VariableImportance','Validation_VariableImportance','Test_GainsPlot','Train_GainsPlot')) NULL else LegendLineType)

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
              ", LegendPosition=", RemixAutoML:::CEP(if(PlotType %in% c('ShapleyVarImp','Train_VariableImportance','Test_VariableImportance','Validation_VariableImportance','Test_GainsPlot','Train_GainsPlot')) 'none' else LegendPosition),
              ", LegendBorderSize=", RemixAutoML:::CEP(if(PlotType %in% c('ShapleyVarImp','Train_VariableImportance','Test_VariableImportance','Validation_VariableImportance','Test_GainsPlot','Train_GainsPlot')) NULL else as.numeric(LegendBorderSize)),
              ", LegendLineType=", RemixAutoML:::CEP(if(PlotType %in% c('ShapleyVarImp','Train_VariableImportance','Test_VariableImportance','Validation_VariableImportance','Test_GainsPlot','Train_GainsPlot')) NULL else LegendBorderSize), ")")
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
            AutoGridHorizontal <- RemixAutoML::ReturnParam(xx=tryCatch({input[['AutoGridHorizontal']]}, error=function(x) FALSE), VarName=NULL, Type='logical', Default = TRUE)
            if(is.null(AutoGridHorizontal)) AutoGridHorizontal <- TRUE
            CodeCollection[[length(CodeCollection)+1L]] <- 'gridExtra::grid.arrange(gridExtra::arrangeGrob(grobs = PlotCollectionList, as.table = FALSE))'

            # Number of plots
            N <- length(PlotCollectionList)
            if(Debug) print(paste0('Length of N = ', N))

            # Build Plots
            if(N == 1L) {
              p1 <- PlotCollectionList[[paste0('p', PlotRefs[1L])]]
              p1 <- RemixAutoML::PlotLimits(
                p1,
                YMin=tryCatch({input[[paste0('YLimMin',PlotRefs[1L])]]}, error = function(x) ""),
                YMax=tryCatch({input[[paste0('YLimMax',PlotRefs[1L])]]}, error = function(x) ""),
                XMin=tryCatch({input[[paste0('XLimMin',PlotRefs[1L])]]}, error = function(x) ""),
                XMax=tryCatch({input[[paste0('XLimMax',PlotRefs[1L])]]}, error = function(x) ""),
                Debug = Debug)

              # Ouput Plot for 1 single plot request
              output$Trend <- shiny::renderPlot(width = PlotWidth, height = PlotHeight, {
                gridExtra::grid.arrange(p1, ncol=1)
              })

            } else if(N == 2L) {
              p1 <- PlotCollectionList[[paste0('p', PlotRefs[1L])]]
              p2 <- PlotCollectionList[[paste0('p', PlotRefs[2L])]]

              # Update axis limits
              p1 <- RemixAutoML::PlotLimits(
                p1,
                YMin=tryCatch({input[[paste0('YLimMin',PlotRefs[1L])]]}, error = function(x) ""),
                YMax=tryCatch({input[[paste0('YLimMax',PlotRefs[1L])]]}, error = function(x) ""),
                XMin=tryCatch({input[[paste0('XLimMin',PlotRefs[1L])]]}, error = function(x) ""),
                XMax=tryCatch({input[[paste0('XLimMax',PlotRefs[1L])]]}, error = function(x) ""),
                Debug = Debug)
              p2 <- RemixAutoML::PlotLimits(
                p2,
                YMin=tryCatch({input[[paste0('YLimMin',PlotRefs[2L])]]}, error = function(x) ""),
                YMax=tryCatch({input[[paste0('YLimMax',PlotRefs[2L])]]}, error = function(x) ""),
                XMin=tryCatch({input[[paste0('XLimMin',PlotRefs[2L])]]}, error = function(x) ""),
                XMax=tryCatch({input[[paste0('XLimMax',PlotRefs[2L])]]}, error = function(x) ""),
                Debug = Debug)

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
              if(Debug) print('Just ran output$Trend for N == 2L')

            } else if(N == 3L) {
              p1 <- PlotCollectionList[[paste0('p', PlotRefs[1L])]]
              p2 <- PlotCollectionList[[paste0('p', PlotRefs[2L])]]
              p3 <- PlotCollectionList[[paste0('p', PlotRefs[3L])]]

              # Update axis limits
              p1 <- RemixAutoML::PlotLimits(
                p1,
                YMin=tryCatch({input[[paste0('YLimMin',PlotRefs[1L])]]}, error = function(x) ""),
                YMax=tryCatch({input[[paste0('YLimMax',PlotRefs[1L])]]}, error = function(x) ""),
                XMin=tryCatch({input[[paste0('XLimMin',PlotRefs[1L])]]}, error = function(x) ""),
                XMax=tryCatch({input[[paste0('XLimMax',PlotRefs[1L])]]}, error = function(x) ""),
                Debug = Debug)
              p2 <- RemixAutoML::PlotLimits(
                p2,
                YMin=tryCatch({input[[paste0('YLimMin',PlotRefs[2L])]]}, error = function(x) ""),
                YMax=tryCatch({input[[paste0('YLimMax',PlotRefs[2L])]]}, error = function(x) ""),
                XMin=tryCatch({input[[paste0('XLimMin',PlotRefs[2L])]]}, error = function(x) ""),
                XMax=tryCatch({input[[paste0('XLimMax',PlotRefs[2L])]]}, error = function(x) ""),
                Debug = Debug)
              p3 <- RemixAutoML::PlotLimits(
                p3,
                YMin=tryCatch({input[[paste0('YLimMin',PlotRefs[3L])]]}, error = function(x) ""),
                YMax=tryCatch({input[[paste0('YLimMax',PlotRefs[3L])]]}, error = function(x) ""),
                XMin=tryCatch({input[[paste0('XLimMin',PlotRefs[3L])]]}, error = function(x) ""),
                XMax=tryCatch({input[[paste0('XLimMax',PlotRefs[3L])]]}, error = function(x) ""),
                Debug = Debug)

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
              p1 <- PlotCollectionList[[paste0('p', PlotRefs[1L])]]
              p2 <- PlotCollectionList[[paste0('p', PlotRefs[2L])]]
              p3 <- PlotCollectionList[[paste0('p', PlotRefs[3L])]]
              p4 <- PlotCollectionList[[paste0('p', PlotRefs[4L])]]

              # Update axis limits
              p1 <- RemixAutoML::PlotLimits(
                p1,
                YMin=tryCatch({input[[paste0('YLimMin',PlotRefs[1L])]]}, error = function(x) ""),
                YMax=tryCatch({input[[paste0('YLimMax',PlotRefs[1L])]]}, error = function(x) ""),
                XMin=tryCatch({input[[paste0('XLimMin',PlotRefs[1L])]]}, error = function(x) ""),
                XMax=tryCatch({input[[paste0('XLimMax',PlotRefs[1L])]]}, error = function(x) ""),
                Debug = Debug)
              p2 <- RemixAutoML::PlotLimits(
                p2,
                YMin=tryCatch({input[[paste0('YLimMin',PlotRefs[2L])]]}, error = function(x) ""),
                YMax=tryCatch({input[[paste0('YLimMax',PlotRefs[2L])]]}, error = function(x) ""),
                XMin=tryCatch({input[[paste0('XLimMin',PlotRefs[2L])]]}, error = function(x) ""),
                XMax=tryCatch({input[[paste0('XLimMax',PlotRefs[2L])]]}, error = function(x) ""),
                Debug = Debug)
              p3 <- RemixAutoML::PlotLimits(
                p3,
                YMin=tryCatch({input[[paste0('YLimMin',PlotRefs[3L])]]}, error = function(x) ""),
                YMax=tryCatch({input[[paste0('YLimMax',PlotRefs[3L])]]}, error = function(x) ""),
                XMin=tryCatch({input[[paste0('XLimMin',PlotRefs[3L])]]}, error = function(x) ""),
                XMax=tryCatch({input[[paste0('XLimMax',PlotRefs[3L])]]}, error = function(x) ""),
                Debug = Debug)
              p4 <- RemixAutoML::PlotLimits(
                p4,
                YMin=tryCatch({input[[paste0('YLimMin',PlotRefs[4L])]]}, error = function(x) ""),
                YMax=tryCatch({input[[paste0('YLimMax',PlotRefs[4L])]]}, error = function(x) ""),
                XMin=tryCatch({input[[paste0('XLimMin',PlotRefs[4L])]]}, error = function(x) ""),
                XMax=tryCatch({input[[paste0('XLimMax',PlotRefs[4L])]]}, error = function(x) ""),
                Debug = Debug)

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

            # Empty plot for errors
            output$Trend <- shiny::renderPlot({ # width = PlotWidth, height = PlotHeight,
              RemixAutoML:::BlankPlot()
            })

            # Send Error Message
            shinyWidgets::sendSweetAlert(session, title = NULL, text = 'Plot could not build. Check for missing variables, such as Date Variables.', type = NULL, btn_labels = "error", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
          }

        } else {

          # Empty plot for errors
          output$Trend <- shiny::renderPlot({ # width = PlotWidth, height = PlotHeight,
            RemixAutoML:::BlankPlot()
          })

        } # end Plot Build
      }
    }

    # Output blank grapth is list is empty
    if(length(PlotCollectionList) == 0) {
      output$Trend <- shiny::renderPlot({
        RemixAutoML:::BlankPlot()
      })
    }
  })

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Close app after closing browser      ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  session$onSessionEnded(function() {
    rm(BlobStorageURL, PlotObjectHome, CodeCollection, data1, PlotCollectionList, SubsetList, rawfiles, cont, ModelData, ModelOutputList, envir = .GlobalEnv)
    shiny::stopApp()
  })
}

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# Run App                              ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
shiny::shinyApp(ui = ui, server = server)


# TODO: possibly allow labeling updating
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
