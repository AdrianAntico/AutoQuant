# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# Environment Setup                    ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
library(data.table)
data.table::setDTthreads(threads = max(1L, parallel::detectCores()-1L))
options(shiny.maxRequestSize = 300000*1024^2)
options(scipen = 999)

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# Passthrough Args                     ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

# data related
data <- shiny::getShinyOption('data')
XVariable <- shiny::getShinyOption('XVariable')
YVariable <- shiny::getShinyOption('YVariable')
DateName <- shiny::getShinyOption('DateName')
GroupVariables <- shiny::getShinyOption('GroupVariables')
FilterVariable <- shiny::getShinyOption('FilterVariable')
ModelOutputList <- shiny::getShinyOption('ModelOutputList')

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
AppTextColor <- shiny::getShinyOption('AppTextColor')
Debug <- shiny::getShinyOption('Debug')

# Usernames and Passwords
UserName_Password_DT <- shiny::getShinyOption('UserName_Password_DT')
if(!is.null(UserName_Password_DT)) {
  if('UserName' %in% names(UserName_Password_DT) && 'Password' %in% names(UserName_Password_DT)) {
    Credentials <- UserName_Password_DT
  } else {
    Credentials <- data.table::data.table(
      UserName = c('Guest'),
      Password = c('Password'))
  }
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
      shinydashboard::menuItem(text="Create Plots", tabName='Plotter', icon=shiny::icon("fort-awesome")))),

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

    # ----

    # ----

    shinydashboard::tabItems(

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
              background = GroupVarsBoxColor, width = 9L,
              shiny::selectInput(
                inputId = "UserName",
                label =  "Select from Names",
                choices = Credentials[['UserName']]),
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
              color = eval(CreatePlotButtonColor))))),

        # ----

        # ----

      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
      # Load Data Page                       ----
      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
      shinydashboard::tabItem(
        tabName = 'LoadDataPage',

        # Loaders
        shiny::fluidRow(
          shiny::column(
            width = AppWidth, shinyjs::useShinyjs(),
            shinydashboard::box(
              title = htmltools::tagList(shiny::icon('filter', lib = 'font-awesome'), 'Loading Objects'),
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = FALSE,
              background = GroupVarsBoxColor,
              width = 9L,
              shiny::fileInput(
                inputId = "DataLoad",
                label =  "Choose CSV File",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
              shiny::fileInput(
                inputId = "ModelObjectLoad",
                label =  "Load Model Output Object")))),

        # Add Space
        RemixAutoML::BlankRow(AppWidth),

        # Go to Plotter
        shiny::fluidRow(
          shiny::column(
            width = 3L, shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = 'LoadDataButton',
              label = 'Load Objects',
              icon = shiny::icon('chevron-right', lib = 'font-awesome'),
              style = 'gradient',
              color = eval(CreatePlotButtonColor))))),

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
        # Variables                            ----
        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
        shiny::fluidRow(
          shiny::column(
            width = AppWidth, shinyjs::useShinyjs(),
            shinydashboard::box(
              title = htmltools::tagList(shiny::icon('filter', lib = 'font-awesome'), 'Plotting Variables'),
              solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE, background = 'navy', width = AppWidth,
              shiny::fluidRow(
                width = AppWidth,
                shiny::column(3L, shiny::uiOutput('YVar')),
                shiny::column(3L, shiny::uiOutput('XVar')),
                shiny::column(3L, shiny::uiOutput('DateVar')),
                shiny::column(3L, shiny::uiOutput('ScoreVar'))),
              shiny::fluidRow(
                width = AppWidth,
                shiny::column(3L, shiny::uiOutput('YMin')),
                shiny::column(3L, shiny::uiOutput('XMin')),
                shiny::column(3L, shiny::uiOutput('DateMin'))),
              shiny::fluidRow(
                width = AppWidth,
                shiny::column(3L, shiny::uiOutput('YMax')),
                shiny::column(3L, shiny::uiOutput('XMax')),
                shiny::column(3L, shiny::uiOutput('DateMax')))))),

        # Add Space
        RemixAutoML::BlankRow(AppWidth),

        # GroupVars, Facets, Size Var
        shiny::fluidRow(
          shiny::column(
            width = 12L, shinyjs::useShinyjs(),
            shinydashboard::box(
              title = htmltools::tagList(shiny::icon('filter', lib = 'font-awesome'), 'Customize'),
              solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE, background = VarsBoxColor, width = AppWidth,
              shiny::column(
                width = 3L,
                tags$h3('Grouping Variables'),
                shinyWidgets::dropdown(
                  animate = TRUE, right = FALSE, circle = FALSE, tooltip = FALSE, status = "primary", inputId = "GroupVariablesUI", icon = icon("gear"), width = LogoWidth,
                  shiny::fluidRow(
                    width = AppWidth,
                    shiny::column(3L, shiny::uiOutput('GroupVars')),
                    shiny::column(3L, shiny::uiOutput('FacetVar1')),
                    shiny::column(3L, shiny::uiOutput('FacetVar2')),
                    shiny::column(3L, shiny::uiOutput('SizeVar1'))),
                  shiny::fluidRow(
                    width = AppWidth,
                    shiny::column(3L, shiny::conditionalPanel(width = 3L, condition = "length(input['GroupVars']) >= 1", shiny::uiOutput('Levels_1'))),
                    shiny::column(3L, shiny::conditionalPanel(width = 3L, condition = "length(input['GroupVars']) >= 2", shiny::uiOutput('Levels_2'))),
                    shiny::column(3L, shiny::conditionalPanel(width = 3L, condition = "length(input['GroupVars']) >= 3", shiny::uiOutput('Levels_3')))),
                  shiny::fluidRow(
                    width = AppWidth,
                    shiny::column(3L, shiny::uiOutput('NumberGroupsDisplay'))))),

              # Filter Vars
              shiny::column(
                width = 3L,
                tags$h3('Data Filtering'),
                shinyWidgets::dropdown(
                  animate = TRUE, right = FALSE, circle = FALSE, tooltip = FALSE, status = "primary", inputId = "DataFiltering", icon = icon("gear"), width = LogoWidth,
                  shiny::fluidRow(
                    width = AppWidth,
                    shiny::column(3L, shiny::uiOutput('FilterVariable_1')),
                    shiny::column(3L, shiny::uiOutput('FilterVariable_2')),
                    shiny::column(3L, shiny::uiOutput('FilterVariable_3')),
                    shiny::column(3L, shiny::uiOutput('FilterVariable_4'))),
                  shiny::fluidRow(
                    width = AppWidth,
                    shiny::column(3L, shiny::uiOutput('FilterLogic_1')),
                    shiny::column(3L, shiny::uiOutput('FilterLogic_2')),
                    shiny::column(3L, shiny::uiOutput('FilterLogic_3')),
                    shiny::column(3L, shiny::uiOutput('FilterLogic_4'))),
                  shiny::fluidRow(
                    width = AppWidth,
                    shiny::column(3L, shiny::uiOutput('FilterValue_1a')),
                    shiny::column(3L, shiny::uiOutput('FilterValue_2a')),
                    shiny::column(3L, shiny::uiOutput('FilterValue_3a')),
                    shiny::column(3L, shiny::uiOutput('FilterValue_4a'))),
                  shiny::fluidRow(
                    width = AppWidth,
                    shiny::column(3L, shiny::uiOutput('FilterValue_1b')),
                    shiny::column(3L, shiny::uiOutput('FilterValue_2b')),
                    shiny::column(3L, shiny::uiOutput('FilterValue_3b')),
                    shiny::column(3L, shiny::uiOutput('FilterValue_4b'))))),

              # Plot Formatting bars
              shiny::column(
                width = 3L,
                tags$h3('Plot Formatting'),
                shinyWidgets::dropdown(
                  animate = TRUE, right = TRUE, circle = FALSE, tooltip = FALSE, status = "primary", inputId = "PlotInputsUI1", icon = icon("gear"), width = LogoWidth,
                  shiny::fluidRow(
                    width = AppWidth,
                    shiny::column(3L, shiny::uiOutput('PlotWidth')),
                    shiny::column(3L, shiny::uiOutput('XTicks'))),
                  shiny::fluidRow(
                    width = AppWidth,
                    shiny::column(3L, shiny::uiOutput('PlotHeight')),
                    shiny::column(3L, shiny::uiOutput('YTicks'))),
                  shiny::fluidRow(
                    width = AppWidth,
                    shiny::column(3L, shiny::uiOutput('TextSize')),
                    shiny::column(3L, shiny::uiOutput('LegendPosition'))),
                  shiny::fluidRow(
                    width = AppWidth,
                    shiny::column(3L, shiny::uiOutput('AngleX')),
                    shiny::column(3L, shiny::uiOutput('LegendBorderSize'))),
                  shiny::fluidRow(
                    width = AppWidth,
                    shiny::column(3L, shiny::uiOutput('AngleY')),
                    shiny::column(3L, shiny::uiOutput('LegendLineType')),
                    shiny::column(3L, shiny::uiOutput('OutlierSize'))))),

              # Plot Formatting bars
              shiny::column(
                width = 3L,
                tags$h3('Plot Colors'),
                shinyWidgets::dropdown(
                  animate = TRUE, right = TRUE, circle = FALSE, tooltip = FALSE, status = "primary", inputId = "PlotInputsUI2", icon = icon("gear"), width = LogoWidth,
                  shiny::fluidRow(
                    width = AppWidth,
                    shiny::column(3L, shiny::uiOutput('BackGroundColor')),
                    shiny::column(3L, shiny::uiOutput('SubTitleColor'))),
                  shiny::fluidRow(
                    width = AppWidth,
                    shiny::column(3L, shiny::uiOutput('ChartColor')),
                    shiny::column(3L, shiny::uiOutput('BorderColor'))),
                  shiny::fluidRow(
                    width = AppWidth,
                    shiny::column(3L, shiny::uiOutput('TextColor')),
                    shiny::column(3L, shiny::uiOutput('BoxPlotFill'))),
                  shiny::fluidRow(
                    width = AppWidth,
                    shiny::column(3L, shiny::uiOutput('OutlierColor')),
                    shiny::column(3L, shiny::uiOutput('GridColor')))))))),

        # Add Space
        RemixAutoML::BlankRow(AppWidth),

        # ----

        # ----

        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
        # Plot Core Inputs                     ----
        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

        # Core Plot Inputs
        shiny::fluidRow(
          shiny::column(
            width = AppWidth, shinyjs::useShinyjs(),
            shinydashboard::box(
              title = htmltools::tagList(shiny::icon('filter', lib = 'font-awesome'), 'Plot Options'),
              solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE, background = PlotBoxColor, width = AppWidth,
              shiny::fluidRow(
                width = AppWidth,
                shiny::column(3L, shiny::uiOutput('GamFitScatter')),
                shiny::column(3L, shiny::uiOutput('PDP_Variable')),
                shiny::column(3L, shiny::uiOutput('Percentile_Buckets'))),

              # Add Space
              RemixAutoML::BlankRow(AppWidth),

              # Plot Type
              shiny::fluidRow(
                width = AppWidth,
                shiny::column(3L, shiny::uiOutput('PlotType')))))),

        # # Add Space
        RemixAutoML::BlankRow(AppWidth),
        RemixAutoML::BlankRow(AppWidth),

        # ----

        # ----

        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
        # Buttons to Build Plot                ----
        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
        shiny::fluidRow(

          # Create Plot!
          shiny::column(
            width = 3L, shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = 'TrendPlotExecute',
              label = 'Create Plot!',
              icon = shiny::icon('chevron-right', lib = 'font-awesome'),
              style = 'gradient',
              color = eval(CreatePlotButtonColor))),

          # Update Theme!
          shiny::column(
            width = 3L, shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = 'UpdatePlotThemeElements',
              label = 'Update Theme!',
              icon = shiny::icon('chevron-right', lib = 'font-awesome'),
              style = 'gradient',
              color = eval(UpdatePlotButtonColor))),

          # Reset Theme!
          shiny::column(
            width = 3L, shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = 'ResetPlotThemeElements',
              label = 'Reset Theme!',
              icon = shiny::icon('chevron-right', lib = 'font-awesome'),
              style = 'gradient',
              color = eval(ResetPlotButtonColor)))),

        # Add Space
        RemixAutoML::BlankRow(AppWidth),

        # ----

        # ----

        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
        # Show Plot                            ----
        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
        shiny::fluidRow(shiny::column(width = AppWidth, shiny::plotOutput('Trend')))))))


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

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Login and Page Navigation            ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  UserName <- shiny::reactive({input$UserName})
  Password <- shiny::reactive({input$Password})
  shiny::observeEvent(eventExpr = input$Check_Credentials, {
    if(UserName() %in% Credentials$UserName && Password() %in% Credentials[UserName == eval(UserName())]$Password) {
      shinyjs::removeCssClass(selector = "a[data-value='LoadDataPage']", class = "inactiveLink")
      shinyjs::removeCssClass(selector = "a[data-value='Plotter']", class = "inactiveLink")
      shinyWidgets::sendSweetAlert(session, title = NULL, text = 'Success', type = NULL, btn_labels = "success", btn_colors = "green", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {
      shinyjs::addCssClass(selector = "a[data-value='LoadDataPage']", class = "inactiveLink")
      shinyjs::addCssClass(selector = "a[data-value='Plotter']", class = "inactiveLink")
      shinyWidgets::sendSweetAlert(session, title = NULL, text = 'Username and / or password is incorrect', type = NULL, btn_labels = "error", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  })
  shiny::observeEvent(eventExpr = input$LoginPage_2_LoadDataPage, {
    print('Go to Load Data Page')
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "LoadDataPage")
  })
  shiny::observeEvent(eventExpr = input$LoadDataPage_2_LoginPage, {
    print('Go to Login Page')
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "Login")
  })
  shiny::observeEvent(eventExpr = input$LoadDataPage_2_PlotterPage, {
    print('Go to Plotter Page')
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "Plotter")
  })
  shiny::observeEvent(eventExpr = input$PlotterPage_2_LoadDataPage, {
    print('Go to Load Data Page')
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "LoadDataPage")
  })

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Load Data and ModelOutputList        ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  shiny::observeEvent(eventExpr = input$LoadDataButton, {
    if(Debug) print('data check 1')
    data <<- RemixAutoML::ReactiveLoadCSV(input, InputVal = "DataLoad", ProjectList = NULL, DateUpdateName = NULL, RemoveObjects = NULL)
    if(Debug) print('data check 2')
    inFile <- input$ModelObjectLoad
    if(!is.null(inFile)) {
      e <- new.env()
      name <- load(inFile$datapath, e)
      ModelOutputList <<- e[[name]]
    } else {
      ModelOutputList <<- NULL
    }
    shinyWidgets::sendSweetAlert(session, title = NULL, text = "Success", type = NULL, btn_labels = "success", btn_colors = "green", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
  })

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Variables                            ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  # YVar and XVar
  output$YVar <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'YVar', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Y-Variable'), Choices = c('None', names(data)), SelectedDefault = YVariable, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  # 'X-Variable'
  output$XVar <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'XVar', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'X-Variable'), Choices = c('None', names(data)), SelectedDefault = XVariable, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  # 'Score-Variable'
  output$ScoreVar <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'ScoreVar', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Scoring-Variable'), Choices = c('None', names(data)), SelectedDefault = XVariable, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  # 'Date Variable'
  output$DateVar <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'DateVar', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Date-Variable'), Choices = c('None', names(data)), SelectedDefault = DateName, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  # Reactives References
  YVar <- shiny::reactive({shiny::req(input[['YVar']])})
  XVar <- shiny::reactive({shiny::req(input[['XVar']])})
  ScoreVar <- shiny::reactive({shiny::req(input[['ScoreVar']])})
  DateVar <- shiny::reactive({shiny::req(input[['DateVar']])})

  # YMin
  output$YMin <- shiny::renderUI({
    if(Debug) print('YMin  PickerInput')
    outvals1 <- RemixAutoML::KeyVarsInit(data, VarName = eval(YVar()))
    minn <- outvals1[['MinVal']]
    choices <- outvals1[['ChoiceInput']]
    RemixAutoML::PickerInput(
      InputID = 'YMin', Multiple = FALSE, Debug = Debug,
      Label = tags$span(style=paste0('color: ', AppTextColor, ';'), 'Min Y-Value'),
      Choices = RemixAutoML::CharNull(choices),
      SelectedDefault = RemixAutoML::CharNull(choices[1L]))
  })

  # XMin
  output$XMin <- shiny::renderUI({
    if(Debug) print('XMin  PickerInput')
    outvals2 <- RemixAutoML::KeyVarsInit(data, VarName = eval(XVar()))
    choices <- outvals2[['ChoiceInput']]
    RemixAutoML::PickerInput(
      InputID = 'XMin', Multiple = FALSE, Debug = Debug,
      Label = tags$span(style=paste0('color: ', AppTextColor, ';'), 'Min X-Value'),
      Choices = RemixAutoML::CharNull(choices),
      SelectedDefault = RemixAutoML::CharNull(choices[1L]))
  })

  # YMax
  output$YMax <- shiny::renderUI({
    if(Debug) print('YMax  PickerInput')
    outvals3 <- RemixAutoML::KeyVarsInit(data, VarName = eval(YVar()))
    maxxy <- outvals3[['MaxVal']]
    choices <- outvals3[['ChoiceInput']]
    if(Debug) print(maxxy)
    RemixAutoML::PickerInput(
      InputID = 'YMax', Multiple = FALSE, Debug = Debug,
      Label = tags$span(style=paste0('color: ', AppTextColor, ';'), 'Max Y-Value'),
      Choices = RemixAutoML::CharNull(choices),
      SelectedDefault = RemixAutoML::CharNull(choices[length(choices)]))
  })

  # XMax
  output$XMax <- shiny::renderUI({
    if(Debug) print('XMax  PickerInput')
    outvals4 <- RemixAutoML::KeyVarsInit(data, VarName = eval(XVar()))
    choices <- outvals4[['ChoiceInput']]
    RemixAutoML::PickerInput(
      InputID = 'XMax', Multiple = FALSE, Debug = Debug,
      Label = tags$span(style=paste0('color: ', AppTextColor, ';'), 'Max X-Value'),
      Choices = RemixAutoML::CharNull(choices),
      SelectedDefault = RemixAutoML::CharNull(choices[length(choices)]))
  })

  # DateMin
  output$DateMin <- shiny::renderUI({
    if(Debug) print('DateMin  PickerInput')
    outvals5 <- RemixAutoML::KeyVarsInit(data, VarName = eval(DateVar()))
    choices <- outvals5[['ChoiceInput']]
    RemixAutoML::PickerInput(
      InputID = 'DateMin', Multiple = FALSE, Debug = Debug,
      Label = tags$span(style=paste0('color: ', AppTextColor, ';'), 'Min Date-Value'),
      Choices = RemixAutoML::CharNull(choices),
      SelectedDefault = RemixAutoML::CharNull(choices[1L]))
  })

  # DateMax
  output$DateMax <- shiny::renderUI({
    if(Debug) print('DateMax PickerInput')
    outvals6 <- RemixAutoML::KeyVarsInit(data, VarName = eval(DateVar()))
    choices <- outvals6[['ChoiceInput']]
    RemixAutoML::PickerInput(
      InputID = 'DateMax', Multiple = FALSE, Debug = Debug,
      Label = tags$span(style=paste0('color: ', AppTextColor, ';'), 'Max Date-Value'),
      Choices = RemixAutoML::CharNull(choices),
      SelectedDefault = RemixAutoML::CharNull(choices[length(choices)]))
  })

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Plotting MetaData                    ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  # Plot Box Shown
  output$PlotType <- shiny::renderUI({
    if(!is.null(ModelOutputList)) {
      MONames <- c(
        "Train_EvaluationPlot", "Train_EvaluationBoxPlot", "Train_ParDepPlots", "Train_ParDepBoxPlots", "Train_ResidualsHistogram",
        "Train_ScatterPlot", "Train_CopulaPlot", "Train_VariableImportance", "Validation_VariableImportance", "Test_EvaluationPlot",
        "Test_EvaluationBoxPlot", "Test_ParDepPlots", "Test_ParDepBoxPlots", "Test_ResidualsHistogram", "Test_ScatterPlot",
        "Test_CopulaPlot", "Test_VariableImportance")
    } else {
      MONames <- NULL
    }
    if(DateVar() != 'None') {
      StandardPlots <- c('BoxPlot','ViolinPlot','Line','Bar','Scatter','Copula')
    } else {
      StandardPlots <- c('Scatter','Copula','BoxPlot','ViolinPlot','Bar')
    }
    RemixAutoML::PickerInput(InputID = 'PlotType', Label = tags$span(style=paste0('color: ', AppTextColor, ';'), 'Plot Type'), Choices = c(StandardPlots, MONames), SelectedDefault = 'BoxPlot', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$PDP_Variable <- shiny::renderUI({
    if(!is.null(ModelOutputList)) {
      vals <- names(ModelOutputList$PlotList$Test_ParDepPlots)
      defa <- vals[1L]
    } else {
      vals <- NULL
      defa <- NULL
    }
    RemixAutoML::PickerInput(InputID = 'PDP_Variable', Label = tags$span(style=paste0('color: ', AppTextColor, ';'), 'PDP Variable'), Choices = vals, SelectedDefault = defa, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$Percentile_Buckets <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'Percentile_Buckets', Label = tags$span(style=paste0('color: ', AppTextColor, ';'), 'Percentile Buckets'), Choices = 1:100, SelectedDefault = 20, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$GamFitScatter <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'GamFitScatter', Label = tags$span(style=paste0('color: ', AppTextColor, ';'), 'Fit Gam on Scatter or Copula'), Choices = c('TRUE', 'FALSE'), SelectedDefault = FALSE, Multiple = FALSE, ActionBox = TRUE)
  })

  # UI Plot Options
  output$NumberGroupsDisplay <- shiny::renderUI({
    RemixAutoML::NumericInput(InputID = 'NumberGroupsDisplay', Label = tags$span(style='color: blue;', 'Dispay N Levels'), Step = 1L, Value = 5L, Min = 1L, Max = 100L)
  })
  output$PlotWidth <- shiny::renderUI({
    RemixAutoML::NumericInput(InputID = "PlotWidth", Label = tags$span(style='color: blue;', 'Plot Width'), Step = 50, Min = 800, Max = 1800, Value = 1600)
  })
  output$PlotHeight <- shiny::renderUI({
    RemixAutoML::NumericInput(InputID = "PlotHeight", Label = tags$span(style='color: blue;', 'Plot Height'), Step = 25, Min = 350, Max = 350*10, Value = 500)
  })
  output$YTicks <- shiny::renderUI({
    Uniques <- tryCatch({data[, unique(get(YVar()))]}, error = function(x) NULL)
    if(!is.null(Uniques)) {
      if(any(class(data[[eval(YVar())]]) %in% c('numeric','integer')) && Uniques > 10L) {
        choices <- c('Default', 'percentiles', '5th-tiles', 'Deciles', 'Quantiles', 'Quartiles', as.character(data[, quantile(round(get(YVar()), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]))
      } else {
        choices <- c('Default', Uniques)
      }
    } else {
      choices <- 'Default'
    }
    RemixAutoML::PickerInput(InputID = 'YTicks', Label = tags$span(style='color: blue;', 'Y-Axis Ticks'), Choices = choices, SelectedDefault = 'Default', Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
  })
  output$XTicks <- shiny::renderUI({
    if(XVar() != 'None') {
      Uniques <- tryCatch({data[, unique(get(XVar()))]}, error = function(x) NULL)
      x <- class(data[[eval(XVar())]])[1L]
      if(x %chin% c('numeric','integer') && length(Uniques) > 10L) {
        choices <- c('Default', 'Percentiles', 'Every 5th percentile', 'Deciles', 'Quantiles', 'Quartiles', as.character(data[, quantile(round(get(XVar()), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]))
      } else if(x %chin% c('Date')) {
        choices <- c('Default', '1 year', '1 day', '1 week', '1 month', '3 day', '2 week', '3 month', '6 month', '2 year', '5 year', '10 year')
      } else if(x %like% c('POSIX')) {
        choices <- c('Default', '1 year', '1 day', '3 day', '1 week', '2 week', '1 month', '3 month', '6 month', '2 year', '5 year', '10 year', '1 minute', '15 minutes', '30 minutes', '1 hour', '3 hour', '6 hour', '12 hour')
      } else if(x %like% c('character','numeric','integer')) {
        choices <- c('Default', Uniques)
      } else {
        choices <- c('Default', Uniques)
      }
    } else if(DateVar() != 'None') {
      Uniques <- tryCatch({data[, unique(get(DateVar()))]}, error = function(x) NULL)
      x <- class(data[[eval(DateVar())]])[1L]
      if(x %chin% c('Date')) {
        choices <- c('Default', '1 year', '1 day', '1 week', '1 month', '3 day', '2 week', '3 month', '6 month', '2 year', '5 year', '10 year')
      } else if(x %like% c('POSIX')) {
        choices <- c('Default', '1 year', '1 day', '3 day', '1 week', '2 week', '1 month', '3 month', '6 month', '2 year', '5 year', '10 year', '1 minute', '15 minutes', '30 minutes', '1 hour', '3 hour', '6 hour', '12 hour')
      } else {
        choices <- c('Default', Uniques)
      }
    }
    RemixAutoML::PickerInput(InputID = 'XTicks', Label = tags$span(style='color: blue;', 'X-Axis Ticks'), Choices = choices, SelectedDefault = 'Default', Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
  })
  output$AngleY <- shiny::renderUI({
    RemixAutoML::NumericInput(InputID = 'AngleY', Label = tags$span(style='color: blue;', 'Y-axis text angle'), Step = 5, Min = 0, Max = 360, Value = 0)
  })
  output$AngleX <- shiny::renderUI({
    RemixAutoML::NumericInput(InputID = 'AngleX', Label = tags$span(style='color: blue;', 'X-axis text angle'), Step = 5, Min = 0, Max = 360, Value = 90)
  })
  output$TextSize <- shiny::renderUI({
    RemixAutoML::NumericInput(InputID = 'TextSize', Label = tags$span(style='color: blue;', 'Text Size'), Step = 1, Min = 1, Max = 50, Value = 12)
  })
  output$OutlierSize <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'OutlierSize', Label = tags$span(style='color: blue;', 'Outlier Size'), Choices = c(seq(0.01,2,0.01)), SelectedDefault = 0.01, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$LegendPosition <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'LegendPosition', Label = tags$span(style='color: blue;', 'Legend Position'), Choices = c('bottom','left','right','top'), SelectedDefault = 'bottom', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$LegendBorderSize <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'LegendBorderSize', Label = tags$span(style='color: blue;', 'Legend Border Size'), Choices = c(as.character(seq(0.01,2,0.01))), SelectedDefault = as.character(0.01), Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$LegendLineType <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'LegendLineType', Label = tags$span(style='color: blue;', 'Legend Border Type'), Choices = c('solid','blank','dashed','dotdash','dotted','longlash','twodash'), SelectedDefault = 'solid', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  # Color boxes
  output$TextColor <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'TextColor', Label = tags$span(style='color: blue;', 'Text Color'), Choices = grDevices::colors(), SelectedDefault = 'darkblue', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$ChartColor <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'ChartColor', Label = tags$span(style='color: blue;', 'Chart Color'), Choices = grDevices::colors(), SelectedDefault = 'lightsteelblue1', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$GridColor <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'GridColor', Label = tags$span(style='color: blue;', 'Grid Lines Color'), Choices = grDevices::colors(), SelectedDefault = 'white', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$BackGroundColor <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'BackGroundColor', Label = tags$span(style='color: blue;', 'Background Color'), Choices = grDevices::colors(), SelectedDefault = 'gray95', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$BorderColor <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'BorderColor', Label = tags$span(style='color: blue;', 'Border Color'), Choices = grDevices::colors(), SelectedDefault = 'darkblue', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$OutlierColor <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'OutlierColor', Label = tags$span(style='color: blue;', 'Outlier Color'), Choices = grDevices::colors(), SelectedDefault = 'blue', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$BoxPlotFill <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'BoxPlotFill', Label = tags$span(style='color: blue;', 'BoxPlot Fill Color'), Choices = grDevices::colors(), SelectedDefault = 'gray', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$SubTitleColor <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'SubTitleColor', Label = tags$span(style='color: blue;', 'Subtitle Color'), Choices = grDevices::colors(), SelectedDefault = 'blue', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Group Variables                      ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  # Select GroupVars
  output$GroupVars <- shiny::renderUI({
    if(Debug) print('PickerInput GroupVars')
    RemixAutoML::PickerInput(
      InputID = 'GroupVars', Label = tags$span(style='color: blue;', 'Select Group Variables'),
      Choices = c('None', names(data)), SelectedDefault = if(!is.null(GroupVariables)) GroupVariables else 'None',
      SelectedText = 'count > 1', Multiple = TRUE, ActionBox = TRUE)
  })

  # Reactive Group Variables
  SelectedGroups <- shiny::reactive({
    g <- tryCatch({input$GroupVars}, error = function(x) NULL)
    RemixAutoML::ReturnParam(input, VarName = 'GroupVars', Default = if(!is.null(GroupVariables)) GroupVariables else 'None', Switch = TRUE, Type = 'character')
  })

  # Levels
  output$Levels_1 <- shiny::renderUI({
    if(Debug) print('PickerInput_GetLevels 1')
    if(Debug) print(SelectedGroups())
    if(Debug) print(length(SelectedGroups()))
    if(Debug) print('here')
    if(Debug) print(RemixAutoML::UniqueLevels(input, data, 1L, GroupVars=SelectedGroups()))
    if(Debug) print('here 1')
    sgs <- SelectedGroups()
    if(Debug) print('None' %in% sgs)
    if(Debug) print(length(sgs) != 1)
    if('None' %in% sgs && length(sgs) != 1) {
      sgs <- sgs[!sgs %in% 'None']
    }
    if(Debug) print(sgs)
    if(Debug) print(RemixAutoML::UniqueLevels(input, data, 1L, GroupVars=sgs))
    RemixAutoML::PickerInput_GetLevels2(
      input, data = 'data', NumGroupVar = 1L, Size = 9, SelectedText = 'count > 1', Multiple = TRUE, ActionBox = TRUE,
      InputID = 'Levels_1', InputID2 = sgs,
      Choices = RemixAutoML::UniqueLevels(input, data, 1L, GroupVars=sgs), SelectedDefault = NULL)
  })

  # Levels
  output$Levels_2 <- shiny::renderUI({
    if(Debug) print('PickerInput_GetLevels 2')
    if('None' %chin% SelectedGroups() && length(SelectedGroups()) != 1) {
      sgs <- SelectedGroups()[!SelectedGroups() %in% 'None']
    } else {
      sgs <- SelectedGroups()
    }
    RemixAutoML::PickerInput_GetLevels2(
      input, data = 'data', NumGroupVar = 2L, Size = 9, SelectedText = 'count > 1', Multiple = TRUE, ActionBox = TRUE,
      InputID = 'Levels_2', InputID2 = sgs,
      Choices = RemixAutoML::UniqueLevels(input, data, 2L, GroupVars=sgs), SelectedDefault = NULL)
  })

  # Levels
  output$Levels_3 <- shiny::renderUI({
    if(Debug) print('PickerInput_GetLevels 3')
    if('None' %chin% SelectedGroups() && length(SelectedGroups()) != 1) {
      sgs <- SelectedGroups()[!SelectedGroups() %in% 'None']
    } else {
      sgs <- SelectedGroups()
    }
    RemixAutoML::PickerInput_GetLevels2(
      input, data = 'data', NumGroupVar = 3L, Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE,
      InputID = 'Levels_3', InputID2 = sgs,
      Choices = RemixAutoML::UniqueLevels(input, data, 3L, GroupVars=sgs), SelectedDefault = NULL)
  })

  # Faceting
  output$FacetVar1 <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'FacetVar1', Label = tags$span(style='color: blue;', 'Facet Variable 1'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$FacetVar2 <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'FacetVar2', Label = tags$span(style='color: blue;', 'Facet Variable 2'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  # Sizing
  output$SizeVar1 <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'SizeVar1', Label = tags$span(style='color: blue;', 'Size Variable'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Filter Variables                     ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  # Filter Variable 1
  output$FilterVariable_1 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterVariable_1', label = tags$span(style='color: blue;', 'Filter Variable 1'), choices=c('None', names(data)), selected='None')
  })

  # Filter Variable 2
  output$FilterVariable_2 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterVariable_2', label = tags$span(style='color: blue;', 'Filter Variable 2'), choices=c('None', names(data)), selected='None')
  })

  # Filter Variable 3
  output$FilterVariable_3 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterVariable_3', label = tags$span(style='color: blue;', 'Filter Variable 3'), choices=c('None', names(data)), selected='None')
  })

  # Filter Variable 4
  output$FilterVariable_4 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterVariable_4', label = tags$span(style='color: blue;', 'Filter Variable 4'), choices=c('None', names(data)), selected='None')
  })

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Filter Logic                         ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  # Filter Logic 1
  output$FilterLogic_1 <- shiny::renderUI({
    if(input[['FilterVariable_1']] != 'None') {
      x <- class(data[[eval(input[['FilterVariable_1']])]])
    } else {
      x <- 'Adrian'
    }
    if(any(x %in% c('factor', 'character'))) FL_Default <- '%chin%' else FL_Default <- '>='
    shiny::selectInput(inputId='FilterLogic_1', label = tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), selected=FL_Default, multiple=FALSE)
  })

  # Filter Logic 2
  output$FilterLogic_2 <- shiny::renderUI({
    if(input[['FilterVariable_2']] != 'None') {
      x <- class(data[[eval(input[['FilterVariable_2']])]])
    } else {
      x <- 'Adrian'
    }
    if(any(x %in% c('factor', 'character'))) FL_Default <- '%chin%' else FL_Default <- '>='
    shiny::selectInput(inputId='FilterLogic_2', label = tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), selected=FL_Default, multiple=FALSE)
  })

  # Filter Logic 3
  output$FilterLogic_3 <- shiny::renderUI({
    if(input[['FilterVariable_3']] != 'None') {
      x <- class(data[[eval(input[['FilterVariable_3']])]])
    } else {
      x <- 'Adrian'
    }
    if(any(x %in% c('factor', 'character'))) FL_Default <- '%chin%' else FL_Default <- '>='
    shiny::selectInput(inputId='FilterLogic_3', label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), selected=FL_Default, multiple=FALSE)
  })

  # Filter Logic 4
  output$FilterLogic_4 <- shiny::renderUI({
    if(input[['FilterVariable_4']] != 'None') {
      x <- class(data[[eval(input[['FilterVariable_4']])]])
    } else {
      x <- 'Adrian'
    }
    if(any(x %in% c('factor', 'character'))) FL_Default <- '%chin%' else FL_Default <- '>='
    shiny::selectInput(inputId='FilterLogic_4', label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), selected=FL_Default, multiple=FALSE)
  })

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Filter Values                        ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  # Filter Values 1a
  output$FilterValue_1a <- shiny::renderUI({
    params <- list(data=data, VarName = input[['FilterVariable_1']], type = 1)
    Mult <- do.call(RemixAutoML::GetFilterValueMultiple, params)
    if(Debug) {print('Here 1111'); print(do.call(RemixAutoML::GetFilterValueLabel, params))}
    Lab <- tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params))
    if(Debug) print(Lab)
    FilterUnique <- do.call(RemixAutoML::FilterValues, params)
    RemixAutoML::PickerInput(InputID='FilterValue_1a', Label=Lab, Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=Mult, ActionBox=TRUE)
  })

  # Filter Values 1b
  output$FilterValue_1b <- shiny::renderUI({
    params <- list(data=data, VarName = input[['FilterVariable_1']], type = 2)
    Mult <- do.call(RemixAutoML::GetFilterValueMultiple, params)
    Lab <- tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params))
    FilterUnique <- do.call(RemixAutoML::FilterValues, params)
    RemixAutoML::PickerInput(InputID='FilterValue_1b', Label=Lab, Choices=FilterUnique, SelectedDefault=FilterUnique[length(FilterUnique)], Multiple=Mult, ActionBox=TRUE)
  })

  # Filter Values 2a
  output$FilterValue_2a <- shiny::renderUI({
    params <- list(data=data, VarName = input[['FilterVariable_2']], type = 1)
    Mult <- do.call(RemixAutoML::GetFilterValueMultiple, params)
    Lab <- tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params))
    FilterUnique <- do.call(RemixAutoML::FilterValues, params)
    RemixAutoML::PickerInput(InputID='FilterValue_2a', Label=Lab, Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=Mult, ActionBox=TRUE)
  })

  # Filter Values 2b
  output$FilterValue_2b <- shiny::renderUI({
    params <- list(data=data, VarName = input[['FilterVariable_2']], type = 2)
    Mult <- do.call(RemixAutoML::GetFilterValueMultiple, params)
    Lab <- tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params))
    FilterUnique <- do.call(RemixAutoML::FilterValues, params)
    RemixAutoML::PickerInput(InputID='FilterValue_2b', Label=Lab, Choices=FilterUnique, SelectedDefault=FilterUnique[length(FilterUnique)], Multiple=Mult, ActionBox=TRUE)
  })

  # Filter Values 3a
  output$FilterValue_3a <- shiny::renderUI({
    params <- list(data=data, VarName = input[['FilterVariable_3']], type = 1)
    Mult <- do.call(RemixAutoML::GetFilterValueMultiple, params)
    Lab <- tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params))
    FilterUnique <- do.call(RemixAutoML::FilterValues, params)
    RemixAutoML::PickerInput(InputID='FilterValue_3a', Label=Lab, Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=Mult, ActionBox=TRUE)
  })

  # Filter Values 3b
  output$FilterValue_3b <- shiny::renderUI({
    params <- list(data=data, VarName = input[['FilterVariable_3']], type = 2)
    Mult <- do.call(RemixAutoML::GetFilterValueMultiple, params)
    Lab <- tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params))
    FilterUnique <- do.call(RemixAutoML::FilterValues, params)
    RemixAutoML::PickerInput(InputID='FilterValue_3b', Label=Lab, Choices=FilterUnique, SelectedDefault=FilterUnique[length(FilterUnique)], Multiple=Mult, ActionBox=TRUE)
  })

  # Filter Values 4a
  output$FilterValue_4a <- shiny::renderUI({
    params <- list(data=data, VarName = input[['FilterVariable_4']], type = 1)
    Mult <- do.call(RemixAutoML::GetFilterValueMultiple, params)
    Lab <- tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params))
    FilterUnique <- do.call(RemixAutoML::FilterValues, params)
    RemixAutoML::PickerInput(InputID='FilterValue_4a', Label=Lab, Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=Mult, ActionBox=TRUE)
  })

  # Filter Values 4b
  output$FilterValue_4b <- shiny::renderUI({
    params <- list(data=data, VarName = input[['FilterVariable_4']], type = 2)
    Mult <- do.call(RemixAutoML::GetFilterValueMultiple, params)
    Lab <- tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params))
    FilterUnique <- do.call(RemixAutoML::FilterValues, params)
    RemixAutoML::PickerInput(InputID='FilterValue_4b', Label=Lab, Choices=FilterUnique, SelectedDefault=FilterUnique[length(FilterUnique)], Multiple=Mult, ActionBox=TRUE)
  })

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Reset Plot Format Only               ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  shiny::observeEvent(eventExpr = input[['ResetPlotThemeElements']], {
    if(Debug) for(zzzz in 1:4) print(':: :: RESET PLOTS :: ::')
    if(Debug) {
      print(!exists('p1') || !exists('data1'))
      print(!exists('p1'))
      print(!exists('data1'))
    }

    # Stop if p1 or data1 doesn't exist
    if(!exists('p1') || !exists('data1')) {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = "Try Create Plot", type = NULL, btn_labels = "error", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")

    } else {

      # Update chart theme elements
      if(Debug) print('Update ChartTheme()')
      p1 <- p1 + RemixAutoML::ChartTheme(
        Size = 12,
        AngleX = 90,
        AngleY = 0,
        ChartColor = 'lightsteelblue1',
        BorderColor = 'darkblue',
        TextColor = 'darkblue',
        GridColor = 'white',
        BackGroundColor = 'gray95',
        SubTitleColor = 'blue',
        LegendPosition = 'bottom',
        LegendBorderSize = 0.5,
        LegendLineType = 'solid') +
          ggplot2::theme(legend.title = ggplot2::element_blank())

      # Update labels
      if(input[['PlotType']] %chin% c('BoxPlot','ViolinPlot','Bar')) {
        if(Debug) print('BoxPlot or ViolinPlot reset labs')
        p1 <- p1 + ggplot2::labs(
          title = paste0(shiny::isolate(input[['PlotType']]), ' Plot'),
          subtitle = 'Blue line = mean(Y)',
          caption = 'by RemixAutoML') +
          ggplot2::ylim(as.numeric(eval(input[['YMin']])), as.numeric(eval(input[['YMax']]))) +
          ggplot2::ylab(shiny::isolate(YVar())) + ggplot2::xlab(shiny::isolate(DateVar()))

      } else if(input[['PlotType']] %chin% c('Line')) {
        if(Debug) print('Line reset labs')
        p1 <- p1 + ggplot2::labs(
          title = paste0(input[['PlotType']], ' Plot'),
          caption = 'by RemixAutoML') +
          ggplot2::ylim(as.numeric(eval(input[['YMin']])), as.numeric(eval(input[['YMax']]))) +
          ggplot2::ylab(shiny::isolate(YVar())) + ggplot2::xlab(shiny::isolate(DateVar()))

      } else if(input[['PlotType']] %chin% c('Scatter','Copula')) {

        # Labs
        if(Debug) print('Scatter or Copula reset labs')
        p1 <- p1 + ggplot2::labs(
          title = paste0(shiny::isolate(input[['PlotType']]), ' Plot'),
          caption = 'by RemixAutoML') +
          ggplot2::ylim(as.numeric(input[['YMin']]), as.numeric(input[['YMax']])) +
          ggplot2::ylab(shiny::isolate(YVar())) + ggplot2::xlab(shiny::isolate(XVar()))

        # Tick Marks
        if('Percentiles' %in% input[['YTicks']]) {
          y_vals <- data1[, quantile(round(get(YVar()), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
        } else if('Every 5th percentile' %in% input[['YTicks']]) {
          y_vals <- data1[, quantile(round(get(YVar()), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
          y_vals <- y_vals[c(seq(6L, length(y_vals)-1L, 5L))]
        } else if('Deciles' %in% input[['YTicks']]) {
          y_vals <- data1[, quantile(round(get(YVar()), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
          y_vals <- y_vals[c(seq(11L, length(y_vals)-1L, 10L))]
        } else if('Quantiles' %in% input[['YTicks']]) {
          y_vals <- data1[, quantile(round(get(YVar()), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
          y_vals <- y_vals[c(seq(21L, length(y_vals)-1L, 20L))]
        } else if('Quartiles' %in% input[['YTicks']]) {
          y_vals <- data1[, quantile(round(get(YVar()), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
          y_vals <- y_vals[c(seq(26L, length(y_vals)-1L, 25L))]
        } else {
          y_vals <- input[['YTicks']]
        }
        if('Percentiles' %in% input[['XTicks']]) {
          x_vals <- data1[, quantile(round(get(XVar()), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
        } else if('Every 5th percentile' %in% input[['XTicks']]) {
          x_vals <- data1[, quantile(round(get(YVar()), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
          x_vals <- x_vals[c(seq(6L, length(x_vals)-1L, 5L))]
        } else if('Deciles' %in% input[['XTicks']]) {
          x_vals <- data1[, quantile(round(get(YVar()), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
          x_vals <- x_vals[c(seq(11L, length(x_vals)-1L, 10L))]
        } else if('Quantiles' %in% input[['XTicks']]) {
          x_vals <- data1[, quantile(round(get(YVar()), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
          x_vals <- x_vals[c(seq(21L, length(x_vals)-1L, 20L))]
        } else if('Quartiles' %in% input[['XTicks']]) {
          x_vals <- data1[, quantile(round(get(YVar()), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
          x_vals <- x_vals[c(seq(26L, length(x_vals)-1L, 25L))]
        } else {
          x_vals <- input[['XTicks']]
        }
        if(!'Default' %in% input[['XTicks']]) p1 <- p1 + suppressMessages(ggplot2::scale_x_continuous(breaks = as.numeric(x_vals)))
        if(!'Default' %in% input[['YTicks']]) p1 <- p1 + suppressMessages(ggplot2::scale_y_continuous(breaks = as.numeric(y_vals)))
      }

      # UI Plot Options ----
      output$PlotWidth <- shiny::renderUI({
        RemixAutoML::NumericInput(InputID = "PlotWidth", Label = tags$span(style='color: blue;', 'Plot Width'), Step = 50, Min = 800, Max = 1800, Value = 1600)
      })
      output$PlotHeight <- shiny::renderUI({
        RemixAutoML::NumericInput(InputID = "PlotHeight", Label = tags$span(style='color: blue;', 'Plot Height'), Step = 25, Min = 350, Max = 350*10, Value = 500)
      })
      output$AngleY <- shiny::renderUI({
        RemixAutoML::NumericInput(InputID = "AngleY", Label = tags$span(style='color: blue;', 'Y-axis text angle'), Step = 5, Min = 0, Max = 360, Value = 0)
      })
      output$AngleX <- shiny::renderUI({
        RemixAutoML::NumericInput(InputID = "AngleX", Label = tags$span(style='color: blue;', 'X-axis text angle'), Step = 5, Min = 0, Max = 360, Value = 90)
      })
      output$TextSize <- shiny::renderUI({
        RemixAutoML::NumericInput(InputID = "TextSize", Label = tags$span(style='color: blue;', 'Text size'), Step = 1, Min = 1, Max = 50, Value = 12)
      })

      # Color boxes ----
      output$TextColor <- shiny::renderUI({
        RemixAutoML::PickerInput(InputID = "TextColor", Label = tags$span(style='color: blue;', 'Text color'), Choices = grDevices::colors(), SelectedDefault = "darkblue", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
      })
      output$ChartColor <- shiny::renderUI({
        RemixAutoML::PickerInput(InputID = "ChartColor", Label = tags$span(style='color: blue;', 'Chart color'), Choices = grDevices::colors(), SelectedDefault = "lightsteelblue1", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
      })
      output$GridColor <- shiny::renderUI({
        RemixAutoML::PickerInput(InputID = "GridColor", Label = tags$span(style='color: blue;', 'Grid lines color'), Choices = grDevices::colors(), SelectedDefault = "white", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
      })
      output$BackGroundColor <- shiny::renderUI({
        RemixAutoML::PickerInput(InputID = "BackGroundColor", Label = tags$span(style='color: blue;', 'Background color'), Choices = grDevices::colors(), SelectedDefault = "gray95", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
      })
      output$BorderColor <- shiny::renderUI({
        RemixAutoML::PickerInput(InputID = "BorderColor", Label = tags$span(style='color: blue;', 'Border color'), Choices = grDevices::colors(), SelectedDefault = "darkblue", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
      })

      # Return
      p1 <<- p1
      output$Trend <- shiny::renderPlot(width = input[['PlotWidth']], height = input[['PlotHeight']], {
        p1
      })
    }
  })

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Update Plot Format Only              ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  shiny::observeEvent(eventExpr = input[['UpdatePlotThemeElements']], {

    if(Debug) for(zzzz in 1:4) print(':: :: UPDATE PLOTS :: ::')
    if(!exists('p1') || !exists('data1')) {

      shinyWidgets::sendSweetAlert(session, title = NULL, text = "Try Create Plot", type = NULL, btn_labels = "error", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")

    } else {

      # Update chart theme elements
      p1 <- p1 + RemixAutoML::ChartTheme(
        Size = input[['TextSize']],
        AngleX = input[['AngleX']],
        AngleY = input[['AngleY']],
        ChartColor = input[['ChartColor']],
        BorderColor = input[['BorderColor']],
        TextColor = input[['TextColor']],
        GridColor = input[['GridColor']],
        BackGroundColor = input[['BackGroundColor']],
        SubTitleColor = input[['SubTitleColor']],
        LegendPosition = input[['LegendPosition']],
        LegendBorderSize = as.numeric(input[['LegendBorderSize']]),
        LegendLineType = input[['LegendLineType']]) +
          ggplot2::theme(legend.title = ggplot2::element_blank())

      # Update labels
      if(input[['PlotType']] %chin% c('BoxPlot','ViolinPlot')) {
        p1 <- p1 + ggplot2::labs(
          title = paste0(input[['PlotType']], ' Plot'),
          subtitle = 'Blue line = mean(Y)',
          caption = 'by RemixAutoML') +
          ggplot2::ylim(as.numeric(input[['YMin']]), as.numeric(input[['YMax']])) +
          ggplot2::ylab(shiny::isolate(YVar())) + ggplot2::xlab(shiny::isolate(DateVar()))
      } else if(shiny::isolate(input[['PlotType']] %chin% c('Line'))) {
        if(!'Default' %in% input[['XTicks']]) p1 <- p1 + suppressMessages(ggplot2::scale_x_date(date_breaks = input[['XTicks']]))
        p1 <- p1 + ggplot2::labs(
          title = paste0(shiny::isolate(input[['PlotType']]), ' Plot'),
          caption = 'by RemixAutoML') +
          ggplot2::ylim(as.numeric(input[['YMin']]), as.numeric(input[['YMax']])) +
          ggplot2::ylab(shiny::isolate(YVar())) + ggplot2::xlab(shiny::isolate(DateVar()))
      } else if(shiny::isolate(input[['PlotType']] %chin% c('Scatter','Copula'))) {
        p1 <- p1 + ggplot2::labs(
          title = paste0(shiny::isolate(input[['PlotType']]), ' Plot'),
          caption = 'by RemixAutoML') +
          ggplot2::ylim(as.numeric(eval(input[['YMin']])), as.numeric(eval(input[['YMax']]))) +
          ggplot2::ylab(shiny::isolate(YVar())) + ggplot2::xlab(shiny::isolate(XVar()))

        # Tick Marks
        if('Percentiles' %in% input[['YTicks']]) {
          y_vals <- data1[, quantile(round(get(YVar()), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
        } else if('Every 5th percentile' %in% input[['YTicks']]) {
          y_vals <- data1[, quantile(round(get(YVar()), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
          y_vals <- y_vals[c(seq(6L, length(y_vals)-1L, 5L))]
        } else if('Deciles' %in% input[['YTicks']]) {
          y_vals <- data1[, quantile(round(get(YVar()), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
          y_vals <- y_vals[c(seq(11L, length(y_vals)-1L, 10L))]
        } else if('Quantiles' %in% input[['YTicks']]) {
          y_vals <- data1[, quantile(round(get(YVar()), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
          y_vals <- y_vals[c(seq(21L, length(y_vals)-1L, 20L))]
        } else if('Quartiles' %in% input[['YTicks']]) {
          y_vals <- data1[, quantile(round(get(YVar()), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
          y_vals <- y_vals[c(seq(26L, length(y_vals)-1L, 25L))]
        } else {
          y_vals <- input[['YTicks']]
        }
        if('Percentiles' %in% input[['XTicks']]) {
          x_vals <- data1[, quantile(round(get(XVar()), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
        } else if('Every 5th percentile' %in% input[['XTicks']]) {
          x_vals <- data1[, quantile(round(get(YVar()), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
          x_vals <- x_vals[c(seq(6L, length(x_vals)-1L, 5L))]
        } else if('Deciles' %in% input[['XTicks']]) {
          x_vals <- data1[, quantile(round(get(YVar()), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
          x_vals <- x_vals[c(seq(11L, length(x_vals)-1L, 10L))]
        } else if('Quantiles' %in% input[['XTicks']]) {
          x_vals <- data1[, quantile(round(get(YVar()), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
          x_vals <- x_vals[c(seq(21L, length(x_vals)-1L, 20L))]
        } else if('Quartiles' %in% input[['XTicks']]) {
          x_vals <- data1[, quantile(round(get(YVar()), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
          x_vals <- x_vals[c(seq(26L, length(x_vals)-1L, 25L))]
        } else {
          x_vals <- input[['XTicks']]
        }
        if(!'Default' %in% input[['XTicks']]) p1 <- p1 + suppressMessages(ggplot2::scale_x_continuous(breaks = as.numeric(x_vals)))
        if(!'Default' %in% input[['YTicks']]) p1 <- p1 + suppressMessages(ggplot2::scale_y_continuous(breaks = as.numeric(y_vals)))
      }

      # Return
      p1 <<- p1
      output$Trend <- shiny::renderPlot(width = shiny::isolate(input[['PlotWidth']]), height = shiny::isolate(input[['PlotHeight']]), {
        p1
      })
    }
  })

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Create Plot                          ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  shiny::observeEvent(eventExpr = input[['TrendPlotExecute']], {

    if(Debug) for(zzzz in 1:4) print(':: :: CREATE PLOTS :: ::')

    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    # PreDefine Args that are hidden       ----
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    NumberGroupsDisplay <<- RemixAutoML::ReturnParam(input, VarName='NumberGroupsDisplay', Type = 'numeric', Default = 5L, Switch = TRUE)
    PlotWidth <<- RemixAutoML::ReturnParam(input, VarName = 'PlotWidth', Type = 'numeric', Default = 1600, Switch = TRUE)
    PlotHeight <<- RemixAutoML::ReturnParam(input, VarName = 'PlotHeight', Type = 'numeric', Default = 500, Switch = TRUE)
    YTicks <<- RemixAutoML::ReturnParam(input, VarName = 'YTicks', Type = 'character', Default = 'Default', Switch = TRUE)
    XTicks <<- RemixAutoML::ReturnParam(input, VarName = 'XTicks', Type = 'character', Default = 'Default', Switch = TRUE)
    AngleY <<- RemixAutoML::ReturnParam(input, VarName = 'AngleY', Type = 'numeric', Default = 0L, Switch = TRUE)
    AngleX <<- RemixAutoML::ReturnParam(input, VarName = 'AngleX', Type = 'numeric', Default = 90L, Switch = TRUE)
    TextSize <<- RemixAutoML::ReturnParam(input, VarName = 'TextSize', Type = 'numeric', Default = 12L, Switch = TRUE)
    OutlierSize <<- RemixAutoML::ReturnParam(input, VarName = 'OutlierSize', Type = 'numeric', Default = 0.01, Switch = TRUE)
    LegendPosition <<- RemixAutoML::ReturnParam(input, VarName = 'LegendPosition', Type = 'character', Default = 'bottom', Switch = TRUE)
    LegendBorderSize <<- RemixAutoML::ReturnParam(input, VarName = 'LegendBorderSize', Type = 'numeric', Default = 0.01, Switch = TRUE)
    LegendLineType <<- RemixAutoML::ReturnParam(input, VarName = 'LegendLineType', Type = 'character', Default = 'solid', Switch = TRUE)

    # Color boxes
    TextColor <<- RemixAutoML::ReturnParam(input, VarName = 'TextColor', Type = 'character', Default = 'darkblue', Switch = TRUE)
    ChartColor <<- RemixAutoML::ReturnParam(input, VarName = 'ChartColor', Type = 'character', Default = 'lightsteelblue1', Switch = TRUE)
    GridColor <<- RemixAutoML::ReturnParam(input, VarName = 'GridColor', Type = 'character', Default = 'white', Switch = TRUE)
    BackGroundColor <<- RemixAutoML::ReturnParam(input, VarName = 'BackGroundColor', Type = 'character', Default = 'gray95', Switch = TRUE)
    BorderColor <<- RemixAutoML::ReturnParam(input, VarName = 'BorderColor', Type = 'character', Default = 'darkblue', Switch = TRUE)
    OutlierColor <<- RemixAutoML::ReturnParam(input, VarName = 'OutlierColor', Type = 'character', Default = 'blue', Switch = TRUE)
    BoxPlotFill <<- RemixAutoML::ReturnParam(input, VarName = 'BoxPlotFill', Type = 'character', Default = 'gray', Switch = TRUE)
    SubTitleColor <<- RemixAutoML::ReturnParam(input, VarName = 'SubTitleColor', Type = 'character', Default = 'blue', Switch = TRUE)

    # Grouping Variable Management
    if(!exists('RunNumber')) {
      SubsetList <- list()
      SubsetList[['RunNumber']] <- 1L
      SubsetList[['DataPrep']] <- TRUE
      SubsetList[['SelectedGroupss']] <- tryCatch({shiny::isolate(SelectedGroups())}, error = function(x) 'None')
      SubsetList[['GroupVars']] <- RemixAutoML::ReturnParam(input, VarName = 'GroupVars', Type = 'character', Default = 'None', Switch = TRUE)
      SubsetList[['Levels_1']] <- RemixAutoML::ReturnParam(input, VarName = 'Levels_1', Type = 'character', Default = 'None', Switch = TRUE)
      SubsetList[['Levels_2']] <- RemixAutoML::ReturnParam(input, VarName = 'Levels_2', Type = 'character', Default = 'None', Switch = TRUE)
      SubsetList[['Levels_3']] <- RemixAutoML::ReturnParam(input, VarName = 'Levels_3', Type = 'character', Default = 'None', Switch = TRUE)
      SubsetList[['FacetVar1']] <- RemixAutoML::ReturnParam(input, VarName = 'FacetVar1', Type = 'character', Default = 'None', Switch = TRUE)
      SubsetList[['FacetVar2']] <- RemixAutoML::ReturnParam(input, VarName = 'FacetVar2', Type = 'character', Default = 'None', Switch = TRUE)
      SubsetList[['SizeVar1']] <- RemixAutoML::ReturnParam(input, VarName = 'SizeVar1', Type = 'character', Default = 'None', Switch = TRUE)
      SubsetList[['FilterVariable_1']] <- RemixAutoML::ReturnParam(input, VarName = 'FilterVariable_1', Type = 'character', Default = 'None', Switch = TRUE)
      SubsetList[['FilterVariable_2']] <- RemixAutoML::ReturnParam(input, VarName = 'FilterVariable_2', Type = 'character', Default = 'None', Switch = TRUE)
      SubsetList[['FilterVariable_3']] <- RemixAutoML::ReturnParam(input, VarName = 'FilterVariable_3', Type = 'character', Default = 'None', Switch = TRUE)
      SubsetList[['FilterVariable_4']] <- RemixAutoML::ReturnParam(input, VarName = 'FilterVariable_4', Type = 'character', Default = 'None', Switch = TRUE)
      SubsetList[['FilterLogic_1']] <- RemixAutoML::ReturnParam(input, VarName = 'FilterLogic_1', Type = 'character', Default = '>=', Switch = TRUE)
      SubsetList[['FilterLogic_2']] <- RemixAutoML::ReturnParam(input, VarName = 'FilterLogic_2', Type = 'character', Default = '>=', Switch = TRUE)
      SubsetList[['FilterLogic_3']] <- RemixAutoML::ReturnParam(input, VarName = 'FilterLogic_3', Type = 'character', Default = '>=', Switch = TRUE)
      SubsetList[['FilterLogic_4']] <- RemixAutoML::ReturnParam(input, VarName = 'FilterLogic_4', Type = 'character', Default = '>=', Switch = TRUE)
      SubsetList[['FilterValue_1a']] <- RemixAutoML::ReturnParam(input, VarName = 'FilterValue_1a', Type = 'character', Default = NULL, Switch = TRUE)
      SubsetList[['FilterValue_1b']] <- RemixAutoML::ReturnParam(input, VarName = 'FilterValue_1b', Type = 'character', Default = NULL, Switch = TRUE)
      SubsetList[['FilterValue_2a']] <- RemixAutoML::ReturnParam(input, VarName = 'FilterValue_2a', Type = 'character', Default = NULL, Switch = TRUE)
      SubsetList[['FilterValue_2b']] <- RemixAutoML::ReturnParam(input, VarName = 'FilterValue_2b', Type = 'character', Default = NULL, Switch = TRUE)
      SubsetList[['FilterValue_3a']] <- RemixAutoML::ReturnParam(input, VarName = 'FilterValue_3a', Type = 'character', Default = NULL, Switch = TRUE)
      SubsetList[['FilterValue_3b']] <- RemixAutoML::ReturnParam(input, VarName = 'FilterValue_3b', Type = 'character', Default = NULL, Switch = TRUE)
      SubsetList[['FilterValue_4a']] <- RemixAutoML::ReturnParam(input, VarName = 'FilterValue_4a', Type = 'character', Default = NULL, Switch = TRUE)
      SubsetList[['FilterValue_4b']] <- RemixAutoML::ReturnParam(input, VarName = 'FilterValue_4b', Type = 'character', Default = NULL, Switch = TRUE)
      assign(x = 'SubsetList', value = SubsetList, envir = .GlobalEnv)

    } else {

      # MetaData
      SubsetList[['RunNumber']] <- RunNumber + 1L
      SubsetList[['DataPrep']] <- FALSE

      # Group Variables
      if(!all(SubsetList[['SelectedGroupss']] == tryCatch({shiny::isolate(SelectedGroups())}, error = function(x) 'None'))) {
        SubsetList[['SelectedGroupss']] <- tryCatch({shiny::isolate(SelectedGroups())}, error = function(x) 'None')
        SubsetList[['DataPrep']] <- TRUE
      }
      if(!all(SubsetList[['GroupVars']] == RemixAutoML::ReturnParam(input, VarName = 'GroupVars', Type = 'character', Default = 'None', Switch = TRUE))) {
        SubsetList[['GroupVars']] <- RemixAutoML::ReturnParam(input, VarName = 'GroupVars', Type = 'character', Default = 'None', Switch = TRUE)
        SubsetList[['DataPrep']] <- TRUE
      }
      if(!all(SubsetList[['Levels_1']] == RemixAutoML::ReturnParam(input, VarName = 'Levels_1', Type = 'character', Default = 'None', Switch = TRUE))) {
        SubsetList[['Levels_1']] <- RemixAutoML::ReturnParam(input, VarName = 'Levels_1', Type = 'character', Default = 'None', Switch = TRUE)
        SubsetList[['DataPrep']] <- TRUE
      }
      if(!all(SubsetList[['Levels_2']] == RemixAutoML::ReturnParam(input, VarName = 'Levels_2', Type = 'character', Default = 'None', Switch = TRUE))) {
        SubsetList[['Levels_2']] <- RemixAutoML::ReturnParam(input, VarName = 'Levels_2', Type = 'character', Default = 'None', Switch = TRUE)
        SubsetList[['DataPrep']] <- TRUE
      }
      if(!all(SubsetList[['Levels_3']] == RemixAutoML::ReturnParam(input, VarName = 'Levels_3', Type = 'character', Default = 'None', Switch = TRUE))) {
        SubsetList[['Levels_3']] <- RemixAutoML::ReturnParam(input, VarName = 'Levels_3', Type = 'character', Default = 'None', Switch = TRUE)
        SubsetList[['DataPrep']] <- TRUE
      }
      if(!all(SubsetList[['FacetVar1']] == RemixAutoML::ReturnParam(input, VarName = 'FacetVar1', Type = 'character', Default = 'None', Switch = TRUE))) {
        SubsetList[['FacetVar1']] <- RemixAutoML::ReturnParam(input, VarName = 'FacetVar1', Type = 'character', Default = 'None', Switch = TRUE)
        SubsetList[['DataPrep']] <- TRUE
      }
      if(!all(SubsetList[['FacetVar2']] == RemixAutoML::ReturnParam(input, VarName = 'FacetVar2', Type = 'character', Default = 'None', Switch = TRUE))) {
        SubsetList[['FacetVar2']] <- RemixAutoML::ReturnParam(input, VarName = 'FacetVar2', Type = 'character', Default = 'None', Switch = TRUE)
        SubsetList[['DataPrep']] <- TRUE
      }
      if(!all(SubsetList[['SizeVar1']] == RemixAutoML::ReturnParam(input, VarName = 'SizeVar1', Type = 'character', Default = 'None', Switch = TRUE))) {
        SubsetList[['SizeVar1']] <- RemixAutoML::ReturnParam(input, VarName = 'SizeVar1', Type = 'character', Default = 'None', Switch = TRUE)
        SubsetList[['DataPrep']] <- TRUE
      }

      # Filter Variables
      if(!all(SubsetList[['FilterVariable_1']] == RemixAutoML::ReturnParam(input, VarName = 'FilterVariable_1', Type = 'character', Default = 'None', Switch = TRUE))) {
        SubsetList[['FilterVariable_1']] <- RemixAutoML::ReturnParam(input, VarName = 'FilterVariable_1', Type = 'character', Default = 'None', Switch = TRUE)
        SubsetList[['DataPrep']] <- TRUE
      }
      if(!all(SubsetList[['FilterVariable_2']] == RemixAutoML::ReturnParam(input, VarName = 'FilterVariable_2', Type = 'character', Default = 'None', Switch = TRUE))) {
        SubsetList[['FilterVariable_2']] <- RemixAutoML::ReturnParam(input, VarName = 'FilterVariable_2', Type = 'character', Default = 'None', Switch = TRUE)
        SubsetList[['DataPrep']] <- TRUE
      }
      if(!all(SubsetList[['FilterVariable_3']] == RemixAutoML::ReturnParam(input, VarName = 'FilterVariable_3', Type = 'character', Default = 'None', Switch = TRUE))) {
        SubsetList[['FilterVariable_3']] <- RemixAutoML::ReturnParam(input, VarName = 'FilterVariable_3', Type = 'character', Default = 'None', Switch = TRUE)
        SubsetList[['DataPrep']] <- TRUE
      }
      if(!all(SubsetList[['FilterVariable_4']] == RemixAutoML::ReturnParam(input, VarName = 'FilterVariable_4', Type = 'character', Default = 'None', Switch = TRUE))) {
        SubsetList[['FilterVariable_4']] <- RemixAutoML::ReturnParam(input, VarName = 'FilterVariable_4', Type = 'character', Default = 'None', Switch = TRUE)
        SubsetList[['DataPrep']] <- TRUE
      }
      if(!all(SubsetList[['FilterLogic_1']] == RemixAutoML::ReturnParam(input, VarName = 'FilterLogic_1', Type = 'character', Default = '>=', Switch = TRUE))) {
        SubsetList[['FilterLogic_1']] <- RemixAutoML::ReturnParam(input, VarName = 'FilterLogic_1', Type = 'character', Default = '>=', Switch = TRUE)
        SubsetList[['DataPrep']] <- TRUE
      }
      if(!all(SubsetList[['FilterLogic_2']] == RemixAutoML::ReturnParam(input, VarName = 'FilterLogic_2', Type = 'character', Default = '>=', Switch = TRUE))) {
        SubsetList[['FilterLogic_2']] <- RemixAutoML::ReturnParam(input, VarName = 'FilterLogic_2', Type = 'character', Default = '>=', Switch = TRUE)
        SubsetList[['DataPrep']] <- TRUE
      }
      if(!all(SubsetList[['FilterLogic_3']] == RemixAutoML::ReturnParam(input, VarName = 'FilterLogic_3', Type = 'character', Default = '>=', Switch = TRUE))) {
        SubsetList[['FilterLogic_3']] <- RemixAutoML::ReturnParam(input, VarName = 'FilterLogic_3', Type = 'character', Default = '>=', Switch = TRUE)
        SubsetList[['DataPrep']] <- TRUE
      }
      if(!all(SubsetList[['FilterLogic_4']] == RemixAutoML::ReturnParam(input, VarName = 'FilterLogic_4', Type = 'character', Default = '>=', Switch = TRUE))) {
        SubsetList[['FilterLogic_4']] <- RemixAutoML::ReturnParam(input, VarName = 'FilterLogic_4', Type = 'character', Default = '>=', Switch = TRUE)
        SubsetList[['DataPrep']] <- TRUE
      }
      if(!all(SubsetList[['FilterValue_1a']] == RemixAutoML::ReturnParam(input, VarName = 'FilterValue_1a', Type = 'character', Default = NULL, Switch = TRUE))) {
        SubsetList[['FilterValue_1a']] <- RemixAutoML::ReturnParam(input, VarName = 'FilterValue_1a', Type = 'character', Default = NULL, Switch = TRUE)
        SubsetList[['DataPrep']] <- TRUE
      }
      if(!all(SubsetList[['FilterValue_1b']] == RemixAutoML::ReturnParam(input, VarName = 'FilterValue_1b', Type = 'character', Default = NULL, Switch = TRUE))) {
        SubsetList[['FilterValue_1b']] <- RemixAutoML::ReturnParam(input, VarName = 'FilterValue_1b', Type = 'character', Default = NULL, Switch = TRUE)
        SubsetList[['DataPrep']] <- TRUE
      }
      if(!all(SubsetList[['FilterValue_2a']] == RemixAutoML::ReturnParam(input, VarName = 'FilterValue_2a', Type = 'character', Default = NULL, Switch = TRUE))) {
        SubsetList[['FilterValue_2a']] <- RemixAutoML::ReturnParam(input, VarName = 'FilterValue_2a', Type = 'character', Default = NULL, Switch = TRUE)
        SubsetList[['DataPrep']] <- TRUE
      }
      if(!all(SubsetList[['FilterValue_2b']] == RemixAutoML::ReturnParam(input, VarName = 'FilterValue_2b', Type = 'character', Default = NULL, Switch = TRUE))) {
        SubsetList[['FilterValue_2b']] <- RemixAutoML::ReturnParam(input, VarName = 'FilterValue_2b', Type = 'character', Default = NULL, Switch = TRUE)
        SubsetList[['DataPrep']] <- TRUE
      }
      if(!all(SubsetList[['FilterValue_3a']] == RemixAutoML::ReturnParam(input, VarName = 'FilterValue_3a', Type = 'character', Default = NULL, Switch = TRUE))) {
        SubsetList[['FilterValue_3a']] <- RemixAutoML::ReturnParam(input, VarName = 'FilterValue_3a', Type = 'character', Default = NULL, Switch = TRUE)
        SubsetList[['DataPrep']] <- TRUE
      }
      if(!all(SubsetList[['FilterValue_3b']] == RemixAutoML::ReturnParam(input, VarName = 'FilterValue_3b', Type = 'character', Default = NULL, Switch = TRUE))) {
        SubsetList[['FilterValue_3b']] <- RemixAutoML::ReturnParam(input, VarName = 'FilterValue_3b', Type = 'character', Default = NULL, Switch = TRUE)
        SubsetList[['DataPrep']] <- TRUE
      }
      if(!all(SubsetList[['FilterValue_4a']] == RemixAutoML::ReturnParam(input, VarName = 'FilterValue_4a', Type = 'character', Default = NULL, Switch = TRUE))) {
        SubsetList[['FilterValue_4a']] <- RemixAutoML::ReturnParam(input, VarName = 'FilterValue_4a', Type = 'character', Default = NULL, Switch = TRUE)
        SubsetList[['DataPrep']] <- TRUE
      }
      if(!all(SubsetList[['FilterValue_4b']] == RemixAutoML::ReturnParam(input, VarName = 'FilterValue_4b', Type = 'character', Default = NULL, Switch = TRUE))) {
        SubsetList[['FilterValue_4b']] <- RemixAutoML::ReturnParam(input, VarName = 'FilterValue_4b', Type = 'character', Default = NULL, Switch = TRUE)
        SubsetList[['DataPrep']] <- TRUE
      }
      assign(x = 'SubsetList', value = SubsetList, envir = .GlobalEnv)
    }

    # Check Logic before proceeding
    x1 <- tryCatch({input[['YMin']]}, error = function(x) NULL)
    x2 <- tryCatch({SubsetList[['FilterLogic_1']]}, error = function(x) NULL)
    x3 <- tryCatch({SubsetList[['SelectedGroupss']]}, error = function(x) NULL)
    x4 <- tryCatch({input[['PlotType']]}, error = function(x) NULL)
    if(Debug) {print(x1); print(x2); print(x3); print(x4)}
    if(any(is.null(x1), is.null(x2), is.null(x3), is.null(x4))) {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = 'You need to expand each dropdown menu at least once to initialize variables before creating plots. I am too lazy to predefine args, currently', type = NULL, btn_labels = "error", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else if(input[['PlotType']] %chin% 'Line' && any(class(data[[eval(shiny::isolate(DateVar()))]]) %chin% c('numeric','integer','factor','character','logical','integer64', 'NULL'))) {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = "X-Variable needs to be a Date, IDate, or Posix type", type = NULL, btn_labels = "error", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else if(input[['PlotType']] %chin% c('Scatter','Copula') && !any(class(data[[eval(shiny::isolate(XVar()))]]) %chin% c('numeric','integer'))) {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = "X-Variable needs to be a numeric or integer variable", type = NULL, btn_labels = "error", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {

      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
      # Prepare data for plotting            ----
      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

      # Get SelectedGroupss set up correctly (allowed to persist for error checking with x1, x2, x3, x4)
      if(Debug) print('Get SelectedGroupss set up correctly')
      if(length(SubsetList[['SelectedGroupss']]) > 0) {
        if(length(SubsetList[['SelectedGroupss']]) == 1 && SubsetList[['SelectedGroupss']] == 'None') {
          SubsetList[['SelectedGroupss']] <- NULL
        } else if('None' %in% SubsetList[['SelectedGroupss']]) {
          SubsetList[['SelectedGroupss']] <- SubsetList[['SelectedGroupss']][!SubsetList[['SelectedGroupss']] %in% 'None']
        }
      } else {
        SubsetList[['SelectedGroupss']] <- NULL
      }

      # DataPrep
      if(Debug) for(zzzz in 1:4) print(':: :: DataPrep :: ::')
      if(!SubsetList[['DataPrep']]) {
        data1 <- data
      } else {
        if(Debug) print('remove NA')
        if(shiny::isolate(YVar()) != 'None') {
          data1 <- data[!is.na(get(shiny::isolate(YVar())))]
        } else {
          data1 <- data
        }

        # Filter by Date
        if(Debug) print('Filter by Date')
        if(shiny::isolate(DateVar()) != 'None') {
          data1 <- data1[get(shiny::isolate(DateVar())) <= eval(input[['DateMax']]) & get(shiny::isolate(DateVar())) >= eval(input[['DateMin']])]
        }

        # Subset by FilterVariable_1
        if(Debug) print('Subset by FilterVariable_1')
        if(SubsetList[['FilterVariable_1']] != 'None') {
          fv <- SubsetList[['FilterValue_1b']]
          data1 <- RemixAutoML::FilterLogicData(data1, input, FilterLogic=SubsetList[['FilterLogic_1']], FilterVariable=SubsetList[['FilterVariable_1']], FilterValue=SubsetList[['FilterValue_1a']], FilterValue2=fv, Debug = Debug)
          if(Debug) print(data1)
        }

        # Subset by FilterVariable_2
        if(Debug) print('Subset by FilterVariable_2')
        if(SubsetList[['FilterVariable_2']] != 'None') {
          fv <- FilterValue_2b
          data1 <- RemixAutoML::FilterLogicData(data1, input, FilterLogic=SubsetList[['FilterLogic_2']], FilterVariable=SubsetList[['FilterVariable_2']], FilterValue=SubsetList[['FilterValue_2a']], FilterValue2=fv)
        }

        # Subset by FilterVariable_3
        if(Debug) print('Subset by FilterVariable_3')
        if(SubsetList[['FilterVariable_3']] != 'None') {
          fv <- SubsetList[['FilterValue_3b']]
          data1 <- RemixAutoML::FilterLogicData(data1, input, FilterLogic=SubsetList[['FilterLogic_3']], FilterVariable=SubsetList[['FilterVariable_3']], FilterValue=SubsetList[['FilterValue_3a']], FilterValue2=fv)
        }

        # Subset by FilterVariable_4
        if(Debug) {print('Subset by FilterVariable_4')}
        if(SubsetList[['FilterVariable_4']] != 'None') {
          fv <- SubsetList[['FilterValue_4b']]
          data1 <- RemixAutoML::FilterLogicData(data1, input, FilterLogic=SubsetList[['FilterLogic_4']], FilterVariable=SubsetList[['FilterVariable_4']], FilterValue=SubsetList[['FilterValue_4a']], FilterValue2=fv)
        }

        # Subset Rows based on Filters
        if(Debug) {print('  Checking SelectedGroupss, Levels_1, Levels_2, and Levels_3'); print(SubsetList[['SelectedGroupss']]); print(SubsetList[['Levels_1']]); print(SubsetList[['Levels_2']]); print(SubsetList[['Levels_3']])}
        if(Debug) {print('  Checking YVar(), XVar(), and DateVar()'); print(shiny::isolate(YVar())); print(shiny::isolate(XVar())); print(shiny::isolate(DateVar()))}
        data1 <- RemixAutoML::PreparePlotData(
          SubsetOnly = if(input[['PlotType']] %chin% c('BoxPlot','ViolinPlot','Scatter','Copula','Train_ParDepPlots','Test_ParDepPlots','Train_ParDepBoxPlots','Test_ParDepBoxPlots','Test__EvaluationPlot','Train_EvaluationPlot','Test_EvaluationBoxPlot','Train_EvaluationBoxPlot')) TRUE else FALSE,
          input, data = data1, Aggregate = 'mean', TargetVariable = shiny::isolate(YVar()),
          DateVariable = if(shiny::isolate(DateVar()) == 'None') NULL else shiny::isolate(DateVar()), GroupVariables = SubsetList[['SelectedGroupss']],
          G1Levels = 'Levels_1', G2Levels = 'Levels_2', G3Levels = 'Levels_3', Debug = Debug)
      }

      # Rebuild Model Eval Plots; # Rebuild needs ScoreVar to not be null, # Rebuild if PDP Var in names(dt)
      # Rebuild if Percentile_Buckets changed from default, # Rebuild if Subsetting is desired
      # x4:Rebuild if PDP_Variable not in names of PlotList
      if(Debug) {print(data1); print(SubsetList[['SelectedGroupss']])}
      x1 <- !is.null(ScoreVar)
      x2 <- input$PDP_Variable %in% names(data1)
      x3 <- input$Percentile_Buckets != 20
      x4 <- !is.null(SubsetList[['SelectedGroupss']]) && (!is.null(RemixAutoML::CharNull(SubsetList[['Levels_1']])) || !is.null(RemixAutoML::CharNull(SubsetList[['Levels_2']])) || !is.null(RemixAutoML::CharNull(SubsetList[['Levels_3']])))
      x5 <- any(c('Test_ParDepPlots','Train_ParDepPlots','Test_ParDepBoxPlots','Train_ParDepBoxPlots','Test_EvaluationPlot','Train_EvaluationPlot','Test_EvaluationBoxPlot','Train_EvaluationBoxPlot','Test_GainsPlot','Train_GainsPlot','Test_LiftPlot','Train_LiftPlot','Test_ScatterPlot','Train_ScatterPlot','Test_CopulaPlot','Train_CopulaPlot','Test_ResidualsHistogram','Train_ResidualsHistogram') %in% input$PlotType)
      Blocker <- !x1 || (!x2 && input$PlotType %in% c('Test_ParDepPlots','Train_ParDepPlots','Test_ParDepBoxPlots','Train_ParDepBoxPlots'))
      if(x5 || x4 || (x3 && input$PlotType %in% c('Test_ParDepPlots','Train_ParDepPlots','Test_ParDepBoxPlots','Train_ParDepBoxPlots'))) {
        if(Blocker) Rebuild <- FALSE else Rebuild <- TRUE
      } else {
        Rebuild <- FALSE
      }

      # ----

      # ----

      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
      # BoxPlot, ViolinPlot, Line, Scatter, Copula ----
      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
      if(Debug) {print('Create Plot Object'); print(input[['PlotType']])}
      if(input[['PlotType']] %chin% c('BoxPlot', 'ViolinPlot', 'Bar', 'Line', 'Scatter', 'Copula')) {

        # Create Plot
        if(Debug) {
          print(paste0('PlotType print: ', input[['PlotType']]))
          print(input[['PlotType']] %chin% c('BoxPlot', 'ViolinPlot', 'Bar', 'Line', 'Scatter', 'Copula'))
        }

        # XVar
        if(shiny::isolate(XVar()) == 'None') {
          if(shiny::isolate(DateVar()) == 'None') {
            xvar <- NULL
          } else {
            xvar <- shiny::isolate(DateVar())
          }
        } else {
          xvar <- shiny::isolate(XVar())
        }

        # AutoPlotter()
        if(Debug) print('Run AutoPlotter')
        p1 <- RemixAutoML:::AutoPlotter(
          dt = data1,
          PlotType = input[['PlotType']],
          YVar = shiny::isolate(YVar()),
          YMin = input[['YMin']], YMax = input[['YMax']],
          XVar = xvar,
          XMin = input[['XMin']], XMax = input[['XMax']],
          ColorVariables = SubsetList[['SelectedGroupss']], SizeVar1 = SubsetList[['SizeVar1']],
          FacetVar1 = SubsetList[['FacetVar1']], FacetVar2 = SubsetList[['FacetVar2']],
          YTicks = YTicks, XTicks = XTicks,
          OutlierSize = OutlierSize, OutlierColor = OutlierColor, BoxPlotFill = BoxPlotFill,
          GamFitScatter = input[['GamFitScatter']],
          TextSize = TextSize, TextColor = TextColor,
          AngleX = AngleX, AngleY = AngleY, ChartColor = ChartColor, BorderColor = BorderColor,
          GridColor = GridColor, BackGroundColor = BackGroundColor, Debug = Debug)

      } else if(!input[['PlotType']] %chin% c('BoxPlot', 'ViolinPlot', 'Bar', 'Line', 'Scatter', 'Copula')) {

        # AppModelInsights()
        if(Debug) print(paste0('PDPVar = ', input[['PDP_Variable']]))
        p1 <- RemixAutoML:::AppModelInsights(
          dt = data1,
          PlotType = input[['PlotType']],
          ModelOutputList = ModelOutputList,
          TargetVar = shiny::isolate(YVar()),
          PredictVar = if(shiny::isolate(ScoreVar()) != 'None') shiny::isolate(ScoreVar()) else NULL,
          PDPVar = if(!is.null(input[['PDP_Variable']])) input[['PDP_Variable']] else NULL,
          DateVar = if(shiny::isolate(DateVar()) != 'None') shiny::isolate(DateVar()) else NULL,
          GamFit = input[['GamFitScatter']],
          Buckets = as.numeric(input[['Percentile_Buckets']]),
          Rebuild = Rebuild, Debug = Debug)
        if(!is.null(p1)) {
          p1 <- p1 + RemixAutoML::ChartTheme(
            Size = TextSize, AngleX = AngleX, AngleY = AngleY, ChartColor = ChartColor,
            BorderColor = BorderColor, TextColor = TextColor, GridColor = GridColor,
            BackGroundColor = BackGroundColor, SubTitleColor = SubTitleColor,
            LegendPosition = LegendPosition, LegendBorderSize = as.numeric(LegendBorderSize),
            LegendLineType = LegendLineType)
        }
      }

      # ----

      # ----

      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
      # Return Plot to UI                          ----
      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
      if(Debug) {print('Return Plot to UI'); print(exists('p1'))}
      if(exists('p1')) {
        if(Debug) print('Convert p1 to global env')
        data1 <<- data1
        p1 <<- p1
        output$Trend <- shiny::renderPlot(width = PlotWidth, height = PlotHeight, {
          if(Debug) print('Create Plot output$Trend')
          p1
        })
      }
    }
  })

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Close app after closing browser            ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  session$onSessionEnded(function() {
    stopApp()
  })
}

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# Run App                              ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
shiny::shinyApp(ui = ui, server = server)
