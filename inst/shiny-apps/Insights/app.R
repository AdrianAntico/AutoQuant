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
# Create ui                            ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
ui <- shinydashboard::dashboardPage(
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
  # Contents of side bar menu            ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  shinydashboard::dashboardSidebar(

    # Needed for authentication-related activities
    shinyjs::useShinyjs(),

    # ----

    # ----

    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    # sidebarMenu                          ----
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
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
  # dashboardBody                        ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  shinydashboard::dashboardBody(

    # ----

    # ----

    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    # tabItems                             ----
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
            width = AppWidth,
            shinyjs::useShinyjs(),
            shinydashboard::box(
              title = htmltools::tagList(shiny::icon('filter', lib = 'font-awesome'), 'Login Panel'),
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = FALSE,
              background = GroupVarsBoxColor,
              width = 9L,
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
            width = 3L,
            shinyjs::useShinyjs(),
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
        # shiny::fluidRow(
        #   shiny::column(
        #     width = 3L,
        #     shinyjs::useShinyjs(),
        #     shinyWidgets::actionBttn(
        #       inputId = 'LoadDataPage_2_LoginPage',
        #       label = 'Login Page',
        #       icon = shiny::icon('chevron-right', lib = 'font-awesome'),
        #       style = 'gradient',
        #       color = eval(CreatePlotButtonColor)))),
        #
        # # Add Space
        # RemixAutoML::BlankRow(AppWidth),

        # Loaders
        shiny::fluidRow(
          shiny::column(
            width = AppWidth,
            shinyjs::useShinyjs(),
            shinydashboard::box(
              title = htmltools::tagList(shiny::icon('filter', lib = 'font-awesome'), 'Loading Objects'),
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = FALSE,
              background = VarsBoxColor,
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
            width = 3L,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = 'LoadDataButton',
              label = 'Load Objects',
              icon = shiny::icon('chevron-right', lib = 'font-awesome'),
              style = 'gradient',
              color = eval(CreatePlotButtonColor)))
          # shiny::column(
          #   width = 3L,
          #   shinyjs::useShinyjs(),
          #   shinyWidgets::actionBttn(
          #     inputId = 'LoadDataPage_2_PlotterPage',
          #     label = 'Plotting Page',
          #     icon = shiny::icon('chevron-right', lib = 'font-awesome'),
          #     style = 'gradient',
          #     color = eval(CreatePlotButtonColor)))
          )),

        # ----

        # ----

      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
      # Plotter Page                         ----
      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
      shinydashboard::tabItem(

        # -- TAB REFERENCE VALUE
        tabName = "Plotter",

        # # Add Space
        # RemixAutoML::BlankRow(AppWidth),
        #
        # # Button to go home
        # shiny::fluidRow(
        #   shiny::column(
        #     width = 3L,
        #     shinyjs::useShinyjs(),
        #     shinyWidgets::actionBttn(
        #       inputId = 'PlotterPage_2_LoadDataPage',
        #       label = 'Load Data Page',
        #       icon = shiny::icon('chevron-right', lib = 'font-awesome'),
        #       style = 'gradient',
        #       color = eval(CreatePlotButtonColor)))),
        #
        # # Add Space
        # RemixAutoML::BlankRow(AppWidth),

        # ----

        # ----

        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
        # Variables                            ----
        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
        shiny::fluidRow(
          shiny::column(
            width = AppWidth,
            shinyjs::useShinyjs(),
            shinydashboard::box(
              title = htmltools::tagList(shiny::icon('filter', lib = 'font-awesome'), 'Plotting Variables'),
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = FALSE,
              background = 'navy',
              width = AppWidth,
              shiny::fluidRow(
                width = AppWidth,
                shiny::column(3L, shiny::uiOutput('YVar')),
                shiny::column(3L, shiny::uiOutput('YMin')),
                shiny::column(3L, shiny::uiOutput('YMax'))),
              shiny::fluidRow(
                width = AppWidth,
                shiny::column(3L, shiny::uiOutput('XVar')),
                shiny::column(3L, shiny::uiOutput('XMin')),
                shiny::column(3L, shiny::uiOutput('XMax'))),
              shiny::fluidRow(
                width = AppWidth,
                shiny::column(3L, shiny::uiOutput('DateVar')),
                shiny::column(3L, shiny::uiOutput('DateMin')),
                shiny::column(3L, shiny::uiOutput('DateMax'))),
              shiny::fluidRow(
                width = AppWidth,
                shiny::column(3L, shiny::uiOutput('ScoreVar')))),

            # Add Space
            RemixAutoML::BlankRow(AppWidth),

            # GroupVars,
            shinydashboard::box(
              title = htmltools::tagList(shiny::icon('filter', lib = 'font-awesome'), 'Plotting Variables'),
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = FALSE,
              background = VarsBoxColor,
              width = AppWidth,
              shiny::fluidRow(
                width = AppWidth,
                shiny::column(
                  width = 3L,
                  tags$h3('Grouping Variables'),
                  shinyWidgets::dropdown(
                    animate = TRUE,
                    right = FALSE,
                    shiny::fluidRow(
                      width = AppWidth,
                      shiny::column(3L, shiny::uiOutput('GroupVars')),
                      shiny::column(3L, shiny::uiOutput('FacetVar1')),
                      shiny::column(3L, shiny::uiOutput('FacetVar2')),
                      shiny::column(3L, shiny::uiOutput('SizeVar1'))),
                    shiny::fluidRow(
                      width = AppWidth,
                      shiny::column(
                        width = 3L,
                        shiny::conditionalPanel(
                          width = 3L,
                          condition = "length(input['GroupVars']) >= 1",
                          shiny::uiOutput('Levels_1'))),

                      # GroupVar2 level selection
                      shiny::column(
                        width = 3L,
                        shiny::conditionalPanel(
                          width = 3L,
                          condition = "length(input['GroupVars']) >= 2",
                          shiny::uiOutput('Levels_2'))),

                      # GroupVar3 level selection
                      shiny::column(
                        width = 3L,
                        shiny::conditionalPanel(
                          width = 3L,
                          condition = "length(input['GroupVars']) >= 3",
                          shiny::uiOutput('Levels_3')))),
                    shiny::fluidRow(
                      width = AppWidth,
                      shiny::column(3L, shiny::uiOutput('NumberGroupsDisplay'))),
                    circle = FALSE, tooltip = FALSE, status = "primary",
                    inputId = "GroupVariablesUI",
                    icon = icon("gear"), width = LogoWidth)),

                # Filter Vars
                shiny::column(
                  width = 3L,
                  tags$h3('Data Filtering'),
                  shinyWidgets::dropdown(
                    animate = TRUE,
                    right = FALSE,
                    shiny::fluidRow(
                      width = AppWidth,
                      shiny::column(3L, shiny::uiOutput('FilterVariable_1')),
                      shiny::column(3L, shiny::uiOutput('FilterLogic_1')),
                      shiny::column(3L, shiny::uiOutput('FilterValue_1a')),
                      shiny::column(3L, shiny::uiOutput('FilterValue_1b'))),
                    shiny::fluidRow(
                      width = AppWidth,
                      shiny::column(3L, shiny::uiOutput('FilterVariable_2')),
                      shiny::column(3L, shiny::uiOutput('FilterLogic_2')),
                      shiny::column(3L, shiny::uiOutput('FilterValue_2a')),
                      shiny::column(3L, shiny::uiOutput('FilterValue_2b'))),
                    shiny::fluidRow(
                      width = AppWidth,
                      shiny::column(3L, shiny::uiOutput('FilterVariable_3')),
                      shiny::column(3L, shiny::uiOutput('FilterLogic_3')),
                      shiny::column(3L, shiny::uiOutput('FilterValue_3a')),
                      shiny::column(3L, shiny::uiOutput('FilterValue_3b'))),
                    shiny::fluidRow(
                      width = AppWidth,
                      shiny::column(3L, shiny::uiOutput('FilterVariable_4')),
                      shiny::column(3L, shiny::uiOutput('FilterLogic_4')),
                      shiny::column(3L, shiny::uiOutput('FilterValue_4a')),
                      shiny::column(3L, shiny::uiOutput('FilterValue_4b'))),
                    circle = FALSE, tooltip = FALSE, status = "primary",
                    inputId = "DataFiltering",
                    icon = icon("gear"), width = LogoWidth)),

                # Plot Formatting bars
                shiny::column(
                  width = 3L,
                  tags$h3('Plot Formatting'),
                  shinyWidgets::dropdown(
                    animate = TRUE,
                    right = FALSE,
                    shiny::fluidRow(
                      width = AppWidth,
                      shiny::column(3L, shiny::uiOutput('PlotWidth')),
                      shiny::column(3L, shiny::uiOutput('PlotHeight')),
                      shiny::column(3L, shiny::uiOutput('XTicks')),
                      shiny::column(3L, shiny::uiOutput('YTicks'))),
                    shiny::fluidRow(
                      width = AppWidth,
                      shiny::column(3L, shiny::uiOutput('AngleY')),
                      shiny::column(3L, shiny::uiOutput('TextSize')),
                      shiny::column(3L, shiny::uiOutput('TextColor')),
                      shiny::column(3L, shiny::uiOutput('ChartColor'))),
                    shiny::fluidRow(
                      width = AppWidth,
                      shiny::column(3L, shiny::uiOutput('AngleX')),
                      shiny::column(3L, shiny::uiOutput('GridColor')),
                      shiny::column(3L, shiny::uiOutput('BackGroundColor')),
                      shiny::column(3L, shiny::uiOutput('BorderColor'))),
                    circle = FALSE, tooltip = FALSE, status = "primary",
                    inputId = "PlotInputsUI",
                    icon = icon("gear"), width = LogoWidth)))))),

        # Add Space
        RemixAutoML::BlankRow(AppWidth),

        # ----

        # ----

        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
        # Plot Inputs                          ----
        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

        # shiny::fluidRow(
        #   shiny::column(
        #     width = AppWidth,
        #     shinyjs::useShinyjs(),
        #     shinydashboard::box(
        #       title = htmltools::tagList(shiny::icon('filter', lib = 'font-awesome'), 'Plot Options'),
        #       solidHeader = TRUE,
        #       collapsible = TRUE,
        #       collapsed = FALSE,
        #       background = PlotBoxColor,
        #       width = AppWidth,
        #       shiny::fluidRow(
        #         shiny::column(
        #           width = 3L,
        #           tags$h3('Plotting Options'),
        #           shinyWidgets::dropdown(
        #             animate = TRUE,
        #             right = FALSE,
        #             shiny::fluidRow(
        #               width = AppWidth,
        #               shiny::column(3L, shiny::uiOutput('PlotWidth')),
        #               shiny::column(3L, shiny::uiOutput('PlotHeight')),
        #               shiny::column(3L, shiny::uiOutput('XTicks')),
        #               shiny::column(3L, shiny::uiOutput('YTicks'))),
        #             shiny::fluidRow(
        #               width = AppWidth,
        #               shiny::column(3L, shiny::uiOutput('AngleY')),
        #               shiny::column(3L, shiny::uiOutput('TextSize')),
        #               shiny::column(3L, shiny::uiOutput('TextColor')),
        #               shiny::column(3L, shiny::uiOutput('ChartColor'))),
        #             shiny::fluidRow(
        #               width = AppWidth,
        #               shiny::column(3L, shiny::uiOutput('AngleX')),
        #               shiny::column(3L, shiny::uiOutput('GridColor')),
        #               shiny::column(3L, shiny::uiOutput('BackGroundColor')),
        #               shiny::column(3L, shiny::uiOutput('BorderColor'))),
        #             circle = FALSE, tooltip = FALSE, status = "primary",
        #             inputId = "PlotInputsUI",
        #             icon = icon("gear"), width = LogoWidth)))))),

        # Add Space
        RemixAutoML::BlankRow(AppWidth),

        # Core Plot Inputs
        shiny::fluidRow(
          shiny::column(
            width = AppWidth,
            shinyjs::useShinyjs(),
            shinydashboard::box(
              title = htmltools::tagList(shiny::icon('filter', lib = 'font-awesome'), 'Plot Options'),
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = FALSE,
              background = PlotBoxColor,
              width = AppWidth,
              shiny::fluidRow(
                width = AppWidth,
                shiny::column(3L, shiny::uiOutput('GamFitScatter')),
                shiny::column(3L, shiny::uiOutput('PDP_Variable')),
                shiny::column(3L, shiny::uiOutput('Percentile_Buckets'))),
              shiny::fluidRow(
                width = AppWidth,
                shiny::column(3L, shiny::uiOutput('PlotType')))))),

        # # Add Space
        RemixAutoML::BlankRow(AppWidth),
        RemixAutoML::BlankRow(AppWidth),

        # ----

        # ----

        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
        # Buttons to build plot                ----
        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
        shiny::fluidRow(

          # Create Plot!
          shiny::column(
            width = 3L,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = 'TrendPlotExecute',
              label = 'Create Plot!',
              icon = shiny::icon('chevron-right', lib = 'font-awesome'),
              style = 'gradient',
              color = eval(CreatePlotButtonColor))),

          # Update Theme!
          shiny::column(
            width = 3L,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = 'UpdatePlotThemeElements',
              label = 'Update Theme!',
              icon = shiny::icon('chevron-right', lib = 'font-awesome'),
              style = 'gradient',
              color = eval(UpdatePlotButtonColor))),

          # Reset Theme!
          shiny::column(
            width = 3L,
            shinyjs::useShinyjs(),
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
# Server Function                      ----
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
    data <<- RemixAutoML::ReactiveLoadCSV(input, InputVal = "DataLoad", ProjectList = NULL, DateUpdateName = NULL, RemoveObjects = NULL)
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
    # 'Y-Variable'
    RemixAutoML::PickerInput(InputID = 'YVar', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Y-Variable'), Choices = c('None', names(data)), SelectedDefault = YVariable, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$XVar <- shiny::renderUI({
    # 'X-Variable'
    RemixAutoML::PickerInput(InputID = 'XVar', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'X-Variable'), Choices = c('None', names(data)), SelectedDefault = XVariable, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$ScoreVar <- shiny::renderUI({
    # 'Score-Variable'
    RemixAutoML::PickerInput(InputID = 'ScoreVar', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Scoring-Variable'), Choices = c('None', names(data)), SelectedDefault = XVariable, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$DateVar <- shiny::renderUI({
    # 'Date Variable'
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
    RemixAutoML::PickerInput(InputID = 'PlotType', Label = tags$span(style=paste0('color: ', AppTextColor, ';'), 'Plot Type'), Choices = c('BoxPlotTS','ViolinPlotTS','Line','Scatter','Copula', MONames), SelectedDefault = 'box', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
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
    RemixAutoML::NumericInput(InputID = 'NumberGroupsDisplay', Label = tags$span(style='color: blue;', '# of Levels'), Step = 1L, Value = 5L, Min = 1L, Max = 100L)
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
    Uniques <- tryCatch({data[, unique(get(XVar()))]}, error = function(x) NULL)
    if(!is.null(Uniques)) {
      if(input[['PlotType']] %chin% c('BoxPlotTS','ViolinPlotTS','Line')) {
        choices <- c('Default', '1 year', '1 day', '3 day', '1 week', '2 week', '1 month', '3 month', '6 month', '2 year', '5 year', '10 year', '1 minute', '15 minutes', '30 minutes', '1 hour', '3 hour', '6 hour', '12 hour')
        default <- '1 year'
      } else if(any(class(data[[eval(XVar())]]) %in% c('numeric','integer')) && length(Uniques) > 10L) {
        choices <- choices <- c('Default', 'Percentiles', 'Every 5th percentile', 'Deciles', 'Quantiles', 'Quartiles', as.character(data[, quantile(round(get(XVar()), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]))
      } else {
        choices <- c('Default', Uniques)
      }
    } else {
      choices <- 'Default'
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

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Group Variables                      ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  # Select GroupVars
  output$GroupVars <- shiny::renderUI({
    if(Debug) print('PickerInput GroupVars')
    RemixAutoML::PickerInput(
      InputID = 'GroupVars',
      Label = tags$span(style='color: blue;', 'Select Group Variables'),
      Choices = c('None', names(data)),
      SelectedDefault = if(!is.null(GroupVariables)) GroupVariables else 'None',
      SelectedText = 'count > 1',
      Multiple = TRUE,
      ActionBox = TRUE)
  })

  # Reactive Group Variables
  SelectedGroups <- shiny::reactive({
    RemixAutoML::ReturnParam(
      input,
      VarName = 'GroupVars',
      Default = if(!is.null(GroupVariables)) GroupVariables else 'None',
      Switch = TRUE,
      Type = 'character')
  })

  # Levels
  output$Levels_1 <- shiny::renderUI({
    if(Debug) print('PickerInput_GetLevels 1')
    if(Debug) print(SelectedGroups())
    if(Debug) print('here')
    if(Debug) print(RemixAutoML::UniqueLevels(input, data, 1L, GroupVars=SelectedGroups()))
    if(Debug) print('here 1')
    RemixAutoML::PickerInput_GetLevels(
      input,
      data = 'data',
      NumGroupVar = 1L,
      InputID = 'Levels_1',
      InputID2 = 'GroupVars',
      Choices = RemixAutoML::UniqueLevels(input, data, 1L, GroupVars=SelectedGroups()),
      SelectedDefault = NULL,
      Size = 9,
      SelectedText = 'count > 1',
      Multiple = TRUE,
      ActionBox = TRUE)
  })

  # Levels
  output$Levels_2 <- shiny::renderUI({
    if(Debug) print('PickerInput_GetLevels 2')
    RemixAutoML::PickerInput_GetLevels(
      input,
      data = 'data',
      NumGroupVar = 2L,
      InputID = 'Levels_2',
      InputID2 = 'GroupVars',
      Choices = RemixAutoML::UniqueLevels(input, data, 2L, GroupVars=SelectedGroups()),
      SelectedDefault = NULL,
      Size = 9,
      SelectedText = 'count > 1',
      Multiple = TRUE,
      ActionBox = TRUE)
  })

  # Levels
  output$Levels_3 <- shiny::renderUI({
    if(Debug) print('PickerInput_GetLevels 3')
    RemixAutoML::PickerInput_GetLevels(
      input,
      data = 'data',
      NumGroupVar = 3L,
      InputID = 'Levels_3',
      InputID2 = 'GroupVars',
      Choices = RemixAutoML::UniqueLevels(input, data, 3L, GroupVars=SelectedGroups()),
      SelectedDefault = NULL,
      Size = 10,
      SelectedText = "count > 1",
      Multiple = TRUE,
      ActionBox = TRUE)
  })

  # Faceting
  output$FacetVar1 <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'FacetVar1', Label = tags$span(style='color: blue;', 'Facet Variable 1'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$FacetVar2 <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'FacetVar2', Label = tags$span(style='color: blue;', 'Facet Variable 2'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  # Coloring
  output$ColorVar1 <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'ColorVar1', Label = tags$span(style='color: blue;', 'Color Variable'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
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
    shiny::selectInput(inputId='FilterVariable_1', label = tags$span(style='color: blue;', 'Filter Variable'), choices=c('None', names(data)), selected='None')
  })

  # Filter Variable 2
  output$FilterVariable_2 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterVariable_2', label = tags$span(style='color: blue;', 'Filter Variable'), choices=c('None', names(data)), selected='None')
  })

  # Filter Variable 3
  output$FilterVariable_3 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterVariable_3', label = tags$span(style='color: blue;', 'Filter Variable'), choices=c('None', names(data)), selected='None')
  })

  # Filter Variable 4
  output$FilterVariable_4 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterVariable_4', label = tags$span(style='color: blue;', 'Filter Variable'), choices=c('None', names(data)), selected='None')
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

    if(Debug) {
      print(!exists('p1') || !exists('data1'))
      print(!exists('p1'))
      print(!exists('data1'))
    }

    if(!exists('p1') || !exists('data1')) {

      shinyWidgets::sendSweetAlert(session, title = NULL, text = "Try Create Plot", type = NULL, btn_labels = "error", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")

    } else {

      # Update chart theme elements
      p1 <- shiny::isolate(p1 + RemixAutoML::ChartTheme(
        Size = 12,
        AngleX = 90,
        AngleY = 0,
        ChartColor = 'lightsteelblue1',
        BorderColor = 'darkblue',
        TextColor = 'darkblue',
        GridColor = 'white',
        BackGroundColor = 'gray95') +
          ggplot2::theme(legend.title = ggplot2::element_blank()))

      # Update labels
      if(input[['PlotType']] %chin% c('BoxPlotTS','ViolinTS')) {
        if(!'Default' %in% input[['XTicks']]) p1 <- p1 + suppressMessages(ggplot2::scale_x_date(date_breaks = input[['XTicks']]))
        p1 <- p1 + ggplot2::labs(
          title = paste0(shiny::isolate(input[['PlotType']]), ' Plot'),
          subtitle = 'Blue line = mean(Y)',
          caption = 'by RemixAutoML') +
          ggplot2::ylim(as.numeric(eval(input[['YMin']])), as.numeric(eval(input[['YMax']]))) +
          ggplot2::ylab(shiny::isolate(YVar())) + ggplot2::xlab(shiny::isolate(DateVar()))
      } else if(input[['PlotType']] %chin% c('Line')) {
        if(!'Default' %in% input[['XTicks']]) {
          p1 <- p1 + suppressMessages(ggplot2::scale_x_date(date_breaks = input[['XTicks']]))
        }
        p1 <- p1 + ggplot2::labs(
          title = paste0(input[['PlotType']], ' Plot'),
          caption = 'by RemixAutoML') +
          ggplot2::ylim(as.numeric(eval(input[['YMin']])), as.numeric(eval(input[['YMax']]))) +
          ggplot2::ylab(shiny::isolate(YVar())) + ggplot2::xlab(shiny::isolate(DateVar()))
      } else if(input[['PlotType']] %chin% c('Scatter','Copula')) {

        # Labs
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

    if(!exists('p1') || !exists('data1')) {

      shinyWidgets::sendSweetAlert(session, title = NULL, text = "Try Create Plot", type = NULL, btn_labels = "error", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")

    } else {

      # Update chart theme elements
      p1 <- shiny::isolate(p1 + RemixAutoML::ChartTheme(
        Size = input[['TextSize']],
        AngleX = input[['AngleX']],
        AngleY = input[['AngleY']],
        ChartColor = input[['ChartColor']],
        BorderColor = input[['BorderColor']],
        TextColor = input[['TextColor']],
        GridColor = input[['GridColor']],
        BackGroundColor = input[['BackGroundColor']]) +
          ggplot2::theme(legend.title = ggplot2::element_blank()))

      # Update labels
      if(shiny::isolate(input[['PlotType']] %chin% c('BoxPlotTS','ViolinPlotTS'))) {
        if(!'Default' %in% input[['XTicks']]) p1 <- p1 + suppressMessages(ggplot2::scale_x_date(date_breaks = input[['XTicks']]))
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
    x1 <- tryCatch({input[['YMin']]}, error = function(x) NULL)
    x2 <- tryCatch({input[['FilterLogic_1']]}, error = function(x) NULL)
    x3 <- tryCatch({SelectedGroups()}, error = function(x) NULL)
    x4 <- tryCatch({input[['PlotType']]}, error = function(x) NULL)
    if(Debug) {print(x1); print(x2); print(x3); print(x4)}
    if(any(is.null(x1), is.null(x2), is.null(x3), is.null(x4))) {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = 'You need to expand each dropdown menu at least once to initialize variables before creating plots. I am too lazy to predefine args, currently', type = NULL, btn_labels = "error", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {

      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
      # Prepare data for plotting            ----
      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

      # Remove NA's
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
      if(input[['FilterVariable_1']] != 'None') {
        fv <- input[['FilterValue_1b']]
        if(Debug) print(fv)
        if(Debug) print(input[['FilterLogic_1']])
        if(Debug) print(data1)
        data1 <- RemixAutoML::FilterLogicData(data1, input, FilterLogic=input[['FilterLogic_1']], FilterVariable=input[['FilterVariable_1']], FilterValue=input[['FilterValue_1a']], FilterValue2=fv, Debug = Debug)
        if(Debug) print(data1)
      }

      # Subset by FilterVariable_2
      if(Debug) print('Subset by FilterVariable_2')
      if(input[['FilterVariable_2']] != 'None') {
        fv <- input[['FilterValue_2b']]
        data1 <- RemixAutoML::FilterLogicData(data1, input, FilterLogic=input[['FilterLogic_2']], FilterVariable=input[['FilterVariable_2']], FilterValue=input[['FilterValue_2a']], FilterValue2=fv)
      }

      # Subset by FilterVariable_3
      if(Debug) print('Subset by FilterVariable_3')
      if(input[['FilterVariable_3']] != 'None') {
        fv <- input[['FilterValue_3b']]
        data1 <- RemixAutoML::FilterLogicData(data1, input, FilterLogic=input[['FilterLogic_3']], FilterVariable=input[['FilterVariable_3']], FilterValue=input[['FilterValue_3a']], FilterValue2=fv)
      }

      # Subset by FilterVariable_4
      if(Debug) {print('Subset by FilterVariable_4'); print(input[['FilterVariable_4']])}
      if(input[['FilterVariable_4']] != 'None') {
        fv <- input[['FilterValue_4b']]
        data1 <- RemixAutoML::FilterLogicData(data1, input, FilterLogic=input[['FilterLogic_4']], FilterVariable=input[['FilterVariable_4']], FilterValue=input[['FilterValue_4a']], FilterValue2=fv)
      }

      # Subset Rows based on Filters
      if(Debug) print('Subset Rows based on Filters')
      x <- tryCatch({shiny::isolate(SelectedGroups())}, error = function(e) NULL)
      if(Debug) print(x)
      if(Debug) print(input$Levels_1)
      if(Debug) print(input$Levels_2)
      if(Debug) print(input$Levels_3)
      if(Debug) print(shiny::isolate(YVar()))
      if(Debug) print(shiny::isolate(DateVar()))
      #if(Debug) print(input[['PlotType']] %chin% c('BoxPlotTS','ViolinPlotTS','Scatter','Copula'))
      data1 <<- RemixAutoML::PreparePlotData(
        SubsetOnly = if(input[['PlotType']] %chin% c('BoxPlotTS','ViolinPlotTS','Scatter','Copula')) TRUE else FALSE,
        input,
        PlotDataForecast = data1,
        Aggregate = 'mean',
        TargetVariable = shiny::isolate(YVar()),
        DateVariable = if(shiny::isolate(DateVar()) == 'None') NULL else shiny::isolate(DateVar()),
        GroupVariables = x,
        G1Levels = 'Levels_1',
        G2Levels = 'Levels_2',
        G3Levels = 'Levels_3',
        Debug = Debug)

      # Render Plot
      if(Debug) print(data1)
      if(Debug) print(shiny::isolate(SelectedGroups()))

      # Subsetcheck
      if(Debug) print('Subset check ----')
      Subsetcheck <- !is.null(x) && (!is.null(RemixAutoML::CharNull(input$Levels_1)) || !is.null(RemixAutoML::CharNull(input$Levels_2)) || !is.null(RemixAutoML::CharNull(input$Levels_3)))

      # PlotBuckets check
      if(Debug) print('PlotBuckets check')
      Check2 <- input$Percentile_Buckets == 20

      # ----

      # ----

      # BoxPlotTS, ViolinPlotTS ----
      if(Debug) {print('Create Plot Object'); print(input[['PlotType']])}
      if(shiny::isolate(input[['PlotType']] %chin% c('BoxPlotTS','ViolinPlotTS'))) {
        if(Debug) print('Create Plot Objects 1')
        p1 <- ggplot2::ggplot(data = data1, ggplot2::aes(x = get(shiny::isolate(DateVar())), y = get(shiny::isolate(YVar())), group = get(shiny::isolate(DateVar()))))
        if(input[['PlotType']] == 'BoxPlotTS') {
          if(Debug) print('Create Plot Objects 2a')
          p1 <- p1 + ggplot2::geom_boxplot(outlier.size = 0.1, outlier.colour = 'blue', fill = 'gray')
        } else if(shiny::isolate(input[['PlotType']] == 'ViolinPlotTS')) {
          if(Debug) print('Create Plot Objects 2b')
          p1 <- p1 + ggplot2::geom_violin(draw_quantiles = TRUE)
        }
        if(Debug) print('Create Plot Objects 3')
        p1 <- shiny::isolate(p1 + ggplot2::geom_hline(color = 'blue', yintercept = eval(mean(data1[[eval(shiny::isolate(YVar()))]], na.rm = TRUE))))
        if(Debug) print('Create Plot Objects 4')
        p1 <- shiny::isolate( p1 + RemixAutoML::ChartTheme(
          Size = input[['TextSize']],
          AngleX = input[['AngleX']],
          AngleY = input[['AngleY']],
          ChartColor = input[['ChartColor']],
          BorderColor = input[['BorderColor']],
          TextColor = input[['TextColor']],
          GridColor = input[['GridColor']],
          BackGroundColor = input[['BackGroundColor']]))
        if(Debug) print('Create Plot Objects 5')
        p1 <- p1 + ggplot2::labs(
          title = 'Distribution over Time',
          subtitle = 'Blue line = mean(Y)',
          caption = 'by RemixAutoML') +
          ggplot2::ylim(as.numeric(eval(input[['YMin']])), as.numeric(eval(input[['YMax']]))) +
          ggplot2::ylab(eval(shiny::isolate(YVar()))) + ggplot2::xlab(shiny::isolate(DateVar()))
        if(Debug) print('XTicks')
        if(Debug) print(input[['XTicks']])
        if(!'Default' %in% input[['XTicks']]) p1 <- p1 + suppressMessages(ggplot2::scale_x_date(date_breaks = input[['XTicks']]))
        if(Debug) print('XTicks end')

        # Add faceting (returns no faceting in none was requested)
        if(shiny::isolate(input[['FacetVar1']]) != 'None' && shiny::isolate(input[['FacetVar2']]) != 'None') {
          if(Debug) print('Create Plot Objects 6a')
          p1 <- p1 + ggplot2::facet_grid(get(shiny::isolate(input[['FacetVar1']])) ~ get(shiny::isolate(input[['FacetVar2']])))
        } else if(shiny::isolate(input[['FacetVar1']]) == 'None' && shiny::isolate(input[['FacetVar2']] != 'None')) {
          if(Debug) print('Create Plot Objects 6b')
          p1 <- p1 + ggplot2::facet_wrap(~ get(shiny::isolate(input[['FacetVar2']])))
        } else if(shiny::isolate(input[['FacetVar1']]) != 'None' && shiny::isolate(input[['FacetVar2']]) == 'None') {
          if(Debug) print('Create Plot Objects 6c')
          p1 <- p1 + ggplot2::facet_wrap(~ get(shiny::isolate(input[['FacetVar1']])))
        }
        p1 <<- p1
      }

      # ----

      # Line Plot ----
      if(shiny::isolate(input[['PlotType']] %chin% c('Line'))) {
        if(!any(class(data1[[eval(shiny::isolate(DateVar()))]]) %chin% c('numeric','integer','factor','character','logical','integer64', 'NULL'))) {
          p1 <- RemixAutoML:::TimeSeriesPlotter(
            data = data1,
            TargetVariable = shiny::isolate(YVar()),
            DateVariable = shiny::isolate(DateVar()),
            GroupVariables = if(shiny::isolate(SelectedGroups()) == 'None') NULL else shiny::isolate(SelectedGroups()),
            Aggregate = 'mean',
            NumberGroupsDisplay = input[['NumberGroupsDisplay']],
            LevelsToDisplay = NULL,
            OtherGroupLabel = "OtherGroups",
            DisplayOtherGroup = TRUE,
            TextSize = input[['TextSize']],
            LineWidth = 0.5,
            Color = 'blue',
            XTickMarks = if('Default' %in% input[['XTicks']]) NULL else input[['XTicks']],
            AngleX = input[['AngleX']],
            AngleY = input[['AngleY']],
            ChartColor = input[['ChartColor']],
            BorderColor = input[['BorderColor']],
            TextColor = input[['TextColor']],
            GridColor = input[['GridColor']],
            BackGroundColor = input[['BackGroundColor']],
            LegendPosition = 'bottom',
            LegendTextColor = 'darkblue',
            LegendTextSize = 10)

          # Update labels
          p1 <- p1 + ggplot2::labs(
            title = 'Time Series Plot',
            caption = 'by RemixAutoML') +
            ggplot2::ylim(as.numeric(eval(input[['YMin']])), as.numeric(eval(input[['YMax']]))) +
            ggplot2::ylab(eval(shiny::isolate(YVar()))) + ggplot2::xlab(eval(shiny::isolate(DateVar()))) +
            ggplot2::theme(legend.title = ggplot2::element_blank())

          # For renderPlot args
          # Add faceting (returns no faceting in none was requested)
          #p1 <- AddFaceting(p1, input)
          p1 <<- p1
        } else {
          shinyWidgets::sendSweetAlert(session, title = NULL, text = "X-Variable needs to be a Date, IDate, or Posix type", type = NULL, btn_labels = "error", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
        }
      }

      # ----

      # Scatter, Copula ----
      if(input[['PlotType']] %chin% c('Scatter','Copula')) {

        # Ensure variables are numeric
        if(Debug) {
          print(YVar())
          print(XVar())
        }

        if(!any(c('numeric','integer') %chin% class(data1[[eval(shiny::isolate(YVar()))]]))) {
          shinyWidgets::sendSweetAlert(session, title = NULL, text = "Y needs to be a numeric variable", type = NULL, btn_labels = "error", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
        } else if(!any(c('numeric','integer') %chin% class(data1[[eval(shiny::isolate(XVar()))]]))) {
          shinyWidgets::sendSweetAlert(session, title = NULL, text = "X needs to be a numeric variable", type = NULL, btn_labels = "error", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
        } else {

          # Generate plot
          # Subset + Sample
          R2_Pearson <- c()
          R2_Spearman <- c()
          yyy <- eval(shiny::isolate(YVar()))
          xxx <- eval(shiny::isolate(XVar()))
          if(Debug) print(data1[,.N])
          if(data1[,.N] < 100000L) {
            for(zz in seq_len(30L)) {
              temp <- data1[order(runif(.N))][seq_len(floor(0.50 * .N))]
              R2_Pearson <- c(R2_Pearson, (cor(x = temp[[yyy]], y = temp[[xxx]], method = "pearson")) ^ 2)
              R2_Spearman <- c(R2_Spearman, (cor(x = temp[[yyy]], y = temp[[xxx]], method = "spearman")) ^ 2)
            }
          } else {
            for(zz in seq_len(30L)) {
              temp <- data1[order(runif(.N))][seq_len(100000L)]
              R2_Pearson <- c(R2_Pearson, (cor(x = temp[[yyy]], y = temp[[xxx]], method = "pearson")) ^ 2)
              R2_Spearman <- c(R2_Spearman, (cor(x = temp[[yyy]], y = temp[[xxx]], method = "spearman")) ^ 2)
            }
          }
          rm(temp)

          # Build plot objects
          if(Debug) print('Build plot objects')
          Output <- RemixAutoML::ScatterCopula(
            data = data1,
            x_var = xxx,
            y_var = yyy,
            GroupVariable = if(shiny::isolate(SelectedGroups())[1L] == 'None') NULL else shiny::isolate(SelectedGroups())[1L],
            FacetCol = if(shiny::isolate(input[['FacetVar2']]) == 'None') NULL else shiny::isolate(input[['FacetVar2']]),
            FacetRow = if(shiny::isolate(input[['FacetVar1']]) == 'None') NULL else shiny::isolate(input[['FacetVar1']]),
            SizeVar1 = if(shiny::isolate(input[['SizeVar1']]) == 'None') NULL else shiny::isolate(input[['SizeVar1']]),
            SampleCount = 100000L,
            FitGam = as.logical(shiny::isolate(input[['GamFitScatter']])))

          # Modify by plot type
          if(Debug) print('Modify by plot type')
          if(shiny::isolate(input[['PlotType']] %chin% c('Scatter'))) {
            p1 <- Output[["ScatterPlot"]]
            p1 <- p1 + ggplot2::labs(
              title = paste0('Scatter Plot'),
              subtitle = paste0("r-sq pearson xbar = ", round(mean(R2_Pearson),3L), " +/- ", round(sd(R2_Pearson) / sqrt(30L), 5L)," :: ",
                                "r-sq spearman xbar = ", round(mean(R2_Spearman),3L), " +/- ", round(sd(R2_Spearman) / sqrt(30L), 5L)))
            p1 <- shiny::isolate( p1 + RemixAutoML::ChartTheme(
              Size = input[['TextSize']], AngleX = input[['AngleX']], AngleY = input[['AngleY']],
              ChartColor = input[['ChartColor']], BorderColor = input[['BorderColor']], TextColor = input[['TextColor']],
              GridColor = input[['GridColor']], BackGroundColor = input[['BackGroundColor']]))
            p1 <- p1 + ggplot2::ylim(as.numeric(eval(shiny::isolate(input[['YMin']]))), as.numeric(eval(shiny::isolate(input[['YMax']]))))
            p1 <- p1 + ggplot2::xlim(as.numeric(eval(shiny::isolate(input[['XMin']]))), as.numeric(eval(shiny::isolate(input[['XMax']]))))

          } else if(shiny::isolate(input[['PlotType']] %chin% c('Copula'))) {

            p1 <- Output[["CopulaPlot"]]
            p1 <- p1 + ggplot2::labs(
              title = paste0('Empirical Copula Plot'),
              subtitle = paste0("r-sq pearson xbar = ", round(mean(R2_Pearson),3L), " +/- ", round(sd(R2_Pearson) / sqrt(30L), 5L)," :: ",
                                "r-sq spearman xbar = ", round(mean(R2_Spearman),3L), " +/- ", round(sd(R2_Spearman) / sqrt(30L), 5L)))
            p1 <- shiny::isolate( p1 + RemixAutoML::ChartTheme(
              Size = input[['TextSize']], AngleX = input[['AngleX']], AngleY = input[['AngleY']],
              ChartColor = input[['ChartColor']], BorderColor = input[['BorderColor']], TextColor = input[['TextColor']],
              GridColor = input[['GridColor']], BackGroundColor = input[['BackGroundColor']]))
          }

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
          if(!'Default' %in% input[['XTicks']]) p1 <- p1 + ggplot2::scale_x_continuous(breaks = as.numeric(x_vals))
          if(!'Default' %in% input[['YTicks']]) p1 <- p1 + ggplot2::scale_y_continuous(breaks = as.numeric(y_vals))
        }
      }

      # ----

      # Test Evaluation Plot ----
      if(any(input[['PlotType']] %chin% "Test_EvaluationPlot")) {
        if(Debug) print('Evaluation Plot')
        if(!Subsetcheck && Check2 && !is.null(ModelOutputList$PlotList[['Test_EvaluationPlot']])) {
          if(Debug) print('!Subsetcheck')
          p1 <- ModelOutputList$PlotList[['Test_EvaluationPlot']]
        } else {
          if(Debug) print('! !Subsetcheck')
          p1 <- RemixAutoML::EvalPlot(
            data = data1,
            PredictionColName = isolate(ScoreVar()),
            TargetColName = shiny::isolate(YVar()),
            GraphType = "calibration",
            PercentileBucket = 1/input$Percentile_Buckets,
            aggrfun = function(x) mean(x, na.rm = TRUE))
        }
      }

      # ----

      # Train Evaluation Plot ----
      if(any(input[['PlotType']] %chin% "Train_EvaluationPlot")) {
        if(Debug) print('Evaluation Plot')
        if(Debug) print(Subsetcheck)
        if(Debug) print(Check2)
        if(!Subsetcheck && Check2 && !is.null(ModelOutputList$PlotList[['Train_EvaluationPlot']])) {
          if(Debug) print('!Subsetcheck')
          p1 <- ModelOutputList$PlotList[['Train_EvaluationPlot']]
        }
      }

      # ----

      # Evaluation BoxPlot ----
      if(any(input[['PlotType']] %chin% "Test_EvaluationBoxPlot")) {
        if(!Subsetcheck && Check2 && !is.null(ModelOutputList$PlotList[['Test_EvaluationBoxPlot']])) {
          if(Debug) print('EvalBoxPlot !Subsetcheck')
          p1 <- ModelOutputList$PlotList[['Test_EvaluationBoxPlot']]
        } else {
          if(Debug) print('EvalBoxPlot ! !Subsetcheck')
          p1 <- RemixAutoML::EvalPlot(
            data = data1,
            PredictionColName = isolate(ScoreVar()),
            TargetColName = shiny::isolate(YVar()),
            GraphType = "boxplot",
            PercentileBucket = 1/input$Percentile_Buckets, aggrfun = function(x) mean(x, na.rm = TRUE))
        }
      }

      # ----

      # Evaluation BoxPlot Train ----
      if(any(input[['PlotType']] %chin% "Train_EvaluationBoxPlot")) {
        if(!Subsetcheck && Check2 && !is.null(ModelOutputList$PlotList[['Train_EvaluationBoxPlot']])) {
          if(Debug) print('EvalBoxPlot !Subsetcheck')
          p1 <- ModelOutputList$PlotList[['Train_EvaluationBoxPlot']]
        }
      }

      # ----

      # ROC Plot ----
      if(any(input[['PlotType']] %chin% "Test_ROC_Plot")) {
        if(!Subsetcheck && !is.null(ModelOutputList$PlotList[['Test_ROC_Plot']])) {
          if(Debug) print('ROC !Subsetcheck')
          p1 <- ModelOutputList$PlotList[['Test_ROC_Plot']]
        } else {
          if(Debug) print('Test_ROC_Plot ! !Subsetcheck')
          p1 <- RemixAutoML::ROCPlot(
            data = data1,
            TargetName = shiny::isolate(YVar()),
            SavePlot = FALSE,
            Name = NULL,
            metapath = NULL,
            modelpath = NULL)
        }
      }

      # ----

      # ROC Plot Train ----
      if(any(input[['PlotType']] %chin% "Train_ROC_Plot")) {
        if(!Subsetcheck && !is.null(ModelOutputList$PlotList[['Train_ROC_Plot']])) {
          if(Debug) print('Train_ROC_Plot !Subsetcheck')
          p1 <- ModelOutputList$PlotList[['Train_ROC_Plot']]
        }
      }

      # ----

      # Gains Plot ----
      if(any(input[['PlotType']] %chin% "Test_GainsPlot")) {
        if(!Subsetcheck && !is.null(ModelOutputList$PlotList[['GainsPlot']])) {
          if(Debug) print('Test_GainsPlot !Subsetcheck')
          p1 <- ModelOutputList$PlotList[['Test_GainsPlot']]
        } else {
          if(Debug) print('Gains Plot ! !Subsetcheck')
          p1 <- RemixAutoML::CumGainsChart(
            data = data1,
            TargetColumnName = shiny::isolate(YVar()),
            PredictedColumnName = isolate(ScoreVar()),
            SavePlot = FALSE,
            Name = NULL,
            metapath = NULL,
            modelpath = NULL)$GainsPlot
        }
      }

      # ----

      # Gains Plot Train ----
      if(any(input[['PlotType']] %chin% "Train_GainsPlot")) {
        if(!Subsetcheck && !is.null(ModelOutputList$PlotList[['Train_GainsPlot']])) {
          if(Debug) print('Gains Plot !Subsetcheck')
          p1 <- ModelOutputList$PlotList[['Train_GainsPlot']]
        }
      }

      # ----

      # Lift Plot Test ----
      if(any(input[['PlotType']] %chin% "Test_LiftPlot")) {
        if(!Subsetcheck && !is.null(ModelOutputList$PlotList[['Test_LiftPlot']])) {
          if(Debug) print('Test_LiftPlot !Subsetcheck')
          p1 <- ModelOutputList$PlotList[['Test_LiftPlot']]
        } else {
          if(Debug) print('Test_LiftPlot ! !Subsetcheck')
          p1 <- RemixAutoML::CumGainsChart(
            data = data1,
            TargetColumnName = shiny::isolate(YVar()),
            PredictedColumnName = isolate(ScoreVar()),
            SavePlot = FALSE,
            Name = NULL,
            metapath = NULL,
            modelpath = NULL)$LiftPlot
        }
      }

      # ----

      # Lift Plot Train ----
      if(any(input[['PlotType']] %chin% "Train_LiftPlot")) {
        if(!Subsetcheck && !is.null(ModelOutputList$PlotList[['Train_LiftPlot']])) {
          if(Debug) print('Train_LiftPlot !Subsetcheck')
          p1 <- ModelOutputList$PlotList[['Train_LiftPlot']]
        }
      }

      # ----

      # Scatter Plot Test ----
      if(any(input[['PlotType']] %chin% "Test_ScatterPlot")) {
        if(!Subsetcheck && !is.null(ModelOutputList$PlotList[['Test_ScatterPlot']])) {
          if(Debug) print('Test_ScatterPlot !Subsetcheck')
          p1 <- ModelOutputList$PlotList[['Test_ScatterPlot']]
        } else {
          if(Debug) print('Test_ScatterPlot ! !Subsetcheck')
          p1 <- RemixAutoML::ResidualPlots(
            TestData = data1,
            Target = shiny::isolate(YVar()),
            Predicted = isolate(ScoreVar()),
            DateColumnName = input$ModelInsights_DateVariable,
            Gam_Fit = FALSE)$ScatterPlot
        }
      }

      # ----

      # Scatter Plot Train ----
      if(any(input[['PlotType']] %chin% "Train_ScatterPlot")) {
        if(!Subsetcheck && !is.null(ModelOutputList$PlotList[['Train_ScatterPlot']])) {
          if(Debug) print('Train_ScatterPlot !Subsetcheck')
          p1 <- ModelOutputList$PlotList[['Train_ScatterPlot']]
        }
      }

      # ----

      # Copula Plot Test ----
      if(any(input[['PlotType']] %chin% "Test_CopulaPlot")) {
        if(!Subsetcheck && !is.null(ModelOutputList$PlotList[['Test_CopulaPlot']])) {
          if(Debug) print('Test_CopulaPlot !Subsetcheck')
          p1 <- ModelOutputList$PlotList[['Test_CopulaPlot']]
        } else {
          if(Debug) print('Test_CopulaPlot ! !Subsetcheck')
          p1 <- RemixAutoML::ResidualPlots(
            TestData = data1,
            Target = shiny::isolate(YVar()),
            Predicted = isolate(ScoreVar()),
            DateColumnName = input$ModelInsights_DateVariable,
            Gam_Fit = FALSE)$CopulaPlot
        }
      }

      # ----

      # Copula Plot Train ----
      if(any(input[['PlotType']] %chin% "Train_CopulaPlot")) {
        if(!Subsetcheck && !is.null(ModelOutputList$PlotList[['Train_CopulaPlot']])) {
          if(Debug) print('Train_CopulaPlot !Subsetcheck')
          p1 <- ModelOutputList$PlotList[['Train_CopulaPlot']]
        }
      }

      # ----

      # Residuals Histogram Plot Test ----
      if(any(input[['PlotType']] %chin% "Test_ResidualsHistogram")) {
        if(!Subsetcheck && !is.null(ModelOutputList$PlotList[['Test_ResidualsHistogram']])) {
          if(Debug) print('Test_ResidualsHistogram !Subsetcheck')
          p1 <- ModelOutputList$PlotList[['Test_ResidualsHistogram']]
        } else {
          if(Debug) print('Test_ResidualsHistogram ! !Subsetcheck')
          p1 <- RemixAutoML::ResidualPlots(
            TestData = data1,
            Target = shiny::isolate(YVar()),
            Predicted = shiny::isolate(ScoreVar()),
            DateColumnName = input$ModelInsights_DateVariable,
            Gam_Fit = FALSE)$ResidualsHistogram
        }
      }

      # ----

      # Residuals Histogram Plot Train ----
      if(any(input[['PlotType']] %chin% "Train_ResidualsHistogram")) {
        if(!Subsetcheck && !is.null(ModelOutputList$PlotList[['Train_ResidualsHistogram']])) {
          if(Debug) print('Train_ResidualsHistogram !Subsetcheck')
          p1 <- ModelOutputList$PlotList[['Train_ResidualsHistogram']]
        }
      }

      # ----

      # Variable Importance Plot Test ----
      if(any(input[['PlotType']] %chin% "Test_Importance")) {
        if(Debug) print('Test_Importance ! !Subsetcheck')
        p1 <- RemixAutoML:::VI_Plot(Type = "catboost", VI_Data = ModelOutputList$VariableImportance[['Test_Importance']], TopN = 25)
      }

      # ----

      # Variable Importance Plot Train ----
      if(any(input[['PlotType']] %chin% 'Train_Importance')) {
        if(Debug) print('Train_Importance ! !Subsetcheck')
        p1 <- RemixAutoML:::VI_Plot(Type = 'catboost', VI_Data = ModelOutputList$VariableImportance[['Train_Importance']], TopN = 25)
      }

      # ----

      # Partial Dependence Plot Test ----
      if(any(input[['PlotType']] %chin% 'Test_ParDepPlots') && !is.null(input[['PDP_Variable']])) {
        if(!Subsetcheck && Check2 && !is.null(ModelOutputList$PlotList$Test_ParDepPlots[[eval(input[['PDP_Variable']])]])) {
          p1 <- p1 <- ModelOutputList$PlotList$Test_ParDepPlots[[eval(input[['PDP_Variable']])]]
          if(!is.null(input[['XMin']]) && !is.null(input[['XMax']])) p1 <- p1 + ggplot2::xlim(as.numeric(eval(input[['XMin']])), as.numeric(eval(input[['XMax']])))
          p1$layers[[6L]] <- NULL
          p1$layers[[5L]] <- NULL
          p1$layers[[4L]] <- NULL
        } else {
          p1 <- RemixAutoML::ParDepCalPlots(
            data = data1,
            PredictionColName = isolate(ScoreVar()),
            TargetColName = shiny::isolate(YVar()),
            IndepVar = input[['PDP_Variable']],
            GraphType = 'calibration',
            PercentileBucket = 1 / input$Percentile_Buckets,
            FactLevels = 10,
            Function = function(x) mean(x, na.rm = TRUE))
          if(!is.null(input[['XMin']]) && !is.null(input[['XMax']])) p1 <- p1 + ggplot2::xlim(as.numeric(eval(input[['XMin']])), as.numeric(eval(input[['XMax']])))
          p1$layers[[6L]] <- NULL
          p1$layers[[5L]] <- NULL
          p1$layers[[4L]] <- NULL
        }
      }

      # ----

      # Partial Dependence Plot Train ----
      if(any(input[['PlotType']] %chin% 'Train_ParDepPlots') && !is.null(input[['PDP_Variable']])) {
        if(!Subsetcheck && Check2 && !is.null(ModelOutputList$PlotList$Train_ParDepPlots[[eval(input[['PDP_Variable']])]])) {
          p1 <- ModelOutputList$PlotList$Train_ParDepPlots[[eval(input[['PDP_Variable']])]]
          if(!is.null(input[['XMin']]) && !is.null(input[['XMax']])) p1 <- p1 + ggplot2::xlim(as.numeric(eval(input[['XMin']])), as.numeric(eval(input[['XMax']])))
          p1$layers[[6L]] <- NULL
          p1$layers[[5L]] <- NULL
          p1$layers[[4L]] <- NULL
        } else {
          p1 <- RemixAutoML::ParDepCalPlots(
            data = data1,
            PredictionColName = isolate(ScoreVar()),
            TargetColName = shiny::isolate(YVar()),
            IndepVar = input[['PDP_Variable']],
            GraphType = 'calibration',
            PercentileBucket = 1 / input$Percentile_Buckets,
            FactLevels = 10,
            Function = function(x) mean(x, na.rm = TRUE))
          if(!is.null(input[['XMin']]) && !is.null(input[['XMax']])) p1 <- p1 + ggplot2::xlim(as.numeric(eval(input[['XMin']])), as.numeric(eval(input[['XMax']])))
          p1$layers[[6L]] <- NULL
          p1$layers[[5L]] <- NULL
          p1$layers[[4L]] <- NULL
        }
      }

      # ----

      # Partial Dependence Box Plot Test ----
      if(any(input[['PlotType']] %chin% 'Test_ParDepBoxPlots') && !is.null(input[['PDP_Variable']])) {
        if(!Subsetcheck && Check2 && !is.null(ModelOutputList$PlotList$Test_ParDepBoxPlots[[eval(input[['PDP_Variable']])]])) {
          output$ML_OutputPlot <- shiny::renderPlot({
            ModelOutputList$PlotList$Test_ParDepBoxPlots[[eval(input[['PDP_Variable']])]]
          })
        } else {
          p1 <- RemixAutoML::ParDepCalPlots(
            data = data1,
            PredictionColName = isolate(ScoreVar()),
            TargetColName = shiny::isolate(YVar()),
            IndepVar = input[['PDP_Variable']],
            GraphType = "boxplot",
            PercentileBucket = 1 / input$Percentile_Buckets,
            FactLevels = 10,
            Function = function(x) mean(x, na.rm = TRUE))
          if(!is.null(input[['XMin']]) && !is.null(input[['XMax']])) p1 <- p1 + ggplot2::xlim(as.numeric(eval(input[['XMin']])), as.numeric(eval(input[['XMax']])))
          p1$layers[[6L]] <- NULL
          p1$layers[[5L]] <- NULL
          p1$layers[[4L]] <- NULL
        }
      }

      # ----

      # Partial Dependence Box Plot Train ----
      if(any(input[['PlotType']] %chin% 'Train_ParDepBoxPlots') && !is.null(input[['PDP_Variable']])) {
        if(!Subsetcheck && Check2 && !is.null(ModelOutputList$PlotList$Train_ParDepBoxPlots[[eval(input[['PDP_Variable']])]])) {
          p1 <- ModelOutputList$PlotList$Train_ParDepBoxPlots[[eval(input[['PDP_Variable']])]]
          p1 <<- p1
        } else {
          p1 <- RemixAutoML::ParDepCalPlots(
            data = data1,
            PredictionColName = isolate(ScoreVar()),
            TargetColName = shiny::isolate(YVar()),
            IndepVar = input[['PDP_Variable']],
            GraphType = "boxplot",
            PercentileBucket = 1 / input$Percentile_Buckets,
            FactLevels = 10,
            Function = function(x) mean(x, na.rm = TRUE))
          if(!is.null(input[['XMin']]) && !is.null(input[['XMax']])) p1 <- p1 + ggplot2::xlim(as.numeric(eval(input[['XMin']])), as.numeric(eval(input[['XMax']])))
          p1$layers[[6L]] <- NULL
          p1$layers[[5L]] <- NULL
          p1$layers[[4L]] <- NULL
        }
      }

      # ----

      # Shap Table Variable Importance ----
      if(any(input[['PlotType']] %chin% "ShapPlot")) {
        if(!Subsetcheck && data.table::is.data.table(ML_ShapTable)) {
          ML_ShapTable2 <- ML_ShapTable[, list(Importance = mean(ShapValue, na.rm = TRUE)), by = "Variable"]
          p1 <- RemixAutoML:::VI_Plot(Type = "catboost", VI_Data = ML_ShapTable2, TopN = 25)
        } else {
          ML_ShapTable1 <- RemixAutoML::AutoShapeShap(ScoringData = data1, Threads = parallel::detectCores(), DateColumnName = input$ModelInsights_DateVariable, ByVariableName = NULL)
          ML_ShapTable2 <- ML_ShapTable1[, list(Importance = mean(ShapValue, na.rm = TRUE)), by = "Variable"]
          p1 <- RemixAutoML:::VI_Plot(Type = "catboost", VI_Data = ML_ShapTable2, TopN = 25)
        }
      }

      # ----

      # Return Plot to UI
      p1 <<- p1
      output$Trend <- shiny::renderPlot(width = shiny::isolate(input[['PlotWidth']]), height = shiny::isolate(input[['PlotHeight']]), {
        if(Debug) print('Create Plot output$Trend')
        p1
      })
    }
  })

  # ----

  # ----

  # Close app after closing browser
  session$onSessionEnded(function() {
    stopApp()
  })
}

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@   ----
# Run the application                    ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@   ----
shiny::shinyApp(ui = ui, server = server)
