# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# Environment Setup                    ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
library(data.table)
data.table::setDTthreads(threads = max(1L, parallel::detectCores()-1L))
options(shiny.maxRequestSize = 10000000*1024^2)
options(scipen = 999)




# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# Passthrough Args                     ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

# data related
data <- shiny::getShinyOption(name = 'data', default = NULL)
BlobStorageURL <- shiny::getShinyOption('BlobStorageURL', default = NULL)
IFrameLocation <- shiny::getShinyOption('IFrameLocation', default = NULL)
XVariable <- shiny::getShinyOption('XVariable', default = NULL)
YVariable <- shiny::getShinyOption('YVariable', default = NULL)
DateName <- shiny::getShinyOption('DateName', default = NULL)
GroupVariables <- shiny::getShinyOption('GroupVariables', default = NULL)
FilterVariable <- shiny::getShinyOption('FilterVariable', default = NULL)
ModelOutputList <- shiny::getShinyOption('ModelOutputList', default = NULL)

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
          width=AppWidth, solidHeader=TRUE, collapsible=TRUE, collapsed=FALSE, background=GroupVarsBoxColor,
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
        # Variables                            ----
        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
        shinydashboard::box(
          title = htmltools::tagList(shiny::icon('filter', lib = 'font-awesome'), 'Plotting Variables'),
          solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE, background = 'purple', width = AppWidth,

          # Y Variables
          shiny::fluidRow(
            shiny::column(
              width = 4L,
              tags$h4('Y-Variables'),
              shinyWidgets::dropdown(
                animate = TRUE, right = FALSE, circle = FALSE, tooltip = FALSE, status = "primary", icon = icon("gear"), width = LogoWidth,
                tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Y-Axis Variables')),
                tags$h4(tags$span(style=paste0('color: ', H4Color, ';'),'Y-Variable N corresponds to Plot Number N')),
                RemixAutoML::BlankRow(AppWidth),
                shiny::fluidRow(
                  width = AppWidth,
                  shiny::column(3L, shiny::uiOutput('YVar1')),
                  shiny::column(3L, shiny::uiOutput('YVar2')),
                  shiny::column(3L, shiny::uiOutput('YVar3')),
                  shiny::column(3L, shiny::uiOutput('YVar4'))),
                shiny::fluidRow(
                  width = AppWidth,
                  shiny::column(3L, shiny::uiOutput('YMin1')),
                  shiny::column(3L, shiny::uiOutput('YMin2')),
                  shiny::column(3L, shiny::uiOutput('YMin3')),
                  shiny::column(3L, shiny::uiOutput('YMin4'))),
                shiny::fluidRow(
                  width = AppWidth,
                  shiny::column(3L, shiny::uiOutput('YMax1')),
                  shiny::column(3L, shiny::uiOutput('YMax2')),
                  shiny::column(3L, shiny::uiOutput('YMax3')),
                  shiny::column(3L, shiny::uiOutput('YMax4'))))),

            # X Variables
            shiny::column(
              width = 4L,
              tags$h4('X-Variables'),
              shinyWidgets::dropdown(
                animate = TRUE, right = FALSE, circle = FALSE, tooltip = FALSE, status = "primary", icon = icon("gear"), width = LogoWidth,
                tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'X-Axis Variables')),
                tags$h4(tags$span(style=paste0('color: ', H4Color, ';'),'X-Variable N corresponds to Plot Number N')),
                RemixAutoML::BlankRow(AppWidth),
                shiny::fluidRow(
                  width = AppWidth,
                  shiny::column(3L, shiny::uiOutput('XVar1')),
                  shiny::column(3L, shiny::uiOutput('XVar2')),
                  shiny::column(3L, shiny::uiOutput('XVar3')),
                  shiny::column(3L, shiny::uiOutput('XVar4'))),
                shiny::fluidRow(
                  width = AppWidth,
                  shiny::column(3L, shiny::uiOutput('XMin1')),
                  shiny::column(3L, shiny::uiOutput('XMin2')),
                  shiny::column(3L, shiny::uiOutput('XMin3')),
                  shiny::column(3L, shiny::uiOutput('XMin4'))),
                shiny::fluidRow(
                  width = AppWidth,
                  shiny::column(3L, shiny::uiOutput('XMax1')),
                  shiny::column(3L, shiny::uiOutput('XMax2')),
                  shiny::column(3L, shiny::uiOutput('XMax3')),
                  shiny::column(3L, shiny::uiOutput('XMax4'))))),

            # Date Variables
            shiny::column(
              width = 4L,
              tags$h4('Date Variables'),
              shinyWidgets::dropdown(
                animate = TRUE, right = TRUE, circle = FALSE, tooltip = FALSE, status = "primary", icon = icon("gear"), width = LogoWidth,
                tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Date Variables')),
                tags$h4(tags$span(style=paste0('color: ', H4Color, ';'),'Variable N corresponds to Plot Number N')),
                RemixAutoML::BlankRow(AppWidth),
                shiny::fluidRow(
                  width = AppWidth,
                  shiny::column(3L, shiny::uiOutput('DateVar1')),
                  shiny::column(3L, shiny::uiOutput('DateVar2')),
                  shiny::column(3L, shiny::uiOutput('DateVar3')),
                  shiny::column(3L, shiny::uiOutput('DateVar4'))),
                shiny::fluidRow(
                  width = AppWidth,
                  shiny::column(3L, shiny::uiOutput('DateMin1')),
                  shiny::column(3L, shiny::uiOutput('DateMin2')),
                  shiny::column(3L, shiny::uiOutput('DateMin3')),
                  shiny::column(3L, shiny::uiOutput('DateMin4'))),
                shiny::fluidRow(
                  width = AppWidth,
                  shiny::column(3L, shiny::uiOutput('DateMax1')),
                  shiny::column(3L, shiny::uiOutput('DateMax2')),
                  shiny::column(3L, shiny::uiOutput('DateMax3')),
                  shiny::column(3L, shiny::uiOutput('DateMax4')))))),

          # Add Space
          RemixAutoML::BlankRow(AppWidth),

          # Group Variables and # Facet and Size Variables
          shiny::fluidRow(

            # Group Variables
            shiny::column(
              width = 4L,
              tags$h4('Group Variables'),
              shinyWidgets::dropdown(
                animate = TRUE, right = FALSE, circle = FALSE, tooltip = FALSE, status = "primary", icon = icon("gear"), width = LogoWidth,
                tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Groups and Associated Levels')),
                tags$h4(tags$span(style=paste0('color: ', H4Color, ';'),'Variable N corresponds to Plot Number N')),
                RemixAutoML::BlankRow(AppWidth),
                shiny::fluidRow(
                  width = AppWidth,
                  shiny::column(3L, shiny::uiOutput('GroupVars1')),
                  shiny::column(3L, shiny::uiOutput('GroupVars2')),
                  shiny::column(3L, shiny::uiOutput('GroupVars3')),
                  shiny::column(3L, shiny::uiOutput('GroupVars4'))),
                shiny::fluidRow(
                  width = AppWidth,
                  shiny::column(3L, shiny::conditionalPanel(width = 3L, condition = "length(input['GroupVars1']) >= 1", shiny::uiOutput('Levels_1_1'))),
                  shiny::column(3L, shiny::conditionalPanel(width = 3L, condition = "length(input['GroupVars2']) >= 1", shiny::uiOutput('Levels_2_1'))),
                  shiny::column(3L, shiny::conditionalPanel(width = 3L, condition = "length(input['GroupVars3']) >= 1", shiny::uiOutput('Levels_3_1'))),
                  shiny::column(3L, shiny::conditionalPanel(width = 3L, condition = "length(input['GroupVars4']) >= 1", shiny::uiOutput('Levels_4_1')))),
                shiny::fluidRow(
                  width = AppWidth,
                  shiny::column(3L, shiny::conditionalPanel(width = 3L, condition = "length(input['GroupVars1']) >= 1", shiny::uiOutput('Levels_1_2'))),
                  shiny::column(3L, shiny::conditionalPanel(width = 3L, condition = "length(input['GroupVars2']) >= 1", shiny::uiOutput('Levels_2_2'))),
                  shiny::column(3L, shiny::conditionalPanel(width = 3L, condition = "length(input['GroupVars3']) >= 1", shiny::uiOutput('Levels_3_2'))),
                  shiny::column(3L, shiny::conditionalPanel(width = 3L, condition = "length(input['GroupVars4']) >= 1", shiny::uiOutput('Levels_4_2')))),
                shiny::fluidRow(
                  width = AppWidth,
                  shiny::column(3L, shiny::conditionalPanel(width = 3L, condition = "length(input['GroupVars1']) >= 1", shiny::uiOutput('Levels_1_3'))),
                  shiny::column(3L, shiny::conditionalPanel(width = 3L, condition = "length(input['GroupVars2']) >= 1", shiny::uiOutput('Levels_2_3'))),
                  shiny::column(3L, shiny::conditionalPanel(width = 3L, condition = "length(input['GroupVars3']) >= 1", shiny::uiOutput('Levels_3_3'))),
                  shiny::column(3L, shiny::conditionalPanel(width = 3L, condition = "length(input['GroupVars4']) >= 1", shiny::uiOutput('Levels_4_3')))))),

            # Facet and Size Variables
            shiny::column(
              width = 4L,
              tags$h4('By Variables'),
              shinyWidgets::dropdown(
                animate = TRUE, right = FALSE, circle = FALSE, tooltip = FALSE, status = "primary", icon = icon("gear"), width = LogoWidth,
                tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Facets and Size Variables')),
                tags$h4(tags$span(style=paste0('color: ', H4Color, ';'),'Variable N corresponds to Plot Number N')),
                RemixAutoML::BlankRow(AppWidth),
                shiny::fluidRow(
                  width = AppWidth,
                  shiny::column(3L, shiny::uiOutput('FacetVar_1_1')),
                  shiny::column(3L, shiny::uiOutput('FacetVar_2_1')),
                  shiny::column(3L, shiny::uiOutput('FacetVar_3_1')),
                  shiny::column(3L, shiny::uiOutput('FacetVar_4_1'))),
                shiny::fluidRow(
                  width = AppWidth,
                  shiny::column(3L, shiny::uiOutput('FacetVar_1_2')),
                  shiny::column(3L, shiny::uiOutput('FacetVar_2_2')),
                  shiny::column(3L, shiny::uiOutput('FacetVar_3_2')),
                  shiny::column(3L, shiny::uiOutput('FacetVar_4_2'))),
                shiny::fluidRow(
                  width = AppWidth,
                  shiny::column(3L, shiny::uiOutput('SizeVar1')),
                  shiny::column(3L, shiny::uiOutput('SizeVar2')),
                  shiny::column(3L, shiny::uiOutput('SizeVar3')),
                  shiny::column(3L, shiny::uiOutput('SizeVar4'))))),

            # Score Variables
            shiny::column(
              width = 4L,
              tags$h4('Model Variables'),
              shinyWidgets::dropdown(
                animate = TRUE, right = TRUE, circle = FALSE, tooltip = FALSE, status = "primary", icon = icon("gear"), width = LogoWidth,
                tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Scoring Variables and Partial Dependence Plot Variables')),
                tags$h4(tags$span(style=paste0('color: ', H4Color, ';'),'Variable N corresponds to Plot Number N')),
                RemixAutoML::BlankRow(AppWidth),
                shiny::fluidRow(
                  width = AppWidth,
                  shiny::column(3L, shiny::uiOutput('ScoreVar1')),
                  shiny::column(3L, shiny::uiOutput('ScoreVar2')),
                  shiny::column(3L, shiny::uiOutput('ScoreVar3')),
                  shiny::column(3L, shiny::uiOutput('ScoreVar4'))),
                shiny::fluidRow(
                  width = AppWidth,
                  width = AppWidth,
                  shiny::column(3L, shiny::uiOutput('PDP_Variable1')),
                  shiny::column(3L, shiny::uiOutput('PDP_Variable2')),
                  shiny::column(3L, shiny::uiOutput('PDP_Variable3')),
                  shiny::column(3L, shiny::uiOutput('PDP_Variable4')))))

            )),

        # Add Space
        RemixAutoML::BlankRow(AppWidth),

        # ----

        # ----

        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
        # Dragula Inputs                       ----
        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

        # Add Space
        RemixAutoML::BlankRow(AppWidth),

        # Plot Type
        shinydashboard::box(
          title = htmltools::tagList(shiny::icon('filter', lib = 'font-awesome'), 'Plot Selection'),
          solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE, background = 'aqua', width = AppWidth,
          shiny::column(12L, shiny::uiOutput('PlotTypeDragula'))),

        # Add Space
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
            shinyWidgets::actionBttn(inputId='TrendPlotExecute', label='Create Plot', icon=shiny::icon('chevron-right', lib = 'font-awesome'), style='gradient', color='royal')), # eval(CreatePlotButtonColor)

          # Update Theme!
          shiny::column(
            width = 3L, shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(inputId='UpdatePlotThemeElements', label='Update Theme', icon=shiny::icon('chevron-right', lib = 'font-awesome'), style='gradient', color='default')), # color=eval(UpdatePlotButtonColor))),

          # Reset Theme!
          shiny::column(
            width = 3L, shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(inputId='ResetPlotThemeElements', label='Reset Theme', icon=shiny::icon('chevron-right', lib = 'font-awesome'), style='gradient', color='default'))), # color=eval(ResetPlotButtonColor)))),

        # Add Space
        RemixAutoML::BlankRow(AppWidth),

        # ----

        # ----

        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
        # Plotting Adjustments                 ----
        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
        shiny::fluidRow(

          # Plot Enhancements (col 1)
          shiny::column(
            width = 1L,
            tags$h4('Plot Extras'),
            shinyWidgets::dropdown(
              right = FALSE, animate = TRUE, circle = FALSE, tooltip = FALSE, status = "primary", inputId = "By-Variables", icon = icon("gear"), width = LogoWidth,
              tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Plot Enhancements')),
              tags$h4(tags$span(style=paste0('color: ', H4Color, ';'),'Additional MetaData Selection for Plot Enhancements')),
              RemixAutoML::BlankRow(AppWidth),
              shiny::fluidRow(
                width = AppWidth,
                shiny::column(3L, shiny::uiOutput('GamFitScatter1')),
                shiny::column(3L, shiny::uiOutput('GamFitScatter2')),
                shiny::column(3L, shiny::uiOutput('GamFitScatter3')),
                shiny::column(3L, shiny::uiOutput('GamFitScatter4'))),
              shiny::fluidRow(
                width = AppWidth,
                shiny::column(3L, shiny::uiOutput('NumberBins1')),
                shiny::column(3L, shiny::uiOutput('NumberBins2')),
                shiny::column(3L, shiny::uiOutput('NumberBins3')),
                shiny::column(3L, shiny::uiOutput('NumberBins4'))),
              shiny::fluidRow(
                width = AppWidth,
                shiny::column(3L, shiny::uiOutput('Percentile_Buckets1')),
                shiny::column(3L, shiny::uiOutput('Percentile_Buckets2')),
                shiny::column(3L, shiny::uiOutput('Percentile_Buckets3')),
                shiny::column(3L, shiny::uiOutput('Percentile_Buckets4'))))),

          # Plot Formatting (col 2)
          shiny::column(
            width = 1L,
            tags$h4('Formatting'),
            shinyWidgets::dropdown(
              right = FALSE, animate = TRUE, circle = FALSE, tooltip = FALSE, status = "primary", icon = icon("gear"), width = LogoWidth,
              tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Plot Formatting')),
              tags$h4(tags$span(style=paste0('color: ', H4Color, ';'),'For color options, see Plot Colors')),
              RemixAutoML::BlankRow(AppWidth),
              shiny::fluidRow(
                width = AppWidth,
                shiny::column(3L, shiny::uiOutput('PlotHeight')),
                shiny::column(3L, shiny::uiOutput('PlotWidth')),
                shiny::column(3L, shiny::uiOutput('AngleY')),
                shiny::column(3L, shiny::uiOutput('AngleX'))),
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
                shiny::column(3L, shiny::uiOutput('TextSize')),
                shiny::column(3L, shiny::uiOutput('OutlierSize')),
                shiny::column(3L, shiny::uiOutput('LegendPosition')),
                shiny::column(3L, shiny::uiOutput('LegendLineType'))))),

            # Plot Colors (col 3)
            shiny::column(
              width = 5L,
              tags$h4('Colors'),
              shinyWidgets::dropdown(
                right = FALSE, animate = TRUE, circle = FALSE, tooltip = FALSE, status = "primary", icon = icon("gear"), width = LogoWidth,
                tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Plot Coloring')),
                RemixAutoML::BlankRow(AppWidth),
                shiny::fluidRow(
                  width = AppWidth,
                  shiny::column(3L, shiny::uiOutput('BackGroundColor')),
                  shiny::column(3L, shiny::uiOutput('SubTitleColor')),
                  shiny::column(3L, shiny::uiOutput('ChartColor')),
                  shiny::column(3L, shiny::uiOutput('BorderColor'))),
                shiny::fluidRow(
                  width = AppWidth,
                  shiny::column(3L, shiny::uiOutput('TextColor')),
                  shiny::column(3L, shiny::uiOutput('BoxPlotFill')),
                  shiny::column(3L, shiny::uiOutput('OutlierColor')),
                  shiny::column(3L, shiny::uiOutput('GridColor'))))),

            # Plot 1 Filter Variables (col 4)
            shiny::column(
              width = 1L,
              tags$h4('Filters P1'),
              shinyWidgets::dropdown(
                right = TRUE, animate = TRUE, circle = FALSE, tooltip = FALSE, status = "success", icon = icon("gear"), width = LogoWidth,
                tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Filter Variables, Logic, and Values')),
                tags$h4(tags$span(style=paste0('color: ', H4Color, ';'),'Filter Information for Plot 1')),
                RemixAutoML::BlankRow(AppWidth),
                shiny::fluidRow(
                  width = AppWidth,
                  shiny::column(3L, shiny::uiOutput('FilterVariable_1_1')),
                  shiny::column(3L, shiny::uiOutput('FilterVariable_1_2')),
                  shiny::column(3L, shiny::uiOutput('FilterVariable_1_3')),
                  shiny::column(3L, shiny::uiOutput('FilterVariable_1_4'))),
                shiny::fluidRow(
                  width = AppWidth,
                  shiny::column(3L, shiny::uiOutput('FilterLogic_1_1')),
                  shiny::column(3L, shiny::uiOutput('FilterLogic_1_2')),
                  shiny::column(3L, shiny::uiOutput('FilterLogic_1_3')),
                  shiny::column(3L, shiny::uiOutput('FilterLogic_1_4'))),
                shiny::fluidRow(
                  width = AppWidth,
                  shiny::column(3L, shiny::uiOutput('FilterValue_1_1_1')),
                  shiny::column(3L, shiny::uiOutput('FilterValue_1_2_1')),
                  shiny::column(3L, shiny::uiOutput('FilterValue_1_3_1')),
                  shiny::column(3L, shiny::uiOutput('FilterValue_1_4_1'))),
                shiny::fluidRow(
                  width = AppWidth,
                  shiny::column(3L, shiny::uiOutput('FilterValue_1_1_2')),
                  shiny::column(3L, shiny::uiOutput('FilterValue_1_2_2')),
                  shiny::column(3L, shiny::uiOutput('FilterValue_1_3_2')),
                  shiny::column(3L, shiny::uiOutput('FilterValue_1_4_2'))))),

            # Plot 2 Filter Variables (col 5)
            shiny::column(
              width = 1L,
              tags$h4('Filters P2'),
              shinyWidgets::dropdown(
                right = TRUE, animate = TRUE, circle = FALSE, tooltip = FALSE, status = "success", icon = icon("gear"), width = LogoWidth,
                tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Filter Variables, Logic, and Values')),
                tags$h4(tags$span(style=paste0('color: ', H4Color, ';'),'Filter Information for Plot 2')),
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
                  shiny::column(3L, shiny::uiOutput('FilterValue_2_4_2'))))),

            # Plot 3 Filter Variables (col 6)
            shiny::column(
              width = 1L,
              tags$h4('Filters P3'),
              shinyWidgets::dropdown(
                right = TRUE, animate = TRUE, circle = FALSE, tooltip = FALSE, status = "success", icon = icon("gear"), width = LogoWidth,
                tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Filter Variables, Logic, and Values')),
                tags$h4(tags$span(style=paste0('color: ', H4Color, ';'),'Filter Information for Plot 3')),
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
                  shiny::column(3L, shiny::uiOutput('FilterValue_3_4_2'))))),

            # Plot 4 Filter Variables (col 7)
            shiny::column(
              width = 1L,
              tags$h4('Filters P4'),
              shinyWidgets::dropdown(
                right = TRUE, animate = TRUE, circle = FALSE, tooltip = FALSE, status = "success", icon = icon("gear"), width = LogoWidth,
                tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Filter Variables, Logic, and Values')),
                tags$h4(tags$span(style=paste0('color: ', H4Color, ';'),'Filter Information for Plot 4')),
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
                  shiny::column(3L, shiny::uiOutput('FilterValue_4_4_2')))))
          ),

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
    )))

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

    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    # Load Data Sets and Rdata             ----
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

    # Local data loading
    if(Debug) print('data check 1')
    CodeCollection <- list()
    data <<- RemixAutoML::ReactiveLoadCSV(Infile = input[['DataLoad']], ProjectList = NULL, DateUpdateName = NULL, RemoveObjects = NULL, Debug = TRUE)
    # (path returned is a temp path) CodeCollection[[1L]] <- paste0("data.table::data.table(file = ", input[['DataLoad']], ")")
    if(Debug) print('data check 2')
    inFile <- tryCatch({input$ModelObjectLoad}, error = function(x) NULL)
    if(Debug) print(inFile)
    if(!is.null(inFile)) {
      if(Debug) print('loading .Rdata')
      e <- new.env()
      name <- load(inFile$datapath, e)
      if(Debug) print('store ModelOutputList globally')
      ModelOutputList <- e[[name]]
      assign(x = 'ModelOutputList', value = ModelOutputList, envir = .GlobalEnv)
    } else {
      ModelOutputList <<- NULL
    }

    # Azure data loading
    if(Debug) print(input$blob)
    if(input$blob != "") {
      if(Debug) {print('data check 3'); print(input$blob)}
      inFile <- input$blob
      if(!is.null(infile)) {
        if(grepl(pattern = '.csv', x = infile)) {
          download.file(url=paste0(BlobStorageURL, infile), destfile = file.path(system.file(package = 'RemixAutoML'), 'tests/CSVs/data.csv'))
          data <<- data.table::fread(file.path(system.file(package = 'RemixAutoML'), 'tests/CSVs/data.csv'))
        } else {
          download.file(url=paste0(BlobStorageURL, infile), destfile = file.path(system.file(package = 'RemixAutoML'), 'tests/CSVs/data.Rdata'))
          e <- new.env()
          name <- load(file.path(system.file(package = 'RemixAutoML'), 'tests/CSVs/data.Rdata'), e)
          ModelOutputList <<- e[[name]]
        }
      }
    }
    CodeCollection <<- CodeCollection

    # ----

    # ----

    # EXACT COPIES FROM THE SET BELOW ----

    # EXACT COPIES FROM THE SET BELOW ----

    # EXACT COPIES FROM THE SET BELOW ----

    # ----

    # ----

    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    # Variables                            ----
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

    if(Debug) print("Here a")

    # YVars
    output$YVar1 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = 'YVar1', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Y-Variable 1'), Choices = c('None', names(data)), SelectedDefault = YVariable, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here b")

    output$YVar2 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = 'YVar2', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Y-Variable 2'), Choices = c('None', names(data)), SelectedDefault = YVariable, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here c")

    output$YVar3 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = 'YVar3', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Y-Variable 3'), Choices = c('None', names(data)), SelectedDefault = YVariable, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here d")

    output$YVar4 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = 'YVar4', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Y-Variable 4'), Choices = c('None', names(data)), SelectedDefault = YVariable, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here e")

    # 'X-Variables'
    output$XVar1 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = 'XVar1', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'X-Variable 1'), Choices = c('None', names(data)), SelectedDefault = XVariable, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here f")

    output$XVar2 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = 'XVar2', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'X-Variable 2'), Choices = c('None', names(data)), SelectedDefault = XVariable, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here g")

    output$XVar3 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = 'XVar3', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'X-Variable 3'), Choices = c('None', names(data)), SelectedDefault = XVariable, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here h")

    output$XVar4 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = 'XVar4', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'X-Variable 4'), Choices = c('None', names(data)), SelectedDefault = XVariable, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here i")

    # 'Score-Variables'
    output$ScoreVar1 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = 'ScoreVar1', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Scoring-Variable 1'), Choices = c('None', names(data)), SelectedDefault = XVariable, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here j")

    output$ScoreVar2 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = 'ScoreVar2', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Scoring-Variable 2'), Choices = c('None', names(data)), SelectedDefault = XVariable, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here k")

    output$ScoreVar3 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = 'ScoreVar3', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Scoring-Variable 3'), Choices = c('None', names(data)), SelectedDefault = XVariable, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here l")

    output$ScoreVar4 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = 'ScoreVar4', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Scoring-Variable 4'), Choices = c('None', names(data)), SelectedDefault = XVariable, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here m")

    # 'Date Variables'
    output$DateVar1 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = 'DateVar1', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Date-Variable 1'), Choices = c('None', names(data)), SelectedDefault = DateName, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here n")

    output$DateVar2 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = 'DateVar2', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Date-Variable 2'), Choices = c('None', names(data)), SelectedDefault = DateName, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here o")

    output$DateVar3 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = 'DateVar3', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Date-Variable 3'), Choices = c('None', names(data)), SelectedDefault = DateName, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here p")

    output$DateVar4 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = 'DateVar4', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Date-Variable 4'), Choices = c('None', names(data)), SelectedDefault = DateName, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
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

    # Y-Mins
    output$YMin1 <- shiny::renderUI({
      if(Debug) print('YMin 1 PickerInput');
      choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = eval(YVar1()))$ChoiceInput}, error = function(x) NULL)
      RemixAutoML::PickerInput(InputID='YMin1', Multiple=FALSE, Debug=Debug, Label=tags$span(style=paste0('color: ', AppTextColor, ';'), 'Min Y1-Value'), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })

    if(Debug) print("Here s")

    output$YMin2 <- shiny::renderUI({
      if(Debug) print('YMin 2 PickerInput');
      choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = eval(YVar2()))$ChoiceInput}, error = function(x) NULL)
      RemixAutoML::PickerInput(InputID='YMin2', Multiple=FALSE, Debug=Debug, Label=tags$span(style=paste0('color: ', AppTextColor, ';'), 'Min Y2-Value'), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })

    if(Debug) print("Here t")

    output$YMin3 <- shiny::renderUI({
      if(Debug) print('YMin 3 PickerInput');
      choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = eval(YVar3()))$ChoiceInput}, error = function(x) NULL)
      RemixAutoML::PickerInput(InputID='YMin3', Multiple=FALSE, Debug=Debug, Label=tags$span(style=paste0('color: ', AppTextColor, ';'), 'Min Y3-Value'), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })

    if(Debug) print("Here u")

    output$YMin4 <- shiny::renderUI({
      if(Debug) print('YMin 4 PickerInput');
      choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = eval(YVar4()))$ChoiceInput}, error = function(x) NULL)
      RemixAutoML::PickerInput(InputID='YMin4', Multiple=FALSE, Debug=Debug, Label=tags$span(style=paste0('color: ', AppTextColor, ';'), 'Min Y4-Value'), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })

    if(Debug) print("Here v")

    # X-Mins
    output$XMin1 <- shiny::renderUI({
      if(Debug) print('XMin 1 PickerInput');
      choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = eval(XVar1()))$ChoiceInput}, error = function(x) NULL)
      RemixAutoML::PickerInput(InputID='XMin1', Multiple=FALSE, Debug=Debug, Label=tags$span(style=paste0('color: ', AppTextColor, ';'), 'Min X1-Value'), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })

    if(Debug) print("Here w")

    output$XMin2 <- shiny::renderUI({
      if(Debug) print('XMin 2 PickerInput');
      choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = eval(XVar2()))$ChoiceInput}, error = function(x) NULL)
      RemixAutoML::PickerInput(InputID='XMin2', Multiple=FALSE, Debug=Debug, Label=tags$span(style=paste0('color: ', AppTextColor, ';'), 'Min X2-Value'), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })

    if(Debug) print("Here x")

    output$XMin3 <- shiny::renderUI({
      if(Debug) print('XMin 3 PickerInput');
      choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = eval(XVar3()))$ChoiceInput}, error = function(x) NULL)
      RemixAutoML::PickerInput(InputID='XMin3', Multiple=FALSE, Debug=Debug, Label=tags$span(style=paste0('color: ', AppTextColor, ';'), 'Min X3-Value'), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })

    if(Debug) print("Here y")

    output$XMin4 <- shiny::renderUI({
      if(Debug) print('XMin 4 PickerInput');
      choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = eval(XVar4()))$ChoiceInput}, error = function(x) NULL)
      RemixAutoML::PickerInput(InputID='XMin4', Multiple=FALSE, Debug=Debug, Label=tags$span(style=paste0('color: ', AppTextColor, ';'), 'Min X4-Value'), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })

    if(Debug) print("Here z")

    # Y-Maxs
    output$YMax1 <- shiny::renderUI({
      if(Debug) print('YMax 1 PickerInput');
      choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = eval(YVar1()))$ChoiceInput}, error = function(x) NULL)
      RemixAutoML::PickerInput(InputID='YMax1', Multiple=FALSE, Debug=Debug, Label=tags$span(style=paste0('color: ', AppTextColor, ';'), 'Max Y1-Value'), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })

    if(Debug) print("Here aa")

    output$YMax2 <- shiny::renderUI({
      if(Debug) print('YMax 2 PickerInput');
      choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = eval(YVar2()))$ChoiceInput}, error = function(x) NULL)
      RemixAutoML::PickerInput(InputID='YMax2', Multiple=FALSE, Debug=Debug, Label=tags$span(style=paste0('color: ', AppTextColor, ';'), 'Max Y2-Value'), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })

    if(Debug) print("Here bb")

    output$YMax3 <- shiny::renderUI({
      if(Debug) print('YMax 3 PickerInput');
      choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = eval(YVar3()))$ChoiceInput}, error = function(x) NULL)
      RemixAutoML::PickerInput(InputID='YMax3', Multiple=FALSE, Debug=Debug, Label=tags$span(style=paste0('color: ', AppTextColor, ';'), 'Max Y3-Value'), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })

    if(Debug) print("Here cc")

    output$YMax4 <- shiny::renderUI({
      if(Debug) print('YMax 4 PickerInput');
      choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = eval(YVar4()))$ChoiceInput}, error = function(x) NULL)
      RemixAutoML::PickerInput(InputID='YMax4', Multiple=FALSE, Debug=Debug, Label=tags$span(style=paste0('color: ', AppTextColor, ';'), 'Max Y4-Value'), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })

    if(Debug) print("Here dd")

    # X-Maxs
    output$XMax1 <- shiny::renderUI({
      if(Debug) print('XMax 1 PickerInput');
      choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = eval(XVar1()))$ChoiceInput}, error = function(x) NULL)
      RemixAutoML::PickerInput(InputID='XMax1', Multiple=FALSE, Debug=Debug, Label=tags$span(style=paste0('color: ', AppTextColor, ';'), 'Max X1-Value'), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })

    if(Debug) print("Here ee")

    output$XMax2 <- shiny::renderUI({
      if(Debug) print('XMax 2 PickerInput');
      choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = eval(XVar2()))$ChoiceInput}, error = function(x) NULL)
      RemixAutoML::PickerInput(InputID='XMax2', Multiple=FALSE, Debug=Debug, Label=tags$span(style=paste0('color: ', AppTextColor, ';'), 'Max X2-Value'), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })

    if(Debug) print("Here ff")

    output$XMax3 <- shiny::renderUI({
      if(Debug) print('XMax 3 PickerInput');
      choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = eval(XVar3()))$ChoiceInput}, error = function(x) NULL)
      RemixAutoML::PickerInput(InputID='XMax3', Multiple=FALSE, Debug=Debug, Label=tags$span(style=paste0('color: ', AppTextColor, ';'), 'Max X3-Value'), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })

    if(Debug) print("Here gg")

    output$XMax4 <- shiny::renderUI({
      if(Debug) print('XMax 4 PickerInput');
      choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = eval(XVar4()))$ChoiceInput}, error = function(x) NULL)
      RemixAutoML::PickerInput(InputID='XMax4', Multiple=FALSE, Debug=Debug, Label=tags$span(style=paste0('color: ', AppTextColor, ';'), 'Max X4-Value'), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })

    if(Debug) print("Here hh")

    # Date-Mins
    output$DateMin1 <- shiny::renderUI({
      if(Debug) print('DateMin 1 PickerInput');
      choices <- RemixAutoML::KeyVarsInit(data, VarName = eval(tryCatch({DateVar1()}, error = function(x) NULL)))$ChoiceInput
      RemixAutoML::PickerInput(InputID='DateMin1', Multiple=FALSE, Debug=Debug, Label=tags$span(style=paste0('color: ', AppTextColor, ';'), 'Min Date-Value 1'), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })

    if(Debug) print("Here ii")

    output$DateMin2 <- shiny::renderUI({
      if(Debug) print('DateMin 2 PickerInput');
      choices <- RemixAutoML::KeyVarsInit(data, VarName = eval(tryCatch({DateVar2()}, error = function(x) NULL)))$ChoiceInput
      RemixAutoML::PickerInput(InputID='DateMin2', Multiple=FALSE, Debug=Debug, Label=tags$span(style=paste0('color: ', AppTextColor, ';'), 'Min Date-Value 2'), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })

    if(Debug) print("Here jj")

    output$DateMin3 <- shiny::renderUI({
      if(Debug) print('DateMin 3 PickerInput');
      choices <- RemixAutoML::KeyVarsInit(data, VarName = eval(tryCatch({DateVar3()}, error = function(x) NULL)))$ChoiceInput
      RemixAutoML::PickerInput(InputID='DateMin3', Multiple=FALSE, Debug=Debug, Label=tags$span(style=paste0('color: ', AppTextColor, ';'), 'Min Date-Value 3'), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })

    if(Debug) print("Here kk")

    output$DateMin4 <- shiny::renderUI({
      if(Debug) print('DateMin 4 PickerInput');
      choices <- RemixAutoML::KeyVarsInit(data, VarName = eval(tryCatch({DateVar4()}, error = function(x) NULL)))$ChoiceInput
      RemixAutoML::PickerInput(InputID='DateMin4', Multiple=FALSE, Debug=Debug, Label=tags$span(style=paste0('color: ', AppTextColor, ';'), 'Min Date-Value 4'), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
    })

    if(Debug) print("Here ll")

    # Date-Maxs
    output$DateMax1 <- shiny::renderUI({
      if(Debug) print('DateMax 1 PickerInput');
      choices <- RemixAutoML::KeyVarsInit(data, VarName = eval(tryCatch({DateVar1()}, error = function(x) NULL)))$ChoiceInput
      RemixAutoML::PickerInput(InputID='DateMax1', Multiple=FALSE, Debug=Debug, Label=tags$span(style=paste0('color: ', AppTextColor, ';'), 'Max Date-Value 1'), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })

    if(Debug) print("Here mm")

    output$DateMax2 <- shiny::renderUI({
      if(Debug) print('DateMax 2 PickerInput');
      choices <- RemixAutoML::KeyVarsInit(data, VarName = eval(tryCatch({DateVar2()}, error = function(x) NULL)))$ChoiceInput
      RemixAutoML::PickerInput(InputID='DateMax2', Multiple=FALSE, Debug=Debug, Label=tags$span(style=paste0('color: ', AppTextColor, ';'), 'Max Date-Value 2'), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })

    if(Debug) print("Here oo")

    output$DateMax3 <- shiny::renderUI({
      if(Debug) print('DateMax 3 PickerInput');
      choices <- RemixAutoML::KeyVarsInit(data, VarName = eval(tryCatch({DateVar3()}, error = function(x) NULL)))$ChoiceInput
      RemixAutoML::PickerInput(InputID='DateMax3', Multiple=FALSE, Debug=Debug, Label=tags$span(style=paste0('color: ', AppTextColor, ';'), 'Max Date-Value 3'), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })

    if(Debug) print("Here pp")

    output$DateMax4 <- shiny::renderUI({
      if(Debug) print('DateMax 4 PickerInput');
      choices <- RemixAutoML::KeyVarsInit(data, VarName = eval(tryCatch({DateVar4()}, error = function(x) NULL)))$ChoiceInput
      RemixAutoML::PickerInput(InputID='DateMax4', Multiple=FALSE, Debug=Debug, Label=tags$span(style=paste0('color: ', AppTextColor, ';'), 'Max Date-Value 4'), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
    })

    # ----

    # ----

    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    # Plotting MetaData                    ----
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

    if(Debug) print("Here qq")

    # Dragula for PlotType
    output$PlotTypeDragula <- shiny::renderUI({
      x <- RemixAutoML:::AvailableAppInsightsPlots(x = ModelOutputList)
      esquisse::dragulaInput(
        inputId = 'PlotTypeDragula',
        label = 'Drag and Drop Plot Types',
        sourceLabel = 'Plots',
        targetsLabels = c('UpperLeftPlot', 'BottomLeftPlot', 'UpperRightPlot', 'BottomRightPlot'),
        #style="color: #fff; background-color: #e95420; border-color: #c34113; border-radius: 10px; border-width: 2px",
        choices = sort(c(x,x,x,x))
        #,
        # choiceNames = , choiceValues = , selected = , status = , replace = , copySource = , badge = , ncolSource = , ncolGrid = , dragulaOpts = , boxStyle = , width = "100%", height = "100%"
      )
    })

    if(Debug) print("Here rr")

    # UI Plot Options
    output$NumberGroupsDisplay <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID = 'NumberGroupsDisplay', Label = tags$span(style='color: blue;', 'Dispay N Levels'), Step = 1L, Value = 5L, Min = 1L, Max = 100L)
    })

    if(Debug) print("Here ss")

    output$PlotWidth <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID = "PlotWidth", Label=tags$span(style='color: blue;', 'Plot Width'), Step = 50, Min = 800, Max = 1800, Value = 1600)
    })

    if(Debug) print("Here tt")

    output$PlotHeight <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID = "PlotHeight", Label=tags$span(style='color: blue;', 'Plot Height'), Step = 25, Min = 350, Max = 350*10, Value = 500)
    })

    if(Debug) print("Here uu")

    # PDP Variables
    output$PDP_Variable1 <- shiny::renderUI({
      pdp <- RemixAutoML:::PDPVar(ModelOutputList)
      RemixAutoML::PickerInput(InputID='PDP_Variable1', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'PDP Variable 1'), Choices=pdp$Names, SelectedDefault=pdp$Default, Size=10, SelectedText="count > 1", Multiple=FALSE, ActionBox=TRUE)
    })

    if(Debug) print("Here vv")

    output$PDP_Variable2 <- shiny::renderUI({
      pdp <- RemixAutoML:::PDPVar(ModelOutputList)
      RemixAutoML::PickerInput(InputID='PDP_Variable2', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'PDP Variable 2'), Choices=pdp$Names, SelectedDefault=pdp$Default, Size=10, SelectedText="count > 1", Multiple=FALSE, ActionBox=TRUE)
    })

    if(Debug) print("Here ww")

    output$PDP_Variable3 <- shiny::renderUI({
      pdp <- RemixAutoML:::PDPVar(ModelOutputList)
      RemixAutoML::PickerInput(InputID='PDP_Variable3', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'PDP Variable 3'), Choices=pdp$Names, SelectedDefault=pdp$Default, Size=10, SelectedText="count > 1", Multiple=FALSE, ActionBox=TRUE)
    })

    if(Debug) print("Here xx")

    output$PDP_Variable4 <- shiny::renderUI({
      pdp <- RemixAutoML:::PDPVar(ModelOutputList)
      RemixAutoML::PickerInput(InputID='PDP_Variable4', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'PDP Variable 4'), Choices=pdp$Names, SelectedDefault=pdp$Default, Size=10, SelectedText="count > 1", Multiple=FALSE, ActionBox=TRUE)
    })

    if(Debug) print("Here yy")

    # Percentile Buckets
    output$Percentile_Buckets1 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID='Percentile_Buckets1', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Percentile Buckets 1'), Choices=1:100, SelectedDefault=20, Size=10, SelectedText="count > 1", Multiple=FALSE, ActionBox=TRUE)
    })

    if(Debug) print("Here zz")

    output$Percentile_Buckets2 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID='Percentile_Buckets2', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Percentile Buckets 2'), Choices=1:100, SelectedDefault=20, Size=10, SelectedText= "count > 1", Multiple=FALSE, ActionBox=TRUE)
    })

    if(Debug) print("Here aaa")

    output$Percentile_Buckets3 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID='Percentile_Buckets3', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Percentile Buckets 3'), Choices=1:100, SelectedDefault=20, Size=10, SelectedText= "count > 1", Multiple=FALSE, ActionBox=TRUE)
    })

    if(Debug) print("Here bbb")

    output$Percentile_Buckets4 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID='Percentile_Buckets4', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Percentile Buckets 4'), Choices=1:100, SelectedDefault=20, Size=10, SelectedText= "count > 1", Multiple=FALSE, ActionBox=TRUE)
    })

    if(Debug) print("Here ccc")

    # Add GAM Fit to Plot
    output$GamFitScatter1 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID='GamFitScatter1', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Fit Gam on Scatter or Copula 1'), Choices=c('TRUE','FALSE'), SelectedDefault=FALSE, Multiple=FALSE, ActionBox=TRUE)
    })

    if(Debug) print("Here ddd")

    output$GamFitScatter2 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID='GamFitScatter2', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Fit Gam on Scatter or Copula 2'), Choices=c('TRUE','FALSE'), SelectedDefault=FALSE, Multiple=FALSE, ActionBox=TRUE)
    })

    if(Debug) print("Here eee")

    output$GamFitScatter3 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID='GamFitScatter3', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Fit Gam on Scatter or Copula 3'), Choices=c('TRUE','FALSE'), SelectedDefault=FALSE, Multiple=FALSE, ActionBox=TRUE)
    })

    if(Debug) print("Here fff")

    output$GamFitScatter4 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID='GamFitScatter4', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Fit Gam on Scatter or Copula 4'), Choices=c('TRUE','FALSE'), SelectedDefault=FALSE, Multiple=FALSE, ActionBox=TRUE)
    })

    if(Debug) print("Here ggg")

    # Histogram Bins
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

    if(Debug) print("Here kkk")

    # YTicks Values (NULL is whats handled by RemixAutoML:::YTicks())
    output$YTicks1 <- shiny::renderUI({
      yy <- tryCatch({YVar1()}, error = function(x) 'None')
      if(Debug) print(RemixAutoML:::XTicks(data, xvar=yy))
      RemixAutoML::PickerInput(InputID = 'YTicks1', Label = tags$span(style='color: blue;', 'Y-Axis 1 Ticks'), Choices = RemixAutoML:::YTicks(data, yvar = yy), SelectedDefault = 'Default', Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })

    if(Debug) print("Here lll")

    output$YTicks2 <- shiny::renderUI({
      yy <- tryCatch({YVar2()}, error = function(x) 'None')
      if(Debug) print(RemixAutoML:::XTicks(data, xvar=yy))
      RemixAutoML::PickerInput(InputID = 'YTicks2', Label = tags$span(style='color: blue;', 'Y-Axis 2 Ticks'), Choices = RemixAutoML:::YTicks(data, yvar = yy), SelectedDefault = 'Default', Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })

    if(Debug) print("Here mmm")

    output$YTicks3 <- shiny::renderUI({
      yy <- tryCatch({YVar3()}, error = function(x) 'None')
      if(Debug) print(RemixAutoML:::XTicks(data, xvar=yy))
      RemixAutoML::PickerInput(InputID = 'YTicks3', Label = tags$span(style='color: blue;', 'Y-Axis 3 Ticks'), Choices = RemixAutoML:::YTicks(data, yvar = yy), SelectedDefault = 'Default', Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })

    if(Debug) print("Here nnn")

    output$YTicks4 <- shiny::renderUI({
      yy <- tryCatch({YVar4()}, error = function(x) 'None')
      if(Debug) print(RemixAutoML:::XTicks(data, xvar=yy))
      RemixAutoML::PickerInput(InputID = 'YTicks4', Label = tags$span(style='color: blue;', 'Y-Axis 4 Ticks'), Choices = RemixAutoML:::YTicks(data, yvar = yy), SelectedDefault = 'Default', Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })

    if(Debug) print("Here ooo")

    # XTicks Values ('None' is whats handled by RemixAutoML:::XTicks())
    output$XTicks1 <- shiny::renderUI({
      xx <- tryCatch({XVar1()}, error = function(x) 'None')
      dd <- tryCatch({DateVar1()}, error = function(x) 'None')
      if(Debug) print(RemixAutoML:::XTicks(data, xvar=xx,datevar=dd))
      RemixAutoML::PickerInput(InputID = 'XTicks1', Label = tags$span(style='color: blue;', 'X-Axis 1 Ticks'), Choices = RemixAutoML:::XTicks(data, xvar=xx,datevar=dd), SelectedDefault = 'Default', Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })

    if(Debug) print("Here ppp")

    output$XTicks2 <- shiny::renderUI({
      xx <- tryCatch({XVar2()}, error = function(x) 'None')
      dd <- tryCatch({DateVar2()}, error = function(x) 'None')
      if(Debug) print(RemixAutoML:::XTicks(data, xvar=xx,datevar=dd))
      RemixAutoML::PickerInput(InputID = 'XTicks2', Label = tags$span(style='color: blue;', 'X-Axis 2 Ticks'), Choices = RemixAutoML:::XTicks(data, xvar=xx,datevar=dd), SelectedDefault = 'Default', Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })

    if(Debug) print("Here qqq")

    output$XTicks3 <- shiny::renderUI({
      xx <- tryCatch({XVar3()}, error = function(x) 'None')
      dd <- tryCatch({DateVar3()}, error = function(x) 'None')
      if(Debug) print(RemixAutoML:::XTicks(data, xvar=xx,datevar=dd))
      RemixAutoML::PickerInput(InputID = 'XTicks3', Label = tags$span(style='color: blue;', 'X-Axis 3 Ticks'), Choices = RemixAutoML:::XTicks(data, xvar=xx,datevar=dd), SelectedDefault = 'Default', Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })

    if(Debug) print("Here rrr")

    output$XTicks4 <- shiny::renderUI({
      xx <- tryCatch({XVar4()}, error = function(x) 'None')
      dd <- tryCatch({DateVar4()}, error = function(x) 'None')
      if(Debug) print(RemixAutoML:::XTicks(data, xvar=xx,datevar=dd))
      RemixAutoML::PickerInput(InputID = 'XTicks4', Label = tags$span(style='color: blue;', 'X-Axis 4 Ticks'), Choices = RemixAutoML:::XTicks(data, xvar=xx,datevar=dd), SelectedDefault = 'Default', Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })

    if(Debug) print("Here sss")

    # Other values
    output$AngleY <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID = 'AngleY', Label = tags$span(style='color: blue;', 'Y-axis text angle'), Step = 5, Min = 0, Max = 360, Value = 0)
    })

    if(Debug) print("Here ttt")

    output$AngleX <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID = 'AngleX', Label = tags$span(style='color: blue;', 'X-axis text angle'), Step = 5, Min = 0, Max = 360, Value = 90)
    })

    if(Debug) print("Here uuu")

    output$TextSize <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = 'TextSize', Label = tags$span(style='color: blue;', 'Text Size'),Choices = c(as.character(seq(1,100,1))), SelectedDefault = '14', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here vvv")

    output$OutlierSize <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = 'OutlierSize', Label = tags$span(style='color: blue;', 'Outlier Size'), Choices = c(seq(0.01,2,0.01)), SelectedDefault = 0.01, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here www")

    output$LegendPosition <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = 'LegendPosition', Label = tags$span(style='color: blue;', 'Legend Position'), Choices = c('bottom','left','right','top'), SelectedDefault = 'bottom', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here xxx")

    output$LegendBorderSize <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = 'LegendBorderSize', Label = tags$span(style='color: blue;', 'Legend Border Size'), Choices = c(as.character(seq(0.01,2,0.01))), SelectedDefault = as.character(0.01), Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here yyy")

    output$LegendLineType <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = 'LegendLineType', Label = tags$span(style='color: blue;', 'Legend Border Type'), Choices = c('solid','blank','dashed','dotdash','dotted','longlash','twodash'), SelectedDefault = 'solid', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here zzz")

    # Color boxes
    output$TextColor <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = 'TextColor', Label = tags$span(style='color: blue;', 'Text Color'), Choices = grDevices::colors(), SelectedDefault = 'darkblue', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here aaaa")

    output$ChartColor <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = 'ChartColor', Label = tags$span(style='color: blue;', 'Chart Color'), Choices = grDevices::colors(), SelectedDefault = 'lightsteelblue1', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here bbbb")

    output$GridColor <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = 'GridColor', Label = tags$span(style='color: blue;', 'Grid Lines Color'), Choices = grDevices::colors(), SelectedDefault = 'white', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here cccc")

    output$BackGroundColor <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = 'BackGroundColor', Label = tags$span(style='color: blue;', 'Background Color'), Choices = grDevices::colors(), SelectedDefault = 'gray95', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here dddd")

    output$BorderColor <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = 'BorderColor', Label = tags$span(style='color: blue;', 'Border Color'), Choices = grDevices::colors(), SelectedDefault = 'darkblue', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here eeee")

    output$OutlierColor <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = 'OutlierColor', Label = tags$span(style='color: blue;', 'Outlier Color'), Choices = grDevices::colors(), SelectedDefault = 'blue', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here ffff")

    output$BoxPlotFill <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = 'BoxPlotFill', Label = tags$span(style='color: blue;', 'BoxPlot Fill Color'), Choices = grDevices::colors(), SelectedDefault = 'gray', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here gggg")

    output$SubTitleColor <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = 'SubTitleColor', Label = tags$span(style='color: blue;', 'Subtitle Color'), Choices = grDevices::colors(), SelectedDefault = 'blue', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    # ----

    # ----

    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    # Group Variables                      ----
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

    if(Debug) print("Here hhhh")

    # Select GroupVars
    output$GroupVars1 <- shiny::renderUI({
      if(Debug) print('PickerInput GroupVars1')
      RemixAutoML::PickerInput(InputID='GroupVars1', Label=tags$span(style='color: blue;', 'Select Group Variables 1'), Choices=c('None',names(data)), SelectedDefault=if(!is.null(GroupVariables)) GroupVariables else 'None', SelectedText='count > 1', Multiple=TRUE, ActionBox=TRUE)
    })

    if(Debug) print("Here iiii")

    output$GroupVars2 <- shiny::renderUI({
      if(Debug) print('PickerInput GroupVars2')
      RemixAutoML::PickerInput(InputID='GroupVars2', Label=tags$span(style='color: blue;', 'Select Group Variables 2'), Choices=c('None',names(data)), SelectedDefault=if(!is.null(GroupVariables)) GroupVariables else 'None', SelectedText='count > 1', Multiple=TRUE, ActionBox=TRUE)
    })

    if(Debug) print("Here jjjj")

    output$GroupVars3 <- shiny::renderUI({
      if(Debug) print('PickerInput GroupVars3')
      RemixAutoML::PickerInput(InputID='GroupVars3', Label=tags$span(style='color: blue;', 'Select Group Variables 3'), Choices=c('None',names(data)), SelectedDefault=if(!is.null(GroupVariables)) GroupVariables else 'None', SelectedText='count > 1', Multiple=TRUE, ActionBox=TRUE)
    })

    if(Debug) print("Here kkkk")

    output$GroupVars4 <- shiny::renderUI({
      if(Debug) print('PickerInput GroupVars4')
      RemixAutoML::PickerInput(InputID='GroupVars4', Label=tags$span(style='color: blue;', 'Select Group Variables 4'), Choices=c('None',names(data)), SelectedDefault=if(!is.null(GroupVariables)) GroupVariables else 'None', SelectedText='count > 1', Multiple=TRUE, ActionBox=TRUE)
    })

    if(Debug) print("Here llll")

    # Reactive Group Variables
    SelectedGroups1 <- shiny::reactive({
      RemixAutoML::ReturnParam(input, VarName = 'GroupVars1', Default = if(!is.null(GroupVariables)) GroupVariables else 'None', Switch = TRUE, Type = 'character')
    })

    if(Debug) print("Here mmmm")

    SelectedGroups2 <- shiny::reactive({
      RemixAutoML::ReturnParam(input, VarName = 'GroupVars2', Default = if(!is.null(GroupVariables)) GroupVariables else 'None', Switch = TRUE, Type = 'character')
    })

    if(Debug) print("Here nnnn")

    SelectedGroups3 <- shiny::reactive({
      RemixAutoML::ReturnParam(input, VarName = 'GroupVars3', Default = if(!is.null(GroupVariables)) GroupVariables else 'None', Switch = TRUE, Type = 'character')
    })

    if(Debug) print("Here oooo")

    SelectedGroups4 <- shiny::reactive({
      RemixAutoML::ReturnParam(input, VarName = 'GroupVars4', Default = if(!is.null(GroupVariables)) GroupVariables else 'None', Switch = TRUE, Type = 'character')
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
      RemixAutoML::PickerInput(InputID='FacetVar_1_1', Label = tags$span(style='color: blue;', 'Facet Variable 1 1'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here ccccc")

    output$FacetVar_1_2 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID='FacetVar_1_2', Label = tags$span(style='color: blue;', 'Facet Variable 1 2'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here ddddd")

    output$FacetVar_2_1 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID='FacetVar_2_1', Label = tags$span(style='color: blue;', 'Facet Variable 2 1'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here eeeee")

    output$FacetVar_2_2 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID='FacetVar_2_2', Label = tags$span(style='color: blue;', 'Facet Variable 2 2'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here fffff")

    output$FacetVar_3_1 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID='FacetVar_3_1', Label = tags$span(style='color: blue;', 'Facet Variable 3 1'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here ggggg")

    output$FacetVar_3_2 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID='FacetVar_3_2', Label = tags$span(style='color: blue;', 'Facet Variable 3 2'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here hhhhh")

    output$FacetVar_4_1 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID='FacetVar_4_1', Label = tags$span(style='color: blue;', 'Facet Variable 4 1'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here iiiii")

    output$FacetVar_4_2 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID='FacetVar_4_2', Label = tags$span(style='color: blue;', 'Facet Variable 4 2'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here jjjjj")

    # Sizing
    output$SizeVar1 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = 'SizeVar1', Label = tags$span(style='color: blue;', 'Size Variable 1'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here kkkkk")

    output$SizeVar2 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = 'SizeVar2', Label = tags$span(style='color: blue;', 'Size Variable 2'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here lllll")

    output$SizeVar3 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = 'SizeVar3', Label = tags$span(style='color: blue;', 'Size Variable 3'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    if(Debug) print("Here mmmmm")

    output$SizeVar4 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = 'SizeVar4', Label = tags$span(style='color: blue;', 'Size Variable 4'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    # ----

    # ----

    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    # Filter Variables                     ----
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

    # Filter Variables
    output$FilterVariable_1_1 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterVariable_1_1', label = tags$span(style='color: blue;', 'Filter Variable 1 1'), choices=c('None', names(data)), selected='None')
    })

    if(Debug) print("Here nnnnn")

    output$FilterVariable_1_2 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterVariable_1_2', label = tags$span(style='color: blue;', 'Filter Variable 1 2'), choices=c('None', names(data)), selected='None')
    })

    if(Debug) print("Here ooooo")

    output$FilterVariable_1_3 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterVariable_1_3', label = tags$span(style='color: blue;', 'Filter Variable 1 3'), choices=c('None', names(data)), selected='None')
    })

    if(Debug) print("Here ppppp")

    output$FilterVariable_1_4 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterVariable_1_4', label = tags$span(style='color: blue;', 'Filter Variable 1 4'), choices=c('None', names(data)), selected='None')
    })

    if(Debug) print("Here qqqqq")

    output$FilterVariable_2_1 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterVariable_2_1', label = tags$span(style='color: blue;', 'Filter Variable 2 1'), choices=c('None', names(data)), selected='None')
    })

    if(Debug) print("Here rrrrr")

    output$FilterVariable_2_2 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterVariable_2_2', label = tags$span(style='color: blue;', 'Filter Variable 2 2'), choices=c('None', names(data)), selected='None')
    })

    if(Debug) print("Here sssss")

    output$FilterVariable_2_3 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterVariable_2_3', label = tags$span(style='color: blue;', 'Filter Variable 2 3'), choices=c('None', names(data)), selected='None')
    })

    if(Debug) print("Here ttttt")

    output$FilterVariable_2_4 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterVariable_2_4', label = tags$span(style='color: blue;', 'Filter Variable 2 4'), choices=c('None', names(data)), selected='None')
    })

    if(Debug) print("Here uuuuu")

    output$FilterVariable_3_1 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterVariable_3_1', label = tags$span(style='color: blue;', 'Filter Variable 3 1'), choices=c('None', names(data)), selected='None')
    })

    if(Debug) print("Here vvvvv")

    output$FilterVariable_3_2 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterVariable_3_2', label = tags$span(style='color: blue;', 'Filter Variable 3 2'), choices=c('None', names(data)), selected='None')
    })

    if(Debug) print("Here wwwww")

    output$FilterVariable_3_3 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterVariable_3_3', label = tags$span(style='color: blue;', 'Filter Variable 3 3'), choices=c('None', names(data)), selected='None')
    })

    if(Debug) print("Here xxxxx")

    output$FilterVariable_3_4 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterVariable_3_4', label = tags$span(style='color: blue;', 'Filter Variable 3 4'), choices=c('None', names(data)), selected='None')
    })

    if(Debug) print("Here yyyyy")

    output$FilterVariable_4_1 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterVariable_4_1', label = tags$span(style='color: blue;', 'Filter Variable 4 1'), choices=c('None', names(data)), selected='None')
    })

    if(Debug) print("Here zzzzz")

    output$FilterVariable_4_2 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterVariable_4_2', label = tags$span(style='color: blue;', 'Filter Variable 4 2'), choices=c('None', names(data)), selected='None')
    })

    if(Debug) print("Here aaaaaa")

    output$FilterVariable_4_3 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterVariable_4_3', label = tags$span(style='color: blue;', 'Filter Variable 4 3'), choices=c('None', names(data)), selected='None')
    })

    if(Debug) print("Here bbbbbb")

    output$FilterVariable_4_4 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterVariable_4_4', label = tags$span(style='color: blue;', 'Filter Variable 4 4'), choices=c('None', names(data)), selected='None')
    })

    # ----

    # ----

    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    # Filter Logic                         ----
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

    if(Debug) print("Here cccccc")

    # Filter Logics
    output$FilterLogic_1_1 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_1_1', selected=RemixAutoML:::FL_Default(data, x=input[['FilterVariable_1_1']]), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })

    if(Debug) print("Here dddddd")

    output$FilterLogic_1_2 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_1_2', selected=RemixAutoML:::FL_Default(data, x=input[['FilterVariable_1_2']]), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })

    if(Debug) print("Here eeeeee")

    output$FilterLogic_1_3 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_1_3', selected=RemixAutoML:::FL_Default(data, x=input[['FilterVariable_1_3']]), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })

    if(Debug) print("Here ffffff")

    output$FilterLogic_1_4 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_1_4', selected=RemixAutoML:::FL_Default(data, x=input[['FilterVariable_1_4']]), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })

    if(Debug) print("Here gggggg")

    output$FilterLogic_2_1 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_2_1', selected=RemixAutoML:::FL_Default(data, x=input[['FilterVariable_2_1']]), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })

    if(Debug) print("Here hhhhhh")

    output$FilterLogic_2_2 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_2_2', selected=RemixAutoML:::FL_Default(data, x=input[['FilterVariable_2_2']]), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })

    if(Debug) print("Here iiiiii")

    output$FilterLogic_2_3 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_2_3', selected=RemixAutoML:::FL_Default(data, x=input[['FilterVariable_2_3']]), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })

    if(Debug) print("Here jjjjjj")

    output$FilterLogic_2_4 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_2_4', selected=RemixAutoML:::FL_Default(data, x=input[['FilterVariable_2_4']]), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })

    if(Debug) print("Here kkkkkk")

    output$FilterLogic_3_1 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_3_1', selected=RemixAutoML:::FL_Default(data, x=input[['FilterVariable_3_1']]), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })

    if(Debug) print("Here llllll")

    output$FilterLogic_3_2 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_3_2', selected=RemixAutoML:::FL_Default(data, x=input[['FilterVariable_3_2']]), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })

    if(Debug) print("Here mmmmmm")

    output$FilterLogic_3_3 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_3_3', selected=RemixAutoML:::FL_Default(data, x=input[['FilterVariable_3_3']]), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })

    if(Debug) print("Here nnnnnn")

    output$FilterLogic_3_4 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_3_4', selected=RemixAutoML:::FL_Default(data, x=input[['FilterVariable_3_4']]), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })

    if(Debug) print("Here oooooo")

    output$FilterLogic_4_1 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_4_1', selected=RemixAutoML:::FL_Default(data, x=input[['FilterVariable_4_1']]), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })

    if(Debug) print("Here pppppp")

    output$FilterLogic_4_2 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_4_2', selected=RemixAutoML:::FL_Default(data, x=input[['FilterVariable_4_2']]), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })

    if(Debug) print("Here qqqqqq")

    output$FilterLogic_4_3 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_4_3', selected=RemixAutoML:::FL_Default(data, x=input[['FilterVariable_4_3']]), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })

    if(Debug) print("Here rrrrrr")

    output$FilterLogic_4_4 <- shiny::renderUI({
      shiny::selectInput(inputId='FilterLogic_4_4', selected=RemixAutoML:::FL_Default(data, x=input[['FilterVariable_4_4']]), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
    })

    # ----

    # ----

    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    # Filter Values                        ----
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

    if(Debug) print("Here ssssss")

    # Filter Values
    #
    # 1_1_1 means Plot 1, Filter Var 1, Filter Value min
    # 1_1_2 means Plot 1, Filter Var 1, filter value max
    #
    # Plot 1
    output$FilterValue_1_1_1 <- shiny::renderUI({
      params <- list(data=data, VarName=input[['FilterVariable_1_1']], type=1); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
      RemixAutoML::PickerInput(InputID='FilterValue_1_1_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
    })

    if(Debug) print("Here tttttt")

    output$FilterValue_1_1_2 <- shiny::renderUI({
      params <- list(data=data, VarName=input[['FilterVariable_1_1']], type=2); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
      RemixAutoML::PickerInput(InputID='FilterValue_1_1_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
    })

    if(Debug) print("Here uuuuuu")

    output$FilterValue_1_2_1 <- shiny::renderUI({
      params <- list(data=data, VarName=input[['FilterVariable_1_2']], type=1); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
      RemixAutoML::PickerInput(InputID='FilterValue_1_2_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
    })

    if(Debug) print("Here vvvvvv")

    output$FilterValue_1_2_2 <- shiny::renderUI({
      params <- list(data=data, VarName=input[['FilterVariable_1_2']], type=2); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
      RemixAutoML::PickerInput(InputID='FilterValue_1_2_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
    })

    if(Debug) print("Here wwwwww")

    output$FilterValue_1_3_1 <- shiny::renderUI({
      params <- list(data=data, VarName=input[['FilterVariable_1_3']], type=1); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
      RemixAutoML::PickerInput(InputID='FilterValue_1_3_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
    })

    if(Debug) print("Here xxxxxx")

    output$FilterValue_1_3_2 <- shiny::renderUI({
      params <- list(data=data, VarName=input[['FilterVariable_1_3']], type=2); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
      RemixAutoML::PickerInput(InputID='FilterValue_1_3_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
    })

    if(Debug) print("Here yyyyyy")

    output$FilterValue_1_4_1 <- shiny::renderUI({
      params <- list(data=data, VarName=input[['FilterVariable_1_4']], type=1); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
      RemixAutoML::PickerInput(InputID='FilterValue_1_4_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
    })

    if(Debug) print("Here zzzzzz")

    output$FilterValue_1_4_2 <- shiny::renderUI({
      params <- list(data=data, VarName=input[['FilterVariable_1_4']], type=2); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
      RemixAutoML::PickerInput(InputID='FilterValue_1_4_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
    })

    if(Debug) print("Here aaaaaaa")

    # Plot 2
    output$FilterValue_2_1_1 <- shiny::renderUI({
      params <- list(data=data, VarName=input[['FilterVariable_2_1']], type=1); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
      RemixAutoML::PickerInput(InputID='FilterValue_2_1_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
    })

    if(Debug) print("Here bbbbbbb")

    output$FilterValue_2_1_2 <- shiny::renderUI({
      params <- list(data=data, VarName=input[['FilterVariable_2_1']], type=2); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
      RemixAutoML::PickerInput(InputID='FilterValue_2_1_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
    })

    if(Debug) print("Here ccccccc")

    output$FilterValue_2_2_1 <- shiny::renderUI({
      params <- list(data=data, VarName=input[['FilterVariable_2_2']], type=1); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
      RemixAutoML::PickerInput(InputID='FilterValue_2_2_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
    })

    if(Debug) print("Here ddddddd")

    output$FilterValue_2_2_2 <- shiny::renderUI({
      params <- list(data=data, VarName=input[['FilterVariable_2_2']], type=2); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
      RemixAutoML::PickerInput(InputID='FilterValue_2_2_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
    })

    if(Debug) print("Here eeeeeee")

    output$FilterValue_2_3_1 <- shiny::renderUI({
      params <- list(data=data, VarName=input[['FilterVariable_2_3']], type=1); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
      RemixAutoML::PickerInput(InputID='FilterValue_2_3_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
    })

    if(Debug) print("Here fffffff")

    output$FilterValue_2_3_2 <- shiny::renderUI({
      params <- list(data=data, VarName=input[['FilterVariable_2_3']], type=2); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
      RemixAutoML::PickerInput(InputID='FilterValue_2_3_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
    })

    if(Debug) print("Here ggggggg")

    output$FilterValue_2_4_1 <- shiny::renderUI({
      params <- list(data=data, VarName=input[['FilterVariable_2_4']], type=1); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
      RemixAutoML::PickerInput(InputID='FilterValue_2_4_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
    })

    if(Debug) print("Here hhhhhhh")

    output$FilterValue_2_4_2 <- shiny::renderUI({
      params <- list(data=data, VarName=input[['FilterVariable_2_4']], type=2); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
      RemixAutoML::PickerInput(InputID='FilterValue_2_4_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
    })

    if(Debug) print("Here qqqqqqq")

    # Plot 3
    output$FilterValue_3_1_1 <- shiny::renderUI({
      params <- list(data=data, VarName=input[['FilterVariable_3_1']], type=1); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
      RemixAutoML::PickerInput(InputID='FilterValue_3_1_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
    })

    if(Debug) print("Here rrrrrrr")

    output$FilterValue_3_1_2 <- shiny::renderUI({
      params <- list(data=data, VarName=input[['FilterVariable_3_1']], type=2); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
      RemixAutoML::PickerInput(InputID='FilterValue_3_1_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
    })

    if(Debug) print("Here sssssss")

    output$FilterValue_3_2_1 <- shiny::renderUI({
      params <- list(data=data, VarName=input[['FilterVariable_3_2']], type=1); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
      RemixAutoML::PickerInput(InputID='FilterValue_3_2_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
    })

    if(Debug) print("Here ttttttt")

    output$FilterValue_3_2_2 <- shiny::renderUI({
      params <- list(data=data, VarName=input[['FilterVariable_3_2']], type=2); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
      RemixAutoML::PickerInput(InputID='FilterValue_3_2_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
    })

    if(Debug) print("Here uuuuuuu")

    output$FilterValue_3_3_1 <- shiny::renderUI({
      params <- list(data=data, VarName=input[['FilterVariable_3_3']], type=1); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
      RemixAutoML::PickerInput(InputID='FilterValue_3_3_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
    })

    if(Debug) print("Here vvvvvvv")

    output$FilterValue_3_3_2 <- shiny::renderUI({
      params <- list(data=data, VarName=input[['FilterVariable_3_3']], type=2); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
      RemixAutoML::PickerInput(InputID='FilterValue_3_3_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
    })

    if(Debug) print("Here wwwwwww")

    output$FilterValue_3_4_1 <- shiny::renderUI({
      params <- list(data=data, VarName=input[['FilterVariable_3_4']], type=1); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
      RemixAutoML::PickerInput(InputID='FilterValue_3_4_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
    })

    if(Debug) print("Here xxxxxxx")

    output$FilterValue_3_4_2 <- shiny::renderUI({
      params <- list(data=data, VarName=input[['FilterVariable_3_4']], type=2); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
      RemixAutoML::PickerInput(InputID='FilterValue_3_4_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
    })

    if(Debug) print("Here yyyyyyy")

    # Plot 4
    output$FilterValue_4_1_1 <- shiny::renderUI({
      params <- list(data=data, VarName=input[['FilterVariable_4_1']], type=1); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
      RemixAutoML::PickerInput(InputID='FilterValue_4_1_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
    })

    if(Debug) print("Here zzzzzzz")

    output$FilterValue_4_1_2 <- shiny::renderUI({
      params <- list(data=data, VarName=input[['FilterVariable_4_1']], type=2); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
      RemixAutoML::PickerInput(InputID='FilterValue_4_1_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
    })

    if(Debug) print("Here aaaaaaaa")

    output$FilterValue_4_2_1 <- shiny::renderUI({
      params <- list(data=data, VarName=input[['FilterVariable_4_2']], type=1); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
      RemixAutoML::PickerInput(InputID='FilterValue_4_2_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
    })

    if(Debug) print("Here bbbbbbbb")

    output$FilterValue_4_2_2 <- shiny::renderUI({
      params <- list(data=data, VarName=input[['FilterVariable_4_2']], type=2); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
      RemixAutoML::PickerInput(InputID='FilterValue_4_2_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
    })

    if(Debug) print("Here cccccccc")

    output$FilterValue_4_3_1 <- shiny::renderUI({
      params <- list(data=data, VarName=input[['FilterVariable_4_3']], type=1); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
      RemixAutoML::PickerInput(InputID='FilterValue_4_3_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
    })

    if(Debug) print("Here dddddddd")

    output$FilterValue_4_3_2 <- shiny::renderUI({
      params <- list(data=data, VarName=input[['FilterVariable_4_3']], type=2); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
      RemixAutoML::PickerInput(InputID='FilterValue_4_3_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
    })

    if(Debug) print("Here eeeeeeee")

    output$FilterValue_4_4_1 <- shiny::renderUI({
      params <- list(data=data, VarName=input[['FilterVariable_4_4']], type=1); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
      RemixAutoML::PickerInput(InputID='FilterValue_4_4_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
    })

    if(Debug) print("Here ffffffff")

    output$FilterValue_4_4_2 <- shiny::renderUI({
      params <- list(data=data, VarName=input[['FilterVariable_4_4']], type=2); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
      RemixAutoML::PickerInput(InputID='FilterValue_4_4_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
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
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "success", btn_colors = "green", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
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

  # YVars
  output$YVar1 <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'YVar1', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Y-Variable 1'), Choices = c('None', names(data)), SelectedDefault = YVariable, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here b")

  output$YVar2 <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'YVar2', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Y-Variable 2'), Choices = c('None', names(data)), SelectedDefault = YVariable, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here c")

  output$YVar3 <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'YVar3', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Y-Variable 3'), Choices = c('None', names(data)), SelectedDefault = YVariable, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here d")

  output$YVar4 <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'YVar4', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Y-Variable 4'), Choices = c('None', names(data)), SelectedDefault = YVariable, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here e")

  # 'X-Variables'
  output$XVar1 <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'XVar1', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'X-Variable 1'), Choices = c('None', names(data)), SelectedDefault = XVariable, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here f")

  output$XVar2 <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'XVar2', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'X-Variable 2'), Choices = c('None', names(data)), SelectedDefault = XVariable, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here g")

  output$XVar3 <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'XVar3', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'X-Variable 3'), Choices = c('None', names(data)), SelectedDefault = XVariable, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here h")

  output$XVar4 <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'XVar4', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'X-Variable 4'), Choices = c('None', names(data)), SelectedDefault = XVariable, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here i")

  # 'Score-Variables'
  output$ScoreVar1 <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'ScoreVar1', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Scoring-Variable 1'), Choices = c('None', names(data)), SelectedDefault = XVariable, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here j")

  output$ScoreVar2 <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'ScoreVar2', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Scoring-Variable 2'), Choices = c('None', names(data)), SelectedDefault = XVariable, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here k")

  output$ScoreVar3 <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'ScoreVar3', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Scoring-Variable 3'), Choices = c('None', names(data)), SelectedDefault = XVariable, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here l")

  output$ScoreVar4 <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'ScoreVar4', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Scoring-Variable 4'), Choices = c('None', names(data)), SelectedDefault = XVariable, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here m")

  # 'Date Variables'
  output$DateVar1 <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'DateVar1', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Date-Variable 1'), Choices = c('None', names(data)), SelectedDefault = DateName, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here n")

  output$DateVar2 <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'DateVar2', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Date-Variable 2'), Choices = c('None', names(data)), SelectedDefault = DateName, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here o")

  output$DateVar3 <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'DateVar3', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Date-Variable 3'), Choices = c('None', names(data)), SelectedDefault = DateName, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here p")

  output$DateVar4 <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'DateVar4', Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Date-Variable 4'), Choices = c('None', names(data)), SelectedDefault = DateName, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
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

  # Y-Mins
  output$YMin1 <- shiny::renderUI({
    if(Debug) print('YMin 1 PickerInput');
    choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = eval(YVar1()))$ChoiceInput}, error = function(x) NULL)
    RemixAutoML::PickerInput(InputID='YMin1', Multiple=FALSE, Debug=Debug, Label=tags$span(style=paste0('color: ', AppTextColor, ';'), 'Min Y1-Value'), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
  })

  if(Debug) print("Here s")

  output$YMin2 <- shiny::renderUI({
    if(Debug) print('YMin 2 PickerInput');
    choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = eval(YVar2()))$ChoiceInput}, error = function(x) NULL)
    RemixAutoML::PickerInput(InputID='YMin2', Multiple=FALSE, Debug=Debug, Label=tags$span(style=paste0('color: ', AppTextColor, ';'), 'Min Y2-Value'), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
  })

  if(Debug) print("Here t")

  output$YMin3 <- shiny::renderUI({
    if(Debug) print('YMin 3 PickerInput');
    choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = eval(YVar3()))$ChoiceInput}, error = function(x) NULL)
    RemixAutoML::PickerInput(InputID='YMin3', Multiple=FALSE, Debug=Debug, Label=tags$span(style=paste0('color: ', AppTextColor, ';'), 'Min Y3-Value'), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
  })

  if(Debug) print("Here u")

  output$YMin4 <- shiny::renderUI({
    if(Debug) print('YMin 4 PickerInput');
    choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = eval(YVar4()))$ChoiceInput}, error = function(x) NULL)
    RemixAutoML::PickerInput(InputID='YMin4', Multiple=FALSE, Debug=Debug, Label=tags$span(style=paste0('color: ', AppTextColor, ';'), 'Min Y4-Value'), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
  })

  if(Debug) print("Here v")

  # X-Mins
  output$XMin1 <- shiny::renderUI({
    if(Debug) print('XMin 1 PickerInput');
    choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = eval(XVar1()))$ChoiceInput}, error = function(x) NULL)
    RemixAutoML::PickerInput(InputID='XMin1', Multiple=FALSE, Debug=Debug, Label=tags$span(style=paste0('color: ', AppTextColor, ';'), 'Min X1-Value'), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
  })

  if(Debug) print("Here w")

  output$XMin2 <- shiny::renderUI({
    if(Debug) print('XMin 2 PickerInput');
    choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = eval(XVar2()))$ChoiceInput}, error = function(x) NULL)
    RemixAutoML::PickerInput(InputID='XMin2', Multiple=FALSE, Debug=Debug, Label=tags$span(style=paste0('color: ', AppTextColor, ';'), 'Min X2-Value'), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
  })

  if(Debug) print("Here x")

  output$XMin3 <- shiny::renderUI({
    if(Debug) print('XMin 3 PickerInput');
    choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = eval(XVar3()))$ChoiceInput}, error = function(x) NULL)
    RemixAutoML::PickerInput(InputID='XMin3', Multiple=FALSE, Debug=Debug, Label=tags$span(style=paste0('color: ', AppTextColor, ';'), 'Min X3-Value'), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
  })

  if(Debug) print("Here y")

  output$XMin4 <- shiny::renderUI({
    if(Debug) print('XMin 4 PickerInput');
    choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = eval(XVar4()))$ChoiceInput}, error = function(x) NULL)
    RemixAutoML::PickerInput(InputID='XMin4', Multiple=FALSE, Debug=Debug, Label=tags$span(style=paste0('color: ', AppTextColor, ';'), 'Min X4-Value'), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
  })

  if(Debug) print("Here z")

  # Y-Maxs
  output$YMax1 <- shiny::renderUI({
    if(Debug) print('YMax 1 PickerInput');
    choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = eval(YVar1()))$ChoiceInput}, error = function(x) NULL)
    RemixAutoML::PickerInput(InputID='YMax1', Multiple=FALSE, Debug=Debug, Label=tags$span(style=paste0('color: ', AppTextColor, ';'), 'Max Y1-Value'), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
  })

  if(Debug) print("Here aa")

  output$YMax2 <- shiny::renderUI({
    if(Debug) print('YMax 2 PickerInput');
    choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = eval(YVar2()))$ChoiceInput}, error = function(x) NULL)
    RemixAutoML::PickerInput(InputID='YMax2', Multiple=FALSE, Debug=Debug, Label=tags$span(style=paste0('color: ', AppTextColor, ';'), 'Max Y2-Value'), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
  })

  if(Debug) print("Here bb")

  output$YMax3 <- shiny::renderUI({
    if(Debug) print('YMax 3 PickerInput');
    choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = eval(YVar3()))$ChoiceInput}, error = function(x) NULL)
    RemixAutoML::PickerInput(InputID='YMax3', Multiple=FALSE, Debug=Debug, Label=tags$span(style=paste0('color: ', AppTextColor, ';'), 'Max Y3-Value'), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
  })

  if(Debug) print("Here cc")

  output$YMax4 <- shiny::renderUI({
    if(Debug) print('YMax 4 PickerInput');
    choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = eval(YVar4()))$ChoiceInput}, error = function(x) NULL)
    RemixAutoML::PickerInput(InputID='YMax4', Multiple=FALSE, Debug=Debug, Label=tags$span(style=paste0('color: ', AppTextColor, ';'), 'Max Y4-Value'), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
  })

  if(Debug) print("Here dd")

  # X-Maxs
  output$XMax1 <- shiny::renderUI({
    if(Debug) print('XMax 1 PickerInput');
    choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = eval(XVar1()))$ChoiceInput}, error = function(x) NULL)
    RemixAutoML::PickerInput(InputID='XMax1', Multiple=FALSE, Debug=Debug, Label=tags$span(style=paste0('color: ', AppTextColor, ';'), 'Max X1-Value'), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
  })

  if(Debug) print("Here ee")

  output$XMax2 <- shiny::renderUI({
    if(Debug) print('XMax 2 PickerInput');
    choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = eval(XVar2()))$ChoiceInput}, error = function(x) NULL)
    RemixAutoML::PickerInput(InputID='XMax2', Multiple=FALSE, Debug=Debug, Label=tags$span(style=paste0('color: ', AppTextColor, ';'), 'Max X2-Value'), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
  })

  if(Debug) print("Here ff")

  output$XMax3 <- shiny::renderUI({
    if(Debug) print('XMax 3 PickerInput');
    choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = eval(XVar3()))$ChoiceInput}, error = function(x) NULL)
    RemixAutoML::PickerInput(InputID='XMax3', Multiple=FALSE, Debug=Debug, Label=tags$span(style=paste0('color: ', AppTextColor, ';'), 'Max X3-Value'), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
  })

  if(Debug) print("Here gg")

  output$XMax4 <- shiny::renderUI({
    if(Debug) print('XMax 4 PickerInput');
    choices <- tryCatch({RemixAutoML::KeyVarsInit(data, VarName = eval(XVar4()))$ChoiceInput}, error = function(x) NULL)
    RemixAutoML::PickerInput(InputID='XMax4', Multiple=FALSE, Debug=Debug, Label=tags$span(style=paste0('color: ', AppTextColor, ';'), 'Max X4-Value'), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
  })

  if(Debug) print("Here hh")

  # Date-Mins
  output$DateMin1 <- shiny::renderUI({
    if(Debug) print('DateMin 1 PickerInput');
    choices <- RemixAutoML::KeyVarsInit(data, VarName = eval(tryCatch({DateVar1()}, error = function(x) NULL)))$ChoiceInput
    RemixAutoML::PickerInput(InputID='DateMin1', Multiple=FALSE, Debug=Debug, Label=tags$span(style=paste0('color: ', AppTextColor, ';'), 'Min Date-Value 1'), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
  })

  if(Debug) print("Here ii")

  output$DateMin2 <- shiny::renderUI({
    if(Debug) print('DateMin 2 PickerInput');
    choices <- RemixAutoML::KeyVarsInit(data, VarName = eval(tryCatch({DateVar2()}, error = function(x) NULL)))$ChoiceInput
    RemixAutoML::PickerInput(InputID='DateMin2', Multiple=FALSE, Debug=Debug, Label=tags$span(style=paste0('color: ', AppTextColor, ';'), 'Min Date-Value 2'), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
  })

  if(Debug) print("Here jj")

  output$DateMin3 <- shiny::renderUI({
    if(Debug) print('DateMin 3 PickerInput');
    choices <- RemixAutoML::KeyVarsInit(data, VarName = eval(tryCatch({DateVar3()}, error = function(x) NULL)))$ChoiceInput
    RemixAutoML::PickerInput(InputID='DateMin3', Multiple=FALSE, Debug=Debug, Label=tags$span(style=paste0('color: ', AppTextColor, ';'), 'Min Date-Value 3'), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
  })

  if(Debug) print("Here kk")

  output$DateMin4 <- shiny::renderUI({
    if(Debug) print('DateMin 4 PickerInput');
    choices <- RemixAutoML::KeyVarsInit(data, VarName = eval(tryCatch({DateVar4()}, error = function(x) NULL)))$ChoiceInput
    RemixAutoML::PickerInput(InputID='DateMin4', Multiple=FALSE, Debug=Debug, Label=tags$span(style=paste0('color: ', AppTextColor, ';'), 'Min Date-Value 4'), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[1L]))
  })

  if(Debug) print("Here ll")

  # Date-Maxs
  output$DateMax1 <- shiny::renderUI({
    if(Debug) print('DateMax 1 PickerInput');
    choices <- RemixAutoML::KeyVarsInit(data, VarName = eval(tryCatch({DateVar1()}, error = function(x) NULL)))$ChoiceInput
    RemixAutoML::PickerInput(InputID='DateMax1', Multiple=FALSE, Debug=Debug, Label=tags$span(style=paste0('color: ', AppTextColor, ';'), 'Max Date-Value 1'), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
  })

  if(Debug) print("Here mm")

  output$DateMax2 <- shiny::renderUI({
    if(Debug) print('DateMax 2 PickerInput');
    choices <- RemixAutoML::KeyVarsInit(data, VarName = eval(tryCatch({DateVar2()}, error = function(x) NULL)))$ChoiceInput
    RemixAutoML::PickerInput(InputID='DateMax2', Multiple=FALSE, Debug=Debug, Label=tags$span(style=paste0('color: ', AppTextColor, ';'), 'Max Date-Value 2'), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
  })

  if(Debug) print("Here oo")

  output$DateMax3 <- shiny::renderUI({
    if(Debug) print('DateMax 3 PickerInput');
    choices <- RemixAutoML::KeyVarsInit(data, VarName = eval(tryCatch({DateVar3()}, error = function(x) NULL)))$ChoiceInput
    RemixAutoML::PickerInput(InputID='DateMax3', Multiple=FALSE, Debug=Debug, Label=tags$span(style=paste0('color: ', AppTextColor, ';'), 'Max Date-Value 3'), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
  })

  if(Debug) print("Here pp")

  output$DateMax4 <- shiny::renderUI({
    if(Debug) print('DateMax 4 PickerInput');
    choices <- RemixAutoML::KeyVarsInit(data, VarName = eval(tryCatch({DateVar4()}, error = function(x) NULL)))$ChoiceInput
    RemixAutoML::PickerInput(InputID='DateMax4', Multiple=FALSE, Debug=Debug, Label=tags$span(style=paste0('color: ', AppTextColor, ';'), 'Max Date-Value 4'), Choices=RemixAutoML::CharNull(choices), SelectedDefault=RemixAutoML::CharNull(choices[length(choices)]))
  })

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Plotting MetaData                    ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  if(Debug) print("Here qq")

  # Dragula for PlotType
  output$PlotTypeDragula <- shiny::renderUI({
    x <- RemixAutoML:::AvailableAppInsightsPlots(x = ModelOutputList)
    esquisse::dragulaInput(
      inputId = 'PlotTypeDragula',
      label = 'Drag and Drop Plot Types',
      sourceLabel = 'Plots',
      targetsLabels = c('UpperLeftPlot', 'BottomLeftPlot', 'UpperRightPlot', 'BottomRightPlot'),
      #style="color: #fff; background-color: #e95420; border-color: #c34113; border-radius: 10px; border-width: 2px",
      choices = sort(c(x,x,x,x))
      #,
      # choiceNames = , choiceValues = , selected = , status = , replace = , copySource = , badge = , ncolSource = , ncolGrid = , dragulaOpts = , boxStyle = , width = "100%", height = "100%"
    )
  })

  if(Debug) print("Here rr")

  # UI Plot Options
  output$NumberGroupsDisplay <- shiny::renderUI({
    RemixAutoML::NumericInput(InputID = 'NumberGroupsDisplay', Label = tags$span(style='color: blue;', 'Dispay N Levels'), Step = 1L, Value = 5L, Min = 1L, Max = 100L)
  })

  if(Debug) print("Here ss")

  output$PlotWidth <- shiny::renderUI({
    RemixAutoML::NumericInput(InputID = "PlotWidth", Label=tags$span(style='color: blue;', 'Plot Width'), Step = 50, Min = 800, Max = 1800, Value = 1600)
  })

  if(Debug) print("Here tt")

  output$PlotHeight <- shiny::renderUI({
    RemixAutoML::NumericInput(InputID = "PlotHeight", Label=tags$span(style='color: blue;', 'Plot Height'), Step = 25, Min = 350, Max = 350*10, Value = 500)
  })

  if(Debug) print("Here uu")

  # PDP Variables
  output$PDP_Variable1 <- shiny::renderUI({
    pdp <- RemixAutoML:::PDPVar(ModelOutputList)
    RemixAutoML::PickerInput(InputID='PDP_Variable1', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'PDP Variable 1'), Choices=pdp$Names, SelectedDefault=pdp$Default, Size=10, SelectedText="count > 1", Multiple=FALSE, ActionBox=TRUE)
  })

  if(Debug) print("Here vv")

  output$PDP_Variable2 <- shiny::renderUI({
    pdp <- RemixAutoML:::PDPVar(ModelOutputList)
    RemixAutoML::PickerInput(InputID='PDP_Variable2', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'PDP Variable 2'), Choices=pdp$Names, SelectedDefault=pdp$Default, Size=10, SelectedText="count > 1", Multiple=FALSE, ActionBox=TRUE)
  })

  if(Debug) print("Here ww")

  output$PDP_Variable3 <- shiny::renderUI({
    pdp <- RemixAutoML:::PDPVar(ModelOutputList)
    RemixAutoML::PickerInput(InputID='PDP_Variable3', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'PDP Variable 3'), Choices=pdp$Names, SelectedDefault=pdp$Default, Size=10, SelectedText="count > 1", Multiple=FALSE, ActionBox=TRUE)
  })

  if(Debug) print("Here xx")

  output$PDP_Variable4 <- shiny::renderUI({
    pdp <- RemixAutoML:::PDPVar(ModelOutputList)
    RemixAutoML::PickerInput(InputID='PDP_Variable4', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'PDP Variable 4'), Choices=pdp$Names, SelectedDefault=pdp$Default, Size=10, SelectedText="count > 1", Multiple=FALSE, ActionBox=TRUE)
  })

  if(Debug) print("Here yy")

  # Percentile Buckets
  output$Percentile_Buckets1 <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID='Percentile_Buckets1', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Percentile Buckets 1'), Choices=1:100, SelectedDefault=20, Size=10, SelectedText="count > 1", Multiple=FALSE, ActionBox=TRUE)
  })

  if(Debug) print("Here zz")

  output$Percentile_Buckets2 <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID='Percentile_Buckets2', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Percentile Buckets 2'), Choices=1:100, SelectedDefault=20, Size=10, SelectedText= "count > 1", Multiple=FALSE, ActionBox=TRUE)
  })

  if(Debug) print("Here aaa")

  output$Percentile_Buckets3 <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID='Percentile_Buckets3', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Percentile Buckets 3'), Choices=1:100, SelectedDefault=20, Size=10, SelectedText= "count > 1", Multiple=FALSE, ActionBox=TRUE)
  })

  if(Debug) print("Here bbb")

  output$Percentile_Buckets4 <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID='Percentile_Buckets4', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Percentile Buckets 4'), Choices=1:100, SelectedDefault=20, Size=10, SelectedText= "count > 1", Multiple=FALSE, ActionBox=TRUE)
  })

  if(Debug) print("Here ccc")

  # Add GAM Fit to Plot
  output$GamFitScatter1 <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID='GamFitScatter1', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Fit Gam on Scatter or Copula 1'), Choices=c('TRUE','FALSE'), SelectedDefault=FALSE, Multiple=FALSE, ActionBox=TRUE)
  })

  if(Debug) print("Here ddd")

  output$GamFitScatter2 <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID='GamFitScatter2', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Fit Gam on Scatter or Copula 2'), Choices=c('TRUE','FALSE'), SelectedDefault=FALSE, Multiple=FALSE, ActionBox=TRUE)
  })

  if(Debug) print("Here eee")

  output$GamFitScatter3 <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID='GamFitScatter3', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Fit Gam on Scatter or Copula 3'), Choices=c('TRUE','FALSE'), SelectedDefault=FALSE, Multiple=FALSE, ActionBox=TRUE)
  })

  if(Debug) print("Here fff")

  output$GamFitScatter4 <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID='GamFitScatter4', Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Fit Gam on Scatter or Copula 4'), Choices=c('TRUE','FALSE'), SelectedDefault=FALSE, Multiple=FALSE, ActionBox=TRUE)
  })

  if(Debug) print("Here ggg")

  # Histogram Bins
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

  if(Debug) print("Here kkk")

  # YTicks Values (NULL is whats handled by RemixAutoML:::YTicks())
  output$YTicks1 <- shiny::renderUI({
    yy <- tryCatch({YVar1()}, error = function(x) 'None')
    if(Debug) print(RemixAutoML:::XTicks(data, xvar=yy))
    RemixAutoML::PickerInput(InputID = 'YTicks1', Label = tags$span(style='color: blue;', 'Y-Axis 1 Ticks'), Choices = RemixAutoML:::YTicks(data, yvar = yy), SelectedDefault = 'Default', Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
  })

  if(Debug) print("Here lll")

  output$YTicks2 <- shiny::renderUI({
    yy <- tryCatch({YVar2()}, error = function(x) 'None')
    if(Debug) print(RemixAutoML:::XTicks(data, xvar=yy))
    RemixAutoML::PickerInput(InputID = 'YTicks2', Label = tags$span(style='color: blue;', 'Y-Axis 2 Ticks'), Choices = RemixAutoML:::YTicks(data, yvar = yy), SelectedDefault = 'Default', Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
  })

  if(Debug) print("Here mmm")

  output$YTicks3 <- shiny::renderUI({
    yy <- tryCatch({YVar3()}, error = function(x) 'None')
    if(Debug) print(RemixAutoML:::XTicks(data, xvar=yy))
    RemixAutoML::PickerInput(InputID = 'YTicks3', Label = tags$span(style='color: blue;', 'Y-Axis 3 Ticks'), Choices = RemixAutoML:::YTicks(data, yvar = yy), SelectedDefault = 'Default', Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
  })

  if(Debug) print("Here nnn")

  output$YTicks4 <- shiny::renderUI({
    yy <- tryCatch({YVar4()}, error = function(x) 'None')
    if(Debug) print(RemixAutoML:::XTicks(data, xvar=yy))
    RemixAutoML::PickerInput(InputID = 'YTicks4', Label = tags$span(style='color: blue;', 'Y-Axis 4 Ticks'), Choices = RemixAutoML:::YTicks(data, yvar = yy), SelectedDefault = 'Default', Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
  })

  if(Debug) print("Here ooo")

  # XTicks Values ('None' is whats handled by RemixAutoML:::XTicks())
  output$XTicks1 <- shiny::renderUI({
    xx <- tryCatch({XVar1()}, error = function(x) 'None')
    dd <- tryCatch({DateVar1()}, error = function(x) 'None')
    if(Debug) print(RemixAutoML:::XTicks(data, xvar=xx,datevar=dd))
    RemixAutoML::PickerInput(InputID = 'XTicks1', Label = tags$span(style='color: blue;', 'X-Axis 1 Ticks'), Choices = RemixAutoML:::XTicks(data, xvar=xx,datevar=dd), SelectedDefault = 'Default', Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
  })

  if(Debug) print("Here ppp")

  output$XTicks2 <- shiny::renderUI({
    xx <- tryCatch({XVar2()}, error = function(x) 'None')
    dd <- tryCatch({DateVar2()}, error = function(x) 'None')
    if(Debug) print(RemixAutoML:::XTicks(data, xvar=xx,datevar=dd))
    RemixAutoML::PickerInput(InputID = 'XTicks2', Label = tags$span(style='color: blue;', 'X-Axis 2 Ticks'), Choices = RemixAutoML:::XTicks(data, xvar=xx,datevar=dd), SelectedDefault = 'Default', Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
  })

  if(Debug) print("Here qqq")

  output$XTicks3 <- shiny::renderUI({
    xx <- tryCatch({XVar3()}, error = function(x) 'None')
    dd <- tryCatch({DateVar3()}, error = function(x) 'None')
    if(Debug) print(RemixAutoML:::XTicks(data, xvar=xx,datevar=dd))
    RemixAutoML::PickerInput(InputID = 'XTicks3', Label = tags$span(style='color: blue;', 'X-Axis 3 Ticks'), Choices = RemixAutoML:::XTicks(data, xvar=xx,datevar=dd), SelectedDefault = 'Default', Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
  })

  if(Debug) print("Here rrr")

  output$XTicks4 <- shiny::renderUI({
    xx <- tryCatch({XVar4()}, error = function(x) 'None')
    dd <- tryCatch({DateVar4()}, error = function(x) 'None')
    if(Debug) print(RemixAutoML:::XTicks(data, xvar=xx,datevar=dd))
    RemixAutoML::PickerInput(InputID = 'XTicks4', Label = tags$span(style='color: blue;', 'X-Axis 4 Ticks'), Choices = RemixAutoML:::XTicks(data, xvar=xx,datevar=dd), SelectedDefault = 'Default', Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
  })

  if(Debug) print("Here sss")

  # Other values
  output$AngleY <- shiny::renderUI({
    RemixAutoML::NumericInput(InputID = 'AngleY', Label = tags$span(style='color: blue;', 'Y-axis text angle'), Step = 5, Min = 0, Max = 360, Value = 0)
  })

  if(Debug) print("Here ttt")

  output$AngleX <- shiny::renderUI({
    RemixAutoML::NumericInput(InputID = 'AngleX', Label = tags$span(style='color: blue;', 'X-axis text angle'), Step = 5, Min = 0, Max = 360, Value = 90)
  })

  if(Debug) print("Here uuu")

  output$TextSize <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'TextSize', Label = tags$span(style='color: blue;', 'Text Size'),Choices = c(as.character(seq(1,100,1))), SelectedDefault = '14', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here vvv")

  output$OutlierSize <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'OutlierSize', Label = tags$span(style='color: blue;', 'Outlier Size'), Choices = c(seq(0.01,2,0.01)), SelectedDefault = 0.01, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here www")

  output$LegendPosition <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'LegendPosition', Label = tags$span(style='color: blue;', 'Legend Position'), Choices = c('bottom','left','right','top'), SelectedDefault = 'bottom', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here xxx")

  output$LegendBorderSize <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'LegendBorderSize', Label = tags$span(style='color: blue;', 'Legend Border Size'), Choices = c(as.character(seq(0.01,2,0.01))), SelectedDefault = as.character(0.01), Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here yyy")

  output$LegendLineType <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'LegendLineType', Label = tags$span(style='color: blue;', 'Legend Border Type'), Choices = c('solid','blank','dashed','dotdash','dotted','longlash','twodash'), SelectedDefault = 'solid', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here zzz")

  # Color boxes
  output$TextColor <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'TextColor', Label = tags$span(style='color: blue;', 'Text Color'), Choices = grDevices::colors(), SelectedDefault = 'darkblue', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here aaaa")

  output$ChartColor <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'ChartColor', Label = tags$span(style='color: blue;', 'Chart Color'), Choices = grDevices::colors(), SelectedDefault = 'lightsteelblue1', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here bbbb")

  output$GridColor <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'GridColor', Label = tags$span(style='color: blue;', 'Grid Lines Color'), Choices = grDevices::colors(), SelectedDefault = 'white', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here cccc")

  output$BackGroundColor <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'BackGroundColor', Label = tags$span(style='color: blue;', 'Background Color'), Choices = grDevices::colors(), SelectedDefault = 'gray95', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here dddd")

  output$BorderColor <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'BorderColor', Label = tags$span(style='color: blue;', 'Border Color'), Choices = grDevices::colors(), SelectedDefault = 'darkblue', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here eeee")

  output$OutlierColor <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'OutlierColor', Label = tags$span(style='color: blue;', 'Outlier Color'), Choices = grDevices::colors(), SelectedDefault = 'blue', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here ffff")

  output$BoxPlotFill <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'BoxPlotFill', Label = tags$span(style='color: blue;', 'BoxPlot Fill Color'), Choices = grDevices::colors(), SelectedDefault = 'gray', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here gggg")

  output$SubTitleColor <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'SubTitleColor', Label = tags$span(style='color: blue;', 'Subtitle Color'), Choices = grDevices::colors(), SelectedDefault = 'blue', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Group Variables                      ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  if(Debug) print("Here hhhh")

  # Select GroupVars
  output$GroupVars1 <- shiny::renderUI({
    if(Debug) print('PickerInput GroupVars1')
    RemixAutoML::PickerInput(InputID='GroupVars1', Label=tags$span(style='color: blue;', 'Select Group Variables 1'), Choices=c('None',names(data)), SelectedDefault=if(!is.null(GroupVariables)) GroupVariables else 'None', SelectedText='count > 1', Multiple=TRUE, ActionBox=TRUE)
  })

  if(Debug) print("Here iiii")

  output$GroupVars2 <- shiny::renderUI({
    if(Debug) print('PickerInput GroupVars2')
    RemixAutoML::PickerInput(InputID='GroupVars2', Label=tags$span(style='color: blue;', 'Select Group Variables 2'), Choices=c('None',names(data)), SelectedDefault=if(!is.null(GroupVariables)) GroupVariables else 'None', SelectedText='count > 1', Multiple=TRUE, ActionBox=TRUE)
  })

  if(Debug) print("Here jjjj")

  output$GroupVars3 <- shiny::renderUI({
    if(Debug) print('PickerInput GroupVars3')
    RemixAutoML::PickerInput(InputID='GroupVars3', Label=tags$span(style='color: blue;', 'Select Group Variables 3'), Choices=c('None',names(data)), SelectedDefault=if(!is.null(GroupVariables)) GroupVariables else 'None', SelectedText='count > 1', Multiple=TRUE, ActionBox=TRUE)
  })

  if(Debug) print("Here kkkk")

  output$GroupVars4 <- shiny::renderUI({
    if(Debug) print('PickerInput GroupVars4')
    RemixAutoML::PickerInput(InputID='GroupVars4', Label=tags$span(style='color: blue;', 'Select Group Variables 4'), Choices=c('None',names(data)), SelectedDefault=if(!is.null(GroupVariables)) GroupVariables else 'None', SelectedText='count > 1', Multiple=TRUE, ActionBox=TRUE)
  })

  if(Debug) print("Here llll")

  # Reactive Group Variables
  SelectedGroups1 <- shiny::reactive({
    RemixAutoML::ReturnParam(input, VarName = 'GroupVars1', Default = if(!is.null(GroupVariables)) GroupVariables else 'None', Switch = TRUE, Type = 'character')
  })

  if(Debug) print("Here mmmm")

  SelectedGroups2 <- shiny::reactive({
    RemixAutoML::ReturnParam(input, VarName = 'GroupVars2', Default = if(!is.null(GroupVariables)) GroupVariables else 'None', Switch = TRUE, Type = 'character')
  })

  if(Debug) print("Here nnnn")

  SelectedGroups3 <- shiny::reactive({
    RemixAutoML::ReturnParam(input, VarName = 'GroupVars3', Default = if(!is.null(GroupVariables)) GroupVariables else 'None', Switch = TRUE, Type = 'character')
  })

  if(Debug) print("Here oooo")

  SelectedGroups4 <- shiny::reactive({
    RemixAutoML::ReturnParam(input, VarName = 'GroupVars4', Default = if(!is.null(GroupVariables)) GroupVariables else 'None', Switch = TRUE, Type = 'character')
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
    RemixAutoML::PickerInput(InputID='FacetVar_1_1', Label = tags$span(style='color: blue;', 'Facet Variable 1 1'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here ccccc")

  output$FacetVar_1_2 <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID='FacetVar_1_2', Label = tags$span(style='color: blue;', 'Facet Variable 1 2'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here ddddd")

  output$FacetVar_2_1 <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID='FacetVar_2_1', Label = tags$span(style='color: blue;', 'Facet Variable 2 1'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here eeeee")

  output$FacetVar_2_2 <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID='FacetVar_2_2', Label = tags$span(style='color: blue;', 'Facet Variable 2 2'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here fffff")

  output$FacetVar_3_1 <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID='FacetVar_3_1', Label = tags$span(style='color: blue;', 'Facet Variable 3 1'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here ggggg")

  output$FacetVar_3_2 <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID='FacetVar_3_2', Label = tags$span(style='color: blue;', 'Facet Variable 3 2'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here hhhhh")

  output$FacetVar_4_1 <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID='FacetVar_4_1', Label = tags$span(style='color: blue;', 'Facet Variable 4 1'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here iiiii")

  output$FacetVar_4_2 <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID='FacetVar_4_2', Label = tags$span(style='color: blue;', 'Facet Variable 4 2'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here jjjjj")

  # Sizing
  output$SizeVar1 <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'SizeVar1', Label = tags$span(style='color: blue;', 'Size Variable 1'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here kkkkk")

  output$SizeVar2 <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'SizeVar2', Label = tags$span(style='color: blue;', 'Size Variable 2'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here lllll")

  output$SizeVar3 <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'SizeVar3', Label = tags$span(style='color: blue;', 'Size Variable 3'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  if(Debug) print("Here mmmmm")

  output$SizeVar4 <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'SizeVar4', Label = tags$span(style='color: blue;', 'Size Variable 4'), Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Filter Variables                     ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  # Filter Variables
  output$FilterVariable_1_1 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterVariable_1_1', label = tags$span(style='color: blue;', 'Filter Variable 1 1'), choices=c('None', names(data)), selected='None')
  })

  if(Debug) print("Here nnnnn")

  output$FilterVariable_1_2 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterVariable_1_2', label = tags$span(style='color: blue;', 'Filter Variable 1 2'), choices=c('None', names(data)), selected='None')
  })

  if(Debug) print("Here ooooo")

  output$FilterVariable_1_3 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterVariable_1_3', label = tags$span(style='color: blue;', 'Filter Variable 1 3'), choices=c('None', names(data)), selected='None')
  })

  if(Debug) print("Here ppppp")

  output$FilterVariable_1_4 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterVariable_1_4', label = tags$span(style='color: blue;', 'Filter Variable 1 4'), choices=c('None', names(data)), selected='None')
  })

  if(Debug) print("Here qqqqq")

  output$FilterVariable_2_1 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterVariable_2_1', label = tags$span(style='color: blue;', 'Filter Variable 2 1'), choices=c('None', names(data)), selected='None')
  })

  if(Debug) print("Here rrrrr")

  output$FilterVariable_2_2 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterVariable_2_2', label = tags$span(style='color: blue;', 'Filter Variable 2 2'), choices=c('None', names(data)), selected='None')
  })

  if(Debug) print("Here sssss")

  output$FilterVariable_2_3 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterVariable_2_3', label = tags$span(style='color: blue;', 'Filter Variable 2 3'), choices=c('None', names(data)), selected='None')
  })

  if(Debug) print("Here ttttt")

  output$FilterVariable_2_4 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterVariable_2_4', label = tags$span(style='color: blue;', 'Filter Variable 2 4'), choices=c('None', names(data)), selected='None')
  })

  if(Debug) print("Here uuuuu")

  output$FilterVariable_3_1 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterVariable_3_1', label = tags$span(style='color: blue;', 'Filter Variable 3 1'), choices=c('None', names(data)), selected='None')
  })

  if(Debug) print("Here vvvvv")

  output$FilterVariable_3_2 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterVariable_3_2', label = tags$span(style='color: blue;', 'Filter Variable 3 2'), choices=c('None', names(data)), selected='None')
  })

  if(Debug) print("Here wwwww")

  output$FilterVariable_3_3 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterVariable_3_3', label = tags$span(style='color: blue;', 'Filter Variable 3 3'), choices=c('None', names(data)), selected='None')
  })

  if(Debug) print("Here xxxxx")

  output$FilterVariable_3_4 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterVariable_3_4', label = tags$span(style='color: blue;', 'Filter Variable 3 4'), choices=c('None', names(data)), selected='None')
  })

  if(Debug) print("Here yyyyy")

  output$FilterVariable_4_1 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterVariable_4_1', label = tags$span(style='color: blue;', 'Filter Variable 4 1'), choices=c('None', names(data)), selected='None')
  })

  if(Debug) print("Here zzzzz")

  output$FilterVariable_4_2 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterVariable_4_2', label = tags$span(style='color: blue;', 'Filter Variable 4 2'), choices=c('None', names(data)), selected='None')
  })

  if(Debug) print("Here aaaaaa")

  output$FilterVariable_4_3 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterVariable_4_3', label = tags$span(style='color: blue;', 'Filter Variable 4 3'), choices=c('None', names(data)), selected='None')
  })

  if(Debug) print("Here bbbbbb")

  output$FilterVariable_4_4 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterVariable_4_4', label = tags$span(style='color: blue;', 'Filter Variable 4 4'), choices=c('None', names(data)), selected='None')
  })

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Filter Logic                         ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  if(Debug) print("Here cccccc")

  # Filter Logics
  output$FilterLogic_1_1 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterLogic_1_1', selected=RemixAutoML:::FL_Default(data, x=input[['FilterVariable_1_1']]), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
  })

  if(Debug) print("Here dddddd")

  output$FilterLogic_1_2 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterLogic_1_2', selected=RemixAutoML:::FL_Default(data, x=input[['FilterVariable_1_2']]), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
  })

  if(Debug) print("Here eeeeee")

  output$FilterLogic_1_3 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterLogic_1_3', selected=RemixAutoML:::FL_Default(data, x=input[['FilterVariable_1_3']]), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
  })

  if(Debug) print("Here ffffff")

  output$FilterLogic_1_4 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterLogic_1_4', selected=RemixAutoML:::FL_Default(data, x=input[['FilterVariable_1_4']]), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
  })

  if(Debug) print("Here gggggg")

  output$FilterLogic_2_1 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterLogic_2_1', selected=RemixAutoML:::FL_Default(data, x=input[['FilterVariable_2_1']]), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
  })

  if(Debug) print("Here hhhhhh")

  output$FilterLogic_2_2 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterLogic_2_2', selected=RemixAutoML:::FL_Default(data, x=input[['FilterVariable_2_2']]), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
  })

  if(Debug) print("Here iiiiii")

  output$FilterLogic_2_3 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterLogic_2_3', selected=RemixAutoML:::FL_Default(data, x=input[['FilterVariable_2_3']]), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
  })

  if(Debug) print("Here jjjjjj")

  output$FilterLogic_2_4 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterLogic_2_4', selected=RemixAutoML:::FL_Default(data, x=input[['FilterVariable_2_4']]), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
  })

  if(Debug) print("Here kkkkkk")

  output$FilterLogic_3_1 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterLogic_3_1', selected=RemixAutoML:::FL_Default(data, x=input[['FilterVariable_3_1']]), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
  })

  if(Debug) print("Here llllll")

  output$FilterLogic_3_2 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterLogic_3_2', selected=RemixAutoML:::FL_Default(data, x=input[['FilterVariable_3_2']]), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
  })

  if(Debug) print("Here mmmmmm")

  output$FilterLogic_3_3 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterLogic_3_3', selected=RemixAutoML:::FL_Default(data, x=input[['FilterVariable_3_3']]), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
  })

  if(Debug) print("Here nnnnnn")

  output$FilterLogic_3_4 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterLogic_3_4', selected=RemixAutoML:::FL_Default(data, x=input[['FilterVariable_3_4']]), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
  })

  if(Debug) print("Here oooooo")

  output$FilterLogic_4_1 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterLogic_4_1', selected=RemixAutoML:::FL_Default(data, x=input[['FilterVariable_4_1']]), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
  })

  if(Debug) print("Here pppppp")

  output$FilterLogic_4_2 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterLogic_4_2', selected=RemixAutoML:::FL_Default(data, x=input[['FilterVariable_4_2']]), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
  })

  if(Debug) print("Here qqqqqq")

  output$FilterLogic_4_3 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterLogic_4_3', selected=RemixAutoML:::FL_Default(data, x=input[['FilterVariable_4_3']]), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
  })

  if(Debug) print("Here rrrrrr")

  output$FilterLogic_4_4 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterLogic_4_4', selected=RemixAutoML:::FL_Default(data, x=input[['FilterVariable_4_4']]), label=tags$span(style='color: blue;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
  })

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Filter Values                        ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  if(Debug) print("Here ssssss")

  # Filter Values
  #
  # 1_1_1 means Plot 1, Filter Var 1, Filter Value min
  # 1_1_2 means Plot 1, Filter Var 1, filter value max
  #
  # Plot 1
  output$FilterValue_1_1_1 <- shiny::renderUI({
    params <- list(data=data, VarName=input[['FilterVariable_1_1']], type=1); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
    RemixAutoML::PickerInput(InputID='FilterValue_1_1_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
  })

  if(Debug) print("Here tttttt")

  output$FilterValue_1_1_2 <- shiny::renderUI({
    params <- list(data=data, VarName=input[['FilterVariable_1_1']], type=2); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
    RemixAutoML::PickerInput(InputID='FilterValue_1_1_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
  })

  if(Debug) print("Here uuuuuu")

  output$FilterValue_1_2_1 <- shiny::renderUI({
    params <- list(data=data, VarName=input[['FilterVariable_1_2']], type=1); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
    RemixAutoML::PickerInput(InputID='FilterValue_1_2_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
  })

  if(Debug) print("Here vvvvvv")

  output$FilterValue_1_2_2 <- shiny::renderUI({
    params <- list(data=data, VarName=input[['FilterVariable_1_2']], type=2); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
    RemixAutoML::PickerInput(InputID='FilterValue_1_2_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
  })

  if(Debug) print("Here wwwwww")

  output$FilterValue_1_3_1 <- shiny::renderUI({
    params <- list(data=data, VarName=input[['FilterVariable_1_3']], type=1); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
    RemixAutoML::PickerInput(InputID='FilterValue_1_3_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
  })

  if(Debug) print("Here xxxxxx")

  output$FilterValue_1_3_2 <- shiny::renderUI({
    params <- list(data=data, VarName=input[['FilterVariable_1_3']], type=2); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
    RemixAutoML::PickerInput(InputID='FilterValue_1_3_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
  })

  if(Debug) print("Here yyyyyy")

  output$FilterValue_1_4_1 <- shiny::renderUI({
    params <- list(data=data, VarName=input[['FilterVariable_1_4']], type=1); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
    RemixAutoML::PickerInput(InputID='FilterValue_1_4_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
  })

  if(Debug) print("Here zzzzzz")

  output$FilterValue_1_4_2 <- shiny::renderUI({
    params <- list(data=data, VarName=input[['FilterVariable_1_4']], type=2); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
    RemixAutoML::PickerInput(InputID='FilterValue_1_4_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
  })

  if(Debug) print("Here aaaaaaa")

  # Plot 2
  output$FilterValue_2_1_1 <- shiny::renderUI({
    params <- list(data=data, VarName=input[['FilterVariable_2_1']], type=1); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
    RemixAutoML::PickerInput(InputID='FilterValue_2_1_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
  })

  if(Debug) print("Here bbbbbbb")

  output$FilterValue_2_1_2 <- shiny::renderUI({
    params <- list(data=data, VarName=input[['FilterVariable_2_1']], type=2); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
    RemixAutoML::PickerInput(InputID='FilterValue_2_1_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
  })

  if(Debug) print("Here ccccccc")

  output$FilterValue_2_2_1 <- shiny::renderUI({
    params <- list(data=data, VarName=input[['FilterVariable_2_2']], type=1); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
    RemixAutoML::PickerInput(InputID='FilterValue_2_2_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
  })

  if(Debug) print("Here ddddddd")

  output$FilterValue_2_2_2 <- shiny::renderUI({
    params <- list(data=data, VarName=input[['FilterVariable_2_2']], type=2); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
    RemixAutoML::PickerInput(InputID='FilterValue_2_2_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
  })

  if(Debug) print("Here eeeeeee")

  output$FilterValue_2_3_1 <- shiny::renderUI({
    params <- list(data=data, VarName=input[['FilterVariable_2_3']], type=1); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
    RemixAutoML::PickerInput(InputID='FilterValue_2_3_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
  })

  if(Debug) print("Here fffffff")

  output$FilterValue_2_3_2 <- shiny::renderUI({
    params <- list(data=data, VarName=input[['FilterVariable_2_3']], type=2); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
    RemixAutoML::PickerInput(InputID='FilterValue_2_3_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
  })

  if(Debug) print("Here ggggggg")

  output$FilterValue_2_4_1 <- shiny::renderUI({
    params <- list(data=data, VarName=input[['FilterVariable_2_4']], type=1); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
    RemixAutoML::PickerInput(InputID='FilterValue_2_4_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
  })

  if(Debug) print("Here hhhhhhh")

  output$FilterValue_2_4_2 <- shiny::renderUI({
    params <- list(data=data, VarName=input[['FilterVariable_2_4']], type=2); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
    RemixAutoML::PickerInput(InputID='FilterValue_2_4_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
  })

  if(Debug) print("Here qqqqqqq")

  # Plot 3
  output$FilterValue_3_1_1 <- shiny::renderUI({
    params <- list(data=data, VarName=input[['FilterVariable_3_1']], type=1); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
    RemixAutoML::PickerInput(InputID='FilterValue_3_1_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
  })

  if(Debug) print("Here rrrrrrr")

  output$FilterValue_3_1_2 <- shiny::renderUI({
    params <- list(data=data, VarName=input[['FilterVariable_3_1']], type=2); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
    RemixAutoML::PickerInput(InputID='FilterValue_3_1_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
  })

  if(Debug) print("Here sssssss")

  output$FilterValue_3_2_1 <- shiny::renderUI({
    params <- list(data=data, VarName=input[['FilterVariable_3_2']], type=1); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
    RemixAutoML::PickerInput(InputID='FilterValue_3_2_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
  })

  if(Debug) print("Here ttttttt")

  output$FilterValue_3_2_2 <- shiny::renderUI({
    params <- list(data=data, VarName=input[['FilterVariable_3_2']], type=2); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
    RemixAutoML::PickerInput(InputID='FilterValue_3_2_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
  })

  if(Debug) print("Here uuuuuuu")

  output$FilterValue_3_3_1 <- shiny::renderUI({
    params <- list(data=data, VarName=input[['FilterVariable_3_3']], type=1); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
    RemixAutoML::PickerInput(InputID='FilterValue_3_3_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
  })

  if(Debug) print("Here vvvvvvv")

  output$FilterValue_3_3_2 <- shiny::renderUI({
    params <- list(data=data, VarName=input[['FilterVariable_3_3']], type=2); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
    RemixAutoML::PickerInput(InputID='FilterValue_3_3_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
  })

  if(Debug) print("Here wwwwwww")

  output$FilterValue_3_4_1 <- shiny::renderUI({
    params <- list(data=data, VarName=input[['FilterVariable_3_4']], type=1); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
    RemixAutoML::PickerInput(InputID='FilterValue_3_4_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
  })

  if(Debug) print("Here xxxxxxx")

  output$FilterValue_3_4_2 <- shiny::renderUI({
    params <- list(data=data, VarName=input[['FilterVariable_3_4']], type=2); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
    RemixAutoML::PickerInput(InputID='FilterValue_3_4_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
  })

  if(Debug) print("Here yyyyyyy")

  # Plot 4
  output$FilterValue_4_1_1 <- shiny::renderUI({
    params <- list(data=data, VarName=input[['FilterVariable_4_1']], type=1); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
    RemixAutoML::PickerInput(InputID='FilterValue_4_1_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
  })

  if(Debug) print("Here zzzzzzz")

  output$FilterValue_4_1_2 <- shiny::renderUI({
    params <- list(data=data, VarName=input[['FilterVariable_4_1']], type=2); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
    RemixAutoML::PickerInput(InputID='FilterValue_4_1_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
  })

  if(Debug) print("Here aaaaaaaa")

  output$FilterValue_4_2_1 <- shiny::renderUI({
    params <- list(data=data, VarName=input[['FilterVariable_4_2']], type=1); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
    RemixAutoML::PickerInput(InputID='FilterValue_4_2_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
  })

  if(Debug) print("Here bbbbbbbb")

  output$FilterValue_4_2_2 <- shiny::renderUI({
    params <- list(data=data, VarName=input[['FilterVariable_4_2']], type=2); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
    RemixAutoML::PickerInput(InputID='FilterValue_4_2_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
  })

  if(Debug) print("Here cccccccc")

  output$FilterValue_4_3_1 <- shiny::renderUI({
    params <- list(data=data, VarName=input[['FilterVariable_4_3']], type=1); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
    RemixAutoML::PickerInput(InputID='FilterValue_4_3_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
  })

  if(Debug) print("Here dddddddd")

  output$FilterValue_4_3_2 <- shiny::renderUI({
    params <- list(data=data, VarName=input[['FilterVariable_4_3']], type=2); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
    RemixAutoML::PickerInput(InputID='FilterValue_4_3_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
  })

  if(Debug) print("Here eeeeeeee")

  output$FilterValue_4_4_1 <- shiny::renderUI({
    params <- list(data=data, VarName=input[['FilterVariable_4_4']], type=1); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
    RemixAutoML::PickerInput(InputID='FilterValue_4_4_1', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
  })

  if(Debug) print("Here ffffffff")

  output$FilterValue_4_4_2 <- shiny::renderUI({
    params <- list(data=data, VarName=input[['FilterVariable_4_4']], type=2); FilterUnique <- do.call(RemixAutoML::FilterValues, params)
    RemixAutoML::PickerInput(InputID='FilterValue_4_4_2', Label=tags$span(style='color: blue;', do.call(RemixAutoML::GetFilterValueLabel, params)), Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=do.call(RemixAutoML::GetFilterValueMultiple, params), ActionBox=TRUE)
  })

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@----
  # Print Code to UI                     ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@----
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
  # Reset Plot Format                    ----
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
        RemixAutoML::PickerInput(InputID = 'TextSize', Label = tags$span(style='color: blue;', 'Text Size'),Choices = c(as.character(seq(1,100,1))), SelectedDefault = '14', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
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
  # Update Plot Format                   ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  shiny::observeEvent(eventExpr = input[['UpdatePlotThemeElements']], {

    if(Debug) for(zzzz in 1:4) print(':: :: UPDATE PLOTS :: ::')
    if(!exists('p1') || !exists('data1')) {

      shinyWidgets::sendSweetAlert(session, title = NULL, text = "Try Create Plot", type = NULL, btn_labels = "error", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")

    } else {

      # Plot Formatting Values
      if(Debug) print('# Plot Formatting Values')
      NumberGroupsDisplay <<- RemixAutoML::ReturnParam(input, VarName='NumberGroupsDisplay', Type = 'numeric', Default = 5L, Switch = TRUE)
      PlotWidth <<- RemixAutoML::ReturnParam(input, VarName = 'PlotWidth', Type = 'numeric', Default = 1600, Switch = TRUE)
      PlotHeight <<- RemixAutoML::ReturnParam(input, VarName = 'PlotHeight', Type = 'numeric', Default = 500, Switch = TRUE)
      YTicks <<- RemixAutoML::ReturnParam(input, VarName = 'YTicks', Type = 'character', Default = 'Default', Switch = TRUE)
      XTicks <<- RemixAutoML::ReturnParam(input, VarName = 'XTicks', Type = 'character', Default = 'Default', Switch = TRUE)
      AngleY <<- RemixAutoML::ReturnParam(input, VarName = 'AngleY', Type = 'numeric', Default = 0L, Switch = TRUE)
      AngleX <<- RemixAutoML::ReturnParam(input, VarName = 'AngleX', Type = 'numeric', Default = 90L, Switch = TRUE)
      TextSize <<- as.numeric(RemixAutoML::ReturnParam(input, VarName = 'TextSize', Type = 'character', Default = 12L, Switch = TRUE))
      OutlierSize <<- RemixAutoML::ReturnParam(input, VarName = 'OutlierSize', Type = 'numeric', Default = 0.01, Switch = TRUE)
      LegendPosition <<- RemixAutoML::ReturnParam(input, VarName = 'LegendPosition', Type = 'character', Default = 'bottom', Switch = TRUE)
      LegendBorderSize <<- RemixAutoML::ReturnParam(input, VarName = 'LegendBorderSize', Type = 'numeric', Default = 0.01, Switch = TRUE)
      LegendLineType <<- RemixAutoML::ReturnParam(input, VarName = 'LegendLineType', Type = 'character', Default = 'solid', Switch = TRUE)

      # Update chart theme elements
      if(Debug) print('ChartTheme Update')
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
      if(Debug) print('Update Labels')
      YTicks <<- RemixAutoML::ReturnParam(input, VarName = 'YTicks', Type = 'character', Default = 'Default', Switch = TRUE)
      XTicks <<- RemixAutoML::ReturnParam(input, VarName = 'XTicks', Type = 'character', Default = 'Default', Switch = TRUE)
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
        if('Percentiles' %in% YTicks) {
          y_vals <- data1[, quantile(round(get(YVar()), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
        } else if('Every 5th percentile' %in% YTicks) {
          y_vals <- data1[, quantile(round(get(YVar()), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
          y_vals <- y_vals[c(seq(6L, length(y_vals)-1L, 5L))]
        } else if('Deciles' %in% YTicks) {
          y_vals <- data1[, quantile(round(get(YVar()), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
          y_vals <- y_vals[c(seq(11L, length(y_vals)-1L, 10L))]
        } else if('Quantiles' %in% YTicks) {
          y_vals <- data1[, quantile(round(get(YVar()), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
          y_vals <- y_vals[c(seq(21L, length(y_vals)-1L, 20L))]
        } else if('Quartiles' %in% YTicks) {
          y_vals <- data1[, quantile(round(get(YVar()), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
          y_vals <- y_vals[c(seq(26L, length(y_vals)-1L, 25L))]
        } else {
          y_vals <- YTicks
        }
        if('Percentiles' %in% XTicks) {
          x_vals <- data1[, quantile(round(get(XVar()), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
        } else if('Every 5th percentile' %in% XTicks) {
          x_vals <- data1[, quantile(round(get(YVar()), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
          x_vals <- x_vals[c(seq(6L, length(x_vals)-1L, 5L))]
        } else if('Deciles' %in% XTicks) {
          x_vals <- data1[, quantile(round(get(YVar()), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
          x_vals <- x_vals[c(seq(11L, length(x_vals)-1L, 10L))]
        } else if('Quantiles' %in% XTicks) {
          x_vals <- data1[, quantile(round(get(YVar()), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
          x_vals <- x_vals[c(seq(21L, length(x_vals)-1L, 20L))]
        } else if('Quartiles' %in% XTicks) {
          x_vals <- data1[, quantile(round(get(YVar()), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
          x_vals <- x_vals[c(seq(26L, length(x_vals)-1L, 25L))]
        } else {
          x_vals <- XTicks
        }
        if(!'Default' %in% input[['XTicks']]) p1 <- p1 + suppressMessages(ggplot2::scale_x_continuous(breaks = as.numeric(x_vals)))
        if(!'Default' %in% input[['YTicks']]) p1 <- p1 + suppressMessages(ggplot2::scale_y_continuous(breaks = as.numeric(y_vals)))
      }

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
  # Create Plot                          ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  shiny::observeEvent(eventExpr = input[['TrendPlotExecute']], {

    # Debug
    if(Debug) for(zzzz in 1:4) print(':: :: CREATE PLOTS :: ::')

    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    # Store Core Plotting Variables        ----
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

    if(Debug) print('Here 1')

    if(Debug) print(shiny::isolate(shiny::req(YVar1())))

    # Core Plotting Variables
    YVarList <- list()
    YVarList[['YVar1']] <- RemixAutoML:::CEPP(input[['YVar1']])
    YVarList[['YVar2']] <- RemixAutoML:::CEPP(input[['YVar2']])
    YVarList[['YVar3']] <- RemixAutoML:::CEPP(input[['YVar3']])
    YVarList[['YVar4']] <- RemixAutoML:::CEPP(input[['YVar4']])
    assign(x = 'YVarList', value = YVarList, envir = .GlobalEnv)

    YMinList <- list(); for(i in seq_len(4L)) {
      if(Debug) {paste0('YMin', i); print(input[[paste0('YMin', i)]]); print(RemixAutoML:::CEPP(input[[paste0('YMin', i)]]))}
      YMinList[[paste0('YMin', i)]] <- RemixAutoML:::CEPP(input[[paste0('YMin', i)]])
    }
    assign(x = 'YMinList', value = YMinList, envir = .GlobalEnv)

    YMaxList <- list(); for(i in seq_len(4L)) {
      if(Debug) {print(paste0('YMax', i)); print(input[[paste0('YMax', i)]]); print(RemixAutoML:::CEPP(input[[paste0('YMax', i)]]))}
      YMaxList[[paste0('YMax', i)]] <- RemixAutoML:::CEPP(input[[paste0('YMax', i)]])
    }
    assign(x = 'YMaxList', value = YMaxList, envir = .GlobalEnv)

    if(Debug) print('Here 2')
    if(Debug) print(input[['XVar1']])

    XVarList <- list()
    XVarList[['XVar1']] <- RemixAutoML:::CEPP(input[['XVar1']])
    XVarList[['XVar2']] <- RemixAutoML:::CEPP(input[['XVar2']])
    XVarList[['XVar3']] <- RemixAutoML:::CEPP(input[['XVar3']])
    XVarList[['XVar4']] <- RemixAutoML:::CEPP(input[['XVar4']])
    assign(x = 'XVarList', value = XVarList, envir = .GlobalEnv)

    XMinList <- list(); for(i in seq_len(4L)) XMinList[[paste0('XMin', i)]] <- RemixAutoML:::CEPP(input[[paste0('XMin', i)]])
    assign(x = 'XMinList', value = XMinList, envir = .GlobalEnv)
    XMaxList <- list(); for(i in seq_len(4L)) XMaxList[[paste0('XMax', i)]] <- RemixAutoML:::CEPP(input[[paste0('XMax', i)]])
    assign(x = 'XMaxList', value = XMaxList, envir = .GlobalEnv)

    if(Debug) print('Here 3')

    DateVarList <- list()
    DateVarList[['DateVar1']] <- RemixAutoML:::CEPP(input[['DateVar1']])
    DateVarList[['DateVar2']] <- RemixAutoML:::CEPP(input[['DateVar2']])
    DateVarList[['DateVar3']] <- RemixAutoML:::CEPP(input[['DateVar3']])
    DateVarList[['DateVar4']] <- RemixAutoML:::CEPP(input[['DateVar4']])
    assign(x = 'DateVarList', value = DateVarList, envir = .GlobalEnv)

    DateMinList <- list(); for(i in seq_len(4L)) DateMinList[[paste0('DateMin', i)]] <- RemixAutoML:::CEPP(input[[paste0('DateMin', i)]])
    assign(x = 'DateMinList', value = DateMinList, envir = .GlobalEnv)
    DateMaxList <- list(); for(i in seq_len(4L)) DateMaxList[[paste0('DateMax', i)]] <- RemixAutoML:::CEPP(input[[paste0('DateMax', i)]])
    assign(x = 'DateMaxList', value = DateMaxList, envir = .GlobalEnv)

    if(Debug) print('Here 4')

    ScoreVarList <- list()
    ScoreVarList[['ScoreVar1']] <- RemixAutoML:::CEPP(input[['ScoreVar1']])
    if(Debug) print('Here 4.1')
    ScoreVarList[['ScoreVar2']] <- RemixAutoML:::CEPP(input[['ScoreVar2']])
    if(Debug) print('Here 4.2')
    ScoreVarList[['ScoreVar3']] <- RemixAutoML:::CEPP(input[['ScoreVar3']])
    if(Debug) print('Here 4.3')
    ScoreVarList[['ScoreVar4']] <- RemixAutoML:::CEPP(input[['ScoreVar4']])
    if(Debug) print('Here 4.4')
    assign(x = 'ScoreVarList', value = ScoreVarList, envir = .GlobalEnv)

    if(Debug) print('Here 5')

    GroupVarsList <- list(); for(i in seq_len(4L)) GroupVarsList[[paste0('GroupVars', i)]] <- RemixAutoML:::CEPP(input[[paste0('GroupVars', i)]])
    assign(x = 'GroupVarsList', value = GroupVarsList, envir = .GlobalEnv)

    SelectedGroupsList <- list(); for(i in seq_len(4L)) SelectedGroupsList[[paste0('SelectedGroups', i)]] <- RemixAutoML:::CEPP(input[[paste0('SelectedGroups', i)]])
    if(Debug) {print('Here 5.1'); print(SelectedGroupsList)}
    assign(x = 'SelectedGroupsList', value = SelectedGroupsList, envir = .GlobalEnv)

    SizeVarList <- list(); for(i in seq_len(4L)) SizeVarList[[paste0('SizeVar', i)]] <- RemixAutoML:::CEPP(input[[paste0('SizeVar', i)]])
    assign(x = 'SizeVarList', value = SizeVarList, envir = .GlobalEnv)

    LevelsList <- list(); for(i in seq_len(4L)) for(j in seq_len(3L)) LevelsList[[paste0('Levels_', i, '_', j)]] <- RemixAutoML:::CEPP(input[[paste0('Levels_', i, '_', j)]])
    assign(x = 'LevelsList', value = LevelsList, envir = .GlobalEnv)

    FacetVarList <- list(); for(i in seq_len(4L)) for(j in seq_len(2L)) FacetVarList[[paste0('FacetVar_', i, '_', j)]] <- RemixAutoML:::CEPP(input[[paste0('FacetVar_', i, '_', j)]])
    assign(x = 'FacetVarList', value = FacetVarList, envir = .GlobalEnv)

    if(Debug) print('Here 6')

    FilterVariableList <- list(); for(i in seq_len(4L)) for(j in seq_len(3L)) FilterVariableList[[paste0('FilterVariable_', i, '_', j)]] <- RemixAutoML:::CEPP(input[[paste0('FilterVariable_', i, '_', j)]])
    assign(x = 'FilterVariableList', value = FilterVariableList, envir = .GlobalEnv)
    FilterLogicList <- list(); for(i in seq_len(4L)) for(j in seq_len(3L)) FilterLogicList[[paste0('FilterLogic_', i, '_', j)]] <- RemixAutoML:::CEPP(input[[paste0('FilterLogic_', i, '_', j)]], Default = '>=')
    assign(x = 'FilterLogicList', value = FilterLogicList, envir = .GlobalEnv)
    FilterValueList <- list(); for(i in seq_len(4L)) for(j in seq_len(4L)) for(k in seq_len(2L)) FilterValueList[[paste0('FilterLogic_', i, '_', j, '_', k)]] <- RemixAutoML:::CEPP(input[[paste0('FilterLogic_', i, '_', j, '_', k)]])
    assign(x = 'FilterValueList', value = FilterValueList, envir = .GlobalEnv)

    if(Debug) print('Here 7')

    # NOTE: Checking to see what comes out the other end when panel isn't opened
    if(Debug) print(input[[paste0('GamFitScatter', i)]])
    if(Debug) print(RemixAutoML:::CEPP(input[[paste0('GamFitScatter', i)]]))

    GamFitScatterList <- list(); for(i in seq_len(4L)) GamFitScatterList[[paste0('GamFitScatter', i)]] <- RemixAutoML:::CEPP(input[[paste0('GamFitScatter', i)]], Default = FALSE)
    assign(x = 'GamFitScatterList', value = GamFitScatterList, envir = .GlobalEnv)
    PDP_VariableList <- list(); for(i in seq_len(4L)) PDP_VariableList[[paste0('PDP_Variable', i)]] <- RemixAutoML:::CEPP(input[[paste0('PDP_Variable', i)]])
    assign(x = 'PDP_VariableList', value = PDP_VariableList, envir = .GlobalEnv)
    NumberBinsList <- list(); for(i in seq_len(4L)) NumberBinsList[[paste0('NumberBins', i)]] <- RemixAutoML:::CEPP(input[[paste0('NumberBins', i)]], Default = 30)
    assign(x = 'NumberBinsList', value = NumberBinsList, envir = .GlobalEnv)
    Percentile_BucketsList <- list(); for(i in seq_len(4L)) Percentile_BucketsList[[paste0('Percentile_Buckets', i)]] <- RemixAutoML:::CEPP(input[[paste0('Percentile_Buckets', i)]], Default = 20)
    assign(x = 'Percentile_BucketsList', value = Percentile_BucketsList, envir = .GlobalEnv)

    if(Debug) print('Here 8')

    YTicksList <- list(); for(i in seq_len(4L)) YTicksList[[paste0('YTicks', i)]] <- RemixAutoML:::CEPP(input[[paste0('YTicks', i)]])
    assign(x = 'YTicksList', value = YTicksList, envir = .GlobalEnv)
    XTicksList <- list(); for(i in seq_len(4L)) XTicksList[[paste0('XTicks', i)]] <- RemixAutoML:::CEPP(input[[paste0('XTicks', i)]])
    assign(x = 'YTicksList', value = YTicksList, envir = .GlobalEnv)

    # ----

    # ----

    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    # Predefine Hidden Args                ----
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

    if(Debug) print('Here 9')

    # Plot Formatting
    NumberGroupsDisplay <<- RemixAutoML::ReturnParam(input, VarName='NumberGroupsDisplay', Type = 'numeric', Default = 5L, Switch = TRUE)
    PlotWidth <<- RemixAutoML::ReturnParam(input, VarName = 'PlotWidth', Type = 'numeric', Default = 1600, Switch = TRUE)
    PlotHeight <<- RemixAutoML::ReturnParam(input, VarName = 'PlotHeight', Type = 'numeric', Default = 500, Switch = TRUE)
    AngleY <<- RemixAutoML::ReturnParam(input, VarName = 'AngleY', Type = 'numeric', Default = 0L, Switch = TRUE)
    AngleX <<- RemixAutoML::ReturnParam(input, VarName = 'AngleX', Type = 'numeric', Default = 90L, Switch = TRUE)
    TextSize <<- RemixAutoML::ReturnParam(input, VarName = 'TextSize', Type = 'numeric', Default = 12L, Switch = TRUE)
    OutlierSize <<- RemixAutoML::ReturnParam(input, VarName = 'OutlierSize', Type = 'numeric', Default = 0.01, Switch = TRUE)
    LegendPosition <<- RemixAutoML::ReturnParam(input, VarName = 'LegendPosition', Type = 'character', Default = 'bottom', Switch = TRUE)
    LegendBorderSize <<- RemixAutoML::ReturnParam(input, VarName = 'LegendBorderSize', Type = 'numeric', Default = 0.01, Switch = TRUE)
    LegendLineType <<- RemixAutoML::ReturnParam(input, VarName = 'LegendLineType', Type = 'character', Default = 'solid', Switch = TRUE)

    if(Debug) print('Here 10')

    # Color boxes
    TextColor <<- RemixAutoML::ReturnParam(input, VarName = 'TextColor', Type = 'character', Default = 'darkblue', Switch = TRUE)
    ChartColor <<- RemixAutoML::ReturnParam(input, VarName = 'ChartColor', Type = 'character', Default = 'lightsteelblue1', Switch = TRUE)
    GridColor <<- RemixAutoML::ReturnParam(input, VarName = 'GridColor', Type = 'character', Default = 'white', Switch = TRUE)
    BackGroundColor <<- RemixAutoML::ReturnParam(input, VarName = 'BackGroundColor', Type = 'character', Default = 'gray95', Switch = TRUE)
    BorderColor <<- RemixAutoML::ReturnParam(input, VarName = 'BorderColor', Type = 'character', Default = 'darkblue', Switch = TRUE)
    OutlierColor <<- RemixAutoML::ReturnParam(input, VarName = 'OutlierColor', Type = 'character', Default = 'blue', Switch = TRUE)
    BoxPlotFill <<- RemixAutoML::ReturnParam(input, VarName = 'BoxPlotFill', Type = 'character', Default = 'gray', Switch = TRUE)
    SubTitleColor <<- RemixAutoML::ReturnParam(input, VarName = 'SubTitleColor', Type = 'character', Default = 'blue', Switch = TRUE)

    # ----

    # ----

    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    # Loop Through Plot Builds             ----
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    if(Debug) print('Here 11: BEGIN PLOTTING LOOP')

    PlotCollectionList <- list()

    if(Debug) {print(tryCatch({names(YVarList)}, error = function(x) NULL));print(tryCatch({names(XVarList)}, error = function(x) NULL))}

    YVarListCount <- 0
    vals <- tryCatch({names(YVarList)[!names(YVarList) %in% 'None']}, error = function(x) NULL)
    if(!is.null(vals)) {
      for(i in vals) {
        if(Debug) print(YVarList[[i]])
        if(YVarList[[i]] != 'None') YVarListCount <- YVarListCount + 1L
      }
    }

    XVarListCount <- 0
    vals <- tryCatch({names(XVarList)[!names(XVarList) %in% 'None']}, error = function(x) NULL)
    if(!is.null(vals)) {
      for(i in names(XVarList)[!names(XVarList) %in% 'None']) {
        if(XVarList[[i]] != 'None') XVarListCount <- XVarListCount + 1L
      }
    }

    # PlotType Determination
    counter <- 0
    DragulaList <- list()
    if(Debug) {print('Dragula Details'); print(input[['PlotTypeDragula']])}
    if(!is.null(input[['PlotTypeDragula']][['target']][['UpperLeftPlot']])) {
      counter <- counter + 1L
      DragulaList[[counter]] <- input[['PlotTypeDragula']][['target']][['UpperLeftPlot']]
      print(paste0('UpperLeftPlot = ', DragulaList[[counter]]))
    }
    if(!is.null(input[['PlotTypeDragula']][['target']][['BottomLeftPlot']])) {
      counter <- counter + 1L
      DragulaList[[counter]] <- input[['PlotTypeDragula']][['target']][['BottomLeftPlot']]
      print(paste0('BottomLeftPlot = ', DragulaList[[counter]]))
    }
    if(!is.null(input[['PlotTypeDragula']][['target']][['UpperRightPlot']])) {
      counter <- counter + 1L
      DragulaList[[counter]] <- input[['PlotTypeDragula']][['target']][['UpperRightPlot']]
      print(paste0('UpperRightPlot = ', DragulaList[[counter]]))
    }
    if(!is.null(input[['PlotTypeDragula']][['target']][['BottomRightPlot']])) {
      counter <- counter + 1L
      DragulaList[[counter]] <- input[['PlotTypeDragula']][['target']][['BottomRightPlot']]
      print(paste0('BottomRightPlot = ', DragulaList[[counter]]))
    }

    if(Debug) {print('counter = '); print(counter)}

    # Loop through plots
    CodeCollection <- list()
    for(run in seq_len(counter)) {

      CodeCollection[[run]] <- 1

      # ----

      # ----

      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
      # Prepare Data and Build Plots         ----
      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

      # Define PlotType
      if(Debug) print(DragulaList[[run]])
      PlotType <- DragulaList[[run]]

      if(Debug) {
        print('Here 15')
        print(run)
        print(tryCatch({YMinList}, error = function(x) NULL))
        print(tryCatch({XMinList}, error = function(x) NULL))
        print('Here 15.1')
      }

      # Check Logic before proceeding
      x1 <- tryCatch({YMinList[[run]]}, error = function(x) NULL)
      print('Here 15.2')
      x1a <- tryCatch({XMinList[[run]]}, error = function(x) NULL)
      print('Here 15.3')
      x2 <- RemixAutoML::ReturnParam(input, VarName=paste0('FilterVariable_', run, '_1'), Type='character', Default='None', Switch=TRUE)
      print('Here 15.4')
      x3 <- tryCatch({GroupVarsList[[run]]}, error = function(x) NULL)
      print('Here 15.5')
      x4 <- PlotType
      if(Debug) {print(x1); print(x1a); print(x2); print(x3); print(x4)}
      if(any((is.null(x1) && is.null(x1a)), is.null(x2), is.null(x4))) {
        shinyWidgets::sendSweetAlert(session, title = NULL, text = 'You need to specify additional variables to generate additional plots', type = NULL, btn_labels = "error", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
      } else if(PlotType %chin% 'Line' && any(class(data[[eval(tryCatch({DateVarList[[run]]}, error = function(x) 'None'))]]) %chin% c('numeric','integer','factor','character','logical','integer64', 'NULL'))) {
        shinyWidgets::sendSweetAlert(session, title = NULL, text = "X-Variable needs to be a Date, IDate, or Posix type", type = NULL, btn_labels = "error", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
      } else if(PlotType %chin% c('Scatter','Copula') && !any(class(data[[eval(tryCatch({XVarList[[run]]}, error = function(x) 'None'))]]) %chin% c('numeric','integer'))) {
        shinyWidgets::sendSweetAlert(session, title = NULL, text = "X-Variable needs to be a numeric or integer variable", type = NULL, btn_labels = "error", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
      } else {

        if(Debug) print('Here 133')

        # Grouping Variable Management
        if(!(exists('SubsetList') && !is.null(SubsetList[['RunNumber']]) && SubsetList[['RunNumber']] >= 1)) {

          if(Debug) print('Here 14 a')

          # AllPossGroupVars <- c(
          #   tryCatch({if(run > length(SelectedGroupsList)) 'None' else SelectedGroupsList[[run]]}, error = function(x) 'None'),
          #   RemixAutoML::ReturnParam(input, VarName=paste0('FacetVar_', run, '_1'), Type='character', Default='None', Switch=TRUE),
          #   RemixAutoML::ReturnParam(input, VarName=paste0('FacetVar_', run, '_2'), Type='character', Default='None', Switch=TRUE),
          #   RemixAutoML::ReturnParam(input, VarName=paste0('SizeVar', run), Type='character', Default='None', Switch=TRUE))

          SubsetList <- list()
          SubsetList[[paste0('RunNumber', run)]] <- 1L
          SubsetList[[paste0('DataPrep', run)]] <- TRUE

          if(Debug) {
            print('Here at 14c')
            print(run)
          }

          SubsetList[[paste0('GroupVars', run)]] <- tryCatch({GroupVarsList[[run]]}, error = function(x) NULL) #RemixAutoML::ReturnParam(input, VarName=paste0('GroupVars', run), Type='character', Default='None', Switch=TRUE)
          SubsetList[[paste0('Levels_', run, '_1')]] <- RemixAutoML::ReturnParam(input, VarName=paste0('Levels_', run, '_1'), Type='character', Default='None', Switch=TRUE)
          SubsetList[[paste0('Levels_', run, '_2')]] <- RemixAutoML::ReturnParam(input, VarName=paste0('Levels_', run, '_2'), Type='character', Default='None', Switch=TRUE)
          SubsetList[[paste0('Levels_', run, '_3')]] <- RemixAutoML::ReturnParam(input, VarName=paste0('Levels_', run, '_3'), Type='character', Default='None', Switch=TRUE)
          SubsetList[[paste0('FacetVar_', run, '_1')]] <- RemixAutoML::ReturnParam(input, VarName=paste0('FacetVar_', run, '_1'), Type='character', Default='None', Switch=TRUE)
          SubsetList[[paste0('FacetVar_', run, '_2')]] <- RemixAutoML::ReturnParam(input, VarName=paste0('FacetVar_', run, '_2'), Type='character', Default='None', Switch=TRUE)
          SubsetList[[paste0('SizeVar', run)]] <- RemixAutoML::ReturnParam(input, VarName=paste0('SizeVar', run), Type='character', Default='None', Switch=TRUE)
          SubsetList[[paste0('FilterVariable_', run, '_1')]] <- RemixAutoML::ReturnParam(input, VarName=paste0('FilterVariable_', run, '_1'), Type='character', Default='None', Switch=TRUE)
          SubsetList[[paste0('FilterVariable_', run, '_2')]] <- RemixAutoML::ReturnParam(input, VarName=paste0('FilterVariable_', run, '_2'), Type='character', Default='None', Switch=TRUE)
          SubsetList[[paste0('FilterVariable_', run, '_3')]] <- RemixAutoML::ReturnParam(input, VarName=paste0('FilterVariable_', run, '_3'), Type='character', Default='None', Switch=TRUE)
          SubsetList[[paste0('FilterVariable_', run, '_4')]] <- RemixAutoML::ReturnParam(input, VarName=paste0('FilterVariable_', run, '_4'), Type='character', Default='None', Switch=TRUE)
          SubsetList[[paste0('FilterLogic_', run, '_1')]] <- RemixAutoML::ReturnParam(input, VarName=paste0('FilterLogic_', run, '_1'), Type='character', Default='>=', Switch=TRUE)
          SubsetList[[paste0('FilterLogic_', run, '_2')]] <- RemixAutoML::ReturnParam(input, VarName=paste0('FilterLogic_', run, '_2'), Type='character', Default='>=', Switch=TRUE)
          SubsetList[[paste0('FilterLogic_', run, '_3')]] <- RemixAutoML::ReturnParam(input, VarName=paste0('FilterLogic_', run, '_3'), Type='character', Default='>=', Switch=TRUE)
          SubsetList[[paste0('FilterLogic_', run, '_3')]] <- RemixAutoML::ReturnParam(input, VarName=paste0('FilterLogic_', run, '_4'), Type='character', Default='>=', Switch=TRUE)
          SubsetList[[paste0('FilterValue_', run, '_1_1')]] <- RemixAutoML::ReturnParam(input, VarName=paste0('FilterValue_', run, '_1_1'), Type='character', Default=NULL, Switch=TRUE)
          SubsetList[[paste0('FilterValue_', run, '_1_2')]] <- RemixAutoML::ReturnParam(input, VarName=paste0('FilterValue_', run, '_1_2'), Type='character', Default=NULL, Switch=TRUE)
          SubsetList[[paste0('FilterValue_', run, '_2_1')]] <- RemixAutoML::ReturnParam(input, VarName=paste0('FilterValue_', run, '_2_1'), Type='character', Default=NULL, Switch=TRUE)
          SubsetList[[paste0('FilterValue_', run, '_2_2')]] <- RemixAutoML::ReturnParam(input, VarName=paste0('FilterValue_', run, '_2_2'), Type='character', Default=NULL, Switch=TRUE)
          SubsetList[[paste0('FilterValue_', run, '_3_1')]] <- RemixAutoML::ReturnParam(input, VarName=paste0('FilterValue_', run, '_3_1'), Type='character', Default=NULL, Switch=TRUE)
          SubsetList[[paste0('FilterValue_', run, '_3_2')]] <- RemixAutoML::ReturnParam(input, VarName=paste0('FilterValue_', run, '_3_2'), Type='character', Default=NULL, Switch=TRUE)
          SubsetList[[paste0('FilterValue_', run, '_4_1')]] <- RemixAutoML::ReturnParam(input, VarName=paste0('FilterValue_', run, '_4_1'), Type='character', Default=NULL, Switch=TRUE)
          SubsetList[[paste0('FilterValue_', run, '_4_2')]] <- RemixAutoML::ReturnParam(input, VarName=paste0('FilterValue_', run, '_4_2'), Type='character', Default=NULL, Switch=TRUE)
          assign(x = 'SubsetList', value = SubsetList, envir = .GlobalEnv)

        } else {

          if(Debug) print('Here 14 b')

          # MetaData
          SubsetList[[paste0('RunNumber', run)]] <- RunNumber + 1L
          SubsetList[[paste0('DataPrep', run)]] <- FALSE

          # Group Variables
          if(Debug) print('# Group Variables')
          if(!all(SubsetList[[paste0('GroupVars', run)]] == RemixAutoML::ReturnParam(input, VarName=paste0('GroupVars', run), Type='character', Default='None', Switch=TRUE))) {
            if(Debug) print(RemixAutoML::ReturnParam(input, VarName=paste0('GroupVars', run), Type='character', Default='None', Switch=TRUE))
            SubsetList[[paste0('GroupVars', run)]] <- RemixAutoML::ReturnParam(input, VarName=paste0('GroupVars', run), Type='character', Default='None', Switch=TRUE)
            SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(Debug) print('# Levels 1 1')
          if(!all(SubsetList[[paste0('Levels_', run, '_1')]] == RemixAutoML::ReturnParam(input, VarName= paste0('Levels_', run, '_1'), Type='character', Default='None', Switch=TRUE))) {
            if(Debug) print(RemixAutoML::ReturnParam(input, VarName=paste0('Levels_', run, '_1'), Type='character', Default='None', Switch=TRUE))
            SubsetList[[paste0('Levels_', run, '_1')]] <- RemixAutoML::ReturnParam(input, VarName=paste0('Levels_', run, '_1'), Type='character', Default='None', Switch=TRUE)
            SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('Levels_', run, '_2')]] == RemixAutoML::ReturnParam(input, VarName = paste0('Levels_', run, '_2'), Type = 'character', Default = 'None', Switch = TRUE))) {
            SubsetList[[paste0('Levels_', run, '_2')]] <- RemixAutoML::ReturnParam(input, VarName = paste0('Levels_', run, '_2'), Type = 'character', Default = 'None', Switch = TRUE)
            SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('Levels_', run, '_3')]] == RemixAutoML::ReturnParam(input, VarName = paste0('Levels_', run, '_3'), Type = 'character', Default = 'None', Switch = TRUE))) {
            SubsetList[[paste0('Levels_', run, '_3')]] <- RemixAutoML::ReturnParam(input, VarName = paste0('Levels_', run, '_3'), Type = 'character', Default = 'None', Switch = TRUE)
            SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FacetVar_', run, '_1')]] == RemixAutoML::ReturnParam(input, VarName = paste0('FacetVar_', run, '_1'), Type = 'character', Default = 'None', Switch = TRUE))) {
            SubsetList[[paste0('FacetVar_', run, '_1')]] <- RemixAutoML::ReturnParam(input, VarName = paste0('FacetVar_', run, '_1'), Type = 'character', Default = 'None', Switch = TRUE)
            SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FacetVar_', run, '_2')]] == RemixAutoML::ReturnParam(input, VarName = paste0('FacetVar_', run, '_2'), Type = 'character', Default = 'None', Switch = TRUE))) {
            SubsetList[[paste0('FacetVar_', run, '_2')]] <- RemixAutoML::ReturnParam(input, VarName = paste0('FacetVar_', run, '_2'), Type = 'character', Default = 'None', Switch = TRUE)
            SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('SizeVar', run)]] == RemixAutoML::ReturnParam(input, VarName = paste0('SizeVar', run), Type = 'character', Default = 'None', Switch = TRUE))) {
            SubsetList[[paste0('SizeVar', run)]] <- RemixAutoML::ReturnParam(input, VarName = paste0('SizeVar', run), Type = 'character', Default = 'None', Switch = TRUE)
            SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }

          # Filter Variables
          if(!all(SubsetList[[paste0('FilterVariable_', run, '_1')]] == RemixAutoML::ReturnParam(input, VarName = paste0('FilterVariable_', run, '_1'), Type = 'character', Default = 'None', Switch = TRUE))) {
            SubsetList[[paste0('FilterVariable_', run, '_1')]] <- RemixAutoML::ReturnParam(input, VarName = paste0('FilterVariable_', run, '_1'), Type = 'character', Default = 'None', Switch = TRUE)
            SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterVariable_', run, '_2')]] == RemixAutoML::ReturnParam(input, VarName = paste0('FilterVariable_', run, '_2'), Type = 'character', Default = 'None', Switch = TRUE))) {
            SubsetList[[paste0('FilterVariable_', run, '_2')]] <- RemixAutoML::ReturnParam(input, VarName = paste0('FilterVariable_', run, '_2'), Type = 'character', Default = 'None', Switch = TRUE)
            SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterVariable_', run, '_3')]] == RemixAutoML::ReturnParam(input, VarName = paste0('FilterVariable_', run, '_3'), Type = 'character', Default = 'None', Switch = TRUE))) {
            SubsetList[[paste0('FilterVariable_', run, '_3')]] <- RemixAutoML::ReturnParam(input, VarName = paste0('FilterVariable_', run, '_3'), Type = 'character', Default = 'None', Switch = TRUE)
            SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterVariable_', run, '_4')]] == RemixAutoML::ReturnParam(input, VarName = paste0('FilterVariable_', run, '_4'), Type = 'character', Default = 'None', Switch = TRUE))) {
            SubsetList[[paste0('FilterVariable_', run, '_4')]] <- RemixAutoML::ReturnParam(input, VarName = paste0('FilterVariable_', run, '_4'), Type = 'character', Default = 'None', Switch = TRUE)
            SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterLogic_', run, '_1')]] == RemixAutoML::ReturnParam(input, VarName = paste0('FilterLogic_', run, '_1'), Type = 'character', Default = '>=', Switch = TRUE))) {
            SubsetList[[paste0('FilterLogic_', run, '_1')]] <- RemixAutoML::ReturnParam(input, VarName = paste0('FilterLogic_', run, '_1'), Type = 'character', Default = '>=', Switch = TRUE)
            SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterLogic_', run, '_2')]] == RemixAutoML::ReturnParam(input, VarName = paste0('FilterLogic_', run, '_2'), Type = 'character', Default = '>=', Switch = TRUE))) {
            SubsetList[[paste0('FilterLogic_', run, '_2')]] <- RemixAutoML::ReturnParam(input, VarName = paste0('FilterLogic_', run, '_2'), Type = 'character', Default = '>=', Switch = TRUE)
            SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterLogic_', run, '_3')]] == RemixAutoML::ReturnParam(input, VarName = paste0('FilterLogic_', run, '_3'), Type = 'character', Default = '>=', Switch = TRUE))) {
            SubsetList[[paste0('FilterLogic_', run, '_3')]] <- RemixAutoML::ReturnParam(input, VarName = paste0('FilterLogic_', run, '_3'), Type = 'character', Default = '>=', Switch = TRUE)
            SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterLogic_', run, '_4')]] == RemixAutoML::ReturnParam(input, VarName = paste0('FilterLogic_', run, '_4'), Type = 'character', Default = '>=', Switch = TRUE))) {
            SubsetList[[paste0('FilterLogic_', run, '_4')]] <- RemixAutoML::ReturnParam(input, VarName = paste0('FilterLogic_', run, '_4'), Type = 'character', Default = '>=', Switch = TRUE)
            SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterValue_', run, '_1_1')]] == RemixAutoML::ReturnParam(input, VarName = paste0('FilterValue_', run, '_1_1'), Type = 'character', Default = NULL, Switch = TRUE))) {
            SubsetList[[paste0('FilterValue_', run, '_1_1')]] <- RemixAutoML::ReturnParam(input, VarName = paste0('FilterValue_', run, '_1_1'), Type = 'character', Default = NULL, Switch = TRUE)
            SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterValue_', run, '_1_2')]] == RemixAutoML::ReturnParam(input, VarName = paste0('FilterValue_', run, '_1_2'), Type = 'character', Default = NULL, Switch = TRUE))) {
            SubsetList[[paste0('FilterValue_', run, '_1_2')]] <- RemixAutoML::ReturnParam(input, VarName = paste0('FilterValue_', run, '_1_2'), Type = 'character', Default = NULL, Switch = TRUE)
            SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterValue_', run, '_2_1')]] == RemixAutoML::ReturnParam(input, VarName = paste0('FilterValue_', run, '_2_1'), Type = 'character', Default = NULL, Switch = TRUE))) {
            SubsetList[[paste0('FilterValue_', run, '_2_1')]] <- RemixAutoML::ReturnParam(input, VarName = paste0('FilterValue_', run, '_2_1'), Type = 'character', Default = NULL, Switch = TRUE)
            SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterValue_', run, '_2_2')]] == RemixAutoML::ReturnParam(input, VarName = paste0('FilterValue_', run, '_2_2'), Type = 'character', Default = NULL, Switch = TRUE))) {
            SubsetList[[paste0('FilterValue_', run, '_2_2')]] <- RemixAutoML::ReturnParam(input, VarName = paste0('FilterValue_', run, '_2_2'), Type = 'character', Default = NULL, Switch = TRUE)
            SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterValue_', run, '_3_1')]] == RemixAutoML::ReturnParam(input, VarName = paste0('FilterValue_', run, '_3_1'), Type = 'character', Default = NULL, Switch = TRUE))) {
            SubsetList[[paste0('FilterValue_', run, '_3_1')]] <- RemixAutoML::ReturnParam(input, VarName = paste0('FilterValue_', run, '_3_1'), Type = 'character', Default = NULL, Switch = TRUE)
            SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterValue_', run, '_3_2')]] == RemixAutoML::ReturnParam(input, VarName = paste0('FilterValue_', run, '_3_2'), Type = 'character', Default = NULL, Switch = TRUE))) {
            SubsetList[[paste0('FilterValue_', run, '_3_2')]] <- RemixAutoML::ReturnParam(input, VarName = paste0('FilterValue_', run, '_3_2'), Type = 'character', Default = NULL, Switch = TRUE)
            SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterValue_', run, '_4_1')]] == RemixAutoML::ReturnParam(input, VarName = paste0('FilterValue_', run, '_4_1'), Type = 'character', Default = NULL, Switch = TRUE))) {
            SubsetList[[paste0('FilterValue_', run, '_4_1')]] <- RemixAutoML::ReturnParam(input, VarName = paste0('FilterValue_', run, '_4_1'), Type = 'character', Default = NULL, Switch = TRUE)
            SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          if(!all(SubsetList[[paste0('FilterValue_', run, '_4_2')]] == RemixAutoML::ReturnParam(input, VarName = paste0('FilterValue_', run, '_4_2'), Type = 'character', Default = NULL, Switch = TRUE))) {
            SubsetList[[paste0('FilterValue_', run, '_4_2')]] <- RemixAutoML::ReturnParam(input, VarName = paste0('FilterValue_', run, '_4_2'), Type = 'character', Default = NULL, Switch = TRUE)
            SubsetList[[paste0('DataPrep', run)]] <- TRUE
          }
          assign(x = 'SubsetList', value = SubsetList, envir = .GlobalEnv)
        }

        if(Debug) print('Here 16')

        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
        # Prepare data for plotting            ----
        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

        # GroupVars creation
        if(Debug) print('Get GroupVars set up correctly')
        if(length(SubsetList[[paste0('GroupVars', run)]]) > 0) {
          if(Debug) print(SubsetList[[paste0('GroupVars', run)]])
          if(length(SubsetList[[paste0('GroupVars', run)]]) == 1 && SubsetList[[paste0('GroupVars', run)]] == 'None') {
            if(Debug) print('Here groupvars')
            GroupVars <- NULL
          } else if('None' %in% SubsetList[[paste0('GroupVars', run)]]) {
            if(Debug) print('There groupvars')
            zz <- SubsetList[[paste0('GroupVars', run)]]
            GroupVars <- zz[!zz %in% 'None']
          } else {
            if(Debug) print('Standard groupvars')
            GroupVars <- SubsetList[[paste0('GroupVars', run)]]
          }
        } else {
          if(Debug) print('holla groupvars')
          GroupVars <- NULL
        }

        if(Debug) print('Here 17')

        # DataPrep
        if(Debug) for(zzzz in 1:4) print(':: :: DataPrep :: ::')
        if(!SubsetList[[paste0('DataPrep', run)]]) {

          if(Debug) print('Here 18 a')
          data1 <- data

        } else {

          if(Debug) print('Here 18 b')

          if(Debug) print('remove NA')
          if(tryCatch({YVarList[[run]]}, error = function(x) 'None') != 'None') {
            data1 <- data[!is.na(get(YVarList[[run]]))]
            if(Debug) {print('data1 <- data[!is.na(get(YVarList[[run]]))]');print(data1[])}
            CodeCollection[[run]][[length(CodeCollection[[run]])+1L]] <- paste0("data1 <- data[!is.na(", YVarList[[run]], ")]")
          } else {
            data1 <- data
            CodeCollection[[run]][[length(CodeCollection[[run]])+1L]] <- paste0("data1 <- data")
          }

          if(Debug) print('Here 19')

          # Filter by Date
          if(Debug) print('Filter by Date')
          if(tryCatch({DateVarList[[run]]}, error = function(x) 'None') != 'None') {

            if(Debug) print('Here 20')

            if(Debug) {print('Date Checking Here');print(exists('temp'));print(exists('DateVariableCheck'));if(exists('DateVariableCheck')) print(DateVariableCheck == DateVarList[[run]]);print(exists('YVariableCheck'));if(exists('YVariableCheck')) print(YVariableCheck == tryCatch({YVarList[[run]]}, error = function(x) NULL))}
            if(!(exists('temp') && exists('DateVariableCheck') && DateVariableCheck == DateVarList[[run]] && exists('YVariableCheck') && YVariableCheck == tryCatch({YVarList[[run]]}, error = function(x) NULL))) {
              YVariableCheck <- tryCatch({YVarList[[run]]}, error = function(x) NULL)
              DateVariableCheck <-DateVarList[[run]]
              temp <- data1[, list(MinDate = min(get(DateVarList[[run]]), na.rm = TRUE), MaxDate = max(get(DateVarList[[run]]), na.rm = TRUE))]
              DataDateMin <- temp[['MinDate']]
              DataDateMax <- temp[['MaxDate']]
              InputDateMin <-DateMinList[[run]]
              InputDateMax <- DateMaxList[[run]]
              assign(x = 'YVariableCheck', value = YVariableCheck, envir = .GlobalEnv)
              assign(x = 'DateVariableCheck', value = DateVariableCheck, envir = .GlobalEnv)
              assign(x = 'temp', value = temp, envir = .GlobalEnv)
              assign(x = 'DataDateMin', value = DataDateMin, envir = .GlobalEnv)
              assign(x = 'DataDateMax', value = DataDateMax, envir = .GlobalEnv)
              assign(x = 'InputDateMin', value = InputDateMin, envir = .GlobalEnv)
              assign(x = 'InputDateMax', value = InputDateMax, envir = .GlobalEnv)
            }

            if(Debug) print('Here 21')

            if(Debug) {print('Debug Date Checks');print(DataDateMin);print(InputDateMin);print(DataDateMax);print(InputDateMax)}
            if(DataDateMin != InputDateMin || DataDateMax != InputDateMax) {
              data1 <- data1[get(DateVarList[[run]]) <= eval(InputDateMax) & get(DateVarList[[run]]) >= eval(InputDateMin)]
              if(Debug){print('here 2');print(paste0("data1 <- data1[", DateVarList[[run]]));print(DateMaxList[[run]]);print(RemixAutoML:::CEP(DateMaxList[[run]]));print(RemixAutoML:::CEP(DateMinList[[run]]))}
              CodeCollection[[run]][[length(CodeCollection[[run]])+1L]] <- paste0('data1 <- data1[, ', DateVarList[[run]], ' <= RemixAutoML:::CEP(', InputDateMax, ') & ', DateVarList[[run]], ' >= RemixAutoML:::CEP(', InputDateMin, ')]')
              if(Debug) print('here 3')
            }
          }

          if(Debug) print('Here 22')

          # Subset by FilterVariable_1
          if(Debug) print('Subset by FilterVariable_1')
          if(SubsetList[[paste0('FilterVariable_', run, '_1')]] != 'None') {
            fv <- SubsetList[[paste0('FilterValue_', run, '_1_2')]]
            data1 <- RemixAutoML::FilterLogicData(data1, FilterLogic=SubsetList[[paste0('FilterLogic_', run, '_1')]], FilterVariable=SubsetList[[paste0('FilterVariable_', run, '_1')]], FilterValue=SubsetList[[paste0('FilterValue_', run, '_1_1')]], FilterValue2=fv, Debug = Debug)
            CodeCollection[[run]][[length(CodeCollection[[run]])+1L]] <- paste0("data1 <- RemixAutoML::FilterLogicData(data1, FilterLogic=", RemixAutoML:::CEP(SubsetList[[paste0('FilterLogic_', run, '_1')]]),", FilterVariable=", RemixAutoML:::CEP(SubsetList[[paste0('FilterVariable_', run, '_1')]]),", FilterValue=", RemixAutoML:::CEP(SubsetList[[paste0('FilterValue_', run, '_1_1')]]),", FilterValue2=", RemixAutoML:::CEP(fv),"))")
            if(Debug) print(data1)
          }

          if(Debug) print('Here 23')

          # Subset by FilterVariable_2
          if(Debug) print('Subset by FilterVariable_2')
          if(SubsetList[[paste0('FilterVariable_', run, '_2')]] != 'None') {
            fv <- SubsetList[[paste0('FilterValue_', run, '_2_2')]]
            data1 <- RemixAutoML::FilterLogicData(data1, FilterLogic=SubsetList[[paste0('FilterLogic_', run, '_2')]], FilterVariable=SubsetList[[paste0('FilterVariable_', run, '_2')]], FilterValue=SubsetList[[paste0('FilterValue_', run, '_2_1')]], FilterValue2=fv)
            CodeCollection[[run]][[length(CodeCollection[[run]])+1L]] <- paste0("data1 <- RemixAutoML::FilterLogicData(data1, FilterLogic=", RemixAutoML:::CEP(SubsetList[[paste0('FilterLogic_', run, '_2')]]),", FilterVariable=", RemixAutoML:::CEP(SubsetList[[paste0('FilterVariable_', run, '_2')]]),", FilterValue=", RemixAutoML:::CEP(SubsetList[[paste0('FilterValue_', run, '_2_1')]]),", FilterValue2=", RemixAutoML:::CEP(fv),"))")
          }

          if(Debug) print('Here 24')

          # Subset by FilterVariable_3
          if(Debug) print('Subset by FilterVariable_3')
          if(SubsetList[[paste0('FilterVariable_', run, '_3')]] != 'None') {
            fv <- SubsetList[[paste0('FilterValue_', run, '_3_2')]]
            data1 <- RemixAutoML::FilterLogicData(data1, FilterLogic=SubsetList[[paste0('FilterLogic_', run, '_3')]], FilterVariable=SubsetList[[paste0('FilterVariable_', run, '_3')]], FilterValue=SubsetList[[paste0('FilterValue_', run, '_3_1')]], FilterValue2=fv)
            CodeCollection[[run]][[length(CodeCollection[[run]])+1L]] <- paste0("data1 <- RemixAutoML::FilterLogicData(data1, FilterLogic=", RemixAutoML:::CEP(SubsetList[[paste0('FilterLogic_', run, '_3')]]),", FilterVariable=", RemixAutoML:::CEP(SubsetList[[paste0('FilterVariable_', run, '_3')]]),", FilterValue=", RemixAutoML:::CEP(SubsetList[[paste0('FilterValue_', run, '_3_1')]]),", FilterValue2=", RemixAutoML:::CEP(fv),"))")
          }

          if(Debug) print('Here 25')

          # Subset by FilterVariable_4
          if(Debug) {print('Subset by FilterVariable_4')}
          if(SubsetList[[paste0('FilterVariable_', run, '_4')]] != 'None') {
            fv <- SubsetList[[paste0('FilterValue_', run, '_4_2')]]
            data1 <- RemixAutoML::FilterLogicData(data1, FilterLogic=SubsetList[[paste0('FilterLogic_', run, '_4')]], FilterVariable=SubsetList[[paste0('FilterVariable_', run, '_4')]], FilterValue=SubsetList[[paste0('FilterValue_', run, '_4_1')]], FilterValue2=fv)
            CodeCollection[[run]][[length(CodeCollection[[run]])+1L]] <- paste0("data1 <- RemixAutoML::FilterLogicData(data1, FilterLogic=", RemixAutoML:::CEP(SubsetList[[paste0('FilterLogic_', run, '_4')]]),", FilterVariable=", RemixAutoML:::CEP(SubsetList[[paste0('FilterVariable_', run, '_4')]]),", FilterValue=", RemixAutoML:::CEP(SubsetList[[paste0('FilterValue_', run, '_4_1')]]),", FilterValue2=", RemixAutoML:::CEP(fv),"))")
          }

          if(Debug) print('Here 26')

          # Subset Rows based on Filters
          if(Debug) {print('Checking Levels_1, Levels_2, and Levels_3'); print(SubsetList[[paste0('Levels_', run, '_1')]]); print(SubsetList[[paste0('Levels_', run, '_2')]]); print(SubsetList[[paste0('Levels_', run, '_3')]])}
          if(Debug) {print('Checking YVar(), XVar(), and DateVar()'); print(tryCatch({YVarList[[run]]}, error = function(x) NULL)); print(tryCatch({XVarList[[run]]}, error = function(x) NULL)); print(tryCatch({DateVarList[[run]]}, error = function(x) NULL))}
          data1 <- RemixAutoML::PreparePlotData(
            SubsetOnly = if(PlotType %chin% c('BoxPlot','ViolinPlot','Scatter','Copula','Histogram','Train_ParDepPlots','Test_ParDepPlots','Train_ParDepBoxPlots','Test_ParDepBoxPlots','Test__EvaluationPlot','Train_EvaluationPlot','Test_EvaluationBoxPlot','Train_EvaluationBoxPlot')) TRUE else FALSE,
            data = data1, Aggregate = 'mean', TargetVariable = tryCatch({YVarList[[run]]}, error = function(x) NULL),
            DateVariable = if(tryCatch({DateVarList[[run]]}, error = function(x) 'None') == 'None') NULL else tryCatch({DateVarList[[run]]}, error = function(x) NULL), GroupVariables = GroupVars,
            G1Levels = SubsetList[[paste0('Levels_', run, '_1')]], G2Levels = SubsetList[[paste0('Levels_', run, '_2')]], G3Levels = SubsetList[[paste0('Levels_', run, '_3')]], Debug = Debug)
          CodeCollection[[run]][[length(CodeCollection[[run]])+1L]] <- paste0("data1 <- RemixAutoML::PreparePlotData(SubsetOnly = ", if(PlotType %chin% c('BoxPlot','ViolinPlot','Scatter','Copula','Histogram','Train_ParDepPlots','Test_ParDepPlots','Train_ParDepBoxPlots','Test_ParDepBoxPlots','Test__EvaluationPlot','Train_EvaluationPlot','Test_EvaluationBoxPlot','Train_EvaluationBoxPlot')) TRUE else FALSE,", data=data1, Aggregate='mean', TargetVariable=", RemixAutoML:::CEP(tryCatch({YVarList[[run]]}, error = function(x) NULL)),", DateVariable=", if(tryCatch({DateVarList[[run]]}, error = function(x) 'None') == 'None') NULL else tryCatch({DateVarList[[run]]}, error = function(x) NULL), ", GroupVariables=", RemixAutoML:::CEP(GroupVars),", G1Levels=", RemixAutoML:::CEP(SubsetList[[paste0('Levels_', run, '_1')]]),", G2Levels=", RemixAutoML:::CEP(SubsetList[[paste0('Levels_', run, '_2')]]),", G3Levels=", RemixAutoML:::CEP(SubsetList[[paste0('Levels_', run, '_3')]]),")")
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
        if(Debug) print(data1)
        if(Debug) print(GroupVars)
        x1 <- tryCatch({!is.null(ScoreVarList[[run]])}, error = function(x) FALSE)
        if(Debug) print('Here 27a')
        x2 <- tryCatch({PDP_VariableList[[run]] %in% names(data1)}, error = function(x) FALSE)
        if(Debug) print('Here 27b')
        x3 <- tryCatch({Percentile_BucketsList[[run]] != 20}, error = function(x) FALSE)
        if(Debug) print('Here 27c')
        x4 <- !is.null(GroupVars) && (!is.null(RemixAutoML::CharNull(SubsetList[[paste0('Levels_', run, '_1')]])) || !is.null(RemixAutoML::CharNull(SubsetList[[paste0('Levels_', run, '_2')]])) || !is.null(RemixAutoML::CharNull(SubsetList[[paste0('Levels_', run, '_3')]])))
        if(Debug) print('Here 27d')
        x5 <- any(c('Test_ParDepPlots','Train_ParDepPlots','Test_ParDepBoxPlots','Train_ParDepBoxPlots','Test_EvaluationPlot','Train_EvaluationPlot','Test_EvaluationBoxPlot','Train_EvaluationBoxPlot','Test_GainsPlot','Train_GainsPlot','Test_LiftPlot','Train_LiftPlot','Test_ScatterPlot','Train_ScatterPlot','Test_CopulaPlot','Train_CopulaPlot','Test_ResidualsHistogram','Train_ResidualsHistogram') %in% PlotType)
        if(Debug) print('Here 27e')
        Blocker <- !x1 || (!x2 && PlotType %in% c('Test_ParDepPlots','Train_ParDepPlots','Test_ParDepBoxPlots','Train_ParDepBoxPlots'))
        if(Debug) print('Here 27f')
        if(x5 || x4 || (x3 && PlotType %in% c('Test_ParDepPlots','Train_ParDepPlots','Test_ParDepBoxPlots','Train_ParDepBoxPlots'))) {
          if(Debug) print('Here 27g')
          if(Blocker) Rebuild <- FALSE else Rebuild <- TRUE
        } else {
          if(Debug) print('Here 27h')
          Rebuild <- FALSE
        }

        # ----

        # ----

        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
        # Create Plots                               ----
        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

        if(Debug) print('Here 28')

        if(Debug) {print('Create Plot Object'); print(PlotType)}
        if(PlotType %chin% c('BoxPlot', 'ViolinPlot', 'Bar', 'Line', 'Scatter', 'Copula', 'Histogram')) {

          # Debug
          if(Debug) {print(paste0('PlotType print: ', PlotType)); print(PlotType %chin% c('BoxPlot', 'ViolinPlot', 'Bar', 'Line', 'Scatter', 'Copula', 'Histogram'))}

          # XVar (complex because Date needs to become X in some contexts)
          if(PlotType == 'Line') {
            if(!is.null(tryCatch({DateVarList[[run]]}, error = function(x) NULL)) && DateVarList[[run]] != 'None') {
              xvar <- tryCatch({DateVarList[[run]]}, error = function(x) NULL)
            } else {
              xvar <- tryCatch({XVarList[[run]]}, error = function(x) NULL)
            }
          } else {
            if(tryCatch({XVarList[[run]]}, error = function(x) 'None') == 'None') {
              if(tryCatch({DateVarList[[run]]}, error = function(x) 'None') == 'None') {
                xvar <- NULL
              } else {
                xvar <- tryCatch({DateVarList[[run]]}, error = function(x) NULL)
                XMinList[[run]] <- tryCatch({DateVarList[[run]]}, error = function(x) NULL)
              }
            } else {
              xvar <- tryCatch({XVarList[[run]]}, error = function(x) NULL)
            }
          }

          # YVar
          if(tryCatch({YVarList[[run]]}, error = function(x) NULL) == 'None') {
            yvar <- NULL
          } else {
            yvar <- tryCatch({YVarList[[run]]}, error = function(x) NULL)
            if(Debug) print(paste0('yvar here: ', yvar))
          }

          if(RemixAutoML:::CEPP(tryCatch({YTicksList[[run]]}, error = function(x) 'No Data Loaded !!')) == 'No Data Loaded !!') {
            YTicksList[[run]] <- 'Default'
          }

          if(RemixAutoML:::CEPP(tryCatch({XTicksList[[run]]}, error = function(x) 'No Data Loaded !!')) == 'No Data Loaded !!') {
            XTicksList[[run]] <- 'Default'
          }

          # AutoPlotter()
          if(Debug) print('Run AutoPlotter')
          PlotCollectionList[[paste0('p', run)]] <- RemixAutoML:::AutoPlotter(
            dt = data1,
            PlotType = PlotType,
            YVar = yvar,
            YMin = tryCatch({RemixAutoML:::CEPP(YMinList[[run]])}, error = function(x) NULL),
            YMax = tryCatch({RemixAutoML:::CEPP(YMaxList[[run]])}, error = function(x) NULL),
            XVar = xvar,
            XMin = tryCatch({RemixAutoML:::CEPP(XMinList[[run]])}, error = function(x) NULL),
            XMax = tryCatch({RemixAutoML:::CEPP(XMaxList[[run]])}, error = function(x) NULL),
            ColorVariables = tryCatch({GroupVars}, error = function(x) NULL),
            SizeVar1 = tryCatch({RemixAutoML:::CEPP(SubsetList[[paste0('SizeVar', run)]])}, error = function(x) NULL),
            FacetVar1 = tryCatch({RemixAutoML:::CEPP(SubsetList[[paste0('FacetVar_', run, '_1')]])}, error = function(x) NULL),
            FacetVar2 = tryCatch({RemixAutoML:::CEPP(SubsetList[[paste0('FacetVar_', run, '_2')]])}, error = function(x) NULL),
            YTicks = tryCatch({RemixAutoML:::CEPP(YTicksList[[run]])}, error = function(x) 'Default'),
            XTicks = tryCatch({RemixAutoML:::CEPP(XTicksList[[run]])}, error = function(x) 'Default'),
            OutlierSize = OutlierSize,
            OutlierColor = OutlierColor,
            BoxPlotFill = BoxPlotFill,
            GamFitScatter = tryCatch({RemixAutoML:::CEPP(GamFitScatterList[[run]])}, error = function(x) FALSE),
            TextSize = TextSize,
            TextColor = TextColor,
            AngleX = AngleX,
            AngleY = AngleY,
            ChartColor = ChartColor,
            BorderColor = BorderColor,
            GridColor = GridColor,
            BackGroundColor = BackGroundColor,
            LegendPosition = LegendPosition,
            LegendBorderSize = tryCatch({as.numeric(LegendBorderSize)}, error = function(x) 0.50),
            Debug = Debug)

          if(Debug) {
            print('Here 29')
            print('paste0(RemixAutoML:::AutoPlotter(dt = data1, PlotType = , ')
            print(tryCatch({RemixAutoML:::CEPP(PlotType)}, error = function(x) 'Error'))
            print(paste0('YVar = ', yvar))
            print(paste0('YMin = ', tryCatch({RemixAutoML:::CEPP(YMinList[[run]])}, error = function(x) 'Error')))
            print(paste0('YMax = ', tryCatch({RemixAutoML:::CEPP(YMaxList[[run]])}, error = function(x) 'Error')))
            print(paste0('XVar = ', xvar))
            print(paste0('XMin = ', tryCatch({RemixAutoML:::CEPP(XMinList[[run]])}, error = function(x) 'Error')))
            print(paste0('XMax = ', tryCatch({RemixAutoML:::CEP(XMaxList[[run]])}, error = function(x) 'Error')))
            print(paste0('ColorVariables = ', tryCatch({RemixAutoML:::CEP(GroupVars)}, error = function(x) 'Error')))
            print(paste0('SizeVar1 = ', tryCatch({RemixAutoML:::CEP(SubsetList[[paste0('SizeVar', run)]])}, error = function(x) 'Error')))
            print(paste0('FacetVar1 = ', tryCatch({RemixAutoML:::CEP(SubsetList[[paste0('FacetVar_', run, '_1')]])}, error = function(x) 'Error')))
            print(paste0('FacetVar2 = ', tryCatch({RemixAutoML:::CEP(SubsetList[[paste0('FacetVar_', run, '_2')]])}, error = function(x) 'Error')))
            print(paste0('YTicks = ', tryCatch({RemixAutoML:::CEP(YTicksList[[run]])}, error = function(x) 'Error')))
            print(paste0('XTicks = ', tryCatch({RemixAutoML:::CEP(XTicksList[[run]])}, error = function(x) 'Error')))
            print(paste0('OutlierSize = ', tryCatch({RemixAutoML:::CEP(OutlierSize)}, error = function(x) 'Error')))
            print(paste0('OutlierColor = ', tryCatch({RemixAutoML:::CEP(OutlierColor)}, error = function(x) 'Error')))
            print(paste0('BoxPlotFill = ', tryCatch({RemixAutoML:::CEP(BoxPlotFill)}, error = function(x) 'Error')))
            print(paste0('GamFitScatter = ', tryCatch({RemixAutoML:::CEP(GamFitScatterList[[run]])}, error = function(x) 'Error')))
            print(paste0('TextSize = ', tryCatch({RemixAutoML:::CEP(TextSize)}, error = function(x) 'Error')))
            print(paste0('TextColor = ', tryCatch({RemixAutoML:::CEP(TextColor)}, error = function(x) 'Error')))
            print(paste0('AngleX = ', tryCatch({RemixAutoML:::CEP(AngleX)}, error = function(x) 'Error')))
            print(paste0('AngleY = ', tryCatch({RemixAutoML:::CEP(AngleY)}, error = function(x) 'Error')))
            print(paste0('ChartColor = ', tryCatch({RemixAutoML:::CEP(ChartColor)}, error = function(x) 'Error')))
            print(paste0('BorderColor = ', tryCatch({RemixAutoML:::CEP(BorderColor)}, error = function(x) 'Error')))
            print(paste0('GridColor = ', tryCatch({RemixAutoML:::CEP(GridColor)}, error = function(x) 'Error')))
            print(paste0('BackGroundColor = ', tryCatch({RemixAutoML:::CEP(BackGroundColor)}, error = function(x) 'Error')))
            print(paste0('LegendBorderSize = ', tryCatch({RemixAutoML:::CEP(as.numeric(LegendBorderSize))}, error = function(x) 'Error')))
            print(paste0('LegendPosition = ', tryCatch({RemixAutoML:::CEP(LegendPosition)}, error = function(x) 'Error')))
          }

          if(Debug) print('Debug Code Collection')

          CodeCollection[[run]][[length(CodeCollection[[run]])+1L]] <- paste0("RemixAutoML:::AutoPlotter(dt = data1, PlotType = ", RemixAutoML:::CEP(PlotType),", YVar=", RemixAutoML:::CEP(yvar),", YMin=", if(run > length(YMinList)) 'NULL' else RemixAutoML:::CEPP(YMinList[[run]]),", YMax=", if(run > length(YMaxList)) 'NULL' else RemixAutoML:::CEPP(YMaxList[[run]]),", XVar=", RemixAutoML:::CEP(xvar),", XMin=", if(run > length(XMinList)) 'NULL' else RemixAutoML:::CEPP(XMinList[[run]]),", XMax=", if(run > length(XMaxList)) 'NULL' else RemixAutoML:::CEP(XMaxList[[run]]),", ColorVariables=", tryCatch({RemixAutoML:::CEP(GroupVars)}, error = function(x) 'Error'),", SizeVar1=", RemixAutoML:::CEP(SubsetList[[paste0('SizeVar', run)]]),", FacetVar1=", RemixAutoML:::CEP(SubsetList[[paste0('FacetVar_', run, '_1')]]), ", FacetVar2=", RemixAutoML:::CEP(SubsetList[[paste0('FacetVar_', run, '_2')]]),", YTicks=", tryCatch({RemixAutoML:::CEP(YTicksList[[run]])}, error = function(x) 'NULL'),", XTicks=", tryCatch({RemixAutoML:::CEP(XTicksList[[run]])}, error = function(x) 'NULL'),", OutlierSize=", RemixAutoML:::CEP(OutlierSize),", OutlierColor=", RemixAutoML:::CEP(OutlierColor), ", BoxPlotFill=", RemixAutoML:::CEP(BoxPlotFill),", GamFitScatter=", RemixAutoML:::CEP(GamFitScatterList[[run]]),", TextSize=", RemixAutoML:::CEP(TextSize), ", TextColor=", RemixAutoML:::CEP(TextColor),", AngleX=", RemixAutoML:::CEP(AngleX), ", AngleY=", RemixAutoML:::CEP(AngleY),", ChartColor=", RemixAutoML:::CEP(ChartColor), ", BorderColor=", RemixAutoML:::CEP(BorderColor),", GridColor=", RemixAutoML:::CEP(GridColor),", BackGroundColor=", RemixAutoML:::CEP(BackGroundColor), ")")
          CodeCollection <<- CodeCollection
          if(Debug) print(unlist(CodeCollection))

        } else if(!PlotType %chin% c('BoxPlot', 'ViolinPlot', 'Bar', 'Line', 'Scatter', 'Copula', 'Histogram')) {

          # AppModelInsights()
          if(Debug) print(paste0('PDPVar = ', input[['PDP_Variable']]))
          PlotCollectionList[[paste0('p', run)]] <- RemixAutoML:::AppModelInsights(
            dt = data1,
            PlotType = PlotType,
            ModelOutputList = ModelOutputList,
            TargetVar = yvar,
            PredictVar = if(ScoreVarList[[run]] != 'None') ScoreVarList[[run]] else NULL,
            PDPVar = if(!is.null(PDP_VariableList[[run]])) PDP_VariableList[[run]] else NULL,
            DateVar = if(DateVarList[[run]] != 'None') DateVarList[[run]] else NULL,
            GamFit = GamFitScatterList[[run]],
            Buckets = as.numeric(Percentile_BucketsList[[run]]),
            Rebuild = Rebuild, Debug = Debug)
          CodeCollection[[run]][[length(CodeCollection[[run]])+1L]] <- paste0("RemixAutoML:::AppModelInsights(dt=data1, PlotType=", RemixAutoML:::CEP(PlotType), ", ModelOutputList=ModelOutputList, TargetVar=", RemixAutoML:::CEP(yvar), ", PredictVar=", if(ScoreVarList[[run]] != 'None') RemixAutoML:::CEP(ScoreVarList[[run]]) else NULL, ", PDPVar=", if(!is.null(PDP_VariableList[[run]])) RemixAutoML:::CEP(PDP_VariableList[[run]]) else NULL, ", DateVar=", if(tryCatch({DateVarList[[run]]}, error = function(x) 'None') != 'None') RemixAutoML:::CEP(tryCatch({DateVarList[[run]]}, error = function(x) 'None')) else NULL, ", GamFit=", RemixAutoML:::CEP(GamFitScatterList[[run]]), ", Buckets=", RemixAutoML:::CEP(as.numeric(Percentile_BucketsList[[run]])), ",Rebuild=", RemixAutoML:::CEP(Rebuild), ")")
          if(!is.null(PlotCollectionList[[paste0('p', run)]])) {
            PlotCollectionList[[paste0('p', run)]] <- PlotCollectionList[[paste0('p', run)]] + RemixAutoML::ChartTheme(
              Size = TextSize, AngleX = AngleX, AngleY = AngleY, ChartColor = ChartColor,
              BorderColor = BorderColor, TextColor = TextColor, GridColor = GridColor,
              BackGroundColor = BackGroundColor, SubTitleColor = SubTitleColor,
              LegendPosition = LegendPosition, LegendBorderSize = as.numeric(LegendBorderSize),
              LegendLineType = LegendLineType)
            CodeCollection[[run]][[length(CodeCollection[[run]])+1L]] <- paste0("RemixAutoML::ChartTheme(Size=", RemixAutoML:::CEP(TextSize), ", AngleX=", RemixAutoML:::CEP(AngleX), ", AngleY=", RemixAutoML:::CEP(AngleY), ", ChartColor=", RemixAutoML:::CEP(ChartColor), ", BorderColor=", RemixAutoML:::CEP(BorderColor), ", TextColor=", RemixAutoML:::CEP(TextColor), ", GridColor=", RemixAutoML:::CEP(GridColor), ", BackGroundColor=", RemixAutoML:::CEP(BackGroundColor), ", SubTitleColor=", RemixAutoML:::CEP(SubTitleColor), ", LegendPosition=", RemixAutoML:::CEP(LegendPosition), ", LegendBorderSize=", RemixAutoML:::CEP(as.numeric(LegendBorderSize)), ", LegendLineType=", RemixAutoML:::CEP(LegendLineType), ")")
          }
          CodeCollection <<- CodeCollection
        }

        # ----

        # ----

      }

      data1 <<- data1
      assign(x = 'PlotCollectionList', value = PlotCollectionList)
    }

    assign(x = 'PlotCollectionList', value = PlotCollectionList)

    # ----

    # ----

    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    # Return Plot to UI                          ----
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    if(Debug) {print('Return Plot to UI'); print(exists("PlotCollectionList")); print(class(PlotCollectionList[[paste0('p', 1)]]))}

    if(exists("PlotCollectionList") && !is.null(PlotCollectionList[[paste0('p', 1)]])) {

      if(Debug) print('Convert p1 to global env')
      if(Debug) {
        print(paste0('Length of PlotCollectionList = ', length(PlotCollectionList)))
        print(paste0('Names of PlotCollectionList = ', names(PlotCollectionList)))
        print(for(i in names(PlotCollectionList)) paste0('Names of PlotCollectionList = ', class(PlotCollectionList[[i]])))
        print('Create Plot output$Trend')
      }
      CodeCollection[[length(CodeCollection)+1L]] <- 'gridExtra::grid.arrange(gridExtra::arrangeGrob(grobs = PlotCollectionList, as.table = FALSE))'
      # Didnt work right away: gridExtra::grid.arrange(gridExtra::arrangeGrob(grobs = PlotCollectionList, as.table = FALSE))

      N <- length(PlotCollectionList)
      if(Debug) print('Length of N = ', N)

      if(N == 1L) {
        if(Debug) print('N == 1L case')
        p1 <- PlotCollectionList[['p1']]
        if(Debug) print(paste0('Class of p1 is :: ', class(p1)))
        output$Trend <- shiny::renderPlot(width = PlotWidth, height = PlotHeight, {
          gridExtra::grid.arrange(p1, ncol=1)
        })
      } else if(N == 2L) {
        if(Debug) print('N == 2L case')
        p1 <- PlotCollectionList[['p1']]
        p2 <- PlotCollectionList[['p2']]
        if(Debug) print(paste0('Class of p1 is :: ', class(p1)))
        if(Debug) print(paste0('Class of p1 is :: ', class(p2)))
        output$Trend <- shiny::renderPlot(width = PlotWidth, height = PlotHeight, {
          gridExtra::grid.arrange(p1,p2, ncol=1)
        })
      } else if(N == 3L) {
        if(Debug) print('N == 3L case')
        p1 <- PlotCollectionList[['p1']]
        p2 <- PlotCollectionList[['p2']]
        p3 <- PlotCollectionList[['p3']]
        if(Debug) print(paste0('Class of p1 is :: ', class(p1)))
        if(Debug) print(paste0('Class of p1 is :: ', class(p2)))
        if(Debug) print(paste0('Class of p1 is :: ', class(p3)))
        output$Trend <- shiny::renderPlot(width = PlotWidth, height = PlotHeight, {
          gridExtra::grid.arrange(p1,p3,p2, layout_matrix = rbind(
            c(1, 2),
            c(3, 3)))
          #gridExtra::grid.arrange(p1,p3,p2, ncol=2)
        })
      } else if(N == 4L) {
        if(Debug) print('N == 4L case')
        p1 <- PlotCollectionList[['p1']]
        p2 <- PlotCollectionList[['p2']]
        p3 <- PlotCollectionList[['p3']]
        p4 <- PlotCollectionList[['p4']]
        if(Debug) print(paste0('Class of p1 is :: ', class(p1)))
        if(Debug) print(paste0('Class of p1 is :: ', class(p2)))
        if(Debug) print(paste0('Class of p1 is :: ', class(p3)))
        if(Debug) print(paste0('Class of p1 is :: ', class(p4)))
        output$Trend <- shiny::renderPlot(width = PlotWidth, height = PlotHeight, {
          gridExtra::grid.arrange(p1,p3,p2,p4, ncol=2)
        })
      }
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = 'Plot could not build. Check for missing variables, such as Date Variables.', type = NULL, btn_labels = "error", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  })

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Close app after closing browser      ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  session$onSessionEnded(function() {
    shiny::stopApp()
  })
}

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# Run App                              ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
shiny::shinyApp(ui = ui, server = server)

# ----

# ----
