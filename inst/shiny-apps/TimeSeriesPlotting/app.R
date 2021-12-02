library(data.table)

#' Search for object with specific class in an environment
#'
#' @param what a class to look for
#' @param env An environment
#'
#' @return Character vector of the names of objects, NULL if none
#' @noRd
#'
#' @examples
#'
#' # NULL if no data.frame
#' search_obj("data.frame")
#'
#' library(ggplot2)
#' data("mpg")
#' search_obj("data.frame")
#'
#'
#' gg <- ggplot()
#' search_obj("ggplot")
#'
search_obj <- function(what = "data.frame", env = globalenv()) {
  all <- ls(name = env)
  objs <- lapply(
    X = all,
    FUN = function(x) {
      if (inherits(get(x, envir = env), what = what)) {
        x
      } else {
        NULL
      }
    }
  )
  objs <- unlist(objs)
  if (length(objs) == 1 && objs == "") {
    NULL
  } else {
    objs
  }
}

#' Tag to display code
#'
#' @param ... Character strings
#'
#' @noRd
rCodeContainer <- function(...) {
  code <- htmltools::HTML(as.character(tags$code(class = "language-r", ...)))
  htmltools::tags$div(htmltools::tags$pre(code))
}

#'@noRd
UniqueLevels <- function(input, data, n, GroupVars=NULL) {
  if(is.null(GroupVars[[n]]) || is.na(GroupVars[[n]])) {
    x <- NULL
  } else {
    x <- tryCatch({
      c(sort(unique(data[[eval(GroupVars[[n]])]])))}, error = function(x)  NULL)
  }
  x
}

#' @noRd
FilterValues <- function(data, VarName = input[['FilterVariable_1']], type = 1) {
  if(tolower(class(data[[eval(VarName)]]) %chin% c('numeric', 'integer'))) {
    x <- unique(as.numeric(sort(data[, quantile(get(VarName), probs = c(seq(0, 1, 0.05)), na.rm = TRUE)])))
  } else if(tolower(class(data[[eval(VarName)]])) %chin% c('factor', 'character')) {
    x <- sort(data[, unique(get(VarName))])
  } else {
    x <- NULL
  }
  x
}

#' @noRd
FilterLogicData <- function(data1, input, FilterLogic = input[['FilterLogic']], FilterVariable = input[['FilterVariable_1']], FilterValue = input[['FilterValue_1a']], FilterValue2 = input[['FilterValue_1b']], Debug = FALSE) {
  if(tolower(class(data1[[eval(FilterVariable)]])) %chin% c('factor', 'character')) {
    if(Debug) print('FilterLogicData else if')
    if(Debug) print(tolower(class(data1[[eval(FilterVariable)]])) %chin% c('factor', 'character'))
    if(FilterLogic == '%in%') {
      data1 <- data1[get(FilterVariable) %chin% c(eval(FilterValue))]
    } else if(input[[eval(FilterLogic)]] == '%like%') {
      data1 <- data1[get(eval(FilterVariable)) %like% c(eval(FilterValue))]
    }
  } else if(tolower(class(data1[[eval(FilterVariable)]])) %chin% c('numeric', 'integer', 'date', 'posix')) {
    if(Debug) print('FilterLogicData else if')
    if(Debug) print(tolower(class(data1[[eval(FilterVariable)]])) %chin% c('numeric', 'integer', 'date', 'posix'))
    if(FilterLogic == '>') {
      data1 <- data1[get(FilterVariable) > eval(as.numeric(FilterValue))]
    } else if(FilterLogic == '>=') {
      data1 <- data1[get(FilterVariable) >= eval(as.numeric(FilterValue))]
    } else if(FilterLogic == '<') {
      data1 <- data1[get(FilterVariable) < eval(as.numeric(FilterValue2))]
    } else if(FilterLogic == '%between%') {
      if(Debug) print('At %between% section')
      if(Debug) print(as.numeric(FilterVariable))
      if(Debug) print(as.numeric(FilterValue))
      if(Debug) print(as.numeric(FilterValue2))
      if(Debug) print(data1)
      if(Debug) print('Run data.table operation')
      data1 <- data1[get(FilterVariable) >= eval(as.numeric(FilterValue)) & get(FilterVariable) <= eval(as.numeric(FilterValue2))]
      if(Debug) print('Done with data.table operation')
      if(Debug) print(data1)
    } else if(FilterLogic == 'not %between%') {
      data1 <- data1[get(FilterVariable) < eval(as.numeric(FilterValue)) | get(FilterVariable) > eval(as.numeric(FilterValue2))]
    } else {
      data1 <- data1[get(FilterVariable) <= eval(as.numeric(FilterValue2))]
    }
  }
  data1
}

#' @noRd
KeyVarsInit <- function(data, VarName = NULL, type = 1) {
  if(!is.null(VarName) && any(c('numeric','integer') %chin% class(data[[eval(VarName)]]))) {
    minn <- tryCatch({floor(data[, min(get(eval(VarName)), na.rm = TRUE)])}, error = function(x) NULL)
    maxx <- tryCatch({ceiling(data[, max(get(eval(VarName)), na.rm = TRUE)])}, error = function(x) NULL)
    choices <- tryCatch({unique(as.character(round(as.numeric(sort(data[, quantile(get(VarName), probs = c(seq(0, 1, 0.05)), na.rm = TRUE)])), 5L)))}, error = function(x) {
      tryCatch({unique(data[[eval(VarName)]])}, error = NULL)
    })
  } else if(!is.null(VarName) && any(c('Date','IDate','POSIXct','POSIXt','character','factor') %chin% class(data[[(eval(VarName))]][[1L]]))) {
    choices <- tryCatch({unique(data[[eval(VarName)]])}, error = function(x) NULL)
    maxx <- tryCatch({data[, max(get(VarName), na.rm = TRUE)]}, error = function(x) NULL)
    minn <- tryCatch({data[, min(get(VarName), na.rm = TRUE)]}, error = function(x) NULL)
  } else {
    minn <- NULL
    maxx <- NULL
    choices <- NULL
  }
  return(list(MinVal = minn, MaxVal = maxx, ChoiceInput = choices))
}

#' @noRd
GetFilterValueLabel <- function(data, VarName = NULL, type = 1) {
  if((!is.null(VarName) || tolower(VarName) != 'None') && !is.null(data)) {
    if(is.numeric(data[[eval(VarName)]])) {
      if(type == 1) x <- 'Min Value' else x <- 'Max Value'
    }  else {
      x <- 'Select Levels'
    }
  } else {
    x <- 'N/A'
  }
  x
}

#' @noRd
GetFilterValueMultiple <- function(data, VarName = NULL, type = 1) {
  if((!is.null(VarName) || tolower(VarName) != 'None') && !is.null(data)) {
    if(!is.numeric(data[[eval(VarName)]])) x <- TRUE else x <- FALSE
  } else {
    x <- FALSE
  }
  x
}

#' @noRd
CharNull <- function(x) {
  if(!is.null(x)) {
    return(as.character(x))
  } else {
    return(NULL)
  }
}

#' @noRd
NumNull <- function(x) {
  if(!is.null(x)) {
    return(as.numeric(x))
  } else {
    return(NULL)
  }
}

#' @noRd
IntNull <- function(x) {
  if(!is.null(x)) {
    return(as.integer(x))
  } else {
    return(NULL)
  }
}

# Turn up horsepower
data.table::setDTthreads(threads = max(1L, parallel::detectCores()-1L))

# Passthrough Args

# data related
data <- shiny::getShinyOption('data')
XVariable <- shiny::getShinyOption('XVariable')
YVariable <- shiny::getShinyOption('YVariable')
DateName <- shiny::getShinyOption('DateName')
GroupVariables <- shiny::getShinyOption('GroupVariables')
FilterVariable <- shiny::getShinyOption('FilterVariable')

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
Debug <- shiny::getShinyOption('Debug')

# Global settings
options(shiny.maxRequestSize = 300000*1024^2)
options(scipen = 999)

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# Create ui ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
ui <- shinydashboard::dashboardPage(

  # Top of page color
  skin = HeaderColor,

  # App Header
  shinydashboard::dashboardHeader(

    # App Title and Width
    #title = AppTitle,
    #titleWidth = 190,

    htmltools::tags$li(class = "dropdown",
                       htmltools::tags$style(".main-header {max-height: 55px}"),
                       htmltools::tags$style(".main-header .logo {height: 55px;}"),
                       htmltools::tags$style(".sidebar-toggle {height: 20px; padding-top: 1px !important;}"),
                       htmltools::tags$style(".navbar {min-height:55px !important}")
    ),
    titleWidth = 190,

    #<img src='https://www.remixinstitute.com/wp-content/uploads/7b-Cheetah_Charcoal_Inline_No_Sub_No_BG.png' align = 'center' height = '20px'></img>
    # Title ----
    title = HTML(
      "
      <div style = 'vertical-align:middle'>
      <img src='NewPackageLogo.png' align = 'center' height = '53px'></img>
      </div>
      ")

    ),


  # App Sidebar
  shinydashboard::dashboardSidebar(disable = TRUE),

  # App Body
  shinydashboard::dashboardBody(

    # Add Space
    RemixAutoML::BlankRow(AppWidth),

    # Add Logo Image
    #shiny::fluidRow(shiny::img(src = 'LogoNoPuma.png', width = LogoWidth, height = LogoHeight)),
    #shiny::fluidRow(shiny::img(src = 'https://raw.githubusercontent.com/AdrianAntico/RemixAutoML/master/inst/shiny-apps/TimeSeriesPlotting/www/LogoNoPuma.png', width = LogoWidth, height = LogoHeight)),


    # Add Space
    #RemixAutoML::BlankRow(AppWidth),

    # ----

    # ----

    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    # Group Variables and Levels Selection ----
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

    # Group Variables Box
    shiny::fluidRow(
      shiny::column(
        width = AppWidth,
        shinyjs::useShinyjs(),
        shinydashboard::box(
          title = htmltools::tagList(shiny::icon('filter', lib = 'font-awesome'), 'Group Variables, Facets, and Levels'),
          solidHeader = TRUE,
          collapsible = FALSE,
          background = GroupVarsBoxColor,
          width = AppWidth,

          # Select GroupVariables
          shiny::column(3L, shiny::uiOutput('GroupVars')),
          shiny::column(3L, shiny::uiOutput('FacetRow')),
          shiny::column(3L, shiny::uiOutput('FacetCol')),

          # Add Space
          RemixAutoML::BlankRow(AppWidth),

          # GroupVar1 level selection
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
              shiny::uiOutput('Levels_3')))))),

    # Add Space
    RemixAutoML::BlankRow(AppWidth),

    # ----

    # ----

    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    # Variables ----
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

    # X, Y, and Date Variables Box
    shiny::fluidRow(
      shiny::column(
        width = AppWidth,
        shinyjs::useShinyjs(),
        shinydashboard::box(
          title = htmltools::tagList(shiny::icon('filter', lib = 'font-awesome'), 'Plotting Variables'),
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,
          background = VarsBoxColor,
          width = AppWidth,

          # Select the Y-Variable
          shiny::fluidRow(
            shiny::column(3L, shiny::uiOutput('YVar')),
            shiny::column(3L, shiny::uiOutput('YMin')),
            shiny::column(3L, shiny::uiOutput('YMax'))),

          # Select the X-Variable
          shiny::fluidRow(
            shiny::column(3L, shiny::uiOutput('XVar')),
            shiny::column(3L, shiny::uiOutput('XMin')),
            shiny::column(3L, shiny::uiOutput('XMax'))),

          # Select the X-Variable
          shiny::fluidRow(
            shiny::column(3L, shiny::uiOutput('DateVar')),
            shiny::column(3L, shiny::uiOutput('DateMin')),
            shiny::column(3L, shiny::uiOutput('DateMax')))))),

    # Add Space
    RemixAutoML::BlankRow(AppWidth),

    # Filter Variable Box
    shiny::fluidRow(
      shiny::column(
        width = AppWidth,
        shinyjs::useShinyjs(),
        shinydashboard::box(
          title = htmltools::tagList(shiny::icon('filter', lib = 'font-awesome'), 'Data Filtering'),
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,
          background = FilterBoxColor,
          width = AppWidth,

    # Select FilterVariable_1x
    shiny::fluidRow(
      shiny::column(3L, shiny::uiOutput('FilterVariable_1')),
      shiny::column(3L, shiny::uiOutput('FilterLogic_1')),
      shiny::column(3L, shiny::uiOutput('FilterValue_1a')),
      shiny::column(3L, shiny::uiOutput('FilterValue_1b'))),

    # Select FilterVariable_1x
    shiny::fluidRow(
      shiny::column(3L, shiny::uiOutput('FilterVariable_2')),
      shiny::column(3L, shiny::uiOutput('FilterLogic_2')),
      shiny::column(3L, shiny::uiOutput('FilterValue_2a')),
      shiny::column(3L, shiny::uiOutput('FilterValue_2b'))),

    # Select FilterVariable_1x
    shiny::fluidRow(
      shiny::column(3L, shiny::uiOutput('FilterVariable_3')),
      shiny::column(3L, shiny::uiOutput('FilterLogic_3')),
      shiny::column(3L, shiny::uiOutput('FilterValue_3a')),
      shiny::column(3L, shiny::uiOutput('FilterValue_3b'))),

    # Select FilterVariable_1x
    shiny::fluidRow(
      shiny::column(3L, shiny::uiOutput('FilterVariable_4')),
      shiny::column(3L, shiny::uiOutput('FilterLogic_4')),
      shiny::column(3L, shiny::uiOutput('FilterValue_4a')),
      shiny::column(3L, shiny::uiOutput('FilterValue_4b')))))),

    # Add Space
    RemixAutoML::BlankRow(AppWidth),

    # ----

    # ----

    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    # Plot Inputs ----
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    shiny::fluidRow(
      shiny::column(
        width = AppWidth,
        shinyjs::useShinyjs(),
        shinydashboard::box(
          title = tagList(shiny::icon("database", lib = "font-awesome"), "Plot Options"),
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,
          background = PlotBoxColor,
          width = AppWidth,

          # UI Plot Options
          # Select plot type
          shiny::fluidRow(
            shiny::column(3L, shiny::uiOutput('PlotType')),
            shiny::column(3L, shiny::uiOutput('NumberGroupsDisplay')),
            shiny::column(3L, shiny::uiOutput('GamFitScatter'))),
          shiny::fluidRow(
            shiny::column(3L, shiny::uiOutput('PlotWidth')),
            shiny::column(3L, shiny::uiOutput('PlotHeight')),
            shiny::column(3L, shiny::uiOutput('DateTicks'))),
          shiny::fluidRow(
            shiny::column(3L, shiny::uiOutput('AngleY')),
            shiny::column(3L, shiny::uiOutput('TextSize')),
            shiny::column(3L, shiny::uiOutput('TextColor')),
            shiny::column(3L, shiny::uiOutput('ChartColor'))),
          shiny::fluidRow(
            shiny::column(3L, shiny::uiOutput('AngleX')),
            shiny::column(3L, shiny::uiOutput('GridColor')),
            shiny::column(3L, shiny::uiOutput('BackGroundColor')),
            shiny::column(3L, shiny::uiOutput('BorderColor')))))),

    # Add Space
    RemixAutoML::BlankRow(AppWidth),

    # ----

    # ----

    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    # Buttons to build plot ----
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
    # Show Plot ----
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    shiny::fluidRow(shiny::column(width = AppWidth, shiny::plotOutput('Trend')))))

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# Server Function ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
server <- function(input, output, session) {

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Variables ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  # YVar and XVar
  output$YVar <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'YVar', Label = 'Y-Variable', Choices = names(data), SelectedDefault = YVariable, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$XVar <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'XVar', Label = 'X-Variable', Choices = names(data), SelectedDefault = XVariable, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$DateVar <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'DateVar', Label = 'Date Variable', Choices = names(data), SelectedDefault = DateName, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  # Reactives References
  YVar <- shiny::reactive({shiny::req(input[['YVar']])})
  XVar <- shiny::reactive({shiny::req(input[['XVar']])})
  DateVar <- shiny::reactive({shiny::req(input[['DateVar']])})

  # YMin
  output$YMin <- shiny::renderUI({
    Output <- KeyVarsInit(data, VarName = eval(YVar()))
    minn <- Output[['MinVal']]
    choices <- Output[['ChoiceInput']]
    RemixAutoML::PickerInput(
      InputID = 'YMin',
      Label = 'Min Y-Value',
      Choices = CharNull(choices),
      SelectedDefault = CharNull(minn),
      Multiple = FALSE)
  })

  # XMin
  output$XMin <- shiny::renderUI({
    Output <- KeyVarsInit(data, VarName = eval(XVar()))
    minnx <- Output[['MinVal']]
    choices <- Output[['ChoiceInput']]
    RemixAutoML::PickerInput(
      InputID = 'XMin',
      Label = 'Min X-Value',
      Choices = CharNull(choices),
      SelectedDefault = CharNull(minnx),
      Multiple = FALSE)
  })

  # YMax
  output$YMax <- shiny::renderUI({
    Output <- KeyVarsInit(data, VarName = eval(YVar()))
    maxxy <- Output[['MaxVal']]
    choices <- Output[['ChoiceInput']]
    if(Debug) {print(maxxy); print(data[[eval(YVar())]][1:5])}
    RemixAutoML::PickerInput(
      InputID = 'YMax',
      Label = 'Max Y-Value',
      Choices = CharNull(choices),
      SelectedDefault = CharNull(maxxy),
      Multiple = FALSE)
  })

  # XMax
  output$XMax <- shiny::renderUI({
    Output <- KeyVarsInit(data, VarName = eval(XVar()))
    maxxxx <- Output[['MaxVal']]
    choices <- Output[['ChoiceInput']]
    RemixAutoML::PickerInput(
      InputID = 'XMax',
      Label = 'Max X-Value',
      Choices = CharNull(choices),
      SelectedDefault = CharNull(maxxxx),
      Multiple = FALSE)
  })

  # DateMin
  output$DateMin <- shiny::renderUI({
    Output <- KeyVarsInit(data, VarName = eval(DateVar()))
    minnd <- Output[['MinVal']]
    choices <- Output[['ChoiceInput']]
    RemixAutoML::PickerInput(
      InputID = 'DateMin',
      Label = 'Min Date-Value',
      Choices = CharNull(choices),
      SelectedDefault = CharNull(minnd),
      Multiple = FALSE)
  })

  # DateMax
  output$DateMax <- shiny::renderUI({
    Output <- KeyVarsInit(data, VarName = eval(DateVar()))
    minnx <- Output[['MinVal']]
    maxxx <- Output[['MaxVal']]
    choices <- Output[['ChoiceInput']]
    RemixAutoML::PickerInput(
      InputID = 'DateMax',
      Label = 'Max Date-Value',
      Choices = CharNull(choices),
      SelectedDefault = CharNull(maxxx),
      Multiple = FALSE)
  })

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Plotting MetaData ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  # PlotType
  output$PlotType <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'PlotType', Label = 'Plot Type', Choices = c('BoxPlotTS','ViolinPlotTS','Line','Scatter','Copula'), SelectedDefault = 'box', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$NumberGroupsDisplay <- shiny::renderUI({
    RemixAutoML::NumericInput(InputID = 'NumberGroupsDisplay', Label = '# of Levels', Step = 1L, Value = 5L, Min = 1L, Max = 100L)
  })
  output$GamFitScatter <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'GamFitScatter', Label = 'Fit Gam on Scatterplot', Choices = c('TRUE', 'FALSE'), SelectedDefault = FALSE, Multiple = FALSE, ActionBox = TRUE)
  })

  # Faceting
  output$FacetRow <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'FacetRow', Label = 'Facet Row Variable', Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$FacetCol <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'FacetCol', Label = 'Facet Col Variable', Choices = c('None', names(data)), SelectedDefault = 'None', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  # UI Plot Options
  output$PlotWidth <- shiny::renderUI({
    RemixAutoML::NumericInput(InputID = "PlotWidth", Label = 'Plot Width', Step = 50, Min = 800, Max = 1800, Value = 1600)
  })
  output$PlotHeight <- shiny::renderUI({
    RemixAutoML::NumericInput(InputID = "PlotHeight", Label = 'Plot Height', Step = 25, Min = 350, Max = 350*10, Value = 500)
  })
  output$DateTicks <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'DateTicks', Label = 'Date ticks x-axis', Choices = c('1 year', '1 day', '3 day', '1 week', '2 week', '1 month', '3 month', '6 month', '2 year', '5 year', '10 year', '1 minute', '15 minutes', '30 minutes', '1 hour', '3 hour', '6 hour', '12 hour'), SelectedDefault = '1 year', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$AngleY <- shiny::renderUI({
    RemixAutoML::NumericInput(InputID = 'AngleY', Label = 'Y-axis text angle', Step = 5, Min = 0, Max = 360, Value = 0)
  })
  output$AngleX <- shiny::renderUI({
    RemixAutoML::NumericInput(InputID = 'AngleX', Label = 'X-axis text angle', Step = 5, Min = 0, Max = 360, Value = 90)
  })
  output$TextSize <- shiny::renderUI({
    RemixAutoML::NumericInput(InputID = 'TextSize', Label = 'Text size', Step = 1, Min = 1, Max = 50, Value = 12)
  })

  # Color boxes
  output$TextColor <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'TextColor', Label = 'Text color', Choices = grDevices::colors(), SelectedDefault = 'darkblue', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$ChartColor <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'ChartColor', Label = 'Chart color', Choices = grDevices::colors(), SelectedDefault = 'lightsteelblue1', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$GridColor <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'GridColor', Label = 'Grid lines color', Choices = grDevices::colors(), SelectedDefault = 'white', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$BackGroundColor <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'BackGroundColor', Label = 'Background color', Choices = grDevices::colors(), SelectedDefault = 'gray95', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$BorderColor <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'BorderColor', Label = 'Border color', Choices = grDevices::colors(), SelectedDefault = 'darkblue', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Group Variables ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  # Select GroupVars
  output$GroupVars <- shiny::renderUI({
    if(Debug) print('PickerInput GroupVars')
    RemixAutoML::PickerInput(
      InputID = 'GroupVars',
      Label = 'Select Group Variables',
      Choices = names(data),
      SelectedDefault = GroupVariables,
      SelectedText = 'count > 1',
      Multiple = TRUE,
      ActionBox = TRUE)
  })

  # Reactive Group Variables
  SelectedGroups <- shiny::reactive({
    RemixAutoML::ReturnParam(
      input,
      VarName = 'GroupVars',
      Default = GroupVariables,
      Switch = TRUE,
      Type = 'character')
  })

  # Levels
  output$Levels_1 <- shiny::renderUI({
    if(Debug) print('PickerInput_GetLevels 1')
    if(Debug) print(SelectedGroups())
    if(Debug) print('here')
    if(Debug) print(UniqueLevels(input, data, 1L, GroupVars=SelectedGroups()))
    if(Debug) print('here 1')
    RemixAutoML::PickerInput_GetLevels(
      input,
      data = 'data',
      NumGroupVar = 1L,
      InputID = 'Levels_1',
      InputID2 = 'GroupVars',
      Choices = UniqueLevels(input, data, 1L, GroupVars=SelectedGroups()),
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
      Choices = UniqueLevels(input, data, 2L, GroupVars=SelectedGroups()),
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
      Choices = UniqueLevels(input, data, 3L, GroupVars=SelectedGroups()),
      SelectedDefault = NULL,
      Size = 10,
      SelectedText = "count > 1",
      Multiple = TRUE,
      ActionBox = TRUE)
  })

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Filter Variables ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  # Filter Variable 1
  output$FilterVariable_1 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterVariable_1', label='Filter Variable', choices=c('None', names(data)), selected='None')
  })

  # Filter Variable 2
  output$FilterVariable_2 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterVariable_2', label='Filter Variable', choices=c('None', names(data)), selected='None')
  })

  # Filter Variable 3
  output$FilterVariable_3 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterVariable_3', label='Filter Variable', choices=c('None', names(data)), selected='None')
  })

  # Filter Variable 4
  output$FilterVariable_4 <- shiny::renderUI({
    shiny::selectInput(inputId='FilterVariable_4', label='Filter Variable', choices=c('None', names(data)), selected='None')
  })

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Filter Logic ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  # Filter Logic 1
  output$FilterLogic_1 <- shiny::renderUI({
    x <- class(data[[eval(input[['FilterVariable_1']])]])
    if(x %in% c('factor', 'character')) FL_Default <- '%chin%' else FL_Default <- '>='
    shiny::selectInput(inputId='FilterLogic_1', label='Logical Operation', choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%','NULL'), selected=FL_Default, multiple=FALSE)
  })

  # Filter Logic 2
  output$FilterLogic_2 <- shiny::renderUI({
    x <- class(data[[eval(input[['FilterVariable_2']])]])
    if(x %in% c('factor', 'character')) FL_Default <- '%chin%' else FL_Default <- '>='
    shiny::selectInput(inputId='FilterLogic_2', label='Logical Operation', choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%','NULL'), selected=FL_Default, multiple=FALSE)
  })

  # Filter Logic 3
  output$FilterLogic_3 <- shiny::renderUI({
    x <- class(data[[eval(input[['FilterVariable_3']])]])
    if(x %in% c('factor', 'character')) FL_Default <- '%chin%' else FL_Default <- '>='
    shiny::selectInput(inputId='FilterLogic_3', label='Logical Operation', choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%','NULL'), selected=FL_Default, multiple=FALSE)
  })

  # Filter Logic 4
  output$FilterLogic_4 <- shiny::renderUI({
    x <- class(data[[eval(input[['FilterVariable_4']])]])
    if(x %in% c('factor', 'character')) FL_Default <- '%chin%' else FL_Default <- '>='
    shiny::selectInput(inputId='FilterLogic_4', label='Logical Operation', choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%','NULL'), selected=FL_Default, multiple=FALSE)
  })

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Filter Values ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  # Filter Values 1a
  output$FilterValue_1a <- shiny::renderUI({
    params <- list(data=data, VarName = input[['FilterVariable_1']], type = 1)
    Mult <- do.call(GetFilterValueMultiple, params)
    Lab <- do.call(GetFilterValueLabel, params)
    FilterUnique <- do.call(FilterValues, params)
    RemixAutoML::PickerInput(InputID='FilterValue_1a', Label=Lab, Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=Mult, ActionBox=TRUE)
  })

  # Filter Values 1b
  output$FilterValue_1b <- shiny::renderUI({
    params <- list(data=data, VarName = input[['FilterVariable_1']], type = 2)
    Mult <- do.call(GetFilterValueMultiple, params)
    Lab <- do.call(GetFilterValueLabel, params)
    FilterUnique <- do.call(FilterValues, params)
    RemixAutoML::PickerInput(InputID='FilterValue_1b', Label=Lab, Choices=FilterUnique, SelectedDefault=FilterUnique[length(FilterUnique)], Multiple=Mult, ActionBox=TRUE)
  })

  # Filter Values 2a
  output$FilterValue_2a <- shiny::renderUI({
    params <- list(data=data, VarName = input[['FilterVariable_2']], type = 1)
    Mult <- do.call(GetFilterValueMultiple, params)
    Lab <- do.call(GetFilterValueLabel, params)
    FilterUnique <- do.call(FilterValues, params)
    RemixAutoML::PickerInput(InputID='FilterValue_2a', Label=Lab, Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=Mult, ActionBox=TRUE)
  })

  # Filter Values 2b
  output$FilterValue_2b <- shiny::renderUI({
    params <- list(data=data, VarName = input[['FilterVariable_2']], type = 2)
    Mult <- do.call(GetFilterValueMultiple, params)
    Lab <- do.call(GetFilterValueLabel, params)
    FilterUnique <- do.call(FilterValues, params)
    RemixAutoML::PickerInput(InputID='FilterValue_2b', Label=Lab, Choices=FilterUnique, SelectedDefault=FilterUnique[length(FilterUnique)], Multiple=Mult, ActionBox=TRUE)
  })

  # Filter Values 3a
  output$FilterValue_3a <- shiny::renderUI({
    params <- list(data=data, VarName = input[['FilterVariable_3']], type = 1)
    Mult <- do.call(GetFilterValueMultiple, params)
    Lab <- do.call(GetFilterValueLabel, params)
    FilterUnique <- do.call(FilterValues, params)
    RemixAutoML::PickerInput(InputID='FilterValue_3a', Label=Lab, Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=Mult, ActionBox=TRUE)
  })

  # Filter Values 3b
  output$FilterValue_3b <- shiny::renderUI({
    params <- list(data=data, VarName = input[['FilterVariable_3']], type = 2)
    Mult <- do.call(GetFilterValueMultiple, params)
    Lab <- do.call(GetFilterValueLabel, params)
    FilterUnique <- do.call(FilterValues, params)
    RemixAutoML::PickerInput(InputID='FilterValue_3b', Label=Lab, Choices=FilterUnique, SelectedDefault=FilterUnique[length(FilterUnique)], Multiple=Mult, ActionBox=TRUE)
  })

  # Filter Values 4a
  output$FilterValue_4a <- shiny::renderUI({
    params <- list(data=data, VarName = input[['FilterVariable_4']], type = 1)
    Mult <- do.call(GetFilterValueMultiple, params)
    Lab <- do.call(GetFilterValueLabel, params)
    FilterUnique <- do.call(FilterValues, params)
    RemixAutoML::PickerInput(InputID='FilterValue_4a', Label=Lab, Choices=FilterUnique, SelectedDefault=FilterUnique[1L], Multiple=Mult, ActionBox=TRUE)
  })

  # Filter Values 4b
  output$FilterValue_4b <- shiny::renderUI({
    params <- list(data=data, VarName = input[['FilterVariable_4']], type = 2)
    Mult <- do.call(GetFilterValueMultiple, params)
    Lab <- do.call(GetFilterValueLabel, params)
    FilterUnique <- do.call(FilterValues, params)
    RemixAutoML::PickerInput(InputID='FilterValue_4b', Label=Lab, Choices=FilterUnique, SelectedDefault=FilterUnique[length(FilterUnique)], Multiple=Mult, ActionBox=TRUE)
  })

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Reset Plot Format Only ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  shiny::observeEvent(eventExpr = input[['ResetPlotThemeElements']], {

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
    if(shiny::isolate(input[['PlotType']] %chin% c('BoxPlotTS','ViolinTS'))) {
      p1 <- p1 + ggplot2::scale_x_date(date_breaks = shiny::isolate(input[['DateTicks']]))
      p1 <- shiny::isolate(p1 + ggplot2::labs(
        title = paste0(shiny::isolate(input[['PlotType']]), ' Plot'),
        subtitle = 'Blue line = mean(Y)',
        caption = 'by RemixAutoML') +
          ggplot2::ylim(as.numeric(eval(input[['YMin']])), as.numeric(eval(input[['YMax']]))) +
          ggplot2::ylab(eval(YVar())) + ggplot2::xlab(DateVar()))
    } else if(shiny::isolate(input[['PlotType']] %chin% c('Line'))) {
      p1 <- p1 + ggplot2::scale_x_date(date_breaks = shiny::isolate(input[['DateTicks']]))
      p1 <- shiny::isolate(p1 + ggplot2::labs(
        title = paste0(shiny::isolate(input[['PlotType']]), ' Plot'),
        caption = 'by RemixAutoML') +
          ggplot2::ylim(as.numeric(eval(input[['YMin']])), as.numeric(eval(input[['YMax']]))) +
          ggplot2::ylab(eval(YVar())) + ggplot2::xlab(DateVar()))
    } else if(shiny::isolate(input[['PlotType']] %chin% c('Scatter','Copula'))) {
      p1 <- shiny::isolate(p1 + ggplot2::labs(
        title = paste0(shiny::isolate(input[['PlotType']]), ' Plot'),
        caption = 'by RemixAutoML') +
          ggplot2::ylim(as.numeric(eval(input[['YMin']])), as.numeric(eval(input[['YMax']]))) +
          ggplot2::ylab(eval(YVar())) + ggplot2::xlab(XVar()))
    }

    # UI Plot Options ----
    output$PlotWidth <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID = "PlotWidth", Label = 'Plot Width', Step = 50, Min = 800, Max = 1800, Value = 1600)
    })
    output$PlotHeight <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID = "PlotHeight", Label = 'Plot Height', Step = 25, Min = 350, Max = 350*10, Value = 500)
    })
    output$AngleY <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID = "AngleY", Label = "Y-axis text angle", Step = 5, Min = 0, Max = 360, Value = 0)
    })
    output$AngleX <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID = "AngleX", Label = "X-axis text angle", Step = 5, Min = 0, Max = 360, Value = 90)
    })
    output$TextSize <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID = "TextSize", Label = "Text size", Step = 1, Min = 1, Max = 50, Value = 12)
    })

    # Color boxes ----
    output$TextColor <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TextColor", Label = "Text color", Choices = grDevices::colors(), SelectedDefault = "darkblue", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$ChartColor <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "ChartColor", Label = "Chart color", Choices = grDevices::colors(), SelectedDefault = "lightsteelblue1", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$GridColor <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "GridColor", Label = "Grid lines color", Choices = grDevices::colors(), SelectedDefault = "white", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$BackGroundColor <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "BackGroundColor", Label = "Background color", Choices = grDevices::colors(), SelectedDefault = "gray95", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$BorderColor <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "BorderColor", Label = "Border color", Choices = grDevices::colors(), SelectedDefault = "darkblue", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    # Return
    p1 <<- p1
    output$Trend <- shiny::renderPlot(width = shiny::isolate(input[['PlotWidth']]), height = shiny::isolate(input[['PlotHeight']]), {
      p1
    })
  })

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Update Plot Format Only ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  shiny::observeEvent(eventExpr = input[['UpdatePlotThemeElements']], {

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
      p1 <- p1 + ggplot2::scale_x_date(date_breaks = shiny::isolate(input[['DateTicks']]))
      p1 <- shiny::isolate(p1 + ggplot2::labs(
        title = paste0(shiny::isolate(input[['PlotType']]), ' Plot'),
        subtitle = 'Blue line = mean(Y)',
        caption = 'by RemixAutoML') +
          ggplot2::ylim(as.numeric(eval(input[['YMin']])), as.numeric(eval(input[['YMax']]))) +
          ggplot2::ylab(eval(YVar())) + ggplot2::xlab(DateVar()))
    } else if(shiny::isolate(input[['PlotType']] %chin% c('Line'))) {
      p1 <- p1 + ggplot2::scale_x_date(date_breaks = shiny::isolate(input[['DateTicks']]))
      p1 <- shiny::isolate(p1 + ggplot2::labs(
        title = paste0(shiny::isolate(input[['PlotType']]), ' Plot'),
        caption = 'by RemixAutoML') +
          ggplot2::ylim(as.numeric(eval(input[['YMin']])), as.numeric(eval(input[['YMax']]))) +
          ggplot2::ylab(eval(YVar())) + ggplot2::xlab(DateVar()))
    } else if(shiny::isolate(input[['PlotType']] %chin% c('Scatter','Copula'))) {
      p1 <- shiny::isolate(p1 + ggplot2::labs(
        title = paste0(shiny::isolate(input[['PlotType']]), ' Plot'),
        caption = 'by RemixAutoML') +
          ggplot2::ylim(as.numeric(eval(input[['YMin']])), as.numeric(eval(input[['YMax']]))) +
          ggplot2::ylab(eval(YVar())) + ggplot2::xlab(XVar()))
    }

    # Return
    p1 <<- p1
    output$Trend <- shiny::renderPlot(width = shiny::isolate(input[['PlotWidth']]), height = shiny::isolate(input[['PlotHeight']]), {
      p1
    })
  })

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Create Plot ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  shiny::observeEvent(eventExpr = input[['TrendPlotExecute']], {

    # Remove NA's
    if(Debug) print('remove NA')
    data1 <- data[!is.na(get(shiny::isolate(YVar())))]

    # Filter by Date
    if(Debug) print('Filter by Date')
    if(!is.null(shiny::isolate(DateVar()))) {
      data1 <- data1[get(shiny::isolate(DateVar())) <= eval(input[['DateMax']]) & get(shiny::isolate(DateVar())) >= eval(input[['DateMin']])]
    }

    # Subset by FilterVariable_1
    if(Debug) print('Subset by FilterVariable_1')
    if(input[['FilterVariable_1']] != 'None') {
      fv <- input[['FilterValue_1b']]
      if(Debug) print(fv)
      if(Debug) print(input[['FilterLogic_1']])
      if(Debug) print(data1)
      data1 <- FilterLogicData(data1, input, FilterLogic=input[['FilterLogic_1']], FilterVariable=input[['FilterVariable_1']], FilterValue=input[['FilterValue_1a']], FilterValue2=fv, Debug = Debug)
      if(Debug) print(data1)
    }

    # Subset by FilterVariable_2
    if(Debug) print('Subset by FilterVariable_2')
    if(input[['FilterVariable_2']] != 'None') {
      fv <- input[['FilterValue_2b']]
      data1 <- FilterLogicData(data1, input, FilterLogic=input[['FilterLogic_2']], FilterVariable=input[['FilterVariable_2']], FilterValue=input[['FilterValue_2a']], FilterValue2=fv)
    }

    # Subset by FilterVariable_3
    if(Debug) print('Subset by FilterVariable_3')
    if(input[['FilterVariable_3']] != 'None') {
      fv <- input[['FilterValue_3b']]
      data1 <- FilterLogicData(data1, input, FilterLogic=input[['FilterLogic_3']], FilterVariable=input[['FilterVariable_3']], FilterValue=input[['FilterValue_3a']], FilterValue2=fv)
    }

    # Subset by FilterVariable_4
    if(Debug) print('Subset by FilterVariable_4')
    if(input[['FilterVariable_4']] != 'None') {
      fv <- input[['FilterValue_4b']]
      data1 <- FilterLogicData(data1, input, FilterLogic=input[['FilterLogic_4']], FilterVariable=input[['FilterVariable_4']], FilterValue=input[['FilterValue_4a']], FilterValue2=fv)
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
    if(Debug) print(data1)
    if(Debug) print(input[['PlotType']] %chin% c('BoxPlotTS','ViolinPlotTS','Scatter','Copula'))
    data1 <- RemixAutoML::PreparePlotData(
      SubsetOnly = if(input[['PlotType']] %chin% c('BoxPlotTS','ViolinPlotTS','Scatter','Copula')) TRUE else FALSE,
      input,
      PlotDataForecast = data1,
      Aggregate = 'mean',
      TargetVariable = shiny::isolate(YVar()),
      DateVariable = shiny::isolate(DateVar()),
      GroupVariables = x,
      G1Levels = 'Levels_1',
      G2Levels = 'Levels_2',
      G3Levels = 'Levels_3',
      Debug = Debug)

    # Render Plot
    if(Debug) print(data1)
    if(Debug) shiny::isolate(print(SelectedGroups()))

    # Create Plot Object
    if(shiny::isolate(input[['PlotType']] %chin% c('BoxPlotTS','ViolinPlotTS'))) {
      if(Debug) print('Create Plot Objects 1')
      p1 <- shiny::isolate(ggplot2::ggplot(data = data1, ggplot2::aes(x = get(shiny::isolate(DateVar())), y = get(shiny::isolate(YVar())), group = get(shiny::isolate(DateVar())))))
      if(shiny::isolate(input[['PlotType']] == 'BoxPlotTS')) {
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
          ggplot2::ylim(as.numeric(eval(shiny::isolate(input[['YMin']]))), as.numeric(eval(shiny::isolate(input[['YMax']])))) +
          ggplot2::ylab(eval(shiny::isolate(YVar()))) + ggplot2::xlab(shiny::isolate(DateVar())) +
          ggplot2::scale_x_date(date_breaks = input[['DateTicks']])

      # Add faceting (returns no faceting in none was requested)
      if(shiny::isolate(input[['FacetRow']]) != 'None' && shiny::isolate(input[['FacetCol']]) != 'None') {
        if(Debug) print('Create Plot Objects 6a')
        p1 <- p1 + ggplot2::facet_grid(get(shiny::isolate(input[['FacetRow']])) ~ get(shiny::isolate(input[['FacetCol']])))
      } else if(shiny::isolate(input[['FacetRow']]) == 'None' && shiny::isolate(input[['FacetCol']] != 'None')) {
        if(Debug) print('Create Plot Objects 6b')
        p1 <- p1 + ggplot2::facet_wrap(~ get(shiny::isolate(input[['FacetCol']])))
      } else if(shiny::isolate(input[['FacetRow']]) != 'None' && shiny::isolate(input[['FacetCol']]) == 'None') {
        if(Debug) print('Create Plot Objects 6c')
        p1 <- p1 + ggplot2::facet_wrap(~ get(shiny::isolate(input[['FacetRow']])))
      }
      p1 <<- p1

    } else if(shiny::isolate(input[['PlotType']] %chin% c('Line'))) {

      if(!shiny::isolate(class(data1[[eval(DateVar())]]) %chin% c('numeric','integer','factor','character','logical','integer64', 'NULL'))) {
        p1 <- RemixAutoML:::TimeSeriesPlotter(
          data = data1,
          TargetVariable = shiny::isolate(YVar()),
          DateVariable = shiny::isolate(DateVar()),
          GroupVariables = shiny::isolate(SelectedGroups()),
          Aggregate = 'mean',
          NumberGroupsDisplay = input[['NumberGroupsDisplay']],
          LevelsToDisplay = NULL,
          OtherGroupLabel = "OtherGroups",
          DisplayOtherGroup = TRUE,
          TextSize = input[['TextSize']],
          LineWidth = 0.5,
          Color = 'blue',
          XTickMarks = input[['DateTicks']],
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

    } else if(shiny::isolate(input[['PlotType']] %chin% c('Scatter','Copula'))) {

      # Ensure variables are numeric
      if(Debug) print(YVar())
      if(Debug) print(XVar())
      if(!any(c('numeric','integer') %chin% class(data1[[eval(YVar())]]))) {
        shinyWidgets::sendSweetAlert(session, title = NULL, text = "Y needs to be a numeric variable", type = NULL, btn_labels = "error", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
      } else if(!any(c('numeric','integer') %chin% class(data1[[eval(XVar())]]))) {
        shinyWidgets::sendSweetAlert(session, title = NULL, text = "X needs to be a numeric variable", type = NULL, btn_labels = "error", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
      } else {

        # Generate plot
        # Subset + Sample
        R2_Pearson <- c()
        R2_Spearman <- c()
        yyy <- eval(shiny::isolate(YVar()))
        xxx <- eval(shiny::isolate(XVar()))
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
        Output <- RemixAutoML::ScatterCopula(
          data = data1,
          x_var = xxx,
          y_var = yyy,
          FacetCol = if(shiny::isolate(input[['FacetCol']]) == 'None') NULL else shiny::isolate(input[['FacetCol']]),
          FacetRow = if(shiny::isolate(input[['FacetRow']]) == 'None') NULL else shiny::isolate(input[['FacetRow']]),
          GroupVariable = NULL,
          SampleCount = 100000L,
          FitGam = as.logical(shiny::isolate(input[['GamFitScatter']])))

        # Modify by plot type
        if(shiny::isolate(input[['PlotType']] %chin% c('Scatter'))) {
          p1 <- Output[["ScatterPlot"]]
          p1 <- p1 + ggplot2::labs(
            title = paste0('Scatter Plot'),
            subtitle = paste0("r-sq pearson xbar = ", round(mean(R2_Pearson),3L), " +/- ", round(sd(R2_Pearson) / sqrt(30L), 5L)," :: ",
                              "r-sq spearman xbar = ", round(mean(R2_Spearman),3L), " +/- ", round(sd(R2_Spearman) / sqrt(30L), 5L)))
          p1 <- shiny::isolate( p1 + RemixAutoML::ChartTheme(
            Size = input[['TextSize']],
            AngleX = input[['AngleX']],
            AngleY = input[['AngleY']],
            ChartColor = input[['ChartColor']],
            BorderColor = input[['BorderColor']],
            TextColor = input[['TextColor']],
            GridColor = input[['GridColor']],
            BackGroundColor = input[['BackGroundColor']]))
          p1 <- p1 + ggplot2::ylim(as.numeric(eval(shiny::isolate(input[['YMin']]))), as.numeric(eval(shiny::isolate(input[['YMax']]))))
          p1 <- p1 + ggplot2::xlim(as.numeric(eval(shiny::isolate(input[['XMin']]))), as.numeric(eval(shiny::isolate(input[['XMax']]))))

        } else if(shiny::isolate(input[['PlotType']] %chin% c('Copula'))) {

          p1 <- Output[["CopulaPlot"]]
          p1 <- p1 + ggplot2::labs(
            title = paste0('Empirical Copula Plot'),
            subtitle = paste0("r-sq pearson xbar = ", round(mean(R2_Pearson),3L), " +/- ", round(sd(R2_Pearson) / sqrt(30L), 5L)," :: ",
                              "r-sq spearman xbar = ", round(mean(R2_Spearman),3L), " +/- ", round(sd(R2_Spearman) / sqrt(30L), 5L)))
          p1 <- shiny::isolate( p1 + RemixAutoML::ChartTheme(
            Size = input[['TextSize']],
            AngleX = input[['AngleX']],
            AngleY = input[['AngleY']],
            ChartColor = input[['ChartColor']],
            BorderColor = input[['BorderColor']],
            TextColor = input[['TextColor']],
            GridColor = input[['GridColor']],
            BackGroundColor = input[['BackGroundColor']]))
        }
        p1 <<- p1
      }
    }

    # Return Plot to UI
    output$Trend <- shiny::renderPlot(width = shiny::isolate(input[['PlotWidth']]), height = shiny::isolate(input[['PlotHeight']]), {
      if(Debug) print('Create Plot output$Trend')
      p1
    })
  })

  # ----

  # ----

}

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# Run the application ----
shinyApp(ui = ui, server = server)

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
