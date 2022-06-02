#' @title StandardPlotsModal1
#'
#' @description Plot Variables Modals
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param id = 'PlotVariablesModals'
#' @param PlotNumber 1
#' @param AppWidth = 12L
#'
#' @export
StandardPlotsModal1 <- function(id, PlotNumber, AppWidth=12L) {
  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns
      shiny::showModal(
        shiny::modalDialog(
          title = 'Plot Inputs',
          size = "l",
          easyClose = TRUE,
          fade = TRUE,
          shiny::tagList(
            shinydashboard::box(
              title = NULL, solidHeader = TRUE, collapsible = FALSE, status = 'warning', width = AppWidth,
              shiny::fluidRow(
                shiny::column(width = 4L, align = 'center', shiny::uiOutput(paste0('YVar', PlotNumber))),
                shiny::column(width = 4L, align = 'center', shiny::uiOutput(paste0('XVar', PlotNumber))),
                shiny::column(width = 4L, align = 'center', shiny::uiOutput(paste0('SampleSize', PlotNumber))))),
            RemixAutoML:::BlankRow(AppWidth),
            shinydashboard::box(
              title = NULL, solidHeader = TRUE, collapsible = FALSE, status = 'warning', width = AppWidth,
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                shiny::column(width = 6L, align = 'center', shiny::uiOutput(paste0('GroupVars', PlotNumber))),
                shiny::column(width = 6L, align = 'center', shiny::uiOutput(paste0('FacetVar_', PlotNumber, '_1')))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                shiny::column(width = 4L, align = 'center', shiny::conditionalPanel(condition = paste0("length(input['GroupVars", PlotNumber, "']) >= 1"), shiny::uiOutput(paste0('Levels_', PlotNumber, '_1')))),
                shiny::column(width = 4L, align = 'center', shiny::conditionalPanel(condition = paste0("length(input['GroupVars", PlotNumber, "']) >= 2"), shiny::uiOutput(paste0('Levels_', PlotNumber, '_2')))),
                shiny::column(width = 4L, align = 'center', shiny::conditionalPanel(condition = paste0("length(input['GroupVars", PlotNumber, "']) >= 3"), shiny::uiOutput(paste0('Levels_', PlotNumber, '_3'))))))),
          footer = shiny::tagList(
            shiny::modalButton(label = "Cancel"),
            shiny::actionButton(paste0("BoxPlotOK", PlotNumber), "OK", width = '50px'))
        )
      )
    })
}

#' @title StandardPlotsModal2
#'
#' @description Plot Variables Modals
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param id = 'PlotVariablesModals'
#' @param PlotNumber 1
#' @param AppWidth = 12L
#'
#' @export
StandardPlotsModal2 <- function(id, PlotNumber, AppWidth=12L) {
  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns
      shiny::showModal(
        shiny::modalDialog(
          title = 'Plot Inputs',
          size = "l",
          easyClose = TRUE,
          fade = TRUE,
          list(
            shinydashboard::box(
              title = NULL, solidHeader = TRUE, collapsible = FALSE, status = 'warning', width = AppWidth,
              shiny::fluidRow(
                width=AppWidth,
                shiny::column(4L, align = 'center', shiny::uiOutput(paste0('YVar', PlotNumber))),
                shiny::column(4L, align = 'center', shiny::uiOutput(paste0('XVar', PlotNumber))),
                shiny::column(4L, align = 'center', shiny::uiOutput(paste0('ZVar', PlotNumber)))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                shiny::column(4L, align = 'center', shiny::uiOutput(paste0('SampleSize', PlotNumber))),
                shiny::column(4L, align = 'center', shiny::uiOutput(paste0('BarPlotAgg', PlotNumber))),
                shiny::column(4L, align = 'center', shiny::uiOutput(paste0('TargetLevel', PlotNumber))))),
            RemixAutoML:::BlankRow(AppWidth),
            shinydashboard::box(
              title = NULL, solidHeader = TRUE, collapsible = FALSE, status = 'warning', width = AppWidth,
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                shiny::column(width = 6L, align = 'center', shiny::uiOutput(paste0('GroupVars', PlotNumber))),
                shiny::column(width = 6L, align = 'center', shiny::uiOutput(paste0('FacetVar_', PlotNumber, '_1')))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                shiny::column(width = 4L, align = 'center', shiny::conditionalPanel(condition = paste0("length(input['GroupVars", PlotNumber, "']) >= 1"), shiny::uiOutput(paste0('Levels_', PlotNumber, '_1')))),
                shiny::column(width = 4L, align = 'center', shiny::conditionalPanel(condition = paste0("length(input['GroupVars", PlotNumber, "']) >= 2"), shiny::uiOutput(paste0('Levels_', PlotNumber, '_2')))),
                shiny::column(width = 4L, align = 'center', shiny::conditionalPanel(condition = paste0("length(input['GroupVars", PlotNumber, "']) >= 3"), shiny::uiOutput(paste0('Levels_', PlotNumber, '_3'))))))),
          footer = shiny::tagList(
            shiny::modalButton(label = "Cancel"),
            shiny::actionButton(paste0("BoxPlotOK", PlotNumber), "OK", width = '50px'))
        )
      )
    }
  )}


#' @title StandardPlotsModal3
#'
#' @description Plot Variables Modals
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param id = 'PlotVariablesModals'
#' @param PlotNumber 1
#' @param AppWidth = 12L
#'
#' @export
StandardPlotsModal3 <- function(id, PlotNumber, AppWidth=12L) {
  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns
      shiny::showModal(
        shiny::modalDialog(
          title = 'Plot Inputs',
          size = "l",
          easyClose = TRUE,
          fade = TRUE,
          list(
            shinydashboard::box(
              title = NULL, solidHeader = TRUE, collapsible = FALSE, status = 'warning', width = AppWidth,
              shiny::fluidRow(
                width=AppWidth,
                shiny::column(8L, align = 'center', shiny::uiOutput(paste0('Companies', PlotNumber)))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                shiny::column(
                  8L,
                  align = 'center',
                  shiny::uiOutput(paste0('StockDateRange', PlotNumber)))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                shiny::column(
                  4L,
                  align = 'center',
                  shiny::uiOutput(paste0('Symbols', PlotNumber))),
                shiny::column(
                  4L,
                  align = 'center',
                  shiny::uiOutput(paste0('StockMetric', PlotNumber))),
                shiny::column(
                  4L,
                  align = 'center',
                  shiny::uiOutput(paste0('StockTimeAgg', PlotNumber))))),
            RemixAutoML:::BlankRow(AppWidth)),
          footer = shiny::tagList(
            shiny::modalButton(label = "Cancel"),
            shiny::actionButton(paste0("BoxPlotOK", PlotNumber), "OK", width = '50px'))
        )
      )
    }
  )}

#' @title StandardPlotsModal4
#'
#' @description Plot Variables Modals
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param id = 'PlotVariablesModals'
#' @param PlotNumber 1
#' @param AppWidth = 12L
#'
#' @export
StandardPlotsModal4 <- function(id, PlotNumber, AppWidth=12L) {
  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns
      shiny::showModal(
        shiny::modalDialog(
          title = 'Plot Inputs',
          size = "l",
          easyClose = TRUE,
          fade = TRUE,
          shiny::tagList(
            shinydashboard::box(
              title = NULL, solidHeader = TRUE, collapsible = FALSE, status = 'warning', width = AppWidth,
              shiny::fluidRow(
                shiny::column(width = 4L, align = 'center', shiny::uiOutput(paste0('YVar', PlotNumber))),
                shiny::column(width = 4L, align = 'center', shiny::uiOutput(paste0('XVar', PlotNumber))),
                shiny::column(width = 4L, align = 'center', shiny::uiOutput(paste0('TargetLevel', PlotNumber)))
                )),
            RemixAutoML:::BlankRow(AppWidth),
            shinydashboard::box(
              title = NULL, solidHeader = TRUE, collapsible = FALSE, status = 'warning', width = AppWidth,
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                shiny::column(width = 6L, align = 'center', shiny::uiOutput(paste0('SampleSize', PlotNumber))),
                shiny::column(width = 6L, align = 'center', shiny::uiOutput(paste0('GroupVars', PlotNumber))),
                shiny::column(width = 6L, align = 'center', shiny::uiOutput(paste0('FacetVar_', PlotNumber, '_1')))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                shiny::column(width = 4L, align = 'center', shiny::conditionalPanel(condition = paste0("length(input['GroupVars", PlotNumber, "']) >= 1"), shiny::uiOutput(paste0('Levels_', PlotNumber, '_1')))),
                shiny::column(width = 4L, align = 'center', shiny::conditionalPanel(condition = paste0("length(input['GroupVars", PlotNumber, "']) >= 2"), shiny::uiOutput(paste0('Levels_', PlotNumber, '_2')))),
                shiny::column(width = 4L, align = 'center', shiny::conditionalPanel(condition = paste0("length(input['GroupVars", PlotNumber, "']) >= 3"), shiny::uiOutput(paste0('Levels_', PlotNumber, '_3'))))))),
          footer = shiny::tagList(
            shiny::modalButton(label = "Cancel"),
            shiny::actionButton(paste0("BoxPlotOK", PlotNumber), "OK", width = '50px'))
        )
      )
    })
}

#' @title BoxPlotModals
#'
#' @description Plot Variables Modals
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param id = 'PlotVariablesModals'
#' @param PlotNumber 1
#' @param AppWidth = 12L
#'
#' @export
BoxPlotModals <- function(id, NumberPlots, AppWidth=12L) {
  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns
      for(i in seq_len(NumberPlots)) {
        shiny::observeEvent(input[[paste0('BoxPlot_MenuButton', i)]], {
          assign(x = 'PlotDropDown', value = get('PlotDropDown'))
          output[[paste0('Plot', i)]] <- shiny::renderUI({RemixAutoML:::SelectizeInput(InputID = ns(paste0('Plot', i)), Label = tags$span(style=paste0('color: ', AppTextColor, ';'), 'Plot Selection'), Choices = c(AvailablePlots), SelectedDefault = 'BoxPlot', Multiple = TRUE, MaxVars = 1, CloseAfterSelect = TRUE, Debug = Debug)})
          PlotDropDown[[paste0('Plot', i)]][['SelectedDefault']][[length(PlotDropDown[[paste0('Plot', i)]][['SelectedDefault']]) + 1L]] <- input[[paste0('Plot', i)]]; PlotDropDown <<- PlotDropDown
          RemixAutoML:::StandardPlotsModal1(id = 'SPM1', PlotNumber = i)
        })
      }
    })
}
