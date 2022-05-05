# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Modals
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Box Plot Modals

#' @param PlotNumber Plot Selection
#' @param AppWidth App Width
#'
#' @noRd
StandardPlotsModal1 <- function(PlotNumber, AppWidth=12L) {
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
}

#' @param PlotNumber Plot Selection
#' @param AppWidth App Width
#'
#' @noRd
StandardPlotsModal2 <- function(PlotNumber, AppWidth=12L) {
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
