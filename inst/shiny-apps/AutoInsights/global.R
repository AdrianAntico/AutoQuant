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


# BoxPlotModal <- function(input,AppTextColor) {
#
#   # Show modal when button is clicked.
#   shiny::observeEvent(input$PartialDependenceLine_MenuButton1, {
#     output$Plot1 <- shiny::renderUI({RemixAutoML:::SelectizeInput(InputID = 'Plot1', Label = tags$span(style=paste0('color: ', AppTextColor, ';'), 'Plot Selection'), Choices = c(AvailablePlots), SelectedDefault = 'PartialDependenceLine', Multiple = TRUE, MaxVars = 1, CloseAfterSelect = TRUE, Debug = Debug)})
#     PlotDropDown1[['Plot1']][['SelectedDefault']][[length(PlotDropDown1[['Plot1']][['SelectedDefault']]) + 1L]] <- input$Plot1;PlotDropDown1 <<- PlotDropDown1
#     StandardPlotsModal1(PlotNumber = 1L)
#   })
#
#   # Show modal when button is clicked.
#   shiny::observeEvent(input$PartialDependenceLine_MenuButton2, {
#     output$Plot2 <- shiny::renderUI({RemixAutoML:::SelectizeInput(InputID = 'Plot2', Label = tags$span(style=paste0('color: ', AppTextColor, ';'), 'Plot Selection'), Choices = c(AvailablePlots), SelectedDefault = 'PartialDependenceLine', Multiple = TRUE, MaxVars = 1, CloseAfterSelect = TRUE, Debug = Debug)})
#     PlotDropDown2[['Plot2']][['SelectedDefault']][[length(PlotDropDown2[['Plot2']][['SelectedDefault']]) + 1L]] <- input$Plot2; PlotDropDown2 <<- PlotDropDown2
#     StandardPlotsModal1(PlotNumber = 2L)
#   })
#
#   # Show modal when button is clicked.
#   shiny::observeEvent(input$PartialDependenceLine_MenuButton3, {
#     output$Plot3 <- shiny::renderUI({RemixAutoML:::SelectizeInput(InputID = 'Plot3', Label = tags$span(style=paste0('color: ', AppTextColor, ';'), 'Plot Selection'), Choices = c(AvailablePlots), SelectedDefault = 'PartialDependenceLine', Multiple = TRUE, MaxVars = 1, CloseAfterSelect = TRUE, Debug = Debug)})
#     PlotDropDown3[['Plot3']][['SelectedDefault']][[length(PlotDropDown3[['Plot3']][['SelectedDefault']]) + 1L]] <- input$Plot3; PlotDropDown3 <<- PlotDropDown3
#     StandardPlotsModal1(PlotNumber = 3L)
#   })
#
#   # Show modal when button is clicked.
#   shiny::observeEvent(input$PartialDependenceLine_MenuButton4, {
#     output$Plot4 <- shiny::renderUI({RemixAutoML:::SelectizeInput(InputID = 'Plot4', Label = tags$span(style=paste0('color: ', AppTextColor, ';'), 'Plot Selection'), Choices = c(AvailablePlots), SelectedDefault = 'PartialDependenceLine', Multiple = TRUE, MaxVars = 1, CloseAfterSelect = TRUE, Debug = Debug)})
#     PlotDropDown4[['Plot4']][['SelectedDefault']][[length(PlotDropDown4[['Plot4']][['SelectedDefault']]) + 1L]] <- input$Plot4;PlotDropDown4 <<- PlotDropDown4
#     StandardPlotsModal1(PlotNumber = 1L)
#   })
# }

