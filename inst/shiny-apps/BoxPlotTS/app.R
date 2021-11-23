#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Local function
UniqueLevels <- function(input, data, n, GroupVariables) {
  tryCatch({
    c(unique(data[[eval(input[['GroupVars']][[n]])]]))}, error = function(x) {
      tryCatch({
        c(unique(data[[eval(GroupVariables[[n]])]]))}, error = function(x) NULL)})
}

# Turn up horsepower
data.table::setDTthreads(threads = max(1L, parallel::detectCores()-1L))
data <- shiny::getShinyOption('data')
FeatureNames <- shiny::getShinyOption('FeatureNames')
GroupVariables <- shiny::getShinyOption('GroupVariables')
FilterVariable <- shiny::getShinyOption('FilterVariable')
DateName <- shiny::getShinyOption('DateName')
DateBreaks <- shiny::getShinyOption('DateBreaks')
DateLabels <- shiny::getShinyOption('DateLabels')
Title <- shiny::getShinyOption('Title')
SubTitle <- shiny::getShinyOption('SubTitle')
Debug <- shiny::getShinyOption('Debug')

# Define UI for application that draws a histogram
# Create ui ----
ui <- shiny::fluidPage(

  # Add Space
  RemixAutoML::BlankRow(12L),

  # Add image
  shiny::fluidRow(shiny::img(src = 'NewPackageLogo.png', width = "1500px", height = "459px")),

  # Add Space
  RemixAutoML::BlankRow(12L),

  # GroupVar_1 selection and Level selection
  shiny::fluidRow(

    # Box to house Group Variables Selection
    shiny::column(
      width = 12,
      shinyjs::useShinyjs(),
      shinydashboard::box(
        title = htmltools::tagList(shiny::icon("filter", lib = "font-awesome"), "Select Levels"),
        solidHeader = TRUE,
        collapsible = FALSE,
        background = "blue",
        width = 12,

        # Select GroupVariables
        shiny::column(width = 12L, shiny::fluidRow(shiny::column(3L, shiny::uiOutput(outputId = 'GroupVars')))),

        # Add Space
        RemixAutoML::BlankRow(12L),

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
  RemixAutoML::BlankRow(12L),

  # Show Plot
  shiny::fluidRow(
    shiny::column(
      width = 12,
      shinyjs::useShinyjs(),
      shinydashboard::box(
        title = htmltools::tagList(shiny::icon("filter", lib = "font-awesome"), "Distribution over time"),
        solidHeader = TRUE,
        collapsible = FALSE,
        background = "blue",
        width = 12L,
        shiny::fluidRow(shiny::column(width = 10L, shiny::plotOutput('Trend')))))),

  # Add Space
  RemixAutoML::BlankRow(12L),

  # Button to build plot
  shiny::fluidRow(
    shiny::column(
      width = 4,
      shinyjs::useShinyjs(),
      shinyWidgets::actionBttn(
        inputId = 'TrendPlotExecute',
        label = 'Create Plot!',
        icon = shiny::icon('chevron-right', lib = 'font-awesome'),
        style = 'gradient',
        color = 'royal'))),

  # Add Space
  RemixAutoML::BlankRow(12L),

  # Inputs for User ----
  shiny::column(
    width = 12,
    shinyjs::useShinyjs(),
    shinydashboard::box(
      title = htmltools::tagList(shiny::icon("filter", lib = "font-awesome"), "Select Plot Variable"),
      solidHeader = TRUE,
      collapsible = FALSE,
      background = "blue",
      width = 12,

      shiny:: fluidRow(

        # Slect the Target Variable ----
        shiny::column(
          3L,
          shiny::selectInput('YVar', 'Y_Variable', FeatureNames, selected = FeatureNames[1L])),

        # Select a Scale Factor for reducing the max-value of the y-axis
        shiny::column(
          3L,
          shiny::uiOutput('YMax')),
        shiny::column(
          3L,
          shiny::uiOutput('YMin'))),

      # Add Space
      RemixAutoML::BlankRow(12L),

      # Filters
      shiny::fluidRow(

        # Select FilterVariable ----
        shiny::column(
          3L,
          shiny::uiOutput(outputId = 'FilterVariable')),

        # Select a Scale Factor for reducing the max-value of the Y-Axis
        shiny::column(
          3L,
          shiny::uiOutput(outputId = 'FilterValue')),

        # GroupVar3 level selection
        shiny::column(
          width = 3L,
          shiny::uiOutput('FilterLogic'))))))



# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # Global
  YVar <- shiny::reactive({shiny::req(input[['YVar']])})

  # YMin
  output$YMin <- shiny::renderUI({
    minn <- floor(data[, min(get(eval(YVar())))])
    maxx <- ceiling(data[, max(get(eval(YVar())))])
    shiny::selectInput(
      inputId = 'YMin',
      label = 'Min Y-Value',
      choices = unique(as.character(c(minn, seq(minn, maxx, 1+floor((maxx-minn)/20)), maxx))),
      selected = as.character(minn))
  })

  # YMax
  output$YMax <- shiny::renderUI({
    minn <- floor(data[, min(get(eval(YVar())))])
    maxx <- ceiling(data[, max(get(eval(YVar())))])
    print(maxx)
    shiny::selectInput(
      inputId = 'YMax',
      label = 'Max Y-Value',
      choices = unique(as.character(c(minn, seq(minn, maxx, 1+floor((maxx-minn)/20)), maxx))),
      selected = as.character(maxx))
  })

  # Filter Variable
  output$FilterVariable <- shiny::renderUI({
    shiny::selectInput(
      inputId = 'FilterVariable',
      label = 'Filter Variable',
      choices = names(data),
      selected = FilterVariable)
  })

  # Select Filter Logic
  output$FilterLogic <- shiny::renderUI({
    x <- class(data[[eval(input$FilterVariable)]])
    if(x %in% c('factor', 'character')) {
      FL_Default <- '%chin%'
    } else {
      FL_Default <- '>='
    }
    shiny::selectInput(
      inputId = 'FilterLogic',
      label = 'Logical Operation', choices = c('<','>','<=','>=','%in%','%like%','NULL'), selected = FL_Default, multiple = FALSE)
  })

  # Filter Values
  output$FilterValue <- shiny::renderUI({

    # Get Choices argument for PickerInput
    if(tolower(class(data[[eval(input[['FilterVariable']])]]) %chin% c('numeric', 'integer'))) {
      FilterUnique <- sort(data[, quantile(get(input[['FilterVariable']]), probs = c(seq(0, 1, 0.05)))])
    } else if(tolower(class(data[[eval(input[['FilterVariable']])]])) %chin% c('factor', 'character')) {
      FilterUnique <- sort(data[, unique(get(input[['FilterVariable']]))])
    } else {
      FilterUnique <- NULL
    }

    # picker
    RemixAutoML::PickerInput(
      InputID = 'FilterValue',
      Label = 'Subset data using FilterVariable',
      Choices = FilterUnique,
      SelectedDefault = FilterUnique[1L],
      Multiple = FALSE,
      ActionBox = TRUE)
  })

  # Metadata
  SelectedGroups <- shiny::reactive({
    RemixAutoML::ReturnParam(
      input,
      VarName = 'GroupVars',
      Default = GroupVariables,
      Switch = TRUE,
      Type = 'character')
  })

  # Select GroupVars
  output$GroupVars <- shiny::renderUI({

    # Debug
    if(Debug) print('PickerInput GroupVars')
    RemixAutoML::PickerInput(
      InputID = 'GroupVars',
      Label = 'Select Group Variables',
      Choices = SelectedGroups(),
      SelectedDefault = SelectedGroups(),
      SelectedText = 'count > 1',
      Multiple = TRUE,
      ActionBox = TRUE)
  })

  # Levels
  output$Levels_1 <- shiny::renderUI({

    # Debug
    if(Debug) print('PickerInput_GetLevels 1')
    RemixAutoML::PickerInput_GetLevels(
      input,
      data = 'data',
      NumGroupVar = 1L,
      InputID = 'Levels_1',
      InputID2 = 'GroupVars',
      Choices = UniqueLevels(input, data, 1L, SelectedGroups()),
      SelectedDefault = UniqueLevels(input, data, 1L, SelectedGroups()),
      Size = 10,
      SelectedText = 'count > 1',
      Multiple = TRUE,
      ActionBox = TRUE)
  })

  # Levels
  output$Levels_2 <- shiny::renderUI({

    # Debug
    if(Debug) print('PickerInput_GetLevels 2')
    RemixAutoML::PickerInput_GetLevels(
      input,
      data = 'data',
      NumGroupVar = 2L,
      InputID = 'Levels_2',
      InputID2 = 'GroupVars',
      Choices = UniqueLevels(input, data, 2L, SelectedGroups()),
      SelectedDefault = UniqueLevels(input, data, 2L, SelectedGroups()),
      Size = 10,
      SelectedText = 'count > 1',
      Multiple = TRUE,
      ActionBox = TRUE)
  })

  # Levels
  output$Levels_3 <- shiny::renderUI({

    # Debug
    if(Debug) print('PickerInput_GetLevels 3')
    RemixAutoML::PickerInput_GetLevels(
      input,
      data = 'data',
      NumGroupVar = 3L,
      InputID = 'Levels_3',
      InputID2 = 'GroupVars',
      Choices = UniqueLevels(input, data, 3L, SelectedGroups()),
      SelectedDefault = UniqueLevels(input, data, 3L, SelectedGroups()),
      Size = 10,
      SelectedText = "count > 1",
      Multiple = TRUE,
      ActionBox = TRUE)
  })

  # Generate Plot
  shiny::observeEvent(eventExpr = input$TrendPlotExecute, {

    # Render Plot
    output$Trend <- shiny::renderPlot({

      # Remove NA's
      if(Debug) print('remove NA')
      data1 <- data[!is.na(get(YVar()))]

      # Subset Rows based on Filters
      if(Debug) print('Subset Rows based on Filters')
      data1 <- RemixAutoML::PreparePlotData(
        SubsetOnly = TRUE,
        input,
        PlotDataForecast = data1,
        Aggregate = 'mean',
        TargetVariable = YVar(),
        DateVariable = DateName,
        GroupVariables = SelectedGroups(),
        G1Levels = 'Levels_1',
        G2Levels = 'Levels_2',
        G3Levels = 'Levels_3')

      # Subset by FilterVariable
      if(Debug) print('Subset by FilterVariable')
      if(!is.null(FilterVariable)) {
        if(tolower(class(data1[[eval(FilterVariable)]])) %chin% c('factor', 'character')) {
          if(input$FilterLogic == '%in%') {
            data1 <- data1[get(FilterVariable) %chin% c(eval(input[['FilterValue']]))]
          } else if(input$FilterLogic == '%like%') {
            data1 <- data1[get(FilterVariable) %like% c(eval(input[['FilterValue']]))]
          }
        } else if(tolower(class(data1[[eval(FilterVariable)]])) %chin% c('numeric', 'integer', 'date', 'posix')) {
          if(input$FilterLogic == '>') {
            data1 <- data1[get(FilterVariable) > eval(input[['FilterValue']])]
          } else if(input$FilterLogic == '>=') {
            data1 <- data1[get(FilterVariable) >= eval(input[['FilterValue']])]
          } else if(input$FilterLogic == '<') {
            data1 <- data1[get(FilterVariable) < eval(input[['FilterValue']])]
          } else {
            data1 <- data1[get(FilterVariable) <= eval(input[['FilterValue']])]
          }
        }
      }

      # Create Plot
      if(Debug) print('Create Plot')
      suppressMessages(
        ggplot2::ggplot(
          data = data1,
          ggplot2::aes(x = get(DateName), y = get(YVar()), group = get(DateName))) +
          ggplot2::geom_boxplot(outlier.size = 0.1, outlier.colour = 'blue', fill = 'gray') +
          ggplot2::geom_hline(color = 'red', yintercept = eval(mean(data1[[eval(YVar())]], na.rm = TRUE))) +
          RemixAutoML::ChartTheme(AngleX = 90) +
          ggplot2::labs(
            title = Title,
            subtitle = SubTitle,
            caption = 'by RemixAutoML') +
          ggplot2::ylim(as.numeric(eval(input[['YMin']])), as.numeric(eval(input[['YMax']]))) +
          ggplot2::ylab(eval(YVar())) + ggplot2::xlab(DateName) +
          ggplot2::scale_x_date(date_breaks = DateBreaks, date_labels = DateLabels))
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
