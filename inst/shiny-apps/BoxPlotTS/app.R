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
AppWidth <- shiny::getShinyOption('AppWidth')
Debug <- shiny::getShinyOption('Debug')

# Define UI for application that draws a histogram
# Create ui ----
#ui <- shiny::fluidPage(

ui <- shinydashboard::dashboardPage(skin = 'purple',

  # Page layout
  shinydashboard::dashboardHeader(title="Distribution over Time"),
  shinydashboard::dashboardSidebar(),
  shinydashboard::dashboardBody(

    # Custom css ----
    htmltools::tags$head(htmltools::tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
    htmltools::tags$head(htmltools::tags$style('h1 {color:darkblue;}')),
    htmltools::tags$head(htmltools::tags$style('body {color:darkblue;}')),
    htmltools::tags$head(htmltools::tags$style('box-solid {color:black;}')),

    # Add Space
    RemixAutoML::BlankRow(AppWidth),

    # Add image
    shiny::fluidRow(shiny::img(src = 'NewPackageLogo.png', width = "1100px", height = "278px")),

    # Add Space
    RemixAutoML::BlankRow(AppWidth),

    # GroupVar_1 selection and Level selection
    shiny::fluidRow(

      # Box to house Group Variables Selection
      shiny::column(
        width = AppWidth,
        shinyjs::useShinyjs(),
        shinydashboard::box(
          title = htmltools::tagList(shiny::icon("filter", lib = "font-awesome"), "Select Levels"),
          solidHeader = TRUE,
          collapsible = FALSE,
          background = "aqua",
          width = AppWidth,

          # Select GroupVariables
          shiny::column(AppWidth, shiny::fluidRow(shiny::column(4L, shiny::uiOutput(outputId = 'GroupVars')))),

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

    # Show Plot
    shiny::fluidRow(
      shiny::column(AppWidth, shiny::fluidRow(shiny::column(width = AppWidth, shiny::plotOutput('Trend'))))),

    # Add Space
    RemixAutoML::BlankRow(AppWidth),

    # Button to build plot
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
          color = 'royal')),

      # Update Theme!
      shiny::column(
        width = 3L,
        shinyjs::useShinyjs(),
        shinyWidgets::actionBttn(
          inputId = 'UpdatePlotThemeElements',
          label = 'Update Theme!',
          icon = shiny::icon('chevron-right', lib = 'font-awesome'),
          style = 'gradient',
          color = 'success'))),

    # Add Space
    RemixAutoML::BlankRow(AppWidth),

    # Inputs for User ----
    shiny::fluidRow(
      shiny::column(
        width = AppWidth,
        shinyjs::useShinyjs(),
        shinydashboard::box(
          title = htmltools::tagList(shiny::icon("filter", lib = "font-awesome"), "Select Plot Variable"),
          solidHeader = TRUE,
          collapsible = FALSE,
          background = "aqua",
          width = AppWidth,

          # Select plot type ----
          shiny::column(3L, shiny::uiOutput('PlotType')),
          shiny::column(3L, shiny::uiOutput('NumberGroupsDisplay')),

          # Add Space
          RemixAutoML::BlankRow(AppWidth),

          # Select the Target Variable ----
          shiny::column(3L, shiny::selectInput('YVar', 'Y_Variable', FeatureNames, selected = FeatureNames[1L])),
          shiny::column(3L, shiny::uiOutput('YMin')),
          shiny::column(3L, shiny::uiOutput('YMax')),

          # Add Space
          RemixAutoML::BlankRow(AppWidth),

          # Select FilterVariable ----
          shiny::column(3L, shiny::uiOutput('FilterVariable')),
          shiny::column(3L, shiny::uiOutput('FilterValue')),
          shiny::column(3L, shiny::uiOutput('FilterLogic'))))),

    # Optional Plot Inputs----
    shiny::fluidRow(
      shiny::column(
        width = AppWidth,
        shinyjs::useShinyjs(),
        shinydashboard::box(
          title = tagList(shiny::icon("database", lib = "font-awesome"), "Plot Options"),
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,
          background = "aqua",
          width = AppWidth,

          # UI Plot Options ----
          shiny::fluidRow(
            shiny::column(3L, shiny::uiOutput("TickMarks"), htmltools::tags$br()),
            shiny::column(3L, shiny::uiOutput("AngleY"), htmltools::tags$br()),
            shiny::column(3L, shiny::uiOutput("AngleX"), htmltools::tags$br())),
          shiny::fluidRow(
            shiny::column(3L, shiny::uiOutput("TextSize"), htmltools::tags$br()),
            shiny::column(3L, shiny::uiOutput("TextColor"), htmltools::tags$br()),
            shiny::column(3L, shiny::uiOutput("ChartColor"), htmltools::tags$br())),
          shiny::fluidRow(
            shiny::column(3L, shiny::uiOutput("GridColor"), htmltools::tags$br()),
            shiny::column(3L, shiny::uiOutput("BackGroundColor"), htmltools::tags$br()),
            shiny::column(3L, shiny::uiOutput("BorderColor"), htmltools::tags$br())))))))

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # PlotType
  output$PlotType <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = 'PlotType', Label = 'Plot Type', Choices = c('box', 'line'), SelectedDefault = 'box', Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$NumberGroupsDisplay <- shiny::renderUI({
    RemixAutoML::NumericInput(InputID = 'NumberGroupsDisplay', Label = '# of Levels', Step = 1L, Value = 5L, Min = 1L, Max = 100L)
  })

  # UI Plot Options ----
  output$TickMarks <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = "TickMarks", Label = "Tick marks x-axis", Choices = c("1 year","1 day","3 day","1 week","2 week","1 month","3 month","6 month","2 year","5 year","10 year","1 minute","15 minutes","30 minutes","1 hour","3 hour","6 hour","12 hour"), SelectedDefault = "1 year", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
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
    if(Debug) print('PickerInput GroupVars')
    RemixAutoML::PickerInput(
      InputID = 'GroupVars',
      Label = 'Select Group Variables',
      Choices = GroupVariables,
      SelectedDefault = GroupVariables,
      SelectedText = 'count > 1',
      Multiple = TRUE,
      ActionBox = TRUE)
  })

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
    x <- class(data[[eval(input[['FilterVariable']])]])
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
      Label = 'Percentile or Levels',
      Choices = FilterUnique,
      SelectedDefault = FilterUnique[1L],
      Multiple = FALSE,
      ActionBox = TRUE)
  })

  # Levels
  output$Levels_1 <- shiny::renderUI({
    if(Debug) print('PickerInput_GetLevels 1')
    RemixAutoML::PickerInput_GetLevels(
      input,
      data = 'data',
      NumGroupVar = 1L,
      InputID = 'Levels_1',
      InputID2 = 'GroupVars',
      Choices = UniqueLevels(input, data, 1L, SelectedGroups()),
      SelectedDefault = UniqueLevels(input, data, 1L, SelectedGroups()),
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
      Choices = UniqueLevels(input, data, 2L, SelectedGroups()),
      SelectedDefault = UniqueLevels(input, data, 2L, SelectedGroups()),
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
      Choices = UniqueLevels(input, data, 3L, SelectedGroups()),
      SelectedDefault = UniqueLevels(input, data, 3L, SelectedGroups()),
      Size = 10,
      SelectedText = "count > 1",
      Multiple = TRUE,
      ActionBox = TRUE)
  })

  # Update Plot Format Only
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
      BackGroundColor = input[['BackGroundColor']]))

    # Update labels
    p1 <- shiny::isolate(p1 + ggplot2::labs(
      title = 'Distribution over Time',
      subtitle = 'Red line = mean(Y)',
      caption = 'by RemixAutoML') +
        ggplot2::ylim(as.numeric(eval(input[['YMin']])), as.numeric(eval(input[['YMax']]))) +
        ggplot2::ylab(eval(YVar())) + ggplot2::xlab(DateName))

    # Return
    p1 <<- p1
    output$Trend <- shiny::renderPlot({
      p1
    })
  })

  # Generate Plot
  shiny::observeEvent(eventExpr = input[['TrendPlotExecute']], {

    # Remove NA's
    if(Debug) print('remove NA')
    data1 <- shiny::isolate(data[!is.na(get(YVar()))])

    # Subset by FilterVariable
    if(Debug) print('Subset by FilterVariable')
    shiny::isolate(
      if(!is.null(input[['FilterVariable']])) {
        if(tolower(class(data1[[eval(input[['FilterVariable']])]])) %chin% c('factor', 'character')) {
          if(input[['FilterLogic']] == '%in%') {
            data1 <- data1[get(input[['FilterVariable']]) %chin% c(eval(input[['FilterValue']]))]
          } else if(input[['FilterLogic']] == '%like%') {
            data1 <- data1[get(input[['FilterVariable']]) %like% c(eval(input[['FilterValue']]))]
          }
        } else if(tolower(class(data1[[eval(input[['FilterVariable']])]])) %chin% c('numeric', 'integer', 'date', 'posix')) {
          if(input[['FilterLogic']] == '>') {
            data1 <- data1[get(input[['FilterVariable']]) > eval(input[['FilterValue']])]
          } else if(input[['FilterLogic']] == '>=') {
            data1 <- data1[get(input[['FilterVariable']]) >= eval(input[['FilterValue']])]
          } else if(input[['FilterLogic']] == '<') {
            data1 <- data1[get(input[['FilterVariable']]) < eval(input[['FilterValue']])]
          } else {
            data1 <- data1[get(input[['FilterVariable']]) <= eval(input[['FilterValue']])]
          }
        }
      })

    # Subset Rows based on Filters
    if(Debug) print('Subset Rows based on Filters')
    data1 <- shiny::isolate(
      RemixAutoML::PreparePlotData(
        SubsetOnly = if(input[['PlotType']] == 'box') TRUE else FALSE,
        input,
        PlotDataForecast = data1,
        Aggregate = 'mean',
        TargetVariable = YVar(),
        DateVariable = DateName,
        GroupVariables = SelectedGroups(),
        G1Levels = 'Levels_1',
        G2Levels = 'Levels_2',
        G3Levels = 'Levels_3'))

    # Render Plot
    output$Trend <- shiny::renderPlot({

      if(Debug) print(data1)
      print(SelectedGroups())

      # Plot
      shiny::isolate(
        if(input[['PlotType']] == 'box') {
          p1 <- shiny::isolate(ggplot2::ggplot(data = data1, ggplot2::aes(x = get(DateName), y = get(YVar()), group = get(DateName))))
          p1 <- p1 + ggplot2::geom_boxplot(outlier.size = 0.1, outlier.colour = 'blue', fill = 'gray')
          p1 <- shiny::isolate(p1 + ggplot2::geom_hline(color = 'red', yintercept = eval(mean(data1[[eval(YVar())]], na.rm = TRUE))))
          p1 <- shiny::isolate(p1 + RemixAutoML::ChartTheme(
            Size = input[['TextSize']],
            AngleX = input[['AngleX']],
            AngleY = input[['AngleY']],
            ChartColor = input[['ChartColor']],
            BorderColor = input[['BorderColor']],
            TextColor = input[['TextColor']],
            GridColor = input[['GridColor']],
            BackGroundColor = input[['BackGroundColor']]))
          p1 <- shiny::isolate(p1 + ggplot2::labs(
            title = 'Distribution over Time',
            subtitle = 'Red line = mean(Y)',
            caption = 'by RemixAutoML') +
              ggplot2::ylim(as.numeric(eval(input[['YMin']])), as.numeric(eval(input[['YMax']]))) +
              ggplot2::ylab(eval(YVar())) + ggplot2::xlab(DateName) +
              ggplot2::scale_x_date(date_breaks = input[['TickMarks']]))
        } else {
          p1 <- RemixAutoML:::TimeSeriesPlotter(
            data = data1,
            TargetVariable = YVar(),
            DateVariable = DateName,
            GroupVariables = SelectedGroups(),
            Aggregate = 'mean',
            NumberGroupsDisplay = input[['NumberGroupsDisplay']],
            LevelsToDisplay = NULL,
            OtherGroupLabel = "OtherGroups",
            DisplayOtherGroup = TRUE,
            TextSize = input[['TextSize']],
            LineWidth = 0.5,
            Color = 'blue',
            XTickMarks = input[['TickMarks']],
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
        })


      # Return
      p1 <<- p1
      p1
      })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
