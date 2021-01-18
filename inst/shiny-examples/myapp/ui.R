library(data.table)
library(RemixAutoML)
library(shiny)
library(shinydashboard)
library(flexdashboard)
library(DT)
library(shinyWidgets)
library(shinyjs)
library(shinyBS)

# Begin UI Design Code ----
dashboardPage(

  # Color of app? ----
  skin = "black",

  #----

  # Title of App ----
  # how to add logo to header: https://stackoverflow.com/questions/48978648/align-header-elements-in-shiny-dashboard
  shinydashboard::dashboardHeader(

    tags$li(class = "dropdown",
            tags$style(".main-header {max-height: 55px}"),
            tags$style(".main-header .logo {height: 55px;}"),
            tags$style(".sidebar-toggle {height: 20px; padding-top: 1px !important;}"),
            tags$style(".navbar {min-height:55px !important}")
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

    # Dropdown Menu ----
    # shinydashboard::dropdownMenu(
    #   type = "messages",
    #   badgeStatus = "warning",
    #   messageItem(
    #     from = "Marketing Department",
    #     message = "Ready to begin forecasting"),
    #   messageItem(
    #     from = "New User",
    #     message = "How do I register?",
    #     icon = icon("question"),
    #     time = "13:45"),
    #   messageItem(
    #     from = "Support",
    #     message = "The new version is ready.",
    #     icon = icon("life-ring"),
    #     time = as.character(Sys.time())))
    ),

  #----

  # Menu Panel for RemixAutoML Modules ----
  shinydashboard::dashboardSidebar(

    # Change width to see all words on menu
    width = 315,

    # Contents of side bar menu ----
    sidebarMenu(
      id = "modelMenu",

      # Automated Machine Learning ----
      menuItem(
        text = "Automated Machine Learning",
        icon = icon("robot"),
        tabName = "automated_machine_learning",

        # Data Import Tab
        menuSubItem(
          text = "Import Data",
          tabName = "automated_machine_learning_data_import",
          icon = icon('database')),

        # Feature Engineering Tab
        menuSubItem(
          text = "Automated Feature Engineering",
          tabName = "automated_machine_learning_feature_engineering",
          icon = icon('wrench')),

        # Model Development Tab
        menuSubItem(
          text = "Automated Model Development",
          tabName = "automated_machine_learning_model_building",
          icon = icon('brain')),

        # Model Evaluation Tab
        menuSubItem(
          text = "Automated Model Evaluation",
          tabName = "automated_machine_learning_model_evaluation",
          icon = icon('check-double')),

        # Feature Evaluation Tab
        menuSubItem(
          text = "Automated Feature Evaluation",
          tabName = "automated_machine_learning_feature_evaluation",
          icon = icon('project-diagram'))),

      # Automated Time Series Forecasting ----
      menuItem(
        text = "Automated Time Series Forecasting",
        tabName = "automated_forecasting",
        icon = icon("chart-line"),

        # Create Project Tab ----
        menuSubItem(
          text = "Create of Open Project",
          tabName = "automated_timeseries_project_creation",
          icon = icon('database')),

        # Import Data Tab ----
        menuSubItem(
          text = "Import Data",
          tabName = "automated_forecasting_data_import",
          icon = icon('database')),

        # Data Analysis Tab ----
        menuSubItem(
          text = "Data Analysis",
          tabName = "automated_forecasting_eda",
          icon = icon('chart-bar')),

        # Forecasting Tab ----
        menuSubItem(
          text = "Time Series Forecast",
          tabName = "automated_forecasting_autots_autocarma",
          icon = icon('crosshairs')),

        # Model Evaluation Tab ----
        menuSubItem(
          text = "Model Evaluation",
          tabName = "automated_forecasting_model_evaluation",
          icon = icon('check-double')),

        # Deployment Tab ----
        menuSubItem(
          text = "Deploy Model",
          tabName = "automated_forecasting_deploy_model",
          icon = icon('terminal'))),

      # Intermittent Demand and Optimization ----
      menuItem(
        text = "Intermittent Demand and Optimization",
        tabName = "automated_inventory_optimization",
        icon = icon("gem"),

        # Create or Open Project Page ----
        menuSubItem(
          text = "Create or Open Project",
          tabName = "automated_inventory_optimization_project_creation",
          icon = icon('bezier-curve')),

        # Import Data Tab ----
        menuSubItem(
          text = "Import Data",
          tabName = "automated_inventory_optimization_data_import",
          icon = icon('database')),

        # Data Construction Tab ----
        menuSubItem(
          text = "Data Construction",
          tabName = "automated_inventory_optimization_data_construction",
          icon = icon('wrench')),

        # Model Building Tab ----
        menuSubItem(
          text = "Intermittent Demand Modeling",
          tabName = "automated_inventory_optimization_modeling",
          icon = icon('brain')),

        # Forecasting Tab ----
        menuSubItem(
          text = "Intermittent Demand Forecasting",
          tabName = "automated_inventory_optimization_forecasting",
          icon = icon('random')),

        # Inventory Optimization Tab ----
        menuSubItem(
          text = "Optimization",
          tabName = "automated_inventory_optimization_recommendations",
          icon = icon('check-double'))),

      # Automated Recommenders ----
      menuItem(
        text = "Automated Recommenders",
        icon = icon("cart-arrow-down"),
        tabName = "automated_recommenders",
        badgeLabel = "soon",
        badgeColor = "fuchsia"),

      # Automated Anomaly Detection ----
      menuItem(
        text = "Anomaly Detection",
        icon = icon("r-project"),
        tabName = "anomaly_detection",
        badgeLabel = "soon",
        badgeColor = "teal"))),

  #----

  # Core UI Body for Display, Output, and User Selection of items ----
  dashboardBody(

    # Instantiate ShinyJS ----
    shinyjs::useShinyjs(),

    #----

    # Custom css ----
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
    tags$head(tags$style('h1 {color:darkblue;}')),
    tags$head(tags$style('body {color:darkblue;}')),

    # Module Body UI Elements Go Here ----
    tabItems(

      #----

      # AUTOMATED MACHINE LEARNING TABS ----
      tabItem(

        # Name of tab being used ----
        tabName = "automated_machine_learning"),

      #----

      #----

      #----

      #----

      # AUTOMATED TIME SERIES FORECASTING TABS ----
      # TS Forecasting Create Project Tab ----
      tabItem(

        # -- TAB REFERENCE VALUE ----
        tabName = "automated_timeseries_project_creation",

        # -- PAGE TITLE ----
        tags$h1("Time Series Modeling"),
        tags$h3("Create or Open Project"),

        # -- ADD SPACE ----
        shiny::fluidRow(shiny::column(width = 12,tags$br())),

        # -- ADD SPACE ----
        shiny::fluidRow(shiny::column(width = 12,tags$br())),

        # -- NEXT PREVIOUS BUTTONS ----
        shiny::fluidRow(
          shiny::column(
            width = 8),
          shiny::column(
            width = 4,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "link_to_automated_forecasting_data_import_1",
              label = "Go To Data Import",
              icon = icon("chevron-right", lib = "font-awesome"),
              style = "gradient",
              color = "royal"))),

        # -- ADD SPACE ----
        shiny::fluidRow(shiny::column(width = 12,tags$br())),

        # Create or Open Project UI ----
        shiny::tabsetPanel(

          # Tab types
          type = "tabs", # "tabs","pills"

          # Create Project ----
          shiny::tabPanel(

            # -- TAB TITLE ----
            title = "Create Project",

            # -- TAB ICON ----
            icon = shiny::icon("layer-group", lib = "font-awesome"),

            # -- ADD SPACE ----
            shiny::fluidRow(shiny::column(width = 12,tags$br())),

            # UI Elements for Creating Project ----
            shiny::fluidRow(
              shiny::column(
                width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("drafting-compass", lib = "font-awesome"), "Create Project"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  background = "navy",
                  width = 12,

                  # -- ADD SPACE ----
                  shiny::fluidRow(shiny::column(width = 12,tags$br())),

                  # UI for Suppling Name of New Project ----
                  shiny::column(
                    width = 12,
                    shiny::uiOutput("TS_NewProjectName"),
                    tags$br()),

                  # -- ADD SPACE ----
                  shiny::fluidRow(shiny::column(width = 12,tags$br())),

                  # UI for Supply Path to Folder to Create Project ----
                  shiny::column(
                    width = 12,
                    shiny::uiOutput("TS_Root_Folder"),
                    tags$br())))),

            # -- ADD SPACE ----
            shiny::fluidRow(shiny::column(width = 12,tags$br())),

            # -- ADD SPACE ----
            shiny::fluidRow(shiny::column(width = 12,tags$br())),

            # Action button to create file directory ----
            shiny::fluidRow(
              shiny::column(
                width = 8,
                shinyjs::useShinyjs(),
                shinyWidgets::actionBttn(
                  inputId = "TS_CreateProject",
                  label = "Press to Create Project",
                  icon = icon("plus", lib = "font-awesome"),
                  style = "gradient",
                  color = "royal")),

              # Action button to Start Over ----
              shiny::column(
                width = 4,
                shinyjs::useShinyjs(),
                shinyWidgets::actionBttn(
                  inputId = "TS_StartOver",
                  label = "Press to Start Over",
                  icon = icon("plus", lib = "font-awesome"),
                  style = "gradient",
                  color = "primary")))),

          # Open Project ----
          shiny::tabPanel(

            # -- TAB TITLE ----
            title = "Open Project",

            # -- TAB ICON ----
            icon = shiny::icon("edit", lib = "font-awesome"),

            # -- ADD SPACE ----
            shiny::fluidRow(shiny::column(width = 12,tags$br())),

            # UI elements for importing data ----
            shiny::fluidRow(
              shiny::column(
                width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("upload", lib = "font-awesome"), "Open Existing Project"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  background = "navy",
                  width = 12,

                  # -- ADD SPACE ----
                  shiny::fluidRow(shiny::column(width = 12,tags$br())),

                  # UI for Supply Path to Folder to Create Project ----
                  shiny::column(
                    width = 10,
                    shiny::uiOutput("TS_ProjectListUpload"),
                    tags$br())))),

            # Action button to open ProjectList ----
            shiny::fluidRow(
              shiny::column(
                width = 4,
                shinyjs::useShinyjs(),
                shinyWidgets::actionBttn(
                  inputId = "TS_OpenProject",
                  label = "Press to Open Project",
                  icon = icon("folder-open", lib = 'font-awesome'),
                  style = "gradient",
                  color = "royal")))),

          # HELP PAGE ----
          shiny::tabPanel(

            # -- TAB TITLE ----
            title = "Help",

            # -- TAB ICON ----
            icon = shiny::icon("ambulance", lib = "font-awesome"),

            # -- ADD SPACE ----
            shiny::fluidRow(shiny::column(width = 12,tags$br())),

            # UI elements for importing data ----
            shiny::fluidRow(
              shiny::column(
                width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("upload", lib = "font-awesome"), "Open Existing Project"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  background = "light-blue",
                  width = 12,

                  # -- ADD SPACE ----
                  shiny::fluidRow(shiny::column(width = 12,tags$br())),

                  # Help with Name Your Project Input ----
                  tags$h4("< Name Your Project >"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),">",HTML('&nbsp'),"The name of your project will the name of the folder containing all the folders with project files."))),

                  # -- ADD SPACE ----
                  shiny::fluidRow(shiny::column(width = 12,tags$br())),

                  # Help with selecting the target variable ----
                  tags$h4("< Supply Path File To Root Directory Folder Where Project Folders will be Created >"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),">",HTML('&nbsp'),
                               "Supply a path file (no quotes around them) to the location where you want your project stored."))))))))),

      #----

      # TS Data Import Tab ----
      tabItem(

        # -- TAB REFERENCE VALUE ----
        tabName = "automated_forecasting_data_import",

        # -- PAGE TITLE ----
        tags$h1("Time Series Modeling"),
        tags$h3("Import and Prepare Data"),

        # -- ADD SPACE ----
        shiny::fluidRow(shiny::column(width = 12,tags$br())),

        # -- ADD SPACE ----
        shiny::fluidRow(shiny::column(width = 12,tags$br())),

        # -- NEXT PREVIOUS BUTTONS ----
        shiny::fluidRow(
          shiny::column(
            width = 8,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "link_to_automated_timeseries_project_creation",
              label = "Create or Open Project",
              icon = icon("chevron-left", lib = "font-awesome"),
              style = "gradient",
              color = "royal")),
          shiny::column(
            width = 4,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "link_to_automated_forecasting_eda",
              label = "Data Analysis",
              icon = icon("chevron-right", lib = "font-awesome"),
              style = "gradient",
              color = "royal"))),

        # -- ADD SPACE ----
        shiny::fluidRow(shiny::column(width = 12,tags$br())),

        # Required Inputs and HELP PAGE ----
        shiny::tabsetPanel(

          # Tab types
          type = "tabs", # "tabs","pills"

          # TAB IMPORT NEW DATA ----
          shiny::tabPanel(

            # -- TAB TITLE ----
            title = "Import New Data",

            # -- TAB ICON ----
            icon = shiny::icon("atom", lib = "font-awesome"),

            # -- ADD SPACE ----
            shiny::fluidRow(shiny::column(width = 12,tags$br())),

            # UI elements for importing TrainData ----
            shiny::fluidRow(
              shiny::column(
                width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("database", lib = "font-awesome"), "Import Training Data"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  background = "navy",
                  width = 12,

                  # -- ADD SPACE ----
                  shiny::fluidRow(shiny::column(width = 12,tags$br())),

                  # UI Import Functionality ----
                  shiny::fileInput(
                    inputId = "TimeSeriesData",
                    label =  "Choose CSV File",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv"))))),

            # -- ADD SPACE ----
            shiny::fluidRow(shiny::column(width = 12,tags$br())),

            # UI elements for importing XREGS ----
            shiny::fluidRow(
              shiny::column(
                width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("database", lib = "font-awesome"), "Import XREGS Data"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  background = "purple",
                  width = 12,

                  # --ADD SPACE----
                  shiny::fluidRow(shiny::column(width = 12,tags$br())),

                  # UI Import Functionality ----
                  shiny::fileInput(
                    inputId = "XREGS",
                    label =  "Choose CSV File",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv"))))),

            # -- ADD SPACE ----
            shiny::fluidRow(shiny::column(width = 12,tags$br())),

            # UI elements for importing EvalData ----
            shiny::fluidRow(
              shiny::column(
                width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("database", lib = "font-awesome"), "Import Evaluation Data"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  background = "aqua",
                  width = 12,

                  # -- ADD SPACE ----
                  shiny::fluidRow(shiny::column(width = 12,tags$br())),

                  # UI Import Functionality ----
                  shiny::fileInput(
                    inputId = "Eval",
                    label =  "Choose CSV File",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv"))))),

            # -- ADD SPACE ----
            shiny::fluidRow(shiny::column(width = 12,tags$br())),

            # -- ADD SPACE ----
            shiny::fluidRow(shiny::column(width = 12,tags$br())),

            # Action Button to Store Data in Data Folder ----
            shiny::fluidRow(
              shiny::column(
                width = 6,
                shinyjs::useShinyjs(),
                shinyWidgets::actionBttn(
                  inputId = "TS_SaveDataToDataFolder",
                  label = "Store Data in Project",
                  icon = icon("save", lib = "font-awesome"),
                  style = "gradient",
                  color = "royal")))),

          # HELP PAGE ----
          shiny::tabPanel(

            # -- TAB TITLE ----
            title = "Help",

            # -- TAB ICON ----
            icon = shiny::icon("ambulance", lib = "font-awesome"),

            # -- ADD SPACE ----
            shiny::fluidRow(shiny::column(width = 12,tags$br())),

            # HELP PAGE ----
            shiny::fluidRow(
              shiny::column(
                width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("filter", lib = "font-awesome"), "Inputs Explanations"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  background = "light-blue",
                  width = 12,

                  # -- ADD SPACE ----
                  shiny::fluidRow(shiny::column(width = 12,tags$br())),

                  # Help with import button ----
                  tags$h4("* Choose CSV File:"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),">",HTML('&nbsp'),"Click the Browse... button to open a window to select your transactions data file"))),

                  # -- ADD SPACE ----
                  shiny::fluidRow(shiny::column(width = 12,tags$br())),

                  # Help with selecting the target variable ----
                  tags$h4("* Select Target Variable"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),">",HTML('&nbsp'),"Click the down arrow to select the column names of the variables you will be forecasting"))),

                  # --ADD SPACE----
                  shiny::fluidRow(shiny::column(width = 12,tags$br())),

                  # Help with selecting the date variable ----
                  tags$h4("* Select Date Variable"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),">",HTML('&nbsp'),"Click the down arrow to select the date column in the data"))),

                  # --ADD SPACE----
                  shiny::fluidRow(shiny::column(width = 12,tags$br())),

                  # Help with selecting the group variable ----
                  tags$h4("* Select Group Variables"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),">",HTML('&nbsp'),"Click the down arrow to select group columns in the data"))))))))),

      #----



      #----

      # Time Series Data Analysis Tab----
      tabItem(

        # -- TAB REFERENCE VALUE ----
        tabName = "automated_forecasting_eda",

        # -- PAGE TITLE ----
        tags$h1("Time Series"),
        tags$h3("Data Analysis"),

        # -- ADD SPACE ----
        shiny::fluidRow(shiny::column(width = 12,tags$br())),

        # -- ADD SPACE ----
        shiny::fluidRow(shiny::column(width = 12,tags$br())),

        # -- NEXT PREVIOUS BUTTONS ----
        shiny::fluidRow(
          shiny::column(
            width = 8,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "link_to_automated_forecasting_data_import",
              label = "Import Data",
              icon = icon("chevron-left", lib = "font-awesome"),
              style = "gradient",
              color = "royal")),
          shiny::column(
            width = 4,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "link_to_automated_forecasting_autots_autocarma",
              label = "Time Series Forecast",
              icon = icon("chevron-right", lib = "font-awesome"),
              style = "gradient",
              color = "royal"))),

        # -- ADD SPACE ----
        shiny::fluidRow(shiny::column(width = 12,tags$br())),

        # Plot Data or See DataTable ----
        shiny::tabsetPanel(
          id = "time_series_EDA_tab_1",
          type = "tabs",

          # Plotly Time Series ----
          shiny::tabPanel(

            # -- TITLE ----
            title = "Time Series Plot",
            icon = icon('chart-line', lib = 'font-awesome'),

            # TRAINING DATA: UI elements for selecting data columns for subsetting ----
            shiny::fluidRow(
              shiny::column(
                width = 12,
                shiny::fluidRow(shiny::column(width = 12,tags$br())),
                shinydashboard::box(
                  title = tagList(shiny::icon("filter", lib = "font-awesome"), "Training Data Variables"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  background = "navy",
                  width = 12,

                  # -- ADD SPACE ----
                  shiny::fluidRow(shiny::column(width = 12,tags$br())),

                  # Model Input Variables ----
                  shiny::fluidRow(
                    shiny::column(
                      width = 4,
                      shiny::uiOutput("timeSeriesTimeUnit")),
                      tags$br()),
                  shiny::fluidRow(shiny::column(width = 12,tags$br())),
                  shiny::fluidRow(
                    shiny::column(
                      width = 4,
                      shiny::uiOutput("timeSeriesTarget")),
                    shiny::column(
                      width = 4,
                      shiny::uiOutput("timeSeriesDateColumn")),
                    shiny::column(
                      width = 4,
                      shiny::uiOutput("timeSeriesGroupVars"))),

                  # -- ADD SPACE ----
                  shiny::fluidRow(shiny::column(width = 12,tags$br())),

                  # TimeSeriesFill and ModelDataPrep ----
                  shiny::fluidRow(
                    shiny::column(
                      width = 5,
                      shinyjs::useShinyjs(),
                      shinyWidgets::actionBttn(
                        inputId = "DataPrep_Train",
                        label = "Prepare Data",
                        icon = icon("chevron-right", lib = "font-awesome"),
                        style = "gradient",
                        color = "royal")))))),

            # -- ADD SPACE ----
            shiny::fluidRow(shiny::column(width = 12,tags$br())),

            # XREGS DATA: UI elements for selecting data columns for subsetting ----
            shiny::fluidRow(
              shiny::column(
                width = 12,
                shiny::fluidRow(shiny::column(width = 12,tags$br())),
                shinydashboard::box(
                  title = tagList(shiny::icon("filter", lib = "font-awesome"), "XREGS Data Variables"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  background = "navy",
                  width = 12,

                  # -- ADD SPACE ----
                  shiny::fluidRow(shiny::column(width = 12,tags$br())),

                  # Model Input Variables ----
                  shiny::fluidRow(
                    shiny::column(
                      width = 4,
                      shiny::uiOutput("timeSeriesTarget_XREGS")),
                    shiny::column(
                      width = 4,
                      shiny::uiOutput("timeSeriesDateColumn_XREGS")),
                    shiny::column(
                      width = 4,
                      shiny::uiOutput("timeSeriesGroupVars_XREGS"))),

                  # -- ADD SPACE ----
                  shiny::fluidRow(shiny::column(width = 12,tags$br())),

                  # TimeSeriesFill and ModelDataPrep ----
                  shiny::column(
                    width = 5,
                    shinyjs::useShinyjs(),
                    shinyWidgets::actionBttn(
                      inputId = "DataPrep_XREGS",
                      label = "Prepare Data",
                      icon = icon("chevron-right", lib = "font-awesome"),
                      style = "gradient",
                      color = "royal"))))),

            # -- ADD SPACE ----
            shiny::fluidRow(shiny::column(width = 12,tags$br())),

            # Eval DATA: UI elements for selecting data columns for subsetting ----
            shiny::fluidRow(
              shiny::column(
                width = 12,
                shiny::fluidRow(shiny::column(width = 12,tags$br())),
                shinydashboard::box(
                  title = tagList(shiny::icon("filter", lib = "font-awesome"), "Evaluation Data Variables"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  background = "purple",
                  width = 12,

                  # -- ADD SPACE ----
                  shiny::fluidRow(shiny::column(width = 12,tags$br())),

                  # Model Input Variables ----
                  shiny::fluidRow(
                    shiny::column(
                      width = 4,
                      shiny::uiOutput("timeSeriesTarget_Eval")),
                    shiny::column(
                      width = 4,
                      shiny::uiOutput("timeSeriesDateColumn_Eval")),
                    shiny::column(
                      width = 4,
                      shiny::uiOutput("timeSeriesGroupVars_Eval"))),

                  # -- ADD SPACE ----
                  shiny::fluidRow(shiny::column(width = 12,tags$br())),

                  # TimeSeriesFill and ModelDataPrep ----
                  shiny::column(
                    width = 5,
                    shinyjs::useShinyjs(),
                    shinyWidgets::actionBttn(
                      inputId = "DataPrep_Eval",
                      label = "Prepare Data",
                      icon = icon("chevron-right", lib = "font-awesome"),
                      style = "gradient",
                      color = "royal"))))),

            # -- ADD SPACE ----
            shiny::fluidRow(shiny::column(width = 12,tags$br())),

            # Group Variables and Levels Selection----
            shiny::fluidRow(
              shiny::column(
                width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("filter", lib = "font-awesome"), "Select group levels"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  background = "blue",
                  width = 12,

                  # -- ADD SPACE ----
                  shiny::fluidRow(shiny::column(width = 12,tags$br())),

                  # Single Group Level Selection ----
                  shiny::column(
                    width = 4,
                    conditionalPanel(
                      condition = "length(input['timeSeriesGroupVars']) >= 1",
                      shiny::uiOutput("TS_Group1Levels"))),

                  # Two Group Level Selection ----
                  shiny::column(
                    width = 4,
                    conditionalPanel(
                      condition = "length(input['timeSeriesGroupVars']) >= 2",
                      shiny::uiOutput("TS_Group2Levels"))),

                  # Three Group Level Selection ----
                  shiny::column(
                    width = 4,
                    conditionalPanel(
                      condition = "1 < 0", #"length(input['timeSeriesGroupVars']) >= 3",
                      shiny::uiOutput("TS_Group3Levels")))))),

            # -- ADD SPACE ----
            shiny::fluidRow(shiny::column(width = 12,tags$br())),

            # Time Series Plot Box ----
            shinydashboard::box(title = tagList(shiny::icon("database", lib = "font-awesome"), "Time Series Plot"),
                solidHeader = TRUE,
                width = 12,
                tags$br(),
                background = "aqua",
                shiny::column(
                  width = 1,
                  shinyWidgets::dropdown(
                    tags$h5("Time Series Plot Inputs"),
                    tags$h5(tags$style('h5 {color:darkblue;}')),
                    tags$h5(tags$style('#TS_AggregateFunction {color:darkblue;}')),
                    tags$h5(tags$style('#TS_OtherGroups {color:darkblue;}')),
                    tags$h5(tags$style('#TS_NumberGroupsDisplay {color:darkblue;}')),
                    style = "pill",
                    icon = icon("gear"),
                    tooltip = tooltipOptions(title = "Click to see plot options"),
                    status = "default", width = "300px",
                    shiny::uiOutput("TS_AggregateFunction"),
                    shiny::uiOutput("TS_OtherGroups"),
                    shiny::uiOutput("TS_NumberGroupsDisplay"))),
                shiny::column(
                  width = 9,
                  shiny::fluidRow(
                    shinyjs::useShinyjs(),
                    shinyWidgets::actionBttn(
                      inputId = "CreateTimeSeriesPlot",
                      label = "Create Time Series Plot",
                      style = "gradient",
                      color = "royal"))),
                shiny::column(
                  width = 2,
                  shiny::fluidRow(
                    shinyjs::useShinyjs(),
                    shinyWidgets::actionBttn(
                      inputId = "TS_ResetPlotSettings",
                      label = "Reset Plot",
                      style = "gradient",
                      color = "primary"))),

                # -- ADD SPACE ----
                shiny::fluidRow(shiny::column(width = 12,tags$br())),


                # Time Series Plot ----
                plotly::plotlyOutput("TimeSeriesPlot", width = "100%")),

            # -- ADD SPACE ----
            shiny::fluidRow(shiny::column(width = 12,tags$br())),

            # Optional Plot Inputs----
            shiny::fluidRow(
              shiny::column(
                width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("database", lib = "font-awesome"), "Plot Options 1"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  background = "aqua",
                  width = 12,

                  # -- ADD SPACE ----
                  shiny::fluidRow(shiny::column(width = 12,tags$br())),

                  # UI Plot Options ----
                  shiny::fluidRow(
                    shiny::column(
                      width = 3,
                      shiny::uiOutput("TickMarksX"),
                      tags$br()),
                    shiny::column(
                      width = 3,
                      shiny::uiOutput("TS_LineWidth"),
                      tags$br()),
                    shiny::column(
                      width = 3,
                      shiny::uiOutput("TS_AngleY"),
                      tags$br()),
                    shiny::column(
                      width = 3,
                      shiny::uiOutput("TS_AngleX"),
                      tags$br())),
                  shiny::fluidRow(
                    shiny::column(
                      width = 3,
                      shiny::uiOutput("TS_TextSize"),
                      tags$br()),
                    shiny::column(
                      width = 3,
                      shiny::uiOutput("TS_TextColor"),
                      tags$br()),
                    shiny::column(
                      width = 3,
                      shiny::uiOutput("TS_LineColor"),
                      tags$br()),
                    shiny::column(
                      width = 3,
                      shiny::uiOutput("TS_LegendTextColor"),
                      tags$br())),
                  shiny::fluidRow(
                    shiny::column(
                      width = 3,
                      shiny::uiOutput("TS_ChartColor"),
                      tags$br()),
                    shiny::column(
                      width = 3,
                      shiny::uiOutput("TS_GridColor"),
                      tags$br()),
                    shiny::column(
                      width = 3,
                      shiny::uiOutput("TS_BackGroundColor"),
                      tags$br()),
                    shiny::column(
                      width = 3,
                      shiny::uiOutput("TS_BorderColor"),
                      tags$br())))))),

          # DataTable Time Series----
          shiny::tabPanel(
            title = "Time Series Data",
            icon = icon('database', lib = 'font-awesome'),
            tags$br(),
            shinydashboard::box(title = "Time Series Data Table",
                solidHeader = TRUE,
                width = 12,
                background = "aqua",
                tags$style(
                  HTML(".dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing, .dataTables_wrapper .dataTables_paginate {
                        color: #ffffff;
                        }
                        thead {
                        color: #ffffff;
                        }
                        tbody {
                        color: #000000;
                        }")),
                DT::DTOutput(outputId = "TimeSeriesDT", width = "100%"))))),

      #----

      # Model Building Tab ----
      tabItem(

        # -- Name of tab ----
        tabName = "automated_forecasting_autots_autocarma",

        # -- PAGE TITLE ----
        tags$h1("Time Series"),
        tags$h3("Model Building and Forecasting"),

        # -- ADD SPACE ----
        shiny::fluidRow(shiny::column(width = 12,tags$br())),

        # -- ADD SPACE ----
        shiny::fluidRow(shiny::column(width = 12,tags$br())),

        # -- NEXT PREVIOUS BUTTONS ----
        shiny::fluidRow(
          shiny::column(width = 8,
                 shinyjs::useShinyjs(),
                 shinyWidgets::actionBttn(
                   inputId = "link_to_automated_forecasting_eda_1",
                   label = "Data Analysis",
                   icon = icon("chevron-left", lib = "font-awesome"),
                   style = "gradient",
                   color = "royal")),
          shiny::column(width = 4,
                 shinyjs::useShinyjs(),
                 shinyWidgets::actionBttn(
                   inputId = "link_to_automated_forecasting_model_evaluation",
                   label = "Model Evaluation", icon = icon("chevron-right", lib = "font-awesome"), style = "gradient", color = "royal"))),

        # -- ADD SPACE ----
        shiny::fluidRow(shiny::column(width = 12, tags$br())),

        # Tab Panels ----
        shiny::tabsetPanel(

          # Tab types ----
          type = "tabs",

          # Model Selection and Required Inputs ----
          shiny::tabPanel(

            # -- TAB TITLE ----
            title = "Model Selection",

            # -- TAB ICON ----
            icon = shiny::icon("atom", lib = "font-awesome"),

            # -- ADD SPACE ----
            shiny::fluidRow(shiny::column(width = 12, tags$br())),

            # UI elements for model selection ----
            shiny::fluidRow(
              shiny::column(
                width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("blender", lib = "font-awesome"), "Production Settings and Insights"),
                  solidHeader = FALSE,
                  collapsible = TRUE,
                  background = "navy",
                  width = 12,
                  shiny::fluidRow(shiny::column(width = 12, tags$br())),

                  # Data Dims ----
                  shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_TaskType")),
                  shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_NumGPU")),

                  # -- ADD SPACE ----
                  shiny::fluidRow(shiny::column(width = 12, tags$br())),
                  shiny::fluidRow(shiny::column(width = 12, tags$br())),

                  # Insights Args ----
                  shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_PDFOutputPath")),
                  shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_SaveDataPath")),
                  shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_NumParDepPlots"))))),

            # -- ADD SPACE ----
            shiny::fluidRow(shiny::column(width = 12,tags$br())),

            # Model Selection Box ----
            shiny::fluidRow(
              shiny::column(
                width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("clone", lib = "font-awesome"), "Model Selection"),
                  solidHeader = FALSE,
                  collapsible = TRUE,
                  background = "purple",
                  width = 12,
                  shiny::fluidRow(shiny::column(width = 12,tags$br())),

                  # Model Selection Args ----
                  shiny::column(width = 4, shiny::uiOutput("TimeSeriesModelsSelection")),
                  shiny::column(width = 4, shiny::uiOutput("TSMLModelsSelection")),
                  shiny::column(width = 4, shiny::uiOutput("H2OModelSelection"))))),

            # -- ADD SPACE ----
            shiny::fluidRow(shiny::column(width = 12,tags$br())),

            # Required inputs for time series models ----
            shiny::fluidRow(
              shiny::column(
                width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("tachometer-alt", lib = "font-awesome"), "Required User Inputs"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  background = "aqua",
                  shiny::fluidRow(shiny::column(width = 12,tags$br())),

                  # Forecasting Args ----
                  shiny::column(width = 4, shiny::uiOutput("TS_TimeUnit")),
                  shiny::column(width = 4, shiny::uiOutput("TS_HoldOutPeriods")),
                  shiny::column(width = 4, shiny::uiOutput("TS_FCPeriods")),

                  # -- ADD SPACE ----
                  shiny::fluidRow(shiny::column(width = 12, tags$br())),

                  # Select Variables for Forecasting ----
                  shiny::column(width = 4, shiny::uiOutput("TS_timeSeriesTarget")),
                  shiny::column(width = 4, shiny::uiOutput("TS_timeSeriesDateColumn")),
                  shiny::column(width = 4, shiny::uiOutput("TS_timeSeriesGroupVars"))))),

            # Save Settings Button----
            shiny::fluidRow(
                shiny::column(
                  width = 9,
                  shinyWidgets::actionBttn(
                    inputId = "TS_SaveModelSettings",
                    label = "Save Model Settings",
                    icon = icon("save", lib = 'font-awesome'),
                    style = "gradient",
                    color = "royal",
                    size = "lg")),
                shiny::column(
                  width = 3,
                  align="center",
                  shinyWidgets::actionBttn(
                    inputId = "TS_Build",
                    label = "Push To Start",
                    icon = icon("power-off", lib = 'font-awesome'),
                    style = "gradient",
                    color = "royal",
                    size = "lg")))),

          # Advanced User Settings Tab ----
          shiny::tabPanel(

            # -- TAB TITLE ----
            title = "Advanced User Settings",

            # -- TAB ICON ----
            icon = shiny::icon("atom", lib = "font-awesome"),

            # ----

            # -- ADD SPACE ----
            shiny::fluidRow(shiny::column(width = 12, tags$br())),

            # CatBoost CARMA Args ----
            shiny::fluidRow(
              shiny::column(width = 12,
                     shinydashboard::box(title = tagList(shiny::icon("tachometer-alt", lib = "font-awesome"), "AutoCatBoostCARMA Parameters"),
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         collapsed = TRUE,
                         width = 12,
                         background = "navy",
                         shiny::fluidRow(shiny::column(width = 12,tags$br())),

                         # Target transformation args ----
                         shinydashboard::box(title = tagList(shiny::icon("tachometer-alt", lib = "font-awesome"), "Target Transformations"),
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             collapsed = TRUE,
                             width = 12,
                             background = "purple",
                             shiny::fluidRow(shiny::column(width = 12,tags$br())),

                             # Target variable transformation args
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_Methods")),
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_Difference")),
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_NonNegativePrep")),
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_RoundPreds"))),

                         # Calendar-related args ----
                         shinydashboard::box(title = tagList(shiny::icon("tachometer-alt", lib = "font-awesome"), "Calendar Features"),
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             collapsed = TRUE,
                             width = 12,
                             background = "purple",
                             shiny::fluidRow(shiny::column(width = 12,tags$br())),

                             # Calendar-related args
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_CalendarVariables")),
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_HolidayVariables")),
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_HolidayLags")),
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_HolidayMovingAverages"))),

                         # Lags, moving averages, and other rolling stats ----
                         shinydashboard::box(title = tagList(shiny::icon("tachometer-alt", lib = "font-awesome"), "Time Series Features"),
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             collapsed = TRUE,
                             width = 12,
                             background = "purple",

                             # Lags, moving averages, and other rolling stats
                             shiny::fluidRow(shiny::column(width = 12,tags$br())),
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_HierarchGroups")),
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_Quantiles_Selected")),
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_Lags")),
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_MovingAverages")),

                             # -- ADD SPACE
                             shiny::fluidRow(shiny::column(width = 12,tags$br())),
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_MovingSD")),
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_MovingSkew")),
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_MovingKurt")),
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_MovingQuantiles")),

                             # -- ADD SPACE
                             shiny::fluidRow(shiny::column(width = 12,tags$br())),
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_Lags1")),
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_MovingAverages1")),
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_MovingSD1")),
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_MovingSkew1")),

                             # -- ADD SPACE
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_MovingKurt1")),
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_MovingQuantiles1"))),

                         # Bonus features ----
                         shinydashboard::box(title = tagList(shiny::icon("tachometer-alt", lib = "font-awesome"), "Bonus Features"),
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             collapsed = TRUE,
                             width = 12,
                             background = "purple",
                             shiny::fluidRow(shiny::column(width = 12,tags$br())),

                             # Bonus features
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_AnomalyDetection_HighThreshold")),
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_AnomalyDetection_LowThreshold")),
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_TimeTrend")),
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_Fourier")),

                             # -- ADD SPACE
                             shiny::fluidRow(shiny::column(width = 12,tags$br())),

                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_DataTruncate")),
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_TimeWeights"))),


                         # ML grid tuning args ----
                         shinydashboard::box(title = tagList(shiny::icon("tachometer-alt", lib = "font-awesome"), "ML Grid Tuning"),
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             collapsed = TRUE,
                             width = 12,
                             background = "light-blue",
                             shiny::fluidRow(shiny::column(width = 12,tags$br())),

                             # ML grid tuning
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_GridTune")),
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_ModelCount")),
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_PassInGrid")),
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_MaxRunsWithoutNewWinner")),

                             # -- ADD SPACE
                             shiny::fluidRow(shiny::column(width = 12,tags$br())),
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_MaxRunMinutes"))),

                         # ML loss functions ----
                         shinydashboard::box(title = tagList(shiny::icon("tachometer-alt", lib = "font-awesome"), "ML Loss Functions"),
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             collapsed = TRUE,
                             width = 12,
                             background = "light-blue",
                             shiny::fluidRow(shiny::column(width = 12,tags$br())),

                             # ML loss functions
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_EvalMetric")),
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_EvalMetricValue")),
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_LossFunction")),
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_LossFunctionValue"))),

                         # ML tuning args ----
                         shinydashboard::box(title = tagList(shiny::icon("tachometer-alt", lib = "font-awesome"), "ML Tuning"),
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             collapsed = TRUE,
                             width = 12,
                             background = "light-blue",
                             shiny::fluidRow(shiny::column(width = 12,tags$br())),

                             # ML tuning args
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_NTrees")),
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_Depth")),
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_Langevin")),
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_DiffusionTemperature"))),

                         # ML overfitting args ----
                         shinydashboard::box(title = tagList(shiny::icon("tachometer-alt", lib = "font-awesome"), "ML Overfitting"),
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             collapsed = TRUE,
                             width = 12,
                             background = "light-blue",
                             shiny::fluidRow(shiny::column(width = 12,tags$br())),

                             # ML overfitting args
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_LearningRate")),
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_L2_Leaf_Reg")),
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_RandomStrength")),
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_BorderCount")),

                             # -- ADD SPACE
                             shiny::fluidRow(shiny::column(width = 12,tags$br())),
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_SubSample")),
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_MinDataInLeaf")),
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_SamplingUnit")),
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_RSM")),

                             # -- ADD SPACE
                             shiny::fluidRow(shiny::column(width = 12,tags$br())),
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_ModelSizeReg"))),

                         # ML style args ----
                         shinydashboard::box(title = tagList(shiny::icon("tachometer-alt", lib = "font-awesome"), "ML Build Style"),
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             collapsed = TRUE,
                             width = 12,
                             background = "light-blue",
                             shiny::fluidRow(shiny::column(width = 12,tags$br())),

                             # ML style args
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_BootStrapType")),
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_GrowPolicy")),
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_ScoreFunction")),
                             shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_FeatureBorderType")))))),

            # -- ADD SPACE ----
            shiny::fluidRow(shiny::column(width = 12,tags$br())),

            # XGBoost CARMA Args ----
            shiny::fluidRow(
              shiny::column(width = 12,
                     shinydashboard::box(title = tagList(shiny::icon("tachometer-alt", lib = "font-awesome"), "AutoXGBoostCARMA Configurations"),
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         collapsed = TRUE,
                         width = 12,
                         background = "navy",
                         shiny::fluidRow(shiny::column(width = 12,tags$br())),

                         # XGBoost CARMA Terms 1 ----
                         shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_NonNegativePrep")),
                         shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_RoundPreds")),
                         shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_GridTune")),
                         shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_DataTruncate")),

                         # -- ADD SPACE ----
                         shiny::fluidRow(shiny::column(width = 12,tags$br())),

                         # XGBoost CARMA Terms 2 ----
                         shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_Transformation")),
                         shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_Difference")),
                         shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_CalendarVariables")),
                         shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_HolidayVariables")),

                         # -- ADD SPACE ----
                         shiny::fluidRow(shiny::column(width = 12,tags$br())),

                         # XGBoost CARMA Terms 3 ----

                         shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_NTrees")),
                         shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_TimeTrend")),
                         shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_Fourier")),
                         shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_Quantiles_Selected")),

                         # -- ADD SPACE ----
                         shiny::fluidRow(shiny::column(width = 12,tags$br())),

                         # XGBoost CARMA Terms 4 ----
                         shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_Lags")),
                         shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_HolidayLags")),
                         shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_HolidayMovingAverages")),
                         shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_MovingAverages")),

                         # -- ADD SPACE ----
                         shiny::fluidRow(shiny::column(width = 12,tags$br())),

                         # XGBoost CARMA Terms 5 ----
                         shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_MovingSD")),
                         shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_MovingSkew")),
                         shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_MovingKurt")),
                         shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_MovingQuantiles"))))),

            # -- ADD SPACE ----
            shiny::fluidRow(shiny::column(width = 12,tags$br())),

            # H2O CARMA Args ----
            shiny::fluidRow(
              shiny::column(width = 12,
                     shinydashboard::box(title = tagList(shiny::icon("tachometer-alt", lib = "font-awesome"), "AutoH2OCARMA Configurations"),
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         collapsed = TRUE,
                         width = 12,
                         background = "navy",
                         shiny::fluidRow(shiny::column(width = 12,tags$br())),

                         # H2O CARMA Terms 1 ----
                         shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_NThreads")),
                         shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_MaxMemory")),
                         shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_GridTune")),
                         shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_ModelCount")),

                         # -- ADD SPACE ----
                         shiny::fluidRow(shiny::column(width = 12,tags$br())),

                         # H2O CARMA Terms 2 ----
                         shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_Transformation")),
                         shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_Difference")),
                         shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_CalendarVariables")),
                         shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_HolidayVariables")),

                         # -- ADD SPACE ----
                         shiny::fluidRow(shiny::column(width = 12,tags$br())),

                         # H2O CARMA Terms 3 ----
                         shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_DataTruncate")),
                         shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_NTrees")),
                         shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_TimeTrend")),
                         shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_Fourier")),

                         # -- ADD SPACE ----
                         shiny::fluidRow(shiny::column(width = 12,tags$br())),

                         # H2O CARMA Terms 4 ----
                         shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_Lags")),
                         shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_HolidayLags")),
                         shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_HolidayMovingAverages")),
                         shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_MovingAverages")),

                         # -- ADD SPACE ----
                         shiny::fluidRow(shiny::column(width = 12,tags$br())),

                         # H2O CARMA Terms 5 ----
                         shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_MovingSD")),
                         shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_MovingSkew")),
                         shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_MovingKurt")),
                         shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_MovingQuantiles")),

                         # -- ADD SPACE ----
                         shiny::fluidRow(shiny::column(width = 12,tags$br())),

                         # H2O CARMA Terms 6 ----
                         shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_Quantiles_Selected")),
                         shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_NonNegativePrep")),
                         shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_RoundPreds"))))),

            # -- ADD SPACE ----
            shiny::fluidRow(shiny::column(width = 12,tags$br())),

            # ARIMA ARGS ----
            shiny::fluidRow(
              shiny::column(width = 12,
                     shinydashboard::box(title = tagList(shiny::icon("tachometer-alt", lib = "font-awesome"), "SARIMA Configurations"),
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         collapsed = TRUE,
                         width = 12,
                         background = "purple",
                         shiny::fluidRow(shiny::column(width = 12,tags$br())),

                         # Regular ARIMA Terms----
                         shiny::column(width = 3, shiny::uiOutput("ARIMA_Lags")),
                         shiny::column(width = 3, shiny::uiOutput("ARIMA_MovingAverages")),
                         shiny::column(width = 3, shiny::uiOutput("ARIMA_MaxFourierTerms")),
                         shiny::column(width = 3, shiny::uiOutput("ARIMA_RunsWithoutWinner")),

                         # -- ADD SPACE ----
                         shiny::fluidRow(shiny::column(width = 12,tags$br())),

                         # Seasonal ARIMA Terms----
                         shiny::column(width = 3, shiny::uiOutput("ARIMA_SeasonalLags")),
                         shiny::column(width = 3, shiny::uiOutput("ARIMA_SeasonalMovingAverages")),
                         shiny::column(width = 3, shiny::uiOutput("ARIMA_MaxNumberModels")),
                         shiny::column(width = 3, shiny::uiOutput("ARIMA_MaxRunTime"))))),

            # -- ADD SPACE ----
            shiny::fluidRow(shiny::column(width = 12,tags$br())),

            # NNET ARGS ----
            shiny::fluidRow(
              shiny::column(width = 12,
                     shinydashboard::box(title = tagList(shiny::icon("tachometer-alt", lib = "font-awesome"), "NNET Configurations"),
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         collapsed = TRUE,
                         width = 12,
                         background = "purple",
                         shiny::fluidRow(shiny::column(width = 12,tags$br())),

                         # Regular NNET Terms ----
                         shiny::column(width = 3, shiny::uiOutput("NNET_Lags")),
                         shiny::column(width = 3, shiny::uiOutput("NNET_SeasonalLags")),
                         shiny::column(width = 3, shiny::uiOutput("NNET_MaxFourierTerms")),

                         # -- ADD SPACE ----
                         shiny::fluidRow(shiny::column(width = 12,tags$br())),

                         # NNET Optimizations ----
                         shiny::column(width = 3, shiny::uiOutput("NNET_RunsWithoutWinner")),
                         shiny::column(width = 3, shiny::uiOutput("NNET_MaxNumberModels")),
                         shiny::column(width = 3, shiny::uiOutput("NNET_MaxRunTime")),

                         # --ADD SPACE----
                         shiny::fluidRow(shiny::column(width = 12,tags$br()))))),

            # -- ADD SPACE ----
            shiny::fluidRow(shiny::column(width = 12,tags$br())),

            # TBATS ARGS----
            shiny::fluidRow(
              shiny::column(width = 12,
                     shinydashboard::box(title = tagList(shiny::icon("tachometer-alt", lib = "font-awesome"), "TBATS Configurations"),
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         collapsed = TRUE,
                         width = 12,
                         background = "aqua",
                         shiny::fluidRow(shiny::column(width = 12,tags$br())),

                         # Regular TBATS Terms----
                         shiny::column(width = 3, shiny::uiOutput("TBATS_Lags")),
                         shiny::column(width = 3, shiny::uiOutput("TBATS_MovingAverages")),

                         # -- ADD SPACE ----
                         shiny::fluidRow(shiny::column(width = 12,tags$br()))))),

            # -- ADD SPACE ----
            shiny::fluidRow(shiny::column(width = 12,tags$br())),

            # ARFIMA ARGS----
            shiny::fluidRow(
              shiny::column(width = 12,
                     shinydashboard::box(title = tagList(shiny::icon("tachometer-alt", lib = "font-awesome"), "ARFIMA Configurations"),
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         collapsed = TRUE,
                         width = 12,
                         background = "aqua",
                         shiny::fluidRow(shiny::column(width = 12,tags$br())),

                         # Regular ARFIMA Terms ----
                         shiny::column(width = 3, shiny::uiOutput("ARFIMA_Lags")),
                         shiny::column(width = 3, shiny::uiOutput("ARFIMA_MovingAverages")),

                         # -- ADD SPACE ----
                         shiny::fluidRow(shiny::column(width = 12, tags$br())))))))),

      # Time Series Model Evaluation Tab -----
      tabItem(

        # Name of tab being used ----
        tabName = "automated_forecasting_model_evaluation",

        # -- TITLE ----
        tags$h1("Time Series"),
        tags$h3("Model Evaluation and Deployment"),

        # -- ADD SPACE ----
        shiny::fluidRow(shiny::column(width = 12,tags$br())),

        # -- ADD SPACE ----
        shiny::fluidRow(shiny::column(width = 12,tags$br())),

        # -- NEXT PREVIOUS BUTTONS ----
        shiny::fluidRow(
          shiny::column(
            width = 8,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "link_to_automated_forecasting_autots_autocarma_1",
              label = "Time Series Forecasting",
              icon = icon("chevron-left", lib = "font-awesome"),
              style = "gradient",
              color = "royal")),
          shiny::column(
            width = 4,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "link_to_automated_forecasting_deploy_model",
              label = "Deploy Model",
              icon = icon("chevron-right", lib = "font-awesome"),
              style = "gradient",
              color = "royal"))),

        # -- ADD SPACE ----
        shiny::fluidRow(shiny::column(width = 12,tags$br())),

        # Tab Panels ----
        shiny::tabsetPanel(

          # Tab types
          type = "tabs",

          # Forecast Plot Panel ----
          shiny::tabPanel(

            # -- TAB TITLE ----
            title = "Forecast Plot",

            # -- TAB ICON ----
            icon = shiny::icon("atom", lib = "font-awesome"),

            # -- ADD SPACE ----
            shiny::fluidRow(shiny::column(width = 12,tags$br())),

            # Target, Model, GroupVar Selections ----
            shiny::fluidRow(
              shiny::column(
                width = 12,
                shiny::fluidRow(shiny::column(width = 12,tags$br())),
                shinydashboard::box(
                  title = tagList(shiny::icon("filter", lib = "font-awesome"), "Select variables"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  background = "navy",
                  width = 12,

                  # -- ADD SPACE ----
                  shiny::fluidRow(shiny::column(width = 12,tags$br())),

                  # Model Input Variables ----
                  shiny::column(width = 3, shiny::uiOutput("Evaluate")),
                  shiny::column(width = 3, shiny::uiOutput("TS_ModelID")),
                  shiny::column(width = 3, shiny::uiOutput("TSEval_timeSeriesTarget2")),
                  shiny::column(width = 3, shiny::uiOutput("TSEval_timeSeriesGroupVars2"))))),

            # -- ADD SPACE ----
            shiny::fluidRow(shiny::column(width = 12,tags$br())),

            # Group Variables and Levels Selection ----
            shiny::fluidRow(
              shiny::column(
                width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("filter", lib = "font-awesome"), "Select group levels"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  background = "blue",
                  width = 12,

                  # -- ADD SPACE ----
                  shiny::fluidRow(shiny::column(width = 12,tags$br())),

                  # Single Group Level Selection ----
                  shiny::column(
                    width = 4,
                    conditionalPanel(
                      condition = "length(input['TSEval_timeSeriesGroupVars']) >= 1",
                      shiny::uiOutput("TS_Group1Levels2"))),

                  # Two Group Level Selection ----
                  shiny::column(
                    width = 4,
                    conditionalPanel(
                      condition = "length(input['TSEval_timeSeriesGroupVars']) >= 2",
                      shiny::uiOutput("TS_Group2Levels2"))),

                  # Three Group Level Selection ----
                  shiny::column(
                    width = 4,
                    conditionalPanel(
                      condition = "1 < 0", #"length(input['TSEval_timeSeriesGroupVars']) >= 3",
                      shiny::uiOutput("TS_Group3Levels2")))))),

            # UI Forecast Chart ----
            shiny::fluidRow(
              shiny::column(
                width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("filter", lib = "font-awesome"), "Model Evaluation Plot"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  background = "aqua",
                  width = 12,
                  shiny::column(
                    width = 1,
                    shinyWidgets::dropdown(
                      tags$h5("Time Series Plot Inputs"),
                      tags$h5(tags$style('h5 {color:darkblue;}')),
                      tags$h5(tags$style('#FC_AggregateFunction {color:darkblue;}')),
                      tags$h5(tags$style('#FC_OtherGroups {color:darkblue;}')),
                      tags$h5(tags$style('#FC_NumberGroupsDisplay {color:darkblue;}')),
                      style = "pill", inputId = "Something",
                      icon = icon("gear"),
                      tooltip = tooltipOptions(title = "Click to see plot options"),
                      status = "default", width = "300px",
                      shiny::uiOutput("FC_AggregateFunction"),
                      shiny::uiOutput("FC_OtherGroups"),
                      shiny::uiOutput("FC_NumberGroupsDisplay"))),

                  # Button Build Plot ----
                  shiny::column(
                    width = 9,
                    shiny::fluidRow(
                      shinyjs::useShinyjs(),
                      shinyWidgets::actionBttn(
                        inputId = "TS_BuildForecastPlot",
                        label = "Create Time Series Plot",
                        style = "gradient",
                        color = "royal"))),
                  shiny::column(
                    width = 2,
                    shiny::fluidRow(
                      shinyjs::useShinyjs(),
                      shinyWidgets::actionBttn(
                        inputId = "TS_ResetPlotSettings",
                        label = "Reset Plot",
                        style = "gradient",
                        color = "primary"))),

                  # -- ADD SPACE ----
                  shiny::fluidRow(shiny::column(width = 12,tags$br())),

                  # Time Series Plot ----
                  shiny::column(
                    width = 12,
                    shiny::plotOutput("TS_ForecastPlot", width = "100%"))))),
                    #plotly::plotlyOutput("TS_ForecastPlot", width = "100%"))))),

            # -- ADD SPACE ----
            shiny::fluidRow(shiny::column(width = 12,tags$br())),

            # Optional Plot Inputs----
            shiny::fluidRow(
              shiny::column(
                width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("database", lib = "font-awesome"), "Forecast plot Options"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  background = "aqua",
                  width = 12,

                  # -- ADD SPACE ----
                  shiny::fluidRow(shiny::column(width = 12,tags$br())),

                  # UI Plot Options----
                  shiny::fluidRow(
                    shiny::column(width = 3, shiny::uiOutput("FC_PredictionIntervals"), tags$br()),
                    shiny::column(width = 3, shiny::uiOutput("FC_ForecastLineColor"), tags$br()),
                    shiny::column(width = 3, shiny::uiOutput("FC_PredictionIntervalColorInner"), tags$br()),
                    shiny::column(width = 3, shiny::uiOutput("FC_PredictionIntervalColorOuter"), tags$br())),
                  shiny::fluidRow(
                    shiny::column(width = 3, shiny::uiOutput("FC_TickMarksX"), tags$br()),
                    shiny::column(width = 3, shiny::uiOutput("FC_LineWidth"), tags$br()),
                    shiny::column(width = 3, shiny::uiOutput("FC_AngleY"), tags$br()),
                    shiny::column(width = 3, shiny::uiOutput("FC_AngleX"), tags$br())),
                  shiny::fluidRow(
                    shiny::column(width = 3, shiny::uiOutput("FC_TextSize"), tags$br()),
                    shiny::column(width = 3, shiny::uiOutput("FC_TextColor"), tags$br()),
                    shiny::column(width = 3, shiny::uiOutput("FC_LineColor"), tags$br()),
                    shiny::column(width = 3, shiny::uiOutput("FC_LegendTextColor"), tags$br())),
                  shiny::fluidRow(
                    shiny::column(width = 3, shiny::uiOutput("FC_ChartColor"), tags$br()),
                    shiny::column(width = 3, shiny::uiOutput("FC_GridColor"), tags$br()),
                    shiny::column(width = 3, shiny::uiOutput("FC_BackGroundColor"), tags$br()),
                    shiny::column(width = 3, shiny::uiOutput("FC_BorderColor"))))))),

          # Model Selection and Required Inputs ----
          shiny::tabPanel(

            # -- TAB TITLE ----
            title = "Model Summaries",

            # -- TAB ICON ----
            icon = shiny::icon("atom", lib = "font-awesome"),

            # -- ADD SPACE ----
            shiny::fluidRow(shiny::column(width = 12,tags$br())),

            # Target, Model, GroupVar Selections ----
            shiny::fluidRow(
              shiny::column(
                width = 12,
                shiny::fluidRow(shiny::column(width = 12,tags$br())),
                shinydashboard::box(
                  title = tagList(shiny::icon("filter", lib = "font-awesome"), "Select variables"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  background = "navy",
                  width = 12,

                  # -- ADD SPACE ----
                  shiny::fluidRow(shiny::column(width = 12,tags$br())),

                  # Model Input Variables ----
                  shiny::column(
                    width = 4,
                    shiny::uiOutput("TSEval_timeSeriesTarget")),
                  shiny::column(
                    width = 4,
                    shiny::uiOutput("TSEval_timeSeriesGroupVars"))))),

            # -- ADD SPACE ----
            shiny::fluidRow(shiny::column(width = 12,tags$br())),

            # -- ADD SPACE ----
            shiny::fluidRow(shiny::column(width = 12,tags$br())),

            # UI DataTable Model Metrics ----
            shiny::fluidRow(
              shiny::column(
                width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("filter", lib = "font-awesome"), "Model Evaluation Metrics"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  background = "aqua",
                  width = 12,

                  tags$style(
                    HTML(".dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing, .dataTables_wrapper .dataTables_paginate {
                        color: #ffffff;
                        }
                        thead {
                        color: #ffffff;
                        }
                        tbody {
                        color: #000000;
                        }")),

                  # Button Model Evaluation ----
                  shiny::fluidRow(
                    shiny::column(
                      width = 12,
                      align="center",
                      shiny::column(
                        width = 3,
                        align="center",
                        shinyWidgets::actionBttn(
                          inputId = "TS_CollectModelMetrics",
                          label = "Create Performance Metrics",
                          icon = icon("power-on", lib = 'font-awesome'),
                          style = "gradient",
                          color = "royal",
                          size = "md")))),

                  # --ADD SPACE ----
                  shiny::fluidRow(shiny::column(width = 12,tags$br())),

                  # DataTable Output ----
                  shiny::column(
                    width = 12,
                    DT::DTOutput(outputId = "TS_ModelMetrics", width = "100%")))))))),

      # Time Series Model Deployment Tab -----
      tabItem(

        # Name of tab being used ----
        tabName = "automated_forecasting_deploy_model",

        tags$h3("This is where the time series model deployment content goes")),

      #----

      #----

      #----

      #----

      # AUTOMATED INVENTORY OPTIMIZATION TABS----

      #----

      # ID Optimization Create Project Tab----
      tabItem(

        # --TAB REFERENCE VALUE----
        tabName = "automated_inventory_optimization_project_creation",

        # --PAGE TITLE----
        tags$h1("Create or Open Project"),

        # --ADD SPACE----
        shiny::fluidRow(column(width = 12,tags$br())),

        # --NEXT PREVIOUS BUTTONS----
        shiny::fluidRow(
          column(
            width = 8),
          column(
            width = 4,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "link_to_automated_inventory_optimization_data_import",
              label = "Go To Data Import",
              icon = icon("chevron-right", lib = "font-awesome"),
              style = "gradient",
              color = "royal"))),

        # --ADD SPACE----
        shiny::fluidRow(column(width = 12,tags$br())),

        # --ADD SPACE----
        shiny::fluidRow(column(width = 12,tags$br())),

        # Create or Open Project UI----
        shiny::tabsetPanel(

          # Tab types
          type = "tabs", # "tabs","pills"

          # Create Project----
          shiny::tabPanel(

            # --TAB TITLE----
            title = "Create Project",

            # --TAB ICON----
            icon = shiny::icon("layer-group", lib = "font-awesome"),

            # --ADD SPACE----
            shiny::fluidRow(column(width = 12,tags$br())),

            # UI Elements for Creating Project----
            shiny::fluidRow(
              column(
                width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("drafting-compass", lib = "font-awesome"), "Create Project"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  background = "navy",
                  width = 12,

                  # --ADD SPACE----
                  shiny::fluidRow(column(width = 12,tags$br())),

                  # UI for Suppling Name of New Project----
                  column(
                    width = 12,
                    shiny::uiOutput("ID_NewProjectName"),
                    tags$br()),

                  # --ADD SPACE----
                  shiny::fluidRow(column(width = 12,tags$br())),

                  # UI for Supply Path to Folder to Create Project----
                  column(
                    width = 12,
                    shiny::uiOutput("ID_Root_Folder"),
                    tags$br())))),

            # --ADD SPACE----
            shiny::fluidRow(column(width = 12,tags$br())),

            # Action button to create file directory----
            shiny::fluidRow(
              column(
                width = 4,
                shinyjs::useShinyjs(),
                shinyWidgets::actionBttn(
                  inputId = "ID_CreateProject",
                  label = "Press to Create Project",
                  icon = icon("plus", lib = "font-awesome"),
                  style = "gradient",
                  color = "success")))),

          # Open Project----
          shiny::tabPanel(

            # --TAB TITLE----
            title = "Open Project",

            # --TAB ICON----
            icon = shiny::icon("edit", lib = "font-awesome"),

            # --ADD SPACE----
            shiny::fluidRow(column(width = 12,tags$br())),

            # UI elements for importing data----
            shiny::fluidRow(
              column(
                width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("upload", lib = "font-awesome"), "Open Existing Project"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  background = "navy",
                  width = 12,

                  # --ADD SPACE----
                  shiny::fluidRow(column(width = 12,tags$br())),

                  # UI for Supply Path to Folder to Create Project----
                  column(
                    width = 10,
                    shiny::uiOutput("ProjectListUpload"),
                    tags$br())))),

            # Action button to open ProjectList----
            shiny::fluidRow(
              column(
                width = 4,
                shinyjs::useShinyjs(),
                shinyWidgets::actionBttn(
                  inputId = "ID_OpenProject",
                  label = "Press to Open Project",
                  icon = icon("folder-open", lib = 'font-awesome'),
                  style = "gradient",
                  color = "success")))),

          # HELP PAGE----
          shiny::tabPanel(

            # --TAB TITLE----
            title = "Help",

            # --TAB ICON----
            icon = shiny::icon("ambulance", lib = "font-awesome"),

            # --ADD SPACE----
            shiny::fluidRow(column(width = 12,tags$br())),

            # UI elements for importing data----
            shiny::fluidRow(
              column(
                width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("upload", lib = "font-awesome"), "Open Existing Project"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  background = "light-blue",
                  width = 12,

                  # --ADD SPACE----
                  shiny::fluidRow(column(width = 12,tags$br())),

                  # Help with Name Your Project Input----
                  tags$h4("< Name Your Project >"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),">",HTML('&nbsp'),"The name of your project will the name of the folder containing all the folders with project files."))),

                  # --ADD SPACE----
                  shiny::fluidRow(column(width = 12,tags$br())),

                  # Help with selecting the target variable----
                  tags$h4("< Supply Path File To Root Directory Folder Where Project Folders will be Created >"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),">",HTML('&nbsp'),"Supply a path file (no quotes around them) to the location where you want your project stored."))))))))),

      # ID Optimization Data Import Tab----
      tabItem(

        # --TAB REFERENCE VALUE----
        tabName = "automated_inventory_optimization_data_import",

        # --PAGE TITLE----
        tags$h1("Import and Prepare Data"),

        # --ADD SPACE----
        shiny::fluidRow(column(width = 12,tags$br())),

        # --NEXT PREVIOUS BUTTONS----
        shiny::fluidRow(
          column(
            width = 8,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "link_to_automated_inventory_optimization_project_creation",
              label = "Create or Open Project",
              icon = icon("chevron-left", lib = "font-awesome"),
              style = "gradient",
              color = "royal")),
          column(
            width = 4,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "link_to_automated_inventory_optimization_data_construction",
              label = "Data Construction",
              icon = icon("chevron-right", lib = "font-awesome"),
              style = "gradient",
              color = "royal"))),

        # --ADD SPACE----
        shiny::fluidRow(column(width = 12,tags$br())),

        # --ADD SPACE----
        shiny::fluidRow(column(width = 12,tags$br())),

        # Required Inputs and HELP PAGE----
        shiny::tabsetPanel(

          # Tab types
          type = "tabs", # "tabs","pills"

          # TAB IMPORT EXISTING DATA----
          shiny::tabPanel(

            # --TAB TITLE----
            title = "Import Existing Data",

            # --TAB ICON----
            icon = shiny::icon("atom", lib = "font-awesome"),

            # --ADD SPACE----
            shiny::fluidRow(column(width = 12,tags$br())),

            # UI elements for importing data----
            shiny::fluidRow(
              column(
                width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("database", lib = "font-awesome"), "Location of Source Data"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  background = "navy",
                  width = 12,

                  # --ADD SPACE----
                  shiny::fluidRow(column(width = 12,tags$br())),

                  # File Path Input----
                  column(
                    width = 12,
                    shiny::uiOutput("ID_SourceData"),
                    tags$br()),
                  column(
                    width = 6,
                    shiny::uiOutput("Creation_Date_Source_Data"),
                    tags$br()),
                  column(
                    width = 6,
                    shiny::uiOutput("Days_Since_Source_Data_Creation"),
                    tags$br())))),

            # --ADD_SPACE----
            shiny::fluidRow(column(width = 12,tags$br())),

            shiny::fluidRow(
              column(
                width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("database", lib = "font-awesome"), "Location of Modeling Data"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  background = "navy",
                  width = 12,

                  # --ADD SPACE----
                  shiny::fluidRow(column(width = 12,tags$br())),

                  # --ADD SPACE----
                  shiny::fluidRow(column(width = 12,tags$br())),

                  # File Path Input----
                  column(
                    width = 12,
                    shiny::uiOutput("ID_ExistingModelDataPath"),
                    tags$br()),
                  column(
                    width = 6,
                    shiny::uiOutput("Days_Since_ID_CountSizeData_Creation"),
                    tags$br()),
                  column(
                    width = 6,
                    shiny::uiOutput("Creation_Date_ID_CountSizeData"),
                    tags$br())))),

            # --ADD SPACE----
            shiny::fluidRow(column(width = 12,tags$br())),

            # Action Button to Store Data in Data Folder----
            shiny::fluidRow(
              column(
                width = 6,
                shinyjs::useShinyjs(),
                shinyWidgets::actionBttn(
                  inputId = "ID_ImportSourceData",
                  label = "Import Existing Data",
                  icon = icon("file-import", lib = "font-awesome"),
                  style = "gradient",
                  color = "success")))),

          # TAB IMPORT NEW DATA----
          shiny::tabPanel(

            # --TAB TITLE----
            title = "Import New Data",

            # --TAB ICON----
            icon = shiny::icon("atom", lib = "font-awesome"),

            # --ADD SPACE----
            shiny::fluidRow(column(width = 12,tags$br())),

            # UI elements for importing data----
            shiny::fluidRow(
              column(
                width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("database", lib = "font-awesome"), "Import New Transactional Data"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  background = "navy",
                  width = 12,

                  # --ADD SPACE----
                  shiny::fluidRow(column(width = 12,tags$br())),

                  # UI Import Functionality----
                  shiny::fileInput(
                    inputId = "InventoryOptimizationData",
                    label =  "Choose CSV File",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv"))))),

            # --ADD SPACE----
            shiny::fluidRow(column(width = 12,tags$br())),

            # --ADD SPACE----
            shiny::fluidRow(column(width = 12,tags$br())),

            # UI elements for selecting data columns for subsetting----
            shiny::fluidRow(
              column(
                width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("filter", lib = "font-awesome"), "Identify Modeling Variables"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  background = "navy",
                  width = 12,

                  # --ADD SPACE----
                  shiny::fluidRow(column(width = 12,tags$br())),

                  # Model Input Variables----
                  column(
                    width = 4,
                    shiny::uiOutput("InventoryOptimizationTarget")),
                  column(
                    width = 4,
                    shiny::uiOutput("InventoryOptimizationDateVariable")),
                  column(
                    width = 4,
                    shiny::uiOutput("InventoryOptimizationGroupVariables"))))),

            # --ADD SPACE----
            shiny::fluidRow(column(width = 12,tags$br())),

            # Action Button to Store Data in Data Folder----
            shiny::fluidRow(
              column(
                width = 6,
                shinyjs::useShinyjs(),
                shinyWidgets::actionBttn(
                  inputId = "ID_SaveDataToDataFolder",
                  label = "Save Data and Selections",
                  icon = icon("save", lib = "font-awesome"),
                  style = "gradient",
                  color = "success")))),

          # HELP PAGE----
          shiny::tabPanel(

            # --TAB TITLE----
            title = "Help",

            # --TAB ICON----
            icon = shiny::icon("ambulance", lib = "font-awesome"),

            # --ADD SPACE----
            shiny::fluidRow(column(width = 12,tags$br())),

            # HELP PAGE----
            shiny::fluidRow(
              column(
                width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("filter", lib = "font-awesome"), "Inputs Explanations"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  background = "light-blue",
                  width = 12,

                  # --ADD SPACE----
                  shiny::fluidRow(column(width = 12,tags$br())),

                  # Help with import button----
                  tags$h4("* Choose CSV File:"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),">",HTML('&nbsp'),"Click the Browse... button to open a window to select your transactions data file"))),

                  # --ADD SPACE----
                  shiny::fluidRow(column(width = 12,tags$br())),

                  # Help with selecting the target variable----
                  tags$h4("* Select Target Variable"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),">",HTML('&nbsp'),"Click the down arrow to select the column names of the variables you will be forecasting"))),

                  # --ADD SPACE----
                  shiny::fluidRow(column(width = 12,tags$br())),

                  # Help with selecting the date variable----
                  tags$h4("* Select Date Variable"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),">",HTML('&nbsp'),"Click the down arrow to select the date column in the data"))),

                  # --ADD SPACE----
                  shiny::fluidRow(column(width = 12,tags$br())),

                  # Help with selecting the group variable----
                  tags$h4("* Select Group Variables"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),">",HTML('&nbsp'),"Click the down arrow to select group columns in the data"))))))))),

      #----

      # ID Optimization Data Construction Tab----
      tabItem(

        # --TAB REFERENCE VALUE----
        tabName = "automated_inventory_optimization_data_construction",

        # --PAGE TITLE----
        tags$h1("Create Modeling Data"),

        # --ADD SPACE----
        shiny::fluidRow(column(width = 12,tags$br())),

        # --NEXT PREVIOUS BUTTONS----
        shiny::fluidRow(
          column(
            width = 8,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "link_to_automated_inventory_optimization_data_import_1",
              label = "Data Import",
              icon = icon("chevron-left", lib = "font-awesome"),
              style = "gradient",
              color = "royal")), #"success"),#"royal"),#"warning"),#"danger"),#"default"),
          column(
            width = 4,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "link_to_automated_inventory_optimization_modeling",
              label = "Demand Modeling",
              icon = icon("chevron-right", lib = "font-awesome"),
              style = "gradient",
              color = "royal"))), #"success"),#"royal"),#"warning"),#"primary"),#"default"),

        # --ADD SPACE----
        shiny::fluidRow(column(width = 12,tags$br())),

        # --ADD SPACE----
        shiny::fluidRow(column(width = 12,tags$br())),

        # Feature Engineering Parameters for RemixAutoML::IntermittentDemandDataGenerator----
        shiny::tabsetPanel(

          # Tab types
          type = "tabs", # "tabs","pills"

          # Required Options Tab----
          shiny::tabPanel(

            # Tab Title----
            title = "Required Inputs",

            # Tab Icon
            icon = shiny::icon("flag", lib = "font-awesome"),

            # --ADD SPACE----
            shiny::fluidRow(column(width = 12,tags$br())),

            # File path argument box----
            shiny::fluidRow(
              column(
                width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("copy", lib = "font-awesome"), "File Location to Save Data"),
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  background = "navy", #"aqua", #"lime", #"navy", #"fuchsia", #"blue", #"maroon", #"purple", #"light-blue", #"yellow", #"red",#yellow, #"olive", #"orange", #"black", #"green",

                  # --ADD SPACE----
                  shiny::fluidRow(column(width = 12,tags$br())),

                  # File Path Input----
                  column(
                    width = 10,
                    shiny::uiOutput("OutputFilePath"),
                    tags$br())))),

            # --ADD SPACE----
            shiny::fluidRow(column(width = 12,tags$br())),

            # Required Inputs----
            shiny::fluidRow(
              column(
                width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("edit", lib = "font-awesome"), "Required Data Construction Options"),
                  collapsed = FALSE,
                  tags$br(),
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  background = "navy", #"aqua", #"lime", #"navy", #"fuchsia", #"blue", #"maroon", #"purple", #"light-blue", #"yellow", #"red",#yellow, #"olive", #"orange", #"black", #"green",
                  column(
                    width = 4,
                    shiny::uiOutput("OutputTimeUnit"),
                    tags$br()),
                  column(
                    width = 4,
                    shiny::uiOutput("ID_FC_Periods"),
                    tags$br()),
                  column(
                    width = 4,
                    shiny::uiOutput("MinimumTransactions"),
                    tags$br()))))),

          # Advanced User Inputs----
          shiny::tabPanel(

            # Tab Title----
            title = "Advanced User Inputs",

            # Tab Icon
            icon = shiny::icon("chalkboard-teacher", lib = "font-awesome"),

            # --ADD SPACE----
            shiny::fluidRow(column(width = 12,tags$br())),

            # More inputs
            shiny::fluidRow(
              column(
                width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("edit", lib = "font-awesome"), "Sampling Options"),
                  collapsed = FALSE,
                  tags$br(),
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  background = "teal", #"aqua", #"lime", #"navy", #"fuchsia", #"blue", #"maroon", #"purple", #"light-blue", #"yellow", #"red",#yellow, #"olive", #"orange", #"black", #"green",
                  column(
                    width = 4,
                    shiny::uiOutput("PowerRate"),
                    tags$br()),
                  column(
                    width = 4,
                    shiny::uiOutput("SampleRate"),
                    tags$br()),
                  column(
                    width = 4,
                    shiny::uiOutput("TargetSamples"),
                    tags$br())))),

            # --ADD SPACE----
            shiny::fluidRow(column(width = 12,tags$br())),

            # Feature Engineering Options Box----
            shiny::fluidRow(
              column(
                width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("chalkboard-teacher", lib = "font-awesome"), "Feature Engineering Options"),
                  solidHeader = TRUE,
                  collapsed = FALSE,
                  tags$br(),
                  background = "teal",
                  width = 12,
                  collapsible = TRUE,
                  column(
                    width = 6,
                    shiny::uiOutput("Lags"),
                    tags$br()
                  ),
                  column(
                    width = 6,
                    shiny::uiOutput("MovingAverages"),
                    tags$br()))))),

          # View Data Sample Tab----
          shiny::tabPanel(

            # Tab title----
            title = "View Data Snapshot", icon = shiny::icon("database", lib = "font-awesome"),

            # --ADD SPACE----
            shiny::fluidRow(column(width = 12,tags$br())),

            # View Subsetted Data----
            shiny::tabPanel(
              title = "Time Series Data",
              icon = icon('database', lib = 'font-awesome'),
              tags$br(),
              shinydashboard::box(title = "Time Series Data Table",
                  solidHeader = TRUE,
                  width = 12,
                  background = "teal",
                  tags$style(
                    HTML(".dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing, .dataTables_wrapper .dataTables_paginate {
                                   color: #ffffff;
                                   }
                                   thead {
                                   color: #ffffff;
                                   }
                                   tbody {
                                   color: #000000;
                                   }")),
                  DT::DTOutput(outputId = "SubsetData", width = "100%")))),

          # HELP PAGE----
          shiny::tabPanel(

            # Tab Title----
            title = "Help",

            # Tab Icon
            icon = shiny::icon("ambulance", lib = "font-awesome"),

            # A--DD SPACE----
            shiny::fluidRow(column(width = 12,tags$br())),

            # UI elements for importing data----
            shiny::fluidRow(
              column(
                width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("flag", lib = "font-awesome"), "Required Inputs"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  background = "light-blue",
                  width = 12,

                  # --ADD SPACE----
                  shiny::fluidRow(column(width = 12,tags$br())),

                  # File path to save data----
                  tags$h4("< File Path to Save Data >"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),"*",HTML('&nbsp'),"Paste in the file path to the folder where you will want the two modeling data sets saved"))),

                  # --ADD SPACE----
                  shiny::fluidRow(column(width = 12,tags$br())),

                  # Time unit selection----
                  tags$h4("< Time Unit Selection >"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),"*",HTML('&nbsp'),"Click the down arrow to select the column names of the variables you will be forecasting"))),

                  # --ADD SPACE----
                  shiny::fluidRow(column(width = 12,tags$br())),

                  # Selecting forecast periods----
                  tags$h4("< Select Forecast Periods >"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),"*",HTML('&nbsp'),"Click the down arrow to select the date column in the data"))),

                  # --ADD SPACE----
                  shiny::fluidRow(column(width = 12,tags$br())),

                  # Min Transactions----
                  tags$h4("< Min Transactions >"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),"*",HTML('&nbsp'),"Select the minimum number of transactions required to keep a 'sku' in the data."))),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),"*",HTML('&nbsp'),"E.g. sku's with less than N transactions will not be included in the modeling data sets")))))),

            # --ADD SPACE----
            shiny::fluidRow(column(width = 12,tags$br())),

            # UI elements for importing data----
            shiny::fluidRow(
              column(
                width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("chalkboard-teacher", lib = "font-awesome"), "Advanced User Options Explanations"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  background = "light-blue",
                  width = 12,

                  # --ADD SPACE----
                  shiny::fluidRow(column(width = 12,tags$br())),

                  # Power Sample Rate----
                  tags$h4("< Power Sample Rate >"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),"*",HTML('&nbsp'),"Transactions ^ (Power Sample Rate). This helps to balance the group variables."))),

                  # --ADD SPACE----
                  shiny::fluidRow(column(width = 12,tags$br())),

                  # Sample Rate----
                  tags$h4("< Sample Rate >"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),"*",HTML('&nbsp'),"(Sample Rate) * Transactions ^ (Power Sample Rate)"))),

                  # --ADD SPACE----
                  shiny::fluidRow(column(width = 12,tags$br())),

                  # Target Samples----
                  tags$h4("< Number of Target Samples >"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),"*",HTML('&nbsp'),"The number of target instances generated per single iteration of the sampling."))),

                  # --ADD SPACE----
                  shiny::fluidRow(column(width = 12,tags$br())),

                  # Max Lags----
                  tags$h4("< Max Moving Averages >"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),"*",HTML('&nbsp'),"The maximum number of target moving averages to utilize. Creates 1 up to Max Moving Averages features.")))))))),

        # --ADD SPACE----
        shiny::fluidRow(column(width = 12,tags$br())),

        # --ADD SPACE----
        shiny::fluidRow(column(width = 12,tags$br())),

        # Data Generator Button and Save Settings Button----
        shiny::fluidRow(
          column(
            width = 8,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "ID_DataGeneratorSaveSettings",
              label = "Save Settings",
              icon = icon("save", lib = "font-awesome"),
              style = "gradient",
              color = "success")),
          column(
            width = 4,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "StartDataGenerator",
              label = "Build Data Sets",
              icon = icon("screwdriver", lib = "font-awesome"),
              style = "gradient",
              color = "primary")))),

      #----

      # ID Optimization Demand Model Building Tab----
      tabItem(

        # --TAB REFERENCE VALUE----
        tabName = "automated_inventory_optimization_modeling",

        # --PAGE TITLE----
        tags$h1("Machine Learning Model Creation"),

        # --ADD SPACE----
        shiny::fluidRow(column(width = 12,tags$br())),

        # --NEXT PREVIOUS BUTTONS----
        shiny::fluidRow(
          column(
            width = 8,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "link_to_automated_inventory_optimization_data_construction_1",
              label = "Data Construction",
              icon = icon("chevron-left", lib = "font-awesome"),
              style = "gradient",
              color = "royal")), #"success"),#"royal"),#"warning"),#"danger"),#"default"),
          column(
            width = 4,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "link_to_automated_inventory_optimization_forecasting",
              label = "Demand Forecasting",
              icon = icon("chevron-right", lib = "font-awesome"),
              style = "gradient",
              color = "royal"))), #"success"),#"royal"),#"warning"),#"primary"),#"default"),),

        # --ADD SPACE----
        shiny::fluidRow(column(width = 12,tags$br())),

        # --ADD SPACE----
        shiny::fluidRow(column(width = 12,tags$br())),

        # Forecast Arguments for RemixAutoML::AutoH2oGBMSizeFreqDist----
        shiny::tabsetPanel(

          # Tab types
          type = "tabs", # "tabs","pills"

          # Required Options Tab----
          shiny::tabPanel(

            # Tab Title----
            title = "Required Inputs",

            # Tab Icon
            icon = shiny::icon("flag", lib = "font-awesome"),

            # --ADD SPACE----
            shiny::fluidRow(column(width = 12,tags$br())),

            # Arguments set 1 RemixAutoML::AutoH2oGBMSizeFreqDist()----
            shiny::fluidRow(
              column(
                width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("server", lib = "font-awesome"), "Directory Inputs"),
                  p("* Path files are supplied if you created or imported a project"), tags$br(),
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  background = "navy", #"aqua", #"lime", #"navy", #"fuchsia", #"blue", #"maroon", #"purple", #"light-blue", #"yellow", #"red",#yellow, #"olive", #"orange", #"black", #"green",
                  column(
                    width = 4,
                    uiOutput("ID_ModelDataPath"),
                    tags$br()
                  ),
                  column(
                    width = 4,
                    uiOutput("ID_ModelPath"),
                    tags$br()
                  ),
                  column(
                    width = 4,
                    uiOutput("ID_MetaDataPath"),
                    tags$br())))),

            # --ADD SPACE----
            shiny::fluidRow(column(width = 12,tags$br())),

            # Arguments set 2 RemixAutoML::AutoH2oGBMSizeFreqDist()----
            shiny::fluidRow(
              column(
                width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("microchip", lib = "font-awesome"), "Compute Resources"),
                  tags$br(),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  background = "navy",
                  width = 12,
                  column(
                    width = 6,
                    uiOutput("ID_MaxMem"),
                    tags$br()
                  ),
                  column(
                    width = 6,
                    uiOutput("ID_NThreads"),
                    tags$br()))))),

          # Advanced User Options Tab----
          shiny::tabPanel(

            # Tab Title----
            title = "Advanced User Inputs",

            # Tab Icon
            icon = shiny::icon("chalkboard-teacher", lib = "font-awesome"),

            # --ADD SPACE----
            shiny::fluidRow(column(width = 12,tags$br())),

            # Quantile Selection Args for RemixAutoML::AutoH2oGBMSizeFreqDist()----
            shiny::fluidRow(
              column(
                width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("dolly", lib = "font-awesome"), "Quantile Regression Models Selection"),
                  tags$br(),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  background = "teal",
                  width = 12,
                  column(
                    width = 4,
                    uiOutput("ID_CountQuantiles"),
                    tags$br()),
                  column(
                    width = 4,
                    uiOutput("ID_SizeQuantiles"),
                    tags$br()
                  ),
                  column(
                    width = 4,
                    uiOutput("ID_GridTune"),
                    tags$br())))),

            # --ADD SPACE----
            shiny::fluidRow(column(width = 12,tags$br())),

            # Other Args for RemixAutoML::AutoH2oGBMSizeFreqDist()----
            shiny::fluidRow(
              column(
                width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("chalkboard-teacher", lib = "font-awesome"), "Model Parameters"),
                  solidHeader = TRUE,
                  tags$br(),
                  collapsible = TRUE,
                  background = "teal",
                  width = 12,
                  column(
                    width = 4,
                    uiOutput("ID_NTrees"),
                    tags$br()
                  ),
                  column(
                    width = 4,
                    uiOutput("ID_NumModelsTest"),
                    tags$br()
                  ),
                  column(
                    width = 4,
                    uiOutput("StratifyTargets"),
                    tags$br())))),

            # --ADD SPACE----
            shiny::fluidRow(column(width = 12,tags$br())),

            # Data Partition Args for RemixAutoML::AutoH2oGBMSizeFreqDist()----
            shiny::fluidRow(
              column(
                width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("clone", lib = "font-awesome"), "Data Partitioning"),
                  tags$br(),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  background = "teal",
                  width = 12,
                  column(
                    width = 4,
                    uiOutput("ID_DataPartitionsTrain"),
                    tags$br()),
                  column(
                    width = 4,
                    uiOutput("ID_DataPartitionsValidate"),
                    tags$br()),
                  column(
                    width = 4,
                    uiOutput("ID_DataPartitionsTest"),
                    tags$br()))))),

          # HELP PAGE Tab----
          shiny::tabPanel(

            # Tab Title----
            title = "Help",

            # Tab Icon
            icon = shiny::icon("ambulance", lib = "font-awesome"),

            # --ADD SPACE----
            shiny::fluidRow(column(width = 12,tags$br())),

            # HELP PAGE Required Inputs----
            shiny::fluidRow(
              column(
                width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("server", lib = "font-awesome"), "Required Inputs"),
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  background = "light-blue", #"aqua", #"lime", #"navy", #"fuchsia", #"blue", #"maroon", #"purple", #"light-blue", #"yellow", #"red",#yellow, #"olive", #"orange", #"black", #"green",

                  # --ADD SPACE----
                  shiny::fluidRow(shiny::column(width = 12,tags$br())),

                  # Path to training data----
                  tags$h4("< Paths to training data >"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),"*",HTML('&nbsp'),"Paste in the file path to the folder where you saved the modeling data in the data construction tab"))),

                  # --ADD SPACE----
                  shiny::fluidRow(shiny::column(width = 12,tags$br())),

                  # Path to model storage----
                  tags$h4("< Path to Model Storage >"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),"*",HTML('&nbsp'),"Paste in the file path to the folder where you want your models saved. Folder must exist before building the models."))),

                  # --ADD SPACE----
                  shiny::fluidRow(shiny::column(width = 12,tags$br())),

                  # Path to metadata storage----
                  tags$h4("< File Path to Metadata Storage >"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),"*",HTML('&nbsp'),"Paste in the file path to the folder where you want model metadata saved. Folder must exist before building the models."))),

                  # --ADD SPACE----
                  shiny::fluidRow(shiny::column(width = 12,tags$br())),

                  # Max Memory----
                  tags$h4("< Maximum Memory >"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),"*",HTML('&nbsp'),"Paste in the amount of memory you would like to dedicate to the model building process."))),

                  # --ADD SPACE----
                  shiny::fluidRow(shiny::column(width = 12,tags$br())),

                  # CPU Threads----
                  tags$h4("< CPU Threads >"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),"*",HTML('&nbsp'),"Paste in the number of threads (typically number of cores * 2) you want to use for the model building.")))))),

            # --ADD SPACE----
            shiny::fluidRow(shiny::column(width = 12,tags$br())),

            # HELP PAGE Advanced Inputs----
            shiny::fluidRow(
              shiny::column(
                width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("chalkboard-teacher", lib = "font-awesome"), "Advanced User Inputs"),
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  background = "light-blue", #"aqua", #"lime", #"navy", #"fuchsia", #"blue", #"maroon", #"purple", #"light-blue", #"yellow", #"red",#yellow, #"olive", #"orange", #"black", #"green",

                  # --ADD SPACE----
                  shiny::fluidRow(shiny::column(width = 12,tags$br())),

                  # Size Quantiles----
                  tags$h4("< Select the size quantiles >"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),"*",HTML('&nbsp'),"The size quantiles selection lets the program know which quantile regression models to build for the size given demand models."))),

                  # --ADD SPACE----
                  shiny::fluidRow(shiny::column(width = 12,tags$br())),

                  # Grid Tune----
                  tags$h4("< Run Grid Tune >"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),"*",HTML('&nbsp'),"Set this option to TRUE to grid tune each of the quantile regression model builds. Use this in conjunction with # of Models in Grid Tune. This will slow down the building process significantly."))),

                  # --ADD SPACE----
                  shiny::fluidRow(shiny::column(width = 12,tags$br())),

                  # Number of Trees----
                  tags$h4("< Number of Trees >"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),"*",HTML('&nbsp'),"To override the default setting, type in the number of trees you want built for each quantile regression model. Early stopping is applied so it's better to select more."))),

                  # --ADD SPACE----
                  shiny::fluidRow(shiny::column(width = 12,tags$br())),

                  # Models in Grid----
                  tags$h4("< # of Models in Grid Tune >"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),"*",HTML('&nbsp'),"Select the number of models you want tested in the grid tuning procedure."))),

                  # --ADD SPACE----
                  shiny::fluidRow(shiny::column(width = 12,tags$br())),

                  # Stratify Target Variable----
                  tags$h4("< Stratify Target Variable >"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),"*",HTML('&nbsp'),"For highly skewed data, this feature will ensure enough samples from the target distributions will be represented in the train, validate, and test data sets."))),

                  # --ADD SPACE----
                  shiny::fluidRow(shiny::column(width = 12,tags$br())),

                  # Data Partition----
                  tags$h4("< Training Data %, Validation Data %, and Test Data % >"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),"*",HTML('&nbsp'),"Specify the percentages of data to be allocated to each data set.")))))))),

        # --ADD SPACE----
        shiny::fluidRow(shiny::column(width = 12,tags$br())),

        # --ADD SPACE----
        shiny::fluidRow(shiny::column(width = 12,tags$br())),

        # Run Model Builder Button----
        shiny::fluidRow(
          shiny::column(
            width = 8,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "ID_SaveModelSettings",
              label = "Save Settings",
              icon = icon("save", lib = "font-awesome"),
              style = "gradient",
              color = "success")),
          shiny::column(
            width = 4,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "BuildIDModels",
              label = "Press to Build Models",
              icon = icon("screwdriver", lib = "font-awesome"),
              style = "gradient",
              color = "primary")))),#"success"),#"danger"),#"warning"),#"primary"),#"default")))),

      #----

      # ID Optimization Demand Forecasting Tab----
      tabItem(

        # --TAB REFERENCE VALUE----
        tabName = "automated_inventory_optimization_forecasting",

        # --PAGE TITLE----
        tags$h1("Demand Forecasting"),

        # --ADD SPACE----
        shiny::fluidRow(column(width = 12,tags$br())),

        # --NEXT PREVIOUS BUTTONS----
        shiny::fluidRow(
          column(
            width = 8,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "link_to_automated_inventory_optimization_modeling_1",
              label = "Demand Modeling",
              icon = icon("chevron-left", lib = "font-awesome"),
              style = "gradient",
              color = "royal")), #"success"),#"royal"),#"warning"),#"danger"),#"default"),
          column(
            width = 4,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "link_to_automated_inventory_optimization_recommendations",
              label = "Optimization",
              icon = icon("chevron-right", lib = "font-awesome"),
              style = "gradient",
              color = "royal"))), #"success"),#"royal"),#"warning"),#"primary"),#"default"),),

        # --ADD SPACE----
        shiny::fluidRow(column(width = 12,tags$br())),

        # --ADD SPACE----
        shiny::fluidRow(column(width = 12,tags$br())),

        # UI elements for importing scoring data----
        shiny::tabsetPanel(

          # Tab types
          type = "tabs", # "tabs","pills"

          # Required Options Tab----
          shiny::tabPanel(

            # Tab Title----
            title = "Required Inputs",

            # Tab Icon
            icon = shiny::icon("flag", lib = "font-awesome"),

            # --ADD SPACE----
            shiny::fluidRow(column(width = 12,tags$br())),

            # Path to Saved Models and ScoringData----
            shiny::fluidRow(
              column(
                width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("copy", lib = "font-awesome"), "File Paths"),
                  solidHeader = TRUE,
                  tags$br(),
                  collapsible = TRUE,
                  background = "navy",#"aqua", #"lime", #"navy", #"fuchsia", #"blue", #"maroon", #"purple", #"light-blue", #"yellow", #"red",#yellow, #"olive", #"orange", #"black", #"green",
                  width = 12,
                  column(
                    width = 6,
                    uiOutput("ID_ModelPath_1"),
                    tags$br()),
                  column(
                    width = 6,
                    uiOutput("ID_PathSaveFCData"),
                    tags$br())))),

            # --ADD SPACE----
            shiny::fluidRow(column(width = 12,tags$br())),

            # Import Data for RemixAutoML::AutoFreqSizeDistScoringFunctions()----
            shiny::fluidRow(
              column(
                width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("download", lib = "font-awesome"), "Import New Transactional Data"),
                  p("* Use this when you have downloaded new source data"),
                  solidHeader = TRUE,
                  tags$br(),
                  collapsible = TRUE,
                  background = "navy", #"aqua", #"lime", #"navy", #"fuchsia", #"blue", #"maroon", #"purple", #"light-blue", #"yellow", #"red",#yellow, #"olive", #"orange", #"teal", #"green",
                  width = 12,

                  # UI Import Functionality----
                  shiny::fileInput(
                    inputId = "InventoryOptimizationScoringData",
                    label =  "Upload New Scoring Data",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")),

                  # Select Variable Columns----
                  column(
                    width = 4,
                    uiOutput("ID_ScoringTargetVariable")),
                  column(
                    width = 4,
                    uiOutput("ID_ScoringDateVariable")),
                  column(
                    width = 4,
                    uiOutput("ID_ScoringGroupVariable"))))),

            # --ADD SPACE----
            shiny::fluidRow(column(width = 12,tags$br())),

            # Forecasting Inputs----
            shiny::fluidRow(
              column(
                width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("chart-line", lib = "font-awesome"), "Forecasting Inputs"),
                  solidHeader = TRUE,
                  tags$br(),
                  collapsible = TRUE,
                  background = "navy",
                  width = 12,
                  column(
                    width = 4,
                    uiOutput("ID_TimeUnit_ScoringData"),
                    tags$br()),
                  column(
                    width = 4,
                    uiOutput("ID_FC_Periods_Scoring_Data"),
                    tags$br()),
                  column(
                    width = 4,
                    uiOutput("ID_CurrentDate_ScoringData"),
                    tags$br()))))),

          # Advanced User Options Tab----
          shiny::tabPanel(

            # Tab Title----
            title = "Advanced User Inputs",

            # Tab Icon
            icon = shiny::icon("chalkboard-teacher", lib = "font-awesome"),

            # --ADD SPACE----
            shiny::fluidRow(column(width = 12,tags$br())),

            # Set Forecasting Parameters RemixAutoML::IntermittentDemandScoringDataGenerator()----
            shiny::fluidRow(
              column(
                width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("chart-line", lib = "font-awesome"), "Data parameters"),
                  solidHeader = TRUE,
                  tags$br(),
                  width = 12,
                  collapsible = TRUE,
                  background = "teal", #"aqua", #"lime", #"navy", #"fuchsia", #"blue", #"maroon", #"purple", #"light-blue", #"yellow", #"red",#yellow, #"olive", #"orange", #"black", #"green",
                  column(
                    width = 4,
                    uiOutput("ID_NumberSims"),
                    tags$br()),
                  column(
                    width = 4,
                    uiOutput("ID_Lags_ScoringData"),
                    tags$br()
                  ),
                  column(
                    width = 4,
                    uiOutput("ID_MovingAverages_ScoringData"),
                    tags$br())))),

            # --ADD SPACE----
            shiny::fluidRow(column(width = 12,tags$br())),

            # Quantiles and Prediction Intervals RemixAutoML::IntermittentDemandScoringDataGenerator()----
            shiny::fluidRow(
              column(
                width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("spray-can", lib = "font-awesome"), "Uncertainty Parameters"),
                  solidHeader = TRUE,
                  tags$br(),
                  width = 12,
                  collapsible = TRUE,
                  background = "teal", #"aqua", #"lime", #"navy", #"fuchsia", #"blue", #"maroon", #"purple", #"light-blue", #"yellow", #"red",#yellow, #"olive", #"orange", #"black", #"green",
                  column(
                    width = 4,
                    uiOutput("ID_CountQuantiles_1"),
                    tags$br()
                  ),
                  column(
                    width = 4,
                    uiOutput("ID_SizeQuantiles_1"),
                    tags$br()),
                  column(
                    width = 4,
                    uiOutput("ID_PredictionIntervals"),
                    tags$br()))))),

          # HELP PAGE Tab----
          shiny::tabPanel(

            # Tab Title----
            title = "Help",

            # Tab Icon
            icon = shiny::icon("ambulance", lib = "font-awesome"),

            # --ADD SPACE----
            shiny::fluidRow(column(width = 12,tags$br())),

            # HELP PAGE Required Inputs----
            shiny::fluidRow(
              column(
                width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("server", lib = "font-awesome"), "Required Inputs"),
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  background = "light-blue", #"aqua", #"lime", #"navy", #"fuchsia", #"blue", #"maroon", #"purple", #"light-blue", #"yellow", #"red",#yellow, #"olive", #"orange", #"black", #"green",

                  # File Path To Models----
                  tags$h4("< File Path to Models >"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),"*",HTML('&nbsp'),"Paste in the file path to the folder where your models are saved."))),

                  # --ADD SPACE----
                  shiny::fluidRow(column(width = 12,tags$br())),

                  # File Path Save Forecast Data----
                  tags$h4("< File Path to Save Forecast Data >"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),"*",HTML('&nbsp'),"Paste in the file path to the folder where you want your forecast data saved. Folder must exist before building the models."))),

                  # --ADD SPACE----
                  shiny::fluidRow(column(width = 12,tags$br())),

                  # Help with import button----
                  tags$h4("< Import Transactional Data >"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),"*",HTML('&nbsp'),"Paste in the file path to the folder with your raw transactional data"))),

                  # --ADD SPACE----
                  shiny::fluidRow(column(width = 12,tags$br())),

                  # Help with selecting the target variable----
                  tags$h4("< Select Target Variable >"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),">",HTML('&nbsp'),"Click the down arrow to select the column names of the variables you will be forecasting"))),

                  # --ADD SPACE----
                  shiny::fluidRow(column(width = 12,tags$br())),

                  # Target variable----
                  tags$h4("< Select Date Variable >"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),">",HTML('&nbsp'),"Click the down arrow to select the date column in the data"))),

                  # --ADD SPACE----
                  shiny::fluidRow(column(width = 12,tags$br())),

                  # Date variable----
                  tags$h4("< Select Group Variables >"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),">",HTML('&nbsp'),"Click the down arrow to select group columns in the data"))),

                  # --ADD SPACE----
                  shiny::fluidRow(column(width = 12,tags$br())),

                  # Group variable----
                  tags$h4("< Select Group Variables >"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),">",HTML('&nbsp'),"Click the down arrow to select group columns in the data"))),

                  # --ADD SPACE----
                  shiny::fluidRow(column(width = 12,tags$br())),

                  # Time Unit Selection----
                  tags$h4("< Time Unit Selection >"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),">",HTML('&nbsp'),"Select from the list of time units. This must match the time unit you used in data construction."))),

                  # --ADD SPACE----
                  shiny::fluidRow(column(width = 12,tags$br())),

                  # Forecast Periods----
                  tags$h4("< Forecast Periods >"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),">",HTML('&nbsp'),"Select the number of periods ahead to forecast."))),

                  # --ADD SPACE----
                  shiny::fluidRow(column(width = 12,tags$br())),

                  # Current Date----
                  tags$h4("< Select Group Variables >"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),">",HTML('&nbsp'),"Input the current date (or previous date) for scoring models.")))))),

            # --ADD SPACE----
            shiny::fluidRow(column(width = 12,tags$br())),

            # HELP PAGE Advanced User Inputs----
            shiny::fluidRow(
              column(
                width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("chalkboard-teacher", lib = "font-awesome"), "Advanced User Inputs"),
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  background = "light-blue", #"aqua", #"lime", #"navy", #"fuchsia", #"blue", #"maroon", #"purple", #"light-blue", #"yellow", #"red",#yellow, #"olive", #"orange", #"black", #"green",

                  # --ADD SPACE----
                  shiny::fluidRow(column(width = 12,tags$br())),

                  # Help with import button----
                  tags$h4("< Max Lags >"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),"*",HTML('&nbsp'),"If you manually specified these in the data construction tab, you will have to set these to the same number."))),

                  # --ADD SPACE----
                  shiny::fluidRow(column(width = 12,tags$br())),

                  # Help with import button----
                  tags$h4("< Max Moving Averages >"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),"*",HTML('&nbsp'),"If you manually specified these in the data construction tab, you will have to set these to the same number."))),

                  # --ADD SPACE----
                  shiny::fluidRow(column(width = 12,tags$br())),

                  # Help with import button----
                  tags$h4("< Count Quantiles Modeled >"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),"*",HTML('&nbsp'),"Supply the same quantile values used in the model building tab."))),

                  # --ADD SPACE----
                  shiny::fluidRow(column(width = 12,tags$br())),

                  # Help with import button----
                  tags$h4("< Size Quantiles Modeled >"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),"*",HTML('&nbsp'),"Supply the same quantile values used in the model building tab."))),

                  # --ADD SPACE----
                  shiny::fluidRow(column(width = 12,tags$br())),

                  # Help with import button----
                  tags$h4("< Percentiles for Prediction Interval >"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),"*",HTML('&nbsp'),"Supply a differnt set than default to construct a prediction interval of your forecast.")))))))),

        # --ADD SPACE----
        shiny::fluidRow(column(width = 12,tags$br())),

        # Run Model Builder Button----
        shiny::fluidRow(
          column(
            width = 8,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "ID_ForecastingSettings",
              label = "Save Settings",
              icon = icon("save", lib = "font-awesome"),
              style = "gradient",
              color = "success")),
          column(
            width = 4,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "ID_ForecastSimulation",
              label = "Generate Forecasts",
              icon = icon("r-project", lib = "font-awesome"),
              style = "gradient",
              color = "primary")))),#"success"),#"danger"),#"warning"),#"primary"),#"default")))),

      #----

      # ID Optimization Recommendations Tab----
      tabItem(

        # --TAB REFERENCE VALUE----
        tabName = "automated_inventory_optimization_recommendations",

        # --PAGE TITLE----
        tags$h1("Optimization Recommendations"),

        # --ADD SPACE----
        shiny::fluidRow(column(width = 12,tags$br())),

        # --NEXT PREVIOUS BUTTONS----
        shiny::fluidRow(
          column(
            width = 6,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "link_to_automated_inventory_optimization_forecasting_1",
              label = "Demand Forecasting",
              icon = icon("chevron-left", lib = "font-awesome"),
              style = "gradient",
              color = "royal")))), #"success"),#"royal"),#"warning"),#"danger"),#"default"),)),


      #----

      #----

      #----

      #----

      # AUTOMATED ANOMALY DETECTION TABS----
      tabItem(

        tabName = "anomaly_detection"),

      #----

      #----

      #----

      #----

      # AUTOMATED RECOMMENDER SYSTEMS TABS----
      tabItem(
        tabName = "automated_recommenders"))))
