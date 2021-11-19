# Code comments and further improvements
# Press Control - A then Alt - o to collapse code to read the code outline
# 4 Blocks of dashes separate modules (Panel Forecasting, Time Series Forecasting, Intermittent Demand Forecasting, Funnel Forecasting, and Vector Panel Forecasting)
# 2 Blocks of dashes separate pages within a model (Create Project, Import Data, Data Analysis, Forecasting, Evaluation, Deployment)
# Utilize PACKAGENAME:: or PACKAGENAME::: before every function call (use the ::: for non exported functions from a package)
# Keep indentations consistent

# Begin UI Design Code ----
library(data.table)
shinydashboard::dashboardPage(

  # Color of app? ----
  skin = "black",

  # . ----

  # shinydashboard::dashboardHeader() ----
  # how to add logo to header: https://stackoverflow.com/questions/48978648/align-header-elements-in-shiny-dashboard
  shinydashboard::dashboardHeader(
    htmltools::tags$li(
      class = "dropdown",
      htmltools::tags$style(".main-header {max-height: 55px}"),
      htmltools::tags$style(".main-header .logo {height: 55px;}"),
      htmltools::tags$style(".sidebar-toggle {height: 20px; padding-top: 1px !important;}"),
      htmltools::tags$style(".navbar {min-height:55px !important}")),
    titleWidth = 190,

    #<img src='https://www.remixinstitute.com/wp-content/uploads/7b-Cheetah_Charcoal_Inline_No_Sub_No_BG.png' align = 'center' height = '20px'></img>
    # Image Home Page ----
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

  # . ----

  # shinydashboard::dashboardSidebar() ----
  # Menu Panel for RemixAutoML Modules
  shinydashboard::dashboardSidebar(

    # Change width to see all words on menu ----
    width = 315,

    # Contents of side bar menu ----
    shinydashboard::sidebarMenu(
      id = "modelMenu",

      # -- ADD SPACE ----
      RemixAutoML::BlankRow(12),

      # Home Page ----
      shinydashboard::menuItem(text="Home", tabName="HomePage", icon=shiny::icon("fort-awesome")),

      # -- ADD SPACE ----
      RemixAutoML::BlankRow(12),

      # MACHINE LEARNING SIDE BAR ----
      shinydashboard::menuItem(
        text="Machine Learning", tabName="machine_learning_home", icon=shiny::icon('bath'),

        # ML :: EDA & Data Cleaning Tab ----
        shinydashboard::menuItem(
          text="EDA & Data Cleaning", tabName="data_prep", icon=shiny::icon('bath'),
          shinydashboard::menuSubItem(text="EDA Home", tabName="eda_home_page", icon=shiny::icon('wrench')),
          shinydashboard::menuSubItem(text="Create Project", tabName="eda_create_project", icon=shiny::icon('wrench')),
          shinydashboard::menuSubItem(text="Load Data", tabName="eda_load_data", icon=shiny::icon('wrench')),
          shinydashboard::menuSubItem(text="EDA & Data Prep", tabName="eda_data_prep", icon=shiny::icon('wrench'))),

        # ML :: Feature Engineering Tab ----
        shinydashboard::menuItem(
          text="Feature Engineering", tabName="feature_engineering", icon=shiny::icon('blender'),
          shinydashboard::menuSubItem(text="Feature Engineering Home", tabName="feature_engineering_home_page", icon=shiny::icon('wrench')),
          shinydashboard::menuSubItem(text="Load Data", tabName="feature_engineering_load_data", icon=shiny::icon('wrench')),
          shinydashboard::menuSubItem(text="Data Engineering Features", tabName="data_engineering_features", icon=shiny::icon('wrench')),
          shinydashboard::menuSubItem(text="Model Based", tabName="model_based_features", icon=shiny::icon('wrench'))),

        # ML :: Model Optimization Tab ----
        shinydashboard::menuItem(
          text="Model Optimization", tabName="model_optimization", icon=shiny::icon('cat'),
          shinydashboard::menuSubItem(text="Model Optimization Home", tabName="model_optimization_home_page", icon=shiny::icon('wrench')),
          shinydashboard::menuSubItem(text="Model Optimization Selection", tabName="model_optimization_selection", icon=shiny::icon('wrench')),
          shinydashboard::menuSubItem(text="Model Optimization Parameter Settings", tabName="model_optimization_settings", icon=shiny::icon('wrench'))),

        # ML :: Model Scoring Tab ----
        shinydashboard::menuItem(
          text="Model Scoring", tabName="model_scoring", icon=shiny::icon('glass-cheers'),
          shinydashboard::menuSubItem(text="Model Scoring Home", tabName="model_scoring_home_page", icon=shiny::icon('wrench')),
          shinydashboard::menuSubItem(text="Model Scoring Setup", tabName="model_scoring_output", icon=shiny::icon('wrench'))),

        # ML :: Model Insights Tab ----
        shinydashboard::menuItem(
          text="Model Insights", tabName="model_insights", icon=shiny::icon('eye'),
          shinydashboard::menuSubItem(text="Model Insights Home", tabName="model_insights_home_page", icon=shiny::icon('wrench')),
          shinydashboard::menuSubItem(text="Load Model Output", tabName="model_insights_load_model_output_list", icon=shiny::icon('wrench')),
          shinydashboard::menuSubItem(text="Evaluation Plots", tabName="model_insights_visualize", icon=shiny::icon('wrench')),
          shinydashboard::menuSubItem(text="Evaluation Metrics", tabName="model_insights_tables", icon=shiny::icon('wrench')))),

      # . ----

      # FC :: Forecasting Series ----
      shinydashboard::menuItem(
        text="Forecasting", tabName="forecasting_home", icon=shiny::icon("gem"),

        # FC :: Panel Series Forecasting ----
        shinydashboard::menuItem(
          text="Panel Data Forecasting", tabName="panel_forecasting", icon=shiny::icon("gem"),
          shinydashboard::menuSubItem(text="Create or Open Project", tabName="panel_forecasting_project_creation", icon=shiny::icon('folder-open')),
          shinydashboard::menuSubItem(text="Import Data", tabName="panel_forecasting_data_import", icon=shiny::icon('database')),
          shinydashboard::menuSubItem(text="Data Analysis", tabName="panel_forecasting_eda", icon=icon('chart-bar')),
          shinydashboard::menuSubItem(text="Panel Series Forecast", tabName="panel_forecasting_modeling", icon=shiny::icon('crosshairs')),
          shinydashboard::menuSubItem(text="Model Evaluation", tabName="panel_forecasting_model_evaluation", icon=shiny::icon('check-double')),
          shinydashboard::menuSubItem(text="Model Deployment", tabName="panel_forecasting_deploy_model", icon=shiny::icon('terminal'))),

        # FC :: Time Series Forecasting ----
        shinydashboard::menuItem(
          text="Time Series Forecasting", tabName="timeseries_forecasting", icon=shiny::icon("chart-line"),
          shinydashboard::menuSubItem(text="Create or Open Project", tabName="timeseries_forecasting_project_creation", icon=shiny::icon('folder-open')),
          shinydashboard::menuSubItem(text="Data Import", tabName="timeseries_forecasting_data_import", icon=shiny::icon('database')),
          shinydashboard::menuSubItem(text="Data Analysis", tabName="timeseries_forecasting_eda", icon=icon('chart-bar')),
          shinydashboard::menuSubItem(text="Time Series Forecast", tabName="timeseries_forecasting", icon=shiny::icon('crosshairs')),
          shinydashboard::menuSubItem(text="Model Evaluation", tabName="timeseries_forecasting_model_evaluation", icon=shiny::icon('check-double')),
          shinydashboard::menuSubItem(text="Model Deployment", tabName="timeseries_forecasting_deploy_model", icon=shiny::icon('terminal'))),

        # FC :: Intermittent Demand Forecasting ----
        shinydashboard::menuItem(
          text="Intermittent Demand Forecasting", tabName="intermittent_demand_forecasting", icon=shiny::icon("hat-wizard"),
          shinydashboard::menuSubItem(text="Create or Open Project", tabName="intermittent_demand_forecasting_project_creation", icon=shiny::icon('folder-open')),
          shinydashboard::menuSubItem(text="Data Import", tabName="intermittent_demand_forecasting_data_import", icon=shiny::icon('database')),
          shinydashboard::menuSubItem(text="Data Analysis", tabName="intermittent_demand_forecasting_eda", icon=shiny::icon('wrench')),
          shinydashboard::menuSubItem(text="Intermittent Demand Forecasting", tabName="intermittent_demand_forecasting", icon=shiny::icon('brain')),
          shinydashboard::menuSubItem(text="Model Evaluation", tabName="intermittent_demand_forecasting_evaluation", icon=shiny::icon('random')),
          shinydashboard::menuSubItem(text="Model Deployment", tabName="intermittent_demand_forecasting_deployment", icon=shiny::icon('check-double'))),

        # FC :: Funnel Forecasting ----
        shinydashboard::menuItem(
          text="Funnel Forecasting", tabName="funnel_forecasting", icon=shiny::icon("funnel-dollar"),
          shinydashboard::menuSubItem(text="Create or Open Project", tabName="funnel_forecasting_project_creation", icon=shiny::icon('folder-open')),
          shinydashboard::menuSubItem(text="Data Import", tabName="funnel_forecasting_data_import", icon=shiny::icon('database')),
          shinydashboard::menuSubItem(text="Data Analysis", tabName="funnel_forecasting_eda", icon=shiny::icon('wrench')),
          shinydashboard::menuSubItem(text="Funnel Forecasting", tabName="funnel_forecasting", icon=shiny::icon('brain')),
          shinydashboard::menuSubItem(text="Model Evaluation", tabName="funnel_forecasting_evaluation", icon=shiny::icon('random')),
          shinydashboard::menuSubItem(text="Model Deployment", tabName="funnel_forecasting_deployment", icon=shiny::icon('check-double'))),

        # FC :: Vector Panel Forecasting ----
        shinydashboard::menuItem(
          text="Vector Panel Forecasting", tabName="vector_panel_forecasting", icon=shiny::icon("glass-cheers"),
          shinydashboard::menuSubItem(text="Create or Open Project", tabName="vector_panel_forecasting_project_creation", icon=shiny::icon('folder-open')),
          shinydashboard::menuSubItem(text="Data Import", tabName="vector_panel_forecasting_data_import", icon=shiny::icon('database')),
          shinydashboard::menuSubItem(text="Data Analysis", tabName="vector_panel_forecasting_eda", icon=shiny::icon('wrench')),
          shinydashboard::menuSubItem(text="Vector Panel Forecasting", tabName="vector_panel_forecasting", icon=shiny::icon('brain')),
          shinydashboard::menuSubItem(text="Model Evaluation", tabName="vector_panel_forecasting_evaluation", icon=shiny::icon('random')),
          shinydashboard::menuSubItem(text="Model Deployment", tabName="vector_panel_forecasting_deployment",icon = shiny::icon('check-double')))))),

  # . ----

  # shinydashboard::dashboardBody() ----
  # Core UI Body for Display, Output, and User Selection of items
  shinydashboard::dashboardBody(

    # Instantiate ShinyJS ----
    shinyjs::useShinyjs(),

    # Custom css ----
    htmltools::tags$head(htmltools::tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
    htmltools::tags$head(htmltools::tags$style('h1 {color:darkblue;}')),
    htmltools::tags$head(htmltools::tags$style('body {color:darkblue;}')),

    # ----

    # ----

    # Module Body UI Elements Go Here ----
    shinydashboard::tabItems(

      # ----

      # ----

      # HOME PAGE ----
      shinydashboard::tabItem(

        # ----

        # ----

        # -- TAB REFERENCE VALUE ----
        tabName = "HomePage",

        # -- PAGE TITLE ----
        htmltools::tags$h1("RemixAutoML Home"),
        htmltools::tags$h3("Select Project Type"),

        # -- ADD SPACE ----
        RemixAutoML::BlankRow(12),

        # Logo :: RemixAutoML Image ----
        shiny::fluidRow(htmltools::img(src = "DarkThemeLogo.png", height = 400, width = 1100)),

        # -- ADD SPACE ----
        RemixAutoML::BlankRow(12),

        # -- ADD SPACE ----
        RemixAutoML::BlankRow(12),

        # ML :: Machine Learning Header ----
        RemixAutoML::BlankRow(12),
        shiny::fluidRow(
          shiny::column(
            width = 4,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "nowhere",
              label = "Machine Learning",
              block = TRUE,
              size = "lg",
              style = "gradient",
              color = "royal"))),

        # ML :: EDA & Data Cleaning ----
        RemixAutoML::BlankRow(12),
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "link_eda_home_page",
              label = "EDA & Data Cleaning",
              size = "lg",
              icon = shiny::icon("bath", lib = "font-awesome"),
              style = "stretch",
              color = "primary"))),

        # ML :: Feature Engineering ----
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "link_feature_engineering_home_page",
              label = "Feature Engineering",
              size = "lg",
              icon = shiny::icon("blender", lib = "font-awesome"),
              style = "stretch",
              color = "primary"))),

        # ML :: Model Optimization ----
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "link_model_optimization_home_page_1",
              label = "Model Optimization",
              size = "lg",
              icon = shiny::icon("cat", lib = "font-awesome"),
              style = "stretch",
              color = "primary"))),

        # ML :: Model Scoring ----
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "link_model_scoring_home_page",
              label = "Model Scoring",
              size = "lg",
              icon = shiny::icon("glass-cheers", lib = "font-awesome"),
              style = "stretch",
              color = "primary"))),

        # ML :: Model Insights ----
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "link_model_insights_home_page",
              label = "Model Insights",
              size = "lg",
              icon = shiny::icon("eye", lib = "font-awesome"),
              style = "stretch",
              color = "primary"))),

        # . ----

        # FC :: Forecasting header ----
        RemixAutoML::BlankRow(12),
        shiny::fluidRow(
          shiny::column(
            width = 4,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "nowhere",
              label = "Forecasting",
              block = TRUE,
              size = "lg",
              style = "gradient",
              color = "royal"))),

        # FC :: Panel Data Forecasting ----
        RemixAutoML::BlankRow(12),
        shiny::fluidRow(
          shiny::column(width = 2),
          shiny::column(
            width = 12,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "link_to_automated_timeseries_project_creation_1",
              label = "Panel Data Forecasting",
              size = "lg",
              icon = shiny::icon("gem", lib = "font-awesome"),
              style = "stretch",
              color = "primary"))),

        # FC :: Time Series Forecasting ----
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "link_to_time_series_start_page___1",
              label = "Time Series Forecasting",
              size = "lg",
              icon = shiny::icon("chart-line", lib = "font-awesome"),
              style = "stretch",
              color = "primary"))),

        # FC :: Intermittent Demand Forecasting ----
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "link_to_intermittent_demand_start_page___1",
              label = "Intermittent Demand Forecasting",
              size = "lg",
              icon = shiny::icon("hat-wizard", lib = "font-awesome"),
              style = "stretch",
              color = "primary"))),

        # FC :: Funnel Forecasting ----
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "link_to_funnel_data_start_page___1",
              label = "Funnel Data Forecasting",
              size = "lg",
              icon = shiny::icon("funnel-dollar", lib = "font-awesome"),
              style = "stretch",
              color = "primary"))),

        # FC :: Vector Panel Data Forecasting ----
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "link_to_vector_panel_data_start_page___1",
              label = "Vector Panel Data Forecasting",
              size = "lg",
              icon = shiny::icon("glass-cheers", lib = "font-awesome"),
              style = "stretch",
              color = "primary")))),

      # . ----

      # . ----

      # . ----

      # . ----


      # EDA HOME ----
      shinydashboard::tabItem(

        # -- TAB REFERENCE VALUE ----
        tabName = "eda_home_page",

        # -- PAGE TITLE ----
        htmltools::tags$h1("Exploratory Data Analysis"),
        htmltools::tags$h3("Guide for this Module"),

        # -- ADD SPACE ----
        RemixAutoML::BlankRow(12),

        # -- ADD SPACE ----
        RemixAutoML::BlankRow(12),

        # -- NEXT PREVIOUS BUTTONS ----
        shiny::fluidRow(
          shiny::column(
            width = 8,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "link_HomePage",
              label = "App Home Page",
              icon = shiny::icon("chevron-left", lib = "font-awesome"),
              style = "gradient",
              color = "royal")),
          shiny::column(
            width = 4,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "link_eda_create_project",
              label = "Create Project",
              icon = shiny::icon("chevron-right", lib = "font-awesome"),
              style = "gradient",
              color = "royal"))),

        # -- ADD SPACE ----
        RemixAutoML::BlankRow(12),
        RemixAutoML::BlankRow(12),
        RemixAutoML::BlankRow(12),

        # Home Page Image ----
        shiny::fluidRow(htmltools::img(src = "EDA.PNG", height = 600, width = 1100))),

      # ML :: EDA Create & Open Project TabItem ----
      shinydashboard::tabItem(

        # -- TAB REFERENCE VALUE ----
        tabName = "eda_create_project",

        # -- PAGE TITLE ----
        htmltools::tags$h1("Exploratory Data Analysis"),
        htmltools::tags$h3("Create or Open Project"),

        # -- ADD SPACE ----
        RemixAutoML::BlankRow(12),

        # -- ADD SPACE ----
        RemixAutoML::BlankRow(12),

        # -- NEXT PREVIOUS BUTTONS ----
        shiny::fluidRow(
          shiny::column(
            width = 8,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "link_eda_home_page_2",
              label = "EDA Home",
              icon = icon("chevron-left", lib = "font-awesome"),
              style = "gradient",
              color = "royal")),
          shiny::column(
            width = 4,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "link_eda_load_data",
              label = "Data Import",
              icon = icon("chevron-right", lib = "font-awesome"),
              style = "gradient",
              color = "royal"))),

        # -- ADD SPACE ----
        RemixAutoML::BlankRow(12),

        # Create or Open Project UI ----
        # EDA Create Project Tab ----
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
            RemixAutoML::BlankRow(12),

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
                  #RemixAutoML::BlankRow(12),

                  # UI for Suppling Name of New Project ----
                  shiny::column(
                    width = 12,
                    shiny::uiOutput("EDA_NewProjectName"),
                    htmltools::tags$br()),

                  # -- ADD SPACE ----
                  RemixAutoML::BlankRow(12),

                  # UI for Supply Path to Folder to Create Project ----
                  shiny::column(
                    width = 5,
                    shinyFiles::shinyDirButton('eda_folder_choose', title = "Open Folder", label = "Select Directory to Store Project", multiple = FALSE),
                    htmltools::tags$br()),

                  # Add Space
                  RemixAutoML::BlankRow(12)))),

            # -- ADD SPACE ----
            RemixAutoML::BlankRow(12),

            # Action Buttons ----
            shiny::fluidRow(

              # Action button to create file directory ----
              shiny::column(
                width = 8,
                shinyjs::useShinyjs(),
                shinyWidgets::actionBttn(
                  inputId = "EDA_CreateProject",
                  label = "Press to Create Project",
                  icon = shiny::icon("plus", lib = "font-awesome"),
                  style = "gradient",
                  color = "royal")),


              # Action button to Start Over ----
              shiny::column(
                width = 4,
                shinyjs::useShinyjs(),
                shinyWidgets::actionBttn(
                  inputId = "EDA_StartOver",
                  label = "Press to Start Over",
                  icon = shiny::icon("plus", lib = "font-awesome"),
                  style = "gradient",
                  color = "primary")))),

          # Open Project ----
          shiny::tabPanel(

            # -- TAB TITLE ----
            title = "Open Project",

            # -- TAB ICON ----
            icon = shiny::icon("edit", lib = "font-awesome"),

            # -- ADD SPACE ----
            RemixAutoML::BlankRow(12),

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
                  RemixAutoML::BlankRow(12),

                  # UI for Supply Path to Folder to Create Project ----
                  shiny::column(
                    width = 5,
                    shinyFiles::shinyDirButton('folder', title = "Open Folder", label = "Select Folder", multiple = FALSE),
                    htmltools::tags$br()),

                  # Add space
                  RemixAutoML::BlankRow(12)))),

            # Action button to open ProjectList ----
            shiny::fluidRow(
              shiny::column(
                width = 4,
                shinyjs::useShinyjs(),
                shinyWidgets::actionBttn(
                  inputId = "EDA_OpenProject",
                  label = "Press to Open Project",
                  icon = shiny::icon("folder-open", lib = 'font-awesome'),
                  style = "gradient",
                  color = "royal")))),

          # HELP PAGE ----
          shiny::tabPanel(

            # -- TAB TITLE ----
            title = "Help",

            # -- TAB ICON ----
            icon = shiny::icon("ambulance", lib = "font-awesome"),

            # -- ADD SPACE ----
            RemixAutoML::BlankRow(12),

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
                  RemixAutoML::BlankRow(12),

                  # Help with Name Your Project Input ----
                  htmltools::tags$h4("< Name Your Project >"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),">",HTML('&nbsp'),"The name of your project will the name of the folder containing all the folders with project files."))),

                  # -- ADD SPACE ----
                  RemixAutoML::BlankRow(12),

                  # Help with selecting the target variable ----
                  htmltools::tags$h4("< Supply Path File To Root Directory Folder Where Project Folders will be Created >"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),">",HTML('&nbsp'),
                               "Press the Select Folder button and click on the project folder. Then press the Press to Open Project button"))))))))),






      # ML :: EDA & Data Preparation ----
      shinydashboard::tabItem(

        # -- TAB REFERENCE VALUE ----
        tabName = "eda_data_prep",

        # -- PAGE TITLE ----
        htmltools::tags$h1("EDA & Data Preparation"),
        htmltools::tags$h3("Analyze and Update Data For Feature Engineering"),

        # -- ADD SPACE ----
        RemixAutoML::BlankRow(12),

        # -- ADD SPACE ----
        RemixAutoML::BlankRow(12),

        # -- NEXT PREVIOUS BUTTONS ----
        shiny::fluidRow(
          shiny::column(
            width = 8,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "link_eda_load_data_2",
              label = "Load Data",
              icon = icon("chevron-left", lib = "font-awesome"),
              style = "gradient",
              color = "royal")),
          shiny::column(
            width = 4,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "link_feature_engineering_home_page_1",
              label = "Data Import",
              icon = icon("chevron-right", lib = "font-awesome"),
              style = "gradient",
              color = "royal"))),

        # -- ADD SPACE ----
        RemixAutoML::BlankRow(12),

        # Create or Open Project UI ----
        # EDA Tab ----
        shiny::tabsetPanel(

          # Tab types
          type = "tabs", # "tabs","pills"

          # Create Project ----
          shiny::tabPanel(

            # -- TAB TITLE ----
            title = "Data Visualization",

            # -- TAB ICON ----
            icon = shiny::icon("layer-group", lib = "font-awesome"),

            # -- ADD SPACE ----
            RemixAutoML::BlankRow(12),

            # UI Elements for Selecting Plotting Variables ----
            shiny::fluidRow(
              shiny::column(width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("drafting-compass", lib = "font-awesome"), "Create Project"),
                  solidHeader = TRUE, collapsible = TRUE, background = "navy", width = 12,
                  shiny::column(width = 4, shiny::uiOutput("EDA_Select_Plot"), htmltools::tags$br()),
                  RemixAutoML::BlankRow(12),
                  shiny::column(width = 4, shiny::uiOutput("EDA_Select_Y_Variable"), htmltools::tags$br()),
                  RemixAutoML::BlankRow(12),
                  shiny::column(width = 4, shiny::uiOutput("EDA_Select_X_Variables"), htmltools::tags$br()),
                  RemixAutoML::BlankRow(12)))),

            # -- ADD SPACE ----
            RemixAutoML::BlankRow(12),

            # Action Buttons ----
            shiny::fluidRow(

              # Action button to create file directory ----
              shiny::column(width = 4,
                shinyjs::useShinyjs(),
                shinyWidgets::actionBttn(inputId="EDA_CreatePlot", label="Press to Create Plot",
                  icon=shiny::icon("plus", lib="font-awesome"), style="gradient", color="royal")),

              # Action button to Start Over ----
              shiny::column(width = 4,
                shinyjs::useShinyjs(),
                shinyWidgets::actionBttn(inputId="EDA_SavePlot", label="Press to Start Over",
                  icon=shiny::icon("plus", lib = "font-awesome"), style="gradient", color="primary")))),

          # Statistical Analysis ----
          shiny::tabPanel(

            # -- TAB TITLE ----
            title = "Statistical Analysis",

            # -- TAB ICON ----
            icon = shiny::icon("edit", lib = "font-awesome"),

            # -- ADD SPACE ----
            RemixAutoML::BlankRow(12),

            # UI elements for importing data ----
            shiny::fluidRow(
              shiny::column(width=12,
                shinydashboard::box(title = tagList(shiny::icon("upload", lib = "font-awesome"), "Statistical Analysis"),
                  solidHeader=TRUE, collapsible=TRUE, background="navy", width=12,
                  RemixAutoML::BlankRow(12),
                  shiny::column(width = 5,
                    shinyFiles::shinyDirButton('folder', title = "Open Folder", label = "Select Folder", multiple = FALSE),
                    htmltools::tags$br()),

                  # Add space
                  RemixAutoML::BlankRow(12)))),

            # Action button to open ProjectList ----
            shiny::fluidRow(
              shiny::column(
                width = 4,
                shinyjs::useShinyjs(),
                shinyWidgets::actionBttn(
                  inputId = "EDA_OpenProject",
                  label = "Press to Open Project",
                  icon = shiny::icon("folder-open", lib = 'font-awesome'),
                  style = "gradient",
                  color = "royal")))),

          # HELP PAGE ----
          shiny::tabPanel(

            # -- TAB TITLE ----
            title = "Help",
            icon = shiny::icon("ambulance", lib = "font-awesome"),
            RemixAutoML::BlankRow(12),

            # UI elements for importing data ----
            shiny::fluidRow(
              shiny::column(width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("upload", lib = "font-awesome"), "Visualizations and Statistical Analysis"),
                  solidHeader = TRUE, collapsible = TRUE, background = "light-blue", width = 12,
                  RemixAutoML::BlankRow(12),
                  htmltools::tags$h4("< Name Your Project >"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),">",HTML('&nbsp'),"The name of your project will the name of the folder containing all the folders with project files."))),
                  RemixAutoML::BlankRow(12),
                  htmltools::tags$h4("< Supply Path File To Root Directory Folder Where Project Folders will be Created >"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),">",HTML('&nbsp'),
                               "Press the Select Folder button and click on the project folder. Then press the Press to Open Project button"))))))))),

      # . ----

      # . ----

      # FEATURE ENGINEERING HOME ----
      shinydashboard::tabItem(

        # -- TAB REFERENCE VALUE ----
        tabName = "feature_engineering_home_page",

        # -- PAGE TITLE ----
        htmltools::tags$h1("Feature Engineering"),
        htmltools::tags$h3("Guide for this Module"),

        # -- ADD SPACE ----
        RemixAutoML::BlankRow(12),

        # -- ADD SPACE ----
        RemixAutoML::BlankRow(12),

        # -- NEXT PREVIOUS BUTTONS ----
        shiny::fluidRow(
          shiny::column(
            width = 8, shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(inputId = "link_HomePage_1",
              label = "App Home Page", icon = shiny::icon("chevron-left", lib = "font-awesome"), style = "gradient", color = "royal")),
          shiny::column(
            width = 4, shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(inputId = "link_feature_engineering_load",
              label = "Feature Engineering Load Data", icon = shiny::icon("chevron-right", lib = "font-awesome"),style = "gradient", color = "royal"))),

        # -- ADD SPACE ----
        RemixAutoML::BlankRow(12),
        RemixAutoML::BlankRow(12),
        RemixAutoML::BlankRow(12),

        # Logo :: Remix Image ----
        shiny::fluidRow(htmltools::img(src = "FeatureEngineering.PNG", height = 600, width = 1100))),

      # MODEL OPTIMIZATION HOME ----
      shinydashboard::tabItem(

        # -- TAB REFERENCE VALUE ----
        tabName = "model_optimization_home_page",

        # -- PAGE TITLE ----
        htmltools::tags$h1("Model Optimization"),
        htmltools::tags$h3("Guide for this Module"),

        # -- ADD SPACE ----
        RemixAutoML::BlankRow(12),

        # -- ADD SPACE ----
        RemixAutoML::BlankRow(12),

        # -- NEXT PREVIOUS BUTTONS ----
        shiny::fluidRow(
          shiny::column(
            width = 8, shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(inputId = "link_HomePage_2",
                                     label = "App Home Page", icon = shiny::icon("chevron-left", lib = "font-awesome"), style = "gradient", color = "royal")),
          shiny::column(
            width = 4, shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(inputId = "link_model_optimization_load",
                                     label = "Load Data", icon = shiny::icon("chevron-right", lib = "font-awesome"),style = "gradient", color = "royal"))),

        # -- ADD SPACE ----
        RemixAutoML::BlankRow(12),
        RemixAutoML::BlankRow(12),
        RemixAutoML::BlankRow(12),

        # Logo :: Remix Image ----
        shiny::fluidRow(htmltools::img(src = "ModelOptimization.PNG", height = 600, width = 1100))),

      # MODEL SCORING HOME ----
      shinydashboard::tabItem(

        # -- TAB REFERENCE VALUE ----
        tabName = "model_scoring_home_page",

        # -- PAGE TITLE ----
        htmltools::tags$h1("Model Scoring"),
        htmltools::tags$h3("Guide for this Module"),

        # -- ADD SPACE ----
        RemixAutoML::BlankRow(12),

        # -- ADD SPACE ----
        RemixAutoML::BlankRow(12),

        # -- NEXT PREVIOUS BUTTONS ----
        shiny::fluidRow(
          shiny::column(
            width = 8,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "link_HomePage_3",
              label = "App Home Page",
              icon = shiny::icon("chevron-left", lib = "font-awesome"),
              style = "gradient",
              color = "royal")),
          shiny::column(
            width = 4,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "link_model_scoring_output_1",
              label = "Create Project",
              icon = shiny::icon("chevron-right", lib = "font-awesome"),
              style = "gradient",
              color = "royal"))),

        # -- ADD SPACE ----
        RemixAutoML::BlankRow(12),
        RemixAutoML::BlankRow(12),
        RemixAutoML::BlankRow(12),

        # Home Page Image ----
        shiny::fluidRow(htmltools::img(src = "ModelScoring.PNG", height = 600, width = 1100))),

      # . ----

      # FEATURE ENGINEERING Import Data Tab

      # . ----

      # Feature Engineering: Row-Dependent Operations

      # . ----

      # Feature Engineering: Row-Independent Operations

      # . ----

      # Feature Engineering: Data Partitioning for Modeling

      # . ----

      # Model-Based Feature Engineering

      # . ----

      # . ----

      # . ----

      # MODEL INSIGHTS HOME PAGE ----
      shinydashboard::tabItem(

        # -- TAB REFERENCE VALUE ----
        tabName = "model_insights_home_page",

        # -- PAGE TITLE ----
        htmltools::tags$h1("Model Insights Home"),
        htmltools::tags$h3("Guide for this Module"),

        # -- ADD SPACE ----
        RemixAutoML::BlankRow(12),

        # -- ADD SPACE ----
        RemixAutoML::BlankRow(12),

        # -- NEXT PREVIOUS BUTTONS ----
        shiny::fluidRow(
          shiny::column(
            width = 8,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "link_HomePage_4",
              label = "App Home Page",
              icon = shiny::icon("chevron-left", lib = "font-awesome"),
              style = "gradient",
              color = "royal")),
          shiny::column(
            width = 4,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "link_model_insights_load",
              label = "Load Model Output",
              icon = shiny::icon("chevron-right", lib = "font-awesome"),
              style = "gradient",
              color = "royal"))),

        # -- ADD SPACE ----
        RemixAutoML::BlankRow(12),
        RemixAutoML::BlankRow(12),
        RemixAutoML::BlankRow(12),

        # RemixAutoML Image ----
        shiny::fluidRow(htmltools::img(src = "ModelInsightsHome.png", height = 600, width = 1100))),

      # . ----

      # Model Insights: Import Tab ----
      shinydashboard::tabItem(

        # -- TAB REFERENCE VALUE ----
        tabName = "model_insights_load_model_output_list",

        # -- PAGE TITLE ----
        htmltools::tags$h1("Model Insights"),
        htmltools::tags$h3("Import Model Output List"),

        # -- ADD SPACE ----
        RemixAutoML::BlankRow(12),

        # -- ADD SPACE ----
        RemixAutoML::BlankRow(12),

        # -- NEXT PREVIOUS BUTTONS ----
        shiny::fluidRow(
          shiny::column(
            width = 8,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "link_model_insights_home_page_1",
              label = "Model Insights Home",
              icon = shiny::icon("chevron-left", lib = "font-awesome"),
              style = "gradient",
              color = "royal")),
          shiny::column(
            width = 4,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "link_model_insights_visualize",
              label = "Evaluation Plots",
              icon = shiny::icon("chevron-right", lib = "font-awesome"),
              style = "gradient",
              color = "royal"))),

        # -- ADD SPACE ----
        RemixAutoML::BlankRow(12),

        # Required Inputs and HELP PAGE ----
        shiny::tabsetPanel(

          # Tab types
          type = "tabs", # "tabs","pills"

          # TAB IMPORT NEW DATA ----
          shiny::tabPanel(

            # -- TAB TITLE ----
            title = "Load Model Objects",

            # -- TAB ICON ----
            icon = shiny::icon("atom", lib = "font-awesome"),

            # -- ADD SPACE ----
            RemixAutoML::BlankRow(12),

            # UI elements for importing Model Output List ----
            shiny::fluidRow(
              shiny::column(
                width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("database", lib = "font-awesome"), "Load Model Output List"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  background = "navy",
                  width = 12,

                  # -- ADD SPACE ----
                  RemixAutoML::BlankRow(12),

                  # UI Import Functionality ----
                  shiny::fileInput(
                    inputId = "ML_ModelOutputList",
                    label =  "Select Model Output List",
                    accept = c(".Rdata"))))),

            # -- ADD SPACE ----
            RemixAutoML::BlankRow(12),

            # UI elements for loading Model Training Data ----
            shiny::fluidRow(
              shiny::column(
                width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("database", lib = "font-awesome"), "Import Training Data"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  background = "blue",
                  width = 12,

                  # -- ADD SPACE ----
                  RemixAutoML::BlankRow(12),

                  # UI Import Functionality ----
                  shiny::fileInput(
                    inputId = "ML_TrainData",
                    label =  "Import Training Data",
                    accept = c(".Rdata", "text/csv", "text/comma-separated-values,text/plain", ".csv"))))),


            # -- ADD SPACE ----
            RemixAutoML::BlankRow(12),

            # UI elements for loading Shap Values Table ----
            shiny::fluidRow(
              shiny::column(
                width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("database", lib = "font-awesome"), "Import Shaped Shap Table"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  background = "aqua",
                  width = 12,

                  # -- ADD SPACE ----
                  RemixAutoML::BlankRow(12),

                  # UI Import Functionality ----
                  shiny::fileInput(
                    inputId = "ML_ShapTable",
                    label =  "Import Shap Data Table",
                    accept = c(".Rdata", "text/csv", "text/comma-separated-values,text/plain", ".csv")))))),


          # HELP PAGE ----
          shiny::tabPanel(

            # -- TAB TITLE ----
            title = "Help",

            # -- TAB ICON ----
            icon = shiny::icon("ambulance", lib = "font-awesome"),

            # -- ADD SPACE ----
            RemixAutoML::BlankRow(12),

            # UI elements for importing data ----
            shiny::fluidRow(
              shiny::column(
                width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("upload", lib = "font-awesome"), "Loading modeling objects"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  background = "light-blue",
                  width = 12,

                  # -- ADD SPACE ----
                  RemixAutoML::BlankRow(12),

                  # Help with Name Your Project Input ----
                  htmltools::tags$h4("Load Model Output Object"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),">",HTML('&nbsp'),"The object that gets returned from one of Remix's Auto__() supervised learning functions"))),

                  # -- ADD SPACE ----
                  RemixAutoML::BlankRow(12),

                  # Help with Name Your Project Input ----
                  htmltools::tags$h4("Load Training Data"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),">",HTML('&nbsp'),"Training Data can be used to generate variable importances by group variables and time slices for added insight along with creating addition model metrics to help you determine the right threshold for classification models"))),

                  # -- ADD SPACE ----
                  RemixAutoML::BlankRow(12),

                  # Help with selecting the target variable ----
                  htmltools::tags$h4("Load Shaped Shap Table"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),">",HTML('&nbsp'),
                               "The Shaped Shap Table is produced by running RemixAutoML::AutoShapeShap()"))))))))),





      # MODEL INSIGHTS VISUALIZATION Tab ----
      shinydashboard::tabItem(

        # -- TAB REFERENCE VALUE ----
        tabName = "model_insights_visualize",

        # -- PAGE TITLE ----
        htmltools::tags$h1("Model Insights"),
        htmltools::tags$h3("Visualize Model Performance"),

        # -- ADD SPACE ----
        RemixAutoML::BlankRow(12),

        # -- ADD SPACE ----
        RemixAutoML::BlankRow(12),

        # -- NEXT PREVIOUS BUTTONS ----
        shiny::fluidRow(
          shiny::column(
            width = 8,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "link_model_insights_load_1",
              label = "Load Model Output",
              icon = shiny::icon("chevron-left", lib = "font-awesome"),
              style = "gradient",
              color = "royal")),
          shiny::column(
            width = 4,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "model_insights_tables",
              label = "Evaluations Metrics",
              icon = shiny::icon("chevron-right", lib = "font-awesome"),
              style = "gradient",
              color = "royal"))),

        # -- ADD SPACE ----
        RemixAutoML::BlankRow(12),

        # Required Inputs and HELP PAGE ----
        shiny::tabsetPanel(

          # Tab types
          type = "tabs", # "tabs","pills"

          # TAB IMPORT NEW DATA ----
          shiny::tabPanel(

            # -- TAB TITLE ----
            title = "Model Output Visualizations",

            # -- TAB ICON ----
            icon = shiny::icon("atom", lib = "font-awesome"),

            # -- ADD SPACE ----
            RemixAutoML::BlankRow(12),

            # Select variables of interest ----
            shiny::fluidRow(
              shiny::column(
                width = 12,
                RemixAutoML::BlankRow(12),
                shinydashboard::box(
                  title = tagList(shiny::icon("filter", lib = "font-awesome"), "Variables of Interest"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  background = "navy",
                  width = 12,

                  # -- ADD SPACE ----
                  RemixAutoML::BlankRow(12),

                  # Model Input Variables ----
                  #RemixAutoML::BlankRow(12),
                  shiny::fluidRow(
                    shiny::column(
                      width = 4,
                      shiny::uiOutput("ModelInsights_ScoreVariable")),
                    shiny::column(
                      width = 4,
                      shiny::uiOutput("ModelInsights_TargetVariable")),
                    shiny::column(
                      width = 4,
                      shiny::uiOutput("ModelInsights_DateVariable")))),

                # Select ByVariables ----
                RemixAutoML::BlankRow(12),
                shinydashboard::box(
                  title = tagList(shiny::icon("filter", lib = "font-awesome"), "By-Variables"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  background = "blue",
                  width = 12,

                  # -- ADD SPACE ----
                  RemixAutoML::BlankRow(12),
                  shiny::fluidRow(
                    shiny::column(
                      width = 4,
                      shiny::uiOutput("ModelInsights_ByVariables1")),
                    shiny::column(
                      width = 4,
                      shiny::uiOutput("ModelInsights_ByVariables2")),
                    shiny::column(
                      width = 4,
                      shiny::uiOutput("ModelInsights_ByVariables3"))),

                  # -- ADD SPACE ----
                  RemixAutoML::BlankRow(12),
                  shiny::fluidRow(
                    shiny::column(
                      width = 4,
                      shiny::uiOutput("ModelInsights_ByVariables1Levels")),
                    shiny::column(
                      width = 4,
                      shiny::uiOutput("ModelInsights_ByVariables2Levels")),
                    shiny::column(
                      width = 4,
                      shiny::uiOutput("ModelInsights_ByVariables3Levels")))),

                # -- ADD SPACE ----
                RemixAutoML::BlankRow(12),
                shinydashboard::box(
                  title = tagList(shiny::icon("database", lib = "font-awesome"), "ML Evaluation Plots"),
                  solidHeader = TRUE,
                  width = 12,
                  htmltools::tags$br(),
                  background = "aqua",

                  # -- ADD SPACE ----
                  shiny::fluidRow(
                    shiny::column(
                      width = 4,
                      shiny::uiOutput("ModelInsights_IndependentVariable")),
                    shiny::column(
                      width = 4,
                      shiny::uiOutput("ModelInsights_PlotBuckets"))),
                  shiny::fluidRow(
                    shiny::column(
                      width = 4,
                      shiny::uiOutput("ModelInsights_PlotOptions"))),

                  # -- ADD SPACE ----
                  RemixAutoML::BlankRow(12),
                  shiny::column(
                    width = 4,
                    shiny::fluidRow(
                      shinyjs::useShinyjs(),
                      shinyWidgets::actionBttn(
                        inputId = "ML_Plot",
                        label = "Generate Plot",
                        style = "gradient",
                        color = "royal"))),

                  # -- ADD SPACE ----
                  RemixAutoML::BlankRow(12),

                  # ML Plot ----
                  shiny::plotOutput("ML_OutputPlot", width = "100%"))))))),











      # PANEL FORECASTING Create Project Tab ----
      shinydashboard::tabItem(

        # -- TAB REFERENCE VALUE ----
        tabName = "panel_forecasting_project_creation",

        # -- PAGE TITLE ----
        htmltools::tags$h1("Panel Data"),
        htmltools::tags$h3("Create or Open Project"),

        # -- ADD SPACE ----
        RemixAutoML::BlankRow(12),

        # -- ADD SPACE ----
        RemixAutoML::BlankRow(12),

        # -- NEXT PREVIOUS BUTTONS ----
        shiny::fluidRow(
          shiny::column(
            width = 8,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "link_HomePage_5",
              label = "App Home",
              icon = icon("chevron-left", lib = "font-awesome"),
              style = "gradient",
              color = "royal")),
          shiny::column(
            width = 4,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "link_to_panel_forecasting_data_import",
              label = "Go To Data Import",
              icon = icon("chevron-right", lib = "font-awesome"),
              style = "gradient",
              color = "royal"))),

        # -- ADD SPACE ----
        RemixAutoML::BlankRow(12),

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
            RemixAutoML::BlankRow(12),

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
                  RemixAutoML::BlankRow(12),

                  # UI for Suppling Name of New Project ----
                  shiny::column(
                    width = 12,
                    shiny::uiOutput("TS_NewProjectName"),
                    htmltools::tags$br()),

                  # -- ADD SPACE ----
                  RemixAutoML::BlankRow(12),

                  # UI for Supply Path to Folder to Create Project ----
                  shiny::column(
                    width = 5,
                    shinyFiles::shinyDirButton('folder1', title = "Open Folder", label = "Select Directory to Store Project", multiple = FALSE),
                    htmltools::tags$br()),

                  # Add Space
                  RemixAutoML::BlankRow(12)))),

            # -- ADD SPACE ----
            RemixAutoML::BlankRow(12),

            # Action Buttons ----
            shiny::fluidRow(

              # Action button to create file directory ----
              shiny::column(
                width = 8,
                shinyjs::useShinyjs(),
                shinyWidgets::actionBttn(
                  inputId = "TS_CreateProject",
                  label = "Press to Create Project",
                  icon = shiny::icon("plus", lib = "font-awesome"),
                  style = "gradient",
                  color = "royal")),


              # Action button to Start Over ----
              shiny::column(
                width = 4,
                shinyjs::useShinyjs(),
                shinyWidgets::actionBttn(
                  inputId = "TS_StartOver",
                  label = "Press to Start Over",
                  icon = shiny::icon("plus", lib = "font-awesome"),
                  style = "gradient",
                  color = "primary")))),

          # Open Project ----
          shiny::tabPanel(

            # -- TAB TITLE ----
            title = "Open Project",

            # -- TAB ICON ----
            icon = shiny::icon("edit", lib = "font-awesome"),

            # -- ADD SPACE ----
            RemixAutoML::BlankRow(12),

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
                  RemixAutoML::BlankRow(12),

                  # UI for Supply Path to Folder to Create Project ----
                  shiny::column(
                    width = 5,
                    shinyFiles::shinyDirButton('folder', title = "Open Folder", label = "Select Folder", multiple = FALSE),
                    htmltools::tags$br()),

                  # Add space
                  RemixAutoML::BlankRow(12)))),

            # Action button to open ProjectList ----
            shiny::fluidRow(
              shiny::column(
                width = 4,
                shinyjs::useShinyjs(),
                shinyWidgets::actionBttn(
                  inputId = "TS_OpenProject",
                  label = "Press to Open Project",
                  icon = shiny::icon("folder-open", lib = 'font-awesome'),
                  style = "gradient",
                  color = "royal")))),

          # HELP PAGE ----
          shiny::tabPanel(

            # -- TAB TITLE ----
            title = "Help",

            # -- TAB ICON ----
            icon = shiny::icon("ambulance", lib = "font-awesome"),

            # -- ADD SPACE ----
            RemixAutoML::BlankRow(12),

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
                  RemixAutoML::BlankRow(12),

                  # Help with Name Your Project Input ----
                  htmltools::tags$h4("< Name Your Project >"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),">",HTML('&nbsp'),"The name of your project will the name of the folder containing all the folders with project files."))),

                  # -- ADD SPACE ----
                  RemixAutoML::BlankRow(12),

                  # Help with selecting the target variable ----
                  htmltools::tags$h4("< Supply Path File To Root Directory Folder Where Project Folders will be Created >"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),">",HTML('&nbsp'),
                               "Press the Select Folder button and click on the project folder. Then press the Press to Open Project button"))))))))),

      # . ----

      # . ----

      # PANEL FORECASTING Data Import Tab ----
      shinydashboard::tabItem(

        # -- TAB REFERENCE VALUE ----
        tabName = "panel_forecasting_data_import",

        # -- PAGE TITLE ----
        htmltools::tags$h1("Panel Data"),
        htmltools::tags$h3("Import Data"),

        # -- ADD SPACE ----
        RemixAutoML::BlankRow(12),

        # -- ADD SPACE ----
        RemixAutoML::BlankRow(12),

        # -- NEXT PREVIOUS BUTTONS ----
        shiny::fluidRow(
          shiny::column(
            width = 8,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "link_to_automated_timeseries_project_creation",
              label = "Create or Open Project",
              icon = shiny::icon("chevron-left", lib = "font-awesome"),
              style = "gradient",
              color = "royal")),
          shiny::column(
            width = 4,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "link_to_panel_forecasting_eda",
              label = "Data Analysis",
              icon = shiny::icon("chevron-right", lib = "font-awesome"),
              style = "gradient",
              color = "royal"))),

        # -- ADD SPACE ----
        RemixAutoML::BlankRow(12),

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
            RemixAutoML::BlankRow(12),

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
                  RemixAutoML::BlankRow(12),

                  # UI Import Functionality ----
                  shiny::fileInput(
                    inputId = "TimeSeriesData",
                    label =  "Choose CSV File",
                    accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))))),

            # -- ADD SPACE ----
            RemixAutoML::BlankRow(12),

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
                  RemixAutoML::BlankRow(12),

                  # UI Import Functionality ----
                  shiny::fileInput(
                    inputId = "XREGS",
                    label =  "Choose CSV File",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv"))))),

            # -- ADD SPACE ----
            RemixAutoML::BlankRow(12),

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
                  RemixAutoML::BlankRow(12),

                  # UI Import Functionality ----
                  shiny::fileInput(
                    inputId = "Eval",
                    label =  "Choose CSV File",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv"))))),

            # -- ADD SPACE ----
            RemixAutoML::BlankRow(12),

            # -- ADD SPACE ----
            RemixAutoML::BlankRow(12),

            # Action Button to Store Data in Data Folder ----
            shiny::fluidRow(
              shiny::column(
                width = 6,
                shinyjs::useShinyjs(),
                shinyWidgets::actionBttn(
                  inputId = "TS_SaveDataToDataFolder",
                  label = "Store Data in Project",
                  icon = shiny::icon("save", lib = "font-awesome"),
                  style = "gradient",
                  color = "royal")))),

          # HELP PAGE ----
          shiny::tabPanel(

            # -- TAB TITLE ----
            title = "Help",

            # -- TAB ICON ----
            icon = shiny::icon("ambulance", lib = "font-awesome"),

            # -- ADD SPACE ----
            RemixAutoML::BlankRow(12),

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
                  RemixAutoML::BlankRow(12),

                  # Help with import button ----
                  htmltools::tags$h4("* Choose CSV File:"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),">",HTML('&nbsp'),"Click the Browse... button to open a window to select your transactions data file"))),

                  # -- ADD SPACE ----
                  RemixAutoML::BlankRow(12),

                  # Help with selecting the target variable ----
                  htmltools::tags$h4("* Select Target Variable"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),">",HTML('&nbsp'),"Click the down arrow to select the column names of the variables you will be forecasting"))),

                  # --ADD SPACE ----
                  RemixAutoML::BlankRow(12),

                  # Help with selecting the date variable ----
                  htmltools::tags$h4("* Select Date Variable"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),">",HTML('&nbsp'),"Click the down arrow to select the date column in the data"))),

                  # --ADD SPACE ----
                  RemixAutoML::BlankRow(12),

                  # Help with selecting the group variable ----
                  htmltools::tags$h4("* Select Group Variables"),
                  HTML(paste(p(HTML('&nbsp'),HTML('&nbsp'),">",HTML('&nbsp'),"Click the down arrow to select group columns in the data"))))))))),

      # . ----

      # . ----

      # PANEL FORECASTING Data Analysis Tab ----
      shinydashboard::tabItem(

        # -- TAB REFERENCE VALUE ----
        tabName = "panel_forecasting_eda",

        # -- PAGE TITLE ----
        htmltools::tags$h1("Panel Data"),
        htmltools::tags$h3("Data Analysis"),

        # -- ADD SPACE ----
        RemixAutoML::BlankRow(12),

        # -- ADD SPACE ----
        RemixAutoML::BlankRow(12),

        # -- NEXT PREVIOUS BUTTONS ----
        shiny::fluidRow(
          shiny::column(
            width = 8,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "link_to_panel_forecasting_data_import_1",
              label = "Import Data",
              icon = shiny::icon("chevron-left", lib = "font-awesome"),
              style = "gradient",
              color = "royal")),
          shiny::column(
            width = 4,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "link_to_panel_forecasting_modeling",
              label = "Time Series Forecast",
              icon = shiny::icon("chevron-right", lib = "font-awesome"),
              style = "gradient",
              color = "royal"))),

        # -- ADD SPACE ----
        RemixAutoML::BlankRow(12),

        # Plot Data or See DataTable ----
        shiny::tabsetPanel(
          id = "time_series_EDA_tab_1",
          type = "tabs",

          # Plotly Time Series ----
          shiny::tabPanel(

            # -- TITLE ----
            title = "Time Series Plot",
            icon = shiny::icon('chart-line', lib = 'font-awesome'),

            # TRAINING DATA: UI elements for selecting data columns for subsetting ----
            shiny::fluidRow(
              shiny::column(
                width = 12,
                RemixAutoML::BlankRow(12),
                shinydashboard::box(
                  title = tagList(shiny::icon("filter", lib = "font-awesome"), "Training Data Variables"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  background = "navy",
                  width = 12,

                  # -- ADD SPACE ----
                  RemixAutoML::BlankRow(12),

                  # Model Input Variables ----
                  shiny::fluidRow(
                    shiny::column(
                      width = 4,
                      shiny::uiOutput("timeSeriesTimeUnit")),
                      htmltools::tags$br()),
                  RemixAutoML::BlankRow(12),
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
                  RemixAutoML::BlankRow(12),

                  # TimeSeriesFill and ModelDataPrep ----
                  shiny::fluidRow(
                    shiny::column(
                      width = 5,
                      shinyjs::useShinyjs(),
                      shinyWidgets::actionBttn(
                        inputId = "DataPrep_Train",
                        label = "Prepare Data",
                        icon = shiny::icon("chevron-right", lib = "font-awesome"),
                        style = "gradient",
                        color = "royal")))))),

            # -- ADD SPACE ----
            RemixAutoML::BlankRow(12),

            # XREGS DATA: UI elements for selecting data columns for subsetting ----
            shiny::fluidRow(
              shiny::column(
                width = 12,
                RemixAutoML::BlankRow(12),
                shinydashboard::box(
                  title = tagList(shiny::icon("filter", lib = "font-awesome"), "XREGS Data Variables"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  background = "purple",
                  width = 12,

                  # -- ADD SPACE ----
                  RemixAutoML::BlankRow(12),

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
                  RemixAutoML::BlankRow(12),

                  # TimeSeriesFill and ModelDataPrep ----
                  shiny::column(
                    width = 5,
                    shinyjs::useShinyjs(),
                    shinyWidgets::actionBttn(
                      inputId = "DataPrep_XREGS",
                      label = "Prepare Data",
                      icon = shiny::icon("chevron-right", lib = "font-awesome"),
                      style = "gradient",
                      color = "royal"))))),

            # -- ADD SPACE ----
            RemixAutoML::BlankRow(12),

            # Eval DATA: UI elements for selecting data columns for subsetting ----
            shiny::fluidRow(
              shiny::column(
                width = 12,
                RemixAutoML::BlankRow(12),
                shinydashboard::box(
                  title = tagList(shiny::icon("filter", lib = "font-awesome"), "Evaluation Data Variables"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  background = "purple",
                  width = 12,

                  # -- ADD SPACE ----
                  RemixAutoML::BlankRow(12),

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
                  RemixAutoML::BlankRow(12),

                  # TimeSeriesFill and ModelDataPrep ----
                  shiny::column(
                    width = 5,
                    shinyjs::useShinyjs(),
                    shinyWidgets::actionBttn(
                      inputId = "DataPrep_Eval",
                      label = "Prepare Data",
                      icon = shiny::icon("chevron-right", lib = "font-awesome"),
                      style = "gradient",
                      color = "royal"))))),

            # -- ADD SPACE ----
            RemixAutoML::BlankRow(12),

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
                  RemixAutoML::BlankRow(12),

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
            RemixAutoML::BlankRow(12),

            # Time Series Plot Box ----
            shinydashboard::box(title = tagList(shiny::icon("database", lib = "font-awesome"), "Time Series Plot"),
                solidHeader = TRUE,
                width = 12,
                htmltools::tags$br(),
                background = "aqua",
                shiny::column(
                  width = 1,
                  shinyWidgets::dropdown(
                    htmltools::tags$h5("Time Series Plot Inputs"),
                    htmltools::tags$h5(htmltools::tags$style('h5 {color:darkblue;}')),
                    htmltools::tags$h5(htmltools::tags$style('#TS_AggregateFunction {color:darkblue;}')),
                    htmltools::tags$h5(htmltools::tags$style('#TS_OtherGroups {color:darkblue;}')),
                    htmltools::tags$h5(htmltools::tags$style('#TS_NumberGroupsDisplay {color:darkblue;}')),
                    style = "pill",
                    icon = shiny::icon("gear"),
                    tooltip = shinyWidgets::tooltipOptions(title = "Click to see plot options"),
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
                RemixAutoML::BlankRow(12),


                # Time Series Plot ----
                plotly::plotlyOutput("TimeSeriesPlot", width = "100%")),

            # -- ADD SPACE ----
            RemixAutoML::BlankRow(12),

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
                  RemixAutoML::BlankRow(12),

                  # UI Plot Options ----
                  shiny::fluidRow(
                    shiny::column(
                      width = 3,
                      shiny::uiOutput("TickMarksX"),
                      htmltools::tags$br()),
                    shiny::column(
                      width = 3,
                      shiny::uiOutput("TS_LineWidth"),
                      htmltools::tags$br()),
                    shiny::column(
                      width = 3,
                      shiny::uiOutput("TS_AngleY"),
                      htmltools::tags$br()),
                    shiny::column(
                      width = 3,
                      shiny::uiOutput("TS_AngleX"),
                      htmltools::tags$br())),
                  shiny::fluidRow(
                    shiny::column(
                      width = 3,
                      shiny::uiOutput("TS_TextSize"),
                      htmltools::tags$br()),
                    shiny::column(
                      width = 3,
                      shiny::uiOutput("TS_TextColor"),
                      htmltools::tags$br()),
                    shiny::column(
                      width = 3,
                      shiny::uiOutput("TS_LineColor"),
                      htmltools::tags$br()),
                    shiny::column(
                      width = 3,
                      shiny::uiOutput("TS_LegendTextColor"),
                      htmltools::tags$br())),
                  shiny::fluidRow(
                    shiny::column(
                      width = 3,
                      shiny::uiOutput("TS_ChartColor"),
                      htmltools::tags$br()),
                    shiny::column(
                      width = 3,
                      shiny::uiOutput("TS_GridColor"),
                      htmltools::tags$br()),
                    shiny::column(
                      width = 3,
                      shiny::uiOutput("TS_BackGroundColor"),
                      htmltools::tags$br()),
                    shiny::column(
                      width = 3,
                      shiny::uiOutput("TS_BorderColor"),
                      htmltools::tags$br())))))),

          # DataTable Time Series----
          shiny::tabPanel(
            title = "Time Series Data",
            icon = shiny::icon('database', lib = 'font-awesome'),
            htmltools::tags$br(),
            shinydashboard::box(title = "Time Series Data Table",
                solidHeader = TRUE,
                width = 12,
                background = "aqua",
                htmltools::tags$style(
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

      # . ----

      # . ----

      # PANEL FORECASTING Model Building Tab ----
      shinydashboard::tabItem(

        # -- Name of tab ----
        tabName = "panel_forecasting_modeling",

        # -- PAGE TITLE ----
        htmltools::tags$h1("Panel Data"),
        htmltools::tags$h3("Model Building and Forecasting"),

        # -- ADD SPACE ----
        RemixAutoML::BlankRow(12),

        # -- ADD SPACE ----
        RemixAutoML::BlankRow(12),

        # -- NEXT PREVIOUS BUTTONS ----
        shiny::fluidRow(
          shiny::column(
            width = 8,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "link_to_panel_forecasting_eda_1",
              label = "Data Analysis",
              icon = shiny::icon("chevron-left", lib = "font-awesome"),
              style = "gradient",
              color = "royal")),
          shiny::column(
            width = 4,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "link_to_panel_forecasting_model_evaluation",
              label = "Model Evaluation", icon = shiny::icon("chevron-right", lib = "font-awesome"), style = "gradient", color = "royal"))),

        # -- ADD SPACE ----
        RemixAutoML::BlankRow(12),

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
            RemixAutoML::BlankRow(12),

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
                  RemixAutoML::BlankRow(12),

                  # Production args ----
                  shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_TaskType")),
                  shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_NumGPU")),
                  shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_NThreads")),
                  shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_MaxMemory")),

                  # -- ADD SPACE ----
                  RemixAutoML::BlankRow(12),
                  RemixAutoML::BlankRow(12),

                  # Insights Args ----
                  shiny::column(width = 3, shiny::uiOutput("TS_MetricEval")),
                  shiny::column(width = 3, shiny::uiOutput("TS_CARMA_PDFOutputPath")),
                  shiny::column(width = 3, shiny::uiOutput("TS_CARMA_SaveDataPath")),
                  shiny::column(width = 3, shiny::uiOutput("TS_CARMA_NumParDepPlots"))))),

            # -- ADD SPACE ----
            RemixAutoML::BlankRow(12),

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
                  RemixAutoML::BlankRow(12),

                  # Model Selection Args ----
                  shiny::column(width = 4, shiny::uiOutput("TSMLModelsSelection")),
                  shiny::column(width = 4, shiny::uiOutput("H2OModelSelection"))))),

            # -- ADD SPACE ----
            RemixAutoML::BlankRow(12),

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
                  RemixAutoML::BlankRow(12),

                  # Forecasting Args ----
                  shiny::column(width = 4, shiny::uiOutput("TS_TimeUnit")),
                  shiny::column(width = 4, shiny::uiOutput("TS_HoldOutPeriods")),
                  shiny::column(width = 4, shiny::uiOutput("TS_FCPeriods")),

                  # -- ADD SPACE ----
                  RemixAutoML::BlankRow(12),

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
                    icon = shiny::icon("save", lib = 'font-awesome'),
                    style = "gradient",
                    color = "royal",
                    size = "lg")),
                shiny::column(
                  width = 3,
                  align="center",
                  shinyWidgets::actionBttn(
                    inputId = "TS_Build",
                    label = "Push To Start",
                    icon = shiny::icon("power-off", lib = 'font-awesome'),
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
            RemixAutoML::BlankRow(12),

            # CatBoost CARMA Args ----
            shiny::fluidRow(
              shiny::column(
                width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("tachometer-alt", lib = "font-awesome"), "AutoCatBoostCARMA Parameters"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = FALSE,
                  width = 12,
                  background = "navy",
                  RemixAutoML::BlankRow(12),

                  # Target transformation args ----
                  shinydashboard::box(
                    title = tagList(shiny::icon("tachometer-alt", lib = "font-awesome"), "Target Transformations"),
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    width = 12,
                    background = "purple",
                    RemixAutoML::BlankRow(12),

                    # Target variable transformation args
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_Methods")),
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_Difference")),
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_NonNegativePrep")),
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_RoundPreds"))),

                  # Calendar-related args ----
                  shinydashboard::box(
                    title = tagList(shiny::icon("tachometer-alt", lib = "font-awesome"), "Calendar Features"),
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    width = 12,
                    background = "purple",
                    RemixAutoML::BlankRow(12),

                    # Calendar-related args
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_CalendarVariables")),
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_HolidayVariables")),
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_HolidayLags")),
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_HolidayMovingAverages"))),

                  # Lags, moving averages, and other rolling stats ----
                  shinydashboard::box(
                    title = tagList(shiny::icon("tachometer-alt", lib = "font-awesome"), "Time Series Features"),
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    width = 12,
                    background = "purple",

                    # Lags, moving averages, and other rolling stats
                    RemixAutoML::BlankRow(12),
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_HierarchGroups")),
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_Quantiles_Selected")),
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_Lags")),
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_MovingAverages")),

                    # -- ADD SPACE
                    RemixAutoML::BlankRow(12),
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_MovingSD")),
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_MovingSkew")),
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_MovingKurt")),
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_MovingQuantiles")),

                    # -- ADD SPACE
                    RemixAutoML::BlankRow(12),
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_Lags1")),
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_MovingAverages1")),
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_MovingSD1")),
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_MovingSkew1")),

                    # -- ADD SPACE
                    RemixAutoML::BlankRow(12),
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_MovingKurt1")),
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_MovingQuantiles1"))),

                  # Bonus features ----
                  shinydashboard::box(
                    title = tagList(shiny::icon("tachometer-alt", lib = "font-awesome"), "Bonus Features"),
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    width = 12,
                    background = "purple",
                    RemixAutoML::BlankRow(12),

                    # Bonus features
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_AnomalyDetection_HighThreshold")),
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_AnomalyDetection_LowThreshold")),
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_TimeTrend")),
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_Fourier")),

                    # -- ADD SPACE
                    RemixAutoML::BlankRow(12),
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_DataTruncate")),
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_TimeWeights"))),


                  # ML grid tuning args ----
                  shinydashboard::box(
                    title = tagList(shiny::icon("tachometer-alt", lib = "font-awesome"), "ML Grid Tuning"),
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    width = 12,
                    background = "light-blue",
                    RemixAutoML::BlankRow(12),

                    # ML grid tuning
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_GridTune")),
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_ModelCount")),
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_PassInGrid")),
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_MaxRunsWithoutNewWinner")),

                    # -- ADD SPACE
                    RemixAutoML::BlankRow(12),
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_MaxRunMinutes"))),


                  # ML loss functions ----
                  shinydashboard::box(
                    title = tagList(shiny::icon("tachometer-alt", lib = "font-awesome"), "ML Loss Functions"),
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    width = 12,
                    background = "light-blue",
                    RemixAutoML::BlankRow(12),

                    # ML loss functions
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_EvalMetric")),
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_EvalMetricValue")),
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_LossFunction")),
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_LossFunctionValue"))),

                  # ML tuning args ----
                  shinydashboard::box(
                    title = tagList(shiny::icon("tachometer-alt", lib = "font-awesome"), "ML Complexity"),
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    width = 12,
                    background = "light-blue",
                    RemixAutoML::BlankRow(12),

                    # ML tuning args
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_NTrees")),
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_Depth")),
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_Langevin")),
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_DiffusionTemperature"))),

                  # ML overfitting args ----
                  shinydashboard::box(
                    title = tagList(shiny::icon("tachometer-alt", lib = "font-awesome"), "ML Overfitting"),
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    width = 12,
                    background = "light-blue",
                    RemixAutoML::BlankRow(12),

                    # ML overfitting args
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_LearningRate")),
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_L2_Leaf_Reg")),
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_RandomStrength")),
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_BorderCount")),

                    # -- ADD SPACE
                    RemixAutoML::BlankRow(12),
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_SubSample")),
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_MinDataInLeaf")),
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_SamplingUnit")),
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_RSM")),

                    # -- ADD SPACE
                    RemixAutoML::BlankRow(12),
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_ModelSizeReg"))),

                  # ML style args ----
                  shinydashboard::box(
                    title = tagList(shiny::icon("tachometer-alt", lib = "font-awesome"), "ML Build Style"),
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    width = 12,
                    background = "light-blue",
                    RemixAutoML::BlankRow(12),

                    # ML style args
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_BootStrapType")),
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_GrowPolicy")),
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_ScoreFunction")),
                    shiny::column(width = 3, shiny::uiOutput("TS_CatBoost_CARMA_FeatureBorderType")))))),

            # -- ADD SPACE ----
            RemixAutoML::BlankRow(12),

            # XGBoost CARMA Args ----
            shiny::fluidRow(
              shiny::column(
                width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("tachometer-alt", lib = "font-awesome"), "AutoXGBoostCARMA Configurations"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = FALSE,
                  width = 12,
                  background = "navy",
                  RemixAutoML::BlankRow(12),

                  # Target transformation args ----
                  shinydashboard::box(
                    title = tagList(shiny::icon("tachometer-alt", lib = "font-awesome"), "Target Transformations"),
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    width = 12,
                    background = "purple",
                    RemixAutoML::BlankRow(12),

                    # Target variable transformation args
                    shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_Transformation")),
                    shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_Difference")),
                    shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_NonNegativePrep")),
                    shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_RoundPreds"))),

                  # Calendar-related args ----
                  shinydashboard::box(
                    title = tagList(shiny::icon("tachometer-alt", lib = "font-awesome"), "Calendar Features"),
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    width = 12,
                    background = "purple",
                    RemixAutoML::BlankRow(12),

                    # Calendar-related args
                    shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_CalendarVariables")),
                    shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_HolidayVariables")),
                    shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_HolidayLags")),
                    shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_HolidayMovingAverages"))),

                  # Lags, moving averages, and other rolling stats ----
                  shinydashboard::box(
                    title = tagList(shiny::icon("tachometer-alt", lib = "font-awesome"), "Time Series Features"),
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    width = 12,
                    background = "purple",
                    RemixAutoML::BlankRow(12),

                    # Lags, moving averages, and other rolling stats
                    shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_HierarchGroups")),
                    shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_Quantiles_Selected")),
                    shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_Lags")),
                    shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_MovingAverages")),

                    # -- ADD SPACE
                    RemixAutoML::BlankRow(12),
                    shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_MovingSD")),
                    shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_MovingSkew")),
                    shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_MovingKurt")),
                    shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_MovingQuantiles")),

                    # -- ADD SPACE
                    RemixAutoML::BlankRow(12),
                    shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_Lags1")),
                    shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_MovingAverages1")),
                    shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_MovingSD1")),
                    shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_MovingSkew1")),

                    # -- ADD SPACE
                    shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_MovingKurt1")),
                    shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_MovingQuantiles1"))),

                  # Bonus features ----
                  shinydashboard::box(
                    title = tagList(shiny::icon("tachometer-alt", lib = "font-awesome"), "Bonus Features"),
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    width = 12,
                    background = "purple",
                    RemixAutoML::BlankRow(12),

                    # Bonus features
                    shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_AnomalyDetection_HighThreshold")),
                    shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_AnomalyDetection_LowThreshold")),
                    shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_TimeTrend")),
                    shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_Fourier")),

                    # -- ADD SPACE
                    RemixAutoML::BlankRow(12),
                    shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_DataTruncate")),
                    shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_TimeWeights"))),

                  # ML grid tuning args ----
                  shinydashboard::box(
                    title = tagList(shiny::icon("tachometer-alt", lib = "font-awesome"), "ML Grid Tuning"),
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    width = 12,
                    background = "light-blue",
                    RemixAutoML::BlankRow(12),

                    # ML grid tuning
                    shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_GridTune")),
                    shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_ModelCount")),
                    shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_PassInGrid")),
                    shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_MaxRunsWithoutNewWinner")),

                    # -- ADD SPACE
                    RemixAutoML::BlankRow(12),
                    shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_MaxRunMinutes"))),

                  # ML loss functions ----
                  shinydashboard::box(
                    title = tagList(shiny::icon("tachometer-alt", lib = "font-awesome"), "ML Loss Functions"),
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    width = 12,
                    background = "light-blue",
                    RemixAutoML::BlankRow(12),

                    # ML loss functions
                    shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_EvalMetric")),
                    shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_LossFunction"))),

                  # ML tuning args ----
                  shinydashboard::box(
                    title = tagList(shiny::icon("tachometer-alt", lib = "font-awesome"), "ML Complexity"),
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    width = 12,
                    background = "light-blue",
                    RemixAutoML::BlankRow(12),

                    # ML tuning args
                    shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_NTrees")),
                    shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_Depth")),
                    shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_LearningRate")),
                    shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_MinChildWeight")),

                    # -- ADD SPACE
                    RemixAutoML::BlankRow(12),
                    shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_SubSample")),
                    shiny::column(width = 3, shiny::uiOutput("TS_XGBoost_CARMA_ColSampleByTree")))))),

            # -- ADD SPACE ----
            RemixAutoML::BlankRow(12),

            # H2O CARMA Args ----
            shiny::fluidRow(
              shiny::column(
                width = 12,
                shinydashboard::box(
                  title = tagList(shiny::icon("tachometer-alt", lib = "font-awesome"), "AutoH2OCARMA Configurations"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = FALSE,
                  width = 12,
                  background = "navy",
                  RemixAutoML::BlankRow(12),

                  # Target transformation args ----
                  shinydashboard::box(
                    title = tagList(shiny::icon("tachometer-alt", lib = "font-awesome"), "Target Transformations"),
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    width = 12,
                    background = "purple",
                    RemixAutoML::BlankRow(12),

                    # Target variable transformation args
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_Transformation")),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_Difference")),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_NonNegativePrep")),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_RoundPreds"))),


                  # Calendar-related args ----
                  shinydashboard::box(
                    title = tagList(shiny::icon("tachometer-alt", lib = "font-awesome"), "Calendar Features"),
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    width = 12,
                    background = "purple",
                    RemixAutoML::BlankRow(12),

                    # Calendar-related args
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_CalendarVariables")),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_HolidayVariables")),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_HolidayLags")),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_HolidayMovingAverages"))),

                  # Lags, moving averages, and other rolling stats ----
                  shinydashboard::box(
                    title = tagList(shiny::icon("tachometer-alt", lib = "font-awesome"), "Time Series Features"),
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    width = 12,
                    background = "purple",
                    RemixAutoML::BlankRow(12),

                    # Lags, moving averages, and other rolling stats
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_HierarchGroups")),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_Quantiles_Selected")),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_Lags")),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_MovingAverages")),

                    # -- ADD SPACE
                    RemixAutoML::BlankRow(12),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_MovingSD")),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_MovingSkew")),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_MovingKurt")),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_MovingQuantiles")),

                    # -- ADD SPACE
                    RemixAutoML::BlankRow(12),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_Lags1")),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_MovingAverages1")),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_MovingSD1")),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_MovingSkew1")),

                    # -- ADD SPACE
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_MovingKurt1")),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_MovingQuantiles1"))),

                  # Bonus features ----
                  shinydashboard::box(
                    title = tagList(shiny::icon("tachometer-alt", lib = "font-awesome"), "Bonus Features"),
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    width = 12,
                    background = "purple",
                    RemixAutoML::BlankRow(12),

                    # Bonus features
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_AnomalyDetection_HighThreshold")),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_AnomalyDetection_LowThreshold")),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_TimeTrend")),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_Fourier")),

                    # -- ADD SPACE
                    RemixAutoML::BlankRow(12),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_DataTruncate")),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_TimeWeights"))),

                  # ML grid tuning args ----
                  shinydashboard::box(
                    title = tagList(shiny::icon("tachometer-alt", lib = "font-awesome"), "ML Grid Tuning"),
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    width = 12,
                    background = "light-blue",
                    RemixAutoML::BlankRow(12),

                    # ML grid tuning
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_GridTune")),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_ModelCount")),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_PassInGrid")),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_MaxRunsWithoutNewWinner")),

                    # -- ADD SPACE
                    RemixAutoML::BlankRow(12),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_MaxRunMinutes"))),


                  # ML args ----
                  shinydashboard::box(
                    title = tagList(shiny::icon("tachometer-alt", lib = "font-awesome"), "ML Arguments"),
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    width = 12,
                    background = "light-blue",
                    RemixAutoML::BlankRow(12),

                    # ML grid tuning
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_NTrees")),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_LearnRate")),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_LearnRateAnnealing")),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_MaxDepth")),

                    # -- ADD SPACE
                    RemixAutoML::BlankRow(12),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_SampleRate")),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_ColSampleRatePerTree")),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_ColSampleRatePerTreeLevel")),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_MinRows")),

                    # -- ADD SPACE
                    RemixAutoML::BlankRow(12),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_NBins")),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_NBinsCats")),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_NBinsTopLevel")),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_HistogramType")),

                    # -- ADD SPACE
                    RemixAutoML::BlankRow(12),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_CategoricalEncoding")),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_MTries")),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_ColSampleRate")),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_RandomColumnNames")),

                    # -- ADD SPACE
                    RemixAutoML::BlankRow(12),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_InteractionColumns")),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_Distribution")),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_Link")),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_Solver")),

                    # -- ADD SPACE
                    RemixAutoML::BlankRow(12),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_Alpha")),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_Lambda")),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_LambdaSearch")),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_NLambdas")),

                    # -- ADD SPACE
                    RemixAutoML::BlankRow(12),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_Standardize")),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_RemoveCollinearColumns")),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_InterceptInclude")),
                    shiny::column(width = 3, shiny::uiOutput("TS_H2O_CARMA_NonNegativeCoefficients"))))))))),

      # . ----

      # . ----

      # PANEL FORECASTING Model Evaluation Tab -----
      shinydashboard::tabItem(

        # Name of tab being used ----
        tabName = "panel_forecasting_model_evaluation",

        # -- TITLE ----
        htmltools::tags$h1("Panel Data"),
        htmltools::tags$h3("Model Evaluation"),

        # -- ADD SPACE ----
        RemixAutoML::BlankRow(12),

        # -- ADD SPACE ----
        RemixAutoML::BlankRow(12),

        # -- NEXT PREVIOUS BUTTONS ----
        shiny::fluidRow(
          shiny::column(
            width = 8,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "link_to_panel_forecasting_modeling_1",
              label = "Time Series Forecasting",
              icon = shiny::icon("chevron-left", lib = "font-awesome"),
              style = "gradient",
              color = "royal")),
          shiny::column(
            width = 4,
            shinyjs::useShinyjs(),
            shinyWidgets::actionBttn(
              inputId = "link_to_panel_forecasting_deploy_model",
              label = "Deploy Model",
              icon = shiny::icon("chevron-right", lib = "font-awesome"),
              style = "gradient",
              color = "royal"))),

        # -- ADD SPACE ----
        RemixAutoML::BlankRow(12),

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
            RemixAutoML::BlankRow(12),

            # Target, Model, GroupVar Selections ----
            shiny::fluidRow(
              shiny::column(
                width = 12,
                RemixAutoML::BlankRow(12),
                shinydashboard::box(
                  title = tagList(shiny::icon("filter", lib = "font-awesome"), "Select variables"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  background = "navy",
                  width = 12,

                  # -- ADD SPACE ----
                  RemixAutoML::BlankRow(12),

                  # Model Input Variables ----
                  shiny::column(width = 3, shiny::uiOutput("Evaluate")),
                  shiny::column(width = 3, shiny::uiOutput("TS_ModelID")),
                  shiny::column(width = 3, shiny::uiOutput("TSEval_timeSeriesTarget2")),
                  shiny::column(width = 3, shiny::uiOutput("TSEval_timeSeriesGroupVars2"))))),

            # -- ADD SPACE ----
            RemixAutoML::BlankRow(12),

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
                  RemixAutoML::BlankRow(12),

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
                      htmltools::tags$h5("Time Series Plot Inputs"),
                      htmltools::tags$h5(htmltools::tags$style('h5 {color:darkblue;}')),
                      htmltools::tags$h5(htmltools::tags$style('#FC_AggregateFunction {color:darkblue;}')),
                      htmltools::tags$h5(htmltools::tags$style('#FC_OtherGroups {color:darkblue;}')),
                      htmltools::tags$h5(htmltools::tags$style('#FC_NumberGroupsDisplay {color:darkblue;}')),
                      style = "pill", inputId = "Something",
                      icon = shiny::icon("gear"),
                      tooltip = shinyWidgets::tooltipOptions(title = "Click to see plot options"),
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
                        inputId = "FC_ResetPlotSettings",
                        label = "Reset Plot",
                        style = "gradient",
                        color = "primary"))),

                  # -- ADD SPACE ----
                  RemixAutoML::BlankRow(12),

                  # Time Series Plot ----
                  shiny::column(
                    width = 12,
                    shiny::plotOutput("TS_ForecastPlot", width = "100%"))))),
                    #plotly::plotlyOutput("TS_ForecastPlot", width = "100%"))))),

            # -- ADD SPACE ----
            RemixAutoML::BlankRow(12),

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
                  RemixAutoML::BlankRow(12),

                  # UI Plot Options----
                  shiny::fluidRow(
                    shiny::column(width = 3, shiny::uiOutput("FC_ChartColor"), htmltools::tags$br()),
                    shiny::column(width = 3, shiny::uiOutput("FC_LineColor"), htmltools::tags$br()),
                    shiny::column(width = 3, shiny::uiOutput("FC_GridColor"), htmltools::tags$br()),
                    shiny::column(width = 3, shiny::uiOutput("FC_BorderColor"))),
                  shiny::fluidRow(
                    shiny::column(width = 3, shiny::uiOutput("FC_TickMarksX"), htmltools::tags$br()),
                    shiny::column(width = 3, shiny::uiOutput("FC_LineWidth"), htmltools::tags$br()),
                    shiny::column(width = 3, shiny::uiOutput("FC_AngleY"), htmltools::tags$br()),
                    shiny::column(width = 3, shiny::uiOutput("FC_AngleX"), htmltools::tags$br())),
                  shiny::fluidRow(
                    shiny::column(width = 3, shiny::uiOutput("FC_TextSize"), htmltools::tags$br()),
                    shiny::column(width = 3, shiny::uiOutput("FC_TextColor"), htmltools::tags$br()),
                    shiny::column(width = 3, shiny::uiOutput("FC_BackGroundColor"), htmltools::tags$br()),
                    shiny::column(width = 3, shiny::uiOutput("FC_LegendTextColor"), htmltools::tags$br())))))),

          # Model Selection and Required Inputs ----
          shiny::tabPanel(

            # -- TAB TITLE ----
            title = "Model Summaries",

            # -- TAB ICON ----
            icon = shiny::icon("atom", lib = "font-awesome"),

            # -- ADD SPACE ----
            RemixAutoML::BlankRow(12),

            # Target, Model, GroupVar Selections ----
            shiny::fluidRow(
              shiny::column(
                width = 12,
                RemixAutoML::BlankRow(12),
                shinydashboard::box(
                  title = tagList(shiny::icon("filter", lib = "font-awesome"), "Select variables"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  background = "navy",
                  width = 12,

                  # -- ADD SPACE ----
                  RemixAutoML::BlankRow(12),

                  # Model Input Variables ----
                  shiny::column(
                    width = 4,
                    shiny::uiOutput("TSEval_timeSeriesTarget")),
                  shiny::column(
                    width = 4,
                    shiny::uiOutput("TSEval_timeSeriesGroupVars"))))),

            # -- ADD SPACE ----
            RemixAutoML::BlankRow(12),

            # -- ADD SPACE ----
            RemixAutoML::BlankRow(12),

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

                  htmltools::tags$style(
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
                          icon = shiny::icon("power-on", lib = 'font-awesome'),
                          style = "gradient",
                          color = "royal",
                          size = "md")))),

                  # --ADD SPACE ----
                  RemixAutoML::BlankRow(12),

                  # DataTable Output ----
                  shiny::column(
                    width = 12,
                    DT::DTOutput(outputId = "TS_ModelMetrics", width = "100%")))))))),

      # . ----

      # . ----

      # PANEL FORECASTING Deployment Tab -----
      shinydashboard::tabItem(

        # Name of tab being used ----
        tabName = "panel_forecasting_deploy_model",

        # -- TITLE ----
        htmltools::tags$h1("Panel Data"),
        htmltools::tags$h3("Model Deployment")),

      # . ----

      # . ----

      # . ----

      # . ----


      # TIME SERIES FORECASTING TABS ----
      shinydashboard::tabItem(

        # Name of tab being used ----
        tabName = "time_series_forecasting"),

      # . ----

      # . ----

      # . ----

      # . ----


      # Intermittent Demand Forecasting ----
      shinydashboard::tabItem(

        # --TAB REFERENCE VALUE ----
        tabName = "intermittent_demand_forecasting_eda"),

      # . ----

      # . ----

      # . ----

      # . ----

      # Funnel Forecasting TABS ----
      shinydashboard::tabItem(

        # --TAB REFERENCE VALUE ----
        tabName = "funnel_forecasting"),

      # . ----

      # . ----

      # . ----

      # . ----

      # Vector Panel Forecasting TABS ----
      shinydashboard::tabItem(

        # --TAB REFERENCE VALUE ----
        tabName = "vector_panel_forecasting"))))
