
# Helpful resources
# 1. Reactive vs Observe vs ObserveEvent: first 2 update immediately, ObserveEvent updates only when triggered, like with a button

# Libraries
library(shiny)
library(shinydashboard)
library(flexdashboard)
library(DT)
library(shinyWidgets)
library(shinyBS)

# Options
options(shiny.maxRequestSize = 300000*1024^2)
options(scipen = 9999)
data.table::setDTthreads(threads = max(1L, parallel::detectCores() - 2L))
data.table::getDTthreads(verbose = TRUE)
Local <- TRUE

# Server begin
server <- shiny::shinyServer(function(input, output, session) {

# AUTOMATED FORECASTING SERVER CODE ----

  # AUTOMATED Time Series CREATE OR OPEN PROJECT ----

  #----

    # Name the project ----
    output$TS_NewProjectName <- shiny::renderUI({
      textInput(inputId = "TS_NewProjectName", label = "Provide a Project Name", value = "", width = "100%", placeholder = "Project_01")
    })

    # Store Project Root Folder Path ----
    output$TS_Root_Folder <- shiny::renderUI({
      textInput(inputId = "TS_Root_Folder", label = "Supply Path File to Root Directory Folder Where Project Folders will be Created", value = "", width = "100%", placeholder = "File Path Here")
    })

    # File Path to Open Project ----
    output$TS_ProjectListUpload <- shiny::renderUI({
      textInput(inputId = "TS_ProjectListUpload", label = "Supply Path File to Project Directory Folder Where Project Files are Stored", value = "", width = "100%", placeholder = "File Path Here")
    })

    # ProjectList[[]] and Create Project ----
    shiny::observeEvent(eventExpr = input$TS_CreateProject, {

      # Define Root Path
      RootPath <<- gsub("\\\\", "/", input$TS_Root_Folder)

      # Check if input is fillout out ----
      if(input$TS_NewProjectName != "" && RootPath != "") {

        # Check if project folder exists----
        if(dir.exists(paths = file.path(normalizePath(RootPath), input$TS_NewProjectName))) {

          # Check for ProjectList
          if(!file.exists(normalizePath(file.path(RootPath, input$TS_NewProjectName, "MetaData", "ProjectList.Rdata")))) {

            # Create project list and save components ----
            ProjectList <<- tryCatch({RemixAutoML::CreateProjectFolders(ProjectName = input$TS_NewProjectName, RootPath = RootPath, ExistsButNoProjectList = TRUE, Local = Local)}, error = function(x) NULL)

            # Notification ----
            if(!is.null(ProjectList)) {
              shinyWidgets::sendSweetAlert(session, title = NULL, text = "Project Setup Complete!", type = NULL, btn_labels = "Ok", btn_colors = "green", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "60%")
            } else {
              shinyWidgets::sendSweetAlert(session, title = NULL, text = "Invalid path location or project name", type = "error", btn_labels = NULL, btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "60%")
            }

          } else {

            # Notification that project isn't open ----
            shinyWidgets::sendSweetAlert(session, title = NULL, text = "Project already exists! Go to the open project tab.", type = NULL, btn_labels = "error", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "60%")
          }
        } else {

          # Build Project Folder Structure and Create MetaDataCollectionList ----
          ProjectList <<- tryCatch({RemixAutoML::CreateProjectFolders(ProjectName = input$TS_NewProjectName, RootPath = RootPath, ExistsButNoProjectList = FALSE, Local = Local)}, error = function(x) NULL)

          # Notification ----
          if(!is.null(ProjectList)) {
            shinyWidgets::sendSweetAlert(session, title = NULL, text = "Project Setup Complete!", type = NULL, btn_labels = "Ok", btn_colors = "green", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "60%")
          } else {
            shinyWidgets::sendSweetAlert(session, title = NULL, text = "Invalid path location or file name", type = "error", btn_labels = NULL, btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "60%")
          }
        }
      } else {

        # Error handling and communication with user ----
        if(input$TS_NewProjectName == "" && RootPath == "") {
          shinyWidgets::sendSweetAlert(session, title = NULL, text = "Supply a name for your project and provide a valid file path", type = "error", btn_labels = NULL, btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
        } else if(input$TS_NewProjectName == "" && RootPath != "") {
          shinyWidgets::sendSweetAlert(session, title = NULL, text = "Supply a name for your project", type = "error", btn_labels = NULL, btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
        } else if(input$TS_NewProjectName != "" && RootPath == "") {
          shinyWidgets::sendSweetAlert(session, title = NULL, text = "Supply a valid file path for your project", type = "error", btn_labels = NULL, btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
        }
      }
    })

    # Open Project ----
    shiny::observeEvent(eventExpr = input$TS_OpenProject, {

      # Define Root Path
      RootPath <<- gsub("\\\\", "/", input$TS_Root_Folder)
      TS_ProjectListUpload <<- gsub("\\\\", "/", input$TS_ProjectListUpload)

      # Check if input is valid ----
      if(TS_ProjectListUpload != "") {

        # Load Project R List ----
        if(dir.exists(paths = file.path(normalizePath(TS_ProjectListUpload), "MetaData"))) {

          # Check if ProjectList exists ----
          if(file.exists(normalizePath(file.path(TS_ProjectListUpload, "MetaData", "ProjectList.Rdata")))) {

            # Load project list into global environment
            load(normalizePath(file.path(TS_ProjectListUpload, "MetaData", "ProjectList.Rdata")), envir = .GlobalEnv)

            # String to communicate with user
            Message <- paste0("ProjectList is loaded")

            # Load SourceData data into project
            if(file.exists(normalizePath(file.path(TS_ProjectListUpload, "Data", "SourceData.csv")))) {
              SourceData <<- data.table::fread(file = file.path(TS_ProjectListUpload, "Data", "SourceData.csv"))
              Message <- c(Message, paste0(" TrainData is loaded"))
            } else {
              Message <- c(Message, paste0(" TrainData NOT FOUND"))
            }

            # Load XREGS data into project
            if(file.exists(normalizePath(file.path(TS_ProjectListUpload, "Data", "XREGS.csv")))) {
              xregs <<- data.table::fread(file = file.path(TS_ProjectListUpload, "Data", "XREGS.csv"))
              Message <- c(Message, paste0(" XREGS is loaded"))
            } else {
              Message <- c(Message, paste0(" XREGS NOT FOUND"))
            }

            # Load XREGS data into project
            if(file.exists(normalizePath(file.path(TS_ProjectListUpload, "Data", "EvalData.csv")))) {
              EvalData <<- data.table::fread(file = file.path(TS_ProjectListUpload, "Data", "EvalData.csv"))
              Message <- c(Message, paste0(" EvalData is loaded"))
            } else {
              Message <- c(Message, paste0(" EvalData NOT FOUND"))
            }

            # Notify user----
            shinyWidgets::sendSweetAlert(session, title = NULL, text = Message, type = "success", btn_labels = "Ok", btn_colors = "green", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "60%")

          } else {

            # Create project list and save components ----
            ProjectList <<- RemixAutoML::CreateProjectFolders(ProjectName = input$TS_NewProjectName, RootPath = RootPath, ExistsButNoProjectList = TRUE, Local = Local)

            # Notification that project is ready ----
            shinyWidgets::sendSweetAlert(session, title = NULL, text = "Project Setup Complete!", type = NULL, btn_labels = "Ok", btn_colors = "green", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "60%")
          }

        } else {

          # Notify user ----
          shinyWidgets::sendSweetAlert(session, title = NULL, text = "Folder does not exist", type = "error", btn_labels = NULL, btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "60%")
        }

      } else {

        # Error handling and communication with user ----
        shinyWidgets::sendSweetAlert(session, title = NULL, text = "Provide a valid file path", type = "error", btn_labels = NULL, btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
      }
    })

    # Start Over ----
    shiny::observeEvent(eventExpr = input$TS_StartOver, {

      # Clear objects ----
      rm(list = ls())

      # Notify user ----
      shinyWidgets::sendSweetAlert(session, title = NULL, text = "All objects have been removed", type = "error", btn_labels = "Ok", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "60%")
    })

    # Next and Previous Buttons ----
    shiny::observeEvent(eventExpr = input$link_to_automated_timeseries_project_creation, {
        updateTabItems(session, inputId = "modelMenu", selected = "automated_timeseries_project_creation")})

  #----

    # AUTOMATED FORECASTING - DATA IMPORT ----

    # New Project: Load Train Data ----
    TimeSeriesData <- shiny::reactive({

      # Time series data will be NULL initially ----
      inFile <- input$TimeSeriesData

      # Do nothing if infile is null ----
      if(is.null(inFile)) return(NULL)

      # Update ProjectList----
      ProjectList[["SourceDataCreateDate"]] <<- Sys.Date()

      # Remove data ----
      if(exists("SourceData")) rm(SourceData)
      if(exists("TimeSeriesFillCheck")) rm(TimeSeriesFillCheck)

      # Import data ----
      data.table::fread(file = inFile$datapath)
    })

    # New Project: Load XREGS Data ----
    XREGS <- shiny::reactive({

      # Time series data will be NULL initially ----
      inFile <- input$XREGS

      # Do nothing if infile is null ----
      if(is.null(inFile)) return(NULL)

      # Update ProjectList----
      ProjectList[["SourceDataCreateDate"]] <<- Sys.Date()

      # Remove data ----
      if(exists("xregs")) rm(XREGS)
      if(exists("TimeSeriesFillCheck1")) rm(TimeSeriesFillCheck1)

      # Import data ----
      data.table::fread(file = inFile$datapath)
    })

    # New Project: Load Eval Data ----
    Eval <- shiny::reactive({

      # Time series data will be NULL initially ----
      inFile <- input$Eval

      # Do nothing if infile is null ----
      if(is.null(inFile)) return(NULL)

      # Update ProjectList----
      ProjectList[["SourceDataCreateDate"]] <<- Sys.Date()

      # Remove data ----
      if(exists("EvalData")) rm(EvalData)
      if(exists("TimeSeriesFillCheck2")) rm(TimeSeriesFillCheck2)

      # Import data ----
      data.table::fread(file = inFile$datapath)
    })

    # UI Selection Boxes ----
    output$TS_SourceData <- shiny::renderUI({
      RemixAutoML::TextInput(InputID = "TS_SourceData", Label = "Path to Data", Value = NULL, Width = "100%", Placeholder = "NULL")
    })

    # Data Create Date ----
    output$TS_Creation_Date_Source_Data <- shiny::renderUI({
      RemixAutoML::DateInput(InputID = "TS_Creation_Date_Source_Data", Label = "Import Data Creation Date", Value = Sys.Date(), Min = "1970-01-01", Max = "2100-01-01", Format = "yyyy-mm-dd")
    })

    # ProjectList[[]] Save Settings and Data to Data Folder ----
    shiny::observeEvent(eventExpr = input$TS_SaveDataToDataFolder, {
      if(exists("ProjectList")) {
        if(!exists("SourceData")) {

          # TrainData global storage
          SourceData <<- tryCatch({TimeSeriesData()}, error = function(x) {
            shinyWidgets::sendSweetAlert(session, title = NULL, text = "A Project Needs to Be Created or Loaded to Utilize This Feature", type = "error", btn_labels = "Ok", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "60%")
          })

          # EvalData global storage
          EvalData <<- tryCatch({Eval()}, error = function(x) {
            shinyWidgets::sendSweetAlert(session, title = NULL, text = "A Project Needs to Be Created or Loaded to Utilize This Feature", type = "error", btn_labels = "Ok", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "60%")
          })

          # xregs global storage
          xregs <<- tryCatch({XREGS()}, error = function(x) NULL)

        } else {

          # Store data ----
          SourceData <<- TimeSeriesData()
          xregs <<- tryCatch({XREGS()}, error = function(x) NULL)
          EvalData <<- Eval()

          # Plot name storage
          ProjectList[["timeSeriesTarget"]] <<- NULL
          ProjectList[["timeSeriesDateColumn"]] <<- NULL
          ProjectList[["timeSeriesGroupVars"]] <<- NULL
          ProjectList[["timeSeriesIndependent_XREGS"]] <<- NULL
          ProjectList[["timeSeriesDateColumn_XREGS"]] <<- NULL
          ProjectList[["timeSeriesGroupVars_XREGS"]] <<- NULL
          ProjectList[["timeSeriesTarget_Eval"]] <<- NULL
          ProjectList[["timeSeriesDateColumn_Eval"]] <<- NULL
          ProjectList[["timeSeriesGroupVars_Eval"]] <<- NULL

          # Modeling name storage
          ProjectList[["TS_timeSeriesTarget"]] <<- NULL
          ProjectList[["TS_timeSeriesDateColumn"]] <<- NULL
          ProjectList[["TS_timeSeriesGroupVars"]] <<- NULL
          ProjectList[["TS_timeSeriesTarget_XREGS"]] <<- NULL
          ProjectList[["TS_timeSeriesDateColumn_XREGS"]] <<- NULL
          ProjectList[["TS_timeSeriesGroupVars_XREGS"]] <<- NULL
          ProjectList[["TS_timeSeriesTarget_Eval"]] <<- NULL
          ProjectList[["TS_timeSeriesDateColumn_Eval"]] <<- NULL
          ProjectList[["TS_timeSeriesGroupVars_Eval"]] <<- NULL

          # Evaluation name storage
          ProjectList[["TSEval_timeSeriesTarget"]] <<- NULL
          ProjectList[["TSEval_timeSeriesTarget2"]] <<- NULL
          ProjectList[["TSEval_timeSeriesGroupVars"]] <<- NULL
          ProjectList[["TSEval_timeSeriesGroupVars2"]] <<- NULL

          # Update inputs
          shinyWidgets::updatePickerInput(session, inputId = "timeSeriesTarget", label = "Select Target Variable", choices = names(SourceData))
          shinyWidgets::updatePickerInput(session, inputId = "timeSeriesDateColumn", label = "Select Date Column", choices = names(SourceData))
          shinyWidgets::updatePickerInput(session, inputId = "timeSeriesGroupVars", label = "Select Group Variables", choices = names(SourceData))
          shinyWidgets::updatePickerInput(session, inputId = "timeSeriesTarget_XREGS", label = "Select Independent Variables", choices = if(!is.null(xregs)) names(xregs) else NULL)
          shinyWidgets::updatePickerInput(session, inputId = "timeSeriesDateColumn_XREGS", label = "Select Date Column", choices = if(!is.null(xregs)) names(xregs) else NULL)
          shinyWidgets::updatePickerInput(session, inputId = "timeSeriesGroupVars_XREGS", label = "Select Group Variables", choices = if(!is.null(xregs)) names(xregs) else NULL)
          shinyWidgets::updatePickerInput(session, inputId = "timeSeriesTarget_Eval", label = "Select Target Variable", choices = if(!is.null(EvalData)) names(EvalData) else NULL)
          shinyWidgets::updatePickerInput(session, inputId = "timeSeriesDateColumn_Eval", label = "Select Date Column", choices = if(!is.null(EvalData)) names(EvalData) else NULL)
          shinyWidgets::updatePickerInput(session, inputId = "timeSeriesGroupVars_Eval", label = "Select Group Variables", choices = if(!is.null(EvalData)) names(EvalData) else NULL)

          shinyWidgets::updatePickerInput(session, inputId = "TS_timeSeriesTarget", label = "Select Target Variable", choices = names(SourceData))
          shinyWidgets::updatePickerInput(session, inputId = "TS_timeSeriesDateColumn", label = "Select Date Variables", choices = names(SourceData))
          shinyWidgets::updatePickerInput(session, inputId = "TS_timeSeriesGroupVars", label = "Select Group Variables", choices = names(SourceData))
          shinyWidgets::updatePickerInput(session, inputId = "TS_timeSeriesTarget_XREGS", label = "Select Independent Variables", choices = if(!is.null(xregs)) names(xregs) else NULL)
          shinyWidgets::updatePickerInput(session, inputId = "TS_timeSeriesDateColumn_XREGS", label = "Select Date Variables", choices = if(!is.null(xregs)) names(xregs) else NULL)
          shinyWidgets::updatePickerInput(session, inputId = "TS_timeSeriesGroupVars_XREGS", label = "Select Group Variables", choices = if(!is.null(xregs)) names(xregs) else NULL)
          shinyWidgets::updatePickerInput(session, inputId = "TS_timeSeriesTarget_Eval", label = "Select Target Variable", choices = if(!is.null(EvalData)) names(EvalData) else NULL)
          shinyWidgets::updatePickerInput(session, inputId = "TS_timeSeriesDateColumn_Eval", label = "Select Date Variables", choices = if(!is.null(EvalData)) names(EvalData) else NULL)
          shinyWidgets::updatePickerInput(session, inputId = "TS_timeSeriesGroupVars_Eval", label = "Select Group Variables", choices = if(!is.null(EvalData)) names(EvalData) else NULL)

          shinyWidgets::updatePickerInput(session, inputId = "TSEval_timeSeriesGroupVars", label = "Select Group Variables", choices = names(SourceData))
          shinyWidgets::updatePickerInput(session, inputId = "TSEval_timeSeriesGroupVars2", label = "Select Group Variables", choices = names(SourceData))
          if(exists("FinalForecastData")) {
            shinyWidgets::updatePickerInput(session, inputId = "TSEval_timeSeriesTarget", label = "Select Target Variable", choices = names(FinalForecastData))
            shinyWidgets::updatePickerInput(session, inputId = "TSEval_timeSeriesTarget2", label = "Select Target Variable", choices = names(FinalForecastData))
          }
        }

        # Write to file ----
        data.table::fwrite(TimeSeriesData(), file = file.path(ProjectList[["DataFolderPath"]], "SourceData.csv"))
        if(!is.null(XREGS())) data.table::fwrite(XREGS(), file = file.path(ProjectList[["DataFolderPath"]], "XREGS.csv"))
        if(!is.null(Eval())) data.table::fwrite(Eval(), file = file.path(ProjectList[["DataFolderPath"]], "EvalData.csv"))

        # Save ProjectList to File ----
        save(ProjectList, file = file.path(ProjectList[["MetaDataPath"]], "ProjectList.Rdata"))

        # Notification ----
        shinyWidgets::sendSweetAlert(session, title = NULL, text = "Data successfully transferred and inputs saved!", type = "success", btn_labels = "Ok", btn_colors = "green", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "60%")

      } else {

        # Notification that project isn't open ----
        shinyWidgets::sendSweetAlert(session, title = NULL, text = "A Project Needs to Be Created or Loaded to Utilize This Feature", type = "error", btn_labels = "Ok", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "60%")
      }
    })

    # Next and Previous Buttons ----
    shiny::observeEvent(input$link_to_automated_forecasting_data_import, {
      updateTabItems(session, inputId = "modelMenu", selected = "automated_forecasting_data_import")
    })
    shiny::observeEvent(input$link_to_automated_forecasting_data_import_1, {
      updateTabItems(session, inputId = "modelMenu", selected = "automated_forecasting_data_import")
    })

  #----

  # AUTOMATED FORECASTING - DATA ANALYSIS ----

    # TrainData Data Management ----
    output$timeSeriesTimeUnit <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "timeSeriesTimeUnit", Label = "Time Series Frequency", Choices = c("minutes","hours","days","weeks","months","quarters","years"), SelectedDefault = "days", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$timeSeriesTarget <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "timeSeriesTarget", Label = "Select Target Variable", Choices = names(SourceData), SelectedDefault = if(exists("ProjectList")) ProjectList[["timeSeriesTarget"]] else names(SourceData), Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$timeSeriesDateColumn <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "timeSeriesDateColumn",Label = "Select Date Variable", Choices = names(SourceData), SelectedDefault = if(exists("ProjectList")) ProjectList[["timeSeriesDateColumn"]] else names(SourceData), Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$timeSeriesGroupVars  <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "timeSeriesGroupVars",Label = "Select Group Variables", Choices = names(SourceData), SelectedDefault = if(exists("ProjectList")) ProjectList[["timeSeriesGroupVars"]] else NULL, Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_Group1Levels <- shiny::renderUI({
      RemixAutoML::PickerInput_GetLevels(input, NumGroupVar = 1L, InputID = "TS_Group1Levels", InputID2 = "timeSeriesGroupVars", Choices = c(as.character(unique(SourceData[[eval(input$timeSeriesGroupVars[[1L]])]]))), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_Group2Levels <- shiny::renderUI({
      RemixAutoML::PickerInput_GetLevels(input, NumGroupVar = 2L, InputID = "TS_Group2Levels", InputID2 = "timeSeriesGroupVars", Choices = c(as.character(unique(SourceData[[eval(input$timeSeriesGroupVars[[2L]])]]))), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_Group3Levels <- shiny::renderUI({
      RemixAutoML::PickerInput_GetLevels(input, NumGroupVar = 3L, InputID = "TS_Group3Levels", InputID2 = "timeSeriesGroupVars", Choices = c(as.character(unique(SourceData[[eval(input$timeSeriesGroupVars[[3L]])]]))), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })

    # XREGS Data Management ----
    output$timeSeriesTarget_XREGS <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "timeSeriesTarget_XREGS", Label = "Select Numeric Independent Variables", Choices = names(xregs), SelectedDefault = if(exists("ProjectList")) ProjectList[["timeSeriesTarget_XREGS"]] else NULL, Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$timeSeriesDateColumn_XREGS <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "timeSeriesDateColumn_XREGS",Label = "Select Date Variable", Choices = names(xregs), SelectedDefault = if(exists("ProjectList")) ProjectList[["timeSeriesDateColumn_XREGS"]] else names(xregs), Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$timeSeriesGroupVars_XREGS  <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "timeSeriesGroupVars_XREGS",Label = "Select Group Variables", Choices = names(xregs), SelectedDefault = if(exists("ProjectList")) ProjectList[["timeSeriesGroupVars_XREGS"]] else NULL, Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_Group1Levels_XREGS <- shiny::renderUI({
      RemixAutoML::PickerInput_GetLevels(input, NumGroupVar = 1L, InputID = "TS_Group1Levels_XREGS", InputID2 = "timeSeriesGroupVars_XREGS", Choices = c(as.character(unique(xregs[[eval(input$timeSeriesGroupVars_XREGS[[1L]])]]))), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_Group2Levels_XREGS <- shiny::renderUI({
      RemixAutoML::PickerInput_GetLevels(input, NumGroupVar = 2L, InputID = "TS_Group2Levels_XREGS", InputID2 = "timeSeriesGroupVars_XREGS", Choices = c(as.character(unique(xregs[[eval(input$timeSeriesGroupVars_XREGS[[2L]])]]))), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_Group3Levels_XREGS <- shiny::renderUI({
      RemixAutoML::PickerInput_GetLevels(input, NumGroupVar = 3L, InputID = "TS_Group3Levels_XREGS", InputID2 = "timeSeriesGroupVars_XREGS", Choices = c(as.character(unique(xregs[[eval(input$timeSeriesGroupVars_XREGS[[3L]])]]))), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })

    # Eval Data Management ----
    output$timeSeriesTarget_Eval <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "timeSeriesTarget_Eval", Label = "Select Target Variable", Choices = names(EvalData), SelectedDefault = if(exists("ProjectList")) ProjectList[["timeSeriesTarget_Eval"]] else names(EvalData), Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$timeSeriesDateColumn_Eval <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "timeSeriesDateColumn_Eval",Label = "Select Date Variable", Choices = names(EvalData), SelectedDefault = if(exists("ProjectList")) ProjectList[["timeSeriesDateColumn_Eval"]] else names(EvalData),Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$timeSeriesGroupVars_Eval  <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "timeSeriesGroupVars_Eval",Label = "Select Group Variables", Choices = names(EvalData), SelectedDefault = if(exists("ProjectList")) ProjectList[["timeSeriesGroupVars_Eval"]] else NULL, Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_Group1Levels_Eval <- shiny::renderUI({
      RemixAutoML::PickerInput_GetLevels(input, NumGroupVar = 1L, InputID = "TS_Group1Levels_Eval", InputID2 = "timeSeriesGroupVars_Eval", Choices = c(as.character(unique(EvalData[[eval(input$timeSeriesGroupVars_XREGS[[1L]])]]))), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_Group2Levels_Eval <- shiny::renderUI({
      RemixAutoML::PickerInput_GetLevels(input, NumGroupVar = 2L, InputID = "TS_Group2Levels_Eval", InputID2 = "timeSeriesGroupVars_Eval", Choices = c(as.character(unique(EvalData[[eval(input$timeSeriesGroupVars_XREGS[[2L]])]]))), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_Group3Levels_Eval <- shiny::renderUI({
      RemixAutoML::PickerInput_GetLevels(input, NumGroupVar = 3L, InputID = "TS_Group3Levels_Eval", InputID2 = "timeSeriesGroupVars_Eval", Choices = c(as.character(unique(EvalData[[eval(input$timeSeriesGroupVars_XREGS[[3L]])]]))), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })

    # UI Button Options ----
    output$TS_AggregateFunction <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_AggregateFunction", Label = "Aggregation method", Choices = c("mean","sum"), SelectedDefault = "mean", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_OtherGroups <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_OtherGroups", Label = "Show other groups", Choices = c("FALSE","TRUE"), SelectedDefault = "FALSE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_NumberGroupsDisplay <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID = "TS_NumberGroupsDisplay",Label = "Lines to display",Step = 1,Min = 1,Max = 50,Value = 5)
    })

    # UI Plot Options ----
    output$TickMarksX <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TickMarksX", Label = "Tick marks x-axis", Choices = c("1 year","1 day","3 day","1 week","2 week","1 month","3 month","6 month","2 year","5 year","10 year","1 minute","15 minutes","30 minutes","1 hour","3 hour","6 hour","12 hour"), SelectedDefault = "1 year", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_LineWidth <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID = "TS_LineWidth", Label = "Line size", Step = 0.10, Min = 0.10, Max = 5, Value = 0.5)
    })
    output$TS_AngleY <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID = "TS_AngleY", Label = "Y-axis text angle", Step = 5, Min = 0, Max = 360, Value = 0)
    })
    output$TS_AngleX <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID = "TS_AngleX", Label = "X-axis text angle", Step = 5, Min = 0, Max = 360, Value = 35)
    })
    output$TS_TextSize <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID = "TS_TextSize", Label = "Text size", Step = 1, Min = 1, Max = 50, Value = 12)
    })
    output$TS_LegendPosition <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_LegendPosition", Label = "Legend position", Choices = c("left","right","top","bottom"), SelectedDefault = "bottom", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_LegendTextSize <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID = "TS_LegendTextSize", Label = "Legend text size", Step = 1, Min = 1, Max = 48, Value = 10)
    })

    # Color boxes ----
    output$TS_TextColor <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_TextColor", Label = "Text color", Choices = grDevices::colors(), SelectedDefault = "darkblue", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_LineColor <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_LineColor", Label = "Line color", Choices = grDevices::colors(), SelectedDefault = "blue", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_LegendTextColor <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_LegendTextColor", Label = "Legend text color", Choices = grDevices::colors(), SelectedDefault = "darkblue", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_ChartColor <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_ChartColor", Label = "Chart color", Choices = grDevices::colors(), SelectedDefault = "lightsteelblue1", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_GridColor <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_GridColor", Label = "Grid lines color", Choices = grDevices::colors(), SelectedDefault = "white", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_BackGroundColor <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_BackGroundColor", Label = "Background color", Choices = grDevices::colors(), SelectedDefault = "gray95", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_BorderColor <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_BorderColor", Label = "Border color", Choices = grDevices::colors(), SelectedDefault = "darkblue", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    # Reset Plot Settings ----
    shiny::observeEvent(eventExpr = input$TS_ResetPlotSettings, {

      if(exists("ProjectList")) {

        # TS_AggregateFunction
        updatePickerInput(session = session, inputId = "TS_AggregateFunction", label = "Aggregation method", choices = c("mean","sum"), selected = "mean")
        ProjectList[["TS_AggregateFunction"]] <<- input$TS_AggregateFunction

        # TS_LegendTextColor
        updatePickerInput(session = session, inputId = "TS_LegendTextColor", label = "Legend text color", choices = grDevices::colors(), selected = "darkblue")
        ProjectList[["TS_LegendTextColor"]] <<- input$TS_LegendTextColor

        # TS_LegendTextSize
        updateNumericInput(session = session, inputId = "TS_LegendTextSize", label = "Legend text size", value = 10, min = 1, max = 48, step = 2)
        ProjectList[["TS_LegendTextSize"]] <<- input$TS_LegendTextSize

        # Line Color
        updatePickerInput(session = session, inputId = "TS_LineColor", label = "Line Color", choices = grDevices::colors(), selected = "blue")
        ProjectList[["TS_LineColor"]] <<- input$TS_LineColor

        # TS_LineWidth
        updateNumericInput(session = session, inputId = "TS_LineWidth", label = "Line size", value = 0.5, min = 0.10, max = 5, step = 0.10)
        ProjectList[["TS_LineWidth"]] <<- input$TS_LineWidth

        # TS_TextSize
        updateNumericInput(session = session, inputId = "TS_TextSize", label = "Text size", value = 12, min = 1, max = 50, step = 1)
        ProjectList[["TS_TextSize"]] <<- input$TS_TextSize

        # TS_NumberGroupsDisplay
        updateNumericInput(session = session, inputId = "TS_NumberGroupsDisplay", label = "Lines to display", value = 5, min = 1, max = 50, step = 1)
        ProjectList[["TS_NumberGroupsDisplay"]] <<- input$TS_NumberGroupsDisplay

        # TickMarksX
        updatePickerInput(session = session, inputId = "TickMarksX", label = "Tick marks X-Axis",
                          choices = c("1 year","1 day","3 day","1 week","2 week","1 month","3 month","6 month","2 year","5 year","10 year","1 minute","15 minutes","30 minutes","1 hour","3 hour","6 hour","12 hour"),
                          selected = "1 year")
        ProjectList[["TickMarksX"]] <<- input$TickMarksX

        # TS_LegendPosition
        updatePickerInput(session = session, inputId = "TS_LegendPosition", label = "Legend position", choices = c("bottom","top","left","right"), selected = "bottom")
        ProjectList[["TS_LegendPosition"]] <<- input$TS_LegendPosition

        # TS_AngleX
        updateNumericInput(session = session, inputId = "TS_AngleX", label = "X-axis text angle", value = 35, min = 0, max = 360, step = 5)
        ProjectList[["TS_AngleX"]] <<- input$TS_AngleX

        # Y Axis angle
        updateNumericInput(session = session, inputId = "TS_AngleY", label = "X-axis text angle", value = 0, min = 0, max = 360, step = 5)
        ProjectList[["TS_AngleY"]] <<- input$TS_AngleY

        # TS_ChartColor
        updatePickerInput(session = session, inputId = "TS_ChartColor", label = "Chart color", choices = grDevices::colors(), selected = "lightsteelblue1")
        ProjectList[["TS_ChartColor"]] <<- input$TS_ChartColor

        # TS_BorderColor
        updatePickerInput(session = session, inputId = "TS_BorderColor", label = "Border color", choices = grDevices::colors(), selected = "darkblue")
        ProjectList[["TS_BorderColor"]] <<- input$TS_BorderColor

        # TS_TextColor
        updatePickerInput(session = session, inputId = "TS_TextColor", label = "Text color", choices = grDevices::colors(), selected = "darkblue")
        ProjectList[["TS_TextColor"]] <<- input$TS_TextColor

        # TS_GridColor
        updatePickerInput(session = session, inputId = "TS_GridColor", label = "Grid lines color", choices = grDevices::colors(), selected = "white")
        ProjectList[["TS_GridColor"]] <<- input$TS_GridColor

        # TS_BackGroundColor
        updatePickerInput(session = session, inputId = "TS_BackGroundColor", label = "Background color", choices = grDevices::colors(), selected = "gray95")
        ProjectList[["TS_BackGroundColor"]] <<- input$TS_BackGroundColor

        # TS_OtherGroups
        updatePickerInput(session = session, inputId = "TS_OtherGroups", label = "Show Other Groups", choices = c("FALSE","TRUE"), selected = "FALSE")
        ProjectList[["TS_OtherGroups"]] <<- input$TS_OtherGroups

        # Generate ggplot----
        output$TimeSeriesPlot <- plotly::renderPlotly({

          # Group Variable
          if("GroupVariableNames" %in% names(ProjectList)) {
            GroupVariables <- ProjectList[["GroupVariableNames"]]
          } else {
            GroupVariables <- NULL
          }

          # Generate Plot----
          TimeSeriesPlotObject <<- RemixAutoML::TimeSeriesPlotter(
            data = if(exists("PlotDataForecastFinal")) PlotDataForecastFinal else SourceData,
            TargetVariable = as.character(input$timeSeriesTarget),
            DateVariable = as.character(input$timeSeriesDateColumn),
            GroupVariables = GroupVariables,
            Aggregate = "mean",
            NumberGroupsDisplay = 5,
            LevelsToDisplay = NULL,
            OtherGroupLabel = "Other",
            DisplayOtherGroup = FALSE,
            TextSize = 12,
            LineWidth = 0.5,
            Color = "blue",
            XTickMarks = "1 year",
            Size = 12,
            AngleX = 35,
            AngleY = 0,
            ChartColor = "lightsteelblue1",
            BorderColor = "darkblue",
            TextColor = "darkblue",
            GridColor = "white",
            BackGroundColor = "gray95",
            LegendPosition = "bottom",
            LegendTextColor = "darkblue",
            LegendTextSize = 10)

          # Convert to plotly
          plotly::ggplotly(TimeSeriesPlotObject, tooltip = NULL)

        })

        # Notify user of error ----
        showNotification(ui = "Plot settings are reset!", closeButton = TRUE, duration = 2, session = session, type = "message")

      } else {
        shinyWidgets::sendSweetAlert(session, title = NULL, text = "No project is loaded", type = NULL, btn_labels = "error", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
      }
    })

    # TrainData TimeSeriesFill() and ModelDataPrep() ----
    shiny::observeEvent(eventExpr = input$DataPrep_Train, {

      # Main Check ----
      if(exists("ProjectList")) {

        # TimeSeriesFill ----
        if(!exists("TimeSeriesFillCheck")) {

          # TrainData ----
          if(exists("SourceData")) {

            # Target Variable ----
            ProjectList[["TargetVariableName"]] <<- as.character(input$timeSeriesTarget)
            SourceData[, eval(ProjectList[["TargetVariableName"]]) := as.numeric(get(ProjectList[["TargetVariableName"]]))]

            # Date Variable ----
            ProjectList[["DateVariableName"]] <<- input$timeSeriesDateColumn
            SourceData[, eval(ProjectList[["DateVariableName"]]) := as.Date(get(ProjectList[["DateVariableName"]]))]

            # TrainData Group Variables ----
            if(!is.null(input$timeSeriesGroupVars)) {
              ProjectList[["GroupVariableNames"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "timeSeriesGroupVars", Default = NULL)
              for(i in ProjectList[["GroupVariableNames"]]) {
                if(!is.character(SourceData[[eval(i)]])) {
                  SourceData[, eval(i) := as.character(get(i))]
                }
              }
            } else {
              ProjectList[["GroupVariableNames"]] <<- NULL
            }

            # Original number of rows
            oldrows <- SourceData[, .N]

            # Fill series and impute ----
            SourceData <<- RemixAutoML::TimeSeriesFill(data = SourceData, DateColumnName = input$timeSeriesDateColumn, GroupVariables = input$timeSeriesGroupVars, TimeUnit = input$timeSeriesTimeUnit, FillType = "maxmax", MaxMissingPercent = 0.10, SimpleImpute = FALSE)
            SourceData <<- RemixAutoML::ModelDataPrep(data = SourceData, Impute = TRUE, CharToFactor = FALSE, FactorToChar = FALSE, IntToNumeric = FALSE, LogicalToBinary = FALSE, DateToChar = FALSE, RemoveDates = FALSE, MissFactor = "0", MissNum = 0.0, IgnoreCols = NULL)
            TimeSeriesFillCheck <<- TRUE
            output$TS_DataRowSize <- renderUI({RemixAutoML::NumericInput(InputID = "TS_DataRowSize", Label = "Rows in TrainData", Min = 1, Max = 2000000000, Value = SourceData[,.N], Step = 1)})
            output$TS_DataColSize <- renderUI({RemixAutoML::NumericInput(InputID = "TS_DataColSize", Label = "Cols in TrainData", Min = 1, Max = 2000000000, Value = ncol(SourceData), Step = 1)})

            # New number of rows
            newrows <- SourceData[, .N]

            # Notify user of error ----
            shinyWidgets::sendSweetAlert(session, title = NULL, text = paste0("Training data has been prepared: Old rows = ",oldrows, " :: new rows = ",newrows), type = NULL, btn_labels = "success", btn_colors = "green", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "60%")
          } else {
            shinyWidgets::sendSweetAlert(session, title = NULL, text = "Training data does not exist", type = NULL, btn_labels = "error", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "60%")
          }
        } else {
          shinyWidgets::sendSweetAlert(session, title = NULL, text = "Training data already exists in session", type = NULL, btn_labels = "error", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "60%")
        }
      } else {
        shinyWidgets::sendSweetAlert(session, title = NULL, text = "No project is loaded", type = NULL, btn_labels = "error", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
      }
    })

    # XREGS TimeSeriesFill() and ModelDataPrep() ----
    shiny::observeEvent(eventExpr = input$DataPrep_XREGS, {

      # Main Check ----
      if(exists("ProjectList")) {

        # xregs ----
        if(!exists("TimeSeriesFillCheck1")) {

          # TimeSeriesFill ----
          if(exists("xregs")) {

            # Subset columns ----
            xregs <- xregs[, .SD, .SDcols = c(input$timeSeriesTarget_XREGS, input$timeSeriesDateColumn_XREGS, input$timeSeriesGroupVars_XREGS)]

            # Independent Variables ----
            ProjectList[["TargetVariableName_XREGS"]] <<- as.character(input$timeSeriesTarget_XREGS)
            for(z in ProjectList[["TargetVariableName_XREGS"]]) xregs[, eval(z) := as.numeric(get(z))]

            # Date Variable ----
            ProjectList[["DateVariableName_XREGS"]] <<- input$timeSeriesDateColumn_XREGS
            xregs[, eval(ProjectList[["DateVariableName_XREGS"]]) := as.Date(get(ProjectList[["DateVariableName_XREGS"]]))]

            # TrainData Group Variables ----
            if(!is.null(input$timeSeriesGroupVars_XREGS)) {
              ProjectList[["GroupVariableNames_XREGS"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "timeSeriesGroupVars_XREGS", Default = NULL)
              for(i in ProjectList[["GroupVariableNames_XREGS"]]) {
                if(!is.character(xregs[[eval(i)]])) {
                  xregs[, eval(i) := as.character(get(i))]
                }
              }
            } else {
              ProjectList[["GroupVariableNames_XREGS"]] <<- NULL
            }

            # Original number of rows
            oldrows <- xregs[, .N]

            # Fill series and impute ----
            xregs <<- RemixAutoML::TimeSeriesFill(data = xregs, DateColumnName = input$timeSeriesDateColumn_XREGS, GroupVariables = c(input$timeSeriesGroupVars_XREGS), TimeUnit = input$timeSeriesTimeUnit, FillType = "maxmax", MaxMissingPercent = 0.10, SimpleImpute = FALSE)
            xregs <<- RemixAutoML::ModelDataPrep(data = xregs, Impute = TRUE, CharToFactor = FALSE, FactorToChar = FALSE, IntToNumeric = FALSE, LogicalToBinary = FALSE, DateToChar = FALSE, RemoveDates = FALSE, MissFactor = "0", MissNum = 0, IgnoreCols = NULL)
            TimeSeriesFillCheck1 <<- TRUE
            output$TS_DataRowSize_XREGS <- renderUI({RemixAutoML::NumericInput(InputID = "TS_DataRowSize_XREGS", Label = "Rows in XREGS", Min = 1, Max = 2000000000, Value = xregs[,.N], Step = 1)})
            output$TS_DataColSize_XREGS <- renderUI({RemixAutoML::NumericInput(InputID = "TS_DataColSize_XREGS", Label = "Cols in XREGS", Min = 1, Max = 2000000000, Value = ncol(xregs), Step = 1)})

            # New number of rows
            newrows <- xregs[, .N]

            # Notify user of error ----
            shinyWidgets::sendSweetAlert(session, title = NULL, text = paste0("XREGS data has been prepared: Old rows = ",oldrows, " :: new rows = ",newrows), type = NULL, btn_labels = "success", btn_colors = "green", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "60%")
          } else {
            shinyWidgets::sendSweetAlert(session, title = NULL, text = "XREGS data does not exist", type = NULL, btn_labels = "error", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "60%")
          }
        } else {
          shinyWidgets::sendSweetAlert(session, title = NULL, text = "XREGS data already exists in session", type = NULL, btn_labels = "error", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "60%")
        }
      } else {
        shinyWidgets::sendSweetAlert(session, title = NULL, text = "No project is loaded", type = NULL, btn_labels = "error", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
      }
    })

    # EvalData TimeSeriesFill() and ModelDataPrep() ----
    shiny::observeEvent(eventExpr = input$DataPrep_Eval, {

      # Main Check ----
      if(exists("ProjectList")) {

        # EvalData ----
        if(!exists("TimeSeriesFillCheck2")) {

          # TimeSeriesFill ----
          if(exists("EvalData")) {

            # Independent Variables ----
            ProjectList[["TargetVariableName_Eval"]] <<- as.character(input$timeSeriesTarget_Eval)
            EvalData[, eval(ProjectList[["TargetVariableName_Eval"]]) := as.numeric(get(ProjectList[["TargetVariableName_Eval"]]))]

            # Date Variable----
            ProjectList[["DateVariableName_Eval"]] <<- input$timeSeriesDateColumn_Eval
            EvalData[, eval(ProjectList[["DateVariableName_Eval"]]) := as.Date(get(ProjectList[["DateVariableName_Eval"]]))]

            # TrainData Group Variables ----
            if(!is.null(input$timeSeriesGroupVars_Eval)) {
              ProjectList[["GroupVariableNames_Eval"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "timeSeriesGroupVars_Eval", Default = NULL)
              for(i in ProjectList[["GroupVariableNames_Eval"]]) {
                if(!is.character(EvalData[[eval(i)]])) {
                  EvalData[, eval(i) := as.character(get(i))]
                }
              }
            } else {
              ProjectList[["GroupVariableNames_Eval"]] <<- NULL
            }

            # Original number of rows
            oldrows <- EvalData[, .N]

            # Fill series and impute ----
            EvalData <<- RemixAutoML::TimeSeriesFill(data = EvalData, DateColumnName = input$timeSeriesDateColumn_Eval, GroupVariables = input$timeSeriesGroupVars_Eval, TimeUnit = input$timeSeriesTimeUnit, FillType = "maxmax", MaxMissingPercent = 0.10, SimpleImpute = FALSE)
            EvalData <<- RemixAutoML::ModelDataPrep(data = EvalData, Impute = TRUE, CharToFactor = FALSE, FactorToChar = FALSE, IntToNumeric = FALSE, LogicalToBinary = FALSE, DateToChar = FALSE, RemoveDates = FALSE, MissFactor = "0", MissNum = 0, IgnoreCols = NULL)
            TimeSeriesFillCheck2 <<- TRUE
            output$TS_DataRowSize_Eval <- renderUI({RemixAutoML::NumericInput(InputID = "TS_DataRowSize_Eval", Label = "Rows in EvalData", Min = 1, Max = 2000000000, Value = EvalData[,.N], Step = 1)})
            output$TS_DataColSize_Eval <- renderUI({RemixAutoML::NumericInput(InputID = "TS_DataColSize_Eval", Label = "Cols in EvalData", Min = 1, Max = 2000000000, Value = ncol(EvalData), Step = 1)})

            # New number of rows
            newrows <- EvalData[, .N]

            # Notify user of error ----
            shinyWidgets::sendSweetAlert(session, title = NULL, text = paste0("Evaluation data has been prepared: Old rows = ",oldrows, " :: new rows = ",newrows), type = NULL, btn_labels = "success", btn_colors = "green", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "60%")
          } else {
            shinyWidgets::sendSweetAlert(session, title = NULL, text = "Evaluation data does not exist", type = NULL, btn_labels = "error", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "60%")
          }
        } else {
          shinyWidgets::sendSweetAlert(session, title = NULL, text = "Evaluation data already exists in session", type = NULL, btn_labels = "error", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "60%")
        }
      } else {
        shinyWidgets::sendSweetAlert(session, title = NULL, text = "No project is loaded", type = NULL, btn_labels = "error", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
      }
    })

    # Create Time Series Plot, Save Plot Args to ProjectList[[]] ----
    shiny::observeEvent(input$CreateTimeSeriesPlot, {

      # Main Check ----
      if(exists("ProjectList")) {

        # Switch ----
        if(exists("Switch_Plot")) {
          Switch_Plot <- TRUE
        } else {
          Switch_Plot <- FALSE
        }

        # Remove data ----
        if(exists("PlotDataForecastFinal")) rm(PlotDataForecastFinal)

        # Ensure Target Variable is numeric ----
        if(!is.numeric(SourceData[[eval(input$timeSeriesTarget)]]) && !is.integer(SourceData[[eval(input$timeSeriesTarget)]])) {

          # Notification that target or date is not the right type ----
          shinyWidgets::sendSweetAlert(session, title = NULL, text = "Ensure that the target variable selected is numeric or integer and make sure the date variable is in date format", type = NULL, btn_labels = "error", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "60%")

        } else {

          # ProjectList Update ----

          # Essentials
          ProjectList[["timeSeriesTimeUnit"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "timeSeriesTimeUnit", Default = "days")

          # Data Management: TrainData
          ProjectList[["timeSeriesTarget"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "timeSeriesTarget", Default = NULL)
          ProjectList[["timeSeriesDateColumn"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "timeSeriesDateColumn", Default = NULL)
          ProjectList[["timeSeriesGroupVars"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "timeSeriesGroupVars", Default = NULL)

          # Data Management: xregs
          ProjectList[["timeSeriesTarget_XREGS"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "timeSeriesTarget_XREGS", Default = NULL)
          ProjectList[["timeSeriesDateColumn_XREGS"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "timeSeriesDateColumn_XREGS", Default = NULL)
          ProjectList[["timeSeriesGroupVars_XREGS"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "timeSeriesGroupVars_XREGS", Default = NULL)

          # Data Management: EvalData
          ProjectList[["timeSeriesTarget_Eval"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "timeSeriesTarget_Eval", Default = NULL)
          ProjectList[["timeSeriesDateColumn_Eval"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "timeSeriesDateColumn_Eval", Default = NULL)
          ProjectList[["timeSeriesGroupVars_Eval"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "timeSeriesGroupVars_Eval", Default = NULL)


          ProjectList[["TS_AggregateFunction"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "TS_AggregateFunction", Default = "mean")
          ProjectList[["TS_LegendTextColor"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "TS_LegendTextColor", Default = "darkblue")
          ProjectList[["TS_LegendTextSize"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "TS_LegendTextSize", Default = 10)
          ProjectList[["TS_LineColor"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "TS_LineColor", Default = "blue")
          ProjectList[["TS_LineWidth"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "TS_LineWidth", Default = 0.5)
          ProjectList[["TS_TextSize"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "TS_TextSize", Default = 12)
          ProjectList[["TS_NumberGroupsDisplay"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "TS_NumberGroupsDisplay", Default = 5)
          ProjectList[["TickMarksX"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "TickMarksX", Default = NULL)
          ProjectList[["TS_LegendPosition"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "TS_LegendPosition", Default = "bottom")
          ProjectList[["TS_AngleX"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "TS_AngleX", Default = 35)
          ProjectList[["TS_AngleY"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "TS_AngleY", Default = 0)
          ProjectList[["TS_ChartColor"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "TS_ChartColor", Default = "lightsteelblue1")
          ProjectList[["TS_BorderColor"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "TS_BorderColor", Default = "darkblue")
          ProjectList[["TS_TextColor"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "TS_TextColor", Default = "darkblue")
          ProjectList[["TS_GridColor"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "TS_GridColor", Default = "white")
          ProjectList[["TS_BackGroundColor"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "TS_BackGroundColor", Default = "gray95")
          ProjectList[["TS_OtherGroups"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "TS_OtherGroups", Default = "FALSE")

          # Save ProjectList to File ----
          save(ProjectList, file = file.path(ProjectList[["MetaDataPath"]], "ProjectList.Rdata"))

          # Plot Options ----
          Aggregate <- RemixAutoML::ReturnParam(input, VarName = "TS_AggregateFunction", Type = "character", Default = "mean", Switch = Switch_Plot)
          TargetVariable <- RemixAutoML::ReturnParam(input, VarName = "timeSeriesTarget", Type = "character", Default = TargetVariableName, Switch = Switch_Plot)
          DateVariable <- RemixAutoML::ReturnParam(input, VarName = "timeSeriesDateColumn", Type = "character", Default = DateVariableName, Switch = Switch_Plot)
          GroupVariables <- RemixAutoML::ReturnParam(input, VarName = "timeSeriesGroupVars", Type = "character", Default = tryCatch({GroupVariableNames}, error = function(x) NULL), Switch = Switch_Plot)
          Color <- RemixAutoML::ReturnParam(input, VarName = "TS_LineColor", Type = "character", Default = "blue", Switch = Switch_Plot)
          LineWidth <- RemixAutoML::ReturnParam(input, VarName = "TS_LineWidth", Type = "numeric", Default = 0.5, Switch = Switch_Plot)
          TextSize <- RemixAutoML::ReturnParam(input, VarName = "TS_TextSize", Type = "numeric", Default = 12, Switch = Switch_Plot)
          NumberGroupsDisplay <- RemixAutoML::ReturnParam(input, VarName = "TS_NumberGroupsDisplay", Type = "numeric", Default = 5, Switch = Switch_Plot)
          XTickMarks <- RemixAutoML::ReturnParam(input, VarName = "TickMarksX", Type = "character", Default = "1 year", Switch = Switch_Plot)
          LegendPosition <- RemixAutoML::ReturnParam(input, VarName = "TS_LegendPosition", Type = "character", Default = "bottom", Switch = Switch_Plot)
          AngleX <- RemixAutoML::ReturnParam(input, VarName = "TS_AngleX", Type = "numeric", Default = 35, Switch = Switch_Plot)
          AngleY <- RemixAutoML::ReturnParam(input, VarName = "TS_AngleY", Type = "numeric", Default = 0, Switch = Switch_Plot)
          ChartColor <- RemixAutoML::ReturnParam(input, VarName = "TS_ChartColor", Type = "character", Default = "lightsteelblue1", Switch = Switch_Plot)
          BorderColor <- RemixAutoML::ReturnParam(input, VarName = "TS_BorderColor", Type = "character", Default = "darkblue", Switch = Switch_Plot)
          TextColor <- RemixAutoML::ReturnParam(input, VarName = "TS_TextColor", Type = "character", Default = "darkblue", Switch = Switch_Plot)
          GridColor <- RemixAutoML::ReturnParam(input, VarName = "TS_GridColor", Type = "character", Default = "white", Switch = Switch_Plot)
          BackGroundColor <- RemixAutoML::ReturnParam(input, VarName = "TS_BackGroundColor", Type = "character", Default = "gray95", Switch = Switch_Plot)
          LegendTextColor <- RemixAutoML::ReturnParam(input, VarName = "TS_LegendTextColor", Type = "character", Default = "darkblue", Switch = Switch_Plot)
          LegendTextSize <- RemixAutoML::ReturnParam(input, VarName = "TS_LegendTextSize", Type = "numeric", Default = 10, Switch = Switch_Plot)
          TS_OtherGroups <- RemixAutoML::ReturnParam(input, VarName = "TS_OtherGroups", Type = "logical", Default = "FALSE", Switch = Switch_Plot)

          # Data Subseting ----
          PlotDataForecast <- data.table::copy(SourceData)

          # Create data for plotting ----
          PlotDataForecastFinal <- RemixAutoML::PreparePlotData(
            input,
            PlotDataForecast,
            Aggregate = Aggregate,
            TargetVariable = TargetVariable,
            DateVariable = DateVariable,
            GroupVariables = GroupVariables,
            G1Levels = "TS_Group1Levels",
            G2Levels = "TS_Group2Levels",
            G3Levels = "TS_Group3Levels")

          # Build Plot ----
          output$TimeSeriesPlot <- plotly::renderPlotly({
            if(!is.null(TargetVariable) & !is.null(DateVariable)) {
              TimeSeriesPlotObject <<- RemixAutoML::TimeSeriesPlotter(
                data = PlotDataForecastFinal,
                TargetVariable = TargetVariable,
                DateVariable = DateVariable,
                GroupVariables = GroupVariables,
                Aggregate = Aggregate,
                NumberGroupsDisplay = NumberGroupsDisplay,
                LevelsToDisplay = NULL,
                OtherGroupLabel = "OtherGroups",
                DisplayOtherGroup = as.logical(TS_OtherGroups),
                TextSize = TextSize,
                LineWidth = LineWidth,
                Color = Color,
                XTickMarks = XTickMarks,
                AngleX = AngleX,
                AngleY = AngleY,
                ChartColor = ChartColor,
                BorderColor = BorderColor,
                TextColor = TextColor,
                GridColor = GridColor,
                BackGroundColor = BackGroundColor,
                LegendPosition = LegendPosition,
                LegendTextColor = LegendTextColor,
                LegendTextSize = 10)

              # Convert to plotly ----
              plotly::ggplotly(TimeSeriesPlotObject, tooltip = NULL)

            } else {

              # Notify User that Process Has Begun ----
              shinyWidgets::sendSweetAlert(session, title = NULL, text = "You need to have a Target Variable and Date Variable selected", type = "error", btn_labels = "Ok", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "60%")
            }
          })
        }
      } else {
        shinyWidgets::sendSweetAlert(session, title = NULL, text = "No project is loaded", type = NULL, btn_labels = "error", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
      }
    })

    # Time Series Data DT ----
    output$TimeSeriesDT <- DT::renderDT(expr = {
      tryCatch({
        if(class(input$timeSeriesGroupVars) == "NULL") {
          GroupVars <- NULL
        } else {
          GroupVars <- input$timeSeriesGroupVars
        }
        RemixAutoML::ColumnSubsetDataTable(
          data = SourceData,
          TargetColumnName = input$timeSeriesTarget,
          DateColumnName = input$timeSeriesDateColumn,
          GroupVars = GroupVars)
      }, error = function(x) data.table::data.table(Target = "No data is loaded"))
    }, server = TRUE, filter = "top")

    # Next and Previous Buttons ----
    shiny::observeEvent(input$link_to_automated_forecasting_eda, {
      shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "automated_forecasting_eda")
    })
    shiny::observeEvent(input$link_to_automated_forecasting_eda_1, {
      updateTabItems(session, inputId = "modelMenu", selected = "automated_forecasting_eda")
    })

  #----

  # AUTOMATED FORECASTING - MODELING ------

    # Model Selection Tab ----
    output$TS_DataRowSize <- shiny::renderUI({RemixAutoML::NumericInput(InputID = "TS_DataRowSize", Label = "Number of Rows in Data", Min = 1, Max = 2000000000, Value = if(exists("SourceData")) SourceData[,.N] else NA, Step = 1)})
    output$TS_SimpleGrid <- shiny::renderUI({RemixAutoML::PickerInput(InputID = "TS_SimpleGrid", Label = "Tree-Based Basic Grid Tune", Choices = c("TRUE","FALSE"), SelectedDefault = "FALSE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)})
    output$TSMLModelsSelection <- shiny::renderUI({RemixAutoML::PickerInput(InputID = "TSMLModelsSelection", Label = "Supercharged ML Models", Choices = c("CatBoost-CARMA","XGBoost-CARMA","H2O-CARMA"), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)})
    output$TimeSeriesModelsSelection <- shiny::renderUI({RemixAutoML::PickerInput(InputID = "TimeSeriesModelsSelection", Label = "Supercharged Linear Models", Choices = c("Supercharged-SARIMA","Supercharged-NNET","TBATS","ETS","ARFIMA","TSLM"), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)})
    output$H2OModelSelection <- shiny::renderUI({RemixAutoML::PickerInput(InputID = "H2OModelSelection", Label = "H2O Algo Selection", Choices = c("Generalized Linear Model","Generalized Additive Model","Gradient Boosting Machine","Distributed Random Forecast","Deep Learning", "AutoML"), SelectedDefault = "Distributed Random Forecast", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)})

    # Data args ----
    output$TS_timeSeriesTarget <- shiny::renderUI({RemixAutoML::PickerInput(InputID = "TS_timeSeriesTarget", Label = "Select Target Variable", Choices = names(SourceData), SelectedDefault = ProjectList[["TargetVariableName"]], Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)})
    output$TS_timeSeriesDateColumn <- shiny::renderUI({RemixAutoML::PickerInput(InputID = "TS_timeSeriesDateColumn", Label = "Select Date Variable", Choices = names(SourceData), SelectedDefault = ProjectList[["DateVariableName"]][[1L]], Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)})
    output$TS_timeSeriesGroupVars <- shiny::renderUI({RemixAutoML::PickerInput(InputID = "TS_timeSeriesGroupVars", Label = "Select Group Variables", Choices = names(SourceData), SelectedDefault = if(!is.null(ProjectList[["GroupVariableNames"]])) ProjectList[["GroupVariableNames"]] else if(class(input$timeSeriesGroupVars) == "NULL") input$timeSeriesGroupVars, Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)})
    output$TS_TimeUnit <- shiny::renderUI({
      if(exists("ProjectList")) {
        tryCatch({
          if(!is.null(ProjectList[["TS_TimeUnit"]])) {
            pickerInput(inputId = "TS_TimeUnit", label = "Periodicity of Data", choices = c("1-Minute","5-Minutes","10-Minutes","15-Minutes","30-Minutes","Hourly","Daily","Weekly","Monthly","Quarterly","Yearly"),
                        selected = ProjectList[["TS_TimeUnit"]][[1]], options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 1"), multiple = TRUE)
          } else if(exists("SourceData")) {

            # Sort by group and take time difference for the TimeUnit UI element
            if(!is.null(ProjectList[["timeSeriesGroupVars"]])) {
              data.table::setorderv(x = SourceData, cols = c(eval(ProjectList[["timeSeriesGroupVars"]]),eval(ProjectList[["DateVariableName"]])), order = c(1,1))
              xx <- abs(as.numeric(difftime(SourceData[2,as.POSIXct(get(ProjectList[["DateVariableName"]]))],SourceData[1, as.POSIXct(get(ProjectList[["DateVariableName"]]))], units = "mins")/60/24))
            } else {
              xx <- abs(as.numeric(difftime(SourceData[2,as.POSIXct(get(ProjectList[["DateVariableName"]]))],SourceData[1, as.POSIXct(get(ProjectList[["DateVariableName"]]))], units = "mins")/60/24))
            }
            if(xx == 1) tunit <- "Daily"
            if(xx == 1/24) tunit <- "Hourly"
            if(xx == 1/24/60) tunit <- "1-Minute"
            if(xx == 1/24/60*5) tunit <- "5-Minutes"
            if(xx == 1/24/60*10) tunit <- "10-Minutes"
            if(xx == 1/24/60*15) tunit <- "5-Minutes"
            if(xx == 1/24/60*15) tunit <- "15-Minutes"
            if(xx == 1/24/60*30) tunit <- "30-Minutes"
            if(xx == 1*7) tunit <- "Weekly"
            if(xx > 1*7*4 & xx < 1*7*5) tunit <- "Monthly"
            if(xx > 1*7*4*3 & xx < 1*7*5*3) tunit <- "Quarterly"
            if(xx > 1*7*5*3) tunit <- "Yearly"
            pickerInput(inputId = "TS_TimeUnit", label = "Periodicity of Data", choices = c("1-Minute","5-Minutes","10-Minutes","15-Minutes","30-Minutes","Hourly","Daily","Weekly","Monthly","Quarterly","Yearly"), selected = tunit, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 1"), multiple = TRUE)
          } else {
            pickerInput(inputId = "TS_TimeUnit", label = "Periodicity of Data", choices = c("1-Minute","5-Minutes","10-Minutes","15-Minutes","30-Minutes","Hourly","Daily","Weekly","Monthly","Quarterly","Yearly"), selected = "Daily", options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 1"), multiple = TRUE)
          }}, error = function(x) pickerInput(inputId = "TS_TimeUnit", label = "Periodicity of Data", choices = c("1-Minute","5-Minutes","10-Minutes","15-Minutes","30-Minutes","Hourly","Daily","Weekly","Monthly","Quarterly","Yearly"), selected = "Daily", options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 1"), multiple = TRUE))

      } else if(exists("SourceData")) {

        # Sort by group and take time difference for the TimeUnit UI element
        if(!is.null(ProjectList[["timeSeriesGroupVars"]])) {
          data.table::setorderv(x = SourceData, cols = c(eval(ProjectList[["timeSeriesGroupVars"]]),eval(ProjectList[["DateVariableName"]])), order = c(1,1))
          xx <- abs(as.numeric(difftime(SourceData[2,as.POSIXct(get(ProjectList[["DateVariableName"]]))],SourceData[1, as.POSIXct(get(ProjectList[["DateVariableName"]]))], units = "mins")/60/24))
        } else {
          xx <- abs(as.numeric(difftime(SourceData[2,as.POSIXct(get(ProjectList[["DateVariableName"]]))],SourceData[1, as.POSIXct(get(ProjectList[["DateVariableName"]]))], units = "mins")/60/24))
        }
        if(xx == 1) tunit <- "Daily"
        if(xx == 1/24) tunit <- "Hourly"
        if(xx == 1/24/60) tunit <- "1-Minute"
        if(xx == 1/24/60*5) tunit <- "5-Minutes"
        if(xx == 1/24/60*10) tunit <- "10-Minutes"
        if(xx == 1/24/60*15) tunit <- "5-Minutes"
        if(xx == 1/24/60*15) tunit <- "15-Minutes"
        if(xx == 1/24/60*30) tunit <- "30-Minutes"
        if(xx == 1*7) tunit <- "Weekly"
        if(xx > 1*7*4 & xx < 1*7*5) tunit <- "Monthly"
        if(xx > 1*7*4*3 & xx < 1*7*5*3) tunit <- "Quarterly"
        if(xx > 1*7*5*3) tunit <- "Yearly"
        pickerInput(inputId = "TS_TimeUnit", label = "Periodicity of Data", choices = c("1-Minute","5-Minutes","10-Minutes","15-Minutes","30-Minutes","Hourly","Daily","Weekly","Monthly","Quarterly","Yearly"), selected = tunit, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 1"), multiple = TRUE)
      } else {
        pickerInput(inputId = "TS_TimeUnit", label = "Periodicity of Data", choices = c("1-Minute","5-Minutes","10-Minutes","15-Minutes","30-Minutes","Hourly","Daily","Weekly","Monthly","Quarterly","Yearly"), selected = "Daily", options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 1"), multiple = TRUE)
      }
    })

    # Modeling Args ----
    output$TS_HoldOutPeriods <- shiny::renderUI({RemixAutoML::NumericInput(InputID = "TS_HoldOutPeriods", Label = "Holdout Periods for Model Evaluation", Min = 1, Max = 1000000000, Step = 1, Value = if(exists("EvalData") & exists("SourceData")) floor(as.numeric(difftime(time1 = max(EvalData[[eval(input$timeSeriesDateColumn_Eval)]]), time2 = max(SourceData[[eval(input$timeSeriesDateColumn)]]), units = input$timeSeriesTimeUnit))) else if(exists("xregs") & exists("SourceData")) floor(as.numeric(difftime(time1 = max(xregs[[eval(input$timeSeriesDateColumn_Eval)]]), time2 = max(SourceData[[eval(input$timeSeriesDateColumn)]]), units = input$timeSeriesTimeUnit))) else 10)})
    output$TS_FCPeriods <- shiny::renderUI({RemixAutoML::NumericInput(InputID = "TS_FCPeriods", Label = "Forecast Periods", Min = 1, Max = 1000000000, Step = 1, Value = if(exists("EvalData") & exists("SourceData")) floor(as.numeric(difftime(time1 = max(EvalData[[eval(input$timeSeriesDateColumn_Eval)]]), time2 = max(SourceData[[eval(input$timeSeriesDateColumn)]]), units = input$timeSeriesTimeUnit))) else if(exists("xregs") & exists("SourceData")) floor(as.numeric(difftime(time1 = max(xregs[[eval(input$timeSeriesDateColumn_Eval)]]), time2 = max(SourceData[[eval(input$timeSeriesDateColumn)]]), units = input$timeSeriesTimeUnit))) else 10)})
    output$TS_MetricEval <- shiny::renderUI({RemixAutoML::PickerInput(InputID = "TS_MetricEval", Label = "Select Back-Testing Metric", Choices = c("MAPE","MAE","MSE","RMSE","R2"), SelectedDefault = if(exists("ProjectList")) ProjectList[["TS_MetricEval"]] else "MAPE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)})

    # Update Linear Models if GroupVariable is Selected ----
    shiny::observeEvent(eventExpr = input$TS_timeSeriesGroupVars, {

      # Update
      if(!is.null(input$TS_timeSeriesGroupVars)) {

        # Update Ensemble indicator
        updatePickerInput(session, inputId = "TimeSeriesModelsSelection", label = "Supercharged Linear Models", choices = NULL, selected = NULL)

      } else {

        # Update Ensemble indicator
        updatePickerInput(session, inputId = "TimeSeriesModelsSelection", label = "Supercharged Linear Models", choices = c("Supercharged-SARIMA","Supercharged-NNET","TBATS","ETS","ARFIMA","TSLM"), selected = "None")
      }
    }, ignoreNULL = FALSE)

    # ARIMA Configurations ----
    output$ARIMA_Lags <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="ARIMA_Lags",Label="Max lags", Step = 1, Value = 5, Min = 0, Max = 100)
    })
    output$ARIMA_MovingAverages <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="ARIMA_MovingAverages",Label="Max moving averages", Step = 1, Value = 5, Min = 0, Max = 100)
    })
    output$ARIMA_Differences <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="ARIMA_Differences",Label="Max differencing", Step = 1, Value = 1, Min = 0, Max = 3)
    })
    output$ARIMA_MaxFourierTerms <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="ARIMA_MaxFourierTerms",Label="Max fourier pairs", Step = 1, Value = 2, Min = 0, Max = 10)
    })
    output$ARIMA_SeasonalLags <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="ARIMA_SeasonalLags",Label="Max seasonal lags", Step = 1, Value = 1, Min = 0, Max = 100)
    })
    output$ARIMA_SeasonalMovingAverages <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="ARIMA_SeasonalMovingAverages",Label="Max seasonal moving averages", Step = 1, Value = 1, Min = 0, Max = 100)
    })
    output$ARIMA_SeasonalDifferences <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="ARIMA_SeasonalDifferences",Label="Max seasonal differences", Step = 1, Value = 1, Min = 0, Max = 10)
    })
    output$ARIMA_TrainShareEvaluate <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="ARIMA_TrainShareEvaluate",Label="Train eval split", Step = 0.01, Value = 0.50, Min = 0, Max = 1)
    })
    output$ARIMA_RunsWithoutWinner <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="ARIMA_RunsWithoutWinner",Label="Max runs without new winner", Step = 1, Value = 100, Min = 10, Max = 10000)
    })
    output$ARIMA_MaxNumberModels <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="ARIMA_MaxNumberModels",Label="Max number of models", Step = 1, Value = 100, Min = 10, Max = 10000)
    })
    output$ARIMA_MaxRunTime <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="ARIMA_MaxRunTime",Label="Max runtime mins", Step = 1, Value = 30, Min = 1, Max = 60*24)
    })

    # NNET Configurations ----
    output$NNET_Lags <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="NNET_Lags",Label="Max lags", Step = 1, Value = 5, Min = 0, Max = 100)
    })
    output$NNET_MaxFourierTerms <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="NNET_MaxFourierTerms",Label="Max fourier pairs", Step = 1, Value = 2, Min = 0, Max = 10)
    })
    output$NNET_SeasonalLags <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="NNET_SeasonalLags",Label="Max seasonal lags", Step = 1, Value = 1, Min = 0, Max = 100)
    })
    output$NNET_TrainShareEvaluate <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="NNET_TrainShareEvaluate",Label="Train eval split", Step = 0.01, Value = 0.50, Min = 0, Max = 1)
    })
    output$NNET_RunsWithoutWinner <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="NNET_RunsWithoutWinner",Label="Max runs without new winner", Step = 1, Value = 30, Min = 5, Max = 10000)
    })
    output$NNET_MaxNumberModels <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="NNET_MaxNumberModels",Label="Max models to test", Step = 1, Value = 200, Min = 10, Max = 10000)
    })
    output$NNET_MaxRunTime <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="NNET_MaxRunTime",Label="Max run time in mins", Step = 1, Value = 30, Min = 1, Max = 60*24)
    })

    # TBATS Configurations ----
    output$TBATS_Lags <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="TBATS_Lags",Label="Number of lags", Step = 1, Value = 5, Min = 0, Max = 100)
    })
    output$TBATS_MovingAverages <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="TBATS_MovingAverages",Label="Number of moving averages", Step = 1, Value = 5, Min = 0, Max = 100)
    })
    output$TBATS_TrainShareEvaluate <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="TBATS_TrainShareEvaluate",Label="Train eval split", Step = 0.01, Value = 0.50, Min = 0, Max = 1)
    })

    # ARFIMA Configurations ----
    output$ARFIMA_Lags <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="ARFIMA_Lags",Label="Number of lags", Step = 1, Value = 5, Min = 0, Max = 100)
    })
    output$ARFIMA_MovingAverages <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="ARFIMA_MovingAverages",Label="Number of moving averages", Step = 1, Value = 5, Min = 0, Max = 100)
    })
    output$ARFIMA_TrainShareEvaluate <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="ARFIMA_TrainShareEvaluate",Label="Train eval split", Step = 0.01, Value = 0.50, Min = 0, Max = 1)
    })

    # AutoCatBoostCARMA Configurations ----

    # Production Args
    output$TS_CatBoost_CARMA_TaskType <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_TaskType", Label = "Train Models with GPU or CPU", Choices = c("GPU","CPU"), SelectedDefault = "CPU", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_CatBoost_CARMA_NumGPU <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_NumGPU", Label = "Number of GPUs Available for Training", Choices = c(1,2,3,4,5,6,7,8), SelectedDefault = 1, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_CatBoost_CARMA_PDFOutputPath <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_PDFOutputPath", Label = "Save Model Insights to PDF", Choices = c("TRUE","FALSE"),SelectedDefault = "FALSE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_CatBoost_CARMA_SaveDataPath <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_SaveDataPath", Label = "Save CARMA Model Training Data", Choices = c("TRUE","FALSE"),SelectedDefault = "FALSE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_CatBoost_CARMA_NumParDepPlots <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="TS_CatBoost_CARMA_NumParDepPlots",Label="Number of Partial Dependence Plots to Save", Step = 1, Value = 25, Min = 1, Max = 10000)
    })

    # Target transformation args
    output$TS_CatBoost_CARMA_Methods <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_Methods", Label = "Select transformations", Choices = c("Identity", "YeoJohnson", "BoxCox", "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit"), SelectedDefault = "Identity", Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_CatBoost_CARMA_Difference <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_Difference", Label = "Target differencing", Choices = c("FALSE","TRUE"), SelectedDefault = "FALSE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_CatBoost_CARMA_NonNegativePrep <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_NonNegativePrep", Label = "Do not allow for negative predictions", Choices = c("TRUE","FALSE"), SelectedDefault = "FALSE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_CatBoost_CARMA_RoundPreds <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_RoundPreds", Label = "Round predictions to integers", Choices = c("TRUE","FALSE"), SelectedDefault = "FALSE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    # Calendar-related args
    output$TS_CatBoost_CARMA_CalendarVariables <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_CalendarVariables", Label = "Calendar variables", Choices = c("minute","hour","wday","mday","yday","week","isoweek","wom","month","quarter","year"), SelectedDefault = c("minute","hour","wday","mday","yday","week","wom","month","quarter"), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_CatBoost_CARMA_HolidayVariables <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_HolidayVariables", Label = "Holiday variables", Choices = c("USPublicHolidays","EasterGroup","ChristmasGroup","OtherEcclesticalFeasts"), SelectedDefault = "USPublicHolidays", Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_CatBoost_CARMA_HolidayLags <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_HolidayLags", Label = "Select Holiday Count Lags", Choices = as.character(1:50), SelectedDefault = c("1","2"), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_CatBoost_CARMA_HolidayMovingAverages <- renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_HolidayMovingAverages", Label = "Select Holiday Count MA's", Choices = as.character(2:50), SelectedDefault = c("2","3"), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })

    # Lags, moving averages, and other rolling stats
    output$TS_CatBoost_CARMA_Lags <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_Lags", Label = "Select lags", Choices = c(1:1000), SelectedDefault = c(1:10), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_CatBoost_CARMA_MovingAverages <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_MovingAverages", Label = "Moving average windows", Choices = c(2:1000), SelectedDefault = c(2:10), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_CatBoost_CARMA_MovingSD <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_MovingSD", Label = "Standard deviation windows", Choices = c(2:1000), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_CatBoost_CARMA_MovingSkew <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_MovingSkew", Label = "Skewness windows", Choices = c(3:1000), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_CatBoost_CARMA_MovingKurt <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_MovingKurt", Label = "Kurtosis windows", Choices = c(4:1000), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_CatBoost_CARMA_MovingQuantiles <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_MovingQuantiles", Label = "Percentile windows", Choices = c(5:1000), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_CatBoost_CARMA_Quantiles_Selected <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_Quantiles_Selected", Label = "Select percentiles", Choices = c("q5","q10","q15","q20","q25","q30","q35","q40","q45","q50","q55","q60","q65","q70","q75","q80","q85","q90","q95"),SelectedDefault = "q50", Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_CatBoost_CARMA_HierarchGroups <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_HierarchGroups", Label = "Hierarchical Rolling Stats", Choices = c("FALSE","TRUE"), SelectedDefault = "FALSE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    # Second time agg: Lags, moving averages, and other rolling stats
    output$TS_CatBoost_CARMA_Lags1 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_Lags1", Label = "Lag windows for second time agg", Choices = c(1:1000), SelectedDefault = c(1:5), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_CatBoost_CARMA_MovingAverages1 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_MovingAverages1", Label = "Moving average windows for second time agg", Choices = c(2:1000), SelectedDefault = c(2:5), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_CatBoost_CARMA_MovingSD1 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_MovingSD1", Label = "Standard deviation windows for second time agg", Choices = c(2:1000), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_CatBoost_CARMA_MovingSkew1 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_MovingSkew1", Label = "Skewness windows for second time agg", Choices = c(3:1000), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_CatBoost_CARMA_MovingKurt1 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_MovingKurt1", Label = "Kurtosis windows for second time agg", Choices = c(4:1000), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_CatBoost_CARMA_MovingQuantiles1 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_MovingQuantiles1", Label = "Percentile windows for second time agg", Choices = c(5:1000), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })

    # Bonus features
    output$TS_CatBoost_CARMA_AnomalyDetection_HighThreshold <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_AnomalyDetection_HighThreshold", Label = "Select tstat for upper threshold", Choices = c(0,3,4,5,6),SelectedDefault = 0, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_CatBoost_CARMA_AnomalyDetection_LowThreshold <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_AnomalyDetection_LowThreshold", Label = "Select tstat for upper threshold", Choices = c(0,-3,-4,-5,-6),SelectedDefault = 0, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_CatBoost_CARMA_Fourier <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_Fourier", Label = "Fourier pairs", Choices = c(0,seq(1,25,1)),SelectedDefault = c(0), Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_CatBoost_CARMA_TimeTrend <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_TimeTrend", Label = "Time Trend", Choices = c("FALSE","TRUE"),SelectedDefault = "TRUE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_CatBoost_CARMA_DataTruncate <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_DataTruncate", Label = "Data Truncation", Choices = c("FALSE","TRUE"),SelectedDefault = "FALSE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_CatBoost_CARMA_TimeWeights <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_TimeWeights", Label = "Select time weights", Choices = c(1,0.9999,0.9995,0.999,0.995,0.99,0.975,0.95),SelectedDefault = 1, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    # ML grid tuning args
    output$TS_CatBoost_CARMA_GridTune <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_GridTune", Label = "Grid Tune", Choices = c("FALSE","TRUE"),SelectedDefault = "FALSE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_CatBoost_CARMA_PassInGrid <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_PassInGrid", Label = "PassInGrid from previous GridTune", Choices = c("NULL", file.path(ProjectList[["DataFolderPath"]],"Grid.csv")), SelectedDefault = "NULL", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_CatBoost_CARMA_MaxRunsWithoutNewWinner <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_MaxRunsWithoutNewWinner", Label = "Max runs without new winner", Choices = c(seq(10,100,10)),SelectedDefault = 10, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_CatBoost_CARMA_MaxRunMinutes <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_MaxRunMinutes", Label = "Max runtime in minutes", Choices = c(seq(60*24,60*24*7,60*24)),SelectedDefault = 60*24, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_CatBoost_CARMA_ModelCount <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="TS_CatBoost_CARMA_ModelCount",Label="Max number of models", Step = 1, Value = 5, Min = 1, Max = 250)
    })

    # ML loss functions
    output$TS_CatBoost_CARMA_EvalMetric <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_EvalMetric", Label = "Eval metric for feature selection", Choices = c("RMSE","MAE","MAPE","Poisson","Quantile","LogLinQuantile","Lq","NumErrors","SMAPE","R2","MSLE","MedianAbsoluteError"),SelectedDefault = "RMSE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_CatBoost_CARMA_EvalMetricValue <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_EvalMetricValue", Label = "Eval metric value", Choices = c(1,2,3,4,5,10,15,20,25,50,75,100),SelectedDefault = 1, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_CatBoost_CARMA_LossFunction <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_LossFunction", Label = "Loss function for model", Choices = c('RMSE','MAE','Quantile','LogLinQuantile','MAPE','Poisson','PairLogitPairwise','Tweedie','QueryRMSE'),SelectedDefault = "RMSE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_CatBoost_CARMA_LossFunctionValue <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_LossFunctionValue", Label = "Loss function value", Choices = c(1,2,3,4,5,10,15,20,25,50,75,100),SelectedDefault = 1, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    # ML tuning args
    output$TS_CatBoost_CARMA_NTrees <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="TS_CatBoost_CARMA_NTrees",Label="Input number of trees", Step = 1, Value = 1000, Min = 50, Max = 250000)
    })
    output$TS_CatBoost_CARMA_Langevin <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_Langevin", Label = "Langevin boosting", Choices = c("TRUE","FALSE"),SelectedDefault = "FALSE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_CatBoost_CARMA_DiffusionTemperature <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_DiffusionTemperature", Label = "Diffusion temperature", Choices = c(seq(2500,25000,2500)),SelectedDefault = 10000, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_CatBoost_CARMA_Depth <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_Depth", Label = "Depth", Choices = c(seq(4,16,1)),SelectedDefault = 6, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    # ML overfitting args
    output$TS_CatBoost_CARMA_L2_Leaf_Reg <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_L2_Leaf_Reg", Label = "L2 leaf regularization", Choices = c(seq(0.0,10.0,1.0)), SelectedDefault = 3.0, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_CatBoost_CARMA_LearningRate <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_LearningRate", Label = "Learning rate", Choices = c(seq(0.0,0.30,0.01)), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_CatBoost_CARMA_RandomStrength <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_RandomStrength", Label = "Random strength", Choices = c(seq(0.05,1.0,0.05)),SelectedDefault = 1, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_CatBoost_CARMA_BorderCount <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_BorderCount", Label = "Border count", Choices = c(seq(32,256,32)), SelectedDefault = 254, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_CatBoost_CARMA_RSM <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_RSM", Label = "Random subspace method", Choices = c(seq(0.05,1.0,0.05)),SelectedDefault = 1, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_CatBoost_CARMA_SamplingUnit <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_SamplingUnit", Label = "Sampling unit", Choices = c("Group","Object"), SelectedDefault = "Object", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_CatBoost_CARMA_SubSample <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_SubSample", Label = "Subsample", Choices = c(seq(0.50,1.0,0.01)),SelectedDefault = 0.66, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_CatBoost_CARMA_ModelSizeReg <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_ModelSizeReg", Label = "Model size regularization", Choices = c(seq(0.0,3.0,0.05)),SelectedDefault = 0.50, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_CatBoost_CARMA_MinDataInLeaf <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_MinDataInLeaf", Label = "Min data in leaf", Choices = c(seq(1,100,1)),SelectedDefault = 1, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    # ML style args
    output$TS_CatBoost_CARMA_BootStrapType <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_BootStrapType", Label = "Boot strap type", Choices = c("Bernoulli","MVS","Bayesian","Poisson","No"),SelectedDefault = "Bayesian", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_CatBoost_CARMA_GrowPolicy <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_GrowPolicy", Label = "Grow policy", Choices = c("SymmetricTree","Depthwise","Lossguide"),SelectedDefault = "SymmetricTree", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_CatBoost_CARMA_FeatureBorderType <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_FeatureBorderType", Label = "Feature border type", Choices = c("GreedyLogSum","Median","Uniform","UniformAndQuantiles","MaxLogSum","MinEntropy"),SelectedDefault = "GreedyLogSum", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_CatBoost_CARMA_ScoreFunction <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_ScoreFunction", Label = "Score function", Choices = c("Cosine","L2","NewtonL2","NewtonCosine"),SelectedDefault = "Cosine", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    # AutoXGBoostCARMA Configurations ----
    output$TS_XGBoost_CARMA_NonNegativePrep <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_NonNegativePrep", Label = "Do not allow for negative predictions", Choices = c("TRUE","FALSE"),SelectedDefault = "FALSE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_XGBoost_CARMA_RoundPreds <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_RoundPreds", Label = "Round predictions", Choices = c("TRUE","FALSE"),SelectedDefault = "FALSE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_XGBoost_CARMA_GridTune <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_GridTune", Label = "Grid tune", Choices = c("FALSE","TRUE"),SelectedDefault = "FALSE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_XGBoost_CARMA_DataTruncate <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_DataTruncate", Label = "Truncate data", Choices = c("FALSE","TRUE"),SelectedDefault = "TRUE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_XGBoost_CARMA_Difference <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_Difference", Label = "Select holiday count lags", Choices = c("TRUE","FALSE"),SelectedDefault = "FALSE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_XGBoost_CARMA_Fourier <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_Fourier", Label = "Fourier pairs", Choices = c(0,seq(1,25,1)),SelectedDefault = c(0), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })


    output$TS_XGBoost_CARMA_NTrees <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="TS_XGBoost_CARMA_NTrees",Label="Input number of trees", Step = 1, Value = 1000, Min = 50, Max = 250000)
    })


    output$TS_XGBoost_CARMA_HolidayLags <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_HolidayLags", Label = "Select Holiday Count Lags", Choices = as.character(0:50),SelectedDefault = as.character(c(1,2)), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_XGBoost_CARMA_HolidayMovingAverages <- renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_HolidayMovingAverages", Label = "Select Holiday Count MA's", Choices = as.character(0:50),SelectedDefault = as.character(c(1,2)), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_XGBoost_CARMA_Transformation <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_Transformation", Label = "Transformation", Choices = c("TRUE","FALSE"),SelectedDefault = c("FALSE"), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_XGBoost_CARMA_CalendarVariables <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_CalendarVariables", Label = "Calendar variables", Choices = c("second","minute","hour","wday","mday","yday","week","isoweek","month","quarter","year"),SelectedDefault = c("second","minute","hour","wday","mday","yday","week","isoweek","month","quarter","year"), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_XGBoost_CARMA_HolidayVariables <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_HolidayVariables", Label = "Holiday variables", Choices = c("USPublicHolidays","EasterGroup","ChristmasGroup","OtherEcclesticalFeasts"),SelectedDefault = "USPublicHolidays", Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_XGBoost_CARMA_TimeTrend <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_TimeTrend", Label = "Time Trend", Choices = c("FALSE","TRUE"),SelectedDefault = "TRUE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_XGBoost_CARMA_Lags <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_Lags", Label = "Lag windows", Choices = c(1:10),SelectedDefault = c(1:10), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_XGBoost_CARMA_MovingAverages <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_MovingAverages", Label = "Moving average windows", Choices = c(2:10),SelectedDefault = c(2:10), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_XGBoost_CARMA_MovingSD <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_MovingSD", Label = "Standard deviation windows", Choices = c(2:10),SelectedDefault = c(2:10), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_XGBoost_CARMA_MovingSkew <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_MovingSkew", Label = "Skewness windows", Choices = c(3:10),SelectedDefault = c(3:10), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_XGBoost_CARMA_MovingKurt <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_MovingKurt", Label = "Kurtosis windows", Choices = c(4:10),SelectedDefault = c(4:10), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_XGBoost_CARMA_MovingQuantiles <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_MovingQuantiles", Label = "Percentile windows", Choices = c(3:10),SelectedDefault = c(3:10), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_XGBoost_CARMA_Quantiles_Selected <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_Quantiles_Selected", Label = "Select percentiles", Choices = c("q5","q10","q15","q20","q25","q30","q35","q40","q45","q50","q55","q60","q65","q70","q75","q80","q85","q90","q95"),SelectedDefault = "q50", Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })

    # AutoH2OCARMA Configurations ----
    output$TS_H2O_CARMA_NThreads <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="TS_H2O_CARMA_NThreads",Label="Input number of CPU threads", Step = 1, Value = max(1,parallel::detectCores()-2), Min = 1, Max = 256)
    })
    output$TS_H2O_CARMA_MaxMemory <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="TS_H2O_CARMA_MaxMemory",Label="Input the amount of memory to allocate",Step = 10, Value = 28, Min = 1, Max = 1568*2)
    })
    output$TS_H2O_CARMA_Difference <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_Difference", Label = "Target variable differencing", Choices = c("TRUE","FALSE"),SelectedDefault = "FALSE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_H2O_CARMA_ModelCount <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="TS_H2O_CARMA_ModelCount",Label="Model count for grid tuning", Step = 1, Value = 30, Min = 1, Max = 100000)
    })
    output$TS_H2O_CARMA_NTrees <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="TS_H2O_CARMA_NTrees",Label="Number of trees", Step = 1, Value = 1000, Min = 50, Max = 100000)
    })
    output$TS_H2O_CARMA_HolidayLags <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_HolidayLags", Label = "Select Holiday Count Lags", Choices = c(seq(1,50,1)), SelectedDefault = c(1,2,3), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_H2O_CARMA_HolidayMovingAverages <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_HolidayMovingAverages", Label = "Select Holiday Count moving averages", Choices = c(seq(2,50,1)), SelectedDefault = c(2,3), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_H2O_CARMA_Fourier <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_Fourier", Label = "Fourier pairs", Choices = c(0,seq(1,25,1)),SelectedDefault = c(0), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_H2O_CARMA_Transformation <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_Transformation", Label = "Transformation", Choices = c("TRUE","FALSE"),SelectedDefault = c("FALSE"), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_H2O_CARMA_CalendarVariables <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_CalendarVariables", Label = "Calendar variables", Choices = c("second","minute","hour","wday","mday","yday","week","isoweek","month","quarter","year"),SelectedDefault = c("second","minute","hour","wday","mday","yday","week","isoweek","month","quarter","year"), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_H2O_CARMA_HolidayVariables <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_HolidayVariables", Label = "Holiday variables", Choices = c("USPublicHolidays","EasterGroup","ChristmasGroup","OtherEcclesticalFeasts"),SelectedDefault = "USPublicHolidays", Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_H2O_CARMA_TimeTrend <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_TimeTrend", Label = "Time Trend", Choices = c("FALSE","TRUE"),SelectedDefault = "TRUE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_H2O_CARMA_DataTruncate <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_DataTruncate", Label = "Data Truncation", Choices = c("FALSE","TRUE"),SelectedDefault = "TRUE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_H2O_CARMA_GridTune <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_GridTune", Label = "Grid Tune", Choices = c("FALSE","TRUE"),SelectedDefault = "FALSE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_H2O_CARMA_Lags <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_Lags", Label = "Select windows", Choices = c(1:10),SelectedDefault = c(1:10), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_H2O_CARMA_MovingAverages <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_MovingAverages", Label = "Select windows", Choices = c(2:10),SelectedDefault = c(2:10), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_H2O_CARMA_MovingSD <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_MovingSD", Label = "Select windows", Choices = c(2:10),SelectedDefault = c(2:10), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_H2O_CARMA_MovingSkew <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_MovingSkew", Label = "Select windows", Choices = c(3:10),SelectedDefault = c(3:10), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_H2O_CARMA_MovingKurt <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_MovingKurt", Label = "Select windows", Choices = c(4:10),SelectedDefault = c(4:10), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_H2O_CARMA_MovingQuantiles <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_MovingQuantiles", Label = "Select windows", Choices = c(3:10),SelectedDefault = c(3:10), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_H2O_CARMA_Quantiles_Selected <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_Quantiles_Selected", Label = "Select percentiles", Choices = c("q5","q10","q15","q20","q25","q30","q35","q40","q45","q50","q55","q60","q65","q70","q75","q80","q85","q90","q95"),SelectedDefault = "q50", Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })

    # ProjectList[[]] Save Model Settings ----
    shiny::observeEvent({
      input$TS_Build
      input$TS_SaveModelSettings}, {

        # Single Click Solution Args ----
        ProjectList[["EnsembleTimeSeriesModelsSelection"]] <<- input$EnsembleTimeSeriesModelsSelection
        ProjectList[["TS_MetricEval"]] <<- input$TS_MetricEval
        ProjectList[["TS_CARMA_TaskType"]] <<- input$TS_CARMA_TaskType

        # Model Selection Args ----
        ProjectList[["TimeSeriesModelsSelection"]] <<- input$TimeSeriesModelsSelection
        ProjectList[["TSMLModelsSelection"]] <<- input$TSMLModelsSelection

        # Required Args ----
        ProjectList[["TS_timeSeriesTarget"]] <<- input$TS_timeSeriesTarget
        ProjectList[["TS_timeSeriesDateColumn"]] <<- input$TS_timeSeriesDateColumn
        ProjectList[["TS_timeSeriesGroupVars"]] <<- input$TS_timeSeriesGroupVars
        ProjectList[["TS_FCPeriods"]] <<- input$TS_FCPeriods
        ProjectList[["TS_TimeUnit"]] <<- input$TS_TimeUnit
        ProjectList[["TS_HoldOutPeriods"]] <<- input$TS_HoldOutPeriods
        ProjectList[["H2OModelSelection"]] <<- input$H2OModelSelection

        # CatBoost CARMA Args ----

        # Productionize args
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_TaskType", Type = "character", Default = "CPU")
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_NumGPU", Type = "numeric", Default = 1)

        # Calendar features
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_CalendarVariables", Type = "character", Default = c("second","minute","hour","wday","mday","yday","week","wom","month","quarter"))
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_HolidayVariables", Type = "character", Default = c("USPublicHolidays"))
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_HolidayLags", Type = "character", Default = "1")
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_HolidayMovingAverages", Type = "character", Default = c("2","3"))

        # Time series features
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_Lags", Type = "character", Default = as.character(1:10))
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_MovingAverages", Type = "character", Default = as.character(2:10))
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_MovingSD", Type = "character", Default = NULL)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_MovingSkew", Type = "character", Default = NULL)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_MovingKurt", Type = "character", Default = NULL)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_MovingQuantiles", Type = "character", Default = NULL)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_Lags1", Type = "character", Default = as.character(1:5))
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_MovingAverages1", Type = "character", Default = as.character(2:5))
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_MovingSD1", Type = "character", Default = NULL)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_MovingSkew1", Type = "character", Default = NULL)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_MovingKurt1", Type = "character", Default = NULL)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_MovingQuantiles1", Type = "character", Default = NULL)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_HierarchGroups", Type = "character", Default = "FALSE")

        # Target transformations
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_Methods", Type = "character", Default = c("Identity"))
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_Difference", Type = "character", Default = "FALSE")
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_NonNegativePrep", Type = "character", Default = "FALSE")
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_RoundPreds", Type = "character", Default = "TRUE")
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_TimeWeights", Type = "numeric", Default = 0.9999)

        # Bonus features
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_Fourier", Type = "numeric", Default = 10)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_TimeTrend", Type = "character", Default = "TRUE")
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_DataTruncate", Type = "character", Default = "FALSE")
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_AnomalyDetection_HighThreshold", Type = "numeric", Default = 0)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_AnomalyDetection_LowThreshold", Type = "numeric", Default = 0)

        # ML grid tuning
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_GridTune", Type = "character", Default = "FALSE")
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_ModelCount", Type = "numeric", Default = 5)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_PassInGrid", Type = "character", Default = "NULL")
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_MaxRunsWithoutNewWinner", Type = "numeric", Default = 25)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_MaxRunMinutes", Type = "numeric", Default = 60*24)

        # ML insights
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_PDFOutputPath", Type = "character", Default = "NULL")
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_SaveDataPath", Type = "character", Default = "NULL")
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_NumParDepPlots", Type = "numeric", Default = 25)

        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_EvalMetricValue", Type = "numeric", Default = 1)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_LossFunction", Type = "character", Default = "RMSE")
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_LossFunctionValue", Type = "numeric", Default = 1)

        # ML tuning
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_NTrees", Type = "numeric", Default = 1000)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_Langevin", Type = "character", Default = "FALSE")
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_DiffusionTemperature", Type = "numeric", Default = 10000)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_Depth", Type = "numeric", Default = 9)

        # ML Overfitting
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_L2_Leaf_Reg", Type = "numeric", Default = 3.0)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_LearningRate", Type = "numeric", Default = 0.03)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_RandomStrength", Type = "numeric", Default = 1)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_BorderCount", Type = "numeric", Default = 254)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_RSM", Type = "numeric", Default = 1)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_ModelSizeReg", Type = "numeric", Default = 0.5)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_SamplingUnit", Type = "character", Default = "Object")
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_SubSample", Type = "numeric", Default = 0.66)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_MinDataInLeaf", Type = "numeric", Default = 1)

        # ML Style
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_BootStrapType", Type = "character", Default = "Bayesian")
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_GrowPolicy", Type = "character", Default = "SymmetricTree")
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_FeatureBorderType", Type = "character", Default = "GreedyLogSum")
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CatBoost_CARMA_ScoreFunction", Type = "character", Default = "Cosine")

        # ARIMA Args ----
        RemixAutoML::StoreArgs(input, ProjectList, "ARIMA_Lags", "numeric", 5)
        RemixAutoML::StoreArgs(input, ProjectList, "ARIMA_MovingAverages", "numeric", 5)
        RemixAutoML::StoreArgs(input, ProjectList, "ARIMA_Differences", "numeric", 1)
        RemixAutoML::StoreArgs(input, ProjectList, "ARIMA_MaxFourierTerms", "numeric", 2)
        RemixAutoML::StoreArgs(input, ProjectList, "ARIMA_SeasonalLags", "numeric", 1)
        RemixAutoML::StoreArgs(input, ProjectList, "ARIMA_SeasonalMovingAverages", "numeric", 1)
        RemixAutoML::StoreArgs(input, ProjectList, "ARIMA_SeasonalDifferences", "numeric", 1)
        RemixAutoML::StoreArgs(input, ProjectList, "ARIMA_TrainShareEvaluate", "numeric", 0.50)
        RemixAutoML::StoreArgs(input, ProjectList, "ARIMA_RunsWithoutWinner", "numeric", 30)
        RemixAutoML::StoreArgs(input, ProjectList, "ARIMA_MaxNumberModels", "numeric", 100)
        RemixAutoML::StoreArgs(input, ProjectList, "ARIMA_MaxRunTime", "numeric", 30)

        # NNET Args ----
        RemixAutoML::StoreArgs(input, ProjectList, "NNET_Lags", "numeric", 5)
        RemixAutoML::StoreArgs(input, ProjectList, "NNET_SeasonalLags", "numeric", 2)
        RemixAutoML::StoreArgs(input, ProjectList, "NNET_MaxFourierTerms", "numeric", 2)
        RemixAutoML::StoreArgs(input, ProjectList, "NNET_TrainShareEvaluate", "numeric", 0.50)
        RemixAutoML::StoreArgs(input, ProjectList, "NNET_RunsWithoutWinner", "numeric", 30)
        RemixAutoML::StoreArgs(input, ProjectList, "NNET_MaxNumberModels", "numeric", 200)
        RemixAutoML::StoreArgs(input, ProjectList, "NNET_MaxRunTime", "numeric", 30)

        # TBATS Args ----
        RemixAutoML::StoreArgs(input, ProjectList, "TBATS_Lags", "numeric", 5)
        RemixAutoML::StoreArgs(input, ProjectList, "TBATS_MovingAverages", "numeric", 5)
        RemixAutoML::StoreArgs(input, ProjectList, "TBATS_TrainShareEvaluate", "numeric", 0.50)

        # ARFIMA Args ----
        RemixAutoML::StoreArgs(input, ProjectList, "ARFIMA_Lags", "numeric", 5)
        RemixAutoML::StoreArgs(input, ProjectList, "ARFIMA_MovingAverages", "numeric", 5)
        RemixAutoML::StoreArgs(input, ProjectList, "ARFIMA_TrainShareEvaluate", "numeric", 0.50)

        # Notify user ----
        showNotification(ui = "Model settings are updated!", closeButton = TRUE, duration = 4, session = session, type = "message")
    })

    # Build Forecasts and Save to Forecasts ----
    shiny::observeEvent(input$TS_Build, {

      # Define Time Series Models ----
      TS_Models <- input$TimeSeriesModelsSelection
      TS_Models <- TS_Models[!(TS_Models %chin% "None")]
      ProjectList[["TS_Models"]] <<- TS_Models

      # Define ML Models ----
      ML_Models <- input$TSMLModelsSelection
      ML_Models <- ML_Models[!(ML_Models %chin% "None")]
      ProjectList[["ML_Models"]] <<- ML_Models

      # Number of models ----
      n <- length(TS_Models) + length(ML_Models)

      # Save ProjectList to File ----
      save(ProjectList, file = file.path(ProjectList[["MetaDataPath"]], "ProjectList.Rdata"))

      # TS Core Variables ----
      TargetName <- input$TS_timeSeriesTarget
      DateName <- input$TS_timeSeriesDateColumn
      if(class(input$TS_timeSeriesGroupVars) == "NULL") {
        GroupVariableNames <- NULL
      } else {
        GroupVariableNames <- input$TS_timeSeriesGroupVars
      }

      # Store data for modeling ----
      data <- RemixAutoML::ColumnSubsetDataTable(
        data = SourceData,
        TargetColumnName = TargetName,
        DateColumnName = DateName,
        GroupVars = GroupVariableNames)
      data[, eval(TargetName) := as.numeric(get(TargetName))]

      # Identify min, average, and max values of training data ----
      MinValue <- min(data[[eval(TargetName)]], na.rm = TRUE)
      AvgValue <- mean(data[[eval(TargetName)]], na.rm = TRUE)
      MaxValue <- max(data[[eval(TargetName)]], na.rm = TRUE)

      # Required User Inputs ----
      TS_FCPeriods <- input$TS_FCPeriods
      TS_HoldOutPeriods <- input$TS_HoldOutPeriods
      TS_MetricEval <- input$TS_MetricEval
      TS_TimeUnit <- c()
      if(any(input$TS_TimeUnit == "Hourly")) TS_TimeUnit <- c(TS_TimeUnit, "hour")
      if(input$TS_TimeUnit == "1-Minutes") TS_TimeUnit <- c(TS_TimeUnit, "1min")
      if(input$TS_TimeUnit == "5-Minutes") TS_TimeUnit <- c(TS_TimeUnit, "5min")
      if(input$TS_TimeUnit == "10-Minutes") TS_TimeUnit <- c(TS_TimeUnit, "10min")
      if(input$TS_TimeUnit == "15-Minutes") TS_TimeUnit <- c(TS_TimeUnit, "15min")
      if(input$TS_TimeUnit == "30-Minutes") TS_TimeUnit <- c(TS_TimeUnit, "30min")
      if(input$TS_TimeUnit == "Daily") TS_TimeUnit <- c(TS_TimeUnit, "days")
      if(input$TS_TimeUnit == "Weekly") TS_TimeUnit <- c(TS_TimeUnit, "weeks")
      if(input$TS_TimeUnit == "Monthly") TS_TimeUnit <- c(TS_TimeUnit, "months")
      if(input$TS_TimeUnit == "Quarterly") TS_TimeUnit <- c(TS_TimeUnit, "quarters")
      if(input$TS_TimeUnit == "Yearly") TS_TimeUnit <- c(TS_TimeUnit, "years")

      # Model Building ----
      withProgress(message = 'Model Building', value = 0, {

        # Check for duplicate dates ----
        if(is.null(GroupVariableNames)) {
          x <- length(SourceData[[eval(DateName)]])
          xx <- length(unique(SourceData[[eval(DateName)]]))
          if(x != xx) {

            # Store decision
            GoodToGo <<- FALSE

            # Notification that project isn't open ----
            shinyWidgets::sendSweetAlert(session,title = NULL, text = "Duplicate dates detected. Did you select a grouping variable?", type = "error", btn_labels = "Ok", btn_colors = "red", html = FALSE,closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "60%")
          } else {
            GoodToGo <<- TRUE
          }
        } else {
          GoodToGo <<- TRUE
        }

        # Build models ----
        if(GoodToGo) {

          # ARIMA ----
          if("Supercharged-SARIMA" %chin% TS_Models) {

            # 0. Arima Args ----
            ARIMA_Lags <- RemixAutoML::ReturnParam(input, VarName = "ARIMA_Lags", Type = "numeric", Default = 5)
            ARIMA_MovingAverages <- RemixAutoML::ReturnParam(input, VarName = "ARIMA_MovingAverages", Type = "numeric", Default = 5)
            ARIMA_MaxFourierTerms <- RemixAutoML::ReturnParam(input, VarName = "ARIMA_MaxFourierTerms", Type = "numeric", Default = 2)
            ARIMA_SeasonalLags <- RemixAutoML::ReturnParam(input, VarName = "ARIMA_SeasonalLags", Type = "numeric", Default = 2)
            ARIMA_SeasonalMovingAverages <- RemixAutoML::ReturnParam(input, VarName = "ARIMA_SeasonalMovingAverages", Type = "numeric", Default = 2)
            ARIMA_TrainShareEvaluate <- RemixAutoML::ReturnParam(input, VarName = "ARIMA_TrainShareEvaluate", Type = "numeric", Default = 0.50)
            ARIMA_RunsWithoutWinner <- RemixAutoML::ReturnParam(input, VarName = "ARIMA_RunsWithoutWinner", Type = "numeric", Default = 30)
            ARIMA_MaxNumberModels <- RemixAutoML::ReturnParam(input, VarName = "ARIMA_MaxNumberModels", Type = "numeric", Default = 100)
            ARIMA_MaxRunTime <- RemixAutoML::ReturnParam(input, VarName = "ARIMA_MaxRunTime", Type = "numeric", Default = 10)

            # 1. Create time series artifacts ----
            Arima_Artifacts_Build <- RemixAutoML::TimeSeriesDataPrepare(
              data = data,
              TargetName = TargetName,
              DateName = DateName,
              Lags = ARIMA_Lags,
              SeasonalLags = ARIMA_SeasonalLags,
              MovingAverages = ARIMA_MovingAverages,
              SeasonalMovingAverages = ARIMA_SeasonalMovingAverages,
              TimeUnit = TS_TimeUnit[1L],
              FCPeriods = TS_FCPeriods,
              HoldOutPeriods = TS_HoldOutPeriods,
              TSClean = TRUE,
              ModelFreq = TRUE,
              FinalBuild = FALSE)

            # 2. Find Best ARIMA Models ----
            Arima_ExperimentGrid <- tryCatch({RemixAutoML::ParallelAutoARIMA(
              MetricSelection = TS_MetricEval,
              Output = Arima_Artifacts_Build,
              MaxFourierTerms = ARIMA_MaxFourierTerms,
              TrainValidateShare = c(ARIMA_TrainShareEvaluate, 1-ARIMA_TrainShareEvaluate),
              MaxNumberModels = ARIMA_MaxNumberModels,
              MaxRunMinutes = ARIMA_MaxRunTime,
              MaxRunsWithoutNewWinner = ARIMA_RunsWithoutWinner)},
              error = function(x) NULL)

            # 3. Create Final Build Data ----
            if(!is.null(Arima_ExperimentGrid)) {
              Arima_Artifacts_Score <- RemixAutoML::TimeSeriesDataPrepare(
                data = data,
                TargetName = TargetName,
                DateName = DateName,
                Lags = ARIMA_Lags,
                SeasonalLags = ARIMA_SeasonalLags,
                MovingAverages = ARIMA_MovingAverages,
                SeasonalMovingAverages = ARIMA_SeasonalMovingAverages,
                TimeUnit = TS_TimeUnit[1L],
                FCPeriods = TS_FCPeriods,
                HoldOutPeriods = 0,
                TSClean = TRUE,
                ModelFreq = TRUE,
                FinalBuild = TRUE)

              # 4. Generate Final Arima Forecasts ----
              counter <- 1L
              repeat {
                Forecast <- tryCatch({RemixAutoML::FinalBuildArima(
                  ModelOutputGrid = Arima_ExperimentGrid,
                  TimeSeriesPrepareOutput = Arima_Artifacts_Score,
                  FCPeriods = TS_FCPeriods,
                  NumberModelsScore = 1,
                  MetricSelection = TS_MetricEval,
                  ByDataType = TRUE)},
                  error = function(x) NULL)

                # Move on if model build failure ----
                if(!is.null(Forecast)) {

                  # Min, Average, Max values
                  FC_MinValue <- min(Forecast[["Forecast"]], na.rm = TRUE)
                  FC_AvgValue <- mean(Forecast[["Forecast"]], na.rm = TRUE)
                  FC_MaxValue <- max(Forecast[["Forecast"]], na.rm = TRUE)

                  # Ensure final models get build and correct grid metric is utilized
                  if(nrow(Forecast) != 0 & ((FC_MaxValue - MaxValue) * TS_FCPeriods / data[,.N]) < 10 * ((MaxValue - AvgValue))) {
                    data.table::fwrite(Forecast, file = file.path(ProjectList[["ModelsFolderPath"]],paste0(TargetName,"-Arima.csv")))
                    data.table::fwrite(Arima_ExperimentGrid, file = file.path(ProjectList[["ModelsFolderPath"]],paste0(TargetName,"-Arima_ExperimentGrid.csv")))
                    break
                  } else {
                    Arima_ExperimentGrid <- Arima_ExperimentGrid[ModelRankByDataType != eval(counter)]
                    counter <- counter + 1L
                    if(counter > 10) {
                      ProjectList[["TS_Models"]] <<- ProjectList[["TS_Models"]][!ProjectList[["TS_Models"]] %chin% "Supercharged-SARIMA"]
                      save(ProjectList, file = file.path(ProjectList[["MetaDataPath"]], "ProjectList.Rdata"))
                      break
                    }
                  }
                } else {
                  ProjectList$TS_Models <<- ProjectList$TS_Models[!ProjectList$TS_Models %chin% "Supercharged-SARIMA"]
                  break
                }
              }
            } else {
              ProjectList$TS_Models <<- ProjectList$TS_Models[!ProjectList$TS_Models %chin% "Supercharged-SARIMA"]
            }

            # 5. Update Progress Bar ----
            incProgress(amount = 1/n, message = "Arima Complete")
          }

          # NNET ----
          if("Supercharged-NNET" %chin% TS_Models) {

            # 0. NNET Args ----
            NNET_Lags <- RemixAutoML::ReturnParam(input, VarName = "NNET_Lags", Type = "numeric", Default = 5)
            NNET_MaxFourierTerms <- RemixAutoML::ReturnParam(input, VarName = "NNET_MaxFourierTerms", Type = "numeric", Default = 2)
            NNET_SeasonalLags <- RemixAutoML::ReturnParam(input, VarName = "NNET_SeasonalLags", Type = "numeric", Default = 2)
            NNET_TrainShareEvaluate <- RemixAutoML::ReturnParam(input, VarName = "NNET_TrainShareEvaluate", Type = "numeric", Default = 0.50)
            NNET_RunsWithoutWinner <- RemixAutoML::ReturnParam(input, VarName = "NNET_RunsWithoutWinner", Type = "numeric", Default = 30)
            NNET_MaxNumberModels <- RemixAutoML::ReturnParam(input, VarName = "NNET_MaxNumberModels", Type = "numeric", Default = 200)
            NNET_MaxRunTime <- RemixAutoML::ReturnParam(input, VarName = "NNET_MaxRunTime", Type = "numeric", Default = 30)

            # 1. Create time series artifacts ----
            NNET_Artifacts_Build <- RemixAutoML::TimeSeriesDataPrepare(
              data = data,
              TargetName = TargetName,
              DateName = DateName,
              Lags = NNET_Lags,
              SeasonalLags = NNET_SeasonalLags,
              MovingAverages = 1,
              SeasonalMovingAverages = 1,
              TimeUnit = TS_TimeUnit[1L],
              FCPeriods = TS_FCPeriods,
              HoldOutPeriods = TS_HoldOutPeriods,
              TSClean = TRUE,
              ModelFreq = TRUE)

            # 2. Find Best NNET Models ----
            NNET_ExperimentGrid <- tryCatch({RemixAutoML::ParallelAutoNNET(
              MetricSelection = TS_MetricEval,
              Output = NNET_Artifacts_Build,
              MaxFourierTerms = NNET_MaxFourierTerms,
              TrainValidateShare = c(NNET_TrainShareEvaluate,1 - NNET_TrainShareEvaluate),
              MaxNumberModels = NNET_MaxNumberModels,
              MaxRunMinutes = NNET_MaxRunTime,
              MaxRunsWithoutNewWinner = NNET_RunsWithoutWinner)},
              error = function(x) NULL)

            # 3. Create Final Build Data ----
            if(!is.null(NNET_ExperimentGrid)) {
              NNET_Artifacts_Score <- RemixAutoML::TimeSeriesDataPrepare(
                data = data,
                TargetName = TargetName,
                DateName = DateName,
                Lags = NNET_Lags,
                SeasonalLags = NNET_SeasonalLags,
                MovingAverages = 1,
                SeasonalMovingAverages = 1,
                TimeUnit = TS_TimeUnit[1L],
                FCPeriods = TS_FCPeriods,
                HoldOutPeriods = 0,
                TSClean = TRUE,
                ModelFreq = TRUE,
                FinalBuild = TRUE)

              # 4. Generate Final NNET Forecasts ----
              counter <- 1L
              repeat {
                Forecast <- tryCatch({RemixAutoML::FinalBuildNNET(
                  ModelOutputGrid = NNET_ExperimentGrid,
                  TimeSeriesPrepareOutput = NNET_Artifacts_Score,
                  FCPeriods = TS_FCPeriods,
                  NumberModelsScore = 1,
                  MetricSelection = TS_MetricEval,
                  ByDataType = TRUE)},
                  error = function(x) NULL)

                # Ensure final models get build and correct grid metric is utilized ----
                if(!is.null(Forecast)) {

                  # Min, Average, Max values
                  FC_MinValue <- min(Forecast[["Forecast"]], na.rm = TRUE)
                  FC_AvgValue <- mean(Forecast[["Forecast"]], na.rm = TRUE)
                  FC_MaxValue <- max(Forecast[["Forecast"]], na.rm = TRUE)

                  # Build model
                  if(nrow(Forecast) != 0 & ((FC_MaxValue - MaxValue) * TS_FCPeriods / data[,.N]) < 10 * ((MaxValue - AvgValue))) {
                    data.table::fwrite(Forecast, file = file.path(ProjectList[["ModelsFolderPath"]],paste0(TargetName,"-NNET.csv")))
                    data.table::fwrite(NNET_ExperimentGrid, file = file.path(ProjectList[["ModelsFolderPath"]],paste0(TargetName,"-NNET_ExperimentGrid.csv")))
                    break
                  } else {
                    NNET_ExperimentGrid <- NNET_ExperimentGrid[ModelRankByDataType != eval(counter)]
                    counter <- counter + 1L
                    if(counter > 10) {
                      ProjectList[["TS_Models"]] <<- ProjectList[["TS_Models"]][!ProjectList[["TS_Models"]] %chin% "Supercharged-NNET"]
                      save(ProjectList, file = file.path(ProjectList[["MetaDataPath"]], "ProjectList.Rdata"))
                      break
                    }
                  }
                } else {
                  ProjectList$TS_Models <<- ProjectList$TS_Models[!ProjectList$TS_Models %chin% "Supercharged-NNET"]
                  break
                }
              }
            } else {
              ProjectList$TS_Models <<- ProjectList$TS_Models[!ProjectList$TS_Models %chin% "Supercharged-NNET"]
            }

            # 5. Update Progress Bar ----
            incProgress(amount = 1/n, message = "NNET Complete")
          }

          # TBATS ----
          if("TBATS" %chin% TS_Models) {

            # 0. TBATS Args ----
            TBATS_Lags <- RemixAutoML::ReturnParam(input, VarName = "TBATS_Lags", Type = "numeric", Default = 5)
            TBATS_MovingAverages <- RemixAutoML::ReturnParam(input, VarName = "TBATS_MovingAverages", Type = "numeric", Default = 5)

            # 1. Create time series artifacts ----
            TBATS_Artifacts_Build <- RemixAutoML::TimeSeriesDataPrepare(
              data = data,
              TargetName = TargetName,
              DateName = DateName,
              Lags = TBATS_Lags,
              SeasonalLags = 1,
              MovingAverages = TBATS_MovingAverages,
              SeasonalMovingAverages = 1,
              TimeUnit = TS_TimeUnit[1L],
              FCPeriods = TS_FCPeriods,
              HoldOutPeriods = TS_HoldOutPeriods,
              TSClean = TRUE,
              ModelFreq = TRUE)

            # 2. Find Best TBATS Models ----
            TBATS_ExperimentGrid <- tryCatch({RemixAutoML::ParallelAutoTBATS(
              MetricSelection = TS_MetricEval,
              Output = TBATS_Artifacts_Build,
              TrainValidateShare = TBATS_TrainShareEvaluate)},
              error = function(x) NULL)

            # 3. Create Final Build Data ----
            if(!is.null(TBATS_ExperimentGrid)) {
              TBATS_Artifacts_Score <- RemixAutoML::TimeSeriesDataPrepare(
                data = data,
                TargetName = TargetName,
                DateName = DateName,
                Lags = TBATS_Lags,
                SeasonalLags = 1,
                MovingAverages = TBATS_MovingAverages,
                SeasonalMovingAverages = 1,
                TimeUnit = TS_TimeUnit[1L],
                FCPeriods = TS_FCPeriods,
                HoldOutPeriods = 0,
                TSClean = TRUE,
                ModelFreq = TRUE,
                FinalBuild = TRUE)

              # 4. Generate Final TBATS Forecasts ----
              Forecast <- tryCatch({RemixAutoML::FinalBuildTBATS(
                ModelOutputGrid = TBATS_ExperimentGrid,
                TimeSeriesPrepareOutput = TBATS_Artifacts_Score,
                FCPeriods = TS_FCPeriods,
                NumberModelsScore = 1,
                MetricSelection = TS_MetricEval,
                ByDataType = TRUE)},
                error = function(x) NULL)

              # Check whether to move on ----
              if(!is.null(Forecast)) {

                # Min, Average, Max values
                FC_MinValue <- min(Forecast[["Forecast"]], na.rm = TRUE)
                FC_AvgValue <- mean(Forecast[["Forecast"]], na.rm = TRUE)
                FC_MaxValue <- max(Forecast[["Forecast"]], na.rm = TRUE)

                # Ensure final models get build and correct grid metric is utilized ----
                if(nrow(Forecast) != 0 & ((FC_MaxValue - MaxValue) * TS_FCPeriods / data[,.N]) < 10 * ((MaxValue - AvgValue))) {
                  data.table::fwrite(Forecast, file = file.path(ProjectList[["ModelsFolderPath"]],paste0(TargetName,"-TBATS.csv")))
                  data.table::fwrite(TBATS_ExperimentGrid, file = file.path(ProjectList[["ModelsFolderPath"]],paste0(TargetName,"-TBATS_ExperimentGrid.csv")))
                } else {
                  ProjectList[["TS_Models"]] <<- ProjectList[["TS_Models"]][!ProjectList[["TS_Models"]] %chin% "TBATS"]
                  save(ProjectList, file = file.path(ProjectList[["MetaDataPath"]], "ProjectList.Rdata"))
                }
              } else {
                ProjectList$TS_Models <<- ProjectList$TS_Models[!ProjectList$TS_Models %chin% "TBATS"]
              }
            } else {
              ProjectList$TS_Models <<- ProjectList$TS_Models[!ProjectList$TS_Models %chin% "TBATS"]
            }

            # 5. Update Progress Bar ----
            incProgress(amount = 1/n, message = "TBATS Complete")
          }

          # ETS ----
          if("ETS" %chin% TS_Models) {

            # 1. Create time series artifacts ----
            ETS_Artifacts_Build <- RemixAutoML::TimeSeriesDataPrepare(
              data = data,
              TargetName = TargetName,
              DateName = DateName,
              Lags = 1,
              SeasonalLags = 1,
              MovingAverages = 1,
              SeasonalMovingAverages = 1,
              TimeUnit = TS_TimeUnit[1L],
              FCPeriods = TS_FCPeriods,
              HoldOutPeriods = TS_HoldOutPeriods,
              TSClean = TRUE,
              ModelFreq = TRUE)

            # 2. Find Best ETS Models ----
            ETS_ExperimentGrid <- tryCatch({RemixAutoML::ParallelAutoETS(
              MetricSelection = TS_MetricEval,
              Output = ETS_Artifacts_Build,
              TrainValidateShare = c(0.50,0.50))},
              error = function(x) NULL)

            # 3. Create Final Build Data ----
            if(!is.null(ETS_ExperimentGrid)) {
              ETS_Artifacts_Score <- RemixAutoML::TimeSeriesDataPrepare(
                data = data,
                TargetName = TargetName,
                DateName = DateName,
                Lags = 1,
                SeasonalLags = 1,
                MovingAverages = 1,
                SeasonalMovingAverages = 1,
                TimeUnit = TS_TimeUnit[1L],
                FCPeriods = TS_FCPeriods,
                HoldOutPeriods = 0,
                TSClean = TRUE,
                ModelFreq = TRUE,
                FinalBuild = TRUE)

              # 4. Generate Final ETS Forecasts ----
              Forecast <- tryCatch({RemixAutoML::FinalBuildETS(
                ModelOutputGrid = ETS_ExperimentGrid,
                TimeSeriesPrepareOutput = ETS_Artifacts_Score,
                FCPeriods = TS_FCPeriods,
                NumberModelsScore = 1,
                MetricSelection = TS_MetricEval,
                ByDataType = TRUE)},
                error = function(x) NULL)

              # Ensure final models get build and correct grid metric is utilized ----
              if(!is.null(Forecast)) {
                if(nrow(Forecast) != 0) {
                  FC_MinValue <- min(Forecast[["Forecast"]], na.rm = TRUE)
                  FC_AvgValue <- mean(Forecast[["Forecast"]], na.rm = TRUE)
                  FC_MaxValue <- max(Forecast[["Forecast"]], na.rm = TRUE)
                  if(((FC_MaxValue - MaxValue) * TS_FCPeriods / data[,.N]) < 10 * ((MaxValue - AvgValue))) {
                    data.table::fwrite(Forecast, file = file.path(ProjectList[["ModelsFolderPath"]],paste0(TargetName,"-ETS.csv")))
                    data.table::fwrite(ETS_ExperimentGrid, file = file.path(ProjectList[["ModelsFolderPath"]],paste0(TargetName,"-ETS_ExperimentGrid.csv")))
                  } else {
                    ProjectList[["TS_Models"]] <<- ProjectList[["TS_Models"]][!ProjectList[["TS_Models"]] %chin% "ETS"]
                    save(ProjectList, file = file.path(ProjectList[["MetaDataPath"]], "ProjectList.Rdata"))
                  }
                } else {
                  ProjectList[["TS_Models"]] <<- ProjectList[["TS_Models"]][!ProjectList[["TS_Models"]] %chin% "ETS"]
                  save(ProjectList, file = file.path(ProjectList[["MetaDataPath"]], "ProjectList.Rdata"))
                }
              } else {
                ProjectList$TS_Models <<- ProjectList$TS_Models[!ProjectList$TS_Models %chin% "ETS"]
              }
            } else {
              ProjectList$TS_Models <<- ProjectList$TS_Models[!ProjectList$TS_Models %chin% "ETS"]
            }

            # 5. Update Progress Bar ----
            incProgress(amount = 1/n, message = "ETS Complete")
          }

          # ARFIMA ----
          if("ARFIMA" %chin% TS_Models) {

            # 0. ARFIMA Args ----
            ARFIMA_Lags <- RemixAutoML::ReturnParam(input, VarName = "ARFIMA_Lags", Type = "numeric", Default = 5)
            ARFIMA_MovingAverages <- RemixAutoML::ReturnParam(input, VarName = "ARFIMA_MovingAverages", Type = "numeric", Default = 5)

            # 1. Create time series artifacts ----
            ARFIMA_Artifacts_Build <- RemixAutoML::TimeSeriesDataPrepare(
              data = data,
              TargetName = TargetName,
              DateName = DateName,
              Lags = ARFIMA_Lags,
              SeasonalLags = 1,
              MovingAverages = ARFIMA_MovingAverages,
              SeasonalMovingAverages = 1,
              TimeUnit = TS_TimeUnit[1L],
              FCPeriods = TS_FCPeriods,
              HoldOutPeriods = TS_HoldOutPeriods,
              TSClean = TRUE,
              ModelFreq = TRUE)

            # 2. Find Best Arfima Models ----
            ARFIMA_ExperimentGrid <- tryCatch({RemixAutoML::ParallelAutoArfima(
              MetricSelection = TS_MetricEval,
              Output = ARFIMA_Artifacts_Build,
              TrainValidateShare = c(0.5,0.5))},
              error = function(x) NULL)

            # 3. Create Final Build Data ----
            if(!is.null(ARFIMA_ExperimentGrid)) {
              ARFIMA_Artifacts_Score <- RemixAutoML::TimeSeriesDataPrepare(
                data = data,
                TargetName = TargetName,
                DateName = DateName,
                Lags = ARFIMA_Lags,
                SeasonalLags = 1,
                MovingAverages = ARFIMA_MovingAverages,
                SeasonalMovingAverages = 1,
                TimeUnit = TS_TimeUnit[1L],
                FCPeriods = TS_FCPeriods,
                HoldOutPeriods = 0,
                TSClean = TRUE,
                ModelFreq = TRUE,
                FinalBuild = TRUE)

              # 4. Generate Final Arfima Forecasts ----
              Forecast <- tryCatch({RemixAutoML::FinalBuildArfima(
                ModelOutputGrid = ARFIMA_ExperimentGrid,
                TimeSeriesPrepareOutput = ARFIMA_Artifacts_Score,
                FCPeriods = TS_FCPeriods,
                NumberModelsScore = 1,
                MetricSelection = TS_MetricEval,
                ByDataType = TRUE)},
                error = function(x) NULL)

              # Final build
              if(!is.null(Forecast)) {
                # Min, Average, Max values
                FC_MinValue <- min(Forecast[["Forecast"]], na.rm = TRUE)
                FC_AvgValue <- mean(Forecast[["Forecast"]], na.rm = TRUE)
                FC_MaxValue <- max(Forecast[["Forecast"]], na.rm = TRUE)

                # Ensure final models get build and correct grid metric is utilized----
                if(nrow(Forecast) != 0 & ((FC_MaxValue - MaxValue) * TS_FCPeriods / data[,.N]) < 10 * ((MaxValue - AvgValue))) {
                  data.table::fwrite(Forecast, file = file.path(ProjectList[["ModelsFolderPath"]],paste0(TargetName,"-ARFIMA.csv")))
                  data.table::fwrite(ARFIMA_ExperimentGrid, file = file.path(ProjectList[["ModelsFolderPath"]],paste0(TargetName,"-ARFIMA_ExperimentGrid.csv")))
                } else {
                  ProjectList[["TS_Models"]] <<- ProjectList[["TS_Models"]][!ProjectList[["TS_Models"]] %chin% "ARFIMA"]
                  save(ProjectList, file = file.path(ProjectList[["MetaDataPath"]], "ProjectList.Rdata"))
                }
              } else {
                ProjectList$TS_Models <<- ProjectList$TS_Models[!ProjectList$TS_Models %chin% "ARFIMA"]
              }
            } else {
              ProjectList$TS_Models <<- ProjectList$TS_Models[!ProjectList$TS_Models %chin% "ARFIMA"]
            }

            # 5. Update Progress Bar ----
            incProgress(amount = 1/n, message = "ARFIMA Complete")
          }

          # TSLM ----
          if("TSLM" %chin% TS_Models) {

            # 1. Create time series artifacts ----
            TSLM_Artifacts_Build <- RemixAutoML::TimeSeriesDataPrepare(
              data = data,
              TargetName = TargetName,
              DateName = DateName,
              Lags = 1,
              SeasonalLags = 1,
              MovingAverages = 1,
              SeasonalMovingAverages = 1,
              TimeUnit = TS_TimeUnit[1L],
              FCPeriods = TS_FCPeriods,
              HoldOutPeriods = TS_HoldOutPeriods,
              TSClean = TRUE,
              ModelFreq = TRUE)

            # 2. Find Best TSLM Models ----
            TSLM_ExperimentGrid <- tryCatch({RemixAutoML::ParallelAutoTSLM(
              MetricSelection = TS_MetricEval,
              Output = TSLM_Artifacts_Build,
              TrainValidateShare = c(0.5,0.5))},
              error = function(x) NULL)

            # 3. Create Final Build Data ----
            if(!is.null(TSLM_ExperimentGrid)) {
              TSLM_Artifacts_Score <- RemixAutoML::TimeSeriesDataPrepare(
                data = data,
                TargetName = TargetName,
                DateName = DateName,
                Lags = 1,
                SeasonalLags = 1,
                MovingAverages = 1,
                SeasonalMovingAverages = 1,
                TimeUnit = TS_TimeUnit[1L],
                FCPeriods = TS_FCPeriods,
                HoldOutPeriods = 0,
                TSClean = TRUE,
                ModelFreq = TRUE,
                FinalBuild = TRUE)

              # 4. Generate Final TSLM Forecasts ----
              Forecast <- tryCatch({RemixAutoML::FinalBuildTSLM(
                ModelOutputGrid = TSLM_ExperimentGrid,
                TimeSeriesPrepareOutput = TSLM_Artifacts_Score,
                FCPeriods = TS_FCPeriods,
                NumberModelsScore = 1,
                MetricSelection = TS_MetricEval,
                ByDataType = TRUE)},
                error = function(x) NULL)

              # Build
              if(!is.null(Forecast)) {

                # Min, Average, Max values
                FC_MinValue <- min(Forecast[["Forecast"]], na.rm = TRUE)
                FC_AvgValue <- mean(Forecast[["Forecast"]], na.rm = TRUE)
                FC_MaxValue <- max(Forecast[["Forecast"]], na.rm = TRUE)

                # Ensure final models get build and correct grid metric is utilized ----
                if(nrow(Forecast) != 0 & ((FC_MaxValue - MaxValue) * TS_FCPeriods / data[,.N]) < 10 * ((MaxValue - AvgValue))) {
                  data.table::fwrite(Forecast, file = file.path(ProjectList[["ModelsFolderPath"]],paste0(TargetName,"-TSLM.csv")))
                  data.table::fwrite(TSLM_ExperimentGrid, file = file.path(ProjectList[["ModelsFolderPath"]],paste0(TargetName,"-TSLM_ExperimentGrid.csv")))
                } else {
                  ProjectList[["TS_Models"]] <<- ProjectList[["TS_Models"]][!ProjectList[["TS_Models"]] %chin% "TSLM"]
                  save(ProjectList, file = file.path(ProjectList[["MetaDataPath"]], "ProjectList.Rdata"))
                }
              } else {
                ProjectList$TS_Models <<- ProjectList$TS_Models[!ProjectList$TS_Models %chin% "TSLM"]
              }
            } else {
              ProjectList$TS_Models <<- ProjectList$TS_Models[!ProjectList$TS_Models %chin% "TSLM"]
            }

            # 5. Update Progress Bar ----
            incProgress(amount = 1/n, message = "TSLM Complete")
          }

          # H2oCARMA ----
          if("H2O-CARMA" %chin% ML_Models) {

            # Grid options ----
            if(!as.logical(input$TS_SimpleGrid)) {
              runs <- 2L
            } else {
              runs <- 5L
            }

            # 1. AutoH2OCARMA Forecast and Save to File ----
            for(runversion in seq_len(runs)) {

              # Test which build is best
              if(runversion != max(runs)) {
                TrainOnFull <- FALSE
                if(!is.null(GroupVariableNames)) {
                  N <- data[,.N, by = eval(GroupVariableNames)][1, max(N)]
                } else {
                  N <- data[,.N]
                }
                if(as.logical(TS_CARMA_GridTune)) {
                  temp_gridtune <- TRUE
                  TS_CARMA_GridTune <- FALSE
                } else {
                  temp_gridtune <- FALSE
                }
                Ratios <- c(1 - TS_HoldOutPeriods / N, TS_HoldOutPeriods / N)
              } else {
                TrainOnFull <- TRUE
                Sys.sleep(5L)
                Ratios <- NULL
                if(temp_gridtune) TS_CARMA_GridTune <- TRUE
              }

              # Final Build
              if(runs == 5L) {
                if(runversion == 1L) {
                  TS_PerformanceCollection <- data.table::data.table(ModelsArgs = c("TT","TF","FT","FF"), Metric = rep(-1L, 4L))
                  TS_CARMA_Transformation <- "TRUE"
                  TS_CARMA_Difference <- TRUE
                } else if(runversion == 2L) {
                  TS_CARMA_Transformation <- "TRUE"
                  TS_CARMA_Difference <- FALSE
                } else if(runversion == 3L) {
                  TS_CARMA_Transformation <- "FALSE"
                  TS_CARMA_Difference <- TRUE
                } else if(runversion == 4L) {
                  TS_CARMA_Transformation <- "FALSE"
                  TS_CARMA_Difference <- FALSE
                } else if(runversion == 5L) {
                  args <- TS_PerformanceCollection[order(Metric)][1,"ModelsArgs"][[1]]
                  if(args == "TT") {

                    # Load winning model eval metrics ----
                    load(file = file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-H2OCARMAEvalOutput-TT.Rdata")), envir = .GlobalEnv)

                    # Save file without qualifier ----
                    save(H2OCARMAEvalOutput, file = file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-H2OCARMAEvalOutput.Rdata")))

                    # Delete non winning model evaluation files ----
                    file.remove(file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-H2OCARMAEvalOutput-TT.Rdata")))
                    file.remove(file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-H2OCARMAEvalOutput-TF.Rdata")))
                    file.remove(file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-H2OCARMAEvalOutput-FT.Rdata")))
                    file.remove(file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-H2OCARMAEvalOutput-FF.Rdata")))

                    # Finalize parameter settings and save to project list ----
                    TS_CARMA_Transformation <- "TRUE"
                    TS_CARMA_Difference <- TRUE
                    ProjectList[["TS_CARMA_Transformation"]] <<- "TRUE"
                    ProjectList[["TS_CARMA_Difference"]] <<- TRUE
                  } else if(args == "TF") {

                    # Load winning model eval metrics ----
                    load(file = file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-H2OCARMAEvalOutput-TF.Rdata")), envir = .GlobalEnv)

                    # Save file without qualifier----
                    save(H2ODRFCARMAEvalOutput, file = file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-H2OCARMAEvalOutput.Rdata")))

                    # Delete non winning model evaluation files ----
                    file.remove(file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-H2OCARMAEvalOutput-TT.Rdata")))
                    file.remove(file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-H2OCARMAEvalOutput-TF.Rdata")))
                    file.remove(file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-H2OCARMAEvalOutput-FT.Rdata")))
                    file.remove(file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-H2OCARMAEvalOutput-FF.Rdata")))

                    # Finalize parameter settings and save to project list ----
                    TS_CARMA_Transformation <- "TRUE"
                    TS_CARMA_Difference <- FALSE
                    ProjectList[["TS_CARMA_Transformation"]] <<- "TRUE"
                    ProjectList[["TS_CARMA_Difference"]] <<- FALSE
                  } else if(args == "FT") {

                    # Load winning model eval metrics ----
                    load(file = file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-H2OCARMAEvalOutput-FT.Rdata")))

                    # Save file without qualifier ----
                    save(H2ODRFCARMAEvalOutput, file = file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-H2ODRFCARMAEvalOutput.Rdata")))

                    # Delete non winning model evaluation files ----
                    file.remove(file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-H2OCARMAEvalOutput-TT.Rdata")))
                    file.remove(file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-H2OCARMAEvalOutput-TF.Rdata")))
                    file.remove(file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-H2OCARMAEvalOutput-FT.Rdata")))
                    file.remove(file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-H2OCARMAEvalOutput-FF.Rdata")))

                    # Finalize parameter settings and save to project list ----
                    TS_CARMA_Transformation <- "FALSE"
                    TS_CARMA_Difference <- TRUE
                    ProjectList[["TS_CARMA_Transformation"]] <<- "FALSE"
                    ProjectList[["TS_CARMA_Difference"]] <<- TRUE
                  } else {

                    # Load winning model eval metrics ----
                    load(file = file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-H2OCARMAEvalOutput-FF.Rdata")), envir = .GlobalEnv)

                    # Save file without qualifier ----
                    save(H2ODRFCARMAEvalOutput, file = file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-H2OCARMAEvalOutput.Rdata")))

                    # Delete non winning model evaluation files ----
                    file.remove(file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-H2OCARMAEvalOutput-TT.Rdata")))
                    file.remove(file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-H2OCARMAEvalOutput-TF.Rdata")))
                    file.remove(file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-H2OCARMAEvalOutput-FT.Rdata")))
                    file.remove(file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-H2OCARMAEvalOutput-FF.Rdata")))

                    # Finalize parameter settings and save to project list ----
                    TS_CARMA_Transformation <- "FALSE"
                    TS_CARMA_Difference <- FALSE
                    ProjectList[["TS_CARMA_Transformation"]] <<- "FALSE"
                    ProjectList[["TS_CARMA_Difference"]] <<- FALSE
                  }
                }
              }

              # Build Model ----
              datax <- data.table::copy(data)
              Forecast1 <- RemixAutoML::AutoH2OCARMA(

                # New Args
                AlgoType = "DRF",
                ExcludeAlgos = "XGBoost",
                NonNegativePred = FALSE,
                RoundPreds = TRUE,
                Methods = c("YeoJohnson","BoxCox","Log","LogPlus1","Sqrt","Asinh","Asin","Logit"),
                AnomalyDetection = NULL,

                data = datax,
                TrainOnFull = TrainOnFull,
                HierarchGroups = GroupVariableNames,
                TimeGroups = TS_TimeUnit,
                Quantiles_Selected = TS_CARMA_Quantiles_Selected,
                DebugMode = TRUE,
                TargetColumnName = TargetName,
                DateColumnName = DateName,
                GroupVariables = c(GroupVariableNames),
                FC_Periods = TS_FCPeriods,
                TimeUnit = TS_TimeUnit[1L],
                TargetTransformation = as.logical(TS_CARMA_Transformation),
                Lags = as.numeric(TS_CARMA_Lags),
                MA_Periods = as.numeric(TS_CARMA_MovingAverages),
                SD_Periods = as.numeric(TS_CARMA_MovingSD),
                Skew_Periods = as.numeric(TS_CARMA_MovingSkew),
                Kurt_Periods = as.numeric(TS_CARMA_MovingKurt),
                Quantile_Periods = as.numeric(TS_CARMA_MovingQuantiles),
                XREGS = NULL,
                CalendarVariables = as.logical(TS_CARMA_CalendarVariables),
                Difference = TS_CARMA_Difference,
                FourierTerms = as.numeric(TS_CARMA_Fourier),
                HolidayVariable = as.logical(TS_CARMA_HolidayVariables),
                HolidayLags = TS_CARMA_HolidayLags,
                HolidayMovingAverages = TS_CARMA_HolidayMovingAverages,
                TimeTrendVariable = as.logical(TS_CARMA_TimeTrend),
                ZeroPadSeries = NULL,
                DataTruncate = as.logical(TS_CARMA_DataTruncate),
                SplitRatios = Ratios,
                EvalMetric = TS_MetricEval,
                GridTune = as.logical(TS_CARMA_GridTune),
                ModelCount = TS_CARMA_ModelCount,
                NTrees = TS_CARMA_NTrees,
                PartitionType = "timeseries",
                MaxMem = TS_CARMA_MaxMemory,
                NThreads = TS_CARMA_NThreads,
                Timer = TRUE)

              # Save data and model output ----
              if(runs == 2L) {
                if(runversion == 2L) {
                  test1 <- Forecast1$Forecast
                  tryCatch({data.table::fwrite(test1, file = file.path(ProjectList[["ModelsFolderPath"]],paste0(TargetName,"-H2OCARMA.csv")))}, error = function(x) print("Error saving"))
                } else if(runversion == 1L) {
                  H2OCARMAEvalOutput <- Forecast1$ModelInformation
                  save(H2OCARMAEvalOutput, file = file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-H2OCARMAEvalOutput.Rdata")))
                }
              } else {
                if(runversion == 5L) {
                  test1 <- Forecast1$Forecast
                  tryCatch({data.table::fwrite(test1, file = file.path(ProjectList[["ModelsFolderPath"]],paste0(TargetName,"-H2OCARMA.csv")))}, error = function(x) print("Error saving"))
                } else {
                  H2OCARMAEvalOutput <- Forecast1$ModelInformation
                  H2O <- Forecast1$ModelInformation$EvaluationMetrics
                  data.table::set(TS_PerformanceCollection, i = eval(runversion), j = "Metric", value = H2O[Metric == eval(TS_MetricEval), "MetricValue"][[1L]])
                  if(runversion == 1L) {
                    save(H2OCARMAEvalOutput, file = file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-H2OCARMAEvalOutput-TT.Rdata")))
                  } else if(runversion == 2L) {
                    save(H2OCARMAEvalOutput, file = file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-H2OCARMAEvalOutput-TF.Rdata")))
                  } else if(runversion == 3L) {
                    save(H2OCARMAEvalOutput, file = file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-H2OCARMAEvalOutput-FT.Rdata")))
                  } else if(runversion == 4L) {
                    save(H2OCARMAEvalOutput, file = file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-H2OCARMAEvalOutput-FF.Rdata")))
                  }
                }
              }
            }

            # 2. Update Progress Bar ----
            incProgress(amount = 1/n, message = "H2O-CARMA Complete")
          }

          # CatBoostCARMA ----
          if("CatBoost-CARMA" %chin% ML_Models) {

            # Switch ----
            if(exists("Switch_CatBoost")) {
              Switch_CatBoost <<- TRUE
            } else {
              Switch_CatBoost <<- FALSE
            }

            # Modeling Parameters ----

            # Productionize args
            TS_CatBoost_CARMA_TaskType <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_TaskType", Type = "character", Default = "CPU", Switch = Switch_CatBoost)
            TS_CatBoost_CARMA_NumGPU <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_NumGPU", Type = "numeric", Default = 1, Switch = Switch_CatBoost)

            # Target transformations
            TS_CatBoost_CARMA_Methods <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_Methods", Type = "character", Default = c("Identity"), Switch = Switch_CatBoost)
            TS_CatBoost_CARMA_Difference <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_Difference", Type = "character", Default = "FALSE", Switch = Switch_CatBoost)
            TS_CatBoost_CARMA_NonNegativePrep <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_NonNegativePrep", Type = "character", Default = "FALSE", Switch = Switch_CatBoost)
            TS_CatBoost_CARMA_RoundPreds <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_RoundPreds", Type = "character", Default = "TRUE", Switch = Switch_CatBoost)

            # Calendar-related features
            TS_CatBoost_CARMA_CalendarVariables <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_CalendarVariables", Type = "character", Default = c("hour","wday","mday","yday","week","wom","month","quarter"), Switch = Switch_CatBoost)
            TS_CatBoost_CARMA_HolidayVariables <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_HolidayVariables", Type = "character", Default = "USPublicHolidays", Switch = Switch_CatBoost)
            TS_CatBoost_CARMA_HolidayLags <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_HolidayLags", Type = "character", Default = c("1","2"), Switch = Switch_CatBoost)
            TS_CatBoost_CARMA_HolidayMovingAverages <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_HolidayMovingAverages", Type = "character", Default = c("2","3"), Switch = Switch_CatBoost)

            # Lags, moving averages, and other rolling stats
            TS_CatBoost_CARMA_Lags <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_Lags", Type = "numeric", Default = c(1L:10L), Switch = Switch_CatBoost)
            TS_CatBoost_CARMA_MovingAverages <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_MovingAverages", Type = "numeric", Default = c(2L:10L), Switch = Switch_CatBoost)
            TS_CatBoost_CARMA_MovingSD <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_MovingSD", Type = "numeric", Default = NULL, Switch = Switch_CatBoost)
            TS_CatBoost_CARMA_MovingSkew <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_MovingSkew", Type = "numeric", Default = NULL, Switch = Switch_CatBoost)
            TS_CatBoost_CARMA_MovingKurt <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_MovingKurt", Type = "numeric", Default = NULL, Switch = Switch_CatBoost)
            TS_CatBoost_CARMA_MovingQuantiles <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_MovingQuantiles", Type = "numeric", Default = NULL, Switch = Switch_CatBoost)
            TS_CatBoost_CARMA_Quantiles_Selected <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_Quantiles_Selected", Type = "character", Default = "NULL", Switch = Switch_CatBoost)
            TS_CatBoost_CARMA_Lags1 <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_Lags1", Type = "numeric", Default = c(1L:5L), Switch = Switch_CatBoost)
            TS_CatBoost_CARMA_MovingAverages1 <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_MovingAverages1", Type = "numeric", Default = c(2L:10L), Switch = Switch_CatBoost)
            TS_CatBoost_CARMA_MovingSD1 <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_MovingSD1", Type = "numeric", Default = NULL, Switch = Switch_CatBoost)
            TS_CatBoost_CARMA_MovingSkew1 <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_MovingSkew1", Type = "numeric", Default = NULL, Switch = Switch_CatBoost)
            TS_CatBoost_CARMA_MovingKurt1 <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_MovingKurt1", Type = "numeric", Default = NULL, Switch = Switch_CatBoost)
            TS_CatBoost_CARMA_MovingQuantiles1 <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_MovingQuantiles1", Type = "numeric", Default = NULL, Switch = Switch_CatBoost)
            TS_CatBoost_CARMA_HierarchGroups <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_HierarchGroups", Type = "character", Default = "FALSE", Switch = Switch_CatBoost)

            # Bonus features
            TS_CatBoost_CARMA_AnomalyDetection_HighThreshold <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_AnomalyDetection_HighThreshold", Type = "numeric", Default = 0, Switch = Switch_CatBoost)
            TS_CatBoost_CARMA_AnomalyDetection_LowThreshold <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_AnomalyDetection_LowThreshold", Type = "numeric", Default = 0, Switch = Switch_CatBoost)
            TS_CatBoost_CARMA_Fourier <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_Fourier", Type = "numeric", Default = 2L, Switch = Switch_CatBoost)
            TS_CatBoost_CARMA_TimeTrend <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_TimeTrend", Type = "character", Default = "TRUE", Switch = Switch_CatBoost)
            TS_CatBoost_CARMA_DataTruncate <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_DataTruncate", Type = "character", Default = "FALSE", Switch = Switch_CatBoost)
            TS_CatBoost_CARMA_TimeWeights <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_TimeWeights", Type = "numeric", Default = 0.9999, Switch = Switch_CatBoost)

            # ML grid tuning args
            TS_CatBoost_CARMA_GridTune <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_GridTune", Type = "character", Default = "FALSE", Switch = Switch_CatBoost)
            TS_CatBoost_CARMA_ModelCount <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_ModelCount", Type = "numeric", Default = 5, Switch = Switch_CatBoost)
            TS_CatBoost_CARMA_PassInGrid <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_PassInGrid", Type = "character", Default = "NULL", Switch = Switch_CatBoost)
            TS_CatBoost_CARMA_MaxRunsWithoutNewWinner <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_MaxRunsWithoutNewWinner", Type = "numeric", Default = 25, Switch = Switch_CatBoost)
            TS_CatBoost_CARMA_MaxRunMinutes <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_MaxRunMinutes", Type = "numeric", Default = 60*24, Switch = Switch_CatBoost)

            # ML evaluation output
            TS_CatBoost_CARMA_NumParDepPlots <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_NumParDepPlots", Type = "numeric", Default = 25, Switch = Switch_CatBoost)
            TS_CatBoost_CARMA_PDFOutputPath <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_PDFOutputPath", Type = "character", Default = "NULL", Switch = Switch_CatBoost)
            TS_CatBoost_CARMA_SaveDataPath <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_SaveDataPath", Type = "character", Default = "NULL", Switch = Switch_CatBoost)

            # ML loss functions
            TS_CatBoost_CARMA_EvalMetric <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_EvalMetric", Type = "character", Default = "RMSE", Switch = Switch_CatBoost)
            TS_CatBoost_CARMA_EvalMetricValue <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_EvalMetricValue", Type = "numeric", Default = 1, Switch = Switch_CatBoost)
            TS_CatBoost_CARMA_LossFunction <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_LossFunction", Type = "character", Default = "RMSE", Switch = Switch_CatBoost)
            TS_CatBoost_CARMA_LossFunctionValue <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_LossFunctionValue", Type = "numeric", Default = 1, Switch = Switch_CatBoost)

            # ML tuning args
            TS_CatBoost_CARMA_NTrees <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_NTrees", Type = "character", Default = 1000, Switch = Switch_CatBoost)
            TS_CatBoost_CARMA_Depth <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_Depth", Type = "numeric", Default = 9, Switch = Switch_CatBoost)
            TS_CatBoost_CARMA_Langevin <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_Langevin", "character", Default = "FALSE", Switch = Switch_CatBoost)
            TS_CatBoost_CARMA_DiffusionTemperature <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_DiffusionTemperature", Type = "numeric", Default = 10000, Switch = Switch_CatBoost)

            # ML overfitting args
            TS_CatBoost_CARMA_SubSample <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_SubSample", "numeric", Default = 0.66, Switch = Switch_CatBoost)
            TS_CatBoost_CARMA_LearningRate <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_LearningRate", Type = "numeric", Default = NULL, Switch = Switch_CatBoost)
            TS_CatBoost_CARMA_L2_Leaf_Reg <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_L2_Leaf_Reg", Type = "numeric", Default = 3.0, Switch = Switch_CatBoost)
            TS_CatBoost_CARMA_ModelSizeReg <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_ModelSizeReg", Type = "numeric", Default = 0.5, Switch = Switch_CatBoost)
            TS_CatBoost_CARMA_RSM <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_RSM", Type = "numeric", Default = 1, Switch = Switch_CatBoost)
            TS_CatBoost_CARMA_MinDataInLeaf <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_MinDataInLeaf", Type = "numeric", Default = 1, Switch = Switch_CatBoost)
            TS_CatBoost_CARMA_BorderCount <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_BorderCount", Type = "numeric", Default = 254, Switch = Switch_CatBoost)
            TS_CatBoost_CARMA_RandomStrength <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_RandomStrength", Type = "numeric", Default = 1, Switch = Switch_CatBoost)
            TS_CatBoost_CARMA_SamplingUnit <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_SamplingUnit", Type = "character", Default = "Object", Switch = Switch_CatBoost)

            # ML styles
            TS_CatBoost_CARMA_BootStrapType <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_BootStrapType", Type = "character", Default = "Bayesian", Switch = Switch_CatBoost)
            TS_CatBoost_CARMA_GrowPolicy <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_GrowPolicy", Type = "character", Default = "SymmetricTree", Switch = Switch_CatBoost)
            TS_CatBoost_CARMA_FeatureBorderType <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_FeatureBorderType", Type = "character", Default = "GreedyLogSum", Switch = Switch_CatBoost)
            TS_CatBoost_CARMA_ScoreFunction <- RemixAutoML::ReturnParam(input, VarName = "TS_CatBoost_CARMA_ScoreFunction", Type = "character", Default = "Cosine", Switch = Switch_CatBoost)

            # Loop through model builds ----
            for(Runs in c("Baseline", "Forecast")) {

              # Define args for baseline and forecast runs ----
              if(Runs == "Baseline") {
                TOF <- FALSE
                datax <- data.table::copy(data)
                if(ncol(datax) > 2) N <- data[, .N, by = c(eval(GroupVariableNames))][, max(N)] else N <- datax[, .N]
                if(exists("xregs")) xregsx <- data.table::copy(xregs)
                NTrees <- TS_CatBoost_CARMA_NTrees
                LearningRate <- TS_CatBoost_CARMA_LearningRate
                Ratios <- c(1 - TS_HoldOutPeriods / N, TS_HoldOutPeriods / N)
                if(length(TS_TimeUnit) > 1L) {
                  Defined_Lags <- list()
                  Defined_MA <- list()
                  Defined_SD <- list()
                  Defined_Skew <- list()
                  Defined_Kurt <- list()
                  Defined_Percentile <- list()
                  for(z in 1:2) {
                    if(z == 1) {
                      if(!is.null(TS_CatBoost_CARMA_Lags)) Defined_Lags[[TS_TimeUnit[z]]] <- as.numeric(TS_CatBoost_CARMA_Lags) else Defined_Lags[[TS_TimeUnit[z]]] <- NULL
                      if(!is.null(TS_CatBoost_CARMA_MovingAverages)) Defined_MA[[TS_TimeUnit[z]]] <- as.numeric(TS_CatBoost_CARMA_MovingAverages) else Defined_MA[[TS_TimeUnit[z]]] <- NULL
                      if(!is.null(TS_CatBoost_CARMA_MovingSD)) Defined_SD[[TS_TimeUnit[z]]] <- as.numeric(TS_CatBoost_CARMA_MovingSD) else Defined_SD[[TS_TimeUnit[z]]] <- NULL
                      if(!is.null(TS_CatBoost_CARMA_MovingSkew)) Defined_Skew[[TS_TimeUnit[z]]] <- as.numeric(TS_CatBoost_CARMA_MovingSkew) else Defined_Skew[[TS_TimeUnit[z]]] <- NULL
                      if(!is.null(TS_CatBoost_CARMA_MovingKurt)) Defined_Kurt[[TS_TimeUnit[z]]] <- as.numeric(TS_CatBoost_CARMA_MovingKurt) else Defined_Kurt[[TS_TimeUnit[z]]] <- NULL
                      if(!is.null(TS_CatBoost_CARMA_MovingQuantiles)) Defined_Percentile[[TS_TimeUnit[z]]] <- as.numeric(TS_CatBoost_CARMA_MovingQuantiles) else Defined_Percentile[[TS_TimeUnit[z]]] <- NULL
                    } else if(z == 2) {
                      if(!is.null(TS_CatBoost_CARMA_Lags)) Defined_Lags[[TS_TimeUnit[z]]] <- as.numeric(TS_CatBoost_CARMA_Lags1) else Defined_Lags[[TS_TimeUnit[z]]] <- NULL
                      if(!is.null(TS_CatBoost_CARMA_MovingAverages)) Defined_MA[[TS_TimeUnit[z]]] <- as.numeric(TS_CatBoost_CARMA_MovingAverages1) else Defined_MA[[TS_TimeUnit[z]]] <- NULL
                      if(!is.null(TS_CatBoost_CARMA_MovingSD)) Defined_SD[[TS_TimeUnit[z]]] <- as.numeric(TS_CatBoost_CARMA_MovingSD1) else Defined_SD[[TS_TimeUnit[z]]] <- NULL
                      if(!is.null(TS_CatBoost_CARMA_MovingSkew)) Defined_Skew[[TS_TimeUnit[z]]] <- as.numeric(TS_CatBoost_CARMA_MovingSkew1) else Defined_Skew[[TS_TimeUnit[z]]] <- NULL
                      if(!is.null(TS_CatBoost_CARMA_MovingKurt)) Defined_Kurt[[TS_TimeUnit[z]]] <- as.numeric(TS_CatBoost_CARMA_MovingKurt1) else Defined_Kurt[[TS_TimeUnit[z]]] <- NULL
                      if(!is.null(TS_CatBoost_CARMA_MovingQuantiles)) Defined_Percentile[[TS_TimeUnit[z]]] <- as.numeric(TS_CatBoost_CARMA_MovingQuantiles1) else Defined_Percentile[[TS_TimeUnit[z]]] <- NULL
                    }
                  }
                } else {
                  if(!is.null(TS_CatBoost_CARMA_Lags)) Defined_Lags <- as.numeric(TS_CatBoost_CARMA_Lags) else Defined_Lags <- NULL
                  if(!is.null(TS_CatBoost_CARMA_MovingAverages)) Defined_MA <- as.numeric(TS_CatBoost_CARMA_MovingAverages) else Defined_MA <- NULL
                  if(!is.null(TS_CatBoost_CARMA_MovingSD)) Defined_SD <- as.numeric(TS_CatBoost_CARMA_MovingSD) else Defined_SD <- NULL
                  if(!is.null(TS_CatBoost_CARMA_MovingSkew)) Defined_Skew <- as.numeric(TS_CatBoost_CARMA_MovingSkew) else Defined_Skew <- NULL
                  if(!is.null(TS_CatBoost_CARMA_MovingKurt)) Defined_Kurt <- as.numeric(TS_CatBoost_CARMA_MovingKurt) else Defined_Kurt <- NULL
                  if(!is.null(TS_CatBoost_CARMA_MovingQuantiles)) Defined_Percentile <- as.numeric(TS_CatBoost_CARMA_MovingQuantiles) else Defined_Percentile <- NULL
                }
              } else if(Runs == "Forecast") {
                TOF <- TRUE
                datax <- data.table::copy(data)
                if(exists("xregs")) xregsx <- data.table::copy(xregs)
                NTrees <- CatBoostResults$Model$tree_count
                if(is.null(TS_CatBoost_CARMA_LearningRate)) LearningRate <- CatBoostResults$Model$learning_rate
                Ratios <- NULL
              }

              # Debugging
              # print("datax")
              # print(head(datax))
              # print(str(datax))
              # print("xregsx")
              # print(xregsx)
              # print(str(xregsx))
              # print("TargetName")
              # print(TargetName)
              # print("DateName")
              # print(DateName)
              # print("GroupVariableNames")
              # print(GroupVariableNames)
              # print("TS_TimeUnit")
              # print(TS_TimeUnit)
              # print("Defined_Lags")
              # print(Defined_Lags)
              # print("Defined_SD")
              # print(Defined_SD)
              # print("Anomaly Detection")
              # print(if(TS_CatBoost_CARMA_AnomalyDetection_HighThreshold != 0 && TS_CatBoost_CARMA_AnomalyDetection_LowThreshold != 0) list("tstat_high" = TS_CatBoost_CARMA_AnomalyDetection_HighThreshold, "tstat_low" = TS_CatBoost_CARMA_AnomalyDetection_LowThreshold) else if(TS_CatBoost_CARMA_AnomalyDetection_HighThreshold != 0 && TS_CatBoost_CARMA_AnomalyDetection_LowThreshold == 0) list("tstat_high" = TS_CatBoost_CARMA_AnomalyDetection_HighThreshold, "tstat_low" = -99999) else if(TS_CatBoost_CARMA_AnomalyDetection_HighThreshold == 0 && TS_CatBoost_CARMA_AnomalyDetection_LowThreshold != 0) list("tstat_high" = 9999, "tstat_low" = TS_CatBoost_CARMA_AnomalyDetection_LowThreshold) else  NULL)
              # print("TS_CatBoost_CARMA_Langevin")
              # print(as.logical(TS_CatBoost_CARMA_Langevin))
              # print("Ratios")
              # print(Ratios)
              # print("Hierach Argument")
              # print(!is.null(input$TS_CatBoost_CARMA_HierarchGroups))
              # print(!is.null(input$TS_CatBoost_CARMA_HierarchGroups) || input$TS_CatBoost_CARMA_HierarchGroups == "FALSE")
              # print(input$TS_CatBoost_CARMA_HierarchGroups)

              # Build model ----
              CatBoostResults <- RemixAutoML::AutoCatBoostCARMA(

                # data args
                data = datax,
                TimeWeights = TS_CatBoost_CARMA_TimeWeights,
                TargetColumnName = TargetName,
                DateColumnName = DateName,
                HierarchGroups = if(!is.null(input$TS_CatBoost_CARMA_HierarchGroups) && input$TS_CatBoost_CARMA_HierarchGroups == "FALSE") NULL else GroupVariableNames,
                GroupVariables = GroupVariableNames,
                TimeUnit = TS_TimeUnit[1L],
                TimeGroups = TS_TimeUnit,

                # Production args
                TrainOnFull = TOF,
                SplitRatios = Ratios,
                PartitionType = "random",
                FC_Periods = TS_FCPeriods,
                TaskType = TS_CatBoost_CARMA_TaskType,
                NumGPU = as.numeric(TS_CatBoost_CARMA_NumGPU),
                Timer = TRUE,
                DebugMode = TRUE,

                # Target variable transformations
                TargetTransformation = if(!all(TS_CatBoost_CARMA_Methods %chin% "Identity")) TRUE else FALSE,
                Methods = TS_CatBoost_CARMA_Methods,
                Difference = as.logical(TS_CatBoost_CARMA_Difference),
                NonNegativePred = as.logical(TS_CatBoost_CARMA_NonNegativePrep),
                RoundPreds = as.logical(TS_CatBoost_CARMA_RoundPreds),

                # Calendar-related features
                CalendarVariables = TS_CatBoost_CARMA_CalendarVariables,
                HolidayVariable = TS_CatBoost_CARMA_HolidayVariables,
                HolidayLags = as.numeric(TS_CatBoost_CARMA_HolidayLags),
                HolidayMovingAverages = as.numeric(TS_CatBoost_CARMA_HolidayMovingAverages),

                # Lags, moving averages, and other rolling stats
                Lags = Defined_Lags,
                MA_Periods = Defined_MA,
                SD_Periods = Defined_SD,
                Skew_Periods = Defined_Skew,
                Kurt_Periods = Defined_Kurt,
                Quantile_Periods = Defined_Percentile,
                Quantiles_Selected = TS_CatBoost_CARMA_Quantiles_Selected,

                # Bonus features
                AnomalyDetection = if(TS_CatBoost_CARMA_AnomalyDetection_HighThreshold != 0 && TS_CatBoost_CARMA_AnomalyDetection_LowThreshold != 0) list("tstat_high" = TS_CatBoost_CARMA_AnomalyDetection_HighThreshold, "tstat_low" = TS_CatBoost_CARMA_AnomalyDetection_LowThreshold) else if(TS_CatBoost_CARMA_AnomalyDetection_HighThreshold != 0 && TS_CatBoost_CARMA_AnomalyDetection_LowThreshold == 0) list("tstat_high" = TS_CatBoost_CARMA_AnomalyDetection_HighThreshold, "tstat_low" = -99999) else if(TS_CatBoost_CARMA_AnomalyDetection_HighThreshold == 0 && TS_CatBoost_CARMA_AnomalyDetection_LowThreshold != 0) list("tstat_high" = 9999, "tstat_low" = TS_CatBoost_CARMA_AnomalyDetection_LowThreshold) else  NULL,
                XREGS = xregsx,
                FourierTerms = as.numeric(TS_CatBoost_CARMA_Fourier),
                TimeTrendVariable = as.logical(TS_CatBoost_CARMA_TimeTrend),
                ZeroPadSeries = NULL,
                DataTruncate = as.logical(TS_CatBoost_CARMA_DataTruncate),

                # ML grid tuning args
                GridTune = as.logical(TS_CatBoost_CARMA_GridTune),
                PassInGrid = if(TS_CatBoost_CARMA_PassInGrid == "NULL") NULL else TS_CatBoost_CARMA_PassInGrid,
                ModelCount = as.numeric(TS_CatBoost_CARMA_ModelCount),
                MaxRunsWithoutNewWinner = as.numeric(TS_CatBoost_CARMA_MaxRunsWithoutNewWinner),
                MaxRunMinutes = as.numeric(TS_CatBoost_CARMA_MaxRunMinutes),

                # ML evaluation output
                PDFOutputPath = if(TS_CatBoost_CARMA_PDFOutputPath == "NULL") NULL else file.path(ProjectList[["DataFolderPath"]], "ModelInsights"),
                SaveDataPath = if(TS_CatBoost_CARMA_SaveDataPath == "NULL") NULL else file.path(ProjectList[["DataFolderPath"]]),
                NumOfParDepPlots = TS_CatBoost_CARMA_NumParDepPlots,

                # ML loss functions
                EvalMetric = TS_CatBoost_CARMA_EvalMetric,
                EvalMetricValue = as.numeric(TS_CatBoost_CARMA_EvalMetricValue),
                LossFunction = TS_CatBoost_CARMA_LossFunction,
                LossFunctionValue = as.numeric(TS_CatBoost_CARMA_LossFunctionValue),

                # ML tuning args
                NTrees = NTrees,
                Depth = as.numeric(TS_CatBoost_CARMA_Depth),
                L2_Leaf_Reg = if(LearningRate == 0) NULL else as.numeric(TS_CatBoost_CARMA_L2_Leaf_Reg),
                LearningRate = if(LearningRate == 0) NULL else as.numeric(LearningRate),
                Langevin = as.logical(TS_CatBoost_CARMA_Langevin),
                DiffusionTemperature = as.numeric(TS_CatBoost_CARMA_DiffusionTemperature),
                RandomStrength = as.numeric(TS_CatBoost_CARMA_RandomStrength),
                BorderCount = as.numeric(TS_CatBoost_CARMA_BorderCount),
                RSM = as.numeric(TS_CatBoost_CARMA_RSM),
                GrowPolicy = as.character(TS_CatBoost_CARMA_GrowPolicy),
                BootStrapType = as.character(TS_CatBoost_CARMA_BootStrapType),
                ModelSizeReg = as.numeric(TS_CatBoost_CARMA_ModelSizeReg),
                FeatureBorderType = TS_CatBoost_CARMA_FeatureBorderType,
                SamplingUnit = TS_CatBoost_CARMA_SamplingUnit,
                SubSample = as.numeric(TS_CatBoost_CARMA_SubSample),
                ScoreFunction = TS_CatBoost_CARMA_ScoreFunction,
                MinDataInLeaf = as.numeric(TS_CatBoost_CARMA_MinDataInLeaf))

              # Save data and model output ----
              if(Runs == "Baseline") {
                save(CatBoostResults, file = file.path(ProjectList[["MetaDataPath"]], paste0(TargetName, "_AutoCatBoostCARMA_ModelInsights.Rdata")))
              } else {
                Forecast <- CatBoostResults$Forecast
                tryCatch({data.table::fwrite(Forecast, file = file.path(ProjectList[["ModelsFolderPath"]], paste0(TargetName, "_AutoCatBoostCARMA_Forecast.csv")))}, error = function(x) print("Error saving"))
              }
            }

            # 2. Update Progress Bar----
            incProgress(amount = 1/n, message = "AutoCatBoostCARMA Complete")
          }

          # XGBoostCARMA ----
          if("XGBoost-CARMA" %chin% ML_Models) {

            # Grid options----
            if(!as.logical(input$TS_SimpleGrid)) {
              runs <- 2L
            } else {
              runs <- 5L
            }

            # 1. AutoXGBoostCARMA Forecast and Save to File----
            for(runversion in seq_len(runs)) {
              if(runversion != max(runs)) {
                TrainOnFull <- FALSE
                if(!is.null(GroupVariableNames)) {
                  N <- data[,.N, by = eval(GroupVariableNames)][1,N][[1]]
                } else {
                  N <- data[,.N]
                }
                if(as.logical(TS_CARMA_GridTune)) {
                  temp_gridtune <- TRUE
                  TS_CARMA_GridTune <- FALSE
                } else {
                  temp_gridtune <- FALSE
                }
                Ratios <- c(1 - TS_HoldOutPeriods / N, TS_HoldOutPeriods / N)
              } else {
                TrainOnFull <- TRUE
                Sys.sleep(5L)
                Ratios <- NULL
                if(temp_gridtune) {
                  TS_CARMA_GridTune <- TRUE
                }
              }
              if(runs == 5L) {
                if(runversion == 1L) {
                  TS_PerformanceCollection <- data.table::data.table(ModelsArgs = c("TT","TF","FT","FF"), Metric = rep(-1L, 4L))
                  TS_CARMA_Transformation <- "TRUE"
                  TS_CARMA_Difference <- TRUE
                } else if(runversion == 2L) {
                  TS_CARMA_Transformation <- "TRUE"
                  TS_CARMA_Difference <- FALSE
                } else if(runversion == 3L) {
                  TS_CARMA_Transformation <- "FALSE"
                  TS_CARMA_Difference <- TRUE
                } else if(runversion == 4L) {
                  TS_CARMA_Transformation <- "FALSE"
                  TS_CARMA_Difference <- FALSE
                } else if(runversion == 5L) {
                  args <- TS_PerformanceCollection[order(Metric)][1,"ModelsArgs"][[1]]
                  if(args == "TT") {

                    # Load winning model eval metrics----
                    load(file = file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-XGBoostCARMAEvalOutput-TT.Rdata")))

                    # Save file without qualifier----
                    save(XGBoostCARMAEvalOutput, file = file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-XGBoostCARMAEvalOutput.Rdata")))

                    # Delete non winning model evaluation files----
                    file.remove(file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-XGBoostCARMAEvalOutput-TT.Rdata")))
                    file.remove(file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-XGBoostCARMAEvalOutput-TF.Rdata")))
                    file.remove(file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-XGBoostCARMAEvalOutput-FT.Rdata")))
                    file.remove(file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-XGBoostCARMAEvalOutput-FF.Rdata")))

                    # Finalize parameter settings and save to project list----
                    TS_CARMA_Transformation <- "TRUE"
                    TS_CARMA_Difference <- TRUE
                    ProjectList[["TS_CARMA_Transformation"]] <<- "TRUE"
                    ProjectList[["TS_CARMA_Difference"]] <<- TRUE
                  } else if(args == "TF") {

                    # Load winning model eval metrics----
                    load(file = file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-XGBoostCARMAEvalOutput-TF.Rdata")), envir = .GlobalEnv)

                    # Save file without qualifier----
                    save(XGBoostCARMAEvalOutput, file = file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-XGBoostCARMAEvalOutput.Rdata")))

                    # Delete non winning model evaluation files----
                    file.remove(file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-XGBoostCARMAEvalOutput-TT.Rdata")))
                    file.remove(file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-XGBoostCARMAEvalOutput-TF.Rdata")))
                    file.remove(file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-XGBoostCARMAEvalOutput-FT.Rdata")))
                    file.remove(file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-XGBoostCARMAEvalOutput-FF.Rdata")))

                    # Finalize parameter settings and save to project list----
                    TS_CARMA_Transformation <- "TRUE"
                    TS_CARMA_Difference <- FALSE
                    ProjectList[["TS_CARMA_Transformation"]] <<- "TRUE"
                    ProjectList[["TS_CARMA_Difference"]] <<- FALSE
                  } else if(args == "FT") {

                    # Load winning model eval metrics----
                    load(file = file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-XGBoostCARMAEvalOutput-FT.Rdata")))

                    # Save file without qualifier----
                    save(XGBoostCARMAEvalOutput, file = file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-XGBoostCARMAEvalOutput.Rdata")))

                    # Delete non winning model evaluation files----
                    file.remove(file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-XGBoostCARMAEvalOutput-TT.Rdata")))
                    file.remove(file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-XGBoostCARMAEvalOutput-TF.Rdata")))
                    file.remove(file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-XGBoostCARMAEvalOutput-FT.Rdata")))
                    file.remove(file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-XGBoostCARMAEvalOutput-FF.Rdata")))

                    # Finalize parameter settings and save to project list----
                    TS_CARMA_Transformation <- "FALSE"
                    TS_CARMA_Difference <- TRUE
                    ProjectList[["TS_CARMA_Transformation"]] <<- "FALSE"
                    ProjectList[["TS_CARMA_Difference"]] <<- TRUE
                  } else {

                    # Load winning model eval metrics----
                    load(file = file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-XGBoostCARMAEvalOutput-FF.Rdata")), envir = .GlobalEnv)

                    # Save file without qualifier----
                    save(XGBoostCARMAEvalOutput, file = file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-XGBoostCARMAEvalOutput.Rdata")))

                    # Delete non winning model evaluation files----
                    file.remove(file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-XGBoostCARMAEvalOutput-TT.Rdata")))
                    file.remove(file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-XGBoostCARMAEvalOutput-TF.Rdata")))
                    file.remove(file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-XGBoostCARMAEvalOutput-FT.Rdata")))
                    file.remove(file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-XGBoostCARMAEvalOutput-FF.Rdata")))

                    # Finalize parameter settings and save to project list----
                    TS_CARMA_Transformation <- "FALSE"
                    TS_CARMA_Difference <- FALSE
                    ProjectList[["TS_CARMA_Transformation"]] <<- "FALSE"
                    ProjectList[["TS_CARMA_Difference"]] <<- FALSE
                  }
                }
              }

              # Trees----
              if(runversion == 1L) {
                Trees <- TS_CARMA_NTrees
              }

              # Build Model----
              datax <- data.table::copy(data)
              Forecast3 <- RemixAutoML::AutoXGBoostCARMA(
                data = datax,
                TrainOnFull = TrainOnFull,
                HierarchGroups = GroupVariableNames,
                TimeGroups = TS_TimeUnit,
                Quantiles_Selected = TS_CARMA_Quantiles_Selected,
                DebugMode = TRUE,
                TargetColumnName = TargetName,
                DateColumnName = DateName,
                GroupVariables = GroupVariableNames,
                FC_Periods = TS_FCPeriods,
                TimeUnit = TS_TimeUnit[1L],
                TargetTransformation = as.logical(TS_CARMA_Transformation),
                Lags = as.numeric(TS_CARMA_Lags),
                MA_Periods = as.numeric(TS_CARMA_MovingAverages),
                SD_Periods = as.numeric(TS_CARMA_MovingSD),
                Skew_Periods = as.numeric(TS_CARMA_MovingSkew),
                Kurt_Periods = as.numeric(TS_CARMA_MovingKurt),
                Quantile_Periods = as.numeric(TS_CARMA_MovingQuantiles),
                XREGS = NULL,
                Difference = TS_CARMA_Difference,
                FourierTerms = as.numeric(TS_CARMA_Fourier),
                CalendarVariables = as.logical(TS_CARMA_CalendarVariables),
                HolidayVariable = as.logical(TS_CARMA_HolidayVariables),
                HolidayLags = TS_CARMA_HolidayLags,
                HolidayMovingAverages = TS_CARMA_HolidayMovingAverages,
                TimeTrendVariable = as.logical(TS_CARMA_TimeTrend),
                ZeroPadSeries = NULL,
                DataTruncate = as.logical(TS_CARMA_DataTruncate),
                SplitRatios = Ratios,
                TreeMethod = if(TS_CARMA_TaskType == "GPU") "gpu_hist" else "hist",
                NThreads = TS_CARMA_NThreads,
                EvalMetric = TS_MetricEval,
                GridTune = as.logical(TS_CARMA_GridTune),
                GridEvalMetric = "mae",
                ModelCount = TS_CARMA_ModelCount,
                NTrees = Trees,
                PartitionType = "timeseries",
                Timer = TRUE)

              # Trees
              if(runversion == 5L) {
                Trees <- Forecast3$Model$niter
              } else {
                Trees <- TS_CARMA_NTrees
              }

              # Save data and model output----
              if(runs == 2L) {
                if(runversion == 2L) {
                  test3 <- Forecast3$Forecast
                  tryCatch({data.table::fwrite(test3, file = file.path(ProjectList[["ModelsFolderPath"]],paste0(TargetName,"-XGBoostCARMA.csv")))},
                           error = function(x) print("Error saving"))
                } else if(runversion == 5L) {
                  XGBoostCARMAEvalOutput <- Forecast3$ModelInformation
                  save(XGBoostCARMAEvalOutput, file = file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-XGBoostCARMAEvalOutput.Rdata")))
                }
              } else {
                if(runversion == 5L) {
                  test3 <- Forecast3$Forecast
                  tryCatch({data.table::fwrite(test3, file = file.path(ProjectList[["ModelsFolderPath"]],paste0(TargetName,"-XGBoostCARMA.csv")))},
                           error = function(x) print("Error saving"))
                } else {
                  XGBoostCARMAEvalOutput <- Forecast3$ModelInformation
                  XGBoost <- Forecast3$ModelInformation$EvaluationMetrics
                  data.table::set(
                    TS_PerformanceCollection,
                    i = eval(runversion),
                    j = "Metric",
                    value = XGBoost[Metric == eval(TS_MetricEval), "MetricValue"][[1L]])
                  if(runversion == 1L) {
                    save(XGBoostCARMAEvalOutput, file = file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-XGBoostCARMAEvalOutput-TT.Rdata")))
                  } else if(runversion == 2L) {
                    save(XGBoostCARMAEvalOutput, file = file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-XGBoostCARMAEvalOutput-TF.Rdata")))
                  } else if(runversion == 3L) {
                    save(XGBoostCARMAEvalOutput, file = file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-XGBoostCARMAEvalOutput-FT.Rdata")))
                  } else if(runversion == 4L) {
                    save(XGBoostCARMAEvalOutput, file = file.path(ProjectList[["MetaDataPath"]],paste0(TargetName,"-XGBoostCARMAEvalOutput-FF.Rdata")))
                  }
                }
              }
            }

            # 2. Update Progress Bar ----
            incProgress(amount = 1/n, message = "XGBoost-CARMA Complete")
          }

          # Notify user ----
          shinyWidgets::sendSweetAlert(session,title = NULL, text = "Model building is complete!", type = "success", btn_labels = "Ok", btn_colors = "green", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "60%")
        }

        # Clear memory ----
        gc()
        })
    })

    # Next and Previous Buttons ----
    shiny::observeEvent(input$link_to_automated_forecasting_autots_autocarma, {
      updateTabItems(session, inputId = "modelMenu", selected = "automated_forecasting_autots_autocarma")
    })
    shiny::observeEvent(input$link_to_automated_forecasting_autots_autocarma_1, {
      updateTabItems(session, inputId = "modelMenu", selected = "automated_forecasting_autots_autocarma")
    })

  #----

  # AUTOMATED FORECASTING - MODEL EVALUATION ----
    # UI Select Target, Models, Group Variables, Group Levels ----
    output$TSEval_timeSeriesTarget <- shiny::renderUI({
      if(exists("ProjectList")) {
        tryCatch({
          if(!is.null(ProjectList[["TS_timeSeriesTarget"]])) {
            pickerInput(inputId = "TSEval_timeSeriesTarget", label = "Select Target Variable", choices = ProjectList[["TS_timeSeriesTarget"]], selected = ProjectList[["TS_timeSeriesTarget"]][[1L]],
                        options = list(`actions-box` = TRUE, size = 10L, `selected-text-format` = "count > 1"), multiple = TRUE)
          } else {
            pickerInput(inputId = "TSEval_timeSeriesTarget", label = "Select Target Variable", choices = names(SourceData)[which(!sapply(SourceData,class) %chin% c("character","factor","Date","POSIXct","POSIXt"))], selected = names(SourceData)[which(!sapply(SourceData,class) %chin% c("character","factor","Date","POSIXct","POSIXt"))][1],
                        options = list(`actions-box` = TRUE, size = 10L, `selected-text-format` = "count > 1"), multiple = TRUE)
          }}, error = function(x) pickerInput(inputId = "TSEval_timeSeriesTarget", label = "Select Target Variable", choices = "!! No data is loaded", selected = "!! No data is loaded",
                                              options = list(`actions-box` = TRUE, size = 10L, `selected-text-format` = "count > 1"), multiple = TRUE))
      } else {
        tryCatch({pickerInput(inputId = "TSEval_timeSeriesTarget", label = "Select Target Variable", choices = names(SourceData)[which(!sapply(SourceData,class) %chin% c("character","factor","Date","POSIXct","POSIXt"))][[1]], selected = names(SourceData)[which(!sapply(SourceData,class) %chin% c("character","factor","Date","POSIXct","POSIXt"))][1],
                    options = list(`actions-box` = TRUE, size = 10L, `selected-text-format` = "count > 1"), multiple = TRUE)},
                 error = function(x) pickerInput(inputId = "TSEval_timeSeriesTarget", label = "Select Target Variable", choices = "!! No data is loaded", selected = "!! No data is loaded",
                                                 options = list(`actions-box` = TRUE, size = 10L, `selected-text-format` = "count > 1"), multiple = TRUE))
      }
    })
    output$TSEval_timeSeriesTarget2 <- shiny::renderUI({
      if(exists("ProjectList")) {
        tryCatch({
          if(!is.null(ProjectList[["TS_timeSeriesTarget"]])) {
            pickerInput(inputId = "TSEval_timeSeriesTarget2", label = "Select Target Variable", choices = ProjectList[["TS_timeSeriesTarget"]], selected = ProjectList[["TS_timeSeriesTarget"]][[1L]],
                        options = list(`actions-box` = TRUE, size = 10L, `selected-text-format` = "count > 1"), multiple = TRUE)
          } else {
            pickerInput(inputId = "TSEval_timeSeriesTarget2", label = "Select Target Variable", choices = names(SourceData)[which(!sapply(SourceData,class) %chin% c("character","factor","Date","POSIXct","POSIXt"))], selected = names(SourceData)[which(!sapply(SourceData,class) %chin% c("character","factor","Date","POSIXct","POSIXt"))][1],
                        options = list(`actions-box` = TRUE, size = 10L, `selected-text-format` = "count > 1"), multiple = TRUE)
          }}, error = function(x) pickerInput(inputId = "TSEval_timeSeriesTarget2", label = "Select Target Variable", choices = "!! No data is loaded", selected = "!! No data is loaded",
                                              options = list(`actions-box` = TRUE, size = 10L, `selected-text-format` = "count > 1"), multiple = TRUE))
      } else {
        tryCatch({pickerInput(inputId = "TSEval_timeSeriesTarget2", label = "Select Target Variable", choices = names(SourceData)[which(!sapply(SourceData,class) %chin% c("character","factor","Date","POSIXct","POSIXt"))][[1]], selected = names(SourceData)[which(!sapply(SourceData,class) %chin% c("character","factor","Date","POSIXct","POSIXt"))][1],
                    options = list(`actions-box` = TRUE, size = 10L, `selected-text-format` = "count > 1"), multiple = TRUE)},
                 error = function(x) pickerInput(inputId = "TSEval_timeSeriesTarget2", label = "Select Target Variable", choices = "!! No data is loaded", selected = "!! No data is loaded",
                                                 options = list(`actions-box` = TRUE, size = 10L, `selected-text-format` = "count > 1"), multiple = TRUE))
      }
    })
    output$TS_ModelID <- shiny::renderUI({
      if(exists("ProjectList")) {
        tryCatch({
          if(!is.null(ProjectList[["TS_ModelID"]])) {
            pickerInput(inputId = "TS_ModelID", label = "Select Model", choices = unique(c(ProjectList[["ML_Models"]], ProjectList[["TS_Models"]])), selected = ProjectList[["TS_ModelID"]][[1L]],
                          options = list(`actions-box` = TRUE, size = 10L, `selected-text-format` = "count > 1"), multiple = FALSE)
          } else {
            pickerInput(inputId = "TS_ModelID", label = "Select Model", choices = unique(c(input$TSMLModelsSelection, input$TimeSeriesModelsSelection)), selected = "", options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 1"), multiple = FALSE)
          }}, error = function(x) pickerInput(inputId = "TS_ModelID", label = "Select Model", choices = "!! No project is loaded", selected = "!! No project is loaded", options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 1"), multiple = FALSE))
      } else {
        pickerInput(inputId = "TS_ModelID", label = "Select Model", choices = "!! No project is loaded", selected = "!! No project is loaded" , options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 1"), multiple = FALSE)
      }
    })
    output$TSEval_timeSeriesGroupVars <- shiny::renderUI({
      if(exists("ProjectList")) {
        tryCatch({
          if(!is.null(ProjectList[["TS_timeSeriesGroupVars"]])) {
            pickerInput(inputId = "TSEval_timeSeriesGroupVars", label = "Select Group Variables", choices = names(SourceData), selected = ProjectList[["TS_timeSeriesGroupVars"]],
                        options = list(`actions-box` = TRUE, size = 10L, `selected-text-format` = "count > 1"), multiple = TRUE)
          } else {
            pickerInput(inputId = "TSEval_timeSeriesGroupVars", label = "Select Group Variables", choices = names(SourceData), selected = "",
                        options = list(`actions-box` = TRUE, size = 10L, `selected-text-format` = "count > 1"), multiple = TRUE)
          }}, error = function(x) pickerInput(inputId = "TSEval_timeSeriesGroupVars", label = "Select Group Variables", choices = "!! No data is loaded", selected = "!! No data is loaded",
                                              options = list(`actions-box` = TRUE, size = 10L, `selected-text-format` = "count > 1"), multiple = TRUE))
      } else {
        tryCatch({pickerInput(inputId = "TSEval_timeSeriesGroupVars", label = "Select Group Variables", choices = names(SourceData), selected = "",
                    options = list(`actions-box` = TRUE, size = 10L, `selected-text-format` = "count > 1"), multiple = TRUE)},
                 error = function(x) pickerInput(inputId = "TSEval_timeSeriesGroupVars", label = "Select Group Variables", choices = "!! No data is loaded", selected = "!! No data is loaded",
                                                 options = list(`actions-box` = TRUE, size = 10L, `selected-text-format` = "count > 1"), multiple = TRUE))
      }
    })
    output$TSEval_timeSeriesGroupVars2 <- shiny::renderUI({
      if(exists("ProjectList")) {
        tryCatch({
          if(!is.null(ProjectList[["TS_timeSeriesGroupVars"]])) {
            pickerInput(inputId = "TSEval_timeSeriesGroupVars2", label = "Select Group Variables", choices = names(SourceData), selected = ProjectList[["TS_timeSeriesGroupVars"]],
                        options = list(`actions-box` = TRUE, size = 10L, `selected-text-format` = "count > 1"), multiple = TRUE)
          } else {
            pickerInput(inputId = "TSEval_timeSeriesGroupVars2", label = "Select Group Variables", choices = names(SourceData), selected = "",
                        options = list(`actions-box` = TRUE, size = 10L, `selected-text-format` = "count > 1"), multiple = TRUE)
          }}, error = function(x) pickerInput(inputId = "TSEval_timeSeriesGroupVars2", label = "Select Group Variables", choices = "!! No data is loaded", selected = "!! No data is loaded",
                                              options = list(`actions-box` = TRUE, size = 10L, `selected-text-format` = "count > 1"), multiple = TRUE))
      } else {
        tryCatch({pickerInput(inputId = "TSEval_timeSeriesGroupVars2", label = "Select Group Variables", choices = names(SourceData), selected = "",
                    options = list(`actions-box` = TRUE, size = 10L, `selected-text-format` = "count > 1"), multiple = TRUE)},
                 error = function(x) pickerInput(inputId = "TSEval_timeSeriesGroupVars2", label = "Select Group Variables", choices = "!! No data is loaded", selected = "!! No data is loaded",
                                                 options = list(`actions-box` = TRUE, size = 10L, `selected-text-format` = "count > 1"), multiple = TRUE))
      }
    })
    output$TS_Group1Levels2 <- shiny::renderUI({
      if(exists("SourceData")) {
        if(!is.null(input$TSEval_timeSeriesGroupVars2)) {
          if(length(input$TSEval_timeSeriesGroupVars2) >= 1L) {
            pickerInput(inputId = "TS_Group1Levels2", label = paste0(input$TSEval_timeSeriesGroupVars2[[1L]]," Levels"),
                        choices = c(as.character(unique(SourceData[[eval(input$TSEval_timeSeriesGroupVars2[[1L]])]]))), selected = NULL,
                        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 1"), multiple = TRUE, width = "100%")
          } else {
            pickerInput(inputId = "TS_Group1Levels2", label = "< N/A >",
                        choices = NULL, selected = NULL, multiple = TRUE, width = "100%")
          }
        } else {
          pickerInput(inputId = "TS_Group1Levels2", label = "< N/A >",
                      choices = NULL, selected = NULL, multiple = TRUE, width = "100%")
        }
      } else {
        pickerInput(inputId = "TS_Group1Levels2", label = "< N/A >",
                    choices = NULL, selected = NULL, multiple = TRUE, width = "100%")
      }
    })
    output$TS_Group2Levels2 <- shiny::renderUI({
      if(exists("SourceData")) {
        if(!is.null(input$TSEval_timeSeriesGroupVars2)) {
          if(length(input$TSEval_timeSeriesGroupVars2) >= 2L) {
            pickerInput(inputId = "TS_Group2Levels2", label = paste0(input$TSEval_timeSeriesGroupVars2[[2L]]," Levels"),
                        choices = c(as.character(unique(SourceData[[eval(input$TSEval_timeSeriesGroupVars2[[2L]])]]))), selected = NULL,
                        options = list(`actions-box` = TRUE, size = 10L, `selected-text-format` = "count > 1"), multiple = TRUE, width = "100%")
          } else {
            pickerInput(inputId = "TS_Group2Levels2", label = "< N/A >",
                        choices = NULL, selected = NULL, multiple = TRUE, width = "100%")
          }
        } else {
          pickerInput(inputId = "TS_Group2Levels2", label = "< N/A >",
                      choices = NULL, selected = NULL, multiple = TRUE, width = "100%")
        }
      } else {
        pickerInput(inputId = "TS_Group2Levels2", label = "< N/A >",
                    choices = NULL, selected = NULL, multiple = TRUE, width = "100%")
      }
    })
    output$TS_Group3Levels2 <- shiny::renderUI({
      if(exists("SourceData")) {
        if(!is.null(input$TSEval_timeSeriesGroupVars2)) {
          if(length(input$TSEval_timeSeriesGroupVars2) >= 3L) {
            pickerInput(inputId = "TS_Group3Levels2", label = paste0(input$TSEval_timeSeriesGroupVars2[[3]]," Levels"),
                        choices = c(as.character(unique(SourceData[[eval(input$TSEval_timeSeriesGroupVars2[[3L]])]]))), selected = NULL,
                        options = list(`actions-box` = TRUE, size = 10L, `selected-text-format` = "count > 1"), multiple = TRUE, width = "100%")
          } else {
            pickerInput(inputId = "TS_Group3Levels2", label = "< N/A >",
                        choices = NULL, selected = NULL, multiple = TRUE, width = "100%")
          }
        } else {
          pickerInput(inputId = "TS_Group3Levels2", label = "< N/A >",
                      choices = NULL, selected = NULL, multiple = TRUE, width = "100%")
        }
      } else {
        pickerInput(inputId = "TS_Group3Levels2", label = "< N/A >",
                    choices = NULL, selected = NULL, multiple = TRUE, width = "100%")
      }
    })
    output$Evaluate <- shiny::renderUI({
      pickerInput(inputId = "Evaluate", label = "TRUE for Backtesting Results", choices = c("TRUE","FALSE"), selected = "FALSE", options = list(`actions-box` = TRUE, size = 10L, `selected-text-format` = "count > 1"), multiple = FALSE, width = "100%")
    })

    # Boxes ----
    output$FC_TickMarksX <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "FC_TickMarksX", Label = "Tick marks x-axis", Choices = c("1 year","1 day","3 day","1 week","2 week","1 month","3 month","6 month","2 year","5 year","10 year","1 minute","15 minutes","30 minutes","1 hour","3 hour","6 hour","12 hour"), SelectedDefault = "1 year", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$FC_LineWidth <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID = "FC_LineWidth", Label = "Select Holiday Count MA's", Min = 0.10, Max = 5, Value = 0.50, Step = 0.10)
    })
    output$FC_AngleY <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID = "FC_AngleY", Label = "Y-axis text angle", Min = 0, Max = 360, Value = 0, Step = 5)
    })
    output$FC_AngleX <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID = "FC_AngleX", Label = "X-axis text angle", Min = 0, Max = 360, Value = 35, Step = 5)
    })
    output$FC_TextSize <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID = "FC_TextSize", Label = "Text size", Min = 1, Max = 50, Value = 18, Step = 1)
    })
    output$FC_LegendPosition <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "FC_LegendPosition", Label = "Legend position", Choices = c("bottom","top","left","right"), SelectedDefault = "bottom", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$FC_LegendTextSize <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID = "FC_LegendTextSize", Label = "Legend text size", Min = 1, Max = 48, Value = 10, Step = 2)
    })
    output$FC_TextColor <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "FC_TextColor", Label = "Text color", Choices = grDevices::colors(), SelectedDefault = "darkblue", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$FC_LineColor <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "FC_LineColor", Label = "Line color", Choices = grDevices::colors(), SelectedDefault = "blue", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$FC_LegendTextColor <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "FC_LegendTextColor", Label = "Legend text color", Choices = grDevices::colors(), SelectedDefault = "darkblue", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$FC_ChartColor <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "FC_ChartColor", Label = "Chart color", Choices = grDevices::colors(), SelectedDefault = "lightsteelblue1", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$FC_GridColor <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "FC_GridColor", Label = "Grid lines color", Choices = grDevices::colors(), SelectedDefault = "white", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$FC_BackGroundColor <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "FC_BackGroundColor", Label = "Background color", Choices = grDevices::colors(), SelectedDefault = "gray95", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$FC_BorderColor <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "FC_BorderColor", Label = "Border color", Choices = grDevices::colors(), SelectedDefault = "darkblue", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$FC_PredictionIntervals <- shiny::renderUI({
      if(input$TS_ModelID %chin% c("CatBoost-CARMA","XGBoost-CARMA","H2O-CARMA")) {
        RemixAutoML::PickerInput(InputID = "FC_PredictionIntervals", Label = "Prediction Intervals N/A", Choices = "FALSE", SelectedDefault = "FALSE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
      } else {
        RemixAutoML::PickerInput(InputID = "FC_PredictionIntervals", Label = "Prediction Intervals", Choices = c("FALSE","TRUE"), SelectedDefault = "FALSE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
      }
    })
    output$FC_ForecastLineColor <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "FC_ForecastLineColor", Label = "Actuals line color", Choices = grDevices::colors(), SelectedDefault = "red", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$FC_PredictionIntervalColorInner <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "FC_PredictionIntervalColorInner", Label = "Prediction Interval Color 1", Choices = grDevices::colors(), SelectedDefault = "white", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$FC_PredictionIntervalColorOuter <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "FC_PredictionIntervalColorOuter", Label = "Prediction Interval Color 2", Choices = grDevices::colors(), SelectedDefault = "darkblue", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$FC_AggregateFunction <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "FC_AggregateFunction", Label = "Aggregation method", Choices = c("mean","sum"), SelectedDefault = "sum", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$FC_OtherGroups <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "FC_OtherGroups", Label = "View 'other' Group", Choices = c("TRUE","FALSE"), SelectedDefault = "FALSE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$FC_NumberGroupsDisplay <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "FC_NumberGroupsDisplay", Label = "Lines to display", Choices = 1L:100L, SelectedDefault = 10, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    # Update Plot Options ----
    shiny::observeEvent(eventExpr = input$TS_ModelID, {
      if(input$TS_ModelID %chin% c("CatBoost-CARMA","XGBoost-CARMA","H2O-CARMA")) {
        updatePickerInput(session, inputId = "FC_PredictionIntervals", selected = "FALSE", choices = "FALSE", label = "Prediction Interval N/A")
      }
    })

    # Reset Plot Settings ----
    shiny::observeEvent(eventExpr = input$FC_ResetPlotSettings, {

      # TS_AggregateFunction
      updatePickerInput(session = session, inputId = "FC_AggregateFunction", label = "Aggregation Method for 'other'", choices = c("mean","sum"), selected = "sum")
      ProjectList[["FC_AggregateFunction"]] <<- input$FC_AggregateFunction

      # TS_LegendTextColor
      updatePickerInput(session = session, inputId = "FC_LegendTextColor", label = "Legend text color", choices = grDevices::colors(), selected = "darkblue")
      ProjectList[["FC_LegendTextColor"]] <<- input$FC_LegendTextColor

      # TS_LegendTextSize
      updateNumericInput(session = session, inputId = "FC_LegendTextSize", label = "Legend text size", value = 10, min = 1, max = 48, step = 2)
      ProjectList[["FC_LegendTextSize"]] <<- input$FC_LegendTextSize

      # Line Color
      updatePickerInput(session = session, inputId = "FC_LineColor", label = "Line Color", choices = grDevices::colors(), selected = "blue")
      ProjectList[["FC_LineColor"]] <<- input$FC_LineColor

      # TS_LineWidth
      updateNumericInput(session = session, inputId = "FC_LineWidth", label = "Line size", value = 0.5, min = 0.10, max = 5, step = 0.10)
      ProjectList[["FC_LineWidth"]] <<- input$FC_LineWidth

      # TS_TextSize
      updateNumericInput(session = session, inputId = "FC_TextSize", label = "Text size", value = 18, min = 1, max = 50, step = 1)
      ProjectList[["FC_TextSize"]] <<- input$FC_TextSize

      # TS_NumberGroupsDisplay
      updateNumericInput(session = session, inputId = "FC_NumberGroupsDisplay", label = "Lines to display", value = 5, min = 1, max = 50, step = 1)
      ProjectList[["FC_NumberGroupsDisplay"]] <<- input$FC_NumberGroupsDisplay

      # TickMarksX
      updatePickerInput(session = session, inputId = "FC_TickMarksX", label = "Tick marks X-Axis",
                        choices = c("1 year","1 day","3 day","1 week","2 week","1 month","3 month","6 month","2 year","5 year","10 year","1 minute","15 minutes","30 minutes","1 hour","3 hour","6 hour","12 hour"),
                        selected = "1 year")
      ProjectList[["FC_TickMarksX"]] <<- input$FC_TickMarksX

      # TS_LegendPosition
      updatePickerInput(session = session, inputId = "FC_LegendPosition", label = "Legend position", choices = c("bottom","top","left","right"), selected = "bottom")
      ProjectList[["FC_LegendPosition"]] <<- input$FC_LegendPosition

      # TS_AngleX
      updateNumericInput(session = session, inputId = "FC_AngleX", label = "X-axis text angle", value = 35, min = 0, max = 360, step = 5)
      ProjectList[["FC_AngleX"]] <<- input$FC_AngleX

      # Y Axis angle
      updateNumericInput(session = session, inputId = "FC_AngleY", label = "X-axis text angle", value = 0, min = 0, max = 360, step = 5)
      ProjectList[["FC_AngleY"]] <<- input$FC_AngleY

      # TS_ChartColor
      updatePickerInput(session = session, inputId = "FC_ChartColor", label = "Chart color", choices = grDevices::colors(), selected = "lightsteelblue1")
      ProjectList[["FC_ChartColor"]] <<- input$FC_ChartColor

      # TS_BorderColor
      updatePickerInput(session = session, inputId = "FC_BorderColor", label = "Border color", choices = grDevices::colors(), selected = "darkblue")
      ProjectList[["FC_BorderColor"]] <<- input$FC_BorderColor

      # TS_TextColor
      updatePickerInput(session = session, inputId = "FC_TextColor", label = "Text color", choices = grDevices::colors(), selected = "darkblue")
      ProjectList[["FC_TextColor"]] <<- input$FC_TextColor

      # TS_GridColor
      updatePickerInput(session = session, inputId = "FC_GridColor", label = "Grid lines color", choices = grDevices::colors(), selected = "white")
      ProjectList[["FC_GridColor"]] <<- input$FC_GridColor

      # TS_BackGroundColor
      updatePickerInput(session = session, inputId = "FC_BackGroundColor", label = "Background color", choices = grDevices::colors(), selected = "gray95")
      ProjectList[["FC_BackGroundColor"]] <<- input$FC_BackGroundColor

      # TS_OtherGroups
      updatePickerInput(session = session, inputId = "FC_OtherGroups", label = "Show Other Groups", choices = c("FALSE","TRUE"), selected = "FALSE")
      ProjectList[["FC_OtherGroups"]] <<- input$FC_OtherGroups

      # Group Variable----
      a <- character(0)
      if(!identical(a,as.character(input$TSEval_timeSeriesGroupVars2))) {
        groupvariables <<- as.character(input$TSEval_timeSeriesGroupVars2)
      } else {
        groupvariables <<- NULL
      }

      # Build plots ----
      output$TS_ForecastPlot <- plotly::renderPlotly({

        # Skip if null
        if(!is.null(targetvariables) & !is.null(datevariables)) {

          # Subset FinalForecastData before plotting ----
          UniqueModelRank <- FinalForecastData[, list(ModelRank = max(ModelRank)), by = "ModelID"]
          PlotDataForecast <<- merge(FinalForecastData, UniqueModelRank, by = c("ModelID","ModelRank"), all = FALSE)

          # Arguments dependent on group variable existence ----
          if(!is.null(groupvariables)) {
            targetVariable <- "Forecast"
            Groupvariables <- groupvariables
            levelsToDisplay <- 5
            forecast <- FALSE
          } else {
            targetVariable <- c(targetvariables,"Forecast")
            Groupvariables <- NULL
            levelsToDisplay <- NULL
            forecast <- TRUE
          }

          # Build plot ----
          TimeSeriesPlotObject <<- RemixAutoML::TimeSeriesPlotter(
            data = PlotDataForecast,
            TargetVariable = targetVariable,
            DateVariable = datevariables,
            GroupVariables = Groupvariables,
            VLineDate = max(SourceData[[eval(datevariables)]], na.rm = TRUE),
            Aggregate = "mean",
            NumberGroupsDisplay = 5L,
            LevelsToDisplay = levelsToDisplay,
            OtherGroupLabel = "OtherGroups",
            DisplayOtherGroup = TRUE,
            TextSize = 12L,
            LineWidth = 0.5,
            Color = "red",
            XTickMarks = '1 year',
            AngleX = 35L,
            AngleY = 0L,
            ChartColor = 'lightsteelblue1',
            BorderColor = "darkblue",
            TextColor = "darkblue",
            GridColor = "white",
            BackGroundColor = "gray95",
            LegendPosition = "right",
            LegendTextColor = "darkblue",
            LegendTextSize = 10L,
            Forecast = forecast,
            PredictionIntervals = FALSE,
            TS_ModelID = as.character(input$TS_ModelID),
            ForecastLineColor = "red",
            PredictionIntervalColorInner = "lightsteelblue1",
            PredictionIntervalColorOuter = "lightsteelblue1")

          # Convert to plotly
          plotly::ggplotly(TimeSeriesPlotObject, tooltip = NULL)

        } else {

          # Notify User that Process Has Begun ----
          shinyWidgets::sendSweetAlert(session, title = NULL, text = "You need to have a Target Variable and Date Variable selected", type = "error", btn_labels = "Ok", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "60%")
        }
      })

      # Notify user ----
      showNotification(ui = "Plot settings are reset!", closeButton = TRUE, duration = 2L, session = session, type = "message")
    })

    # Create Evaluation DataTable ----
    shiny::observeEvent({
      input$TS_CollectModelMetrics
      input$TS_BuildForecastPlot}, {

        # Evaluation Mode ----
        EvalMode <- as.logical(input$Evaluate)

        # Define target variable ----
        targetvariable <<- as.character(input$TSEval_timeSeriesTarget)
        datevariable <<- as.character(input$TS_timeSeriesDateColumn)
        if(class(input$TSEval_timeSeriesGroupVars) == "NULL") {
          groupvariable <- NULL
        } else {
          groupvariable <- input$TSEval_timeSeriesGroupVars
        }

        # Counter ----
        Counter <- 0L

        # Grid Collection loop ----
        for(tsmodels in sort(x = unique(c(ProjectList[["TS_Models"]], ProjectList[["ML_Models"]])), decreasing = FALSE)) {

          # ARIMA ----
          if(tsmodels == "Supercharged-SARIMA") {
            if(Counter != 0L) {
              temp1 <- data.table::fread(file.path(ProjectList[["ModelsFolderPath"]],paste0(targetvariable,"-Arima_ExperimentGrid.csv")))
              temp1 <- temp1[order(ModelRank)][1,.SD, .SDcols = TS_Keep][, Model := tsmodels]
              if(TS_Keep == "Validate_MSE") {
                temp1[, `Validation Metric` := "MSE"]
                data.table::setnames(temp1, "Validate_MSE", "MetricValue", skip_absent = TRUE)
              } else {
                temp1[, `Validation Metric` := "MAE"]
                data.table::setnames(temp1, "Validate_MAE", "MetricValue", skip_absent = TRUE)
              }
              FinalGrids <- data.table::rbindlist(list(FinalGrids,temp1), fill = TRUE)
              Counter <- Counter + 1L
            } else {
              temp1 <- data.table::fread(file.path(ProjectList[["ModelsFolderPath"]],paste0(targetvariable,"-Arima_ExperimentGrid.csv")))
              temp1 <- temp1[order(ModelRank)][1,.SD, .SDcols = TS_Keep][, Model := tsmodels]
              if(TS_Keep == "Validate_MSE") {
                temp1[, `Validation Metric` := "MSE"]
                data.table::setnames(temp1, "Validate_MSE", "MetricValue", skip_absent = TRUE)
              } else {
                temp1[, `Validation Metric` := "MAE"]
                data.table::setnames(temp1, "Validate_MAE", "MetricValue", skip_absent = TRUE)
              }
              FinalGrids <- temp1
              Counter <- Counter + 1L
            }
          }

          # NNET ----
          if(tsmodels == "Supercharged-NNET") {
            if(Counter != 0L) {
              temp1 <- data.table::fread(file.path(ProjectList[["ModelsFolderPath"]],paste0(targetvariable,"-NNET_ExperimentGrid.csv")))
              temp1 <- temp1[order(ModelRank)][1,.SD, .SDcols = TS_Keep][, Model := tsmodels]
              if(TS_Keep == "Validate_MSE") {
                temp1[, `Validation Metric` := "MSE"]
                data.table::setnames(temp1, "Validate_MSE", "MetricValue", skip_absent = TRUE)
              } else {
                temp1[, `Validation Metric` := "MAE"]
                data.table::setnames(temp1, "Validate_MAE", "MetricValue", skip_absent = TRUE)
              }
              FinalGrids <- data.table::rbindlist(list(FinalGrids,temp1), fill = TRUE)
              Counter <- Counter + 1L
            } else {
              temp1 <- data.table::fread(file.path(ProjectList[["ModelsFolderPath"]],paste0(targetvariable,"-NNET_ExperimentGrid.csv")))
              temp1 <- temp1[order(ModelRank)][1,.SD, .SDcols = TS_Keep][, Model := tsmodels]
              if(TS_Keep == "Validate_MSE") {
                temp1[, `Validation Metric` := "MSE"]
                data.table::setnames(temp1, "Validate_MSE", "MetricValue", skip_absent = TRUE)
              } else {
                temp1[, `Validation Metric` := "MAE"]
                data.table::setnames(temp1, "Validate_MAE", "MetricValue", skip_absent = TRUE)
              }
              FinalGrids <- temp1
              Counter <- Counter + 1L
            }
          }

          # ARFIMA ----
          if(tsmodels == "ARFIMA") {
            if(Counter != 0L) {
              temp1 <- data.table::fread(file.path(ProjectList[["ModelsFolderPath"]],paste0(targetvariable,"-ARFIMA_ExperimentGrid.csv")))
              temp1 <- temp1[order(ModelRank)][1,.SD, .SDcols = TS_Keep][, Model := tsmodels]
              if(TS_Keep == "Validate_MSE") {
                temp1[, `Validation Metric` := "MSE"]
                data.table::setnames(temp1, "Validate_MSE", "MetricValue", skip_absent = TRUE)
              } else {
                temp1[, `Validation Metric` := "MAE"]
                data.table::setnames(temp1, "Validate_MAE", "MetricValue", skip_absent = TRUE)
              }
              FinalGrids <- data.table::rbindlist(list(FinalGrids,temp1), fill = TRUE)
              Counter <- Counter + 1L
            } else {
              temp1 <- data.table::fread(file.path(ProjectList[["ModelsFolderPath"]],paste0(targetvariable,"-ARFIMA_ExperimentGrid.csv")))
              temp1 <- temp1[order(ModelRank)][1,.SD, .SDcols = TS_Keep][, Model := tsmodels]
              if(TS_Keep == "Validate_MSE") {
                temp1[, `Validation Metric` := "MSE"]
                data.table::setnames(temp1, "Validate_MSE", "MetricValue", skip_absent = TRUE)
              } else {
                temp1[, `Validation Metric` := "MAE"]
                data.table::setnames(temp1, "Validate_MAE", "MetricValue", skip_absent = TRUE)
              }
              FinalGrids <- temp1
              Counter <- Counter + 1L
            }
          }

          # TBATS ----
          if(tsmodels == "TBATS") {
            if(Counter != 0L) {
              temp1 <- data.table::fread(file.path(ProjectList[["ModelsFolderPath"]],paste0(targetvariable,"-TBATS_ExperimentGrid.csv")))
              temp1 <- temp1[order(ModelRank)][1,.SD, .SDcols = TS_Keep][, Model := tsmodels]
              if(TS_Keep == "Validate_MSE") {
                temp1[, `Validation Metric` := "MSE"]
                data.table::setnames(temp1, "Validate_MSE", "MetricValue", skip_absent = TRUE)
              } else {
                temp1[, `Validation Metric` := "MAE"]
                data.table::setnames(temp1, "Validate_MAE", "MetricValue", skip_absent = TRUE)
              }
              FinalGrids <- data.table::rbindlist(list(FinalGrids,temp1), fill = TRUE)
              Counter <- Counter + 1L
            } else {
              temp1 <- data.table::fread(file.path(ProjectList[["ModelsFolderPath"]],paste0(targetvariable,"-TBATS_ExperimentGrid.csv")))
              temp1 <- temp1[order(ModelRank)][1,.SD, .SDcols = TS_Keep][, Model := tsmodels]
              if(TS_Keep == "Validate_MSE") {
                temp1[, `Validation Metric` := "MSE"]
                data.table::setnames(temp1, "Validate_MSE", "MetricValue", skip_absent = TRUE)
              } else {
                temp1[, `Validation Metric` := "MAE"]
                data.table::setnames(temp1, "Validate_MAE", "MetricValue", skip_absent = TRUE)
              }
              FinalGrids <- temp1
              Counter <- Counter + 1L
            }
          }

          # ETS ----
          if(tsmodels == "ETS") {
            if(Counter != 0L) {
              temp1 <- data.table::fread(file.path(ProjectList[["ModelsFolderPath"]],paste0(targetvariable,"-ETS_ExperimentGrid.csv")))
              temp1 <- temp1[order(ModelRank)][1,.SD, .SDcols = TS_Keep][, Model := tsmodels]
              if(TS_Keep == "Validate_MSE") {
                temp1[, `Validation Metric` := "MSE"]
                data.table::setnames(temp1, "Validate_MSE", "MetricValue", skip_absent = TRUE)
              } else {
                temp1[, `Validation Metric` := "MAE"]
                data.table::setnames(temp1, "Validate_MAE", "MetricValue", skip_absent = TRUE)
              }
              FinalGrids <- data.table::rbindlist(list(FinalGrids,temp1), fill = TRUE)
              Counter <- Counter + 1L
            } else {
              temp1 <- data.table::fread(file.path(ProjectList[["ModelsFolderPath"]],paste0(targetvariable,"-ETS_ExperimentGrid.csv")))
              temp1 <- temp1[order(ModelRank)][1,.SD, .SDcols = TS_Keep][, Model := tsmodels]
              if(TS_Keep == "Validate_MSE") {
                temp1[, `Validation Metric` := "MSE"]
                data.table::setnames(temp1, "Validate_MSE", "MetricValue", skip_absent = TRUE)
              } else {
                temp1[, `Validation Metric` := "MAE"]
                data.table::setnames(temp1, "Validate_MAE", "MetricValue", skip_absent = TRUE)
              }
              FinalGrids <- temp1
              Counter <- Counter + 1L
            }
          }

          # TSLM ----
          if(tsmodels == "TSLM") {

            # Increment Counter ----
            Counter <- Counter + 1L

            if(Counter != 0L) {
              temp1 <- data.table::fread(file.path(ProjectList[["ModelsFolderPath"]],paste0(targetvariable,"-TSLM_ExperimentGrid.csv")))
              temp1 <- temp1[order(ModelRank)][1,.SD, .SDcols = TS_Keep][, Model := tsmodels]
              if(TS_Keep == "Validate_MSE") {
                temp1[, `Validation Metric` := "MSE"]
                data.table::setnames(temp1, "Validate_MSE", "MetricValue", skip_absent = TRUE)
              } else {
                temp1[, `Validation Metric` := "MAE"]
                data.table::setnames(temp1, "Validate_MAE", "MetricValue", skip_absent = TRUE)
              }
              FinalGrids <- data.table::rbindlist(list(FinalGrids,temp1), fill = TRUE)
              Counter <- Counter + 1L
            } else {
              temp1 <- data.table::fread(file.path(ProjectList[["ModelsFolderPath"]],paste0(targetvariable,"-TSLM_ExperimentGrid.csv")))
              temp1 <- temp1[order(ModelRank)][1,.SD, .SDcols = TS_Keep][, Model := tsmodels]
              if(TS_Keep == "Validate_MSE") {
                temp1[, `Validation Metric` := "MSE"]
                data.table::setnames(temp1, "Validate_MSE", "MetricValue", skip_absent = TRUE)
              } else {
                temp1[, `Validation Metric` := "MAE"]
                data.table::setnames(temp1, "Validate_MAE", "MetricValue", skip_absent = TRUE)
              }
              FinalGrids <- temp1
              Counter <- Counter + 1L
            }
          }

          # Catboost ----
          if(tsmodels == "CatBoost-CARMA") {

            # Increment Counter ----
            Counter <- Counter + 1L

            # Load FC data ----
            FCData <- tryCatch({data.table::fread(file.path(ProjectList$ModelsFolderPath, paste0(ProjectList$TS_timeSeriesTarget, "_AutoCatBoostCARMA_Forecast.csv")))}, error = function(x) {
              tryCatch({data.table::fread(file.path(ProjectList$ModelsFolderPath, paste0(ProjectList$TS_timeSeriesTarget, "_AutoCatBoostCARMA_Forecast.csv")))}, error = function(x) NULL)})
            data.table::setnames(FCData, ProjectList$TS_timeSeriesTarget, paste0("Add_",ProjectList$TS_timeSeriesTarget))
            for(x in ProjectList$GroupVariableNames) if(!is.character(FCData[[eval(x)]])) FCData[, eval(x) := as.character(get(x))]

            # EvalData Load ----
            if(!exists("EvalData")) EvalData <- data.table::fread(file.path(ProjectList$DataFolderPath, "EvalData.csv"))
            for(x in ProjectList$GroupVariableNames) if(!is.character(EvalData[[eval(x)]])) EvalData[, eval(x) := as.character(get(x))]

            # Combine data sets ----
            FCData <- merge(FCData, EvalData, by = c(ProjectList$DateVariableName, ProjectList$GroupVariableNames), all = FALSE)

            # Remove historical records ----
            EvalDataPass <- FCData[is.na(get(paste0("Add_", ProjectList$TS_timeSeriesTarget)))]

            # Add model name ----
            EvalDataPass[, ModelID := "CatBoost"]
            EvalDataPass[, DateTime := Sys.time()]

            # Prepare data for evaluation
            temp1 <- GenerateEvaluationMetrics(
              EvalData = EvalDataPass,
              TargetName = input$TS_timeSeriesTarget,
              DateName = ProjectList$DateVariableName,
              GroupNames = ProjectList$GroupVariableNames)

            # Finalize data ----
            if(Counter == 1L) {
              FinalGrids <- temp1
              PlotData <- EvalDataPass
            } else {
              FinalGrids <- data.table::rbindlist(list(FinalGrids, temp1))
              PlotData <- data.table::rbindlist(list(PlotData, EvalDataPass))
            }
          }

          # XGBoost ----
          if(tsmodels == "XGBoost-CARMA") {

            # Increment Counter ----
            Counter <- Counter + 1L

            # Load FC data ----
            FCData <- tryCatch({data.table::fread(file.path(ProjectList$ModelsFolderPath, paste0(ProjectList$TS_timeSeriesTarget, "_AutoXGBoostCARMA_Forecast.csv")))}, error = function(x) {
              tryCatch({data.table::fread(file.path(ProjectList$ModelsFolderPath, paste0(ProjectList$TS_timeSeriesTarget, "_AutoXGBoostCARMA_Forecast.csv")))}, error = function(x) NULL)})
            data.table::setnames(FCData, ProjectList$TS_timeSeriesTarget, paste0("Add_",ProjectList$TS_timeSeriesTarget))
            for(x in ProjectList$GroupVariableNames) if(!is.character(FCData[[eval(x)]])) FCData[, eval(x) := as.character(get(x))]

            # EvalData Load ----
            if(!exists("EvalData")) EvalData <- data.table::fread(file.path(ProjectList$DataFolderPath, "EvalData.csv"))
            for(x in ProjectList$GroupVariableNames) if(!is.character(EvalData[[eval(x)]])) EvalData[, eval(x) := as.character(get(x))]

            # Combine data sets ----
            FCData <- merge(FCData, EvalData, by = c(ProjectList$DateVariableName, ProjectList$GroupVariableNames), all = FALSE)

            # Remove historical records ----
            EvalDataPass <- FCData[is.na(get(paste0("Add_", ProjectList$TS_timeSeriesTarget)))]

            # Add model name ----
            EvalDataPass[, ModelID := "XGBoost"]
            EvalDataPass[, DateTime := Sys.time()]

            # Prepare data for evaluation
            temp1 <- GenerateEvaluationMetrics(
              EvalData = EvalDataPass,
              TargetName = input$TS_timeSeriesTarget,
              DateName = ProjectList$DateVariableName,
              GroupNames = ProjectList$GroupVariableNames)

            # Finalize data ----
            if(Counter == 1L) {
              FinalGrids <- temp1
              PlotData <- EvalDataPass
            } else {
              FinalGrids <- data.table::rbindlist(list(FinalGrids, temp1))
              PlotData <- data.table::rbindlist(list(PlotData, EvalDataPass))
            }
          }

          # H2O-CARMA ----
          if(tsmodels == "H2O-CARMA") {

            # Increment Counter ----
            Counter <- Counter + 1L

            # Load FC data ----
            FCData <- tryCatch({data.table::fread(file.path(ProjectList$ModelsFolderPath, paste0(ProjectList$TS_timeSeriesTarget, "_AutoH2OCARMA_Forecast.csv")))}, error = function(x) {
              tryCatch({data.table::fread(file.path(ProjectList$ModelsFolderPath, paste0(ProjectList$TS_timeSeriesTarget, "_AutoH2OCARMA_Forecast.csv")))}, error = function(x) NULL)})
            data.table::setnames(FCData, ProjectList$TS_timeSeriesTarget, paste0("Add_",ProjectList$TS_timeSeriesTarget))
            for(x in ProjectList$GroupVariableNames) if(!is.character(FCData[[eval(x)]])) FCData[, eval(x) := as.character(get(x))]

            # EvalData Load ----
            if(!exists("EvalData")) EvalData <- data.table::fread(file.path(ProjectList$DataFolderPath, "EvalData.csv"))
            for(x in ProjectList$GroupVariableNames) if(!is.character(EvalData[[eval(x)]])) EvalData[, eval(x) := as.character(get(x))]

            # Combine data sets ----
            FCData <- merge(FCData, EvalData, by = c(ProjectList$DateVariableName, ProjectList$GroupVariableNames), all = FALSE)

            # Remove historical records ----
            EvalDataPass <- FCData[is.na(get(paste0("Add_", ProjectList$TS_timeSeriesTarget)))]

            # Add model name ----
            EvalDataPass[, ModelID := "H2O-CARMA"]
            EvalDataPass[, DateTime := Sys.time()]

            # Prepare data for evaluation
            temp1 <- GenerateEvaluationMetrics(
              EvalData = EvalDataPass,
              TargetName = input$TS_timeSeriesTarget,
              DateName = ProjectList$DateVariableName,
              GroupNames = ProjectList$GroupVariableNames)

            # Finalize data ----
            if(Counter == 1L) {
              FinalGrids <- temp1
              PlotData <- EvalDataPass
            } else {
              FinalGrids <- data.table::rbindlist(list(FinalGrids, temp1))
              PlotData <- data.table::rbindlist(list(PlotData, EvalDataPass))
            }
          }
        }

        # Store globally ----
        FinalGridsGlobal <<- FinalGrids
        PlotDataGlobal <<- PlotData
        data.table::setnames(PlotDataGlobal, "Predictions", "Forecast", skip_absent = TRUE)
        PlotDataGlobal <- PlotDataGlobal[, .SD, .SDcols = c(targetvariable, "Forecast", datevariable, groupvariable)]
        data.table::fwrite(PlotDataGlobal, file = file.path(ProjectList$ModelsFolderPath, "PlotData.csv"))
        data.table::fwrite(FinalGridsGlobal, file = file.path(ProjectList$ModelsFolderPath, "MetricsData.csv"))
        a <- character(0)

        # DataTable Model Metrics ----
        output$TS_ModelMetrics <- DT::renderDT({if(exists("FinalGridsGlobal")) FinalGridsGlobal}, server = TRUE, filter = "top")

        # Update Select Model Dropdown ----
        updatePickerInput(session, inputId = "TS_ModelID", label = "Select Model", choices = unique(c(ProjectList[["TS_Models"]], ProjectList[["ML_Models"]])), selected = ProjectList[["TS_ModelID"]][[1L]])
        updatePickerInput(session, inputId = "TS_ModelID2", label = "Select Model", choices = unique(c(ProjectList[["TS_Models"]], ProjectList[["ML_Models"]])), selected = ProjectList[["TS_ModelID2"]][[1L]])
        # a <- character(0)
        # if(identical(a,input$TSEval_timeSeriesGroupVars)) {
        #   updatePickerInput(session, inputId = "TSEval_timeSeriesGroupVars2", label = "Select Group Variables", choices = "", selected = "")
        # } else {
        #   updatePickerInput(session, inputId = "TSEval_timeSeriesGroupVars2", label = "Select Group Variables", choices = names(SourceData), selected = input$TSEval_timeSeriesGroupVars)
        # }
    })

    # Plot Forecasts ----
    shiny::observeEvent({
      input$TS_BuildForecastPlot}, {

        # Evaluation Mode ----
        EvalMode <- as.logical(input$Evaluate)

        # Store Plotting Args ----
        ProjectList[["TSEval_timeSeriesTarget"]] <<- input$TSEval_timeSeriesTarget
        ProjectList[["TSEval_timeSeriesTarget2"]] <<- input$TSEval_timeSeriesTarget2
        ProjectList[["TS_ModelID"]] <<- input$TS_ModelID
        ProjectList[["TSEval_timeSeriesDateColumn"]] <<- input$TSEval_timeSeriesDateColumn
        ProjectList[["TSEval_timeSeriesGroupVars"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "TSEval_timeSeriesGroupVars", Default = NULL)
        ProjectList[["TSEval_timeSeriesGroupVars2"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "TSEval_timeSeriesGroupVars2", Default = NULL)
        ProjectList[["FC_AggregateFunction"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "FC_AggregateFunction", Default = "sum")
        ProjectList[["FC_LegendTextColor"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "FC_LegendTextColor", Default = "darkblue")
        ProjectList[["FC_LegendTextSize"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "FC_LegendTextSize", Default = "blue")
        ProjectList[["FC_LineColor"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "FC_LineColor", Default = "blue")
        ProjectList[["FC_LineWidth"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "FC_LineWidth", Default = 0.50)
        ProjectList[["FC_TextSize"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "FC_TextSize", Default = 12L)
        ProjectList[["FC_NumberGroupsDisplay"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "FC_NumberGroupsDisplay", Default = 5L)
        ProjectList[["FC_TickMarksX"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "FC_TickMarksX", Default = NULL)
        ProjectList[["FC_LegendPosition"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "FC_LegendPosition", Default = "bottom")
        ProjectList[["FC_AngleX"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "FC_AngleX", Default = 35L)
        ProjectList[["FC_AngleY"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "FC_AngleY", Default = 0L)
        ProjectList[["FC_ChartColor"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "FC_ChartColor", Default = "lightsteelblue1")
        ProjectList[["FC_BorderColor"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "FC_BorderColor", Default = "darkblue")
        ProjectList[["FC_TextColor"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "FC_TextColor", Default = "darkblue")
        ProjectList[["FC_GridColor"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "FC_GridColor", Default = "white")
        ProjectList[["FC_BackGroundColor"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "FC_BackGroundColor", Default = "gray95")
        ProjectList[["FC_OtherGroups"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "FC_OtherGroups", Default = "FALSE")
        ProjectList[["FC_PredictionIntervals"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "FC_PredictionIntervals", Default = FALSE)
        ProjectList[["FC_ForecastLineColor"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "FC_ForecastLineColor", Default = "red")
        ProjectList[["FC_PredictionIntervalColorInner"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "FC_PredictionIntervalColorInner", Default = "lightsteelblue1")
        ProjectList[["FC_PredictionIntervalColorOuter"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "FC_PredictionIntervalColorOuter", Default = "lightsteelblue1")

        # Save ProjectList to File ----
        save(ProjectList, file = file.path(ProjectList[["MetaDataPath"]], "ProjectList.Rdata"))

        # Define target variable ----
        targetvariables <<- tryCatch({as.character(input$TSEval_timeSeriesTarget2)}, error = function(x) NULL)
        datevariables <<- tryCatch({as.character(input$TS_timeSeriesDateColumn)}, error = function(x) NULL)
        a <- character(0)
        if(!identical(a,as.character(input$TSEval_timeSeriesGroupVars2))) {
          groupvariables <<- tryCatch({as.character(input$TSEval_timeSeriesGroupVars2)}, error = function(x) NULL)
        } else {
          groupvariables <<- NULL
        }

        # Initiate Counter----
        Counter <- 0L

        # Load plot data ----
        if(!exists("PlotDataGlobal")) {

          print("here motherfucker")

          # Loop through models ----
          for(tsmodels in sort(x = c(ProjectList[["TS_Models"]], ProjectList[["ML_Models"]]), decreasing = FALSE)) {

            # ARIMA ----
            if(tsmodels == "Supercharged-SARIMA") {
              temp1 <- data.table::fread(file.path(ProjectList[["ModelsFolderPath"]], paste0(targetvariables,"-Arima.csv")))
              data.table::setnames(temp1, "Target", targetvariables)
              if(Counter != 0L) {
                ForecastResults <- data.table::rbindlist(list(ForecastResults,temp1), fill = TRUE)
                Counter <- Counter + 1L
              } else {
                ForecastResults <- temp1
                Counter <- Counter + 1L
              }
            }

            # NNET ----
            if(tsmodels == "Supercharged-NNET") {
              temp1 <- data.table::fread(file.path(ProjectList[["ModelsFolderPath"]], paste0(targetvariables,"-NNET.csv")))
              data.table::setnames(temp1, "Target", targetvariables)
              if(Counter != 0L) {
                ForecastResults <- data.table::rbindlist(list(ForecastResults,temp1), fill = TRUE)
                Counter <- Counter + 1L
              } else {
                ForecastResults <- temp1
                Counter <- Counter + 1L
              }
            }

            # ARFIMA ----
            if(tsmodels == "ARFIMA") {
              temp1 <- data.table::fread(file.path(ProjectList[["ModelsFolderPath"]], paste0(targetvariables,"-ARFIMA.csv")))
              data.table::setnames(temp1, "Target", targetvariables)
              if(Counter != 0L) {
                ForecastResults <- data.table::rbindlist(list(ForecastResults,temp1), fill = TRUE)
                Counter <- Counter + 1L
              } else {
                ForecastResults <- temp1
                Counter <- Counter + 1L
              }
            }

            # ETS ----
            if(tsmodels == "ETS") {
              temp1 <- data.table::fread(file.path(ProjectList[["ModelsFolderPath"]], paste0(targetvariables,"-ETS.csv")))
              data.table::setnames(temp1, "Target", targetvariables)
              if(Counter != 0L) {
                ForecastResults <- data.table::rbindlist(list(ForecastResults, temp1), fill = TRUE)
                Counter <- Counter + 1L
              } else {
                ForecastResults <- temp1
                Counter <- Counter + 1L
              }
            }

            # TBATS ----
            if(tsmodels == "TBATS") {
              temp1 <- data.table::fread(file.path(ProjectList[["ModelsFolderPath"]],paste0(targetvariables,"-TBATS.csv")))
              data.table::setnames(temp1, "Target",targetvariables)
              if(Counter != 0L) {
                ForecastResults <- data.table::rbindlist(list(ForecastResults,temp1), fill = TRUE)
                Counter <- Counter + 1L
              } else {
                ForecastResults <- temp1
                Counter <- Counter + 1L
              }
            }

            # TSLM ----
            if(tsmodels == "TSLM") {
              if(Counter != 0L) {
                if(file.exists(file.path(ProjectList[["ModelsFolderPath"]],paste0(targetvariables,"-TSLM.csv")))) {
                  temp1 <- data.table::fread(file.path(ProjectList[["ModelsFolderPath"]],paste0(targetvariables,"-TSLM.csv")))
                  data.table::setnames(temp1, "Target",targetvariables)
                  ForecastResults <- data.table::rbindlist(list(ForecastResults,temp1), fill = TRUE)
                  Counter <- Counter + 1L
                }
              } else {
                temp1 <- data.table::fread(file.path(ProjectList[["ModelsFolderPath"]], paste0(targetvariables,"-TSLM.csv")))
                data.table::setnames(temp1, "Target",targetvariables)
                ForecastResults <- temp1
                Counter <- Counter + 1L
              }
            }

            # CatBoost ----
            if(tsmodels == "CatBoost-CARMA") {
              FCData <- data.table::fread(file = file.path(ProjectList[["ModelsFolderPath"]], paste0(ProjectList[["TS_timeSeriesTarget"]], "_AutoCatBoostCARMA_Forecast.csv")))
              if(Counter != 0L) {
                PlotData <<- data.table::rbindlist(list(PlotData, FCData), fill = TRUE)
                Counter <- Counter + 1L
              } else {
                PlotData <<- FCData
                data.table::setnames(PlotData, "Predictions", "Forecast", skip_absent = TRUE)
                Counter <- Counter + 1L
              }
            }

            # XGBoost ----
            if(tsmodels == "XGBoost-CARMA") {
              FCData <- data.table::fread(file = file.path(ProjectList[["ModelsFolderPath"]], paste0(ProjectList[["TS_timeSeriesTarget"]], "_AutoXGBoostCARMA_Forecast.csv")))
              if(Counter != 0L) {
                PlotData <<- data.table::rbindlist(list(PlotData, FCData), fill = TRUE)
                Counter <- Counter + 1L
              } else {
                PlotData <<- FCData
                data.table::setnames(PlotData, "Predictions", "Forecast", skip_absent = TRUE)
                Counter <- Counter + 1L
              }
            }

            # H2O ----
            if(tsmodels == "H2O-CARMA") {
              FCData <- data.table::fread(file = file.path(ProjectList[["ModelsFolderPath"]], paste0(ProjectList[["TS_timeSeriesTarget"]], "_AutoH2OCARMA_Forecast.csv")))
              if(Counter != 0L) {
                PlotData <<- data.table::rbindlist(list(PlotData, FCData), fill = TRUE)
                Counter <- Counter + 1L
              } else {
                PlotData <<- FCData
                data.table::setnames(PlotData, "Predictions", "Forecast", skip_absent = TRUE)
                Counter <- Counter + 1L
              }
            }
          }
        }

        # Plot Options ----
        Aggregate <- RemixAutoML::ReturnParam(input, VarName = "FC_AggregateFunction", Type = "character", Default = "sum")
        Color <- RemixAutoML::ReturnParam(input, VarName = "FC_LineColor", Type = "character", Default = "red")
        LineWidth <- RemixAutoML::ReturnParam(input, VarName = "FC_LineWidth", Type = "numeric", Default = 0.50)
        TextSize <- RemixAutoML::ReturnParam(input, VarName = "FC_TextSize", Type = "numeric", Default = 18)
        NumberGroupsDisplay <- RemixAutoML::ReturnParam(input, VarName = "FC_NumberGroupsDisplay", Type = "numeric", Default = 5)
        XTickMarks <- RemixAutoML::ReturnParam(input, VarName = "FC_TickMarksX", Type = "character", Default = "1 year")
        LegendPosition <- RemixAutoML::ReturnParam(input, VarName = "FC_LegendPosition", Type = "character", Default = "right")
        AngleX <- RemixAutoML::ReturnParam(input, VarName = "FC_AngleX", Type = "numeric", Default = 35)
        AngleY <- RemixAutoML::ReturnParam(input, VarName = "FC_AngleY", Type = "numeric", Default = 0)
        ChartColor <- RemixAutoML::ReturnParam(input, VarName = "FC_ChartColor", Type = "character", Default = "lightsteelblue1")
        BorderColor <- RemixAutoML::ReturnParam(input, VarName = "FC_BorderColor", Type = "character", Default = "darkblue")
        TextColor <- RemixAutoML::ReturnParam(input, VarName = "FC_TextColor", Type = "character", Default = "darkblue")
        GridColor <- RemixAutoML::ReturnParam(input, VarName = "FC_GridColor", Type = "character", Default = "white")
        BackGroundColor <- RemixAutoML::ReturnParam(input, VarName = "FC_BackGroundColor", Type = "character", Default = "gray95")
        LegendTextColor <- RemixAutoML::ReturnParam(input, VarName = "FC_LegendTextColor", Type = "character", Default = "darkblue")
        LegendTextSize <- RemixAutoML::ReturnParam(input, VarName = "FC_LegendTextSize", Type = "numeric", Default = 10)
        FC_OtherGroups <- RemixAutoML::ReturnParam(input, VarName = "FC_OtherGroups", Type = "logical", Default = "FALSE")
        if(input$TS_ModelID %chin% c("CatBoost-CARMA","XGBoost-CARMA","H2O-CARMA")) {
          FC_PredictionIntervals <- "FALSE"
        } else {
          FC_PredictionIntervals <- RemixAutoML::ReturnParam(input, VarName = "FC_PredictionIntervals", Type = "character", Default = "FALSE")
        }
        FC_ForecastLineColor <- RemixAutoML::ReturnParam(input, VarName = "FC_ForecastLineColor", Type = "character", Default = "red")
        FC_PredictionIntervalColorInner <- RemixAutoML::ReturnParam(input, VarName = "FC_PredictionIntervalColorInner", Type = "character", Default = "lightsteelblue1")
        FC_PredictionIntervalColorOuter <- RemixAutoML::ReturnParam(input, VarName = "FC_PredictionIntervalColorOuter", Type = "character", Default = "lightsteelblue1")

        # Create data for plotting ----
        if(!is.null(groupvariables)) PlotDataGlobal[, eval(groupvariables) := lapply(.SD, as.character), .SDcols = c(groupvariables)]
        PlotDataForecastFinal <<- RemixAutoML::PreparePlotData(
          input,
          PlotDataGlobal,
          Aggregate = Aggregate,
          TargetVariable = c("Forecast", targetvariables),
          DateVariable = datevariables,
          GroupVariables = groupvariables,
          G1Levels = "TS_Group1Levels2",
          G2Levels = "TS_Group2Levels2",
          G3Levels = "TS_Group3Levels2")

        # Check for uniqueness since Evaluate mode plots forecast and actual for a single series ----
        if(EvalMode & length(unique(PlotDataForecastFinal[[eval(datevariables)]])) != PlotDataForecastFinal[,.N]) {
          if(Aggregate == "mean") Agg <- as.function(x = , mean) else if(Aggregate == "sum") Agg <- as.function(x = , sum)
          PlotDataForecastFinal <- PlotDataForecastFinal[, c("Forecast", eval(targetvariables)) := lapply(.SD, Agg), by = c(datevariables), .SDcols = c("Forecast", eval(targetvariables))][, .SD, .SDcols = c(datevariables, "Forecast", eval(targetvariables))]
          #shinyWidgets::sendSweetAlert(session, title = NULL, text = "When Evaluate is TRUE you must select a single group level for all group variables", type = NULL, btn_labels = "error", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "60%")
        }

        # Build Plot ----
        #output$TS_ForecastPlot <- plotly::renderPlotly({
        output$TS_ForecastPlot <- shiny::renderPlot({

          # Plot forecast data ----
          #TimeSeriesPlotObject <<-
          RemixAutoML::TimeSeriesPlotter(
            EvaluationMode = EvalMode,
            data = PlotDataForecastFinal,
            TargetVariable = if(EvalMode) c("Forecast", targetvariables) else targetvariables,
            DateVariable = datevariables,
            GroupVariables = if(EvalMode) NULL else groupvariables,
            VLineDate = max(SourceData[[eval(datevariables)]], na.rm = TRUE),
            Aggregate = Aggregate,
            NumberGroupsDisplay = as.numeric(NumberGroupsDisplay),
            LevelsToDisplay = NULL,
            OtherGroupLabel = "OtherGroups",
            DisplayOtherGroup = as.logical(FC_OtherGroups),
            TextSize = TextSize,
            LineWidth = LineWidth,
            Color = Color,
            XTickMarks = XTickMarks,
            AngleX = AngleX,
            AngleY = AngleY,
            ChartColor = ChartColor,
            BorderColor = BorderColor,
            TextColor = TextColor,
            GridColor = GridColor,
            BackGroundColor = BackGroundColor,
            LegendPosition = LegendPosition,
            LegendTextColor = LegendTextColor,
            LegendTextSize = LegendTextSize,
            PredictionIntervals = as.logical(FC_PredictionIntervals),
            TS_ModelID = input$TS_ModelID,
            ForecastLineColor = FC_ForecastLineColor,
            PredictionIntervalColorInner = FC_PredictionIntervalColorInner,
            PredictionIntervalColorOuter = FC_PredictionIntervalColorOuter)

          # Convert to plotly
          #plotly::ggplotly(TimeSeriesPlotObject, tooltip = NULL)
        })
    })

    # Next and Previous Buttons ----
    shiny::observeEvent(input$link_to_automated_forecasting_model_evaluation, {
      updateTabItems(session, inputId = "modelMenu", selected = "automated_forecasting_model_evaluation")
    })
    shiny::observeEvent(input$link_to_automated_forecasting_model_evaluation_1, {
      updateTabItems(session, inputId = "modelMenu", selected = "automated_forecasting_model_evaluation")
    })
})
