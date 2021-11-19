
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
options(scipen = 999)
data.table::setDTthreads(threads = max(1L, parallel::detectCores() - 2L))
data.table::getDTthreads(verbose = TRUE)
Local <- TRUE

# Server begin
server <- shiny::shinyServer(function(input, output, session) {

# Global file info ----
  volumes <<- c(Home = fs::path_home(), "R Installation" = R.home(), shinyFiles::getVolumes()())

  # . ----

  # . ----

  # . ----

  # . ----

# Home Page Links ----
  shiny::observeEvent(eventExpr = input$link_HomePage, {
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "HomePage")
  })
  shiny::observeEvent(eventExpr = input$link_HomePage_1, {
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "HomePage")
  })
  shiny::observeEvent(eventExpr = input$link_HomePage_2, {
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "HomePage")
  })
  shiny::observeEvent(eventExpr = input$link_HomePage_3, {
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "HomePage")
  })
  shiny::observeEvent(eventExpr = input$link_HomePage_4, {
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "HomePage")
  })
  shiny::observeEvent(eventExpr = input$link_HomePage_5, {
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "HomePage")
  })
  shiny::observeEvent(eventExpr = input$link_HomePage_6, {
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "HomePage")
  })
  shiny::observeEvent(eventExpr = input$link_HomePage_7, {
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "HomePage")
  })
  shiny::observeEvent(eventExpr = input$link_HomePage_8, {
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "HomePage")
  })
  shiny::observeEvent(eventExpr = input$link_HomePage_9, {
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "HomePage")
  })

  # . ----

  # . ----

  # . ----

  # . ----


# EDA SERVER CODE ----

  # Next and Previous Buttons ----
  shiny::observeEvent(eventExpr = input$link_eda_home_page, {
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "eda_home_page")
  })
  shiny::observeEvent(eventExpr = input$link_eda_home_page_1, {
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "eda_home_page")
  })
  shiny::observeEvent(eventExpr = input$link_eda_home_page_2, {
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "eda_home_page")
  })

  # Next and Previous Buttons ----
  shiny::observeEvent(eventExpr = input$link_eda_create_project, {
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "eda_create_project")
  })
  shiny::observeEvent(eventExpr = input$link_eda_create_project_1, {
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "eda_create_project")
  })
  shiny::observeEvent(eventExpr = input$link_eda_create_project_2, {
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "eda_create_project")
  })

  # Next and Previous Buttons ----
  shiny::observeEvent(eventExpr = input$link_eda_load_data, {
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "eda_load_data")
  })
  shiny::observeEvent(eventExpr = input$link_eda_load_data_1, {
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "eda_load_data")
  })
  shiny::observeEvent(eventExpr = input$link_eda_load_data_2, {
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "eda_load_data")
  })

  # Next and Previous Buttons ----
  shiny::observeEvent(eventExpr = input$link_eda_data_prep, {
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "eda_data_prep")
  })
  shiny::observeEvent(eventExpr = input$link_eda_data_prep_1, {
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "eda_data_prep")
  })
  shiny::observeEvent(eventExpr = input$link_eda_data_prep_2, {
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "eda_data_prep")
  })

  # . ----

# FEATURE ENGINEERING SERVER CODE ----

  # Next and Previous Buttons ----
  shiny::observeEvent(eventExpr = input$link_feature_engineering_home_page, {
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "feature_engineering_home_page")
  })
  shiny::observeEvent(eventExpr = input$link_feature_engineering_home_page_1, {
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "feature_engineering_home_page")
  })
  shiny::observeEvent(eventExpr = input$link_feature_engineering_home_page_2, {
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "feature_engineering_home_page")
  })

  # Next and Previous Buttons ----
  shiny::observeEvent(eventExpr = input$link_feature_engineering_load, {
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "feature_engineering_load")
  })
  shiny::observeEvent(eventExpr = input$link_feature_engineering_load_1, {
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "feature_engineering_load")
  })
  shiny::observeEvent(eventExpr = input$link_feature_engineering_load_2, {
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "feature_engineering_load")
  })

  # . ----

# MODEL OPTIMIZATION SERVER CODE ----

  # Next and Previous Buttons ----
  shiny::observeEvent(eventExpr = input$link_model_optimization_home_page, {
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "model_optimization_home_page")
  })
  shiny::observeEvent(eventExpr = input$link_model_optimization_home_page_1, {
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "model_optimization_home_page")
  })
  shiny::observeEvent(eventExpr = input$link_model_optimization_2, {
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "model_optimization_home_page")
  })

  # Next and Previous Buttons ----
  shiny::observeEvent(eventExpr = input$link_model_optimization_load, {
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "model_optimization_load")
  })
  shiny::observeEvent(eventExpr = input$link_model_optimization_load_1, {
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "model_optimization_load")
  })
  shiny::observeEvent(eventExpr = input$link_model_optimization_load_2, {
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "model_optimization_load")
  })

  # . ----

# MODEL SCORING SERVER CODE ----

  # Next and Previous Buttons ----
  shiny::observeEvent(eventExpr = input$link_model_scoring_home_page, {
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "model_scoring_home_page")
  })
  shiny::observeEvent(eventExpr = input$link_model_scoring_home_page_1, {
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "model_scoring_home_page")
  })
  shiny::observeEvent(eventExpr = input$link_model_scoring_home_page_2, {
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "model_scoring_home_page")
  })

  # Next and Previous Buttons ----
  shiny::observeEvent(eventExpr = input$link_model_scoring_load, {
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "model_scoring_output")
  })
  shiny::observeEvent(eventExpr = input$link_model_scoring_load_1, {
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "model_scoring_output")
  })
  shiny::observeEvent(eventExpr = input$link_model_scoring_load_2, {
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "model_scoring_output")
  })

  # . ----

# MODEL INSIGHTS SERVER CODE ----

  # Next and Previous Buttons ----
  shiny::observeEvent(eventExpr = input$link_model_insights_home_page, {
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "model_insights_home_page")
  })
  shiny::observeEvent(eventExpr = input$link_model_insights_home_page_1, {
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "model_insights_home_page")
  })

  # . ----

  # ModelInsights - Load Model Objects Tab: Load Model Output List ----
  RemixAutoML::observeEventLoad(input, InputVal = "ML_ModelOutputList", ObjectName = "ModelList")

  # ModelInsights - Load Model Objects Tab: Load Training Data
  ML_TrainData <<- RemixAutoML::ReactiveLoadCSV(input, InputVal = "ML_TrainData")
  ML_ShapTable <<- RemixAutoML::ReactiveLoadCSV(input, InputVal = "ML_ShapTable")

  # ModelInsights - Load Model Objects Tab: Load Model Output List ----
  RemixAutoML::observeEventLoad(input, InputVal = "ML_ArgsList", ObjectName = "ArgsList")

  # ModelInsights - Load Model Objects Tab: Load Training Data
  ML_WarehouseData <<- RemixAutoML::ReactiveLoadCSV(input, InputVal = "ML_TrainData")

  # Next and Previous Buttons ----
  shiny::observeEvent(eventExpr = input$link_model_insights_load, {
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "model_insights_load_model_output_list")
  })
  shiny::observeEvent(eventExpr = input$link_model_insights_load_1, {
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "model_insights_load_model_output_list")
  })

  # . ----

  # ModelInsights - Visualization Tab: Variables of Interest ----
  output$ModelInsights_ScoreVariable <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = "ModelInsights_ScoreVariable", Label = "Select Predicted Column", Choices = if("p1" %chin% names(ModelList$ValidationData)) "p1" else names(ModelList$ValidationData), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$ModelInsights_TargetVariable <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = "ModelInsights_TargetVariable", Label = "Select Target Column", Choices = names(ModelList$ValidationData), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$ModelInsights_DateVariable <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = "ModelInsights_DateVariable",Label = "Select Date Column", Choices = names(ModelList$ValidationData), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$ModelInsights_IndependentVariable <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = "ModelInsights_IndependentVariable",Label = "Select Independent Variable Column", Choices = c(ModelList$ColNames[[1L]]), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  # ByVariables
  output$ModelInsights_ByVariables1  <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = "ModelInsights_ByVariables1",Label = "By-Variables (1)", Choices = c("None", names(ModelList$ValidationData)), SelectedDefault = "None", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$ModelInsights_ByVariables2  <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = "ModelInsights_ByVariables2",Label = "By-Variables (2)", Choices = c("None", names(ModelList$ValidationData)), SelectedDefault = "None", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$ModelInsights_ByVariables3  <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = "ModelInsights_ByVariables3",Label = "By-Variables (3)", Choices = c("None", names(ModelList$ValidationData)), SelectedDefault = "None", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  # ByVariable levels
  output$ModelInsights_ByVariables1Levels  <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = "ModelInsights_ByVariables1Levels", Label = "Select Levels (1)", Choices = if(!is.null(ModelList$ValidationData) && !is.null(input$ModelInsights_ByVariables1)) sort(unique(as.character(ModelList$ValidationData[[eval(input$ModelInsights_ByVariables1)]]))) else NULL, SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$ModelInsights_ByVariables2Levels  <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = "ModelInsights_ByVariables2Levels", Label = "Select Levels (2)", Choices = if(!is.null(ModelList$ValidationData) && !is.null(input$ModelInsights_ByVariables2)) sort(unique(as.character(ModelList$ValidationData[[eval(input$ModelInsights_ByVariables2)]]))) else NULL, SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })
  output$ModelInsights_ByVariables3Levels  <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = "ModelInsights_ByVariables3Levels", Label = "Select Levels (3)", Choices = if(!is.null(ModelList$ValidationData) && !is.null(input$ModelInsights_ByVariables3)) sort(unique(as.character(ModelList$ValidationData[[eval(input$ModelInsights_ByVariables3)]]))) else NULL, SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  # Plot Options
  output$ModelInsights_PlotBuckets  <- shiny::renderUI({
    RemixAutoML::NumericInput(InputID = "ModelInsights_PlotBuckets", Label = "# Buckets for Plots", Min = 5, Max = 100, Step = 5, Value = 20)
  })

  # Plot Type
  output$ModelInsights_PlotOptions  <- shiny::renderUI({
    RemixAutoML::PickerInput(InputID = "ModelInsights_PlotOptions",Label = "Select Plot Option", Choices = c("EvaluationPlot", "EvaluationBoxPlot", "PartialDependencePlot", "PartialDependenceBoxPlot", "ROC_Plot", "GainsPlot", "LiftPlot", "VariableImportance", "ShapPlot"), SelectedDefault = "EvaluationPlot", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
  })

  # ModelInsights - Visualization ----

  # EvaluationPlot
  shiny::observeEvent(eventExpr = input$ML_Plot, {

    # Subset check ----
    Check <- (!is.null(input$ModelInsights_ByVariables1) || !is.null(input$ModelInsights_ByVariables2) || !is.null(input$ModelInsights_ByVariables3)) &&
      (!is.null(input$ModelInsights_ByVariables1Levels) || !is.null(input$ModelInsights_ByVariables2Levels) || !is.null(input$ModelInsights_ByVariables3Levels))

    # PlotBuckets check
    Check2 <- input$ModelInsights_PlotBuckets == 20

    # SubsetData
    if(Check) {

      # All
      if((!is.null(input$ModelInsights_ByVariables1) && !is.null(input$ModelInsights_ByVariables1Levels)) &&
         (!is.null(input$ModelInsights_ByVariables2) && !is.null(input$ModelInsights_ByVariables2Levels)) &&
         (!is.null(input$ModelInsights_ByVariables3) && !is.null(input$ModelInsights_ByVariables3Levels))) {
        temp <- ModelList$ValidationData[
          get(input$ModelInsights_ByVariables1) %in% c(eval(input$ModelInsights_ByVariables1Levels)) &
            get(input$ModelInsights_ByVariables2) %in% c(eval(input$ModelInsights_ByVariables2Levels)) &
            get(input$ModelInsights_ByVariables3) %in% c(eval(input$ModelInsights_ByVariables3Levels))]
      }

      # 2V1
      if((!is.null(input$ModelInsights_ByVariables1) && !is.null(input$ModelInsights_ByVariables1Levels)) &&
         is.null(input$ModelInsights_ByVariables2) &&
         (!is.null(input$ModelInsights_ByVariables3) && !is.null(input$ModelInsights_ByVariables3Levels))) {
        temp <- ModelList$ValidationData[
          get(input$ModelInsights_ByVariables1) %in% c(eval(input$ModelInsights_ByVariables1Levels)) &
            get(input$ModelInsights_ByVariables3) %in% c(eval(input$ModelInsights_ByVariables3Levels))]
      }

      # 2V2
      if((!is.null(input$ModelInsights_ByVariables1) && !is.null(input$ModelInsights_ByVariables1Levels)) &&
         (!is.null(input$ModelInsights_ByVariables2) && !is.null(input$ModelInsights_ByVariables2Levels)) &&
         is.null(input$ModelInsights_ByVariables3)) {
        temp <- ModelList$ValidationData[
          get(input$ModelInsights_ByVariables1) %in% c(eval(input$ModelInsights_ByVariables1Levels)) &
            get(input$ModelInsights_ByVariables2) %in% c(eval(input$ModelInsights_ByVariables2Levels))]
      }

      # 2V3
      if(is.null(input$ModelInsights_ByVariables1) &&
         (!is.null(input$ModelInsights_ByVariables2) && !is.null(input$ModelInsights_ByVariables2Levels)) &&
         (!is.null(input$ModelInsights_ByVariables3) && !is.null(input$ModelInsights_ByVariables3Levels))) {
        temp <- ModelList$ValidationData[
          get(input$ModelInsights_ByVariables2) %in% c(eval(input$ModelInsights_ByVariables2Levels)) &
            get(input$ModelInsights_ByVariables3) %in% c(eval(input$ModelInsights_ByVariables3Levels))]
      }

      # 1V1
      if((!is.null(input$ModelInsights_ByVariables1) && !is.null(input$ModelInsights_ByVariables1Levels)) &&
         is.null(input$ModelInsights_ByVariables2) && is.null(input$ModelInsights_ByVariables3)) {
        temp <- ModelList$ValidationData[get(eval(input$ModelInsights_ByVariables1)) %in% c(eval(input$ModelInsights_ByVariables1Levels))]
      }

      # 1V2
      if(is.null(input$ModelInsights_ByVariables1) && is.null(input$ModelInsights_ByVariables3) &&
         (!is.null(input$ModelInsights_ByVariables2) && !is.null(input$ModelInsights_ByVariables2Levels))) {
        temp <- ModelList$ValidationData[get(eval(input$ModelInsights_ByVariables2)) %in% c(eval(input$ModelInsights_ByVariables2Levels))]
      }

      # 1V3
      if(is.null(input$ModelInsights_ByVariables1) && is.null(input$ModelInsights_ByVariables2) &&
         (!is.null(input$ModelInsights_ByVariables3) && !is.null(input$ModelInsights_ByVariables3Levels))) {
        temp <- ModelList$ValidationData[get(eval(input$ModelInsights_ByVariables3)) %in% c(eval(input$ModelInsights_ByVariables3Levels))]
      }

      # 3V1
      if(is.null(input$ModelInsights_ByVariables1) && is.null(input$ModelInsights_ByVariables2) && is.null(input$ModelInsights_ByVariables3)) {
        temp <- ModelList$ValidationData
      }
    } else {
      temp <- ModelList$ValidationData
    }

    # Evaluation Plot ----
    if(any(input$ModelInsights_PlotOptions %chin% "EvaluationPlot")) {
      if(!Check && Check2 && !is.null(ModelList$EvaluationPlot)) {
        output$ML_OutputPlot <- shiny::renderPlot({
          ModelList$EvaluationPlot
        })
      } else {
        output$ML_OutputPlot <- shiny::renderPlot({
          RemixAutoML::EvalPlot(
            data = temp,
            PredictionColName = input$ModelInsights_ScoreVariable,
            TargetColName = input$ModelInsights_TargetVariable,
            GraphType = "calibration",
            PercentileBucket = 1/input$ModelInsights_PlotBuckets,
            aggrfun = function(x) mean(x, na.rm = TRUE))
        })
      }
    }

    # Evaluation BoxPlot ----
    if(any(input$ModelInsights_PlotOptions %chin% "EvaluationBoxPlot")) {
      if(!Check && Check2 && !is.null(ModelList$EvaluationBoxPlot)) {
        output$ML_OutputPlot <- shiny::renderPlot({
          ModelList$EvaluationBoxPlot
        })
      } else {
        output$ML_OutputPlot <- shiny::renderPlot({
          RemixAutoML::EvalPlot(
            data = temp,
            PredictionColName = input$ModelInsights_ScoreVariable,
            TargetColName = input$ModelInsights_TargetVariable,
            GraphType = "boxplot",
            PercentileBucket = 1/input$ModelInsights_PlotBuckets, aggrfun = function(x) mean(x, na.rm = TRUE))
        })
      }
    }

    # ROC Plot ----
    if(any(input$ModelInsights_PlotOptions %chin% "ROC_Plot")) {
      if(!Check && !is.null(ModelList$ROC_Plot)) {
        output$ML_OutputPlot <- shiny::renderPlot({
          ModelList$ROC_Plot
        })
      } else {
        output$ML_OutputPlot <- shiny::renderPlot({
          RemixAutoML::ROCPlot(
            data = temp,
            TargetName = input$ModelInsights_TargetVariable,
            SavePlot = FALSE,
            Name = NULL,
            metapath = NULL,
            modelpath = NULL)
        })
      }
    }

    # Gains Plot ----
    if(any(input$ModelInsights_PlotOptions %chin% "GainsPlot")) {
      if(!Check && !is.null(ModelList$GainsPlot)) {
        output$ML_OutputPlot <- shiny::renderPlot({
          ModelList$GainsPlot
        })
      } else {
        output$ML_OutputPlot <- shiny::renderPlot({
          RemixAutoML::CumGainsChart(
            data = temp,
            TargetColumnName = input$ModelInsights_TargetVariable,
            PredictedColumnName = input$ModelInsights_ScoreVariable,
            SavePlot = FALSE,
            Name = NULL,
            metapath = NULL,
            modelpath = NULL)$GainsPlot
        })
      }
    }

    # Lift Plot ----
    if(any(input$ModelInsights_PlotOptions %chin% "LiftPlot")) {
      if(!Check && !is.null(ModelList$LiftPlot)) {
        output$ML_OutputPlot <- shiny::renderPlot({
          ModelList$LiftPlot
        })
      } else {
        output$ML_OutputPlot <- shiny::renderPlot({
          RemixAutoML::CumGainsChart(
            data = temp,
            TargetColumnName = input$ModelInsights_TargetVariable,
            PredictedColumnName = input$ModelInsights_ScoreVariable,
            SavePlot = FALSE,
            Name = NULL,
            metapath = NULL,
            modelpath = NULL)$LiftPlot
        })
      }
    }

    # Variable Importance Plot ----
    if(any(input$ModelInsights_PlotOptions %chin% "VariableImportance")) {
      output$ML_OutputPlot <- shiny::renderPlot({
        RemixAutoML:::VI_Plot(Type = "catboost", VI_Data = ModelList$VariableImportance, TopN = 25)
      })
    }

    # Partial Dependence Plot ----
    if(any(input$ModelInsights_PlotOptions %chin% "PartialDependencePlot") && !is.null(input$ModelInsights_IndependentVariable)) {
      if(!Check && Check2 && !is.null(ModelList$PartialDependencePlots[[eval(input$ModelInsights_IndependentVariable)]])) {
        output$ML_OutputPlot <- shiny::renderPlot({
          ModelList$PartialDependencePlots[[eval(input$ModelInsights_IndependentVariable)]]
        })
      } else {
        print(temp)
        output$ML_OutputPlot <- shiny::renderPlot({
          RemixAutoML::ParDepCalPlots(
            data = temp,
            PredictionColName = input$ModelInsights_ScoreVariable,
            TargetColName = input$ModelInsights_TargetVariable,
            IndepVar = input$ModelInsights_IndependentVariable,
            GraphType = "calibration",
            PercentileBucket = 1 / input$ModelInsights_PlotBuckets,
            FactLevels = 10,
            Function = function(x) mean(x, na.rm = TRUE))
        })
      }
    }

    # Partial Dependence Box Plot ----
    if(any(input$ModelInsights_PlotOptions %chin% "PartialDependenceBoxPlot") && !is.null(input$ModelInsights_IndependentVariable)) {
      if(!Check && Check2 && !is.null(ModelList$PartialDependenceBoxPlots[[eval(input$ModelInsights_IndependentVariable)]])) {
        output$ML_OutputPlot <- shiny::renderPlot({
          ModelList$PartialDependenceBoxPlots[[eval(input$ModelInsights_IndependentVariable)]]
        })
      } else {
        output$ML_OutputPlot <- shiny::renderPlot({
          RemixAutoML::ParDepCalPlots(
            data = temp,
            PredictionColName = input$ModelInsights_ScoreVariable,
            TargetColName = input$ModelInsights_TargetVariable,
            IndepVar = input$ModelInsights_IndependentVariable,
            GraphType = "boxplot",
            PercentileBucket = 1 / input$ModelInsights_PlotBuckets,
            FactLevels = 10,
            Function = function(x) mean(x, na.rm = TRUE))
        })
      }
    }

    # Shap Table Variable Importance ----
    if(any(input$ModelInsights_PlotOptions %chin% "ShapPlot")) {
      if(!Check && data.table::is.data.table(ML_ShapTable)) {
        ML_ShapTable2 <- ML_ShapTable[, list(Importance = mean(ShapValue, na.rm = TRUE)), by = "Variable"]
        output$ML_OutputPlot <- shiny::renderPlot({
          RemixAutoML:::VI_Plot(Type = "catboost", VI_Data = ML_ShapTable2, TopN = 25)
        })
      } else {
        ML_ShapTable1 <- RemixAutoML::AutoShapeShap(ScoringData = temp, Threads = parallel::detectCores(), DateColumnName = input$ModelInsights_DateVariable, ByVariableName = NULL)
        ML_ShapTable2 <- ML_ShapTable1[, list(Importance = mean(ShapValue, na.rm = TRUE)), by = "Variable"]
        output$ML_OutputPlot <- shiny::renderPlot({
          RemixAutoML:::VI_Plot(Type = "catboost", VI_Data = ML_ShapTable2, TopN = 25)
        })
      }
    }
  })

  # Next and Previous Buttons ----
  shiny::observeEvent(eventExpr = input$link_model_insights_visualize, {
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "model_insights_visualize")
  })
  shiny::observeEvent(eventExpr = input$link_model_insights_visualize_1, {
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "model_insights_visualize")
  })

  # . ----

  # Model Insights - Tables ----


  # Next and Previous Buttons ----
  shiny::observeEvent(eventExpr = input$link_model_insights_tables, {
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "model_insights_tables")
  })
  shiny::observeEvent(eventExpr = input$link_model_insights_tables_1, {
    shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "model_insights_tables")
  })

  # . ----

  # . ----

  # . ----

  # . ----

# PANEL FORECASTING SERVER CODE ----

  # . ----

  # Panel Forecasting CREATE OR OPEN PROJECT ----

    # Name the project ----
    output$TS_NewProjectName <- shiny::renderUI({
      shiny::textInput(inputId = "TS_NewProjectName", label = "Provide a Project Name", value = "", width = "100%", placeholder = "Project_01")
    })

    # Store Project Root Folder Path ----
    output$TS_Root_Folder <- shiny::renderUI({
      shiny::textInput(inputId = "TS_Root_Folder", label = "Supply Path File to Root Directory Folder Where Project Folders will be Created", value = "", width = "100%", placeholder = "File Path Here")
    })

    # File Path to Open Project ----
    output$TS_ProjectListUpload <- shiny::renderUI({
      shiny::textInput(inputId = "TS_ProjectListUpload", label = "Supply Path File to Project Directory Folder Where Project Files are Stored", value = "", width = "100%", placeholder = "File Path Here")
    })

    # Create Project and ProjectList[[]] ----
    shiny::observeEvent(eventExpr = input$TS_CreateProject, {

      # Define Root Path
      RootPath <<- shinyFiles::parseDirPath(roots = volumes, selection = input$folder1)

      # Check if input is valid ----
      if(input$TS_NewProjectName != "" && length(RootPath) > 0) {

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
          ProjectList <<- tryCatch({RemixAutoML:::CreateProjectFolders(ProjectName = input$TS_NewProjectName, RootPath = RootPath, ExistsButNoProjectList = FALSE, Local = Local)}, error = function(x) NULL)

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

    # Open Project Button to select folder ----
    shinyFiles::shinyDirChoose(input, 'folder', roots = volumes)

    # Create Project Button to select folder ----
    shinyFiles::shinyDirChoose(input, 'folder1', roots = volumes)

    # Open Project ----
    shiny::observeEvent(eventExpr = input$TS_OpenProject, {

      # Define Root Path
      RootPath <<- shinyFiles::parseDirPath(roots = volumes, selection = input$folder)

      # Check if input is valid ----
      if(length(RootPath) > 0 && RootPath != "") {

        # Load Project R List ----
        if(dir.exists(paths = file.path(normalizePath(RootPath), "MetaData"))) {

          # Check if ProjectList exists ----
          if(file.exists(normalizePath(file.path(RootPath, "MetaData", "ProjectList.Rdata")))) {

            # Load project list into global environment
            #ProjectList <<-
            load(file = normalizePath(file.path(RootPath, "MetaData", "ProjectList.Rdata")), env = .GlobalEnv)

            # String to communicate with user
            Message <- paste0("ProjectList is loaded")

            # Load SourceData data into project
            if(file.exists(file.path(RootPath, "Data", "SourceData.csv"))) {
              SourceData <<- data.table::fread(file = file.path(RootPath, "Data", "SourceData.csv"))
              Message <- c(Message, paste0(" TrainData is loaded"))
            } else {
              Message <- c(Message, paste0(" TrainData not found"))
            }

            # Load XREGS data into project
            if(file.exists(file.path(RootPath, "Data", "XREGS.csv"))) {
              xregs <<- data.table::fread(file = file.path(RootPath, "Data", "XREGS.csv"))
              Message <- c(Message, paste0(" XREGS is loaded"))
            } else {
              Message <- c(Message, paste0(" XREGS data not found"))
            }

            # Load XREGS data into project
            if(file.exists(file.path(RootPath, "Data", "EvalData.csv"))) {
              EvalData <<- data.table::fread(file = file.path(RootPath, "Data", "EvalData.csv"))
              Message <- c(Message, paste0(" EvalData is loaded"))
            } else {
              Message <- c(Message, paste0(" EvalData Not Found"))
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
      shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "panel_forecasting_project_creation")
    })
    shiny::observeEvent(eventExpr = input$link_to_automated_timeseries_project_creation_1, {
      shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "panel_forecasting_project_creation")
    })

  # . ----

  # Panel Forecasting - DATA IMPORT ----

    # New Project: Load Train Data ----
    TimeSeriesData <- RemixAutoML::ReactiveLoadCSV(input, InputVal = "TimeSeriesData", ProjectList = ProjectList, DateUpdateName = "SourceDataCreateDate", RemoveObjects = c("SourceData","TimeSeriesFillCheck"))
    XREGS <- RemixAutoML::ReactiveLoadCSV(input, InputVal = "XREGS", ProjectList = ProjectList, DateUpdateName = "SourceXREGSCreateDate", RemoveObjects = c("XREGS","TimeSeriesFillCheck1"))
    Eval <- RemixAutoML::ReactiveLoadCSV(input, InputVal = "Eval", ProjectList = ProjectList, DateUpdateName = "SourceEvalCreateDate", RemoveObjects = c("EvalData","TimeSeriesFillCheck2"))

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
        save(x = ProjectList, file = file.path(ProjectList[["MetaDataPath"]], "ProjectList.Rdata"))

        # Notification ----
        shinyWidgets::sendSweetAlert(session, title = NULL, text = "Data successfully transferred and inputs saved!", type = "success", btn_labels = "Ok", btn_colors = "green", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "60%")

      } else {

        # Notification that project isn't open ----
        shinyWidgets::sendSweetAlert(session, title = NULL, text = "A Project Needs to Be Created or Loaded to Utilize This Feature", type = "error", btn_labels = "Ok", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "60%")
      }
    })

    # Next and Previous Buttons ----
    shiny::observeEvent(input$link_to_panel_forecasting_data_import, {
      shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "panel_forecasting_data_import")
    })
    shiny::observeEvent(input$link_to_panel_forecasting_data_import_1, {
      shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "panel_forecasting_data_import")
    })

  # . ----

  # Panel Forecasting - DATA ANALYSIS ----

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
      RemixAutoML::PickerInput(InputID = "timeSeriesTarget_XREGS", Label = "Select Numeric Independent Variables", Choices = if(exists("ProjectList") && exists("xregs")) names(xregs) else NULL, SelectedDefault = if(exists("ProjectList") && exists("xregs")) ProjectList[["timeSeriesTarget_XREGS"]] else NULL, Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$timeSeriesDateColumn_XREGS <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "timeSeriesDateColumn_XREGS",Label = "Select Date Variable", Choices = if(exists("ProjectList") && exists("xregs")) names(xregs) else NULL, SelectedDefault = if(exists("ProjectList") && exists("xregs")) ProjectList[["timeSeriesDateColumn_XREGS"]] else NULL, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$timeSeriesGroupVars_XREGS  <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "timeSeriesGroupVars_XREGS",Label = "Select Group Variables", Choices = if(exists("ProjectList") && exists("xregs")) names(xregs) else NULL, SelectedDefault = if(exists("ProjectList") && exists("xregs")) ProjectList[["timeSeriesGroupVars_XREGS"]] else NULL, Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_Group1Levels_XREGS <- shiny::renderUI({
      RemixAutoML::PickerInput_GetLevels(input, NumGroupVar = 1L, InputID = "TS_Group1Levels_XREGS", InputID2 = "timeSeriesGroupVars_XREGS", Choices = if(exists("ProjectList") && exists("xregs")) c(as.character(unique(xregs[[eval(input$timeSeriesGroupVars_XREGS[[1L]])]]))) else NULL, SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_Group2Levels_XREGS <- shiny::renderUI({
      RemixAutoML::PickerInput_GetLevels(input, NumGroupVar = 2L, InputID = "TS_Group2Levels_XREGS", InputID2 = "timeSeriesGroupVars_XREGS", Choices = if(exists("ProjectList") && exists("xregs")) c(as.character(unique(xregs[[eval(input$timeSeriesGroupVars_XREGS[[2L]])]]))) else NULL, SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_Group3Levels_XREGS <- shiny::renderUI({
      RemixAutoML::PickerInput_GetLevels(input, NumGroupVar = 3L, InputID = "TS_Group3Levels_XREGS", InputID2 = "timeSeriesGroupVars_XREGS", Choices = if(exists("ProjectList") && exists("xregs")) c(as.character(unique(xregs[[eval(input$timeSeriesGroupVars_XREGS[[3L]])]]))) else NULL, SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })

    # Eval Data Management ----
    output$timeSeriesTarget_Eval <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "timeSeriesTarget_Eval", Label = "Select Target Variable", Choices = if(exists("ProjectList") && exists("EvalData")) names(EvalData) else NULL, SelectedDefault = if(exists("ProjectList") && exists("EvalData")) ProjectList[["timeSeriesTarget_Eval"]] else NULL, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$timeSeriesDateColumn_Eval <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "timeSeriesDateColumn_Eval",Label = "Select Date Variable", Choices = if(exists("ProjectList") && exists("EvalData")) names(EvalData) else NULL, SelectedDefault = if(exists("ProjectList") && exists("EvalData")) ProjectList[["timeSeriesDateColumn_Eval"]] else NULL,Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$timeSeriesGroupVars_Eval  <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "timeSeriesGroupVars_Eval",Label = "Select Group Variables", Choices = if(exists("ProjectList") && exists("EvalData")) names(EvalData) else NULL, SelectedDefault = if(exists("ProjectList") && exists("EvalData")) ProjectList[["timeSeriesGroupVars_Eval"]] else NULL, Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_Group1Levels_Eval <- shiny::renderUI({
      RemixAutoML::PickerInput_GetLevels(input, NumGroupVar = 1L, InputID = "TS_Group1Levels_Eval", InputID2 = "timeSeriesGroupVars_Eval", Choices = if(exists("ProjectList") && exists("EvalData")) c(as.character(unique(EvalData[[eval(input$timeSeriesGroupVars_XREGS[[1L]])]]))) else NULL, SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_Group2Levels_Eval <- shiny::renderUI({
      RemixAutoML::PickerInput_GetLevels(input, NumGroupVar = 2L, InputID = "TS_Group2Levels_Eval", InputID2 = "timeSeriesGroupVars_Eval", Choices = if(exists("ProjectList") && exists("EvalData")) c(as.character(unique(EvalData[[eval(input$timeSeriesGroupVars_XREGS[[2L]])]]))) else NULL, SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_Group3Levels_Eval <- shiny::renderUI({
      RemixAutoML::PickerInput_GetLevels(input, NumGroupVar = 3L, InputID = "TS_Group3Levels_Eval", InputID2 = "timeSeriesGroupVars_Eval", Choices = if(exists("ProjectList") && exists("EvalData")) c(as.character(unique(EvalData[[eval(input$timeSeriesGroupVars_XREGS[[3L]])]]))) else NULL, SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
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
        shinyWidgets::updatePickerInput(session = session, inputId = "TS_AggregateFunction", label = "Aggregation method", choices = c("mean","sum"), selected = "mean")
        ProjectList[["TS_AggregateFunction"]] <<- input$TS_AggregateFunction

        # TS_LegendTextColor
        shinyWidgets::updatePickerInput(session = session, inputId = "TS_LegendTextColor", label = "Legend text color", choices = grDevices::colors(), selected = "darkblue")
        ProjectList[["TS_LegendTextColor"]] <<- input$TS_LegendTextColor

        # TS_LegendTextSize
        shiny::updateNumericInput(session = session, inputId = "TS_LegendTextSize", label = "Legend text size", value = 10, min = 1, max = 48, step = 2)
        ProjectList[["TS_LegendTextSize"]] <<- input$TS_LegendTextSize

        # Line Color
        shinyWidgets::updatePickerInput(session = session, inputId = "TS_LineColor", label = "Line Color", choices = grDevices::colors(), selected = "blue")
        ProjectList[["TS_LineColor"]] <<- input$TS_LineColor

        # TS_LineWidth
        shiny::updateNumericInput(session = session, inputId = "TS_LineWidth", label = "Line size", value = 0.5, min = 0.10, max = 5, step = 0.10)
        ProjectList[["TS_LineWidth"]] <<- input$TS_LineWidth

        # TS_TextSize
        shiny::updateNumericInput(session = session, inputId = "TS_TextSize", label = "Text size", value = 12, min = 1, max = 50, step = 1)
        ProjectList[["TS_TextSize"]] <<- input$TS_TextSize

        # TS_NumberGroupsDisplay
        shiny::updateNumericInput(session = session, inputId = "TS_NumberGroupsDisplay", label = "Lines to display", value = 5, min = 1, max = 50, step = 1)
        ProjectList[["TS_NumberGroupsDisplay"]] <<- input$TS_NumberGroupsDisplay

        # TickMarksX
        shinyWidgets::updatePickerInput(session = session, inputId = "TickMarksX", label = "Tick marks X-Axis",
                          choices = c("1 year","1 day","3 day","1 week","2 week","1 month","3 month","6 month","2 year","5 year","10 year","1 minute","15 minutes","30 minutes","1 hour","3 hour","6 hour","12 hour"),
                          selected = "1 year")
        ProjectList[["TickMarksX"]] <<- input$TickMarksX

        # TS_LegendPosition
        shinyWidgets::updatePickerInput(session = session, inputId = "TS_LegendPosition", label = "Legend position", choices = c("bottom","top","left","right"), selected = "bottom")
        ProjectList[["TS_LegendPosition"]] <<- input$TS_LegendPosition

        # TS_AngleX
        shiny::updateNumericInput(session = session, inputId = "TS_AngleX", label = "X-axis text angle", value = 35, min = 0, max = 360, step = 5)
        ProjectList[["TS_AngleX"]] <<- input$TS_AngleX

        # Y Axis angle
        shiny::updateNumericInput(session = session, inputId = "TS_AngleY", label = "X-axis text angle", value = 0, min = 0, max = 360, step = 5)
        ProjectList[["TS_AngleY"]] <<- input$TS_AngleY

        # TS_ChartColor
        shinyWidgets::updatePickerInput(session = session, inputId = "TS_ChartColor", label = "Chart color", choices = grDevices::colors(), selected = "lightsteelblue1")
        ProjectList[["TS_ChartColor"]] <<- input$TS_ChartColor

        # TS_BorderColor
        shinyWidgets::updatePickerInput(session = session, inputId = "TS_BorderColor", label = "Border color", choices = grDevices::colors(), selected = "darkblue")
        ProjectList[["TS_BorderColor"]] <<- input$TS_BorderColor

        # TS_TextColor
        shinyWidgets::updatePickerInput(session = session, inputId = "TS_TextColor", label = "Text color", choices = grDevices::colors(), selected = "darkblue")
        ProjectList[["TS_TextColor"]] <<- input$TS_TextColor

        # TS_GridColor
        shinyWidgets::updatePickerInput(session = session, inputId = "TS_GridColor", label = "Grid lines color", choices = grDevices::colors(), selected = "white")
        ProjectList[["TS_GridColor"]] <<- input$TS_GridColor

        # TS_BackGroundColor
        shinyWidgets::updatePickerInput(session = session, inputId = "TS_BackGroundColor", label = "Background color", choices = grDevices::colors(), selected = "gray95")
        ProjectList[["TS_BackGroundColor"]] <<- input$TS_BackGroundColor

        # TS_OtherGroups
        shinyWidgets::updatePickerInput(session = session, inputId = "TS_OtherGroups", label = "Show Other Groups", choices = c("FALSE","TRUE"), selected = "FALSE")
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
        shiny::showNotification(ui = "Plot settings are reset!", closeButton = TRUE, duration = 2, session = session, type = "message")

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
            if(!is.null(input$timeSeriesGroupVars)) {
              SourceData <<- RemixAutoML::TimeSeriesFill(data = SourceData, DateColumnName = input$timeSeriesDateColumn, GroupVariables = input$timeSeriesGroupVars, TimeUnit = input$timeSeriesTimeUnit, FillType = "maxmax", MaxMissingPercent = 0.10, SimpleImpute = FALSE)
              SourceData <<- RemixAutoML::ModelDataPrep(data = SourceData, Impute = TRUE, CharToFactor = FALSE, FactorToChar = FALSE, IntToNumeric = FALSE, LogicalToBinary = FALSE, DateToChar = FALSE, RemoveDates = FALSE, MissFactor = "0", MissNum = 0.0, IgnoreCols = NULL)
              TimeSeriesFillCheck <<- TRUE
              output$TS_DataRowSize <- shiny::renderUI({RemixAutoML::NumericInput(InputID = "TS_DataRowSize", Label = "Rows in TrainData", Min = 1, Max = 2000000000, Value = SourceData[,.N], Step = 1)})
              output$TS_DataColSize <- shiny::renderUI({RemixAutoML::NumericInput(InputID = "TS_DataColSize", Label = "Cols in TrainData", Min = 1, Max = 2000000000, Value = ncol(SourceData), Step = 1)})

              # New number of rows
              newrows <- SourceData[, .N]
            }

            # Notify user of error ----
            if(!is.null(input$timeSeriesGroupVars)) {
              shinyWidgets::sendSweetAlert(session, title = NULL, text = paste0("Training data has been prepared: Old rows = ",oldrows, " :: new rows = ",newrows), type = NULL, btn_labels = "success", btn_colors = "green", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "60%")
            } else {
              shinyWidgets::sendSweetAlert(session, title = NULL, text = paste0("Training data has been prepared"), type = NULL, btn_labels = "success", btn_colors = "green", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "60%")
            }
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
            output$TS_DataRowSize_XREGS <- shiny::renderUI({RemixAutoML::NumericInput(InputID = "TS_DataRowSize_XREGS", Label = "Rows in XREGS", Min = 1, Max = 2000000000, Value = xregs[,.N], Step = 1)})
            output$TS_DataColSize_XREGS <- shiny::renderUI({RemixAutoML::NumericInput(InputID = "TS_DataColSize_XREGS", Label = "Cols in XREGS", Min = 1, Max = 2000000000, Value = ncol(xregs), Step = 1)})

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
            output$TS_DataRowSize_Eval <- shiny::renderUI({RemixAutoML::NumericInput(InputID = "TS_DataRowSize_Eval", Label = "Rows in EvalData", Min = 1, Max = 2000000000, Value = EvalData[,.N], Step = 1)})
            output$TS_DataColSize_Eval <- shiny::renderUI({RemixAutoML::NumericInput(InputID = "TS_DataColSize_Eval", Label = "Cols in EvalData", Min = 1, Max = 2000000000, Value = ncol(EvalData), Step = 1)})

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
          if(!is.null(TargetVariable) & !is.null(DateVariable)) {

            # Plot ----
            output$TimeSeriesPlot <- plotly::renderPlotly({
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
            })

          } else {
            shinyWidgets::sendSweetAlert(session, title = NULL, text = "You need to have a Target Variable and Date Variable selected", type = "error", btn_labels = "Ok", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "60%")
          }
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
    shiny::observeEvent(input$link_to_panel_forecasting_eda, {
      shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "panel_forecasting_eda")
    })
    shiny::observeEvent(input$link_to_panel_forecasting_eda_1, {
      shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "panel_forecasting_eda")
    })

  # . ----

  # Panel Forecasting - MODELING ------

    # Model Selection Tab ----
    output$TS_DataRowSize <- shiny::renderUI({RemixAutoML::NumericInput(InputID = "TS_DataRowSize", Label = "Number of Rows in Data", Min = 1, Max = 2000000000, Value = if(exists("SourceData")) SourceData[,.N] else NA, Step = 1)})
    output$TS_SimpleGrid <- shiny::renderUI({RemixAutoML::PickerInput(InputID = "TS_SimpleGrid", Label = "Tree-Based Basic Grid Tune", Choices = c("TRUE","FALSE"), SelectedDefault = "FALSE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)})
    output$TSMLModelsSelection <- shiny::renderUI({RemixAutoML::PickerInput(InputID = "TSMLModelsSelection", Label = "Supercharged ML Models", Choices = c("CatBoost-CARMA","XGBoost-CARMA","H2O-CARMA"), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)})
    output$H2OModelSelection <- shiny::renderUI({RemixAutoML::PickerInput(InputID = "H2OModelSelection", Label = "H2O Algo Selection", Choices = c("Generalized Linear Model","Generalized Additive Model","Gradient Boosting Machine","Distributed Random Forest","AutoML"), SelectedDefault = "Distributed Random Forecast", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)})
    output$TS_CARMA_PDFOutputPath <- shiny::renderUI({RemixAutoML::PickerInput(InputID = "TS_CARMA_PDFOutputPath", Label = "Save Model Insights to PDF", Choices = c("TRUE","FALSE"),SelectedDefault = "FALSE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)})
    output$TS_CARMA_SaveDataPath <- shiny::renderUI({RemixAutoML::PickerInput(InputID = "TS_CARMA_SaveDataPath", Label = "Save CARMA Model Training Data", Choices = c("TRUE","FALSE"),SelectedDefault = "FALSE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)})
    output$TS_CARMA_NumParDepPlots <- shiny::renderUI({RemixAutoML::NumericInput(InputID="TS_CARMA_NumParDepPlots",Label="Number of Partial Dependence Plots to Save", Step = 1, Value = 25, Min = 1, Max = 10000)})

    # Data args ----
    output$TS_timeSeriesTarget <- shiny::renderUI({RemixAutoML::PickerInput(InputID = "TS_timeSeriesTarget", Label = "Select Target Variable", Choices = names(SourceData), SelectedDefault = ProjectList[["TargetVariableName"]], Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)})
    output$TS_timeSeriesDateColumn <- shiny::renderUI({RemixAutoML::PickerInput(InputID = "TS_timeSeriesDateColumn", Label = "Select Date Variable", Choices = names(SourceData), SelectedDefault = ProjectList[["DateVariableName"]][[1L]], Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)})
    output$TS_timeSeriesGroupVars <- shiny::renderUI({RemixAutoML::PickerInput(InputID = "TS_timeSeriesGroupVars", Label = "Select Group Variables", Choices = names(SourceData), SelectedDefault = if(!is.null(ProjectList[["GroupVariableNames"]])) ProjectList[["GroupVariableNames"]] else if(class(input$timeSeriesGroupVars) == "NULL") input$timeSeriesGroupVars, Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)})
    output$TS_TimeUnit <- shiny::renderUI({
      if(exists("ProjectList")) {
        tryCatch({
          if(!is.null(ProjectList[["TS_TimeUnit"]])) {
            shinyWidgets::pickerInput(inputId = "TS_TimeUnit", label = "Periodicity of Data", choices = c("1-Minute","5-Minutes","10-Minutes","15-Minutes","30-Minutes","Hourly","Daily","Weekly","Monthly","Quarterly","Yearly"),
                        selected = ProjectList[["TS_TimeUnit"]][[1L]], options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 1"), multiple = TRUE)
          } else if(exists("SourceData")) {

            # Sort by group and take time difference for the TimeUnit UI element
            if(!is.null(ProjectList[["timeSeriesGroupVars"]])) {
              OrderVector <- c()
              for(zz in seq_len(length(ProjectList[["timeSeriesGroupVars"]]) + 1L)) OrderVector <- c(OrderVector, 1)
              data.table::setorderv(x = SourceData, cols = c(eval(ProjectList[["timeSeriesGroupVars"]]), eval(ProjectList[["DateVariableName"]])), order = OrderVector)
              xx <- abs(as.numeric(difftime(SourceData[2L, as.POSIXct(get(ProjectList[["DateVariableName"]]))], SourceData[1L, as.POSIXct(get(ProjectList[["DateVariableName"]]))], units = "mins")/60/24))
            } else {
              xx <- abs(as.numeric(difftime(SourceData[2L, as.POSIXct(get(ProjectList[["DateVariableName"]]))], SourceData[1L, as.POSIXct(get(ProjectList[["DateVariableName"]]))], units = "mins")/60/24))
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
            shinyWidgets::pickerInput(inputId = "TS_TimeUnit", label = "Periodicity of Data", choices = c("1-Minute","5-Minutes","10-Minutes","15-Minutes","30-Minutes","Hourly","Daily","Weekly","Monthly","Quarterly","Yearly"), selected = tunit, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 1"), multiple = TRUE)
          } else {
            shinyWidgets::pickerInput(inputId = "TS_TimeUnit", label = "Periodicity of Data", choices = c("1-Minute","5-Minutes","10-Minutes","15-Minutes","30-Minutes","Hourly","Daily","Weekly","Monthly","Quarterly","Yearly"), selected = "Daily", options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 1"), multiple = TRUE)
          }}, error = function(x) shinyWidgets::pickerInput(inputId = "TS_TimeUnit", label = "Periodicity of Data", choices = c("1-Minute","5-Minutes","10-Minutes","15-Minutes","30-Minutes","Hourly","Daily","Weekly","Monthly","Quarterly","Yearly"), selected = "Daily", options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 1"), multiple = TRUE))

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
        shinyWidgets::pickerInput(inputId = "TS_TimeUnit", label = "Periodicity of Data", choices = c("1-Minute","5-Minutes","10-Minutes","15-Minutes","30-Minutes","Hourly","Daily","Weekly","Monthly","Quarterly","Yearly"), selected = tunit, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 1"), multiple = TRUE)
      } else {
        shinyWidgets::pickerInput(inputId = "TS_TimeUnit", label = "Periodicity of Data", choices = c("1-Minute","5-Minutes","10-Minutes","15-Minutes","30-Minutes","Hourly","Daily","Weekly","Monthly","Quarterly","Yearly"), selected = "Daily", options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 1"), multiple = TRUE)
      }
    })

    # Modeling Args ----
    output$TS_HoldOutPeriods <- shiny::renderUI({RemixAutoML::NumericInput(InputID = "TS_HoldOutPeriods", Label = "Holdout Periods for Model Evaluation", Min = 1, Max = 1000000000, Step = 1, Value = if(exists("EvalData") && !is.null(EvalData) && exists("SourceData") && !is.null(SourceData)) floor(as.numeric(difftime(time1 = max(EvalData[[eval(input$timeSeriesDateColumn_Eval)]]), time2 = max(SourceData[[eval(input$timeSeriesDateColumn)]]), units = input$timeSeriesTimeUnit))) else if(exists("xregs") && !is.null(xregs) && exists("SourceData") && !is.null(SourceData)) floor(as.numeric(difftime(time1 = max(xregs[[eval(input$timeSeriesDateColumn_Eval)]]), time2 = max(SourceData[[eval(input$timeSeriesDateColumn)]]), units = input$timeSeriesTimeUnit))) else 10)})
    output$TS_FCPeriods <- shiny::renderUI({RemixAutoML::NumericInput(InputID = "TS_FCPeriods", Label = "Forecast Periods", Min = 1, Max = 1000000000, Step = 1, Value = if(exists("EvalData") && !is.null(EvalData) && exists("SourceData") && (!is.null(SourceData))) floor(as.numeric(difftime(time1 = max(EvalData[[eval(input$timeSeriesDateColumn_Eval)]]), time2 = max(SourceData[[eval(input$timeSeriesDateColumn)]]), units = input$timeSeriesTimeUnit))) else if(exists("xregs") && !is.null(xregs) && exists("SourceData") && !is.null(SourceData)) floor(as.numeric(difftime(time1 = max(xregs[[eval(input$timeSeriesDateColumn_Eval)]]), time2 = max(SourceData[[eval(input$timeSeriesDateColumn)]]), units = input$timeSeriesTimeUnit))) else 10)})
    output$TS_MetricEval <- shiny::renderUI({RemixAutoML::PickerInput(InputID = "TS_MetricEval", Label = "Select Back-Testing Metric", Choices = c("MAPE","MAE","MSE","RMSE","R2"), SelectedDefault = if(exists("ProjectList")) ProjectList[["TS_MetricEval"]] else "MAE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)})

    # CatBoost Configurations ----

    # Production Args
    output$TS_CatBoost_CARMA_TaskType <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_TaskType", Label = "Train Models with GPU or CPU", Choices = c("GPU","CPU"), SelectedDefault = "CPU", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_CatBoost_CARMA_NumGPU <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_NumGPU", Label = "Number of GPUs Available for Training", Choices = c(1,2,3,4,5,6,7,8), SelectedDefault = 1, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    # Target transformation args
    output$TS_CatBoost_CARMA_TimeWeights <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_TimeWeights", Label = "Select time weights", Choices = c(1,0.9999,0.9995,0.999,0.995,0.99,0.975,0.95),SelectedDefault = 1, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
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
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_TimeTrend", Label = "Time trend", Choices = c("FALSE","TRUE"),SelectedDefault = "TRUE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_CatBoost_CARMA_DataTruncate <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_CatBoost_CARMA_DataTruncate", Label = "Data Truncation", Choices = c("FALSE","TRUE"),SelectedDefault = "FALSE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
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
      RemixAutoML::NumericInput(InputID="TS_CatBoost_CARMA_NTrees",Label="Number of trees", Step = 1, Value = 1000, Min = 50, Max = 250000)
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

    # XGBoost Configurations ----

    # Production Args
    output$TS_XGBoost_CARMA_TaskType <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_TaskType", Label = "Train Models with GPU or CPU", Choices = c("GPU","CPU"), SelectedDefault = "CPU", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    # Target transformation args
    output$TS_XGBoost_CARMA_Transformation <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_Methods", Label = "Transformation options", Choices = c("Identity","YeoJohnson","BoxCox","Asinh","Log","LogPlus1","Sqrt","Asin","Logit"),SelectedDefault = c("Identity"), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_XGBoost_CARMA_Difference <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_Difference", Label = "Differencing target variable", Choices = c("FALSE","TRUE"), SelectedDefault = "FALSE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_XGBoost_CARMA_NonNegativePrep <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_NonNegativePrep", Label = "Do not allow for negative predictions", Choices = c("TRUE","FALSE"),SelectedDefault = "FALSE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_XGBoost_CARMA_RoundPreds <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_RoundPreds", Label = "Round predictions", Choices = c("TRUE","FALSE"),SelectedDefault = "FALSE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    # Calendar-related args
    output$TS_XGBoost_CARMA_CalendarVariables <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_CalendarVariables", Label = "Calendar variables", Choices = c("second","minute","hour","wday","mday","yday","week","isoweek","wom","month","quarter","year"), SelectedDefault = c("minute","hour","wday","mday","yday","week","wom","month","quarter"), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_XGBoost_CARMA_HolidayVariables <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_HolidayVariables", Label = "Holiday variables", Choices = c("USPublicHolidays","EasterGroup","ChristmasGroup","OtherEcclesticalFeasts"), SelectedDefault = "USPublicHolidays", Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_XGBoost_CARMA_HolidayLags <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_HolidayLags", Label = "Select Holiday Count Lags", Choices = as.character(1:50), SelectedDefault = as.character(c(1,2)), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_XGBoost_CARMA_HolidayMovingAverages <- renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_HolidayMovingAverages", Label = "Select Holiday Count MA's", Choices = as.character(2:50), SelectedDefault = as.character(c(2,3)), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })

    # Lags, moving averages, and other rolling stats
    output$TS_XGBoost_CARMA_HierarchGroups <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_HierarchGroups", Label = "Hierarchical rolling stats", Choices = c("FALSE","TRUE"), SelectedDefault = "FALSE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_XGBoost_CARMA_Lags <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_Lags", Label = "Lag windows", Choices = c(1:1000), SelectedDefault = c(1:10), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_XGBoost_CARMA_MovingAverages <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_MovingAverages", Label = "Moving average windows", Choices = c(2:1000), SelectedDefault = c(2:10), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_XGBoost_CARMA_MovingSD <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_MovingSD", Label = "Standard deviation windows", Choices = c(2:1000), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_XGBoost_CARMA_MovingSkew <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_MovingSkew", Label = "Skewness windows", Choices = c(3:1000), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_XGBoost_CARMA_MovingKurt <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_MovingKurt", Label = "Kurtosis windows", Choices = c(4:1000), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_XGBoost_CARMA_MovingQuantiles <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_MovingQuantiles", Label = "Percentile windows", Choices = c(5:1000), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_XGBoost_CARMA_Quantiles_Selected <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_Quantiles_Selected", Label = "Select percentiles", Choices = c("q5","q10","q15","q20","q25","q30","q35","q40","q45","q50","q55","q60","q65","q70","q75","q80","q85","q90","q95"),SelectedDefault = "q50", Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })

    # Second time agg: Lags, moving averages, and other rolling stats
    output$TS_XGBoost_CARMA_Lags1 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_Lags1", Label = "Lag windows", Choices = c(1:1000), SelectedDefault = c(1:10), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_XGBoost_CARMA_MovingAverages1 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_MovingAverages1", Label = "Moving average windows", Choices = c(2:1000), SelectedDefault = c(2:10), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_XGBoost_CARMA_MovingSD1 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_MovingSD1", Label = "Standard deviation windows", Choices = c(2:1000), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_XGBoost_CARMA_MovingSkew1 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_MovingSkew1", Label = "Skewness windows", Choices = c(3:1000), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_XGBoost_CARMA_MovingKurt1 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_MovingKurt1", Label = "Kurtosis windows", Choices = c(4:1000), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_XGBoost_CARMA_MovingQuantiles1 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_MovingQuantiles1", Label = "Percentile windows", Choices = c(3:1000), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })

    # Bonus features
    output$TS_XGBoost_CARMA_TimeWeights <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_TimeWeights", Label = "Select time weights", Choices = c(1,0.9999,0.9995,0.999,0.995,0.99,0.975,0.95),SelectedDefault = 1, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_XGBoost_CARMA_DataTruncate <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_DataTruncate", Label = "Truncate data", Choices = c("FALSE","TRUE"), SelectedDefault = "FALSE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_XGBoost_CARMA_Fourier <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_Fourier", Label = "Fourier pairs", Choices = c(0,seq(1,25,1)), SelectedDefault = c(0), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_XGBoost_CARMA_TimeTrend <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_TimeTrend", Label = "Time trend", Choices = c("FALSE","TRUE"), SelectedDefault = "TRUE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_XGBoost_CARMA_AnomalyDetection_HighThreshold <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_AnomalyDetection_HighThreshold", Label = "Select tstat for upper threshold", Choices = c(0,3,4,5,6),SelectedDefault = 0, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_XGBoost_CARMA_AnomalyDetection_LowThreshold <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_AnomalyDetection_LowThreshold", Label = "Select tstat for upper threshold", Choices = c(0,-3,-4,-5,-6),SelectedDefault = 0, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    # ML evaluation args
    output$TS_XGBoost_CARMA_EvalMetric <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_EvalMetric", Label = "Select evaluation metric", Choices = c("RMSE","MAE"), SelectedDefault = "rmse", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_XGBoost_CARMA_LossFunction <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_LossFunction", Label = "Select a loss function", Choices = c("Squared Error", "Squared Log Error", "Pseudo Huber", "Poisson", "Gamma", "Tweedie"), SelectedDefault = "squarederror", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    # ML grid tuning args
    output$TS_XGBoost_CARMA_GridTune <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_GridTune", Label = "Grid tune", Choices = c("FALSE","TRUE"),SelectedDefault = "FALSE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_XGBoost_CARMA_PassInGrid <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_PassInGrid", Label = "PassInGrid from previous GridTune", Choices = c("NULL", file.path(ProjectList[["DataFolderPath"]],"Grid.csv")), SelectedDefault = "NULL", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_XGBoost_CARMA_MaxRunsWithoutNewWinner <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_MaxRunsWithoutNewWinner", Label = "Max runs without new winner", Choices = c(seq(10,100,10)),SelectedDefault = 10, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_XGBoost_CARMA_MaxRunMinutes <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_MaxRunMinutes", Label = "Max runtime in minutes", Choices = c(seq(60*24,60*24*7,60*24)),SelectedDefault = 60*24, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_XGBoost_CARMA_ModelCount <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="TS_XGBoost_CARMA_ModelCount",Label="Max number of models", Step = 1, Value = 5, Min = 1, Max = 250)
    })

    # ML args
    output$TS_XGBoost_CARMA_NTrees <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="TS_XGBoost_CARMA_NTrees",Label="Number of trees", Step = 1, Value = 1000, Min = 50, Max = 250000)
    })
    output$TS_XGBoost_CARMA_LearningRate <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_LearningRate", Label = "Learning rate", Choices = c(seq(0.0,0.50,0.01)), SelectedDefault = 0.30, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_XGBoost_CARMA_Depth <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_Depth", Label = "Depth", Choices = c(seq(4,16,1)), SelectedDefault = 6, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_XGBoost_CARMA_MinChildWeight <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_MinChildWeight", Label = "Min instances per tree", Choices = c(seq(1,250,1)), SelectedDefault = 1, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_XGBoost_CARMA_SubSample <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_SubSample", Label = "Row sample for training", Choices = c(seq(0.05,1,0.05)), SelectedDefault = 1, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_XGBoost_CARMA_ColSampleByTree <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_XGBoost_CARMA_ColSampleByTree", Label = "Column sample by tree", Choices = c(seq(0.05,1,0.05)), SelectedDefault = 1, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    # H2O Configurations ----

    # Production args
    output$TS_H2O_CARMA_EvalMetric <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_EvalMetric", Label = "Eval metric", Choices = c("RMSE", "MAE", "MAPE", "Poisson", "Quantile", "LogLinQuantile", "Lq", "SMAPE", "R2", "MSLE", "MedianAbsoluteError"), SelectedDefault = "RMSE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_H2O_CARMA_ExcludeAlgos <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_ExcludeAlgos", Label = "Algos to exclude from the run", Choices = c("TRUE","FALSE"),SelectedDefault = "FALSE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_H2O_CARMA_NThreads <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="TS_H2O_CARMA_NThreads",Label="Input number of CPU threads", Step = 1, Value = max(1,parallel::detectCores()-2), Min = 1, Max = 256)
    })
    output$TS_H2O_CARMA_MaxMemory <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="TS_H2O_CARMA_MaxMemory",Label="Input the amount of memory to allocate",Step = 10, Value = 28, Min = 1, Max = 1568*2)
    })

    # Target transformations
    output$TS_H2O_CARMA_TimeWeights <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_TimeWeights", Label = "Select time weights", Choices = c(1,0.9999,0.9995,0.999,0.995,0.99,0.975,0.95),SelectedDefault = 1, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_H2O_CARMA_Methods <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_Methods", Label = "Select transformations", Choices = c("Identity", "YeoJohnson", "BoxCox", "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit"), SelectedDefault = "Identity", Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_H2O_CARMA_NonNegativePred <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_NonNegativePred", Label = "Target variable differencing", Choices = c("TRUE","FALSE"),SelectedDefault = "FALSE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_H2O_CARMA_RoundPreds <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_RoundPreds", Label = "Target variable differencing", Choices = c("TRUE","FALSE"),SelectedDefault = "FALSE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_H2O_CARMA_Difference <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_Difference", Label = "Target variable differencing", Choices = c("TRUE","FALSE"),SelectedDefault = "FALSE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    # Calendar features
    output$TS_H2O_CARMA_CalendarVariables <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_CalendarVariables", Label = "Calendar variables", Choices = c("second","minute","hour","wday","mday","yday","week","isoweek","wom","month","quarter","year"),SelectedDefault = c("minute","hour","wday","mday","yday","week","wom","month","quarter"), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_H2O_CARMA_HolidayVariables <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_HolidayVariables", Label = "Holiday variables", Choices = c("USPublicHolidays","EasterGroup","ChristmasGroup","OtherEcclesticalFeasts"),SelectedDefault = "USPublicHolidays", Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_H2O_CARMA_HolidayLags <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_HolidayLags", Label = "Select Holiday Count Lags", Choices = c(seq(1,50,1)), SelectedDefault = c(1,2,3), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_H2O_CARMA_HolidayMovingAverages <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_HolidayMovingAverages", Label = "Select Holiday Count moving averages", Choices = c(seq(2,50,1)), SelectedDefault = c(2,3), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })

    # Bonus features
    output$TS_H2O_CARMA_AnomalyDetection_HighThreshold <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_AnomalyDetection_HighThreshold", Label = "Select tstat for upper threshold", Choices = c(0,3,4,5,6),SelectedDefault = 0, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_H2O_CARMA_AnomalyDetection_LowThreshold <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_AnomalyDetection_LowThreshold", Label = "Select tstat for upper threshold", Choices = c(0,-3,-4,-5,-6),SelectedDefault = 0, Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_H2O_CARMA_TimeTrend <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_TimeTrend", Label = "Time Trend", Choices = c("FALSE","TRUE"),SelectedDefault = "TRUE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_H2O_CARMA_DataTruncate <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_DataTruncate", Label = "Data Truncation", Choices = c("FALSE","TRUE"),SelectedDefault = "TRUE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_H2O_CARMA_Fourier <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_Fourier", Label = "Fourier pairs", Choices = c(0,seq(1,25,1)),SelectedDefault = c(0), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })

    # Lags, moving averages, and other rolling stats
    output$TS_H2O_CARMA_Quantiles_Selected <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_Quantiles_Selected", Label = "Select percentiles", Choices = c("q5","q10","q15","q20","q25","q30","q35","q40","q45","q50","q55","q60","q65","q70","q75","q80","q85","q90","q95"),SelectedDefault = "q50", Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
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

    # Second time agg: Lags, moving averages, and other rolling stats
    output$TS_H2O_CARMA_Lags1 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_Lags1", Label = "Select windows", Choices = c(1:10),SelectedDefault = c(1:10), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_H2O_CARMA_MovingAverages1 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_MovingAverages1", Label = "Select windows", Choices = c(2:10),SelectedDefault = c(2:10), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_H2O_CARMA_MovingSD1 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_MovingSD1", Label = "Select windows", Choices = c(2:10),SelectedDefault = c(2:10), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_H2O_CARMA_MovingSkew1 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_MovingSkew1", Label = "Select windows", Choices = c(3:10),SelectedDefault = c(3:10), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_H2O_CARMA_MovingKurt1 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_MovingKurt1", Label = "Select windows", Choices = c(4:10),SelectedDefault = c(4:10), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_H2O_CARMA_MovingQuantiles1 <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_MovingQuantiles1", Label = "Select windows", Choices = c(3:10),SelectedDefault = c(3:10), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })

    # ML grid tuning args
    output$TS_H2O_CARMA_GridTune <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_GridTune", Label = "Grid tune", Choices = c("FALSE","TRUE"), SelectedDefault = "FALSE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_H2O_CARMA_ModelCount <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="TS_H2O_CARMA_ModelCount", Label="Max number of models", Step = 1, Value = 30, Min = 1, Max = 100000)
    })
    output$TS_H2O_CARMA_GridStrategy <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_GridStrategy", Label = "Grid tune strategy", Choices = c("Cartesian","RandomDiscrete"), SelectedDefault = "RandomDiscrete", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_H2O_CARMA_MaxRuntimeSecs <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="TS_H2O_CARMA_MaxRuntimeSecs", Label="Max runtime seconds", Step = 1, Value = 60*60*24, Min = 1, Max = 86400*30)
    })
    output$TS_H2O_CARMA_StoppingRounds <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="TS_H2O_CARMA_StoppingRounds", Label="Max trees built without improvement", Step = 1, Value = 10, Min = 1, Max = 10*100)
    })

    # ML args GBM and DRF Specific
    output$TS_H2O_CARMA_NTrees <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="TS_H2O_CARMA_NTrees", Label="Number of trees", Step = 1, Value = 1000, Min = 50, Max = 100000)
    })
    output$TS_H2O_CARMA_LearnRate <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="TS_H2O_CARMA_LearnRate", Label="Learning rate", Step = 0.01, Value = 0.10, Min = 0, Max = 0.99)
    })
    output$TS_H2O_CARMA_LearnRateAnnealing <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="TS_H2O_CARMA_LearnRateAnnealing", Label="Learn rate annealing", Step = 0.01, Value = 1, Min = 0.01, Max = 1)
    })
    output$TS_H2O_CARMA_MaxDepth <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="TS_H2O_CARMA_MaxDepth", Label="Max depth of trees", Step = 1, Value = 6, Min = 4, Max = 20)
    })
    output$TS_H2O_CARMA_SampleRate <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="TS_H2O_CARMA_SampleRate", Label="Sample rate", Step = 0.01, Value = 0.632, Min = 0.10, Max = 1)
    })
    output$TS_H2O_CARMA_ColSampleRatePerTree <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="TS_H2O_CARMA_ColSampleRatePerTree",Label="Column sample rate per tree", Step = 0.01, Value = 1, Min = 0.10, Max = 1)
    })
    output$TS_H2O_CARMA_ColSampleRatePerTreeLevel <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="TS_H2O_CARMA_ColSampleRatePerTreeLevel", Label="Column sample rate per tree level", Step = 0.01, Value = 1, Min = 0.10, Max = 1)
    })
    output$TS_H2O_CARMA_MinRows <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="TS_H2O_CARMA_MinRows", Label="Minimum rows per leaf", Step = 1, Value = 1, Min = 1, Max = 5000)
    })
    output$TS_H2O_CARMA_NBins <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="TS_H2O_CARMA_NBins", Label = "Number of bins for numeric features", Step = 1, Value = 20, Min = 2, Max = 256)
    })
    output$TS_H2O_CARMA_NBinsCats <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="TS_H2O_CARMA_NBinsCats",Label="Number of bins for categorical features", Step = 64, Value = 1024, Min = 2, Max = 1024*10)
    })
    output$TS_H2O_CARMA_NBinsTopLevel <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="TS_H2O_CARMA_NBinsTopLevel", Label="Number of bins top level", Step = 1, Value = 1024, Min = 2, Max = 1024*10)
    })
    output$TS_H2O_CARMA_HistogramType <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_HistogramType", Label = "Histogram type", Choices = c("AUTO", "UniformAdaptive", "Random", "QuantilesGlobal", "RoundRobin"), SelectedDefault = "AUTO", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_H2O_CARMA_CategoricalEncoding <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_CategoricalEncoding", Label = "Categorical encoding", Choices = c("AUTO", "Enum", "OneHotInternal", "OneHotExplicit", "Binary", "Eigen", "LabelEncoder", "Sort-ByResponse", "EnumLimited"), SelectedDefault = "AUTO", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    # DRF only
    output$TS_H2O_CARMA_MTries <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="TS_H2O_CARMA_MTries", Label = "Rows per tree", Step = 0.01, Value = -1, Min = -1, Max = 1)
    })
    output$TS_H2O_CARMA_ColSampleRate <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="TS_H2O_CARMA_ColSampleRate", Label = "Column sample rate", Step = 0.01, Value = 1, Min = 0.10, Max = 1)
    })

    # GLM and GAM args
    output$TS_H2O_CARMA_RandomColumnNames <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_RandomColumnNames", Label = "Random effects columns", Choices = if(exists("SourceData")) names(SourceData) else NULL, SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_H2O_CARMA_InteractionColumns <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_InteractionColumns", Label = "Interaction columns", Choices = if(exists("SourceData")) names(SourceData) else NULL, SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_H2O_CARMA_Distribution <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_Distribution", Label = "Model family", Choices = c("AUTO", "gaussian", "poisson", "gamma", "tweedie", "negativebinomial"), SelectedDefault = "AUTO", Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_H2O_CARMA_Link <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_Link", Label = "Model family link", Choices = c("family_default", "identity", "log", "inverse", "tweedie"), SelectedDefault = NULL, Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)
    })
    output$TS_H2O_CARMA_Solver <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_Solver", Label = "Model optimizers", Choices = c("AUTO", "IRLSM", "L_BFGS", "COORDINATE_DESCENT_NAIVE","COORDINATE_DESCENT", "GRADIENT_DESCENT_LH", "GRADIENT_DESCENT_SQERR"), SelectedDefault = "AUTO", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_H2O_CARMA_Alpha <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="TS_H2O_CARMA_Alpha", Label = "Regularization blend", Step = 0.01, Value = 0.5, Min = 0.0, Max = 1.0)
    })
    output$TS_H2O_CARMA_Lambda <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="TS_H2O_CARMA_NTrees", Label = "Regularization strength", Step = 0.10, Value = 1, Min = 0.10, Max = 100)
    })
    output$TS_H2O_CARMA_LambdaSearch <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_LambdaSearch", Label = "Lambda tuning", Choices = c("FALSE","TRUE"), SelectedDefault = "FALSE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_H2O_CARMA_NLambdas <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID="TS_H2O_CARMA_NLambdas", Label = "Number of lambdas to test", Step = 1, Value = 30, Min = 2, Max = 500)
    })
    output$TS_H2O_CARMA_Standardize <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_Standardize", Label = "Standardize features", Choices = c("FALSE","TRUE"), SelectedDefault = "TRUE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_H2O_CARMA_RemoveCollinearColumns <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_RemoveCollinearColumns", Label = "Remove collinear columns", Choices = c("FALSE","TRUE"), SelectedDefault = "TRUE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_H2O_CARMA_InterceptInclude <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_InterceptInclude", Label = "Include intercept", Choices = c("FALSE","TRUE"), SelectedDefault = "TRUE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$TS_H2O_CARMA_NonNegativeCoefficients <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "TS_H2O_CARMA_NonNegativeCoefficients", Label = "Force non-negative coefficients", Choices = c("FALSE","TRUE"), SelectedDefault = "FALSE", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })

    # ProjectList[[]] Save Model Settings ----
    shiny::observeEvent({
      input$TS_Build
      input$TS_SaveModelSettings}, {

        # Single Click Solution Args ----
        ProjectList[["TS_MetricEval"]] <<- input$TS_MetricEval
        ProjectList[["TS_CARMA_TaskType"]] <<- input$TS_CARMA_TaskType

        # Model Selection Args ----
        ProjectList[["TSMLModelsSelection"]] <<- input$TSMLModelsSelection

        # Required Args ----
        ProjectList[["TS_timeSeriesTarget"]] <<- input$TS_timeSeriesTarget
        ProjectList[["TS_timeSeriesDateColumn"]] <<- input$TS_timeSeriesDateColumn
        ProjectList[["TS_timeSeriesGroupVars"]] <<- input$TS_timeSeriesGroupVars
        ProjectList[["TS_FCPeriods"]] <<- input$TS_FCPeriods
        ProjectList[["TS_TimeUnit"]] <<- input$TS_TimeUnit
        ProjectList[["TS_HoldOutPeriods"]] <<- input$TS_HoldOutPeriods
        ProjectList[["H2OModelSelection"]] <<- input$H2OModelSelection
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CARMA_PDFOutputPath", Type = "character", Default = "NULL")
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CARMA_SaveDataPath", Type = "character", Default = "NULL")
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CARMA_NumParDepPlots", Type = "numeric", Default = 25)

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
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_CARMA_NumParDepPlots", Type = "numeric", Default = 25)

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

        # XGBoost CARMA Args ----

        # Production args
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_XGBoost_CARMA_TaskType", Type = "character", Default = "CPU")

        # Target transformation args
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_XGBoost_CARMA_TimeWeights", Type = "numeric", Default = 0.9999)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_XGBoost_CARMA_Methods", Type = "character", Default = c("Identity"))
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_XGBoost_CARMA_Difference", Type = "character", Default = "FALSE")
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_XGBoost_CARMA_NonNegativePrep", Type = "character", Default = "FALSE")
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_XGBoost_CARMA_RoundPreds", Type = "character", Default = "TRUE")

        # Bonus features
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_XGBoost_CARMA_Fourier", Type = "numeric", Default = 2)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_XGBoost_CARMA_TimeTrend", Type = "character", Default = "TRUE")
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_XGBoost_CARMA_DataTruncate", Type = "character", Default = "FALSE")
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_XGBoost_CARMA_AnomalyDetection_HighThreshold", Type = "numeric", Default = 0)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_XGBoost_CARMA_AnomalyDetection_LowThreshold", Type = "numeric", Default = 0)

        # Calendar features
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_XGBoost_CARMA_CalendarVariables", Type = "character", Default = c("minute","hour","wday","mday","yday","week","wom","month","quarter"))
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_XGBoost_CARMA_HolidayVariables", Type = "character", Default = c("USPublicHolidays"))
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_XGBoost_CARMA_HolidayLags", Type = "character", Default = "1")
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_XGBoost_CARMA_HolidayMovingAverages", Type = "character", Default = c("2","3"))

        # Time series features
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_XGBoost_CARMA_Lags", Type = "character", Default = as.character(1:10))
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_XGBoost_CARMA_MovingAverages", Type = "character", Default = as.character(2:10))
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_XGBoost_CARMA_MovingSD", Type = "character", Default = NULL)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_XGBoost_CARMA_MovingSkew", Type = "character", Default = NULL)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_XGBoost_CARMA_MovingKurt", Type = "character", Default = NULL)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_XGBoost_CARMA_MovingQuantiles", Type = "character", Default = NULL)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_XGBoost_CARMA_Lags1", Type = "character", Default = as.character(1:5))
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_XGBoost_CARMA_MovingAverages1", Type = "character", Default = as.character(2:5))
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_XGBoost_CARMA_MovingSD1", Type = "character", Default = NULL)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_XGBoost_CARMA_MovingSkew1", Type = "character", Default = NULL)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_XGBoost_CARMA_MovingKurt1", Type = "character", Default = NULL)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_XGBoost_CARMA_MovingQuantiles1", Type = "character", Default = NULL)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_XGBoost_CARMA_HierarchGroups", Type = "character", Default = "FALSE")

        # ML evaluation args
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_XGBoost_CARMA_EvalMetric", Type = "character", Default = "RMSE")
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_XGBoost_CARMA_LossFunction", Type = "character", Default = "Squared Error")

        # ML grid tuning
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_XGBoost_CARMA_GridTune", Type = "character", Default = "FALSE")
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_XGBoost_CARMA_ModelCount", Type = "numeric", Default = 5)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_XGBoost_CARMA_PassInGrid", Type = "character", Default = "NULL")
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_XGBoost_CARMA_MaxRunsWithoutNewWinner", Type = "numeric", Default = 25)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_XGBoost_CARMA_MaxRunMinutes", Type = "numeric", Default = 60*24)

        # ML args
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_XGBoost_CARMA_NTrees", Type = "numeric", Default = 1000)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_XGBoost_CARMA_LearningRate", Type = "numeric", Default = 0.3)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_XGBoost_CARMA_Depth", Type = "numeric", Default = 6)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_XGBoost_CARMA_MinChildWeight", Type = "numeric", Default = 1)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_XGBoost_CARMA_SubSample", Type = "numeric", Default = 1)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_XGBoost_CARMA_ColSampleByTree", Type = "numeric", Default = 1)

        # H2O CARMA Args ----
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_EvalMetric", Type = "character", Default = "RMSE")
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_ExcludeAlgos", Type = "character", Default = "XGBoost")
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "H2OModelSelection", Type = "character", Default = "DRF")
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_TimeWeights", Type = "numeric", Default = 1)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_Methods", Type = "character", Default = "Identity")
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_NonNegativePred", Type = "character", Default = "FALSE")
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_RoundPreds", Type = "character", Default = "FALSE")
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_NThreads", Type = "numeric", Default = -1)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_MaxMemory", Type = "character", Default = "28g")
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_Difference", Type = "character", Default = "FALSe")
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_CalendarVariables", Type = "character", Default = c("minute","hour","wday","mday","yday","week","wom","month","quarter"))
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_HolidayVariables", Type = "character", Default = "USPublicHolidays")
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_HolidayLags", Type = "numeric", Default = c(1,2))
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_HolidayMovingAverages", Type = "numeric", Default = c(2,3))
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_TimeTrend", Type = "character", Default = "TRUE")
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_DataTruncate", Type = "character", Default = "FALSE")
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_Fourier", Type = "numeric", Default = 0)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_Quantiles_Selected", Type = "character", Default = NULL)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_Lags", Type = "numeric", Default = c(1:10))
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_MovingAverages", Type = "numeric", Default = c(1:10))
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_MovingSD", Type = "numeric", Default = NULL)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_MovingSkew", Type = "numeric", Default = NULL)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_MovingKurt", Type = "numeric", Default = NULL)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_MovingQuantiles", Type = "numeric", Default = NULL)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_Lags1", Type = "numeric", Default = c(1:10))
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_MovingAverages1", Type = "numeric", Default = c(1:10))
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_MovingSD1", Type = "numeric", Default = NULL)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_MovingSkew1", Type = "numeric", Default = NULL)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_MovingKurt1", Type = "numeric", Default = NULL)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_MovingQuantiles1", Type = "numeric", Default = NULL)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_GridTune", Type = "character", Default = "FALSE")
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_ModelCount", Type = "numeric", Default = 20)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_GridStrategy", Type = "character", Default = "RandomDiscrete")
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_MaxRuntimeSecs", Type = "numeric", Default = 60*60*24)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_StoppingRounds", Type = "numeric", Default = 10)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_NTrees", Type = "numeric", Default = 1000)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_LearnRate", Type = "numeric", Default = 0.10)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_LearnRateAnnealing", Type = "numeric", Default = 1)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_MaxDepth", Type = "numeric", Default = 6)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_SampleRate", Type = "character", Default = 0.623)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_ColSampleRatePerTree", Type = "numeric", Default = 1)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_ColSampleRatePerTreeLevel", Type = "numeric", Default = 1)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_MinRows", Type = "numeric", Default = 1)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_NBins", Type = "numeric", Default = 1024)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_NBinsCats", Type = "numeric", Default = 1024)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_NBinsTopLevel", Type = "numeric", Default = 1024)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_HistogramType", Type = "character", Default = "AUTO")
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_CategoricalEncoding", Type = "character", Default = "AUTO")
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_MTries", Type = "numeric", Default = 0.623)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_ColSampleRate", Type = "numeric", Default = 1)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_RandomColumnNames", Type = "character", Default = if(exists("SourceData")) names(SourceData) else NULL)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_InteractionColumns", Type = "character", Default = if(exists("SourceData")) names(SourceData) else NULL)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_Distribution", Type = "character", Default = "gaussian")
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_Link", Type = "character", Default = "Identity")
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_Solver", Type = "character", Default = "AUTO")
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_Alpha", Type = "numeric", Default = 0.5)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_Lambda", Type = "numeric", Default = 1)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_LambdaSearch", Type = "character", Default = "FALSE")
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_NLambdas", Type = "numeric", Default = 30)
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_Standardize", Type = "character", Default = "TRUE")
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_RemoveCollinearColumns", Type = "character", Default = "TRUE")
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_InterceptInclude", Type = "character", Default = "TRUE")
        RemixAutoML::StoreArgs(input = input, ProjectList = ProjectList, VarName = "TS_H2O_CARMA_NonNegativeCoefficients", Type = "character", Default = "FALSE")

        # Success ----
        shinyWidgets::sendSweetAlert(session, title = NULL, text = "Model settings are saved!", type = "success", btn_labels = "Ok", btn_colors = "green", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "60%")
    })

    # Build Forecasts and Save to Forecasts sz  /----
    shiny::observeEvent(input$TS_Build, {

      # Define ML Models ----
      ML_Models <- input$TSMLModelsSelection
      ML_Models <- ML_Models[!ML_Models %chin% "None"]
      ProjectList[["ML_Models"]] <<- ML_Models

      # Number of models ----
      n <- length(ML_Models)

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

      # Required User Inputs ----
      TS_FCPeriods <- input$TS_FCPeriods
      TS_HoldOutPeriods <- input$TS_HoldOutPeriods
      TS_MetricEval <- input$TS_MetricEval
      TS_TimeUnit <- c()
      if(any(input$TS_TimeUnit == "Hourly")) TS_TimeUnit <- c(TS_TimeUnit, "hour")
      if(any(input$TS_TimeUnit == "1-Minutes")) TS_TimeUnit <- c(TS_TimeUnit, "1min")
      if(any(input$TS_TimeUnit == "5-Minutes")) TS_TimeUnit <- c(TS_TimeUnit, "5min")
      if(any(input$TS_TimeUnit == "10-Minutes")) TS_TimeUnit <- c(TS_TimeUnit, "10min")
      if(any(input$TS_TimeUnit == "15-Minutes")) TS_TimeUnit <- c(TS_TimeUnit, "15min")
      if(any(input$TS_TimeUnit == "30-Minutes")) TS_TimeUnit <- c(TS_TimeUnit, "30min")
      if(any(input$TS_TimeUnit == "Daily")) TS_TimeUnit <- c(TS_TimeUnit, "days")
      if(any(input$TS_TimeUnit == "Weekly")) TS_TimeUnit <- c(TS_TimeUnit, "weeks")
      if(any(input$TS_TimeUnit == "Monthly")) TS_TimeUnit <- c(TS_TimeUnit, "months")
      if(any(input$TS_TimeUnit == "Quarterly")) TS_TimeUnit <- c(TS_TimeUnit, "quarters")
      if(any(input$TS_TimeUnit == "Yearly")) TS_TimeUnit <- c(TS_TimeUnit, "years")

      # Model Building ----
      shiny::withProgress(message = 'Model Building', value = 0, {

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

          # CatBoostCARMA ----
          if("CatBoost-CARMA" %chin% ML_Models) {

            # Switch ----
            if(exists("Switch_CatBoost")) {
              Switch_CatBoost <<- TRUE
            } else {
              Switch_CatBoost <<- FALSE
            }

            # Modeling Parameters ----

            # General args
            TS_CARMA_NumParDepPlots <- RemixAutoML::ReturnParam(input, VarName = "TS_CARMA_NumParDepPlots", Type = "numeric", Default = 25, Switch = Switch_CatBoost)
            TS_CARMA_PDFOutputPath <- RemixAutoML::ReturnParam(input, VarName = "TS_CARMA_PDFOutputPath", Type = "character", Default = "NULL", Switch = Switch_CatBoost)
            TS_CARMA_SaveDataPath <- RemixAutoML::ReturnParam(input, VarName = "TS_CARMA_SaveDataPath", Type = "character", Default = "NULL", Switch = Switch_CatBoost)

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

            # N ----
            if(ncol(data) > 2L) N <- data[, .N, by = c(eval(GroupVariableNames))][, max(N)] else N <- data[, .N]

            # Time series features ----
            if(length(TS_TimeUnit) > 1L) {
              Defined_Lags <- list()
              Defined_MA <- list()
              Defined_SD <- list()
              Defined_Skew <- list()
              Defined_Kurt <- list()
              Defined_Percentile <- list()
              for(z in seq_len(2L)) {
                if(z == 1L) {
                  if(!is.null(TS_CatBoost_CARMA_Lags)) Defined_Lags[[TS_TimeUnit[z]]] <- as.numeric(TS_CatBoost_CARMA_Lags) else Defined_Lags[[TS_TimeUnit[z]]] <- NULL
                  if(!is.null(TS_CatBoost_CARMA_MovingAverages)) Defined_MA[[TS_TimeUnit[z]]] <- as.numeric(TS_CatBoost_CARMA_MovingAverages) else Defined_MA[[TS_TimeUnit[z]]] <- NULL
                  if(!is.null(TS_CatBoost_CARMA_MovingSD)) Defined_SD[[TS_TimeUnit[z]]] <- as.numeric(TS_CatBoost_CARMA_MovingSD) else Defined_SD[[TS_TimeUnit[z]]] <- NULL
                  if(!is.null(TS_CatBoost_CARMA_MovingSkew)) Defined_Skew[[TS_TimeUnit[z]]] <- as.numeric(TS_CatBoost_CARMA_MovingSkew) else Defined_Skew[[TS_TimeUnit[z]]] <- NULL
                  if(!is.null(TS_CatBoost_CARMA_MovingKurt)) Defined_Kurt[[TS_TimeUnit[z]]] <- as.numeric(TS_CatBoost_CARMA_MovingKurt) else Defined_Kurt[[TS_TimeUnit[z]]] <- NULL
                  if(!is.null(TS_CatBoost_CARMA_MovingQuantiles)) Defined_Percentile[[TS_TimeUnit[z]]] <- as.numeric(TS_CatBoost_CARMA_MovingQuantiles) else Defined_Percentile[[TS_TimeUnit[z]]] <- NULL
                } else if(z == 2L) {
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

            # Loop through model builds ----
            for(Runs in c("Baseline", "Forecast")) {

              # Copy data ----
              if(!is.null(GroupVariableNames)) {
                datax <- data.table::copy(data)
              } else {
                if(Runs == "Baseline") data.table::fwrite(data, file.path(ProjectList$DataFolderPath, "EvalData.csv"))
                data.table::setorderv(x = data, cols = eval(DateName), order = 1L)
                datax <- data[seq_len(.N - eval(TS_FCPeriods))]
              }

              # Copy xregs data ----
              if(exists("XREGS")) xregsx <- tryCatch({data.table::copy(xregs)}, error = function(x) NULL) else xregsx <- NULL

              # Define args for baseline and forecast runs ----
              if(Runs == "Baseline") {
                TOF <- FALSE
                NTrees <- TS_CatBoost_CARMA_NTrees
                LearningRate <- TS_CatBoost_CARMA_LearningRate
                Ratios <- c(1 - TS_HoldOutPeriods / N, TS_HoldOutPeriods / N)
              } else if(Runs == "Forecast") {
                TOF <- TRUE
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
                XREGS = xregsx,
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
                PDFOutputPath = if(is.null(TS_CARMA_PDFOutputPath)) NULL else file.path(ProjectList[["DataFolderPath"]]),
                SaveDataPath = if(is.null(TS_CARMA_SaveDataPath)) NULL else file.path(ProjectList[["DataFolderPath"]]),
                NumOfParDepPlots = TS_CARMA_NumParDepPlots,

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

            # Update Progress Bar----
            incProgress(amount = 1/n, message = "AutoCatBoostCARMA Complete")
          }

          # XGBoostCARMA ----
          if("XGBoost-CARMA" %chin% ML_Models) {

            # Switch ----
            if(exists("Switch_XGBoost")) {
              Switch_XGBoost <<- TRUE
            } else {
              Switch_XGBoost <<- FALSE
            }

            # Modeling Parameters ----

            # General args
            TS_CARMA_NumParDepPlots <- RemixAutoML::ReturnParam(input, VarName = "TS_CARMA_NumParDepPlots", Type = "numeric", Default = 25, Switch = Switch_XGBoost)
            TS_CARMA_PDFOutputPath <- RemixAutoML::ReturnParam(input, VarName = "TS_CARMA_PDFOutputPath", Type = "character", Default = "NULL", Switch = Switch_XGBoost)
            TS_CARMA_SaveDataPath <- RemixAutoML::ReturnParam(input, VarName = "TS_CARMA_SaveDataPath", Type = "character", Default = "NULL", Switch = Switch_XGBoost)

            # Productionize args
            TS_XGBoost_CARMA_TaskType <- RemixAutoML::ReturnParam(input, VarName = "TS_XGBoost_CARMA_TaskType", Type = "character", Default = "CPU", Switch = Switch_XGBoost)

            # Target transformations
            TS_XGBoost_CARMA_Methods <- RemixAutoML::ReturnParam(input, VarName = "TS_XGBoost_CARMA_Methods", Type = "character", Default = c("Identity"), Switch = Switch_XGBoost)
            TS_XGBoost_CARMA_Difference <- RemixAutoML::ReturnParam(input, VarName = "TS_XGBoost_CARMA_Difference", Type = "character", Default = "FALSE", Switch = Switch_XGBoost)
            TS_XGBoost_CARMA_NonNegativePrep <- RemixAutoML::ReturnParam(input, VarName = "TS_XGBoost_CARMA_NonNegativePrep", Type = "character", Default = "FALSE", Switch = Switch_XGBoost)
            TS_XGBoost_CARMA_RoundPreds <- RemixAutoML::ReturnParam(input, VarName = "TS_XGBoost_CARMA_RoundPreds", Type = "character", Default = "TRUE", Switch = Switch_XGBoost)
            #TS_XGBoost_CARMA_TimeWeights <- RemixAutoML::ReturnParam(input, VarName = "TS_XGBoost_CARMA_TimeWeights", Type = "numeric", Default = 0.9999, Switch = Switch_XGBoost)

            # Calendar-related features
            TS_XGBoost_CARMA_CalendarVariables <- RemixAutoML::ReturnParam(input, VarName = "TS_XGBoost_CARMA_CalendarVariables", Type = "character", Default = c("hour","wday","mday","yday","week","wom","month","quarter"), Switch = Switch_XGBoost)
            TS_XGBoost_CARMA_HolidayVariables <- RemixAutoML::ReturnParam(input, VarName = "TS_XGBoost_CARMA_HolidayVariables", Type = "character", Default = "USPublicHolidays", Switch = Switch_XGBoost)
            TS_XGBoost_CARMA_HolidayLags <- RemixAutoML::ReturnParam(input, VarName = "TS_XGBoost_CARMA_HolidayLags", Type = "character", Default = c("1"), Switch = Switch_XGBoost)
            TS_XGBoost_CARMA_HolidayMovingAverages <- RemixAutoML::ReturnParam(input, VarName = "TS_XGBoost_CARMA_HolidayMovingAverages", Type = "character", Default = c("2","3"), Switch = Switch_XGBoost)

            # Lags, moving averages, and other rolling stats
            TS_XGBoost_CARMA_Lags <- RemixAutoML::ReturnParam(input, VarName = "TS_XGBoost_CARMA_Lags", Type = "numeric", Default = c(1L:10L), Switch = Switch_XGBoost)
            TS_XGBoost_CARMA_MovingAverages <- RemixAutoML::ReturnParam(input, VarName = "TS_XGBoost_CARMA_MovingAverages", Type = "numeric", Default = c(2L:10L), Switch = Switch_XGBoost)
            TS_XGBoost_CARMA_MovingSD <- RemixAutoML::ReturnParam(input, VarName = "TS_XGBoost_CARMA_MovingSD", Type = "numeric", Default = NULL, Switch = Switch_XGBoost)
            TS_XGBoost_CARMA_MovingSkew <- RemixAutoML::ReturnParam(input, VarName = "TS_XGBoost_CARMA_MovingSkew", Type = "numeric", Default = NULL, Switch = Switch_XGBoost)
            TS_XGBoost_CARMA_MovingKurt <- RemixAutoML::ReturnParam(input, VarName = "TS_XGBoost_CARMA_MovingKurt", Type = "numeric", Default = NULL, Switch = Switch_XGBoost)
            TS_XGBoost_CARMA_MovingQuantiles <- RemixAutoML::ReturnParam(input, VarName = "TS_XGBoost_CARMA_MovingQuantiles", Type = "numeric", Default = NULL, Switch = Switch_XGBoost)
            TS_XGBoost_CARMA_Quantiles_Selected <- RemixAutoML::ReturnParam(input, VarName = "TS_XGBoost_CARMA_Quantiles_Selected", Type = "character", Default = "NULL", Switch = Switch_XGBoost)
            TS_XGBoost_CARMA_Lags1 <- RemixAutoML::ReturnParam(input, VarName = "TS_XGBoost_CARMA_Lags1", Type = "numeric", Default = c(1L:5L), Switch = Switch_XGBoost)
            TS_XGBoost_CARMA_MovingAverages1 <- RemixAutoML::ReturnParam(input, VarName = "TS_XGBoost_CARMA_MovingAverages1", Type = "numeric", Default = c(2L:10L), Switch = Switch_XGBoost)
            TS_XGBoost_CARMA_MovingSD1 <- RemixAutoML::ReturnParam(input, VarName = "TS_XGBoost_CARMA_MovingSD1", Type = "numeric", Default = NULL, Switch = Switch_XGBoost)
            TS_XGBoost_CARMA_MovingSkew1 <- RemixAutoML::ReturnParam(input, VarName = "TS_XGBoost_CARMA_MovingSkew1", Type = "numeric", Default = NULL, Switch = Switch_XGBoost)
            TS_XGBoost_CARMA_MovingKurt1 <- RemixAutoML::ReturnParam(input, VarName = "TS_XGBoost_CARMA_MovingKurt1", Type = "numeric", Default = NULL, Switch = Switch_XGBoost)
            TS_XGBoost_CARMA_MovingQuantiles1 <- RemixAutoML::ReturnParam(input, VarName = "TS_XGBoost_CARMA_MovingQuantiles1", Type = "numeric", Default = NULL, Switch = Switch_XGBoost)
            TS_XGBoost_CARMA_HierarchGroups <- RemixAutoML::ReturnParam(input, VarName = "TS_XGBoost_CARMA_HierarchGroups", Type = "character", Default = "FALSE", Switch = Switch_XGBoost)

            # Bonus features
            TS_XGBoost_CARMA_AnomalyDetection_HighThreshold <- RemixAutoML::ReturnParam(input, VarName = "TS_XGBoost_CARMA_AnomalyDetection_HighThreshold", Type = "numeric", Default = 0, Switch = Switch_XGBoost)
            TS_XGBoost_CARMA_AnomalyDetection_LowThreshold <- RemixAutoML::ReturnParam(input, VarName = "TS_XGBoost_CARMA_AnomalyDetection_LowThreshold", Type = "numeric", Default = 0, Switch = Switch_XGBoost)
            TS_XGBoost_CARMA_Fourier <- RemixAutoML::ReturnParam(input, VarName = "TS_XGBoost_CARMA_Fourier", Type = "numeric", Default = 2L, Switch = Switch_XGBoost)
            TS_XGBoost_CARMA_TimeTrend <- RemixAutoML::ReturnParam(input, VarName = "TS_XGBoost_CARMA_TimeTrend", Type = "character", Default = "TRUE", Switch = Switch_XGBoost)
            TS_XGBoost_CARMA_DataTruncate <- RemixAutoML::ReturnParam(input, VarName = "TS_XGBoost_CARMA_DataTruncate", Type = "character", Default = "FALSE", Switch = Switch_XGBoost)

            # ML grid tuning args
            TS_XGBoost_CARMA_GridTune <- RemixAutoML::ReturnParam(input, VarName = "TS_XGBoost_CARMA_GridTune", Type = "character", Default = "FALSE", Switch = Switch_XGBoost)
            TS_XGBoost_CARMA_ModelCount <- RemixAutoML::ReturnParam(input, VarName = "TS_XGBoost_CARMA_ModelCount", Type = "numeric", Default = 5, Switch = Switch_XGBoost)
            TS_XGBoost_CARMA_PassInGrid <- RemixAutoML::ReturnParam(input, VarName = "TS_XGBoost_CARMA_PassInGrid", Type = "character", Default = "NULL", Switch = Switch_XGBoost)
            TS_XGBoost_CARMA_MaxRunsWithoutNewWinner <- RemixAutoML::ReturnParam(input, VarName = "TS_XGBoost_CARMA_MaxRunsWithoutNewWinner", Type = "numeric", Default = 25, Switch = Switch_XGBoost)
            TS_XGBoost_CARMA_MaxRunMinutes <- RemixAutoML::ReturnParam(input, VarName = "TS_XGBoost_CARMA_MaxRunMinutes", Type = "numeric", Default = 60*24, Switch = Switch_XGBoost)

            # ML loss functions
            TS_XGBoost_CARMA_EvalMetric <- RemixAutoML::ReturnParam(input, VarName = "TS_XGBoost_CARMA_EvalMetric", Type = "character", Default = "RMSE", Switch = Switch_XGBoost)
            TS_XGBoost_CARMA_LossFunction <- RemixAutoML::ReturnParam(input, VarName = "TS_XGBoost_CARMA_LossFunction", Type = "character", Default = "Squared Error", Switch = Switch_XGBoost)
            #TS_XGBoost_CARMA_EvalMetricValue <- RemixAutoML::ReturnParam(input, VarName = "TS_XGBoost_CARMA_EvalMetricValue", Type = "numeric", Default = 1, Switch = Switch_XGBoost)
            #TS_XGBoost_CARMA_LossFunctionValue <- RemixAutoML::ReturnParam(input, VarName = "TS_XGBoost_CARMA_LossFunctionValue", Type = "numeric", Default = 1, Switch = Switch_XGBoost)

            # ML tuning args
            TS_XGBoost_CARMA_NTrees <- RemixAutoML::ReturnParam(input, VarName = "TS_XGBoost_CARMA_NTrees", Type = "character", Default = 1000, Switch = Switch_XGBoost)
            TS_XGBoost_CARMA_Depth <- RemixAutoML::ReturnParam(input, VarName = "TS_XGBoost_CARMA_Depth", Type = "numeric", Default = 9, Switch = Switch_XGBoost)
            TS_XGBoost_CARMA_LearningRate <- RemixAutoML::ReturnParam(input, VarName = "TS_XGBoost_CARMA_LearningRate", Type = "numeric", Default = 0.30, Switch = Switch_XGBoost)
            TS_XGBoost_CARMA_MinChildWeight <- RemixAutoML::ReturnParam(input, VarName = "TS_XGBoost_CARMA_MinChildWeight", Type = "numeric", Default = 1, Switch = Switch_XGBoost)
            TS_XGBoost_CARMA_SubSample <- RemixAutoML::ReturnParam(input, VarName = "TS_XGBoost_CARMA_SubSample", Type = "numeric", Default = 1, Switch = Switch_XGBoost)
            TS_XGBoost_CARMA_ColSampleByTree <- RemixAutoML::ReturnParam(input, VarName = "TS_XGBoost_CARMA_ColSampleByTree", Type = "numeric", Default = 1, Switch = Switch_XGBoost)

            # N ----
            if(ncol(data) > 2) N <- data[, .N, by = c(eval(GroupVariableNames))][, max(N)] else N <- data[, .N]

            # LossFunction ----
            if(TS_XGBoost_CARMA_LossFunction == "Squared Error") {
              loss_function <- "reg:squarederror"
            } else if(TS_XGBoost_CARMA_LossFunction == "Squared Log Error") {
              loss_function <- "reg:squaredlogerror"
            } else if(TS_XGBoost_CARMA_LossFunction == "Pseudo Huber") {
              loss_function <- "reg:pseudohubererror"
            } else if(TS_XGBoost_CARMA_LossFunction == "Poisson") {
              loss_function <- "count:poisson"
            } else if(TS_XGBoost_CARMA_LossFunction == "Gamma") {
              loss_function <- "reg:gamma"
            } else if(TS_XGBoost_CARMA_LossFunction == "Tweedie") {
              loss_function <- "reg:tweedie"
            }

            # Time series features ----
            if(length(TS_TimeUnit) > 1L) {
              Defined_Lags <- list()
              Defined_MA <- list()
              Defined_SD <- list()
              Defined_Skew <- list()
              Defined_Kurt <- list()
              Defined_Percentile <- list()
              for(z in 1:2) {
                if(z == 1) {
                  if(!is.null(TS_XGBoost_CARMA_Lags)) Defined_Lags[[TS_TimeUnit[z]]] <- as.numeric(TS_XGBoost_CARMA_Lags) else Defined_Lags[[TS_TimeUnit[z]]] <- NULL
                  if(!is.null(TS_XGBoost_CARMA_MovingAverages)) Defined_MA[[TS_TimeUnit[z]]] <- as.numeric(TS_XGBoost_CARMA_MovingAverages) else Defined_MA[[TS_TimeUnit[z]]] <- NULL
                  if(!is.null(TS_XGBoost_CARMA_MovingSD)) Defined_SD[[TS_TimeUnit[z]]] <- as.numeric(TS_XGBoost_CARMA_MovingSD) else Defined_SD[[TS_TimeUnit[z]]] <- NULL
                  if(!is.null(TS_XGBoost_CARMA_MovingSkew)) Defined_Skew[[TS_TimeUnit[z]]] <- as.numeric(TS_XGBoost_CARMA_MovingSkew) else Defined_Skew[[TS_TimeUnit[z]]] <- NULL
                  if(!is.null(TS_XGBoost_CARMA_MovingKurt)) Defined_Kurt[[TS_TimeUnit[z]]] <- as.numeric(TS_XGBoost_CARMA_MovingKurt) else Defined_Kurt[[TS_TimeUnit[z]]] <- NULL
                  if(!is.null(TS_XGBoost_CARMA_MovingQuantiles)) Defined_Percentile[[TS_TimeUnit[z]]] <- as.numeric(TS_XGBoost_CARMA_MovingQuantiles) else Defined_Percentile[[TS_TimeUnit[z]]] <- NULL
                } else if(z == 2) {
                  if(!is.null(TS_XGBoost_CARMA_Lags)) Defined_Lags[[TS_TimeUnit[z]]] <- as.numeric(TS_XGBoost_CARMA_Lags1) else Defined_Lags[[TS_TimeUnit[z]]] <- NULL
                  if(!is.null(TS_XGBoost_CARMA_MovingAverages)) Defined_MA[[TS_TimeUnit[z]]] <- as.numeric(TS_XGBoost_CARMA_MovingAverages1) else Defined_MA[[TS_TimeUnit[z]]] <- NULL
                  if(!is.null(TS_XGBoost_CARMA_MovingSD)) Defined_SD[[TS_TimeUnit[z]]] <- as.numeric(TS_XGBoost_CARMA_MovingSD1) else Defined_SD[[TS_TimeUnit[z]]] <- NULL
                  if(!is.null(TS_XGBoost_CARMA_MovingSkew)) Defined_Skew[[TS_TimeUnit[z]]] <- as.numeric(TS_XGBoost_CARMA_MovingSkew1) else Defined_Skew[[TS_TimeUnit[z]]] <- NULL
                  if(!is.null(TS_XGBoost_CARMA_MovingKurt)) Defined_Kurt[[TS_TimeUnit[z]]] <- as.numeric(TS_XGBoost_CARMA_MovingKurt1) else Defined_Kurt[[TS_TimeUnit[z]]] <- NULL
                  if(!is.null(TS_XGBoost_CARMA_MovingQuantiles)) Defined_Percentile[[TS_TimeUnit[z]]] <- as.numeric(TS_XGBoost_CARMA_MovingQuantiles1) else Defined_Percentile[[TS_TimeUnit[z]]] <- NULL
                }
              }
            } else {
              if(!is.null(TS_XGBoost_CARMA_Lags)) Defined_Lags <- as.numeric(TS_XGBoost_CARMA_Lags) else Defined_Lags <- NULL
              if(!is.null(TS_XGBoost_CARMA_MovingAverages)) Defined_MA <- as.numeric(TS_XGBoost_CARMA_MovingAverages) else Defined_MA <- NULL
              if(!is.null(TS_XGBoost_CARMA_MovingSD)) Defined_SD <- as.numeric(TS_XGBoost_CARMA_MovingSD) else Defined_SD <- NULL
              if(!is.null(TS_XGBoost_CARMA_MovingSkew)) Defined_Skew <- as.numeric(TS_XGBoost_CARMA_MovingSkew) else Defined_Skew <- NULL
              if(!is.null(TS_XGBoost_CARMA_MovingKurt)) Defined_Kurt <- as.numeric(TS_XGBoost_CARMA_MovingKurt) else Defined_Kurt <- NULL
              if(!is.null(TS_XGBoost_CARMA_MovingQuantiles)) Defined_Percentile <- as.numeric(TS_XGBoost_CARMA_MovingQuantiles) else Defined_Percentile <- NULL
            }

            # Loop through model builds ----
            for(Runs in c("Baseline", "Forecast")) {

              # Copy data ----
              if(!is.null(GroupVariableNames)) {
                datax <- data.table::copy(data)
              } else {
                if(Runs == "Baseline") data.table::fwrite(data, file.path(ProjectList$DataFolderPath, "EvalData.csv"))
                data.table::setorderv(x = data, cols = eval(DateName), order = 1L)
                datax <- data[seq_len(.N - eval(TS_FCPeriods))]
              }

              # Copy xregs data ----
              if(exists("XREGS")) xregsx <- tryCatch({data.table::copy(xregs)}, error = function(x) NULL) else xregsx <- NULL

              # Define args for baseline and forecast runs ----
              if(Runs == "Baseline") {
                TOF <- FALSE
                NTrees <- TS_XGBoost_CARMA_NTrees
                Ratios <- c(1 - TS_HoldOutPeriods / N, TS_HoldOutPeriods / N)
              } else if(Runs == "Forecast") {
                TOF <- TRUE
                NTrees <- XGBoostResults$Model$niter
                Ratios <- NULL
              }

              # Debugging
              # print(paste0("TS_FCPeriods: ",TS_FCPeriods))
              # print(paste0("TS_FCPeriods: ",TOF))
              # print(paste0("TS_XGBoost_CARMA_TaskType: ",if(TS_XGBoost_CARMA_TaskType == "CPU") "hist" else "gpu_hist"))
              # print(paste0("NThreads: ",max(1L, parallel::detectCores()-2L)))
              # print(paste0("NonNegativePred: ",TS_XGBoost_CARMA_NonNegativePrep))
              # print(paste0("RoundPreds: ",TS_XGBoost_CARMA_RoundPreds))
              # print(paste0("TargetColumnName: ",TargetName))
              # print(paste0("DateColumnName: ",DateName))
              # print(paste0("HierarchGroups: ",if(!is.null(input$TS_XGBoost_CARMA_HierarchGroups) && input$TS_XGBoost_CARMA_HierarchGroups == "FALSE") NULL else GroupVariableNames))
              # print(paste0("GroupVariables: ",GroupVariableNames))
              # print(paste0("TimeUnit: ",TS_TimeUnit[1L]))
              # print(paste0("TimeGroups: ",TS_TimeUnit))
              # print(paste0("TargetTransformation: ",if(!any(TS_XGBoost_CARMA_Methods %chin% "Identity")) TRUE else FALSE))
              # print(paste0("Methods: ",TS_XGBoost_CARMA_Methods))
              # print(paste0("Difference: ",TS_XGBoost_CARMA_Difference))
              # print(paste0("CalendarVariables: ",TS_XGBoost_CARMA_CalendarVariables))
              # print(paste0("HolidayVariable: ",TS_XGBoost_CARMA_HolidayVariables))
              # print(paste0("HolidayLags: ",as.numeric(TS_XGBoost_CARMA_HolidayLags)))
              # print(paste0("HolidayMovingAverages: ",as.numeric(TS_XGBoost_CARMA_HolidayMovingAverages)))
              # print(paste0("Lags: ",Defined_Lags))
              # print(paste0("MA_Periods: ",Defined_MA))
              # print(paste0("SD_Periods: ",Defined_SD))
              # print(paste0("Skew_Periods: ",Defined_Skew))
              # print(paste0("Kurt_Periods: ",Defined_Kurt))
              # print(paste0("Quantile_Periods: ",Defined_Percentile))
              # print(paste0("Quantiles_Selected: ",TS_XGBoost_CARMA_Quantiles_Selected))
              # print(paste0("FourierTerms: ",TS_XGBoost_CARMA_Fourier))
              # print(paste0("TimeTrendVariable: ",TS_XGBoost_CARMA_TimeTrend))
              # print(paste0("ZeroPadSeries: ",NULL))
              # print(paste0("DataTruncate: ",as.logical(TS_XGBoost_CARMA_DataTruncate)))
              # print(paste0("SplitRatios: ",Ratios))
              # print(paste0("PartitionType: ","random"))
              # print(paste0("AnomalyDetection: ",if(TS_XGBoost_CARMA_AnomalyDetection_HighThreshold != 0 && TS_XGBoost_CARMA_AnomalyDetection_LowThreshold != 0) list("tstat_high" = TS_XGBoost_CARMA_AnomalyDetection_HighThreshold, "tstat_low" = TS_XGBoost_CARMA_AnomalyDetection_LowThreshold) else if(TS_XGBoost_CARMA_AnomalyDetection_HighThreshold != 0 && TS_XGBoost_CARMA_AnomalyDetection_LowThreshold == 0) list("tstat_high" = TS_XGBoost_CARMA_AnomalyDetection_HighThreshold, "tstat_low" = -99999) else if(TS_XGBoost_CARMA_AnomalyDetection_HighThreshold == 0 && TS_XGBoost_CARMA_AnomalyDetection_LowThreshold != 0) list("tstat_high" = 9999, "tstat_low" = TS_XGBoost_CARMA_AnomalyDetection_LowThreshold) else  NULL))
              # print(paste0("LossFunction: ",loss_function))
              # print(paste0("EvalMetric: ",TS_XGBoost_CARMA_EvalMetric))
              # print(paste0("PDFOutputPath: ",if(TS_CARMA_PDFOutputPath == "NULL") NULL else file.path(ProjectList[["DataFolderPath"]])))
              # print(paste0("SaveDataPath: ",if(TS_CARMA_SaveDataPath == "NULL") NULL else file.path(ProjectList[["DataFolderPath"]])))
              # print(paste0("GridTune: ",as.logical(TS_XGBoost_CARMA_GridTune)))
              # print(paste0("ModelCount: ",as.numeric(TS_XGBoost_CARMA_ModelCount)))
              # print(paste0("MaxRunsWithoutNewWinner: ",as.numeric(TS_XGBoost_CARMA_MaxRunsWithoutNewWinner)))
              # print(paste0("MaxRunMinutes: ",as.numeric(TS_XGBoost_CARMA_MaxRunMinutes)))
              # print(paste0("GridEvalMetric: ",if(TS_XGBoost_CARMA_EvalMetric == "RMSE") "mse" else "mae"))
              # print(paste0("NTrees: ",NTrees))
              # print(paste0("LearningRate: ",as.numeric(TS_XGBoost_CARMA_LearningRate)))
              # print(paste0("MaxDepth: ",as.numeric(TS_XGBoost_CARMA_Depth)))
              # print(paste0("MinChildWeight: ",as.numeric(TS_XGBoost_CARMA_MinChildWeight)))
              # print(paste0("SubSample: ",as.numeric(TS_XGBoost_CARMA_SubSample)))
              # print(paste0("ColSampleByTree: ",as.numeric(TS_XGBoost_CARMA_ColSampleByTree)))

              # Build model ----
              XGBoostResults <- RemixAutoML::AutoXGBoostCARMA(

                # Productionize
                FC_Periods = TS_FCPeriods,
                TrainOnFull = TOF,
                TreeMethod = if(TS_XGBoost_CARMA_TaskType == "CPU") "hist" else "gpu_hist",
                NThreads = max(1L, parallel::detectCores()-2L),
                Timer = TRUE,
                DebugMode = TRUE,

                # Data Artifacts
                data = datax,
                XREGS = xregsx,
                NonNegativePred = TS_XGBoost_CARMA_NonNegativePrep,
                RoundPreds = TS_XGBoost_CARMA_RoundPreds,
                TargetColumnName = TargetName,
                DateColumnName = DateName,
                HierarchGroups = if(!is.null(input$TS_XGBoost_CARMA_HierarchGroups) && input$TS_XGBoost_CARMA_HierarchGroups == "FALSE") NULL else GroupVariableNames,
                GroupVariables = GroupVariableNames,
                TimeUnit = TS_TimeUnit[1L],
                TimeGroups = TS_TimeUnit,

                # Target Transformations
                TargetTransformation = if(!any(TS_XGBoost_CARMA_Methods %chin% "Identity")) TRUE else FALSE,
                Methods = TS_XGBoost_CARMA_Methods,
                Difference = as.logical(TS_XGBoost_CARMA_Difference),

                # Calendar features
                CalendarVariables = TS_XGBoost_CARMA_CalendarVariables,
                HolidayVariable = TS_XGBoost_CARMA_HolidayVariables,
                HolidayLags = as.numeric(TS_XGBoost_CARMA_HolidayLags),
                HolidayMovingAverages = as.numeric(TS_XGBoost_CARMA_HolidayMovingAverages),

                # Lags, moving averages, and other rolling stats
                Lags = Defined_Lags,
                MA_Periods = Defined_MA,
                SD_Periods = Defined_SD,
                Skew_Periods = Defined_Skew,
                Kurt_Periods = Defined_Kurt,
                Quantile_Periods = Defined_Percentile,
                Quantiles_Selected = TS_XGBoost_CARMA_Quantiles_Selected,

                # Bonus features
                FourierTerms = TS_XGBoost_CARMA_Fourier,
                TimeTrendVariable = TS_XGBoost_CARMA_TimeTrend,
                ZeroPadSeries = NULL,
                DataTruncate = as.logical(TS_XGBoost_CARMA_DataTruncate),
                SplitRatios = Ratios,
                PartitionType = "random",
                AnomalyDetection = if(TS_XGBoost_CARMA_AnomalyDetection_HighThreshold != 0 && TS_XGBoost_CARMA_AnomalyDetection_LowThreshold != 0) list("tstat_high" = TS_XGBoost_CARMA_AnomalyDetection_HighThreshold, "tstat_low" = TS_XGBoost_CARMA_AnomalyDetection_LowThreshold) else if(TS_XGBoost_CARMA_AnomalyDetection_HighThreshold != 0 && TS_XGBoost_CARMA_AnomalyDetection_LowThreshold == 0) list("tstat_high" = TS_XGBoost_CARMA_AnomalyDetection_HighThreshold, "tstat_low" = -99999) else if(TS_XGBoost_CARMA_AnomalyDetection_HighThreshold == 0 && TS_XGBoost_CARMA_AnomalyDetection_LowThreshold != 0) list("tstat_high" = 9999, "tstat_low" = TS_XGBoost_CARMA_AnomalyDetection_LowThreshold) else  NULL,

                # ML evaluation args
                LossFunction = loss_function,
                EvalMetric = TS_XGBoost_CARMA_EvalMetric,
                PDFOutputPath = if(TS_CARMA_PDFOutputPath == "NULL") NULL else file.path(ProjectList[["DataFolderPath"]]),
                SaveDataPath = if(TS_CARMA_SaveDataPath == "NULL") NULL else file.path(ProjectList[["DataFolderPath"]]),

                # ML grid tuning
                GridTune = as.logical(TS_XGBoost_CARMA_GridTune),
                ModelCount = as.numeric(TS_XGBoost_CARMA_ModelCount),
                MaxRunsWithoutNewWinner = as.numeric(TS_XGBoost_CARMA_MaxRunsWithoutNewWinner),
                MaxRunMinutes = as.numeric(TS_XGBoost_CARMA_MaxRunMinutes),
                GridEvalMetric = if(TS_XGBoost_CARMA_EvalMetric == "RMSE") "mse" else "mae",

                # ML args
                NTrees = NTrees,
                LearningRate = as.numeric(TS_XGBoost_CARMA_LearningRate),
                MaxDepth = as.numeric(TS_XGBoost_CARMA_Depth),
                MinChildWeight = as.numeric(TS_XGBoost_CARMA_MinChildWeight),
                SubSample = as.numeric(TS_XGBoost_CARMA_SubSample),
                ColSampleByTree = as.numeric(TS_XGBoost_CARMA_ColSampleByTree))

              # Save data and model output ----
              if(Runs == "Baseline") {
                save(XGBoostResults, file = file.path(ProjectList[["MetaDataPath"]], paste0(TargetName, "_AutoXGBoostCARMA_ModelInsights.Rdata")))
              } else {
                Forecast <- XGBoostResults$Forecast
                tryCatch({data.table::fwrite(Forecast, file = file.path(ProjectList[["ModelsFolderPath"]], paste0(TargetName, "_AutoXGBoostCARMA_Forecast.csv")))}, error = function(x) print("Error saving"))
              }
            }
          }

          # H2oCARMA ----
          if("H2O-CARMA" %chin% ML_Models) {

            # Switch ----
            if(exists("Switch_CatBoost")) {
              Switch_H2O <<- TRUE
            } else {
              Switch_H2O <<- FALSE
            }

            # Modeling Parameters ----

            # General args
            TS_H2O_CARMA_EvalMetric <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_EvalMetric", Type = "character", Default = "RMSE", Switch = Switch_H2O)
            TS_H2O_CARMA_ExcludeAlgos <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_ExcludeAlgos", Type = "character", Default = "XGBoost", Switch = Switch_H2O)
            TS_H2O_CARMA_H2OModelSelection <- RemixAutoML::ReturnParam(input, VarName = "H2OModelSelection", Type = "character", Default = "DRF", Switch = Switch_H2O)
            TS_H2O_CARMA_TimeWeights <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_TimeWeights", Type = "numeric", Default = 1, Switch = Switch_H2O)
            TS_H2O_CARMA_Methods <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_Methods", Type = "character", Default = "Identity", Switch = Switch_H2O)
            TS_H2O_CARMA_NonNegativePred <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_NonNegativePred", Type = "character", Default = "FALSE", Switch = Switch_H2O)
            TS_H2O_CARMA_RoundPreds <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_RoundPreds", Type = "character", Default = "FALSE", Switch = Switch_H2O)
            TS_H2O_CARMA_NThreads <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_NThreads", Type = "numeric", Default = -1, Switch = Switch_H2O)
            TS_H2O_CARMA_MaxMemory <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_MaxMemory", Type = "character", Default = "28g", Switch = Switch_H2O)
            TS_H2O_CARMA_Difference <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_Difference", Type = "character", Default = "FALSE", Switch = Switch_H2O)
            TS_H2O_CARMA_CalendarVariables <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_CalendarVariables", Type = "character", Default = c("minute","hour","wday","mday","yday","week","wom","month","quarter"), Switch = Switch_H2O)
            TS_H2O_CARMA_HolidayVariables <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_HolidayVariables", Type = "character", Default = "USPublicHolidays", Switch = Switch_H2O)
            TS_H2O_CARMA_HolidayLags <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_HolidayLags", Type = "numeric", Default = c(1,2), Switch = Switch_H2O)
            TS_H2O_CARMA_HolidayMovingAverages <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_HolidayMovingAverages", Type = "numeric", Default = c(2,3), Switch = Switch_H2O)
            TS_H2O_CARMA_TimeTrend <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_TimeTrend", Type = "character", Default = "FALSE", Switch = Switch_H2O)
            TS_H2O_CARMA_DataTruncate <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_DataTruncate", Type = "character", Default = "FALSE", Switch = Switch_H2O)
            TS_H2O_CARMA_Fourier <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_Fourier", Type = "numeric", Default = 0, Switch = Switch_H2O)
            TS_H2O_CARMA_Quantiles_Selected <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_Quantiles_Selected", Type = "character", Default = NULL, Switch = Switch_H2O)
            TS_H2O_CARMA_Lags <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_Lags", Type = "numeric", Default = c(1:10), Switch = Switch_H2O)
            TS_H2O_CARMA_MovingAverages <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_MovingAverages", Type = "numeric", Default = c(2:10), Switch = Switch_H2O)
            TS_H2O_CARMA_MovingSD <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_MovingSD", Type = "numeric", Default = NULL, Switch = Switch_H2O)
            TS_H2O_CARMA_MovingSkew <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_MovingSkew", Type = "numeric", Default = NULL, Switch = Switch_H2O)
            TS_H2O_CARMA_MovingKurt <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_MovingKurt", Type = "numeric", Default = NULL, Switch = Switch_H2O)
            TS_H2O_CARMA_MovingQuantiles <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_MovingQuantiles", Type = "numeric", Default = NULL, Switch = Switch_H2O)
            TS_H2O_CARMA_Lags1 <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_Lags1", Type = "numeric", Default = c(1:10), Switch = Switch_H2O)
            TS_H2O_CARMA_MovingAverages1 <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_MovingAverages1", Type = "numeric", Default = c(2:10), Switch = Switch_H2O)
            TS_H2O_CARMA_MovingSD1 <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_MovingSD1", Type = "numeric", Default = NULL, Switch = Switch_H2O)
            TS_H2O_CARMA_MovingSkew1 <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_MovingSkew1", Type = "numeric", Default = NULL, Switch = Switch_H2O)
            TS_H2O_CARMA_MovingKurt1 <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_MovingKurt1", Type = "numeric", Default = NULL, Switch = Switch_H2O)
            TS_H2O_CARMA_MovingQuantiles1 <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_MovingQuantiles1", Type = "numeric", Default = NULL, Switch = Switch_H2O)
            TS_H2O_CARMA_GridTune <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_GridTune", Type = "character", Default = "FALSE", Switch = Switch_H2O)
            TS_H2O_CARMA_ModelCount <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_ModelCount", Type = "numeric", Default = 20, Switch = Switch_H2O)
            TS_H2O_CARMA_GridStrategy <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_GridStrategy", Type = "character", Default = "RandomDiscrete", Switch = Switch_H2O)
            TS_H2O_CARMA_MaxRuntimeSecs <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_MaxRuntimeSecs", Type = "numeric", Default = 60*60*24, Switch = Switch_H2O)
            TS_H2O_CARMA_StoppingRounds <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_StoppingRounds", Type = "numeric", Default = 10, Switch = Switch_H2O)
            TS_H2O_CARMA_NTrees <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_NTrees", Type = "numeric", Default = 1000, Switch = Switch_H2O)
            TS_H2O_CARMA_LearnRate <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_LearnRate", Type = "numeric", Default = 0.10, Switch = Switch_H2O)
            TS_H2O_CARMA_LearnRateAnnealing <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_LearnRateAnnealing", Type = "numeric", Default = 1, Switch = Switch_H2O)
            TS_H2O_CARMA_MaxDepth <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_MaxDepth", Type = "numeric", Default = 6, Switch = Switch_H2O)
            TS_H2O_CARMA_SampleRate <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_SampleRate", Type = "numeric", Default = 0.632, Switch = Switch_H2O)
            TS_H2O_CARMA_ColSampleRatePerTree <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_ColSampleRatePerTree", Type = "numeric", Default = 1, Switch = Switch_H2O)
            TS_H2O_CARMA_ColSampleRatePerTreeLevel <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_ColSampleRatePerTreeLevel", Type = "numeric", Default = 1, Switch = Switch_H2O)
            TS_H2O_CARMA_MinRows <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_MinRows", Type = "numeric", Default = 1, Switch = Switch_H2O)
            TS_H2O_CARMA_NBins <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_NBins", Type = "numeric", Default = 1024, Switch = Switch_H2O)
            TS_H2O_CARMA_NBinsCats <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_NBinsCats", Type = "numeric", Default = 1024, Switch = Switch_H2O)
            TS_H2O_CARMA_NBinsTopLevel <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_NBinsTopLevel", Type = "numeric", Default = 1024, Switch = Switch_H2O)
            TS_H2O_CARMA_HistogramType <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_HistogramType", Type = "character", Default = "AUTO", Switch = Switch_H2O)
            TS_H2O_CARMA_CategoricalEncoding <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_CategoricalEncoding", Type = "character", Default = "AUTO", Switch = Switch_H2O)
            TS_H2O_CARMA_MTries <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_MTries", Type = "numeric", Default = 1, Switch = Switch_H2O)
            TS_H2O_CARMA_ColSampleRate <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_ColSampleRate", Type = "numeric", Default = 1, Switch = Switch_H2O)
            TS_H2O_CARMA_RandomColumnNames <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_RandomColumnNames", Type = "character", Default = if(exists("SourceData")) names(SourceData) else NULL, Switch = Switch_H2O)
            TS_H2O_CARMA_InteractionColumns <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_InteractionColumns", Type = "character", Default = if(exists("SourceData")) names(SourceData) else NULL, Switch = Switch_H2O)
            TS_H2O_CARMA_Distribution <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_Distribution", Type = "character", Default = "gaussian", Switch = Switch_H2O)
            TS_H2O_CARMA_Link <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_Link", Type = "character", Default = "Identity", Switch = Switch_H2O)
            TS_H2O_CARMA_Methods <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_Solver", Type = "character", Default = "AUTO", Switch = Switch_H2O)
            TS_H2O_CARMA_Alpha <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_Alpha", Type = "numeric", Default = 0.5, Switch = Switch_H2O)
            TS_H2O_CARMA_Lambda <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_Lambda", Type = "numeric", Default = 1, Switch = Switch_H2O)
            TS_H2O_CARMA_LambdaSearch <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_LambdaSearch", Type = "character", Default = "FALSE", Switch = Switch_H2O)
            TS_H2O_CARMA_NLambdas <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_NLambdas", Type = "numeric", Default = 30, Switch = Switch_H2O)
            TS_H2O_CARMA_Standardize <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_Standardize", Type = "character", Default = "TRUE", Switch = Switch_H2O)
            TS_H2O_CARMA_RemoveCollinearColumns <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_RemoveCollinearColumns", Type = "character", Default = "TRUE", Switch = Switch_H2O)
            TS_H2O_CARMA_InterceptInclude <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_InterceptInclude", Type = "character", Default = "TRUE", Switch = Switch_H2O)
            TS_H2O_CARMA_NonNegativeCoefficients <- RemixAutoML::ReturnParam(input, VarName = "TS_H2O_CARMA_NonNegativeCoefficients", Type = "character", Default = "FALSE", Switch = Switch_H2O)

            # N ----
            if(ncol(data) > 2) N <- data[, .N, by = c(eval(GroupVariableNames))][, max(N)] else N <- data[, .N]

            # Time series features ----
            if(length(TS_TimeUnit) > 1L) {
              Defined_Lags <- list()
              Defined_MA <- list()
              Defined_SD <- list()
              Defined_Skew <- list()
              Defined_Kurt <- list()
              Defined_Percentile <- list()
              for(z in 1:2) {
                if(z == 1) {
                  if(!is.null(TS_H2O_CARMA_Lags)) Defined_Lags[[TS_TimeUnit[z]]] <- as.numeric(TS_H2O_CARMA_Lags) else Defined_Lags[[TS_TimeUnit[z]]] <- NULL
                  if(!is.null(TS_H2O_CARMA_MovingAverages)) Defined_MA[[TS_TimeUnit[z]]] <- as.numeric(TS_H2O_CARMA_MovingAverages) else Defined_MA[[TS_TimeUnit[z]]] <- NULL
                  if(!is.null(TS_H2O_CARMA_MovingSD)) Defined_SD[[TS_TimeUnit[z]]] <- as.numeric(TS_H2O_CARMA_MovingSD) else Defined_SD[[TS_TimeUnit[z]]] <- NULL
                  if(!is.null(TS_H2O_CARMA_MovingSkew)) Defined_Skew[[TS_TimeUnit[z]]] <- as.numeric(TS_H2O_CARMA_MovingSkew) else Defined_Skew[[TS_TimeUnit[z]]] <- NULL
                  if(!is.null(TS_H2O_CARMA_MovingKurt)) Defined_Kurt[[TS_TimeUnit[z]]] <- as.numeric(TS_H2O_CARMA_MovingKurt) else Defined_Kurt[[TS_TimeUnit[z]]] <- NULL
                  if(!is.null(TS_H2O_CARMA_MovingQuantiles)) Defined_Percentile[[TS_TimeUnit[z]]] <- as.numeric(TS_H2O_CARMA_MovingQuantiles) else Defined_Percentile[[TS_TimeUnit[z]]] <- NULL
                } else if(z == 2) {
                  if(!is.null(TS_H2O_CARMA_Lags)) Defined_Lags[[TS_TimeUnit[z]]] <- as.numeric(TS_H2O_CARMA_Lags1) else Defined_Lags[[TS_TimeUnit[z]]] <- NULL
                  if(!is.null(TS_H2O_CARMA_MovingAverages)) Defined_MA[[TS_TimeUnit[z]]] <- as.numeric(TS_H2O_CARMA_MovingAverages1) else Defined_MA[[TS_TimeUnit[z]]] <- NULL
                  if(!is.null(TS_H2O_CARMA_MovingSD)) Defined_SD[[TS_TimeUnit[z]]] <- as.numeric(TS_H2O_CARMA_MovingSD1) else Defined_SD[[TS_TimeUnit[z]]] <- NULL
                  if(!is.null(TS_H2O_CARMA_MovingSkew)) Defined_Skew[[TS_TimeUnit[z]]] <- as.numeric(TS_H2O_CARMA_MovingSkew1) else Defined_Skew[[TS_TimeUnit[z]]] <- NULL
                  if(!is.null(TS_H2O_CARMA_MovingKurt)) Defined_Kurt[[TS_TimeUnit[z]]] <- as.numeric(TS_H2O_CARMA_MovingKurt1) else Defined_Kurt[[TS_TimeUnit[z]]] <- NULL
                  if(!is.null(TS_H2O_CARMA_MovingQuantiles)) Defined_Percentile[[TS_TimeUnit[z]]] <- as.numeric(TS_H2O_CARMA_MovingQuantiles1) else Defined_Percentile[[TS_TimeUnit[z]]] <- NULL
                }
              }
            } else {
              if(!is.null(TS_H2O_CARMA_Lags)) Defined_Lags <- as.numeric(TS_H2O_CARMA_Lags) else Defined_Lags <- NULL
              if(!is.null(TS_H2O_CARMA_MovingAverages)) Defined_MA <- as.numeric(TS_H2O_CARMA_MovingAverages) else Defined_MA <- NULL
              if(!is.null(TS_H2O_CARMA_MovingSD)) Defined_SD <- as.numeric(TS_H2O_CARMA_MovingSD) else Defined_SD <- NULL
              if(!is.null(TS_H2O_CARMA_MovingSkew)) Defined_Skew <- as.numeric(TS_H2O_CARMA_MovingSkew) else Defined_Skew <- NULL
              if(!is.null(TS_H2O_CARMA_MovingKurt)) Defined_Kurt <- as.numeric(TS_H2O_CARMA_MovingKurt) else Defined_Kurt <- NULL
              if(!is.null(TS_H2O_CARMA_MovingQuantiles)) Defined_Percentile <- as.numeric(TS_H2O_CARMA_MovingQuantiles) else Defined_Percentile <- NULL
            }

            # Loop through model builds ----
            for(Runs in c("Baseline", "Forecast")) {

              # Copy data ----
              if(!is.null(GroupVariableNames)) {
                datax <- data.table::copy(data)
              } else {
                if(Runs == "Baseline") data.table::fwrite(data, file.path(ProjectList$DataFolderPath, "EvalData.csv"))
                data.table::setorderv(x = data, cols = eval(DateName), order = 1L)
                datax <- data[seq_len(.N - eval(TS_FCPeriods))]
              }

              # Copy xregs data ----
              if(exists("XREGS")) xregsx <- tryCatch({data.table::copy(xregs)}, error = function(x) NULL) else xregsx <- NULL

              # Define args for baseline and forecast runs ----
              if(Runs == "Baseline") {
                TOF <- FALSE
                NTrees <- TS_H2O_CARMA_NTrees
                Ratios <- c(1 - TS_HoldOutPeriods / N, TS_HoldOutPeriods / N)
              } else if(Runs == "Forecast") {
                TOF <- TRUE
                NTrees <- H2OResults$Model$tree_count
                Ratios <- NULL
              }

              # Build model ----
              H2OResults <- RemixAutoML::AutoH2OCARMA(

                # Data Artifacts
                AlgoType = TS_H2O_CARMA_H2OModelSelection,
                ExcludeAlgos = TS_H2O_CARMA_ExcludeAlgos,
                data = datax,

                # Data args
                TargetColumnName = TargetName,
                DateColumnName = DateName,
                HierarchGroups = if(!is.null(input$TS_H2o_CARMA_HierarchGroups) && input$TS_H2o_CARMA_HierarchGroups == "FALSE") NULL else GroupVariableNames,
                GroupVariables = GroupVariableNames,
                TimeUnit = TS_TimeUnit[1L],
                TimeGroups = TS_TimeUnit,

                # Data Wrangling Features
                SplitRatios = Ratios,
                PartitionType = "random",

                # Production args
                FC_Periods = TS_FCPeriods,
                TrainOnFull = TOF,
                MaxMem = TS_H2O_CARMA_MaxMemory,
                NThreads = TS_H2O_CARMA_NThreads,
                PDFOutputPath = if(TS_CARMA_PDFOutputPath == "NULL") NULL else file.path(ProjectList[["DataFolderPath"]]),
                SaveDataPath = if(TS_CARMA_SaveDataPath == "NULL") NULL else file.path(ProjectList[["DataFolderPath"]]),
                Timer = TRUE,
                DebugMode = TRUE,

                # Target Transformations
                TargetTransformation = if(!all(TS_H2O_CARMA_Methods %chin% "Identity")) TRUE else FALSE,
                Methods = TS_H2O_CARMA_Methods,
                Difference = as.logical(TS_H2O_CARMA_Difference),
                NonNegativePred = as.logical(TS_H2O_CARMA_NonNegativePrep),
                RoundPreds = as.logical(TS_H2O_CARMA_RoundPreds),

                # Calendar features
                CalendarVariables = TS_H2O_CARMA_CalendarVariables,
                HolidayVariable = TS_H2O_CARMA_HolidayVariables,
                TimeTrendVariable = as.logical(TS_H2O_CARMA_TimeTrend),
                HolidayLags = TS_H2O_CARMA_HolidayLags,
                HolidayMovingAverages = TS_H2O_CARMA_HolidayMovingAverages,

                # Time series features
                Lags = Defined_Lags,
                MA_Periods = Defined_MA,
                SD_Periods = Defined_SD,
                Skew_Periods = Defined_Skew,
                Kurt_Periods = Defined_Kurt,
                Quantile_Periods = Defined_Percentile,
                Quantiles_Selected = TS_H2O_CARMA_Quantiles_Selected,

                # Bonus Features
                AnomalyDetection = if(TS_H2O_CARMA_AnomalyDetection_HighThreshold != 0 && TS_H2O_CARMA_AnomalyDetection_LowThreshold != 0) list("tstat_high" = TS_H2O_CARMA_AnomalyDetection_HighThreshold, "tstat_low" = TS_H2O_CARMA_AnomalyDetection_LowThreshold) else if(TS_H2O_CARMA_AnomalyDetection_HighThreshold != 0 && TS_H2O_CARMA_AnomalyDetection_LowThreshold == 0) list("tstat_high" = TS_H2O_CARMA_AnomalyDetection_HighThreshold, "tstat_low" = -99999) else if(TS_H2O_CARMA_AnomalyDetection_HighThreshold == 0 && TS_H2O_CARMA_AnomalyDetection_LowThreshold != 0) list("tstat_high" = 9999, "tstat_low" = TS_H2O_CARMA_AnomalyDetection_LowThreshold) else  NULL,
                XREGS = xregsx,
                FourierTerms = as.numeric(TS_H2O_CARMA_Fourier),
                ZeroPadSeries = NULL,
                DataTruncate = as.logical(TS_H2O_CARMA_DataTruncate),

                # ML evaluation args
                EvalMetric = "RMSE",
                NumOfParDepPlots = TS_CARMA_NumParDepPlots,

                # ML grid tuning args
                GridTune = as.logical(TS_H2O_CARMA_GridTune),
                GridStrategy = TS_H2O_CARMA_GridStrategy,
                ModelCount = TS_H2O_CARMA_ModelCount,
                MaxRuntimeSecs = TS_H2O_CARMA_MaxRuntimeSecs,
                StoppingRounds = TS_H2O_CARMA_StoppingRounds,

                # ML Args
                NTrees = NTrees,
                MaxDepth = TS_H2O_CARMA_MaxDepth,
                SampleRate = TS_H2O_CARMA_SampleRate,
                MTries = TS_H2O_CARMA_MTries,
                ColSampleRatePerTree = TS_H2O_CARMA_ColSampleRatePerTree,
                ColSampleRatePerTreeLevel = TS_H2O_CARMA_ColSampleRatePerTreeLevel,
                MinRows = TS_H2O_CARMA_MinRows,
                NBins = TS_H2O_CARMA_NBins,
                NBinsCats = TS_H2O_CARMA_NBinsCats,
                NBinsTopLevel = TS_H2O_CARMA_NBinsTopLevel,
                HistogramType = TS_H2O_CARMA_HistogramType,
                CategoricalEncoding = TS_H2O_CARMA_CategoricalEncoding,
                RandomColNumbers = TS_H2O_CARMA_RandomColumnNames,
                InteractionColNumbers = TS_H2O_CARMA_InteractionColumns,
                WeightsColumn = TS_H2O_CARMA_WeightsColumn,

                # ML args
                Distribution = TS_H2O_CARMA_Distribution,
                Link = TS_H2O_CARMA_Link,
                RandomDistribution = TS_H2O_CARMA_RandomDistribution,
                RandomLink = TS_H2O_CARMA_RandomLink,
                Solver = TS_H2O_CARMA_Solver,
                Alpha = TS_H2O_CARMA_Alpha,
                Lambda = TS_H2O_CARMA_Lambda,
                LambdaSearch = as.logical(TS_H2O_CARMA_LambdaSearch),
                NLambdas = TS_H2O_CARMA_NLambdas,
                Standardize = as.logical(TS_H2O_CARMA_Standardize),
                RemoveCollinearColumns = as.logical(TS_H2O_CARMA_RemoveCollinearColumns),
                InterceptInclude = as.logical(TS_H2O_CARMA_InterceptInclude),
                NonNegativeCoefficients = as.logical(TS_H2O_CARMA_NonNegativeCoefficients))

              # Save data and model output ----
              if(Runs == "Baseline") {
                save(H2OResults, file = file.path(ProjectList[["MetaDataPath"]], paste0(TargetName, "_AutoH2OCARMA_ModelInsights.Rdata")))
              } else {
                Forecast <- H2OResults$Forecast
                tryCatch({data.table::fwrite(Forecast, file = file.path(ProjectList[["ModelsFolderPath"]], paste0(TargetName, "_AutoH2OCARMA_Forecast.csv")))}, error = function(x) print("Error saving"))
              }
            }

            # Update Progress Bar ----
            incProgress(amount = 1/n, message = "H2O-CARMA Complete")
          }

          # Update Progress Bar ----
          incProgress(amount = 1/n, message = "XGBoost-CARMA Complete")

          # Notify user ----
          shinyWidgets::sendSweetAlert(session,title = NULL, text = "Model building is complete!", type = "success", btn_labels = "Ok", btn_colors = "green", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "60%")
        }

        # Clear memory ----
        gc()
      })
    })

    # Next and Previous Buttons ----
    shiny::observeEvent(input$link_to_panel_forecasting_modeling, {
      shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "panel_forecasting_modeling")
    })
    shiny::observeEvent(input$link_to_panel_forecasting_modeling_1, {
      shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "panel_forecasting_modeling")
    })

  # . ----

  # Panel Forecasting - MODEL EVALUATION ----
    # UI Select Target, Models, Group Variables, Group Levels ----
    output$TSEval_timeSeriesTarget <- shiny::renderUI({
      if(exists("ProjectList")) {
        tryCatch({
          if(!is.null(ProjectList[["TS_timeSeriesTarget"]])) {
            shinyWidgets::pickerInput(inputId = "TSEval_timeSeriesTarget", label = "Select Target Variable", choices = ProjectList[["TS_timeSeriesTarget"]], selected = ProjectList[["TS_timeSeriesTarget"]][[1L]],
                        options = list(`actions-box` = TRUE, size = 10L, `selected-text-format` = "count > 1"), multiple = TRUE)
          } else {
            shinyWidgets::pickerInput(inputId = "TSEval_timeSeriesTarget", label = "Select Target Variable", choices = names(SourceData)[which(!sapply(SourceData,class) %chin% c("character","factor","Date","POSIXct","POSIXt"))], selected = names(SourceData)[which(!sapply(SourceData,class) %chin% c("character","factor","Date","POSIXct","POSIXt"))][1],
                        options = list(`actions-box` = TRUE, size = 10L, `selected-text-format` = "count > 1"), multiple = TRUE)
          }}, error = function(x) shinyWidgets::pickerInput(inputId = "TSEval_timeSeriesTarget", label = "Select Target Variable", choices = "!! No data is loaded", selected = "!! No data is loaded",
                                              options = list(`actions-box` = TRUE, size = 10L, `selected-text-format` = "count > 1"), multiple = TRUE))
      } else {
        tryCatch({shinyWidgets::pickerInput(inputId = "TSEval_timeSeriesTarget", label = "Select Target Variable", choices = names(SourceData)[which(!sapply(SourceData,class) %chin% c("character","factor","Date","POSIXct","POSIXt"))][[1]], selected = names(SourceData)[which(!sapply(SourceData,class) %chin% c("character","factor","Date","POSIXct","POSIXt"))][1],
                    options = list(`actions-box` = TRUE, size = 10L, `selected-text-format` = "count > 1"), multiple = TRUE)},
                 error = function(x) shinyWidgets::pickerInput(inputId = "TSEval_timeSeriesTarget", label = "Select Target Variable", choices = "!! No data is loaded", selected = "!! No data is loaded",
                                                 options = list(`actions-box` = TRUE, size = 10L, `selected-text-format` = "count > 1"), multiple = TRUE))
      }
    })
    output$TSEval_timeSeriesTarget2 <- shiny::renderUI({
      if(exists("ProjectList")) {
        tryCatch({
          if(!is.null(ProjectList[["TS_timeSeriesTarget"]])) {
            shinyWidgets::pickerInput(inputId = "TSEval_timeSeriesTarget2", label = "Select Target Variable", choices = ProjectList[["TS_timeSeriesTarget"]], selected = ProjectList[["TS_timeSeriesTarget"]][[1L]],
                        options = list(`actions-box` = TRUE, size = 10L, `selected-text-format` = "count > 1"), multiple = TRUE)
          } else {
            shinyWidgets::pickerInput(inputId = "TSEval_timeSeriesTarget2", label = "Select Target Variable", choices = names(SourceData)[which(!sapply(SourceData,class) %chin% c("character","factor","Date","POSIXct","POSIXt"))], selected = names(SourceData)[which(!sapply(SourceData,class) %chin% c("character","factor","Date","POSIXct","POSIXt"))][1],
                        options = list(`actions-box` = TRUE, size = 10L, `selected-text-format` = "count > 1"), multiple = TRUE)
          }}, error = function(x) shinyWidgets::pickerInput(inputId = "TSEval_timeSeriesTarget2", label = "Select Target Variable", choices = "!! No data is loaded", selected = "!! No data is loaded",
                                              options = list(`actions-box` = TRUE, size = 10L, `selected-text-format` = "count > 1"), multiple = TRUE))
      } else {
        tryCatch({shinyWidgets::pickerInput(inputId = "TSEval_timeSeriesTarget2", label = "Select Target Variable", choices = names(SourceData)[which(!sapply(SourceData,class) %chin% c("character","factor","Date","POSIXct","POSIXt"))][[1]], selected = names(SourceData)[which(!sapply(SourceData,class) %chin% c("character","factor","Date","POSIXct","POSIXt"))][1],
                    options = list(`actions-box` = TRUE, size = 10L, `selected-text-format` = "count > 1"), multiple = TRUE)},
                 error = function(x) shinyWidgets::pickerInput(inputId = "TSEval_timeSeriesTarget2", label = "Select Target Variable", choices = "!! No data is loaded", selected = "!! No data is loaded",
                                                 options = list(`actions-box` = TRUE, size = 10L, `selected-text-format` = "count > 1"), multiple = TRUE))
      }
    })
    output$TS_ModelID <- shiny::renderUI({
      if(exists("ProjectList")) {
        tryCatch({
          if(!is.null(ProjectList[["TS_ModelID"]])) {
            shinyWidgets::pickerInput(inputId = "TS_ModelID", label = "Select Model", choices = unique(c(ProjectList[["ML_Models"]])), selected = ProjectList[["TS_ModelID"]][[1L]],
                          options = list(`actions-box` = TRUE, size = 10L, `selected-text-format` = "count > 1"), multiple = FALSE)
          } else {
            shinyWidgets::pickerInput(inputId = "TS_ModelID", label = "Select Model", choices = unique(c(input$TSMLModelsSelection)), selected = "", options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 1"), multiple = FALSE)
          }}, error = function(x) shinyWidgets::pickerInput(inputId = "TS_ModelID", label = "Select Model", choices = "!! No project is loaded", selected = "!! No project is loaded", options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 1"), multiple = FALSE))
      } else {
        shinyWidgets::pickerInput(inputId = "TS_ModelID", label = "Select Model", choices = "!! No project is loaded", selected = "!! No project is loaded" , options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 1"), multiple = FALSE)
      }
    })
    output$TSEval_timeSeriesGroupVars <- shiny::renderUI({
      if(exists("ProjectList")) {
        tryCatch({
          if(!is.null(ProjectList[["TS_timeSeriesGroupVars"]])) {
            shinyWidgets::pickerInput(inputId = "TSEval_timeSeriesGroupVars", label = "Select Group Variables", choices = names(SourceData), selected = ProjectList[["TS_timeSeriesGroupVars"]],
                        options = list(`actions-box` = TRUE, size = 10L, `selected-text-format` = "count > 1"), multiple = TRUE)
          } else {
            shinyWidgets::pickerInput(inputId = "TSEval_timeSeriesGroupVars", label = "Select Group Variables", choices = names(SourceData), selected = "",
                        options = list(`actions-box` = TRUE, size = 10L, `selected-text-format` = "count > 1"), multiple = TRUE)
          }}, error = function(x) shinyWidgets::pickerInput(inputId = "TSEval_timeSeriesGroupVars", label = "Select Group Variables", choices = "!! No data is loaded", selected = "!! No data is loaded",
                                              options = list(`actions-box` = TRUE, size = 10L, `selected-text-format` = "count > 1"), multiple = TRUE))
      } else {
        tryCatch({shinyWidgets::pickerInput(inputId = "TSEval_timeSeriesGroupVars", label = "Select Group Variables", choices = names(SourceData), selected = "",
                    options = list(`actions-box` = TRUE, size = 10L, `selected-text-format` = "count > 1"), multiple = TRUE)},
                 error = function(x) shinyWidgets::pickerInput(inputId = "TSEval_timeSeriesGroupVars", label = "Select Group Variables", choices = "!! No data is loaded", selected = "!! No data is loaded",
                                                 options = list(`actions-box` = TRUE, size = 10L, `selected-text-format` = "count > 1"), multiple = TRUE))
      }
    })
    output$TSEval_timeSeriesGroupVars2 <- shiny::renderUI({
      if(exists("ProjectList")) {
        tryCatch({
          if(!is.null(ProjectList[["TS_timeSeriesGroupVars"]])) {
            shinyWidgets::pickerInput(inputId = "TSEval_timeSeriesGroupVars2", label = "Select Group Variables", choices = names(SourceData), selected = ProjectList[["TS_timeSeriesGroupVars"]],
                        options = list(`actions-box` = TRUE, size = 10L, `selected-text-format` = "count > 1"), multiple = TRUE)
          } else {
            shinyWidgets::pickerInput(inputId = "TSEval_timeSeriesGroupVars2", label = "Select Group Variables", choices = names(SourceData), selected = "",
                        options = list(`actions-box` = TRUE, size = 10L, `selected-text-format` = "count > 1"), multiple = TRUE)
          }}, error = function(x) shinyWidgets::pickerInput(inputId = "TSEval_timeSeriesGroupVars2", label = "Select Group Variables", choices = "!! No data is loaded", selected = "!! No data is loaded",
                                              options = list(`actions-box` = TRUE, size = 10L, `selected-text-format` = "count > 1"), multiple = TRUE))
      } else {
        tryCatch({shinyWidgets::pickerInput(inputId = "TSEval_timeSeriesGroupVars2", label = "Select Group Variables", choices = names(SourceData), selected = "",
                    options = list(`actions-box` = TRUE, size = 10L, `selected-text-format` = "count > 1"), multiple = TRUE)},
                 error = function(x) shinyWidgets::pickerInput(inputId = "TSEval_timeSeriesGroupVars2", label = "Select Group Variables", choices = "!! No data is loaded", selected = "!! No data is loaded",
                                                 options = list(`actions-box` = TRUE, size = 10L, `selected-text-format` = "count > 1"), multiple = TRUE))
      }
    })
    output$TS_Group1Levels2 <- shiny::renderUI({
      if(exists("SourceData")) {
        if(!is.null(input$TSEval_timeSeriesGroupVars2)) {
          if(length(input$TSEval_timeSeriesGroupVars2) >= 1L) {
            shinyWidgets::pickerInput(inputId = "TS_Group1Levels2", label = paste0(input$TSEval_timeSeriesGroupVars2[[1L]]," Levels"),
                        choices = c(as.character(unique(SourceData[[eval(input$TSEval_timeSeriesGroupVars2[[1L]])]]))), selected = NULL,
                        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 1"), multiple = TRUE, width = "100%")
          } else {
            shinyWidgets::pickerInput(inputId = "TS_Group1Levels2", label = "< N/A >",
                        choices = NULL, selected = NULL, multiple = TRUE, width = "100%")
          }
        } else {
          shinyWidgets::pickerInput(inputId = "TS_Group1Levels2", label = "< N/A >",
                      choices = NULL, selected = NULL, multiple = TRUE, width = "100%")
        }
      } else {
        shinyWidgets::pickerInput(inputId = "TS_Group1Levels2", label = "< N/A >",
                    choices = NULL, selected = NULL, multiple = TRUE, width = "100%")
      }
    })
    output$TS_Group2Levels2 <- shiny::renderUI({
      if(exists("SourceData")) {
        if(!is.null(input$TSEval_timeSeriesGroupVars2)) {
          if(length(input$TSEval_timeSeriesGroupVars2) >= 2L) {
            shinyWidgets::pickerInput(inputId = "TS_Group2Levels2", label = paste0(input$TSEval_timeSeriesGroupVars2[[2L]]," Levels"),
                        choices = c(as.character(unique(SourceData[[eval(input$TSEval_timeSeriesGroupVars2[[2L]])]]))), selected = NULL,
                        options = list(`actions-box` = TRUE, size = 10L, `selected-text-format` = "count > 1"), multiple = TRUE, width = "100%")
          } else {
            shinyWidgets::pickerInput(inputId = "TS_Group2Levels2", label = "< N/A >",
                        choices = NULL, selected = NULL, multiple = TRUE, width = "100%")
          }
        } else {
          shinyWidgets::pickerInput(inputId = "TS_Group2Levels2", label = "< N/A >",
                      choices = NULL, selected = NULL, multiple = TRUE, width = "100%")
        }
      } else {
        shinyWidgets::pickerInput(inputId = "TS_Group2Levels2", label = "< N/A >",
                    choices = NULL, selected = NULL, multiple = TRUE, width = "100%")
      }
    })
    output$TS_Group3Levels2 <- shiny::renderUI({
      if(exists("SourceData")) {
        if(!is.null(input$TSEval_timeSeriesGroupVars2)) {
          if(length(input$TSEval_timeSeriesGroupVars2) >= 3L) {
            shinyWidgets::pickerInput(inputId = "TS_Group3Levels2", label = paste0(input$TSEval_timeSeriesGroupVars2[[3]]," Levels"),
                        choices = c(as.character(unique(SourceData[[eval(input$TSEval_timeSeriesGroupVars2[[3L]])]]))), selected = NULL,
                        options = list(`actions-box` = TRUE, size = 10L, `selected-text-format` = "count > 1"), multiple = TRUE, width = "100%")
          } else {
            shinyWidgets::pickerInput(inputId = "TS_Group3Levels2", label = "< N/A >",
                        choices = NULL, selected = NULL, multiple = TRUE, width = "100%")
          }
        } else {
          shinyWidgets::pickerInput(inputId = "TS_Group3Levels2", label = "< N/A >",
                      choices = NULL, selected = NULL, multiple = TRUE, width = "100%")
        }
      } else {
        shinyWidgets::pickerInput(inputId = "TS_Group3Levels2", label = "< N/A >",
                    choices = NULL, selected = NULL, multiple = TRUE, width = "100%")
      }
    })
    output$Evaluate <- shiny::renderUI({
      shinyWidgets::pickerInput(inputId = "Evaluate", label = "TRUE for Backtesting Results", choices = c("TRUE","FALSE"), selected = "FALSE", options = list(`actions-box` = TRUE, size = 10L, `selected-text-format` = "count > 1"), multiple = FALSE, width = "100%")
    })

    # Boxes ----
    output$FC_TickMarksX <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "FC_TickMarksX", Label = "Tick marks x-axis", Choices = c("1 year","1 day","3 day","1 week","2 week","1 month","3 month","6 month","2 year","5 year","10 year","1 minute","15 minutes","30 minutes","1 hour","3 hour","6 hour","12 hour"), SelectedDefault = "1 year", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
    })
    output$FC_LineWidth <- shiny::renderUI({
      RemixAutoML::NumericInput(InputID = "FC_LineWidth", Label = "Line Width", Min = 0.10, Max = 5, Value = 0.50, Step = 0.10)
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
    output$FC_ForecastLineColor <- shiny::renderUI({
      RemixAutoML::PickerInput(InputID = "FC_ForecastLineColor", Label = "Actuals line color", Choices = grDevices::colors(), SelectedDefault = "red", Size = 10, SelectedText = "count > 1", Multiple = FALSE, ActionBox = TRUE)
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

    # Reset Plot Settings ----
    shiny::observeEvent(eventExpr = input$FC_ResetPlotSettings, {

      # Args ----

      # TS_AggregateFunction
      shinyWidgets::updatePickerInput(session = session, inputId = "FC_AggregateFunction", label = "Aggregation Method for 'other'", choices = c("mean","sum"), selected = "sum")
      ProjectList[["FC_AggregateFunction"]] <<- input$FC_AggregateFunction

      # TS_LegendTextColor
      shinyWidgets::updatePickerInput(session = session, inputId = "FC_LegendTextColor", label = "Legend text color", choices = grDevices::colors(), selected = "darkblue")
      ProjectList[["FC_LegendTextColor"]] <<- input$FC_LegendTextColor

      # TS_LegendTextSize
      shiny::updateNumericInput(session = session, inputId = "FC_LegendTextSize", label = "Legend text size", value = 10, min = 1, max = 48, step = 2)
      ProjectList[["FC_LegendTextSize"]] <<- input$FC_LegendTextSize

      # Line Color
      shinyWidgets::updatePickerInput(session = session, inputId = "FC_LineColor", label = "Line Color", choices = grDevices::colors(), selected = "blue")
      ProjectList[["FC_LineColor"]] <<- input$FC_LineColor

      # TS_LineWidth
      shiny::updateNumericInput(session = session, inputId = "FC_LineWidth", label = "Line size", value = 0.5, min = 0.10, max = 5, step = 0.10)
      ProjectList[["FC_LineWidth"]] <<- input$FC_LineWidth

      # TS_TextSize
      shiny::updateNumericInput(session = session, inputId = "FC_TextSize", label = "Text size", value = 18, min = 1, max = 50, step = 1)
      ProjectList[["FC_TextSize"]] <<- input$FC_TextSize

      # TS_NumberGroupsDisplay
      shiny::updateNumericInput(session = session, inputId = "FC_NumberGroupsDisplay", label = "Lines to display", value = 5, min = 1, max = 50, step = 1)
      ProjectList[["FC_NumberGroupsDisplay"]] <<- input$FC_NumberGroupsDisplay

      # TickMarksX
      # shinyWidgets::updatePickerInput(session = session, inputId = "FC_TickMarksX", label = "Tick marks X-Axis",
      #                   choices = c("1 year","1 day","3 day","1 week","2 week","1 month","3 month","6 month","2 year","5 year","10 year","1 minute","15 minutes","30 minutes","1 hour","3 hour","6 hour","12 hour"),
      #                   selected = "1 year")
      # ProjectList[["FC_TickMarksX"]] <<- input$FC_TickMarksX

      # TS_LegendPosition
      shinyWidgets::updatePickerInput(session = session, inputId = "FC_LegendPosition", label = "Legend position", choices = c("bottom","top","left","right"), selected = "bottom")
      ProjectList[["FC_LegendPosition"]] <<- input$FC_LegendPosition

      # TS_AngleX
      shiny::updateNumericInput(session = session, inputId = "FC_AngleX", label = "X-axis text angle", value = 35, min = 0, max = 360, step = 5)
      ProjectList[["FC_AngleX"]] <<- input$FC_AngleX

      # Y Axis angle
      shiny::updateNumericInput(session = session, inputId = "FC_AngleY", label = "X-axis text angle", value = 0, min = 0, max = 360, step = 5)
      ProjectList[["FC_AngleY"]] <<- input$FC_AngleY

      # TS_ChartColor
      shinyWidgets::updatePickerInput(session = session, inputId = "FC_ChartColor", label = "Chart color", choices = grDevices::colors(), selected = "lightsteelblue1")
      ProjectList[["FC_ChartColor"]] <<- input$FC_ChartColor

      # TS_BorderColor
      shinyWidgets::updatePickerInput(session = session, inputId = "FC_BorderColor", label = "Border color", choices = grDevices::colors(), selected = "darkblue")
      ProjectList[["FC_BorderColor"]] <<- input$FC_BorderColor

      # TS_TextColor
      shinyWidgets::updatePickerInput(session = session, inputId = "FC_TextColor", label = "Text color", choices = grDevices::colors(), selected = "darkblue")
      ProjectList[["FC_TextColor"]] <<- input$FC_TextColor

      # TS_GridColor
      shinyWidgets::updatePickerInput(session = session, inputId = "FC_GridColor", label = "Grid lines color", choices = grDevices::colors(), selected = "white")
      ProjectList[["FC_GridColor"]] <<- input$FC_GridColor

      # TS_BackGroundColor
      shinyWidgets::updatePickerInput(session = session, inputId = "FC_BackGroundColor", label = "Background color", choices = grDevices::colors(), selected = "gray95")
      ProjectList[["FC_BackGroundColor"]] <<- input$FC_BackGroundColor

      # TS_OtherGroups
      shinyWidgets::updatePickerInput(session = session, inputId = "FC_OtherGroups", label = "Show Other Groups", choices = c("FALSE","TRUE"), selected = "FALSE")
      ProjectList[["FC_OtherGroups"]] <<- input$FC_OtherGroups

      # Group Variable----
      a <- character(0)
      if(!identical(a,as.character(input$TSEval_timeSeriesGroupVars2))) {
        groupvariables <<- as.character(input$TSEval_timeSeriesGroupVars2)
      } else {
        groupvariables <<- NULL
      }

      # Build plots ----
      output$TS_ForecastPlot <- shiny::renderPlot({

        # Update theme
        if(exists("TimeSeriesPlotObject")) {
          TimeSeriesPlotObject <- TimeSeriesPlotObject + RemixAutoML::ChartTheme(
            Size = 12,
            AngleX = 35,
            AngleY = 0,
            ChartColor = "lightsteelblue1",
            BorderColor = "darkblue",
            TextColor = "darkblue",
            GridColor = "white",
            BackGroundColor = "gray95",
            LegendPosition = "bottom")
          TimeSeriesPlotObject
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
        for(tsmodels in sort(x = unique(c(ProjectList[["ML_Models"]])), decreasing = FALSE)) {

          # Catboost ----
          if(tsmodels == "CatBoost-CARMA") {

            # Increment Counter ----
            Counter <- Counter + 1L

            # Load FC data ----
            FCData <- tryCatch({data.table::fread(file.path(ProjectList$ModelsFolderPath, paste0(ProjectList$TS_timeSeriesTarget, "_AutoCatBoostCARMA_Forecast.csv")))}, error = function(x) NULL)
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
            EvalDataPass[, ModelID := "CatBoost-CARMA"]
            EvalDataPass[, DateTime := Sys.time()]

            # Prepare data for evaluation ----
            temp1 <- RemixAutoML::GenerateEvaluationMetrics(
              EvalData = EvalDataPass,
              TargetName = input$TS_timeSeriesTarget,
              DateName = ProjectList$DateVariableName,
              GroupNames = ProjectList$GroupVariableNames)

            # Finalize data ----
            if(Counter == 1L) {
              FinalGridsGlobal <<- temp1
              PlotDataGlobal <<- EvalDataPass
            } else {
              FinalGridsGlobal <<- data.table::rbindlist(list(FinalGridsGlobal, temp1))
              PlotDataGlobal <<- data.table::rbindlist(list(PlotDataGlobal, EvalDataPass))
            }
          }

          # XGBoost ----
          if(tsmodels == "XGBoost-CARMA") {

            # Increment Counter ----
            Counter <- Counter + 1L

            # Load FC data ----
            FCData <- tryCatch({data.table::fread(file.path(ProjectList$ModelsFolderPath, paste0(ProjectList$TS_timeSeriesTarget, "_AutoXGBoostCARMA_Forecast.csv")))}, error = function(x) NULL)
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
            EvalDataPass[, ModelID := "XGBoost-CARMA"]
            EvalDataPass[, DateTime := Sys.time()]

            # Prepare data for evaluation ----
            temp1 <- RemixAutoML::GenerateEvaluationMetrics(
              EvalData = EvalDataPass,
              TargetName = input$TS_timeSeriesTarget,
              DateName = ProjectList$DateVariableName,
              GroupNames = ProjectList$GroupVariableNames)

            # Finalize data ----
            if(Counter == 1L) {
              FinalGridsGlobal <<- temp1
              PlotDataGlobal <<- EvalDataPass
            } else {
              FinalGridsGlobal <<- data.table::rbindlist(list(FinalGridsGlobal, temp1))
              PlotDataGlobal <<- data.table::rbindlist(list(PlotDataGlobal, EvalDataPass))
            }
          }

          # H2O-CARMA ----
          if(tsmodels == "H2O-CARMA") {

            # Increment Counter ----
            Counter <- Counter + 1L

            # Load FC data ----
            FCData <- tryCatch({data.table::fread(file.path(ProjectList$ModelsFolderPath, paste0(ProjectList$TS_timeSeriesTarget, "_AutoH2OCARMA_Forecast.csv")))}, error = function(x) NULL)
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

            # Prepare data for evaluation ----
            temp1 <- RemixAutoML::GenerateEvaluationMetrics(
              EvalData = EvalDataPass,
              TargetName = input$TS_timeSeriesTarget,
              DateName = ProjectList$DateVariableName,
              GroupNames = ProjectList$GroupVariableNames)

            # Finalize data ----
            if(Counter == 1L) {
              FinalGridsGlobal <<- temp1
              PlotDataGlobal <<- EvalDataPass
            } else {
              FinalGridsGlobal <<- data.table::rbindlist(list(FinalGridsGlobal, temp1))
              PlotDataGlobal <<- data.table::rbindlist(list(PlotDataGlobal, EvalDataPass))
            }
          }
        }

        # Store globally ----
        if(exists("FinalGridsGlobal")) data.table::fwrite(FinalGridsGlobal, file = file.path(ProjectList$ModelsFolderPath, "MetricsData.csv"))

        # Store globally ----
        if(exists("PlotDataGlobal")) {
          data.table::setnames(PlotDataGlobal, "Predictions", "Forecast", skip_absent = TRUE)
          PlotDataGlobal <<- PlotDataGlobal[, .SD, .SDcols = c(input$TS_timeSeriesTarget, "Forecast", ProjectList$DateVariableName, ProjectList$GroupVariableNames, "ModelID")]
          data.table::fwrite(PlotDataGlobal, file = file.path(ProjectList$ModelsFolderPath, "PlotData.csv"))
        }

        # DataTable Model Metrics ----
        output$TS_ModelMetrics <- DT::renderDT({if(exists("FinalGridsGlobal")) FinalGridsGlobal else if(exists("PlotDataGlobal")) PlotDataGlobal}, server = TRUE, filter = "top")

        # Update Select Model Dropdown ----
        #shinyWidgets::updatePickerInput(session, inputId = "TS_ModelID", label = "Select Model", choices = unique(c(ProjectList[["ML_Models"]])), selected = ProjectList[["TS_ModelID"]][[1L]])
        #shinyWidgets::updatePickerInput(session, inputId = "TS_ModelID2", label = "Select Model", choices = unique(c(ProjectList[["ML_Models"]])), selected = ProjectList[["TS_ModelID2"]][[1L]])
    })

    # Plot Forecasts ----
    shiny::observeEvent({
      input$TS_BuildForecastPlot}, {

        # Evaluation Mode ----
        EvalMode <- as.logical(input$Evaluate)

        # Model_ID ----
        Model_ID <- input$TS_ModelID

        # Levels ----
        Group1Levels2 <- input$TS_Group1Levels2
        Group2Levels2 <- input$TS_Group2Levels2
        Group3Levels2 <- input$TS_Group3Levels2

        # Store Plotting Args ----
        ProjectList[["TSEval_timeSeriesTarget"]] <<- input$TSEval_timeSeriesTarget
        ProjectList[["TSEval_timeSeriesTarget2"]] <<- input$TSEval_timeSeriesTarget2
        ProjectList[["TS_ModelID"]] <<- Model_ID
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
        ProjectList[["FC_ForecastLineColor"]] <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "FC_ForecastLineColor", Default = "red")

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

        # Load ML models plot data ----
        if(!exists("PlotDataGlobal")) {

          # Loop through models ----
          for(tsmodels in ProjectList[["ML_Models"]]) {

            # CatBoost ----
            if(tsmodels == "CatBoost-CARMA") {
              FCData <- data.table::fread(file = file.path(ProjectList[["ModelsFolderPath"]], paste0(ProjectList[["TS_timeSeriesTarget"]], "_AutoCatBoostCARMA_Forecast.csv")))
              FCData[, ModelID := "CatBoost-CARMA"]
              if(Counter != 0L) {
                PlotDataGlobal <<- data.table::rbindlist(list(PlotDataGlobal, FCData), fill = TRUE)
                Counter <- Counter + 1L
              } else {
                PlotDataGlobal <<- FCData
                data.table::setnames(PlotDataGlobal, "Predictions", "Forecast", skip_absent = TRUE)
                Counter <- Counter + 1L
              }
            }

            # XGBoost ----
            if(tsmodels == "XGBoost-CARMA") {
              FCData <- data.table::fread(file = file.path(ProjectList[["ModelsFolderPath"]], paste0(ProjectList[["TS_timeSeriesTarget"]], "_AutoXGBoostCARMA_Forecast.csv")))
              FCData[, ModelID := "XGBoost-CARMA"]
              if(Counter != 0L) {
                PlotDataGlobal <<- data.table::rbindlist(list(PlotDataGlobal, FCData), fill = TRUE)
                Counter <- Counter + 1L
              } else {
                PlotDataGlobal <<- FCData
                data.table::setnames(PlotDataGlobal, "Predictions", "Forecast", skip_absent = TRUE)
                Counter <- Counter + 1L
              }
            }

            # H2O ----
            if(tsmodels == "H2O-CARMA") {
              FCData <- data.table::fread(file = file.path(ProjectList[["ModelsFolderPath"]], paste0(ProjectList[["TS_timeSeriesTarget"]], "_AutoH2OCARMA_Forecast.csv")))
              FCData[, ModelID := "H2O-CARMA"]
              if(Counter != 0L) {
                PlotDataGlobal <<- data.table::rbindlist(list(PlotDataGlobal, FCData), fill = TRUE)
                Counter <- Counter + 1L
              } else {
                PlotDataGlobal <<- FCData
                data.table::setnames(PlotDataGlobal, "Predictions", "Forecast", skip_absent = TRUE)
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
        FC_ForecastLineColor <- RemixAutoML::ReturnParam(input, VarName = "FC_ForecastLineColor", Type = "character", Default = "red")

        # Create data for plotting ML results ----

        print(PlotDataGlobal)

        PlotDataForecastFinal <- data.table::copy(PlotDataGlobal[ModelID == eval(Model_ID)])

        print(PlotDataForecastFinal)

        print(datevariables)
        print(groupvariables)

        if(!is.null(groupvariables)) PlotDataForecastFinal[, eval(groupvariables) := lapply(.SD, as.character), .SDcols = c(groupvariables)]
        print("here motherfucker")
        PlotDataForecastFinal <- RemixAutoML::PreparePlotData(
          input,
          PlotDataForecastFinal,
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
        }

        # Build Plot ----

        print(PlotDataForecastFinal)

        #output$TS_ForecastPlot <- plotly::renderPlotly({
        output$TS_ForecastPlot <- shiny::renderPlot({

          # Plot forecast data ----
          #TimeSeriesPlotObject <<-
          # Update theme
          print(exists("EvalMode1"))
          #EvalMode1 == EvalMode
          #ModelID_ == Model_ID
          print("Update ChartTheme only?")
          print(exists("EvalMode1") && EvalMode_ == EvalMode && ModelID_ == Model_ID && Group1Levels2_ == Group1Levels2 && Group2Levels2_ == Group2Levels2 && Group3Levels2_ == Group3Levels2)
          if(exists("EvalMode1") && EvalMode_ == EvalMode && ModelID_ == Model_ID && Group1Levels2_ == Group1Levels2 && Group2Levels2_ == Group2Levels2 && Group3Levels2_ == Group3Levels2) {
            TimeSeriesPlotObject <- TimeSeriesPlotObject + RemixAutoML::ChartTheme(
              Size = 12,
              AngleX = 35,
              AngleY = 0,
              ChartColor = ChartColor,
              BorderColor = BorderColor,
              TextColor = TextColor,
              GridColor = GridColor,
              BackGroundColor = BackGroundColor,
              LegendPosition = LegendPosition)
            TimeSeriesPlotObject
          } else {
            EvalMode_ <<- EvalMode
            ModelID_ <<- Model_ID
            Group1Levels2_ <<- Group1Levels2
            Group2Levels2_ <<- Group2Levels2
            Group3Levels2_ <<- Group3Levels2
            TimeSeriesPlotObject  <<- RemixAutoML::TimeSeriesPlotter(
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
              SSForecast = FALSE,
              ChartColor = ChartColor,
              BorderColor = BorderColor,
              TextColor = TextColor,
              GridColor = GridColor,
              BackGroundColor = BackGroundColor,
              LegendPosition = LegendPosition,
              LegendTextColor = LegendTextColor,
              LegendTextSize = LegendTextSize,
              PredictionIntervals = FALSE,
              TS_ModelID = Model_ID,
              ForecastLineColor = FC_ForecastLineColor)

            # Send to renderPlot
            TimeSeriesPlotObject
          }

          # Convert to plotly
          #plotly::ggplotly(TimeSeriesPlotObject, tooltip = NULL)
        })
    })

    # Next and Previous Buttons ----
    shiny::observeEvent(input$link_to_panel_forecasting_model_evaluation, {
      shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "panel_forecasting_model_evaluation")
    })
    shiny::observeEvent(input$link_to_panel_forecasting_model_evaluation_1, {
      shinydashboard::updateTabItems(session, inputId = "modelMenu", selected = "panel_forecasting_model_evaluation")
    })

})
