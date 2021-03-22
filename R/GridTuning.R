#' @title GridTuner
#'
#' @description Execute grid tuning for catboost or xgboost
#'
#' @author Adrian Antico
#' @family Reinforcement Learning
#'
#' @param AlgoType "catboost"
#' @param ModelType "classification"
#' @param TrainOnFull. TrainOnFull
#' @param TargetColumnName. TargetColumnName
#' @param DebugMode. DebugMode
#' @param task_type. task_type
#' @param Trees. Trees
#' @param Depth. Depth
#' @param LearningRate. LearningRate
#' @param L2_Leaf_Reg. L2_Leaf_Reg
#' @param BorderCount. BorderCount
#' @param RandomStrength. RandomStrength
#' @param RSM. RSM
#' @param BootStrapType. BootStrapType
#' @param GrowPolicy. GrowPolicy
#' @param NumGPUs NumGPUs
#' @param LossFunction LossFunction
#' @param EvalMetric EvalMetric
#' @param MetricPeriods MetricPeriods
#' @param ClassWeights ClassWeights
#' @param CostMatrixWeights CostMatrixWeights
#' @param model_path model_path
#' @param data data
#' @param TrainPool. TrainPool
#' @param TestPool. TestPool
#' @param TestTarget. TestTarget
#' @param FinalTestPool. FinalTestPool
#' @param TestData. TestData
#' @param TestMerge. TestMerge
#' @param TargetLevels. TargetLevels
#' @param MaxRunsWithoutNewWinner MaxRunsWithoutNewWinner
#' @param MaxModelsInGrid MaxModelsInGrid
#' @param MaxRunMinutes MaxRunMinutes
#' @param SaveModelObjects SaveModelObjects
#' @param metadata_path metadata_path
#' @param model_path model_path
#' @param ModelID ModelID
#' @param BaselineComparison. BaselineComparison
#' @param grid_eval_metric. MultiClass
#'
#' @export
GridTuner <- function(AlgoType="catboost",
                      ModelType="classification",
                      TrainOnFull.=TrainOnFull,
                      TargetColumnName.=TargetColumnName,
                      DebugMode.=DebugMode.,
                      HasTime=HasTime,

                      # CatBoostParameterGrids
                      task_type.=task_type,
                      Trees.=Trees,
                      Depth.=Depth,
                      LearningRate.=LearningRate,
                      L2_Leaf_Reg.=L2_Leaf_Reg,
                      BorderCount.=BorderCount,
                      RandomStrength.=RandomStrength,
                      RSM.=RSM,
                      BootStrapType.=BootStrapType,
                      GrowPolicy.=GrowPolicy,

                      # CatBoostGridParams
                      NumGPUs=NumGPUs,
                      LossFunction=LossFunction,
                      EvalMetric=EvalMetric,
                      MetricPeriods=MetricPeriods,
                      ClassWeights=ClassWeights,
                      CostMatrixWeights=CostMatrixWeights,

                      # Model Building
                      data=data,
                      TrainPool.=TrainPool,
                      TestPool.=TestPool,
                      FinalTestTarget.=FinalTestTarget,
                      TestTarget.=TestTarget,
                      FinalTestPool.=FinalTestPool,
                      TestData.=TestData,
                      TestMerge.=TestMerge,
                      TargetLevels.=TargetLevels,

                      # Grid tuning args
                      MaxRunsWithoutNewWinner=MaxRunsWithoutNewWinner,
                      MaxModelsInGrid=MaxModelsInGrid,
                      MaxRunMinutes=MaxRunMinutes,
                      BaselineComparison.=BaselineComparison,

                      # File path args
                      SaveModelObjects=SaveModelObjects,
                      metadata_path=metadata_path,
                      model_path=model_path,
                      ModelID=ModelID,
                      grid_eval_metric.=grid_eval_metric) {

  # Kicking off grid tuning ----
  if(DebugMode.) print("Grid Tuning Start")

  # Generate grid sets ----
  if(AlgoType == "catboost") {
    if(DebugMode.) print("Generate grid sets")
    Grids <- CatBoostParameterGrids(TaskType=task_type., Shuffles=1, NTrees=Trees., Depth=Depth., LearningRate=LearningRate., L2_Leaf_Reg=L2_Leaf_Reg., BorderCount=BorderCount., RandomStrength=RandomStrength., RSM=RSM., BootStrapType=BootStrapType., GrowPolicy=GrowPolicy.)
    ExperimentalGrid <- Grids$ExperimentalGrid
    GridClusters <- Grids$Grids
    Grid <- Grids$Grid
    rm(Grids)
  } else if(AlgoType == "xgboost") {
    print("hello")
  }

  # Initialize RL objects ----
  if(AlgoType == "catboost") {
    if(DebugMode.) print("Initialize RL objects")
    RL_Start <- RL_Initialize(ParameterGridSet = GridClusters, Alpha = 1L, Beta = 1L, SubDivisions = 1000L)
    RunsWithoutNewWinner <- 0L
    BanditProbs <- RL_Start$BanditProbs
    BanditArmsN <- RL_Start$BanditArmsN
    Successes <- RL_Start$Successes
    GridIDs <- RL_Start$GridIDs
    Trials <- RL_Start$Trials
    rm(RL_Start)
  } else if(AlgoType == "xgboost") {
    print("hello")
  }

  # Initalize counters ----
  counter <- 0L; NewGrid <- 1L; BestPerformance <- 1L

  # Grid Tuning Main Loop ----
  if(DebugMode.) print("Grid Tuning Main Loop")
  repeat {

    # Increment counter ----
    counter <- counter + 1L

    # Once all arms have been tried once
    #   Check if there are still values left to sample from a particular grid
    if(counter > BanditArmsN+1L) {

      # Stop sampling from a Grid Cluster if you have already tried every option within it

      # See if the number of trials is greater than the max number of rows in that cluster
      N <- Trials[NewGrid]
      N1 <- GridClusters[[paste0("Grid_", max(1L, NewGrid))]][, .N]
      if(N > N1) Check <- FALSE else Check <- TRUE

      # By setting the success vector to zero, the probability of picking it becomes very slim
      if(!Check) Successes[NewGrid] <- 0

    } else {
      N <- 1L
      Check <- TRUE
    }

    # Build model and collect stats
    if(Check) {

      # Build, Score, Evaluate
      if(AlgoType == "catboost") {
        base_params <- CatBoostGridParams(N.=N, NumGPUs.=NumGPUs, LossFunction.=LossFunction, counter.=counter, BanditArmsN.=BanditArmsN, HasTime.=HasTime, MetricPeriods.=MetricPeriods, ClassWeights.=ClassWeights, EvalMetric.=EvalMetric, task_type.=task_type., model_path.=model_path, Grid.=Grid, ExperimentalGrid.=ExperimentalGrid, GridClusters.=GridClusters, NewGrid.=if(!exists("NewGrid")) NULL else NewGrid)
        RunTime <- system.time(model <- catboost::catboost.train(learn_pool = TrainPool., test_pool = TestPool., params = base_params))
      } else if(AlgoType == "xgboost") {
        print("hello")
      }

      # Classification (AUC EvalMetric Used) ----
      if(AlgoType == "catboost" && ModelType == "classification") {

        # Score model
        if(!is.null(TestData.)) {
          predict <- catboost::catboost.predict(model = model, pool = FinalTestPool., prediction_type = "Probability", thread_count = parallel::detectCores())
          ValidationData <- data.table::as.data.table(cbind(Target = FinalTestTarget., p1 = predict))
          data.table::setnames(ValidationData, "Target", eval(TargetColumnName.))
        } else {
          predict <- catboost::catboost.predict(model = model,pool = TestPool., prediction_type = "Probability", thread_count = parallel::detectCores())
          ValidationData <- data.table::as.data.table(cbind(Target = TestTarget., p1 = predict))
          data.table::setnames(ValidationData, "Target", eval(TargetColumnName.))
        }

        # Generate EvaluationMetrics
        if(DebugMode.) print("Running BinaryMetrics()")
        EvalMetrics <- BinaryMetrics(MLModels.="catboost", ClassWeights.=ClassWeights, CostMatrixWeights.=CostMatrixWeights, SaveModelObjects.=SaveModelObjects, ValidationData.=ValidationData, TrainOnFull.=TrainOnFull., TargetColumnName.=TargetColumnName., ModelID.=ModelID, model_path.=model_path, metadata_path.=metadata_path)

        # New performance
        if(grid_eval_metric. %in% c("Utility", "MCC", "Acc", "F1_Score", "F2_Score", "F0.5_Score", "NPV", "PPV", "TPR", "TNR", "ThreatScore")) {
          NewPerformance <- EvalMetrics[order(-paste0("Cat_", grid_eval_metric.))][[paste0("Cat_", grid_eval_metric.)]]
        } else {
          NewPerformance <- EvalMetrics[order(paste0("Cat_", grid_eval_metric.))][[paste0("Cat_", grid_eval_metric.)]]
        }

        # Update Experimental Grid with Param values
        data.table::set(ExperimentalGrid, i = counter, j = "GridNumber", value = if(counter == 1L) 0 else NewGrid)
        data.table::set(ExperimentalGrid, i = counter, j = "RunTime", value = RunTime[[3L]])
        data.table::set(ExperimentalGrid, i = counter, j = "eval_metric", value = NewPerformance)
        data.table::set(ExperimentalGrid, i = counter, j = "TreesBuilt", value = model$tree_count)

        # BestPerformance thus far
        if(counter > 1L) {
          if(tolower(BaselineComparison.) == "default") {
            BestPerformance <- ExperimentalGrid[RunNumber == 1L][["eval_metric"]]
          } else {
            BestPerformance <- max(ExperimentalGrid[RunNumber < counter][["eval_metric"]], na.rm = TRUE)
          }
        }

        # Performance measures ----
        if(NewPerformance > BestPerformance) RunsWithoutNewWinner <- 0L else RunsWithoutNewWinner <- RunsWithoutNewWinner + 1L
      }

      # Regression (MSE EvalMetric Used) ----
      if(AlgoType == "catboost" && ModelType == "regression") {

        # Score and measure model ----
        if(!is.null(TestData.)) {
          predict <- catboost::catboost.predict(model = model, pool = FinalTestPool., prediction_type = "RawFormulaVal", thread_count = parallel::detectCores())
          ValidationData <- data.table::as.data.table(cbind(Target = FinalTestTarget., Predict = predict))
          data.table::setnames(ValidationData, "Target", TargetColumnName.)
        } else {
          predict <- catboost::catboost.predict(model = model, pool = TestPool., prediction_type = "RawFormulaVal", thread_count = parallel::detectCores())
          ValidationData <- data.table::as.data.table(cbind(Target = TestTarget., Predict = predict))
          data.table::setnames(ValidationData, "Target", TargetColumnName.)
        }

        # Generate EvaluationMetrics
        EvalMetrics <- RegressionMetrics(SaveModelObjects.=SaveModelObjects, data.=data, ValidationData.=ValidationData, TrainOnFull.=TrainOnFull., LossFunction.=LossFunction, EvalMetric.=EvalMetric, TargetColumnName.=TargetColumnName., ModelID.=ModelID, model_path.=model_path, metadata_path.=metadata_path)

        # New performance
        NewPerformance <- EvalMetrics[Metric == eval(toupper(grid_eval_metric.))]$MetricValue

        # Update Experimental Grid with Param values----
        data.table::set(ExperimentalGrid, i = counter, j = "GridNumber", value = NewGrid)
        data.table::set(ExperimentalGrid, i = counter, j = "RunTime", value = RunTime[[3L]])
        data.table::set(ExperimentalGrid, i = counter, j = "EvalMetric", value = NewPerformance)
        data.table::set(ExperimentalGrid, i = counter, j = "TreesBuilt", value = model$tree_count)

        # BestPerformance thus far
        if(counter > 1L) {
          if(tolower(BaselineComparison.) == "default") {
            BestPerformance <- ExperimentalGrid[RunNumber == 1L][["EvalMetric"]]
          } else {
            BestPerformance <- ExperimentalGrid[RunNumber < counter, min(EvalMetric, na.rm = TRUE)]
          }
        }

        # Performance measures ----
        if(NewPerformance < BestPerformance) RunsWithoutNewWinner <- 0L else RunsWithoutNewWinner <- RunsWithoutNewWinner + 1L
      }

      # MultiClass () ----
      if(AlgoType == "catboost" && ModelType == "multiclass") {

        # MultiClass Score Final Test Data ----
        predict <- cbind(
          1 + catboost::catboost.predict(
            model = model,
            pool = if(!is.null(TestData.)) FinalTestPool. else TestPool.,
            prediction_type = "Class"),
          catboost::catboost.predict(
            model = model,
            pool = if(!is.null(TestData.)) FinalTestPool. else TestPool.,
            prediction_type = "Probability"))

        # MultiClass Grid Validation Data ----
        if(!is.null(TestData.)) {
          ValidationData <- data.table::as.data.table(cbind(Target = FinalTestTarget., predict, TestMerge.[, .SD, .SDcols = unique(names(TestMerge.)[c(1L:(ncol(TestMerge.)-1L))])]))
        } else {
          ValidationData <- data.table::as.data.table(cbind(Target = TestTarget., predict))
        }
        if(TrainOnFull.) {
          ValidationData <- merge(
            ValidationData,
            TargetLevels.,
            by.x = "Target",
            by.y = "NewLevels",
            all = FALSE)
          ValidationData[, Target := OriginalLevels][, OriginalLevels := NULL]
          ValidationData <- merge(
            ValidationData,
            TargetLevels.,
            by.x = "V2",
            by.y = "NewLevels",
            all = FALSE)
          ValidationData[, V2 := OriginalLevels][, OriginalLevels := NULL]
        } else {
          ValidationData <- merge(
            ValidationData,
            TargetLevels.,
            by.x = "V1",
            by.y = "NewLevels",
            all = FALSE)
          ValidationData[, V1 := OriginalLevels][, OriginalLevels := NULL]
          ValidationData <- merge(
            ValidationData,
            TargetLevels.,
            by.x = "Target",
            by.y = "NewLevels",
            all = FALSE)
          ValidationData[, Target := OriginalLevels][, OriginalLevels := NULL]
        }

        # MultiClass Update Names for Predicted Value Columns----
        if(!TrainOnFull.) k <- 1L else k <- 2L
        for(name in as.character(TargetLevels.[[1L]])) {
          k <- k + 1L
          data.table::setnames(ValidationData, paste0("V", k), name)
        }
        if(!TrainOnFull.) data.table::setnames(ValidationData, "V1", "Predict") else data.table::setnames(ValidationData, "V2", "Predict")
        data.table::set(ValidationData, j = "Target", value = as.character(ValidationData[["Target"]]))
        data.table::set(ValidationData, j = "Predict", value = as.character(ValidationData[["Predict"]]))

        # MultiClass Metrics Accuracy----
        if(tolower(grid_eval_metric.) == "accuracy") {
          NewPerformance <- ValidationData[, mean(data.table::fifelse(as.character(Target) == as.character(Predict), 1.0, 0.0), na.rm = TRUE)]

        } else if(tolower(grid_eval_metric.) == "microauc") {
          NewPerformance <- round(as.numeric(noquote(stringr::str_extract(pROC::multiclass.roc(response = ValidationData[["Target"]], predictor = as.matrix(ValidationData[, .SD, .SDcols = names(ValidationData)[3L:(ncol(predict)+1L)]]))$auc, "\\d+\\.*\\d*"))), 4L)

        } else if(tolower(grid_eval_metric.) == "logloss") {
          temp <- ValidationData[, 1L]
          temp[, Truth := get(TargetColumnName.)]
          temp <- DummifyDT(
            data = temp,
            cols = eval(TargetColumnName.),
            KeepFactorCols = FALSE,
            OneHot = FALSE,
            SaveFactorLevels = FALSE,
            SavePath = NULL,
            ImportFactorLevels = FALSE,
            FactorLevelsList = NULL,
            ClustScore = FALSE,
            ReturnFactorLevels = FALSE)
          N <- TargetLevels.[, .N]
          NewPerformance <- MLmetrics::LogLoss(y_pred = as.matrix(ValidationData[, 3L:(2L+N)]), y_true = as.matrix(temp[, 2L:(1L+N)]))
        }

        # Update Experimental Grid with Param values ----
        data.table::set(ExperimentalGrid, i = counter, j = "GridNumber", value = NewGrid)
        data.table::set(ExperimentalGrid, i = counter, j = "RunTime", value = RunTime[[3L]])
        data.table::set(ExperimentalGrid, i = counter, j = "eval_metric", value = NewPerformance)
        data.table::set(ExperimentalGrid, i = counter, j = "TreesBuilt", value = model$tree_count)

        # BestPerformance thus far
        if(counter > 1L) {
          if(tolower(BaselineComparison.) == "default") {
            BestPerformance <- ExperimentalGrid[RunNumber == 1L][["EvalMetric"]]
          } else {
            BestPerformance <- ExperimentalGrid[RunNumber < counter, min(EvalMetric, na.rm = TRUE)]
          }
        }

        # Performance measures ----
        if(tolower(grid_eval_metric.) %chin% c("accuracy","microauc")) {
          if(NewPerformance > BestPerformance) RunsWithoutNewWinner <- 0L else RunsWithoutNewWinner <- RunsWithoutNewWinner + 1L
        } else if(tolower(grid_eval_metric.) %chin% c("logloss")) {
          if(NewPerformance < BestPerformance) RunsWithoutNewWinner <- 0L else RunsWithoutNewWinner <- RunsWithoutNewWinner + 1L
        }
      }

      # Total Run Time
      TotalRunTime <- ExperimentalGrid[RunTime != -1L, sum(RunTime, na.rm = TRUE)]

      # Remove Model and Collect Garbage ----
      rm(model); gc()
    }

    # Update bandit probabilities ----
    RL_Update_Output <- RL_ML_Update(ModelType="classification", Iteration=counter, NewGrid.=NewGrid, NewPerformance.=NewPerformance, BestPerformance.=BestPerformance, Trials.=Trials, Successes.=Successes, GridIDs.=GridIDs, BanditArmsN.=BanditArmsN, RunsWithoutNewWinner.=RunsWithoutNewWinner, MaxRunsWithoutNewWinner.=MaxRunsWithoutNewWinner, MaxModelsInGrid.=MaxModelsInGrid, MaxRunMinutes.=MaxRunMinutes, TotalRunTime.=TotalRunTime, BanditProbs.=BanditProbs)
    BanditProbs <- RL_Update_Output$BanditProbs
    NewGrid <- RL_Update_Output$NewGrid
    Trials <- RL_Update_Output$Trials

    # Continue or stop ----
    if(RL_Update_Output$BreakLoop != "stay") break

    # Update collection table ----
    data.table::set(ExperimentalGrid, i = counter+1L, j = "NTrees", value = GridClusters[[paste0("Grid_",NewGrid)]][["NTrees"]][Trials[NewGrid]+1L])
    data.table::set(ExperimentalGrid, i = counter+1L, j = "Depth", value = GridClusters[[paste0("Grid_",NewGrid)]][["Depth"]][Trials[NewGrid]+1L])
    data.table::set(ExperimentalGrid, i = counter+1L, j = "LearningRate", value = GridClusters[[paste0("Grid_",NewGrid)]][["LearningRate"]][Trials[NewGrid]+1L])
    data.table::set(ExperimentalGrid, i = counter+1L, j = "L2_Leaf_Reg", value = GridClusters[[paste0("Grid_",NewGrid)]][["L2_Leaf_Reg"]][Trials[NewGrid]+1L])
    data.table::set(ExperimentalGrid, i = counter+1L, j = "RandomStrength", value = GridClusters[[paste0("Grid_",NewGrid)]][["RandomStrength"]][Trials[NewGrid]+1L])
    data.table::set(ExperimentalGrid, i = counter+1L, j = "BorderCount", value = GridClusters[[paste0("Grid_",NewGrid)]][["BorderCount"]][Trials[NewGrid]+1L])
    data.table::set(ExperimentalGrid, i = counter+1L, j = "BootStrapType", value = GridClusters[[paste0("Grid_",NewGrid)]][["BootStrapType"]][Trials[NewGrid]+1L])

    # GPU args ----
    if(!tolower(task_type.) == "gpu") {
      data.table::set(ExperimentalGrid, i = counter+1L, j = "RSM", value = GridClusters[[paste0("Grid_",NewGrid)]][["RSM"]][Trials[NewGrid]+1L])
      data.table::set(ExperimentalGrid, i = counter+1L, j = "GrowPolicy", value = GridClusters[[paste0("Grid_",NewGrid)]][["GrowPolicy"]][Trials[NewGrid]+1L])
    }

    # Update bandit probs
    for(bandit in seq_len(length(BanditProbs))) data.table::set(ExperimentalGrid, i = counter+1L, j = paste0("BanditProbs_Grid_",bandit), value = BanditProbs[bandit])

    # Binary Save ExperimentalGrid repeatedly in case of crash, results will be available that way ----
    if(SaveModelObjects) {
      if(!is.null(metadata_path)) {
        data.table::fwrite(ExperimentalGrid[RunTime != -1L], file = file.path(metadata_path, paste0(ModelID, "_ExperimentalGrid.csv")))
      } else {
        data.table::fwrite(ExperimentalGrid[RunTime != -1L], file = file.path(model_path, paste0(ModelID, "_ExperimentalGrid.csv")))
      }
    }
  }

  # Remove unneeded rows ----
  ExperimentalGrid <- ExperimentalGrid[RunTime != -1L]

  # Sort by EvalMetric
  if(ModelType == "classification") {
    BestGrid <- ExperimentalGrid[order(-EvalMetric)][1L]
  } else if(ModelType == "regression") {
    BestGrid <- ExperimentalGrid[order(EvalMetric)][1L]
  } else {
    if(grid_eval_metric. %in% c("accuracy", "microauc")) {
      BestGrid <- ExperimentalGrid[order(-EvalMetric)][1L]
    } else {
      BestGrid <- ExperimentalGrid[order(EvalMetric)][1L]
    }
  }

  # Return
  return(list(ExperimentalGrid = ExperimentalGrid, BestGrid = BestGrid))
}
