#' @title CatBoostGridTuner
#'
#' @description Execute grid tuning for catboost
#'
#' @author Adrian Antico
#' @family Reinforcement Learning
#'
#' @param ModelType "classification"
#' @param TrainOnFull. TrainOnFull
#' @param TargetColumnName. TargetColumnName
#' @param DebugMode. DebugMode
#' @param HasTime HasTime
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
#' @noRd
CatBoostGridTuner <- function(ModelType="classification",
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
  if(DebugMode.) print("Generate grid sets")
  Grids <- CatBoostParameterGrids(TaskType=task_type., Shuffles=1, NTrees=Trees., Depth=Depth., LearningRate=LearningRate., L2_Leaf_Reg=L2_Leaf_Reg., BorderCount=BorderCount., RandomStrength=RandomStrength., RSM=RSM., BootStrapType=BootStrapType., GrowPolicy=GrowPolicy.)
  ExperimentalGrid <- Grids$ExperimentalGrid
  GridClusters <- Grids$Grids
  Grid <- Grids$Grid
  rm(Grids)

  # Initialize RL objects ----
  if(DebugMode.) print("Initialize RL objects")
  RL_Start <- RL_Initialize(ParameterGridSet = GridClusters, Alpha = 1L, Beta = 1L, SubDivisions = 1000L)
  RunsWithoutNewWinner <- 0L
  BanditProbs <- RL_Start$BanditProbs
  BanditArmsN <- RL_Start$BanditArmsN
  Successes <- RL_Start$Successes
  GridIDs <- RL_Start$GridIDs
  Trials <- RL_Start$Trials
  rm(RL_Start)

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
      base_params <- CatBoostGridParams(N.=N, NumGPUs.=NumGPUs, LossFunction.=LossFunction, counter.=counter, BanditArmsN.=BanditArmsN, HasTime.=HasTime, MetricPeriods.=MetricPeriods, ClassWeights.=ClassWeights, EvalMetric.=EvalMetric, task_type.=task_type., model_path.=model_path, Grid.=Grid, GridClusters.=GridClusters, NewGrid.=if(!exists("NewGrid")) NULL else NewGrid)
      RunTime <- system.time(model <- catboost::catboost.train(learn_pool = TrainPool., test_pool = TestPool., params = base_params))

      # Classification (AUC EvalMetric Used) ----
      if(ModelType == "classification") {

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
        EvalMetrics <- BinaryMetrics(ClassWeights.=ClassWeights, CostMatrixWeights.=CostMatrixWeights, SaveModelObjects.=SaveModelObjects, ValidationData.=ValidationData, TrainOnFull.=TrainOnFull., TargetColumnName.=TargetColumnName., ModelID.=ModelID, model_path.=model_path, metadata_path.=metadata_path)

        # New performance
        if(grid_eval_metric. %in% c("Utility", "MCC", "Acc", "F1_Score", "F2_Score", "F0.5_Score", "NPV", "PPV", "TPR", "TNR", "ThreatScore")) {
          NewPerformance <- EvalMetrics[order(-get(grid_eval_metric.))][[eval(grid_eval_metric.)]][[1L]]
        } else {
          NewPerformance <- EvalMetrics[order(get(grid_eval_metric.))][[eval(grid_eval_metric.)]][[1L]]
        }

        # Update Experimental Grid with Param values
        data.table::set(ExperimentalGrid, i = counter, j = "GridNumber", value = if(counter == 1L) 0 else NewGrid)
        data.table::set(ExperimentalGrid, i = counter, j = "RunTime", value = RunTime[[3L]])
        data.table::set(ExperimentalGrid, i = counter, j = "MetricValue", value = NewPerformance)
        data.table::set(ExperimentalGrid, i = counter, j = "TreesBuilt", value = model$tree_count)

        # BestPerformance thus far
        if(counter > 1L) {
          if(tolower(BaselineComparison.) == "default") {
            BestPerformance <- ExperimentalGrid[RunNumber == 1L][["MetricValue"]]
          } else {
            BestPerformance <- max(ExperimentalGrid[RunNumber < counter][["MetricValue"]], na.rm = TRUE)
          }
        }

        # Performance measures ----
        if(NewPerformance > BestPerformance) RunsWithoutNewWinner <- 0L else RunsWithoutNewWinner <- RunsWithoutNewWinner + 1L
      }

      # Regression (MSE EvalMetric Used) ----
      if(ModelType == "regression") {

        # Score and measure model ----
        if(!is.null(TestData.)) {
          predict <- catboost::catboost.predict(model = model, pool = FinalTestPool., prediction_type = "RawFormulaVal", thread_count = parallel::detectCores())
          if(length(TargetColumnName.) > 1L) {
            ValidationData <- data.table::as.data.table(cbind(TestMerge., Predict = predict))
          } else {
            ValidationData <- data.table::as.data.table(cbind(Target = FinalTestTarget., Predict = predict))
            data.table::setnames(ValidationData, "Target", TargetColumnName.)
          }
        } else {
          predict <- catboost::catboost.predict(model = model, pool = TestPool., prediction_type = "RawFormulaVal", thread_count = parallel::detectCores())
          if(length(TargetColumnName.) > 1L) {
            ValidationData <- data.table::as.data.table(cbind(TestTarget., Predict = predict))
            data.table::setnames(ValidationData, c("V1","V2"), TargetColumnName.)
          } else {
            ValidationData <- data.table::as.data.table(cbind(Target = TestTarget., Predict = predict))
            data.table::setnames(ValidationData, "Target", TargetColumnName.)
          }
        }

        # Generate EvaluationMetrics
        EvalMetrics <- RegressionMetrics(SaveModelObjects.=SaveModelObjects, data.=data, ValidationData.=ValidationData, TrainOnFull.=TrainOnFull., LossFunction.=LossFunction, EvalMetric.=EvalMetric, TargetColumnName.=TargetColumnName., ModelID.=ModelID, model_path.=model_path, metadata_path.=metadata_path)

        # New performance
        if(length(TargetColumnName.) > 1L) {
          EvalMetrics <- data.table::rbindlist(EvalMetrics)
          NewPerformance <- EvalMetrics[Metric == eval(toupper(grid_eval_metric.)), mean(MetricValue, na.rm = TRUE)]
        } else {
          NewPerformance <- EvalMetrics[Metric == eval(toupper(grid_eval_metric.))]$MetricValue
        }

        # Update Experimental Grid with Param values----
        data.table::set(ExperimentalGrid, i = counter, j = "GridNumber", value = NewGrid)
        data.table::set(ExperimentalGrid, i = counter, j = "RunTime", value = RunTime[[3L]])
        data.table::set(ExperimentalGrid, i = counter, j = "MetricValue", value = NewPerformance)
        data.table::set(ExperimentalGrid, i = counter, j = "TreesBuilt", value = model$tree_count)

        # BestPerformance thus far
        if(counter > 1L) {
          if(tolower(BaselineComparison.) == "default") {
            BestPerformance <- ExperimentalGrid[RunNumber == 1L][["MetricValue"]]
          } else {
            BestPerformance <- ExperimentalGrid[RunNumber < counter, min(MetricValue, na.rm = TRUE)]
          }
        }

        # Performance measures ----
        if(grid_eval_metric. %in% c("r2")) {
          if(NewPerformance > BestPerformance) RunsWithoutNewWinner <- 0L else RunsWithoutNewWinner <- RunsWithoutNewWinner + 1L
        } else {
          if(NewPerformance < BestPerformance) RunsWithoutNewWinner <- 0L else RunsWithoutNewWinner <- RunsWithoutNewWinner + 1L
        }
      }

      # MultiClass () ----
      if(ModelType == "multiclass") {

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

        # MultiClass Update Names for Predicted Value Columns----
        k <- 1L
        for(name in as.character(TargetLevels.[[1L]])) {
          k <- k + 1L
          data.table::setnames(ValidationData, paste0("V", k), name)
        }
        data.table::setnames(ValidationData, "V1", "Predict")
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
        data.table::set(ExperimentalGrid, i = counter, j = "MetricValue", value = NewPerformance)
        data.table::set(ExperimentalGrid, i = counter, j = "TreesBuilt", value = model$tree_count)

        # BestPerformance thus far
        if(counter > 1L) {
          if(tolower(BaselineComparison.) == "default") {
            BestPerformance <- ExperimentalGrid[RunNumber == 1L][["MetricValue"]]
          } else {
            BestPerformance <- ExperimentalGrid[RunNumber < counter, min(MetricValue, na.rm = TRUE)]
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
    RL_Update_Output <- RL_ML_Update(ModelType=ModelType, grid_eval_metric=grid_eval_metric., Iteration=counter, NewGrid.=NewGrid, NewPerformance.=NewPerformance, BestPerformance.=BestPerformance, Trials.=Trials, Successes.=Successes, GridIDs.=GridIDs, BanditArmsN.=BanditArmsN, RunsWithoutNewWinner.=RunsWithoutNewWinner, MaxRunsWithoutNewWinner.=MaxRunsWithoutNewWinner, MaxModelsInGrid.=MaxModelsInGrid, MaxRunMinutes.=MaxRunMinutes, TotalRunTime.=TotalRunTime, BanditProbs.=BanditProbs)
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
    data.table::set(ExperimentalGrid, i = counter+1L, j = "GrowPolicy", value = GridClusters[[paste0("Grid_",NewGrid)]][["GrowPolicy"]][Trials[NewGrid]+1L])

    # GPU args ----
    if(tolower(task_type.) != "gpu") {
      data.table::set(ExperimentalGrid, i = counter+1L, j = "RSM", value = GridClusters[[paste0("Grid_",NewGrid)]][["RSM"]][Trials[NewGrid]+1L])
    }

    # Update bandit probs
    for(bandit in seq_along(BanditProbs)) data.table::set(ExperimentalGrid, i = counter+1L, j = paste0("BanditProbs_Grid_",bandit), value = BanditProbs[bandit])

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

  # Sort by MetricValue
  PositiveClassificationMeasures <- c('Utility','MCC','Acc','F1_Score','F2_Score','F0.5_Score','TPR','TNR','NPV','PPV','ThreatScore')
  PositiveRegressionMeasures <- c('r2')
  PositiveMultiClassMeasures <- c('AUC', 'Accuracy')
  if(grid_eval_metric. %chin% c(PositiveClassificationMeasures, PositiveRegressionMeasures, PositiveMultiClassMeasures)) {
    BestGrid <- ExperimentalGrid[order(-MetricValue)][1L]
  } else {
    BestGrid <- ExperimentalGrid[order(MetricValue)][1L]
  }

  # Return
  return(list(ExperimentalGrid = ExperimentalGrid, BestGrid = BestGrid))
}

#' @title XGBoostGridTuner
#'
#' @description Execute grid tuning for xgboost
#'
#' @author Adrian Antico
#' @family Reinforcement Learning
#'
#' @param ModelType "classification"
#' @param TrainOnFull. TrainOnFull
#' @param TargetColumnName. TargetColumnName
#' @param DebugMode. DebugMode
#' @param TreeMethod. TreeMethod
#' @param Trees. Trees
#' @param Depth. max_depth
#' @param LearningRate. eta
#' @param min_child_weight. min_child_weight
#' @param subsample. subsample
#' @param colsample_bytree. colsample_bytree
#' @param LossFunction LossFunction
#' @param EvalMetric EvalMetric
#' @param grid_eval_metric. MultiClass
#' @param MetricPeriods MetricPeriods
#' @param ClassWeights ClassWeights
#' @param CostMatrixWeights CostMatrixWeights
#' @param model_path model_path
#' @param TargetColumnName. TargetColumnName
#' @param datatrain. datatrain
#' @param datavalidate. datavalidate
#' @param datatest. datatest
#' @param EvalSets. EvalSets
#' @param TestTarget. TestTarget
#' @param FinalTestTarget. FinalTestTarget
#' @param TargetLevels. TargetLevels
#' @param MaxRunsWithoutNewWinner MaxRunsWithoutNewWinner
#' @param MaxModelsInGrid MaxModelsInGrid
#' @param MaxRunMinutes MaxRunMinutes
#' @param SaveModelObjects SaveModelObjects
#' @param metadata_path metadata_path
#' @param model_path model_path
#' @param ModelID ModelID
#' @param BaselineComparison. BaselineComparison
#' @param Verbose. Verbose
#' @param NumLevels. NumLevels
#'
#' @noRd
XGBoostGridTuner <- function(ModelType="classification",
                             TrainOnFull.=TrainOnFull,
                             DebugMode.=DebugMode.,
                             TreeMethod.=TreeMethod,

                             # CatBoostParameterGrids
                             Trees.=Trees,
                             Depth.=max_depth,
                             LearningRate.=eta,
                             min_child_weight.=min_child_weight,
                             subsample. = subsample,
                             colsample_bytree. = colsample_bytree,

                             # CatBoostGridParams
                             LossFunction=LossFunction,
                             EvalMetric=eval_metric,
                             grid_eval_metric.=grid_eval_metric,
                             CostMatrixWeights=CostMatrixWeights,

                             # Model Building
                             TargetColumnName. = TargetColumnName,
                             datatrain. = datatrain,
                             datavalidate. = datavalidate,
                             datatest. = datatest,
                             EvalSets. = EvalSets,
                             TestTarget. = TestTarget,
                             FinalTestTarget. = FinalTestTarget,
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
                             Verbose. = Verbose,
                             NumLevels.=NumLevels) {

  # Kicking off grid tuning ----
  if(DebugMode.) print("Grid Tuning Start")

  # Generate grid sets ----
  if(DebugMode.) print("Generate grid sets")
  Grids <- XGBoostParameterGrids(Shuffles=1,NTrees=Trees.,Depth=Depth.,LearningRate=LearningRate.,MinChildWeight=min_child_weight.,SubSample=subsample.,ColSampleByTree=colsample_bytree.)
  Grid <- Grids$Grid
  GridClusters <- Grids$Grids
  ExperimentalGrid <- Grids$ExperimentalGrid

  # Initialize RL objects ----
  if(DebugMode.) print("Initialize RL objects")
  RL_Start <- RL_Initialize(ParameterGridSet = GridClusters, Alpha = 1L, Beta = 1L, SubDivisions = 1000L)
  RunsWithoutNewWinner <- 0L
  BanditProbs <- RL_Start$BanditProbs
  BanditArmsN <- RL_Start$BanditArmsN
  Successes <- RL_Start$Successes
  GridIDs <- RL_Start$GridIDs
  Trials <- RL_Start$Trials
  rm(RL_Start)

  # Initalize counters ----
  counter <- 0L; NewGrid <- 1L; BestPerformance <- 1L

  # Grid Tuning Main Loop ----
  if(DebugMode.) print("Grid Tuning Main Loop")
  repeat {

    # Increment counter ----
    counter <- counter + 1L

    # Once all arms have been tried once
    if(counter > BanditArmsN+1L) {

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

      # Define parameters
      base_params <- XGBoostGridParams(N.=N, NewGrid.=NewGrid,Objective.=LossFunction,counter.=counter,BanditArmsN.=BanditArmsN,EvalMetric.=EvalMetric,TreeMethod.=TreeMethod.,model_path.=model_path,Grid.=Grid,GridClusters.=GridClusters)

      # Define number of trees
      if(counter <= BanditArmsN + 1L) {
        if(counter == 1L) {
          nrounds <- max(Grid$NTrees)
        } else {
          nrounds <- GridClusters[[paste0("Grid_", counter-1L)]][["NTrees"]][1L]
        }
      } else {
        nrounds <- GridClusters[[paste0("Grid_", NewGrid)]][["NTrees"]][1L]
      }

      # Multiclass
      if(ModelType == "multiclass") base_params[["num_class"]] <- NumLevels.

      # Build model
      RunTime <- system.time(model <- xgboost::xgb.train(params=base_params, data=datatrain., nrounds = nrounds, watchlist=EvalSets., verbose=Verbose.))

      # Binary Grid Score Model
      if(!is.null(datatest.)) {
        if(ModelType == "classification") {
          predict <- stats::predict(model, datatest.)
          ValidationData <- data.table::as.data.table(cbind(Target = FinalTestTarget., p1 = predict))
          data.table::setnames(ValidationData, "Target", TargetColumnName.)
        } else if(ModelType == "regression") {
          predict <- stats::predict(model, datatest.)
          ValidationData <- data.table::as.data.table(cbind(Target = FinalTestTarget., Predict = predict))
          data.table::setnames(ValidationData, "Target", TargetColumnName.)
        } else {
          predict <-XGBoostMultiClassPredict(model=model, datatest=datatest., TargetLevels=TargetLevels., NumLevels=NumLevels., NumberRows=nrow(datatest.))
          ValidationData <- data.table::as.data.table(cbind(Target = FinalTestTarget., predict))
          data.table::setnames(ValidationData, "Target", TargetColumnName.)
          data.table::setkeyv(ValidationData, cols = TargetColumnName.)
          data.table::setkeyv(TargetLevels., cols = "NewLevels")
          ValidationData[TargetLevels., OriginalLevels := i.OriginalLevels][, eval(TargetColumnName.) := OriginalLevels][, OriginalLevels := NULL]
        }
      } else {
        if(ModelType == "classification") {
          predict <- stats::predict(model, datavalidate.)
          ValidationData <- data.table::as.data.table(cbind(Target = TestTarget., p1 = predict))
          data.table::setnames(ValidationData, "Target", TargetColumnName.)
        } else if(ModelType == "regression") {
          predict <- stats::predict(model, datavalidate.)
          ValidationData <- data.table::as.data.table(cbind(Target = TestTarget., Predict = predict))
          data.table::setnames(ValidationData, "Target", TargetColumnName.)
        } else {
          predict <- XGBoostMultiClassPredict(model=model, datatest=datavalidate., TargetLevels=TargetLevels., NumLevels=NumLevels., NumberRows=nrow(datavalidate.))
          ValidationData <- data.table::as.data.table(cbind(Target = TestTarget., predict))
          data.table::setnames(ValidationData, "Target", TargetColumnName.)
          data.table::setkeyv(ValidationData, cols = TargetColumnName.)
          data.table::setkeyv(TargetLevels., cols = "NewLevels")
          ValidationData[TargetLevels., OriginalLevels := i.OriginalLevels][, eval(TargetColumnName.) := OriginalLevels][, OriginalLevels := NULL]
        }
      }

      # Eval metrics
      if(tolower(ModelType) == "classification") {
        EvalMetrics <- BinaryMetrics(ClassWeights.=NULL, CostMatrixWeights.=CostMatrixWeights, SaveModelObjects.=SaveModelObjects, ValidationData.=ValidationData, TrainOnFull.=TrainOnFull., TargetColumnName.=TargetColumnName., ModelID.=ModelID, model_path.=model_path, metadata_path.=metadata_path)
      } else if(tolower(ModelType) == "regression") {
        EvalMetrics <- RegressionMetrics(SaveModelObjects.=SaveModelObjects, data.=data, ValidationData.=ValidationData, TrainOnFull.=TrainOnFull., LossFunction.="Adrian", EvalMetric.=NULL, TargetColumnName.=TargetColumnName., ModelID.=ModelID, model_path.=model_path, metadata_path.=metadata_path)
      } else if(tolower(ModelType) == "multiclass") {
        EvalMetrics <- MultiClassMetrics(ModelClass="xgboost", DataType = "validate", SaveModelObjects.=FALSE, ValidationData.=ValidationData, PredictData.=predict, TrainOnFull.=FALSE, TargetColumnName.=TargetColumnName., TargetLevels.=TargetLevels., ModelID.=ModelID, model_path.=model_path, metadata_path.=metadata_path)
      }

      # New performance
      if(ModelType == "classification") {
        if(grid_eval_metric. %chin% c("Utility", "MCC", "Acc", "F1_Score", "F2_Score", "F0.5_Score", "NPV", "PPV", "TPR", "TNR", "ThreatScore")) {
          NewPerformance <- EvalMetrics[order(-get(grid_eval_metric.))][[eval(grid_eval_metric.)]][[1L]]
        } else {
          NewPerformance <- EvalMetrics[order(get(grid_eval_metric.))][[eval(grid_eval_metric.)]][[1L]]
        }
      } else {
        NewPerformance <- EvalMetrics[toupper(Metric) == eval(toupper(grid_eval_metric.))][["MetricValue"]]
      }

      # Update Experimental Grid with Param values
      data.table::set(ExperimentalGrid, i = counter, j = "GridNumber", value = if(counter == 1L) 0 else NewGrid)
      data.table::set(ExperimentalGrid, i = counter, j = "RunTime", value = RunTime[[3L]])
      data.table::set(ExperimentalGrid, i = counter, j = "EvalMetric", value = NewPerformance)
      data.table::set(ExperimentalGrid, i = counter, j = "TreesBuilt", value = model$niter)

      # Total Run Time
      TotalRunTime <- ExperimentalGrid[RunTime != -1L, sum(RunTime, na.rm = TRUE)]

      # Binary Remove Model and Collect Garbage
      rm(model)
      gc()
    }

    # Update bandit probabilities ----
    RL_Update_Output <- RL_ML_Update(ModelType=ModelType, grid_eval_metric=grid_eval_metric., Iteration=counter, NewGrid.=NewGrid, NewPerformance.=NewPerformance, BestPerformance.=BestPerformance, Trials.=Trials, Successes.=Successes, GridIDs.=GridIDs, BanditArmsN.=BanditArmsN, RunsWithoutNewWinner.=RunsWithoutNewWinner, MaxRunsWithoutNewWinner.=MaxRunsWithoutNewWinner, MaxModelsInGrid.=MaxModelsInGrid, MaxRunMinutes.=MaxRunMinutes, TotalRunTime.=TotalRunTime, BanditProbs.=BanditProbs)
    BanditProbs <- RL_Update_Output$BanditProbs
    NewGrid <- RL_Update_Output$NewGrid
    Trials <- RL_Update_Output$Trials

    # Continue or stop ----
    if(RL_Update_Output$BreakLoop != "stay") break

    # Update collection table ----
    data.table::set(ExperimentalGrid, i = counter+1L, j = "GridNumber", value = NewGrid)
    data.table::set(ExperimentalGrid, i = counter+1L, j = "NTrees", value = GridClusters[[paste0("Grid_", NewGrid)]][["NTrees"]][Trials[NewGrid]+1L])
    data.table::set(ExperimentalGrid, i = counter+1L, j = "Depth", value = GridClusters[[paste0("Grid_", NewGrid)]][["Depth"]][Trials[NewGrid]+1L])
    data.table::set(ExperimentalGrid, i = counter+1L, j = "LearningRate", value = GridClusters[[paste0("Grid_", NewGrid)]][["LearningRate"]][Trials[NewGrid]+1L])
    data.table::set(ExperimentalGrid, i = counter+1L, j = "MinChildWeight", value = GridClusters[[paste0("Grid_", NewGrid)]][["MinChildWeight"]][Trials[NewGrid]+1L])
    data.table::set(ExperimentalGrid, i = counter+1L, j = "SubSample", value = GridClusters[[paste0("Grid_", NewGrid)]][["SubSample"]][Trials[NewGrid]+1L])
    data.table::set(ExperimentalGrid, i = counter+1L, j = "ColSampleByTree", value = GridClusters[[paste0("Grid_", NewGrid)]][["ColSampleByTree"]][Trials[NewGrid]+1L])

    # Update bandit probs
    for(bandit in seq_along(BanditProbs)) data.table::set(ExperimentalGrid, i = counter+1L, j = paste0("BanditProbs_Grid_",bandit), value = BanditProbs[bandit])

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
  PositiveClassificationMeasures <- c('Utility','MCC','Acc','F1_Score','F2_Score','F0.5_Score','TPR','TNR','NPV','PPV','ThreatScore')
  PositiveRegressionMeasures <- c('r2')
  PositiveMultiClassMeasures <- c('AUC', 'Accuracy')
  if(grid_eval_metric. %chin% c(PositiveClassificationMeasures, PositiveRegressionMeasures, PositiveMultiClassMeasures)) {
    BestGrid <- ExperimentalGrid[order(-EvalMetric)][1L]
  } else {
    BestGrid <- ExperimentalGrid[order(EvalMetric)][1L]
  }

  # Return
  return(list(ExperimentalGrid = ExperimentalGrid, BestGrid = BestGrid))
}

#' @title LightGBMGridTuner
#'
#' @description Execute grid tuning for lightgbm
#'
#' @author Adrian Antico
#' @family Reinforcement Learning
#'
#' @param ModelType "classification"
#' @param TrainOnFull. TrainOnFull
#' @param TargetColumnName. TargetColumnName
#' @param DebugMode. DebugMode
#' @param num_iterations. num_iterations
#' @param max_depth. max_depth
#' @param eta. eta
#' @param num_leaves. num_leaves
#' @param min_data_in_leaf. min_data_in_leaf
#' @param bagging_freq. bagging_freq
#' @param bagging_fraction. bagging_fraction
#' @param feature_fraction. feature_fraction
#' @param feature_fraction_bynode. feature_fraction_bynode
#' @param lambda_l1. lambda_l1
#' @param lambda_l2. lambda_l2
#' @param LossFunction LossFunction
#' @param EvalMetric EvalMetric
#' @param grid_eval_metric. MultiClass
#' @param MetricPeriods MetricPeriods
#' @param ClassWeights ClassWeights
#' @param CostMatrixWeights CostMatrixWeights
#' @param model_path model_path
#' @param TargetColumnName. TargetColumnName
#' @param datatrain. datatrain
#' @param dataTest. dataTest
#' @param TestData. TestData
#' @param EvalSets. EvalSets
#' @param TestTarget. TestTarget
#' @param FinalTestTarget. FinalTestTarget
#' @param TargetLevels. TargetLevels
#' @param MaxRunsWithoutNewWinner MaxRunsWithoutNewWinner
#' @param MaxModelsInGrid MaxModelsInGrid
#' @param MaxRunMinutes MaxRunMinutes
#' @param SaveModelObjects SaveModelObjects
#' @param metadata_path metadata_path
#' @param model_path model_path
#' @param ModelID ModelID
#' @param BaselineComparison. BaselineComparison
#' @param Verbose. Verbose
#' @param NumLevels. NumLevels
#'
#' @noRd
LightGBMGridTuner <- function(ModelType="classification",
                              TrainOnFull.=TrainOnFull,
                              DebugMode.=DebugMode.,
                              params. = params,

                              # CatBoostParameterGrids
                              num_iterations.=params$num_iterations,
                              max_depth.=params$max_depth,
                              eta.=params$eta,
                              num_leaves.=params$num_leaves,
                              min_data_in_leaf.=params$min_data_in_leaf,
                              bagging_freq.=params$bagging_freq,
                              bagging_fraction.=params$bagging_fraction,
                              feature_fraction.=params$feature_fraction,
                              feature_fraction_bynode.=params$feature_fraction_bynode,
                              lambda_l1.=params$lambda_l1,
                              lambda_l2.=params$lambda_l2,

                              # CatBoostGridParams
                              LossFunction=LossFunction,
                              EvalMetric=eval_metric,
                              grid_eval_metric.=grid_eval_metric,
                              CostMatrixWeights=CostMatrixWeights,

                              # Model Building
                              TargetColumnName. = TargetColumnName,
                              datatrain. = datatrain,
                              dataTest. = dataTest,
                              TestData. = TestData,
                              EvalSets. = EvalSets,
                              TestTarget. = TestTarget,
                              FinalTestTarget. = FinalTestTarget,
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
                              NumLevels.=NumLevels) {

  # Kicking off grid tuning ----
  if(DebugMode.) print("Grid Tuning Start")

  # Enables grid tuning so that min_data_in_leaf can update
  params.[["feature_pre_filter"]] <- FALSE
  params.[["predict_disable_shape_check"]] <- TRUE
  if(ModelType == "multiclass") params.[["num_class"]] <- NumLevels.

  # Generate grid sets ----
  if(DebugMode.) print("Generate grid sets")
  Grids <- LightGBMParameterGrids(num_iterations=num_iterations., max_depth=max_depth., eta=eta., num_leaves=num_leaves., min_data_in_leaf=min_data_in_leaf., bagging_freq=bagging_freq., bagging_fraction=bagging_fraction., feature_fraction=feature_fraction., feature_fraction_bynode=feature_fraction_bynode., lambda_l1=lambda_l1., lambda_l2=lambda_l2.)
  Grid <- Grids$Grid
  GridClusters <- Grids$Grids
  ExperimentalGrid <- Grids$ExperimentalGrid

  # Initialize RL objects ----
  if(DebugMode.) print("Initialize RL objects")
  RL_Start <- RL_Initialize(ParameterGridSet = GridClusters, Alpha = 1L, Beta = 1L, SubDivisions = 1000L)
  RunsWithoutNewWinner <- 0L
  BanditProbs <- RL_Start$BanditProbs
  BanditArmsN <- RL_Start$BanditArmsN
  Successes <- RL_Start$Successes
  GridIDs <- RL_Start$GridIDs
  Trials <- RL_Start$Trials
  rm(RL_Start)

  # Initalize counters ----
  counter <- 0L; NewGrid <- 1L; BestPerformance <- 1L

  # Grid Tuning Main Loop ----
  if(DebugMode.) print("Grid Tuning Main Loop")
  repeat {

    # Increment counter ----
    counter <- counter + 1L

    # Once all arms have been tried once
    if(counter > BanditArmsN+1L) {

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

      # Define parameters
      if(counter != 1L) {
        base_params <- LightGBMGridParams(N.=N, params=params., counter.=counter, BanditArmsN.=BanditArmsN, model_path.=model_path, NewGrid.=NewGrid, Grid.=Grid, GridClusters.=GridClusters)
      } else {
        base_params <- params.
        for(z in seq_along(base_params)) if(length(base_params[[z]]) > 1L) base_params[[z]] <- base_params[[z]][length(base_params[[z]])]
      }

      # Multiclass
      if(ModelType == "multiclass") base_params[["num_class"]] <- NumLevels.

      # Build model
      RunTime <- system.time(model <- lightgbm::lgb.train(params=base_params, data=datatrain., valids=EvalSets., nrounds = 5L))

      # Binary Grid Score Model
      if(!is.null(TestData.)) {
        if(ModelType == "classification") {
          predict <- predict(model, as.matrix(TestData.))
          ValidationData <- data.table::as.data.table(cbind(Target = FinalTestTarget., p1 = predict))
          data.table::setnames(ValidationData, "Target", TargetColumnName.)
        } else if(ModelType == "regression") {
          predict <- predict(model, as.matrix(TestData.))
          ValidationData <- data.table::as.data.table(cbind(Target = FinalTestTarget., Predict = predict))
          data.table::setnames(ValidationData, "Target", TargetColumnName.)
        } else {
          predict <- XGBoostMultiClassPredict(model=model, datatest=as.matrix(TestData.), TargetLevels=TargetLevels., NumLevels=NumLevels., NumberRows=nrow(TestData.))
          ValidationData <- data.table::as.data.table(cbind(Target = FinalTestTarget., predict))
          data.table::setnames(ValidationData, "Target", TargetColumnName.)
          data.table::setkeyv(ValidationData, cols = TargetColumnName.)
          data.table::setkeyv(TargetLevels., cols = "NewLevels")
          ValidationData[TargetLevels., OriginalLevels := i.OriginalLevels][, eval(TargetColumnName.) := OriginalLevels][, OriginalLevels := NULL]
        }
      } else {
        if(ModelType == "classification") {
          predict <- predict(model, as.matrix(dataTest.))
          ValidationData <- data.table::as.data.table(cbind(Target = TestTarget., p1 = predict))
          data.table::setnames(ValidationData, "Target", TargetColumnName.)
        } else if(ModelType == "regression") {
          predict <- predict(model, as.matrix(dataTest.))
          ValidationData <- data.table::as.data.table(cbind(Target = TestTarget., Predict = predict))
          data.table::setnames(ValidationData, "Target", TargetColumnName.)
        } else {
          predict <- XGBoostMultiClassPredict(model=model, datatest=as.matrix(dataTest.), TargetLevels=TargetLevels., NumLevels=NumLevels., NumberRows=nrow(dataTest.))
          ValidationData <- data.table::as.data.table(cbind(Target = TestTarget., predict))
          data.table::setnames(ValidationData, "Target", TargetColumnName.)
          data.table::setkeyv(ValidationData, cols = TargetColumnName.)
          data.table::setkeyv(TargetLevels., cols = "NewLevels")
          ValidationData[TargetLevels., OriginalLevels := i.OriginalLevels][, eval(TargetColumnName.) := OriginalLevels][, OriginalLevels := NULL]
        }
      }

      # Eval metrics
      if(tolower(ModelType) == "classification") {
        EvalMetrics <- BinaryMetrics(ClassWeights.=NULL, CostMatrixWeights.=CostMatrixWeights, SaveModelObjects.=SaveModelObjects, ValidationData.=ValidationData, TrainOnFull.=TrainOnFull., TargetColumnName.=TargetColumnName., ModelID.=ModelID, model_path.=model_path, metadata_path.=metadata_path)
      } else if(tolower(ModelType) == "regression") {
        EvalMetrics <- RegressionMetrics(SaveModelObjects.=SaveModelObjects, data.=data, ValidationData.=ValidationData, TrainOnFull.=TrainOnFull., LossFunction.="Adrian", EvalMetric.=NULL, TargetColumnName.=TargetColumnName., ModelID.=ModelID, model_path.=model_path, metadata_path.=metadata_path)
      } else if(tolower(ModelType) == "multiclass") {
        EvalMetrics <- MultiClassMetrics(ModelClass="xgboost", DataType = "validate", SaveModelObjects.=FALSE, ValidationData.=ValidationData, PredictData.=predict, TrainOnFull.=FALSE, TargetColumnName.=TargetColumnName., TargetLevels.=TargetLevels., ModelID.=ModelID, model_path.=model_path, metadata_path.=metadata_path)
      }

      # New performance
      if(ModelType == "classification") {
        if(grid_eval_metric. %chin% c("Utility", "MCC", "Acc", "F1_Score", "F2_Score", "F0.5_Score", "NPV", "PPV", "TPR", "TNR", "ThreatScore")) {
          NewPerformance <- EvalMetrics[order(-get(grid_eval_metric.))][[eval(grid_eval_metric.)]][[1L]]
        } else {
          NewPerformance <- EvalMetrics[order(get(grid_eval_metric.))][[eval(grid_eval_metric.)]][[1L]]
        }
      } else {
        NewPerformance <- EvalMetrics[toupper(Metric) == eval(toupper(grid_eval_metric.))][["MetricValue"]]
      }

      # Update Experimental Grid with Param values
      data.table::set(ExperimentalGrid, i = counter, j = "GridNumber", value = if(counter == 1L) 0 else NewGrid)
      data.table::set(ExperimentalGrid, i = counter, j = "RunTime", value = RunTime[[3L]])
      data.table::set(ExperimentalGrid, i = counter, j = "EvalMetric", value = NewPerformance)

      # Total Run Time
      TotalRunTime <- ExperimentalGrid[RunTime != -1L, sum(RunTime, na.rm = TRUE)]

      # Binary Remove Model and Collect Garbage
      rm(model)
      gc()
    }

    # Update bandit probabilities ----
    RL_Update_Output <- RL_ML_Update(ModelType=ModelType, grid_eval_metric=grid_eval_metric., Iteration=counter, NewGrid.=NewGrid, NewPerformance.=NewPerformance, BestPerformance.=BestPerformance, Trials.=Trials, Successes.=Successes, GridIDs.=GridIDs, BanditArmsN.=BanditArmsN, RunsWithoutNewWinner.=RunsWithoutNewWinner, MaxRunsWithoutNewWinner.=MaxRunsWithoutNewWinner, MaxModelsInGrid.=MaxModelsInGrid, MaxRunMinutes.=MaxRunMinutes, TotalRunTime.=TotalRunTime, BanditProbs.=BanditProbs)
    BanditProbs <- RL_Update_Output$BanditProbs
    NewGrid <- RL_Update_Output$NewGrid
    Trials <- RL_Update_Output$Trials

    # Continue or stop ----
    if(RL_Update_Output$BreakLoop != "stay") break

    # Update collection table ----
    data.table::set(ExperimentalGrid, i = counter+1L, j = "GridNumber", value = NewGrid)
    data.table::set(ExperimentalGrid, i = counter+1L, j = "num_iterations", value = GridClusters[[paste0("Grid_",NewGrid)]][["num_iterations"]][Trials[NewGrid]+1L])
    data.table::set(ExperimentalGrid, i = counter+1L, j = "max_depth", value = GridClusters[[paste0("Grid_",NewGrid)]][["max_depth"]][Trials[NewGrid]+1L])
    data.table::set(ExperimentalGrid, i = counter+1L, j = "eta", value = GridClusters[[paste0("Grid_",NewGrid)]][["eta"]][Trials[NewGrid]+1L])
    data.table::set(ExperimentalGrid, i = counter+1L, j = "num_leaves", value = GridClusters[[paste0("Grid_",NewGrid)]][["num_leaves"]][Trials[NewGrid]+1L])
    data.table::set(ExperimentalGrid, i = counter+1L, j = "min_data_in_leaf", value = GridClusters[[paste0("Grid_",NewGrid)]][["min_data_in_leaf"]][Trials[NewGrid]+1L])
    data.table::set(ExperimentalGrid, i = counter+1L, j = "bagging_freq", value = GridClusters[[paste0("Grid_",NewGrid)]][["bagging_freq"]][Trials[NewGrid]+1L])
    data.table::set(ExperimentalGrid, i = counter+1L, j = "bagging_fraction", value = GridClusters[[paste0("Grid_",NewGrid)]][["bagging_fraction"]][Trials[NewGrid]+1L])
    data.table::set(ExperimentalGrid, i = counter+1L, j = "feature_fraction", value = GridClusters[[paste0("Grid_",NewGrid)]][["feature_fraction"]][Trials[NewGrid]+1L])
    data.table::set(ExperimentalGrid, i = counter+1L, j = "feature_fraction_bynode", value = GridClusters[[paste0("Grid_",NewGrid)]][["feature_fraction_bynode"]][Trials[NewGrid]+1L])
    data.table::set(ExperimentalGrid, i = counter+1L, j = "lambda_l1", value = GridClusters[[paste0("Grid_",NewGrid)]][["lambda_l1"]][Trials[NewGrid]+1L])
    data.table::set(ExperimentalGrid, i = counter+1L, j = "lambda_l2", value = GridClusters[[paste0("Grid_",NewGrid)]][["lambda_l2"]][Trials[NewGrid]+1L])

    # Update bandit probs
    for(bandit in seq_along(BanditProbs)) data.table::set(ExperimentalGrid, i = counter+1L, j = paste0("BanditProbs_Grid_",bandit), value = BanditProbs[bandit])

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
  PositiveClassificationMeasures <- c('Utility','MCC','Acc','F1_Score','F2_Score','F0.5_Score','TPR','TNR','NPV','PPV','ThreatScore')
  PositiveRegressionMeasures <- c('r2')
  PositiveMultiClassMeasures <- c('AUC', 'Accuracy')
  if(grid_eval_metric. %chin% c(PositiveClassificationMeasures, PositiveRegressionMeasures, PositiveMultiClassMeasures)) {
    BestGrid <- ExperimentalGrid[order(-EvalMetric)][1L]
  } else {
    BestGrid <- ExperimentalGrid[order(EvalMetric)][1L]
  }

  # Return
  return(list(ExperimentalGrid = ExperimentalGrid, BestGrid = BestGrid))
}
