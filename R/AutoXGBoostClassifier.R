#' @title AutoXGBoostClassifier
#'
#' @description AutoXGBoostClassifier is an automated XGBoost modeling framework with grid-tuning and model evaluation that runs a variety of steps. First, a stratified sampling (by the target variable) is done to create train and validation sets. Then, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation plot, evaluation boxplot, evaluation metrics, variable importance, partial dependence calibration plots, partial dependence calibration box plots, and column names used in model fitting.
#'
#' @author Adrian Antico
#' @family Automated Supervised Learning - Binary Classification
#'
#' @param data This is your data set for training and testing your model
#' @param TrainOnFull Set to TRUE to train on full data
#' @param ValidationData This is your holdout data set used in modeling either refine your hyperparameters.
#' @param TestData This is your holdout data set. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TargetColumnName Either supply the target column name OR the column number where the target is located (but not mixed types). Note that the target column needs to be a 0 | 1 numeric variable.
#' @param FeatureColNames Either supply the feature column names OR the column number where the target is located (but not mixed types)
#' @param IDcols A vector of column names or column numbers to keep in your data but not include in the modeling.
#' @param LossFunction Select from 'reg:logistic', "binary:logistic"
#' @param CostMatrixWeights A vector with 4 elements c(True Positive Cost, False Negative Cost, False Positive Cost, True Negative Cost). Default c(1,0,0,1),
#' @param eval_metric This is the metric used to identify best grid tuned model. Choose from "logloss","error","aucpr","auc"
#' @param NThreads Set the maximum number of threads you'd like to dedicate to the model run. E.g. 8
#' @param TreeMethod Choose from "hist", "gpu_hist"
#' @param model_path A character string of your path file to where you want your output saved
#' @param SaveInfoToPDF Set to TRUE to save modeling information to PDF. If model_path or metadata_path aren't defined then output will be saved to the working directory
#' @param metadata_path A character string of your path file to where you want your model evaluation output saved. If left NULL, all output will be saved to model_path.
#' @param ModelID A character string to name your model and output
#' @param NumOfParDepPlots Tell the function the number of partial dependence calibration plots you want to create.
#' @param Verbose Set to 0 if you want to suppress model evaluation updates in training
#' @param ReturnModelObjects Set to TRUE to output all modeling objects (E.g. plots and evaluation metrics)
#' @param ReturnFactorLevels TRUE or FALSE. Set to FALSE to not return factor levels.
#' @param SaveModelObjects Set to TRUE to return all modeling objects to your environment
#' @param GridTune Set to TRUE to run a grid tuning procedure
#' @param Trees Bandit grid partitioned. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the trees numbers you want to test. For running grid tuning, a NULL value supplied will mean these values are tested seq(1000L, 10000L, 1000L)
#' @param eta Bandit grid partitioned. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the LearningRate values to test. For running grid tuning, a NULL value supplied will mean these values are tested c(0.01,0.02,0.03,0.04)
#' @param max_depth Bandit grid partitioned. Number, or vector for depth to test.  For running grid tuning, a NULL value supplied will mean these values are tested seq(4L, 16L, 2L)
#' @param min_child_weight Number, or vector for min_child_weight to test.  For running grid tuning, a NULL value supplied will mean these values are tested seq(1.0, 10.0, 1.0)
#' @param subsample Number, or vector for subsample to test.  For running grid tuning, a NULL value supplied will mean these values are tested seq(0.55, 1.0, 0.05)
#' @param colsample_bytree Number, or vector for colsample_bytree to test.  For running grid tuning, a NULL value supplied will mean these values are tested seq(0.55, 1.0, 0.05)
#' @param PassInGrid Default is NULL. Provide a data.table of grid options from a previous run.
#' @param MaxModelsInGrid Number of models to test from grid options.
#' @param MaxRunsWithoutNewWinner A number
#' @param MaxRunMinutes In minutes
#' @param BaselineComparison Set to either "default" or "best". Default is to compare each successive model build to the baseline model using max trees (from function args). Best makes the comparison to the current best model.
#' @param DebugMode TRUE to print to console the steps taken
#' @examples
#' \dontrun{
#' # Create some dummy correlated data
#' data <- RemixAutoML::FakeDataGenerator(
#'   Correlation = 0.85,
#'   N = 1000L,
#'   ID = 2L,
#'   ZIP = 0L,
#'   AddDate = FALSE,
#'   Classification = TRUE,
#'   MultiClass = FALSE)
#'
#' # Run function
#' TestModel <- RemixAutoML::AutoXGBoostClassifier(
#'
#'     # GPU or CPU
#'     TreeMethod = "hist",
#'     NThreads = parallel::detectCores(),
#'
#'     # Metadata args
#'     model_path = normalizePath("./"),
#'     metadata_path = NULL,
#'     ModelID = "Test_Model_1",
#'     ReturnFactorLevels = TRUE,
#'     ReturnModelObjects = TRUE,
#'     SaveModelObjects = FALSE,
#'     SaveInfoToPDF = FALSE,
#'
#'     # Data args
#'     data = data,
#'     TrainOnFull = FALSE,
#'     ValidationData = NULL,
#'     TestData = NULL,
#'     TargetColumnName = "Adrian",
#'     FeatureColNames = names(data)[!names(data) %in%
#'       c("IDcol_1", "IDcol_2","Adrian")],
#'     IDcols = c("IDcol_1","IDcol_2"),
#'
#'     # Model evaluation
#'     LossFunction = 'reg:logistic',
#'     CostMatrixWeights = c(1,0,0,1),
#'     eval_metric = "auc",
#'     NumOfParDepPlots = 3L,
#'
#'     # Grid tuning args
#'     PassInGrid = NULL,
#'     GridTune = FALSE,
#'     BaselineComparison = "default",
#'     MaxModelsInGrid = 10L,
#'     MaxRunsWithoutNewWinner = 20L,
#'     MaxRunMinutes = 24L*60L,
#'     Verbose = 1L,
#'
#'     # ML args
#'     Trees = 500L,
#'     eta = 0.30,
#'     max_depth = 9L,
#'     min_child_weight = 1.0,
#'     subsample = 1,
#'     colsample_bytree = 1,
#'     DebugMode = FALSE)
#' }
#' @return Saves to file and returned in list: VariableImportance.csv, Model, ValidationData.csv, EvalutionPlot.png, EvaluationMetrics.csv, ParDepPlots.R a named list of features with partial dependence calibration plots, GridCollect, and GridList
#' @export
AutoXGBoostClassifier <- function(data,
                                  TrainOnFull = FALSE,
                                  ValidationData = NULL,
                                  TestData = NULL,
                                  TargetColumnName = NULL,
                                  FeatureColNames = NULL,
                                  IDcols = NULL,
                                  model_path = NULL,
                                  metadata_path = NULL,
                                  SaveInfoToPDF = FALSE,
                                  ModelID = "FirstModel",
                                  ReturnFactorLevels = TRUE,
                                  ReturnModelObjects = TRUE,
                                  SaveModelObjects = FALSE,
                                  Verbose = 0L,
                                  NumOfParDepPlots = 3L,
                                  NThreads = max(1L, parallel::detectCores()-2L),
                                  LossFunction = 'reg:logistic',
                                  CostMatrixWeights = c(1,0,0,1),
                                  eval_metric = "auc",
                                  TreeMethod = "hist",
                                  GridTune = FALSE,
                                  BaselineComparison = "default",
                                  MaxModelsInGrid = 10L,
                                  MaxRunsWithoutNewWinner = 20L,
                                  MaxRunMinutes = 24L*60L,
                                  PassInGrid = NULL,
                                  Trees = 1000L,
                                  eta = 0.30,
                                  max_depth = 9,
                                  min_child_weight = 1,
                                  subsample = 1,
                                  colsample_bytree = 1,
                                  DebugMode = FALSE) {

  # Check args ----
  XGBoostArgsCheck(GridTune.=GridTune, model_path.=model_path, metadata_path.=metadata_path, Trees.=Trees, max_depth.=max_depth, eta.=eta, min_child_weight.=min_child_weight, subsample.=subsample, colsample_bytree.=colsample_bytree)

  # Data prep ----
  Output <- XGBoostDataPrep(ModelType="classification", data.=data, ValidationData.=ValidationData, TestData.=TestData, TargetColumnName.=TargetColumnName, FeatureColNames.=FeatureColNames, IDcols.=IDcols, TransformNumericColumns.=NULL, Methods.=NULL, ModelID.=ModelID, model_path.=model_path, TrainOnFull.=TrainOnFull, SaveModelObjects.=SaveModelObjects, ReturnFactorLevels.=ReturnFactorLevels)
  FactorLevelsList <- Output$FactorLevelsList; Output$FactorLevelsList <- NULL
  FinalTestTarget <- Output$FinalTestTarget; Output$FinalTestTarget <- NULL
  datavalidate <- Output$datavalidate; Output$datavalidate <- NULL
  TrainTarget <- Output$TrainTarget; Output$TrainTarget <- NULL
  TestTarget <- Output$TestTarget; Output$TestTarget <- NULL
  datatrain <- Output$datatrain; Output$datatrain <- NULL
  TestMerge <- Output$TestMerge; Output$TestMerge <- NULL
  TestData <- Output$TestData.; Output$TestData. <- NULL
  datatest <- Output$datatest; Output$datatest <- NULL
  EvalSets <- Output$EvalSets; Output$EvalSets <- NULL
  dataTest <- Output$dataTest; Output$dataTest <- NULL
  IDcols <- Output$IDcols; Output$IDcols <- NULL
  Names <- Output$Names; rm(Output)

  # Bring into existence
  ExperimentalGrid <- NULL; BestGrid <- NULL

  # Grid tuning ----
  if(GridTune) {
    Output <- GridTuner(AlgoType="xgboost", ModelType="classification", TrainOnFull.=TrainOnFull, BaselineComparison.=BaselineComparison, HasTime=NULL, TargetColumnName.=TargetColumnName, DebugMode.=DebugMode, task_type.=task_type, Trees.=Trees, Depth.=Depth, LearningRate.=LearningRate, L2_Leaf_Reg.=L2_Leaf_Reg, BorderCount.=BorderCount, RandomStrength.=RandomStrength, RSM.=RSM, BootStrapType.=BootStrapType, GrowPolicy.=GrowPolicy, NumGPUs=NumGPUs, LossFunction=LossFunction, EvalMetric=EvalMetric, MetricPeriods=MetricPeriods, ClassWeights=ClassWeights, CostMatrixWeights=CostMatrixWeights, data=data, TrainPool.=TrainPool, TestPool.=TestPool, FinalTestTarget.=FinalTestTarget, TestTarget.=TestTarget, FinalTestPool.=FinalTestPool, TestData.=TestData, TestMerge.=TestMerge, TargetLevels.=NULL, MaxRunsWithoutNewWinner=MaxRunsWithoutNewWinner, MaxModelsInGrid=MaxModelsInGrid, MaxRunMinutes=MaxRunMinutes, SaveModelObjects=SaveModelObjects, metadata_path=metadata_path, model_path=model_path, ModelID=ModelID, grid_eval_metric.=grid_eval_metric)
    ExperimentalGrid <- Output$ExperimentalGrid
    BestGrid <- Output$BestGrid
  }

  # Binary Grid Tune or Not Check----
  if(GridTune & !TrainOnFull) {

    # Pull in Grid sets----
    Grids <- XGBoostParameterGrids(TaskType=TreeMethod,Shuffles=Shuffles,NTrees=Trees,Depth=max_depth,LearningRate=eta,MinChildWeight=min_child_weight,SubSample=subsample,ColSampleByTree=colsample_bytree)
    Grid <- Grids$Grid
    GridClusters <- Grids$Grids
    ExperimentalGrid <- Grids$ExperimentalGrid

    # Initialize RL----
    RL_Start <- RL_Initialize(ParameterGridSet = GridClusters, Alpha = 1L, Beta = 1L, SubDivisions = 1000L)
    BanditArmsN <- RL_Start[["BanditArmsN"]]
    Successes <- RL_Start[["Successes"]]
    Trials <- RL_Start[["Trials"]]
    GridIDs <- RL_Start[["GridIDs"]]
    BanditProbs <- RL_Start[["BanditProbs"]]
    RunsWithoutNewWinner <- 0L
    rm(RL_Start)

    # Add bandit probs columns to ExperimentalGrid----
    data.table::set(ExperimentalGrid, j = paste0("BanditProbs_", names(GridClusters)), value = -10)

    # Binary Grid Tuning Main Loop----
    counter <- 0L
    TotalRunTime <- 0
    repeat {

      # Increment counter----
      counter <- counter + 1L

      # Check if grid still has elements in it----
      if(!exists("NewGrid")) zz <- counter else zz <- NewGrid
      if(!is.null(GridClusters[[paste0("Grid_",max(1L,zz-1L))]][["Depth"]][1L])) {

        # Define parameters----
        if(!exists("NewGrid")) {
          base_params <- XGBoostClassifierParams(counter=counter,Objective=LossFunction,BanditArmsN=BanditArmsN,eval_metric=eval_metric,task_type=TreeMethod,model_path=model_path,Grid=Grid,ExperimentalGrid=ExperimentalGrid,GridClusters=GridClusters)
        } else {
          base_params <- XGBoostClassifierParams(NewGrid=NewGrid,Objective=LossFunction,counter=counter,BanditArmsN=BanditArmsN,eval_metric=eval_metric,task_type=TreeMethod,model_path=model_path,Grid=Grid,ExperimentalGrid=ExperimentalGrid,GridClusters=GridClusters)
        }

        # Run model----
        if(counter <= BanditArmsN + 1L) {
          if(counter == 1L) {
            nrounds <- max(Grid$NTrees)
            print(base_params)
            RunTime <- system.time(model <- model <- xgboost::xgb.train(params=base_params, data=datatrain, nrounds = nrounds, watchlist=EvalSets, verbose=Verbose))
          } else {
            nrounds <- GridClusters[[paste0("Grid_",counter-1L)]][["NTrees"]][1L]
            print(base_params)
            RunTime <- system.time(model <- model <- xgboost::xgb.train(params=base_params, data=datatrain, nrounds = nrounds, watchlist=EvalSets, verbose=Verbose))
          }
        } else {
          nrounds <- GridClusters[[paste0("Grid_",NewGrid)]][["NTrees"]][1L]
          print(base_params)
          RunTime <- system.time(model <- model <- xgboost::xgb.train(params=base_params, data=datatrain, nrounds = nrounds, watchlist=EvalSets, verbose=Verbose))
        }

        # Binary Grid Score Model----
        if(!is.null(TestData)) {
          predict <- stats::predict(model, datatest)
          calibEval <- data.table::as.data.table(cbind(Target = FinalTestTarget, p1 = predict))
          AUC_Metrics <- pROC::roc(response = calibEval[["Target"]], predictor = calibEval[["p1"]], na.rm = TRUE, algorithm = 3L, auc = TRUE, ci = TRUE)
        } else {
          predict <- stats::predict(model, datavalidate)
          calibEval <- data.table::as.data.table(cbind(Target = TestTarget, p1 = predict))
          AUC_Metrics <- pROC::roc(response = calibEval[["Target"]], predictor = calibEval[["p1"]], na.rm = TRUE, algorithm = 3L, auc = TRUE, ci = TRUE)
        }

        # Update Experimental Grid with Param values----
        if(!exists("NewGrid")) {
          GridNumber <- counter - 1L
          data.table::set(ExperimentalGrid, i = counter, j = "GridNumber", value = GridNumber)
        } else {
          data.table::set(ExperimentalGrid, i = counter, j = "GridNumber", value = NewGrid)
        }
        NewPerformance <- as.numeric(AUC_Metrics$auc)
        data.table::set(ExperimentalGrid, i = counter, j = "RunTime", value = RunTime[[3L]])
        data.table::set(ExperimentalGrid, i = counter, j = "EvalMetric", value = NewPerformance)
        data.table::set(ExperimentalGrid, i = counter, j = "TreesBuilt", value = model$niter)
        if(counter == 1L) {
          BestPerformance <- 1L
        } else {
          if(tolower(BaselineComparison) == "default") {
            BestPerformance <- ExperimentalGrid[RunNumber == 1L][["EvalMetric"]]
          } else {
            BestPerformance <- ExperimentalGrid[RunNumber < counter, max(EvalMetric, na.rm = TRUE)]
          }
        }

        # Performance measures----
        TotalRunTime <- ExperimentalGrid[RunTime != -1L, sum(RunTime, na.rm = TRUE)]
        if(NewPerformance > BestPerformance) {
          RunsWithoutNewWinner <- 0L
        } else {
          RunsWithoutNewWinner <- RunsWithoutNewWinner + 1L
        }

        # Binary Remove Model and Collect Garbage----
        rm(model)
        gc()
      }

      # Update bandit probabilities and whatnot----
      RL_Update_Output <- RL_ML_Update(ModelType="classification", Iteration=counter, NewGrid.=NewGrid, NewPerformance.=NewPerformance, BestPerformance.=BestPerformance, Trials.=Trials, Successes.=Successes, GridIDs.=GridIDs, BanditArmsN.=BanditArmsN, RunsWithoutNewWinner.=RunsWithoutNewWinner, MaxRunsWithoutNewWinner.=MaxRunsWithoutNewWinner, MaxModelsInGrid.=MaxModelsInGrid, MaxRunMinutes.=MaxRunMinutes, TotalRunTime.=TotalRunTime, BanditProbs.=BanditProbs)
      BanditProbs <- RL_Update_Output[["BanditProbs"]]
      Trials <- RL_Update_Output[["Trials"]]
      Successes <- RL_Update_Output[["Successes"]]
      NewGrid <- RL_Update_Output[["NewGrid"]]

      # Continue or stop----
      if(RL_Update_Output$BreakLoop != "stay") break else print("still going")
      data.table::set(ExperimentalGrid, i = counter+1L, j = "GridNumber", value = NewGrid)
      data.table::set(ExperimentalGrid, i = counter+1L, j = "NTrees", value = GridClusters[[paste0("Grid_",NewGrid)]][["NTrees"]][Trials[NewGrid]+1L])
      data.table::set(ExperimentalGrid, i = counter+1L, j = "Depth", value = GridClusters[[paste0("Grid_",NewGrid)]][["Depth"]][Trials[NewGrid]+1L])
      data.table::set(ExperimentalGrid, i = counter+1L, j = "LearningRate", value = GridClusters[[paste0("Grid_",NewGrid)]][["LearningRate"]][Trials[NewGrid]+1L])
      data.table::set(ExperimentalGrid, i = counter+1L, j = "MinChildWeight", value = GridClusters[[paste0("Grid_",NewGrid)]][["MinChildWeight"]][Trials[NewGrid]+1L])
      data.table::set(ExperimentalGrid, i = counter+1L, j = "SubSample", value = GridClusters[[paste0("Grid_",NewGrid)]][["SubSample"]][Trials[NewGrid]+1L])
      data.table::set(ExperimentalGrid, i = counter+1L, j = "ColSampleByTree", value = GridClusters[[paste0("Grid_",NewGrid)]][["ColSampleByTree"]][Trials[NewGrid]+1L])
      for(bandit in seq_len(length(BanditProbs))) data.table::set(ExperimentalGrid, i = counter+1L, j = paste0("BanditProbs_Grid_",bandit), value = BanditProbs[bandit])
    }

    # Remove unneeded rows----
    ExperimentalGrid <- ExperimentalGrid[RunTime != -1L]
    gc()

    # Binary Save GridCollect and GridList----
    if(SaveModelObjects & GridTune) {
      if(!is.null(metadata_path)) {
        data.table::fwrite(ExperimentalGrid, file = file.path(normalizePath(metadata_path), paste0(ModelID, "ExperimentalGrid.csv")))
      } else {
        data.table::fwrite(ExperimentalGrid, file = file.path(normalizePath(model_path), paste0(ModelID, "ExperimentalGrid.csv")))
      }
    }
  }

  # Final Params ----
  Output <- XGBoostFinalParams(PassInGrid.=PassInGrid, BestGrid.=BestGrid, GridTune.=GridTune, LossFunction.=LossFunction, eval_metric.=eval_metric, NThreads.=NThreads, TreeMethod.=TreeMethod, Trees.=Trees)
  base_params <- Output$base_params
  NTrees <- Output$NTrees; rm(Output)

  # Build model ----
  model <- xgboost::xgb.train(params = base_params, data = datatrain, watchlist = EvalSets, nrounds = NTrees, Verbose = Verbose)

  # Binary Save Model ----
  if(SaveModelObjects) {
    if(getwd() == model_path) {
      xgboost::xgb.save(model = model, fname = ModelID)
    } else {
      save(model, file = file.path(model_path, ModelID))
    }
  }

  # Binary Grid Score Model ----
  predict <- stats::predict(model, if(!is.null(TestData)) datatest else if(!TrainOnFull) datavalidate else datatrain)

  # Binary Validation Data ----
  if(!is.null(TestData)) {
    ValidationData <- data.table::as.data.table(cbind(Target = FinalTestTarget, TestMerge, p1 = predict))
  } else if(!TrainOnFull) {
    ValidationData <- data.table::as.data.table(cbind(Target = TestTarget, dataTest, p1 = predict))
  } else {
    ValidationData <- data.table::as.data.table(cbind(Target = TrainTarget, dataTrain, p1 = predict))
  }
  data.table::setnames(ValidationData, "Target", TargetColumnName)

  # Binary Variable Importance ----
  VariableImportance <- tryCatch({data.table::as.data.table(xgboost::xgb.importance(model = model))}, error = function(x) NULL)
  if(!is.null(VariableImportance)) {
    VariableImportance[, ':=' (Gain = round(Gain, 4L), Cover = round(Cover, 4L), Frequency = round(Frequency, 4L))]
    if (SaveModelObjects) {
      if(!is.null(metadata_path)) {
        data.table::fwrite(VariableImportance, file = file.path(metadata_path, paste0(ModelID, "_VariableImportance.csv")))
      } else {
        data.table::fwrite(VariableImportance, file = file.path(model_path, paste0(ModelID, "_VariableImportance.csv")))
      }
    }
  }

  # Classification evaluation plots ----
  if(DebugMode) print("Running ML_EvalPlots()")
  Output <- ML_EvalPlots(ModelType="classification", TrainOnFull.=TrainOnFull, ValidationData.=ValidationData, NumOfParDepPlots.=NumOfParDepPlots, VariableImportance.=VariableImportance, TargetColumnName.=TargetColumnName, FeatureColNames.=FeatureColNames, SaveModelObjects.=SaveModelObjects, ModelID.=ModelID, metadata_path.=metadata_path, model_path.=model_path, LossFunction.=NULL, EvalMetric.=NULL, EvaluationMetrics.=NULL, predict.=NULL)
  EvaluationPlot <- Output$EvaluationPlot; Output$EvaluationPlot <- NULL
  ParDepPlots <- Output$ParDepPlots; Output$ParDepPlots <- NULL
  ROC_Plot <- Output$ROC_Plot; rm(Output)

  # Generate EvaluationMetrics ----
  if(DebugMode) print("Running BinaryMetrics()")
  EvalMetrics <- BinaryMetrics(ClassWeights.=NULL, CostMatrixWeights.=CostMatrixWeights, SaveModelObjects.=SaveModelObjects, ValidationData.=ValidationData, TrainOnFull.=TrainOnFull, TargetColumnName.=TargetColumnName, ModelID.=ModelID, model_path.=model_path, metadata_path.=metadata_path)

  # Send output to pdf ----
  if(DebugMode) print("Running CatBoostPDF()")
  CatBoostPDF(ModelClass = "xgboost", ModelType="classification", TrainOnFull.=TrainOnFull, SaveInfoToPDF.=SaveInfoToPDF, EvaluationPlot.=EvaluationPlot, EvaluationBoxPlot.=NULL, VariableImportance.=VariableImportance, ParDepPlots.=ParDepPlots, ParDepBoxPlots.=NULL, EvalMetrics.=EvalMetrics, Interaction.=NULL, model_path.=model_path, metadata_path.=metadata_path)

  # Binary Return Model Objects----
  if(!exists("FactorLevelsList")) FactorLevelsList <- NULL

  # Return objects ----
  if(ReturnModelObjects) {
    return(list(
      Model = model,
      ValidationData = ValidationData,
      ROC_Plot = ROC_Plot,
      EvaluationPlot = EvaluationPlot,
      EvaluationMetrics = EvalMetrics,
      VariableImportance = VariableImportance,
      VI_Plot = if(!is.null(VariableImportance)) tryCatch({if(all(c("plotly","dplyr") %chin% installed.packages())) plotly::ggplotly(VI_Plot(Type = "xgboost", VI_Data = VariableImportance)) else VI_Plot(Type = "xgboost", VI_Data = VariableImportance)}, error = function(x) NULL) else NULL,
      PartialDependencePlots = ParDepPlots,
      GridMetrics = if(GridTune) data.table::setorderv(ExperimentalGrid, cols = "EvalMetric", order = -1L, na.last = TRUE) else NULL,
      ColNames = Names,
      FactorLevels = FactorLevelsList))
  }
}
