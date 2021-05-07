#' @title AutoXGBoostMultiClass
#'
#' @description AutoXGBoostMultiClass is an automated XGBoost modeling framework with grid-tuning and model evaluation that runs a variety of steps. First, a stratified sampling (by the target variable) is done to create train and validation sets. Then, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation metrics, variable importance, and column names used in model fitting.
#'
#' @author Adrian Antico
#' @family Automated Supervised Learning - Multiclass Classification
#'
#' @param data This is your data set for training and testing your model
#' @param TrainOnFull Set to TRUE to train on full data
#' @param ValidationData This is your holdout data set used in modeling either refine your hyperparameters.
#' @param TestData This is your holdout data set. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TargetColumnName Either supply the target column name OR the column number where the target is located (but not mixed types). Note that the target column needs to be a 0 | 1 numeric variable.
#' @param FeatureColNames Either supply the feature column names OR the column number where the target is located (but not mixed types)
#' @param IDcols A vector of column names or column numbers to keep in your data but not include in the modeling.
#' @param eval_metric This is the metric used to identify best grid tuned model. Choose from "logloss","error","aucpr","auc"
#' @param NThreads Set the maximum number of threads you'd like to dedicate to the model run. E.g. 8
#' @param TreeMethod Choose from "hist", "gpu_hist"
#' @param model_path A character string of your path file to where you want your output saved
#' @param metadata_path A character string of your path file to where you want your model evaluation output saved. If left NULL, all output will be saved to model_path.
#' @param ModelID A character string to name your model and output
#' @param NumOfParDepPlots Tell the function the number of partial dependence calibration plots you want to create.
#' @param Verbose Set to 0 if you want to suppress model evaluation updates in training
#' @param EncodingMethod Choose from 'binary', 'poly_encode', 'backward_difference', 'helmert'
#' @param ReturnModelObjects Set to TRUE to output all modeling objects (E.g. plots and evaluation metrics)
#' @param ReturnFactorLevels TRUE or FALSE. Set to FALSE to not return factor levels.
#' @param SaveModelObjects Set to TRUE to return all modeling objects to your environment
#' @param GridTune Set to TRUE to run a grid tuning procedure
#' @param LossFunction 'multi:softmax'
#' @param grid_eval_metric "accuracy", "logloss", "microauc"
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
#' @param DebugMode Set to TRUE to get a print out of the steps taken internally
#' @examples
#' \dontrun{
#' # Create some dummy correlated data
#' data <- RemixAutoML::FakeDataGenerator(
#'   Correlation = 0.85,
#'   N = 1000L,
#'   ID = 2L,
#'   ZIP = 0L,
#'   AddDate = FALSE,
#'   Classification = FALSE,
#'   MultiClass = TRUE)
#'
#' # Run function
#' TestModel <- RemixAutoML::AutoXGBoostMultiClass(
#'
#'     # GPU or CPU
#'     TreeMethod = "hist",
#'     NThreads = parallel::detectCores(),
#'
#'     # Metadata args
#'     model_path = normalizePath("./"),
#'     metadata_path = normalizePath("./"),
#'     ModelID = "Test_Model_1",
#'     EncodingMethod = "binary",
#'     ReturnFactorLevels = TRUE,
#'     ReturnModelObjects = TRUE,
#'     SaveModelObjects = FALSE,
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
#'     # Model evaluation args
#'     eval_metric = "merror",
#'     LossFunction = 'multi:softmax',
#'     grid_eval_metric = "accuracy",
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
#'     DebugMode = FALSE,
#'
#'     # ML args
#'     Trees = 50L,
#'     eta = 0.05,
#'     max_depth = 4L,
#'     min_child_weight = 1.0,
#'     subsample = 0.55,
#'     colsample_bytree = 0.55)
#' }
#' @return Saves to file and returned in list: VariableImportance.csv, Model, ValidationData.csv, EvaluationMetrics.csv, GridCollect, GridList, and TargetLevels
#' @export
AutoXGBoostMultiClass <- function(data,
                                  TrainOnFull = FALSE,
                                  ValidationData = NULL,
                                  TestData = NULL,
                                  TargetColumnName = NULL,
                                  FeatureColNames = NULL,
                                  IDcols = NULL,
                                  model_path = NULL,
                                  metadata_path = NULL,
                                  ModelID = "FirstModel",
                                  LossFunction = 'multi:softmax',
                                  EncodingMethod = "binary",
                                  ReturnFactorLevels = TRUE,
                                  ReturnModelObjects = TRUE,
                                  SaveModelObjects = FALSE,
                                  Verbose = 0L,
                                  DebugMode = FALSE,
                                  NumOfParDepPlots = 3L,
                                  NThreads = parallel::detectCores(),
                                  eval_metric = "merror",
                                  grid_eval_metric = "accuracy",
                                  TreeMethod = "hist",
                                  GridTune = FALSE,
                                  BaselineComparison = "default",
                                  MaxModelsInGrid = 10L,
                                  MaxRunsWithoutNewWinner = 20L,
                                  MaxRunMinutes = 24L*60L,
                                  PassInGrid = NULL,
                                  Trees = 50L,
                                  eta = NULL,
                                  max_depth = NULL,
                                  min_child_weight = NULL,
                                  subsample = NULL,
                                  colsample_bytree = NULL) {

  # Check args ----
  if(DebugMode) print("Check args ----")
  XGBoostArgsCheck(GridTune.=GridTune, model_path.=model_path, metadata_path.=metadata_path, Trees.=Trees, max_depth.=max_depth, eta.=eta, min_child_weight.=min_child_weight, subsample.=subsample, colsample_bytree.=colsample_bytree)

  # Data prep ----
  if(DebugMode) print("Data prep ----")
  if(EncodingMethod %chin% c("target_encode", "credibility", "m_estimator", "woe")) EncodingMethod <- "poly_encode"
  Output <- XGBoostDataPrep(ModelType="multiclass", data.=data, ValidationData.=ValidationData, TestData.=TestData, TargetColumnName.=TargetColumnName, FeatureColNames.=FeatureColNames, IDcols.=IDcols, TransformNumericColumns.=NULL, Methods.=NULL, ModelID.=ModelID, model_path.=model_path, TrainOnFull.=TrainOnFull, SaveModelObjects.=SaveModelObjects, ReturnFactorLevels.=ReturnFactorLevels, EncodingMethod.=EncodingMethod)
  FactorLevelsList <- Output$FactorLevelsList; Output$FactorLevelsList <- NULL
  FinalTestTarget <- Output$FinalTestTarget; Output$FinalTestTarget <- NULL
  TargetLevels <- Output$TargetLevels; Output$TargetLevels <- NULL
  datavalidate <- Output$datavalidate; Output$datavalidate <- NULL
  TrainTarget <- Output$TrainTarget; Output$TrainTarget <- NULL
  TrainMerge <- Output$TrainMerge; Output$TrainMerge <- NULL
  ValidMerge <- Output$ValidMerge; Output$ValidMerge <- NULL
  TestTarget <- Output$TestTarget; Output$TestTarget <- NULL
  NumLevels <- Output$NumLevels; Output$NumLevels <- NULL
  datatrain <- Output$datatrain; Output$datatrain <- NULL
  dataTrain <- Output$dataTrain; Output$dataTrain <- NULL
  TestMerge <- Output$TestMerge; Output$TestMerge <- NULL
  TestData <- Output$TestData; Output$TestData <- NULL
  datatest <- Output$datatest; Output$datatest <- NULL
  EvalSets <- Output$EvalSets; Output$EvalSets <- NULL
  dataTest <- Output$dataTest; Output$dataTest <- NULL
  IDcols <- Output$IDcols; Output$IDcols <- NULL
  Names <- Output$Names; rm(Output)

  # Bring into existence
  ExperimentalGrid <- NULL; BestGrid <- NULL

  # Grid tuning ----
  if(DebugMode) print("Grid tuning ----")
  if(GridTune) {
    Output <- XGBoostGridTuner(ModelType="multiclass", TrainOnFull.=TrainOnFull, TargetColumnName.=TargetColumnName, DebugMode.=DebugMode, TreeMethod.=TreeMethod, Trees.=Trees, Depth.=max_depth, LearningRate.=eta, min_child_weight.=min_child_weight, subsample.=subsample, colsample_bytree.=colsample_bytree, LossFunction=LossFunction, EvalMetric=eval_metric, grid_eval_metric.=grid_eval_metric, CostMatrixWeights=NULL, datatrain.=datatrain, datavalidate.=datavalidate, datatest.=datatest, EvalSets.=EvalSets, TestTarget.=TestTarget, FinalTestTarget.=FinalTestTarget, TargetLevels.=TargetLevels, MaxRunsWithoutNewWinner=MaxRunsWithoutNewWinner, MaxModelsInGrid=MaxModelsInGrid, MaxRunMinutes=MaxRunMinutes, BaselineComparison.=BaselineComparison, SaveModelObjects=SaveModelObjects, metadata_path=metadata_path, model_path=model_path, ModelID=ModelID, Verbose.=Verbose, NumLevels.=NumLevels)
    ExperimentalGrid <- Output$ExperimentalGrid
    BestGrid <- Output$BestGrid
  }

  # Final Params ----
  if(DebugMode) print("Final Params ----")
  Output <- XGBoostFinalParams(TrainOnFull.=TrainOnFull, PassInGrid.=PassInGrid, BestGrid.=BestGrid, GridTune.=GridTune, LossFunction.=LossFunction, eval_metric.=eval_metric, NThreads.=NThreads, TreeMethod.=TreeMethod, Trees.=Trees)
  base_params <- Output$base_params
  NTrees <- if(length(Output$NTrees) > 1L) max(Output$NTrees) else Output$NTrees; rm(Output)
  base_params[["num_class"]] <- NumLevels

  # Train Final Model ----
  if(DebugMode) print("Train Final Model ----")
  model <- xgboost::xgb.train(params=base_params, data=datatrain, watchlist=EvalSets, nrounds=NTrees)

  # Save Model ----
  if(DebugMode) print("Save Model ----")
  if(SaveModelObjects) {
    if(getwd() == model_path) {
      xgboost::xgb.save(model = model, fname = ModelID)
    } else {
      save(model, file = file.path(model_path, ModelID))
    }
  }

  # Grid Score Model ----
  if(DebugMode) print("Grid Score Model ----")
  predict <- stats::predict(model, if(!is.null(TestData)) datatest else if(!TrainOnFull) datavalidate else datatrain)

  # Convert predict object if softprob ----
  if(DebugMode) print("Convert predict object if softprob ----")
  if(LossFunction == "multi:softprob") {
    for(counter in seq.int(NumLevels)) {
      if(counter == 1L) {
        Final <- data.table::as.data.table(predict[seq_along(predict/NumLevels)])
        data.table::setnames(x = Final, old = "V1", new = as.character(TargetLevels[counter, OriginalLevels]))
      } else {
        temp <- data.table::as.data.table(predict[(1 + (counter-1) * (length(predict)/NumLevels)):(counter * (length(predict)/NumLevels))])
        data.table::setnames(x = temp, old = "V1", new = as.character(TargetLevels[counter,OriginalLevels]))
        Final <- cbind(Final, temp)
      }
    }
  }

  # Validation, Importance, Shap data ----
  Output <- XGBoostValidation(ModelType.="multiclass", TrainOnFull.=TrainOnFull, model.=model, TargetColumnName.=TargetColumnName, SaveModelObjects.=SaveModelObjects, metadata_path.=metadata_path, model_path.=model_path, ModelID.=ModelID, TestData.=TestData, TestTarget.=TestTarget, FinalTestTarget.=FinalTestTarget, TestMerge.=TestMerge, dataTest.=dataTest, TrainTarget.=TrainTarget, dataTrain.=dataTrain, Final.=Final, predict.=predict, TransformNumericColumns.=NULL, TransformationResults.=NULL, GridTune.=NULL, data.=NULL, LossFunction.=LossFunction)
  VariableImportance <- Output$VariableImportance; Output$VariableImportance <- NULL
  ValidationData <- Output$ValidationData; Output$ValidationData <- NULL
  ShapValues <- Output$ShapValues; rm(Output)

  # Evaluation Metrics ----
  if(DebugMode) print("Evaluation Metrics ----")
  if(LossFunction != "multi:softprob") {
    EvaluationMetrics <- data.table::data.table(
      Metric = "Accuracy",
      MetricValue = ValidationData[, mean(data.table::fifelse(p1 == eval(Target), 1, 0), na.rm = TRUE)])
  }

  # Save EvaluationMetrics to File ----
  if(DebugMode) print("Save EvaluationMetrics to File ----")
  if(LossFunction != "multi:softprob") {
    EvaluationMetrics <- EvaluationMetrics[MetricValue != 999999]
    if(SaveModelObjects) {
      if(!is.null(metadata_path)) {
        data.table::fwrite(EvaluationMetrics, file = file.path(metadata_path, paste0(ModelID, "_EvaluationMetrics.csv")))
      } else {
        data.table::fwrite(EvaluationMetrics, file = file.path(model_path, paste0(ModelID, "_EvaluationMetrics.csv")))
      }
    }
  }

  # Save GridCollect and grid_metrics ----
  if(DebugMode) print("Save GridCollect and grid_metrics ----")
  if(SaveModelObjects & GridTune) {
    if(!is.null(metadata_path)) {
      data.table::fwrite(ExperimentalGrid, file = file.path(metadata_path, paste0(ModelID, "ExperimentalGrid.csv")))
    } else {
      data.table::fwrite(ExperimentalGrid, file = file.path(model_path, paste0(ModelID, "ExperimentalGrid.csv")))
    }
  }

  # MultiClass Return Model Objects ----
  if(!exists("FactorLevelsList")) FactorLevelsList <- NULL
  if(!exists("ExperimentalGrid")) ExperimentalGrid <- NULL

  # Return Model Objects ----
  if(DebugMode) print("Return Model Objects ----")
  if(!GridTune) GridMetrics <- NULL
  if(ReturnModelObjects) {
    return(list(
      Model = model,
      ValidationData = if(exists("ValidationData")) ValidationData else NULL,
      ShapValues = if(exists("ShapValues")) ShapValues else NULL,
      EvaluationMetrics = if(exists("EvaluationMetrics")) EvaluationMetrics else NULL,
      VariableImportance = if(exists("VariableImportance")) VariableImportance else NULL,
      VI_Plot = if(exists("VariableImportance") && !is.null(VariableImportance)) tryCatch({if(all(c("plotly","dplyr") %chin% installed.packages())) plotly::ggplotly(VI_Plot(Type = "h2o", VariableImportance)) else VI_Plot(Type = "h2o", VariableImportance)}, error = function(x) NULL) else NULL,
      GridMetrics = if(exists("ExperimentalGrid")) ExperimentalGrid else NULL,
      ColNames = if(exists("Names")) Names else NULL,
      TargetLevels = if(exists("TargetLevels")) TargetLevels else NULL,
      FactorLevels = if(exists("FactorLevelsList")) FactorLevelsList else NULL))
  }
}
