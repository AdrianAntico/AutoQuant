# AutoQuant is a package for quickly creating high quality visualizations under a common and easy api.
# Copyright (C) <year>  <name of author>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

#' @title AutoXGBoostClassifier
#'
#' @description AutoXGBoostClassifier is an automated XGBoost modeling framework with grid-tuning and model evaluation that runs a variety of steps. First, a stratified sampling (by the target variable) is done to create train and validation sets. Then, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation plot, evaluation boxplot, evaluation metrics, variable importance, partial dependence calibration plots, partial dependence calibration box plots, and column names used in model fitting.
#'
#' @author Adrian Antico
#' @family Automated Supervised Learning - Binary Classification
#'
#' @param OutputSelection You can select what type of output you want returned. Choose from c("Importances", "EvalPlots", "EvalMetrics", "Score_TrainData")
#' @param data This is your data set for training and testing your model
#' @param TrainOnFull Set to TRUE to train on full data
#' @param ValidationData This is your holdout data set used in modeling either refine your hyperparameters.
#' @param TestData This is your holdout data set. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TargetColumnName Either supply the target column name OR the column number where the target is located (but not mixed types). Note that the target column needs to be a 0 | 1 numeric variable.
#' @param FeatureColNames Either supply the feature column names OR the column number where the target is located (but not mixed types)
#' @param WeightsColumnName Supply a column name for your weights column. Leave NULL otherwise
#' @param IDcols A vector of column names or column numbers to keep in your data but not include in the modeling.
#' @param CostMatrixWeights A vector with 4 elements c(True Positive Cost, False Negative Cost, False Positive Cost, True Negative Cost). Default c(1,0,0,1),
#' @param LossFunction Select from 'reg:logistic', "binary:logistic"
#' @param eval_metric This is the metric used to identify best grid tuned model. Choose from "logloss","error","aucpr","auc"
#' @param grid_eval_metric Case sensitive. I typically choose 'Utility' or 'MCC'. Choose from 'Utility', 'MCC', 'Acc', 'F1_Score', 'F2_Score', 'F0.5_Score', 'TPR', 'TNR', 'FNR', 'FPR', 'FDR', 'FOR', 'NPV', 'PPV', 'ThreatScore'
#' @param NThreads Set the maximum number of threads you'd like to dedicate to the model run. E.g. 8
#' @param TreeMethod Choose from "hist", "gpu_hist"
#' @param model_path A character string of your path file to where you want your output saved
#' @param SaveInfoToPDF Set to TRUE to save modeling information to PDF. If model_path or metadata_path aren't defined then output will be saved to the working directory
#' @param metadata_path A character string of your path file to where you want your model evaluation output saved. If left NULL, all output will be saved to model_path.
#' @param ModelID A character string to name your model and output
#' @param NumOfParDepPlots Tell the function the number of partial dependence calibration plots you want to create.
#' @param Verbose Set to 0 if you want to suppress model evaluation updates in training
#' @param EncodingMethod Choose from 'binary', 'm_estimator', 'credibility', 'woe', 'target_encoding', 'poly_encode', 'backward_difference', 'helmert'
#' @param ReturnModelObjects Set to TRUE to output all modeling objects (E.g. plots and evaluation metrics)
#' @param ReturnFactorLevels TRUE or FALSE. Set to FALSE to not return factor levels.
#' @param SaveModelObjects Set to TRUE to return all modeling objects to your environment
#' @param GridTune Set to TRUE to run a grid tuning procedure
#' @param early_stopping_rounds = 100L
#' @param Trees Bandit grid partitioned. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the trees numbers you want to test. For running grid tuning, a NULL value supplied will mean these values are tested seq(1000L, 10000L, 1000L)
#' @param num_parallel_tree = 1. If setting greater than 1, set colsample_bytree < 1, subsample < 1 and round = 1
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
#' @param alpha 0. L1 Reg.
#' @param lambda 1. L2 Reg.
#' @examples
#' \dontrun{
#' # Create some dummy correlated data
#' data <- AutoQuant::FakeDataGenerator(
#'   Correlation = 0.85,
#'   N = 1000L,
#'   ID = 2L,
#'   ZIP = 0L,
#'   AddDate = FALSE,
#'   Classification = TRUE,
#'   MultiClass = FALSE)
#'
#' # Run function
#' TestModel <- AutoQuant::AutoXGBoostClassifier(
#'
#'   # GPU or CPU
#'   TreeMethod = "hist",
#'   NThreads = parallel::detectCores(),
#'
#'   # Metadata args
#'   OutputSelection = c('Importances', 'EvalPlots', 'EvalMetrics', 'Score_TrainData'),
#'   model_path = normalizePath("./"),
#'   metadata_path = NULL,
#'   ModelID = "Test_Model_1",
#'   EncodingMethod = "binary",
#'   ReturnFactorLevels = TRUE,
#'   ReturnModelObjects = TRUE,
#'   SaveModelObjects = FALSE,
#'   SaveInfoToPDF = FALSE,
#'
#'   # Data args
#'   data = data,
#'   TrainOnFull = FALSE,
#'   ValidationData = NULL,
#'   TestData = NULL,
#'   TargetColumnName = "Adrian",
#'   FeatureColNames = names(data)[!names(data) %in%
#'     c("IDcol_1", "IDcol_2","Adrian")],
#'   WeightsColumnName = NULL,
#'   IDcols = c("IDcol_1","IDcol_2"),
#'
#'   # Model evaluation
#'   LossFunction = 'reg:logistic',
#'   CostMatrixWeights = c(0,1,1,0),
#'   eval_metric = "auc",
#'   grid_eval_metric = "MCC",
#'   NumOfParDepPlots = 3L,
#'
#'   # Grid tuning args
#'   PassInGrid = NULL,
#'   GridTune = FALSE,
#'   BaselineComparison = "default",
#'   MaxModelsInGrid = 10L,
#'   MaxRunsWithoutNewWinner = 20L,
#'   MaxRunMinutes = 24L*60L,
#'   Verbose = 1L,
#'
#'   # ML args
#'   Trees = 500L,
#'   eta = 0.30,
#'   max_depth = 9L,
#'   min_child_weight = 1.0,
#'   subsample = 1,
#'   colsample_bytree = 1,
#'   DebugMode = FALSE)
#' }
#' @return Saves to file and returned in list: VariableImportance.csv, Model, ValidationData.csv, EvalutionPlot.png, EvaluationMetrics.csv, ParDepPlots.R a named list of features with partial dependence calibration plots, GridCollect, and GridList
#' @export
AutoXGBoostClassifier <- function(OutputSelection = c('Importances','EvalPlots','EvalMetrics','Score_TrainData'),
                                  data = NULL,
                                  TrainOnFull = FALSE,
                                  ValidationData = NULL,
                                  TestData = NULL,
                                  TargetColumnName = NULL,
                                  FeatureColNames = NULL,
                                  WeightsColumnName = NULL,
                                  IDcols = NULL,
                                  model_path = NULL,
                                  metadata_path = NULL,
                                  SaveInfoToPDF = FALSE,
                                  ModelID = "FirstModel",
                                  EncodingMethod = "credibility",
                                  ReturnFactorLevels = TRUE,
                                  ReturnModelObjects = TRUE,
                                  SaveModelObjects = FALSE,
                                  Verbose = 0L,
                                  NumOfParDepPlots = 3L,
                                  NThreads = max(1L, parallel::detectCores()-2L),
                                  LossFunction = 'reg:logistic',
                                  CostMatrixWeights = c(0,1,1,0),
                                  grid_eval_metric = "MCC",
                                  eval_metric = "auc",
                                  TreeMethod = "hist",
                                  GridTune = FALSE,
                                  BaselineComparison = "default",
                                  MaxModelsInGrid = 10L,
                                  MaxRunsWithoutNewWinner = 20L,
                                  MaxRunMinutes = 24L*60L,
                                  PassInGrid = NULL,
                                  early_stopping_rounds = 100L,
                                  Trees = 1000L,
                                  num_parallel_tree = 1,
                                  eta = 0.30,
                                  max_depth = 9,
                                  min_child_weight = 1,
                                  subsample = 1,
                                  colsample_bytree = 1,
                                  DebugMode = FALSE,
                                  alpha = 0,
                                  lambda = 1) {

  # Check args ----
  if(DebugMode) print("Check args ----")
  XGBoostArgsCheck(GridTune.=GridTune, model_path.=model_path, metadata_path.=metadata_path, Trees.=Trees, max_depth.=max_depth, eta.=eta, min_child_weight.=min_child_weight, subsample.=subsample, colsample_bytree.=colsample_bytree)

  # Grab all official parameters and their evaluated arguments
  ArgsList <- c(as.list(environment()))
  ArgsList[['data']] <- NULL
  ArgsList[['ValidationData']] <- NULL
  ArgsList[['TestData']] <- NULL
  if(SaveModelObjects) {
    if(!is.null(metadata_path)) {
      save(ArgsList, file = file.path(metadata_path, paste0(ModelID, "_ArgsList.Rdata")))
    } else if(!is.null(model_path)) {
      save(ArgsList, file = file.path(model_path, paste0(ModelID, "_ArgsList.Rdata")))
    }
  }

  # Data prep ----
  if(DebugMode) print("Data prep ----")
  Output <- XGBoostDataPrep(Algo="xgboost", ModelType="classification", data.=data, ValidationData.=ValidationData, TestData.=TestData, TargetColumnName.=TargetColumnName, FeatureColNames.=FeatureColNames, WeightsColumnName.=WeightsColumnName, IDcols.=IDcols, TransformNumericColumns.=NULL, Methods.=NULL, ModelID.=ModelID, model_path.=model_path, TrainOnFull.=TrainOnFull, SaveModelObjects.=SaveModelObjects, ReturnFactorLevels.=ReturnFactorLevels, EncodingMethod.=EncodingMethod, DebugMode.=DebugMode)
  FactorLevelsList <- Output$FactorLevelsList; Output$FactorLevelsList <- NULL
  FinalTestTarget <- Output$FinalTestTarget; Output$FinalTestTarget <- NULL
  WeightsVector <- Output$WeightsVector; Output$WeightsVector <- NULL
  datavalidate <- Output$datavalidate; Output$datavalidate <- NULL
  TrainTarget <- Output$TrainTarget; Output$TrainTarget <- NULL
  TestTarget <- Output$TestTarget; Output$TestTarget <- NULL
  TrainMerge <- Output$TrainMerge; Output$TrainMerge <- NULL
  datatrain <- Output$datatrain; Output$datatrain <- NULL
  TestMerge <- Output$TestMerge; Output$TestMerge <- NULL
  dataTrain <- Output$dataTrain; Output$dataTrain <- NULL
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
    Output <- XGBoostGridTuner(ModelType="classification", TrainOnFull.=TrainOnFull, TargetColumnName.=TargetColumnName, DebugMode.=DebugMode, TreeMethod.=TreeMethod, Trees.=Trees, Depth.=max_depth, LearningRate.=eta, min_child_weight.=min_child_weight, subsample.=subsample, colsample_bytree.=colsample_bytree, LossFunction=LossFunction, EvalMetric=eval_metric, grid_eval_metric.=grid_eval_metric, CostMatrixWeights=CostMatrixWeights, datatrain.=datatrain, datavalidate.=datavalidate, datatest.=datatest, EvalSets.=EvalSets, TestTarget.=TestTarget, FinalTestTarget.=FinalTestTarget, TargetLevels.=TargetLevels, MaxRunsWithoutNewWinner=MaxRunsWithoutNewWinner, MaxModelsInGrid=MaxModelsInGrid, MaxRunMinutes=MaxRunMinutes, BaselineComparison.=BaselineComparison, SaveModelObjects=SaveModelObjects, metadata_path=metadata_path, model_path=model_path, ModelID=ModelID, Verbose.=Verbose, NumLevels.=NULL)
    ExperimentalGrid <- Output$ExperimentalGrid
    BestGrid <- Output$BestGrid
  }

  # Final Params ----
  if(DebugMode) print("Final Params ----")
  Output <- XGBoostFinalParams(PassInGrid.=PassInGrid, TrainOnFull.=TrainOnFull, BestGrid.=BestGrid, GridTune.=GridTune, LossFunction.=LossFunction, eval_metric.=eval_metric, NThreads.=NThreads, TreeMethod.=TreeMethod, Trees.=Trees, Alpha.=alpha, Lambda.=lambda)
  base_params <- Output$base_params
  NTrees <- if(length(Output$NTrees) > 1L) max(Output$NTrees) else Output$NTrees; rm(Output)
  if(num_parallel_tree > 1) {
    if(colsample_bytree == 1) colsample_bytree <- 0.50
    if(length(subsample) == 0L || subsample == 1) subsample <- 0.70
    base_params$round <- 1
  }

  # Build model ----
  if(DebugMode) print("Build model ----")
  if(!is.null(WeightsVector)) {
    model <- xgboost::xgb.train(params = base_params, data = datatrain, watchlist = EvalSets, nrounds = NTrees, verbose = Verbose, weight = WeightsVector, early_stopping_rounds = early_stopping_rounds)
  } else {
    model <- xgboost::xgb.train(params = base_params, data = datatrain, watchlist = EvalSets, nrounds = NTrees, verbose = Verbose, early_stopping_rounds = early_stopping_rounds)
  }

  # Save Model ----
  if(DebugMode) print("Save Model ----")
  if(SaveModelObjects) save(model, file = file.path(model_path, ModelID))

  # TrainData + ValidationData Scoring + Shap ----
  ShapValues <- list()
  if("score_traindata" %chin% tolower(OutputSelection) && !TrainOnFull) {
    predict <- data.table::as.data.table(stats::predict(model, datatrain))
    if(!is.null(datatest)) {
      predict_validate <- data.table::as.data.table(stats::predict(model, datavalidate))
      predict <- data.table::rbindlist(list(predict, predict_validate))
      data.table::setnames(predict, names(predict), "p1")
      rm(predict_validate)
    }
    Output <- XGBoostValidationData(model.=model, TestData.=NULL, ModelType="classification", TrainOnFull.=TRUE, TestDataCheck=FALSE, FinalTestTarget.=FinalTestTarget, TestTarget.=TestTarget, TrainTarget.=TrainTarget, TrainMerge.=TrainMerge, TestMerge.=TestMerge, dataTest.=dataTest, data.=dataTrain, predict.=predict, TargetColumnName.=TargetColumnName, SaveModelObjects. = SaveModelObjects, metadata_path.=metadata_path, model_path.=model_path, ModelID.=ModelID, LossFunction.=NULL, TransformNumericColumns.=NULL, GridTune.=GridTune, TransformationResults.=NULL, TargetLevels.=NULL)
    TrainData <- Output$ValidationData; rm(Output)
    if(!"p1" %chin% names(TrainData)) data.table::setnames(TrainData, "V1", "p1")
  } else {
    TrainData <- NULL
  }

  # Score Model ----
  if(DebugMode) print("Score Model ----")
  predict <- stats::predict(model, if(!is.null(TestData)) datatest else if(!TrainOnFull) datavalidate else datatrain)

  # Validation Data (generate validation data, back transform, save to file) ----
  if(DebugMode) print("Running ValidationData()")
  Output <- XGBoostValidationData(model.=model, TestData.=TestData, ModelType="classification", TrainOnFull.=TrainOnFull, TestDataCheck=!is.null(TestData), FinalTestTarget.=FinalTestTarget, TestTarget.=TestTarget, TrainTarget.=TrainTarget, TestMerge.=TestMerge, dataTest.=dataTest, data.=dataTrain, predict.=predict, TargetColumnName.=TargetColumnName, SaveModelObjects. = SaveModelObjects, metadata_path.=metadata_path, model_path.=model_path, ModelID.=ModelID, LossFunction.=NULL, TransformNumericColumns.=NULL, GridTune.=GridTune, TransformationResults.=NULL, TargetLevels.=NULL)
  ValidationData <- Output[['ValidationData']]; Output$ValidationData <- NULL
  VariableImportance <- Output[['VariableImportance']]; rm(Output)

  # Generate EvaluationMetrics ----
  if(DebugMode) print("Running BinaryMetrics()")
  EvalMetricsList <- list()
  EvalMetrics2List <- list()
  if("evalmetrics" %chin% tolower(OutputSelection)) {
    if("score_traindata" %chin% tolower(OutputSelection) && !TrainOnFull) {
      EvalMetricsList[["TrainData"]] <- BinaryMetrics(ClassWeights.=NULL, CostMatrixWeights.=CostMatrixWeights, SaveModelObjects.=FALSE, ValidationData.=TrainData, TrainOnFull.=TrainOnFull, TargetColumnName.=TargetColumnName, ModelID.=ModelID, model_path.=model_path, metadata_path.=metadata_path, Method = "threshold")
      EvalMetrics2List[["TrainData"]] <- BinaryMetrics(ClassWeights.=NULL, CostMatrixWeights.=CostMatrixWeights, SaveModelObjects.=FALSE, ValidationData.=TrainData, TrainOnFull.=TrainOnFull, TargetColumnName.=TargetColumnName, ModelID.=ModelID, model_path.=model_path, metadata_path.=metadata_path, Method = "bins")
      if(SaveModelObjects) {
        if(!is.null(metadata_path)) {
          data.table::fwrite(EvalMetricsList[['TrainData']], file = file.path(metadata_path, paste0(ModelID, "_Train_EvaluationMetrics.csv")))
        } else if(!is.null(model_path)) {
          data.table::fwrite(EvalMetricsList[['TrainData']], file = file.path(model_path, paste0(ModelID, "_Train_EvaluationMetrics.csv")))
        }
      }
    }
    EvalMetricsList[["TestData"]] <- BinaryMetrics(
      ClassWeights.=NULL,
      CostMatrixWeights.=CostMatrixWeights,
      SaveModelObjects.=FALSE,
      ValidationData.=ValidationData,
      TrainOnFull.=TrainOnFull,
      TargetColumnName.=TargetColumnName,
      ModelID.=ModelID,
      model_path.=model_path,
      metadata_path.=metadata_path,
      Method = "threshold")
    EvalMetrics2List[["TestData"]] <- BinaryMetrics(ClassWeights.=NULL, CostMatrixWeights.=CostMatrixWeights, SaveModelObjects.=FALSE, ValidationData.=ValidationData, TrainOnFull.=TrainOnFull, TargetColumnName.=TargetColumnName, ModelID.=ModelID, model_path.=model_path, metadata_path.=metadata_path, Method = "bins")
    if(SaveModelObjects) {
      if(!is.null(metadata_path)) {
        data.table::fwrite(EvalMetricsList[['TestData']], file = file.path(metadata_path, paste0(ModelID, "_Test_EvaluationMetrics.csv")))
      } else if(!is.null(model_path)) {
        data.table::fwrite(EvalMetricsList[['TestData']], file = file.path(model_path, paste0(ModelID, "_Test_EvaluationMetrics.csv")))
      }
    }
  }

  # Classification evaluation plots ----
  if(DebugMode) print("Running ML_EvalPlots()")
  PlotList <- list()
  if("evalplots" %chin% tolower(OutputSelection)) {
    if("score_traindata" %chin% tolower(OutputSelection) && !TrainOnFull) {
      Output <- ML_EvalPlots(ModelType="classification", TrainOnFull.=TrainOnFull, DataType = 'Train', ValidationData.=TrainData, NumOfParDepPlots.=NumOfParDepPlots, VariableImportance.=VariableImportance, TargetColumnName.=TargetColumnName, FeatureColNames.=FeatureColNames, SaveModelObjects.=SaveModelObjects, ModelID.=ModelID, metadata_path.=metadata_path, model_path.=model_path, LossFunction.=NULL, EvalMetric.=NULL, EvaluationMetrics.=NULL, predict.=NULL)
      PlotList[["Train_EvaluationPlot"]] <- Output$EvaluationPlot; Output$EvaluationPlot <- NULL
      PlotList[["Train_ParDepPlots"]] <- Output$ParDepPlots; Output$ParDepPlots <- NULL
      PlotList[["Train_GainsPlot"]] <- Output$GainsPlot; Output$GainsPlot <- NULL
      PlotList[["Train_LiftPlot"]] <- Output$LiftPlot; Output$LiftPlot <- NULL
      PlotList[["Train_ROC_Plot"]] <- Output$ROC_Plot; rm(Output)
    }
    Output <- ML_EvalPlots(ModelType="classification", TrainOnFull.=TrainOnFull, DataType = 'Test', ValidationData.=ValidationData, NumOfParDepPlots.=NumOfParDepPlots, VariableImportance.=VariableImportance, TargetColumnName.=TargetColumnName, FeatureColNames.=FeatureColNames, SaveModelObjects.=SaveModelObjects, ModelID.=ModelID, metadata_path.=metadata_path, model_path.=model_path, LossFunction.=NULL, EvalMetric.=NULL, EvaluationMetrics.=NULL, predict.=NULL)
    PlotList[["Test_EvaluationPlot"]] <- Output$EvaluationPlot; Output$EvaluationPlot <- NULL
    PlotList[["Test_ParDepPlots"]] <- Output$ParDepPlots; Output$ParDepPlots <- NULL
    PlotList[["Test_GainsPlot"]] <- Output$GainsPlot; Output$GainsPlot <- NULL
    PlotList[["Test_LiftPlot"]] <- Output$LiftPlot; Output$LiftPlot <- NULL
    PlotList[["Test_ROC_Plot"]] <- Output$ROC_Plot; rm(Output)
    if(!is.null(VariableImportance) && "plotly" %chin% installed.packages()) PlotList[['Train_VariableImportance']] <- plotly::ggplotly(VI_Plot(Type = "xgboost", VariableImportance)) else if(!is.null(VariableImportance)) PlotList[['Train_VariableImportance']] <- VI_Plot(Type = "xgboost", VariableImportance)
  }

  # Binary Return Model Objects ----
  if(!exists("FactorLevelsList")) FactorLevelsList <- NULL

  # Return objects ----
  if(DebugMode) print("Return objects ----")
  if(ReturnModelObjects) {
    outputList <- list()
    outputList[["Model"]] <- model
    outputList[["TrainData"]] <- if(exists('ShapValues') && !is.null(ShapValues[['Train_Shap']])) ShapValues[['Train_Shap']] else if(exists('TrainData')) TrainData else NULL
    outputList[["TestData"]] <- if(exists('ShapValues') && !is.null(ShapValues[['Test_Shap']])) ShapValues[['Test_Shap']] else if(exists('ValidationData')) ValidationData else NULL
    outputList[["PlotList"]] <- if(exists('PlotList')) PlotList else NULL
    outputList[["EvaluationMetrics"]] <- if(exists('EvalMetricsList')) EvalMetricsList else NULL
    outputList[["EvaluationMetrics2"]] <- if(exists('EvalMetrics2List')) EvalMetrics2List else NULL
    outputList[["VariableImportance"]] <- if(exists('VariableImportance')) VariableImportance else NULL
    outputList[["GridMetrics"]] <- if(exists('ExperimentalGrid') && !is.null(ExperimentalGrid)) ExperimentalGrid else NULL
    outputList[["ColNames"]] <- if(exists('Names')) Names else NULL
    outputList[["FactorLevelsList"]] <- if(exists('FactorLevelsList')) FactorLevelsList else NULL
    outputList[["ArgsList"]] <- ArgsList
    return(outputList)
  }
}
