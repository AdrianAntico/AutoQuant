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

#' @title AutoCatBoostClassifier
#'
#' @description AutoCatBoostClassifier is an automated modeling function that runs a variety of steps. First, a stratified sampling (by the target variable) is done to create train, validation, and test sets (if not supplied). Then, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions (on test data), an ROC plot, evaluation plot, evaluation metrics, variable importance, partial dependence calibration plots, partial dependence calibration box plots, and column names used in model fitting. You can download the catboost package using devtools, via: devtools::install_github('catboost/catboost', subdir = 'catboost/R-package')
#'
#' @author Adrian Antico
#' @family Automated Supervised Learning - Binary Classification
#'
#' @param OutputSelection You can select what type of output you want returned. Choose from c('Importances', 'EvalPlots', 'EvalMetrics', 'Score_TrainData')
#' @param data This is your data set for training and testing your model
#' @param ValidationData This is your holdout data set used in modeling either refine your hyperparameters. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TestData This is your holdout data set. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TargetColumnName Either supply the target column name OR the column number where the target is located, but not mixed types. Note that the target column needs to be a 0 | 1 numeric variable.
#' @param FeatureColNames Either supply the feature column names OR the column number where the target is located, but not mixed types. Also, not zero-indexed.
#' @param PrimaryDateColumn Supply a date or datetime column for catboost to utilize time as its basis for handling categorical features, instead of random shuffling
#' @param WeightsColumnName Supply a column name for your weights column. Leave NULL otherwise
#' @param IDcols A vector of column names or column numbers to keep in your data but not include in the modeling.
#' @param EncodeMethod 'credibility', 'binary', 'm_estimator', 'woe', 'target_encoding', 'poly_encode', 'backward_difference', 'helmert'
#' @param TrainOnFull Set to TRUE to train on full data and skip over evaluation steps
#' @param task_type Set to 'GPU' to utilize your GPU for training. Default is 'CPU'.
#' @param NumGPUs Numeric. If you have 4 GPUs supply 4 as a value.
#' @param DebugMode Set to TRUE to get a printout of which step the function is on. FALSE, otherwise
#' @param model_path A character string of your path file to where you want your output saved
#' @param metadata_path A character string of your path file to where you want your model evaluation output saved. If left NULL, all output will be saved to model_path.
#' @param ModelID A character string to name your model and output
#' @param SaveInfoToPDF Set to TRUE to save modeling information to PDF. If model_path or metadata_path aren't defined then output will be saved to the working directory
#' @param ReturnModelObjects Set to TRUE to output all modeling objects. E.g. plots and evaluation metrics
#' @param SaveModelObjects Set to TRUE to return all modeling objects to your environment
#' @param NumOfParDepPlots Tell the function the number of partial dependence calibration plots you want to create. Calibration boxplots will only be created for numerical features (not dummy variables)
#' @param ClassWeights Supply a vector of weights for your target classes. E.g. c(0.25, 1) to weight your 0 class by 0.25 and your 1 class by 1.
#' @param CostMatrixWeights A vector with 4 elements c(True Positive Cost, False Negative Cost, False Positive Cost, True Negative Cost). Default c(1,0,0,1)
#' @param EvalMetric This is the metric used inside catboost to measure performance on validation data during a grid-tune. 'AUC' is the default. 'Logloss', 'CrossEntropy', 'Precision', 'Recall', 'F1', 'BalancedAccuracy', 'BalancedErrorRate', 'MCC', 'Accuracy', 'CtrFactor', 'AUC', 'BrierScore', 'HingeLoss', 'HammingLoss', 'ZeroOneLoss', 'Kappa', 'WKappa', 'LogLikelihoodOfPrediction', 'TotalF1', 'PairLogit', 'PairLogitPairwise', 'PairAccuracy', 'QueryCrossEntropy', 'QuerySoftMax', 'PFound', 'NDCG', 'AverageGain', 'PrecisionAt', 'RecallAt', 'MAP'
#' @param grid_eval_metric Case sensitive. I typically choose 'Utility' or 'MCC'. Choose from 'Utility', 'MCC', 'Acc', 'F1_Score', 'F2_Score', 'F0.5_Score', 'TPR', 'TNR', 'FNR', 'FPR', 'FDR', 'FOR', 'NPV', 'PPV', 'ThreatScore'
#' @param LossFunction Default is NULL. Select the loss function of choice. c('Logloss','CrossEntropy','Lq','PairLogit','PairLogitPairwise','YetiRank','YetiRankPairwise','QueryCrossEntropy','QuerySoftMax')
#' @param PassInGrid Defaults to NULL. Pass in a single row of grid from a previous output as a data.table (they are collected as data.tables)
#' @param GridTune Set to TRUE to run a grid tuning procedure. Set a number in MaxModelsInGrid to tell the procedure how many models you want to test.
#' @param MaxModelsInGrid Number of models to test from grid options.
#' @param MaxRunsWithoutNewWinner A number
#' @param MaxRunMinutes In minutes
#' @param BaselineComparison Set to either 'default' or 'best'. Default is to compare each successive model build to the baseline model using max trees (from function args). Best makes the comparison to the current best model.
#' @param MetricPeriods Number of trees to build before evaluating intermediate metrics. Default is 10L
#' @param langevin TRUE or FALSE. TRUE enables
#' @param diffusion_temperature Default value is 10000
#' @param Trees Bandit grid partitioned. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the trees numbers you want to test. For running grid tuning, a NULL value supplied will mean these values are tested seq(1000L, 10000L, 1000L)
#' @param Depth Bandit grid partitioned Number, or vector for depth to test.  For running grid tuning, a NULL value supplied will mean these values are tested seq(4L, 16L, 2L)
#' @param LearningRate Bandit grid partitioned. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the LearningRate values to test. For running grid tuning, a NULL value supplied will mean these values are tested c(0.01,0.02,0.03,0.04)
#' @param L2_Leaf_Reg Random testing. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the L2_Leaf_Reg values to test. For running grid tuning, a NULL value supplied will mean these values are tested seq(1.0, 10.0, 1.0)
#' @param RandomStrength A multiplier of randomness added to split evaluations. Default value is 1 which adds no randomness.
#' @param BorderCount Number of splits for numerical features. Catboost defaults to 254 for CPU and 128 for GPU
#' @param RSM CPU only. Random testing. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the RSM values to test. For running grid tuning, a NULL value supplied will mean these values are tested c(0.80, 0.85, 0.90, 0.95, 1.0)
#' @param BootStrapType Random testing. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the BootStrapType values to test. For running grid tuning, a NULL value supplied will mean these values are tested c('Bayesian', 'Bernoulli', 'Poisson', 'MVS', 'No')
#' @param GrowPolicy Random testing. NULL, character, or vector for GrowPolicy to test. For grid tuning, supply a vector of values. For running grid tuning, a NULL value supplied will mean these values are tested c('SymmetricTree', 'Depthwise', 'Lossguide')
#' @param model_size_reg Defaults to 0.5. Set to 0 to allow for bigger models. This is for models with high cardinality categorical features. Valuues greater than 0 will shrink the model and quality will decline but models won't be huge.
#' @param feature_border_type Defaults to 'GreedyLogSum'. Other options include: Median, Uniform, UniformAndQuantiles, MaxLogSum, MinEntropy
#' @param sampling_unit Default is Group. Other option is Object. if GPU is selected, this will be turned off unless the LossFunction is YetiRankPairWise
#' @param subsample Default is NULL. Catboost will turn this into 0.66 for BootStrapTypes Poisson and Bernoulli. 0.80 for MVS. Doesn't apply to others.
#' @param score_function Default is Cosine. CPU options are Cosine and L2. GPU options are Cosine, L2, NewtonL2, and NewtomCosine (not available for Lossguide)
#' @param min_data_in_leaf Default is 1. Cannot be used with SymmetricTree is GrowPolicy
#' @examples
#' \dontrun{
#' # Create some dummy correlated data
#' data <- AutoQuant::FakeDataGenerator(
#'   Correlation = 0.85,
#'   N = 10000,
#'   ID = 2,
#'   ZIP = 0,
#'   AddDate = FALSE,
#'   Classification = TRUE,
#'   MultiClass = FALSE)
#'
#' # Run function
#' TestModel <- AutoQuant::AutoCatBoostClassifier(
#'
#'   # GPU or CPU and the number of available GPUs
#'   task_type = 'GPU',
#'   NumGPUs = 1,
#'   TrainOnFull = FALSE,
#'   DebugMode = FALSE,
#'
#'   # Metadata args
#'   OutputSelection = c('Score_TrainData', 'Importances', 'EvalPlots', 'EvalMetrics'),
#'   ModelID = 'Test_Model_1',
#'   model_path = normalizePath('./'),
#'   metadata_path = normalizePath('./'),
#'   SaveModelObjects = FALSE,
#'   ReturnModelObjects = TRUE,
#'   SaveInfoToPDF = FALSE,
#'
#'   # Data args
#'   data = data,
#'   ValidationData = NULL,
#'   TestData = NULL,
#'   TargetColumnName = 'Adrian',
#'   FeatureColNames = names(data)[!names(data) %in%
#'     c('IDcol_1','IDcol_2','Adrian')],
#'   PrimaryDateColumn = NULL,
#'   WeightsColumnName = NULL,
#'   IDcols = c('IDcol_1','IDcol_2'),
#'   EncodeMethod = 'credibility',
#'
#'   # Evaluation args
#'   ClassWeights = c(1L,1L),
#'   CostMatrixWeights = c(0,1,1,0),
#'   EvalMetric = 'AUC',
#'   grid_eval_metric = 'MCC',
#'   LossFunction = 'Logloss',
#'   MetricPeriods = 10L,
#'   NumOfParDepPlots = ncol(data)-1L-2L,
#'
#'   # Grid tuning args
#'   PassInGrid = NULL,
#'   GridTune = FALSE,
#'   MaxModelsInGrid = 30L,
#'   MaxRunsWithoutNewWinner = 20L,
#'   MaxRunMinutes = 24L*60L,
#'   BaselineComparison = 'default',
#'
#'   # ML args
#'   Trees = 1000,
#'   Depth = 9,
#'   LearningRate = NULL,
#'   L2_Leaf_Reg = NULL,
#'   model_size_reg = 0.5,
#'   langevin = FALSE,
#'   diffusion_temperature = 10000,
#'   RandomStrength = 1,
#'   BorderCount = 128,
#'   RSM = 1,
#'   BootStrapType = 'Bayesian',
#'   GrowPolicy = 'SymmetricTree',
#'   feature_border_type = 'GreedyLogSum',
#'   sampling_unit = 'Object',
#'   subsample = NULL,
#'   score_function = 'Cosine',
#'   min_data_in_leaf = 1)
#' }
#' @return Saves to file and returned in list: VariableImportance.csv, Model (the model), ValidationData.csv, ROC_Plot.png, EvalutionPlot.png, EvaluationMetrics.csv, ParDepPlots.R a named list of features with partial dependence calibration plots, GridCollect, and GridList
#' @export
AutoCatBoostClassifier <- function(OutputSelection = c('Importances','EvalPlots','EvalMetrics','Score_TrainData'),
                                   data = NULL,
                                   ValidationData = NULL,
                                   TestData = NULL,
                                   TargetColumnName = NULL,
                                   FeatureColNames = NULL,
                                   PrimaryDateColumn = NULL,
                                   WeightsColumnName = NULL,
                                   IDcols = NULL,
                                   EncodeMethod = 'credibility',
                                   TrainOnFull = FALSE,
                                   task_type = 'GPU',
                                   NumGPUs = 1,
                                   ReturnModelObjects = TRUE,
                                   SaveModelObjects = FALSE,
                                   SaveInfoToPDF = FALSE,
                                   ModelID = 'FirstModel',
                                   model_path = NULL,
                                   metadata_path = NULL,
                                   EvalMetric = 'MCC',
                                   LossFunction = 'Logloss',
                                   grid_eval_metric = 'MCC',
                                   ClassWeights = c(1,1),
                                   CostMatrixWeights = c(0,1,1,0),
                                   NumOfParDepPlots = 0L,
                                   PassInGrid = NULL,
                                   GridTune = FALSE,
                                   MaxModelsInGrid = 30L,
                                   MaxRunsWithoutNewWinner = 20L,
                                   MaxRunMinutes = 24L*60L,
                                   BaselineComparison = 'default',
                                   MetricPeriods = 10L,
                                   Trees = 50L,
                                   Depth = 6,
                                   LearningRate = NULL,
                                   L2_Leaf_Reg = 3,
                                   RandomStrength = 1,
                                   BorderCount = 128,
                                   RSM = NULL,
                                   BootStrapType = NULL,
                                   GrowPolicy = 'SymmetricTree',
                                   langevin = FALSE,
                                   diffusion_temperature = 10000,
                                   model_size_reg = 0.5,
                                   feature_border_type = 'GreedyLogSum',
                                   sampling_unit = 'Object',
                                   subsample = NULL,
                                   score_function = 'Cosine',
                                   min_data_in_leaf = 1,
                                   DebugMode = FALSE) {

  # Load catboost ----
  loadNamespace(package = 'catboost')

  # Args Checking (ensure args are set consistently) ----
  if(DebugMode) print('Running CatBoostArgsCheck()')
  Output <- CatBoostArgsCheck(ModelType='classifier', PrimaryDateColumn.=PrimaryDateColumn, GridTune.=GridTune, model_path.=model_path, metadata_path.=metadata_path, ClassWeights.=ClassWeights, LossFunction.=LossFunction, loss_function.=NULL, loss_function_value.=NULL, eval_metric.=NULL, eval_metric_value.=NULL, task_type.=task_type, NumGPUs.=NumGPUs, MaxModelsInGrid.=MaxModelsInGrid, NumOfParDepPlots.=NumOfParDepPlots,ReturnModelObjects.=ReturnModelObjects, SaveModelObjects.=SaveModelObjects, PassInGrid.=PassInGrid, MetricPeriods.=MetricPeriods, langevin.=langevin, diffusion_temperature.=diffusion_temperature, Trees.=Trees, Depth.=Depth, LearningRate.=LearningRate, L2_Leaf_Reg.=L2_Leaf_Reg,RandomStrength.=RandomStrength, BorderCount.=BorderCount, RSM.=RSM, BootStrapType.=BootStrapType, GrowPolicy.=GrowPolicy, model_size_reg.=model_size_reg, feature_border_type.=feature_border_type, sampling_unit.=sampling_unit, subsample.=subsample, score_function.=score_function, min_data_in_leaf.=min_data_in_leaf)
  score_function <- Output$score_function
  BootStrapType <- Output$BootStrapType
  sampling_unit <- Output$sampling_unit
  LossFunction <- Output$LossFunction
  GrowPolicy <- Output$GrowPolicy
  task_type <- Output$task_type
  GridTune <- Output$GridTune
  HasTime <- Output$HasTime
  NumGPUs <- Output$NumGPUs
  Depth <- Output$Depth
  RSM <- Output$RSM; rm(Output)

  # Grab all official parameters and their evaluated arguments
  ArgsList <- c(as.list(environment()))
  ArgsList[['data']] <- NULL
  ArgsList[['ValidationData']] <- NULL
  ArgsList[['TestData']] <- NULL
  ArgsList[['Algo']] <- "CatBoost"
  ArgsList[['TargetType']] <- "Binary Classification"
  ArgsList[['PredictionColumnName']] <- "p1"
  if(SaveModelObjects) {
    if(!is.null(metadata_path)) {
      save(ArgsList, file = file.path(metadata_path, paste0(ModelID, "_ArgsList.Rdata")))
    } else if(!is.null(model_path)) {
      save(ArgsList, file = file.path(model_path, paste0(ModelID, "_ArgsList.Rdata")))
    }
  }

  # Data Prep (model data prep, dummify, create sets) ----
  if(DebugMode) print('Running CatBoostDataPrep()')
  Output <- CatBoostDataPrep(OutputSelection.=OutputSelection, EncodeMethod. = EncodeMethod, ModelType='classification', data.=data, ValidationData.=ValidationData, TestData.=TestData, TargetColumnName.=TargetColumnName, FeatureColNames.=FeatureColNames, PrimaryDateColumn.=PrimaryDateColumn, WeightsColumnName.=WeightsColumnName, IDcols.=IDcols,TrainOnFull.=TrainOnFull, SaveModelObjects.=SaveModelObjects, TransformNumericColumns.=NULL, Methods.=NULL, model_path.=metadata_path, ModelID.=ModelID, LossFunction.=NULL, EvalMetric.=NULL)
  FactorLevelsList <- Output$FactorLevelsList; Output$FactorLevelsList <- NULL
  FinalTestTarget <- Output$FinalTestTarget; Output$FinalTestTarget <- NULL
  FeatureColNames <- Output$FeatureColNames; Output$FeatureColNames <- NULL
  UseBestModel <- Output$UseBestModel; Output$UseBestModel <- NULL
  TrainTarget <- Output$TrainTarget; Output$TrainTarget <- NULL
  CatFeatures <- Output$CatFeatures; Output$CatFeatures <- NULL
  if(length(CatFeatures) == 0) CatFeatures <- NULL
  TestTarget <- Output$TestTarget; Output$TestTarget <- NULL
  TrainMerge <- Output$TrainMerge; Output$TrainMerge <- NULL
  dataTrain <- Output$dataTrain; Output$dataTrain <- NULL
  TestMerge <- Output$TestMerge; Output$TestMerge <- NULL
  dataTest <- Output$dataTest; Output$dataTest <- NULL
  TestData <- Output$TestData; Output$TestData <- NULL
  Names <- Output$Names; rm(Output)

  # Create catboost data objects ----
  if(DebugMode) print('Running CatBoostDataConversion()')
  Output <- CatBoostDataConversion(CatFeatures.=CatFeatures, dataTrain.=dataTrain, dataTest.=dataTest, TestData.=TestData, TrainTarget.=TrainTarget, TestTarget.=TestTarget, FinalTestTarget.=FinalTestTarget, TrainOnFull.=TrainOnFull, Weights.=WeightsColumnName)
  TrainPool <- Output$TrainPool; Output$TrainPool <- NULL
  TestPool <- Output$TestPool; Output$TestPool <- NULL
  FinalTestPool <- Output$FinalTestPool; rm(Output)

  # Bring into existence ----
  ExperimentalGrid <- NULL; BestGrid <- NULL

  # Grid tuning ----
  if(GridTune) {
    Output <- CatBoostGridTuner(ModelType='classification', TrainOnFull.=TrainOnFull, BaselineComparison.=BaselineComparison, HasTime=HasTime, TargetColumnName.=TargetColumnName, DebugMode.=DebugMode, task_type.=task_type, Trees.=Trees, Depth.=Depth, LearningRate.=LearningRate, L2_Leaf_Reg.=L2_Leaf_Reg, BorderCount.=BorderCount, RandomStrength.=RandomStrength, RSM.=RSM, BootStrapType.=BootStrapType, GrowPolicy.=GrowPolicy, NumGPUs=NumGPUs, LossFunction=LossFunction, EvalMetric=EvalMetric, MetricPeriods=MetricPeriods, ClassWeights=ClassWeights, CostMatrixWeights=CostMatrixWeights, data=data, TrainPool.=TrainPool, TestPool.=TestPool, FinalTestTarget.=FinalTestTarget, TestTarget.=TestTarget, FinalTestPool.=FinalTestPool, TestData.=TestData, TestMerge.=TestMerge, TargetLevels.=NULL, MaxRunsWithoutNewWinner=MaxRunsWithoutNewWinner, MaxModelsInGrid=MaxModelsInGrid, MaxRunMinutes=MaxRunMinutes, SaveModelObjects=SaveModelObjects, metadata_path=metadata_path, model_path=model_path, ModelID=ModelID, grid_eval_metric.=grid_eval_metric)
    ExperimentalGrid <- Output$ExperimentalGrid
    BestGrid <- Output$BestGrid
  }

  # Final Parameters ----
  if(DebugMode) print('Running CatBoostFinalParams()')
  base_params <- CatBoostFinalParams(ModelType='classification', UseBestModel.=UseBestModel, ClassWeights.=ClassWeights, PassInGrid.=PassInGrid, BestGrid.=BestGrid, ExperimentalGrid.=ExperimentalGrid, GridTune.=GridTune, TrainOnFull.=TrainOnFull, MetricPeriods.=MetricPeriods, LossFunction.=LossFunction, EvalMetric.=EvalMetric, score_function.=score_function, HasTime.=HasTime, task_type.=task_type, NumGPUs.=NumGPUs, NTrees.=Trees, Depth.=Depth, LearningRate.=LearningRate, L2_Leaf_Reg.=L2_Leaf_Reg, langevin.=langevin, diffusion_temperature.=diffusion_temperature, sampling_unit.=sampling_unit, RandomStrength.=RandomStrength, BorderCount.=BorderCount, RSM.=RSM, GrowPolicy.=GrowPolicy, BootStrapType.=BootStrapType, model_size_reg.=model_size_reg, feature_border_type.=feature_border_type, subsample.=subsample, min_data_in_leaf.=min_data_in_leaf)

  # Train Final Model ----
  if(DebugMode) print('Running catboost.train')
  if(!TrainOnFull) {
    model <- catboost::catboost.train(learn_pool = TrainPool, test_pool = TestPool, params = base_params)
  } else {
    model <- catboost::catboost.train(learn_pool = TrainPool, params = base_params)
  }

  # Save Model ----
  if(DebugMode) print('Running catboost.save_model')
  if(SaveModelObjects) catboost::catboost.save_model(model = model, model_path = file.path(model_path, ModelID))

  # TrainData + ValidationData Scoring + Shap
  if('score_traindata' %chin% tolower(OutputSelection) && !TrainOnFull) {
    predict <- data.table::as.data.table(catboost::catboost.predict(model = model, pool = TrainPool, prediction_type = 'Probability', thread_count = parallel::detectCores()))
    if(!is.null(TestPool)) {
      predict_validate <- data.table::as.data.table(catboost::catboost.predict(model = model, pool = TestPool, prediction_type = 'Probability', thread_count = parallel::detectCores()))
      predict <- data.table::rbindlist(list(predict, predict_validate))
      data.table::setnames(predict, names(predict), 'p1')
      rm(predict_validate)
    }
    TrainData <- CatBoostValidationData(ModelType='classification', TrainOnFull.=TRUE, TestDataCheck=FALSE, FinalTestTarget.=FinalTestTarget, TestTarget.=TestTarget, TrainTarget.=TrainTarget, TrainMerge.=TrainMerge, TestMerge.=TestMerge, dataTest.=dataTest, data.=dataTrain, predict.=predict, TargetColumnName.=TargetColumnName, SaveModelObjects. = SaveModelObjects, metadata_path.=metadata_path, model_path.=metadata_path, ModelID.=ModelID, LossFunction.=NULL, TransformNumericColumns.=NULL, GridTune.=GridTune, TransformationResults.=NULL, TargetLevels.=NULL)
    if(!'p1' %chin% names(TrainData)) data.table::setnames(TrainData, 'V1', 'p1')
  } else {
    TrainData <- NULL
  }

  # Score Final Test Data ----
  if(DebugMode) print('Running catboost.predict')
  predict <- catboost::catboost.predict(model = model, pool = if(!is.null(TestData)) FinalTestPool else if(TrainOnFull) TrainPool else TestPool, prediction_type = 'Probability', thread_count = parallel::detectCores())

  # Validation Data (generate validation data, back transform, save to file) ----
  if(DebugMode) print('Running CatBoostValidationData()')
  ValidationData <- CatBoostValidationData(ModelType='classification', TrainOnFull.=TrainOnFull, TestDataCheck=!is.null(TestData), FinalTestTarget.=FinalTestTarget, TestTarget.=TestTarget, TrainTarget.=TrainTarget, TestMerge.=TestMerge, dataTest.=dataTest, data.=data, predict.=predict, TargetColumnName.=TargetColumnName, SaveModelObjects. = SaveModelObjects, metadata_path.=metadata_path, model_path.=metadata_path, ModelID.=ModelID, LossFunction.=NULL, TransformNumericColumns.=NULL, GridTune.=GridTune, TransformationResults.=NULL, TargetLevels.=NULL)

  # Gather importance and shap values ----
  if(DebugMode) print('Running CatBoostImportances()')
  if(any(c('importances','importance') %chin% tolower(OutputSelection))) {
    Output <- tryCatch({CatBoostImportances(ModelType='classification', TargetColumnName.=TargetColumnName, TrainPool.=TrainPool, TestPool.=TestPool, FinalTestPool.=FinalTestPool, TrainData.=TrainData, ValidationData.=ValidationData, SaveModelObjects.=SaveModelObjects, model.=model, ModelID.=ModelID, model_path.=metadata_path, metadata_path.=metadata_path, GrowPolicy.=GrowPolicy)}, error = function(x) list(Interaction = NULL, VariableImportance = NULL, ShapValues = NULL))
    VariableImportance <- Output$VariableImportance; Output$VariableImportance <- NULL
    Interaction <- Output$Interaction; Output$Interaction <- NULL
    ShapValues <- Output$ShapValues; rm(Output)
  }

  # Generate EvaluationMetrics ----
  if(DebugMode) print('Running BinaryMetrics()')
  EvalMetricsList <- list()
  EvalMetrics2List <- list()
  if('evalmetrics' %chin% tolower(OutputSelection)) {
    if('score_traindata' %chin% tolower(OutputSelection) && !TrainOnFull) {
      EvalMetricsList[['TrainData']] <- BinaryMetrics(ClassWeights.=ClassWeights, CostMatrixWeights.=CostMatrixWeights, SaveModelObjects.=SaveModelObjects, ValidationData.=TrainData, TrainOnFull.=TrainOnFull, TargetColumnName.=TargetColumnName, ModelID.=ModelID, model_path.=metadata_path, metadata_path.=metadata_path, Method = 'threshold')
      EvalMetrics2List[['TrainData']] <- BinaryMetrics(ClassWeights.=ClassWeights, CostMatrixWeights.=CostMatrixWeights, SaveModelObjects.=SaveModelObjects, ValidationData.=TrainData, TrainOnFull.=TrainOnFull, TargetColumnName.=TargetColumnName, ModelID.=ModelID, model_path.=metadata_path, metadata_path.=metadata_path, Method = 'bins')
      if(SaveModelObjects) {
        if(!is.null(metadata_path)) {
          data.table::fwrite(EvalMetricsList[['TrainData']], file = file.path(metadata_path, paste0(ModelID, "_Train_EvaluationMetrics.csv")))
        } else if(!is.null(model_path)) {
          data.table::fwrite(EvalMetricsList[['TrainData']], file = file.path(model_path, paste0(ModelID, "_Train_EvaluationMetrics.csv")))
        }
      }
    }
    EvalMetricsList[['TestData']] <- BinaryMetrics(ClassWeights.=ClassWeights, CostMatrixWeights.=CostMatrixWeights, SaveModelObjects.=SaveModelObjects, ValidationData.=ValidationData, TrainOnFull.=TrainOnFull, TargetColumnName.=TargetColumnName, ModelID.=ModelID, model_path.=metadata_path, metadata_path.=metadata_path, Method = 'threshold')
    EvalMetrics2List[['TestData']] <- BinaryMetrics(ClassWeights.=ClassWeights, CostMatrixWeights.=CostMatrixWeights, SaveModelObjects.=SaveModelObjects, ValidationData.=ValidationData, TrainOnFull.=TrainOnFull, TargetColumnName.=TargetColumnName, ModelID.=ModelID, model_path.=metadata_path, metadata_path.=metadata_path, Method = 'bins')
    if(SaveModelObjects) {
      if(!is.null(metadata_path)) {
        data.table::fwrite(EvalMetricsList[['TestData']], file = file.path(metadata_path, paste0(ModelID, "_Test_EvaluationMetrics.csv")))
      } else if(!is.null(model_path)) {
        data.table::fwrite(EvalMetricsList[['TestData']], file = file.path(model_path, paste0(ModelID, "_Test_EvaluationMetrics.csv")))
      }
    }
  }

  # Classification evaluation plots ----
  if(DebugMode) print('Running ML_EvalPlots()')
  PlotList <- list()
  if('evalplots' %chin% tolower(OutputSelection)) {
    if('score_traindata' %chin% tolower(OutputSelection) && !TrainOnFull) {
      Output <- tryCatch({ML_EvalPlots(ModelType='classification', DataType = 'Train', TrainOnFull.=TrainOnFull, ValidationData.=TrainData, NumOfParDepPlots.=NumOfParDepPlots, VariableImportance.=VariableImportance, TargetColumnName.=TargetColumnName, FeatureColNames.=FeatureColNames, SaveModelObjects.=SaveModelObjects, ModelID.=ModelID, metadata_path.=metadata_path, model_path.=metadata_path, LossFunction.=NULL, EvalMetric.=NULL, EvaluationMetrics.=NULL, predict.=NULL)}, error = function(x) NULL)
      if(length(Output) > 0L) {
        PlotList[['Train_EvaluationPlot']] <- Output$EvaluationPlot; Output$EvaluationPlot <- NULL
        PlotList[['Train_ParDepPlots']] <- Output$ParDepPlots; Output$ParDepPlots <- NULL
        PlotList[['Train_GainsPlot']] <- Output$GainsPlot; Output$GainsPlot <- NULL
        PlotList[['Train_LiftPlot']] <- Output$LiftPlot; Output$LiftPlot <- NULL
        PlotList[['Train_ROC_Plot']] <- Output$ROC_Plot; rm(Output)
        if(!is.null(VariableImportance$Train_Importance) && "plotly" %chin% installed.packages()) PlotList[['Train_VariableImportance']] <- plotly::ggplotly(VI_Plot(Type = 'catboost', VariableImportance$Train_Importance)) else if(!is.null(VariableImportance$Train_Importance)) PlotList[['Train_VariableImportance']] <- VI_Plot(Type = 'catboost', VariableImportance$Train_Importance)
        if(!is.null(VariableImportance$Validation_Importance) && "plotly" %chin% installed.packages()) PlotList[['Validation_VariableImportance']] <- plotly::ggplotly(VI_Plot(Type = 'catboost', VariableImportance$Validation_Importance)) else if(!is.null(VariableImportance$Validation_Importance)) PlotList[['Validation_VariableImportance']] <- VI_Plot(Type = 'catboost', VariableImportance$Validation_Importance)
      }
    }
    Output <- tryCatch({ML_EvalPlots(ModelType='classification', DataType = 'Test', TrainOnFull.=TrainOnFull, ValidationData.=ValidationData, NumOfParDepPlots.=NumOfParDepPlots, VariableImportance.=VariableImportance, TargetColumnName.=TargetColumnName, FeatureColNames.=FeatureColNames, SaveModelObjects.=SaveModelObjects, ModelID.=ModelID, metadata_path.=metadata_path, model_path.=metadata_path, LossFunction.=NULL, EvalMetric.=NULL, EvaluationMetrics.=NULL, predict.=NULL)}, error = function(x) NULL)
    if(length(Output) > 0L) {
      PlotList[['Test_EvaluationPlot']] <- Output$EvaluationPlot; Output$EvaluationPlot <- NULL
      PlotList[['Test_ParDepPlots']] <- Output$ParDepPlots; Output$ParDepPlots <- NULL
      PlotList[['Test_GainsPlot']] <- Output$GainsPlot; Output$GainsPlot <- NULL
      PlotList[['Test_LiftPlot']] <- Output$LiftPlot; Output$LiftPlot <- NULL
      PlotList[['Test_ROC_Plot']] <- Output$ROC_Plot; rm(Output)
      if(!is.null(VariableImportance[['Test_VariableImportance']]) && "plotly" %chin% installed.packages()) PlotList[['Test_VariableImportance']] <- plotly::ggplotly(VI_Plot(Type = 'catboost', VariableImportance[['Test_VariableImportance']])) else if(!is.null(VariableImportance[['Test_VariableImportance']])) PlotList[['Test_VariableImportance']] <- VI_Plot(Type = 'catboost', VariableImportance[['Test_VariableImportance']])
    }
  }

  # Remove extenal files if GridTune is TRUE ----
  if(DebugMode) print('Running CatBoostRemoveFiles()')
  CatBoostRemoveFiles(GridTune. = GridTune, model_path. = model_path)

  # Final Garbage Collection ----
  if(tolower(task_type) == 'gpu') gc()

  # Return Model Objects ----
  if(DebugMode) print('Return Model Objects')
  outputList <- list()
  outputList[["Model"]] <- model
  outputList[["TrainData"]] <- if(exists('ShapValues') && !is.null(ShapValues[['Train_Shap']])) ShapValues[['Train_Shap']] else if(exists('TrainData')) TrainData else NULL
  outputList[["TestData"]] <- if(exists('ShapValues') && !is.null(ShapValues[['Test_Shap']])) ShapValues[['Test_Shap']] else if(exists('ValidationData')) ValidationData else NULL
  outputList[["PlotList"]] <- if(exists('PlotList')) PlotList else NULL
  outputList[["EvaluationMetrics"]] <- if(exists('EvalMetricsList')) EvalMetricsList else NULL
  outputList[["EvaluationMetrics2"]] <- if(exists('EvalMetrics2List')) EvalMetrics2List else NULL
  outputList[["VariableImportance"]] <- if(exists('VariableImportance')) VariableImportance else NULL
  outputList[["InteractionImportance"]] <- if(exists('Interaction')) Interaction else NULL
  outputList[["GridMetrics"]] <- if(exists('ExperimentalGrid') && !is.null(ExperimentalGrid)) ExperimentalGrid else NULL
  outputList[["ColNames"]] <- if(exists('Names')) Names else NULL
  outputList[["FactorLevelsList"]] <- if(exists('FactorLevelsList')) FactorLevelsList else NULL
  outputList[["ArgsList"]] <- ArgsList
  return(outputList)
}
