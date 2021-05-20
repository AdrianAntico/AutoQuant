#' @title AutoCatBoostMultiClass
#'
#' @description AutoCatBoostMultiClass is an automated modeling function that runs a variety of steps. First, a stratified sampling (by the target variable) is done to create train and validation sets. Then, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation metrics, variable importance, and column names used in model fitting. You can download the catboost package using devtools, via: devtools::install_github('catboost/catboost', subdir = 'catboost/R-package').
#'
#' @author Adrian Antico
#' @family Automated Supervised Learning - Multiclass Classification
#'
#' @param OutputSelection You can select what type of output you want returned. Choose from c("Importances", "EvalPlots", "EvalMetrics", "PDFs", "Score_TrainData")
#' @param data This is your data set for training and testing your model
#' @param TrainOnFull Set to TRUE to train on full data and skip over evaluation steps
#' @param ValidationData This is your holdout data set used in modeling either refine your hyperparameters. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TestData This is your holdout data set. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TargetColumnName Either supply the target column name OR the column number where the target is located, but not mixed types. Note that the target column needs to be a 0 | 1 numeric variable.
#' @param FeatureColNames Either supply the feature column names OR the column number where the target is located, but not mixed types. Also, not zero-indexed.
#' @param PrimaryDateColumn Supply a date or datetime column for catboost to utilize time as its basis for handling categorical features, instead of random shuffling
#' @param WeightsColumnName Supply a column name for your weights column. Leave NULL otherwise
#' @param ClassWeights Supply a vector of weights for your target classes. E.g. c(0.25, 1) to weight your 0 class by 0.25 and your 1 class by 1.
#' @param IDcols A vector of column names or column numbers to keep in your data but not include in the modeling.
#' @param task_type Set to "GPU" to utilize your GPU for training. Default is "CPU".
#' @param NumGPUs Set to 1, 2, 3, etc.
#' @param NumOfParDepPlots Number of partial dependence plots to create for each target level
#' @param eval_metric Internal bandit metric. Select from 'MultiClass', 'MultiClassOneVsAll', 'AUC', 'TotalF1', 'MCC', 'Accuracy', 'HingeLoss', 'HammingLoss', 'ZeroOneLoss', 'Kappa', 'WKappa'
#' @param loss_function Select from 'MultiClass' or 'MultiClassOneVsAll'
#' @param grid_eval_metric For evaluating models within grid tuning. Choices include, "accuracy", "microauc", "logloss"
#' @param model_path A character string of your path file to where you want your output saved
#' @param metadata_path A character string of your path file to where you want your model evaluation output saved. If left NULL, all output will be saved to model_path.
#' @param ModelID A character string to name your model and output
#' @param ReturnModelObjects Set to TRUE to output all modeling objects. E.g. plots and evaluation metrics
#' @param SaveModelObjects Set to TRUE to return all modeling objects to your environment
#' @param PassInGrid Defaults to NULL. Pass in a single row of grid from a previous output as a data.table (they are collected as data.tables)
#' @param GridTune Set to TRUE to run a grid tuning procedure. Set a number in MaxModelsInGrid to tell the procedure how many models you want to test.
#' @param MaxModelsInGrid Number of models to test from grid options.
#' @param MaxRunsWithoutNewWinner A number
#' @param MaxRunMinutes In minutes
#' @param BaselineComparison Set to either "default" or "best". Default is to compare each successive model build to the baseline model using max trees (from function args). Best makes the comparison to the current best model.
#' @param MetricPeriods Number of trees to build before evaluating intermediate metrics. Default is 10L
#' @param langevin TRUE or FALSE. Enable stochastic gradient langevin boosting
#' @param diffusion_temperature Default is 10000 and is only used when langevin is set to TRUE
#' @param Trees Bandit grid partitioned. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the trees numbers you want to test. For running grid tuning, a NULL value supplied will mean these values are tested seq(1000L, 10000L, 1000L)
#' @param Depth Bandit grid partitioned. Number, or vector for depth to test.  For running grid tuning, a NULL value supplied will mean these values are tested seq(4L, 16L, 2L)
#' @param LearningRate Bandit grid partitioned. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the LearningRate values to test. For running grid tuning, a NULL value supplied will mean these values are tested c(0.01,0.02,0.03,0.04)
#' @param L2_Leaf_Reg Random testing. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the L2_Leaf_Reg values to test. For running grid tuning, a NULL value supplied will mean these values are tested seq(1.0, 10.0, 1.0)
#' @param RandomStrength A multiplier of randomness added to split evaluations. Default value is 1 which adds no randomness.
#' @param BorderCount Number of splits for numerical features. Catboost defaults to 254 for CPU and 128 for GPU
#' @param RSM CPU only. Random testing. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the RSM values to test. For running grid tuning, a NULL value supplied will mean these values are tested c(0.80, 0.85, 0.90, 0.95, 1.0)
#' @param BootStrapType Random testing. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the BootStrapType values to test. For running grid tuning, a NULL value supplied will mean these values are tested c("Bayesian", "Bernoulli", "Poisson", "MVS", "No")
#' @param GrowPolicy Random testing. NULL, character, or vector for GrowPolicy to test. For grid tuning, supply a vector of values. For running grid tuning, a NULL value supplied will mean these values are tested c("SymmetricTree", "Depthwise", "Lossguide")
#' @param model_size_reg Defaults to 0.5. Set to 0 to allow for bigger models. This is for models with high cardinality categorical features. Valuues greater than 0 will shrink the model and quality will decline but models won't be huge.
#' @param feature_border_type Defaults to "GreedyLogSum". Other options include: Median, Uniform, UniformAndQuantiles, MaxLogSum, MinEntropy
#' @param sampling_unit Default is Group. Other option is Object. if GPU is selected, this will be turned off unless the loss_function is YetiRankPairWise
#' @param subsample Default is NULL. Catboost will turn this into 0.66 for BootStrapTypes Poisson and Bernoulli. 0.80 for MVS. Doesn't apply to others.
#' @param score_function Default is Cosine. CPU options are Cosine and L2. GPU options are Cosine, L2, NewtonL2, and NewtomCosine (not available for Lossguide)
#' @param min_data_in_leaf Default is 1. Cannot be used with SymmetricTree is GrowPolicy
#' @param DebugMode TRUE to print out steps taken
#'
#' @examples
#' \dontrun{
#' # Create some dummy correlated data
#' data <- RemixAutoML::FakeDataGenerator(
#'   Correlation = 0.85,
#'   N = 10000L,
#'   ID = 2L,
#'   ZIP = 0L,
#'   AddDate = FALSE,
#'   Classification = FALSE,
#'   MultiClass = TRUE)
#'
#' # Run function
#' TestModel <- RemixAutoML::AutoCatBoostMultiClass(
#'
#'     # GPU or CPU and the number of available GPUs
#'     task_type = "GPU",
#'     NumGPUs = 1,
#'     TrainOnFull = FALSE,
#'     DebugMode = FALSE,
#'
#'     # Metadata args
#'     OutputSelection = c("Importances", "EvalPlots", "EvalMetrics", "Score_TrainData"),
#'     ModelID = "Test_Model_1",
#'     model_path = normalizePath("./"),
#'     metadata_path = normalizePath("./"),
#'     SaveModelObjects = FALSE,
#'     ReturnModelObjects = TRUE,
#'
#'     # Data args
#'     data = data,
#'     ValidationData = NULL,
#'     TestData = NULL,
#'     TargetColumnName = "Adrian",
#'     FeatureColNames = names(data)[!names(data) %in%
#'       c("IDcol_1", "IDcol_2","Adrian")],
#'     PrimaryDateColumn = NULL,
#'     WeightsColumnName = NULL,
#'     ClassWeights = c(1L,1L,1L,1L,1L),
#'     IDcols = c("IDcol_1","IDcol_2"),
#'
#'     # Model evaluation
#'     eval_metric = "MCC",
#'     loss_function = "MultiClassOneVsAll",
#'     grid_eval_metric = "Accuracy",
#'     MetricPeriods = 10L,
#'     NumOfParDepPlots = 3,
#'
#'     # Grid tuning args
#'     PassInGrid = NULL,
#'     GridTune = TRUE,
#'     MaxModelsInGrid = 30L,
#'     MaxRunsWithoutNewWinner = 20L,
#'     MaxRunMinutes = 24L*60L,
#'     BaselineComparison = "default",
#'
#'     # ML args
#'     langevin = FALSE,
#'     diffusion_temperature = 10000,
#'     Trees = seq(100L, 500L, 50L),
#'     Depth = seq(4L, 8L, 1L),
#'     LearningRate = seq(0.01,0.10,0.01),
#'     L2_Leaf_Reg = seq(1.0, 10.0, 1.0),
#'     RandomStrength = 1,
#'     BorderCount = 254,
#'     RSM = c(0.80, 0.85, 0.90, 0.95, 1.0),
#'     BootStrapType = c("Bayesian", "Bernoulli", "Poisson", "MVS", "No"),
#'     GrowPolicy = c("SymmetricTree", "Depthwise", "Lossguide"),
#'     model_size_reg = 0.5,
#'     feature_border_type = "GreedyLogSum",
#'     sampling_unit = "Object",
#'     subsample = NULL,
#'     score_function = "Cosine",
#'     min_data_in_leaf = 1)
#'
#' # Output
#' TestModel$Model
#' TestModel$ValidationData
#' TestModel$EvaluationMetrics
#' TestModel$Evaluation
#' TestModel$VI_Plot
#' TestModel$VariableImportance
#' TestModel$InteractionImportance
#' TestModel$GridMetrics
#' TestModel$ColNames = Names
#' TestModel$TargetLevels
#' }
#' @return Saves to file and returned in list: VariableImportance.csv, Model (the model), ValidationData.csv, EvaluationMetrics.csv, GridCollect, and GridList
#' @export
AutoCatBoostMultiClass <- function(OutputSelection = c("Importances", "EvalPlots", "EvalMetrics", "Score_TrainData"),
                                   data = NULL,
                                   ValidationData = NULL,
                                   TestData = NULL,
                                   TargetColumnName = NULL,
                                   FeatureColNames = NULL,
                                   PrimaryDateColumn = NULL,
                                   WeightsColumnName = NULL,
                                   IDcols = NULL,
                                   TrainOnFull = FALSE,
                                   task_type = "GPU",
                                   NumGPUs = 1,
                                   DebugMode = FALSE,
                                   ReturnModelObjects = TRUE,
                                   SaveModelObjects = FALSE,
                                   ModelID = "FirstModel",
                                   model_path = NULL,
                                   metadata_path = NULL,
                                   ClassWeights = NULL,
                                   NumOfParDepPlots = 3,
                                   eval_metric = "MultiClassOneVsAll",
                                   loss_function = "MultiClassOneVsAll",
                                   grid_eval_metric = "Accuracy",
                                   BaselineComparison = "default",
                                   MetricPeriods = 10L,
                                   PassInGrid = NULL,
                                   GridTune = FALSE,
                                   MaxModelsInGrid = 30L,
                                   MaxRunsWithoutNewWinner = 20L,
                                   MaxRunMinutes = 24L*60L,
                                   Trees = 50L,
                                   Depth = 6,
                                   LearningRate = NULL,
                                   L2_Leaf_Reg = NULL,
                                   RandomStrength = 1,
                                   BorderCount = 128,
                                   RSM = NULL,
                                   BootStrapType = NULL,
                                   GrowPolicy = NULL,
                                   langevin = FALSE,
                                   diffusion_temperature = 10000,
                                   model_size_reg = 0.5,
                                   feature_border_type = "GreedyLogSum",
                                   sampling_unit = "Object",
                                   subsample = NULL,
                                   score_function = "Cosine",
                                   min_data_in_leaf = 1) {
  # Load catboost ----
  loadNamespace(package = "catboost")

  # Args Checking (ensure args are set consistently) ----
  if(DebugMode) print("Running CatBoostArgsCheck()")
  Output <- CatBoostArgsCheck(ModelType="multiclass", PrimaryDateColumn.=PrimaryDateColumn, GridTune.=GridTune, model_path.=model_path, metadata_path.=metadata_path, ClassWeights.=ClassWeights, LossFunction.=NULL, loss_function.=loss_function, loss_function_value.=NULL, eval_metric.=eval_metric, eval_metric_value.=NULL, task_type.=task_type, NumGPUs.=NumGPUs, MaxModelsInGrid.=MaxModelsInGrid, NumOfParDepPlots.=0,ReturnModelObjects.=ReturnModelObjects, SaveModelObjects.=SaveModelObjects, PassInGrid.=PassInGrid, MetricPeriods.=MetricPeriods, langevin.=langevin, diffusion_temperature.=diffusion_temperature, Trees.=Trees, Depth.=Depth, LearningRate.=LearningRate, L2_Leaf_Reg.=L2_Leaf_Reg,RandomStrength.=RandomStrength, BorderCount.=BorderCount, RSM.=RSM, BootStrapType.=BootStrapType, GrowPolicy.=GrowPolicy, model_size_reg.=model_size_reg, feature_border_type.=feature_border_type, sampling_unit.=sampling_unit, subsample.=subsample, score_function.=score_function, min_data_in_leaf.=min_data_in_leaf)
  score_function <- Output$score_function
  BootStrapType <- Output$BootStrapType
  sampling_unit <- Output$sampling_unit
  LossFunction <- Output$LossFunction
  GrowPolicy <- Output$GrowPolicy
  EvalMetric <- Output$EvalMetric
  task_type <- Output$task_type
  GridTune <- Output$GridTune
  HasTime <- Output$HasTime
  NumGPUs <- Output$NumGPUs
  Depth <- Output$Depth
  RSM <- Output$RSM; rm(Output)

  # Data Prep (model data prep, dummify, create sets) ----
  if(DebugMode) print("Running CatBoostDataPrep()")
  Output <- CatBoostDataPrep(OutputSelection.=OutputSelection, ModelType="multiclass", data.=data, ValidationData.=ValidationData, TestData.=TestData, TargetColumnName.=TargetColumnName, FeatureColNames.=FeatureColNames, PrimaryDateColumn.=PrimaryDateColumn, WeightsColumnName.=WeightsColumnName, IDcols.=IDcols,TrainOnFull.=TrainOnFull, SaveModelObjects.=SaveModelObjects, TransformNumericColumns.=NULL, Methods.=NULL, model_path.=model_path, ModelID.=ModelID, LossFunction.=NULL, EvalMetric.=NULL)
  FinalTestTarget <- Output$FinalTestTarget; Output$FinalTestTarget <- NULL
  UseBestModel <- Output$UseBestModel; Output$UseBestModel <- NULL
  TrainTarget <- Output$TrainTarget; Output$TrainTarget <- NULL
  CatFeatures <- Output$CatFeatures; Output$CatFeatures <- NULL
  TestTarget <- Output$TestTarget; Output$TestTarget <- NULL
  TrainMerge <- Output$TrainMerge; Output$TrainMerge <- NULL
  dataTrain <- Output$dataTrain; Output$dataTrain <- NULL
  TestMerge <- Output$TestMerge; Output$TestMerge <- NULL
  dataTest <- Output$dataTest; Output$dataTest <- NULL
  TestData <- Output$TestData; Output$TestData <- NULL
  TargetLevels <- Output$TargetLevels; Output$TargetLevels <- NULL
  Names <- Output$Names; rm(Output)

  # Create catboost data objects ----
  if(DebugMode) print("Running CatBoostDataConversion()")
  Output <- CatBoostDataConversion(CatFeatures.=CatFeatures, dataTrain.=dataTrain, dataTest.=dataTest, TestData.=TestData, TrainTarget.=TrainTarget, TestTarget.=TestTarget, FinalTestTarget.=FinalTestTarget, TrainOnFull.=TrainOnFull, Weights.=WeightsColumnName)
  TrainPool <- Output$TrainPool; Output$TrainPool <- NULL
  TestPool <- Output$TestPool; Output$TestPool <- NULL
  FinalTestPool <- Output$FinalTestPool; rm(Output)

  # Bring into existence ----
  ExperimentalGrid <- NULL; BestGrid <- NULL

  # Grid tuning ----
  if(GridTune) {
    Output <- CatBoostGridTuner(ModelType="multiclass", TrainOnFull.=TrainOnFull, HasTime=HasTime, BaselineComparison.=BaselineComparison, TargetColumnName.=TargetColumnName, DebugMode.=DebugMode, task_type.=task_type, Trees.=Trees, Depth.=Depth, LearningRate.=LearningRate, L2_Leaf_Reg.=L2_Leaf_Reg, BorderCount.=BorderCount, RandomStrength.=RandomStrength, RSM.=RSM, BootStrapType.=BootStrapType, GrowPolicy.=GrowPolicy, NumGPUs=NumGPUs, LossFunction=LossFunction, EvalMetric=EvalMetric, MetricPeriods=MetricPeriods, ClassWeights=NULL, CostMatrixWeights=NULL, data=data, TrainPool.=TrainPool, TestPool.=TestPool, FinalTestTarget.=FinalTestTarget, TestTarget.=TestTarget, FinalTestPool.=FinalTestPool, TestData.=TestData, TestMerge.=TestMerge, TargetLevels.=TargetLevels, MaxRunsWithoutNewWinner=MaxRunsWithoutNewWinner, MaxModelsInGrid=MaxModelsInGrid, MaxRunMinutes=MaxRunMinutes, SaveModelObjects=SaveModelObjects, metadata_path=metadata_path, model_path=model_path, ModelID=ModelID, grid_eval_metric.=grid_eval_metric)
    ExperimentalGrid <- Output$ExperimentalGrid
    BestGrid <- Output$BestGrid
  }

  # Final Parameters (put parameters in list to pass into catboost) ----
  if(DebugMode) print("Running CatBoostFinalParams()")
  base_params <- CatBoostFinalParams(ModelType="multiclass", UseBestModel.=UseBestModel, ClassWeights.=ClassWeights, PassInGrid.=PassInGrid, BestGrid.=BestGrid, ExperimentalGrid.=ExperimentalGrid, GridTune.=GridTune, TrainOnFull.=TrainOnFull, MetricPeriods.=MetricPeriods, LossFunction.=LossFunction, EvalMetric.=EvalMetric, score_function.=score_function, HasTime.=HasTime, task_type.=task_type, NumGPUs.=NumGPUs, NTrees.=Trees, Depth.=Depth, LearningRate.=LearningRate, L2_Leaf_Reg.=L2_Leaf_Reg, langevin.=langevin, diffusion_temperature.=diffusion_temperature, sampling_unit.=sampling_unit, RandomStrength.=RandomStrength, BorderCount.=BorderCount, RSM.=RSM, GrowPolicy.=GrowPolicy, BootStrapType.=BootStrapType, model_size_reg.=model_size_reg, feature_border_type.=feature_border_type, subsample.=subsample, min_data_in_leaf.=min_data_in_leaf)

  # MultiClass Train Final Model ----
  if(!TrainOnFull) {
    model <- catboost::catboost.train(learn_pool = TrainPool, test_pool = TestPool, params = base_params)
  } else {
    model <- catboost::catboost.train(learn_pool = TrainPool, params = base_params)
  }

  # MultiClass Save Model ----
  if(SaveModelObjects) catboost::catboost.save_model(model = model, model_path = file.path(model_path, ModelID))

  # TrainData + ValidationData Scoring + Shap
  if("score_traindata" %chin% tolower(OutputSelection) && !TrainOnFull) {
    predict <- data.table::as.data.table(cbind(
      1 + catboost::catboost.predict(
        model = model,
        pool = TrainPool,
        prediction_type = "Class"),
      catboost::catboost.predict(
        model = model,
        pool = TrainPool,
        prediction_type = "Probability")))
    if(!is.null(TestPool)) {
      predict_validate <- data.table::as.data.table(cbind(
        1 + catboost::catboost.predict(
          model = model,
          pool = TestPool,
          prediction_type = "Class"),
        catboost::catboost.predict(
          model = model,
          pool = TestPool,
          prediction_type = "Probability")))
      predict <- data.table::rbindlist(list(predict, predict_validate))
      rm(predict_validate)
    }
    TrainData <- CatBoostValidationData(ModelType="multiclass", TrainOnFull.=TRUE, TestDataCheck=FALSE, FinalTestTarget.=FinalTestTarget, TestTarget.=TestTarget, TrainTarget.=TrainTarget, TrainMerge.=TrainMerge, TestMerge.=TestMerge, dataTest.=dataTest, data.=dataTrain, predict.=predict, TargetColumnName.=TargetColumnName, SaveModelObjects. = SaveModelObjects, metadata_path.=metadata_path, model_path.=model_path, ModelID.=ModelID, LossFunction.=NULL, TransformNumericColumns.=NULL, GridTune.=GridTune, TransformationResults.=NULL, TargetLevels.=TargetLevels)
  } else {
    TrainData <- NULL
  }

  # MultiClass Score Final Test Data ----
  predict <- data.table::as.data.table(cbind(
    1 + catboost::catboost.predict(
      model = model,
      pool = if(!is.null(TestData)) FinalTestPool else if(!TrainOnFull) TestPool else TrainPool,
      prediction_type = "Class"),
    catboost::catboost.predict(
      model = model,
      pool = if(!is.null(TestData)) FinalTestPool else if(!TrainOnFull) TestPool else TrainPool,
      prediction_type = "Probability")))

  # MultiClass Validation Data (generate validation data, save to file) ----
  if(DebugMode) print("Running CatBoostValidationData()")
  ValidationData <- CatBoostValidationData(ModelType="multiclass", TrainOnFull.=TrainOnFull, TestDataCheck=!is.null(TestData), FinalTestTarget.=FinalTestTarget, TestTarget.=TestTarget, TrainMerge.=NULL, TrainTarget.=TrainTarget, TestMerge.=TestMerge, dataTest.=dataTest, data.=data, predict.=predict, TargetColumnName.=TargetColumnName, SaveModelObjects. = SaveModelObjects, metadata_path.=metadata_path, model_path.=model_path, ModelID.=ModelID, LossFunction.=NULL, TransformNumericColumns.=NULL, GridTune. = GridTune, TransformationResults. = NULL, TargetLevels.=TargetLevels)

  # Gather importance and shap values ----
  if(DebugMode) print("Running CatBoostImportances()")
  Output <- CatBoostImportances(ModelType="multiclass", TargetColumnName.=TargetColumnName, TrainPool.=TrainPool, TestPool.=TestPool, FinalTestPool.=FinalTestPool, ValidationData.=ValidationData, SaveModelObjects.=SaveModelObjects, model.=model, ModelID.=ModelID, model_path.=model_path, metadata_path.=metadata_path, GrowPolicy.=GrowPolicy)
  VariableImportance <- Output$VariableImportance; Output$VariableImportance <- NULL
  Interaction <- Output$Interaction; Output$Interaction <- NULL

  # Generate EvaluationMetrics ----
  if(DebugMode) print("Running MultiClassMetrics()")
  MultinomialMetrics <- list()
  MultinomialMetrics[["TestData"]] <- MultiClassMetrics(ModelClass="catboost", DataType = "validate", SaveModelObjects.=SaveModelObjects, ValidationData.=ValidationData, PredictData.=predict, TrainOnFull.=TrainOnFull, TargetColumnName.=TargetColumnName, TargetLevels.=TargetLevels, ModelID.=ModelID, model_path.=model_path, metadata_path.=metadata_path)
  if("score_traindata" %chin% tolower(OutputSelection) && !TrainOnFull) {
    MultinomialMetrics[["TrainData"]] <- MultiClassMetrics(ModelClass="catboost", DataType = "train", SaveModelObjects.=SaveModelObjects, ValidationData.=ValidationData, PredictData.=predict, TrainOnFull.=TrainOnFull, TargetColumnName.=TargetColumnName, TargetLevels.=TargetLevels, ModelID.=ModelID, model_path.=model_path, metadata_path.=metadata_path)
  }

  # Generate EvaluationMetrics ----
  if(DebugMode) print("Running BinaryMetrics()")
  EvalMetricsList <- list()
  EvalMetrics2List <- list()
  if("evalmetrics" %chin% tolower(OutputSelection)) {
    if("score_traindata" %chin% tolower(OutputSelection) && !TrainOnFull) {
      for(tarlevel in as.character(unique(TargetLevels[["OriginalLevels"]]))) {
        TrainData[, p1 := get(tarlevel)]
        TrainData[, paste0("Temp_",tarlevel) := data.table::fifelse(Predict == eval(tarlevel), 1, 0)]
        EvalMetricsList[[paste0("TrainData_",tarlevel)]] <- BinaryMetrics(ClassWeights.=c(1,1), CostMatrixWeights.=c(1,0,0,1), SaveModelObjects.=SaveModelObjects, ValidationData.=TrainData, TrainOnFull.=TrainOnFull, TargetColumnName.=paste0("Temp_",tarlevel), ModelID.=ModelID, model_path.=model_path, metadata_path.=metadata_path, Method = "threshold")
        EvalMetrics2List[[paste0("TrainData_",tarlevel)]] <- BinaryMetrics(ClassWeights.=c(1,1), CostMatrixWeights.=c(1,0,0,1), SaveModelObjects.=SaveModelObjects, ValidationData.=TrainData, TrainOnFull.=TrainOnFull, TargetColumnName.=paste0("Temp_",tarlevel), ModelID.=ModelID, model_path.=model_path, metadata_path.=metadata_path, Method = "bins")
        data.table::set(TrainData, j = c("p1",paste0("Temp_",tarlevel)), value = NULL)
      }
    }
    for(tarlevel in as.character(unique(TargetLevels[["OriginalLevels"]]))) {
      ValidationData[, p1 := get(tarlevel)]
      ValidationData[, paste0("Temp_",tarlevel) := data.table::fifelse(Predict == eval(tarlevel), 1, 0)]
      EvalMetricsList[[paste0("TestData_",tarlevel)]] <- BinaryMetrics(ClassWeights.=c(1,1), CostMatrixWeights.=c(1,0,0,1), SaveModelObjects.=SaveModelObjects, ValidationData.=ValidationData, TrainOnFull.=TrainOnFull, TargetColumnName.=paste0("Temp_",tarlevel), ModelID.=ModelID, model_path.=model_path, metadata_path.=metadata_path, Method = "threshold")
      EvalMetrics2List[[paste0("TestData_",tarlevel)]] <- BinaryMetrics(ClassWeights.=c(1,1), CostMatrixWeights.=c(1,0,0,1), SaveModelObjects.=SaveModelObjects, ValidationData.=ValidationData, TrainOnFull.=TrainOnFull, TargetColumnName.=paste0("Temp_",tarlevel), ModelID.=ModelID, model_path.=model_path, metadata_path.=metadata_path, Method = "bins")
      data.table::set(ValidationData, j = c("p1",paste0("Temp_",tarlevel)), value = NULL)
    }
  }

  # Classification evaluation plots ----
  if(DebugMode) print("Running ML_EvalPlots()")
  PlotList <- list()
  options(warn = -1)
  if("evalplots" %chin% tolower(OutputSelection)) {
    if("score_traindata" %chin% tolower(OutputSelection) && !TrainOnFull) {
      if(!is.null(VariableImportance$Train_Importance)) PlotList[["Train_VariableImportance"]] <- VI_Plot(Type = "catboost", VariableImportance$Train_Importance)
      if(!is.null(VariableImportance$Validation_Importance)) PlotList[["Validation_VariableImportance"]] <- VI_Plot(Type = "catboost", VariableImportance$Validation_Importance)
      for(tarlevel in as.character(unique(TargetLevels[["OriginalLevels"]]))) {
        TrainData[, p1 := get(tarlevel)]
        TrainData[, paste0("Temp_",tarlevel) := data.table::fifelse(Predict == eval(tarlevel), 1, 0)]
        Output <- ML_EvalPlots(ModelType="classification", TrainOnFull.=TrainOnFull, ValidationData.=TrainData, NumOfParDepPlots.=NumOfParDepPlots, VariableImportance.=VariableImportance, TargetColumnName.=paste0("Temp_",tarlevel), FeatureColNames.=FeatureColNames, SaveModelObjects.=FALSE, ModelID.=ModelID, metadata_path.=metadata_path, model_path.=model_path, LossFunction.=NULL, EvalMetric.=NULL, EvaluationMetrics.=NULL, predict.=NULL)
        PlotList[[paste0("Train_EvaluationPlot_",tarlevel)]] <- Output$EvaluationPlot; Output$EvaluationPlot <- NULL
        PlotList[[paste0("Train_ParDepPlots_",tarlevel)]] <- Output$ParDepPlots; Output$ParDepPlots <- NULL
        PlotList[[paste0("Train_GainsPlot_",tarlevel)]] <- Output$GainsPlot; Output$GainsPlot <- NULL
        PlotList[[paste0("Train_LiftPlot_",tarlevel)]] <- Output$LiftPlot; Output$LiftPlot <- NULL
        PlotList[[paste0("Train_ROC_Plot_",tarlevel)]] <- Output$ROC_Plot; rm(Output)
        data.table::set(TrainData, j = c("p1",paste0("Temp_",tarlevel)), value = NULL)
      }
    }
    if(!is.null(VariableImportance[["Test_Importance"]])) PlotList[["Test_VariableImportance"]] <- plotly::ggplotly(VI_Plot(Type = "catboost", VariableImportance[["Test_Importance"]])) else PlotList[["Train_VariableImportance"]] <- VI_Plot(Type = "catboost", VariableImportance[["Train_Importance"]])
    for(tarlevel in as.character(unique(TargetLevels[["OriginalLevels"]]))) {
      ValidationData[, p1 := get(tarlevel)]
      ValidationData[, paste0("Temp_",tarlevel) := data.table::fifelse(Predict == eval(tarlevel), 1, 0)]
      Output <- ML_EvalPlots(ModelType="classification", TrainOnFull.=TrainOnFull, ValidationData.=ValidationData, NumOfParDepPlots.=NumOfParDepPlots, VariableImportance.=VariableImportance, TargetColumnName.=TargetColumnName, FeatureColNames.=FeatureColNames, SaveModelObjects.=SaveModelObjects, ModelID.=ModelID, metadata_path.=metadata_path, model_path.=model_path, LossFunction.=NULL, EvalMetric.=NULL, EvaluationMetrics.=NULL, predict.=NULL)
      PlotList[[paste0("Test_EvaluationPlot_",tarlevel)]] <- Output$EvaluationPlot; Output$EvaluationPlot <- NULL
      PlotList[[paste0("Test_ParDepPlots_",tarlevel)]] <- Output$ParDepPlots; Output$ParDepPlots <- NULL
      PlotList[[paste0("Test_GainsPlot_",tarlevel)]] <- Output$GainsPlot; Output$GainsPlot <- NULL
      PlotList[[paste0("Test_LiftPlot_",tarlevel)]] <- Output$LiftPlot; Output$LiftPlot <- NULL
      PlotList[[paste0("Test_ROC_Plot_",tarlevel)]] <- Output$ROC_Plot; rm(Output)
      data.table::set(ValidationData, j = c("p1",paste0("Temp_",tarlevel)), value = NULL)
    }
  }

  # Send output to pdf ----
  if(DebugMode) print("Running CatBoostPDF()")
  if("pdfs" %chin% tolower(OutputSelection) && SaveModelObjects) {
    CatBoostPDF(ModelClass = "catboost", ModelType="classification", TrainOnFull.=TrainOnFull, SaveInfoToPDF.=SaveInfoToPDF, PlotList.=PlotList, VariableImportance.=VariableImportance, EvalMetricsList.=EvalMetricsList, Interaction.=Interaction, model_path.=model_path, metadata_path.=metadata_path)
  }

  # Remove extenal files if GridTune is TRUE ----
  if(DebugMode) print("Running CatBoostRemoveFiles()")
  CatBoostRemoveFiles(GridTune. = GridTune, model_path. = model_path)

  # Final Garbage Collection ----
  if(tolower(task_type) == "gpu") gc()

  # Return Model Objects ----
  if(DebugMode) print("Return Model Objects")
    if(ReturnModelObjects) {
    return(list(
      Model = model,
      TrainData = if(exists("TrainData")) TrainData else NULL,
      TestData = if(exists("ValidationData") && !is.null(ValidationData)) ValidationData else NULL,
      PlotList = if(exists("PlotList")) PlotList else NULL,
      MultinomialMetrics = if(exists("MultinomialMetrics") && !is.null(MultinomialMetrics)) MultinomialMetrics else NULL,
      EvaluationMetrics = if(exists("EvalMetricsList")) EvalMetricsList else NULL,
      EvaluationMetrics2 = if(exists("EvalMetrics2List")) EvalMetrics2List else NULL,
      VariableImportance = if(exists("VariableImportance")) VariableImportance else NULL,
      InteractionImportance = if(exists("Interaction")) Interaction else NULL,
      GridMetrics = if(exists("ExperimentalGrid") && !is.null(ExperimentalGrid)) ExperimentalGrid else NULL,
      ColNames = if(exists("Names") && !is.null(Names)) Names else NULL,
      TargetLevels = if(exists("TargetLevels") && !is.null(TargetLevels)) TargetLevels else NULL))
  }
}
