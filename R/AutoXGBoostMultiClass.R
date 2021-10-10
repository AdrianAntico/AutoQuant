#' @title AutoXGBoostMultiClass
#'
#' @description AutoXGBoostMultiClass is an automated XGBoost modeling framework with grid-tuning and model evaluation that runs a variety of steps. First, a stratified sampling (by the target variable) is done to create train and validation sets. Then, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation metrics, variable importance, and column names used in model fitting.
#'
#' @author Adrian Antico
#' @family Automated Supervised Learning - Multiclass Classification
#'
#' @param OutputSelection You can select what type of output you want returned. Choose from c("Importances", "EvalPlots", "EvalMetrics", "PDFs", "Score_TrainData")
#' @param data This is your data set for training and testing your model
#' @param TrainOnFull Set to TRUE to train on full data
#' @param ValidationData This is your holdout data set used in modeling either refine your hyperparameters.
#' @param TestData This is your holdout data set. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TargetColumnName Either supply the target column name OR the column number where the target is located (but not mixed types). Note that the target column needs to be a 0 | 1 numeric variable.
#' @param FeatureColNames Either supply the feature column names OR the column number where the target is located (but not mixed types)
#' @param WeightsColumnName Supply a column name for your weights column. Leave NULL otherwise
#' @param IDcols A vector of column names or column numbers to keep in your data but not include in the modeling.
#' @param eval_metric This is the metric used to identify best grid tuned model. Choose from "logloss","error","aucpr","auc"
#' @param NThreads Set the maximum number of threads you'd like to dedicate to the model run. E.g. 8
#' @param TreeMethod Choose from "hist", "gpu_hist"
#' @param model_path A character string of your path file to where you want your output saved
#' @param metadata_path A character string of your path file to where you want your model evaluation output saved. If left NULL, all output will be saved to model_path.
#' @param ModelID A character string to name your model and output
#' @param NumOfParDepPlots Tell the function the number of partial dependence calibration plots you want to create.
#' @param Verbose Set to 0 if you want to suppress model evaluation updates in training
#' @param EncodingMethod Choose from 'binary', 'm_estimator', 'credibility', 'woe', 'target_encoding', 'poly_encode', 'backward_difference', 'helmert'
#' @param ReturnModelObjects Set to TRUE to output all modeling objects (E.g. plots and evaluation metrics)
#' @param ReturnFactorLevels TRUE or FALSE. Set to FALSE to not return factor levels.
#' @param SaveModelObjects Set to TRUE to return all modeling objects to your environment
#' @param GridTune Set to TRUE to run a grid tuning procedure
#' @param LossFunction Use 'multi:sofprob', I set it up to return the class label and the individual probabilities, just like catboost. Doesn't come like that off the shelf
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
# Create some dummy correlated data
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
#'   # GPU or CPU
#'   TreeMethod = "hist",
#'   NThreads = parallel::detectCores(),
#'
#'   # Metadata args
#'   OutputSelection = c("Importances", "EvalPlots", "EvalMetrics", "PDFs", "Score_TrainData"),
#'   model_path = normalizePath("./"),
#'   metadata_path = normalizePath("./"),
#'   ModelID = "Test_Model_1",
#'   EncodingMethod = "binary",
#'   ReturnFactorLevels = TRUE,
#'   ReturnModelObjects = TRUE,
#'   SaveModelObjects = FALSE,
#'
#'   # Data args
#'   data = data,
#'   TrainOnFull = FALSE,
#'   ValidationData = NULL,
#'   TestData = NULL,
#'   TargetColumnName = "Adrian",
#'   FeatureColNames = names(data)[!names(data) %in%
#'                                   c("IDcol_1", "IDcol_2","Adrian")],
#'   WeightsColumnName = NULL,
#'   IDcols = c("IDcol_1","IDcol_2"),
#'
#'   # Model evaluation args
#'   eval_metric = "merror",
#'   LossFunction = 'multi:softprob',
#'   grid_eval_metric = "accuracy",
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
#'   DebugMode = FALSE,
#'
#'   # ML args
#'   Trees = 50L,
#'   eta = 0.05,
#'   max_depth = 4L,
#'   min_child_weight = 1.0,
#'   subsample = 0.55,
#'   colsample_bytree = 0.55)
#' }
#' @return Saves to file and returned in list: VariableImportance.csv, Model, ValidationData.csv, EvaluationMetrics.csv, GridCollect, GridList, and TargetLevels
#' @export
AutoXGBoostMultiClass <- function(OutputSelection = c("Importances", "EvalPlots", "EvalMetrics", "Score_TrainData"),
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
                                  ModelID = "FirstModel",
                                  LossFunction = 'multi:softprob',
                                  EncodingMethod = "credibility",
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
  if(EncodingMethod %chin% c("target_encode", "credibility", "m_estimator", "woe")) EncodingMethod <- "poly_encode"
  Output <- XGBoostDataPrep(Algo="xgboost", ModelType="multiclass", data.=data, ValidationData.=ValidationData, TestData.=TestData, TargetColumnName.=TargetColumnName, FeatureColNames.=FeatureColNames, WeightsColumnName.=WeightsColumnName, IDcols.=IDcols, TransformNumericColumns.=NULL, Methods.=NULL, ModelID.=ModelID, model_path.=model_path, TrainOnFull.=TrainOnFull, SaveModelObjects.=SaveModelObjects, ReturnFactorLevels.=ReturnFactorLevels, EncodingMethod.=EncodingMethod, DebugMode.=DebugMode)
  FactorLevelsList <- Output$FactorLevelsList; Output$FactorLevelsList <- NULL
  FinalTestTarget <- Output$FinalTestTarget; Output$FinalTestTarget <- NULL
  WeightsVector <- Output$WeightsVector; Output$WeightsVector <- NULL
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
  if(SaveModelObjects) save(model, file = file.path(model_path, ModelID))

  # Grid Score Model ----
  if(DebugMode) print("Grid Score Model ----")
  if(!is.null(datatest)) {
    predict <- XGBoostMultiClassPredict(model=model, datatest=datatest, TargetLevels=TargetLevels, NumLevels=NumLevels, NumberRows=nrow(datatest))
  } else if(!is.null(datavalidate)) {
    predict <- XGBoostMultiClassPredict(model=model, datatest=datavalidate, TargetLevels=TargetLevels, NumLevels=NumLevels, NumberRows=nrow(datavalidate))
  } else {
    predict <- XGBoostMultiClassPredict(model=model, datatest=datatrain, TargetLevels=TargetLevels, NumLevels=NumLevels, NumberRows=nrow(datatrain))
  }

  # Validation, Importance, Shap data ----
  if(DebugMode) print("Validation, Importance, Shap data ----")
  Output <- XGBoostValidationData(TrainMerge.=TrainMerge, ModelType="multiclass", TestDataCheck=!is.null(TestData), TrainOnFull.=TrainOnFull, model.=model, TargetColumnName.=TargetColumnName, SaveModelObjects.=SaveModelObjects, metadata_path.=metadata_path, model_path.=model_path, ModelID.=ModelID, TestData.=TestData, TestTarget.=TestTarget, FinalTestTarget.=FinalTestTarget, TestMerge.=TestMerge, dataTest.=dataTest, TrainTarget.=TrainTarget, predict.=predict, TransformNumericColumns.=NULL, TransformationResults.=NULL, GridTune.=NULL, data.=dataTrain, LossFunction.=LossFunction)
  VariableImportance <- Output[['VariableImportance']]; Output$VariableImportance <- NULL
  ValidationData <- Output$ValidationData; rm(Output)

  # TrainData + ValidationData Scoring + Shap ----
  if(DebugMode) print("TrainData + ValidationData Scoring + Shap ----")
  if("score_traindata" %chin% tolower(OutputSelection) && !TrainOnFull) {
    predict <- XGBoostMultiClassPredict(model=model, datatest=datatrain, TargetLevels=TargetLevels, NumLevels=NumLevels, NumberRows=nrow(datatrain))
    if(!is.null(datatest)) {
      predict_validate <- XGBoostMultiClassPredict(model=model, datatest=datavalidate, TargetLevels=TargetLevels, NumLevels=NumLevels, NumberRows=nrow(datavalidate))
      predict <- data.table::rbindlist(list(predict, predict_validate))
      rm(predict_validate)
    }
    Output <- XGBoostValidationData(model.=model, TestData.=NULL, ModelType="multiclass", TrainOnFull.=TRUE, TestDataCheck=FALSE, FinalTestTarget.=FinalTestTarget, TestTarget.=TestTarget, TrainTarget.=TrainTarget, TrainMerge.=TrainMerge, TestMerge.=TestMerge, dataTest.=dataTest, data.=dataTrain, predict.=predict, TargetColumnName.=TargetColumnName, SaveModelObjects. = SaveModelObjects, metadata_path.=metadata_path, model_path.=model_path, ModelID.=ModelID, LossFunction.=LossFunction, TransformNumericColumns.=NULL, GridTune.=GridTune, TransformationResults.=NULL, TargetLevels.=NULL)
    TrainData <- Output$ValidationData; rm(Output)
  } else {
    TrainData <- NULL
  }

  # Generate EvaluationMetrics ----
  if(DebugMode) print("Running MultiClassMetrics()")
  MultinomialMetrics <- list()
  MultinomialMetrics[["TestData"]] <- MultiClassMetrics(ModelClass="xgboost", DataType = "Test", SaveModelObjects.=SaveModelObjects, ValidationData.=ValidationData, PredictData.=predict, TrainOnFull.=TrainOnFull, TargetColumnName.=TargetColumnName, TargetLevels.=TargetLevels, ModelID.=ModelID, model_path.=model_path, metadata_path.=metadata_path)
  if("score_traindata" %chin% tolower(OutputSelection) && !TrainOnFull) {
    MultinomialMetrics[["TrainData"]] <- MultiClassMetrics(ModelClass="xgboost", DataType = "Train", SaveModelObjects.=SaveModelObjects, ValidationData.=ValidationData, PredictData.=predict, TrainOnFull.=TrainOnFull, TargetColumnName.=TargetColumnName, TargetLevels.=TargetLevels, ModelID.=ModelID, model_path.=model_path, metadata_path.=metadata_path)
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
    if(SaveModelObjects) {
      if(!is.null(metadata_path)) {
        save(EvalMetricsList, file = file.path(metadata_path, paste0(ModelID, "_EvaluationMetrics.csv")))
      } else if(!is.null(model_path)) {
        save(EvalMetricsList, file = file.path(model_path, paste0(ModelID, "_EvaluationMetrics.csv")))
      }
    }
  }

  # Classification evaluation plots ----
  if(DebugMode) print("Running ML_EvalPlots()")
  PlotList <- list()
  options(warn = -1)
  if("evalplots" %chin% tolower(OutputSelection)) {
    if("score_traindata" %chin% tolower(OutputSelection) && !TrainOnFull) {
      for(tarlevel in as.character(unique(TargetLevels[["OriginalLevels"]]))) {
        TrainData[, p1 := get(tarlevel)]
        TrainData[, paste0("Temp_",tarlevel) := data.table::fifelse(Predict == eval(tarlevel), 1, 0)]
        if(length(unique(TrainData[[paste0('Temp_',tarlevel)]])) == 1) next
        Output <- ML_EvalPlots(ModelType="classification", TrainOnFull.=TrainOnFull, ValidationData.=TrainData, NumOfParDepPlots.=NumOfParDepPlots, VariableImportance.=VariableImportance, TargetColumnName.=paste0("Temp_",tarlevel), FeatureColNames.=FeatureColNames, SaveModelObjects.=FALSE, ModelID.=ModelID, metadata_path.=metadata_path, model_path.=model_path, LossFunction.=NULL, EvalMetric.=NULL, EvaluationMetrics.=NULL, predict.=NULL)
        PlotList[[paste0("Train_EvaluationPlot_",tarlevel)]] <- Output$EvaluationPlot; Output$EvaluationPlot <- NULL
        PlotList[[paste0("Train_ParDepPlots_",tarlevel)]] <- Output$ParDepPlots; Output$ParDepPlots <- NULL
        PlotList[[paste0("Train_GainsPlot_",tarlevel)]] <- Output$GainsPlot; Output$GainsPlot <- NULL
        PlotList[[paste0("Train_LiftPlot_",tarlevel)]] <- Output$LiftPlot; Output$LiftPlot <- NULL
        PlotList[[paste0("Train_ROC_Plot_",tarlevel)]] <- Output$ROC_Plot; rm(Output)
        data.table::set(TrainData, j = c("p1",paste0("Temp_",tarlevel)), value = NULL)
      }
    }
    if(!is.null(VariableImportance) && "plotly" %chin% installed.packages()) PlotList[["VariableImportance"]] <- plotly::ggplotly(VI_Plot(Type = "xgboost", VariableImportance)) else if(!is.null(VariableImportance)) PlotList[["VariableImportance"]] <- VI_Plot(Type = "xgboost", VariableImportance)
    for(tarlevel in as.character(unique(TargetLevels[["OriginalLevels"]]))) {
      ValidationData[, p1 := get(tarlevel)]
      ValidationData[, paste0("Temp_",tarlevel) := data.table::fifelse(Predict == eval(tarlevel), 1, 0)]
      if(length(unique(ValidationData[[paste0('Temp_',tarlevel)]])) == 1) next
      Output <- ML_EvalPlots(ModelType="classification", TrainOnFull.=TrainOnFull, ValidationData.=ValidationData, NumOfParDepPlots.=NumOfParDepPlots, VariableImportance.=VariableImportance, TargetColumnName.=TargetColumnName, FeatureColNames.=FeatureColNames, SaveModelObjects.=SaveModelObjects, ModelID.=ModelID, metadata_path.=metadata_path, model_path.=model_path, LossFunction.=NULL, EvalMetric.=NULL, EvaluationMetrics.=NULL, predict.=NULL)
      PlotList[[paste0("Test_EvaluationPlot_",tarlevel)]] <- Output$EvaluationPlot; Output$EvaluationPlot <- NULL
      PlotList[[paste0("Test_ParDepPlots_",tarlevel)]] <- Output$ParDepPlots; Output$ParDepPlots <- NULL
      PlotList[[paste0("Test_GainsPlot_",tarlevel)]] <- Output$GainsPlot; Output$GainsPlot <- NULL
      PlotList[[paste0("Test_LiftPlot_",tarlevel)]] <- Output$LiftPlot; Output$LiftPlot <- NULL
      PlotList[[paste0("Test_ROC_Plot_",tarlevel)]] <- Output$ROC_Plot; rm(Output)
      data.table::set(ValidationData, j = c("p1",paste0("Temp_",tarlevel)), value = NULL)
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

  # Running CatBoostPDF() ----
  if(DebugMode) print("Running CatBoostPDF()")
  if("pdfs" %chin% tolower(OutputSelection) && SaveModelObjects) {
    CatBoostPDF(ModelClass = "xgboost", ModelType="classification", TrainOnFull.=TrainOnFull, SaveInfoToPDF.=SaveInfoToPDF, PlotList.=PlotList, VariableImportance.=VariableImportance, EvalMetricsList.=EvalMetricsList, Interaction.=NULL, model_path.=model_path, metadata_path.=metadata_path)
  }

  # Return Model Objects ----
  if(DebugMode) print("Return Model Objects ----")
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
      GridMetrics = if(exists("ExperimentalGrid") && !is.null(ExperimentalGrid)) ExperimentalGrid else NULL,
      ColNames = if(exists("Names") && !is.null(Names)) Names else NULL,
      FactorLevelsList = if(exists("FactorLevelsList")) FactorLevelsList else NULL,
      TargetLevels = if(exists("TargetLevels") && !is.null(TargetLevels)) TargetLevels else NULL,
      ArgsList = ArgsList))
  }
}
