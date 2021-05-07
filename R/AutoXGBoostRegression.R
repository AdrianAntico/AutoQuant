#' @title AutoXGBoostRegression
#'
#' @description AutoXGBoostRegression is an automated XGBoost modeling framework with grid-tuning and model evaluation that runs a variety of steps. First, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation plot, evaluation boxplot, evaluation metrics, variable importance, partial dependence calibration plots, partial dependence calibration box plots, and column names used in model fitting.
#'
#' @author Adrian Antico
#' @family Automated Supervised Learning - Regression
#'
#' @param data This is your data set for training and testing your model
#' @param TrainOnFull Set to TRUE to train on full data
#' @param DebugMode Set to TRUE to get a print out of the steps taken throughout the function
#' @param ValidationData This is your holdout data set used in modeling either refine your hyperparameters.
#' @param TestData This is your holdout data set. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TargetColumnName Either supply the target column name OR the column number where the target is located (but not mixed types).
#' @param FeatureColNames Either supply the feature column names OR the column number where the target is located (but not mixed types)
#' @param IDcols A vector of column names or column numbers to keep in your data but not include in the modeling.
#' @param ReturnFactorLevels Set to TRUE to have the factor levels returned with the other model objects
#' @param TransformNumericColumns Set to NULL to do nothing; otherwise supply the column names of numeric variables you want transformed
#' @param Methods Choose from "BoxCox", "Asinh", "Asin", "Log", "LogPlus1", "Sqrt", "Logit", "YeoJohnson". Function will determine if one cannot be used because of the underlying data.
#' @param LossFunction Default is 'reg:squarederror'. Other options include 'reg:squaredlogerror', 'reg:pseudohubererror', 'count:poisson', 'survival:cox', 'survival:aft', 'aft_loss_distribution', 'reg:gamma', 'reg:tweedie'
#' @param eval_metric This is the metric used to identify best grid tuned model. Choose from "r2", "RMSE", "MSE", "MAE"
#' @param GridTune Set to TRUE to run a grid tuning procedure. Set a number in MaxModelsInGrid to tell the procedure how many models you want to test.
#' @param grid_eval_metric "mae", "mape", "rmse", "r2". Case sensitive
#' @param NThreads Set the maximum number of threads you'd like to dedicate to the model run. E.g. 8
#' @param TreeMethod Choose from "hist", "gpu_hist"
#' @param MaxModelsInGrid Number of models to test from grid options (243 total possible options)
#' @param model_path A character string of your path file to where you want your output saved
#' @param metadata_path A character string of your path file to where you want your model evaluation output saved. If left NULL, all output will be saved to model_path.
#' @param ModelID A character string to name your model and output
#' @param NumOfParDepPlots Tell the function the number of partial dependence calibration plots you want to create.
#' @param Verbose Set to 0 if you want to suppress model evaluation updates in training
#' @param EncodingMethod Choose from 'binary', 'm_estimator', 'credibility', 'woe', 'target_encoding', 'poly_encode', 'backward_difference', 'helmert'
#' @param ReturnModelObjects Set to TRUE to output all modeling objects (E.g. plots and evaluation metrics)
#' @param SaveModelObjects Set to TRUE to return all modeling objects to your environment
#' @param SaveInfoToPDF Set to TRUE to save model insights to pdf
#' @param PassInGrid Default is NULL. Provide a data.table of grid options from a previous run.
#' @param MaxRunsWithoutNewWinner Runs without new winner to end procedure
#' @param MaxRunMinutes In minutes
#' @param BaselineComparison Set to either "default" or "best". Default is to compare each successive model build to the baseline model using max trees (from function args). Best makes the comparison to the current best model.
#' @param Trees Bandit grid partitioned. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the trees numbers you want to test. For running grid tuning, a NULL value supplied will mean these values are tested seq(1000L, 10000L, 1000L)
#' @param eta Bandit grid partitioned. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the LearningRate values to test. For running grid tuning, a NULL value supplied will mean these values are tested c(0.01,0.02,0.03,0.04)
#' @param max_depth Bandit grid partitioned. Number, or vector for depth to test.  For running grid tuning, a NULL value supplied will mean these values are tested seq(4L, 16L, 2L)
#' @param min_child_weight Number, or vector for min_child_weight to test.  For running grid tuning, a NULL value supplied will mean these values are tested seq(1.0, 10.0, 1.0)
#' @param subsample Number, or vector for subsample to test.  For running grid tuning, a NULL value supplied will mean these values are tested seq(0.55, 1.0, 0.05)
#' @param colsample_bytree Number, or vector for colsample_bytree to test.  For running grid tuning, a NULL value supplied will mean these values are tested seq(0.55, 1.0, 0.05)
#' @examples
#' \dontrun{
#' # Create some dummy correlated data
#' data <- RemixAutoML::FakeDataGenerator(
#'   Correlation = 0.85,
#'   N = 1000,
#'   ID = 2,
#'   ZIP = 0,
#'   AddDate = FALSE,
#'   Classification = FALSE,
#'   MultiClass = FALSE)
#'
#' # Run function
#' TestModel <- RemixAutoML::AutoXGBoostRegression(
#'
#'     # GPU or CPU
#'     TreeMethod = "hist",
#'     NThreads = parallel::detectCores(),
#'     LossFunction = 'reg:squarederror',
#'
#'     # Metadata args
#'     model_path = normalizePath("./"),
#'     metadata_path = NULL,
#'     ModelID = "Test_Model_1",
#'     EncodingMethod = "binary",
#'     ReturnFactorLevels = TRUE,
#'     ReturnModelObjects = TRUE,
#'     SaveModelObjects = FALSE,
#'     SaveInfoToPDF = FALSE,
#'     DebugMode = FALSE,
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
#'     TransformNumericColumns = NULL,
#'     Methods = c("BoxCox", "Asinh", "Asin", "Log",
#'       "LogPlus1", "Sqrt", "Logit", "YeoJohnson"),
#'
#'     # Model evaluation args
#'     eval_metric = "rmse",
#'     NumOfParDepPlots = 3L,
#'
#'     # Grid tuning args
#'     PassInGrid = NULL,
#'     GridTune = FALSE,
#'     grid_eval_metric = "r2",
#'     BaselineComparison = "default",
#'     MaxModelsInGrid = 10L,
#'     MaxRunsWithoutNewWinner = 20L,
#'     MaxRunMinutes = 24L*60L,
#'     Verbose = 1L,
#'
#'     # ML args
#'     Trees = 50L,
#'     eta = 0.05,
#'     max_depth = 4L,
#'     min_child_weight = 1.0,
#'     subsample = 0.55,
#'     colsample_bytree = 0.55)
#' }
#' @return Saves to file and returned in list: VariableImportance.csv, Model, ValidationData.csv, EvalutionPlot.png, EvalutionBoxPlot.png, EvaluationMetrics.csv, ParDepPlots.R a named list of features with partial dependence calibration plots, ParDepBoxPlots.R, GridCollect, and GridList
#' @export
AutoXGBoostRegression <- function(data,
                                  TrainOnFull = FALSE,
                                  ValidationData = NULL,
                                  TestData = NULL,
                                  TargetColumnName = NULL,
                                  FeatureColNames = NULL,
                                  IDcols = NULL,
                                  model_path = NULL,
                                  metadata_path = NULL,
                                  DebugMode = FALSE,
                                  SaveInfoToPDF = FALSE,
                                  ModelID = "FirstModel",
                                  EncodingMethod = "binary",
                                  ReturnFactorLevels = TRUE,
                                  ReturnModelObjects = TRUE,
                                  SaveModelObjects = FALSE,
                                  TransformNumericColumns = NULL,
                                  Methods = c("BoxCox", "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit"),
                                  Verbose = 0L,
                                  NumOfParDepPlots = 3L,
                                  NThreads = parallel::detectCores(),
                                  LossFunction = 'reg:squarederror',
                                  eval_metric = "rmse",
                                  grid_eval_metric = "r2",
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
  Output <- XGBoostDataPrep(ModelType="regression", data.=data, ValidationData.=ValidationData, TestData.=TestData, TargetColumnName.=TargetColumnName, FeatureColNames.=FeatureColNames, IDcols.=IDcols, TransformNumericColumns.=TransformNumericColumns, Methods.=Methods, ModelID.=ModelID, model_path.=model_path, TrainOnFull.=TrainOnFull, SaveModelObjects.=SaveModelObjects, ReturnFactorLevels.=ReturnFactorLevels, EncodingMethod.=EncodingMethod)
  TransformNumericColumns <- Output$TransformNumericColumns; Output$TransformNumericColumns <- NULL
  TransformationResults <- Output$TransformationResults; Output$TransformationResults <- NULL
  FactorLevelsList <- Output$FactorLevelsList; Output$FactorLevelsList <- NULL
  FinalTestTarget <- Output$FinalTestTarget; Output$FinalTestTarget <- NULL
  datavalidate <- Output$datavalidate; Output$datavalidate <- NULL
  TargetLevels <- Output$TargetLevels; Output$TargetLevels <- NULL
  TrainTarget <- Output$TrainTarget; Output$TrainTarget <- NULL
  TrainMerge <- Output$TrainMerge; Output$TrainMerge <- NULL
  ValidMerge <- Output$ValidMerge; Output$ValidMerge <- NULL
  TestTarget <- Output$TestTarget; Output$TestTarget <- NULL
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
    Output <- XGBoostGridTuner(ModelType="regression", TrainOnFull.=TrainOnFull, TargetColumnName.=TargetColumnName, DebugMode.=DebugMode, TreeMethod.=TreeMethod, Trees.=Trees, Depth.=max_depth, LearningRate.=eta, min_child_weight.=min_child_weight, subsample.=subsample, colsample_bytree.=colsample_bytree, LossFunction=LossFunction, EvalMetric=eval_metric, grid_eval_metric.=grid_eval_metric, CostMatrixWeights=NULL, datatrain.=datatrain, datavalidate.=datavalidate, datatest.=datatest, EvalSets.=EvalSets, TestTarget.=TestTarget, FinalTestTarget.=FinalTestTarget, TargetLevels.=TargetLevels, MaxRunsWithoutNewWinner=MaxRunsWithoutNewWinner, MaxModelsInGrid=MaxModelsInGrid, MaxRunMinutes=MaxRunMinutes, BaselineComparison.=BaselineComparison, SaveModelObjects=SaveModelObjects, metadata_path=metadata_path, model_path=model_path, ModelID=ModelID, Verbose.=Verbose, NumLevels.=NULL)
    ExperimentalGrid <- Output$ExperimentalGrid
    BestGrid <- Output$BestGrid
  }

  # Final Params ----
  if(DebugMode) print("Final Params ----")
  Output <- XGBoostFinalParams(TrainOnFull.=TrainOnFull, PassInGrid.=PassInGrid, BestGrid.=BestGrid, GridTune.=GridTune, LossFunction.=LossFunction, eval_metric.=eval_metric, NThreads.=NThreads, TreeMethod.=TreeMethod, Trees.=Trees)
  base_params <- Output$base_params
  NTrees <- if(length(Output$NTrees) > 1L) max(Output$NTrees) else Output$NTrees; rm(Output)

  # Build model ----
  if(DebugMode) print("Build model ----")
  model <- xgboost::xgb.train(params = base_params, data = datatrain, watchlist = EvalSets, nrounds = NTrees, Verbose = Verbose)

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
  predict <- stats::predict(object = model, if(!is.null(TestData)) datatest else if(!is.null(ValidationData) && !TrainOnFull) datavalidate else datatrain)

  # Validation, Importance, Shap data ----
  Output <- XGBoostValidation(ModelType.="regression", TrainOnFull.=TrainOnFull, model.=model, TargetColumnName.=TargetColumnName, SaveModelObjects.=SaveModelObjects, metadata_path.=metadata_path, model_path.=model_path, ModelID.=ModelID, TestData.=TestData, TestTarget.=TestTarget, FinalTestTarget.=FinalTestTarget, TestMerge.=TestMerge, dataTest.=dataTest, TrainTarget.=TrainTarget, dataTrain.=dataTrain, Final.=NULL, predict.=predict, TransformNumericColumns.=TransformNumericColumns, TransformationResults.=TransformationResults, GridTune.=GridTune, data.=data)
  TransformationResults <- Output$TransformationResults; Output$TransformationResults <- NULL
  VariableImportance <- Output$VariableImportance; Output$VariableImportance <- NULL
  ValidationData <- Output$ValidationData; Output$ValidationData <- NULL
  ShapValues <- Output$ShapValues; rm(Output)

  # Eval Metrics ----
  if(DebugMode) print("Eval Metrics ----")
  EvaluationMetrics <- RegressionMetrics(SaveModelObjects.=SaveModelObjects, data.=data, ValidationData.=ValidationData, TrainOnFull.=TrainOnFull, LossFunction.="Adrian", EvalMetric.=NULL, TargetColumnName.=TargetColumnName, ModelID.=ModelID, model_path.=model_path, metadata_path.=metadata_path)

  # Generate plots ----
  if(DebugMode) print("Generate plots ----")
  Output <- ML_EvalPlots(ModelType="regression", TrainOnFull.=TrainOnFull, LossFunction.=LossFunction, EvalMetric.=EvalMetric, EvaluationMetrics.=EvaluationMetrics, ValidationData.=ValidationData, NumOfParDepPlots.=NumOfParDepPlots, VariableImportance.=VariableImportance, TargetColumnName.=TargetColumnName, FeatureColNames.=FeatureColNames, SaveModelObjects.=SaveModelObjects, ModelID.=ModelID, metadata_path.=metadata_path, model_path.=model_path, predict.=predict)
  EvaluationBoxPlot <- Output$EvaluationBoxPlot; Output$EvaluationBoxPlot <- NULL
  EvaluationPlot <- Output$EvaluationPlot; Output$EvaluationPlot <- NULL
  ParDepBoxPlots <- Output$ParDepBoxPlots; Output$ParDepBoxPlots <- NULL
  ParDepPlots <- Output$ParDepPlots; rm(Output)

  # Subset Transformation Object ----
  if(DebugMode) print("Subset Transformation Object ----")
  if(!is.null(TransformNumericColumns)) {
    if(TargetColumnName == "Target") {
      TransformationResults <- TransformationResults[!(ColumnName %chin% c("Predict"))]
    } else {
      TransformationResults <- TransformationResults[!(ColumnName %chin% c("Predict", "Target"))]
    }
  }

  # Save PDF of model information ----
  if(DebugMode) print("Save PDF of model information ----")
  CatBoostPDF(ModelClass = "xgboost", ModelType="regression", TrainOnFull.=TrainOnFull, SaveInfoToPDF.=SaveInfoToPDF, EvaluationPlot.=EvaluationPlot, EvaluationBoxPlot.=EvaluationBoxPlot, VariableImportance.=VariableImportance, ParDepPlots.=ParDepPlots, ParDepBoxPlots.=ParDepBoxPlots, EvalMetrics.=EvaluationMetrics, Interaction.=NULL, model_path.=model_path, metadata_path.=metadata_path)

  # FactorLevelsList ----
  if(!exists("FactorLevelsList")) FactorLevelsList <- NULL

  # Return Model Objects ----
  if(DebugMode) print("Return Model Objects ----")
  if(ReturnModelObjects) {
    return(list(
      Model = model,
      ValidationData = if(exists("ValidationData")) ValidationData else NULL,
      ShapValues = if(exists("ShapValues")) ShapValues else NULL,
      EvaluationPlot = if(exists("EvaluationPlot")) EvaluationPlot else NULL,
      EvaluationBoxPlot = if(exists("EvaluationBoxPlot")) EvaluationBoxPlot else NULL,
      EvaluationMetrics = if(exists("EvaluationMetrics")) EvaluationMetrics else NULL,
      VariableImportance = if(exists("VariableImportance")) VariableImportance else NULL,
      VI_Plot = if(exists("VariableImportance") && !is.null(VariableImportance)) tryCatch({if(all(c("plotly","dplyr") %chin% installed.packages())) plotly::ggplotly(VI_Plot(Type = "h2o", VariableImportance)) else VI_Plot(Type = "h2o", VariableImportance)}, error = function(x) NULL) else NULL,
      PartialDependencePlots = if(exists("ParDepPlots")) ParDepPlots else NULL,
      PartialDependenceBoxPlots = if(exists("ParDepBoxPlots")) ParDepBoxPlots else NULL,
      GridMetrics = if(exists("ExperimentalGrid")) ExperimentalGrid else NULL,
      ColNames = if(exists("Names")) Names else NULL,
      TransformationResults = if(exists("TransformationResults")) TransformationResults else NULL,
      FactorLevelsList = if(exists("FactorLevelsList")) FactorLevelsList else NULL))
  }
}
