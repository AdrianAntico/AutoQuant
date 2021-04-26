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
#' @param grid_eval_metric Case sensitive. I typically choose 'Utility' or 'MCC'. Choose from 'Utility', 'MCC', 'Acc', 'F1_Score', 'F2_Score', 'F0.5_Score', 'TPR', 'TNR', 'FNR', 'FPR', 'FDR', 'FOR', 'NPV', 'PPV', 'ThreatScore'
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
#'     grid_eval_metric = "MCC",
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
                                  grid_eval_metric = "MCC",
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
  if(DebugMode) print("Check args ----")
  XGBoostArgsCheck(GridTune.=GridTune, model_path.=model_path, metadata_path.=metadata_path, Trees.=Trees, max_depth.=max_depth, eta.=eta, min_child_weight.=min_child_weight, subsample.=subsample, colsample_bytree.=colsample_bytree)

  # Data prep ----
  if(DebugMode) print("Data prep ----")
  Output <- XGBoostDataPrep(ModelType="classification", data.=data, ValidationData.=ValidationData, TestData.=TestData, TargetColumnName.=TargetColumnName, FeatureColNames.=FeatureColNames, IDcols.=IDcols, TransformNumericColumns.=NULL, Methods.=NULL, ModelID.=ModelID, model_path.=model_path, TrainOnFull.=TrainOnFull, SaveModelObjects.=SaveModelObjects, ReturnFactorLevels.=ReturnFactorLevels)
  FactorLevelsList <- Output$FactorLevelsList; Output$FactorLevelsList <- NULL
  FinalTestTarget <- Output$FinalTestTarget; Output$FinalTestTarget <- NULL
  datavalidate <- Output$datavalidate; Output$datavalidate <- NULL
  TrainTarget <- Output$TrainTarget; Output$TrainTarget <- NULL
  TestTarget <- Output$TestTarget; Output$TestTarget <- NULL
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
  Output <- XGBoostFinalParams(PassInGrid.=PassInGrid, TrainOnFull.=TrainOnFull, BestGrid.=BestGrid, GridTune.=GridTune, LossFunction.=LossFunction, eval_metric.=eval_metric, NThreads.=NThreads, TreeMethod.=TreeMethod, Trees.=Trees)
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
  predict <- stats::predict(model, if(!is.null(TestData)) datatest else if(!TrainOnFull) datavalidate else datatrain)

  # Validation, Importance, Shap data ----
  Output <- XGBoostValidation(ModelType.="classifier", TrainOnFull.=TrainOnFull, model.=model, TargetColumnName.=TargetColumnName, SaveModelObjects.=SaveModelObjects, metadata_path.=metadata_path, model_path.=model_path, ModelID.=ModelID, TestData.=TestData, TestTarget.=TestTarget, FinalTestTarget.=FinalTestTarget, TestMerge.=TestMerge, dataTest.=dataTest, TrainTarget.=TrainTarget, dataTrain.=dataTrain, Final.=NULL, predict.=predict, TransformNumericColumns.=NULL, TransformationResults.=NULL, GridTune.=NULL, data.=NULL)
  VariableImportance <- Output$VariableImportance; Output$VariableImportance <- NULL
  ValidationData <- Output$ValidationData; Output$ValidationData <- NULL
  ShapValues <- Output$ShapValues; rm(Output)

  # Evaluation plots ----
  if(DebugMode) print("Evaluation plots ----")
  Output <- ML_EvalPlots(ModelType="classification", TrainOnFull.=TrainOnFull, ValidationData.=ValidationData, NumOfParDepPlots.=NumOfParDepPlots, VariableImportance.=VariableImportance, TargetColumnName.=TargetColumnName, FeatureColNames.=FeatureColNames, SaveModelObjects.=SaveModelObjects, ModelID.=ModelID, metadata_path.=metadata_path, model_path.=model_path, LossFunction.=NULL, EvalMetric.=NULL, EvaluationMetrics.=NULL, predict.=NULL)
  EvaluationPlot <- Output$EvaluationPlot; Output$EvaluationPlot <- NULL
  ParDepPlots <- Output$ParDepPlots; Output$ParDepPlots <- NULL
  GainsPlot <- Output$Gains; Output$GainsPlot <- NULL
  LiftPlot <- Output$LiftPlot; Output$LiftPlot <- NULL
  ROC_Plot <- Output$ROC_Plot; rm(Output)

  # Generate EvaluationMetrics ----
  if(DebugMode) print("Generate EvaluationMetrics ----")
  EvalMetrics <- BinaryMetrics(ClassWeights.=NULL, CostMatrixWeights.=CostMatrixWeights, SaveModelObjects.=SaveModelObjects, ValidationData.=ValidationData, TrainOnFull.=TrainOnFull, TargetColumnName.=TargetColumnName, ModelID.=ModelID, model_path.=model_path, metadata_path.=metadata_path)

  # Send output to pdf ----
  if(DebugMode) print("Send output to pdf ----")
  CatBoostPDF(ModelClass = "xgboost", ModelType="classification", TrainOnFull.=TrainOnFull, SaveInfoToPDF.=SaveInfoToPDF, EvaluationPlot.=EvaluationPlot, EvaluationBoxPlot.=NULL, ROC_Plot.=ROC_Plot, Gains.=GainsPlot, Lift.=LiftPlot, VariableImportance.=VariableImportance, ParDepPlots.=ParDepPlots, ParDepBoxPlots.=NULL, EvalMetrics.=EvalMetrics, Interaction.=NULL, model_path.=model_path, metadata_path.=metadata_path)

  # Binary Return Model Objects ----
  if(!exists("FactorLevelsList")) FactorLevelsList <- NULL

  # Return objects ----
  if(DebugMode) print("Return objects ----")
  if(ReturnModelObjects) {
    return(list(
      Model = model,
      ValidationData = if(exists("ValidationData")) ValidationData else NULL,
      GainsPlot = if(exists("GainsPlot")) GainsPlot else NULL,
      LiftPlot = if(exists("LiftPlot")) LiftPlot else NULL,
      ROC_Plot = if(exists("ROC_Plot")) ROC_Plot else NULL,
      EvaluationPlot = if(exists("EvaluationPlot")) EvaluationPlot else NULL,
      EvaluationMetrics = if(exists("EvalMetrics")) EvalMetrics else NULL,
      VariableImportance = if(exists("VariableImportance")) VariableImportance else NULL,
      ShapValues = if(exists("ShapValues")) ShapValues else NULL,
      VI_Plot = if(exists("VariableImportance") && !is.null(VariableImportance)) tryCatch({if(all(c("plotly","dplyr") %chin% installed.packages())) plotly::ggplotly(VI_Plot(Type = "h2o", VariableImportance)) else VI_Plot(Type = "h2o", VariableImportance)}, error = function(x) NULL) else NULL,
      PartialDependencePlots = if(exists("ParDepPlots")) ParDepPlots else NULL,
      GridMetrics = if(exists("ExperimentalGrid") && !is.null(ExperimentalGrid)) data.table::setorderv(ExperimentalGrid, cols = "EvalMetric", order = -1L, na.last = TRUE) else NULL,
      ColNames = if(exists("Names")) Names else NULL,
      FactorLevels = if(exists("FactorLevelsList")) FactorLevelsList else NULL))
  }
}
