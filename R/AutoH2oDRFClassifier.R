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

#' @title AutoH2oDRFClassifier
#'
#' @description AutoH2oDRFClassifier is an automated H2O modeling framework with grid-tuning and model evaluation that runs a variety of steps. First, a stratified sampling (by the target variable) is done to create train and validation sets. Then, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation plot, evaluation metrics, variable importance, partial dependence calibration plots, and column names used in model fitting.
#'
#' @author Adrian Antico
#' @family Automated Supervised Learning - Binary Classification
#'
#' @param OutputSelection You can select what type of output you want returned. Choose from "EvalMetrics", "Score_TrainData", "h2o.explain"
#' @param data This is your data set for training and testing your model
#' @param TrainOnFull Set to TRUE to train on full data
#' @param ValidationData This is your holdout data set used in modeling either refine your hyperparameters.
#' @param TestData This is your holdout data set. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TargetColumnName Either supply the target column name OR the column number where the target is located (but not mixed types). Note that the target column needs to be a 0 | 1 numeric variable.
#' @param FeatureColNames Either supply the feature column names OR the column number where the target is located (but not mixed types)
#' @param CostMatrixWeights A vector with 4 elements c(True Positive Cost, False Negative Cost, False Positive Cost, True Negative Cost). Default c(1,0,0,1),
#' @param WeightsColumn Column name of a weights column
#' @param H2OShutdown Set to TRUE to shutdown H2O after running the function
#' @param H2OStartUp Defaults to TRUE which means H2O will be started inside the function
#' @param ReturnModelObjects Set to TRUE to output all modeling objects (E.g. plots and evaluation metrics)
#' @param SaveModelObjects Set to TRUE to return all modeling objects to your environment
#' @param IfSaveModel Set to "mojo" to save a mojo file, otherwise "standard" to save a regular H2O model object
#' @param DebugMode Set to TRUE to get a printout of each step taken internally
#' @param SaveInfoToPDF Set to TRUE to save modeling information to PDF. If model_path or metadata_path aren't defined then output will be saved to the working directory
#' @param ModelID A character string to name your model and output
#' @param MaxMem Set the maximum amount of memory you'd like to dedicate to the model run. E.g. "32G"
#' @param NThreads Set the number of threads you want to dedicate to the model building
#' @param model_path A character string of your path file to where you want your output saved
#' @param metadata_path A character string of your path file to where you want your model evaluation output saved. If left NULL, all output will be saved to model_path.
#' @param NumOfParDepPlots Tell the function the number of partial dependence calibration plots you want to create.
#' @param eval_metric This is the metric used to identify best grid tuned model. Choose from "AUC" or "logloss"
#' @param GridTune Set to TRUE to run a grid tuning procedure. Set a number in MaxModelsInGrid to tell the procedure how many models you want to test.
#' @param GridStrategy Default "Cartesian"
#' @param MaxRunTimeSecs Default 86400
#' @param MaxModelsInGrid Number of models to test from grid options (1080 total possible options)
#' @param StoppingRounds Default 10
#' @param Trees The maximum number of trees you want in your models
#' @param MaxDepth Default 20
#' @param SampleRate Default 0.632
#' @param MTries Default -1 means it will default to number of features divided by 3
#' @param ColSampleRatePerTree Default 1
#' @param ColSampleRatePerTreeLevel Default 1
#' @param MinRows Default 1
#' @param NBinsCats Default 1024
#' @param NBinsTopLevel Default 1024
#' @param HistogramType Default "AUTO"
#' @param CategoricalEncoding Default "AUTO"
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
#' TestModel <- AutoQuant::AutoH2oDRFClassifier(
#'
#'   # Compute management args
#'   MaxMem = {gc();paste0(as.character(floor(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE)) / 1000000)),"G")},
#'   NThreads = max(1L, parallel::detectCores() - 2L),
#'   IfSaveModel = "mojo",
#'   H2OShutdown = FALSE,
#'   H2OStartUp = TRUE,
#'
#'   # Model evaluation args
#'   eval_metric = "auc",
#'   NumOfParDepPlots = 3L,
#'   CostMatrixWeights = c(1,0,0,1),
#'
#'   # Metadata args
#'   OutputSelection = c("EvalMetrics","Score_TrainData"),
#'   model_path = normalizePath("./"),
#'   metadata_path = NULL,
#'   ModelID = "FirstModel",
#'   ReturnModelObjects = TRUE,
#'   SaveModelObjects = FALSE,
#'   SaveInfoToPDF = FALSE,
#'   DebugMode = FALSE,
#'
#'   # Data args
#'   data,
#'   TrainOnFull = FALSE,
#'   ValidationData = NULL,
#'   TestData = NULL,
#'   TargetColumnName = "Adrian",
#'   FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2", "Adrian")],
#'   WeightsColumn = NULL,
#'
#'   # Grid Tuning Args
#'   GridStrategy = "RandomDiscrete",
#'   GridTune = FALSE,
#'   MaxModelsInGrid = 10,
#'   MaxRunTimeSecs = 60*60*24,
#'   StoppingRounds = 10,
#'
#'   # Model args
#'   Trees = 50L,
#'   MaxDepth = 20,
#'   SampleRate = 0.632,
#'   MTries = -1,
#'   ColSampleRatePerTree = 1,
#'   ColSampleRatePerTreeLevel = 1,
#'   MinRows = 1,
#'   NBins = 20,
#'   NBinsCats = 1024,
#'   NBinsTopLevel = 1024,
#'   HistogramType = "AUTO",
#'   CategoricalEncoding = "AUTO")
#' }
#' @return Saves to file and returned in list: VariableImportance.csv, Model, ValidationData.csv, EvalutionPlot.png, EvaluationMetrics.csv, ParDepPlots.R a named list of features with partial dependence calibration plots, GridCollect, and GridList
#' @export
AutoH2oDRFClassifier <- function(OutputSelection = c("EvalMetrics","Score_TrainData"),
                                 data = NULL,
                                 TrainOnFull = FALSE,
                                 ValidationData = NULL,
                                 TestData = NULL,
                                 TargetColumnName = NULL,
                                 FeatureColNames = NULL,
                                 WeightsColumn = NULL,
                                 MaxMem = {gc();paste0(as.character(floor(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE)) / 1000000)),"G")},
                                 NThreads = max(1L, parallel::detectCores() - 2L),
                                 model_path = NULL,
                                 metadata_path = NULL,
                                 ModelID = "FirstModel",
                                 NumOfParDepPlots = 3L,
                                 ReturnModelObjects = TRUE,
                                 SaveModelObjects = FALSE,
                                 SaveInfoToPDF = FALSE,
                                 IfSaveModel = "mojo",
                                 H2OShutdown = FALSE,
                                 H2OStartUp = TRUE,
                                 GridTune = FALSE,
                                 GridStrategy = "RandomDiscrete",
                                 MaxRunTimeSecs = 60*60*24,
                                 StoppingRounds = 10,
                                 MaxModelsInGrid = 2,
                                 DebugMode = FALSE,
                                 eval_metric = "auc",
                                 CostMatrixWeights = c(1,0,0,1),
                                 Trees = 50L,
                                 MaxDepth = 20L,
                                 SampleRate = 0.632,
                                 MTries = -1,
                                 ColSampleRatePerTree = 1,
                                 ColSampleRatePerTreeLevel  = 1,
                                 MinRows = 1,
                                 NBins = 20,
                                 NBinsCats = 1024,
                                 NBinsTopLevel = 1024,
                                 HistogramType = "AUTO",
                                 CategoricalEncoding = "AUTO") {

  # Args check ----
  Decreasing <- H2OArgsCheck(ModelType="drf", TargetType = "classification", model_path.=model_path, metadata_path.=metadata_path, eval_metric.=eval_metric, MaxModelsInGrid.=MaxModelsInGrid, ModelID.=ModelID, NumOfParDepPlots.=NumOfParDepPlots, ReturnModelObjects.=ReturnModelObjects, SaveModelObjects.=SaveModelObjects, GridTune.=GridTune, GridStrategy.=GridStrategy, CostMatrixWeights.=CostMatrixWeights, IfSaveModel.=IfSaveModel, Trees.=Trees, MaxDepth.=MaxDepth, SampleRate.=SampleRate, MTries.=MTries, ColSampleRatePerTree.=ColSampleRatePerTree, ColSampleRatePerTreeLevel.=ColSampleRatePerTreeLevel, MinRows.=MinRows, NBins.=NBins, NBinsCats.=NBinsCats, NBinsTopLevel.=NBinsTopLevel, HistogramType.=HistogramType, CategoricalEncoding.=CategoricalEncoding)

  # Grab all official parameters and their evaluated arguments
  ArgsList <- c(as.list(environment()))
  ArgsList[['data']] <- NULL
  ArgsList[['ValidationData']] <- NULL
  ArgsList[['TestData']] <- NULL
  ArgsList[['Algo']] <- "H2ODRF"
  ArgsList[['TargetType']] <- "Binary Classification"
  ArgsList[['PredictionColumnName']] <- "p1"
  if(SaveModelObjects) {
    if(!is.null(metadata_path)) {
      save(ArgsList, file = file.path(metadata_path, paste0(ModelID, "_ArgsList.Rdata")))
    }
  }

  # Data Prepare ----
  if(DebugMode) print("Data Prepare ----")
  Output <- H2ODataPrep(TargetType.="classifier", TargetColumnName.=TargetColumnName, data.=data, ValidationData.=ValidationData, TestData.=TestData, TrainOnFull.=TrainOnFull, FeatureColNames.=FeatureColNames, SaveModelObjects.=SaveModelObjects, model_path.=metadata_path, ModelID.=ModelID)
  TargetColumnName <- Output$TargetColumnName; Output$TargetColumnName <- NULL
  dataTrain <- Output$dataTrain; Output$dataTrain <- NULL
  dataTest <- Output$dataTest; Output$dataTest <- NULL
  TestData <- Output$TestData; Output$TestData <- NULL
  Names <- Output$Names; rm(Output)

  # Grid Tune ----
  if(GridTune && !TrainOnFull) {

    # Debug
    if(DebugMode) print("Grid Tune ----")

    # Start Up H2O ----
    if(H2OStartUp) localHost <- h2o::h2o.init(nthreads = NThreads, max_mem_size = MaxMem, enable_assertions = FALSE)
    datatrain <- h2o::as.h2o(dataTrain)
    if(!TrainOnFull) datavalidate <- h2o::as.h2o(dataTest, use_datatable = TRUE) else datavalidate <- NULL
    if(!is.null(TestData)) datatest <- h2o::as.h2o(TestData, use_datatable = TRUE) else datatest <- NULL

    # Grid Tune Search Criteria ----
    search_criteria  <- list(strategy=GridStrategy, max_runtime_secs=MaxRunTimeSecs, max_models=MaxModelsInGrid, seed=1234, stopping_rounds=StoppingRounds, stopping_metric=toupper(eval_metric), stopping_tolerance=1e-3)

    # Grid Parameters ----
    hyper_params <- list()
    hyper_params[["ntrees"]] <- Trees
    hyper_params[["max_depth"]] <- MaxDepth
    hyper_params[["sample_rate"]] <- SampleRate
    hyper_params[["col_sample_rate_per_tree"]] <- ColSampleRatePerTree
    hyper_params[["col_sample_rate_change_per_level"]] <- ColSampleRatePerTreeLevel
    hyper_params[["min_rows"]] <- MinRows
    hyper_params[["nbins"]] <- NBins
    hyper_params[["nbins_cats"]] <- NBinsCats
    hyper_params[["histogram_type"]] <- HistogramType
    hyper_params[["nbins_top_level"]] <- NBinsTopLevel
    hyper_params[["categorical_encoding"]] <- CategoricalEncoding
    hyper_params[["mtries"]] <- MTries

    # Grid Train Model ----
    grid <- h2o::h2o.grid(
      hyper_params = hyper_params,
      search_criteria = if(GridStrategy != "Cartesian") search_criteria else NULL,
      is_supervised = TRUE,
      algorithm = "randomForest",
      grid_id = paste0(ModelID, "_Grid"),
      x = FeatureColNames,
      y = TargetColumnName,
      training_frame = datatrain,
      validation_frame = datavalidate,
      max_runtime_secs = 3600 * 24 * 7,
      stopping_rounds = 10,
      stopping_tolerance = 1e-3,
      stopping_metric = eval_metric,
      score_tree_interval = 10,
      seed = 1234)

    # Get Best Model ----
    Grid_Out <- h2o::h2o.getGrid(grid_id = paste0(ModelID, "_Grid"), sort_by = eval_metric, decreasing = Decreasing)

    # Collect Best Grid Model ----
    base_model <- h2o::h2o.getModel(Grid_Out@model_ids[[1L]])
  }

  # Build Model ----
  if(!GridTune) {

    # Debug ----
    if(DebugMode) print("Build Model ----")

    # Prepare data ----
    if(H2OStartUp) localHost <- h2o::h2o.init(nthreads = NThreads, max_mem_size = MaxMem, enable_assertions = FALSE)
    datatrain <- h2o::as.h2o(dataTrain, use_datatable = TRUE)
    if(!TrainOnFull) datavalidate <- h2o::as.h2o(dataTest, use_datatable = TRUE) else datavalidate <- NULL
    if(!is.null(TestData)) datatest <- h2o::as.h2o(TestData, use_datatable = TRUE) else datatest <- NULL

    # Define args ----
    H2OArgs <- list()
    H2OArgs[["x"]] <- FeatureColNames
    H2OArgs[["y"]] <- TargetColumnName
    H2OArgs[["weights_column"]] <- WeightsColumn
    H2OArgs[["training_frame"]] <- datatrain
    H2OArgs[["validation_frame"]] <- datavalidate
    H2OArgs[["model_id"]] <- ModelID
    H2OArgs[["ntrees"]] <- Trees
    H2OArgs[["max_depth"]] <- MaxDepth
    H2OArgs[["sample_rate"]] <- SampleRate
    H2OArgs[["col_sample_rate_per_tree"]] <- ColSampleRatePerTree
    H2OArgs[["col_sample_rate_change_per_level"]] <- ColSampleRatePerTreeLevel
    H2OArgs[["min_rows"]] <- MinRows
    H2OArgs[["nbins"]] <- NBins
    H2OArgs[["nbins_cats"]] <- NBinsCats
    H2OArgs[["histogram_type"]] <- HistogramType
    H2OArgs[["nbins_top_level"]] <- NBinsTopLevel
    H2OArgs[["categorical_encoding"]] <- CategoricalEncoding
    H2OArgs[["mtries"]] <- MTries

    # Build model ----
    base_model <- do.call(h2o::h2o.randomForest, H2OArgs)
  }

  # Save Final Model ----
  if(DebugMode) print("Save Final Model ----")
  H2OSaveModel(SaveModelObjects.=SaveModelObjects, IfSaveModel.=IfSaveModel, base_model.=base_model, model_path.=model_path, ModelID.=ModelID)

  # Score Train Data ----
  if(DebugMode) print("Score Final Test Data ----")
  if("score_traindata" %chin% tolower(OutputSelection) && !TrainOnFull) {
    Predict <- data.table::as.data.table(h2o::h2o.predict(object = base_model, newdata = datatrain))
  }

  # Create Train Validation Data ----
  if(DebugMode) print("Create Validation Data ----")
  if("score_traindata" %chin% tolower(OutputSelection) && !TrainOnFull) {
    Output <- H2OValidationData(Predict.=Predict, TestData.=NULL, dataTest.=NULL, dataTrain.=dataTrain, TrainOnFull.=TRUE, SaveModelObjects.=SaveModelObjects, metadata_path.=metadata_path, model_path.=metadata_path, ModelID.=ModelID, TransformNumericColumns.=NULL, TransformationResults.=NULL, TargetColumnName.=NULL, data.=NULL)
    TrainData <- Output$ValidationData; rm(Output)
  }

  # Score Validation Data ----
  Predict <- data.table::as.data.table(h2o::h2o.predict(object = base_model, newdata = if(!is.null(TestData)) datatest else if(!TrainOnFull) datavalidate else datatrain))
  data.table::set(Predict, j = "p0", value = NULL)

  # Create Validation Data ----
  Output <- H2OValidationData(Predict.=Predict, TestData.=TestData, dataTest.=dataTest, dataTrain.=dataTrain, TrainOnFull.=TrainOnFull, SaveModelObjects.=SaveModelObjects, metadata_path.=metadata_path, model_path.=metadata_path, ModelID.=ModelID, TransformNumericColumns.=NULL, TransformationResults.=NULL, TargetColumnName.=NULL, data.=NULL)
  ValidationData <- Output$ValidationData; rm(Output)

  # Variable Importance ----
  if(DebugMode) print("Variable Importance ----")
  VariableImportance <- H2OVariableImportance(TrainOnFull.=TrainOnFull, base_model.=base_model, SaveModelObjects.=SaveModelObjects, metadata_path.=metadata_path, model_path.=metadata_path, ModelID.=ModelID)

  # H2O Explain TrainData ----
  if(DebugMode) print("H2O Explain TrainData ----")
  ExplainList <- list()
  if(!TrainOnFull && 'h2o.explain' %in% OutputSelection) {
    ExplainList[["Train_Explain"]] <- h2o::h2o.explain(base_model, newdata = datatrain)
  }

  # H2O Explain ValidationData ----
  if(DebugMode) print("H2O Explain ValidationData ----")
  if(!TrainOnFull && 'h2o.explain' %in% OutputSelection) {
    ExplainList[["Test_Explain"]] <- h2o::h2o.explain(base_model, newdata = if(!is.null(TestData)) datatest else if(!is.null(ValidationData) && !TrainOnFull) datavalidate else datatrain)
  }

  # H2O Shutdown ----
  if(DebugMode) print("H2O Shutdown ----")
  if(H2OShutdown) h2o::h2o.shutdown(prompt = FALSE)

  # Generate EvaluationMetrics ----
  if(DebugMode) print("Running BinaryMetrics()")
  EvalMetricsList <- list()
  EvalMetrics2List <- list()
  if("evalmetrics" %chin% tolower(OutputSelection)) {
    if("score_traindata" %chin% tolower(OutputSelection) && !TrainOnFull) {
      EvalMetricsList[["TrainData"]] <- BinaryMetrics(ClassWeights.=NULL, CostMatrixWeights.=CostMatrixWeights, SaveModelObjects.=FALSE, ValidationData.=TrainData, TrainOnFull.=TrainOnFull, TargetColumnName.=TargetColumnName, ModelID.=ModelID, model_path.=metadata_path, metadata_path.=metadata_path, Method = "threshold")
      EvalMetrics2List[["TrainData"]] <- BinaryMetrics(ClassWeights.=NULL, CostMatrixWeights.=CostMatrixWeights, SaveModelObjects.=FALSE, ValidationData.=TrainData, TrainOnFull.=TrainOnFull, TargetColumnName.=TargetColumnName, ModelID.=ModelID, model_path.=metadata_path, metadata_path.=metadata_path, Method = "bins")
      if(SaveModelObjects) {
        if(!is.null(metadata_path)) {
          data.table::fwrite(EvalMetricsList[['TrainData']], file = file.path(metadata_path, paste0(ModelID, "_Train_EvaluationMetrics.csv")))
        }
      }
    }
    EvalMetricsList[["TestData"]] <- BinaryMetrics(ClassWeights.=NULL, CostMatrixWeights.=CostMatrixWeights, SaveModelObjects.=FALSE, ValidationData.=ValidationData, TrainOnFull.=TrainOnFull, TargetColumnName.=TargetColumnName, ModelID.=ModelID, model_path.=metadata_path, metadata_path.=metadata_path, Method = "threshold")
    EvalMetrics2List[["TestData"]] <- BinaryMetrics(ClassWeights.=NULL, CostMatrixWeights.=CostMatrixWeights, SaveModelObjects.=FALSE, ValidationData.=ValidationData, TrainOnFull.=TrainOnFull, TargetColumnName.=TargetColumnName, ModelID.=ModelID, model_path.=metadata_path, metadata_path.=metadata_path, Method = "bins")
    if(SaveModelObjects) {
      if(!is.null(metadata_path)) {
        data.table::fwrite(EvalMetricsList[['TestData']], file = file.path(metadata_path, paste0(ModelID, "_Test_EvaluationMetrics.csv")))
      }
    }
  }

  # Return Objects ----
  if(DebugMode) print("Return Objects ----")
  if(ReturnModelObjects) {
    outputList <- list()
    outputList[["Model"]] <- base_model
    outputList[["TrainData"]] <- if(exists("TrainData") && !is.null(TrainData)) TrainData else NULL
    outputList[["TestData"]] <- if(exists("ValidationData") && !is.null(ValidationData)) ValidationData else NULL
    outputList[["H2OExplain"]] <- if(exists("ExplainList")) ExplainList else NULL
    outputList[["EvaluationMetrics"]] <- if(exists("EvalMetricsList")) EvalMetricsList else NULL
    outputList[["VariableImportance"]] <- if(exists("VariableImportance")) VariableImportance else NULL
    outputList[["EvaluationMetrics2"]] <- if(exists("EvalMetrics2List")) EvalMetrics2List else NULL
    outputList[["ColNames"]] <- if(exists("Names")) Names else NULL
    outputList[["ArgsList"]] <- ArgsList
    return(outputList)
  }
}
