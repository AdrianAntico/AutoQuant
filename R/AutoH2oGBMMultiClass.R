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

#' @title AutoH2oGBMMultiClass
#'
#' @description AutoH2oGBMMultiClass is an automated H2O modeling framework with grid-tuning and model evaluation that runs a variety of steps. First, a stratified sampling (by the target variable) is done to create train and validation sets. Then, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation metrics, confusion matrix, and variable importance.
#'
#' @author Adrian Antico
#' @family Automated Supervised Learning - Multiclass Classification
#'
#' @param OutputSelection You can select what type of output you want returned. Choose from c("EvalMetrics", "Score_TrainData", "h2o.explain")
#' @param data This is your data set for training and testing your model
#' @param TrainOnFull Set to TRUE to train on full data
#' @param ValidationData This is your holdout data set used in modeling either refine your hyperparameters.
#' @param TestData This is your holdout data set. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TargetColumnName Either supply the target column name OR the column number where the target is located (but not mixed types).
#' @param FeatureColNames Either supply the feature column names OR the column number where the target is located (but not mixed types)
#' @param WeightsColumn Column name of a weights column
#' @param model_path A character string of your path file to where you want your output saved
#' @param ModelID A character string to name your model and output
#' @param metadata_path A character string of your path file to where you want your model evaluation output saved. If left NULL, all output will be saved to model_path.
#' @param NumOfParDepPlots Tell the function the number of partial dependence calibration plots you want to create. Calibration boxplots will only be created for numerical features (not dummy variables)
#' @param ReturnModelObjects Set to TRUE to output all modeling objects (E.g. plots and evaluation metrics)
#' @param SaveModelObjects Set to TRUE to return all modeling objects to your environment
#' @param SaveInfoToPDF Set to TRUE to save insights to PDF
#' @param IfSaveModel Set to "mojo" to save a mojo file, otherwise "standard" to save a regular H2O model object
#' @param H2OStartUp Defaults to TRUE which means H2O will be started inside the function
#' @param H2OShutdown Set to TRUE to shutdown H2O inside the function
#' @param MaxMem Set the maximum amount of memory you'd like to dedicate to the model run. E.g. "32G"
#' @param NThreads Set to the mamimum amount of threads you want to use for this function
#' @param GridTune Set to TRUE to run a grid tuning procedure. Set a number in MaxModelsInGrid to tell the procedure how many models you want to test.
#' @param MaxModelsInGrid Number of models to test from grid options (1080 total possible options)
#' @param Distribution Choose from "multinomial". Placeholder in more options get added
#' @param eval_metric This is the metric used to identify best grid tuned model. Choose from "auc", "logloss"
#' @param GridStrategy Default "Cartesian"
#' @param StoppingRounds Number of runs
#' @param MaxRunTimeSecs Default 60*60*24
#' @param Trees The maximum number of trees you want in your models
#' @param MaxDepth Default 20
#' @param LearnRate Default 0.10
#' @param LearnRateAnnealing Default 1
#' @param SampleRate Default 0.632
#' @param ColSampleRate Default 1
#' @param ColSampleRatePerTree Default 1
#' @param ColSampleRatePerTreeLevel Default 1
#' @param MinRows Default 1
#' @param NBins Default 20
#' @param NBinsCats Default 1024
#' @param NBinsTopLevel Default 1024
#' @param HistogramType Default "AUTO"
#' @param CategoricalEncoding Default "AUTO"
#' @param DebugMode Set to TRUE to print steps
#' @examples
#' \donttest{
#' # Create some dummy correlated data
#' data <- AutoQuant::FakeDataGenerator(
#'   Correlation = 0.85,
#'   N = 1000,
#'   ID = 2,
#'   ZIP = 0,
#'   AddDate = FALSE,
#'   Classification = FALSE,
#'   MultiClass = TRUE)
#'
#' # Run function
#' TestModel <- AutoQuant::AutoH2oGBMMultiClass(
#'   OutputSelection = c("EvalMetrics", "Score_TrainData"),
#'   data,
#'   TrainOnFull = FALSE,
#'   ValidationData = NULL,
#'   TestData = NULL,
#'   TargetColumnName = "Adrian",
#'   FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
#'   WeightsColumn = NULL,
#'   eval_metric = "logloss",
#'   MaxMem = {gc();paste0(as.character(floor(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE)) / 1000000)),"G")},
#'   NThreads = max(1, parallel::detectCores()-2),
#'   model_path = normalizePath("./"),
#'   metadata_path = file.path(normalizePath("./")),
#'   ModelID = "FirstModel",
#'   ReturnModelObjects = TRUE,
#'   SaveModelObjects = FALSE,
#'   IfSaveModel = "mojo",
#'   H2OShutdown = TRUE,
#'   H2OStartUp = TRUE,
#'   DebugMode = FALSE,
#'
#'   # Model args
#'   GridTune = FALSE,
#'   GridStrategy = "Cartesian",
#'   MaxRunTimeSecs = 60*60*24,
#'   StoppingRounds = 10,
#'   MaxModelsInGrid = 2,
#'   Trees = 50,
#'   LearnRate = 0.10,
#'   LearnRateAnnealing = 1,
#'   Distribution = "multinomial",
#'   MaxDepth = 20,
#'   SampleRate = 0.632,
#'   ColSampleRate = 1,
#'   ColSampleRatePerTree = 1,
#'   ColSampleRatePerTreeLevel  = 1,
#'   MinRows = 1,
#'   NBins = 20,
#'   NBinsCats = 1024,
#'   NBinsTopLevel = 1024,
#'   HistogramType = "AUTO",
#'   CategoricalEncoding = "AUTO")
#' }
#' @return Saves to file and returned in list: VariableImportance.csv, Model, ValidationData.csv, EvaluationMetrics.csv, GridCollect, and GridList
#' @export
AutoH2oGBMMultiClass <- function(OutputSelection = c("EvalMetrics","Score_TrainData"),
                                 data = NULL,
                                 TrainOnFull = FALSE,
                                 ValidationData = NULL,
                                 TestData = NULL,
                                 TargetColumnName = NULL,
                                 FeatureColNames = NULL,
                                 WeightsColumn = NULL,
                                 MaxMem = {gc();paste0(as.character(floor(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE)) / 1000000)),"G")},
                                 NThreads = max(1L, parallel::detectCores()-2L),
                                 model_path = NULL,
                                 metadata_path = NULL,
                                 ModelID = "FirstModel",
                                 NumOfParDepPlots = 3L,
                                 ReturnModelObjects = TRUE,
                                 SaveModelObjects = FALSE,
                                 IfSaveModel = "mojo",
                                 H2OShutdown = TRUE,
                                 H2OStartUp = TRUE,
                                 DebugMode = FALSE,
                                 GridTune = FALSE,
                                 GridStrategy = "Cartesian",
                                 MaxRunTimeSecs = 60*60*24,
                                 StoppingRounds = 10,
                                 MaxModelsInGrid = 2,
                                 eval_metric = "auc",
                                 Trees = 50L,
                                 LearnRate = 0.10,
                                 LearnRateAnnealing = 1,
                                 Distribution = "multinomial",
                                 MaxDepth = 20,
                                 SampleRate = 0.632,
                                 MTries = -1,
                                 ColSampleRate = 1,
                                 ColSampleRatePerTree = 1,
                                 ColSampleRatePerTreeLevel  = 1,
                                 MinRows = 1,
                                 NBins = 20,
                                 NBinsCats = 1024,
                                 NBinsTopLevel = 1024,
                                 HistogramType = "AUTO",
                                 CategoricalEncoding = "AUTO") {

  # Args check ----
  Decreasing <- H2OArgsCheck(ModelType="gbm", TargetType = "multiclass", model_path.=model_path, metadata_path.=metadata_path, eval_metric.=eval_metric, MaxModelsInGrid.=MaxModelsInGrid, ModelID.=ModelID, NumOfParDepPlots.=0, ReturnModelObjects.=ReturnModelObjects, SaveModelObjects.=SaveModelObjects, GridTune.=GridTune, GridStrategy.=GridStrategy, CostMatrixWeights.=NULL, IfSaveModel.=IfSaveModel, Trees.=Trees, MaxDepth.=MaxDepth, SampleRate.=SampleRate, MTries.=MTries, ColSampleRatePerTree.=ColSampleRatePerTree, ColSampleRatePerTreeLevel.=ColSampleRatePerTreeLevel, MinRows.=MinRows, NBins.=NBins, NBinsCats.=NBinsCats, NBinsTopLevel.=NBinsTopLevel, HistogramType.=HistogramType, CategoricalEncoding.=CategoricalEncoding)

  # Grab all official parameters and their evaluated arguments
  ArgsList <- c(as.list(environment()))
  ArgsList[['data']] <- NULL
  ArgsList[['ValidationData']] <- NULL
  ArgsList[['TestData']] <- NULL
  ArgsList[['Algo']] <- "H2OGBM"
  ArgsList[['TargetType']] <- "MultiClass"
  ArgsList[['PredictionColumnName']] <- "Predict"
  if(SaveModelObjects) {
    if(!is.null(metadata_path)) {
      save(ArgsList, file = file.path(metadata_path, paste0(ModelID, "_ArgsList.Rdata")))
    }
  }

  # Data Prepare ----
  if(DebugMode) print("Data Prepare ----")
  Output <- H2ODataPrep(TargetType.="multiclass", TargetColumnName.=TargetColumnName, data.=data, ValidationData.=ValidationData, TestData.=TestData, TrainOnFull.=TrainOnFull, FeatureColNames.=FeatureColNames, SaveModelObjects.=SaveModelObjects, model_path.=metadata_path, ModelID.=ModelID)
  TargetColumnName <- Output$TargetColumnName; Output$TargetColumnName <- NULL
  TargetLevels <- Output$TargetLevels; Output$TargetLevels <- NULL
  dataTrain <- Output$dataTrain; Output$dataTrain <- NULL
  dataTest <- Output$dataTest; Output$dataTest <- NULL
  TestData <- Output$TestData; Output$TestData <- NULL
  Names <- Output$Names; rm(Output)

  # Grid Tune Check ----
  if(GridTune & !TrainOnFull) {

    # Grid tune ----
    if(DebugMode) print("Grid tune ----")

    # Prepare data ----
    if(H2OStartUp) localHost <- h2o::h2o.init(nthreads = NThreads, max_mem_size = MaxMem, enable_assertions = FALSE)
    datatrain <- h2o::as.h2o(dataTrain)
    if(!TrainOnFull) datavalidate <- h2o::as.h2o(dataTest, use_datatable = TRUE) else datavalidate <- NULL
    if(!is.null(TestData)) datatest <- h2o::as.h2o(TestData, use_datatable = TRUE) else datatest <- NULL

    # Grid Tune Search Criteria ----
    search_criteria  <- list(
      strategy = GridStrategy,
      max_runtime_secs = MaxRunTimeSecs,
      max_models = MaxModelsInGrid,
      seed = 1234,
      stopping_rounds = StoppingRounds,
      stopping_metric = toupper(eval_metric),
      stopping_tolerance = 1e-3)

    # Binary Grid Parameters ----
    hyper_params <- list()
    hyper_params[["ntrees"]] <- Trees
    hyper_params[["max_depth"]] <- MaxDepth
    hyper_params[["learn_rate"]] <- LearnRate
    hyper_params[["learn_rate_annealing"]] <- LearnRateAnnealing
    hyper_params[["sample_rate"]] <- SampleRate
    hyper_params[["col_sample_rate"]] <- ColSampleRate
    hyper_params[["col_sample_rate_per_tree"]] <- ColSampleRatePerTree
    hyper_params[["col_sample_rate_change_per_level"]] <- ColSampleRatePerTreeLevel
    hyper_params[["min_rows"]] <- MinRows
    hyper_params[["nbins"]] <- NBins
    hyper_params[["nbins_cats"]] <- NBinsCats
    hyper_params[["histogram_type"]] <- HistogramType
    hyper_params[["nbins_top_level"]] <- NBinsTopLevel
    hyper_params[["categorical_encoding"]] <- CategoricalEncoding
    hyper_params[["distribution"]] <- Distribution

    # Grid Train Model ----
    grid <- h2o::h2o.grid(
      hyper_params = hyper_params,
      search_criteria = search_criteria,
      is_supervised = TRUE,
      algorithm = "gbm",
      grid_id = paste0(ModelID, "_Grid"),
      x = FeatureColNames,
      y = TargetColumnName,
      training_frame = datatrain,
      validation_frame = datavalidate,
      max_runtime_secs = 3600 * 24 * 7,
      stopping_rounds = 10L,
      stopping_tolerance = 1e-3,
      stopping_metric = eval_metric,
      score_tree_interval = 10L,
      seed = 1234)

    # Get Best Model ----
    Grid_Out <- h2o::h2o.getGrid(grid_id = paste0(ModelID, "_Grid"), sort_by = eval_metric, decreasing = Decreasing)

    # Collect Best Grid Model ----
    base_model <- h2o::h2o.getModel(Grid_Out@model_ids[[1L]])
  }

  # Start Up H2O ----
  if(!GridTune) {

    # Build Model ----
    if(DebugMode) print("Build Model ----")

    # Load data
    if(H2OStartUp) localHost <- h2o::h2o.init(nthreads = NThreads, max_mem_size = MaxMem, enable_assertions = FALSE)
    datatrain <- h2o::as.h2o(dataTrain, use_datatable = TRUE)
    if(!TrainOnFull) datavalidate <- h2o::as.h2o(dataTest, use_datatable = TRUE) else datavalidate <- NULL
    if(!is.null(TestData)) datatest <- h2o::as.h2o(TestData, use_datatable = TRUE) else datatest <- NULL

    # Define args ----
    H2OArgs <- list()
    H2OArgs[["x"]] <- FeatureColNames
    H2OArgs[["y"]] <- TargetColumnName
    H2OArgs[["weights_column"]] <- WeightsColumn[1L]
    H2OArgs[["training_frame"]] <- datatrain
    H2OArgs[["validation_frame"]] <- datavalidate
    H2OArgs[["distribution"]] <- Distribution[1L]
    H2OArgs[["model_id"]] <- ModelID[1L]
    H2OArgs[["ntrees"]] <- Trees[1L]
    H2OArgs[["max_depth"]] <- MaxDepth[1L]
    H2OArgs[["learn_rate"]] <- LearnRate[1L]
    H2OArgs[["learn_rate_annealing"]] <- LearnRateAnnealing[1L]
    H2OArgs[["sample_rate"]] <- SampleRate[1L]
    H2OArgs[["col_sample_rate"]] <- ColSampleRate[1L]
    H2OArgs[["col_sample_rate_per_tree"]] <- ColSampleRatePerTree[1L]
    H2OArgs[["col_sample_rate_change_per_level"]] <- ColSampleRatePerTreeLevel[1L]
    H2OArgs[["min_rows"]] <- MinRows[1L]
    H2OArgs[["nbins"]] <- NBins[1L]
    H2OArgs[["nbins_cats"]] <- NBinsCats[1L]
    H2OArgs[["nbins_top_level"]] <- NBinsTopLevel[1L]
    H2OArgs[["histogram_type"]] <- HistogramType[1L]
    H2OArgs[["categorical_encoding"]] <- CategoricalEncoding[1L]

    # Build model ----
    base_model <- do.call(h2o::h2o.gbm, H2OArgs)
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
  if(DebugMode) print("Create Train Validation Data ----")
  if("score_traindata" %chin% tolower(OutputSelection) && !TrainOnFull) {
    Output <- H2OValidationData(Predict.=Predict, TestData.=NULL, dataTest.=NULL, dataTrain.=dataTrain, TrainOnFull.=TRUE, SaveModelObjects.=SaveModelObjects, metadata_path.=metadata_path, model_path.=metadata_path, ModelID.=ModelID, TransformNumericColumns.=NULL, TransformationResults.=NULL, TargetColumnName.=NULL, data.=NULL)
    TrainData <- Output$ValidationData; rm(Output)
  }

  # Score Validation Data ----
  Predict <- data.table::as.data.table(h2o::h2o.predict(object = base_model, newdata = if(!is.null(TestData)) datatest else if(!TrainOnFull) datavalidate else datatrain))

  # Create Validation Data ----
  if(DebugMode) print("Create Validation Data ----")
  Output <- H2OValidationData(Predict.=Predict, TestData.=TestData, dataTest.=dataTest, dataTrain.=dataTrain, TrainOnFull.=TrainOnFull, SaveModelObjects.=SaveModelObjects, metadata_path.=metadata_path, model_path.=metadata_path, ModelID.=ModelID, TransformNumericColumns.=NULL, TransformationResults.=NULL, TargetColumnName.=NULL, data.=NULL)
  ValidationData <- Output$ValidationData; rm(Output)

  # Variable Importance ----
  if(DebugMode) print("Variable Importance ----")
  VariableImportance <- H2OVariableImportance(TrainOnFull.=TrainOnFull, base_model.=base_model, SaveModelObjects.=SaveModelObjects, metadata_path.=metadata_path, model_path.=metadata_path, ModelID.=ModelID)

  # H2O Explain TrainData ----
  if(DebugMode) print("H2O Explain TrainData ----")
  ExplainList <- list()
  if("score_traindata" %chin% tolower(OutputSelection) && !TrainOnFull && 'h2o.explain' %in% OutputSelection) {
    ExplainList[["Train_Explain"]] <- h2o::h2o.explain(base_model, newdata = datatrain)
  }

  # H2O Explain ValidationData ----
  if(DebugMode) print("H2O Explain ValidationData ----")
  if(!TrainOnFull && 'h2o.explain' %in% OutputSelection) {
    ExplainList[["Test_Explain"]] <- h2o::h2o.explain(base_model, newdata = if(!is.null(TestData)) datatest else if(!is.null(ValidationData) && !TrainOnFull) datavalidate else datatrain)
  }

  # H2O Shutdown ----
  if(H2OShutdown) h2o::h2o.shutdown(prompt = FALSE)

  # Generate EvaluationMetrics ----
  if(DebugMode) print("Running MultiClassMetrics()")
  MultinomialMetrics <- list()
  MultinomialMetrics[["TestData"]] <- MultiClassMetrics(ModelClass="h2o", DataType = 'Test', SaveModelObjects.=SaveModelObjects, ValidationData.=ValidationData, PredictData.=predict, TrainOnFull.=TrainOnFull, TargetColumnName.=TargetColumnName, TargetLevels.=TargetLevels, ModelID.=ModelID, model_path.=metadata_path, metadata_path.=metadata_path, Debug = DebugMode)
  if("score_traindata" %chin% tolower(OutputSelection) && !TrainOnFull) {
    MultinomialMetrics[["TrainData"]] <- MultiClassMetrics(ModelClass="h2o", DataType = 'Train', SaveModelObjects.=SaveModelObjects, ValidationData.=TrainData, PredictData.=predict, TrainOnFull.=TrainOnFull, TargetColumnName.=TargetColumnName, TargetLevels.=TargetLevels, ModelID.=ModelID, model_path.=metadata_path, metadata_path.=metadata_path, Debug = DebugMode)
  }

  # Generate EvaluationMetrics ----
  tryCatch({
    if(DebugMode) print("Running BinaryMetrics()")
    EvalMetricsList <- list()
    EvalMetrics2List <- list()
    if("evalmetrics" %chin% tolower(OutputSelection)) {
      if("score_traindata" %chin% tolower(OutputSelection) && !TrainOnFull) {
        for(tarlevel in TargetLevels) {
          TrainData[, p1 := get(tarlevel)]
          TrainData[, paste0("Temp_",tarlevel) := data.table::fifelse(get(TargetColumnName) == eval(tarlevel), 1, 0)]
          EvalMetricsList[[paste0("TrainData_",tarlevel)]] <- BinaryMetrics(ClassWeights.=c(1,1), CostMatrixWeights.=c(1,0,0,1), SaveModelObjects.=FALSE, ValidationData.=TrainData, TrainOnFull.=TrainOnFull, TargetColumnName.=paste0("Temp_",tarlevel), ModelID.=ModelID, model_path.=metadata_path, metadata_path.=metadata_path, Method = "threshold")
          EvalMetrics2List[[paste0("TrainData_",tarlevel)]] <- BinaryMetrics(ClassWeights.=c(1,1), CostMatrixWeights.=c(1,0,0,1), SaveModelObjects.=FALSE, ValidationData.=TrainData, TrainOnFull.=TrainOnFull, TargetColumnName.=paste0("Temp_",tarlevel), ModelID.=ModelID, model_path.=metadata_path, metadata_path.=metadata_path, Method = "bins")
          data.table::set(TrainData, j = c("p1",paste0("Temp_",tarlevel)), value = NULL)
        }
      }
      for(tarlevel in TargetLevels) {
        ValidationData[, p1 := get(tarlevel)]
        ValidationData[, paste0("Temp_",tarlevel) := data.table::fifelse(get(TargetColumnName) == eval(tarlevel), 1, 0)]
        EvalMetricsList[[paste0("TestData_",tarlevel)]] <- BinaryMetrics(ClassWeights.=c(1,1), CostMatrixWeights.=c(1,0,0,1), SaveModelObjects.=FALSE, ValidationData.=ValidationData, TrainOnFull.=TrainOnFull, TargetColumnName.=paste0("Temp_",tarlevel), ModelID.=ModelID, model_path.=metadata_path, metadata_path.=metadata_path, Method = "threshold")
        EvalMetrics2List[[paste0("TestData_",tarlevel)]] <- BinaryMetrics(ClassWeights.=c(1,1), CostMatrixWeights.=c(1,0,0,1), SaveModelObjects.=FALSE, ValidationData.=ValidationData, TrainOnFull.=TrainOnFull, TargetColumnName.=paste0("Temp_",tarlevel), ModelID.=ModelID, model_path.=metadata_path, metadata_path.=metadata_path, Method = "bins")
        data.table::set(ValidationData, j = c("p1",paste0("Temp_",tarlevel)), value = NULL)
      }
      if(SaveModelObjects) {
        if(!is.null(metadata_path)) {
          save(EvalMetricsList, file = file.path(metadata_path, paste0(ModelID, "_EvaluationMetrics.Rdata")))
        }
      }
    }
  }, error = function(x) print("skipping BinaryMetrics()"))

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
    outputList[["ArgsList"]] <- ArgsList
    outputList[["ColNames"]] <- if(exists("Names")) Names else NULL
    outputList[["MultinomialMetrics"]] <- if(exists("MultinomialMetrics")) MultinomialMetrics else NULL
    return(outputList)
  }
}
