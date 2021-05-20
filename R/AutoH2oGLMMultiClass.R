#' @title AutoH2oGLMMultiClass
#'
#' @description AutoH2oGLMMultiClass is an automated H2O modeling framework with grid-tuning and model evaluation that runs a variety of steps. First, a stratified sampling (by the target variable) is done to create train and validation sets. Then, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation metrics, confusion matrix, and variable importance.
#'
#' @author Adrian Antico
#' @family Automated Supervised Learning - Multiclass Classification
#'
#' @param OutputSelection You can select what type of output you want returned. Choose from c("EvalMetrics", "PDFs", "Score_TrainData")
#' @param data This is your data set for training and testing your model
#' @param TrainOnFull Set to TRUE to train on full data
#' @param ValidationData This is your holdout data set used in modeling either refine your hyperparameters.
#' @param TestData This is your holdout data set. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TargetColumnName Either supply the target column name OR the column number where the target is located (but not mixed types).
#' @param FeatureColNames Either supply the feature column names OR the column number where the target is located (but not mixed types)
#' @param RandomColNumbers Random effects column number indicies
#' @param InteractionColNumbers Column numbers of the features you want to be pairwise interacted
#' @param WeightsColumn Column name of a weights column
#' @param eval_metric This is the metric used to identify best grid tuned model. Choose from "logloss"
#' @param GridTune Set to TRUE to run a grid tuning procedure. Set a number in MaxModelsInGrid to tell the procedure how many models you want to test.
#' @param GridStrategy "RandomDiscrete" or "Cartesian"
#' @param MaxRunTimeSecs Max run time in seconds
#' @param StoppingRounds Iterations in grid tuning
#' @param MaxModelsInGrid Number of models to test from grid options (1080 total possible options)
#' @param MaxMem Set the maximum amount of memory you'd like to dedicate to the model run. E.g. "32G"
#' @param NThreads Set the number of threads you want to dedicate to the model building
#' @param model_path A character string of your path file to where you want your output saved
#' @param metadata_path A character string of your path file to where you want your model evaluation output saved. If left NULL, all output will be saved to model_path.
#' @param ModelID A character string to name your model and output
#' @param NumOfParDepPlots Tell the function the number of partial dependence calibration plots you want to create. Calibration boxplots will only be created for numerical features (not dummy variables)
#' @param ReturnModelObjects Set to TRUE to output all modeling objects (E.g. plots and evaluation metrics)
#' @param SaveModelObjects Set to TRUE to return all modeling objects to your environment
#' @param SaveInfoToPDF Set to TRUE to save insights to PDF
#' @param IfSaveModel Set to "mojo" to save a mojo file, otherwise "standard" to save a regular H2O model object
#' @param H2OStartUp Defaults to TRUE which means H2O will be started inside the function
#' @param H2OShutdown Set to TRUE to shutdown H2O inside the function
#' @param DebugMode Set to TRUE to see a printout of each step
#' @param Distribution "multinomial"
#' @param link "family_default"
#' @param RandomDistribution Random effects family. Defaults NULL, otherwise it will run a hierarchical glm
#' @param RandomLink Random effects link. Defaults NULL, otherwise it will run a hierarchical glm
#' @param Solver Default "AUTO". Options include "IRLSM", "L_BFGS", "COORDINATE_DESCENT_NAIVE", "COORDINATE_DESCENT", "GRADIENT_DESCENT_LH", "GRADIENT_DESCENT_SQERR"
#' @param Alpha Default 0.5 Otherwise supply a value between 0 and 1. 1 is equivalent to Lasso regression. 0 is equivalent to Ridge regression. Inbetween for a blend of the two.
#' @param Lambda Default NULL. Regularization strength.
#' @param LambdaSearch Default FALSE.
#' @param NLambdas Default -1
#' @param Standardize Default TRUE. Standardize numerical columns
#' @param RemoveCollinearColumns Default FALSE. Removes some of the linearly dependent columns
#' @param InterceptInclude Default TRUE
#' @param NonNegativeCoefficients Default FALSE
#' @examples
#' \donttest{
#' # Create some dummy correlated data with numeric and categorical features
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
#' TestModel <- RemixAutoML::AutoH2oGLMMultiClass(
#'
#'   # Compute management
#'   OutputSelection = c("EvalMetrics", "PDFs", "Score_TrainData"),
#'   MaxMem = {gc();paste0(as.character(floor(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE)) / 1000000)),"G")},
#'   NThreads = max(1, parallel::detectCores()-2),
#'   H2OShutdown = TRUE,
#'   H2OStartUp = TRUE,
#'   IfSaveModel = "mojo",
#'
#'   # Model evaluation:
#'   eval_metric = "logloss",
#'   NumOfParDepPlots = 3,
#'
#'   # Metadata arguments:
#'   model_path = NULL,
#'   metadata_path = NULL,
#'   ModelID = "FirstModel",
#'   ReturnModelObjects = TRUE,
#'   SaveModelObjects = FALSE,
#'   SaveInfoToPDF = FALSE,
#'   DebugMode = FALSE,
#'
#'   # Data arguments:
#'   data = data,
#'   TrainOnFull = FALSE,
#'   ValidationData = NULL,
#'   TestData = NULL,
#'   TargetColumnName = "Adrian",
#'   FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
#'   RandomColNumbers = NULL,
#'   InteractionColNumbers = NULL,
#'   WeightsColumn = NULL,
#'
#'   # Model args
#'   GridTune = FALSE,
#'   GridStrategy = "Cartesian",
#'   StoppingRounds = 10,
#'   MaxRunTimeSecs = 3600 * 24 * 7,
#'   MaxModelsInGrid = 10,
#'   Distribution = "multinomial",
#'   Link = "family_default",
#'   RandomDistribution = NULL,
#'   RandomLink = NULL,
#'   Solver = "AUTO",
#'   Alpha = 0.5,
#'   Lambda = NULL,
#'   LambdaSearch = FALSE,
#'   NLambdas = -1,
#'   Standardize = TRUE,
#'   RemoveCollinearColumns = FALSE,
#'   InterceptInclude = TRUE,
#'   NonNegativeCoefficients = FALSE)
#' }
#' @return Saves to file and returned in list: VariableImportance.csv, Model, ValidationData.csv, EvaluationMetrics.csv, GridCollect, and GridList
#' @export
AutoH2oGLMMultiClass <- function(OutputSelection = c("EvalMetrics", "PDFs", "Score_TrainData"),
                                 data = NULL,
                                 TrainOnFull = FALSE,
                                 ValidationData = NULL,
                                 TestData = NULL,
                                 TargetColumnName = NULL,
                                 FeatureColNames = NULL,
                                 RandomColNumbers = NULL,
                                 InteractionColNumbers = NULL,
                                 WeightsColumn = NULL,
                                 MaxMem = {gc();paste0(as.character(floor(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE)) / 1000000)),"G")},
                                 NThreads = max(1, parallel::detectCores()-2),
                                 ModelID = "FirstModel",
                                 ReturnModelObjects = TRUE,
                                 model_path = NULL,
                                 metadata_path = NULL,
                                 DebugMode = FALSE,
                                 SaveModelObjects = FALSE,
                                 SaveInfoToPDF = FALSE,
                                 IfSaveModel = "mojo",
                                 H2OShutdown = TRUE,
                                 H2OStartUp = TRUE,
                                 MaxModelsInGrid = 2,
                                 NumOfParDepPlots = 3,
                                 GridTune = FALSE,
                                 GridStrategy = "Cartesian",
                                 StoppingRounds = 10,
                                 MaxRunTimeSecs = 3600 * 24 * 7,
                                 Distribution = "multinomial",
                                 Link = "family_default",
                                 eval_metric = "logloss",
                                 RandomDistribution = NULL,
                                 RandomLink = NULL,
                                 Solver = "AUTO",
                                 Alpha = 0.5,
                                 Lambda = NULL,
                                 LambdaSearch = FALSE,
                                 NLambdas = -1,
                                 Standardize = TRUE,
                                 RemoveCollinearColumns = FALSE,
                                 InterceptInclude = TRUE,
                                 NonNegativeCoefficients = FALSE) {

  # Ensure model_path and metadata_path exists ----
  if(!is.null(model_path)) if(!dir.exists(file.path(model_path))) dir.create(model_path)
  if(!is.null(metadata_path)) if(!is.null(metadata_path)) if(!dir.exists(file.path(metadata_path))) dir.create(metadata_path)

  # Check Arguments ----
  if(!(tolower(eval_metric) %chin% c("auc", "logloss"))) stop("eval_metric not in AUC, logloss")
  if(!GridTune %in% c(TRUE, FALSE)) stop("GridTune needs to be TRUE or FALSE")
  if(MaxModelsInGrid < 1 && GridTune == TRUE) stop("MaxModelsInGrid needs to be at least 1")
  if(!is.null(model_path)) if(!is.character(model_path)) stop("model_path needs to be a character type")
  if(!is.null(metadata_path)) if(!is.character(metadata_path)) stop("metadata_path needs to be a character type")
  if(!is.character(ModelID) && !is.null(ModelID)) stop("ModelID needs to be a character type")
  if(!(ReturnModelObjects %in% c(TRUE, FALSE))) stop("ReturnModelObjects needs to be TRUE or FALSE")
  if(!(SaveModelObjects %in% c(TRUE, FALSE))) stop("SaveModelObjects needs to be TRUE or FALSE")
  if(eval_metric == "auc") Decreasing <- FALSE else Decreasing <- TRUE

  # Data Prepare ----
  if(DebugMode) print("Data Prepare ----")
  Output <- H2ODataPrep(TargetType.="multiclass", TargetColumnName.=TargetColumnName, data.=data, ValidationData.=ValidationData, TestData.=TestData, TrainOnFull.=TrainOnFull, FeatureColNames.=FeatureColNames, SaveModelObjects.=SaveModelObjects, model_path.=model_path, ModelID.=ModelID)
  TargetColumnName <- Output$TargetColumnName; Output$TargetColumnName <- NULL
  TargetLevels <- Output$TargetLevels; Output$TargetLevels <- NULL
  dataTrain <- Output$dataTrain; Output$dataTrain <- NULL
  dataTest <- Output$dataTest; Output$dataTest <- NULL
  TestData <- Output$TestData; Output$TestData <- NULL
  Names <- Output$Names; rm(Output)

  # Grid Tune Check ----
  if(GridTune && !TrainOnFull) {

    # Grid tune ----
    if(DebugMode) print("Grid tune ----")

    # Load data ----
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

    # Grid Parameters ----
    hyper_params <- list()
    hyper_params[["alpha"]] <- Alpha
    hyper_params[["lambda"]] <- Lambda

    # Grid Train Model ----
    grid <- h2o::h2o.grid(
      hyper_params = hyper_params,
      search_criteria = search_criteria,
      is_supervised = TRUE,
      algorithm = "glm",
      family = "multinomial",
      grid_id = paste0(ModelID, "_Grid"),
      x = FeatureColNames,
      y = TargetColumnName,
      training_frame = datatrain,
      validation_frame = datavalidate)

    # Get Best Model ----
    Grid_Out <- h2o::h2o.getGrid(
      grid_id = paste0(ModelID, "_Grid"),
      sort_by = eval_metric,
      decreasing = Decreasing)

    # Collect Best Grid Model ----
    base_model <- h2o::h2o.getModel(Grid_Out@model_ids[[1L]])
  }

  # Start Up H2O ----
  if(!GridTune) {

    # Build model ----
    if(DebugMode) print("Build model ----")

    # Load data ----
    if(H2OStartUp) localHost <- h2o::h2o.init(nthreads = NThreads, max_mem_size = MaxMem, enable_assertions = FALSE)
    datatrain <- h2o::as.h2o(dataTrain)
    if(!TrainOnFull) datavalidate <- h2o::as.h2o(dataTest, use_datatable = TRUE) else datavalidate <- NULL
    if(!is.null(TestData)) datatest <- h2o::as.h2o(TestData, use_datatable = TRUE) else datatest <- NULL

    # Define ml args ----
    H2OArgs <- list()
    H2OArgs[["x"]] <- FeatureColNames
    H2OArgs[["y"]] <- TargetColumnName
    H2OArgs[["interactions"]] <- InteractionColNumbers
    H2OArgs[["weights_column"]] <- WeightsColumn
    if(!is.null(RandomDistribution) & !is.null(RandomLink)) H2OArgs[["HGLM"]] <- TRUE else H2OArgs[["HGLM"]] <- FALSE
    H2OArgs[["training_frame"]] <- datatrain
    H2OArgs[["validation_frame"]] <- datavalidate
    H2OArgs[["family"]] <- Distribution
    H2OArgs[["link"]] <- Link
    H2OArgs[["model_id"]] <- ModelID
    H2OArgs[["rand_family"]] <- RandomDistribution
    H2OArgs[["rand_link"]] <- RandomLink
    H2OArgs[["random_columns"]] <- RandomColNumbers
    H2OArgs[["solver"]] <- Solver
    H2OArgs[["alpha"]] <- Alpha
    H2OArgs[["lambda"]] <- Lambda
    H2OArgs[["lambda_search"]] <- LambdaSearch
    H2OArgs[["nlambdas"]] <- NLambdas
    H2OArgs[["standardize"]] <- Standardize
    H2OArgs[["remove_collinear_columns"]] <- RemoveCollinearColumns
    H2OArgs[["intercept"]] <- InterceptInclude
    H2OArgs[["non_negative"]] <- NonNegativeCoefficients

    # Build model ----
    base_model <- do.call(what = h2o::h2o.glm, args = H2OArgs)
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
    Output <- H2OValidationData(Predict.=Predict, TestData.=NULL, dataTest.=NULL, dataTrain.=dataTrain, TrainOnFull.=TRUE, SaveModelObjects.=SaveModelObjects, metadata_path.=metadata_path, model_path.=model_path, ModelID.=ModelID, TransformNumericColumns.=NULL, TransformationResults.=NULL, TargetColumnName.=NULL, data.=NULL)
    TrainData <- Output$ValidationData; rm(Output)
  }

  # Score Validation Data ----
  Predict <- data.table::as.data.table(h2o::h2o.predict(object = base_model, newdata = if(!is.null(TestData)) datatest else if(!TrainOnFull) datavalidate else datatrain))

  # Create Validation Data ----
  if(DebugMode) print("Create Validation Data ----")
  Output <- H2OValidationData(Predict.=Predict, TestData.=TestData, dataTest.=dataTest, dataTrain.=dataTrain, TrainOnFull.=TrainOnFull, SaveModelObjects.=SaveModelObjects, metadata_path.=metadata_path, model_path.=model_path, ModelID.=ModelID, TransformNumericColumns.=NULL, TransformationResults.=NULL, TargetColumnName.=NULL, data.=NULL)
  ValidationData <- Output$ValidationData; rm(Output)

  # Variable Importance ----
  if(DebugMode) print("Variable Importance ----")
  VariableImportance <- H2OVariableImportance(TrainOnFull.=TrainOnFull, base_model.=base_model, SaveModelObjects.=SaveModelObjects, metadata_path.=metadata_path, model_path.=model_path, ModelID.=ModelID)

  # H2O Explain TrainData ----
  if(DebugMode) print("H2O Explain TrainData ----")
  ExplainList <- list()
  if("score_traindata" %chin% tolower(OutputSelection) && !TrainOnFull) {
    ExplainList[["Train_Explain"]] <- h2o::h2o.explain(base_model, newdata = datatrain)
  }

  # H2O Explain ValidationData ----
  if(DebugMode) print("H2O Explain ValidationData ----")
  if(!TrainOnFull) {
    ExplainList[["Test_Explain"]] <- h2o::h2o.explain(base_model, newdata = if(!is.null(TestData)) datatest else if(!is.null(ValidationData) && !TrainOnFull) datavalidate else datatrain)
  }

  # H2O Shutdown ----
  if(H2OShutdown) h2o::h2o.shutdown(prompt = FALSE)

  # Generate EvaluationMetrics ----
  if(DebugMode) print("Running BinaryMetrics()")
  EvalMetricsList <- list()
  if("evalmetrics" %chin% tolower(OutputSelection)) {
    if("score_traindata" %chin% tolower(OutputSelection) && !TrainOnFull) {
      EvalMetricsList[["TrainData"]] <- MultiClassMetrics(ModelClass="h2o", SaveModelObjects.=SaveModelObjects, ValidationData.=ValidationData, PredictData.=predict, TrainOnFull.=TrainOnFull, TargetColumnName.=TargetColumnName, TargetLevels.=TargetLevels, ModelID.=ModelID, model_path.=model_path, metadata_path.=metadata_path)
    }
    EvalMetricsList[["TestData"]] <- MultiClassMetrics(ModelClass="h2o", SaveModelObjects.=SaveModelObjects, ValidationData.=ValidationData, PredictData.=predict, TrainOnFull.=TrainOnFull, TargetColumnName.=TargetColumnName, TargetLevels.=TargetLevels, ModelID.=ModelID, model_path.=model_path, metadata_path.=metadata_path)
  }

  # Return Objects ----
  if(DebugMode) print("Return Objects ----")
  if(ReturnModelObjects) {
    return(list(
      Model = base_model,
      TrainData = if(exists("TrainData")) TrainData else NULL,
      TestData = if(exists("ValidationData") && !is.null(ValidationData)) ValidationData else NULL,
      H2OExplain = if(exists("ExplainList") && !is.null(ExplainList)) ExplainList else NULL,
      EvaluationMetrics = if(exists("EvalMetricsList") && !is.null(EvalMetricsList)) EvalMetricsList else NULL,
      VariableImportance = if(exists("VariableImportance") && !is.null(VariableImportance)) VariableImportance else NULL,
      ColNames = if(exists("Names") && !is.null(Names)) Names else NULL))
  }
}
