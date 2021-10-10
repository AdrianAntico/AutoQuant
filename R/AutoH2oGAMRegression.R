#' @title AutoH2oGAMRegression
#'
#' @description AutoH2oGAMRegression is an automated H2O modeling framework with grid-tuning and model evaluation that runs a variety of steps. First, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation plot, evaluation boxplot, evaluation metrics, variable importance, partial dependence calibration plots, partial dependence calibration box plots, and column names used in model fitting.
#'
#' @author Adrian Antico
#' @family Automated Supervised Learning - Regression
#'
#' @param OutputSelection You can select what type of output you want returned. Choose from c("EvalMetrics", "PDFs", "Score_TrainData")
#' @param data This is your data set for training and testing your model
#' @param TrainOnFull Set to TRUE to train on full data
#' @param ValidationData This is your holdout data set used in modeling either refine your hyperparameters.
#' @param TestData This is your holdout data set. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TargetColumnName Either supply the target column name OR the column number where the target is located (but not mixed types).
#' @param FeatureColNames Either supply the feature column names OR the column number where the target is located (but not mixed types)
#' @param GamColNames GAM column names. Up to 9 features
#' @param InteractionColNumbers Column numbers of the features you want to be pairwise interacted
#' @param WeightsColumn Column name of a weights column
#' @param Distribution : "AUTO", "gaussian", "binomial", "quasi-binomial", "ordinal", "multinomial", "poisson", "gamma", "tweedie", "negative-binomial", "fractionalbinomial"
#' @param Link "family_default", "identity", "logit", "log", "inverse", "tweedie","ologit"
#' @param TweedieLinkPower See h2o docs for background
#' @param TweedieVariancePower See h2o docs for background
#' @param TransformNumericColumns Set to NULL to do nothing; otherwise supply the column names of numeric variables you want transformed
#' @param Methods Choose from "BoxCox", "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", or "Logit". If more than one is selected, the one with the best normalization pearson statistic will be used. Identity is automatically selected and compared.
#' @param eval_metric This is the metric used to identify best grid tuned model. Choose from "MSE", "RMSE", "MAE", "RMSLE"
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
#' @param num_knots Numeric values for gam
#' @param keep_gam_cols Logical
#' @param Solver Default "AUTO". Options include "IRLSM", "L_BFGS", "COORDINATE_DESCENT_NAIVE", "COORDINATE_DESCENT", "GRADIENT_DESCENT_LH", "GRADIENT_DESCENT_SQERR"
#' @param Alpha Gridable. Default 0.5 Otherwise supply a value between 0 and 1. 1 is equivalent to Lasso regression. 0 is equivalent to Ridge regression. Inbetween for a blend of the two.
#' @param Lambda Gridable. Default NULL. Regularization strength.
#' @param LambdaSearch Default FALSE.
#' @param NLambdas Default -1
#' @param Standardize Default TRUE. Standardize numerical columns
#' @param RemoveCollinearColumns Default FALSE. Removes some of the linearly dependent columns
#' @param InterceptInclude Default TRUE
#' @param NonNegativeCoefficients Default FALSE
#' @param DebugMode Set to TRUE to get a printout of steps taken
#' @examples
#' \donttest{
#'
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
#' # Define GAM Columns to use - up to 9 are allowed
#' GamCols <- names(which(unlist(lapply(data, is.numeric))))
#' GamCols <- GamCols[!GamCols %in% c("Adrian","IDcol_1","IDcol_2")]
#' GamCols <- GamCols[1L:(min(9L,length(GamCols)))]
#'
#' # Run function
#' TestModel <- RemixAutoML::AutoH2oGAMRegression(
#'
#'  # Compute management
#'  MaxMem = {gc();paste0(as.character(floor(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE)) / 1000000)),"G")},
#'  NThreads = max(1, parallel::detectCores()-2),
#'  H2OShutdown = TRUE,
#'  H2OStartUp = TRUE,
#'  IfSaveModel = "mojo",
#'
#'  # Model evaluation
#'  eval_metric = "RMSE",
#'  NumOfParDepPlots = 3,
#'
#'  # Metadata arguments
#'  OutputSelection = c("EvalMetrics", "PDFs", "Score_TrainData"),
#'  model_path = NULL,
#'  metadata_path = NULL,
#'  ModelID = "FirstModel",
#'  ReturnModelObjects = TRUE,
#'  SaveModelObjects = FALSE,
#'  SaveInfoToPDF = FALSE,
#'
#'  # Data arguments:
#'  data = data,
#'  TrainOnFull = FALSE,
#'  ValidationData = NULL,
#'  TestData = NULL,
#'  TargetColumnName = "Adrian",
#'  FeatureColNames = names(data)[!names(data) %in%
#'                                  c("IDcol_1", "IDcol_2","Adrian")],
#'  InteractionColNumbers = NULL,
#'  WeightsColumn = NULL,
#'  GamColNames = GamCols,
#'  TransformNumericColumns = NULL,
#'  Methods = c("BoxCox", "Asinh", "Asin", "Log",
#'              "LogPlus1", "Sqrt", "Logit"),
#'
#'  # Model args
#'  num_knots = NULL,
#'  keep_gam_cols = TRUE,
#'  GridTune = FALSE,
#'  GridStrategy = "Cartesian",
#'  StoppingRounds = 10,
#'  MaxRunTimeSecs = 3600 * 24 * 7,
#'  MaxModelsInGrid = 10,
#'  Distribution = "gaussian",
#'  Link = "Family_Default",
#'  TweedieLinkPower = NULL,
#'  TweedieVariancePower = NULL,
#'  Solver = "AUTO",
#'  Alpha = 0.5,
#'  Lambda = NULL,
#'  LambdaSearch = FALSE,
#'  NLambdas = -1,
#'  Standardize = TRUE,
#'  RemoveCollinearColumns = FALSE,
#'  InterceptInclude = TRUE,
#'  NonNegativeCoefficients = FALSE,
#'  DebugMode = FALSE)
#' }
#' @return Saves to file and returned in list: VariableImportance.csv, Model, ValidationData.csv, EvalutionPlot.png, EvalutionBoxPlot.png, EvaluationMetrics.csv, ParDepPlots.R a named list of features with partial dependence calibration plots, ParDepBoxPlots.R, GridCollect, GridList, and Transformation metadata
#' @export
AutoH2oGAMRegression <- function(OutputSelection = c("EvalMetrics", "PDFs", "Score_TrainData"),
                                 data = NULL,
                                 TrainOnFull = FALSE,
                                 ValidationData = NULL,
                                 TestData = NULL,
                                 TargetColumnName = NULL,
                                 FeatureColNames = NULL,
                                 InteractionColNumbers = NULL,
                                 WeightsColumn = NULL,
                                 GamColNames = NULL,
                                 Distribution = "gaussian",
                                 Link = "identity",
                                 TweedieLinkPower = NULL,
                                 TweedieVariancePower = NULL,
                                 TransformNumericColumns = NULL,
                                 Methods = c("BoxCox", "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit"),
                                 eval_metric = "RMSE",
                                 MaxMem = {gc();paste0(as.character(floor(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE)) / 1000000)),"G")},
                                 NThreads = max(1, parallel::detectCores()-2),
                                 model_path = NULL,
                                 metadata_path = NULL,
                                 ModelID = "FirstModel",
                                 NumOfParDepPlots = 3,
                                 ReturnModelObjects = TRUE,
                                 SaveModelObjects = FALSE,
                                 SaveInfoToPDF = FALSE,
                                 IfSaveModel = "mojo",
                                 H2OShutdown = TRUE,
                                 H2OStartUp = TRUE,
                                 GridTune = FALSE,
                                 GridStrategy = "Cartesian",
                                 StoppingRounds = 10,
                                 MaxRunTimeSecs = 3600 * 24 * 7,
                                 MaxModelsInGrid = 2,
                                 num_knots = NULL,
                                 keep_gam_cols = TRUE,
                                 Solver = "AUTO",
                                 Alpha = 0.5,
                                 Lambda = NULL,
                                 LambdaSearch = FALSE,
                                 NLambdas = -1,
                                 Standardize = TRUE,
                                 RemoveCollinearColumns = FALSE,
                                 InterceptInclude = TRUE,
                                 NonNegativeCoefficients = FALSE,
                                 DebugMode = FALSE) {

  # Check Arguments ----
  if(!(tolower(eval_metric) %chin% c("mse", "rmse", "mae", "rmsle"))) stop("eval_metric not in MSE, RMSE, MAE, RMSLE")
  if(!GridTune %in% c(TRUE, FALSE)) stop("GridTune needs to be TRUE or FALSE")
  if(MaxModelsInGrid < 1 && GridTune) stop("MaxModelsInGrid needs to be at least 1")
  if(!is.null(model_path)) if(!is.character(model_path)) stop("model_path needs to be a character type")
  if(!is.null(metadata_path)) if(!is.character(metadata_path)) stop("metadata_path needs to be a character type")
  if(!is.character(ModelID) && !is.null(ModelID)) stop("ModelID needs to be a character type")
  if(NumOfParDepPlots < 0) stop("NumOfParDepPlots needs to be a positive number")
  if(!(ReturnModelObjects %in% c(TRUE, FALSE))) stop("ReturnModelObjects needs to be TRUE or FALSE")
  if(!(SaveModelObjects %in% c(TRUE, FALSE))) stop("SaveModelObjects needs to be TRUE or FALSE")

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

  # Data Prepare ----
  if(DebugMode) print("Data Prepare ----")
  Output <- H2ODataPrep(TargetType.="regression", TargetColumnName.=TargetColumnName, data.=data, ValidationData.=ValidationData, TestData.=TestData, TrainOnFull.=TrainOnFull, FeatureColNames.=FeatureColNames, SaveModelObjects.=SaveModelObjects, model_path.=model_path, ModelID.=ModelID, TransformNumericColumns.=TransformNumericColumns, Methods.=Methods)
  TransformationResults <- Output$TransformationResults; Output$TransformationResults <- NULL
  dataTrain <- Output$dataTrain; Output$dataTrain <- NULL
  dataTest <- Output$dataTest; Output$dataTest <- NULL
  TestData <- Output$TestData; Output$TestData <- NULL
  MinVal <- Output$MinVal; Output$MinVal <- NULL
  Names <- Output$Names; rm(Output)

  # Grid Tune Check ----
  if(GridTune && !TrainOnFull) {

    # Grid tune ----
    if(DebugMode) print("Grid tune ----")

    # H2O init and data load
    if(H2OStartUp) localHost <- h2o::h2o.init(nthreads = NThreads, max_mem_size = MaxMem, enable_assertions = FALSE)
    datatrain    <- h2o::as.h2o(dataTrain)
    if(!TrainOnFull) datavalidate <- h2o::as.h2o(dataTest, use_datatable = TRUE) else datavalidate <- NULL
    if(!is.null(TestData)) datatest <- h2o::as.h2o(TestData, use_datatable = TRUE) else datatest <- NULL

    # Grid Tune Search Criteria ----
    search_criteria  <- list(
      strategy = GridStrategy,
      max_runtime_secs = MaxRunTimeSecs,
      max_models = MaxModelsInGrid,
      seed = 1234,
      stopping_rounds = StoppingRounds,
      stopping_metric = toupper(eval_metric))

    # Hyperparameters ----
    hyper_params <- list()
    hyper_params[["alpha"]] <- Alpha
    hyper_params[["lambda"]] <- Lambda
    hyper_params[["tweedie_link_power"]] <- TweedieLinkPower
    hyper_params[["tweedie_variance_power"]] <- TweedieVariancePower

    # Link ----
    if(is.null(Link)) Link <- "identity"

    # Grid Train Model ----
    grid <- h2o::h2o.grid(
      hyper_params = hyper_params,
      search_criteria = search_criteria,
      is_supervised = TRUE,
      algorithm = "gam",
      grid_id = paste0(ModelID, "_Grid"),
      x = FeatureColNames,
      gam_columns = GamColNames[1L:(min(length(GamColNames),9L))],
      y = TargetColumnName,
      training_frame = datatrain,
      validation_frame = datavalidate,
      family = Distribution,
      link = Link,
      seed = 1234)

    # Get Best Model ----
    Grid_Out <- h2o::h2o.getGrid(grid_id = paste0(ModelID, "_Grid"), sort_by = eval_metric, decreasing = FALSE)

    # Collect Best Grid Model ----
    base_model <- h2o::h2o.getModel(Grid_Out@model_ids[[1L]])
  }

  # Build model ----
  if(!GridTune) {

    # Build Model ----
    if(DebugMode) print("Build Model ----")

    # Load data
    if(H2OStartUp) localHost <- h2o::h2o.init(nthreads = NThreads, max_mem_size = MaxMem, enable_assertions = FALSE)
    datatrain <- h2o::as.h2o(dataTrain, use_datatable = TRUE)
    if(!TrainOnFull) datavalidate <- h2o::as.h2o(dataTest, use_datatable = TRUE) else datavalidate <- NULL
    if(!is.null(TestData)) datatest <- h2o::as.h2o(TestData, use_datatable = TRUE) else datatest <- NULL

    # Define link ----
    if(!GridTune) if(is.null(Link)) Link <- "family_default"

    # Define args ----
    H2OArgs <- list()
    H2OArgs[["x"]] <- FeatureColNames
    H2OArgs[["y"]] <- TargetColumnName
    H2OArgs[["gam_columns"]] <- GamColNames[1L:(min(length(GamColNames),9L))]
    H2OArgs[["weights_column"]] <- WeightsColumn
    H2OArgs[["training_frame"]] <- datatrain
    H2OArgs[["validation_frame"]] <- datavalidate
    H2OArgs[["family"]] <- Distribution
    H2OArgs[["link"]] <- Link
    H2OArgs[["model_id"]] <- ModelID
    H2OArgs[["solver"]] <- Solver
    H2OArgs[["alpha"]] <- Alpha
    H2OArgs[["lambda"]] <- Lambda
    H2OArgs[["lambda_search"]] <- LambdaSearch
    H2OArgs[["nlambdas"]] <- NLambdas
    H2OArgs[["standardize"]] <- Standardize
    H2OArgs[["remove_collinear_columns"]] <- RemoveCollinearColumns
    H2OArgs[["intercept"]] <- InterceptInclude
    H2OArgs[["non_negative"]] <- NonNegativeCoefficients
    H2OArgs[["tweedie_link_power"]] <- TweedieLinkPower
    H2OArgs[["tweedie_variance_power"]] <- TweedieVariancePower

    # Build model ----
    base_model <- do.call(h2o::h2o.gam, H2OArgs)
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

  # Score Validation Data
  Predict <- data.table::as.data.table(h2o::h2o.predict(object = base_model, newdata = if(!is.null(TestData)) datatest else if(!TrainOnFull) datavalidate else datatrain))

  # Create Validation Data ----
  Output <- H2OValidationData(Predict.=Predict, TestData.=TestData, dataTest.=dataTest, dataTrain.=dataTrain, TrainOnFull.=TrainOnFull, SaveModelObjects.=SaveModelObjects, metadata_path.=metadata_path, model_path.=model_path, ModelID.=ModelID, TransformNumericColumns.=NULL, TransformationResults.=NULL, TargetColumnName.=NULL, data.=NULL)
  ValidationData <- Output$ValidationData; rm(Output)

  # Variable Importance ----
  if(DebugMode) print("Variable Importance ----")
  VariableImportance <- H2OVariableImportance(TrainOnFull.=TrainOnFull, base_model.=base_model, SaveModelObjects.=SaveModelObjects, metadata_path.=metadata_path, model_path.=model_path, ModelID.=ModelID)

  # H2O Explain TrainData ----
  # if(DebugMode) print("H2O Explain TrainData ----")
  # ExplainList <- list()
  # if("score_traindata" %chin% tolower(OutputSelection) && !TrainOnFull) {
  #   ExplainList[["Train_Explain"]] <- h2o::h2o.explain(base_model, newdata = datatrain)
  # }

  # H2O Explain ValidationData ----
  # if(DebugMode) print("H2O Explain ValidationData ----")
  # if(!TrainOnFull) {
  #   ExplainList[["Test_Explain"]] <- h2o::h2o.explain(base_model, newdata = if(!is.null(TestData)) datatest else if(!is.null(ValidationData) && !TrainOnFull) datavalidate else datatrain)
  # }

  # H2O Shutdown ----
  if(DebugMode) print("H2O Shutdown ----")
  if(H2OShutdown) h2o::h2o.shutdown(prompt = FALSE)

  # Generate EvaluationMetrics ----
  if(DebugMode) print("Generate EvaluationMetrics ----")
  EvalMetricsList <- list()
  if("evalmetrics" %chin% tolower(OutputSelection)) {
    if("score_traindata" %chin% tolower(OutputSelection) && !TrainOnFull) {
      EvalMetricsList[["TrainData"]] <- RegressionMetrics(SaveModelObjects.=SaveModelObjects, data.=data, ValidationData.=TrainData, TrainOnFull.=TrainOnFull, LossFunction.=eval_metric, EvalMetric.=eval_metric, TargetColumnName.=TargetColumnName, ModelID.=ModelID, model_path.=model_path, metadata_path.=metadata_path)
    }
    EvalMetricsList[["TestData"]] <- RegressionMetrics(SaveModelObjects.=SaveModelObjects, data.=data, ValidationData.=ValidationData, TrainOnFull.=TrainOnFull, LossFunction.=eval_metric, EvalMetric.=eval_metric, TargetColumnName.=TargetColumnName, ModelID.=ModelID, model_path.=model_path, metadata_path.=metadata_path)
  }

  # Subset Transformation Object ----
  if(!is.null(TransformNumericColumns)) {
    if(TargetColumnName == "Target") {
      TransformationResults <- TransformationResults[!(ColumnName %chin% c("Predict"))]
    } else {
      TransformationResults <- TransformationResults[!(ColumnName %chin% c("Predict", "Target"))]
    }
  }

  # Send output to pdf ----
  if(DebugMode) print("Running CatBoostPDF()")
  if("pdfs" %chin% tolower(OutputSelection) && SaveModelObjects) {
    CatBoostPDF(ModelClass = "h2o", ModelType="regression", TrainOnFull.=TrainOnFull, SaveInfoToPDF.=SaveInfoToPDF, PlotList.=NULL, VariableImportance.=VariableImportance, EvalMetricsList.=EvalMetricsList, Interaction.=NULL, model_path.=model_path, metadata_path.=metadata_path)
  }

  # Return Objects ----
  if(DebugMode) print("Return Objects ----")
  if(ReturnModelObjects) {
    return(list(
      Model = base_model,
      TrainData = if(exists("TrainData") && !is.null(TrainData)) TrainData else NULL,
      TestData = if(exists("ValidationData") && !is.null(ValidationData)) ValidationData else NULL,
      H2OExplain = if(exists("ExplainList")) ExplainList else NULL,
      EvaluationMetrics = if(exists("EvalMetricsList")) EvalMetricsList else NULL,
      VariableImportance = if(exists("VariableImportance")) VariableImportance else NULL,
      TransformationResults = if(exists("TransformationResults")) TransformationResults else NULL,
      ColNames = if(exists("Names")) Names else NULL))
  }
}
