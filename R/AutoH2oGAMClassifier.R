#' @title AutoH2oGAMClassifier
#'
#' @description AutoH2oGAMClassifier is an automated H2O modeling framework with grid-tuning and model evaluation that runs a variety of steps. First, a stratified sampling (by the target variable) is done to create train and validation sets. Then, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation plot, evaluation metrics, variable importance, partial dependence calibration plots, and column names used in model fitting.
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
#' @param CostMatrixWeights A vector with 4 elements c(True Positive Cost, False Negative Cost, False Positive Cost, True Negative Cost). Default c(1,0,0,1),
#' @param WeightsColumn Weighted classification
#' @param GamColNames GAM column names. Up to 9 features
#' @param Distribution "binomial", "quasibinomial"
#' @param Link identity, logit, log, inverse, tweedie
#' @param eval_metric This is the metric used to identify best grid tuned model. Choose from "AUC" or "logloss"
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
#' @param NumOfParDepPlots Tell the function the number of partial dependence calibration plots you want to create.
#' @param ReturnModelObjects Set to TRUE to output all modeling objects (E.g. plots and evaluation metrics)
#' @param SaveModelObjects Set to TRUE to return all modeling objects to your environment
#' @param IfSaveModel Set to "mojo" to save a mojo file, otherwise "standard" to save a regular H2O model object
#' @param SaveInfoToPDF Set to TRUE to save modeling information to PDF. If model_path or metadata_path aren't defined then output will be saved to the working directory
#' @param DebugMode Set to TRUE to get a print out of steps taken internally
#' @param H2OShutdown Set to TRUE to shutdown H2O after running the function
#' @param H2OStartUp Set to TRUE to start up H2O inside function
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
#'   Classification = TRUE,
#'   MultiClass = FALSE)
#'
#' # Define GAM Columns to use - up to 9 are allowed
#' GamCols <- names(which(unlist(lapply(data, is.numeric))))
#' GamCols <- GamCols[!GamCols %in% c("Adrian","IDcol_1","IDcol_2")]
#' GamCols <- GamCols[1L:(min(9L,length(GamCols)))]
#'
#' # Run function
#' TestModel <- RemixAutoML::AutoH2oGAMClassifier(
#'
#'   # Compute management
#'   MaxMem = {gc();paste0(as.character(floor(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE)) / 1000000)),"G")},
#'   NThreads = max(1, parallel::detectCores()-2),
#'   H2OShutdown = TRUE,
#'   H2OStartUp = TRUE,
#'   IfSaveModel = "mojo",
#'
#'   # Model evaluation args
#'   CostMatrixWeights = c(1,0,0,1),
#'   eval_metric = "auc",
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
#'   # Data args
#'   data = data,
#'   TrainOnFull = FALSE,
#'   ValidationData = NULL,
#'   TestData = NULL,
#'   TargetColumnName = "Adrian",
#'   FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
#'   WeightsColumn = NULL,
#'   GamColNames = GamCols,
#'
#'   # ML args
#'   num_knots = NULL,
#'   keep_gam_cols = TRUE,
#'   GridTune = FALSE,
#'   GridStrategy = "Cartesian",
#'   StoppingRounds = 10,
#'   MaxRunTimeSecs = 3600 * 24 * 7,
#'   MaxModelsInGrid = 10,
#'   Distribution = "binomial",
#'   Link = "logit",
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
#' @return Saves to file and returned in list: VariableImportance.csv, Model, ValidationData.csv, EvalutionPlot.png, EvaluationMetrics.csv, ParDepPlots.R a named list of features with partial dependence calibration plots, GridCollect, and GridList
#' @export
AutoH2oGAMClassifier <- function(data,
                                 TrainOnFull = FALSE,
                                 ValidationData = NULL,
                                 TestData = NULL,
                                 TargetColumnName = NULL,
                                 FeatureColNames = NULL,
                                 WeightsColumn = NULL,
                                 GamColNames = NULL,
                                 Distribution = "binomial",
                                 Link = "logit",
                                 eval_metric = "auc",
                                 CostMatrixWeights = c(1,0,0,1),
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
                                 H2OShutdown = FALSE,
                                 H2OStartUp = TRUE,
                                 DebugMode = FALSE,
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
                                 NonNegativeCoefficients = FALSE) {

  # Ensure model_path and metadata_path exists ----
  if(!is.null(model_path)) if(!dir.exists(file.path(model_path))) dir.create(model_path)
  if(!is.null(metadata_path)) if(!is.null(metadata_path)) if(!dir.exists(file.path(metadata_path))) dir.create(metadata_path)

  # Check Arguments ----
  if(!(tolower(eval_metric) %chin% c("auc", "logloss"))) stop("eval_metric not in AUC, logloss")
  if(!GridTune %in% c(TRUE, FALSE)) stop("GridTune needs to be TRUE or FALSE")
  if(MaxModelsInGrid < 1 && GridTune) stop("MaxModelsInGrid needs to be at least 1")
  if(!is.null(model_path)) if(!is.character(model_path)) stop("model_path needs to be a character type")
  if(!is.null(metadata_path)) if(!is.character(metadata_path)) stop("metadata_path needs to be a character type")
  if(!is.character(ModelID) && !is.null(ModelID)) stop("ModelID needs to be a character type")
  if(NumOfParDepPlots < 0) stop("NumOfParDepPlots needs to be a positive number")
  if(!(ReturnModelObjects %in% c(TRUE, FALSE))) stop("ReturnModelObjects needs to be TRUE or FALSE")
  if(!(SaveModelObjects %in% c(TRUE, FALSE))) stop("SaveModelObjects needs to be TRUE or FALSE")
  if(!(tolower(eval_metric) == "auc")) eval_metric <- tolower(eval_metric) else eval_metric <- toupper(eval_metric)
  if(tolower(eval_metric) %chin% c("auc")) Decreasing <- TRUE else Decreasing <- FALSE

  # Data Prepare ----
  if(DebugMode) print("Data Prepare ----")
  Output <- H2ODataPrep(TargetType.="classifier", TargetColumnName.=TargetColumnName, data.=data, ValidationData.=ValidationData, TestData.=TestData, TrainOnFull.=TrainOnFull, FeatureColNames.=FeatureColNames, SaveModelObjects.=SaveModelObjects, model_path.=model_path, ModelID.=ModelID)
  TargetColumnName <- Output$TargetColumnName; Output$TargetColumnName <- NULL
  dataTrain <- Output$dataTrain; Output$dataTrain <- NULL
  dataTest <- Output$dataTest; Output$dataTest <- NULL
  TestData <- Output$TestData; Output$TestData <- NULL
  Names <- Output$Names; rm(Output)

  # Grid Tune Check ----
  if(GridTune && !TrainOnFull) {

    # Grid tune ----
    if(DebugMode) print("Grid tune ----")

    # Load data
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
      stopping_metric = toupper(eval_metric))

    # Hyperparameters ----
    hyper_params <- list()
    hyper_params[["alpha"]] <- Alpha
    hyper_params[["lambda"]] <- Lambda

    # Link ----
    if(is.null(Link)) Link <- "logit"

    # Binary Grid Train Model ----
    grid <- h2o::h2o.grid(
      hyper_params = hyper_params,
      search_criteria = search_criteria,
      is_supervised = TRUE,
      algorithm = "gam",
      grid_id = paste0(ModelID, "_Grid"),
      x = FeatureColNames,
      gam_columns = GamColNames[seq_len(min(length(GamColNames),9L))],
      y = TargetColumnName,
      training_frame = datatrain,
      validation_frame = datavalidate,
      link = Link)

    # Get Best Model ----
    Grid_Out   <- h2o::h2o.getGrid(grid_id = paste0(ModelID, "_Grid"), sort_by = eval_metric, decreasing = Decreasing)

    # Collect Best Grid Model ----
    base_model <- h2o::h2o.getModel(Grid_Out@model_ids[[1L]])
  }

  # Build model ----
  if(!GridTune) {

    # Build model ----
    if(DebugMode) print("Build model ----")

    # Load data
    if(H2OStartUp) localHost <- h2o::h2o.init(nthreads = NThreads, max_mem_size = MaxMem, enable_assertions = FALSE)
    datatrain <- h2o::as.h2o(dataTrain, use_datatable = TRUE)
    if(!TrainOnFull) datavalidate <- h2o::as.h2o(dataTest, use_datatable = TRUE) else datavalidate <- NULL
    if(!is.null(TestData)) datatest <- h2o::as.h2o(TestData, use_datatable = TRUE) else datatest <- NULL

    # Define link ----
    if(!GridTune) if(is.null(Link)) Link <- "logit"

    # Define args ----
    H2OArgs <- list()
    H2OArgs[["x"]] <- FeatureColNames
    H2OArgs[["y"]] <- TargetColumnName
    H2OArgs[["gam_columns"]] <- GamColNames[seq_len(min(length(GamColNames),9L))]
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

    # Build model ----
    base_model <- do.call(h2o::h2o.gam, H2OArgs)
  }

  # Save model ----
  if(DebugMode) print("Save model ----")
  H2OSaveModel(SaveModelObjects.=SaveModelObjects, IfSaveModel.=IfSaveModel, base_model.=base_model, model_path.=model_path, ModelID.=ModelID)

  # Score Final Test Data ----
  if(DebugMode) print("Score Final Test Data ----")
  Predict <- data.table::as.data.table(h2o::h2o.predict(object = base_model, newdata = if(!is.null(TestData)) datatest else if(!TrainOnFull) datavalidate else datatrain))
  data.table::set(Predict, j = "p0", value = NULL)

  # Validation data ----
  if(DebugMode) print("Validation data ----")
  Output <- H2OValidationData(Predict.=Predict, TestData.=TestData, dataTest.=dataTest, dataTrain.=dataTrain, TrainOnFull.=TrainOnFull, SaveModelObjects.=SaveModelObjects, metadata_path.=metadata_path, model_path.=model_path, ModelID.=ModelID, TransformNumericColumns.=NULL, TransformationResults.=NULL, TargetColumnName.=NULL, data.=NULL)
  ValidationData <- Output$ValidationData; rm(Output)

  # Variable Importance ----
  if(DebugMode) print("Variable Importance ----")
  VariableImportance <- H2OVariableImportance(TrainOnFull.=TrainOnFull, base_model.=base_model, SaveModelObjects.=SaveModelObjects, metadata_path.=metadata_path, model_path.=model_path, ModelID.=ModelID)

  # H2O Explain ----
  # if(DebugMode) print("H2O Explain ----")
  # if(!TrainOnFull) {
  #   Explain <- h2o::h2o.explain(base_model, newdata = if(!is.null(TestData)) datatest else if(!is.null(ValidationData) && !TrainOnFull) datavalidate else datatrain)
  # }

  # H2O Shutdown ----
  if(DebugMode) print("H2O Shutdown ----")
  if(H2OShutdown) h2o::h2o.shutdown(prompt = FALSE)

  # Classification evaluation plots ----
  if(DebugMode) print("Running ML_EvalPlots()")
  Output <- ML_EvalPlots(ModelType="classification", TrainOnFull.=TrainOnFull, ValidationData.=ValidationData, NumOfParDepPlots.=NumOfParDepPlots, VariableImportance.=VariableImportance, TargetColumnName.=TargetColumnName, FeatureColNames.=FeatureColNames, SaveModelObjects.=SaveModelObjects, ModelID.=ModelID, metadata_path.=metadata_path, model_path.=model_path, LossFunction.=NULL, EvalMetric.=NULL, EvaluationMetrics.=NULL, predict.=NULL)
  EvaluationPlot <- Output$EvaluationPlot; Output$EvaluationPlot <- NULL
  ParDepPlots <- Output$ParDepPlots; Output$ParDepPlots <- NULL
  ROC_Plot <- Output$ROC_Plot; rm(Output)

  # Generate EvaluationMetrics ----
  if(DebugMode) print("Running BinaryMetrics()")
  EvalMetrics <- BinaryMetrics(ClassWeights.=NULL, CostMatrixWeights.=CostMatrixWeights, SaveModelObjects.=SaveModelObjects, ValidationData.=ValidationData, TrainOnFull.=TrainOnFull, TargetColumnName.=TargetColumnName, ModelID.=ModelID, model_path.=model_path, metadata_path.=metadata_path)

  # Send output to pdf ----
  if(DebugMode) print("Running CatBoostPDF()")
  CatBoostPDF(ModelClass="h2o", ModelType="classification", TrainOnFull.=TrainOnFull, SaveInfoToPDF.=SaveInfoToPDF, EvaluationPlot.=EvaluationPlot, EvaluationBoxPlot.=NULL, ROC_Plot.=ROC_Plot, VariableImportance.=VariableImportance, ParDepPlots.=ParDepPlots, ParDepBoxPlots.=NULL, EvalMetrics.=EvalMetrics, Interaction.=NULL, model_path.=model_path, metadata_path.=metadata_path)

  # Return Objects ----
  if(DebugMode) print("Return Objects ----")
  if(ReturnModelObjects) {
    return(list(
      Model = base_model,
      ValidationData = if(exists("ValidationData") && !is.null(ValidationData)) ValidationData else NULL,
      #H2OExplain = if(exists("Explain") && !is.null(Explain)) Explain else NULL,
      ROC_Plot = if(exists("ROC_Plot") && !is.null(ROC_Plot)) ROC_Plot else NULL,
      EvaluationPlot = if(exists("EvaluationPlot") && !is.null(EvaluationPlot)) EvaluationPlot else NULL,
      EvaluationMetrics = if(exists("EvalMetrics") && !is.null(EvalMetrics)) EvalMetrics else NULL,
      VariableImportance = if(exists("VariableImportance") && !is.null(VariableImportance)) VariableImportance else NULL,
      VI_Plot = if(exists("VariableImportance") && !is.null(VariableImportance)) tryCatch({if(all(c("plotly","dplyr") %chin% installed.packages())) plotly::ggplotly(VI_Plot(Type = "h2o", VariableImportance)) else VI_Plot(Type = "h2o", VariableImportance)}, error = function(x) NULL) else NULL,
      PartialDependencePlots = if(exists("ParDepPlots") && !is.null(ParDepPlots)) ParDepPlots else NULL,
      ColNames = if(exists("Names") && !is.null(Names)) Names else NULL))
  }
}
