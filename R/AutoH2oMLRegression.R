#' @title AutoH2oMLRegression
#'
#' @description AutoH2oMLRegression is an automated H2O modeling framework with grid-tuning and model evaluation that runs a variety of steps. First, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation plot, evaluation boxplot, evaluation metrics, variable importance, partial dependence calibration plots, partial dependence calibration box plots, and column names used in model fitting.
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
#' @param ExcludeAlgos "DRF","GLM","XGBoost","GBM","DeepLearning" and "Stacke-dEnsemble"
#' @param TransformNumericColumns Set to NULL to do nothing; otherwise supply the column names of numeric variables you want transformed
#' @param Methods Choose from "YeoJohnson", "BoxCox", "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", or "Logit". If more than one is selected, the one with the best normalization pearson statistic will be used. Identity is automatically selected and compared.
#' @param eval_metric This is the metric used to identify best grid tuned model. Choose from "MSE", "RMSE", "MAE", "RMSLE"
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
#' @param DebugMode Set to TRUE to print to screen steps taken internally
#' @examples
#' \donttest{
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
#' TestModel <- RemixAutoML::AutoH2oMLRegression(
#'
#'   # Compute management
#'   MaxMem = {gc();paste0(as.character(floor(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE)) / 1000000)),"G")},
#'   NThreads = max(1, parallel::detectCores()-2),
#'   H2OShutdown = TRUE,
#'   H2OStartUp = TRUE,
#'   IfSaveModel = "mojo",
#'
#'   # Model evaluation
#'   eval_metric = "RMSE",
#'   NumOfParDepPlots = 3,
#'
#'   # Metadata arguments
#'   OutputSelection = c("EvalMetrics", "PDFs", "Score_TrainData"),
#'   model_path = NULL,
#'   metadata_path = NULL,
#'   ModelID = "FirstModel",
#'   ReturnModelObjects = TRUE,
#'   SaveModelObjects = FALSE,
#'   SaveInfoToPDF = TRUE,
#'   DebugMode = FALSE,
#'
#'   # Data arguments
#'   TrainOnFull = FALSE,
#'   ValidationData = NULL,
#'   TestData = NULL,
#'   TargetColumnName = "Adrian",
#'   FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
#'   TransformNumericColumns = NULL,
#'   Methods = c("BoxCox", "Asinh", "Asin", "Log", "LogPlus1", "Sqrt", "Logit"),
#'
#'   # Model args
#'   ExcludeAlgos = NULL)
#' }
#' @return Saves to file and returned in list: VariableImportance.csv, Model, ValidationData.csv, EvalutionPlot.png, EvalutionBoxPlot.png, EvaluationMetrics.csv, ParDepPlots.R a named list of features with partial dependence calibration plots, ParDepBoxPlots.R, GridCollect, GridList, and Transformation metadata
#' @export
AutoH2oMLRegression <- function(OutputSelection = c("EvalMetrics", "PDFs", "Score_TrainData"),
                                data = NULL,
                                TrainOnFull = FALSE,
                                ValidationData = NULL,
                                TestData = NULL,
                                TargetColumnName = NULL,
                                FeatureColNames = NULL,
                                ExcludeAlgos = NULL,
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
                                SaveInfoToPDF = TRUE,
                                IfSaveModel = "mojo",
                                H2OShutdown = TRUE,
                                H2OStartUp = TRUE,
                                DebugMode = FALSE) {

  # Ensure model_path and metadata_path exists ----
  if(!is.null(model_path)) if(!dir.exists(file.path(model_path))) dir.create(model_path)
  if(!is.null(metadata_path)) if(!is.null(metadata_path)) if(!dir.exists(file.path(metadata_path))) dir.create(metadata_path)

  # Check Arguments ----
  if(!(tolower(eval_metric) %chin% c("mse", "rmse", "mae", "rmsle"))) stop("eval_metric not in MSE, RMSE, MAE, RMSLE")
  if(!is.null(model_path)) if(!is.character(model_path)) stop("model_path needs to be a character type")
  if(!is.null(metadata_path)) if(!is.character(metadata_path)) stop("metadata_path needs to be a character type")
  if(!is.character(ModelID) & !is.null(ModelID)) stop("ModelID needs to be a character type")
  if(NumOfParDepPlots < 0) stop("NumOfParDepPlots needs to be a positive number")
  if(!(ReturnModelObjects %in% c(TRUE, FALSE))) stop("ReturnModelObjects needs to be TRUE or FALSE")
  if(!(SaveModelObjects %in% c(TRUE, FALSE))) stop("SaveModelObjects needs to be TRUE or FALSE")

  # Data Prepare ----
  if(DebugMode) print("Data Prepare ----")
  Output <- H2ODataPrep(TargetType.="regression", TargetColumnName.=TargetColumnName, data.=data, ValidationData.=ValidationData, TestData.=TestData, TrainOnFull.=TrainOnFull, FeatureColNames.=FeatureColNames, SaveModelObjects.=SaveModelObjects, model_path.=model_path, ModelID.=ModelID, TransformNumericColumns.=TransformNumericColumns, Methods.=Methods)
  TransformationResults <- Output$TransformationResults; Output$TransformationResults <- NULL
  dataTrain <- Output$dataTrain; Output$dataTrain <- NULL
  dataTest <- Output$dataTest; Output$dataTest <- NULL
  TestData <- Output$TestData; Output$TestData <- NULL
  MinVal <- Output$MinVal; Output$MinVal <- NULL
  Names <- Output$Names; rm(Output)

  # Regression Start Up H2O ----
  if(H2OStartUp) localHost <- h2o::h2o.init(nthreads = NThreads, max_mem_size = MaxMem, enable_assertions = FALSE)
  datatrain <- h2o::as.h2o(dataTrain, use_datatable = TRUE)
  if(!TrainOnFull) datavalidate <- h2o::as.h2o(dataTest, use_datatable = TRUE) else datavalidate <- NULL

  # Regression Baseline Model ----
  if(!h2o::h2o.xgboost.available()) exclude <- unique(c(ExcludeAlgos,"XGBoost"))

  # Define args ----
  H2OArgs <- list()
  H2OArgs[["x"]] <- FeatureColNames
  H2OArgs[["y"]] <- TargetColumnName
  H2OArgs[["training_frame"]] <- datatrain
  H2OArgs[["validation_frame"]] <- datavalidate
  H2OArgs[["nfolds"]] <- 2L
  H2OArgs[["stopping_metric"]] <- "AUTO"
  H2OArgs[["project_name"]] <- "winner"
  H2OArgs[["exclude_algos"]] <- ExcludeAlgos
  H2OArgs[["sort_metric"]] <- "AUTO"
  H2OArgs[["max_models"]] <- 20L
  H2OArgs[["seed"]] <- 1L

  # Build model ----
  base_model <- do.call(h2o::h2o.automl, H2OArgs)
  base_model <- base_model@leader

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

  # Save Validation Data to File ----
  if(DebugMode) print("Save Validation Data to File ----")
  H2OSaveModel(SaveModelObjects.=SaveModelObjects, IfSaveModel.=IfSaveModel, base_model.=base_model, model_path.=model_path, ModelID.=ModelID)

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
