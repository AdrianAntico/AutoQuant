#' AutoH2oGLMMultiClass is an automated H2O modeling framework with grid-tuning and model evaluation
#'
#' AutoH2oGLMMultiClass is an automated H2O modeling framework with grid-tuning and model evaluation that runs a variety of steps. First, a stratified sampling (by the target variable) is done to create train and validation sets. Then, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation metrics, confusion matrix, and variable importance.
#' @author Adrian Antico
#' @family Automated Supervised Learning - Multiclass Classification
#' @param data This is your data set for training and testing your model
#' @param TrainOnFull Set to TRUE to train on full data
#' @param ValidationData This is your holdout data set used in modeling either refine your hyperparameters.
#' @param TestData This is your holdout data set. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TargetColumnName Either supply the target column name OR the column number where the target is located (but not mixed types).
#' @param FeatureColNames Either supply the feature column names OR the column number where the target is located (but not mixed types)
#' @param RandomColNumbers Random effects column number indicies
#' @param InteractionColNumbers Column numbers of the features you want to be pairwise interacted
#' @param WeightsColumn Column name of a weights column
#' @param TransformNumericColumns Set to NULL to do nothing; otherwise supply the column names of numeric variables you want transformed
#' @param Methods Choose from "YeoJohnson", "BoxCox", "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", or "Logit". If more than one is selected, the one with the best normalization pearson statistic will be used. Identity is automatically selected and compared.
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
#'     # Compute management
#'     MaxMem = {gc();paste0(as.character(floor(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE)) / 1000000)),"G")},
#'     NThreads = max(1, parallel::detectCores()-2),
#'     H2OShutdown = TRUE,
#'     H2OStartUp = TRUE,
#'     IfSaveModel = "mojo",
#'
#'     # Model evaluation:
#'     eval_metric = "logloss",
#'     NumOfParDepPlots = 3,
#'
#'     # Metadata arguments:
#'     model_path = NULL,
#'     metadata_path = NULL,
#'     ModelID = "FirstModel",
#'     ReturnModelObjects = TRUE,
#'     SaveModelObjects = FALSE,
#'     SaveInfoToPDF = FALSE,
#'
#'     # Data arguments:
#'     data = data,
#'     TrainOnFull = FALSE,
#'     ValidationData = NULL,
#'     TestData = NULL,
#'     TargetColumnName = "Adrian",
#'     FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
#'     RandomColNumbers = NULL,
#'     InteractionColNumbers = NULL,
#'     WeightsColumn = NULL,
#'     TransformNumericColumns = NULL,
#'     Methods = c("BoxCox", "Asinh", "Asin", "Log", "LogPlus1", "Sqrt", "Logit", "YeoJohnson"),
#'
#'     # Model args
#'     GridTune = FALSE,
#'     GridStrategy = "Cartesian",
#'     StoppingRounds = 10,
#'     MaxRunTimeSecs = 3600 * 24 * 7,
#'     MaxModelsInGrid = 10,
#'     Distribution = "multinomial",
#'     Link = "family_default",
#'     RandomDistribution = NULL,
#'     RandomLink = NULL,
#'     Solver = "AUTO",
#'     Alpha = 0.5,
#'     Lambda = NULL,
#'     LambdaSearch = FALSE,
#'     NLambdas = -1,
#'     Standardize = TRUE,
#'     RemoveCollinearColumns = FALSE,
#'     InterceptInclude = TRUE,
#'     NonNegativeCoefficients = FALSE)
#' }
#' @return Saves to file and returned in list: VariableImportance.csv, Model, ValidationData.csv, EvaluationMetrics.csv, GridCollect, and GridList
#' @export
AutoH2oGLMMultiClass <- function(data,
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
                                 SaveModelObjects = FALSE,
                                 SaveInfoToPDF = FALSE,
                                 IfSaveModel = "mojo",
                                 H2OShutdown = TRUE,
                                 H2OStartUp = TRUE,
                                 TransformNumericColumns = NULL,
                                 Methods = c("YeoJohnson", "BoxCox", "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit"),
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

  # data.table optimize ----
  if(parallel::detectCores() > 10) data.table::setDTthreads(threads = max(1L, parallel::detectCores() - 2L)) else data.table::setDTthreads(threads = max(1L, parallel::detectCores()))

  # Ensure model_path and metadata_path exists ----
  if(!is.null(model_path)) if(!dir.exists(file.path(normalizePath(model_path)))) dir.create(normalizePath(model_path))
  if(!is.null(metadata_path)) if(!is.null(metadata_path)) if(!dir.exists(file.path(normalizePath(metadata_path)))) dir.create(normalizePath(metadata_path))

  # MultiClass Check Arguments----
  if(!(tolower(eval_metric) %chin% c("auc", "logloss"))) stop("eval_metric not in AUC, logloss")
  if(!GridTune %in% c(TRUE, FALSE)) stop("GridTune needs to be TRUE or FALSE")
  if(MaxModelsInGrid < 1 & GridTune == TRUE) stop("MaxModelsInGrid needs to be at least 1")
  if(!is.null(model_path)) if(!is.character(model_path)) stop("model_path needs to be a character type")
  if(!is.null(metadata_path)) if(!is.character(metadata_path)) stop("metadata_path needs to be a character type")
  if(!is.character(ModelID) & !is.null(ModelID)) stop("ModelID needs to be a character type")
  if(!(ReturnModelObjects %in% c(TRUE, FALSE))) stop("ReturnModelObjects needs to be TRUE or FALSE")
  if(!(SaveModelObjects %in% c(TRUE, FALSE))) stop("SaveModelObjects needs to be TRUE or FALSE")
  Decreasing <- FALSE

  # MultiClass Target Name Storage ----
  if(!is.character(TargetColumnName)) TargetColumnName <- names(data)[TargetColumnName]

  # MultiClass Ensure data is a data.table ----
  if(!data.table::is.data.table(data)) data.table::setDT(data)
  if(!is.null(ValidationData)) if(!data.table::is.data.table(ValidationData)) data.table::setDT(ValidationData)
  if(!is.null(TestData)) if(!data.table::is.data.table(TestData)) data.table::setDT(TestData)

  # MultiClass Data Partition ----
  if(is.null(ValidationData) & is.null(TestData) & !TrainOnFull) {
    dataSets <- AutoDataPartition(
      data,
      NumDataSets = 3L,
      Ratios = c(0.70, 0.20, 0.10),
      PartitionType = "random",
      StratifyColumnNames = TargetColumnName,
      TimeColumnName = NULL)
    dataTrain <- dataSets$TrainData
    dataTest <- dataSets$ValidationData
    TestData <- dataSets$TestData
  }

  # Create dataTrain if not exists ----
  if(!exists("dataTrain")) dataTrain <- data
  if(!exists("dataTest") && !TrainOnFull) dataTest <- ValidationData

  # Regression ModelDataPrep----
  dataTrain <- ModelDataPrep(data = dataTrain, Impute = FALSE, CharToFactor = TRUE, FactorToChar = FALSE, IntToNumeric = TRUE, LogicalToBinary = TRUE, DateToChar = TRUE, RemoveDates = FALSE, MissFactor = "0", MissNum = -1, IgnoreCols = NULL)
  dataTrain <- ModelDataPrep(data = dataTrain, Impute = FALSE, CharToFactor = TRUE, FactorToChar = FALSE, IntToNumeric = FALSE, LogicalToBinary = FALSE, DateToChar = FALSE, RemoveDates = FALSE, MissFactor = "0", MissNum = -1, IgnoreCols = NULL)
  if(!TrainOnFull) {
    dataTest <- ModelDataPrep(data = dataTest, Impute = FALSE, CharToFactor = TRUE, FactorToChar = FALSE, IntToNumeric = TRUE, LogicalToBinary = TRUE, DateToChar = TRUE, RemoveDates = FALSE, MissFactor = "0", MissNum = -1, IgnoreCols = NULL)
    dataTest <- ModelDataPrep(data = dataTest, Impute = FALSE, CharToFactor = TRUE, FactorToChar = FALSE, IntToNumeric = FALSE, LogicalToBinary = FALSE, DateToChar = FALSE, RemoveDates = FALSE, MissFactor = "0", MissNum = -1, IgnoreCols = NULL)
  }
  if(!is.null(TestData)) {
    TestData <- ModelDataPrep(data = TestData, Impute = FALSE, CharToFactor = TRUE, FactorToChar = FALSE, IntToNumeric = TRUE, LogicalToBinary = TRUE, DateToChar = TRUE, RemoveDates = FALSE, MissFactor = "0", MissNum = -1, IgnoreCols = NULL)
    TestData <- ModelDataPrep(data = TestData, Impute = FALSE, CharToFactor = TRUE, FactorToChar = FALSE, IntToNumeric = FALSE, LogicalToBinary = FALSE, DateToChar = FALSE, RemoveDates = FALSE, MissFactor = "0", MissNum = -1, IgnoreCols = NULL)
  }

  # MultiClass Ensure TargetColumnName Is a Factor Type ----
  if(!is.factor(dataTrain[[eval(TargetColumnName)]])) dataTrain[, eval(TargetColumnName) := as.factor(get(TargetColumnName))]
  if(!TrainOnFull) if(!is.factor(dataTest[[eval(TargetColumnName)]])) dataTest[, eval(TargetColumnName) := as.factor(get(TargetColumnName))]
  if(!is.null(TestData)) if(!is.factor(TestData[[eval(TargetColumnName)]])) TestData[, eval(TargetColumnName) := as.factor(get(TargetColumnName))]

  # MultiClass Save Names of data ----
  if(is.numeric(FeatureColNames)) {
    Names <- data.table::as.data.table(names(data)[FeatureColNames])
    data.table::setnames(Names, "V1", "ColNames")
  } else {
    Names <- data.table::as.data.table(FeatureColNames)
    if(!"V1" %chin% names(Names)) {
      data.table::setnames(Names, "FeatureColNames", "ColNames")
    } else {
      data.table::setnames(Names, "V1", "ColNames")
    }
  }
  if(SaveModelObjects) data.table::fwrite(Names, file = file.path(normalizePath(model_path), paste0(ModelID, "_ColNames.csv")))

  # MultiClass Grid Tune Check ----
  if(GridTune & !TrainOnFull) {
    if(H2OStartUp) localHost <- h2o::h2o.init(nthreads = NThreads, max_mem_size = MaxMem, enable_assertions = FALSE)
    datatrain <- h2o::as.h2o(dataTrain)
    datavalidate <- h2o::as.h2o(dataTest)

    # MultiClass Grid Tune Search Criteria ----
    search_criteria  <- list(
      strategy = GridStrategy,
      max_runtime_secs = MaxRunTimeSecs,
      max_models = MaxModelsInGrid,
      seed = 1234,
      stopping_rounds = StoppingRounds,
      stopping_metric = toupper(eval_metric),
      stopping_tolerance = 1e-3)

    # MultiClass Grid Parameters ----
    hyper_params <- list()
    hyper_params[["solver"]] <- Solver
    hyper_params[["alpha"]] <- Alpha
    hyper_params[["lambda"]] <- Lambda
    hyper_params[["lambda_search"]] <- LambdaSearch
    hyper_params[["standardize"]] <- Standardize
    hyper_params[["remove_collinear_columns"]] <- RemoveCollinearColumns
    hyper_params[["intercept"]] <- InterceptInclude
    hyper_params[["non_negative"]] <- NonNegativeCoefficients

    # MultiClass Grid Train Model ----
    grid <- h2o::h2o.grid(
      hyper_params = hyper_params,
      search_criteria = search_criteria,
      is_supervised = TRUE,
      algorithm = "glm",
      family = "multinomial",
      grid_id = paste0(ModelID, "_Grid"),
      x = FeatureColNames,
      y = TargetColumnName,
      ntrees = Trees,
      training_frame = datatrain,
      validation_frame = datavalidate)

    # MultiClass Get Best Model ----
    Grid_Out <- h2o::h2o.getGrid(
      grid_id = paste0(ModelID, "_Grid"),
      sort_by = eval_metric,
      decreasing = Decreasing)

    # MultiClass Collect Best Grid Model ----
    grid_model <- h2o::h2o.getModel(Grid_Out@model_ids[[1L]])
  }

  # MultiClass Start Up H2O ----
  if(!GridTune) {
    if(H2OStartUp) localHost <- h2o::h2o.init(nthreads = NThreads, max_mem_size = MaxMem, enable_assertions = FALSE)
    datatrain <- h2o::as.h2o(dataTrain, use_datatable = TRUE)
    if(!TrainOnFull) datavalidate <- h2o::as.h2o(dataTest, use_datatable = TRUE) else datavalidate <- NULL
  }

  # Define ml args ----
  H2OArgs <- list()
  H2OArgs[["x"]] <- FeatureColNames
  H2OArgs[["y"]] <- TargetColumnName
  H2OArgs[["interactions"]] <- InteractionColNumbers
  H2OArgs[["weights_column"]] <- WeightsColumn[1L]
  if(!is.null(RandomDistribution) & !is.null(RandomLink)) H2OArgs[["HGLM"]] <- TRUE else H2OArgs[["HGLM"]] <- FALSE
  H2OArgs[["training_frame"]] <- datatrain
  H2OArgs[["validation_frame"]] <- datavalidate
  H2OArgs[["family"]] <- Distribution[1L]
  H2OArgs[["link"]] <- Link[1L]
  H2OArgs[["model_id"]] <- ModelID[1L]
  H2OArgs[["rand_family"]] <- RandomDistribution[1L]
  H2OArgs[["rand_link"]] <- RandomLink[1L]
  H2OArgs[["random_columns"]] <- RandomColNumbers
  H2OArgs[["solver"]] <- Solver[1L]
  H2OArgs[["alpha"]] <- Alpha[1L]
  H2OArgs[["lambda"]] <- Lambda[1L]
  H2OArgs[["lambda_search"]] <- LambdaSearch[1L]
  H2OArgs[["nlambdas"]] <- NLambdas
  H2OArgs[["standardize"]] <- Standardize[1L]
  H2OArgs[["remove_collinear_columns"]] <- RemoveCollinearColumns
  H2OArgs[["intercept"]] <- InterceptInclude[1L]
  H2OArgs[["non_negative"]] <- NonNegativeCoefficients[1L]

  # Build model ----
  base_model <- do.call(what = h2o::h2o.glm, args = H2OArgs)

  # MultiClass Get Metrics ----
  if(GridTune) {
    if(!is.null(TestData)) {
      datatest <-  h2o::as.h2o(TestData)
      GridMetrics <- h2o::h2o.performance(model = base_model, newdata = datatest)
      BaseMetrics <- h2o::h2o.performance(model = base_model, newdata = datatest)
    } else {
      GridMetrics <- h2o::h2o.performance(model = base_model, newdata = datavalidate)
      BaseMetrics <- h2o::h2o.performance(model = base_model, newdata = datavalidate)
    }
  } else if(!TrainOnFull) {
    if(!is.null(TestData)) {
      datatest <- h2o::as.h2o(TestData)
      BaseMetrics <- h2o::h2o.performance(model = base_model, newdata = datatest)
    } else {
      BaseMetrics <- h2o::h2o.performance(model = base_model, newdata = datavalidate)
    }
  } else {
    BaseMetrics <- h2o::h2o.performance(model = base_model, newdata = datatrain)
  }

  # MultiClass Evaluate Metrics ----
  if(GridTune & !TrainOnFull) {
    if(tolower(eval_metric) == "logloss") {
      BaseMetric <- BaseMetrics@metrics$logloss
      GridMetric <- GridMetrics@metrics$logloss
      if(GridMetric < BaseMetric) {
        FinalModel <- grid_model
        EvalMetric <- GridMetric
        ConfusionMatrix <- data.table::as.data.table(GridMetrics@metrics$cm$table)
      } else {
        FinalModel <- base_model
        EvalMetric <- BaseMetrics@metrics$logloss
        ConfusionMatrix <- data.table::as.data.table(BaseMetrics@metrics$cm$table)
      }
    } else if(tolower(eval_metric) == "r2") {
      BaseMetric <- BaseMetrics@metrics$r2
      GridMetric <- GridMetrics@metrics$r2
      if(GridMetric > BaseMetric) {
        FinalModel <- grid_model
        EvalMetric <- GridMetric
        ConfusionMatrix <- data.table::as.data.table(GridMetrics@metrics$cm$table)
      } else {
        FinalModel <- base_model
        EvalMetric <- BaseMetric
        ConfusionMatrix <- data.table::as.data.table(BaseMetrics@metrics$cm)
      }
    } else if(tolower(eval_metric) == "rmse") {
      BaseMetric <- BaseMetrics@metrics$logloss
      GridMetric <- GridMetrics@metrics$logloss
      if(GridMetric < BaseMetric) {
        FinalModel <- grid_model
        EvalMetric <- GridMetric
        ConfusionMatrix <- data.table::as.data.table(GridMetrics@metrics$cm$table)
      } else {
        FinalModel <- base_model
        EvalMetric <- BaseMetric
        ConfusionMatrix <- data.table::as.data.table(BaseMetrics@metrics$cm)
      }
    } else if(tolower(eval_metric) == "mse") {
      BaseMetric <- BaseMetrics@metrics$logloss
      GridMetric <- GridMetrics@metrics$logloss
      if(GridMetric < BaseMetric) {
        FinalModel <- grid_model
        EvalMetric <- GridMetric
        ConfusionMatrix <- data.table::as.data.table(GridMetrics@metrics$cm$table)
      } else {
        FinalModel <- base_model
        EvalMetric <- BaseMetric
        ConfusionMatrix <- data.table::as.data.table(BaseMetrics@metrics$cm$table)
      }
    }
  } else {
    if(tolower(eval_metric) == "logloss") {
      FinalModel <- base_model
      EvalMetric <- BaseMetrics@metrics$logloss
      ConfusionMatrix <- data.table::as.data.table(BaseMetrics@metrics$cm$table)
    } else if(tolower(eval_metric) == "r2") {
      FinalModel <- base_model
      EvalMetric <- BaseMetrics@metrics$r2
      ConfusionMatrix <- data.table::as.data.table(BaseMetrics@metrics$cm$table)
    } else if(tolower(eval_metric) == "rmse") {
      FinalModel <- base_model
      EvalMetric <- BaseMetrics@metrics$RMSE
      ConfusionMatrix <- data.table::as.data.table(BaseMetrics@metrics$cm$table)
    } else if(tolower(eval_metric) == "mse") {
      FinalModel <- base_model
      EvalMetric <- BaseMetrics@metrics$MSE
      ConfusionMatrix <- data.table::as.data.table(BaseMetrics@metrics$cm$table)
    }
  }

  # MultiClass Save Final Model ----
  if(SaveModelObjects) {
    if(tolower(IfSaveModel) == "mojo") {
      SaveModel <- h2o::h2o.saveMojo(object = FinalModel, path = model_path, force = TRUE)
      h2o::h2o.download_mojo(
        model = FinalModel,
        path = model_path,
        get_genmodel_jar = TRUE,
        genmodel_path = model_path,
        genmodel_name = ModelID)
    } else {
      SaveModel <- h2o::h2o.saveModel(object = FinalModel, path = model_path, force = TRUE)
    }
  }

  # MultiClass Score Final Test Data ----
  if(!is.null(TestData)) {
    Predict <- data.table::as.data.table(h2o::h2o.predict(object = FinalModel, newdata = datatest))
  } else if(!TrainOnFull) {
    Predict <- data.table::as.data.table(h2o::h2o.predict(object = FinalModel, newdata = datavalidate))
  } else {
    Predict <- data.table::as.data.table(h2o::h2o.predict(object = FinalModel, newdata = datatrain))
  }

  # MultiClass Variable Importance ----
  VariableImportance <- data.table::as.data.table(h2o::h2o.varimp(object = FinalModel))

  # MultiClass Format Variable Importance Table ----
  data.table::setnames(VariableImportance, c("variable","relative_importance","scaled_importance","percentage"), c("Variable","RelativeImportance","ScaledImportance","Percentage"))
  VariableImportance[, ':=' (
    RelativeImportance = round(RelativeImportance, 4L),
    ScaledImportance = round(ScaledImportance, 4L),
    Percentage = round(Percentage, 4L))]

  # MultiClass Save Variable Importance ----
  if(SaveModelObjects) {
    if(!is.null(metadata_path)) {
      data.table::fwrite(VariableImportance, file = file.path(normalizePath(metadata_path), paste0(ModelID, "_VariableImportance.csv")))
    } else {
      data.table::fwrite(VariableImportance, file = file.path(normalizePath(model_path), paste0(ModelID, "_VariableImportance.csv")))
    }
  }

  # MultiClass H2O Shutdown ----
  if(H2OShutdown) h2o::h2o.shutdown(prompt = FALSE)

  # MultiClass Create Validation Data ----
  if(!is.null(TestData)) {
    ValidationData <- data.table::as.data.table(cbind(TestData, Predict))
    data.table::setnames(ValidationData, "predict", "Predict", skip_absent = TRUE)
  } else if(!TrainOnFull) {
    ValidationData <- data.table::as.data.table(cbind(dataTest, Predict))
    data.table::setnames(ValidationData, "predict", "Predict", skip_absent = TRUE)
  } else {
    ValidationData <- data.table::as.data.table(cbind(dataTrain, Predict))
    data.table::setnames(ValidationData, "predict", "Predict", skip_absent = TRUE)
  }

  # MultiClass Metrics Accuracy ----
  if(!TrainOnFull) {
    ValidationData[, eval(TargetColumnName) := as.character(get(TargetColumnName))]
    ValidationData[, Predict := as.character(Predict)]
    MetricAcc <- ValidationData[, mean(data.table::fifelse(get(TargetColumnName) == Predict, 1.0, 0.0), na.rm = TRUE)]
  }

  # MultiClass Evaluation Metrics Table ----
  if(!TrainOnFull) {
    EvaluationMetrics <- data.table::data.table(Metric = c("Accuracy", "MicroAUC", "temp"), Value = c(round(MetricAcc, 4),NA,round(EvalMetric, 4L)))
    data.table::set(EvaluationMetrics, i = 3L, j = 1L, value = paste0(eval_metric))
  }

  # MultiClass Save Validation Data to File ----
  if(SaveModelObjects & !TrainOnFull) {
    if(!is.null(metadata_path)) {
      data.table::fwrite(ValidationData, file = file.path(normalizePath(metadata_path), paste0(ModelID, "_ValidationData.csv")))
    } else {
      data.table::fwrite(ValidationData, file = file.path(normalizePath(model_path), paste0(ModelID, "_ValidationData.csv")))
    }
  }

  # MultiClass Save ConfusionMatrix to File ----
  if(SaveModelObjects & !TrainOnFull) {
    if(!is.null(metadata_path)) {
      data.table::fwrite(ConfusionMatrix, file = file.path(normalizePath(metadata_path), paste0(ModelID, "_EvaluationMetrics.csv")))
    } else {
      data.table::fwrite(ConfusionMatrix, file = file.path(normalizePath(model_path), paste0(ModelID, "_EvaluationMetrics.csv")))
    }
  }

  # VI_Plot_Function
  if(!TrainOnFull) {
    VI_Plot <- function(VI_Data, ColorHigh = "darkblue", ColorLow = "white") {
      ggplot2::ggplot(VI_Data[1:min(10,.N)], ggplot2::aes(x = reorder(Variable, ScaledImportance ), y = ScaledImportance , fill = ScaledImportance )) +
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::scale_fill_gradient2(mid = ColorLow,high = ColorHigh) +
        ChartTheme(Size = 12L, AngleX = 0L, LegendPosition = "right") +
        ggplot2::coord_flip() +
        ggplot2::labs(title = "Global Variable Importance") +
        ggplot2::xlab("Top Model Features") +
        ggplot2::ylab("Value") +
        ggplot2::theme(legend.position = "none")
    }
  }

  # MultiClass Return Objects ----
  if(ReturnModelObjects) {
    if(!TrainOnFull) {
      return(list(
        Model = FinalModel,
        ValidationData = ValidationData,
        ConfusionMatrix = ConfusionMatrix,
        EvaluationMetrics = EvaluationMetrics,
        VariableImportance = VariableImportance,
        VI_Plot = VI_Plot(VI_Data = VariableImportance),
        ColNames = Names))
    } else {
      return(list(
        Model = FinalModel,
        ColNames = Names))
    }
  }
}
