#' AutoH2oGAMRegression is an automated H2O modeling framework with grid-tuning and model evaluation
#'
#' AutoH2oGAMRegression is an automated H2O modeling framework with grid-tuning and model evaluation that runs a variety of steps. First, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation plot, evaluation boxplot, evaluation metrics, variable importance, partial dependence calibration plots, partial dependence calibration box plots, and column names used in model fitting.
#' @author Adrian Antico
#' @family Automated Supervised Learning - Regression
#' @param data This is your data set for training and testing your model
#' @param TrainOnFull Set to TRUE to train on full data
#' @param ValidationData This is your holdout data set used in modeling either refine your hyperparameters.
#' @param TestData This is your holdout data set. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TargetColumnName Either supply the target column name OR the column number where the target is located (but not mixed types).
#' @param FeatureColNames Either supply the feature column names OR the column number where the target is located (but not mixed types)
#' @param GamColNames GAM column names. Up to 9 features
#' @param Distribution "binomial", "quasibinomial"
#' @param link identity, logit, log, inverse, tweedie
#' @param TransformNumericColumns Set to NULL to do nothing; otherwise supply the column names of numeric variables you want transformed
#' @param Methods Choose from "BoxCox", "Asinh", "Asin", "Log", "LogPlus1", "Logit", "YeoJohnson". Function will determine if one cannot be used because of the underlying data.
#' @param eval_metric This is the metric used to identify best grid tuned model. Choose from "MSE", "RMSE", "MAE", "RMSLE"
#' @param GridTune Set to TRUE to run a grid tuning procedure. Set a number in MaxModelsInGrid to tell the procedure how many models you want to test.
#' @param MaxMem Set the maximum amount of memory you'd like to dedicate to the model run. E.g. "32G"
#' @param NThreads Set the number of threads you want to dedicate to the model building
#' @param MaxModelsInGrid Number of models to test from grid options (1080 total possible options)
#' @param model_path A character string of your path file to where you want your output saved
#' @param metadata_path A character string of your path file to where you want your model evaluation output saved. If left NULL, all output will be saved to model_path.
#' @param ModelID A character string to name your model and output
#' @param NumOfParDepPlots Tell the function the number of partial dependence calibration plots you want to create. Calibration boxplots will only be created for numerical features (not dummy variables)
#' @param ReturnModelObjects Set to TRUE to output all modeling objects (E.g. plots and evaluation metrics)
#' @param SaveModelObjects Set to TRUE to return all modeling objects to your environment
#' @param IfSaveModel Set to "mojo" to save a mojo file, otherwise "standard" to save a regular H2O model object
#' @param H2OShutdown For use in other functions.
#' @param HurdleModel Set to FALSE
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
#'  MaxMem = "32G",
#'  NThreads = max(1, parallel::detectCores()-2),
#'  H2OShutdown = TRUE,
#'  IfSaveModel = "mojo",
#'
#'  # Model evaluation:
#'  eval_metric = "RMSE",
#'  NumOfParDepPlots = 3,
#'
#'  # Metadata arguments:
#'  model_path = NULL,
#'  metadata_path = NULL,
#'  ModelID = "FirstModel",
#'  ReturnModelObjects = TRUE,
#'  SaveModelObjects = FALSE,
#'
#'  # Data arguments:
#'  data = data,
#'  TrainOnFull = FALSE,
#'  ValidationData = NULL,
#'  TestData = NULL,
#'  TargetColumnName = "Adrian",
#'  FeatureColNames = names(data)[!names(data) %chin%
#'                                  c("IDcol_1", "IDcol_2","Adrian")],
#'  GamColNames = GamCols,
#'  TransformNumericColumns = NULL,
#'  Methods = c("BoxCox", "Asinh", "Asin", "Log",
#'              "LogPlus1", "Logit", "YeoJohnson"),
#'
#'  # Model args
#'  GridTune = FALSE,
#'  MaxModelsInGrid = 10,
#'  Distribution = "gaussian",
#'  link = "Family_Default")
#' }
#' @return Saves to file and returned in list: VariableImportance.csv, Model, ValidationData.csv, EvalutionPlot.png, EvalutionBoxPlot.png, EvaluationMetrics.csv, ParDepPlots.R a named list of features with partial dependence calibration plots, ParDepBoxPlots.R, GridCollect, GridList, and Transformation metadata
#' @export
AutoH2oGAMRegression <- function(data,
                                 TrainOnFull = FALSE,
                                 ValidationData = NULL,
                                 TestData = NULL,
                                 TargetColumnName = NULL,
                                 FeatureColNames = NULL,
                                 GamColNames = NULL,
                                 Distribution = "gaussian",
                                 link = "identity",
                                 TransformNumericColumns = NULL,
                                 Methods = c("BoxCox", "Asinh", "Asin", "Log", "LogPlus1", "Logit", "YeoJohnson"),
                                 eval_metric = "RMSE",
                                 GridTune = FALSE,
                                 MaxMem = "32G",
                                 NThreads = max(1, parallel::detectCores()-2),
                                 MaxModelsInGrid = 2,
                                 model_path = NULL,
                                 metadata_path = NULL,
                                 ModelID = "FirstModel",
                                 NumOfParDepPlots = 3,
                                 ReturnModelObjects = TRUE,
                                 SaveModelObjects = FALSE,
                                 IfSaveModel = "mojo",
                                 H2OShutdown = TRUE,
                                 HurdleModel = FALSE) {

  # data.table optimize----
  if(parallel::detectCores() > 10) data.table::setDTthreads(threads = max(1L, parallel::detectCores() - 2L)) else data.table::setDTthreads(threads = max(1L, parallel::detectCores()))

  # Ensure model_path and metadata_path exists----
  if(!is.null(model_path)) if(!dir.exists(file.path(normalizePath(model_path)))) dir.create(normalizePath(model_path))
  if(!is.null(metadata_path)) if(!is.null(metadata_path)) if(!dir.exists(file.path(normalizePath(metadata_path)))) dir.create(normalizePath(metadata_path))

  # Regression Check Arguments----
  if(!(tolower(eval_metric) %chin% c("mse", "rmse", "mae", "rmsle"))) return("eval_metric not in MSE, RMSE, MAE, RMSLE")
  if(!GridTune %in% c(TRUE, FALSE)) return("GridTune needs to be TRUE or FALSE")
  if(MaxModelsInGrid < 1 & GridTune) return("MaxModelsInGrid needs to be at least 1")
  if(!is.null(model_path)) if(!is.character(model_path)) return("model_path needs to be a character type")
  if(!is.null(metadata_path)) if(!is.character(metadata_path)) return("metadata_path needs to be a character type")
  if(!is.character(ModelID) & !is.null(ModelID)) return("ModelID needs to be a character type")
  if(NumOfParDepPlots < 0) return("NumOfParDepPlots needs to be a positive number")
  if(!(ReturnModelObjects %in% c(TRUE, FALSE))) return("ReturnModelObjects needs to be TRUE or FALSE")
  if(!(SaveModelObjects %in% c(TRUE, FALSE))) return("SaveModelObjects needs to be TRUE or FALSE")

  # Regression Ensure data is a data.table----
  if(!data.table::is.data.table(data)) data.table::setDT(data)
  if(!is.null(ValidationData)) if(!data.table::is.data.table(ValidationData)) data.table::setDT(ValidationData)
  if(!is.null(TestData)) if(!data.table::is.data.table(TestData)) data.table::setDT(TestData)

  # Convert TransformNumericColumns to Names if not character----
  if(!is.null(TransformNumericColumns)) if(!is.character(TransformNumericColumns)) TransformNumericColumns <- names(data)[TransformNumericColumns]

  # Transform data, ValidationData, and TestData----
  if(!is.null(ValidationData) & !is.null(TransformNumericColumns)) {
    MeanTrainTarget <- data[, mean(get(TargetColumnName))]
    Output <- AutoTransformationCreate(
      data,
      ColumnNames = TransformNumericColumns,
      Methods = c("BoxCox", "Asinh", "Asin", "Log", "LogPlus1", "Logit", "YeoJohnson"),
      Path = model_path,
      TransID = ModelID,
      SaveOutput = SaveModelObjects)
    data <- Output$Data
    TransformationResults <- Output$FinalResults

    # Transform ValidationData----
    ValidationData <- AutoTransformationScore(
      ScoringData = ValidationData,
      Type = "Apply",
      FinalResults = TransformationResults,
      TransID = NULL,
      Path = NULL)

    # Transform TestData----
    if(!is.null(TestData)) {
      TestData <- AutoTransformationScore(
        ScoringData = TestData,
        Type = "Apply",
        FinalResults = TransformationResults,
        TransID = NULL,
        Path = NULL)
    }
  }

  # Regression Data Partition----
  if(is.null(ValidationData) & is.null(TestData) & !TrainOnFull) {
    if(!is.null(TransformNumericColumns)) {
      dataSets <- AutoDataPartition(
        data,
        NumDataSets = 3L,
        Ratios = c(0.70, 0.20, 0.10),
        PartitionType = "random",
        StratifyColumnNames = NULL,
        TimeColumnName = NULL)
      data <- dataSets$TrainData
      ValidationData <- dataSets$ValidationData
      TestData <- dataSets$TestData

      # Mean of data----
      MeanTrainTarget <- data[, mean(get(TargetColumnName))]

      # Transform data sets----
      Output <- AutoTransformationCreate(
        data,
        ColumnNames = TransformNumericColumns,
        Methods = c("BoxCox", "Asinh", "Asin", "Log", "LogPlus1", "Logit", "YeoJohnson"),
        Path = model_path,
        TransID = ModelID,
        SaveOutput = SaveModelObjects)
      data <- Output$Data
      TransformationResults <- Output$FinalResults

      # Transform ValidationData----
      ValidationData <- AutoTransformationScore(
        ScoringData = ValidationData,
        Type = "Apply",
        FinalResults = TransformationResults,
        TransID = NULL,
        Path = NULL)

      # Transform TestData----
      if(!is.null(TestData)) {
        TestData <- AutoTransformationScore(
          ScoringData = TestData,
          Type = "Apply",
          FinalResults = TransformationResults,
          TransID = NULL,
          Path = NULL)
      }
    } else {
      dataSets <- AutoDataPartition(
        data,
        NumDataSets = 3L,
        Ratios = c(0.70, 0.20, 0.10),
        PartitionType = "random",
        StratifyColumnNames = NULL,
        TimeColumnName = NULL)
      data <- dataSets$TrainData
      ValidationData <- dataSets$ValidationData
      TestData <- dataSets$TestData
      MeanTrainTarget <- data[, mean(get(TargetColumnName))]
    }
  }

  # Regression ModelDataPrep----
  dataTrain <- ModelDataPrep(data = data, Impute = FALSE, CharToFactor = TRUE)

  # Regression ModelDataPrep----
  if(!TrainOnFull) dataTest <- ModelDataPrep(data = ValidationData, Impute = FALSE, CharToFactor = TRUE)
  if(!is.null(TestData)) TestData <- ModelDataPrep(data = TestData, Impute = FALSE, CharToFactor = TRUE)
  if(is.character(TargetColumnName)) Target <- TargetColumnName else Target <- names(data)[TargetColumnName]

  # Regression Get Min Value of Target Data----
  MinVal <- min(data[[eval(Target)]], na.rm = TRUE)

  # Regression Save Names of data----
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

  # Regression Grid Tune Check----
  if(GridTune & !TrainOnFull) {
    if(!HurdleModel) h2o::h2o.init(max_mem_size = MaxMem, nthreads = NThreads, enable_assertions = FALSE)
    datatrain    <- h2o::as.h2o(dataTrain)
    datavalidate <- h2o::as.h2o(dataTest)

    # Regression Grid Tune Search Criteria----
    search_criteria  <- list(
      strategy             = "RandomDiscrete",
      max_runtime_secs     = 3600 * 24 * 7,
      max_models           = MaxModelsInGrid,
      seed                 = 1234,
      stopping_rounds      = 10L,
      stopping_metric      = toupper(eval_metric),
      stopping_tolerance   = 1e-3)

    # Regression Grid Parameters----
    if(Distribution == "tweedie") {
      hyper_params <- list(
        alpha = c(0,0.25,0.5,0.75,1),
        lambda = c(0,0.01,0.05,0.10),
        theta = c(1e-10, 0.01, 0.05, 0.10),
        tweedie_link_power = c(1, 0.95, 0.90, 0.75),
        tweedie_variance_power = c(0,0.25,0.50,0.75,1))
    } else {
      hyper_params <- list(
        alpha = c(0,0.25,0.5,0.75,1),
        lambda = c(0,0.01,0.05,0.10),
        theta = c(1e-10, 0.01, 0.05, 0.10))
    }

    # Link----
    if(!is.null(link)) Link <- link else Link <- "identity"

    # Regression Grid Train Model----
    grid <- h2o::h2o.grid(
      hyper_params         = hyper_params,
      search_criteria      = search_criteria,
      is_supervised        = TRUE,
      algorithm            = "gam",
      grid_id              = paste0(ModelID, "_Grid"),
      x                    = FeatureColNames,
      gam_columns          = GamColNames[1L:(min(length(GamColNames),9L))],
      y                    = TargetColumnName,
      training_frame       = datatrain,
      validation_frame     = datavalidate,
      family               = Distribution,
      link                 = Link,
      seed                 = 1234)

    # Regression Get Best Model----
    Grid_Out <- h2o::h2o.getGrid(grid_id = paste0(ModelID, "_Grid"), sort_by = eval_metric, decreasing = FALSE)

    # Regression Collect Best Grid Model----
    grid_model <- h2o::h2o.getModel(Grid_Out@model_ids[[1L]])
  }

  # Regression Start Up H2O----
  if(!GridTune) {
    if(!HurdleModel) h2o::h2o.init(max_mem_size = MaxMem, nthreads = NThreads, enable_assertions = FALSE)
    datatrain <- h2o::as.h2o(dataTrain)
    if(!TrainOnFull) datavalidate <- h2o::as.h2o(dataTest)
  } else {
    datatrain <- h2o::as.h2o(data)
  }

  # Define link----
  if(!GridTune) if(!is.null(link)) Link <- link else Link <- "family_default"

  # Regression Build Baseline Model----
  if(!TrainOnFull) {
    base_model <- h2o::h2o.gam(
      x                = FeatureColNames,
      gam_columns      = GamColNames[1L:(min(length(GamColNames),9L))],
      y                = TargetColumnName,
      training_frame   = datatrain,
      validation_frame = datavalidate,
      family           = Distribution,
      link             = Link,
      model_id         = ModelID)
  } else {
    base_model <- h2o::h2o.gam(
      x                = FeatureColNames,
      gam_columns      = GamColNames[1L:(min(length(GamColNames),9L))],
      y                = TargetColumnName,
      training_frame   = datatrain,
      family           = Distribution,
      link             = Link,
      model_id         = ModelID)
  }

  # Regression Grab Evaluation Metric----
  if(GridTune & !TrainOnFull) {
    if(!is.null(TestData)) {
      datatest <- h2o::as.h2o(TestData)
      if(tolower(eval_metric) == "mse") {
        GridModelEval <- h2o::h2o.mse(h2o::h2o.performance(model = grid_model, newdata = datatest))
        BaseModelEval <- h2o::h2o.mse(h2o::h2o.performance(model = base_model, newdata = datatest))
      } else if(tolower(eval_metric) == "rmse") {
        GridModelEval <- h2o::h2o.rmse(h2o::h2o.performance(model = grid_model, newdata = datatest))
        BaseModelEval <- h2o::h2o.rmse(h2o::h2o.performance(model = base_model, newdata = datatest))
      } else if(tolower(eval_metric) == "mae") {
        GridModelEval <- h2o::h2o.mae(h2o::h2o.performance(model = grid_model, newdata = datatest))
        BaseModelEval <- h2o::h2o.mae(h2o::h2o.performance(model = base_model, newdata = datatest))
      } else if(tolower(eval_metric) == "rmsle") {
        GridModelEval <- h2o::h2o.rmsle(h2o::h2o.performance(model = grid_model, newdata = datatest))
        BaseModelEval <- h2o::h2o.rmsle(h2o::h2o.performance(model = base_model, newdata = datatest))
      }
    } else {
      if(tolower(eval_metric) == "mse") {
        GridModelEval <- h2o::h2o.mse(h2o::h2o.performance(model = grid_model, newdata = datavalidate))
        BaseModelEval <- h2o::h2o.mse(h2o::h2o.performance(model = base_model, newdata = datavalidate))
      } else if(tolower(eval_metric) == "rmse") {
        GridModelEval <- h2o::h2o.rmse(h2o::h2o.performance(model = grid_model, newdata = datavalidate))
        BaseModelEval <- h2o::h2o.rmse(h2o::h2o.performance(model = base_model, newdata = datavalidate))
      } else if(tolower(eval_metric) == "mae") {
        GridModelEval <- h2o::h2o.mae(h2o::h2o.performance(model = grid_model, newdata = datavalidate))
        BaseModelEval <- h2o::h2o.mae(h2o::h2o.performance(model = base_model, newdata = datavalidate))
      } else if(tolower(eval_metric) == "rmsle") {
        GridModelEval <- h2o::h2o.rmsle(h2o::h2o.performance(model = grid_model, newdata = datavalidate))
        BaseModelEval <- h2o::h2o.rmsle(h2o::h2o.performance(model = base_model, newdata = datavalidate))
      }
    }
  } else if(!TrainOnFull) {
    if(!is.null(TestData)) {
      datatest <-  h2o::as.h2o(TestData)
      if(tolower(eval_metric) == "mse") {
        BaseModelEval <- h2o::h2o.mse(h2o::h2o.performance(model = base_model, newdata = datatest))
      } else if(tolower(eval_metric) == "rmse") {
        BaseModelEval <- h2o::h2o.rmse(h2o::h2o.performance(model = base_model, newdata = datatest))
      } else if(tolower(eval_metric) == "mae") {
        BaseModelEval <- h2o::h2o.mae(h2o::h2o.performance(model = base_model, newdata = datatest))
      } else if(tolower(eval_metric) == "rmsle") {
        BaseModelEval <- h2o::h2o.rmsle(h2o::h2o.performance(model = base_model, newdata = datatest))
      }
    } else if(!is.null(ValidationData)) {
      if(tolower(eval_metric) == "mse") {
        BaseModelEval <- h2o::h2o.mse(h2o::h2o.performance(model = base_model, newdata = datavalidate))
      } else if(tolower(eval_metric) == "rmse") {
        BaseModelEval <- h2o::h2o.rmse(h2o::h2o.performance(model = base_model, newdata = datavalidate))
      } else if(tolower(eval_metric) == "mae") {
        BaseModelEval <- h2o::h2o.mae(h2o::h2o.performance(model = base_model, newdata = datavalidate))
      } else if(tolower(eval_metric) == "rmsle") {
        BaseModelEval <- h2o::h2o.rmsle(h2o::h2o.performance(model = base_model, newdata = datavalidate))
      }
    } else {
      if(tolower(eval_metric) == "mse") {
        BaseModelEval <- h2o::h2o.mse(h2o::h2o.performance(model = base_model, newdata = datatrain))
      } else if(tolower(eval_metric) == "rmse") {
        BaseModelEval <- h2o::h2o.rmse(h2o::h2o.performance(model = base_model, newdata = datatrain))
      } else if(tolower(eval_metric) == "mae") {
        BaseModelEval <- h2o::h2o.mae(h2o::h2o.performance(model = base_model, newdata = datatrain))
      } else if(tolower(eval_metric) == "rmsle") {
        BaseModelEval <- h2o::h2o.rmsle(h2o::h2o.performance(model = base_model, newdata = datatrain))
      }
    }
  }

  # Regression Pick Winner----
  if(GridTune & !TrainOnFull) {
    if(GridModelEval < BaseModelEval) {
      FinalModel <- grid_model
    } else {
      FinalModel <- base_model
    }
  } else {
    FinalModel <- base_model
  }

  # Regression Save Model----
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

  # Regression Score Final Test Data----
  if(!is.null(TestData)) {
    Predict <- data.table::as.data.table(h2o::h2o.predict(object = FinalModel, newdata = datatest))
  } else if(!is.null(ValidationData) & TrainOnFull == FALSE) {
    Predict <- data.table::as.data.table(h2o::h2o.predict(object = FinalModel, newdata = datavalidate))
  } else {
    Predict <- data.table::as.data.table(h2o::h2o.predict(object = FinalModel, newdata = datatrain))
  }

  # Regression Variable Importance----
  if(!TrainOnFull) {
    VariableImportance <- data.table::as.data.table(h2o::h2o.varimp(object = FinalModel))
    if(SaveModelObjects) {
      if(!is.null(metadata_path)) {
        data.table::fwrite(VariableImportance, file = file.path(normalizePath(metadata_path), paste0(ModelID, "_VariableImportance.csv")))
      } else {
        data.table::fwrite(VariableImportance, file = file.path(normalizePath(model_path), paste0(ModelID, "_VariableImportance.csv")))
      }
    }

    # Regression Format Variable Importance Table----
    data.table::setnames(
      VariableImportance,
      c("variable", "relative_importance", "scaled_importance", "percentage"),
      c("Variable", "RelativeImportance", "ScaledImportance", "Percentage"))
    VariableImportance[, ':=' (RelativeImportance = round(RelativeImportance, 4L), ScaledImportance = round(ScaledImportance, 4L), Percentage = round(Percentage, 4L))]
  }

  # Regression H2O Shutdown----
  if(H2OShutdown) h2o::h2o.shutdown(prompt = FALSE)

  # Regression Create Validation Data----
  if(!is.null(TestData)) {
    ValidationData <- data.table::as.data.table(cbind(TestData, Predict))
  } else if(!is.null(ValidationData) & !TrainOnFull) {
    ValidationData <- data.table::as.data.table(cbind(dataTest, Predict))
  } else {
    ValidationData <- data.table::as.data.table(cbind(dataTrain, Predict))
  }

  # Regression Change Prediction Name----
  data.table::setnames(ValidationData, "predict", "Predict")

  # Inverse Transform----
  if(!is.null(TransformNumericColumns)) {
    if(GridTune) TransformationResults <- TransformationResults[ColumnName != "Predict"]
    TransformationResults <- data.table::rbindlist(list(
      TransformationResults,
      data.table::data.table(
        ColumnName = "Predict",
        MethodName = rep(TransformationResults[ColumnName == eval(TargetColumnName), MethodName], 1),
        Lambda = rep(TransformationResults[ColumnName == eval(TargetColumnName), Lambda], 1),
        NormalizedStatistics = rep(0, 1))))

    # If Actual target columnname == "Target" remove the duplicate version----
    if(length(unique(TransformationResults[["ColumnName"]])) != nrow(TransformationResults)) {
      temp <- TransformationResults[, .N, by = "ColumnName"][N != 1][[1]]
      temp1 <- which(names(ValidationData) == temp)[1]
      ValidationData[, eval(names(data)[temp1]) := NULL]
      TransformationResults <- TransformationResults[, ID := 1:.N][ID != which(TransformationResults[["ID"]] == temp1)][, ID := NULL]
    }

    # Transform Target and Predicted Value----
    ValidationData <- AutoTransformationScore(
      ScoringData = ValidationData,
      Type = "Inverse",
      FinalResults = TransformationResults,
      TransID = NULL,
      Path = NULL)
  }

  # Regression Get R2----
  if(!TrainOnFull) r_squared <- (ValidationData[, stats::cor(get(TargetColumnName), Predict)][[1L]]) ^ 2L

  # Regression Save Validation Data to File----
  if(SaveModelObjects) {
    if(!TrainOnFull) {
      if(!is.null(metadata_path)) {
        data.table::fwrite(ValidationData, file = file.path(normalizePath(metadata_path), paste0(ModelID, "_ValidationData.csv")))
      } else {
        data.table::fwrite(ValidationData, file = file.path(normalizePath(model_path), paste0(ModelID, "_ValidationData.csv")))
      }
    } else {
      if(!is.null(metadata_path)) {
        data.table::fwrite(ValidationData, file = file.path(normalizePath(metadata_path), paste0(ModelID, "_FullDataPredictions.csv")))
      } else {
        data.table::fwrite(ValidationData, file = file.path(normalizePath(model_path), paste0(ModelID, "_FullDataPredictions.csv")))
      }
    }
  }

  # Regression Evaluation Calibration Plot----
  if(!TrainOnFull) {
    EvaluationPlot <- EvalPlot(
      data = ValidationData,
      PredictionColName = "Predict",
      TargetColName = Target,
      GraphType = "calibration",
      PercentileBucket = 0.05,
      aggrfun = function(x) mean(x, na.rm = TRUE))

    # Regression Evaluation Plot Update Title----
    if(GridTune) {
      val <- max(GridModelEval, BaseModelEval)
      EvaluationPlot <- EvaluationPlot + ggplot2::ggtitle(paste0("Random Forest Calibration Evaluation Plot: ", toupper(eval_metric), " = ", round(val, 3L)))
    } else {
      EvaluationPlot <- EvaluationPlot + ggplot2::ggtitle(paste0("Calibration Evaluation Plot: ", toupper(eval_metric), " = ", round(BaseModelEval, 3L)))
    }

    # Save plot to file
    if(SaveModelObjects) {
      if(!is.null(metadata_path)) {
        ggplot2::ggsave(file.path(normalizePath(metadata_path), paste0(ModelID, "_EvaluationPlot.png")))
      } else {
        ggplot2::ggsave(file.path(normalizePath(model_path), paste0(ModelID, "_EvaluationPlot.png")))
      }
    }

    # Regression Evaluation BoxPlot----
    EvaluationBoxPlot <- EvalPlot(
      data = ValidationData,
      PredictionColName = "Predict",
      TargetColName = Target,
      GraphType = "boxplot",
      PercentileBucket = 0.05,
      aggrfun = function(x) mean(x, na.rm = TRUE))

    # Regression Evaluation Plot Update Title----
    if(GridTune) {
      val <- max(GridModelEval, BaseModelEval)
      EvaluationBoxPlot <- EvaluationBoxPlot + ggplot2::ggtitle(paste0("Random Forest Calibration Evaluation Plot: ", toupper(eval_metric), " = ", round(val, 3L)))
    } else {
      EvaluationBoxPlot <- EvaluationBoxPlot + ggplot2::ggtitle(paste0("Random Forest Calibration Evaluation Plot: ", toupper(eval_metric), " = ", round(BaseModelEval, 3L)))
    }

    # Save plot to file
    if(SaveModelObjects) {
      if(!is.null(metadata_path)) {
        ggplot2::ggsave(file.path(normalizePath(metadata_path), paste0(ModelID, "_EvaluationBoxPlot.png")))
      } else {
        ggplot2::ggsave(file.path(normalizePath(model_path), paste0(ModelID, "_EvaluationBoxPlot.png")))
      }
    }

    # Regression Evaluation Metrics----
    EvaluationMetrics <- data.table::data.table(Metric = c("MAE","MAPE","RMSE","R2"), MetricValue = rep(999999, 8L))
    i <- 0L
    for(metric in c("mae", "mape", "rmse", "r2")) {
      i <- i + 1L
      tryCatch({
        if(tolower(metric) == "mae") {
          ValidationData[, Metric := abs(get(TargetColumnName) - Predict)]
          Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
        } else if (tolower(metric) == "mape") {
          ValidationData[, Metric := abs((get(TargetColumnName) - Predict) / (get(TargetColumnName) + 1))]
          Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
        } else if (tolower(metric) == "rmse") {
          ValidationData[, Metric := (get(TargetColumnName) - Predict) ^ 2L]
          Metric <- sqrt(ValidationData[, mean(Metric, na.rm = TRUE)])
        } else if (tolower(metric) == "r2") {
          Metric <- (ValidationData[, stats::cor(get(TargetColumnName), Predict)][[1L]]) ^ 2L
        }
        data.table::set(EvaluationMetrics, i = i, j = 2L, value = round(Metric, 4L))
      }, error = function(x) "skip")
    }

    # Remove Features----
    ValidationData[, ':=' (Metric  = NULL)]

    # Regression Save EvaluationMetrics to File----
    EvaluationMetrics <- EvaluationMetrics[MetricValue != 999999]
    if(SaveModelObjects) {
      if(!is.null(metadata_path)) {
        data.table::fwrite(EvaluationMetrics, file = file.path(normalizePath(model_path), paste0(ModelID, "_EvaluationMetrics.csv")))
      } else {
        data.table::fwrite(EvaluationMetrics, file = file.path(normalizePath(model_path), paste0(ModelID, "_EvaluationMetrics.csv")))
      }
    }

    # Regression Partial Dependence----
    ParDepPlots <- list()
    ParDepBoxPlots <- list()
    if(NumOfParDepPlots != 0L) {
      j <- 0L
      k <- 0L
      for(i in seq_len(min(length(FeatureColNames), NumOfParDepPlots, VariableImportance[,.N]))) {
        tryCatch({
          Out <- ParDepCalPlots(
            data = ValidationData,
            PredictionColName = "Predict",
            TargetColName = Target,
            IndepVar = gsub("\\..*","",VariableImportance[i, Variable]),
            GraphType = "calibration",
            PercentileBucket = 0.05,
            FactLevels = 10L,
            Function = function(x) mean(x, na.rm = TRUE))
          j <- j + 1L
          ParDepPlots[[paste0(VariableImportance[j, Variable])]] <- Out
        }, error = function(x) "skip")
        tryCatch({
          Out1 <- ParDepCalPlots(
            data = ValidationData,
            PredictionColName = "Predict",
            TargetColName = Target,
            IndepVar = gsub("\\..*","",VariableImportance[i, Variable]),
            GraphType = "boxplot",
            PercentileBucket = 0.05,
            FactLevels = 10L,
            Function = function(x) mean(x, na.rm = TRUE))
          k <- k + 1L
          ParDepBoxPlots[[paste0(VariableImportance[k, Variable])]] <- Out1
        }, error = function(x) "skip")
      }

      # Regression Save ParDepPlots to file----
      if(SaveModelObjects) {
        if(!is.null(metadata_path)) {
          save(ParDepPlots, file = file.path(normalizePath(metadata_path), paste0(ModelID, "_ParDepPlots.R")))
        } else {
          save(ParDepPlots, file = file.path(normalizePath(model_path), paste0(ModelID, "_ParDepPlots.R")))
        }
      }

      # Regression Save ParDepBoxPlots to file----
      if(SaveModelObjects) {
        if(!is.null(metadata_path)) {
          save(ParDepBoxPlots, file = file.path(normalizePath(metadata_path), paste0(ModelID, "_ParDepBoxPlots.R")))
        } else {
          save(ParDepBoxPlots, file = file.path(normalizePath(model_path), paste0(ModelID, "_ParDepBoxPlots.R")))
        }
      }
    }
  }

  # Subset Transformation Object----
  if(!is.null(TransformNumericColumns)) {
    if(TargetColumnName == "Target") {
      TransformationResults <- TransformationResults[!(ColumnName %chin% c("Predict"))]
    } else {
      TransformationResults <- TransformationResults[!(ColumnName %chin% c("Predict", "Target"))]
    }
  }

  # VI_Plot_Function
  VI_Plot <- function(VI_Data, ColorHigh = "darkblue", ColorLow = "white") {
    ggplot2::ggplot(VI_Data[1L:min(10L, .N)], ggplot2::aes(x = reorder(Variable, Importance), y = Importance, fill = Importance)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::scale_fill_gradient2(mid = ColorLow,high = ColorHigh) +
      ChartTheme(Size = 12L, AngleX = 0L, LegendPosition = "right") +
      ggplot2::coord_flip() +
      ggplot2::labs(title = "Global Variable Importance") +
      ggplot2::xlab("Top Model Features") +
      ggplot2::ylab("Value")
  }

  # Regression Return Objects----
  if(ReturnModelObjects) {
    if(!TrainOnFull) {
      return(list(
        Model = FinalModel,
        ValidationData = ValidationData,
        EvaluationPlot = EvaluationPlot,
        EvaluationBoxPlot = EvaluationBoxPlot,
        EvaluationMetrics = EvaluationMetrics,
        VariableImportance = VariableImportance,
        VI_Plot = VI_Plot(VI_Data = VariableImportance),
        PartialDependencePlots = ParDepPlots,
        PartialDependenceBoxPlots = ParDepBoxPlots,
        TransformationInformation = if(!is.null(TransformNumericColumns)) TransformationResults else NULL,
        ColNames = Names))
    } else {
      return(list(
        Model = FinalModel,
        ValidationData = ValidationData,
        TransformationInformation = if(!is.null(TransformNumericColumns)) TransformationResults else NULL,
        ColNames = Names))
    }
  }
}
