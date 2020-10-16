#' AutoH2oGAMClassifier is an automated H2O modeling framework with grid-tuning and model evaluation
#'
#' AutoH2oGAMClassifier is an automated H2O modeling framework with grid-tuning and model evaluation that runs a variety of steps. First, a stratified sampling (by the target variable) is done to create train and validation sets. Then, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation plot, evaluation metrics, variable importance, partial dependence calibration plots, and column names used in model fitting.
#' @author Adrian Antico
#' @family Automated Supervised Learning - Binary Classification
#' @param data This is your data set for training and testing your model
#' @param TrainOnFull Set to TRUE to train on full data
#' @param ValidationData This is your holdout data set used in modeling either refine your hyperparameters.
#' @param TestData This is your holdout data set. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TargetColumnName Either supply the target column name OR the column number where the target is located (but not mixed types). Note that the target column needs to be a 0 | 1 numeric variable.
#' @param FeatureColNames Either supply the feature column names OR the column number where the target is located (but not mixed types)
#' @param GamColNames GAM column names. Up to 9 features
#' @param Distribution "binomial", "quasibinomial"
#' @param link identity, logit, log, inverse, tweedie
#' @param eval_metric This is the metric used to identify best grid tuned model. Choose from "AUC" or "logloss"
#' @param GridTune Set to TRUE to run a grid tuning procedure. Set a number in MaxModelsInGrid to tell the procedure how many models you want to test.
#' @param MaxMem Set the maximum amount of memory you'd like to dedicate to the model run. E.g. "32G"
#' @param NThreads Set the number of threads you want to dedicate to the model building
#' @param MaxModelsInGrid Number of models to test from grid options (1080 total possible options)
#' @param model_path A character string of your path file to where you want your output saved
#' @param metadata_path A character string of your path file to where you want your model evaluation output saved. If left NULL, all output will be saved to model_path.
#' @param ModelID A character string to name your model and output
#' @param NumOfParDepPlots Tell the function the number of partial dependence calibration plots you want to create.
#' @param ReturnModelObjects Set to TRUE to output all modeling objects (E.g. plots and evaluation metrics)
#' @param SaveModelObjects Set to TRUE to return all modeling objects to your environment
#' @param IfSaveModel Set to "mojo" to save a mojo file, otherwise "standard" to save a regular H2O model object
#' @param H2OShutdown Set to TRUE to shutdown H2O after running the function
#' @param HurdleModel Set to FALSE
#' @examples
#' \donttest{
#' # Create some dummy correlated data with numeric and categorical features
#' data <- RemixAutoML::FakeDataGenerator(
#'   Correlation = 0.85,
#'   N = 1000L,
#'   ID = 2L,
#'   ZIP = 0L,
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
#'    data,
#'    TrainOnFull = FALSE,
#'    ValidationData = NULL,
#'    TestData = NULL,
#'    TargetColumnName = "Adrian",
#'    FeatureColNames = names(data)[!names(data) %chin%
#'      c("IDcol_1", "IDcol_2","Adrian")],
#'    GamColNames = GamCols,
#'    Distribution = "binomial",
#'    link = "logit",
#'    eval_metric = "auc",
#'    GridTune = FALSE,
#'    MaxMem = "32G",
#'    NThreads = max(1, parallel::detectCores()-2),
#'    MaxModelsInGrid = 10,
#'    model_path = NULL,
#'    metadata_path = NULL,
#'    ModelID = "FirstModel",
#'    NumOfParDepPlots = 3,
#'    ReturnModelObjects = TRUE,
#'    SaveModelObjects = FALSE,
#'    IfSaveModel = "mojo",
#'    H2OShutdown = FALSE,
#'    HurdleModel = FALSE)
#' }
#' @return Saves to file and returned in list: VariableImportance.csv, Model, ValidationData.csv, EvalutionPlot.png, EvaluationMetrics.csv, ParDepPlots.R a named list of features with partial dependence calibration plots, GridCollect, and GridList
#' @export
AutoH2oGAMClassifier <- function(data,
                                 TrainOnFull = FALSE,
                                 ValidationData = NULL,
                                 TestData = NULL,
                                 TargetColumnName = NULL,
                                 FeatureColNames = NULL,
                                 GamColNames = NULL,
                                 Distribution = "binomial",
                                 link = "logit",
                                 eval_metric = "auc",
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
                                 H2OShutdown = FALSE,
                                 HurdleModel = FALSE) {

  # data.table optimize----
  if(parallel::detectCores() > 10) data.table::setDTthreads(threads = max(1L, parallel::detectCores() - 2L)) else data.table::setDTthreads(threads = max(1L, parallel::detectCores()))

  # Ensure model_path and metadata_path exists----
  if(!is.null(model_path)) if(!dir.exists(file.path(normalizePath(model_path)))) dir.create(normalizePath(model_path))
  if(!is.null(metadata_path)) if(!is.null(metadata_path)) if(!dir.exists(file.path(normalizePath(metadata_path)))) dir.create(normalizePath(metadata_path))

  # Binary Check Arguments----
  if(!(tolower(eval_metric) %chin% c("auc", "logloss"))) return("eval_metric not in AUC, logloss")
  if(!GridTune %in% c(TRUE, FALSE)) return("GridTune needs to be TRUE or FALSE")
  if(MaxModelsInGrid < 1 & GridTune) return("MaxModelsInGrid needs to be at least 1")
  if(!is.null(model_path)) if(!is.character(model_path)) return("model_path needs to be a character type")
  if(!is.null(metadata_path)) if(!is.character(metadata_path)) return("metadata_path needs to be a character type")
  if(!is.character(ModelID) & !is.null(ModelID)) return("ModelID needs to be a character type")
  if(NumOfParDepPlots < 0) return("NumOfParDepPlots needs to be a positive number")
  if(!(ReturnModelObjects %in% c(TRUE, FALSE))) return("ReturnModelObjects needs to be TRUE or FALSE")
  if(!(SaveModelObjects %in% c(TRUE, FALSE))) return("SaveModelObjects needs to be TRUE or FALSE")
  if(!(tolower(eval_metric) == "auc")) eval_metric <- tolower(eval_metric) else eval_metric <- toupper(eval_metric)
  if(tolower(eval_metric) %chin% c("auc")) Decreasing <- TRUE else Decreasing <- FALSE

  # Binary Target Name Storage----
  if(!is.character(TargetColumnName)) TargetColumnName <- names(data)[TargetColumnName]

  # Binary Ensure data is a data.table----
  if(!data.table::is.data.table(data)) data.table::setDT(data)
  if(!is.null(ValidationData)) if(!data.table::is.data.table(ValidationData)) data.table::setDT(ValidationData)
  if(!is.null(TestData)) if(!data.table::is.data.table(TestData)) data.table::setDT(TestData)

  # Ensure Target is a factor ----
  if(!is.factor(data[[eval(TargetColumnName)]])) {
    data[, eval(TargetColumnName) := as.factor(get(TargetColumnName))]
    if(!is.null(ValidationData)) ValidationData[, eval(TargetColumnName) := as.factor(get(TargetColumnName))]
    if(!is.null(TestData)) TestData[, eval(TargetColumnName) := as.factor(get(TargetColumnName))]
  }

  # Binary Data Partition----
  if(is.null(ValidationData) & is.null(TestData) & !TrainOnFull) {
    dataSets <- AutoDataPartition(
      data,
      NumDataSets = 3L,
      Ratios = c(0.70, 0.20, 0.10),
      PartitionType = "random",
      StratifyColumnNames = TargetColumnName,
      TimeColumnName = NULL)
    data <- dataSets$TrainData
    ValidationData <- dataSets$ValidationData
    TestData <- dataSets$TestData
  }

  # Binary ModelDataPrep----
  dataTrain <- ModelDataPrep(data = data, Impute = FALSE, CharToFactor = TRUE)
  if(!TrainOnFull) dataTest <- ModelDataPrep(data = ValidationData, Impute = FALSE, CharToFactor = TRUE)
  if(!is.null(TestData)) TestData <- ModelDataPrep(data = TestData, Impute = FALSE, CharToFactor = TRUE)

  # Binary Save Names of data----
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

  # Binary Grid Tune Check----
  if(GridTune & !TrainOnFull) {
    if(!HurdleModel) h2o::h2o.init(max_mem_size = MaxMem, nthreads = NThreads, enable_assertions = FALSE)
    datatrain <- h2o::as.h2o(dataTrain)
    datavalidate <- h2o::as.h2o(dataTest)

    # Binary Grid Tune Search Criteria----
    search_criteria <- list(
      strategy             = "RandomDiscrete",
      max_runtime_secs     = 3600 * 24 * 7,
      max_models           = MaxModelsInGrid,
      seed                 = 1234,
      stopping_rounds      = 10,
      stopping_metric      = eval_metric,
      stopping_tolerance   = 1e-3)

    # Binary Grid Parameters----
    hyper_params <- list(
      alpha = c(0,0.25,0.5,0.75,1),
      lambda = c(0,0.01,0.05,0.10),
      theta = c(1e-10, 0.01, 0.05, 0.10))

    # Link----
    if(!is.null(link)) Link <- link else Link <- "logit"

    # Binary Grid Train Model----
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
      link                 = Link)

    # Binary Get Best Model----
    Grid_Out   <- h2o::h2o.getGrid(grid_id = paste0(ModelID, "_Grid"), sort_by = eval_metric, decreasing = Decreasing)

    # Binary Collect Best Grid Model----
    grid_model <- h2o::h2o.getModel(Grid_Out@model_ids[[1L]])
  }

  # Binary Start Up H2O----
  if(!GridTune) {
    if(!HurdleModel) h2o::h2o.init(max_mem_size = MaxMem, nthreads = NThreads, enable_assertions = FALSE)
    datatrain <- h2o::as.h2o(dataTrain)
    if(!TrainOnFull) datavalidate <- h2o::as.h2o(dataTest)
  }

  # Define link----
  if(!GridTune) if(!is.null(link)) Link <- link else Link <- "logit"

  # Binary Build Baseline Model----
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

  # Binary Get Metrics----
  if(GridTune & !TrainOnFull) {
    if(!is.null(TestData)) {
      datatest <- h2o::as.h2o(TestData)
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

  # Binary Evaluate Metrics----
  if(!is.numeric(data[[eval(TargetColumnName)]])) {
    if(GridTune & !TrainOnFull) {
      if(tolower(eval_metric) == "auc") {
        BaseMetric <- BaseMetrics@metrics$AUC
        GridMetric <- GridMetrics@metrics$AUC
        if(GridMetric > BaseMetric) {
          FinalModel <- grid_model
          EvalMetric <- GridMetric
          FinalThresholdTable <- data.table::as.data.table(GridMetrics@metrics$max_criteria_and_metric_scores)
          data.table::setnames(FinalThresholdTable,c("metric", "threshold", "value"), c("Metric", "Threshold", "Value"))
          FinalThresholdTable[, idx := NULL]
          FinalThresholdTable[, ':=' (Threshold = round(Threshold, 4L), Value = round(Value, 4L))]
        } else {
          FinalModel <- base_model
          EvalMetric <- BaseMetric
          FinalThresholdTable <- data.table::as.data.table(BaseMetrics@metrics$max_criteria_and_metric_scores)
          data.table::setnames(FinalThresholdTable,c("metric", "threshold", "value"),c("Metric", "Threshold", "Value"))
          FinalThresholdTable[, idx := NULL]
          FinalThresholdTable[, ':=' (Threshold = round(Threshold, 4L),Value = round(Value, 4L))]
        }
      } else if (tolower(eval_metric) == "logloss") {
        BaseMetric <- BaseMetrics@metrics$logloss
        GridMetric <- GridMetrics@metrics$logloss
        if(GridMetric < BaseMetric) {
          FinalModel <- grid_model
          EvalMetric <- GridMetric
          FinalThresholdTable <- data.table::as.data.table(GridMetrics@metrics$max_criteria_and_metric_scores)
          data.table::setnames(FinalThresholdTable,c("metric", "threshold", "value"),c("Metric", "Threshold", "Value"))
          FinalThresholdTable[, idx := NULL]
          FinalThresholdTable[, ':=' (Threshold = round(Threshold, 4L),Value = round(Value, 4L))]
        } else {
          FinalModel <- base_model
          EvalMetric <- BaseMetric
          FinalThresholdTable <- data.table::as.data.table(BaseMetrics@metrics$max_criteria_and_metric_scores)
          data.table::setnames(FinalThresholdTable,c("metric", "threshold", "value"),c("Metric", "Threshold", "Value"))
          FinalThresholdTable[, idx := NULL]
          FinalThresholdTable[, ':=' (Threshold = round(Threshold, 4L),Value = round(Value, 4L))]
        }
      }
    } else {
      if(tolower(eval_metric) == "auc") {
        BaseMetric <- BaseMetrics@metrics$AUC
        FinalModel <- base_model
        EvalMetric <- BaseMetric
        FinalThresholdTable <- data.table::as.data.table(BaseMetrics@metrics$max_criteria_and_metric_scores)
        data.table::setnames(FinalThresholdTable,c("metric", "threshold", "value"),c("Metric", "Threshold", "Value"))
        FinalThresholdTable[, idx := NULL]
        FinalThresholdTable[, ':=' (Threshold = round(Threshold, 4L),Value = round(Value, 4L))]
      } else {
        BaseMetric <- BaseMetrics@metrics$logloss
        FinalModel <- base_model
        EvalMetric <- BaseMetric
        FinalThresholdTable <- data.table::as.data.table(BaseMetrics@metrics$max_criteria_and_metric_scores)
        data.table::setnames(FinalThresholdTable,c("metric", "threshold", "value"),c("Metric", "Threshold", "Value"))
        FinalThresholdTable[, idx := NULL]
        FinalThresholdTable[, ':=' (Threshold = round(Threshold, 4L),Value = round(Value, 4L))]
      }
    }
  } else {
    FinalModel <- base_model
  }

  # Binary Save Final Model----
  if(SaveModelObjects) {
    if(tolower(IfSaveModel) == "mojo") {
      SaveModel <- h2o::h2o.saveMojo(object = FinalModel, path = model_path,force = TRUE)
      h2o::h2o.download_mojo(
        model = FinalModel,
        path = model_path,
        get_genmodel_jar = TRUE,
        genmodel_path = model_path,
        genmodel_name = ModelID)
    } else {
      SaveModel <- h2o::h2o.saveModel(object = FinalModel, path = model_path,force = TRUE)
    }
  }

  # Binary Score Final Test Data----
  if(!is.numeric(data[[eval(TargetColumnName)]])) {
    if(!is.null(TestData)) {
      Predict <- data.table::as.data.table(h2o::h2o.predict(object = FinalModel,newdata = datatest))
      Predict[, p0 := NULL]
    } else if(!TrainOnFull) {
      Predict <- data.table::as.data.table(h2o::h2o.predict(object = FinalModel,newdata = datavalidate))
      Predict[, p0 := NULL]
    } else {
      Predict <- data.table::as.data.table(h2o::h2o.predict(object = FinalModel,newdata = datatrain))
      Predict[, p0 := NULL]
    }
  } else {
    if(!is.null(TestData)) {
      Predict <- data.table::as.data.table(h2o::h2o.predict(object = FinalModel,newdata = datatest))
      data.table::setnames(Predict, "predict", "predict")
    } else if(!TrainOnFull) {
      Predict <- data.table::as.data.table(h2o::h2o.predict(object = FinalModel,newdata = datavalidate))
      data.table::setnames(Predict, "predict", "predict")
    } else {
      Predict <- data.table::as.data.table(h2o::h2o.predict(object = FinalModel,newdata = datatrain))
      data.table::setnames(Predict, "predict", "predict")
    }
  }

  # Binary Variable Importance----
  VariableImportance <- data.table::as.data.table(h2o::h2o.varimp(object = FinalModel))

  # Binary Format Variable Importance Table----
  data.table::setnames(VariableImportance, c("variable","relative_importance","scaled_importance","percentage"), c("Variable","RelativeImportance","ScaledImportance","Percentage"))
  VariableImportance[, ':=' (
    RelativeImportance = round(RelativeImportance, 4L),
    ScaledImportance = round(ScaledImportance, 4L),
    Percentage = round(Percentage, 4L))]

  # Binary Save Variable Importance----
  if(SaveModelObjects) {
    if(!is.null(metadata_path)) {
      data.table::fwrite(VariableImportance, file = file.path(normalizePath(metadata_path), paste0(ModelID, "_VariableImportance.csv")))
    } else {
      data.table::fwrite(VariableImportance, file = file.path(normalizePath(model_path), paste0(ModelID, "_VariableImportance.csv")))
    }
  }

  # Binary H2O Shutdown----
  if(H2OShutdown) h2o::h2o.shutdown(prompt = FALSE)

  # Binary Create Validation Data----
  if(!is.null(TestData)) {
    ValidationData <- data.table::as.data.table(cbind(TestData, Predict))
  } else if(!TrainOnFull) {
    ValidationData <- data.table::as.data.table(cbind(dataTest, Predict))
  } else {
    ValidationData <- data.table::as.data.table(cbind(dataTrain, Predict))
  }

  # Binary Change Prediction Name----
  data.table::setnames(ValidationData, "predict", "Predict")

  # Binary Save Validation Data to File----
  if(SaveModelObjects) {
    if(!is.null(metadata_path)) {
      data.table::fwrite(ValidationData, file = file.path(normalizePath(metadata_path), paste0(ModelID, "_ValidationData.csv")))
    } else {
      data.table::fwrite(ValidationData, file = file.path(normalizePath(model_path), paste0(ModelID, "_ValidationData.csv")))
    }
  }

  # Binary Evaluation Calibration Plot----
  if(!is.numeric(data[[eval(TargetColumnName)]])) {
    EvaluationPlot <- EvalPlot(
      data = ValidationData,
      PredictionColName = "p1",
      TargetColName = TargetColumnName,
      GraphType = "calibration",
      PercentileBucket = 0.05,
      aggrfun = function(x) mean(x, na.rm = TRUE))
  } else {
    EvaluationPlot <- EvalPlot(
      data = ValidationData,
      PredictionColName = "Predict",
      TargetColName = TargetColumnName,
      GraphType = "calibration",
      PercentileBucket = 0.05,
      aggrfun = function(x) mean(x, na.rm = TRUE))
  }

  # Binary Evaluation Plot Update Title----
  if(!is.numeric(data[[eval(TargetColumnName)]])) {
    if(GridTune) {
      EvaluationPlot <- EvaluationPlot + ggplot2::ggtitle(paste0("GLM Calibration Evaluation Plot: ", toupper(eval_metric)," = ", round(EvalMetric, 3L)))
    } else {
      EvaluationPlot <- EvaluationPlot + ggplot2::ggtitle(paste0("Calibration Evaluation Plot: ", toupper(eval_metric)," = ", round(EvalMetric, 3L)))
    }
  }

  # Binary Save plot to file----
  if(SaveModelObjects) {
    if(!is.null(metadata_path)) {
      ggplot2::ggsave(file.path(normalizePath(metadata_path), paste0(ModelID,"_EvaluationPlot.png")))
    } else {
      ggplot2::ggsave(file.path(normalizePath(model_path), paste0(ModelID,"_EvaluationPlot.png")))
    }
  }

  # Binary AUC Object Create----
  temp <- ValidationData[order(runif(ValidationData[,.N]))][1L:min(100000L, ValidationData[,.N])]
  if(!is.numeric(data[[eval(TargetColumnName)]])) {
    AUC_Metrics <- pROC::roc(response = temp[[eval(TargetColumnName)]],
                             predictor = temp[["p1"]],
                             na.rm = TRUE,
                             algorithm = 3L,
                             auc = TRUE,
                             ci = TRUE)
  } else {
    AUC_Metrics <- pROC::roc(response = temp[[eval(TargetColumnName)]],
                             predictor = temp[["Predict"]],
                             na.rm = TRUE,
                             algorithm = 3L,
                             auc = TRUE,
                             ci = TRUE)
  }
  rm(temp)

  # Binary AUC Conversion to data.table----
  AUC_Data <- data.table::data.table(
    ModelNumber = 0,
    Sensitivity = AUC_Metrics$sensitivities,
    Specificity = AUC_Metrics$specificities)

  # Binary Plot ROC Curve----
  ROC_Plot <- ggplot2::ggplot(AUC_Data, ggplot2::aes(x = 1 - Specificity)) +
    ggplot2::geom_line(ggplot2::aes(y = AUC_Data[["Sensitivity"]]), color = "blue") +
    ggplot2::geom_abline(slope = 1, color = "black") +
    ggplot2::ggtitle(paste0("GLM AUC: ", 100 * round(AUC_Metrics$auc, 3), "%")) +
    ChartTheme() + ggplot2::xlab("Specificity") +
    ggplot2::ylab("Sensitivity")

  # Save plot to file----
  if(SaveModelObjects) {
    if(!is.null(metadata_path)) {
      ggplot2::ggsave(file.path(normalizePath(metadata_path), paste0(ModelID, "_ROC_Plot.png")))
    } else {
      ggplot2::ggsave(file.path(normalizePath(model_path), paste0(ModelID, "_ROC_Plot.png")))
    }
  }

  # Binary Save EvaluationMetrics to File----
  if(exists("FinalThresholdTable")) {
    if(SaveModelObjects) {
      if(!is.null(metadata_path)) {
        data.table::fwrite(FinalThresholdTable, file = file.path(normalizePath(metadata_path), paste0(ModelID, "_EvaluationMetrics.csv")))
      } else {
        data.table::fwrite(FinalThresholdTable, file = file.path(normalizePath(model_path), paste0(ModelID, "_EvaluationMetrics.csv")))
      }
    }
  }

  # Binary Partial Dependence----
  ParDepPlots <- list()
  j <- 0L
  if(!is.numeric(data[[eval(TargetColumnName)]])) {
    for(i in seq_len(min(length(FeatureColNames), NumOfParDepPlots, VariableImportance[,.N]))) {
      tryCatch({
        Out <- ParDepCalPlots(
          data = ValidationData,
          PredictionColName = "p1",
          TargetColName = TargetColumnName,
          IndepVar = gsub("\\..*","",VariableImportance[i, Variable]),
          GraphType = "calibration",
          PercentileBucket = 0.05,
          FactLevels = 10L,
          Function = function(x) mean(x, na.rm = TRUE))
        j <- j + 1L
        ParDepPlots[[paste0(VariableImportance[j, Variable])]] <- Out
      }, error = function(x) "skip")
    }
  } else {
    for(i in seq_len(min(length(FeatureColNames), NumOfParDepPlots, VariableImportance[,.N]))) {
      tryCatch({
        Out <- ParDepCalPlots(
          data = ValidationData,
          PredictionColName = "Predict",
          TargetColName = TargetColumnName,
          IndepVar = gsub("\\..*","",VariableImportance[i, Variable]),
          GraphType = "calibration",
          PercentileBucket = 0.05,
          FactLevels = 10L,
          Function = function(x) mean(x, na.rm = TRUE))
        j <- j + 1L
        ParDepPlots[[paste0(VariableImportance[j, Variable])]] <- Out
      }, error = function(x) "skip")
    }
  }

  # Binary Save ParDepPlots to file----
  if(SaveModelObjects) {
    if(!is.null(metadata_path)) {
      save(ParDepPlots, file = file.path(normalizePath(metadata_path), paste0(ModelID, "_ParDepPlots.R")))
    } else {
      save(ParDepPlots, file = file.path(normalizePath(model_path), paste0(ModelID, "_ParDepPlots.R")))
    }
  }

  # VI_Plot_Function
  VI_Plot <- function(VI_Data, ColorHigh = "darkblue", ColorLow = "white") {
    ggplot2::ggplot(VI_Data[1:min(10,.N)], ggplot2::aes(x = reorder(Variable, Percentage), y = Percentage, fill = Percentage)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::scale_fill_gradient2(mid = ColorLow,high = ColorHigh) +
      ChartTheme(Size = 12L, AngleX = 0L, LegendPosition = "right") +
      ggplot2::coord_flip() +
      ggplot2::labs(title = "Global Variable Importance") +
      ggplot2::xlab("Top Model Features") +
      ggplot2::ylab("Value")
  }

  # Binary Return Objects----
  if(ReturnModelObjects) {
    if(!is.numeric(data[[eval(TargetColumnName)]])) {
      return(list(
        Model = FinalModel,
        ValidationData = ValidationData,
        ROC_Plot = ROC_Plot,
        EvaluationPlot = EvaluationPlot,
        EvaluationMetrics = FinalThresholdTable,
        VariableImportance = VariableImportance,
        VI_Plot = VI_Plot(VI_Data = VariableImportance),
        PartialDependencePlots = ParDepPlots,
        ColNames = Names))
    } else {
      return(list(
        Model = FinalModel,
        ValidationData = ValidationData,
        ROC_Plot = ROC_Plot,
        EvaluationPlot = EvaluationPlot,
        EvaluationMetrics = NULL,
        VariableImportance = VariableImportance,
        VI_Plot = VI_Plot(VI_Data = VariableImportance),
        PartialDependencePlots = ParDepPlots,
        ColNames = Names))
    }
  }
}