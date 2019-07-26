#' AutoH2oDRFRegression is an automated H2O modeling framework with grid-tuning and model evaluation
#'
#' AutoH2oDRFRegression is an automated H2O modeling framework with grid-tuning and model evaluation that runs a variety of steps. First, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation plot, evaluation boxplot, evaluation metrics, variable importance, partial dependence calibration plots, partial dependence calibration box plots, and column names used in model fitting.
#' @author Adrian Antico
#' @family Supervised Learning
#' @param data This is your data set for training and testing your model
#' @param ValidationData This is your holdout data set used in modeling either refine your hyperparameters.
#' @param TestData This is your holdout data set. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TargetColumnName Either supply the target column name OR the column number where the target is located (but not mixed types).
#' @param FeatureColNames Either supply the feature column names OR the column number where the target is located (but not mixed types)
#' @param TransformNumericColumns Set to NULL to do nothing; otherwise supply the column names of numeric variables you want transformed
#' @param eval_metric This is the metric used to identify best grid tuned model. Choose from "MSE", "RMSE", "MAE", "RMSLE"
#' @param Trees The maximum number of trees you want in your models
#' @param GridTune Set to TRUE to run a grid tuning procedure. Set a number in MaxModelsInGrid to tell the procedure how many models you want to test.
#' @param MaxMem Set the maximum amount of memory you'd like to dedicate to the model run. E.g. "32G"
#' @param NThreads Set the number of threads you want to dedicate to the model building
#' @param MaxModelsInGrid Number of models to test from grid options (1080 total possible options)
#' @param model_path A character string of your path file to where you want your output saved
#' @param ModelID A character string to name your model and output
#' @param NumOfParDepPlots Tell the function the number of partial dependence calibration plots you want to create. Calibration boxplots will only be created for numerical features (not dummy variables)
#' @param ReturnModelObjects Set to TRUE to output all modeling objects (E.g. plots and evaluation metrics)
#' @param SaveModelObjects Set to TRUE to return all modeling objects to your environment
#' @param IfSaveModel Set to "mojo" to save a mojo file, otherwise "standard" to save a regular H2O model object
#' @param StopH2O For use in other functions.
#' @examples
#' \donttest{
#' Correl <- 0.85
#' N <- 1000
#' data <- data.table::data.table(Target = runif(N))
#' data[, x1 := qnorm(Target)]
#' data[, x2 := runif(N)]
#' data[, Independent_Variable1 := log(pnorm(Correl * x1 +
#'                                             sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable2 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable3 := exp(pnorm(Correl * x1 +
#'                                             sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable4 := exp(exp(pnorm(Correl * x1 +
#'                                                 sqrt(1-Correl^2) * qnorm(x2))))]
#' data[, Independent_Variable5 := sqrt(pnorm(Correl * x1 +
#'                                              sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable6 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^0.10]
#' data[, Independent_Variable7 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^0.25]
#' data[, Independent_Variable8 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^0.75]
#' data[, Independent_Variable9 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^2]
#' data[, Independent_Variable10 := (pnorm(Correl * x1 +
#'                                           sqrt(1-Correl^2) * qnorm(x2)))^4]
#' data[, Independent_Variable11 := as.factor(
#'   ifelse(Independent_Variable2 < 0.20, "A",
#'          ifelse(Independent_Variable2 < 0.40, "B",
#'                 ifelse(Independent_Variable2 < 0.6,  "C",
#'                        ifelse(Independent_Variable2 < 0.8,  "D", "E")))))]
#' data[, ':=' (x1 = NULL, x2 = NULL)]
#' TestModel <- AutoH2oDRFRegression(data,
#'                                   ValidationData = NULL,
#'                                   TestData = NULL,
#'                                   TargetColumnName = "Target",
#'                                   FeatureColNames = 2:ncol(data),
#'                                   TransformNumericColumns = NULL,
#'                                   eval_metric = "RMSE",
#'                                   Trees = 50,
#'                                   GridTune = FALSE,
#'                                   MaxMem = "32G",
#'                                   NThreads = max(1, parallel::detectCores()-2),
#'                                   MaxModelsInGrid = 10,
#'                                   model_path = NULL,
#'                                   ModelID = "FirstModel",
#'                                   NumOfParDepPlots = 3,
#'                                   ReturnModelObjects = TRUE,
#'                                   SaveModelObjects = FALSE,
#'                                   IfSaveModel = "mojo",
#'                                   StopH2O = TRUE)
#' }
#' @return Saves to file and returned in list: VariableImportance.csv, Model, ValidationData.csv, EvalutionPlot.png, EvalutionBoxPlot.png, EvaluationMetrics.csv, ParDepPlots.R a named list of features with partial dependence calibration plots, ParDepBoxPlots.R, GridCollect, GridList, and Transformation metadata
#' @export
AutoH2oDRFRegression <- function(data,
                                 ValidationData = NULL,
                                 TestData = NULL,
                                 TargetColumnName = NULL,
                                 FeatureColNames = NULL,
                                 TransformNumericColumns = NULL,
                                 eval_metric = "RMSE",
                                 Trees = 50,
                                 GridTune = FALSE,
                                 MaxMem = "32G",
                                 NThreads = max(1, parallel::detectCores()-2),
                                 MaxModelsInGrid = 2,
                                 model_path = NULL,
                                 ModelID = "FirstModel",
                                 NumOfParDepPlots = 3,
                                 ReturnModelObjects = TRUE,
                                 SaveModelObjects = FALSE,
                                 IfSaveModel = "mojo",
                                 StopH2O = TRUE) {
  
  # Regression Check Arguments----
  if (!(tolower(eval_metric) %chin% c("mse", "rmse", "mae", "rmsle"))) {
    warning("eval_metric not in MSE, RMSE, MAE, RMSLE")
    
  }
  if (Trees < 1)
    warning("Trees must be greater than 1")
  if (!GridTune %in% c(TRUE, FALSE))
    warning("GridTune needs to be TRUE or FALSE")
  if (MaxModelsInGrid < 1 & GridTune == TRUE) {
    warning("MaxModelsInGrid needs to be at least 1")
  }
  if (!is.null(model_path)) {
    if (!is.character(model_path))
      warning("model_path needs to be a character type")
  }
  if (!is.character(ModelID))
    warning("ModelID needs to be a character type")
  if (NumOfParDepPlots < 0)
    warning("NumOfParDepPlots needs to be a positive number")
  if (!(ReturnModelObjects %in% c(TRUE, FALSE)))
    warning("ReturnModelObjects needs to be TRUE or FALSE")
  if (!(SaveModelObjects %in% c(TRUE, FALSE)))
    warning("SaveModelObjects needs to be TRUE or FALSE")
  
  # Regression Ensure data is a data.table----
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Regression Ensure data is a data.table----
  if (!is.null(ValidationData)) {
    if (!data.table::is.data.table(ValidationData)) {
      ValidationData <- data.table::as.data.table(ValidationData)
    }
  }
  
  # Regression Ensure data is a data.table----
  if (!is.null(TestData)) {
    if (!data.table::is.data.table(TestData)) {
      TestData <- data.table::as.data.table(TestData)
    }
  }
  
  # Convert TransformNumericColumns to Names if not character----
  if (!is.null(TransformNumericColumns)) {
    if(!is.character(TransformNumericColumns)) {
      TransformNumericColumns <- names(data)[TransformNumericColumns]      
    }
  }
  
  # Transform data, ValidationData, and TestData----
  if (!is.null(ValidationData) &
      !is.null(TransformNumericColumns)) {
    MeanTrainTarget <- data[, mean(get(TargetColumnName))]
    Output <- AutoTransformationCreate(
      data,
      ColumnNames = TransformNumericColumns,
      Methods = c("BoxCox",
                  "YeoJohnson",
                  "Asinh",
                  "Asin",
                  "Logit"),
      Path = model_path,
      TransID = ModelID,
      SaveOutput = SaveModelObjects
    )
    data <- Output$Data
    TransformationResults <- Output$FinalResults
    
    # Transform ValidationData----
    ValidationData <- AutoTransformationScore(
      ScoringData = ValidationData,
      Type = "Apply",
      FinalResults = TransformationResults,
      TransID = NULL,
      Path = NULL
    )
    
    # Transform TestData----
    if (!is.null(TestData)) {
      TestData <- AutoTransformationScore(
        ScoringData = TestData,
        Type = "Apply",
        FinalResults = TransformationResults,
        TransID = NULL,
        Path = NULL
      )
    }
  }
  
  # Regression Data Partition----
  if (is.null(ValidationData) & is.null(TestData)) {
    if (!is.null(TransformNumericColumns)) {
      # Partition----
      dataSets <- AutoDataPartition(
        data,
        NumDataSets = 3,
        Ratios = c(0.70, 0.20, 0.10),
        PartitionType = "random",
        StratifyColumnNames = NULL,
        TimeColumnName = NULL
      )
      data <- dataSets$TrainData
      ValidationData <- dataSets$ValidationData
      TestData <- dataSets$TestData
      
      # Mean of data----
      MeanTrainTarget <- data[, mean(get(TargetColumnName))]
      
      # Transform data sets----
      Output <- AutoTransformationCreate(
        data,
        ColumnNames = TransformNumericColumns,
        Methods = c("BoxCox",
                    "YeoJohnson",
                    "Asinh",
                    "Asin",
                    "Logit"),
        Path = model_path,
        TransID = ModelID,
        SaveOutput = SaveModelObjects
      )
      data <- Output$Data
      TransformationResults <- Output$FinalResults
      
      # Transform ValidationData----
      ValidationData <- AutoTransformationScore(
        ScoringData = ValidationData,
        Type = "Apply",
        FinalResults = TransformationResults,
        TransID = NULL,
        Path = NULL
      )
      
      # Transform TestData----
      if (!is.null(TestData)) {
        TestData <- AutoTransformationScore(
          ScoringData = TestData,
          Type = "Apply",
          FinalResults = TransformationResults,
          TransID = NULL,
          Path = NULL
        )
      }
    } else {
      dataSets <- AutoDataPartition(
        data,
        NumDataSets = 3,
        Ratios = c(0.70, 0.20, 0.10),
        PartitionType = "random",
        StratifyColumnNames = NULL,
        TimeColumnName = NULL
      )
      data <- dataSets$TrainData
      ValidationData <- dataSets$ValidationData
      TestData <- dataSets$TestData
      MeanTrainTarget <- data[, mean(get(TargetColumnName))]
    }
  }
  
  # Regression ModelDataPrep----
  dataTrain <- ModelDataPrep(data = data,
                             Impute = FALSE,
                             CharToFactor = TRUE)
  
  # Regression ModelDataPrep----
  dataTest <- ModelDataPrep(data = ValidationData,
                            Impute = FALSE,
                            CharToFactor = TRUE)
  
  # Regression ModelDataPrep----
  if (!is.null(TestData)) {
    TestData <- ModelDataPrep(data = TestData,
                              Impute = FALSE,
                              CharToFactor = TRUE)
  }
  
  # Regression Target Name Storage----
  if (is.character(TargetColumnName)) {
    Target <- TargetColumnName
  } else {
    Target <- names(data)[TargetColumnName]
  }
  
  # Regression Get Min Value of Target Data----
  MinVal <- min(data[[eval(Target)]], na.rm = TRUE)
  
  # Regression Grid Tune Check----
  if (GridTune) {
    # Regression Start Up H2O----
    h2o::h2o.init(max_mem_size = MaxMem,
                  nthreads = NThreads,
                  enable_assertions = FALSE)
    
    # Regression Define data sets----
    datatrain    <- h2o::as.h2o(dataTrain)
    datavalidate <- h2o::as.h2o(dataTest)
    
    # Regression Grid Tune Search Criteria----
    search_criteria  <- list(
      strategy             = "RandomDiscrete",
      max_runtime_secs     = 3600 * 24 * 7,
      max_models           = MaxModelsInGrid,
      seed                 = 1234,
      stopping_rounds      = 10,
      stopping_metric      = toupper(eval_metric),
      stopping_tolerance   = 1e-3
    )
    
    # Regression Grid Parameters----
    hyper_params <- list(
      max_depth                        = c(6, 9, 12),
      sample_rate                      = c(0.5, 0.75, 1.0),
      col_sample_rate_per_tree         = c(0.5, 0.75, 1.0),
      col_sample_rate_change_per_level = c(0.9, 1.0, 1.1),
      min_rows                         = c(1, 10),
      nbins                            = c(10, 20, 30),
      nbins_cats                       = c(64, 256, 512),
      histogram_type                   = c("UniformAdaptive",
                                           "QuantilesGlobal",
                                           "RoundRobin")
    )
    
    # Regression Grid Train Model----
    grid <- h2o::h2o.grid(
      hyper_params         = hyper_params,
      search_criteria      = search_criteria,
      is_supervised        = TRUE,
      algorithm            = "randomForest",
      grid_id              = paste0(ModelID, "_Grid"),
      x                    = FeatureColNames,
      y                    = TargetColumnName,
      ntrees               = Trees,
      training_frame       = datatrain,
      validation_frame     = datavalidate,
      max_runtime_secs     = 3600 * 24 * 7,
      stopping_rounds      = 10,
      stopping_tolerance   = 1e-3,
      stopping_metric      = toupper(eval_metric),
      score_tree_interval  = 10,
      seed                 = 1234
    )
    
    # Regression Get Best Model----
    Grid_Out   <- h2o::h2o.getGrid(
      grid_id = paste0(ModelID, "_Grid"),
      sort_by = eval_metric,
      decreasing = FALSE
    )
    
    # Regression Collect Best Grid Model----
    grid_model <- h2o::h2o.getModel(Grid_Out@model_ids[[1]])
  }
  
  # Regression Start Up H2O----
  if (!GridTune) {
    h2o::h2o.init(max_mem_size = MaxMem,
                  enable_assertions = FALSE)
    
    # Regression Define data sets----
    datatrain    <- h2o::as.h2o(dataTrain)
    datavalidate <- h2o::as.h2o(dataTest)
  }
  
  # Regression Baseline Model----
  base_model <- h2o::h2o.randomForest(
    x                = FeatureColNames,
    y                = TargetColumnName,
    training_frame   = datatrain,
    validation_frame = datavalidate,
    model_id         = ModelID,
    ntrees           = Trees
  )
  
  # Regression Grab Evaluation Metric----
  if (GridTune) {
    if (!is.null(TestData)) {
      datatest        <-  h2o::as.h2o(TestData)
      if (tolower(eval_metric) == "mse") {
        GridModelEval <-
          h2o::h2o.mse(h2o::h2o.performance(model = grid_model,
                                            newdata = datatest))
        BaseModelEval <-
          h2o::h2o.mse(h2o::h2o.performance(model = base_model,
                                            newdata = datatest))
      } else if (tolower(eval_metric) == "rmse") {
        GridModelEval <-
          h2o::h2o.rmse(h2o::h2o.performance(model = grid_model,
                                             newdata = datatest))
        BaseModelEval <-
          h2o::h2o.rmse(h2o::h2o.performance(model = base_model,
                                             newdata = datatest))
      } else if (tolower(eval_metric) == "mae") {
        GridModelEval <-
          h2o::h2o.mae(h2o::h2o.performance(model = grid_model,
                                            newdata = datatest))
        BaseModelEval <-
          h2o::h2o.mae(h2o::h2o.performance(model = base_model,
                                            newdata = datatest))
      } else if (tolower(eval_metric) == "rmsle") {
        GridModelEval <-
          h2o::h2o.rmsle(h2o::h2o.performance(model = grid_model,
                                              newdata = datatest))
        BaseModelEval <-
          h2o::h2o.rmsle(h2o::h2o.performance(model = base_model,
                                              newdata = datatest))
      }
    } else {
      if (tolower(eval_metric) == "mse") {
        GridModelEval <-
          h2o::h2o.mse(h2o::h2o.performance(model = grid_model,
                                            newdata = datavalidate))
        BaseModelEval <-
          h2o::h2o.mse(h2o::h2o.performance(model = base_model,
                                            newdata = datavalidate))
      } else if (tolower(eval_metric) == "rmse") {
        GridModelEval <-
          h2o::h2o.rmse(h2o::h2o.performance(model = grid_model,
                                             newdata = datavalidate))
        BaseModelEval <-
          h2o::h2o.rmse(h2o::h2o.performance(model = base_model,
                                             newdata = datavalidate))
      } else if (tolower(eval_metric) == "mae") {
        GridModelEval <-
          h2o::h2o.mae(h2o::h2o.performance(model = grid_model,
                                            newdata = datavalidate))
        BaseModelEval <-
          h2o::h2o.mae(h2o::h2o.performance(model = base_model,
                                            newdata = datavalidate))
      } else if (tolower(eval_metric) == "rmsle") {
        GridModelEval <-
          h2o::h2o.rmsle(h2o::h2o.performance(model = grid_model,
                                              newdata = datavalidate))
        BaseModelEval <-
          h2o::h2o.rmsle(h2o::h2o.performance(model = base_model,
                                              newdata = datavalidate))
      }
      
    }
  } else {
    if (!is.null(TestData)) {
      datatest        <-  h2o::as.h2o(TestData)
      if (tolower(eval_metric) == "mse") {
        BaseModelEval <-
          h2o::h2o.mse(h2o::h2o.performance(model = base_model,
                                            newdata = datatest))
      } else if (tolower(eval_metric) == "rmse") {
        BaseModelEval <-
          h2o::h2o.rmse(h2o::h2o.performance(model = base_model,
                                             newdata = datatest))
      } else if (tolower(eval_metric) == "mae") {
        BaseModelEval <-
          h2o::h2o.mae(h2o::h2o.performance(model = base_model,
                                            newdata = datatest))
      } else if (tolower(eval_metric) == "rmsle") {
        BaseModelEval <-
          h2o::h2o.rmsle(h2o::h2o.performance(model = base_model,
                                              newdata = datatest))
      }
    } else {
      if (tolower(eval_metric) == "mse") {
        BaseModelEval <-
          h2o::h2o.mse(h2o::h2o.performance(model = base_model,
                                            newdata = datavalidate))
      } else if (tolower(eval_metric) == "rmse") {
        BaseModelEval <-
          h2o::h2o.rmse(h2o::h2o.performance(model = base_model,
                                             newdata = datavalidate))
      } else if (tolower(eval_metric) == "mae") {
        BaseModelEval <-
          h2o::h2o.mae(h2o::h2o.performance(model = base_model,
                                            newdata = datavalidate))
      } else if (tolower(eval_metric) == "rmsle") {
        BaseModelEval <-
          h2o::h2o.rmsle(h2o::h2o.performance(model = base_model,
                                              newdata = datavalidate))
      }
    }
  }
  
  # Regression Pick Winner----
  if (GridTune) {
    if (GridModelEval < BaseModelEval) {
      FinalModel <- grid_model
    } else {
      FinalModel <- base_model
    }
  } else {
    FinalModel <- base_model
  }
  
  # Regression Save Model----
  if (SaveModelObjects) {
    if (tolower(IfSaveModel) == "mojo") {
      SaveModel <- h2o::h2o.saveMojo(object = FinalModel,
                                     path = model_path,
                                     force = TRUE)
      h2o::h2o.download_mojo(
        model = FinalModel,
        path = model_path,
        get_genmodel_jar = TRUE,
        genmodel_path = model_path,
        genmodel_name = ModelID
      )
    } else {
      SaveModel <- h2o::h2o.saveModel(object = FinalModel,
                                      path = model_path,
                                      force = TRUE)
    }
  }
  
  # Regression Score Final Test Data----
  if (!is.null(TestData)) {
    Predict <- h2o::h2o.predict(object = FinalModel,
                                                 newdata = datatest)
    Predict <- data.table::as.data.table(Predict)
  } else {
    Predict <-
      data.table::as.data.table(h2o::h2o.predict(object = FinalModel,
                                                 newdata = datavalidate))
  }
  
  # Regression Variable Importance----
  VariableImportance <-
    data.table::as.data.table(h2o::h2o.varimp(object = FinalModel))
  if (SaveModelObjects) {
    data.table::fwrite(VariableImportance,
                       file = paste0(model_path,
                                     "/",
                                     ModelID, "_VariableImportance.csv"))
  }
  
  # Regression Format Variable Importance Table----
  data.table::setnames(
    VariableImportance,
    c(
      "variable",
      "relative_importance",
      "scaled_importance",
      "percentage"
    ),
    c(
      "Variable",
      "RelativeImportance",
      "ScaledImportance",
      "Percentage"
    )
  )
  VariableImportance[, ':=' (
    RelativeImportance = round(RelativeImportance, 4),
    ScaledImportance = round(ScaledImportance, 4),
    Percentage = round(Percentage, 4)
  )]
  
  # Regression H2O Shutdown----
  if(StopH2O) {
    h2o::h2o.shutdown(prompt = FALSE)
  }

  # Regression Create Validation Data----
  if (!is.null(TestData)) {
    ValidationData <-
      data.table::as.data.table(cbind(TestData, Predict))
  } else {
    ValidationData <-
      data.table::as.data.table(cbind(dataTest, Predict))
  }
  
  # Regression Change Prediction Name----
  data.table::setnames(ValidationData, "predict", "Predict")
  
  # Inverse Transform----
  if (!is.null(TransformNumericColumns)) {
    # Append record for Predicted Column----
    if (GridTune) {
      TransformationResults <-
        TransformationResults[ColumnName != "Predict"]
    }
    TransformationResults <- data.table::rbindlist(list(
      TransformationResults,
      data.table::data.table(
        ColumnName = "Predict",
        MethodName = rep(TransformationResults[ColumnName == eval(TargetColumnName),
                                               MethodName], 1),
        Lambda = rep(TransformationResults[ColumnName == eval(TargetColumnName),
                                           Lambda], 1),
        NormalizedStatistics = rep(0, 1)
      )
    ))
    
    # If Actual target columnname == "Target" remove the duplicate version----
    if (length(unique(TransformationResults[["ColumnName"]])) != nrow(TransformationResults)) {
      temp <- TransformationResults[, .N, by = "ColumnName"][N != 1][[1]]
      temp1 <- which(names(ValidationData) == temp)[1]
      ValidationData[, eval(names(data)[temp1]) := NULL]
      TransformationResults <- TransformationResults[, ID := 1:.N][
        ID != which(TransformationResults[["ID"]] == temp1)][
          , ID := NULL]
    }
    
    # Transform Target and Predicted Value----
    ValidationData <- AutoTransformationScore(
      ScoringData = ValidationData,
      Type = "Inverse",
      FinalResults = TransformationResults,
      TransID = NULL,
      Path = NULL
    )
  }
  
  # Regression Get R2----
  r_squared <-
    (ValidationData[, stats::cor(get(TargetColumnName), Predict)][[1]]) ^ 2
  
  # Regression Save Validation Data to File----
  if (SaveModelObjects) {
    data.table::fwrite(ValidationData,
                       file = paste0(model_path,
                                     "/",
                                     ModelID,
                                     "_ValidationData.csv"))
  }
  
  # Regression Evaluation Calibration Plot----
  EvaluationPlot <- EvalPlot(
    data = ValidationData,
    PredictionColName = "Predict",
    TargetColName = Target,
    GraphType = "calibration",
    PercentileBucket = 0.05,
    aggrfun = function(x)
      mean(x, na.rm = TRUE)
  )
  
  # Regression Evaluation Plot Update Title----
  if (GridTune) {
    val <- max(GridModelEval, BaseModelEval)
    EvaluationPlot <- EvaluationPlot +
      ggplot2::ggtitle(paste0(
        "Random Forest Calibration Evaluation Plot: ",
        toupper(eval_metric),
        " = ",
        round(val, 3)
      ))
  } else {
    EvaluationPlot <- EvaluationPlot +
      ggplot2::ggtitle(paste0(
        "Calibration Evaluation Plot: ",
        toupper(eval_metric),
        " = ",
        round(BaseModelEval, 3)
      ))
  }
  
  # Save plot to file
  if (SaveModelObjects) {
    ggplot2::ggsave(paste0(model_path,
                           "/",
                           ModelID,
                           "_EvaluationPlot.png"))
  }
  
  # Regression Evaluation BoxPlot----
  EvaluationBoxPlot <- EvalPlot(
    data = ValidationData,
    PredictionColName = "Predict",
    TargetColName = Target,
    GraphType = "boxplot",
    PercentileBucket = 0.05,
    aggrfun = function(x)
      mean(x, na.rm = TRUE)
  )
  
  # Regression Evaluation Plot Update Title----
  if (GridTune) {
    val <- max(GridModelEval, BaseModelEval)
    EvaluationBoxPlot <- EvaluationBoxPlot +
      ggplot2::ggtitle(paste0(
        "Random Forest Calibration Evaluation Plot: ",
        toupper(eval_metric),
        " = ",
        round(val, 3)
      ))
  } else {
    EvaluationBoxPlot <- EvaluationBoxPlot +
      ggplot2::ggtitle(paste0(
        "Random Forest Calibration Evaluation Plot: ",
        toupper(eval_metric),
        " = ",
        round(BaseModelEval, 3)
      ))
  }
  
  # Save plot to file
  if (SaveModelObjects) {
    ggplot2::ggsave(paste0(model_path,
                           "/",
                           ModelID,
                           "_EvaluationBoxPlot.png"))
  }
  
  # Regression Evaluation Metrics----
  EvaluationMetrics <-
    data.table::data.table(
      Metric = c("Poisson", "MAE",
                 "MAPE", "MSE", "MSLE",
                 "KL", "CS", "R2"),
      MetricValue = rep(999999, 8)
    )
  i <- 0
  for (metric in c("poisson", "mae", "mape", "mse", "msle", "kl", "cs", "r2")) {
    i <- as.integer(i + 1)
    tryCatch({
      # Regression Grid Evaluation Metrics----
      if (tolower(metric) == "poisson") {
        if (MinVal > 0 &
            min(ValidationData[["Predict"]], na.rm = TRUE) > 0) {
          ValidationData[, Metric := Predict - get(TargetColumnName) * log(Predict + 1)]
          Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
        }
      } else if (tolower(metric) == "mae") {
        ValidationData[, Metric := abs(get(TargetColumnName) - Predict)]
        Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
      } else if (tolower(metric) == "mape") {
        ValidationData[, Metric := abs((get(TargetColumnName) - Predict) / (get(TargetColumnName) + 1))]
        Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
      } else if (tolower(metric) == "mse") {
        ValidationData[, Metric := (get(TargetColumnName) - Predict) ^ 2]
        Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
      } else if (tolower(metric) == "msle") {
        if (MinVal > 0 &
            min(ValidationData[["Predict"]], na.rm = TRUE) > 0) {
          ValidationData[, Metric := (log(get(TargetColumnName) + 1) - log(Predict + 1)) ^ 2]
          Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
        }
      } else if (tolower(metric) == "kl") {
        if (MinVal > 0 &
            min(ValidationData[["Predict"]], na.rm = TRUE) > 0) {
          ValidationData[, Metric := get(TargetColumnName) * log((get(TargetColumnName) + 1) /
                                                                   (Predict + 1))]
          Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
        }
      } else if (tolower(metric) == "cs") {
        ValidationData[, ':=' (
          Metric1 = get(TargetColumnName) * Predict,
          Metric2 = get(TargetColumnName) ^ 2,
          Metric3 = Predict ^ 2
        )]
        Metric <-
          ValidationData[, sum(Metric1, na.rm = TRUE)] / (sqrt(ValidationData[, sum(Metric2, na.rm = TRUE)]) *
                                                            sqrt(ValidationData[, sum(Metric3, na.rm = TRUE)]))
      } else if (tolower(metric) == "r2") {
        Metric <-
          (ValidationData[, stats::cor(get(TargetColumnName), Predict)][[1]]) ^ 2
      }
      data.table::set(
        EvaluationMetrics,
        i = i,
        j = 2L,
        value = round(Metric, 4)
      )
      data.table::set(EvaluationMetrics,
                      i = i,
                      j = 3L,
                      value = NA)
    }, error = function(x)
      "skip")
  }
  
  # Remove Features----
  ValidationData[, ':=' (
    Metric  = NULL, 
    Metric1 = NULL,
    Metric2 = NULL,
    Metric3 = NULL)]
  
  # Regression Save EvaluationMetrics to File----
  EvaluationMetrics <- EvaluationMetrics[MetricValue != 999999]
  if (SaveModelObjects) {
    data.table::fwrite(EvaluationMetrics,
                       file = paste0(model_path,
                                     "/",
                                     ModelID,
                                     "_EvaluationMetrics.csv"))
  }
  
  # Regression Partial Dependence----
  ParDepPlots <- list()
  j <- 0
  ParDepBoxPlots <- list()
  k <- 0
  for (i in seq_len(min(length(FeatureColNames), NumOfParDepPlots))) {
    tryCatch({
      Out <- ParDepCalPlots(
        data = ValidationData,
        PredictionColName = "Predict",
        TargetColName = Target,
        IndepVar = VariableImportance[i, Variable],
        GraphType = "calibration",
        PercentileBucket = 0.05,
        FactLevels = 10,
        Function = function(x)
          mean(x, na.rm = TRUE)
      )
      
      j <- j + 1
      ParDepPlots[[paste0(VariableImportance[j, Variable])]] <-
        Out
    }, error = function(x)
      "skip")
    tryCatch({
      Out1 <- ParDepCalPlots(
        data = ValidationData,
        PredictionColName = "Predict",
        TargetColName = Target,
        IndepVar = VariableImportance[i, Variable],
        GraphType = "boxplot",
        PercentileBucket = 0.05,
        FactLevels = 10,
        Function = function(x)
          mean(x, na.rm = TRUE)
      )
      
      k <- k + 1
      ParDepBoxPlots[[paste0(VariableImportance[k, Variable])]] <-
        Out1
    }, error = function(x)
      "skip")
    
  }
  
  # Regression Save ParDepPlots to file----
  if (SaveModelObjects) {
    save(ParDepPlots,
         file = paste0(model_path, "/", ModelID, "_ParDepPlots.R"))
  }
  
  # Regression Save ParDepBoxPlots to file----
  if (SaveModelObjects) {
    save(ParDepBoxPlots,
         file = paste0(model_path, "/", ModelID, "_ParDepBoxPlots.R"))
  }
  
  # Subset Transformation Object----
  if(!is.null(TransformNumericColumns)) {
    if(TargetColumnName == "Target") {
      TransformationResults <- TransformationResults[!(ColumnName %chin% c("Predict"))]
    } else {
      TransformationResults <- TransformationResults[!(ColumnName %chin% c("Predict", "Target"))]
    }    
  }

  # Regression Return Objects----
  if (ReturnModelObjects) {
    if(!is.null(TransformNumericColumns)) {
      return(
        list(
          Model = FinalModel,
          ValidationData = ValidationData,
          EvaluationPlot = EvaluationPlot,
          EvaluationBoxPlot = EvaluationBoxPlot,
          EvaluationMetrics = EvaluationMetrics,
          VariableImportance = VariableImportance,
          PartialDependencePlots = ParDepPlots,
          PartialDependenceBoxPlots = ParDepBoxPlots,
          TransformationInformation = TransformationResults
        )
      )
    } else {
      return(
        list(
          Model = FinalModel,
          ValidationData = ValidationData,
          EvaluationPlot = EvaluationPlot,
          EvaluationBoxPlot = EvaluationBoxPlot,
          EvaluationMetrics = EvaluationMetrics,
          VariableImportance = VariableImportance,
          PartialDependencePlots = ParDepPlots,
          PartialDependenceBoxPlots = ParDepBoxPlots
        )
      )      
    }
  }
}
