#' AutoXGBoostRegression is an automated XGBoost modeling framework with grid-tuning and model evaluation
#'
#' AutoXGBoostRegression is an automated XGBoost modeling framework with grid-tuning and model evaluation that runs a variety of steps. First, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation plot, evaluation boxplot, evaluation metrics, variable importance, partial dependence calibration plots, partial dependence calibration box plots, and column names used in model fitting.
#' @author Adrian Antico
#' @family Supervised Learning
#' @param data This is your data set for training and testing your model
#' @param ValidationData This is your holdout data set used in modeling either refine your hyperparameters.
#' @param TestData This is your holdout data set. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TargetColumnName Either supply the target column name OR the column number where the target is located (but not mixed types).
#' @param FeatureColNames Either supply the feature column names OR the column number where the target is located (but not mixed types)
#' @param IDcols A vector of column names or column numbers to keep in your data but not include in the modeling.
#' @param ReturnFactorLevels Set to TRUE to have the factor levels returned with the other model objects
#' @param TransformNumericColumns Set to NULL to do nothing; otherwise supply the column names of numeric variables you want transformed
#' @param eval_metric This is the metric used to identify best grid tuned model. Choose from "r2", "RMSE", "MSE", "MAE"
#' @param Trees The maximum number of trees you want in your models
#' @param GridTune Set to TRUE to run a grid tuning procedure. Set a number in MaxModelsInGrid to tell the procedure how many models you want to test.
#' @param grid_eval_metric Choose from "poisson","mae","mape","mse","msle","kl","cs","r2"
#' @param NThreads Set the maximum number of threads you'd like to dedicate to the model run. E.g. 8
#' @param TreeMethod Choose from "hist", "gpu_hist"
#' @param MaxModelsInGrid Number of models to test from grid options (243 total possible options)
#' @param model_path A character string of your path file to where you want your output saved
#' @param ModelID A character string to name your model and output
#' @param NumOfParDepPlots Tell the function the number of partial dependence calibration plots you want to create.
#' @param Verbose Set to 0 if you want to suppress model evaluation updates in training
#' @param ReturnModelObjects Set to TRUE to output all modeling objects (E.g. plots and evaluation metrics)
#' @param SaveModelObjects Set to TRUE to return all modeling objects to your environment
#' @param PassInGrid Default is NULL. Provide a data.table of grid options from a previous run.
#' @examples
#' \donttest{
#' Correl <- 0.85
#' N <- 10000
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
#' TestModel <- AutoXGBoostRegression(data,
#'                                    ValidationData = NULL,
#'                                    TestData = NULL,
#'                                    TargetColumnName = 1,
#'                                    FeatureColNames = 2:12,
#'                                    IDcols = NULL,
#'                                    ReturnFactorLevels = FALSE,
#'                                    TransformNumericColumns = NULL,
#'                                    eval_metric = "RMSE",
#'                                    Trees = 50,
#'                                    GridTune = TRUE,
#'                                    grid_eval_metric = "mae",
#'                                    MaxModelsInGrid = 10,
#'                                    NThreads = 8,
#'                                    TreeMethod = "hist",
#'                                    model_path = getwd(),
#'                                    ModelID = "FirstModel",
#'                                    NumOfParDepPlots = 3,
#'                                    ReturnModelObjects = TRUE,
#'                                    SaveModelObjects = FALSE,
#'                                    PassInGrid = NULL)
#' }
#' @return Saves to file and returned in list: VariableImportance.csv, Model, ValidationData.csv, EvalutionPlot.png, EvalutionBoxPlot.png, EvaluationMetrics.csv, ParDepPlots.R a named list of features with partial dependence calibration plots, ParDepBoxPlots.R, GridCollect, and GridList
#' @export
# Train code
AutoXGBoostRegression <- function(data,
                                  ValidationData = NULL,
                                  TestData = NULL,
                                  TargetColumnName = NULL,
                                  FeatureColNames = NULL,
                                  IDcols = NULL,
                                  ReturnFactorLevels = FALSE,
                                  TransformNumericColumns = NULL,
                                  eval_metric = "RMSE",
                                  Trees = 50,
                                  GridTune = FALSE,
                                  grid_eval_metric = "mae",
                                  TreeMethod = "hist",
                                  MaxModelsInGrid = 10,
                                  NThreads = 8,
                                  model_path = NULL,
                                  ModelID = "FirstModel",
                                  NumOfParDepPlots = 3,
                                  Verbose = 0,
                                  ReturnModelObjects = TRUE,
                                  SaveModelObjects = FALSE,
                                  PassInGrid = NULL) {
  # Regression Check Arguments----
  if (!(tolower(eval_metric) %chin% c("rmse", "mae", "mape", "r2"))) {
    warning("eval_metric not in RMSE, MAE, MAPE, R2")
    
  }
  if (Trees < 1)
    warning("Trees must be greater than 1")
  if (!GridTune %in% c(TRUE, FALSE))
    warning("GridTune needs to be TRUE or FALSE")
  if (MaxModelsInGrid < 1 |
      MaxModelsInGrid > 1080 & GridTune == TRUE) {
    warning("MaxModelsInGrid needs to be at least 1 and less than 1080")
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
  
  # Regression Ensure TestData is a data.table----
  if (!is.null(TestData)) {
    if (!data.table::is.data.table(TestData)) {
      TestData <- data.table::as.data.table(TestData)
    }
  }
  
  # Regression Target Name Storage----
  if (is.character(TargetColumnName)) {
    Target <- TargetColumnName
  } else {
    Target <- names(data)[TargetColumnName]
  }
  
  # Regression IDcol Name Storage----
  if (!is.null(IDcols)) {
    if (!is.character(IDcols)) {
      IDcols <- names(data)[IDcols]
    }
  }
  
  # Regression Identify column numbers for factor variables----
  CatFeatures <- sort(c(as.numeric(which(
    sapply(data, is.factor)
  )),
  as.numeric(which(
    sapply(data, is.character)
  ))))
  CatFeatures <- names(data)[CatFeatures]
  
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
  
  # Regression data Subset Columns Needed----
  if (is.numeric(FeatureColNames) | is.integer(FeatureColNames)) {
    keep1 <- names(data)[c(FeatureColNames)]
    keep <- c(keep1, Target)
    dataTrain <- data[, ..keep]
    dataTest <- ValidationData[, ..keep]
  } else {
    keep <- c(FeatureColNames, Target)
    dataTrain <- data[, ..keep]
    dataTest <- ValidationData[, ..keep]
  }
  
  # Regression TestData Subset Columns Needed----
  if (!is.null(TestData)) {
    if (is.numeric(FeatureColNames) | is.integer(FeatureColNames)) {
      keep1 <- names(TestData)[c(FeatureColNames)]
      if (!is.null(IDcols)) {
        keep <- c(IDcols, keep1, Target)
      } else {
        keep <- c(keep1, Target)
      }
      TestData <- TestData[, ..keep]
    } else {
      keep1 <- c(FeatureColNames)
      if (!is.null(IDcols)) {
        keep <- c(IDcols, FeatureColNames, Target)
      } else {
        keep <- c(FeatureColNames, Target)
      }
      TestData <- TestData[, ..keep]
    }
    if (!is.null(IDcols)) {
      TestMerge <- data.table::copy(TestData)
      keep <- c(keep1, Target)
      TestData <- TestData[, ..keep]
    } else {
      TestMerge <- data.table::copy(TestData)
    }
  }
  
  # Regression Dummify dataTrain Categorical Features----
  if (SaveModelObjects) {
    if (!is.null(dataTest) & !is.null(TestData)) {
      data.table::set(dataTrain,
                      j = "ID_Factorizer",
                      value = "TRAIN")
      data.table::set(dataTest,
                      j = "ID_Factorizer",
                      value = "VALIDATE")
      data.table::set(TestData,
                      j = "ID_Factorizer",
                      value = "TEST")
      temp <-
        data.table::rbindlist(list(dataTrain, dataTest, TestData))
      if(ReturnFactorLevels) {
        temp <- DummifyDT(
          data = temp,
          cols = CatFeatures,
          KeepFactorCols = FALSE,
          OneHot = FALSE,
          SaveFactorLevels = TRUE,
          ReturnFactorLevels = ReturnFactorLevels,
          SavePath = model_path,
          ImportFactorLevels = FALSE
        )
        FactorLevelsList <- temp$FactorLevelsList
        temp <- temp$data
      } else {
        temp <- DummifyDT(
          data = temp,
          cols = CatFeatures,
          KeepFactorCols = FALSE,
          OneHot = FALSE,
          SaveFactorLevels = FALSE,
          ReturnFactorLevels = ReturnFactorLevels,
          SavePath = model_path,
          ImportFactorLevels = FALSE
        )
      }
      dataTrain <- temp[ID_Factorizer == "TRAIN"]
      data.table::set(dataTrain,
                      j = "ID_Factorizer",
                      value = NULL)
      dataTest <- temp[ID_Factorizer == "VALIDATE"]
      data.table::set(dataTest,
                      j = "ID_Factorizer",
                      value = NULL)
      TestData <- temp[ID_Factorizer == "TEST"]
      data.table::set(TestData,
                      j = "ID_Factorizer",
                      value = NULL)
    } else {
      data.table::set(dataTrain,
                      j = "ID_Factorizer",
                      value = "TRAIN")
      data.table::set(dataTest,
                      j = "ID_Factorizer",
                      value = "TRAIN")
      temp <- data.table::rbindlist(list(dataTrain, dataTest))
      if(ReturnFactorLevels) {
        temp <- DummifyDT(
          data = temp,
          cols = CatFeatures,
          KeepFactorCols = FALSE,
          OneHot = FALSE,
          SaveFactorLevels = TRUE,
          ReturnFactorLevels = ReturnFactorLevels,
          SavePath = model_path,
          ImportFactorLevels = FALSE
        )
        FactorLevelsList <- temp$FactorLevelsList
        temp <- temp$data
      } else {
        temp <- DummifyDT(
          data = temp,
          cols = CatFeatures,
          KeepFactorCols = FALSE,
          OneHot = FALSE,
          SaveFactorLevels = TRUE,
          ReturnFactorLevels = ReturnFactorLevels,
          SavePath = model_path,
          ImportFactorLevels = FALSE
        )  
      }
      dataTrain <- temp[ID_Factorizer == "TRAIN"]
      data.table::set(dataTrain,
                      j = "ID_Factorizer",
                      value = NULL)
      dataTest <- temp[ID_Factorizer == "VALIDATE"]
      data.table::set(dataTest,
                      j = "ID_Factorizer",
                      value = NULL)
    }
  } else {
    if (!is.null(dataTest) & !is.null(TestData)) {
      data.table::set(dataTrain,
                      j = "ID_Factorizer",
                      value = "TRAIN")
      data.table::set(dataTest,
                      j = "ID_Factorizer",
                      value = "VALIDATE")
      data.table::set(TestData,
                      j = "ID_Factorizer",
                      value = "TEST")
      temp <-
        data.table::rbindlist(list(dataTrain, dataTest, TestData))
      if(ReturnFactorLevels) {
        temp <- DummifyDT(
          data = temp,
          cols = CatFeatures,
          KeepFactorCols = FALSE,
          OneHot = FALSE,
          SaveFactorLevels = FALSE,
          ReturnFactorLevels = ReturnFactorLevels,
          SavePath = NULL,
          ImportFactorLevels = FALSE
        )
        FactorLevelsList <- temp$FactorLevelsList
        temp <- temp$data
      } else {
        temp <- DummifyDT(
          data = temp,
          cols = CatFeatures,
          KeepFactorCols = FALSE,
          OneHot = FALSE,
          SaveFactorLevels = FALSE,
          ReturnFactorLevels = ReturnFactorLevels,
          SavePath = NULL,
          ImportFactorLevels = FALSE
        )        
      }
      dataTrain <- temp[ID_Factorizer == "TRAIN"]
      data.table::set(dataTrain,
                      j = "ID_Factorizer",
                      value = NULL)
      dataTest <- temp[ID_Factorizer == "VALIDATE"]
      data.table::set(dataTest,
                      j = "ID_Factorizer",
                      value = NULL)
      TestData <- temp[ID_Factorizer == "TEST"]
      data.table::set(TestData,
                      j = "ID_Factorizer",
                      value = NULL)
      
    } else {
      data.table::set(dataTrain,
                      j = "ID_Factorizer",
                      value = "TRAIN")
      data.table::set(dataTest,
                      j = "ID_Factorizer",
                      value = "TRAIN")
      temp <- data.table::rbindlist(list(dataTrain, dataTest))
      if(ReturnFactorLevels) {
        temp <- DummifyDT(
          data = temp,
          cols = CatFeatures,
          KeepFactorCols = FALSE,
          OneHot = FALSE,
          SaveFactorLevels = FALSE,
          ReturnFactorLevels = ReturnFactorLevels,
          SavePath = NULL,
          ImportFactorLevels = FALSE
        )
      } else {
        temp <- DummifyDT(
          data = temp,
          cols = CatFeatures,
          KeepFactorCols = FALSE,
          OneHot = FALSE,
          SaveFactorLevels = FALSE,
          ReturnFactorLevels = ReturnFactorLevels,
          SavePath = NULL,
          ImportFactorLevels = FALSE
        )  
      }
      dataTrain <- temp[ID_Factorizer == "TRAIN"]
      data.table::set(dataTrain,
                      j = "ID_Factorizer",
                      value = NULL)
      dataTest <- temp[ID_Factorizer == "VALIDATE"]
      data.table::set(dataTest,
                      j = "ID_Factorizer",
                      value = NULL)
    }
  }
  
  # Regression Save Names of data----
  Names <- data.table::as.data.table(names(data))
  data.table::setnames(Names, "V1", "ColNames")
  if (SaveModelObjects) {
    data.table::fwrite(Names, paste0(model_path,
                                     "/"
                                     , ModelID, "_ColNames.csv"))
  }
  
  # Regression Subset Target Variables----
  TrainTarget <-
    tryCatch({
      dataTrain[, get(Target)]
    }, error = function(x)
      dataTrain[, eval(Target)])
  TestTarget <-
    tryCatch({
      dataTest[, get(Target)]
    }, error = function(x)
      dataTest[, eval(Target)])
  if (!is.null(TestData)) {
    FinalTestTarget <-
      tryCatch({
        TestData[, get(Target)]
      }, error = function(x)
        TestData[, eval(Target)])
  }
  
  # Regression Remove Target Variable from Feature Data
  dataTrain[, eval(Target) := NULL]
  dataTest[, eval(Target) := NULL]
  if (!is.null(TestData)) {
    TestData[, eval(Target) := NULL]
  }
  
  # Regression Initialize Catboost Data Conversion----
  datatrain <-
    xgboost::xgb.DMatrix(as.matrix(dataTrain), label = TrainTarget)
  datavalidate <-
    xgboost::xgb.DMatrix(as.matrix(dataTest), label = TestTarget)
  if (!is.null(TestData)) {
    datatest <-
      xgboost::xgb.DMatrix(as.matrix(TestData), label = FinalTestTarget)
    EvalSets <- list(train = datavalidate, test = datatest)
  } else {
    EvalSets <- list(train = datatrain, test = datavalidate)
  }
  
  # Regression Grid Tune or Not Check----
  if (GridTune) {
    # Regression Grid Create data.table To Store Results----
    GridCollect <-
      data.table::data.table(
        ParamRow = 1:(MaxModelsInGrid + 1),
        EvalStat = rep(9999999, MaxModelsInGrid + 1)
      )
    
    # Regression Grid Define Hyper Parameters----
    if (!is.null(PassInGrid)) {
      if (!data.table::is.data.table(PassInGrid)) {
        PassInGrid <- data.table::as.data.table(PassInGrid)
      }
      grid_params <- data.table::CJ(
        eta = c(0.30, 0.25, 0.35),
        max_depth = c(6, 8, 10),
        min_child_weight = c(1, 2, 3),
        subsample = c(1, 0.90, 0.80),
        colsample_bytree = c(1, 0.90, 0.80)
      )
      grid_params[, ID := runif(nrow(grid_params))]
      grid_params <-
        grid_params[order(ID)][1:(MaxModelsInGrid)][, ID := NULL]
      grid_params <-
        data.table::rbindlist(list(PassInGrid, grid_params))
    } else {
      grid_params <- data.table::CJ(
        eta = c(0.30, 0.25, 0.35),
        max_depth = c(6, 8, 10),
        min_child_weight = c(1, 2, 3),
        subsample = c(1, 0.90, 0.80),
        colsample_bytree = c(1, 0.90, 0.80)
      )
      grid_params[, ID := runif(nrow(grid_params))]
      grid_params <-
        grid_params[order(ID)][1:(MaxModelsInGrid + 1)][, ID := NULL]
    }
    
    # Regression Grid Tuning Main Loop----
    for (i in as.integer(seq_len(MaxModelsInGrid + 1))) {
      # Print i
      print(i)
      
      # Regression Grid Define Base Parameters----
      if (i == 1) {
        base_params <- list(
          booster = "gbtree",
          objective = 'reg:linear',
          eval_metric = tolower(eval_metric),
          eta = 0.30,
          max_depth = 6,
          min_child_weight = 1,
          subsample = 1,
          colsample_bytree = 1,
          nthread = NThreads,
          max_bin = 64,
          tree_method = TreeMethod
        )
      } else {
        base_params <- list(
          booster = "gbtree",
          objective = 'reg:linear',
          eval_metric = tolower(eval_metric),
          nthread = NThreads,
          max_bin = 64,
          tree_method = TreeMethod
        )
      }
      
      # Regression Grid Merge Model Parameters----
      # Have first model be the baseline model
      if (i != 1) {
        base_params <- c(as.list(grid_params[i, ]), base_params)
      }
      
      # Regression Grid Train Model----
      if (Verbose == 0) {
        model <- xgboost::xgb.train(
          params = base_params,
          data = datatrain,
          watchlist = EvalSets,
          nrounds = Trees,
          verbose = Verbose,
          early_stopping_rounds = 10
        )
      } else {
        model <- xgboost::xgb.train(
          params = base_params,
          data = datatrain,
          watchlist = EvalSets,
          nrounds = Trees,
          early_stopping_rounds = 10
        )
      }
      
      
      # Regression Grid Score Model----
      if (!is.null(TestData)) {
        predict <- stats::predict(model, datatest)
      } else {
        predict <- stats::predict(model, datavalidate)
      }
      
      # Regression Grid Validation Data----
      if (!is.null(TestData)) {
        calibEval <-
          data.table::as.data.table(cbind(Target = FinalTestTarget, Predicted = predict))
      } else {
        calibEval <-
          data.table::as.data.table(cbind(Target = TestTarget, Predicted = predict))
      }
      
      # Regression Grid Evaluation Metrics----
      if (tolower(grid_eval_metric) == "poisson") {
        if (MinVal > 0 & min(calibEval[["Predicted"]], na.rm = TRUE) > 0) {
          calibEval[, Metric := Predicted - Target * log(Predicted + 1)]
          Metric <- calibEval[, mean(Metric, na.rm = TRUE)]
        }
      } else if (tolower(grid_eval_metric) == "mae") {
        calibEval[, Metric := abs(Target - Predicted)]
        Metric <- calibEval[, mean(Metric, na.rm = TRUE)]
      } else if (tolower(grid_eval_metric) == "mape") {
        calibEval[, Metric := abs((Target - Predicted) / (Target + 1))]
        Metric <- calibEval[, mean(Metric, na.rm = TRUE)]
      } else if (tolower(grid_eval_metric) == "mse") {
        calibEval[, Metric := (Target - Predicted) ^ 2]
        Metric <- calibEval[, mean(Metric, na.rm = TRUE)]
      } else if (tolower(grid_eval_metric) == "msle") {
        if (MinVal > 0 & min(calibEval[["Predicted"]], na.rm = TRUE) > 0) {
          calibEval[, Metric := (log(Target + 1) - log(Predicted + 1)) ^ 2]
          Metric <- calibEval[, mean(Metric, na.rm = TRUE)]
        }
      } else if (tolower(grid_eval_metric) == "kl") {
        if (MinVal > 0 & min(calibEval[["Predicted"]], na.rm = TRUE) > 0) {
          calibEval[, Metric := Target * log((Target + 1) / (Predicted + 1))]
          Metric <- calibEval[, mean(Metric, na.rm = TRUE)]
        }
      } else if (tolower(grid_eval_metric) == "cs") {
        calibEval[, ':=' (
          Metric1 = Target * Predicted,
          Metric2 = Target ^ 2,
          Metric3 = Predicted ^ 2
        )]
        Metric <-
          calibEval[, sum(Metric1, na.rm = TRUE)] / (sqrt(calibEval[, sum(Metric2, na.rm = TRUE)]) *
                                                       sqrt(calibEval[, sum(Metric3, na.rm = TRUE)]))
      } else if (tolower(grid_eval_metric) == "r2") {
        Metric <-
          (calibEval[, stats::cor(eval(Target), Predicted)][[1]]) ^ 2
      }
      
      # Regression Metrics Collection----
      data.table::set(GridCollect,
                      i = i,
                      j = 1L,
                      value = i)
      data.table::set(
        GridCollect,
        i = i,
        j = 2L,
        value = round(Metric, 4)
      )
    }
  }
  
  # Regression Define Final Model Parameters----
  if (GridTune) {
    if (grid_eval_metric %chin% c("kl", "cs", "r2")) {
      BestGrid <- GridCollect[order(-EvalStat)][1, ParamRow]
      if (BestGrid == 1) {
        base_params <- list(
          booster = "gbtree",
          objective = 'reg:linear',
          eval_metric = tolower(eval_metric),
          eta = 0.30,
          max_depth = 6,
          min_child_weight = 1,
          subsample = 1,
          colsample_bytree = 1,
          nthread = NThreads,
          max_bin = 64,
          tree_method = TreeMethod
        )
        
      } else {
        base_params <- list(
          booster = "gbtree",
          objective = 'reg:linear',
          eval_metric = tolower(eval_metric),
          nthread = NThreads,
          max_bin = 64,
          tree_method = TreeMethod
        )
        base_params <-
          c(as.list(grid_params[BestGrid, ]), base_params)
      }
    } else {
      BestGrid <- GridCollect[order(EvalStat)][1, ParamRow]
      if (BestGrid == 1) {
        base_params <- list(
          booster = "gbtree",
          objective = 'reg:linear',
          eval_metric = tolower(eval_metric),
          eta = 0.30,
          max_depth = 6,
          min_child_weight = 1,
          subsample = 1,
          colsample_bytree = 1,
          nthread = NThreads,
          max_bin = 64,
          tree_method = TreeMethod
        )
        
      } else {
        base_params <- list(
          booster = "gbtree",
          objective = 'reg:linear',
          eval_metric = tolower(eval_metric),
          nthread = NThreads,
          max_bin = 64,
          tree_method = TreeMethod
        )
        base_params <-
          c(as.list(grid_params[BestGrid, ]), base_params)
      }
    }
  } else {
    base_params <- list(
      booster = "gbtree",
      objective = 'reg:linear',
      eval_metric = tolower(eval_metric),
      nthread = NThreads,
      max_bin = 64,
      tree_method = TreeMethod
    )
    if (!is.null(PassInGrid)) {
      base_params <- c(base_params, as.list(PassInGrid[1,]))
    }
  }
  
  # Regression Train Final Model----
  if (Verbose == 0) {
    model <- xgboost::xgb.train(
      params = base_params,
      data = datatrain,
      watchlist = EvalSets,
      nrounds = Trees,
      verbose = Verbose,
      early_stopping_rounds = 10
    )
  } else {
    model <- xgboost::xgb.train(
      params = base_params,
      data = datatrain,
      watchlist = EvalSets,
      nrounds = Trees,
      early_stopping_rounds = 10
    )
  }
  
  # Regression Save Model----
  if (SaveModelObjects) {
    xgboost::xgb.save(model = model, fname = ModelID)
  }
  
  # Regression Grid Score Model----
  if (!is.null(TestData)) {
    predict <- stats::predict(model, datatest)
  } else {
    predict <- stats::predict(model, datavalidate)
  }
  
  # Regression Validation Data----
  if (!is.null(TestData)) {
    ValidationData <-
      data.table::as.data.table(cbind(TestMerge, Predict = predict))
  } else {
    ValidationData <-
      data.table::as.data.table(cbind(Target = TestTarget, dataTest, Predict = predict))
    data.table::setnames(ValidationData, "Target", eval(TargetColumnName))
  }
  
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
  
  # Regression r2 via sqrt of correlation
  r_squared <- (ValidationData[, stats::cor(get(Target), Predict)]) ^ 2
  
  # Save Validation Data to File----
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
    TargetColName = eval(TargetColumnName),
    GraphType = "calibration",
    PercentileBucket = 0.05,
    aggrfun = function(x)
      mean(x, na.rm = TRUE)
  )
  
  # Add Number of Trees to Title
  EvaluationPlot <- EvaluationPlot +
    ggplot2::ggtitle(paste0("Calibration Evaluation Plot: R2 = ",
                            round(r_squared, 3)))
  
  # Save plot to file
  if (SaveModelObjects) {
    ggplot2::ggsave(paste0(model_path,
                           "/",
                           ModelID, "_EvaluationPlot.png"))
  }
  
  # Regression Evaluation Calibration Plot----
  EvaluationBoxPlot <- EvalPlot(
    data = ValidationData,
    PredictionColName = "Predict",
    TargetColName = eval(TargetColumnName),
    GraphType = "boxplot",
    PercentileBucket = 0.05,
    aggrfun = function(x)
      mean(x, na.rm = TRUE)
  )
  
  # Add Number of Trees to Title
  EvaluationBoxPlot <- EvaluationBoxPlot +
    ggplot2::ggtitle(paste0("Calibration Evaluation Plot: R2 = ",
                            round(r_squared, 3)))
  
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
          ValidationData[, Metric := Predict - get(Target) * log(Predict + 1)]
          Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
        }
      } else if (tolower(metric) == "mae") {
        ValidationData[, Metric := abs(get(Target) - Predict)]
        Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
      } else if (tolower(metric) == "mape") {
        ValidationData[, Metric := abs((get(Target) - Predict) / (get(Target) + 1))]
        Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
      } else if (tolower(metric) == "mse") {
        ValidationData[, Metric := (get(Target) - Predict) ^ 2]
        Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
      } else if (tolower(metric) == "msle") {
        if (MinVal > 0 &
            min(ValidationData[["Predict"]], na.rm = TRUE) > 0) {
          ValidationData[, Metric := (log(get(Target) + 1) - log(Predict + 1)) ^ 2]
          Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
        }
      } else if (tolower(metric) == "kl") {
        if (MinVal > 0 &
            min(ValidationData[["Predict"]], na.rm = TRUE) > 0) {
          ValidationData[, Metric := get(Target) * log((get(Target) + 1) /
                                                         (Predict + 1))]
          Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
        }
      } else if (tolower(metric) == "cs") {
        ValidationData[, ':=' (
          Metric1 = get(Target) * Predict,
          Metric2 = get(Target) ^ 2,
          Metric3 = Predict ^ 2
        )]
        Metric <-
          ValidationData[, sum(Metric1, na.rm = TRUE)] / (sqrt(ValidationData[, sum(Metric2, na.rm = TRUE)]) *
                                                            sqrt(ValidationData[, sum(Metric3, na.rm = TRUE)]))
      } else if (tolower(metric) == "r2") {
        Metric <-
          (ValidationData[, stats::cor(eval(Target), Predict)][[1]]) ^ 2
      }
      data.table::set(EvaluationMetrics,
                      i = i,
                      j = 2L,
                      value = Metric)
    }, error = function(x)
      "skip")
  }
  
  # Save EvaluationMetrics to File
  EvaluationMetrics <- EvaluationMetrics[MetricValue != 999999]
  if (SaveModelObjects) {
    data.table::fwrite(EvaluationMetrics,
                       file = paste0(model_path,
                                     "/",
                                     ModelID, "_EvaluationMetrics.csv"))
  }
  
  # Regression Variable Importance----
  VariableImportance <- xgboost::xgb.importance(model = model)
  VariableImportance[, ':=' (
    Gain = round(Gain, 4),
    Cover = round(Cover, 4),
    Frequency = round(Frequency, 4)
  )]
  if (SaveModelObjects) {
    data.table::fwrite(VariableImportance,
                       file = paste0(model_path,
                                     "/",
                                     ModelID, "_VariableImportance.csv"))
  }
  
  # Regression Partial Dependence----
  ParDepPlots <- list()
  j <- 0
  ParDepBoxPlots <- list()
  k <- 0
  for (i in seq_len(min(length(VariableImportance[, Feature]), NumOfParDepPlots))) {
    tryCatch({
      Out <- ParDepCalPlots(
        data = ValidationData,
        PredictionColName = "Predict",
        TargetColName = eval(TargetColumnName),
        IndepVar = VariableImportance[i, Feature],
        GraphType = "calibration",
        PercentileBucket = 0.05,
        FactLevels = 10,
        Function = function(x)
          mean(x, na.rm = TRUE)
      )
      
      j <- j + 1
      ParDepPlots[[paste0(VariableImportance[j, Feature])]] <- Out
    }, error = function(x)
      "skip")
    tryCatch({
      Out1 <- ParDepCalPlots(
        data = ValidationData,
        PredictionColName = "Predict",
        TargetColName = eval(TargetColumnName),
        IndepVar = VariableImportance[i, Feature],
        GraphType = "boxplot",
        PercentileBucket = 0.05,
        FactLevels = 10,
        Function = function(x)
          mean(x, na.rm = TRUE)
      )
      
      k <- k + 1
      ParDepBoxPlots[[paste0(VariableImportance[k, Feature])]] <-
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
  
  # Regression Save GridCollect and GridList----
  if (SaveModelObjects & GridTune == TRUE) {
    data.table::fwrite(grid_params,
                       file = paste0(model_path,
                                     "/",
                                     ModelID,
                                     "_grid_params.csv"))
    data.table::fwrite(GridCollect,
                       file = paste0(model_path,
                                     "/",
                                     ModelID,
                                     "_GridCollect.csv"))
  }
  
  # Regression Remove Extraneous Variables----
  ValidationData[, ':=' (
    Metric = NULL,
    Metric1 = NULL,
    Metric2 = NULL,
    Metric3 = NULL
  )]
  
  # Regression Formal Evaluation Table
  EvaluationMetrics[, MetricValue := round(MetricValue, 4)]
  
  # Subset Transformation Object----
  if(TargetColumnName == "Target") {
    TransformationResults <- TransformationResults[!(ColumnName %chin% c("Predict"))]
  } else {
    TransformationResults <- TransformationResults[!(ColumnName %chin% c("Predict", "Target"))]
  }
  
  # Regression Return Model Objects----
  if (GridTune) {
    if (!is.null(TransformNumericColumns)) {
      if (ReturnModelObjects) {
        if(ReturnFactorLevels) {
          return(
            list(
              Model = model,
              ValidationData = ValidationData,
              EvaluationPlot = EvaluationPlot,
              EvaluationBoxPlot = EvaluationBoxPlot,
              EvaluationMetrics = EvaluationMetrics,
              VariableImportance = VariableImportance,
              PartialDependencePlots = ParDepPlots,
              PartialDependenceBoxPlots = ParDepBoxPlots,
              GridList = grid_params,
              GridMetrics = GridCollect,
              ColNames = Names,
              TransformationResults = TransformationResults,
              FactorLevelsList = FactorLevelsList
            )
          )
        } else {
          return(
            list(
              Model = model,
              ValidationData = ValidationData,
              EvaluationPlot = EvaluationPlot,
              EvaluationBoxPlot = EvaluationBoxPlot,
              EvaluationMetrics = EvaluationMetrics,
              VariableImportance = VariableImportance,
              PartialDependencePlots = ParDepPlots,
              PartialDependenceBoxPlots = ParDepBoxPlots,
              GridList = grid_params,
              GridMetrics = GridCollect,
              ColNames = Names,
              TransformationResults = TransformationResults
            )
          )          
        }
      }
    } else {
      if(ReturnFactorLevels) {
        return(
          list(
            Model = model,
            ValidationData = ValidationData,
            EvaluationPlot = EvaluationPlot,
            EvaluationBoxPlot = EvaluationBoxPlot,
            EvaluationMetrics = EvaluationMetrics,
            VariableImportance = VariableImportance,
            PartialDependencePlots = ParDepPlots,
            PartialDependenceBoxPlots = ParDepBoxPlots,
            GridList = grid_params,
            GridMetrics = GridCollect,
            ColNames = Names,
            FactorLevelsList = FactorLevelsList
          )
        )
      } else {
        return(
          list(
            Model = model,
            ValidationData = ValidationData,
            EvaluationPlot = EvaluationPlot,
            EvaluationBoxPlot = EvaluationBoxPlot,
            EvaluationMetrics = EvaluationMetrics,
            VariableImportance = VariableImportance,
            PartialDependencePlots = ParDepPlots,
            PartialDependenceBoxPlots = ParDepBoxPlots,
            GridList = grid_params,
            GridMetrics = GridCollect,
            ColNames = Names
          )
        )        
      }
    }
  } else {
    if (!is.null(TransformNumericColumns)) {
      if (ReturnModelObjects) {
        if(ReturnFactorLevels) {
          return(
            list(
              Model = model,
              ValidationData = ValidationData,
              EvaluationPlot = EvaluationPlot,
              EvaluationBoxPlot = EvaluationBoxPlot,
              EvaluationMetrics = EvaluationMetrics,
              VariableImportance = VariableImportance,
              PartialDependencePlots = ParDepPlots,
              PartialDependenceBoxPlots = ParDepBoxPlots,
              ColNames = Names,
              TransformationResults = TransformationResults,
              FactorLevelsList = FactorLevelsList
            )
          )
        } else {
          return(
            list(
              Model = model,
              ValidationData = ValidationData,
              EvaluationPlot = EvaluationPlot,
              EvaluationBoxPlot = EvaluationBoxPlot,
              EvaluationMetrics = EvaluationMetrics,
              VariableImportance = VariableImportance,
              PartialDependencePlots = ParDepPlots,
              PartialDependenceBoxPlots = ParDepBoxPlots,
              ColNames = Names,
              TransformationResults = TransformationResults
            )
          )          
        }
      }
    } else {
      if(ReturnFactorLevels) {
        return(
          list(
            Model = model,
            ValidationData = ValidationData,
            EvaluationPlot = EvaluationPlot,
            EvaluationBoxPlot = EvaluationBoxPlot,
            EvaluationMetrics = EvaluationMetrics,
            VariableImportance = VariableImportance,
            PartialDependencePlots = ParDepPlots,
            PartialDependenceBoxPlots = ParDepBoxPlots,
            ColNames = Names,
            FactorLevelsList = FactorLevelsList
          )
        )
      } else {
        return(
          list(
            Model = model,
            ValidationData = ValidationData,
            EvaluationPlot = EvaluationPlot,
            EvaluationBoxPlot = EvaluationBoxPlot,
            EvaluationMetrics = EvaluationMetrics,
            VariableImportance = VariableImportance,
            PartialDependencePlots = ParDepPlots,
            PartialDependenceBoxPlots = ParDepBoxPlots,
            ColNames = Names
          )
        )        
      }
    }
  }
}