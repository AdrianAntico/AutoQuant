#' AutoXGBoostClassifier is an automated XGBoost modeling framework with grid-tuning and model evaluation
#'
#' AutoXGBoostClassifier is an automated XGBoost modeling framework with grid-tuning and model evaluation that runs a variety of steps. First, a stratified sampling (by the target variable) is done to create train and validation sets. Then, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation plot, evaluation boxplot, evaluation metrics, variable importance, partial dependence calibration plots, partial dependence calibration box plots, and column names used in model fitting.
#' @author Adrian Antico
#' @family Supervised Learning
#' @param data This is your data set for training and testing your model
#' @param ValidationData This is your holdout data set used in modeling either refine your hyperparameters.
#' @param TestData This is your holdout data set. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TargetColumnName Either supply the target column name OR the column number where the target is located (but not mixed types). Note that the target column needs to be a 0 | 1 numeric variable.
#' @param FeatureColNames Either supply the feature column names OR the column number where the target is located (but not mixed types)
#' @param IDcols A vector of column names or column numbers to keep in your data but not include in the modeling.
#' @param eval_metric This is the metric used to identify best grid tuned model. Choose from "logloss","error","aucpr","auc"
#' @param Trees The maximum number of trees you want in your models
#' @param GridTune Set to TRUE to run a grid tuning procedure. Set a number in MaxModelsInGrid to tell the procedure how many models you want to test.
#' @param NThreads Set the maximum number of threads you'd like to dedicate to the model run. E.g. 8
#' @param TreeMethod Choose from "hist", "gpu_hist"
#' @param grid_eval_metric Set to "f","auc","tpr","fnr","fpr","tnr","prbe","f","odds"
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
#' data[, Target := ifelse(Target > 0.5, 1, 0)]
#' TestModel <- AutoXGBoostClassifier(data,
#'                                    ValidationData = NULL,
#'                                    TestData = NULL,
#'                                    TargetColumnName = 1,
#'                                    FeatureColNames = 2:12,
#'                                    IDcols = NULL,
#'                                    eval_metric = "auc",
#'                                    Trees = 50,
#'                                    GridTune = TRUE,
#'                                    grid_eval_metric = "auc",
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
#' @return Saves to file and returned in list: VariableImportance.csv, Model, ValidationData.csv, EvalutionPlot.png, EvaluationMetrics.csv, ParDepPlots.R a named list of features with partial dependence calibration plots, GridCollect, and GridList
#' @export
AutoXGBoostClassifier <- function(data,
                                  ValidationData = NULL,
                                  TestData = NULL,
                                  TargetColumnName = NULL,
                                  FeatureColNames = NULL,
                                  IDcols = NULL,
                                  eval_metric = "auc",
                                  Trees = 50,
                                  GridTune = FALSE,
                                  grid_eval_metric = "auc",
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
  # Binary Check Arguments----
  if (!(
    tolower(grid_eval_metric) %chin% c(
      "accuracy",
      "auc",
      "tpr",
      "fnr",
      "fpr",
      "tnr",
      "prbe",
      "f",
      "odds",
      "chisq"
    )
  )) {
    warning(
      "grid_eval_metric not in c('accuracy','auc','tpr','fnr','fpr','tnr','prbe','f','odds','chisq')"
    )
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
  
  # Binary Ensure data is a data.table----
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Binary Ensure data is a data.table----
  if (!is.null(ValidationData)) {
    if (!data.table::is.data.table(ValidationData)) {
      ValidationData <- data.table::as.data.table(ValidationData)
    }
  }
  
  # Binary Ensure TestData is a data.table----
  if (!is.null(TestData)) {
    if (!data.table::is.data.table(TestData)) {
      TestData <- data.table::as.data.table(TestData)
    }
  }
  
  # Binary Target Name Storage----
  if (is.character(TargetColumnName)) {
    Target <- TargetColumnName
  } else {
    Target <- names(data)[TargetColumnName]
  }
  
  # Binary IDcol Name Storage----
  if (!is.null(IDcols)) {
    if (!is.character(IDcols)) {
      IDcols <- names(data)[IDcols]
    }
  }
  
  # Binary Identify column numbers for factor variables----
  CatFeatures <- sort(c(as.numeric(which(
    sapply(data, is.factor)
  )),
  as.numeric(which(
    sapply(data, is.character)
  ))))
  CatFeatures <- names(data)[CatFeatures]
  
  # Binary Data Partition----
  if (is.null(ValidationData) & is.null(TestData)) {
    dataSets <- AutoDataPartition(
      data,
      NumDataSets = 3,
      Ratios = c(0.70, 0.20, 0.10),
      PartitionType = "random",
      StratifyColumnNames = Target,
      TimeColumnName = NULL
    )
    data <- dataSets$TrainData
    ValidationData <- dataSets$ValidationData
    TestData <- dataSets$TestData
  }
  
  # Binary data Subset Columns Needed----
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
  
  # Binary TestData Subset Columns Needed----
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
  
  # Binary Dummify dataTrain Categorical Features----
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
      temp <- DummifyDT(
        data = temp,
        cols = CatFeatures,
        KeepFactorCols = FALSE,
        OneHot = FALSE,
        SaveFactorLevels = TRUE,
        SavePath = model_path,
        ImportFactorLevels = FALSE
      )
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
      temp <- DummifyDT(
        data = temp,
        cols = CatFeatures,
        KeepFactorCols = FALSE,
        OneHot = FALSE,
        SaveFactorLevels = TRUE,
        SavePath = model_path,
        ImportFactorLevels = FALSE
      )
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
      temp <- DummifyDT(
        data = temp,
        cols = CatFeatures,
        KeepFactorCols = FALSE,
        OneHot = FALSE,
        SaveFactorLevels = FALSE,
        SavePath = NULL,
        ImportFactorLevels = FALSE
      )
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
      temp <- DummifyDT(
        data = temp,
        cols = CatFeatures,
        KeepFactorCols = FALSE,
        OneHot = FALSE,
        SaveFactorLevels = FALSE,
        SavePath = NULL,
        ImportFactorLevels = FALSE
      )
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
  
  # Binary Save Names of data----
  Names <- data.table::as.data.table(names(data))
  data.table::setnames(Names, "V1", "ColNames")
  if (SaveModelObjects) {
    data.table::fwrite(Names, paste0(model_path,
                                     "/"
                                     , ModelID, "_ColNames.csv"))
  }
  
  # Binary Subset Target Variables----
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
  
  # Binary Remove Target Variable from Feature Data
  dataTrain[, eval(Target) := NULL]
  dataTest[, eval(Target) := NULL]
  if (!is.null(TestData)) {
    TestData[, eval(Target) := NULL]
  }
  
  # Binary Initialize Catboost Data Conversion----
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
  
  
  # Binary Grid Tune or Not Check----
  if (GridTune) {
    # Binary Grid Create data.table To Store Results----
    GridCollect <-
      data.table::data.table(
        ParamRow = 1:(MaxModelsInGrid + 1),
        EvalStat = rep(9999999, MaxModelsInGrid + 1)
      )
    
    # Binary Grid Define Hyper Parameters----
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
    
    # Binary Grid Tuning Main Loop----
    for (i in as.integer(seq_len(MaxModelsInGrid + 1))) {
      # Print i
      print(i)
      
      # Binary Grid Define Base Parameters----
      if (i == 1) {
        base_params <- list(
          booster = "gbtree",
          objective = 'reg:logistic',
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
          objective = 'reg:logistic',
          eval_metric = tolower(eval_metric),
          nthread = NThreads,
          max_bin = 64,
          tree_method = TreeMethod
        )
      }
      
      # Binary Grid Merge Model Parameters----
      # Have first model be the baseline model
      if (i != 1) {
        base_params <- c(as.list(grid_params[i, ]), base_params)
      }
      
      # Binary Grid Train Model----
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
      
      # Binary Grid Score Model----
      if (!is.null(TestData)) {
        predict <- stats::predict(model, datatest)
      } else {
        predict <- stats::predict(model, datavalidate)
      }
      
      # Binary Grid Validation Data----
      if (!is.null(TestData)) {
        calibEval <-
          data.table::as.data.table(cbind(Target = FinalTestTarget, p1 = predict))
      } else {
        calibEval <-
          data.table::as.data.table(cbind(Target = TestTarget, p1 = predict))
      }
      
      # Binary Initialize AUC_List
      AUC_List <- list()
      
      # Binary Grid Evaluation Metrics for Each Grid----
      if (tolower(grid_eval_metric) == "accuracy") {
        j <- 0
        x <- data.table::data.table(
          Metric = "Accuracy",
          MetricValue = 5.0,
          Threshold = seq(0.01, 0.99, 0.001)
        )
        for (k in unique(x[["Threshold"]])) {
          j = as.integer(j + 1)
          Accuracy <-
            mean(calibEval[, ifelse(p1 > k &
                                      Target == 1 |
                                      p1 < k & Target == 0, 1, 0)])
          data.table::set(x,
                          i = j,
                          j = 2L,
                          value = round(Accuracy, 4))
        }
        data.table::setorderv(x,
                              "MetricValue",
                              order = -1,
                              na.last = TRUE)
        Metric <- x[1, MetricValue]
      } else {
        x <-
          ROCR::prediction(predictions = calibEval[["p1"]], labels = calibEval[["Target"]])
        y <-
          ROCR::performance(prediction.obj = x, measure = grid_eval_metric)
        if (any(
          nrow(data.table::as.data.table(y@y.values)) <= 1 |
          nrow(data.table::as.data.table(y@x.values)) <= 1
        )) {
          if (nrow(data.table::as.data.table(y@y.values)) <= 1 &
              nrow(data.table::as.data.table(y@x.values)) <= 1) {
            z <-
              data.table::as.data.table(cbind(
                Metric = y@y.values,
                Threshold = y@x.values
              ))
            Metric <- z[[1]]
          } else if (nrow(data.table::as.data.table(y@y.values)) <= 1 &
                     !(nrow(data.table::as.data.table(y@x.values) <= 1))) {
            z <-
              data.table::as.data.table(cbind(
                Metric = y@y.values,
                Threshold = y@x.values[[1]]
              ))
            Metric <- z[!is.infinite(Threshold)][[1]]
          } else if (!(nrow(data.table::as.data.table(y@y.values)) <= 1) &
                     nrow(data.table::as.data.table(y@x.values) <= 1)) {
            if (grid_eval_metric %chin% c("auc", "tpr", "tnr", "prbe", "f", "odds")) {
              z <-
                data.table::as.data.table(cbind(
                  Metric = y@y.values[[1]],
                  Threshold = y@x.values
                ))
              Metric <-
                z[order(-Metric)][!is.infinite(Metric)][[1]]
            } else {
              z <-
                data.table::as.data.table(cbind(
                  Metric = y@y.values[[1]],
                  Threshold = y@x.values
                ))
              Metric <-
                z[order(Metric)][!is.infinite(Metric)][[1]]
            }
          }
        } else {
          if (grid_eval_metric %chin% c("auc", "tpr", "tnr", "prbe", "f", "odds")) {
            z <-
              data.table::as.data.table(cbind(
                Metric = y@y.values[[1]],
                Threshold = y@x.values[[1]]
              ))
            Metric <-
              z[order(-Metric)][!is.infinite(Threshold) &
                                  !is.infinite(Metric)][1, ]
          } else {
            z <-
              data.table::as.data.table(cbind(
                Metric = y@y.values[[1]],
                Threshold = y@x.values[[1]]
              ))
            Metric <-
              z[order(Metric)][!is.infinite(Threshold) &
                                 !is.infinite(Metric)][1, ]
          }
        }
      }
      
      # Binary AUC Object Create----
      AUC_Metrics <- pROC::roc(
        response = calibEval[["Target"]],
        predictor = calibEval[["p1"]],
        na.rm = TRUE,
        algorithm = 3,
        auc = TRUE,
        ci = TRUE
      )
      
      # Binary AUC Conversion to data.table----
      AUC_List[[i]] <- data.table::data.table(
        ModelNumber = i,
        Sensitivity = as.numeric(AUC_Metrics$sensitivities + 0.0001),
        Specificity = as.numeric(AUC_Metrics$specificities + 0.0001)
      )
      
      # Collect Metrics and Corresponding Grids
      # Store Output Information
      if (tolower(grid_eval_metric) == "accuracy") {
        data.table::set(GridCollect,
                        i = i,
                        j = 1L,
                        value = i)
        data.table::set(GridCollect,
                        i = i,
                        j = 2L,
                        value = Metric)
      } else if (any(nrow(data.table::as.data.table(y@y.values)) <= 1 |
                     nrow(data.table::as.data.table(y@x.values)) <= 1)) {
        data.table::set(GridCollect,
                        i = i,
                        j = 1L,
                        value = i)
        data.table::set(GridCollect,
                        i = i,
                        j = 2L,
                        value = Metric)
      } else {
        data.table::set(GridCollect,
                        i = i,
                        j = 1L,
                        value = i)
        data.table::set(GridCollect,
                        i = i,
                        j = 2L,
                        value = Metric[, 1])
      }
    }
  }
  
  # Binary Define Final Model Parameters----
  if (GridTune) {
    if (eval_metric %chin% c("accuracy", "auc", "tpr", "prbe", "f", "odds")) {
      BestGrid <- GridCollect[order(-EvalStat)][1, ParamRow]
      if (BestGrid == 1) {
        base_params <- list(
          booster = "gbtree",
          objective = 'reg:logistic',
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
          objective = 'reg:logistic',
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
      BestThresh <- GridCollect[order(EvalStat)][1, EvalStat]
      if (BestGrid == 1) {
        base_params <- list(
          booster = "gbtree",
          objective = 'reg:logistic',
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
          objective = 'reg:logistic',
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
      objective = 'reg:logistic',
      eval_metric = tolower(eval_metric),
      nthread = NThreads,
      max_bin = 64,
      tree_method = TreeMethod
    )
    if (!is.null(PassInGrid)) {
      base_params <- c(base_params, as.list(PassInGrid[1,]))
    }
  }
  
  # Binary Train Final Model----
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
  
  # Binary Save Model----
  if (SaveModelObjects) {
    xgboost::xgb.save(model = model, fname = ModelID)
  }
  
  # Binary Grid Score Model----
  if (!is.null(TestData)) {
    predict <- stats::predict(model, datatest)
  } else {
    predict <- stats::predict(model, datavalidate)
  }
  
  # Binary Validation Data----
  if (!is.null(TestData)) {
    ValidationData <-
      data.table::as.data.table(cbind(Target = FinalTestTarget, TestMerge, p1 = predict))
  } else {
    ValidationData <-
      data.table::as.data.table(cbind(Target = TestTarget, dataTest, p1 = predict))
  }
  
  # Binary AUC Object Create----
  AUC_Metrics <- pROC::roc(
    response = ValidationData[["Target"]],
    predictor = ValidationData[["p1"]],
    na.rm = TRUE,
    algorithm = 3,
    auc = TRUE,
    ci = TRUE
  )
  
  # Binary AUC Conversion to data.table----
  AUC_Data <- data.table::data.table(
    ModelNumber = 0,
    Sensitivity = AUC_Metrics$sensitivities,
    Specificity = AUC_Metrics$specificities
  )
  
  # Binary Rbind AUC
  if (GridTune == TRUE & MaxModelsInGrid <= 15) {
    temp <- data.table::rbindlist(AUC_List)
    AUC_Data <- data.table::rbindlist(list(temp, AUC_Data))
    AUC_Data[, ModelNumber := as.factor(ModelNumber)]
    
    # Binary Plot ROC Curve----
    ROC_Plot <-
      ggplot2::ggplot(AUC_Data,
                      ggplot2::aes(
                        x = 1 - Specificity,
                        group = ModelNumber,
                        color = ModelNumber
                      )) +
      ggplot2::geom_line(ggplot2::aes(y = AUC_Data[["Sensitivity"]])) +
      ggplot2::geom_abline(slope = 1, color = "black") +
      ggplot2::ggtitle(paste0(
        "Catboost Best Model AUC: ",
        100 * round(AUC_Metrics$auc, 3),
        "%"
      )) +
      ChartTheme() + ggplot2::xlab("Specificity") +
      ggplot2::ylab("Sensitivity")
    
  } else {
    ROC_Plot <-
      ggplot2::ggplot(AUC_Data, ggplot2::aes(x = 1 - Specificity)) +
      ggplot2::geom_line(ggplot2::aes(y = AUC_Data[["Sensitivity"]]), color = "blue") +
      ggplot2::geom_abline(slope = 1, color = "black") +
      ggplot2::ggtitle(paste0("Catboost AUC: ",
                              100 * round(AUC_Metrics$auc, 3), "%")) +
      ChartTheme() + ggplot2::xlab("Specificity") +
      ggplot2::ylab("Sensitivity")
  }
  
  # Save plot to file
  if (SaveModelObjects) {
    ggplot2::ggsave(paste0(model_path, "/", ModelID, "_ROC_Plot.png"))
  }
  
  # Binary Evaluation Calibration Plot----
  EvaluationPlot <- EvalPlot(
    data = ValidationData,
    PredictionColName = "p1",
    TargetColName = Target,
    GraphType = "calibration",
    PercentileBucket = 0.05,
    aggrfun = function(x)
      mean(x, na.rm = TRUE)
  )
  
  # Add Number of Trees to Title
  EvaluationPlot <- EvaluationPlot +
    ggplot2::ggtitle(paste0(
      "Calibration Evaluation Plot: AUC = ",
      round(AUC_Metrics$auc, 3)
    ))
  
  # Save plot to file
  if (SaveModelObjects) {
    ggplot2::ggsave(paste0(model_path, "/", ModelID, "_EvaluationPlot.png"))
  }
  
  # Evaluation Metrics at Optimial Threshold----
  x <- ROCR::prediction(predictions = ValidationData[["p1"]],
                        labels = ValidationData[["Target"]])
  EvaluationMetrics <-
    data.table::data.table(
      Metric = c(
        "AUC",
        "TruePositiveRate",
        "FalseNegativeRate",
        "FalsePositiveRate",
        "TrueNegativeRate",
        "PreceisionRecallBreakEven",
        "F1_Score",
        "Odds"
      ),
      MetricValue = rep(999999, 8),
      Threshold   = rep(999999, 8)
    )
  i <- 0
  for (metric in c("auc", "tpr", "fnr", "fpr", "tnr", "prbe", "f", "odds")) {
    i <- as.integer(i + 1)
    tryCatch({
      y <- ROCR::performance(prediction.obj = x, measure = metric)
      if (any(nrow(data.table::as.data.table(y@y.values)) <= 1 |
              nrow(data.table::as.data.table(y@x.values)) <= 1)) {
        if (nrow(data.table::as.data.table(y@y.values)) <= 1 &
            nrow(data.table::as.data.table(y@x.values)) <= 1) {
          z <-
            data.table::as.data.table(cbind(
              Metric = y@y.values,
              Threshold = y@x.values
            ))
          Metric <- z[[1]]
        } else if (nrow(data.table::as.data.table(y@y.values)) <= 1 &
                   !(nrow(data.table::as.data.table(y@x.values) <= 1))) {
          z <-
            data.table::as.data.table(cbind(
              Metric = y@y.values,
              Threshold = y@x.values[[1]]
            ))
          Metric <- z[!is.infinite(Threshold)][[1]]
        } else if (!(nrow(data.table::as.data.table(y@y.values)) <= 1) &
                   nrow(data.table::as.data.table(y@x.values) <= 1)) {
          if (metric %chin% c("auc", "tpr", "tnr", "prbe", "f", "odds")) {
            z <-
              data.table::as.data.table(cbind(
                Metric = y@y.values[[1]],
                Threshold = y@x.values
              ))
            Metric <- z[order(-Metric)][!is.infinite(Metric)][[1]]
          } else {
            z <-
              data.table::as.data.table(cbind(
                Metric = y@y.values[[1]],
                Threshold = y@x.values
              ))
            Metric <- z[order(Metric)][!is.infinite(Metric)][[1]]
          }
        }
      } else {
        if (metric %chin% c("auc", "tpr", "tnr", "prbe", "f", "odds")) {
          z <-
            data.table::as.data.table(cbind(
              Metric = y@y.values[[1]],
              Threshold = y@x.values[[1]]
            ))
          Metric <-
            z[order(-Metric)][!is.infinite(Threshold) &
                                !is.infinite(Metric)][1, ]
        } else {
          z <-
            data.table::as.data.table(cbind(
              Metric = y@y.values[[1]],
              Threshold = y@x.values[[1]]
            ))
          Metric <-
            z[order(Metric)][!is.infinite(Threshold) &
                               !is.infinite(Metric)][1, ]
        }
      }
      
      # Store Output Information
      if (any(nrow(data.table::as.data.table(y@y.values)) <= 1 |
              nrow(data.table::as.data.table(y@x.values)) <= 1)) {
        data.table::set(
          EvaluationMetrics,
          i = i,
          j = 2L,
          value = round(Metric[[1]], 4)
        )
        data.table::set(EvaluationMetrics,
                        i = i,
                        j = 3L,
                        value = NA)
      } else {
        data.table::set(
          EvaluationMetrics,
          i = i,
          j = 2L,
          value = round(Metric[[1]], 4)
        )
        data.table::set(
          EvaluationMetrics,
          i = i,
          j = 3L,
          value = Metric[[2]]
        )
      }
    }, error = function(x)
      "skip")
  }
  
  # Binary Accuracy Threshold and Metric----
  j <- 0
  x <-
    data.table(
      Metric = "Accuracy",
      MetricValue = 5.0,
      Threshold = seq(0.01, 0.99, 0.001)
    )
  for (i in unique(x[["Threshold"]])) {
    j = as.integer(j + 1)
    Accuracy <-
      mean(ValidationData[, ifelse(p1 > i &
                                     Target == 1 |
                                     p1 < i & Target == 0, 1, 0)])
    set(x,
        i = j,
        j = 2L,
        value = round(Accuracy, 4))
  }
  data.table::setorderv(x, "MetricValue", order = -1, na.last = TRUE)
  x <- x[1, ]
  EvaluationMetrics <-
    data.table::rbindlist(list(EvaluationMetrics, x))
  
  # Save EvaluationMetrics to File
  EvaluationMetrics <- EvaluationMetrics[MetricValue != 999999]
  if (SaveModelObjects) {
    data.table::fwrite(EvaluationMetrics,
                       file = paste0(model_path, "/", ModelID, "_EvaluationMetrics.csv"))
  }
  
  # Binary Variable Importance----
  VariableImportance <- tryCatch({
    data.table::as.data.table(xgboost::xgb.importance(model = model))}, 
    error = function(x) data.table(Gain = NULL, Cover = NULL, Frequency = NULL))
  if(VariableImportance[, .N] != 0) {
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
    
    # Binary Partial Dependence----
    ParDepPlots <- list()
    j <- 0
    ParDepBoxPlots <- list()
    k <- 0
    for (i in seq_len(min(length(FeatureColNames), NumOfParDepPlots))) {
      tryCatch({
        Out <- ParDepCalPlots(
          data = ValidationData,
          PredictionColName = "p1",
          TargetColName = Target,
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
    }  
  }
  
  # Binary Save ParDepPlots to file----
  if (SaveModelObjects) {
    save(ParDepPlots,
         file = paste0(model_path, "/", ModelID, "_ParDepPlots.R"))
  }
  
  # Binary Save GridCollect and GridList----
  if (SaveModelObjects & GridTune == TRUE) {
    data.table::fwrite(grid_params,
                       file = paste0(model_path, "/", ModelID, "_grid_params.csv"))
    data.table::fwrite(GridCollect,
                       file = paste0(model_path, "/", ModelID, "_GridCollect.csv"))
  }
  
  # Binary Return Model Objects----
  if (GridTune) {
    if (ReturnModelObjects) {
      return(
        list(
          Model = model,
          ValidationData = ValidationData,
          ROC_Plot = ROC_Plot,
          EvaluationPlot = EvaluationPlot,
          EvaluationMetrics = EvaluationMetrics,
          VariableImportance = VariableImportance,
          PartialDependencePlots = ParDepPlots,
          GridList = grid_params,
          GridMetrics = GridCollect,
          ColNames = Names
        )
      )
    }
  } else {
    if (ReturnModelObjects) {
      return(
        list(
          Model = model,
          ValidationData = ValidationData,
          ROC_Plot = ROC_Plot,
          EvaluationPlot = EvaluationPlot,
          EvaluationMetrics = EvaluationMetrics,
          VariableImportance = VariableImportance,
          PartialDependencePlots = ParDepPlots,
          ColNames = Names
        )
      )
    }
  }
}
