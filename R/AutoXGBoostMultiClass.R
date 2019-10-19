#' AutoXGBoostMultiClass is an automated XGBoost modeling framework with grid-tuning and model evaluation
#'
#' AutoXGBoostMultiClass is an automated XGBoost modeling framework with grid-tuning and model evaluation that runs a variety of steps. First, a stratified sampling (by the target variable) is done to create train and validation sets. Then, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation metrics, variable importance, and column names used in model fitting.
#' @author Adrian Antico
#' @family Automated MultiClass Classification
#' @param data This is your data set for training and testing your model
#' @param ValidationData This is your holdout data set used in modeling either refine your hyperparameters.
#' @param TestData This is your holdout data set. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TargetColumnName Either supply the target column name OR the column number where the target is located (but not mixed types). Target should be in factor or character form.
#' @param FeatureColNames Either supply the feature column names OR the column number where the target is located (but not mixed types)
#' @param IDcols A vector of column names or column numbers to keep in your data but not include in the modeling.
#' @param eval_metric This is the metric used to identify best grid tuned model. Choose from "merror", "mlogloss"
#' @param Trees The maximum number of trees you want in your models
#' @param GridTune Set to TRUE to run a grid tuning procedure. Set a number in MaxModelsInGrid to tell the procedure how many models you want to test.
#' @param NThreads Set the maximum number of threads you'd like to dedicate to the model run. E.g. 8
#' @param TreeMethod Choose from "hist", "gpu_hist"
#' @param Objective Choose from 'multi:softmax' or 'multi:softprob'
#' @param grid_eval_metric Set to "accuracy" (only option currently)
#' @param MaxModelsInGrid Number of models to test from grid options (243 total possible options)
#' @param model_path A character string of your path file to where you want your output saved
#' @param metadata_path A character string of your path file to where you want your model evaluation output saved. If left NULL, all output will be saved to model_path.
#' @param ModelID A character string to name your model and output
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
#' data[, Target := as.factor(
#'   ifelse(Independent_Variable2 < 0.20, "A",
#'          ifelse(Independent_Variable2 < 0.40, "B",
#'                 ifelse(Independent_Variable2 < 0.6,  "C",
#'                        ifelse(Independent_Variable2 < 0.8,  "D", "E")))))]
#' data[, Independent_Variable11 := as.factor(
#' ifelse(Independent_Variable2 < 0.25, "A",
#'        ifelse(Independent_Variable2 < 0.35, "B",
#'               ifelse(Independent_Variable2 < 0.65,  "C",
#'                      ifelse(Independent_Variable2 < 0.75,  "D", "E")))))]
#' data[, ':=' (x1 = NULL, x2 = NULL)]
#' TestModel <- AutoXGBoostMultiClass(data,
#'                                    ValidationData = NULL,
#'                                    TestData = NULL,
#'                                    TargetColumnName = 1,
#'                                    FeatureColNames = 2:12,
#'                                    IDcols = NULL,
#'                                    eval_metric = "merror",
#'                                    Trees = 50,
#'                                    GridTune = TRUE,
#'                                    grid_eval_metric = "accuracy",
#'                                    MaxModelsInGrid = 10,
#'                                    NThreads = 8,
#'                                    TreeMethod = "hist",
#'                                    Objective = 'multi:softmax',
#'                                    model_path = NULL,
#'                                    metadata_path = NULL,
#'                                    ModelID = "FirstModel",
#'                                    ReturnModelObjects = TRUE,
#'                                    SaveModelObjects = FALSE,
#'                                    PassInGrid = NULL)
#' }
#' @return Saves to file and returned in list: VariableImportance.csv, Model, ValidationData.csv, EvaluationMetrics.csv, GridCollect, GridList, and TargetLevels
#' @export
AutoXGBoostMultiClass <- function(data,
                                  ValidationData = NULL,
                                  TestData = NULL,
                                  TargetColumnName = NULL,
                                  FeatureColNames = NULL,
                                  IDcols = NULL,
                                  eval_metric = "merror",
                                  Trees = 50,
                                  GridTune = FALSE,
                                  grid_eval_metric = "merror",
                                  TreeMethod = "hist",
                                  Objective = 'multi:softmax',
                                  MaxModelsInGrid = 10,
                                  NThreads = 8,
                                  model_path = NULL,
                                  metadata_path = NULL,
                                  ModelID = "FirstModel",
                                  Verbose = 0,
                                  ReturnModelObjects = TRUE,
                                  SaveModelObjects = FALSE,
                                  PassInGrid = NULL) {
  # MultiClass Check Arguments----
  if (!(tolower(grid_eval_metric) %chin% c("accuracy"))) {
    warning("grid_eval_metric not accuracy")
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
  if (!is.null(metadata_path)) {
    if (!is.character(metadata_path))
      warning("metadata_path needs to be a character type")
  }
  if (!is.character(ModelID))
    warning("ModelID needs to be a character type")
  if (!(ReturnModelObjects %in% c(TRUE, FALSE)))
    warning("ReturnModelObjects needs to be TRUE or FALSE")
  if (!(SaveModelObjects %in% c(TRUE, FALSE)))
    warning("SaveModelObjects needs to be TRUE or FALSE")
  
  # MultiClass Ensure data is a data.table----
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # MultiClass Ensure data is a data.table----
  if (!is.null(ValidationData)) {
    if (!data.table::is.data.table(ValidationData)) {
      ValidationData <- data.table::as.data.table(ValidationData)
    }
  }
  
  # MultiClass Ensure TestData is a data.table----
  if (!is.null(TestData)) {
    if (!data.table::is.data.table(TestData)) {
      TestData <- data.table::as.data.table(TestData)
    }
  }
  
  # MultiClass Target Name Storage----
  if (is.character(TargetColumnName)) {
    Target <- TargetColumnName
  } else {
    Target <- names(data)[TargetColumnName]
  }
  
  # MultiClass IDcol Name Storage----
  if (!is.null(IDcols)) {
    if (!is.character(IDcols)) {
      IDcols <- names(data)[IDcols]
    }
  }
  
  # MultiClass Identify column numbers for factor variables----
  CatFeatures <- sort(c(as.numeric(which(sapply(data, is.factor))),
                        as.numeric(which(sapply(data, is.character)))))
  CatFeatures <- names(data)[CatFeatures]
  CatFeatures <- CatFeatures[CatFeatures != IDcols]
  if(length(CatFeatures)==0) {
    CatFeatures <- NULL
  }
  CatFeatures <- setdiff(CatFeatures, Target)
  
  # MultiClass Data Partition----
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
  
  # MultiClass data Subset Columns Needed----
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
  
  # MultiClass TestData Subset Columns Needed----
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
  
  # MultiClass Obtain Unique Target Levels
  if (!is.null(TestData)) {
    temp <- data.table::rbindlist(list(dataTrain, dataTest, TestData))
  } else {
    temp <- data.table::rbindlist(list(dataTrain, dataTest))
  }
  TargetLevels <-
    data.table::as.data.table(sort(unique(temp[[eval(Target)]])))
  data.table::setnames(TargetLevels, "V1", "OriginalLevels")
  TargetLevels[, NewLevels := 0:(.N - 1)]
  if (SaveModelObjects) {
    data.table::fwrite(TargetLevels,
                       file = paste0(model_path,
                                     "/",
                                     ModelID,
                                     "_TargetLevels.csv"))
  }
  
  # Number of levels----
  NumLevels <- TargetLevels[, .N]
  
  # MultiClass Convert Target to Numeric Factor----
  dataTrain <- merge(
    dataTrain,
    TargetLevels,
    by.x = eval(Target),
    by.y = "OriginalLevels",
    all = FALSE
  )
  dataTrain[, paste0(Target) := NewLevels]
  dataTrain[, NewLevels := NULL]
  dataTest <- merge(
    dataTest,
    TargetLevels,
    by.x = eval(Target),
    by.y = "OriginalLevels",
    all = FALSE
  )
  dataTest[, paste0(Target) := NewLevels]
  dataTest[, NewLevels := NULL]
  if (!is.null(TestData)) {
    TestData <- merge(
      TestData,
      TargetLevels,
      by.x = eval(Target),
      by.y = "OriginalLevels",
      all = FALSE
    )
    TestData[, paste0(Target) := NewLevels]
    TestData[, NewLevels := NULL]
  }
  
  # MultiClass Dummify dataTrain Categorical Features----
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
        ImportFactorLevels = FALSE, 
        ReturnFactorLevels = TRUE
      )
      FactorLevels <- temp$FactorLevelsList
      temp <- temp$data
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
        ImportFactorLevels = FALSE, 
        ReturnFactorLevels = TRUE
      )
      FactorLevels <- temp$FactorLevelsList
      temp <- temp$data
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
        ImportFactorLevels = FALSE, 
        ReturnFactorLevels = TRUE
      )
      FactorLevels <- temp$FactorLevelsList
      temp <- temp$data
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
        ImportFactorLevels = FALSE, 
        ReturnFactorLevels = TRUE
      )
      FactorLevels <- temp$FactorLevelsList
      temp <- temp$data
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
  
  # MultiClass Save Names of data----
  Names <- data.table::as.data.table(names(data))
  data.table::setnames(Names, "V1", "ColNames")
  if (SaveModelObjects) {
    data.table::fwrite(Names, paste0(model_path,
                                     "/"
                                     , ModelID, "_ColNames.csv"))
  }
  
  # MultiClass Subset Target Variables----
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
  
  # MultiClass Remove Target Variable from Feature Data
  dataTrain[, eval(Target) := NULL]
  dataTest[, eval(Target) := NULL]
  if (!is.null(TestData)) {
    TestData[, eval(Target) := NULL]
  }
  
  # MultiClass Initialize XGBoost Data Conversion----
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
  
  # MultiClass Grid Tune or Not Check----
  if (GridTune) {
    # MultiClass Grid Create data.table To Store Results----
    GridCollect <-
      data.table::data.table(
        ParamRow = 1:(MaxModelsInGrid + 1),
        EvalStat = rep(9999999, MaxModelsInGrid + 1)
      )
    
    # MultiClass Grid Define Hyper Parameters----
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
    
    # MultiClass Grid Tuning Main Loop----
    for (i in as.integer(seq_len(MaxModelsInGrid + 1))) {
      # Print i
      print(i)
      
      # MultiClass Grid Define Base Parameters----
      if (i == 1) {
        base_params <- list(
          booster = "gbtree",
          objective = Objective,
          eval_metric = tolower(eval_metric),
          num_class = (TargetLevels[, max(NewLevels)] +
                         1),
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
          objective = Objective,
          eval_metric = tolower(eval_metric),
          num_class = (TargetLevels[, max(NewLevels)] +
                         1),
          nthread = NThreads,
          max_bin = 64,
          tree_method = TreeMethod
        )
      }
      
      # MultiClass Grid Merge Model Parameters----
      # Have first model be the baseline model
      if (i != 1) {
        base_params <- c(as.list(grid_params[i, ]), base_params)
      }
      
      # MultiClass Grid Train Model----
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
      
      
      # MultiClass Grid Score Model----
      if (!is.null(TestData)) {
        predict <- stats::predict(model, datatest)
      } else {
        predict <- stats::predict(model, datavalidate)
      }
      
      # MultiClass Grid Validation Data----
      if (!is.null(TestData)) {
        calibEval <-
          data.table::as.data.table(cbind(Target = FinalTestTarget, p1 = predict))
      } else {
        calibEval <-
          data.table::as.data.table(cbind(Target = TestTarget, p1 = predict))
      }
      
      # MultiClass Accuracy
      Metric <-
        calibEval[, mean(data.table::fifelse(p1 == eval(Target), 1, 0), na.rm = TRUE)]
      
      # MultiClass Store Output Information----
      data.table::set(GridCollect,
                      i = i,
                      j = 1L,
                      value = i)
      data.table::set(GridCollect,
                      i = i,
                      j = 2L,
                      value = Metric)
    }
  }
  
  # MultiClass Define Final Model Parameters----
  if (GridTune) {
    if (eval_metric %chin% c("merror", "mlogloss")) {
      BestGrid <- GridCollect[order(-EvalStat)][1, ParamRow]
      if (BestGrid == 1) {
        base_params <- list(
          booster = "gbtree",
          objective = Objective,
          eval_metric = tolower(eval_metric),
          num_class = (TargetLevels[, max(NewLevels)] +
                         1),
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
          objective = Objective,
          eval_metric = tolower(eval_metric),
          num_class = (TargetLevels[, max(NewLevels)] +
                         1),
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
          objective = Objective,
          eval_metric = tolower(eval_metric),
          num_class = (TargetLevels[, max(NewLevels)] +
                         1),
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
          objective = Objective,
          eval_metric = tolower(eval_metric),
          num_class = (TargetLevels[, max(NewLevels)] +
                         1),
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
      objective = Objective,
      eval_metric = tolower(eval_metric),
      num_class = (TargetLevels[, max(NewLevels)] +
                     1),
      nthread = NThreads,
      max_bin = 64,
      tree_method = TreeMethod
    )
    if (!is.null(PassInGrid)) {
      base_params <- c(base_params, as.list(PassInGrid[1,]))
    }
  }
  
  # MultiClass Train Final Model----
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
  
  # MultiClass Save Model----
  if (SaveModelObjects) {
    xgboost::xgb.save(model = model, fname = ModelID)
  }
  
  # MultiClass Grid Score Model----
  if (!is.null(TestData)) {
    predict <- stats::predict(model, datatest)
  } else {
    predict <- stats::predict(model, datavalidate)
  }    
  
  # Convert predict object if softprob----
  if(Objective == "multi:softprob") {
    for(counter in seq.int(NumLevels)) {
      if(counter == 1) {
        Final <- data.table::as.data.table(
          predict[1:(length(predict)/NumLevels)])
        data.table::setnames(x = Final, 
                             old = "V1",
                             new = as.character(TargetLevels[counter,OriginalLevels]))
      } else {
        temp <- data.table::as.data.table(
          predict[(1 + (counter-1) * (length(predict)/NumLevels)):
                    (counter * (length(predict)/NumLevels))])
        data.table::setnames(x = temp, 
                             old = "V1",
                             new = as.character(TargetLevels[counter,OriginalLevels]))
        Final <- cbind(Final, temp)
      }
    }
  }

  # MultiClass Validation Data----
  if(Objective == "multi:softprob") {
    if (!is.null(TestData)) {
      ValidationData <-
        data.table::as.data.table(cbind(Target = FinalTestTarget, TestMerge, Final))
    } else {
      ValidationData <-
        data.table::as.data.table(cbind(Target = TestTarget, dataTest, Final))
    }
  } else {
    if (!is.null(TestData)) {
      ValidationData <-
        data.table::as.data.table(cbind(Target = FinalTestTarget, TestMerge, p1 = predict))
    } else {
      ValidationData <-
        data.table::as.data.table(cbind(Target = TestTarget, dataTest, p1 = predict))
    }    
  }

  # MultiClass Evaluation Metrics----
  if(Objective != "multi:softprob") {
    EvaluationMetrics <- data.table::data.table(
      Metric = "Accuracy",
      MetricValue = ValidationData[, mean(data.table::fifelse(p1 == eval(Target), 1, 0),
                                          na.rm = TRUE)])
  }
  
  # Save EvaluationMetrics to File
  if(Objective != "multi:softprob") {
    EvaluationMetrics <- EvaluationMetrics[MetricValue != 999999]
    if (SaveModelObjects) {
      if(!is.null(metadata_path)) {
        data.table::fwrite(EvaluationMetrics,
                           file = paste0(metadata_path, "/", ModelID, "_EvaluationMetrics.csv"))
      } else {
        data.table::fwrite(EvaluationMetrics,
                           file = paste0(model_path, "/", ModelID, "_EvaluationMetrics.csv"))        
      }
    }
  }
  
  # MultiClass Variable Importance----
  VariableImportance <- xgboost::xgb.importance(model = model)
  VariableImportance[, ':=' (
    Gain = round(Gain, 4),
    Cover = round(Cover, 4),
    Frequency = round(Frequency, 4)
  )]
  if (SaveModelObjects) {
    if(!is.null(metadata_path)) {
      data.table::fwrite(VariableImportance,
                         file = paste0(metadata_path,
                                       "/",
                                       ModelID, "_VariableImportance.csv"))
    } else {
      data.table::fwrite(VariableImportance,
                         file = paste0(model_path,
                                       "/",
                                       ModelID, "_VariableImportance.csv"))      
    }
  }
  
  # MultiClass Save GridCollect and grid_metrics----
  if (SaveModelObjects & GridTune == TRUE) {
    if(!is.null(metadata_path)) {
      data.table::fwrite(grid_params,
                         file = paste0(metadata_path, "/", ModelID, "_grid_params.csv"))
      data.table::fwrite(GridCollect,
                         file = paste0(metadata_path, "/", ModelID, "_GridCollect.csv"))
    } else {
      data.table::fwrite(grid_params,
                         file = paste0(model_path, "/", ModelID, "_grid_params.csv"))
      data.table::fwrite(GridCollect,
                         file = paste0(model_path, "/", ModelID, "_GridCollect.csv"))      
    }
  }
  
  # MultiClass Return Model Objects----
  if (GridTune) {
    if (ReturnModelObjects) {
      if(Objective != "multi:softprob") {
        return(
          list(
            Model = model,
            ValidationData = ValidationData,
            EvaluationMetrics = EvaluationMetrics,
            VariableImportance = VariableImportance,
            GridList = grid_params,
            GridMetrics = GridCollect,
            ColNames = Names,
            TargetLevels = TargetLevels,
            FactorLevels = FactorLevels
          )
        )        
      } else {
        return(
          list(
            Model = model,
            ValidationData = ValidationData,
            VariableImportance = VariableImportance,
            GridList = grid_params,
            GridMetrics = GridCollect,
            ColNames = Names,
            TargetLevels = TargetLevels,
            FactorLevels = FactorLevels
          )
        )
      }
    }
  } else {
    if (ReturnModelObjects) {
      if(Objective != "multi:softprob") {
        return(
          list(
            Model = model,
            ValidationData = ValidationData,
            EvaluationMetrics = EvaluationMetrics,
            VariableImportance = VariableImportance,
            ColNames = Names,
            TargetLevels = TargetLevels,
            FactorLevels = FactorLevels
          )
        )        
      } else {
        return(
          list(
            Model = model,
            ValidationData = ValidationData,
            VariableImportance = VariableImportance,
            ColNames = Names,
            TargetLevels = TargetLevels,
            FactorLevels = FactorLevels
          )
        )
      }
    }
  }
}
