#' AutoCatBoostMultiClass is an automated catboost model grid-tuning multinomial classifier and evaluation system
#'
#' AutoCatBoostMultiClass is an automated modeling function that runs a variety of steps. First, a stratified sampling (by the target variable) is done to create train and validation sets. Then, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation metrics, variable importance, and column names used in model fitting. You can download the catboost package using devtools, via: devtools::install_github('catboost/catboost', subdir = 'catboost/R-package').
#' 
#' @author Adrian Antico
#' @family Automated MultiClass Classification
#' @param data This is your data set for training and testing your model
#' @param ValidationData This is your holdout data set used in modeling either refine your hyperparameters. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TestData This is your holdout data set. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TargetColumnName Either supply the target column name OR the column number where the target is located, but not mixed types.
#' @param FeatureColNames Either supply the feature column names OR the column number where the target is located, but not mixed types. Also, not zero-indexed.
#' @param PrimaryDateColumn Supply a date or datetime column for catboost to utilize time as its basis for handling categorical features, instead of random shuffling
#' @param ClassWeights Supply a vector of weights for your target classes. E.g. c(0.25, 1) to weight your 0 class by 0.25 and your 1 class by 1.
#' @param IDcols A vector of column names or column numbers to keep in your data but not include in the modeling.
#' @param task_type Set to "GPU" to utilize your GPU for training. Default is "CPU".
#' @param eval_metric This is the metric used inside catboost to measure performance on validation data during a grid-tune. "MultiClass" or "MultiClassOneVsAll"
#' @param grid_eval_metric This is the metric used to find the threshold "auc","accuracy"
#' @param Trees The maximum number of trees you want in your models
#' @param GridTune Set to TRUE to run a grid tuning procedure. Set a number in MaxModelsInGrid to tell the procedure how many models you want to test.
#' @param MaxModelsInGrid Number of models to test from grid options. 1080 total possible options
#' @param model_path A character string of your path file to where you want your output saved
#' @param metadata_path A character string of your path file to where you want your model evaluation output saved. If left NULL, all output will be saved to model_path.
#' @param ModelID A character string to name your model and output
#' @param ReturnModelObjects Set to TRUE to output all modeling objects. E.g. plots and evaluation metrics
#' @param SaveModelObjects Set to TRUE to return all modeling objects to your environment
#' @param PassInGrid Defaults to NULL. Pass in a single row of grid from a previous output as a data.table (they are collected as data.tables)
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
#' data[, Target := as.factor(
#'   ifelse(Independent_Variable2 < 0.20, "A",
#'          ifelse(Independent_Variable2 < 0.40, "B",
#'                 ifelse(Independent_Variable2 < 0.6,  "C",
#'                        ifelse(Independent_Variable2 < 0.8,  "D", "E")))))]
#' data[, ':=' (x1 = NULL, x2 = NULL)]
#' TestModel <- AutoCatBoostMultiClass(data,
#'                                     ValidationData = NULL,
#'                                     TestData = NULL,
#'                                     TargetColumnName = "Target",
#'                                     FeatureColNames = c(2:11),
#'                                     PrimaryDateColumn = NULL,
#'                                     ClassWeights = NULL,
#'                                     IDcols = NULL,
#'                                     MaxModelsInGrid = 1,
#'                                     task_type = "GPU",
#'                                     eval_metric = "MultiClass",
#'                                     grid_eval_metric = "Accuracy",
#'                                     Trees = 50,
#'                                     GridTune = FALSE,
#'                                     model_path = NULL,
#'                                     metadata_path = NULL,
#'                                     ModelID = "ModelTest",
#'                                     ReturnModelObjects = TRUE,
#'                                     SaveModelObjects = FALSE,
#'                                     PassInGrid = NULL)
#' }
#' @return Saves to file and returned in list: VariableImportance.csv, Model (the model), ValidationData.csv, EvaluationMetrics.csv, GridCollect, and GridList
#' @export
AutoCatBoostMultiClass <- function(data,
                                   ValidationData = NULL,
                                   TestData = NULL,
                                   TargetColumnName = NULL,
                                   FeatureColNames = NULL,
                                   PrimaryDateColumn = NULL,
                                   ClassWeights = NULL,
                                   IDcols = NULL,
                                   task_type = "GPU",
                                   eval_metric = "MultiClassOneVsAll",
                                   Trees = 50,
                                   GridTune = FALSE,
                                   grid_eval_metric = "Accuracy",
                                   MaxModelsInGrid = 10,
                                   model_path = NULL,
                                   metadata_path = NULL,
                                   ModelID = "FirstModel",
                                   ReturnModelObjects = TRUE,
                                   SaveModelObjects = FALSE,
                                   PassInGrid = NULL) {
  # Load catboost----
  loadNamespace(package = "catboost")
  
  # MultiClass Check Arguments----
  if (!(tolower(task_type) %chin% c("gpu", "cpu")))
    warning("task_type needs to be either 'GPU' or 'CPU'")
  if (!(tolower(eval_metric) %chin% c("multiclass", "multiclassonevsall"))) {
    warning("eval_metric not in c('MultiClass','MultiClassOneVsAll')")
  }
  if (!is.null(PrimaryDateColumn)) {
    HasTime <- TRUE
  } else {
    HasTime <- FALSE
  }
  if (Trees < 1)
    warning("Trees must be greater than 1")
  if (!GridTune %in% c(TRUE, FALSE))
    warning("GridTune needs to be TRUE or FALSE")
  if (!(tolower(grid_eval_metric) %chin% c("accuracy", "auc"))) {
    warning("grid_eval_metric not in c('accuracy','auc')")
  }
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
  
  # Update working directory----
  # working_directory <- getwd()
  # if (!is.null(model_path)) {
  #   if (working_directory != model_path)
  #     setwd(model_path)
  # }
  
  # MultiClass Ensure data is a data.table----
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # MultiClass Ensure ValidationData is a data.table----
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
  
  # MultiClass Sort data if PrimaryDateColumn----
  if (!is.null(PrimaryDateColumn)) {
    data <- data[order(get(PrimaryDateColumn))]
    if (!(eval(PrimaryDateColumn) %in% IDcols)) {
      data.table::set(data,
                      j = eval(PrimaryDateColumn),
                      value = NULL)
    }
  }
  
  # MultiClass Sort ValidationData if PrimaryDateColumn----
  if (!is.null(PrimaryDateColumn)) {
    ValidationData <- ValidationData[order(get(PrimaryDateColumn))]
    if (!(eval(PrimaryDateColumn) %in% IDcols)) {
      data.table::set(ValidationData,
                      j = eval(PrimaryDateColumn),
                      value = NULL)
    }
  }
  
  # MultiClass Sort TestData if PrimaryDateColumn----
  if (!is.null(TestData)) {
    if (!is.null(PrimaryDateColumn)) {
      TestData <- TestData[order(get(PrimaryDateColumn))]
      if (!(eval(PrimaryDateColumn) %in% IDcols)) {
        data.table::set(TestData,
                        j = eval(PrimaryDateColumn),
                        value = NULL)
      }
    }
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
  
  # Identify column numbers for factor variables----
  CatFeatures <- sort(c(as.numeric(which(
    sapply(data, is.factor)
  )),
  as.numeric(which(
    sapply(data, is.character)
  ))))
  TargetNum <- which(names(data) == Target)
  CatFeatures <- setdiff(CatFeatures, TargetNum)
  
  # MultiClass Convert CatFeatures to 1-indexed----
  if (length(CatFeatures) > 0) {
    for (i in seq_len(length(CatFeatures))) {
      CatFeatures[i] <- CatFeatures[i] - 1
    }
  }
  
  # MultiClass Train ModelDataPrep----
  dataTrain <- ModelDataPrep(
    data = dataTrain,
    Impute = TRUE,
    CharToFactor = TRUE,
    RemoveDates = TRUE,
    MissFactor = "0",
    MissNum = -1
  )
  
  # MultiClass Validation ModelDataPrep----
  dataTest <- ModelDataPrep(
    data = dataTest,
    Impute = TRUE,
    CharToFactor = TRUE,
    RemoveDates = TRUE,
    MissFactor = "0",
    MissNum = -1
  )
  
  # MultiClass Test ModelDataPrep----
  if (!is.null(TestData)) {
    TestData <- ModelDataPrep(
      data = TestData,
      Impute = TRUE,
      CharToFactor = TRUE,
      RemoveDates = TRUE,
      MissFactor = "0",
      MissNum = -1
    )
  }
  
  # MultiClass Obtain Unique Target Levels
  if (!is.null(TestData)) {
    temp <- data.table::rbindlist(list(dataTrain, dataTest, TestData))
  } else {
    temp <- data.table::rbindlist(list(dataTrain, dataTest))
  }
  TargetLevels <-
    data.table::as.data.table(sort(unique(temp[[eval(TargetColumnName)]])))
  data.table::setnames(TargetLevels, "V1", "OriginalLevels")
  TargetLevels[, NewLevels := 1:.N]
  if (SaveModelObjects) {
    data.table::fwrite(TargetLevels,
                       file = paste0(model_path,
                                     "/",
                                     ModelID,
                                     "_TargetLevels.csv"))
  }
  
  # MultiClass Convert Target to Numeric Factor
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
  
  # Reorder Colnames
  data.table::setcolorder(dataTrain, c(2:ncol(dataTrain), 1))
  data.table::setcolorder(dataTest, c(2:ncol(dataTest), 1))
  if (!is.null(TestData)) {
    data.table::setcolorder(TestData, c(2:ncol(TestData), 1))
  }
  
  # MultiClass Save Names of data----
  Names <- data.table::as.data.table(names(data))
  data.table::setnames(Names, "V1", "ColNames")
  if (SaveModelObjects) {
    data.table::fwrite(Names, paste0(model_path, "/", ModelID, "_ColNames.csv"))
  }
  
  # MultiClass Subset Target Variables----
  TrainTarget <-
    tryCatch({
      dataTrain[, as.numeric(get(Target))]
    }, error = function(x)
      dataTrain[, as.numeric(eval(Target))])
  TestTarget <-
    tryCatch({
      dataTest[, as.numeric(get(Target))]
    }, error = function(x)
      dataTest[, as.numeric(eval(Target))])
  if (!is.null(TestData)) {
    FinalTestTarget <-
      tryCatch({
        TestData[, as.numeric(get(Target))]
      }, error = function(x)
        TestData[, as.numeric(eval(Target))])
  }
  
  # MultiClass Initialize Catboost Data Conversion----
  if (!is.null(CatFeatures)) {
    if (!is.null(TestData)) {
      TrainPool <-
        catboost::catboost.load_pool(dataTrain[, eval(Target) := NULL],
                                     label = TrainTarget,
                                     cat_features = CatFeatures)
      TestPool <-
        catboost::catboost.load_pool(dataTest[, eval(Target) := NULL], label = TestTarget, cat_features = CatFeatures)
      FinalTestPool <-
        catboost::catboost.load_pool(TestData[, eval(Target) := NULL], label = FinalTestTarget, cat_features = CatFeatures)
    } else {
      TrainPool <-
        catboost::catboost.load_pool(dataTrain[, eval(Target) := NULL],
                                     label = TrainTarget,
                                     cat_features = CatFeatures)
      TestPool <-
        catboost::catboost.load_pool(dataTest[, eval(Target) := NULL], label = TestTarget, cat_features = CatFeatures)
    }
  } else {
    if (!is.null(TestData)) {
      TrainPool <-
        catboost::catboost.load_pool(dataTrain[, eval(Target) := NULL], label = TrainTarget)
      TestPool <-
        catboost::catboost.load_pool(dataTest[, eval(Target) := NULL], label = TestTarget)
      FinalTestPool <-
        catboost::catboost.load_pool(TestData[, eval(Target) := NULL], label = FinalTestTarget)
    } else {
      TrainPool <-
        catboost::catboost.load_pool(dataTrain[, eval(Target) := NULL], label = TrainTarget)
      TestPool <-
        catboost::catboost.load_pool(dataTest[, eval(Target) := NULL], label = TestTarget)
    }
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
      catboostGridList <- data.table::CJ(
        l2_leaf_reg = c(0, 1, 2, 3),
        learning_rate = c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08),
        bootstrap_type = c("Poisson", "Bayesian", "Bernoulli", "No"),
        depth = c(4:12)
      )
      if (tolower(task_type) != "gpu") {
        catboostGridList <- catboostGridList[bootstrap_type != "Poisson"]
      }
      catboostGridList[, ID := runif(nrow(catboostGridList))]
      catboostGridList <-
        catboostGridList[order(ID)][1:(MaxModelsInGrid)][, ID := NULL]
      catboostGridList <-
        data.table::rbindlist(list(PassInGrid, catboostGridList))
    } else {
      catboostGridList <- data.table::CJ(
        l2_leaf_reg = c(0, 1, 2, 3),
        learning_rate = c(0.01, 0.02, 0.03, 0.04, 0.05),
        bootstrap_type = c("Poisson", "Bayesian", "Bernoulli", "No"),
        depth = c(4:8)
      )
      if (tolower(task_type) != "gpu") {
        catboostGridList <- catboostGridList[bootstrap_type != "Poisson"]
      }
      catboostGridList[, ID := runif(nrow(catboostGridList))]
      catboostGridList <-
        catboostGridList[order(ID)][1:(MaxModelsInGrid + 1)][, ID := NULL]
    }
    
    # MultiClass Grid Tuning Main Loop----
    for (i in as.integer(seq_len(MaxModelsInGrid + 1))) {
      # Print i
      print(i)
      
      # MultiClass Grid Define Base Parameters----
      if (!is.null(ClassWeights)) {
        base_params <- list(
          iterations           = Trees,
          loss_function        = eval_metric,
          eval_metric          = eval_metric,
          use_best_model       = TRUE,
          has_time             = HasTime,
          best_model_min_trees = 10,
          metric_period        = 10,
          task_type            = task_type,
          class_weights        = ClassWeights
        )
      } else {
        base_params <- list(
          iterations           = Trees,
          loss_function        = eval_metric,
          eval_metric          = eval_metric,
          use_best_model       = TRUE,
          has_time             = HasTime,
          best_model_min_trees = 10,
          metric_period        = 10,
          task_type            = task_type
        )
      }
      
      # MultiClass Grid Merge Model Parameters----
      # Have first model be the baseline model
      if (i != 1) {
        base_params <- c(as.list(catboostGridList[i, ]), base_params)
      }
      
      # MultiClass Grid Train Model----
      model <- catboost::catboost.train(learn_pool = TrainPool,
                                        test_pool  = TestPool,
                                        params     = base_params)
      
      # MultiClass Grid Score Model----
      tryCatch({
        if (!is.null(TestData)) {
          predict <- cbind(
            1 + catboost::catboost.predict(
              model = model,
              pool = FinalTestPool,
              prediction_type = "Class"
            ),
            catboost::catboost.predict(
              model = model,
              pool = FinalTestPool,
              prediction_type = "Probability"
            )
          )
        } else {
          predict <- cbind(
            1 + catboost::catboost.predict(
              model = model,
              pool = TestPool,
              prediction_type = "Class"
            ),
            catboost::catboost.predict(
              model = model,
              pool = TestPool,
              prediction_type = "Probability"
            )
          )
        }
        
        # MultiClass Remove Model and Collect Garbage----
        rm(model)
        gc()
        
        # MultiClass Grid Validation Data----
        if (!is.null(TestData)) {
          calibEval <-
            data.table::as.data.table(cbind(Target = FinalTestTarget, predict))
        } else {
          calibEval <-
            data.table::as.data.table(cbind(Target = TestTarget, predict))
        }
        ValidationData <- merge(
          calibEval,
          TargetLevels,
          by.x = "V2",
          by.y = "NewLevels",
          all = FALSE
        )
        ValidationData[, V2 := OriginalLevels][, OriginalLevels := NULL]
        ValidationData <- merge(
          ValidationData,
          TargetLevels,
          by.x = "Target",
          by.y = "NewLevels",
          all = FALSE
        )
        ValidationData[, Target := OriginalLevels][, OriginalLevels := NULL]
        
        # MultiClass Update Names for Predicted Value Columns
        k <- 2
        for (name in as.character(TargetLevels[[1]])) {
          k <- k + 1
          data.table::setnames(ValidationData, paste0("V", k), name)
        }
        data.table::setnames(ValidationData, "V2", "Predict")
        data.table::set(ValidationData,
                        j = "Target",
                        value = as.character(ValidationData[["Target"]]))
        data.table::set(ValidationData,
                        j = "Predict",
                        value = as.character(ValidationData[["Predict"]]))
        
        # MultiClass Metric----
        if (tolower(grid_eval_metric) == "accuracy") {
          Metric <- ValidationData[, mean(ifelse(as.character(Target) ==
                                                   as.character(Predict),
                                                 1,
                                                 0),
                                          na.rm = TRUE)]
        } else {
          # MultiClass Metric for MicroAUC----
          ValidationData[, vals := 0.5]
          z <- ncol(ValidationData)
          col <- "Target"
          for (l in seq_len(nrow(ValidationData))) {
            cols <- ValidationData[l, get(col)][[1]]
            valss <- ValidationData[l, ..cols][[1]]
            data.table::set(
              ValidationData,
              i = l,
              j = z,
              value = valss
            )
          }
          Metric <- round(as.numeric(noquote(
            stringr::str_extract(
              pROC::multiclass.roc(ValidationData[["Target"]], ValidationData[["vals"]])$auc,
              "\\d+\\.*\\d*"
            )
          )), 4)
        }
        
        # Collect Metrics and Corresponding Grids
        # Store Output Information
        data.table::set(GridCollect,
                        i = i,
                        j = 1L,
                        value = i)
        data.table::set(GridCollect,
                        i = i,
                        j = 2L,
                        value = Metric)
      }, error = function(x)
        "skip")
    }
  }
  
  # MultiClass Define Final Model Parameters----
  if (GridTune) {
    BestGrid <- GridCollect[order(-EvalStat)][1, ParamRow]
    if (BestGrid == 1) {
      BestThresh <- GridCollect[order(-EvalStat)][1, EvalStat]
      if (!is.null(ClassWeights)) {
        base_params <- list(
          iterations           = Trees,
          loss_function        = eval_metric,
          eval_metric          = eval_metric,
          use_best_model       = TRUE,
          has_time             = HasTime,
          best_model_min_trees = 10,
          metric_period        = 10,
          task_type            = task_type,
          class_weights        = ClassWeights
        )
      } else {
        base_params <- list(
          iterations           = Trees,
          loss_function        = eval_metric,
          eval_metric          = eval_metric,
          use_best_model       = TRUE,
          has_time             = HasTime,
          best_model_min_trees = 10,
          metric_period        = 10,
          task_type            = task_type
        )
      }
    } else {
      BestThresh <- GridCollect[order(-EvalStat)][1, EvalStat]
      if (!is.null(ClassWeights)) {
        base_params <- list(
          iterations           = Trees,
          loss_function        = eval_metric,
          eval_metric          = eval_metric,
          use_best_model       = TRUE,
          has_time             = HasTime,
          best_model_min_trees = 10,
          metric_period        = 10,
          task_type            = task_type,
          class_weights        = ClassWeights
        )
      } else {
        base_params <- list(
          iterations           = Trees,
          loss_function        = eval_metric,
          eval_metric          = eval_metric,
          use_best_model       = TRUE,
          has_time             = HasTime,
          best_model_min_trees = 10,
          metric_period        = 10,
          task_type            = task_type
        )
      }
      base_params <-
        c(as.list(catboostGridList[BestGrid, ]), base_params)
    }
  } else {
    if (!is.null(ClassWeights)) {
      base_params <- list(
        iterations           = Trees,
        loss_function        = eval_metric,
        eval_metric          = eval_metric,
        use_best_model       = TRUE,
        has_time             = HasTime,
        best_model_min_trees = 10,
        metric_period        = 10,
        task_type            = task_type,
        class_weights        = ClassWeights
      )
    } else {
      base_params <- list(
        iterations           = Trees,
        loss_function        = eval_metric,
        eval_metric          = eval_metric,
        use_best_model       = TRUE,
        has_time             = HasTime,
        best_model_min_trees = 10,
        metric_period        = 10,
        task_type            = task_type
      )
    }
    if (!is.null(PassInGrid)) {
      base_params <- c(base_params, as.list(PassInGrid[1,]))
    }
  }
  
  # MultiClass Train Final Model----
  model <- catboost::catboost.train(learn_pool = TrainPool,
                                    test_pool  = TestPool,
                                    params     = base_params)
  
  # MultiClass Save Model----
  if (SaveModelObjects) {
    catboost::catboost.save_model(model = model,
                                  model_path = paste0(model_path, "/", ModelID))
  }
  
  # MultiClass Score Final Test Data----
  if (!is.null(TestData)) {
    predict <- cbind(
      1 + catboost::catboost.predict(
        model = model,
        pool = FinalTestPool,
        prediction_type = "Class"
      ),
      catboost::catboost.predict(
        model = model,
        pool = FinalTestPool,
        prediction_type = "Probability"
      )
    )
  } else {
    predict <- cbind(
      1 + catboost::catboost.predict(
        model = model,
        pool = TestPool,
        prediction_type = "Class"
      ),
      catboost::catboost.predict(
        model = model,
        pool = TestPool,
        prediction_type = "Probability"
      )
    )
  }
  
  # MultiClass Grid Validation Data----
  if (!is.null(TestData)) {
    ValidationData <-
      data.table::as.data.table(cbind(Target = FinalTestTarget, predict, TestMerge))
  } else {
    ValidationData <-
      data.table::as.data.table(cbind(Target = TestTarget, predict))
  }
  ValidationData <- merge(
    ValidationData,
    TargetLevels,
    by.x = "V1",
    by.y = "NewLevels",
    all = FALSE
  )
  ValidationData[, V1 := OriginalLevels][, OriginalLevels := NULL]
  ValidationData <- merge(
    ValidationData,
    TargetLevels,
    by.x = "Target",
    by.y = "NewLevels",
    all = FALSE
  )
  ValidationData[, Target := OriginalLevels][, OriginalLevels := NULL]
  
  # MultiClass Update Names for Predicted Value Columns
  k <- 1
  for (name in as.character(TargetLevels[[1]])) {
    k <- k + 1
    data.table::setnames(ValidationData, paste0("V", k), name)
  }
  data.table::setnames(ValidationData, "V1", "Predict")
  data.table::set(ValidationData,
                  j = "Target",
                  value = as.character(ValidationData[["Target"]]))
  data.table::set(ValidationData,
                  j = "Predict",
                  value = as.character(ValidationData[["Predict"]]))
  
  # MultiClass Metrics Accuracy----
  MetricAcc <-
    ValidationData[, mean(ifelse(as.character(Target) ==
                                   as.character(Predict),
                                 1.0,
                                 0.0),
                          na.rm = TRUE)]
  
  # MultiClass Metrics MicroAUC----
  y <- ValidationData[[eval(Target)]]
  keep <- names(ValidationData)[3:(ncol(predict) + 1)]
  x <- as.matrix(ValidationData[, ..keep])
  z <- pROC::multiclass.roc(response = y, predictor = x)
  MetricAUC <- round(as.numeric(noquote(
    stringr::str_extract(z$auc, "\\d+\\.*\\d*")
  )), 4)
  
  # MultiClass Save Validation Data to File----
  if (SaveModelObjects) {
    if (!is.null(metadata_path)) {
      data.table::fwrite(ValidationData,
                         file = paste0(metadata_path, "/", ModelID, "_ValidationData.csv"))
    } else {
      data.table::fwrite(ValidationData,
                         file = paste0(model_path, "/", ModelID, "_ValidationData.csv"))      
    }
  }
  
  # MultiClass Evaluation Metrics----
  EvaluationMetrics <-
    data.table::data.table(
      Metric = c("AUC", "Accuracy"),
      MetricValue = c(MetricAUC, MetricAcc)
    )
  
  # MultiClass Save EvaluationMetrics to File
  if (SaveModelObjects) {
    if (!is.null(metadata_path)) {
      data.table::fwrite(EvaluationMetrics,
                         file = paste0(metadata_path, "/", ModelID, "_EvaluationMetrics.csv"))
    } else {
      data.table::fwrite(EvaluationMetrics,
                         file = paste0(model_path, "/", ModelID, "_EvaluationMetrics.csv"))      
    }
  }
  
  # MultiClass Variable Importance----
  temp <- catboost::catboost.get_feature_importance(model)
  VariableImportance <-
    data.table::data.table(cbind(Variable = rownames(temp), temp))
  data.table::setnames(VariableImportance, "V2", "Importance")
  VariableImportance[, Importance := round(as.numeric(Importance), 4)]
  VariableImportance <- VariableImportance[order(-Importance)]
  if (SaveModelObjects) {
    if (!is.null(metadata_path)) {
      data.table::fwrite(VariableImportance,
                         file = paste0(metadata_path, "/", ModelID, "_VariableImportance.csv"))
    } else {
      data.table::fwrite(VariableImportance,
                         file = paste0(model_path, "/", ModelID, "_VariableImportance.csv"))      
    }
  }
  
  # MultiClass Save GridCollect and catboostGridList----
  if (SaveModelObjects & GridTune == TRUE) {
    if (!is.null(metadata_path)) {
      data.table::fwrite(catboostGridList,
                         file = paste0(metadata_path,
                                       "/",
                                       ModelID,
                                       "_catboostGridList.csv"))
      data.table::fwrite(GridCollect,
                         file = paste0(metadata_path,
                                       "/",
                                       ModelID,
                                       "_GridCollect.csv"))
    } else {
      data.table::fwrite(catboostGridList,
                         file = paste0(model_path,
                                       "/",
                                       ModelID,
                                       "_catboostGridList.csv"))
      data.table::fwrite(GridCollect,
                         file = paste0(model_path,
                                       "/",
                                       ModelID,
                                       "_GridCollect.csv"))      
    }
  }
  
  # Final Garbage Collection----
  if (tolower(task_type) == "gpu") {
    gc()
  }
  
  # Reset working directory----
  # setwd(working_directory)
  
  # MultiClass Return Model Objects----
  if (GridTune) {
    if (ReturnModelObjects) {
      return(
        list(
          Model = model,
          ValidationData = ValidationData,
          EvaluationMetrics = EvaluationMetrics,
          VariableImportance = VariableImportance,
          GridList = catboostGridList,
          GridMetrics = GridCollect,
          ColNames = Names,
          TargetLevels = TargetLevels
        )
      )
    }
  } else {
    if (ReturnModelObjects) {
      return(
        list(
          Model = model,
          ValidationData = ValidationData,
          EvaluationMetrics = EvaluationMetrics,
          VariableImportance = VariableImportance,
          ColNames = Names,
          TargetLevels = TargetLevels
        )
      )
    }
  }
}