#' AutoCatBoostClassifier is an automated catboost model grid-tuning classifier and evaluation system
#'
#' AutoCatBoostClassifier is an automated modeling function that runs a variety of steps. First, a stratified sampling (by the target variable) is done to create train, validation, and test sets (if not supplied). Then, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions (on test data), an ROC plot, evaluation plot, evaluation metrics, variable importance, partial dependence calibration plots, partial dependence calibration box plots, and column names used in model fitting. You can download the catboost package using devtools, via: devtools::install_github('catboost/catboost', subdir = 'catboost/R-package')
#' @author Adrian Antico
#' @family Automated Binary Classification
#' @param data This is your data set for training and testing your model
#' @param TrainOnFull Set to TRUE to train on full data and skip over evaluation steps
#' @param ValidationData This is your holdout data set used in modeling either refine your hyperparameters. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TestData This is your holdout data set. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TargetColumnName Either supply the target column name OR the column number where the target is located, but not mixed types. Note that the target column needs to be a 0 | 1 numeric variable.
#' @param FeatureColNames Either supply the feature column names OR the column number where the target is located, but not mixed types. Also, not zero-indexed.
#' @param PrimaryDateColumn Supply a date or datetime column for catboost to utilize time as its basis for handling categorical features, instead of random shuffling
#' @param ClassWeights Supply a vector of weights for your target classes. E.g. c(0.25, 1) to weight your 0 class by 0.25 and your 1 class by 1.
#' @param IDcols A vector of column names or column numbers to keep in your data but not include in the modeling.
#' @param task_type Set to "GPU" to utilize your GPU for training. Default is "CPU".
#' @param eval_metric This is the metric used inside catboost to measure performance on validation data during a grid-tune. "AUC" is the default, but other options include "Logloss", "CrossEntropy", "Precision", "Recall", "F1", "BalancedAccuracy", "BalancedErrorRate", "MCC", "Accuracy", "CtrFactor", "AUC", "BrierScore", "HingeLoss", "HammingLoss", "ZeroOneLoss", "Kappa", "WKappa", "LogLikelihoodOfPrediction"
#' @param grid_eval_metric This is the metric used to find the threshold "f", "auc", "tpr", "fnr", "fpr", "tnr", "prbe", "f", "odds"
#' @param Trees The maximum number of trees you want in your models
#' @param GridTune Set to TRUE to run a grid tuning procedure. Set a number in MaxModelsInGrid to tell the procedure how many models you want to test.
#' @param MaxModelsInGrid Number of models to test from grid options.
#' @param model_path A character string of your path file to where you want your output saved
#' @param metadata_path A character string of your path file to where you want your model evaluation output saved. If left NULL, all output will be saved to model_path.
#' @param ModelID A character string to name your model and output
#' @param NumOfParDepPlots Tell the function the number of partial dependence calibration plots you want to create. Calibration boxplots will only be created for numerical features (not dummy variables)
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
#' data[, Independent_Variable11 := as.factor(
#'   ifelse(Independent_Variable2 < 0.20, "A",
#'          ifelse(Independent_Variable2 < 0.40, "B",
#'                 ifelse(Independent_Variable2 < 0.6,  "C",
#'                        ifelse(Independent_Variable2 < 0.8,  "D", "E")))))]
#' data[, ':=' (x1 = NULL, x2 = NULL)]
#' data[, Target := ifelse(Target < 0.5, 1, 0)]
#' TestModel <- AutoCatBoostClassifier(data,
#'                                     TrainOnFull = FALSE,
#'                                     ValidationData = NULL,
#'                                     TestData = NULL,
#'                                     TargetColumnName = "Target",
#'                                     FeatureColNames = c(2:12),
#'                                     PrimaryDateColumn = NULL,
#'                                     ClassWeights = NULL,
#'                                     IDcols = NULL,
#'                                     MaxModelsInGrid = 3,
#'                                     task_type = "GPU",
#'                                     eval_metric = "AUC",
#'                                     grid_eval_metric = "auc",
#'                                     Trees = 50,
#'                                     GridTune = FALSE,
#'                                     model_path = NULL,
#'                                     metadata_path = NULL,
#'                                     ModelID = "ModelTest",
#'                                     NumOfParDepPlots = 15,
#'                                     ReturnModelObjects = TRUE,
#'                                     SaveModelObjects = FALSE,
#'                                     PassInGrid = NULL)
#' }
#' @return Saves to file and returned in list: VariableImportance.csv, Model (the model), ValidationData.csv, ROC_Plot.png, EvalutionPlot.png, EvaluationMetrics.csv, ParDepPlots.R a named list of features with partial dependence calibration plots, GridCollect, and GridList
#' @export
AutoCatBoostClassifier <- function(data,
                                   TrainOnFull = FALSE,
                                   ValidationData = NULL,
                                   TestData = NULL,
                                   TargetColumnName = NULL,
                                   FeatureColNames = NULL,
                                   PrimaryDateColumn = NULL,
                                   ClassWeights = NULL,
                                   IDcols = NULL,
                                   task_type = "GPU",
                                   eval_metric = "AUC",
                                   Trees = 50,
                                   GridTune = FALSE,
                                   grid_eval_metric = "f",
                                   MaxModelsInGrid = 10,
                                   model_path = NULL,
                                   metadata_path = NULL,
                                   ModelID = "FirstModel",
                                   NumOfParDepPlots = 3,
                                   ReturnModelObjects = TRUE,
                                   SaveModelObjects = FALSE,
                                   PassInGrid = NULL) {
  # Load catboost----
  loadNamespace(package = "catboost")
  
  # Turn on full speed ahead----
  data.table::setDTthreads(percent = 100)
  
  # Binary Check Arguments----
  if (!(tolower(task_type) %chin% c("gpu", "cpu"))) stop("task_type needs to be either 'GPU' or 'CPU'")
  if (!(tolower(eval_metric) %chin% c("logloss","crossentropy","precision","recall","f1",
                                      "balancedaccuracy","balancederrorrate","mcc","accuracy","ctrfactor",
                                      "auc","brierscore","hingeloss","hammingloss","zerooneloss","kappa",
                                      "wkappa","loglikelihoodofprediction"))) {
    stop("eval_metric not in c('Logloss','CrossEntropy','Precision','Recall','F1','BalancedAccuracy','BalancedErrorRate','MCC',
    'Accuracy','CtrFactor','AUC','BrierScore','HingeLoss','HammingLoss','ZeroOneLoss','Kappa','WKappa','LogLikelihoodOfPrediction')")
  }
  if (!is.null(ClassWeights)) {
    LossFunction <- "Logloss"
  } else {
    LossFunction <- "CrossEntropy"
  }
  if (!is.null(PrimaryDateColumn)) {
    HasTime <- TRUE
  } else {
    HasTime <- FALSE
  }
  if (Trees < 1) stop("Trees must be greater than 1")
  if (!GridTune %in% c(TRUE, FALSE)) stop("GridTune needs to be TRUE or FALSE")
  if (!(tolower(grid_eval_metric) %chin% c("accuracy","auc","tpr","fnr","fpr","tnr","prbe","f","odds","chisq"))) {
    stop("grid_eval_metric not in c('accuracy','auc','tpr','fnr','fpr','tnr','prbe','f','odds','chisq')")
  }
  if((MaxModelsInGrid < 1 | MaxModelsInGrid > 1080) & GridTune == TRUE) {
    stop("MaxModelsInGrid needs to be at least 1 and less than 1080")
  }
  if(!is.null(model_path)) {
    if (!is.character(model_path)) stop("model_path needs to be a character type")
  }
  if (!is.null(metadata_path)) {
    if (!is.character(metadata_path)) stop("metadata_path needs to be a character type")
  }
  if (!is.character(ModelID)) stop("ModelID needs to be a character type")
  if (NumOfParDepPlots < 0) stop("NumOfParDepPlots needs to be a positive number")
  if (!(ReturnModelObjects %in% c(TRUE, FALSE))) stop("ReturnModelObjects needs to be TRUE or FALSE")
  if (!(SaveModelObjects %in% c(TRUE, FALSE))) stop("SaveModelObjects needs to be TRUE or FALSE")
  
  # Binary Ensure data is a data.table----
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Binary Ensure ValidationData is a data.table----
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
  
  # Binary Data Partition----
  if (is.null(ValidationData) & is.null(TestData) & TrainOnFull != TRUE) {
    dataSets <- AutoDataPartition(
      data,
      NumDataSets = 3,
      Ratios = c(0.70, 0.20, 0.10),
      PartitionType = "random",
      StratifyColumnNames = eval(Target),
      TimeColumnName = NULL)
    data <- dataSets$TrainData
    ValidationData <- dataSets$ValidationData
    TestData <- dataSets$TestData
  }
  
  # Binary Sort data if PrimaryDateColumn----
  if (!is.null(PrimaryDateColumn)) {
    data <- data[order(get(PrimaryDateColumn))]
    if (!(eval(PrimaryDateColumn) %in% IDcols)) {
      data.table::set(data,
                      j = eval(PrimaryDateColumn),
                      value = NULL)
    }
  }
  
  # Binary Sort ValidationData if PrimaryDateColumn----
  if (!is.null(PrimaryDateColumn) & TrainOnFull != TRUE) {
    ValidationData <- ValidationData[order(get(PrimaryDateColumn))]
    if (!(eval(PrimaryDateColumn) %in% IDcols)) {
      data.table::set(ValidationData,
                      j = eval(PrimaryDateColumn),
                      value = NULL)
    }
  }
  
  # Binary Sort TestData if PrimaryDateColumn----
  if (!is.null(TestData) & TrainOnFull != TRUE) {
    if (!is.null(PrimaryDateColumn)) {
      TestData <- TestData[order(get(PrimaryDateColumn))]
      if (!(eval(PrimaryDateColumn) %in% IDcols)) {
        data.table::set(TestData, j = eval(PrimaryDateColumn), value = NULL)
      }
    }
  }
  
  # Binary data Subset Columns Needed----
  if (is.numeric(FeatureColNames) | is.integer(FeatureColNames)) {
    keep1 <- names(data)[c(FeatureColNames)]
    keep <- c(keep1, Target)
    dataTrain <- data[, ..keep]
    if(TrainOnFull != TRUE) dataTest <- ValidationData[, ..keep]
  } else {
    keep <- c(FeatureColNames, Target)
    dataTrain <- data[, ..keep]
    if(TrainOnFull != TRUE) dataTest <- ValidationData[, ..keep]
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
  
  # Binary Identify column numbers for factor variables----
  CatFeatures <- sort(c(as.numeric(which(sapply(data, is.factor))),
                        as.numeric(which(sapply(data, is.character)))))
  
  # Binary Convert CatFeatures to 1-indexed----
  if (length(CatFeatures) > 0) {
    for (i in seq_len(length(CatFeatures))) {
      CatFeatures[i] <- CatFeatures[i] - 1
    }
  }
  
  # Binary Train ModelDataPrep----
  dataTrain <- ModelDataPrep(
    data = dataTrain,
    Impute = TRUE,
    CharToFactor = TRUE,
    RemoveDates = TRUE,
    MissFactor = "0",
    MissNum = -1)  
  
  # Binary Validation ModelDataPrep----
  if(TrainOnFull != TRUE) {
    dataTest <- ModelDataPrep(
      data = dataTest,
      Impute = TRUE,
      CharToFactor = TRUE,
      RemoveDates = TRUE,
      MissFactor = "0",
      MissNum = -1)  
  }
  
  # Binary Test ModelDataPrep----
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
  if (SaveModelObjects) {
    data.table::fwrite(Names, paste0(model_path, "/", ModelID, "_ColNames.csv"))
  }
  
  # Binary Subset Target Variables----
  TrainTarget <- tryCatch({dataTrain[, get(Target)]}, error = function(x) dataTrain[, eval(Target)])
  if(TrainOnFull != TRUE) {
    TestTarget <- tryCatch({dataTest[, get(Target)]}, error = function(x) dataTest[, eval(Target)])
    if (!is.null(TestData)) {
      FinalTestTarget <- tryCatch({TestData[, get(Target)]}, error = function(x) TestData[, eval(Target)])
    }  
  }
  
  # Binary Initialize Catboost Data Conversion----
  if (!is.null(CatFeatures)) {
    if (!is.null(TestData)) {
      TrainPool <- catboost::catboost.load_pool(dataTrain[, eval(Target) := NULL],
                                                label = TrainTarget,
                                                cat_features = CatFeatures)
      if(TrainOnFull != TRUE) {
        TestPool <- catboost::catboost.load_pool(dataTest[, eval(Target) := NULL], label = TestTarget, cat_features = CatFeatures)
        FinalTestPool <- catboost::catboost.load_pool(TestData[, eval(Target) := NULL], label = FinalTestTarget, cat_features = CatFeatures)        
      }
    } else {
      TrainPool <- catboost::catboost.load_pool(dataTrain[, eval(Target) := NULL],
                                                label = TrainTarget,
                                                cat_features = CatFeatures)
      if(TrainOnFull != TRUE) {
        TestPool <- catboost::catboost.load_pool(dataTest[, eval(Target) := NULL], label = TestTarget, cat_features = CatFeatures)        
      }
    }
  } else {
    if (!is.null(TestData)) {
      TrainPool <- catboost::catboost.load_pool(dataTrain[, eval(Target) := NULL], label = TrainTarget)
      if(TrainOnFull != TRUE) {
        TestPool <- catboost::catboost.load_pool(dataTest[, eval(Target) := NULL], label = TestTarget)
        FinalTestPool <- catboost::catboost.load_pool(TestData[, eval(Target) := NULL], label = FinalTestTarget)  
      }
    } else {
      TrainPool <- catboost::catboost.load_pool(dataTrain[, eval(Target) := NULL], label = TrainTarget)
      if(TrainOnFull != TRUE) {
        TestPool <- catboost::catboost.load_pool(dataTest[, eval(Target) := NULL], label = TestTarget)
      }
    }
  }
  
  # Binary Grid Tune or Not Check----
  if (GridTune == TRUE & TrainOnFull != TRUE) {
    
    # Binary Grid Create data.table To Store Results----
    GridCollect <- data.table::data.table(
      ParamRow = 1:(MaxModelsInGrid + 1),
      EvalStat = rep(9999999, MaxModelsInGrid + 1))
    
    # Binary Grid Define Hyper Parameters----
    if (!is.null(PassInGrid)) {
      if (!data.table::is.data.table(PassInGrid)) {
        PassInGrid <- data.table::as.data.table(PassInGrid)
      }
      catboostGridList <- data.table::CJ(
        l2_leaf_reg = c(0, 1, 2, 3),
        learning_rate = c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08),
        bootstrap_type = c("Bayesian", "Bernoulli", "No"),
        depth = c(4:15))
      if (tolower(task_type) != "gpu") {
        catboostGridList <- catboostGridList[bootstrap_type != "Poisson"]
      }
      if (tolower(task_type) == "gpu") {
        catboostGridList <- catboostGridList[depth <= 8L]
      }
      catboostGridList[, ID := runif(nrow(catboostGridList))]
      catboostGridList <- catboostGridList[order(ID)][1:(MaxModelsInGrid)][, ID := NULL]
      catboostGridList <- data.table::rbindlist(list(PassInGrid, catboostGridList))
    } else {
      catboostGridList <- data.table::CJ(
        l2_leaf_reg = c(0, 1, 2, 3),
        learning_rate = c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08),
        bootstrap_type = c("Bayesian", "Bernoulli", "No"),
        depth = c(4:15))
      if (tolower(task_type) != "gpu") {
        catboostGridList <- catboostGridList[bootstrap_type != "Poisson"]
      }
      if (tolower(task_type) == "gpu") {
        catboostGridList <- catboostGridList[depth <= 8L]
      }
      catboostGridList[, ID := runif(nrow(catboostGridList))]
      catboostGridList <- catboostGridList[order(ID)][1:(MaxModelsInGrid + 1)][, ID := NULL]
    }
    
    # Binary AUC List----
    AUC_List <- list()
    
    # Binary Grid Tuning Main Loop----
    for (i in as.integer(seq_len(MaxModelsInGrid + 1))) {
      
      # Print i
      print(i)
      
      # Binary Grid Define Base Parameters----
      if (!is.null(ClassWeights)) {
        base_params <- list(
          iterations           = Trees,
          loss_function        = LossFunction,
          eval_metric          = eval_metric,
          use_best_model       = TRUE,
          has_time             = HasTime,
          best_model_min_trees = 10,
          metric_period        = 1,
          train_dir            = model_path,
          task_type            = task_type,
          class_weights        = ClassWeights)
      } else {
        base_params <- list(
          iterations           = Trees,
          loss_function        = LossFunction,
          eval_metric          = eval_metric,
          use_best_model       = TRUE,
          has_time             = HasTime,
          best_model_min_trees = 10,
          metric_period        = 1,
          train_dir            = model_path,
          task_type            = task_type)
      }
      
      # Binary Grid Merge Model Parameters----
      if (i != 1) {
        base_params <- c(as.list(catboostGridList[i, ]), base_params)
      }
      
      # Binary Grid Train Model----
      model <- catboost::catboost.train(learn_pool = TrainPool,
                                        test_pool  = TestPool,
                                        params     = base_params)
      
      # Binary Grid Score Model----
      if (!is.null(TestData)) {
        predict <- catboost::catboost.predict(
          model = model,
          pool = FinalTestPool,
          prediction_type = "Probability",
          thread_count = -1)
      } else {
        predict <- catboost::catboost.predict(
          model = model,
          pool = TestPool,
          prediction_type = "Probability",
          thread_count = -1)
      }
      
      # Binary Remove Model and Collect Garbage----
      rm(model)
      gc()
      
      # Binary Grid Validation Data----
      if (!is.null(TestData)) {
        calibEval <- data.table::as.data.table(cbind(Target = FinalTestTarget, p1 = predict))
      } else {
        calibEval <- data.table::as.data.table(cbind(Target = TestTarget, p1 = predict))
      }
      
      # Binary Grid Evaluation Metrics for Each Grid----
      if (tolower(grid_eval_metric) == "accuracy") {
        j <- 0
        x <- data.table::data.table(
          Metric = "Accuracy",
          MetricValue = 5.0,
          Threshold = seq(0.01, 0.99, 0.001))
        for (k in unique(x[["Threshold"]])) {
          j = as.integer(j + 1)
          Accuracy <- mean(calibEval[, data.table::fifelse(p1 > k & Target == 1 | p1 < k & Target == 0, 1, 0)])
          data.table::set(x, i = j, j = 2L, value = round(Accuracy, 4))
        }
        data.table::setorderv(x, "MetricValue", order = -1, na.last = TRUE)
        Metric <- x[1, MetricValue]
      } else {
        x <- ROCR::prediction(predictions = calibEval[["p1"]], labels = calibEval[["Target"]])
        y <- ROCR::performance(prediction.obj = x, measure = grid_eval_metric)
        if (any(
          nrow(data.table::as.data.table(y@y.values)) <= 1 |
          nrow(data.table::as.data.table(y@x.values)) <= 1)) {
          if (nrow(data.table::as.data.table(y@y.values)) <= 1 &
              nrow(data.table::as.data.table(y@x.values)) <= 1) {
            z <- data.table::as.data.table(cbind(
              Metric = y@y.values,
              Threshold = y@x.values))
            Metric <- z[[1]]
          } else if (nrow(data.table::as.data.table(y@y.values)) <= 1 &
                     !(nrow(data.table::as.data.table(y@x.values) <= 1))) {
            z <- data.table::as.data.table(cbind(
              Metric = y@y.values,
              Threshold = y@x.values[[1]]))
            Metric <- z[!is.infinite(Threshold)][[1]]
          } else if (!(nrow(data.table::as.data.table(y@y.values)) <= 1) &
                     nrow(data.table::as.data.table(y@x.values) <= 1)) {
            if (grid_eval_metric %chin% c("auc", "tpr", "tnr", "prbe", "f", "odds")) {
              z <- data.table::as.data.table(cbind(
                Metric = y@y.values[[1]],
                Threshold = y@x.values))
              Metric <- z[order(-Metric)][!is.infinite(Metric)][[1]]
            } else {
              z <- data.table::as.data.table(cbind(
                Metric = y@y.values[[1]],
                Threshold = y@x.values))
              Metric <- z[order(Metric)][!is.infinite(Metric)][[1]]
            }
          }
        } else {
          if (grid_eval_metric %chin% c("auc", "tpr", "tnr", "prbe", "f", "odds")) {
            z <- data.table::as.data.table(cbind(
              Metric = y@y.values[[1]],
              Threshold = y@x.values[[1]]))
            Metric <- z[order(-Metric)][!is.infinite(Threshold) & !is.infinite(Metric)][1, ]
          } else {
            z <- data.table::as.data.table(cbind(
              Metric = y@y.values[[1]],
              Threshold = y@x.values[[1]]))
            Metric <- z[order(Metric)][!is.infinite(Threshold) & !is.infinite(Metric)][1, ]
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
        ci = TRUE)
      
      # Binary AUC Conversion to data.table----
      AUC_List[[i]] <- data.table::data.table(
        ModelNumber = i,
        Sensitivity = as.numeric(AUC_Metrics$sensitivities + 0.0001),
        Specificity = as.numeric(AUC_Metrics$specificities + 0.0001))
      
      # Store Output Information
      if (tolower(grid_eval_metric) == "accuracy") {
        data.table::set(GridCollect, i = i, j = 1L, value = i)
        data.table::set(GridCollect, i = i, j = 2L, value = Metric)
      } else if (any(nrow(data.table::as.data.table(y@y.values)) <= 1 |
                     nrow(data.table::as.data.table(y@x.values)) <= 1)) {
        data.table::set(GridCollect, i = i, j = 1L, value = i)
        data.table::set(GridCollect, i = i, j = 2L, value = Metric)
      } else {
        data.table::set(GridCollect, i = i, j = 1L, value = i)
        data.table::set(GridCollect, i = i, j = 2L, value = Metric[, 1])
      }
    }
  }
  
  # Binary Define Final Model Parameters----
  if (GridTune & TrainOnFull == FALSE) {
    if (grid_eval_metric %chin% c("accuracy", "auc", "tpr", "tnr", "prbe", "f", "odds")) {
      BestGrid <- GridCollect[order(-EvalStat)][1, ParamRow]
      if (BestGrid == 1) {
        BestThresh <- GridCollect[order(-EvalStat)][1, EvalStat]
        if (!is.null(ClassWeights)) {
          base_params <- list(
            iterations           = Trees,
            loss_function        = LossFunction,
            eval_metric          = eval_metric,
            use_best_model       = TRUE,
            has_time             = HasTime,
            best_model_min_trees = 10,
            metric_period        = 10,
            task_type            = task_type,
            class_weights        = ClassWeights)
        } else {
          base_params <- list(
            iterations           = Trees,
            loss_function        = LossFunction,
            eval_metric          = eval_metric,
            use_best_model       = TRUE,
            has_time             = HasTime,
            best_model_min_trees = 10,
            metric_period        = 10,
            task_type            = task_type)
        }
      } else {
        BestThresh <- GridCollect[order(-EvalStat)][1, EvalStat]
        if (!is.null(ClassWeights)) {
          base_params <- list(
            iterations           = Trees,
            loss_function        = LossFunction,
            eval_metric          = eval_metric,
            use_best_model       = TRUE,
            has_time             = HasTime,
            best_model_min_trees = 10,
            metric_period        = 10,
            task_type            = task_type,
            class_weights        = ClassWeights)
        } else {
          base_params <- list(
            iterations           = Trees,
            loss_function        = LossFunction,
            eval_metric          = eval_metric,
            use_best_model       = TRUE,
            has_time             = HasTime,
            best_model_min_trees = 10,
            metric_period        = 10,
            task_type            = task_type)
        }
        base_params <- c(as.list(catboostGridList[BestGrid, ]), base_params)
      }
    } else {
      BestGrid <- GridCollect[order(EvalStat)][1, ParamRow]
      BestThresh <- GridCollect[order(EvalStat)][1, EvalStat]
    }
    if (!is.null(ClassWeights)) {
      base_params <- list(
        iterations           = Trees,
        loss_function        = LossFunction,
        eval_metric          = eval_metric,
        use_best_model       = TRUE,
        has_time             = HasTime,
        best_model_min_trees = 10,
        metric_period        = 10,
        task_type            = task_type,
        class_weights        = ClassWeights)
    } else {
      base_params <- list(
        iterations           = Trees,
        loss_function        = LossFunction,
        eval_metric          = eval_metric,
        use_best_model       = TRUE,
        has_time             = HasTime,
        best_model_min_trees = 10,
        metric_period        = 10,
        task_type            = task_type)
    }
    base_params <- c(as.list(catboostGridList[BestGrid, ]), base_params)
  } else {
    if (!is.null(ClassWeights)) {
      base_params <- list(
        iterations           = Trees,
        loss_function        = LossFunction,
        eval_metric          = eval_metric,
        use_best_model       = TRUE,
        has_time             = HasTime,
        best_model_min_trees = 10,
        metric_period        = 10,
        task_type            = task_type,
        class_weights        = ClassWeights)
    } else {
      base_params <- list(
        iterations           = Trees,
        loss_function        = LossFunction,
        eval_metric          = eval_metric,
        use_best_model       = TRUE,
        has_time             = HasTime,
        best_model_min_trees = 10,
        metric_period        = 10,
        task_type            = task_type)
    }
    if (!is.null(PassInGrid)) {
      base_params <- c(base_params, as.list(PassInGrid[1,]))
    }
  }
  
  # Binary Train Final Model----
  if(!TrainOnFull) {
    model <- catboost::catboost.train(learn_pool = TrainPool,
                                      test_pool  = TestPool,
                                      params     = base_params)
  } else {
    model <- catboost::catboost.train(learn_pool = TrainPool,
                                      params     = base_params)
  }
  
  # Binary Save Model----
  if (SaveModelObjects) {
    catboost::catboost.save_model(model = model, model_path = paste0(model_path, "/", ModelID))
  }
  
  # Binary Score Final Test Data----
  if(TrainOnFull != TRUE) {
    if (!is.null(TestData)) {
      predict <- catboost::catboost.predict(
        model = model,
        pool = FinalTestPool,
        prediction_type = "Probability",
        thread_count = -1)
    } else if(TrainOnFull) {
      predict <- catboost::catboost.predict(
        model = model,
        pool = TrainPool,
        prediction_type = "Probability",
        thread_count = -1)
    } else {
      predict <- catboost::catboost.predict(
        model = model,
        pool = TestPool,
        prediction_type = "Probability",
        thread_count = -1)
    }
  }
  
  # Binary Validation Data----
  if(TrainOnFull != TRUE) {
    if (!is.null(TestData)) {
      ValidationData <- data.table::as.data.table(cbind(Target = FinalTestTarget, TestMerge, p1 = predict))
      data.table::setnames(ValidationData, "Target",eval(TargetColumnName))
    } else {
      ValidationData <- data.table::as.data.table(cbind(Target = TestTarget, dataTest, p1 = predict))
      data.table::setnames(ValidationData, "Target",eval(TargetColumnName))
    }  
  } else {
    data <- data.table::as.data.table(cbind(Target = TrainTarget, data, Predict = predict)) 
    data.table::setnames(data, "Target",eval(TargetColumnName))
  }
  
  # Save Validation Data to File----
  if (SaveModelObjects & TrainOnFull != TRUE) {
    if(!is.null(metadata_path)) {
      data.table::fwrite(ValidationData, file = paste0(metadata_path, "/", ModelID, "_ValidationData.csv"))
    } else {
      data.table::fwrite(ValidationData, file = paste0(model_path, "/", ModelID, "_ValidationData.csv"))      
    }
  }
  
  # Binary AUC Object Create----
  if(!TrainOnFull) {
    AUC_Metrics <- pROC::roc(
      response = ValidationData[[eval(TargetColumnName)]],
      predictor = ValidationData[["p1"]],
      na.rm = TRUE,
      algorithm = 3,
      auc = TRUE,
      ci = TRUE)  
  }
  
  # Binary AUC Conversion to data.table----
  if(!TrainOnFull) {
    AUC_Data <- data.table::data.table(
      ModelNumber = 0,
      Sensitivity = AUC_Metrics$sensitivities,
      Specificity = AUC_Metrics$specificities)  
  }
  
  # Binary Plot ROC Curve----
  if(!TrainOnFull) {
    if (GridTune == TRUE & MaxModelsInGrid <= 15) {
      temp <- data.table::rbindlist(AUC_List)
      AUC_Data <- data.table::rbindlist(list(temp, AUC_Data))
      AUC_Data[, ModelNumber := as.factor(ModelNumber)]
      ROC_Plot <- ggplot2::ggplot(AUC_Data, ggplot2::aes(x = 1 - Specificity,group = ModelNumber,color = ModelNumber)) +
        ggplot2::geom_line(ggplot2::aes(y = AUC_Data[["Sensitivity"]])) +
        ggplot2::geom_abline(slope = 1, color = "black") +
        ggplot2::ggtitle(paste0("Catboost Best Model AUC: ",100 * round(AUC_Metrics$auc, 3),"%")) +
        ChartTheme() + ggplot2::xlab("Specificity") +
        ggplot2::ylab("Sensitivity")
    } else {
      ROC_Plot <- ggplot2::ggplot(AUC_Data, ggplot2::aes(x = 1 - Specificity)) +
        ggplot2::geom_line(ggplot2::aes(y = AUC_Data[["Sensitivity"]]), color = "blue") +
        ggplot2::geom_abline(slope = 1, color = "black") +
        ggplot2::ggtitle(paste0("Catboost AUC: ", 100 * round(AUC_Metrics$auc, 3), "%")) + 
        ChartTheme() + ggplot2::xlab("Specificity") +
        ggplot2::ylab("Sensitivity")
    }  
  }
  
  # Save plot to file----
  if(!TrainOnFull) {
    if (SaveModelObjects) {
      if(!is.null(metadata_path)) {
        ggplot2::ggsave(paste0(metadata_path, "/", ModelID, "_ROC_Plot.png"))
      } else {
        ggplot2::ggsave(paste0(model_path, "/", ModelID, "_ROC_Plot.png"))      
      }
    }  
  }
  
  # Binary Evaluation Calibration Plot----
  if(!TrainOnFull) {
    EvaluationPlot <- EvalPlot(
      data = ValidationData,
      PredictionColName = "p1",
      TargetColName = eval(TargetColumnName),
      GraphType = "calibration",
      PercentileBucket = 0.05,
      aggrfun = function(x) mean(x, na.rm = TRUE))  
  }
  
  # Add Number of Trees to Title----
  if(!TrainOnFull) {
    EvaluationPlot <- EvaluationPlot +
      ggplot2::ggtitle(paste0("Calibration Evaluation Plot: AUC = ",round(AUC_Metrics$auc, 3)))  
  }
  
  # Save plot to file----
  if(!TrainOnFull) {
    if (SaveModelObjects) {
      if(!is.null(metadata_path)) {
        ggplot2::ggsave(paste0(metadata_path, "/", ModelID, "_EvaluationPlot.png"))
      } else {
        ggplot2::ggsave(paste0(model_path, "/", ModelID, "_EvaluationPlot.png"))      
      }
    }  
  }
  
  # Evaluation Metrics at Optimial Threshold----
  if(!TrainOnFull) {
    x <- ROCR::prediction(predictions = ValidationData[["p1"]],
                          labels = ValidationData[[eval(TargetColumnName)]])
    EvaluationMetrics <- data.table::data.table(
      Metric = c("AUC","TruePositiveRate","FalseNegativeRate","FalsePositiveRate","TrueNegativeRate",
                 "PreceisionRecallBreakEven","F1_Score","Odds"),
      MetricValue = rep(999999, 8),
      Threshold   = rep(999999, 8))
    i <- 0
    for (metric in c("auc", "tpr", "fnr", "fpr", "tnr", "prbe", "f", "odds")) {
      i <- as.integer(i + 1)
      tryCatch({
        y <- ROCR::performance(prediction.obj = x, measure = metric)
        if (any(nrow(data.table::as.data.table(y@y.values)) <= 1 |
                nrow(data.table::as.data.table(y@x.values)) <= 1)) {
          if (nrow(data.table::as.data.table(y@y.values)) <= 1 &
              nrow(data.table::as.data.table(y@x.values)) <= 1) {
            z <- data.table::as.data.table(cbind(Metric = y@y.values,Threshold = y@x.values))
            Metric <- z[[1]]
          } else if (nrow(data.table::as.data.table(y@y.values)) <= 1 &
                     !(nrow(data.table::as.data.table(y@x.values) <= 1))) {
            z <- data.table::as.data.table(cbind(
              Metric = y@y.values,
              Threshold = y@x.values[[1]]))
            Metric <- z[!is.infinite(Threshold)][[1]]
          } else if(!(nrow(data.table::as.data.table(y@y.values)) <= 1) &
                    nrow(data.table::as.data.table(y@x.values) <= 1)) {
            if (metric %chin% c("auc", "tpr", "tnr", "prbe", "f", "odds")) {
              z <- data.table::as.data.table(cbind(
                Metric = y@y.values[[1]],
                Threshold = y@x.values))
              Metric <- z[order(-Metric)][!is.infinite(Metric)][[1]]
            } else {
              z <- data.table::as.data.table(cbind(Metric = y@y.values[[1]],Threshold = y@x.values))
              Metric <- z[order(Metric)][!is.infinite(Metric)][[1]]
            }
          }
        } else {
          if (metric %chin% c("auc", "tpr", "tnr", "prbe", "f", "odds")) {
            z <- data.table::as.data.table(cbind(
              Metric = y@y.values[[1]],
              Threshold = y@x.values[[1]]))
            Metric <- z[order(-Metric)][!is.infinite(Threshold) & !is.infinite(Metric)][1,]
          } else {
            z <- data.table::as.data.table(cbind(Metric = y@y.values[[1]],Threshold = y@x.values[[1]]))
            Metric <- z[order(Metric)][!is.infinite(Threshold) & !is.infinite(Metric)][1,]
          }
        }
        
        # Store Output Information
        if (any(nrow(data.table::as.data.table(y@y.values)) <= 1 |
                nrow(data.table::as.data.table(y@x.values)) <= 1)) {
          data.table::set(EvaluationMetrics,i = i,j = 2L,value = round(Metric[[1]], 4))
          data.table::set(EvaluationMetrics,i = i,j = 3L,value = NA)
        } else {
          data.table::set(EvaluationMetrics,i = i,j = 2L,value = round(Metric[[1]], 4))
          data.table::set(EvaluationMetrics,i = i,j = 3L,value = Metric[[2]])
        }
      }, error = function(x) "skip")
    }
  }
  
  # Binary Accuracy Threshold and Metric----
  if(!TrainOnFull) {
    j <- 0
    x <- data.table::data.table(
      Metric = "Accuracy",
      MetricValue = 5.0,
      Threshold = seq(0.01, 0.99, 0.001))
    for (i in unique(x[["Threshold"]])) {
      j = as.integer(j + 1)
      Accuracy <- mean(ValidationData[, data.table::fifelse(
        (p1 > i & get(TargetColumnName) == 1) | (p1 < i & get(TargetColumnName) == 0), 1, 0)])
      data.table::set(x, i = j, j = 2L, value = round(Accuracy, 4))
    }
    data.table::setorderv(x, "MetricValue", order = -1, na.last = TRUE)
    x <- x[1, ]
    EvaluationMetrics <- data.table::rbindlist(list(EvaluationMetrics, x))
    
    # Save EvaluationMetrics to File
    EvaluationMetrics <- EvaluationMetrics[MetricValue != 999999]
    if (SaveModelObjects) {
      if(!is.null(metadata_path)) {
        data.table::fwrite(EvaluationMetrics, file = paste0(metadata_path, "/", ModelID, "_EvaluationMetrics.csv"))
      } else {
        data.table::fwrite(EvaluationMetrics, file = paste0(model_path, "/", ModelID, "_EvaluationMetrics.csv"))      
      }
    }
  }
  
  # Binary Variable Importance----
  temp <- catboost::catboost.get_feature_importance(model)
  VariableImportance <- data.table::data.table(cbind(Variable = rownames(temp), temp))
  data.table::setnames(VariableImportance, "V2", "Importance")
  VariableImportance[, Importance := round(as.numeric(Importance), 4)]
  VariableImportance <- VariableImportance[order(-Importance)]
  if (SaveModelObjects) {
    if(!is.null(metadata_path)) {
      data.table::fwrite(VariableImportance, file = paste0(metadata_path, "/", ModelID, "_VariableImportance.csv"))
    } else {
      data.table::fwrite(VariableImportance, file = paste0(model_path, "/", ModelID, "_VariableImportance.csv"))      
    }
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
        TargetColName = eval(TargetColumnName),
        IndepVar = VariableImportance[i, Variable],
        GraphType = "calibration",
        PercentileBucket = 0.05,
        FactLevels = 10,
        Function = function(x) mean(x, na.rm = TRUE))
      j <- j + 1
      ParDepPlots[[paste0(VariableImportance[j, Variable])]] <- Out
    }, error = function(x) "skip")
  }
  
  # Binary Save ParDepPlots to file----
  if (SaveModelObjects) {
    if(!is.null(metadata_path)) {
      save(ParDepPlots, file = paste0(metadata_path, "/", ModelID, "_ParDepPlots.R"))
    } else {
      save(ParDepPlots, file = paste0(model_path, "/", ModelID, "_ParDepPlots.R"))      
    }
  }
  
  # Binary Save GridCollect and catboostGridList----
  if (SaveModelObjects & GridTune == TRUE) {
    if(!is.null(metadata_path)) {
      data.table::fwrite(catboostGridList, file = paste0(metadata_path, "/", ModelID, "_catboostGridList.csv"))
      data.table::fwrite(GridCollect, file = paste0(metadata_path, "/", ModelID, "_GridCollect.csv"))
    } else {
      data.table::fwrite(catboostGridList, file = paste0(model_path, "/", ModelID, "_catboostGridList.csv"))
      data.table::fwrite(GridCollect, file = paste0(model_path, "/", ModelID, "_GridCollect.csv"))      
    }
  }
  
  # Final Garbage Collection----
  if (tolower(task_type) == "gpu") {
    gc()
  }
  
  # VI_Plot_Function
  VI_Plot <- function(VI_Data, ColorHigh = "darkblue", ColorLow = "white") {
    ggplot2::ggplot(VI_Data[1:min(10,.N)], ggplot2::aes(x = reorder(Variable, Importance), y = Importance, fill = Importance)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::scale_fill_gradient2(mid = ColorLow,high = ColorHigh) +
      ChartTheme(Size = 12,AngleX = 0,LegendPosition = "right") +
      ggplot2::coord_flip() +
      ggplot2::labs(title = "Global Variable Importance") +
      ggplot2::xlab("Top Model Features") +
      ggplot2::ylab("Value")
  }
  
  # Binary Return Model Objects----
  if (GridTune) {
    if (ReturnModelObjects) {
      GridMetrics <- cbind(catboostGridList,GridCollect)
      data.table::setorderv(GridMetrics, cols = "EvalStat", order = -1, na.last = TRUE)
      return(
        list(
          Model = model,
          ValidationData = ValidationData,
          ROC_Plot = ROC_Plot,
          EvaluationPlot = EvaluationPlot,
          EvaluationMetrics = EvaluationMetrics,
          VariableImportance = VariableImportance,
          VI_Plot = VI_Plot(VariableImportance),
          PartialDependencePlots = ParDepPlots,
          GridMetrics = GridMetrics,
          ColNames = Names))
    }
  } else if(TrainOnFull) {
    if (ReturnModelObjects) {
      return(
        list(
          Model = model,
          ValidationData = ValidationData,
          VariableImportance = VariableImportance,
          VI_Plot = VI_Plot(VariableImportance),
          ColNames = Names))
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
          VI_Plot = VI_Plot(VariableImportance),
          PartialDependencePlots = ParDepPlots,
          ColNames = Names))
    }
  }
}
