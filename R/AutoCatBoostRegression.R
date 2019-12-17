#' AutoCatBoostRegression is an automated catboost model grid-tuning classifier and evaluation system
#'
#' AutoCatBoostRegression is an automated modeling function that runs a variety of steps. First, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation plot, evaluation boxplot, evaluation metrics, variable importance, partial dependence calibration plots, partial dependence calibration box plots, and column names used in model fitting. You can download the catboost package using devtools, via: devtools::install_github('catboost/catboost', subdir = 'catboost/R-package')
#' @author Adrian Antico
#' @family Automated Regression
#' @param data This is your data set for training and testing your model
#' @param TrainOnFull Set to TRUE to train on full data and skip over evaluation steps
#' @param ValidationData This is your holdout data set used in modeling either refine your hyperparameters. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TestData This is your holdout data set. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TargetColumnName Either supply the target column name OR the column number where the target is located (but not mixed types).
#' @param FeatureColNames Either supply the feature column names OR the column number where the target is located (but not mixed types)
#' @param PrimaryDateColumn Supply a date or datetime column for catboost to utilize time as its basis for handling categorical features, instead of random shuffling
#' @param IDcols A vector of column names or column numbers to keep in your data but not include in the modeling.
#' @param TransformNumericColumns Set to NULL to do nothing; otherwise supply the column names of numeric variables you want transformed
#' @param task_type Set to "GPU" to utilize your GPU for training. Default is "CPU".
#' @param eval_metric This is the metric used inside catboost to measure performance on validation data during a grid-tune. "RMSE" is the default, but other options include: "MAE", "MAPE", "Poisson", "Quantile", "LogLinQuantile", "Lq", "NumErrors", "SMAPE", "R2", "MSLE", "MedianAbsoluteError".
#' @param grid_eval_metric This is the metric used to find the threshold 'poisson', 'mae', 'mape', 'mse', 'msle', 'kl', 'cs', 'r2'
#' @param Trees The maximum number of trees you want in your models
#' @param GridTune Set to TRUE to run a grid tuning procedure. Set a number in MaxModelsInGrid to tell the procedure how many models you want to test.
#' @param MaxModelsInGrid Number of models to test from grid options (1080 total possible options)
#' @param model_path A character string of your path file to where you want your output saved
#' @param metadata_path A character string of your path file to where you want your model evaluation output saved. If left NULL, all output will be saved to model_path.
#' @param ModelID A character string to name your model and output
#' @param NumOfParDepPlots Tell the function the number of partial dependence calibration plots you want to create. Calibration boxplots will only be created for numerical features (not dummy variables)
#' @param ReturnModelObjects Set to TRUE to output all modeling objects (E.g. plots and evaluation metrics)
#' @param SaveModelObjects Set to TRUE to return all modeling objects to your environment
#' @param PassInGrid Defaults to NULL. Pass in a single row of grid from a previous output as a data.table (they are collected as data.tables)
#' @param Methods Default is all transformation methods. You can select a subset of them. Choices are in the default model in the help file.
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
#' TestModel <- AutoCatBoostRegression(data,
#'                                     TrainOnFull = FALSE,
#'                                     ValidationData = NULL,
#'                                     TestData = NULL,
#'                                     TargetColumnName = "Target",
#'                                     FeatureColNames = c(2:12),
#'                                     PrimaryDateColumn = NULL,
#'                                     IDcols = NULL,
#'                                     TransformNumericColumns = NULL,
#'                                     MaxModelsInGrid = 1,
#'                                     task_type = "GPU",
#'                                     eval_metric = "RMSE",
#'                                     grid_eval_metric = "r2",
#'                                     Trees = 50,
#'                                     GridTune = FALSE,
#'                                     model_path = NULL,
#'                                     metadata_path = NULL,
#'                                     ModelID = "ModelTest",
#'                                     NumOfParDepPlots = 3,
#'                                     ReturnModelObjects = TRUE,
#'                                     SaveModelObjects = FALSE,
#'                                     PassInGrid = NULL,
#'                                     Methods = c("BoxCox", 
#'                                                 "Asinh", 
#'                                                 "Asin",
#'                                                 "Log",
#'                                                 "LogPlus1",
#'                                                 "Logit",
#'                                                 "YeoJohnson"))
#' }
#' @return Saves to file and returned in list: VariableImportance.csv, Model, ValidationData.csv, EvalutionPlot.png, EvalutionBoxPlot.png, EvaluationMetrics.csv, ParDepPlots.R a named list of features with partial dependence calibration plots, ParDepBoxPlots.R, GridCollect, catboostgrid, and a transformation details file.
#' @export
AutoCatBoostRegression <- function(data,
                                   TrainOnFull = FALSE,
                                   ValidationData = NULL,
                                   TestData = NULL,
                                   TargetColumnName = NULL,
                                   FeatureColNames = NULL,
                                   PrimaryDateColumn = NULL,
                                   IDcols = NULL,
                                   TransformNumericColumns = NULL,
                                   task_type = "GPU",
                                   eval_metric = "RMSE",
                                   Trees = 50,
                                   GridTune = FALSE,
                                   grid_eval_metric = "mae",
                                   MaxModelsInGrid = 10,
                                   model_path = NULL,
                                   metadata_path = NULL,
                                   ModelID = "FirstModel",
                                   NumOfParDepPlots = 3,
                                   ReturnModelObjects = TRUE,
                                   SaveModelObjects = FALSE,
                                   PassInGrid = NULL,
                                   Methods = c("BoxCox", "Asinh", "Asin", "Log", "LogPlus1", "Logit", "YeoJohnson")) {
  # Load catboost----
  loadNamespace(package = "catboost")
  
  # Regression Check Arguments----
  if (!is.null(PrimaryDateColumn)) {
    HasTime <- TRUE
  } else {
    HasTime <- FALSE
  }
  if (Trees < 1) stop("Trees must be greater than 1")
  if (!GridTune %in% c(TRUE, FALSE)) stop("GridTune needs to be TRUE or FALSE")
  if (!(tolower(grid_eval_metric) %chin% c("poisson", "mae", "mape", "mse", "msle", "kl", "cs", "r2"))) {
    stop("grid_eval_metric not in c('poisson','mae','mape','mse','msle','kl','cs','r2')")
  }
  if((MaxModelsInGrid < 1 | MaxModelsInGrid > 1080) & GridTune == TRUE) {
    stop("MaxModelsInGrid needs to be at least 1 and less than 1080")
  }
  if (!is.null(model_path)) {
    if (!is.character(model_path)) stop("model_path needs to be a character type")
  }
  if (!is.null(metadata_path)) {
    if (!is.character(metadata_path)) stop("metadata_path needs to be a character type")
  }
  if(!is.character(ModelID)) stop("ModelID needs to be a character type")
  if(NumOfParDepPlots < 0) stop("NumOfParDepPlots needs to be a positive number")
  if (!(ReturnModelObjects %in% c(TRUE, FALSE))) stop("ReturnModelObjects needs to be TRUE or FALSE")
  if (!(SaveModelObjects %in% c(TRUE, FALSE))) stop("SaveModelObjects needs to be TRUE or FALSE")
  
  # Regression Ensure data is a data.table----
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Regression Ensure ValidationData is a data.table----
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
  
  # Convert TransformNumericColumns to Names if not character----
  if (!is.null(TransformNumericColumns)) {
    if(!is.character(TransformNumericColumns)) {
      TransformNumericColumns <- names(data)[TransformNumericColumns]      
    }
  }
  
  # Transform data, ValidationData, and TestData----
  if (!is.null(ValidationData) &
      !is.null(TransformNumericColumns)) {
    MeanTrainTarget <- mean(data[[eval(TargetColumnName)]], na.rm = TRUE)
    Output <- AutoTransformationCreate(
      data,
      ColumnNames = TransformNumericColumns,
      Methods = Methods,
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
  
  # Regression Data Partition----
  if (is.null(ValidationData) & is.null(TestData) & TrainOnFull != TRUE) {
    if (!is.null(TransformNumericColumns)) {
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
      MeanTrainTarget <- mean(data[[eval(TargetColumnName)]], na.rm = TRUE)
      
      # Transform data sets----
      Output <- AutoTransformationCreate(
        data,
        ColumnNames = TransformNumericColumns,
        Methods = Methods,
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
      MeanTrainTarget <- mean(data[[eval(TargetColumnName)]], na.rm = TRUE)
    }
  }
  
  # Regression Sort data if PrimaryDateColumn----
  if (!is.null(PrimaryDateColumn)) {
    data <- data[order(get(PrimaryDateColumn))]
    if (!(eval(PrimaryDateColumn) %in% IDcols)) {
      data.table::set(data,
                      j = eval(PrimaryDateColumn),
                      value = NULL)
    }
  }
  
  # Regression Sort ValidationData if PrimaryDateColumn----
  if (!is.null(PrimaryDateColumn) & TrainOnFull != TRUE) {
    ValidationData <- ValidationData[order(get(PrimaryDateColumn))]
    if (!(eval(PrimaryDateColumn) %in% IDcols)) {
      data.table::set(ValidationData,
                      j = eval(PrimaryDateColumn),
                      value = NULL)
    }
  }
  
  # Regression Sort TestData if PrimaryDateColumn----
  if (!is.null(TestData) & TrainOnFull != TRUE) {
    if (!is.null(PrimaryDateColumn)) {
      TestData <- TestData[order(get(PrimaryDateColumn))]
      if (!(eval(PrimaryDateColumn) %in% IDcols)) {
        data.table::set(TestData,
                        j = eval(PrimaryDateColumn),
                        value = NULL)
      }
    }
  }
  
  # Regression data Subset Columns Needed----
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
  
  # Regression Identify column numbers for factor variables----
  CatFeatures <- sort(c(as.numeric(which(sapply(dataTrain, is.factor))),
                        as.numeric(which(sapply(dataTrain, is.character)))))
  
  # Regression Convert CatFeatures to 1-indexed----
  if (length(CatFeatures) > 0) {
    for (i in seq_len(length(CatFeatures))) {
      CatFeatures[i] <- CatFeatures[i] - 1
    }
  }
  
  # Regression Train ModelDataPrep----
  dataTrain <- ModelDataPrep(
    data = dataTrain,
    Impute = TRUE,
    CharToFactor = TRUE,
    RemoveDates = TRUE,
    MissFactor = "0",
    MissNum = -1
  )
  
  # Regression Validation ModelDataPrep----
  if(TrainOnFull != TRUE) {
    if(!is.null(dataTest)) {
      dataTest <- ModelDataPrep(
        data = dataTest,
        Impute = TRUE,
        CharToFactor = TRUE,
        RemoveDates = TRUE,
        MissFactor = "0",
        MissNum = -1)    
    }    
  }

  # Regression Test ModelDataPrep----
  if (!is.null(TestData)) {
    TestData <- ModelDataPrep(
      data = TestData,
      Impute = TRUE,
      CharToFactor = TRUE,
      RemoveDates = TRUE,
      MissFactor = "0",
      MissNum = -1)
  }
  
  # Regression Save Names of data----
  Names <- data.table::as.data.table(names(data))
  data.table::setnames(Names, "V1", "ColNames")
  if (SaveModelObjects) {
    data.table::fwrite(Names, paste0(model_path,"/", ModelID, "_ColNames.csv"))
  }
  
  # Regression Get Min Value of Target Data----
  MinVal <- min(data[[eval(Target)]], na.rm = TRUE)
  
  # Regression Subset Target Variables----
  TrainTarget <- dataTrain[, .SD, .SDcols = eval(Target)][[1]]
  if(TrainOnFull != TRUE) {
    TestTarget <- dataTest[, .SD, .SDcols = eval(Target)][[1]]
    if (!is.null(TestData)) {
      FinalTestTarget <- TestData[, .SD, .SDcols = eval(Target)][[1]]
    }
  }

  # Regression eval_metric checks
  if(TrainOnFull != TRUE) {
    if (tolower(eval_metric) == "poisson" & (min(TrainTarget) < 0 |
                                             min(TestTarget) < 0)) {
      warning("eval_metric Poisson requires positive values for Target")
    }    
  }
  
  # Regression Initialize Catboost Data Conversion----
  if (!is.null(CatFeatures)) {
    if (!is.null(TestData)) {
      TrainPool <- catboost::catboost.load_pool(dataTrain[, eval(Target) := NULL],
                                                label = TrainTarget,
                                                cat_features = CatFeatures)
      
      if(TrainOnFull != TRUE) TestPool <- catboost::catboost.load_pool(dataTest[, eval(Target) := NULL], label = TestTarget, cat_features = CatFeatures)
      if(TrainOnFull != TRUE) FinalTestPool <- catboost::catboost.load_pool(TestData[, eval(Target) := NULL], label = FinalTestTarget, cat_features = CatFeatures)
    } else {
      TrainPool <- catboost::catboost.load_pool(dataTrain[, eval(Target) := NULL],
                                                label = TrainTarget,
                                                cat_features = CatFeatures)
      
      if(TrainOnFull != TRUE) TestPool <- catboost::catboost.load_pool(dataTest[, eval(Target) := NULL], label = TestTarget, cat_features = CatFeatures)
    }
  } else {
    if (!is.null(TestData)) {
      TrainPool <- catboost::catboost.load_pool(dataTrain[, eval(Target) := NULL], label = TrainTarget)
      if(TrainOnFull != TRUE) TestPool <- catboost::catboost.load_pool(dataTest[, eval(Target) := NULL], label = TestTarget)
      if(TrainOnFull != TRUE) FinalTestPool <- catboost::catboost.load_pool(TestData[, eval(Target) := NULL], label = FinalTestTarget)
    } else {
      TrainPool <- catboost::catboost.load_pool(dataTrain[, eval(Target) := NULL], label = TrainTarget)
      if(TrainOnFull != TRUE) TestPool <- catboost::catboost.load_pool(dataTest[, eval(Target) := NULL], label = TestTarget)
    }
  }
  
  # Regression Grid Tune or Not Check----
  if (GridTune == TRUE & TrainOnFull == FALSE) {
    
    # Regression Grid Create data.table To Store Results----
    GridCollect <- data.table::data.table(
      ParamRow = 1:(MaxModelsInGrid + 1),
      EvalStat = rep(9999999, MaxModelsInGrid + 1)
    )
    
    # Regression Grid Define Hyper Parameters----
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
        depth = c(4:12)
      )
      if (tolower(task_type) != "gpu") {
        catboostGridList <- catboostGridList[bootstrap_type != "Poisson"]
      }
      catboostGridList[, ID := runif(nrow(catboostGridList))]
      catboostGridList <-
        catboostGridList[order(ID)][1:(MaxModelsInGrid + 1)][, ID := NULL]
    }
    
    # Regression Grid Tuning Main Loop----
    for (i in as.integer(seq_len(MaxModelsInGrid + 1))) {
      # Print i
      print(i)
      
      # Regression Grid Define Base Parameters----
      base_params <- list(
        iterations           = Trees,
        loss_function        = eval_metric,
        eval_metric          = eval_metric,
        use_best_model       = TRUE,
        has_time             = HasTime,
        best_model_min_trees = 10,
        metric_period        = 10,
        task_type            = task_type
        #, early_stopping_rounds = TRUE
        #, od_wait = 50
      )

      # Regression Grid Merge Model Parameters----
      # Have first model be the baseline model
      if (i != 1) {
        base_params <- c(as.list(catboostGridList[i, ]), base_params)
      }
      
      # Regression Grid Train Model----
      model <- catboost::catboost.train(learn_pool = TrainPool,
                                        test_pool  = TestPool,
                                        params     = base_params)
      
      # Regression Grid Score Model----
      if (!is.null(TestData)) {
        predict <- catboost::catboost.predict(
          model = model,
          pool = FinalTestPool,
          prediction_type = "RawFormulaVal",
          thread_count = -1
        )
      } else if(!is.null(dataTest)) {
        predict <- catboost::catboost.predict(
          model = model,
          pool = TestPool,
          prediction_type = "RawFormulaVal",
          thread_count = -1
        )
      } else {
        predict <- catboost::catboost.predict(
          model = model,
          pool = TrainPool,
          prediction_type = "RawFormulaVal",
          thread_count = -1)
      }
      
      # Regression Remove Model and Collect Garbage----
      rm(model)
      gc()
      
      # Regression Grid Validation Data----
      if (!is.null(TestData)) {
        calibEval <-
          data.table::as.data.table(cbind(Target = FinalTestTarget, Predicted = predict))
      } else {
        calibEval <-
          data.table::as.data.table(cbind(Target = TestTarget, Predicted = predict))
      }
      
      # Inverse Transform----
      if (!is.null(TransformNumericColumns)) {
        # Make copy of TransformationResults----
        grid_trans_results <-
          data.table::copy(TransformationResults)
        
        # Append record for Predicted Column----
        data.table::set(
          grid_trans_results,
          i = which(grid_trans_results[["ColumnName"]] == eval(TargetColumnName)),
          j = "ColumnName",
          value = "Target"
        )
        grid_trans_results <- data.table::rbindlist(list(
          grid_trans_results,
          data.table::data.table(
            ColumnName = c("Predicted"),
            MethodName = grid_trans_results[ColumnName == "Target",
                                            MethodName],
            Lambda = grid_trans_results[ColumnName == "Target",
                                        Lambda],
            NormalizedStatistics = 0
          )
        ))
        
        # If Actual target columnname == "Target" remove the duplicate version----
        if (length(unique(grid_trans_results[["ColumnName"]])) != nrow(grid_trans_results)) {
          grid_trans_results <-
            grid_trans_results[, .N, by = "ColumnName"][N != 1][[1]]
          temp1 <- which(names(calibEval) == temp)[1]
          calibEval[, eval(names(data)[temp1]) := NULL]
          grid_trans_results <- grid_trans_results[, ID := 1:.N][ID != which(grid_trans_results[["ID"]] == temp)][, ID := NULL]
        }
        
        # Run Back-Transform----
        calibEval <- AutoTransformationScore(
          ScoringData = calibEval,
          Type = "Inverse",
          FinalResults = grid_trans_results,
          TransID = NULL,
          Path = NULL
        )
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
        Metric <- (calibEval[, stats::cor(Target, Predicted)]) ^ 2
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
  if (GridTune & TrainOnFull == FALSE) {
    if (grid_eval_metric %chin% c("poisson", "mae", "mape", "mse", "msle", "kl", "cs", "r2")) {
      BestGrid <- GridCollect[order(-EvalStat)][1, ParamRow]
      if (BestGrid == 1) {
        BestThresh <- GridCollect[order(-EvalStat)][1, EvalStat]
        base_params <- list(
          iterations           = Trees,
          learning_rate        = 0.03,
          depth                = 10,
          loss_function        = eval_metric,
          eval_metric          = eval_metric,
          use_best_model       = TRUE,
          has_time             = HasTime,
          best_model_min_trees = 10,
          metric_period        = 10,
          task_type            = task_type
          #, early_stopping_rounds = TRUE
          #, od_wait = 50
        )
      } else {
        BestThresh <- GridCollect[order(-EvalStat)][1, EvalStat]
        base_params <- list(
          iterations           = Trees,
          loss_function        = eval_metric,
          eval_metric          = eval_metric,
          use_best_model       = TRUE,
          has_time             = HasTime,
          best_model_min_trees = 10,
          metric_period        = 10,
          task_type            = task_type
          #, early_stopping_rounds = TRUE
          #, od_wait = 50
        )
        base_params <-
          c(as.list(catboostGridList[BestGrid, ]), base_params)
      }
    } else {
      if(BestGrid == 1) {
        BestGrid <- GridCollect[order(EvalStat)][1, ParamRow]
        BestThresh <- GridCollect[order(EvalStat)][1, EvalStat]
        base_params <- list(
          iterations           = Trees,
          learning_rate        = 0.03,
          loss_function        = eval_metric,
          eval_metric          = eval_metric,
          use_best_model       = TRUE,
          has_time             = HasTime,
          best_model_min_trees = 10,
          metric_period        = 10,
          task_type            = task_type
          #, early_stopping_rounds = TRUE
          #, od_wait = 50
        )
        base_params <- c(as.list(catboostGridList[BestGrid, ]),
                         base_params)
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
          #, early_stopping_rounds = TRUE
          #, od_wait = 50
        )
        base_params <-
          c(as.list(catboostGridList[BestGrid, ]), base_params)
      }
    }
    
  } else {
    base_params <- list(
      iterations           = Trees,
      learning_rate        = 0.03,
      depth                = 10,
      loss_function        = eval_metric,
      eval_metric          = eval_metric,
      use_best_model       = TRUE,
      has_time             = HasTime,
      best_model_min_trees = 10,
      metric_period        = 10,
      task_type            = task_type
      #, early_stopping_rounds = TRUE
      #, od_wait = 50
    )
    if (!is.null(PassInGrid)) {
      base_params <- c(base_params, as.list(PassInGrid[1,]))
    }
  }
  
  # Regression Train Final Model----
  if(TrainOnFull) {
    model <- catboost::catboost.train(learn_pool = TrainPool,
                                      params     = base_params)    
  } else {
    model <- catboost::catboost.train(learn_pool = TrainPool,
                                      test_pool  = TestPool,
                                      params     = base_params)
  }
  
  # Regression Save Model----
  if (SaveModelObjects) {
    catboost::catboost.save_model(model = model,
                                  model_path = paste0(model_path, "/", ModelID))
  }
  
  # Regression Score Final Test Data----
  if (!is.null(TestData)) {
    predict <- catboost::catboost.predict(
      model = model,
      pool = FinalTestPool,
      prediction_type = "RawFormulaVal",
      thread_count = -1)
  } else if(TrainOnFull) {
    predict <- catboost::catboost.predict(
      model = model,
      pool = TrainPool,
      prediction_type = "RawFormulaVal",
      thread_count = -1)
  } else {
    predict <- catboost::catboost.predict(
      model = model,
      pool = TestPool,
      prediction_type = "RawFormulaVal",
      thread_count = -1)
  }
  
  # Regression Validation Data----
  if(!TrainOnFull) {
    if (!is.null(TestData)) {
      ValidationData <- data.table::as.data.table(cbind(Target = FinalTestTarget, TestMerge, Predict = predict))
      data.table::setnames(ValidationData, "Target",eval(TargetColumnName))
    } else {
      ValidationData <- data.table::as.data.table(cbind(Target = TestTarget, dataTest, Predict = predict))
      data.table::setnames(ValidationData, "Target",eval(TargetColumnName))
    }    
  } else {
    data <- data.table::as.data.table(cbind(Target = TrainTarget, data, Predict = predict)) 
    data.table::setnames(data, "Target",eval(TargetColumnName))
  }
  
  # Inverse Transform----
  if (!is.null(TransformNumericColumns)) {
    
    # Append record for Predicted Column----
    if (GridTune & TrainOnFull == FALSE) {
      TransformationResults <- TransformationResults[ColumnName != "Predicted"]
    }
    TransformationResults <- data.table::rbindlist(list(
      TransformationResults,
      data.table::data.table(
        ColumnName = c("Predict", eval(TargetColumnName)),
        MethodName = rep(TransformationResults[ColumnName == eval(TargetColumnName),
                                               MethodName], 2),
        Lambda = rep(TransformationResults[ColumnName == eval(TargetColumnName),
                                           Lambda], 2),
        NormalizedStatistics = rep(0, 2))))
    
    # If Actual target columnname == "Target" remove the duplicate version----
    if (length(unique(TransformationResults[["ColumnName"]])) != nrow(TransformationResults)) {
      temp <- TransformationResults[, .N, by = "ColumnName"][N != 1][[1]]
      temp1 <- which(names(ValidationData) == temp)[1]
      if(!TrainOnFull) {
        ValidationData[, eval(names(data)[temp1]) := NULL]
      } else {
        data[, eval(names(data)[temp1]) := NULL]
      }
      TransformationResults <- TransformationResults[, ID := 1:.N][ID != which(TransformationResults[["ID"]] == temp1)][, ID := NULL]
    }
    
    # Transform Target and Predicted Value----
    if(!TrainOnFull) {
      ValidationData <- AutoTransformationScore(
        ScoringData = ValidationData,
        Type = "Inverse",
        FinalResults = TransformationResults,
        TransID = NULL,
        Path = NULL
      )      
    } else {
      data <- AutoTransformationScore(
        ScoringData = data,
        Type = "Inverse",
        FinalResults = TransformationResults,
        TransID = NULL,
        Path = NULL
      )
    }
  }
  
  # Regression r2 via sqrt of correlation
  if(!TrainOnFull) r_squared <- (ValidationData[, stats::cor(ValidationData[[eval(TargetColumnName)]], Predict)]) ^ 2
  
  # Regression Save Validation Data to File----
  if (SaveModelObjects) {
    if(!is.null(metadata_path)) {
      if(!TrainOnFull) {
        data.table::fwrite(ValidationData,
                           file = paste0(metadata_path,
                                         "/",
                                         ModelID,
                                         "_ValidationData.csv"))        
      } else {
        data.table::fwrite(data,
                           file = paste0(metadata_path,
                                         "/",
                                         ModelID,
                                         "_FullDataPredictions.csv"))
      }
    } else {
      if(!TrainOnFull) {
        data.table::fwrite(ValidationData,
                           file = paste0(model_path,
                                         "/",
                                         ModelID,
                                         "_ValidationData.csv"))
      } else {
        data.table::fwrite(data,
                           file = paste0(model_path,
                                         "/",
                                         ModelID,
                                         "_FullDataPredictions.csv"))
      }
    }
  }
  
  # Regression Evaluation Calibration Plot----
  if(!TrainOnFull) {
    EvaluationPlot <- EvalPlot(
      data = ValidationData,
      PredictionColName = "Predict",
      TargetColName = eval(TargetColumnName),
      GraphType = "calibration",
      PercentileBucket = 0.05,
      aggrfun = function(x)
        mean(x, na.rm = TRUE)
    )    
  }

  # Add Number of Trees to Title
  if(!TrainOnFull) EvaluationPlot <- EvaluationPlot + ggplot2::ggtitle(paste0("Calibration Evaluation Plot: R2 = ", round(r_squared, 3)))
  
  # Save plot to file
  if(!TrainOnFull) {
    if (SaveModelObjects) {
      if(!is.null(metadata_path)) {
        ggplot2::ggsave(paste0(metadata_path,
                               "/",
                               ModelID, "_EvaluationPlot.png"))
      } else {
        ggplot2::ggsave(paste0(model_path,
                               "/",
                               ModelID, "_EvaluationPlot.png"))      
      }
    }    
  }

  # Regression Evaluation Calibration Plot----
  if(!TrainOnFull) {
    EvaluationBoxPlot <- EvalPlot(
      data = ValidationData,
      PredictionColName = "Predict",
      TargetColName = eval(TargetColumnName),
      GraphType = "boxplot",
      PercentileBucket = 0.05,
      aggrfun = function(x)
        mean(x, na.rm = TRUE)
    )    
  }

  # Add Number of Trees to Title
  if(!TrainOnFull) EvaluationBoxPlot <- EvaluationBoxPlot + ggplot2::ggtitle(paste0("Calibration Evaluation Plot: R2 = ", round(r_squared, 3)))
  
  # Save plot to file
  if(!TrainOnFull) {
    if (SaveModelObjects) {
      if(!is.null(metadata_path)) {
        ggplot2::ggsave(paste0(metadata_path,
                               "/",
                               ModelID,
                               "_EvaluationBoxPlot.png"))
      } else {
        ggplot2::ggsave(paste0(model_path,
                               "/",
                               ModelID,
                               "_EvaluationBoxPlot.png"))      
      }
    }
  }

  # Regression Evaluation Metrics----
  if(!TrainOnFull) {
    EvaluationMetrics <-
      data.table::data.table(
        Metric = c("MAE","MAPE","MSE","R2"),
        MetricValue = rep(999999, 8))
    i <- 0L
    for (metric in c("mae", "mape", "mse", "r2")) {
      i <- as.integer(i + 1)
      tryCatch({
        # Regression Grid Evaluation Metrics----
        if (tolower(metric) == "poisson") {
          if (MinVal > 0 &
              min(ValidationData[["Predict"]], na.rm = TRUE) > 0) {
            ValidationData[, Metric := Predict - ValidationData[[eval(TargetColumnName)]] * log(Predict + 1)]
            Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
          }
        } else if (tolower(metric) == "mae") {
          ValidationData[, Metric := abs(ValidationData[[eval(TargetColumnName)]] - Predict)]
          Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
        } else if (tolower(metric) == "mape") {
          ValidationData[, Metric := abs((ValidationData[[eval(TargetColumnName)]] - Predict) / (ValidationData[[eval(TargetColumnName)]] + 1))]
          Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
        } else if (tolower(metric) == "mse") {
          ValidationData[, Metric := (ValidationData[[eval(TargetColumnName)]] - Predict) ^ 2]
          Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
        } else if (tolower(metric) == "msle") {
          if (MinVal > 0 &
              min(ValidationData[["Predict"]], na.rm = TRUE) > 0) {
            ValidationData[, Metric := (log(ValidationData[[eval(TargetColumnName)]] + 1) - log(Predict + 1)) ^ 2]
            Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
          }
        } else if (tolower(metric) == "kl") {
          if (MinVal > 0 &
              min(ValidationData[["Predict"]], na.rm = TRUE) > 0) {
            ValidationData[, Metric := ValidationData[[eval(TargetColumnName)]] * log((ValidationData[[eval(TargetColumnName)]] + 1) /
                                                      (Predict + 1))]
            Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
          }
        } else if (tolower(metric) == "cs") {
          ValidationData[, ':=' (
            Metric1 = ValidationData[[eval(TargetColumnName)]] * Predict,
            Metric2 = ValidationData[[eval(TargetColumnName)]] ^ 2,
            Metric3 = Predict ^ 2
          )]
          Metric <-
            ValidationData[, sum(Metric1, na.rm = TRUE)] / (sqrt(ValidationData[, sum(Metric2, na.rm = TRUE)]) *
                                                              sqrt(ValidationData[, sum(Metric3, na.rm = TRUE)]))
        } else if (tolower(metric) == "r2") {
          ValidationData[, ':=' (
            Metric1 = (ValidationData[[eval(TargetColumnName)]] - MeanTrainTarget) ^ 2,
            Metric2 = (ValidationData[[eval(TargetColumnName)]] - Predict) ^ 2
          )]
          Metric <-
            1 - ValidationData[, sum(Metric2, na.rm = TRUE)] /
            ValidationData[, sum(Metric1, na.rm = TRUE)]
        }
        data.table::set(
          EvaluationMetrics,
          i = i,
          j = 2L,
          value = round(Metric, 4))
      }, error = function(x)
        "skip")
    }
    
    # Remove Cols
    ValidationData[, ':=' (
      Metric = NULL
    )]
    
    # Save EvaluationMetrics to File
    EvaluationMetrics <- EvaluationMetrics[MetricValue != 999999]
    if (SaveModelObjects) {
      if(!is.null(metadata_path)) {
        data.table::fwrite(EvaluationMetrics,
                           file = paste0(metadata_path,
                                         "/",
                                         ModelID, "_EvaluationMetrics.csv"))
      } else {
        data.table::fwrite(EvaluationMetrics,
                           file = paste0(model_path,
                                         "/",
                                         ModelID, "_EvaluationMetrics.csv"))      
      }
    }
    
    # Regression Variable Importance----
    temp <- catboost::catboost.get_feature_importance(model)
    VariableImportance <-
      data.table::data.table(cbind(Variable = row.names(temp), temp))
    data.table::setnames(VariableImportance, "V2", "Importance")
    VariableImportance[, Importance := round(as.numeric(Importance), 4)]
    VariableImportance <- VariableImportance[order(-Importance)]
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
    
    # Regression Partial Dependence----
    ParDepBoxPlots <- list()
    ParDepPlots <- list()
    if(NumOfParDepPlots == 0) {
      j <- 0
      k <- 0
      for (i in seq_len(min(length(FeatureColNames), NumOfParDepPlots))) {
        tryCatch({
          Out <- ParDepCalPlots(
            data = ValidationData,
            PredictionColName = "Predict",
            TargetColName = eval(TargetColumnName),
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
            TargetColName = eval(TargetColumnName),
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
        if(!is.null(metadata_path)) {
          save(ParDepPlots,
               file = paste0(metadata_path, "/", ModelID, "_ParDepPlots.R"))
        } else {
          save(ParDepPlots,
               file = paste0(model_path, "/", ModelID, "_ParDepPlots.R"))      
        }
      }
      
      # Regression Save ParDepBoxPlots to file----
      if (SaveModelObjects) {
        if(!is.null(metadata_path)) {
          save(ParDepBoxPlots,
               file = paste0(metadata_path, "/", ModelID, "_ParDepBoxPlots.R"))
        } else {
          save(ParDepBoxPlots,
               file = paste0(model_path, "/", ModelID, "_ParDepBoxPlots.R"))      
        }
      }    
    }
    
    # Regression Save GridCollect and catboostGridList----
    if (SaveModelObjects & GridTune == TRUE) {
      if(!is.null(metadata_path)) {
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
  }

  # Final Garbage Collection----
  if (tolower(task_type) == "gpu") {
    gc()
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
    ggplot2::ggplot(VI_Data, ggplot2::aes(x = reorder(Variable, Importance), y = Importance, fill = Importance)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::scale_fill_gradient2(
        mid = ColorLow,
        high = ColorHigh) +
      RemixAutoML::ChartTheme(
        Size = 12,
        AngleX = 0,
        LegendPosition = "right"
      ) +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = "Global Variable Importance") +
      ggplot2::xlab("Top Model Features") +
      ggplot2::ylab("Value")
  }
  
  # Regression Return Model Objects----
  if(!TrainOnFull) {
    if (GridTune) {
      if (!is.null(TransformNumericColumns)) {
        if (ReturnModelObjects) {
          return(
            list(
              Model = model,
              ValidationData = ValidationData,
              EvaluationPlot = EvaluationPlot,
              EvaluationBoxPlot = EvaluationBoxPlot,
              EvaluationMetrics = EvaluationMetrics,
              VariableImportance = VariableImportance,
              VI_Plot = VI_Plot(VariableImportance),
              PartialDependencePlots = ParDepPlots,
              PartialDependenceBoxPlots = ParDepBoxPlots,
              GridList = catboostGridList,
              GridMetrics = GridCollect,
              ColNames = Names,
              TransformationResults = TransformationResults
            )
          )
        }
      } else {
        return(
          list(
            Model = model,
            ValidationData = ValidationData,
            EvaluationPlot = EvaluationPlot,
            EvaluationBoxPlot = EvaluationBoxPlot,
            EvaluationMetrics = EvaluationMetrics,
            VariableImportance = VariableImportance,
            VI_Plot = VI_Plot(VariableImportance),
            PartialDependencePlots = ParDepPlots,
            PartialDependenceBoxPlots = ParDepBoxPlots,
            GridList = catboostGridList,
            GridMetrics = GridCollect,
            ColNames = Names
          )
        )
      }
    } else {
      if (!is.null(TransformNumericColumns)) {
        if (ReturnModelObjects) {
          return(
            list(
              Model = model,
              ValidationData = ValidationData,
              EvaluationPlot = EvaluationPlot,
              EvaluationBoxPlot = EvaluationBoxPlot,
              EvaluationMetrics = EvaluationMetrics,
              VariableImportance = VariableImportance,
              VI_Plot = VI_Plot(VariableImportance),
              PartialDependencePlots = ParDepPlots,
              PartialDependenceBoxPlots = ParDepBoxPlots,
              ColNames = Names,
              TransformationResults = TransformationResults
            )
          )
        }
      } else {
        return(
          list(
            Model = model,
            ValidationData = ValidationData,
            EvaluationPlot = EvaluationPlot,
            EvaluationBoxPlot = EvaluationBoxPlot,
            EvaluationMetrics = EvaluationMetrics,
            VariableImportance = VariableImportance,
            VI_Plot = VI_Plot(VariableImportance),
            PartialDependencePlots = ParDepPlots,
            PartialDependenceBoxPlots = ParDepBoxPlots,
            ColNames = Names
          )
        )
      }
    }
  } else {
    return(
      if(!is.null(TransformNumericColumns)) {
        list(
          Model = model,
          data = data,
          ColNames = Names,
          TransformationResults = TransformationResults)
      } else {
        list(
          Model = model,
          data = data,
          ColNames = Names)
      }
    )
  }
}
