#' AutoH2oDRFHurdleModel is generalized hurdle modeling framework
#'
#' @family Automated Regression
#' @param data Source training data. Do not include a column that has the class labels for the buckets as they are created internally.
#' @param ValidationData Source validation data. Do not include a column that has the class labels for the buckets as they are created internally.
#' @param TestData Souce test data. Do not include a column that has the class labels for the buckets as they are created internally.
#' @param Buckets A numeric vector of the buckets used for subsetting the data. NOTE: the final Bucket value will first create a subset of data that is less than the value and a second one thereafter for data greater than the bucket value.
#' @param TargetColumnName Supply the column name or number for the target variable
#' @param FeatureColNames Supply the column names or number of the features (not included the PrimaryDateColumn)
#' @param TransformNumericColumns Transform numeric column inside the AutoCatBoostRegression() function
#' @param SplitRatios Supply vector of partition ratios. For example, c(0.70,0.20,0,10).
#' @param ModelID Define a character name for your models
#' @param Paths The path to your folder where you want your model information saved
#' @param MetaDataPaths A character string of your path file to where you want your model evaluation output saved. If left NULL, all output will be saved to Paths.
#' @param SaveModelObjects Set to TRUE to save the model objects to file in the folders listed in Paths
#' @param IfSaveModel Save as "mojo" or "standard"
#' @param MaxMem Set the maximum memory your system can provide
#' @param NThreads Set the number of threads you want to dedicate to the model building
#' @param Trees Default 1000
#' @param GridTune Set to TRUE if you want to grid tune the models
#' @param MaxModelsInGrid Set to a numeric value for the number of models to try in grid tune
#' @param NumOfParDepPlots Set to pull back N number of partial dependence calibration plots.
#' @param PassInGrid Pass in a grid for changing up the parameter settings for catboost
#' @return Returns AutoXGBoostRegression() model objects: VariableImportance.csv, Model, ValidationData.csv, EvalutionPlot.png, EvalutionBoxPlot.png, EvaluationMetrics.csv, ParDepPlots.R a named list of features with partial dependence calibration plots, ParDepBoxPlots.R, GridCollect, and the grid used
#' @examples
#' \donttest{
#' Output <- AutoH2oDRFHurdleModel(
#'   data,
#'   ValidationData = NULL,
#'   TestData = NULL,
#'   Buckets = 1L,
#'   TargetColumnName = "Target_Variable",
#'   FeatureColNames = 4:ncol(data),
#'   TransformNumericColumns = NULL,
#'   SplitRatios = c(0.7, 0.2, 0.1),
#'   NThreads = max(1L, parallel::detectCores()-2L),
#'   ModelID = "ModelID",
#'   Paths = NULL,
#'   MetaDataPaths = NULL,
#'   SaveModelObjects = TRUE,
#'   IfSaveModel = "mojo",
#'   Trees = 1000L,
#'   GridTune = FALSE,
#'   MaxModelsInGrid = 1L,
#'   NumOfParDepPlots = 10L,
#'   PassInGrid = NULL)
#' }
#' @export
AutoH2oDRFHurdleModel <- function(data,
                                  ValidationData = NULL,
                                  TestData = NULL,
                                  Buckets = 0L,
                                  TargetColumnName = NULL,
                                  FeatureColNames = NULL,
                                  TransformNumericColumns = NULL,
                                  SplitRatios = c(0.70, 0.20, 0.10),
                                  ModelID = "ModelTest",
                                  Paths = NULL,
                                  MetaDataPaths = NULL,
                                  SaveModelObjects = TRUE,
                                  IfSaveModel = "mojo",
                                  MaxMem = "28G",
                                  NThreads = max(1L, parallel::detectCores()-2L),
                                  Trees = 1000L,
                                  GridTune = TRUE,
                                  MaxModelsInGrid = 1L,
                                  NumOfParDepPlots = 10L,
                                  PassInGrid = NULL) {
  
  # Turn on full speed ahead----
  data.table::setDTthreads(percent = 100L)
  
  # Check args----
  if (is.character(Buckets) | is.factor(Buckets) | is.logical(Buckets)) {
    return("Buckets needs to be a numeric scalar or vector")
  }
  if (!is.logical(SaveModelObjects)) return("SaveModelOutput needs to be set to either TRUE or FALSE")
  if (is.character(Trees) | is.factor(Trees) | is.logical(Trees) | length(Trees) > 1L) {
    return("NumTrees needs to be a numeric scalar")
  }
  if (!is.logical(GridTune)) return("GridTune needs to be either TRUE or FALSE")
  if (is.character(MaxModelsInGrid) | is.factor(MaxModelsInGrid) | is.logical(MaxModelsInGrid) | length(MaxModelsInGrid) > 1L) {
    return("NumberModelsInGrid needs to be a numeric scalar")
  }
  
  # Initialize H2O----
  h2o::h2o.init(max_mem_size = MaxMem, nthreads = NThreads, enable_assertions = FALSE)
  
  # Initialize collection and counter----
  ModelInformationList <- list()
  if(!is.null(Paths)) {
    if (length(Paths) == 1L) {
      Paths <- rep(Paths, length(Buckets) + 1L)
    }    
  }
  if(!is.null(MetaDataPaths)) {
    if (length(MetaDataPaths) == 1L) {
      MetaDataPaths <- rep(MetaDataPaths, length(Buckets) + 1L)
    }    
  }

  # Data.table check----
  if (!data.table::is.data.table(data)) data.table::setDT(data)
  if (!is.null(ValidationData)) {
    if (!data.table::is.data.table(ValidationData)) data.table::setDT(ValidationData)
  }
  if (!is.null(TestData)) {
    if (!data.table::is.data.table(TestData)) data.table::setDT(TestData)
  }
  
  # FeatureColumnNames----
  if (is.numeric(FeatureColNames) | is.integer(FeatureColNames)) {
    FeatureNames <- names(data)[FeatureColNames]
  } else {
    FeatureNames <- FeatureColNames
  }
  
  # Add target bucket column----
  if(length(Buckets) == 1L) {
    data.table::set(data, i = which(data[[eval(TargetColumnName)]] <= Buckets[1L]), j = "Target_Buckets", value = 0L)
    data.table::set(data, i = which(data[[eval(TargetColumnName)]] > Buckets[1L]), j = "Target_Buckets", value = 1L)
  } else {
    for (i in seq_len(length(Buckets) + 1L)) {
      if (i == 1L) {
        data.table::set(data, i = which(data[[eval(TargetColumnName)]] <= Buckets[i]), j = "Target_Buckets", value = as.factor(Buckets[i]))
      } else if (i == length(Buckets) + 1L) {
        data.table::set(data, i = which(data[[eval(TargetColumnName)]] > Buckets[i - 1L]), j = "Target_Buckets", value = as.factor(paste0(Buckets[i-1L], "+")))
      } else {
        data.table::set(data, i = which(data[[eval(TargetColumnName)]] <= Buckets[i] & data[[eval(TargetColumnName)]] > Buckets[i-1L]), j = "Target_Buckets", value = as.factor(Buckets[i]))
      }      
    }
  }
  
  # Add target bucket column----
  if (!is.null(ValidationData)) {
    ValidationData[, Target_Buckets := as.factor(Buckets[1])]
    for (i in seq_len(length(Buckets) + 1)) {
      if (i == 1L) {
        data.table::set(ValidationData, i = which(ValidationData[[eval(TargetColumnName)]] <= Buckets[i]), j = "Target_Buckets", value = as.factor(Buckets[i]))
      } else if (i == length(Buckets) + 1L) {
        data.table::set(ValidationData, i = which(ValidationData[[eval(TargetColumnName)]] > Buckets[i - 1L]), j = "Target_Buckets", value = as.factor(paste0(Buckets[i - 1L], "+")))
      } else {
        data.table::set(ValidationData, i = which(ValidationData[[eval(TargetColumnName)]] <= Buckets[i] & ValidationData[[eval(TargetColumnName)]] > Buckets[i - 1L]), j = "Target_Buckets", value = as.factor(Buckets[i]))
      }
    }
  }
  
  # Add target bucket column----
  if (!is.null(TestData)) {
    TestData[, Target_Buckets := as.factor(Buckets[1L])]
    for (i in seq_len(length(Buckets) + 1L)) {
      if (i == 1L) {
        data.table::set(TestData, i = which(TestData[[eval(TargetColumnName)]] <= Buckets[i]), j = "Target_Buckets", value = as.factor(Buckets[i]))
      } else if (i == length(Buckets) + 1L) {
        data.table::set(TestData, i = which(TestData[[eval(TargetColumnName)]] > Buckets[i-1L]), j = "Target_Buckets", value = as.factor(paste0(Buckets[i - 1L], "+")))
      } else {
        data.table::set(TestData, i = which(TestData[[eval(TargetColumnName)]] <= Buckets[i] & TestData[[eval(TargetColumnName)]] > Buckets[i - 1L]), j = "Target_Buckets", value = as.factor(Buckets[i]))
      }
    }
  }
  
  # AutoDataPartition if Validation and TestData are NULL----
  if (is.null(ValidationData) & is.null(TestData)) {
    DataSets <- AutoDataPartition(
      data = data,
      NumDataSets = 3L,
      Ratios = SplitRatios,
      PartitionType = "random",
      StratifyColumnNames = "Target_Buckets",
      TimeColumnName = NULL)
    data <- DataSets$TrainData
    ValidationData <- DataSets$ValidationData
    TestData <- DataSets$TestData
    rm(DataSets)
  }
  
  # Begin classification model building----
  if (length(Buckets) == 1L) {
    ClassifierModel <- AutoH2oDRFClassifier(
      data = data,
      ValidationData = ValidationData,
      TestData = TestData,
      TargetColumnName = "Target_Buckets",
      FeatureColNames = FeatureNames,
      eval_metric = "auc",
      Trees = Trees,
      GridTune = GridTune,
      MaxMem = MaxMem,
      NThreads = NThreads,
      MaxModelsInGrid = MaxModelsInGrid,
      model_path = Paths[1L],
      metadata_path = MetaDataPaths[1L],
      ModelID = ModelID,
      NumOfParDepPlots = NumOfParDepPlots,
      ReturnModelObjects = TRUE,
      SaveModelObjects = SaveModelObjects,
      IfSaveModel = IfSaveModel,
      H2OShutdown = FALSE)
  } else {
    ClassifierModel <- AutoH2oDRFMultiClass(
      data = data,
      ValidationData = ValidationData,
      TestData = TestData,
      TargetColumnName = "Target_Buckets",
      FeatureColNames = FeatureNames,
      eval_metric = "logloss",
      Trees = Trees,
      GridTune = GridTune,
      MaxMem = MaxMem,
      NThreads = NThreads,
      MaxModelsInGrid = MaxModelsInGrid,
      model_path = Paths[1L],
      metadata_path = MetaDataPaths[1L],
      ModelID = ModelID,
      ReturnModelObjects = TRUE,
      SaveModelObjects = SaveModelObjects,
      IfSaveModel = IfSaveModel,
      H2OShutdown = FALSE)
  }
  
  # Store metadata----
  ClassModel <- ClassifierModel$Model
  ClassEvaluationMetrics <- ClassifierModel$EvaluationMetrics
  VariableImportance <- ClassifierModel$VariableImportance
  rm(ClassifierModel)
  
  # Model Scoring----
  TestData <- AutoH2OMLScoring(
    ScoringData = data,
    ModelObject = ClassModel,
    ModelType = "mojo",
    H2OShutdown = FALSE,
    MaxMem = "28G",
    JavaOptions = '-Xmx1g -XX:ReservedCodeCacheSize=256m',
    ModelPath = Paths[1L],
    ModelID = "ModelTest",
    ReturnFeatures = TRUE,
    TransformNumeric = FALSE,
    BackTransNumeric = FALSE,
    TargetColumnName = NULL,
    TransformationObject = NULL,
    TransID = NULL,
    TransPath = NULL,
    MDP_Impute = TRUE,
    MDP_CharToFactor = TRUE,
    MDP_RemoveDates = TRUE,
    MDP_MissFactor = "0",
    MDP_MissNum = -1)
  
  # Remove classification Prediction----
  TestData <- TestData[, Predictions := NULL]
  
  # Change name for classification----
  if(length(Buckets) == 1L) {
    data.table::setnames(TestData, "p0","Predictions_C0")
    data.table::setnames(TestData, "p1","Predictions_C1")
  } else {
    data.table::setnames(TestData, names(TestData)[1:length(Buckets)], paste0("P_",gsub('[[:punct:] ]+',' ',names(TestData)[1L:length(Buckets)])))
    data.table::setnames(TestData, names(TestData)[length(Buckets)+1L], paste0("P+_",gsub('[[:punct:] ]+',' ',names(TestData)[length(Buckets)+1L])))
  }

  # Remove Model Object----
  rm(ClassModel)
  
  # Remove Target_Buckets----
  data.table::set(data, j = "Target_Buckets", value = NULL)
  data.table::set(ValidationData, j = "Target_Buckets", value = NULL)
  
  # Begin regression model building----
  counter <- 0L
  Degenerate <- 0L
  for (bucket in rev(seq_len(length(Buckets) + 1L))) {
    # Filter By Buckets----
    if (bucket == max(seq_len(length(Buckets) + 1L))) {
      if (!is.null(TestData)) {
        trainBucket <- data[get(TargetColumnName) > eval(Buckets[bucket - 1L])]
        validBucket <- ValidationData[get(TargetColumnName) > eval(Buckets[bucket - 1L])]
        testBucket <- TestData[get(TargetColumnName) > eval(Buckets[bucket - 1L])]
        data.table::set(testBucket, j = setdiff(names(testBucket), names(data)), value = NULL)
      } else {
        trainBucket <- data[get(TargetColumnName) > eval(Buckets[bucket - 1L])]
        validBucket <- ValidationData[get(TargetColumnName) > eval(Buckets[bucket - 1L])]
        testBucket <- NULL
      }
    } else if (bucket == 1L) {
      if (!is.null(TestData)) {
        trainBucket <- data[get(TargetColumnName) <= eval(Buckets[bucket])]
        validBucket <- ValidationData[get(TargetColumnName) <= eval(Buckets[bucket])]
        testBucket <- TestData[get(TargetColumnName) <= eval(Buckets[bucket])]
        data.table::set(testBucket, j = setdiff(names(testBucket), names(data)), value = NULL)
      } else {
        trainBucket <- data[get(TargetColumnName) <= eval(Buckets[bucket])]
        validBucket <- ValidationData[get(TargetColumnName) <= eval(Buckets[bucket])]
        testBucket <- NULL
      }
    } else {
      if (!is.null(TestData)) {
        trainBucket <- data[get(TargetColumnName) <= eval(Buckets[bucket]) & get(TargetColumnName) > eval(Buckets[bucket - 1L])]
        validBucket <- ValidationData[get(TargetColumnName) <= eval(Buckets[bucket]) & get(TargetColumnName) > eval(Buckets[bucket - 1L])]
        testBucket <- TestData[get(TargetColumnName) <= eval(Buckets[bucket]) & get(TargetColumnName) > eval(Buckets[bucket - 1L])]
        data.table::set(testBucket, j = setdiff(names(testBucket), names(data)), value = NULL)
      } else {
        trainBucket <- data[get(TargetColumnName) <= eval(Buckets[bucket]) & get(TargetColumnName) > eval(Buckets[bucket - 1L])]
        validBucket <- ValidationData[get(TargetColumnName) <= eval(Buckets[bucket]) & get(TargetColumnName) > eval(Buckets[bucket - 1L])]
        testBucket <- NULL
      }
    }
    
    # Load Winning Grid if it exists----
    if (file.exists(paste0(Paths[bucket], "/grid", Buckets[bucket], ".csv"))) {
      gridSaved <- data.table::fread(paste0(Paths[bucket], "/grid", Buckets[bucket], ".csv"))
    }
    if (file.exists(paste0(MetaDataPaths[bucket], "/grid", Buckets[bucket], ".csv"))) {
      gridSaved <- data.table::fread(paste0(MetaDataPaths[bucket], "/grid", Buckets[bucket], ".csv"))
    }
    
    # AutoCatBoostRegression()----
    if (trainBucket[, .N] != 0L) {
      if (var(trainBucket[[eval(TargetColumnName)]]) > 0L) {
        counter <- counter + 1L
        if (bucket == max(seq_len(length(Buckets) + 1L))) {
          TestModel <- AutoH2oDRFRegression(
            data = trainBucket,
            ValidationData = validBucket,
            TestData = testBucket,
            TargetColumnName = TargetColumnName,
            FeatureColNames = FeatureNames,
            TransformNumericColumns = TransformNumericColumns,
            eval_metric = "RMSE",
            Trees = Trees,
            GridTune = GridTune,
            MaxMem = MaxMem,
            NThreads = NThreads,
            MaxModelsInGrid = MaxModelsInGrid,
            model_path = Paths[1L],
            metadata_path = MetaDataPaths[1L],
            ModelID = paste0(ModelID,"_",bucket-1,"_"),
            NumOfParDepPlots = NumOfParDepPlots,
            ReturnModelObjects = TRUE,
            SaveModelObjects = SaveModelObjects,
            IfSaveModel = IfSaveModel,
            H2OShutdown = FALSE)
        } else {
          TestModel <- AutoH2oDRFRegression(
            data = trainBucket,
            ValidationData = validBucket,
            TestData = testBucket,
            TargetColumnName = TargetColumnName,
            FeatureColNames = FeatureNames,
            TransformNumericColumns = TransformNumericColumns,
            eval_metric = "RMSE",
            Trees = Trees,
            GridTune = GridTune,
            MaxMem = MaxMem,
            NThreads = NThreads,
            MaxModelsInGrid = MaxModelsInGrid,
            model_path = Paths,
            metadata_path = MetaDataPaths[1L],
            ModelID = paste0(ModelID,"_",bucket-1L),
            NumOfParDepPlots = NumOfParDepPlots,
            ReturnModelObjects = TRUE,
            SaveModelObjects = SaveModelObjects,
            IfSaveModel = IfSaveModel,
            H2OShutdown = FALSE)
        }
        
        # Store Model----
        RegressionModel <- TestModel$Model
        if(!is.null(TransformNumericColumns)) {
          TransformationResults <- TestModel$TransformationInformation
        }
        rm(TestModel)
        
        # Garbage Collection----
        gc()
        
        # Score TestData----
        if (bucket == max(seq_len(length(Buckets) + 1L))) {
          if(!is.null(TransformNumericColumns)) {
            TestData <- AutoH2OMLScoring(
              ScoringData = TestData,
              ModelObject = RegressionModel,
              ModelType = "mojo",
              H2OShutdown = FALSE,
              MaxMem = "28G",
              JavaOptions = '-Xmx1g -XX:ReservedCodeCacheSize=256m',
              ModelPath = paste0(ModelID, "_", bucket,"_"),
              ModelID = "ModelTest",
              ReturnFeatures = TRUE,
              TransformNumeric = TRUE,
              BackTransNumeric = TRUE,
              TargetColumnName = eval(TargetColumnName),
              TransformationObject = TransformationResults,
              TransID = NULL,
              TransPath = NULL,
              MDP_Impute = TRUE,
              MDP_CharToFactor = TRUE,
              MDP_RemoveDates = TRUE,
              MDP_MissFactor = "0",
              MDP_MissNum = -1)
          } else {
            TestData <- AutoH2OMLScoring(
              ScoringData = TestData,
              ModelObject = RegressionModel,
              ModelType = "mojo",
              H2OShutdown = FALSE,
              MaxMem = "28G",
              JavaOptions = '-Xmx1g -XX:ReservedCodeCacheSize=256m',
              ModelPath = Paths,
              ModelID = paste0(ModelID, "_", bucket,"_"),
              ReturnFeatures = TRUE,
              TransformNumeric = FALSE,
              BackTransNumeric = FALSE,
              TargetColumnName = NULL,
              TransformationObject = NULL,
              TransID = NULL,
              TransPath = NULL,
              MDP_Impute = TRUE,
              MDP_CharToFactor = TRUE,
              MDP_RemoveDates = TRUE,
              MDP_MissFactor = "0",
              MDP_MissNum = -1)
          }
        } else {
          if(!is.null(TransformNumericColumns)) {
            TestData <- AutoH2OMLScoring(
              ScoringData = TestData,
              ModelObject = RegressionModel,
              ModelType = "mojo",
              H2OShutdown = FALSE,
              MaxMem = "28G",
              JavaOptions = '-Xmx1g -XX:ReservedCodeCacheSize=256m',
              ModelPath = paste0(ModelID, "_", bucket),
              ModelID = "ModelTest",
              ReturnFeatures = TRUE,
              TransformNumeric = TRUE,
              BackTransNumeric = TRUE,
              TargetColumnName = eval(TargetColumnName),
              TransformationObject = TransformationResults,
              TransID = NULL,
              TransPath = NULL,
              MDP_Impute = TRUE,
              MDP_CharToFactor = TRUE,
              MDP_RemoveDates = TRUE,
              MDP_MissFactor = "0",
              MDP_MissNum = -1)
          } else {
            TestData <- AutoH2OMLScoring(
              ScoringData = TestData,
              ModelObject = RegressionModel,
              ModelType = "mojo",
              H2OShutdown = FALSE,
              MaxMem = "28G",
              JavaOptions = '-Xmx1g -XX:ReservedCodeCacheSize=256m',
              ModelPath = Paths,
              ModelID = paste0(ModelID, "_", bucket),
              ReturnFeatures = TRUE,
              TransformNumeric = FALSE,
              BackTransNumeric = FALSE,
              TargetColumnName = NULL,
              TransformationObject = NULL,
              TransID = NULL,
              TransPath = NULL,
              MDP_Impute = TRUE,
              MDP_CharToFactor = TRUE,
              MDP_RemoveDates = TRUE,
              MDP_MissFactor = "0",
              MDP_MissNum = -1)
          }
        }
        
        # Clear TestModel From Memory----
        rm(RegressionModel)
        
        # Change prediction name to prevent duplicates----
        if (bucket == max(seq_len(length(Buckets) + 1L))) {
          data.table::setnames(TestData, "Predictions", paste0("Predictions_", Buckets[bucket - 1L], "+"))
        } else {
          data.table::setnames(TestData, "Predictions", paste0("Predictions_", Buckets[bucket]))
        }
      } else {
        
        # Use single value for predictions in the case of zero variance----
        if (bucket == max(seq_len(length(Buckets) + 1L))) {
          Degenerate <- Degenerate + 1L
          data.table::set(TestData, j = paste0("Predictions_", Buckets[bucket - 1L], "+"), value = Buckets[bucket])
          data.table::setcolorder(TestData, c(ncol(TestData), 1L:(ncol(TestData)-1L)))
        } else {
          Degenerate <- Degenerate + 1L
          data.table::set(TestData, j = paste0("Predictions_", Buckets[bucket]), value = Buckets[bucket])
          data.table::setcolorder(TestData, c(ncol(TestData), 1L:(ncol(TestData)-1L)))
        }
      }
    }
  }
  
  # Final Combination of Predictions----
  # Logic: 1 Buckets --> 4 columns of preds
  #        2 Buckets --> 6 columns of preds
  #        3 Buckets --> 8 columns of preds
  # Secondary logic: for i == 1, need to create the final column first
  #                  for i > 1, need to take the final column and add the product of the next preds
  Cols <- ncol(TestData)
  if(counter > 2L) {
    for (i in seq_len(length(Buckets)+1L)) {
      if (i == 1L) {
        data.table::set(TestData, j = "UpdatedPrediction", value = TestData[[i]] * TestData[[i + counter + Degenerate]])
      } else {
        data.table::set(TestData, j = "UpdatedPrediction", value = TestData[["UpdatedPrediction"]] + TestData[[i]] * TestData[[i + counter + Degenerate]])
      }
    }  
  } else if(counter == 2L & length(Buckets) != 1L) {
    for (i in seq_len(length(Buckets)+1)) {
      if (i == 1L) {
        data.table::set(TestData, j = "UpdatedPrediction", value = TestData[[i]] * TestData[[i + 1L + counter]])
      } else {
        data.table::set(TestData, j = "UpdatedPrediction", value = TestData[["UpdatedPrediction"]] + TestData[[i]] * TestData[[i + 1L + counter]])
      }
    }  
  } else if(counter == 2L & length(Buckets) == 1L) {
    data.table::set(TestData, j = "UpdatedPrediction", value = TestData[[1L]] * TestData[[3L]] +  TestData[[2L]] * TestData[[4L]])
  } else {
    data.table::set(TestData, j = "UpdatedPrediction", value = TestData[[1]] * TestData[[3L]] +  TestData[[2L]] * TestData[[4L]]) 
  }
  
  # Regression r2 via sqrt of correlation
  r_squared <- (TestData[, stats::cor(get(TargetColumnName), UpdatedPrediction)]) ^ 2L
  
  # Regression Save Validation Data to File----
  if (SaveModelObjects) {
    if(!is.null(MetaDataPaths[1L])) {
      data.table::fwrite(TestData, file = paste0(MetaDataPaths[1L], "/", ModelID, "_ValidationData.csv"))
    } else {
      data.table::fwrite(TestData, file = paste0(Paths[1L], "/", ModelID, "_ValidationData.csv"))      
    }
  }
  
  # Regression Evaluation Calibration Plot----
  EvaluationPlot <- EvalPlot(
    data = TestData,
    PredictionColName = "UpdatedPrediction",
    TargetColName = eval(TargetColumnName),
    GraphType = "calibration",
    PercentileBucket = 0.05,
    aggrfun = function(x) mean(x, na.rm = TRUE))
  
  # Add Number of Trees to Title
  EvaluationPlot <- EvaluationPlot +
    ggplot2::ggtitle(paste0("Calibration Evaluation Plot: R2 = ", round(r_squared, 3L)))
  
  # Save plot to file
  if (SaveModelObjects) {
    if(!is.null(MetaDataPaths[1L])) {
      ggplot2::ggsave(paste0(MetaDataPaths[1L], "/", ModelID, "_EvaluationPlot.png"))
    } else {
      ggplot2::ggsave(paste0(Paths[1L], "/", ModelID, "_EvaluationPlot.png"))      
    }
  }
  
  # Regression Evaluation Calibration Plot----
  EvaluationBoxPlot <- EvalPlot(
    data = TestData,
    PredictionColName = "UpdatedPrediction",
    TargetColName = eval(TargetColumnName),
    GraphType = "boxplot",
    PercentileBucket = 0.05,
    aggrfun = function(x) mean(x, na.rm = TRUE))
  
  # Add Number of Trees to Title----
  EvaluationBoxPlot <- EvaluationBoxPlot +
    ggplot2::ggtitle(paste0("Calibration Evaluation Plot: R2 = ", round(r_squared, 3L)))
  
  # Save plot to file----
  if (SaveModelObjects) {
    if(!is.null(MetaDataPaths[1L])) {
      ggplot2::ggsave(paste0(MetaDataPaths[1L], "/", ModelID, "_EvaluationBoxPlot.png"))
    } else {
      ggplot2::ggsave(paste0(Paths[1L], "/", ModelID, "_EvaluationBoxPlot.png"))      
    }
  }
  
  # Regression Evaluation Metrics----
  EvaluationMetrics <- data.table::data.table(Metric = c("Poisson", "MAE","MAPE", "MSE", "MSLE","KL", "CS", "R2"), MetricValue = rep(999999, 8))
  i <- 0L
  MinVal <- min(TestData[, min(get(TargetColumnName))], TestData[, min(UpdatedPrediction)])
  for (metric in c("poisson", "mae", "mape", "mse", "msle", "kl", "cs", "r2")) {
    i <- as.integer(i + 1L)
    tryCatch({
      if (tolower(metric) == "poisson") {
        if (MinVal > 0L & min(TestData[["UpdatedPrediction"]], na.rm = TRUE) > 0L) {
          TestData[, Metric := UpdatedPrediction - get(TargetColumnName) * log(UpdatedPrediction + 1)]
          Metric <- TestData[, mean(Metric, na.rm = TRUE)]
        }
      } else if (tolower(metric) == "mae") {
        TestData[, Metric := abs(get(TargetColumnName) - UpdatedPrediction)]
        Metric <- TestData[, mean(Metric, na.rm = TRUE)]
      } else if (tolower(metric) == "mape") {
        TestData[, Metric := abs((get(TargetColumnName) - UpdatedPrediction) / (get(TargetColumnName) + 1))]
        Metric <- TestData[, mean(Metric, na.rm = TRUE)]
      } else if (tolower(metric) == "mse") {
        TestData[, Metric := (get(TargetColumnName) - UpdatedPrediction) ^ 2L]
        Metric <- TestData[, mean(Metric, na.rm = TRUE)]
      } else if (tolower(metric) == "msle") {
        if (MinVal > 0L & min(TestData[["UpdatedPrediction"]], na.rm = TRUE) > 0L) {
          TestData[, Metric := (log(get(TargetColumnName) + 1) - log(UpdatedPrediction + 1)) ^ 2L]
          Metric <- TestData[, mean(Metric, na.rm = TRUE)]
        }
      } else if (tolower(metric) == "kl") {
        if (MinVal > 0L & min(TestData[["UpdatedPrediction"]], na.rm = TRUE) > 0L) {
          TestData[, Metric := get(TargetColumnName) * log((get(TargetColumnName) + 1) / (UpdatedPrediction + 1))]
          Metric <- TestData[, mean(Metric, na.rm = TRUE)]
        }
      } else if (tolower(metric) == "cs") {
        TestData[, ':=' (
          Metric1 = get(TargetColumnName) * UpdatedPrediction,
          Metric2 = get(TargetColumnName) ^ 2L,
          Metric3 = UpdatedPrediction ^ 2L)]
        Metric <- TestData[, sum(Metric1, na.rm = TRUE)] / (sqrt(TestData[, sum(Metric2, na.rm = TRUE)]) * sqrt(TestData[, sum(Metric3, na.rm = TRUE)]))
      } else if (tolower(metric) == "r2") {
        TestData[, ':=' (Metric1 = (get(TargetColumnName) - mean(get(TargetColumnName))) ^ 2L, Metric2 = (get(TargetColumnName) - UpdatedPrediction) ^ 2)]
        Metric <- 1 - TestData[, sum(Metric2, na.rm = TRUE)] / TestData[, sum(Metric1, na.rm = TRUE)]
      }
      data.table::set(EvaluationMetrics, i = i, j = 2L, value = round(Metric, 4L))
      data.table::set(EvaluationMetrics, i = i, j = 3L, value = NA)
    }, error = function(x) "skip")
  }
  
  # Remove Cols----
  TestData[, ':=' (Metric = NULL, Metric1 = NULL, Metric2 = NULL, Metric3 = NULL)]
  
  # Save EvaluationMetrics to File
  EvaluationMetrics <- EvaluationMetrics[MetricValue != 999999]
  if (SaveModelObjects) {
    if(!is.null(MetaDataPaths[1])) {
      data.table::fwrite(EvaluationMetrics, file = paste0(MetaDataPaths[1], "/", ModelID, "_EvaluationMetrics.csv"))
    } else {
      data.table::fwrite(EvaluationMetrics, file = paste0(Paths[1L], "/", ModelID, "_EvaluationMetrics.csv"))      
    }
  }
  
  # Regression Partial Dependence----
  ParDepPlots <- list()
  j <- 0L
  ParDepBoxPlots <- list()
  k <- 0L
  for (i in seq_len(min(length(FeatureColNames), NumOfParDepPlots))) {
    tryCatch({
      Out <- ParDepCalPlots(
        data = TestData,
        PredictionColName = "UpdatedPrediction",
        TargetColName = eval(TargetColumnName),
        IndepVar = VariableImportance[i, Variable],
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
        PredictionColName = "UpdatedPrediction",
        TargetColName = eval(TargetColumnName),
        IndepVar = VariableImportance[i, Variable],
        GraphType = "boxplot",
        PercentileBucket = 0.05,
        FactLevels = 10L,
        Function = function(x) mean(x, na.rm = TRUE))
      k <- k + 1L
      ParDepBoxPlots[[paste0(VariableImportance[k, Variable])]] <- Out1
    }, error = function(x) "skip")
  }
  
  # Regression Save ParDepBoxPlots to file----
  if (SaveModelObjects) {
    if(!is.null(MetaDataPaths[1L])) {
      save(ParDepBoxPlots, file = paste0(MetaDataPaths[1L], "/", ModelID, "_ParDepBoxPlots.R"))
    } else {
      save(ParDepBoxPlots, file = paste0(Paths[1L], "/", ModelID, "_ParDepBoxPlots.R"))
    }
  }

  # Return Output----
  return(
    list(
      ClassificationMetrics = ClassEvaluationMetrics,
      FinalTestData = TestData,
      EvaluationPlot = EvaluationPlot,
      EvaluationBoxPlot = EvaluationBoxPlot,
      EvaluationMetrics = EvaluationMetrics,
      PartialDependencePlots = ParDepPlots,
      PartialDependenceBoxPlots = ParDepBoxPlots))
}
