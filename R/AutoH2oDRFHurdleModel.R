#' AutoH2oDRFHurdleModel is generalized hurdle modeling framework
#'
#' @family Supervised Learning
#' @param data Source training data. Do not include a column that has the class labels for the buckets as they are created internally.
#' @param ValidationData Source validation data. Do not include a column that has the class labels for the buckets as they are created internally.
#' @param TestData Souce test data. Do not include a column that has the class labels for the buckets as they are created internally.
#' @param Buckets A numeric vector of the buckets used for subsetting the data. NOTE: the final Bucket value will first create a subset of data that is less than the value and a second one thereafter for data greater than the bucket value.
#' @param TargetColumnName Supply the column name or number for the target variable
#' @param FeatureColNames Supply the column names or number of the features (not included the PrimaryDateColumn)
#' @param TransformNumericColumns Transform numeric column inside the AutoCatBoostRegression() function
#' @param SplitRatios Supply vector of partition ratios. For example, c(0.70,0.20,0,10).
#' @param NThreads Set to the number of threads you would like to dedicate to training
#' @param ModelID Define a character name for your models
#' @param Paths The path to your folder where you want your model information saved
#' @param SaveModelObjects Set to TRUE to save the model objects to file in the folders listed in Paths
#' @param IfSaveModel Save as "mojo" or "standard"
#' @param MaxMem Set the maximum memory your system can provide
#' @param NThreads Set the number of threads you want to dedicate to the model building
#' @param Trees Default 15000
#' @param GridTune Set to TRUE if you want to grid tune the models
#' @param MaxModelsInGrid Set to a numeric value for the number of models to try in grid tune
#' @param NumOfParDepPlots Set to pull back N number of partial dependence calibration plots.
#' @param PassInGrid Pass in a grid for changing up the parameter settings for catboost
#' @return Returns AutoXGBoostRegression() model objects: VariableImportance.csv, Model, ValidationData.csv, EvalutionPlot.png, EvalutionBoxPlot.png, EvaluationMetrics.csv, ParDepPlots.R a named list of features with partial dependence calibration plots, ParDepBoxPlots.R, GridCollect, and the grid used
#' @examples
#' \donttest{
#' Output <- RemixAutoML::AutoH2oDRFHurdleModel(
#'   data,
#'   ValidationData = NULL,
#'   TestData = NULL,
#'   Buckets = 1,
#'   TargetColumnName = "Target_Variable",
#'   FeatureColNames = 4:ncol(data),
#'   TransformNumericColumns = NULL,
#'   SplitRatios = c(0.7, 0.2, 0.1),
#'   NThreads = max(1, parallel::detectCores()-2),
#'   ModelID = "ModelID",
#'   Paths = NULL,
#'   SaveModelObjects = TRUE,
#'   IfSaveModel = "mojo",
#'   Trees = 1000,
#'   GridTune = FALSE,
#'   MaxModelsInGrid = 1,
#'   NumOfParDepPlots = 10,
#'   PassInGrid = NULL)
#' }
#' @export
AutoH2oDRFHurdleModel <- function(data,
                                  ValidationData = NULL,
                                  TestData = NULL,
                                  Buckets = 0,
                                  TargetColumnName = NULL,
                                  FeatureColNames = NULL,
                                  IDcols = NULL,
                                  TransformNumericColumns = NULL,
                                  SplitRatios = c(0.70, 0.20, 0.10),
                                  ModelID = "ModelTest",
                                  Paths = NULL,
                                  SaveModelObjects = TRUE,
                                  IfSaveModel = "mojo",
                                  MaxMem = "28G",
                                  NThreads = max(1, parallel::detectCores()-2),
                                  Trees = 1000,
                                  GridTune = TRUE,
                                  MaxModelsInGrid = 1,
                                  NumOfParDepPlots = 10,
                                  PassInGrid = NULL) {
  # Check args----
  if (is.character(Buckets) |
      is.factor(Buckets) | is.logical(Buckets)) {
    return("Buckets needs to be a numeric scalar or vector")
  }
  if (!is.logical(SaveModelObjects)) {
    return("SaveModelOutput needs to be set to either TRUE or FALSE")
  }
  if (is.character(Trees) |
      is.factor(Trees) | is.logical(Trees) | length(Trees) > 1) {
    return("NumTrees needs to be a numeric scalar")
  }
  if (!is.logical(GridTune)) {
    return("GridTune needs to be either TRUE or FALSE")
  }
  if (is.character(MaxModelsInGrid) |
      is.factor(MaxModelsInGrid) |
      is.logical(MaxModelsInGrid) | length(MaxModelsInGrid) > 1) {
    return("NumberModelsInGrid needs to be a numeric scalar")
  }
  
  # Update working directory----
  working_directory <- getwd()
  if (!is.null(Paths)) {
    if (working_directory != Paths)
      setwd(Paths)
  }
  
  # Initialize H2O----
  h2o::h2o.init(max_mem_size = MaxMem, 
                nthreads = NThreads, 
                enable_assertions = FALSE)
  
  # Initialize collection and counter----
  ModelInformationList <- list()
  if (length(Paths) == 1) {
    Paths <- rep(Paths, length(Buckets) + 1)
  }
  
  # Data.table check----
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  if (!is.null(ValidationData)) {
    if (!data.table::is.data.table(ValidationData)) {
      ValidationData <- data.table::as.data.table(ValidationData)
    }
  }
  if (!is.null(TestData)) {
    if (!data.table::is.data.table(TestData)) {
      TestData <- data.table::as.data.table(TestData)
    }
  }
  
  # FeatureColumnNames----
  if (is.numeric(FeatureColNames) | is.integer(FeatureColNames)) {
    FeatureNames <- names(data)[FeatureColNames]
  } else {
    FeatureNames <- FeatureColNames
  }
  
  # Add target bucket column----
  if(length(Buckets) == 1) {
    data.table::set(
      data,
      i = which(data[[eval(TargetColumnName)]] <= Buckets[1]),
      j = "Target_Buckets",
      value = 0
    )
    data.table::set(
      data,
      i = which(data[[eval(TargetColumnName)]] > Buckets[1]),
      j = "Target_Buckets",
      value = 1
    )
  } else {
    for (i in seq_len(length(Buckets) + 1)) {
      if (i == 1) {
        data.table::set(
          data,
          i = which(data[[eval(TargetColumnName)]] <= Buckets[i]),
          j = "Target_Buckets",
          value = as.factor(Buckets[i])
        )
      } else if (i == length(Buckets) + 1) {
        data.table::set(
          data,
          i = which(data[[eval(TargetColumnName)]] > Buckets[i -
                                                               1]),
          j = "Target_Buckets",
          value = as.factor(paste0(Buckets[i-1], "+"))
        )
      } else {
        data.table::set(
          data,
          i = which(data[[eval(TargetColumnName)]] <= Buckets[i] &
                      data[[eval(TargetColumnName)]] > Buckets[i-1]),
          j = "Target_Buckets",
          value = as.factor(Buckets[i])
        )
      }      
    }
  }
  
  # Add target bucket column----
  if (!is.null(ValidationData)) {
    ValidationData[, Target_Buckets := as.factor(Buckets[1])]
    for (i in seq_len(length(Buckets) + 1)) {
      if (i == 1) {
        data.table::set(
          ValidationData,
          i = which(ValidationData[[eval(TargetColumnName)]] <= Buckets[i]),
          j = "Target_Buckets",
          value = as.factor(Buckets[i])
        )
      } else if (i == length(Buckets) + 1) {
        data.table::set(
          ValidationData,
          i = which(ValidationData[[eval(TargetColumnName)]] > Buckets[i -
                                                                         1]),
          j = "Target_Buckets",
          value = as.factor(paste0(Buckets[i - 1], "+"))
        )
      } else {
        data.table::set(
          ValidationData,
          i = which(ValidationData[[eval(TargetColumnName)]] <= Buckets[i] &
                      ValidationData[[eval(TargetColumnName)]] > Buckets[i -
                                                                           1]),
          j = "Target_Buckets",
          value = as.factor(Buckets[i])
        )
      }
    }
  }
  
  # Add target bucket column----
  if (!is.null(TestData)) {
    TestData[, Target_Buckets := as.factor(Buckets[1])]
    for (i in seq_len(length(Buckets) + 1)) {
      if (i == 1) {
        data.table::set(
          TestData,
          i = which(TestData[[eval(TargetColumnName)]] <= Buckets[i]),
          j = "Target_Buckets",
          value = as.factor(Buckets[i])
        )
      } else if (i == length(Buckets) + 1) {
        data.table::set(
          TestData,
          i = which(TestData[[eval(TargetColumnName)]] > Buckets[i-1]),
          j = "Target_Buckets",
          value = as.factor(paste0(Buckets[i - 1], "+"))
        )
      } else {
        data.table::set(
          TestData,
          i = which(TestData[[eval(TargetColumnName)]] <= Buckets[i] &
                      TestData[[eval(TargetColumnName)]] > Buckets[i -
                                                                     1]),
          j = "Target_Buckets",
          value = as.factor(Buckets[i])
        )
      }
    }
  }
  
  # AutoDataPartition if Validation and TestData are NULL----
  if (is.null(ValidationData) & is.null(TestData)) {
    DataSets <- AutoDataPartition(
      data = data,
      NumDataSets = 3,
      Ratios = SplitRatios,
      PartitionType = "random",
      StratifyColumnNames = "Target_Buckets",
      TimeColumnName = NULL
    )
    data <- DataSets$TrainData
    ValidationData <- DataSets$ValidationData
    TestData <- DataSets$TestData
    rm(DataSets)
  }
  
  # Begin classification model building----
  if (length(Buckets) == 1) {
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
      model_path = Paths,
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
      model_path = Paths,
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
    ModelPath = Paths,
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
  if(length(Buckets) == 1) {
    data.table::setnames(TestData, "p0","Predictions_C0")
    data.table::setnames(TestData, "p1","Predictions_C1")
  } else {
    data.table::setnames(TestData,
                         names(TestData)[1:length(Buckets)],
                         paste0("P_",gsub('[[:punct:] ]+',' ',names(TestData)[1:length(Buckets)])))
    data.table::setnames(TestData,
                         names(TestData)[length(Buckets)+1],
                         paste0("P+_",gsub('[[:punct:] ]+',' ',names(TestData)[length(Buckets)+1])))
  }

  # Remove Model Object----
  rm(ClassModel)
  
  # Remove Target_Buckets----
  data[, Target_Buckets := NULL]
  ValidationData[, Target_Buckets := NULL]
  
  # Begin regression model building----
  counter <- 0
  Degenerate <- 0
  for (bucket in rev(seq_len(length(Buckets) + 1))) {
    # Filter By Buckets----
    if (bucket == max(seq_len(length(Buckets) + 1))) {
      if (!is.null(TestData)) {
        trainBucket <-
          data[get(TargetColumnName) > eval(Buckets[bucket - 1])]
        validBucket <-
          ValidationData[get(TargetColumnName) > eval(Buckets[bucket - 1])]
        testBucket <-
          TestData[get(TargetColumnName) > eval(Buckets[bucket - 1])]
        testBucket[, setdiff(names(testBucket), names(data)) := NULL]
      } else {
        trainBucket <-
          data[get(TargetColumnName) > eval(Buckets[bucket - 1])]
        validBucket <-
          ValidationData[get(TargetColumnName) > eval(Buckets[bucket - 1])]
        testBucket <- NULL
      }
    } else if (bucket == 1) {
      if (!is.null(TestData)) {
        trainBucket <- data[get(TargetColumnName) <= eval(Buckets[bucket])]
        validBucket <-
          ValidationData[get(TargetColumnName) <= eval(Buckets[bucket])]
        testBucket <-
          TestData[get(TargetColumnName) <= eval(Buckets[bucket])]
        testBucket[, setdiff(names(testBucket), names(data)) := NULL]
      } else {
        trainBucket <- data[get(TargetColumnName) <= eval(Buckets[bucket])]
        validBucket <-
          ValidationData[get(TargetColumnName) <= eval(Buckets[bucket])]
        testBucket <- NULL
      }
    } else {
      if (!is.null(TestData)) {
        trainBucket <- data[get(TargetColumnName) <= eval(Buckets[bucket]) &
                              get(TargetColumnName) > eval(Buckets[bucket -
                                                                     1])]
        validBucket <-
          ValidationData[get(TargetColumnName) <= eval(Buckets[bucket]) &
                           get(TargetColumnName) > eval(Buckets[bucket -
                                                                  1])]
        testBucket <-
          TestData[get(TargetColumnName) <= eval(Buckets[bucket]) &
                     get(TargetColumnName) > eval(Buckets[bucket -
                                                            1])]
        testBucket[, setdiff(names(testBucket), names(data)) := NULL]
      } else {
        trainBucket <- data[get(TargetColumnName) <= eval(Buckets[bucket]) &
                              get(TargetColumnName) > eval(Buckets[bucket -
                                                                     1])]
        validBucket <-
          ValidationData[get(TargetColumnName) <= eval(Buckets[bucket]) &
                           get(TargetColumnName) > eval(Buckets[bucket -
                                                                  1])]
        testBucket <- NULL
      }
    }
    
    # Load Winning Grid if it exists----
    if (file.exists(paste0(Paths[bucket], "/grid", Buckets[bucket], ".csv"))) {
      gridSaved <-
        data.table::fread(paste0(Paths[bucket], "/grid", Buckets[bucket], ".csv"))
    }
    
    # AutoCatBoostRegression()----
    if (trainBucket[, .N] != 0) {
      if (var(trainBucket[[eval(TargetColumnName)]]) > 0) {
        counter <- counter + 1
        if (bucket == max(seq_len(length(Buckets) + 1))) {
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
            ModelID = paste0(ModelID,"_",bucket-1),
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
        if (bucket == max(seq_len(length(Buckets) + 1))) {
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
        if (bucket == max(seq_len(length(Buckets) + 1))) {
          data.table::setnames(TestData,
                               "Predictions",
                               paste0("Predictions_", Buckets[bucket - 1], "+"))
        } else {
          data.table::setnames(TestData,
                               "Predictions",
                               paste0("Predictions_", Buckets[bucket]))
        }
      } else {
        
        # Use single value for predictions in the case of zero variance----
        if (bucket == max(seq_len(length(Buckets) + 1))) {
          Degenerate <- Degenerate + 1
          TestData[, paste0("Predictions_", Buckets[bucket - 1], "+") := Buckets[bucket]]
          data.table::setcolorder(TestData, c(ncol(TestData), 1:(ncol(TestData)-1)))
        } else {
          Degenerate <- Degenerate + 1
          TestData[, paste0("Predictions_", Buckets[bucket]) := Buckets[bucket]]
          data.table::setcolorder(TestData, c(ncol(TestData), 1:(ncol(TestData)-1)))
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
  if(counter > 2) {
    for (i in seq_len(length(Buckets)+1)) {
      if (i == 1) {
        data.table::set(TestData,
                        j = "UpdatedPrediction",
                        value = TestData[[i]] *
                          TestData[[i + counter + Degenerate]])
      } else {
        data.table::set(TestData,
                        j = "UpdatedPrediction",
                        value = TestData[["UpdatedPrediction"]] +
                          TestData[[i]] *
                          TestData[[i + counter + Degenerate]])
      }
    }  
  } else if(counter == 2 & length(Buckets) != 1) {
    for (i in seq_len(length(Buckets)+1)) {
      if (i == 1) {
        data.table::set(TestData,
                        j = "UpdatedPrediction",
                        value = TestData[[i]] *
                          TestData[[i + 1 + counter]])
      } else {
        data.table::set(TestData,
                        j = "UpdatedPrediction",
                        value = TestData[["UpdatedPrediction"]] +
                          TestData[[i]] *
                          TestData[[i + 1 + counter]])
      }
    }  
  } else if(counter == 2 & length(Buckets) == 1) {
    data.table::set(TestData,
                    j = "UpdatedPrediction",
                    value = TestData[[1]] * TestData[[3]] + 
                      TestData[[2]] * TestData[[4]])
  } else {
    data.table::set(TestData,
                    j = "UpdatedPrediction",
                    value = TestData[[1]] * TestData[[3]] + 
                      TestData[[2]] * TestData[[4]])
  }
  
  # Regression r2 via sqrt of correlation
  r_squared <-
    (TestData[, stats::cor(get(TargetColumnName), UpdatedPrediction)]) ^ 2
  
  # Regression Save Validation Data to File----
  if (SaveModelObjects) {
    data.table::fwrite(TestData,
                       file = paste0(Paths[1],
                                     "/",
                                     ModelID,
                                     "_ValidationData.csv"))
  }
  
  # Regression Evaluation Calibration Plot----
  EvaluationPlot <- EvalPlot(
    data = TestData,
    PredictionColName = "UpdatedPrediction",
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
    ggplot2::ggsave(paste0(Paths[1],
                           "/",
                           ModelID, "_EvaluationPlot.png"))
  }
  
  # Regression Evaluation Calibration Plot----
  EvaluationBoxPlot <- EvalPlot(
    data = TestData,
    PredictionColName = "UpdatedPrediction",
    TargetColName = eval(TargetColumnName),
    GraphType = "boxplot",
    PercentileBucket = 0.05,
    aggrfun = function(x)
      mean(x, na.rm = TRUE)
  )
  
  # Add Number of Trees to Title----
  EvaluationBoxPlot <- EvaluationBoxPlot +
    ggplot2::ggtitle(paste0("Calibration Evaluation Plot: R2 = ",
                            round(r_squared, 3)))
  
  # Save plot to file----
  if (SaveModelObjects) {
    ggplot2::ggsave(paste0(Paths[1],
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
  MinVal <-
    min(TestData[, min(get(TargetColumnName))], TestData[, min(UpdatedPrediction)])
  for (metric in c("poisson", "mae", "mape", "mse", "msle", "kl", "cs", "r2")) {
    i <- as.integer(i + 1)
    tryCatch({
      # Regression Grid Evaluation Metrics----
      if (tolower(metric) == "poisson") {
        if (MinVal > 0 &
            min(TestData[["UpdatedPrediction"]], na.rm = TRUE) > 0) {
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
        TestData[, Metric := (get(TargetColumnName) - UpdatedPrediction) ^ 2]
        Metric <- TestData[, mean(Metric, na.rm = TRUE)]
      } else if (tolower(metric) == "msle") {
        if (MinVal > 0 &
            min(TestData[["UpdatedPrediction"]], na.rm = TRUE) > 0) {
          TestData[, Metric := (log(get(TargetColumnName) + 1) - log(UpdatedPrediction + 1)) ^ 2]
          Metric <- TestData[, mean(Metric, na.rm = TRUE)]
        }
      } else if (tolower(metric) == "kl") {
        if (MinVal > 0 &
            min(TestData[["UpdatedPrediction"]], na.rm = TRUE) > 0) {
          TestData[, Metric := get(TargetColumnName) * log((get(TargetColumnName) + 1) /
                                                             (UpdatedPrediction + 1))]
          Metric <- TestData[, mean(Metric, na.rm = TRUE)]
        }
      } else if (tolower(metric) == "cs") {
        TestData[, ':=' (
          Metric1 = get(TargetColumnName) * UpdatedPrediction,
          Metric2 = get(TargetColumnName) ^ 2,
          Metric3 = UpdatedPrediction ^ 2
        )]
        Metric <-
          TestData[, sum(Metric1, na.rm = TRUE)] / (sqrt(TestData[, sum(Metric2, na.rm = TRUE)]) *
                                                      sqrt(TestData[, sum(Metric3, na.rm = TRUE)]))
      } else if (tolower(metric) == "r2") {
        TestData[, ':=' (
          Metric1 = (get(TargetColumnName) - mean(get(
            TargetColumnName
          ))) ^ 2,
          Metric2 = (get(TargetColumnName) - UpdatedPrediction) ^ 2
        )]
        Metric <-
          1 - TestData[, sum(Metric2, na.rm = TRUE)] /
          TestData[, sum(Metric1, na.rm = TRUE)]
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
  
  # Remove Cols----
  TestData[, ':=' (
    Metric = NULL,
    Metric1 = NULL,
    Metric2 = NULL,
    Metric3 = NULL
  )]
  
  # Save EvaluationMetrics to File
  EvaluationMetrics <- EvaluationMetrics[MetricValue != 999999]
  if (SaveModelObjects) {
    data.table::fwrite(EvaluationMetrics,
                       file = paste0(Paths[1],
                                     "/",
                                     ModelID, "_EvaluationMetrics.csv"))
  }
  
  # Regression Partial Dependence----
  ParDepPlots <- list()
  j <- 0
  ParDepBoxPlots <- list()
  k <- 0
  for (i in seq_len(min(length(FeatureColNames), NumOfParDepPlots))) {
    tryCatch({
      Out <- ParDepCalPlots(
        data = TestData,
        PredictionColName = "UpdatedPrediction",
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
        PredictionColName = "UpdatedPrediction",
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
  
  # Regression Save ParDepBoxPlots to file----
  if (SaveModelObjects) {
    save(ParDepBoxPlots,
         file = paste0(Paths[1], "/", ModelID, "_ParDepBoxPlots.R"))
  }
  
  # Reset workding directory
  # Update working directory----
  setwd(working_directory)
  
  # Return Output----
  return(
    list(
      ClassificationMetrics = ClassEvaluationMetrics,
      FinalTestData = TestData,
      EvaluationPlot = EvaluationPlot,
      EvaluationBoxPlot = EvaluationBoxPlot,
      EvaluationMetrics = EvaluationMetrics,
      PartialDependencePlots = ParDepPlots,
      PartialDependenceBoxPlots = ParDepBoxPlots
    )
  )
}
