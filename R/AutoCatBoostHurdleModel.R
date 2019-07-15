#' AutoCatBoostdHurdleModel is a Retrain Function for the Regression Models for the Subsetted Data in P6
#'
#' @family Supervised Learning
#' @param data Source training data. Do not include a column that has the class labels for the buckets as they are created internally.
#' @param ValidationData Source validation data. Do not include a column that has the class labels for the buckets as they are created internally.
#' @param TestData Souce test data. Do not include a column that has the class labels for the buckets as they are created internally.
#' @param Buckets A numeric vector of the buckets used for subsetting the data. NOTE: the final Bucket value will first create a subset of data that is less than the value and a second one thereafter for data greater than the bucket value.
#' @param TargetColumnName Supply the column name or number for the target variable
#' @param FeatureColNames Supply the column names or number of the features (not included the PrimaryDateColumn)
#' @param PrimaryDateColumn Supply a date column if the data is functionally related to it
#' @param IDcols Includes PrimaryDateColumn and any other columns you want returned in the validation data with predictions
#' @param TransformNumericColumns Transform numeric column inside the AutoCatBoostRegression() function
#' @param ClassWeights Utilize these for the classifier model
#' @param SplitRatios Supply vector of partition ratios. For example, c(0.70,0.20,0,10).
#' @param task_type Set to "GPU" or "CPU"
#' @param ModelID Define a character name for your models
#' @param Paths A character vector of the path file strings. EITHER SUPPLY 1 file path or N file paths for N models
#' @param SaveModelObjects Set to TRUE to save the model objects to file in the folders listed in Paths
#' @param Trees Default 15000
#' @param GridTune Set to TRUE if you want to grid tune the models
#' @param MaxModelsInGrid Set to a numeric value for the number of models to try in grid tune
#' @param NumOfParDepPlots Set to pull back N number of partial dependence calibration plots.
#' @param PassInGrid Pass in a grid for changing up the parameter settings for catboost
#' @return Returns AutoCatBoostRegression() model objects: VariableImportance.csv, Model, ValidationData.csv, EvalutionPlot.png, EvalutionBoxPlot.png, EvaluationMetrics.csv, ParDepPlots.R a named list of features with partial dependence calibration plots, ParDepBoxPlots.R, GridCollect, and catboostgrid
#' @examples
#' \donttest{
#' Output <- RemixAutoML::AutoCatBoostdHurdleModel(
#'   data,
#'   ValidationData = NULL,
#'   TestData = NULL,
#'   Buckets = 1,
#'   TargetColumnName = "Target_Variable",
#'   FeatureColNames = 4:ncol(data),
#'   PrimaryDateColumn = "Date",
#'   IDcols = 1:3,
#'   TransformNumericColumns = NULL,
#'   ClassWeights = NULL,
#'   SplitRatios = c(0.7, 0.2, 0.1),
#'   task_type = "GPU",
#'   ModelID = "ModelID",
#'   Paths = NULL,
#'   SaveModelObjects = TRUE,
#'   Trees = 1000,
#'   GridTune = FALSE,
#'   MaxModelsInGrid = 1,
#'   NumOfParDepPlots = 10,
#'   PassInGrid = NULL)
#' }
#' @export
AutoCatBoostdHurdleModel <- function(data,
                                     ValidationData = NULL,
                                     TestData = NULL,
                                     Buckets = 0,
                                     TargetColumnName = NULL,
                                     FeatureColNames = NULL,
                                     PrimaryDateColumn = NULL,
                                     IDcols = NULL,
                                     TransformNumericColumns = NULL,
                                     ClassWeights = NULL,
                                     SplitRatios = c(0.70, 0.20, 0.10),
                                     task_type = "GPU",
                                     ModelID = "ModelTest",
                                     Paths = NULL,
                                     SaveModelObjects = TRUE,
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
    if (working_directory != Paths[1])
      setwd(Paths[1])
  }
  
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
  
  # IDcols to Names----
  if (!is.null(IDcols)) {
    if (is.numeric(IDcols) | is.integer(IDcols)) {
      IDcols <- names(data)[IDcols]
    }
  }
  
  # Primary Date Column----
  if (is.numeric(PrimaryDateColumn) |
      is.integer(PrimaryDateColumn)) {
    PrimaryDateColumn <- names(data)[PrimaryDateColumn]
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
    ClassifierModel <- AutoCatBoostClassifier(
      data = data,
      ValidationData = ValidationData,
      TestData = TestData,
      TargetColumnName = "Target_Buckets",
      FeatureColNames = FeatureNames,
      PrimaryDateColumn = PrimaryDateColumn,
      ClassWeights = ClassWeights,
      IDcols = IDcols,
      MaxModelsInGrid = MaxModelsInGrid,
      task_type = task_type,
      eval_metric = "AUC",
      grid_eval_metric = "auc",
      Trees = Trees,
      GridTune = GridTune,
      model_path = Paths[1],
      ModelID = ModelID,
      NumOfParDepPlots = NumOfParDepPlots,
      ReturnModelObjects = TRUE,
      SaveModelObjects = SaveModelObjects,
      PassInGrid = NULL
    )
  } else {
    ClassifierModel <- AutoCatBoostMultiClass(
      data = data,
      ValidationData = ValidationData,
      TestData = TestData,
      TargetColumnName = "Target_Buckets",
      FeatureColNames = FeatureNames,
      PrimaryDateColumn = PrimaryDateColumn,
      ClassWeights = ClassWeights,
      IDcols = IDcols,
      MaxModelsInGrid = MaxModelsInGrid,
      task_type = task_type,
      eval_metric = "MultiClass",
      grid_eval_metric = "Accuracy",
      Trees = Trees,
      GridTune = GridTune,
      model_path = Paths[1],
      ModelID = ModelID,
      ReturnModelObjects = TRUE,
      SaveModelObjects = SaveModelObjects,
      PassInGrid = NULL
    )
  }
  
  # Store metadata----
  ClassModel <- ClassifierModel$Model
  ClassEvaluationMetrics <- ClassifierModel$EvaluationMetrics
  VariableImportance <- ClassifierModel$VariableImportance
  if(length(Buckets > 1)) {
    TargetLevels <- ClassifierModel$TargetLevels
  } else {
    TargetLevels <- NULL
  }
  rm(ClassifierModel)
  
  # Add Target to IDcols----
  IDcols <- c(IDcols, TargetColumnName)
  
  # Score Classification Model----
  if (length(Buckets) == 1) {
    TargetType <- "Classification"
  } else {
    TargetType <- "Multiclass"
  }
   
  # Model Scoring---- 
  TestData <- AutoCatBoostScoring(
    TargetType = TargetType,
    ScoringData = TestData,
    FeatureColumnNames = FeatureNames,
    IDcols = IDcols,
    ModelObject = ClassModel,
    ModelPath = NULL,
    ModelID = ModelID,
    ReturnFeatures = TRUE,
    MultiClassTargetLevels = TargetLevels,
    TransformNumeric = FALSE, 
    BackTransNumeric = FALSE, 
    TargetColumnName = "Adrian", 
    TransformationObject = NULL, 
    TransID = NULL, 
    TransPath = Path[1],
    MDP_Impute = FALSE,
    MDP_CharToFactor = TRUE,
    MDP_RemoveDates = FALSE, 
    MDP_MissFactor = "0",
    MDP_MissNum = -1
  )

  # Remove Model Object----
  rm(ClassModel)
  
  # Remove Target_Buckets----
  data[, Target_Buckets := NULL]
  ValidationData[, Target_Buckets := NULL]
  
  # Remove Target From IDcols----
  IDcols <- IDcols[!(IDcols %chin% TargetColumnName)]
  
  # Change Name of Predicted MultiClass Column----
  if(length(Buckets) == 1) {
    data.table::setnames(TestData, "Predictions", "Predictions_Classification")    
  } else {
    data.table::setnames(TestData, "Predictions", "Predictions_MultiClass")
  }
  
  # Begin regression model building----
  counter <- 0
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
    
    # Create Modified IDcols----
    IDcolsModified <-
      c(IDcols, setdiff(names(TestData), names(trainBucket)), TargetColumnName)      
    
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
          TestModel <- AutoCatBoostRegression(
            data = trainBucket,
            ValidationData = validBucket,
            TestData = testBucket,
            TargetColumnName = TargetColumnName,
            TransformNumericColumns = TransformNumericColumns,
            FeatureColNames = FeatureNames,
            PrimaryDateColumn = PrimaryDateColumn,
            IDcols = IDcols,
            MaxModelsInGrid = MaxModelsInGrid,
            task_type = task_type,
            eval_metric = "RMSE",
            grid_eval_metric = "r2",
            Trees = Trees,
            GridTune = GridTune,
            model_path = Paths[bucket - 1],
            ModelID = paste0(ModelID,"_",bucket-1,"+"),
            NumOfParDepPlots = NumOfParDepPlots,
            ReturnModelObjects = TRUE,
            SaveModelObjects = SaveModelObjects,
            PassInGrid = PassInGrid
          )
        } else {
          TestModel <- AutoCatBoostRegression(
            data = trainBucket,
            ValidationData = validBucket,
            TestData = testBucket,
            TargetColumnName = TargetColumnName,
            TransformNumericColumns = TransformNumericColumns,
            FeatureColNames = FeatureNames,
            PrimaryDateColumn = PrimaryDateColumn,
            IDcols = IDcols,
            MaxModelsInGrid = MaxModelsInGrid,
            task_type = task_type,
            eval_metric = "RMSE",
            grid_eval_metric = "r2",
            Trees = Trees,
            GridTune = GridTune,
            model_path = Paths[bucket],
            ModelID = paste0(ModelID, "_", bucket),
            NumOfParDepPlots = NumOfParDepPlots,
            ReturnModelObjects = TRUE,
            SaveModelObjects = SaveModelObjects,
            PassInGrid = NULL
          )
        }
        
        # Store Model----
        RegressionModel <- TestModel$Model
        if(!is.null(TransformNumericColumns)) {
          TransformationResults <- TestModel$TransformationResults
        }
        rm(TestModel)
        
        # Garbage Collection----
        gc()
        
        # Score TestData----
        if (bucket == max(seq_len(length(Buckets) + 1))) {
          if(!is.null(TransformNumericColumns)) {
            TestData <- AutoCatBoostScoring(
              TargetType = "regression",
              ScoringData = TestData,
              FeatureColumnNames = FeatureNames,
              IDcols = IDcolsModified,
              ModelObject = RegressionModel,
              ModelPath = Path[buckets - 1],
              ModelID = paste0(ModelID,"_",bucket-1,"+"),
              ReturnFeatures = TRUE,
              TransformationObject = TransformationResults,
              TargetColumnName = eval(TargetColumnName),
              TransformNumeric = TRUE,
              BackTransNumeric = TRUE,
              TransID = NULL,
              TransPath = NULL,
              MDP_Impute = TRUE,
              MDP_CharToFactor = TRUE,
              MDP_RemoveDates = FALSE,
              MDP_MissFactor = "0",
              MDP_MissNum = -1
            )
          } else {
            TestData <- AutoCatBoostScoring(
              TargetType = "regression",
              ScoringData = TestData,
              FeatureColumnNames = FeatureNames,
              IDcols = IDcolsModified,
              ModelObject = RegressionModel,
              ModelPath = Path[buckets - 1],
              ModelID = paste0(ModelID, "_", bucket),
              ReturnFeatures = TRUE,
              TransformNumeric = FALSE,
              BackTransNumeric = FALSE,
              TargetColumnName = eval(TargetColumnName),
              TransformationObject = NULL,
              TransID = NULL, 
              TransPath = NULL,
              MDP_Impute = TRUE,
              MDP_CharToFactor = TRUE,
              MDP_RemoveDates = FALSE,
              MDP_MissFactor = "0",
              MDP_MissNum = -1
            )            
          }
        } else {
          if(!is.null(TransformNumericColumns)) {
            TestData <- AutoCatBoostScoring(
              TargetType = "regression",
              ScoringData = TestData,
              FeatureColumnNames = FeatureNames,
              IDcols = IDcolsModified,
              ModelObject = RegressionModel,
              ModelPath = Path[buckets],
              ModelID = paste0("P6_", bucket),
              ReturnFeatures = TRUE,
              TransformNumeric = TRUE,
              BackTransNumeric = TRUE,
              TargetColumnName = eval(TargetColumnName),
              TransformationObject = TransformationResults,
              TransID = NULL,
              TransPath = NULL,
              MDP_Impute = TRUE,
              MDP_CharToFactor = TRUE,
              MDP_RemoveDates = FALSE,
              MDP_MissFactor = "0",
              MDP_MissNum = -1
            )
          } else {
            TestData <- AutoCatBoostScoring(
              TargetType = "regression",
              ScoringData = TestData,
              FeatureColumnNames = FeatureNames,
              IDcols = IDcolsModified,
              ModelObject = RegressionModel,
              ModelPath = Path[buckets],
              ModelID = paste0("P6_", bucket),
              ReturnFeatures = TRUE,
              TransformNumeric = TRUE,
              BackTransNumeric = TRUE,
              TargetColumnName = eval(TargetColumnName),
              TransformationObject = NULL,
              TransID = NULL,
              TransPath = NULL,
              MDP_Impute = TRUE,
              MDP_CharToFactor = TRUE,
              MDP_RemoveDates = FALSE,
              MDP_MissFactor = "0",
              MDP_MissNum = -1
            )
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
          TestData[, paste0("Predictions", Buckets[bucket - 1], "+") := Buckets[bucket]]
        } else {
          TestData[, paste0("Predictions", Buckets[bucket]) := Buckets[bucket]]
        }
      }
    }
  }
  
  # Rearrange Column order----
  if(counter != 1) {
    if(length(IDcols) != 0) {
      data.table::setcolorder(TestData, c(2:(1 + length(IDcols)), 1, (2 + length(IDcols)):ncol(TestData)))
      data.table::setcolorder(TestData, c(
        1:length(IDcols),
        (length(IDcols) + counter + 1),
        (length(IDcols) + counter + 1 + counter +
           1):ncol(TestData),
        (length(IDcols) + 1):(length(IDcols) +
                                counter),
        (length(IDcols) + counter + 2):(length(IDcols) + counter + 1 + counter)
      ))
    } else {
      data.table::setcolorder(TestData, c(counter+1, 1:counter, (counter+2):ncol(TestData)))
      data.table::setcolorder(x, c(1,(counter*2+2):ncol(TestData),2:(counter*2+1)))
    }
  } else {
    if(length(IDcols) != 0) {
      data.table::setcolorder(TestData, c(2:(1+length(IDcols)),1,(2+length(IDcols)):ncol(TestData)))
      data.table::setcolorder(TestData, c(1:length(IDcols),(1+length(IDcols)+2):(ncol(TestData)-1),(1+length(IDcols)):(1+length(IDcols)+1),ncol(TestData)))
    } else {
      data.table::setcolorder(TestData, c(3:(ncol(TestData)-1), 1,2,ncol(TestData)))
    }
  }

  # Final Combination of Predictions----
  # Logic: 1 Buckets --> 4 columns of preds
  #        2 Buckets --> 6 columns of preds
  #        3 Buckets --> 8 columns of preds
  # Secondary logic: for i == 1, need to create the final column first
  #                  for i > 1, need to take the final column and add the product of the next preds
  Cols <- ncol(TestData)
  if(counter != 1) {
    for (i in seq_len(length(Buckets) + 1)) {
      if (length(Buckets) == 1) {
        if (i == 1) {
          data.table::set(TestData,
                          j = "UpdatedPrediction",
                          value = TestData[[(Cols - (4 - i))]] *
                            TestData[[Cols - (2 - i)]])
        } else {
          data.table::set(TestData,
                          j = "UpdatedPrediction",
                          value = TestData[["UpdatedPrediction"]] +
                            TestData[[(Cols - (4 - i))]] *
                            TestData[[(Cols - (2 - i))]])
        }
      } else {
        if (i == 1) {
          data.table::set(TestData,
                          j = "UpdatedPrediction",
                          value = TestData[[(Cols - ((length(Buckets) + 1) * 2 - i))]] *
                            TestData[[(Cols - ((length(Buckets) + 1) - i))]])
        } else {
          data.table::set(TestData,
                          j = "UpdatedPrediction",
                          value = TestData[["UpdatedPrediction"]] +
                            TestData[[(Cols - ((length(Buckets) + 1) * 2 - i))]] *
                            TestData[[(Cols - ((length(Buckets) + 1) - i))]])
        }
      }
    }  
  } else {
    data.table::set(TestData,
                    j = "UpdatedPrediction",
                    value = TestData[[ncol(TestData)]] * (1 - TestData[[(ncol(TestData)-1)]]) + 
                      TestData[[(ncol(TestData)-2)]] * (TestData[[(ncol(TestData)-1)]]))
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
