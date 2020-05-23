#' AutoHurdleScoring() 
#' 
#' 
#' @author Adrian Antico
#' @family Automated Model Scoring
#' @param TestData scoring data.table
#' @param Path Supply if ArgList is NULL or ModelList is null.
#' @param ModelID Supply if ArgList is NULL or ModelList is null. Same as used in model training.
#' @param ModelClass Name of model type. "catboost" is currently the only available option
#' @param ArgList Output from the hurdle model
#' @param ModelList Output from the hurdle model
#' @return A data.table with the final predicted value, the intermediate model predictions, and your source data
#' @examples 
#' \donttest{
#' 
#' # Pull in some data:
#' #   N - Change the number of records pulled in
#' # data <- RemixAutoML::FakeDataGenerator(Correlation = 0.85, N = 25000, ID = 0, ZIP = 1, AddDate = FALSE)
#' # Path <- "C:/Users/aantico/Documents/Package/GUI_Package"
#' 
#' Output <- RemixAutoML::AutoXGBoostHurdleModel(
#' 
#'    # Operationalization args
#'    TreeMethod = "hist",
#'    TrainOnFull = FALSE,
#'    PassInGrid = NULL,
#'
#'    # Metadata args
#'    NThreads = max(1L, parallel::detectCores()-2L),
#'    ModelID = "ModelTest",
#'    Paths = normalizePath(Path),
#'    MetaDataPaths = NULL,
#'
#'    # data args
#'    data,
#'    ValidationData = NULL,
#'    TestData = NULL,
#'    Buckets = 0L,
#'    TargetColumnName = "Adrian",
#'    FeatureColNames = names(data)[2:ncol(data)],
#'    IDcols = NULL,
#'
# options
#'    TransformNumericColumns = NULL,
#'    SplitRatios = c(0.70, 0.20, 0.10),
#'    SaveModelObjects = TRUE,
#'    NumOfParDepPlots = 10L,
#'
# grid tuning args
#'    GridTune = FALSE,
#'    grid_eval_metric = "accuracy",
#'    MaxModelsInGrid = 1L,
#'    BaselineComparison = "default",
#'    MaxRunsWithoutNewWinner = 10L,
#'    MaxRunMinutes = 60L,
#'
#'    # bandit hyperparameters
#'    Trees = 100L,
#'    eta = seq(0.05,0.40,0.05),
#'    max_depth = seq(4L, 16L, 2L),
#'
#'    # random hyperparameters
#'    min_child_weight = seq(1.0, 10.0, 1.0),
#'    subsample = seq(0.55, 1.0, 0.05),
#'    colsample_bytree = seq(0.55, 1.0, 0.05))
#' 
#' # Score models by calling them from file
#' HurdleScores <- RemixAutoML::AutoHurdleScoring(
#'    TestData = data,
#'    Path = Path,
#'    ModelID = "ModelTest",
#'    ModelClass = "catboost",
#'    ModelList = NULL,
#'    ArgList = NULL)
#'
#' # Score models by using output object from training
#' HurdleScores <- RemixAutoML::AutoHurdleScoring(
#'    TestData = data,
#'    Path = Path,
#'    ModelID = "ModelTest",
#'    ModelClass = "catboost",
#'    ModelList = Output$ModelList,
#'    ArgList = Output$ArgsList)
#' }
#' @export
AutoHurdleScoring <- function(TestData = NULL,
                              Path = NULL,
                              ModelID = NULL,
                              ModelClass = "catboost",
                              ArgList = NULL,
                              ModelList = NULL) {
  
  # Load ArgList and ModelList if NULL----
  if(is.null(Path) & (is.null(ArgList) | is.null(ModelList))) return("Supply a value to the Path argument to where the ArgList and ModelList are located")
  
  # Load ArgList and ModelList if not supplied----
  if(is.null(ArgList)) {
    load(file.path(normalizePath(Path), paste0(ModelID, "_HurdleArgList.Rdata")))
    ArgList <- ArgsList
    rm(ArgsList)
  }
  
  # Define Buckets----
  Buckets <- ArgList$Buckets
  
  # Score Classification Model----
  if(length(Buckets) == 1L) TargetType <- "Classification" else TargetType <- "Multiclass"
  
  # Store classifier model----
  if(tolower(ModelClass) == "catboost") if(!is.null(ModelList)) ClassModel <- ModelList[[1L]] else if(!is.null(ArgList$ModelID)) ClassModel <- catboost::catboost.load_model(model_path = file.path(normalizePath(ArgList$Paths), ArgList$ModelID)) else return("Need to supply a ModelList")
  if(tolower(ModelClass) == "xgboost") {
    if(!is.null(ModelList)) {
      ClassModel <- ModelList[[1L]]
    } else if(!is.null(ArgList$ModelID)) {
      load(file.path(normalizePath(ArgList$Paths), ArgList$ModelID))
      ClassModel <- model; rm(model)
    } else {
      return("Need to supply a ModelList")
    }
  } 
  
  # Store FeatureNames----
  FeatureNames <- ArgList$FeatureColNames
  
  # Factor levels list----
  if(!is.null(ArgList$FactorLevelsList)) FactorLevelsList <- ArgList$FactorLevelsList else FactorLevelsList <- NULL
  
  # Store IDcols----
  IDcolsReorder <- ArgList$IDcols
  IDcols <- ArgList$IDcols
  IDcols <- c(IDcols, setdiff(names(TestData), c(IDcols, ArgList$FeatureColNames)))
  
  # Store colnames----
  ColumnNames <- names(TestData)
  
  # Classification Model Scoring---- 
  if(tolower(ModelClass) == "catboost") {
    TestData <- AutoCatBoostScoring(
      TargetType = TargetType,
      ScoringData = TestData,
      FeatureColumnNames = FeatureNames,
      IDcols = IDcols,
      ModelObject = ClassModel,
      ModelPath = ArgList$Paths,
      ModelID = ArgList$ModelID,
      RemoveModel = TRUE,
      ReturnFeatures = TRUE,
      MultiClassTargetLevels = ArgList$TargetLevels,
      TransformationObject = NULL,
      TargetColumnName = NULL,
      TransformNumeric = FALSE,
      BackTransNumeric = FALSE,
      TransID = NULL,
      TransPath = NULL,
      MDP_Impute = FALSE,
      MDP_CharToFactor = TRUE,
      MDP_RemoveDates = FALSE,
      MDP_MissFactor = "0",
      MDP_MissNum = -1)
  } else if(tolower(ModelClass) == "xgboost") {
    TestData <- AutoXGBoostScoring(
      TargetType = TargetType,
      ScoringData = TestData,
      FeatureColumnNames = FeatureNames,
      IDcols = IDcols,
      FactorLevelsList = FactorLevelsList,
      TargetLevels = ArgList$TargetLevels,
      Objective = Objective,
      OneHot = FALSE,
      ModelObject = ClassModel,
      ModelPath = ArgList$Paths,
      ModelID = ArgList$ModelID,
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
  } else if(tolower(ModelClass) == "h2odrf") {
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
  }
  
  # Remove Model Object----
  rm(ClassModel)
  
  # Change name for classification----
  if(tolower(TargetType) == "classification" & tolower(ModelClass) == "catboost") {
    data.table::setnames(TestData, "p1", "Predictions_C1")
    TestData[, Predictions_C0 := 1 - Predictions_C1]
    data.table::setcolorder(TestData, c(ncol(TestData), 1L, 2L:(ncol(TestData) - 1L)))
  } else if(tolower(TargetType) == "classification" & tolower(ModelClass) == "xgboost") {
    data.table::setnames(TestData, "Predictions", "Predictions_C1")
    TestData[, Predictions_C0 := 1 - Predictions_C1]
    data.table::setcolorder(TestData, c(ncol(TestData), 1L, 2L:(ncol(TestData) - 1L)))
  }
  
  # Change Name of Predicted MultiClass Column----
  if(length(Buckets) != 1L) data.table::setnames(TestData, "Predictions", "Predictions_MultiClass")
  
  # Begin regression model building----
  counter <- max(rev(seq_len(length(Buckets) + 1L))) + 1L
  Degenerate <- 0L
  for(bucket in rev(seq_len(length(Buckets) + 1L))) {
    
    # Update IDcols----
    IDcolsModified <- c(IDcols, setdiff(names(TestData), ColumnNames))
    
    # Check for constant value bucket----
    if(!any(bucket %in% c(ArgList$constant))) {
      
      # Increment----
      counter <- counter - 1L
      
      # Score TestData----
      if(bucket == max(seq_len(length(Buckets) + 1L))) ModelIDD <- paste0(ArgList$ModelID,"_",bucket,"+") else ModelIDD <- paste0(ArgList$ModelID, "_", bucket)
      
      # Manage TransformationResults
      if(is.null(ArgList$TransformNumericColumns)) TransformationResults <- NULL else TransformationResults <- ArgList$TransformNumericColumns
      
      # Store Transformations----
      if(!is.null(ArgList$TransformNumericColumns)) {
        TransformationResults <- ArgList[[paste0("TransformationResults_", ModelIDD)]]
        Transform <- TRUE
      } else {
        TransformationResults <- NULL
        Transform <- FALSE
      }
      
      # Model----
      if(!is.null(ModelList)) {
        RegressionModel <- ModelList[[ModelIDD]]
      } else {
        
        # Catboost
        if(tolower(ModelClass) == "catboost") {
          RegressionModel <- catboost::catboost.load_model(model_path = file.path(normalizePath(ArgList$Paths), ModelIDD))
        } 
        
        # XGBoost
        if(tolower(ModelClass) == "xgboost") {
          load(file.path(normalizePath(ArgList$Paths), ModelIDD))
          RegressionModel <- model
        }
      }
      
      # Catboost Model Scroring----
      if(tolower(ModelClass) == "catboost") {
        TestData <- AutoCatBoostScoring(
          TargetType = "regression",
          ScoringData = TestData,
          FeatureColumnNames = FeatureNames,
          IDcols = IDcolsModified,
          ModelObject = RegressionModel,
          ModelPath = Paths,
          ModelID = ModelIDD,
          ReturnFeatures = TRUE,
          TransformationObject = TransformationResults,
          TargetColumnName = ArgList$TransformNumericColumns,
          TransformNumeric = Transform,
          BackTransNumeric = Transform,
          TransID = NULL,
          TransPath = NULL,
          MDP_Impute = TRUE,
          MDP_CharToFactor = TRUE,
          MDP_RemoveDates = FALSE,
          MDP_MissFactor = "0",
          MDP_MissNum = -1)
      }
      
      # XGBoost Model Scoring----
      if(tolower(ModelClass) == "xgboost") {
        TestData <- AutoXGBoostScoring(
          TargetType = "regression",
          ScoringData = TestData,
          FeatureColumnNames = FeatureNames,
          IDcols = IDcolsModified,
          FactorLevelsList = FactorLevelsList,
          OneHot = FALSE,
          ModelObject = RegressionModel,
          ModelPath = Paths,
          ModelID = ModelIDD,
          ReturnFeatures = TRUE,
          TargetColumnName = ArgList$TransformNumericColumns,
          TransformNumeric = Transform,
          BackTransNumeric = Transform,
          TransformationObject = TransformationObject,
          TransID = NULL,
          TransPath = NULL,
          MDP_Impute = TRUE,
          MDP_CharToFactor = TRUE,
          MDP_RemoveDates = FALSE,
          MDP_MissFactor = "0",
          MDP_MissNum = -1)
      }
      
      # H2O DRF Model Scroring----
      if(tolower(ModelClass) == "h2odrf") {
        TestData <- AutoH2OMLScoring(
          ScoringData = TestData,
          ModelObject = RegressionModels,
          ModelType = "mojo",
          H2OShutdown = if(bucket == min(rev(seq_len(length(Buckets) + 1L)))) TRUE else FALSE,
          MaxMem = "28G",
          JavaOptions = '-Xmx1g -XX:ReservedCodeCacheSize=256m',
          ModelPath = ModelPath,
          ModelID = "ModelTest",
          ReturnFeatures = TRUE,
          TargetColumnName = ArgList$TransformNumericColumns,
          TransformNumeric = Transform,
          BackTransNumeric = Transform,
          TransformationObject = TransformationObject,
          TransID = NULL,
          TransPath = NULL,
          MDP_Impute = TRUE,
          MDP_CharToFactor = TRUE,
          MDP_RemoveDates = TRUE,
          MDP_MissFactor = "0",
          MDP_MissNum = -1)
      }
      
      # Remove Model----
      rm(RegressionModel)
      
      # Change prediction name to prevent duplicates----
      if(bucket == max(seq_len(length(Buckets) + 1L))) Val <- paste0("Predictions_", bucket - 1L, "+") else Val <- paste0("Predictions_", bucket)
      data.table::setnames(TestData, "Predictions", Val)
      
    } else {
      
      # Use single value for predictions in the case of zero variance----
      if(bucket == max(seq_len(length(Buckets) + 1L))) {
        data.table::set(TestData, j = paste0("Predictions", Buckets[bucket - 1L], "+"), value = Buckets[bucket])
        data.table::setcolorder(TestData, c(ncol(TestData), 1L:(ncol(TestData)-1L)))
      } else {
        data.table::set(TestData, j = paste0("Predictions", Buckets[bucket]), value = Buckets[bucket])
        data.table::setcolorder(TestData, c(ncol(TestData), 1L:(ncol(TestData)-1L)))
      }
    }
  }
  
  # Rearrange Column order----
  if(counter > 2L) {
    if(length(IDcols) != 0L) {
      if(Degenerate == 0L) {
        data.table::setcolorder(TestData, c(2L:(1L + length(IDcols)), 1L, (2L + length(IDcols)):ncol(TestData)))
        data.table::setcolorder(TestData, c(1L:length(IDcols),(length(IDcols) + counter + 1L),(length(IDcols) + counter + 1L + counter +1L):ncol(TestData), (length(IDcols) + 1L):(length(IDcols) + counter),(length(IDcols) + counter + 2L):(length(IDcols)+counter+1L+counter)))
      } else {
        data.table::setcolorder(TestData, c(3L:(2L + length(IDcols)), 1L:2L, (3L + length(IDcols)):ncol(TestData)))
        data.table::setcolorder(TestData, c(1L:length(IDcols),(length(IDcols) + counter + 1L + Degenerate),(length(IDcols) + counter + 3L + counter + Degenerate):ncol(TestData),(length(IDcols) + 1L):(length(IDcols) + counter + Degenerate),(length(IDcols) + counter + 2L + Degenerate):(length(IDcols)+counter+counter+Degenerate+2L)))
      }
    } else {
      data.table::setcolorder(TestData, c(1L:(counter+Degenerate),(2L+counter+Degenerate):(1L+2L*(counter+Degenerate)),(1L+counter+Degenerate),(2L+2L*(counter+Degenerate)):ncol(TestData)))
      data.table::setcolorder(TestData, c((2L*(counter+Degenerate)+1L):ncol(TestData),1L:(2L*(counter+Degenerate))))
    }
  } else if(counter == 2L & length(Buckets) == 1L) {
    if(length(IDcols) != 0L) data.table::setcolorder(TestData, c(1L, 2L, (3L + length(IDcols)):ncol(TestData), 3L:(2L + length(IDcols))))
  } else if(counter == 2L & length(Buckets) != 1L) {
    if(length(IDcols) != 0L) {
      data.table::setcolorder(TestData, c(1L:counter, (counter + length(IDcols) + 1L):(counter + length(IDcols) + 2L + length(Buckets) + 1L), which(names(TestData) %in% c(setdiff(names(TestData), names(TestData)[c(1L:counter, (counter + length(IDcols) + 1L):(counter + length(IDcols) + 2L + length(Buckets) + 1L))])))))
      data.table::setcolorder(TestData, c(1L:(counter + 1L), (counter + 1L + 2L):(counter + 1L + 2L + counter), which(names(TestData) %in% setdiff(names(TestData), names(TestData)[c(1L:(counter + 1L), (counter + 1L + 2L):(counter + 1L + 2L + counter))]))))
    } else {
      data.table::setcolorder(TestData, c(1L:(counter + 1L), (counter + 3L):(3L + 2 * counter), (counter + 2L), which(!names(TestData) %in% names(TestData)[c(1L:(counter + 1L), (counter + 3L):(3L + 2 * counter), (counter + 2L))])))
    }
  } else {
    if(length(IDcols) != 0L) {
      data.table::setcolorder(TestData, c(1L:2L, (3L + length(IDcols)):((3L + length(IDcols)) + 1L),3L:(2L + length(IDcols)),(((3L + length(IDcols)) + 2L):ncol(TestData))))
      data.table::setcolorder(TestData, c(5L:ncol(TestData), 1L:4L))
    }
  }
  
  # Final Combination of Predictions----
  if(counter > 2L | (counter == 2L & length(Buckets) != 1L)) {
    for(i in seq_len(length(Buckets) + 1L)) {
      if(i == 1L) {
        TestData[, FinalPredictedValue := TestData[[i]] * TestData[[i + (length(Buckets) + 1L)]]]
      } else {
        TestData[, FinalPredictedValue := FinalPredictedValue + TestData[[i]] * TestData[[i + (length(Buckets) + 1L)]]]
      }
    }
  } else {
    TestData[, FinalPredictedValue := TestData[[1L]] * TestData[[3L]] + TestData[[2L]] * TestData[[4L]]]
  }
  
  # Return preds----
  return(data.table::setcolorder(TestData, c(ncol(TestData), 1L:(ncol(TestData) - 1L))))
}
