#' AutoHurdleScoring()
#' 
#' 
#' @author Adrian Antico
#' @family Automated Model Scoring
#' @param TestData scoring data.table
#' @param ModelClass Name of model type. "catboost" is currently the only available option
#' @param ArgList Output from the hurdle model
#' @param ModelList Output from the hurdle model
#' @export
AutoHurdleScoring <- function(TestData = NULL,
                              ModelClass = "catboost",
                              ArgList = Output$ArgsList,
                              ModelList = Output$ModelList) {
  
  # Define Buckets----
  Buckets <- ArgList$Buckets
  
  # Score Classification Model----
  if(length(Buckets) == 1L) TargetType <- "Classification" else TargetType <- "Multiclass"
  
  # Store classifier model----
  if(!is.null(ModelList)) ClassModel <- ModelList[[1L]] else if(!is.null(ArgList$ModelID)) ClassModel <- catboost::catboost.load_model(model_path = file.path(normalizePath(ArgList$Paths),ArgList$ModelID)) else return("Need to supply a ModelList or ModelID")
  
  # Store FeatureNames----
  FeatureNames <- ArgList$FeatureColNames
  
  # Classification Model Scoring---- 
  if(tolower(ModelClass) == "catboost") {
    TestData <- AutoCatBoostScoring(
      TargetType = TargetType,
      ScoringData = TestData,
      FeatureColumnNames = FeatureNames,
      IDcols = ArgList$IDcols,
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
  if(TargetType == "Classification") {
    data.table::setnames(TestData, "p1", "Predictions_C1")
    TestData[, Predictions_C0 := 1 - Predictions_C1]
    data.table::setcolorder(TestData, c(ncol(TestData), 1L, 2L:(ncol(TestData) - 1L)))
  }
  
  # Remove Target From IDcols----
  IDcols <- ArgList$IDcols[!(ArgList$IDcols %chin% ArgList$TargetColumnName)]
  
  # Change Name of Predicted MultiClass Column----
  if(length(Buckets) != 1L) data.table::setnames(TestData, "Predictions", "Predictions_MultiClass")
  
  # Begin regression model building----
  counter <- max(rev(seq_len(length(Buckets) + 1L))) + 1L
  Degenerate <- 0L
  for(bucket in rev(seq_len(length(Buckets) + 1L))) {
    
    # Check for degenerate bucket----
    if(!any(bucket %in% c(ArgList$constant))) {
      
      # Check for constant value bucket----
      if(!any(bucket %in% c(Output$ArgsList$constant))) {
        
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
        
        # Catboost Model Scroring----
        if(tolower(ModelClass) == "catboost") {
          TestData <- AutoCatBoostScoring(
            TargetType = "regression",
            ScoringData = TestData,
            FeatureColumnNames = FeatureNames,
            IDcols = IDcols,
            ModelObject = ModelList[[bucket]],
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
            TargetColumnName = if(is.null(TransformationResults)) FALSE else TRUE,
            TransformNumeric = if(is.null(TransformationResults)) FALSE else TRUE,
            BackTransNumeric = if(is.null(TransformationResults)) FALSE else TRUE,
            TransformationObject = TransformationObject,
            TransID = NULL,
            TransPath = NULL,
            MDP_Impute = TRUE,
            MDP_CharToFactor = TRUE,
            MDP_RemoveDates = TRUE,
            MDP_MissFactor = "0",
            MDP_MissNum = -1)
        }
        
        # Change prediction name to prevent duplicates----
        if(bucket == max(seq_len(length(Buckets) + 1L))) Val <- paste0("Predictions_", bucket - 1L, "+") else Val <- paste0("Predictions_", bucket)
        data.table::setnames(TestData, "Predictions", Val)
        
      } else {
        
        # Use single value for predictions in the case of zero variance----
        if (bucket == max(seq_len(length(Buckets) + 1L))) {
          data.table::set(TestData, j = paste0("Predictions", Buckets[bucket - 1L], "+"), value = Buckets[bucket])
          data.table::setcolorder(TestData, c(ncol(TestData), 1L:(ncol(TestData)-1L)))
        } else {
          data.table::set(TestData, j = paste0("Predictions", Buckets[bucket]), value = Buckets[bucket])
          data.table::setcolorder(TestData, c(ncol(TestData), 1L:(ncol(TestData)-1L)))
        }
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
    if(length(IDcols) != 0L) {
      data.table::setcolorder(TestData, c(2L:(1L+length(IDcols)),1L,(2L+length(IDcols)):ncol(TestData)))
      data.table::setcolorder(TestData, c(1L:length(IDcols),(5L+length(IDcols)):ncol(TestData),(1L+length(IDcols)):(1L+length(IDcols)+3L)))
    } else {
      data.table::setcolorder(TestData, c(5L:ncol(TestData), 1L:4L))
    }
  } else if(counter == 2L & length(Buckets) != 1L) {
    if(length(IDcols) != 0L) {
      data.table::setcolorder(TestData, c(2L:(1L+length(IDcols)),1L,(2+length(IDcols)):ncol(TestData)))
      data.table::setcolorder(TestData,c(1L:length(IDcols),length(IDcols)+1L+length(IDcols),(length(IDcols)+5L+length(IDcols)):(ncol(TestData)-1L),(4L+length(IDcols)):(6L+length(IDcols)),ncol(TestData),(1L+length(IDcols)):(2L+length(IDcols))))
    } else {
      data.table::setcolorder(TestData, c(4L:ncol(TestData), 1L:3L))
    }
  } else {
    if(length(IDcols) != 0L) {
      data.table::setcolorder(TestData, c(1L:2L, (3L+length(IDcols)):((3L+length(IDcols))+1L),3L:(2L+length(IDcols)),(((3L+length(IDcols))+2L):ncol(TestData))))
      data.table::setcolorder(TestData, c(5L:ncol(TestData),1L:4L))
    } else {
      data.table::setcolorder(TestData, c(5L:ncol(TestData), 1L:4L))
    }
  }
  
  # Final Combination of Predictions----
  # Logic: 1 Buckets --> 4 columns of preds
  #        2 Buckets --> 6 columns of preds
  #        3 Buckets --> 8 columns of preds
  # Secondary logic: for i == 1, need to create the final column first
  #                  for i > 1, need to take the final column and add the product of the next preds
  Cols <- ncol(TestData)
  if(counter > 2L | (counter == 2L & length(Buckets) != 1L)) {
    for (i in seq_len(length(Buckets)+1)) {
      if (i == 1L) {
        TestData[, UpdatedPrediction := TestData[[(Cols - ((length(Buckets) + 1L) * 2L - i))]] * TestData[[(Cols - ((length(Buckets) + 1L) - i))]]]
      } else {
        TestData[, UpdatedPrediction := TestData[["UpdatedPrediction"]] + TestData[[(Cols - ((length(Buckets) + 1L) * 2L - i))]] * TestData[[(Cols - ((length(Buckets) + 1L) - i))]]]
      }
    }  
  } else if(counter == 2L & length(Buckets) == 1L) {
    TestData[, UpdatedPrediction := TestData[[ncol(TestData)]] * TestData[[(ncol(TestData)-2L)]] + TestData[[ncol(TestData)-1L]] * (TestData[[(ncol(TestData)-3L)]])]
  } else {
    TestData[, UpdatedPrediction := TestData[[ncol(TestData)]] * TestData[[(ncol(TestData)-2L)]] + TestData[[(ncol(TestData)-1L)]] * TestData[[(ncol(TestData)-3L)]]]
  }
  
  # Return preds----
  return(TestData)
}
