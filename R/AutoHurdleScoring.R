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
  # IDcols <- IDcols[!(IDcols %chin% ArgList$TargetColumnName)]
  
  # Change Name of Predicted MultiClass Column----
  if(length(Buckets) != 1L) data.table::setnames(TestData, "Predictions", "Predictions_MultiClass")
  
  # Begin regression model building----
  counter <- max(rev(seq_len(length(Buckets) + 1L))) + 1L
  Degenerate <- 0L
  for(bucket in rev(seq_len(length(Buckets) + 1L))) {
    
    # Update IDcols----
    IDcolsModified <- c(IDcols, setdiff(names(TestData), ColumnNames))
    
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
          IDcols = IDcolsModified,
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
    if(length(IDcolsReorderReorder) != 0L) {
      if(Degenerate == 0L) {
        data.table::setcolorder(TestData, c(2L:(1L + length(IDcolsReorderReorder)), 1L, (2L + length(IDcolsReorder)):ncol(TestData)))
        data.table::setcolorder(TestData, c(1L:length(IDcolsReorder),(length(IDcolsReorder) + counter + 1L),(length(IDcolsReorder) + counter + 1L + counter +1L):ncol(TestData), (length(IDcolsReorder) + 1L):(length(IDcolsReorder) + counter),(length(IDcolsReorder) + counter + 2L):(length(IDcolsReorder)+counter+1L+counter)))
      } else {
        data.table::setcolorder(TestData, c(3L:(2L + length(IDcolsReorder)), 1L:2L, (3L + length(IDcolsReorder)):ncol(TestData)))
        data.table::setcolorder(TestData, c(1L:length(IDcolsReorder),(length(IDcolsReorder) + counter + 1L + Degenerate),(length(IDcolsReorder) + counter + 3L + counter + Degenerate):ncol(TestData),(length(IDcolsReorder) + 1L):(length(IDcolsReorder) + counter + Degenerate),(length(IDcolsReorder) + counter + 2L + Degenerate):(length(IDcolsReorder)+counter+counter+Degenerate+2L)))
      }
    } else {
      data.table::setcolorder(TestData, c(1L:(counter+Degenerate),(2L+counter+Degenerate):(1L+2L*(counter+Degenerate)),(1L+counter+Degenerate),(2L+2L*(counter+Degenerate)):ncol(TestData)))
      data.table::setcolorder(TestData, c((2L*(counter+Degenerate)+1L):ncol(TestData),1L:(2L*(counter+Degenerate))))
    }
  } else if(counter == 2L & length(Buckets) == 1L) {
    if(length(IDcolsReorder) != 0L) data.table::setcolorder(TestData, c(1L, 2L, (3L + length(IDcolsReorder)):ncol(TestData), 3L:(2L + length(IDcolsReorder))))
  } else if(counter == 2L & length(Buckets) != 1L) {
    if(length(IDcolsReorder) != 0L) {
      data.table::setcolorder(TestData, c(2L:(1L+length(IDcolsReorder)),1L,(2+length(IDcolsReorder)):ncol(TestData)))
      data.table::setcolorder(TestData,c(1L:length(IDcolsReorder),length(IDcolsReorder) + 1L + length(IDcolsReorder),(length(IDcolsReorder)+5L+length(IDcolsReorder)):(ncol(TestData)-1L),(4L+length(IDcolsReorder)):(6L+length(IDcolsReorder)),ncol(TestData),(1L+length(IDcolsReorder)):(2L+length(IDcolsReorder))))
    } else {
      data.table::setcolorder(TestData, c(4L:ncol(TestData), 1L:3L))
    }
  } else {
    if(length(IDcolsReorder) != 0L) {
      data.table::setcolorder(TestData, c(1L:2L, (3L + length(IDcolsReorder)):((3L + length(IDcolsReorder)) + 1L),3L:(2L + length(IDcolsReorder)),(((3L + length(IDcolsReorder)) + 2L):ncol(TestData))))
      data.table::setcolorder(TestData, c(5L:ncol(TestData), 1L:4L))
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
  } else {
    TestData[, UpdatedPrediction := TestData[[1L]] * TestData[[3L]] + TestData[[2L]] * (TestData[[4L]])]
  }
  
  # Return preds----
  return(TestData)
}
