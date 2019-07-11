AutoCatBoostScoring <- function(TargetType = NULL,
                                ScoringData = NULL,
                                FeatureColumnNames = NULL,
                                IDcols = NULL,
                                ModelObject = NULL,
                                ModelPath = NULL,
                                ModelID = NULL,
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
                                MDP_MissNum = -1) {
  # Load catboost----
  loadNamespace(package = "catboost")
  
  # Check arguments----
  if (is.null(ScoringData)) {
    warning("ScoringData cannot be NULL")
  }
  if (!data.table::is.data.table(ScoringData)) {
    ScoringData <- data.table::as.data.table(ScoringData)
  }
  if (!is.logical(MDP_Impute)) {
    warning("MDP_Impute (ModelDataPrep) should be TRUE or FALSE")
  }
  if (!is.logical(MDP_CharToFactor)) {
    warning("MDP_CharToFactor (ModelDataPrep) should be TRUE or FALSE")
  }
  if (!is.logical(MDP_RemoveDates)) {
    warning("MDP_RemoveDates (ModelDataPrep) should be TRUE or FALSE")
  }
  if (!is.character(MDP_MissFactor) & !is.factor(MDP_MissFactor)) {
    warning("MDP_MissFactor should be a character or factor value")
  }
  if (!is.numeric(MDP_MissNum)) {
    warning("MDP_MissNum should be a numeric or integer value")
  }
  
  # Pull in ColNames----
  if (is.null(FeatureColumnNames)) {
    FeatureColumnNames <-
      data.table::fread(file = paste0(ModelID, "_ColNames.csv"))
  }
  
  # Pull In Transformation Object----
  if (is.null(TransformationObject)) {
    if (TransformNumeric == TRUE | BackTransNumeric == TRUE) {
      if(is.null(TargetColumnName)) {
        return("TargetColumnName needs to be supplied")
      }
      TransformationObject <-
        data.table::fread(paste0(TransPath,"/",TransID, "_transformation.csv"))
    }
  }
  
  # ModelDataPrep Check----
  ScoringData <- ModelDataPrep(
    data = ScoringData,
    Impute = MDP_Impute,
    CharToFactor = MDP_CharToFactor,
    RemoveDates = MDP_RemoveDates,
    MissFactor = MDP_MissFactor,
    MissNum = MDP_MissNum
  )
  
  # Identify column numbers for factor variables----
  CatFeatures <-
    sort(c(as.numeric(which(
      sapply(ScoringData, is.factor)
    )),
    as.numeric(which(
      sapply(ScoringData, is.character)
    ))))
  
  # Convert CatFeatures to 1-indexed----
  if (!is.null(CatFeatures)) {
    for (i in seq_len(length(CatFeatures))) {
      CatFeatures[i] <- CatFeatures[i] - 1
    }
  }
  
  # IDcols conversion----
  if (is.numeric(IDcols) | is.integer(IDcols)) {
    IDcols <- names(data)[IDcols]
  }
  
  # Apply Transform Numeric Variables----
  if (TransformNumeric) {
    tempTrans <- data.table::copy(TransformationObject)
    tempTrans <- tempTrans[ColumnName != eval(TargetColumnName)]
    ScoringData <- AutoTransformationScore(
      ScoringData = ScoringData,
      FinalResults = tempTrans,
      Type = "Apply",
      TransID = TransID,
      Path = TransPath
    )
  }
  
  # Convert FeatureColumnNames to Character Names----
  if (data.table::is.data.table(FeatureColumnNames)) {
    FeatureColumnNames <- FeatureColumnNames[[1]]
  } else if (is.numeric(FeatureColumnNames)) {
    FeatureColumnNames <- names(ScoringData)[FeatureColumnNames]
  }
  
  # Remove Target from FeatureColumnNames----
  if(is.null(TargetColumnName)) {
    if (TargetColumnName %chin% FeatureColumnNames) {
      FeatureColumnNames <-
        FeatureColumnNames[!(TargetColumnName == FeatureColumnNames)]
    }    
  } 
  
  # Subset Columns Needed----
  keep1 <- c(FeatureColumnNames)
  if (!is.null(IDcols)) {
    keep <- c(IDcols, FeatureColumnNames)
  } else {
    keep <- c(FeatureColumnNames)
  }
  ScoringData <- ScoringData[, ..keep]
  if (!is.null(IDcols)) {
    ScoringMerge <- data.table::copy(ScoringData)
    keep <- c(keep1)
    ScoringData <- ScoringData[, ..keep]
  } else {
    ScoringMerge <- data.table::copy(ScoringData)
  }
  
  # Initialize Catboost Data Conversion----
  if (!is.null(CatFeatures)) {
    ScoringPool <-
      catboost::catboost.load_pool(ScoringData, cat_features = CatFeatures)
  } else {
    ScoringPool <-
      catboost::catboost.load_pool(ScoringData)
  }
  
  # Load model----
  if (!is.null(ModelObject)) {
    model <- ModelObject
  } else {
    model <- tryCatch({
      catboost::catboost.load_model(paste0(ModelPath, "/", ModelID))
    },
    error = function(x)
      return("Model not found in ModelPath"))
  }
  
  # Score model----
  if (tolower(TargetType) == "regression") {
    predict <- data.table::as.data.table(
      catboost::catboost.predict(
        model = model,
        pool = ScoringPool,
        prediction_type = "RawFormulaVal",
        thread_count = -1
      )
    )
  } else if (tolower(TargetType) == "classification") {
    predict <- data.table::as.data.table(
      catboost::catboost.predict(
        model = model,
        pool = ScoringPool,
        prediction_type = "Probability",
        thread_count = -1
      )
    )
  } else if (tolower(TargetType) == "multiclass") {
    predict <- data.table::as.data.table(cbind(
      1 + catboost::catboost.predict(
        model = model,
        pool = ScoringPool,
        prediction_type = "Class"
      ),
      catboost::catboost.predict(
        model = model,
        pool = ScoringPool,
        prediction_type = "Probability"
      )
    ))
  }
  
  # Change Output Predictions Column Name----
  if (tolower(TargetType) != "multiclass") {
    data.table::setnames(predict, "V1", "Predictions")
  } else if (tolower(TargetType) == "multiclass") {
    TargetLevels <-
      data.table::fread(paste0(ModelPath, "/", ModelID, "_TargetLevels.csv"))
    k <- 1
    for (name in as.character(TargetLevels[[1]])) {
      k <- k + 1
      data.table::setnames(predict, paste0("V", k), name)
    }
    data.table::setnames(predict, "V1", "Predictions")
    predict <- merge(
      predict,
      TargetLevels,
      by.x = "Predictions",
      by.y = "NewLevels",
      all = FALSE
    )
    predict[, Predictions := OriginalLevels][, OriginalLevels := NULL]
  }
  
  # Merge features back on----
  if (ReturnFeatures) {
    predict <- cbind(predict, ScoringMerge)
  }
  
  # Back Transform Numeric Variables----
  if (BackTransNumeric) {
    # Make copy of TransformationResults----
    grid_trans_results <- data.table::copy(TransformationObject)
    grid_trans_results <-
      grid_trans_results[ColumnName != eval(TargetColumnName)]
    
    # Append record for Predicted Column----
    data.table::set(
      grid_trans_results,
      i = which(grid_trans_results[["ColumnName"]] == eval(TargetColumnName)),
      j = "ColumnName",
      value = "Predictions"
    )
    grid_trans_results <-
      grid_trans_results[ColumnName != eval(TargetColumnName)]
    
    # Run Back-Transform----
    predict <- AutoTransformationScore(
      ScoringData = predict,
      Type = "Inverse",
      FinalResults = grid_trans_results,
      TransID = NULL,
      Path = NULL
    )
  }
  
  # Garbage Collection----
  gc()
  
  # Return data----
  return(predict)
}
