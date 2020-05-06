#' AutoCatBoostScoring is an automated scoring function that compliments the AutoCatBoost model training functions.
#'
#' AutoCatBoostScoring is an automated scoring function that compliments the AutoCatBoost model training functions. This function requires you to supply features for scoring. It will run ModelDataPrep() to prepare your features for catboost data conversion and scoring.
#'
#' @author Adrian Antico
#' @family Automated Model Scoring
#' @param TargetType Set this value to "regression", "classification", or "multiclass" to score models built using AutoCatBoostRegression(), AutoCatBoostClassify() or AutoCatBoostMultiClass().
#' @param ScoringData This is your data.table of features for scoring. Can be a single row or batch.
#' @param FeatureColumnNames Supply either column names or column numbers used in the AutoCatBoostRegression() function
#' @param IDcols Supply ID column numbers for any metadata you want returned with your predicted values
#' @param ModelObject Supply the model object directly for scoring instead of loading it from file. If you supply this, ModelID and ModelPath will be ignored.
#' @param ModelPath Supply your path file used in the AutoCatBoost__() function
#' @param ModelID Supply the model ID used in the AutoCatBoost__() function
#' @param ReturnFeatures Set to TRUE to return your features with the predicted values.
#' @param MultiClassTargetLevels For use with AutoCatBoostMultiClass(). If you saved model objects then this scoring function will locate the target levels file. If you did not save model objects, you can supply the target levels returned from AutoCatBoostMultiClass().
#' @param TransformNumeric Set to TRUE if you have features that were transformed automatically from an Auto__Regression() model AND you haven't already transformed them.
#' @param BackTransNumeric Set to TRUE to generate back-transformed predicted values. Also, if you return features, those will also be back-transformed.
#' @param TargetColumnName Input your target column name used in training if you are utilizing the transformation service
#' @param TransformationObject Set to NULL if you didn't use transformations or if you want the function to pull from the file output from the Auto__Regression() function. You can also supply the transformation data.table object with the transformation details versus having it pulled from file.
#' @param TransID Set to the ID used for saving the transformation data.table object or set it to the ModelID if you are pulling from file from a build with Auto__Regression().
#' @param TransPath Set the path file to the folder where your transformation data.table detail object is stored. If you used the Auto__Regression() to build, set it to the same path as ModelPath.
#' @param MDP_Impute Set to TRUE if you did so for modeling and didn't do so before supplying ScoringData in this function
#' @param MDP_CharToFactor Set to TRUE to turn your character columns to factors if you didn't do so to your ScoringData that you are supplying to this function
#' @param MDP_RemoveDates Set to TRUE if you have date of timestamp columns in your ScoringData
#' @param MDP_MissFactor If you set MDP_Impute to TRUE, supply the character values to replace missing values with
#' @param MDP_MissNum If you set MDP_Impute to TRUE, supply a numeric value to replace missing values with
#' @param RemoveModel Set to TRUE if you want the model removed immediately after scoring
#' @examples
#' \donttest{
#' Preds <- AutoCatBoostScoring(TargetType = "regression",
#'                              ScoringData = data,
#'                              FeatureColumnNames = 2:12,
#'                              IDcols = NULL,
#'                              ModelObject = NULL,
#'                              ModelPath = "home",
#'                              ModelID = "ModelTest",
#'                              ReturnFeatures = TRUE,
#'                              MultiClassTargetLevels = NULL,
#'                              TransformNumeric = FALSE,
#'                              BackTransNumeric = FALSE,
#'                              TargetColumnName = NULL,
#'                              TransformationObject = NULL,
#'                              TransID = NULL,
#'                              TransPath = NULL,
#'                              MDP_Impute = TRUE,
#'                              MDP_CharToFactor = TRUE,
#'                              MDP_RemoveDates = TRUE,
#'                              MDP_MissFactor = "0",
#'                              MDP_MissNum = -1,
#'                              RemoveModel = FALSE)
#' }
#' @return A data.table of predicted values with the option to return model features as well.
#' @export
AutoCatBoostScoring <- function(TargetType = NULL,
                                ScoringData = NULL,
                                FeatureColumnNames = NULL,
                                IDcols = NULL,
                                ModelObject = NULL,
                                ModelPath = NULL,
                                ModelID = NULL,
                                ReturnFeatures = TRUE,
                                MultiClassTargetLevels = NULL,
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
                                MDP_MissNum = -1,
                                RemoveModel = FALSE) {
  # Load catboost----
  loadNamespace(package = "catboost")
  
  # Turn on full speed ahead----
  data.table::setDTthreads(percent = 100)
  
  # Check arguments----
  if (is.null(ScoringData)) {
    stop("ScoringData cannot be NULL")
  }
  if (!data.table::is.data.table(ScoringData)) {
    ScoringData <- data.table::as.data.table(ScoringData)
  }
  if (!is.logical(MDP_Impute)) {
    stop("MDP_Impute (ModelDataPrep) should be TRUE or FALSE")
  }
  if (!is.logical(MDP_CharToFactor)) {
    stop("MDP_CharToFactor (ModelDataPrep) should be TRUE or FALSE")
  }
  if (!is.logical(MDP_RemoveDates)) {
    stop("MDP_RemoveDates (ModelDataPrep) should be TRUE or FALSE")
  }
  if (!is.character(MDP_MissFactor) & !is.factor(MDP_MissFactor)) {
    stop("MDP_MissFactor should be a character or factor value")
  }
  if (!is.numeric(MDP_MissNum)) {
    stop("MDP_MissNum should be a numeric or integer value")
  }
  
  # Pull in ColNames----
  if (is.null(FeatureColumnNames)) {
    FeatureColumnNames <- data.table::fread(file = paste0(ModelID, "_ColNames.csv"))
  }
  
  # Pull In Transformation Object----
  if (is.null(TransformationObject)) {
    if (TransformNumeric == TRUE | BackTransNumeric == TRUE) {
      if(is.null(TargetColumnName)) {
        return("TargetColumnName needs to be supplied")
      }
      TransformationObject <- data.table::fread(paste0(TransPath,"/",TransID, "_transformation.csv"))
    }
  }
  
  # ModelDataPrep Check----
  ScoringData <- ModelDataPrep(
    data = ScoringData,
    Impute = MDP_Impute,
    CharToFactor = MDP_CharToFactor,
    RemoveDates = MDP_RemoveDates,
    MissFactor = MDP_MissFactor,
    MissNum = MDP_MissNum)
  
  # Identify column numbers for factor variables----
  CatFeatures <- sort(c(as.numeric(which(sapply(ScoringData, is.factor))),
                        as.numeric(which(sapply(ScoringData, is.character)))))
  
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
      Path = TransPath)
  }
  
  # Convert FeatureColumnNames to Character Names----
  if (data.table::is.data.table(FeatureColumnNames)) {
    FeatureColumnNames <- FeatureColumnNames[[1]]
  } else if (is.numeric(FeatureColumnNames)) {
    FeatureColumnNames <- names(ScoringData)[FeatureColumnNames]
  }
  
  # Remove Target from FeatureColumnNames----
  if (TransformNumeric == TRUE | BackTransNumeric == TRUE) {
    if(!is.null(TargetColumnName)) {
      if (TargetColumnName %chin% FeatureColumnNames) {
        FeatureColumnNames <- FeatureColumnNames[!(TargetColumnName == FeatureColumnNames)]
      }    
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
    ScoringPool <- catboost::catboost.load_pool(ScoringData, cat_features = CatFeatures)
  } else {
    ScoringPool <- catboost::catboost.load_pool(ScoringData)
  }
  
  # Load model----
  if (!is.null(ModelObject)) {
    model <- ModelObject
  } else {
    model <- tryCatch({catboost::catboost.load_model(paste0(ModelPath, "/", ModelID))},
                      error = function(x) return("Model not found in ModelPath"))
  }
  
  # Score model----
  if (tolower(TargetType) == "regression") {
    predict <- data.table::as.data.table(
      catboost::catboost.predict(
        model = model,
        pool = ScoringPool,
        prediction_type = "RawFormulaVal",
        thread_count = -1))
  } else if (tolower(TargetType) == "classification") {
    predict <- data.table::as.data.table(
      catboost::catboost.predict(
        model = model,
        pool = ScoringPool,
        prediction_type = "Probability",
        thread_count = -1))
  } else if (tolower(TargetType) == "multiclass") {
    predict <- data.table::as.data.table(cbind(
      1 + catboost::catboost.predict(
        model = model,
        pool = ScoringPool,
        prediction_type = "Class"),
      catboost::catboost.predict(
        model = model,
        pool = ScoringPool,
        prediction_type = "Probability")))
  }
  
  # Remove Model----
  if(RemoveModel) {
    rm(model)
  }
  
  # Change Output Predictions Column Name----
  if (tolower(TargetType) != "multiclass") {
    data.table::setnames(predict, "V1", "Predictions")
  } else if (tolower(TargetType) == "multiclass") {
    if(!is.null(MultiClassTargetLevels)) {
      TargetLevels <- MultiClassTargetLevels
    } else {
      TargetLevels <- data.table::fread(paste0(ModelPath, "/", ModelID, "_TargetLevels.csv"))      
    }
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
      all = FALSE)
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
    
    # Append record for Predicted Column----
    data.table::set(
      grid_trans_results,
      i = which(grid_trans_results[["ColumnName"]] == eval(TargetColumnName)),
      j = "ColumnName",
      value = "Predictions")
    grid_trans_results <- grid_trans_results[ColumnName != eval(TargetColumnName)]
    
    # Run Back-Transform----
    predict <- AutoTransformationScore(
      ScoringData = predict,
      Type = "Inverse",
      FinalResults = grid_trans_results,
      TransID = NULL,
      Path = NULL)
  }
  
  # Garbage Collection----
  gc()
  
  # Return data----
  return(predict)
}
