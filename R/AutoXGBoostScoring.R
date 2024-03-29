# AutoQuant is a package for quickly creating high quality visualizations under a common and easy api.
# Copyright (C) <year>  <name of author>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

#' @title AutoXGBoostScoring
#'
#' @description AutoXGBoostScoring is an automated scoring function that compliments the AutoXGBoost model training functions. This function requires you to supply features for scoring. It will run ModelDataPrep() and the DummifyDT() function to prepare your features for xgboost data conversion and scoring.
#'
#' @author Adrian Antico
#' @family Automated Model Scoring
#'
#' @param TargetType Set this value to "regression", "classification", or "multiclass" to score models built using AutoXGBoostRegression(), AutoXGBoostClassify() or AutoXGBoostMultiClass()
#' @param ScoringData This is your data.table of features for scoring. Can be a single row or batch.
#' @param ReturnShapValues Set to TRUE to return shap values for the predicted values
#' @param FeatureColumnNames Supply either column names or column numbers used in the AutoXGBoost__() function
#' @param IDcols Supply ID column numbers for any metadata you want returned with your predicted values
#' @param EncodingMethod Choose from 'binary', 'm_estimator', 'credibility', 'woe', 'target_encoding', 'poly_encode', 'backward_difference', 'helmert'
#' @param FactorLevelsList Supply the factor variables' list from DummifyDT()
#' @param TargetLevels Supply the target levels output from AutoXGBoostMultiClass() or the scoring function will go looking for it in the file path you supply.
#' @param ModelObject Supply a model for scoring, otherwise it will have to search for it in the file path you specify
#' @param ModelPath Supply your path file used in the AutoXGBoost__() function
#' @param ModelID Supply the model ID used in the AutoXGBoost__() function
#' @param ReturnFeatures Set to TRUE to return your features with the predicted values.
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
#' @examples
#' \dontrun{
#' Preds <- AutoXGBoostScoring(
#'   TargetType = "regression",
#'   ScoringData = data,
#'   ReturnShapValues = FALSE,
#'   FeatureColumnNames = 2:12,
#'   IDcols = NULL,
#'   EncodingMethod = "binary",
#'   FactorLevelsList = NULL,
#'   TargetLevels = NULL,
#'   ModelObject = NULL,
#'   ModelPath = "home",
#'   ModelID = "ModelTest",
#'   ReturnFeatures = TRUE,
#'   TransformNumeric = FALSE,
#'   BackTransNumeric = FALSE,
#'   TargetColumnName = NULL,
#'   TransformationObject = NULL,
#'   TransID = NULL,
#'   TransPath = NULL,
#'   MDP_Impute = TRUE,
#'   MDP_CharToFactor = TRUE,
#'   MDP_RemoveDates = TRUE,
#'   MDP_MissFactor = "0",
#'   MDP_MissNum = -1)
#' }
#' @return A data.table of predicted values with the option to return model features as well.
#' @export
AutoXGBoostScoring <- function(TargetType = NULL,
                               ScoringData = NULL,
                               ReturnShapValues = FALSE,
                               FeatureColumnNames = NULL,
                               IDcols = NULL,
                               EncodingMethod = "binary",
                               FactorLevelsList = NULL,
                               TargetLevels = NULL,
                               OneHot = FALSE,
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
                               MDP_MissNum = -1,
                               Debug = FALSE) {

  if(Debug) print("XGBoost Scoring 1")

  # Check arguments ----
  if(is.null(ScoringData)) stop("ScoringData cannot be NULL")
  if(is.null(FeatureColumnNames)) stop("FeatureColumnNames cannot be NULL")
  if(!data.table::is.data.table(ScoringData)) data.table::setDT(ScoringData)
  if(!is.logical(MDP_Impute)) stop("MDP_Impute (ModelDataPrep) should be TRUE or FALSE")
  if(!is.logical(MDP_CharToFactor)) stop("MDP_CharToFactor (ModelDataPrep) should be TRUE or FALSE")
  if(!is.logical(MDP_RemoveDates)) stop("MDP_RemoveDates (ModelDataPrep) should be TRUE or FALSE")
  if(!is.character(MDP_MissFactor) && !is.factor(MDP_MissFactor)) stop("MDP_MissFactor should be a character or factor value")
  if(!is.numeric(MDP_MissNum)) stop("MDP_MissNum should be a numeric or integer value")

  # IDcols conversion ----
  if(Debug) print("XGBoost Scoring 2")
  if(is.numeric(IDcols)) IDcols <- names(data)[IDcols]
  if('ID_Factorizer' %in% names(ScoringData)) data.table::set(ScoringData, j = 'ID_Factorizer', value = NULL)

  # Apply Transform Numeric Variables----
  if(Debug) print("XGBoost Scoring 3")
  if(TransformNumeric) {
    if(!is.null(TransformationObject)) {
      tempTrans <- data.table::copy(TransformationObject)
      tempTrans <- tempTrans[ColumnName != eval(TargetColumnName)]
      ScoringData <- Rodeo::AutoTransformationScore(ScoringData = ScoringData, FinalResults = tempTrans, Type = "Apply", TransID = TransID, Path = NULL)
    } else {
      ScoringData <- Rodeo::AutoTransformationScore(ScoringData = ScoringData, FinalResults = tempTrans, Type = "Apply", TransID = TransID, Path = TransPath)
    }
  }

  # Subset Columns Needed----
  if(Debug) print("XGBoost Scoring 4")
  if(is.numeric(FeatureColumnNames) || is.integer(FeatureColumnNames)) {
    keep1 <- names(ScoringData)[c(FeatureColumnNames)]
    if(!is.null(IDcols)) keep <- c(IDcols, keep1) else keep <- c(keep1)
    ScoringData <- ScoringData[, ..keep]
  } else {
    keep1 <- c(FeatureColumnNames)
    if(!is.null(IDcols)) keep <- c(IDcols, FeatureColumnNames) else keep <- c(FeatureColumnNames)
    ScoringData <- ScoringData[, ..keep]
  }
  if(!is.null(IDcols)) {
    ScoringMerge <- data.table::copy(ScoringData)
    keep <- c(keep1)
    ScoringData <- ScoringData[, ..keep]
  } else {
    ScoringMerge <- data.table::copy(ScoringData)
  }

  # DummifyDT categorical columns ----
  if(Debug) print("XGBoost Scoring 5")
  if(!is.null(EncodingMethod) && EncodingMethod == "binary") {
    if(!is.null(FactorLevelsList)) {
      ScoringData <- Rodeo::DummifyDT(data=ScoringData, cols=names(FactorLevelsList)[-length(names(FactorLevelsList))], KeepFactorCols=FALSE, OneHot=FALSE, SaveFactorLevels=FALSE, SavePath=ModelPath, ImportFactorLevels=FALSE, FactorLevelsList=FactorLevelsList, ReturnFactorLevels=FALSE, ClustScore=FALSE, GroupVar=TRUE)
    } else {
      CatFeatures <- sort(c(as.numeric(which(sapply(ScoringData, is.factor))), as.numeric(which(sapply(ScoringData, is.character)))))
      CatFeatures <- names(ScoringData)[CatFeatures]
      if(length(IDcols) > 0L) CatFeatures <- CatFeatures[!CatFeatures %in% IDcols]
      if(!identical(CatFeatures, character(0)) && !is.null(CatFeatures)) {
        ScoringData <- Rodeo::DummifyDT(data=ScoringData, cols=CatFeatures, KeepFactorCols=FALSE, OneHot=FALSE, SaveFactorLevels=FALSE, SavePath=ModelPath, ImportFactorLevels=TRUE, ReturnFactorLevels=FALSE, ClustScore=FALSE, GroupVar=TRUE)
      }
    }
  } else if(length(EncodingMethod) > 0L && length(FactorLevelsList$EncodingMethod) > 0L) {

    # Encode
    x <- FactorLevelsList$EncodingMethod
    if(x == 'target_encoding') {
      x <- 'TargetEncode'
    } else if(x == 'credibility') {
      x <- 'Crediblity'
    } else if(x == 'woe') {
      x <- "WOE"
    } else if(x == 'poly_encode') {
      x <- 'PolyEncode'
    } else if(tolower(x) == 'meow') {
      x <- 'MEOW'
    }
    y <- names(ScoringData)[which(names(ScoringData) %like% paste0('_', x))]
    if(length(y) != 0) data.table::set(ScoringData, j = c(names(ScoringData)[which(names(ScoringData) %like% paste0('_', x))]), value = NULL)
    xx <- names(data.table::copy(ScoringData))
    Output <- Rodeo::EncodeCharacterVariables(
      RunMode = 'score',
      ModelType = TargetType,
      TrainData = ScoringData,
      ValidationData = NULL,
      TestData = NULL,
      TargetVariableName = NULL,
      CategoricalVariableNames = names(FactorLevelsList)[-length(FactorLevelsList)],
      EncodeMethod = FactorLevelsList$EncodingMethod,
      KeepCategoricalVariables = TRUE,
      ReturnMetaData = TRUE,
      MetaDataPath = ModelPath,
      MetaDataList = FactorLevelsList,
      ImputeMissingValue = 0)
    ScoringData <- Output$TrainData
    MetaData <- Output$MetaData

    # # Args to step through
    # RunMode = 'score'
    # ModelType = TargetType
    # TrainData = ScoringData
    # ValidationData = NULL
    # TestData = NULL
    # TargetVariableName = NULL
    # CategoricalVariableNames = names(FactorLevelsList)[-length(FactorLevelsList)]
    # EncodeMethod = FactorLevelsList$EncodingMethod
    # KeepCategoricalVariables = TRUE
    # ReturnMetaData = TRUE
    # MetaDataPath = ModelPath
    # MetaDataList = FactorLevelsList
    # ImputeMissingValue = 0
    # Debug = FALSE

    # Update FeatureColumnNames
    zz <- names(FactorLevelsList)[-length(FactorLevelsList)]
    if(tolower(FactorLevelsList$EncodingMethod) == 'meow') {
      FeatureColumnNames <- unique(c(FeatureColumnNames, paste0(names(FactorLevelsList)[-length(FactorLevelsList)], '_MixedEffects')))
    } else if(tolower(FactorLevelsList$EncodingMethod) == 'credibility') {
      FeatureColumnNames <- unique(c(FeatureColumnNames, paste0(names(FactorLevelsList)[-length(FactorLevelsList)], '_Credibility')))
    } else if(tolower(FactorLevelsList$EncodingMethod) == 'target_encoding') {
      FeatureColumnNames <- unique(c(FeatureColumnNames, paste0(names(FactorLevelsList)[-length(FactorLevelsList)], '_TargetEncode')))
    }
    yy <- names(data.table::copy(ScoringData))
    FeatureColumnNames <- unique(FeatureColumnNames[!FeatureColumnNames %in% zz])
    FeatureColumnNames <- unique(c(FeatureColumnNames, setdiff(yy,xx)))
    CatFeatures <- NULL
  }

  # Load model ----
  if(Debug) print("XGBoost Scoring 6")
  if(!is.null(ModelObject)) model <- ModelObject else model <- tryCatch({load(file.path(ModelPath, ModelID))}, error = function(x) stop(paste0("Model not found in ModelPath: " , file.path(ModelPath, ModelID))))

  # ModelDataPrep Check ----
  if(Debug) print("XGBoost Scoring 7")
  ScoringData <- Rodeo::ModelDataPrep(data = ScoringData, Impute = MDP_Impute, CharToFactor = MDP_CharToFactor, RemoveDates = MDP_RemoveDates, MissFactor = MDP_MissFactor, MissNum = MDP_MissNum)
  a <- which(!names(ScoringData) %in% model$feature_names)
  if(!identical(a, integer(0))) data.table::set(ScoringData, j = c(names(ScoringData)[which(!names(ScoringData) %in% model$feature_names)]), value = NULL)

  # AutoXGBoostHurdleCARMA 17
  # names(ScoringData)
  # "Date_week"
  # "Date_wom"
  # "Date_month"
  # "Date_quarter"
  # "HolidayCounts"
  # "weeks_LAG_1_Weekly_Sales"
  # "weeks_LAG_2_Weekly_Sales"
  # "weeks_LAG_3_Weekly_Sales"
  # "weeks_LAG_4_Weekly_Sales"
  # "weeks_LAG_5_Weekly_Sales"
  # "Mean_2_weeks_LAG_1_Weekly_Sales"
  # "Mean_3_weeks_LAG_1_Weekly_Sales"
  # "Mean_4_weeks_LAG_1_Weekly_Sales"
  # "Mean_5_weeks_LAG_1_Weekly_Sales"
  # "months_LAG_1_Weekly_Sales"
  # "months_LAG_2_Weekly_Sales"
  # "months_LAG_3_Weekly_Sales"
  # "Mean_2_months_LAG_1_Weekly_Sales"
  # "Mean_3_months_LAG_1_Weekly_Sales"
  # "weeks_LAG_1_HolidayCounts"
  # "Mean_2_weeks_LAG_1_HolidayCounts"
  # "TimeTrend"
  # "GroupVar_Credibility"


  # Initialize XGBoost Data Conversion ----
  if(Debug) print("XGBoost Scoring 8")
  ScoringMatrix <- xgboost::xgb.DMatrix(as.matrix(ScoringData))

  # Score model ----
  if(Debug) print("XGBoost Scoring 9")
  if(tolower(TargetType) != "multiclass") {
    predict <- data.table::as.data.table(stats::predict(model, ScoringMatrix))
    if(ReturnShapValues) ShapValues <- data.table::as.data.table(xgboost:::xgb.shap.data(as.matrix(ScoringData), model = ModelObject, features = names(ScoringData))$shap_contrib)
  }

  # Change Output Predictions Column Name ----
  if(Debug) print("XGBoost Scoring 10")
  if(tolower(TargetType) == "classification") {
    data.table::setnames(predict, "V1", "p1")
  } else if(tolower(TargetType) == "regression") {
    data.table::setnames(predict, "V1", "Predictions")
  } else if(tolower(TargetType) == "multiclass") {
    if(is.null(TargetLevels)) TargetLevels <- data.table::fread(file.path(ModelPath, paste0(ModelID, "_TargetLevels.csv")))
    NumLevels <- TargetLevels[, .N]
    predict <- XGBoostMultiClassPredict(model, ScoringMatrix, TargetLevels, NumLevels, NumberRows = nrow(ScoringMatrix))
    if(ReturnShapValues) ShapValues <- data.table::as.data.table(xgboost:::xgb.shap.data(as.matrix(ScoringData), model = ModelObject, features = names(ScoringData))$shap_contrib)
  }

  # Merge features back ----
  if(Debug) print("XGBoost Scoring 11")
  if(ReturnFeatures && ReturnShapValues) {
    predict <- cbind(predict, ScoringMerge, ShapValues)
  } else if(ReturnFeatures) {
    predict <- cbind(predict, ScoringMerge)
  } else if(ReturnShapValues) {
    predict <- cbind(predict, ShapValues)
  }

  # Back Transform Numeric Variables ----
  if(Debug) print("XGBoost Scoring 12")
  if(BackTransNumeric) {
    grid_trans_results <- data.table::copy(TransformationObject)
    grid_trans_results <- grid_trans_results[ColumnName != eval(TargetColumnName)]
    data.table::set(grid_trans_results, i = which(grid_trans_results[["ColumnName"]] == eval(TargetColumnName)), j = "ColumnName", value = "Predictions")
    predict <- Rodeo::AutoTransformationScore(ScoringData = predict, Type = "Inverse", FinalResults = grid_trans_results, TransID = NULL, Path = NULL)
  }

  # Return data ----
  if(Debug) print("XGBoost Scoring 13")
  return(predict)
}
