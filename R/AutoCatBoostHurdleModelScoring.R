#' @title AutoCatBoostHurdleModelScoring
#'
#' @description AutoCatBoostHurdleModelScoring can score AutoCatBoostHurdleModel() models
#'
#' @author Adrian Antico
#' @family Automated Model Hurdle Modeling
#'
#' @param TestData scoring data.table
#' @param Path Supply if ArgsList is NULL or ModelList is null.
#' @param ModelID Supply if ArgsList is NULL or ModelList is null. Same as used in model training.
#' @param ArgsList Output from the hurdle model
#' @param ModelList Output from the hurdle model
#' @param Threshold NULL to use raw probabilities to predict. Otherwise, supply a threshold
#' @param CARMA Keep FALSE. Used for CARMA functions internals
#' @return A data.table with the final predicted value, the intermediate model predictions, and your source data
#' @examples
#' \dontrun{
#'
#' # Define file path
#' Path <- getwd()
#'
#' # Create hurdle data with correlated features
#' data <- RemixAutoML::FakeDataGenerator(
#'   Correlation = 0.70,
#'   N = 25000,
#'   ID = 3,
#'   FactorCount = 2L,
#'   AddDate = TRUE,
#'   ZIP = 1,
#'   Classification = FALSE,
#'   MultiClass = FALSE)
#'
#' # Define features
#' Features <- names(data)[!names(data) %chin%
#'   c("Adrian","IDcol_1","IDcol_2","IDcol_3","DateTime")]
#'
#' # Build hurdle model
#' Output <- RemixAutoML::AutoCatBoostHurdleModel(
#'
#'   # Operationalization args
#'   TreeMethod = "hist",
#'   TrainOnFull = FALSE,
#'   PassInGrid = NULL,
#'
#'   # Metadata args
#'   NThreads = max(1L, parallel::detectCores()-2L),
#'   ModelID = "ModelTest",
#'   Paths = normalizePath(Path),
#'   MetaDataPaths = NULL,
#'   ReturnModelObjects = TRUE,
#'
#'   # data args
#'   data,
#'   ValidationData = NULL,
#'   TestData = NULL,
#'   Buckets = c(0),
#'   TargetColumnName = "Adrian",
#'   FeatureColNames = Features,
#'   IDcols = c("IDcol_1","IDcol_2","IDcol_3"),
#'
#'   # options
#'   TransformNumericColumns = NULL,
#'   SplitRatios = c(0.70, 0.20, 0.10),
#'   SaveModelObjects = TRUE,
#'   NumOfParDepPlots = 10L,
#'
#'   # grid tuning args
#'   GridTune = FALSE,
#'   grid_eval_metric = "accuracy",
#'   MaxModelsInGrid = 1L,
#'   BaselineComparison = "default",
#'   MaxRunsWithoutNewWinner = 10L,
#'   MaxRunMinutes = 60L,
#'
#'   # bandit hyperparameters
#'   Trees = 100L,
#'   eta = seq(0.05,0.40,0.05),
#'   max_depth = seq(4L, 16L, 2L),
#'
#'   # random hyperparameters
#'   min_child_weight = seq(1.0, 10.0, 1.0),
#'   subsample = seq(0.55, 1.0, 0.05),
#'   colsample_bytree = seq(0.55, 1.0, 0.05))
#'
#' # Score XGBoost Hurdle Model
#' HurdleScores <- RemixAutoML::AutoCatBoostHurdleModelScoring(
#'   TestData = data,
#'   Path = Path,
#'   ModelID = "ModelTest",
#'   ModelList = NULL,
#'   ArgsList = NULL,
#'   Threshold = NULL)
#' }
#' @export
AutoCatBoostHurdleModelScoring <- function(TestData = NULL,
                                           Path = NULL,
                                           ModelID = NULL,
                                           ArgsList = NULL,
                                           ModelList = NULL,
                                           Threshold = NULL,
                                           CARMA = FALSE) {

  # Load ArgsList and ModelList if NULL ----
  if(is.null(Path) && (is.null(ArgsList) || is.null(ModelList))) stop("Supply a value to the Path argument to where the ArgsList and ModelList are located")

  # Load ArgsList and ModelList if not supplied ----
  if(is.null(ArgsList)) {
    load(file.path(Path, paste0(ModelID, "_HurdleArgList.Rdata")), envir = .GlobalEnv)
  }

  # Define Buckets ----
  Buckets <- ArgsList$Buckets

  # Score Classification Model ----
  if(length(Buckets) == 1L) TargetType <- "Classification" else TargetType <- "Multiclass"

  # Store classifier model----
  if(!is.null(ModelList)) {
    ClassModel <- ModelList[[1L]]
  } else if(!is.null(ArgsList$ModelID)) {
    ClassModel <- catboost::catboost.load_model(model_path = file.path(ArgsList$Paths, ArgsList$ModelID))
  } else {
    stop("Need to supply a ModelList")
  }

  # Store FeatureNames ----
  FeatureNames <- ArgsList$FeatureColNames

  # Factor levels list ----
  if(!is.null(ArgsList$FactorLevelsList)) FactorLevelsList <- ArgsList$FactorLevelsList else FactorLevelsList <- NULL

  # Store IDcols ----
  IDcols <- c(setdiff(names(TestData), c(ArgsList$FeatureColNames)))

  # Store colnames ----
  ColumnNames <- names(TestData)

  # Classification Model Scoring ----
  temp <- AutoCatBoostScoring(
    TargetType = TargetType,
    ScoringData = TestData,
    FeatureColumnNames = FeatureNames,
    IDcols = IDcols,
    ModelObject = ClassModel,
    ModelPath = ArgsList$Paths,
    ModelID = ArgsList$ModelID,
    RemoveModel = TRUE,
    ReturnFeatures = TRUE,
    MultiClassTargetLevels = ArgsList$TargetLevels,
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

  # Nuance
  if(TargetType != "Classification") {
    TestData <- cbind(temp, TestData)
  } else {
    TestData <- temp
    rm(temp)
  }

  # Change name for classification ----
  if(tolower(TargetType) == 'classification') {
    data.table::setnames(TestData, 'p1', 'Predictions_C1')
    TestData[, Predictions_C0 := 1 - Predictions_C1]
    data.table::setcolorder(TestData, c(ncol(TestData), 1L, 2L:(ncol(TestData) - 1L)))
  }

  # Change Name of Predicted MultiClass Column ----
  if(tolower(TargetType) != 'classification') {
    if(length(Buckets) != 1L) data.table::setnames(TestData, 'Predictions', 'Predictions_MultiClass')
  }

  # Begin regression model scoring ----
  looper <- ArgsList$looper
  counter <- max(looper)

  # Loop through model scoring ----
  for(bucket in looper) {

    # Update IDcols----
    IDcolsModified <- c(IDcols, setdiff(names(TestData), ColumnNames))

    # Check for constant value bucket----
    if(!any(bucket %in% c(ArgsList$degenerate))) {

      # Increment----
      counter <- counter - 1L

      # Score TestData----
      if(bucket == max(looper)) ModelIDD <- paste0(ArgsList$ModelID,"_",bucket,"+") else ModelIDD <- paste0(ArgsList$ModelID, "_", bucket)

      # Manage TransformationResults
      if(is.null(ArgsList$TransformNumericColumns)) TransformationResults <- NULL else TransformationResults <- ArgsList$TransformNumericColumns

      # Store Transformations----
      if(!is.null(ArgsList$TransformNumericColumns)) {
        TransformationResults <- ArgsList[[paste0("TransformationResults_", ModelIDD)]]
        Transform <- TRUE
      } else {
        TransformationResults <- NULL
        Transform <- FALSE
      }

      # Load Model----
      if(!is.null(ModelList)) {
        RegressionModel <- ModelList[[ModelIDD]]
      } else {
        RegressionModel <- catboost::catboost.load_model(model_path = file.path(ArgsList$Paths, ModelIDD))
      }

      # Catboost Model Scroring ----
      TestData <- AutoCatBoostScoring(
        TargetType = "regression",
        ScoringData = TestData,
        FeatureColumnNames = FeatureNames,
        IDcols = IDcolsModified,
        ModelObject = RegressionModel,
        ModelPath = ArgsList$Paths,
        ModelID = ModelIDD,
        ReturnFeatures = TRUE,
        TransformationObject = TransformationResults,
        TargetColumnName = ArgsList$TransformNumericColumns,
        TransformNumeric = Transform,
        BackTransNumeric = Transform,
        TransID = NULL,
        TransPath = NULL,
        MDP_Impute = TRUE,
        MDP_CharToFactor = TRUE,
        MDP_RemoveDates = FALSE,
        MDP_MissFactor = "0",
        MDP_MissNum = -1)

      # Remove Model----
      rm(RegressionModel)

      # Change prediction name to prevent duplicates----
      if(bucket == max(looper)) Val <- paste0('Predictions_', bucket - 1L, '+') else Val <- paste0('Predictions_', bucket)
      if(length(which(names(TestData) == "Predictions")) > 1) {
        zzz <- names(TestData)
        zzz[max(which(names(TestData) == "Predictions"))] <- Val
        colnames(TestData) <- zzz
      } else {
        data.table::setnames(TestData, "Predictions", Val)
      }

    } else {

      # Use single value for predictions in the case of zero variance ----
      if(bucket == max(seq_len(length(Buckets) + 1L))) {
        data.table::setalloccol(DT = TestData, n = 10)
        data.table::set(TestData, j = paste0("Predictions", Buckets[bucket - 1L], "+"), value = Buckets[bucket])
        data.table::setcolorder(TestData, c(ncol(TestData), 1L:(ncol(TestData)-1L)))
      } else {
        data.table::setalloccol(DT = TestData, n = ncol(TestData) + 10)
        data.table::set(TestData, j = paste0("Predictions", Buckets[bucket]), value = Buckets[bucket])
        data.table::setcolorder(TestData, c(ncol(TestData), 1L:(ncol(TestData)-1L)))
      }
    }
  }

  # Rearrange cols ----
  data.table::setcolorder(TestData, c(which(names(TestData) %like% "Predictions"), setdiff(seq_along(TestData), which(names(TestData) %like% "Predictions"))))

  # Final Combination of Predictions ----
  counter <- length(Buckets)
  if(counter > 2L || (counter == 2L && length(Buckets) != 1L)) {
    for(i in rev(looper)) {
      if(i == 1L) {
        TestData[, UpdatedPrediction := TestData[[i]] * TestData[[i + 1L + (length(looper))]]]
      } else {
        TestData[, UpdatedPrediction := UpdatedPrediction + TestData[[i]] * TestData[[i + 1L + length(looper)]]]
      }
    }
  } else {
    if(0 %in% Buckets) {
      TestData[, UpdatedPrediction := TestData[[2L]] * TestData[[4L]]]
    } else {
      TestData[, UpdatedPrediction := TestData[[1L]] * TestData[[3L]] + TestData[[2L]] * TestData[[4L]]]
    }
  }

  # Final column rearrange----
  data.table::setcolorder(TestData, c(ncol(TestData), 1L:(ncol(TestData) - 1L)))

  # Return preds----
  return(TestData)
}
