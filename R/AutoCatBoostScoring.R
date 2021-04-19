#' @title AutoCatBoostScoring
#'
#' @description AutoCatBoostScoring is an automated scoring function that compliments the AutoCatBoost model training functions. This function requires you to supply features for scoring. It will run ModelDataPrep() to prepare your features for catboost data conversion and scoring.
#'
#' @author Adrian Antico
#' @family Automated Model Scoring
#'
#' @param TargetType Set this value to "regression", "classification", "multiclass", or "multiregression" to score models built using AutoCatBoostRegression(), AutoCatBoostClassify() or AutoCatBoostMultiClass().
#' @param ScoringData This is your data.table of features for scoring. Can be a single row or batch.
#' @param FeatureColumnNames Supply either column names or column numbers used in the AutoCatBoostRegression() function
#' @param FactorLevelsList List of factors levels to DummifyDT()
#' @param IDcols Supply ID column numbers for any metadata you want returned with your predicted values
#' @param OneHot Passsed to DummifyD
#' @param ReturnShapValues Set to TRUE to return a data.table of feature contributions to all predicted values generated
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
#' \dontrun{
#'
#' # Create some dummy correlated data
#' data <- RemixAutoML::FakeDataGenerator(
#'   Correlation = 0.85,
#'   N = 10000,
#'   ID = 2,
#'   ZIP = 0,
#'   AddDate = FALSE,
#'   Classification = FALSE,
#'   MultiClass = FALSE)
#'
#' # Train a Multiple Regression Model (two target variables)
#' TestModel <- RemixAutoML::AutoCatBoostRegression(
#'
#'   # GPU or CPU and the number of available GPUs
#'   task_type = "GPU",
#'   NumGPUs = 1,
#'
#'   # Metadata arguments
#'   ModelID = "Test_Model_1",
#'   model_path = normalizePath("./"),
#'   metadata_path = NULL,
#'   SaveModelObjects = FALSE,
#'   ReturnModelObjects = TRUE,
#'
#'   # Data arguments
#'   data = data,
#'   TrainOnFull = FALSE,
#'   ValidationData = NULL,
#'   TestData = NULL,
#'   Weights = NULL,
#'   DummifyCols = FALSE,
#'   TargetColumnName = c("Adrian","Independent_Variable1"),
#'   FeatureColNames = names(data)[!names(data) %in%
#'     c("IDcol_1","IDcol_2","Adrian")],
#'   PrimaryDateColumn = NULL,
#'   IDcols = c("IDcol_1","IDcol_2"),
#'   TransformNumericColumns = NULL,
#'   Methods = c("BoxCox","Asinh","Asin","Log","LogPlus1",
#'     "Logit","YeoJohnson"),
#'
#'   # Model evaluation
#'   eval_metric = "MultiRMSE",
#'   eval_metric_value = 1.5,
#'   loss_function = "MultiRMSE",
#'   loss_function_value = 1.5,
#'   MetricPeriods = 10L,
#'   NumOfParDepPlots = ncol(data)-1L-2L,
#'   EvalPlots = TRUE,
#'
#'   # Grid tuning
#'   PassInGrid = NULL,
#'   GridTune = FALSE,
#'   MaxModelsInGrid = 100L,
#'   MaxRunsWithoutNewWinner = 100L,
#'   MaxRunMinutes = 60*60,
#'   BaselineComparison = "default",
#'
#'   # ML Args
#'   langevin = TRUE,
#'   diffusion_temperature = 10000,
#'   Trees = 250,
#'   Depth = 6,
#'   L2_Leaf_Reg = 3.0,
#'   RandomStrength = 1,
#'   BorderCount = 128,
#'   LearningRate = seq(0.01,0.10,0.01),
#'   RSM = c(0.80, 0.85, 0.90, 0.95, 1.0),
#'   BootStrapType = c("Bayesian","Bernoulli","Poisson","MVS","No"),
#'   GrowPolicy = c("SymmetricTree", "Depthwise", "Lossguide"))
#'
#' # Output
#' TestModel$Model
#' TestModel$ValidationData
#' TestModel$EvaluationPlot
#' TestModel$EvaluationBoxPlot
#' TestModel$EvaluationMetrics
#' TestModel$VariableImportance
#' TestModel$InteractionImportance
#' TestModel$ShapValuesDT
#' TestModel$VI_Plot
#' TestModel$PartialDependencePlots
#' TestModel$PartialDependenceBoxPlots
#' TestModel$GridList
#' TestModel$ColNames
#' TestModel$TransformationResults
#'
#' # Score a multiple regression model
#' Preds <- RemixAutoML::AutoCatBoostScoring(
#'   TargetType = "multiregression",
#'   ScoringData = data,
#'   FeatureColumnNames = names(data)[!names(data) %in%
#'     c("IDcol_1", "IDcol_2","Adrian")],
#'   FactorLevelsList = TestModel$FactorLevelsList,
#'   IDcols = c("IDcol_1","IDcol_2"),
#'   OneHot = FALSE,
#'   ReturnShapValues = TRUE,
#'   ModelObject = TestModel$Model,
#'   ModelPath = NULL, #normalizePath("./"),
#'   ModelID = "Test_Model_1",
#'   ReturnFeatures = TRUE,
#'   MultiClassTargetLevels = NULL,
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
#'   MDP_MissNum = -1,
#'   RemoveModel = FALSE)
#' }
#' @return A data.table of predicted values with the option to return model features as well.
#' @export
AutoCatBoostScoring <- function(TargetType = NULL,
                                ScoringData = NULL,
                                FeatureColumnNames = NULL,
                                FactorLevelsList = NULL,
                                IDcols = NULL,
                                OneHot = FALSE,
                                ReturnShapValues = FALSE,
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
                                MDP_Impute = FALSE,
                                MDP_CharToFactor = FALSE,
                                MDP_RemoveDates = FALSE,
                                MDP_MissFactor = "0",
                                MDP_MissNum = -1,
                                RemoveModel = FALSE) {

  # Load catboost ----
  loadNamespace(package = "catboost")

  # tolower(TargetType)
  TargetType <- tolower(TargetType)
  check1 <- ReturnShapValues && !TargetType %chin% c("multiregression", "multiclass")

  # Check arguments ----
  if(!data.table::is.data.table(ScoringData)) data.table::setDT(ScoringData)

  # Pull in ColNames ----
  if(is.null(FeatureColumnNames) && !is.null(ModelPath)) FeatureColumnNames <- data.table::fread(file = file.path(ModelPath, paste0(ModelID, "_ColNames.csv")))

  # Pull In Transformation Object ----
  if(is.null(TransformationObject) && (TransformNumeric || BackTransNumeric)) {
    TransformationObject <- data.table::fread(file.path(TransPath, paste0(TransID, "_transformation.csv")))
  }

  # Identify column numbers for factor variables ----
  CatFeatures <- tryCatch({sort(c(as.numeric(which(sapply(ScoringData, is.factor))), as.numeric(which(sapply(ScoringData, is.character)))))}, error = function(x) NULL)
  if(identical(CatFeatures, numeric(0))) CatFeatures <- NULL

  # DummifyDT categorical columns ----
  if(!is.null(CatFeatures) && TargetType == "multiregression") {
    if(!is.null(FactorLevelsList)) {
      ScoringData <- DummifyDT(
        data = ScoringData,
        cols = if(!is.character(CatFeatures)) names(ScoringData)[CatFeatures] else CatFeatures,
        KeepFactorCols = FALSE,
        OneHot = OneHot,
        SaveFactorLevels = FALSE,
        SavePath = ModelPath,
        ImportFactorLevels = FALSE,
        FactorLevelsList = FactorLevelsList,
        ReturnFactorLevels = FALSE,
        ClustScore = FALSE,
        GroupVar = TRUE)
    } else {
      ScoringData <- DummifyDT(
        data = ScoringData,
        cols = if(!is.character(CatFeatures)) names(ScoringData)[CatFeatures] else CatFeatures,
        KeepFactorCols = FALSE,
        OneHot = OneHot,
        SaveFactorLevels = FALSE,
        SavePath = ModelPath,
        ImportFactorLevels = TRUE,
        ReturnFactorLevels = FALSE,
        ClustScore = FALSE,
        GroupVar = TRUE)
    }

    # Return value to CatFeatures as if there are no categorical variables
    CatFeatures <- NULL
  }

  # Convert CatFeatures to 1-indexed ----
  if(!is.null(CatFeatures)) CatFeatures <- CatFeatures - 1L

  # ModelDataPrep Check ----
  if(any(c(MDP_Impute,MDP_CharToFactor,MDP_RemoveDates))) {
    ScoringData <- ModelDataPrep(
      data = ScoringData,
      Impute = MDP_Impute,
      CharToFactor = MDP_CharToFactor,
      RemoveDates = MDP_RemoveDates,
      MissFactor = MDP_MissFactor,
      MissNum = MDP_MissNum)
  }

  # IDcols conversion ----
  if(is.numeric(IDcols)) IDcols <- names(data)[IDcols]

  # Apply Transform Numeric Variables ----
  if(TransformNumeric) {
    tempTrans <- data.table::copy(TransformationObject)
    tempTrans <- tempTrans[ColumnName != eval(TargetColumnName)]
    ScoringData <- AutoTransformationScore(
      ScoringData = ScoringData,
      FinalResults = tempTrans,
      Type = "Apply",
      TransID = TransID,
      Path = TransPath)
  }

  # Convert FeatureColumnNames to Character Names ----
  if(data.table::is.data.table(FeatureColumnNames)) {
    FeatureColumnNames <- FeatureColumnNames[[1L]]
  } else if(is.numeric(FeatureColumnNames)) {
    FeatureColumnNames <- names(ScoringData)[FeatureColumnNames]
  }

  # Remove Target from FeatureColumnNames ----
  if((TransformNumeric || BackTransNumeric) && !is.null(TargetColumnName) && TargetColumnName %chin% FeatureColumnNames) {
    FeatureColumnNames <- FeatureColumnNames[!(TargetColumnName == FeatureColumnNames)]
  }

  # Subset Columns Needed ----
  if(ReturnFeatures && TargetType != "multiclass") ScoringMerge <- data.table::copy(ScoringData)
  if(!is.null(IDcols) && TargetType != "multiregression") {
    FeatureColumnNames <- FeatureColumnNames[!FeatureColumnNames %chin% c(IDcols)]
  } else if(TargetType == "multiregression") {
    temp <- setdiff(names(ScoringData), c(TargetColumnName, FeatureColumnNames))
    FeatureColumnNames <- c(FeatureColumnNames, temp)
    FeatureColumnNames <- FeatureColumnNames[!FeatureColumnNames %chin% "GroupVar"]
  }
  if(!identical(setdiff(names(ScoringData), FeatureColumnNames), character(0)) && TargetType != "multiregression") {
    data.table::set(ScoringData, j = setdiff(names(ScoringData), FeatureColumnNames), value = NULL)
  } else if(TargetType == "multiregression") {
    data.table::set(ScoringData, j = setdiff(names(ScoringData), FeatureColumnNames), value = NULL)
  }

  # Initialize Catboost Data Conversion ----
  if(!is.null(CatFeatures)) {
    ScoringPool <- catboost::catboost.load_pool(ScoringData, cat_features = CatFeatures)
  } else {
    ScoringPool <- catboost::catboost.load_pool(ScoringData)
  }

  # Load model ----
  if(is.null(ModelObject)) ModelObject <- catboost::catboost.load_model(file.path(ModelPath, ModelID))

  # Score model ----
  if(TargetType == "regression" || TargetType == "multiregression") {
    predict <- data.table::as.data.table(
      catboost::catboost.predict(
        model = ModelObject,
        pool = ScoringPool,
        prediction_type = "RawFormulaVal",
        thread_count = -1L))
  } else if(TargetType == "classification") {
    predict <- data.table::as.data.table(
      catboost::catboost.predict(
        model = ModelObject,
        pool = ScoringPool,
        prediction_type = "Probability"))
  } else if(TargetType == "multiclass") {
    predict <- data.table::as.data.table(cbind(
      1 + catboost::catboost.predict(
        model = ModelObject,
        pool = ScoringPool,
        prediction_type = "Class"),
      catboost::catboost.predict(
        model = ModelObject,
        pool = ScoringPool,
        prediction_type = "Probability")))
  }

  # Create ShapValues ----
  if(check1) {
    ShapValues <- data.table::as.data.table(catboost::catboost.get_feature_importance(ModelObject, pool = ScoringPool, type = "ShapValues"))
    data.table::setnames(ShapValues, names(ShapValues), c(paste0("Shap_",FeatureColumnNames), "Predictions"))
    ShapValues[, Predictions := NULL]
  }

  # Remove Model ----
  if(RemoveModel) rm(ModelObject)

  # Score model -----
  if(TargetType == "multiclass") {
    data.table::setnames(predict, "V1", "Predictions")
    if(is.null(MultiClassTargetLevels)) MultiClassTargetLevels <- data.table::fread(file.path(ModelPath, paste0(ModelID, "_TargetLevels.csv")))
    g <- as.character(MultiClassTargetLevels[[1L]])
    data.table::setnames(predict, paste0("V", seq_along(g)), g)
    predict <- merge(
      predict,
      MultiClassTargetLevels,
      by.x = "Predictions",
      by.y = "NewLevels",
      all = FALSE)
    predict[, Predictions := OriginalLevels][, OriginalLevels := NULL]
  }

  # Rename predicted value ----
  if(TargetType == "regression") {
    data.table::setnames(predict, "V1", "Predictions")
  } else if(TargetType == "multiregression") {
    data.table::setnames(predict, paste0("V",seq_along(predict)), paste0("Predictions.V", seq_along(predict)))
  } else if(TargetType == "classification") {
    data.table::setnames(predict, "V1", "p1")
  }

  # Merge features back on ----
  if(ReturnFeatures && TargetType != "multiclass") predict <- cbind(predict, ScoringMerge)

  # Back Transform Numeric Variables ----
  if(BackTransNumeric && TargetType != "multiregression") {
    grid_trans_results <- data.table::copy(TransformationObject)
    data.table::set(grid_trans_results, i = which(grid_trans_results[["ColumnName"]] == eval(TargetColumnName)), j = "ColumnName", value = "Predictions")
    grid_trans_results <- grid_trans_results[ColumnName != eval(TargetColumnName)]

    # Run Back-Transform ----
    predict <- AutoTransformationScore(
      ScoringData = predict,
      Type = "Inverse",
      FinalResults = grid_trans_results,
      TransID = NULL,
      Path = NULL)
  } else if(BackTransNumeric && TargetType == "multiregression") {

    # Prepare transformation object
    TransformationObject <- data.table::rbindlist(list(TransformationObject, TransformationObject))
    for(z in seq_along(TransformationObject)) TransformationObject[length(TargetColumnName.) + z, ColumnName := paste0("Predict.V",z)]

    # Back transform
    predict <- AutoTransformationScore(
      ScoringData = predict,
      Type = "Inverse",
      FinalResults = TransformationObject,
      TransID = NULL,
      Path = NULL)
  }

  # Return data ----
  if(check1) {
    return(cbind(predict, ShapValues))
  } else {
    return(predict)
  }
}
