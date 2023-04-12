#' @title AutoCatBoostScoring
#'
#' @description AutoCatBoostScoring is an automated scoring function that compliments the AutoCatBoost model training functions. This function requires you to supply features for scoring. It will run ModelDataPrep() to prepare your features for catboost data conversion and scoring.
#'
#' @author Adrian Antico
#' @family Automated Model Scoring
#'
#' @param TargetType Set this value to 'regression', 'classification', 'multiclass', or 'multiregression' to score models built using AutoCatBoostRegression(), AutoCatBoostClassifier() or AutoCatBoostMultiClass().
#' @param ScoringData This is your data.table of features for scoring. Can be a single row or batch.
#' @param FeatureColumnNames Supply either column names or column numbers used in the AutoCatBoostRegression() function
#' @param FactorLevelsList List of factors levels to CharacterEncode()
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
#' @param Debug = FALSE
#' @examples
#' \dontrun{
#'
#' # CatBoost Regression Example
#'
#' # Create some dummy correlated data
#' data <- AutoQuant::FakeDataGenerator(
#'   Correlation = 0.85,
#'   N = 10000,
#'   ID = 2,
#'   ZIP = 0,
#'   AddDate = FALSE,
#'   Classification = FALSE,
#'   MultiClass = FALSE)
#'
#' # Copy data
#' data1 <- data.table::copy(data)
#'
#' # Run function
#' TestModel <- AutoQuant::AutoCatBoostRegression(
#'
#'   # GPU or CPU and the number of available GPUs
#'   TrainOnFull = FALSE,
#'   task_type = 'CPU',
#'   NumGPUs = 1,
#'   DebugMode = FALSE,
#'
#'   # Metadata args
#'   OutputSelection = c('Importances','EvalPlots','EvalMetrics','Score_TrainData'),
#'   ModelID = 'Test_Model_1',
#'   model_path = getwd(),
#'   metadata_path = getwd(),
#'   SaveModelObjects = FALSE,
#'   SaveInfoToPDF = FALSE,
#'   ReturnModelObjects = TRUE,
#'
#'   # Data args
#'   data = data1,
#'   ValidationData = NULL,
#'   TestData = NULL,
#'   TargetColumnName = 'Adrian',
#'   FeatureColNames = names(data1)[!names(data1) %in% c('IDcol_1', 'IDcol_2','Adrian')],
#'   PrimaryDateColumn = NULL,
#'   WeightsColumnName = NULL,
#'   IDcols = c('IDcol_1','IDcol_2'),
#'   TransformNumericColumns = 'Adrian',
#'   Methods = c('Asinh','Asin','Log','LogPlus1','Sqrt','Logit'),
#'
#'   # Model evaluation
#'   eval_metric = 'RMSE',
#'   eval_metric_value = 1.5,
#'   loss_function = 'RMSE',
#'   loss_function_value = 1.5,
#'   MetricPeriods = 10L,
#'   NumOfParDepPlots = ncol(data1)-1L-2L,
#'
#'   # Grid tuning args
#'   PassInGrid = NULL,
#'   GridTune = FALSE,
#'   MaxModelsInGrid = 30L,
#'   MaxRunsWithoutNewWinner = 20L,
#'   MaxRunMinutes = 60*60,
#'   BaselineComparison = 'default',
#'
#'   # ML args
#'   langevin = FALSE,
#'   diffusion_temperature = 10000,
#'   Trees = 1000,
#'   Depth = 9,
#'   L2_Leaf_Reg = NULL,
#'   RandomStrength = 1,
#'   BorderCount = 128,
#'   LearningRate = NULL,
#'   RSM = 1,
#'   BootStrapType = NULL,
#'   GrowPolicy = 'SymmetricTree',
#'   model_size_reg = 0.5,
#'   feature_border_type = 'GreedyLogSum',
#'   sampling_unit = 'Object',
#'   subsample = NULL,
#'   score_function = 'Cosine',
#'   min_data_in_leaf = 1)
#'
#' # Trained Model Object
#' TestModel$Model
#'
#' # Train Data (includes validation data) and Test Data with predictions and shap values
#' TestModel$TrainData
#' TestModel$TestData
#'
#' # Calibration Plots
#' TestModel$PlotList$Train_EvaluationPlot
#' TestModel$PlotList$Test_EvaluationPlot
#'
#' # Calibration Box Plots
#' TestModel$PlotList$Train_EvaluationBoxPlot
#' TestModel$PlotList$Test_EvaluationBoxPlot
#'
#' # Residual Analysis Plots
#' TestModel$PlotList$Train_ResidualsHistogram
#' TestModel$PlotList$Test_ResidualsHistogram
#'
#' # Preds vs Actuals Scatterplots
#' TestModel$PlotList$Train_ScatterPlot
#' TestModel$PlotList$Test_ScatterPlot
#'
#' # Preds vs Actuals Copula Plot
#' TestModel$PlotList$Train_CopulaPlot
#' TestModel$PlotList$Test_CopulaPlot
#'
#' # Variable Importance Plots
#' TestModel$PlotList$Train_VariableImportance
#' TestModel$PlotList$Validation_VariableImportance
#' TestModel$PlotList$Test_VariableImportance
#'
#' # Evaluation Metrics
#' TestModel$EvaluationMetrics$TrainData
#' TestModel$EvaluationMetrics$TestData
#'
#' # Variable Importance Tables
#' TestModel$VariableImportance$Train_Importance
#' TestModel$VariableImportance$Validation_Importance
#' TestModel$VariableImportance$Test_Importance
#'
#' # Interaction Importance
#' TestModel$InteractionImportance$Train_Interaction
#' TestModel$InteractionImportance$Validation_Interaction
#' TestModel$InteractionImportance$Test_Interaction
#'
#' # Meta Data
#' TestModel$ColNames
#' TestModel$TransformationResults
#' TestModel$GridList
#'
#' # Score data
#' Preds <- AutoQuant::AutoCatBoostScoring(
#'   TargetType = 'regression',
#'   ScoringData = data,
#'   FeatureColumnNames = names(data)[!names(data) %in% c('IDcol_1', 'IDcol_2','Adrian')],
#'   FactorLevelsList = TestModel$FactorLevelsList,
#'   IDcols = c('IDcol_1','IDcol_2'),
#'   OneHot = FALSE,
#'   ReturnShapValues = TRUE,
#'   ModelObject = TestModel$Model,
#'   ModelPath = NULL,
#'   ModelID = 'Test_Model_1',
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
#'   MDP_MissFactor = '0',
#'   MDP_MissNum = -1,
#'   RemoveModel = FALSE)
#'
#'   # Step through scoring function
#'   library(AutoQuant)
#'   library(data.table)
#'   TargetType = 'regression'
#'   ScoringData = data
#'   FeatureColumnNames = names(data)[!names(data) %in% c('IDcol_1', 'IDcol_2','Adrian')]
#'   FactorLevelsList = TestModel$FactorLevelsList
#'   IDcols = c('IDcol_1','IDcol_2')
#'   OneHot = FALSE
#'   ReturnShapValues = TRUE
#'   ModelObject = TestModel$Model
#'   ModelPath = NULL
#'   ModelID = 'Test_Model_1'
#'   ReturnFeatures = TRUE
#'   MultiClassTargetLevels = NULL
#'   TransformNumeric = FALSE
#'   BackTransNumeric = FALSE
#'   TargetColumnName = NULL
#'   TransformationObject = NULL
#'   TransID = NULL
#'   TransPath = NULL
#'   MDP_Impute = TRUE
#'   MDP_CharToFactor = TRUE
#'   MDP_RemoveDates = TRUE
#'   MDP_MissFactor = '0'
#'   MDP_MissNum = -1
#'   RemoveModel = FALSE
#'   Debug = TRUE
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
                                MDP_MissFactor = '0',
                                MDP_MissNum = -1,
                                RemoveModel = FALSE,
                                Debug = FALSE) {

  # Load catboost ----
  loadNamespace(package = 'catboost')

  # tolower(TargetType)
  TargetType <- tolower(TargetType)
  check1 <- ReturnShapValues && !TargetType %chin% c('multiregression', 'multiclass')

  # Check arguments ----
  if(!data.table::is.data.table(ScoringData)) data.table::setDT(ScoringData)

  # IDcols conversion ----
  if(is.numeric(IDcols)) IDcols <- names(data)[IDcols]

  # Pull in ColNames ----
  if(is.null(FeatureColumnNames) && !is.null(ModelPath)) FeatureColumnNames <- data.table::fread(file = file.path(ModelPath, paste0(ModelID, '_ColNames.csv')))

  # Pull In Transformation Object ----
  if(is.null(TransformationObject) && (TransformNumeric || BackTransNumeric)) {
    TransformationObject <- data.table::fread(file.path(TransPath, paste0(TransID, '_transformation.csv')))
  }

  # Identify column numbers for factor variables ----
  if(Debug) print('Scoring Here 1')
  CatFeatures <- tryCatch({sort(c(as.numeric(which(sapply(ScoringData, is.factor))), as.numeric(which(sapply(ScoringData, is.character)))))}, error = function(x) NULL)
  CatFeatures <- CatFeatures[CatFeatures %in% which(names(ScoringData) %in% FeatureColumnNames)]
  if(!is.null(IDcols)) CatFeatures <- CatFeatures[!CatFeatures %in% which(names(ScoringData) %in% IDcols)]
  if(identical(CatFeatures, numeric(0))) CatFeatures <- NULL

  # DummifyDT categorical columns ----
  if(Debug) print('Scoring Here 2')
  if(length(CatFeatures) > 0L) {

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
      CategoricalVariableNames = if(!is.character(CatFeatures)) names(ScoringData)[CatFeatures] else CatFeatures,
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
    # CategoricalVariableNames = if(!is.character(CatFeatures)) names(ScoringData)[CatFeatures] else CatFeatures
    # EncodeMethod = FactorLevelsList$EncodingMethod
    # KeepCategoricalVariables = TRUE
    # ReturnMetaData = TRUE
    # MetaDataPath = ModelPath
    # MetaDataList = FactorLevelsList
    # ImputeMissingValue = 0

    # Update FeatureColumnNames
    if(!is.character(CatFeatures)) zz <- names(ScoringData)[CatFeatures] else zz <- CatFeatures
    zz <- names(FactorLevelsList)[-length(FactorLevelsList)]
    if(tolower(FactorLevelsList$EncodingMethod) == 'meow') {
      FeatureColumnNames <- unique(c(FeatureColumnNames, paste0(names(FactorLevelsList)[-length(FactorLevelsList)], '_MixedEffects')))
    } else if(tolower(FactorLevelsList$EncodingMethod) == 'credibility') {
      FeatureColumnNames <- unique(c(FeatureColumnNames, paste0(names(FactorLevelsList)[-length(FactorLevelsList)], '_Credibility')))
    } else if(tolower(FactorLevelsList$EncodingMethod) == 'target_encoding') {
      FeatureColumnNames <- unique(c(FeatureColumnNames, paste0(names(FactorLevelsList)[-length(FactorLevelsList)], '_TargetEncode')))
    }
    yy <- names(data.table::copy(ScoringData))
    FeatureColumnNames <- FeatureColumnNames[!FeatureColumnNames %in% zz]
    FeatureColumnNames <- c(FeatureColumnNames, setdiff(yy,xx))
    CatFeatures <- NULL

  } else {
    MetaData <- NULL
    CatFeatures <- NULL
  }

  # Convert CatFeatures to 1-indexed ----
  if(!is.null(CatFeatures)) CatFeatures <- CatFeatures - 1L

  # ModelDataPrep Check ----
  if(ReturnFeatures && TargetType != 'multiclass') ScoringMerge <- data.table::copy(ScoringData)
  if(any(c(MDP_Impute, MDP_CharToFactor, MDP_RemoveDates))) {
    ScoringData <- Rodeo::ModelDataPrep(
      data = ScoringData,
      Impute = MDP_Impute,
      CharToFactor = MDP_CharToFactor,
      RemoveDates = MDP_RemoveDates,
      MissFactor = MDP_MissFactor,
      MissNum = MDP_MissNum)
  }

  # Apply Transform Numeric Variables ----
  if(TransformNumeric) {
    tempTrans <- data.table::copy(TransformationObject)
    tempTrans <- tempTrans[ColumnName != eval(TargetColumnName)]
    ScoringData <- Rodeo::AutoTransformationScore(
      ScoringData = ScoringData,
      FinalResults = tempTrans,
      Type = 'Apply',
      TransID = TransID,
      Path = TransPath)
  }

  # Convert FeatureColumnNames to Character Names ----
  if(Debug) print('Scoring Here 3')
  if(data.table::is.data.table(FeatureColumnNames)) {
    FeatureColumnNames <- FeatureColumnNames[[1L]]
  } else if(is.numeric(FeatureColumnNames)) {
    FeatureColumnNames <- names(ScoringData)
  }

  # Remove Target from FeatureColumnNames ----
  if((TransformNumeric || BackTransNumeric) && !is.null(TargetColumnName) && TargetColumnName %chin% FeatureColumnNames) {
    FeatureColumnNames <- FeatureColumnNames[!(TargetColumnName == FeatureColumnNames)]
  }

  # Subset Columns Needed ----
  if(Debug) print('Scoring Here 4')
  if(!is.null(IDcols) && TargetType != 'multiregression' && any(FeatureColumnNames %chin% c(IDcols))) {
    FeatureColumnNames <- FeatureColumnNames[!FeatureColumnNames %chin% c(IDcols)]
  } else if(TargetType == 'multiregression') {
    temp <- setdiff(names(ScoringData), c(TargetColumnName, FeatureColumnNames))
    FeatureColumnNames <- c(FeatureColumnNames, temp)
    FeatureColumnNames <- FeatureColumnNames[!FeatureColumnNames %chin% 'GroupVar']
  }

  # Debugging
  if(Debug) {
    print('AutoCatBoostHurdleModelScoring QA Check 1')
    print(!identical(setdiff(names(ScoringData), FeatureColumnNames), character(0)) && TargetType != 'multiregression')
  }

  # Remove unnecessary columns
  if(Debug) print('Scoring Here 5')
  if(Debug) print(!identical(setdiff(names(ScoringData), FeatureColumnNames), character(0)))
  if(!identical(setdiff(names(ScoringData), FeatureColumnNames), character(0)) && TargetType != 'multiregression') {
    if(Debug) {
      print('AutoCatBoostHurdleModelScoring QA Check 2')
      print(c(setdiff(names(ScoringData), FeatureColumnNames)))
      print(all(c(setdiff(names(ScoringData), FeatureColumnNames)) %in% names(ScoringData)))
    }
    if(length(setdiff(names(ScoringData), FeatureColumnNames)) > 0L) {
      data.table::set(ScoringData, j = c(setdiff(names(ScoringData), FeatureColumnNames)), value = NULL)
    }
  } else if(TargetType == 'multiregression') {
    if(length(setdiff(names(ScoringData), FeatureColumnNames)) > 0L) {
      if(Debug) print("Here 5.1")
      if(Debug) print(length(setdiff(names(ScoringData), FeatureColumnNames)) > 0L)
      tryCatch({data.table::set(ScoringData, j = setdiff(names(ScoringData), FeatureColumnNames), value = NULL)}, error = function(x) NULL)
    }
  }

  # Initialize Catboost Data Conversion ----
  if(Debug) print('Scoring Here 6')
  if(!is.null(CatFeatures)) {
    if(Debug) print("CatFeatures is NOT NULL")
    ScoringPool <- catboost::catboost.load_pool(ScoringData, cat_features = CatFeatures)
  } else {
    if(Debug) print("CatFeatures is NULL")
    print(ScoringData)
    ScoringPool <- catboost::catboost.load_pool(ScoringData)
  }

  # Load model ----
  if(is.null(ModelObject)) ModelObject <- catboost::catboost.load_model(file.path(ModelPath, ModelID))

  # Score model ----
  if(Debug) print('Scoring Here 7')
  if(TargetType %chin% c('regression', 'multiregression')) {
    predict <- data.table::as.data.table(
      catboost::catboost.predict(
        model = ModelObject,
        pool = ScoringPool,
        prediction_type = 'RawFormulaVal',
        thread_count = -1L))
  } else if(TargetType == 'classification') {
    predict <- data.table::as.data.table(
      catboost::catboost.predict(
        model = ModelObject,
        pool = ScoringPool,
        prediction_type = 'Probability',
        thread_count = -1L))
  } else if(TargetType == 'multiclass') {
    predict <- data.table::as.data.table(cbind(
      1 + catboost::catboost.predict(
        model = ModelObject,
        pool = ScoringPool,
        prediction_type = 'Class',
        thread_count = -1L),
      catboost::catboost.predict(
        model = ModelObject,
        pool = ScoringPool,
        prediction_type = 'Probability',
        thread_count = -1L)))
  }

  # Create ShapValues ----
  if(check1) {
    ShapValues <- data.table::as.data.table(catboost::catboost.get_feature_importance(ModelObject, pool = ScoringPool, type = 'ShapValues'))
    data.table::setnames(ShapValues, names(ShapValues), c(paste0('Shap_',FeatureColumnNames), 'Predictions'))
    ShapValues[, Predictions := NULL]
  }

  # Remove Model ----
  if(RemoveModel) rm(ModelObject)

  # MultiClass Mgt -----
  if(Debug) print('Scoring Here 8')
  if(TargetType == 'multiclass') {
    data.table::setnames(predict, 'V1', 'Predictions')
    if(is.null(MultiClassTargetLevels)) MultiClassTargetLevels <- data.table::fread(file.path(ModelPath, paste0(ModelID, '_TargetLevels.csv')))
    g <- as.character(MultiClassTargetLevels[[1L]])
    data.table::setnames(predict, names(predict)[names(predict) %like% "V"], g)
    predict <- merge(
      predict,
      MultiClassTargetLevels,
      by.x = 'Predictions',
      by.y = 'NewLevels',
      all = FALSE)
    predict[, Predictions := OriginalLevels][, OriginalLevels := NULL]
  }

  # Rename predicted value ----
  if(Debug) print('Scoring Here 9')
  if(TargetType == 'regression') {
    data.table::setnames(predict, 'V1', 'Predictions')
  } else if(TargetType == 'multiregression') {
    data.table::setnames(predict, paste0('V',seq_along(predict)), paste0('Predictions.V', seq_along(predict)))
  } else if(TargetType == 'classification') {
    data.table::setnames(predict, 'V1', 'p1')
  }

  # Merge features back on ----
  if(ReturnFeatures && TargetType != 'multiclass') {
    if(Debug) {
      print('Scoring Here 10')
      print(predict[, .N])
      print(ScoringMerge[, .N])
      print(ReturnFeatures && TargetType != 'multiclass')
      print(predict)
      print(ScoringMerge)
      print('HERE 1')
      print(length(predict[[1L]]))
    }
    if(tolower(TargetType) == 'classification') {
      ScoringMerge[, p1 := predict[[1L]]]
    } else {
      ScoringMerge[, Predict := predict[[1L]]]
    }
    if(Debug) print('HERE 2')
    predict <- ScoringMerge
    if(Debug) print('HERE 3')
    data.table::setcolorder(predict, c(ncol(predict), 1L:(ncol(predict)-1L)))
  }

  if(Debug) print('HERE 4')

  # Back Transform Numeric Variables ----
  if(BackTransNumeric && TargetType != 'multiregression') {

    if(Debug) print('Scoring Here 11')

    grid_trans_results <- data.table::copy(TransformationObject)
    data.table::set(grid_trans_results, i = which(grid_trans_results[['ColumnName']] == eval(TargetColumnName)), j = 'ColumnName', value = 'Predictions')
    grid_trans_results <- grid_trans_results[ColumnName != eval(TargetColumnName)]

    if(Debug) print('Scoring Here 12')

    # Run Back-Transform ----
    predict <- Rodeo::AutoTransformationScore(
      ScoringData = predict,
      Type = 'Inverse',
      FinalResults = grid_trans_results,
      TransID = NULL,
      Path = NULL)

    print('Scoring Here 13')

  } else if(BackTransNumeric && TargetType == 'multiregression') {

    if(Debug) print('Scoring Here 11.b')

    # Prepare transformation object
    TransformationObject <- data.table::rbindlist(list(TransformationObject, TransformationObject))
    for(z in seq_along(TransformationObject)) TransformationObject[length(TargetColumnName.) + z, ColumnName := paste0('Predict.V',z)]

    if(Debug) print('Scoring Here 12.b')

    # Back transform
    predict <- Rodeo::AutoTransformationScore(
      ScoringData = predict,
      Type = 'Inverse',
      FinalResults = TransformationObject,
      TransID = NULL,
      Path = NULL)

    if(Debug) print('Scoring Here 13.b')
  }

  # Return data ----
  if(Debug) print("HERE 5")
  if(Debug) print(check1)
  if(check1) {
    if(Debug) {
      print('Scoring Here 14.a')
      print(predict[, .N])
      print(ShapValues[, .N])
    }
    return(cbind(predict, ShapValues))
  } else {
    return(predict)
  }
}
