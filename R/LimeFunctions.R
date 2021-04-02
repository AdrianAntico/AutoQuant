#' LimeModel to build a lime model
#'
#' LimeModel to build a lime model for prediction explanations in this package#'
#' @author Adrian Antico
#' @family Model Evaluation and Interpretation
#' @param data Supply a training data set. This data set should be the data right before it gets converted to an h2o, catboost, or xgboost data object.
#' @param Model Supply the model returned from training with the Auto__() functions.
#' @param Bins Number of bins for discretizing numeric features
#' @param ModelType Select from xgboost, h2o, and catboost
#' @param NThreads Number of CPU threads
#' @param MaxMem For use with H2O models. E.g. set to "28G"
#' @param ModelPath Set to the path where your ML model is saved
#' @param ModelID ID used to identify your ML model
#' @return Model for utilizing lime
#' @noRd
LimeModel <- function(data,
                      Model = NULL,
                      Bins = 10,
                      ModelType = "xgboost",
                      NThreads = parallel::detectCores(),
                      MaxMem = "32G",
                      ModelPath = NULL,
                      ModelID = NULL) {

  # XGBoost----
  if(tolower(ModelType) == "xgboost") {
    return(
      if(Bins > 0) {
        tryCatch({lime::lime(data, Model, n_bins = Bins)},
                 error = function(x) {
                   tryCatch({lime::lime(data, Model)},
                            error = function(x) NULL)})
      } else {
        tryCatch({lime::lime(data, Model)},
                 error = function(x) {
                   tryCatch({lime::lime(data, Model, n_bins = Bins)},
                            error = function(x) NULL)})
      })
  }

  # H2O----
  if(tolower(ModelType) == "h2o") {
    h2o::h2o.init(startH2O = TRUE, enable_assertions = FALSE, max_mem_size = MaxMem, nthreads = NThreads)
    Model <- h2o::h2o.loadModel(path = paste0(ModelPath, "/", ModelID))
    return(
      if(Bins > 0) {
        tryCatch({lime::lime(data, Model, n_bins = Bins)},
                 error = function(x) {
                   tryCatch({lime::lime(data, Model)},
                            error = function(x) NULL)})
      } else {
        tryCatch({lime::lime(data, Model)},
                 error = function(x) {
                   tryCatch({lime::lime(data, Model, n_bins = Bins)},
                            error = function(x) NULL)})
      })
  }

  # CatBoost----
  if(tolower(ModelType) == "catboost") {
    return(
      if(Bins > 0) {
        tryCatch({lime::lime(data, Model, n_bins = Bins)},
                 error = function(x) {
                   tryCatch({lime::lime(data, Model)},
                            error = function(x) NULL)})
      } else {
        tryCatch({lime::lime(data, Model)},
                 error = function(x) {
                   tryCatch({lime::lime(data, Model, n_bins = Bins)},
                            error = function(x) NULL)})
      })
  }
}

#' AutoLimeAid automated lime
#'
#' AutoLimeAid automated lime explanations and lime model builds.
#'
#' @author Adrian Antico
#' @family Model Evaluation and Interpretation
#' @param EvalPredsData Data used for interpretation. Should be the same kind of data used on ML_Scoring functions.
#' @param LimeTrainingData Data used to train your ML model
#' @param LimeBins Number of bins to use for bucketing numeric variables
#' @param LimeIterations Number of lime permutations ran to generate interpretation of predicted value
#' @param LimeNumFeatures How many features do you want to be considering for the Lime evaluation? Set to 0 to use all features
#' @param LimeModel Supply a model if you have one available. Otherwise, provide a model path and either it will be pulling in or made and saved there.
#' @param LimeModelPath Supply a path to where your model is located or to be stored.
#' @param LimeModelID Provide a name for your model. If left NULL, a name will be created for you (and a new model).
#' @param MLModel Supply the model object (except for H2O models). Can leave null.
#' @param MLModelPath Supply a path to where your model is located. If this is supplied, the model will be pulled in from file (even if you supply a model)
#' @param MLMetaDataPath Supply a path to where your model metadata is located (might be the same of the MLModelPath). If this is supplied, artifacts about the model will be pulled in from there.
#' @param MLModelID The name of your model as read in the file directory
#' @param ModelType Choose from "xgboost", "h2o", "catboost"
#' @param TargetType For catboost models only. Select from "classification", "regression", "multiclass"
#' @param NThreads Number of CPU threads.
#' @param MaxMem Set the max memory you want to allocate. E.g. "32G"
#' @param FeatureColumnNames The names of the features used in training your ML model (should be returned with the model or saved to file)
#' @param IDcols The ID columns used in either CatBoost or XGBoost
#' @param FactorLevelsList = TestModel$FactorLevels,
#' @param TargetLevels The target levels used in MultiClass models
#' @param OneHot Replicate what you did with the model training
#' @param ReturnFeatures TRUE or FALSE
#' @param TransformNumeric Replicate what you did with the model training
#' @param BackTransNumeric TRUE or FALSE. Replicate what you did with the model training.
#' @param TargetColumnName For the transformations
#' @param TransformationObject TRUE or FALSE. Replicate what you did with the model training.
#' @param TransID Set to the ID used in model training.
#' @param TransPath Same path used in model training.
#' @param MDP_Impute Replicate what you did with the model training.
#' @param MDP_CharToFactor Replicate what you did with the model training.
#' @param MDP_RemoveDates Replicate what you did with the model training.
#' @param MDP_MissFactor Replicate what you did with the model training.
#' @param MDP_MissNum Replicate what you did with the model training.
#' @return LimeModelObject and Lime Explanations
#' @examples
#' \dontrun{
#' # CatBoost data generator
#' dataGenH2O <- function() {
#'   Correl <- 0.85
#'   N <- 10000
#'   data <- data.table::data.table(Classification = runif(N))
#'   data[, x1 := qnorm(Classification)]
#'   data[, x2 := runif(N)]
#'   data[, Independent_Variable1 := log(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
#'   data[, Independent_Variable2 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
#'   data[, Independent_Variable3 := exp(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
#'   data[, Independent_Variable4 := exp(exp(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2))))]
#'   data[, Independent_Variable5 := sqrt(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
#'   data[, Independent_Variable6 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^0.10]
#'   data[, Independent_Variable7 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^0.25]
#'   data[, Independent_Variable8 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^0.75]
#'   data[, Independent_Variable9 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^2]
#'   data[, Independent_Variable10 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^4]
#'   data[, Independent_Variable11 := as.factor(
#'     ifelse(Independent_Variable2 < 0.20,
#'     "A",ifelse(Independent_Variable2 < 0.40,
#'     "B",ifelse(Independent_Variable2 < 0.6,
#'     "C",ifelse(Independent_Variable2 < 0.8,  "D", "E")))))]
#'   data[, ':=' (x1 = NULL, x2 = NULL)]
#'   data[, Classification := ifelse(Classification > 0.5, 1, 0)]
#'   rm(N,Correl)
#'   return(data)
#' }
#' data <- dataGenH2O()
#' TestModel <- RemixAutoML::AutoCatBoostRegression(
#'   data,
#'   TrainOnFull = FALSE,
#'   ValidationData = NULL,
#'   TestData = NULL,
#'   TargetColumnName = "Classification",
#'   FeatureColNames = c(2:12),
#'   PrimaryDateColumn = NULL,
#'   IDcols = NULL,
#'   MaxModelsInGrid = 3,
#'   task_type = "GPU",
#'   eval_metric = "RMSE",
#'   Trees = 50,
#'   GridTune = FALSE,
#'   model_path = "C:/Users/aantico/Documents/Package/GUI_Package",
#'   metadata_path = NULL,
#'   ModelID = "Adrian",
#'   NumOfParDepPlots = 15,
#'   ReturnModelObjects = TRUE,
#'   SaveModelObjects = TRUE,
#'   PassInGrid = NULL)
#'
#' # CatBoost Build Lime Model and Explanations
#' LimeOutput <- RemixAutoML::AutoLimeAid(
#'   EvalPredsData = data[c(1,15)],
#'   LimeTrainingData = data,
#'   LimeBins = 10,
#'   LimeIterations = 7500,
#'   LimeNumFeatures = 0,
#'   TargetType = "regression",
#'   LimeModel = NULL,
#'   LimeModelPath = "C:/Users/aantico/Documents/Package/GUI_Package",
#'   LimeModelID = "AdrianLime",
#'   MLModel = NULL,
#'   MLModelPath = "C:/Users/aantico/Documents/Package/GUI_Package",
#'   MLMetaDataPath = NULL,
#'   MLModelID = "Adrian",
#'   ModelType = "catboost",
#'   NThreads = parallel::detectCores(),
#'   MaxMem = "14G",
#'   FeatureColumnNames = NULL,
#'   IDcols = NULL,
#'   FactorLevelsList = NULL,
#'   TargetLevels = NULL,
#'   OneHot = FALSE,
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
#'
#' # Plot lime objects
#' lime::plot_features(LimeOutput$LimeExplanations)
#' suppressWarnings(lime::plot_explanations(LimeOutput$LimeExplanations))
#'
#' # H2O data generator
#' dataGenH2O <- function() {
#'   Correl <- 0.85
#'   N <- 10000
#'   data <- data.table::data.table(Classification = runif(N))
#'   data[, x1 := qnorm(Classification)]
#'   data[, x2 := runif(N)]
#'   data[, Independent_Variable1 := log(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
#'   data[, Independent_Variable2 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
#'   data[, Independent_Variable3 := exp(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
#'   data[, Independent_Variable4 := exp(exp(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2))))]
#'   data[, Independent_Variable5 := sqrt(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
#'   data[, Independent_Variable6 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^0.10]
#'   data[, Independent_Variable7 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^0.25]
#'   data[, Independent_Variable8 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^0.75]
#'   data[, Independent_Variable9 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^2]
#'   data[, Independent_Variable10 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^4]
#'   data[, Independent_Variable11 := as.factor(ifelse(Independent_Variable2 < 0.20,
#'     "A",ifelse(Independent_Variable2 < 0.40,
#'     "B",ifelse(Independent_Variable2 < 0.6,
#'     "C",ifelse(Independent_Variable2 < 0.8,  "D", "E")))))]
#'   data[, ':=' (x1 = NULL, x2 = NULL)]
#'   data[, Classification := ifelse(Classification > 0.5, 1, 0)]
#'   rm(N,Correl)
#'   return(data)
#' }
#' data <- dataGenH2O()
#' TestModel <- RemixAutoML::AutoH2oDRFClassifier(
#'   data = data,
#'   TrainOnFull = FALSE,
#'   ValidationData = NULL,
#'   TestData = NULL,
#'   TargetColumnName = "Classification",
#'   FeatureColNames = setdiff(names(data),"Classification"),
#'   eval_metric = "auc",
#'   Trees = 50,
#'   GridTune = FALSE,
#'   MaxMem = "32G",
#'   NThreads = max(1, parallel::detectCores()-2),
#'   MaxModelsInGrid = 10,
#'   model_path = "C:/Users/aantico/Desktop/Retention Analytics",
#'   metadata_path = NULL,
#'   ModelID = "Adrian",
#'   NumOfParDepPlots = 10,
#'   ReturnModelObjects = TRUE,
#'   SaveModelObjects = TRUE,
#'   IfSaveModel = "standard",
#'   H2OShutdown = TRUE)
#'
# H2O Build Lime Model and Explanations
#' LimeOutput <- RemixAutoML::AutoLimeAid(
#'   EvalPredsData = data[c(1,15)],
#'   LimeTrainingData = data,
#'   LimeBins = 10,
#'   LimeIterations = 7500,
#'   TargetType = "regression",
#'   LimeNumFeatures = 0,
#'   LimeModel = NULL,
#'   LimeModelPath = "C:/Users/aantico/Desktop/Retention Analytics",
#'   LimeModelID = "AdrianLime",
#'   MLModel = NULL,
#'   MLModelPath = "C:/Users/aantico/Desktop/Retention Analytics",
#'   MLMetaDataPath = NULL,
#'   MLModelID = "Adrian",
#'   ModelType = "h2o",
#'   NThreads = parallel::detectCores(),
#'   MaxMem = "14G",
#'   FeatureColumnNames = NULL,
#'   IDcols = NULL,
#'   FactorLevelsList = NULL,
#'   TargetLevels = NULL,
#'   OneHot = FALSE,
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
#'
#' # Plot lime objects
#' lime::plot_features(LimeOutput$LimeExplanations)
#' suppressWarnings(lime::plot_explanations(LimeOutput$LimeExplanations))
#'
#' # XGBoost create data function
#' dataGenXGBoost <- function() {
#'   Correl <- 0.85
#'   N <- 10000
#'   data <- data.table::data.table(Classification = runif(N))
#'   data[, x1 := qnorm(Classification)]
#'   data[, x2 := runif(N)]
#'   data[, Independent_Variable1 := log(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
#'   data[, Independent_Variable2 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
#'   data[, Independent_Variable3 := exp(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
#'   data[, Independent_Variable4 := exp(exp(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2))))]
#'   data[, Independent_Variable5 := sqrt(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
#'   data[, Independent_Variable6 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^0.10]
#'   data[, Independent_Variable7 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^0.25]
#'   data[, Independent_Variable8 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^0.75]
#'   data[, Independent_Variable9 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^2]
#'   data[, Independent_Variable10 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^4]
#'   data[, Independent_Variable11 := as.factor(ifelse(Independent_Variable2 < 0.20,
#'     "A",ifelse(Independent_Variable2 < 0.40,
#'     "B",ifelse(Independent_Variable2 < 0.6,
#'     "C",ifelse(Independent_Variable2 < 0.8,  "D", "E")))))]
#'   data[, ':=' (x1 = NULL, x2 = NULL)]
#'   data[, Classification := ifelse(Classification > 0.5, 1, 0)]
#'   rm(Correl,N)
#'   return(data)
#' }
#' data <- dataGenXGBoost()
#' TestModel <- RemixAutoML::AutoXGBoostClassifier(
#'   data,
#'   TrainOnFull = FALSE,
#'   ValidationData = NULL,
#'   TestData = NULL,
#'   TargetColumnName = "Classification",
#'   FeatureColNames = 2:12,
#'   IDcols = NULL,
#'   eval_metric = "auc",
#'   Trees = 50,
#'   GridTune = FALSE,
#'   grid_eval_metric = "auc",
#'   MaxModelsInGrid = 10,
#'   NThreads = 8,
#'   TreeMethod = "hist",
#'   model_path = "C:/Users/aantico/Desktop/Retention Analytics",
#'   metadata_path = NULL,
#'   ModelID = "Adrian2",
#'   NumOfParDepPlots = 3,
#'   ReturnModelObjects = TRUE,
#'   ReturnFactorLevels = TRUE,
#'   SaveModelObjects = TRUE,
#'   PassInGrid = NULL)
#'
#' # XGBoost Build Lime and Generate Output
#' LimeOutput <- RemixAutoML::AutoLimeAid(
#'   EvalPredsData = data[c(1,15)],
#'   LimeTrainingData = data,
#'   LimeBins = 10,
#'   TargetType = "classification",
#'   LimeIterations = 7500,
#'   LimeNumFeatures = 0,
#'   LimeModel = NULL,
#'   LimeModelPath = "C:/Users/aantico/Desktop/Retention Analytics",
#'   LimeModelID = "Adrian2Lime",
#'   MLModel = NULL,
#'   MLModelPath = "C:/Users/aantico/Desktop/Retention Analytics",
#'   MLMetaDataPath = NULL,
#'   MLModelID = "Adrian2",
#'   ModelType = "xgboost",
#'   NThreads = parallel::detectCores(),
#'   MaxMem = "14G",
#'   FeatureColumnNames = NULL,
#'   IDcols = NULL,
#'   FactorLevelsList = NULL,
#'   TargetLevels = NULL,
#'   OneHot = FALSE,
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
#'
#' # Plot lime objects
#' lime::plot_features(LimeOutput$LimeExplanations)
#' suppressWarnings(lime::plot_explanations(LimeOutput$LimeExplanations))
#' }
#' @noRd
AutoLimeAid <- function(EvalPredsData = data,
                        LimeTrainingData = data,
                        LimeBins = 10,
                        LimeIterations = 7500,
                        LimeNumFeatures = 0,
                        LimeModel = NULL,
                        LimeModelPath = NULL,
                        LimeModelID = NULL,
                        MLModel = NULL,
                        MLModelPath = NULL,
                        MLMetaDataPath = NULL,
                        MLModelID = NULL,
                        ModelType = "xgboost",
                        TargetType = "classification",
                        NThreads = parallel::detectCores(),
                        MaxMem = "32G",
                        FeatureColumnNames = TestModel$ColNames,
                        IDcols = NULL,
                        FactorLevelsList = TestModel$FactorLevels,
                        TargetLevels = NULL,
                        OneHot = FALSE,
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

  # Check Lime Arguments----
  if(is.null(EvalPredsData) & is.null(LimeTrainingData)) return("EvalPredsData and LimeTrainingData cannot both be NULL.")
  if(is.null(LimeModelPath)) {
    if(is.null(LimeModel)) return("You need to either supply a model or a path to a model if you aren't supplying training data to build a model")
    if(is.null(LimeTrainingData)) return("You need to supply LimeTrainingData to train your lime model.")
  } else {
    if(!dir.exists(LimeModelPath)) {
      if(is.null(LimeTrainingData)) return("Not creating directory: There is no lime training data.") else dir.create(LimeModelPath)
    } else if(is.null(LimeTrainingData)) {
      if(is.null(LimeModelID)) return("I need your LimeModelID so I know which file to load.")
      if(!file.exists(file.path(LimeModelPath,LimeModelID))) {
        return(paste0("Directory exists but the file named LimeModelID (",LimeModelID,") is not located there."))
      } else {
        load(file.path(LimeModelPath,LimeModelID))
        LimeModel <- LimeAId
      }
    }
  }
  if(is.null(LimeModel)) BuildLimeModel <- TRUE else BuildLimeModel <- FALSE

  # Check ML Model Arguments----
  if(is.null(MLModel)) {
    if(is.null(MLModelPath)) return("I need a model supplied (unless the model is by H2O) or a pathfile to go find your model")
    if(!dir.exists(MLModelPath)) return(paste0("MLModelPath (",MLModelPath,") directory does not exist"))
    if(is.null(MLModelID)) return("I need your ModelID so I know which file to load.")
    if(!file.exists(file.path(MLModelPath,MLModelID))) return(paste0("Directory exists but the file named MLModelID (",MLModelID,") is not located there."))
  } else {
    if(tolower(ModelType) == "h2o") return("You need to save your H2O model and have it pulled in with a MLModelPath and MLModelID")
    if(file.exists(file.path(MLModelPath,MLModelID,"_ColNames"))) FeatureColumnNames <- data.table::fread(file.path(MLModelPath,MLModelID,"_ColNames"))
  }
  if(!is.null(MLMetaDataPath)) {
    if(!dir.exists(MLMetaDataPath)) return("The MLMetaDataPath directory does not exist")
    if(file.exists(file.path(MLMetaDataPath,MLModelID,"_ColNames"))) FeatureColumnNames <- data.table::fread(file.path(MLMetaDataPath,MLModelID,"_ColNames"))
  }

  # Check Data Arguments----
  if(is.null(FeatureColumnNames)) {
    if(is.null(MLModelPath)) return("MLModelPath cannot be NULL if you are not supply FeatureColumnNames")
    FeatureColumnNames <- data.table::fread(file.path(MLModelPath,paste0(MLModelID,"_ColNames.csv")))
  }
  if(!is.character(FeatureColumnNames)) FeatureColumnNames <- as.character(FeatureColumnNames[[1L]])
  if(!data.table::is.data.table(EvalPredsData)) EvalPredsData <- data.table::as.data.table(EvalPredsData)
  if(!is.logical(MDP_Impute)) return("MDP_Impute (ModelDataPrep) should be TRUE or FALSE")
  if(!is.logical(MDP_CharToFactor)) return("MDP_CharToFactor (ModelDataPrep) should be TRUE or FALSE")
  if(!is.logical(MDP_RemoveDates)) return("MDP_RemoveDates (ModelDataPrep) should be TRUE or FALSE")
  if(!is.character(MDP_MissFactor) & !is.factor(MDP_MissFactor)) return("MDP_MissFactor should be a character or factor value")
  if(!is.numeric(MDP_MissNum)) return("MDP_MissNum should be a numeric or integer value")

  # LimeNumFeatures == 0 means use all features----
  if(LimeNumFeatures == 0) NumFeatures <- length(FeatureColumnNames) else NumFeatures <- LimeNumFeatures

  # IDcols conversion----
  if(is.numeric(IDcols) | is.integer(IDcols)) IDcols <- names(data)[IDcols]

  # Apply Transform Numeric Variables----
  if(TransformNumeric) {
    if(!is.null(TransformationObject)) {
      tempTrans <- data.table::copy(TransformationObject)
      tempTrans <- tempTrans[ColumnName != eval(TargetColumnName)]
      EvalPredsData <- AutoTransformationScore(
        ScoringData = EvalPredsData,
        FinalResults = tempTrans,
        Type = "Apply",
        TransID = TransID,
        Path = NULL)
      if(!is.null(LimeTrainingData)) {
        LimeTrainingData <- AutoTransformationScore(
          ScoringData = LimeTrainingData,
          FinalResults = tempTrans,
          Type = "Apply",
          TransID = TransID,
          Path = NULL)
      }
    } else {
      EvalPredsData <- AutoTransformationScore(
        ScoringData = EvalPredsData,
        FinalResults = tempTrans,
        Type = "Apply",
        TransID = TransID,
        Path = TransPath)
      if(!is.null(LimeTrainingData)) {
        LimeTrainingData <- AutoTransformationScore(
          ScoringData = LimeTrainingData,
          FinalResults = tempTrans,
          Type = "Apply",
          TransID = TransID,
          Path = TransPath)
      }
    }
  }

  # Binary Identify column numbers for factor variables----
  if(ModelType != "h2o") {
    CatFeatures <- sort(c(as.numeric(which(sapply(EvalPredsData, is.factor))), as.numeric(which(sapply(EvalPredsData, is.character)))))
    CatFeatures <- names(EvalPredsData)[CatFeatures]
  }

  # Subset Columns Needed----
  if(is.numeric(FeatureColumnNames) | is.integer(FeatureColumnNames)) {
    keep1 <- names(EvalPredsData)[c(FeatureColumnNames)]
    if(!is.null(IDcols)) keep <- c(IDcols, keep1) else keep <- c(keep1)
    EvalPredsData <- EvalPredsData[, ..keep]
    if(!is.null(LimeTrainingData)) LimeTrainingData <- LimeTrainingData[, ..keep]
  } else {
    keep1 <- c(FeatureColumnNames)
    if (!is.null(IDcols)) keep <- c(IDcols, FeatureColumnNames) else keep <- c(FeatureColumnNames)
    EvalPredsData <- EvalPredsData[, ..keep]
    if(!is.null(LimeTrainingData)) LimeTrainingData <- LimeTrainingData[, ..keep]
  }
  if(!is.null(IDcols)) {
    ScoringMerge <- data.table::copy(EvalPredsData)
    keep <- c(keep1)
    EvalPredsData <- EvalPredsData[, ..keep]
    if(!is.null(LimeTrainingData)) LimeTrainingData <- LimeTrainingData[, ..keep]
  } else {
    ScoringMerge <- data.table::copy(EvalPredsData)
    if(!is.null(LimeTrainingData)) LimeTrainingData <- LimeTrainingData[, ..keep]
  }

  # DummifyDT categorical columns----
  if(tolower(ModelType) == "xgboost") {
    if(!is.null(CatFeatures)) {
      if(!is.null(FactorLevelsList)) {
        EvalPredsData <- DummifyDT(
          data = EvalPredsData,
          cols = CatFeatures,
          KeepFactorCols = FALSE,
          OneHot = OneHot,
          SaveFactorLevels = FALSE,
          SavePath = MLModelPath,
          ImportFactorLevels = FALSE,
          FactorLevelsList = FactorLevelsList,
          ReturnFactorLevels = FALSE,
          ClustScore = FALSE)
        if(!is.null(LimeTrainingData)) {
          LimeTrainingData <- DummifyDT(
            data = LimeTrainingData,
            cols = CatFeatures,
            KeepFactorCols = FALSE,
            OneHot = OneHot,
            SaveFactorLevels = FALSE,
            SavePath = MLModelPath,
            ImportFactorLevels = FALSE,
            FactorLevelsList = FactorLevelsList,
            ReturnFactorLevels = FALSE,
            ClustScore = FALSE)
        }
      } else {
        EvalPredsData <- DummifyDT(
          data = EvalPredsData,
          cols = CatFeatures,
          KeepFactorCols = FALSE,
          OneHot = OneHot,
          SaveFactorLevels = FALSE,
          SavePath = MLModelPath,
          ImportFactorLevels = TRUE,
          ReturnFactorLevels = FALSE,
          ClustScore = FALSE)
        if(!is.null(LimeTrainingData)) {
          LimeTrainingData <- DummifyDT(
            data = LimeTrainingData,
            cols = CatFeatures,
            KeepFactorCols = FALSE,
            OneHot = OneHot,
            SaveFactorLevels = FALSE,
            SavePath = MLModelPath,
            ImportFactorLevels = TRUE,
            ReturnFactorLevels = FALSE,
            ClustScore = FALSE)
        }
      }
    }
  }

  # ModelDataPrep Check----
  EvalPredsData <- ModelDataPrep(
    data = EvalPredsData,
    Impute = MDP_Impute,
    CharToFactor = MDP_CharToFactor,
    RemoveDates = MDP_RemoveDates,
    MissFactor = MDP_MissFactor,
    MissNum = MDP_MissNum)
  if(!is.null(LimeTrainingData)) {
    LimeTrainingData <- ModelDataPrep(
      data = LimeTrainingData,
      Impute = MDP_Impute,
      CharToFactor = MDP_CharToFactor,
      RemoveDates = MDP_RemoveDates,
      MissFactor = MDP_MissFactor,
      MissNum = MDP_MissNum)
  }

  # CatBoost Functions----
  if(tolower(ModelType) == "catboost") {
    loadNamespace(package = "catboost")
    if(tolower(TargetType) == "classification") {
      model_type.catboost.Model <<- function(x, ...) return("classification")
    } else if(tolower(TargetType) == "regression") {
      model_type.catboost.Model <<- function(x, ...) return("regression")
    } else if(tolower(TargetType) == "multiclass") {
      model_type.catboost.Model <<- function(x, ...) return("multilabel")
    }

    # Score model functions----
    predict <- function(x, ScoringPool, ...) {
      if(tolower(...) == "regression") {
        score <- data.table::as.data.table(
          catboost::catboost.predict(
            model = x,
            pool = ScoringPool,
            prediction_type = "RawFormulaVal",
            thread_count = -1))
      } else if(tolower(...) == "classification") {
        score <- data.table::as.data.table(
          catboost::catboost.predict(
            model = x,
            pool = ScoringPool,
            prediction_type = "Probability",
            thread_count = -1))
      } else if(tolower(...) == "multiclass") {
        score <- data.table::as.data.table(cbind(
          1 + catboost::catboost.predict(
            model = x,
            pool = ScoringPool,
            prediction_type = "Class"),
          catboost::catboost.predict(
            model = x,
            pool = ScoringPool,
            prediction_type = "Probability")))
      }
      return(score)
    }

    # Define CatBoost predict_model----
    if(tolower(TargetType) == "regression") {
      predict_model.catboost.Model <<- function(x, newdata, CatFeaturesX = CatFeatures, ...) {

        # Initialize Catboost Data Conversion----
        if(!is.null(CatFeaturesX)) {
          ScoringPool <- catboost::catboost.load_pool(newdata, cat_features = CatFeaturesX)
        } else {
          ScoringPool <- catboost::catboost.load_pool(newdata)
        }

        # Score model----
        score <- predict(x = MLModel, ScoringPool, TargetType)

        # Change Output Predictions Column Name----
        data.table::setnames(score, "V1", "Response")
        return(as.data.frame(score))
      }
    } else if(tolower(TargetType) == "classification") {
      predict_model.catboost.Model <<- function(x, newdata, TargetType = "regression", CatFeaturesX = CatFeatures, ...) {

        # Initialize Catboost Data Conversion----
        if(!is.null(CatFeaturesX)) {
          ScoringPool <- catboost::catboost.load_pool(newdata, cat_features = CatFeaturesX)
        } else {
          ScoringPool <- catboost::catboost.load_pool(newdata)
        }

        # Score model----
        score <- predict(x = MLModel, ScoringPool, TargetType)

        # Change Output Predictions Column Name----
        data.table::setnames(score, "V1", "1")
        data.table::set(score, j = "0", value = 1 - score[["1"]])
        return(as.data.frame(score))
      }
    } else {
      predict_model.catboost.Model <<- function(x, newdata, TargetType = "regression", CatFeaturesX = CatFeatures, ...) {

        # Initialize Catboost Data Conversion----
        if(!is.null(CatFeaturesX)) {
          ScoringPool <- catboost::catboost.load_pool(newdata, cat_features = CatFeaturesX)
        } else {
          ScoringPool <- catboost::catboost.load_pool(newdata)
        }

        # Score model----
        score <- predict(x = MLModel, ScoringPool, TargetType)

        # Change Output Predictions Column Name----
        if(!is.null(MultiClassTargetLevels)) {
          TargetLevels <- MultiClassTargetLevels
        } else {
          TargetLevels <- data.table::fread(file.path(normalizePath(ModelPath), paste0(ModelID, "_TargetLevels.csv")))
        }
        k <- 1
        for (name in as.character(TargetLevels[[1L]])) {
          k <- k + 1
          data.table::setnames(score, paste0("V", k), name)
        }
        data.table::setnames(score, "V1", "Predictions")
        score <- merge(
          score,
          TargetLevels,
          by.x = "Predictions",
          by.y = "NewLevels",
          all = FALSE)
        score[, Predictions := OriginalLevels][, OriginalLevels := NULL]
        return(as.data.frame(score))
      }
    }
  }

  # Lime Aid----
  if(is.null(LimeModel)) {

    # Check if LimeTrainingData exists
    if(is.null(LimeTrainingData)) return("Need to supply LimeTrainingData")

    # Check if base ML model is supplied or exists in file
    if(is.null(MLModel) | tolower(ModelType) == "h2o") {

      # Load MLModels----
      options(warn = -1)
      if(is.null(MLModel) | tolower(ModelType) == "h2o") {
        if(tolower(ModelType) == "h2o") {
          h2o::h2o.init(startH2O = TRUE, nthreads = NThreads, max_mem_size = MaxMem)
          Model <- h2o::h2o.loadModel(path = file.path(MLModelPath,MLModelID))
        } else if(tolower(ModelType) == "xgboost") {
          load(file.path(MLModelPath,MLModelID))
          MLModel <- model
        } else if(tolower(ModelType) == "catboost") {
          MLModel <- catboost::catboost.load_model(file.path(MLModelPath,MLModelID))
        }
      }

      # LimeModel will pull in model object
      LimeAId <- LimeModel(
        data = LimeTrainingData,
        Model = MLModel,
        Bins = LimeBins,
        ModelType = ModelType,
        ModelPath = MLModelPath,
        ModelID = MLModelID)

      # Shutdown H2O----
      if(!BuildLimeModel) if(tolower(ModelType) == "h2o") tryCatch({h2o::h2o.shutdown(prompt = FALSE)}, error = function(x) NULL)

      # Save LimeModel if Path Provided----
      save(LimeAId, file = file.path(normalizePath(LimeModelPath), LimeModelID))

      # Explain Predictions----
      if(!is.null(EvalPredsData)) {
        LimeExplanations <- tryCatch({lime::explain(
          x = data.table::setDF(EvalPredsData),
          explainer = LimeAId,
          n_permutations = LimeIterations,
          dist_fun = "gower",
          kernel_width = .75,
          n_features = NumFeatures,
          feature_select = "none",
          labels = "Yes")}, error = function(x) {
            LimeExplanations <- tryCatch({lime::explain(
              x = EvalPredsData,
              explainer = LimeModel,
              n_permutations = LimeIterations,
              dist_fun = "gower",
              kernel_width = .75,
              n_features = 2^ceiling(log(NumFeatures)/log(2)),
              feature_select = "tree",
              labels = "Yes")}, error = function(x) {
                LimeExplanations <- tryCatch({lime::explain(
                  x = EvalPredsData,
                  explainer = LimeModel,
                  n_permutations = LimeIterations,
                  dist_fun = "gower",
                  kernel_width = .75,
                  n_features = min(length(FeatureColumnNames),NumFeatures),
                  feature_select = "lasso_path",
                  labels = "Yes")}, error = function(x) {
                    LimeExplanations <- tryCatch({lime::explain(
                      x = EvalPredsData,
                      explainer = LimeModel,
                      n_permutations = LimeIterations,
                      dist_fun = "gower",
                      kernel_width = .75,
                      n_features = min(length(FeatureColumnNames),NumFeatures),
                      feature_select = "auto",
                      labels = "Yes")}, error = function(x) {
                        NULL
                      })
                  })
              })
          })
      }
    } else {

      # LimeModel will pull in model object
      LimeAId <- LimeModel(
        data = LimeTrainingData,
        Model = MLModel,
        Bins = LimeBins,
        ModelType = ModelType,
        ModelPath = MLModelPath,
        ModelID = MLModelID)

      # Shutdown H2O----
      if(!BuildLimeModel) if(tolower(ModelType) == "h2o") tryCatch({h2o::h2o.shutdown(prompt = FALSE)}, error = function(x) NULL)

      # Save LimeModel if Path Provided----
      save(LimeAId, file = file.path(normalizePath(LimeModelPath), LimeModelID))

      # Explain Predictions----
      if(!is.null(EvalPredsData)) {
        LimeExplanations <- tryCatch({lime::explain(
          x = EvalPredsData,
          explainer = LimeAId,
          n_permutations = LimeIterations,
          dist_fun = "gower",
          kernel_width = .75,
          n_features = NumFeatures,
          feature_select = "none",
          labels = "Yes")}, error = function(x) {
            LimeExplanations <- tryCatch({lime::explain(
              x = EvalPredsData,
              explainer = LimeModel,
              n_permutations = LimeIterations,
              dist_fun = "gower",
              kernel_width = .75,
              n_features = 2^ceiling(log(NumFeatures)/log(2)),
              feature_select = "tree",
              labels = "Yes")}, error = function(x) {
                LimeExplanations <- tryCatch({lime::explain(
                  x = EvalPredsData,
                  explainer = LimeModel,
                  n_permutations = LimeIterations,
                  dist_fun = "gower",
                  kernel_width = .75,
                  n_features = min(length(FeatureColumnNames),NumFeatures),
                  feature_select = "lasso_path",
                  labels = "Yes")}, error = function(x) {
                    LimeExplanations <- tryCatch({lime::explain(
                      x = EvalPredsData,
                      explainer = LimeModel,
                      n_permutations = LimeIterations,
                      dist_fun = "gower",
                      kernel_width = .75,
                      n_features = min(length(FeatureColumnNames),NumFeatures),
                      feature_select = "auto",
                      labels = "Yes")}, error = function(x) {
                        NULL
                      })
                  })
              })
          })
      }
    }
  } else {

    # Load ML Models----
    options(warn = -1)
    if(is.null(MLModel) | tolower(ModelType) == "h2o") {
      if(tolower(ModelType) == "h2o") {
        h2o::h2o.init(startH2O = TRUE, nthreads = NThreads, max_mem_size = MaxMem)
        Model <- h2o::h2o.loadModel(path = file.path(MLModelPath,MLModelID))
      } else if(tolower(ModelType) == "xgboost") {
        load(file.path(MLModelPath,MLModelID))
        Model <- model
      } else if(tolower(ModelType) == "catboost") {
        model <- catboost::catboost.load_model(file.path(MLModelPath,MLModelID))
      }
    }

    # Create Explanations----
    if(!is.null(EvalPredsData)) {
      LimeExplanations <- tryCatch({lime::explain(
        x = EvalPredsData,
        explainer = LimeModel,
        n_permutations = 7500,
        n_features = NumFeatures,
        dist_fun = "gower",
        kernel_width = .75,
        feature_select = "none",
        labels = "Yes")}, error = function(x) {
          LimeExplanations <- tryCatch({lime::explain(
            x = EvalPredsData,
            explainer = LimeModel,
            n_permutations = 7500,
            dist_fun = "gower",
            kernel_width = .75,
            n_features = 2^ceiling(log(NumFeatures)/log(2)),
            feature_select = "tree",
            labels = "Yes")}, error = function(x) {
              LimeExplanations <- tryCatch({lime::explain(
                x = EvalPredsData,
                explainer = LimeModel,
                n_permutations = 7500,
                dist_fun = "gower",
                kernel_width = .75,
                n_features = min(length(FeatureColumnNames),NumFeatures),
                feature_select = "lasso_path",
                labels = "Yes")}, error = function(x) {
                  LimeExplanations <- tryCatch({lime::explain(
                    x = EvalPredsData,
                    explainer = LimeModel,
                    n_permutations = 7500,
                    dist_fun = "gower",
                    kernel_width = .75,
                    n_features = min(length(FeatureColumnNames),NumFeatures),
                    feature_select = "auto",
                    labels = "Yes")}, error = function(x) {
                      NULL
                    })
                })
            })
        })
    }
  }

  # Convert warning back to original state----
  options(warn = 0, error = NULL)

  # Shutdown H2O----
  if(tolower(ModelType) == "h2o") tryCatch({h2o::h2o.shutdown(prompt = FALSE)}, error = function(x) NULL)

  # Return Lime Artifacts----
  if(!is.null(LimeModel)) {
    return(list(
      LimeModel = LimeModel,
      LimeExplanations = LimeExplanations))
  } else {
    return(list(
      LimeModel = LimeAId,
      LimeExplanations = LimeExplanations))
  }
}
