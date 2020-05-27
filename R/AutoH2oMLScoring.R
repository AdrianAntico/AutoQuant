#' AutoH2OMLScoring is an automated scoring function that compliments the AutoH2o model training functions.
#'
#' AutoH2OMLScoring is an automated scoring function that compliments the AutoH2oGBM__() and AutoH2oDRF__() models training functions. This function requires you to supply features for scoring. It will run ModelDataPrep()to prepare your features for H2O data conversion and scoring.
#'
#' @author Adrian Antico
#' @family Automated Model Scoring
#' @param ScoringData This is your data.table of features for scoring. Can be a single row or batch.
#' @param ModelObject Supply a model object from AutoH2oDRF__()
#' @param ModelType Set to either "mojo" or "standard" depending on which version you saved
#' @param H2OShutdown Set to TRUE is you are scoring a "standard" model file and you aren't planning on continuing to score.
#' @param MaxMem Set to you dedicated amount of memory. E.g. "28G"
#' @param NThreads Default set to max(1, parallel::detectCores()-2)
#' @param JavaOptions Change the default to your machines specification if needed. Default is '-Xmx1g -XX:ReservedCodeCacheSize=256m',
#' @param ModelPath Supply your path file used in the AutoH2o__() function
#' @param ModelID Supply the model ID used in the AutoH2o__() function
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
#' \donttest{
#' Preds <- AutoH2OMLScoring(ScoringData = data,
#'                           ModelObject = NULL,
#'                           ModelType = "mojo",
#'                           H2OShutdown = TRUE,
#'                           MaxMem = "28G",
#'                           NThreads = max(1, parallel::detectCores()-2),
#'                           JavaOptions = '-Xmx1g -XX:ReservedCodeCacheSize=256m',
#'                           ModelPath = normalizePath("./"),
#'                           ModelID = "ModelTest",
#'                           ReturnFeatures = TRUE,
#'                           TransformNumeric = FALSE,
#'                           BackTransNumeric = FALSE,
#'                           TargetColumnName = NULL,
#'                           TransformationObject = NULL,
#'                           TransID = NULL,
#'                           TransPath = NULL,
#'                           MDP_Impute = TRUE,
#'                           MDP_CharToFactor = TRUE,
#'                           MDP_RemoveDates = TRUE,
#'                           MDP_MissFactor = "0",
#'                           MDP_MissNum = -1)
#' }
#' @return A data.table of predicted values with the option to return model features as well.
#' @export
AutoH2OMLScoring <- function(ScoringData = NULL,
                             ModelObject = NULL,
                             ModelType = "mojo",
                             H2OShutdown = TRUE,
                             MaxMem = "28G",
                             NThreads = max(1, parallel::detectCores()-2),
                             JavaOptions = '-Xmx1g -XX:ReservedCodeCacheSize=256m',
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
  
  # Turn on full speed ahead----
  data.table::setDTthreads(threads = max(1L, parallel::detectCores()-2L))
  
  # Check arguments----
  if(is.null(ScoringData)) return("ScoringData cannot be NULL")
  if(!data.table::is.data.table(ScoringData)) data.table::setDT(ScoringData)
  if(!is.logical(MDP_Impute)) return("MDP_Impute (ModelDataPrep) should be TRUE or FALSE")
  if(!is.logical(MDP_CharToFactor)) return("MDP_CharToFactor (ModelDataPrep) should be TRUE or FALSE")
  if(!is.logical(MDP_RemoveDates)) return("MDP_RemoveDates (ModelDataPrep) should be TRUE or FALSE")
  if(!is.character(MDP_MissFactor) & !is.factor(MDP_MissFactor)) return("MDP_MissFactor should be a character or factor value")
  if(!is.numeric(MDP_MissNum)) return("MDP_MissNum should be a numeric or integer value")

  # Pull In Transformation Object----
  if(is.null(TransformationObject)) {
    if(TransformNumeric | BackTransNumeric) {
      if(is.null(TargetColumnName)) return("TargetColumnName needs to be supplied")
      TransformationObject <- data.table::fread(file.path(normalizePath(TransPath), paste0(TransID, "_transformation.csv")))
    }
  }

  # Apply Transform Numeric Variables----
  if(!is.null(TransformationObject)) {
    if(TransformNumeric | BackTransNumeric) {
      tempTrans <- data.table::copy(TransformationObject)
      tempTrans <- tempTrans[ColumnName != eval(TargetColumnName)]
      ScoringData <- AutoTransformationScore(
        ScoringData = ScoringData,
        FinalResults = tempTrans,
        Type = "Apply",
        TransID = TransID,
        Path = TransPath)
    }
  }
  
  # Initialize H2O----
  if(tolower(ModelType) == "standard") h2o::h2o.init(max_mem_size = MaxMem, nthreads = NThreads, enable_assertions = FALSE)
  
  # ModelDataPrep Check----
  ScoringData <- ModelDataPrep(
    data = ScoringData,
    Impute = MDP_Impute,
    CharToFactor = MDP_CharToFactor,
    RemoveDates = MDP_RemoveDates,
    MissFactor = MDP_MissFactor,
    MissNum = MDP_MissNum)
  
  # Initialize H2O Data Conversion----
  if(!is.null(ModelType)) {
    if(tolower(ModelType) != "mojo" | !is.null(ModelObject)) {
      ScoreData <- h2o::as.h2o(ScoringData)
    } else {
      ScoreData <- ScoringData
    }    
  }

  # Make Predictions----
  if(!is.null(ModelObject)) {
    predict <- data.table::as.data.table(h2o::h2o.predict(ModelObject, newdata = ScoreData))
  } else {
    if(tolower(ModelType) == "mojo") {
      predict <- data.table::as.data.table(
        h2o::h2o.mojo_predict_df(
          frame = ScoreData,
          mojo_zip_path = file.path(normalizePath(ModelPath), paste0(ModelID, ".zip")),
          genmodel_jar_path = file.path(normalizePath(ModelPath), ModelID),
          java_options = JavaOptions))
      
    } else if(tolower(ModelType) == "standard") {
      model <- h2o::h2o.loadModel(path = file.path(normalizePath(ModelPath), ModelID))
      predict <- data.table::as.data.table(h2o::h2o.predict(object = model, newdata = ScoreData))
    }    
  }

  # Change column name----
  data.table::setnames(predict, "predict", "Predictions")
  
  # Shut down H2O----
  if(tolower(ModelType) != "mojo") if(H2OShutdown) h2o::h2o.shutdown(prompt = FALSE)
  
  # Merge features back on----
  if(ReturnFeatures) predict <- cbind(predict, ScoringData)
  
  # Back Transform Numeric Variables----
  if(BackTransNumeric) {
    
    # Make copy of TransformationResults----
    grid_trans_results <- data.table::copy(TransformationObject)
    
    # Append record for Predicted Column----
    data.table::set(
      grid_trans_results,
      i = which(grid_trans_results[["ColumnName"]] == eval(TargetColumnName)),
      j = "ColumnName",
      value = "Predictions")
    
    # Remove target variable----
    grid_trans_results <- grid_trans_results[ColumnName != eval(TargetColumnName)]
    
    # Run Back-Transform----
    predict <- AutoTransformationScore(
      ScoringData = predict,
      Type = "Inverse",
      FinalResults = grid_trans_results,
      TransID = NULL,
      Path = NULL)
  }
  
  # Return data----
  return(predict)
}
