#' @title H2OAutoencoder
#'
#' @description H2OAutoencoder for anomaly detection and or dimensionality reduction
#'
#' @author Adrian Antico
#' @family Feature Engineering
#'
#' @param AnomalyDetection Set to TRUE to run anomaly detection
#' @param DimensionReduction Set to TRUE to run dimension reduction
#' @param data The data.table with the columns you wish to have analyzed
#' @param ValidationData The data.table with the columns you wish to have scored
#' @param Features NULL Column numbers or column names
#' @param RemoveFeatures Set to TRUE if you want the features you specify in the Features argument to be removed from the data returned
#' @param NThreads max(1L, parallel::detectCores()-2L)
#' @param MaxMem "28G"
#' @param LayerStructure If NULL, layers and sizes will be created for you, using NodeShrinkRate and 7 layers will be created.
#' @param NodeShrinkRate = (sqrt(5) - 1) / 2,
#' @param H2OStart TRUE to start H2O inside the function
#' @param H2OShutdown Setting to TRUE will shutdown H2O when it done being used internally.
#' @param ModelID "TestModel"
#' @param model_path If NULL no model will be saved. If a valid path is supplied the model will be saved there
#' @param ReturnLayer Which layer of the NNet to return. Choose from 1-7 with 4 being the layer with the least amount of nodes
#' @param per_feature Set to TRUE to have per feature anomaly detection generated. Otherwise and overall value will be generated
#' @param Activation Choose from "Tanh", "TanhWithDropout", "Rectifier", "RectifierWithDropout","Maxout", "MaxoutWithDropout"
#' @param Epochs Quantile value to find the cutoff value for classifying outliers
#' @param L2 Specify the amount of memory to allocate to H2O. E.g. "28G"
#' @param ElasticAveraging Specify the number of threads (E.g. cores * 2)
#' @param ElasticAveragingMovingRate Specify the number of decision trees to build
#' @param ElasticAveragingRegularization Specify the row sample rate per tree
#' @examples
#' \dontrun{
#' ############################
#' # Training
#' ############################
#'
#' # Create simulated data
#' data <- RemixAutoML::FakeDataGenerator(
#'   Correlation = 0.70,
#'   N = 1000L,
#'   ID = 2L,
#'   FactorCount = 2L,
#'   AddDate = TRUE,
#'   AddComment = FALSE,
#'   ZIP = 2L,
#'   TimeSeries = FALSE,
#'   ChainLadderData = FALSE,
#'   Classification = FALSE,
#'   MultiClass = FALSE)
#'
#' # Run algo
#' Output <- RemixAutoML::H2OAutoencoder(
#'
#'   # Select the service
#'   AnomalyDetection = FALSE,
#'   DimensionReduction = TRUE,
#'
#'   # Data related args
#'   data = data,
#'   ValidationData = NULL,
#'   Features = names(data)[2L:(ncol(data)-1L)],
#'   per_feature = FALSE,
#'   RemoveFeatures = TRUE,
#'   ModelID = "TestModel",
#'   model_path = getwd(),
#'
#'   # H2O Environment
#'   NThreads = max(1L, parallel::detectCores()-2L),
#'   MaxMem = "28G",
#'   H2OStart = TRUE,
#'   H2OShutdown = TRUE,
#'
#'   # H2O ML Args
#'   LayerStructure = NULL,
#'   NodeShrinkRate = (sqrt(5) - 1) / 2,
#'   ReturnLayer = 4L,
#'   Activation = "Tanh",
#'   Epochs = 5L,
#'   L2 = 0.10,
#'   ElasticAveraging = TRUE,
#'   ElasticAveragingMovingRate = 0.90,
#'   ElasticAveragingRegularization = 0.001)
#'
#' # Inspect output
#' data <- Output$Data
#' Model <- Output$Model
#'
#' # If ValidationData is not null
#' ValidationData <- Output$ValidationData
#'
#' ############################
#' # Scoring
#' ############################
#'
#' # Create simulated data
#' data <- RemixAutoML::FakeDataGenerator(
#'   Correlation = 0.70,
#'   N = 1000L,
#'   ID = 2L,
#'   FactorCount = 2L,
#'   AddDate = TRUE,
#'   AddComment = FALSE,
#'   ZIP = 2L,
#'   TimeSeries = FALSE,
#'   ChainLadderData = FALSE,
#'   Classification = FALSE,
#'   MultiClass = FALSE)
#'
#' # Run algo
#' data <- RemixAutoML::H2OAutoencoderScoring(
#'
#'   # Select the service
#'   AnomalyDetection = TRUE,
#'   DimensionReduction = TRUE,
#'
#'   # Data related args
#'   data = data,
#'   Features = names(data)[2L:ncol(data)],
#'   RemoveFeatures = TRUE,
#'   per_feature = FALSE,
#'   ModelObject = NULL,
#'   ModelID = "TestModel",
#'   model_path = getwd(),
#'
#'   # H2O args
#'   NThreads = max(1L, parallel::detectCores()-2L),
#'   MaxMem = "28G",
#'   H2OStart = TRUE,
#'   H2OShutdown = TRUE,
#'   ReturnLayer = 4L)
#' }
#' @return A data.table
#' @export
H2OAutoencoder <- function(AnomalyDetection = FALSE,
                           DimensionReduction = TRUE,
                           data,
                           ValidationData = NULL,
                           Features = NULL,
                           RemoveFeatures = FALSE,
                           NThreads = max(1L, parallel::detectCores()-2L),
                           MaxMem = "28G",
                           H2OStart = TRUE,
                           H2OShutdown = TRUE,
                           ModelID = "TestModel",
                           model_path = NULL,
                           LayerStructure  = NULL,
                           NodeShrinkRate = (sqrt(5) - 1) / 2,
                           ReturnLayer = 4L,
                           per_feature = TRUE,
                           Activation = "Tanh",
                           Epochs = 5L,
                           L2 = 0.10,
                           ElasticAveraging = TRUE,
                           ElasticAveragingMovingRate = 0.90,
                           ElasticAveragingRegularization = 0.001) {

  # Ensure data.table ----
  if(!data.table::is.data.table(data)) data.table::setDT(data)
  if(!is.null(ValidationData)) if(!data.table::is.data.table(ValidationData)) data.table::setDT(ValidationData)

  # Return because of mispecified arguments----
  if(!AnomalyDetection && !DimensionReduction) stop("Why are you running this function if you do not want anomaly detection nor dimension reduction?")

  # Constants ----
  F_Length <- length(Features)
  if(is.numeric(Features) || is.integer(Features)) Features <- names(data)[Features]

  # Ensure categoricals are set as factors----
  data <- ModelDataPrep(data=data, Impute=FALSE, CharToFactor=TRUE, FactorToChar=FALSE, IntToNumeric=FALSE, DateToChar=FALSE, RemoveDates=TRUE, MissFactor="0", MissNum=-1, IgnoreCols=NULL)

  # Initialize H2O----
  if(H2OStart) LocalHost <- h2o::h2o.init(nthreads = NThreads, max_mem_size = MaxMem, enable_assertions = FALSE)
  H2O_Data <- h2o::as.h2o(data)
  if(RemoveFeatures) data.table::set(data, j = Features, value = NULL)
  if(!is.null(ValidationData)) H2O_Validation <- h2o::as.h2o(ValidationData)

  # Layer selection - Eventually put in an arg for Type for some alternative pre-set LayerStructure----
  if(is.null(LayerStructure)) LayerStructure <- c(F_Length, ceiling(F_Length * NodeShrinkRate), ceiling(F_Length * NodeShrinkRate ^ 2), ceiling(F_Length * NodeShrinkRate ^ 3), ceiling(F_Length * NodeShrinkRate ^ 2), ceiling(F_Length * NodeShrinkRate), F_Length)

  # Build model----
  if(!is.null(ValidationData)) {

    # Build Model
    Model <- h2o::h2o.deeplearning(
      autoencoder = TRUE,
      model_id = ModelID,
      training_frame = H2O_Data,
      validation_frame = H2O_Validation,
      x = Features,
      l2 = L2,
      epochs = Epochs,
      hidden = LayerStructure,
      activation = Activation,
      elastic_averaging = ElasticAveraging,
      elastic_averaging_moving_rate = ElasticAveragingMovingRate,
      elastic_averaging_regularization = ElasticAveragingRegularization)

    # Save Model
    if(!is.null(model_path)) SaveModel <- h2o::h2o.saveModel(object = Model, path = model_path, force = TRUE)

  } else {

    # Build Model
    Model <- h2o::h2o.deeplearning(
      autoencoder = TRUE,
      model_id = ModelID,
      training_frame = H2O_Data,
      x = Features,
      l2 = L2,
      epochs = Epochs,
      hidden = LayerStructure,
      activation = Activation,
      elastic_averaging = ElasticAveraging,
      elastic_averaging_moving_rate = ElasticAveragingMovingRate,
      elastic_averaging_regularization = ElasticAveragingRegularization)

    # Save Model
    if(!is.null(model_path)) SaveModel <- h2o::h2o.saveModel(object = Model, path = model_path, force = TRUE)
  }

  # Create and Merge features----
  if(AnomalyDetection && DimensionReduction) {
    if(!is.null(ValidationData)) {
      Data <- cbind(
        data,
        data.table::as.data.table(h2o::h2o.deepfeatures(object = Model, data = H2O_Data, layer = ReturnLayer)),
        data.table::as.data.table(h2o::h2o.anomaly(object = Model, data = H2O_Data, per_feature = per_feature)))
      ValData <- cbind(
        ValidationData,
        data.table::as.data.table(h2o::h2o.deepfeatures(object = Model, data = H2O_Validation, layer = ReturnLayer)),
        data.table::as.data.table(h2o::h2o.anomaly(object = Model, data = H2O_Validation, per_feature = per_feature)))
    } else {
      Data <- cbind(
        data,
        data.table::as.data.table(h2o::h2o.deepfeatures(object = Model, data = H2O_Data, layer = ReturnLayer)),
        data.table::as.data.table(h2o::h2o.anomaly(object = Model, data = H2O_Data, per_feature = per_feature)))
    }
  } else if(!AnomalyDetection && DimensionReduction) {
    if(!is.null(ValidationData)) {
      Data <- cbind(
        data,
        data.table::as.data.table(h2o::h2o.deepfeatures(object = Model, data = H2O_Data, layer = ReturnLayer)))
      ValData <- cbind(
        ValidationData,
        data.table::as.data.table(h2o::h2o.deepfeatures(object = Model, data = H2O_Validation, layer = ReturnLayer)))
    } else {
      Data <- cbind(
        data,
        data.table::as.data.table(h2o::h2o.deepfeatures(object = Model, data = H2O_Data, layer = ReturnLayer)))
    }
  } else if(AnomalyDetection && !DimensionReduction) {
    if(!is.null(ValidationData)) {
      Data <- cbind(
        data,
        data.table::as.data.table(h2o::h2o.anomaly(object = Model, data = H2O_Data, per_feature = per_feature)))
      ValData <- cbind(
        ValidationData,
        data.table::as.data.table(h2o::h2o.anomaly(object = Model, data = H2O_Validation, per_feature = per_feature)))
    } else {
      Data <- cbind(
        data,
        data.table::as.data.table(h2o::h2o.anomaly(object = Model, data = H2O_Data, per_feature = per_feature)))
    }
  }

  # Shutdown h2o----
  if(H2OShutdown) h2o::h2o.shutdown(prompt = FALSE)

  # Return output----
  return(list(Data = Data, Model = Model, ValidationData = if(!is.null(ValidationData)) ValData else NULL))
}
