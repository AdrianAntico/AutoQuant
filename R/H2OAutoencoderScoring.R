#' @title H2OAutoencoderScoring
#'
#' @description H2OAutoencoderScoring for anomaly detection and or dimensionality reduction
#'
#' @author Adrian Antico
#' @family Feature Engineering
#'
#' @param AnomalyDetection Set to TRUE to run anomaly detection
#' @param DimensionReduction Set to TRUE to run dimension reduction
#' @param data The data.table with the columns you wish to have analyzed
#' @param Features NULL Column numbers or column names
#' @param RemoveFeatures Set to TRUE if you want the features you specify in the Features argument to be removed from the data returned
#' @param ModelObject If NULL then the model will be loaded from file. Otherwise, it will use what is supplied
#' @param NThreads max(1L, parallel::detectCores()-2L)
#' @param MaxMem "28G"
#' @param H2OStart TRUE to start H2O inside the function
#' @param H2OShutdown Setting to TRUE will shutdown H2O when it done being used internally.
#' @param ModelID "TestModel"
#' @param model_path If NULL no model will be saved. If a valid path is supplied the model will be saved there
#' @param ReturnLayer Which layer of the NNet to return. Choose from 1-7 with 4 being the layer with the least amount of nodes
#' @param per_feature Set to TRUE to have per feature anomaly detection generated. Otherwise and overall value will be generated
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
#'   AnomalyDetection = TRUE,
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
H2OAutoencoderScoring <- function(data,
                                  Features = NULL,
                                  RemoveFeatures = FALSE,
                                  ModelObject = NULL,
                                  AnomalyDetection = TRUE,
                                  DimensionReduction = TRUE,
                                  ReturnLayer = 4L,
                                  per_feature = TRUE,
                                  NThreads = max(1L, parallel::detectCores()-2L),
                                  MaxMem = "28G",
                                  H2OStart = TRUE,
                                  H2OShutdown = TRUE,
                                  ModelID = "TestModel",
                                  model_path = NULL) {

  # Full speed ahead----
  data.table::setDTthreads(threads = max(1L, parallel::detectCores() - 2L))

  # Ensure data.table----
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  # Return because of mispecified arguments----
  if(!AnomalyDetection & !DimensionReduction) stop("Why are you running this function if you do not want anomaly detection nor dimension reduction?")

  # Constants----
  GR <- (sqrt(5) - 1) / 2
  F_Length <- length(Features)
  if(is.numeric(Features) || is.integer(Features)) Features <- names(data)[Features]

  # Ensure categoricals are set as factors----
  data <- ModelDataPrep(data=data, Impute=FALSE, CharToFactor=TRUE, FactorToChar=FALSE, IntToNumeric=FALSE, DateToChar=TRUE, RemoveDates=TRUE, MissFactor="0", MissNum=-1, IgnoreCols=NULL)

  # Initialize H2O----
  if(H2OStart) h2o::h2o.init(nthreads = NThreads, max_mem_size = MaxMem, enable_assertions = FALSE)
  H2O_Data <- h2o::as.h2o(data)
  if(is.null(ModelObject)) ModelObject <- h2o::h2o.loadModel(path = file.path(model_path, ModelID))
  if(RemoveFeatures) data.table::set(data, j = Features, value = NULL)

  # Create and Merge features ----
  if(AnomalyDetection && DimensionReduction) {
    Data <- cbind(
      data,
      data.table::as.data.table(h2o::h2o.deepfeatures(object = ModelObject, data = H2O_Data, layer = ReturnLayer)),
      data.table::as.data.table(h2o::h2o.anomaly(object = ModelObject, data = H2O_Data, per_feature = per_feature)))
  } else if(!AnomalyDetection && DimensionReduction) {
    Data <- cbind(
      data,
      data.table::as.data.table(h2o::h2o.deepfeatures(object = ModelObject, data = H2O_Data, layer = ReturnLayer)))
  } else if(AnomalyDetection && !DimensionReduction) {
    Data <- cbind(
      data,
      data.table::as.data.table(h2o::h2o.anomaly(object = ModelObject, data = H2O_Data, per_feature = per_feature)))
  }

  # Shutdown h2o----
  if(H2OShutdown) h2o::h2o.shutdown(prompt = FALSE)

  # Return output----
  return(Data)
}
