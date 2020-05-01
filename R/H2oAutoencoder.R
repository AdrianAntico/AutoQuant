#' H2oAutoencoder for anomaly detection and dimensionality reduction
#'
#' H2oAutoencoder for anomaly detection and or dimensionality reduction
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @param AnomalyDetection Set to TRUE to run anomaly detection 
#' @param DimensionReduction Set to TRUE to run dimension reduction
#' @param data The data.table with the columns you wish to have analyzed
#' @param ValidationData The data.table with the columns you wish to have scored
#' @param Features NULL Column numbers or column names
#' @param RemoveFeatures Set to TRUE if you want the features you specify in the Features argument to be removed from the data returned
#' @param NThreads max(1L, parallel::detectCores()-2L)
#' @param MaxMem "28G"
#' @param H2oShutdown Setting to TRUE will shutdown H2O when it done being used internally.
#' @param ModelID "TestModel"
#' @param ReturnLayer Which layer of the NNet to return. Choose from 1-7 with 4 being the layer with the least amount of nodes
#' @param per_feature Set to TRUE to have per feature anomaly detection generated. Otherwise and overall value will be generated
#' @param Activation Choose from "Tanh", "TanhWithDropout", "Rectifier", "RectifierWithDropout","Maxout", "MaxoutWithDropout"
#' @param Epochs Quantile value to find the cutoff value for classifying outliers
#' @param L2 Specify the amount of memory to allocate to H2O. E.g. "28G"
#' @param ElasticAveraging Specify the number of threads (E.g. cores * 2)
#' @param ElasticAveragingMovingRate Specify the number of decision trees to build
#' @param ElasticAveragingRegularization Specify the row sample rate per tree
#' @examples
#' \donttest{
#' 
#' # Create simulated data
#' 
#' # Define correlation strength of features to target
#' Correl <- 0.85
#' 
#' # Number of rows you want returned
#' N <- 10000
#' 
#' # Create data
#' data <- data.table::data.table(Adrian = runif(N))
#' data[, x1 := qnorm(Adrian)]
#' data[, x2 := runif(N)]
#' data[, Independent_Variable1 := log(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable2 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable3 := exp(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable4 := exp(exp(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2))))]
#' data[, Independent_Variable5 := sqrt(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable6 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^0.10]
#' data[, Independent_Variable7 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^0.25]
#' data[, Independent_Variable8 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^0.75]
#' data[, Independent_Variable9 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^2]
#' data[, Independent_Variable10 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^4]
#' data[, Independent_Variable11 := as.factor(
#'  data.table::fifelse(Independent_Variable2 < 0.15, "A",
#'         data.table::fifelse(Independent_Variable2 < 0.45, "B",
#'                data.table::fifelse(Independent_Variable2 < 0.65,  "C",
#'                       data.table::fifelse(Independent_Variable2 < 0.85,  "D", "E")))))]
#' data.table::set(data, j = c("x1", "x2"), value = NULL)
#' 
#' # Get number of columns for LayerStructure
#' N <- length(names(data)[2L:ncol(data)])
#' 
#' # Run algo
#' Output <- RemixAutoML::H2oAutoencoder(
#'    
#'    # Select the service
#'    AnomalyDetection = TRUE, 
#'    DimensionReduction = TRUE,
#'    
#'    # Data related args
#'    data = data,
#'    ValidationData = NULL,
#'    Features = names(data)[2L:ncol(data)],
#'    RemoveFeatures = FALSE,
#'    
#'    # H2O args
#'    NThreads = max(1L, parallel::detectCores()-2L),
#'    MaxMem = "28G",
#'    H2oShutdown = TRUE,
#'    ModelID = "TestModel",
#'    LayerStructure = NULL, # creates this internally if LayerStructure is NULL --> c(N, N * GR, N GR ^ 2, N * GR ^ 3, N * GR ^ 2, N * GR, N)
#'    ReturnLayer = 4L, 
#'    per_feature = TRUE,
#'    Activation = "Tanh", 
#'    Epochs = 5L, 
#'    L2 = 0.10, 
#'    ElasticAveraging = TRUE, 
#'    ElasticAveragingMovingRate = 0.90, 
#'    ElasticAveragingRegularization = 0.001)
#'    
#'  # Inspect output
#'  Data <- Output$Data
#'  Model <- Output$Model
#'  
#'  # If ValidationData is not null
#'  ValidationData <- Output$ValidationData  
#' }
#' @return A data.table
#' @export
H2oAutoencoder <- function(AnomalyDetection = TRUE,
                           DimensionReduction = TRUE,
                           data,
                           ValidationData = NULL,
                           Features = NULL,
                           RemoveFeatures = FALSE,
                           NThreads = max(1L, parallel::detectCores()-2L),
                           MaxMem = "28G",
                           H2oShutdown = TRUE,
                           ModelID = "TestModel",
                           LayerStructure  = NULL,
                           ReturnLayer = 4L,
                           per_feature = TRUE,
                           Activation = "Tanh",
                           Epochs = 5L,
                           L2 = 0.10,
                           ElasticAveraging = TRUE,
                           ElasticAveragingMovingRate = 0.90,
                           ElasticAveragingRegularization = 0.001) {
  
  # Return because of mispecified arguments----
  if(!AnomalyDetection & !DimensionReduction) return("Why are you running this function if you do not want anomaly detection nor dimension reduction?")
  
  # Constants----
  GR <- (sqrt(5) - 1) / 2
  F_Length <- length(Features)
  if(is.numeric(Features) | is.integer(Features)) Features <- names(data)[Features]
  
  # Ensure categoricals are set as factors----
  data <- ModelDataPrep(data=data, Impute=FALSE, CharToFactor=TRUE, FactorToChar=FALSE, IntToNumeric=FALSE, DateToChar=TRUE, RemoveDates=TRUE, MissFactor="0", MissNum=-1, IgnoreCols=NULL)
  
  # Initialize H2O----
  h2o::h2o.init(nthreads = NThreads, max_mem_size = MaxMem, enable_assertions = FALSE)
  H2O_Data <- h2o::as.h2o(data)
  if(RemoveFeatures) data.table::set(data, j = Features, value = NULL)
  if(!is.null(ValidationData)) H2O_Validation <- h2o::as.h2o(ValidationData)
  
  # Layer selection - Eventually put in an arg for Type for some alternative pre-set LayerStructure----
  if(is.null(LayerStructure)) {
    LayerStructure <- c(F_Length,ceiling(F_Length * GR), ceiling(F_Length * GR ^ 2), ceiling(F_Length * GR ^ 3), ceiling(F_Length * GR ^ 2), ceiling(F_Length * GR), F_Length)
  }
  
  # Build model----
  if(!is.null(ValidationData)) {
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
  } else {
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
  }
  
  # Create and Merge features----
  if(AnomalyDetection & DimensionReduction) {
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
  }
  if(!AnomalyDetection & DimensionReduction) {
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
  }
  if(AnomalyDetection & !DimensionReduction) {
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
  if(H2oShutdown) h2o::h2o.shutdown(prompt = FALSE)
  
  # Return output----
  return(list(Data = Data, Model = Model, ValidationData = if(!is.null(ValidationData)) ValData else NULL))
}
