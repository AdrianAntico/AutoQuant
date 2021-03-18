#' @title AutoClustering
#'
#' @description AutoClustering adds a column to your original data with a cluster number identifier
#'
#' @author Adrian Antico
#' @family Unsupervised Learning
#'
#' @param data is the source time series data.table
#' @param FeatureColumns Independent variables
#' @param ModelID For naming the files to save
#' @param SavePath Directory path for saving models
#' @param NThreads set based on number of threads your machine has available
#' @param MaxMemory set based on the amount of memory your machine has available
#' @param MaxClusters number of factors to test out in k-means to find the optimal number
#' @param ClusterMetric pick the metric to identify top model in grid tune c("totss","betweenss","withinss")
#' @param Epochs For the autoencoder
#' @param L2_Reg For the autoencoder
#' @param ElasticAveraging For the autoencoder
#' @param ElasticAveragingMovingRate For the autoencoder
#' @param ElasticAveragingRegularization For the autoencoder
#'
#' @examples
#' \dontrun{
#' }
#' @return Original data.table with added column with cluster number identifier
#' @export
AutoClustering <- function(data,
                           FeatureColumns = NULL,
                           ModelID = "TestModel",
                           SavePath = NULL,
                           NThreads = 8,
                           MaxMemory = "28G",
                           MaxClusters = 50,
                           ClusterMetric = "totss",
                           Epochs = 5L,
                           L2_Reg = 0.10,
                           ElasticAveraging = TRUE,
                           ElasticAveragingMovingRate = 0.90,
                           ElasticAveragingRegularization = 0.001) {

  # Check data.table ----
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  # Column names ----
  tempnames <- names(data.table::copy(data))

  # Subset data
  temp <- data[, .SD, .SDcols = c(FeatureColumns)]

  # H2OAutoencoder ----
  Output <- RemixAutoML::H2OAutoencoder(

    # Select the service
    AnomalyDetection = FALSE,
    DimensionReduction = TRUE,

    # Data related args
    data = temp,
    ValidationData = NULL,
    Features = FeatureColumns,
    per_feature = FALSE,
    RemoveFeatures = FALSE,
    ModelID = paste0(ModelID,"_H2OAutoencoder"),
    model_path = SavePath,

    # H2O Environment
    NThreads = NThreads,
    MaxMem = MaxMemory,
    H2OStart = TRUE,
    H2OShutdown = TRUE,

    # H2O ML Args
    LayerStructure = NULL,
    ReturnLayer = 4L,
    Activation = "Tanh",
    Epochs = Epochs,
    L2 = L2_Reg,
    ElasticAveraging = ElasticAveraging,
    ElasticAveragingMovingRate = ElasticAveragingMovingRate,
    ElasticAveragingRegularization = ElasticAveragingRegularization)

  # Collect output
  data <- Output$Data; rm(Output)

  # New cols
  NewCols <- setdiff(names(data), tempnames)

  # New data
  Kdata <- data[, .SD, .SDcols = c(NewCols)]

  # Convert to h2o frame
  LocalH2O <- h2o::h2o.init(nthreads = NThreads, max_mem_size = MaxMemory, enable_assertions = FALSE)
  H2OData <- h2o::as.h2o(Kdata)

  # Define grid tune search scheme in a named list----
  search_criteria  <- list(
    strategy = "RandomDiscrete",
    max_runtime_secs = 3600,
    max_models = 30,
    seed = 1234,
    stopping_rounds = 10)

  # Define hyperparameters----
  HyperParams <- list(max_iterations = c(10, 20, 50, 100), init = c("Random","PlusPlus","Furthest"))

  # Run grid tune----
  grid <- h2o::h2o.grid(
    "kmeans",
    search_criteria = search_criteria,
    training_frame = H2OData,
    x = NewCols,
    k = MaxClusters,
    grid_id = paste0(ModelID,"_KMeans"),
    estimate_k = TRUE,
    hyper_params = HyperParams)

  # Get best performer----
  Grid_Out <- h2o::h2o.getGrid(grid_id = paste0(ModelID,"_KMeans"), sort_by = ClusterMetric, decreasing = FALSE)
  model <- h2o::h2o.getModel(model_id = Grid_Out@model_ids[[1L]])

  # Save model if requested----
  if(!is.null(SavePath)) save_model <- h2o::h2o.saveModel(object = model, path = SavePath, force = TRUE)

  # Combine outputs----
  preds <- data.table::as.data.table(h2o::h2o.predict(model, H2OData))
  h2o::h2o.shutdown(prompt = FALSE)
  data <- data.table::as.data.table(cbind(data, preds))
  data.table::setnames(data, "predict", "ClusterID")
  return(data)
}
