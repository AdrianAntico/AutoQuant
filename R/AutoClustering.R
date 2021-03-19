#' @title AutoClustering
#'
#' @description AutoClustering adds a column to your original data with a cluster number identifier. You can run request an autoencoder to be built to reduce the dimensionality of your data before running the clusering algo.
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
#' @param RunDimReduction If TRUE, an autoencoder will be built to reduce the feature space. Otherwise, all features in FeatureColumns will be used for clustering
#' @param ShrinkRate Node shrink rate for H2OAutoencoder. See that function for details.
#' @param Epochs For the autoencoder
#' @param L2_Reg For the autoencoder
#' @param ElasticAveraging For the autoencoder
#' @param ElasticAveragingMovingRate For the autoencoder
#' @param ElasticAveragingRegularization For the autoencoder
#'
#' @examples
#' \dontrun{
#' #########################
#' # Training Setup
#' #########################
#'
#' # Create fake data
#' data <- RemixAutoML::FakeDataGenerator(
#'   Correlation = 0.85,
#'   N = 1000,
#'   ID = 2,
#'   ZIP = 0,
#'   AddDate = TRUE,
#'   Classification = FALSE,
#'   MultiClass = FALSE)
#'
#' # Run function
#' data <- RemixAutoML::AutoClustering(
#'   data,
#'   FeatureColumns = names(data)[2:(ncol(data)-1)],
#'   ModelID = "TestModel",
#'   SavePath = getwd(),
#'   NThreads = 8,
#'   MaxMemory = "28G",
#'   MaxClusters = 50,
#'   ClusterMetric = "totss",
#'   RunDimReduction = TRUE,
#'   ShrinkRate = (sqrt(5) - 1) / 2,
#'   Epochs = 5L,
#'   L2_Reg = 0.10,
#'   ElasticAveraging = TRUE,
#'   ElasticAveragingMovingRate = 0.90,
#'   ElasticAveragingRegularization = 0.001)
#'
#' #########################
#' # Scoring Setup
#' #########################
#'
#' Sys.sleep(10)
#'
#' # Create fake data
#' data <- RemixAutoML::FakeDataGenerator(
#'   Correlation = 0.85,
#'   N = 1000,
#'   ID = 2,
#'   ZIP = 0,
#'   AddDate = TRUE,
#'   Classification = FALSE,
#'   MultiClass = FALSE)
#'
#' # Run function
#' data <- RemixAutoML::AutoClusteringScoring(
#'   data,
#'   FeatureColumns = names(data)[2:(ncol(data)-1)],
#'   ModelID = "TestModel",
#'   SavePath = getwd(),
#'   NThreads = 8,
#'   MaxMemory = "28G",
#'   DimReduction = TRUE)
#' }
#'
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
                           RunDimReduction = TRUE,
                           ShrinkRate = (sqrt(5) - 1) / 2,
                           Epochs = 5L,
                           L2_Reg = 0.10,
                           ElasticAveraging = TRUE,
                           ElasticAveragingMovingRate = 0.90,
                           ElasticAveragingRegularization = 0.001) {

  # Check data.table ----
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  # Dim Reduction
  if(RunDimReduction) {

    # Column names ----
    tempnames <- names(data.table::copy(data))

    # H2OAutoencoder ----
    data <- RemixAutoML::H2OAutoencoder(

      # Select the service
      AnomalyDetection = FALSE,
      DimensionReduction = TRUE,

      # Data related args
      data = data,
      Features = FeatureColumns,
      per_feature = FALSE,
      RemoveFeatures = FALSE,
      ModelID = paste0(ModelID,"_Cluster_H2OAutoencoder"),
      model_path = SavePath,

      # H2O Environment
      NThreads = NThreads,
      MaxMem = MaxMemory,
      H2OStart = TRUE,
      H2OShutdown = TRUE,

      # H2O ML Args
      NodeShrinkRate = ShrinkRate,
      LayerStructure = NULL,
      ReturnLayer = 4L,
      Activation = "Tanh",
      Epochs = Epochs,
      L2 = L2_Reg,
      ElasticAveraging = ElasticAveraging,
      ElasticAveragingMovingRate = ElasticAveragingMovingRate,
      ElasticAveragingRegularization = ElasticAveragingRegularization)

    # Sleep
    Sys.sleep(10L)

    # New cols
    FeatureColumns <- setdiff(names(data), tempnames)

    # New data
    temp <- data[, .SD, .SDcols = c(FeatureColumns)]

  } else {
    temp <- data[, .SD, .SDcols = c(FeatureColumns)]
  }

  # Convert to h2o frame
  LocalH2O <- h2o::h2o.init(nthreads = NThreads, max_mem_size = MaxMemory, enable_assertions = FALSE)
  H2OData <- h2o::as.h2o(temp)

  # Define grid tune search scheme in a named list ----
  search_criteria  <- list(
    strategy = "RandomDiscrete",
    max_runtime_secs = 3600,
    max_models = 30,
    seed = 1234,
    stopping_rounds = 10)

  # Define hyperparameters----
  HyperParams <- list(max_iterations = c(10, 20, 50, 100), init = c("Random","PlusPlus","Furthest"))

  # Run grid tune ----
  grid <- h2o::h2o.grid(
    "kmeans",
    search_criteria = search_criteria,
    training_frame = H2OData,
    x = FeatureColumns,
    k = MaxClusters,
    grid_id = paste0(ModelID,"_KMeans"),
    estimate_k = TRUE,
    hyper_params = HyperParams)

  # Get best performer----
  Grid_Out <- h2o::h2o.getGrid(grid_id = paste0(ModelID,"_KMeans"), sort_by = ClusterMetric, decreasing = FALSE)
  ClusterModel <- h2o::h2o.getModel(model_id = Grid_Out@model_ids[[1L]])

  # Save ClusterModel if requested ----
  if(!is.null(SavePath)) save_model <- h2o::h2o.saveModel(object = ClusterModel, path = SavePath, force = TRUE)

  # Combine outputs ----
  preds <- data.table::as.data.table(h2o::h2o.predict(ClusterModel, H2OData))
  h2o::h2o.shutdown(prompt = FALSE)
  data <- data.table::as.data.table(cbind(data, preds))
  data.table::setnames(data, "predict", "ClusterID")
  file.rename(from = save_model, to = file.path(SavePath, paste0(ModelID, "_KMeans")))
  return(data)
}

#' @title AutoClusteringScoring
#'
#' @description AutoClusteringScoring adds a column to your original data with a cluster number identifier. You can run request an autoencoder to be built to reduce the dimensionality of your data before running the clusering algo.
#'
#' @author Adrian Antico
#' @family Unsupervised Learning
#'
#' @param data is the source time series data.table
#' @param FeatureColumns Independent variables
#' @param ModelID This is returned from the training run in the output list with element named 'model_name'. It's not identical to the ModelID used in training due to the grid tuning.
#' @param SavePath Directory path for saving models
#' @param NThreads set based on number of threads your machine has available
#' @param MaxMemory set based on the amount of memory your machine has available
#' @param DimReduction Set to TRUE if you set RunDimReduction in the training version of this function
#'
#' @examples
#' \dontrun{
#' #########################
#' # Training Setup
#' #########################
#'
#' # Create fake data
#' data <- RemixAutoML::FakeDataGenerator(
#'   Correlation = 0.85,
#'   N = 1000,
#'   ID = 2,
#'   ZIP = 0,
#'   AddDate = TRUE,
#'   Classification = FALSE,
#'   MultiClass = FALSE)
#'
#' # Run function
#' data <- RemixAutoML::AutoClustering(
#'   data,
#'   FeatureColumns = names(data)[2:(ncol(data)-1)],
#'   ModelID = "TestModel",
#'   SavePath = getwd(),
#'   NThreads = 8,
#'   MaxMemory = "28G",
#'   MaxClusters = 50,
#'   ClusterMetric = "totss",
#'   RunDimReduction = TRUE,
#'   ShrinkRate = (sqrt(5) - 1) / 2,
#'   Epochs = 5L,
#'   L2_Reg = 0.10,
#'   ElasticAveraging = TRUE,
#'   ElasticAveragingMovingRate = 0.90,
#'   ElasticAveragingRegularization = 0.001)
#'
#' #########################
#' # Scoring Setup
#' #########################
#'
#' Sys.sleep(10)
#'
#' # Create fake data
#' data <- RemixAutoML::FakeDataGenerator(
#'   Correlation = 0.85,
#'   N = 1000,
#'   ID = 2,
#'   ZIP = 0,
#'   AddDate = TRUE,
#'   Classification = FALSE,
#'   MultiClass = FALSE)
#'
#' # Run function
#' data <- RemixAutoML::AutoClusteringScoring(
#'   data,
#'   FeatureColumns = names(data)[2:(ncol(data)-1)],
#'   ModelID = "TestModel",
#'   SavePath = getwd(),
#'   NThreads = 8,
#'   MaxMemory = "28G",
#'   DimReduction = TRUE)
#' }
#'
#' @return Original data.table with added column with cluster number identifier
#' @export
AutoClusteringScoring <- function(data,
                                  FeatureColumns = NULL,
                                  ModelID = "TestModel",
                                  SavePath = NULL,
                                  NThreads = 8,
                                  MaxMemory = "28G",
                                  DimReduction = TRUE) {

  # Check data.table ----
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  # Dim Reduction
  if(DimReduction) {

    # Column names ----
    tempnames <- names(data.table::copy(data))

    # Score H2OAutoencoder
    data <- RemixAutoML::H2OAutoencoderScoring(

      # Select the service
      AnomalyDetection = FALSE,
      DimensionReduction = TRUE,

      # Data related args
      data = data,
      Features = FeatureColumns,
      RemoveFeatures = FALSE,
      per_feature = FALSE,
      ModelObject = NULL,
      ModelID = paste0(ModelID,"_Cluster_H2OAutoencoder"),
      model_path = SavePath,

      # H2O args
      NThreads = NThreads,
      MaxMem = MaxMemory,
      H2OStart = TRUE,
      H2OShutdown = TRUE,
      ReturnLayer = 4L)

    # Sleep
    Sys.sleep(10L)

    # New cols
    FeatureColumns <- setdiff(names(data), tempnames)

    # New data
    temp <- data[, .SD, .SDcols = c(FeatureColumns)]

  } else {
    temp <- data[, .SD, .SDcols = c(FeatureColumns)]
  }

  # Convert to h2o frame
  LocalH2O <- h2o::h2o.init(nthreads = NThreads, max_mem_size = MaxMemory, enable_assertions = FALSE)
  H2OData <- h2o::as.h2o(temp)

  # Load model
  ClusterModel <- h2o::h2o.loadModel(path = file.path(SavePath, paste0(ModelID, "_KMeans")))

  # Combine outputs ----
  preds <- data.table::as.data.table(h2o::h2o.predict(ClusterModel, H2OData))
  h2o::h2o.shutdown(prompt = FALSE)
  data <- data.table::as.data.table(cbind(data, preds))
  data.table::setnames(data, "predict", "ClusterID")
  return(data)
}
