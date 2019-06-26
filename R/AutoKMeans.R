#' AutoKMeans Automated row clustering for mixed column types
#'
#' AutoKMeans adds a column to your original data with a cluster number identifier. Uses glrm (grid tune-able) and then k-means to find optimal k.
#'
#' @author Adrian Antico
#' @family Unsupervised Learning
#' @param data is the source time series data.table
#' @param GridTuneGLRM If you want to grid tune the glrm model, set to TRUE, FALSE otherwise
#' @param GridTuneKMeans If you want to grid tuen the KMeans model, set to TRUE, FALSE otherwise
#' @param nthreads set based on number of threads your machine has available
#' @param MaxMem set based on the amount of memory your machine has available
#' @param glrmCols the column numbers for the glrm
#' @param IgnoreConstCols tell H2O to ignore any columns that have zero variance
#' @param glrmFactors similar to the number of factors to return from PCA
#' @param Loss set to one of "Quadratic", "Absolute", "Huber", "Poisson", "Hinge", "Logistic", "Periodic"
#' @param glrmMaxIters max number of iterations
#' @param SVDMethod choose from "Randomized","GramSVD","Power"
#' @param MaxRunTimeSecs set the timeout for max run time
#' @param KMeansK number of factors to test out in k-means to find the optimal number
#' @param KMeansMetric pick the metric to identify top model in grid tune c("totss","betweenss","withinss")
#' @param SaveModels Set to "standard", "mojo", or NULL (default)
#' @param PathFile Set to folder where you will keep the models
#' @examples
#' \donttest{
#' data <- data.table::as.data.table(iris)
#' data <- AutoKMeans(data,
#'                    nthreads = 8,
#'                    MaxMem = "28G",
#'                    SaveModels = NULL,
#'                    PathFile = NULL,
#'                    GridTuneGLRM = TRUE,
#'                    GridTuneKMeans = TRUE,
#'                    glrmCols = 1:(ncol(data)-1),
#'                    IgnoreConstCols = TRUE,
#'                    glrmFactors = 2,
#'                    Loss = "Absolute",
#'                    glrmMaxIters = 1000,
#'                    SVDMethod = "Randomized",
#'                    MaxRunTimeSecs = 3600,
#'                    KMeansK = 5,
#'                    KMeansMetric = "totss")
#' unique(data[["Species"]])
#' unique(data[["ClusterID"]])
#' temp <- data[, mean(ClusterID), by = "Species"]
#' Setosa <- round(temp[Species == "setosa", V1][[1]],0)
#' Versicolor <- round(temp[Species == "versicolor", V1][[1]],0)
#' Virginica <- round(temp[Species == "virginica", V1][[1]],0)
#' data[, Check := "a"]
#' data[ClusterID == eval(Setosa), Check := "setosa"]
#' data[ClusterID == eval(Virginica), Check := "virginica"]
#' data[ClusterID == eval(Versicolor), Check := "versicolor"]
#' data[, Acc := as.numeric(ifelse(Check == Species, 1, 0))]
#' data[, mean(Acc)][[1]]
#' }
#' @return Original data.table with added column with cluster number identifier
#' @export
AutoKMeans <- function(data,
                       nthreads        = 8,
                       MaxMem          = "28G",
                       SaveModels      = NULL,
                       PathFile        = NULL,
                       GridTuneGLRM    = TRUE,
                       GridTuneKMeans  = TRUE,
                       glrmCols        = c(1:5),
                       IgnoreConstCols = TRUE,
                       glrmFactors     = 5,
                       Loss            = "Absolute",
                       glrmMaxIters    = 1000,
                       SVDMethod       = "Randomized",
                       MaxRunTimeSecs  = 3600,
                       KMeansK         = 50,
                       KMeansMetric    = "totss") {
  # Check Arguments----
  if (nthreads < 0) {
    warning("nthreads needs to be a positive integer")
  }
  if (!is.character(MaxMem)) {
    warning("MaxMem needs to be a character value. E.g. MaxMem = '28G'")
  }
  if (!is.null(SaveModels)) {
    if (!(tolower(SaveModels) %chin% c("mojo", "standard"))) {
      warning("SaveModels needs to be either NULL, 'mojo', or 'standard'")
    }
  }
  if (!is.null(FilePath)) {
    if (!is.character(FilePath)) {
      warning("FilePath needs to resolve to a character value. E.g. getwd()")
    }
  }
  if (!is.logical(GridTuneGLRM)) {
    warning("GridTuneGLRM needs to be either TRUE or FALSE")
  }
  if (!is.logical(GridTuneKMeans)) {
    warning("GridTuneKMeans needs to be either TRUE or FALSE")
  }
  if (!(is.numeric(glrmCols) | is.integer(glrmCols))) {
    warning("glrmCols needs to be the column numbers")
  }
  if (!is.logical(IgnoreConstCols)) {
    warning("IgnoreConstCols needs to be either TRUE or FALSE")
  }
  if (!(is.numeric(glrmFactors) | is.integer(glrmFactors))) {
    warning("glrmFactors needs to be an integer value")
  }
  
  # Check data.table----
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Set up Scoring File if SaveModels is not NULL----
  if (!is.null(SaveModels)) {
    KMeansModelFile <- data.table::data.table(
      Name = c("GLMR", "AutoKMeans"),
      FilePath1 = rep("bla", 2),
      FilePath2 = rep("bla", 2)
    )
  }
  
  # Build glmr model----
  h2o::h2o.init(nthreads = nthreads, max_mem_size = MaxMem)
  datax <- h2o::as.h2o(data)
  if (GridTuneGLRM) {
    # Define grid tune search scheme in a named list----
    search_criteria  <-
      list(
        strategy             = "RandomDiscrete",
        max_runtime_secs     = 3600,
        max_models           = 30,
        seed                 = 1234,
        stopping_rounds      = 10,
        stopping_metric      = "MSE",
        stopping_tolerance   = 1e-3
      )
    
    # Define hyperparameters----
    HyperParams <-
      list(
        transform        = c("NONE",
                             "DEMEAN",
                             "DESCALE",
                             "STANDARDIZE"),
        k                = 1:5,
        regularization_x = c(
          "None",
          "Quadratic",
          "L2",
          "L1",
          "NonNegative",
          "OneSparse",
          "UnitOneSparse",
          "Simplex"
        ),
        regularization_y = c(
          "None",
          "Quadratic",
          "L2",
          "L1",
          "NonNegative",
          "OneSparse",
          "UnitOneSparse",
          "Simplex"
        ),
        gamma_x          = seq(0.01, 0.10, 0.01),
        gamma_y          = seq(0.01, 0.10, 0.01),
        svd_method       = c("Randomized",
                             "GramSVD",
                             "Power")
      )
    
    # Run grid tune----
    grid <- h2o::h2o.grid(
      "glrm",
      search_criteria   = search_criteria,
      training_frame    = datax,
      grid_id           = "GLRM",
      ignore_const_cols = IgnoreConstCols,
      loss              = Loss,
      hyper_params      = HyperParams
    )
    
    # Get best performer----
    Grid_Out <-
      h2o::h2o.getGrid(
        grid_id = "GLRM",
        sort_by = search_criteria$stopping_metric,
        decreasing = FALSE
      )
    model <- h2o::h2o.getModel(model_id = Grid_Out@model_ids[[1]])
  } else {
    model <- h2o::h2o.glrm(
      training_frame    = datax,
      cols              = glrmCols,
      ignore_const_cols = IgnoreConstCols,
      k                 = glrmFactors,
      loss              = Loss,
      max_iterations    = glrmMaxIters,
      svd_method        = SVDMethod,
      max_runtime_secs  = MaxRunTimeSecs
    )
  }
  
  # Save model if requested----
  if (!is.null(SaveModels)) {
    # Save archetypes and colnames----
    fitY <- model@model$archetypes
    save(fitY, file = paste0(PathFile, "/fitY"))
    data.table::set(
      KMeansModelFile,
      i = 1L,
      j = 2L,
      value = paste0(PathFile, "/fitY")
    )
  }
  
  # Run k-means----
  if (GridTuneKMeans) {
    # GLRM output----
    x_raw <- h2o::h2o.getFrame(model@model$representation_name)
    Names <- colnames(x_raw)
    if (!is.null(SaveModels)) {
      save(Names, file = paste0(PathFile, "/Names.Rdata"))
      data.table::set(
        KMeansModelFile,
        i = 1L,
        j = 3L,
        value = paste0(PathFile, "/Names.Rdata")
      )
      save(KMeansModelFile,
           file = paste0(PathFile, "/KMeansModelFile.Rdata"))
    }
    
    # Define grid tune search scheme in a named list----
    search_criteria  <-
      list(
        strategy             = "RandomDiscrete",
        max_runtime_secs     = 3600,
        max_models           = 30,
        seed                 = 1234,
        stopping_rounds      = 10
      )
    
    # Define hyperparameters----
    HyperParams <- list(
      max_iterations   = c(10, 20, 50, 100),
      init             = c("Random", "PlusPlus", "Furthest")
    )
    
    # Run grid tune----
    grid <- h2o::h2o.grid(
      "kmeans",
      search_criteria   = search_criteria,
      training_frame    = x_raw,
      x                 = Names,
      k                 = KMeansK,
      grid_id           = "KMeans",
      estimate_k        = TRUE,
      hyper_params      = HyperParams
    )
    
    # Get best performer----
    Grid_Out <-
      h2o::h2o.getGrid(grid_id = "KMeans",
                       sort_by = KMeansMetric,
                       decreasing = FALSE)
    model <- h2o::h2o.getModel(model_id = Grid_Out@model_ids[[1]])
  } else {
    # GLRM output----
    x_raw <- h2o::h2o.getFrame(model@model$representation_name)
    Names <- colnames(x_raw)
    if (!is.null(SaveModels)) {
      save(Names, file = paste0(PathFile, "/Names.Rdata"))
      data.table::set(
        KMeansModelFile,
        i = 1L,
        j = 3L,
        value = paste0(PathFile, "/Names.Rdata")
      )
      save(KMeansModelFile,
           file = paste0(PathFile, "/KMeansModelFile.Rdata"))
    }
    
    # Train KMeans----
    model <- h2o::h2o.kmeans(
      training_frame = x_raw,
      x              = Names,
      k              = KMeansK,
      estimate_k     = TRUE
    )
  }
  
  # Save model if requested----
  if (!is.null(SaveModels)) {
    if (tolower(SaveModels) == "mojo") {
      save_model <-
        h2o::h2o.saveMojo(object = model,
                          path = PathFile,
                          force = TRUE)
      h2o::h2o.download_mojo(
        model = model,
        path = PathFile,
        get_genmodel_jar = TRUE,
        genmodel_path = PathFile,
        genmodel_name = "KMeans"
      )
      data.table::set(KMeansModelFile,
                      i = 2L,
                      j = 2L,
                      value = save_model)
      data.table::set(
        KMeansModelFile,
        i = 2L,
        j = 3L,
        value = paste0(PathFile, "/KMeans")
      )
      save(KMeansModelFile,
           file = paste0(PathFile,
                         "/KMeansModelFile.Rdata"))
    } else if (tolower(SaveModels) == "standard") {
      save_model <-
        h2o::h2o.saveModel(object = model,
                           path = PathFile,
                           force = TRUE)
      data.table::set(KMeansModelFile,
                      i = 2L,
                      j = 2L,
                      value = save_model)
      save(KMeansModelFile,
           file = paste0(PathFile,
                         "/KMeansModelFile.Rdata"))
    }
  }
  
  # Combine outputs----
  preds <- data.table::as.data.table(h2o::h2o.predict(model, x_raw))
  h2o::h2o.shutdown(prompt = FALSE)
  data <- data.table::as.data.table(cbind(preds, data))
  data.table::setnames(data, "predict", "ClusterID")
  return(data)
}