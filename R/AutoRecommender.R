#' @title AutoRecomDataCreate
#'
#' @description AutoRecomDataCreate to create data that is prepared for modeling
#'
#' @author Adrian Antico and Douglas Pestana
#' @family Recommenders
#'
#' @param data This is your transactional data.table. Must include an Entity (typically customer), ProductCode (such as SKU), and a sales metric (such as total sales).
#' @param EntityColName This is the column name in quotes that represents the column name for the Entity, such as customer
#' @param ProductColName This is the column name in quotes that represents the column name for the product, such as SKU
#' @param MetricColName This is the column name in quotes that represents the column name for the metric, such as total sales
#' @param ReturnMatrix Set to FALSE to coerce the object (desired route) or TRUE to return a matrix
#' @return A BinaryRatingsMatrix
#' @examples
#' \dontrun{
#' RatingsMatrix <- AutoRecomDataCreate(
#'   data,
#'   EntityColName = "CustomerID",
#'   ProductColName = "StockCode",
#'   MetricColName = "TotalSales",
#'   ReturnMatrix = TRUE)
#' }
#' @export
AutoRecomDataCreate <- function(data,
                                EntityColName  = "CustomerID",
                                ProductColName = "StockCode",
                                MetricColName  = "TotalSales",
                                ReturnMatrix   = FALSE) {

  # data.table optimize----
  if(parallel::detectCores() > 10) data.table::setDTthreads(threads = max(1L, parallel::detectCores() - 2L)) else data.table::setDTthreads(threads = max(1L, parallel::detectCores()))

  # Require RecommenderLab
  requireNamespace("recommenderlab", quietly = TRUE)

  # Ensure data is data.table----
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  # Ensure EntityColName is character type----
  if(!is.character(data[1, get(EntityColName)])) {
    data[, eval(EntityColName) := as.character(get(EntityColName))]
  }

  # Ensure ProductColName is character type----
  if(!is.character(data[1, get(ProductColName)])) {
    data[, eval(ProductColName) := as.character(get(ProductColName))]
  }

  # Ensure MetricColName is numeric----
  if(!is.numeric(data[1, get(MetricColName)])) {
    data[, eval(MetricColName) := as.numeric(get(MetricColName))]
  }

  # Only keep the necessary columns----
  keep <- c(EntityColName, ProductColName, MetricColName)
  data <- data[, ..keep]

  # CREATE BINARY RATING MATRIX-----
  train_data <- data.table::dcast(
    data,
    get(EntityColName) ~ get(ProductColName),
    value.var = eval(MetricColName),
    fun.aggregate = function(x) sum(!is.na(x)),
    fill = 0)

  # Change name back to original----
  data.table::setnames(train_data, "EntityColName", eval(EntityColName))

  # Convert Sales data to Binary----
  for(j in 2:ncol(train_data)) {
    data.table::set(x, j = j, value = data.table::fifelse(x[[j]] > 0, 1, 0))
  }

  # Store customerID for rownames----
  train_data_rownames <- train_data[[eval(EntityColName)]]

  # Remove CustomerID column----
  train_data[, eval(EntityColName) := NULL]

  # Convert train to matrix----
  train_data_matrix <- as.matrix(train_data)

  # Set rownames
  row.names(train_data_matrix) <- train_data_rownames

  # Return binary rating matrix----
  methods::as(object = train_data_matrix, Class = "binaryRatingMatrix")
}

#' Automatically build the best recommender model among models available.
#'
#' This function returns the winning model that you pass onto AutoRecommenderScoring
#' @author Adrian Antico and Douglas Pestana
#' @family Recommenders
#' @param data This is your BinaryRatingsMatrix. See function RecomDataCreate
#' @param Partition Choose from "split", "cross-validation", "bootstrap". See evaluationScheme in recommenderlab for details.
#' @param KFolds Choose 1 for traditional train and test. Choose greater than 1 for the number of cross validations
#' @param Ratio The ratio for train and test. E.g. 0.75 for 75 percent data allocated to training
#' @param Given The number of products you would like to evaluate. Negative values implement all-but schemes.
#' @param RatingType Choose from "TopN", "ratings", "ratingMatrix"
#' @param RatingsKeep The total ratings you wish to return. Default is 20.
#' @param SkipModels AssociationRules runs the slowest and may crash your system. Choose from: "AssociationRules","ItemBasedCF","UserBasedCF","PopularItems","RandomItems"
#' @param ModelMetric Choose from "Precision", "Recall", "TPR", or "FPR"
#' @examples
#' \dontrun{
#' WinningModel <- AutoRecommender(
#'   RatingsMatrix,
#'   Partition = "Split",
#'   KFolds = 1,
#'   Ratio = 0.75,
#'   Given = 1,
#'   RatingType = "TopN",
#'   RatingsKeep = 20,
#'   SkipModels = "AssociationRules",
#'   ModelMetric = "TPR")
#' }
#' @return The winning model used for scoring in the AutoRecommenderScoring function
#' @export
AutoRecommender <- function(data,
                            Partition   = "Split",
                            KFolds      = 1,
                            Ratio       = 0.75,
                            Given       = 1,
                            RatingType  = "TopN",
                            RatingsKeep = 20,
                            SkipModels  = "AssociationRules",
                            ModelMetric = "TPR") {

  # data.table optimize----
  if(parallel::detectCores() > 10) data.table::setDTthreads(threads = max(1L, parallel::detectCores() - 2L)) else data.table::setDTthreads(threads = max(1L, parallel::detectCores()))

  # Ensure data is proper----
  if(class(data)[1] != "binaryRatingMatrix") return("data must be of class binaryRatingMatrix")

  # Ensure KFolds is correct----
  if(tolower(Partition) == "split") KFolds <- 1

  # Ensure Ratio is proper----
  if(abs(Ratio) > 1 | Ratio == 0) return("Ratio must be a decimal between 0 and 1. is 0.75")

  # Ensure RatingType is real----
  if(tolower(RatingType) == "topn") {
    RatingType <- "topNList"
  } else if(tolower(RatingType) == "ratings") {
    RatingType <- "ratings"
  } else if(tolower(RatingType) == "ratingMatrix") {
    RatingType <- "ratingMatrix"
  }

  # Pick winning model based max TPR for 10th recommendation----
  if(tolower(ModelMetric) == "precision") {
    ModelMetric <- "precision"
  } else if(tolower(ModelMetric) == "recall") {
    ModelMetric <- "recall"
  } else if(tolower(ModelMetric) == "tpr") {
    ModelMetric <- "TPR"
  } else if(tolower(ModelMetric) == "fpr") {
    ModelMetric <- "FPR"
  } else {
    warning("ModelMetric not in list of usable metrics")
  }

  # Evaluation setup----
  scheme <- recommenderlab::evaluationScheme(
    data,
    method     = tolower(Partition),
    k          = KFolds,
    train      = Ratio,
    given      = Given,
    goodRating = 1)

  # Store algorithms in nested list----
  algorithms <- list(
    "RandomItems"  = list(name = "RANDOM",  param = NULL),
    "PopularItems" = list(name = "POPULAR", param = NULL),
    "UserBasedCF" = list(name = "UBCF",    param = NULL),
    "ItemBasedCF" = list(name = "IBCF",    param = NULL),
    "AssociationRules" = list(
      name = "AR",
      param = list(support = 0.001, confidence = 0.05)))

  # Remove all algos in SkipModels----
  if(any(tolower(SkipModels) == "associationrules")) algorithms[["AssociationRules"]] <- NULL
  if(any(tolower(SkipModels) == "itembasedcf")) algorithms[["ItemBasedCF"]] <- NULL
  if(any(tolower(SkipModels) == "userbasedcf")) algorithms[["UserBasedCF"]] <- NULL
  if(any(tolower(SkipModels) == "popularitems")) algorithms[["PopularItems"]] <- NULL
  if(any(tolower(SkipModels) == "randomitems")) algorithms[["RandomItems"]] <- NULL
  if(length(algorithms) == 0) return("You must have at least one algorithm to run")

  # evauluate predicted ratings from each algorithm----
  results <- recommenderlab::evaluate(
    x      = scheme,
    method = algorithms,
    type   = RatingType,
    n      = seq_len(RatingsKeep))

  # determine winning model - highest TPR for next best 10 products----
  # start by averaging Confusion Matrix for all k-fold runs
  n <- length(results)
  store <- list()
  for(i in seq_len(n)) {
    temp <- data.table(recommenderlab::avg(results)[[i]])
    temp[, model := results[[i]]@method]
    temp[, n_products := seq(1:RatingsKeep)]
    store[[i]] <- temp
  }

  # Collect results in one data.table----
  x <- data.table::rbindlist(store)
  WinningModel <- x[n_products == 10][order(-get(ModelMetric))][1, "model"][[1L]]
  return(WinningModel)
}

#' The AutoRecomScoring function scores recommender models from AutoRecommender()
#'
#' This function will take your ratings matrix and model and score your data in parallel.
#' @author Adrian Antico and Douglas Pestana
#' @family Recommenders
#' @param data The binary ratings matrix from RecomDataCreate()
#' @param WinningModel The winning model returned from AutoRecommender()
#' @param EntityColName Typically your customer ID
#' @param ProductColName Something like "StockCode"
#' @param NumItemsReturn Number of items to return on scoring
#' @return Returns the prediction data
#' @examples
#' \dontrun{
#' Results <- AutoRecommenderScoring(
#'   data = AutoRecomDataCreate(
#'       data,
#'       EntityColName = "CustomerID",
#'       ProductColName = "StockCode",
#'       MetricColName = "TotalSales"),
#'   WinningModel = AutoRecommender(
#'       AutoRecomDataCreate(
#'         data,
#'         EntityColName = "CustomerID",
#'         ProductColName = "StockCode",
#'         MetricColName = "TotalSales"),
#'       Partition = "Split",
#'       KFolds = 2,
#'       Ratio = 0.75,
#'       RatingType = "TopN",
#'       RatingsKeep = 20,
#'       SkipModels = "AssociationRules",
#'       ModelMetric = "TPR"),
#'   EntityColName = "CustomerID",
#'   ProductColName = "StockCode")
#' }
#' @export
AutoRecommenderScoring <- function(data,
                                   WinningModel,
                                   EntityColName  = "CustomerID",
                                   ProductColName = "StockCode",
                                   NumItemsReturn = 1) {

  # data.table optimize----
  if(parallel::detectCores() > 10) data.table::setDTthreads(threads = max(1L, parallel::detectCores() - 2L)) else data.table::setDTthreads(threads = max(1L, parallel::detectCores()))
  requireNamespace('parallel', quietly = FALSE)
  requireNamespace('doParallel', quietly = FALSE)
  requireNamespace("data.table", quietly = FALSE)

  # Setup winning model and arguments----
  if(WinningModel == "AR") {
    recommender <- recommenderlab::Recommender(data = data, method = "AR", parameter = list(support = 0.001, confidence = 0.05))
  } else {
    recommender <- recommenderlab::Recommender(data = data, method = WinningModel)
  }

  # Setup the parallel environment----
  packages <- c("curl", "reshape2", "recommenderlab", "data.table")
  cores    <- parallel::detectCores()
  parts    <- floor(nrow(data) * ncol(data) / 250000)
  cl       <- parallel::makePSOCKcluster(cores)
  doParallel::registerDoParallel(cl)

  # Begin scoring----
  results <- foreach::foreach(
    i = itertools::isplitRows(data, chunks = parts),
    .combine = function(...) data.table::rbindlist(list(...)),
    .multicombine = TRUE,
    .packages = packages
  ) %dopar% {
    data <- methods::as(recommenderlab::predict(
      recommender,
      i,
      type = "topNList",
      n = NumItemsReturn),
      "list")

    # Data transformations----
    temp <- data.table::data.table(data.table::melt(data))
    data.table::setcolorder(temp, c(2, 1))
    data.table::setnames(temp, c("L1", "value"), c(EntityColName, ProductColName))
    temp
  }

  # shut down parallel objects----
  parallel::stopCluster(cl)
  rm(cl)

  # Finalize data transformations: append list of data.tables, add ProductRank, gsub x 2, add ts
  if(!data.table::is.data.table(results)) results <- data.table::as.data.table(results)
  results[, ProductRank := seq_len(.N), by = eval(EntityColName)]
  results[, ':=' (TimeStamp = as.character(Sys.time()))]
  return(results)
}
