#' @title AutoRecomScoring
#'
#' @description This function will take your ratings matrix and model and score your data in parallel.
#'
#' @author Adrian Antico and Douglas Pestana
#' @family Recommenders
#'
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
