#' Automatically build the best recommender model among models available.
#'
#' This function returns the winning model that you pass onto AutoRecommenderScoring
#' @author Adrian Antico and Douglas Pestana
#' @family Marketing Modeling
#' @param data This is your BinaryRatingsMatrix. See function RecomDataCreate
#' @param Partition Choose from "split", "cross-validation", "bootstrap". See evaluationScheme in recommenderlab for details.
#' @param KFolds Choose 2 for traditional train and test. Choose greater than 2 for the number of cross validations
#' @param Ratio The ratio for train and test. E.g. 0.75 for 75 percent data allocated to training
#' @param ProductEvaluation The number of products you would like to evaluate. Negative values implement all-but schemes.   
#' @param RatingType Choose from "TopN", "ratings", "ratingMatrix"
#' @param RatingsKeep The total ratings you wish to return. Default is 20.
#' @param SkipModels AssociationRules runs the slowest and may crash your system. Choose from: "AssociationRules","ItemBasedCF","UserBasedCF","PopularItems","RandomItems"
#' @param ModelMetric Choose from "Precision", "Recall", "TPR", or "FPR"
#' @examples
#' \donttest{
#' WinningModel <- AutoRecommender(RatingsMatrix,
#'                                 Partition = "Split",
#'                                 KFolds = 2,
#'                                 Ratio = 0.75,
#'                                 ProductEvaluation = 1,
#'                                 RatingType = "TopN",
#'                                 RatingsKeep = 20,
#'                                 SkipModels = "AssociationRules",
#'                                 ModelMetric = "TPR")
#' }
#' @return The winning model used for scoring in the AutoRecommenderScoring function
#' @export
AutoRecommender <- function(data,
                            Partition   = "Split",
                            KFolds      = 1,
                            Ratio       = 0.75,
                            ProductEvaluation = 1,
                            RatingType  = "TopN",
                            RatingsKeep = 20,
                            SkipModels  = "AssociationRules",
                            ModelMetric = "TPR") {
  # Ensure data is proper----
  if (class(data)[1] != "binaryRatingMatrix") {
    warning("data must be of class binaryRatingMatrix")
  }
  
  # Ensure KFolds is correct----
  if (tolower(Partition) == "split") {
    KFolds <- 1
  }
  
  # Ensure Ratio is proper----
  if (abs(Ratio) > 1 | Ratio == 0) {
    warning("Ratio must be a decimal between 0 and 1.
         Default is 0.75")
  }
  
  # Ensure RatingType is real----
  if (tolower(RatingType) == "topn") {
    RatingType <- "topNList"
  } else if (tolower(RatingType) == "ratings") {
    RatingType <- "ratings"
  } else if (tolower(RatingType) == "ratingMatrix") {
    RatingType <- "ratingMatrix"
  }
  
  # Pick winning model based max TPR for 10th recommendation----
  if (tolower(ModelMetric) == "precision") {
    ModelMetric <- "precision"
  } else if (tolower(ModelMetric) == "recall") {
    ModelMetric <- "recall"
  } else if (tolower(ModelMetric) == "tpr") {
    ModelMetric <- "TPR"
  } else if (tolower(ModelMetric) == "fpr") {
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
    given      = ProductEvaluation,
    goodRating = 1
  )
  
  # Store algorithms in nested list----
  algorithms <- list(
    "RandomItems"  = list(name = "RANDOM",  param = NULL),
    "PopularItems" = list(name = "POPULAR", param = NULL),
    "UserBasedCF" = list(name = "UBCF",    param = NULL),
    "ItemBasedCF" = list(name = "IBCF",    param = NULL),
    "AssociationRules" = list(
      name = "AR",
      param = list(support = 0.001, confidence = 0.05)
    )
  )
  
  # Remove all algos in SkipModels----
  if (any(tolower(SkipModels) == "associationrules")) {
    algorithms[["AssociationRules"]] <- NULL
  }
  if (any(tolower(SkipModels) == "itembasedcf")) {
    algorithms[["ItemBasedCF"]] <- NULL
  }
  if (any(tolower(SkipModels) == "userbasedcf")) {
    algorithms[["UserBasedCF"]] <- NULL
  }
  if (any(tolower(SkipModels) == "popularitems")) {
    algorithms[["PopularItems"]] <- NULL
  }
  if (any(tolower(SkipModels) == "randomitems")) {
    algorithms[["RandomItems"]] <- NULL
  }
  if (length(algorithms) == 0) {
    warning("You must have at least one algorithm to run")
  }
  
  # evauluate predicted ratings from each algorithm----
  results <- recommenderlab::evaluate(
    x      = scheme,
    method = algorithms,
    type   = RatingType,
    n      = 1:RatingsKeep
  )
  
  # determine winning model - highest TPR for next best 10 products----
  # start by averaging Confusion Matrix for all k-fold runs
  n <- length(results)
  store <- list()
  for (i in 1:n) {
    temp <- data.table(recommenderlab::avg(results)[[i]])
    temp[, model := results[[i]]@method]
    temp[, n_products := seq(1:RatingsKeep)]
    store[[i]] <- temp
  }
  
  # Collect results in one data.table----
  x <- data.table::rbindlist(store)
  
  WinningModel <-
    x[n_products == 10][order(-get(ModelMetric))][1, "model"][[1]]
  return(WinningModel)
}
