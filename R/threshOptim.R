#' Utility maximizing thresholds for binary classification
#'
#' This function will return the utility maximizing threshold for future predictions along with the data generated to estimate the threshold
#'
#' @author Adrian Antico
#' @family Model Evaluation and Interpretation
#' @param data data is the data table you are building the modeling on
#' @param actTar The column name where the actual target variable is located (in binary form)
#' @param predTar The column name where the predicted values are located
#' @param tpProfit This is the utility for generating a true positive prediction
#' @param tnProfit This is the utility for generating a true negative prediction
#' @param fpProfit This is the cost of generating a false positive prediction
#' @param fnProfit This is the cost of generating a false negative prediction
#' @param MinThresh Minimum value to consider for model threshold
#' @param MaxThresh Maximum value to consider for model threshold
#' @param ThresholdPrecision Incrementing value in search
#' @examples
#' data <- data.table::data.table(Target = runif(10))
#' data[, x1 := qnorm(Target)]
#' data[, x2 := runif(10)]
#' data[, Predict := log(pnorm(0.85 * x1 +
#'                               sqrt(1-0.85^2) * qnorm(x2)))]
#' data[, ':=' (x1 = NULL, x2 = NULL)]
#' data <- threshOptim(data     = data,
#'                     actTar   = "Target",
#'                     predTar  = "Predict",
#'                     tpProfit = 0,
#'                     tnProfit = 0,
#'                     fpProfit = -1,
#'                     fnProfit = -2,
#'                     MinThresh = 0.001,
#'                     MaxThresh = 0.999,
#'                     ThresholdPrecision = 0.001)
#' optimalThreshold <- data$Thresholds
#' allResults <- data$EvaluationTable
#' @return Optimal threshold and corresponding utilities for the range of thresholds tested
#' @export
threshOptim <- function(data,
                        actTar   = "target",
                        predTar  = "p1",
                        tpProfit = 0,
                        tnProfit = 0,
                        fpProfit = -1,
                        fnProfit = -2,
                        MinThresh = 0.001,
                        MaxThresh = 0.999,
                        ThresholdPrecision = 0.001) {
  # Check data.table
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Convert factor target to numeric
  data[, eval(actTar) := as.numeric(as.character(get(actTar)))]
  
  # Optimize each column's classification threshold ::
  popTrue <- base::mean(data[[(actTar)]])
  store   <- list()
  j <- 0
  options(warn = -1)
  for (i in seq(from = MinThresh, to = MaxThresh, by = ThresholdPrecision)) {
    j <- j + 1
    tp      <-
      base::sum(ifelse(data[[actTar]] == 1 &
                         data[[predTar]] >= i, 1, 0))
    tn      <-
      base::sum(ifelse(data[[actTar]] == 0 &
                         data[[predTar]] <  i, 1, 0))
    fp      <-
      base::sum(ifelse(data[[actTar]] == 0 &
                         data[[predTar]] >= i, 1, 0))
    fn      <-
      base::sum(ifelse(data[[actTar]] == 1 &
                         data[[predTar]] <  i, 1, 0))
    tpr     <- ifelse((tp + fn) == 0, 0, tp / (tp + fn))
    fpr     <- ifelse((fp + tn) == 0, 0, fp / (fp + tn))
    utility <-
      popTrue * (tpProfit * tpr +
                   fnProfit * (1 - tpr)) +
      (1 - popTrue) * (fpProfit * fpr + tnProfit * (1 - fpr))
    store[[j]] <- c(i, utility)
  }
  all <- data.table::rbindlist(list(store))
  utilities <- data.table::melt(all[2,])
  data.table::setnames(utilities, "value", "Utilities")
  thresholds <- data.table::melt(all[1,])
  data.table::setnames(thresholds, "value", "Thresholds")
  results <- cbind(utilities, thresholds)[, c(-1,-3)]
  thresh <- results[order(-Utilities)][1, 2][[1]]
  options(warn = 1)
  return(list(Thresholds = thresh, EvaluationTable = results))
}
