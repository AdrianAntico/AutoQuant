#' EvalPlot automatically builds calibration plots for model evaluation
#'
#' This function automatically builds calibration plots and calibration boxplots for model evaluation using regression, quantile regression, and binary and multinomial classification
#' @author Adrian Antico
#' @family Model Evaluation and Interpretation
#' @param data Data containing predicted values and actual values for comparison
#' @param PredictionColName String representation of column name with predicted values from model
#' @param TargetColName String representation of column name with target values from model
#' @param GraphType Calibration or boxplot - calibration aggregated data based on summary statistic; boxplot shows variation
#' @param PercentileBucket Number of buckets to partition the space on (0,1) for evaluation
#' @param aggrfun The statistics function used in aggregation, listed as a function
#' @return Calibration plot or boxplot
#' @examples
#' \dontrun{
#' # Create fake data
#' data <- RemixAutoML::FakeDataGenerator(
#'   Correlation = 0.70, N = 10000000, Classification = TRUE)
#' data.table::setnames(data, "IDcol_1", "Predict")
#'
#' # Run function
#' EvalPlot(data,
#'          PredictionColName = "Predict",
#'          TargetColName = "Adrian",
#'          GraphType = "calibration",
#'          PercentileBucket = 0.05,
#'          aggrfun = function(x) mean(x, na.rm = TRUE))
#' }
#' @export
EvalPlot <- function(data,
                     PredictionColName = c("PredictedValues"),
                     TargetColName  = c("ActualValues"),
                     GraphType        = c("calibration"),
                     PercentileBucket = 0.05,
                     aggrfun     = function(x) mean(x, na.rm = TRUE)) {

  # Turn data into data.table if not already----
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  # Cap number of records----
  if(data[,.N] > 1000000) data <- data[order(runif(.N))][1:1000000]

  # Structure data
  data <- data[, .SD, .SDcols = c(eval(PredictionColName), eval(TargetColName))]
  data.table::setcolorder(data, c(PredictionColName, TargetColName))
  data.table::setnames(data, c(PredictionColName, TargetColName), c("preds", "acts"))

  # If actual is in factor form, convert to numeric----
  if(!is.numeric(data[["acts"]])) {
    data.table::set(data, j = "acts", value = as.numeric(as.character(data[["acts"]])))
    GraphType <- "calibration"
  }

  # Add a column that ranks predicted values----
  data[, rank := round(data.table::frank(preds) * (1/PercentileBucket) /.N) * PercentileBucket]

  # Plot----
  if(GraphType == "boxplot") {
    data.table::set(data, j = "rank", value = as.factor(data[["rank"]]))
    cols <- c("rank", "preds")
    zz1 <- data[, ..cols]
    zz1[, Type := 'predicted']
    data.table::setnames(zz1, c("preds"), c("output"))
    cols <- c("rank", "acts")
    zz2 <- data[, ..cols]
    zz2[, Type := 'actual']
    data.table::setnames(zz2, c("acts"), c("output"))
    data <- data.table::rbindlist(list(zz1, zz2))
    plot <- ggplot2::ggplot(data, ggplot2::aes(x = rank, y = output, fill = Type)) +
      ggplot2::geom_boxplot(outlier.color = "red", color = "black") +
      ggplot2::ggtitle("Calibration Evaluation Boxplot") +
      ggplot2::xlab("Predicted Percentile") +
      ggplot2::ylab("Observed Values") +
      ChartTheme(Size = 15) +
      ggplot2::scale_fill_manual(values = c("red", "blue"))

  } else {
    data <- data[, lapply(.SD, noquote(aggrfun)), by = list(rank)]
    plot  <- ggplot2::ggplot(data, ggplot2::aes(x = rank))  +
      ggplot2::geom_line(ggplot2::aes(y = data[[3L]], color = "Actual")) +
      ggplot2::geom_line(ggplot2::aes(y = data[[2L]], color = "Predicted")) +
      ggplot2::xlab("Predicted Percentile") +
      ggplot2::ylab("Observed Values") +
      ggplot2::scale_color_manual(values = c("red", "blue")) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::ggtitle("Calibration Evaluation Plot") +
      ChartTheme(Size = 15) +
      ggplot2::scale_fill_manual(values = c("blue", "gold"))
  }
  return(plot)
}
