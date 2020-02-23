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
#' Correl <- 0.85
#' data <- data.table::data.table(Target = runif(100))
#' data[, x1 := qnorm(Target)]
#' data[, x2 := runif(100)]
#' data[, Independent_Variable1 := log(pnorm(Correl * x1 +
#'                                             sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Predict := (pnorm(Correl * x1 +
#'                            sqrt(1-Correl^2) * qnorm(x2)))]
#' EvalPlot(data,
#'          PredictionColName = "Predict",
#'          TargetColName = "Target",
#'          GraphType = "calibration",
#'          PercentileBucket = 0.05,
#'          aggrfun = function(x) quantile(x, probs = 0.50, na.rm = TRUE))
#' @export
EvalPlot <- function(data,
                     PredictionColName = c("PredictedValues"),
                     TargetColName  = c("ActualValues"),
                     GraphType        = c("calibration"),
                     PercentileBucket = 0.05,
                     aggrfun     = function(x)
                       mean(x, na.rm = TRUE)) {
  
  # Turn on full speed ahead----
  data.table::setDTthreads(percent = 100)
  
  # Turn data into data.table if not already
  if (!data.table::is.data.table(data))
    data <- data.table::as.data.table(data)
  
  # Structure data
  cols <- c(eval(PredictionColName), eval(TargetColName))
  data <- data[, ..cols]
  data.table::setcolorder(data, c(PredictionColName, TargetColName))
  data.table::setnames(data,
                       c(PredictionColName, TargetColName),
                       c("preds", "acts"))
  
  # If actual is in factor form, convert to numeric
  if (!is.numeric(data[["acts"]])) {
    data.table::set(data, j = "acts", value = as.numeric(as.character(data[["acts"]])))
    GraphType <- "calibration"
  }
  
  # Add a column that ranks predicted values
  data.table::set(data,
                  j = "rank",
                  value = 100 * (round(percRank(data[[1]]) / PercentileBucket) * PercentileBucket))
  
  # Plot
  if (GraphType == "boxplot") {
    # Remove classification and non-event predicted values
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
    plot <-
      ggplot2::ggplot(data, ggplot2::aes(x = rank,
                                         y = output,
                                         fill = Type)) +
      ggplot2::geom_boxplot(outlier.color = "red",
                            color = "black") +
      ggplot2::ggtitle("Calibration Evaluation Boxplot") +
      ggplot2::xlab("Predicted Percentile") +
      ggplot2::ylab("Observed Values") +
      ChartTheme(Size = 15) +
      ggplot2::scale_fill_manual(values = c("blue",
                                            "red"))
    
  } else {
    # Aggregate all columns by rank, utilizing mean as the aggregator statistic
    data <- data[, lapply(.SD, noquote(aggrfun)), by = rank]
    
    # Build calibration plot
    plot  <- ggplot2::ggplot(data, ggplot2::aes(x = rank))  +
      ggplot2::geom_line(ggplot2::aes(y = data[[3]],
                                      color = "Actual")) +
      ggplot2::geom_line(ggplot2::aes(y = data[[2]],
                                      color = "Predicted")) +
      ggplot2::xlab("Predicted Percentile") +
      ggplot2::ylab("Observed Values") +
      ggplot2::scale_color_manual(values = c("red", "blue")) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                         hjust = 1)) +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::ggtitle("Calibration Evaluation Plot") +
      ChartTheme(Size = 15) +
      ggplot2::scale_fill_manual(values = c("blue",
                                            "gold"))
  }
  return(plot)
}
