#' @title ParDepCalPlots
#'
#' @description This function automatically builds partial dependence calibration plots and partial dependence calibration boxplots for model evaluation using regression, quantile regression, and binary and multinomial classification
#' @author Adrian Antico
#' @family Model Evaluation and Interpretation
#'
#' @param data Data containing predicted values and actual values for comparison
#' @param PredictionColName Predicted values column names
#' @param TargetColName Target value column names
#' @param IndepVar Independent variable column names
#' @param GraphType calibration or boxplot - calibration aggregated data based on summary statistic; boxplot shows variation
#' @param PercentileBucket Number of buckets to partition the space on (0,1) for evaluation
#' @param FactLevels The number of levels to show on the chart (1. Levels are chosen based on frequency; 2. all other levels grouped and labeled as "Other")
#' @param Function Supply the function you wish to use for aggregation.
#' @return Partial dependence calibration plot or boxplot
#' @examples
#' \dontrun{
#' # Create fake data
#' data <- RemixAutoML::FakeDataGenerator(
#'   Correlation = 0.70, N = 10000000, Classification = FALSE)
#' data.table::setnames(data, "Independent_Variable2", "Predict")
#'
#' # Build plot
#' Plot <- RemixAutoML::ParDepCalPlots(
#'   data,
#'   PredictionColName = "Predict",
#'   TargetColName = "Adrian",
#'   IndepVar = "Independent_Variable1",
#'   GraphType = "calibration",
#'   PercentileBucket = 0.20,
#'   FactLevels = 10,
#'   Function = function(x) mean(x, na.rm = TRUE))
#' }
#' @export
ParDepCalPlots <- function(data,
                           PredictionColName = c("PredictedValues"),
                           TargetColName  = c("ActualValues"),
                           IndepVar = c("Independent_Variable_Name"),
                           GraphType = c("calibration"),
                           PercentileBucket = 0.05,
                           FactLevels = 10,
                           Function = function(x) mean(x, na.rm = TRUE)) {

  # Turn off ggplot2 warnings----
  options(warn = -1L)

  # Build buckets by independent variable of choice----
  preds2 <- data.table::as.data.table(data)

  # Cap number of records----
  if(data[,.N] > 1000000) data <- data[order(runif(.N))][1:1000000]

  # Subset columns----
  cols <- c(PredictionColName, TargetColName, IndepVar)
  preds2 <- preds2[, ..cols]

  # Structure data----
  data <- data[, ..cols]
  data.table::setcolorder(data, c(PredictionColName, TargetColName, IndepVar))

  # If actual is in factor form, convert to numeric----
  if(!is.numeric(preds2[[TargetColName]])) {
    preds2[, eval(TargetColName) := as.numeric(as.character(get(TargetColName)))]
    GraphType <- "calibration"
  }

  # Prepare for both calibration and boxplot----
  if(is.numeric(preds2[[IndepVar]]) | is.integer(preds2[[IndepVar]])) {
    preds2[, rank := round(data.table::frank(preds2[[IndepVar]]) * (1/PercentileBucket) /.N) * PercentileBucket]
  } else {
    GraphType <- "FactorVar"
    preds2[, id := seq_len(.N), by = get(IndepVar)]
    preds2 <- preds2[, .(Function(get(TargetColName)), Function(get(PredictionColName)), max(id)), by = get(IndepVar)][order(-V3)]
    if(nrow(preds2) > FactLevels) {
      temp1 <- preds2[1:FactLevels][, V3 := NULL]
      temp2 <- preds2[(FactLevels + 1):nrow(preds2)]
      temp2[, ':=' (V1 = V1 * V3 / sum(V3), V2 = V2 * V3 / sum(V3))]
      temp3 <- temp2[, list(sum(V1), sum(V2))]
      temp3[, get := "Other"]
      data.table::setcolorder(temp3, c(3L, 1L, 2L))
    }
    preds2[, V3 := NULL]
    if(nrow(preds2) > FactLevels) {
      preds3 <- data.table::rbindlist(list(temp1, temp3))
    } else {
      preds3 <- preds2
    }
    data.table::setnames(preds3, old = c("get", "V1", "V2"), new = c(IndepVar, TargetColName, PredictionColName))
    preds3 <- preds3[order(-get(PredictionColName))]
  }

  # Build plots----
  if(GraphType == "calibration") {
    preds3 <- preds2[, lapply(.SD, noquote(Function)), by = rank][order(rank)]
    preds3[, eval(IndepVar) := as.numeric(get(IndepVar))]

    # Partial dependence calibration plot----
    plot <- ggplot2::ggplot(preds3, ggplot2::aes(x = preds3[[IndepVar]])) +
      ggplot2::geom_line(ggplot2::aes(y = preds3[[PredictionColName]], color = "Predicted")) +
      ggplot2::geom_line(ggplot2::aes(y = preds3[[TargetColName]], color = "Actuals")) +
      ggplot2::ylab("Actual | Predicted") +
      ggplot2::xlab(IndepVar) +
      ggplot2::scale_colour_manual("", breaks = c("Actuals", "Predicted"), values = c("red", "blue")) +
      ChartTheme(Size = 15) +
      ggplot2::ggtitle("Partial Dependence Calibration Plot")
  } else if (GraphType == "boxplot") {
    keep <- c("rank", TargetColName, IndepVar)
    actual <- preds2[, ..keep]
    actual[, Type := "actual"]
    data.table::setnames(actual, TargetColName, "Output")
    keep <- c("rank", PredictionColName, IndepVar)
    predicted <- preds2[, ..keep]
    predicted[, Type := "predicted"]
    data.table::setnames(predicted, PredictionColName, "Output")
    data <- data.table::rbindlist(list(actual, predicted))[order(rank)]
    data[, rank := as.factor(rank)]
    data <- data[, eval(IndepVar) := as.numeric(get(IndepVar))]
    data <- data[, eval(IndepVar) := round(Function(get(IndepVar)), 3L), by = rank]
    data[, eval(IndepVar) := as.factor(get(IndepVar))]
    data[, rank := NULL]
    plot <- ggplot2::ggplot(data, ggplot2::aes(x = data[[IndepVar]], y = Output)) +
      ggplot2::geom_boxplot(ggplot2::aes(fill = Type)) +
      ggplot2::scale_fill_manual(values = c("red", "blue")) +
      ggplot2::ggtitle("Partial Dependence Calibration Boxplot") +
      ggplot2::xlab(eval(IndepVar)) +
      ggplot2::ylab("Actual | Predicted") +
      ChartTheme(Size = 15)
  } else if (GraphType == "FactorVar") {
    keep <- c(IndepVar, TargetColName)
    actual <- preds3[, ..keep]
    actual[, Type := "actual"]
    data.table::setnames(actual, TargetColName, "Output")
    keep <- c(IndepVar, PredictionColName)
    predicted <- preds3[, ..keep]
    predicted[, Type := "predicted"]
    data.table::setnames(predicted, PredictionColName, "Output")
    data <- data.table::rbindlist(list(actual, predicted))[order(-Output)]
    plot <- ggplot2::ggplot(data, ggplot2::aes(x = data[[IndepVar]], y = Output)) +
      ggplot2::geom_bar(stat = "identity", position = "dodge", ggplot2::aes(fill = Type)) +
      ggplot2::scale_fill_manual(values = c("red", "blue")) +
      ggplot2::ggtitle("Partial Dependence Calibration Barplot") +
      ggplot2::xlab(eval(IndepVar)) +
      ggplot2::ylab("Actual | Predicted") +
      ChartTheme(Size = 15)
  }

  # Return plot----
  return(plot)
}
