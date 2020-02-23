#' CarmaHoldoutMetrics
#' 
#' CarmaHoldoutMetrics
#' 
#' @author Adrian Antico
#' @family Time Series 
#' @param DATA TestDataEval
#' @param TARGETCOLUMNNAME TargetColumnName
#' @param GROUPVARIABLES GroupVariables
#' @export
CarmaHoldoutMetrics <- function(DATA = TestDataEval, 
                                TARGETCOLUMNNAME = TargetColumnName, 
                                GROUPVARIABLES = GroupingVariables) {
  
  # Start----
  if (!is.null(GROUPVARIABLES)) {
    MetricCollection <- DATA[, GroupVar, by = "GroupVar"][, GroupVar := NULL]
  } else {
    MetricCollection <- data.table::data.table(Metric = c("MAE","MSE","MAPE","R2"), MetricValue = rep(0,4))
  }
  
  # MAE----
  if (!is.null(GROUPVARIABLES)) {
    data.table::set(DATA, j = "Metric", value = abs(DATA[[eval(TARGETCOLUMNNAME)]] - DATA[["Predictions"]]))
    MetricCollection <- merge(MetricCollection, DATA[, .(MAE_Metric = mean(Metric, na.rm = TRUE)), by = "GroupVar"],
                              by = "GroupVar",
                              all = FALSE)
  } else {
    data.table::set(DATA, j = "Metric", value = abs(DATA[[eval(TARGETCOLUMNNAME)]] - DATA[["Predictions"]]))
    data.table::set(MetricCollection, i = 1L, j = "MetricValue", value = mean(DATA[["Metric"]], na.rm = TRUE))
  }
  
  # MAPE----
  if (!is.null(GROUPVARIABLES)) {
    DATA[, Metric := abs((get(TARGETCOLUMNNAME) - Predictions) / (get(TARGETCOLUMNNAME) + 1))]
    MetricCollection <- merge(MetricCollection, DATA[, .(MAPE_Metric = mean(Metric, na.rm = TRUE)), by = "GroupVar"],
                              by = "GroupVar",
                              all = FALSE)
  } else {
    data.table::set(DATA, j = "Metric", value = abs((DATA[[eval(TARGETCOLUMNNAME)]] - DATA[["Predictions"]]) / (DATA[[eval(TARGETCOLUMNNAME)]] + 1)))
    data.table::set(MetricCollection, i = 3L, j = "MetricValue", value = round(mean(DATA[["Metric"]], na.rm = TRUE),3))
  }
  
  # MSE----
  if (!is.null(GROUPVARIABLES)) {
    DATA[, Metric := (get(TARGETCOLUMNNAME) - Predictions) ^ 2]
    MetricCollection <- merge(MetricCollection, DATA[, .(MSE_Metric = mean(Metric, na.rm = TRUE)), by = "GroupVar"],
                              by = "GroupVar",
                              all = FALSE)
  } else {
    data.table::set(DATA, j = "Metric", value = (DATA[[eval(TARGETCOLUMNNAME)]] - DATA[["Predictions"]]) ^ 2)
    data.table::set(MetricCollection, i = 2L, j = "MetricValue", value = round(mean(DATA[["Metric"]], na.rm = TRUE),3))
  }
  
  # R2----
  if (!is.null(GROUPVARIABLES)) {
    DATA[, Metric := stats::cor(DATA[[eval(TARGETCOLUMNNAME)]], DATA[["Predictions"]]), by = "GroupVar"]
    MetricCollection <- merge(MetricCollection, DATA[, .(R2_Metric = stats::cor(get(TARGETCOLUMNNAME), Predictions)), 
                                                     by = "GroupVar"],
            by = "GroupVar",
            all = FALSE)
    MetricCollection[, R2_Metric := R2_Metric ^ 2]
  } else {
    data.table::set(MetricCollection, i = 4L, j = "MetricValue", value = round(stats::cor(DATA[[eval(TARGETCOLUMNNAME)]], DATA[["Predictions"]]) ^ 2,3))
  }
  
  # Return----
  return(MetricCollection)
}

#' DT_BinaryConfusionMatrix
#'
#' DT_BinaryConfusionMatrix is for computing all metrics related to binary modeling outcomes
#' 
#' @family Supervised Machine Learning
#' @author Adrian Antico
#' @param data Supply your model validation data with predictions
#' @param GroupVariables Supply grouping variables to generate statistics by groups
#' @param Target The name of your target variable column
#' @param Predicted The name of your predicted value column#'
#' @examples 
#' \donttest{
#' AggMetricsByGroup <- DT_BinaryConfusionMatrix(
#'   data,
#'   GroupVariables = c("Store","Dept"),
#'   Target = "HitTarget",
#'   Predicted = "p1")
#' }
#' @export
DT_BinaryConfusionMatrix <- function(data = MetricsData, 
                                     GroupVariables = "IntervalNum", 
                                     Target = "ActiveAtInterval", 
                                     Predicted = "p1") {
  
  # Build columns to get info----
  data.table::set(data, j = "Correct", value = data.table::fifelse(data[[eval(Predicted)]] == data[[eval(Target)]], 1, 0))
  data.table::set(
    data, j = "Correct_1", 
    value = data.table::fifelse(
      data[[eval(Predicted)]] == data[[(Target)]] & 
        data[[eval(Target)]] == 1, 1, 0))  
  data.table::set(
    data, j = "Correct_0",
    value = data.table::fifelse(
      data[[eval(Predicted)]] == data[[eval(Target)]] & 
        data[[eval(Target)]] == 0, 1, 0))
  data.table::set(
    data, j = "Incorrect_1", 
    value = data.table::fifelse(
      data[[(Predicted)]] != data[[eval(Target)]] & 
        data[[eval(Target)]] == 0, 1, 0))
  data.table::set(
    data, j = "Incorrect_0", 
    value = data.table::fifelse(
      data[[eval(Predicted)]] != data[[eval(Target)]] & 
        data[[eval(Target)]] == 1, 1, 0))
  
  # Compute confusion matrix by group----
  AggData <- data[, .(Counts = .N,
           P  = sum(get(Target) == 0, na.rm = TRUE),
           N  = sum(get(Target) == 1, na.rm = TRUE),
           TP = sum(Correct_1, na.rm = TRUE),
           TN = sum(Correct_0, na.rm = TRUE),
           FP = sum(Incorrect_1, na.rm = TRUE),
           FN = sum(Incorrect_0, na.rm = TRUE)),
       by = c(GroupVariables)][order(GroupVariables)]
  
  # Add other confusion matrix measures----
  data.table::set(AggData, j = "Accuracy",     value = (AggData[["TP"]] + AggData[["TN"]]) / (AggData[["Counts"]]))
  data.table::set(AggData, j = "Precision",    value = AggData[["TP"]] / (AggData[["TP"]] + AggData[["FP"]]))
  data.table::set(AggData, j = "NegPredValue", value = AggData[["TN"]] / (AggData[["TN"]] + AggData[["FN"]]))
  data.table::set(AggData, j = "F1_Score",     value = 2 * AggData[["TP"]] / (2 * AggData[["TP"]] + data[["FP"]] + data[["FN"]]))
  data.table::set(AggData, j = "MCC",          value = (AggData[["TP"]] * AggData[["TN"]] - AggData[["FP"]] * AggData[["FN"]]) / sqrt((AggData[["TP"]] + AggData[["FP"]]) * (AggData[["TP"]] + AggData[["FN"]]) * (AggData[["TN"]] + AggData[["FP"]]) * (AggData[["TN"]] + AggData[["FN"]])))
  data.table::set(AggData, j = "TPR",          value = AggData[["TP"]] / (AggData[["TP"]] + AggData[["FN"]]))
  data.table::set(AggData, j = "TNR",          value = AggData[["TN"]] / (AggData[["TN"]] + AggData[["FP"]]))
  data.table::set(AggData, j = "FNR",          value = AggData[["FN"]] / (AggData[["FN"]] + AggData[["TP"]]))
  data.table::set(AggData, j = "FPR",          value = AggData[["FP"]] / (AggData[["FP"]] + AggData[["TN"]]))
  data.table::set(AggData, j = "FDR",          value = AggData[["FP"]] / (AggData[["FP"]] + AggData[["TP"]]))
  data.table::set(AggData, j = "FOR",          value = AggData[["FN"]] / (AggData[["FN"]] + AggData[["TN"]]))
  data.table::set(AggData, j = "ThreatScore",  value = AggData[["TP"]] / (AggData[["TP"]] + AggData[["FN"]] + AggData[["FP"]]))
  
  # return AggData----
  return(AggData)
}

