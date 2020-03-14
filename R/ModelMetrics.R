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
    MetricCollection <- merge(MetricCollection, DATA[, .(MAE_Metric = mean(Metric, na.rm = TRUE)), by = list(GroupVar)],
                              by = "GroupVar",
                              all = FALSE)
  } else {
    data.table::set(DATA, j = "Metric", value = abs(DATA[[eval(TARGETCOLUMNNAME)]] - DATA[["Predictions"]]))
    data.table::set(MetricCollection, i = 1L, j = "MetricValue", value = mean(DATA[["Metric"]], na.rm = TRUE))
  }
  
  # MAPE----
  if (!is.null(GROUPVARIABLES)) {
    data.table::set(DATA, j = "Metric", value = abs((DATA[[eval(TARGETCOLUMNNAME)]] - DATA[["Predictions"]]) / (DATA[[eval(TARGETCOLUMNNAME)]] + 1)))
    MetricCollection <- merge(MetricCollection, DATA[, .(MAPE_Metric = mean(Metric, na.rm = TRUE)), by = list(GroupVar)],
                              by = "GroupVar",
                              all = FALSE)
  } else {
    data.table::set(DATA, j = "Metric", value = abs((DATA[[eval(TARGETCOLUMNNAME)]] - DATA[["Predictions"]]) / (DATA[[eval(TARGETCOLUMNNAME)]] + 1)))
    data.table::set(MetricCollection, i = 3L, j = "MetricValue", value = round(mean(DATA[["Metric"]], na.rm = TRUE),3))
  }
  
  # MSE----
  if (!is.null(GROUPVARIABLES)) {
    data.table::set(DATA, j = "Metric", value = (DATA[[eval(TARGETCOLUMNNAME)]] - DATA[["Predictions"]]) ^ 2)
    MetricCollection <- merge(MetricCollection, DATA[, .(MSE_Metric = mean(Metric, na.rm = TRUE)), by = list(GroupVar)],
                              by = "GroupVar",
                              all = FALSE)
  } else {
    data.table::set(DATA, j = "Metric", value = (DATA[[eval(TARGETCOLUMNNAME)]] - DATA[["Predictions"]]) ^ 2)
    data.table::set(MetricCollection, i = 2L, j = "MetricValue", value = round(mean(DATA[["Metric"]], na.rm = TRUE),3))
  }
  
  # R2----
  tryCatch({if (!is.null(GROUPVARIABLES)) {
    MetricCollection <- merge(MetricCollection, 
                              DATA[, .(R2_Metric = stats::cor(get(TARGETCOLUMNNAME), Predictions)), by = list(GroupVar)],
                              by = "GroupVar",
                              all = FALSE)
    data.table::set(MetricCollection, j = "R2_Metric", value = MetricCollection[["R2_Metric"]]^2)
  } else {
    data.table::set(MetricCollection, i = 4L, j = "MetricValue", value = round(stats::cor(DATA[[eval(TARGETCOLUMNNAME)]], DATA[["Predictions"]]) ^ 2,3))
  }}, error = function(x) NULL)
  
  # Return----
  return(MetricCollection)
}

#' DT_BinaryConfusionMatrix
#'
#' DT_BinaryConfusionMatrix is for computing all metrics related to binary modeling outcomes
#' 
#' @family Model Evaluation
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

#' ClassificationMetrics
#'
#' ClassificationMetrics
#'
#' @author Adrian Antico
#' @family Model Evaluation
#' @param TestData Test data from your modeling
#' @param Target Name of your target variable
#' @param Predict Name of your predicted value variable
#' @export
ClassificationMetrics <- function(TestData, Target, Predict) {
  ThresholdOutput <- data.table::data.table(
    Threshold = rep(1,99),
    Accuracy = rep(1,99),
    MCC = rep(1,99),
    TN = rep(1,99),
    TP = rep(1,99),
    FP = rep(1,99),
    FN = rep(1,99))
  counter <- 0L
  for(Thresh in seq(0.01,0.99,0.01)) {
    counter <- counter + 1L
    TP <- TestData[, sum(data.table::fifelse(get(Predict) < Thresh & get(Target) == 0, 1, 0))]
    TN <- TestData[, sum(data.table::fifelse(get(Predict) > Thresh & get(Target) == 1, 1, 0))]
    FN <- TestData[, sum(data.table::fifelse(get(Predict) < Thresh & get(Target) == 1, 1, 0))]
    FP <- TestData[, sum(data.table::fifelse(get(Predict) > Thresh & get(Target) == 0, 1, 0))]
    N  <- TestData[,.N]
    P  <- TestData[get(Target) == 0, .N]
    acc <- (TP+TN)/N
    TPR <- TP/P
    TNR <- TN/(N-P)
    MCC <- (TP*TN-FP*FN)/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
    data.table::set(ThresholdOutput, i = counter, j = "Threshold", value = Thresh)
    data.table::set(ThresholdOutput, i = counter, j = "Accuracy", value = acc)
    data.table::set(ThresholdOutput, i = counter, j = "MCC", value = MCC)
    data.table::set(ThresholdOutput, i = counter, j = "TP", value = TP)
    data.table::set(ThresholdOutput, i = counter, j = "FP", value = FP)
    data.table::set(ThresholdOutput, i = counter, j = "TN", value = TN)
    data.table::set(ThresholdOutput, i = counter, j = "FN", value = FN)
  }
  return(ThresholdOutput)
}

#' RemixClassificationMetrics
#'
#' RemixClassificationMetrics
#'
#' @author Adrian Antico
#' @family Model Evaluation
#' @param MLModels A vector of model names from remixautoml
#' @param TargetVariable Name of your target variable
#' @param CatBoostTestData Test data returned from AutoCatBoostClassifier
#' @param H2oGBMTestData Test data returned from AutoCatBoostClassifier
#' @param H2oDRFTestData Test data returned from AutoCatBoostClassifier
#' @param XGBoostTestData Test data returned from AutoCatBoostClassifier
#' @export
RemixClassificationMetrics <- function(MLModels = c("catboost","h2ogbm","h2odrf","xgboost"),
                                       TargetVariable = "Value",
                                       CatBoostTestData = CatModel$ValidationData,
                                       H2oGBMTestData = H2oGBMModel$ValidationData,
                                       H2oDRFTestData = H2oDRFModel$ValidationData,
                                       XGBoostTestData = XGBoostModel$ValidationData) {
  
  # Store output----
  ThresholdOutput <- list()
  
  # CatBoost----
  if(any(tolower(MLModels) == "catboost")) {
    if(!"p1" %in% names(CatBoostTestData)) data.table::setnames(CatBoostTestData, "Predict", "p1")
    temp <- ClassificationMetrics(
      TestData = CatBoostTestData,
      Target = eval(TargetVariable),
      Predict = "p1")
    data.table::setorderv(temp, cols = "MCC", order = -1L)
    data.table::setnames(temp, c("Accuracy","MCC","TN","TP","FP","FN"), c("Cat_Acc","Cat_MCC","Cat_TN","Cat_TP","Cat_FP","Cat_FN"))
    print("catboost here")
    ThresholdOutput[["catboost"]] <- temp
  }
  
  # H2oGBMBoost----
  if(any(tolower(MLModels) == "h2ogbm")) {
    if(!"p1" %in% names(H2oGBMTestData)) data.table::setnames(H2oGBMTestData, "Predict", "p1")
    temp <- ClassificationMetrics(
      TestData = H2oGBMTestData,
      Target = eval(TargetVariable),
      Predict = "p1")
    data.table::setorderv(temp, cols = "MCC", order = -1L)
    data.table::setnames(temp, c("Accuracy","MCC","TN","TP","FP","FN"), c("GBM_Acc","GBM_MCC","GBM_TN","GBM_TP","GBM_FP","GBM_FN"))
    print("h2ogbm here")
    ThresholdOutput[["h2ogbm"]] <- temp
  }
  
  # H2oDRFBoost----
  if(any(tolower(MLModels) == "h2odrf")) {
    if(!"p1" %in% names(H2oDRFTestData)) data.table::setnames(H2oDRFTestData, "Predict", "p1")
    temp <- ClassificationMetrics(
      TestData = H2oDRFTestData,
      Target = eval(TargetVariable),
      Predict = "p1")
    data.table::setorderv(temp, cols = "MCC", order = -1L)
    data.table::setnames(temp, c("Accuracy","MCC","TN","TP","FP","FN"), c("DRF_Acc","DRF_MCC","DRF_TN","DRF_TP","DRF_FP","DRF_FN"))
    print("h2odrf here")
    ThresholdOutput[["h2odrf"]] <- temp
  }
  
  # XGBoost----
  if(any(tolower(MLModels) == "xgboost")) {
    if(!"p1" %in% names(XGBoostTestData)) data.table::setnames(XGBoostTestData, "Predict", "p1")
    if(!TargetVariable %in% names(XGBoostTestData)) data.table::setnames(XGBoostTestData, "Target", eval(TargetVariable))
    temp <- ClassificationMetrics(
      TestData = XGBoostTestData,
      Target = TargetVariable,
      Predict = "p1")
    data.table::setorderv(temp, cols = "MCC", order = -1L)
    data.table::setnames(temp, c("Accuracy","MCC","TN","TP","FP","FN"), c("XGB_Acc","XGB_MCC","XGB_TN","XGB_TP","XGB_FP","XGB_FN"))
    print("xgboost here")
    ThresholdOutput[["xgboost"]] <- temp
  }
  
  # Combine output----
  incr <- 0L
  for(val in seq_len(length(ThresholdOutput))) {
    incr <- incr + 1L
    if(length(ThresholdOutput) == 1L) {
      return(ThresholdOutput[[val]])
    } else if(incr == 1L) {
      Metrics <- ThresholdOutput[[val]]
    } else if(incr > 1L) {
      Metrics <- merge(Metrics, ThresholdOutput[[val]], by = "Threshold", all = FALSE)
    }
  }
  
  # Return values----
  return(Metrics)
}
