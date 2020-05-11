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
  if(!is.null(GroupVariables)) {
    AggData <- data[, .(Counts = .N,
                        P  = sum(get(Target) == 0, na.rm = TRUE),
                        N  = sum(get(Target) == 1, na.rm = TRUE),
                        TP = sum(Correct_1, na.rm = TRUE),
                        TN = sum(Correct_0, na.rm = TRUE),
                        FP = sum(Incorrect_1, na.rm = TRUE),
                        FN = sum(Incorrect_0, na.rm = TRUE)),
                    by = c(GroupVariables)][order(GroupVariables)]
  } else {
    
    # No grouping variables----
    AggData <- data[, .(Counts = .N,
                        P  = sum(get(Target) == 0, na.rm = TRUE),
                        N  = sum(get(Target) == 1, na.rm = TRUE),
                        TP = sum(Correct_1, na.rm = TRUE),
                        TN = sum(Correct_0, na.rm = TRUE),
                        FP = sum(Incorrect_1, na.rm = TRUE),
                        FN = sum(Incorrect_0, na.rm = TRUE))]
  }
  
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
#' @param PositiveOutcome The value of the positive outcome level
#' @param NegativeOutcome The value of the negative outcome level
#' @param CostMatrix c(True Positive Cost, False Negative Cost, False Positive Cost, True Negative Cost)
#' @export
ClassificationMetrics <- function(TestData, Thresholds, Target, Predict, PositiveOutcome, NegativeOutcome, CostMatrix = c(1,0,0,1)) {
  if("Target" %chin% names(TestData)) data.table::set(TestData, j = "Target", value = NULL)
  ThreshLength <- length(Thresholds)
  ThresholdOutput <- data.table::data.table(
    Threshold   = rep(1,ThreshLength),
    TN          = rep(1,ThreshLength),
    TP          = rep(1,ThreshLength),
    FN          = rep(1,ThreshLength),
    FP          = rep(1,ThreshLength),
    N           = rep(1,ThreshLength),
    P           = rep(1,ThreshLength),
    MCC         = rep(1,ThreshLength),
    Accuracy    = rep(1,ThreshLength),
    TPR         = rep(1,ThreshLength),
    TNR         = rep(1,ThreshLength),
    FNR         = rep(1,ThreshLength),
    FPR         = rep(1,ThreshLength),
    FDR         = rep(1,ThreshLength),
    FOR         = rep(1,ThreshLength),
    F1_Score    = rep(1,ThreshLength),
    F2_Score    = rep(1,ThreshLength),
    F0.5_Score  = rep(1,ThreshLength),
    NPV         = rep(1,ThreshLength),
    PPV         = rep(1,ThreshLength),
    ThreatScore = rep(1,ThreshLength),
    Utility     = rep(1,ThreshLength))
  counter <- 0L
  for(Thresh in Thresholds) {
    counter <- counter + 1L
    if(PositiveOutcome == 1L) {
      TN <- TestData[, sum(data.table::fifelse(get(Predict) < Thresh & get(Target) == NegativeOutcome, 1, 0))]
      TP <- TestData[, sum(data.table::fifelse(get(Predict) > Thresh & get(Target) == PositiveOutcome, 1, 0))]
      FN <- TestData[, sum(data.table::fifelse(get(Predict) < Thresh & get(Target) == PositiveOutcome, 1, 0))]
      FP <- TestData[, sum(data.table::fifelse(get(Predict) > Thresh & get(Target) == NegativeOutcome, 1, 0))]
      N  <- TestData[,.N]
      P  <- TestData[get(Target) == 1, .N]
    } else {
      TN <- TestData[, sum(data.table::fifelse(get(Predict) > Thresh & get(Target) == NegativeOutcome, 1, 0))]
      TP <- TestData[, sum(data.table::fifelse(get(Predict) < Thresh & get(Target) == PositiveOutcome, 1, 0))]
      FN <- TestData[, sum(data.table::fifelse(get(Predict) > Thresh & get(Target) == PositiveOutcome, 1, 0))]
      FP <- TestData[, sum(data.table::fifelse(get(Predict) < Thresh & get(Target) == NegativeOutcome, 1, 0))]
      N  <- TestData[,.N]
      P  <- TestData[get(Target) == 1, .N]
    }
    
    # Calculate metrics----
    MCC         <- (TP*TN-FP*FN)/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
    Accuracy    <- (TP+TN)/N
    TPR         <- TP/P
    TNR         <- TN/(N-P)
    FNR         <- FN / P
    FPR         <- FP / N
    FDR         <- FP / (FP + TP)
    FOR         <- FN / (FN + TN)
    F1_Score    <- 2 * TP / (2 * TP + FP + FN)
    F2_Score    <- 3 * TP / (2 * TP + FP + FN)
    F0.5_Score  <- 1.5 * TP / (0.5 * TP + FP + FN)
    NPV         <- TN / (TN + FN)
    PPV         <- TP / (TP + FP)
    ThreatScore <- TP / (TP + FN + FP)
    Utility     <- P/N * (CostMatrix[1] * TPR + CostMatrix[2] * (1 - TPR)) + (1 - P/N) * (CostMatrix[3] * FPR + CostMatrix[4] * (1 - FPR))
    
    # Fill in values----
    data.table::set(ThresholdOutput, i = counter, j = "Threshold",   value = Thresh)
    data.table::set(ThresholdOutput, i = counter, j = "P",           value = P)
    data.table::set(ThresholdOutput, i = counter, j = "N",           value = N)
    data.table::set(ThresholdOutput, i = counter, j = "TN",          value = TN)
    data.table::set(ThresholdOutput, i = counter, j = "TP",          value = TP)
    data.table::set(ThresholdOutput, i = counter, j = "FP",          value = FP)
    data.table::set(ThresholdOutput, i = counter, j = "FN",          value = FN)
    data.table::set(ThresholdOutput, i = counter, j = "Utility",     value = Utility)
    data.table::set(ThresholdOutput, i = counter, j = "MCC",         value = MCC)
    data.table::set(ThresholdOutput, i = counter, j = "Accuracy",    value = Accuracy)
    data.table::set(ThresholdOutput, i = counter, j = "F1_Score",    value = F1_Score)
    data.table::set(ThresholdOutput, i = counter, j = "F0.5_Score",  value = F0.5_Score)
    data.table::set(ThresholdOutput, i = counter, j = "F2_Score",    value = F2_Score)
    data.table::set(ThresholdOutput, i = counter, j = "NPV",         value = NPV)
    data.table::set(ThresholdOutput, i = counter, j = "TPR",         value = TPR)
    data.table::set(ThresholdOutput, i = counter, j = "TNR",         value = TNR)
    data.table::set(ThresholdOutput, i = counter, j = "FNR",         value = FNR)
    data.table::set(ThresholdOutput, i = counter, j = "FPR",         value = FPR)
    data.table::set(ThresholdOutput, i = counter, j = "FDR",         value = FDR)
    data.table::set(ThresholdOutput, i = counter, j = "FOR",         value = FOR)
    data.table::set(ThresholdOutput, i = counter, j = "PPV",         value = PPV)
    data.table::set(ThresholdOutput, i = counter, j = "ThreatScore", value = ThreatScore)
  }
  return(ThresholdOutput)
}

#' RemixClassificationMetrics
#'
#' RemixClassificationMetrics
#'
#' @author Adrian Antico
#' @family Model Evaluation
#' @param MLModels A vector of model names from remixautoml. e.g. c("catboost","h2oautoml","h2ogbm","h2odrf","h2oglm","xgboost")
#' @param TargetVariable Name of your target variable
#' @param Thresholds seq(0.01,0.99,0.01),
#' @param CostMatrix c(1,0,0,1),
#' @param ClassLabels c(1,0),
#' @param CatBoostTestData Test data returned from AutoCatBoostClassifier
#' @param H2oAutoMLTestData Test data returned from AutoCatBoostClassifier
#' @param H2oGBMTestData Test data returned from AutoH2oGBMClassifier
#' @param H2oDRFTestData Test data returned from AutoH2oDRFClassifier
#' @param H2oGLMTestData Test data returned from AutoH2oGLMClassifier
#' @param XGBoostTestData Test data returned from AutoXGBoostClassifier
#' @examples 
#' \donttest{
#' RemixClassificationMetrics <- function(MLModels = c("catboost","h2oautoml","h2ogbm","h2odrf","xgboost"),
#'   TargetVariable = "Value",
#'   Thresholds = seq(0.01,0.99,0.01),
#'   CostMatrix = c(1,0,0,1),
#'   ClassLabels = c(1,0),
#'   CatBoostTestData = NULL,
#'   H2oAutoMLTestData = NULL,
#'   H2oGBMTestData = NULL,
#'   H2oDRFTestData = NULL,
#'   H2oGLMTestData = NULL,
#'   XGBoostTestData = NULL)
#' }
#' @export
RemixClassificationMetrics <- function(MLModels = c("catboost","h2oautoml","h2ogbm","h2odrf","xgboost"),
                                       TargetVariable = "Value",
                                       Thresholds = seq(0.01,0.99,0.01),
                                       CostMatrix = c(1,0,0,1),
                                       ClassLabels = c(1,0),
                                       CatBoostTestData = NULL,
                                       H2oAutoMLTestData = NULL,
                                       H2oGBMTestData = NULL,
                                       H2oDRFTestData = NULL,
                                       H2oGLMTestData = NULL,
                                       XGBoostTestData = NULL) {
  
  # Store output----
  ThresholdOutput <- list()
  
  # CatBoost----
  if(any(tolower(MLModels) == "catboost")) {
    if(!"p1" %chin% names(CatBoostTestData)) data.table::setnames(CatBoostTestData, "Predict", "p1")
    temp <- ClassificationMetrics(
      TestData = CatBoostTestData,
      Target = eval(TargetVariable),
      Predict = "p1",
      Thresholds = Thresholds,
      PositiveOutcome = ClassLabels[1L],
      NegativeOutcome = ClassLabels[2L],
      CostMatrix = CostMatrix)
    data.table::setorderv(temp, cols = "MCC", order = -1L)
    data.table::setnames(
      temp, 
      c("N","P","TN","TP","FP","FN","Utility","MCC","Accuracy","F1_Score","F0.5_Score","F2_Score","NPV","TPR","TNR","FNR","FPR","FDR","FOR","PPV","ThreatScore"),
      c("Cat_N","Cat_P","Cat_TN","Cat_TP","Cat_FP","Cat_FN","Cat_Utility","Cat_MCC","Cat_Acc","Cat_F1_Score","Cat_F0.5_Score","Cat_F2_Score","Cat_NPV","Cat_TPR","Cat_TNR","Cat_FNR","Cat_FPR","Cat_FDR","Cat_FOR","Cat_PPV","Cat_ThreatScore"))
    print("catboost here")
    ThresholdOutput[["catboost"]] <- temp
  }
  
  # H2oAutoML----
  if(any(tolower(MLModels) == "h2oautoml")) {
    if(!"p1" %chin% names(H2oAutoMLTestData)) data.table::setnames(H2oGBMTestData, "Predict", "p1")
    temp <- ClassificationMetrics(
      TestData = H2oAutoMLTestData,
      Target = eval(TargetVariable),
      Predict = "p1",
      Thresholds = Thresholds,
      PositiveOutcome = ClassLabels[1L], 
      NegativeOutcome = ClassLabels[2L], 
      CostMatrix = CostMatrix)
    data.table::setorderv(temp, cols = "MCC", order = -1L)
    data.table::setnames(
      temp, 
      c("N","P","TN","TP","FP","FN","Utility","MCC","Accuracy","F1_Score","F0.5_Score","F2_Score","NPV","TPR","TNR","FNR","FPR","FDR","FOR","PPV","ThreatScore"),
      c("H2oML_N","H2oML_P","H2oML_TN","H2oML_TP","H2oML_FP","H2oML_FN","H2oML_Utility","H2oML_MCC","H2oML_Acc","H2oML_F1_Score","H2oML_F0.5_Score","H2oML_F2_Score","H2oML_NPV","H2oML_TPR","H2oML_TNR","H2oML_FNR","H2oML_FPR","H2oML_FDR","H2oML_FOR","H2oML_PPV","H2oML_ThreatScore"))
    print("h2ogbm here")
    ThresholdOutput[["h2ogbm"]] <- temp
  }
  
  # H2oGBM----
  if(any(tolower(MLModels) == "h2ogbm")) {
    if(!"p1" %chin% names(H2oGBMTestData)) data.table::setnames(H2oGBMTestData, "Predict", "p1")
    temp <- ClassificationMetrics(
      TestData = H2oGBMTestData,
      Target = eval(TargetVariable),
      Predict = "p1",
      Thresholds = Thresholds,
      PositiveOutcome = ClassLabels[1L], 
      NegativeOutcome = ClassLabels[2L], 
      CostMatrix = CostMatrix)
    data.table::setorderv(temp, cols = "MCC", order = -1L)
    data.table::setnames(
      temp, 
      c("N","P","TN","TP","FP","FN","Utility","MCC","Accuracy","F1_Score","F0.5_Score","F2_Score","NPV","TPR","TNR","FNR","FPR","FDR","FOR","PPV","ThreatScore"),
      c("H2oGBM_N","H2oGBM_P","H2oGBM_TN","H2oGBM_TP","H2oGBM_FP","H2oGBM_FN","H2oGBM_Utility","H2oGBM_MCC","H2oGBM_Acc","H2oGBM_F1_Score","H2oGBM_F0.5_Score","H2oGBM_F2_Score","H2oGBM_NPV","H2oGBM_TPR","H2oGBM_TNR","H2oGBM_FNR","H2oGBM_FPR","H2oGBM_FDR","H2oGBM_FOR","H2oGBM_PPV","H2oGBM_ThreatScore"))
    print("h2ogbm here")
    ThresholdOutput[["h2ogbm"]] <- temp
  }
  
  # H2oDRF----
  if(any(tolower(MLModels) == "h2odrf")) {
    if(!"p1" %chin% names(H2oDRFTestData)) data.table::setnames(H2oDRFTestData, "Predict", "p1")
    temp <- ClassificationMetrics(
      TestData = H2oDRFTestData,
      Target = eval(TargetVariable),
      Predict = "p1", 
      Thresholds = Thresholds,
      PositiveOutcome = ClassLabels[1L], 
      NegativeOutcome = ClassLabels[2L],
      CostMatrix = CostMatrix)
    data.table::setorderv(temp, cols = "MCC", order = -1L)
    data.table::setnames(
      temp, 
      c("N","P","TN","TP","FP","FN","Utility","MCC","Accuracy","F1_Score","F0.5_Score","F2_Score","NPV","TPR","TNR","FNR","FPR","FDR","FOR","PPV","ThreatScore"),
      c("H2oDRF_N","H2oDRF_P","H2oDRF_TN","H2oDRF_TP","H2oDRF_FP","H2oDRF_FN","H2oDRF_Utility","H2oDRF_MCC","H2oDRF_Acc","H2oDRF_F1_Score","H2oDRF_F0.5_Score","H2oDRF_F2_Score","H2oDRF_NPV","H2oDRF_TPR","H2oDRF_TNR","H2oDRF_FNR","H2oDRF_FPR","H2oDRF_FDR","H2oDRF_FOR","H2oDRF_PPV","H2oDRF_ThreatScore"))
    print("h2odrf here")
    ThresholdOutput[["h2odrf"]] <- temp
  }
  
  # H2oGLM----
  if(any(tolower(MLModels) == "h2oglm")) {
    if(!"p1" %chin% names(H2oDRFTestData)) data.table::setnames(H2oDRFTestData, "Predict", "p1")
    temp <- ClassificationMetrics(
      TestData = H2oGLMTestData,
      Target = eval(TargetVariable),
      Predict = "p1", 
      Thresholds = Thresholds,
      PositiveOutcome = ClassLabels[1L], 
      NegativeOutcome = ClassLabels[2L],
      CostMatrix = CostMatrix)
    data.table::setorderv(temp, cols = "MCC", order = -1L)
    data.table::setnames(
      temp, 
      c("N","P","TN","TP","FP","FN","Utility","MCC","Accuracy","F1_Score","F0.5_Score","F2_Score","NPV","TPR","TNR","FNR","FPR","FDR","FOR","PPV","ThreatScore"),
      c("H2oDRF_N","H2oDRF_P","H2oDRF_TN","H2oDRF_TP","H2oDRF_FP","H2oDRF_FN","H2oDRF_Utility","H2oDRF_MCC","H2oDRF_Acc","H2oDRF_F1_Score","H2oDRF_F0.5_Score","H2oDRF_F2_Score","H2oDRF_NPV","H2oDRF_TPR","H2oDRF_TNR","H2oDRF_FNR","H2oDRF_FPR","H2oDRF_FDR","H2oDRF_FOR","H2oDRF_PPV","H2oDRF_ThreatScore"))
    print("h2odrf here")
    ThresholdOutput[["h2odrf"]] <- temp
  }
  
  # XGBoost----
  if(any(tolower(MLModels) == "xgboost")) {
    if(!"p1" %chin% names(XGBoostTestData)) data.table::setnames(XGBoostTestData, "Predict", "p1")
    if(!TargetVariable %chin% names(XGBoostTestData)) data.table::setnames(XGBoostTestData, "Target", eval(TargetVariable))
    temp <- ClassificationMetrics(
      TestData = XGBoostTestData,
      Target = eval(TargetVariable),
      Predict = "p1",
      Thresholds = Thresholds,
      PositiveOutcome = ClassLabels[1L],
      NegativeOutcome = ClassLabels[2L],
      CostMatrix = CostMatrix)
    data.table::setorderv(temp, cols = "MCC", order = -1L)
    data.table::setnames(
      temp, 
      c("N","P","TN","TP","FP","FN","Utility","MCC","Accuracy","F1_Score","F0.5_Score","F2_Score","NPV","TPR","TNR","FNR","FPR","FDR","FOR","PPV","ThreatScore"),
      c("XGB_N","XGB_P","XGB_TN","XGB_TP","XGB_FP","XGB_FN","XGB_Utility","XGB_MCC","XGB_Acc","XGB_F1_Score","XGB_F0.5_Score","XGB_F2_Score","XGB_NPV","XGB_TPR","XGB_TNR","XGB_FNR","XGB_FPR","XGB_FDR","XGB_FOR","XGB_PPV","XGB_ThreatScore"))
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
