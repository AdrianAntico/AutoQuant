#' @title CarmaHoldoutMetrics
#'
#' @description  CarmaHoldoutMetrics
#'
#' @author Adrian Antico
#' @family Carma Helper
#'
#' @param DATA TestDataEval
#' @param TARGETCOLUMNNAME TargetColumnName
#' @param GROUPVARIABLES GroupVariables
#' @export
CarmaHoldoutMetrics <- function(DATA = TestDataEval,
                                TARGETCOLUMNNAME = TargetColumnName,
                                GROUPVARIABLES = GroupingVariables) {

  # Start----
  if(!is.null(GROUPVARIABLES)) {
    MetricCollection <- DATA[, GroupVar, by = "GroupVar"][, GroupVar := NULL]
  } else {
    MetricCollection <- data.table::data.table(Metric = c("MAE","MSE","MAPE","R2"), MetricValue = rep(0,4))
  }

  # MAE----
  if(!is.null(GROUPVARIABLES)) {
    data.table::set(DATA, j = "Metric", value = abs(DATA[[eval(TARGETCOLUMNNAME)]] - DATA[["Predictions"]]))
    MetricCollection <- merge(MetricCollection, DATA[, .(MAE_Metric = mean(Metric, na.rm = TRUE)), by = list(GroupVar)],
                              by = "GroupVar",
                              all = FALSE)
  } else {
    data.table::set(DATA, j = "Metric", value = abs(DATA[[eval(TARGETCOLUMNNAME)]] - DATA[["Predictions"]]))
    data.table::set(MetricCollection, i = 1L, j = "MetricValue", value = mean(DATA[["Metric"]], na.rm = TRUE))
  }

  # MAPE----
  if(!is.null(GROUPVARIABLES)) {
    data.table::set(DATA, j = "Metric", value = abs((DATA[[eval(TARGETCOLUMNNAME)]] - DATA[["Predictions"]]) / (DATA[[eval(TARGETCOLUMNNAME)]] + 1)))
    MetricCollection <- merge(MetricCollection, DATA[, .(MAPE_Metric = mean(Metric, na.rm = TRUE)), by = list(GroupVar)],
                              by = "GroupVar",
                              all = FALSE)
  } else {
    data.table::set(DATA, j = "Metric", value = abs((DATA[[eval(TARGETCOLUMNNAME)]] - DATA[["Predictions"]]) / (DATA[[eval(TARGETCOLUMNNAME)]] + 1)))
    data.table::set(MetricCollection, i = 3L, j = "MetricValue", value = round(mean(DATA[["Metric"]], na.rm = TRUE),3))
  }

  # MSE----
  if(!is.null(GROUPVARIABLES)) {
    data.table::set(DATA, j = "Metric", value = (DATA[[eval(TARGETCOLUMNNAME)]] - DATA[["Predictions"]]) ^ 2)
    MetricCollection <- merge(MetricCollection, DATA[, .(MSE_Metric = mean(Metric, na.rm = TRUE)), by = list(GroupVar)],
                              by = "GroupVar",
                              all = FALSE)
  } else {
    data.table::set(DATA, j = "Metric", value = (DATA[[eval(TARGETCOLUMNNAME)]] - DATA[["Predictions"]]) ^ 2)
    data.table::set(MetricCollection, i = 2L, j = "MetricValue", value = round(mean(DATA[["Metric"]], na.rm = TRUE),3))
  }

  # R2----
  tryCatch({if(!is.null(GROUPVARIABLES)) {
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

#' @title DT_BinaryConfusionMatrix
#'
#' @description DT_BinaryConfusionMatrix is for computing all metrics related to binary modeling outcomes
#'
#' @family Model Evaluation
#' @author Adrian Antico
#'
#' @param data Supply your model validation data with predictions
#' @param GroupVariables Supply grouping variables to generate statistics by groups
#' @param Target The name of your target variable column
#' @param Predicted The name of your predicted value column#'
#' @examples
#' \dontrun{
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
  data.table::set(data, j = "Correct_1", value = data.table::fifelse(data[[eval(Predicted)]] == data[[(Target)]] & data[[eval(Target)]] == 1, 1, 0))
  data.table::set(data, j = "Correct_0", value = data.table::fifelse(data[[eval(Predicted)]] == data[[eval(Target)]] & data[[eval(Target)]] == 0, 1, 0))
  data.table::set(data, j = "Incorrect_1", value = data.table::fifelse(data[[(Predicted)]] != data[[eval(Target)]] & data[[eval(Target)]] == 0, 1, 0))
  data.table::set(data, j = "Incorrect_0", value = data.table::fifelse(data[[eval(Predicted)]] != data[[eval(Target)]] & data[[eval(Target)]] == 1, 1, 0))

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

#' @title ClassificationMetrics
#'
#' @description ClassificationMetrics
#'
#' @author Adrian Antico
#' @family Model Evaluation
#'
#' @param TestData Test data from your modeling
#' @param Thresholds Value
#' @param Target Name of your target variable
#' @param PredictColumnName Name of your predicted value variable
#' @param PositiveOutcome The value of the positive outcome level
#' @param NegativeOutcome The value of the negative outcome level
#' @param CostMatrix c(True Positive Cost, False Negative Cost, False Positive Cost, True Negative Cost)
#' @export
ClassificationMetrics <- function(TestData,
                                  Thresholds,
                                  Target,
                                  PredictColumnName,
                                  PositiveOutcome,
                                  NegativeOutcome,
                                  CostMatrix = c(1,0,0,1)) {
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
      TN <- TestData[, sum(data.table::fifelse(get(PredictColumnName) < Thresh & get(Target) == eval(NegativeOutcome), 1, 0))]
      TP <- TestData[, sum(data.table::fifelse(get(PredictColumnName) > Thresh & get(Target) == eval(PositiveOutcome), 1, 0))]
      FN <- TestData[, sum(data.table::fifelse(get(PredictColumnName) < Thresh & get(Target) == eval(PositiveOutcome), 1, 0))]
      FP <- TestData[, sum(data.table::fifelse(get(PredictColumnName) > Thresh & get(Target) == eval(NegativeOutcome), 1, 0))]
      N  <- TestData[,.N]
      P  <- TestData[get(Target) == 1, .N]
    } else {
      TN <- TestData[, sum(data.table::fifelse(get(PredictColumnName) > Thresh & get(Target) == eval(NegativeOutcome), 1, 0))]
      TP <- TestData[, sum(data.table::fifelse(get(PredictColumnName) < Thresh & get(Target) == eval(PositiveOutcome), 1, 0))]
      FN <- TestData[, sum(data.table::fifelse(get(PredictColumnName) > Thresh & get(Target) == eval(PositiveOutcome), 1, 0))]
      FP <- TestData[, sum(data.table::fifelse(get(PredictColumnName) < Thresh & get(Target) == eval(NegativeOutcome), 1, 0))]
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

#' @title RemixClassificationMetrics
#'
#' @description RemixClassificationMetrics
#'
#' @author Adrian Antico
#' @family Model Evaluation
#'
#' @param MLModels A vector of model names from remixautoml. e.g. c("catboost","h2oautoml","h2ogbm","h2odrf","h2oglm","h2ogam","xgboost")
#' @param TargetVariable Name of your target variable
#' @param Thresholds seq(0.01,0.99,0.01),
#' @param CostMatrix c(1,0,0,1),
#' @param ClassLabels c(1,0),
#' @param CatBoostTestData Test data returned from AutoCatBoostClassifier
#' @param H2oAutoMLTestData Test data returned from AutoCatBoostClassifier
#' @param H2oGBMTestData Test data returned from AutoH2oGBMClassifier
#' @param H2oGAMTestData Test data returned from AutoH2oGAMClassifier
#' @param H2oGAMTestData Test data returned from AutoH2oDRFClassifier
#' @param H2oGLMTestData Test data returned from AutoH2oGLMClassifier
#' @param XGBoostTestData Test data returned from AutoXGBoostClassifier
#' @examples
#' \dontrun{
#' RemixClassificationMetrics <- function(
#'   MLModels = "catboost",
#'   TargetVariable = "Adrian",
#'   Thresholds = seq(0.01,0.99,0.01),
#'   CostMatrix = c(1,0,0,1),
#'   ClassLabels = c(1,0),
#'   CatBoostTestData = NULL,
#'   H2oAutoMLTestData = NULL,
#'   H2oGBMTestData = NULL,
#'   H2oGAMTestData = NULL,
#'   H2oDRFTestData = NULL,
#'   H2oGLMTestData = NULL,
#'   XGBoostTestData = NULL)
#' }
#' @export
RemixClassificationMetrics <- function(MLModels = NULL,
                                       TargetVariable = NULL,
                                       Thresholds = seq(0.01,0.99,0.01),
                                       CostMatrix = c(1,0,0,1),
                                       ClassLabels = c(1,0),
                                       CatBoostTestData = NULL,
                                       H2oAutoMLTestData = NULL,
                                       H2oGBMTestData = NULL,
                                       H2oGAMTestData = NULL,
                                       H2oDRFTestData = NULL,
                                       H2oGLMTestData = NULL,
                                       XGBoostTestData = NULL) {

  # Store output ----
  ThresholdOutput <- list()

  # CatBoost ----
  if(any(tolower(MLModels) == "catboost")) {
    if(!"p1" %chin% names(CatBoostTestData)) data.table::setnames(CatBoostTestData, "Predict", "p1")
    temp <- ClassificationMetrics(
      TestData = CatBoostTestData,
      Target = eval(TargetVariable),
      PredictColumnName = "p1",
      Thresholds = Thresholds,
      PositiveOutcome = ClassLabels[1L],
      NegativeOutcome = ClassLabels[2L],
      CostMatrix = CostMatrix)
    data.table::setorderv(temp, cols = "MCC", order = -1L, na.last = TRUE)
    data.table::setnames(
      temp,
      c("N","P","TN","TP","FP","FN","Utility","MCC","Accuracy","F1_Score","F0.5_Score","F2_Score","NPV","TPR","TNR","FNR","FPR","FDR","FOR","PPV","ThreatScore"),
      c("Cat_N","Cat_P","Cat_TN","Cat_TP","Cat_FP","Cat_FN","Cat_Utility","Cat_MCC","Cat_Acc","Cat_F1_Score","Cat_F0.5_Score","Cat_F2_Score","Cat_NPV","Cat_TPR","Cat_TNR","Cat_FNR","Cat_FPR","Cat_FDR","Cat_FOR","Cat_PPV","Cat_ThreatScore"))
    ThresholdOutput[["catboost"]] <- temp
  }

  # H2oAutoML ----
  if(any(tolower(MLModels) == "h2oautoml")) {
    if(!"p1" %chin% names(H2oAutoMLTestData)) data.table::setnames(H2oAutoMLTestData, "Predict", "p1")
    temp <- ClassificationMetrics(
      TestData = H2oAutoMLTestData,
      Target = eval(TargetVariable),
      PredictColumnName = "p1",
      Thresholds = Thresholds,
      PositiveOutcome = ClassLabels[1L],
      NegativeOutcome = ClassLabels[2L],
      CostMatrix = CostMatrix)
    data.table::setorderv(temp, cols = "MCC", order = -1L)
    data.table::setnames(
      temp,
      c("N","P","TN","TP","FP","FN","Utility","MCC","Accuracy","F1_Score","F0.5_Score","F2_Score","NPV","TPR","TNR","FNR","FPR","FDR","FOR","PPV","ThreatScore"),
      c("H2oML_N","H2oML_P","H2oML_TN","H2oML_TP","H2oML_FP","H2oML_FN","H2oML_Utility","H2oML_MCC","H2oML_Acc","H2oML_F1_Score","H2oML_F0.5_Score","H2oML_F2_Score","H2oML_NPV","H2oML_TPR","H2oML_TNR","H2oML_FNR","H2oML_FPR","H2oML_FDR","H2oML_FOR","H2oML_PPV","H2oML_ThreatScore"))
    ThresholdOutput[["h2oautoml"]] <- temp
  }

  # H2oGBM ----
  if(any(tolower(MLModels) == "h2ogbm")) {
    if(!"p1" %chin% names(H2oGBMTestData)) data.table::setnames(H2oGBMTestData, "Predict", "p1")
    temp <- ClassificationMetrics(
      TestData = H2oGBMTestData,
      Target = eval(TargetVariable),
      PredictColumnName = "p1",
      Thresholds = Thresholds,
      PositiveOutcome = ClassLabels[1L],
      NegativeOutcome = ClassLabels[2L],
      CostMatrix = CostMatrix)
    data.table::setorderv(temp, cols = "MCC", order = -1L)
    data.table::setnames(
      temp,
      c("N","P","TN","TP","FP","FN","Utility","MCC","Accuracy","F1_Score","F0.5_Score","F2_Score","NPV","TPR","TNR","FNR","FPR","FDR","FOR","PPV","ThreatScore"),
      c("H2oGBM_N","H2oGBM_P","H2oGBM_TN","H2oGBM_TP","H2oGBM_FP","H2oGBM_FN","H2oGBM_Utility","H2oGBM_MCC","H2oGBM_Acc","H2oGBM_F1_Score","H2oGBM_F0.5_Score","H2oGBM_F2_Score","H2oGBM_NPV","H2oGBM_TPR","H2oGBM_TNR","H2oGBM_FNR","H2oGBM_FPR","H2oGBM_FDR","H2oGBM_FOR","H2oGBM_PPV","H2oGBM_ThreatScore"))
    ThresholdOutput[["h2ogbm"]] <- temp
  }

  # H2oDRF ----
  if(any(tolower(MLModels) == "h2odrf")) {
    if(!"p1" %chin% names(H2oDRFTestData)) data.table::setnames(H2oDRFTestData, "Predict", "p1")
    temp <- ClassificationMetrics(
      TestData = H2oDRFTestData,
      Target = eval(TargetVariable),
      PredictColumnName = "p1",
      Thresholds = Thresholds,
      PositiveOutcome = ClassLabels[1L],
      NegativeOutcome = ClassLabels[2L],
      CostMatrix = CostMatrix)
    data.table::setorderv(temp, cols = "MCC", order = -1L)
    data.table::setnames(
      temp,
      c("N","P","TN","TP","FP","FN","Utility","MCC","Accuracy","F1_Score","F0.5_Score","F2_Score","NPV","TPR","TNR","FNR","FPR","FDR","FOR","PPV","ThreatScore"),
      c("H2oDRF_N","H2oDRF_P","H2oDRF_TN","H2oDRF_TP","H2oDRF_FP","H2oDRF_FN","H2oDRF_Utility","H2oDRF_MCC","H2oDRF_Acc","H2oDRF_F1_Score","H2oDRF_F0.5_Score","H2oDRF_F2_Score","H2oDRF_NPV","H2oDRF_TPR","H2oDRF_TNR","H2oDRF_FNR","H2oDRF_FPR","H2oDRF_FDR","H2oDRF_FOR","H2oDRF_PPV","H2oDRF_ThreatScore"))
    ThresholdOutput[["h2odrf"]] <- temp
  }

  # H2oGLM ----
  if(any(tolower(MLModels) == "h2oglm")) {
    if(!"p1" %chin% names(H2oGLMTestData)) data.table::setnames(H2oGLMTestData, "Predict", "p1")
    temp <- ClassificationMetrics(
      TestData = H2oGLMTestData,
      Target = eval(TargetVariable),
      PredictColumnName = "p1",
      Thresholds = Thresholds,
      PositiveOutcome = ClassLabels[1L],
      NegativeOutcome = ClassLabels[2L],
      CostMatrix = CostMatrix)
    data.table::setorderv(temp, cols = "MCC", order = -1L)
    data.table::setnames(
      temp,
      c("N","P","TN","TP","FP","FN","Utility","MCC","Accuracy","F1_Score","F0.5_Score","F2_Score","NPV","TPR","TNR","FNR","FPR","FDR","FOR","PPV","ThreatScore"),
      c("H2oGLM_N","H2oGLM_P","H2oGLM_TN","H2oGLM_TP","H2oGLM_FP","H2oGLM_FN","H2oGLM_Utility","H2oGLM_MCC","H2oGLM_Acc","H2oGLM_F1_Score","H2oGLM_F0.5_Score","H2oGLM_F2_Score","H2oGLM_NPV","H2oGLM_TPR","H2oGLM_TNR","H2oGLM_FNR","H2oGLM_FPR","H2oGLM_FDR","H2oGLM_FOR","H2oGLM_PPV","H2oGLM_ThreatScore"))
    ThresholdOutput[["h2oglm"]] <- temp
  }

  # H2oGAM ----
  if(any(tolower(MLModels) == "h2ogam")) {
    if(!"p1" %chin% names(H2oGAMTestData)) data.table::setnames(H2oGAMTestData, "Predict", "p1")
    temp <- ClassificationMetrics(
      TestData = H2oGAMTestData,
      Target = eval(TargetVariable),
      PredictColumnName = "p1",
      Thresholds = Thresholds,
      PositiveOutcome = ClassLabels[1L],
      NegativeOutcome = ClassLabels[2L],
      CostMatrix = CostMatrix)
    data.table::setorderv(temp, cols = "MCC", order = -1L)
    data.table::setnames(
      temp,
      c("N","P","TN","TP","FP","FN","Utility","MCC","Accuracy","F1_Score","F0.5_Score","F2_Score","NPV","TPR","TNR","FNR","FPR","FDR","FOR","PPV","ThreatScore"),
      c("H2oGAM_N","H2oGAM_P","H2oGAM_TN","H2oGAM_TP","H2oGAM_FP","H2oGAM_FN","H2oGAM_Utility","H2oGAM_MCC","H2oGAM_Acc","H2oGAM_F1_Score","H2oGAM_F0.5_Score","H2oGAM_F2_Score","H2oGAM_NPV","H2oGAM_TPR","H2oGAM_TNR","H2oGAM_FNR","H2oGAM_FPR","H2oGAM_FDR","H2oGAM_FOR","H2oGAM_PPV","H2oGAM_ThreatScore"))
    ThresholdOutput[["h2ogam"]] <- temp
  }

  # XGBoost ----
  if(any(tolower(MLModels) == "xgboost")) {
    if(!"p1" %chin% names(XGBoostTestData)) data.table::setnames(XGBoostTestData, "Predict", "p1")
    if(!TargetVariable %chin% names(XGBoostTestData)) data.table::setnames(XGBoostTestData, "Target", eval(TargetVariable))
    temp <- ClassificationMetrics(
      TestData = XGBoostTestData,
      Target = eval(TargetVariable),
      PredictColumnName = "p1",
      Thresholds = Thresholds,
      PositiveOutcome = ClassLabels[1L],
      NegativeOutcome = ClassLabels[2L],
      CostMatrix = CostMatrix)
    data.table::setorderv(temp, cols = "MCC", order = -1L)
    data.table::setnames(
      temp,
      c("N","P","TN","TP","FP","FN","Utility","MCC","Accuracy","F1_Score","F0.5_Score","F2_Score","NPV","TPR","TNR","FNR","FPR","FDR","FOR","PPV","ThreatScore"),
      c("XGB_N","XGB_P","XGB_TN","XGB_TP","XGB_FP","XGB_FN","XGB_Utility","XGB_MCC","XGB_Acc","XGB_F1_Score","XGB_F0.5_Score","XGB_F2_Score","XGB_NPV","XGB_TPR","XGB_TNR","XGB_FNR","XGB_FPR","XGB_FDR","XGB_FOR","XGB_PPV","XGB_ThreatScore"))
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

#' @title BinaryMetrics
#'
#' @description Compute binary metrics and save them to file
#'
#' @author Adrian Antico
#' @family Model Evaluation
#'
#' @param MLModels = "catboost"
#' @param ClassWeights. = ClassWeights
#' @param SaveModelObjects. = SaveModelObjects
#' @param ValidationData. = ValidationData
#' @param TrainOnFull. = TrainOnFull
#' @param TargetColumnName. = TargetColumnName
#' @param ModelID. = ModelID
#' @param model_path. = model_path
#' @param metadata_path. = metadata_path
#'
#' @export
BinaryMetrics <- function(MLModels = "catboost",
                          ClassWeights. = ClassWeights,
                          SaveModelObjects. = SaveModelObjects,
                          ValidationData. = ValidationData,
                          TrainOnFull. = TrainOnFull,
                          TargetColumnName. = TargetColumnName,
                          ModelID. = ModelID,
                          model_path. = model_path,
                          metadata_path. = metadata_path) {

  CostMatrixWeights <- c(ClassWeights.[1L], 0, 0, ClassWeights.[2L])
  if(SaveModelObjects. && !TrainOnFull.) {
    EvalMetrics <- RemixClassificationMetrics(MLModels = "catboost", TargetVariable = eval(TargetColumnName.), Thresholds = seq(0.01,0.99,0.01), CostMatrix = CostMatrixWeights, ClassLabels = c(1,0), CatBoostTestData = ValidationData.)
    if(!is.null(metadata_path.)) {
      data.table::fwrite(EvalMetrics, file = file.path(metadata_path., paste0(ModelID., "_EvaluationMetrics.csv")))
    } else {
      data.table::fwrite(EvalMetrics, file = file.path(model_path., paste0(ModelID., "_EvaluationMetrics.csv")))
    }
  } else if(!TrainOnFull.) {
    EvalMetrics <- RemixClassificationMetrics(MLModels = "catboost", TargetVariable = eval(TargetColumnName.), Thresholds = seq(0.01,0.99,0.01), CostMatrix = CostMatrixWeights, ClassLabels = c(1,0), CatBoostTestData = ValidationData.)
  } else {
    EvalMetrics <- NULL
  }
  return(EvalMetrics)
}

#' @title RegressionMetrics
#'
#' @description Compute regression metrics and save them to file
#'
#' @author Adrian Antico
#' @family Model Evaluation
#'
#' @param SaveModelObjects. = SaveModelObjects
#' @param data. = data
#' @param ValidationData. = ValidationData
#' @param TrainOnFull. = TrainOnFull
#' @param LossFunction. = LossFunction
#' @param EvalMetric. = EvalMetric
#' @param TargetColumnName. = TargetColumnName
#' @param ModelID. = ModelID
#' @param model_path. = model_path
#' @param metadata_path. = metadata_path
#'
#' @export
RegressionMetrics <- function(SaveModelObjects. = SaveModelObjects,
                              data. = data,
                              ValidationData. = ValidationData,
                              TrainOnFull. = TrainOnFull,
                              LossFunction. = LossFunction,
                              EvalMetric. = EvalMetric,
                              TargetColumnName. = TargetColumnName,
                              ModelID. = ModelID,
                              model_path. = model_path,
                              metadata_path. = metadata_path) {

  if(!TrainOnFull. && (!is.null(LossFunction.) && LossFunction. != "MultiRMSE") || (!is.null(EvalMetric.) && EvalMetric. != "MultiRMSE")) {
    EvaluationMetrics <- data.table::data.table(Metric = c("MAE","MAPE","RMSE","R2"), MetricValue = rep(999999, 4L))
    i <- 0L
    for(metric in c("mae", "mape", "rmse", "r2")) {
      i <- i + 1L
      tryCatch({
        if(tolower(metric) == "mae") {
          ValidationData.[, Metric := abs(ValidationData.[[eval(TargetColumnName.)]] - Predict)]
          Metric <- ValidationData.[, mean(Metric, na.rm = TRUE)]
        } else if(tolower(metric) == "mape") {
          ValidationData.[, Metric := abs((ValidationData.[[eval(TargetColumnName.)]] - Predict) / (ValidationData.[[eval(TargetColumnName.)]] + 1))]
          Metric <- ValidationData.[, mean(Metric, na.rm = TRUE)]
        } else if(tolower(metric) == "rmse") {
          ValidationData.[, Metric := (ValidationData.[[eval(TargetColumnName.)]] - Predict) ^ 2]
          Metric <- sqrt(ValidationData.[, mean(Metric, na.rm = TRUE)])
        } else if(tolower(metric) == "r2") {
          ValidationData.[, ':=' (Metric1 = (ValidationData.[[eval(TargetColumnName.)]] - data.[, mean(get(TargetColumnName.))]) ^ 2, Metric2 = (ValidationData.[[eval(TargetColumnName.)]] - Predict) ^ 2)]
          Metric <- 1 - ValidationData.[, sum(Metric2, na.rm = TRUE)] / ValidationData.[, sum(Metric1, na.rm = TRUE)]
        }
        data.table::set(EvaluationMetrics, i = i, j = 2L, value = round(Metric, 4L))
      }, error = function(x) "skip")
    }

    # Remove Cols
    ValidationData.[, ':=' (Metric = NULL)]

    # Save EvaluationMetrics to File
    EvaluationMetrics <- EvaluationMetrics[MetricValue != 999999]
    if(SaveModelObjects.) {
      if(!is.null(metadata_path.)) {
        data.table::fwrite(EvaluationMetrics, file = file.path(metadata_path., paste0(ModelID., "_EvaluationMetrics.csv")))
      } else {
        data.table::fwrite(EvaluationMetrics, file = file.path(model_path., paste0(ModelID., "_EvaluationMetrics.csv")))
      }
    }
  } else {
    EvaluationMetrics <- list()

    # Loop through Target Variables
    for(TV in seq_along(TargetColumnName.)) {

      # Eval Metrics
      EvaluationMetrics[[TargetColumnName.[TV]]] <- data.table::data.table(Metric = c("MAE","MAPE","RMSE","R2"), MetricValue = rep(999999, 4L))
      i <- 0L
      for(metric in c("mae", "mape", "rmse", "r2")) {
        i <- i + 1L
        tryCatch({
          if(tolower(metric) == "mae") {
            ValidationData.[, Metric := abs(ValidationData.[[eval(TargetColumnName.[TV])]] - ValidationData.[[eval(paste0("Predict.V", TV))]])]
            Metric <- ValidationData.[, mean(Metric, na.rm = TRUE)]
          } else if(tolower(metric) == "mape") {
            ValidationData.[, Metric := abs((ValidationData.[[eval(TargetColumnName.[TV])]] - ValidationData.[[eval(paste0("Predict.V", TV))]]) / (ValidationData.[[eval(TargetColumnName.[TV])]] + 1))]
            Metric <- ValidationData.[, mean(Metric, na.rm = TRUE)]
          } else if(tolower(metric) == "rmse") {
            ValidationData.[, Metric := (ValidationData.[[eval(TargetColumnName.[TV])]] - ValidationData.[[eval(paste0("Predict.V", TV))]]) ^ 2]
            Metric <- sqrt(ValidationData.[, mean(Metric, na.rm = TRUE)])
          } else if(tolower(metric) == "r2") {
            ValidationData.[, ':=' (Metric1 = (ValidationData.[[eval(TargetColumnName.[TV])]] - data.[, mean(get(TargetColumnName.[TV]))]) ^ 2, Metric2 = (ValidationData.[[eval(TargetColumnName.[TV])]] - ValidationData.[[eval(paste0("Predict.V",TV))]]) ^ 2)]
            Metric <- 1 - ValidationData.[, sum(Metric2, na.rm = TRUE)] / ValidationData.[, sum(Metric1, na.rm = TRUE)]
          }
          data.table::set(EvaluationMetrics[[TargetColumnName.[TV]]], i = i, j = 2L, value = round(Metric, 4L))
        }, error = function(x) "skip")
      }

      # Remove Cols
      ValidationData.[, ':=' (Metric = NULL)]

      # Save EvaluationMetrics to File
      EvaluationMetrics[[TargetColumnName.[TV]]] <- EvaluationMetrics[[TargetColumnName.[TV]]][MetricValue != 999999]
      if(SaveModelObjects.) {
        if(!is.null(metadata_path.)) {
          data.table::fwrite(EvaluationMetrics[[TargetColumnName.[TV]]], file = file.path(metadata_path., paste0(ModelID., "_", TargetColumnName.[TV], "_EvaluationMetrics.csv")))
        } else {
          data.table::fwrite(EvaluationMetrics[[TargetColumnName.[TV]]], file = file.path(model_path., paste0(ModelID., "_", TargetColumnName.[TV], "_EvaluationMetrics.csv")))
        }
      }
    }
  }
  return(EvaluationMetrics)
}


#' @title MultiClassMetrics
#'
#' @description Compute regression metrics and save them to file
#'
#' @author Adrian Antico
#' @family Model Evaluation
#'
#' @param SaveModelObjects. = SaveModelObjects
#' @param ValidationData. = ValidationData
#' @param PredictData. = predict
#' @param TrainOnFull. = TrainOnFull
#' @param TargetColumnName. = TargetColumnName
#' @param TargetLevels. = TargetLevels
#' @param ModelID. = ModelID
#' @param model_path. = model_path
#' @param metadata_path. = metadata_path
#'
#' @export
MultiClassMetrics <- function(SaveModelObjects. = SaveModelObjects,
                              ValidationData. = ValidationData,
                              PredictData. = predict,
                              TrainOnFull. = TrainOnFull,
                              TargetColumnName. = TargetColumnName,
                              TargetLevels. = TargetLevels,
                              ModelID. = ModelID,
                              model_path. = model_path,
                              metadata_path. = metadata_path) {

  # MultiClass Metrics Accuracy ----
  MetricAcc <- ValidationData.[, mean(data.table::fifelse(as.character(get(TargetColumnName.)) == as.character(Predict), 1.0, 0.0), na.rm = TRUE)]

  # MultiClass Metrics MicroAUC ----
  MetricAUC <- round(as.numeric(noquote(stringr::str_extract(pROC::multiclass.roc(response = ValidationData.[[eval(TargetColumnName.)]], predictor = as.matrix(ValidationData.[, .SD, .SDcols = unique(names(ValidationData.)[3L:(ncol(PredictData.)+1L)])]))$auc, "\\d+\\.*\\d*"))), 4L)

  # Logloss ----
  if(!TrainOnFull.) temp <- ValidationData.[, 1L] else temp <- ValidationData.[, 2L]
  temp[, Truth := get(TargetColumnName.)]
  temp <- DummifyDT(
    data = temp,
    cols = eval(TargetColumnName.),
    KeepFactorCols = FALSE,
    OneHot = FALSE,
    SaveFactorLevels = FALSE,
    SavePath = NULL,
    ImportFactorLevels = FALSE,
    FactorLevelsList = NULL,
    ClustScore = FALSE,
    ReturnFactorLevels = FALSE)
  N <- TargetLevels.[, .N]
  logloss <- MLmetrics::LogLoss(y_pred = as.matrix(ValidationData.[, .SD, .SDcols = c(names(ValidationData.)[c(3L:(2L+N))])]), y_true = as.matrix(temp[, .SD, .SDcols = c(names(temp)[c(2L:(1L+N))])]))

  # MultiClass Save Validation Data to File ----
  if(SaveModelObjects.) {
    if(!is.null(metadata_path.)) {
      data.table::fwrite(ValidationData., file = file.path(metadata_path., paste0(ModelID., "_ValidationData.csv")))
    } else {
      data.table::fwrite(ValidationData., file = file.path(model_path., paste0(ModelID., "_ValidationData.csv")))
    }
  }

  # MultiClass Evaluation Metrics ----
  if(!TrainOnFull.) {
    EvaluationMetrics <- data.table::data.table(Metric = c("AUC", "Accuracy", "LogLoss"), MetricValue = c(MetricAUC, MetricAcc, logloss))
    if(SaveModelObjects.) {
      if(!is.null(metadata_path.)) {
        data.table::fwrite(EvaluationMetrics, file = file.path(metadata_path., paste0(ModelID., "_EvaluationMetrics.csv")))
      } else {
        data.table::fwrite(EvaluationMetrics, file = file.path(model_path., paste0(ModelID., "_EvaluationMetrics.csv")))
      }
    }
  }
  return(EvaluationMetrics)
}
