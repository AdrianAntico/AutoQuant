# AutoQuant is a package for quickly creating high quality visualizations under a common and easy api.
# Copyright (C) <year>  <name of author>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
#' @noRd
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
#' @noRd
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
#' @noRd
ClassificationMetrics <- function(TestData,
                                  Thresholds,
                                  Target,
                                  PredictColumnName,
                                  PositiveOutcome,
                                  NegativeOutcome,
                                  CostMatrix = c(0,1,1,0)) {
  if("Target" %chin% names(TestData)) data.table::set(TestData, j = "Target", value = NULL)
  ThreshLength <- rep(1, length(Thresholds))
  ThresholdOutput <- data.table::data.table(
    Threshold   = ThreshLength,
    TN          = ThreshLength,
    TP          = ThreshLength,
    FN          = ThreshLength,
    FP          = ThreshLength,
    N           = ThreshLength,
    P           = ThreshLength,
    MCC         = ThreshLength,
    Accuracy    = ThreshLength,
    TPR         = ThreshLength,
    TNR         = ThreshLength,
    FNR         = ThreshLength,
    FPR         = ThreshLength,
    FDR         = ThreshLength,
    FOR         = ThreshLength,
    F1_Score    = ThreshLength,
    F2_Score    = ThreshLength,
    F0.5_Score  = ThreshLength,
    NPV         = ThreshLength,
    PPV         = ThreshLength,
    ThreatScore = ThreshLength,
    Utility     = ThreshLength)
  counter <- 0L
  for(Thresh in Thresholds) {
    counter <- counter + 1L
    TN <- TestData[, sum(data.table::fifelse(get(PredictColumnName) < Thresh & get(Target) == eval(NegativeOutcome), 1, 0))]
    TP <- TestData[, sum(data.table::fifelse(get(PredictColumnName) > Thresh & get(Target) == eval(PositiveOutcome), 1, 0))]
    FN <- TestData[, sum(data.table::fifelse(get(PredictColumnName) < Thresh & get(Target) == eval(PositiveOutcome), 1, 0))]
    FP <- TestData[, sum(data.table::fifelse(get(PredictColumnName) > Thresh & get(Target) == eval(NegativeOutcome), 1, 0))]
    N1  <- TestData[, .N]
    N  <- TestData[get(PredictColumnName) < eval(Thresh), .N]
    P1  <- TestData[get(Target) == 1, .N]
    P  <- TestData[get(Target) == 1 & get(PredictColumnName) > Thresh, .N]

    # Calculate metrics ----
    MCC         <- (TP*TN-FP*FN)/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
    Accuracy    <- (TP+TN)/N1
    TPR         <- TP/P1
    TNR         <- TN/(N1-P1)
    FNR         <- FN / P1
    FPR         <- FP / N1
    FDR         <- FP / (FP + TP)
    FOR         <- FN / (FN + TN)
    F1_Score    <- 2 * TP / (2 * TP + FP + FN)
    F2_Score    <- 3 * TP / (2 * TP + FP + FN)
    F0.5_Score  <- 1.5 * TP / (0.5 * TP + FP + FN)
    NPV         <- TN / (TN + FN)
    PPV         <- TP / (TP + FP)
    ThreatScore <- TP / (TP + FN + FP)
    Utility     <- P1/N1 * (CostMatrix[1L] * TPR + CostMatrix[2L] * (1 - TPR)) + (1 - P1/N1) * (CostMatrix[3L] * FPR + CostMatrix[4L] * (1 - FPR))

    # Fill in values ----
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

  # Remove NA's
  ThresholdOutput <- ThresholdOutput[, RowSum := rowSums(x = as.matrix(ThresholdOutput))][!is.na(RowSum)][, RowSum := NULL]
  return(ThresholdOutput)
}

#' @title RemixClassificationMetrics
#'
#' @description RemixClassificationMetrics
#'
#' @author Adrian Antico
#' @family Model Evaluation
#'
#' @param TargetVariable Name of your target variable
#' @param Thresholds seq(0.01,0.99,0.01),
#' @param CostMatrix c(1,0,0,1) c(TP utility, FN utility, FP utility, TN utility)
#' @param ClassLabels c(1,0),
#' @param ValidationData. Test data
#' @examples
#' \dontrun{
#' RemixClassificationMetrics <- function(
#'   TargetVariable = "Adrian",
#'   Thresholds = seq(0.01,0.99,0.01),
#'   CostMatrix = c(1,0,0,1),
#'   ClassLabels = c(1,0),
#'   ValidationData. = ValidationData)
#' }
#' @noRd
RemixClassificationMetrics <- function(TargetVariable = NULL,
                                       Thresholds = seq(0.01,0.99,0.01),
                                       CostMatrix = c(1,0,0,1),
                                       ClassLabels = c(1,0),
                                       ValidationData. = NULL) {

  # Create metrics
  if(!"p1" %chin% names(ValidationData.)) data.table::setnames(ValidationData., "Predict", "p1")
  temp <- ClassificationMetrics(
    TestData = ValidationData.,
    Target = eval(TargetVariable),
    PredictColumnName = "p1",
    Thresholds = Thresholds,
    PositiveOutcome = ClassLabels[1L],
    NegativeOutcome = ClassLabels[2L],
    CostMatrix = CostMatrix)
  if(temp[,.N] > 95) data.table::setorderv(temp, cols = "MCC", order = -1L, na.last = TRUE)

  # Return values----
  return(temp)
}

#' @title BinaryMetrics
#'
#' @description Compute binary metrics and save them to file
#'
#' @author Adrian Antico
#' @family Model Evaluation
#'
#' @param ClassWeights. = ClassWeights
#' @param CostMatrixWeights. = CostMatrixWeights
#' @param SaveModelObjects. = SaveModelObjects
#' @param ValidationData. = ValidationData
#' @param TrainOnFull. = TrainOnFull
#' @param TargetColumnName. = TargetColumnName
#' @param ModelID. = ModelID
#' @param model_path. = model_path
#' @param metadata_path. = metadata_path
#' @param Method 'threshold' for 0.01 to 0.99 by 0.01 thresholds or 'bins' for 20 equally sized bins
#'
#' @noRd
BinaryMetrics <- function(ClassWeights. = ClassWeights,
                          CostMatrixWeights. = CostMatrixWeights,
                          SaveModelObjects. = SaveModelObjects,
                          ValidationData. = ValidationData,
                          TrainOnFull. = TrainOnFull,
                          TargetColumnName. = TargetColumnName,
                          ModelID. = ModelID,
                          model_path. = model_path,
                          metadata_path. = metadata_path,
                          Method = "threshold") {
  if(is.null(CostMatrixWeights.)) CostMatrixWeights. <- c(ClassWeights.[1L], 0, 0, ClassWeights.[2L])
  if(Method == "threshold") {
    vals <- seq(0.01,0.99,0.01)
  } else if(Method == "bins") {
    temp <- ValidationData.$p1
    vals <- quantile(temp, probs = seq(0.05,1,0.05), type = 7)
  }
  if(SaveModelObjects. && !TrainOnFull.) {
    EvalMetrics <- RemixClassificationMetrics(TargetVariable = eval(TargetColumnName.), Thresholds = unique(vals), CostMatrix = CostMatrixWeights., ClassLabels = c(1,0), ValidationData. = ValidationData.)
  } else {
    EvalMetrics <- RemixClassificationMetrics(TargetVariable = eval(TargetColumnName.), Thresholds = unique(vals), CostMatrix = CostMatrixWeights., ClassLabels = c(1,0), ValidationData. = ValidationData.)
  }
  EvalMetrics[, P_Predicted := TP + FP]
  data.table::setcolorder(EvalMetrics, c(1,ncol(EvalMetrics),2:(ncol(EvalMetrics)-1)))
  data.table::setcolorder(EvalMetrics, c(1:8, ncol(EvalMetrics), 9:10, 17:19, 11:16, 20:(ncol(EvalMetrics)-1)))
  data.table::setcolorder(EvalMetrics, c(1:14, ncol(EvalMetrics), 15:(ncol(EvalMetrics)-1)))
  data.table::setorderv(EvalMetrics, "Utility", -1)
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
#' @noRd
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
    # metric <- 'r2'
    for(metric in c("mae", "mape", "rmse", "r2")) {# metric = "r2"
      i <- i + 1L
      if(tolower(metric) == "mae") {
        ValidationData.[, Metric := abs(get(TargetColumnName.) - Predict)]
        MetricVal <- ValidationData.[, mean(Metric, na.rm = TRUE)]
      } else if(tolower(metric) == "mape") {
        ValidationData.[, Metric := abs((get(TargetColumnName.) - Predict) / (get(TargetColumnName.) + 1))]
        MetricVal <- ValidationData.[, mean(Metric, na.rm = TRUE)]
      } else if(tolower(metric) == "rmse") {
        ValidationData.[, Metric := (get(TargetColumnName.) - Predict) ^ 2]
        MetricVal <- sqrt(ValidationData.[, mean(Metric, na.rm = TRUE)])
      } else if(tolower(metric) == "r2") {
        ValidationData.[, ':=' (Metric1 = (get(TargetColumnName.) - data.[, mean(get(TargetColumnName.))]) ^ 2, Metric2 = (get(TargetColumnName.) - Predict) ^ 2)]
        MetricVal <- 1 - ValidationData.[, sum(Metric2, na.rm = TRUE)] / ValidationData.[, sum(Metric1, na.rm = TRUE)]
      }
      data.table::set(EvaluationMetrics, i = i, j = 2L, value = round(MetricVal, 4L))
    }

    # Save EvaluationMetrics to File
    EvaluationMetrics <- EvaluationMetrics[MetricValue != 999999]
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
            MetricVal <- ValidationData.[, mean(Metric, na.rm = TRUE)]
          } else if(tolower(metric) == "mape") {
            ValidationData.[, Metric := abs((ValidationData.[[eval(TargetColumnName.[TV])]] - ValidationData.[[eval(paste0("Predict.V", TV))]]) / (ValidationData.[[eval(TargetColumnName.[TV])]] + 1))]
            MetricVal <- ValidationData.[, mean(Metric, na.rm = TRUE)]
          } else if(tolower(metric) == "rmse") {
            ValidationData.[, Metric := (ValidationData.[[eval(TargetColumnName.[TV])]] - ValidationData.[[eval(paste0("Predict.V", TV))]]) ^ 2]
            MetricVal <- sqrt(ValidationData.[, mean(Metric, na.rm = TRUE)])
          } else if(tolower(metric) == "r2") {
            ValidationData.[, ':=' (Metric1 = (ValidationData.[[eval(TargetColumnName.[TV])]] - data.[, mean(get(TargetColumnName.[TV]))]) ^ 2, Metric2 = (ValidationData.[[eval(TargetColumnName.[TV])]] - ValidationData.[[eval(paste0("Predict.V",TV))]]) ^ 2)]
            MetricVal <- 1 - ValidationData.[, sum(Metric2, na.rm = TRUE)] / ValidationData.[, sum(Metric1, na.rm = TRUE)]
          }
          data.table::set(EvaluationMetrics[[TargetColumnName.[TV]]], i = i, j = 2L, value = round(MetricVal, 4L))
        }, error = function(x) "skip")
      }

      # Save EvaluationMetrics to File
      EvaluationMetrics[[TargetColumnName.[TV]]] <- EvaluationMetrics[[TargetColumnName.[TV]]][MetricValue != 999999]
    }
  }

  # Remove Columns
  if("Metric" %chin% names(ValidationData.)) data.table::set(ValidationData., j = "Metric", value = NULL)
  if("Metric1" %chin% names(ValidationData.)) data.table::set(ValidationData., j = "Metric1", value = NULL)
  if("Metric2" %chin% names(ValidationData.)) data.table::set(ValidationData., j = "Metric2", value = NULL)

  return(EvaluationMetrics)
}


#' @title MultiClassMetrics
#'
#' @description Compute regression metrics and save them to file
#'
#' @author Adrian Antico
#' @family Model Evaluation
#'
#' @param ModelClass "catboost"
#' @param DataType "test" or "train"
#' @param SaveModelObjects. = SaveModelObjects
#' @param ValidationData. = ValidationData
#' @param PredictData. = predict
#' @param TrainOnFull. = TrainOnFull
#' @param TargetColumnName. = TargetColumnName
#' @param TargetLevels. = TargetLevels
#' @param ModelID. = ModelID
#' @param model_path. = model_path
#' @param metadata_path. = metadata_path
#' @param Debug = FALSE
#'
#' @noRd
MultiClassMetrics <- function(ModelClass = "catboost",
                              DataType = "test",
                              SaveModelObjects. = SaveModelObjects,
                              ValidationData. = ValidationData,
                              PredictData. = predict,
                              TrainOnFull. = TrainOnFull,
                              TargetColumnName. = TargetColumnName,
                              TargetLevels. = TargetLevels,
                              ModelID. = ModelID,
                              model_path. = NULL,
                              metadata_path. = NULL,
                              Debug = FALSE) {

  # Convert Target Variable back to Categorical
  if(Debug) print("# Convert Target Variable back to Categorical")
  if(is.numeric(ValidationData.[[TargetColumnName.]])) {
    data.table::setkeyv(ValidationData., TargetColumnName.)
    data.table::setkeyv(TargetLevels., 'NewLevels')
    ValidationData.[TargetLevels., OriginalLevels := i.OriginalLevels][, eval(TargetColumnName.) := OriginalLevels][, OriginalLevels := NULL]
  }

  # MultiClass Metrics Accuracy
  if(Debug) print("# MultiClass Metrics Accuracy")
  MetricAcc <- ValidationData.[, mean(data.table::fifelse(as.character(get(TargetColumnName.)) == as.character(Predict), 1.0, 0.0), na.rm = TRUE)]

  # MultiClass Metrics MicroAUC Setup
  if(Debug) print("# MultiClass Metrics MicroAUC Setup")
  Response <- ValidationData.[[eval(TargetColumnName.)]]
  if(ModelClass == "catboost") {
    Predictor <- as.matrix(ValidationData.[, .SD, .SDcols = unique(as.character(TargetLevels.[['OriginalLevels']]))])
  } else if(ModelClass == "h2o") {
    Predictor <- as.matrix(ValidationData.[, .SD, .SDcols = unique(names(ValidationData.)[(ncol(ValidationData.) + 1 - length(TargetLevels.)):(ncol(ValidationData.))])])
  } else if(ModelClass == "xgboost") {
    Predictor <- as.matrix(ValidationData.[, .SD, .SDcols = c(unique(as.character(TargetLevels.[["OriginalLevels"]])))])
  }

  # Generate metric
  if(Debug) print("# Generate metric")
  MetricAUC <- round(as.numeric(noquote(stringr::str_extract(pROC::multiclass.roc(response = Response, predictor = Predictor)$auc, "\\d+\\.*\\d*"))), 4L)

  # Logloss
  if(Debug) print("# Logloss")
  if(!data.table::is.data.table(TargetLevels.)) N <- length(TargetLevels.) else N <- TargetLevels.[, .N]
  temp <- ValidationData.[, .SD, .SDcols = c(TargetColumnName., "Predict")]
  temp <- Rodeo::DummifyDT(data=temp, cols=eval(TargetColumnName.), KeepFactorCols=FALSE, OneHot=FALSE, SaveFactorLevels=FALSE, SavePath=NULL, ImportFactorLevels=FALSE, FactorLevelsList=NULL, ClustScore=FALSE, ReturnFactorLevels=FALSE)
  if(ModelClass == "xgboost") {
    if(Debug) print("# ModelClass == xgboost")
    logloss <- MLmetrics::LogLoss(
      y_pred = as.matrix(ValidationData.[, .SD, .SDcols = c(as.character(TargetLevels.[["OriginalLevels"]]))]),
      y_true = as.matrix(temp[, .SD, .SDcols = c(names(temp)[c(2L:(1L+N))])]))
  } else if(ModelClass == "catboost") {
    if(Debug) print("# ModelClass == catboost")
    temp <- Rodeo::DummifyDT(data=temp, cols='Predict', KeepFactorCols=FALSE, OneHot=FALSE, SaveFactorLevels=FALSE, SavePath=NULL, ImportFactorLevels=FALSE, FactorLevelsList=NULL, ClustScore=FALSE, ReturnFactorLevels=FALSE)
    logloss <- MLmetrics::LogLoss(
      y_pred = as.matrix(ValidationData.[, .SD, .SDcols = unique(as.character(TargetLevels.[['OriginalLevels']]))]),
      y_true = as.matrix(temp[, .SD, .SDcols = c(names(temp)[seq_len(N)])]))
  } else if(ModelClass == "h2o") {
    if(Debug) print("# ModelClass == h2o")
    logloss <- MLmetrics::LogLoss(
      y_pred = as.matrix(ValidationData.[, .SD, .SDcols = c(unique(names(ValidationData.)[(ncol(ValidationData.) + 1 - length(TargetLevels.)):(ncol(ValidationData.))]))]),
      y_true = as.matrix(temp[, .SD, .SDcols = c(names(temp)[c(2L:(1L+N))])]))
  }

  # MCC for MultiClass
  if(Debug) print("# MCC for MultiClass")
  ConfusionMatrix <- table(ValidationData.$Predict, ValidationData.[[eval(TargetColumnName.)]])
  c <- sum(diag(ConfusionMatrix))
  s <- sum(ConfusionMatrix)

  # MCC
  if(ModelClass != 'h2o') {

    # pk * tk
    if(Debug) print("# pk * tk")
    sumPkTk <- c()
    for(i in as.character(TargetLevels.[['OriginalLevels']])) {
      pk <- ValidationData.[Predict == eval(i), .N]
      tk <- ValidationData.[get(TargetColumnName.) == eval(i), .N]
      sumPkTk <- c(sumPkTk, pk/100 * tk*100)
    }

    # (s^2 - sum(pk^2))
    if(Debug) print("# (s^2 - sum(pk^2))")
    sumPk2 <- c()
    for(i in as.character(TargetLevels.[['OriginalLevels']])) {
      pk <- ValidationData.[Predict == eval(i), .N]
      sumPk2 <- c(sumPk2, pk ^ 2)
    }

    # (s^2 - sum(tk^2))
    if(Debug) print("# (s^2 - sum(tk^2))")
    sumTk2 <- c()
    for(i in as.character(TargetLevels.[['OriginalLevels']])) {
      tk <- ValidationData.[get(TargetColumnName.) == eval(i), .N]
      sumTk2 <- c(sumTk2, tk ^ 2)
    }

    # Result
    if(Debug) print("# Result")
    denom <- sqrt((s^2 - sum(sumPk2))) * sqrt((s^2 - sum(sumTk2)))
    MCC <- (c / denom * s  - sum(sumPkTk) / denom)
  } else {
    MCC <- NA_real_
  }

  # MultiClass Evaluation Metrics
  if(Debug) print("# MultiClass Evaluation Metrics")
  EvaluationMetrics <- data.table::data.table(Metric = c("MCC","MicroAUC","Accuracy","LogLoss"), MetricValue = c(MCC, MetricAUC, MetricAcc, logloss))
  if(SaveModelObjects.) {
    if(!is.null(metadata_path.)) {
      if(tolower(DataType) == "train") data.table::fwrite(EvaluationMetrics, file = file.path(metadata_path., paste0(ModelID., "_Train_EvaluationMetrics.csv")))
      if(tolower(DataType) == "test") data.table::fwrite(EvaluationMetrics, file = file.path(metadata_path., paste0(ModelID., "_Test_EvaluationMetrics.csv")))
    }
  }
  if(Debug) print("# Return")
  return(EvaluationMetrics)
}
