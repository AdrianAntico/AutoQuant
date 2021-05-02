#' @title EvalPlot
#'
#' @description This function automatically builds calibration plots and calibration boxplots for model evaluation using regression, quantile regression, and binary and multinomial classification
#'
#' @author Adrian Antico
#' @family Model Evaluation and Interpretation
#'
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
  data[, rank := round(data.table::frank(preds) * (1 / PercentileBucket) / .N) * PercentileBucket]

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

#' @title ParDepCalPlots
#'
#' @description This function automatically builds partial dependence calibration plots and partial dependence calibration boxplots for model evaluation using regression, quantile regression, and binary and multinomial classification
#'
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
  if(data[,.N] > 1000000) data <- data[order(runif(.N))][seq_len(1000000)]

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
  if(is.numeric(preds2[[IndepVar]]) || is.integer(preds2[[IndepVar]])) {
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

#' @title ROCPlot
#'
#' @description Internal usage for classification methods. Returns an ROC plot
#'
#' @author Adrian Antico
#' @family Model Evaluation and Interpretation
#'
#' @param data validation data
#' @param TargetName Target variable name
#' @param SavePlot TRUE or FALSE
#' @param Name Name for saving
#' @param metapath Passthrough
#' @param modelpath Passthrough
#'
#' @return ROC Plot for classification models
#' @export
ROCPlot <- function(data = ValidationData,
                    TargetName = TargetColumnName,
                    SavePlot = SaveModelObjects,
                    Name = ModelID,
                    metapath = metadata_path,
                    modelpath = model_path) {

  # AUC measture
  temp <- data[order(runif(.N))][seq_len(min(100000L, .N))]
  AUC_Metrics <- pROC::roc(
    response = temp[[eval(TargetName)]],
    predictor = temp[["p1"]],
    na.rm = TRUE,
    algorithm = 3L,
    auc = TRUE,
    ci = TRUE)
  rm(temp)

  # Turn into a data.table
  AUC_Data <- data.table::data.table(
    ModelNumber = 0L,
    Sensitivity = AUC_Metrics$sensitivities,
    Specificity = AUC_Metrics$specificities)

  # Create plot
  ROC_Plot <- ggplot2::ggplot(AUC_Data, ggplot2::aes(x = 1 - Specificity)) +
    ggplot2::geom_line(ggplot2::aes(y = AUC_Data[["Sensitivity"]]), color = "blue") +
    ggplot2::geom_abline(slope = 1, color = "black") +
    ggplot2::ggtitle(paste0("Catboost AUC: ", 100 * round(AUC_Metrics$auc, 3), "%")) +
    ChartTheme() + ggplot2::xlab("Specificity") +
    ggplot2::ylab("Sensitivity")

  # Save plot
  if(SavePlot) {
    if(!is.null(metapath)) {
      ggplot2::ggsave(plot = ROC_Plot, filename = paste0(Name, "_ROC_Plot.png"), path = file.path(metapath))
    } else {
      ggplot2::ggsave(plot = ROC_Plot, filename = paste0(Name, "_ROC_Plot.png"), path = file.path(modelpath))
    }
  }

  # Return
  return(list(ROC_Plot = ROC_Plot, AUC_Metrics = AUC_Metrics))
}

#' @title VI_Plot
#'
#' @description Generate variable importance plots
#'
#' @author Adrian Antico
#' @family Model Evaluation and Interpretation
#'
#' @param Type 'catboost', 'xgboost', 'h2o'
#' @param VI_Data Source data
#' @param ColorHigh darkblue
#' @param ColorLow white
#' @param TopN Number of variables to display
#'
#' @return ROC Plot for classification models
#' @noRd
VI_Plot <- function(Type = "catboost",
                    VI_Data = NULL,
                    ColorHigh = "darkblue",
                    ColorLow = "white",
                    TopN = 10) {

  # Catboost
  if(Type == "catboost") {
    return(eval(
      ggplot2::ggplot(VI_Data[seq_len(min(TopN,.N))], ggplot2::aes(x = reorder(Variable, abs(Importance)), y = Importance, fill = Importance)) +
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::scale_fill_gradient2(mid = ColorLow, high = ColorHigh) +
        ChartTheme(Size = 12L, AngleX = 0L, LegendPosition = "right") +
        ggplot2::coord_flip() +
        ggplot2::labs(title = "Global Variable Importance") +
        ggplot2::xlab("Top Model Features") +
        ggplot2::ylab("Value") +
        ggplot2::theme(legend.position = "none")))
  }

  # XGBoost
  if(Type == "xgboost") {
    return(eval(ggplot2::ggplot(VI_Data[seq_len(min(TopN,.N))], ggplot2::aes(x = reorder(Feature, Gain), y = Gain, fill = Gain)) +
                  ggplot2::geom_bar(stat = "identity") +
                  ggplot2::scale_fill_gradient2(mid = ColorLow, high = ColorHigh) +
                  ChartTheme(Size = 12L, AngleX = 0L, LegendPosition = "right") +
                  ggplot2::coord_flip() +
                  ggplot2::labs(title = "Global Variable Importance") +
                  ggplot2::xlab("Top Model Features") +
                  ggplot2::ylab("Value") +
                  ggplot2::theme(legend.position = "none")))
  }

  # H2O
  if(Type == "h2o") {
    return(eval(ggplot2::ggplot(VI_Data[seq_len(min(TopN,.N))], ggplot2::aes(x = reorder(Variable, ScaledImportance ), y = ScaledImportance , fill = ScaledImportance )) +
                  ggplot2::geom_bar(stat = "identity") +
                  ggplot2::scale_fill_gradient2(mid = ColorLow,high = ColorHigh) +
                  ChartTheme(Size = 12L, AngleX = 0L, LegendPosition = "right") +
                  ggplot2::coord_flip() +
                  ggplot2::labs(title = "Global Variable Importance") +
                  ggplot2::xlab("Top Model Features") +
                  ggplot2::ylab("Value") +
                  ggplot2::theme(legend.position = "none")))
  }
}

#' @title CumGainsChart
#'
#' @description Create a cumulative gains chart
#'
#' @family Model Evaluation and Interpretation
#'
#' @author Adrian Antico
#'
#' @param data Test data with predictions. data.table
#' @param PredictedColumnName Name of column that is the model score
#' @param TargetColumnName Name of your target variable column
#' @param NumBins Number of percentile bins to plot
#' @param SavePlot FALSE by default
#' @param Name File name for saving
#' @param metapath Path to directory
#' @param modelpath Path to directory
#'
#' @export
CumGainsChart <- function(data = NULL,
                          PredictedColumnName = "p1",
                          TargetColumnName = NULL,
                          NumBins = 20,
                          SavePlot = FALSE,
                          Name = NULL,
                          metapath = NULL,
                          modelpath = NULL) {
  options(scipen = 999)
  temp <- data[, .SD, .SDcols = c(eval(PredictedColumnName), eval(TargetColumnName))]
  temp[, NegScore := -get(PredictedColumnName)]
  if(NumBins < 1 || missing(NumBins)) NumBins <- 20
  Bins <- c(seq(1/NumBins, 1 - 1/NumBins, 1/NumBins), 1)
  Cuts <- quantile(x = temp[["NegScore"]], probs = Bins)
  temp[, eval(TargetColumnName) := as.character(get(TargetColumnName))][, eval(TargetColumnName) := as.character(get(TargetColumnName))]
  grp <- temp[, .N, by = eval(TargetColumnName)][order(N)]
  smaller_class <- grp[1L, 1L][[1L]]
  LiftTable <- round(100 * sapply(Cuts, function(x) {
    temp[NegScore <= x & get(TargetColumnName) == eval(smaller_class), .N] / temp[get(TargetColumnName) == eval(smaller_class), .N]
  }), 2)
  LiftRes <- rbind(LiftTable, -Cuts)
  rownames(LiftRes) <- c("Gain", "Score.Point")
  likelihood_lrc <- grp[1,2] / (grp[2,2] + grp[1,2])
  LiftRes_T <- data.table::as.data.table(t(LiftRes))
  LiftRes_T[, Population := as.numeric(100 * eval(Bins))]
  LiftRes_T[, Lift := round(Gain / 100 / Bins, 2)]
  LiftRes_T_Gain <- data.table::rbindlist(list(data.table::data.table(Gain = 0, Score.Point = 0, Population = 0, Lift = 0), LiftRes_T))

  # Create Gains Plot
  p_gain <- eval(ggplot2::ggplot(
    data = LiftRes_T_Gain,
    ggplot2::aes(x = Population, y = Gain, label = round(Gain, 1), group = 1)) +
      ggplot2::geom_line(stat = "identity") +
      ggplot2::geom_point(ggplot2::aes(colour = Gain)) +
      ggplot2::theme_bw() +
      ggplot2::ylab("Cumulative Gain (%)") + ggplot2::xlab("Population (%)") +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        legend.title = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(vjust = 2),
        axis.ticks.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_text(margin = ggplot2::margin(15,0,0,0)),
        axis.title.y = ggplot2::element_text(margin = ggplot2::margin(1,15,0,0))) +
      ggplot2::geom_label(
        ggplot2::aes(fill = factor(Gain)),
        colour = "white",
        fontface = "bold",
        vjust = -0.5,
        label.padding = ggplot2::unit(0.2, "lines")) +
      ggplot2::ylim(0,110) +
      ggplot2::guides(fill = FALSE) +
      ggplot2::scale_colour_continuous(breaks = c(0, seq(10, 100, 10))))

  # Create Lift Chart
  p_lift <- (ggplot2::ggplot(
    data = LiftRes_T,
    ggplot2::aes(Population, Lift, label = Lift, group = 1)) +
      ggplot2::geom_line(stat = "identity") +
      ggplot2::geom_point(ggplot2::aes(colour = Lift)) +
      ggplot2::theme_bw() +
      ggplot2::ylab("Lift") +
      ggplot2::xlab("Population (%)") +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        legend.title = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(vjust = 2),
        axis.ticks.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_text(margin = ggplot2::margin(15,0,0,0)),
        axis.title.y = ggplot2::element_text(margin = ggplot2::margin(0,15,0,0))) +
      ggplot2::geom_label(
        ggplot2::aes(fill = -Lift),
        size = 3.5,
        colour = "white",
        vjust = -0.5,
        label.padding = ggplot2::unit(0.2, "lines")) +
      ggplot2::ylim(min(LiftRes_T[["Lift"]]), max(LiftRes_T[["Lift"]] * 1.1)) +
      ggplot2::guides(fill = FALSE) +
      ggplot2::scale_colour_continuous(guide = FALSE) +
      ggplot2::scale_x_continuous(breaks = c(0, seq(10,100,10))))

  # Save plot
  if(SavePlot) {
    if(!is.null(metapath)) {
      ggplot2::ggsave(plot = p_gain, filename = paste0(Name, "_Gain_Plot.png"), path = file.path(metapath))
      ggplot2::ggsave(plot = p_lift, filename = paste0(Name, "_Lift_Plot.png"), path = file.path(metapath))
    } else {
      ggplot2::ggsave(plot = p_gain, filename = paste0(Name, "_Gain_Plot.png"), path = file.path(modelpath))
      ggplot2::ggsave(plot = p_lift, filename = paste0(Name, "_Lift_Plot.png"), path = file.path(modelpath))
    }
  }

  # Return
  return(list(GainsPlot = p_gain, LiftPlot = p_lift))
}

#' @title ML_EvalPlots
#'
#' @description Generate evaluation plots
#'
#' @author Adrian
#' @family Model Evaluation and Interpretation
#'
#' @param ModelType 'classification', 'multiclass', 'regression', or 'vector'
#' @param TrainOnFull. Passthrough
#' @param LossFunction. Passthrough regression
#' @param EvalMetric. Passthrough regression
#' @param EvaluationMetrics. Passthrough regression
#' @param ValidationData. Passthrough
#' @param NumOfParDepPlots. Passthrough
#' @param VariableImportance. Passthrough
#' @param TargetColumnName. Passthrough
#' @param FeatureColNames. Passthrough
#' @param SaveModelObjects. Passthrough
#' @param ModelID. Passthrough
#' @param model_path. Passthrough
#' @param metadata_path. Passthrough
#'
#' @noRd
ML_EvalPlots <- function(ModelType = "classification",
                         TrainOnFull. = TrainOnFull,
                         LossFunction. = LossFunction,
                         EvalMetric. = EvalMetric,
                         EvaluationMetrics. = EvaluationMetrics,
                         ValidationData. = ValidationData,
                         NumOfParDepPlots. = NumOfParDepPlots,
                         VariableImportance. = VariableImportance,
                         TargetColumnName. = TargetColumnName,
                         FeatureColNames. = FeatureColNames,
                         SaveModelObjects. = SaveModelObjects,
                         ModelID. = ModelID,
                         metadata_path. = metadata_path,
                         model_path. = model_path,
                         predict. = predict) {

  # Classification
  if(ModelType == "classification") {

    # Full data
    if(!TrainOnFull.) {

      # ROC
      Output <- ROCPlot(data = ValidationData., TargetName = TargetColumnName., SavePlot = SaveModelObjects., Name = ModelID., metapath = metadata_path., modelpath = model_path.)
      ROC_Plot <- Output$ROC_Plot
      AUC_Metrics <- Output$AUC_Metrics
      rm(Output)

      # Decile
      EvaluationPlot <- EvalPlot(data = ValidationData., PredictionColName = "p1", TargetColName = eval(TargetColumnName.), GraphType = "calibration", PercentileBucket = 0.05, aggrfun = function(x) mean(x, na.rm = TRUE))
      EvaluationPlot <- EvaluationPlot + ggplot2::ggtitle(paste0("Calibration Evaluation Plot: AUC = ", round(AUC_Metrics$auc, 3L)))
      if(SaveModelObjects.) {
        if(!is.null(metadata_path.)) {
          ggplot2::ggsave(file.path(metadata_path., paste0(ModelID., "_EvaluationPlot.png")))
        } else {
          ggplot2::ggsave(file.path(model_path., paste0(ModelID., "_EvaluationPlot.png")))
        }
      }

      # Gains and Lift
      Output <- CumGainsChart(data = ValidationData., PredictedColumnName = "p1", TargetColumnName = eval(TargetColumnName.), NumBins = 20, SavePlot = SaveModelObjects., Name = ModelID., metapath = metadata_path., modelpath = model_path.)
      Gains <- Output$GainsPlot
      Lift <- Output$LiftPlot; rm(Output)

      # ParDep
      ParDepPlots <- list()
      j <- 0L
      if(!is.null(VariableImportance.) && NumOfParDepPlots. > 0L && !TrainOnFull.) {
        for(i in seq_len(min(length(FeatureColNames.), NumOfParDepPlots., VariableImportance.[,.N]))) {
          tryCatch({
            Out <- ParDepCalPlots(
              data = ValidationData.,
              PredictionColName = "p1",
              TargetColName = eval(TargetColumnName.),
              IndepVar = if("Variable" %in% names(VariableImportance.)) gsub("\\..*","", VariableImportance.[i, Variable]) else gsub("\\..*","", VariableImportance.[i, Feature]),
              GraphType = "calibration",
              PercentileBucket = 0.05,
              FactLevels = 10L,
              Function = function(x) mean(x, na.rm = TRUE))
            j <- j + 1L
            if("Variable" %in% names(VariableImportance.)) ParDepPlots[[paste0(VariableImportance.[j, Variable])]] <- Out else ParDepPlots[[paste0(VariableImportance.[j, Feature])]] <- Out
          }, error = function(x) "skip")
        }
      }
      if(SaveModelObjects.) {
        if(!is.null(metadata_path.)) {
          if(!is.null(VariableImportance.)) save(ParDepPlots, file = file.path(metadata_path., paste0(ModelID., "_ParDepPlots.R")))
        } else {
          if(!is.null(VariableImportance.)) save(ParDepPlots, file = file.path(model_path., paste0(ModelID., "_ParDepPlots.R")))
        }
      }
    } else {
      ROC_Plot <- NULL
      EvaluationPlot <- NULL
      ParDepPlots <- NULL
      Gains <- NULL
      Lift <- NULL
    }

    # Return
    return(list(ROC_Plot = ROC_Plot, EvaluationPlot = EvaluationPlot, GainsPlot = Gains, LiftPlot = Lift, ParDepPlots = ParDepPlots))
  }

  # Regression
  if(ModelType %chin% c("regression", "vector")) {
    if(!TrainOnFull. && ((!is.null(LossFunction.) && LossFunction. != "MultiRMSE") || (!is.null(EvalMetric.) && EvalMetric. != "MultiRMSE"))) {

      # Eval Plots
      EvaluationPlot <- EvalPlot(
        data = ValidationData.,
        PredictionColName = "Predict",
        TargetColName = eval(TargetColumnName.),
        GraphType = "calibration",
        PercentileBucket = 0.05,
        aggrfun = function(x) mean(x, na.rm = TRUE))

      # Add Number of Trees to Title
      if(!TrainOnFull.) EvaluationPlot <- EvaluationPlot + ggplot2::ggtitle(paste0("Calibration Evaluation Plot: R2 = ", round(EvaluationMetrics.[Metric == "R2", MetricValue], 3L)))

      # Save plot to file
      if(!TrainOnFull.) {
        if(SaveModelObjects.) {
          if(!is.null(metadata_path.)) {
            ggplot2::ggsave(file.path(metadata_path., paste0(ModelID., "_EvaluationPlot.png")))
          } else {
            ggplot2::ggsave(file.path(model_path., paste0(ModelID., "_EvaluationPlot.png")))
          }
        }
      }

      # Regression Evaluation Calibration BoxPlot----
      EvaluationBoxPlot <- EvalPlot(
        data = ValidationData.,
        PredictionColName = "Predict",
        TargetColName = eval(TargetColumnName.),
        GraphType = "boxplot",
        PercentileBucket = 0.05,
        aggrfun = function(x) mean(x, na.rm = TRUE))

      # Add Number of Trees to Title
      if(!TrainOnFull.) EvaluationBoxPlot <- EvaluationBoxPlot + ggplot2::ggtitle(paste0("Calibration Evaluation Plot: R2 = ", round(EvaluationMetrics.[Metric == "R2", MetricValue], 3L)))

      # Save plot to file
      if(SaveModelObjects.) {
        if(!is.null(metadata_path.)) {
          ggplot2::ggsave(file.path(metadata_path., paste0(ModelID., "_EvaluationBoxPlot.png")))
        } else {
          ggplot2::ggsave(file.path(model_path., paste0(ModelID., "_EvaluationBoxPlot.png")))
        }
      }

      # Regression Partial Dependence
      if(!is.null(VariableImportance.)) {
        ParDepBoxPlots <- list()
        ParDepPlots <- list()
        if(NumOfParDepPlots. > 0L) {
          j <- 0L
          k <- 0L
          for(i in seq_len(min(length(FeatureColNames.), NumOfParDepPlots., VariableImportance.[,.N]))) {
            tryCatch({
              Out <- ParDepCalPlots(
                data = ValidationData.,
                PredictionColName = "Predict",
                TargetColName = eval(TargetColumnName.),
                IndepVar = if("Variable" %in% names(VariableImportance.)) gsub("\\..*","", VariableImportance.[i, Variable]) else gsub("\\..*","", VariableImportance.[i, Feature]),
                GraphType = "calibration",
                PercentileBucket = 0.05,
                FactLevels = 10L,
                Function = function(x) mean(x, na.rm = TRUE))
              j <- j + 1L
              if("Variable" %in% names(VariableImportance.)) ParDepPlots[[paste0(VariableImportance.[j, Variable])]] <- Out else ParDepPlots[[paste0(VariableImportance.[j, Feature])]] <- Out
            }, error = function(x) "skip")
            tryCatch({
              Out1 <- ParDepCalPlots(
                data = ValidationData.,
                PredictionColName = "Predict",
                TargetColName = eval(TargetColumnName.),
                IndepVar = if("Variable" %in% names(VariableImportance.)) VariableImportance.[i, Variable] else VariableImportance.[i, Feature],
                GraphType = "boxplot",
                PercentileBucket = 0.05,
                FactLevels = 10L,
                Function = function(x) mean(x, na.rm = TRUE))
              k <- k + 1L
              if("Variable" %in% names(VariableImportance.)) ParDepBoxPlots[[paste0(VariableImportance.[k, Variable])]] <- Out1 else ParDepBoxPlots[[paste0(VariableImportance.[k, Feature])]] <- Out1
            }, error = function(x) "skip")
          }

          # Regression Save ParDepPlots to file
          if(SaveModelObjects.) {
            if(!is.null(metadata_path.)) {
              save(ParDepPlots, file = file.path(metadata_path., paste0(ModelID., "_ParDepPlots.R")))
            } else {
              save(ParDepPlots, file = file.path(model_path., paste0(ModelID., "_ParDepPlots.R")))
            }
          }

          # Regression Save ParDepBoxPlots to file
          if(SaveModelObjects.) {
            if(!is.null(metadata_path.)) {
              save(ParDepBoxPlots, file = file.path(metadata_path., paste0(ModelID., "_ParDepBoxPlots.R")))
            } else {
              save(ParDepBoxPlots, file = file.path(model_path., paste0(ModelID., "_ParDepBoxPlots.R")))
            }
          }
        }
      } else {
        ParDepBoxPlots <- NULL
        ParDepPlots <- NULL
      }
    } else if(!TrainOnFull. && ((!is.null(LossFunction.) && LossFunction. == "MultiRMSE") || (!is.null(EvalMetric.) && EvalMetric. == "MultiRMSE"))) {

      # Initialize plots
      EvaluationPlot <- list()
      EvaluationBoxPlot <- list()
      ParDepBoxPlots <- list()
      ParDepPlots <- list()

      # Build plots
      for(TV in seq_along(TargetColumnName.)) {


        # Eval Plots
        EvaluationPlot[[TargetColumnName.[TV]]] <- EvalPlot(
          data = ValidationData.,
          PredictionColName = paste0("Predict.V",TV),
          TargetColName = eval(TargetColumnName.[TV]),
          GraphType = "calibration",
          PercentileBucket = 0.05,
          aggrfun = function(x) mean(x, na.rm = TRUE))

        # Add Number of Trees to Title
        if(!TrainOnFull.) EvaluationPlot[[TargetColumnName.[TV]]] <- EvaluationPlot[[TargetColumnName.[TV]]] + ggplot2::ggtitle(paste0("Calibration Evaluation Plot: R2 = ", round(EvaluationMetrics.[[TargetColumnName.[TV]]][Metric == "R2", MetricValue], 3L)))

        # Save plot to file
        if(!TrainOnFull.) {
          if(SaveModelObjects.) {
            if(!is.null(metadata_path.)) {
              ggplot2::ggsave(file.path(metadata_path., paste0(ModelID., "_EvaluationPlot.png")))
            } else {
              ggplot2::ggsave(file.path(model_path., paste0(ModelID., "_EvaluationPlot.png")))
            }
          }
        }

        # Regression Evaluation Calibration BoxPlot
        EvaluationBoxPlot[[TargetColumnName.[TV]]] <- EvalPlot(
          data = ValidationData.,
          PredictionColName = paste0("Predict.V",TV),
          TargetColName = eval(TargetColumnName.[TV]),
          GraphType = "boxplot",
          PercentileBucket = 0.05,
          aggrfun = function(x) mean(x, na.rm = TRUE))

        # Add Number of Trees to Title
        if(!TrainOnFull.) EvaluationBoxPlot[[TargetColumnName.[TV]]] <- EvaluationBoxPlot[[TargetColumnName.[TV]]] + ggplot2::ggtitle(paste0("Calibration Evaluation Plot: R2 = ", round(EvaluationMetrics.[[TargetColumnName.[TV]]][Metric == "R2", MetricValue], 3L)))

        # Save plot to file
        if(SaveModelObjects.) {
          if(!is.null(metadata_path.)) {
            ggplot2::ggsave(file.path(metadata_path., paste0(ModelID., "_EvaluationBoxPlot.png")))
          } else {
            ggplot2::ggsave(file.path(model_path., paste0(ModelID., "_EvaluationBoxPlot.png")))
          }
        }

        # Regression Partial Dependence
        if(!is.null(VariableImportance.)) {
          ParDepBoxPlots <- list()
          ParDepPlots <- list()
          if(NumOfParDepPlots. > 0L) {
            j <- 0L
            k <- 0L
            for(i in seq_len(min(length(FeatureColNames.), NumOfParDepPlots., VariableImportance.[,.N]))) {
              tryCatch({
                Out <- ParDepCalPlots(
                  data = ValidationData.,
                  PredictionColName = paste0("Predict.V",TV),
                  TargetColName = eval(TargetColumnName.[TV]),
                  IndepVar = gsub("\\..*","", VariableImportance.[i, Variable]),
                  GraphType = "calibration",
                  PercentileBucket = 0.05,
                  FactLevels = 10L,
                  Function = function(x) mean(x, na.rm = TRUE))
                j <- j + 1L
                ParDepPlots[[paste0(TargetColumnName.[TV],"_",VariableImportance.[j, Variable])]] <- Out
              }, error = function(x) "skip")
              tryCatch({
                Out1 <- ParDepCalPlots(
                  data = ValidationData.,
                  PredictionColName = paste0("Predict.V",TV),
                  TargetColName = eval(TargetColumnName.[TV]),
                  IndepVar = gsub("\\..*","", VariableImportance.[i, Variable]),
                  GraphType = "boxplot",
                  PercentileBucket = 0.05,
                  FactLevels = 10L,
                  Function = function(x) mean(x, na.rm = TRUE))
                k <- k + 1L
                ParDepBoxPlots[[paste0(TargetColumnName.[TV],"_",VariableImportance.[k, Variable])]] <- Out1
              }, error = function(x) "skip")
            }

            # Regression Save ParDepPlots to file
            if(SaveModelObjects.) {
              if(!is.null(metadata_path.)) {
                save(ParDepPlots, file = file.path(metadata_path., paste0(ModelID., "_", TargetColumnName.[TV], "_ParDepPlots.R")))
              } else {
                save(ParDepPlots, file = file.path(model_path., paste0(ModelID., "_", TargetColumnName.[TV], "_ParDepPlots.R")))
              }
            }

            # Regression Save ParDepBoxPlots to file
            if(SaveModelObjects.) {
              if(!is.null(metadata_path.)) {
                save(ParDepBoxPlots, file = file.path(metadata_path., paste0(ModelID., "_", TargetColumnName.[TV], "_ParDepBoxPlots.R")))
              } else {
                save(ParDepBoxPlots, file = file.path(model_path., paste0(ModelID., "_", TargetColumnName.[TV], "_ParDepBoxPlots.R")))
              }
            }
          }
        } else {
          ParDepBoxPlots <- NULL
          ParDepPlots <- NULL
        }
      }
    } else {
      EvaluationPlot <- NULL
      EvaluationBoxPlot <- NULL
      ParDepBoxPlots <- NULL
      ParDepPlots <- NULL
    }
    return(list(EvaluationPlot = EvaluationPlot, EvaluationBoxPlot = EvaluationBoxPlot, ParDepPlots = ParDepPlots, ParDepBoxPlots = ParDepBoxPlots))
  }
}

#' @title CumGainsChart
#'
#' @description Create a cumulative gains chart
#'
#' @family Model Evaluation and Interpretation
#'
#' @author Adrian Antico
#'
#' @param data Test data with predictions. data.table
#' @param PredictionColumnName Name of column that is the model score
#' @param TargetColumnName Name of your target variable column
#' @param NumBins Number of percentile bins to plot
#' @param SavePlot FALSE by default
#' @param Name File name for saving
#' @param metapath Path to directory
#' @param modelpath Path to directory
#'
#' @export
ShapPlot <- function(ShapData = NULL,
                     VarList = NULL,
                     PlotTitle = "Shap Plot") {

  eval(
    ggplot2::ggplot(
      data = ShapData[Variable %chin% VarList],
      ggplot2::aes(
        x = Variable,
        y = SumShapValue,
        fill = Variable)) +
      ggplot2::geom_boxplot() +
      ggplot2::coord_flip() +
      ChartTheme() +
      ggplot2::theme(
        legend.position = "none",
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank()) +
      viridis::scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
      ggplot2::geom_jitter(color = "black", size = 0.4, alpha = 0.9) +
      ggplot2::ggtitle(PlotTitle))

}
