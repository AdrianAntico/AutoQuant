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
#' RemixAutoML::EvalPlot(
#'   data,
#'   PredictionColName = "Predict",
#'   TargetColName = "Adrian",
#'   GraphType = "calibration",
#'   PercentileBucket = 0.05,
#'   aggrfun = function(x) mean(x, na.rm = TRUE))
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
  if(data[,.N] > 1000000) data <- data[order(runif(.N))][seq_len(1000000)]

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
    plot <- eval(ggplot2::ggplot(data, ggplot2::aes(x = rank, y = output, fill = Type)) +
      ggplot2::geom_boxplot(outlier.color = "red", color = "black") +
      ggplot2::ggtitle("Calibration Evaluation Boxplot") +
      ggplot2::xlab("Predicted Percentile") +
      ggplot2::ylab("Observed Values") +
      ChartTheme(Size = 15) +
      ggplot2::scale_fill_manual(values = c("red", "blue")))

  } else {
    data <- data[, lapply(.SD, noquote(aggrfun)), by = list(rank)]
    plot <- eval(
      ggplot2::ggplot(data, ggplot2::aes(x = rank))  +
        ggplot2::geom_line(ggplot2::aes(y = data[[3L]], color = "Actual")) +
        ggplot2::geom_line(ggplot2::aes(y = data[[2L]], color = "Predicted")) +
        ggplot2::xlab("Predicted Percentile") +
        ggplot2::ylab("Observed Values") +
        ggplot2::scale_color_manual(values = c("red", "blue")) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::ggtitle("Calibration Evaluation Plot") +
        ggplot2::scale_fill_manual(values = c("blue", "gold")) +
        ChartTheme(Size = 15))
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
#' @param DateColumn Add date column for 3D scatterplot
#' @param DateAgg_3D Aggregate date column by 'day', 'week', 'month', 'quarter', 'year'
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
#'   Function = function(x) mean(x, na.rm = TRUE),
#'   DateColumn = NULL,
#'   DateAgg_3D = NULL)
#' }
#' @export
ParDepCalPlots <- function(data,
                           PredictionColName = c("PredictedValues"),
                           TargetColName  = c("ActualValues"),
                           IndepVar = c("Independent_Variable_Name"),
                           GraphType = c("calibration"),
                           PercentileBucket = 0.05,
                           FactLevels = 10,
                           Function = function(x) mean(x, na.rm = TRUE),
                           DateColumn = NULL,
                           DateAgg_3D = NULL,
                           PlotYMeanColor = 'black',
                           PlotXMeanColor = 'chocolate',
                           PlotXLowColor = 'purple',
                           PlotXHighColor = 'purple') {

  # Turn off ggplot2 warnings ----
  options(warn = -1L)

  # Cap number of records ----
  if(data[,.N] > 1000000) data <- data[order(runif(.N))][seq_len(1000000)]

  # Build buckets by independent variable of choice ----
  if(is.null(DateAgg_3D) || is.null(DateColumn)) {
    preds2 <- data[, .SD, .SDcols = c(PredictionColName, TargetColName, IndepVar)]
    data.table::setcolorder(data, c(PredictionColName, TargetColName, IndepVar))
  } else {
    preds2 <- data[, .SD, .SDcols = c(PredictionColName, TargetColName, IndepVar, DateColumn)]
    data.table::setcolorder(data, c(DateColumn, PredictionColName, TargetColName, IndepVar))
  }

  # If actual is in factor form, convert to numeric ----
  if(!is.numeric(preds2[[TargetColName]])) {
    preds2[, eval(TargetColName) := as.numeric(as.character(get(TargetColName)))]
    GraphType <- "calibration"
  }

  # Prepare for both calibration and boxplot ----
  if(is.numeric(preds2[[IndepVar]]) || is.integer(preds2[[IndepVar]])) {
    preds2[, rank := round(data.table::frank(preds2[[IndepVar]]) * (1/PercentileBucket) /.N) * PercentileBucket]
  } else {
    GraphType <- "FactorVar"
    preds2 <- preds2[, .(Function(get(TargetColName)), Function(get(PredictionColName)), .N), by = get(IndepVar)][order(-N)]
    if(nrow(preds2) > FactLevels) {
      temp1 <- preds2[seq_len(min(.N,FactLevels))]
      temp2 <- preds2[(FactLevels + 1):nrow(preds2)]
      temp2[, ':=' (V1 = V1 * N / sum(N), V2 = V2 * N / sum(N))]
      temp3 <- temp2[, list(sum(V1), sum(V2), N = sum(N))]
      temp3[, get := "Other"]
      data.table::setcolorder(temp3, c(3L, 1L, 2L))
      preds3 <- data.table::rbindlist(list(temp1, temp3), use.names = TRUE)
    }
    if(nrow(preds2) <= FactLevels) preds3 <- preds2
    data.table::setnames(preds3, old = c("get", "V1", "V2"), new = c(IndepVar, TargetColName, PredictionColName))
    preds3 <- preds3[order(-N)]
  }

  # Build plots ----
  if(GraphType == "calibration") {
    if(class(preds2[[eval(IndepVar)]])[1L] != "numeric") preds2[, eval(IndepVar) := as.numeric(get(IndepVar))]
    preds3 <- preds2[, lapply(.SD, noquote(Function)), by = "rank"][order(rank)]

    # Cross section plot
    plot <- eval(
      ggplot2::ggplot(preds3, ggplot2::aes(x = preds3[[IndepVar]])) +
        ggplot2::geom_line(ggplot2::aes(y = preds3[[PredictionColName]], color = "Predicted")) +
        ggplot2::geom_line(ggplot2::aes(y = preds3[[TargetColName]], color = "Actuals")) +
        ggplot2::ylab(eval(TargetColName)) +
        ggplot2::xlab(IndepVar) +
        ggplot2::scale_colour_manual("", breaks = c("Actuals", "Predicted"), values = c("red", "blue")) +
        ChartTheme(Size = 13, AngleX = 90) +
        ggplot2::labs(
          title = "Partial Dependence Calibration Plot",
          subtitle = paste0("black line -> mean(", TargetColName,"); purple lines -> 10th & 90th-%tile; chocolate line = mean")) +
        ggplot2::geom_hline(yintercept = data[, mean(get(TargetColName))], color = "black") +
        ggplot2::geom_vline(xintercept = data[, mean(get(IndepVar))], color = "chocolate") +
        ggplot2::geom_vline(xintercept = data[, quantile(get(IndepVar), probs = 0.10)][[1L]], color = "purple") +
        ggplot2::geom_vline(xintercept = data[, quantile(get(IndepVar), probs = 0.90)][[1L]], color = "purple"))

    # Heatmap
    if(!is.null(DateColumn) && !is.null(DateAgg_3D)) {
      preds2[, Time := lubridate::floor_date(get(DateColumn), unit = DateAgg_3D)]
      preds4 <- preds2[, lapply(.SD, noquote(Function)), by = c("rank", "Time")][order(rank, Time)]
      data.table::setnames(preds4, eval(TargetColName), "Target_Variable")
      plot2 <- ggplot2::ggplot(data = preds4, ggplot2::aes(x = Time, y = rank, fill = Target_Variable)) +
        ggplot2::geom_tile() +
        ChartTheme() +
        ggplot2::xlab(DateColumn) + ggplot2::ylab(paste0(IndepVar, "_Percentile"))
    }
  } else if(GraphType == "boxplot") {
    keep <- c("rank", TargetColName, IndepVar)
    actual <- preds2[, ..keep]
    actual[, Type := "actual"]
    data.table::setnames(actual, TargetColName, "Output")
    keep <- c("rank", PredictionColName, IndepVar)
    preds2 <- preds2[, ..keep]
    preds2[, Type := "predicted"]
    data.table::setnames(preds2, PredictionColName, "Output")
    preds2 <- data.table::rbindlist(list(actual, preds2))[order(rank)]
    preds2[, rank := as.factor(rank)]
    preds2 <- preds2[, eval(IndepVar) := as.numeric(get(IndepVar))]
    preds2 <- preds2[, eval(IndepVar) := round(Function(get(IndepVar)), 3L), by = rank]
    preds2[, eval(IndepVar) := as.factor(get(IndepVar))]
    preds2[, rank := NULL]
    plot <- eval(
      ggplot2::ggplot(preds2, ggplot2::aes(x = preds2[[IndepVar]], y = Output)) +
        ggplot2::geom_boxplot(ggplot2::aes(fill = Type)) +
        ggplot2::scale_fill_manual(values = c("red", "blue")) +
        ggplot2::labs(
          title = "Partial Dependence Calibration Plot",
          subtitle = paste0("black line -> mean(", TargetColName,")")) +
        ggplot2::xlab(eval(IndepVar)) +
        ggplot2::ylab(eval(TargetColName)) +
        ChartTheme(Size = 13) +
        ggplot2::geom_hline(yintercept = data[, mean(get(TargetColName))], color = "black"))
  } else if(GraphType == "FactorVar") {
    keep <- c(IndepVar, TargetColName, "N")
    actual <- preds3[, ..keep]
    actual[, Type := "actual"]
    data.table::setnames(actual, TargetColName, "Output")
    keep <- c(IndepVar, PredictionColName)
    preds3 <- preds3[, ..keep]
    preds3[, Type := "predicted"]
    data.table::setnames(preds3, PredictionColName, "Output")
    preds3 <- data.table::rbindlist(list(actual, preds3), fill = TRUE)[order(-N)]
    plot <- eval(
      ggplot2::ggplot(preds3, ggplot2::aes(x = preds3[[IndepVar]], y = Output)) +
       ggplot2::geom_bar(stat = "identity", position = "dodge", ggplot2::aes(fill = Type)) +
        ggplot2::scale_fill_manual(values = c("red", "blue")) +
        ggplot2::labs(
          title = "Partial Dependence Calibration Plot",
          subtitle = paste0("Black line -> mean(", TargetColName,"); Labels -> counts per bucket")) +
        ggplot2::xlab(eval(IndepVar)) +
        ggplot2::ylab(eval(TargetColName)) +
        RemixAutoML::ChartTheme(Size = 13) +
        ggplot2::geom_hline(yintercept = data[, mean(get(TargetColName))], color = "black") +
        ggplot2::geom_text(ggplot2::aes(label= N), vjust=-1.25, hjust = 1.5, color = "black"))
  }

  # Return plot----
  if(!is.null(DateColumn) && !is.null(DateAgg_3D)) {
    return(list(CrossSection = plot, HeatMap = plot2))
  } else {
    return(plot)
  }
}

#' @title ResidualPlots
#'
#' @description Residual plots for regression models
#'
#' @author Adrian Antico
#'
#' @family Model Evaluation and Interpretation
#'
#' @param TestData = NULL,
#' @param Target = "Adrian",
#' @param Predicted = "Independent_Variable1",
#' @param DateColumnName "DateTime"
#' @param Gam_Fit = TRUE
#'
#' @examples
#' \dontrun{
#' # Create fake data
#' test_data <- RemixAutoML::FakeDataGenerator(
#'   Correlation = 0.80,
#'   N = 250000,
#'   ID = 0,
#'   FactorCount = 0,
#'   AddDate = TRUE,
#'   AddComment = FALSE,
#'   AddWeightsColumn = FALSE,
#'   ZIP = 0)
#'
#' # Build Plots
#' output <- RemixAutoML::ResidualPlots(
#'   TestData = test_data,
#'   Target = "Adrian",
#'   Predicted = "Independent_Variable1",
#'   DateColumnName = "DateTime",
#'   Gam_Fit = TRUE)
#' }
#'
#' @export
ResidualPlots <- function(TestData = NULL,
                          Target = "Adrian",
                          Predicted = "Independent_Variable1",
                          DateColumnName = NULL,
                          Gam_Fit = FALSE) {

  # Subset + Sample
  R2_Pearson <- c()
  R2_Spearman <- c()
  if(TestData[,.N] < 100000L) {
    for(zz in seq_len(30L)) {
      temp <- TestData[order(runif(.N))][seq_len(0.50 * .N)]
      R2_Pearson <- c(R2_Pearson, (cor(x = temp[[Target]], y = temp[[Predicted]], method = "pearson")) ^ 2)
      R2_Spearman <- c(R2_Spearman, (cor(x = temp[[Target]], y = temp[[Predicted]], method = "spearman")) ^ 2)
    }
  } else {
    for(zz in seq_len(30L)) {
      temp <- TestData[order(runif(.N))][seq_len(min(100000L, .N))]
      R2_Pearson <- c(R2_Pearson, (cor(x = temp[[Target]], y = temp[[Predicted]], method = "pearson")) ^ 2)
      R2_Spearman <- c(R2_Spearman, (cor(x = temp[[Target]], y = temp[[Predicted]], method = "spearman")) ^ 2)
    }
  }

  # Histogram of residuals
  temp <- TestData[, .SD, .SDcols = c(Target, Predicted)]
  temp[, Residuals := get(Target) - get(Predicted)]
  ResidualsHistogram <- eval(ggplot2::ggplot(
    data = temp, ggplot2::aes_string(x = "Residuals")) +
      ggplot2::geom_histogram(ggplot2::aes(y = ..density..),bins = 30) +
      ggplot2::geom_density(alpha = 0.2, fill = "antiquewhite3") +
      ChartTheme() +
      ggplot2::labs(
        title = paste0("Residual Analsis: r-sq's based N=30 bootstrap; r-sq = sqrt(cor)"),
        subtitle = paste0("r-sq pearson xbar = ", round(mean(R2_Pearson),3L), " +/- ", round(sd(R2_Pearson) / sqrt(30L), 5L)," :: ",
                          "r-sq spearman xbar = ", round(mean(R2_Spearman),3L), " +/- ", round(sd(R2_Spearman) / sqrt(30L), 5L))))

  # Residuals over time
  if(!is.null(DateColumnName) && DateColumnName %chin% names(TestData)) {
    temp <- TestData[order(runif(.N))][seq_len(100000)]
    data.table::setorderv(temp[, Residual := get(Target) - get(Predicted)], cols = DateColumnName, order = 1)
    ResidualTime <- ggplot2::ggplot(data = temp, ggplot2::aes_string(x = DateColumnName, y = "Residual")) +
      ggplot2::geom_point(size = 0.10) +
      ChartTheme() +
      ggplot2::ggtitle("Residuals Over Time")

    # Gam Fit
    if(Gam_Fit) ResidualTime <- ResidualTime + ggplot2::geom_smooth(method = "gam")

  } else {
    ResidualTime <- NULL
  }

  # ScatterCopula
  Output <- ScatterCopula(data=TestData, x_var=Predicted, y_var=Target, GroupVariable=NULL, SampleCount=100000L, FitGam=Gam_Fit)
  ScatterPlot <- Output[["ScatterPlot"]]
  ScatterPlot <- ScatterPlot + ggplot2::labs(
    title = paste0("Scatter Plot: Actual vs Predicted"),
    subtitle = paste0("r-sq pearson xbar = ", round(mean(R2_Pearson),3L), " +/- ", round(sd(R2_Pearson) / sqrt(30L), 5L)," :: ",
                      "r-sq spearman xbar = ", round(mean(R2_Spearman),3L), " +/- ", round(sd(R2_Spearman) / sqrt(30L), 5L)))
  CopulaPlot <- Output[["CopulaPlot"]]
  CopulaPlot <- CopulaPlot + ggplot2::labs(
    title = paste0("Empirical Copula Plot: Actual vs Predicted"),
    subtitle = paste0("r-sq pearson xbar = ", round(mean(R2_Pearson),3L), " +/- ", round(sd(R2_Pearson) / sqrt(30L), 5L)," :: ",
                      "r-sq spearman xbar = ", round(mean(R2_Spearman),3L), " +/- ", round(sd(R2_Spearman) / sqrt(30L), 5L)))

  # Return
  return(list(ResidualsHistogram = ResidualsHistogram,
              ResidualTime = ResidualTime,
              ScatterPlot = ScatterPlot,
              CopulaPlot = CopulaPlot))
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
  ROC_Plot <- eval(ggplot2::ggplot(AUC_Data, ggplot2::aes(x = 1 - Specificity)) +
    ggplot2::geom_line(ggplot2::aes(y = AUC_Data[["Sensitivity"]]), color = "blue") +
    ggplot2::geom_abline(slope = 1, color = "black") +
    ggplot2::ggtitle(paste0("Catboost AUC: ", 100 * round(AUC_Metrics$auc, 3), "%")) +
    ChartTheme() + ggplot2::xlab("Specificity") +
    ggplot2::ylab("Sensitivity"))

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
  if(Type != "h2o") {
    p1 <- eval(
      ggplot2::ggplot(VI_Data[seq_len(min(TopN,.N))], ggplot2::aes(x = reorder(Variable, abs(Importance)), y = Importance, fill = Importance)) +
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::scale_fill_gradient2(mid = ColorLow, high = ColorHigh) +
        ChartTheme(Size = 12L, AngleX = 0L, LegendPosition = "right") +
        ggplot2::coord_flip() +
        ggplot2::labs(title = "Global Variable Importance") +
        ggplot2::xlab("Top Model Features") +
        ggplot2::ylab("Value") +
        ggplot2::theme(legend.position = "none"))
    return(p1)
  }

  # H2O
  if(Type == "h2o") {
    p1 <- eval(
      ggplot2::ggplot(VI_Data[seq_len(min(TopN,.N))], ggplot2::aes(x = reorder(Variable, ScaledImportance ), y = ScaledImportance , fill = ScaledImportance )) +
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::scale_fill_gradient2(mid = ColorLow,high = ColorHigh) +
        ChartTheme(Size = 12L, AngleX = 0L, LegendPosition = "right") +
        ggplot2::coord_flip() +
        ggplot2::labs(title = "Global Variable Importance") +
        ggplot2::xlab("Top Model Features") +
        ggplot2::ylab("Value") +
        ggplot2::theme(legend.position = "none"))
    return(p1)
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
      ggplot2::guides(fill = "none") +
      ggplot2::scale_colour_continuous(breaks = c(0, seq(10, 100, 10))) +
      ChartTheme() + ggplot2::theme(legend.position = "none"))

  # Create Lift Chart
  p_lift <- eval(ggplot2::ggplot(
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
      ggplot2::guides(fill = "none") +
      ggplot2::scale_colour_continuous(guide = FALSE) +
      ggplot2::scale_x_continuous(breaks = c(0, seq(10,100,10))) +
      ChartTheme() + ggplot2::theme(legend.position = "none"))

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
#' @param DataType 'train'
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
#' @param DateColumnName. Regression Passthrough
#'
#' @noRd
ML_EvalPlots <- function(ModelType = "classification",
                         DataType = 'train',
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
                         predict. = predict,
                         DateColumnName. = NULL,
                         TargetLevel = NULL) {

  # Variable Importance Managment
  if(!data.table::is.data.table(VariableImportance.)) {
    VarImp <- VariableImportance.[[1L]]
  } else {
    VarImp <- VariableImportance.
  }

  # Helper
  ID <- paste0(ModelID.,"_", DataType, "_")
  if(tolower(ModelType) == 'multiclass') {
    PNG <- paste0('_', TargetLevel, '.png')
    Rdata <- paste0('_', TargetLevel, '.Rdata')
    ModelType <- 'classification'
  } else {
    PNG <- paste0('.png')
    Rdata <- paste0('.Rdata')
  }

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
          ggplot2::ggsave(file.path(metadata_path., paste0(ID, "EvaluationPlot", PNG)))
        } else {
          ggplot2::ggsave(file.path(model_path., paste0(ID, "EvaluationPlot", PNG)))
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
        for(i in seq_len(min(length(FeatureColNames.), NumOfParDepPlots., VarImp[,.N]))) {
          tryCatch({
            Out <- ParDepCalPlots(
              data = ValidationData.,
              PredictionColName = "p1",
              TargetColName = eval(TargetColumnName.),
              IndepVar = if("Variable" %in% names(VarImp)) gsub("\\..*","", VarImp[i, Variable]) else gsub("\\..*","", VarImp[i, Variable]),
              GraphType = "calibration",
              PercentileBucket = 0.05,
              FactLevels = 10L,
              Function = function(x) mean(x, na.rm = TRUE))
            j <- j + 1L
            if("Variable" %in% names(VarImp)) ParDepPlots[[paste0(VarImp[j, Variable])]] <- Out else ParDepPlots[[paste0(VarImp[j, Variable])]] <- Out
          }, error = function(x) "skip")
        }
      }
      if(SaveModelObjects.) {
        if(!is.null(metadata_path.)) {
          if(!is.null(VarImp)) save(ParDepPlots, file = file.path(metadata_path., paste0(ID, "ParDepPlots", Rdata)))
        } else {
          if(!is.null(VarImp)) save(ParDepPlots, file = file.path(model_path., paste0(ID, "ParDepPlots", Rdata)))
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

    # Regression
    if(!TrainOnFull. && ((!is.null(LossFunction.) && LossFunction. != "MultiRMSE") || (!is.null(EvalMetric.) && EvalMetric. != "MultiRMSE"))) {

      # Eval Plots
      EvaluationPlot <- EvalPlot(data=ValidationData., PredictionColName="Predict", TargetColName=eval(TargetColumnName.), GraphType="calibration", PercentileBucket=0.05, aggrfun=function(x) mean(x, na.rm = TRUE))

      # Add Number of Trees to Title
      if(!TrainOnFull.) EvaluationPlot <- EvaluationPlot + ggplot2::ggtitle(paste0("Calibration Evaluation Plot: R2 = ", round(EvaluationMetrics.[[1L]][Metric == "R2", MetricValue], 3L)))

      # Save plot to file
      if(!TrainOnFull. && SaveModelObjects.) {
        if(!is.null(metadata_path.)) {
          ggplot2::ggsave(file.path(metadata_path., paste0(ID, "EvaluationPlot", PNG)))
        } else {
          ggplot2::ggsave(file.path(model_path., paste0(ID, "EvaluationPlot", PNG)))
        }
      }

      # Regression Evaluation Calibration BoxPlot
      EvaluationBoxPlot <- EvalPlot(data=ValidationData., PredictionColName="Predict", TargetColName=eval(TargetColumnName.), GraphType="boxplot", PercentileBucket=0.05, aggrfun=function(x) mean(x, na.rm = TRUE))

      # Residual Analysis
      Output <- ResidualPlots(TestData=ValidationData., Target=TargetColumnName., Predicted="Predict", DateColumnName=DateColumnName., Gam_Fit=FALSE)
      ResidualsHistogram <- Output$ResidualsHistogram
      ResidualTime <- Output$ResidualTime
      ScatterPlot <- Output$ScatterPlot
      CopulaPlot <- Output$CopulaPlot; rm(Output)

      # Add Number of Trees to Title
      if(!TrainOnFull.) EvaluationBoxPlot <- EvaluationBoxPlot + ggplot2::ggtitle(paste0("Calibration Evaluation Plot: R2 = ", round(EvaluationMetrics.[[1L]][Metric == "R2", MetricValue], 3L)))

      # Save plot to file
      if(SaveModelObjects.) {
        if(!is.null(metadata_path.)) {
          ggplot2::ggsave(file.path(metadata_path., paste0(ID, "EvaluationBoxPlot", PNG)))
        } else {
          ggplot2::ggsave(file.path(model_path., paste0(ID, "EvaluationBoxPlot", PNG)))
        }
      }

      # Regression Partial Dependence
      if(!is.null(VariableImportance.)) {
        ParDepBoxPlots <- list()
        ParDepPlots <- list()
        if(NumOfParDepPlots. > 0L) {
          j <- 0L
          k <- 0L
          for(i in seq_len(min(length(FeatureColNames.), NumOfParDepPlots., VarImp[,.N]))) {
            tryCatch({
              Out <- ParDepCalPlots(
                data = ValidationData.,
                PredictionColName = "Predict",
                TargetColName = eval(TargetColumnName.),
                IndepVar = if("Variable" %in% names(VarImp)) gsub("\\..*","", VarImp[i, Variable]) else gsub("\\..*","", VarImp[i, Variable]),
                GraphType = "calibration",
                PercentileBucket = 0.05,
                FactLevels = 10L,
                Function = function(x) mean(x, na.rm = TRUE))
              j <- j + 1L
              if("Variable" %in% names(VarImp)) ParDepPlots[[paste0(VarImp[j, Variable])]] <- Out else ParDepPlots[[paste0(VarImp[j, Variable])]] <- Out
            }, error = function(x) "skip")
            tryCatch({
              Out1 <- ParDepCalPlots(
                data = ValidationData.,
                PredictionColName = "Predict",
                TargetColName = eval(TargetColumnName.),
                IndepVar = if("Variable" %in% names(VarImp)) VarImp[i, Variable] else VarImp[i, Variable],
                GraphType = "boxplot",
                PercentileBucket = 0.05,
                FactLevels = 10L,
                Function = function(x) mean(x, na.rm = TRUE))
              k <- k + 1L
              if("Variable" %in% names(VarImp)) ParDepBoxPlots[[paste0(VarImp[k, Variable])]] <- Out1 else ParDepBoxPlots[[paste0(VarImp[k, Variable])]] <- Out1
            }, error = function(x) "skip")
          }

          # Regression Save ParDepPlots to file
          if(SaveModelObjects.) {
            if(!is.null(metadata_path.)) {
              save(ParDepPlots, file = file.path(metadata_path., paste0(ID, "ParDepPlots", Rdata)))
            } else {
              save(ParDepPlots, file = file.path(model_path., paste0(ID, "ParDepPlots", Rdata)))
            }
          }

          # Regression Save ParDepBoxPlots to file
          if(SaveModelObjects.) {
            if(!is.null(metadata_path.)) {
              save(ParDepBoxPlots, file = file.path(metadata_path., paste0(ID, "ParDepBoxPlots", Rdata)))
            } else {
              save(ParDepBoxPlots, file = file.path(model_path., paste0(ID, "ParDepBoxPlots", Rdata)))
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
      ResidualsHistogram <- list()
      ResidualTime <- list()
      ScatterPlot <- list()
      CopulaPlot <- list()

      # Build plots
      for(TV in seq_along(TargetColumnName.)) {

        # Residual Analysis
        Output <- ResidualPlots(TestData=ValidationData., Target=TargetColumnName.[TV], Predicted=paste0("Predict.V",TV), DateColumnName=DateColumnName., Gam_Fit=FALSE)
        ResidualsHistogram[[TargetColumnName.[TV]]] <- Output$ResidualsHistogram
        ResidualTime[[TargetColumnName.[TV]]] <- Output$ResidualTime
        ScatterPlot[[TargetColumnName.[TV]]] <- Output$ScatterPlot
        CopulaPlot[[TargetColumnName.[TV]]] <- Output$CopulaPlot; rm(Output)

        # Eval Plots
        EvaluationPlot[[TargetColumnName.[TV]]] <- EvalPlot(
          data = ValidationData.,
          PredictionColName = paste0("Predict.V",TV),
          TargetColName = eval(TargetColumnName.[TV]),
          GraphType = "calibration",
          PercentileBucket = 0.05,
          aggrfun = function(x) mean(x, na.rm = TRUE))

        # Add Number of Trees to Title
        if(!TrainOnFull.) EvaluationPlot[[TargetColumnName.[TV]]] <- EvaluationPlot[[TargetColumnName.[TV]]] + ggplot2::ggtitle(paste0("Calibration Evaluation Plot: R2 = ", round(EvaluationMetrics.[[1L]][[TargetColumnName.[TV]]][Metric == "R2", MetricValue], 3L)))

        # Save plot to file
        if(!TrainOnFull.) {
          if(SaveModelObjects.) {
            if(!is.null(metadata_path.)) {
              ggplot2::ggsave(file.path(metadata_path., paste0(ID, "EvaluationPlot", PNG)))
            } else {
              ggplot2::ggsave(file.path(model_path., paste0(ID, "EvaluationPlot", PNG)))
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
        if(!TrainOnFull.) EvaluationBoxPlot[[TargetColumnName.[TV]]] <- EvaluationBoxPlot[[TargetColumnName.[TV]]] + ggplot2::ggtitle(paste0("Calibration Evaluation Plot: R2 = ", round(EvaluationMetrics.[[1L]][[TargetColumnName.[TV]]][Metric == "R2", MetricValue], 3L)))

        # Save plot to file
        if(SaveModelObjects.) {
          if(!is.null(metadata_path.)) {
            ggplot2::ggsave(file.path(metadata_path., paste0(ID, "EvaluationBoxPlot", PNG)))
          } else {
            ggplot2::ggsave(file.path(model_path., paste0(ID, "EvaluationBoxPlot", PNG)))
          }
        }

        # Regression Partial Dependence
        if(!is.null(VariableImportance.)) {
          ParDepBoxPlots <- list()
          ParDepPlots <- list()
          if(NumOfParDepPlots. > 0L) {
            j <- 0L
            k <- 0L
            for(i in seq_len(min(length(FeatureColNames.), NumOfParDepPlots., VarImp[,.N]))) {
              tryCatch({
                Out <- ParDepCalPlots(
                  data = ValidationData.,
                  PredictionColName = paste0("Predict.V",TV),
                  TargetColName = eval(TargetColumnName.[TV]),
                  IndepVar = gsub("\\..*","", VarImp[i, Variable]),
                  GraphType = "calibration",
                  PercentileBucket = 0.05,
                  FactLevels = 10L,
                  Function = function(x) mean(x, na.rm = TRUE))
                j <- j + 1L
                ParDepPlots[[paste0(TargetColumnName.[TV], "_", VarImp[j, Variable])]] <- Out
              }, error = function(x) "skip")
              tryCatch({
                Out1 <- ParDepCalPlots(
                  data = ValidationData.,
                  PredictionColName = paste0("Predict.V", TV),
                  TargetColName = eval(TargetColumnName.[TV]),
                  IndepVar = gsub("\\..*","", VarImp[i, Variable]),
                  GraphType = "boxplot",
                  PercentileBucket = 0.05,
                  FactLevels = 10L,
                  Function = function(x) mean(x, na.rm = TRUE))
                k <- k + 1L
                ParDepBoxPlots[[paste0(TargetColumnName.[TV], "_", VarImp[k, Variable])]] <- Out1
              }, error = function(x) "skip")
            }

            # Regression Save ParDepPlots to file
            if(SaveModelObjects.) {
              if(!is.null(metadata_path.)) {
                save(ParDepPlots, file = file.path(metadata_path., paste0(ID, TargetColumnName.[TV], "ParDepPlots", Rdata)))
              } else {
                save(ParDepPlots, file = file.path(model_path., paste0(ID, TargetColumnName.[TV], "ParDepPlots", Rdata)))
              }
            }

            # Regression Save ParDepBoxPlots to file
            if(SaveModelObjects.) {
              if(!is.null(metadata_path.)) {
                save(ParDepBoxPlots, file = file.path(metadata_path., paste0(ID, TargetColumnName.[TV], "ParDepBoxPlots", Rdata)))
              } else {
                save(ParDepBoxPlots, file = file.path(model_path., paste0(ID, TargetColumnName.[TV], "ParDepBoxPlots", Rdata)))
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
      ResidualsHistogram <- NULL
      ResidualTime <- NULL
      ScatterPlot <- NULL
      CopulaPlot <- NULL
    }

    # Regression Return
    return(list(
      EvaluationPlot = EvaluationPlot,
      EvaluationBoxPlot = EvaluationBoxPlot,
      ParDepPlots = ParDepPlots,
      ParDepBoxPlots = ParDepBoxPlots,
      ResidualsHistogram = if(exists("ResidualsHistogram")) ResidualsHistogram else NULL,
      ResidualTime = if(exists("ResidualTime")) ResidualTime else NULL,
      ScatterPlot = if(exists("ScatterPlot")) ScatterPlot else NULL,
      CopulaPlot = if(exists("CopulaPlot")) CopulaPlot else NULL))
  }
}

#' @title ShapStep1
#'
#' @description ShapStep1 shap helper
#'
#' @family Model Evaluation and Interpretation
#'
#' @author Adrian Antico
#'
#' @param ShapData Scoring data from AutoCatBoostScoring with classification or regression
#' @param EntityID Typically something like a user id
#' @param DateColumnName Date column name in your data. Alternatively, supply an ID of sorts to distinguish scoring data from one session to another
#'
#' @noRd
ShapStep1 <- function(ShapData = NULL,
                      EntityID = NULL,
                      DateColumnName = NULL) {
  if(!is.null(EntityID)) Ent <- ShapData[[eval(EntityID)]] else Ent <- "Placeholder"
  if(!is.null(DateColumnName)) Date <- ShapData[[eval(DateColumnName)]] else Date <- Sys.Time()
  data.table::set(ShapData, j = setdiff(names(ShapData), names(ShapData)[names(ShapData) %like% "Shap_"]), value = NULL)
  ShapVals <- data.table::transpose(ShapData)
  data.table::setnames(ShapVals, "V1", "ShapValue")
  data.table::set(ShapVals, j = "Variable", value = gsub(pattern = "Shap_", replacement = "", x = names(ShapData)))
  ShapVals <- ShapVals[!Variable %like% "Diff"]
  if(!is.null(EntityID)) data.table::set(ShapVals, j = eval(EntityID), value = Ent) else data.table::set(ShapVals, j = "EntityID", value = Ent)
  if(!is.null(DateColumnName)) data.table::set(ShapVals, j = eval(DateColumnName), value = Date) else data.table::set(ShapVals, j = "Date", value = Date)
  data.table::set(ShapVals, j = "ShapValue", value = round(ShapVals[["ShapValue"]], 4L))
  data.table::set(ShapVals, j = "Variable", value = gsub(pattern = "Shap_", replacement = "", x = ShapVals[["Variable"]]))
  return(ShapVals)
}

#' @title ShapStep2
#'
#' @description ShapStep2 shap helper
#'
#' @family Model Evaluation and Interpretation
#'
#' @author Adrian Antico
#'
#' @param ShapData Scoring data from AutoCatBoostScoring with classification or regression
#' @param Step1Out Output from ShapStep1
#'
#' @noRd
ShapStep2 <- function(ShapData = NULL,
                      Step1Out = NULL) {
  Keep <- names(ShapData)[names(ShapData) %like% "Diff"]
  if(!identical(Keep, character(0))) {
    Keep <- Keep[Keep %like% "Shap_Diff_1"]
    data.table::set(ShapData, j = setdiff(names(ShapData), Keep), value = NULL)
    if(any(ColTypes(ShapData) == "character")) {
      ShapData[, (names(ShapData)) := lapply(.SD, as.character), .SDcols = c(names(ShapData))]
    }
    DiffVal <- data.table::transpose(ShapData)
    data.table::set(DiffVal, j = "Variable", value = gsub(pattern = "Diff_1", replacement = "", gsub(pattern = "Shap_Diff_1_", replacement = "", x = names(ShapData))))
    data.table::setnames(DiffVal, "V1", "ShapDiffValue")
    data.table::set(DiffVal, j = "ShapDiffValue", value = round(DiffVal[["ShapDiffValue"]], 4L))
    Step1Out[DiffVal, on = "Variable", ShapDiffValue := i.ShapDiffValue]
    return(Step1Out)
  } else {
    return(Step1Out)
  }
}

#' @title ShapStep3
#'
#' @description ShapStep3 shap helper
#'
#' @family Model Evaluation and Interpretation
#'
#' @author Adrian Antico
#'
#' @param ShapData Scoring data from AutoCatBoostScoring with classification or regression
#' @param Step2Out Output from ShapStep2
#'
#' @noRd
ShapStep3 <- function(ShapData = NULL,
                      Step2Out = NULL) {
  Keep <- names(ShapData)[!names(ShapData) %like% "Diff"]
  Keep <- Keep[Keep %like% "Shap_"]
  Keep <- gsub(pattern = "Shap_", replacement = "", x = Keep)
  CurrVals <- ShapData[, .SD, .SDcols = c(Keep)]
  if(any(ColTypes(CurrVals) == "character")) {
    CurrVals[, (names(CurrVals)) := lapply(.SD, as.character), .SDcols = c(names(CurrVals))]
  }
  CurrVal <- data.table::transpose(CurrVals)
  CurrVal[, Variable := names(CurrVals)]
  data.table::setnames(CurrVal, "V1", "CurrentValue")
  Step2Out[CurrVal, on = "Variable", CurrentValue := i.CurrentValue]

  # Diff actuals
  Keep <- names(ShapData)[names(ShapData) %like% "Diff_1"]
  if(!identical(Keep, character(0))) {
    Keep <- Keep[Keep %like% "Shap_"]
    Keep <- gsub(pattern = "Shap_", replacement = "", x = Keep)
    DiffVals <- ShapData[, .SD, .SDcols = c(Keep)]
    if(any(ColTypes(DiffVals) == "character")) {
      DiffVals[, (names(DiffVals)) := lapply(.SD, as.character), .SDcols = c(names(DiffVals))]
    }
    DiffVal <- data.table::transpose(DiffVals)
    DiffVal[, Variable := gsub(pattern = "Diff_1_", replacement = "", names(DiffVals))]
    data.table::setnames(DiffVal, "V1", "DiffValue")
    Step2Out <- merge(Step2Out, DiffVal, by = "Variable", all.x = TRUE)
    return(Step2Out)
  } else {
    return(Step2Out)
  }
}

#' @title SingleRowShapeShap
#'
#' @description SingleRowShapeShap will convert a single row of your shap data into a table
#'
#' @family Model Evaluation and Interpretation
#'
#' @author Adrian Antico
#'
#' @param ShapData Scoring data from AutoCatBoostScoring with classification or regression
#'
#' @export
SingleRowShapeShap <- function(ShapData = NULL, EntityID = NULL, DateColumnName = NULL) {
  x <- ShapStep1(ShapData = data.table::copy(ShapData), EntityID = EntityID, DateColumnName = DateColumnName)
  x <- ShapStep2(ShapData = data.table::copy(ShapData), Step1Out = x)
  x <- ShapStep3(ShapData = ShapData, Step2Out = x)
  return(x)
}

#' @title AutoShapeShap
#'
#' @description AutoShapeShap will convert your scored shap values from CatBoost
#'
#' @family Model Evaluation and Interpretation
#'
#' @author Adrian Antico
#'
#' @param ScoringData Scoring data from AutoCatBoostScoring with classification or regression
#' @param Threads Number of threads to use for the parellel routine
#' @param DateColumnName Name of the date column in scoring data
#' @param ByVariableName Name of your base entity column name
#'
#' @export
AutoShapeShap <- function(ScoringData = NULL,
                          Threads = max(1L, parallel::detectCores()-2L),
                          DateColumnName = "Date",
                          ByVariableName = "GroupVariable") {

  # NULL Corrections
  if(is.null(DateColumnName)) DateColumnName <- "Date"
  if(is.null(ByVariableName)) {
    ScoringData[, ID__ := seq_len(.N)]
    ByVariableName <- "ID__"
  }

  # Parallel env setup
  if(Threads > 1L) {
    library(parallel); library(doParallel); library(foreach)
    Packages <- c("RemixAutoML", "data.table", "parallel", "doParallel", "foreach")
    cl <- parallel::makePSOCKcluster(Threads)
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl))
    NumRowsPerThread <- floor(ScoringData[, .N] / Threads)
    Chunks <- floor(ScoringData[, .N] / NumRowsPerThread)

    # stopCluster(cl)

    # Parallel loop
    ShapValues <- foreach::foreach(
      i = itertools::isplitRows(ScoringData, chunks = Chunks),
      .combine = function(...) data.table::rbindlist(list(...)),
      .multicombine = TRUE,
      .packages = Packages
    ) %dopar% {
      Rows <- i[, .N]
      Output <- list()
      for(j in seq_len(Rows)) Output[[j]] <- SingleRowShapeShap(ShapData = i[j], EntityID = ByVariableName, DateColumnName = DateColumnName)
      out <- data.table::rbindlist(Output)
      out
    }
  } else {
    Rows <- ScoringData[, .N]
    Output <- list()
    for(j in seq_len(Rows)) Output[[j]] <- SingleRowShapeShap(ShapData = ScoringData[j], EntityID = ByVariableName, DateColumnName = DateColumnName)
    ShapValues <- data.table::rbindlist(Output)
  }

  # Add more columns
  if(any(names(ShapValues) %like% "Diff")) {
    options(warn = -1)
    if(class(ShapValues[["DiffValue"]]) %chin% c("numeric","integer")) {
      ShapValues[, PreviousValue := data.table::fcase(
        DiffValue == CurrentValue, "0",
        DiffValue != CurrentValue, as.character(as.numeric(CurrentValue) - as.numeric(DiffValue)))]
    } else {
      ShapValues[, PreviousValue := data.table::fcase(
        DiffValue %like% "New=", gsub('.*Old=', '', DiffValue),
        DiffValue == "No_Change", CurrentValue,
        DiffValue == CurrentValue, "0",
        DiffValue != CurrentValue, as.character(as.numeric(CurrentValue) - as.numeric(DiffValue)))]
    }

    ShapValues[, SumShapValue := data.table::fifelse(is.na(DiffValue), ShapValue, ShapValue + ShapDiffValue)]
    ShapValues[, AbsSumShapValue := abs(SumShapValue)]
    data.table::setcolorder(ShapValues, c(4,3,1,10,9,2,5:8))
    data.table::setorderv(ShapValues, cols = c(DateColumnName, ByVariableName, "AbsSumShapValue"), order = c(-1,1,-1))
    return(ShapValues)
  } else {
    ShapValues[, AbsShapValue := abs(ShapValue)]
    data.table::setcolorder(ShapValues, c(4,3,2,6,1,5))
    data.table::setorderv(ShapValues, cols = c(DateColumnName, ByVariableName, "AbsShapValue"), order = c(-1,1,-1))
    return(ShapValues)
  }
}
