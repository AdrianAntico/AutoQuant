#' AutoBanditSarima
#'
#' AutoBanditSarima is a multi-armed bandit model testing framework for SARIMA. Randomized probability matching is the underlying bandit algorithm. Model evaluation is done by blending the training error and the validation error from testing the model on out of sample data. The bandit algorithm compares the performance of the current build against the previous builds which starts with the classic auto.arima from the forecast package. Depending on how many lags, moving averages, seasonal lags and moving averages you test the number of combinations of features to test begins to approach 100,000 different combinations of settings. The function tests out transformations, differencing, and variations of the lags and moving averages. The paramter space is broken up into various buckets that are increasing in sophistication. The bandit algorithm samples from those buckets and based on many rounds of testing it determines which buckets to generate samples from more frequently based on the models performance coming from that bucket. All of the models have performance data collected on them and a final rebuild is initiated when a winner is found. The rebuild process begins by retraining the model with the settings that produced the best performance. If the model fails to build, for whatever reason, the next best buildable model is rebuilt.
#'
#' @author Adrian Antico
#' @family Automated Time Series
#' @param data Source data.table
#' @param FilePath NULL to return nothing. Provide a file path to save the model and xregs if available
#' @param ByDataType TRUE returns the best model from the four base sets of possible models. FALSE returns the best model.
#' @param TargetVariableName Name of your time series target variable
#' @param DateColumnName Name of your date column
#' @param TimeAggLevel Choose from "year", "quarter", "month", "week", "day", "hour"
#' @param EvaluationMetric Choose from MAE, MSE, and MAPE
#' @param MaxLags A single value of the max number of lags to test
#' @param MaxSeasonalLags A single value of the max number of seasonal lags to test
#' @param MaxMovingAverages A single value of the max number of moving averages to test
#' @param MaxSeasonalMovingAverages A single value of the max number of seasonal moving averages to test
#' @param MaxFourierPairs A single value of the max number of fourier pairs to test
#' @param NumHoldOutPeriods Number of time periods to use in the out of sample testing
#' @param NumFCPeriods Number of periods to forecast
#' @param TrainWeighting Model ranking is based on a weighted average of training metrics and out of sample metrics. Supply the weight of the training metrics, such as 0.50 for 50 percent.
#' @param MaxConsecutiveFails When a new best model is found MaxConsecutiveFails resets to zero. Indicated the number of model attemps without a new winner before terminating the procedure.
#' @param MaxNumberModels Indicate the maximum number of models to test.
#' @param MaxRunTimeMinutes Indicate the maximum number of minutes to wait for a result.
#' @param NumberCores Default max(1L, min(4L, parallel::detectCores()-2L))
#' @param DebugMode Set to TRUE to get print outs of particular steps helpful in tracing errors
#' @return data.table containing historical values and the forecast values along with the grid tuning results in full detail, as a second data.table
#' @examples
#' \dontrun{
#' # Create fake data
#' data <- RemixAutoML::FakeDataGenerator(TimeSeries = TRUE, TimeSeriesTimeAgg = "days")
#'
#' # Build models
#' Output <- RemixAutoML::AutoBanditSarima(
#'   data = data,
#'   FilePath = NULL,
#'   ByDataType = FALSE,
#'   TargetVariableName = "Weekly_Sales",
#'   DateColumnName = "Date",
#'   TimeAggLevel = "1min",
#'   EvaluationMetric = "MAE",
#'   NumHoldOutPeriods = 12L,
#'   NumFCPeriods = 16L,
#'   MaxLags = 10L,
#'   MaxSeasonalLags = 0L,
#'   MaxMovingAverages = 3L,
#'   MaxSeasonalMovingAverages = 0L,
#'   MaxFourierPairs = 2L,
#'   TrainWeighting = 0.50,
#'   MaxConsecutiveFails = 50L,
#'   MaxNumberModels = 100L,
#'   MaxRunTimeMinutes = 10L,
#'   NumberCores Default max(1L, min(4L, parallel::detectCores()-2L)),
#'   DebugMode = FALSE)
#'
#' # Output
#' Output$ForecastPlot
#' Output$Forecast
#' Output$PerformanceGrid
#' Output$ErrorLagMA2x2
#' }
#' @export
AutoBanditSarima <- function(data,
                             FilePath = NULL,
                             ByDataType = TRUE,
                             TargetVariableName,
                             DateColumnName,
                             TimeAggLevel = "week",
                             EvaluationMetric = "MAE",
                             NumHoldOutPeriods = 5L,
                             NumFCPeriods = 5L,
                             MaxLags = 5L,
                             MaxSeasonalLags = 0L,
                             MaxMovingAverages = 5L,
                             MaxSeasonalMovingAverages = 0L,
                             MaxFourierPairs = 2L,
                             TrainWeighting = 0.50,
                             MaxConsecutiveFails = 25L,
                             MaxNumberModels = 100L,
                             MaxRunTimeMinutes = 10L,
                             NumberCores = max(1L, min(4L, parallel::detectCores()-2L)),
                             DebugMode = FALSE) {

  # Ensure only the required columns are used ----
  data <- data.table::copy(data[, .SD, .SDcols = c(DateColumnName, TargetVariableName)])

  # Check for data issues ----
  x <- length(data[[eval(DateColumnName)]])
  xx <- length(unique(data[[eval(DateColumnName)]]))
  if(x != xx) stop(print("Non unique values detected in data"))

  # Lags----
  if(!is.integer(MaxLags) || length(MaxLags) > 1L) {
    ARIMA_Lags <- as.integer(max(MaxLags))
  } else {
    ARIMA_Lags <- MaxLags
  }

  # Seasonal Lags----
  if(!is.integer(MaxSeasonalLags) || length(MaxSeasonalLags) > 1L) {
    ARIMA_SeasonalLags <- as.integer(max(MaxSeasonalLags))
  } else {
    ARIMA_SeasonalLags <- MaxSeasonalLags
  }

  # Moving Averages----
  if(!is.integer(MaxMovingAverages) || length(MaxMovingAverages) > 1L) {
    ARIMA_MovingAverages <- as.integer(max(MaxMovingAverages))
  } else {
    ARIMA_MovingAverages <- MaxMovingAverages
  }

  # Moving Averages----
  if(!is.integer(MaxSeasonalMovingAverages) || length(MaxSeasonalMovingAverages) > 1L) {
    ARIMA_SeasonalMovingAverages <- as.integer(max(MaxSeasonalMovingAverages))
  } else {
    ARIMA_SeasonalMovingAverages <- MaxSeasonalMovingAverages
  }

  # Fourier Pairs----
  if(!is.integer(MaxFourierPairs) || length(MaxFourierPairs) > 1L) {
    ARIMA_MaxFourierTerms <- as.integer(max(MaxFourierPairs))
  } else {
    ARIMA_MaxFourierTerms <- MaxFourierPairs
  }

  # Train Share Evaluate ----
  ARIMA_TrainShareEvaluate <- TrainWeighting

  # MaxConsecutiveFails----
  if(!is.integer(MaxConsecutiveFails) || length(MaxConsecutiveFails) > 1L) {
    ARIMA_RunsWithoutWinner <- as.integer(MaxConsecutiveFails)
  } else {
    ARIMA_RunsWithoutWinner <- MaxConsecutiveFails
  }

  # MaxConsecutiveFails----
  if(!is.integer(MaxNumberModels) || length(MaxNumberModels) > 1L) {
    ARIMA_MaxNumberModels <- as.integer(MaxNumberModels)
  } else {
    ARIMA_MaxNumberModels <- MaxNumberModels
  }

  # MaxConsecutiveFails----
  if(!is.integer(MaxRunTimeMinutes) || length(MaxRunTimeMinutes) > 1L) {
    ARIMA_MaxRunTime <- as.integer(MaxRunTimeMinutes)
  } else {
    ARIMA_MaxRunTime <- MaxRunTimeMinutes
  }

  # Values----
  MinValue <- min(data[[eval(TargetVariableName)]], na.rm = TRUE)
  AvgValue <- mean(data[[eval(TargetVariableName)]], na.rm = TRUE)
  MaxValue <- max(data[[eval(TargetVariableName)]], na.rm = TRUE)

  # Debug
  if(DebugMode) print("here")

  # 1. Create time series artifacts----
  Arima_Artifacts_Build <- TimeSeriesDataPrepare(
    data = data,
    TargetName = TargetVariableName,
    DateName = DateColumnName,
    Lags = ARIMA_Lags,
    SeasonalLags = ARIMA_SeasonalLags,
    MovingAverages = ARIMA_MovingAverages,
    SeasonalMovingAverages = ARIMA_SeasonalMovingAverages,
    TimeUnit = TimeAggLevel,
    FCPeriods = NumFCPeriods,
    HoldOutPeriods = NumHoldOutPeriods,
    TSClean = TRUE,
    ModelFreq = TRUE,
    FinalBuild = FALSE)

  # 2. Find Best ARIMA Models----
  Arima_ExperimentGrid <- tryCatch({ParallelAutoARIMA(
    MetricSelection = EvaluationMetric,
    Output = Arima_Artifacts_Build,
    MaxFourierTerms = ARIMA_MaxFourierTerms,
    TrainValidateShare = c(ARIMA_TrainShareEvaluate, 1-ARIMA_TrainShareEvaluate),
    MaxNumberModels = ARIMA_MaxNumberModels,
    MaxRunMinutes = ARIMA_MaxRunTime,
    MaxRunsWithoutNewWinner = ARIMA_RunsWithoutWinner,
    NumCores = NumberCores)}, error = function(x) NULL)

  # Debugging ----
  if(DebugMode && !is.null(Arima_ExperimentGrid)) {
    for(jj in 1:10) print(paste0("ParallelAutoARIMA() finished successfully"))
  } else if(DebugMode) {
    print(Arima_ExperimentGrid)
  }

  # Reutrn if no suitable models were fit----
  if(Arima_ExperimentGrid[1L]$Train_MSE == -7 || is.null(Arima_ExperimentGrid)) stop(paste0("Unable to fit an arima to this data"))

  # 3. Create Final Build Data----
  if(!is.null(Arima_ExperimentGrid)) {
    Arima_Artifacts_Score <<- TimeSeriesDataPrepare(
      data = data,
      TargetName = TargetVariableName,
      DateName = DateColumnName,
      Lags = ARIMA_Lags,
      SeasonalLags = ARIMA_SeasonalLags,
      MovingAverages = ARIMA_MovingAverages,
      SeasonalMovingAverages = ARIMA_SeasonalMovingAverages,
      TimeUnit = TimeAggLevel,
      FCPeriods = NumFCPeriods,
      HoldOutPeriods = 0L,
      TSClean = TRUE,
      ModelFreq = TRUE,
      FinalBuild = TRUE)

    # Final Build - returns NULLs which is a bitch to troubleshoot
    counter <<- 1L
    repeat {
      ForecastOutput <<- tryCatch({FinalBuildArima(
        SavePath = FilePath,
        ModelOutputGrid = Arima_ExperimentGrid,
        TimeSeriesPrepareOutput = Arima_Artifacts_Score,
        FCPeriods = NumFCPeriods,
        NumberModelsScore = 1,
        MetricSelection = EvaluationMetric,
        ByDataType = FALSE,
        DebugMode = DebugMode)},
        error = function(x) NULL)
      if(DebugMode) if(is.null(ForecastOutput)) for(kk in 1:10) print("Forecast output is NULL") else for(kk in 1:10) print("Forecast output is NOT NULL")

      # Move on if model build failure----
      if(!is.null(ForecastOutput)) {
        FC_MaxValue <<- max(ForecastOutput[["Forecast"]], na.rm = TRUE)
        if(nrow(ForecastOutput) != 0 & ((FC_MaxValue - MaxValue) * NumFCPeriods / data[,.N]) < 10 * ((MaxValue - AvgValue))) {
          data.table::setnames(ForecastOutput, "Target", eval(TargetVariableName))
          Output <<- list(Forecast = ForecastOutput, PerformanceGrid = Arima_ExperimentGrid)
          if(!TimeAggLevel %chin% c("day","days","dy","dys","week","weeks","wk","wks","month","months","mth","mths","quarter","quarters","year","years","yr","yrs")) XTickMarkss <- "1 day"
          if(TimeAggLevel %chin% c("day","days","dy","dys")) XTickMarkss <- "1 week"
          if(TimeAggLevel %chin% c("week","weeks","wk","wks")) XTickMarkss <- "1 month"
          if(TimeAggLevel %chin% c("month","months","mth","mths")) XTickMarkss <- "1 year"
          if(TimeAggLevel %chin% c("quarter","quarters")) XTickMarkss <- "2 year"
          if(TimeAggLevel %chin% c("year","years","yr","yrs")) XTickMarkss <- "5 year"
          Output$ForecastPlot <- tryCatch({RemixAutoML::TimeSeriesPlotter(
            data = Output$Forecast,
            TargetVariable = c(TargetVariableName,"Forecast"),
            DateVariable = DateColumnName,
            GroupVariables = NULL,
            VLineDate = NULL,
            Aggregate = NULL,
            NumberGroupsDisplay = 2,
            LevelsToDisplay = NULL,
            OtherGroupLabel = "Other",
            DisplayOtherGroup = FALSE,
            TextSize = 12,
            LineWidth = 1,
            Color = "blue",
            XTickMarks = XTickMarkss,
            AngleX = 35,
            AngleY = 0,
            ChartColor = "lightsteelblue1",
            BorderColor = "darkblue",
            TextColor = "darkblue",
            GridColor = "white",
            BackGroundColor = "gray95",
            LegendPosition = "bottom",
            LegendTextColor = "darkblue",
            LegendTextSize = 10,
            EvaluationMode = FALSE,
            SSForecast = TRUE,
            ForecastLineColor = "black",
            PredictionIntervals = TRUE,
            TS_ModelID = "Supercharged-SARIMA",
            PredictionIntervalColorInner = "white",
            PredictionIntervalColorOuter = "darkblue")}, error = function(x) NULL)

          # Forecast plot
          if(!is.null(Output$ForecastPlot)) {
            Output$ForecastPlot <- Output$ForecastPlot + ggplot2::labs(
              title = paste0("Winning Model: SARIMA(",Arima_ExperimentGrid$Lags[[1L]],",",Arima_ExperimentGrid$Differences[[1L]],",",Arima_ExperimentGrid$MovingAverages[[1L]],")(",Arima_ExperimentGrid$SeasonalLags[[1L]],",",Arima_ExperimentGrid$SeasonalDifferences[[1L]],",",Arima_ExperimentGrid$SeasonalMovingAverages[[1L]],")"),
              subtitle = paste0(
                "MAPE = ", round(100 * Arima_ExperimentGrid$Validate_MAPE[[1L]],2L),"%",
                " :: MAE = ",round(Arima_ExperimentGrid$Validate_MAE[[1L]],1L),
                " :: RMSE = ", round(sqrt(Arima_ExperimentGrid$Validate_MSE[[1L]]),1L)))
          }

          # Lag plot
          Output$ErrorLagMA2x2 <- AutoBanditSarima2x2LagMA(Output)
          rm(envir = .GlobalEnv, Arima_ExperimentGrid, Arima_Artifacts_Build, Arima_Artifacts_Score, FC_Data, FinalFC, FinalForecastData, ForecastOutput, Forecasts, RawOutput, Results, ReturnData,ScoreGrid, Train_Score, TrainArtifacts, TSGridList,XREG, XREGFC, counter, Counter1, FC_MaxValue, FCPeriods, lambda,RunSuccess, Successs, TrainRows)
          return(Output)
        } else {
          Arima_ExperimentGrid <<- Arima_ExperimentGrid[ModelRankByDataType != eval(counter)]
          counter <<- counter + 1L
          if(counter > MaxConsecutiveFails) stop("Unable to build model")
        }
      } else {
        counter <<- counter + 1L
        if(counter > MaxConsecutiveFails) stop("Unable to build model")
      }
    }
  } else {
    stop("Model unable to build")
  }
}

#' AutoBanditNNet
#'
#' AutoBanditNNet is a multi-armed bandit model testing framework for AR and SAR NNets. Randomized probability matching is the underlying bandit algorithm. Model evaluation is done by blending the training error and the validation error from testing the model on out of sample data. The bandit algorithm compares the performance of the current build against the previous builds which starts with the classic nnetar model from the forecast package. Depending on how many lags, seasonal lags, and fourier pairs you test the number of combinations of features to test begins to approach 10,000 different combinations of settings. The function tests out transformations, differencing, and variations of the lags, seasonal lags, and fourier pairs. The paramter space is broken up into various buckets that are increasing in sophistication. The bandit algorithm samples from those buckets and based on many rounds of testing it determines which buckets to generate samples from more frequently based on the models performance coming from that bucket. All of the models have performance data collected on them and a final rebuild is initiated when a winner is found. The rebuild process begins by retraining the model with the settings that produced the best performance. If the model fails to build, for whatever reason, the next best buildable model is rebuilt.
#'
#' @author Adrian Antico
#' @family Automated Time Series
#' @param data Source data.table
#' @param FilePath NULL to return nothing. Provide a file path to save the model and xregs if available
#' @param TargetVariableName Name of your time series target variable
#' @param DateColumnName Name of your date column
#' @param TimeAggLevel Choose from "year", "quarter", "month", "week", "day", "hour"
#' @param EvaluationMetric Choose from MAE, MSE, and MAPE
#' @param MaxLags A single value of the max number of lags to test
#' @param MaxSeasonalLags A single value of the max number of seasonal lags to test
#' @param MaxFourierPairs A single value of the max number of fourier pairs to test
#' @param NumHoldOutPeriods Number of time periods to use in the out of sample testing
#' @param NumFCPeriods Number of periods to forecast
#' @param TrainWeighting Model ranking is based on a weighted average of training metrics and out of sample metrics. Supply the weight of the training metrics, such as 0.50 for 50 percent.
#' @param MaxConsecutiveFails When a new best model is found MaxConsecutiveFails resets to zero. Indicated the number of model attemps without a new winner before terminating the procedure.
#' @param MaxNumberModels Indicate the maximum number of models to test.
#' @param MaxRunTimeMinutes Indicate the maximum number of minutes to wait for a result
#' @param NumberCores Default max(1L, min(4L, parallel::detectCores()-2L))
#' @param Debug Set to TRUE to print some steps
#' @examples
#' \dontrun{
#' # Create fake data
#' data <- RemixAutoML::FakeDataGenerator(TimeSeries = TRUE, TimeSeriesTimeAgg = "days")
#'
#' # Build models
#' Output <- RemixAutoML::AutoBanditNNet(
#'   data = data,
#'   FilePath = NULL,
#'   TargetVariableName = "Weekly_Sales",
#'   DateColumnName = "Date",
#'   TimeAggLevel = "day",
#'   EvaluationMetric = "MAE",
#'   NumHoldOutPeriods = 5L,
#'   NumFCPeriods = 5L,
#'   MaxLags = 5L,
#'   MaxSeasonalLags = 1L,
#'   MaxFourierPairs = 2L,
#'   TrainWeighting = 0.50,
#'   MaxConsecutiveFails = 12L,
#'   MaxNumberModels = 100L,
#'   MaxRunTimeMinutes = 10L,
#'   NumberCores = max(1L, min(4L, parallel::detectCores()-2L)),
#'   Debug = FALSE)
#'
#' # Output
#' Output$ForecastPlot
#' Output$Forecast
#' Output$PerformanceGrid
#' }
#' @export
AutoBanditNNet <- function(data,
                           FilePath = NULL,
                           TargetVariableName,
                           DateColumnName,
                           TimeAggLevel = "week",
                           EvaluationMetric = "MAE",
                           NumHoldOutPeriods = 5L,
                           NumFCPeriods = 5L,
                           MaxLags = 5L,
                           MaxSeasonalLags = 1L,
                           MaxFourierPairs = 2L,
                           TrainWeighting = 0.50,
                           MaxConsecutiveFails = 12L,
                           MaxNumberModels = 100L,
                           MaxRunTimeMinutes = 10L,
                           NumberCores = max(1L, min(4L, parallel::detectCores()-2L)),
                           Debug = FALSE) {

  # Ensure only the required columns are used ----
  data <- data.table::copy(data[, .SD, .SDcols = c(DateColumnName, TargetVariableName)])

  # Check for data issues----
  x <- length(data[[eval(DateColumnName)]])
  xx <- length(unique(data[[eval(DateColumnName)]]))
  if(x != xx) return(print("Non unique values detected in data"))

  # Lags----
  if(!is.integer(MaxLags) || length(MaxLags) > 1L) {
    NNET_Lags <- as.integer(max(MaxLags))
  } else {
    NNET_Lags <- MaxLags
  }

  # Seasonal Lags----
  if(!is.integer(MaxSeasonalLags) || length(MaxSeasonalLags) > 1L) {
    NNET_SeasonalLags <- as.integer(max(MaxSeasonalLags))
  } else {
    NNET_SeasonalLags <- MaxSeasonalLags
  }

  # Fourier Pairs----
  if(!is.integer(MaxFourierPairs) || length(MaxFourierPairs) > 1L) {
    NNET_MaxFourierTerms <- as.integer(max(MaxFourierPairs))
  } else {
    NNET_MaxFourierTerms <- MaxFourierPairs
  }

  # Train Share----
  if(!is.numeric(TrainWeighting)) {
    NNET_TrainShareEvaluate <- is.numeric(TrainWeighting)
  } else {
    NNET_TrainShareEvaluate <- TrainWeighting
  }

  # Runs without winner----
  if(!is.integer(MaxConsecutiveFails) || length(MaxConsecutiveFails) > 1L) {
    NNET_RunsWithoutWinner <- as.integer(max(MaxConsecutiveFails))
  } else {
    NNET_RunsWithoutWinner <- MaxConsecutiveFails
  }

  # Max Number Models----
  if(!is.integer(MaxNumberModels) || length(MaxNumberModels) > 1L) {
    NNET_MaxNumberModels <- as.integer(max(MaxNumberModels))
  } else {
    NNET_MaxNumberModels <- MaxNumberModels
  }

  # Max Run Time----
  if(!is.integer(MaxRunTimeMinutes) || length(MaxRunTimeMinutes) > 1L) {
    NNET_MaxRunTime <- as.integer(max(MaxRunTimeMinutes))
  } else {
    NNET_MaxRunTime <- MaxRunTimeMinutes
  }

  # Values----
  MinValue <- min(data[[eval(TargetVariableName)]], na.rm = TRUE)
  AvgValue <- mean(data[[eval(TargetVariableName)]], na.rm = TRUE)
  MaxValue <- max(data[[eval(TargetVariableName)]], na.rm = TRUE)

  # 1. Create time series artifacts----
  NNET_Artifacts_Build <- TimeSeriesDataPrepare(
    data = data,
    TargetName = TargetVariableName,
    DateName = DateColumnName,
    FinalBuild = FALSE,
    Lags = NNET_Lags,
    SeasonalLags = NNET_SeasonalLags,
    MovingAverages = 1,
    SeasonalMovingAverages = 1,
    TimeUnit = TimeAggLevel,
    FCPeriods = NumFCPeriods,
    HoldOutPeriods = NumHoldOutPeriods,
    TSClean = TRUE,
    ModelFreq = TRUE)

  # 2. Find Best NNET Models----
  NNET_ExperimentGrid <- tryCatch({ParallelAutoNNET(
    MetricSelection = EvaluationMetric,
    Output = NNET_Artifacts_Build,
    MaxFourierTerms = NNET_MaxFourierTerms,
    TrainValidateShare = c(NNET_TrainShareEvaluate,1 - NNET_TrainShareEvaluate),
    MaxNumberModels = NNET_MaxNumberModels,
    MaxRunMinutes = NNET_MaxRunTime,
    NumCores = NumberCores,
    MaxRunsWithoutNewWinner = NNET_RunsWithoutWinner)},
    error = function(x) NULL)

  # 3. Create Final Build Data----
  if(!is.null(NNET_ExperimentGrid)) {
    NNET_Artifacts_Score <- TimeSeriesDataPrepare(
      data = data,
      TargetName = TargetVariableName,
      DateName = DateColumnName,
      Lags = NNET_Lags,
      SeasonalLags = NNET_SeasonalLags,
      MovingAverages = 1,
      SeasonalMovingAverages = 1,
      TimeUnit = TimeAggLevel,
      FCPeriods = NumFCPeriods,
      HoldOutPeriods = 0,
      TSClean = TRUE,
      ModelFreq = TRUE,
      FinalBuild = TRUE)

    # 4. Generate Final NNET Forecasts----
    counter <- 1L
    repeat{
      if(Debug) for(zzz in 1:100) print(counter)
      ForecastOutput <- tryCatch({FinalBuildNNET(
        ModelOutputGrid = NNET_ExperimentGrid,
        SavePath = FilePath,
        TimeSeriesPrepareOutput = NNET_Artifacts_Score,
        FCPeriods = NumFCPeriods,
        NumberModelsScore = 1,
        MetricSelection = EvaluationMetric,
        ByDataType = FALSE)},
        error = function(x) NULL)

      # Move on if model build failure----
      if(!is.null(ForecastOutput)) {
        if(Debug) for(zzz in 1:100) print("ForecastOutput is not null")
        FC_MaxValue <<- max(ForecastOutput[["Forecast"]], na.rm = TRUE)
        if(nrow(ForecastOutput) != 0 && ((FC_MaxValue - MaxValue) * NumFCPeriods / data[,.N]) < 10 * ((MaxValue - AvgValue))) {
          data.table::setnames(ForecastOutput, "Target", eval(TargetVariableName))
          Output <<- list(Forecast = ForecastOutput, PerformanceGrid = NNET_ExperimentGrid)
          if(!TimeAggLevel %chin% c("day","days","dy","dys","week","weeks","wk","wks","month","months","mth","mths","quarter","quarters","year","years","yr","yrs")) XTickMarkss <- "1 day"
          if(TimeAggLevel %chin% c("day","days","dy","dys")) XTickMarkss <- "1 week"
          if(TimeAggLevel %chin% c("week","weeks","wk","wks")) XTickMarkss <- "1 month"
          if(TimeAggLevel %chin% c("month","months","mth","mths")) XTickMarkss <- "1 year"
          if(TimeAggLevel %chin% c("quarter","quarters")) XTickMarkss <- "2 year"
          if(TimeAggLevel %chin% c("year","years","yr","yrs")) XTickMarkss <- "5 year"
          Output$ForecastPlot <- tryCatch({RemixAutoML::TimeSeriesPlotter(
            data = Output$Forecast,
            TargetVariable = c(TargetVariableName,"Forecast"),
            DateVariable = DateColumnName,
            GroupVariables = NULL,
            VLineDate = NULL,
            Aggregate = NULL,
            NumberGroupsDisplay = 2,
            LevelsToDisplay = NULL,
            OtherGroupLabel = "Other",
            DisplayOtherGroup = FALSE,
            TextSize = 12,
            LineWidth = 1,
            Color = "blue",
            XTickMarks = XTickMarkss,
            AngleX = 35,
            AngleY = 0,
            ChartColor = "lightsteelblue1",
            BorderColor = "darkblue",
            TextColor = "darkblue",
            GridColor = "white",
            BackGroundColor = "gray95",
            LegendPosition = "bottom",
            LegendTextColor = "darkblue",
            LegendTextSize = 10,
            EvaluationMode = FALSE,
            SSForecast = TRUE,
            ForecastLineColor = "black",
            PredictionIntervals = if(any(!is.na(Output$Forecast$Low95))) TRUE else FALSE,
            TS_ModelID = "Supercharged-NNET",
            PredictionIntervalColorInner = "white",
            PredictionIntervalColorOuter = "darkblue")}, error = function(x) NULL)

          # Add headers to forecast plot
          if(!is.null(Output$ForecastPlot)) {
            Output$ForecastPlot <- Output$ForecastPlot + ggplot2::labs(
              title = paste0("Winning Model: SARIMA(",NNET_ExperimentGrid$Lags[[1L]],",",0,",",0,")(",NNET_ExperimentGrid$SeasonalLags[[1L]],",",0,",",0,")"),
              subtitle = paste0(
                "MAPE = ", round(100 * NNET_ExperimentGrid$Validate_MAPE[[1L]],2L),"%",
                " :: MAE = ",round(NNET_ExperimentGrid$Validate_MAE[[1L]],1L),
                " :: RMSE = ", round(sqrt(NNET_ExperimentGrid$Validate_MSE[[1L]]),1L)))
          }

          # Clear global environment
          rm(envir = .GlobalEnv, NNET_ExperimentGrid, NNET_Artifacts_Build, NNET_Artifacts_Score, FC_Data, FinalFC, FinalForecastData, ForecastOutput, Forecasts, RawOutput, Results, ReturnData,ScoreGrid, Train_Score, TrainArtifacts, TSGridList,XREG, XREGFC, counter, Counter1, FC_MaxValue, FCPeriods, lambda,RunSuccess, Successs, TrainRows)
          return(Output)
        } else {
          if(Debug) for(zzz in 1:100) print(paste0("Counter after failure", counter))
          NNET_ExperimentGrid <- NNET_ExperimentGrid[ModelRankByDataType != eval(counter)]
          counter <- counter + 1L
          if(counter > MaxConsecutiveFails) stop("Unable to build model")
        }
      } else {
        counter <- counter + 1L
        if(counter > MaxConsecutiveFails) stop("Unable to build model")
      }
    }
  } else {
    stop("Unable to build a model")
  }
}

#' AutoTBATS
#'
#' AutoTBATS is a multi-armed bandit model testing framework for AR and SAR NNets. Randomized probability matching is the underlying bandit algorithm. Model evaluation is done by blending the training error and the validation error from testing the model on out of sample data. The bandit algorithm compares the performance of the current build against the previous builds which starts with the classic nnetar model from the forecast package. Depending on how many lags, seasonal lags, and fourier pairs you test the number of combinations of features to test begins to approach 10,000 different combinations of settings. The function tests out transformations, differencing, and variations of the lags, seasonal lags, and fourier pairs. The paramter space is broken up into various buckets that are increasing in sophistication. The bandit algorithm samples from those buckets and based on many rounds of testing it determines which buckets to generate samples from more frequently based on the models performance coming from that bucket. All of the models have performance data collected on them and a final rebuild is initiated when a winner is found. The rebuild process begins by retraining the model with the settings that produced the best performance. If the model fails to build, for whatever reason, the next best buildable model is rebuilt.
#'
#' @author Adrian Antico
#' @family Automated Time Series
#' @param data Source data.table
#' @param FilePath NULL to return nothing. Provide a file path to save the model and xregs if available
#' @param TargetVariableName Name of your time series target variable
#' @param DateColumnName Name of your date column
#' @param TimeAggLevel Choose from "year", "quarter", "month", "week", "day", "hour"
#' @param EvaluationMetric Choose from MAE, MSE, and MAPE
#' @param MaxLags A single value of the max number of lags to use in the internal auto.arima of tbats
#' @param MaxMovingAverages A single value of the max number of moving averages to use in the internal auto.arima of tbats
#' @param MaxSeasonalPeriods A single value for the max allowable seasonal periods to be tested in the tbats framework
#' @param NumHoldOutPeriods Number of time periods to use in the out of sample testing
#' @param NumFCPeriods Number of periods to forecast
#' @param TrainWeighting Model ranking is based on a weighted average of training metrics and out of sample metrics. Supply the weight of the training metrics, such as 0.50 for 50 percent.
#' @param MaxConsecutiveFails When a new best model is found MaxConsecutiveFails resets to zero. Indicated the number of model attemps without a new winner before terminating the procedure.
#' @param MaxNumberModels Indicate the maximum number of models to test.
#' @param MaxRunTimeMinutes Indicate the maximum number of minutes to wait for a result.
#' @param NumberCores Default max(1L, min(4L, parallel::detectCores()-2L))
#' @examples
#' \dontrun{
#' # Create fake data
#' data <- RemixAutoML::FakeDataGenerator(TimeSeries = TRUE, TimeSeriesTimeAgg = "days")
#'
#' # Build model
#' Output <- RemixAutoML::AutoTBATS(
#'   data,
#'   FilePath = NULL,
#'   TargetVariableName = "Weekly_Sales",
#'   DateColumnName = "Date",
#'   TimeAggLevel = "weeks",
#'   EvaluationMetric = "MAE",
#'   NumHoldOutPeriods = 5L,
#'   NumFCPeriods = 5L,
#'   MaxLags = 5L,
#'   MaxMovingAverages = 5L,
#'   MaxSeasonalPeriods = 1L,
#'   TrainWeighting = 0.50,
#'   MaxConsecutiveFails = 12L,
#'   MaxNumberModels = 100L,
#'   MaxRunTimeMinutes = 10L,
#'   NumberCores = max(1L, min(4L, parallel::detectCores()-2L)))
#'
#' # Output
#' Output$ForecastPlot
#' Output$Forecast
#' Output$PerformanceGrid
#' }
#' @export
AutoTBATS <- function(data,
                      FilePath = NULL,
                      TargetVariableName,
                      DateColumnName,
                      TimeAggLevel = "week",
                      EvaluationMetric = "MAE",
                      NumHoldOutPeriods = 5L,
                      NumFCPeriods = 5L,
                      MaxLags = 5L,
                      MaxMovingAverages = 5L,
                      MaxSeasonalPeriods = 1L,
                      TrainWeighting = 0.50,
                      MaxConsecutiveFails = 12L,
                      MaxNumberModels = 100L,
                      MaxRunTimeMinutes = 10L,
                      NumberCores = max(1L, min(4L, parallel::detectCores()-2L))) {

  # Ensure only the required columns are used ----
  data <- data.table::copy(data[, .SD, .SDcols = c(DateColumnName, TargetVariableName)])

  # Check for data issues----
  x <- length(data[[eval(DateColumnName)]])
  xx <- length(unique(data[[eval(DateColumnName)]]))
  if(x != xx) stop(print("Non unique values detected in data"))

  # Lags----
  if(!is.integer(MaxLags) || length(MaxLags) > 1L) {
    TBATS_Lags <- as.integer(max(MaxLags))
  } else {
    TBATS_Lags <- MaxLags
  }

  # Moving Averages----
  if(!is.integer(MaxMovingAverages) || length(MaxMovingAverages) > 1L) {
    TBATS_MovingAverages <- as.integer(max(MaxMovingAverages))
  } else {
    TBATS_MovingAverages <- MaxMovingAverages
  }

  # Moving Averages----
  if(!is.numeric(TrainWeighting) || length(TrainWeighting) > 1L) {
    TBATS_TrainShareEvaluate <- as.numeric(TrainWeighting)
  } else {
    TBATS_TrainShareEvaluate <- TrainWeighting
  }

  # Values----
  MinValue <- min(data[[eval(TargetVariableName)]], na.rm = TRUE)
  AvgValue <- mean(data[[eval(TargetVariableName)]], na.rm = TRUE)
  MaxValue <- max(data[[eval(TargetVariableName)]], na.rm = TRUE)

  # 1. Create time series artifacts----
  TBATS_Artifacts_Build <- TimeSeriesDataPrepare(
    data = data,
    TargetName = TargetVariableName,
    DateName = DateColumnName,
    Lags = TBATS_Lags,
    SeasonalLags = MaxSeasonalPeriods,
    MovingAverages = TBATS_MovingAverages,
    SeasonalMovingAverages = MaxSeasonalPeriods,
    TimeUnit = TimeAggLevel,
    FCPeriods = NumFCPeriods,
    HoldOutPeriods = NumHoldOutPeriods,
    TSClean = TRUE,
    ModelFreq = TRUE)

  # 2. Find Best TBATS Models----
  TBATS_ExperimentGrid <- tryCatch({ParallelAutoTBATS(
    MetricSelection = EvaluationMetric,
    Output = TBATS_Artifacts_Build,
    NumCores = NumberCores,
    TrainValidateShare = TBATS_TrainShareEvaluate)},
    error = function(x) NULL)

  # 3. Create Final Build Data----
  if(!is.null(TBATS_ExperimentGrid)) {
    TBATS_Artifacts_Score <- TimeSeriesDataPrepare(
      data = data,
      TargetName = TargetVariableName,
      DateName = DateColumnName,
      Lags = TBATS_Lags,
      SeasonalLags = MaxSeasonalPeriods,
      MovingAverages = TBATS_MovingAverages,
      SeasonalMovingAverages = MaxSeasonalPeriods,
      TimeUnit = TimeAggLevel,
      FCPeriods = NumFCPeriods,
      HoldOutPeriods = 0,
      TSClean = TRUE,
      ModelFreq = TRUE,
      FinalBuild = TRUE)

    # 4. Generate Final TBATS Forecasts----
    counter <- 1L
    repeat{
      ForecastOutput <- tryCatch({FinalBuildTBATS(
        ModelOutputGrid = TBATS_ExperimentGrid,
        SavePath = FilePath,
        TimeSeriesPrepareOutput = TBATS_Artifacts_Score,
        FCPeriods = NumFCPeriods,
        NumberModelsScore = 1,
        MetricSelection = EvaluationMetric,
        DebugMode = FALSE,
        ByDataType = FALSE)},
        error = function(x) NULL)

      # Move on if model build failure----
      if(!is.null(ForecastOutput)) {
        FC_MaxValue <<- max(ForecastOutput[["Forecast"]], na.rm = TRUE)
        if(nrow(ForecastOutput) != 0 & ((FC_MaxValue - MaxValue) * NumFCPeriods / data[,.N]) < 10 * ((MaxValue - AvgValue))) {
          data.table::setnames(ForecastOutput, "Target", eval(TargetVariableName))
          Output <<- list(Forecast = ForecastOutput, PerformanceGrid = TBATS_ExperimentGrid)
          if(!TimeAggLevel %chin% c("day","days","dy","dys","week","weeks","wk","wks","month","months","mth","mths","quarter","quarters","year","years","yr","yrs")) XTickMarkss <- "1 day"
          if(TimeAggLevel %chin% c("day","days","dy","dys")) XTickMarkss <- "1 week"
          if(TimeAggLevel %chin% c("week","weeks","wk","wks")) XTickMarkss <- "1 month"
          if(TimeAggLevel %chin% c("month","months","mth","mths")) XTickMarkss <- "1 year"
          if(TimeAggLevel %chin% c("quarter","quarters")) XTickMarkss <- "2 year"
          if(TimeAggLevel %chin% c("year","years","yr","yrs")) XTickMarkss <- "5 year"
          Output$ForecastPlot <- tryCatch({RemixAutoML::TimeSeriesPlotter(
            data = Output$Forecast,
            TargetVariable = c(TargetVariableName,"Forecast"),
            DateVariable = DateColumnName,
            GroupVariables = NULL,
            VLineDate = NULL,
            Aggregate = NULL,
            NumberGroupsDisplay = 2,
            LevelsToDisplay = NULL,
            OtherGroupLabel = "Other",
            DisplayOtherGroup = FALSE,
            TextSize = 12,
            LineWidth = 1,
            Color = "blue",
            XTickMarks = XTickMarkss,
            AngleX = 35,
            AngleY = 0,
            ChartColor = "lightsteelblue1",
            BorderColor = "darkblue",
            TextColor = "darkblue",
            GridColor = "white",
            BackGroundColor = "gray95",
            LegendPosition = "bottom",
            LegendTextColor = "darkblue",
            LegendTextSize = 10,
            EvaluationMode = FALSE,
            SSForecast = TRUE,
            ForecastLineColor = "black",
            PredictionIntervals = TRUE,
            TS_ModelID = "TBATS",
            PredictionIntervalColorInner = "white",
            PredictionIntervalColorOuter = "darkblue")}, error = function(x) NULL)

          # Create forecast plot headers
          if(!is.null(Output$ForecastPlot)) {
            Output$ForecastPlot <- Output$ForecastPlot + ggplot2::labs(
              title = paste0("TBATS(",TBATS_ExperimentGrid$Lags[[1L]],",",0,",",TBATS_ExperimentGrid$MovingAverages[[1L]],") :: ARMA Errors = ", TBATS_ExperimentGrid$UseARMAErrors[[1L]]," :: Seasonal Periods = ", TBATS_ExperimentGrid$SeasonalPeriods[[1L]]," :: Transform = ",TBATS_ExperimentGrid$Lambda[[1L]]," :: Trend = ",TBATS_ExperimentGrid$Trend[[1L]]," :: Damped = ",TBATS_ExperimentGrid$Damped[[1L]]),
              subtitle = paste0(
                "MAPE = ", round(100 * TBATS_ExperimentGrid$Validate_MAPE[[1L]],2L),"%",
                " :: MAE = ",round(TBATS_ExperimentGrid$Validate_MAE[[1L]],1L),
                " :: RMSE = ", round(sqrt(TBATS_ExperimentGrid$Validate_MSE[[1L]]),1L)))
          }

          # Clear global environment
          rm(envir = .GlobalEnv, TBATS_ExperimentGrid, TBATS_Artifacts_Build, TBATS_Artifacts_Score, FC_Data, FinalFC, FinalForecastData, ForecastOutput, Forecasts, RawOutput, Results, ReturnData,ScoreGrid, Train_Score, TrainArtifacts, TSGridList,XREG, XREGFC, counter, Counter1, FC_MaxValue, FCPeriods, lambda,RunSuccess, Successs, TrainRows)
          return(Output)
        } else {
          TBATS_ExperimentGrid <<- TBATS_ExperimentGrid[ModelRankByDataType != eval(counter)]
          counter <- counter + 1L
          if(counter > MaxConsecutiveFails) stop("Unable to build model")
        }
      } else {
        counter <- counter + 1L
        if(counter > MaxConsecutiveFails) stop("Unable to build model")
      }
    }
  } else {
    stop("Unable to build model")
  }
}

#' AutoETS
#'
#' AutoETS is a multi-armed bandit model testing framework for AR and SAR NNets. Randomized probability matching is the underlying bandit algorithm. Model evaluation is done by blending the training error and the validation error from testing the model on out of sample data. The bandit algorithm compares the performance of the current build against the previous builds which starts with the classic nnetar model from the forecast package. Depending on how many lags, seasonal lags, and fourier pairs you test the number of combinations of features to test begins to approach 10,000 different combinations of settings. The function tests out transformations, differencing, and variations of the lags, seasonal lags, and fourier pairs. The paramter space is broken up into various buckets that are increasing in sophistication. The bandit algorithm samples from those buckets and based on many rounds of testing it determines which buckets to generate samples from more frequently based on the models performance coming from that bucket. All of the models have performance data collected on them and a final rebuild is initiated when a winner is found. The rebuild process begins by retraining the model with the settings that produced the best performance. If the model fails to build, for whatever reason, the next best buildable model is rebuilt.
#'
#' @author Adrian Antico
#' @family Automated Time Series
#' @param data Source data.table
#' @param FilePath NULL to return nothing. Provide a file path to save the model and xregs if available
#' @param TargetVariableName Name of your time series target variable
#' @param DateColumnName Name of your date column
#' @param TimeAggLevel Choose from "year", "quarter", "month", "week", "day", "hour"
#' @param EvaluationMetric Choose from MAE, MSE, and MAPE
#' @param NumHoldOutPeriods Number of time periods to use in the out of sample testing
#' @param NumFCPeriods Number of periods to forecast
#' @param TrainWeighting Model ranking is based on a weighted average of training metrics and out of sample metrics. Supply the weight of the training metrics, such as 0.50 for 50 percent.
#' @param MaxConsecutiveFails When a new best model is found MaxConsecutiveFails resets to zero. Indicated the number of model attemps without a new winner before terminating the procedure.
#' @param MaxNumberModels Indicate the maximum number of models to test.
#' @param MaxRunTimeMinutes Indicate the maximum number of minutes to wait for a result.
#' @param NumberCores Default max(1L, min(4L, parallel::detectCores()-2L))
#' @examples
#' \dontrun{
#' # Create fake data
#' data <- RemixAutoML::FakeDataGenerator(TimeSeries = TRUE, TimeSeriesTimeAgg = "days")
#'
#' # Build model
#' Output <- RemixAutoML::AutoETS(
#'   data,
#'   FilePath = NULL,
#'   TargetVariableName = "Weekly_Sales",
#'   DateColumnName = "Date",
#'   TimeAggLevel = "weeks",
#'   EvaluationMetric = "MAE",
#'   NumHoldOutPeriods = 5L,
#'   NumFCPeriods = 5L,
#'   TrainWeighting = 0.50,
#'   MaxConsecutiveFails = 12L,
#'   MaxNumberModels = 100L,
#'   MaxRunTimeMinutes = 10L,
#'   NumberCores = max(1L, min(4L, parallel::detectCores()-2L)))
#'
#' # Output
#' Output$ForecastPlot
#' Output$Forecast
#' Output$PerformanceGrid
#' }
#' @export
AutoETS <- function(data,
                    FilePath = NULL,
                    TargetVariableName,
                    DateColumnName,
                    TimeAggLevel = "week",
                    EvaluationMetric = "MAE",
                    NumHoldOutPeriods = 5L,
                    NumFCPeriods = 5L,
                    TrainWeighting = 0.50,
                    MaxConsecutiveFails = 12L,
                    MaxNumberModels = 100L,
                    MaxRunTimeMinutes = 10L,
                    NumberCores = max(1L, min(4L, parallel::detectCores()-2L))) {

  # Ensure only the required columns are used ----
  data <- data.table::copy(data[, .SD, .SDcols = c(DateColumnName, TargetVariableName)])

  # Check for data issues----
  x <- length(data[[eval(DateColumnName)]])
  xx <- length(unique(data[[eval(DateColumnName)]]))
  if(x != xx) stop(print("Non unique values detected in data"))

  # Moving Averages----
  if(!is.numeric(TrainWeighting) || length(TrainWeighting) > 1L) {
    ETS_TrainShareEvaluate <- as.numeric(TrainWeighting)
  } else {
    ETS_TrainShareEvaluate <- TrainWeighting
  }

  # Values----
  MinValue <- min(data[[eval(TargetVariableName)]], na.rm = TRUE)
  AvgValue <- mean(data[[eval(TargetVariableName)]], na.rm = TRUE)
  MaxValue <- max(data[[eval(TargetVariableName)]], na.rm = TRUE)

  # 1. Create time series artifacts----
  ETS_Artifacts_Build <- TimeSeriesDataPrepare(
    data = data,
    TargetName = TargetVariableName,
    DateName = DateColumnName,
    TimeUnit = TimeAggLevel,
    Lags = 0,
    SeasonalLags = 0,
    MovingAverages = 0,
    SeasonalMovingAverages = 0,
    FCPeriods = NumFCPeriods,
    HoldOutPeriods = NumHoldOutPeriods,
    TSClean = TRUE,
    ModelFreq = TRUE)

  # 2. Find Best ETS Models----
  ETS_ExperimentGrid <- tryCatch({ParallelAutoETS(
    MetricSelection = EvaluationMetric,
    Output = ETS_Artifacts_Build,
    NumCores = NumberCores,
    TrainValidateShare = ETS_TrainShareEvaluate)},
    error = function(x) NULL)

  # 3. Create Final Build Data----
  if(!is.null(ETS_ExperimentGrid)) {
    ETS_Artifacts_Score <- TimeSeriesDataPrepare(
      data = data,
      TargetName = TargetVariableName,
      DateName = DateColumnName,
      TimeUnit = TimeAggLevel,
      FCPeriods = NumFCPeriods,
      SeasonalLags = 0,
      MovingAverages = 0,
      SeasonalMovingAverages = 0,
      HoldOutPeriods = 0,
      TSClean = TRUE,
      ModelFreq = TRUE,
      FinalBuild = TRUE)

    # 4. Generate Final ETS Forecasts----
    counter <- 1L
    repeat{
      ForecastOutput <- tryCatch({FinalBuildETS(
        ModelOutputGrid = ETS_ExperimentGrid,
        SavePath = FilePath,
        TimeSeriesPrepareOutput = ETS_Artifacts_Score,
        FCPeriods = NumFCPeriods,
        NumberModelsScore = 1,
        DebugMode = FALSE,
        MetricSelection = EvaluationMetric,
        ByDataType = FALSE)},
        error = function(x) NULL)

      # Move on if model build failure----
      if(!is.null(ForecastOutput)) {
        FC_MaxValue <<- max(ForecastOutput[["Forecast"]], na.rm = TRUE)
        if(nrow(ForecastOutput) != 0 & ((FC_MaxValue - MaxValue) * NumFCPeriods / data[,.N]) < 10 * ((MaxValue - AvgValue))) {
          data.table::setnames(ForecastOutput, "Target", eval(TargetVariableName))
          Output <<- list(Forecast = ForecastOutput, PerformanceGrid = ETS_ExperimentGrid)
          if(!TimeAggLevel %chin% c("day","days","dy","dys","week","weeks","wk","wks","month","months","mth","mths","quarter","quarters","year","years","yr","yrs")) XTickMarkss <- "1 day"
          if(TimeAggLevel %chin% c("day","days","dy","dys")) XTickMarkss <- "1 week"
          if(TimeAggLevel %chin% c("week","weeks","wk","wks")) XTickMarkss <- "1 month"
          if(TimeAggLevel %chin% c("month","months","mth","mths")) XTickMarkss <- "1 year"
          if(TimeAggLevel %chin% c("quarter","quarters")) XTickMarkss <- "2 year"
          if(TimeAggLevel %chin% c("year","years","yr","yrs")) XTickMarkss <- "5 year"
          Output$ForecastPlot <- tryCatch({RemixAutoML::TimeSeriesPlotter(
            data = Output$Forecast,
            TargetVariable = c(TargetVariableName,"Forecast"),
            DateVariable = DateColumnName,
            GroupVariables = NULL,
            VLineDate = NULL,
            Aggregate = NULL,
            NumberGroupsDisplay = 2,
            LevelsToDisplay = NULL,
            OtherGroupLabel = "Other",
            DisplayOtherGroup = FALSE,
            TextSize = 12,
            LineWidth = 1,
            Color = "blue",
            XTickMarks = XTickMarkss,
            AngleX = 35,
            AngleY = 0,
            ChartColor = "lightsteelblue1",
            BorderColor = "darkblue",
            TextColor = "darkblue",
            GridColor = "white",
            BackGroundColor = "gray95",
            LegendPosition = "bottom",
            LegendTextColor = "darkblue",
            LegendTextSize = 10,
            EvaluationMode = FALSE,
            SSForecast = TRUE,
            ForecastLineColor = "black",
            PredictionIntervals = TRUE,
            TS_ModelID = "ETS",
            PredictionIntervalColorInner = "white",
            PredictionIntervalColorOuter = "darkblue")}, error = function(x) NULL)

          # Create forecast plot headers
          if(!is.null(Output$ForecastPlot)) {
            Output$ForecastPlot <- Output$ForecastPlot + ggplot2::labs(
              title = paste0("ETS(",ETS_ExperimentGrid$Lags[[1L]],",",0,",",ETS_ExperimentGrid$MovingAverages[[1L]],") :: ARMA Errors = ", ETS_ExperimentGrid$UseARMAErrors[[1L]]," :: Seasonal Periods = ", ETS_ExperimentGrid$SeasonalPeriods[[1L]]," :: Transform = ",ETS_ExperimentGrid$Lambda[[1L]]," :: Trend = ",ETS_ExperimentGrid$Trend[[1L]]," :: Damped = ",ETS_ExperimentGrid$Damped[[1L]]),
              subtitle = paste0(
                "MAPE = ", round(100 * ETS_ExperimentGrid$Validate_MAPE[[1L]],2L),"%",
                " :: MAE = ",round(ETS_ExperimentGrid$Validate_MAE[[1L]],1L),
                " :: RMSE = ", round(sqrt(ETS_ExperimentGrid$Validate_MSE[[1L]]),1L)))
          }

          # Clear global environment
          rm(envir = .GlobalEnv, ETS_ExperimentGrid, ETS_Artifacts_Build, ETS_Artifacts_Score, FC_Data, FinalFC, FinalForecastData, ForecastOutput, Forecasts, RawOutput, Results, ReturnData,ScoreGrid, Train_Score, TrainArtifacts, TSGridList,XREG, XREGFC, counter, Counter1, FC_MaxValue, FCPeriods, lambda,RunSuccess, Successs, TrainRows)
          return(Output)
        } else {
          ETS_ExperimentGrid <<- ETS_ExperimentGrid[ModelRankByDataType != eval(counter)]
          counter <- counter + 1L
          if(counter > MaxConsecutiveFails) stop("Unable to build model")
        }
      } else {
        counter <- counter + 1L
        if(counter > MaxConsecutiveFails) stop("Unable to build model")
      }
    }
  } else {
    stop("Unable to build model")
  }
}

#' AutoArfima
#'
#' AutoArfima is a multi-armed bandit model testing framework for AR and SAR NNets. Randomized probability matching is the underlying bandit algorithm. Model evaluation is done by blending the training error and the validation error from testing the model on out of sample data. The bandit algorithm compares the performance of the current build against the previous builds which starts with the classic nnetar model from the forecast package. Depending on how many lags, seasonal lags, and fourier pairs you test the number of combinations of features to test begins to approach 10,000 different combinations of settings. The function tests out transformations, differencing, and variations of the lags, seasonal lags, and fourier pairs. The paramter space is broken up into various buckets that are increasing in sophistication. The bandit algorithm samples from those buckets and based on many rounds of testing it determines which buckets to generate samples from more frequently based on the models performance coming from that bucket. All of the models have performance data collected on them and a final rebuild is initiated when a winner is found. The rebuild process begins by retraining the model with the settings that produced the best performance. If the model fails to build, for whatever reason, the next best buildable model is rebuilt.
#'
#' @author Adrian Antico
#' @family Automated Time Series
#' @param data Source data.table
#' @param FilePath NULL to return nothing. Provide a file path to save the model and xregs if available
#' @param TargetVariableName Name of your time series target variable
#' @param DateColumnName Name of your date column
#' @param TimeAggLevel Choose from "year", "quarter", "month", "week", "day", "hour"
#' @param EvaluationMetric Choose from MAE, MSE, and MAPE
#' @param MaxLags A single value of the max number of lags to use in the internal auto.arima of tbats
#' @param MaxMovingAverages A single value of the max number of moving averages to use in the internal auto.arima of arfima
#' @param NumHoldOutPeriods Number of time periods to use in the out of sample testing
#' @param NumFCPeriods Number of periods to forecast
#' @param TrainWeighting Model ranking is based on a weighted average of training metrics and out of sample metrics. Supply the weight of the training metrics, such as 0.50 for 50 percent.
#' @param MaxConsecutiveFails When a new best model is found MaxConsecutiveFails resets to zero. Indicated the number of model attemps without a new winner before terminating the procedure.
#' @param MaxNumberModels Indicate the maximum number of models to test.
#' @param MaxRunTimeMinutes Indicate the maximum number of minutes to wait for a result.
#' @param NumberCores Default max(1L, min(4L, parallel::detectCores()-2L))
#' @examples
#' \dontrun{
#' # Create fake data
#' data <- RemixAutoML::FakeDataGenerator(TimeSeries = TRUE, TimeSeriesTimeAgg = "days")
#'
#' # Build model
#' Output <- RemixAutoML::AutoArfima(
#'   data,
#'   FilePath = NULL,
#'   TargetVariableName = "Weekly_Sales",
#'   DateColumnName = "Date",
#'   TimeAggLevel = "weeks",
#'   EvaluationMetric = "MAE",
#'   NumHoldOutPeriods = 5L,
#'   NumFCPeriods = 5L,
#'   MaxLags = 5L,
#'   MaxMovingAverages = 5L,
#'   TrainWeighting = 0.50,
#'   MaxConsecutiveFails = 12L,
#'   MaxNumberModels = 100L,
#'   MaxRunTimeMinutes = 10L,
#'   NumberCores = max(1L, min(4L, parallel::detectCores()-2L)))
#'
#' # Output
#' Output$ForecastPlot
#' Output$Forecast
#' Output$PerformanceGrid
#' }
#' @export
AutoArfima <- function(data,
                       FilePath = NULL,
                       TargetVariableName,
                       DateColumnName,
                       TimeAggLevel = "week",
                       EvaluationMetric = "MAE",
                       NumHoldOutPeriods = 5L,
                       NumFCPeriods = 5L,
                       MaxLags = 5L,
                       MaxMovingAverages = 5L,
                       TrainWeighting = 0.50,
                       MaxConsecutiveFails = 12L,
                       MaxNumberModels = 100L,
                       MaxRunTimeMinutes = 10L,
                       NumberCores = max(1L, min(4L, parallel::detectCores()-2L))) {

  # Ensure only the required columns are used ----
  data <- data.table::copy(data[, .SD, .SDcols = c(DateColumnName, TargetVariableName)])

  # Check for data issues----
  x <- length(data[[eval(DateColumnName)]])
  xx <- length(unique(data[[eval(DateColumnName)]]))
  if(x != xx) stop(print("Non unique values detected in data"))

  # Lags----
  if(!is.integer(MaxLags) || length(MaxLags) > 1L) {
    Arfima_Lags <- as.integer(max(MaxLags))
  } else {
    Arfima_Lags <- MaxLags
  }

  # Moving Averages----
  if(!is.integer(MaxMovingAverages) || length(MaxMovingAverages) > 1L) {
    Arfima_MovingAverages <- as.integer(max(MaxMovingAverages))
  } else {
    Arfima_MovingAverages <- MaxMovingAverages
  }

  # Moving Averages----
  if(!is.numeric(TrainWeighting) || length(TrainWeighting) > 1L) {
    Arfima_TrainShareEvaluate <- as.numeric(TrainWeighting)
  } else {
    Arfima_TrainShareEvaluate <- TrainWeighting
  }

  # Values----
  MinValue <- min(data[[eval(TargetVariableName)]], na.rm = TRUE)
  AvgValue <- mean(data[[eval(TargetVariableName)]], na.rm = TRUE)
  MaxValue <- max(data[[eval(TargetVariableName)]], na.rm = TRUE)

  # 1. Create time series artifacts----
  Arfima_Artifacts_Build <- TimeSeriesDataPrepare(
    data = data,
    TargetName = TargetVariableName,
    DateName = DateColumnName,
    Lags = Arfima_Lags,
    SeasonalLags = 0,
    MovingAverages = Arfima_MovingAverages,
    SeasonalMovingAverages = 0,
    TimeUnit = TimeAggLevel,
    FCPeriods = NumFCPeriods,
    HoldOutPeriods = NumHoldOutPeriods,
    TSClean = TRUE,
    ModelFreq = TRUE)

  # 2. Find Best Arfima Models----
  Arfima_ExperimentGrid <- tryCatch({ParallelAutoArfima(
    MetricSelection = EvaluationMetric,
    Output = Arfima_Artifacts_Build,
    NumCores = NumberCores,
    TrainValidateShare = Arfima_TrainShareEvaluate)},
    error = function(x) NULL)

  # 3. Create Final Build Data----
  if(!is.null(Arfima_ExperimentGrid)) {
    Arfima_Artifacts_Score <- TimeSeriesDataPrepare(
      data = data,
      TargetName = TargetVariableName,
      DateName = DateColumnName,
      Lags = Arfima_Lags,
      SeasonalLags = 0,
      MovingAverages = Arfima_MovingAverages,
      SeasonalMovingAverages = 0,
      TimeUnit = TimeAggLevel,
      FCPeriods = NumFCPeriods,
      HoldOutPeriods = 0,
      TSClean = TRUE,
      ModelFreq = TRUE,
      FinalBuild = TRUE)

    # 4. Generate Final Arfima Forecasts----
    counter <- 1L
    repeat{
      ForecastOutput <- tryCatch({FinalBuildArfima(
        ModelOutputGrid = Arfima_ExperimentGrid,
        SavePath = FilePath,
        TimeSeriesPrepareOutput = Arfima_Artifacts_Score,
        FCPeriods = NumFCPeriods,
        NumberModelsScore = 1,
        MetricSelection = EvaluationMetric,
        DebugMode = FALSE,
        ByDataType = FALSE)},
        error = function(x) NULL)

      # Move on if model build failure----
      if(!is.null(ForecastOutput)) {
        FC_MaxValue <<- max(ForecastOutput[["Forecast"]], na.rm = TRUE)
        if(nrow(ForecastOutput) != 0 & ((FC_MaxValue - MaxValue) * NumFCPeriods / data[,.N]) < 10 * ((MaxValue - AvgValue))) {
          data.table::setnames(ForecastOutput, "Target", eval(TargetVariableName))
          Output <<- list(Forecast = ForecastOutput, PerformanceGrid = Arfima_ExperimentGrid)
          if(!TimeAggLevel %chin% c("day","days","dy","dys","week","weeks","wk","wks","month","months","mth","mths","quarter","quarters","year","years","yr","yrs")) XTickMarkss <- "1 day"
          if(TimeAggLevel %chin% c("day","days","dy","dys")) XTickMarkss <- "1 week"
          if(TimeAggLevel %chin% c("week","weeks","wk","wks")) XTickMarkss <- "1 month"
          if(TimeAggLevel %chin% c("month","months","mth","mths")) XTickMarkss <- "1 year"
          if(TimeAggLevel %chin% c("quarter","quarters")) XTickMarkss <- "2 year"
          if(TimeAggLevel %chin% c("year","years","yr","yrs")) XTickMarkss <- "5 year"
          Output$ForecastPlot <- tryCatch({RemixAutoML::TimeSeriesPlotter(
            data = Output$Forecast,
            TargetVariable = c(TargetVariableName,"Forecast"),
            DateVariable = DateColumnName,
            GroupVariables = NULL,
            VLineDate = NULL,
            Aggregate = NULL,
            NumberGroupsDisplay = 2,
            LevelsToDisplay = NULL,
            OtherGroupLabel = "Other",
            DisplayOtherGroup = FALSE,
            TextSize = 12,
            LineWidth = 1,
            Color = "blue",
            XTickMarks = XTickMarkss,
            AngleX = 35,
            AngleY = 0,
            ChartColor = "lightsteelblue1",
            BorderColor = "darkblue",
            TextColor = "darkblue",
            GridColor = "white",
            BackGroundColor = "gray95",
            LegendPosition = "bottom",
            LegendTextColor = "darkblue",
            LegendTextSize = 10,
            EvaluationMode = FALSE,
            SSForecast = TRUE,
            ForecastLineColor = "black",
            PredictionIntervals = TRUE,
            TS_ModelID = "ARFIMA",
            PredictionIntervalColorInner = "white",
            PredictionIntervalColorOuter = "darkblue")}, error = function(x) NULL)

          # Create forecast plot headers
          if(!is.null(Output$ForecastPlot)) {
            Output$ForecastPlot <- Output$ForecastPlot + ggplot2::labs(
              title = paste0("Arfima(",Arfima_ExperimentGrid$Lags[[1L]],",",0,",",Arfima_ExperimentGrid$MovingAverages[[1L]],") :: ARMA Errors = ", Arfima_ExperimentGrid$UseARMAErrors[[1L]]," :: Seasonal Periods = ", Arfima_ExperimentGrid$SeasonalPeriods[[1L]]," :: Transform = ",Arfima_ExperimentGrid$Lambda[[1L]]," :: Trend = ",Arfima_ExperimentGrid$Trend[[1L]]," :: Damped = ",Arfima_ExperimentGrid$Damped[[1L]]),
              subtitle = paste0(
                "MAPE = ", round(100 * Arfima_ExperimentGrid$Validate_MAPE[[1L]],2L),"%",
                " :: MAE = ",round(Arfima_ExperimentGrid$Validate_MAE[[1L]],1L),
                " :: RMSE = ", round(sqrt(Arfima_ExperimentGrid$Validate_MSE[[1L]]),1L)))
          }

          # Clear global environment
          rm(envir = .GlobalEnv, Arfima_ExperimentGrid, Arfima_Artifacts_Build, Arfima_Artifacts_Score, FC_Data, FinalFC, FinalForecastData, ForecastOutput, Forecasts, RawOutput, Results, ReturnData,ScoreGrid, Train_Score, TrainArtifacts, TSGridList,XREG, XREGFC, counter, Counter1, FC_MaxValue, FCPeriods, lambda,RunSuccess, Successs, TrainRows)
          return(Output)
        } else {
          Arfima_ExperimentGrid <<- Arfima_ExperimentGrid[ModelRankByDataType != eval(counter)]
          counter <- counter + 1L
          if(counter > MaxConsecutiveFails) stop("Unable to build model")
        }
      } else {
        counter <- counter + 1L
        if(counter > MaxConsecutiveFails) stop("Unable to build model")
      }
    }
  } else {
    stop("Unable to build model")
  }
}
