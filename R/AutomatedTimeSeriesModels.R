#' AutoBanditSarima
#'
#' AutoBanditSarima is a multi-armed bandit model testing framework for SARIMA. Randomized probability matching is the underlying bandit algorithm. Model evaluation is done by blending the training error and the validation error from testing the model on out of sample data. The bandit algorithm compares the performance of the current build against the previous builds which starts with the classic auto.arima from the forecast package. Depending on how many lags, moving averages, seasonal lags and moving averages you test the number of combinations of features to test begins to approach 100,000 different combinations of settings. The function tests out transformations, differencing, and variations of the lags and moving averages. The paramter space is broken up into various buckets that are increasing in sophistication. The bandit algorithm samples from those buckets and based on many rounds of testing it determines which buckets to generate samples from more frequently based on the models performance coming from that bucket. All of the models have performance data collected on them and a final rebuild is initiated when a winner is found. The rebuild process begins by retraining the model with the settings that produced the best performance. If the model fails to build, for whatever reason, the next best buildable model is rebuilt.
#'
#' @author Adrian Antico
#' @family Automated Time Series
#' @param data Source data.table
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
#' @param NumberCores Number of cores to use in parallelism. E.g. if you have a 4 core CPU then supply 4 if you want to utilize all four cores
#' @param DebugMode Set to TRUE to get print outs of particular steps helpful in tracing errors
#' @return data.table containing historical values and the forecast values along with the grid tuning results in full detail, as a second data.table
#' @examples
#' \dontrun{
#' # Build model
#' data <- RemixAutoML::FakeDataGenerator(
#'   TimeSeries = TRUE, TimeSeriesTimeAgg = "1min")
#'
#' # Pimping
#' Output <- RemixAutoML::AutoBanditSarima(
#'   data = data,
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
#'   NumberCores = 12,
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
                             NumberCores = max(1L, parallel::detectCores()),
                             DebugMode = FALSE) {

  # Debug
  if(DebugMode) print("here 1")

  # Ensure only the required columns are used ----
  data <- data.table::copy(data[, .SD, .SDcols = c(DateColumnName, TargetVariableName)])

  # Debug
  if(DebugMode) print("here 2")

  # Check for data issues ----
  x <- length(data[[eval(DateColumnName)]])
  xx <- length(unique(data[[eval(DateColumnName)]]))
  if(x != xx) return(print("Non unique values detected in data"))

  # Debug
  if(DebugMode) print("here 3")

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

  # Debug
  if(DebugMode) print("here 4")

  # Train Share Evaluate ----
  ARIMA_TrainShareEvaluate <- TrainWeighting

  # Debug
  if(DebugMode) print("here 5")

  # MaxConsecutiveFails----
  if(!is.integer(MaxConsecutiveFails) || length(MaxConsecutiveFails) > 1L) {
    ARIMA_RunsWithoutWinner <- as.integer(MaxConsecutiveFails)
  } else {
    ARIMA_RunsWithoutWinner <- MaxConsecutiveFails
  }

  # Debug
  if(DebugMode) print("here 6")

  # MaxConsecutiveFails----
  if(!is.integer(MaxNumberModels) || length(MaxNumberModels) > 1L) {
    ARIMA_MaxNumberModels <- as.integer(MaxNumberModels)
  } else {
    ARIMA_MaxNumberModels <- MaxNumberModels
  }

  # Debug
  if(DebugMode) print("here 7")

  # MaxConsecutiveFails----
  if(!is.integer(MaxRunTimeMinutes) || length(MaxRunTimeMinutes) > 1L) {
    ARIMA_MaxRunTime <- as.integer(MaxRunTimeMinutes)
  } else {
    ARIMA_MaxRunTime <- MaxRunTimeMinutes
  }

  # Debug
  if(DebugMode) print("here 8")

  # Values----
  MinValue <- min(data[[eval(TargetVariableName)]], na.rm = TRUE)
  AvgValue <- mean(data[[eval(TargetVariableName)]], na.rm = TRUE)
  MaxValue <- max(data[[eval(TargetVariableName)]], na.rm = TRUE)

  # Debug
  if(DebugMode) print("here 9")

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
  if(DebugMode) for(jj in 1:10) print(paste0("ParallelAutoARIMA() finished successfully"))

  # Reutrn if no suitable models were fit----
  if(Arima_ExperimentGrid[1]$Train_MSE == -7) stop(paste0("Unable to fit an arima to this data"))

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
            TargetVariable = c("Weekly_Sales","Forecast"),
            DateVariable = "Date",
            GroupVariables = NULL,
            VLineDate = NULL,
            Aggregate = NULL,
            NumberGroupsDisplay = 0,
            LevelsToDisplay = NULL,
            OtherGroupLabel = "Other",
            DisplayOtherGroup = FALSE,
            TextSize = 12,
            LineWidth = 1,
            Color = "blue",
            XTickMarks = XTickMarkss,
            Size = 12,
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
            ForecastLineColor = "black",
            Forecast = TRUE,
            PredictionIntervals = TRUE,
            TS_ModelID = NULL,
            PredictionIntervalColorInner = "white",
            PredictionIntervalColorOuter = "darkblue")}, error = function(x) NULL)
          Output$ErrorLagMA2x2 <- AutoBanditSarima2x2LagMA(Output)
          rm(envir = .GlobalEnv, Arima_ExperimentGrid, Arima_Artifacts_Build, Arima_Artifacts_Score, FC_Data, FinalFC, FinalForecastData, ForecastOutput, Forecasts, RawOutput, Results, ReturnData,ScoreGrid, Train_Score, TrainArtifacts, TSGridList,XREG, XREGFC, counter, Counter1, FC_MaxValue, FCPeriods, lambda,RunSuccess, Successs, TrainRows)
          return(Output)
        } else {
          Arima_ExperimentGrid <<- Arima_ExperimentGrid[ModelRankByDataType != eval(counter)]
          counter <<- counter + 1L
          if(counter > MaxConsecutiveFails) break
        }
      } else {
        counter <<- counter + 1L
        if(counter > MaxConsecutiveFails) break
      }
    }
  } else {

    # Build plot
    if(!TimeAggLevel %chin% c("day","days","dy","dys","week","weeks","wk","wks","month","months","mth","mths","quarter","quarters","year","years","yr","yrs")) XTickMarkss <- "1 day"
    if(TimeAggLevel %chin% c("day","days","dy","dys")) XTickMarkss <- "1 week"
    if(TimeAggLevel %chin% c("week","weeks","wk","wks")) XTickMarkss <- "1 month"
    if(TimeAggLevel %chin% c("month","months","mth","mths")) XTickMarkss <- "1 year"
    if(TimeAggLevel %chin% c("quarter","quarters")) XTickMarkss <- "2 year"
    if(TimeAggLevel %chin% c("year","years","yr","yrs")) XTickMarkss <- "5 year"
    Output$ForecastPlot <- tryCatch({RemixAutoML::TimeSeriesPlotter(
      data = Output$Forecast,
      TargetVariable = c("Weekly_Sales","Forecast"),
      DateVariable = "Date",
      GroupVariables = NULL,
      VLineDate = NULL,
      Aggregate = NULL,
      NumberGroupsDisplay = 0,
      LevelsToDisplay = NULL,
      OtherGroupLabel = "Other",
      DisplayOtherGroup = FALSE,
      TextSize = 12,
      LineWidth = 1,
      Color = "blue",
      XTickMarks = XTickMarkss,
      Size = 12,
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
      ForecastLineColor = "black",
      Forecast = TRUE,
      PredictionIntervals = TRUE,
      TS_ModelID = NULL,
      PredictionIntervalColorInner = "white",
      PredictionIntervalColorOuter = "darkblue")}, error = function(x) NULL)
    Output$ErrorLagMA2x2 <- AutoBanditSarima2x2LagMA(Output)
    rm(envir = .GlobalEnv, Arima_ExperimentGrid, Arima_Artifacts_Build, Arima_Artifacts_Score, FC_Data, FinalFC, FinalForecastData, ForecastOutput, Forecasts, RawOutput, Results, ReturnData,ScoreGrid, Train_Score, TrainArtifacts, TSGridList,XREG, XREGFC, counter, Counter1, FC_MaxValue, FCPeriods, lambda,RunSuccess, Successs, TrainRows)
    if(!is.null(Output)) return(Output) else stop(print("Unable to build arima on given data"))
  }
}

#' AutoBanditNNet
#'
#' AutoBanditNNet is a multi-armed bandit model testing framework for AR and SAR NNets. Randomized probability matching is the underlying bandit algorithm. Model evaluation is done by blending the training error and the validation error from testing the model on out of sample data. The bandit algorithm compares the performance of the current build against the previous builds which starts with the classic nnetar model from the forecast package. Depending on how many lags, seasonal lags, and fourier pairs you test the number of combinations of features to test begins to approach 10,000 different combinations of settings. The function tests out transformations, differencing, and variations of the lags, seasonal lags, and fourier pairs. The paramter space is broken up into various buckets that are increasing in sophistication. The bandit algorithm samples from those buckets and based on many rounds of testing it determines which buckets to generate samples from more frequently based on the models performance coming from that bucket. All of the models have performance data collected on them and a final rebuild is initiated when a winner is found. The rebuild process begins by retraining the model with the settings that produced the best performance. If the model fails to build, for whatever reason, the next best buildable model is rebuilt.
#'
#' @author Adrian Antico
#' @family Automated Time Series
#' @param data Source data.table
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
#' @param MaxRunTimeMinutes Indicate the maximum number of minutes to wait for a result.
#' @export
AutoBanditNNet <- function(data,
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
                           MaxRunTimeMinutes = 10L) {

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
    repeat {
      Forecast <- tryCatch({FinalBuildNNET(
        ModelOutputGrid = NNET_ExperimentGrid,
        TimeSeriesPrepareOutput = NNET_Artifacts_Score,
        FCPeriods = NumFCPeriods,
        NumberModelsScore = 1,
        MetricSelection = EvaluationMetric,
        ByDataType = TRUE)},
        error = function(x) NULL)

      # Ensure final models get build and correct grid metric is utilized----
      if(!is.null(Forecast)) {

        # Min, Average, Max values
        FC_MinValue <- min(Forecast[["Forecast"]], na.rm = TRUE)
        FC_AvgValue <- mean(Forecast[["Forecast"]], na.rm = TRUE)
        FC_MaxValue <- max(Forecast[["Forecast"]], na.rm = TRUE)

        # Build model
        if(nrow(Forecast) != 0L & ((FC_MaxValue - MaxValue) * NumFCPeriods / data[,.N]) < 10 * ((MaxValue - AvgValue))) {
          return(list(Forecast = Forecast, PerformanceGrid = NNET_ExperimentGrid))
        } else {
          NNET_ExperimentGrid <- NNET_ExperimentGrid[ModelRankByDataType != eval(counter)]
          counter <- counter + 1L
          if(counter > MaxConsecutiveFails) {
            stop(print("Unable to build a model"))
          }
        }
      } else {
        stop(print("Unable to build a model"))
      }
    }
  } else {
    stop(print("Unable to build a model"))
  }
}

#' AutoTBATS
#'
#' AutoTBATS is a multi-armed bandit model testing framework for AR and SAR NNets. Randomized probability matching is the underlying bandit algorithm. Model evaluation is done by blending the training error and the validation error from testing the model on out of sample data. The bandit algorithm compares the performance of the current build against the previous builds which starts with the classic nnetar model from the forecast package. Depending on how many lags, seasonal lags, and fourier pairs you test the number of combinations of features to test begins to approach 10,000 different combinations of settings. The function tests out transformations, differencing, and variations of the lags, seasonal lags, and fourier pairs. The paramter space is broken up into various buckets that are increasing in sophistication. The bandit algorithm samples from those buckets and based on many rounds of testing it determines which buckets to generate samples from more frequently based on the models performance coming from that bucket. All of the models have performance data collected on them and a final rebuild is initiated when a winner is found. The rebuild process begins by retraining the model with the settings that produced the best performance. If the model fails to build, for whatever reason, the next best buildable model is rebuilt.
#'
#' @author Adrian Antico
#' @family Automated Time Series
#' @param data Source data.table
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
#' @export
AutoTBATS <- function(data,
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
                      MaxRunTimeMinutes = 10L) {

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
    Forecast <- tryCatch({FinalBuildTBATS(
      ModelOutputGrid = TBATS_ExperimentGrid,
      TimeSeriesPrepareOutput = TBATS_Artifacts_Score,
      FCPeriods = NumFCPeriods,
      NumberModelsScore = 1,
      MetricSelection = EvaluationMetric,
      ByDataType = TRUE)},
      error = function(x) NULL)

    # Check whether to move on----
    if(!is.null(Forecast)) {

      # Min, Average, Max values
      FC_MinValue <- min(Forecast[["Forecast"]], na.rm = TRUE)
      FC_AvgValue <- mean(Forecast[["Forecast"]], na.rm = TRUE)
      FC_MaxValue <- max(Forecast[["Forecast"]], na.rm = TRUE)

      # Ensure final models get build and correct grid metric is utilized----
      if(nrow(Forecast) != 0 & ((FC_MaxValue - MaxValue) * NumFCPeriods / data[,.N]) < 10 * ((MaxValue - AvgValue))) {
        return(list(Forecast = Forecast, PerformanceGrid = TBATS_ExperimentGrid))
      } else {
        stop(print("Unable to build model"))
      }
    } else {
      stop(print("Unable to build model"))
    }
  } else {
    stop(print("Unable to build model"))
  }
}
