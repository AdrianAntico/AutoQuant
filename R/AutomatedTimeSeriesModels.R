#' AutoBanditSarima
#'
#' AutoBanditSarima is a multi-armed bandit model testing framework for SARIMA. Randomized probability matching is the underlying bandit algorithm. Model evaluation is done by blending the training error and the validation error from testing the model on out of sample data. The bandit algorithm compares the performance of the current build against the previous builds which starts with the classic auto.arima from the forecast package. Depending on how many lags, moving averages, seasonal lags and moving averages you test the number of combinations of features to test begins to approach 100,000 different combinations of settings. The function tests out transformations, differencing, and variations of the lags and moving averages. The paramter space is broken up into various buckets that are increasing in sophistication. The bandit algorithm samples from those buckets and based on many rounds of testing it determines which buckets to generate samples from more frequently based on the models performance coming from that bucket. All of the models have performance data collected on them and a final rebuild is initiated when a winner is found. The rebuild process begins by retraining the model with the settings that produced the best performance. If the model fails to build, for whatever reason, the next best buildable model is rebuilt.
#'
#' @author Adrian Antico
#' @family Time Series
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
#'
#' 2. BoxCox - "skip" means I didn't use it
#'
#' 3. IncludeDrift - TRUE or FALSE in forecast::Arima()
#'
#' 4. SeasonalDifferences - 0, 1, 2, ... Set to 0 by default as values > 0 can cause model runs to take significantly longer depending on the size of the data
#'
#' 5. SeasonalMovingAverages - Q in Arima(p,d,q)(P,D,Q)
#'
#' 6. SeasonalLags - P in Arima(p,d,q)(P,D,Q)
#'
#' 7. MaxFourierTerms - used in xreg argument in Arima
#'
#' 8. Differences - d in Arima(p,d,q)(P,D,Q)
#'
#' 9. MovingAverages - q in Arima(p,d,q)(P,D,Q)
#'
#' 10. Lags - p in Arima(p,d,q)(P,D,Q)
#'
#' 11. BiasAdj - TRUE if BoxCox isn't "skip"
#'
#' 12. GridName - ID for set of function arguments that are treated like hyperparameters
#'
#' 13. Train_MSE - MSE of the training data fit
#'
#' 14. Train_MAE - MAE of the training data fit
#'
#' 15. Train_MAPE - MAPE of the training data fit
#'
#' 16. Validate_MSE - MSE of the validation data fit
#'
#' 17. Validate_MAE - MAE of the validation data fit
#'
#' 18. Validate_MAPE - MAPE of the validation data fit
#'
#' 19. Blended_MSE - MSE  weighted by the TrainWeighting argument so that the Blended MSE = TrainWeighting * Train_MSE + (1 - TrainWeighting)  * Validate_MSE
#'
#' 20. Blended_MAE - like above
#'
#' 21. Blended_MAPE - like above
#'
#' # Non overlapping set of Arima arguments in order of increasing sophistication
#'
#' 22. BanditProbs_StratifyParsimonousGrid_3
#'
#' 23. BanditProbs_StratifyParsimonousGrid_4
#'
#' 24. BanditProbs_StratifyParsimonousGrid_5
#'
#' 25. BanditProbs_StratifyParsimonousGrid_6
#'
#' 26. BanditProbs_StratifyParsimonousGrid_7
#'
#' 27. BanditProbs_StratifyParsimonousGrid_8
#'
#' 28. BanditProbs_StratifyParsimonousGrid_9
#'
#' 29. BanditProbs_StratifyParsimonousGrid_10
#'
#' 30. RunTime - Time taken to build the model using the set of arguments
#'
#' 31. ModelRankByDataType - There are 4 data types: user-supplied frequency or not (2) and forecast::tsclean() or not (2)
#'
#' 32. ModelRank - the rank of the model based on the Blended_xxx measure
#'
#' 34. ModelRunNumber - The order that the model was run
#' @examples
#' \donttest{
#' # Pull in data
#' data <- data.table::as.data.table(fpp::cafe)
#' data.table::setnames(data, "x", "Weekly_Sales")
#' data.table::set(data, j = "Date", value = "1982-01-01")
#' data.table::setcolorder(data, c(2,1))
#' data[, Date := as.POSIXct(Date)]
#'
#' # "1min"
#' data[, xx := 1:.N][, Date := Date + lubridate::minutes(1 * xx)][, xx := NULL]
#'
#' # "5min"
#' #data[, xx := 1:.N][, Date := Date + lubridate::minutes(5 * xx)][, xx := NULL]
#'
#' # "10min"
#' #data[, xx := 1:.N][, Date := Date + lubridate::minutes(10 * xx)][, xx := NULL]
#'
#' # "15min"
#' #data[, xx := 1:.N][, Date := Date + lubridate::minutes(15 * xx)][, xx := NULL]
#'
#' # "30min"
#' #data[, xx := 1:.N][, Date := Date + lubridate::minutes(30 * xx)][, xx := NULL]
#'
#' # "hour"
#' #data[, xx := 1:.N][, Date := Date + lubridate::hours(xx)][, xx := NULL]
#'
#' # Build model
#' Output <- RemixAutoML::AutoBanditSarima(
#'   data = data,
#'   ByDataType = TRUE,
#'   TargetVariableName = "Weekly_Sales",
#'   DateColumnName = "Date",
#'   TimeAggLevel = "1min",
#'   EvaluationMetric = "MAE",
#'   NumHoldOutPeriods = 5L,
#'   NumFCPeriods = 5L,
#'   MaxLags = 5L,
#'   MaxSeasonalLags = 0L,
#'   MaxMovingAverages = 5L,
#'   MaxSeasonalMovingAverages = 0L,
#'   MaxFourierPairs = 2L,
#'   TrainWeighting = 0.50,
#'   MaxConsecutiveFails = 50L,
#'   MaxNumberModels = 500L,
#'   MaxRunTimeMinutes = 30L,
#'   NumberCores = max(1L, parallel::detectCores()))
#'
#'# View output
#' Output$Forecast[ModelRank == min(ModelRank)]
#' View(Output$PerformanceGrid[DataSetName == "TSCleanModelFrequency"])
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

  GlobalObjectsPreRun <- ls(envir = .GlobalEnv)

  # Check for data issues----
  x <- length(data[[eval(DateColumnName)]])
  xx <- length(unique(data[[eval(DateColumnName)]]))
  if(x != xx) return(print("Non unique values detected in data"))

  # Lags----
  if(!is.integer(MaxLags) | length(MaxLags) > 1L) {
    ARIMA_Lags <- as.integer(max(MaxLags))
  } else {
    ARIMA_Lags <- MaxLags
  }

  # Seasonal Lags----
  if(!is.integer(MaxSeasonalLags) | length(MaxSeasonalLags) > 1L) {
    ARIMA_SeasonalLags <- as.integer(max(MaxSeasonalLags))
  } else {
    ARIMA_SeasonalLags <- MaxSeasonalLags
  }

  # Moving Averages----
  if(!is.integer(MaxMovingAverages) | length(MaxMovingAverages) > 1L) {
    ARIMA_MovingAverages <- as.integer(max(MaxMovingAverages))
  } else {
    ARIMA_MovingAverages <- MaxMovingAverages
  }

  # Moving Averages----
  if(!is.integer(MaxSeasonalMovingAverages) | length(MaxSeasonalMovingAverages) > 1L) {
    ARIMA_SeasonalMovingAverages <- as.integer(max(MaxSeasonalMovingAverages))
  } else {
    ARIMA_SeasonalMovingAverages <- MaxSeasonalMovingAverages
  }

  # Fourier Pairs----
  if(!is.integer(MaxFourierPairs) | length(MaxFourierPairs) > 1L) {
    ARIMA_MaxFourierTerms <- as.integer(max(MaxFourierPairs))
  } else {
    ARIMA_MaxFourierTerms <- MaxFourierPairs
  }

  # Train Share Evaluate----
  ARIMA_TrainShareEvaluate <- TrainWeighting

  # MaxConsecutiveFails----
  if(!is.integer(MaxConsecutiveFails) | length(MaxConsecutiveFails) > 1L) {
    ARIMA_RunsWithoutWinner <- as.integer(MaxConsecutiveFails)
  } else {
    ARIMA_RunsWithoutWinner <- MaxConsecutiveFails
  }

  # MaxConsecutiveFails----
  if(!is.integer(MaxNumberModels) | length(MaxNumberModels) > 1L) {
    ARIMA_MaxNumberModels <- as.integer(MaxNumberModels)
  } else {
    ARIMA_MaxNumberModels <- MaxNumberModels
  }

  # MaxConsecutiveFails----
  if(!is.integer(MaxRunTimeMinutes) | length(MaxRunTimeMinutes) > 1L) {
    ARIMA_MaxRunTime <- as.integer(MaxRunTimeMinutes)
  } else {
    ARIMA_MaxRunTime <- MaxRunTimeMinutes
  }

  # Values----
  MinValue <- min(data[[eval(TargetVariableName)]], na.rm = TRUE)
  AvgValue <- mean(data[[eval(TargetVariableName)]], na.rm = TRUE)
  MaxValue <- max(data[[eval(TargetVariableName)]], na.rm = TRUE)

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

  # Debugging ----
  # TargetName = TargetVariableName
  # DateName = DateColumnName
  # Lags = ARIMA_Lags
  # SeasonalLags = ARIMA_SeasonalLags
  # MovingAverages = ARIMA_MovingAverages
  # SeasonalMovingAverages = ARIMA_SeasonalMovingAverages
  # TimeUnit = TimeAggLevel
  # FCPeriods = NumFCPeriods
  # HoldOutPeriods = NumHoldOutPeriods
  # TSClean = TRUE
  # ModelFreq = TRUE
  # FinalBuild = FALSE

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

  # MetricSelection = EvaluationMetric
  # Output = Arima_Artifacts_Build
  # MaxFourierTerms = ARIMA_MaxFourierTerms
  # TrainValidateShare = c(ARIMA_TrainShareEvaluate, 1-ARIMA_TrainShareEvaluate)
  # MaxNumberModels = ARIMA_MaxNumberModels
  # MaxRunMinutes = ARIMA_MaxRunTime
  # MaxRunsWithoutNewWinner = ARIMA_RunsWithoutWinner

  # Reutrn if no suitable models were fit----
  if(Arima_ExperimentGrid[1]$Train_MSE == -7) return(paste0("Unable to fit an arima to this data"))

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

      # # Debugging ----
      # ModelOutputGrid = Arima_ExperimentGrid
      # TimeSeriesPrepareOutput = Arima_Artifacts_Score
      # FCPeriods = NumFCPeriods
      # NumberModelsScore = 1
      # MetricSelection = EvaluationMetric
      # ByDataType = FALSE
      if(DebugMode) if(is.null(ForecastOutput)) for(kk in 1:10) print("Forecast output is NULL") else for(kk in 1:10) print("Forecast output is NOT NULL")

      # Move on if model build failure----
      if(!is.null(ForecastOutput)) {
        FC_MaxValue <<- max(ForecastOutput[["Forecast"]], na.rm = TRUE)
        if(nrow(ForecastOutput) != 0 & ((FC_MaxValue - MaxValue) * NumFCPeriods / data[,.N]) < 10 * ((MaxValue - AvgValue))) {
          data.table::setnames(ForecastOutput, "Target", eval(TargetVariableName))
          Output <<- list(Forecast = ForecastOutput, PerformanceGrid = Arima_ExperimentGrid)
          rm(envir = .GlobalEnv, Arima_ExperimentGrid, Arima_Artifacts_Build, Arima_Artifacts_Score, FC_Data, FinalFC, FinalForecastData, ForecastOutput, Forecasts, RawOutput, Results, ReturnData,ScoreGrid, Train_Score, TrainArtifacts, TSGridList,XREG, XREGFC, counter, Counter1, FC_MaxValue, FCPeriods, lambda,RunSuccess, Successs, TrainRows)
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
            XTickMarks = "1 year",
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
          return(Output)
        } else {
          Arima_ExperimentGrid <<- Arima_ExperimentGrid[ModelRankByDataType != eval(counter)]
          counter <<- counter + 1L
          if(counter > 25) break
        }
      } else {
        counter <<- counter + 1L
        if(counter > 25) break
      }
    }
  } else {
    rm(envir = .GlobalEnv, Arima_ExperimentGrid, Arima_Artifacts_Build, Arima_Artifacts_Score, FC_Data, FinalFC, FinalForecastData, ForecastOutput, Forecasts, RawOutput, Results, ReturnData,ScoreGrid, Train_Score, TrainArtifacts, TSGridList,XREG, XREGFC, counter, Counter1, FC_MaxValue, FCPeriods, lambda,RunSuccess, Successs, TrainRows)
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
      XTickMarks = "1 year",
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
    if(!is.null(Output)) return(Output) else return(print("Unable to build arima on given data"))
  }
}

#' AutoBanditNNet
#'
#' AutoBanditNNet is a multi-armed bandit model testing framework for AR and SAR NNets. Randomized probability matching is the underlying bandit algorithm. Model evaluation is done by blending the training error and the validation error from testing the model on out of sample data. The bandit algorithm compares the performance of the current build against the previous builds which starts with the classic nnetar model from the forecast package. Depending on how many lags, seasonal lags, and fourier pairs you test the number of combinations of features to test begins to approach 10,000 different combinations of settings. The function tests out transformations, differencing, and variations of the lags, seasonal lags, and fourier pairs. The paramter space is broken up into various buckets that are increasing in sophistication. The bandit algorithm samples from those buckets and based on many rounds of testing it determines which buckets to generate samples from more frequently based on the models performance coming from that bucket. All of the models have performance data collected on them and a final rebuild is initiated when a winner is found. The rebuild process begins by retraining the model with the settings that produced the best performance. If the model fails to build, for whatever reason, the next best buildable model is rebuilt.
#'
#' @author Adrian Antico
#' @family Time Series
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

  # Check for data issues----
  x <- length(data[[eval(DateColumnName)]])
  xx <- length(unique(data[[eval(DateColumnName)]]))
  if(x != xx) return(print("Non unique values detected in data"))

  # Lags----
  if(!is.integer(MaxLags) | length(MaxLags) > 1L) {
    NNET_Lags <- as.integer(max(MaxLags))
  } else {
    NNET_Lags <- MaxLags
  }

  # Seasonal Lags----
  if(!is.integer(MaxSeasonalLags) | length(MaxSeasonalLags) > 1L) {
    NNET_SeasonalLags <- as.integer(max(MaxSeasonalLags))
  } else {
    NNET_SeasonalLags <- MaxSeasonalLags
  }

  # Fourier Pairs----
  if(!is.integer(MaxFourierPairs) | length(MaxFourierPairs) > 1L) {
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
  if(!is.integer(MaxConsecutiveFails) | length(MaxConsecutiveFails) > 1L) {
    NNET_RunsWithoutWinner <- as.integer(max(MaxConsecutiveFails))
  } else {
    NNET_RunsWithoutWinner <- MaxConsecutiveFails
  }

  # Max Number Models----
  if(!is.integer(MaxNumberModels) | length(MaxNumberModels) > 1L) {
    NNET_MaxNumberModels <- as.integer(max(MaxNumberModels))
  } else {
    NNET_MaxNumberModels <- MaxNumberModels
  }

  # Max Run Time----
  if(!is.integer(MaxRunTimeMinutes) | length(MaxRunTimeMinutes) > 1L) {
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
          if(counter > 10L) {
            return(print("Unable to build a model"))
          }
        }
      } else {
        return(print("Unable to build a model"))
      }
    }
  } else {
    return(print("Unable to build a model"))
  }
}

#' AutoTBATS
#'
#' AutoTBATS is a multi-armed bandit model testing framework for AR and SAR NNets. Randomized probability matching is the underlying bandit algorithm. Model evaluation is done by blending the training error and the validation error from testing the model on out of sample data. The bandit algorithm compares the performance of the current build against the previous builds which starts with the classic nnetar model from the forecast package. Depending on how many lags, seasonal lags, and fourier pairs you test the number of combinations of features to test begins to approach 10,000 different combinations of settings. The function tests out transformations, differencing, and variations of the lags, seasonal lags, and fourier pairs. The paramter space is broken up into various buckets that are increasing in sophistication. The bandit algorithm samples from those buckets and based on many rounds of testing it determines which buckets to generate samples from more frequently based on the models performance coming from that bucket. All of the models have performance data collected on them and a final rebuild is initiated when a winner is found. The rebuild process begins by retraining the model with the settings that produced the best performance. If the model fails to build, for whatever reason, the next best buildable model is rebuilt.
#'
#' @author Adrian Antico
#' @family Time Series
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

  # Check for data issues----
  x <- length(data[[eval(DateColumnName)]])
  xx <- length(unique(data[[eval(DateColumnName)]]))
  if(x != xx) {
    return(print("Non unique values detected in data"))
  }

  # Lags----
  if(!is.integer(MaxLags) | length(MaxLags) > 1L) {
    TBATS_Lags <- as.integer(max(MaxLags))
  } else {
    TBATS_Lags <- MaxLags
  }

  # Moving Averages----
  if(!is.integer(MaxMovingAverages) | length(MaxMovingAverages) > 1L) {
    TBATS_MovingAverages <- as.integer(max(MaxMovingAverages))
  } else {
    TBATS_MovingAverages <- MaxMovingAverages
  }

  # Moving Averages----
  if(!is.numeric(TrainWeighting) | length(TrainWeighting) > 1L) {
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
    data                   = data,
    TargetName             = TargetVariableName,
    DateName               = DateColumnName,
    Lags                   = TBATS_Lags,
    SeasonalLags           = MaxSeasonalPeriods,
    MovingAverages         = TBATS_MovingAverages,
    SeasonalMovingAverages = MaxSeasonalPeriods,
    TimeUnit               = TimeAggLevel,
    FCPeriods              = NumFCPeriods,
    HoldOutPeriods         = NumHoldOutPeriods,
    TSClean                = TRUE,
    ModelFreq              = TRUE)

  # 2. Find Best TBATS Models----
  TBATS_ExperimentGrid <- tryCatch({ParallelAutoTBATS(
    MetricSelection = EvaluationMetric,
    Output = TBATS_Artifacts_Build,
    TrainValidateShare = TBATS_TrainShareEvaluate)},
    error = function(x) NULL)

  # 3. Create Final Build Data----
  if(!is.null(TBATS_ExperimentGrid)) {
    TBATS_Artifacts_Score <- TimeSeriesDataPrepare(
      data                   = data,
      TargetName             = TargetVariableName,
      DateName               = DateColumnName,
      Lags                   = TBATS_Lags,
      SeasonalLags           = MaxSeasonalPeriods,
      MovingAverages         = TBATS_MovingAverages,
      SeasonalMovingAverages = MaxSeasonalPeriods,
      TimeUnit               = TimeAggLevel,
      FCPeriods              = NumFCPeriods,
      HoldOutPeriods         = 0,
      TSClean                = TRUE,
      ModelFreq              = TRUE,
      FinalBuild             = TRUE)

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
        return(print("Unable to build model"))
      }
    } else {
      return(print("Unable to build model"))
    }
  } else {
    return(print("Unable to build model"))
  }
}
