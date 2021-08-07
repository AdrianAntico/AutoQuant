
# Collection data.table
QA_Results <- data.table::data.table(
  Type = c("SARIMA","NNET", "TBATS","ETS","ARFIMA"),
  Outcome = rep("Failure", 5))


# AutoBanditSarima() ----

# Build model
data <- RemixAutoML::FakeDataGenerator(TimeSeries = TRUE, TimeSeriesTimeAgg = "days")

# Pimping
TestModel <- tryCatch({RemixAutoML::AutoBanditSarima(
  data = data,
  FilePath = getwd(),
  ByDataType = FALSE,
  TargetVariableName = "Weekly_Sales",
  DateColumnName = "Date",
  TimeAggLevel = "days",
  EvaluationMetric = "MAE",
  NumHoldOutPeriods = 12L,
  NumFCPeriods = 16L,
  MaxLags = 10L,
  MaxSeasonalLags = 0L,
  MaxMovingAverages = 3L,
  MaxSeasonalMovingAverages = 0L,
  MaxFourierPairs = 2L,
  TrainWeighting = 0.50,
  MaxConsecutiveFails = 50L,
  MaxNumberModels = 100L,
  MaxRunTimeMinutes = 10L,
  NumberCores = 4,
  DebugMode = TRUE)}, error = function(x) NULL)

# zz <- TestModel$ForecastPlot
# qs::qsave(x = zz, file = "C:/Users/Bizon/Documents/GitHub/QA_Code/Plot")
# qs::qload(file = "C:/Users/Bizon/Documents/GitHub/QA_Code/Plot")

# Outcome
if(!is.null(TestModel)) QA_Results[1L, Outcome := "Success"]
rm(TestModel)
Sys.sleep(5)

# Debugging ----
# library(RemixAutoML)
# library(data.table)
# library(lubridate)
#
# ByDataType = FALSE
# TargetVariableName = "Weekly_Sales"
# DateColumnName = "Date"
# TimeAggLevel = "days"
# EvaluationMetric = "MAE"
# NumHoldOutPeriods = 12L
# NumFCPeriods = 16L
# MaxLags = 10L
# MaxSeasonalLags = 0L
# MaxMovingAverages = 3L
# MaxSeasonalMovingAverages = 0L
# MaxFourierPairs = 2L
# TrainWeighting = 0.50
# MaxConsecutiveFails = 50L
# MaxNumberModels = 100L
# MaxRunTimeMinutes = 10L
# NumberCores = 12
# DebugMode = TRUE
#
#
# Output
# Output$ForecastPlot
# Output$Forecast
# Output$PerformanceGrid

# AutoBanditNNet() ----

# Create fake data
data <- RemixAutoML::FakeDataGenerator(TimeSeries = TRUE, TimeSeriesTimeAgg = "days")
data <- RemixAutoML::DifferenceData(data = data, ColumnsToDiff = "Weekly_Sales", CARMA = FALSE)
data <- data$DiffData

# Build model
TestModel <- tryCatch({RemixAutoML::AutoBanditNNet(
  data = data,
  FilePath = getwd(),
  TargetVariableName = "Weekly_Sales",
  DateColumnName = "Date",
  TimeAggLevel = "day",
  EvaluationMetric = "MAE",
  NumHoldOutPeriods = 12L,
  NumFCPeriods = 5L,
  MaxLags = 3L,
  MaxSeasonalLags = 1L,
  MaxFourierPairs = 2L,
  TrainWeighting = 0.50,
  MaxConsecutiveFails = 20L,
  MaxNumberModels = 30L,
  MaxRunTimeMinutes = 10L,
  NumberCores = 4)}, error = function(x) NULL)

# Outcome
if(!is.null(TestModel)) QA_Results[2L, Outcome := "Success"]
rm(TestModel)
Sys.sleep(5)


# # Create fake data
#
# Output
# Output$ForecastPlot
# Output$Forecast
# Output$PerformanceGrid
#
# data <- RemixAutoML::FakeDataGenerator(TimeSeries = TRUE, TimeSeriesTimeAgg = "days")
#
# library(RemixAutoML)
# library(data.table)
# library(lubridate)
# TargetVariableName = "Weekly_Sales"
# DateColumnName = "Date"
# TimeAggLevel = "day"
# EvaluationMetric = "MAE"
# NumHoldOutPeriods = 5L
# NumFCPeriods = 5L
# MaxLags = 5L
# MaxSeasonalLags = 1L
# MaxFourierPairs = 2L
# TrainWeighting = 0.50
# MaxConsecutiveFails = 20
# MaxNumberModels = 30
# MaxRunTimeMinutes = 10L
# NumberCores = 4
#
# # final build nnet
# ModelOutputGrid = NNET_ExperimentGrid
# TimeSeriesPrepareOutput = NNET_Artifacts_Score
# FCPeriods = NumFCPeriods
# NumberModelsScore = 1
# MetricSelection = EvaluationMetric
# ByDataType = FALSE
#
# #opt nnet
# Output = TimeSeriesPrepareOutput
# DataSetName = TrainArtifacts[[ScoreGrid[i,1][[1]]]][["Name"]]
# train = TrainArtifacts[[ScoreGrid[i,1][[1]]]][["Data"]]
# test = TimeSeriesPrepareOutput$TestData
# Lags = TimeSeriesPrepareOutput$Lags
# SeasonalLags = TimeSeriesPrepareOutput$SeasonalLags
# FullData = TimeSeriesPrepareOutput$FullData
# HoldOutPeriods = TimeSeriesPrepareOutput$HoldOutPeriods
# MinVal = TimeSeriesPrepareOutput$MinVal
# TargetName = TimeSeriesPrepareOutput$TargetName
# DateName = TimeSeriesPrepareOutput$DateName
# MaxFourierTerms = 0
# TrainValidateShare = c(1.0,0.0,0.0)
# MaxNumberModels = NumberModelsScore
# MaxRunMinutes = 100
# FinalGrid = ScoreGrid[i]
#
# # parallel nnet
# MetricSelection = EvaluationMetric
# Output = NNET_Artifacts_Build
# MaxFourierTerms = NNET_MaxFourierTerms
# TrainValidateShare = c(NNET_TrainShareEvaluate,1 - NNET_TrainShareEvaluate)
# MaxNumberModels = NNET_MaxNumberModels
# MaxRunMinutes = NNET_MaxRunTime
# MaxRunsWithoutNewWinner = NNET_RunsWithoutWinner
#
# # Output
# Output$ForecastPlot
# Output$Forecast
# Output$PerformanceGrid

# AutoTBATS() ----

# Create fake data
data <- RemixAutoML::FakeDataGenerator(TimeSeries = TRUE, TimeSeriesTimeAgg = "days")

# Build model
TestModel <- tryCatch({RemixAutoML::AutoTBATS(
  data,
  FilePath = getwd(),
  TargetVariableName = "Weekly_Sales",
  DateColumnName = "Date",
  TimeAggLevel = "weeks",
  EvaluationMetric = "MAE",
  NumHoldOutPeriods = 5L,
  NumFCPeriods = 5L,
  MaxLags = 5L,
  MaxMovingAverages = 5L,
  MaxSeasonalPeriods = 1L,
  TrainWeighting = 0.50,
  MaxConsecutiveFails = 12L,
  MaxNumberModels = 100L,
  MaxRunTimeMinutes = 10L)}, error = function(x) NULL)

# Outcome
if(!is.null(TestModel)) QA_Results[3L, Outcome := "Success"]
rm(TestModel)
Sys.sleep(5)

# # Output
# Output$ForecastPlot
# Output$Forecast
# Output$PerformanceGrid
#
# # Create fake data
# data <- RemixAutoML::FakeDataGenerator(TimeSeries = TRUE, TimeSeriesTimeAgg = "days")
#
# library(RemixAutoML)
# library(data.table)
# library(lubridate)
# TargetVariableName = "Weekly_Sales"
# DateColumnName = "Date"
# TimeAggLevel = "weeks"
# EvaluationMetric = "MAE"
# NumHoldOutPeriods = 5L
# NumFCPeriods = 5L
# MaxLags = 5L
# MaxMovingAverages = 5L
# MaxSeasonalPeriods = 1L
# TrainWeighting = 0.50
# MaxConsecutiveFails = 12L
# MaxNumberModels = 100L
# MaxRunTimeMinutes = 10L
#
# Output$Forecast
#
#

# AutoETS() ----

# Create fake data
data <- RemixAutoML::FakeDataGenerator(TimeSeries = TRUE, TimeSeriesTimeAgg = "days")

# Build model
TestModel <- tryCatch({RemixAutoML::AutoETS(
  data,
  FilePath = getwd(),
  TargetVariableName = "Weekly_Sales",
  DateColumnName = "Date",
  TimeAggLevel = "weeks",
  EvaluationMetric = "MAE",
  NumHoldOutPeriods = 5L,
  NumFCPeriods = 5L,
  TrainWeighting = 0.50,
  MaxConsecutiveFails = 12L,
  MaxNumberModels = 100L,
  MaxRunTimeMinutes = 10L)}, error = function(x) NULL)

# Outcome
if(!is.null(TestModel)) QA_Results[4L, Outcome := "Success"]
rm(TestModel)
Sys.sleep(5)

# library(RemixAutoML)
# library(data.table)
# library(lubridate)
#
# TargetVariableName = "Weekly_Sales"
# DateColumnName = "Date"
# TimeAggLevel = "weeks"
# EvaluationMetric = "MAE"
# NumHoldOutPeriods = 5L
# NumFCPeriods = 5L
# TrainWeighting = 0.50
# MaxConsecutiveFails = 12L
# MaxNumberModels = 100L
# MaxRunTimeMinutes = 10L
# Lags = 0
# SeasonalLags = 0
# MovingAverages = 0
# SeasonalMovingAverages = 0

# AutoArfima() ----

# Create fake data
data <- RemixAutoML::FakeDataGenerator(TimeSeries = TRUE, TimeSeriesTimeAgg = "days")

# Build model
TestModel <- tryCatch({RemixAutoML::AutoArfima(
  data,
  FilePath = getwd(),
  TargetVariableName = "Weekly_Sales",
  DateColumnName = "Date",
  TimeAggLevel = "weeks",
  EvaluationMetric = "MAE",
  NumHoldOutPeriods = 5L,
  NumFCPeriods = 5L,
  MaxLags = 5L,
  MaxMovingAverages = 5L,
  TrainWeighting = 0.50,
  MaxConsecutiveFails = 12L,
  NumberCores = 4,
  MaxNumberModels = 100L,
  MaxRunTimeMinutes = 10L)}, error = function(x) NULL)

# Outcome
if(!is.null(TestModel)) QA_Results[5L, Outcome := "Success"]
rm(TestModel)
Sys.sleep(5)

# library(RemixAutoML)
# library(data.table)
# library(lubridate)
#
# TargetVariableName = "Weekly_Sales"
# DateColumnName = "Date"
# TimeAggLevel = "weeks"
# EvaluationMetric = "MAE"
# NumHoldOutPeriods = 5L
# NumFCPeriods = 5L
# MaxLags = 5L
# MaxMovingAverages = 5L
# TrainWeighting = 0.50
# MaxConsecutiveFails = 12L
# MaxNumberModels = 100L
# MaxRunTimeMinutes = 10L
# NumberCores = 4
#
# # Output
# TestModel$ForecastPlot
# TestModel$Forecast
# TestModel$PerformanceGrid








# library(RemixAutoML)
# library(data.table)
#
# data = Output$Forecast
# TargetVariable = c("Weekly_Sales","Forecast")
# DateVariable = "Date"
# GroupVariables = NULL
# VLineDate = NULL
# Aggregate = NULL
# NumberGroupsDisplay = 2
# LevelsToDisplay = NULL
# OtherGroupLabel = "Other"
# DisplayOtherGroup = FALSE
# TextSize = 12
# LineWidth = 1
# Color = "blue"
# XTickMarks = XTickMarkss
# AngleX = 35
# AngleY = 0
# ChartColor = "lightsteelblue1"
# BorderColor = "darkblue"
# TextColor = "darkblue"
# GridColor = "white"
# BackGroundColor = "gray95"
# LegendPosition = "bottom"
# LegendTextColor = "darkblue"
# LegendTextSize = 10
# ForecastLineColor = "black"
# PredictionIntervals = TRUE
# TS_ModelID = "Supercharged-SARIMA"
# PredictionIntervalColorInner = "white"
# PredictionIntervalColorOuter = "darkblue"
# EvaluationMode = FALSE
# SSForecast = TRUE
#
# RemixAutoML::TimeSeriesPlotter(
#   data = Output$Forecast,
#   TargetVariable = c("Weekly_Sales","Forecast"),
#   DateVariable = "Date",
#   GroupVariables = NULL,
#   VLineDate = NULL,
#   Aggregate = NULL,
#   NumberGroupsDisplay = 0,
#   LevelsToDisplay = NULL,
#   OtherGroupLabel = "Other",
#   DisplayOtherGroup = FALSE,
#   TextSize = 12,
#   LineWidth = 1,
#   Color = "blue",
#   XTickMarks = XTickMarkss,
#   AngleX = 35,
#   AngleY = 0,
#   ChartColor = "lightsteelblue1",
#   BorderColor = "darkblue",
#   TextColor = "darkblue",
#   GridColor = "white",
#   BackGroundColor = "gray95",
#   LegendPosition = "bottom",
#   LegendTextColor = "darkblue",
#   LegendTextSize = 10,
#   ForecastLineColor = "black",
#   PredictionIntervals = TRUE,
#   TS_ModelID = NULL,
#   PredictionIntervalColorInner = "white",
#   PredictionIntervalColorOuter = "darkblue")


