# Build model
Outputt <- data.table::data.table(Success = rep("Failure", 11), TimeAgg = rep("a", 11))
Version <- 0L
for(TimeAgg in opt$Test_TimeAggs) {

  # Increment
  Version <- Version + 1L

  # Create data
  data <- RemixAutoML::FakeDataGenerator(TimeSeries = TRUE, TimeSeriesTimeAgg = TimeAgg)

  # Run system
  Output <- tryCatch({RemixAutoML::AutoBanditSarima(
    data = data,
    ByDataType = FALSE,
    TargetVariableName = opt$TargetVariableName,
    DateColumnName = opt$DateColumnName,
    TimeAggLevel = TimeAgg,
    EvaluationMetric = "",
    NumHoldOutPeriods = 5L,
    NumFCPeriods = 5L,
    MaxLags = 5,
    MaxSeasonalLags = 1L,
    MaxMovingAverages = 5,
    MaxSeasonalMovingAverages = 0L,
    MaxFourierPairs = 2L,
    TrainWeighting = 0.50,
    MaxConsecutiveFails = 50L,
    MaxNumberModels = 100L,
    MaxRunTimeMinutes = 10L,
    NumberCores = 8,
    DebugMode = FALSE)}, error = function(x) NULL)

  # View output
  if(!is.null(Output$ForecastPlot)) {
    data.table::set(Outputt, i = Version, j = "Success", value = 1)
    data.table::set(Outputt, i = Version, j = "TimeAgg", value = TimeAgg)
  }

  # Save data
  data.table::fwrite(Outputt, file = "C:/Users/Bizon/Documents/GitHub/QA_Code/AutoBanditSarima_QA.csv")

  # Pause a bit
  Sys.sleep(8)
}

# Store results
data.table::fwrite(Outputt, file = file.path(opt$CSV_Path, "QA-AutoBanditSarima.csv"))

###################################################################################################################

# Load results
#data.table::fread(file.choose())


# Debugging: STEP THROUGH EXAMPLE

# library(RemixAutoML)
# library(lubridate)
# library(data.table)
#
# TargetVariableName = "Weekly_Sales"
# DateColumnName = "Date"
# TimeAggLevel = "1min"
# EvaluationMetric = "MAE"
# NumHoldOutPeriods = 5L
# NumFCPeriods = 5L
# MaxLags = 5L
# MaxSeasonalLags = 0L
# MaxMovingAverages = 5L
# MaxSeasonalMovingAverages = 0L
# MaxFourierPairs = 2L
# TrainWeighting = 0.50
# MaxConsecutiveFails = 50L
# MaxNumberModels = 500L
# MaxRunTimeMinutes = 30L
# NumberCores = parallel::detectCores() - 2
# DebugMode = TRUE
