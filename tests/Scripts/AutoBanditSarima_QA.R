# Build model
Outputt <- data.table::data.table(Success = rep("Failure", 11), TimeAgg = rep("a", 11))
Version <- 0L

for(i in c('month', 'week', 'day', 'hour') Test_TimeAggs) {

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
  RemixAutoML:::Post_Append_Helper(Outputt,'AutoBanditSarima_QA')
  # Pause a bit
  Sys.sleep(8)
}

# Store results
RemixAutoML:::Post_Append_Helper(Outputt,'AutoBanditSarima_QA')

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


# FROM rocker/shiny:4.0.5
#
# # Get linux up to date
# RUN apt-get update && apt-get install -y \
# --no-install-recommends \
# git-core \
# libssl-dev \
# libcurl4-gnutls-dev \
# curl \
# libsodium-dev \
# libxml2-dev \
# libicu-dev \
# && apt-get clean \
# && rm -rf /var/lib/apt/lists/*
#
#   # Something
#   ENV _R_SHLIB_STRIP_=true
#
# # It's free real estate!
# RUN echo 'Its free real estate'
#
# # Install packages
# RUN install2.r --error --skipinstalled \
# esquisse \
# shiny \
# forecast \
# jsonlite \
# ggplot2 \
# htmltools \
# plotly \
# devtools \
# arules \
# bit64 \
# combinat \
# data.table \
# doParallel \
# e1071 \
# fBasics \
# foreach \
# forecast \
# fpp \
# ggplot2 \
# gridExtra \
# itertools \
# lubridate \
# MLmetrics \
# nortest \
# RColorBrewer \
# recommenderlab \
# pROC \
# Rfast \
# scatterplot3d \
# stringr \
# timeDate \
# tsoutliers \
# xgboost \
# lightgbm \
# jsonlite \
# RCurl \
# shinydashboard \
# shinyjs \
# shinyWidgets \
# htmltools \
# AzureStor
#
# # Install github packages
# RUN R -e "devtools::install_github('catboost/catboost', subdir = 'catboost/R-package')"
# RUN R -e "devtools::install_github('AdrianAntico/prettydoc', upgrade = FALSE, dependencies = FALSE, force = TRUE)"
# RUN R -e "devtools::install_github('AdrianAntico/RemixAutoML', upgrade = FALSE, dependencies = FALSE, force = TRUE, quiet = FALSE)"
#
# # Copy in credentials for azure
# COPY AutoPlotterCreds.csv .
#
# # Run app
# CMD /bin/bash R -e "options('shiny.port'=3838,shiny.host='0.0.0.0'); library(RemixAutoML); RemixAutoML::AppsPlotting(UserName_Password_DT=data.table::data.table(UserName = c('UserID'), Password = c('Nova2046')), Debug = TRUE, PlotObjectHome = RemixAutoML:::InitializePlotObjects(4L), RunMode = 'package', AzureCredsFile = normalizePath('.'))"





