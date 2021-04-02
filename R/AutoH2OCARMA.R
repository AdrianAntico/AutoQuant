#' @title AutoH2OCARMA
#'
#' @description AutoH2OCARMA Mutlivariate Forecasting with calendar variables, Holiday counts, holiday lags, holiday moving averages, differencing, transformations, interaction-based categorical encoding using target variable and features to generate various time-based aggregated lags, moving averages, moving standard deviations, moving skewness, moving kurtosis, moving quantiles, parallelized interaction-based fourier pairs by grouping variables, and Trend Variables.
#'
#' @author Adrian Antico
#' @family Automated Panel Data Forecasting
#'
#' @param AlgoType Select from "dfr" for RandomForecast, "gbm" for gradient boosting, "glm" for generalized linear model, "automl" for H2O's AutoML algo, and "gam" for H2O's Generalized Additive Model.
#' @param ExcludeAlgos For use when AlgoType = "AutoML". Selections include "DRF","GLM","XGBoost","GBM","DeepLearning" and "Stacke-dEnsemble"
#' @param data Supply your full series data set here
#' @param TrainOnFull Set to TRUE to train on full data
#' @param TargetColumnName List the column name of your target variables column. E.g. "Target"
#' @param PDFOutputPath NULL or a path file to output PDFs to a specified folder
#' @param SaveDataPath NULL Or supply a path. Data saved will be called 'ModelID'_data.csv
#' @param WeightsColumn NULL
#' @param NonNegativePred TRUE or FALSE
#' @param RoundPreds Rounding predictions to an integer value. TRUE or FALSE. Defaults to FALSE
#' @param DateColumnName List the column name of your date column. E.g. "DateTime"
#' @param GroupVariables Defaults to NULL. Use NULL when you have a single series. Add in GroupVariables when you have a series for every level of a group or multiple groups.
#' @param HierarchGroups Vector of hierachy categorical columns.
#' @param TimeUnit List the time unit your data is aggregated by. E.g. "1min", "5min", "10min", "15min", "30min", "hour", "day", "week", "month", "quarter", "year".
#' @param TimeGroups Select time aggregations for adding various time aggregated GDL features.
#' @param FC_Periods Set the number of periods you want to have forecasts for. E.g. 52 for weekly data to forecast a year ahead
#' @param PartitionType Select "random" for random data partitioning "time" for partitioning by time frames
#' @param MaxMem Set to the maximum amount of memory you want to allow for running this function. Default is "32G".
#' @param NThreads Set to the number of threads you want to dedicate to this function.
#' @param Timer Set to FALSE to turn off the updating print statements for progress
#' @param DebugMode Defaults to FALSE. Set to TRUE to get a print statement of each high level comment in function
#' @param TargetTransformation Run AutoTransformationCreate() to find best transformation for the target variable. Tests YeoJohnson, BoxCox, and Asigh (also Asin and Logit for proportion target variables).
#' @param Methods Choose from "YeoJohnson", "BoxCox", "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", or "Logit". If more than one is selected, the one with the best normalization pearson statistic will be used. Identity is automatically selected and compared.
#' @param XREGS Additional data to use for model development and forecasting. Data needs to be a complete series which means both the historical and forward looking values over the specified forecast window needs to be supplied.
#' @param Lags Select the periods for all lag variables you want to create. E.g. c(1:5,52) or list("day" = c(1:10), "weeks" = c(1:4))
#' @param MA_Periods Select the periods for all moving average variables you want to create. E.g. c(1:5,52) or list("day" = c(2:10), "weeks" = c(2:4))
#' @param SD_Periods Select the periods for all moving standard deviation variables you want to create. E.g. c(1:5,52) or list("day" = c(2:10), "weeks" = c(2:4))
#' @param Skew_Periods Select the periods for all moving skewness variables you want to create. E.g. c(1:5,52) or list("day" = c(2:10), "weeks" = c(2:4))
#' @param Kurt_Periods Select the periods for all moving kurtosis variables you want to create. E.g. c(1:5,52) or list("day" = c(2:10), "weeks" = c(2:4))
#' @param Quantile_Periods Select the periods for all moving quantiles variables you want to create. E.g. c(1:5,52) or list("day" = c(2:10), "weeks" = c(2:4))
#' @param Quantiles_Selected Select from the following c("q5","q10","q15","q20","q25","q30","q35","q40","q45","q50","q55","q60","q65","q70","q75","q80","q85","q90","q95")
#' @param AnomalyDetection NULL for not using the service. Other, provide a list, e.g. AnomalyDetection = list("tstat_high" = 4, tstat_low = -4)
#' @param Difference Puts the I in ARIMA for single series and grouped series.
#' @param FourierTerms Set to the max number of pairs. E.g. 2 means to generate two pairs for by each group level and interations if hierarchy is enabled.
#' @param CalendarVariables NULL, or select from "second", "minute", "hour", "wday", "mday", "yday", "week", "isoweek", "month", "quarter", "year"
#' @param HolidayVariable NULL, or select from "USPublicHolidays", "EasterGroup", "ChristmasGroup", "OtherEcclesticalFeasts"
#' @param HolidayLookback Number of days in range to compute number of holidays from a given date in the data. If NULL, the number of days are computed for you.
#' @param HolidayLags Number of lags to build off of the holiday count variable.
#' @param HolidayMovingAverages Number of moving averages to build off of the holiday count variable.
#' @param TimeTrendVariable Set to TRUE to have a time trend variable added to the model. Time trend is numeric variable indicating the numeric value of each record in the time series (by group). Time trend starts at 1 for the earliest point in time and increments by one for each success time point.
#' @param DataTruncate Set to TRUE to remove records with missing values from the lags and moving average features created
#' @param ZeroPadSeries NULL to do nothing. Otherwise, set to "maxmax", "minmax", "maxmin", "minmin". See \code{\link{TimeSeriesFill}} for explanations of each type
#' @param SplitRatios E.g c(0.7,0.2,0.1) for train, validation, and test sets
#' @param EvalMetric Select from "RMSE", "MAE", "MAPE", "Poisson", "Quantile", "LogLinQuantile", "Lq", "SMAPE", "R2", "MSLE", "MedianAbsoluteError"
#' @param NumOfParDepPlots Set to zeros if you do not want any returned. Can set to a very large value and it will adjust to the max number of features if it's too high
#' @param GridTune Set to TRUE to run a grid tune
#' @param ModelCount Set the number of models to try in the grid tune
#' @param NTrees Select the number of trees you want to have built to train the model
#' @param LearnRate Default 0.10, models available include gbm
#' @param LearnRateAnnealing Default 1, models available include gbm
#' @param GridStrategy Default "Cartesian", models available include
#' @param MaxRunTimeSecs Default 60*60*24, models available include
#' @param StoppingRounds Default 10, models available include
#' @param MaxDepth Default 20, models available include drf, gbm
#' @param SampleRate Default 0.632, models available include drf, gbm
#' @param MTries Default 1, models available include drf
#' @param ColSampleRate Default 1, model available include gbm
#' @param ColSampleRatePerTree Default 1, models available include drf, gbm
#' @param ColSampleRatePerTreeLevel Default  1, models available include drf, gbm
#' @param MinRows Default 1, models available include drf, gbm
#' @param NBins Default 20, models available include drf, gbm
#' @param NBinsCats Default 1024, models available include drf, gbm
#' @param NBinsTopLevel Default 1024, models available include drf, gbm
#' @param CategoricalEncoding Default "AUTO". Choices include :  "AUTO", "Enum", "OneHotInternal", "OneHotExplicit", "Binary", "Eigen", "LabelEncoder", "Sort-ByResponse", "EnumLimited"
#' @param HistogramType Default "AUTO". Select from "AUTO", "UniformAdaptive", "Random", "QuantilesGlobal", "RoundRobin"
#' @param Distribution Model family
#' @param Link Link for model family
#' @param RandomDistribution Default NULL
#' @param RandomLink Default NULL
#' @param Solver Model optimizer
#' @param Alpha Default NULL
#' @param Lambda Default NULL
#' @param LambdaSearch Default FALSE,
#' @param NLambdas Default -1
#' @param Standardize Default TRUE
#' @param RemoveCollinearColumns Default FALSE
#' @param InterceptInclude Default TRUE
#' @param NonNegativeCoefficients Default FALSE
#' @param RandomColNumbers NULL
#' @param InteractionColNumbers NULL
#' @examples
#' \dontrun{
#'
#' # Load data
#' data <- data.table::fread("https://www.dropbox.com/s/2str3ek4f4cheqi/walmart_train.csv?dl=1")
#'
#' # Ensure series have no missing dates (also remove series with more than 25% missing values)
#' data <- RemixAutoML::TimeSeriesFill(
#'   data,
#'   DateColumnName = "Date",
#'   GroupVariables = c("Store","Dept"),
#'   TimeUnit = "weeks",
#'   FillType = "maxmax",
#'   MaxMissingPercent = 0.25,
#'   SimpleImpute = TRUE)
#'
#' # Set negative numbers to 0
#' data <- data[, Weekly_Sales := data.table::fifelse(Weekly_Sales < 0, 0, Weekly_Sales)]
#'
#' # Remove IsHoliday column
#' data[, IsHoliday := NULL]
#'
#' # Create xregs (this is the include the categorical variables instead of utilizing only the interaction of them)
#' xregs <- data[, .SD, .SDcols = c("Date", "Store", "Dept")]
#'
#' # Change data types
#' data[, ":=" (Store = as.character(Store), Dept = as.character(Dept))]
#' xregs[, ":=" (Store = as.character(Store), Dept = as.character(Dept))]
#'
#' # Build forecast
#' Results <- RemixAutoML::AutoH2OCARMA(
#'
#'   # Data Artifacts
#'   AlgoType = "drf",
#'   ExcludeAlgos = NULL,
#'   data = data,
#'   TargetColumnName = "Weekly_Sales",
#'   DateColumnName = "Date",
#'   HierarchGroups = NULL,
#'   GroupVariables = c("Dept"),
#'   TimeUnit = "week",
#'   TimeGroups = c("weeks","months"),
#'
#'   # Data Wrangling Features
#'   SplitRatios = c(1 - 10 / 138, 10 / 138),
#'   PartitionType = "random",
#'
#'   # Production args
#'   FC_Periods = 4L,
#'   TrainOnFull = FALSE,
#'   MaxMem = {gc();paste0(as.character(floor(max(32, as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE)) -32) / 1000000)),"G")},
#'   NThreads = parallel::detectCores(),
#'   PDFOutputPath = NULL,
#'   SaveDataPath = NULL,
#'   Timer = TRUE,
#'   DebugMode = TRUE,
#'
#'   # Target Transformations
#'   TargetTransformation = FALSE,
#'   Methods = c("BoxCox", "Asinh", "Asin", "Log",
#'     "LogPlus1", "Sqrt", "Logit", "YeoJohnson"),
#'   Difference = FALSE,
#'   NonNegativePred = FALSE,
#'   RoundPreds = FALSE,
#'
#'   # Calendar features
#'   CalendarVariables = c("week", "wom", "month", "quarter", "year"),
#'   HolidayVariable = c("USPublicHolidays","EasterGroup",
#'     "ChristmasGroup","OtherEcclesticalFeasts"),
#'   HolidayLookback = NULL,
#'   HolidayLags = 1:7,
#'   HolidayMovingAverages = 2:7,
#'   TimeTrendVariable = TRUE,
#'
#'   # Time series features
#'   Lags = list("weeks" = c(1:4), "months" = c(1:3)),
#'   MA_Periods = list("weeks" = c(2:8), "months" = c(6:12)),
#'   SD_Periods = NULL,
#'   Skew_Periods = NULL,
#'   Kurt_Periods = NULL,
#'   Quantile_Periods = NULL,
#'   Quantiles_Selected = NULL,
#'
#'   # Bonus Features
#'   XREGS = NULL,
#'   FourierTerms = 2L,
#'   AnomalyDetection = NULL,
#'   ZeroPadSeries = NULL,
#'   DataTruncate = FALSE,
#'
#'   # ML evaluation args
#'   EvalMetric = "RMSE",
#'   NumOfParDepPlots = 0L,
#'
#'   # ML grid tuning args
#'   GridTune = FALSE,
#'   GridStrategy = "Cartesian",
#'   ModelCount = 5,
#'   MaxRunTimeSecs = 60*60*24,
#'   StoppingRounds = 10,
#'
#'   # ML Args
#'   NTrees = 1000L,
#'   MaxDepth = 20,
#'   SampleRate = 0.632,
#'   MTries = -1,
#'   ColSampleRatePerTree = 1,
#'   ColSampleRatePerTreeLevel  = 1,
#'   MinRows = 1,
#'   NBins = 20,
#'   NBinsCats = 1024,
#'   NBinsTopLevel = 1024,
#'   HistogramType = "AUTO",
#'   CategoricalEncoding = "AUTO",
#'   RandomColNumbers = NULL,
#'   InteractionColNumbers = NULL,
#'   WeightsColumn = NULL,
#'
#'   # ML args
#'   Distribution = "gaussian",
#'   Link = "identity",
#'   RandomDistribution = NULL,
#'   RandomLink = NULL,
#'   Solver = "AUTO",
#'   Alpha = NULL,
#'   Lambda = NULL,
#'   LambdaSearch = FALSE,
#'   NLambdas = -1,
#'   Standardize = TRUE,
#'   RemoveCollinearColumns = FALSE,
#'   InterceptInclude = TRUE,
#'   NonNegativeCoefficients = FALSE)
#'
#' UpdateMetrics <-
#'   Results$ModelInformation$EvaluationMetrics[
#'     Metric == "MSE", MetricValue := sqrt(MetricValue)]
#' print(UpdateMetrics)
#'
#' # Get final number of trees actually used
#' Results$Model@model$model_summary$number_of_internal_trees
#'
#' # Inspect performance
#' Results$ModelInformation$EvaluationMetricsByGroup[order(-R2_Metric)]
#' Results$ModelInformation$EvaluationMetricsByGroup[order(MAE_Metric)]
#' Results$ModelInformation$EvaluationMetricsByGroup[order(MSE_Metric)]
#' Results$ModelInformation$EvaluationMetricsByGroup[order(MAPE_Metric)]
#' }
#' @return See examples
#' @export
AutoH2OCARMA <- function(AlgoType = "drf",
                         ExcludeAlgos = "XGBoost",
                         data,
                         TrainOnFull = FALSE,
                         TargetColumnName = "Target",
                         PDFOutputPath = NULL,
                         SaveDataPath = NULL,
                         WeightsColumn = NULL,
                         NonNegativePred = FALSE,
                         RoundPreds = FALSE,
                         DateColumnName = "DateTime",
                         GroupVariables = NULL,
                         HierarchGroups = NULL,
                         TimeUnit = "week",
                         TimeGroups = c("weeks","months"),
                         FC_Periods = 30,
                         PartitionType = "timeseries",
                         MaxMem = {gc();paste0(as.character(floor(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE)) / 1000000)),"G")},
                         NThreads = max(1, parallel::detectCores() - 2),
                         Timer = TRUE,
                         DebugMode = FALSE,
                         TargetTransformation = FALSE,
                         Methods = c("YeoJohnson", "BoxCox", "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit"),
                         XREGS = NULL,
                         Lags = c(1:5),
                         MA_Periods = c(1:5),
                         SD_Periods = NULL,
                         Skew_Periods = NULL,
                         Kurt_Periods = NULL,
                         Quantile_Periods = NULL,
                         Quantiles_Selected = NULL,
                         AnomalyDetection = NULL,
                         Difference = TRUE,
                         FourierTerms = 6,
                         CalendarVariables = c("second", "minute", "hour", "wday", "mday", "yday", "week", "wom", "isoweek", "month", "quarter", "year"),
                         HolidayVariable = c("USPublicHolidays","EasterGroup","ChristmasGroup","OtherEcclesticalFeasts"),
                         HolidayLookback = NULL,
                         HolidayLags = 1,
                         HolidayMovingAverages = 1:2,
                         TimeTrendVariable = FALSE,
                         DataTruncate = FALSE,
                         ZeroPadSeries = NULL,
                         SplitRatios = c(0.7, 0.2, 0.1),
                         EvalMetric = "rmse",
                         NumOfParDepPlots = 0L,
                         GridTune = FALSE,
                         ModelCount = 1,
                         NTrees = 1000,
                         LearnRate = 0.10,
                         LearnRateAnnealing = 1,
                         GridStrategy = "Cartesian",
                         MaxRunTimeSecs = 60*60*24,
                         StoppingRounds = 10,
                         MaxDepth = 20,
                         SampleRate = 0.632,
                         MTries = -1,
                         ColSampleRate = 1,
                         ColSampleRatePerTree = 1,
                         ColSampleRatePerTreeLevel  = 1,
                         MinRows = 1,
                         NBins = 20,
                         NBinsCats = 1024,
                         NBinsTopLevel = 1024,
                         CategoricalEncoding = "AUTO",
                         HistogramType = "AUTO",
                         Distribution = "gaussian",
                         Link = "identity",
                         RandomDistribution = NULL,
                         RandomLink = NULL,
                         Solver = "AUTO",
                         Alpha = NULL,
                         Lambda = NULL,
                         LambdaSearch = FALSE,
                         NLambdas = -1,
                         Standardize = TRUE,
                         RemoveCollinearColumns = FALSE,
                         InterceptInclude = TRUE,
                         NonNegativeCoefficients = FALSE,
                         RandomColNumbers = NULL,
                         InteractionColNumbers = NULL) {

  # data.table optimize----
  if(parallel::detectCores() > 10) data.table::setDTthreads(threads = max(1L, parallel::detectCores() - 2L)) else data.table::setDTthreads(threads = max(1L, parallel::detectCores()))

  # Purified args: see CARMA HELPER FUNCTIONS----
  if(DebugMode) print("# Purified args: see CARMA HELPER FUNCTIONS----")
  Args <- CARMA_Define_Args(
    TimeUnit = TimeUnit, TimeGroups = TimeGroups, HierarchGroups = HierarchGroups, GroupVariables = GroupVariables,
    FC_Periods = FC_Periods, PartitionType = PartitionType, TrainOnFull = TrainOnFull, SplitRatios = SplitRatios,
    SD_Periods = SD_Periods, Skew_Periods = Skew_Periods, Kurt_Periods = Kurt_Periods, Quantile_Periods = Quantile_Periods)

  # Store purified args----
  if(DebugMode) print("# Store purified args----")
  IndepentVariablesPass <- Args$IndepentVariablesPass
  TimeGroups            <- Args$TimeGroups
  TimeUnit              <- Args$TimeUnit
  TimeGroup             <- Args$TimeGroupPlaceHolder
  HierarchGroups        <- Args$HierarchGroups
  GroupVariables        <- Args$GroupVariables
  FC_Periods            <- Args$FC_Periods
  HoldOutPeriods        <- Args$HoldOutPeriods
  SD_Periods            <- Args$SD_Periods
  Skew_Periods          <- Args$Skew_Periods
  Kurt_Periods          <- Args$Kurt_Periods
  Quantile_Periods      <- Args$Quantile_Periods

  # Arg check ----
  if(!is.null(HolidayLookback) && !is.numeric(HolidayLookback)) stop("HolidayLookback has to be numeric")

  # Variables for Program: Redefine HoldOutPerids----
  if(!TrainOnFull) HoldOutPeriods <- round(SplitRatios[2]*length(unique(data[[eval(DateColumnName)]])),0)

  # Convert data to data.table----
  if(DebugMode) print("Convert data to data.table----")
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  # Feature Engineering: Add Zero Padding for missing dates----
  if(DebugMode) print("Feature Engineering: Add Zero Padding for missing dates----")
  if(!is.null(ZeroPadSeries)) {
    data <- TimeSeriesFill(
      data,
      DateColumnName = eval(DateColumnName),
      GroupVariables = GroupVariables,
      TimeUnit = TimeUnit,
      FillType = ZeroPadSeries,
      MaxMissingPercent = 0.0,
      SimpleImpute = FALSE)
    data <- RemixAutoML::ModelDataPrep(data = data, Impute = TRUE, CharToFactor = FALSE, FactorToChar = FALSE, IntToNumeric = FALSE, LogicalToBinary = FALSE, DateToChar = FALSE, RemoveDates = FALSE, MissFactor = "0", MissNum = 0, IgnoreCols = NULL)

  } else {

    # Ensure series are filled
    temp <- RemixAutoML::TimeSeriesFill(
      data,
      DateColumnName = eval(DateColumnName),
      GroupVariables = GroupVariables,
      TimeUnit = TimeUnit,
      FillType = "maxmax",
      MaxMissingPercent = 0.25,
      SimpleImpute = FALSE)

    # If not, stop and explain to the user what to do
    if(temp[,.N] != data[,.N]) {
      stop("There are missing dates in your series. You can utilize the ZeroPadSeries argument to handle this or manage it before running the function")
    }
  }

  # Feature Engineering: Add XREGS----
  if(DebugMode) print("Feature Engineering: Add XREGS----")

  # Convert XREGS to data.table
  if(DebugMode) print("# Convert XREGS to data.table")
  if(!is.null(XREGS)) if(!data.table::is.data.table(XREGS)) data.table::setDT(XREGS)

  # Modify FC_Periods ----
  if(DebugMode) print(names(XREGS))
  if(DebugMode) print("# Check lengths of XREGS")
  if(!is.null(XREGS) & TrainOnFull) {
    if(Difference) {
      FC_Periods <- min(-1L + length(unique(XREGS[[eval(DateColumnName)]])) - length(unique(data[[eval(DateColumnName)]])), FC_Periods)
    } else {
      FC_Periods <- min(length(unique(XREGS[[eval(DateColumnName)]])) - length(unique(data[[eval(DateColumnName)]])), FC_Periods)
    }

    # Stop if XREGS doesn't supply forward looking data
    if(FC_Periods < 1) return("Your XREGS does not have forward looking data")

  } else if(!is.null(XREGS)) {
    FC_Periods <- HoldOutPeriods
    HoldOutPeriods <- FC_Periods
  }

  # Check for any Target Variable hiding in XREGS ----
  if(DebugMode) print("# Check for any Target Variable hiding in XREGS")
  if(any(eval(TargetColumnName) %chin% names(XREGS))) data.table::set(XREGS, j = eval(TargetColumnName), value = NULL)

  # Merge data and XREG for Training ----
  if(DebugMode) print("merging xregs to data")
  if(!is.null(XREGS)) {
    if(!is.null(GroupVariables)) {

      # I need GroupVar in the xregs. if not there, add it
      if(!"GroupVar" %chin% names(XREGS)) {
        XREGS[, GroupVar := do.call(paste, c(.SD, sep = " ")), .SDcols = c(GroupVariables)]
      }

      # I need the GroupVariable names to be different from data
      if(any(GroupVariables %chin% names(XREGS))) {
        for(g in GroupVariables) {
          data.table::setnames(x = XREGS, old = eval(g), new = paste0("Add_",eval(g)))
        }
      }

      # Merge data and XREGS
      if(length(GroupVariables) > 1) {
        data[, GroupVar := do.call(paste, c(.SD, sep = " ")), .SDcols = GroupVariables]
        data <- merge(data, XREGS, by = c("GroupVar", eval(DateColumnName)), all.x = TRUE)
        data <- ModelDataPrep(data = data, Impute = TRUE, CharToFactor = FALSE, FactorToChar = FALSE, IntToNumeric = FALSE, LogicalToBinary = FALSE, DateToChar = FALSE, RemoveDates = FALSE, MissFactor = "0", MissNum = -1, IgnoreCols = NULL)
      } else {
        data <- merge(data, XREGS, by.x = c(eval(GroupVariables), eval(DateColumnName)), by.y = c("GroupVar", eval(DateColumnName)), all.x = TRUE)
        data <- ModelDataPrep(data = data, Impute = TRUE, CharToFactor = FALSE, FactorToChar = FALSE, IntToNumeric = FALSE, LogicalToBinary = FALSE, DateToChar = FALSE, RemoveDates = FALSE, MissFactor = "0", MissNum = -1, IgnoreCols = NULL)
      }
    } else {
      data <- merge(data, XREGS, by = c(eval(DateColumnName)), all.x = TRUE)
      data <- ModelDataPrep(data = data, Impute = TRUE, CharToFactor = FALSE, FactorToChar = FALSE, IntToNumeric = FALSE, LogicalToBinary = FALSE, DateToChar = FALSE, RemoveDates = FALSE, MissFactor = "0", MissNum = -1, IgnoreCols = NULL)
    }
  }

  # Check for duplication in the data ----
  if(data[, .N] != unique(data)[, .N]) stop("There is duplicates in your data")

  # Set Keys for data.table usage----
  if(DebugMode) print("# Set Keys for data.table usage----")
  if(!is.null(GroupVariables)) {
    data.table::setkeyv(x = data, cols = c(eval(GroupVariables), eval(DateColumnName)))
    if(!is.null(XREGS)) {
      data.table::setkeyv(x = XREGS, cols = c("GroupVar", eval(DateColumnName)))
    }
  } else {
    data.table::setkeyv(x = data, cols = c(eval(DateColumnName)))
    if(!is.null(XREGS)) {
      data.table::setkeyv(x = XREGS, cols = c(eval(DateColumnName)))
    }
  }

  # Data Wrangling: Remove Unnecessary Columns----
  if(DebugMode) print("Data Wrangling: Remove Unnecessary Columns----")
  if(!is.null(XREGS)) {
    if(!is.null(GroupVariables)) {
      xx <- c(DateColumnName, TargetColumnName, GroupVariables, setdiff(c(names(data), names(XREGS)), c(DateColumnName, TargetColumnName, GroupVariables)))
      xx <- xx[!xx %chin% "GroupVar"]
      data <- data[, .SD, .SDcols = xx]
    } else {
      data <- data[, .SD, .SDcols = c(DateColumnName, TargetColumnName, setdiff(c(names(data), names(XREGS)), c(DateColumnName, TargetColumnName)))]
    }
  } else {
    if(!is.null(GroupVariables)) {
      data <- data[, .SD, .SDcols = c(DateColumnName, TargetColumnName, GroupVariables)]
    } else {
      data <- data[, .SD, .SDcols = c(DateColumnName, TargetColumnName)]
    }
  }

  # Feature Engineering: Concat Categorical Columns - easier to deal with this way (it splits back at end):----
  if(DebugMode) print("Feature Engineering: Concat Categorical Columns - easier to deal with this way (it splits back at end):----")
  if(!is.null(GroupVariables)) {
    if(length(GroupVariables) > 1) {
      data[, GroupVar := do.call(paste, c(.SD, sep = " ")), .SDcols = GroupVariables]
      data[, eval(GroupVariables) := NULL]
    } else {
      data[, GroupVar := do.call(paste, c(.SD, sep = " ")), .SDcols = GroupVariables]
      if(GroupVariables != "GroupVar") data[, eval(GroupVariables) := NULL]
    }
  }

  # Variables for Program: Store unique values of GroupVar in GroupVarVector----
  if(DebugMode) print("Variables for Program: Store unique values of GroupVar in GroupVarVector----")
  if(!is.null(GroupVariables)) {
    GroupVarVector <- data.table::as.data.table(x = unique(as.character(data[["GroupVar"]])))
    data.table::setnames(GroupVarVector, "V1", "GroupVar")
  }

  # Data Wrangling: Standardize column ordering----
  if(DebugMode) print("Data Wrangling: Standardize column ordering----")
  if(!is.null(GroupVariables)) {
    data.table::setcolorder(data, c("GroupVar", eval(DateColumnName), eval(TargetColumnName)))
  } else {
    data.table::setcolorder(data, c(eval(DateColumnName), eval(TargetColumnName)))
  }

  # Data Wrangling: Convert DateColumnName to Date or POSIXct----
  if(DebugMode) print("Data Wrangling: Convert DateColumnName to Date or POSIXct----")
  if(is.character(data[[eval(DateColumnName)]])) {
    if(!(tolower(TimeUnit) %chin% c("1min","5min","10min","15min","30min","hour"))) {
      x <- data[1,get(DateColumnName)]
      x1 <- lubridate::guess_formats(x, orders = c("mdY", "BdY", "Bdy", "bdY", "bdy", "mdy", "dby", "Ymd", "Ydm"))
      data[, eval(DateColumnName) := as.Date(get(DateColumnName), tryFormats = x1)]
    } else {
      data[, eval(DateColumnName) := as.POSIXct(get(DateColumnName))]
    }
  }
  if(!is.null(XREGS)) {
    if(is.character(XREGS[[eval(DateColumnName)]])) {
      x <- XREGS[1,get(DateColumnName)]
      x1 <- lubridate::guess_formats(x, orders = c("mdY", "BdY", "Bdy", "bdY", "bdy", "mdy", "dby", "Ymd", "Ydm"))
      XREGS[, eval(DateColumnName) := as.Date(get(DateColumnName), tryFormats = x1)]
    }
  }

  # Data Wrangling: Ensure TargetColumnName is Numeric----
  if(DebugMode) print("Data Wrangling: Ensure TargetColumnName is Numeric----")
  if(!is.numeric(data[[eval(TargetColumnName)]])) data[, eval(TargetColumnName) := as.numeric(get(TargetColumnName))]

  # Variables for Program: Store number of data partitions in NumSets----
  if(DebugMode) print("Variables for Program: Store number of data partitions in NumSets----")
  NumSets <- 2L

  # Variables for Program: Store Maximum Value of TargetColumnName in val----
  if(DebugMode) print("Variables for Program: Store Maximum Value of TargetColumnName in val----")
  if(is.list(Lags) & is.list(MA_Periods)) val <- max(unlist(Lags), unlist(MA_Periods)) else val <- max(Lags, MA_Periods)

  # Data Wrangling: Sort data by GroupVar then DateColumnName----
  if(DebugMode) print("Data Wrangling: Sort data by GroupVar then DateColumnName----")
  if(!is.null(GroupVariables)) {
    data <- data[order(GroupVar, get(DateColumnName))]
  } else {
    data <- data[order(get(DateColumnName))]
  }

  # Feature Engineering: Add Fourier Features by GroupVar----
  # To error check, store arg values and run through EconometricsFunctions.R AutoHierarchicalFourier
  if(DebugMode) print("Feature Engineering: Add Fourier Features by GroupVar----")
  if(FourierTerms > 0L) {

    # Split GroupVar and Define HierarchyGroups and IndependentGroups
    Output <- CARMA_GroupHierarchyCheck(data = data, Group_Variables = GroupVariables, HierarchyGroups = HierarchGroups)
    data <- Output$data
    HierarchSupplyValue <- Output$HierarchSupplyValue
    IndependentSupplyValue <- Output$IndependentSupplyValue
    rm(Output)

    # Run Independently or Hierarchy (Source: EconometricsFunctions.R)
    Output <- tryCatch({AutoHierarchicalFourier(
      datax = data,
      xRegs = names(XREGS),
      FourierTermS = FourierTerms,
      TimeUniT = TimeUnit,
      FC_PeriodS = FC_Periods,
      TargetColumN = TargetColumnName,
      DateColumN = DateColumnName,
      HierarchGroups = HierarchSupplyValue,
      IndependentGroups = IndependentSupplyValue)},
      error = function(x) NULL)

    # Store Objects If No Error in Hierarchy Run----
    if(!is.null(Output)) {
      if(Output$data[, .N] != 0) {
        data <- Output$data
        FourierFC <- Output$FourierFC
      } else {
        print("Turning off Fourier Terms. Failed to build.")
        FourierTerms <<- 0
      }
    } else {
      print("Turning off Fourier Terms. Failed to build.")
      FourierTerms <<- 0
    }

    # If Fourier is turned off, concatenate grouping cols
    if(FourierTerms == 0) {
      if(!is.null(HierarchGroups)) {
        if(length(HierarchGroups) > 1) {
          if(any(HierarchGroups %chin% names(data))) {
            data[, GroupVar := do.call(paste, c(.SD, sep = " ")), .SDcols = HierarchGroups]
            data[, eval(HierarchGroups) := NULL]
          }
        } else {
          data[, GroupVar := do.call(paste, c(.SD, sep = " ")), .SDcols = HierarchGroups]
          if(HierarchGroups != "GroupVar") {
            data[, eval(HierarchGroups) := NULL]
          }
        }
      } else if(!is.null(GroupVariables)) {
        if(all(GroupVariables %chin% names(data))) {
          data[, GroupVar := do.call(paste, c(.SD, sep = " ")), .SDcols = GroupVariables]
        }
      }
    }
  }

  # Feature Engineering: Add Create Calendar Variables----
  if(DebugMode) print("Feature Engineering: Add Create Calendar Variables----")
  if(!is.null(CalendarVariables)) {

    # Create calendar variables
    data <- CreateCalendarVariables(
      data = data,
      DateCols = eval(DateColumnName),
      AsFactor = FALSE,
      TimeUnits = CalendarVariables)
  }

  # Feature Engineering: Add Create Holiday Variables----
  if(DebugMode) print("Feature Engineering: Add Create Holiday Variables----")
  if(!is.null(HolidayVariable)) {
    data <- CreateHolidayVariables(
      data,
      DateCols = eval(DateColumnName),
      LookbackDays = if(!is.null(HolidayLookback)) HolidayLookback else LB(TimeUnit),
      HolidayGroups = HolidayVariable,
      Holidays = NULL)

    # Convert to lubridate as_date() or POSIXct----
    if(!(tolower(TimeUnit) %chin% c("1min","5min","10min","15min","30min","hour"))) {
      data[, eval(DateColumnName) := lubridate::as_date(get(DateColumnName))]
    } else {
      data[, eval(DateColumnName) := as.POSIXct(get(DateColumnName))]
    }
  }

  # Anomaly detection by Group and Calendar Vars ----
  if(!is.null(AnomalyDetection)) {
    if(!is.null(CalendarVariables) & !is.null(GroupVariables)) {
      if(length(GroupVariables) > 1) {
        groupvars <- c(GroupVariables, paste0(DateColumnName, "_", CalendarVariables[1]))
      } else {
        groupvars <- c("GroupVar", paste0(DateColumnName, "_", CalendarVariables[1]))
      }
      data <- RemixAutoML::GenTSAnomVars(
        data = data, ValueCol = eval(TargetColumnName),
        GroupVars = ,
        DateVar = eval(DateColumnName),
        HighThreshold = AnomalyDetection$tstat_high,
        LowThreshold = AnomalyDetection$tstat_low,
        KeepAllCols = TRUE,
        IsDataScaled = FALSE)
      data[, paste0(eval(TargetColumnName), "_zScaled") := NULL]
      data[, ":=" (RowNumAsc = NULL, CumAnomHigh = NULL, CumAnomLow = NULL, AnomHighRate = NULL, AnomLowRate = NULL)]
    } else if(!is.null(GroupVariables)) {
      data <- RemixAutoML::GenTSAnomVars(
        data = data, ValueCol = eval(TargetColumnName),
        GroupVars = c("GroupVar"),
        DateVar = eval(DateColumnName),
        HighThreshold = AnomalyDetection$tstat_high,
        LowThreshold = AnomalyDetection$tstat_low,
        KeepAllCols = TRUE,
        IsDataScaled = FALSE)
      data[, paste0(eval(TargetColumnName), "_zScaled") := NULL]
      data[, ":=" (RowNumAsc = NULL, CumAnomHigh = NULL, CumAnomLow = NULL, AnomHighRate = NULL, AnomLowRate = NULL)]
    } else {
      data <- RemixAutoML::GenTSAnomVars(
        data = data, ValueCol = eval(TargetColumnName),
        GroupVars = NULL,
        DateVar = eval(DateColumnName),
        HighThreshold = AnomalyDetection$tstat_high,
        LowThreshold = AnomalyDetection$tstat_low,
        KeepAllCols = TRUE,
        IsDataScaled = FALSE)
      data[, paste0(eval(TargetColumnName), "_zScaled") := NULL]
      data[, ":=" (RowNumAsc = NULL, CumAnomHigh = NULL, CumAnomLow = NULL, AnomHighRate = NULL, AnomLowRate = NULL)]
    }
  }

  # Feature Engineering: Add Target Transformation----
  if(DebugMode) print("Feature Engineering: Add Target Transformation----")
  if(TargetTransformation) {
    TransformResults <- AutoTransformationCreate(
      data,
      ColumnNames = TargetColumnName,
      Methods = Methods,
      Path = NULL,
      TransID = "Trans",
      SaveOutput = FALSE
    )
    data <- TransformResults$Data
    TransformObject <- TransformResults$FinalResults
  }

  # Copy data for non grouping + difference----
  if(DebugMode) print("Copy data for non grouping + difference----")
  if(is.null(GroupVariables) & Difference == TRUE) {
    antidiff <- data.table::copy(data[, .SD, .SDcols = c(eval(TargetColumnName),eval(DateColumnName))])
  }

  # Feature Engineering: Add Difference Data----
  if(DebugMode) print("Feature Engineering: Add Difference Data----")
  if(!is.null(GroupVariables) & Difference == TRUE) {
    data[, TargetDiffMidStep := data.table::shift(x = get(TargetColumnName), n = 1, fill = NA, type = "lag"), by = c("GroupVar")][, ModTarget := get(TargetColumnName) - TargetDiffMidStep]
    dataStart <- data[is.na(TargetDiffMidStep)]
    data <- data[!is.na(TargetDiffMidStep)]
    FC_Periods <- FC_Periods + 1L
  } else if(Difference == TRUE) {
    DiffTrainOutput <- DifferenceData(
      data = data,
      ColumnsToDiff = eval(TargetColumnName),
      CARMA = TRUE,
      TargetVariable = eval(TargetColumnName),
      GroupingVariable = NULL)
    Train <- DiffTrainOutput$DiffData
    if(ncol(data) >= 3) {
      data <- cbind(Train,data[1:(nrow(data)-1)][,.SD, .SDcols = names(data)[3:ncol(data)]])
    } else {
      data <- Train
    }
    FC_Periods <- FC_Periods + 1L
  }

  # Feature Engineering: Add GDL Features based on the TargetColumnName ----
  if(DebugMode) print("Feature Engineering: Add GDL Features based on the TargetColumnName----")

  # Group and !Difference
  if(!is.null(GroupVariables) & !Difference) {

    # Split GroupVar and Define HierarchyGroups and IndependentGroups----
    Output <- CARMA_GroupHierarchyCheck(data = data, Group_Variables = GroupVariables, HierarchyGroups = HierarchGroups)
    data <- Output$data
    HierarchSupplyValue <- Output$HierarchSupplyValue
    IndependentSupplyValue <- Output$IndependentSupplyValue

    # Generate features----
    data <- AutoLagRollStats(

      # Data
      data                 = data,
      DateColumn           = eval(DateColumnName),
      Targets              = eval(TargetColumnName),
      HierarchyGroups      = HierarchSupplyValue,
      IndependentGroups    = IndependentSupplyValue,

      # Services
      TimeBetween          = NULL,
      TimeUnit             = TimeUnit,
      TimeUnitAgg          = TimeUnit,
      TimeGroups           = TimeGroups,
      RollOnLag1           = TRUE,
      Type                 = "Lag",
      SimpleImpute         = TRUE,

      # Calculated Columns
      Lags                  = Lags,
      MA_RollWindows        = MA_Periods,
      SD_RollWindows        = SD_Periods,
      Skew_RollWindows      = Skew_Periods,
      Kurt_RollWindows      = Kurt_Periods,
      Quantile_RollWindows  = Quantile_Periods,
      Quantiles_Selected    = Quantiles_Selected,
      Debug                 = FALSE)

    # Keep interaction group as GroupVar----
    if(length(GroupVariables) > 1) {
      if(!"GroupVar" %chin% names(data)) data[, GroupVar := do.call(paste, c(.SD, sep = " ")), .SDcols = GroupVariables]
      if(!is.null(HierarchGroups)) Categoricals <- FullFactorialCatFeatures(GroupVars = HierarchGroups, BottomsUp = TRUE)
      if(!is.null(HierarchGroups)) GroupVarVector <- data[, .SD, .SDcols = c(Categoricals,"GroupVar")] else GroupVarVector <- data[, .SD, .SDcols = c("GroupVar")]
    } else {
      if(!"GroupVar" %chin% names(data)) data[, GroupVar := do.call(paste, c(.SD, sep = " ")), .SDcols = GroupVariables]
    }
  }

  # Group and Difference
  if(!is.null(GroupVariables) & Difference) {

    # Split GroupVar and Define HierarchyGroups and IndependentGroups----
    Output <- CARMA_GroupHierarchyCheck(data = data, Group_Variables = GroupVariables, HierarchyGroups = HierarchGroups)
    data <- Output$data
    HierarchSupplyValue <- Output$HierarchSupplyValue
    IndependentSupplyValue <- Output$IndependentSupplyValue

    # Generate features----
    data <- AutoLagRollStats(

      # Data
      data                 = data,
      DateColumn           = DateColumnName,
      Targets              = "ModTarget",
      HierarchyGroups      = HierarchSupplyValue,
      IndependentGroups    = IndependentSupplyValue,

      # Services
      TimeBetween          = NULL,
      TimeUnit             = TimeUnit,
      TimeUnitAgg          = TimeUnit,
      TimeGroups           = TimeGroups,
      RollOnLag1           = TRUE,
      Type                 = "Lag",
      SimpleImpute         = TRUE,

      # Calculated Columns
      Lags                 = Lags,
      MA_RollWindows       = MA_Periods,
      SD_RollWindows       = SD_Periods,
      Skew_RollWindows     = Skew_Periods,
      Kurt_RollWindows     = Kurt_Periods,
      Quantile_RollWindows = Quantile_Periods,
      Quantiles_Selected   = Quantiles_Selected,
      Debug                = DebugMode)

    # Keep interaction group as GroupVar----
    if(length(GroupVariables) > 1L) {
      if(!"GroupVar" %chin% names(data)) data[, GroupVar := do.call(paste, c(.SD, sep = " ")), .SDcols = GroupVariables]
      if(!is.null(HierarchGroups)) Categoricals <- FullFactorialCatFeatures(GroupVars = HierarchGroups, BottomsUp = TRUE)
      if(!is.null(HierarchGroups)) GroupVarVector <- data[, .SD, .SDcols = c(Categoricals,"GroupVar")] else GroupVarVector <- data[, .SD, .SDcols = c("GroupVar")]
    } else {
      if(!"GroupVar" %chin% names(data)) data[, GroupVar := do.call(paste, c(.SD, sep = " ")), .SDcols = GroupVariables]
    }
  }

  # No Group with or without Difference
  if(is.null(GroupVariables)) {

    # Generate features----
    data <- AutoLagRollStats(

      # Data
      data                 = data,
      DateColumn           = eval(DateColumnName),
      Targets              = eval(TargetColumnName),
      HierarchyGroups      = NULL,
      IndependentGroups    = NULL,

      # Services
      TimeBetween          = NULL,
      TimeUnit             = TimeUnit,
      TimeUnitAgg          = TimeUnit,
      TimeGroups           = TimeGroups,
      RollOnLag1           = TRUE,
      Type                 = "Lag",
      SimpleImpute         = TRUE,

      # Calculated Columns
      Lags                  = Lags,
      MA_RollWindows        = MA_Periods,
      SD_RollWindows        = SD_Periods,
      Skew_RollWindows      = Skew_Periods,
      Kurt_RollWindows      = Kurt_Periods,
      Quantile_RollWindows  = Quantile_Periods,
      Quantiles_Selected    = Quantiles_Selected,
      Debug                 = TRUE)
  }

  # Feature Engineering: Add Lag / Lead, MA Holiday Variables----
  if(DebugMode) print("Feature Engineering: Add Lag / Lead, MA Holiday Variables----")
  if(!is.null(HolidayVariable) & max(HolidayLags) > 0 & max(HolidayMovingAverages) > 0) {
    if(!is.null(GroupVariables)) {
      data <- DT_GDL_Feature_Engineering(
        data,
        lags            = HolidayLags,
        periods         = HolidayMovingAverages[!HolidayMovingAverages %in% 1],
        SDperiods       = 0,
        Skewperiods     = 0,
        Kurtperiods     = 0,
        Quantileperiods = 0,
        statsFUNs       = "mean",
        targets         = "HolidayCounts",
        groupingVars    = IndepentVariablesPass,
        sortDateName    = eval(DateColumnName),
        timeDiffTarget  = NULL,
        timeAgg         = TimeGroups[1],
        WindowingLag    = 1,
        Type            = "Lag",
        SimpleImpute    = TRUE)

    } else {
      data <- DT_GDL_Feature_Engineering(
        data,
        lags            = HolidayLags,
        periods         = HolidayMovingAverages[!HolidayMovingAverages %in% 1],
        SDperiods       = 0,
        Skewperiods     = 0,
        Kurtperiods     = 0,
        Quantileperiods = 0,
        statsFUNs       = "mean",
        targets         = "HolidayCounts",
        groupingVars    = NULL,
        sortDateName    = eval(DateColumnName),
        timeDiffTarget  = NULL,
        timeAgg         = TimeGroups[1],
        WindowingLag    = 1,
        Type            = "Lag",
        SimpleImpute    = TRUE)
    }
  }

  # Create GroupVar----
  if(!is.null(GroupVariables)) {
    if(length(GroupVariables) > 1) {
      if(!"GroupVar" %chin% names(data)) {
        data[, GroupVar := do.call(paste, c(.SD, sep = " ")), .SDcols = GroupVariables]
      }
    } else {
      if(!"GroupVar" %chin% names(data)) {
        data[, GroupVar := do.call(paste, c(.SD, sep = " ")), .SDcols = GroupVariables]
      }
    }
  }

  # Data Wrangling: ModelDataPrep() to prepare data----
  if(DebugMode) print("Data Wrangling: ModelDataPrep() to prepare data----")
  data <- ModelDataPrep(
    data,
    Impute = TRUE,
    CharToFactor = TRUE,
    RemoveDates = FALSE,
    MissFactor = "0",
    MissNum    = -1)

  # Data Wrangling: Remove dates with imputed data from the DT_GDL_Feature_Engineering() features----
  if(DebugMode) print("Data Wrangling: Remove dates with imputed data from the DT_GDL_Feature_Engineering() features----")
  if(DataTruncate & !is.null(Lags)) {
    mindate <- data[, min(get(DateColumnName))]
    if(tolower(TimeUnit) %chin% c("hour","hours")) {
      newdate <- mindate + lubridate::hours(val + 1)
    } else if(tolower(TimeUnit) %chin% c("1min","1mins","1minute","1minutes")) {
      newdate <- mindate + lubridate::minutes(val + 1)
    } else if(tolower(TimeUnit) %chin% c("5min","5mins","5minute","5minutes")) {
      newdate <- mindate + lubridate::minutes(val + 1)
    } else if(tolower(TimeUnit) %chin% c("10min","10mins","10minute","10minutes")) {
      newdate <- mindate + lubridate::minutes(val + 1)
    } else if(tolower(TimeUnit) %chin% c("15min","15mins","15minute","15minutes")) {
      newdate <- mindate + lubridate::minutes(val + 1)
    } else if(tolower(TimeUnit) %chin% c("30min","30mins","30minute","30minutes")) {
      newdate <- mindate + lubridate::minutes(val + 1)
    } else if(tolower(TimeUnit) %chin% c("day","days")) {
      newdate <- mindate + lubridate::days(val + 1)
    } else if(tolower(TimeUnit) %chin% c("week","weeks")) {
      newdate <- mindate + lubridate::weeks(val + 1)
    } else if(tolower(TimeUnit) %chin% c("month","months")) {
      newdate <- mindate %m+% months(val + 1)
    } else if(tolower(TimeUnit) %chin% c("quarter","quarters")) {
      newdate <- mindate %m+% months(val + 1)
    } else if(tolower(TimeUnit) %chin% c("years","year")) {
      newdate <- mindate + lubridate::years(val + 1)
    }

    # Update date
    data <- data[get(DateColumnName) >= eval(newdate)]
  }

  # Feature Engineering: Add TimeTrend Variable----
  if(DebugMode) print("Feature Engineering: Add TimeTrend Variable----")
  if(TimeTrendVariable) {
    if(!is.null(GroupVariables)) {
      data[, TimeTrend := 1:.N, by = "GroupVar"]
    } else {
      data[, TimeTrend := 1:.N]
    }
  }

  # Store Date Info----
  if(DebugMode) print("Store Date Info----")
  FutureDateData <- unique(data[, get(DateColumnName)])

  # Return engineered data before Partitioning ----
  if(!is.null(SaveDataPath)) {
    data.table::fwrite(data, file.path(SaveDataPath, "ModelData.csv"))
  }

  # Data Wrangling: Partition data with AutoDataPartition() ----
  if(DebugMode) print("Data Wrangling: Partition data with AutoDataPartition()----")
  if(!TrainOnFull) {
    if(Difference & !is.null(GroupVariables)) {
      x <- length(unique(data[[eval(DateColumnName)]]))
      N1 <- x+1L - SplitRatios[1]*(x+1L)
      DataSets <- AutoDataPartition(
        data,
        NumDataSets = NumSets,
        Ratios = c(1-N1/x,N1/x),
        PartitionType = PartitionType,
        StratifyColumnNames = "GroupVar",
        TimeColumnName = eval(DateColumnName))
    } else if(Difference) {
      x <- length(unique(data[[eval(DateColumnName)]]))
      N1 <- x+1L - SplitRatios[1]*(x+1L)
      DataSets <- AutoDataPartition(
        data,
        NumDataSets = NumSets,
        Ratios = c(1-N1/x,N1/x),
        PartitionType = PartitionType,
        StratifyColumnNames = NULL,
        TimeColumnName = eval(DateColumnName))
    } else if(!is.null(GroupVariables)) {
      DataSets <- AutoDataPartition(
        data,
        NumDataSets = NumSets,
        Ratios = SplitRatios,
        PartitionType = PartitionType,
        StratifyColumnNames = "GroupVar",
        TimeColumnName = eval(DateColumnName))
    } else {
      DataSets <- AutoDataPartition(
        data,
        NumDataSets = NumSets,
        Ratios = SplitRatios,
        PartitionType = PartitionType,
        StratifyColumnNames = NULL,
        TimeColumnName = eval(DateColumnName))
    }

    # Remove ID Column----
    if("ID" %chin% names(data)) data.table::set(data, j = "ID", value = NULL)
  }

  # Variables for CARMA function: Define data sets----
  if(DebugMode) print("Variables for CARMA function: Define data sets----")
  if(!TrainOnFull) {
    if(NumSets == 2) {
      train <- DataSets$TrainData
      valid <- DataSets$ValidationData
      test  <- NULL
    } else if(NumSets == 3) {
      train <- DataSets$TrainData
      valid <- DataSets$ValidationData
      test  <- DataSets$TestData
    }
    rm(DataSets)
  } else {
    train <- data
    valid <- NULL
    test  <- NULL
  }

  # Data Wrangling: copy data or train for later in function since AutoRegression will modify data and train----
  if(DebugMode) print("Data Wrangling: copy data or train for later in function since AutoRegression will modify data and train----")
  if(!is.null(GroupVariables)) {
    data.table::setorderv(x = data, cols = c("GroupVar",eval(DateColumnName)), order = c(1,1))
    Step1SCore <- data.table::copy(data)
  } else {
    data.table::setorderv(x = data, cols = c(eval(DateColumnName)), order = c(1))
    Step1SCore <- data.table::copy(data)
  }

  # Define features for training ----
  if(DebugMode) print("Machine Learning: Build Model----")

  # Define CARMA feature names
  if(Difference == FALSE || is.null(GroupVariables)) {
    if(!is.null(XREGS)) {
      ModelFeatures <- setdiff(names(data),c(eval(TargetColumnName),eval(DateColumnName)))
    } else {
      ModelFeatures <- setdiff(names(train),c(eval(TargetColumnName),eval(DateColumnName)))
    }
    TargetVariable <- eval(TargetColumnName)
  } else if(Difference == TRUE && !is.null(GroupVariables)) {
    ModelFeatures <- setdiff(names(train),c(eval(TargetColumnName),"ModTarget",eval(DateColumnName)))
    TargetVariable <- "ModTarget"
  } else {
    ModelFeatures <- setdiff(names(train),c(eval(TargetColumnName),eval(DateColumnName)))
  }

  # Initialize H2O ----

  ####
  #if(DebugMode) print("Initialize H2O----")
  #tryCatch({h2o::h2o.init(startH2O = FALSE)}, error = function(x) localHost <- h2o::h2o.init(nthreads = NThreads, max_mem_size = MaxMem, enable_assertions = FALSE))
  ####

  # Return warnings to default since h2o will issue warning for constant valued coluns
  if(DebugMode) options(warn = 0)

  # Run ML Algo and return list of ml objects ----
  if(tolower(AlgoType) == "drf") {

    # Distributed Random Forecast ----
    TestModel <- RemixAutoML::AutoH2oDRFRegression(

      # Compute management
      MaxMem = MaxMem,
      NThreads = NThreads,
      H2OShutdown = TRUE,
      H2OStartUp = TRUE,
      IfSaveModel = "mojo",

      # Model evaluation
      eval_metric = EvalMetric,
      NumOfParDepPlots = NumOfParDepPlots,

      # Metadata arguments
      model_path = ModelPath,
      metadata_path = if(!is.null(PDFOutputPath)) PDFOutputPath else getwd(),
      SaveInfoToPDF = if(!is.null(PDFOutputPath)) TRUE else FALSE,
      ModelID = paste0(AlgoType, "_Carma"),
      ReturnModelObjects = TRUE,
      SaveModelObjects = TRUE,
      DebugMode = DebugMode,

      # Data arguments
      data = train,
      TrainOnFull = TrainOnFull,
      ValidationData = valid,
      TestData = test,
      TargetColumnName = TargetVariable,
      FeatureColNames = ModelFeatures,
      WeightsColumn = NULL,
      TransformNumericColumns = NULL,
      Methods = NULL,

      # Grid tuning args
      GridTune = GridTune,
      MaxModelsInGrid = ModelCount,
      GridStrategy = GridStrategy,
      MaxRunTimeSecs = MaxRunTimeSecs,
      StoppingRounds = StoppingRounds,

      # ML Args
      Trees = NTrees,
      MaxDepth = MaxDepth,
      SampleRate = SampleRate,
      MTries = MTries,
      ColSampleRatePerTree = ColSampleRatePerTree,
      ColSampleRatePerTreeLevel = ColSampleRatePerTreeLevel,
      MinRows = MinRows,
      NBins = NBins,
      NBinsCats = NBinsCats,
      NBinsTopLevel = NBinsTopLevel,
      CategoricalEncoding = CategoricalEncoding,
      HistogramType = "AUTO")

  } else if(tolower(AlgoType) == "gbm") {

    # Gradient Boosting Machine ----
    TestModel <- RemixAutoML::AutoH2oGBMRegression(

      # Compute management
      MaxMem = MaxMem,
      NThreads = NThreads,
      H2OShutdown = TRUE,
      H2OStartUp = TRUE,
      IfSaveModel = "mojo",
      Alpha = NULL,
      Distribution = "gaussian",

      # Model evaluation
      eval_metric = EvalMetric,
      NumOfParDepPlots = NumOfParDepPlots,

      # Metadata arguments
      model_path = ModelPath,
      metadata_path = if(!is.null(PDFOutputPath)) PDFOutputPath else getwd(),
      SaveInfoToPDF = if(!is.null(PDFOutputPath)) TRUE else FALSE,
      ModelID = paste0(AlgoType, "_Carma"),
      ReturnModelObjects = TRUE,
      SaveModelObjects = TRUE,
      DebugMode = DebugMode,

      # Data arguments
      data = train,
      TrainOnFull = TrainOnFull,
      ValidationData = valid,
      TestData = test,
      TargetColumnName = TargetVariable,
      FeatureColNames = ModelFeatures,
      WeightsColumn = NULL,
      TransformNumericColumns = NULL,
      Methods = NULL,

      # Model args
      GridTune = GridTune,
      MaxModelsInGrid = ModelCount,
      GridStrategy = GridStrategy,
      MaxRunTimeSecs = MaxRunTimeSecs,
      StoppingRounds = StoppingRounds,

      # ML args
      Trees = NTrees,
      LearnRate = LearnRate,
      LearnRateAnnealing = LearnRateAnnealing,
      MaxDepth = MaxDepth,
      SampleRate = SampleRate,
      ColSampleRate = ColSampleRate,
      ColSampleRatePerTree = ColSampleRatePerTree,
      ColSampleRatePerTreeLevel = ColSampleRatePerTreeLevel,
      MinRows = MinRows,
      NBins = NBins,
      NBinsCats = NBinsCats,
      NBinsTopLevel = NBinsTopLevel,
      HistogramType = HistogramType,
      CategoricalEncoding = CategoricalEncoding)

  } else if(tolower(AlgoType) == "glm") {

    # Generalized Linear Model ----
    TestModel <- RemixAutoML::AutoH2oGLMRegression(

      # Compute management
      MaxMem = MaxMem,
      NThreads = NThreads,
      H2OShutdown = TRUE,
      H2OStartUp = TRUE,
      IfSaveModel = "mojo",

      # Model evaluation
      eval_metric = EvalMetric,
      NumOfParDepPlots = NumOfParDepPlots,

      # Metadata arguments
      model_path = ModelPath,
      metadata_path = if(!is.null(PDFOutputPath)) PDFOutputPath else getwd(),
      SaveInfoToPDF = if(!is.null(PDFOutputPath)) TRUE else FALSE,
      ModelID = paste0(AlgoType, "_Carma"),
      ReturnModelObjects = TRUE,
      SaveModelObjects = TRUE,
      DebugMode = DebugMode,

      # Data arguments
      data = train,
      TrainOnFull = TrainOnFull,
      ValidationData = valid,
      TestData = test,
      TargetColumnName = TargetVariable,
      FeatureColNames = ModelFeatures,
      RandomColNumbers = RandomColNumbers,
      InteractionColNumbers = InteractionColNumbers,
      WeightsColumn = WeightsColumn,
      TransformNumericColumns = NULL,
      Methods = NULL,

      # Model args
      GridTune = GridTune,
      MaxModelsInGrid = ModelCount,
      Distribution = Distribution,
      Link = Link,
      RandomDistribution = RandomDistribution,
      RandomLink = RandomLink,
      Solver = Solver,
      Alpha = Alpha,
      Lambda = Lambda,
      LambdaSearch = LambdaSearch,
      NLambdas = NLambdas,
      Standardize = Standardize,
      RemoveCollinearColumns = RemoveCollinearColumns,
      InterceptInclude = InterceptInclude,
      NonNegativeCoefficients = NonNegativeCoefficients)

  } else if(tolower(AlgoType) == "automl") {

    # H2O AutoML ----
    TestModel <- RemixAutoML::AutoH2oMLRegression(

      # Compute management
      MaxMem = MaxMem,
      NThreads = NThreads,
      H2OShutdown = TRUE,
      H2OStartUp = TRUE,
      IfSaveModel = "mojo",

      # Model evaluation
      eval_metric = EvalMetric,
      NumOfParDepPlots = NumOfParDepPlots,

      # Metadata arguments
      model_path = ModelPath,
      metadata_path = getwd(),
      ModelID = paste0(AlgoType, "_Carma"),
      ReturnModelObjects = TRUE,
      SaveModelObjects = TRUE,
      DebugMode = DebugMode,

      # Data arguments
      data = data,
      TrainOnFull = TrainOnFull,
      ValidationData = valid,
      TestData = test,
      TargetColumnName = TargetVariable,
      FeatureColNames = ModelFeatures,
      TransformNumericColumns = NULL,
      Methods = NULL,

      # Model args
      ExcludeAlgos = ExcludeAlgos,
      Trees = NTrees,
      MaxModelsInGrid = ModelCount)
  } else if(tolower(AlgoType) == "gam") {

    # Define Gam Cols ----
    GamCols <- names(which(unlist(lapply(data[, .SD, .SDcols = ModelFeatures], is.numeric))))
    GamCols <- GamCols[!GamCols %like% "HolidayCounts" & !GamCols %like% "wom" & !GamCols %like% "quarter" & !GamCols %like% "year"]
    GamCols <- GamCols[1L:(min(9L,length(GamCols)))]

    # Build GAM ----
    TestModel <- RemixAutoML::AutoH2oGAMRegression(

      # Compute management
      MaxMem = MaxMem,
      NThreads = NThreads,
      H2OShutdown = TRUE,
      H2OStartUp = TRUE,
      IfSaveModel = "mojo",
      Distribution = "gaussian",

      # Model evaluation
      eval_metric = EvalMetric,
      NumOfParDepPlots = NumOfParDepPlots,

      # Metadata arguments
      model_path = ModelPath,
      metadata_path = if(!is.null(PDFOutputPath)) PDFOutputPath else getwd(),
      SaveInfoToPDF = if(!is.null(PDFOutputPath)) TRUE else FALSE,
      ModelID = paste0(AlgoType, "_Carma"),
      ReturnModelObjects = TRUE,
      SaveModelObjects = TRUE,
      DebugMode = DebugMode,

      # Data arguments
      data = train,
      TrainOnFull = TrainOnFull,
      ValidationData = valid,
      TestData = test,
      TargetColumnName = TargetVariable,
      FeatureColNames = ModelFeatures,
      GamColNames = GamCols,
      TransformNumericColumns = NULL,
      Methods = NULL,

      # Model args
      GridTune = GridTune,
      MaxModelsInGrid = ModelCount)
  }

  # Return model object for when TrainOnFull is FALSE ----
  if(!TrainOnFull) return(TestModel)

  # Turn warnings into errors back on
  if(DebugMode) options(warn = 0)

  # Variable for storing ML model: Pull model object out of TestModel list----
  if(DebugMode) print("Variable for storing ML model: Pull model object out of TestModel list----")
  Model <- TestModel$Model

  # Variable for interation counts: max number of rows in train data.table across all group----
  if(DebugMode) print("Variable for interation counts: max number of rows in train data.table across all group----")
  if(!is.null(GroupVariables)) {
    if(Difference) {
      if(!"GroupVar" %chin% names(train)) N <- as.integer(train[, .N, by = c(eval(GroupVariables))][, max(N)]) - 2L else N <- as.integer(train[, .N, by = "GroupVar"][, max(N)]) - 2L
    } else {
      N <- as.integer(train[, .N, by = "GroupVar"][, max(N)])
    }
  } else {
    N <- as.integer(train[, .N])
  }

  # Number of forecast periods----
  if(DebugMode) print("Number of forecast periods----")
  if(TrainOnFull) ForecastRuns <- FC_Periods else ForecastRuns <- HoldOutPeriods

  #----

  #----

  # ARMA PROCESS FORECASTING ----
  if(DebugMode) print("ARMA PROCESS FORECASTING----")
  if(DebugMode) print("ARMA PROCESS FORECASTING----")
  if(DebugMode) print("ARMA PROCESS FORECASTING----")
  if(DebugMode) print("ARMA PROCESS FORECASTING----")
  if(DebugMode) print("ARMA PROCESS FORECASTING----")
  for(i in seq_len(ForecastRuns + 1L)) {

    # Row counts----
    if(DebugMode) print("Row counts----")
    if (i != 1) N <- N + 1L

    ###############
    # ML Scoring
    ###############

    # Machine Learning: Generate predictions ----
    if(DebugMode) print("Machine Learning: Generate predictions----")
    if(i == 1L) {

      # i = 1 Score Model With Group Variables ----
      if(DebugMode) print("# i = 1 Score Model With Group Variables----")
      Preds <- AutoH2OMLScoring(
        ScoringData = Step1SCore,
        ModelObject = NULL,
        ModelType = "mojo",
        H2OShutdown = TRUE,
        MaxMem = MaxMem,
        JavaOptions = NULL,
        ModelPath = ModelPath,
        ModelID = paste0(AlgoType, "_Carma"),
        ReturnFeatures = TRUE,
        TransformNumeric = FALSE,
        BackTransNumeric = FALSE,
        TargetColumnName = NULL,
        TransformationObject = NULL,
        TransID = NULL,
        TransPath = NULL,
        MDP_Impute = TRUE,
        MDP_CharToFactor = TRUE,
        MDP_RemoveDates = FALSE,
        MDP_MissFactor = "0",
        MDP_MissNum = -1)

      # Data Wrangline: grab historical data and one more future record ----
      if(Difference) {
        if(eval(TargetColumnName) %chin% names(Step1SCore)) {
          if(eval(TargetColumnName) %chin% names(Preds)) {
            data.table::set(Preds, j = eval(TargetColumnName), value = NULL)
          }
        }
        if(eval(DateColumnName) %chin% names(Preds)) data.table::set(Preds, j = eval(DateColumnName), value = NULL)
        UpdateData <- cbind(Step1SCore[, .SD, .SDcols = c(eval(TargetColumnName), eval(DateColumnName))],Preds)
      } else {
        if(NonNegativePred) Preds[, Predictions := data.table::fifelse(Predictions < 0.5, 0, Predictions)]
        if(RoundPreds) Preds[, Predictions := round(Predictions)]
        UpdateData <- Preds
      }

    } else {
      if(!is.null(GroupVariables)) {
        if(Difference) IDcols = "ModTarget" else IDcols <- eval(TargetColumnName)

        # GroupVar or Hierarchical----
        if(!is.null(HierarchGroups)) {
          temp <- data.table::copy(UpdateData[, ID := 1L:.N, by = c(eval(GroupVariables))])
          temp <- temp[ID == N][, ID := NULL]
        } else {
          temp <- data.table::copy(UpdateData[, ID := 1L:.N, by = "GroupVar"])
          temp <- temp[ID == N][, ID := NULL]
        }

        # Score model----
        Preds <- AutoH2OMLScoring(
          ScoringData = temp,
          ModelObject = NULL,
          ModelType = "mojo",
          H2OShutdown = TRUE,
          MaxMem = MaxMem,
          JavaOptions = NULL,
          ModelPath = ModelPath,
          ModelID = paste0(AlgoType, "_Carma"),
          ReturnFeatures = FALSE,
          TransformNumeric = FALSE,
          BackTransNumeric = FALSE,
          TargetColumnName = NULL,
          TransformationObject = NULL,
          TransID = NULL,
          TransPath = NULL,
          MDP_Impute = TRUE,
          MDP_CharToFactor = TRUE,
          MDP_RemoveDates = TRUE,
          MDP_MissFactor = "0",
          MDP_MissNum = -1)

        # Update data group case----
        if(DebugMode) print("Update data group case----")
        data.table::setnames(Preds, "Predictions", "Preds")
        if(NonNegativePred & !Difference) Preds[, Preds := data.table::fifelse(Preds < 0.5, 0, Preds)]
        Preds <- cbind(UpdateData[ID == N], Preds)
        if(Difference) Preds[, ModTarget := Preds][, eval(TargetColumnName) := Preds] else Preds[, eval(TargetColumnName) := Preds]
        Preds[, Predictions := Preds][, Preds := NULL]
        if(RoundPreds) Preds[, Predictions := round(Predictions)]
        UpdateData <- UpdateData[ID != N]
        if(any(class(UpdateData$Date) %chin% c("POSIXct","POSIXt")) & any(class(Preds$Date) == "Date")) UpdateData[, eval(DateColumnName) := as.Date(get(DateColumnName))]
        UpdateData <- data.table::rbindlist(list(UpdateData, Preds))
        if(Difference) UpdateData[ID %in% c(N-1L,N), eval(TargetColumnName) := cumsum(get(TargetColumnName)), by = "GroupVar"]
        UpdateData[, ID := NULL]

      } else {

        # Score Model----
        Preds <- AutoH2OMLScoring(
          ScoringData = UpdateData[.N],
          ModelObject = NULL,
          ModelType = "mojo",
          H2OShutdown = TRUE,
          MaxMem = MaxMem,
          JavaOptions = NULL,
          ModelPath = ModelPath,
          ModelID = paste0(AlgoType, "_Carma"),
          ReturnFeatures = TRUE,
          TransformNumeric = FALSE,
          BackTransNumeric = FALSE,
          TargetColumnName = NULL,
          TransformationObject = NULL,
          TransID = NULL,
          TransPath = NULL,
          MDP_Impute = TRUE,
          MDP_CharToFactor = TRUE,
          MDP_RemoveDates = FALSE,
          MDP_MissFactor = "0",
          MDP_MissNum = -1)

        # Update data non-group case----
        if(DebugMode) print("Update data non-group case----")
        if(RoundPreds) Preds[, eval(names(Preds)[1L]) := round(Preds[[1L]])]
        data.table::set(UpdateData, i = N, j = 2L:3L, value = Preds[[1L]])
      }
    }

    ###############
    # Forecasting
    ###############

    # Update lags and moving average features for next run----
    if(i != ForecastRuns+1L) {

      # Timer----
      if(DebugMode) print("Timer----")
      if(Timer) if(i != 1) print(paste("Forecast future step: ", i-1))
      if(Timer) starttime <- Sys.time()

      # Create single future record----
      if(DebugMode) print("Create single future record----")
      d <- max(UpdateData[[eval(DateColumnName)]])
      if (tolower(TimeUnit) %chin% c("hour","hours")) {
        CalendarFeatures <- data.table::as.data.table(d + lubridate::hours(1))
      } else if(tolower(TimeUnit) %chin% c("1min","1mins","1minute","1minutes")) {
        CalendarFeatures <- data.table::as.data.table(d + lubridate::minutes(1))
      } else if(tolower(TimeUnit) %chin% c("5min","5mins","5minute","5minutes")) {
        CalendarFeatures <- data.table::as.data.table(d + lubridate::minutes(5))
      } else if(tolower(TimeUnit) %chin% c("10min","10mins","10minute","10minutes")) {
        CalendarFeatures <- data.table::as.data.table(d + lubridate::minutes(10))
      } else if(tolower(TimeUnit) %chin% c("15min","15mins","15minute","15minutes")) {
        CalendarFeatures <- data.table::as.data.table(d + lubridate::minutes(15))
      } else if(tolower(TimeUnit) %chin% c("30min","30mins","30minute","30minutes")) {
        CalendarFeatures <- data.table::as.data.table(d + lubridate::minutes(30))
      } else if(tolower(TimeUnit) %chin% c("day","days")) {
        CalendarFeatures <- data.table::as.data.table(d + lubridate::days(1))
      } else if(tolower(TimeUnit) %chin% c("week","weeks")) {
        CalendarFeatures <- data.table::as.data.table(d + lubridate::weeks(1))
      } else if(tolower(TimeUnit) %chin% c("month","months")) {
        CalendarFeatures <- data.table::as.data.table(d %m+% months(1))
      } else if(tolower(TimeUnit) %chin% c("quarter","quarters")) {
        CalendarFeatures <- data.table::as.data.table(d %m+% months(3))
      } else if(tolower(TimeUnit) %chin% c("years","year")) {
        CalendarFeatures <- data.table::as.data.table(d + lubridate::years(1))
      }

      # Merge groups vars----
      if(DebugMode) print("Merge groups vars----")
      if(!is.null(GroupVariables)) CalendarFeatures <- cbind(unique(GroupVarVector), CalendarFeatures)

      # Update colname for date----
      if(DebugMode) print("Update colname for date----")
      data.table::setnames(CalendarFeatures, names(CalendarFeatures)[ncol(CalendarFeatures)], eval(DateColumnName))

      # Merge XREGS if not null----
      if(DebugMode) print("Merge XREGS if not null----")
      if(!is.null(XREGS)) {
        if(!is.null(GroupVariables)) {
          CalendarFeatures <- merge(CalendarFeatures, XREGS, by = c("GroupVar",eval(DateColumnName)), all = FALSE)
        } else {
          CalendarFeatures <- merge(CalendarFeatures, XREGS, by = eval(DateColumnName), all = FALSE)
        }
      }

      # Add fouier terms----
      if(DebugMode) print("Add fouier terms----")
      if(is.null(GroupVariables) & FourierTerms > 0) {
        CalendarFeatures <- merge(CalendarFeatures, FourierFC, by = DateColumnName, all = FALSE)
      } else if(FourierTerms > 0) {
        if(exists("FourierFC")) {
          if(length(FourierFC) != 0) {
            CalendarFeatures <- merge(CalendarFeatures, FourierFC, by = c("GroupVar",eval(DateColumnName)), all = FALSE)
          }
        }
      }

      # Prepare for more feature engineering----
      if(DebugMode) print("Prepare for more feature engineering----")
      if(!tolower(TimeGroups[1]) %chin% c("5min","10min","15min","30min","hour")) CalendarFeatures[, eval(DateColumnName) := data.table::as.IDate(get(DateColumnName))]

      # Update calendar variables----
      if(DebugMode) print("Update calendar variables----")
      if(!is.null(CalendarVariables)) {
        CalendarFeatures <- CreateCalendarVariables(
          data = CalendarFeatures,
          DateCols = eval(DateColumnName),
          AsFactor = FALSE,
          TimeUnits = CalendarVariables)
      }

      # Update Time Trend feature----
      if(DebugMode) print("Update Time Trend feature----")
      if(TimeTrendVariable) CalendarFeatures[, TimeTrend := N + 1]

      # Prepare data for scoring----
      if(DebugMode) print("Prepare data for scoring----")
      temp <- cbind(CalendarFeatures, 1)
      data.table::setnames(temp, c("V2"), c(eval(TargetColumnName)))
      if(any(class(UpdateData[[eval(DateColumnName)]]) %chin% c("POSIXct","POSIXt","IDate"))) UpdateData[, eval(DateColumnName) := as.Date(get(DateColumnName))]
      if(any(class(temp[[eval(DateColumnName)]]) %chin% c("POSIXct","POSIXt","IDate"))) temp[, eval(DateColumnName) := as.Date(get(DateColumnName))]
      UpdateData <- data.table::rbindlist(list(UpdateData, temp), fill = TRUE)

      # Update holiday feature----
      if(DebugMode) print("Update holiday feature----")
      if(!is.null(HolidayVariable)) {
        UpdateData <- CreateHolidayVariables(
          UpdateData,
          DateCols = eval(DateColumnName),
          LookbackDays = if(!is.null(HolidayLookback)) HolidayLookback else LB(TimeUnit),
          HolidayGroups = HolidayVariable,
          Holidays = NULL)
      }

      # Update Anomaly Detection ----
      if(i > 1) {
        if(!is.null(AnomalyDetection)) {
          UpdateData[, ":=" (AnomHigh = 0, AnomLow = 0)]
        }
      }

      # Update Lags and MA's----
      if(DebugMode) print("Update Lags and MA's----")

      # Group with or No Diff
      if(!is.null(GroupVariables) & Difference) {

        # Calendar and Holiday----
        if(!is.null(CalendarVariables)) CalVar <- TRUE else CalVar <- FALSE
        if(!is.null(HolidayVariable)) HolVar <- TRUE else HolVar <- FALSE

        # Create data for GDL----
        temp <- CarmaCatBoostKeepVarsGDL(
          IndepVarPassTRUE = NULL,
          data,UpdateData,CalendarFeatures,XREGS,Difference,HierarchGroups,GroupVariables,
          GroupVarVector,CalendarVariables=CalVar,HolidayVariable=HolVar,TargetColumnName,DateColumnName,Preds)
        Temporary <- temp$data
        keep <- temp$keep

        # Generate GDL Features for Updated Records----
        if(DebugMode) print("Generate GDL Features for Updated Records----")

        # Build Features
        Temporary <- AutoLagRollStatsScoring(

          # Data
          data                 = Temporary,
          RowNumsID            = "ID",
          RowNumsKeep          = 1,
          DateColumn           = eval(DateColumnName),
          Targets              = "ModTarget",
          HierarchyGroups      = HierarchSupplyValue,
          IndependentGroups    = IndependentSupplyValue,

          # Services
          TimeBetween          = NULL,
          TimeUnit             = TimeUnit,
          TimeUnitAgg          = TimeGroups[1],
          TimeGroups           = TimeGroups,
          RollOnLag1           = TRUE,
          Type                 = "Lag",
          SimpleImpute         = TRUE,

          # Calculated Columns
          Lags                 = Lags,
          MA_RollWindows       = MA_Periods,
          SD_RollWindows       = SD_Periods,
          Skew_RollWindows     = Skew_Periods,
          Kurt_RollWindows     = Kurt_Periods,
          Quantile_RollWindows = Quantile_Periods,
          Quantiles_Selected   = Quantiles_Selected,
          Debug                = TRUE)

        # Lag / Lead, MA Holiday Variables----
        if(DebugMode) print("Lag / Lead, MA Holiday Variables----")
        if(!is.null(HolidayVariable) & max(HolidayLags) > 0 & max(HolidayMovingAverages) > 0) {

          # Calendar and Holiday----
          if(!is.null(CalendarVariables)) CalVar <- TRUE else CalVar <- FALSE
          if(!is.null(HolidayVariable)) HolVar <- TRUE else HolVar <- FALSE

          # Create copy of data----
          temp <- CarmaCatBoostKeepVarsGDL(IndepVarPassTRUE = IndepentVariablesPass,
                                           data,UpdateData,CalendarFeatures,XREGS,Difference,HierarchGroups,GroupVariables,
                                           GroupVarVector,CalendarVariables=CalVar,HolidayVariable=HolVar,TargetColumnName,DateColumnName,Preds)
          Temporary1 <- temp$data
          keep <- temp$keep

          # Generate GDL Features for Updated Records----
          if(DebugMode) print("Generate GDL Features for Updated Records----")
          IndepentVariablesPass <- CARMA_Get_IndepentVariablesPass(HierarchGroups)

          # Generate GDL Features for Updated Records----
          if(DebugMode) print("Generate GDL Features for Updated Records----")
          Temporary1 <- AutoLagRollStatsScoring(

            # Data
            data                 = Temporary1,
            RowNumsID            = "ID",
            RowNumsKeep          = 1,
            DateColumn           = eval(DateColumnName),
            Targets              = "HolidayCounts",
            HierarchyGroups      = NULL,
            IndependentGroups    = IndepentVariablesPass,

            # Services
            TimeUnit             = TimeUnit,
            TimeUnitAgg          = TimeGroups[1],
            TimeGroups           = TimeGroups[1],
            TimeBetween          = NULL,
            RollOnLag1           = TRUE,
            Type                 = "Lag",
            SimpleImpute         = TRUE,

            # Calculated Columns
            Lags                 = HolidayLags,
            MA_RollWindows       = HolidayMovingAverages,
            SD_RollWindows       = NULL,
            Skew_RollWindows     = NULL,
            Kurt_RollWindows     = NULL,
            Quantile_RollWindows = NULL,
            Quantiles_Selected   = NULL)

          # Join Holiday Lags and Moving Averages back to UpdateData
          if(!"GroupVar" %chin% names(Temporary)) {
            keep <- c(eval(GroupVariables),eval(DateColumnName),setdiff(names(Temporary1), names(Temporary)))
            Temporary <- merge(Temporary, Temporary1[, .SD, .SDcols = c(keep)], by = c(eval(GroupVariables), eval(DateColumnName)), all = FALSE)
          } else {
            keep <- c("GroupVar",eval(DateColumnName),setdiff(names(Temporary1), names(Temporary)))
            Temporary <- merge(Temporary, Temporary1[, .SD, .SDcols = c(keep)], by = c("GroupVar", eval(DateColumnName)), all = FALSE)
          }
        }

        # Update data for scoring next iteration----
        if(DebugMode) print("Update data for scoring next iteration----")
        UpdateData <- data.table::rbindlist(list(UpdateData[ID != 1], Temporary), fill = TRUE, use.names = TRUE)

      }

      # Group and Diff
      if(!is.null(GroupVariables) & !Difference) {

        # Calendar and Holiday----
        if(!is.null(CalendarVariables)) CalVar <- TRUE else CalVar <- FALSE
        if(!is.null(HolidayVariable)) HolVar <- TRUE else HolVar <- FALSE

        # Create data for GDL----
        temp <- CarmaCatBoostKeepVarsGDL(
          IndepVarPassTRUE = NULL,
          data,UpdateData,CalendarFeatures,XREGS,Difference,HierarchGroups,GroupVariables,
          GroupVarVector,CalendarVariables=CalVar,HolidayVariable=HolVar,TargetColumnName,DateColumnName,Preds)
        Temporary <- temp$data
        keep <- temp$keep

        # Generate GDL Features for Updated Records----
        if(DebugMode) print("Generate GDL Features for Updated Records----")

        # Build Features
        Temporary <- AutoLagRollStatsScoring(

          # Data
          data                 = Temporary,
          RowNumsID            = "ID",
          RowNumsKeep          = 1,
          DateColumn           = eval(DateColumnName),
          Targets              = eval(TargetColumnName),
          HierarchyGroups      = HierarchSupplyValue,
          IndependentGroups    = IndependentSupplyValue,

          # Services
          TimeBetween          = NULL,
          TimeUnit             = TimeUnit,
          TimeUnitAgg          = TimeGroups[1],
          TimeGroups           = TimeGroups,
          RollOnLag1           = TRUE,
          Type                 = "Lag",
          SimpleImpute         = TRUE,

          # Calculated Columns
          Lags                 = Lags,
          MA_RollWindows       = MA_Periods,
          SD_RollWindows       = SD_Periods,
          Skew_RollWindows     = Skew_Periods,
          Kurt_RollWindows     = Kurt_Periods,
          Quantile_RollWindows = Quantile_Periods,
          Quantiles_Selected   = Quantiles_Selected,
          Debug                = DebugMode)

        # Lag / Lead, MA Holiday Variables----
        if(DebugMode) print("Lag / Lead, MA Holiday Variables----")
        if(!is.null(HolidayVariable) & max(HolidayLags) > 0 & max(HolidayMovingAverages) > 0) {

          # Calendar and Holiday----
          if(!is.null(CalendarVariables)) CalVar <- TRUE else CalVar <- FALSE
          if(!is.null(HolidayVariable)) HolVar <- TRUE else HolVar <- FALSE

          # Generate GDL Features for Updated Records----
          if(DebugMode) print("Generate GDL Features for Updated Records----")
          IndepentVariablesPass <- CARMA_Get_IndepentVariablesPass(HierarchGroups)

          # Create copy of data----
          temp <- CarmaCatBoostKeepVarsGDL(IndepVarPassTRUE = IndepentVariablesPass,
                                           data,UpdateData,CalendarFeatures,XREGS,Difference,HierarchGroups,GroupVariables,
                                           GroupVarVector,CalendarVariables=CalVar,HolidayVariable=HolVar,TargetColumnName,DateColumnName,Preds)
          Temporary1 <- temp$data
          keep <- temp$keep

          # Generate GDL Features for Updated Records----
          if(DebugMode) print("Generate GDL Features for Updated Records----")
          Temporary1 <- AutoLagRollStatsScoring(

            # Data
            data                 = Temporary1,
            RowNumsID            = "ID",
            RowNumsKeep          = 1,
            DateColumn           = eval(DateColumnName),
            Targets              = "HolidayCounts",
            HierarchyGroups      = NULL,
            IndependentGroups    = IndepentVariablesPass,

            # Services
            TimeUnit             = TimeUnit,
            TimeUnitAgg          = TimeGroups[1],
            TimeGroups           = TimeGroups[1],
            RollOnLag1           = TRUE,
            Type                 = "Lag",
            SimpleImpute         = TRUE,

            # Calculated Columns
            Lags                 = HolidayLags,
            MA_RollWindows       = HolidayMovingAverages[!HolidayMovingAverages %in% 1],
            SD_RollWindows       = NULL,
            Skew_RollWindows     = NULL,
            Kurt_RollWindows     = NULL,
            Quantile_RollWindows = NULL,
            Quantiles_Selected   = NULL)

          # Join Holiday Lags and Moving Averages back to UpdateData
          if(!"GroupVar" %chin% names(Temporary)) {
            keep <- c(eval(GroupVariables),eval(DateColumnName),setdiff(names(Temporary1), names(Temporary)))
            if(eval(DateColumnName) %chin% names(Temporary))
              Temporary <- merge(Temporary[, .SD, .SDcols = unique(names(Temporary))], Temporary1[, .SD, .SDcols = c(keep)], by = c(eval(GroupVariables), eval(DateColumnName)), all = FALSE)
          } else {
            keep <- c("GroupVar",eval(DateColumnName),setdiff(names(Temporary1), names(Temporary)))
            Temporary <- merge(Temporary[, .SD, .SDcols = unique(names(Temporary))], Temporary1[, .SD, .SDcols = c(keep)], by = c("GroupVar", eval(DateColumnName)), all = FALSE)
          }
        }

        # Update data for scoring next iteration----
        if(DebugMode) print("Update data for scoring next iteration----")
        UpdateData <- data.table::rbindlist(list(UpdateData[ID != 1], Temporary), fill = TRUE, use.names = TRUE)

      }

      # No Group with or without Diff
      if(is.null(GroupVariables)) {

        # Calendar and Holiday----
        if(!is.null(CalendarVariables)) CalVar <- TRUE else CalVar <- FALSE
        if(!is.null(HolidayVariable)) HolVar <- TRUE else HolVar <- FALSE

        # Create data for GDL----
        temp <- CarmaCatBoostKeepVarsGDL(
          IndepVarPassTRUE = NULL,
          data,UpdateData,CalendarFeatures,XREGS,Difference,HierarchGroups,GroupVariables,
          GroupVarVector,CalendarVariables=CalVar,HolidayVariable=HolVar,TargetColumnName,DateColumnName,Preds)
        Temporary <- temp$data
        keep <- temp$keep
        if("GroupVar" %chin% keep) keep <- keep[!keep %chin% "GroupVar"]

        # Generate GDL Features for Updated Records----
        if(DebugMode) print("Generate GDL Features for Updated Records----")
        Temporary <- AutoLagRollStatsScoring(

          # Data
          data                 = Temporary,
          RowNumsID            = "ID",
          RowNumsKeep          = 1,
          DateColumn           = eval(DateColumnName),
          Targets              = eval(TargetColumnName),
          HierarchyGroups      = NULL,
          IndependentGroups    = NULL,

          # Services
          TimeBetween          = NULL,
          TimeUnit             = TimeUnit,
          TimeUnitAgg          = TimeGroups[1],
          TimeGroups           = TimeGroups,
          RollOnLag1           = TRUE,
          Type                 = "Lag",
          SimpleImpute         = TRUE,

          # Calculated Columns
          Lags                 = Lags,
          MA_RollWindows       = MA_Periods,
          SD_RollWindows       = SD_Periods,
          Skew_RollWindows     = Skew_Periods,
          Kurt_RollWindows     = Kurt_Periods,
          Quantile_RollWindows = Quantile_Periods,
          Quantiles_Selected   = Quantiles_Selected)

        # Lag / Lead, MA Holiday Variables----
        if(DebugMode) print("Lag / Lead, MA Holiday Variables----")
        if(!is.null(HolidayVariable) & max(HolidayLags) > 0 & max(HolidayMovingAverages) > 0) {

          # Calendar and Holiday----
          if(!is.null(CalendarVariables)) CalVar <- TRUE else CalVar <- FALSE
          if(!is.null(HolidayVariable)) HolVar <- TRUE else HolVar <- FALSE

          # Copy data----
          temp <- CarmaCatBoostKeepVarsGDL(IndepVarPassTRUE = NULL,
                                           data,UpdateData,CalendarFeatures,XREGS,Difference,HierarchGroups,GroupVariables,
                                           GroupVarVector,CalendarVariables=CalVar,HolidayVariable=HolVar,TargetColumnName,DateColumnName,Preds)
          Temporary1 <- temp$data
          keep <- temp$keep

          # Generate GDL Features for Updated Records----
          if(DebugMode) print("Generate GDL Features for Updated Records----")
          Temporary1 <- AutoLagRollStatsScoring(

            # Data
            data                 = Temporary1,
            RowNumsID            = "ID",
            RowNumsKeep          = 1,
            DateColumn           = eval(DateColumnName),
            Targets              = "HolidayCounts",
            HierarchyGroups      = NULL,
            IndependentGroups    = NULL,

            # Services
            TimeUnit             = TimeUnit,
            TimeUnitAgg          = TimeGroups[1],
            TimeGroups           = TimeGroups[1],
            TimeBetween          = NULL,
            RollOnLag1           = TRUE,
            Type                 = "Lag",
            SimpleImpute         = TRUE,

            # Calculated Columns
            Lags                 = HolidayLags,
            MA_RollWindows       = HolidayMovingAverages[!HolidayMovingAverages %in% 1],
            SD_RollWindows       = NULL,
            Skew_RollWindows     = NULL,
            Kurt_RollWindows     = NULL,
            Quantile_RollWindows = NULL,
            Quantiles_Selected   = NULL)

          # Join Holiday Lags and Moving Averages back to UpdateData
          keep <- unique(c(eval(DateColumnName),setdiff(names(Temporary1), names(Temporary))))
          Temporary <- merge(Temporary, Temporary1[, .SD, .SDcols = c(keep)], by = c(eval(DateColumnName)), all = FALSE)
        }

        # Update data for scoring next iteration----
        if(DebugMode) print("Update data for scoring next iteration----")
        if(!"ID" %chin% c(names(UpdateData))) data.table::set(UpdateData, j = "ID", value = nrow(UpdateData):1L)
        UpdateData <- data.table::rbindlist(list(UpdateData[ID > 1L][, ID := NULL], Temporary), fill = TRUE, use.names = TRUE)
      }
    }
    gc()
  }

  #----

  #----

  # Remove duplicate date names----
  if(DebugMode) print("Remove duplicate date names----")
  if(sum(names(UpdateData) %chin% eval(DateColumnName)) > 1) {
    data.table::set(UpdateData, j = which(names(UpdateData) %chin% eval(DateColumnName))[2], value = NULL)
  }

  # Remove duplicate target names----
  if(DebugMode) print("Remove duplicate target names----")
  if(sum(names(UpdateData) %chin% eval(TargetColumnName)) > 1) {
    data.table::set(UpdateData, j = which(names(UpdateData) %chin% eval(TargetColumnName))[2], value = NULL)
  }

  # Reverse Difference----
  if(DebugMode) print("Reverse Difference----")
  if(is.null(GroupVariables) & Difference == TRUE) {
    UpdateData <- DifferenceDataReverse(
      data = UpdateData,
      ScoreData = NULL,
      CARMA = TRUE,
      TargetCol = eval(TargetColumnName),
      FirstRow = DiffTrainOutput$FirstRow[[eval(TargetColumnName)]],
      LastRow = NULL)
  } else if(!is.null(GroupVariables) & Difference == TRUE) {
    if(any(class(UpdateData$Date) %chin% c("POSIXct","POSIXt")) & any(class(dataStart$Date) == eval(DateColumnName))) {
      UpdateData[, eval(DateColumnName) := as.Date(get(DateColumnName))]
    }
    UpdateData <- data.table::rbindlist(list(dataStart,UpdateData), fill = TRUE)
    UpdateData <- UpdateData[, .SD, .SDcols = c(eval(DateColumnName),eval(TargetColumnName),"Predictions","GroupVar")]
    data.table::set(UpdateData, j = "Predictions", value = UpdateData[[eval(TargetColumnName)]])
  }

  # BackTransform----
  if(DebugMode) print("BackTransform----")
  if(TargetTransformation) {

    # Prepare transformobject----
    if(DebugMode) print("Prepare transformobject----")
    temptrans <- data.table::copy(TransformObject)
    data.table::set(TransformObject, i = 1L, j = "ColumnName", value = "Predictions")
    TransformObject <- data.table::rbindlist(list(temptrans,TransformObject))

    # Ensure positive values in case transformation method requires so----
    if(DebugMode) print("Ensure positive values in case transformation method requires so----")
    if(Difference) {
      if(!is.null(GroupVariables) & TrainOnFull == TRUE) {
        UpdateData[!get(DateColumnName) %in% FutureDateData, eval(TargetColumnName) := 1, by = "GroupVar"]
      } else if(TrainOnFull) {
        UpdateData[!get(DateColumnName) %in% FutureDateData, eval(TargetColumnName) := 1]
      }
    }

    # Backtrans----
    if(DebugMode) print("Backtrans----")
    UpdateData <- AutoTransformationScore(
      ScoringData = UpdateData,
      FinalResults = TransformObject,
      Type = "Inverse",
      TransID = NULL,
      Path = NULL)
  }

  # Update ValidationData and Create Metrics Data----
  if(DebugMode) print("Update ValidationData and Create Metrics Data----")
  if(!TrainOnFull) {
    if(!is.null(GroupVariables)) {

      # Sometimes GroupVar is passed, other times not, for both data sets----
      if(!is.null(HierarchGroups)) {
        x1 <- tryCatch({valid[, .SD, .SDcols = c(eval(TargetColumnName),eval(DateColumnName),eval(GroupVariables),"GroupVar")]}, error = function(x) {
          tryCatch({valid[, .SD, .SDcols = c(eval(TargetColumnName),eval(DateColumnName),eval(GroupVariables))]}, error = function(x) {
            tryCatch({valid[, .SD, .SDcols = c(eval(TargetColumnName),eval(DateColumnName),"GroupVar")]}, error = function(x) NULL)
          })
        })
        tryCatch({data.table::setkeyv(x1, cols = c(eval(GroupVariables),eval(DateColumnName)))}, error = function(x) NULL)
        x2 <- tryCatch({UpdateData[, .SD, .SDcols =  c("Predictions",eval(GroupVariables),eval(DateColumnName))]}, error = function(x) {
          tryCatch({UpdateData[, .SD, .SDcols =  c("Predictions","GroupVar",eval(DateColumnName))]}, error = function(x) NULL)
        })
        TestDataEval <- tryCatch({x2[x1, on = c(eval(GroupVariables), eval(DateColumnName))][!Predictions %in% c(NA,1)]}, error = function(x) {
          tryCatch({x2[x1, on = c("GroupVar",eval(DateColumnName))][!Predictions %in% c(NA,1)]})
        })
      } else {
        x1 <- tryCatch({valid[, .SD, .SDcols = c(eval(TargetColumnName),eval(DateColumnName),eval(GroupVariables))]}, error = function(x) {
          tryCatch({valid[, .SD, .SDcols = c(eval(TargetColumnName),eval(DateColumnName),"GroupVar")]}, error = function(x) NULL)
        })
        x2 <- tryCatch({UpdateData[, .SD, .SDcols =  c("Predictions",eval(GroupVariables),eval(DateColumnName))]}, error = function(x) {
          tryCatch({UpdateData[, .SD, .SDcols =  c("Predictions","GroupVar",eval(DateColumnName))]}, error = function(x) NULL)
        })
        data.table::setkeyv(x1, cols = c("GroupVar",eval(DateColumnName)))
        TestDataEval <- x2[x1, on = c("GroupVar", eval(DateColumnName))][!Predictions %in% c(NA,1)]
      }
    } else {
      x2 <- UpdateData[, .SD, .SDcols =  c("Predictions",eval(DateColumnName))]
      if(Difference) {
        TestDataEval <- merge(antidiff,x2, by = c(eval(DateColumnName)), all = FALSE)
        TestDataEval <- merge(TestDataEval,valid[, .SD, .SDcols = eval(DateColumnName)], by = eval(DateColumnName), all = FALSE)
      } else {
        TestDataEval <- merge(x2,valid[, .SD, .SDcols = c(eval(TargetColumnName),eval(DateColumnName))], by = eval(DateColumnName), all = FALSE)
      }
    }

    # Transform target----
    if(DebugMode) print("Transform target----")
    if(TargetTransformation) {
      TestDataEval <- AutoTransformationScore(
        ScoringData = TestDataEval,
        FinalResults = TransformObject[1],
        Type = "Inverse",
        TransID = NULL,
        Path = NULL)
    }

    # Collect Performance Metrics MSE, MAE, MAPE, R2----
    if(DebugMode) print("Collect Performance Metrics MSE, MAE, MAPE, R2----")
    MetricOutput <- tryCatch({CarmaHoldoutMetrics(
      DATA = TestDataEval,
      TARGETCOLUMNNAME = eval(TargetColumnName),
      GROUPVARIABLES = NULL)}, error = function(x) -1)
    TestModel[["EvaluationMetrics"]] <- MetricOutput

    # Update GroupVar with Original Columns, reorder columns, add to model objects----
    if(DebugMode) print("Update GroupVar with Original Columns, reorder columns, add to model objects----")
    if(!is.null(GroupVariables)) {
      MetricOutputByGroup <- tryCatch({CarmaHoldoutMetrics(
        DATA = TestDataEval,
        TARGETCOLUMNNAME = eval(TargetColumnName),
        GROUPVARIABLES = "GroupVar")}, error = function(x) -1)

      # Convert GroupVar back to original columns----
      if(DebugMode) print("Convert GroupVar back to original columns----")
      if(length(GroupVariables) > 1 & data.table::is.data.table(MetricOutputByGroup) == TRUE) {
        if("GroupVar" %chin% names(MetricOutputByGroup)) {
          tryCatch({data.table::set(MetricOutputByGroup, j = c(eval(GroupVariables)), value = data.table::tstrsplit(MetricOutputByGroup[["GroupVar"]], " "))}, error = function(x) NULL)
          data.table::set(MetricOutputByGroup, j = "GroupVar", value = NULL)
        }
      } else if(data.table::is.data.table(MetricOutputByGroup)) {
        if(GroupVariables != "GroupVar") {
          MetricOutputByGroup[, eval(GroupVariables) := GroupVar][, GroupVar := NULL]
        }
      }
      TestModel[["EvaluationMetricsByGroup"]] <- MetricOutputByGroup
    }

    # Return if TrainOnFull is FALSE----
    if(DebugMode) print("Return if TrainOnFull is FALSE----")
    if(TargetTransformation) {
      return(list(ModelInformation = TestModel, TransformationDetail = TransformObject))
    } else {
      return(list(ModelInformation = TestModel))
    }
  }

  # Remove target variables values on FC periods----
  if(DebugMode) print("Remove target variables values on FC periods----")
  if(!is.null(GroupVariables)) {
    UpdateData[!get(DateColumnName) %in% FutureDateData, eval(TargetColumnName) := NA, by = "GroupVar"]
  } else {
    UpdateData[!get(DateColumnName) %in% FutureDateData, eval(TargetColumnName) := NA]
  }

  # Shutdown h2o ----
  try(h2o::h2o.shutdown(prompt = FALSE), silent = TRUE)

  # Return data----
  if(DebugMode) print("Return data----")
  if(!is.null(GroupVariables)) {
    keep <- c("GroupVar",eval(DateColumnName),eval(TargetColumnName),"Predictions")
    UpdateData <- UpdateData[, ..keep]
    if(length(GroupVariables) > 1) {
      UpdateData[, eval(GroupVariables) := data.table::tstrsplit(GroupVar, " ")][, GroupVar := NULL]
    } else {
      UpdateData[, eval(GroupVariables) := GroupVar][, GroupVar := NULL]
    }
    if(TargetTransformation) {
      return(
        list(
          Forecast = UpdateData,
          ModelInformation = TestModel,
          TransformationDetail = TransformObject))
    } else {
      return(
        list(
          Forecast = UpdateData,
          ModelInformation = TestModel))
    }
  } else {
    if(TargetTransformation) {
      return(
        list(
          Forecast = UpdateData,
          ModelInformation = TestModel,
          TransformationDetail = TransformObject))
    } else {
      return(
        list(
          Forecast = UpdateData,
          ModelInformation = TestModel))
    }
  }
}
