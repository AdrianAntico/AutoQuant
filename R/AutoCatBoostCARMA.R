#' @title AutoCatBoostCARMA
#'
#' @description AutoCatBoostCARMA Mutlivariate Forecasting with calendar variables, Holiday counts, holiday lags, holiday moving averages, differencing, transformations, interaction-based categorical encoding using target variable and features to generate various time-based aggregated lags, moving averages, moving standard deviations, moving skewness, moving kurtosis, moving quantiles, parallelized interaction-based fourier pairs by grouping variables, and Trend Variables.
#'
#' @author Adrian Antico
#' @family Automated Panel Data Forecasting
#'
#' @param data Supply your full series data set here
#' @param TrainOnFull Set to TRUE to train on full data
#' @param TargetColumnName List the column name of your target variables column. E.g. 'Target'
#' @param NonNegativePred TRUE or FALSE
#' @param RoundPreds Rounding predictions to an integer value. TRUE or FALSE. Defaults to FALSE
#' @param DateColumnName List the column name of your date column. E.g. 'DateTime'
#' @param GroupVariables Defaults to NULL. Use NULL when you have a single series. Add in GroupVariables when you have a series for every level of a group or multiple groups.
#' @param HierarchGroups Vector of hierachy categorical columns.
#' @param TimeWeights Supply a value that will be multiplied by he time trend value
#' @param TimeUnit List the time unit your data is aggregated by. E.g. '1min', '5min', '10min', '15min', '30min', 'hour', 'day', 'week', 'month', 'quarter', 'year'.
#' @param TimeGroups Select time aggregations for adding various time aggregated GDL features.
#' @param FC_Periods Set the number of periods you want to have forecasts for. E.g. 52 for weekly data to forecast a year ahead
#' @param PDFOutputPath NULL or a path file to output PDFs to a specified folder
#' @param SaveDataPath NULL Or supply a path. Data saved will be called 'ModelID'_data.csv
#' @param TargetTransformation TRUE or FALSE. If TRUE, select the methods in the Methods arg you want tested. The best one will be applied.
#' @param Methods Choose from 'YeoJohnson', 'BoxCox', 'Asinh', 'Log', 'LogPlus1', 'Sqrt', 'Asin', or 'Logit'. If more than one is selected, the one with the best normalization pearson statistic will be used. Identity is automatically selected and compared.
#' @param XREGS Additional data to use for model development and forecasting. Data needs to be a complete series which means both the historical and forward looking values over the specified forecast window needs to be supplied.
#' @param Timer Set to FALSE to turn off the updating print statements for progress
#' @param DebugMode Defaults to FALSE. Set to TRUE to get a print statement of each high level comment in function
#' @param Lags Select the periods for all lag variables you want to create. E.g. c(1:5,52) or list('day' = c(1:10), 'weeks' = c(1:4))
#' @param MA_Periods Select the periods for all moving average variables you want to create. E.g. c(1:5,52) or list('day' = c(2:10), 'weeks' = c(2:4))
#' @param SD_Periods Select the periods for all moving standard deviation variables you want to create. E.g. c(1:5,52) or list('day' = c(2:10), 'weeks' = c(2:4))
#' @param Skew_Periods Select the periods for all moving skewness variables you want to create. E.g. c(1:5,52) or list('day' = c(2:10), 'weeks' = c(2:4))
#' @param Kurt_Periods Select the periods for all moving kurtosis variables you want to create. E.g. c(1:5,52) or list('day' = c(2:10), 'weeks' = c(2:4))
#' @param Quantile_Periods Select the periods for all moving quantiles variables you want to create. E.g. c(1:5,52) or list('day' = c(2:10), 'weeks' = c(2:4))
#' @param Quantiles_Selected Select from the following 'q5', 'q10', 'q15', 'q20', 'q25', 'q30', 'q35', 'q40', 'q45', 'q50', 'q55', 'q60', 'q65', 'q70', 'q75', 'q80', 'q85', 'q90', 'q95'
#' @param AnomalyDetection NULL for not using the service. Other, provide a list, e.g. AnomalyDetection = list('tstat_high' = 4, 'tstat_low' = -4)
#' @param Difference Puts the I in ARIMA for single series and grouped series.
#' @param FourierTerms Set to the max number of pairs. E.g. 2 means to generate two pairs for by each group level and interations if hierarchy is enabled.
#' @param CalendarVariables NULL, or select from 'minute', 'hour', 'wday', 'mday', 'yday', 'week', 'isoweek', 'month', 'quarter', 'year'
#' @param HolidayVariable NULL, or select from 'USPublicHolidays', 'EasterGroup', 'ChristmasGroup', 'OtherEcclesticalFeasts'
#' @param HolidayLookback Number of days in range to compute number of holidays from a given date in the data. If NULL, the number of days are computed for you.
#' @param HolidayLags Number of lags to build off of the holiday count variable.
#' @param HolidayMovingAverages Number of moving averages to build off of the holiday count variable.
#' @param TimeTrendVariable Set to TRUE to have a time trend variable added to the model. Time trend is numeric variable indicating the numeric value of each record in the time series (by group). Time trend starts at 1 for the earliest point in time and increments by one for each success time point.
#' @param DataTruncate Set to TRUE to remove records with missing values from the lags and moving average features created
#' @param ZeroPadSeries NULL to do nothing. Otherwise, set to 'maxmax', 'minmax', 'maxmin', 'minmin'. See \code{\link{TimeSeriesFill}} for explanations of each type
#' @param PartitionType Select 'random' for random data partitioning 'timeseries' for partitioning by time frames
#' @param SplitRatios E.g c(0.7,0.2,0.1) for train, validation, and test sets
#' @param NumOfParDepPlots Supply a number for the number of partial dependence plots you want returned
#' @param EvalMetric Select from 'RMSE', 'MAE', 'MAPE', 'Poisson', 'Quantile', 'LogLinQuantile', 'Lq', 'NumErrors', 'SMAPE', 'R2', 'MSLE', 'MedianAbsoluteError'
#' @param EvalMetricValue Used when EvalMetric accepts an argument. See \code{\link{AutoCatBoostRegression}}
#' @param LossFunction Used in model training for model fitting. Select from 'RMSE', 'MAE', 'Quantile', 'LogLinQuantile', 'MAPE', 'Poisson', 'PairLogitPairwise', 'Tweedie', 'QueryRMSE'
#' @param LossFunctionValue Used when LossFunction accepts an argument. See \code{\link{AutoCatBoostRegression}}
#' @param TaskType Default is 'GPU' but you can also set it to 'CPU'
#' @param NumGPU Defaults to 1. If CPU is set this argument will be ignored.
#' @param GridTune Set to TRUE to run a grid tune
#' @param PassInGrid Defaults to NULL
#' @param ModelCount Set the number of models to try in the grid tune
#' @param MaxRunsWithoutNewWinner Default is 50
#' @param MaxRunMinutes Default is 60*60
#' @param Langevin Enables the Stochastic Gradient Langevin Boosting mode. If TRUE and TaskType == 'GPU' then TaskType will be converted to 'CPU'
#' @param DiffusionTemperature Default is 10000
#' @param NTrees Select the number of trees you want to have built to train the model
#' @param Depth Depth of catboost model
#' @param L2_Leaf_Reg l2 reg parameter
#' @param LearningRate Defaults to NULL. Catboost will dynamically define this if L2_Leaf_Reg is NULL and RMSE is chosen (otherwise catboost will default it to 0.03). Then you can pull it out of the model object and pass it back in should you wish.
#' @param RandomStrength Default is 1
#' @param BorderCount Default is 254
#' @param RSM CPU only. If TaskType is GPU then RSM will not be used
#' @param BootStrapType If NULL, then if TaskType is GPU then Bayesian will be used. If CPU then MVS will be used. If MVS is selected when TaskType is GPU, then BootStrapType will be switched to Bayesian
#' @param GrowPolicy Default is SymmetricTree. Others include Lossguide and Depthwise
#' @param ModelSizeReg Defaults to 0.5. Set to 0 to allow for bigger models. This is for models with high cardinality categorical features. Valuues greater than 0 will shrink the model and quality will decline but models won't be huge.
#' @param FeatureBorderType Defaults to 'GreedyLogSum'. Other options include: Median, Uniform, UniformAndQuantiles, MaxLogSum, MinEntropy
#' @param SamplingUnit Default is Group. Other option is Object. if GPU is selected, this will be turned off unless the loss_function is YetiRankPairWise
#' @param SubSample Can use if BootStrapType is neither Bayesian nor No. Pass NULL to use Catboost default. Used for bagging.
#' @param ScoreFunction Default is Cosine. CPU options are Cosine and L2. GPU options are Cosine, L2, NewtonL2, and NewtomCosine (not available for Lossguide)
#' @param MinDataInLeaf Defaults to 1. Used if GrowPolicy is not SymmetricTree
#' @examples
#' \dontrun{
#'
#' # Set up your output file path for saving results as a .csv
#' Path <- 'C:/YourPathHere'
#'
#' # Run on GPU or CPU (some options in the grid tuning force usage of CPU for some runs)
#' TaskType = 'GPU'
#'
#' # Define number of CPU threads to allow data.table to utilize
#' data.table::setDTthreads(percent = max(1L, parallel::detectCores()-2L))
#'
#' # Load data
#' data <- data.table::fread('https://www.dropbox.com/s/2str3ek4f4cheqi/walmart_train.csv?dl=1')
#'
#' # Ensure series have no missing dates (also remove series with more than 25% missing values)
#' data <- RemixAutoML::TimeSeriesFill(
#'   data,
#'   DateColumnName = 'Date',
#'   GroupVariables = c('Store','Dept'),
#'   TimeUnit = 'weeks',
#'   FillType = 'maxmax',
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
#' xregs <- data[, .SD, .SDcols = c('Date', 'Store', 'Dept')]
#'
#' # Change data types
#' data[, ':=' (Store = as.character(Store), Dept = as.character(Dept))]
#' xregs[, ':=' (Store = as.character(Store), Dept = as.character(Dept))]
#'
#' # Subset data so we have an out of time sample
#' data1 <- data.table::copy(data[, ID := 1L:.N, by = c('Store','Dept')][ID <= 125L][, ID := NULL])
#' data[, ID := NULL]
#'
#' # Define values for SplitRatios and FCWindow Args
#' N1 <- data1[, .N, by = c('Store','Dept')][1L, N]
#' N2 <- xregs[, .N, by = c('Store','Dept')][1L, N]
#'
#' # Setup Grid Tuning & Feature Tuning data.table using a cross join of vectors
#' Tuning <- data.table::CJ(
#'   TimeWeights = c('None',0.999),
#'   MaxTimeGroups = c('weeks','months'),
#'   TargetTransformation = c('TRUE','FALSE'),
#'   Difference = c('TRUE','FALSE'),
#'   HoldoutTrain = c(6,18),
#'   Langevin = c('TRUE','FALSE'),
#'   NTrees = c(2500,5000),
#'   Depth = c(6,9),
#'   RandomStrength = c(0.75,1),
#'   L2_Leaf_Reg = c(3.0,4.0),
#'   RSM = c(0.75,'NULL'),
#'   GrowPolicy = c('SymmetricTree','Lossguide','Depthwise'),
#'   BootStrapType = c('Bayesian','MVS','No'))
#'
#' # Remove options that are not compatible with GPU (skip over this otherwise)
#' Tuning <- Tuning[Langevin == 'TRUE' | (Langevin == 'FALSE' & RSM == 'NULL' & BootStrapType %in% c('Bayesian','No'))]
#'
#' # Randomize order of Tuning data.table
#' Tuning <- Tuning[order(runif(.N))]
#'
#' # Load grid results and remove rows that have already been tested
#' if(file.exists(file.path(Path, 'Walmart_CARMA_Metrics.csv'))) {
#'   Metrics <- data.table::fread(file.path(Path, 'Walmart_CARMA_Metrics.csv'))
#'   temp <- data.table::rbindlist(list(Metrics,Tuning), fill = TRUE)
#'   temp <- unique(temp, by = c(4:(ncol(temp)-1)))
#'   Tuning <- temp[is.na(RunTime)][, .SD, .SDcols = names(Tuning)]
#'   rm(Metrics,temp)
#' }
#'
#' # Define the total number of runs
#' TotalRuns <- Tuning[,.N]
#'
#' # Kick off feature + grid tuning
#' for(Run in seq_len(TotalRuns)) {
#'
#'   # Print run number
#'   for(zz in seq_len(100)) print(Run)
#'
#'   # Use fresh data for each run
#'   xregs_new <- data.table::copy(xregs)
#'   data_new <- data.table::copy(data1)
#'
#'   # Timer start
#'   StartTime <- Sys.time()
#'
#'   # Run carma system
#'   CatBoostResults <- RemixAutoML::AutoCatBoostCARMA(
#'
#'     # data args
#'     data = data_new,
#'     TimeWeights = if(Tuning[Run, TimeWeights] == 'None') NULL else as.numeric(Tuning[Run, TimeWeights]),
#'     TargetColumnName = 'Weekly_Sales',
#'     DateColumnName = 'Date',
#'     HierarchGroups = NULL,
#'     GroupVariables = c('Store','Dept'),
#'     TimeUnit = 'weeks',
#'     TimeGroups = if(Tuning[Run, MaxTimeGroups] == 'weeks') 'weeks' else if(Tuning[Run, MaxTimeGroups] == 'months') c('weeks','months') else c('weeks','months','quarters'),
#'
#'     # Production args
#'     TrainOnFull = TRUE,
#'     SplitRatios = c(1 - Tuning[Run, HoldoutTrain] / N2, Tuning[Run, HoldoutTrain] / N2),
#'     PartitionType = 'random',
#'     FC_Periods = N2-N1,
#'     TaskType = TaskType,
#'     NumGPU = 1,
#'     Timer = TRUE,
#'     DebugMode = TRUE,
#'
#'     # Target variable transformations
#'     TargetTransformation = as.logical(Tuning[Run, TargetTransformation]),
#'     Methods = c('YeoJohnson', 'BoxCox', 'Asinh', 'Log', 'LogPlus1', 'Sqrt', 'Asin', 'Logit'),
#'     Difference = as.logical(Tuning[Run, Difference]),
#'     NonNegativePred = TRUE,
#'     RoundPreds = FALSE,
#'
#'     # Calendar-related features
#'     CalendarVariables = c('week','wom','month','quarter'),
#'     HolidayVariable = c('USPublicHolidays'),
#'     HolidayLookback = NULL,
#'     HolidayLags = c(1,2,3),
#'     HolidayMovingAverages = c(2,3),
#'
#'     # Lags, moving averages, and other rolling stats
#'     Lags = if(Tuning[Run, MaxTimeGroups] == 'weeks') c(1,2,3,4,5,8,9,12,13,51,52,53) else if(Tuning[Run, MaxTimeGroups] == 'months') list('weeks' = c(1,2,3,4,5,8,9,12,13,51,52,53), 'months' = c(1,2,6,12)) else list('weeks' = c(1,2,3,4,5,8,9,12,13,51,52,53), 'months' = c(1,2,6,12), 'quarters' = c(1,2,3,4)),
#'     MA_Periods = if(Tuning[Run, MaxTimeGroups] == 'weeks') c(2,3,4,5,8,9,12,13,51,52,53) else if(Tuning[Run, MaxTimeGroups] == 'months') list('weeks' = c(2,3,4,5,8,9,12,13,51,52,53), 'months' = c(2,6,12)) else list('weeks' = c(2,3,4,5,8,9,12,13,51,52,53), 'months' = c(2,6,12), 'quarters' = c(2,3,4)),
#'     SD_Periods = NULL,
#'     Skew_Periods = NULL,
#'     Kurt_Periods = NULL,
#'     Quantile_Periods = NULL,
#'     Quantiles_Selected = NULL,
#'
#'     # Bonus features
#'     AnomalyDetection = NULL,
#'     XREGS = xregs_new,
#'     FourierTerms = 0,
#'     TimeTrendVariable = TRUE,
#'     ZeroPadSeries = NULL,
#'     DataTruncate = FALSE,
#'
#'     # ML grid tuning args
#'     GridTune = FALSE,
#'     PassInGrid = NULL,
#'     ModelCount = 5,
#'     MaxRunsWithoutNewWinner = 50,
#'     MaxRunMinutes = 60*60,
#'
#'     # ML evaluation output
#'     PDFOutputPath = NULL,
#'     SaveDataPath = NULL,
#'     NumOfParDepPlots = 0L,
#'
#'     # ML loss functions
#'     EvalMetric = 'RMSE',
#'     EvalMetricValue = 1,
#'     LossFunction = 'RMSE',
#'     LossFunctionValue = 1,
#'
#'     # ML tuning args
#'     NTrees = Tuning[Run, NTrees],
#'     Depth = Tuning[Run, Depth],
#'     L2_Leaf_Reg = Tuning[Run, L2_Leaf_Reg],
#'     LearningRate = 0.03,
#'     Langevin = as.logical(Tuning[Run, Langevin]),
#'     DiffusionTemperature = 10000,
#'     RandomStrength = Tuning[Run, RandomStrength],
#'     BorderCount = 254,
#'     RSM = if(Tuning[Run, RSM] == 'NULL') NULL else as.numeric(Tuning[Run, RSM]),
#'     GrowPolicy = Tuning[Run, GrowPolicy],
#'     BootStrapType = Tuning[Run, BootStrapType],
#'     ModelSizeReg = 0.5,
#'     FeatureBorderType = 'GreedyLogSum',
#'     SamplingUnit = 'Group',
#'     SubSample = NULL,
#'     ScoreFunction = 'Cosine',
#'     MinDataInLeaf = 1)
#'
#'   # Timer End
#'   EndTime <- Sys.time()
#'
#'   # Prepare data for evaluation
#'   Results <- CatBoostResults$Forecast
#'   data.table::setnames(Results, 'Weekly_Sales', 'bla')
#'   Results <- merge(Results, data, by = c('Store','Dept','Date'), all = FALSE)
#'   Results <- Results[is.na(bla)][, bla := NULL]
#'
#'   # Create totals and subtotals
#'   Results <- data.table::groupingsets(
#'     x = Results,
#'     j = list(Predictions = sum(Predictions), Weekly_Sales = sum(Weekly_Sales)),
#'     by = c('Date', 'Store', 'Dept'),
#'     sets = list(c('Date', 'Store', 'Dept'), c('Store', 'Dept'), 'Store', 'Dept', 'Date'))
#'
#'   # Fill NAs with 'Total' for totals and subtotals
#'   for(cols in c('Store','Dept')) Results[, eval(cols) := data.table::fifelse(is.na(get(cols)), 'Total', get(cols))]
#'
#'   # Add error measures
#'   Results[, Weekly_MAE := abs(Weekly_Sales - Predictions)]
#'   Results[, Weekly_MAPE := Weekly_MAE / Weekly_Sales]
#'
#'   # Weekly results
#'   Weekly_MAPE <- Results[, list(Weekly_MAPE = mean(Weekly_MAPE)), by = list(Store,Dept)]
#'
#'   # Monthly results
#'   temp <- data.table::copy(Results)
#'   temp <- temp[, Date := lubridate::floor_date(Date, unit = 'months')]
#'   temp <- temp[, lapply(.SD, sum), by = c('Date','Store','Dept'), .SDcols = c('Predictions', 'Weekly_Sales')]
#'   temp[, Monthly_MAE := abs(Weekly_Sales - Predictions)]
#'   temp[, Monthly_MAPE := Monthly_MAE / Weekly_Sales]
#'   Monthly_MAPE <- temp[, list(Monthly_MAPE = mean(Monthly_MAPE)), by = list(Store,Dept)]
#'
#'   # Collect metrics for Total (feel free to switch to something else or no filter at all)
#'   Metrics <- data.table::data.table(
#'     RunNumber = Run,
#'     Total_Weekly_MAPE = Weekly_MAPE[Store == 'Total' & Dept == 'Total', Weekly_MAPE],
#'     Total_Monthly_MAPE = Monthly_MAPE[Store == 'Total' & Dept == 'Total', Monthly_MAPE],
#'     Tuning[Run],
#'     RunTime = EndTime - StartTime)
#'
#'   # Append to file (not overwrite)
#'   data.table::fwrite(Metrics, file = file.path(Path, 'Walmart_CARMA_Metrics.csv'), append = TRUE)
#'
#'   # Remove objects (clear space before new runs)
#'   rm(CatBoostResults, Results, temp, Weekly_MAE, Weekly_MAPE, Monthly_MAE, Monthly_MAPE)
#'
#'   # Garbage collection because of GPU
#'   gc()
#' }
#' }
#' @return See examples
#' @export
AutoCatBoostCARMA <- function(data,
                              TimeWeights = NULL,
                              NonNegativePred = FALSE,
                              RoundPreds = FALSE,
                              TrainOnFull = FALSE,
                              TargetColumnName = 'Target',
                              DateColumnName = 'DateTime',
                              HierarchGroups = NULL,
                              GroupVariables = NULL,
                              FC_Periods = 30,
                              TimeUnit = 'week',
                              TimeGroups = c('weeks','months'),
                              PDFOutputPath = NULL,
                              SaveDataPath = NULL,
                              NumOfParDepPlots = 10L,
                              TargetTransformation = FALSE,
                              Methods = c('BoxCox', 'Asinh', 'Log', 'LogPlus1', 'Sqrt', 'Asin', 'Logit'),
                              AnomalyDetection = NULL,
                              XREGS = NULL,
                              Lags = c(1L:5L),
                              MA_Periods = c(2L:5L),
                              SD_Periods = NULL,
                              Skew_Periods = NULL,
                              Kurt_Periods = NULL,
                              Quantile_Periods = NULL,
                              Quantiles_Selected = c('q5','q95'),
                              Difference = TRUE,
                              FourierTerms = 6L,
                              CalendarVariables = c('minute', 'hour', 'wday', 'mday', 'yday', 'week', 'isoweek', 'month', 'quarter', 'year'),
                              HolidayVariable = c('USPublicHolidays','EasterGroup','ChristmasGroup','OtherEcclesticalFeasts'),
                              HolidayLookback = NULL,
                              HolidayLags = 1L,
                              HolidayMovingAverages = 1L:2L,
                              TimeTrendVariable = FALSE,
                              ZeroPadSeries = NULL,
                              DataTruncate = FALSE,
                              SplitRatios = c(0.7, 0.2, 0.1),
                              PartitionType = 'timeseries',
                              TaskType = 'GPU',
                              NumGPU = 1,
                              DebugMode = FALSE,
                              Timer = TRUE,
                              EvalMetric = 'RMSE',
                              EvalMetricValue = 1.5,
                              LossFunction = 'RMSE',
                              LossFunctionValue = 1.5,
                              GridTune = FALSE,
                              PassInGrid = NULL,
                              ModelCount = 100,
                              MaxRunsWithoutNewWinner = 50,
                              MaxRunMinutes = 24L*60L,
                              Langevin = FALSE,
                              DiffusionTemperature = 10000,
                              NTrees = 1000,
                              L2_Leaf_Reg = NULL,
                              LearningRate = NULL,
                              RandomStrength = 1,
                              BorderCount = 254,
                              Depth = 6,
                              RSM = 1,
                              BootStrapType = 'Bayesian',
                              GrowPolicy = 'SymmetricTree',
                              ModelSizeReg = 0.5,
                              FeatureBorderType = 'GreedyLogSum',
                              SamplingUnit = 'Group',
                              SubSample = NULL,
                              ScoreFunction = 'Cosine',
                              MinDataInLeaf = 1) {
  # Load catboost ----
  loadNamespace(package = 'catboost')

  # Args checking ----
  if(DebugMode) print('# Purified args: see CARMA HELPER FUNCTIONS----')
  Args <- CARMA_Define_Args(TimeUnit=TimeUnit, TimeGroups=TimeGroups, HierarchGroups=HierarchGroups, GroupVariables=GroupVariables, FC_Periods=FC_Periods, PartitionType=PartitionType, TrainOnFull=TrainOnFull, SplitRatios=SplitRatios, SD_Periods=SD_Periods, Skew_Periods=Skew_Periods, Kurt_Periods=Kurt_Periods, Quantile_Periods=Quantile_Periods, TaskType=TaskType, BootStrapType=BootStrapType, GrowPolicy=GrowPolicy, TimeWeights=TimeWeights, HolidayLookback=HolidayLookback, Difference=Difference, NonNegativePred=NonNegativePred)
  IndepentVariablesPass <- Args$IndepentVariablesPass
  NonNegativePred <- Args$NonNegativePred
  HolidayLookback <- Args$HolidayLookback
  HoldOutPeriods <- Args$HoldOutPeriods
  HierarchGroups <- Args$HierarchGroups
  GroupVariables <- Args$GroupVariables
  BootStrapType <- Args$BootStrapType
  TimeWeights <- Args$TimeWeights
  TimeGroups <- Args$TimeGroups
  FC_Periods <- Args$FC_Periods
  GrowPolicy <- Args$GrowPolicy
  Difference <- Args$Difference
  TimeGroup <- Args$TimeGroupPlaceHolder
  TimeUnit <- Args$TimeUnit
  TaskType <- Args$TaskType; rm(Args)

  # Grab all official parameters and their evaluated arguments
  ArgsList <- c(as.list(environment()))

  # Convert data to data.table ----
  if(!data.table::is.data.table(data)) data.table::setDT(data)
  if(!is.null(XREGS) && !data.table::is.data.table(XREGS)) data.table::setDT(XREGS)

  # Variables for Program: Redefine HoldOutPerids ----
  if(!TrainOnFull) HoldOutPeriods <- round(SplitRatios[2L] * length(unique(data[[eval(DateColumnName)]])), 0L)

  # Feature Engineering: Add Zero Padding for missing dates ----
  if(DebugMode) print('Feature Engineering: Add Zero Padding for missing dates----')
  if(data[, .N] != unique(data)[, .N]) stop('There is duplicates in your data')
  if(!is.null(ZeroPadSeries)) {
    data <- TimeSeriesFill(data, DateColumnName=eval(DateColumnName), GroupVariables=GroupVariables, TimeUnit=TimeUnit, FillType=ZeroPadSeries, MaxMissingPercent=0.0, SimpleImpute=FALSE)
    data <- ModelDataPrep(data=data, Impute=TRUE, CharToFactor=FALSE, FactorToChar=FALSE, IntToNumeric=FALSE, LogicalToBinary=FALSE, DateToChar=FALSE, RemoveDates=FALSE, MissFactor='0', MissNum=0, IgnoreCols=NULL)
  } else {
    temp <- TimeSeriesFill(data, DateColumnName=eval(DateColumnName), GroupVariables=GroupVariables, TimeUnit=TimeUnit, FillType='maxmax', MaxMissingPercent=0.25, SimpleImpute=FALSE)
    if(temp[,.N] != data[,.N]) stop('There are missing dates in your series. You can utilize the ZeroPadSeries argument to handle this or manage it before running the function')
  }

  # Modify FC_Periods ----
  if(DebugMode) print('# Check lengths of XREGS')
  Output <- CarmaFCHorizon(data.=data, XREGS.=XREGS, TrainOnFull.=TrainOnFull, Difference.= Difference, FC_Periods.=FC_Periods, HoldOutPeriods.=HoldOutPeriods, DateColumnName.=DateColumnName)
  FC_Periods <- Output$FC_Periods
  HoldOutPeriods <- Output$HoldOutPeriods; rm(Output)

  # Merge data and XREG for Training ----
  if(DebugMode) print('merging xregs to data')
  if(!is.null(XREGS)) {
    Output <- CarmaMergeXREGS(data.=data, XREGS.=XREGS, TargetColumnName.=TargetColumnName, GroupVariables.=GroupVariables, DateColumnName.=DateColumnName)
    data <- Output$data; Output$data <- NULL
    XREGS <- Output$XREGS; rm(Output)
  }

  # Set Keys for data.table usage ----
  if(DebugMode) print('# Set Keys for data.table usage ----')
  if(!is.null(GroupVariables)) {
    data.table::setkeyv(x = data, cols = c(eval(GroupVariables), eval(DateColumnName)))
    if(!is.null(XREGS)) data.table::setkeyv(x = XREGS, cols = c('GroupVar', eval(DateColumnName)))
  } else {
    data.table::setkeyv(x = data, cols = c(eval(DateColumnName)))
    if(!is.null(XREGS)) data.table::setkeyv(x = XREGS, cols = c(eval(DateColumnName)))
  }

  # Data Wrangling: Remove Unnecessary Columns ----
  if(DebugMode) print('Data Wrangling: Remove Unnecessary Columns ----')
  data <- CarmaSubsetColumns(data.=data, XREGS.=XREGS, GroupVariables.=GroupVariables, DateColumnName.=DateColumnName, TargetColumnName.=TargetColumnName)

  # Feature Engineering: Concat Categorical Columns - easier to deal with this way ----
  if(DebugMode) print('Feature Engineering: Concat Categorical Columns - easier to deal with this way ----')
  if(!is.null(GroupVariables)) {
    data[, GroupVar := do.call(paste, c(.SD, sep = ' ')), .SDcols = GroupVariables]
    if(length(GroupVariables) > 1L) data[, eval(GroupVariables) := NULL] else if(GroupVariables != 'GroupVar') data[, eval(GroupVariables) := NULL]
  }

  # Variables for Program: Store unique values of GroupVar in GroupVarVector ----
  if(DebugMode) print('Variables for Program: Store unique values of GroupVar in GroupVarVector ----')
  if(!is.null(GroupVariables)) {
    GroupVarVector <- data.table::as.data.table(x = unique(as.character(data[['GroupVar']])))
    data.table::setnames(GroupVarVector, 'V1', 'GroupVar')
  }

  # Data Wrangling: Standardize column ordering ----
  if(DebugMode) print('Data Wrangling: Standardize column ordering ----')
  if(!is.null(GroupVariables)) data.table::setcolorder(data, c('GroupVar', eval(DateColumnName), eval(TargetColumnName))) else data.table::setcolorder(data, c(eval(DateColumnName), eval(TargetColumnName)))

  # Data Wrangling: Convert DateColumnName to Date or POSIXct ----
  if(DebugMode) print('Data Wrangling: Convert DateColumnName to Date or POSIXct ----')
  Output <- CarmaDateStandardize(data.=data, XREGS.=XREGS, DateColumnName.=DateColumnName, TimeUnit.=TimeUnit)
  data <- Output$data; Output$data <- NULL
  XREGS <- Output$XREGS; rm(Output)

  # Data Wrangling: Ensure TargetColumnName is Numeric ----
  if(DebugMode) print('Data Wrangling: Ensure TargetColumnName is Numeric ----')
  if(!is.numeric(data[[eval(TargetColumnName)]])) data[, eval(TargetColumnName) := as.numeric(get(TargetColumnName))]

  # Variables for Program: Store number of data partitions in NumSets ----
  if(DebugMode) print('Variables for Program: Store number of data partitions in NumSets ----')
  NumSets <- length(SplitRatios)

  # Variables for Program: Store Maximum Value of TargetColumnName in val ----
  if(DebugMode) print('Variables for Program: Store Maximum Value of TargetColumnName in val ----')
  if(!is.null(Lags)) {
    if(is.list(Lags) && is.list(MA_Periods)) val <- max(unlist(Lags), unlist(MA_Periods)) else val <- max(Lags, MA_Periods)
  }

  # Data Wrangling: Sort data ----
  if(DebugMode) print('Data Wrangling: Sort data by GroupVar then DateColumnName ----')
  if(!is.null(GroupVariables)) data <- data[order(GroupVar, get(DateColumnName))] else data <- data[order(get(DateColumnName))]

  # Feature Engineering: Create Fourier Features ----
  if(DebugMode) print('Feature Engineering: Fourier Features ----')
  Output <- CarmaFourier(data.=data, XREGS.=XREGS, FourierTerms.=FourierTerms, TimeUnit.=TimeUnit, TargetColumnName.=TargetColumnName, GroupVariables.=GroupVariables, DateColumnName.=DateColumnName, HierarchGroups.=HierarchGroups)
  FourierTerms <- Output$FourierTerms; Output$FourierTerms <- NULL
  FourierFC <- Output$FourierFC; Output$FourierFC <- NULL
  data <- Output$data; rm(Output)

  # Feature Engineering: Create Calendar Variables ----
  if(DebugMode) print('Feature Engineering: Add Create Calendar Variables ----')
  if(!is.null(CalendarVariables)) data <- CreateCalendarVariables(data=data, DateCols=eval(DateColumnName), AsFactor=FALSE, TimeUnits=CalendarVariables)

  # Feature Engineering: Create Holiday Variables ----
  if(DebugMode) print('Feature Engineering: Add Create Holiday Variables ----')
  if(!is.null(HolidayVariable)) {
    data <- CreateHolidayVariables(data, DateCols = eval(DateColumnName), LookbackDays = if(!is.null(HolidayLookback)) HolidayLookback else LB(TimeUnit), HolidayGroups = HolidayVariable, Holidays = NULL)
    if(!(tolower(TimeUnit) %chin% c('1min','5min','10min','15min','30min','hour'))) {
      data[, eval(DateColumnName) := lubridate::as_date(get(DateColumnName))]
    } else {
      data[, eval(DateColumnName) := as.POSIXct(get(DateColumnName))]
    }
  }

  # Anomaly detection by Group and Calendar Vars ----
  if(DebugMode) print('Anomaly detection by Group and Calendar Vars ----')
  if(!is.null(AnomalyDetection)) {
    data <- GenTSAnomVars(
      data = data, ValueCol = eval(TargetColumnName),
      GroupVars = if(!is.null(CalendarVariables) && !is.null(GroupVariables)) c('GroupVar', paste0(DateColumnName, '_', CalendarVariables[1])) else if(!is.null(GroupVariables)) 'GroupVar' else NULL,
      DateVar = eval(DateColumnName), KeepAllCols = TRUE, IsDataScaled = FALSE,
      HighThreshold = AnomalyDetection$tstat_high,
      LowThreshold = AnomalyDetection$tstat_low)
    data[, paste0(eval(TargetColumnName), '_zScaled') := NULL]
    data[, ':=' (RowNumAsc = NULL, CumAnomHigh = NULL, CumAnomLow = NULL, AnomHighRate = NULL, AnomLowRate = NULL)]
  }

  # Feature Engineering: Target Transformation ----
  if(DebugMode) print('Feature Engineering: Add Target Transformation ----')
  if(TargetTransformation) {
    TransformResults <- AutoTransformationCreate(data, ColumnNames=TargetColumnName, Methods=Methods, Path=NULL, TransID='Trans', SaveOutput=FALSE)
    data <- TransformResults$Data; TransformResults$Data <- NULL
    TransformObject <- TransformResults$FinalResults; rm(TransformResults)
  } else {
    TransformObject <- NULL
  }

  # Copy data for non grouping + difference ----
  if(DebugMode) print('Copy data for non grouping + difference ----')
  if(is.null(GroupVariables) && Difference) antidiff <- data.table::copy(data[, .SD, .SDcols = c(eval(TargetColumnName),eval(DateColumnName))])

  # Variables for CARMA function IDcols ----
  if(DebugMode) print('Variables for CARMA function:IDcols----')
  IDcols <- names(data)[which(names(data) %chin% DateColumnName)]
  if(Difference && !is.null(GroupVariables)) IDcols <- c(IDcols, names(data)[which(names(data) == TargetColumnName)], names(data)[which(names(data) == 'TargetDiffMidStep')])

  # Feature Engineering: Differencing ----
  if(DebugMode) print('Feature Engineering: Add Difference Data ----')
  Output <- CarmaDifferencing(GroupVariables.=GroupVariables, Difference.=Difference, data.=data, TargetColumnName.=TargetColumnName, FC_Periods.=FC_Periods)
  data <- Output$data; Output$data <- NULL
  dataStart <- Output$dataStart; Output$dataStart <- NULL
  FC_Periods <- Output$FC_Periods; Output$FC_Periods <- NULL
  DiffTrainOutput <- Output$DiffTrainOutput
  Train <- Output$Train; rm(Output)
  if(Difference && !is.null(GroupVariables)) IDcols <- c(IDcols, 'TargetDiffMidStep')

  # Feature Engineering: Lags and Rolling Stats ----
  if(DebugMode) print('Feature Engineering: Lags and Rolling Stats ----')
  Output <- CarmaTimeSeriesFeatures(data.=data, TargetColumnName.=TargetColumnName, DateColumnName.=DateColumnName, GroupVariables.=GroupVariables, HierarchGroups.=HierarchGroups, Difference.=Difference, TimeGroups.=TimeGroups, TimeUnit.=TimeUnit, Lags.=Lags, MA_Periods.=MA_Periods, SD_Periods.=SD_Periods, Skew_Periods.=Skew_Periods, Kurt_Periods.=Kurt_Periods, Quantile_Periods.=Quantile_Periods, Quantiles_Selected.=Quantiles_Selected, HolidayVariable.=HolidayVariable, HolidayLags.=HolidayLags, HolidayMovingAverages.=HolidayMovingAverages, DebugMode.=DebugMode)
  IndependentSupplyValue <- Output$IndependentSupplyValue; Output$IndependentSupplyValue <- NULL
  HierarchSupplyValue <- Output$HierarchSupplyValue; Output$HierarchSupplyValue <- NULL
  GroupVarVector <- Output$GroupVarVector; Output$GroupVarVector <- NULL
  Categoricals <- Output$Categoricals; Output$Categoricals <- NULL
  data <- Output$data; rm(Output)

  # Create GroupVar ----
  if(!is.null(GroupVariables) && !'GroupVar' %chin% names(data)) data[, GroupVar := do.call(paste, c(.SD, sep = ' ')), .SDcols = c(GroupVariables)]

  # Data Wrangling: ModelDataPrep() to prepare data ----
  if(DebugMode) print('Data Wrangling: ModelDataPrep() to prepare data ----')
  data <- ModelDataPrep(data=data, Impute=TRUE, IntToNumeric=TRUE, DateToChar=FALSE, FactorToChar=FALSE, CharToFactor=TRUE, LogicalToBinary=FALSE, RemoveDates=FALSE, MissFactor='0', MissNum=-1, IgnoreCols=NULL)

  # Data Wrangling: Remove dates with imputed data from the DT_GDL_Feature_Engineering() features ----
  if(DebugMode) print('Data Wrangling: Remove dates with imputed data from the DT_GDL_Feature_Engineering() features ----')
  if(DataTruncate && !is.null(Lags)) data <- CarmaTruncateData(data.=data, DateColumnName.=DateColumnName, TimeUnit.=TimeUnit)

  # Feature Engineering: Add TimeTrend Variable ----
  if(DebugMode) print('Feature Engineering: Add TimeTrend Variable----')
  if(TimeTrendVariable) {
    if(!is.null(GroupVariables)) data[, TimeTrend := seq_len(.N), by = 'GroupVar'] else data[, TimeTrend := seq_len(.N)]
  }

  # Store Date Info ----
  FutureDateData <- unique(data[, get(DateColumnName)])

  # Create TimeWeights ----
  if(DebugMode) print('Create TimeWeights ----')
  train <- CarmaTimeWeights(train.=data, TimeWeights.=TimeWeights, GroupVariables.=GroupVariables, DateColumnName.=DateColumnName)

  # Save data to file before Partitioning ----
  if(!is.null(SaveDataPath)) data.table::fwrite(data, file.path(SaveDataPath, 'ModelData.csv'))

  # Data Wrangling: Partition data ----
  if(DebugMode) print('Data Wrangling: Partition data with AutoDataPartition()----')
  if(tolower(PartitionType) == 'timeseries' && is.null(GroupVariables)) PartitionType <- 'time'
  Output <- CarmaPartition(data.=data, SplitRatios.=SplitRatios, TrainOnFull.=TrainOnFull, NumSets.=NumSets, PartitionType.=PartitionType, GroupVariables.=GroupVariables, DateColumnName.=DateColumnName)
  train <- Output$train; Output$train <- NULL
  valid <- Output$valid; Output$valid <- NULL
  data <- Output$data; Output$data <- NULL
  test <- Output$test; rm(Output)

  # Data Wrangling: copy data or train for later in function since AutoRegression will modify data and train ----
  if(DebugMode) print('Data Wrangling: copy data or train for later in function since AutoRegression will modify data and train ----')
  if(!is.null(GroupVariables)) data.table::setorderv(x = data, cols = c('GroupVar',eval(DateColumnName)), order = c(1,1)) else data.table::setorderv(x = data, cols = c(eval(DateColumnName)), order = c(1))
  Step1SCore <- data.table::copy(data)

  # Define Target and Features ----
  if(DebugMode) print('Define ML args ----')
  Output <- CarmaFeatures(data.=data, train.=train, XREGS.=XREGS, Difference.=Difference, TargetColumnName.=TargetColumnName, DateColumnName.=DateColumnName, GroupVariables.=GroupVariables)
  ModelFeatures <- Output$ModelFeatures
  TargetVariable <- Output$TargetVariable; rm(Output)

  # Switch up TrainOnFull if SplitRatios is not null ----
  if(!is.null(SplitRatios) || !TrainOnFull) TOF <- FALSE else TOF <- TRUE

  # Run AutoCatBoostRegression and return list of ml objects ----
  TestModel <- AutoCatBoostRegression(

    # GPU or CPU and the number of available GPUs
    task_type = TaskType,
    NumGPUs = NumGPU,
    OutputSelection = c('Importances', 'EvalPlots', 'EvalMetrics', 'Score_TrainData'),

    # Metadata arguments
    ModelID = 'CatBoost',
    model_path = getwd(),
    metadata_path = if(!is.null(PDFOutputPath)) PDFOutputPath else getwd(),
    SaveModelObjects = FALSE,
    ReturnModelObjects = TRUE,
    SaveInfoToPDF = if(!is.null(PDFOutputPath)) TRUE else FALSE,

    # Data arguments
    data = train,
    TrainOnFull = TOF,
    ValidationData = valid,
    TestData = test,
    TargetColumnName = TargetVariable,
    FeatureColNames = ModelFeatures,
    PrimaryDateColumn = eval(DateColumnName),
    WeightsColumnName = if('Weights' %in% names(train)) 'Weights' else NULL,
    IDcols = IDcols,
    TransformNumericColumns = NULL,
    Methods = NULL,

    # Model evaluation
    eval_metric = EvalMetric,
    eval_metric_value = EvalMetricValue,
    loss_function = LossFunction,
    loss_function_value = LossFunctionValue,
    MetricPeriods = 10L,
    NumOfParDepPlots = NumOfParDepPlots,

    # Grid tuning arguments
    PassInGrid = PassInGrid,
    GridTune = GridTune,
    MaxModelsInGrid = ModelCount,
    MaxRunsWithoutNewWinner = MaxRunsWithoutNewWinner,
    MaxRunMinutes = 60*60,
    BaselineComparison = 'default',

    # ML args
    langevin = Langevin,
    diffusion_temperature = DiffusionTemperature,
    Trees = NTrees,
    Depth = Depth,
    LearningRate = LearningRate,
    L2_Leaf_Reg = L2_Leaf_Reg,
    RandomStrength = RandomStrength,
    BorderCount = BorderCount,
    RSM = if(TaskType == 'GPU') NULL else RSM,
    BootStrapType = BootStrapType,
    GrowPolicy = GrowPolicy,

    # New ML args
    model_size_reg = ModelSizeReg,
    feature_border_type = FeatureBorderType,
    sampling_unit = SamplingUnit,
    subsample = SubSample,
    score_function = ScoreFunction,
    min_data_in_leaf = MinDataInLeaf,
    DebugMode = DebugMode)

  # Return model object for when TrainOnFull is FALSE ----
  if(!TrainOnFull) return(TestModel)

  # Turn warnings into errors back on ----
  if(DebugMode) options(warn = 2)

  # Variable for storing ML model: Pull model object out of TestModel list ----
  if(DebugMode) print('Variable for storing ML model: Pull model object out of TestModel list ----')
  Model <- TestModel$Model

  # Variable for interation counts: max number of rows in Step1SCore data.table across all group ----
  if(DebugMode) print('Variable for interation counts: max number of rows in Step1SCore data.table across all group ----')
  N <- CarmaRecordCount(GroupVariables.=GroupVariables,Difference.=Difference, Step1SCore.=Step1SCore)

  # ARMA PROCESS FORECASTING ----
  if(DebugMode) print('ARMA PROCESS FORECASTING ----')
  for(i in seq_len(FC_Periods+1L)) {

    # Score model ----
    if(DebugMode) print('Score model ----')
    if(i == 1L) UpdateData <- NULL
    Output <- CarmaScore(Type = 'catboost', i.=i, N.=N, GroupVariables.=GroupVariables, ModelFeatures.=ModelFeatures, HierarchGroups.=HierarchGroups, DateColumnName.=DateColumnName, Difference.=Difference, TargetColumnName.=TargetColumnName, Step1SCore.=Step1SCore, Model.=Model, FutureDateData.=FutureDateData, NonNegativePred.=NonNegativePred, RoundPreds.=RoundPreds, UpdateData.=UpdateData, FactorList.=NULL)
    UpdateData <- Output$UpdateData; Output$UpdateData <- NULL
    Preds <- Output$Preds; Output$Preds <- NULL
    N <- Output$N; rm(Output)

    # Update data for next prediction ----
    if(DebugMode) print('Update data for next prediction ----')
    if(i != FC_Periods+1L) {

      # Timer ----
      if(DebugMode) print('Timer----')
      if(Timer) if(i != 1) print(paste0('Forecast future step: ', i-1))
      if(Timer) starttime <- Sys.time()

      # Create single future record ----
      if(DebugMode) print('Create single future record ----')
      CalendarFeatures <- NextTimePeriod(UpdateData.=UpdateData, TimeUnit.=TimeUnit, DateColumnName.=DateColumnName)

      # Update flat feature engineering ----
      if(DebugMode) print('Update feature engineering ----')
      UpdateData <- UpdateFeatures(UpdateData.=UpdateData, GroupVariables.=GroupVariables, CalendarFeatures.=CalendarFeatures, CalendarVariables.=CalendarVariables, GroupVarVector.=GroupVarVector, DateColumnName.=DateColumnName, XREGS.=XREGS, FourierTerms.=FourierTerms, FourierFC.=FourierFC, TimeGroups.=TimeGroups, TimeTrendVariable.=TimeTrendVariable, N.=N, TargetColumnName.=TargetColumnName, HolidayVariable.=HolidayVariable, HolidayLookback.=HolidayLookback, TimeUnit.=TimeUnit, AnomalyDetection.=AnomalyDetection, i.=i)

      # Update Lags and MA's ----
      if(DebugMode) print('Update Lags and MAs ----')
      UpdateData <- CarmaRollingStatsUpdate(ModelType='catboost', DebugMode.=DebugMode, UpdateData.=UpdateData, GroupVariables.=GroupVariables, Difference.=Difference, CalendarVariables.=CalendarVariables, HolidayVariable.=HolidayVariable, IndepVarPassTRUE.=IndepentVariablesPass, data.=data, CalendarFeatures.=CalendarFeatures, XREGS.=XREGS, HierarchGroups.=HierarchGroups, GroupVarVector.=GroupVarVector, TargetColumnName.=TargetColumnName, DateColumnName.=DateColumnName, Preds.=Preds, HierarchSupplyValue.=HierarchSupplyValue, IndependentSupplyValue.=IndependentSupplyValue, TimeUnit.=TimeUnit, TimeGroups.=TimeGroups, Lags.=Lags, MA_Periods.=MA_Periods, SD_Periods.=SD_Periods, Skew_Periods.=Skew_Periods, Kurt_Periods.=Kurt_Periods, Quantile_Periods.=Quantile_Periods, Quantiles_Selected.=Quantiles_Selected, HolidayLags.=HolidayLags, HolidayMovingAverages.=HolidayMovingAverages)

      # Print time to complete ----
      if(Timer) endtime <- Sys.time()
      if(Timer && i != 1) print(endtime - starttime)
    }
  }

  # Memory support ----
  gc()

  # Return data prep ----
  if(DebugMode) print('Return data prep ----')
  Output <- CarmaReturnDataPrep(UpdateData.=UpdateData, FutureDateData.=FutureDateData, dataStart.=dataStart, DateColumnName.=DateColumnName, TargetColumnName.=TargetColumnName, GroupVariables.=GroupVariables, Difference.=Difference, TargetTransformation.=TargetTransformation, TransformObject.=TransformObject, NonNegativePred.=NonNegativePred, DiffTrainOutput.=DiffTrainOutput)
  UpdateData <- Output$UpdateData; Output$UpdateData <- NULL
  TransformObject <- Output$TransformObject; rm(Output)

  # Return ----
  return(list(
    Forecast = UpdateData,
    ModelInformation = TestModel,
    TransformationDetail = if(exists('TransformObject') && !is.null(TransformObject)) TransformObject else NULL,
    ArgsList = ArgsList))
}
