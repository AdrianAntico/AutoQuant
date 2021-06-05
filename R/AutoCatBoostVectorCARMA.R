#' @title AutoCatBoostVectorCARMA
#'
#' @description AutoCatBoostVectorCARMA Multiple Regression, Mutlivariate Forecasting with calendar variables, Holiday counts, holiday lags, holiday moving averages, differencing, transformations, interaction-based categorical encoding using target variable and features to generate various time-based aggregated lags, moving averages, moving standard deviations, moving skewness, moving kurtosis, moving quantiles, parallelized interaction-based fourier pairs by grouping variables, and Trend Variables.
#'
#' @author Adrian Antico
#' @family Automated Panel Data Forecasting
#'
#' @param data Supply your full series data set here
#' @param TrainOnFull Set to TRUE to train on full data
#' @param TaskType Has to CPU for now. If catboost makes GPU available for 'MultiRMSE' then it will be enabled. If you set to GPU the function will coerce it back to CPU.
#' @param NumGPU Defaults to 1. If CPU is set this argument will be ignored.
#' @param TargetColumnName List the column names of your target variables column. E.g. c('Target1','Target2', ..., 'TargetN')
#' @param NonNegativePred TRUE or FALSE
#' @param RoundPreds Rounding predictions to an integer value. TRUE or FALSE. Defaults to FALSE
#' @param DateColumnName List the column name of your date column. E.g. 'DateTime'
#' @param GroupVariables Defaults to NULL. Use NULL when you have a single series. Add in GroupVariables when you have a series for every level of a group or multiple groups.
#' @param HierarchGroups Vector of hierachy categorical columns.
#' @param TimeWeights NULL or a value.
#' @param TimeUnit List the time unit your data is aggregated by. E.g. '1min', '5min', '10min', '15min', '30min', 'hour', 'day', 'week', 'month', 'quarter', 'year'.
#' @param TimeGroups Select time aggregations for adding various time aggregated GDL features.
#' @param FC_Periods Set the number of periods you want to have forecasts for. E.g. 52 for weekly data to forecast a year ahead
#' @param TargetTransformation Run AutoTransformationCreate() to find best transformation for the target variable. Tests YeoJohnson, BoxCox, and Asigh (also Asin and Logit for proportion target variables).
#' @param Methods Transformation options to test which include 'BoxCox', 'Asinh', 'Asin', 'Log', 'LogPlus1', 'Logit', 'YeoJohnson'
#' @param XREGS Additional data to use for model development and forecasting. Data needs to be a complete series which means both the historical and forward looking values over the specified forecast window needs to be supplied.
#' @param Timer Set to FALSE to turn off the updating print statements for progress
#' @param DebugMode Defaults to FALSE. Set to TRUE to get a print statement of each high level comment in function
#' @param Lags Select the periods for all lag variables you want to create. E.g. c(1:5,52)
#' @param MA_Periods Select the periods for all moving average variables you want to create. E.g. c(1:5,52)
#' @param SD_Periods Select the periods for all moving standard deviation variables you want to create. E.g. c(1:5,52)
#' @param Skew_Periods Select the periods for all moving skewness variables you want to create. E.g. c(1:5,52)
#' @param Kurt_Periods Select the periods for all moving kurtosis variables you want to create. E.g. c(1:5,52)
#' @param Quantile_Periods Select the periods for all moving quantiles variables you want to create. E.g. c(1:5,52)
#' @param Quantiles_Selected Select from the following 'q5', 'q10', 'q15', 'q20', 'q25', 'q30', 'q35', 'q40', 'q45', 'q50', 'q55', 'q60', 'q65', 'q70', 'q75', 'q80', 'q85', 'q90', 'q95'
#' @param AnomalyDetection NULL for not using the service. Other, provide a list, e.g. AnomalyDetection = list('tstat_high' = 4, tstat_low = -4)
#' @param Difference Puts the I in ARIMA for single series and grouped series.
#' @param FourierTerms Set to the max number of pairs. E.g. 2 means to generate two pairs for by each group level and interations if hierarchy is enabled.
#' @param CalendarVariables NULL, or select from 'second', 'minute', 'hour', 'wday', 'mday', 'yday', 'week', 'isoweek', 'month', 'quarter', 'year'
#' @param HolidayVariable NULL, or select from 'USPublicHolidays', 'EasterGroup', 'ChristmasGroup', 'OtherEcclesticalFeasts'
#' @param HolidayLookback Number of days in range to compute number of holidays from a given date in the data. If NULL, the number of days are computed for you.
#' @param HolidayLags Number of lags to build off of the holiday count variable.
#' @param HolidayMovingAverages Number of moving averages to build off of the holiday count variable.
#' @param TimeTrendVariable Set to TRUE to have a time trend variable added to the model. Time trend is numeric variable indicating the numeric value of each record in the time series (by group). Time trend starts at 1 for the earliest point in time and increments by one for each success time point.
#' @param DataTruncate Set to TRUE to remove records with missing values from the lags and moving average features created
#' @param ZeroPadSeries Set to 'all', 'inner', or NULL. See TimeSeriesFill for explanation
#' @param PartitionType Select 'random' for random data partitioning 'timeseries' for partitioning by time frames
#' @param SplitRatios E.g c(0.7,0.2,0.1) for train, validation, and test sets
#' @param NumOfParDepPlots Supply a number for the number of partial dependence plots you want returned
#' @param EvalMetric 'MultiRMSE' only. If catboost updates this I'll add more later
#' @param EvalMetricValue Placeholder for later
#' @param LossFunction 'MultiRMSE' only. If catboost updates this I'll add more later
#' @param LossFunctionValue Placeholder for later
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
#' # Two group variables and xregs
#'
#' # Load Walmart Data from Dropbox
#' data <- data.table::fread(
#'  'https://www.dropbox.com/s/2str3ek4f4cheqi/walmart_train.csv?dl=1')
#'
#' # Filter out zeros
#' data <- data[Weekly_Sales != 0]
#'
#' # Subset for Stores / Departments With Full Series
#' data <- data[, Counts := .N, by = c('Store','Dept')][Counts == 143][
#'  , Counts := NULL]
#'
#' # Subset Columns (remove IsHoliday column)----
#' keep <- c('Store','Dept','Date','Weekly_Sales')
#' data <- data[, ..keep]
#' data <- data[Store %in% c(1,2)]
#' xregs <- data.table::copy(data)
#' xregs[, GroupVar := do.call(paste, c(.SD, sep = ' ')), .SDcols = c('Store','Dept')]
#' xregs[, c('Store','Dept') := NULL]
#' data.table::setnames(xregs, 'Weekly_Sales', 'Other')
#' xregs[, Other := jitter(Other, factor = 25)]
#' data <- data[as.Date(Date) < as.Date('2012-09-28')]
#'
#' # Vector CARMA testing
#' data[, Weekly_Profit := Weekly_Sales * 0.75]
#'
#' # Build forecast
#' CatBoostResults <- RemixAutoML::AutoCatBoostVectorCARMA(
#'
#'   # data args
#'   data = data, # TwoGroup_Data,
#'   TargetColumnName = c('Weekly_Sales','Weekly_Profit'),
#'   DateColumnName = 'Date',
#'   HierarchGroups = NULL,
#'   GroupVariables = c('Store','Dept'),
#'   TimeWeights = 1,
#'   TimeUnit = 'weeks',
#'   TimeGroups = c('weeks','months'),
#'
#'   # Production args
#'   TaskType = 'GPU',
#'   NumGPU = 1,
#'   TrainOnFull = TRUE,
#'   SplitRatios = c(1 - 10 / 138, 10 / 138),
#'   PartitionType = 'random',
#'   FC_Periods = 4,
#'   Timer = TRUE,
#'   DebugMode = TRUE,
#'
#'   # Target transformations
#'   TargetTransformation = TRUE,
#'   Methods = c('BoxCox', 'Asinh', 'Asin', 'Log',
#'               'LogPlus1', 'Logit', 'YeoJohnson'),
#'   Difference = FALSE,
#'   NonNegativePred = FALSE,
#'   RoundPreds = FALSE,
#'
#'   # Date features
#'   CalendarVariables = c('week', 'month', 'quarter'),
#'   HolidayVariable = c('USPublicHolidays',
#'                       'EasterGroup',
#'                       'ChristmasGroup','OtherEcclesticalFeasts'),
#'   HolidayLookback = NULL,
#'   HolidayLags = 1,
#'   HolidayMovingAverages = 1:2,
#'
#'   # Time series features
#'   Lags = list('weeks' = seq(2L, 10L, 2L),
#'               'months' = c(1:3)),
#'   MA_Periods = list('weeks' = seq(2L, 10L, 2L),
#'                     'months' = c(2,3)),
#'   SD_Periods = NULL,
#'   Skew_Periods = NULL,
#'   Kurt_Periods = NULL,
#'   Quantile_Periods = NULL,
#'   Quantiles_Selected = c('q5','q95'),
#'
#'   # Bonus features
#'   AnomalyDetection = NULL,
#'   XREGS = xregs,
#'   FourierTerms = 2,
#'   TimeTrendVariable = TRUE,
#'   ZeroPadSeries = NULL,
#'   DataTruncate = FALSE,
#'
#'   # Eval args
#'   NumOfParDepPlots = 100L,
#'   EvalMetric = 'MultiRMSE',
#'   EvalMetricValue = 1.5,
#'   LossFunction = 'MultiRMSE',
#'   LossFunctionValue = 1.5,
#'
#'   # Grid args
#'   GridTune = FALSE,
#'   PassInGrid = NULL,
#'   ModelCount = 5,
#'   MaxRunsWithoutNewWinner = 50,
#'   MaxRunMinutes = 60*60,
#'
#'   # ML Args
#'   NTrees = 1000,
#'   Depth = 6,
#'   LearningRate = NULL,
#'   L2_Leaf_Reg = NULL,
#'   RandomStrength = 1,
#'   BorderCount = 254,
#'   RSM = 1,
#'   BootStrapType = 'Bayesian',
#'   GrowPolicy = 'SymmetricTree',
#'   Langevin = FALSE,
#'   DiffusionTemperature = 10000,
#'   ModelSizeReg = 0.5,
#'   FeatureBorderType = 'GreedyLogSum',
#'   SamplingUnit = 'Group',
#'   SubSample = NULL,
#'   ScoreFunction = 'Cosine',
#'   MinDataInLeaf = 1)
#' }
#' @return Returns a data.table of original series and forecasts, the catboost model objects (everything returned from AutoCatBoostRegression()), a time series forecast plot, and transformation info if you set TargetTransformation to TRUE. The time series forecast plot will plot your single series or aggregate your data to a single series and create a plot from that.
#' @export
AutoCatBoostVectorCARMA <- function(data,
                                    NonNegativePred = FALSE,
                                    RoundPreds = FALSE,
                                    TrainOnFull = FALSE,
                                    TargetColumnName = 'Target',
                                    DateColumnName = 'DateTime',
                                    HierarchGroups = NULL,
                                    GroupVariables = NULL,
                                    TimeWeights = 1,
                                    FC_Periods = 30,
                                    TimeUnit = 'week',
                                    TimeGroups = c('weeks','months'),
                                    NumOfParDepPlots = 10L,
                                    TargetTransformation = FALSE,
                                    Methods = c('BoxCox', 'Asinh', 'Asin', 'Log', 'LogPlus1', 'Logit', 'YeoJohnson'),
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
                                    CalendarVariables = c('second', 'minute', 'hour', 'wday', 'mday', 'yday', 'week', 'isoweek', 'month', 'quarter', 'year'),
                                    HolidayVariable = c('USPublicHolidays','EasterGroup','ChristmasGroup','OtherEcclesticalFeasts'),
                                    HolidayLookback = NULL,
                                    HolidayLags = 1L,
                                    HolidayMovingAverages = 1L:2L,
                                    TimeTrendVariable = FALSE,
                                    ZeroPadSeries = NULL,
                                    DataTruncate = FALSE,
                                    SplitRatios = c(0.7, 0.2, 0.1),
                                    TaskType = 'GPU',
                                    NumGPU = 1,
                                    PartitionType = 'timeseries',
                                    Timer = TRUE,
                                    DebugMode = FALSE,
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
  # Load catboost----
  if(DebugMode) print('Load catboost----')
  if(DebugMode) print("loadNamespace(package = 'catboost')")
  loadNamespace(package = 'catboost')

  # Purified args: see CARMA HELPER FUNCTIONS----
  if(DebugMode) print('# Purified args: see CARMA HELPER FUNCTIONS----')
  Args <- CARMA_Define_Args(
    TimeUnit = TimeUnit, TimeGroups = TimeGroups, HierarchGroups = HierarchGroups, GroupVariables = GroupVariables,
    FC_Periods = FC_Periods, PartitionType = PartitionType, TrainOnFull = TrainOnFull, SplitRatios = SplitRatios,
    SD_Periods = SD_Periods, Skew_Periods = Skew_Periods, Kurt_Periods = Kurt_Periods, Quantile_Periods = Quantile_Periods)

  # Store purified args
  if(DebugMode) print('# Store purified args----')
  IndepentVariablesPass <- Args$IndepentVariablesPass
  TimeGroups            <- Args$TimeGroups
  TimeUnit              <- Args$TimeUnit
  TimeGroup             <- Args$TimeGroupPlaceHolder
  HierarchGroups        <- Args$HierarchGroups
  GroupVariables        <- Args$GroupVariables
  FC_Periods            <- Args$FC_Periods
  HoldOutPeriods        <- Args$HoldOutPeriods

  # Arg check ----
  if(!is.null(HolidayLookback) && !is.numeric(HolidayLookback)) stop('HolidayLookback has to be numeric')

  # Variables for Program: Redefine HoldOutPerids----
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

  # Convert data to data.table ----
  if(!data.table::is.data.table(data)) data.table::setDT(data)
  if(!is.null(XREGS) && !data.table::is.data.table(XREGS)) data.table::setDT(XREGS)

  # Modify FC_Periods ----
  if(DebugMode) print('# Check lengths of XREGS')
  Output <- CarmaFCHorizon(data.=data, XREGS.=XREGS, TrainOnFull.=TrainOnFull, Difference.= Difference, FC_Periods.=FC_Periods, HoldOutPeriods.=HoldOutPeriods, DateColumnName.=DateColumnName)
  FC_Periods <- Output$FC_Periods
  HoldOutPeriods <- Output$HoldOutPeriods; rm(Output)

  # Check for any Target Variable hiding in XREGS ----
  if(DebugMode) print('# Check for any Target Variable hiding in XREGS')
  if(any(eval(TargetColumnName) %chin% names(XREGS))) data.table::set(XREGS, j = eval(TargetColumnName), value = NULL)

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
  Output <- CarmaDateStandardize(data.=data, XREGS.=NULL, DateColumnName.=DateColumnName, TimeUnit.=TimeUnit)
  data <- Output$data; Output$data <- NULL
  XREGS <- Output$XREGS; rm(Output)

  # Data Wrangling: Ensure TargetColumnName is Numeric ----
  if(DebugMode) print('Data Wrangling: Ensure TargetColumnName is Numeric ----')
  for(zz in seq_len(length(TargetColumnName))) if(!is.numeric(data[[eval(TargetColumnName[zz])]])) data[, eval(TargetColumnName[zz]) := as.numeric(get(TargetColumnName[zz]))]

  # Variables for Program: Store number of data partitions in NumSets ----
  if(DebugMode) print('Variables for Program: Store number of data partitions in NumSets----')
  NumSets <- length(SplitRatios)

  # Variables for Program: Store Maximum Value of TargetColumnName in val ----
  if(DebugMode) print('Variables for Program: Store Maximum Value of TargetColumnName in val ----')
  if(!is.null(Lags)) {
    if(is.list(Lags) && is.list(MA_Periods)) val <- max(unlist(Lags), unlist(MA_Periods)) else val <- max(Lags, MA_Periods)
  }

  # Data Wrangling: Sort data ----
  if(DebugMode) print('Data Wrangling: Sort data by GroupVar then DateColumnName ----')
  if(!is.null(GroupVariables)) data <- data[order(GroupVar, get(DateColumnName))] else data <- data[order(get(DateColumnName))]

  # Feature Engineering: Add Fourier Features by GroupVar----
  # To error check, store arg values and run through EconometricsFunctions.R AutoHierarchicalFourier
  if(DebugMode) print('Feature Engineering: Add Fourier Features by GroupVar----')
  if(FourierTerms > 0L) {

    # Loop through targets ----
    for(zz in seq_len(length(TargetColumnName))) {

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
        TargetColumN = TargetColumnName[[zz]],
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
          print('Turning off Fourier Terms. Failed to build.')
          FourierTerms <<- 0
        }
      } else {
        print('Turning off Fourier Terms. Failed to build.')
        FourierTerms <<- 0
      }

      # If Fourier is turned off, concatenate grouping cols
      if(FourierTerms == 0) {
        if(!is.null(HierarchGroups)) {
          if(length(HierarchGroups) > 1) {
            if(any(HierarchGroups %chin% names(data))) {
              data[, GroupVar := do.call(paste, c(.SD, sep = ' ')), .SDcols = HierarchGroups]
              data[, eval(HierarchGroups) := NULL]
            }
          } else {
            data[, GroupVar := do.call(paste, c(.SD, sep = ' ')), .SDcols = HierarchGroups]
            if(HierarchGroups != 'GroupVar') {
              data[, eval(HierarchGroups) := NULL]
            }
          }
        } else if(!is.null(GroupVariables)) {
          if(all(GroupVariables %chin% names(data))) {
            data[, GroupVar := do.call(paste, c(.SD, sep = ' ')), .SDcols = GroupVariables]
          }
        }
      }
    }
  }

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
  if(!is.null(AnomalyDetection)) {
    for(zz in seq_len(length(TargetColumnName))) {
      if(!is.null(CalendarVariables) & !is.null(GroupVariables)) {
        groupvars <- c('GroupVar', paste0(DateColumnName, '_', CalendarVariables[1]))
        data <- RemixAutoML::GenTSAnomVars(
          data = data, ValueCol = eval(TargetColumnName[zz]),
          GroupVars = groupvars,
          DateVar = eval(DateColumnName),
          HighThreshold = AnomalyDetection$tstat_high,
          LowThreshold = AnomalyDetection$tstat_low,
          KeepAllCols = TRUE,
          IsDataScaled = FALSE)
        data[, paste0(eval(TargetColumnName[zz]), '_zScaled') := NULL]
        data[, ':=' (RowNumAsc = NULL, CumAnomHigh = NULL, CumAnomLow = NULL, AnomHighRate = NULL, AnomLowRate = NULL)]
      } else if(!is.null(GroupVariables)) {
        data <- RemixAutoML::GenTSAnomVars(
          data = data, ValueCol = eval(TargetColumnName[zz]),
          GroupVars = 'GroupVar',
          DateVar = eval(DateColumnName),
          HighThreshold = AnomalyDetection$tstat_high,
          LowThreshold = AnomalyDetection$tstat_low,
          KeepAllCols = TRUE,
          IsDataScaled = FALSE)
        data[, paste0(eval(TargetColumnName[zz]), '_zScaled') := NULL]
        data[, ':=' (RowNumAsc = NULL, CumAnomHigh = NULL, CumAnomLow = NULL, AnomHighRate = NULL, AnomLowRate = NULL)]
      } else {
        data <- RemixAutoML::GenTSAnomVars(
          data = data, ValueCol = eval(TargetColumnName[zz]),
          GroupVars = NULL,
          DateVar = eval(DateColumnName),
          HighThreshold = AnomalyDetection$tstat_high,
          LowThreshold = AnomalyDetection$tstat_low,
          KeepAllCols = TRUE,
          IsDataScaled = FALSE)
        data[, paste0(eval(TargetColumnName[zz]), '_zScaled') := NULL]
        data[, ':=' (RowNumAsc = NULL, CumAnomHigh = NULL, CumAnomLow = NULL, AnomHighRate = NULL, AnomLowRate = NULL)]
      }
    }
  }

  # Feature Engineering: Add Target Transformation----
  if(DebugMode) print('Feature Engineering: Add Target Transformation----')
  if(TargetTransformation) {
    TransformObject <- list()
    for(zz in seq_along(TargetColumnName)) {
      TransformResults <- AutoTransformationCreate(
        data,
        ColumnNames = TargetColumnName[zz],
        Methods = Methods,
        Path = NULL,
        TransID = 'Trans',
        SaveOutput = FALSE)
      data <- TransformResults$Data
      TransformObject[[zz]] <- TransformResults$FinalResults
    }
  } else {
    TransformObject <- NULL
  }

  # Copy data for non grouping + difference ----
  if(DebugMode) print('Copy data for non grouping + difference----')
  if(is.null(GroupVariables) & Difference) antidiff <- data.table::copy(data[, .SD, .SDcols = c(eval(TargetColumnName),eval(DateColumnName))])

  # Feature Engineering: Add Difference Data ----
  if(DebugMode) print('Feature Engineering: Add Difference Data----')
  if(!is.null(GroupVariables) & Difference) {
    data[, TargetDiffMidStep := data.table::shift(x = get(TargetColumnName), n = 1, fill = NA, type = 'lag'), by = c('GroupVar')][, ModTarget := get(TargetColumnName) - TargetDiffMidStep]
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
    dataStart <- NULL
    if(ncol(data) >= 3) {
      data <- cbind(Train,data[1:(nrow(data)-1)][,.SD, .SDcols = names(data)[3:ncol(data)]])
    } else {
      data <- Train
    }
    FC_Periods <- FC_Periods + 1L
  } else {
    dataStart <- NULL
  }

  # Feature Engineering: Add GDL Features based on the TargetColumnName----
  if(!is.null(Lags)) {

    if(DebugMode) print('Feature Engineering: Add GDL Features based on the TargetColumnName----')

    # Group and No Differencing
    if(!is.null(GroupVariables) & !Difference) {

      # Split GroupVar and Define HierarchyGroups and IndependentGroups----
      Output <- CARMA_GroupHierarchyCheck(data = data, Group_Variables = GroupVariables, HierarchyGroups = HierarchGroups)
      data <- Output$data
      HierarchSupplyValue <- Output$HierarchSupplyValue
      IndependentSupplyValue <- Output$IndependentSupplyValue
      if(is.list(Lags)) TimeGroups <- names(Lags)

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
        Type                 = 'Lag',
        SimpleImpute         = TRUE,

        # Calculated Columns
        Lags                  = Lags,
        MA_RollWindows        = MA_Periods,
        SD_RollWindows        = SD_Periods,
        Skew_RollWindows      = Skew_Periods,
        Kurt_RollWindows      = Kurt_Periods,
        Quantile_RollWindows  = Quantile_Periods,
        Quantiles_Selected    = Quantiles_Selected)

      # Keep interaction group as GroupVar----
      if(length(GroupVariables) > 1) {
        if(!'GroupVar' %chin% names(data)) data[, GroupVar := do.call(paste, c(.SD, sep = ' ')), .SDcols = GroupVariables]
        if(!is.null(HierarchGroups)) Categoricals <- FullFactorialCatFeatures(GroupVars = HierarchGroups, BottomsUp = TRUE)
        if(!is.null(HierarchGroups)) GroupVarVector <- data[, .SD, .SDcols = c(Categoricals,'GroupVar')] else GroupVarVector <- data[, .SD, .SDcols = c('GroupVar')]
      } else {
        if(!'GroupVar' %chin% names(data)) data[, GroupVar := do.call(paste, c(.SD, sep = ' ')), .SDcols = GroupVariables]
      }
    }

    # Group and No Differencing
    if(!is.null(GroupVariables) & Difference) {

      # Split GroupVar and Define HierarchyGroups and IndependentGroups----
      Output <- CARMA_GroupHierarchyCheck(data = data, Group_Variables = GroupVariables, HierarchyGroups = HierarchGroups)
      data <- Output$data
      HierarchSupplyValue <- Output$HierarchSupplyValue
      IndependentSupplyValue <- Output$IndependentSupplyValue
      if(is.list(Lags)) TimeGroups <- names(Lags)

      # Generate features----
      data <- AutoLagRollStats(

        # Data
        data                 = data,
        DateColumn           = eval(DateColumnName),
        Targets              = c('ModTarget'),
        HierarchyGroups      = HierarchSupplyValue,
        IndependentGroups    = IndependentSupplyValue,

        # Services
        TimeBetween          = NULL,
        TimeUnit             = TimeUnit,
        TimeUnitAgg          = TimeUnit,
        TimeGroups           = TimeGroups,
        RollOnLag1           = TRUE,
        Type                 = 'Lag',
        SimpleImpute         = TRUE,

        # Calculated Columns
        Lags                  = Lags,
        MA_RollWindows        = MA_Periods,
        SD_RollWindows        = SD_Periods,
        Skew_RollWindows      = Skew_Periods,
        Kurt_RollWindows      = Kurt_Periods,
        Quantile_RollWindows  = Quantile_Periods,
        Quantiles_Selected    = Quantiles_Selected,
        Debug                 = DebugMode)

      # Keep interaction group as GroupVar----
      if(length(GroupVariables) > 1L) {
        if(!'GroupVar' %chin% names(data)) data[, GroupVar := do.call(paste, c(.SD, sep = ' ')), .SDcols = GroupVariables]
        if(!is.null(HierarchGroups)) Categoricals <- FullFactorialCatFeatures(GroupVars = HierarchGroups, BottomsUp = TRUE)
        if(!is.null(HierarchGroups)) GroupVarVector <- data[, .SD, .SDcols = c(Categoricals,'GroupVar')] else GroupVarVector <- data[, .SD, .SDcols = c('GroupVar')]
      } else {
        if(!'GroupVar' %chin% names(data)) data[, GroupVar := do.call(paste, c(.SD, sep = ' ')), .SDcols = GroupVariables]
      }
    }

    # No Group with or without Diff
    if(is.null(GroupVariables)) {

      # TimeGroups----
      if(is.list(Lags)) TimeGroups <- names(Lags)

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
        Type                 = 'Lag',
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
    if(DebugMode) print('Feature Engineering: Add Lag / Lead, MA Holiday Variables----')
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
          statsFUNs       = 'mean',
          targets         = 'HolidayCounts',
          groupingVars    = IndepentVariablesPass,
          sortDateName    = eval(DateColumnName),
          timeDiffTarget  = NULL,
          timeAgg         = TimeGroups[1],
          WindowingLag    = 1,
          Type            = 'Lag',
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
          statsFUNs       = 'mean',
          targets         = 'HolidayCounts',
          groupingVars    = NULL,
          sortDateName    = eval(DateColumnName),
          timeDiffTarget  = NULL,
          timeAgg         = TimeGroups[1],
          WindowingLag    = 1,
          Type            = 'Lag',
          SimpleImpute    = TRUE)
      }
    }
  }

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

  # Create TimeWeights ----
  if(DebugMode) print('Create TimeWeights ----')
  train <- CarmaTimeWeights(train.=data, TimeWeights.=TimeWeights, GroupVariables.=GroupVariables, DateColumnName.=DateColumnName)

  # Store Date Info ----
  FutureDateData <- unique(data[, get(DateColumnName)])

  # Data Wrangling: Partition data ----
  if(DebugMode) print('Data Wrangling: Partition data with AutoDataPartition()----')
  Output <- CarmaPartition(data.=data, SplitRatios.=SplitRatios, TrainOnFull.=TrainOnFull, NumSets.=NumSets, PartitionType.=PartitionType, GroupVariables.=GroupVariables, DateColumnName.=DateColumnName)
  train <- Output$train; Output$train <- NULL
  valid <- Output$valid; Output$valid <- NULL
  data <- Output$data; Output$data <- NULL
  test <- Output$test; rm(Output)

  # Variables for CARMA function IDcols ----
  if(DebugMode) print('Variables for CARMA function:IDcols----')
  IDcols <- names(data)[which(names(data) %chin% DateColumnName)]
  if(Difference && !is.null(GroupVariables)) IDcols <- c(IDcols, names(data)[which(names(data) %chin% TargetColumnName)])

  # Data Wrangling: copy data or train for later in function since AutoRegression will modify data and train----
  if(DebugMode) print('Data Wrangling: copy data or train for later in function since AutoRegression will modify data and train----')
  if(TrainOnFull) Step1SCore <- data.table::copy(data) else Step1SCore <- data.table::copy(train)

  # Machine Learning: Build Model ----
  if(DebugMode) print('Machine Learning: Build Model----')

  # Define Target and Features ----
  if(DebugMode) print('Define ML args ----')
  Output <- CarmaFeatures(data.=data, train.=train, XREGS.=XREGS, Difference.=Difference, TargetColumnName.=TargetColumnName, DateColumnName.=DateColumnName, GroupVariables.=GroupVariables)
  ModelFeatures <- Output$ModelFeatures
  TargetVariable <- Output$TargetVariable; rm(Output)

  # Switch up TrainOnFull if SplitRatios is not null ----
  if(!is.null(SplitRatios) || !TrainOnFull) TOF <- FALSE else TOF <- TRUE

  # Run AutoCatBoostRegression and return list of ml objects ----
  if(DebugMode) print('Run AutoCatBoostRegression and return list of ml objects ----')
  TestModel <- AutoCatBoostRegression(

    # GPU or CPU and the number of available GPUs
    task_type = TaskType,
    NumGPUs = NumGPU,

    # Metadata arguments
    OutputSelection = c('Importances', 'EvalPlots', 'EvalMetrics', 'Score_TrainData'),
    ModelID = 'ModelTest',
    model_path = getwd(),
    metadata_path = getwd(),
    SaveModelObjects = FALSE,
    ReturnModelObjects = TRUE,
    DebugMode = DebugMode,

    # Data arguments
    data = train,
    TrainOnFull = TOF,
    ValidationData = valid,
    TestData = test,
    WeightsColumnName = if('Weights' %in% names(train)) 'Weights' else NULL,
    TargetColumnName = TargetVariable,
    FeatureColNames = ModelFeatures,
    PrimaryDateColumn = eval(DateColumnName),
    IDcols = IDcols,
    TransformNumericColumns = if(TargetTransformation) TargetVariable else NULL,
    Methods = Methods,

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
    min_data_in_leaf = MinDataInLeaf)

  # Return model object for when TrainOnFull is FALSE ----
  if(!TrainOnFull) return(TestModel)

  # Turn warnings into errors back on
  if(DebugMode) options(warn = 2)

  # Variable for storing ML model: Pull model object out of TestModel list ----
  if(DebugMode) print('Variable for storing ML model: Pull model object out of TestModel list----')
  Model <- TestModel$Model

  # Variable for interation counts: max number of rows in Step1SCore data.table across all group ----
  if(DebugMode) print('Variable for interation counts: max number of rows in Step1SCore data.table across all group ----')
  N <- CarmaRecordCount(GroupVariables.=GroupVariables,Difference.=Difference, Step1SCore.=Step1SCore)

  # Number of forecast periods----
  if(DebugMode) print('Number of forecast periods----')
  if(TrainOnFull) ForecastRuns <- FC_Periods else ForecastRuns <- HoldOutPeriods

  #----

  #----

  # ARMA PROCESS FORECASTING ----
  if(DebugMode) print('ARMA PROCESS FORECASTING----')
  for(i in seq_len(ForecastRuns+1L)) {

    # Row counts ----
    if(DebugMode) print('Row counts----')
    if(i != 1) N <- as.integer(N + 1L)

    ###############
    # ML Scoring
    ###############

    # Machine Learning: Generate predictions----
    if(DebugMode) print('Machine Learning: Generate predictions----')
    if(i == 1L) {
      if(!is.null(GroupVariables)) {

        # Define IDcols----
        if(DebugMode) print('# Define IDcols----')
        if(Difference) IDcols <- 'ModTarget' else IDcols <- eval(TargetColumnName)

        # i = 1 Score Model With Group Variables----
        if(DebugMode) print('# i = 1 Score Model With Group Variables----')
        Preds <- AutoCatBoostScoring(
          TargetType = 'multiregression',
          ScoringData = Step1SCore,
          FeatureColumnNames = ModelFeatures,
          ReturnShapValues = FALSE,
          FactorLevelsList = TestModel$FactorLevelsList,
          IDcols = IDcols,
          OneHot = FALSE,
          ModelObject = Model,
          ModelPath = getwd(),
          ModelID = 'ModelTest',
          ReturnFeatures = TRUE,
          TransformNumeric = FALSE,
          BackTransNumeric = FALSE,
          TransformationObject = NULL,
          TransID = NULL,
          TransPath = NULL,
          TargetColumnName = TargetColumnName,
          MDP_Impute = TRUE,
          MDP_CharToFactor = TRUE,
          MDP_RemoveDates = TRUE,
          MDP_MissFactor = '0',
          MDP_MissNum = -1)

      } else {

        # i = 1 Define IDcols----
        if(DebugMode) print('# i = 1 Define IDcols----')
        IDcols <- eval(TargetColumnName)

        # i = 1 Score Model No Group Variables----
        if(DebugMode) print('# i = 1 Score Model No Group Variables----')
        Preds <- AutoCatBoostScoring(
          TargetType = 'multiregression',
          ScoringData = Step1SCore,
          FeatureColumnNames = ModelFeatures,
          FactorLevelsList = TestModel$FactorLevelsList,
          IDcols = IDcols,
          OneHot = FALSE,
          ModelObject = Model,
          ModelPath = getwd(),
          ModelID = 'ModelTest',
          ReturnFeatures = TRUE,
          TransformNumeric = FALSE,
          BackTransNumeric = FALSE,
          TransformationObject = NULL,
          TransID = NULL,
          TransPath = NULL,
          TargetColumnName = TargetColumnName,
          MDP_Impute = TRUE,
          MDP_CharToFactor = TRUE,
          MDP_RemoveDates = TRUE,
          MDP_MissFactor = '0',
          MDP_MissNum = -1)
      }

      # CatFeatures ----
      CatFeatures <- names(data)[sort(c(as.numeric(which(sapply(data, is.factor))),as.numeric(which(sapply(data, is.character)))))]

      # Data Wrangline: grab historical data and one more future record----
      if(Difference) {
        if(eval(TargetColumnName) %chin% names(Step1SCore)) {
          if(eval(TargetColumnName) %chin% names(Preds)) {
            data.table::set(Preds, j = eval(TargetColumnName), value = NULL)
          }
        }
        if(eval(DateColumnName) %chin% names(Step1SCore)) data.table::set(Step1SCore, j = eval(DateColumnName), value = NULL)
        if(eval(DateColumnName) %chin% names(Preds)) data.table::set(Preds, j = eval(DateColumnName), value = NULL)
        if(!is.null(GroupVariables)) {
          UpdateData <- cbind(FutureDateData, Step1SCore[, .SD, .SDcols = eval(TargetColumnName)],Preds)
        } else {
          UpdateData <- cbind(FutureDateData[2L:(nrow(Step1SCore)+1L)], Step1SCore[, .SD, .SDcols = eval(TargetColumnName)],Preds)
        }
        data.table::setnames(UpdateData, 'FutureDateData', eval(DateColumnName))
      } else {

        # NonNeg Preds
        if(NonNegativePred) for(zz in seq_len(length(TargetColumnName))) Preds[, paste0('Predictions.V',zz) := data.table::fifelse(get(paste0('Predictions.V',zz)) < 0.5, 0, get(paste0('Predictions.V',zz)))]

        # Convert dummies back to categoricals
        zz <- names(Preds)[which(names(Preds) %like% 'Predictions.V')]
        xx <- Preds[, .SD, .SDcols = c(names(Preds)[which(!names(Preds) %chin% c(zz,ModelFeatures,TargetColumnName))])]
        for(cat in rev(CatFeatures)) {
          aaa <- data.table::copy(xx[, .SD, .SDcols = c(paste0(eval(cat), '_', as.character(unique(data[[eval(cat)]]))))])
          aa <- data.table::melt.data.table(data = aaa, id.vars = NULL, measure.vars = c(paste0(eval(cat), '_', as.character(unique(data[[eval(cat)]])))))[value != 0][, value := NULL]
          aa[, variable := gsub(pattern = paste0(cat,'_'), replacement = '', x = variable)]
          data.table::setnames(aa, 'variable', cat)
          data.table::set(Preds, j = c(paste0(eval(cat), '_', as.character(unique(data[[eval(cat)]])))), value = NULL)
          Preds <- cbind(Preds, aa)
        }

        # Combine data sets
        UpdateData <- cbind(FutureDateData[1L:N],Preds)
        data.table::setnames(UpdateData,c('V1'),c(eval(DateColumnName)))
      }
    } else {
      if(!is.null(GroupVariables)) {

        # Modify target reference ----
        if(Difference) IDcols = 'ModTarget' else IDcols <- eval(TargetColumnName)

        # GroupVar or Hierarchical----
        if(!is.null(HierarchGroups)) {
          temp <- data.table::copy(UpdateData[, ID := 1:.N, by = c(eval(GroupVariables))])
          temp <- temp[ID == N][, ID := NULL]
        } else {
          temp <- data.table::copy(UpdateData[, ID := 1:.N, by = 'GroupVar'])
          temp <- temp[ID == N][, ID := NULL]
        }

        # Score model----
        Preds <- AutoCatBoostScoring(
          TargetType = 'multiregression',
          ScoringData = temp,
          FeatureColumnNames = ModelFeatures,
          FactorLevelsList = TestModel$FactorLevelsList,
          IDcols = IDcols,
          OneHot = FALSE,
          ModelObject = Model,
          ModelPath = getwd(),
          ModelID = 'ModelTest',
          ReturnFeatures = FALSE,
          TransformNumeric = FALSE,
          BackTransNumeric = FALSE,
          TargetColumnName = TargetColumnName,
          TransformationObject = NULL,
          TransID = NULL,
          TransPath = NULL,
          MDP_Impute = TRUE,
          MDP_CharToFactor = TRUE,
          MDP_RemoveDates = TRUE,
          MDP_MissFactor = '0',
          MDP_MissNum = -1)

        # Update data group case----
        if(DebugMode) print('Update data group case----')
        for(zz in seq_len(length(TargetColumnName))) data.table::setnames(Preds, paste0('Predictions.V',zz), paste0('Preds.V',zz))
        if(NonNegativePred & !Difference) for(zz in seq_len(length(TargetColumnName))) Preds[, paste0('Preds.V',zz) := data.table::fifelse(get(paste0('Preds.V',zz)) < 0.5, 0, get(paste0('Preds.V',zz)))]
        Preds <- cbind(UpdateData[ID == N], Preds)
        if(Difference) for(zz in seq_len(length(TargetColumnName))) Preds[, paste0('ModTarget.V',zz) := get(paste0('Preds.V',zz))][, eval(TargetColumnName[zz]) := get(paste0('Preds.V',zz))] else Preds[, eval(TargetColumnName[zz]) := get(paste0('Preds.V',zz))]
        for(zz in seq_len(length(TargetColumnName))) Preds[, paste0('Predictions.V',zz) := get(paste0('Preds.V',zz))][, paste0('Preds.V',zz) := NULL]
        UpdateData <- UpdateData[ID != N]
        if(any(class(UpdateData[[eval(DateColumnName)]]) %chin% c('POSIXct','POSIXt')) & any(class(Preds[[eval(DateColumnName)]]) == 'Date')) UpdateData[, eval(DateColumnName) := as.Date(get(DateColumnName))]
        UpdateData <- data.table::rbindlist(list(UpdateData, Preds))
        if(Difference) for(zz in seq_len(length(TargetColumnName))) UpdateData[ID %in% c(N-1,N), eval(TargetColumnName[zz]) := cumsum(get(TargetColumnName[zz])), by = 'GroupVar']
        UpdateData[, ID := NULL]

      } else {

        # Score Model----
        Preds <- AutoCatBoostScoring(
          TargetType = 'multiregression',
          ScoringData = UpdateData[.N, ],
          FeatureColumnNames = ModelFeatures,
          FactorLevelsList = TestModel$FactorLevelsList,
          IDcols = NULL,
          OneHot = FALSE,
          ModelObject = Model,
          ModelPath = getwd(),
          ModelID = 'ModelTest',
          ReturnFeatures = FALSE,
          TransformNumeric = FALSE,
          BackTransNumeric = FALSE,
          TargetColumnName = TargetColumnName,
          TransformationObject = NULL,
          TransID = NULL,
          TransPath = NULL,
          MDP_Impute = TRUE,
          MDP_CharToFactor = TRUE,
          MDP_RemoveDates = TRUE,
          MDP_MissFactor = '0',
          MDP_MissNum = -1)

        # Update data non-group case----
        if(DebugMode) print('Update data non-group case----')
        for(zz in seq_along(TargetColumnName)) {
          data.table::set(UpdateData, i = N, j = names(UpdateData)[1L + zz], value = Preds[[paste0('Predictions.V',zz)]])
        }
      }
    }

    ###############
    # Forecasting
    ###############

    # Update lags and moving average features for next run----
    if(i != ForecastRuns+1L) {

      # Timer----
      if(DebugMode) print('Timer----')
      if(Timer) if(i != 1) print(paste('Forecast future step: ', i-1))
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
  Output <- CarmaReturnDataPrep(UpdateData.=UpdateData, FutureDateData.=FutureDateData, dataStart.=dataStart, DateColumnName.=DateColumnName, TargetColumnName.=TargetColumnName, GroupVariables.=GroupVariables, Difference.=Difference, TargetTransformation.=TargetTransformation, TransformObject.=TransformObject, NonNegativePred.=NonNegativePred)
  UpdateData <- Output$UpdateData; Output$UpdateData <- NULL
  TransformObject <- Output$TransformObject; rm(Output)

  # Return ----
  return(list(
    Forecast = UpdateData,
    ModelInformation = TestModel,
    TransformationDetail = if(exists('TransformObject') && !is.null(TransformObject)) TransformObject else NULL))
}
