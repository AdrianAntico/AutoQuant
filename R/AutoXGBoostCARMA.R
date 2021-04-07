#' @title AutoXGBoostCARMA
#'
#' @description AutoXGBoostCARMA Mutlivariate Forecasting with calendar variables, Holiday counts, holiday lags, holiday moving averages, differencing, transformations, interaction-based categorical encoding using target variable and features to generate various time-based aggregated lags, moving averages, moving standard deviations, moving skewness, moving kurtosis, moving quantiles, parallelized interaction-based fourier pairs by grouping variables, and Trend Variables.
#'
#' @author Adrian Antico
#' @family Automated Panel Data Forecasting
#'
#' @param data Supply your full series data set here
#' @param NonNegativePred TRUE or FALSE
#' @param RoundPreds Rounding predictions to an integer value. TRUE or FALSE. Defaults to FALSE
#' @param TrainOnFull Set to TRUE to train on full data
#' @param TargetColumnName List the column name of your target variables column. E.g. "Target"
#' @param DateColumnName List the column name of your date column. E.g. "DateTime"
#' @param HierarchGroups = NULL Character vector or NULL with names of the columns that form the interaction hierarchy
#' @param GroupVariables Defaults to NULL. Use NULL when you have a single series. Add in GroupVariables when you have a series for every level of a group or multiple groups.
#' @param TimeUnit List the time unit your data is aggregated by. E.g. "1min", "5min", "10min", "15min", "30min", "hour", "day", "week", "month", "quarter", "year"
#' @param TimeGroups Select time aggregations for adding various time aggregated GDL features.
#' @param FC_Periods Set the number of periods you want to have forecasts for. E.g. 52 for weekly data to forecast a year ahead
#' @param PDFOutputPath Supply a path to save model insights to PDF
#' @param SaveDataPath Path to save modeling data
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
#' @param Difference Set to TRUE to put the I in ARIMA
#' @param AnomalyDetection NULL for not using the service. Other, provide a list, e.g. AnomalyDetection = list("tstat_high" = 4, tstat_low = -4)
#' @param FourierTerms Set to the max number of pairs
#' @param CalendarVariables NULL, or select from "second", "minute", "hour", "wday", "mday", "yday", "week", "wom", "isoweek", "month", "quarter", "year"
#' @param HolidayVariable NULL, or select from "USPublicHolidays", "EasterGroup", "ChristmasGroup", "OtherEcclesticalFeasts"
#' @param HolidayLookback Number of days in range to compute number of holidays from a given date in the data. If NULL, the number of days are computed for you.
#' @param HolidayLags Number of lags for the holiday counts
#' @param HolidayMovingAverages Number of moving averages for holiday counts
#' @param TimeTrendVariable Set to TRUE to have a time trend variable added to the model. Time trend is numeric variable indicating the numeric value of each record in the time series (by group). Time trend starts at 1 for the earliest point in time and increments by one for each success time point.
#' @param DataTruncate Set to TRUE to remove records with missing values from the lags and moving average features created
#' @param ZeroPadSeries NULL to do nothing. Otherwise, set to "maxmax", "minmax", "maxmin", "minmin". See \code{\link{TimeSeriesFill}} for explanations of each type
#' @param SplitRatios E.g c(0.7,0.2,0.1) for train, validation, and test sets
#' @param PartitionType Select "random" for random data partitioning "time" for partitioning by time frames
#' @param Timer Setting to TRUE prints out the forecast number while it is building
#' @param DebugMode Setting to TRUE generates printout of all header code comments during run time of function
#' @param TreeMethod Choose from "hist", "gpu_hist"
#' @param NThreads Set the maximum number of threads you'd like to dedicate to the model run. E.g. 8
#' @param EvalMetric Select from "r2", "RMSE", "MSE", "MAE"
#' @param LossFunction Default is 'reg:squarederror'. Other options include 'reg:squaredlogerror', 'reg:pseudohubererror', 'count:poisson', 'survival:cox', 'survival:aft', 'aft_loss_distribution', 'reg:gamma', 'reg:tweedie'
#' @param GridTune Set to TRUE to run a grid tune
#' @param GridEvalMetric This is the metric used to find the threshold 'poisson', 'mae', 'mape', 'mse', 'msle', 'kl', 'cs', 'r2'
#' @param ModelCount Set the number of models to try in the grid tune
#' @param MaxRunsWithoutNewWinner Number of consecutive runs without a new winner in order to terminate procedure
#' @param MaxRunMinutes Default 24L*60L
#' @param NTrees Select the number of trees you want to have built to train the model
#' @param LearningRate Learning Rate
#' @param MaxDepth Depth
#' @param MinChildWeight Records in leaf
#' @param SubSample Random forecast setting
#' @param ColSampleByTree Self explanatory
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
#'  # Build forecast
#' XGBoostResults <- AutoXGBoostCARMA(
#'
#'   # Data Artifacts
#'   data = data,
#'   NonNegativePred = FALSE,
#'   RoundPreds = FALSE,
#'   TargetColumnName = "Weekly_Sales",
#'   DateColumnName = "Date",
#'   HierarchGroups = NULL,
#'   GroupVariables = c("Store","Dept"),
#'   TimeUnit = "weeks",
#'   TimeGroups = c("weeks","months"),
#'
#'   # Data Wrangling Features
#'   ZeroPadSeries = NULL,
#'   DataTruncate = FALSE,
#'   SplitRatios = c(1 - 10 / 138, 10 / 138),
#'   PartitionType = "timeseries",
#'   AnomalyDetection = NULL,
#'
#'   # Productionize
#'   FC_Periods = 0,
#'   TrainOnFull = FALSE,
#'   NThreads = 8,
#'   Timer = TRUE,
#'   DebugMode = FALSE,
#'   SaveDataPath = NULL,
#'   PDFOutputPath = NULL,
#'
#'   # Target Transformations
#'   TargetTransformation = TRUE,
#'   Methods = c("BoxCox", "Asinh", "Asin", "Log",
#'               "LogPlus1", "Sqrt", "Logit","YeoJohnson"),
#'   Difference = FALSE,
#'
#'   # Features
#'   Lags = list("weeks" = seq(1L, 10L, 1L),
#'               "months" = seq(1L, 5L, 1L)),
#'   MA_Periods = list("weeks" = seq(5L, 20L, 5L),
#'                     "months" = seq(2L, 10L, 2L)),
#'   SD_Periods = NULL,
#'   Skew_Periods = NULL,
#'   Kurt_Periods = NULL,
#'   Quantile_Periods = NULL,
#'   Quantiles_Selected = c("q5","q95"),
#'   XREGS = xregs,
#'   FourierTerms = 4,
#'   CalendarVariables = c("week", "wom", "month", "quarter"),
#'   HolidayVariable = c("USPublicHolidays","EasterGroup",
#'     "ChristmasGroup","OtherEcclesticalFeasts"),
#'   HolidayLookback = NULL,
#'   HolidayLags = 1,
#'   HolidayMovingAverages = 1:2,
#'   TimeTrendVariable = TRUE,
#'
#'   # ML eval args
#'   TreeMethod = "hist",
#'   EvalMetric = "RMSE",
#'   LossFunction = 'reg:squarederror',
#'
#'   # ML grid tuning
#'   GridTune = FALSE,
#'   ModelCount = 5,
#'   MaxRunsWithoutNewWinner = 20L,
#'   MaxRunMinutes = 24L*60L,
#'
#'   # ML args
#'   NTrees = 300,
#'   LearningRate = 0.3,
#'   MaxDepth = 9L,
#'   MinChildWeight = 1.0,
#'   SubSample = 1.0,
#'   ColSampleByTree = 1.0)
#'
#' UpdateMetrics <- print(
#'   XGBoostResults$ModelInformation$EvaluationMetrics[
#'     Metric == "MSE", MetricValue := sqrt(MetricValue)])
#' print(UpdateMetrics)
#' XGBoostResults$ModelInformation$EvaluationMetricsByGroup[order(-R2_Metric)]
#' XGBoostResults$ModelInformation$EvaluationMetricsByGroup[order(MAE_Metric)]
#' XGBoostResults$ModelInformation$EvaluationMetricsByGroup[order(MSE_Metric)]
#' XGBoostResults$ModelInformation$EvaluationMetricsByGroup[order(MAPE_Metric)]
#' }
#' @return See examples
#' @export
AutoXGBoostCARMA <- function(data,
                             NonNegativePred = FALSE,
                             RoundPreds = FALSE,
                             TrainOnFull = FALSE,
                             TargetColumnName = NULL,
                             DateColumnName = NULL,
                             HierarchGroups = NULL,
                             GroupVariables = NULL,
                             FC_Periods = 5,
                             SaveDataPath = NULL,
                             PDFOutputPath = NULL,
                             TimeUnit = "week",
                             TimeGroups = c("weeks","months"),
                             TargetTransformation = FALSE,
                             Methods = c("YeoJohnson", "BoxCox", "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit"),
                             AnomalyDetection = NULL,
                             XREGS = NULL,
                             Lags = c(1:5),
                             MA_Periods = c(1:5),
                             SD_Periods = NULL,
                             Skew_Periods = NULL,
                             Kurt_Periods = NULL,
                             Quantile_Periods = NULL,
                             Quantiles_Selected = NULL,
                             Difference = TRUE,
                             FourierTerms = 6,
                             CalendarVariables = c("second", "minute", "hour", "wday", "mday", "yday", "week", "wom", "isoweek", "month", "quarter", "year"),
                             HolidayVariable = c("USPublicHolidays","EasterGroup","ChristmasGroup","OtherEcclesticalFeasts"),
                             HolidayLookback = NULL,
                             HolidayLags = 1L,
                             HolidayMovingAverages = 3L,
                             TimeTrendVariable = FALSE,
                             DataTruncate = FALSE,
                             ZeroPadSeries = NULL,
                             SplitRatios = c(1 - 10/100, 10/100),
                             TreeMethod = "hist",
                             NThreads = max(1, parallel::detectCores()-2L),
                             PartitionType = "random",
                             Timer = TRUE,
                             DebugMode = FALSE,
                             EvalMetric = "MAE",
                             LossFunction = 'reg:squarederror',
                             GridTune = FALSE,
                             GridEvalMetric = "mae",
                             ModelCount = 30L,
                             MaxRunsWithoutNewWinner = 20L,
                             MaxRunMinutes = 24L*60L,
                             NTrees = 1000L,
                             LearningRate = 0.3,
                             MaxDepth = 9L,
                             MinChildWeight = 1.0,
                             SubSample = 1.0,
                             ColSampleByTree = 1.0) {

  # Purified args: see CARMA HELPER FUNCTIONS ----
  Args <- CARMA_Define_Args(TimeUnit=TimeUnit,TimeGroups=TimeGroups,HierarchGroups=HierarchGroups,GroupVariables=GroupVariables,FC_Periods=FC_Periods,PartitionType=PartitionType,TrainOnFull=TrainOnFull,SplitRatios=SplitRatios)
  IndepentVariablesPass <- Args$IndepentVariablesPass
  HoldOutPeriods <- Args$HoldOutPeriods
  HierarchGroups <- Args$HierarchGroups
  GroupVariables <- Args$GroupVariables
  TimeGroups <- Args$TimeGroups
  FC_Periods <- Args$FC_Periods
  TimeGroup <- Args$TimeGroupPlaceHolder
  TimeUnit <- Args$TimeUnit

  # Additonal Args check ----
  if(!is.null(HolidayLookback) && !is.numeric(HolidayLookback)) stop("HolidayLookback has to be numeric")
  if(!tolower(EvalMetric) %chin% c("RMSE","MAE","MAPE","r2")) EvalMetric <- "RMSE"
  if(!TrainOnFull) HoldOutPeriods <- round(SplitRatios[2]*length(unique(data[[eval(DateColumnName)]])),0)

  # Convert data to data.table ----
  if(DebugMode) print("Convert data to data.table----")
  if(!data.table::is.data.table(data)) data.table::setDT(data)
  if(!is.null(XREGS)) if(!data.table::is.data.table(XREGS)) data.table::setDT(XREGS)

  # Feature Engineering: Add Zero Padding for missing dates ----
  if(DebugMode) print("Feature Engineering: Add Zero Padding for missing dates----")
  if(!is.null(ZeroPadSeries)) {
    data <- TimeSeriesFill(data, DateColumnName=eval(DateColumnName), GroupVariables=GroupVariables, TimeUnit=TimeUnit, FillType=ZeroPadSeries, MaxMissingPercent=0.0, SimpleImpute=FALSE)
    data <- RemixAutoML::ModelDataPrep(data=data, Impute=TRUE, CharToFactor=FALSE, FactorToChar=FALSE, IntToNumeric=FALSE, LogicalToBinary=FALSE, DateToChar=FALSE, RemoveDates=FALSE, MissFactor="0", MissNum=0, IgnoreCols=NULL)
  } else {
    temp <- RemixAutoML::TimeSeriesFill(data, DateColumnName=eval(DateColumnName), GroupVariables=GroupVariables, TimeUnit=TimeUnit, FillType="maxmax", MaxMissingPercent=0.25, SimpleImpute=FALSE)
    if(temp[,.N] != data[,.N]) stop("There are missing dates in your series. You can utilize the ZeroPadSeries argument to handle this or manage it before running the function")
  }

  # Modify FC_Periods ----
  if(DebugMode) print("# Check lengths of XREGS")
  Output <- CarmaFCHorizon(data.=data, XREGS.=XREGS, TrainOnFull.=TrainOnFull, Difference.= Difference, FC_Periods.=FC_Periods, HoldOutPeriods.=HoldOutPeriods, DateColumnName.=DateColumnName)
  FC_Periods <- Output$FC_Periods
  HoldOutPeriods <- Output$HoldOutPeriods; rm(Output)

  # Merge data and XREG for Training ----
  if(DebugMode) print("merging xregs to data")
  if(!is.null(XREGS)) {
    Output <- CarmaMergeXREGS(data.=data, XREGS.=XREGS, TargetColumnName.=TargetColumnName, GroupVariables.=GroupVariables, DateColumnName.=DateColumnName)
    data <- Output$data; Output$data <- NULL
    XREGS <- Output$XREGS; rm(Output)
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

  # Data Wrangling: Standardize column ordering ----
  if(DebugMode) print("Data Wrangling: Standardize column ordering----")
  if(!is.null(GroupVariables)) data.table::setcolorder(data, c("GroupVar", eval(DateColumnName), eval(TargetColumnName))) else data.table::setcolorder(data, c(eval(DateColumnName), eval(TargetColumnName)))

  # Data Wrangling: Convert DateColumnName to Date or POSIXct ----
  if(DebugMode) print("Data Wrangling: Convert DateColumnName to Date or POSIXct----")
  Output <- CarmaDateStandardize(data.=data, XREGS.=NULL, DateColumnName.=DateColumnName, TimeUnit.=TimeUnit)
  data <- Output$data; Output$data <- NULL
  XREGS <- Output$XREGS; rm(Output)

  # Data Wrangling: Ensure TargetColumnName is Numeric ----
  if(DebugMode) print("Data Wrangling: Ensure TargetColumnName is Numeric----")
  if(!is.numeric(data[[eval(TargetColumnName)]])) data[, eval(TargetColumnName) := as.numeric(get(TargetColumnName))]

  # Variables for Program: Store number of data partitions in NumSets ----
  if(DebugMode) print("Variables for Program: Store number of data partitions in NumSets----")
  NumSets <- length(SplitRatios)

  # Variables for Program: Store Maximum Value of TargetColumnName in val ----
  if(DebugMode) print("Variables for Program: Store Maximum Value of TargetColumnName in val----")
  if(is.list(Lags) && is.list(MA_Periods)) val <- max(unlist(Lags), unlist(MA_Periods)) else val <- max(Lags, MA_Periods)

  # Data Wrangling: Sort data by GroupVar then DateColumnName ----
  if(DebugMode) print("Data Wrangling: Sort data by GroupVar then DateColumnName----")
  if(!is.null(GroupVariables)) data <- data[order(GroupVar, get(DateColumnName))] else data <- data[order(get(DateColumnName))]

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
    } else if(!is.null(HierarchGroups)) {
      if(!"GroupVar" %chin% names(data)) data[, GroupVar := do.call(paste, c(.SD, sep = " ")), .SDcols = HierarchGroups]
    }
  }

  # Feature Engineering: Add Create Calendar Variables ----
  if(DebugMode) print("Feature Engineering: Add Create Calendar Variables----")
  if(!is.null(CalendarVariables)) data <- CreateCalendarVariables(data=data, DateCols=eval(DateColumnName), AsFactor=FALSE, TimeUnits=CalendarVariables)

  # Feature Engineering: Add Create Holiday Variables ----
  if(DebugMode) print("Feature Engineering: Add Create Holiday Variables ----")
  if(!is.null(HolidayVariable)) {
    data <- CreateHolidayVariables(data, DateCols = eval(DateColumnName), LookbackDays = if(!is.null(HolidayLookback)) HolidayLookback else LB(TimeUnit), HolidayGroups = HolidayVariable, Holidays = NULL)
    if(!(tolower(TimeUnit) %chin% c("1min","5min","10min","15min","30min","hour"))) {
      data[, eval(DateColumnName) := lubridate::as_date(get(DateColumnName))]
    } else {
      data[, eval(DateColumnName) := as.POSIXct(get(DateColumnName))]
    }
  }

  # Anomaly detection by Group and Calendar Vars ----
  if(!is.null(AnomalyDetection)) {
    data <- RemixAutoML::GenTSAnomVars(
      data = data, ValueCol = eval(TargetColumnName),
      GroupVars = if(!is.null(CalendarVariables) && !is.null(GroupVariables)) c("GroupVar", paste0(DateColumnName, "_", CalendarVariables[1])) else if(!is.null(GroupVariables)) "GroupVar" else NULL,
      DateVar = eval(DateColumnName),
      HighThreshold = AnomalyDetection$tstat_high,
      LowThreshold = AnomalyDetection$tstat_low,
      KeepAllCols = TRUE,
      IsDataScaled = FALSE)
    data[, paste0(eval(TargetColumnName), "_zScaled") := NULL]
    data[, ":=" (RowNumAsc = NULL, CumAnomHigh = NULL, CumAnomLow = NULL, AnomHighRate = NULL, AnomLowRate = NULL)]
  }

  # Feature Engineering: Add Target Transformation----
  if(DebugMode) print("Feature Engineering: Add Target Transformation----")
  if(TargetTransformation) {
    TransformResults <- AutoTransformationCreate(data, ColumnNames=TargetColumnName, Methods=Methods, Path=NULL, TransID="Trans", SaveOutput=FALSE)
    data <- TransformResults$Data
    TransformObject <- TransformResults$FinalResults
  } else {
    TransformObject <- NULL
  }

  # Copy data for non grouping + difference----
  if(DebugMode) print("Copy data for non grouping + difference----")
  if(is.null(GroupVariables) && Difference) antidiff <- data.table::copy(data[, .SD, .SDcols = c(eval(TargetColumnName),eval(DateColumnName))])

  # Feature Engineering: Add Difference Data----
  if(DebugMode) print("Feature Engineering: Add Difference Data----")
  Output <- CarmaDifferencing(GroupVariables.=GroupVariables, Difference.=Difference, data.=data, TargetColumnName.=TargetColumnName, FC_Periods.=FC_Periods)
  data <- Output$data; Output$data <- NULL
  dataStart <- Output$dataStart; Output$dataStart <- NULL
  FC_Periods <- Output$FC_Periods; Output$FC_Periods <- NULL
  Train <- Output$Train; rm(Output)

  # Feature Engineering: Lags and Rolling Stats ----
  if(DebugMode) print("Feature Engineering: Lags and Rolling Stats ----")
  Output <- CarmaTimeSeriesFeatures(data.=data, TargetColumnName.=TargetColumnName, DateColumnName.=DateColumnName, GroupVariables.=GroupVariables, HierarchGroups.=HierarchGroups, Difference.=Difference, TimeGroups.=TimeGroups, TimeUnit.=TimeUnit, Lags.=Lags, MA_Periods.=MA_Periods, SD_Periods.=SD_Periods, Skew_Periods.=Skew_Periods, Kurt_Periods.=Kurt_Periods, Quantile_Periods.=Quantile_Periods, Quantiles_Selected.=Quantiles_Selected, HolidayVariable.=HolidayVariable, HolidayLags.=HolidayLags, HolidayMovingAverages.=HolidayMovingAverages, DebugMode.=DebugMode)
  IndependentSupplyValue <- Output$IndependentSupplyValue; Output$IndependentSupplyValue <- NULL
  HierarchSupplyValue <- Output$HierarchSupplyValue; Output$HierarchSupplyValue <- NULL
  GroupVarVector <- Output$GroupVarVector; Output$GroupVarVector <- NULL
  Categoricals <- Output$Categoricals; Output$Categoricals <- NULL
  data <- Output$data; rm(Output)

  # Create GroupVar ----
  if(!is.null(GroupVariables)) if(!"GroupVar" %chin% names(data)) data[, GroupVar := do.call(paste, c(.SD, sep = " ")), .SDcols = GroupVariables]

  # Data Wrangling: ModelDataPrep() to prepare data----
  if(DebugMode) print("Data Wrangling: ModelDataPrep() to prepare data----")
  data <- ModelDataPrep(
    data,
    Impute = TRUE,
    CharToFactor = TRUE,
    RemoveDates = FALSE,
    MissFactor = "0",
    MissNum    = -1)

  # Data Wrangling: Remove dates with imputed data from the DT_GDL_Feature_Engineering() features ----
  if(DebugMode) print("Data Wrangling: Remove dates with imputed data from the DT_GDL_Feature_Engineering() features ----")
  if(DataTruncate && !is.null(Lags)) data <- CarmaTruncateData(data.=data, DateColumnName.=DateColumnName, TimeUnit.=TimeUnit)

  # Feature Engineering: Add TimeTrend Variable ----
  if(DebugMode) print("Feature Engineering: Add TimeTrend Variable----")
  if(TimeTrendVariable) {
    if(!is.null(GroupVariables)) data[, TimeTrend := seq_len(.N), by = "GroupVar"] else data[, TimeTrend := seq_len(.N)]
  }

  # Store Date Info----
  if(DebugMode) print("Store Date Info----")
  FutureDateData <- unique(data[, get(DateColumnName)])

  # Return engineered data before Partitioning ----
  if(!is.null(SaveDataPath)) {
    data.table::fwrite(data, file.path(SaveDataPath, "ModelData.csv"))
  }

  # Data Wrangling: Partition data with AutoDataPartition ----
  if(DebugMode) print("Data Wrangling: Partition data with AutoDataPartition()----")
  Output <- CarmaPartition(data.=data, SplitRatios.=SplitRatios, TrainOnFull.=TrainOnFull, NumSets.=NumSets, PartitionType.=PartitionType, GroupVariables.=GroupVariables, DateColumnName.=DateColumnName)
  train <- Output$train; Output$train <- NULL
  valid <- Output$valid; Output$valid <- NULL
  data <- Output$data; Output$data <- NULL
  test <- Output$test; rm(Output)

  # Variables for CARMA function IDcols ----
  if(DebugMode) print("Variables for CARMA function:IDcols----")
  IDcols <- which(names(data) %chin% DateColumnName)
  if(Difference && !is.null(GroupVariables)) IDcols <- c(IDcols, which(names(data) == TargetColumnName))

  # Data Wrangling: copy data or train for later in function since AutoRegression will modify data and train----
  if(DebugMode) print("Data Wrangling: copy data or train for later in function since AutoRegression will modify data and train----")
  if(!is.null(GroupVariables)) {
    data.table::setorderv(x = data, cols = c("GroupVar",eval(DateColumnName)), order = c(1,1))
    Step1SCore <- data.table::copy(data)
  } else {
    data.table::setorderv(x = data, cols = c(eval(DateColumnName)), order = c(1))
    Step1SCore <- data.table::copy(data)
  }

  # Define Target and Features ----
  if(DebugMode) print("Define ML args ----")
  Output <- CarmaFeatures(data.=data, train.=train, XREGS.=XREGS, Difference.=Difference, TargetColumnName.=TargetColumnName, DateColumnName.=DateColumnName, GroupVariables.=GroupVariables)
  ModelFeatures <- Output$ModelFeatures
  TargetVariable <- Output$TargetVariable; rm(Output)

  # Machine Learning: Build Model ----
  if(DebugMode) options(warn = 0)
  TestModel <- RemixAutoML::AutoXGBoostRegression(

    # GPU or CPU
    TreeMethod = TreeMethod,
    NThreads = NThreads,

    # Metadata arguments
    model_path = getwd(),
    metadata_path = if(!is.null(PDFOutputPath)) PDFOutputPath else getwd(),
    ModelID = "XGBoost",
    ReturnFactorLevels = TRUE,
    ReturnModelObjects = TRUE,
    SaveModelObjects = FALSE,
    SaveInfoToPDF = if(!is.null(PDFOutputPath)) TRUE else FALSE,

    # Data arguments
    data = train,
    TrainOnFull = TrainOnFull,
    ValidationData = valid,
    TestData = test,
    TargetColumnName = TargetVariable,
    FeatureColNames = ModelFeatures,
    IDcols = IDcols,
    TransformNumericColumns = NULL,
    Methods = NULL,

    # Model evaluation
    LossFunction = LossFunction,
    eval_metric = EvalMetric,
    NumOfParDepPlots = 10,

    # Grid tuning arguments - PassInGrid is the best of GridMetrics
    PassInGrid = NULL,
    GridTune = GridTune,
    grid_eval_metric = GridEvalMetric,
    BaselineComparison = "default",
    MaxModelsInGrid = ModelCount,
    MaxRunsWithoutNewWinner = MaxRunsWithoutNewWinner,
    MaxRunMinutes = MaxRunMinutes,
    Verbose = 1L,

    # ML Args
    Trees = NTrees,
    eta = LearningRate,
    max_depth = MaxDepth,
    min_child_weight = MinChildWeight,
    subsample = SubSample,
    colsample_bytree = ColSampleByTree)

  # Return if TrainOnFull is FALSE ----
  if(!TrainOnFull) return(TestModel)

  # Variable for storing ML model: Pull model object out of TestModel list ----
  if(DebugMode) print("Variable for storing ML model: Pull model object out of TestModel list----")
  Model <- TestModel$Model

  # Grab factor level list ----
  if(DebugMode) print("Grab factor level list----")
  if(!is.null(GroupVariables)) FactorList <- TestModel$FactorLevelsList else FactorList <- NULL

  # Variable for interation counts: max number of rows in Step1SCore data.table across all group ----
  if(DebugMode) print("Variable for interation counts: max number of rows in Step1SCore data.table across all group ----")
  N <- CarmaRecordCount(GroupVariables.=GroupVariables,Difference.=Difference, Step1SCore.=Step1SCore)

  # Number of forecast periods----
  if(DebugMode) print("Number of forecast periods----")
  ForecastRuns <- FC_Periods

  #----

  #----

  # ARMA PROCESS FORECASTING----
  if(DebugMode) print("ARMA PROCESS FORECASTING----")
  if(DebugMode) print("ARMA PROCESS FORECASTING----")
  if(DebugMode) print("ARMA PROCESS FORECASTING----")
  if(DebugMode) print("ARMA PROCESS FORECASTING----")
  if(DebugMode) print("ARMA PROCESS FORECASTING----")

  #----

  #----

  # Begin loop for generating forecasts ----
  for(i in seq_len(ForecastRuns + 1L)) {

    # Row counts----
    if(i != 1) N <- N + 1L

    ###############
    # ML Scoring
    ###############

    # Machine Learning: Generate predictions----
    if(DebugMode) print("# Machine Learning: Generate predictions----")
    if(i == 1L) {
      if(!is.null(GroupVariables)) {

        # i = 1 Define IDcols----
        if(DebugMode) print("# i = 1 Define IDcols----")
        if(Difference) IDcols = "ModTarget" else IDcols <- eval(TargetColumnName)

        # Score model----
        Preds <- AutoXGBoostScoring(
          TargetType = "regression",
          ScoringData = Step1SCore,
          FeatureColumnNames = ModelFeatures,
          OneHot = FALSE,
          IDcols = IDcols,
          ModelObject = Model,
          ModelPath = getwd(),
          ModelID = "ModelTest",
          ReturnFeatures = TRUE,
          TransformNumeric = FALSE,
          BackTransNumeric = FALSE,
          TargetColumnName = NULL,
          TransformationObject = NULL,
          FactorLevelsList = FactorList,
          TransID = NULL,
          TransPath = NULL,
          MDP_Impute = TRUE,
          MDP_CharToFactor = TRUE,
          MDP_RemoveDates = TRUE,
          MDP_MissFactor = "0",
          MDP_MissNum = -1)

      } else {

        # Score model----
        if(DebugMode) print("# i = 1 Define IDcols----")
        IDcols <- eval(TargetVariable)
        Preds <- AutoXGBoostScoring(
          TargetType = "regression",
          ScoringData = Step1SCore,
          FeatureColumnNames = ModelFeatures,
          OneHot = FALSE,
          IDcols = IDcols,
          ModelObject = Model,
          ModelPath = getwd(),
          ModelID = "ModelTest",
          ReturnFeatures = TRUE,
          TransformNumeric = FALSE,
          BackTransNumeric = FALSE,
          TargetColumnName = NULL,
          TransformationObject = NULL,
          FactorLevelsList = FactorList,
          TransID = NULL,
          TransPath = NULL,
          MDP_Impute = TRUE,
          MDP_CharToFactor = TRUE,
          MDP_RemoveDates = TRUE,
          MDP_MissFactor = "0",
          MDP_MissNum = -1)
      }

      # Data Wrangline: grab historical data and one more future record----
      if(Difference) {
        if(eval(TargetColumnName) %chin% names(Step1SCore)) if(eval(TargetColumnName) %chin% names(Preds)) data.table::set(Preds, j = eval(TargetColumnName), value = NULL)
        if(eval(DateColumnName) %chin% names(Step1SCore)) data.table::set(Step1SCore, j = eval(DateColumnName), value = NULL)
        if(eval(DateColumnName) %chin% names(Preds)) data.table::set(Preds, j = eval(DateColumnName), value = NULL)
        UpdateData <- cbind(FutureDateData[1L:N], Step1SCore[, .SD, .SDcols = eval(TargetColumnName)], Preds)
        data.table::setnames(UpdateData, c("V1"), c(eval(DateColumnName)))
      } else {
        if(NonNegativePred) Preds[, Predictions := data.table::fifelse(Predictions < 0.5, 0, Predictions)]
        if(RoundPreds) Preds[, Predictions := round(Predictions)]
        UpdateData <- cbind(FutureDateData[1L:N], Preds)
        data.table::setnames(UpdateData, c("V1"), c(eval(DateColumnName)))
      }
    } else {
      if(!is.null(GroupVariables)) {

        # Correctly indicate the target variables being generated----
        if(Difference) IDcols = "ModTarget" else IDcols <- eval(TargetColumnName)

        # GroupVar or Hierarchical----
        if(!is.null(HierarchGroups)) {
          temp <- data.table::copy(UpdateData[, ID := 1:.N, by = c(eval(GroupVariables))])
        } else {
          temp <- data.table::copy(UpdateData[, ID := 1:.N, by = "GroupVar"])
        }

        # Score model----
        temp <- temp[ID == max(ID)][, ID := NULL]
        Preds <- AutoXGBoostScoring(
          TargetType = "regression",
          ScoringData = temp,
          FeatureColumnNames = ModelFeatures,
          OneHot = FALSE,
          IDcols = IDcols,
          ModelObject = Model,
          ModelPath = getwd(),
          ModelID = "ModelTest",
          ReturnFeatures = FALSE,
          TransformNumeric = FALSE,
          BackTransNumeric = FALSE,
          TargetColumnName = eval(TargetColumnName),
          TransformationObject = NULL,
          FactorLevelsList = FactorList,
          TransID = NULL,
          TransPath = NULL,
          MDP_Impute = TRUE,
          MDP_CharToFactor = TRUE,
          MDP_RemoveDates = TRUE,
          MDP_MissFactor = "0",
          MDP_MissNum = -1)

        # Update data group case----
        data.table::setnames(Preds, "Predictions", "Preds")
        Preds <- cbind(UpdateData[ID == N], Preds)
        if(Difference) {
          Preds[, ModTarget := Preds][, eval(TargetColumnName) := Preds]
          if(RoundPreds) Preds[, ModTarget := round(ModTarget)]
        } else {
          if(NonNegativePred) Preds[, Preds := data.table::fifelse(Preds < 0.5, 0, Preds)]
          Preds[, eval(TargetColumnName) := Preds]
        }
        Preds[, Predictions := Preds][, Preds := NULL]
        if(RoundPreds) Preds[, Predictions := round(Predictions)]
        UpdateData <- UpdateData[ID != N]
        if(any(class(UpdateData[[eval(DateColumnName)]]) %chin% c("POSIXct","POSIXt","IDate"))) UpdateData[, eval(DateColumnName) := as.Date(get(DateColumnName))]
        if(any(class(Preds[[eval(DateColumnName)]]) %chin% c("POSIXct","POSIXt","IDate"))) Preds[, eval(DateColumnName) := as.Date(get(DateColumnName))]
        UpdateData <- data.table::rbindlist(list(UpdateData, Preds))
        if(Difference) {
          if(!is.null(HierarchGroups)) {
            UpdateData[ID %in% c(N-1,N), eval(TargetColumnName) := cumsum(get(TargetColumnName)), by = c(eval(GroupVariables))]
          } else {
            UpdateData[ID %in% c(N-1,N), eval(TargetColumnName) := cumsum(get(TargetColumnName)), by = "GroupVar"]
          }
        }
        UpdateData[, ID := NULL]
      } else {
        Preds <- AutoXGBoostScoring(
          TargetType = "regression",
          ScoringData = UpdateData[.N, ],
          FeatureColumnNames = ModelFeatures,
          OneHot = FALSE,
          IDcols = NULL,
          ModelObject = Model,
          ModelPath = getwd(),
          ModelID = "ModelTest",
          ReturnFeatures = FALSE,
          TransformNumeric = FALSE,
          BackTransNumeric = FALSE,
          TargetColumnName = eval(TargetColumnName),
          TransformationObject = NULL,
          FactorLevelsList = NULL,
          TransID = NULL,
          TransPath = NULL,
          MDP_Impute = TRUE,
          MDP_CharToFactor = TRUE,
          MDP_RemoveDates = TRUE,
          MDP_MissFactor = "0",
          MDP_MissNum = -1)

        # Update data non-group case----
        if(NonNegativePred) Preds[, Predictions := data.table::fifelse(Predictions < 0.5, 0, Predictions)]
        if(RoundPreds) Preds[, Predictions := round(Predictions)]
        data.table::set(UpdateData, i = N, j = 2L:3L, value = Preds[[1]])
      }
    }

    ###############
    # Forecasting
    ###############

    # Update features for next run----
    if(i != ForecastRuns + 1L) {

      # Timer----
      if(DebugMode) print("Timer----")
      if(Timer) if(i != 1) print(paste("Forecast future step: ", i-1))
      if(Timer) starttime <- Sys.time()

      # Create single future record----
      if(DebugMode) print("Create single future record----")
      CalendarFeatures <- NextTimePeriod(UpdateData.=UpdateData, TimeUnit.=TimeUnit, DateColumnName.=DateColumnName)

      # Update feature engineering ----
      if(DebugMode) print("Update feature engineering ----")
      UpdateData <- UpdateFeatures(UpdateData.=UpdateData, GroupVariables.=GroupVariables, CalendarFeatures.=CalendarFeatures, CalendarVariables.=CalendarVariables, GroupVarVector.=GroupVarVector, DateColumnName.=DateColumnName, XREGS.=XREGS, FourierTerms.=FourierTerms, FourierFC.=FourierFC, TimeGroups.=TimeGroups, TimeTrendVariable.=TimeTrendVariable, N.=N, TargetColumnName.=TargetColumnName, HolidayVariable.=HolidayVariable, HolidayLookback.=HolidayLookback, TimeUnit.=TimeUnit, AnomalyDetection.=AnomalyDetection, i.=i)

      # Update Lags and MA's----
      if(DebugMode) print("Update Lags and MA's----")
      UpdateData <- CarmaRollingStatsUpdate(
        ModelType = "xgboost", DebugMode.=DebugMode, UpdateData.=UpdateData, GroupVariables.=GroupVariables, Difference.=Difference, CalendarVariables.=CalendarVariables, HolidayVariable.=HolidayVariable, IndepVarPassTRUE.=IndepentVariablesPass, data.=data, CalendarFeatures.=CalendarFeatures, XREGS.=XREGS, HierarchGroups.=HierarchGroups, GroupVarVector.=GroupVarVector, TargetColumnName.=TargetColumnName, DateColumnName.=DateColumnName, Preds.=NULL,
        HierarchSupplyValue.=HierarchSupplyValue, IndependentSupplyValue.=IndependentSupplyValue, TimeUnit.=TimeUnit, TimeGroups.=TimeGroups, Lags.=Lags, MA_Periods.=MA_Periods, SD_Periods.=SD_Periods, Skew_Periods.=Skew_Periods, Kurt_Periods.=Kurt_Periods, Quantile_Periods.=Quantile_Periods, Quantiles_Selected.=Quantiles_Selected, HolidayLags.=HolidayLags, HolidayMovingAverages.=HolidayMovingAverages)
    }
    gc()
  }

  # Return data prep ----
  if(DebugMode) print("Return data prep ----")
  Output <- CarmaReturnDataPrep(UpdateData.=UpdateData, FutureDateData.=FutureDateData, dataStart.=dataStart, DateColumnName.=DateColumnName, TargetColumnName.=TargetColumnName, GroupVariables.=GroupVariables, Difference.=Difference, TargetTransformation.=TargetTransformation, TransformObject.=TransformObject, NonNegativePred.=NonNegativePred)
  UpdateData <- Output$UpdateData; Output$UpdateData <- NULL
  TransformObject <- Output$TransformObject; rm(Output)

  # Return
  return(list(
    Forecast = UpdateData,
    ModelInformation = TestModel,
    TransformationDetail = if(exists("TransformObject") && !is.null(TransformObject)) TransformObject else NULL))
}
