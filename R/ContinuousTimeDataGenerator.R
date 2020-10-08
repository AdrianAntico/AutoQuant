#' ContinuousTimeDataGenerator for creating continuous time data sets for on demand modeling
#'
#' ContinuousTimeDataGenerator for creating continuous time data sets for on demand modeling of transactional panel data.
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @param data This is your transactional level data
#' @param Case Currently set as 1 for forecasting and 2 for other
#' @param RestrictDateRange Set to TRUE to only pull samples by entity within the entity life (not beyond)
#' @param FC_Periods The number of future periods to collect data on
#' @param SaveData Set to TRUE to save the MetaData and final modeling data sets to file
#' @param FilePath Set to your file of choice for where you want the data sets saved
#' @param TargetVariableName The name of your target variable that represents demand
#' @param DateVariableName  The date variable of the demand instances
#' @param GDL_Targets The variable names to run through AutoLagRollStats()
#' @param HierarchyGroupVars Group vars
#' @param GroupingVariables These variables (or sinlge variable) is the combination of categorical variables that uniquely defines the level of granularity of each individual level to forecast. E.g. "sku" or c("Store","Department"). Sku is typically unique for all sku's. Store and Department in combination defines all unique departments as the department may be repeated across the stores.
#' @param MinTimeWindow The number of time periods you would like to omit for training. Default is 1 so that at a minimum, there is at least one period of values to forecast. You can set it up to a larger value if you do not want more possible target windows for the lower target window values.
#' @param MinTxnRecords I typically set this to 2 so that there is at least one other instance of demand so that the forecasted values are not complete nonsense.
#' @param TimeUnit List the time unit your data is aggregated by. E.g. "day", "week", "month", "quarter", "year"
#' @param TimeGroups = c("raw","day","week"),
#' @param Lags Select the periods for all lag variables you want to create. E.g. c(1:5,52)
#' @param MA_Periods Select the periods for all moving average variables you want to create. E.g. c(1:5,52)
#' @param SD_Periods Select the periods for all sd variables you want to create. E.g. c(1:5,52)
#' @param Skew_Periods Select the periods for all skew variables you want to create. E.g. c(1:5,52)
#' @param Kurt_Periods Select the periods for all kurtosis variables you want to create. E.g. c(1:5,52)
#' @param Quantile_Periods Select the periods for all quantiles variables you want to create. E.g. c(1:5,52)
#' @param Quantiles_Selected Select the quantiles you want. q5, q10, ..., q95
#' @param HolidayLags Select the lags you want generated
#' @param HolidayMovingAverages Select the moving averages you want generated
#' @param TimeBetween Supply a name or NULL
#' @param CalendarVariables Set to TRUE to have calendar variables created. The calendar variables are numeric representations of second, minute, hour, week day, month day, year day, week, isoweek, quarter, and year
#' @param HolidayGroups Input the holiday groups of your choice from the CreateHolidayVariable() function in this package
#' @param TimeTrendVariable Set to TRUE to have a time trend variable added to the model. Time trend is numeric variable indicating the numeric value of each record in the time series (by group). Time trend starts at 1 for the earliest point in time and increments by one for each success time point.
#' @param PowerRate Sampling parameter
#' @param SampleRate Set this to a value greater than 0. The calculation used is the number of records per group level raised to the power of PowerRate. Then that values is multiplied by SampleRate.
#' @param TargetWindowSamples = 5
#' @param PrintSteps Set to TRUE to have operation steps printed to the console
#' @examples
#' \dontrun{
#' DataSets <- ContinuousTimeDataGenerator(
#'   data,
#'   RestrictDateRange = TRUE,
#'   FC_Periods = 52,
#'   SaveData = FALSE,
#'   FilePath = normalizePath("./"),
#'   TargetVariableName = "qty",
#'   DateVariableName = "date",
#'   GDL_Targets = NULL,
#'   GroupingVariables = "sku",
#'   HierarchyGroupVars = NULL,
#'   TimeGroups = c("raw","day","week"),
#'   MinTimeWindow = 1,
#'   MinTxnRecords = 2,
#'   Lags = 1:7,
#'   MA_Periods = 10L,
#'   SD_Periods = 10L,
#'   Skew_Periods = 10L,
#'   Kurt_Periods = 10L,
#'   Quantile_Periods = 10L,
#'   Quantiles_Selected = c("q5"),
#'   HolidayLags = c(1L:7L),
#'   HolidayMovingAverages = c(2L:14L),
#'   TimeBetween = NULL,
#'   TimeTrendVariable = TRUE,
#'   TimeUnit = "day",
#'   CalendarVariables = c("wday",
#'     "mday",
#'     "yday",
#'     "week",
#'     "isoweek",
#'     "month",
#'     "quarter",
#'     "year"),
#'   HolidayGroups = "USPublicHolidays",
#'   PowerRate = 0.5,
#'   SampleRate = 5,
#'   TargetWindowSamples = 5,
#'   PrintSteps = TRUE)
#' CountModelData <- DataSets$CountModelData
#' SizeModelData <- DataSets$SizeModelData
#' rm(DataSets)
#' }
#' @return Returns two data.table data sets: The first is a modeling data set for the count distribution while the second data set if for the size model data set.
#' @export
ContinuousTimeDataGenerator <- function(data,
                                        RestrictDateRange = TRUE,
                                        Case = 2L,
                                        FC_Periods = 52L,
                                        SaveData = FALSE,
                                        FilePath = NULL,
                                        TargetVariableName = "qty",
                                        DateVariableName = "date",
                                        GDL_Targets = NULL,
                                        TimeUnit = "raw",
                                        TimeGroups = c("raw","day","week"),
                                        GroupingVariables = "sku",
                                        HierarchyGroupVars = NULL,
                                        MinTimeWindow = 1L,
                                        MinTxnRecords = 2L,
                                        Lags = 1L:7L,
                                        MA_Periods = 10L,
                                        SD_Periods = 10L,
                                        Skew_Periods = 10L,
                                        Kurt_Periods = 10L,
                                        Quantile_Periods = 10L,
                                        Quantiles_Selected = c("q5"),
                                        HolidayLags = c(1L:7L),
                                        HolidayMovingAverages = c(2L:14L),
                                        TimeBetween = NULL,
                                        TimeTrendVariable = TRUE,
                                        CalendarVariables = c("wday",
                                                              "mday",
                                                              "yday",
                                                              "week",
                                                              "isoweek",
                                                              "month",
                                                              "quarter",
                                                              "year"),
                                        HolidayGroups = "USPublicHolidays",
                                        PowerRate = 0.5,
                                        SampleRate = 5,
                                        TargetWindowSamples = 5,
                                        PrintSteps = TRUE) {

  # Initialize timer list----
  ProfilerList <- list()

  # Ensure is data.table----
  if(PrintSteps) print("Running initial data prep")
  DataPrepStart <- Sys.time()
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  # Ensure Date Column is a Date----
  DateTypeConversionStart <- Sys.time()
  if(is.character(data[[eval(DateVariableName)]])) {
    if(tolower(TimeUnit) == "raw") {
      data.table::set(data, j = eval(DateVariableName), value = as.POSIXct(data[[eval(DateVariableName)]]))
    } else {
      data.table::set(data, j = eval(DateVariableName), value = as.Date(data[[eval(DateVariableName)]]))
    }
  }
  DateTypeConversionEnd <- Sys.time()
  ProfilerList[["DateConversion"]] <- DateTypeConversionEnd - DateTypeConversionStart
  print("DateConversion run time")
  print(ProfilerList[["DateConversion"]])

  # Round down dates (add option to not do this----
  RoundDatesStart <- Sys.time()
  if(TimeUnit != "raw") data.table::set(data, j = eval(DateVariableName), value = lubridate::floor_date(data[[eval(DateVariableName)]], unit = TimeUnit))
  RoundDatesEnd <- Sys.time()
  ProfilerList[["RoundDownDates"]] <- RoundDatesEnd - RoundDatesStart
  print("RoundDownDates run time")
  print(ProfilerList[["RoundDownDates"]])

  # Group Concatenation----
  GroupConcatenationStart <- Sys.time()
  if(!is.null(GroupingVariables)) {
    if(length(GroupingVariables) > 1) {
      data[, GroupVar := do.call(paste, c(.SD, sep = " ")), .SDcols = GroupingVariables]
      data[, eval(GroupingVariables) := NULL]
    } else {
      data.table::setnames(data, eval(GroupingVariables), "GroupVar")
    }

    # Modify GroupingVariables argument
    ReverseGroupingVariables <- GroupingVariables
    GroupingVariables <- "GroupVar"
  }
  GroupConcatenationEnd <- Sys.time()
  ProfilerList[["GroupConcatenation"]] <- GroupConcatenationEnd - GroupConcatenationStart
  print("GroupConcatenation run time")
  print(ProfilerList[["GroupConcatenation"]])

  # Ensure data is aggregated to proper time unit----
  if(Case == 1L) {
    if(TimeUnit != "raw") {
      AggregateTimeUnitStart <- Sys.time()
      data <- data[, lapply(.SD, sum), .SDcols = c(eval(TargetVariableName)), by = c(eval(GroupingVariables), eval(DateVariableName))]
      AggregateTimeUnitEnd <- Sys.time()
      ProfilerList[["AggregateByTimeUnit"]] <- AggregateTimeUnitEnd - AggregateTimeUnitStart
    }
  }

  # Update timing list----
  DataPrepEnd <- Sys.time()
  ProfilerList[["InitialDataPrepAll"]] <- DataPrepEnd - DataPrepStart
  print("InitialDataPrepAll run time")
  print(ProfilerList[["InitialDataPrepAll"]])

  # Generate Metadata----
  if(PrintSteps) print("Running ID_MetadataGenerator()")
  MetaDataStart <- Sys.time()
  MetaData <- ID_MetadataGenerator(
    data = data,
    DateVariableName = DateVariableName,
    GroupingVariables = "GroupVar",
    MinTimeWindow = MinTimeWindow,
    MinTxnRecords = MinTxnRecords,
    DateInterval = TimeUnit)
  MetaDataEnd <- Sys.time()
  ProfilerList[["MetaDataGeneratorAll"]] <- MetaDataEnd - MetaDataStart
  print("MetaDataGeneratorAll run time")
  print(ProfilerList[["MetaDataGeneratorAll"]])

  # Save Data----
  if(SaveData) data.table::fwrite(MetaData, file = file.path(normalizePath(FilePath), "MetaData.csv"))

  # Add Calendar Variables----
  if(PrintSteps) print("Running CreateCalendarVariables()")
  CreateCalendarVariablesStart <- Sys.time()
  if(!is.null(CalendarVariables)) {
    data <- CreateCalendarVariables(
      data,
      DateCols = DateVariableName,
      AsFactor = FALSE,
      TimeUnits = CalendarVariables)
  }
  CreateCalendarVariablesEnd <- Sys.time()
  ProfilerList[["CreateCalendarVariables()"]] <- CreateCalendarVariablesEnd - CreateCalendarVariablesStart
  print("CreateCalendarVariables() run time")
  print(ProfilerList[["CreateCalendarVariables()"]])

  # Add Holiday Variables----
  if(PrintSteps) print("Running CreateHolidayVariables()")
  CreateHolidayVariablesStart <- Sys.time()
  if(!is.null(HolidayGroups)) {
    data <- CreateHolidayVariables(
      data,
      DateCols = DateVariableName,
      HolidayGroups = HolidayGroups,
      Holidays = NULL,
      GroupingVars = "GroupVar")
  }
  CreateHolidayVariablesEnd <- Sys.time()
  ProfilerList[["CreateHolidayVariables()"]] <- CreateHolidayVariablesEnd - CreateHolidayVariablesStart
  print("CreateHolidayVariables() run time")
  print(ProfilerList[["CreateHolidayVariables()"]])

  # Holiday Lags and Moving Average----
  if(PrintSteps) print("Running AutoLagRollStats() for Holiday Counts")
  CreateHolidayLagsStart <- Sys.time()
  if(!is.null(HolidayGroups)) {
    data <- AutoLagRollStats(

      # Data Args
      data                 = data,
      DateColumn           = eval(DateVariableName),
      Targets              = "HolidayCounts",
      HierarchyGroups      = NULL,
      IndependentGroups    = "GroupVar",

      # Services
      TimeBetween          = TimeBetween,
      TimeUnit             = if(tolower(TimeUnit) == "raw") "day" else TimeUnit,
      TimeUnitAgg          = TimeUnit,
      TimeGroups           = TimeGroups[1L],
      RollOnLag1           = FALSE,
      Type                 = "Lag",
      SimpleImpute         = TRUE,

      # Calculated Columns
      Lags                  = HolidayLags,
      MA_RollWindows        = HolidayMovingAverages,
      SD_RollWindows        = NULL,
      Skew_RollWindows      = NULL,
      Kurt_RollWindows      = NULL,
      Quantile_RollWindows  = NULL,
      Quantiles_Selected    = NULL)
  }
  CreateHolidayLagsEnd <- Sys.time()
  ProfilerList[["HolidayLags"]] <- CreateHolidayLagsEnd - CreateHolidayLagsStart
  print("HolidayLags run time")
  print(ProfilerList[["HolidayLags"]])

  # Add in the time varying features----
  if(PrintSteps) print("Running AutoLagRollStats()")
  AutoLagRollStatsStart <- Sys.time()
  data <- AutoLagRollStats(

    # Data Args
    data                 = data,
    DateColumn           = eval(DateVariableName),
    Targets              = GDL_Targets,
    HierarchyGroups      = NULL,
    IndependentGroups    = "GroupVar",

    # Services
    TimeBetween          = TimeBetween,
    TimeUnit             = if(tolower(TimeUnit) == "raw") "day" else TimeUnit,
    TimeUnitAgg          = TimeUnit,
    TimeGroups           = TimeGroups,
    RollOnLag1           = FALSE,
    Type                 = "Lag",
    SimpleImpute         = TRUE,

    # Calculated Columns
    Lags                  = c(Lags),
    MA_RollWindows        = c(MA_Periods),
    SD_RollWindows        = c(SD_Periods),
    Skew_RollWindows      = c(Skew_Periods),
    Kurt_RollWindows      = c(Kurt_Periods),
    Quantile_RollWindows  = c(Quantile_Periods),
    Quantiles_Selected    = c(Quantiles_Selected))
  AutoLagRollStatsEnd <- Sys.time()
  ProfilerList[["AutoLagRollStats()"]] <- AutoLagRollStatsEnd - AutoLagRollStatsStart
  print("AutoLagRollStats() run time")
  print(ProfilerList[["AutoLagRollStats()"]])

  # Add Time Trend Variable----
  if(PrintSteps) print("Running Time Trend Calculation")
  TimeTrendStart <- Sys.time()
  if(!is.null(GroupingVariables)) {
    data.table::setorderv(data, cols = c(eval(GroupingVariables), eval(DateVariableName)), order = c(1L, 1L))
    data[, TimeTrend := 1L:.N, by = list(GroupVar)]
  } else {
    data.table::setorderv(data, cols = c(eval(DateVariableName)), order = c(1L))
    data[, TimeTrend := 1L:.N]
  }
  TimeTrendEnd <- Sys.time()
  ProfilerList[["TimeTrend"]] <- TimeTrendEnd - TimeTrendStart
  print("TimeTrend run time")
  print(ProfilerList[["TimeTrend"]])

  # Run Final Build----
  if(PrintSteps) print("Running ID_BuildTrainDataSets()")
  BuildDataSetsStart <- Sys.time()
  packages <- c("RemixAutoML","data.table","forecast","lubridate")
  cores <- parallel::detectCores()

  # Create File Splitter Column Indicator----
  MetaData <- MetaData[, ID := runif(MetaData[,.N])][order(ID)][, ID := NULL]
  MetaData[, SelectRows := sample(c(seq_len(cores)), size = MetaData[,.N], replace = TRUE, prob = c(rep(1/cores, cores)))]
  data.table::set(MetaData, j = "GroupVar", value = as.character(MetaData[["GroupVar"]]))
  data.table::set(data, j = "GroupVar", value = as.character(data[["GroupVar"]]))
  data <- merge(x = data, y = MetaData[,.SD, .SDcols = c("GroupVar","SelectRows")], by = "GroupVar", all = FALSE)

  # Parallelize Build----
  if(PrintSteps) print("Running Parallel Build")
  cl <- parallel::makePSOCKcluster(max(1L, min(as.numeric(cores), length(unique(MetaData[["SelectRows"]])))))
  doParallel::registerDoParallel(cl)
  if(Case == 1L) {
    Results <- foreach::foreach(
      i = unique(MetaData[["SelectRows"]]),
      .combine = function(x, ...) mapply(function(...) data.table::rbindlist(list(...), use.names = TRUE, fill = TRUE), x, ..., SIMPLIFY = FALSE),
      .multicombine = TRUE,
      .packages = packages) %dopar% {

        # Loops----
        ModelDataSets <- tryCatch({ID_BuildTrainDataSets(
          MetaData = MetaData[SelectRows == eval(i)],
          data = data[SelectRows == eval(i)],
          Case = 2L,
          TargetVariableName = TargetVariableName,
          DateVariableName = DateVariableName,
          GroupingVariables = GroupingVariables,
          FC_Periods = FC_Periods,
          TimeUnit = TimeUnit,
          PowerRate = PowerRate,
          SampleRate = SampleRate,
          TargetWindowSamples = TargetWindowSamples)}, error = function(x) NULL)

        # Store individual file outputs----
        if(!is.null(ModelDataSets)) {
          if(Case == 1L) {
            CountModelData <- ModelDataSets$CountModelData
            SizeModelData <- ModelDataSets$SizeModelData
            list(CountModelData = CountModelData, SizeModelData = SizeModelData)
          } else if(Case == 2L) {
            CountModelData <- ModelDataSets$CountModelData
            list(CountModelData = CountModelData, SizeModelData = SizeModelData)
          }
        }
      }
  } else if(Case == 2L) {
    Results <- foreach::foreach(
      i = unique(MetaData[["SelectRows"]]),
      .combine = function(...) data.table::rbindlist(list(...), use.names = TRUE, fill = TRUE),
      .multicombine = TRUE,
      .packages = packages) %dopar% {

        # Loops----
        ModelDataSets <- tryCatch({ID_BuildTrainDataSets(
          MetaData = MetaData[SelectRows == eval(i)],
          data = data[SelectRows == eval(i)],
          Case = 2L,
          TargetVariableName = TargetVariableName,
          DateVariableName = DateVariableName,
          GroupingVariables = GroupingVariables,
          FC_Periods = FC_Periods,
          TimeUnit = TimeUnit,
          PowerRate = PowerRate,
          SampleRate = SampleRate,
          TargetWindowSamples = TargetWindowSamples)}, error = function(x) NULL)

        # Store individual file outputs----
        if(!is.null(ModelDataSets)) ModelDataSets$CountModelData
      }
  }

  # Store results----
  if(Case == 1L) {
    CountModelData <- Results$CountModelData
    SizeModelData <- Results$SizeModelData
    SizeModelData <- SizeModelData[Size != 0]
  } else if(Case == 2L) {
    CountModelData <- Results
  }

  # Shut down parallel objects----
  parallel::stopCluster(cl)
  rm(cl)
  BuildDataSetsEnd <- Sys.time()
  ProfilerList[["BuildDataSets"]] <- BuildDataSetsEnd - BuildDataSetsStart
  print("BuildDataSets run time")
  print(ProfilerList[["BuildDataSets"]])

  # Back-transform GroupingVariables----
  if(PrintSteps) print("Final Data Wrangling")
  BackTransformStart <- Sys.time()
  if(length(ReverseGroupingVariables) > 1L) {
    CountModelData[, eval(ReverseGroupingVariables) := data.table::tstrsplit(GroupVar, " ")][, GroupVar := NULL]
    data.table::setcolorder(CountModelData, c((ncol(CountModelData)-length(ReverseGroupingVariables)+1L):ncol(CountModelData),1L:(ncol(CountModelData)-length(ReverseGroupingVariables))))
    if(exists("SizeModelData")) SizeModelData[, eval(ReverseGroupingVariables) := data.table::tstrsplit(GroupVar, " ")][, GroupVar := NULL]
  } else {
    data.table::setnames(CountModelData, "GroupVar", eval(ReverseGroupingVariables))
    if(exists("SizeModelData")) data.table::setnames(SizeModelData, "GroupVar", eval(ReverseGroupingVariables))
  }
  BackTransformEnd <- Sys.time()
  ProfilerList[["BackTransform"]] <- BackTransformEnd - BackTransformStart
  print("Backtransform run time")
  print(ProfilerList[["BackTransform"]])

  # Save Data----
  if(SaveData) {

    # Save modeling data sets----
    if(Case == 1L) {
      data.table::fwrite(CountModelData, file = file.path(normalizePath(FilePath), "CountModelData.csv"))
    } else if(Case == 2L) {
      data.table::fwrite(CountModelData, file = file.path(normalizePath(FilePath), "ModelingData.csv"))
    }
    if(exists("SizeModelData")) data.table::fwrite(SizeModelData, file = file.path(normalizePath(FilePath), "SizeModelData.csv"))

    # Save column names for modeling data----
    CountPredNames <- names(CountModelData)
    if(exists("SizeModelData")) SizeModelData <- names(SizeModelData)
    save(CountPredNames, file = file.path(FilePath,"ModelDataColumnNames.csv"))
    if(exists("SizeModelData")) save(SizeModelData, file = file.path(normalizePath(FilePath), "SizePredNames.Rdata"))
  }

  # Remove select rows----
  if("SelectRows" %chin% names(CountModelData)) data.table::set(CountModelData, j = "SelectRows", value = NULL)

  # Reorder columns----
  if("TimeTrend" %chin% names(CountModelData)) {
    data.table::setcolorder(x = CountModelData, neworder = c((ncol(CountModelData)-2L):ncol(CountModelData), 1L:(ncol(CountModelData)-3L)))
  } else {
    data.table::setcolorder(x = CountModelData, neworder = c((ncol(CountModelData)-1L):ncol(CountModelData), 1L:(ncol(CountModelData)-2L)))
  }

  # Return data sets----
  if(Case == 1L) {
    return(list(CountData = CountModelData, SizeData = SizeModelData, ProfilerList = ProfilerList))
  } else if(Case == 2L) {
    return(list(Data = CountModelData, ProfilerList = ProfilerList))
  }
}

#' ID_MetadataGenerator for summary metadata for transactional data
#'
#' ID_MetadataGenerator for summary metadata for transactional data. The data returned from this function feeds into the IntermittentDemandBootStrapper() function.
#'
#' @family Feature Engineering Helper
#' @param data This is your transactional level data
#' @param RestrictDateRange = TRUE
#' @param DateVariableName Bla
#' @param GroupingVariables Bla
#' @param MinTimeWindow The number of time periods you would like to omit for training. Default is 1 so that at a minimum, there is at least one period of values to forecast. You can set it up to a larger value if you do not want more possible target windows for the lower target window values.
#' @param MinTxnRecords I typically set this to 2 so that there is at least one other instance of demand so that the forecasted values are not complete nonsense.
#' @param DateInterval This is the time unit for determining date calculations
#' @examples
#' \dontrun{
#' # Generate Metadata----
#' MetaData <- ID_MetadataGenerator(
#'   data = data,
#'   RestrictDateRange = TRUE,
#'   DateVariableName = DateVariableName,
#'   GroupingVariables = GroupingVariables,
#'   MinTimeWindow = MinTimeWindow,
#'   MinTxnRecords = MinTxnRecords,
#'   DateInterval = TimeUnit,
#'   TimeUnit = TimeUnit
#' )
#' }
#' @return Returns a data.table with summary information for the IntermittentDemandBootStrapper() function.
#' @export
ID_MetadataGenerator <- function(data,
                                 RestrictDateRange = TRUE,
                                 DateVariableName = NULL,
                                 GroupingVariables = NULL,
                                 MinTimeWindow = 1L,
                                 MinTxnRecords = 2L,
                                 DateInterval = "day") {

  # Define max date for sampling window----
  if(RestrictDateRange) {

    # Per entity ID
    data[, max_date := max(get(DateVariableName)), by = list(GroupVar)]

    # Set up base table----
    Step1 <- data[, .(.N, max(lubridate::floor_date(as.Date(max_date, unit = DateInterval)))), by = list(GroupVar)]
    data.table::setorderv(Step1, "N", -1L)

  } else {
    if(tolower(DateInterval) == "week") {
      max_date <- data[, max(get(DateVariableName))][[1L]] - 7L * MinTimeWindow
    } else if(tolower(DateInterval) == "day") {
      max_date <- data[, max(get(DateVariableName))][[1L]] - MinTimeWindow
    } else if(tolower(DateInterval) == "month") {
      max_date <- data[, max(get(DateVariableName))][[1L]] %m+% months(-MinTimeWindow)
    }

    # Set up base table----
    Step1 <- data[, .(.N, lubridate::floor_date(as.Date(max_date, unit = DateInterval))), by = list(GroupVar)]
    data.table::setorderv(Step1, "N", -1)
  }

  # Gather second to last distinct date by GroupingVariable----
  Step2 <- data[, .(.N, get(DateVariableName)), by = list(GroupVar)][order(GroupVar,-V2)][, sum(N), by = c(eval(GroupingVariables),"V2")][, txn := .N:1L, by = list(GroupVar)][txn == MinTxnRecords][, txn := NULL]
  data.table::setnames(Step2,c("V1","V2"),c("Txns","MinDate"))
  Step2 <- Step2[, .SD, .SDcols = c(eval(GroupingVariables),"MinDate")]

  # Merge, change names, filter out infrequent levels----
  BaseTable2 <- merge(Step2, Step1, by = eval(GroupingVariables), all = FALSE)
  data.table::setnames(BaseTable2, c("N","V2"), c("Txns", "MaxDate"))
  if(tolower(DateInterval) == "raw") {
    BaseTable2 <- BaseTable2[, Date_Range := as.numeric(difftime(MaxDate, MinDate, units = "day"))][order(-Txns)]
  } else {
    BaseTable2 <- BaseTable2[, Date_Range := as.numeric(difftime(MaxDate, MinDate, units = DateInterval))][order(-Txns)]
  }

  # Remove levels with less than MinTxnRecords distinct past dates----
  MetaData <- BaseTable2[Txns >= eval(MinTxnRecords)][Date_Range > 0L]

  # Return data----
  return(MetaData)
}

#' ID_TrainingDataGenerator for subsetting data
#'
#' ID_TrainingDataGenerator for subsetting data for the IntermittentDemandBootStrapper() function.
#'
#' @family Feature Engineering Helper
#' @param data Source data
#' @param Type "timetoevent1", "eventinwindow1"
#' @param TargetVariableName Name of the variables to run feature engineering on. List the actual target variable name first.
#' @param Level The individual level of your group variable
#' @param GroupingVariables Your grouping variables
#' @param DateVariableName Name of your date variable
#' @param RandomStartDate The date to partition the data
#' @param TimeUnit This is the TimeUnit you selected for aggregation
#' @param TargetWindow The length of the target window sampled
#' @return Returns two data sets for the IntermittentDemandBootStrapper() function based on a single level from the grouping variables.
#' @export
ID_TrainingDataGenerator <- function(data,
                                     Type = "timetoevent1",
                                     TargetVariableName = NULL,
                                     Level = NULL,
                                     DateVariableName = NULL,
                                     GroupingVariables = NULL,
                                     RandomStartDate = NULL,
                                     TimeUnit = NULL,
                                     TargetWindow = NULL) {

  # historical data <--> point in time <--> target window----
  histDemandRaw <- data[get(DateVariableName) < eval(RandomStartDate)]

  # Data within target window----
  counter <- 0L
  for(tar in TargetWindow) {

    # Target variable data
    if(lubridate::is.POSIXct(data[[eval(DateVariableName)]])) {
      targetDemand  <- data[
        get(DateVariableName) >= eval(RandomStartDate) &
          get(DateVariableName) - 86400L * eval(tar) <= eval(RandomStartDate)]
    } else {
      targetDemand  <- data[
        get(DateVariableName) >= eval(RandomStartDate) &
          get(DateVariableName) - eval(tar) <= eval(RandomStartDate)]
    }

    # Add in the time since last demand instance from RandomStartDate----
    histDemandRaw <- histDemandRaw[order(-get(DateVariableName))][, TimeSinceLastDemand := as.numeric(difftime(RandomStartDate,get(DateVariableName), units = TimeUnit))]

    # Remove meta data for feature creation set----
    features <- histDemandRaw[order(-get(DateVariableName))][, paste0(eval(DateVariableName)) := NULL][1L,]
    data.table::set(features, j = "FC_Window", value = tar)

    # Remove data and rename target variable----
    keep <- eval(TargetVariableName)
    targetDemand <- targetDemand[, ..keep]
    data.table::setnames(targetDemand, old = eval(TargetVariableName[1L]), new = "Size")

    # Merge Features and Targets----
    if(nrow(targetDemand) != 0L) {
      TargetCount <- cbind(targetDemand[, .(Counts = .N)], features)
      TargetSize  <- cbind(targetDemand, features)
    } else {
      TargetCount <- cbind(data.table(Counts = 0L), features)
      TargetSize  <- cbind(data.table::data.table(Temp = 0L), features)
      data.table::setnames(TargetSize, "Temp", "Size")
    }

    # Combine data sets----
    counter <- counter + 1L
    if(counter == 1L) {
      CountFinal <- TargetCount
      SizeFinal <- TargetSize
    } else {
      CountFinal <- data.table::rbindlist(list(CountFinal,TargetCount), fill = TRUE)
      SizeFinal <- data.table::rbindlist(list(SizeFinal,TargetSize), fill = TRUE)
    }
  }

  # Output data file----
  return(list(CountData = CountFinal, SizeData = SizeFinal))
}

#' ID_TrainingDataGenerator2 for subsetting data
#'
#' ID_TrainingDataGenerator2 for subsetting data for the IntermittentDemandBootStrapper() function.
#'
#' @family Feature Engineering Helper
#' @param data Source data
#' @param TargetVariableName vector of variable names
#' @param Level The individual level of your group variable
#' @param GroupingVariables Your grouping variables
#' @param DateVariableName Name of your date variable
#' @param RandomStartDate The date to partition the data
#' @param TimeUnit This is the TimeUnit you selected for aggregation
#' @param TargetWindow The length of the target window sampled
#' @return Returns two data sets for the IntermittentDemandBootStrapper() function based on a single level from the grouping variables.
#' @export
ID_TrainingDataGenerator2 <- function(data,
                                      TargetVariableName = NULL,
                                      Level = NULL,
                                      GroupingVariables = NULL,
                                      DateVariableName = NULL,
                                      RandomStartDate = NULL,
                                      TimeUnit = NULL,
                                      TargetWindow = NULL) {

  # historical data <--> point in time <--> target window----
  histDemandRaw <- data[get(DateVariableName) < eval(RandomStartDate)]

  # Data within target window----
  counter <- 0L
  for(tar in TargetWindow) {

    # Classification target variable data----
    if(lubridate::is.POSIXct(data[[eval(DateVariableName)]])) {
      binarytarget <- min(data[get(DateVariableName) > eval(RandomStartDate) & get(DateVariableName) - 86400L * eval(tar) <= eval(RandomStartDate), get(TargetVariableName[1L])], na.rm = TRUE)
    } else {
      binarytarget <- min(data[get(DateVariableName) > eval(RandomStartDate) & get(DateVariableName) - eval(tar) <= eval(RandomStartDate), get(TargetVariableName[1L])], na.rm = TRUE)
    }

    # Build records----
    if(!is.finite(binarytarget)) binarytarget <- 0L

    # Time to event target variable data----
    if(lubridate::is.POSIXct(data[[eval(DateVariableName)]])) {
      temp <- data[get(DateVariableName) > eval(RandomStartDate), get(TargetVariableName[1L])]
      temp <- temp[!is.na(temp)]
      if(is.na(temp[1L])) timetoevent <- 0L else timetoevent <- temp[length(temp)]
    } else {
      temp <- data[get(DateVariableName) - eval(tar) > eval(RandomStartDate), get(TargetVariableName[1L])]
      temp <- temp[!is.na(temp)]
      if(is.na(temp[1L])) timetoevent <- 0L else timetoevent <- temp[length(temp)]
    }

    # Build records----
    if(is.numeric(timetoevent) | is.integer(timetoevent)) {

      # Time to event target variable data----
      if(lubridate::is.POSIXct(data[[eval(DateVariableName)]])) {
        outcome <- as.character(data[get(DateVariableName) - 86400 * eval(tar) > eval(RandomStartDate), get(TargetVariableName[2L])][1L])
      } else {
        outcome <- as.character(data[get(DateVariableName) - eval(tar) > eval(RandomStartDate), get(TargetVariableName[2L])][1L])
      }

      # Add in the time since last demand instance from RandomStartDate----
      data.table::setorderv(x = histDemandRaw, cols = eval(DateVariableName), order = -1L)
      features <- histDemandRaw[1L]
      data.table::set(features, j = "TimeSinceLastDemand", value = as.numeric(difftime(RandomStartDate, features[[eval(DateVariableName)]], units = TimeUnit)))

      # Remove meta data for feature creation set----
      data.table::set(features, j = unique(TargetVariableName), value = NULL)
      data.table::set(features, j = "FC_Window", value = tar)

      # Merge Features and Targets----
      temp <- cbind(binarytarget, timetoevent, outcome, features)
      data.table::setnames(temp, names(temp)[1L], "BinaryOutcome")
      data.table::setnames(temp, names(temp)[2L], "TimeToEvent")
      data.table::setnames(temp, names(temp)[3L], "Outcome")
      data.table::set(temp, j = "BinaryOutcome", value = data.table::fifelse(temp[["BinaryOutcome"]] == 0L, 1L, 0L))

      # Combine data sets----
      counter <- counter + 1L
      if(counter == 1L) Final <- temp else Final <- data.table::rbindlist(list(Final,temp), use.names = TRUE, fill = TRUE)
    }
  }

  # Output data file----
  return(data = Final)
}

#' ID_BuildTrainDataSets for assembling data
#'
#' ID_BuildTrainDataSets for assembling data for the IntermittentDemandBootStrapper() function.
#'
#' @family Feature Engineering Helper
#' @param MetaData This is the metadata returned from the ID_MetadataGenerator() function
#' @param data This is your transactional data
#' @param Case Indicate which data constructor method to use
#' @param TargetVariableName Your target variable names
#' @param DateVariableName Your date variable names
#' @param GroupingVariables Your grouping variables
#' @param FC_Periods The number of periods to forecast
#' @param TimeUnit The time period unit, such as "day", "week", or "month"
#' @param PowerRate The calculated for determining the total samples is number of records to the power of PowerRate. Then that values is multiplied by the SampleRate. This ensures that a more representative sample is generated across the data set.
#' @param SampleRate The value used to sample from each level of the grouping variables
#' @param TargetWindowSamples The number of different targets to utilize for a single random start date
#' @return Returns the count modeling data and the size modeling data
#' @export
ID_BuildTrainDataSets <- function(MetaData,
                                  data,
                                  Case = 2L,
                                  TargetVariableName = NULL,
                                  DateVariableName = NULL,
                                  GroupingVariables = NULL,
                                  FC_Periods,
                                  TimeUnit = "week",
                                  PowerRate = 0.5,
                                  SampleRate = 5L,
                                  TargetWindowSamples = 5L) {

  # Define DateUnit----
  if(TimeUnit == "week") {
    DateUnit <- 7L
  } else if (TimeUnit == "day") {
    DateUnit <- 1L
  } else if (TimeUnit == "month") {
    DateUnit <- 30L
  } else {
    DateUnit <- 1L
    TimeUnit <- "day"
  }

  # Set up collection objects----
  SMD <- list()
  CMD <- list()
  j <- 0L

  # Store levels in vector----
  LevelVector <- as.character(MetaData[, get(GroupingVariables)])

  # Store number of levels to go through----
  LevelCount <- length(LevelVector)

  # Create modeling data----
  for(level in LevelVector) {

    # Set iterations----
    issuances  <- as.numeric(ceiling(MetaData[get(GroupingVariables) == eval(level), "Txns"][[1L]]))
    iterations <- ceiling((issuances^PowerRate)*SampleRate)

    # Check to ensure issuances and iterations exist----
    if(length(issuances) == 0L | length(iterations) == 0L) next
    j <- j + 1L

    # Track progress----
    print(j / LevelCount)

    # Initialize / reset storage lists----
    countData <- list()
    sizeData  <- list()

    # Subset data before looping through a single GroupingVariable----
    level_data <- data[get(GroupingVariables) == eval(level)]

    # Set date range----
    DateRange <- MetaData[GroupVar == eval(level), "Date_Range"][[1]]

    # Data generator
    for(i in seq_len(iterations)) {

      # Set Random Starting Date----
      if(lubridate::is.POSIXct(MetaData$MinDate[1])) {
        RandomStartDate <- MetaData[GroupVar == eval(level), "MinDate"][[1]] +
          DateUnit * ceiling(sample(86400L:(86400L * DateUnit * DateRange), 1L))
      } else {
        RandomStartDate <- MetaData[GroupVar == eval(level), "MinDate"][[1L]] +
          DateUnit * ceiling(sample(1L:(DateUnit*DateRange), 1L))
      }

      # Set Target Window Max Sample Window----
      TargetWindowMax <- min(ceiling(as.numeric(difftime(
              lubridate::floor_date(MetaData[get(GroupingVariables) == eval(level), "MaxDate"][[1L]], unit = TimeUnit),
              RandomStartDate,
              units = TimeUnit))),
              FC_Periods)

      # Set Target Window----
      TargetWindow <- sample(x = seq_len(TargetWindowMax), size = TargetWindowSamples, replace = TRUE)

      # Create samples----
      if(Case == 1L) {
        SampleData <- ID_TrainingDataGenerator(
          data = level_data,
          TargetVariableName = TargetVariableName,
          DateVariableName = DateVariableName,
          RandomStartDate = RandomStartDate,
          TimeUnit = TimeUnit,
          TargetWindow = TargetWindow)
      } else if(Case == 2L) {
        SampleData <- ID_TrainingDataGenerator2(
          data = level_data,
          TargetVariableName = TargetVariableName,
          DateVariableName = DateVariableName,
          RandomStartDate = RandomStartDate,
          TimeUnit = TimeUnit,
          TargetWindow = TargetWindow)
      }

      # Build data sets----
      if(Case == 1L) {
        if(i == 1L) {
          countData <- SampleData$CountData
          sizeData <- SampleData$SizeData
        } else {
          countData <- data.table::rbindlist(list(countData, SampleData$CountData), fill = TRUE)
          sizeData <- data.table::rbindlist(list(sizeData, SampleData$SizeData), fill = TRUE)
        }
      } else if(Case == 2L) {
        if(i == 1L) {
          countData <- SampleData
        } else {
          countData <- data.table::rbindlist(list(countData, SampleData), fill = TRUE)
        }
      }
    }

    # Collect samples----
    if(Case == 1L) {
      if(j == 1L) {
        CMD <- countData
        SMD <- sizeData
      } else {
        CMD <- data.table::rbindlist(list(CMD,countData), fill = TRUE)
        SMD <- data.table::rbindlist(list(SMD,sizeData), fill = TRUE)
      }
    } else if(Case == 2L) {
      if(j == 1L) {
        CMD <- countData
      } else {
        CMD <- data.table::rbindlist(list(CMD,countData), fill = TRUE)
      }
    }
  }

  # Return data----
  if(Case == 1L) {
    return(list(CountModelData = CMD, SizeModelData = SMD))
  } else if(Case == 2L) {
    return(list(CountModelData = CMD, SizeModelData = NULL))
  }
}
