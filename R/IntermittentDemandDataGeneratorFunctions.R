#' IntermittentDemandDataGenerator for frequency and size data sets
#'
#' IntermittentDemandDataGenerator for frequency and size data sets. This function generates count and size data sets for various future window sizes.
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
#' @param GroupingVariables These variables (or sinlge variable) is the combination of categorical variables that uniquely defines the level of granularity of each individual level to forecast. E.g. "sku" or c("Store","Department"). Sku is typically unique for all sku's. Store and Department in combination defines all unique departments as the department may be repeated across the stores.
#' @param MinTimeWindow The number of time periods you would like to omit for training. Default is 1 so that at a minimum, there is at least one period of values to forecast. You can set it up to a larger value if you do not want more possible target windows for the lower target window values.
#' @param MinTxnRecords I typically set this to 2 so that there is at least one other instance of demand so that the forecasted values are not complete nonsense.
#' @param TimeUnit List the time unit your data is aggregated by. E.g. "day", "week", "month", "quarter", "year"
#' @param Lags Select the periods for all lag variables you want to create. E.g. c(1:5,52)
#' @param MovingAverages Select the periods for all moving average variables you want to create. E.g. c(1:5,52)
#' @param CalendarVariables Set to TRUE to have calendar variables created. The calendar variables are numeric representations of second, minute, hour, week day, month day, year day, week, isoweek, quarter, and year
#' @param HolidayGroups Input the holiday groups of your choice from the CreateHolidayVariable() function in this package
#' @param TimeTrendVariable Set to TRUE to have a time trend variable added to the model. Time trend is numeric variable indicating the numeric value of each record in the time series (by group). Time trend starts at 1 for the earliest point in time and increments by one for each success time point.
#' @param PowerRate 
#' @param SampleRate Set this to a value greater than 0. The calculation used is the number of records per group level raised to the power of PowerRate. Then that values is multiplied by SampleRate. 
#' @param PrintSteps Set to TRUE to have operation steps printed to the console 
#' @examples
#' \donttest{
#' DataSets <- IntermittentDemandDataGenerator(data,
#'                                             RestrictDateRange = TRUE,
#'                                             FC_Periods = 52,
#'                                             SaveData = FALSE,
#'                                             FilePath = NULL,
#'                                             TargetVariableName = "qty",
#'                                             DateVariableName = "date",
#'                                             GDL_Targets = NULL,
#'                                             GroupingVariables = "sku",
#'                                             MinTimeWindow = 1,
#'                                             MinTxnRecords = 2,
#'                                             Lags = 1:7,
#'                                             MovingAverages = seq(7,28,7),
#'                                             TimeTrendVariable = TRUE,
#'                                             TimeUnit = "day",
#'                                             CalendarVariables = c("wday",
#'                                                                   "mday",
#'                                                                   "yday",
#'                                                                   "week",
#'                                                                   "isoweek",
#'                                                                   "month",
#'                                                                   "quarter",
#'                                                                   "year"),
#'                                             HolidayGroups = "USPublicHolidays",
#'                                             PowerRate = 0.5,             
#'                                             SampleRate = 5,
#'                                             TargetWindowSamples = 5,
#'                                             PrintSteps = TRUE)
#' CountModelData <- DataSets$CountModelData
#' SizeModelData <- DataSets$SizeModelData
#' rm(DataSets)
#' }
#' @return Returns two data.table data sets: The first is a modeling data set for the count distribution while the second data set if for the size model data set.
#' @export
IntermittentDemandDataGenerator <- function(data,
                                            RestrictDateRange = TRUE,
                                            Case = 2L,
                                            FC_Periods = 52,
                                            SaveData = FALSE,
                                            FilePath = NULL,
                                            TargetVariableName = "qty",
                                            DateVariableName = "date",
                                            GDL_Targets = NULL,
                                            TimeUnit = "raw",
                                            TimeGroups = c("raw","day","week"),
                                            GroupingVariables = "sku",
                                            HierarchyGroupVars = NULL,
                                            MinTimeWindow = 1,
                                            MinTxnRecords = 2,
                                            Lags = 1:7,
                                            MovingAverages = seq(7,28,7),
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
  
  # Print Steps----
  if(PrintSteps) {
    print("Running initial data prep") 
  }
  
  # Ensure is data.table----
  if(!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Ensure Date Column is a Date----
  if(is.character(data[[eval(DateVariableName)]])) {
    if(tolower(TimeUnit) == "raw") {
      data.table::set(
        data, 
        j = eval(DateVariableName), 
        value = as.POSIXct(data[[eval(DateVariableName)]]))
    } else {
      data.table::set(
        data, 
        j = eval(DateVariableName), 
        value = as.Date(data[[eval(DateVariableName)]]))      
    }
  }
  
  # Round down dates (add option to not do this)----
  if(TimeUnit != "raw") {
    data.table::set(
      data,
      j = eval(DateVariableName),
      value = lubridate::floor_date(
        data[[eval(DateVariableName)]],
        unit = TimeUnit))
  }
  
  # Copy data----
  datax <- data.table::copy(data)
  
  # Group Concatenation----
  if (!is.null(GroupingVariables)) {
    if(length(GroupingVariables) > 1) {
      datax[, GroupVar := do.call(paste, c(.SD, sep = " ")), .SDcols = GroupingVariables]
      datax[, eval(GroupingVariables) := NULL]      
    } else {
      data.table::setnames(datax, eval(GroupingVariables), "GroupVar")
    }
    
    # Modify GroupingVariables argument
    ReverseGroupingVariables <- GroupingVariables
    GroupingVariables <- "GroupVar"
  }
  
  # Ensure datax is aggregated to proper time unit----
  if(Case == 1L) {
    if(TimeUnit != "raw") {
      datax <- datax[, lapply(.SD, sum), .SDcols = c(eval(TargetVariableName)), 
                     by = c(eval(GroupingVariables), eval(DateVariableName))]
    }
  }
  
  # Print Steps----
  if(PrintSteps) {
    print("Running ID_MetadataGenerator()") 
  }
  
  # Generate Metadata----
  MetaData <- ID_MetadataGenerator(
    data = datax,
    DateVariableName = DateVariableName,
    GroupingVariables = "GroupVar",
    MinTimeWindow = MinTimeWindow,
    MinTxnRecords = MinTxnRecords,
    DateInterval = TimeUnit)
  
  # Save Data----
  if(SaveData) {
    data.table::fwrite(
      MetaData, file = file.path(FilePath, "MetaData.csv"))
  }
  
  # Add Calendar Variables----
  if(!is.null(CalendarVariables)) {
    
    # Print Steps----
    if(PrintSteps) {
      print("Running CreateCalendarVariables()") 
    }
    
    # Run function----
    datax <- CreateCalendarVariables(
      datax, 
      DateCols = DateVariableName,
      AsFactor = FALSE, 
      TimeUnits = CalendarVariables)    
  }
  
  # Add Holiday Variables----
  if(!is.null(HolidayGroups)) {
    
    # Print Steps----
    if(PrintSteps) {
      print("Running CreateHolidayVariables()") 
    }
    
    # Run function----
    datax <- CreateHolidayVariables(
      datax, 
      DateCols = DateVariableName,
      HolidayGroups = HolidayGroups, 
      Holidays = NULL,
      GroupingVars = "GroupVar")    
  }
  
  # Print Steps----
  if(PrintSteps) {
    print("Running DT_GDL_Feature_Engineering()") 
  }
  
  # Define targets for AutoLagRollStats----
  if(Case == 1L) {
    targs <- c(eval(TargetVariableName))
  } else if(Case == 2L) {
    targs <- c(eval(TargetVariableName[1L:2L]))
  }
  
  # Add in the time varying features----
  datax <- AutoLagRollStats(
    
    # Data
    data                 = datax,
    DateColumn           = eval(DateVariableName),
    Targets              = targs,
    HierarchyGroups      = NULL,
    IndependentGroups    = "GroupVar",
    
    # Services
    TimeBetween          = "bla",
    TimeUnit             = if(tolower(TimeUnit) == "raw") "day" else TimeUnit,
    TimeUnitAgg          = TimeUnit,
    TimeGroups           = TimeGroups,
    RollOnLag1           = FALSE,
    Type                 = "Lag",
    SimpleImpute         = TRUE,
    
    # Calculated Columns
    Lags                  = c(Lags),
    MA_RollWindows        = c(MovingAverages),
    SD_RollWindows        = 0L,
    Skew_RollWindows      = 0L,
    Kurt_RollWindows      = 0L,
    Quantile_RollWindows  = 0L,
    Quantiles_Selected    = 0L)
  
  # Add Time Trend Variable----
  if(!is.null(GroupingVariables)) {
    
    # Print Steps----
    if(PrintSteps) {
      print("Running Time Trend Calculation") 
    }
    
    # Create trend----
    data.table::setorderv(
      datax, 
      cols = c(eval(GroupingVariables), eval(DateVariableName)), 
      order = c(1,-1))
    datax[, TimeTrend := 1:.N, by = list(GroupVar)]
  }
  
  # Print Steps----
  if(PrintSteps) {
    print("Running ID_BuildTrainDataSets()") 
  }
  
  # Run Final Build----
  packages <- c("RemixAutoML","data.table","forecast","lubridate")
  cores <- parallel::detectCores()
  
  # Create File Splitter Column Indicator----
  MetaData <- MetaData[, ID := runif(MetaData[,.N])][order(ID)][, ID := NULL]
  MetaData[, SelectRows := sample(c(seq_len(cores)), size = MetaData[,.N], replace = TRUE, prob = c(rep(1/cores, cores)))]
  datax <- merge(data.table::set(datax, j = "GroupVar", value = as.character(datax[["GroupVar"]])), MetaData[,.SD, .SDcols = c("GroupVar","SelectRows")], by = "GroupVar", all = FALSE)
  
  # Parallelize Build----
  cl <- parallel::makePSOCKcluster(cores)
  doParallel::registerDoParallel(cl)
  Results <- foreach::foreach(
    i = seq_len(cores),
    .combine = function(x,...) mapply(function(...) data.table::rbindlist(list(...), fill = TRUE),x,...,SIMPLIFY=FALSE),
    .multicombine = TRUE,
    .packages = packages) %dopar% {
      
      # Loops----
      ModelDataSets <- ID_BuildTrainDataSets(
        MetaData = MetaData[SelectRows == i],
        data = datax[SelectRows == i],
        Case = 2L,
        TargetVariableName = TargetVariableName,
        DateVariableName = DateVariableName,
        GroupingVariables = GroupingVariables,
        FC_Periods = FC_Periods,
        TimeUnit = TimeUnit,
        PowerRate = PowerRate,
        SampleRate = SampleRate,
        TargetWindowSamples = TargetWindowSamples)
      
      # Store individual file outputs----
      if(Case == 1L) {
        CountModelData <- ModelDataSets$CountModelData
        SizeModelData <- ModelDataSets$SizeModelData
        list(CountModelData = CountModelData, SizeModelData = SizeModelData)
      } else if(Case == 2L) {
        CountModelData <- ModelDataSets$CountModelData
        list(CountModelData = CountModelData)
      }
    }
  
  # Remove Zeros----
  if(Case == 1L) {
    CountModelData <- Results$CountModelData
    SizeModelData <- Results$SizeModelData
    SizeModelData <- SizeModelData[Size != 0]
  } else if(Case == 2L) {
    CountModelData <- Results$CountModelData
  }
  
  # shut down parallel objects----
  parallel::stopCluster(cl)
  rm(cl)
  
  # Back-transform GroupingVariables----
  if(length(ReverseGroupingVariables) > 1) {
    CountModelData[, eval(ReverseGroupingVariables) := data.table::tstrsplit(GroupVar, " ")][, GroupVar := NULL]
    if(exists("SizeModelData")) SizeModelData[, eval(ReverseGroupingVariables) := data.table::tstrsplit(GroupVar, " ")][, GroupVar := NULL]
  } else {
    data.table::setnames(CountModelData, eval(GroupingVariables), eval(ReverseGroupingVariables))
    if(exists("SizeModelData")) data.table::setnames(SizeModelData, eval(GroupingVariables), eval(ReverseGroupingVariables))
  }
  
  # Save Data----
  if(SaveData) {
    
    # Save modeling data sets----
    data.table::fwrite(CountModelData, file = file.path(FilePath, "CountModelData.csv"))
    if(exists("SizeModelData")) data.table::fwrite(SizeModelData, file = file.path(FilePath, "SizeModelData.csv"))
    
    # Save column names for modeling data----
    CountPredNames <- names(CountModelData)
    if(exists("SizeModelData")) SizeModelData <- names(SizeModelData)
    save(CountPredNames, file = file.path(FilePath,"CountPredNames.Rdata"))
    if(exists("SizeModelData")) save(SizeModelData, file = file.path(FilePath,"SizePredNames.Rdata"))
  }
  
  # Return CountModelData and SizeModelData----
  if(Case == 1L) {
    return(list(CountData = CountModelData, 
                SizeData = SizeModelData))
  } else if(Case == 2L) {
    return(list(CountData = CountModelData))
  }
}

#' ID_MetadataGenerator for summary metadata for transactional data
#'
#' ID_MetadataGenerator for summary metadata for transactional data. The data returned from this function feeds into the IntermittentDemandBootStrapper() function.
#'
#' @param data This is your transactional level data
#' @param RestrictDateRange = TRUE
#' @param DateVariableName Bla
#' @param GroupingVariables Bla
#' @param MinTimeWindow The number of time periods you would like to omit for training. Default is 1 so that at a minimum, there is at least one period of values to forecast. You can set it up to a larger value if you do not want more possible target windows for the lower target window values.
#' @param MinTxnRecords I typically set this to 2 so that there is at least one other instance of demand so that the forecasted values are not complete nonsense.
#' @param DateInterval This is the time unit for determining date calculations
#' @noRd
#' @examples
#' \donttest{
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
ID_MetadataGenerator <- function(data,
                                 RestrictDateRange = TRUE,
                                 DateVariableName = NULL,
                                 GroupingVariables = NULL,
                                 MinTimeWindow = 1,
                                 MinTxnRecords = 2,
                                 DateInterval = "day") {
  
  # Define max date for sampling window----
  if(RestrictDateRange) {
    
    # Per entity ID
    data[, max_date := max(get(DateVariableName)), by = list(GroupVar)]
    
    # Set up base table----
    Step1 <- data[, .(.N, max(lubridate::floor_date(as.Date(max_date, unit = DateInterval)))), by = list(GroupVar)]
    data.table::setorderv(Step1, "N", -1)
    
  } else {
    if(tolower(DateInterval) == "week") {
      max_date <- data[, max(get(DateVariableName))][[1]] - 7 * MinTimeWindow
    } else if(tolower(DateInterval) == "day") {
      max_date <- data[, max(get(DateVariableName))][[1]] - MinTimeWindow
    } else if(tolower(DateInterval) == "month") {
      max_date <- data[, max(get(DateVariableName))][[1]] %m+% months(-MinTimeWindow)
    }
    
    # Set up base table----
    Step1 <- data[, .(.N, lubridate::floor_date(as.Date(max_date, unit = DateInterval))), by = list(GroupVar)]
    data.table::setorderv(Step1, "N", -1)
  }
  
  # Gather second to last distinct date by GroupingVariable----
  Step2 <- data[, .(.N, get(DateVariableName)), by = list(GroupVar)]
  data.table::setorderv(Step2, c("GroupVar","V2"), c(1,-1))
  Step2 <- Step2[, sum(N), by = c(eval(GroupingVariables),"V2")]
  Step3 <- Step2[, txn := .N:1, by = list(GroupVar)]
  Step4 <- Step3[txn == MinTxnRecords]
  Step4[, txn := NULL]
  data.table::setnames(Step4,c("V1","V2"),c("Txns","MinDate"))
  keep <- c(eval(GroupingVariables),"MinDate")
  Step5 <- Step4[, ..keep]
  
  # Merge, change names, filter out infrequent levels----
  BaseTable2 <- merge(Step5, Step1, by = eval(GroupingVariables), all = FALSE)
  data.table::setnames(BaseTable2, c("N","V2"), c("Txns", "MaxDate"))
  if(tolower(DateInterval) == "raw") {
    BaseTable3 <- BaseTable2[, Date_Range := as.numeric(difftime(MaxDate, MinDate, units = "day"))][order(-Txns)]
  } else {
    BaseTable3 <- BaseTable2[, Date_Range := as.numeric(difftime(MaxDate, MinDate, units = DateInterval))][order(-Txns)]  
  }
  
  # Remove levels with less than MinTxnRecords distinct past dates----
  MetaData <- BaseTable3[Txns >= eval(MinTxnRecords)][Date_Range > 0]
  
  # Return data----
  return(MetaData)
}

#' ID_TrainingDataGenerator for subsetting data
#' 
#' ID_TrainingDataGenerator for subsetting data for the IntermittentDemandBootStrapper() function.
#' 
#' @param data Source data
#' @param Type "timetoevent1", "eventinwindow1"
#' @param TargetVariableName Name of the variables to run feature engineering on. List the actual target variable name first.
#' @param Level The individual level of your group variable
#' @param GroupingVariables Your grouping variables
#' @param DateVariableName Name of your date variable
#' @param RandomStartDate The date to partition the data
#' @param TimeUnit This is the TimeUnit you selected for aggregation
#' @param TargetWindow The length of the target window sampled
#' @noRd
#' @return Returns two data sets for the IntermittentDemandBootStrapper() function based on a single level from the grouping variables.
ID_TrainingDataGenerator <- function(data,
                                     Type = "timetoevent1",
                                     TargetVariableName = NULL,
                                     Level = NULL,
                                     DateVariableName = NULL,
                                     RandomStartDate = NULL,
                                     TimeUnit = NULL,
                                     TargetWindow = NULL) {
  
  # historical data <--> point in time <--> target window----
  histDemandRaw <- data[get(DateVariableName) < eval(RandomStartDate)]
  
  # Data within target window----
  counter <- 0L
  for (tar in TargetWindow) {
    
    # Target variable data
    if(lubridate::is.POSIXct(data[[eval(DateVariableName)]])) {
      targetDemand  <- data[
        get(DateVariableName) >= eval(RandomStartDate) &
          get(DateVariableName) - 86400 * eval(tar) <= eval(RandomStartDate)]
    } else {
      targetDemand  <- data[
        get(DateVariableName) >= eval(RandomStartDate) &
          get(DateVariableName) - eval(tar) <= eval(RandomStartDate)]
    }
    
    # Add in the time since last demand instance from RandomStartDate----
    histDemandRaw <- histDemandRaw[order(-get(DateVariableName))][
      , TimeSinceLastDemand := as.numeric(difftime(RandomStartDate,get(DateVariableName), units = TimeUnit))]
    
    # Remove meta data for feature creation set----
    features <- histDemandRaw[order(-get(DateVariableName))][
      , paste0(eval(DateVariableName)) := NULL][1,]
    data.table::set(features, 
                    j = "FC_Window", 
                    value = tar)
    
    # Remove data and rename target variable----
    keep <- eval(TargetVariableName)
    targetDemand <- targetDemand[, ..keep]
    data.table::setnames(targetDemand, 
                         old = eval(TargetVariableName[1]), 
                         new = "Size")
    
    # Merge Features and Targets----
    if(nrow(targetDemand) != 0) {
      TargetCount <- cbind(targetDemand[, .(Counts = .N)], features)
      TargetSize  <- cbind(targetDemand, features)
    } else {
      TargetCount <- cbind(data.table(Counts = 0), features)
      TargetSize  <- cbind(data.table::data.table(Temp = 0), features)
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
  return(
    list(
      CountData = CountFinal, 
      SizeData = SizeFinal))
}

#' ID_TrainingDataGenerator2 for subsetting data
#' 
#' ID_TrainingDataGenerator2 for subsetting data for the IntermittentDemandBootStrapper() function.
#' 
#' @param data Source data
#' @param TargetVariableName vector of variable names
#' @param Level The individual level of your group variable
#' @param GroupingVariables Your grouping variables
#' @param DateVariableName Name of your date variable
#' @param RandomStartDate The date to partition the data
#' @param TimeUnit This is the TimeUnit you selected for aggregation
#' @param TargetWindow The length of the target window sampled
#' @noRd
#' @return Returns two data sets for the IntermittentDemandBootStrapper() function based on a single level from the grouping variables.
ID_TrainingDataGenerator2 <- function(data,
                                      TargetVariableName = NULL,
                                      Level = NULL,
                                      DateVariableName = NULL,
                                      RandomStartDate = NULL,
                                      TimeUnit = NULL,
                                      TargetWindow = NULL) {
  
  # historical data <--> point in time <--> target window----
  histDemandRaw <- data[get(DateVariableName) < eval(RandomStartDate)]
  
  # Data within target window----
  counter <- 0L
  for (tar in TargetWindow) {
    
    # Classification target variable data----
    if(lubridate::is.POSIXct(data[[eval(DateVariableName)]])) {
      binarytarget  <- min(data[get(DateVariableName) >= eval(RandomStartDate) & get(DateVariableName) - 86400 * eval(tar) <= eval(RandomStartDate), 
                                get(TargetVariableName[1L])], na.rm = TRUE)
    } else {
      binarytarget  <- min(data[get(DateVariableName) >= eval(RandomStartDate) & get(DateVariableName) - eval(tar) <= eval(RandomStartDate), 
                                get(TargetVariableName[1L])], na.rm = TRUE)
    }
    
    # Time to event target variable data----
    timetoevent <- data[1L, get(TargetVariableName[2L])]
    outcome <- data[1L, get(TargetVariableName[3L])]
    
    # Add in the time since last demand instance from RandomStartDate----
    histDemandRaw <- histDemandRaw[order(-get(DateVariableName))][
      , TimeSinceLastDemand := as.numeric(difftime(RandomStartDate,get(DateVariableName), units = TimeUnit))]
    
    # Remove meta data for feature creation set----
    features <- histDemandRaw[order(-get(DateVariableName))][
      , paste0(eval(DateVariableName)) := NULL][1,]
    data.table::set(features, j = "FC_Window", value = tar)
    
    # Merge Features and Targets----
    temp <- cbind(binarytarget, timetoevent, outcome, features)
    data.table::setnames(temp, names(temp)[1L], eval(TargetVariableName[1L]))
    data.table::setnames(temp, names(temp)[2L], eval(TargetVariableName[2L]))
    data.table::setnames(temp, names(temp)[3L], eval(TargetVariableName[3L]))
    data.table::set(temp, data.table::fifelse(temp[[eval(TargetVariableName)]] == 0, 1, 0))
    
    # Combine data sets----
    counter <- counter + 1L
    if(counter == 1L) {
      Final <- temp
    } else {
      Final <- data.table::rbindlist(list(Final,temp), fill = TRUE)
    }
  }
  
  # Output data file----
  return(data = Final)
}

#' ID_BuildTrainDataSets for assembling data
#' 
#' ID_BuildTrainDataSets for assembling data for the IntermittentDemandBootStrapper() function.
#' 
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
#' @noRd
#' @return Returns the count modeling data and the size modeling data
ID_BuildTrainDataSets <- function(MetaData,
                                  data,
                                  Case = 2L,
                                  TargetVariableName = NULL,
                                  DateVariableName = NULL,
                                  GroupingVariables = NULL,
                                  FC_Periods,
                                  TimeUnit = "week",
                                  PowerRate = 0.5,
                                  SampleRate = 5,
                                  TargetWindowSamples = 5) {
  
  # Define DateUnit----
  if(TimeUnit == "week") {
    DateUnit <- 7
  } else if (TimeUnit == "day") {
    DateUnit <- 1
  } else if (TimeUnit == "month") {
    DateUnit <- 30
  } else {
    DateUnit <- 1
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
    issuances  <- as.numeric(ceiling(MetaData[get(GroupingVariables) == eval(level), "Txns"][[1]]))
    iterations <- ceiling((issuances^PowerRate)*SampleRate)
    
    # Check to ensure issuances and iterations exist----
    if(length(issuances) == 0 | length(iterations) == 0) next
    j <- j + 1L
    
    # Track progress----
    print(j / LevelCount)
    
    # Initialize / reset storage lists----
    countData <- list()
    sizeData  <- list()
    
    # Subset data before looping through a single GroupingVariable----
    level_data <- data[get(GroupingVariables) == eval(level)]
    
    # Set date range----
    DateRange <- MetaData[get(GroupingVariables) == eval(level), "Date_Range"][[1]]
    
    # Data generator
    for (i in seq_len(iterations)) {
      
      # Set Random Starting Date----
      if(lubridate::is.POSIXct(MetaData$MinDate[1])) {
        RandomStartDate  <- MetaData[GroupVar == eval(level), "MinDate"][[1]] + 
          DateUnit * ceiling(sample(86400:(86400 * DateUnit * DateRange), 1))
      } else {
        RandomStartDate  <- MetaData[get(GroupingVariables) == eval(level), "MinDate"][[1]] + 
          DateUnit * ceiling(sample(1:(DateUnit*DateRange), 1))
      }
      
      # Set Target Window Max Sample Window----
      TargetWindowMax <- ceiling(as.numeric(difftime(
              lubridate::floor_date(MetaData[eval(GroupingVariables) == eval(level), "MaxDate"][[1]], unit = TimeUnit), 
              RandomStartDate, 
              units = TimeUnit)))
      
      # Set Target Window----
      TargetWindow <- sample(x = seq_len(TargetWindowMax),
                             size = TargetWindowSamples,
                             replace = TRUE)
      
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
          countData <- data.table::rbindlist(
            list(countData, SampleData$CountData), fill = TRUE)
          sizeData <- data.table::rbindlist(
            list(sizeData, SampleData$SizeData), fill = TRUE)
        }
      } else if(Case == 2L) {
        if(i == 1L) {
          countData <- SampleData$data
        } else {
          countData <- data.table::rbindlist(
            list(countData, SampleData$data), fill = TRUE)
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
    return(list(
      CountModelData = CMD, 
      SizeModelData = SMD))
  } else if(Case == 2L) {
    return(list(
      CountModelData = CMD, 
      SizeModelData = NULL))
  }
}
