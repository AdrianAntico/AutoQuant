#' IntermittentDemandScoringDataGenerator creates the scoring data for forecasting
#'
#' IntermittentDemandScoringDataGenerator creates the scoring data for forecasting. It will recreate the same features used for modeling, take the most recent record, and then duplicate those records for each forecast period specifed.
#' 
#' @author Adrian Antico
#' @family Automated Time Series
#' @param data This is your source data
#' @param SaveData Set to TRUE to save the output data to file
#' @param FilePath Set a path file have the data saved there
#' @param TargetVariableName Name or column number of your target variable
#' @param DateVariableName Name or column number of your date variable
#' @param GroupingVariables Name or column number of your group variables
#' @param Lags The number of lags used in building the modeling data sets
#' @param MovingAverages The number of moving averages used in building the modeling data sets
#' @param TimeTrendVariable Set to TRUE if you did so in model data creation
#' @param TimeUnit Set to the same time unit used in modeling data creation
#' @param CurrentDate Set this to the current date or a date that you want. It is user specified in case you want to score historical data.
#' @param CalendarVariables Set this to the same setting you used in modeling data creation
#' @param HolidayGroups Set this to the same setting you used in modeling data creation
#' @examples 
#' \donttest{
#'  ScoringData <- IntermittentDemandScoringDataGenerator(
#'    data = data,
#'    SaveData = FALSE,
#'    FilePath = NULL,
#'    TargetVariableName = "qty",
#'    DateVariableName = "date",
#'    GroupingVariables = "sku",
#'    Lags = 1:7,
#'    MovingAverages = seq(7,28,7),
#'    TimeTrendVariable = TRUE,
#'    TimeUnit = "day",
#'    CurrentDate = NULL,
#'    CalendarVariables = c("wday",
#'                          "mday",
#'                          "yday",
#'                          "week",
#'                          "isoweek",
#'                          "month",
#'                          "quarter",
#'                          "year"),
#'    HolidayGroups = "USPublicHolidays")
#' }
#' @return Returns the most recent records for every level of your grouping variables with all the feature used in model building
#' @export
IntermittentDemandScoringDataGenerator <- function(data = NULL,
                                                   FC_Periods = 52,
                                                   SaveData = FALSE,
                                                   FilePath = NULL,
                                                   TargetVariableName = "qty",
                                                   DateVariableName = "date",
                                                   GroupingVariables = "sku",
                                                   Lags = 1:7,
                                                   MovingAverages = seq(7,28,7),
                                                   TimeTrendVariable = TRUE,
                                                   TimeUnit = "day",
                                                   CurrentDate = NULL,
                                                   CalendarVariables = c("wday",
                                                                         "mday",
                                                                         "yday",
                                                                         "week",
                                                                         "isoweek",
                                                                         "month",
                                                                         "quarter",
                                                                         "year"),
                                                   HolidayGroups = "USPublicHolidays") {
  
  # Copy data----
  datax <- data.table::copy(data)
  
  # Current date calculated like data gen process----
  if(is.null(CurrentDate)) {
    if(tolower(TimeUnit) == "day") {
      CurrentDate <- lubridate::floor_date(x = Sys.Date(), unit = TimeUnit)
    } else if(tolower(TimeUnit) == "week") {
      CurrentDate <- lubridate::floor_date(x = Sys.Date(), unit = "weeks")
    } else if(tolower(TimeUnit) == "month") {
      CurrentDate <- lubridate::floor_date(x = Sys.Date(), unit = "month")
    }
  }

  # Ensure is data.table----
  if(!data.table::is.data.table(datax)) {
    datax <- data.table::as.data.table(datax)
  }
  
  # Round up dates----
  datax[, paste0(eval(DateVariableName)) := lubridate::floor_date(
    get(DateVariableName),
    unit = TimeUnit)]
  
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
  
  # Ensure data is aggregated to proper time unit----
  datax <- datax[, sum(get(TargetVariableName)), 
               by = c(eval(GroupingVariables), eval(DateVariableName))]
  data.table::setnames(datax, "V1", eval(TargetVariableName))
  
  # Add Calendar Variables----
  if(!is.null(CalendarVariables)) {
    datax <- CreateCalendarVariables(
      datax, 
      DateCols = DateVariableName,
      AsFactor = FALSE, 
      TimeUnits = CalendarVariables)    
  }
  
  # Add Holiday Variables----
  if(!is.null(HolidayGroups)) {
    datax <- CreateHolidayVariables(
      datax, 
      DateCols = DateVariableName,
      HolidayGroups = HolidayGroups, 
      Holidays = NULL)    
  }

  # Add in the time varying features----
  datax <- DT_GDL_Feature_Engineering(
    datax,
    lags           = Lags,
    periods        = MovingAverages,
    statsNames     = c("MA"),
    targets        = TargetVariableName,
    groupingVars   = GroupingVariables,
    sortDateName   = DateVariableName,
    timeDiffTarget = "TimeGap",
    timeAgg        = TimeUnit,
    WindowingLag   = 0,
    Type           = "Lag",
    SimpleImpute   = TRUE)
  
  # Add Time Trend Variable----
  if(!is.null(GroupingVariables)) {
    data.table::setorderv(
      datax, 
      cols = c(eval(GroupingVariables), eval(DateVariableName)), 
      order = c(1,-1))
    datax[, TimeTrend := 1:.N, by = eval(GroupingVariables)]
  }
  
  # Add in the time since last demand instance from RandomStartDate----
  datax <- datax[order(-get(DateVariableName))][
      , TimeSinceLastDemand := difftime(CurrentDate, get(DateVariableName), units = TimeUnit)]
  
  # Subset data----
  datax <- datax[TimeTrend == 1]
  
  # Add FC_Window----
  temp <- data.table::CJ(GroupVar = as.character(datax[["GroupVar"]]), FC_Window = seq_len(FC_Periods))
  datax <- merge(datax, temp, by = "GroupVar", all = FALSE)
  
  # Save data----
  if(SaveData) {
    data.table::fwrite(datax, file = file.path(FilePath,"ScoringData.csv"))
  }
  
  # Return datax----
  return(datax)
}