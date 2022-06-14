#' @title CreateCalendarVariables
#'
#' @description CreateCalendarVariables Rapidly creates calendar variables based on the date column you provide
#'
#' @author Adrian Antico
#' @family Feature Engineering
#'
#' @param data This is your data
#' @param DateCols Supply either column names or column numbers of your date columns you want to use for creating calendar variables
#' @param AsFactor Set to TRUE if you want factor type columns returned; otherwise integer type columns will be returned
#' @param TimeUnits Supply a character vector of time units for creating calendar variables. Options include: "second", "minute", "hour", "wday", "mday", "yday", "week", "isoweek", "wom" (week of month), "month", "quarter", "year"
#' @param CachePath Path to data in a local directory. .csv only for now
#' @param Debug = FALSE
#' @examples
#' \dontrun{
#' # Create fake data with a Date column----
#' data <- RemixAutoML::FakeDataGenerator(
#'   Correlation = 0.75,
#'   N = 25000L,
#'   ID = 2L,
#'   ZIP = 0L,
#'   FactorCount = 4L,
#'   AddDate = TRUE,
#'   Classification = FALSE,
#'   MultiClass = FALSE)
#' for(i in seq_len(20L)) {
#'   print(i)
#'   data <- data.table::rbindlist(
#'     list(data, RemixAutoML::FakeDataGenerator(
#'     Correlation = 0.75,
#'     N = 25000L,
#'     ID = 2L,
#'     ZIP = 0L,
#'     FactorCount = 4L,
#'     AddDate = TRUE,
#'     Classification = FALSE,
#'     MultiClass = FALSE)))
#' }
#'
#' # Create calendar variables - automatically excludes
#' #   the second, minute, and hour selections since
#' #   it is not timestamp data
#' runtime <- system.time(
#'   data <- RemixAutoML::CreateCalendarVariables(
#'     data = data,
#'     DateCols = "DateTime",
#'     AsFactor = FALSE,
#'     TimeUnits = c("second",
#'                   "minute",
#'                   "hour",
#'                   "wday",
#'                   "mday",
#'                   "yday",
#'                   "week",
#'                   "isoweek",
#'                   "wom",
#'                   "month",
#'                   "quarter",
#'                   "year")))
#' head(data)
#' print(runtime)
#' }
#' @return Returns your data.table with the added calendar variables at the end
#' @export
CreateCalendarVariables <- function(data,
                                    DateCols = NULL,
                                    AsFactor = FALSE,
                                    TimeUnits = "wday",
                                    CachePath = NULL,
                                    Debug = FALSE) {

  if(Debug) print('CreateHolidayVariables 1')

  # Load data from file if CachePath is not NULL
  if(length(CachePath) > 0L) {
    if(Debug) print('CreateHolidayVariables 1.1')
    data <- RemixAutoML:::ReactiveLoadCSV(Infile = CachePath, Debug = Debug)
    if(Debug) print('CreateHolidayVariables 1.2')
    for(i in DateCols) if(class(data[[i]])[1L] %in% c('character')) {
      if(Debug) print('CreateHolidayVariables 1.3')
      data.table::set(data, j = eval(i), value = as.Date(data[[i]], format = "%m/%d/%y"))
    }
  }

  # Debug
  if(Debug) print('CreateHolidayVariables 2')

  # Check args
  if(!is.logical(AsFactor)) {
    if(Debug) print("AsFactor needs to be TRUE or FALSE")
    return(data)
  }
  if(!(any(tolower(TimeUnits) %chin% c("second","minute","hour","wday","mday","yday","week","isoweek","wom","month","quarter","year")))) {
    if(Debug) print("TimeUnits needs to be one of 'second', 'minute', 'hour', 'wday','mday', 'yday','week','wom','month', 'quarter', 'year'")
    return(data)
  }

  # Turn DateCols into character names if not already
  for(i in DateCols) if(!is.character(DateCols[i])) DateCols[i] <- names(data)[DateCols[i]]

  # Revise TimeUnits Based on Data
  x <- 0L
  TimeList <- list()
  Cols <- c()
  for(i in seq_along(DateCols)) {
    if(!any(TimeUnits %chin% c("second","minute","hour"))) {
      TimeList[[i]] <- TimeUnits
      Cols[i] <- length(TimeList[[i]])
    } else {
      if(min(data.table::as.ITime(data[[eval(DateCols[i])]])) - max(data.table::as.ITime(data[[eval(DateCols[i])]])) == 0L) {
        TimeList[[i]] <- TimeUnits[!(tolower(TimeUnits) %chin% c("second", "minute", "hour"))]
        Cols[i] <- length(TimeList[[i]])
      } else {
        TimeList[[i]] <- TimeUnits
        Cols[i] <- length(TimeList[[i]])
      }
    }
  }

  # Allocate data.table cols
  data.table::alloc.col(DT = data, ncol(data) + sum(Cols))

  # Create DateCols to data.table IDateTime types
  for(i in seq_along(DateCols)) {
    if(length(TimeList) > 0L) {
      if(any(tolower(TimeList[[i]]) %chin% c("second", "minute", "hour"))) {
        data.table::set(data, j = paste0("TIME_", eval(DateCols[i])), value = data.table::as.ITime(data[[eval(DateCols[i])]]))
      } else if(any(tolower(TimeList[[i]]) %chin% c("wday","mday","yday","week","isoweek","wom","month","quarter","year"))) {
        data.table::set(data, j = paste0("DATE_", eval(DateCols[i])), value = data.table::as.IDate(data[[eval(DateCols[i])]]))
      }
    }
  }

  # Build Features ----
  # i = 1L
  for(i in seq_along(DateCols)) {

    # Define DateCols ----
    DateColRef <- DateCols[i]

    # Get unique date values in table and then merge back to source data at the end one time ----
    if(any(tolower(TimeList[[i]]) %chin% c("second", "minute", "hour"))) {
      DataCompute <- unique(data[, .SD, .SDcols = c(paste0("TIME_", DateColRef), paste0("DATE_", DateColRef))])
    } else {
      DataCompute <- unique(data[, .SD, .SDcols = c(paste0("DATE_", DateColRef))])
    }

    # Build calendar variables ----
    # j = TimeList[[i]][1L]
    for(j in TimeList[[i]]) {
      if(tolower(j) == "second") {
        if(AsFactor) {
          data.table::set(DataCompute, j = paste0(DateColRef, "_", j), value = as.factor(data.table::second(DataCompute[[eval(paste0("TIME_", DateColRef))]])))
        } else {
          data.table::set(DataCompute, j = paste0(DateColRef, "_", j), value = data.table::second(DataCompute[[eval(paste0("TIME_", DateColRef))]]))
        }
      } else if(tolower(j) == "minute") {
        if(AsFactor) {
          data.table::set(DataCompute, j = paste0(DateColRef, "_", TimeList[[i]][j]), value = as.factor(data.table::minute(DataCompute[[eval(paste0("TIME_", DateColRef))]])))
        } else {
          data.table::set(DataCompute, j = paste0(DateColRef, "_", j), value = data.table::minute(DataCompute[[paste0("TIME_", DateColRef)]]))
        }
      } else if(tolower(j) == "hour") {
        if(AsFactor) {
          data.table::set(DataCompute, j = paste0(DateColRef, "_", TimeList[[i]][j]), value = as.factor(data.table::hour(DataCompute[[eval(paste0("TIME_", DateColRef))]])))
        } else {
          data.table::set(DataCompute, j = paste0(DateColRef, "_", j), value = data.table::hour(DataCompute[[eval(paste0("TIME_", DateColRef))]]))
        }
      } else if(tolower(j) == "wday") {
        if(AsFactor) {
          data.table::set(DataCompute, j = paste0(DateColRef, "_", j), value = as.factor(data.table::wday(DataCompute[[eval(paste0("DATE_", DateColRef))]])))
        } else {
          data.table::set(DataCompute, j = paste0(DateColRef, "_", j), value = data.table::wday(DataCompute[[eval(paste0("DATE_", DateColRef))]]))
        }
      } else if(tolower(j) == "mday") {
        if(AsFactor) {
          data.table::set(DataCompute, j = paste0(DateColRef, "_", j), value = as.factor(data.table::mday(DataCompute[[eval(paste0("DATE_", DateColRef))]])))
        } else {
          data.table::set(DataCompute, j = paste0(DateColRef, "_", j), value = as.integer(data.table::mday(DataCompute[[eval(paste0("DATE_", DateColRef))]])))
        }
      } else if(tolower(j) == "yday") {
        if(AsFactor) {
          data.table::set(DataCompute, j = paste0(DateColRef, "_", j), value = as.factor(data.table::yday(DataCompute[[eval(paste0("DATE_", DateColRef))]])))
        } else {
          data.table::set(DataCompute, j = paste0(DateColRef, "_", j), value = data.table::yday(DataCompute[[eval(paste0("DATE_", DateColRef))]]))
        }
      } else if(tolower(j) == "week") {
        if(AsFactor) {
          data.table::set(DataCompute, j = paste0(DateColRef, "_", j), value = as.factor(data.table::week(DataCompute[[eval(paste0("DATE_", DateColRef))]])))
        } else {
          data.table::set(DataCompute, j = paste0(DateColRef, "_", j), value = data.table::week(DataCompute[[eval(paste0("DATE_", DateColRef))]]))
        }
      } else if(tolower(j) == "isoweek") {
        if(AsFactor) {
          data.table::set(DataCompute, j = paste0(DateColRef, "_", j), value = as.factor(data.table::isoweek(DataCompute[[eval(paste0("DATE_", DateColRef))]])))
        } else {
          data.table::set(DataCompute, j = paste0(DateColRef, "_", j), value = data.table::isoweek(DataCompute[[eval(paste0("DATE_", DateColRef))]]))
        }
      } else if(tolower(j) == "month") {
        if(AsFactor) {
          data.table::set(DataCompute, j = paste0(DateColRef, "_", j), value = as.factor(data.table::month(DataCompute[[eval(paste0("DATE_", DateColRef))]])))
        } else {
          data.table::set(DataCompute, j = paste0(DateColRef, "_", j), value = data.table::month(DataCompute[[eval(paste0("DATE_", DateColRef))]]))
        }
      } else if(tolower(j) == "wom") {
        if (AsFactor) {
          data.table::set(DataCompute, j = paste0(DateColRef, "_", j), value = as.factor(data.table::fifelse(ceiling(data.table::mday(DataCompute[[eval(paste0("DATE_", DateColRef))]])/7) == 5, 4, ceiling(data.table::mday(DataCompute[[eval(paste0("DATE_", DateColRef))]])/7))))
        } else {
          data.table::set(DataCompute, j = paste0(DateColRef, "_", j), value = data.table::fifelse(ceiling(data.table::mday(DataCompute[[eval(paste0("DATE_", DateColRef))]])/7) == 5, 4, ceiling(data.table::mday(DataCompute[[eval(paste0("DATE_", DateColRef))]])/7)))
        }
      } else if(tolower(j) == "quarter") {
        if(AsFactor) {
          data.table::set(DataCompute, j = paste0(DateColRef, "_", j), value = as.factor(data.table::quarter(DataCompute[[eval(paste0("DATE_", DateColRef))]])))
        } else {
          data.table::set(DataCompute, j = paste0(DateColRef, "_", j), value = as.integer(data.table::quarter(DataCompute[[eval(paste0("DATE_", DateColRef))]])))
        }
      } else if(tolower(j) == "year") {
        if(AsFactor) {
          data.table::set(DataCompute, j = paste0(DateColRef, "_", j), value = as.factor(data.table::year(DataCompute[[eval(paste0("DATE_", DateColRef))]])))
        } else {
          data.table::set(DataCompute, j = paste0(DateColRef, "_", j), value = data.table::year(DataCompute[[eval(paste0("DATE_", DateColRef))]]))
        }
      }
    }

    # Merge back----
    if(any(tolower(TimeList[[i]]) %chin% c("second", "minute", "hour"))) {
      data <- merge(data, DataCompute, by = c(paste0("TIME_", DateColRef), paste0("DATE_", DateColRef)), all = FALSE)
    } else {
      data <- merge(data, DataCompute, by = c(paste0("DATE_", DateColRef)), all = FALSE)
    }

    # Remove ITime columns if they exist and change names of non IDate columns----
    if(any(tolower(TimeList[[i]]) %chin% c("second", "minute", "hour"))) data.table::set(data, j = paste0("TIME_", DateColRef), value = NULL)
    if(any(tolower(TimeList[[i]]) %chin% c("wday","mday","yday","week","isoweek","wom","month","quarter","year"))) data.table::set(data, j = paste0("DATE_", DateColRef), value = NULL)
  }

  # Return data ----
  if(length(CachePath) > 0L) {
    data.table::fwrite(x = data, file = CachePath)
    if(Debug) print('Returned here 1')
    return("Remix")
  } else {
    if(Debug) print('Returned here 2')
    return(data)
  }
}

#' @title CreateHolidayVariables
#'
#' @description CreateHolidayVariables Rapidly creates holiday count variables based on the date columns you provide
#'
#' @author Adrian Antico
#' @family Feature Engineering
#'
#' @param data This is your data
#' @param DateCols Supply either column names or column numbers of your date columns you want to use for creating calendar variables
#' @param LookbackDays Default NULL which investigates Date - Lag1Date to compute Holiday's per period. Otherwise it will lookback LokkbackDays.
#' @param HolidayGroups Pick groups
#' @param Holidays Pick holidays
#' @param Print Set to TRUE to print iteration number to console
#' @param CachePath = NULL
#' @param Debug = FALSE
#' @import timeDate
#' @examples
#' \dontrun{
#' # Create fake data with a Date----
#' data <- RemixAutoML::FakeDataGenerator(
#'   Correlation = 0.75,
#'   N = 25000L,
#'   ID = 2L,
#'   ZIP = 0L,
#'   FactorCount = 4L,
#'   AddDate = TRUE,
#'   Classification = FALSE,
#'   MultiClass = FALSE)
#' for(i in seq_len(20L)) {
#'   print(i)
#'   data <- data.table::rbindlist(list(data,
#'   RemixAutoML::FakeDataGenerator(
#'     Correlation = 0.75,
#'     N = 25000L,
#'     ID = 2L,
#'     ZIP = 0L,
#'     FactorCount = 4L,
#'     AddDate = TRUE,
#'     Classification = FALSE,
#'     MultiClass = FALSE)))
#' }
#' # Run function and time it
#' runtime <- system.time(
#'   data <- RemixAutoML::CreateHolidayVariables(
#'     data,
#'     DateCols = "DateTime",
#'     LookbackDays = NULL,
#'     HolidayGroups = c("USPublicHolidays","EasterGroup",
#'       "ChristmasGroup","OtherEcclesticalFeasts"),
#'     Holidays = NULL,
#'     Print = FALSE))
#' head(data)
#' print(runtime)
#' }
#' @return Returns your data.table with the added holiday indicator variable
#' @export
CreateHolidayVariables <- function(data,
                                   DateCols = NULL,
                                   LookbackDays = NULL,
                                   HolidayGroups = c('USPublicHolidays','EasterGroup','ChristmasGroup','OtherEcclesticalFeasts'),
                                   Holidays = NULL,
                                   Print = FALSE,
                                   CachePath = NULL,
                                   Debug = FALSE) {

  if(Debug) print('CreateHolidayVariables 1')

  if(length(CachePath) > 0L) {
    if(Debug) {print('CreateHolidayVariables 1.1'); print(CachePath)}
    data <- RemixAutoML:::ReactiveLoadCSV(Infile = CachePath, Debug = Debug)
    if(Debug) print(data)
    if(Debug) print(class(data))
    if(Debug) print('CreateHolidayVariables 1.2')
    for(i in DateCols) if(class(data[[i]])[1L] %in% c('character')) {
      if(Debug) print('CreateHolidayVariables 1.3')
      data.table::set(data, j = eval(i), value = as.Date(data[[i]], format = "%m/%d/%y"))
    }
  }

  # Turn on full speed ahead----
  if(Debug) print('CreateHolidayVariables 2')

  # Convert to date or posix
  for(dat in DateCols) {
    if(Debug) print('CreateHolidayVariables 4')
    if(any(class(data[[dat]]) %chin% c("IDate"))) {
      if(Debug) print('CreateHolidayVariables 5.A')
      data[, eval(dat) := as.Date(get(dat))]
    } else if(any(class(data[[dat]]) %chin% c("IDateTime"))) {
      if(Debug) print('CreateHolidayVariables 5.B')
      data[, eval(dat) := as.POSIXct(get(dat))]
    }
  }

  if(Debug) print('CreateHolidayVariables 6')

  # Require namespace----
  requireNamespace("timeDate", quietly = TRUE)

  if(Debug) print('CreateHolidayVariables 7')

  # Function for expanding dates, vectorize----
  HolidayCountsInRange <- function(Start, End, Values) return(as.integer(length(which(x = Values %in% seq(as.Date(Start), as.Date(End), by = "days")))))

  if(Debug) print('CreateHolidayVariables 8')

  # Store individual holidays if HolidayGroups is specified----
  Holidays <- c()
  if(!is.null(HolidayGroups)) {

    if(Debug) print('CreateHolidayVariables 9')

    for(counter in seq_len(length(HolidayGroups))) {

      if(Debug) print(paste0('CreateHolidayVariables 10: iteration - ', counter))

      if(tolower(HolidayGroups[counter]) == "eastergroup") {
        Holidays <- c(Holidays,"Septuagesima","Quinquagesima","PalmSunday","GoodFriday","EasterSunday","Easter","EasterMonday","RogationSunday",
                      "Ascension","Pentecost","PentecostMonday","TrinitySunday","CorpusChristi","AshWednesday")
      }
      if(tolower(HolidayGroups[counter]) == "christmasgroup") {
        Holidays <- c(Holidays,"ChristTheKing","Advent1st","Advent1st","Advent3rd","Advent4th","ChristmasEve","ChristmasDay","BoxingDay","NewYearsDay")
      }
      if(tolower(HolidayGroups[counter]) == "otherecclesticalfeasts") {
        Holidays <- c(Holidays,"SolemnityOfMary","Epiphany","PresentationOfLord",
                      "Annunciation","TransfigurationOfLord","AssumptionOfMary",
                      "AssumptionOfMary","BirthOfVirginMary","CelebrationOfHolyCross",
                      "MassOfArchangels","AllSaints","AllSouls")
      }
      if(tolower(HolidayGroups[counter]) == "uspublicholidays") {
        Holidays <- c(Holidays,"USNewYearsDay","USInaugurationDay","USMLKingsBirthday","USLincolnsBirthday","USWashingtonsBirthday","USCPulaskisBirthday","USGoodFriday",
                      "USMemorialDay","USIndependenceDay","USLaborDay","USColumbusDay","USElectionDay","USVeteransDay","USThanksgivingDay","USChristmasDay")
      }
    }
  }

  # Turn DateCols into character names if not already ----
  if(Debug) print('CreateHolidayVariables 11')
  for(i in DateCols) if(!is.character(DateCols[i])) DateCols[i] <- names(data)[DateCols[i]]

  # Allocate data.table cols ----
  if(Debug) print('CreateHolidayVariables 12')
  data.table::alloc.col(DT = data, ncol(data) + 1L)

  # Create Temp Date Columns ----
  if(Debug) print('CreateHolidayVariables 13')
  MinDate <- data[, min(get(DateCols[1L]), na.rm = TRUE)]

  # Run holiday function to get unique dates ----
  if(Debug) print('CreateHolidayVariables 14')
  library(timeDate)

  # Define Holidays
  if(Debug) {
    print('CreateHolidayVariables 15')
    print(DateCols)
    print(head(data))
    print(class(data[[eval(DateCols)]]))
  }
  yrs <- unique(data.table::year(data[[eval(DateCols[1L])]]))
  if(length(DateCols) > 1L) {
    for(i in DateCols[-1L]) {
      yrs <- unique(c(yrs, data.table::year(data[[eval(DateCols[i])]])))
    }
  }
  HolidayVals <- sort(unique(as.Date(timeDate::holiday(year = yrs, Holiday = Holidays))))

  # Compute ----
  if(Debug) print('CreateHolidayVariables 16')
  for(i in seq_along(DateCols)) {

    if(Debug) print(paste0('CreateHolidayVariables 17: iteration - ', i))

    if(!is.null(LookbackDays)) {
      if(Debug) print(paste0('CreateHolidayVariables 18: iteration - ', i))
      x <- LookbackDays
    } else {
      if(Debug) print(paste0('CreateHolidayVariables 18: iteration - ', i))
      x <- 1
    }
    if(Debug) print('CreateHolidayVariables 19')
    LagCol <- paste0("Lag1_", DateCols[i])
    data[, eval(LagCol) := get(DateCols[i]) - lubridate::days(x)]
    if(Debug) print('CreateHolidayVariables 20')
    data.table::setkeyv(x = data, cols = c(DateCols[i], LagCol))
    if(Debug) print('CreateHolidayVariables 21')
    data.table::set(data, i = which(data[[eval(DateCols[i])]] == MinDate), j = eval(paste0("Lag1_",DateCols[i])), value = MinDate - x)
    if(Debug) print('CreateHolidayVariables 22')
    temp <- unique(data[, .SD, .SDcols = c(DateCols[i], LagCol)])
    if(Debug) print('CreateHolidayVariables 23')
    temp[, HolidayCounts := 0L]
    if(Debug) print('CreateHolidayVariables 24')
    NumRows <- seq_len(temp[,.N])
    if(Debug) print('CreateHolidayVariables 25')
    if(Print) {
      for(Rows in NumRows) {
        print(Rows)
        data.table::set(x = temp, i = Rows, j = "HolidayCounts", value = sum(HolidayCountsInRange(Start = temp[[eval(LagCol)]][[Rows]], End = temp[[eval(DateCols)]][[Rows]], Values = HolidayVals)))
      }
    } else {
      for(Rows in NumRows) {
        data.table::set(x = temp, i = Rows, j = "HolidayCounts", value = sum(HolidayCountsInRange(Start = temp[[eval(LagCol)]][[Rows]], End = temp[[eval(DateCols)]][[Rows]], Values = HolidayVals)))
      }
    }
    if(Debug) print('CreateHolidayVariables 26')
    data[temp, on = c(eval(DateCols[i]), eval(LagCol)), HolidayCounts := i.HolidayCounts]
    if(Debug) print('CreateHolidayVariables 27')
    if(length(DateCols) > 1L) data.table::setnames(data, "HolidayCounts", paste0(DateCols[i], "_HolidayCounts"))
    if(Debug) print('CreateHolidayVariables 28')
    data.table::set(data, j = eval(eval(LagCol)), value = NULL)
  }

  if(Debug) print('CreateHolidayVariables 29')

  # Return data ----
  if(length(CachePath) > 0L) {
    data.table::fwrite(x = data, file = CachePath)
    if(Debug) print('Returned here 1')
    return("Remix")
  } else {
    if(Debug) print('Returned here 2')
    return(data)
  }
}

#' @title CalendarVariables
#'
#' @description Create calendar variables
#'
#' @author Adrian Antico
#' @family Feature Engineering - Date Types
#'
#' @param data Source data
#' @param RunMode 'train' or 'score'
#' @param ArgsList ArgsList_FFE
#' @param SkipCols Vector of column names to remove from data
#'
#' @examples
#' \dontrun{
#' Output <- RemixAutoML:::CalendarVariables(
#'   data = data,
#'   RunMode = "train",
#'   ArgsList = ArgsList_FE,
#'   SkipCols = NULL)
#' data <- Output$data
#' ArgsList_FE <- Output$ArgsList
#' }
#'
#' @return A list containing the data and the ArgsList
#' @noRd
CalendarVariables <- function(data = NULL,
                              RunMode = "train",
                              ArgsList = NULL,
                              SkipCols = NULL) {

  # Metadata
  Start <- Sys.time()

  # Tempnames
  tempnames <- names(data.table::copy(data))

  # Run function
  if(tolower(RunMode) == "train") {
    for(dat in ArgsList$Data$DateVariables) {
      data <- RemixAutoML::CreateCalendarVariables(
        data = data,
        DateCols = dat,
        AsFactor = FALSE,
        TimeUnits = ArgsList$FE_Args$Calendar$CalendarVariables)
    }

    # SkipCols
    if(!is.null(SkipCols)) {
      temp <- SkipCols[!SkipCols %chin% names(data)]
      data[, (temp) := NULL]
    }

    # Args Collection
    if(!is.null(ArgsList)) {

      # ArgsList
      ArgsList$CalendarVariables$DateCols <- ArgsList$Data$DateVariables
      ArgsList$CalendarVariables$TimeUnits <- ArgsList$FE_Args$Calendar$CalendarVariables
      ArgsList$CalendarVariables$AsFactor <- FALSE

      # Column tracking
      ArgsList$FE_Columns$CalendarVariables_Training <- setdiff(names(data), tempnames)

      # Run time tracking
      End <- Sys.time()
      ArgsList$RunTime$CalendarVariables_Training <- difftime(End, Start, units = "mins")
    }

  } else {

    # Run function
    for(dat in DateVariables) {
      data <- RemixAutoML::CreateCalendarVariables(
        data = data,
        DateCols = ArgsList$CalendarVariables$DateCols,
        AsFactor = ArgsList$CalendarVariables$AsFactor,
        TimeUnits = ArgsList$CalendarVariables$TimeUnits)
    }

    # SkipCols
    if(!is.null(SkipCols)) {
      temp <- SkipCols[!SkipCols %chin% names(data)]
      data[, (temp) := NULL]
    }

    # Args Collection
    if(!is.null(ArgsList)) {
      End <- Sys.time()
      ArgsList$RunTime$CalendarVariables_Scoring <- difftime(End, Start, units = "mins")
    }
  }

  # Return
  return(list(data = data, ArgsList = ArgsList))
}

#' @title HolidayVariables
#'
#' @description Create holiday variables
#'
#' @author Adrian Antico
#' @family Feature Engineering - Date Types
#'
#' @param data Source data
#' @param RunMode 'train' or 'score'
#' @param ArgsList ArgsList_FFE
#' @param SkipCols Vector of column names to remove from data
#'
#' @examples
#' \dontrun{
#' Output <- RemixAutoML:::HolidayVariables(
#'   data = data,
#'   RunMode = "train",
#'   ArgsList = ArgsList,
#'   SkipCols = NULL)
#' data <- Output$data
#' ArgsList_FE <- Output$ArgsList
#' }
#'
#' @return A list containing the data and the ArgsList
#' @noRd
HolidayVariables <- function(data = NULL,
                             RunMode = "train",
                             ArgsList = ArgsList,
                             SkipCols = NULL) {

  # Metadata
  Start <- Sys.time()

  # Run function
  if(tolower(RunMode) == "train") {
    tempnames <- names(data.table::copy(data))
    for(dat in ArgsList$Data$DateVariables) {
      for(i in seq_along(ArgsList$FE_Args$Holiday_Variables$HolidayVariables)) {
        data <- RemixAutoML::CreateHolidayVariables(
          data = data,
          DateCols = dat,
          LookbackDays = ArgsList$FE_Args$Holiday_Variables$LookBackDays,
          HolidayGroups = ArgsList$FE_Args$Holiday_Variables$HolidayVariables[i],
          Holidays = NULL,
          Print = FALSE)
        data.table::setnames(data, "HolidayCounts", paste0(dat, "_", ArgsList$FE_Args$Holiday$HolidayVariables[i], "_HolidayCounts"))
      }
    }

    # SkipCols
    if(!is.null(SkipCols)) {
      temp <- SkipCols[!SkipCols %chin% names(data)]
      data[, (temp) := NULL]
    }

    # ArgsList
    ArgsList$HolidayVariables$DateCols <- ArgsList$Data$DateVariables
    ArgsList$HolidayVariables$LookbackDays <- ArgsList$FE_Args$Holiday_Variables$LookBackDays
    ArgsList$HolidayVariables$AsFactor <- FALSE
    ArgsList$HolidayVariables$HolidaySets <- ArgsList$FE_Args$Holiday_Variables$HolidayVariables

    # Column tracking
    ArgsList$FE_Columns$HolidayVariables_Training <- setdiff(names(data), tempnames)

    # Run time tracking
    End <- Sys.time()
    ArgsList$RunTime$HolidayVariables_Training <- difftime(End, Start, units = "mins")

  } else {

    # Run function
    for(dat in ArgsList$FE_HolidayVariables$DateCols) {
      for(i in seq_along(ArgsList$FE_HolidayVariables$HolidaySets)) {
        data <- RemixAutoML::CreateHolidayVariables(
          data = data,
          DateCols = ArgsList$HolidayVariables$DateCols,
          LookbackDays = ArgsList$HolidayVariables$LookbackDays,
          HolidayGroups = ArgsList$HolidayVariables$HolidaySets[i],
          Holidays = NULL,
          Print = FALSE)
        data.table::setnames(data, "HolidayCounts", paste0(dat, "_", ArgsList$HolidayVariables$HolidaySets[i], "_HolidayCounts"))
      }
    }

    # SkipCols
    if(!is.null(SkipCols)) {
      temp <- SkipCols[!SkipCols %chin% names(data)]
      data[, (temp) := NULL]
    }

    # Run time tracking
    End <- Sys.time()
    ArgsList$RunTime$HolidayVariables_Scoring <- difftime(End, Start, units = "mins")
  }

  # Return
  return(list(data = data, ArgsList = ArgsList))
}
