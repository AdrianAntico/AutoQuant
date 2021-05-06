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
                                    TimeUnits = "wday") {

  # Turn on full speed ahead----
  data.table::setDTthreads(threads = max(1L, parallel::detectCores() - 2L))
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  # Check args----
  if(!is.logical(AsFactor)) {
    print("AsFactor needs to be TRUE or FALSE")
    return(data)
  }
  if(!(any(tolower(TimeUnits) %chin% c("second","minute","hour","wday","mday","yday","week","isoweek","wom","month","quarter","year")))) {
    print("TimeUnits needs to be one of 'second', 'minute', 'hour', 'wday','mday', 'yday','week','wom','month', 'quarter', 'year'")
    return(data)
  }

  # Turn DateCols into character names if not already----
  for(i in DateCols) if(!is.character(DateCols[i])) DateCols[i] <- names(data)[DateCols[i]]

  # Revise TimeUnits Based on Data----
  x <- 0L
  TimeList <- list()
  Cols <- c()
  for(i in seq_len(length(DateCols))) {
    if(any(TimeUnits %chin% c("second", "minute", "hour"))) {
      if(min(data.table::as.ITime(data[[eval(DateCols[i])]])) - max(data.table::as.ITime(data[[eval(DateCols[i])]])) == 0L) {
        TimeList[[i]] <- TimeUnits[!(tolower(TimeUnits) %chin% c("second", "minute", "hour"))]
        Cols[i] <- length(TimeList[[i]])
      } else {
        TimeList[[i]] <- TimeUnits
        Cols[i] <- length(TimeList[[i]])
      }
    } else {
      TimeList[[i]] <- TimeUnits
      Cols[i] <- length(TimeList[[i]])
    }
  }

  # Number of supplied columns----
  NumCols <- ncol(data.table::copy(data))

  # Allocate data.table cols----
  data.table::alloc.col(DT = data, ncol(data) + sum(Cols))

  # Create DateCols to data.table IDateTime types----
  for(i in seq_len(length(DateCols))) {
    if(length(TimeList) != 0L) {
      if(any(tolower(TimeList[[i]]) %chin% c("second", "minute", "hour"))) data.table::set(data, j = paste0("TIME_", eval(DateCols[i])), value = data.table::as.ITime(data[[eval(DateCols[i])]]))
      if(any(tolower(TimeList[[i]]) %chin% c("wday","mday","yday","week","isoweek","wom","month","quarter","year"))) data.table::set(data, j = paste0("DATE_", eval(DateCols[i])), value = data.table::as.IDate(data[[eval(DateCols[i])]]))
    }
  }

  # Build Features----
  for(i in seq_len(length(DateCols))) {

    # Define DateCols----
    DateColRef <- DateCols[i]

    # Get unique date values in table and then merge back to source data at the end one time----
    if(any(tolower(TimeList[[i]]) %chin% c("second", "minute", "hour"))) {
      DataCompute <- unique(data[, .SD, .SDcols = c(paste0("TIME_", DateColRef), paste0("DATE_", DateColRef))])
    } else {
      DataCompute <- unique(data[, .SD, .SDcols = c(paste0("DATE_", DateColRef))])
    }

    # Build calendar variables----
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
          data.table::set(DataCompute, j = paste0(DateColRef, "_", TimeList[[i]][j]), value = as.factor(data.table::wday(DataCompute[[eval(paste0("DATE_", DateColRef))]])))
        } else {
          data.table::set(DataCompute, j = paste0(DateColRef, "_", j), value = data.table::wday(DataCompute[[eval(paste0("DATE_", DateColRef))]]))
        }
      } else if(tolower(j) == "mday") {
        if (AsFactor) {
          data.table::set(DataCompute, j = paste0(DateColRef, "_", TimeList[[i]][j]), value = as.factor(data.table::mday(DataCompute[[eval(paste0("DATE_", DateColRef))]])))
        } else {
          data.table::set(DataCompute, j = paste0(DateColRef, "_", j), value = as.integer(data.table::mday(DataCompute[[eval(paste0("DATE_", DateColRef))]])))
        }
      } else if(tolower(j) == "yday") {
        if (AsFactor) {
          data.table::set(DataCompute, j = paste0(DateColRef, "_", TimeList[[i]][j]), value = as.factor(data.table::yday(DataCompute[[eval(paste0("DATE_", DateColRef))]])))
        } else {
          data.table::set(DataCompute, j = paste0(DateColRef, "_", j), value = data.table::yday(DataCompute[[eval(paste0("DATE_", DateColRef))]]))
        }
      } else if(tolower(j) == "week") {
        if (AsFactor) {
          data.table::set(DataCompute, j = paste0(DateColRef, "_", TimeList[[i]][j]), value = as.factor(data.table::week(DataCompute[[eval(paste0("DATE_", DateColRef))]])))
        } else {
          data.table::set(DataCompute, j = paste0(DateColRef, "_", j), value = data.table::week(DataCompute[[eval(paste0("DATE_", DateColRef))]]))
        }
      } else if(tolower(j) == "isoweek") {
        if (AsFactor) {
          data.table::set(DataCompute, j = paste0(DateColRef, "_", TimeList[[i]][j]), value = as.factor(data.table::isoweek(DataCompute[[eval(paste0("DATE_", DateColRef))]])))
        } else {
          data.table::set(DataCompute, j = paste0(DateColRef, "_", j), value = data.table::isoweek(DataCompute[[eval(paste0("DATE_", DateColRef))]]))
        }
      } else if(tolower(j) == "month") {
        if (AsFactor) {
          data.table::set(DataCompute, j = paste0(DateColRef, "_", TimeList[[i]][j]), value = as.factor(data.table::month(DataCompute[[eval(paste0("DATE_", DateColRef))]])))
        } else {
          data.table::set(DataCompute, j = paste0(DateColRef, "_", j), value = data.table::month(DataCompute[[eval(paste0("DATE_", DateColRef))]]))
        }
      } else if(tolower(j) == "wom") {
        if (AsFactor) {
          data.table::set(DataCompute, j = paste0(DateColRef, "_", TimeList[[i]][j]), value = as.factor(data.table::fifelse(ceiling(data.table::mday(DataCompute[[eval(paste0("DATE_", DateColRef))]])/7) == 5, 4, ceiling(data.table::mday(DataCompute[[eval(paste0("DATE_", DateColRef))]])/7))))
        } else {
          data.table::set(DataCompute, j = paste0(DateColRef, "_", j), value = data.table::fifelse(ceiling(data.table::mday(DataCompute[[eval(paste0("DATE_", DateColRef))]])/7) == 5, 4, ceiling(data.table::mday(DataCompute[[eval(paste0("DATE_", DateColRef))]])/7)))
        }
      } else if(tolower(j) == "quarter") {
        if (AsFactor) {
          data.table::set(DataCompute, j = paste0(DateColRef, "_", TimeList[[i]][j]), value = as.factor(data.table::quarter(DataCompute[[eval(paste0("DATE_", DateColRef))]])))
        } else {
          data.table::set(DataCompute, j = paste0(DateColRef, "_", j), value = as.integer(data.table::quarter(DataCompute[[eval(paste0("DATE_", DateColRef))]])))
        }
      } else if(tolower(j) == "year") {
        if(AsFactor) {
          data.table::set(DataCompute, j = paste0(DateColRef, "_", TimeList[[i]][j]), value = as.factor(data.table::year(DataCompute[[eval(paste0("DATE_", DateColRef))]])))
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

  # Return data----
  return(data)
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
#'   data <- CreateHolidayVariables(
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
                                   HolidayGroups = c("USPublicHolidays","EasterGroup","ChristmasGroup","OtherEcclesticalFeasts"),
                                   Holidays = NULL,
                                   Print = FALSE) {

  # Turn on full speed ahead----
  data.table::setDTthreads(threads = max(1L, parallel::detectCores()-2L))

  # Convert to data.table----
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  # Convert to date or posix
  for(dat in DateCols) {
    if(any(class(data[[dat]]) %chin% c("IDate"))) {
      data[, eval(dat) := as.Date(get(dat))]
    } else if(any(class(data[[dat]]) %chin% c("IDateTime"))) {
      data[, eval(dat) := as.POSIXct(get(dat))]
    }
  }

  # Require namespace----
  requireNamespace("timeDate", quietly = TRUE)

  # Function for expanding dates, vectorize----
  HolidayCountsInRange <- function(Start, End, Values) return(as.integer(length(which(x = Values %in% seq(as.Date(Start), as.Date(End), by = "days")))))

  # Store individual holidays if HolidayGroups is specified----
  Holidays <- c()
  if(!is.null(HolidayGroups)) {
    for(counter in seq_len(length(HolidayGroups))) {
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

  # Turn DateCols into character names if not already----
  for(i in DateCols) if(!is.character(DateCols[i])) DateCols[i] <- names(data)[DateCols[i]]

  # Allocate data.table cols----
  data.table::alloc.col(DT = data, ncol(data) + 1L)

  # Create Temp Date Columns----
  MinDate <- data[, min(get(DateCols[1L]))]

  # Run holiday function to get unique dates----
  library(timeDate)

  # Define Holidays
  HolidayVals <- sort(unique(as.Date(timeDate::holiday(year = unique(lubridate::year(data[[eval(DateCols)]])), Holiday = Holidays))))

  # Compute----
  for(i in seq_along(DateCols)) {
    if(!is.null(LookbackDays)) {
      x <- LookbackDays
    } else {
      x <- data[, quantile(x = (data[[eval(DateCols[i])]] - data[[(paste0("Lag1_",eval(DateCols[i])))]]), probs = 0.99)]
    }
    data[, eval(paste0("Lag1_", DateCols[i])) := get(DateCols[i]) - lubridate::days(x)]
    data.table::setkeyv(x = data, cols = c(DateCols[i], paste0("Lag1_", eval(DateCols[i]))))
    data.table::set(data, i = which(data[[eval(DateCols[i])]] == MinDate), j = eval(paste0("Lag1_",DateCols[i])), value = MinDate - x)
    temp <- unique(data[, .SD, .SDcols = c(DateCols[i], paste0("Lag1_", eval(DateCols[i])))])
    temp[, HolidayCounts := 0L]
    NumRows <- seq_len(temp[,.N])
    if(Print) {
      for(Rows in NumRows) {
        print(Rows)
        data.table::set(x = temp, i = Rows, j = "HolidayCounts", value = sum(HolidayCountsInRange(Start = temp[[paste0("Lag1_", DateCols[i])]][[Rows]], End = temp[[eval(DateCols)]][[Rows]], Values = HolidayVals)))
      }
    } else {
      for(Rows in NumRows) {
        data.table::set(x = temp, i = Rows, j = "HolidayCounts", value = sum(HolidayCountsInRange(Start = temp[[paste0("Lag1_", DateCols[i])]][[Rows]], End = temp[[eval(DateCols)]][[Rows]], Values = HolidayVals)))
      }
    }
    data[temp, on = c(eval(DateCols[i]), paste0("Lag1_", DateCols[i])), HolidayCounts := i.HolidayCounts]
    if(length(DateCols) > 1L) data.table::setnames(data, "HolidayCounts", paste0(DateCols[i], "_HolidayCounts"))
    data.table::set(data, j = eval(paste0("Lag1_", DateCols[i])), value = NULL)
  }

  # Return data----
  return(data)
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
#' @param DateVariables Only required if you don't pass in ArgsList
#' @param Vars Calendar variables to create
#'
#' @examples
#' \dontrun{
#' Output <- RemixAutoML:::CalendarVariables(
#'   data = data,
#'   RunMode = "train",
#'   ArgsList = ArgsList_FE,
#'   SkipCols = NULL,
#'   Vars = c("week", "wom", "month", "quarter"))
#' data <- Output$data
#' ArgsList_FE <- Output$ArgsList
#' }
#'
#' @return A list containing the data and the ArgsList
#' @noRd
CalendarVariables <- function(data = NULL,
                              RunMode = "train",
                              ArgsList = NULL,
                              SkipCols = NULL,
                              DateVariables = NULL,
                              Vars = c("wday", "mday", "yday", "week", "wom", "month", "quarter")) {

  # Date variables
  if(!is.null(ArgsList)) DateVariables <- ArgsList$Data$DateVariables

  # Metadata
  Start <- Sys.time()

  # Tempnames
  tempnames <- names(data.table::copy(data))

  # Run function
  if(tolower(RunMode) == "train") {
    for(dat in DateVariables) {
      data <- RemixAutoML::CreateCalendarVariables(
        data = data,
        DateCols = dat,
        AsFactor = FALSE,
        TimeUnits = Vars)
    }

    # SkipCols
    if(!is.null(SkipCols)) {
      temp <- SkipCols[!SkipCols %chin% names(data)]
      data[, (temp) := NULL]
    }

    # Args Collection
    if(!is.null(ArgsList)) {

      # ArgsList
      ArgsList$FE_CalendarVariables$DateCols <- ArgsList$Data$DateVariables
      ArgsList$FE_CalendarVariables$TimeUnits <- Vars
      ArgsList$FE_CalendarVariables$AsFactor <- FALSE

      # Column tracking
      ArgsList$FE_Columns$FE_CalendarVariables_Training <- setdiff(names(data), tempnames)

      # Run time tracking
      End <- Sys.time()
      ArgsList$RunTime$FE_CalendarVariables_Training <- difftime(End, Start, units = "mins")
    }

  } else {

    # Run function
    for(dat in DateVariables) {
      data <- RemixAutoML::CreateCalendarVariables(
        data = data,
        DateCols = ArgsList$FE_CalendarVariables$DateCols,
        AsFactor = ArgsList$FE_CalendarVariables$AsFactor,
        TimeUnits = ArgsList$FE_CalendarVariables$TimeUnits)
    }

    # SkipCols
    if(!is.null(SkipCols)) {
      temp <- SkipCols[!SkipCols %chin% names(data)]
      data[, (temp) := NULL]
    }

    # Args Collection
    if(!is.null(ArgsList)) {
      End <- Sys.time()
      ArgsList$RunTime$FE_CalendarVariables_Scoring <- difftime(End, Start, units = "mins")
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
#' @param Vars Calendar variables to create
#'
#' @examples
#' \dontrun{
#' Output <- RemixAutoML:::HolidayVariables(
#'   data = data,
#'   RunMode = "train",
#'   ArgsList = ArgsList_FE,
#'   LookbackDays = 7,
#'   HolidaySets = c("USPublicHolidays","EasterGroup","ChristmasGroup","OtherEcclesticalFeasts"),
#'   SkipCols = NULL)
#' data <- Output$data
#' ArgsList_FE <- Output$ArgsList
#' }
#'
#' @return A list containing the data and the ArgsList
#' @noRd
HolidayVariables <- function(data = NULL,
                             RunMode = "train",
                             ArgsList = ArgsList_FFE,
                             LookbackDays = 7,
                             HolidaySets = c("USPublicHolidays","EasterGroup","ChristmasGroup","OtherEcclesticalFeasts"),
                             SkipCols = NULL) {

  # Metadata
  Start <- Sys.time()

  # Run function
  if(tolower(RunMode) == "train") {
    tempnames <- names(data.table::copy(data))
    for(dat in ArgsList$Data$DateVariables) {
      for(i in seq_along(HolidaySets)) {
        data <- RemixAutoML::CreateHolidayVariables(
          data = data,
          DateCols = dat,
          LookbackDays = 7,
          HolidayGroups = HolidaySets[i],
          Holidays = NULL,
          Print = FALSE)
        data.table::setnames(data, "HolidayCounts", paste0(dat, "_", HolidaySets[i], "_HolidayCounts"))
      }
    }

    # SkipCols
    if(!is.null(SkipCols)) {
      temp <- SkipCols[!SkipCols %chin% names(data)]
      data[, (temp) := NULL]
    }

    # ArgsList
    ArgsList$FE_HolidayVariables$DateCols <- ArgsList$Data$DateVariables
    ArgsList$FE_HolidayVariables$LookbackDays <- LookbackDays
    ArgsList$FE_HolidayVariables$AsFactor <- FALSE
    ArgsList$FE_HolidayVariables$HolidaySets <- HolidaySets

    # Column tracking
    ArgsList$FE_Columns$FE_HolidayVariables_Training <- setdiff(names(data), tempnames)

    # Run time tracking
    End <- Sys.time()
    ArgsList$RunTime$FE_HolidayVariables_Training <- difftime(End, Start, units = "mins")

  } else {

    # Run function
    for(dat in ArgsList$FE_HolidayVariables$DateCols) {
      for(i in seq_along(ArgsList$FE_HolidayVariables$HolidaySets)) {
        data <- RemixAutoML::CreateHolidayVariables(
          data = data,
          DateCols = ArgsList$FE_HolidayVariables$DateCols,
          LookbackDays = ArgsList$FE_HolidayVariables$LookbackDays,
          HolidayGroups = ArgsList$FE_HolidayVariables$HolidaySets[i],
          Holidays = NULL,
          Print = FALSE)
        data.table::setnames(data, "HolidayCounts", paste0(dat, "_", ArgsList$FE_HolidayVariables$HolidaySets[i], "_HolidayCounts"))
      }
    }

    # SkipCols
    if(!is.null(SkipCols)) {
      temp <- SkipCols[!SkipCols %chin% names(data)]
      data[, (temp) := NULL]
    }

    # Run time tracking
    End <- Sys.time()
    ArgsList$RunTime$FE_HolidayVariables_Scoring <- difftime(End, Start, units = "mins")
  }

  # Return
  return(list(data = data, ArgsList = ArgsList))
}
