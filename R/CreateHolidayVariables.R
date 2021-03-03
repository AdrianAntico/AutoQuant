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
