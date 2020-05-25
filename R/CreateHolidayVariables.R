#' CreateHolidayVariables Create Holiday Count Columns
#'
#' CreateHolidayVariables Rapidly creates holiday count variables based on the date columns you provide
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @param data This is your data
#' @param DateCols Supply either column names or column numbers of your date columns you want to use for creating calendar variables
#' @param HolidayGroups Pick groups
#' @param Holidays Pick holidays
#' @param GroupingVars Grouping variable names
#' @import timeDate
#' @examples
#' \donttest{
#' data <- data.table::data.table(Date = as.Date(rep('2018-01-01 00:00:00',100)))
#' data1 <- data.table::data.table(Date = as.Date(rep('2018-01-01 00:00:00',100)))
#' data[, ID := 1L:.N][, Date := Date + lubridate::days(ID)][, ID := NULL]
#' data1[, ID := 1L:.N][, Date := Date + lubridate::days(ID)][, ID := NULL]
#' data[, GroupVar := "A"]
#' data1[, GroupVar := "B"]
#' data <- data.table::rbindlist(list(data,data1))
#' data <- CreateHolidayVariables(
#'    data,
#'    DateCols = "Date",
#'    HolidayGroups = c("USPublicHolidays","EasterGroup","ChristmasGroup","OtherEcclesticalFeasts"),
#'    Holidays = NULL,
#'    GroupingVars = "GroupVar")
#' }
#' @return Returns your data.table with the added holiday indicator variable
#' @export
CreateHolidayVariables <- function(data,
                                   DateCols = NULL,
                                   HolidayGroups = c("USPublicHolidays","EasterGroup","ChristmasGroup","OtherEcclesticalFeasts"),
                                   Holidays = NULL,
                                   GroupingVars = NULL) {
  
  # Turn on full speed ahead----
  data.table::setDTthreads(threads = max(1L, parallel::detectCores()-2L))
  
  # If GroupVars are numeric, convert them to character
  for(zz in seq_along(GroupingVars)) {
    if(is.numeric(data[[eval(GroupingVars[zz])]]) | is.integer(data[[eval(GroupingVars[zz])]])) {
      data.table::set(data, j = GroupingVars[zz], value = as.character(data[[eval(GroupingVars[zz])]]))  
    }
  }
  
  # Require namespace----
  requireNamespace("timeDate", quietly = TRUE)
  
  # Function for expanding dates, vectorize----
  HolidayCountsInRange <- function(Start, End, Values) {
    DateRange <- seq(as.Date(Start), as.Date(End), by = "days")
    return(as.integer(length(which(x = Values %in% DateRange))))
  }
  
  # Convert to data.table----
  if(!data.table::is.data.table(data)) data <- data.table::as.data.table(data)
  
  # Sort by group and date----
  if(!is.null(GroupingVars)) {
    if(!any(class(data[[eval(DateCols)]]) %chin% c("POSIXct","POSIXt","Date"))) data[, eval(DateCols) := as.POSIXct(data[[eval(DateCols)]])]
    data <- data[order(get(GroupingVars),get(DateCols))]
  }
  
  # Store individual holidays if HolidayGroups is specified----
  Holidays <- NULL
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
  
  # Run holiday function to get unique dates----
  library(timeDate)
  Holidays <- unique(as.Date(timeDate::holiday(year = unique(lubridate::year(data[[eval(DateCols)]])), Holiday = Holidays)))
  
  # Turn DateCols into character names if not already----
  for(i in DateCols) if(!is.character(DateCols[i])) DateCols[i] <- names(data)[DateCols[i]]
  
  # Allocate data.table cols----
  data.table::alloc.col(DT = data, ncol(data) + 1L)
  
  # Create Temp Date Columns----
  MinDate <- data[, min(get(DateCols[1L]))]
  if(!is.null(GroupingVars)) {
    for(i in seq_len(length(DateCols))) {
      data.table::setorderv(x = data, cols = c(eval(GroupingVars), eval(DateCols[i])), order = 1L, na.last = TRUE)
      data[, paste0("Lag1_", eval(DateCols[i])) := data.table::shift(x = get(DateCols[i]), n = 1L, fill = MinDate, type = "lag"),  by = c(eval(GroupingVars))]
    }  
  } else {
    for(i in seq_len(length(DateCols))) {
      data.table::setorderv(x = data, cols = eval(DateCols[i]), order = 1L, na.last = TRUE)
      data.table::set(data, j = paste0("Lag1_", eval(DateCols[i])), value = data.table::shift(x = data[[eval(DateCols[i])]], n = 1L, fill = MinDate, type = "lag"))
    }
  }
  
  # Enforce the missing lagged date to equal the regular date minus a constant----
  x <- data[, quantile(x = (data[[eval(DateCols[1])]] - data[[(paste0("Lag1_",eval(DateCols[1])))]]), probs = 0.99)]
  data[, eval(paste0("Lag1_",DateCols[i])) := get(DateCols[i]) - x]
  
  # Compute----
  for(i in seq_len(length(DateCols))) {
    data.table::setkeyv(x = data, cols = c(eval(GroupingVars), DateCols[i], paste0("Lag1_", eval(DateCols[i]))))
    data.table::set(data, i = which(data[[eval(DateCols[i])]] == MinDate), j = eval(paste0("Lag1_",DateCols[i])), value = MinDate - x)
    temp <- unique(data[, .SD, .SDcols = c(DateCols[i], paste0("Lag1_", eval(DateCols[i])))])
    temp[, HolidayCounts := 0L]
    for(i in seq_len(temp[,.N])) data.table::set(x = temp, i = i, j = "HolidayCounts", value = HolidayCountsInRange(Start = temp[[paste0("Lag1_", DateCols[1L])]][[i]], End = temp[i, get(DateCols)], Values = Holidays))
    data[temp, on = c(eval(DateCols[1L]), paste0("Lag1_", DateCols[1L])), HolidayCounts := i.HolidayCounts]
  }

  # Return data----
  return(data)
}
