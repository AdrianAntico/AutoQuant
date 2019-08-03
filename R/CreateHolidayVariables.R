#' CreateHolidayVariables Create Holiday Count Columns
#'
#' CreateHolidayVariables Rapidly creates holiday count variables based on the date columns you provide
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @param data This is your data
#' @param DateCols Supply either column names or column numbers of your date columns you want to use for creating calendar variables
#' @param AsFactor Set to TRUE if you want factor type columns returned; otherwise integer type columns will be returned
#' @param TimeUnit Supply a character value for the time unit your date columns are in, such as 'day'
#' @import timeDate
#' @examples
#' data <- data.table::data.table(Date = "2018-01-01 00:00:00")
#' data <- CreateHolidayVariables(data,
#'                                DateCols = "DateTime",
#'                                TimeUnit = "day",
#'                                HolidayGroups = c("USPublicHolidays","EasterGroup",
#'                                                  "ChristmasGroup","OtherEcclesticalFeasts"),
#'                                Holidays = NULL)
#' @return Returns your data.table with the added holiday indicator variable
#' @export
CreateHolidayVariables <- function(data,
                                   DateCols = "DateTime",
                                   TimeUnit = "day",
                                   HolidayGroups = c("USPublicHolidays","EasterGroup",
                                                     "ChristmasGroup",
                                                     "OtherEcclesticalFeasts"),
                                   Holidays = NULL) {
  
  # Require namespace----
  requireNamespace("timeDate", quietly = TRUE)
  
  # Function for exapanding dates, vectorize----
  HolidayCountsInRange <- function(Start, End, Values) {
    DateRange <- seq(as.Date(Start), 
                     as.Date(End), 
                     by = "days")
    return(as.integer(length(which(x = Values %in% DateRange))))
  }
  
  
  # Convert to data.table----
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Check args----
  if (!(any(
    tolower(TimeUnit) %chin% c(
      "second",
      "minute",
      "hour",
      "day",
      "week",
      "month",
      "quarter",
      "year"
    )
  ))) {
    warning(
      "TimeUnit needs to be one of 'minute', 'hour', 'wday',
            'mday', 'yday','week', 'month', 'quarter', 'year'"
    )
  }
  
  # Store individual holidays if HolidayGroups is specified----
  if(!is.null(HolidayGroups)) {
    for(counter in seq_len(length(HolidayGroups))) {
      if(tolower(HolidayGroups[counter]) == "eastergroup") {
        if(counter == 1) {
          Holidays <- c("Septuagesima", "Quinquagesima","AshWednesday", 
                        "PalmSunday","GoodFriday","EasterSunday",
                        "Easter","EasterMonday","RogationSunday",
                        "Ascension","Pentecost","PentecostMonday",
                        "TrinitySunday","CorpusChristi")          
        } else {
          Holidays <- c(Holidays,"Septuagesima", "Quinquagesima", 
                        "PalmSunday","GoodFriday","EasterSunday",
                        "Easter","EasterMonday","RogationSunday",
                        "Ascension","Pentecost","PentecostMonday",
                        "TrinitySunday","CorpusChristi","AshWednesday")
        }
      }
      if(tolower(HolidayGroups[counter]) == "christmasgroup") {
        if(counter == 1) {
          Holidays <- c("ChristTheKing","Advent1st","Advent1st",
                        "Advent3rd","Advent4th","ChristmasEve",
                        "ChristmasDay","BoxingDay","NewYearsDay")
        } else {
          Holidays <- c(Holidays,"ChristTheKing","Advent1st","Advent1st",
                        "Advent3rd","Advent4th","ChristmasEve",
                        "ChristmasDay","BoxingDay","NewYearsDay")
        }
      }
      if(tolower(HolidayGroups[counter]) == "otherecclesticalfeasts") {
        if(counter == 1) {
          Holidays <- c("SolemnityOfMary","Epiphany","PresentationOfLord",
                        "Annunciation","TransfigurationOfLord","AssumptionOfMary",
                        "AssumptionOfMary","BirthOfVirginMary","CelebrationOfHolyCross",
                        "MassOfArchangels","AllSaints","AllSouls")
        } else {
          Holidays <- c(Holidays,"SolemnityOfMary","Epiphany","PresentationOfLord",
                        "Annunciation","TransfigurationOfLord","AssumptionOfMary",
                        "AssumptionOfMary","BirthOfVirginMary","CelebrationOfHolyCross",
                        "MassOfArchangels","AllSaints","AllSouls")
        }
      }
      if(tolower(HolidayGroups[counter]) == "uspublicholidays") {
        if(counter == 1) {
          Holidays <- c("USNewYearsDay","USInaugurationDay","USGoodFriday",
                        "USMLKingsBirthday","USLincolnsBirthday","USWashingtonsBirthday",
                        "USMemorialDay","USIndependenceDay","USLaborDay",
                        "USColumbusDay","USElectionDay","USVeteransDay",
                        "USThanksgivingDay","USChristmasDay","USCPulaskisBirthday")
        } else {
          Holidays <- c(Holidays,"USNewYearsDay","USInaugurationDay",
                        "USMLKingsBirthday","USLincolnsBirthday","USWashingtonsBirthday",
                        "USMemorialDay","USIndependenceDay","USLaborDay",
                        "USColumbusDay","USElectionDay","USVeteransDay",
                        "USThanksgivingDay","USChristmasDay","USCPulaskisBirthday","USGoodFriday")
        }
      }
    }
  }
  
  # Run holiday function to get unique dates----
  library(timeDate)
  Holidays <- unique(as.Date(timeDate::holiday(
    year = unique(lubridate::year(data[[eval(DateCols)]])), 
    Holiday = Holidays)))
  
  # Turn DateCols into character names if not already----
  for (i in DateCols) {
    if (!is.character(DateCols[i])) {
      DateCols[i] <- names(data)[DateCols[i]]
    }
  }
  
  # Allocate data.table cols----
  data.table::alloc.col(DT = data, ncol(data) + 1L)
  
  # Create Temp Date Columns----
  MinDate <- data[, min(get(DateCols[1]))]
  for (i in seq_len(length(DateCols))) {
    data.table::setorderv(x = data, cols = eval(DateCols[i]), order = 1, na.last = TRUE)
    data.table::set(data,
                    j = paste0("Lag1_", eval(DateCols[i])),
                    value = data.table::shift(x = data[[eval(DateCols[i])]],
                                              n = 1L, fill = MinDate, type = "lag"))
  }
  
  # Enforce the missing lagged date to equal the regular date minus a constant----
  x <- data[2, get(DateCols[1])] - data[2, get(paste0("Lag1_",eval(DateCols[1])))]
  
  # Build Features----
  for (i in seq_len(length(DateCols))) {
    EndDateVector <- data[[eval(DateCols[i])]]
    StartDateVector <- data[[paste0("Lag1_", eval(DateCols[i]))]]
    data.table::set(data, 
                    i = which(data[[eval(DateCols[i])]] == MinDate),
                    j = eval(paste0("Lag1_",DateCols[i])),
                    value = MinDate - x)
    for(j in as.integer(seq_len(data[,.N]))) {
      data.table::set(x = data,
                      i = j,
                      j = "HolidayCounts", 
                      value = HolidayCountsInRange(
                        Start = StartDateVector[j],
                        End = EndDateVector[j],
                        Values = Holidays))
      
      # Remove Lag1date----
      data.table::set(data, 
                      j = eval(paste0("Lag1_",DateCols[i])), 
                      value = NULL)
    }
  }
  return(data)
}
