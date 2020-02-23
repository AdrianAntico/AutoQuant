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
#' data <- data.table::data.table(Date = '2018-01-01 00:00:00')
#' data <- CreateHolidayVariables(data,
#'                                DateCols = "DateTime",
#'                                HolidayGroups = c("USPublicHolidays","EasterGroup",
#'                                                  "ChristmasGroup","OtherEcclesticalFeasts"),
#'                                Holidays = NULL,
#'                                GroupingVars = NULL)
#' }
#' @return Returns your data.table with the added holiday indicator variable
#' @export
CreateHolidayVariables <- function(data,
                                   DateCols = "DateTime",
                                   HolidayGroups = c("USPublicHolidays","EasterGroup",
                                                     "ChristmasGroup",
                                                     "OtherEcclesticalFeasts"),
                                   Holidays = NULL,
                                   GroupingVars = NULL) {
  
  # Turn on full speed ahead----
  data.table::setDTthreads(percent = 100)
  
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
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Sort by group and date----
  if(!is.null(GroupingVars)) {
    if(!any(class(data[[eval(DateCols)]]) %chin% c("POSIXct","POSIXt","Date"))) {
      data[, eval(DateCols) := as.POSIXct(data[[eval(DateCols)]])]
    }
    data <- data[order(get(GroupingVars),get(DateCols))]
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
  if(!is.null(GroupingVars)) {
    for (i in seq_len(length(DateCols))) {
      data.table::setorderv(x = data, cols = c(eval(GroupingVars), eval(DateCols[i])), order = 1, na.last = TRUE)
      data[, paste0("Lag1_", eval(DateCols[i])) := data.table::shift(
        x = get(DateCols[i]), n = 1L, fill = MinDate, type = "lag"), 
        by = c(eval(GroupingVars))]
    }  
  } else {
    for (i in seq_len(length(DateCols))) {
      data.table::setorderv(x = data, cols = eval(DateCols[i]), order = 1, na.last = TRUE)
      data.table::set(data, j = paste0("Lag1_", eval(DateCols[i])),
                      value = data.table::shift(x = data[[eval(DateCols[i])]], n = 1L, fill = MinDate, type = "lag"))
    }
  }
  
  # Enforce the missing lagged date to equal the regular date minus a constant----
  x <- data[, quantile(x = (data[[eval(DateCols[1])]] - data[[(paste0("Lag1_",eval(DateCols[1])))]]), probs = 0.99)]
  data[, eval(paste0("Lag1_",DateCols[i])) := get(DateCols[i]) - x]
  
  # Sort by group and date----
  if(!is.null(GroupingVars)) {
    data <- data[order(get(GroupingVars),get(DateCols))]
  }
  
  # Build Features----
  for (i in seq_len(length(DateCols))) {
    EndDateVector <- data[[eval(DateCols[i])]]
    StartDateVector <- data[[paste0("Lag1_", eval(DateCols[i]))]]
    data.table::set(data, i = which(data[[eval(DateCols[i])]] == MinDate), j = eval(paste0("Lag1_",DateCols[i])), value = MinDate - x)
    for(j in as.integer(seq_len(data[,.N]))) {
      data.table::set(x = data, i = j, j = "HolidayCounts",
                      value = HolidayCountsInRange(Start = StartDateVector[j],End = EndDateVector[j],Values = Holidays))
    }
    
    # Remove Lag1date----
    data.table::set(data, j = eval(paste0("Lag1_",DateCols[i])), value = NULL)
  }
  
  # Return data
  return(data)
}
