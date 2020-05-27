#' CreateCalendarVariables Create Caledar Variables
#'
#' CreateCalendarVariables Rapidly creates calendar variables based on the date column you provide
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @param data This is your data
#' @param DateCols Supply either column names or column numbers of your date columns you want to use for creating calendar variables
#' @param AsFactor Set to TRUE if you want factor type columns returned; otherwise integer type columns will be returned
#' @param TimeUnits Supply a character vector of time units for creating calendar variables. Options include: "second", "minute", "hour", "wday", "mday", "yday", "week", "isoweek", "month", "quarter", "year"
#' @examples
#' data <- data.table::data.table(Date = "2018-01-01 00:00:00")
#' data <- CreateCalendarVariables(
#'    data,
#'    DateCols = "Date",
#'    AsFactor = FALSE,
#'    TimeUnits = c("wday", "month", "year"))
#' @return Returns your data.table with the added calendar variables at the end
#' @export
CreateCalendarVariables <- function(data,
                                    DateCols = NULL,
                                    AsFactor = FALSE,
                                    TimeUnits = "wday") {
  
  # Turn on full speed ahead----
  data.table::setDTthreads(threads = max(1L, parallel::detectCores()-2L))
  if(!data.table::is.data.table(data)) data.table::setDT(data)
  
  # Check args----
  if(!is.logical(AsFactor)) return("AsFactor needs to be TRUE or FALSE")
  if(!(any(tolower(TimeUnits) %chin% c("second","minute","hour","wday","mday","yday","week","isoweek","month","quarter","year")))) {
    return("TimeUnits needs to be one of 'minute', 'hour', 'wday','mday', 'yday','week', 'month', 'quarter', 'year'")
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
  
  # Allocate data.table cols
  data.table::alloc.col(DT = data, ncol(data) + sum(Cols))
  
  # Create DateCols to data.table IDateTime types----
  for(i in seq_len(length(DateCols))) {
    if(length(TimeList) != 0L) {
      if(any(tolower(TimeList[[i]]) %chin% c("second", "minute", "hour"))) data.table::set(data, j = paste0("TIME_", eval(DateCols[i])), value = data.table::as.ITime(data[[eval(DateCols[i])]]))
      if(any(tolower(TimeList[[i]]) %chin% c("wday","mday","yday","week","isoweek","month","quarter","year"))) data.table::set(data, j = paste0("DATE_", eval(DateCols[i])), value = data.table::as.IDate(data[[eval(DateCols[i])]]))
    }
  }
  
  # Build Features----
  for(i in seq_len(length(DateCols))) {
    for(j in TimeList[[i]]) {
      if(tolower(j) == "second") {
        if(AsFactor) {
          data.table::set(data, j = paste0(DateCols[i], "_", j), value = as.factor(data.table::second(data[[eval(paste0("TIME_", DateCols[i]))]])))
        } else {
          data.table::set(data, j = paste0(DateCols[i], "_", j), value = as.integer(data.table::second(data[[eval(paste0("TIME_", DateCols[i]))]])))
        }
      } else if(tolower(j) == "minute") {
        if(AsFactor) {
          data.table::set(data, j = paste0(DateCols[i], "_", TimeList[[i]][j]), value = as.factor(data.table::minute(data[[eval(paste0("TIME_", DateCols[i]))]])))
        } else {
          data.table::set(data, j = paste0(DateCols[i], "_", j), value = as.integer(data.table::minute(data[[paste0("TIME_", DateCols[i])]])))
        }
      } else if(tolower(j) == "hour") {
        if(AsFactor) {
          data.table::set(data, j = paste0(DateCols[i], "_", TimeList[[i]][j]), value = as.factor(data.table::hour(data[[eval(paste0("TIME_", DateCols[i]))]])))
        } else {
          data.table::set(data, j = paste0(DateCols[i], "_", j), value = as.integer(data.table::hour(data[[eval(paste0("TIME_", DateCols[i]))]])))
        }
      } else if(tolower(j) == "wday") {
        if (AsFactor) {
          data.table::set(data, j = paste0(DateCols[i], "_", TimeList[[i]][j]), value = as.factor(data.table::wday(data[[eval(paste0("DATE_", DateCols[i]))]])))
        } else {
          data.table::set(data, j = paste0(DateCols[i], "_", j), value = as.integer(data.table::wday(data[[eval(paste0("DATE_", DateCols[i]))]])))
        }
      } else if(tolower(j) == "mday") {
        if (AsFactor) {
          data.table::set(data, j = paste0(DateCols[i], "_", TimeList[[i]][j]), value = as.factor(data.table::mday(data[[eval(paste0("DATE_", DateCols[i]))]])))
        } else {
          data.table::set(data, j = paste0(DateCols[i], "_", j), value = as.integer(data.table::mday(data[[eval(paste0("DATE_", DateCols[i]))]])))
        }
      } else if(tolower(j) == "yday") {
        if (AsFactor) {
          data.table::set(data, j = paste0(DateCols[i], "_", TimeList[[i]][j]), value = as.factor(data.table::yday(data[[eval(paste0("DATE_", DateCols[i]))]])))
        } else {
          data.table::set(data, j = paste0(DateCols[i], "_", j), value = as.integer(data.table::yday(data[[eval(paste0("DATE_", DateCols[i]))]])))
        }
      } else if(tolower(j) == "week") {
        if (AsFactor) {
          data.table::set(data, j = paste0(DateCols[i], "_", TimeList[[i]][j]), value = as.factor(data.table::week(data[[eval(paste0("DATE_", DateCols[i]))]])))
        } else {
          data.table::set(data, j = paste0(DateCols[i], "_", j), value = as.integer(data.table::week(data[[eval(paste0("DATE_", DateCols[i]))]])))
        }
      } else if(tolower(j) == "isoweek") {
        if (AsFactor) {
          data.table::set(data, j = paste0(DateCols[i], "_", TimeList[[i]][j]), value = as.factor(data.table::isoweek(data[[eval(paste0("DATE_", DateCols[i]))]])))
        } else {
          data.table::set(data, j = paste0(DateCols[i], "_", j), value = as.integer(data.table::isoweek(data[[eval(paste0("DATE_", DateCols[i]))]])))
        }
      } else if(tolower(j) == "month") {
        if (AsFactor) {
          data.table::set(data, j = paste0(DateCols[i], "_", TimeList[[i]][j]), value = as.factor(data.table::month(data[[eval(paste0("DATE_", DateCols[i]))]])))
        } else {
          data.table::set(data, j = paste0(DateCols[i], "_", j), value = as.integer(data.table::month(data[[eval(paste0("DATE_", DateCols[i]))]])))
        }
      } else if(tolower(j) == "quarter") {
        if (AsFactor) {
          data.table::set(data, j = paste0(DateCols[i], "_", TimeList[[i]][j]), value = as.factor(data.table::quarter(data[[eval(paste0("DATE_", DateCols[i]))]])))
        } else {
          data.table::set(data, j = paste0(DateCols[i], "_", j), value = as.integer(data.table::quarter(data[[eval(paste0("DATE_", DateCols[i]))]])))
        }
      } else if(tolower(j) == "year") {
        if (AsFactor) {
          data.table::set(data, j = paste0(DateCols[i], "_", TimeList[[i]][j]), value = as.factor(data.table::year(data[[eval(paste0("DATE_", DateCols[i]))]])))
        } else {
          data.table::set(data, j = paste0(DateCols[i], "_", j), value = as.integer(data.table::year(data[[eval(paste0("DATE_", DateCols[i]))]])))
        }
      }
    }
    if(any(tolower(TimeList[[i]]) %chin% c("second", "minute", "hour"))) data.table::set(data, j = paste0("TIME_", DateCols[i]), value = NULL)
    if(any(tolower(TimeList[[i]]) %chin% c("wday","mday","yday","week","isoweek","month","quarter","year"))) data.table::set(data, j = paste0("DATE_", DateCols[i]), value = NULL)
  }
  
  # Remove constant columns----
  if(ncol(data) - NumCols > 0L & nrow(data) > 1L) for(col in (NumCols + 1L):ncol(data)) if(var(data[[names(data)[col]]], na.rm = TRUE) == 0L) data.table::set(data, j = eval(col), value = NULL)

  # Return data----
  return(data)
}
