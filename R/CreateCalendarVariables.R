#' CreateCalendarVariables Create Calendar Variables
#'
#' CreateCalendarVariables Rapidly creates calendar variables based on the date column you provide
#'
#' @author Adrian Antico
#' @family Feature Engineering
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
  if(!is.logical(AsFactor)) return("AsFactor needs to be TRUE or FALSE")
  if(!(any(tolower(TimeUnits) %chin% c("second","minute","hour","wday","mday","yday","week","isoweek","wom","month","quarter","year")))) {
    return("TimeUnits needs to be one of 'minute', 'hour', 'wday','mday', 'yday','week','wom','month', 'quarter', 'year'")
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
      } else if(tolower(j) == "isoweek") {
        if (AsFactor) {
          data.table::set(DataCompute, j = paste0(DateColRef, "_", TimeList[[i]][j]), value = as.factor(data.table::isoweek(DataCompute[[eval(paste0("DATE_", DateColRef))]])))
        } else {
          data.table::set(DataCompute, j = paste0(DateColRef, "_", j), value = data.table::isoweek(DataCompute[[eval(paste0("DATE_", DateColRef))]]))
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
    if(any(tolower(TimeList[[i]]) %chin% c("wday","mday","yday","week","isoweek","month","quarter","year"))) data.table::set(data, j = paste0("DATE_", DateColRef), value = NULL)
  }

  # Return data----
  return(data)
}
