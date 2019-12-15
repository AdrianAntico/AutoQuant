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
#' data <- CreateCalendarVariables(data,
#'                                 DateCols = "Date",
#'                                 AsFactor = FALSE,
#'                                 TimeUnits = c("wday", "month", "year"))
#' @return Returns your data.table with the added calendar variables at the end
#' @export
CreateCalendarVariables <- function(data,
                                    DateCols = c("Date", "Date2"),
                                    AsFactor = FALSE,
                                    TimeUnits = "wday") {
  # Convert to data.table----
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Check args----
  if (!is.logical(AsFactor)) {
    warning("AsFactor needs to be TRUE or FALSE")
  }
  if (!(any(
    tolower(TimeUnits) %chin% c(
      "second",
      "minute",
      "hour",
      "wday",
      "mday",
      "yday",
      "week",
      "isoweek",
      "month",
      "quarter",
      "year"
    )
  ))) {
    warning(
      "TimeUnits needs to be one of 'minute', 'hour', 'wday',
            'mday', 'yday','week', 'month', 'quarter', 'year'"
    )
  }
  
  # Turn DateCols into character names if not already----
  for (i in DateCols) {
    if (!is.character(DateCols[i])) {
      DateCols[i] <- names(data)[DateCols[i]]
    }
  }
  
  # Revise TimeUnits Based on Data----
  x <- 0
  TimeList <- list()
  Cols <- c()
  for (i in seq_len(length(DateCols))) {
    if (any(TimeUnits %chin% c("second", "minute", "hour"))) {
      if (min(as.ITime(data[[eval(DateCols[i])]])) - max(as.ITime(data[[eval(DateCols[i])]])) == 0) {
        TimeList[[i]] <-
          TimeUnits[!(tolower(TimeUnits) %chin% c("second", "minute", "hour"))]
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
  
  # Allocate data.table cols
  data.table::alloc.col(DT = data, ncol(data) + sum(Cols))
  
  # Create DateCols to data.table IDateTime types----
  for (i in seq_len(length(DateCols))) {
    if (length(TimeList) != 0) {
      if (any(tolower(TimeList[[i]]) %chin% c("second", "minute", "hour"))) {
        data.table::set(data,
                        j = paste0("TIME_", eval(DateCols[i])),
                        value = as.ITime(data[[eval(DateCols[i])]]))
      }
      if (any(
        tolower(TimeList[[i]]) %chin% c(
          "wday",
          "mday",
          "yday",
          "week",
          "isoweek",
          "month",
          "quarter",
          "year"
        )
      )) {
        data.table::set(data,
                        j = paste0("DATE_", eval(DateCols[i])),
                        value = data.table::as.IDate(data[[eval(DateCols[i])]]))
      }
    }
  }
  
  # Build Features----
  for (i in seq_len(length(DateCols))) {
    for (j in TimeList[[i]]) {
      if (tolower(j) == "second") {
        if (AsFactor) {
          data.table::set(
            data,
            j = paste0(DateCols[i], "_", j),
            value = as.factor(data.table::second(get(
              paste0("TIME_", DateCols[i])
            )))
          )
        } else {
          data.table::set(
            data,
            j = paste0(DateCols[i], "_", j),
            value = as.integer(data.table::second(data[[paste0("TIME_", DateCols[i])]]))
          )
        }
      } else if (tolower(j) == "minute") {
        if (AsFactor) {
          data.table::set(
            data,
            j = paste0(DateCols[i], "_", TimeList[[i]][j]),
            value = as.factor(data.table::minute(get(
              paste0("TIME_", DateCols[i])
            )))
          )
        } else {
          data.table::set(
            data,
            j = paste0(DateCols[i], "_", j),
            value = as.integer(data.table::minute(data[[paste0("TIME_", DateCols[i])]]))
          )
        }
      } else if (tolower(j) == "hour") {
        if (AsFactor) {
          data.table::set(
            data,
            j = paste0(DateCols[i], "_", TimeList[[i]][j]),
            value = as.factor(data.table::hour(get(
              paste0("TIME_", DateCols[i])
            )))
          )
        } else {
          data.table::set(
            data,
            j = paste0(DateCols[i], "_", j),
            value = as.integer(data.table::hour(data[[paste0("TIME_", DateCols[i])]]))
          )
        }
      } else if (tolower(j) == "wday") {
        if (AsFactor) {
          data.table::set(
            data,
            j = paste0(DateCols[i], "_", TimeList[[i]][j]),
            value = as.factor(data.table::wday(get(
              paste0("DATE_", DateCols[i])
            )))
          )
        } else {
          data.table::set(
            data,
            j = paste0(DateCols[i], "_", j),
            value = as.integer(data.table::wday(data[[paste0("DATE_", DateCols[i])]]))
          )
        }
      } else if (tolower(j) == "mday") {
        if (AsFactor) {
          data.table::set(
            data,
            j = paste0(DateCols[i], "_", TimeList[[i]][j]),
            value = as.factor(data.table::mday(get(
              paste0("DATE_", DateCols[i])
            )))
          )
        } else {
          data.table::set(
            data,
            j = paste0(DateCols[i], "_", j),
            value = as.integer(data.table::mday(data[[paste0("DATE_", DateCols[i])]]))
          )
        }
      } else if (tolower(j) == "yday") {
        if (AsFactor) {
          data.table::set(
            data,
            j = paste0(DateCols[i], "_", TimeList[[i]][j]),
            value = as.factor(data.table::yday(get(
              paste0("DATE_", DateCols[i])
            )))
          )
        } else {
          data.table::set(
            data,
            j = paste0(DateCols[i], "_", j),
            value = as.integer(data.table::yday(data[[paste0("DATE_", DateCols[i])]]))
          )
        }
      } else if (tolower(j) == "week") {
        if (AsFactor) {
          data.table::set(
            data,
            j = paste0(DateCols[i], "_", TimeList[[i]][j]),
            value = as.factor(data.table::week(get(
              paste0("DATE_", DateCols[i])
            )))
          )
        } else {
          data.table::set(
            data,
            j = paste0(DateCols[i], "_", j),
            value = as.integer(data.table::week(data[[paste0("DATE_", DateCols[i])]]))
          )
        }
      } else if (tolower(j) == "isoweek") {
        if (AsFactor) {
          data.table::set(
            data,
            j = paste0(DateCols[i], "_", TimeList[[i]][j]),
            value = as.factor(data.table::isoweek(get(
              paste0("DATE_", DateCols[i])
            )))
          )
        } else {
          data.table::set(
            data,
            j = paste0(DateCols[i], "_", j),
            value = as.integer(data.table::isoweek(data[[paste0("DATE_", DateCols[i])]]))
          )
        }
      } else if (tolower(j) == "month") {
        if (AsFactor) {
          data.table::set(
            data,
            j = paste0(DateCols[i], "_", TimeList[[i]][j]),
            value = as.factor(data.table::month(get(
              paste0("DATE_", DateCols[i])
            )))
          )
        } else {
          data.table::set(
            data,
            j = paste0(DateCols[i], "_", j),
            value = as.integer(data.table::month(data[[paste0("DATE_", DateCols[i])]]))
          )
        }
      } else if (tolower(j) == "quarter") {
        if (AsFactor) {
          data.table::set(
            data,
            j = paste0(DateCols[i], "_", TimeList[[i]][j]),
            value = as.factor(data.table::quarter(get(
              paste0("DATE_", DateCols[i])
            )))
          )
        } else {
          data.table::set(
            data,
            j = paste0(DateCols[i], "_", j),
            value = as.integer(data.table::quarter(data[[paste0("DATE_", DateCols[i])]]))
          )
        }
      } else if (tolower(j) == "year") {
        if (AsFactor) {
          data.table::set(
            data,
            j = paste0(DateCols[i], "_", TimeList[[i]][j]),
            value = as.factor(data.table::year(get(
              paste0("DATE_", DateCols[i])
            )))
          )
        } else {
          data.table::set(
            data,
            j = paste0(DateCols[i], "_", j),
            value = as.integer(data.table::year(data[[paste0("DATE_", DateCols[i])]]))
          )
        }
      }
    }
    if (any(tolower(TimeList[[i]]) %chin% c("second", "minute", "hour"))) {
      data.table::set(data,
                      j = paste0("TIME_", DateCols[i]),
                      value = NULL)
    }
    if (any(
      tolower(TimeList[[i]]) %chin% c(
        "wday",
        "mday",
        "yday",
        "week",
        "isoweek",
        "month",
        "quarter",
        "year"
      )
    )) {
      data.table::set(data,
                      j = paste0("DATE_", DateCols[i]),
                      value = NULL)
    }
  }
  return(data)
}
