#' TimeSeriesFill For Completing Time Series Data
#'
#' TimeSeriesFill For Completing Time Series Data For Single Series or Time Series by Group
#'
#' @family Feature Engineering
#' @param data Supply your full series data set here
#' @param DateColumnName Supply the name of your date column
#' @param GroupVariables Supply the column names of your group variables. E.g. "Group" or c("Group1","Group2")
#' @param TimeUnit Choose from "second", "minute", "hour", "day", "week", "month", "quarter", "year"
#' @param FillType Choose from maxmax - Fill from the absolute min date to the absolute max date, minmax - Fill from the max date of the min set to the absolute max date, maxmin - Fill from the absolute min date to the min of the max dates, or minmin - Fill from the max date of the min dates to the min date of the max dates
#' @param MaxMissingPercent The maximum amount of missing values an individual series can have to remain and be imputed. Otherwise, they are discarded.
#' @param SimpleImpute Set to TRUE or FALSE. With TRUE numeric cols will fill NAs with a -1 and non-numeric cols with a "0"
#' @examples
#' \dontrun{
#'
#' # Pull in data
#' data <- data <- data.table::fread("https://www.dropbox.com/s/2str3ek4f4cheqi/walmart_train.csv?dl=1")
#'
#' # Run function
#' data <- TimeSeriesFill(
#'   data,
#'   DateColumnName = "Date",
#'   GroupVariables = c("Store","Dept"),
#'   TimeUnit = "weeks",
#'   FillType = "maxmax",
#'   SimpleImpute = FALSE)
#' }
#' @return Returns a data table with missing time series records filled (currently just zeros)
#' @export
TimeSeriesFill <- function(data = data,
                           DateColumnName = "Date",
                           GroupVariables = c("Store","Dept"),
                           TimeUnit = "weeks",
                           FillType = c("maxmax","minmax","maxmin","minmin"),
                           MaxMissingPercent = 0.05,
                           SimpleImpute = FALSE) {

  # Grab args
  if(length(FillType) > 1) FillType <- FillType[1]

  # Set up list
  CJList <- list()

  # Fill from the absolute min date to the absolute max date
  if(FillType == "maxmax") {

    # Setup vectors for crossjoin
    MinDate <- data[, min(get(DateColumnName))]
    MaxDate <- data[, max(get(DateColumnName))]
    CJList[[eval(DateColumnName)]] <- seq(from = MinDate, to = MaxDate, by = TimeUnit)
    if(!is.null(GroupVariables)) {
      for(group in GroupVariables) {
        CJList[[eval(group)]] <- unique(data[[eval(group)]])
      }
    }

    # Cross join and then merge back original features
    FillData <- do.call(data.table::CJ, CJList)
    FillData <- merge(FillData, data, by = c(eval(DateColumnName),eval(GroupVariables)), all.x = TRUE)
  }

  # Fill from the max date of the min set to the absolute max date
  if(FillType == "minmax") {

    # Setup vectors for crossjoin
    MinDate <- data[, min(get(DateColumnName)), by = c(eval(GroupVariables))][, max(V1)]
    MaxDate <- data[, max(get(DateColumnName))]
    CJList[[eval(DateColumnName)]] <- seq(from = MinDate, to = MaxDate, by = TimeUnit)
    if(!is.null(GroupVariables)) {
      for(group in GroupVariables) {
        CJList[[eval(group)]] <- unique(data[[eval(group)]])
      }
    }

    # Cross join and then merge back original features
    FillData <- do.call(data.table::CJ, CJList)
    FillData <- merge(FillData, data, by = c(eval(DateColumnName),eval(GroupVariables)), all.x = TRUE)
  }

  # Fill from the absolute min date to the min of the max dates
  if(FillType == "maxmin") {

    # Setup vectors for crossjoin
    MinDate <- data[, min(get(DateColumnName))]
    MaxDate <- data[, max(get(DateColumnName)), by = c(eval(GroupVariables))][, min(V1)]
    CJList[[eval(DateColumnName)]] <- seq(from = MinDate, to = MaxDate, by = TimeUnit)
    if(!is.null(GroupVariables)) {
      for(group in GroupVariables) {
        CJList[[eval(group)]] <- unique(data[[eval(group)]])
      }
    }

    # Cross join and then merge back original features
    FillData <- do.call(data.table::CJ, CJList)
    FillData <- merge(FillData, data, by = c(eval(DateColumnName),eval(GroupVariables)), all.x = TRUE)
  }

  # Fill from the max date of the min dates to the min date of the max dates
  if(FillType == "minmin") {

    # Setup vectors for crossjoin
    MinDate <- data[, min(get(DateColumnName))]
    MaxDate <- data[, max(get(DateColumnName)), by = c(eval(GroupVariables))][, min(V1)]
    CJList[[eval(DateColumnName)]] <- seq(from = MinDate, to = MaxDate, by = TimeUnit)
    if(!is.null(GroupVariables)) {
      for(group in GroupVariables) {
        CJList[[eval(group)]] <- unique(data[[eval(group)]])
      }
    }

    # Cross join and then merge back original features
    FillData <- do.call(data.table::CJ, CJList)
    FillData <- merge(FillData, data, by = c(eval(DateColumnName),eval(GroupVariables)), all.x = TRUE)
  }

  # Remove combinations that never existed
  temp <- unique(data[, mget(GroupVariables)])
  FillData <- merge(FillData, temp, by = GroupVariables, all = FALSE)
  FillData[, Check := sum(!is.na(names(FillData)[!names(FillData) %chin% c(eval(GroupVariables),eval(DateColumnName))][1L])), by = eval(GroupVariables)]
  CompareVal <- FillData[, quantile(Check, 0.95)[[1L]]]
  FillData <- FillData[Check > (1 - eval(MaxMissingPercent)) * eval(CompareVal)][, Check := NULL]

  # Impute
  if(SimpleImpute) {
    FillData <- RemixAutoML::ModelDataPrep(
      FillData,
      Impute = TRUE,
      CharToFactor = FALSE,
      FactorToChar = FALSE,
      IntToNumeric = FALSE,
      DateToChar = FALSE,
      RemoveDates = FALSE,
      MissFactor = "0",
      MissNum = -1,
      IgnoreCols = NULL)
  }

  # Return data
  return(FillData)
}
