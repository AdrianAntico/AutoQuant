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
#' @param SimpleImpute Set to TRUE or FALSE. With TRUE numeric cols will fill NAs with a -1 and non-numeric cols with a "0"
#' @examples
#' \dontrun{
#' data <- TimeSeriesFill(
#'   data,
#'   DateColumnName = "Date",
#'   GroupVariables = "GroupVar",
#'   TimeUnit = "days",
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
                           SimpleImpute = FALSE) {

  # Grab args
  if(length(FillType) > 1) FillType <- FillType[1]

  # Set up list
  CJList <- list()

  # Get list of unique vectors
  if(!is.null(GroupVariables)) {
    for(group in GroupVariables) {
      CJList[[eval(group)]] <- unique(data[[eval(group)]])
    }
  }

  # Fill from the absolute min date to the absolute max date
  if(FillType == "maxmax") {
    MinDate <- data[, min(get(DateColumnName))]
    MaxDate <- data[, max(get(DateColumnName))]
    CJList[[eval(DateColumnName)]] <- seq(from = MinDate, to = MaxDate, by = TimeUnit)
  }

  # Fill from the max date of the min set to the absolute max date
  if(FillType == "minmax") {
    MinDate <- data[, min(get(DateColumnName)), by = c(eval(GroupVariables))][, max(V1)]
    MaxDate <- data[, max(get(DateColumnName))]
    CJList[[eval(DateColumnName)]] <- seq(from = MinDate, to = MaxDate, by = TimeUnit)
  }

  # Fill from the absolute min date to the min of the max dates
  if(FillType == "maxmin") {
    MinDate <- data[, min(get(DateColumnName))]
    MaxDate <- data[, max(get(DateColumnName)), by = c(eval(GroupVariables))][, min(V1)]
    CJList[[eval(DateColumnName)]] <- seq(from = MinDate, to = MaxDate, by = TimeUnit)
  }

  # Fill from the max date of the min dates to the min date of the max dates
  if(FillType == "minmin") {
    MinDate <- data[, min(get(DateColumnName))]
    MaxDate <- data[, max(get(DateColumnName)), by = c(eval(GroupVariables))][, min(V1)]
    CJList[[eval(DateColumnName)]] <- seq(from = MinDate, to = MaxDate, by = TimeUnit)
  }

  # Cross join keys
  FillData <- do.call(data.table::CJ, CJList)

  # Join data back to FillData
  FillData <- merge(FillData, data, by = c(eval(DateColumnName),eval(GroupVariables)), all.x = TRUE)

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
