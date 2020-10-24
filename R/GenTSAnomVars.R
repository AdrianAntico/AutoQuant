#' GenTSAnomVars is an automated z-score anomaly detection via GLM-like procedure
#'
#' GenTSAnomVars is an automated z-score anomaly detection via GLM-like procedure. Data is z-scaled and grouped by factors and time periods to determine which points are above and below the control limits in a cumulative time fashion. Then a cumulative rate is created as the final variable. Set KeepAllCols to FALSE to utilize the intermediate features to create rolling stats from them. The anomalies are separated into those that are extreme on the positive end versus those that are on the negative end.
#'
#' @author Adrian Antico
#' @family Unsupervised Learning
#' @param data the source residuals data.table
#' @param ValueCol the numeric column to run anomaly detection over
#' @param GroupVars this is a group by variable
#' @param DateVar this is a time variable for grouping
#' @param HighThreshold this is the threshold on the high end
#' @param LowThreshold this is the threshold on the low end
#' @param KeepAllCols set to TRUE to remove the intermediate features
#' @param IsDataScaled set to TRUE if you already scaled your data
#' @examples
#' \dontrun{
#' data <- data.table::data.table(
#'   DateTime = as.Date(Sys.time()),
#'   Target = stats::filter(
#'     rnorm(10000, mean = 50, sd = 20),
#'   filter=rep(1,10),
#'   circular=TRUE))
#' data[, temp := seq(1:10000)][, DateTime := DateTime - temp][
#'   , temp := NULL]
#' data <- data[order(DateTime)]
#' x <- data.table::as.data.table(sde::GBM(N=10000)*1000)
#' data[, predicted := x[-1,]]
#' data[, Fact1 := sample(letters, size = 10000, replace = TRUE)]
#' data[, Fact2 := sample(letters, size = 10000, replace = TRUE)]
#' data[, Fact3 := sample(letters, size = 10000, replace = TRUE)]
#' stuff <- GenTSAnomVars(
#'   data,
#'   ValueCol = "Target",
#'   GroupVars = c("Fact1","Fact2","Fact3"),
#'   DateVar = "DateTime",
#'   HighThreshold = 1.96,
#'   LowThreshold = -1.96,
#'   KeepAllCols = TRUE,
#'   IsDataScaled  = FALSE)
#' }
#' @return The original data.table with the added columns merged in. When KeepAllCols is set to FALSE, you will get back two columns: AnomHighRate and AnomLowRate - these are the cumulative anomaly rates over time for when you get anomalies from above the thresholds (e.g. 1.96) and below the thresholds.
#' @export
GenTSAnomVars <- function(data,
                          ValueCol    = "Value",
                          GroupVars   = NULL,
                          DateVar     = "DATE",
                          HighThreshold = 1.96,
                          LowThreshold  = -1.96,
                          KeepAllCols = TRUE,
                          IsDataScaled  = FALSE) {

  # Turn on full speed ahead----
  data.table::setDTthreads(percent = 100)

  # Check data.table----
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  # Scale data if not already----
  if(!IsDataScaled) {
    newValCol <- paste(ValueCol, "zScaled", sep = "_")
    data[, eval(newValCol) := scale(get(ValueCol), center = TRUE, scale = TRUE)]
    ValueCol <- newValCol
  }

  # Global check for date----
  if(!is.null(DateVar)) {
    if(is.null(GroupVars)) {
      data.table::setorderv(data,eval(DateVar))
      data[, RowNumAsc := 1:.N]
      data[, AnomHigh := as.numeric(data.table::fifelse(get(ValueCol) > HighThreshold,1, 0))]
      data[, AnomLow := as.numeric(data.table::fifelse(get(ValueCol) < LowThreshold,1, 0))]
      data[, CumAnomHigh := cumsum(AnomHigh)]
      data[, CumAnomLow := cumsum(AnomLow)]
      data[, AnomHighRate := CumAnomHigh / RowNumAsc]
      data[, AnomLowRate := CumAnomLow / RowNumAsc]
      if(!KeepAllCols) {
        data[, ':=' (
          AnomHigh = NULL,
          AnomLow = NULL,
          CumAnomHigh = NULL,
          CumAnomLow = NULL,
          RowNumAsc = NULL)]
      }
    } else {
      data.table::setorderv(data, cols = c(eval(GroupVars),eval(DateVar)))
      data[, RowNumAsc := 1:.N, by = c(eval(GroupVars))]
      data[, AnomHigh := as.numeric(data.table::fifelse(get(ValueCol) > HighThreshold,1, 0))]
      data[, AnomLow := as.numeric(data.table::fifelse(get(ValueCol) < LowThreshold,1, 0))]
      data[, CumAnomHigh := cumsum(AnomHigh), by = c(eval(GroupVars))]
      data[, CumAnomLow := cumsum(AnomLow), by = c(eval(GroupVars))]
      data[, AnomHighRate := CumAnomHigh / RowNumAsc]
      data[, AnomLowRate := CumAnomLow / RowNumAsc]
      if(!KeepAllCols) {
        data[, ':=' (
          AnomHigh = NULL,
          AnomLow = NULL,
          CumAnomHigh = NULL,
          CumAnomLow = NULL,
          RowNumAsc = NULL)]
      }
    }
    return(data)
  }
  return(data)
}
