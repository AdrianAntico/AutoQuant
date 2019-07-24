#' ResidualOutliers is an automated time series outlier detection function
#'
#' ResidualOutliers is an automated time series outlier detection function that utilizes tsoutliers and auto.arima. It looks for five types of outliers: "AO" Additive outliter - a singular extreme outlier that surrounding values aren't affected by; "IO" Innovational outlier - Initial outlier with subsequent anomalous values; "LS" Level shift - An initial outlier with subsequent observations being shifted by some constant on average; "TC" Transient change - initial outlier with lingering effects that dissapate exponentially over time; "SLS" Seasonal level shift - similar to level shift but on a seasonal scale.
#'
#' @author Adrian Antico
#' @family Unsupervised Learning
#' @param data the source residuals data.table
#' @param DateColName The name of your data column to use in reference to the target variable
#' @param TargetColName The name of your target variable column
#' @param PredictedColName The name of your predicted value column. If you supply this, you will run anomaly detection of the difference between the target variable and your predicted value. If you leave PredictedColName NULL then you will run anomaly detection over the target variable.
#' @param TimeUnit The time unit of your date column: hour, day, week, month, quarter, year
#' @param maxN the largest lag or moving average (seasonal too) values for the arima fit
#' @param tstat the t-stat value for tsoutliers
#' @examples
#' data <- data.table::data.table(DateTime = as.Date(Sys.time()),
#'                                Target = as.numeric(stats::filter(rnorm(1000,
#'                                                                        mean = 50,
#'                                                                        sd = 20),
#'                                                                  filter=rep(1,10),
#'                                                                  circular=TRUE)))
#' data[, temp := seq(1:1000)][, DateTime := DateTime - temp][, temp := NULL]
#' data <- data[order(DateTime)]
#' data[, Predicted := as.numeric(stats::filter(rnorm(1000,
#'                                                    mean = 50,
#'                                                    sd = 20),
#'                                              filter=rep(1,10),
#'                                              circular=TRUE))]
#' stuff <- ResidualOutliers(data = data,
#'                           DateColName = "DateTime",
#'                           TargetColName = "Target",
#'                           PredictedColName = NULL,
#'                           TimeUnit = "day",
#'                           maxN = 5,
#'                           tstat = 4)
#' data     <- stuff[[1]]
#' model    <- stuff[[2]]
#' outliers <- data[type != "<NA>"]
#' @return A named list containing FullData = original data.table with outliers data and ARIMA_MODEL = the arima model.
#' @export
ResidualOutliers <- function(data,
                             DateColName = "DateTime",
                             TargetColName = "Target",
                             PredictedColName = NULL,
                             TimeUnit = "day",
                             maxN = 5,
                             tstat = 2) {
  # Define TS Frequency
  if (tolower(TimeUnit) == "hour") {
    freq <- 24
  } else if (tolower(TimeUnit) == "day") {
    freq <- 365
  } else if (tolower(TimeUnit) == "week") {
    freq <- 52
  } else if (tolower(TimeUnit) == "month") {
    freq <- 12
  } else if (tolower(TimeUnit) == "quarter") {
    freq <- 4
  } else if (tolower(TimeUnit) == "year") {
    freq <- 1
  } else {
    warning("TimeUnit is not in hour, day, week, month,
    quarter, or year")
  }
  
  # Ensure data is a data.table----
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # convert DateColName to POSIXct----
  if(is.character(data[[eval(DateColName)]] | is.factor(data[[eval(DateColName)]])) {
    data[, eval(DateColName) := as.POSIXct(get(DateColName))]
  }
    
  # Ensure data is sorted----
  data.table::setorderv(x = data,
                        cols = eval(DateColName),
                        order = 1)
  
  # Keep columns----
  if (!is.null(PredictedColName)) {
    data[, Residuals := get(TargetColName) - get(PredictedColName)]
  } else {
    data[, Residuals := get(TargetColName)]
  }
  keep <- c(DateColName, "Residuals")
  temp <- data[, ..keep]
  MinVal <- min(data[[eval(TargetColName)]], na.rm = TRUE)
  
  
  # Convert to time series object----
  tsData <- stats::ts(temp,
                      start = temp[, min(get(DateColName))][[1]],
                      frequency = freq)
  
  # Build the auto arima----
  if (MinVal > 0) {
    fit <-
      tryCatch({
        forecast::auto.arima(
          y = tsData[, "Residuals"],
          max.p = maxN,
          max.q = maxN,
          max.P = maxN,
          max.Q = maxN,
          max.d = 1,
          max.D = 1,
          ic = "bic",
          lambda = TRUE,
          biasadj = TRUE,
          stepwise = TRUE
        )
      },
      error = function(x)
        "empty")
  } else {
    fit <-
      tryCatch({
        forecast::auto.arima(
          y = tsData[, "Residuals"],
          max.p = maxN,
          max.q = maxN,
          max.P = maxN,
          max.Q = maxN,
          max.d = 1,
          max.D = 1,
          ic = "bic",
          lambda = FALSE,
          biasadj = FALSE,
          stepwise = TRUE
        )
      },
      error = function(x)
        "empty")
  }
  
  # Store the arima parameters----
  pars  <- tsoutliers::coefs2poly(fit)
  
  # Store the arima residuals----
  resid <- cbind(tsData, stats::residuals(fit))
  
  # Find the outliers
  x <- data.table::as.data.table(tsoutliers::locate.outliers(
    resid = resid[, 3],
    pars = pars,
    cval = tstat,
    types = c("AO", "TC", "LS", "IO", "SLS")
  ))
  
  # Merge back to source data----
  residDT <- data.table::as.data.table(resid)
  z <- cbind(data, residDT)
  z[, ind := 1:.N]
  data.table::setnames(z,
                       names(z)[c((ncol(z) - 3):(ncol(z) - 1))],
                       c("ObsNum", "Preds", "ARIMA_Residuals"))
  z[, ObsNum := NULL]
  data <- merge(z, x, by = "ind", all.x = TRUE)
  data[, ':=' (ind = NULL, coefhat = NULL)]
  data[type == "<NA>", type := NA]
  
  # Reorder data, remove the coefhat column to send to database or stakeholder----
  return(list(FullData = data, ARIMA_MODEL = fit))
}
