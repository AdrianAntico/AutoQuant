# AutoQuant is a package for quickly creating high quality visualizations under a common and easy api.
# Copyright (C) <year>  <name of author>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

#' @title ResidualOutliers
#'
#' @description ResidualOutliers is an automated time series outlier detection function that utilizes tsoutliers and auto.arima. It looks for five types of outliers: "AO" Additive outliter - a singular extreme outlier that surrounding values aren't affected by; "IO" Innovational outlier - Initial outlier with subsequent anomalous values; "LS" Level shift - An initial outlier with subsequent observations being shifted by some constant on average; "TC" Transient change - initial outlier with lingering effects that dissapate exponentially over time; "SLS" Seasonal level shift - similar to level shift but on a seasonal scale.
#'
#' @author Adrian Antico
#' @family Unsupervised Learning
#'
#' @param data the source residuals data.table
#' @param DateColName The name of your data column to use in reference to the target variable
#' @param TargetColName The name of your target variable column
#' @param PredictedColName The name of your predicted value column. If you supply this, you will run anomaly detection of the difference between the target variable and your predicted value. If you leave PredictedColName NULL then you will run anomaly detection over the target variable.
#' @param TimeUnit The time unit of your date column: hour, day, week, month, quarter, year
#' @param Lags the largest lag or moving average (seasonal too) values for the arima fit
#' @param Diff The largest d value for differencing
#' @param MA Max moving average
#' @param SLags Max seasonal lags
#' @param SDiff The largest d value for seasonal differencing
#' @param SMA Max seasonal moving averages
#' @param tstat the t-stat value for tsoutliers
#' @param FixedParams Set to TRUE or FALSE. If TRUE, a stats::Arima() model if fitted with those parameter values. If FALSE, then an auto.arima is built with the parameter values representing the max those values can be.
#' @examples
#' \dontrun{
#' data <- data.table::data.table(
#'   DateTime = as.Date(Sys.time()),
#'   Target = as.numeric(
#'     stats::filter(
#'       rnorm(1000, mean = 50, sd = 20),
#'       filter=rep(1,10),
#'       circular=TRUE)))
#' data[, temp := seq(1:1000)][, DateTime := DateTime - temp][, temp := NULL]
#' data.table::setorderv(x = data, cols = 'DateTime', 1)
#' data[, Predicted := as.numeric(
#'   stats::filter(
#'     rnorm(1000, mean = 50, sd = 20),
#'     filter=rep(1,10),
#'     circular=TRUE))]
#' Output <- ResidualOutliers(
#'   data = data,
#'   DateColName = "DateTime",
#'   TargetColName = "Target",
#'   PredictedColName = NULL,
#'   TimeUnit = "day",
#'   Lags = 5,
#'   Diff = 1,
#'   MA = 5,
#'   SLags = 0,
#'   SDiff = 0,
#'   SMA = 0,
#'   tstat = 4)
#' data <- Output[['FullData']]
#' model <- Output[['ARIMA_MODEL']]
#' outliers <- data[type != "<NA>"]
#' }
#' @return A named list containing FullData = original data.table with outliers data and ARIMA_MODEL = the arima model object
#' @export
ResidualOutliers <- function(data,
                             DateColName = NULL,
                             TargetColName = NULL,
                             PredictedColName = NULL,
                             TimeUnit = 'day',
                             Lags = 5,
                             Diff = 1,
                             MA = 5,
                             SLags = 0,
                             SDiff = 1,
                             SMA = 0,
                             tstat = 2,
                             FixedParams = FALSE) {

  # Define TS Frequency
  if(tolower(TimeUnit) %chin% c('hour','hours')) {
    freq <- 24
  } else if (tolower(TimeUnit) %chin% c('day','days')) {
    freq <- 365
  } else if (tolower(TimeUnit) %chin% c('week','weeks')) {
    freq <- 52
  } else if (tolower(TimeUnit) %chin% c('month','months')) {
    freq <- 12
  } else if (tolower(TimeUnit) %chin% c('quarter','quarters')) {
    freq <- 4
  } else if (tolower(TimeUnit) %chin% c('year','years')) {
    freq <- 1
  } else {
    stop('TimeUnit is not in hour, day, week, month, quarter, or year')
  }

  # Ensure data is a data.table ----
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  # convert DateColName to POSIXct ----
  if(is.character(data[[eval(DateColName)]]) || is.factor(data[[eval(DateColName)]])) {
    data[, eval(DateColName) := as.POSIXct(get(DateColName))]
  }

  # Ensure data is sorted ----
  data.table::setorderv(x = data, cols = eval(DateColName), order = 1L)

  # Residuals
  if(!is.null(PredictedColName)) {
    data[, Residuals := get(TargetColName) - get(PredictedColName)]
  } else {
    data[, Residuals := get(TargetColName)]
  }

  # Keep columns ----
  temp <- data[, .SD, .SDcols = c(DateColName, 'Residuals')]
  MinVal <- data[, min(get(TargetColName), na.rm = TRUE)]

  # Convert to time series object ----
  tsData <- stats::ts(temp, start = temp[, min(get(DateColName))][[1L]], frequency = freq)

  # Build the auto arima ----
  if(!FixedParams) {
    fit <- tryCatch({
      forecast::auto.arima(
        y = tsData[, 'Residuals'],
        max.p = Lags,
        max.q = MA,
        max.P = SLags,
        max.Q = SMA,
        max.d = Diff,
        max.D = SDiff,
        ic = 'bic',
        lambda = if(MinVal > 0) TRUE else FALSE,
        biasadj = if(MinVal > 0) TRUE else FALSE,
        stepwise = TRUE)
      }, error = function(x) NULL)
  } else {
    fit <- tryCatch({
      stats::arima(
        tsData[, 'Residuals'],
        order = c(Lags, Diff, MA),
        seasonal = list(order = c(SLags, SDiff, SMA), period = freq),
        xreg = NULL, include.mean = TRUE,
        transform.pars = TRUE,
        fixed = NULL, init = NULL,
        method = c("ML"),
        optim.method = "BFGS",
        optim.control = list(),
        kappa = 1e6)
    }, error = function(x) NULL)

    # If FixedParams fails to build, try auto.arima approach
    if(is.null(fit)) {
      fit <- tryCatch({
        forecast::auto.arima(
          y = tsData[, 'Residuals'],
          max.p = Lags,
          max.q = MA,
          max.P = SLags,
          max.Q = SMA,
          max.d = if(Diff == 0) 1 else Diff,
          max.D = if(SDiff == 0) 1 else SDiff,
          ic = 'bic',
          lambda = if(MinVal > 0) TRUE else FALSE,
          biasadj = if(MinVal > 0) TRUE else FALSE,
          stepwise = TRUE)
      }, error = function(x) NULL)
    }
  }

  # Store the arima parameters ----
  if(is.null(fit)) stop('No model could be fit')
  pars <- tsoutliers::coefs2poly(fit)

  # Store the arima residuals ----
  resid <- cbind(tsData, stats::residuals(fit))

  # Find the outliers ----
  x <- data.table::as.data.table(
    tsoutliers::locate.outliers(
      resid = resid[, 3L],
      pars = pars,
      cval = tstat,
      types = c('AO','TC','LS','IO','SLS')))

  # Merge back to source data ----
  residDT <- data.table::as.data.table(resid)
  z <- cbind(data, residDT)
  z[, ind := seq_len(.N)]
  data.table::setnames(z, names(z)[c((ncol(z) - 3L):(ncol(z) - 1L))], c('ObsNum','Preds','ARIMA_Residuals'))
  data.table::set(z, j = 'ObsNum', value = NULL)
  data <- merge(z, x, by = 'ind', all.x = TRUE)
  data.table::set(data, j = c('ind', 'coefhat'), value = NULL)
  data[type == '<NA>', type := NA]

  # Reorder data, remove the coefhat column to send to database or stakeholder ----
  return(list(FullData = data, ARIMA_MODEL = fit))
}
