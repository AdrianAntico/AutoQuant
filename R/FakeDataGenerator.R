#' FakeDataGenerator
#'
#' @author Adrian Antico
#' @family Data Wrangling
#' @param Correlation Set the correlation value for simulated data
#' @param N Number of records
#' @param ID Number of IDcols to include
#' @param ZIP Zero Inflation Model target variable creation. Select from 0 to 5 to create that number of distinctly distributed data, stratifed from small to large
#' @param FactorCount Number of factor type columns to create
#' @param AddDate Set to TRUE to include a date column
#' @param AddComment Set to TRUE to add a comment column
#' @param TimeSeries For testing AutoBanditSarima
#' @param TimeSeriesTimeAgg Choose from "1min", "5min", "10min", "15min", "30min", "hour", "day", "week", "month", "quarter", "year",
#' @param ChainLadderData Set to TRUE to return Chain Ladder Data for using AutoMLChainLadderTrainer
#' @param Classification Set to TRUE to build classification data
#' @param MultiClass Set to TRUE to build MultiClass data
#' @examples
#' \dontrun{
#' data <- RemixAutoML::FakeDataGenerator(
#'    Correlation = 0.70,
#'    N = 1000L,
#'    ID = 2L,
#'    FactorCount = 2L,
#'    AddDate = TRUE,
#'    AddComment = FALSE,
#'    ZIP = 2L,
#'    TimeSeries = FALSE,
#'    ChainLadderData = FALSE,
#'    Classification = FALSE,
#'    MultiClass = FALSE)
#' }
#' @export
FakeDataGenerator <- function(Correlation = 0.70,
                              N = 1000L,
                              ID = 5L,
                              FactorCount = 2L,
                              AddDate = TRUE,
                              AddComment = FALSE,
                              ZIP = 5L,
                              TimeSeries = FALSE,
                              TimeSeriesTimeAgg = "day",
                              ChainLadderData = FALSE,
                              Classification = FALSE,
                              MultiClass = FALSE) {

  # Error checking
  if(sum(TimeSeries, Classification, MultiClass) > 1) stop("Only one of the following can be set to TRUE: TimeSeries, Classifcation, and MultiClass")

  # TimeSeries----
  if(TimeSeries) {

    # Error msg
    if(is.null(TimeSeriesTimeAgg)) stop("TimeSeriesAgg cannot be NULL when using TimeSeries = TRUE")

    # Pull in data
    data <- data.table::as.data.table(as.numeric(fpp::cafe))

    # Change names to common names for other calls in this function
    data.table::setnames(data, "V1", "Weekly_Sales")

    # Pick a starting date
    data.table::set(data, j = "Date", value = "1982-01-01")
    data.table::setcolorder(data, c(2L, 1L))
    data[, Date := as.Date(Date)]

    # "1min"
    if(tolower(TimeSeriesTimeAgg) %chin% c("1min","1mins","minutes","min","mins","01min","01mins")) {
      data[, xx := 1:.N][, Date := Date + lubridate::minutes(1 * 1:.N)][, xx := NULL]
    }

    # "5min"
    if(tolower(TimeSeriesTimeAgg) %chin% c("5min","5mins","5minutes","min5","mins5","05min")) {
      data[, Date := Date + lubridate::minutes(5 * 1:.N)]
    }

    # "10min"
    if(tolower(TimeSeriesTimeAgg) %chin% c("10min","10mins","10minutes","min10","mins10")) {
      data[, Date := Date + lubridate::minutes(10 * 1:.N)]
    }

    # "15min"
    if(tolower(TimeSeriesTimeAgg) %chin% c("15min","15mins","15minutes","min15","mins15")) {
      data[, Date := Date + lubridate::minutes(15 * 1:.N)]
    }

    # "30min"
    if(tolower(TimeSeriesTimeAgg) %chin% c("30min","30mins","30minutes","min30","mins30")) {
      data[, Date := Date + lubridate::minutes(30 * 1:.N)]
    }

    # "hour"
    if(tolower(TimeSeriesTimeAgg) %chin% c("hour","hours","hr","hrs","our","ours")) {
      data[, Date := Date + lubridate::hours(1:.N)]
    }

    # "day"
    if(tolower(TimeSeriesTimeAgg) %chin% c("day","days","daily","dy","das")) {
      data[, Date := Date + lubridate::days(1:.N)]
    }

    # "week"
    if(tolower(TimeSeriesTimeAgg) %chin% c("week","weeks","wk","wks")) {
      data[, Date := Date + lubridate::weeks(1:.N)]
    }

    # "month"
    if(tolower(TimeSeriesTimeAgg) %chin% c("month","months","mth","mths")) {
      data[, Date := Date %m+% months(1:.N)]
    }

    # "quarter"
    if(tolower(TimeSeriesTimeAgg) %chin% c("quarter","quarters"," qtr","qtrs","qarter")) {
      data[, Date := Date %m+% months(3 * 1:.N)]
    }

    # "year"
    if(tolower(TimeSeriesTimeAgg) %chin% c("year","years","yr","yrs","yts")) {
      data[, Date := Date + lubridate::years(1:.N)]
    }

    # Return data
    return(data)
  }

  # Create ChainLadderData----
  if(ChainLadderData) {

    # Overwrite N
    N <- 1000

    # Define constants
    MaxCohortDays <- 15L

    # Start date
    CalendarDateData <- data.table::data.table(CalendarDateColumn = rep(as.Date("2017-01-01"), N), key = "CalendarDateColumn")

    # Increment date column so it is sequential
    CalendarDateData[, temp := seq_len(N)]
    CalendarDateData[, CalendarDateColumn := CalendarDateColumn + lubridate::days(temp) - 1L]
    CohortDate_temp <- data.table::copy(CalendarDateData)
    data.table::setnames(x = CohortDate_temp, old = c("CalendarDateColumn"), new = c("CohortDate_temp"))

    # Cross join the two data sets
    ChainLadderData <- data.table::setkeyv(data.table::CJ(
      CalendarDateColumn = CalendarDateData$CalendarDateColumn,
      CohortDateColumn = CohortDate_temp$CohortDate_temp,
      sorted = TRUE,
      unique = TRUE),
    cols = c("CalendarDateColumn", "CohortDateColumn"))

    # Remove starter data sets and N
    rm(CalendarDateData, CohortDate_temp, N)

    # Remove impossible dates
    ChainLadderData <- ChainLadderData[CohortDateColumn >= CalendarDateColumn]

    # Add CohortPeriods
    ChainLadderData[, CohortDays := as.numeric(difftime(CohortDateColumn, CalendarDateColumn, tz = "MST", units = "day"))]

    # Limit the number of CohortTime
    ChainLadderData <- ChainLadderData[CohortDays < MaxCohortDays]

    # Add measure columns placeholder values
    ChainLadderData[, ":=" (Leads = 0, Appointments = 0, Rates = 0)]

    # Sort decending both date columns
    data.table::setorderv(x = ChainLadderData, cols = c("CalendarDateColumn","CohortDateColumn"), order = c(-1L, 1L))

    # Add columns for BaselineMeasure and ConversionMeasure
    UniqueCalendarDates <- unique(ChainLadderData$CalendarDateColumn)
    NN <- length(UniqueCalendarDates)
    LoopSeq <- c(1:15)
    LoopSeq <- cumsum(LoopSeq)
    LoopSeq <- c(1, LoopSeq)
    LoopSeq <- c(LoopSeq, seq(135, 15*993, 15))
    for(cal in seq(NN)) {

      # Generate first element of decay data
      DecayCurveData <- dgeom(x = 0, prob = runif(n = 1L, min = 0.45, max = 0.55), log = FALSE)

      # Fill in remain elements in vector
      if(cal > 1L) {
        zz <- seq_len(min(15L, cal))
        for(i in zz[1:min(cal-1L,15)]) {
          DecayCurveData <- c(DecayCurveData, c(dgeom(x = i, prob = runif(n = 1L, min = 0.45, max = 0.55), log = FALSE)))
        }
      }

      # Fill ChainLadderData
      data.table::set(ChainLadderData, i = (LoopSeq[cal]+1L):LoopSeq[cal + 1L], j = "Rates", value = DecayCurveData[seq_len(min(15L, cal))])
    }

    # Fill in Leads and Conversions----
    x <- unique(ChainLadderData[, .SD, .SDcols = c("CalendarDateColumn","Leads")])
    x[, Leads := runif(n = x[, .N], min = 100, max = 500)]
    ChainLadderData <- merge(ChainLadderData[, .SD, .SDcols = c("CalendarDateColumn","CohortDateColumn","CohortDays","Appointments","Rates")], x, by = "CalendarDateColumn", all = FALSE)
    ChainLadderData[, Appointments := Leads * Rates]
    ChainLadderData[, Rates := NULL]
    data.table::setcolorder(ChainLadderData, c(1,2,3,5,4))
    ChainLadderData[, Leads2 := runif(n = ChainLadderData[,.N], min = 100, max = 500)]
    data.table::setcolorder(ChainLadderData, c(1:4, 6, 5))
    return(ChainLadderData)
  }

  # Modify----
  if(MultiClass & FactorCount == 0L) {
    FactorCount <- 1L
    temp <- 1L
  }

  # Create data----
  Correl <- Correlation
  data <- data.table::data.table(Adrian = runif(N))
  data[, x1 := qnorm(Adrian)]
  data[, x2 := runif(N)]
  data[, Independent_Variable1 := log(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
  data[, Independent_Variable2 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
  data[, Independent_Variable3 := exp(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
  data[, Independent_Variable4 := exp(exp(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2))))]
  data[, Independent_Variable5 := sqrt(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
  data[, Independent_Variable6 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^0.10]
  data[, Independent_Variable7 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^0.25]
  data[, Independent_Variable8 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^0.75]
  data[, Independent_Variable9 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^2]
  data[, Independent_Variable10 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^4]
  if(ID > 0L) for(i in seq_len(ID)) data[, paste0("IDcol_", i) := runif(N)]
  data[, ":=" (x2 = NULL)]

  # FactorCount----
  for(i in seq_len(FactorCount)) {
    RandomValues <- sort(c(runif(n = 4L, min = 0.01, max = 0.99)))
    RandomLetters <- sort(c(sample(x = LETTERS, size = 5L, replace = FALSE)))
    data[, paste0("Factor_", i) := as.factor(
      data.table::fifelse(Independent_Variable2 < RandomValues[1L], RandomLetters[1L],
                          data.table::fifelse(Independent_Variable2 < RandomValues[2L], RandomLetters[2L],
                                              data.table::fifelse(Independent_Variable2 < RandomValues[3L],  RandomLetters[3L],
                                                                  data.table::fifelse(Independent_Variable2 < RandomValues[4L],  RandomLetters[4L], RandomLetters[5L])))))]
  }

  # Add date----
  if(AddDate) {
    if(FactorCount == 0) {
      data <- data[, DateTime := as.Date(Sys.time())]
      data[, temp := 1L:.N][, DateTime := DateTime - temp][, temp := NULL]
      data <- data[order(DateTime)]
    } else {
      data <- data[, DateTime := as.Date(Sys.time())]
      data[, temp := 1L:.N, by = "Factor_1"][, DateTime := DateTime - temp][, temp := NULL]
      data <- data[order(Factor_1, DateTime)]
    }
  }

  # Zero Inflation Setup----
  if(!Classification & !MultiClass) {
    if(ZIP == 1L) {
      data[, Adrian := data.table::fifelse(Adrian < 0.5, 0, Independent_Variable8)][, Independent_Variable8 := NULL]
    } else if(ZIP == 2L) {
      data[, Adrian := data.table::fifelse(Adrian < 0.33, 0, data.table::fifelse(Adrian < 0.66, log(Adrian * 10), log(Adrian*20)))]
    } else if(ZIP == 3L) {
      data[, Adrian := data.table::fifelse(Adrian < 0.25, 0, data.table::fifelse(Adrian < 0.50, log(Adrian * 10), data.table::fifelse(Adrian < 0.75, log(Adrian * 50), log(Adrian * 150))))]
    } else if(ZIP == 4L) {
      data[, Adrian := data.table::fifelse(Adrian < 0.20, 0, data.table::fifelse(Adrian < 0.40, log(Adrian * 10), data.table::fifelse(Adrian < 0.60, log(Adrian * 50), data.table::fifelse(Adrian < 0.80, log(Adrian * 150), log(Adrian * 250)))))]
    } else if(ZIP == 5L) {
      data[, Adrian := data.table::fifelse(Adrian < 1/6, 0, data.table::fifelse(Adrian < 2/6, log(Adrian * 10), data.table::fifelse(Adrian < 3/6, log(Adrian * 50), data.table::fifelse(Adrian < 4/6, log(Adrian * 250), data.table::fifelse(Adrian < 5/6, log(Adrian * 500), log(Adrian * 1000))))))]
    }
  }

  # Classification----
  if(Classification) data[, Adrian := data.table::fifelse(x1 > 0.5, 1, 0)]

  # Remove----
  data[, ":=" (x1 = NULL)]

  # MultiClass----
  if(MultiClass) {
    data[, Adrian := NULL]
    data.table::setnames(data, "Factor_1", "Adrian")
  }

  # Comment data ----
  if(AddComment) {
    a <- c('Hello', 'Hi', 'Howdy')
    b <- c('really like', 'absolutely adore', 'sucks ass')
    c <- c('noload', 'download', 'upload')
    N1 <- 1/length(a)
    N2 <- 1/length(b)
    N3 <- 1/length(c)
    N11 <- 1/N1
    N22 <- 1/N2
    N33 <- 1/N3
    RandomText <- function(N1,N11,N2,N22,N3,N33,a,b,c) {
      paste(sample(x = a, size = 1, replace = TRUE, prob = rep(N1, N11)),
            sample(x = b, size = 1, replace = TRUE, prob = rep(N2, N22)),
            sample(x = c, size = 1, replace = TRUE, prob = rep(N3, N33)))
    }
    data[, Comments := "a"]
    for(i in seq_len(data[, .N])) {
      data.table::set(data, i = i, j = "Comment", value = RandomText(N1,N11,N2,N22,N3,N33,a,b,c))
    }
  }

  # Return data----
  return(data)
}
