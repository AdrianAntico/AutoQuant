#' FakeDataGenerator
#' 
#' @author Adrian Antico
#' @family Data Wrangling
#' @param Correlation Set the correlation value for simulated data
#' @param N Number of records
#' @param ID Number of IDcols to include
#' @param ZIP Zero Inflation Model target variable creation. Select from 0 to 5 to create that number of distinctly distributed data, stratifed from small to large
#' @param ChainLadderData Set to TRUE to return Chain Ladder Data for using AutoMLChainLadderTrainer
#' @param FactorCount Number of factor type columns to create
#' @param AddDate Set to TRUE to include a date column
#' @param Classification Set to TRUE to build classification data
#' @param MultiClass Set to TRUE to build MultiClass data
#' @examples 
#' \donttest{
#' data <- RemixAutoML::FakeDataGenerator(
#'    Correlation = 0.70,
#'    N = 25000L,
#'    ID = 2L,
#'    ZIP = 2L,
#'    ChainLadderData = FALSE,
#'    FactorCount = 2L,
#'    AddDate = TRUE,
#'    Classification = FALSE,
#'    MultiClass = FALSE)
#' }
#' @export
FakeDataGenerator <- function(Correlation = 0.70,
                              N = 25000L,
                              ID = 5L,
                              ZIP = 5L,
                              ChainLadderData = FALSE,
                              FactorCount = 2L,
                              AddDate = TRUE,
                              Classification = FALSE,
                              MultiClass = FALSE) {
  
  # Create ChainLadderData----
  if(ChainLadderData) {
    
    # Define constants
    N <- 1000L
    MaxCohortDays <- 15L
    
    # Start date
    CalendarDateData <- data.table::data.table(CalendarDateColumn = rep(as.Date("2017-01-01"), N))
    
    # Increment date column so it is sequential
    CalendarDateData[, temp := seq_len(N)]
    CalendarDateData[, CalendarDateColumn := CalendarDateColumn + lubridate::days(temp) - 1L]
    CohortDate_temp <- data.table::copy(CalendarDateData)
    data.table::setnames(x = CohortDate_temp, old = c("CalendarDateColumn"), new = c("CohortDate_temp"))
    
    # Cross join the two data sets
    ChainLadderData <- data.table::CJ(
      CalendarDateColumn = CalendarDateData$CalendarDateColumn, 
      CohortDateColumn = CohortDate_temp$CohortDate_temp, 
      sorted = TRUE)
    
    # Remove starter data sets and N
    rm(CalendarDateData, CohortDate_temp, N)
    
    # View ChainLadderData
    print(ChainLadderData)
    
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
      
      # Print to watch speed
      print(cal)
    }
    
    # Fill in Leads and Conversions----
    x <- unique(ChainLadderData[, .SD, .SDcols = c("CalendarDateColumn","Leads")])
    x[, Leads := runif(n = x[, .N], min = 100, max = 500)]
    ChainLadderData <- merge(ChainLadderData[, .SD, .SDcols = c("CalendarDateColumn","CohortDateColumn","CohortDays","Appointments","Rates")], x, by = "CalendarDateColumn", all = FALSE)
    ChainLadderData[, Appointments := Leads * Rates]
    ChainLadderData[, Rates := NULL]
    data.table::setcolorder(ChainLadderData, c(1,2,3,5,4))
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
    data <- data[, DateTime := as.Date(Sys.time())]
    data[, temp := 1L:.N][, DateTime := DateTime - temp][, temp := NULL]
    data <- data[order(DateTime)]
  }
  
  # Zero Inflation Setup----
  if(!Classification & !MultiClass) {
    if(ZIP == 1L) {
      # hist(data$Adrian)
      # hist(data$Independent_Variable8)
      # hist(log(MASS::rnegbin(n = N, mu = 50, theta = 0.25)) + 1L)
      # hist(rnbinom(n = N, size = 50, prob = 0.5))
      data[, Adrian := data.table::fifelse(Adrian < 0.5, 0, Independent_Variable8)][, Independent_Variable8 := NULL]
    } else if(ZIP == 2L) {
      data[, Adrian := data.table::fifelse(Adrian < 0.33, 0, data.table::fifelse(Adrian < 0.66, log(Adrian * 10), log(Adrian*20)))]
    } else if(ZIP == 3L) {
      data[, Adrian := data.table::fifelse(Adrian < 0.25, 0, data.table::fifelse(Adrian < 0.50, log(Adrian * 10), data.table::fifelse(Adrian < 0.75, log(Adrian * 50), log(Adrian * 150))))]
    } else if(ZIP == 4L) {
      data[, Adrian := data.table::fifelse(Adrian < 0.20, 0, data.table::fifelse(Adrian < 0.40, log(Adrian * 10), data.table::fifelse(Adrian < 0.60, log(Adrian * 50), data.table::fifelse(Adrian < 0.80, log(Adrian * 150), log(Adrian * 250)))))]
    } else if(ZIP == 5L) {
      data[, Adrian := data.table::fifelse(Adrian < 1/6, 0, data.table::fifelse(Adrian < 2/6, log(Adrian * 10), data.table::fifelse(Adrian < 3/6, log(Adrian * 50), data.table::fifelse(Adrian < 4/6, log(Adrian * 250), data.table::fifelse(Adrian < 5/6, log(Adrian * 500))))))]
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
  
  # Return data----
  return(data)
}