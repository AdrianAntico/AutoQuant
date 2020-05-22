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
#' @param Classification Set to TRUE to build classification data
#' @param MultiClass Set to TRUE to build MultiClass data
#' @examples 
#' \donttest{
#' data <- RemixAutoML::FakeDataGenerator(
#'    Correlation = 0.70,
#'    N = 25000L,
#'    ID = 2L,
#'    ZIP = 2L,
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
                              FactorCount = 2L,
                              AddDate = TRUE,
                              Classification = FALSE,
                              MultiClass = FALSE) {
  
  # 
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
      data[, Adrian := data.table::fifelse(Adrian < 0.5, 0, log(Adrian*10))]
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
  
  if(MultiClass & FactorCount == 0L) FactorCount <- 1L
  
  # Return data----
  return(data)
}