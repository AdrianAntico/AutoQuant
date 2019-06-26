#' ProblematicRecords identifies problematic records for further investigation
#'
#' ProblematicRecords identifies problematic records for further investigation and data.table with 3 additional columns at the beginning of the data.table: PredictedOutlier (0 = no outlier, 1 = outlier), predict (raw H2O predicted value from Isolation Forest), and mean_length (mean length of number of splits)
#'
#' @author Adrian Antico
#' @family EDA
#' @param data The data.table with the columns you wish to have analyzed
#' @param ColumnNumbers A vector with the column numbers you wish to analyze
#' @param Threshold Quantile value to find the cutoff value for classifying outliers
#' @param MaxMem Specify the amount of memory to allocate to H2O. E.g. "28G"
#' @param NThreads Specify the number of threads (E.g. cores * 2)
#' @param NTrees Specify the number of decision trees to build
#' @param SampleRate Specify the row sample rate per tree
#' @examples
#' \donttest{
#'  Correl <- 0.85
#' N <- 10000
#' data <- data.table::data.table(Target = runif(N))
#' data[, x1 := qnorm(Target)]
#' data[, x2 := runif(N)]
#' data[, Independent_Variable1 := log(pnorm(Correl * x1 +
#'                                            sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable2 := (pnorm(Correl * x1 +
#'                                         sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable3 := exp(pnorm(Correl * x1 +
#'                                            sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable4 := exp(exp(pnorm(Correl * x1 +
#'                                                sqrt(1-Correl^2) * qnorm(x2))))]
#' data[, Independent_Variable5 := sqrt(pnorm(Correl * x1 +
#'                                             sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable6 := (pnorm(Correl * x1 +
#'                                         sqrt(1-Correl^2) * qnorm(x2)))^0.10]
#' data[, Independent_Variable7 := (pnorm(Correl * x1 +
#'                                         sqrt(1-Correl^2) * qnorm(x2)))^0.25]
#' data[, Independent_Variable8 := (pnorm(Correl * x1 +
#'                                         sqrt(1-Correl^2) * qnorm(x2)))^0.75]
#' data[, Independent_Variable9 := (pnorm(Correl * x1 +
#'                                         sqrt(1-Correl^2) * qnorm(x2)))^2]
#' data[, Independent_Variable10 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^4]
#' data[, Target := as.factor(
#'  ifelse(Independent_Variable2 < 0.20, "A",
#'         ifelse(Independent_Variable2 < 0.40, "B",
#'                ifelse(Independent_Variable2 < 0.6,  "C",
#'                       ifelse(Independent_Variable2 < 0.8,  "D", "E")))))]
#' data[, Independent_Variable11 := as.factor(
#'  ifelse(Independent_Variable2 < 0.15, "A",
#'         ifelse(Independent_Variable2 < 0.45, "B",
#'                ifelse(Independent_Variable2 < 0.65,  "C",
#'                       ifelse(Independent_Variable2 < 0.85,  "D", "E")))))]
#' data[, ':=' (x1 = NULL, x2 = NULL)]
#' Outliers <- ProblematicRecords(data,
#'                               ColumnNumbers = NULL,
#'                               Threshold = 0.95,
#'                               MaxMem = "28G",
#'                               NThreads = -1)
#' }
#' @return A data.table
#' @export
ProblematicRecords <- function(data,
                               ColumnNumbers = NULL,
                               Threshold = 0.975,
                               MaxMem = "28G",
                               NThreads = -1,
                               NTrees = 100,
                               SampleRate = (sqrt(5) - 1) / 2) {
  # Ensure H2O is installed----
  if (!requireNamespace("h2o")) {
    warning("Install H2O to run this function")
  }
  
  # Ensure data is a data.table----
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Initialize H2O----
  h2o::h2o.init(
    max_mem_size = MaxMem,
    nthreads = NThreads,
    enable_assertions = FALSE
  )
  
  # Ensure Characters are Converted to Factors----
  data <- RemixAutoML::ModelDataPrep(data,
                                     Impute = FALSE,
                                     CharToFactor = TRUE)
  
  # Convert data to H2O Frame----
  Data <- h2o::as.h2o(data)
  
  # Build Isolation Forest----
  if (is.null(ColumnNumbers)) {
    IsolationForest <- h2o::h2o.isolationForest(
      training_frame = Data,
      x = names(data),
      model_id = "test",
      ntrees = NTrees,
      sample_rate = SampleRate
    )
  } else {
    IsolationForest <- h2o::h2o.isolationForest(
      training_frame = Data,
      x = names(data)[ColumnNumbers],
      model_id = "test",
      ntrees = NTrees,
      sample_rate = SampleRate
    )
  }
  
  # Generate Outliers data.table----
  OutliersRaw <-
    data.table::as.data.table(h2o::h2o.predict(object = IsolationForest,
                                               newdata = Data))
  
  # Shutdown H2O
  h2o::h2o.shutdown(prompt = FALSE)
  
  # Add column for outlier indicator----
  setnames(OutliersRaw,
           c("predict", "mean_length"),
           c("PredictIsoForest", "MeanLength"))
  Cutoff <- quantile(OutliersRaw[["PredictIsoForest"]],
                     probs = Threshold)[[1]]
  OutliersRaw[, PredictedOutlier := ifelse(PredictIsoForest > Cutoff, 1, 0)]
  OutliersRaw[, PercentileRank := percRank(PredictIsoForest)]
  data.table::setcolorder(OutliersRaw, c(4, 3, 1, 2))
  
  # Merge back with source data----
  OutputData <- cbind(OutliersRaw, data)
  
  # Return data----
  return(OutputData[order(-PredictIsoForest)])
}