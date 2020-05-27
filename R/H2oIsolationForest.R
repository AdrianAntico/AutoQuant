#' H2oIsolationForest for anomaly detection
#'
#' H2oIsolationForest for anomaly detection
#'
#' @author Adrian Antico
#' @family Unsupervised Learning
#' @param data The data.table with the columns you wish to have analyzed
#' @param TestData Data for scoring the trained isolation forest
#' @param ColumnNumbers A vector with the column numbers you wish to analyze
#' @param Threshold Quantile value to find the cutoff value for classifying outliers
#' @param MaxMem Specify the amount of memory to allocate to H2O. E.g. "28G"
#' @param NThreads Specify the number of threads (E.g. cores * 2)
#' @param NTrees Specify the number of decision trees to build
#' @param SampleRate Specify the row sample rate per tree
#' @examples
#' \donttest{
#' 
#' # Create simulated data
#' 
#' # Define correlation strength of features to target
#' Correl <- 0.85
#' 
#' # Number of rows you want returned
#' N <- 10000L
#' 
#' # Create data
#' data <- data.table::data.table(Target = runif(N))
#' data[, x1 := qnorm(Target)]
#' data[, x2 := runif(N)]
#' data[, Independent_Variable1 := log(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable2 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable3 := exp(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable4 := exp(exp(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2))))]
#' data[, Independent_Variable5 := sqrt(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable6 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^0.10]
#' data[, Independent_Variable7 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^0.25]
#' data[, Independent_Variable8 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^0.75]
#' data[, Independent_Variable9 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^2]
#' data[, Independent_Variable10 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^4]
#' data[, Target := as.factor(
#'  data.table::fifelse(Independent_Variable2 < 0.20, "A",
#'         data.table::fifelse(Independent_Variable2 < 0.40, "B",
#'                data.table::fifelse(Independent_Variable2 < 0.6,  "C",
#'                       data.table::fifelse(Independent_Variable2 < 0.8,  "D", "E")))))]
#' data[, Independent_Variable11 := as.factor(
#'  data.table::fifelse(Independent_Variable2 < 0.15, "A",
#'         data.table::fifelse(Independent_Variable2 < 0.45, "B",
#'                data.table::fifelse(Independent_Variable2 < 0.65,  "C",
#'                       data.table::fifelse(Independent_Variable2 < 0.85,  "D", "E")))))]
#' data.table::set(data, j = c("x1", "x2"), value = NULL)
#' 
#' # Run algo
#' Outliers <- H2oIsolationForest(data,
#'                                TestData = NULL,
#'                                ColumnNumbers = NULL,
#'                                Threshold = 0.95,
#'                                MaxMem = "28G",
#'                                NThreads = -1,
#'                                NTrees = 100,
#'                                SampleRate = (sqrt(5)-1)/2)
#' }
#' @return A data.table
#' @export
H2oIsolationForest <- function(data,
                               TestData = NULL,
                               ColumnNumbers = NULL,
                               Threshold = 0.975,
                               MaxMem = "28G",
                               NThreads = -1,
                               NTrees = 100,
                               SampleRate = (sqrt(5)-1)/2) {
  
  # Turn on full speed ahead----
  data.table::setDTthreads(threads = max(1L, parallel::detectCores() - 2L))
  
  # Ensure H2O is installed----
  if(!requireNamespace("h2o")) return("Install H2O to run this function")
  
  # Ensure data is a data.table----
  if(!data.table::is.data.table(data)) data.table::setDT(data)
  
  # Ensure data is a data.table----
  if(!is.null(TestData)) if(!data.table::is.data.table(TestData)) data.table::setDT(TestData)
  
  # Initialize H2O----
  h2o::h2o.init(max_mem_size = MaxMem, nthreads = NThreads, enable_assertions = FALSE)
  
  # Ensure Characters are Converted to Factors----
  data <- ModelDataPrep(data, Impute = FALSE, CharToFactor = TRUE)
  
  # Convert chars to factors----
  if(!is.null(TestData)) TestData <- ModelDataPrep(TestData, Impute = FALSE, CharToFactor = TRUE)
  
  # Convert data to H2O Frame----
  Data <- h2o::as.h2o(data)
  
  # Convert data to H2O Frame----
  if(!is.null(TestData)) TestDataH2O <- h2o::as.h2o(TestData)
  
  # Build Isolation Forest----
  if(is.null(ColumnNumbers)) {
    IsolationForest <- h2o::h2o.isolationForest(
      training_frame = Data,
      x = names(data),
      model_id = "test",
      ntrees = NTrees,
      sample_rate = SampleRate)
  } else {
    IsolationForest <- h2o::h2o.isolationForest(
      training_frame = Data,
      x = names(data)[ColumnNumbers],
      model_id = "test",
      ntrees = NTrees,
      sample_rate = SampleRate)
  }
  
  # Generate Outliers data.table----
  if(!is.null(TestData)) OutliersRawTest <- data.table::as.data.table(h2o::h2o.predict(object = IsolationForest, newdata = TestDataH2O))
  OutliersRaw <- data.table::as.data.table(h2o::h2o.predict(object = IsolationForest, newdata = Data))
  
  # Shutdown H2O
  h2o::h2o.shutdown(prompt = FALSE)
  
  # Add column for outlier indicator----
  data.table::setnames(OutliersRaw, c("predict", "mean_length"), c("PredictIsoForest", "MeanLength"))
  Cutoff <- quantile(OutliersRaw[["PredictIsoForest"]], probs = Threshold)[[1]]
  data.table::set(OutliersRaw, j = "PredictedOutlier", value = data.table::fifelse(OutliersRaw[["PredictIsoForest"]] > Cutoff, 1, 0))
  data.table::set(OutliersRaw, j = "PercentileRank", value = percRank(OutliersRaw[["PredictIsoForest"]]))
  data.table::setcolorder(OutliersRaw, c(4L, 3L, 1L, 2L))
  
  # TestData----
  if(!is.null(TestData)) {
    data.table::setnames(OutliersRawTest, c("predict", "mean_length"), c("PredictIsoForest", "MeanLength"))
    Cutoff <- quantile(OutliersRawTest[["PredictIsoForest"]], probs = Threshold)[[1]]
    data.table::set(OutliersRawTest, j = "PredictedOutlier", value = data.table::fifelse(OutliersRawTest[["PredictIsoForest"]] > Cutoff, 1, 0))
    data.table::set(OutliersRawTest, j = "PercentileRank", value = percRank(OutliersRawTest[["PredictIsoForest"]]))
    data.table::setcolorder(OutliersRawTest, c(4L, 3L, 1L, 2L))
  }
  
  # Merge back with source data----
  OutputData <- cbind(OutliersRaw, data)
  if(!is.null(TestData)) OutputDataTest <- cbind(OutliersRawTest, TestData)
  
  # Return data----
  return(list(
    Data     = OutputData[order(-PredictIsoForest)],
    TestData = tryCatch({OutputDataTest[order(-PredictIsoForest)]}, error = function(x) NULL),
    Model    = IsolationForest))
}
