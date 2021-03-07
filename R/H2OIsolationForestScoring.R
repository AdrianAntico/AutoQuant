#' @title H2OIsolationForestScoring
#'
#' @description H2OIsolationForestScoring for dimensionality reduction and / or anomaly detection scoring on new data
#'
#' @author Adrian Antico
#' @family Unsupervised Learning
#'
#' @param data The data.table with the columns you wish to have analyzed
#' @param Features A character vector with the column names to utilize in the isolation forest
#' @param IDcols A character vector with the column names to not utilize in the isolation forest but have returned with the data output. Otherwise those columns will be removed
#' @param H2OStart TRUE to have H2O started inside function
#' @param H2OShutdown TRUE to shutdown H2O inside function
#' @param ModelID Name for model that gets saved to file if SavePath is supplied and valid
#' @param SavePath Path directory to store saved model
#' @param Threshold Quantile value to find the cutoff value for classifying outliers
#' @param MaxMem Specify the amount of memory to allocate to H2O. E.g. "28G"
#' @param NThreads Specify the number of threads (E.g. cores * 2)
#' @param Debug Debugging
#' @examples
#' \dontrun{
#' # Create simulated data
#' data <- RemixAutoML::FakeDataGenerator(
#'   Correlation = 0.70,
#'   N = 50000,
#'   ID = 2L,
#'   FactorCount = 2L,
#'   AddDate = TRUE,
#'   ZIP = 0L,
#'   TimeSeries = FALSE,
#'   ChainLadderData = FALSE,
#'   Classification = FALSE,
#'   MultiClass = FALSE)
#'
#' # Run algo
#' data <- RemixAutoML::H2OIsolationForest(
#'   data,
#'   Features = names(data)[2L:ncol(data)],
#'   IDcols = c("Adrian", "IDcol_1", "IDcol_2"),
#'   ModelID = "Adrian",
#'   SavePath = getwd(),
#'   Threshold = 0.95,
#'   MaxMem = "28G",
#'   NThreads = -1,
#'   NTrees = 100,
#'   SampleRate = (sqrt(5)-1)/2,
#'   MaxDepth = 8,
#'   MinRows = 1,
#'   ColSampleRate = 1,
#'   ColSampleRatePerLevel = 1,
#'   ColSampleRatePerTree = 1,
#'   CategoricalEncoding = c("AUTO"),
#'   Debug = TRUE)
#'
#' # Remove output from data and then score
#' data[, eval(names(data)[17:ncol(data)]) := NULL]
#'
#' # Run algo
#' Outliers <- RemixAutoML::H2OIsolationForestScoring(
#'   data,
#'   Features = names(data)[2:ncol(data)],
#'   IDcols = c("Adrian", "IDcol_1", "IDcol_2"),
#'   H2OStart = TRUE,
#'   H2OShutdown = TRUE,
#'   ModelID = "TestModel",
#'   SavePath = getwd(),
#'   Threshold = 0.95,
#'   MaxMem = "28G",
#'   NThreads = -1,
#'   Debug = FALSE)
#' }
#' @return Source data.table with predictions. Note that any columns not listed in Features nor IDcols will not be returned with data. If you want columns returned but not modeled, supply them as IDcols
#' @export
H2OIsolationForestScoring <- function(data,
                                      Features = NULL,
                                      IDcols = NULL,
                                      H2OStart = TRUE,
                                      H2OShutdown = TRUE,
                                      ModelID = "TestModel",
                                      SavePath = NULL,
                                      Threshold = 0.975,
                                      MaxMem = "28G",
                                      NThreads = -1,
                                      Debug = FALSE) {

  # Arg checks ----
  if(!is.null(SavePath) && !dir.exists(SavePath)) stop("SavePath is not a valid directory")
  if(!data.table::is.data.table(data)) data.table::setDT(data)
  if(!is.null(IDcols) && !is.character(IDcols)) stop("IDcols needs to be a character scalar or vector")
  if(!is.null(ModelID) && !is.character(ModelID)) stop("ModelID needs to be a character scalar or vector")
  if(!is.null(Features) && !is.character(ModelID)) stop("Features needs to be a character scalar or vector")
  if(!is.null(SavePath) && !is.character(SavePath)) stop("SavePath needs to be a character scalar or vector")
  if(!is.null(SavePath) && is.character(SavePath) && !dir.exists(SavePath)) warning("SavePath directory did not exist but one was made")

  # Get date col names if exist ----
  ID <- IDcols
  for(i in seq_len(length(names(data)))) {
    if(any(class(data[[i]]) %in% c("Date","POSIXct","IDate","IDateTime"))) {
      Features <- Features[!Features %in% names(data)[i]]
      ID <- c(ID, names(data)[i])
    }
    if(names(data)[i] %chin% ID) {
      Features <- Features[!Features %in% names(data)[i]]
    }
  }

  # Unique ----
  Features <- unique(Features)
  ID <- unique(ID)

  # Subset ID ----
  if(!is.null(ID) && (length(ID) + length(Features) == length(names(data)))) {
    IDcolData <- data[, .SD, .SDcols = c(ID)]
    data[, (ID) := NULL]
  } else if(!is.null(ID) && (length(ID) + length(Features) != length(names(data)))) {
    ID <- c(ID, setdiff(names(data), c(Features, ID)))
    IDcolData <- data[, .SD, .SDcols = c(ID)]
    data[, (ID) := NULL]
  }

  # Ensure Characters are Converted to Factors ----
  data <- ModelDataPrep(
    data = data,
    Impute = TRUE,
    CharToFactor = TRUE,
    FactorToChar = FALSE,
    IntToNumeric = TRUE,
    LogicalToBinary = TRUE,
    DateToChar = FALSE,
    IDateConversion = FALSE,
    RemoveDates = FALSE,
    MissFactor = "0",
    MissNum = -1,
    IgnoreCols = NULL)

  # Debug
  if(Debug) print(str(data))
  if(Debug) print(str(IDcolData))
  if(Debug) print(Features)

  # Prepare H2O ----
  if(H2OStart) localH2O <- h2o::h2o.init(nthreads = NThreads, max_mem_size = MaxMem, enable_assertions = FALSE)
  H2O_Data <- h2o::as.h2o(data)
  ModelObject <- h2o::h2o.loadModel(path = file.path(SavePath, ModelID))

  # Generate Outliers data.table ----
  OutliersRaw <- data.table::as.data.table(h2o::h2o.predict(object = ModelObject, newdata = H2O_Data))
  rm(H2O_Data, ModelObject)

  # Shutdown h2o----
  if(H2OShutdown) h2o::h2o.shutdown(prompt = FALSE)

  # Add column for outlier indicator ----
  data.table::setnames(OutliersRaw, c("predict", "mean_length"), c("PredictIsoForest", "MeanLength"))
  Cutoff <- quantile(OutliersRaw[["PredictIsoForest"]], probs = Threshold)[[1L]]
  OutliersRaw[, PredictedOutlier := data.table::fifelse(PredictIsoForest > eval(Cutoff), 1, 0)]
  OutliersRaw[, Rank := data.table::frank(PredictIsoForest) / .N]
  data.table::setcolorder(OutliersRaw, c(4L, 3L, 1L, 2L))

  # Merge back with source data ----
  data <- cbind(data, OutliersRaw)

  # Merge data back with IDcolData ----
  if(exists("IDcolData")) data <- cbind(IDcolData, data)

  # Return data ----
  return(data)
}
