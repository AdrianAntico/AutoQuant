#' AutoDataPartition
#'
#' This function will take your ratings matrix and model and score your data in parallel.
#' @author Adrian Antico and Douglas Pestana
#' @family Feature Engineering
#' @param data Source data to do your partitioning on
#' @param NumDataSets The number of total data sets you want built
#' @param Ratios A vector of values for how much data each data set should get in each split. E.g. c(0.70, 0.20, 0.10)
#' @param PartitionType Set to either "random", "timeseries", or "time". With "random", your data will be paritioned randomly (with stratified sampling if column names are supplied). With "timeseries", you can partition by time with a stratify option (so long as you have an equal number of records for each strata). With "time" you will have data sets generated so that the training data contains the earliest records in time, validation data the second earliest, test data the third earliest, etc.
#' @param StratifyColumnNames Supply column names of categorical features to use in a stratified sampling procedure for partitioning the data. Partition type must be "random" to use this option
#' @param StratifyNumericTarget Supply a column name that is numeric. Use for "random" PartitionType, you can stratify your numeric variable by splitting up based on percRank to ensure a proper allocation of extreme values in your created data sets.
#' @param StratTargetPrecision For "random" PartitionType and when StratifyNumericTarget is not null, precision will be the number of decimals used in the percentile calculation. If you supply a value of 1, deciles will be used. For a value of 2, percentiles will be used. Larger values are supported.
#' @param TimeColumnName Supply a date column name or a name of a column with an ID for sorting by time such that the smallest number is the earliest in time.
#' @return Returns a list of data.tables
#' @examples
#' \donttest{
#' # Create fake data
#' data <- RemixAutoML::FakeDataGenerator(
#'   Correlation = 0.85,
#'   N = 1000,
#'   ID = 2,
#'   ZIP = 0,
#'   AddDate = FALSE,
#'   Classification = FALSE,
#'   MultiClass = FALSE)
#'
#' # Run data partitioning function
#' dataSets <- RemixAutoML::AutoDataPartition(
#'   data,
#'   NumDataSets = 3L,
#'   Ratios = c(0.70,0.20,0.10),
#'   PartitionType = "random",
#'   StratifyColumnNames = NULL,
#'   StratifyNumericTarget = NULL,
#'   StratTargetPrecision = 1L,
#'   TimeColumnName = NULL)
#'
#' # Collect data
#' TrainData <- dataSets$TrainData
#' ValidationData <- dataSets$ValidationData
#' TestData <- dataSets$TestData
#' }
#' @export
AutoDataPartition <- function(data,
                              NumDataSets = 3L,
                              Ratios = c(0.70, 0.20, 0.10),
                              PartitionType = "random",
                              StratifyColumnNames = NULL,
                              StratifyNumericTarget = NULL,
                              StratTargetPrecision = 3L,
                              TimeColumnName = NULL) {

  # data.table optimize----
  if(parallel::detectCores() > 10) data.table::setDTthreads(threads = max(1L, parallel::detectCores() - 2L)) else data.table::setDTthreads(threads = max(1L, parallel::detectCores()))

  # Arguments----
  if(NumDataSets < 0) return("NumDataSets needs to be a positive integer. Typically 3 modeling sets are used.")
  if(!is.null(StratifyNumericTarget)) {
    if(!is.character(StratifyNumericTarget)) return("StratifyNumericTarget is your target column name in quotes")
    if(!is.numeric(StratTargetPrecision)) return("StratTargetPrecision needs to be values of 1,2,...,N")
  }
  if(abs(round(NumDataSets) - NumDataSets) > 0.01) return("NumDataSets needs to be an integer valued positive number")
  if(length(Ratios) != NumDataSets) return("You need to supply the percentage of data used for each data set.")
  if(sum(Ratios) != 1.0) return("The sum of Ratios needs to equal 1.0")
  if(!(tolower(PartitionType) %chin% c("random", "time", "timeseries"))) return("PartitionType needs to be either 'random', 'timeseries' or 'time'.")
  if(!is.null(StratifyColumnNames)) {
    if(!is.character(StratifyColumnNames)) return("StratifyColumnNames needs to be a character vector of column names")
    if(!all(StratifyColumnNames %chin% names(data))) return("StratifyColumnNames not in vector of data names")
  }
  if(!is.null(TimeColumnName)) {
    if(!(TimeColumnName %chin% names(data))) return("TimeColumnName not in vector of data names")
    if(is.character(data[[eval(TimeColumnName)]]) | is.factor(data[[eval(TimeColumnName)]])) return("TimeColumnName is not a data, Posix_, numeric, or integer valued column")
  }

  # Ensure data.table----
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  # Stratify Numeric Target----
  if(PartitionType == "random") {
    if(!is.null(StratifyNumericTarget)) {
      data[, StratCol := as.factor(round(percRank(data[[eval(StratifyNumericTarget)]]), StratTargetPrecision))]
      StratifyColumnNames <- "StratCol"
    }
  }

  # Partition Steps----
  if(tolower(PartitionType) == "time") {

    # Data prep----
    copy_data <- data.table::copy(data)
    DataCollect <- list()
    if(!is.null(StratifyColumnNames)) keep <- c(eval(StratifyColumnNames))

    # Modify ratios to account for data decrements----
    RatioList <- c()
    RatioList[NumDataSets] <- Ratios[NumDataSets]
    for(i in rev(seq_len(NumDataSets - 1L))) {
      tempRatio <- 0
      for(j in (i + 1L):NumDataSets) tempRatio <- Ratios[j] + tempRatio
      RatioList[i] <- Ratios[i] * (1 / (1 - tempRatio))
    }

    # Gather Row Numbers----
    RowList <- list()
    for(i in NumDataSets:1L) {
      if(!is.null(StratifyColumnNames)) {
        if(i == 1L) {
          temp <- copy_data
        } else {
          x <- copy_data[, .I[sample(.N, max(1L, .N * RatioList[i]))], by = list(get(keep))]$V1
          RowList[[i]] <- x
          copy_data <- copy_data[-x]
        }
      } else {
        if(i == 1L) {
          temp <- copy_data
        } else {
          x <- copy_data[, .I[sample(.N, .N * RatioList[i])]]
          RowList[[i]] <- x
          copy_data <- copy_data[-x]
        }
      }
    }

    # Partition Data----
    for(i in seq_len(NumDataSets)) {
      if(i == 1L) {
        DataCollect[["TrainData"]] <- temp
      } else if(i == 2L) {
        DataCollect[["ValidationData"]] <- data[eval(RowList[[i]])]
      } else if(i == 3L) {
        DataCollect[["TestData"]] <- data[RowList[[i]]]
      } else {
        DataCollect[[paste0("TestData", NumDataSets - 2L)]] <- data[RowList[[i]]]
      }
    }

    # Remove StratCol from StratifyNumericTarget----
    if(PartitionType == "random") {
      if(!is.null(StratifyNumericTarget)) {
        x1 <- DataCollect$TrainData
        x1[, StratCol := NULL]
        x2 <- DataCollect$ValidationData
        x2[, StratCol := NULL]
        x3 <- DataCollect$TestData
        x3[, StratCol := NULL]
        DataCollect$TrainData <- x1
        DataCollect$Validation <- x2
        DataCollect$TestData <- x3
      }
    }

  } else if(tolower(PartitionType) == "timeseries" & !is.null(StratifyColumnNames)) {

    # Initalize collection----
    DataCollect <- list()
    data[, ID := 1:.N, by = c(eval(StratifyColumnNames))]
    if(var(data[, mean(ID), by = c(eval(StratifyColumnNames))][["V1"]]) != 0) return("There are an unequal number of records by strata. PartitionType 'timeseries' requires equal number of observations for each strata")
    Rows <- data[, .N, by = c(eval(StratifyColumnNames))][1, N]

    # Figure out which rows go to which data set
    for(i in NumDataSets:1L) {
      if(i == 1L) {
        DataCollect[["TrainData"]] <- data
      } else if(i == 2L) {
        RowEnd <- data[, .N, by = c(eval(StratifyColumnNames))][1L, N]
        NumRows <- floor(Ratios[i] * Rows)
        DataCollect[["ValidationData"]] <- data[ID %in% (RowEnd - NumRows + 1L):RowEnd]
        DataCollect[["ValidationData"]] <- DataCollect[["ValidationData"]][, ID := NULL]
        data <- data[!(ID %in% (RowEnd - NumRows + 1L):RowEnd)][, ID := NULL]
      } else if(i == 3L) {
        RowEnd <- data[, .N, by = c(eval(StratifyColumnNames))][1L, N]
        NumRows <- floor(Ratios[i] * Rows)
        DataCollect[["TestData"]] <- data[ID %in% (RowEnd - NumRows + 1L):RowEnd]
        DataCollect[["TestData"]] <- DataCollect[["TestData"]][, ID := NULL]
        data <- data[!(ID %in% (RowEnd - NumRows + 1L):RowEnd)]
      } else {
        RowEnd <- data[, .N, by = c(eval(StratifyColumnNames))][1L, N]
        NumRows <- floor(Ratios[i] * Rows)
        DataCollect[[paste0("TestData", NumDataSets - 2L)]] <- data[ID %in% (RowEnd - NumRows + 1L):RowEnd]
        DataCollect[[paste0("TestData", NumDataSets - 2L)]] <- DataCollect[[paste0("TestData", NumDataSets - 2L)]][, ID := NULL]
        data <- data[!(ID %in% (RowEnd - NumRows + 1L):RowEnd)]
      }
    }
  } else {

    # Initialize DataCollect
    DataCollect <- list()
    data <- data[order(runif(.N))]
    Rows <- data[, .N]

    # Figure out which rows go to which data set
    for(i in rev(seq_len(NumDataSets))) {
      if(i == 1L) {
        DataCollect[["TrainData"]] <- data
      } else if(i == 2L) {
        RowEnd <- data[, .N]
        NumRows <- floor(Ratios[i] * Rows)
        DataCollect[["ValidationData"]] <- data[(RowEnd - NumRows + 1L):RowEnd]
        data <- data[-((RowEnd - NumRows + 1L):RowEnd)]
      } else if(i == 3L) {
        RowEnd <- data[, .N]
        NumRows <- floor(Ratios[i] * Rows)
        DataCollect[["TestData"]] <- data[(RowEnd - NumRows + 1L):RowEnd]
        data <- data[-((RowEnd - NumRows + 1L):RowEnd)]
      } else {
        RowEnd <- data[, .N]
        NumRows <- floor(Ratios[i] * Rows)
        DataCollect[[paste0("TestData", NumDataSets - 2L)]] <- data[(RowEnd - NumRows + 1L):RowEnd]
        data <- data[-((RowEnd - NumRows + 1L):RowEnd)]
      }
    }
  }

  # Return data----
  return(DataCollect)
}
