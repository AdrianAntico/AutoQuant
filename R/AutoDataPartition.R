#' @title AutoDataPartition
#'
#' @description This function will take your ratings matrix and model and score your data in parallel.
#'
#' @author Adrian Antico
#' @family Feature Engineering
#'
#' @param data Source data to do your partitioning on
#' @param NumDataSets The number of total data sets you want built
#' @param Ratios A vector of values for how much data each data set should get in each split. E.g. c(0.70, 0.20, 0.10)
#' @param PartitionType Set to either "random", "timeseries", or "time". With "random", your data will be paritioned randomly (with stratified sampling if column names are supplied). With "timeseries", you can partition by time with a stratify option (so long as you have an equal number of records for each strata). With "time" you will have data sets generated so that the training data contains the earliest records in time, validation data the second earliest, test data the third earliest, etc.
#' @param StratifyColumnNames Supply column names of categorical features to use in a stratified sampling procedure for partitioning the data. Partition type must be "random" to use this option
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
                              TimeColumnName = NULL) {

  # Arguments ----
  if(length(Ratios) != NumDataSets) stop("You need to supply the percentage of data used for each data set.")
  if(sum(Ratios) != 1.0) stop("The sum of Ratios needs to equal 1.0")
  if(!(tolower(PartitionType) %chin% c("random", "time", "timeseries"))) stop("PartitionType needs to be either 'random', 'timeseries' or 'time'.")

  # Ensure data.table ----
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  # Partition Steps ----
  if(tolower(PartitionType) %in% c("time","random")) {

    # Sort data
    if(tolower(PartitionType) == "time") {
      data.table::setorderv(x = data, cols = TimeColumnName, order = 1)
      StratifyColumnNames <- NULL
    }

    # Data prep----
    copy_data <- data.table::copy(data)
    DataCollect <- list()
    if(!is.null(StratifyColumnNames)) {
      keep <- StratifyColumnNames
      Check1 <- is.numeric(copy_data[[StratifyColumnNames]]) && length(unique(copy_data[[StratifyColumnNames]])) > 20
    }

    # Modify ratios to account for data decrements ----
    RatioList <- c()
    RatioList[NumDataSets] <- Ratios[NumDataSets]
    for(i in rev(seq_len(NumDataSets - 1L))) {
      tempRatio <- 0
      for(j in (i + 1L):NumDataSets) tempRatio <- Ratios[j] + tempRatio
      RatioList[i] <- Ratios[i] * (1 / (1 - tempRatio))
    }

    # Gather Row Numbers ----
    RowList <- list()
    if(!is.null(StratifyColumnNames)) {
      if(length(StratifyColumnNames) > 1) {
        copy_data[, rank := do.call(paste, c(.SD, sep = " ")), .SDcols = c(StratifyColumnNames)]
      } else {
        if(Check1) {
          copy_data[, rank := round(data.table::frank(get(keep)) * 20 / .N) * 1/20]
        } else {
          data.table::setnames(copy_data, StratifyColumnNames, "rank")
        }
      }
    }
    for(i in NumDataSets:1L) {
      N <- copy_data[, .N]
      if(!is.null(StratifyColumnNames)) {
        if(i == 1L) {
          temp <- copy_data
        } else {
          x <- copy_data[, .I[sample(.N, max(1L, round(.N * RatioList[i])))], by = list(rank)]$V1
          RowList[[i]] <- x
          copy_data <- copy_data[-x]
        }
      } else {
        if(i == 1L) {
          temp <- copy_data
        } else {
          x <- seq(floor(N * (1 - RatioList[i])), N)
          RowList[[i]] <- x
          copy_data <- copy_data[-x]
        }
      }
    }

    # Partition Data ----
    for(i in rev(seq_len(NumDataSets))) {
      if(i == 1L) {
        DataCollect[["TrainData"]] <- data
      } else if(i == 2L) {
        DataCollect[["ValidationData"]] <- data[RowList[[i]]]
        data <- data[-RowList[[i]]]
      } else if(i == 3L) {
        DataCollect[["TestData"]] <- data[RowList[[i]]]
        data <- data[-RowList[[i]]]
      } else {
        DataCollect[[paste0("TestData", NumDataSets - 2L)]] <- data[RowList[[i]]]
        data <- data[-RowList[[i]]]
      }
    }

  } else if(tolower(PartitionType) == "timeseries" & !is.null(StratifyColumnNames)) {

    # Initalize collection----
    DataCollect <- list()
    data[, ID := 1:.N, by = c(eval(StratifyColumnNames))]
    if(var(data[, sum(ID), by = c(eval(StratifyColumnNames))][["V1"]]) != 0) {
      data[, ID := NULL]
      stop("There are an unequal number of records by strata. PartitionType 'timeseries' requires equal number of observations for each strata")
    }
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
  }

  # Return data ----
  if(!is.null(StratifyColumnNames)) {
    if(length(StratifyColumnNames) > 1) {
      for(g in seq_along(DataCollect)) if("rank" %chin% names(DataCollect[[g]])) data.table::setnames(DataCollect[[g]], "rank", StratifyColumnNames)
    } else if(Check1) {
      for(g in seq_along(DataCollect)) if("rank" %chin% names(DataCollect[[g]])) DataCollect[[g]][, rank := NULL]
    }
  }
  return(DataCollect)
}
