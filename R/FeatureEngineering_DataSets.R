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
      if(!is.null(StratifyColumnNames)) {
        OrderLength <- length(c(TimeColumnName, StratifyColumnNames))
        data.table::setorderv(x = data, cols = c(TimeColumnName, StratifyColumnNames), order = rep(1,OrderLength))
      } else {
        data.table::setorderv(x = data, cols = TimeColumnName, order = 1)
      }
      StratifyColumnNames <- NULL
    } else {
      data <- data[order(runif(.N))]
    }

    # Data prep----
    copy_data <- data.table::copy(data)
    DataCollect <- list()
    if(!is.null(StratifyColumnNames)) {
      keep <- StratifyColumnNames
      Check1 <- is.numeric(copy_data[[StratifyColumnNames[1L]]]) && length(unique(copy_data[[StratifyColumnNames]])) > 20
    }

    # Modify ratios to account for data decrementing ----
    RatioList <- c()
    RatioList[NumDataSets] <- Ratios[NumDataSets]
    for(i in rev(seq_len(NumDataSets - 1L))) {
      tempRatio <- 0
      for(j in (i + 1L):NumDataSets) tempRatio <- Ratios[j] + tempRatio
      RatioList[i] <- Ratios[i] * (1 / (1 - tempRatio))
    }

    # Stratification management ----
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

    # Gather row numbers ----
    RowList <- list()
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

  } else if(tolower(PartitionType) == "timeseries" && !is.null(StratifyColumnNames)) {

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
    } else if(PartitionType != "timeseries" && Check1) {
      for(g in seq_along(DataCollect)) if("rank" %chin% names(DataCollect[[g]])) DataCollect[[g]][, rank := NULL]
    }
  }
  return(DataCollect)
}

#' @title ModelDataPrep
#'
#' @description This function replaces inf values with NA, converts characters to factors, and imputes with constants
#'
#' @author Adrian Antico
#' @family Feature Engineering
#'
#' @param data This is your source data you'd like to modify
#' @param Impute Defaults to TRUE which tells the function to impute the data
#' @param CharToFactor Defaults to TRUE which tells the function to convert characters to factors
#' @param FactorToChar Converts to character
#' @param IntToNumeric Defaults to TRUE which tells the function to convert integers to numeric
#' @param DateToChar Converts date columns into character columns
#' @param IDateConversion Convert IDateTime to POSIXct and IDate to Date types
#' @param LogicalToBinary Converts logical values to binary numeric values
#' @param RemoveDates Defaults to FALSE. Set to TRUE to remove date columns from your data.table
#' @param MissFactor Supply the value to impute missing factor levels
#' @param MissNum Supply  the value to impute missing numeric values
#' @param IgnoreCols Supply column numbers for columns you want the function to ignore
#' @examples
#' \dontrun{
#' # Create fake data
#' data <- RemixAutoML::FakeDataGenerator(
#'   Correlation = 0.75,
#'   N = 250000L,
#'   ID = 2L,
#'   ZIP = 0L,
#'   FactorCount = 6L,
#'   AddDate = TRUE,
#'   Classification = FALSE,
#'   MultiClass = FALSE)
#'
#' # Check column types
#' str(data)
#'
#' # Convert some factors to character
#' data <- RemixAutoML::ModelDataPrep(
#'   data,
#'   Impute       = TRUE,
#'   CharToFactor = FALSE,
#'   FactorToChar = TRUE,
#'   IntToNumeric = TRUE,
#'   LogicalToBinary = FALSE,
#'   DateToChar   = FALSE,
#'   IDateConversion = FALSE,
#'   RemoveDates  = TRUE,
#'   MissFactor   = "0",
#'   MissNum      = -1,
#'   IgnoreCols   = c("Factor_1"))
#'
#' # Check column types
#' str(data)
#' }
#' @return Returns the original data table with corrected values
#' @export
ModelDataPrep <- function(data,
                          Impute          = TRUE,
                          CharToFactor    = TRUE,
                          FactorToChar    = FALSE,
                          IntToNumeric    = TRUE,
                          LogicalToBinary = FALSE,
                          DateToChar      = FALSE,
                          IDateConversion = FALSE,
                          RemoveDates     = FALSE,
                          MissFactor      = "0",
                          MissNum         = -1,
                          IgnoreCols      = NULL) {

  # Check data.table ----
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  # Prepare columns for action ----
  x <- as.integer(seq_along(data))
  if(!is.null(IgnoreCols)) if(class(IgnoreCols)[1L] == "character") x <- setdiff(x, which(names(data) %chin% IgnoreCols)) else x <- setdiff(x, IgnoreCols)

  # Replace any inf values with NA ----
  for(col in x) data.table::set(data, j = col, value = replace(data[[col]], is.infinite(data[[col]]), NA))

  # Turn character columns into factors----
  if(CharToFactor) for(col in x) if(is.character(data[[col]])) data.table::set(data, j = col, value = as.factor(data[[col]]))

  # Convert IDate to Date and IDateTime to POSIXct ----
  if(IDateConversion) {
    for(col in x) if(any(class(data[[col]]) %chin% "IDateTime")) data.table::set(data, j = col, value = as.POSIXct(data[[col]]))
    for(col in x) if(any(class(data[[col]]) %chin% "IDate")) data.table::set(data, j = col, value = as.Date(data[[col]]))
  }

  # Turn factor columns into character ----
  if(FactorToChar) for(col in x) if(is.factor(data[[col]])) data.table::set(data, j = col, value = as.character(data[[col]]))

  # Turn integers columns into numeric ----
  if(IntToNumeric) for(col in x) if(is.integer(data[[col]])) data.table::set(data, j = col, value = as.numeric(data[[col]]))

  # Turn logical columns into numeric ----
  if(IntToNumeric && !LogicalToBinary) LogicalToBinary <- TRUE # backwards compatability
  if(LogicalToBinary) for(col in x) if(is.logical(data[[col]])) data.table::set(data, j = col, value = as.numeric(data[[col]]))

  # Impute missing values ----
  if(Impute) {
    numcols <- names(data)[which(sapply(data, is.numeric))]
    xx <- setdiff(names(data), numcols)
    for(col in xx) {
      if(is.factor(data[[col]])) {
        data.table::set(data, which(!(data[[col]] %in% levels(data[[col]]))), col, MissFactor)
      } else if(is.character(data[[col]])) {
        data.table::set(data, which(is.na(data[[col]])), col, MissFactor)
      }
    }
    if(!identical(character(0), numcols)) data.table::setnafill(x = data, cols = numcols, type = "const", fill = MissNum)
  }

  # Remove Dates ----
  if(RemoveDates || DateToChar) {
    for(col in rev(x)) {
      if(!is.character(data[[col]]) && !is.factor(data[[col]]) && !is.numeric(data[[col]]) && !is.integer(data[[col]]) && !is.logical(data[[col]]) && !is.complex(data[[col]])) {
        if(DateToChar) {
          data.table::set(data, j = paste0(names(data)[col]), value = as.character(data[[eval(col)]]))
        } else {
          data.table::set(data, j = paste0(names(data)[col]), value = NULL)
        }
      }
    }
  }

  # Return data ----
  return(data)
}

#' @title PartitionData
#'
#' @description Create data sets for machine learning
#'
#' @author Adrian Antico
#' @family Feature Engineering - DataSet
#'
#' @param data Source data
#' @param ArgsList ArgsList
#'
#' @examples
#' \dontrun{
#' Output <- RemixAutoML:::PartitionData(
#'   data = data,
#'   ArgsList = ArgsList)
#' TrainData <- Output$TrainData
#' ArgsList <- Output$ArgsList
#' }
#'
#' @return A list containing the data and the ArgsList
#' @noRd
PartitionData <- function(data = NULL,
                          ArgsList = ArgsList) {

  # Metadata
  Start <- Sys.time()

  # Run function
  DataSets <- RemixAutoML::AutoDataPartition(
    data = data,
    NumDataSets = length(ArgsList[['FE_Args']][['Partition']][['PartitionRatios']]),
    Ratios = ArgsList[['FE_Args']][['Partition']][['PartitionRatios']],
    PartitionType = ArgsList[['FE_Args']][['Partition']][['PartitionMethod']],
    StratifyColumnNames = ArgsList[['FE_Args']][['Partition']][['PartitionByVariables']],
    TimeColumnName = ArgsList[['FE_Args']][['Partition']][['PartitionTimeColumnName']])

  # Collect data
  TrainData <- DataSets[['TrainData']]; DataSets[['TrainData']] <- NULL
  ValidationData <- DataSets[['ValidationData']]; DataSets[['ValidationData']] <- NULL
  TestData <- DataSets[['TestData']]; DataSets[['TestData']] <- NULL

  # Run time tracking
  End <- Sys.time()
  ArgsList[['RunTime']][['PartitionData']] <- difftime(End, Start, units = "mins")

  # Return
  return(list(TrainData = TrainData, ValidationData = ValidationData, TestData = TestData, ArgsList = ArgsList))
}
