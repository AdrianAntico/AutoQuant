
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
#' data <- AutoQuant::FakeDataGenerator(
#'   Correlation = 0.85,
#'   N = 1000,
#'   ID = 2,
#'   ZIP = 0,
#'   AddDate = FALSE,
#'   Classification = FALSE,
#'   MultiClass = FALSE)
#'
#' # Run data partitioning function
#' dataSets <- AutoQuant::AutoDataPartition(
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
      if(length(StratifyColumnNames) > 1 && StratifyColumnNames %in% names(copy_data)) {
        copy_data[, rank := do.call(paste, c(.SD, sep = " ")), .SDcols = c(StratifyColumnNames)]
      } else {
        if(Check1) {
          copy_data[, rank := round(data.table::frank(get(keep)) * 20 / .N) * 1/20]
        } else {
          data.table::setnames(copy_data, StratifyColumnNames, "rank", skip_absent = TRUE)
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

    TVT <- list()
    rll <- length(RowList)
    if(rll >= 3L) {
      TVT$Train <- setdiff(seq_len(nrow(data)), c(RowList[[2L]],RowList[[3L]]))
      TVT$Valid <- RowList[[2L]]
      TVT$Test <- RowList[[3L]]
    } else if(rll == 2L) {
      TVT$Train <- setdiff(seq_len(nrow(data)), c(RowList[[2L]]))
      TVT$Valid <- RowList[[2L]]
    } else {
      TVT$Train <- seq_len(nrow(data))
    }
    DataCollect[['TVT']] <- TVT

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

  # DataCollect$TVT <- RowList
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
#' data <- AutoQuant::FakeDataGenerator(
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
#' data <- AutoQuant::ModelDataPrep(
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
#' Output <- AutoQuant:::PartitionData(
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
  DataSets <- AutoQuant::AutoDataPartition(
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

#' @title TimeSeriesFill
#'
#' @description TimeSeriesFill For Completing Time Series Data For Single Series or Time Series by Group
#'
#' @family Feature Engineering
#' @author Adrian Antico
#'
#' @param data Supply your full series data set here
#' @param TargetColumn = NULL
#' @param DateColumnName Supply the name of your date column
#' @param GroupVariables Supply the column names of your group variables. E.g. "Group" or c("Group1","Group2")
#' @param TimeUnit Choose from "second", "minute", "hour", "day", "week", "month", "quarter", "year"
#' @param FillType Choose from maxmax - Fill from the absolute min date to the absolute max date, minmax - Fill from the max date of the min set to the absolute max date, maxmin - Fill from the absolute min date to the min of the max dates, or minmin - Fill from the max date of the min dates to the min date of the max dates
#' @param MaxMissingPercent The maximum amount of missing values an individual series can have to remain and be imputed. Otherwise, they are discarded.
#' @param SimpleImpute Set to TRUE or FALSE. With TRUE numeric cols will fill NAs with a 0 and non-numeric cols with a "0"
#' @examples
#' \dontrun{
#'
#' # Pull in data
#' data <- data.table::fread("https://www.dropbox.com/s/2str3ek4f4cheqi/walmart_train.csv?dl=1")
#'
#' # Run function
#' data <- TimeSeriesFill(
#'   data,
#'   DateColumnName = "Date",
#'   GroupVariables = c("Store","Dept"),
#'   TimeUnit = "weeks",
#'   FillType = "maxmax",
#'   SimpleImpute = FALSE)
#'
#' # data <- data.table::fread("https://www.dropbox.com/s/2str3ek4f4cheqi/walmart_train.csv?dl=1")
#' # DateColumnName = "Date"
#' # GroupVariables = c("Store","Dept")
#' # TimeUnit = "weeks"
#' # FillType = "maxmax" # "minmin" # "maxmin" # "dynamic:method" # "minmax" #
#' # SimpleImpute = FALSE
#'
#' }
#' @return Returns a data table with missing time series records filled (currently just zeros)
#' @export
TimeSeriesFill <- function(data = NULL,
                           TargetColumn = NULL,
                           DateColumnName = NULL,
                           GroupVariables = NULL,
                           TimeUnit = 'days',
                           FillType = "maxmax",
                           MaxMissingPercent = 0.05,
                           SimpleImpute = FALSE) {

  # Grab args
  if(length(FillType) > 1L) FillType <- FillType[1L]

  # Set up list
  CJList <- list()

  # Check date type
  if(class(data[[DateColumnName]])[1L] %in% c('numeric','integer')) {
    data[, eval(DateColumnName) := as.Date(as.character(get(DateColumnName)), format = "%Y%m%d")]
  }

  # Fill other
  x <- tryCatch({substr(x = FillType[1L], start = 1L, stop = 7L)}, error = function(x) NULL)
  if(length(x) > 0L && tolower(x) == "dynamic") {

    # Fill
    data <- data[, .SD, .SDcols = c(TargetColumn,DateColumnName,GroupVariables)]
    FillData <- Rappture::FE.Impute.TimeSeriesFill(data,TargetColumn,DateColumnName,TimeUnit,FillType,CJList,GroupVariables = GroupVariables)
    return(FillData)
  }

  # Fill from the absolute min date to the absolute max date
  if(FillType == "maxmax") {

    # Date variables
    MinDate <- data[, min(get(DateColumnName))]
    MaxDate <- data[, max(get(DateColumnName))]
    if(any(TimeUnit %chin% c("1min","5min","10min","15min","30min","45min"))) {
      CJList[[eval(DateColumnName)]] <- seq(from = MinDate, to = MaxDate, by = 60 * as.numeric(gsub("([0-9]+).*$", "\\1", TimeUnit)))
    } else {
      CJList[[eval(DateColumnName)]] <- seq(from = MinDate, to = MaxDate, by = TimeUnit)
    }

    # Group variables
    if(!is.null(GroupVariables)) {
      for(group in GroupVariables) {
        CJList[[eval(group)]] <- unique(data[[eval(group)]])
      }
    }

    # Cross join and then merge back original features
    FillData <- do.call(data.table::CJ, CJList)
    FillData <- merge(FillData, data, by = c(DateColumnName, GroupVariables), all.x = TRUE)
  }

  # Fill from the max date of the min set to the absolute max date
  if(FillType == "minmax") {

    # Date variables
    MinDate <- data[, min(get(DateColumnName)), by = c(eval(GroupVariables))][, max(V1)]
    MaxDate <- data[, max(get(DateColumnName))]
    if(any(TimeUnit %chin% c("1min","5min","10min","15min","30min","45min"))) {
      CJList[[eval(DateColumnName)]] <- seq(from = MinDate, to = MaxDate, by = 60 * as.numeric(gsub("([0-9]+).*$", "\\1", TimeUnit)))
    } else {
      CJList[[eval(DateColumnName)]] <- seq(from = MinDate, to = MaxDate, by = TimeUnit)
    }

    # Group variable
    if(!is.null(GroupVariables)) {
      for(group in GroupVariables) {
        CJList[[eval(group)]] <- unique(data[[eval(group)]])
      }
    }

    # Cross join and then merge back original features
    FillData <- do.call(data.table::CJ, CJList)
    FillData <- merge(FillData, data, by = c(DateColumnName, GroupVariables), all.x = TRUE)
  }

  # Fill from the absolute min date to the min of the max dates
  if(FillType == "maxmin") {

    # Date variable
    MinDate <- data[, min(get(DateColumnName))]
    MaxDate <- data[, max(get(DateColumnName)), by = c(eval(GroupVariables))][, min(V1)]
    if(any(TimeUnit %chin% c("1min","5min","10min","15min","30min","45min"))) {
      CJList[[eval(DateColumnName)]] <- seq(from = MinDate, to = MaxDate, by = 60 * as.numeric(gsub("([0-9]+).*$", "\\1", TimeUnit)))
    } else {
      CJList[[eval(DateColumnName)]] <- seq(from = MinDate, to = MaxDate, by = TimeUnit)
    }

    # Group variables
    if(!is.null(GroupVariables)) {
      for(group in GroupVariables) {
        CJList[[eval(group)]] <- unique(data[[eval(group)]])
      }
    }

    # Cross join and then merge back original features
    FillData <- do.call(data.table::CJ, CJList)
    FillData <- merge(FillData, data, by = c(DateColumnName, GroupVariables), all.x = TRUE)
  }

  # Fill from the max date of the min dates to the min date of the max dates
  if(FillType == "minmin") {

    # Date variables
    MinDate <- data[, min(get(DateColumnName))]
    MaxDate <- data[, max(get(DateColumnName)), by = c(eval(GroupVariables))][, min(V1)]
    if(any(TimeUnit %chin% c("1min","5min","10min","15min","30min","45min"))) {
      CJList[[eval(DateColumnName)]] <- seq(from = MinDate, to = MaxDate, by = 60 * as.numeric(gsub("([0-9]+).*$", "\\1", TimeUnit)))
    } else {
      CJList[[eval(DateColumnName)]] <- seq(from = MinDate, to = MaxDate, by = TimeUnit)
    }

    # Group variables
    if(!is.null(GroupVariables)) {
      for(group in GroupVariables) {
        CJList[[eval(group)]] <- unique(data[[eval(group)]])
      }
    }

    # Cross join and then merge back original features
    FillData <- do.call(data.table::CJ, CJList)
    FillData <- merge(FillData, data, by = c(DateColumnName, GroupVariables), all.x = TRUE)
  }

  # Remove combinations that never existed
  if(!is.null(GroupVariables)) {
    temp <- unique(data[, mget(GroupVariables)])
    FillData <- merge(FillData, temp, by = GroupVariables, all = FALSE)
    FillData[, Check := sum(!is.na(get(names(FillData)[!names(FillData) %chin% c(eval(GroupVariables),eval(DateColumnName))][1L]))), by = eval(GroupVariables)]
    CompareVal <- FillData[, quantile(Check, 0.95)[[1L]]]
    FillData <- FillData[Check > (1 - eval(MaxMissingPercent)) * eval(CompareVal)][, Check := NULL]
  }

  # Impute
  if(SimpleImpute) {
    FillData <- AutoQuant::ModelDataPrep(
      FillData,
      Impute = TRUE,
      CharToFactor = FALSE,
      FactorToChar = FALSE,
      IntToNumeric = FALSE,
      DateToChar = FALSE,
      RemoveDates = FALSE,
      MissFactor = "0",
      MissNum = 0,
      IgnoreCols = NULL)
  }

  # Return data
  return(FillData)
}

#' @title TimeSeriesFillRoll
#'
#' @description TimeSeriesFillRoll For Completing Time Series Data For Single Series or Time Series by Group
#'
#' @family Feature Engineering
#' @author Adrian Antico
#'
#' @param data Supply your full series data set here
#' @param RollVars = NULL,
#' @param NonRollVars = NULL,
#' @param DateColumnName Supply the name of your date column
#' @param GroupVariables Supply the column names of your group variables. E.g. "Group" or c("Group1","Group2")
#' @param RollDirection 'backward' or 'forward'
#' @param TimeUnit Choose from "second", "minute", "hour", "day", "week", "month", "quarter", "year"
#' @param SimpleImpute Set to TRUE or FALSE. With TRUE numeric cols will fill NAs with a 0 and non-numeric cols with a "0"
#' @examples
#' \dontrun{
#'
#' # Pull in data
#' data <- data <- data.table::fread("https://www.dropbox.com/s/2str3ek4f4cheqi/walmart_train.csv?dl=1")
#'
#' # Run function
#' data <- TimeSeriesFillRoll(
#'   data,
#'   RollVars = c('Net_Revenue', 'Units', 'SIZE_UNITS', 'Liters', 'Accum_Units'),
#'   NonRollVars = c('Diff_1_DATE_ISO','Net_Revenue_PerDay','Liters_PerDay','Units_PerDay'),
#'   DateColumnName = "Date",
#'   GroupVariables = c("Store","Dept"),
#'   RollDirection = 'backward',
#'   TimeUnit = "weeks",
#'   SimpleImpute = FALSE)
#' }
#' @return Returns a data table with missing time series records filled (currently just zeros)
#' @export
TimeSeriesFillRoll <- function(data = NULL,
                               DateColumnName = NULL,
                               RollVars = NULL,
                               NonRollVars = NULL,
                               GroupVariables = NULL,
                               RollDirection = 'backward',
                               TimeUnit = "days",
                               SimpleImpute = FALSE) {

  # Prep
  data <- ModelDataPrep(
    data = data, Impute = TRUE, CharToFactor = FALSE, FactorToChar = TRUE, IntToNumeric = TRUE, LogicalToBinary = TRUE,
    DateToChar = FALSE, IDateConversion = TRUE, RemoveDates = FALSE, MissFactor = 'missing', MissNum = 0.0)

  # Check date type
  if(class(data[[DateColumnName]])[1L] %in% c('numeric','integer')) {
    data[, eval(DateColumnName) := as.Date(as.character(get(DateColumnName)), format = "%Y%m%d")]
  }

  # Fill data then merge originals and then spread originals
  FillData <- data[, list(date = seq(min(get(DateColumnName)), max(get(DateColumnName)), TimeUnit)), by = c(GroupVariables)]
  data.table::setnames(FillData, 'date', DateColumnName)

  # Merge non-rolling vars back
  if(length(NonRollVars) > 0L) FillData <- merge(FillData, data[, .SD, .SDcols = c(GroupVariables,DateColumnName,NonRollVars)], by = c(DateColumnName, GroupVariables), all.x = TRUE)

  # Merge rolling vars back
  if(length(RollVars) > 0L) {
    data.table::setkeyv(FillData, cols = c(GroupVariables,DateColumnName))
    data.table::setkeyv(data, cols = c(GroupVariables,DateColumnName))
    if(tolower(RollDirection) %in% c('backward','backwards','bwd')) {
      FillData <- data[, .SD, .SDcols = c(GroupVariables,DateColumnName,RollVars)][FillData, roll = -Inf]
    } else {
      FillData <- data[, .SD, .SDcols = c(GroupVariables,DateColumnName,RollVars)][FillData, roll = Inf]
    }
  }

  # Impute
  if(SimpleImpute) FillData <- ModelDataPrep(FillData, Impute = TRUE, CharToFactor = FALSE, FactorToChar = FALSE, IntToNumeric = FALSE, DateToChar = FALSE, RemoveDates = FALSE, MissFactor = "0", MissNum = 0, IgnoreCols = NULL)

  # Return data
  return(FillData)
}
