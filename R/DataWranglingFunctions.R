#' @title ColumnSubsetDataTable
#'
#' @description ColumnSubsetDataTable will subset data tables by column
#'
#' @family Data Wrangling
#'
#' @author Adrian Antico
#'
#' @param data data.table
#' @param TargetColumnName Target variable
#' @param DateColumnName Date variable
#' @param GroupVars Group variables
#'
#' @export
ColumnSubsetDataTable <- function(data,
                                  TargetColumnName = NULL,
                                  DateColumnName = NULL,
                                  GroupVars = NULL) {

  # Check to see if data is actual data----
  if(!(any(class(data) %in% c("data.table","data.frame","tibble")))) {
    return(NULL)
  }

  # Subset----
  data <- data[, .SD, .SDcols = c(eval(TargetColumnName),eval(DateColumnName),eval(GroupVars))]

  # Ensure Date Column is Date----
  if(is.character(data[[eval(DateColumnName)]])) {
    x <- data[1,get(DateColumnName)]
    x1 <- lubridate::guess_formats(x, orders = c("mdY","BdY","Bdy","bdY","bdy","mdy","dby","Ymd","Ydm"))
    data[, eval(DateColumnName) := as.Date(get(DateColumnName), tryFormats = x1)]
  }

  # Return data----
  return(data)
}

#' @title DataDisplayMeta
#'
#' @description DataDisplayMeta
#'
#' @author Adrian Antico
#'
#' @family Data Wrangling
#'
#' @param data Source data
#'
#' @export
DataDisplayMeta <- function(data) {

  # Check to see if data is actual data ----
  if(!(any(class(data) %in% c("data.table","data.frame","tibble")))) stop("data is not a data.table")

  # Begin process----
  Counter <- 0L
  N <- data[, .N]
  x <- data.table::data.table(Variable = rep("donotuse", N), Type = rep("donotuse", N))
  for(name in names(data)) {
    Counter <- Counter + 1L
    data.table::set(x, i = Counter, j = "Variable", value = eval(name))
    data.table::set(x, i = Counter, j = "DataType", value = class(data[[eval(name)]]))
  }

  # Return table
  return(x[Variable != "donotuse"])
}

#' @title TimeSeriesMelt
#'
#' @description TimeSeriesMelt
#'
#' @family Data Wrangling
#'
#' @author Adrian Antico
#'
#' @param data source data
#' @param TargetVariable vector of target variable names
#' @param DateVariable Name of date variable
#' @param GroupVariables Vector of group variable names
#'
#' @export
TimeSeriesMelt <- function(data,
                           TargetVariable = NULL,
                           DateVariable = NULL,
                           GroupVariables = NULL) {

  # 2 Cases:
  #  Multiple Targets + Grouping Variables
  #  Multiple Targets + No Grouping Variables
  if(length(TargetVariable) > 1) {
    if(!is.null(GroupVariables)) {
      data <- data.table::melt(
        data = data,
        id.vars = c(eval(DateVariable),eval(GroupVariables)),
        measure.vars = eval(TargetVariable),
        variable.name = "GroupVar",
        value.name = "TargetSeries")
    } else {
      data <- data.table::melt(
        data = data,
        id.vars = eval(DateVariable),
        measure.vars = c(eval(TargetVariable)),
        variable.name = "GroupVar",
        value.name = "TargetSeries")
    }
  }

  # Return
  return(data)
}

#' @title DifferenceData
#'
#' @description DifferenceData differences your data set
#'
#' @family Feature Engineering
#'
#' @author Adrian Antico
#'
#' @param data Source data
#' @param ColumnsToDiff The column numbers you want differenced
#' @param CARMA Set to TRUE for CARMA functions
#' @param TargetVariable The target variable name
#' @param GroupingVariable Difference data by group
#'
#' @export
DifferenceData <- function(data,
                           ColumnsToDiff = c(names(data)[2:ncol(data)]),
                           CARMA = FALSE,
                           TargetVariable = NULL,
                           GroupingVariable = NULL) {

  # Turn on full speed ahead----
  data.table::setDTthreads(threads = max(1L, parallel::detectCores() - 2L))

  # Ensure data.table----
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  # Keep First Row of Data
  if(!is.null(GroupingVariable)) {
    FirstRow <- data[data[, .I[1], get(GroupingVariable)]$V1]
  } else {
    FirstRow <- data[1,]
  }

  # Keep Last Row of Target Variable----
  if(!is.null(GroupingVariable)) {
    LastRow <- data[data[, .I[.N], get(GroupingVariable)]]$V1
  } else {
    LastRow <- data[data[, .I[.N]]]
  }

  # Diff data
  if(!is.null(GroupingVariable)) {
    DiffData <- cbind(data[1:(data[, .I[.N-1], get(GroupingVariable)]$V1),1],data[, lapply(.SD,diff), by = eval(GroupingVariable), .SDcols = ColumnsToDiff])
  } else {
    DiffData <- cbind(data[1:(nrow(data)-1),1],data[, lapply(.SD,diff), .SDcols = ColumnsToDiff])
  }

  # Return data
  if(!CARMA) {
    return(list(DiffData = DiffData, FirstRow = FirstRow, LastRow = data[nrow(data),]))
  } else {
    if(!is.null(GroupingVariable)) {
      FirstRow <- FirstRow[, get(TargetVariable), by = eval(GroupingVariable)]
      return(list(DiffData = DiffData, FirstRow = FirstRow, LastRow = LastRow))
    } else {

      #FirstRow <- FirstRow[, get(TargetVariable)]
      return(list(DiffData = DiffData, FirstRow = FirstRow, LastRow = LastRow))
    }
  }
}

#' @title DifferenceDataReverse
#'
#' @description DifferenceDataReverse reverses the difference
#'
#' @family Feature Engineering
#'
#' @author Adrian Antico
#'
#' @param data Pre differenced scoring data
#' @param ScoreData Predicted values from ML model
#' @param LastRow The last row from training data target variables
#' @param TargetCol Target column name
#' @param CARMA Set to TRUE for CARMA utilization
#' @param FirstRow The first row of the target variable
#' @param GroupingVariables Group columns
#'
#' @export
DifferenceDataReverse <- function(data,
                                  ScoreData = Forecasts$Predictions,
                                  LastRow = DiffTrainOutput$LastRow$Weekly_Sales,
                                  CARMA = FALSE,
                                  TargetCol = TargetColumnName,
                                  FirstRow = DiffTrainOutput$FirstRow,
                                  GroupingVariables = NULL) {

  # Turn on full speed ahead----
  data.table::setDTthreads(threads = max(1L, parallel::detectCores() - 2L))

  # Ensure data.table----
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  ModifiedData <- data.table::copy(data)
  if(!CARMA) {
    if(!is.null(GroupingVariables)) {
      ""
    } else {
      return(ModifiedData[, Predictions := cumsum(c(LastRow,ScoreData))])
    }
  } else {
    if(!is.null(GroupingVariables)) {
      ""
    } else {
      x <- cumsum(c(FirstRow,ModifiedData[[eval(TargetCol)]]))
      xx <- x[-length(x)]
      return(ModifiedData[, eval(TargetCol) := xx][, Predictions := xx])
    }
  }
}

#' @title FullFactorialCatFeatures
#'
#' @description FullFactorialCatFeatures reverses the difference
#'
#' @family Data Wrangling
#'
#' @author Adrian Antico
#'
#' @param GroupVars Character vector of categorical columns to fully interact
#' @param MaxCombin The max K in N choose K. If NULL, K will loop through 1 to length(GroupVars)
#' @param BottomsUp TRUE or FALSE. TRUE starts with the most comlex interaction to the main effects
#'
#' @export
FullFactorialCatFeatures <- function(GroupVars = GroupVariables,
                                     MaxCombin = NULL,
                                     BottomsUp = TRUE) {

  if(is.null(MaxCombin)) {
    MaxCombin <- N <- length(GroupVars)
  } else {
    N <- MaxCombin
  }
  Categoricals <- c()

  # N choose 1 case ----
  for(j in seq_along(GroupVars)) Categoricals <- c(Categoricals,GroupVars[j])

  # N choose i for 2 <= i < N
  for(i in seq_len(N)[-1L]) {

    # Case 2: N choose 2 up to N choose N-1: Middle-Hierarchy Interactions
    if(MaxCombin == length(GroupVars)) {
      if(i < N) {
        temp <- combinat::combn(GroupVars, m = i)
        temp2 <- c()
        for(k in seq_len(ncol(temp))) {
          for(l in seq_len(i)) {
            if(l == 1L) {
              temp2 <- temp[l,k]
            } else {
              temp2 <- paste(temp2,temp[l,k], sep = "_")
            }
          }
          Categoricals <- c(Categoricals, temp2)
        }

        # Case 3: N choose N - Full Interaction
      } else if(i == length(GroupVars)) {
        temp <- combinat::combn(GroupVars, m = i)
        for(m in seq_len(N)) {
          if(m == 1) {
            temp2 <- temp[m]
          } else {
            temp2 <- paste(temp2,temp[m], sep = "_")
          }
        }
        Categoricals <- c(Categoricals, temp2)
      }
    } else {
      if(i <= N) {
        temp <- combinat::combn(GroupVars, m = i)
        temp2 <- c()
        for(k in seq_len(ncol(temp))) {
          for(l in seq_len(i)) {
            if(l == 1L) {
              temp2 <- temp[l,k]
            } else {
              temp2 <- paste(temp2,temp[l,k], sep = "_")
            }
          }
          Categoricals <- c(Categoricals, temp2)
        }

        # Case 3: N choose N - Full Interaction
      } else if(i == length(GroupVars)) {
        temp <- combinat::combn(GroupVars, m = i)
        for(m in seq_len(N)) {
          if(m == 1) {
            temp2 <- temp[m]
          } else {
            temp2 <- paste(temp2,temp[m], sep = "_")
          }
        }
        Categoricals <- c(Categoricals, temp2)
      }
    }

  }

  # Order of output ----
  if(BottomsUp) return(rev(Categoricals)) else return(Categoricals)
}

#' @title Interact
#'
#' @family Feature Engineering
#'
#' @param x Names
#' @param i Iteration
#' @param NumVarOperations List of names
#' @param Standardize List of results
#'
#' @noRd
Interact <- function(x,
                     i,
                     NumVarOperations,
                     Standardize) {
  if(i > 2L) {
    temp <- Standardize[[NumVarOperations[[x]][[1L]]]]$Result * Standardize[[NumVarOperations[[x]][[2L]]]]$Result
    for(ggg in 3L:i) temp <- temp * Standardize[[NumVarOperations[[x]][[ggg]]]]$Result
  } else {
    temp <- Standardize[[NumVarOperations[[x]][[1L]]]]$Result * Standardize[[NumVarOperations[[x]][[2L]]]]$Result
  }
  temp
}

#' @title AutoInteraction
#'
#' @description AutoInteraction creates interaction variables from your numerical features in your data. Supply a set of column names to utilize and set the interaction level. Supply a character vector of columns to exclude and the function will ignore those features.
#'
#' @family Feature Engineering
#'
#' @author Adrian Antico
#'
#' @param data Source data.table
#' @param NumVars Names of numeric columns (if NULL, all numeric and integer columns will be used)
#' @param InteractionDepth The max K in N choose K. If NULL, K will loop through 1 to length(NumVars). Default is 2 for pairwise interactions
#' @param Center TRUE to center the data
#' @param Scale TRUE to scale the data
#' @param SkipCols Use this to exclude features from being created. An example could be, you build a model with all variables and then use the varaible importance list to determine which features aren't necessary and pass that set of features into this argument as a character vector.
#' @param Scoring Defaults to FALSE. Set to TRUE for generating these columns in a model scoring setting
#' @param File When Scoring is set to TRUE you have to supply either the .Rdata list with lookup values for recreating features or a pathfile to the .Rdata file with the lookup values. If you didn't center or scale the data then this argument can be ignored.
#'
#' @examples
#' \dontrun{
#'
#' #########################################
#' # Feature Engineering for Model Training
#' #########################################
#'
#' # Create fake data
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
#' # Print number of columns
#' print(ncol(data))
#'
#' # Store names of numeric and integer cols
#' Cols <-names(data)[c(which(unlist(lapply(data, is.numeric))),
#'                      which(unlist(lapply(data, is.integer))))]
#'
#' # Model Training Feature Engineering
#' system.time(data <- RemixAutoML::AutoInteraction(
#'   data = data,
#'   NumericVars = Cols,
#'   InteractionDepth = 4,
#'   Center = TRUE,
#'   Scale = TRUE,
#'   SkipCols = NULL,
#'   Scoring = FALSE,
#'   File = getwd()))
#'
#' # user  system elapsed
#' # 0.30    0.11    0.41
#'
#' # Print number of columns
#' print(ncol(data))
#'
#' ########################################
#' # Feature Engineering for Model Scoring
#' ########################################
#'
#' # Create fake data
#' data <- RemixAutoML::FakeDataGenerator(
#'   Correlation = 0.70,
#'   N = 1000,
#'   ID = 2L,
#'   FactorCount = 2L,
#'   AddDate = TRUE,
#'   ZIP = 0L,
#'   TimeSeries = FALSE,
#'   ChainLadderData = FALSE,
#'   Classification = FALSE,
#'   MultiClass = FALSE)
#'
#' # Print number of columns
#' print(ncol(data))
#'
#' # Reduce to single row to mock a scoring scenario
#' data <- data[1L]
#'
#' # Model Scoring Feature Engineering
#' system.time(data <- RemixAutoML::AutoInteraction(
#'   data = data,
#'   NumericVars = names(data)[
#'     c(which(unlist(lapply(data, is.numeric))),
#'       which(unlist(lapply(data, is.integer))))],
#'   InteractionDepth = 4,
#'   Center = TRUE,
#'   Scale = TRUE,
#'   SkipCols = NULL,
#'   Scoring = TRUE,
#'   File = file.path(getwd(), "Standardize.Rdata")))
#'
#' # user  system elapsed
#' # 0.19    0.00    0.19
#'
#' # Print number of columns
#' print(ncol(data))
#' }
#' @export
AutoInteraction <- function(data = NULL,
                            NumericVars = NULL,
                            InteractionDepth = 2,
                            Center = TRUE,
                            Scale = TRUE,
                            SkipCols = NULL,
                            Scoring = FALSE,
                            File = NULL) {

  # Arg Check ----
  if(InteractionDepth > length(NumericVars)) stop("InteractionDepth cannot be greater than the length of NumericVars")

  # Check data ----
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  # Check args ----
  if(!is.logical(Center)) stop("Center must be either TRUE or FALSE")
  if(!is.logical(Scale)) stop("Scale must be either TRUE or FALSE")
  if(!is.logical(Scoring)) stop("Scoring must be either TRUE or FALSE")
  if(!is.null(NumericVars) && !is.character(NumericVars)) stop("NumericVars must be a character vector or NULL")
  if(!is.null(SkipCols) && !is.character(SkipCols)) stop("SkipCols must be a character vector")
  if(!is.null(InteractionDepth) && !(is.numeric(InteractionDepth) || is.integer(InteractionDepth))) stop("InteractionDepth must be numeric or NULL")

  # Check File ----
  if(Scoring && (Center || Scale) && is.null(File)) stop("You need to supply the path for the File argument")
  if(Scoring && !is.null(File)) if(!dir.exists(File) && !file.exists(File) && !is.list(File)) stop("File is not valid")
  if(!Scoring && !is.null(File)) if(!dir.exists(File)) stop("File is not valid path")

  # Columns Validity check ----
  if(is.null(NumericVars)) {
    NumericVars <- names(data)[c(which(unlist(lapply(data, is.numeric))), which(unlist(lapply(data, is.integer))))]
    if(identical(NumericVars, character(0))) stop("No numeric or integer valued columns in data")
    for(nam in NumericVars) {
      tmp <- class(data[1L, get(nam)])
      if(!any(tmp %chin% c("numeric","integer"))) NumericVars <- NumericVars[!NumericVars %chin% nam]
    }
    if(identical(NumericVars, character(0))) stop("No numeric or integer valued columns in data")
    if(length(NumericVars) == 1L) stop("Only one column exists so no interactions can be generated")
    if(length(NumericVars) < InteractionDepth) stop("You can't have an InteractionDepth that is greater than the number of NumericVars")
  } else {
    if(!all(NumericVars %chin% names(data))) stop("at least some NumericVars are not in data")
    for(nam in NumericVars) {
      tmp <- class(data[1L, get(nam)])
      if(!any(tmp %chin% c("numeric","integer"))) NumericVars <- NumericVars[!NumericVars %chin% nam]
    }
    if(identical(NumericVars, character(0))) stop("No numeric or integer valued columns in data")
    if(length(NumericVars) == 1L) stop("Only one column exists so no interactions can be generated")
    if(length(NumericVars) < InteractionDepth) stop("You can't have an InteractionDepth that is greater than the number of NumericVars")
  }

  # Define Number of computations and increase alloc if too many ----
  N1 <- length(NumericVars)
  N <- InteractionDepth
  Total <- c()
  for(com in seq_len(InteractionDepth)[-1L]) Total <- c(Total, ncol(combinat::combn(NumericVars, m = com)))
  if(sum(Total) + ncol(data) > 1028L) data.table::setalloccol(DT = data, n = sum(Total) + ncol(data), verbose = TRUE)

  # Standardize collection list ----
  if(Scoring && (Center || Scale) && !is.list(File)) load(file = file.path(File))
  if(!exists("Standardize")) Standardize <- list()

  # Get results and metadata ahead of time ----
  if(!Scoring) {
    for(nam in NumericVars) {
      if(Center && Scale) {
        a1 <- as.matrix(data[[nam]])
        Standardize[[nam]]$Mean <- Rfast::colmeans(a1)
        c1 <- t(a1) - Standardize[[nam]]$Mean
        Standardize[[nam]]$Denom <- sqrt(Rfast::rowsums(c1^2))
        Standardize[[nam]]$Factor <- sqrt((dim(a1)[1L] - 1))
        Standardize[[nam]]$Result <- t(c1/Standardize[[nam]]$Denom * Standardize[[nam]]$Factor)
      } else if(Center && !Scale) {
        a1 <- as.matrix(data[[nam]])
        Standardize[[nam]]$Mean <- Rfast::colmeans(a1)
        Standardize[[nam]]$Result <- t(a1) - Standardize[[nam]]$Mean
      } else if(!Center && Scale) {
        a1 <- as.matrix(data[[nam]])
        Standardize[[nam]]$Denom <- sqrt(Rfast::rowsums(a1^2))
        Standardize[[nam]]$Factor <- sqrt((dim(a1)[1L] - 1))
        Standardize[[nam]]$Result <- t(a1/Standardize[[nam]]$Denom * Standardize[[nam]]$Factor)
      } else {
        Standardize[[nam]]$Result <- data[[NumVarOperations[[x]][[2L]]]]
      }
    }
  } else {
    for(nam in NumericVars) {
      if(Center && Scale) {
        a1 <- as.matrix(data[[nam]])
        c1 <- t(a1) - Standardize[[nam]]$Mean
        Standardize[[nam]]$Result <- t(c1/Standardize[[nam]]$Denom * Standardize[[nam]]$Factor)
      } else if(Center && !Scale) {
        a1 <- as.matrix(data[[nam]])
        Standardize[[nam]]$Result <- t(a1) - Standardize[[nam]]$Mean
      } else if(!Center && Scale) {
        a1 <- as.matrix(data[[nam]])
        Standardize[[nam]]$Result <- t(a1/Standardize[[nam]]$Denom * Standardize[[nam]]$Factor)
      } else {
        Standardize[[nam]]$Result <- data[[NumVarOperations[[x]][[2L]]]]
      }
    }
  }

  # Define colnames and contributing features ----
  for(i in seq_len(N)[-1L]) {

    # Initialize lists and vector
    NumVarsNames <- c()
    NumVarOperations <- list()

    # Interaction Depth equals number of variables
    if(i == N1) {
      temp <- combinat::combn(NumericVars, m = i)
      for(m in N1) {
        temp2 <- paste(temp, collapse = "_")
        templist <- list()
        for(zz in seq_len(m)) templist[[zz]] <- temp[zz]
        NumVarOperations[[temp2]] <- templist
      }
      NumVarsNames <- c(NumVarsNames, temp2)
    } else if(i <= N1) {
      temp <- combinat::combn(NumericVars, m = i)
      temp2 <- c()
      for(k in seq_len(ncol(temp))) {
        for(l in seq_len(i)) {
          if(l == 1L) {
            temp2 <- temp[l,k]
          } else {
            temp2 <- paste(temp2,temp[l, k], sep = "_")
            templist <- list()
            for(zz in seq_len(l)) templist[[zz]] <- temp[zz, k]
            NumVarOperations[[temp2]] <- templist
          }
        }
        NumVarsNames <- c(NumVarsNames, temp2)
      }
    }

    # SkipCols ----
    if(!is.null(SkipCols)) NumVarsNames <- NumVarsNames[!NumVarsNames %chin% SkipCols]

    # Build features ----
    data[, (NumVarsNames) := lapply(NumVarsNames, Interact, i, NumVarOperations, Standardize)]
  }

  # Save Standardize if Center or Scale ----
  if(!Scoring && (Center || Scale)) {
    for(nam in names(Standardize)) Standardize[[nam]][["Result"]] <- NULL
    if(!is.null(File)) save(Standardize, file = file.path(File, "Standardize.Rdata"))
  }

  # Return data ----
  return(data)
}

#' @title DiffDT
#'
#' @description Difference a column in a data.table
#'
#' @author Adrian Antico
#' @family Misc
#'
#' @param x Column name
#' @param NLag1 Numeric
#' @param NLag2 Numeric
#' @param Type Choose from 'numeric' or 'date'
#' @noRd
DiffDT <- function(x, NLag1, NLag2, Type = "numeric") {
  if(Type == "numeric") {
    if(NLag1 == 0) {
      temp <- data[[eval(x)]] - data[[paste0("Diff_", NLag2, "_", x)]]
    } else {
      temp <- data[[paste0("Diff_", NLag1, "_", x)]] - data[[paste0("Diff_", NLag1,"-", NLag2, "_", x)]]
    }
  } else if(Type == "date") {
    if(NLag1 == 0) {
      temp <- difftime(time1 = data[[eval(x)]], time2 = data[[paste0("Diff_", NLag1,"-", NLag2, "_", x)]], units = "days")
    } else {
      temp <- difftime(time1 = data[[paste0("Diff_",NLag1, "_", x)]], time2 = data[[paste0("Diff_", NLag1,"-", NLag2, "_", x)]], units = "days")
    }
  }
  return(as.numeric(temp))
}

#' @title AutoDiffLagN
#'
#' @description AutoDiffLagN create differences for selected numerical columns
#'
#' @family Feature Engineering
#' @author Adrian Antico
#'
#' @param data Source data
#' @param DateVariable Date column used for sorting
#' @param GroupVariables Difference data by group
#' @param DiffVariables Column names of numeric columns to difference
#' @param DiffDateVariables Columns names for date variables to difference. Output is a numeric value representing the difference in days.
#' @param NLag1 If the diff calc, we have column 1 - column 2. NLag1 is in reference to column 1. If you want to take the current value minus the previous weeks value, supply a zero. If you want to create a lag2 - lag4 NLag1 gets a 2.
#' @param NLag2 If the diff calc, we have column 1 - column 2. NLag2 is in reference to column 2. If you want to take the current value minus the previous weeks value, supply a 1. If you want to create a lag2 - lag4 NLag1 gets a 4.
#' @param Sort TRUE to sort your data inside the function
#' @param RemoveNA Set to TRUE to remove rows with NA generated by the lag operation
#' @examples
#' \dontrun{
#'
#' # Create fake data
#' data <- RemixAutoML::FakeDataGenerator(
#'   Correlation = 0.70,
#'   N = 50000,
#'   ID = 2L,
#'   FactorCount = 3L,
#'   AddDate = TRUE,
#'   ZIP = 0L,
#'   TimeSeries = FALSE,
#'   ChainLadderData = FALSE,
#'   Classification = FALSE,
#'   MultiClass = FALSE)
#'
#' # Store Cols to diff
#' Cols <- names(data)[which(unlist(data[, lapply(.SD, is.numeric)]))]
#'
#' # Clean data before running AutoDiffLagN
#' data <- RemixAutoML::ModelDataPrep(data = data, Impute = FALSE, CharToFactor = FALSE, FactorToChar = TRUE)
#'
#' # Run function
#' data <- RemixAutoML::AutoDiffLagN(
#'   data,
#'   DateVariable = "DateTime",
#'   GroupVariables = c("Factor_1", "Factor_2"),
#'   DiffVariables = Cols,
#'   DiffDateVariables = NULL,
#'   NLag1 = 0L,
#'   NLag2 = 1L,
#'   Sort = TRUE,
#'   RemoveNA = TRUE)
#' }
#'
#' @export
AutoDiffLagN <- function(data,
                         DateVariable = NULL,
                         GroupVariables = NULL,
                         DiffVariables = NULL,
                         DiffDateVariables = NULL,
                         NLag1 = 0L,
                         NLag2 = 1L,
                         Sort = FALSE,
                         RemoveNA = TRUE) {

  # Turn on full speed ahead ----
  data.table::setDTthreads(threads = max(1L, parallel::detectCores() - 2L))

  # Ensure data.table ----
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  # Check args ----
  if(!is.character(DateVariable)) stop("DateVariable needs to be a charcter valued vector or scalar")
  if(!is.character(GroupVariables)) stop("GroupVariables needs to be a charcter valued vector or scalar")
  if(!is.character(DiffVariables)) stop("DiffVariables needs to be a charcter valued vector or scalar")
  if(!is.numeric(NLag1)) stop("NLag1 needs to be a numeric valued scalar")
  if(!is.numeric(NLag2)) stop("NLag2 needs to be a numeric valued scalar")
  if(NLag1 < 0) stop("NLag1 needs to be a positive numeric valued scalar")
  if(NLag2 < 0) stop("NLag2 needs to be a positive numeric valued scalar")
  if(!is.logical(Sort)) stop("Sort needs to be a logical valued scalar")

  # Sort if TRUE ----
  if(!is.null(GroupVariables)) {
    data.table::setorderv(x = data, cols = c(GroupVariables, DateVariable), order = c(rep(1, length(c(GroupVariables, DateVariable)))), na.last = FALSE)
  } else {
    data.table::setorderv(x = data, cols = c(GroupVariables, DateVariable), order = 1L, na.last = FALSE)
  }

  # Diff numeric data ----
  if(!is.null(DiffVariables)) {
    if(NLag1 == 0L) {
      ColNames <- names(data.table::copy(data))
      ModDiffVariables <- paste0("Diff_", NLag2, "_", DiffVariables)
      if(!is.null(GroupVariables)) {
        data <- data[, (ModDiffVariables) := data.table::shift(x = .SD, n = NLag2, fill = NA, type = "lag"), .SDcols = c(DiffVariables), by = eval(GroupVariables)]
        data <- data[, (ModDiffVariables) := lapply(DiffVariables, DiffDT, NLag1, NLag2)]
      } else {
        data <- data[, (ModDiffVariables) := data.table::shift(x = .SD, n = NLag2, fill = NA, type = "lag"), .SDcols = c(DiffVariables)]
        data <- data[, (ModDiffVariables) := lapply(DiffVariables, DiffDT, NLag1, NLag2)]
      }
    } else {
      ColNames <- names(data.table::copy(data))
      ModDiffVariables1 <- paste0("Diff_", NLag1, "_", DiffVariables)
      ModDiffVariables2 <- paste0("Diff_", NLag1,"-", NLag2, "_", DiffVariables)
      if(!is.null(GroupVariables)) {
        data <- data[, (ModDiffVariables1) := data.table::shift(x = .SD, n = NLag1, fill = NA, type = "lag"), .SDcols = c(DiffVariables), by = eval(GroupVariables)]
        data <- data[, (ModDiffVariables2) := data.table::shift(x = .SD, n = NLag2, fill = NA, type = "lag"), .SDcols = c(DiffVariables), by = eval(GroupVariables)]
        data <- data[, (ModDiffVariables2) := lapply(DiffVariables, DiffDT, NLag1, NLag2, Type = "numeric")]
        data[, (ModDiffVariables1) := NULL]
      } else {
        data <- data[, (ModDiffVariables1) := data.table::shift(x = .SD, n = NLag1, fill = NA, type = "lag"), .SDcols = c(DiffVariables)]
        data <- data[, (ModDiffVariables2) := data.table::shift(x = .SD, n = NLag2, fill = NA, type = "lag"), .SDcols = c(DiffVariables)]
        data <- data[, (ModDiffVariables2) := lapply(DiffVariables, DiffDT, NLag1, NLag2, Type = "numeric")]
        data[, (ModDiffVariables1) := NULL]
      }
    }
  }

  # Diff date data ----
  if(!is.null(DiffDateVariables)) {
    if(NLag1 == 0L) {
      ColNames <- names(data.table::copy(data))
      ModDiffVariables <- paste0("Diff_", NLag2, "_", DiffDateVariables)
      if(!is.null(GroupVariables)) {
        data <- data[, (ModDiffVariables) := data.table::shift(x = .SD, n = NLag2, fill = NA, type = "lag"), .SDcols = c(DiffDateVariables), by = eval(GroupVariables)]
        data <- data[, (ModDiffVariables) := lapply(DiffDateVariables, DiffDT, NLag1, NLag2, Type = "date")]
      } else {
        data <- data[, (ModDiffVariables) := data.table::shift(x = .SD, n = NLag2, fill = NA, type = "lag"), .SDcols = c(DiffDateVariables)]
        data <- data[, (ModDiffVariables) := lapply(DiffDateVariables, DiffDT, NLag1, NLag2, Type = "date")]
      }
    } else {
      ColNames <- names(data.table::copy(data))
      ModDiffVariables1 <- paste0("Diff_", NLag1, "_", DiffDateVariables)
      ModDiffVariables2 <- paste0("Diff_", NLag1,"-", NLag2, "_", DiffDateVariables)
      if(!is.null(GroupVariables)) {
        data <- data[, (ModDiffVariables1) := data.table::shift(x = .SD, n = NLag1, fill = NA, type = "lag"), .SDcols = c(DiffDateVariables), by = eval(GroupVariables)]
        data <- data[, (ModDiffVariables2) := data.table::shift(x = .SD, n = NLag2, fill = NA, type = "lag"), .SDcols = c(DiffDateVariables), by = eval(GroupVariables)]
        data <- data[, (ModDiffVariables2) := lapply(DiffDateVariables, DiffDT, NLag1, NLag2, Type = "date")]
        data[, (ModDiffVariables1) := NULL]
      } else {
        data <- data[, (ModDiffVariables1) := data.table::shift(x = .SD, n = NLag1, fill = NA, type = "lag"), .SDcols = c(DiffDateVariables)]
        data <- data[, (ModDiffVariables2) := data.table::shift(x = .SD, n = NLag2, fill = NA, type = "lag"), .SDcols = c(DiffDateVariables)]
        data <- data[, (ModDiffVariables2) := lapply(DiffDateVariables, DiffDT, NLag1, NLag2, Type = "date")]
        data[, (ModDiffVariables1) := NULL]
      }
    }
  }

  # Final prep ----
  if(RemoveNA) {
    if(NLag1 == 0L) {
      if(!is.null(DiffVariables)) {
        data <- data[!is.na(get(paste0("Diff_", NLag2, "_", DiffVariables[[1L]])))]
      } else {
        data <- data[!is.na(get(paste0("Diff_", NLag2, "_", DiffDateVariables[[1L]])))]
      }
    } else {
      if(!is.null(DiffVariables)) {
        data <- data[!is.na(get(paste0("Diff_", NLag1,"-", NLag2, "_", DiffVariables[[1L]])))]
      } else {
        data <- data[!is.na(get(paste0("Diff_", NLag1,"-", NLag2, "_", DiffDateVariables[[1L]])))]
      }
    }
  }

  # Return data ----
  return(data)
}
