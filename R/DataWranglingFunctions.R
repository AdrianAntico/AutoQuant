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
#' @family Time Series
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
#' @family Time Series
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

#' @title AutoNumericInteraction
#'
#' @description AutoNumericInteraction creates interaction variables from your numerical features in your data. Supply a set of column names to utilize and set the interaction level. Supply a character vector of columns to exclude and the function will ignore those features.
#'
#' @family Feature Engineering
#'
#' @author Adrian Antico
#'
#' @param data Source data.table
#' @param NumVars Names of numeric columns (if NULL, all numeric and integer columns will be used)
#' @param InteractionDepth The max K in N choose K. If NULL, K will loop through 1 to length(NumVars)
#' @param Center TRUE to center the data
#' @param Scale TRUE to scale the data
#' @param SkipCols Use this to exclude features from being created. An example could be, you build a model with all variables and then use the varaible importance list to determine which features aren't necessary and pass that set of features into this argument as a character vector.
#'
#' @examples
#' \dontrun{
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
#' # Store names of numeric and integer cols
#' Cols <-names(data)[c(which(unlist(lapply(data, is.numeric))),
#'                      which(unlist(lapply(data, is.integer))))]
#'
#' # Create features
#' data <- RemixAutoML::AutoInteraction(
#'   data = data,
#'   NumericVars = Cols,
#'   InteractionDepth = 4,
#'   Center = TRUE,
#'   Scale = TRUE,
#'   SkipCols = NULL)
#' }
#'
#' @export
AutoInteraction <- function(data = NULL,
                            NumericVars = NULL,
                            InteractionDepth = NULL,
                            Center = TRUE,
                            Scale = TRUE,
                            SkipCols = NULL) {

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
  print(data.table::truelength(data))
  if(sum(Total) + ncol(data) > 1028L) data.table::setalloccol(DT = data, n = sum(Total) + ncol(data), verbose = TRUE)

  # N choose i for 2 <= i < N
  for(i in seq_len(N)[-1L]) {

    # Initialize lists and vector
    NumVarsNames <- c()
    NumVarOperations <- list()

    # Case 2: N choose 2 up to N choose N-1: Middle-Hierarchy Interactions
    if(i == N1) {
      if(i < N) {
        temp <- combinat::combn(NumericVars, m = i)
        temp2 <- c()
        for(k in seq_len(ncol(temp))) {
          for(zz in seq_len(l)) templist[[zz]] <- temp[zz, k]
          NumVarsNames <- c(NumVarsNames, temp2)
        }

        # Case 3: N choose N - Full Interaction
      } else if(i == N1) {
        temp <- combinat::combn(NumericVars, m = i)
        for(m in seq_len(N)) {
          if(m == 1) {
            temp2 <- temp[m]
          } else {
            temp2 <- paste(temp2,temp[l,k], sep = "_")
            templist <- list()
            for(zz in seq_len(l)) templist[[zz]] <- temp[zz, k]
            NumVarOperations[[temp2]] <- templist
          }
        }
        NumVarsNames <- c(NumVarsNames, temp2)
      }
    } else {
      if(i <= N1) {
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

        # Case 3: N choose N - Full Interaction
      } else if(i == N1) {
        temp <- combinat::combn(NumericVars, m = i)
        for(m in seq_len(N)) {
          if(m == 1) {
            temp2 <- temp[m]
          } else {
            temp2 <- paste(temp2,temp[m], sep = "_")
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
    data[, (NumVarsNames) := lapply(NumVarsNames, FUN = function(x) {
      if(i > 2L) {
        if(any(c(Center,Scale))) {
          temp <- Rfast::standardise(as.matrix(data[[NumVarOperations[[x]][[1L]]]]), center = Center, scale = Scale) * Rfast::standardise(as.matrix(data[[NumVarOperations[[x]][[2L]]]]), center = Center, scale = Scale)
          for(ggg in 3L:i) {
            temp <- temp * Rfast::standardise(as.matrix(data[[NumVarOperations[[x]][[ggg]]]]), center = Center, scale = Scale)
          }
        } else {
          temp <- data[[NumVarOperations[[x]][[1L]]]] * data[[NumVarOperations[[x]][[2L]]]]
          for(ggg in 3L:i) {
            temp <- temp * data[[NumVarOperations[[x]][[ggg]]]]
          }
        }
      } else {
        if(any(c(Center,Scale))) {
          temp <- Rfast::standardise(as.matrix(data[[NumVarOperations[[x]][[1L]]]]), center = Center, scale = Scale) * Rfast::standardise(as.matrix(data[[NumVarOperations[[x]][[2L]]]]), center = Center, scale = Scale)
        } else {
          temp <- data[[NumVarOperations[[x]][[1L]]]] * data[[NumVarOperations[[x]][[2L]]]]
        }
      }
      temp
    })]
  }

  # Return data ----
  return(data)
}
