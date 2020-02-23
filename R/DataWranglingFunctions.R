#' ColumnSubsetDataTable
#'
#' ColumnSubsetDataTable will subset data tables by column
#'
#' @family Data Wrangling
#' @author Adrian Antico
#' @param data data.table
#' @param Target Target variable
#' @param Date Date variable
#' @param GroupVars Group variables
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

#' DataDisplayMeta
#'
#' DataDisplayMeta
#'
#' @author Adrian Antico
#' @family Data Wrangling
#' @param data Source data
#' @export
DataDisplayMeta <- function(data) {
  
  # Check to see if data is actual data----
  if(!(any(class(data) %in% c("data.table","data.frame","tibble")))) {
    return(NULL)
  }
  
  # Begin process----
  Counter <- 0L
  N <- data[, .N]
  x <- data.table::data.table(Variable = rep("donotuse", N), Type = rep("donotuse", N))
  for(name in names(data)) {
    Counter <- Counter + 1L
    data.table::set(x, 
        i = Counter, 
        j = "Variable", 
        value = eval(name))
    data.table::set(x, 
        i = Counter, 
        j = "DataType", 
        value = class(data[[eval(name)]]))
  }
  
  # Return table
  return(x[Variable != "donotuse"])
}

#' TimeSeriesMelt
#'
#' TimeSeriesMelt
#'
#' @family Data Wrangling
#' @author Adrian Antico
#' @param data source data
#' @param TargetVariable vector of target variable names
#' @param DateVariable Name of date variable
#' @param GroupVariables Vector of group variable names
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

#' DifferenceData
#' 
#' DifferenceData differences your data set
#' @family Time Series
#' @author Adrian Antico
#' @param data Source data
#' @param ColumnsToDiff The column numbers you want differenced
#' @param CARMA Set to TRUE for CARMA functions
#' @param TargetVariable The target variable name
#' @param GroupingVariable Difference data by group
#' @export
DifferenceData <- function(data, 
                           ColumnsToDiff = c(names(data)[2:ncol(data)]),
                           CARMA = FALSE,
                           TargetVariable = NULL,
                           GroupingVariable = NULL) {
  
  # Turn on full speed ahead----
  data.table::setDTthreads(percent = 100)
  
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
    return(list(DiffData = DiffData,
                FirstRow = FirstRow,
                LastRow = data[nrow(data),])) 
  } else {
    if(!is.null(GroupingVariable)) {
      FirstRow <- FirstRow[, get(TargetVariable), by = eval(GroupingVariable)]
      return(list(DiffData = DiffData,
                  FirstRow = FirstRow,
                  LastRow = LastRow))
    } else {
      
      #FirstRow <- FirstRow[, get(TargetVariable)]
      return(list(DiffData = DiffData,
                  FirstRow = FirstRow,
                  LastRow = LastRow))      
    }
  }
}

#' DifferenceDataReverse
#' 
#' DifferenceDataReverse reverses the difference
#' @family Time Series
#' @author Adrian Antico
#' @param data Pre differenced scoring data
#' @param ScoreData Predicted values from ML model
#' @param LastRow The last row from training data target variables
#' @param TargetCol Target column name
#' @param CARMA Set to TRUE for CARMA utilization
#' @param FirstRow The first row of the target variable
#' @param GroupingVariables Group columns
#' @export
DifferenceDataReverse <- function(data, 
                                  ScoreData = Forecasts$Predictions, 
                                  LastRow = DiffTrainOutput$LastRow$Weekly_Sales,
                                  CARMA = FALSE,
                                  TargetCol = TargetColumnName,
                                  FirstRow = DiffTrainOutput$FirstRow,
                                  GroupingVariables = NULL) {
  
  # Turn on full speed ahead----
  data.table::setDTthreads(percent = 100)
  
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

#' FullFactorialCatFeatures
#' 
#' FullFactorialCatFeatures reverses the difference
#' @family Data Wrangling
#' @author Adrian Antico
#' @param GroupVars Character vector of categorical columns to fully interact
#' @param BottomsUp TRUE or FALSE. TRUE starts with the most comlex interaction to the main effects
#' @export
FullFactorialCatFeatures <- function(GroupVars = GroupVariables,
                                     BottomsUp = TRUE) {
  
  N <- length(GroupVars)
  Categoricals <- c()
  
  # Binomial Expansion
  for(i in 1:N) {
    
    # Case 1: N choose 1 - Store each individual column name separately (main effects)
    if(i == 1) {
      for(j in 1:N) {
        Categoricals <- c(Categoricals,GroupVars[j])
      }
      
    # Case 2: N choose 2 up to N choose N-1: Middle-Hierarchy Interactions
    } else if(i < N) {
      temp <- combinat::combn(GroupVars, m = i)
      temp2 <- c()
      for(k in 1:ncol(temp)) {
        for(l in 1:nrow(temp)) {
          if(l == 1) {
            temp2 <- temp[l,k]
          } else {
            temp2 <- paste(temp2,temp[l,k], sep = "_")
          }
        }
        Categoricals <- c(Categoricals, temp2)
      }
      
    # Case 3: N choose N - Full Interaction
    } else {
      temp <- combinat::combn(GroupVars, m = i)
      for(m in 1:N) {
        if(m == 1) {
          temp2 <- temp[m]
        } else {
          temp2 <- paste(temp2,temp[m], sep = "_")
        }
      }
      Categoricals <- c(Categoricals, temp2)
    }
  }
  
  # Order of output----
  if(BottomsUp) {
    return(rev(Categoricals))  
  } else {
    return(Categoricals)
  }
}
