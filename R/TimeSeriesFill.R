#' TimeSeriesFill For Completing Time Series Data
#'
#' TimeSeriesFill For Completing Time Series Data For Single Series or Time Series by Group
#'
#' @family Automated Time Series
#' @param data Supply your full series data set here
#' @param DateColumnName Supply the name of your date column
#' @param GroupVariables Supply the column names of your group variables. E.g. "Group" or c("Group1","Group2")
#' @param TimeUnit Choose from "hour", "day", "week", "month", "quarter", "year"
#' @param FillType Choose from "all" or "inner". Only relevant for when you have GroupVariables. The "all" option will take the max date and the min date of the entire data set and fill according to those. The "inner" option will grab the max and min dates by group levels and fill each group level based on those.
#' @examples
#' \donttest{
#' data <- TimeSeriesFill(data,
#'                        DateColumnName = "Date"
#'                        GroupVariables = "GroupVar",
#'                        TimeUnit = "days",
#'                        FillType = "inner")
#' }
#' @return Returns a data table with missing time series records filled (currently just zeros)
#' @export 
TimeSeriesFill <- function(data = data, 
                           DateColumnName = "Date",
                           GroupVariables = NULL, 
                           TimeUnit = "days",
                           FillType = "all") {
  
  # Ensure data.table----
  if(!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Check Args----
  if(!is.character(DateColumnName)) {
    warning("DateColumnName needs to be a character value")
    return(data)
  }
  if(!(FillType %chin% c("all", "inner"))) {
    warning("TimeUnit is not one of 'all' 'inner'")
    return(data)
  }
  if(!(TimeUnit %chin% c("hour", "day", "week", "month", "quarter", "year"))) {
    warning("TimeUnit needs to be one of 'hour' 'day' 'week' 'month' 'quarter' 'year'")
    return(data)
  }
  
  # Ensure date column is a date----
  x <- class(data[[eval(DateColumnName)]])
  if(is.character(x) | is.factor(x) | is.numeric(x) | is.integer(x)) {
    if(TimeUnit %chin% c("day", "week", "month", "quarter", "year")) {
      data.table::set(data, j = eval(DateColumnName), value = as.Date(data[[eval(DateColumnName)]]))
    } else {
      data.table::set(data, j = eval(DateColumnName), value = as.POSIXct(data[[eval(DateColumnName)]]))
    }
  }
  
  # Modify GroupVariables----
  if(!is.null(GroupVariables)) {
    if(length(GroupVariables) > 1) {
      data[, GroupVar := do.call(paste, c(.SD, sep = " ")), .SDcols = GroupVariables]
      data[, eval(GroupVariables) := NULL]
    } else {
      data.table::setnames(data, eval(GroupVariables), "GroupVar")
    }  
  }
  
  # Modify TimeUnit for Week----
  if(TimeUnit == "week") {
    TimeUnit <- "weeks"
  } else if(TimeUnit == "second") {
    TimeUnit <- "secs"
  } else if (TimeUnit == "minute") {
    TimeUnit <- "min"
  }
  
  # Manage data to modify----
  if(!is.null(GroupVariables) & tolower(FillType) == "all") {
    
    # Define date range----
    MinRange <- data[, min(get(DateColumnName))]
    MaxRange <- data[, max(get(DateColumnName))]
    
    # Store logical----
    Rows <- length(
      seq(from = MinRange, 
          to = MaxRange, 
          by = TimeUnit)) * length(unique(data[, GroupVar])) != data[, .N]
    
    
    if(Rows) {
      # Figure out which series need treatment
      FullSeriesLength <- length(seq(from = MinRange, to = MaxRange, by = TimeUnit))
      GroupSeriesLengths <- data[, .N, by = "GroupVar"]
      GroupSeriesLengths[, FullLength := FullSeriesLength]
      
      # ***** temp to test
      # data.table::set(GroupSeriesLengths, i = c(1,5), j = "FullLength", value = 500)
      
      ModGroups <- GroupSeriesLengths[N != FullLength, GroupVar]
      KeepGroups <- GroupSeriesLengths[N == FullLength, GroupVar]
      if(length(KeepGroups) != 0) {
        NoModData <- data[GroupVar %chin% eval(KeepGroups)]
        data <- data[!(GroupVar %chin% eval(KeepGroups))]
      }
    }
    
    # Modify data
    Groups <- unique(data[["GroupVar"]])
    counter <- 0L
    for(i in Groups) {
      counter <- counter + 1
      tempData <- data.table::CJ(
        i,
        seq(from = MinRange,
            to = MaxRange, 
            by = eval(TimeUnit)))
      FinalData <- merge(x = tempData,
                         y = data, 
                         by.x = c("i","V2"), 
                         by.y = c("GroupVar",eval(DateColumnName)),
                         all.x = TRUE)
      
      # Zero Fill----
      FinalData <- RemixAutoML::ModelDataPrep(
        data = FinalData, 
        Impute = TRUE, 
        CharToFactor = FALSE, 
        RemoveDates = FALSE,
        MissFactor = "0",
        MissNum = 0,
        IgnoreCols = NULL)
      data.table::setnames(
        FinalData, 
        old = c("i","V2"), 
        new = c("GroupVar",eval(DateColumnName)))
      if(counter == 1) {
        ReturnData <- FinalData
      } else {
        ReturnData <- data.table::rbindlist(
          list(ReturnData, FinalData))
      }
    }
    
    # Reverse group variables concatenation----
    if(!is.null(GroupVariables)) {
      if(length(GroupVariables) > 1) {
        if(Rows == TRUE & length(KeepGroups) != 0) {
          ReturnData[, eval(GroupVariables) := data.table::tstrsplit(GroupVar, " ")][, GroupVar := NULL]
          NoModData[, eval(GroupVariables) := data.table::tstrsplit(GroupVar, " ")][, GroupVar := NULL]
        }
        
        # Return data----
        return(
          data.table::rbindlist(
            list(NoModData,ReturnData)))
      } else {
        if(Rows == TRUE & length(KeepGroups) != 0) {
          data.table::setnames(ReturnData, "GroupVar", eval(GroupVariables))
          data.table::setnames(NoModData, "GroupVar", eval(GroupVariables))
          
          # Return data----
          return(
            data.table::rbindlist(
              list(NoModData,ReturnData)))
        } else {
          data.table::setnames(ReturnData, "GroupVar", eval(GroupVariables))    
          
          # Return data----
          return(ReturnData)
        }
      }
    }
    
    # Return data----
    if(Rows == TRUE & length(KeepGroups) != 0) {
      return(data.table::rbindlist(list(ReturnData, NoModData)))
    } else if(Rows) {
      return(ReturnData)
    } else {
      return(ReturnData)      
    }
    
    # Inner with Group
  } else if(!is.null(GroupVariables) & tolower(FillType) == "inner") {
    # Define date range----
    MinRange <- data[, min(get(DateColumnName)), by = "GroupVar"]
    data.table::setnames(MinRange, "V1", "MinDate")
    MaxRange <- data[, max(get(DateColumnName)), by = "GroupVar"]
    data.table::setnames(MaxRange, "V1", "MaxDate")
    MinMax <- merge(MinRange, MaxRange, by = "GroupVar", all = FALSE)
    NGroup <- data[, .N, by = "GroupVar"]
    
    # Store logical----
    data.table::set(
      MinMax,
      j = "Length",
      value = as.numeric(1 + difftime(MinMax[["MaxDate"]], MinMax[["MinDate"]], units = eval(TimeUnit))))
    MinMax <- merge(MinMax, NGroup, by = "GroupVar", all = FALSE)
    Rows <- MinMax[Length > N, GroupVar]
    Len <- unique(MinMax[["GroupVar"]])
    counter <- 0L
    
    # Some or all levels----
    if(length(Rows) != 0) {
      NoModData <- data[GroupVar %chin% MinMax[Length == N, GroupVar]]
      for(i in Rows) {
        counter <- counter + 1
        tempData <- data.table::CJ(
          i,
          seq(from = MinRange[GroupVar == eval(i), MinDate],
              MaxRange[GroupVar == eval(i), MaxDate], 
              by = eval(TimeUnit)))
        tempData2 <- data[GroupVar == eval(i)]
        FinalData <- merge(x = tempData,
                           y = tempData2, 
                           by.x = c("i","V2"), 
                           by.y = c("GroupVar",eval(DateColumnName)),
                           all.x = TRUE)
        
        # Zero Fill----
        FinalData <- RemixAutoML::ModelDataPrep(
          data = FinalData, 
          Impute = TRUE, 
          CharToFactor = FALSE, 
          RemoveDates = FALSE,
          MissFactor = "0",
          MissNum = 0,
          IgnoreCols = NULL)
        data.table::setnames(
          FinalData, 
          old = c("i","V2"), 
          new = c("GroupVar",eval(DateColumnName)))
        if(counter == 1) {
          ReturnData <- FinalData
        } else {
          ReturnData <- data.table::rbindlist(
            list(ReturnData, FinalData))
        }
      }
      
      # Reverse group variables concatenation----
      if(length(GroupVariables) > 1) {
        ReturnData[, eval(GroupVariables) := data.table::tstrsplit(GroupVar, " ")][, GroupVar := NULL]
        NoModData[, eval(GroupVariables) := data.table::tstrsplit(GroupVar, " ")][, GroupVar := NULL]
        
        # Return data----
        return(
          data.table::rbindlist(
            list(NoModData,ReturnData)))
      }
    } else {
      if(length(GroupVariables) > 1) {
        return(
          data[, eval(GroupVariables) := data.table::tstrsplit(GroupVar, " ")][, GroupVar := NULL])
        
      } else {
        return(data.table::setnames(data, "GroupVar", eval(GroupVariables)))
      }
    }
  } else {
    
    # Store logical
    Rows <- length(
      seq(from = data[, min(get(DateColumnName))], 
          to = data[, max(get(DateColumnName))], 
          by = TimeUnit)) != data[, .N]
    
    # Fill----
    if(Rows) {
      # Build data----
      tempData <- data.table::as.data.table(
        seq(from = data[, min(get(DateColumnName))],
            to = data[, max(get(DateColumnName))], 
            by = eval(TimeUnit)))
      
      # Join data----
      FinalData <- merge(x = tempData,
                         y = data, 
                         by.x = c("V1"), 
                         by.y = c(eval(DateColumnName)),
                         all.x = TRUE)
      
      # Zero Fill----
      FinalData <- RemixAutoML::ModelDataPrep(
        data = FinalData, 
        Impute = TRUE, 
        CharToFactor = FALSE, 
        RemoveDates = FALSE,
        MissFactor = "0",
        MissNum = 0,
        IgnoreCols = NULL)
      
      # Return data----
      return(data.table::setnames(
        FinalData, 
        old = c("V1"), 
        new = c(eval(DateColumnName))))
    } else {
      # Return otherwise
      return(data)
    }
  }
}
