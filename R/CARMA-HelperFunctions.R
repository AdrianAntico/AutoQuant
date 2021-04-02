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
#' @noRd
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
#' @noRd
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
#' @noRd
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

#' @title CARMA_GroupHierarchyCheck
#'
#' @author Adrian Antico
#' @family Carma Helper
#'
#' @param data data fed into function
#' @param Group_Variables Takes GroupVariables from caram function
#' @param HierarchyGroups Vector of group variables
#' @noRd
CARMA_GroupHierarchyCheck <- function(data = data,
                                      Group_Variables = GroupVariables,
                                      HierarchyGroups = HierarchGroups) {

  # Simple organization of option sets
  if(length(Group_Variables) > 1 & !is.null(HierarchyGroups)) {
    if("GroupVar" %chin% names(data)) data[, eval(Group_Variables) := data.table::tstrsplit(GroupVar, " ")][, GroupVar := NULL]
    HierarchSupplyValue <- HierarchyGroups
  } else {
    HierarchSupplyValue <- NULL
  }
  if(!is.null(Group_Variables)) {
    IndependentSupplyValue <- CARMA_Get_IndepentVariablesPass(HierarchyGroups)
  } else {
    IndependentSupplyValue <- NULL
  }
  return(list(data = data, HierarchSupplyValue = HierarchyGroups, IndependentSupplyValue = IndependentSupplyValue))
}

#' @title CARMA_Define_Args
#'
#' @description CARMA_Define_Args is to help manage carma code
#'
#' @author Adrian Antico
#' @family Carma Helper
#'
#' @param TimeUnit = TimeUnit
#' @param TimeGroups = TimeGroups
#' @param HierarchGroups = HierarchGroups
#' @param GroupVariables = GroupVariables
#' @param FC_Periods = FC_Periods
#' @param PartitionType = PartitionType
#' @param TrainOnFull = TrainOnFull
#' @param SplitRatios = SplitRatios
#' @param SD_Periods = 0L turns it off, otherwise values must be greater than 1 such as c(2L,5L,6L,25L)
#' @param Skew_Periods = 0L turns it off, otherwise values must be greater than 2 such as c(3L,5L,6L,25L)
#' @param Kurt_Periods = 0L turns it off, otherwise values must be greater than 3 such as c(4L,5L,6L,25L)
#' @param Quantile_Periods = 0L turns it off, otherwise values must be greater than 3 such as c(5L,6L,25L)
#' @noRd
CARMA_Define_Args <- function(TimeUnit = NULL,
                              TimeGroups = NULL,
                              HierarchGroups = NULL,
                              GroupVariables = NULL,
                              FC_Periods = NULL,
                              PartitionType = NULL,
                              TrainOnFull = NULL,
                              SplitRatios = NULL,
                              SD_Periods = 0L,
                              Skew_Periods = 0L,
                              Kurt_Periods = 0L,
                              Quantile_Periods = 0L) {

  # TimeUnit and TimeGroups Args----
  TimeGroupPlaceHolder <- c()
  if(any(tolower(c("hours","hour","hr","hrs","hourly")) %chin% tolower(TimeGroups))) {
    TimeGroupPlaceHolder <- c(TimeGroupPlaceHolder, "hour")
  }
  if(any(tolower(c("hours","hour","hr","hrs","hourly")) %chin% tolower(TimeUnit))) {
    TimeUnit <- "hour"
  }
  if(any(tolower(c("days","day","dy","dd","d")) %chin% tolower(TimeGroups))) {
    TimeGroupPlaceHolder <- c(TimeGroupPlaceHolder, "day")
  }
  if(any(tolower(c("days","day","dy","dd","d")) %chin% tolower(TimeUnit))) {
    TimeUnit <- "day"
  }
  if(any(tolower(c("weeks","week","weaks","weak","wk","wkly","wks")) %chin% tolower(TimeGroups))) {
    TimeGroupPlaceHolder <- c(TimeGroupPlaceHolder, "weeks")
  }
  if(any(tolower(c("weeks","week","weaks","weak","wk","wkly","wks")) %chin% tolower(TimeUnit))) {
    TimeUnit <- "week"
  }
  if(any(tolower(c("months","month","mth","mnth","monthly","mnthly")) %chin% tolower(TimeGroups))) {
    TimeGroupPlaceHolder <- c(TimeGroupPlaceHolder, "months")
  }
  if(any(tolower(c("months","month","mth","mnth","monthly","mnthly")) %chin% tolower(TimeUnit))) {
    TimeUnit <- "months"
  }
  if(any(tolower(c("quarter","qarter","quarterly","q","qtly")) %chin% tolower(TimeGroups))) {
    TimeGroupPlaceHolder <- c(TimeGroupPlaceHolder, "quarter")
  }
  if(any(tolower(c("quarter","qarter","quarterly","q","qtly")) %chin% tolower(TimeUnit))) {
    TimeUnit <- "quarter"
  }
  if(any(tolower(c("years","year","annual","yearly","annually","ann","yr","yrly")) %chin% tolower(TimeGroups))) {
    TimeGroupPlaceHolder <- c(TimeGroupPlaceHolder, "year")
  }
  if(any(tolower(c("years","year","annual","yearly","annually","ann","yr","yrly")) %chin% tolower(TimeUnit))) {
    TimeUnit <- "year"
  }

  # IndependentVariablePass is for referencing the interaction group column names----
  IndepentVariablesPass <- CARMA_Get_IndepentVariablesPass(HierarchGroups)

  # FC Periods----
  if(FC_Periods <= 1L) {
    FC_Periods <- 2L
  } else {
    FC_Periods <- FC_Periods + 1L
  }

  # Check arguments----
  if (!(tolower(PartitionType) %chin% c("random", "time", "timeseries"))) {
    return("PartitionType needs to be one of 'random', 'time', or 'timeseries'")
  }
  if (tolower(PartitionType) == "timeseries" & is.null(GroupVariables)) {
    PartitionType <- "time"
  }

  # Return args----
  return(list(TimeUnit              = TimeUnit,
              TimeGroups            = TimeGroupPlaceHolder,
              IndepentVariablesPass = IndepentVariablesPass,
              HierarchGroups        = HierarchGroups,
              GroupVariables        = GroupVariables,
              FC_Periods            = FC_Periods))
}

#' @title CARMA_Get_IndepentVariablesPass
#'
#' CARMA_Get_IndepentVariablesPass is to help manage carma code
#'
#' @author Adrian Antico
#' @family Carma Helper
#' @param HierarchGroups Supply HierarchGroups
#' @noRd
CARMA_Get_IndepentVariablesPass <- function(HierarchGroups) {

  # Determine how to define IndependentVariablePass based on HierarchGroups----
  if(!is.null(HierarchGroups)) {
    for(zzz in seq_len(length(HierarchGroups))) {
      if(zzz == 1L) {
        IndepentVariablesPass <- HierarchGroups[zzz]
      } else {
        IndepentVariablesPass <- paste(IndepentVariablesPass, HierarchGroups[zzz], sep = "_")
      }
    }
  } else {
    IndepentVariablesPass <- "GroupVar"
  }

  # Return----
  return(IndepentVariablesPass)
}

#' @title CarmaH2OKeepVarsGDL
#'
#' @description CarmaH2OKeepVarsGDL is to help manage carma code
#'
#' @author Adrian Antico
#' @family Carma Helper
#'
#' @param data Supply data
#' @param IndepVarPassTRUE Name of the column used as a single grouping variable.
#' @param UpdateData Supply UpdateData
#' @param CalendarFeatures Supply CalendarFeatures
#' @param XREGS Supply XREGS
#' @param Difference Supply Difference
#' @param HierarchGroups Supply HierarchGroups
#' @param GroupVariables Supply GroupVariables
#' @param GroupVarVector Supply GroupVarVector
#' @param CalendarVariables Supply CalendarVariables
#' @param HolidayVariable Supply HolidayVariable
#' @param TargetColumnName Supply TargetColumnName
#' @param DateColumnName Supply DateColumnName
#' @noRd
CarmaH2OKeepVarsGDL <- function(data,
                                IndepVarPassTRUE = "GroupVar",
                                UpdateData,
                                CalendarFeatures,
                                XREGS,
                                Difference,
                                HierarchGroups,
                                GroupVariables,
                                GroupVarVector,
                                CalendarVariables = NULL,
                                HolidayVariable = NULL,
                                TargetColumnName,
                                DateColumnName) {

  if(any(is.na(UpdateData[["Predictions"]]))) {
    data.table::set(UpdateData, i = which(is.na(UpdateData[["Predictions"]])), j = "Predictions", value = 1.0)
  }
  if(Difference == TRUE & !is.null(GroupVariables)) {
    if(any(is.na(UpdateData[["ModTarget"]]))) {
      data.table::set(x = UpdateData, i = which(is.na(UpdateData[["ModTarget"]])), j = "ModTarget", value = 1.0)
    }
    if(!is.null(HierarchGroups)) {
      data.table::setorderv(x = UpdateData, cols = c(eval(GroupVariables), eval(DateColumnName)))
      UpdateData[, ID := .N:1, by = c(eval(GroupVariables))]
    } else {
      data.table::setorderv(x = UpdateData, cols = c("GroupVar", eval(DateColumnName)))
      UpdateData[, ID := .N:1, by = "GroupVar"]
    }
    if(!is.null(CalendarVariables) & !is.null(HolidayVariable)) {
      if(!is.null(HierarchGroups)) {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"ModTarget","Predictions",names(GroupVarVector),"ID",names(CalendarFeatures),"HolidayCounts"))
      } else {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"ModTarget","Predictions","GroupVar","ID",names(CalendarFeatures),"HolidayCounts"))
      }
    } else if(!is.null(CalendarVariables) & !is.null(HolidayVariable)) {
      if(!is.null(HierarchGroups)) {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"ModTarget","Predictions",names(GroupVarVector),"ID",names(CalendarFeatures)))
      } else {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"ModTarget","Predictions","GroupVar","ID",names(CalendarFeatures)))
      }
    } else if(!is.null(CalendarVariables) & !is.null(HolidayVariable)) {
      if(!is.null(HierarchGroups)) {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"ModTarget","Predictions",names(GroupVarVector),"ID","HolidayCounts"))
      } else {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"ModTarget","Predictions","GroupVar","ID","HolidayCounts"))
      }
    } else {
      if(!is.null(HierarchGroups)) {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"ModTarget","Predictions",names(GroupVarVector),"ID"))
      } else {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"ModTarget","Predictions","GroupVar","ID"))
      }
    }

    # Return data based on GDL condition in CARMA----
    if(!is.null(IndepVarPassTRUE)) {
      keep <- unique(c(keep,IndepVarPassTRUE))
      return(list(data = data.table::copy(UpdateData[, ..keep]), keep = keep))
    } else {
      return(list(data = data.table::copy(UpdateData[, ..keep]), keep = keep))
    }

  } else if (!is.null(GroupVariables)) {
    if(!is.null(HierarchGroups)) {
      data.table::setorderv(x = UpdateData, cols = c(eval(GroupVariables), eval(DateColumnName)))
      UpdateData[, ID := .N:1, by = c(eval(GroupVariables))]
    } else {
      data.table::setorderv(x = UpdateData, cols = c("GroupVar", eval(DateColumnName)))
      UpdateData[, ID := .N:1, by = "GroupVar"]
    }
    if(!is.null(CalendarVariables) & !is.null(HolidayVariable)) {
      if(!is.null(HierarchGroups)) {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"Predictions",names(GroupVarVector),"ID",names(CalendarFeatures),"HolidayCounts"))
      } else {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"Predictions","GroupVar","ID",names(CalendarFeatures),"HolidayCounts"))
      }
    } else if(!is.null(CalendarVariables) & !is.null(HolidayVariable)) {
      if(!is.null(HierarchGroups)) {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"Predictions",names(GroupVarVector),"ID",names(CalendarFeatures)))
      } else {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"Predictions","GroupVar","ID",names(CalendarFeatures)))
      }
    } else if(!is.null(CalendarVariables) & !is.null(HolidayVariable)) {
      if(!is.null(HierarchGroups)) {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"Predictions",names(GroupVarVector),"ID","HolidayCounts"))
      } else {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"Predictions","GroupVar","ID","HolidayCounts"))
      }
    } else {
      if(!is.null(HierarchGroups)) {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"Predictions",names(GroupVarVector),"ID"))
      } else {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"Predictions","GroupVar","ID"))
      }
    }

    # Return data based on GDL condition in CARMA----
    if(!is.null(IndepVarPassTRUE)) {
      keep <- unique(c(keep,IndepVarPassTRUE))
      return(list(data = data.table::copy(UpdateData[, ..keep]), keep = keep))
    } else {
      return(list(data = data.table::copy(UpdateData[, ..keep]), keep = keep))
    }

  } else {

    if(!is.null(CalendarVariables) & !is.null(HolidayVariable)) {
      if(!is.null(XREGS)) {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"Predictions","ID",names(CalendarFeatures),"HolidayCounts"))
        keep <- c(keep[1:2],setdiff(names(XREGS)[length(XREGS)],keep),keep[(2+length(setdiff(names(XREGS)[length(XREGS)],keep))):length(keep)])
      } else {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"Predictions","ID",names(CalendarFeatures),"HolidayCounts"))
      }
    } else if(!is.null(CalendarVariables) & !is.null(HolidayVariable)) {
      if(!is.null(XREGS)) {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"Predictions","ID",names(CalendarFeatures)))
        keep <- c(keep[1:2],setdiff(names(XREGS)[length(XREGS)],keep),keep[(2+length(setdiff(names(XREGS)[length(XREGS)],keep))):length(keep)])
      } else {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"Predictions","ID",names(CalendarFeatures)))
      }
    } else if(!is.null(CalendarVariables) & !is.null(HolidayVariable)) {
      if(!is.null(XREGS)) {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"Predictions","ID","HolidayCounts"))
        keep <- c(keep[1:2],setdiff(names(XREGS)[length(XREGS)],keep),keep[(2+length(setdiff(names(XREGS)[length(XREGS)],keep))):length(keep)])
      } else {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"Predictions","ID","HolidayCounts"))
      }
    } else {
      if(!is.null(XREGS)) {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"Predictions","ID"))
        keep <- c(keep[1:2],setdiff(names(XREGS)[length(XREGS)],keep),keep[(2+length(setdiff(names(XREGS)[length(XREGS)],keep))):length(keep)])
      } else {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"Predictions","ID"))
      }
    }

    # Return data based on GDL condition in CARMA----
    if(!is.null(IndepVarPassTRUE)) {
      keep <- unique(c(keep,IndepVarPassTRUE))
      return(list(data = data.table::copy(UpdateData[, ..keep]), keep = keep))
    } else {
      return(list(data = data.table::copy(UpdateData[, ..keep]), keep = keep))
    }
  }
}

#' @title CarmaXGBoostKeepVarsGDL
#'
#' @description CarmaXGBoostKeepVarsGDL is to help manage carma code
#'
#' @author Adrian Antico
#' @family Carma Helper
#'
#' @param data Supply data
#' @param IndepVarPassTRUE Name of the column used as a single grouping variable.
#' @param UpdateData Supply UpdateData
#' @param CalendarFeatures Supply CalendarFeatures
#' @param XREGS Supply XREGS
#' @param Difference Supply Difference
#' @param HierarchGroups Supply HierarchGroups
#' @param GroupVariables Supply GroupVariables
#' @param GroupVarVector Supply GroupVarVector
#' @param CalendarVariables Supply CalendarVariables
#' @param HolidayVariable Supply HolidayVariable
#' @param TargetColumnName Supply TargetColumnName
#' @param DateColumnName Supply DateColumnName
#' @noRd
CarmaXGBoostKeepVarsGDL <- function(data,
                                    IndepVarPassTRUE = "GroupVar",
                                    UpdateData,
                                    CalendarFeatures,
                                    XREGS,
                                    Difference,
                                    HierarchGroups,
                                    GroupVariables,
                                    GroupVarVector,
                                    CalendarVariables = NULL,
                                    HolidayVariable = NULL,
                                    TargetColumnName,
                                    DateColumnName) {

  if(any(is.na(UpdateData[["Predictions"]]))) data.table::set(UpdateData, i = which(is.na(UpdateData[["Predictions"]])), j = "Predictions", value = 1.0)
  if(Difference & !is.null(GroupVariables)) {
    if(any(is.na(UpdateData[["ModTarget"]]))) {
      data.table::set(x = UpdateData, i = which(is.na(UpdateData[["ModTarget"]])), j = "ModTarget", value = 1.0)
    }
    if(!is.null(HierarchGroups)) {
      data.table::setorderv(x = UpdateData, cols = c(eval(GroupVariables), eval(DateColumnName)))
      UpdateData[, ID := .N:1, by = c(eval(GroupVariables))]
    } else {
      data.table::setorderv(x = UpdateData, cols = c("GroupVar", eval(DateColumnName)))
      UpdateData[, ID := .N:1, by = "GroupVar"]
    }
    if(!is.null(CalendarVariables) & !is.null(HolidayVariable)) {
      if(!is.null(HierarchGroups)) {
        if(!"GroupVar" %chin% names(UpdateData)) {
          if("GroupVar" %chin% names(GroupVarVector)) {
            data.table::set(GroupVarVector, j = "GroupVar", value = NULL)
          }
        }
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"ModTarget","Predictions",names(GroupVarVector),"ID",names(CalendarFeatures),"HolidayCounts"))
      } else {
        if(!"GroupVar" %chin% names(UpdateData)) {
          if("GroupVar" %chin% names(GroupVarVector)) {
            data.table::set(GroupVarVector, j = "GroupVar", value = NULL)
          }
        }
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"ModTarget","Predictions",IndepVarPassTRUE,"ID",names(CalendarFeatures),"HolidayCounts"))
      }
    } else if(!is.null(CalendarVariables) & !is.null(HolidayVariable)) {
      if(!is.null(HierarchGroups)) {
        if(!"GroupVar" %chin% names(UpdateData)) {
          if("GroupVar" %chin% names(GroupVarVector)) {
            data.table::set(GroupVarVector, j = "GroupVar", value = NULL)
          }
        }
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"ModTarget","Predictions",names(GroupVarVector),"ID",names(CalendarFeatures)))
      } else {
        if(!"GroupVar" %chin% names(UpdateData)) {
          if("GroupVar" %chin% names(GroupVarVector)) {
            data.table::set(GroupVarVector, j = "GroupVar", value = NULL)
          }
        }
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"ModTarget","Predictions",IndepVarPassTRUE,"ID",names(CalendarFeatures)))
      }
    } else if(!is.null(CalendarVariables) & !is.null(HolidayVariable)) {
      if(!is.null(HierarchGroups)) {
        if(!"GroupVar" %chin% names(UpdateData)) {
          if("GroupVar" %chin% names(GroupVarVector)) {
            data.table::set(GroupVarVector, j = "GroupVar", value = NULL)
          }
        }
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"ModTarget","Predictions",names(GroupVarVector),"ID","HolidayCounts"))
      } else {
        if(!"GroupVar" %chin% names(UpdateData)) {
          if("GroupVar" %chin% names(GroupVarVector)) {
            data.table::set(GroupVarVector, j = "GroupVar", value = NULL)
          }
        }
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"ModTarget","Predictions",IndepVarPassTRUE,"ID","HolidayCounts"))
      }
    } else {
      if(!is.null(HierarchGroups)) {
        if(!"GroupVar" %chin% names(UpdateData)) {
          if("GroupVar" %chin% names(GroupVarVector)) {
            data.table::set(GroupVarVector, j = "GroupVar", value = NULL)
          }
        }
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"ModTarget","Predictions",names(GroupVarVector),"ID"))
      } else {
        if(!"GroupVar" %chin% names(UpdateData)) {
          if("GroupVar" %chin% names(GroupVarVector)) {
            data.table::set(GroupVarVector, j = "GroupVar", value = NULL)
          }
        }
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"ModTarget","Predictions","GroupVar","ID"))
      }
    }

    # Return data based on GDL condition in CARMA----
    if(!is.null(IndepVarPassTRUE)) {
      keep <- unique(c(keep,IndepVarPassTRUE))
      return(list(data = data.table::copy(UpdateData[, ..keep]), keep = keep))
    } else {
      return(list(data = data.table::copy(UpdateData[, ..keep]), keep = keep))
    }

  } else if(!is.null(GroupVariables)) {
    if(!is.null(HierarchGroups)) {
      data.table::setorderv(x = UpdateData, cols = c(eval(GroupVariables), eval(DateColumnName)))
      UpdateData[, ID := .N:1, by = c(eval(GroupVariables))]
    } else {
      data.table::setorderv(x = UpdateData, cols = c("GroupVar", eval(DateColumnName)))
      UpdateData[, ID := .N:1, by = "GroupVar"]
    }
    if(!is.null(CalendarVariables) & !is.null(HolidayVariable)) {
      if(!is.null(HierarchGroups)) {
        if(!"GroupVar" %chin% names(UpdateData)) {
          if("GroupVar" %chin% names(GroupVarVector)) {
            data.table::set(GroupVarVector, j = "GroupVar", value = NULL)
          }
          keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"Predictions",names(GroupVarVector)[2:length(GroupVarVector)],"ID",names(CalendarFeatures),"HolidayCounts"))
        } else {
          keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"Predictions",names(GroupVarVector),"ID",names(CalendarFeatures),"HolidayCounts"))
        }
      } else {
        if(!"GroupVar" %chin% names(UpdateData)) {
          if("GroupVar" %chin% names(GroupVarVector)) data.table::set(GroupVarVector, j = "GroupVar", value = NULL)
          keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"Predictions",IndepVarPassTRUE,"ID",names(CalendarFeatures),"HolidayCounts"))
        } else {
          keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"Predictions","GroupVar","ID",names(CalendarFeatures),"HolidayCounts"))
        }
      }
    } else if(!is.null(CalendarVariables) & !is.null(HolidayVariable)) {
      if(!is.null(HierarchGroups)) {
        if(!"GroupVar" %chin% names(UpdateData)) {
          if("GroupVar" %chin% names(GroupVarVector)) data.table::set(GroupVarVector, j = "GroupVar", value = NULL)
          keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"Predictions",names(GroupVarVector)[2:length(GroupVarVector)],"ID",names(CalendarFeatures)))
        } else {
          keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"Predictions",names(GroupVarVector),"ID",names(CalendarFeatures)))
        }
      } else {
        if(!"GroupVar" %chin% names(UpdateData)) {
          if("GroupVar" %chin% names(GroupVarVector)) data.table::set(GroupVarVector, j = "GroupVar", value = NULL)
          keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"Predictions",IndepVarPassTRUE,"ID",names(CalendarFeatures)))
        } else {
          keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"Predictions","GroupVar","ID",names(CalendarFeatures)))
        }
      }
    } else if(!is.null(CalendarVariables) & !is.null(HolidayVariable)) {
      if(!is.null(HierarchGroups)) {
        if(!"GroupVar" %chin% names(UpdateData)) {
          if("GroupVar" %chin% names(GroupVarVector)) data.table::set(GroupVarVector, j = "GroupVar", value = NULL)
          keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"Predictions",names(GroupVarVector)[2:length(GroupVarVector)],"ID","HolidayCounts"))
        } else {
          keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"Predictions",names(GroupVarVector),"ID","HolidayCounts"))
        }
      } else {
        if(!"GroupVar" %chin% names(UpdateData)) {
          if("GroupVar" %chin% names(GroupVarVector)) data.table::set(GroupVarVector, j = "GroupVar", value = NULL)
          keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"Predictions",IndepVarPassTRUE,"ID","HolidayCounts"))
        } else {
          keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"Predictions","GroupVar","ID","HolidayCounts"))
        }
      }
    } else {
      if(!is.null(HierarchGroups)) {
        if(!"GroupVar" %chin% names(UpdateData)) {
          if("GroupVar" %chin% names(GroupVarVector)) data.table::set(GroupVarVector, j = "GroupVar", value = NULL)
          keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"Predictions",names(GroupVarVector)[2:length(GroupVarVector)],"ID"))
        } else {
          keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"Predictions",names(GroupVarVector),"ID"))
        }
      } else {
        if(!"GroupVar" %chin% names(UpdateData)) {
          if("GroupVar" %chin% names(GroupVarVector)) data.table::set(GroupVarVector, j = "GroupVar", value = NULL)
          keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"Predictions",IndepVarPassTRUE,"ID"))
        } else {
          keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"Predictions","GroupVar","ID"))
        }
      }
    }

    # Return data based on GDL condition in CARMA----
    if(!is.null(IndepVarPassTRUE)) {
      keep <- unique(c(keep,IndepVarPassTRUE))
      return(list(data = data.table::copy(UpdateData[, ..keep]), keep = keep))
    } else {
      return(list(data = data.table::copy(UpdateData[, ..keep]), keep = keep))
    }

  } else {
    UpdateData <- UpdateData[order(get(DateColumnName))]
    UpdateData[, ID := .N:1]
    if(!is.null(CalendarVariables) & !is.null(HolidayVariable)) {
      if(!is.null(XREGS)) {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"Predictions","ID",names(CalendarFeatures),"HolidayCounts"))
        keep <- c(keep[1:2],setdiff(names(XREGS)[length(XREGS)],keep),keep[(2+length(setdiff(names(XREGS)[length(XREGS)],keep))):length(keep)])
      } else {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"Predictions","ID",names(CalendarFeatures),"HolidayCounts"))
      }
    } else if(!is.null(CalendarVariables) & !is.null(HolidayVariable)) {
      if(!is.null(XREGS)) {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"Predictions","ID",names(CalendarFeatures)))
        keep <- c(keep[1:2],setdiff(names(XREGS)[length(XREGS)],keep),keep[(2+length(setdiff(names(XREGS)[length(XREGS)],keep))):length(keep)])
      } else {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"Predictions","ID",names(CalendarFeatures)))
      }
    } else if(!is.null(CalendarVariables) & !is.null(HolidayVariable)) {
      if(!is.null(XREGS)) {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"Predictions","ID","HolidayCounts"))
        keep <- c(keep[1:2],setdiff(names(XREGS)[length(XREGS)],keep),keep[(2+length(setdiff(names(XREGS)[length(XREGS)],keep))):length(keep)])
      } else {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"Predictions","ID","HolidayCounts"))
      }
    } else {
      if(!is.null(XREGS)) {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"Predictions","ID"))
        keep <- c(keep[1:2],setdiff(names(XREGS)[length(XREGS)],keep),keep[(2+length(setdiff(names(XREGS)[length(XREGS)],keep))):length(keep)])
      } else {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"Predictions","ID"))
      }
    }

    # Return data based on GDL condition in CARMA----
    if(!is.null(IndepVarPassTRUE)) {
      keep <- unique(c(keep,IndepVarPassTRUE))
      return(list(data = data.table::copy(UpdateData[, ..keep]), keep = keep))
    } else {
      return(list(data = data.table::copy(UpdateData[, ..keep]), keep = keep))
    }
  }
}

#' @title CarmaCatBoostKeepVarsGDL
#'
#' @description CarmaCatBoostKeepVarsGDL is to help manage carma code
#'
#' @author Adrian Antico
#' @family Carma Helper
#'
#' @param data Supply data
#' @param IndepVarPassTRUE Name of the column used as a single grouping variable.
#' @param UpdateData Supply UpdateData
#' @param CalendarFeatures Supply CalendarFeatures
#' @param XREGS Supply XREGS
#' @param Difference Supply Difference
#' @param HierarchGroups Supply HierarchGroups
#' @param GroupVariables Supply GroupVariables
#' @param GroupVarVector Supply GroupVarVector
#' @param CalendarVariables Supply CalendarVariables
#' @param HolidayVariable Supply HolidayVariable
#' @param TargetColumnName Supply TargetColumnName
#' @param DateColumnName Supply DateColumnName
#' @param Preds Supply Preds
#' @noRd
CarmaCatBoostKeepVarsGDL <- function(data,
                                     IndepVarPassTRUE = "GroupVar",
                                     UpdateData,
                                     CalendarFeatures,
                                     XREGS,
                                     Difference,
                                     HierarchGroups,
                                     GroupVariables,
                                     GroupVarVector,
                                     CalendarVariables,
                                     HolidayVariable,
                                     TargetColumnName,
                                     DateColumnName,
                                     Preds) {

  if(any(is.na(UpdateData[["Predictions"]]))) {
    data.table::set(x = UpdateData, i = which(is.na(UpdateData[["Predictions"]])), j = "Predictions", value = 1.0)
  }
  if(Difference == TRUE & !is.null(GroupVariables)) {
    if(any(is.na(UpdateData[["ModTarget"]]))) {
      data.table::set(x = UpdateData, i = which(is.na(UpdateData[["ModTarget"]])), j = "ModTarget", value = 1.0)
    }
    if(!is.null(HierarchGroups)) {
      data.table::setorderv(x = UpdateData, cols = c(eval(GroupVariables), eval(DateColumnName)))
      UpdateData[, ID := .N:1, by = c(eval(GroupVariables))]
    } else {
      data.table::setorderv(x = UpdateData, cols = c("GroupVar", eval(DateColumnName)))
      UpdateData[, ID := .N:1, by = "GroupVar"]
    }
    if(CalendarVariables == TRUE & HolidayVariable == TRUE) {
      if(!is.null(HierarchGroups)) {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"ModTarget",names(GroupVarVector),"ID",names(CalendarFeatures),"HolidayCounts", names(Preds)[which(names(Preds) %like% "Predictions")]))
      } else {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"ModTarget","GroupVar","ID",names(CalendarFeatures),"HolidayCounts",names(Preds)[which(names(Preds) %like% "Predictions")]))
      }
    } else if(CalendarVariables == TRUE & HolidayVariable == FALSE) {
      if(!is.null(HierarchGroups)) {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"ModTarget",names(GroupVarVector),"ID",names(CalendarFeatures),names(Preds)[which(names(Preds) %like% "Predictions")]))
      } else {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"ModTarget","GroupVar","ID",names(CalendarFeatures),names(Preds)[which(names(Preds) %like% "Predictions")]))
      }
    } else if(CalendarVariables == FALSE & HolidayVariable == TRUE) {
      if(!is.null(HierarchGroups)) {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"ModTarget",names(GroupVarVector),"ID","HolidayCounts",names(Preds)[which(names(Preds) %like% "Predictions")]))
      } else {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"ModTarget","GroupVar","ID","HolidayCounts",names(Preds)[which(names(Preds) %like% "Predictions")]))
      }
    } else {
      if(!is.null(HierarchGroups)) {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"ModTarget",names(GroupVarVector),"ID",names(Preds)[which(names(Preds) %like% "Predictions")]))
      } else {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"ModTarget","GroupVar","ID",names(Preds)[which(names(Preds) %like% "Predictions")]))
      }
    }

    # Return data based on GDL condition in CARMA----
    if(!is.null(IndepVarPassTRUE)) {
      keep <- unique(c(keep,IndepVarPassTRUE))
      return(list(data = data.table::copy(UpdateData[, ..keep]), keep = keep))
    } else {
      return(list(data = data.table::copy(UpdateData[, ..keep]), keep = keep))
    }
  } else if(!is.null(GroupVariables)) {
    if(!is.null(HierarchGroups)) {
      data.table::setorderv(x = UpdateData, cols = c(eval(GroupVariables), eval(DateColumnName)))
      UpdateData[, ID := .N:1, by = c(eval(GroupVariables))]
    } else {
      data.table::setorderv(x = UpdateData, cols = c("GroupVar", eval(DateColumnName)))
      UpdateData[, ID := .N:1, by = "GroupVar"]
    }
    if(CalendarVariables == TRUE & HolidayVariable == TRUE) {
      if(!is.null(HierarchGroups)) {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),names(GroupVarVector),"ID",names(CalendarFeatures),"HolidayCounts",names(Preds)[which(names(Preds) %like% "Predictions")]))
      } else {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"GroupVar","ID",names(CalendarFeatures),"HolidayCounts",names(Preds)[which(names(Preds) %like% "Predictions")]))
      }
    } else if(CalendarVariables == TRUE & HolidayVariable == FALSE) {
      if(!is.null(HierarchGroups)) {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),names(GroupVarVector),"ID",names(CalendarFeatures),names(Preds)[which(names(Preds) %like% "Predictions")]))
      } else {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"GroupVar","ID",names(CalendarFeatures),names(Preds)[which(names(Preds) %like% "Predictions")]))
      }
    } else if(CalendarVariables == FALSE & HolidayVariable == TRUE) {
      if(!is.null(HierarchGroups)) {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),names(GroupVarVector),"ID","HolidayCounts",names(Preds)[which(names(Preds) %like% "Predictions")]))
      } else {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"GroupVar","ID","HolidayCounts",names(Preds)[which(names(Preds) %like% "Predictions")]))
      }
    } else {
      if(!is.null(HierarchGroups)) {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),names(GroupVarVector),"ID",names(Preds)[which(names(Preds) %like% "Predictions")]))
      } else {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"GroupVar","ID",names(Preds)[which(names(Preds) %like% "Predictions")]))
      }
    }

    # Return data based on GDL condition in CARMA----
    if(!is.null(IndepVarPassTRUE)) {
      keep <- unique(c(keep,IndepVarPassTRUE))
      return(list(data = data.table::copy(UpdateData[, ..keep]), keep = keep))
    } else {
      return(list(data = data.table::copy(UpdateData[, ..keep]), keep = keep))
    }

  } else {
    UpdateData <- UpdateData[order(get(DateColumnName))]
    UpdateData[, ID := .N:1]
    if(CalendarVariables == TRUE & HolidayVariable == TRUE) {
      if(!is.null(XREGS)) {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"ID",names(CalendarFeatures),"HolidayCounts",names(Preds)[which(names(Preds) %like% "Predictions")]))
        keep <- c(keep[1],setdiff(names(XREGS)[length(XREGS)],keep),keep[(2+length(setdiff(names(XREGS)[length(XREGS)],keep))):length(keep)])
      } else {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"ID",names(CalendarFeatures),"HolidayCounts",names(Preds)[which(names(Preds) %like% "Predictions")]))
      }
    } else if(CalendarVariables == TRUE & HolidayVariable == FALSE) {
      if(!is.null(XREGS)) {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"ID",names(CalendarFeatures),names(Preds)[which(names(Preds) %like% "Predictions")]))
        keep <- c(keep[1],setdiff(names(XREGS)[length(XREGS)],keep),keep[(2+length(setdiff(names(XREGS)[length(XREGS)],keep))):length(keep)])
      } else {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"ID",names(CalendarFeatures),names(Preds)[which(names(Preds) %like% "Predictions")]))
      }
    } else if(CalendarVariables == FALSE & HolidayVariable == TRUE) {
      if(!is.null(XREGS)) {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"ID","HolidayCounts",names(Preds)[which(names(Preds) %like% "Predictions")]))
        keep <- c(keep[1:2],setdiff(names(XREGS)[length(XREGS)],keep),keep[(2+length(setdiff(names(XREGS)[length(XREGS)],keep))):length(keep)])
      } else {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"ID","HolidayCounts",names(Preds)[which(names(Preds) %like% "Predictions")]))
      }
    } else {
      if(!is.null(XREGS)) {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"ID",names(Preds)[which(names(Preds) %like% "Predictions")]))
        keep <- c(keep[1:2],setdiff(names(XREGS)[length(XREGS)],keep),keep[(2+length(setdiff(names(XREGS)[length(XREGS)],keep))):length(keep)])
      } else {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"ID",names(Preds)[which(names(Preds) %like% "Predictions")]))
      }
    }

    # Return data based on GDL condition in CARMA----
    if(!is.null(IndepVarPassTRUE)) {
      keep <- unique(c(keep,IndepVarPassTRUE))
      return(list(data = data.table::copy(UpdateData[, ..keep]), keep = keep))
    } else {
      return(list(data = data.table::copy(UpdateData[, ..keep]), keep = keep))
    }
  }
}
