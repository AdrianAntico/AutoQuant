#' @author Adrian Antico
#' @family Carma Helper
#' @noRd
#' @export
CARMA_GroupHierarchyCheck <- function(data = data,
                                      Group_Variables = GroupVariables,
                                      HierarchyGroups = HierarchGroups) {

  # Simple organization of option sets
  if(length(Group_Variables) > 1 & !is.null(HierarchyGroups)) {
    data[, eval(Group_Variables) := data.table::tstrsplit(GroupVar, " ")][, GroupVar := NULL]
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

#' CARMA_Define_Args
#'
#' CARMA_Define_Args is to help manage carma code
#'
#' @author Adrian Antico
#' @family Carma Helper
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
#' @export
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

#' CARMA_Get_IndepentVariablesPass
#'
#' CARMA_Get_IndepentVariablesPass is to help manage carma code
#'
#' @author Adrian Antico
#' @family Carma Helper
#' @param HierarchGroups Supply HierarchGroups
#' @export
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

#' CarmaH2OKeepVarsGDL
#'
#' CarmaH2OKeepVarsGDL is to help manage carma code
#'
#' @author Adrian Antico
#' @family Carma Helper
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
#' @export
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

#' CarmaXGBoostKeepVarsGDL
#'
#' CarmaXGBoostKeepVarsGDL is to help manage carma code
#'
#' @author Adrian Antico
#' @family Carma Helper
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
#' @export
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

#' CarmaCatBoostKeepVarsGDL
#'
#' CarmaCatBoostKeepVarsGDL is to help manage carma code
#'
#' @author Adrian Antico
#' @family Carma Helper
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
#' @export
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
                                     DateColumnName) {

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
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"ModTarget","Predictions",names(GroupVarVector),"ID",names(CalendarFeatures),"HolidayCounts"))
      } else {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"ModTarget","Predictions","GroupVar","ID",names(CalendarFeatures),"HolidayCounts"))
      }
    } else if(CalendarVariables == TRUE & HolidayVariable == FALSE) {
      if(!is.null(HierarchGroups)) {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"ModTarget","Predictions",names(GroupVarVector),"ID",names(CalendarFeatures)))
      } else {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"ModTarget","Predictions","GroupVar","ID",names(CalendarFeatures)))
      }
    } else if(CalendarVariables == FALSE & HolidayVariable == TRUE) {
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
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"Predictions",names(GroupVarVector),"ID",names(CalendarFeatures),"HolidayCounts"))
      } else {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"Predictions","GroupVar","ID",names(CalendarFeatures),"HolidayCounts"))
      }
    } else if(CalendarVariables == TRUE & HolidayVariable == FALSE) {
      if(!is.null(HierarchGroups)) {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"Predictions",names(GroupVarVector),"ID",names(CalendarFeatures)))
      } else {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"Predictions","GroupVar","ID",names(CalendarFeatures)))
      }
    } else if(CalendarVariables == FALSE & HolidayVariable == TRUE) {
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
    UpdateData <- UpdateData[order(get(DateColumnName))]
    UpdateData[, ID := .N:1]
    if(CalendarVariables == TRUE & HolidayVariable == TRUE) {
      if(!is.null(XREGS)) {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"Predictions","ID",names(CalendarFeatures),"HolidayCounts"))
        keep <- c(keep[1],setdiff(names(XREGS)[length(XREGS)],keep),keep[(2+length(setdiff(names(XREGS)[length(XREGS)],keep))):length(keep)])
      } else {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"Predictions","ID",names(CalendarFeatures),"HolidayCounts"))
      }
    } else if(CalendarVariables == TRUE & HolidayVariable == FALSE) {
      if(!is.null(XREGS)) {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"Predictions","ID",names(CalendarFeatures)))
        keep <- c(keep[1],setdiff(names(XREGS)[length(XREGS)],keep),keep[(2+length(setdiff(names(XREGS)[length(XREGS)],keep))):length(keep)])
      } else {
        keep <- unique(c(eval(DateColumnName),eval(TargetColumnName),"Predictions","ID",names(CalendarFeatures)))
      }
    } else if(CalendarVariables == FALSE & HolidayVariable == TRUE) {
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
