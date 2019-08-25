#' An Automated Feature Engineering Function Using data.table frollmean
#'
#' Builds autoregressive and moving average from target columns and distributed lags and distributed moving average for independent features distributed across time. On top of that, you can also create time between instances along with their associated lags and moving averages. This function works for data with groups and without groups.
#' @author Adrian Antico
#' @family Feature Engineering
#' @param data A data.table you want to run the function on
#' @param lags A numeric vector of the specific lags you want to have generated. You must include 1 if WindowingLag = 1.
#' @param periods A numeric vector of the specific rolling statistics window sizes you want to utilize in the calculations.
#' @param statsNames A character vector of the corresponding names to create for the rollings stats variables.
#' @param targets A character vector of the column names for the reference column in which you will build your lags and rolling stats
#' @param groupingVars A character vector of categorical variable names you will build your lags and rolling stats by
#' @param sortDateName The column name of your date column used to sort events over time
#' @param timeDiffTarget Specify a desired name for features created for time between events. Set to NULL if you don't want time between events features created.
#' @param timeAgg List the time aggregation level for the time between events features, such as "hour", "day", "week", "month", "quarter", or "year"
#' @param WindowingLag Set to 0 to build rolling stats off of target columns directly or set to 1 to build the rolling stats off of the lag-1 target
#' @param Type List either "Lag" if you want features built on historical values or "Lead" if you want features built on future values
#' @param Timer Set to TRUE if you percentage complete tracker printout
#' @param SimpleImpute Set to TRUE for factor level imputation of "0" and numeric imputation of -1
#' @return data.table of original data plus created lags, rolling stats, and time between event lags and rolling stats
#' @examples
#' N = 25116
#' data <- data.table::data.table(DateTime = as.Date(Sys.time()),
#'                                Target = stats::filter(rnorm(N,
#'                                                             mean = 50,
#'                                                             sd = 20),
#'                                                       filter=rep(1,10),
#'                                                       circular=TRUE))
#' data[, temp := seq(1:N)][, DateTime := DateTime - temp][, temp := NULL]
#' data <- data[order(DateTime)]
#' data <- DT_GDL_Feature_Engineering(data,
#'                                    lags           = c(seq(1,5,1)),
#'                                    periods        = c(3,5,10,15,20,25),
#'                                    statsNames     = c("MA"),
#'                                    targets        = c("Target"),
#'                                    groupingVars   = NULL,
#'                                    sortDateName   = "DateTime",
#'                                    timeDiffTarget = c("Time_Gap"),
#'                                    timeAgg        = c("days"),
#'                                    WindowingLag   = 1,
#'                                    Type           = "Lag",
#'                                    Timer          = TRUE,
#'                                    SimpleImpute   = TRUE)
#' @export
DT_GDL_Feature_Engineering <- function(data,
                                       lags           = c(seq(1, 50, 1)),
                                       periods        = c(seq(5, 95, 5)),
                                       statsNames     = c("MA"),
                                       targets        = c("qty"),
                                       groupingVars   = c("Group1",
                                                          "Group2"),
                                       sortDateName   = c("date"),
                                       timeDiffTarget = c("TimeDiffName"),
                                       timeAgg        = c("days"),
                                       WindowingLag   = 0,
                                       Type           = c("Lag"),
                                       Timer          = TRUE,
                                       SimpleImpute   = TRUE) {
  # Argument Checks----
  if (is.null(lags) & WindowingLag == 1) {
    lags <- 1
  }
  if (!(1 %in% lags) & WindowingLag == 1) {
    lags <- c(1, lags)
  }
  if (any(lags < 0)) {
    warning("lags need to be positive integers")
  }
  if (!is.character(statsNames)) {
    warning("statsNames needs to be a character scalar or vector")
  }
  if (!is.null(groupingVars)) {
    if (!is.character(groupingVars)) {
      warning("groupingVars needs to be a character scalar or vector")
    }
  }
  if (!is.character(targets)) {
    warning("targets needs to be a character scalar or vector")
  }
  if (!is.character(sortDateName)) {
    warning("sortDateName needs to be a character scalar or vector")
  }
  if (!is.null(timeDiffTarget)) {
    if (!is.character(timeDiffTarget)) {
      warning("timeDiffTarget needs to be a character scalar or vector")
    }
  }
  if (!is.null(timeAgg)) {
    if (!is.character(timeAgg)) {
      warning("timeAgg needs to be a character scalar or vector")
    }
  }
  if (!(WindowingLag %in% c(0, 1))) {
    warning("WindowingLag needs to be either 0 or 1")
  }
  if (!(tolower(Type) %chin% c("lag", "lead"))) {
    warning("Type needs to be either Lag or Lead")
  }
  if (!is.logical(Timer)) {
    warning("Timer needs to be TRUE or FALSE")
  }
  if (!is.logical(SimpleImpute)) {
    warning("SimpleImpute needs to be TRUE or FALSE")
  }
  
  # Convert to data.table if not already----
  if (!data.table::is.data.table(data))
    data <- data.table::as.data.table(data)
  
  # Ensure target is numeric----
  for(t in targets) {
    data[, eval(t) := as.numeric(get(t))]    
  }
  
  # Set up counter for countdown----
  if (!is.null(timeDiffTarget)) {
    tarNum <- length(targets) + 1
  } else {
    tarNum <- length(targets)
  }
  
  # Ensure enough columns are allocated beforehand----
  if(is.null(timeDiffTarget)) {
    if(ncol(data) +
       (max(lags + 1) + max(periods + 1)) * tarNum > data.table::truelength(data)) {
      data.table::alloc.col(DT = data, 
                            n = ncol(data) +
                              (max(lags + 1) + max(periods + 1)) * tarNum)
    }
  } else {
    if(ncol(data) +
       (max(lags + 1) + max(periods + 1)) * tarNum * 2 > data.table::truelength(data)) {
      data.table::alloc.col(DT = data, 
                            n = ncol(data) +
                              (max(lags + 1) + max(periods + 1)) * tarNum * 2)
    }
  }
  
  # Begin feature engineering----
  if (!is.null(groupingVars)) {
    for (i in seq_along(groupingVars)) {
      Targets <- targets
      
      # Sort data----
      if (tolower(Type) == "lag") {
        colVar <- c(groupingVars[i], sortDateName[1])
        data.table::setorderv(data, colVar, order = 1)
      } else {
        colVar <- c(groupingVars[i], sortDateName[1])
        data.table::setorderv(data, colVar, order = -1)
      }
      
      # Lags----
      LAG_Names <- c()
      for (t in Targets) {
        LAG_Names <- c(LAG_Names, paste0(groupingVars[i],"_LAG_", lags, "_", t))
      }
      
      # Build features----
      data[, paste0(LAG_Names) := data.table::shift(.SD, 
                                                    n = lags, 
                                                    type = "lag"), 
           by = get(groupingVars[i]), .SDcols = Targets]
      
      # Time lags----
      if (!is.null(timeDiffTarget)) {
        # Lag the dates first----
        data[, paste0(groupingVars[i],
                      "TEMP",
                      lags) := data.table::shift(get(sortDateName),
                                                 n = lags,
                                                 type = "lag"),
             by = get(groupingVars[i])]
        
        # Difference the lag dates----
        if (WindowingLag != 0) {
          for (l in seq_along(lags)) {
            if (l == 1) {
              data[, paste0(groupingVars[i],
                            timeDiffTarget,
                            lags[l]) := as.numeric(difftime(
                              get(sortDateName),
                              get(paste0(
                                groupingVars[i], "TEMP", lags[l]
                              )),
                              units = eval(timeAgg)
                            )), by = get(groupingVars[i])]
            } else {
              data[, paste0(groupingVars[i],
                            timeDiffTarget,
                            lags[l]) := as.numeric(difftime(get(
                              paste0(groupingVars[i], "TEMP", (lags[l - 1]))
                            ),
                            get(
                              paste0(groupingVars[i], "TEMP", lags[l])
                            ),
                            units = eval(timeAgg))), by = get(groupingVars[i])]
            }
          }
        } else {
          for (l in seq_along(lags)) {
            if (l == 1) {
              data[, paste0(groupingVars[i],
                            timeDiffTarget,
                            lags[l]) := as.numeric(difftime(
                              get(sortDateName),
                              get(paste0(
                                groupingVars[i], "TEMP", lags[l]
                              )),
                              units = eval(timeAgg)
                            )), by = get(groupingVars[i])]
            } else {
              data[, paste0(groupingVars[i],
                            timeDiffTarget,
                            lags[l]) := as.numeric(difftime(get(
                              paste0(groupingVars[i], "TEMP", (lags[l - 1]))
                            ),
                            get(
                              paste0(groupingVars[i], "TEMP", lags[l])
                            ),
                            units = eval(timeAgg))), by = get(groupingVars[i])]
            }
          }
        }
        
        # Remove temporary lagged dates----
        for (l in seq_along(lags)) {
          data[, paste0(groupingVars[i], "TEMP",
                        lags[l]) := NULL]
        }
        
        # Store new target----
        timeTarget <- paste0(groupingVars[i],
                             timeDiffTarget, "1")
      }
      
      # Define targets----
      if (WindowingLag != 0) {
        if (!is.null(timeDiffTarget)) {
          Targets <-
            c(paste0(groupingVars[i],
                     "_LAG_",
                     WindowingLag,
                     "_",
                     Targets),
              timeTarget)
        } else {
          Targets <-
            c(paste0(groupingVars[i],
                     "_LAG_",
                     WindowingLag,
                     "_",
                     Targets))
        }
      } else {
        if (!is.null(timeDiffTarget)) {
          Targets <- c(Targets, timeTarget)
        } else {
          Targets <- Targets
        }
      }
      
      # Moving stats----
      MA_Names <- c()
      for (t in Targets) {
        for (j in seq_along(periods)) {
          for (k in seq_along(statsNames)) {
            MA_Names <- c(MA_Names,
                          paste0(groupingVars[i],
                                 statsNames[k],
                                 "_",
                                 periods[j],
                                 "_",
                                 t))
          }
        }
      }
      data[, paste0(MA_Names) := data.table::frollmean(
        x = .SD,
        n = periods,
        fill = NA,
        algo = "fast",
        align = "right",
        na.rm = TRUE,
        hasNA = TRUE,
        adaptive = FALSE
      ),
      by = get(groupingVars[i]), .SDcols = Targets]
    }
    
    # Replace any inf values with NA----
    for (col in seq_along(data)) {
      data.table::set(data,
                      j = col,
                      value = replace(data[[col]],
                                      is.infinite(data[[col]]), NA))
    }
    
    # Turn character columns into factors----
    for (col in seq_along(data)) {
      if (is.character(data[[col]])) {
        data.table::set(data, j = col, value = as.factor(data[[col]]))
      }
    }
    
    # Impute missing values----
    if (SimpleImpute) {
      for (j in seq_along(data)) {
        if (is.factor(data[[j]])) {
          data.table::set(data,
                          which(!(data[[j]] %in% levels(data[[j]]))), j, "0")
        } else {
          data.table::set(data,
                          which(is.na(data[[j]])), j,-1)
        }
      }
    }
    
    # Done!!----
    return(data)
    
  } else {
    if (tolower(Type) == "lag") {
      colVar <- c(sortDateName[1])
      data.table::setorderv(data, colVar, order = 1)
    } else {
      colVar <- c(sortDateName[1])
      data.table::setorderv(data, colVar, order = -1)
    }
    Targets <- targets
    
    # Build features----
    for(t in Targets) {
      data.table::set(
        data,
        j = paste0("LAG_", lags, "_", t),
        value = data.table::shift(data[[eval(t)]], n = lags, type = "lag")
      )
    }
    
    # Time lags----
    if(!is.null(timeDiffTarget)) {
      
      # Build Features----
      data.table::set(
        data,
        j = paste0("TEMP", lags),
        value = data.table::shift(data[[eval(sortDateName)]], n = lags, type = "lag")
      )
      
      # Difference the lag dates----
      if (WindowingLag != 0) {
        for (l in seq_along(lags)) {
          if (l == 1) {
            data.table::set(
              data,
              j = paste0(timeDiffTarget, "_", lags[l]),
              value = as.numeric(difftime(
                data[[eval(sortDateName)]],
                data[[eval(paste0("TEMP", lags[l]))]],
                units = eval(timeAgg)
              ))
            )
          } else {
            data.table::set(
              data,
              j = paste0(timeDiffTarget, "_", lags[l]),
              value = as.numeric(difftime(
                data[[eval(paste0("TEMP", lags[l] - 1))]],
                data[[eval(paste0("TEMP", lags[l]))]],
                units = eval(timeAgg)
              ))
            )
          }
        }
      } else {
        for (l in seq_along(lags)) {
          if (l == 1) {
            data.table::set(
              data,
              j = paste0(timeDiffTarget, "_", lags[l]),
              value = as.numeric(difftime(
                data[[eval(sortDateName)]],
                data[[eval(paste0("TEMP", lags[l]))]],
                units = eval(timeAgg)
              ))
            )
          } else {
            data.table::set(
              data,
              j = paste0(timeDiffTarget, "_", lags[l]),
              value = as.numeric(difftime(
                data[[eval(paste0("TEMP", (lags[l - 1])))]],
                data[[eval(paste0("TEMP", lags[l]))]],
                units = eval(timeAgg)
              ))
            )
          }
        }
      }
      
      # Remove temporary lagged dates----
      data.table::set(data, j = paste0("TEMP", lags), value = NULL)
      
      # Store new target----
      timeTarget <- paste0(timeDiffTarget, "_1")      
    }
    
    # Define targets----
    if (WindowingLag != 0) {
      if (!is.null(timeDiffTarget)) {
        Targets <-
          c(paste0("LAG_", WindowingLag, "_", Targets),
            timeTarget)
      } else {
        Targets <-
          c(paste0("LAG_", WindowingLag, "_", Targets))
      }
    } else {
      if (!is.null(timeDiffTarget)) {
        Targets <- c(Targets, timeTarget)
      } else {
        Targets <- Targets
      }
    }
    
    # Moving stats----
    for (t in seq_along(Targets)) {
      data.table::set(
        data,
        j = paste0(statsNames, "_", periods, "_", Targets[t]),
        value = data.table::frollmean(
          x = data[[eval(Targets[t])]],
          n = periods,
          fill = NA,
          algo = "fast",
          align = "right",
          na.rm = TRUE,
          hasNA = TRUE,
          adaptive = FALSE
        )
      )      
    }
    
    # Replace any inf values with NA----
    for (col in seq_along(data)) {
      data.table::set(data,
                      j = col,
                      value = replace(data[[col]],
                                      is.infinite(data[[col]]), NA))
    }
    
    # Turn character columns into factors----
    for (col in seq_along(data)) {
      if (is.character(data[[col]])) {
        data.table::set(data,
                        j = col,
                        value = as.factor(data[[col]]))
      }
    }
    
    # Impute missing values----
    if (SimpleImpute) {
      for (j in seq_along(data)) {
        if (is.factor(data[[j]])) {
          data.table::set(data,
                          which(!(data[[j]] %in% levels(data[[j]]))), j, "0")
        } else {
          data.table::set(data,
                          which(is.na(data[[j]])), j,-1)
        }
      }
    }
    
    # Done!!----
    return(data)
  }
}