#' A version of the DT_GDL function for creating the GDL features for a subset of records in your data
#'
#' A version of the DT_GDL function for creating the GDL features for a subset of records in your data. Create the same lags, moving averages, and the like off of time between records. 100% data.table.
#' @author Adrian Antico
#' @family Feature Engineering
#' @param data A data.table you want to run the function on
#' @param lags A numeric vector of the specific lags you want to have generated. You must include 1 if WindowingLag = 1.
#' @param periods A numeric vector of the specific rolling statistics window sizes you want to utilize in the calculations.
#' @param statsFUNs Vector of functions for your rolling windows, such as mean, sd, min, max, quantile
#' @param statsNames A character vector of the corresponding names to create for the rollings stats variables.
#' @param targets A character vector of the column names for the reference column in which you will build your lags and rolling stats
#' @param groupingVars A character vector of categorical variable names you will build your lags and rolling stats by
#' @param sortDateName The column name of your date column used to sort events over time
#' @param timeDiffTarget Specify a desired name for features created for time between events. Set to NULL if you don't want time between events features created.
#' @param timeAgg List the time aggregation level for the time between events features, such as "hour", "day", "week", "month", "quarter", or "year"
#' @param WindowingLag Set to 0 to build rolling stats off of target columns directly or set to 1 to build the rolling stats off of the lag-1 target
#' @param Type List either "Lag" if you want features built on historical values or "Lead" if you want features built on future values
#' @param Timer Set to TRUE if you percentage complete tracker printout
#' @param SkipCols Defaults to NULL; otherwise supply a character vector of the names of columns to skip
#' @param SimpleImpute Set to TRUE for factor level imputation of "0" and numeric imputation of -1
#' @param AscRowByGroup Required to have a column with a Row Number by group (if grouping) with 1 being the record for scoring (typically the most current in time)
#' @param RecordsKeep List the number of records to retain (1 for last record, 2 for last 2 records, etc.)
#' @return data.table of original data plus created lags, rolling stats, and time between event lags and rolling stats
#' @examples
#' N = 25116
#' data <- data.table::data.table(DateTime = as.Date(Sys.time()),
#'   Target = stats::filter(rnorm(N,
#'                                mean = 50,
#'                                sd = 20),
#'                          filter=rep(1,10),
#'                          circular=TRUE))
#' data[, temp := seq(1:N)][, DateTime := DateTime - temp]
#' data <- data[order(DateTime)]
#' data <- Partial_DT_GDL_Feature_Engineering(data,
#'                                      lags           = c(1:5),
#'                                      periods        = c(seq(10,50,10)),
#'                                      statsNames     = "mean",
#'                                      targets        = c("Target"),
#'                                      groupingVars   = NULL,
#'                                      sortDateName   = "DateTime",
#'                                      timeDiffTarget = c("Time_Gap"),
#'                                      timeAgg        = "days",
#'                                      WindowingLag   = 1,
#'                                      Type           = "Lag",
#'                                      Timer          = TRUE,
#'                                      SimpleImpute   = TRUE,
#'                                      AscRowByGroup  = "temp",
#'                                      RecordsKeep    = 1)
#' @export
Partial_DT_GDL_Feature_Engineering <- function(data,
                                               lags           = 1:5,
                                               periods        = seq(10,50,10),
                                               statsNames     = "MA",
                                               targets        = "Target",
                                               groupingVars   = "GroupVariable",
                                               sortDateName   = "DateTime",
                                               timeDiffTarget = NULL,
                                               timeAgg        = NULL,
                                               WindowingLag   = 1,
                                               Type           = "Lag",
                                               Timer          = FALSE,
                                               SimpleImpute   = TRUE,
                                               AscRowByGroup  = "temp",
                                               RecordsKeep    = 1) {
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
  if (!is.character(AscRowByGroup)) {
    warning("AscRowByGroup needs to be a character scalar for the name of your RowID column")
  }
  if (RecordsKeep < 1) {
    warning("RecordsKeep less than 1 means zero data. Why run this?")
  }
  
  # Convert to data.table if not already----
  if (!data.table::is.data.table(data))
    data <- data.table::as.data.table(data)
  
  # Max data to keep----
  MAX_RECORDS_FULL <-
    max(lags + 1 + RecordsKeep, periods + 1 + RecordsKeep)
  MAX_RECORDS_LAGS <- lags + 1 + RecordsKeep
  MAX_RECORDS_ROLL <- periods + 1 + RecordsKeep
  
  # Set up counter for countdown----
  CounterIndicator <- 0
  if (!is.null(timeDiffTarget)) {
    tarNum <- length(targets) + 1
  } else {
    tarNum <- length(targets)
  }
  
  # Define total runs----
  if (!is.null(groupingVars)) {
    runs <-
      length(groupingVars) * tarNum * (length(periods) *
                                         length(statsNames) +
                                         length(lags))
  } else {
    runs <-
      tarNum * (length(periods) * length(statsNames) +
                  length(lags))
  }
  
  # Begin feature engineering----
  if (!is.null(groupingVars)) {
    for (i in seq_along(groupingVars)) {
      Targets <- targets
      if (tolower(Type) == "lag") {
        colVar <- c(groupingVars[i], sortDateName[1])
        data.table::setorderv(data, colVar, order = 1)
      } else {
        colVar <- c(groupingVars[i], sortDateName[1])
        data.table::setorderv(data, colVar, order = -1)
      }
      
      # Remove records----
      tempData <- data[get(AscRowByGroup) <= MAX_RECORDS_FULL]
      
      # Lags----
      for (l in seq_along(lags)) {
        for (t in Targets) {
          tempData[, paste0(groupingVars[i],
                            "_LAG_",
                            lags[l], "_", t) := data.table::shift(get(t), 
                                                                  n = lags[l],
                                                                  type = "lag"),
                   by = get(groupingVars[i])]
          CounterIndicator <- CounterIndicator + 1
          if (Timer) {
            print(CounterIndicator / runs)
          }
        }
      }
      
      # Time lags----
      if (!is.null(timeDiffTarget)) {
        # Lag the dates first----
        for (l in seq_along(lags)) {
          tempData[, paste0(groupingVars[i],
                            "TEMP",
                            lags[l]) := data.table::shift(get(sortDateName),
                                                          n = lags[l],
                                                          type = "lag"), by = get(groupingVars[i])]
        }
        
        # Difference the lag dates----
        if (WindowingLag != 0) {
          for (l in seq_along(lags)) {
            if (l == 1) {
              tempData[, paste0(groupingVars[i],
                                timeDiffTarget,
                                lags[l]) := as.numeric(difftime(
                                  get(sortDateName),
                                  get(paste0(
                                    groupingVars[i], "TEMP", lags[l]
                                  )),
                                  units = eval(timeAgg)
                                )), by = get(groupingVars[i])]
              CounterIndicator <- CounterIndicator + 1
              if (Timer) {
                print(CounterIndicator / runs)
              }
            } else {
              tempData[, paste0(groupingVars[i],
                                timeDiffTarget,
                                lags[l]) := as.numeric(difftime(get(
                                  paste0(groupingVars[i],
                                         "TEMP",
                                         (lags[l - 1]))
                                ),
                                get(
                                  paste0(groupingVars[i],
                                         "TEMP",
                                         lags[l])
                                ),
                                units = eval(timeAgg))),
                       by = get(groupingVars[i])]
              CounterIndicator <- CounterIndicator + 1
              if (Timer) {
                print(CounterIndicator / runs)
              }
            }
          }
        } else {
          for (l in seq_along(lags)) {
            if (l == 1) {
              tempData[, paste0(groupingVars[i],
                                timeDiffTarget,
                                lags[l]) := as.numeric(difftime(
                                  get(sortDateName),
                                  get(paste0(
                                    groupingVars[i], "TEMP", lags[l]
                                  )),
                                  units = eval(timeAgg)
                                )), by = get(groupingVars[i])]
              CounterIndicator <- CounterIndicator + 1
              if (Timer) {
                print(CounterIndicator / runs)
              }
            } else {
              tempData[, paste0(groupingVars[i],
                                timeDiffTarget,
                                lags[l]) := as.numeric(difftime(get(
                                  paste0(groupingVars[i],
                                         "TEMP",
                                         (lags[l - 1]))
                                ),
                                get(
                                  paste0(groupingVars[i],
                                         "TEMP",
                                         lags[l])
                                ),
                                units = eval(timeAgg))),
                       by = get(groupingVars[i])]
              CounterIndicator <- CounterIndicator + 1
              if (Timer) {
                print(CounterIndicator / runs)
              }
            }
          }
        }
        
        # Remove temporary lagged dates----
        for (l in seq_along(lags)) {
          tempData[, paste0(groupingVars[i], "TEMP", lags[l]) := NULL]
        }
        
        # Store new target----
        timeTarget <- paste0(groupingVars[i], timeDiffTarget, "1")
      }
      
      # Define targets----
      if (WindowingLag != 0) {
        if (!is.null(timeDiffTarget)) {
          Targets <-
            c(paste0(groupingVars[i], "_LAG_", WindowingLag, "_", Targets),
              timeTarget)
        } else {
          Targets <-
            c(paste0(groupingVars[i], "_LAG_", WindowingLag, "_", Targets))
        }
      } else {
        if (!is.null(timeDiffTarget)) {
          Targets <- c(Targets, timeTarget)
        } else {
          Targets <- Targets
        }
      }
      
      # Keep final values----
      tempData1 <- tempData[get(AscRowByGroup) <= eval(RecordsKeep)]
      
      # Moving stats----
      for (j in seq_along(periods)) {
        for (k in seq_along(statsNames)) {
          for (t in Targets) {
            keep <- c(groupingVars[i], t, AscRowByGroup)
            temp2 <-
              tempData[get(AscRowByGroup) <=
                         MAX_RECORDS_ROLL][, ..keep]
            temp3 <-
              temp2[, paste0(groupingVars[i],
                             statsNames[k],
                             "_",
                             periods[j],
                             "_",
                             t) := data.table::frollmean(
                               x = get(t),
                               n = periods[j],
                               fill = NA,
                               algo = "fast",
                               align = "right",
                               na.rm = TRUE,
                               hasNA = TRUE,
                               adaptive = FALSE
                             ), by = get(groupingVars[i])]
            
            if (Timer) {
              CounterIndicator <- CounterIndicator + 1
              print(CounterIndicator / runs)
            }
            
            # Merge files----
            temp4 <-
              temp3[get(AscRowByGroup) <= eval(RecordsKeep)][, c(eval(t)) := NULL]
            tempData1 <-
              merge(tempData1, temp4, by = c(eval(groupingVars[i]),
                                             eval(AscRowByGroup)))
          }
        }
      }
    }
    
    # Replace any inf values with NA----
    for (col in seq_along(tempData1)) {
      data.table::set(tempData1,
                      j = col,
                      value = replace(tempData1[[col]],
                                      is.infinite(tempData1[[col]]), NA))
    }
    
    # Turn character columns into factors----
    for (col in seq_along(tempData1)) {
      if (is.character(tempData1[[col]])) {
        data.table::set(tempData1,
                        j = col,
                        value = as.factor(tempData1[[col]]))
      }
    }
    
    # Impute missing values----
    if (SimpleImpute) {
      for (j in seq_along(tempData1)) {
        if (is.factor(tempData1[[j]])) {
          data.table::set(tempData1, which(!(
            tempData1[[j]] %in% levels(tempData1[[j]])
          )), j, "0")
        } else {
          data.table::set(tempData1, which(is.na(tempData1[[j]])), j,-1)
        }
      }
    }
    
    # Done!!
    return(tempData1)
    
  } else {
    # Sort data----
    if (tolower(Type) == "lag") {
      colVar <- c(sortDateName[1])
      data.table::setorderv(data, colVar, order = 1)
    } else {
      colVar <- c(sortDateName[1])
      data.table::setorderv(data, colVar, order = -1)
    }
    Targets <- targets
    
    # Remove records----
    tempData <- data[get(AscRowByGroup) <= MAX_RECORDS_FULL]
    
    # Lags
    for (l in seq_along(lags)) {
      for (t in Targets) {
        tempData[, paste0("LAG_", lags[l], "_", t) := data.table::shift(get(t), n = lags[l], type = "lag")]
        CounterIndicator <- CounterIndicator + 1
        if (Timer) {
          print(CounterIndicator / runs)
        }
      }
    }
    
    # Time lags----
    if (!is.null(timeDiffTarget)) {
      # Lag the dates first
      for (l in seq_along(lags)) {
        tempData[, paste0("TEMP",
                          lags[l]) := data.table::shift(get(sortDateName), n = lags[l], type = "lag")]
      }
      
      # Difference the lag dates----
      if (WindowingLag != 0) {
        for (l in seq_along(lags)) {
          if (l == 1) {
            tempData[, paste0(timeDiffTarget,
                              "_",
                              lags[l]) := as.numeric(difftime(get(sortDateName),
                                                              get(paste0(
                                                                "TEMP", lags[l]
                                                              )),
                                                              units = eval(timeAgg)))]
            CounterIndicator <- CounterIndicator + 1
            if (Timer) {
              print(CounterIndicator / runs)
            }
          } else {
            tempData[, paste0(timeDiffTarget,
                              "_", lags[l]) := as.numeric(difftime(get(paste0(
                                "TEMP", lags[l] - 1
                              )),
                              get(paste0(
                                "TEMP", lags[l]
                              )),
                              units = eval(timeAgg)))]
            CounterIndicator <- CounterIndicator + 1
            if (Timer) {
              print(CounterIndicator / runs)
            }
          }
        }
      } else {
        for (l in seq_along(lags)) {
          if (l == 1) {
            tempData[, paste0(timeDiffTarget,
                              "_", lags[l]) := as.numeric(difftime(
                                get(sortDateName),
                                get(paste0("TEMP", lags[l])),
                                units = eval(timeAgg)
                              ))]
            CounterIndicator <- CounterIndicator + 1
            if (Timer) {
              print(CounterIndicator / runs)
            }
          } else {
            if (!(paste0(timeDiffTarget,
                         "_",
                         lags[l]) %in% SkipCols)) {
              tempData[, paste0(timeDiffTarget,
                                "_",
                                lags[l]) := as.numeric(difftime(get(paste0(
                                  "TEMP", (lags[l - 1])
                                )),
                                get(paste0(
                                  "TEMP", lags[l]
                                )),
                                units = eval(timeAgg)))]
              CounterIndicator <- CounterIndicator + 1
              if (Timer) {
                print(CounterIndicator / runs)
              }
            }
          }
        }
      }
      
      # Remove temporary lagged dates----
      for (l in seq_along(lags)) {
        tempData[, paste0("TEMP", lags[l]) := NULL]
      }
      
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
    
    # Keep final values----
    tempData1 <- tempData[get(AscRowByGroup) <= eval(RecordsKeep)]
    
    # Moving stats----
    for (j in seq_along(periods)) {
      for (k in seq_along(statsNames)) {
        for (t in Targets) {
          keep <- c(t, AscRowByGroup)
          temp2 <-
            tempData1[get(AscRowByGroup) <= MAX_RECORDS_ROLL][, ..keep]
          temp3 <-
            temp2[, paste0(statsNames[k],
                           "_",
                           periods[j],
                           "_",
                           t) := data.table::frollmean(
                             x = get(t),
                             n = periods[j],
                             fill = NA,
                             algo = "fast",
                             align = "right",
                             na.rm = TRUE,
                             hasNA = TRUE,
                             adaptive = FALSE
                           )]
          if (Timer) {
            CounterIndicator <- CounterIndicator + 1
            print(CounterIndicator / runs)
          }
          # Merge files----
          temp4 <-
            temp3[get(AscRowByGroup) <=
                    eval(RecordsKeep)][, c(eval(t)) := NULL]
          tempData1 <-
            merge(tempData1, temp4, by = c(eval(AscRowByGroup)))
        }
      }
    }
    
    # Replace any inf values with NA----
    for (col in seq_along(tempData1)) {
      data.table::set(tempData1,
                      j = col,
                      value = replace(tempData1[[col]],
                                      is.infinite(tempData1[[col]]),
                                      NA))
    }
    
    # Turn character columns into factors----
    for (col in seq_along(tempData1)) {
      if (is.character(tempData1[[col]])) {
        data.table::set(tempData1,
                        j = col,
                        value = as.factor(tempData1[[col]]))
      }
    }
    
    # Impute missing values----
    if (SimpleImpute) {
      for (j in seq_along(tempData1)) {
        if (is.factor(tempData1[[j]])) {
          data.table::set(tempData1,
                          which(!(
                            tempData1[[j]] %in% levels(tempData1[[j]])
                          )),
                          j,
                          "0")
        } else {
          data.table::set(tempData1,
                          which(is.na(tempData1[[j]])),
                          j,-1)
        }
      }
    }
    
    # Ensure correct order of columns----
    setcolorder(tempData1, c(2, 3, 1, 4:ncol(tempData1)))
    
    # Done!!
    return(tempData1)
  }
}
