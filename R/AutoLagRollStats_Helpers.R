#' @title DT_GDL_Feature_Engineering
#'
#' @description Builds autoregressive and moving average from target columns and distributed lags and distributed moving average for independent features distributed across time. On top of that, you can also create time between instances along with their associated lags and moving averages. This function works for data with groups and without groups.
#'
#' @author Adrian Antico
#' @family Feature Engineering
#'
#' @param data A data.table you want to run the function on
#' @param lags A numeric vector of the specific lags you want to have generated. You must include 1 if WindowingLag = 1.
#' @param periods A numeric vector of the specific rolling statistics window sizes you want to utilize in the calculations.
#' @param SDperiods  A numeric vector of Standard Deviation rolling statistics window sizes you want to utilize in the calculations.
#' @param Skewperiods  A numeric vector of Skewness rolling statistics window sizes you want to utilize in the calculations.
#' @param Kurtperiods  A numeric vector of Kurtosis rolling statistics window sizes you want to utilize in the calculations.
#' @param Quantileperiods A numeric vector of Quantile rolling statistics window sizes you want to utilize in the calculations.
#' @param statsFUNs Select from the following c("mean","sd","skew","kurt","q5","q10","q15","q20","q25","q30","q35","q40","q45","q50","q55","q60","q65","q70","q75","q80","q85","q90","q95")
#' @param targets A character vector of the column names for the reference column in which you will build your lags and rolling stats
#' @param groupingVars A character vector of categorical variable names you will build your lags and rolling stats by
#' @param sortDateName The column name of your date column used to sort events over time
#' @param timeDiffTarget Specify a desired name for features created for time between events. Set to NULL if you don't want time between events features created.
#' @param timeAgg List the time aggregation level for the time between events features, such as "hour", "day", "week", "month", "quarter", or "year"
#' @param WindowingLag Set to 0 to build rolling stats off of target columns directly or set to 1 to build the rolling stats off of the lag-1 target
#' @param Type List either "Lag" if you want features built on historical values or "Lead" if you want features built on future values
#' @param SimpleImpute Set to TRUE for factor level imputation of "0" and numeric imputation of -1
#' @return data.table of original data plus created lags, rolling stats, and time between event lags and rolling stats
#' @examples
#' \dontrun{
#' N = 25116
#' data <- data.table::data.table(
#'   DateTime = as.Date(Sys.time()),
#'   Target = stats::filter(rnorm(N, mean = 50, sd = 20),
#'   filter=rep(1,10),
#'   circular=TRUE))
#' data[, temp := seq(1:N)][, DateTime := DateTime - temp][
#'   , temp := NULL]
#' data <- data[order(DateTime)]
#' data <- DT_GDL_Feature_Engineering(
#'   data,
#'   lags           = c(seq(1,5,1)),
#'   periods        = c(3,5,10,15,20,25),
#'   SDperiods       = c(seq(5, 95, 5)),
#'   Skewperiods     = c(seq(5, 95, 5)),
#'   Kurtperiods     = c(seq(5, 95, 5)),
#'   Quantileperiods = c(seq(5, 95, 5)),
#'   statsFUNs      = c("mean",
#'     "sd","skew","kurt","q05","q95"),
#'   targets        = c("Target"),
#'   groupingVars   = NULL,
#'   sortDateName   = "DateTime",
#'   timeDiffTarget = c("Time_Gap"),
#'   timeAgg        = c("days"),
#'   WindowingLag   = 1,
#'   Type           = "Lag",
#'   SimpleImpute   = TRUE)
#' }
#' @noRd
DT_GDL_Feature_Engineering <- function(data,
                                       lags            = c(seq(1, 50, 1)),
                                       periods         = c(seq(5, 95, 5)),
                                       SDperiods       = c(seq(5, 95, 5)),
                                       Skewperiods     = c(seq(5, 95, 5)),
                                       Kurtperiods     = c(seq(5, 95, 5)),
                                       Quantileperiods = c(seq(5, 95, 5)),
                                       statsFUNs       = c("mean"),
                                       targets         = NULL,
                                       groupingVars    = NULL,
                                       sortDateName    = NULL,
                                       timeDiffTarget  = NULL,
                                       timeAgg         = c("days"),
                                       WindowingLag    = 0,
                                       Type            = c("Lag"),
                                       SimpleImpute    = TRUE) {

  # timeAgg----
  if(is.null(timeAgg)) {
    timeAgg <- "TimeUnitNULL"
  } else if(tolower(timeAgg) == "raw") {
    timeAggss <- "transactional"
    timeAgg <- "day"
  } else {
    timeAggss <- timeAgg
  }

  # Argument Checks----
  if(is.null(lags) & WindowingLag == 1) lags <- 1
  if(!(1 %in% lags) & WindowingLag == 1) lags <- c(1, lags)
  if(any(lags < 0)) return("lags need to be positive integers")
  if(!is.null(groupingVars)) if(!is.character(groupingVars)) return("groupingVars needs to be a character scalar or vector")
  if(!is.character(targets)) return("targets needs to be a character scalar or vector")
  if(!is.character(sortDateName)) return("sortDateName needs to be a character scalar or vector")
  if(!is.null(timeDiffTarget)) if(!is.character(timeDiffTarget)) return("timeDiffTarget needs to be a character scalar or vector")
  if(!is.null(timeAgg)) if(!is.character(timeAgg)) return("timeAgg needs to be a character scalar or vector")
  if(!(WindowingLag %in% c(0, 1))) return("WindowingLag needs to be either 0 or 1")
  if(!(tolower(Type) %chin% c("lag", "lead"))) return("Type needs to be either Lag or Lead")
  if(!is.logical(SimpleImpute)) return("SimpleImpute needs to be TRUE or FALSE")

  # Convert to data.table if not already----
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  # Ensure target is numeric----
  for(t in targets) data[, eval(t) := as.numeric(get(t))]

  # Set up counter for countdown----
  if(!is.null(timeDiffTarget)) tarNum <- length(targets) + 1L else tarNum <- length(targets)

  # Ensure enough columns are allocated beforehand----
  if(!is.null(groupingVars)) {
    if(is.null(timeDiffTarget)) {
      if(ncol(data) + (length(lags) + length(periods)) * tarNum * length(groupingVars) * length(statsFUNs) > data.table::truelength(data)) {
        data.table::alloc.col(DT = data, n = ncol(data) + (length(lags) + length(periods)) * tarNum * length(groupingVars) * length(statsFUNs))
      }
    } else {
      if(ncol(data) + (length(lags) + length(periods)) * tarNum * 2 * length(groupingVars) * length(statsFUNs) > data.table::truelength(data)) {
        data.table::alloc.col(DT = data, n = ncol(data) + (length(lags) + length(periods)) * tarNum * 2 * length(groupingVars) * length(statsFUNs))
      }
    }
  } else {
    if(is.null(timeDiffTarget)) {
      if(ncol(data) + (length(lags) + length(periods)) * tarNum * length(statsFUNs) > data.table::truelength(data)) {
        data.table::alloc.col(DT = data, n = ncol(data) + (length(lags) + length(periods)) * tarNum * length(statsFUNs))
      }
    } else {
      if(ncol(data) + (length(lags) + length(periods)) * tarNum * 2 * length(statsFUNs) > data.table::truelength(data)) {
        data.table::alloc.col(DT = data, n = ncol(data) + (length(lags) + length(periods)) * tarNum * 2 * length(statsFUNs))
      }
    }
  }

  # Begin feature engineering----
  if(!is.null(groupingVars)) {
    for(i in seq_along(groupingVars)) {
      Targets <- targets

      # Sort data----
      if(tolower(Type) == "lag") {
        colVar <- c(groupingVars[i], sortDateName[1])
        data.table::setorderv(data, colVar, order = 1)
      } else {
        colVar <- c(groupingVars[i], sortDateName[1])
        data.table::setorderv(data, colVar, order = -1)
      }

      # Lags----
      LAG_Names <- c()
      for(t in Targets) LAG_Names <- c(LAG_Names, paste0(timeAggss, "_", groupingVars[i], "_LAG_", lags, "_", t))

      # Build features----
      data[, paste0(LAG_Names) := data.table::shift(.SD, n = lags, type = "lag"), by = c(groupingVars[i]), .SDcols = Targets]

      # Time lags----
      if(!is.null(timeDiffTarget)) {

        # Lag the dates first----
        data[, paste0(timeAggss, "_", groupingVars[i], "TEMP", lags) := data.table::shift(get(sortDateName), n = lags, type = "lag"), by = c(groupingVars[i])]

        # Difference the lag dates----
        if(WindowingLag != 0L) {
          for(l in seq_along(lags)) {
            if(l == 1L) {
              data[, paste0(timeAggss, "_", groupingVars[i],timeDiffTarget,lags[l]) := as.numeric(difftime(
                get(sortDateName),
                get(paste0(groupingVars[i], "TEMP", lags[l])),
                units = eval(timeAgg))),
                by = c(groupingVars[i])]
            } else {
              data[, paste0(timeAggss, "_", groupingVars[i],timeDiffTarget,lags[l]) := as.numeric(difftime(
                get(paste0(timeAggss, "_", groupingVars[i], "TEMP", (lags[l - 1L]))),
                get(paste0(timeAggss, "_", groupingVars[i], "TEMP", lags[l])),
                units = eval(timeAgg))), by = c(groupingVars[i])]
            }
          }
        } else {
          for(l in seq_along(lags)) {
            if(l == 1L) {
              data[, paste0(timeAggss, "_", groupingVars[i],timeDiffTarget,lags[l]) := as.numeric(difftime(
                get(sortDateName),
                get(paste0(timeAggss, "_", groupingVars[i], "TEMP", lags[l])),units = eval(timeAgg))),
                by = c(groupingVars[i])]
            } else {
              data[, paste0(timeAggss, "_", groupingVars[i],timeDiffTarget,lags[l]) := as.numeric(difftime(
                get(paste0(timeAggss, "_", groupingVars[i], "TEMP", (lags[l - 1L]))),
                get(paste0(timeAggss, "_", groupingVars[i], "TEMP", lags[l])),
                units = eval(timeAgg))), by = c(groupingVars[i])]
            }
          }
        }

        # Remove temporary lagged dates----
        for(l in seq_along(lags)) data[, paste0(timeAggss, "_", groupingVars[i], "TEMP",lags[l]) := NULL]

        # Store new target----
        timeTarget <- paste0(timeAggss, "_", groupingVars[i],timeDiffTarget, "1")
      }

      # Define targets----
      if(WindowingLag != 0L) {
        if(!is.null(timeDiffTarget)) {
          Targets <- c(paste0(timeAggss, "_", groupingVars[i], "_LAG_", WindowingLag, "_", Targets), timeTarget)
        } else {
          Targets <- c(paste0(timeAggss, "_", groupingVars[i], "_LAG_", WindowingLag, "_", Targets))
        }
      } else {
        if(!is.null(timeDiffTarget)) {
          Targets <- c(Targets, timeTarget)
        } else {
          Targets <- Targets
        }
      }

      # MA stats----
      if(any(tolower(statsFUNs) %chin% "mean") & !all(periods %in% c(0L,1L))) {

        # Collect Names----
        periods <- periods[periods > 1L]
        MA_Names <- c()
        for(t in Targets) for(j in seq_along(periods)) MA_Names <- c(MA_Names,paste0(timeAggss, "_", groupingVars[i],"Mean","_",periods[j],"_",t))

        # Build Features----
        data[, paste0(MA_Names) := data.table::frollmean(
          x = .SD, n = periods, fill = NA, algo = "fast", align = "right", na.rm = TRUE, hasNA = TRUE, adaptive = FALSE),
          by = c(groupingVars[i]), .SDcols = c(Targets)]
      }

      # SD stats----
      if(any(tolower(statsFUNs) %chin% c("sd")) & !all(SDperiods %in% c(0L,1L))) {

        # Collect Names----
        tempperiods <- SDperiods[SDperiods > 1L]
        SD_Names <- c()
        for(t in Targets) for(j in seq_along(tempperiods)) SD_Names <- c(SD_Names,paste0(timeAggss, "_", groupingVars[i], "SD_", tempperiods[j], "_", t))

        # Build Features----
        data[, paste0(SD_Names) := data.table::frollapply(x = .SD, n = tempperiods, FUN = sd, na.rm = TRUE), by = c(groupingVars[i]), .SDcols = c(Targets)]
      }

      # Skewness stats----
      if(any(tolower(statsFUNs) %chin% c("skew")) & !all(Skewperiods %in% c(0L,1L,2L))) {

        # Collect Names----
        tempperiods <- Skewperiods[Skewperiods > 2L]
        Skew_Names <- c()
        for(t in Targets) for(j in seq_along(tempperiods)) Skew_Names <- c(Skew_Names,paste0(timeAggss, "_", groupingVars[i], "Skew_", tempperiods[j], "_", t))

        # Build Features----
        data[, paste0(Skew_Names) := data.table::frollapply(x = .SD, n = tempperiods, FUN = e1071::skewness, na.rm = TRUE), by = c(groupingVars[i]), .SDcols = Targets]
      }

      # Kurtosis stats----
      if(any(tolower(statsFUNs) %chin% c("kurt")) & !all(Kurtperiods %in% c(0L,1L,2L,3L,4L))) {

        # Collect Names----
        tempperiods <- Kurtperiods[Kurtperiods > 3L]
        Kurt_Names <- c()
        for(t in Targets) for(j in seq_along(tempperiods)) Kurt_Names <- c(Kurt_Names,paste0(timeAggss, "_", groupingVars[i], "Kurt_", tempperiods[j], "_", t))

        # Build Features----
        data[, paste0(Kurt_Names) := data.table::frollapply(x = .SD, n = tempperiods, FUN = e1071::kurtosis, na.rm = TRUE), by = c(groupingVars[i]), .SDcols = c(Targets)]
      }

      # Quantiles----
      if(!all(Quantileperiods %in% c(0L,1L,2L,3L,4L))) {

        tempperiods <- Quantileperiods[Quantileperiods > 4L]
        for(z in c(seq(5L,95L,5L))) {

          if(any(paste0("q",z) %chin% statsFUNs)) {

            Names <- c()
            for(t in Targets) for(j in seq_along(tempperiods)) Names <- c(Names,paste0(timeAggss, "_", groupingVars[i], "Q_", z, "_", tempperiods[j], "_", t))

            # Build Features----
            data[, paste0(Names) := data.table::frollapply(x = .SD, n = tempperiods, FUN = quantile, probs = z/100, na.rm = TRUE), by = c(groupingVars[i]), .SDcols = c(Targets)]
          }
        }
      }
    }

    # Replace any inf values with NA----
    for (col in seq_along(data)) data.table::set(data, j = col, value = replace(data[[col]], is.infinite(data[[col]]), NA))

    # Impute missing values----
    if(SimpleImpute) {
      for(j in seq_along(data)) {
        if(is.factor(data[[j]])) {
          data.table::set(data, which(!(data[[j]] %in% levels(data[[j]]))), j, "0")
        } else {
          data.table::set(data, which(is.na(data[[j]])), j, -1)
        }
      }
    }

    # Done!!----
    return(data)

  } else {
    if(tolower(Type) == "lag") {
      colVar <- c(sortDateName[1L])
      data.table::setorderv(data, colVar, order = 1L)
    } else {
      colVar <- c(sortDateName[1L])
      data.table::setorderv(data, colVar, order = -1L)
    }
    Targets <- targets

    # Lags----
    LAG_Names <- c()
    for(t in Targets) LAG_Names <- c(LAG_Names, paste0(timeAggss, "_", "LAG_", lags, "_", t))

    # Build features----
    data[, paste0(LAG_Names) := data.table::shift(.SD, n = lags, type = "lag"), .SDcols = c(Targets)]

    # Time lags----
    if(!is.null(timeDiffTarget)) {

      # Build Features----
      data.table::set(data, j = paste0(timeAggss, "_", "TEMP", lags), value = data.table::shift(data[[eval(sortDateName)]], n = lags, type = "lag"))

      # Difference the lag dates----
      if(WindowingLag != 0L) {
        for(l in seq_along(lags)) {
          if(l == 1L) {
            data.table::set(
              data,
              j = paste0(timeDiffTarget, "_", lags[l]),
              value = as.numeric(difftime(
                data[[eval(sortDateName)]],
                data[[eval(paste0(timeAggss, "_", "TEMP", lags[l]))]],
                units = eval(timeAgg))))
          } else {
            data.table::set(
              data,
              j = paste0(timeDiffTarget, "_", lags[l]),
              value = as.numeric(difftime(
                data[[eval(paste0(timeAggss, "_", "TEMP", lags[l] - 1L))]],
                data[[eval(paste0(timeAggss, "_", "TEMP", lags[l]))]],
                units = eval(timeAgg))))
          }
        }
      } else {
        for(l in seq_along(lags)) {
          if(l == 1L) {
            data.table::set(
              data,
              j = paste0(timeDiffTarget, "_", lags[l]),
              value = as.numeric(difftime(
                data[[eval(sortDateName)]],
                data[[eval(paste0(timeAggss, "_", "TEMP", lags[l]))]],
                units = eval(timeAgg))))
          } else {
            data.table::set(
              data,
              j = paste0(timeDiffTarget, "_", lags[l]),
              value = as.numeric(difftime(
                data[[eval(paste0(timeAggss, "_", "TEMP", (lags[l - 1L])))]],
                data[[eval(paste0(timeAggss, "_", "TEMP", lags[l]))]],
                units = eval(timeAgg))))
          }
        }
      }

      # Remove temporary lagged dates----
      data.table::set(data, j = paste0(timeAggss, "_", "TEMP", lags), value = NULL)

      # Store new target----
      timeTarget <- paste0(timeDiffTarget, "_1")
    }

    # Define targets----
    if(WindowingLag != 0L) {
      if(!is.null(timeDiffTarget)) {
        Targets <- c(paste0(timeAggss, "_", "LAG_", WindowingLag, "_", Targets), timeTarget)
      } else {
        Targets <- c(paste0(timeAggss, "_", "LAG_", WindowingLag, "_", Targets))
      }
    } else {
      if(!is.null(timeDiffTarget)) {
        Targets <- c(Targets, timeTarget)
      } else {
        Targets <- Targets
      }
    }

    # MA stats----
    if(any(tolower(statsFUNs) %chin% "mean") & !all(periods %in% c(0L, 1L))) {

      # Collect Names----
      periods <- periods[periods > 1L]
      MA_Names <- c()
      for(t in Targets) for (j in seq_along(periods)) MA_Names <- c(MA_Names,paste0(timeAggss, "_", "Mean_",periods[j],"_",t))

      # Build Features----
      data[, paste0(MA_Names) := data.table::frollmean(x = .SD, n = periods, fill = NA, algo = "fast", align = "right", na.rm = TRUE, hasNA = TRUE, adaptive = FALSE), .SDcols = c(Targets)]
    }

    # SD stats----
    if(any(tolower(statsFUNs) %chin% c("sd")) & !all(SDperiods %in% c(0L,1L))) {

      # Collect Names----
      tempperiods <- SDperiods[SDperiods > 1L]
      SD_Names <- c()
      for(t in Targets) for (j in seq_along(tempperiods)) SD_Names <- c(SD_Names,paste0(timeAggss, "_", "SD_",tempperiods[j],"_",t))

      # Build Features----
      data[, paste0(SD_Names) := data.table::frollapply(x = .SD, n = tempperiods, FUN = sd, na.rm = TRUE), .SDcols = c(Targets)]
    }

    # Skewness stats----
    if(any(tolower(statsFUNs) %chin% c("skew")) & !all(Skewperiods %in% c(0L,1L,2L))) {

      # Collect Names----
      tempperiods <- Skewperiods[Skewperiods > 2L]
      Skew_Names <- c()
      for(t in Targets) for (j in seq_along(tempperiods)) Skew_Names <- c(Skew_Names,paste0(timeAggss, "_", "Skew_",tempperiods[j],"_",t))

      # Build Features----
      data[, paste0(Skew_Names) := data.table::frollapply(x = .SD, n = tempperiods, FUN = e1071::skewness, na.rm = TRUE), .SDcols = c(Targets)]
    }

    # Kurtosis stats----
    if(any(tolower(statsFUNs) %chin% c("kurt")) & !all(Kurtperiods %in% c(0L,1L,2L,3L))) {

      # Collect Names----
      tempperiods <- Kurtperiods[Kurtperiods > 3L]
      Kurt_Names <- c()
      for(t in Targets) for (j in seq_along(tempperiods)) Kurt_Names <- c(Kurt_Names,paste0(timeAggss, "_", "Kurt_",tempperiods[j],"_",t))

      # Build Features----
      data[, paste0(Kurt_Names) := data.table::frollapply(x = .SD, n = tempperiods, FUN = e1071::kurtosis, na.rm = TRUE), .SDcols = c(Targets)]
    }

    # Quantiles----
    if(!all(Quantileperiods %in% c(0L,1L,2L,3L,4L))) {
      tempperiods <- Quantileperiods[Quantileperiods > 4L]
      for(z in c(seq(5L,95L,5L))) {
        if(any(paste0("q",z) %chin% statsFUNs)) {
          Names <- c()
          for (t in Targets) for (j in seq_along(tempperiods)) Names <- c(Names,paste0(timeAggss, "_", "Q_",z,"_",tempperiods[j],"_",t))

          # Build Features----
          data[, paste0(Names) := data.table::frollapply(x = .SD, n = tempperiods, FUN = quantile, probs = z/100, na.rm = TRUE), .SDcols = c(Targets)]
        }
      }
    }

    # Replace any inf values with NA----
    for(col in seq_along(data)) data.table::set(data, j = col, value = replace(data[[col]], is.infinite(data[[col]]), NA))

    # Impute missing values----
    if(SimpleImpute) {
      for(j in seq_along(data)) {
        if(is.factor(data[[j]])) {
          data.table::set(data, which(!(data[[j]] %in% levels(data[[j]]))), j, "0")
        } else {
          data.table::set(data, which(is.na(data[[j]])), j, -1)
        }
      }
    }

    # Done!!----
    return(data)
  }
}

#' @title Partial_DT_GDL_Feature_Engineering
#'
#' @description For scoring models in production that have > 1 grouping variables and for when you need > 1 record (or records per grouping variables) returned. This function is for generating lags and moving averages (along with lags and moving averages off of time between records), for a partial set of records in your data set, typical new records that become available for model scoring. Column names and ordering will be identical to the output from the corresponding DT_GDL_Feature_Engineering() function, which most likely was used to create features for model training.
#'
#' @author Adrian Antico
#' @family Feature Engineering
#'
#' @param data A data.table you want to run the function on
#' @param lags A numeric vector of the specific lags you want to have generated. You must include 1 if WindowingLag = 1.
#' @param periods A numeric vector of the specific rolling statistics window sizes you want to utilize in the calculations.
#' @param SDperiods  A numeric vector of Standard Deviation rolling statistics window sizes you want to utilize in the calculations.
#' @param Skewperiods  A numeric vector of Skewness rolling statistics window sizes you want to utilize in the calculations.
#' @param Kurtperiods  A numeric vector of Kurtosis rolling statistics window sizes you want to utilize in the calculations.
#' @param Quantileperiods A numeric vector of Quantile rolling statistics window sizes you want to utilize in the calculations.
#' @param statsFUNs Select from the following c("mean","sd","skew","kurt","q5","q10","q15","q20","q25","q30","q35","q40","q45","q50","q55","q60","q65","q70","q75","q80","q85","q90","q95")
#' @param targets A character vector of the column names for the reference column in which you will build your lags and rolling stats
#' @param groupingVars A character vector of categorical variable names you will build your lags and rolling stats by
#' @param sortDateName The column name of your date column used to sort events over time
#' @param timeDiffTarget Specify a desired name for features created for time between events. Set to NULL if you don't want time between events features created.
#' @param timeAgg List the time aggregation level for the time between events features, such as "hour", "day", "week", "month", "quarter", or "year"
#' @param WindowingLag Set to 0 to build rolling stats off of target columns directly or set to 1 to build the rolling stats off of the lag-1 target
#' @param Type List either "Lag" if you want features built on historical values or "Lead" if you want features built on future values
#' @param Timer Set to TRUE if you percentage complete tracker printout
#' @param SimpleImpute Set to TRUE for factor level imputation of "0" and numeric imputation of -1
#' @param AscRowByGroup Required to have a column with a Row Number by group (if grouping) with the smallest numbers being the records for scoring (typically the most current in time).
#' @param RecordsKeep List the row number of AscRowByGroup and those data points will be returned
#' @param AscRowRemove Set to TRUE to remove the AscRowByGroup column upon returning data.
#' @return data.table of original data plus created lags, rolling stats, and time between event lags and rolling stats
#' @examples
#' \dontrun{
#' N = 25116
#' data <- data.table::data.table(
#'   DateTime = as.Date(Sys.time()),
#'   Target = stats::filter(
#'     rnorm(N, mean = 50, sd = 20),
#'   filter=rep(1,10),
#'   circular=TRUE))
#' data[, temp := seq(1:N)][, DateTime := DateTime - temp]
#' data <- data[order(DateTime)]
#' data <- Partial_DT_GDL_Feature_Engineering(
#'   data,
#'   lags           = c(1:5),
#'   periods        = c(seq(10,50,10)),
#'   SDperiods       = c(seq(5, 95, 5)),
#'   Skewperiods     = c(seq(5, 95, 5)),
#'   Kurtperiods     = c(seq(5, 95, 5)),
#'   Quantileperiods = c(seq(5, 95, 5)),
#'   statsFUNs      = c("mean","sd", "skew",
#'     "kurt","q5","q95"),
#'   targets        = c("Target"),
#'   groupingVars   = NULL,
#'   sortDateName   = "DateTime",
#'   timeDiffTarget = c("Time_Gap"),
#'   timeAgg        = "days",
#'   WindowingLag   = 1,
#'   Type           = "Lag",
#'   Timer          = TRUE,
#'   SimpleImpute   = TRUE,
#'   AscRowByGroup  = "temp",
#'   RecordsKeep    = c(1,5,100,2500),
#'   AscRowRemove   = TRUE)
#' }
#' @noRd
Partial_DT_GDL_Feature_Engineering <- function(data,
                                               lags            = c(seq(1,5,1)),
                                               periods         = c(3,5,10,15,20,25),
                                               SDperiods       = c(seq(5, 95, 5)),
                                               Skewperiods     = c(seq(5, 95, 5)),
                                               Kurtperiods     = c(seq(5, 95, 5)),
                                               Quantileperiods = c(seq(5, 95, 5)),
                                               statsFUNs       = c("mean"),
                                               targets         = c("Target"),
                                               groupingVars    = NULL,
                                               sortDateName    = NULL,
                                               timeDiffTarget  = NULL,
                                               timeAgg         = NULL,
                                               WindowingLag    = 1,
                                               Type            = "Lag",
                                               Timer           = TRUE,
                                               SimpleImpute    = TRUE,
                                               AscRowByGroup   = "temp",
                                               RecordsKeep     = 1,
                                               AscRowRemove    = TRUE) {

  # Turn on full speed ahead----
  data.table::setDTthreads(threads = max(1L, parallel::detectCores() - 2L))

  # Argument Checks----
  if(!is.null(timeAgg)) if(!is.character(timeAgg)) stop("timeAgg needs to be a character scalar or vector")
  if(is.null(timeAgg)) {
    timeAgg <- "TimeUnitNULL"
  } else if(tolower(timeAgg) == "raw") {
    timeAggss <- "transactional"
  } else {
    timeAggss <- timeAgg
  }
  if(is.null(lags) && WindowingLag == 1) lags <- 1
  if(!(1 %in% lags) && WindowingLag == 1) lags <- c(1, lags)
  if(any(lags < 0)) stop("lags need to be positive integers")
  if(!is.null(groupingVars)) if(!is.character(groupingVars)) stop("groupingVars needs to be a character scalar or vector")
  if(!is.character(targets)) stop("targets needs to be a character scalar or vector")
  if(!is.character(sortDateName)) stop("sortDateName needs to be a character scalar or vector")
  if(!is.null(timeDiffTarget)) if(!is.character(timeDiffTarget)) stop("timeDiffTarget needs to be a character scalar or vector")
  if(!(WindowingLag %in% c(0, 1))) stop("WindowingLag needs to be either 0 or 1")
  if(!(tolower(Type) %chin% c("lag", "lead"))) stop("Type needs to be either Lag or Lead")
  if(!is.logical(Timer)) stop("Timer needs to be TRUE or FALSE")
  if(!is.logical(SimpleImpute)) stop("SimpleImpute needs to be TRUE or FALSE")
  if(!is.character(AscRowByGroup)) stop("AscRowByGroup needs to be a character scalar for the name of your RowID column")

  # Base columns from data----
  ColKeep <- names(data)

  # Convert to data.table if not already----
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  # Max data to keep----
  MaxCols <- 0
  if(!is.null(lags)) MaxCols <- max(MaxCols,lags)
  if(!is.null(periods)) MaxCols <- max(MaxCols,periods)
  if(!is.null(SDperiods)) MaxCols <- max(MaxCols,SDperiods)
  if(!is.null(Skewperiods)) MaxCols <- max(MaxCols,Skewperiods)
  if(!is.null(Kurtperiods)) MaxCols <- max(MaxCols,Kurtperiods)
  if(!is.null(Quantileperiods)) MaxCols <- max(MaxCols,Quantileperiods)
  if(WindowingLag == 0) MaxCols <- MaxCols - 1L

  # Set up counter for countdown----
  CounterIndicator <- 0
  if(!is.null(timeDiffTarget)) {
    tarNum <- length(targets) + 1
  } else {
    tarNum <- length(targets)
  }

  # Ensure enough columns are allocated beforehand: data.table allocates 1024 by default----
  if(!is.null(groupingVars)) {
    if(is.null(timeDiffTarget)) {
      if(ncol(data) + (length(lags) + length(periods)) * tarNum * length(groupingVars) * length(statsFUNs) > data.table::truelength(data)) {
        data.table::alloc.col(DT = data, n = ncol(data) + (length(lags) + length(periods)) * tarNum * length(groupingVars) * length(statsFUNs))
      }
    } else {
      if(ncol(data) + (length(lags) + length(periods)) * tarNum * 2 * length(groupingVars) * length(statsFUNs) > data.table::truelength(data)) {
        data.table::alloc.col(DT = data, n = ncol(data) + (length(lags) + length(periods)) * tarNum * 2 * length(groupingVars) * length(statsFUNs))
      }
    }
  } else {
    if(is.null(timeDiffTarget)) {
      if(ncol(data) + (length(lags) + length(periods)) * tarNum * length(statsFUNs) > data.table::truelength(data)) {
        data.table::alloc.col(DT = data, n = ncol(data) + (length(lags) + length(periods)) * tarNum * length(statsFUNs))
      }
    } else {
      if(ncol(data) + (length(lags) + length(periods)) * tarNum * 2 * length(statsFUNs) > data.table::truelength(data)) {
        data.table::alloc.col(DT = data, n = ncol(data) + (length(lags) + length(periods)) * tarNum * 2 * length(statsFUNs))
      }
    }
  }

  # Begin feature engineering----
  if(!is.null(groupingVars)) {
    for(i in seq_along(groupingVars)) {
      TargetS <- targets

      ##############################################################################################################################
      # Error on 3G 1xregs CARMA
      # Error: Object 'ID' not found. Perhaps you intended ThirdGroup, Date, Dept, ThirdGroup-Store, ThirdGroup-Dept or 106 more
      ##############################################################################################################################

      # Subset data----
      data1 <- data[get(groupingVars[i]) %in% data[get(AscRowByGroup) %in% c(RecordsKeep)][[eval(groupingVars[i])]]]

      # Sort data----
      if(tolower(Type) == "lag") {
        colVar <- c(groupingVars[i], sortDateName[1])
        data.table::setorderv(data1, colVar, order = 1)
      } else {
        colVar <- c(groupingVars[i], sortDateName[1])
        data.table::setorderv(data1, colVar, order = -1)
      }

      # Subset data for the rows needed to compute MaxCols----
      rows <- data1[, .I[get(AscRowByGroup) %in% c(RecordsKeep)]]
      Rows <- c()
      for(x in as.integer(seq_along(rows))) {
        if(x == 1) {
          Rows <- rows[x]:(max(rows[x]-MaxCols,1))
        } else {
          Rows <- c(Rows, rows[x]:(max(rows[x]-MaxCols,1)))
        }
      }
      data1 <- data1[unique(Rows)]

      # Sort data----
      if(tolower(Type) == "lag") {
        colVar <- c(groupingVars[i], sortDateName[1])
        data.table::setorderv(data1, colVar, order = 1)
      } else {
        colVar <- c(groupingVars[i], sortDateName[1])
        data.table::setorderv(data1, colVar, order = -1)
      }

      # Lags----
      LagKeep <- c()
      LagKeeps <- list()
      LagCols <- c()
      LagColss <- list()
      for(t in TargetS) {
        for(l in seq_len(MaxCols)) {
          LagCols <- c(LagCols, paste0(timeAggss, "_", groupingVars[i], "_LAG_", l, "_", t))
          if(l %in% lags) LagKeep <- c(LagKeep, paste0(timeAggss, "_", groupingVars[i], "_LAG_", l, "_", t))
        }
        LagColss[[t]] <- LagCols
        LagKeeps[[t]] <- LagKeep
        LagCols <- c()
        LagKeep <- c()
      }

      ######################################################
      # Error here: CARMA 3G 1XREGS
      # Error in `[.data.table`(data1, , `:=`(paste0(unlist(LagColss)), data.table::shift(.SD,  : Some items of .SDcols are not column names: [ThirdGroup-Dept_LAG_1_Weekly_Sales]
      ######################################################

      # Build features----
      data1[, paste0(unlist(LagColss)) := data.table::shift(.SD, n = seq_len(MaxCols), type = "lag"), by = c(groupingVars[i]), .SDcols = c(TargetS)]

      # Time lags----
      if(!is.null(timeDiffTarget)) {

        # Lag the dates first----
        timeDiffKeep <- c()
        for(l in seq_len(MaxCols+1)) timeDiffKeep <- c(timeDiffKeep,paste0(timeAggss, "_", groupingVars[i],"TEMP",l))

        # Build features----
        data1[, paste0(timeDiffKeep) := data.table::shift(get(sortDateName),n = 1:MaxCols,type = "lag"), by = c(groupingVars[i])]

        # Difference the lag dates----
        if(WindowingLag != 0) {
          for(l in seq_len(MaxCols+1)) {
            if(l == 1) {
              data1[, paste0(groupingVars[i],timeDiffTarget,l) := as.numeric(difftime(
                get(sortDateName),
                get(paste0(timeAggss, "_", groupingVars[i], "TEMP", l)),
                units = eval(timeAgg))), by = c(groupingVars[i])]

              # TimeLagCols----
              TimeLagCols <- c(paste0(timeAggss, "_", groupingVars[i], timeDiffTarget, l))

              # TimeLagKeep----
              TimeLagKeep <- c(paste0(timeAggss, "_", groupingVars[i], timeDiffTarget, l))

            } else {
              data1[, paste0(timeAggss, "_", groupingVars[i],timeDiffTarget,l) := as.numeric(difftime(get(
                paste0(timeAggss, "_", groupingVars[i], "TEMP", (l - 1))),
                get(paste0(timeAggss, "_", groupingVars[i], "TEMP", l)),
                units = eval(timeAgg))), by = c(groupingVars[i])]

              # TimeLagCols----
              TimeLagCols <- c(TimeLagCols, paste0(timeAggss, "_", groupingVars[i], timeDiffTarget, l))

              # TimeLagKeep----
              if(l %in% lags) {
                TimeLagKeep <- c(TimeLagKeep, paste0(timeAggss, "_", groupingVars[i], timeDiffTarget, l))
              }
            }
          }
        } else {
          for(l in seq_len(MaxCols+1)) {
            if(l == 1) {
              data1[, paste0(timeAggss, "_", groupingVars[i],timeDiffTarget,l) := as.numeric(difftime(
                get(sortDateName),
                get(paste0(timeAggss, "_", groupingVars[i], "TEMP", l)),
                units = eval(timeAgg))), by = c(groupingVars[i])]

              # TimeLagCols----
              TimeLagCols <- c(timeAggss, "_", paste0(groupingVars[i], timeDiffTarget, l))

              # TimeLagKeep----
              if(l %in% lags) {
                TimeLagKeep <- c(paste0(timeAggss, "_", groupingVars[i], timeDiffTarget, l))
              }
            } else {
              data1[, paste0(timeAggss, "_", groupingVars[i],timeDiffTarget,l) := as.numeric(difftime(
                get(paste0(timeAggss, "_", groupingVars[i], "TEMP", (l - 1))),
                get(paste0(timeAggss, "_", groupingVars[i], "TEMP", l)),
                units = eval(timeAgg))), by = c(groupingVars[i])]

              # TimeLagCols----
              TimeLagCols <- c(TimeLagCols, paste0(timeAggss, "_", groupingVars[i],timeDiffTarget,l))

              # TimeLagKeep----
              if(l %in% lags) {
                TimeLagKeep <- c(TimeLagKeep, paste0(timeAggss, "_", groupingVars[i],timeDiffTarget,l))
              }
            }
          }
        }

        # Remove temporary lagged dates----
        dropcols <- c()
        for(l in seq_len(MaxCols+1)) dropcols <- c(dropcols,paste0(timeAggss, "_", groupingVars[i], "TEMP", l))

        # Store new target----
        timeTarget <- paste0(timeAggss, "_", groupingVars[i], timeDiffTarget,"_1")
      }

      # Define targets----
      if(WindowingLag != 0) {
        if(!is.null(timeDiffTarget)) {
          TargetS <- c(paste0(timeAggss, "_", groupingVars[i], "_LAG_", WindowingLag, "_", TargetS), timeTarget)
        } else {
          TargetS <- c(paste0(timeAggss, "_", groupingVars[i], "_LAG_", WindowingLag, "_", TargetS))
        }
      } else {
        if(!is.null(timeDiffTarget)) {
          TargetS <- c(TargetS, timeTarget)
        } else {
          TargetS <- TargetS
        }
      }

      # Subset data----
      data1 <- data1[get(AscRowByGroup) %in% c(RecordsKeep)]

      # Initialize PeriodKeep
      PeriodKeep <- c()

      # Moving Averages----
      if(any(tolower(statsFUNs) %chin% "mean") & !all(periods %in% c(0L,1L))) {

        # Begin
        periods <- periods[periods > 1L]
        TargetN <- 0L
        for(t in TargetS) {

          TargetN <- TargetN + 1L
          for(j in periods) {

            # Check if target is for time between records or not----
            if(!is.null(timeDiffTarget)) {
              if(grepl(pattern = eval(timeDiffTarget),x = t)) {
                data1[, eval(paste0(timeAggss, "_", groupingVars[i],"Mean_",j,"_",t)) := fBasics::rowAvgs(.SD), .SDcols = TimeLagCols[1:j]]
                PeriodKeep <- c(PeriodKeep, paste0(timeAggss, "_", groupingVars[i],"Mean_",j,"_",t))
              } else {
                data1[, eval(paste0(timeAggss, "_", groupingVars[i],"Mean_",j,"_",t)) := fBasics::rowAvgs(.SD), .SDcols = LagCols[1:j]]
                PeriodKeep <-  c(PeriodKeep, paste0(timeAggss, "_", groupingVars[i],"Mean_",j,"_",t))
              }
            } else {
              if(!is.numeric(data1[[eval(t)]])) data.table::set(data1, j = eval(t), value = as.numeric(data[[eval(t)]]))
              data1[, eval(paste0(timeAggss, "_", groupingVars[i], "Mean_", j, "_", t)) := fBasics::rowAvgs(.SD), .SDcols = LagColss[[TargetN]][1:j]]
              PeriodKeep <-  c(PeriodKeep, paste0(timeAggss, "_", groupingVars[i],"Mean_",j,"_",t))
            }
          }
        }
      }

      # Standard Deviations----
      if(any(tolower(statsFUNs) %chin% "sd") && !all(SDperiods %in% c(0L,1L))) {
        tempperiods <- SDperiods[SDperiods > 1L]
        TargetN <- 0L
        for(t in TargetS) {
          TargetN <- TargetN + 1L
          for(j in tempperiods) {
            if(!is.null(timeDiffTarget)) {
              if(grepl(pattern = eval(timeDiffTarget),x = t)) {
                data1[, eval(paste0(timeAggss, "_", groupingVars[i],"SD_",j,"_",t)) := fBasics::rowSds(.SD), .SDcols = TimeLagCols[1:j]]
                PeriodKeep <- c(PeriodKeep, paste0(timeAggss, "_", groupingVars[i],"SD_",j,"_",t))
              } else {
                data1[, eval(paste0(timeAggss, "_", groupingVars[i],"SD_",j,"_",t)) := fBasics::rowSds(.SD), .SDcols = LagCols[1:j]]
                PeriodKeep <- c(PeriodKeep, paste0(timeAggss, "_", groupingVars[i],"SD_",j,"_",t))
              }
            } else {
              data1[, eval(paste0(timeAggss, "_", groupingVars[i],"SD_",j,"_",t)) := fBasics::rowSds(.SD), .SDcols = c(LagColss[[TargetN]][1:j])]
              PeriodKeep <- c(PeriodKeep, paste0(timeAggss, "_", groupingVars[i],"SD_",j,"_",t))
            }
          }
        }
      }

      # Skewnewss----
      if(any(tolower(statsFUNs) %chin% "skew") && !all(Skewperiods %in% c(0,1,2))) {
        tempperiods <- Skewperiods[Skewperiods > 2L]
        TargetN <- 0L
        for (t in TargetS) {
          TargetN <- TargetN + 1L
          for(j in tempperiods) {
            if(!is.null(timeDiffTarget)) {
              if(grepl(pattern = eval(timeDiffTarget),x = t)) {
                data1[, eval(paste0(timeAggss, "_", groupingVars[i],"Skew_",j,"_",t)) := fBasics::rowSkewness(.SD), .SDcols = TimeLagCols[1:j]]
                PeriodKeep <- c(PeriodKeep, paste0(timeAggss, "_", groupingVars[i],"Skew_",j,"_",t))
              } else {
                data1[, eval(paste0(timeAggss, "_", groupingVars[i],"Skew_",j,"_",t)) := fBasics::rowSkewness(.SD), .SDcols = LagCols[1:j]]
                PeriodKeep <-  c(PeriodKeep, paste0(timeAggss, "_", groupingVars[i],"Skew_",j,"_",t))
              }
            } else {
              data1[, eval(paste0(timeAggss, "_", groupingVars[i],"Skew_",j,"_",t)) := fBasics::rowSkewness(.SD), .SDcols = LagColss[[TargetN]][1:j]]
              PeriodKeep <- c(PeriodKeep, paste0(timeAggss, "_", groupingVars[i],"Skew_",j,"_",t))
            }
          }
        }
      }

      # Kurt stats----
      if(any(tolower(statsFUNs) %chin% "kurt") && !all(Kurtperiods %in% c(0,1,2,3))) {
        tempperiods <- Kurtperiods[Kurtperiods > 3L]
        TargetN <- 0L
        for(t in TargetS) {
          TargetN <- TargetN + 1L
          for(j in tempperiods) {
            if(!is.null(timeDiffTarget)) {
              if(grepl(pattern = eval(timeDiffTarget),x = t)) {
                data1[, eval(paste0(timeAggss, "_", groupingVars[i],"Kurt_",j,"_",t)) := fBasics::rowKurtosis(.SD), .SDcols = TimeLagCols[1:j]]
                PeriodKeep <- c(PeriodKeep, paste0(timeAggss, "_", groupingVars[i],"Kurt_",j,"_",t))
              } else {
                data1[, eval(paste0(timeAggss, "_", groupingVars[i],"Kurt_",j,"_",t)) := fBasics::rowKurtosis(.SD), .SDcols = LagCols[1:j]]
                PeriodKeep <- c(PeriodKeep, paste0(timeAggss, "_", groupingVars[i],"Kurt_",j,"_",t))
              }
            } else {
              data1[, eval(paste0(timeAggss, "_", groupingVars[i],"Kurt_",j,"_",t)) := fBasics::rowKurtosis(.SD), .SDcols = LagColss[[TargetN]][1:j]]
              PeriodKeep <- c(PeriodKeep, paste0(timeAggss, "_", groupingVars[i],"Kurt_",j,"_",t))
            }
          }
        }
      }

      # Quantile----
      if(!all(Quantileperiods %in% c(0,1,2,3,4))) {
        tempperiods <- Quantileperiods[Quantileperiods > 4L]
        for(z in c(seq(5,95,5))) {
          if(any(paste0("q",z) %chin% statsFUNs)) {
            TargetN <- 0L
            for(t in TargetS) {
              TargetN <- TargetN + 1L
              for(j in tempperiods) {
                if(!is.null(timeDiffTarget)) {
                  if(grepl(pattern = eval(timeDiffTarget),x = t)) {
                    PeriodKeep <- c(PeriodKeep, paste0(timeAggss, "_", groupingVars[i],"Q_",z,"_",j,"_",t))
                  } else {
                    data1[, eval(paste0(timeAggss, "_", groupingVars[i],"Q_",z,"_",j,"_",t)) := fBasics::rowKurtosis(.SD), .SDcols = LagCols[1:j]]
                    PeriodKeep <- c(PeriodKeep, paste0(timeAggss, "_", groupingVars[i],"Q_",z,"_",j,"_",t))
                  }
                } else {
                  data1[, eval(paste0(timeAggss, "_", groupingVars[i],"Q_",z,"_",j,"_",t)) := fBasics::rowKurtosis(.SD), .SDcols = LagColss[[TargetN]][1:j]]
                  PeriodKeep <- c(PeriodKeep, paste0(timeAggss, "_", groupingVars[i],"Q_",z,"_",j,"_",t))
                }
              }
            }

            # Drop cols for time between----
            if(!is.null(timeDiffTarget)) data1[, paste0(dropcols) := NULL]
          }
        }
      }

      # Only keep requested columns----
      if(is.null(timeDiffTarget)) {
        if(i == 1) {
          keep <- c(ColKeep, unlist(LagKeeps), PeriodKeep)
          FinalData <- data1[, ..keep]
        } else {
          keep <- c("TEMPDATE",AscRowByGroup, unlist(LagKeeps), PeriodKeep)
          FinalData <- merge(FinalData, data1[, ..keep], by = "TEMPDATE", all = FALSE)
        }
      } else {
        if(i == 1) {
          keep <- c(ColKeep, unlist(LagKeeps), TimeLagKeep, PeriodKeep)
          FinalData <- data1[, ..keep]
        } else {
          keep <- c("TEMPDATE",AscRowByGroup, unlist(LagKeeps), TimeLagKeep, PeriodKeep)
          FinalData <- merge(FinalData, data1[, ..keep], by = "TEMPDATE", all = TRUE)
        }
      }
    }

    # Replace any inf values with NA----
    for(col in seq_along(FinalData)) data.table::set(FinalData, j = col, value = replace(FinalData[[col]], is.infinite(FinalData[[col]]), NA))

    # Turn character columns into factors----
    for(col in seq_along(FinalData)) if(is.character(FinalData[[col]])) data.table::set(FinalData, j = col, value = as.factor(FinalData[[col]]))

    # Impute missing values----
    if(SimpleImpute) {
      for(j in seq_along(FinalData)) {
        if(is.factor(FinalData[[j]])) {
          data.table::set(FinalData, which(!(FinalData[[j]] %in% levels(FinalData[[j]]))), j, "0")
        } else {
          data.table::set(FinalData, which(is.na(FinalData[[j]])), j,-1)
        }
      }
    }

    # Done----
    if(AscRowRemove) if(eval(AscRowByGroup) %chin% names(FinalData)) return(FinalData[, eval(AscRowByGroup) := NULL]) else return(FinalData)

    # Non-grouping case----
  } else {
    TargetS <- targets

    # Sort data----
    if(tolower(Type) == "lag") {
      colVar <- c(sortDateName[1])
      data.table::setorderv(data, colVar, order = 1L)
    } else {
      colVar <- c(sortDateName[1])
      data.table::setorderv(data, colVar, order = -1L)
    }

    # Subset data for the rows needed to compute MaxCols----
    rows <- data[, .I[get(AscRowByGroup) %in% c(RecordsKeep)]]
    Rows <- c()
    for(x in seq_along(rows)) {
      if(x == 1) {
        Rows <- rows[x]:(max(rows[x]-MaxCols, 0L))
      } else {
        Rows <- c(Rows, rows[x]:(max(rows[x]-MaxCols, 0L)))
      }
    }
    data <- data[unique(Rows)]

    # Sort data----
    if(tolower(Type) == "lag") {
      colVar <- c(sortDateName[1])
      data.table::setorderv(data, colVar, order = 1L)
    } else {
      colVar <- c(sortDateName[1])
      data.table::setorderv(data, colVar, order = -1L)
    }

    # Lags----
    LagCols <- c()
    LagColss <- list()
    LagKeep <- c()
    LagKeeps <- list()
    for(t in TargetS) {
      for(l in seq_len(MaxCols)) {
        LagCols <- c(LagCols, paste0(timeAggss, "_", "LAG_", l, "_", t))
        if(l %in% lags) LagKeep <- c(LagKeep, paste0(timeAggss, "_", "LAG_", l, "_", t))
      }
      LagColss[[t]] <- LagCols
      LagCols <- c()
      LagKeeps[[t]] <- LagKeep
      LagKeep <- c()
    }

    # Build features----
    data[, paste0(unlist(LagColss)) := data.table::shift(.SD, n = seq_len(MaxCols), type = "lag"), .SDcols = c(TargetS)]

    # Time lags----
    if(!is.null(timeDiffTarget)) {
      timeDiffKeep <- c()
      for(l in seq_len(MaxCols+1)) timeDiffKeep <- c(timeDiffKeep, paste0(timeAggss, "_", "TEMP",l))

      # Build features----
      data[, paste0(timeDiffKeep) := data.table::shift(get(sortDateName), n = seq_len(MaxCols+1), type = "lag")]
      if(WindowingLag != 0) {
        for(l in seq_len(MaxCols+1)) {
          if(l == 1) {
            data[, paste0(timeAggss, "_", timeDiffTarget, "_", l) := as.numeric(difftime(
              get(sortDateName),
              get(paste0(timeAggss, "_", "TEMP", l)),
              units = eval(timeAgg)))]

            # TimeLagCols----
            TimeLagCols <- paste0(timeAggss, "_", timeDiffTarget, "_", l)

            # TimeLagKeep----
            TimeLagKeep <- paste0(timeAggss, "_", timeDiffTarget, "_", l)
          } else {
            data[, paste0(timeAggss, "_", timeDiffTarget, "_", l) := as.numeric(difftime(get(
              paste0(timeAggss, "_", "TEMP", (l - 1))),
              get(paste0(timeAggss, "_", "TEMP", l)),
              units = eval(timeAgg)))]

            # TimeLagCols----
            TimeLagCols <- c(TimeLagCols,paste0(timeAggss, "_", timeDiffTarget, "_", l))

            # TimeLagKeep----
            if(l %in% lags) TimeLagKeep <- c(TimeLagKeep, paste0(timeAggss, "_", timeDiffTarget, "_", l))
          }
        }
      } else {
        for(l in seq_len(MaxCols+1)) {
          if(l == 1) {
            data[, paste0(timeDiffTarget, "_", l) := as.numeric(difftime(
              get(sortDateName),
              get(paste0(timeAggss, "_", "TEMP", l)),
              units = eval(timeAgg)))]

            # TimeLagCols----
            TimeLagCols <- paste0(timeAggss, "_", timeDiffTarget, "_", l)

            # TimeLagKeep----
            TimeLagKeep <- paste0(timeAggss, "_", timeDiffTarget, "_", l)
          } else {
            data[, paste0(timeDiffTarget, "_", l) := as.numeric(difftime(get(
              paste0(timeAggss, "_", "TEMP", (l - 1))),
              get(paste0(timeAggss, "_", "TEMP", l)),
              units = eval(timeAgg)))]

            # TimeLagCols----
            TimeLagCols <- c(TimeLagCols, paste0(timeAggss, "_", timeDiffTarget, "_", l))

            # TimeLagKeep----
            if(l %in% lags) TimeLagKeep <- c(TimeLagKeep, paste0(timeAggss, "_", timeDiffTarget, "_", l))
          }
        }
      }

      # Store new target----
      timeTarget <- paste0(timeAggss, "_", timeDiffTarget, "_1")
    }

    # Define targets----
    if(WindowingLag != 0) {
      if(!is.null(timeDiffTarget)) {
        TargetS <- c(paste0(timeAggss, "_", "LAG_", WindowingLag, "_", TargetS), timeTarget)
      } else {
        TargetS <- c(paste0(timeAggss, "_", "LAG_", WindowingLag, "_", TargetS))
      }
    } else {
      if(!is.null(timeDiffTarget)) TargetS <- c(TargetS, timeTarget)
    }

    # Subset data----
    data <- data[get(AscRowByGroup) %in% c(RecordsKeep)]


    # Initalize PeriodKeep
    PeriodKeep <- c()

    # Moving Averages----
    if(any(tolower(statsFUNs) %chin% "mean") & !all(periods %in% c(0L, 1L))) {
      periods <- periods[periods > 1L]
      incre <- 0L
      TargetN <- 0L
      for(t in TargetS) {
        TargetN <- TargetN + 1L
        for(j in periods) {
          if(!is.null(timeDiffTarget)) {
            if(grepl(pattern = eval(timeDiffTarget),x = t)) {
              data[, eval(paste0(timeAggss, "_", "Mean_", j, "_", t)) := fBasics::rowAvgs(.SD), .SDcols = TimeLagCols[1:j]]
              PeriodKeep <- c(PeriodKeep, paste0(timeAggss, "_", "Mean_", j, "_", t))
            } else {
              data[, eval(paste0(timeAggss, "_", "Mean_", j, "_", t)) := fBasics::rowAvgs(.SD), .SDcols = LagColss[[TargetN]][1:j]]
              PeriodKeep <- c(PeriodKeep, paste0(timeAggss, "_", "Mean_", j, "_", t))
            }
          } else {
            data[, eval(paste0(timeAggss, "_", "Mean_", j, "_", t)) := fBasics::rowAvgs(.SD), .SDcols = LagColss[[TargetN]][1:j]]
            PeriodKeep <- c(PeriodKeep, paste0(timeAggss, "_", "Mean_", j, "_", t))
          }
        }

      }
    }

    # Standard Deviations----
    if(any(tolower(statsFUNs) %chin% "sd") & !all(SDperiods %in% c(0,1))) {
      tempperiods <- SDperiods[SDperiods > 1L]
      TargetN <- 0L
      for(t in TargetS) {
        TargetN <- TargetN + 1L
        for(j in tempperiods) {
          if(!is.null(timeDiffTarget)) {
            if(grepl(pattern = eval(timeDiffTarget),x = t)) {
              data[, eval(paste0(timeAggss, "_", "SD_", j, "_", t)) := fBasics::rowSds(.SD), .SDcols = TimeLagCols[1:j]]
              PeriodKeep <-  c(PeriodKeep, paste0(timeAggss, "_", "SD_", j, "_", t))
            } else {
              data[, eval(paste0(timeAggss, "_", "SD_", j, "_", t)) := fBasics::rowSds(.SD), .SDcols = LagColss[[TargetN]][1:j]]
              if(incre == 1) {
                PeriodKeep <- paste0(timeAggss, "_", "SD_", j, "_", t)
              } else {
                PeriodKeep <- c(PeriodKeep, paste0(timeAggss, "_", "SD_", j, "_", t))
              }
            }
          } else {
            data[, eval(paste0(timeAggss, "_", "SD_", j, "_", t)) := fBasics::rowSds(.SD), .SDcols = LagColss[[TargetN]][1:j]]
            PeriodKeep <- c(PeriodKeep, paste0(timeAggss, "_", "SD_", j, "_", t))
          }
        }

      }
    }

    # Skewness----
    if(any(tolower(statsFUNs) %chin% "skew") & !all(Skewperiods %in% c(0,1,2))) {
      tempperiods <- Skewperiods[Skewperiods > 2L]
      TargetN <- 0L
      for(t in TargetS) {
        TargetN <- TargetN + 1L
        for(j in tempperiods) {
          if(!is.null(timeDiffTarget)) {
            if(grepl(pattern = eval(timeDiffTarget),x = t)) {
              data[, eval(paste0(timeAggss, "_", "Skew_", j, "_", t)) := fBasics::rowSkewness(.SD), .SDcols = TimeLagCols[1:j]]
              PeriodKeep <- c(PeriodKeep, paste0(timeAggss, "_", "Skew_", j, "_", t))
            } else {
              data[, eval(paste0(timeAggss, "_", "Skew_", j, "_", t)) := fBasics::rowSkewness(.SD), .SDcols = LagColss[[TargetN]][1:j]]
              PeriodKeep <- c(PeriodKeep, paste0(timeAggss, "_", "Skew_", j, "_", t))
            }
          } else {
            data[, eval(paste0(timeAggss, "_", "Skew_", j, "_", t)) := fBasics::rowSkewness(.SD), .SDcols = LagColss[[TargetN]][1:j]]
            PeriodKeep <- c(PeriodKeep, paste0(timeAggss, "_", "Skew_", j, "_", t))
          }
        }
      }
    }

    # Kurtosis----
    if(any(tolower(statsFUNs) %chin% "kurt") & !all(Kurtperiods %in% c(0,1,2,3))) {
      tempperiods <- Kurtperiods[Kurtperiods > 3L]
      TargetN <- 0L
      for(t in TargetS) {
        TargetN <- TargetN + 1L
        for(j in tempperiods) {
          if(!is.null(timeDiffTarget)) {
            if(grepl(pattern = eval(timeDiffTarget),x = t)) {
              data[, eval(paste0(timeAggss, "_", "Kurt_", j, "_", t)) := fBasics::rowKurtosis(.SD), .SDcols = TimeLagCols[1:j]]
              PeriodKeep <- c(PeriodKeep, paste0(timeAggss, "_", "Kurt_", j, "_", t))
            } else {
              data[, eval(paste0(timeAggss, "_", "Kurt_", j, "_", t)) := fBasics::rowKurtosis(.SD), .SDcols = LagColss[[TargetN]][1:j]]
              PeriodKeep <- c(PeriodKeep, paste0(timeAggss, "_", "Kurt_", j, "_", t))
            }
          } else {
            data[, eval(paste0(timeAggss, "_", "Kurt_", j, "_", t)) := fBasics::rowKurtosis(.SD), .SDcols = LagColss[[TargetN]][1:j]]
            PeriodKeep <- c(PeriodKeep, paste0(timeAggss, "_", "Kurt_", j, "_", t))
          }
        }
      }
    }

    # Quantiles----
    if(!all(Quantileperiods %in% c(0,1,2,3,4))) {
      tempperiods <- Quantileperiods[Quantileperiods > 4L]
      for(z in c(seq(5,95,5))) {
        if(any(paste0("q",z) %chin% tolower(statsFUNs))) {
          TargetN <- 0L
          for(t in TargetS) {
            TargetN <- TargetN + 1L
            for(j in tempperiods) {
              if(!is.null(timeDiffTarget)) {
                if(grepl(pattern = eval(timeDiffTarget),x = t)) {
                  data[, eval(paste0(timeAggss, "_", "Q_",z,"_",j,"_",t)) := fBasics::rowQuantiles(x = .SD, prob = z/100), .SDcols = TimeLagCols[1:j]]
                  PeriodKeep <- c(PeriodKeep, paste0(timeAggss, "_", "Q_",z,"_",j,"_",t))
                } else {
                  data[, eval(paste0(timeAggss, "_", "Q_",z,"_",j,"_",t)) := rowQuantiles(x = .SD, prob = z/100), .SDcols = LagColss[[TargetN]][1:j]]
                  PeriodKeep <- c(PeriodKeep, paste0(timeAggss, "_", "Q_",z,"_",j,"_",t))
                }
              } else {
                data[, eval(paste0(timeAggss, "_", "Q_",z,"_",j,"_",t)) := rowQuantiles(x = .SD, prob = z/100), .SDcols = LagColss[[TargetN]][1:j]]
                PeriodKeep <- c(PeriodKeep, paste0(timeAggss, "_", "Q_",z,"_",j,"_",t))
              }
            }
          }
        }
      }
    }

    # Remove temporary lagged dates----
    if(!is.null(timeDiffTarget)) {
      nullcols <- c()
      for(l in seq_along(MaxCols+1)) nullcols <- c(nullcols, paste0(timeAggss, "_", "TEMP", l))
      data.table::set(data, j = nullcols, value = NULL)
    }

    # Only keep requested columns----
    if(is.null(timeDiffTarget)) {
      keep <- c(ColKeep, unlist(LagKeeps), PeriodKeep)
      data <- data[, ..keep]
    } else {
      keep <- c(ColKeep, unlist(LagKeeps), TimeLagKeep, PeriodKeep)
      data <- data[, ..keep]
    }

    # Replace any inf values with NA----
    for(col in seq_along(data)) data.table::set(data, j = col, value = replace(data[[col]], is.infinite(data[[col]]), NA))

    # Impute missing values----
    if(SimpleImpute) {
      for(j in seq_along(data)) {
        if(is.factor(data[[j]])) {
          data.table::set(data, which(!(data[[j]] %in% levels(data[[j]]))), j, "0")
        } else {
          data.table::set(data, which(is.na(data[[j]])), j, -1L)
        }
      }
    }

    # Done----
    if(AscRowRemove) {
      return(data[, eval(AscRowByGroup) := NULL])
    } else {
      return(data)
    }
  }
}
