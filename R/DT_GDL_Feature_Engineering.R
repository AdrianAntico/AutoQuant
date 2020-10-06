#' An Automated Feature Engineering Function Using data.table frollmean
#'
#' Builds autoregressive and moving average from target columns and distributed lags and distributed moving average for independent features distributed across time. On top of that, you can also create time between instances along with their associated lags and moving averages. This function works for data with groups and without groups.
#' @author Adrian Antico
#' @family Feature Engineering
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
#' @export
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
      if(ncol(data) +
         (length(lags) + length(periods)) * tarNum * length(groupingVars) * length(statsFUNs) > data.table::truelength(data)) {
        data.table::alloc.col(DT = data,
                              n = ncol(data) +
                                (length(lags) + length(periods)) * tarNum * length(groupingVars) * length(statsFUNs))
      }
    } else {
      if(ncol(data) +
         (length(lags) + length(periods)) * tarNum * 2 * length(groupingVars) * length(statsFUNs) > data.table::truelength(data)) {
        data.table::alloc.col(DT = data,
                              n = ncol(data) +
                                (length(lags) + length(periods)) * tarNum * 2 * length(groupingVars) * length(statsFUNs))
      }
    }
  } else {
    if(is.null(timeDiffTarget)) {
      if(ncol(data) +
         (length(lags) + length(periods)) * tarNum * length(statsFUNs) > data.table::truelength(data)) {
        data.table::alloc.col(DT = data,
                              n = ncol(data) +
                                (length(lags) + length(periods)) * tarNum * length(statsFUNs))
      }
    } else {
      if(ncol(data) +
         (length(lags) + length(periods)) * tarNum * 2 * length(statsFUNs) > data.table::truelength(data)) {
        data.table::alloc.col(DT = data,
                              n = ncol(data) +
                                (length(lags) + length(periods)) * tarNum * 2 * length(statsFUNs))
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
