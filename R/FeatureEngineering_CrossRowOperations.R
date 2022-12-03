#' @title Mode
#'
#' @description Statistical mode. Only returns the first mode if there are many
#'
#' @author Adrian Antico
#' @family EDA
#'
#' @param x vector
#' @export
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#' @title AutoLagRollMode
#'
#' @description Create lags and rolling modes for categorical variables.
#'
#' @author Adrian Antico
#' @family Feature Engineering
#'
#' @param data A data.table you want to run the function on
#' @param Lags A numeric vector of the specific lags you want to have generated. You must include 1 if WindowingLag = 1.
#' @param ModePeriods A numberic vector of window sizes
#' @param Targets A character vector of the column names for the reference column in which you will build your lags and rolling stats
#' @param GroupingVars A character vector of categorical variable names you will build your lags and rolling stats by
#' @param SortDateName The column name of your date column used to sort events over time
#' @param WindowingLag Set to 0 to build rolling stats off of target columns directly or set to 1 to build the rolling stats off of the lag-1 target
#' @param Type List either "Lag" if you want features built on historical values or "Lead" if you want features built on future values
#' @param SimpleImpute Set to TRUE for factor level imputation of "0" and numeric imputation of -1
#' @param Debug = FALSE
#' @return data.table of original data plus created lags, rolling stats, and time between event lags and rolling stats
#' @examples
#' \dontrun{
#' # NO GROUPING CASE: Create fake Panel Data----
#' Count <- 1L
#' for(Level in LETTERS) {
#'   datatemp <- AutoQuant::FakeDataGenerator(
#'     Correlation = 0.75,
#'     N = 25000L,
#'     ID = 0L,
#'     ZIP = 0L,
#'     FactorCount = 2L,
#'     AddDate = TRUE,
#'     Classification = FALSE,
#'     MultiClass = FALSE)
#'   datatemp[, Factor1 := eval(Level)]
#'   if(Count == 1L) {
#'     data <- data.table::copy(datatemp)
#'   } else {
#'     data <- data.table::rbindlist(
#'       list(data, data.table::copy(datatemp)))
#'   }
#'   Count <- Count + 1L
#' }
#'
#' # NO GROUPING CASE: Create rolling modes for categorical features
#' data <- AutoQuant::AutoLagRollMode(
#'   data,
#'   Lags           = seq(1,5,1),
#'   ModePeriods    = seq(2,5,1),
#'   Targets        = c("Factor_1"),
#'   GroupingVars   = NULL,
#'   SortDateName   = "DateTime",
#'   WindowingLag   = 1,
#'   Type           = "Lag",
#'   SimpleImpute   = TRUE)
#'
#' # GROUPING CASE: Create fake Panel Data----
#' Count <- 1L
#' for(Level in LETTERS) {
#'   datatemp <- AutoQuant::FakeDataGenerator(
#'     Correlation = 0.75,
#'     N = 25000L,
#'     ID = 0L,
#'     ZIP = 0L,
#'     FactorCount = 2L,
#'     AddDate = TRUE,
#'     Classification = FALSE,
#'     MultiClass = FALSE)
#'   datatemp[, Factor1 := eval(Level)]
#'   if(Count == 1L) {
#'     data <- data.table::copy(datatemp)
#'   } else {
#'     data <- data.table::rbindlist(
#'       list(data, data.table::copy(datatemp)))
#'   }
#'   Count <- Count + 1L
#' }
#'
#' # GROUPING CASE: Create rolling modes for categorical features
#' data <- AutoQuant::AutoLagRollMode(
#'   data,
#'   Lags           = seq(1,5,1),
#'   ModePeriods    = seq(2,5,1),
#'   Targets        = c("Factor_1"),
#'   GroupingVars   = "Factor_2",
#'   SortDateName   = "DateTime",
#'   WindowingLag   = 1,
#'   Type           = "Lag",
#'   SimpleImpute   = TRUE)
#' }
#' @export
AutoLagRollMode <- function(data,
                            Lags = 1,
                            ModePeriods = 0,
                            Targets = NULL,
                            GroupingVars = NULL,
                            SortDateName = NULL,
                            WindowingLag = 0,
                            Type = c("Lag"),
                            SimpleImpute = TRUE,
                            Debug = FALSE) {

  # Number of Targets
  tarNum <- length(Targets)

  if(Debug) print('AutoLagRollMode 1')

  # Argument Checks ----
  if(is.null(Lags) && WindowingLag == 1) Lags <- 1
  if(!(1 %in% Lags) && WindowingLag == 1) Lags <- c(1, Lags)
  if(any(Lags < 0)) stop("Lags need to be positive integers")
  if(!is.null(GroupingVars)) if(!is.character(GroupingVars)) stop("GroupingVars needs to be a character scalar or vector")
  if(!is.character(Targets)) stop("Targets needs to be a character scalar or vector")
  if(!is.character(SortDateName)) stop("SortDateName needs to be a character scalar or vector")
  if(!(WindowingLag %in% c(0, 1))) stop("WindowingLag needs to be either 0 or 1")
  if(!(tolower(Type) %chin% c("lag", "lead"))) stop("Type needs to be either Lag or Lead")
  if(!is.logical(SimpleImpute)) stop("SimpleImpute needs to be TRUE or FALSE")

  if(Debug) print('AutoLagRollMode 2')

  # Ensure enough columns are allocated beforehand----
  if(!is.null(GroupingVars)) {
    if(Debug) print('AutoLagRollMode 3.a')
    if(ncol(data) + (length(Lags) + length(ModePeriods)) * tarNum * length(GroupingVars) > data.table::truelength(data)) {
      data.table::alloc.col(DT = data, n = ncol(data) + (length(Lags) + length(ModePeriods)) * tarNum * length(GroupingVars))
    }
  } else {
    if(Debug) print('AutoLagRollMode 3.b')
    if(ncol(data) + (length(Lags) + length(ModePeriods)) * tarNum > data.table::truelength(data)) {
      data.table::alloc.col(DT = data, n = ncol(data) + (length(Lags) + length(ModePeriods)) * tarNum)
    }
  }

  if(Debug) print('AutoLagRollMode 4')

  # Names
  ColumnNames <- names(data.table::copy(data))

  if(Debug) print('AutoLagRollMode 5')

  # Ensure Targets are Factors
  class_switch <- c()
  for(fac in Targets) {
    if(class(data[[eval(fac)]])[1L] %in% c('character')) {
      class_switch <- c(class_switch, fac)
      data.table::set(data, j = eval(fac), value = as.factor(data[[eval(fac)]]))
    }
  }

  # Begin feature engineering ----
  if(!is.null(GroupingVars)) {

    if(Debug) print('AutoLagRollMode 6')

    # i = 1L
    for(i in seq_along(GroupingVars)) {

      if(Debug) print('AutoLagRollMode 7')

      # Sort data ----
      if(tolower(Type) == "lag") {
        colVar <- c(GroupingVars[i], SortDateName[1L])
        data.table::setorderv(data, colVar, order = 1L)
      } else {
        colVar <- c(GroupingVars[i], SortDateName[1L])
        data.table::setorderv(data, colVar, order = -1L)
      }

      if(Debug) print('AutoLagRollMode 8')

      # Lags ----
      LAG_Names <- c()
      for(t in Targets) LAG_Names <- c(LAG_Names, paste0(GroupingVars[i], "_LAG_", Lags, "_", t))
      data[, paste0(LAG_Names) := data.table::shift(.SD, n = Lags, type = "lag"), by = c(GroupingVars[i]), .SDcols = Targets]

      if(Debug) print('AutoLagRollMode 9')

      # Define targets ----
      if(WindowingLag > 0L) {
        Targets <- c(paste0(GroupingVars[i], "_LAG_", WindowingLag, "_", Targets))
      }

      if(Debug) print('AutoLagRollMode 10')

      # Mode ----
      if(!all(ModePeriods %in% c(0,1))) {

        if(Debug) print('AutoLagRollMode 11')

        tempperiods <- ModePeriods[ModePeriods > 1L]
        Mode_Names <- c()
        if(Debug) print('AutoLagRollMode 12.a')
        counter <- 1L
        temp_targets <- c()
        print(data)
        for(gg in Targets) {
          data[is.na(get(gg)), eval(gg) := "0"]
          temp_targets <- c(temp_targets, paste0("TEMP_", gg))
          if(Debug) print(str(data))
          data[, paste0("TEMP_", gg) := unclass(get(gg))]
          if(Debug) print(str(data))
          counter <- counter + 1L
        }

        if(Debug) print('AutoLagRollMode 13')

        for(t in Targets) for(j in seq_along(tempperiods)) Mode_Names <- c(Mode_Names, paste0(GroupingVars[i], "Mode_", tempperiods[j], "_", t))

        if(Debug) {
          print(data)
          print(temp_targets)
        }

        data[, paste0(Mode_Names) := data.table::frollapply(x = .SD, n = tempperiods, FUN = Mode), by = c(GroupingVars[i]), .SDcols = c(temp_targets)]

        if(Debug) print('AutoLagRollMode 14')

        # Convert back to catgegorical ----
        if(Debug) print('AutoLagRollMode 15')
        for(t in seq_along(Targets)) {
          if(Debug) print('AutoLagRollMode 16')
          for(j in seq_along(tempperiods)) {
            if(Debug) print('AutoLagRollMode 17')
            temp <- data[, .N, by = c(Targets[t], temp_targets[t])][, N := NULL]
            data.table::setkeyv(temp, paste0("TEMP_", Targets[t]))
            data[, paste0("remove_", GroupingVars[i], "Mode_", tempperiods[j], "_", Targets[t]) := get(paste0(GroupingVars[i], "Mode_", tempperiods[j], "_", Targets[t]))]
            data[, paste0(GroupingVars[i], "Mode_", tempperiods[j], "_", Targets[t]) := NULL]
            data.table::setkeyv(data, paste0("remove_", GroupingVars[i], "Mode_", tempperiods[j], "_", Targets[t]))
            data[temp, paste0(GroupingVars[i], "Mode_", tempperiods[j], "_", Targets[t]) := as.character(get(paste0("i.", Targets[t])))]
            data[, paste0("remove_", GroupingVars[i], "Mode_", tempperiods[j], "_", Targets[t]) := NULL]
          }
        }

        if(Debug) print('AutoLagRollMode 18')

        # Remove TEMP_ columns
        data.table::set(data, j = c(names(data)[names(data) %like% "TEMP_"]), value = NULL)
      }
    }

    if(Debug) print('AutoLagRollMode 19')

    # Impute missing values ----
    UpdateColumnNames <- setdiff(names(data), ColumnNames)
    if(SimpleImpute) {
      for(j in which(names(data) %in% UpdateColumnNames)) {
        if(is.factor(data[[j]])) {
          data.table::set(data, which(!(data[[j]] %in% levels(data[[j]]))), j, "0")
        } else if(is.character(data[[j]])) {
          data.table::set(data, which(is.na(data[[j]])), j, "0")
        } else {
          data.table::set(data, which(is.na(data[[j]])), j, -1)
        }
      }
    }

    if(Debug) print('AutoLagRollMode 20')

    # Convert changed types back to original types
    if(length(class_switch) > 0L) {
      for(fac in class_switch) data.table::set(data, j = eval(fac), value = as.character(data[[eval(fac)]]))
    }

    # Done!! ----
    return(data)

  } else {

    if(Debug) print('AutoLagRollMode 6.b')

    # Non grouping case
    if(tolower(Type) == "lag") {
      colVar <- c(SortDateName[1L])
      data.table::setorderv(data, colVar, order = 1L)
    } else {
      colVar <- c(SortDateName[1L])
      data.table::setorderv(data, colVar, order = -1L)
    }

    if(Debug) print('AutoLagRollMode 7.b')

    # Lags ----
    LAG_Names <- c()
    for(t in Targets) LAG_Names <- c(LAG_Names, paste0("LAG_", Lags, "_", t))

    if(Debug) print('AutoLagRollMode 8.b')

    # Build features ----
    data[, paste0(LAG_Names) := data.table::shift(.SD, n = Lags, type = "lag"), .SDcols = c(Targets)]

    if(Debug) print('AutoLagRollMode 9.b')

    # Define targets ----
    if(WindowingLag != 0L) {
      Targets <- c(paste0("LAG_", WindowingLag, "_", Targets))
    } else {
      Targets <- Targets
    }

    if(Debug) print('AutoLagRollMode 10.b')

    # Mode ----
    if(!all(ModePeriods %in% c(0,1))) {

      if(Debug) print('AutoLagRollMode 11.b')

      tempperiods <- ModePeriods[ModePeriods > 1L]
      Mode_Names <- c()
      g <- names(data)[which(sapply(data, is.factor))]
      if(!identical(integer(0), g)) {

        if(Debug) print('AutoLagRollMode 12.b')

        cats <- g[which(g %chin% Targets)]
        counter <- 1L
        temp_targets <- c()
        for(gg in cats) {

          if(Debug) print('AutoLagRollMode 13.b')

          data[is.na(get(gg)), eval(gg) := "0"]
          temp_targets <- c(temp_targets, paste0("TEMP_", gg))
          data[, paste0("TEMP_", gg) := unclass(get(gg))]
          counter <- counter + 1L
        }
      } else {
        if(Debug) print('AutoLagRollMode 11.bb')
        temp_targets <- Targets
      }

      if(Debug) print('AutoLagRollMode 14.b')

      for(t in Targets) for(j in seq_along(tempperiods)) Mode_Names <- c(Mode_Names, paste0("Mode_", tempperiods[j], "_", t))
      data[, paste0(Mode_Names) := data.table::frollapply(x = .SD, n = tempperiods, FUN = Mode), .SDcols = c(temp_targets)]

      if(Debug) print('AutoLagRollMode 15.b')

      # Convert back to catgegorical
      if(!identical(integer(0), g)) {
        if(Debug) print('AutoLagRollMode 16.b')
        for(t in seq_along(Targets)) {
          if(Debug) print('AutoLagRollMode 17.b')
          for(j in seq_along(tempperiods)) {
            if(Debug) print('AutoLagRollMode 18.b')
            temp <- data[, .N, by = c(Targets[t], temp_targets[t])][, N := NULL]
            data.table::setkeyv(temp, paste0("TEMP_", Targets[t]))
            data[, paste0("remove_", "Mode_", tempperiods[j], "_", Targets[t]) := get(paste0("Mode_", tempperiods[j], "_", Targets[t]))]
            data[, paste0("Mode_", tempperiods[j], "_", Targets[t]) := NULL]
            data.table::setkeyv(data, paste0("remove_", "Mode_", tempperiods[j], "_", Targets[t]))
            data[temp, paste0("Mode_", tempperiods[j], "_", Targets[t]) := as.character(get(paste0("i.", Targets[t])))]
            data[, paste0("remove_", "Mode_", tempperiods[j], "_", Targets[t]) := NULL]
          }
        }
      }

      if(Debug) print('AutoLagRollMode 19.b')

      # Remove TEMP_ columns
      data.table::set(data, j = c(names(data)[names(data) %like% "TEMP_"]), value = NULL)
    }

    if(Debug) print('AutoLagRollMode 20.b')

    # Impute missing values ----
    UpdateColumnNames <- setdiff(names(data), ColumnNames)
    if(SimpleImpute) {
      for(j in which(names(data) %in% UpdateColumnNames)) {
        if(is.factor(data[[j]])) {
          data.table::set(data, which(!(data[[j]] %in% levels(data[[j]]))), j, "0")
        } else if(is.character(data[[j]])) {
          data.table::set(data, which(is.na(data[[j]])), j, "0")
        } else {
          data.table::set(data, which(is.na(data[[j]])), j, -1)
        }
      }
    }

    if(Debug) print('AutoLagRollMode 21.b')

    # Convert changed types back to original types
    if(length(class_switch) > 0L) {
      for(fac in class_switch) data.table::set(data, j = eval(fac), value = as.character(data[[eval(fac)]]))
    }

    # Done!! ----
    return(data)
  }
}

#' @title DiffDT
#'
#' @description Difference a column in a data.table
#'
#' @author Adrian Antico
#' @family Misc
#'
#' @param data Source data
#' @param x Column name
#' @param NLag1 Numeric
#' @param NLag2 Numeric
#' @param Type Choose from 'numeric' or 'date'
#' @noRd
DiffDT <- function(data, x, NLag1, NLag2, Type = "numeric") {
  if(Type == "numeric") {
    if(NLag1 == 0) {
      temp <- data[[eval(x)]] - data[[paste0("Diff_", NLag2, "_", x)]]
    } else {
      temp <- data[[paste0("Diff_", NLag1, "_", x)]] - data[[paste0("Diff_", NLag1,"-", NLag2, "_", x)]]
    }
  } else if(Type == "date") {
    if(NLag1 == 0) {
      temp <- difftime(time1 = data[[eval(x)]], time2 = data[[paste0("Diff_", NLag2, "_", x, "_temp")]], units = "days")
    } else {
      temp <- difftime(time1 = data[[paste0("Diff_",NLag1, "_", x, "_temp")]], time2 = data[[paste0("Diff_", NLag1,"-", NLag2, "_", x, "_temp")]], units = "days")
    }
  } else if(Type == "categorical") {
    if(NLag1 == 0) {
      temp <- "No_Change"
      temp <- ifelse(data[[eval(x)]] != data[[paste0("Diff_", NLag2, "_", x)]], paste0("New=",data[[eval(x)]]," Old=",data[[paste0("Diff_", NLag2, "_", x)]]), "No_Change")
    } else {
      temp <- ifelse(data[[paste0("Diff_", NLag1, "_", x)]] != data[[paste0("Diff_", NLag2, "_", x)]], paste0("New=",data[[eval(x)]]," Old=",data[[paste0("Diff_", NLag2, "_", x)]]), "No_Change")
    }
  }
  if(Type == "categorical") {
    return(temp)
  } else {
    return(as.numeric(temp))
  }
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
#' @param DiffGroupVariables Column names for categorical variables to difference. If no change then the output is 'No_Change' else 'New=NEWVAL Old=OLDVAL' where NEWVAL and OLDVAL are placeholders for the actual values
#' @param NLag1 If the diff calc, we have column 1 - column 2. NLag1 is in reference to column 1. If you want to take the current value minus the previous weeks value, supply a zero. If you want to create a lag2 - lag4 NLag1 gets a 2.
#' @param NLag2 If the diff calc, we have column 1 - column 2. NLag2 is in reference to column 2. If you want to take the current value minus the previous weeks value, supply a 1. If you want to create a lag2 - lag4 NLag1 gets a 4.
#' @param Type 'lag' or 'lead'
#' @param Sort TRUE to sort your data inside the function
#' @param RemoveNA Set to TRUE to remove rows with NA generated by the lag operation
#' @examples
#' \dontrun{
#'
#' # Create fake data
#' data <- AutoQuant::FakeDataGenerator(
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
#' data <- AutoQuant::ModelDataPrep(data = data, Impute = FALSE, CharToFactor = FALSE, FactorToChar = TRUE)
#'
#' # Run function
#' data <- AutoQuant::AutoDiffLagN(
#'   data,
#'   DateVariable = "DateTime",
#'   GroupVariables = c("Factor_1", "Factor_2"),
#'   DiffVariables = Cols,
#'   DiffDateVariables = NULL,
#'   DiffGroupVariables = NULL,
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
                         DiffGroupVariables = NULL,
                         NLag1 = 0L,
                         NLag2 = 1L,
                         Type = 'lag',
                         Sort = FALSE,
                         RemoveNA = TRUE) {

  # Ensure data.table ----
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  # Check args ----
  if(!is.null(DateVariable) && !is.character(DateVariable)) stop("DateVariable needs to be a charcter valued vector or scalar")
  if(!is.null(GroupVariables) && !is.character(GroupVariables)) stop("GroupVariables needs to be a charcter valued vector or scalar")
  if(!is.numeric(NLag1)) stop("NLag1 needs to be a numeric valued scalar")
  if(!is.numeric(NLag2)) stop("NLag2 needs to be a numeric valued scalar")
  if(NLag1 < 0) stop("NLag1 needs to be a positive numeric valued scalar")
  if(NLag2 < 0) stop("NLag2 needs to be a positive numeric valued scalar")
  if(!is.logical(Sort)) stop("Sort needs to be a logical valued scalar")

  # Sort if TRUE ----
  if(!is.null(GroupVariables)) {
    data.table::setorderv(x = data, cols = c(GroupVariables, DateVariable), order = c(rep(1, length(GroupVariables)), 1), na.last = FALSE)
  } else {
    data.table::setorderv(x = data, cols = c(DateVariable), order = 1L, na.last = FALSE)
  }

  # Diff numeric data ----
  if(!is.null(DiffVariables)) {
    if(NLag1 == 0L) {
      ModDiffVariables <- paste0("Diff_", NLag2, "_", DiffVariables)
      data[, (ModDiffVariables) := lapply(.SD, collapse::fdiff, n = NLag2), .SDcols = c(DiffVariables)]
    } else {
      ModDiffVariables1 <- paste0("Diff_", NLag1, "_", DiffVariables)
      ModDiffVariables2 <- paste0("Diff_", NLag1,"-", NLag2, "_", DiffVariables)
      if(!is.null(GroupVariables)) {
        data <- data[, (ModDiffVariables1) := data.table::shift(x = .SD, n = NLag1, fill = NA, type = Type), .SDcols = c(DiffVariables), by = eval(GroupVariables)]
        data <- data[, (ModDiffVariables2) := data.table::shift(x = .SD, n = NLag2, fill = NA, type = Type), .SDcols = c(DiffVariables), by = eval(GroupVariables)]
      } else {
        data <- data[, (ModDiffVariables1) := data.table::shift(x = .SD, n = NLag1, fill = NA, type = Type), .SDcols = c(DiffVariables)]
        data <- data[, (ModDiffVariables2) := data.table::shift(x = .SD, n = NLag2, fill = NA, type = Type), .SDcols = c(DiffVariables)]
      }
      data[, (ModDiffVariables2) := {g <- list(); for(x in DiffVariables) g[[x]] <- DiffDT(data, x, NLag1, NLag2, Type = "numeric"); g}]
      data.table::set(data, j = ModDiffVariables1, value = NULL)
    }
  }

  # Diff date data ----
  if(!is.null(DiffDateVariables)) {
    if(NLag1 == 0L) {
      ModDiffVariables1 <- paste0("Diff_", NLag2, "_", DiffDateVariables)
      ModDiffVariables2 <- paste0("Diff_", NLag2, "_", DiffDateVariables, "_temp")
      if(!is.null(GroupVariables)) {
        data <- data[, (ModDiffVariables2) := data.table::shift(x = .SD, n = NLag2, fill = NA, type = Type), .SDcols = c(DiffDateVariables), by = eval(GroupVariables)]
      } else {
        data <- data[, (ModDiffVariables2) := data.table::shift(x = .SD, n = NLag2, fill = NA, type = Type), .SDcols = c(DiffDateVariables)]
      }
      data <- data[, (ModDiffVariables1) := {g <- list(); for(x in DiffDateVariables) g[[x]] <- DiffDT(data, x, NLag1, NLag2, Type = "date"); g}]
      data.table::set(data, j = ModDiffVariables2, value = NULL)
    } else {
      ModDiffVariables1 <- paste0("Diff_", NLag1, "_", DiffDateVariables,"_temp")
      ModDiffVariables2 <- paste0("Diff_", NLag1,"-", NLag2, "_", DiffDateVariables,"_temp")
      ModDiffVariables22 <- paste0("Diff_", NLag1,"-", NLag2, "_", DiffDateVariables)
      if(!is.null(GroupVariables)) {
        data <- data[, (ModDiffVariables1) := data.table::shift(x = .SD, n = NLag1, fill = NA, type = Type), .SDcols = c(DiffDateVariables), by = eval(GroupVariables)]
        data <- data[, (ModDiffVariables2) := data.table::shift(x = .SD, n = NLag2, fill = NA, type = Type), .SDcols = c(DiffDateVariables), by = eval(GroupVariables)]
      } else {
        data <- data[, (ModDiffVariables1) := data.table::shift(x = .SD, n = NLag1, fill = NA, type = Type), .SDcols = c(DiffDateVariables)]
        data <- data[, (ModDiffVariables2) := data.table::shift(x = .SD, n = NLag2, fill = NA, type = Type), .SDcols = c(DiffDateVariables)]
      }
      data <- data[, (ModDiffVariables22) := {g <- list(); for(x in DiffDateVariables) g[[x]] <- DiffDT(data, x, NLag1, NLag2, Type = "date"); g}]
      data.table::set(data, j = c(ModDiffVariables1, ModDiffVariables2), value = NULL)
    }
  }

  # Diff categorical data ----
  if(!is.null(DiffGroupVariables)) {
    if(NLag1 == 0L) {
      ModDiffVariables <- paste0("Diff_", NLag2, "_", DiffGroupVariables)
      if(!is.null(GroupVariables)) {
        data <- data[, (ModDiffVariables) := data.table::shift(x = .SD, n = NLag2, fill = "missing", type = Type), .SDcols = c(DiffGroupVariables), by = eval(GroupVariables)]
      } else {
        data <- data[, (ModDiffVariables) := data.table::shift(x = .SD, n = NLag2, fill = "missing", type = Type), .SDcols = c(DiffGroupVariables)]
      }
      data <- data[, (ModDiffVariables) := {g <- list(); for(x in DiffGroupVariables) g[[x]] <- DiffDT(data, x, NLag1, NLag2, Type = "categorical"); g}]
    } else {
      ModDiffVariables1 <- paste0("Diff_", NLag1, "_", DiffGroupVariables)
      ModDiffVariables2 <- paste0("Diff_", NLag1,"-", NLag2, "_", DiffGroupVariables)
      if(!is.null(GroupVariables)) {
        data <- data[, (ModDiffVariables1) := data.table::shift(x = .SD, n = NLag1, fill = "missing", type = Type), .SDcols = c(DiffGroupVariables), by = eval(GroupVariables)]
        data <- data[, (ModDiffVariables2) := data.table::shift(x = .SD, n = NLag2, fill = "missing", type = Type), .SDcols = c(DiffGroupVariables), by = eval(GroupVariables)]
      } else {
        data <- data[, (ModDiffVariables1) := data.table::shift(x = .SD, n = NLag1, fill = "missing", type = Type), .SDcols = c(DiffGroupVariables)]
        data <- data[, (ModDiffVariables2) := data.table::shift(x = .SD, n = NLag2, fill = "missing", type = Type), .SDcols = c(DiffGroupVariables)]
      }
      data <- data[, (ModDiffVariables2) := {g <- list(); for(x in DiffGroupVariables) g[[x]] <- DiffDT(data, x, NLag1, NLag2, Type = "categorical"); g}]
      data.table::set(data, j = ModDiffVariables1, value = NULL)
    }
  }

  # Final prep ----
  if(RemoveNA) {
    if(NLag1 == 0L) {
      if(!is.null(DiffVariables)) {
        data <- data[!is.na(get(paste0("Diff_", NLag2, "_", DiffVariables[[1L]])))]
      } else if(!is.null(DiffDateVariables)) {
        data <- data[!is.na(get(paste0("Diff_", NLag2, "_", DiffDateVariables[[1L]])))]
      } else if(!is.null(DiffGroupVariables)) {
        data <- data[!is.na(get(paste0("Diff_", NLag2, "_", DiffGroupVariables[[1L]])))]
      }
    } else {
      if(!is.null(DiffVariables)) {
        data <- data[!is.na(get(paste0("Diff_", NLag1,"-", NLag2, "_", DiffVariables[[1L]])))]
      } else if(!is.null(DiffDateVariables)) {
        if(!is.null(DiffDateVariables)) data <- data[!is.na(get(paste0("Diff_", NLag1,"-", NLag2, "_", DiffDateVariables[[1L]])))]
      } else if(!is.null(DiffGroupVariables)) {
        data <- data[!is.na(get(paste0("Diff_", NLag1,"-", NLag2, "_", DiffGroupVariables[[1L]])))]
      }
    }
  }

  # Return data
  return(data)
}

#' @title AutoLagRollStats
#'
#' @description AutoLagRollStats Builds lags and a large variety of rolling statistics with options to generate them for hierarchical categorical interactions.
#'
#' @author Adrian Antico
#' @family Feature Engineering
#'
#' @param data A data.table you want to run the function on
#' @param Targets A character vector of the column names for the reference column in which you will build your lags and rolling stats
#' @param DateColumn The column name of your date column used to sort events over time
#' @param IndependentGroups A vector of categorical column names that you want to have run independently of each other. This will mean that no interaction will be done.
#' @param HierarchyGroups A vector of categorical column names that you want to have generate all lags and rolling stats done for the individual columns and their full set of interactions.
#' @param TimeGroups A vector of TimeUnits indicators to specify any time-aggregated GDL features you want to have returned. E.g. c("raw" (no aggregation is done),"hour", "day","week","month","quarter","year")
#' @param TimeBetween Specify a desired name for features created for time between events. Set to NULL if you don't want time between events features created.
#' @param TimeUnit List the time aggregation level for the time between events features, such as "hour", "day", "weeks", "months", "quarter", or "year"
#' @param TimeUnitAgg List the time aggregation of your data that you want to use as a base time unit for your features. E.g. "raw" or "day"
#' @param Lags A numeric vector of the specific lags you want to have generated. You must include 1 if WindowingLag = 1.
#' @param MA_RollWindows A numeric vector of the specific rolling statistics window sizes you want to utilize in the calculations.
#' @param SD_RollWindows  A numeric vector of Standard Deviation rolling statistics window sizes you want to utilize in the calculations.
#' @param Skew_RollWindows  A numeric vector of Skewness rolling statistics window sizes you want to utilize in the calculations.
#' @param Kurt_RollWindows  A numeric vector of Kurtosis rolling statistics window sizes you want to utilize in the calculations.
#' @param Quantile_RollWindows A numeric vector of Quantile rolling statistics window sizes you want to utilize in the calculations.
#' @param Quantiles_Selected Select from the following c("q5", "q10", "q15", "q20", "q25", "q30", "q35", "q40", "q45", "q50", "q55", "q60"," q65", "q70", "q75", "q80", "q85", "q90", "q95")
#' @param RollOnLag1 Set to FALSE to build rolling stats off of target columns directly or set to TRUE to build the rolling stats off of the lag-1 target
#' @param Type List either "Lag" if you want features built on historical values or "Lead" if you want features built on future values
#' @param SimpleImpute Set to TRUE for factor level imputation of "0" and numeric imputation of -1
#' @param ShortName Default TRUE. If FALSE, Group Variable names will be added to the rolling stat and lag names. If you plan on have multiple versions of lags and rollings stats by different group variables then set this to FALSE.
#' @param Debug Set to TRUE to get a print of which steps are running
#' @return data.table of original data plus created lags, rolling stats, and time between event lags and rolling stats
#' @examples
#' \dontrun{
#' # Create fake Panel Data----
#' Count <- 1L
#' for(Level in LETTERS) {
#'   datatemp <- AutoQuant::FakeDataGenerator(
#'     Correlation = 0.75,
#'     N = 25000L,
#'     ID = 0L,
#'     ZIP = 0L,
#'     FactorCount = 0L,
#'     AddDate = TRUE,
#'     Classification = FALSE,
#'     MultiClass = FALSE)
#'   datatemp[, Factor1 := eval(Level)]
#'   if(Count == 1L) {
#'     data <- data.table::copy(datatemp)
#'   } else {
#'     data <- data.table::rbindlist(
#'       list(data, data.table::copy(datatemp)))
#'   }
#'   Count <- Count + 1L
#' }
#'
#' # Add scoring records
#' data <- AutoQuant::AutoLagRollStats(
#'   data                 = data,
#'   DateColumn           = "DateTime",
#'   Targets              = "Adrian",
#'   HierarchyGroups      = NULL,
#'   IndependentGroups    = c("Factor1"),
#'   TimeUnitAgg          = "days",
#'   TimeGroups           = c("days","weeks","months","quarters"),
#'   TimeBetween          = NULL,
#'   TimeUnit             = "days",
#'   RollOnLag1           = TRUE,
#'   Type                 = "Lag",
#'   SimpleImpute         = TRUE,
#'   Lags                 = list("days" = c(seq(1,5,1)), "weeks" = c(seq(1,3,1)), "months" = c(seq(1,2,1)), "quarters" = c(seq(1,2,1))),
#'   MA_RollWindows       = list("days" = c(seq(1,5,1)), "weeks" = c(seq(1,3,1)), "months" = c(seq(1,2,1)), "quarters" = c(seq(1,2,1))),
#'   SD_RollWindows       = list("days" = c(seq(1,5,1)), "weeks" = c(seq(1,3,1)), "months" = c(seq(1,2,1)), "quarters" = c(seq(1,2,1))),
#'   Skew_RollWindows     = list("days" = c(seq(1,5,1)), "weeks" = c(seq(1,3,1)), "months" = c(seq(1,2,1)), "quarters" = c(seq(1,2,1))),
#'   Kurt_RollWindows     = list("days" = c(seq(1,5,1)), "weeks" = c(seq(1,3,1)), "months" = c(seq(1,2,1)), "quarters" = c(seq(1,2,1))),
#'   Quantile_RollWindows = list("days" = c(seq(1,5,1)), "weeks" = c(seq(1,3,1)), "months" = c(seq(1,2,1)), "quarters" = c(seq(1,2,1))),
#'   Quantiles_Selected   = c('q5','q50'),
#'   Debug                = FALSE)
#' }
#' @export
AutoLagRollStats <- function(data,
                             Targets              = NULL,
                             HierarchyGroups      = NULL,
                             IndependentGroups    = NULL,
                             DateColumn           = NULL,
                             TimeUnit             = NULL,
                             TimeUnitAgg          = NULL,
                             TimeGroups           = NULL,
                             TimeBetween          = NULL,
                             RollOnLag1           = TRUE,
                             Type                 = "Lag",
                             SimpleImpute         = TRUE,
                             Lags                 = NULL,
                             MA_RollWindows       = NULL,
                             SD_RollWindows       = NULL,
                             Skew_RollWindows     = NULL,
                             Kurt_RollWindows     = NULL,
                             Quantile_RollWindows = NULL,
                             Quantiles_Selected   = NULL,
                             ShortName            = TRUE,
                             Debug                = FALSE) {

  # Define args ----
  RollFunctions <- c()
  if(!is.null(MA_RollWindows)) RollFunctions <- c(RollFunctions,"mean")
  if(!is.null(SD_RollWindows)) RollFunctions <- c(RollFunctions,"sd")
  if(!is.null(Skew_RollWindows)) RollFunctions <- c(RollFunctions,"skew")
  if(!is.null(Kurt_RollWindows)) RollFunctions <- c(RollFunctions,"kurt")
  if(!is.null(Quantiles_Selected)) RollFunctions <- c(RollFunctions,Quantiles_Selected)
  if(is.null(TimeBetween)) TimeBetween <- NULL else TimeBetween <- "TimeBetweenRecords"
  if(RollOnLag1) RollOnLag1 <- 1L else RollOnLag1 <- 0L
  TimeGroupPlaceHolder <- c()
  if("raw" %chin% tolower(TimeGroups)) TimeGroupPlaceHolder <- c(TimeGroupPlaceHolder, "raw")
  if(any(c("hours","hour","hr","hrs","hourly") %chin% tolower(TimeGroups))) {
    TimeGroupPlaceHolder <- c(TimeGroupPlaceHolder, "hour")
    if(is.list(Lags)) names(Lags)[which(names(Lags) %chin% c("hours","hour","hr","hrs","hourly"))] <- "hour"
    if(is.list(MA_RollWindows)) names(MA_RollWindows)[which(names(MA_RollWindows) %chin% c("hours","hour","hr","hrs","hourly"))] <- "hour"
    if(is.list(SD_RollWindows)) names(SD_RollWindows)[which(names(SD_RollWindows) %chin% c("hours","hour","hr","hrs","hourly"))] <- "hour"
    if(is.list(Skew_RollWindows)) names(Skew_RollWindows)[which(names(Skew_RollWindows) %chin% c("hours","hour","hr","hrs","hourly"))] <- "hour"
    if(is.list(Kurt_RollWindows)) names(Kurt_RollWindows)[which(names(Kurt_RollWindows) %chin% c("hours","hour","hr","hrs","hourly"))] <- "hour"
    if(is.list(Quantile_RollWindows)) names(Quantile_RollWindows)[which(names(Quantile_RollWindows) %chin% c("hours","hour","hr","hrs","hourly"))] <- "hour"
  }
  if(any(c("days","day","dy","dd","d") %chin% tolower(TimeGroups))) {
    TimeGroupPlaceHolder <- c(TimeGroupPlaceHolder, "day")
    if(is.list(Lags)) names(Lags)[which(names(Lags) %chin% c("days","day","dy","dd","d"))] <- "day"
    if(is.list(MA_RollWindows)) names(MA_RollWindows)[which(names(MA_RollWindows) %chin% c("days","day","dy","dd","d"))] <- "day"
    if(is.list(SD_RollWindows)) names(SD_RollWindows)[which(names(SD_RollWindows) %chin% c("days","day","dy","dd","d"))] <- "day"
    if(is.list(Skew_RollWindows)) names(Skew_RollWindows)[which(names(Skew_RollWindows) %chin% c("days","day","dy","dd","d"))] <- "day"
    if(is.list(Kurt_RollWindows)) names(Kurt_RollWindows)[which(names(Kurt_RollWindows) %chin% c("days","day","dy","dd","d"))] <- "day"
    if(is.list(Quantile_RollWindows)) names(Quantile_RollWindows)[which(names(Quantile_RollWindows) %chin% c("days","day","dy","dd","d"))] <- "day"
  }
  if(any(c("weeks","week","weaks","weak","wk","wkly","wks") %chin% tolower(TimeGroups))) {
    TimeGroupPlaceHolder <- c(TimeGroupPlaceHolder, "weeks")
    if(is.list(Lags)) names(Lags)[which(names(Lags) %chin% c("weeks","week","weaks","weak","wk","wkly","wks"))] <- "weeks"
    if(is.list(MA_RollWindows)) names(MA_RollWindows)[which(names(MA_RollWindows) %chin% c("weeks","week","weaks","weak","wk","wkly","wks"))] <- "weeks"
    if(is.list(SD_RollWindows)) names(SD_RollWindows)[which(names(SD_RollWindows) %chin% c("weeks","week","weaks","weak","wk","wkly","wks"))] <- "weeks"
    if(is.list(Skew_RollWindows)) names(Skew_RollWindows)[which(names(Skew_RollWindows) %chin% c("weeks","week","weaks","weak","wk","wkly","wks"))] <- "weeks"
    if(is.list(Kurt_RollWindows)) names(Kurt_RollWindows)[which(names(Kurt_RollWindows) %chin% c("weeks","week","weaks","weak","wk","wkly","wks"))] <- "weeks"
    if(is.list(Quantile_RollWindows)) names(Quantile_RollWindows)[which(names(Quantile_RollWindows) %chin% c("weeks","week","weaks","weak","wk","wkly","wks"))] <- "weeks"
  }
  if(any(c("months","month","mth","mnth","monthly","mnthly") %chin% tolower(TimeGroups))) {
    TimeGroupPlaceHolder <- c(TimeGroupPlaceHolder, "months")
    if(is.list(Lags)) names(Lags)[which(names(Lags) %chin% c("months","month","mth","mnth","monthly","mnthly"))] <- "months"
    if(is.list(MA_RollWindows)) names(MA_RollWindows)[which(names(MA_RollWindows) %chin% c("months","month","mth","mnth","monthly","mnthly"))] <- "months"
    if(is.list(SD_RollWindows)) names(SD_RollWindows)[which(names(SD_RollWindows) %chin% c("months","month","mth","mnth","monthly","mnthly"))] <- "months"
    if(is.list(Skew_RollWindows)) names(Skew_RollWindows)[which(names(Skew_RollWindows) %chin% c("months","month","mth","mnth","monthly","mnthly"))] <- "months"
    if(is.list(Kurt_RollWindows)) names(Kurt_RollWindows)[which(names(Kurt_RollWindows) %chin% c("months","month","mth","mnth","monthly","mnthly"))] <- "months"
    if(is.list(Quantile_RollWindows)) names(Quantile_RollWindows)[which(names(Quantile_RollWindows) %chin% c("months","month","mth","mnth","monthly","mnthly"))] <- "months"
  }
  if(any(c("quarter","quarters","qarter","quarterly","q","qtly") %chin% tolower(TimeGroups))) {
    TimeGroupPlaceHolder <- c(TimeGroupPlaceHolder, "quarter")
    if(is.list(Lags)) names(Lags)[which(names(Lags) %chin% c("quarter","qarter","quarterly","q","qtly"))] <- "quarter"
    if(is.list(MA_RollWindows)) names(MA_RollWindows)[which(names(MA_RollWindows) %chin% c("quarter","qarter","quarterly","q","qtly"))] <- "quarter"
    if(is.list(SD_RollWindows)) names(SD_RollWindows)[which(names(SD_RollWindows) %chin% c("quarter","qarter","quarterly","q","qtly"))] <- "quarter"
    if(is.list(Skew_RollWindows)) names(Skew_RollWindows)[which(names(Skew_RollWindows) %chin% c("quarter","qarter","quarterly","q","qtly"))] <- "quarter"
    if(is.list(Kurt_RollWindows)) names(Kurt_RollWindows)[which(names(Kurt_RollWindows) %chin% c("quarter","qarter","quarterly","q","qtly"))] <- "quarter"
    if(is.list(Quantile_RollWindows)) names(Quantile_RollWindows)[which(names(Quantile_RollWindows) %chin% c("quarter","qarter","quarterly","q","qtly"))] <- "quarter"
  }
  if(any(c("year","years","annual","yearly","annually","ann","yr","yrly") %chin% tolower(TimeGroups))) {
    TimeGroupPlaceHolder <- c(TimeGroupPlaceHolder, "year")
    if(is.list(Lags)) names(Lags)[which(names(Lags) %chin% c("year","annual","yearly","annually","ann","yr","yrly"))] <- "year"
    if(is.list(MA_RollWindows)) names(MA_RollWindows)[which(names(MA_RollWindows) %chin% c("year","annual","yearly","annually","ann","yr","yrly"))] <- "year"
    if(is.list(SD_RollWindows)) names(SD_RollWindows)[which(names(SD_RollWindows) %chin% c("year","annual","yearly","annually","ann","yr","yrly"))] <- "year"
    if(is.list(Skew_RollWindows)) names(Skew_RollWindows)[which(names(Skew_RollWindows) %chin% c("year","annual","yearly","annually","ann","yr","yrly"))] <- "year"
    if(is.list(Kurt_RollWindows)) names(Kurt_RollWindows)[which(names(Kurt_RollWindows) %chin% c("year","annual","yearly","annually","ann","yr","yrly"))] <- "year"
    if(is.list(Quantile_RollWindows)) names(Quantile_RollWindows)[which(names(Quantile_RollWindows) %chin% c("year","annual","yearly","annually","ann","yr","yrly"))] <- "year"
  }
  TimeGroups <- TimeGroupPlaceHolder
  if(is.null(TimeUnitAgg)) TimeUnitAgg <- TimeGroups[1L]
  #The correct TimeGroups are: c("hour", "day", "weeks", "months", "quarter", "year", "1min", "5min", "10min", "15min", "30min", "45min")

  # Ensure date column is proper ----
  if(Debug) print("Data Wrangling: Convert DateColumnName to Date or POSIXct----")
  if(!(tolower(TimeUnit) %chin% c("1min","5min","10min","15min","30min","hour"))) {
    if(is.character(data[[eval(DateColumn)]])) {
      x <- data[1,get(DateColumn)]
      x1 <- lubridate::guess_formats(x, orders = c("mdY", "BdY", "Bdy", "bdY", "bdy", "mdy", "dby", "Ymd", "Ydm"))
      data.table::set(data, j = eval(DateColumn), value = as.Date(data[[eval(DateColumn)]], tryFormats = x1))
    }
  } else {
    data.table::set(data, j = eval(DateColumn), value = as.POSIXct(data[[eval(DateColumn)]]))
  }

  # Debugging----
  if(Debug) print("AutoLagRollStats: No Categoricals")

  # No Categoricals----
  if(is.null(IndependentGroups) && is.null(HierarchyGroups)) {

    # Initialize Counter----
    Counter <- 0L

    # Loop through various time aggs----
    for(timeaggs in TimeGroups) {

      # Increment Counter----
      Counter <- Counter + 1L

      # Copy data----
      tempData <- data.table::copy(data)

      # Check time scale----
      if(Counter > 1) {

        # Floor Date column to timeagg level----
        data.table::set(tempData, j = eval(DateColumn), value = lubridate::floor_date(x = tempData[[eval(DateColumn)]], unit = timeaggs))

        # Agg by date column----
        tempData <- tempData[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c(eval(Targets)), by = c(eval(DateColumn))]

        # Build features----
        tempData <- DT_GDL_Feature_Engineering(
          tempData,
          lags            = if(is.list(Lags))                 Lags[[timeaggs]]                 else Lags,
          periods         = if(is.list(MA_RollWindows))       MA_RollWindows[[timeaggs]]       else MA_RollWindows,
          SDperiods       = if(is.list(SD_RollWindows))       SD_RollWindows[[timeaggs]]       else SD_RollWindows,
          Skewperiods     = if(is.list(Skew_RollWindows))     Skew_RollWindows[[timeaggs]]     else Skew_RollWindows,
          Kurtperiods     = if(is.list(Kurt_RollWindows))     Kurt_RollWindows[[timeaggs]]     else Kurt_RollWindows,
          Quantileperiods = if(is.list(Quantile_RollWindows)) Quantile_RollWindows[[timeaggs]] else Quantile_RollWindows,
          statsFUNs       = RollFunctions,
          targets         = Targets,
          groupingVars    = NULL,
          sortDateName    = DateColumn,
          timeDiffTarget  = NULL,
          timeAgg         = timeaggs,
          WindowingLag    = RollOnLag1,
          ShortName       = ShortName,
          Type            = Type,
          SimpleImpute    = SimpleImpute)

      } else {

        # Build features----
        data.table::setkeyv(data <- DT_GDL_Feature_Engineering(
          data,
          lags            = if(is.list(Lags))                 Lags[[timeaggs]]                 else Lags,
          periods         = if(is.list(MA_RollWindows))       MA_RollWindows[[timeaggs]]       else MA_RollWindows,
          SDperiods       = if(is.list(SD_RollWindows))       SD_RollWindows[[timeaggs]]       else SD_RollWindows,
          Skewperiods     = if(is.list(Skew_RollWindows))     Skew_RollWindows[[timeaggs]]     else Skew_RollWindows,
          Kurtperiods     = if(is.list(Kurt_RollWindows))     Kurt_RollWindows[[timeaggs]]     else Kurt_RollWindows,
          Quantileperiods = if(is.list(Quantile_RollWindows)) Quantile_RollWindows[[timeaggs]] else Quantile_RollWindows,
          statsFUNs       = RollFunctions,
          targets         = Targets,
          groupingVars    = NULL,
          sortDateName    = DateColumn,
          timeDiffTarget  = NULL,
          timeAgg         = timeaggs,
          WindowingLag    = RollOnLag1,
          ShortName       = ShortName,
          Type            = Type,
          SimpleImpute    = SimpleImpute), DateColumn)
      }

      # Check if timeaggs is same of TimeUnit----
      if(Counter > 1L) {
        data.table::setkeyv(data[, TEMPDATE := lubridate::floor_date(get(DateColumn), unit = eval(timeaggs))], "TEMPDATE")
        data[tempData, (setdiff(names(tempData), names(data))) := mget(paste0("i.", setdiff(names(tempData), names(data))))]
        data.table::set(data, j = "TEMPDATE", value = NULL)
      }
    }
  }

  # Debugging----
  if(Debug) print("AutoLagRollStats: Indep + Hierach")

  # Hierarchy Categoricals----
  if(!is.null(HierarchyGroups)) {

    # Categorical Names Fully Interacted----
    Categoricals <- FullFactorialCatFeatures(GroupVars = HierarchyGroups, BottomsUp = TRUE)

    # Categorical Names Fully Interacted (Check if there already)----
    for(cat in seq_len(length(Categoricals)-length(HierarchyGroups))) {
      if(!any(names(data) %chin% Categoricals[cat])) data[, eval(Categoricals[cat]) := do.call(paste, c(.SD, sep = " ")), .SDcols = c(unlist(data.table::tstrsplit(Categoricals[cat], "_")))]
    }

    # Loop through each feature interaction
    Counter <- 0L
    for(Fact in Categoricals) {

      # Loop through all TimeGroups----
      for(timeaggs in TimeGroups) {

        # Counter incrementing
        Counter <- Counter + 1L

        # Check if timeaggs is same of TimeUnitAgg ----
        if(Counter > 1L) {

          # Aggregate tempData and tempRegs to correct dimensional level----
          tempData <- data[, .SD, .SDcols = c(eval(Targets), eval(DateColumn), eval(Fact))]

          # Agg by date column ----
          if(timeaggs != "raw") {
            tempData[, eval(DateColumn) := lubridate::floor_date(x = get(DateColumn), unit = timeaggs)]
            tempData <- tempData[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c(eval(Targets)), by = c(eval(DateColumn), eval(Fact))]
          }

          # Build GDL Features----
          data.table::setkeyv(tempData <- DT_GDL_Feature_Engineering(
            tempData,
            lags            = if(is.list(Lags))                 Lags[[timeaggs]]                 else Lags,
            periods         = if(is.list(MA_RollWindows))       MA_RollWindows[[timeaggs]]       else MA_RollWindows,
            SDperiods       = if(is.list(SD_RollWindows))       SD_RollWindows[[timeaggs]]       else SD_RollWindows,
            Skewperiods     = if(is.list(Skew_RollWindows))     Skew_RollWindows[[timeaggs]]     else Skew_RollWindows,
            Kurtperiods     = if(is.list(Kurt_RollWindows))     Kurt_RollWindows[[timeaggs]]     else Kurt_RollWindows,
            Quantileperiods = if(is.list(Quantile_RollWindows)) Quantile_RollWindows[[timeaggs]] else Quantile_RollWindows,
            statsFUNs       = RollFunctions,
            targets         = Targets,
            groupingVars    = Fact,
            sortDateName    = DateColumn,
            timeDiffTarget  = NULL,
            timeAgg         = timeaggs,
            WindowingLag    = RollOnLag1,
            ShortName       = ShortName,
            Type            = Type,
            SimpleImpute    = SimpleImpute), c(Fact, DateColumn))

        } else {

          # Build GDL Features----
          data <- DT_GDL_Feature_Engineering(
            data,
            lags            = if(is.list(Lags))                 Lags[[timeaggs]]                 else Lags,
            periods         = if(is.list(MA_RollWindows))       MA_RollWindows[[timeaggs]]       else MA_RollWindows,
            SDperiods       = if(is.list(SD_RollWindows))       SD_RollWindows[[timeaggs]]       else SD_RollWindows,
            Skewperiods     = if(is.list(Skew_RollWindows))     Skew_RollWindows[[timeaggs]]     else Skew_RollWindows,
            Kurtperiods     = if(is.list(Kurt_RollWindows))     Kurt_RollWindows[[timeaggs]]     else Kurt_RollWindows,
            Quantileperiods = if(is.list(Quantile_RollWindows)) Quantile_RollWindows[[timeaggs]] else Quantile_RollWindows,
            statsFUNs       = RollFunctions,
            targets         = Targets,
            groupingVars    = Fact,
            sortDateName    = DateColumn,
            timeDiffTarget  = NULL,
            timeAgg         = timeaggs,
            WindowingLag    = RollOnLag1,
            ShortName       = ShortName,
            Type            = Type,
            SimpleImpute    = SimpleImpute)
        }

        # Check if timeaggs is same of TimeUnit----
        if(Counter > 1L) {
          data.table::setkeyv(data[, TEMPDATE := lubridate::floor_date(get(DateColumn), unit = eval(timeaggs))], c(Fact,"TEMPDATE"))
          data[tempData, (setdiff(names(tempData), names(data))) := mget(paste0("i.", setdiff(names(tempData), names(data))))]
          data.table::set(data, j = "TEMPDATE", value = NULL)
        }
      }
    }
  }

  # Debugging----
  if(Debug) print("AutoLagRollStats: Indep")

  # Single categoricals at a time AND no hierarchical: if there are hierarchical the single cats will be handled above----
  if(!is.null(IndependentGroups) && is.null(HierarchyGroups)) {

    # Loop through IndependentGroups----
    Counter <- 0L
    # Fact = IndependentGroups[1]
    # timeaggs = TimeGroups[1]
    for(Fact in IndependentGroups) {

      # Loop through all TimeGroups----
      for(timeaggs in TimeGroups) {

        # Counter incrementing
        Counter <- Counter + 1L

        # Copy data----
        tempData <- data.table::copy(data)

        # Check if timeaggs is same of TimeUnit ----
        if(Counter > 1L) {

          # Floor Date column to timeagg level ----
          tempData[, eval(DateColumn) := lubridate::floor_date(x = get(DateColumn), unit = timeaggs)]

          # Agg by date column----
          tempData <- tempData[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c(eval(Targets)), by = c(eval(DateColumn),eval(Fact))]

          # Build GDL Features----
          data.table::setkeyv(tempData <- DT_GDL_Feature_Engineering(
            tempData,
            lags            = if(is.list(Lags))                 Lags[[timeaggs]]                 else Lags,
            periods         = if(is.list(MA_RollWindows))       MA_RollWindows[[timeaggs]]       else MA_RollWindows,
            SDperiods       = if(is.list(SD_RollWindows))       SD_RollWindows[[timeaggs]]       else SD_RollWindows,
            Skewperiods     = if(is.list(Skew_RollWindows))     Skew_RollWindows[[timeaggs]]     else Skew_RollWindows,
            Kurtperiods     = if(is.list(Kurt_RollWindows))     Kurt_RollWindows[[timeaggs]]     else Kurt_RollWindows,
            Quantileperiods = if(is.list(Quantile_RollWindows)) Quantile_RollWindows[[timeaggs]] else Quantile_RollWindows,
            statsFUNs       = RollFunctions,
            targets         = Targets,
            groupingVars    = Fact,
            sortDateName    = DateColumn,
            timeDiffTarget  = NULL,
            timeAgg         = timeaggs,
            ShortName       = ShortName,
            WindowingLag    = RollOnLag1,
            Type            = Type,
            SimpleImpute    = SimpleImpute), c(Fact, DateColumn))

        } else {

          # Set up for binary search instead of vector scan
          data.table::setkeyv(x = data, cols = c(eval(Fact),eval(DateColumn)))

          # Build GDL Features
          data <- DT_GDL_Feature_Engineering(
            data,
            lags            = if(is.list(Lags))                 Lags[[timeaggs]]                 else Lags,
            periods         = if(is.list(MA_RollWindows))       MA_RollWindows[[timeaggs]]       else MA_RollWindows,
            SDperiods       = if(is.list(SD_RollWindows))       SD_RollWindows[[timeaggs]]       else SD_RollWindows,
            Skewperiods     = if(is.list(Skew_RollWindows))     Skew_RollWindows[[timeaggs]]     else Skew_RollWindows,
            Kurtperiods     = if(is.list(Kurt_RollWindows))     Kurt_RollWindows[[timeaggs]]     else Kurt_RollWindows,
            Quantileperiods = if(is.list(Quantile_RollWindows)) Quantile_RollWindows[[timeaggs]] else Quantile_RollWindows,
            statsFUNs       = RollFunctions,
            targets         = Targets,
            groupingVars    = Fact,
            sortDateName    = DateColumn,
            timeDiffTarget  = TimeBetween,
            timeAgg         = timeaggs,
            WindowingLag    = RollOnLag1,
            Type            = Type,
            ShortName       = ShortName,
            SimpleImpute    = SimpleImpute)

          # lags            = if(is.list(Lags))                 Lags[[timeaggs]]                 else Lags
          # periods         = if(is.list(MA_RollWindows))       MA_RollWindows[[timeaggs]]       else MA_RollWindows
          # SDperiods       = if(is.list(SD_RollWindows))       SD_RollWindows[[timeaggs]]       else SD_RollWindows
          # Skewperiods     = if(is.list(Skew_RollWindows))     Skew_RollWindows[[timeaggs]]     else Skew_RollWindows
          # Kurtperiods     = if(is.list(Kurt_RollWindows))     Kurt_RollWindows[[timeaggs]]     else Kurt_RollWindows
          # Quantileperiods = if(is.list(Quantile_RollWindows)) Quantile_RollWindows[[timeaggs]] else Quantile_RollWindows
          # statsFUNs       = RollFunctions
          # targets         = Targets
          # groupingVars    = Fact
          # sortDateName    = DateColumn
          # timeDiffTarget  = TimeBetween
          # timeAgg         = timeaggs
          # WindowingLag    = RollOnLag1
          # Type            = Type
          # SimpleImpute    = SimpleImpute
        }

        # Check if timeaggs is same of TimeUnit ----
        if(Counter > 1L) {
          data.table::setkeyv(data[, TEMPDATE := lubridate::floor_date(get(DateColumn), unit = eval(timeaggs))], c(Fact, "TEMPDATE"))
          data[tempData, (setdiff(names(tempData), names(data))) := mget(paste0("i.", setdiff(names(tempData), names(data))))]
          data.table::set(data, j = "TEMPDATE", value = NULL)
        }
      }
    }
  }

  # Simple impute missed ----
  if(SimpleImpute) {
    for(miss in seq_along(data)) {
      data.table::set(data, i = which(is.na(data[[miss]])), j = miss, value = -1)
    }
  }

  # Return data ----
  if("TEMPDATE" %chin% names(data)) data.table::set(data, j = "TEMPDATE", value = NULL)
  return(data)
}

#' @title AutoLagRollStatsScoring
#'
#' @description AutoLagRollStatsScoring Builds lags and a large variety of rolling statistics with options to generate them for hierarchical categorical interactions.
#'
#' @author Adrian Antico
#' @family Feature Engineering
#'
#' @param data A data.table you want to run the function on
#' @param RowNumsID The name of your column used to id the records so you can specify which rows to keep
#' @param RowNumsKeep The RowNumsID numbers that you want to keep
#' @param Targets A character vector of the column names for the reference column in which you will build your lags and rolling stats
#' @param IndependentGroups Only supply if you do not want HierarchyGroups. A vector of categorical column names that you want to have run independently of each other. This will mean that no interaction will be done.
#' @param HierarchyGroups A vector of categorical column names that you want to have generate all lags and rolling stats done for the individual columns and their full set of interactions.
#' @param DateColumn The column name of your date column used to sort events over time
#' @param TimeGroups A vector of TimeUnits indicators to specify any time-aggregated GDL features you want to have returned. E.g. c("hour", "day","week","month","quarter","year"). STILL NEED TO ADD these '1min', '5min', '10min', '15min', '30min', '45min'
#' @param TimeBetween Specify a desired name for features created for time between events. Set to NULL if you don't want time between events features created.
#' @param TimeUnit List the time aggregation level for the time between events features, such as "hour", "day", "weeks", "months", "quarter", or "year"
#' @param TimeUnitAgg List the time aggregation of your data that you want to use as a base time unit for your features. E.g. "day",
#' @param Lags A numeric vector of the specific lags you want to have generated. You must include 1 if WindowingLag = 1.
#' @param MA_RollWindows A numeric vector of the specific rolling statistics window sizes you want to utilize in the calculations.
#' @param SD_RollWindows  A numeric vector of Standard Deviation rolling statistics window sizes you want to utilize in the calculations.
#' @param Skew_RollWindows  A numeric vector of Skewness rolling statistics window sizes you want to utilize in the calculations.
#' @param Kurt_RollWindows  A numeric vector of Kurtosis rolling statistics window sizes you want to utilize in the calculations.
#' @param Quantile_RollWindows A numeric vector of Quantile rolling statistics window sizes you want to utilize in the calculations.
#' @param Quantiles_Selected Select from the following c("q5", "q10", "q15", "q20", "q25", "q30", "q35", "q40", "q45", "q50", "q55", "q60"," q65", "q70", "q75", "q80", "q85", "q90", "q95")
#' @param RollOnLag1 Set to FALSE to build rolling stats off of target columns directly or set to TRUE to build the rolling stats off of the lag-1 target
#' @param Type List either "Lag" if you want features built on historical values or "Lead" if you want features built on future values
#' @param SimpleImpute Set to TRUE for factor level imputation of "0" and numeric imputation of -1
#' @param ShortName Default TRUE. If FALSE, Group Variable names will be added to the rolling stat and lag names. If you plan on have multiple versions of lags and rollings stats by different group variables then set this to FALSE.
#' @param Debug Set to TRUE to get a print out of which step you are on
#' @return data.table of original data plus created lags, rolling stats, and time between event lags and rolling stats
#' @examples
#' \donttest{
#' # Create fake Panel Data----
#' Count <- 1L
#' for(Level in LETTERS) {
#'   datatemp <- AutoQuant::FakeDataGenerator(
#'     Correlation = 0.75,
#'     N = 25000L,
#'     ID = 0L,
#'     ZIP = 0L,
#'     FactorCount = 0L,
#'     AddDate = TRUE,
#'     Classification = FALSE,
#'     MultiClass = FALSE)
#'   datatemp[, Factor1 := eval(Level)]
#'   if(Count == 1L) {
#'     data1 <- data.table::copy(datatemp)
#'   } else {
#'     data1 <- data.table::rbindlist(
#'       list(data1, data.table::copy(datatemp)))
#'   }
#'   Count <- Count + 1L
#' }
#'
#' # Create ID columns to know which records to score
#' data1[, ID := .N:1L, by = "Factor1"]
#' data.table::set(data1, i = which(data1[["ID"]] == 2L), j = "ID", value = 1L)
#'
#' # Score records
#' data1 <- AutoQuant::AutoLagRollStatsScoring(
#'
#'   # Data
#'   data                 = data1,
#'   RowNumsID            = "ID",
#'   RowNumsKeep          = 1,
#'   DateColumn           = "DateTime",
#'   Targets              = "Adrian",
#'   HierarchyGroups      = NULL,
#'   IndependentGroups    = c("Factor1"),
#'
#'   # Services
#'   TimeBetween          = NULL,
#'   TimeGroups           = c("days","weeks","months","quarters"),
#'   TimeUnit             = "day",
#'   TimeUnitAgg          = "day",
#'   RollOnLag1           = TRUE,
#'   Type                 = "Lag",
#'   SimpleImpute         = TRUE,
#'
#'   # Calculated Columns
#'   Lags                 = list("days" = c(seq(1,5,1)), "weeks" = c(seq(1,3,1)), "months" = c(seq(1,2,1)), "quarters" = c(seq(1,2,1))),
#'   MA_RollWindows       = list("days" = c(seq(1,5,1)), "weeks" = c(seq(1,3,1)), "months" = c(seq(1,2,1)), "quarters" = c(seq(1,2,1))),
#'   SD_RollWindows       = list("days" = c(seq(1,5,1)), "weeks" = c(seq(1,3,1)), "months" = c(seq(1,2,1)), "quarters" = c(seq(1,2,1))),
#'   Skew_RollWindows     = list("days" = c(seq(1,5,1)), "weeks" = c(seq(1,3,1)), "months" = c(seq(1,2,1)), "quarters" = c(seq(1,2,1))),
#'   Kurt_RollWindows     = list("days" = c(seq(1,5,1)), "weeks" = c(seq(1,3,1)), "months" = c(seq(1,2,1)), "quarters" = c(seq(1,2,1))),
#'   Quantile_RollWindows = list("days" = c(seq(1,5,1)), "weeks" = c(seq(1,3,1)), "months" = c(seq(1,2,1)), "quarters" = c(seq(1,2,1))),
#'   Quantiles_Selected   = c('q5','q50'),
#'   Debug                 = FALSE)
#' }
#' @export
AutoLagRollStatsScoring <- function(data,
                                    RowNumsID            = "temp",
                                    RowNumsKeep          = 1,
                                    Targets              = NULL,
                                    HierarchyGroups      = NULL,
                                    IndependentGroups    = NULL,
                                    DateColumn           = NULL,
                                    TimeUnit             = "day",
                                    TimeUnitAgg          = "day",
                                    TimeGroups           = "day",
                                    TimeBetween          = NULL,
                                    RollOnLag1           = 1,
                                    Type                 = "Lag",
                                    SimpleImpute         = TRUE,
                                    Lags                 = NULL,
                                    MA_RollWindows       = NULL,
                                    SD_RollWindows       = NULL,
                                    Skew_RollWindows     = NULL,
                                    Kurt_RollWindows     = NULL,
                                    Quantile_RollWindows = NULL,
                                    Quantiles_Selected   = NULL,
                                    ShortName            = TRUE,
                                    Debug                = FALSE) {

  Nam <- names(data.table::copy(data))

  # Define args ----
  RollFunctions <- c()
  if(!is.null(MA_RollWindows)) RollFunctions <- c(RollFunctions,"mean")
  if(!is.null(SD_RollWindows)) RollFunctions <- c(RollFunctions,"sd")
  if(!is.null(Skew_RollWindows)) RollFunctions <- c(RollFunctions,"skew")
  if(!is.null(Kurt_RollWindows)) RollFunctions <- c(RollFunctions,"kurt")
  if(!is.null(Quantile_RollWindows)) RollFunctions <- c(RollFunctions,Quantiles_Selected)
  if(is.null(TimeBetween)) TimeBetween <- NULL else TimeBetween <- "TimeBetweenRecords" # Cant remember why I put the NULL there
  if(RollOnLag1) RollOnLag1 <- 1L else RollOnLag1 <- 0L
  TimeGroupPlaceHolder <- c()
  if("raw" %chin% tolower(TimeGroups)) TimeGroupPlaceHolder <- c(TimeGroupPlaceHolder, "raw")
  if(any(c("hours","hour","hr","hrs","hourly") %chin% tolower(TimeGroups))) {
    TimeGroupPlaceHolder <- c(TimeGroupPlaceHolder, "hour")
    if(is.list(Lags)) names(Lags)[which(names(Lags) %chin% c("hours","hour","hr","hrs","hourly"))] <- "hour"
    if(is.list(MA_RollWindows)) names(MA_RollWindows)[which(names(MA_RollWindows) %chin% c("hours","hour","hr","hrs","hourly"))] <- "hour"
    if(is.list(SD_RollWindows)) names(SD_RollWindows)[which(names(SD_RollWindows) %chin% c("hours","hour","hr","hrs","hourly"))] <- "hour"
    if(is.list(Skew_RollWindows)) names(Skew_RollWindows)[which(names(Skew_RollWindows) %chin% c("hours","hour","hr","hrs","hourly"))] <- "hour"
    if(is.list(Kurt_RollWindows)) names(Kurt_RollWindows)[which(names(Kurt_RollWindows) %chin% c("hours","hour","hr","hrs","hourly"))] <- "hour"
    if(is.list(Quantile_RollWindows)) names(Quantile_RollWindows)[which(names(Quantile_RollWindows) %chin% c("hours","hour","hr","hrs","hourly"))] <- "hour"
  }
  if(any(c("days","day","dy","dd","d") %chin% tolower(TimeGroups))) {
    TimeGroupPlaceHolder <- c(TimeGroupPlaceHolder, "day")
    if(is.list(Lags)) names(Lags)[which(names(Lags) %chin% c("days","day","dy","dd","d"))] <- "day"
    if(is.list(MA_RollWindows)) names(MA_RollWindows)[which(names(MA_RollWindows) %chin% c("days","day","dy","dd","d"))] <- "day"
    if(is.list(SD_RollWindows)) names(SD_RollWindows)[which(names(SD_RollWindows) %chin% c("days","day","dy","dd","d"))] <- "day"
    if(is.list(Skew_RollWindows)) names(Skew_RollWindows)[which(names(Skew_RollWindows) %chin% c("days","day","dy","dd","d"))] <- "day"
    if(is.list(Kurt_RollWindows)) names(Kurt_RollWindows)[which(names(Kurt_RollWindows) %chin% c("days","day","dy","dd","d"))] <- "day"
    if(is.list(Quantile_RollWindows)) names(Quantile_RollWindows)[which(names(Quantile_RollWindows) %chin% c("days","day","dy","dd","d"))] <- "day"
  }
  if(any(c("weeks","week","weaks","weak","wk","wkly","wks") %chin% tolower(TimeGroups))) {
    TimeGroupPlaceHolder <- c(TimeGroupPlaceHolder, "weeks")
    if(is.list(Lags)) names(Lags)[which(names(Lags) %chin% c("weeks","week","weaks","weak","wk","wkly","wks"))] <- "weeks"
    if(is.list(MA_RollWindows)) names(MA_RollWindows)[which(names(MA_RollWindows) %chin% c("weeks","week","weaks","weak","wk","wkly","wks"))] <- "weeks"
    if(is.list(SD_RollWindows)) names(SD_RollWindows)[which(names(SD_RollWindows) %chin% c("weeks","week","weaks","weak","wk","wkly","wks"))] <- "weeks"
    if(is.list(Skew_RollWindows)) names(Skew_RollWindows)[which(names(Skew_RollWindows) %chin% c("weeks","week","weaks","weak","wk","wkly","wks"))] <- "weeks"
    if(is.list(Kurt_RollWindows)) names(Kurt_RollWindows)[which(names(Kurt_RollWindows) %chin% c("weeks","week","weaks","weak","wk","wkly","wks"))] <- "weeks"
    if(is.list(Quantile_RollWindows)) names(Quantile_RollWindows)[which(names(Quantile_RollWindows) %chin% c("weeks","week","weaks","weak","wk","wkly","wks"))] <- "weeks"
  }
  if(any(c("months","month","mth","mnth","monthly","mnthly") %chin% tolower(TimeGroups))) {
    TimeGroupPlaceHolder <- c(TimeGroupPlaceHolder, "months")
    if(is.list(Lags)) names(Lags)[which(names(Lags) %chin% c("months","month","mth","mnth","monthly","mnthly"))] <- "months"
    if(is.list(MA_RollWindows)) names(MA_RollWindows)[which(names(MA_RollWindows) %chin% c("months","month","mth","mnth","monthly","mnthly"))] <- "months"
    if(is.list(SD_RollWindows)) names(SD_RollWindows)[which(names(SD_RollWindows) %chin% c("months","month","mth","mnth","monthly","mnthly"))] <- "months"
    if(is.list(Skew_RollWindows)) names(Skew_RollWindows)[which(names(Skew_RollWindows) %chin% c("months","month","mth","mnth","monthly","mnthly"))] <- "months"
    if(is.list(Kurt_RollWindows)) names(Kurt_RollWindows)[which(names(Kurt_RollWindows) %chin% c("months","month","mth","mnth","monthly","mnthly"))] <- "months"
    if(is.list(Quantile_RollWindows)) names(Quantile_RollWindows)[which(names(Quantile_RollWindows) %chin% c("months","month","mth","mnth","monthly","mnthly"))] <- "months"
  }
  if(any(c("quarter","quarters","qarter","quarterly","q","qtly") %chin% tolower(TimeGroups))) {
    TimeGroupPlaceHolder <- c(TimeGroupPlaceHolder, "quarter")
    if(is.list(Lags)) names(Lags)[which(names(Lags) %chin% c("quarter","qarter","quarterly","q","qtly"))] <- "quarter"
    if(is.list(MA_RollWindows)) names(MA_RollWindows)[which(names(MA_RollWindows) %chin% c("quarter","qarter","quarterly","q","qtly"))] <- "quarter"
    if(is.list(SD_RollWindows)) names(SD_RollWindows)[which(names(SD_RollWindows) %chin% c("quarter","qarter","quarterly","q","qtly"))] <- "quarter"
    if(is.list(Skew_RollWindows)) names(Skew_RollWindows)[which(names(Skew_RollWindows) %chin% c("quarter","qarter","quarterly","q","qtly"))] <- "quarter"
    if(is.list(Kurt_RollWindows)) names(Kurt_RollWindows)[which(names(Kurt_RollWindows) %chin% c("quarter","qarter","quarterly","q","qtly"))] <- "quarter"
    if(is.list(Quantile_RollWindows)) names(Quantile_RollWindows)[which(names(Quantile_RollWindows) %chin% c("quarter","qarter","quarterly","q","qtly"))] <- "quarter"
  }
  if(any(c("year","years","annual","yearly","annually","ann","yr","yrly") %chin% tolower(TimeGroups))) {
    TimeGroupPlaceHolder <- c(TimeGroupPlaceHolder, "year")
    if(is.list(Lags)) names(Lags)[which(names(Lags) %chin% c("year","annual","yearly","annually","ann","yr","yrly"))] <- "year"
    if(is.list(MA_RollWindows)) names(MA_RollWindows)[which(names(MA_RollWindows) %chin% c("year","annual","yearly","annually","ann","yr","yrly"))] <- "year"
    if(is.list(SD_RollWindows)) names(SD_RollWindows)[which(names(SD_RollWindows) %chin% c("year","annual","yearly","annually","ann","yr","yrly"))] <- "year"
    if(is.list(Skew_RollWindows)) names(Skew_RollWindows)[which(names(Skew_RollWindows) %chin% c("year","annual","yearly","annually","ann","yr","yrly"))] <- "year"
    if(is.list(Kurt_RollWindows)) names(Kurt_RollWindows)[which(names(Kurt_RollWindows) %chin% c("year","annual","yearly","annually","ann","yr","yrly"))] <- "year"
    if(is.list(Quantile_RollWindows)) names(Quantile_RollWindows)[which(names(Quantile_RollWindows) %chin% c("year","annual","yearly","annually","ann","yr","yrly"))] <- "year"
  }
  TimeGroups <- TimeGroupPlaceHolder
  if(is.null(TimeUnitAgg)) TimeUnitAgg <- TimeGroups[1L]
  #The correct TimeGroups are: c("hour", "day", "weeks", "months", "quarter", "year", "1min", "5min", "10min", "15min", "30min", "45min")

  # Ensure date column is proper ----
  if(Debug) print("Data Wrangling: Convert DateColumnName to Date or POSIXct----")
  if(!(tolower(TimeUnit) %chin% c("1min","5min","10min","15min","30min","hour"))) {
    if(is.character(data[[eval(DateColumn)]])) {
      x <- data[1,get(DateColumn)]
      x1 <- lubridate::guess_formats(x, orders = c("mdY", "BdY", "Bdy", "bdY", "bdy", "mdy", "dby", "Ymd", "Ydm"))
      data.table::set(data, j = eval(DateColumn), value = as.Date(data[[eval(DateColumn)]], tryFormats = x1))
    }
  } else {
    data.table::set(data, j = eval(DateColumn), value = as.POSIXct(data[[eval(DateColumn)]]))
  }

  # Debugging ----
  if(Debug) print("AutoLagRollStatsScoring: No Categoricals")

  # No Categoricals ----
  if(is.null(HierarchyGroups) && is.null(IndependentGroups)) {

    # Initialize counter ----
    Counter <- 0L

    # Loop through the time aggs ----
    for(timeaggs in TimeGroups) {

      # Increment----
      Counter <- Counter + 1L

      # Check if timeaggs is same of TimeUnitAgg ----
      if(Counter > 1L) {

        # Copy data ----
        tempData <- data.table::copy(data)
        data.table::setnames(tempData, eval(DateColumn), "TEMPDATE")

        # Floor Date column to timeagg level----
        if(tolower(timeaggs) != "raw") data.table::set(tempData, j = "TEMPDATE", value = lubridate::floor_date(x = tempData[["TEMPDATE"]], unit = timeaggs))

        # Ensure Targets is numeric - someimes comes in as list----
        for(tar in Targets) if(!is.numeric(tempData[[eval(tar)]])) data.table::set(tempData, j = eval(tar), value = as.numeric(tempData[[eval(tar)]]))

        # Dim and Time Aggregation----
        if(tolower(timeaggs) != "raw") {
          tempData <- tempData[, c(min(get(RowNumsID)), lapply(.SD, mean, na.rm = TRUE)), .SDcols = c(eval(Targets)), by = c("TEMPDATE")]
          data.table::setnames(tempData, c("V1"), c(RowNumsID))
        }

        # Build GDL Features ----
        tempData <- Partial_DT_GDL_Feature_Engineering2(
          tempData,
          lags            = if(is.list(Lags))                 Lags[[timeaggs]]                 else Lags,
          periods         = if(is.list(MA_RollWindows))       MA_RollWindows[[timeaggs]]       else MA_RollWindows,
          SDperiods       = if(is.list(SD_RollWindows))       SD_RollWindows[[timeaggs]]       else SD_RollWindows,
          Skewperiods     = if(is.list(Skew_RollWindows))     Skew_RollWindows[[timeaggs]]     else Skew_RollWindows,
          Kurtperiods     = if(is.list(Kurt_RollWindows))     Kurt_RollWindows[[timeaggs]]     else Kurt_RollWindows,
          Quantileperiods = if(is.list(Quantile_RollWindows)) Quantile_RollWindows[[timeaggs]] else Quantile_RollWindows,
          statsFUNs = RollFunctions,
          targets = Targets,
          groupingVars = NULL,
          sortDateName = "TEMPDATE",
          timeDiffTarget = NULL,
          timeAgg = timeaggs,
          WindowingLag = RollOnLag1,
          Type = Type,
          Timer = FALSE,
          SimpleImpute = SimpleImpute,
          AscRowByGroup = RowNumsID,
          RecordsKeep = RowNumsKeep,
          AscRowRemove = TRUE)

      } else {

        # Build GDL Features----
        KeepData <- Partial_DT_GDL_Feature_Engineering2(
          data,
          lags            = if(is.list(Lags))                 Lags[[timeaggs]]                 else Lags,
          periods         = if(is.list(MA_RollWindows))       MA_RollWindows[[timeaggs]]       else MA_RollWindows,
          SDperiods       = if(is.list(SD_RollWindows))       SD_RollWindows[[timeaggs]]       else SD_RollWindows,
          Skewperiods     = if(is.list(Skew_RollWindows))     Skew_RollWindows[[timeaggs]]     else Skew_RollWindows,
          Kurtperiods     = if(is.list(Kurt_RollWindows))     Kurt_RollWindows[[timeaggs]]     else Kurt_RollWindows,
          Quantileperiods = if(is.list(Quantile_RollWindows)) Quantile_RollWindows[[timeaggs]] else Quantile_RollWindows,
          statsFUNs = RollFunctions,
          targets = Targets,
          groupingVars = NULL,
          sortDateName = eval(DateColumn),
          timeDiffTarget = NULL,
          timeAgg = timeaggs,
          WindowingLag = RollOnLag1,
          Type = Type,
          Timer = FALSE,
          SimpleImpute = SimpleImpute,
          AscRowByGroup = RowNumsID,
          RecordsKeep = RowNumsKeep,
          AscRowRemove = TRUE)
      }

      # Combine data sets----
      if(Counter > 1L) {
        KeepData[, TEMPDATE := lubridate::floor_date(get(DateColumn), unit = eval(timeaggs))]
        data.table::set(tempData, j = setdiff(names(tempData), c(setdiff(names(tempData), Targets))), value = NULL)
        KeepData <- merge(
          x = KeepData,
          y = tempData,
          by = c("TEMPDATE"),
          all.x = TRUE)
        data.table::set(KeepData, j = "TEMPDATE", value = NULL)
      }
    }
  }

  # Debugging----
  if(Debug) print("AutoLagRollStatsScoring: Hierarchies")

  # Hierarchy Categoricals ----
  if(!is.null(HierarchyGroups)) {

    # Categorical Names Fully Interacted----
    Categoricals <- FullFactorialCatFeatures(GroupVars = HierarchyGroups, BottomsUp = TRUE)

    # Check if there already----
    for(cat in seq_len(length(Categoricals)-length(HierarchyGroups))) if(!any(names(data) %chin% Categoricals[cat])) data[, eval(Categoricals[cat]) := do.call(paste, c(.SD, sep = " ")), .SDcols = c(unlist(data.table::tstrsplit(Categoricals[cat], "_")))]

    # Loop through each feature interaction----
    Counter <- 0L
    for(Fact in Categoricals) {

      # Loop through the time aggs----
      for(timeaggs in TimeGroups) {

        # Increment----
        Counter <- Counter + 1L

        # Copy data----
        tempData <- data.table::copy(data)
        data.table::setnames(tempData, eval(DateColumn), "TEMPDATE")

        # Check if timeaggs is same of TimeUnitAgg----
        if(Counter > 1L) {

          # Floor Date column to timeagg level----
          if(tolower(timeaggs) != "raw") data.table::set(tempData, j = "TEMPDATE", value = lubridate::floor_date(x = tempData[["TEMPDATE"]], unit = timeaggs))

          # Ensure Targets is numeric - someimes comes in as list----
          for(tar in Targets) if(!is.numeric(tempData[[eval(tar)]])) data.table::set(tempData, j = eval(tar), value = as.numeric(tempData[[eval(tar)]]))

          # Dim and Time Aggregation----
          if(tolower(timeaggs) != "raw") {
            tempData <- tempData[, c(min(get(RowNumsID)), lapply(.SD, mean, na.rm = TRUE)), .SDcols = c(eval(Targets)), by = c("TEMPDATE",eval(Fact))]
            data.table::setnames(tempData, c("V1"), c(RowNumsID))
          }

          # Build features----
          tempData <- Partial_DT_GDL_Feature_Engineering2(
            data            = tempData,
            lags            = if(is.list(Lags))                 Lags[[timeaggs]]                 else Lags,
            periods         = if(is.list(MA_RollWindows))       MA_RollWindows[[timeaggs]]       else MA_RollWindows,
            SDperiods       = if(is.list(SD_RollWindows))       SD_RollWindows[[timeaggs]]       else SD_RollWindows,
            Skewperiods     = if(is.list(Skew_RollWindows))     Skew_RollWindows[[timeaggs]]     else Skew_RollWindows,
            Kurtperiods     = if(is.list(Kurt_RollWindows))     Kurt_RollWindows[[timeaggs]]     else Kurt_RollWindows,
            Quantileperiods = if(is.list(Quantile_RollWindows)) Quantile_RollWindows[[timeaggs]] else Quantile_RollWindows,
            statsFUNs       = RollFunctions,
            targets         = Targets,
            groupingVars    = Fact,
            sortDateName    = "TEMPDATE",
            timeDiffTarget  = NULL,
            timeAgg         = timeaggs,
            WindowingLag    = RollOnLag1,
            Type            = Type,
            Timer           = FALSE,
            SimpleImpute    = SimpleImpute,
            AscRowByGroup   = RowNumsID,
            RecordsKeep     = RowNumsKeep,
            AscRowRemove    = TRUE)

        } else {

          # Build features----
          KeepData <- Partial_DT_GDL_Feature_Engineering2(
            data            = tempData,
            lags            = if(is.list(Lags))                 Lags[[timeaggs]]                 else Lags,
            periods         = if(is.list(MA_RollWindows))       MA_RollWindows[[timeaggs]]       else MA_RollWindows,
            SDperiods       = if(is.list(SD_RollWindows))       SD_RollWindows[[timeaggs]]       else SD_RollWindows,
            Skewperiods     = if(is.list(Skew_RollWindows))     Skew_RollWindows[[timeaggs]]     else Skew_RollWindows,
            Kurtperiods     = if(is.list(Kurt_RollWindows))     Kurt_RollWindows[[timeaggs]]     else Kurt_RollWindows,
            Quantileperiods = if(is.list(Quantile_RollWindows)) Quantile_RollWindows[[timeaggs]] else Quantile_RollWindows,
            statsFUNs       = RollFunctions,
            targets         = Targets,
            groupingVars    = Fact,
            sortDateName    = "TEMPDATE",
            timeDiffTarget  = TimeBetween,
            timeAgg         = timeaggs,
            WindowingLag    = RollOnLag1,
            Type            = Type,
            Timer           = FALSE,
            SimpleImpute    = SimpleImpute,
            AscRowByGroup   = RowNumsID,
            RecordsKeep     = RowNumsKeep,
            AscRowRemove    = TRUE)

          # Update vals----
          data.table::set(KeepData, j = eval(DateColumn), value = KeepData[["TEMPDATE"]])
          data.table::setcolorder(KeepData, c(which(names(KeepData) == eval(DateColumn)), setdiff(seq_len(ncol(KeepData)), which(names(KeepData) == eval(DateColumn)))))
        }

        # Merge data----
        if(Counter > 1L) {

          # I need to match up date aggregation to join properly----
          if(tolower(timeaggs) != "raw") {
            KeepData <- merge(
              x = data.table::set(KeepData, j = "TEMPDATE", value = lubridate::floor_date(KeepData[[eval(DateColumn)]], unit = timeaggs)),
              y = data.table::set(tempData, j = c(setdiff(names(tempData),c(eval(Fact),"TEMPDATE",setdiff(names(tempData),names(KeepData))))), value = NULL),
              by = c(eval(Fact),"TEMPDATE"),
              all.x = TRUE)
          } else if(tolower(timeaggs) == "raw") {
            KeepData <- merge(
              x = data.table::set(KeepData, j = "TEMPDATE", value = KeepData[[eval(DateColumn)]]),
              y = data.table::set(tempData, j = c(setdiff(names(tempData),c(eval(Fact),"TEMPDATE",setdiff(names(tempData),names(KeepData))))), value = NULL),
              by = c(eval(Fact),"TEMPDATE"),
              all.x = TRUE)
          }
        }
      }
    }
    if("TEMPDATE" %chin% names(KeepData)) data.table::set(KeepData, j = "TEMPDATE", value = NULL)
  }

  # Debugging----
  if(Debug) print("AutoLagRollStatsScoring: Independent")

  # Single categoricals at a time----
  if(!is.null(IndependentGroups) && is.null(HierarchyGroups)) {

    # Initialize counter----
    Counter <- 0L

    # Loop through IndependentGroups----
    for(Fact in IndependentGroups) {

      # Loop through the time aggs----
      for(timeaggs in TimeGroups) {

        # Increment----
        Counter <- Counter + 1L

        # Check if timeaggs is same of TimeUnit----
        if(Counter > 1L) {

          # Copy data----
          tempData <- data.table::copy(data)

          # Floor Date column to timeagg level----
          if(Fact != IndependentGroups[1] || timeaggs != TimeGroups[1]) tempData[, TEMPDATE := lubridate::floor_date(x = get(DateColumn), unit = timeaggs)]

          # Ensure Targets is numeric - someimes comes in as list----
          for(tar in Targets) if(!is.numeric(tempData[[eval(tar)]])) data.table::set(tempData, j = eval(tar), value = as.numeric(tempData[[eval(tar)]]))

          # Dim and Time Aggregation----
          tempData <- tempData[, c(min(get(RowNumsID)), lapply(.SD, mean, na.rm = TRUE)), .SDcols = c(eval(Targets)), by = c("TEMPDATE",eval(Fact))]
          data.table::setnames(tempData, c("V1"), c(RowNumsID))

          # Build features ----
          data.table::setkeyv(tempData <- Partial_DT_GDL_Feature_Engineering2(
            data            = tempData,
            lags            = if(is.list(Lags)) Lags[[timeaggs]] else Lags,
            periods         = if(is.list(MA_RollWindows)) MA_RollWindows[[timeaggs]] else MA_RollWindows,
            SDperiods       = if(is.list(SD_RollWindows)) SD_RollWindows[[timeaggs]] else SD_RollWindows,
            Skewperiods     = if(is.list(Skew_RollWindows)) Skew_RollWindows[[timeaggs]] else Skew_RollWindows,
            Kurtperiods     = if(is.list(Kurt_RollWindows)) Kurt_RollWindows[[timeaggs]] else Kurt_RollWindows,
            Quantileperiods = if(is.list(Quantile_RollWindows)) Quantile_RollWindows[[timeaggs]] else Quantile_RollWindows,
            statsFUNs       = RollFunctions,
            targets         = Targets,
            groupingVars    = Fact,
            sortDateName    = "TEMPDATE",
            timeDiffTarget  = NULL,
            timeAgg         = timeaggs,
            WindowingLag    = RollOnLag1,
            Type            = Type,
            Timer           = FALSE,
            SimpleImpute    = SimpleImpute,
            AscRowByGroup   = RowNumsID,
            RecordsKeep     = RowNumsKeep,
            AscRowRemove    = TRUE), c(Fact, "TEMPDATE"))

        } else {

          # Build features----
          KeepData <- Partial_DT_GDL_Feature_Engineering2(
            data            = data,
            lags            = if(is.list(Lags)) Lags[[timeaggs]] else Lags,
            periods         = if(is.list(MA_RollWindows)) MA_RollWindows[[timeaggs]] else MA_RollWindows,
            SDperiods       = if(is.list(SD_RollWindows)) SD_RollWindows[[timeaggs]] else SD_RollWindows,
            Skewperiods     = if(is.list(Skew_RollWindows)) Skew_RollWindows[[timeaggs]] else Skew_RollWindows,
            Kurtperiods     = if(is.list(Kurt_RollWindows)) Kurt_RollWindows[[timeaggs]] else Kurt_RollWindows,
            Quantileperiods = if(is.list(Quantile_RollWindows)) Quantile_RollWindows[[timeaggs]] else Quantile_RollWindows,
            statsFUNs       = RollFunctions,
            targets         = Targets,
            groupingVars    = Fact,
            sortDateName    = DateColumn,
            timeDiffTarget  = NULL,
            timeAgg         = timeaggs,
            WindowingLag    = RollOnLag1,
            Type            = Type,
            Timer           = FALSE,
            SimpleImpute    = SimpleImpute,
            AscRowByGroup   = RowNumsID,
            RecordsKeep     = RowNumsKeep,
            AscRowRemove    = TRUE)
        }

        # Combine data sets ----
        if(Counter > 1L) {
          data.table::setkeyv(KeepData[, TEMPDATE := lubridate::floor_date(get(DateColumn), unit = eval(timeaggs))], c(Fact, "TEMPDATE"))
          KeepData[tempData, (setdiff(names(tempData), Targets)) := mget(paste0("i.", setdiff(names(tempData), Targets)))]
          data.table::set(KeepData, j = "TEMPDATE", value = NULL)
        }
      }
    }
  }

  # Simple impute missed----
  for(miss in seq_len(ncol(KeepData))) data.table::set(KeepData, i = which(is.na(KeepData[[miss]])), j = miss, value = -1)

  # Return data----
  if("TEMPDATE" %chin% names(KeepData)) data.table::setnames(KeepData, "TEMPDATE", DateColumn)

  # Reorder columns
  Nam1 <- names(KeepData)
  Nam <- Nam[Nam %in% Nam1]
  KeepData <- KeepData[, .SD, .SDcols = c(Nam, setdiff(Nam1,Nam))]
  if(Debug) {
    print(Nam)
    print(Nam1)
    print(names(KeepData))
    print(c(Nam, setdiff(Nam1,Nam)))
  }
  return(KeepData)
}

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
#' @param ShortName Default TRUE. If FALSE, Group Variable names will be added to the rolling stat and lag names. If you plan on have multiple versions of lags and rollings stats by different group variables then set this to FALSE.
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
#'   Modeperiods     = 0,
#'   statsFUNs      = c("mean",
#'     "sd","skew","kurt","q05","q95"),
#'   targets        = c("Target"),
#'   groupingVars   = NULL,
#'   sortDateName   = "DateTime",
#'   timeDiffTarget = NULL, # deprecated
#'   timeAgg        = c("days"),
#'   WindowingLag   = 1,
#'   Type           = "Lag",
#'   SimpleImpute   = TRUE)
#' }
#' @noRd
DT_GDL_Feature_Engineering <- function(data,
                                       lags            = 1,
                                       periods         = 0,
                                       SDperiods       = 0,
                                       Skewperiods     = 0,
                                       Kurtperiods     = 0,
                                       Quantileperiods = 0,
                                       statsFUNs       = c("mean"),
                                       targets         = NULL,
                                       groupingVars    = NULL,
                                       sortDateName    = NULL,
                                       timeDiffTarget  = NULL,
                                       timeAgg         = c("days"),
                                       WindowingLag    = 0,
                                       ShortName       = TRUE,
                                       Type            = c("Lag"),
                                       SimpleImpute    = TRUE) {

  # timeAgg
  if(is.null(timeAgg)) {
    timeAgg <- "TimeUnitNULL"
  } else if(tolower(timeAgg) == "raw") {
    timeAggss <- "transactional"
    timeAgg <- "day"
  } else {
    timeAggss <- timeAgg
  }

  # Number of targets
  tarNum <- length(targets)

  # Argument Checks
  if(is.null(lags) && WindowingLag == 1) lags <- 1
  if(!(1 %in% lags) && WindowingLag == 1) lags <- c(1, lags)
  if(any(lags < 0)) stop("lags need to be positive integers")
  if(!is.null(groupingVars)) if(!is.character(groupingVars)) stop("groupingVars needs to be a character scalar or vector")
  if(!is.character(targets)) stop("targets needs to be a character scalar or vector")
  if(!is.character(sortDateName)) stop("sortDateName needs to be a character scalar or vector")
  if(!is.null(timeAgg)) if(!is.character(timeAgg)) stop("timeAgg needs to be a character scalar or vector")
  if(!(WindowingLag %in% c(0, 1))) stop("WindowingLag needs to be either 0 or 1")
  if(!(tolower(Type) %chin% c("lag", "lead"))) stop("Type needs to be either Lag or Lead")
  if(!is.logical(SimpleImpute)) stop("SimpleImpute needs to be TRUE or FALSE")

  # Ensure enough columns are allocated beforehand
  if(!is.null(groupingVars)) {
    if(ncol(data) + (length(lags) + length(periods)) * tarNum * length(groupingVars) * length(statsFUNs) > data.table::truelength(data)) {
      data.table::alloc.col(DT = data, n = ncol(data) + (length(lags) + length(periods)) * tarNum * length(groupingVars) * length(statsFUNs))
    }
  } else {
    if(ncol(data) + (length(lags) + length(periods)) * tarNum * length(statsFUNs) > data.table::truelength(data)) {
      data.table::alloc.col(DT = data, n = ncol(data) + (length(lags) + length(periods)) * tarNum * length(statsFUNs))
    }
  }

  # Begin feature engineering----
  if(!is.null(groupingVars)) {
    for(i in seq_along(groupingVars)) {
      Targets <- targets

      # Sort data----
      if(tolower(Type) == "lag") {
        colVar <- c(groupingVars[i], sortDateName[1L])
        data.table::setorderv(data, colVar, order = 1L)
      } else {
        colVar <- c(groupingVars[i], sortDateName[1L])
        data.table::setorderv(data, colVar, order = -1L)
      }

      # Lags ----
      LAG_Names <- c()
      for(t in Targets) {
        if(ShortName) {
          LAG_Names <- c(LAG_Names, paste0(timeAggss, "_LAG_", lags, "_", t))
        } else {
          LAG_Names <- c(LAG_Names, paste0(timeAggss, "_", groupingVars[i], "_LAG_", lags, "_", t))
        }
      }
      data[, paste0(LAG_Names) := data.table::shift(.SD, n = lags, type = "lag"), by = c(groupingVars[i]), .SDcols = c(Targets)]

      # Define targets----
      if(WindowingLag != 0L) {
        if(ShortName) {
          Targets <- paste0(timeAggss, "_LAG_", WindowingLag, "_", Targets)
        } else {
          Targets <- paste0(timeAggss, "_", groupingVars[i], "_LAG_", WindowingLag, "_", Targets)
        }
      }

      # MA stats ----
      if(any(tolower(statsFUNs) %chin% "mean") && !all(periods %in% c(0L, 1L))) {
        periods <- periods[periods > 1L]
        MA_Names <- c()
        for(t in Targets) for(j in seq_along(periods)) MA_Names <- c(MA_Names, paste0("Mean_", periods[j],"_", t))
        data[, paste0(MA_Names) := data.table::frollmean(
          x = .SD, n = periods, fill = NA, algo = "fast", align = "right", na.rm = TRUE, hasNA = TRUE, adaptive = FALSE),
          by = c(groupingVars[i]), .SDcols = c(Targets)]
      }

      # SD stats ----
      if(any(tolower(statsFUNs) %chin% c("sd")) && !all(SDperiods %in% c(0L,1L))) {
        tempperiods <- SDperiods[SDperiods > 1L]
        SD_Names <- c()
        for(t in Targets) for(j in seq_along(tempperiods)) SD_Names <- c(SD_Names, paste0("SD_", tempperiods[j], "_", t))
        data[, paste0(SD_Names) := data.table::frollapply(x = .SD, n = tempperiods, FUN = sd, na.rm = TRUE), by = c(groupingVars[i]), .SDcols = c(Targets)]
      }

      # Skewness stats ----
      if(any(tolower(statsFUNs) %chin% c("skew")) && !all(Skewperiods %in% c(0L,1L,2L))) {
        tempperiods <- Skewperiods[Skewperiods > 2L]
        Skew_Names <- c()
        for(t in Targets) for(j in seq_along(tempperiods)) Skew_Names <- c(Skew_Names, paste0("Skew_", tempperiods[j], "_", t))
        data[, paste0(Skew_Names) := data.table::frollapply(x = .SD, n = tempperiods, FUN = e1071::skewness, na.rm = TRUE), by = c(groupingVars[i]), .SDcols = Targets]
      }

      # Kurtosis stats ----
      if(any(tolower(statsFUNs) %chin% c("kurt")) && !all(Kurtperiods %in% c(0L,1L,2L,3L,4L))) {
        tempperiods <- Kurtperiods[Kurtperiods > 3L]
        Kurt_Names <- c()
        for(t in Targets) for(j in seq_along(tempperiods)) Kurt_Names <- c(Kurt_Names, paste0("Kurt_", tempperiods[j], "_", t))
        data[, paste0(Kurt_Names) := data.table::frollapply(x = .SD, n = tempperiods, FUN = e1071::kurtosis, na.rm = TRUE), by = c(groupingVars[i]), .SDcols = c(Targets)]
      }

      # Quantiles ----
      if(!all(Quantileperiods %in% c(0L,1L,2L,3L,4L))) {
        tempperiods <- Quantileperiods[Quantileperiods > 4L]
        for(z in c(seq(5L,95L,5L))) {
          if(any(paste0("q",z) %chin% statsFUNs)) {
            Names <- c()
            for(t in Targets) for(j in seq_along(tempperiods)) Names <- c(Names, paste0("Q_", z, "_", tempperiods[j], "_", t))
            data[, paste0(Names) := data.table::frollapply(x = .SD, n = tempperiods, FUN = quantile, probs = z/100, na.rm = TRUE), by = c(groupingVars[i]), .SDcols = c(Targets)]
          }
        }
      }
    }

    # Impute missing values ----
    if(SimpleImpute) {
      for(j in seq_along(data)) {
        if(is.factor(data[[j]])) {
          data.table::set(data, which(!(data[[j]] %in% levels(data[[j]]))), j, "0")
        } else {
          data.table::set(data, which(is.na(data[[j]])), j, -1)
        }
      }
    }

    # Done!! ----
    return(data)

  } else {

    # Sort data
    if(tolower(Type) == "lag") {
      data.table::setorderv(data, c(sortDateName[1L]), order = 1L)
    } else {
      data.table::setorderv(data, c(sortDateName[1L]), order = -1L)
    }
    Targets <- targets

    # Lags ----
    LAG_Names <- c()
    for(t in Targets) LAG_Names <- c(LAG_Names, paste0(timeAggss, "_", "LAG_", lags, "_", t))

    # Build features ----
    data[, eval(LAG_Names) := data.table::shift(.SD, n = lags, type = "lag"), .SDcols = c(Targets)]

    # Define targets ----
    if(WindowingLag != 0L) {
      Targets <- paste0(timeAggss, "_", "LAG_", WindowingLag, "_", Targets)
    } else {
      Targets <- Targets
    }

    # MA stats ----
    if(any(tolower(statsFUNs) %chin% "mean") && !all(periods %in% c(0L, 1L))) {
      periods <- periods[periods > 1L]
      MA_Names <- c()
      for(t in Targets) for(j in seq_along(periods)) MA_Names <- c(MA_Names, paste0("Mean_", periods[j], "_", t))
      data[, paste0(MA_Names) := data.table::frollmean(x = .SD, n = periods, fill = NA, algo = "fast", align = "right", na.rm = TRUE, hasNA = TRUE, adaptive = FALSE), .SDcols = c(Targets)]
    }

    # SD stats ----
    if(any(tolower(statsFUNs) %chin% c("sd")) && !all(SDperiods %in% c(0L,1L))) {
      tempperiods <- SDperiods[SDperiods > 1L]
      SD_Names <- c()
      for(t in Targets) for(j in seq_along(tempperiods)) SD_Names <- c(SD_Names, paste0("SD_", tempperiods[j], "_", t))
      data[, paste0(SD_Names) := data.table::frollapply(x = .SD, n = tempperiods, FUN = sd, na.rm = TRUE), .SDcols = c(Targets)]
    }

    # Skewness stats ----
    if(any(tolower(statsFUNs) %chin% c("skew")) && !all(Skewperiods %in% c(0L,1L,2L))) {
      tempperiods <- Skewperiods[Skewperiods > 2L]
      Skew_Names <- c()
      for(t in Targets) for(j in seq_along(tempperiods)) Skew_Names <- c(Skew_Names, paste0("Skew_", tempperiods[j], "_", t))
      data[, paste0(Skew_Names) := data.table::frollapply(x = .SD, n = tempperiods, FUN = e1071::skewness, na.rm = TRUE), .SDcols = c(Targets)]
    }

    # Kurtosis stats ----
    if(any(tolower(statsFUNs) %chin% c("kurt")) && !all(Kurtperiods %in% c(0L,1L,2L,3L))) {
      tempperiods <- Kurtperiods[Kurtperiods > 3L]
      Kurt_Names <- c()
      for(t in Targets) for(j in seq_along(tempperiods)) Kurt_Names <- c(Kurt_Names, paste0("Kurt_", tempperiods[j], "_", t))
      data[, paste0(Kurt_Names) := data.table::frollapply(x = .SD, n = tempperiods, FUN = e1071::kurtosis, na.rm = TRUE), .SDcols = c(Targets)]
    }

    # Quantiles ----
    if(!all(Quantileperiods %in% c(0L,1L,2L,3L,4L))) {
      tempperiods <- Quantileperiods[Quantileperiods > 4L]
      for(z in c(seq(5L,95L,5L))) {
        if(any(paste0("q",z) %chin% statsFUNs)) {
          Names <- c()
          for(t in Targets) for(j in seq_along(tempperiods)) Names <- c(Names, paste0("Q_", z, "_", tempperiods[j], "_", t))
          data[, paste0(Names) := data.table::frollapply(x = .SD, n = tempperiods, FUN = quantile, probs = z/100, na.rm = TRUE), .SDcols = c(Targets)]
        }
      }
    }

    # Impute missing values ----
    if(SimpleImpute) {
      for(j in seq_along(data)) {
        if(is.factor(data[[j]])) {
          data.table::set(data, which(!(data[[j]] %in% levels(data[[j]]))), j, "0")
        } else {
          data.table::set(data, which(is.na(data[[j]])), j, -1)
        }
      }
    }

    # Done!! ----
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
#' @param ShortName Default TRUE. If FALSE, Group Variable names will be added to the rolling stat and lag names. If you plan on have multiple versions of lags and rollings stats by different group variables then set this to FALSE.
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
#'   timeDiffTarget = NULL, # deprecated
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
                                               lags            = 1,
                                               periods         = 0,
                                               SDperiods       = 0,
                                               Skewperiods     = 0,
                                               Kurtperiods     = 0,
                                               Quantileperiods = 0,
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
                                               ShortName       = TRUE,
                                               RecordsKeep     = 1,
                                               AscRowRemove    = TRUE) {

  # Argument Checks ----
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
  if(!(WindowingLag %in% c(0, 1))) stop("WindowingLag needs to be either 0 or 1")
  if(!(tolower(Type) %chin% c("lag", "lead"))) stop("Type needs to be either Lag or Lead")
  if(!is.logical(Timer)) stop("Timer needs to be TRUE or FALSE")
  if(!is.logical(SimpleImpute)) stop("SimpleImpute needs to be TRUE or FALSE")
  if(!is.character(AscRowByGroup)) stop("AscRowByGroup needs to be a character scalar for the name of your RowID column")

  # Base columns from data ----
  ColKeep <- names(data)

  # Convert to data.table if not already----
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  # Max data to keep ----
  MaxCols <- 0
  if(!is.null(lags)) MaxCols <- max(MaxCols,lags)
  if(!is.null(periods)) MaxCols <- max(MaxCols,periods)
  if(!is.null(SDperiods)) MaxCols <- max(MaxCols,SDperiods)
  if(!is.null(Skewperiods)) MaxCols <- max(MaxCols,Skewperiods)
  if(!is.null(Kurtperiods)) MaxCols <- max(MaxCols,Kurtperiods)
  if(!is.null(Quantileperiods)) MaxCols <- max(MaxCols,Quantileperiods)
  if(WindowingLag == 0) MaxCols <- MaxCols - 1L

  # Set up counter for countdown ----
  CounterIndicator <- 0
  tarNum <- length(targets)

  # Ensure enough columns are allocated beforehand: data.table allocates 1024 by default----
  if(!is.null(groupingVars)) {
    if(ncol(data) + (length(lags) + length(periods)) * tarNum * length(groupingVars) * length(statsFUNs) > data.table::truelength(data)) {
      data.table::alloc.col(DT = data, n = ncol(data) + (length(lags) + length(periods)) * tarNum * length(groupingVars) * length(statsFUNs))
    }
  } else {
    if(ncol(data) + (length(lags) + length(periods)) * tarNum * length(statsFUNs) > data.table::truelength(data)) {
      data.table::alloc.col(DT = data, n = ncol(data) + (length(lags) + length(periods)) * tarNum * length(statsFUNs))
    }
  }

  # Begin feature engineering ----
  if(!is.null(groupingVars)) {
    for(i in seq_along(groupingVars)) {
      TargetS <- targets

      # Subset data ----
      data1 <- data[get(groupingVars[i]) %in% data[get(AscRowByGroup) %in% c(RecordsKeep)][[eval(groupingVars[i])]]]

      # Sort data----
      colVar <- c(groupingVars[i], sortDateName[1L])
      if(tolower(Type) == "lag") {
        data.table::setorderv(data1, colVar, order = 1L)
      } else {
        data.table::setorderv(data1, colVar, order = -1L)
      }

      # Subset data for the rows needed to compute MaxCols ----
      rows <- data1[, .I[get(AscRowByGroup) %in% c(RecordsKeep)]]
      Rows <- c()
      for(x in seq_along(rows)) {
        if(x == 1L) {
          Rows <- rows[x]:(max(rows[x]-MaxCols,1))
        } else {
          Rows <- c(Rows, rows[x]:(max(rows[x]-MaxCols,1)))
        }
      }
      data1 <- data1[unique(Rows)]

      # Sort data ----
      if(tolower(Type) == "lag") {
        data.table::setorderv(data1, colVar, order = 1L)
      } else {
        data.table::setorderv(data1, colVar, order = -1L)
      }

      # Lags ----
      LagKeep <- c()
      LagKeeps <- list()
      LagCols <- c()
      LagColss <- list()
      for(t in TargetS) {
        for(l in seq_len(MaxCols)) {
          if(ShortName) {
            LagCols <- c(LagCols, paste0(timeAggss, "_LAG_", l, "_", t))
          } else {
            LagCols <- c(LagCols, paste0(timeAggss, "_", groupingVars[i], "_LAG_", l, "_", t))
          }
          if(l %in% lags) {
            if(ShortName) {
              LagKeep <- c(LagKeep, paste0(timeAggss, "_LAG_", l, "_", t))
            } else {
              LagKeep <- c(LagKeep, paste0(timeAggss, "_", groupingVars[i], "_LAG_", l, "_", t))
            }
          }
        }
        LagColss[[t]] <- LagCols
        LagKeeps[[t]] <- LagKeep
        LagCols <- c()
        LagKeep <- c()
      }

      # Build features ----
      data1[, paste0(unlist(LagColss)) := data.table::shift(.SD, n = seq_len(MaxCols), type = "lag"), by = c(groupingVars[i]), .SDcols = c(TargetS)]

      # Define targets----
      if(WindowingLag != 0) {
        if(ShortName) {
          TargetS <- c(paste0(timeAggss, "_LAG_", WindowingLag, "_", TargetS))
        } else {
          TargetS <- c(paste0(timeAggss, "_", groupingVars[i], "_LAG_", WindowingLag, "_", TargetS))
        }
      } else {
        TargetS <- TargetS
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
            data1[, paste0("Mean_", j, "_", t) := fBasics::rowAvgs(.SD), .SDcols = LagColss[[TargetN]][seq_len(j)]]
            PeriodKeep <-  c(PeriodKeep, paste0("Mean_",j,"_",t))
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
            data1[, paste0("SD_",j,"_",t) := fBasics::rowSds(.SD), .SDcols = c(LagColss[[TargetN]][seq_len(j)])]
            PeriodKeep <- c(PeriodKeep, paste0("SD_",j,"_",t))
          }
        }
      }

      # Skewnewss----
      if(any(tolower(statsFUNs) %chin% "skew") && !all(Skewperiods %in% c(0,1,2))) {
        tempperiods <- Skewperiods[Skewperiods > 2L]
        TargetN <- 0L
        for(t in TargetS) {
          TargetN <- TargetN + 1L
          for(j in tempperiods) {
            data1[, paste0("Skew_",j,"_",t) := fBasics::rowSkewness(.SD), .SDcols = LagColss[[TargetN]][seq_len(j)]]
            PeriodKeep <- c(PeriodKeep, paste0("Skew_",j,"_",t))
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
            data1[, paste0("Kurt_",j,"_",t) := fBasics::rowKurtosis(.SD), .SDcols = LagColss[[TargetN]][seq_len(j)]]
            PeriodKeep <- c(PeriodKeep, paste0("Kurt_",j,"_",t))
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
                data1[, paste0("Q_",z,"_",j,"_",t) := fBasics::rowKurtosis(.SD), .SDcols = LagColss[[TargetN]][seq_len(j)]]
                PeriodKeep <- c(PeriodKeep, paste0("Q_", z, "_", j, "_", t))
              }
            }
          }
        }
      }

      # Only keep requested columns----
      if(i == 1) {
        keep <- c(ColKeep, unlist(LagKeeps), PeriodKeep)
        FinalData <- data1[, ..keep]
      } else {
        keep <- c("TEMPDATE",AscRowByGroup, unlist(LagKeeps), PeriodKeep)
        FinalData <- merge(FinalData, data1[, ..keep], by = "TEMPDATE", all = FALSE)
      }
    }

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
      colVar <- c(sortDateName[1L])
      data.table::setorderv(data, colVar, order = 1L)
    } else {
      colVar <- c(sortDateName[1L])
      data.table::setorderv(data, colVar, order = -1L)
    }

    # Subset data for the rows needed to compute MaxCols ----
    rows <- data[, .I[get(AscRowByGroup) %in% c(RecordsKeep)]]
    Rows <- c()
    for(x in seq_along(rows)) {
      if(x == 1L) {
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

    # Define targets ----
    if(WindowingLag != 0) TargetS <- c(paste0(timeAggss, "_LAG_", WindowingLag, "_", TargetS))

    # Subset data----
    data <- data[get(AscRowByGroup) %in% c(RecordsKeep)]

    # Initalize PeriodKeep
    PeriodKeep <- c()

    # Moving Averages----
    if(any(tolower(statsFUNs) %chin% "mean") && !all(periods %in% c(0L, 1L))) {
      periods <- periods[periods > 1L]
      incre <- 0L
      TargetN <- 0L
      for(t in TargetS) { # t <- "days_LAG_1_Target"
        TargetN <- TargetN + 1L
        for(j in periods) { # j = 10
          data[, paste0("Mean_", j, "_", t) := fBasics::rowAvgs(.SD), .SDcols = LagColss[[TargetN]][seq_len(j)]]
          #data[, paste0("Mean_", j, "_", t) := mean(.SD), .SDcols = LagColss[[TargetN]][seq_len(j)], by = .I]
          PeriodKeep <- c(PeriodKeep, paste0("Mean_", j, "_", t))
        }
      }
    }

    # Standard Deviations----
    if(any(tolower(statsFUNs) %chin% "sd") && !all(SDperiods %in% c(0,1))) {
      tempperiods <- SDperiods[SDperiods > 1L]
      TargetN <- 0L
      for(t in TargetS) {
        TargetN <- TargetN + 1L
        for(j in tempperiods) {
          data[, paste0("SD_", j, "_", t) := fBasics::rowSds(.SD), .SDcols = LagColss[[TargetN]][seq_len(j)]]
          PeriodKeep <- c(PeriodKeep, paste0("SD_", j, "_", t))
        }
      }
    }

    # Skewness----
    if(any(tolower(statsFUNs) %chin% "skew") && !all(Skewperiods %in% c(0,1,2))) {
      tempperiods <- Skewperiods[Skewperiods > 2L]
      TargetN <- 0L
      for(t in TargetS) {
        TargetN <- TargetN + 1L
        for(j in tempperiods) {
          data[, paste0("Skew_", j, "_", t) := fBasics::rowSkewness(.SD), .SDcols = LagColss[[TargetN]][seq_len(j)]]
          PeriodKeep <- c(PeriodKeep, paste0("Skew_", j, "_", t))
        }
      }
    }

    # Kurtosis----
    if(any(tolower(statsFUNs) %chin% "kurt") && !all(Kurtperiods %in% c(0,1,2,3))) {
      tempperiods <- Kurtperiods[Kurtperiods > 3L]
      TargetN <- 0L
      for(t in TargetS) {
        TargetN <- TargetN + 1L
        for(j in tempperiods) {
          data[, paste0("Kurt_", j, "_", t) := fBasics::rowKurtosis(.SD), .SDcols = LagColss[[TargetN]][seq_len(j)]]
          PeriodKeep <- c(PeriodKeep, paste0("Kurt_", j, "_", t))
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
              data[, eval(paste0("Q_",z,"_",j,"_",t)) := rowQuantiles(x = .SD, prob = z/100), .SDcols = LagColss[[TargetN]][seq_len(j)]]
              PeriodKeep <- c(PeriodKeep, paste0("Q_",z,"_",j,"_",t))
            }
          }
        }
      }
    }

    # Only keep requested columns----
    keep <- c(ColKeep, unlist(LagKeeps), PeriodKeep)
    data <- data[, ..keep]

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

#' @title Partial_DT_GDL_Feature_Engineering2
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
#' data <- Partial_DT_GDL_Feature_Engineering2(
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
#'   timeDiffTarget = NULL, # deprecated
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
Partial_DT_GDL_Feature_Engineering2 <- function(data,
                                                lags            = 1,
                                                periods         = 0,
                                                SDperiods       = 0,
                                                Skewperiods     = 0,
                                                Kurtperiods     = 0,
                                                Quantileperiods = 0,
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

  # timeAgg----
  if(is.null(timeAgg)) {
    timeAgg <- "TimeUnitNULL"
  } else if(tolower(timeAgg) == "raw") {
    timeAggss <- "transactional"
    timeAgg <- "day"
  } else {
    timeAggss <- timeAgg
  }

  # Number of targets
  tarNum <- length(targets)

  # Argument Checks ----
  if(is.null(lags) && WindowingLag == 1) lags <- 1
  if(!(1 %in% lags) && WindowingLag == 1) lags <- c(1, lags)
  if(any(lags < 0)) stop("lags need to be positive integers")
  if(!is.null(groupingVars)) if(!is.character(groupingVars)) stop("groupingVars needs to be a character scalar or vector")
  if(!is.character(targets)) stop("targets needs to be a character scalar or vector")
  if(!is.character(sortDateName)) stop("sortDateName needs to be a character scalar or vector")
  if(!is.null(timeAgg)) if(!is.character(timeAgg)) stop("timeAgg needs to be a character scalar or vector")
  if(!(WindowingLag %in% c(0, 1))) stop("WindowingLag needs to be either 0 or 1")
  if(!(tolower(Type) %chin% c("lag", "lead"))) stop("Type needs to be either Lag or Lead")
  if(!is.logical(SimpleImpute)) stop("SimpleImpute needs to be TRUE or FALSE")

  # Ensure enough columns are allocated beforehand----
  if(!is.null(groupingVars)) {
    if(ncol(data) + (length(lags) + length(periods)) * tarNum * length(groupingVars) * length(statsFUNs) > data.table::truelength(data)) {
      data.table::alloc.col(DT = data, n = ncol(data) + (length(lags) + length(periods)) * tarNum * length(groupingVars) * length(statsFUNs))
    }
  } else {
    if(ncol(data) + (length(lags) + length(periods)) * tarNum * length(statsFUNs) > data.table::truelength(data)) {
      data.table::alloc.col(DT = data, n = ncol(data) + (length(lags) + length(periods)) * tarNum * length(statsFUNs))
    }
  }

  # Rows to keep for max periods (used to be columns)
  MaxCols <- max(lags,periods,SDperiods,Skewperiods,Kurtperiods,Quantileperiods)

  # Colnames
  Cols <- names(data.table::copy(data))

  # Begin feature engineering----
  if(!is.null(groupingVars)) {
    for(i in seq_along(groupingVars)) {
      Targets <- targets

      # Subset data ----
      data1 <- data[get(groupingVars[i]) %in% data[get(AscRowByGroup) %in% c(RecordsKeep)][[eval(groupingVars[i])]]]

      # Sort data----
      colVar <- c(groupingVars[i], sortDateName[1L])
      if(tolower(Type) == "lag") {
        data.table::setorderv(data1, colVar, order = 1L)
      } else {
        data.table::setorderv(data1, colVar, order = -1L)
      }

      # Rows needed
      rows <- data1[, .I[get(AscRowByGroup) %in% c(RecordsKeep)]]
      Rows <- c()
      for(x in seq_along(rows)) {
        if(x == 1L) {
          Rows <- rows[x]:(max(rows[x]-MaxCols,1))
        } else {
          Rows <- c(Rows, rows[x]:(max(rows[x]-MaxCols,1)))
        }
      }
      data1 <- data1[unique(Rows)]

      # Sort data----
      if(tolower(Type) == "lag") {
        colVar <- c(groupingVars[i], sortDateName[1L])
        data.table::setorderv(data1, colVar, order = 1L)
      } else {
        colVar <- c(groupingVars[i], sortDateName[1L])
        data.table::setorderv(data1, colVar, order = -1L)
      }

      # Lags ----
      LAG_Names <- c()
      for(t in Targets) LAG_Names <- c(LAG_Names, paste0(timeAggss, "_", groupingVars[i], "_LAG_", lags, "_", t))
      data1[, paste0(LAG_Names) := data.table::shift(.SD, n = lags, type = "lag"), by = c(groupingVars[i]), .SDcols = Targets]

      # Define targets----
      if(WindowingLag != 0L) {
        Targets <- c(paste0(timeAggss, "_", groupingVars[i], "_LAG_", WindowingLag, "_", Targets))
      }

      # MA stats ----
      if(any(tolower(statsFUNs) %chin% "mean") && !all(periods %in% c(0L, 1L))) {
        periods <- periods[periods > 1L]
        MA_Names <- c()
        for(t in Targets) for(j in seq_along(periods)) MA_Names <- c(MA_Names, paste0("Mean","_", periods[j],"_", t))
        data1[, paste0(MA_Names) := data.table::frollmean(
          x = .SD, n = periods, fill = NA, algo = "fast", align = "right", na.rm = TRUE, hasNA = TRUE, adaptive = FALSE),
          by = c(groupingVars[i]), .SDcols = c(Targets)]
      }

      # SD stats ----
      if(any(tolower(statsFUNs) %chin% c("sd")) && !all(SDperiods %in% c(0L,1L))) {
        tempperiods <- SDperiods[SDperiods > 1L]
        SD_Names <- c()
        for(t in Targets) for(j in seq_along(tempperiods)) SD_Names <- c(SD_Names, paste0("SD_", tempperiods[j], "_", t))
        data1[, paste0(SD_Names) := data.table::frollapply(x = .SD, n = tempperiods, FUN = sd, na.rm = TRUE), by = c(groupingVars[i]), .SDcols = c(Targets)]
      }

      # Skewness stats ----
      if(any(tolower(statsFUNs) %chin% c("skew")) && !all(Skewperiods %in% c(0L,1L,2L))) {
        tempperiods <- Skewperiods[Skewperiods > 2L]
        Skew_Names <- c()
        for(t in Targets) for(j in seq_along(tempperiods)) Skew_Names <- c(Skew_Names, paste0("Skew_", tempperiods[j], "_", t))
        data1[, paste0(Skew_Names) := data.table::frollapply(x = .SD, n = tempperiods, FUN = e1071::skewness, na.rm = TRUE), by = c(groupingVars[i]), .SDcols = Targets]
      }

      # Kurtosis stats ----
      if(any(tolower(statsFUNs) %chin% c("kurt")) && !all(Kurtperiods %in% c(0L,1L,2L,3L,4L))) {
        tempperiods <- Kurtperiods[Kurtperiods > 3L]
        Kurt_Names <- c()
        for(t in Targets) for(j in seq_along(tempperiods)) Kurt_Names <- c(Kurt_Names, paste0("Kurt_", tempperiods[j], "_", t))
        data1[, paste0(Kurt_Names) := data.table::frollapply(x = .SD, n = tempperiods, FUN = e1071::kurtosis, na.rm = TRUE), by = c(groupingVars[i]), .SDcols = c(Targets)]
      }

      # Quantiles ----
      if(!all(Quantileperiods %in% c(0L,1L,2L,3L,4L))) {
        tempperiods <- Quantileperiods[Quantileperiods > 4L]
        for(z in c(seq(5L,95L,5L))) {
          if(any(paste0("q",z) %chin% statsFUNs)) {
            Names <- c()
            for(t in Targets) for(j in seq_along(tempperiods)) Names <- c(Names, paste0("Q_", z, "_", tempperiods[j], "_", t))
            data1[, paste0(Names) := data.table::frollapply(x = .SD, n = tempperiods, FUN = quantile, probs = z/100, na.rm = TRUE), by = c(groupingVars[i]), .SDcols = c(Targets)]
          }
        }
      }

      # Only keep requested columns----
      if(i == 1) {
        FinalData <- data.table::copy(data1)
      } else {
        keep <- c("TEMPDATE",AscRowByGroup, setdiff(names(data1), names(data)))
        FinalData <- merge(FinalData, data1[, ..keep], by = "TEMPDATE", all = FALSE)
      }
    }

    # Subset data
    FinalData <- FinalData[get(AscRowByGroup) %in% RecordsKeep][, eval(AscRowByGroup) := NULL]

    # Impute missing values ----
    if(SimpleImpute) {
      for(j in which(names(FinalData) %chin% setdiff(names(FinalData), names(data)))) {
        if(is.factor(FinalData[[j]])) {
          data.table::set(FinalData, which(!(FinalData[[j]] %in% levels(FinalData[[j]]))), j, "0")
        } else {
          data.table::set(FinalData, which(is.na(FinalData[[j]])), j, -1)
        }
      }
    }

    # Done!! ----
    return(FinalData)

  } else {

    TargetS <- targets

    # Sort data----
    if(tolower(Type) == "lag") {
      colVar <- c(sortDateName[1L])
      data.table::setorderv(data, colVar, order = 1L)
    } else {
      colVar <- c(sortDateName[1L])
      data.table::setorderv(data, colVar, order = -1L)
    }

    # Subset data for the rows needed to compute MaxCols ----
    MaxCols <- max(lags, periods, SDperiods, Skewperiods, Kurtperiods, Quantileperiods)
    rows <- data[, .I[get(AscRowByGroup) %in% c(RecordsKeep)]]
    Rows <- c()
    for(x in seq_along(rows)) {
      if(x == 1L) {
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

    # Lags ----
    LAG_Names <- c()
    for(t in TargetS) LAG_Names <- c(LAG_Names, paste0(timeAggss, "_", "LAG_", lags, "_", t))

    # Build features ----
    data[, paste0(LAG_Names) := data.table::shift(.SD, n = lags, type = "lag"), .SDcols = c(TargetS)]

    # Define targets ----
    if(WindowingLag != 0) TargetS <- c(paste0(timeAggss, "_LAG_", WindowingLag, "_", TargetS))

    # Define TargetS ----
    if(WindowingLag != 0L) {
      Targets <- TargetS
    } else {
      Targets <- Targets
    }

    # MA stats ----
    if(any(tolower(statsFUNs) %chin% "mean") & !all(periods %in% c(0L, 1L))) {
      periods <- periods[periods > 1L]
      MA_Names <- c()
      for(t in Targets) for(j in seq_along(periods)) MA_Names <- c(MA_Names, paste0("Mean_", periods[j], "_", t))
      data[, paste0(MA_Names) := data.table::frollmean(x = .SD, n = periods, fill = NA, algo = "fast", align = "right", na.rm = TRUE, hasNA = TRUE, adaptive = FALSE), .SDcols = c(Targets)]
    }

    # SD stats ----
    if(any(tolower(statsFUNs) %chin% c("sd")) & !all(SDperiods %in% c(0L,1L))) {
      tempperiods <- SDperiods[SDperiods > 1L]
      SD_Names <- c()
      for(t in Targets) for(j in seq_along(tempperiods)) SD_Names <- c(SD_Names, paste0("SD_", tempperiods[j], "_", t))
      data[get(AscRowByGroup) %in% RecordsKeep, paste0(SD_Names) := data.table::frollapply(x = .SD, n = tempperiods, FUN = sd, na.rm = TRUE), .SDcols = c(Targets)]
    }

    # Skewness stats ----
    if(any(tolower(statsFUNs) %chin% c("skew")) & !all(Skewperiods %in% c(0L,1L,2L))) {
      tempperiods <- Skewperiods[Skewperiods > 2L]
      Skew_Names <- c()
      for(t in Targets) for(j in seq_along(tempperiods)) Skew_Names <- c(Skew_Names, paste0("Skew_", tempperiods[j], "_", t))
      data[get(AscRowByGroup) %in% RecordsKeep, paste0(Skew_Names) := data.table::frollapply(x = .SD, n = tempperiods, FUN = e1071::skewness, na.rm = TRUE), .SDcols = c(Targets)]
    }

    # Kurtosis stats ----
    if(any(tolower(statsFUNs) %chin% c("kurt")) & !all(Kurtperiods %in% c(0L,1L,2L,3L))) {
      tempperiods <- Kurtperiods[Kurtperiods > 3L]
      Kurt_Names <- c()
      for(t in Targets) for(j in seq_along(tempperiods)) Kurt_Names <- c(Kurt_Names, paste0("Kurt_", tempperiods[j], "_", t))
      data[get(AscRowByGroup) %in% RecordsKeep, paste0(Kurt_Names) := data.table::frollapply(x = .SD, n = tempperiods, FUN = e1071::kurtosis, na.rm = TRUE), .SDcols = c(Targets)]
    }

    # Quantiles ----
    if(!all(Quantileperiods %in% c(0L,1L,2L,3L,4L))) {
      tempperiods <- Quantileperiods[Quantileperiods > 4L]
      for(z in c(seq(5L,95L,5L))) {
        if(any(paste0("q",z) %chin% statsFUNs)) {
          Names <- c()
          for(t in Targets) for(j in seq_along(tempperiods)) Names <- c(Names, paste0("Q_", z, "_", tempperiods[j], "_", t))
          data[get(AscRowByGroup) %in% RecordsKeep, paste0(Names) := data.table::frollapply(x = .SD, n = tempperiods, FUN = quantile, probs = z/100, na.rm = TRUE), .SDcols = c(Targets)]
        }
      }
    }

    # Only keep requested columns ----
    data <- data[get(AscRowByGroup) %in% RecordsKeep][, temp := NULL]

    # Impute missing values ----
    if(SimpleImpute) {
      for(j in which(setdiff(names(data), Cols) %chin% names(data))) {
        if(is.factor(data[[j]])) {
          data.table::set(data, which(!(data[[j]] %in% levels(data[[j]]))), j, "0")
        } else {
          data.table::set(data, which(is.na(data[[j]])), j, -1)
        }
      }
    }

    # Done!! ----
    return(data)
  }
}

#' @title DiffLagN
#'
#' @description Create differences for numeric and date variables
#'
#' @author Adrian Antico
#' @family Feature Engineering - Numeric Types
#'
#' @param data Source data
#' @param RunMode 'train' or 'score'
#' @param ArgsList ArgsList_FFE
#' @param SkipCols Vector of column names to remove from data
#' @param N1 Lookback for time period 1
#' @param N2 Lookback for time period 2
#' @param RunNumber Iteration number when running multiple times
#' @param RemoveNAs Remove NULL values created by lags
#'
#' @examples
#' \dontrun{
#' Output <- AutoQuant:::DiffLagN(
#'   data = data,
#'   RunMode = "train",
#'   ArgsList = ArgsList_FE,
#'   SkipCols = NULL,
#'   N1 = 0,
#'   N2 = 1,
#'   RunNumber = 1,
#'   RemoveNAs = FALSE)
#' data <- Output$data
#' ArgsList_FE <- Output$ArgsList
#' }
#'
#' @return A list containing the data and the ArgsList
#' @noRd
DiffLagN <- function(data = NULL,
                     RunMode = "train",
                     ArgsList = ArgsList_FFE,
                     SkipCols = NULL,
                     N1 = 0,
                     N2 = 1,
                     RunNumber = 1,
                     RemoveNAs = FALSE) {

  # Metadata
  Start <- Sys.time()

  # Run function
  if(tolower(RunMode) == "train") {

    # Metadata
    tempnames <- names(data.table::copy(data))

    # Run function
    data <- AutoQuant::AutoDiffLagN(
      data = data,
      GroupVariables = ArgsList$Data$GroupVariables,
      DateVariable = ArgsList$Data$DateVariables,
      DiffVariables = ArgsList$Data$DiffVariables,
      DiffDateVariables = ArgsList$Data$DiffDateVariables,
      NLag1 = N1,
      NLag2 = N2,
      Sort = TRUE,
      RemoveNA = RemoveNAs)

    # Args Tracking
    ArgsList$FE_AutoDiffLagN$GroupVariables[[RunNumber]] <- ArgsList$Data$GroupVariables
    ArgsList$FE_AutoDiffLagN$DateVariable[[RunNumber]] <- ArgsList$Data$DateVariables
    ArgsList$FE_AutoDiffLagN$DiffVariables[[RunNumber]] <- ArgsList$Data$DiffVariables
    ArgsList$FE_AutoDiffLagN$DiffDateVariables[[RunNumber]] <- ArgsList$Data$DiffDateVariables
    ArgsList$FE_AutoDiffLagN$NLag1[[RunNumber]] <- N1
    ArgsList$FE_AutoDiffLagN$NLag2[[RunNumber]] <- N2
    ArgsList$FE_AutoDiffLagN$Sort[[RunNumber]] <- TRUE
    ArgsList$FE_AutoDiffLagN$RemoveNA[[RunNumber]] <- RemoveNAs

    # New columns tracking
    ArgsList$FE_Columns$AutoLagDiffN[[RunNumber]] <- setdiff(names(data), tempnames)

    # Run time tracking
    End <- Sys.time()
    ArgsList$RunTime$AutoLagDiffN_Training[[RunNumber]] <- difftime(End, Start, units = "mins")

  } else {

    # Metadata
    Start <- Sys.time()

    # Run function
    data <- AutoQuant::AutoDiffLagN(
      data = data,
      GroupVariables = ArgsList$FE_AutoDiffLagN$GroupVariables[[RunNumber]],
      DateVariable = ArgsList$FE_AutoDiffLagN$DateVariables[[RunNumber]],
      DiffVariables = ArgsList$FE_AutoDiffLagN$DiffVariables[[RunNumber]],
      DiffDateVariables = ArgsList$FE_AutoDiffLagN$DiffDateVariables[[RunNumber]],
      NLag1 = ArgsList$FE_AutoDiffLagN$NLag1[[RunNumber]],
      NLag2 = ArgsList$FE_AutoDiffLagN$NLag2[[RunNumber]],
      Sort = ArgsList$FE_AutoDiffLagN$Sort[[RunNumber]],
      RemoveNA = ArgsList$FE_AutoDiffLagN$RemoveNA[[RunNumber]])

    # Skip cols
    if(!is.null(SkipCols)) {
      temp <- names(data)
      temp <- temp[!temp %chin% SkipCols]
      temp <- setdiff(names(data), temp)
      data.table::set(data, j = temp, value = NULL)
    }

    # Run time tracking
    End <- Sys.time()
    ArgsList$RunTime$AutoDiffLagN_Scoring[[RunNumber]] <- difftime(End, Start, units = "mins")
  }

  # Return
  return(list(data = data, ArgsList = ArgsList))
}

#' @title TimeSeriesFeatures
#'
#' @description Create lags and rolling stats
#'
#' @author Adrian Antico
#' @family Feature Engineering - Cross Row Operations
#'
#' @param data Source data
#' @param RunMode 'train' or 'score'
#' @param ArgsList ArgsList_FFE
#' @param SkipCols Vector of column names to remove from data
#' @param KeepRowsColumnName 'scoring' mode. Name of column where subset values reside
#' @param KeepRowsGroupID id values to use for subsetting for records to be scored
#' @param DebugMode Logical
#'
#' @examples
#' \dontrun{
#' Output <- AutoQuant:::TimeSeriesFeatures(
#'   data = data,
#'   RunMode = "train",
#'   ArgsList = NULL,
#'   KeepRowsColumnName = NULL,
#'   KeepRowsGroupID = NULL,
#'   SkipCols = NULL,
#'   DebugMode = FALSE)
#' data <- Output$data
#' ArgsList <- Output$ArgsList
#' }
#'
#' @return A list containing the data and the ArgsList
#' @noRd
TimeSeriesFeatures <- function(data = NULL,
                               RunMode = "train",
                               ArgsList = NULL,
                               KeepRowsColumnName = NULL,
                               KeepRowsGroupID = NULL,
                               SkipCols = NULL,
                               DebugMode = FALSE) {

  # Metadata
  Start <- Sys.time()

  # Run function
  if(tolower(RunMode) == "train") {

    # Metadata
    tempnames <- names(data.table::copy(data))

    # Run function
    data <- AutoQuant::AutoLagRollStats(

      # Data
      data                 = data,
      DateColumn           = ArgsList$Data$DateVariables[1L],
      Targets              = ArgsList$Data$TargetVariables[1L],
      HierarchyGroups      = NULL,
      IndependentGroups    = ArgsList$FE_Args$TimeSeriesVariables$TimeSeriesGroupVariables,
      TimeUnitAgg          = ArgsList$FE_Args$TimeSeriesVariables$TimeUnitAgg,
      TimeGroups           = ArgsList$FE_Args$TimeSeriesVariables$TimeSeriesDateGroups,
      TimeBetween          = NULL,
      TimeUnit             = ArgsList$FE_Args$TimeSeriesVariables$TimeUnitAgg,

      # Services
      RollOnLag1           = TRUE,
      Type                 = "Lag",
      SimpleImpute         = ArgsList$FE_Args$Clean$Impute,

      # Calculated Columns
      Lags                 = ArgsList$FE_Args$TimeSeriesVariables$Lag_Periods,
      MA_RollWindows       = ArgsList$FE_Args$TimeSeriesVariables$RollAverage_Periods,
      SD_RollWindows       = ArgsList$FE_Args$TimeSeriesVariables$RollStandardDeviation_Periods,
      Skew_RollWindows     = ArgsList$FE_Args$TimeSeriesVariables$RollSkewness_Periods,
      Kurt_RollWindows     = ArgsList$FE_Args$TimeSeriesVariables$RollKurtosis_Periods,
      Quantile_RollWindows = ArgsList$FE_Args$TimeSeriesVariables$RollQuantiles_Periods,
      Quantiles_Selected   = ArgsList$FE_Args$TimeSeriesVariables$RollQuantiles,
      Debug                = DebugMode)

    # Args Tracking
    ArgsList$TimeSeriesFeatures$DateColumn <- ArgsList$Data$DateVariables[1L]
    ArgsList$TimeSeriesFeatures$Targets <- ArgsList$Data$TargetVariables[1L]
    ArgsList$TimeSeriesFeatures$HierarchyGroups <- NULL
    ArgsList$TimeSeriesFeatures$IndependentGroups <- ArgsList$FE_Args$TimeSeriesVariables$TimeSeriesGroupVariables
    ArgsList$TimeSeriesFeatures$TimeUnitAgg <- ArgsList$FE_Args$TimeSeriesVariables$TimeUnitAgg
    ArgsList$TimeSeriesFeatures$TimeGroups <- ArgsList$FE_Args$TimeSeriesVariables$TimeSeriesDateGroups
    ArgsList$TimeSeriesFeatures$TimeBetween <- NULL
    ArgsList$TimeSeriesFeatures$TimeUnit <- ArgsList$FE_Args$TimeSeriesVariables$TimeUnitAgg
    ArgsList$TimeSeriesFeatures$RollOnLag1 <- TRUE
    ArgsList$TimeSeriesFeatures$Type <- "Lag"
    ArgsList$TimeSeriesFeatures$SimpleImpute <- ArgsList$FE_Args$Clean$MissNum
    ArgsList$TimeSeriesFeatures$Lags <- ArgsList$FE_Args$TimeSeriesVariables$Lag_Periods
    ArgsList$TimeSeriesFeatures$MA_RollWindows <- ArgsList$FE_Args$TimeSeriesVariables$RollAverage_Periods
    ArgsList$TimeSeriesFeatures$SD_RollWindows <- ArgsList$FE_Args$TimeSeriesVariables$RollStandardDeviation_Periods
    ArgsList$TimeSeriesFeatures$Skew_RollWindows <- ArgsList$FE_Args$TimeSeriesVariables$RollSkewness_Periods
    ArgsList$TimeSeriesFeatures$Kurt_RollWindows <- ArgsList$FE_Args$TimeSeriesVariables$RollKurtosis_Periods
    ArgsList$TimeSeriesFeatures$Quantile_RollWindows <- ArgsList$FE_Args$TimeSeriesVariables$RollQuantiles_Periods
    ArgsList$TimeSeriesFeatures$Quantiles_Selected <- ArgsList$FE_Args$TimeSeriesVariables$RollQuantiles

    # New columns tracking
    ArgsList$FE_Columns$TimeSeriesFeatures <- setdiff(names(data), tempnames)

    # Run time tracking
    End <- Sys.time()
    ArgsList$RunTime$TimeSeriesFeatures_Training <- difftime(End, Start, units = "mins")

  } else {

    # Metadata
    Start <- Sys.time()

    # Run function
    data <- AutoQuant::AutoLagRollStatsScoring(

      # Data
      data                 = data,
      RowNumsID            = KeepRowsColumnName,
      RowNumsKeep          = KeepRowsGroupID,
      DateColumn           = ArgsList$TimeSeriesFeatures$DateColumn,
      Targets              = ArgsList$TimeSeriesFeatures$Targets,
      HierarchyGroups      = ArgsList$TimeSeriesFeatures$HierarchyGroups,
      IndependentGroups    = ArgsList$TimeSeriesFeatures$IndependentGroups,

      # Services
      TimeBetween          = ArgsList$TimeSeriesFeatures$TimeBetween,
      TimeGroups           = ArgsList$TimeSeriesFeatures$TimeGroups,
      TimeUnit             = ArgsList$TimeSeriesFeatures$TimeUnit,
      TimeUnitAgg          = ArgsList$TimeSeriesFeatures$TimeUnitAgg,
      RollOnLag1           = ArgsList$TimeSeriesFeatures$RollOnLag1,
      Type                 = ArgsList$TimeSeriesFeatures$Type,
      SimpleImpute         = ArgsList$TimeSeriesFeatures$SimpleImpute,

      # Calculated Columns
      Lags                  = ArgsList$TimeSeriesFeatures$Lags,
      MA_RollWindows        = ArgsList$TimeSeriesFeatures$MA_RollWindows,
      SD_RollWindows        = ArgsList$TimeSeriesFeatures$SD_RollWindows,
      Skew_RollWindows      = ArgsList$TimeSeriesFeatures$Skew_RollWindows,
      Kurt_RollWindows      = ArgsList$TimeSeriesFeatures$Kurt_RollWindows,
      Quantile_RollWindows  = ArgsList$TimeSeriesFeatures$Quantile_RollWindows,
      Quantiles_Selected    = ArgsList$TimeSeriesFeatures$Quantiles_Selected,
      Debug                 = DebugMode)

    # Skip cols
    if(!is.null(SkipCols)) {
      temp <- names(data)
      temp <- temp[!temp %chin% SkipCols]
      temp <- setdiff(names(data), temp)
      data.table::set(data, j = temp, value = NULL)
    }

    # Run time tracking
    End <- Sys.time()
    ArgsList$RunTime$TimeSeriesFeatures_Scoring <- difftime(End, Start, units = "mins")
  }

  # Return
  return(list(data = data, ArgsList = ArgsList))
}
