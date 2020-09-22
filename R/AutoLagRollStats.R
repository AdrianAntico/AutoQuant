#' AutoLagRollStats
#'
#' AutoLagRollStats Builds lags and a large variety of rolling statistics with options to generate them for hierarchical categorical interactions.
#' @author Adrian Antico
#' @family Feature Engineering
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
#' @param MARollWindows A numeric vector of the specific rolling statistics window sizes you want to utilize in the calculations.
#' @param SDRollWindows  A numeric vector of Standard Deviation rolling statistics window sizes you want to utilize in the calculations.
#' @param SkewRollWindows  A numeric vector of Skewness rolling statistics window sizes you want to utilize in the calculations.
#' @param KurtRollWindows  A numeric vector of Kurtosis rolling statistics window sizes you want to utilize in the calculations.
#' @param QuantileRollWindows A numeric vector of Quantile rolling statistics window sizes you want to utilize in the calculations.
#' @param Quantiles_Selected Select from the following c("q5", "q10", "q15", "q20", "q25", "q30", "q35", "q40", "q45", "q50", "q55", "q60"," q65", "q70", "q75", "q80", "q85", "q90", "q95")
#' @param RollOnLag1 Set to FALSE to build rolling stats off of target columns directly or set to TRUE to build the rolling stats off of the lag-1 target
#' @param Type List either "Lag" if you want features built on historical values or "Lead" if you want features built on future values
#' @param SimpleImpute Set to TRUE for factor level imputation of "0" and numeric imputation of -1
#' @param Debug Set to TRUE to get a print of which steps are running
#' @return data.table of original data plus created lags, rolling stats, and time between event lags and rolling stats
#' @examples
#'
#' # Create fake Panel Data----
#' Count <- 1L
#' for(Level in LETTERS) {
#'   datatemp <- RemixAutoML::FakeDataGenerator(
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
#'     data <- data.table::rbindlist(list(data, data.table::copy(datatemp)))
#'   }
#'   Count <- Count + 1L
#' }
#'
#' # Add scoring records
#' data <- RemixAutoML::AutoLagRollStats(
#'
#'   # Data
#'   data                 = data,
#'   DateColumn           = "DateTime",
#'   Targets              = "Adrian",
#'   HierarchyGroups      = NULL,
#'   IndependentGroups    = c("Factor1"),
#'   TimeUnitAgg          = "days",
#'   TimeGroups           = c("days", "weeks", "months", "quarters"),
#'   TimeBetween          = NULL,
#'   TimeUnit             = "days",
#'
#'   # Services
#'   RollOnLag1           = TRUE,
#'   Type                 = "Lag",
#'   SimpleImpute         = TRUE,
#'
#'   # Calculated Columns
#'   Lags                 = list("days" = c(seq(1,5,1)), "weeks" = c(seq(1,3,1)), "months" = c(seq(1,2,1)), "quarters" = c(seq(1,2,1))),
#'   MA_RollWindows       = list("days" = c(seq(1,5,1)), "weeks" = c(seq(1,3,1)), "months" = c(seq(1,2,1)), "quarters" = c(seq(1,2,1))),
#'   SD_RollWindows       = NULL,
#'   Skew_RollWindows     = NULL,
#'   Kurt_RollWindows     = NULL,
#'   Quantile_RollWindows = NULL,
#'   Quantiles_Selected   = NULL,
#'   Debug                = FALSE)
#' @export
AutoLagRollStats <- function(data,
                             Targets              = NULL,
                             HierarchyGroups      = NULL,
                             IndependentGroups    = NULL,
                             DateColumn           = NULL,
                             TimeUnit             = "day",
                             TimeUnitAgg          = "day",
                             TimeGroups           = "day",
                             TimeBetween          = NULL,
                             RollOnLag1           = TRUE,
                             Type                 = "Lag",
                             SimpleImpute         = TRUE,
                             Lags                 = c(1:5),
                             MA_RollWindows       = c(2,5,10),
                             SD_RollWindows       = c(5,10),
                             Skew_RollWindows     = c(5,10),
                             Kurt_RollWindows     = c(5,10),
                             Quantile_RollWindows = c(10),
                             Quantiles_Selected   = c("q25","q75"),
                             Debug                = FALSE) {

  # Turn on full speed----
  if(parallel::detectCores() > 10) data.table::setDTthreads(threads = max(1L, parallel::detectCores() - 2L)) else data.table::setDTthreads(threads = max(1L, parallel::detectCores()))

  # Define args----
  RollFunctions <- c()
  if(!is.null(MA_RollWindows)) RollFunctions <- c(RollFunctions,"mean")
  if(!is.null(SD_RollWindows)) RollFunctions <- c(RollFunctions,"sd")
  if(!is.null(Skew_RollWindows)) RollFunctions <- c(RollFunctions,"skew")
  if(!is.null(Kurt_RollWindows)) RollFunctions <- c(RollFunctions,"kurt")
  if(!is.null(Quantiles_Selected)) RollFunctions <- c(RollFunctions,Quantiles_Selected)
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

  # Ensure date column is proper----
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
  if(is.null(IndependentGroups) & is.null(HierarchyGroups)) {

    # Initialize counter----
    counter <- 0L

    # Loop through various time aggs----
    for(timeaggs in TimeGroups) {

      # Increment counter----
      counter <- counter + 1L

      # Copy data----
      tempData <- data.table::copy(data)

      # Check time scale----
      if(counter > 1) {

        # Floor Date column to timeagg level----
        data.table::set(tempData, j = eval(DateColumn), value = lubridate::floor_date(x = tempData[[eval(DateColumn)]], unit = timeaggs))

        # Agg by date column----
        tempData <- tempData[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c(eval(Targets)), keyby = c(eval(DateColumn))]

        # Ensure TimeBetween is null for aggregated data----
        if(!is.null(TimeBetween)) TimeBetween <- NULL

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
          timeDiffTarget  = TimeBetween,
          timeAgg         = timeaggs,
          WindowingLag    = RollOnLag1,
          Type            = Type,
          SimpleImpute    = SimpleImpute)

      } else {

        # lags            = if(is.list(Lags))                 Lags[[timeaggs]]                 else Lags
        # periods         = if(is.list(MA_RollWindows))       MA_RollWindows[[timeaggs]]       else MA_RollWindows
        # SDperiods       = if(is.list(SD_RollWindows))       SD_RollWindows[[timeaggs]]       else SD_RollWindows
        # Skewperiods     = if(is.list(Skew_RollWindows))     Skew_RollWindows[[timeaggs]]     else Skew_RollWindows
        # Kurtperiods     = if(is.list(Kurt_RollWindows))     Kurt_RollWindows[[timeaggs]]     else Kurt_RollWindows
        # Quantileperiods = if(is.list(Quantile_RollWindows)) Quantile_RollWindows[[timeaggs]] else Quantile_RollWindows
        # targets         = Targets
        # groupingVars    = NULL
        # sortDateName    = DateColumn
        # timeDiffTarget  = TimeBetween
        # timeAgg         = timeaggs
        # WindowingLag    = RollOnLag1
        # Type            = Type
        # SimpleImpute    = SimpleImpute

        # Build features----
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
          groupingVars    = NULL,
          sortDateName    = DateColumn,
          timeDiffTarget  = TimeBetween,
          timeAgg         = timeaggs,
          WindowingLag    = RollOnLag1,
          Type            = Type,
          SimpleImpute    = SimpleImpute)
      }

      # Check if timeaggs is same of TimeUnit----
      if(Counter > 1L) {
        data[, TEMPDATE := lubridate::floor_date(get(DateColumn), unit = eval(timeaggs))]
        data <- merge(
          data, tempData[, .SD, .SDcols = c(eval(DateColumn),setdiff(names(tempData),names(data)))],
          by.x = c("TEMPDATE"),
          by.y = c(eval(DateColumn)),
          all.x = TRUE)
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

        # Check if timeaggs is same of TimeUnitAgg----
        if(Counter > 1L) {

          # Aggregate tempData and tempRegs to correct dimensional level----
          tempData <- data[, .SD, .SDcols = c(eval(Targets), eval(DateColumn), eval(Fact))]

          # Agg by date column----
          if(timeaggs != "raw") {
            data.table::set(tempData, j = eval(DateColumn), value = lubridate::floor_date(x = tempData[[eval(DateColumn)]], unit = timeaggs))
            tempData <- tempData[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c(eval(Targets)), keyby = c(eval(DateColumn),eval(Fact))]
          }

          # Ensure TimeBetween is null for aggregated data----
          if(!is.null(TimeBetween)) TimeBetween <- NULL

          # Build GDL Features----
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
            groupingVars    = Fact,
            sortDateName    = DateColumn,
            timeDiffTarget  = TimeBetween,
            timeAgg         = timeaggs,
            WindowingLag    = RollOnLag1,
            Type            = Type,
            SimpleImpute    = SimpleImpute)

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
            timeDiffTarget  = TimeBetween,
            timeAgg         = timeaggs,
            WindowingLag    = RollOnLag1,
            Type            = Type,
            SimpleImpute    = SimpleImpute)
        }

        # Check if timeaggs is same of TimeUnit----
        if(Counter > 1L) {
          data[, TEMPDATE := lubridate::floor_date(get(DateColumn), unit = eval(timeaggs))]
          data <- merge(
            data,
            tempData[, .SD, .SDcols = c(eval(Fact),eval(DateColumn),setdiff(names(tempData),names(data)))],
            by.x = c(eval(Fact),"TEMPDATE"),
            by.y = c(eval(Fact),eval(DateColumn)), all.x = TRUE)
          data.table::set(data, j = "TEMPDATE", value = NULL)
        }
      }
    }
  }

  # Debugging----
  if(Debug) print("AutoLagRollStats: Indep")

  # Single categoricals at a time AND no hierarchical: if there are hierarchical the single cats will be handled above----
  if(!is.null(IndependentGroups) & is.null(HierarchyGroups)) {

    # Loop through IndependentGroups----
    Counter <- 0L
    for(Fact in IndependentGroups) {

      # Loop through all TimeGroups----
      for(timeaggs in TimeGroups) {

        # Counter incrementing
        Counter <- Counter + 1L

        # Copy data----
        tempData <- data.table::copy(data)

        # Check if timeaggs is same of TimeUnit----
        if(Counter > 1L) {

          # Floor Date column to timeagg level----
          data.table::set(tempData, j = eval(DateColumn), value = lubridate::floor_date(x = tempData[[eval(DateColumn)]], unit = timeaggs))

          # Agg by date column----
          tempData <- tempData[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c(eval(Targets)), keyby = c(eval(DateColumn),eval(Fact))]

          # Ensure TimeBetween is null for aggregated data----
          if(!is.null(TimeBetween)) TimeBetween <- NULL

          # Build GDL Features----
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
            groupingVars    = Fact,
            sortDateName    = DateColumn,
            timeDiffTarget  = TimeBetween,
            timeAgg         = timeaggs,
            WindowingLag    = RollOnLag1,
            Type            = Type,
            SimpleImpute    = SimpleImpute)

        } else {

          # Set up for binary search instead of vector scan----
          data.table::setkeyv(x = data, cols = c(eval(Fact),eval(DateColumn)))

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
            timeDiffTarget  = TimeBetween,
            timeAgg         = timeaggs,
            WindowingLag    = RollOnLag1,
            Type            = Type,
            SimpleImpute    = SimpleImpute)
        }

        # Check if timeaggs is same of TimeUnit----
        if(Counter > 1L) {
          data[, TEMPDATE := lubridate::floor_date(get(DateColumn), unit = eval(timeaggs))]
          data <- merge(
            data, tempData[, .SD, .SDcols = c(eval(Fact),eval(DateColumn),setdiff(names(tempData),names(data)))],
            by.x = c(eval(Fact),"TEMPDATE"),
            by.y = c(eval(Fact),eval(DateColumn)),
            all.x = TRUE)
          data.table::set(data, j = "TEMPDATE", value = NULL)
        }
      }
    }
  }

  # Simple impute missed----
  for(miss in seq_len(ncol(data))) {
    data.table::set(data, i = which(is.na(data[[miss]])), j = miss, value = -1)
  }

  # Return data----
  if("TEMPDATE" %chin% names(data)) data.table::set(data, j = "TEMPDATE", value = NULL)
  return(data)
}

#' AutoLagRollStatsScoring
#'
#' AutoLagRollStatsScoring Builds lags and a large variety of rolling statistics with options to generate them for hierarchical categorical interactions.
#' @author Adrian Antico
#' @family Feature Engineering
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
#' @param MARollWindows A numeric vector of the specific rolling statistics window sizes you want to utilize in the calculations.
#' @param SDRollWindows  A numeric vector of Standard Deviation rolling statistics window sizes you want to utilize in the calculations.
#' @param SkewRollWindows  A numeric vector of Skewness rolling statistics window sizes you want to utilize in the calculations.
#' @param KurtRollWindows  A numeric vector of Kurtosis rolling statistics window sizes you want to utilize in the calculations.
#' @param QuantileRollWindows A numeric vector of Quantile rolling statistics window sizes you want to utilize in the calculations.
#' @param Quantiles_Selected Select from the following c("q5", "q10", "q15", "q20", "q25", "q30", "q35", "q40", "q45", "q50", "q55", "q60"," q65", "q70", "q75", "q80", "q85", "q90", "q95")
#' @param RollOnLag1 Set to FALSE to build rolling stats off of target columns directly or set to TRUE to build the rolling stats off of the lag-1 target
#' @param Type List either "Lag" if you want features built on historical values or "Lead" if you want features built on future values
#' @param SimpleImpute Set to TRUE for factor level imputation of "0" and numeric imputation of -1
#' @param Debug Set to TRUE to get a print out of which step you are on
#' @return data.table of original data plus created lags, rolling stats, and time between event lags and rolling stats
#' @examples
#'
#' # Create fake Panel Data----
#' Count <- 1L
#' for(Level in LETTERS) {
#'   datatemp <- RemixAutoML::FakeDataGenerator(
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
#'     data <- data.table::rbindlist(list(data, data.table::copy(datatemp)))
#'   }
#'   Count <- Count + 1L
#' }
#'
#' # Create ID columns to know which records to score
#' data[, ID := .N:1L, by = "Factor1"]
#' data.table::set(data, i = which(data[["ID"]] == 2L), j = "ID", value = 1L)
#'
#' # Score records
#' data <- RemixAutoML::AutoLagRollStatsScoring(
#'
#'   # Data
#'   data                 = data,
#'   RowNumsID            = "ID",
#'   RowNumsKeep          = 1,
#'   DateColumn           = "DateTime",
#'   Targets              = "Adrian",
#'   HierarchyGroups      = c("Store","Dept"),
#'   IndependentGroups    = NULL,
#'
#'   # Services
#'   TimeBetween          = NULL,
#'   TimeGroups           = c("days", "weeks", "months"),
#'   TimeUnit             = "day",
#'   TimeUnitAgg          = "day",
#'   RollOnLag1           = TRUE,
#'   Type                 = "Lag",
#'   SimpleImpute         = TRUE,
#'
#'   # Calculated Columns
#'   Lags                  = list("days" = c(seq(1,5,1)), "weeks" = c(seq(1,3,1)), "months" = c(seq(1,2,1))),
#'   MA_RollWindows        = list("days" = c(seq(1,5,1)), "weeks" = c(seq(1,3,1)), "months" = c(seq(1,2,1))),
#'   SD_RollWindows        = list("days" = c(seq(1,5,1)), "weeks" = c(seq(1,3,1)), "months" = c(seq(1,2,1))),
#'   Skew_RollWindows      = list("days" = c(seq(1,5,1)), "weeks" = c(seq(1,3,1)), "months" = c(seq(1,2,1))),
#'   Kurt_RollWindows      = list("days" = c(seq(1,5,1)), "weeks" = c(seq(1,3,1)), "months" = c(seq(1,2,1))),
#'   Quantile_RollWindows  = list("days" = c(seq(1,5,1)), "weeks" = c(seq(1,3,1)), "months" = c(seq(1,2,1))),
#'   Quantiles_Selected    = c("q5","q10","q95"),
#'   Debug                 = FALSE)
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
                                    Debug                = FALSE) {

  # Turn on full speed----
  if(parallel::detectCores() > 10) data.table::setDTthreads(threads = max(1L, parallel::detectCores() - 2L)) else data.table::setDTthreads(threads = max(1L, parallel::detectCores()))

  # Define args----
  RollFunctions <- c()
  if(!is.null(MA_RollWindows)) RollFunctions <- c(RollFunctions,"mean")
  if(!is.null(SD_RollWindows)) RollFunctions <- c(RollFunctions,"sd")
  if(!is.null(Skew_RollWindows)) RollFunctions <- c(RollFunctions,"skew")
  if(!is.null(Kurt_RollWindows)) RollFunctions <- c(RollFunctions,"kurt")
  if(!is.null(Quantiles_Selected)) RollFunctions <- c(RollFunctions,Quantiles_Selected)
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

  # Ensure date column is proper----
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
  if(Debug) print("AutoLagRollStatsScoring: No Categoricals")

  # No Categoricals----
  if(is.null(HierarchyGroups) & is.null(IndependentGroups)) {

    # Initialize counter----
    Counter <- 0L

    # Loop through the time aggs----
    for(timeaggs in TimeGroups) {

      # Increment----
      Counter <- Counter + 1L

      # Check if timeaggs is same of TimeUnitAgg----
      if(Counter > 1L) {

        # Copy data----
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

        # Ensure TimeBetween is null for aggregated data----
        if(!is.null(TimeBetween)) TimeBetween <- NULL

        # Build GDL Features----
        tempData <- Partial_DT_GDL_Feature_Engineering(
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
          timeDiffTarget = TimeBetween,
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
        KeepData <- Partial_DT_GDL_Feature_Engineering(
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
          timeDiffTarget = TimeBetween,
          timeAgg = timeaggs,
          WindowingLag = RollOnLag1,
          Type = Type,
          Timer = FALSE,
          SimpleImpute = SimpleImpute,
          AscRowByGroup = RowNumsID,
          RecordsKeep = RowNumsKeep,
          AscRowRemove = TRUE)
      }

      # When Fact changes, dates are different - find out where the date changes
      if(Counter > 1L) {

        # I need to match up date aggregation to join properly----
        if(timeaggs != TimeUnitAgg) {
          KeepData <- merge(
            x = data.table::set(KeepData, j = "TEMPDATE", value = lubridate::floor_date(KeepData[[eval(DateColumn)]], unit = timeaggs)),
            y = data.table::set(tempData, j = c(setdiff(names(tempData),c("TEMPDATE",setdiff(names(tempData),names(KeepData))))), value = NULL),
            by = c("TEMPDATE"),
            all.x = TRUE)
        }

        # I need to match up date aggregation to join properly----
        if(timeaggs == TimeUnitAgg) {
          data.table::set(KeepData, j = "TEMPDATE", value = KeepData[[eval(DateColumn)]])
          KeepData <- merge(
            x = KeepData,
            y = data.table::set(tempData, j = c(setdiff(names(tempData),c("TEMPDATE",setdiff(names(tempData),names(KeepData))))), value = NULL),
            by = c("TEMPDATE"),
            all.x = TRUE)
        }
      }
    }
  }

  # Debugging----
  if(Debug) print("AutoLagRollStatsScoring: Hierarchies")

  # Hierarchy Categoricals----
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

          # Ensure TimeBetween is null for aggregated data----
          if(!is.null(TimeBetween)) TimeBetween <- NULL

          # Build features----
          tempData <- Partial_DT_GDL_Feature_Engineering(
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

        } else {

          # Build features----
          KeepData <- Partial_DT_GDL_Feature_Engineering(
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
          if(timeaggs != TimeUnitAgg & timeaggs != "raw") {
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

          # I need to match up date aggregation to join properly----
          if(timeaggs == TimeUnitAgg) {
            data.table::set(KeepData, j = "TEMPDATE", value = KeepData[[eval(DateColumn)]])
            KeepData <- merge(
              x = KeepData,
              y = data.table::set(tempData, j = c(setdiff(names(tempData),c(eval(Fact),"TEMPDATE",setdiff(names(tempData),names(KeepData))))), value = NULL),
              by = c(eval(Fact),"TEMPDATE"),
              all.x = TRUE)
          }
        }
      }
    }
  }

  # Debugging----
  if(Debug) print("AutoLagRollStatsScoring: Independent")

  # Single categoricals at a time----
  if(!is.null(IndependentGroups) & is.null(HierarchyGroups)) {

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
          if(timeaggs != TimeGroups[1]) tempData[, TEMPDATE := lubridate::floor_date(x = get(DateColumn), unit = timeaggs)]

          # Ensure Targets is numeric - someimes comes in as list----
          for(tar in Targets) if(!is.numeric(tempData[[eval(tar)]])) data.table::set(tempData, j = eval(tar), value = as.numeric(tempData[[eval(tar)]]))

          # Dim and Time Aggregation----
          tempData <- tempData[, c(min(get(RowNumsID)), lapply(.SD, mean, na.rm = TRUE)), .SDcols = c(eval(Targets)), by = c("TEMPDATE",eval(Fact))]
          data.table::setnames(tempData, c("V1"), c(RowNumsID))

          # Ensure TimeBetween is null for aggregated data----
          if(!is.null(TimeBetween)) TimeBetween <- NULL

          # Build features----
          tempData <- Partial_DT_GDL_Feature_Engineering(
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
            timeDiffTarget  = TimeBetween,
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
          KeepData <- Partial_DT_GDL_Feature_Engineering(
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
            timeDiffTarget  = TimeBetween,
            timeAgg         = timeaggs,
            WindowingLag    = RollOnLag1,
            Type            = Type,
            Timer           = FALSE,
            SimpleImpute    = SimpleImpute,
            AscRowByGroup   = RowNumsID,
            RecordsKeep     = RowNumsKeep,
            AscRowRemove    = TRUE)
        }

        # Combine data sets----
        if(Counter > 1L) {
          KeepData[, TEMPDATE := lubridate::floor_date(get(DateColumn), unit = eval(timeaggs))]
          KeepData <- merge(
            x = KeepData,
            y = data.table::set(tempData, j = c(setdiff(names(tempData),c(eval(Fact),"TEMPDATE",setdiff(names(tempData),names(KeepData))))), value = NULL),
            by = c(eval(Fact),"TEMPDATE"),
            all.x = TRUE)
          data.table::set(KeepData, j = "TEMPDATE", value = NULL)
        }
      }
    }
  }

  # Simple impute missed----
  for(miss in seq_len(ncol(KeepData))) data.table::set(KeepData, i = which(is.na(KeepData[[miss]])), j = miss, value = -1)

  # Return data----
  if("TEMPDATE" %chin% names(KeepData)) data.table::setnames(KeepData, "TEMPDATE", DateColumn)
  return(KeepData)
}
