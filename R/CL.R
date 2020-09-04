#' @title CLTrainer
#'
#' @description CLTrainer is a forecasting model for chain ladder style forecasting
#'
#' @author Adrian Antico
#' @family Automated Time Series
#' @param data data
#' @param PartitionRatios R
#' @param BaseFunnelMeasure E into the future. You should have two months into the future of values of BaseFunnelMeasure
#' @param ConversionMeasure E
#' @param ConversionRateMeasure C
#' @param CohortPeriodsVariable N
#' @param TargetVariable T
#' @param CalendarDate T
#' @param CohortDate T
#' @param TruncateDate N
#' @param MaxCohortPeriods T
#' @param TimeUnit B
#' @param TransformTargetVariable T
#' @param TransformMethods C
#' @param AnomalyDetection P
#' @param Jobs D
#' @param CalendarTimeGroups T
#' @param CohortTimeGroups T
#' @param ModelPath P
#' @param MetaDataPath P
#' @param ModelID A
#' @param NumOfParDepPlots T
#' @param ReturnModelObjects S
#' @param SaveModelObjects S
#' @param TaskType t
#' @param NumGPUs N
#' @param EvaluationMetric T
#' @param LossFunction U
#' @param NumOfParDepPlots N
#' @param MetricPeriods N
#' @param DT_Threads N
#' @param ImputeRollStats C
#' @param CohortHolidayLags c
#' @param CohortHolidayMovingAverages c
#' @param CalendarHolidayLags c
#' @param CalendarHolidayMovingAverages c
#' @param CalendarLags L
#' @param CalendarMovingAverages L
#' @param CalendarStandardDeviations L
#' @param CalendarSkews L
#' @param CalendarKurts L
#' @param CalendarQuantiles L
#' @param CalendarQuantilesSelected S
#' @param CohortLags L
#' @param CohortMovingAverages L
#' @param CohortStandardDeviations L
#' @param CohortSkews L
#' @param CohortKurts L
#' @param CohortQuantiles L
#' @param CohortQuantilesSelected S
#' @param CalendarVariables w
#' @param HolidayGroups c
#' @param PassInGrid D
#' @param GridTune S
#' @param BaselineComparison S
#' @param MaxModelsInGrid N
#' @param MaxRunMinutes M
#' @param MaxRunsWithoutNewWinner N
#' @param Trees B
#' @param Depth B
#' @param LearningRate B
#' @param L2_Leaf_Reg R
#' @param RSM C
#' @param BootStrapType R
#' @param GrowPolicy R
#' @return S
#' @export
CLTrainer <- function(data,
                      PartitionRatios = c(0.70,0.20,0.10),
                      BaseFunnelMeasure = NULL,
                      ConversionMeasure = NULL,
                      ConversionRateMeasure = NULL,
                      CohortPeriodsVariable = NULL,
                      CalendarDate = NULL,
                      CohortDate = NULL,
                      TruncateDate = NULL,
                      TimeUnit = c("day"),
                      CalendarTimeGroups = c("day","week","month"),
                      CohortTimeGroups = c("day","week","month"),
                      TransformTargetVariable = TRUE,
                      TransformMethods = c("Identity","YeoJohnson"),
                      AnomalyDetection = list(tstat_high = 3, tstat_low = -2),
                      Jobs = c("Evaluate","Train"),
                      SaveModelObjects = TRUE,
                      ModelID = "Segment_ID",
                      ModelPath = NULL,
                      MetaDataPath = NULL,
                      TaskType = "CPU",
                      NumGPUs = 1,
                      DT_Threads = max(1L, parallel::detectCores()),
                      EvaluationMetric = "RMSE",
                      LossFunction = "RMSE",
                      NumOfParDepPlots = 1L,
                      MetricPeriods = 50L,
                      CalendarVariables = c("wday","mday","yday","week","isoweek","month","quarter","year"),
                      HolidayGroups = c("USPublicHolidays","EasterGroup","ChristmasGroup","OtherEcclesticalFeasts"),
                      ImputeRollStats = -0.001,
                      CohortHolidayLags = c(1L, 2L, 7L),
                      CohortHolidayMovingAverages = c(3L, 7L),
                      CalendarHolidayLags = c(1L, 2L, 7L),
                      CalendarHolidayMovingAverages = c(3L, 7L),
                      CalendarLags = list("day" = c(1L, 7L, 21L), "week" = c(1L, 4L, 52L), "month" = c(1L, 6L, 12L)),
                      CalendarMovingAverages = list("day" = c(1L, 7L, 21L), "week" = c(1L, 4L, 52L), "month" = c(1L, 6L, 12L)),
                      CalendarStandardDeviations = NULL,
                      CalendarSkews = NULL,
                      CalendarKurts = NULL,
                      CalendarQuantiles = NULL,
                      CalendarQuantilesSelected = "q50",
                      CohortLags = list("day" = c(1L, 7L, 21L), "week" = c(1L, 4L, 52L), "month" = c(1L, 6L, 12L)),
                      CohortMovingAverages = list("day" = c(1L, 7L, 21L), "week" = c(1L, 4L, 52L), "month" = c(1L, 6L, 12L)),
                      CohortStandardDeviations = NULL,
                      CohortSkews = NULL,
                      CohortKurts = NULL,
                      CohortQuantiles = NULL,
                      CohortQuantilesSelected = "q50",
                      PassInGrid = NULL,
                      GridTune = FALSE,
                      BaselineComparison = "default",
                      MaxModelsInGrid = 25L,
                      MaxRunMinutes = 180L,
                      MaxRunsWithoutNewWinner = 10L,
                      Trees = 3000L,
                      Depth = seq(4L, 8L, 1L),
                      LearningRate = seq(0.01,0.10,0.01),
                      L2_Leaf_Reg = seq(1.0, 10.0, 1.0),
                      RSM = c(0.80, 0.85, 0.90, 0.95, 1.0),
                      BootStrapType = c("Bayesian", "Bernoulli", "Poisson", "MVS", "No"),
                      GrowPolicy = c("SymmetricTree", "Depthwise", "Lossguide")) {

  # Init: Get catboost loaded----
  loadNamespace(package = "catboost")

  # Init: DT_Threads----
  if(parallel::detectCores() > 10) data.table::setDTthreads(threads = max(1L, parallel::detectCores() - 2L)) else data.table::setDTthreads(threads = max(1L, parallel::detectCores()))

  # Ensure ModelPath and MetaDataPath exists----
  if(!is.null(ModelPath)) if(!dir.exists(file.path(normalizePath(ModelPath)))) dir.create(normalizePath(ModelPath))
  if(!is.null(MetaDataPath)) if(!is.null(MetaDataPath)) if(!dir.exists(file.path(normalizePath(MetaDataPath)))) dir.create(normalizePath(MetaDataPath))

  # Args List----
  ArgsList <- list()
  if(!is.null(PartitionRatios)) ArgsList[["PartitionRatios"]] <- PartitionRatios else ArgsList[["PartitionRatios"]] <- c(0.70,0.20,0.10)
  ArgsList[["Algorithm"]] <- "catboost"
  ArgsList[["BaseFunnelMeasure"]] <- BaseFunnelMeasure
  ArgsList[["ConversionMeasure"]] <- ConversionMeasure
  ArgsList[["CohortPeriodsVariable"]] <- CohortPeriodsVariable
  ArgsList[["CalendarDate"]] <- CalendarDate
  ArgsList[["CohortDate"]] <- CohortDate
  if(tolower(TimeUnit) %chin% c("day","days")) {
    TimeUnit <- "days"
  } else if(tolower(TimeUnit) %chin% c("week","weeks")) {
    TimeUnit <- "weeks"
  } else if(tolower(TimeUnit) %chin% c("month","months")) {
    TimeUnit <- "months"
  } else if(tolower(TimeUnit) %chin% c("quarter","quarters")) {
    TimeUnit <- "quarters"
  } else if(tolower(TimeUnit) %chin% c("year","years")) {
    TimeUnit <- "years"
  }
  ArgsList[["AnomalyDetection"]] <- AnomalyDetection
  ArgsList[["TimeUnit"]] <- TimeUnit
  ArgsList[["CalendarTimeGroups"]] <- CalendarTimeGroups
  ArgsList[["CohortTimeGroups"]] <- CohortTimeGroups
  ArgsList[["Jobs"]] <- Jobs
  ArgsList[["ModelID"]] <- ModelID
  ArgsList[["ModelPath"]] <- ModelPath
  if(is.null(MetaDataPath)) if(!is.null(ModelPath)) MetaDataPath <- ModelPath
  ArgsList[["MetaDataPath"]] <- MetaDataPath
  ArgsList[["TaskType"]] <- TaskType
  ArgsList[["NumGPUs"]] <- NumGPUs
  ArgsList[["DT_Threads"]] <- DT_Threads
  ArgsList[["EvaluationMetric"]] <- EvaluationMetric
  ArgsList[["LossFunction"]] <- LossFunction
  ArgsList[["CalendarVariables"]] <- CalendarVariables
  ArgsList[["HolidayGroups"]] <- HolidayGroups
  ArgsList[["ImputeRollStats"]] <- ImputeRollStats
  ArgsList[["CohortHolidayLags"]] <- CohortHolidayLags
  ArgsList[["CohortHolidayMovingAverages"]] <- CohortHolidayMovingAverages
  ArgsList[["CalendarHolidayLags"]] <- CalendarHolidayLags
  ArgsList[["CalendarHolidayMovingAverages"]] <- CalendarHolidayMovingAverages
  ArgsList[["CalendarLags"]] <- CalendarLags
  ArgsList[["CalendarMovingAverages"]] <- CalendarMovingAverages
  ArgsList[["CalendarStandardDeviations"]] <- CalendarStandardDeviations
  ArgsList[["CalendarSkews"]] <- CalendarSkews
  ArgsList[["CalendarKurts"]] <- CalendarKurts
  ArgsList[["CalendarQuantiles"]] <- CalendarQuantiles
  ArgsList[["CalendarQuantilesSelected"]] <- CalendarQuantilesSelected
  ArgsList[["CohortLags"]] <- CohortLags
  ArgsList[["CohortMovingAverages"]] <- CohortMovingAverages
  ArgsList[["CohortStandardDeviations"]] <- CohortStandardDeviations
  ArgsList[["CohortSkews"]] <- CohortSkews
  ArgsList[["CohortKurts"]] <- CohortKurts
  ArgsList[["CohortQuantiles"]] <- CohortQuantiles
  ArgsList[["CohortQuantilesSelected"]] <- CohortQuantilesSelected
  ArgsList[["GridTune"]] <- GridTune
  ArgsList[["BaselineComparison"]] <- BaselineComparison
  ArgsList[["MaxModelsInGrid"]] <- MaxModelsInGrid
  ArgsList[["MaxRunMinutes"]] <- MaxRunMinutes
  ArgsList[["MaxRunsWithoutNewWinner"]] <- MaxRunsWithoutNewWinner
  ArgsList[["Trees"]] <- Trees
  ArgsList[["Depth"]] <- Depth
  ArgsList[["LearningRate"]] <- LearningRate
  ArgsList[["L2_Leaf_Reg"]] <- L2_Leaf_Reg
  ArgsList[["RSM"]] <- RSM
  ArgsList[["BootStrapType"]] <- BootStrapType
  ArgsList[["GrowPolicy"]] <- GrowPolicy
  ArgsList[["PassInGrid"]] <- PassInGrid

  # Special Args----
  ArgsList[[paste0("Min","-",eval(CalendarDate))]] <- data[, min(get(CalendarDate))][[1L]]
  ArgsList[[paste0("Max","-",eval(CalendarDate))]] <- data[, max(get(CalendarDate))][[1L]]

  # Init: Define SaveTimers() function----
  SaveTimers <- function(SaveModelObjectss = SaveModelObjects, procs = proc, TimerDataEvals = TimerDataEval, TimerDataTrains = TimerDataTrain, MetaDataPaths = MetaDataPath, ModelIDs = ModelID) {
    if(SaveModelObjectss) {
      if(procs %chin% c("evaluate","eval")) {
        data.table::fwrite(TimerDataEvals[Process != "a"], file = file.path(normalizePath(MetaDataPaths), paste0(ModelIDs, "_Eval_Timer.csv")))
      } else if(procs %chin% c("training","train")) {
        data.table::fwrite(TimerDataTrains[Process != "a"], file = file.path(normalizePath(MetaDataPaths), paste0(ModelIDs, "_Train_Timer.csv")))
      }
    }
  }

  # Init: Create timers----
  TimerDataEval <- data.table::data.table(Process = rep("a", 25L), Time = rep(999, 25L))
  TimerDataTrain <- data.table::data.table(Process = rep("a", 25L), Time = rep(999, 25L))

  # DM: Convert data to data.table----
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  # DM: Type Casting CalendarDate and CohortDate to Date or POSIXct----
  if(!(tolower(TimeUnit) %chin% c("1min","5min","10min","15min","30min","hour"))) {
    if(is.character(data[[eval(CalendarDate)]])) {
      x <- data[1L, get(CalendarDate)]
      x1 <- lubridate::guess_formats(x, orders = c("mdY", "BdY", "Bdy", "bdY", "bdy", "mdy", "dby", "Ymd", "Ydm"))
      data[, eval(CalendarDate) := as.Date(get(CalendarDate), tryFormats = x1)]
    }
    if(is.character(data[[eval(CohortDate)]])) {
      x <- data[1L, get(CohortDate)]
      x1 <- lubridate::guess_formats(x, orders = c("mdY", "BdY", "Bdy", "bdY", "bdy", "mdy", "dby", "Ymd", "Ydm"))
      data[, eval(CohortDate) := as.Date(get(CohortDate), tryFormats = x1)]
    }
  } else {
    data[, eval(CalendarDate) := as.POSIXct(get(CalendarDate))]
    data[, eval(CohortDate) := as.POSIXct(get(CohortDate))]
  }

  # FE: Create CohortPeriodsVariable----
  if(is.null(CohortPeriodsVariable)) {
    data[, CohortPeriods := as.numeric(difftime(time1 = get(CohortDate), time2 = get(CalendarDate), units = eval(TimeUnit)))]
    CohortPeriodsVariable <- "CohortPeriods"
    ArgsList[["CohortPeriodsVariable"]] <- CohortPeriodsVariable
  }

  # DM: ConversionRateMeasure if NULL----
  if(is.null(ConversionRateMeasure)) {
    data[, Rate := get(ConversionMeasure) / (get(BaseFunnelMeasure[1]) + 1)]
    ConversionRateMeasure <- "Rate"
  } else {
    if(ConversionRateMeasure != "Rate") data.table::setnames(data, eval(ConversionRateMeasure), "Rate")
  }

  # ML Process: Train and Evaluate Models----
  for(proc in Jobs) {

    # Init: Function Similify----
    proc <- tolower(proc)

    # Copy data----
    if(proc %chin% c("evaluate","eval","evaluation")) {
      data1 <- data.table::copy(data)
    } else {
      data <- data1
      rm(data1)
    }

    # FE: CreateCalendarVariables() CalendarDate and CohortDate----
    x <- system.time(gcFirst = FALSE, data <- RemixAutoML::CreateCalendarVariables(data, DateCols = c(eval(CalendarDate), eval(CohortDate)), AsFactor = FALSE, TimeUnits = CalendarVariables))
    if(proc %chin% c("evaluate","eval","evaluation")) {
      data.table::set(TimerDataEval, i = 2L, j = "Time", value = x[[3L]])
      data.table::set(TimerDataEval, i = 2L, j = "Process", value = "# Add CalendarDate and CohortDate calendar variables----")
    } else if(proc %chin% c("training","train")) {
      data.table::set(TimerDataTrain, i = 2L, j = "Time", value = x[[3L]])
      data.table::set(TimerDataTrain, i = 2L, j = "Process", value = "# Add CalendarDate and CohortDate calendar variables----")
    }

    # Save Timers to file
    SaveTimers(SaveModelObjectss = SaveModelObjects, procs = proc, TimerDataEvals = TimerDataEval, TimerDataTrains = TimerDataTrain, MetaDataPaths = MetaDataPath, ModelIDs = ModelID)

    # FE: CreateHolidayVariables() CalendarDate----
    x <- system.time(gcFirst = FALSE, data <- RemixAutoML::CreateHolidayVariables(data, DateCols = eval(CalendarDate), HolidayGroups = HolidayGroups, Holidays = NULL, GroupingVars = NULL, Print = FALSE))
    data.table::setnames(data, old = "HolidayCounts", new = paste0(CalendarDate,"HolidayCounts"))
    if(proc %chin% c("evaluate","eval","evaluation")) {
      data.table::set(TimerDataEval, i = 3L, j = "Time", value = x[[3L]])
      data.table::set(TimerDataEval, i = 3L, j = "Process", value = "# Add CalendarDate holiday variables----")
    } else if(proc %chin% c("training","train")) {
      data.table::set(TimerDataTrain, i = 3L, j = "Time", value = x[[3L]])
      data.table::set(TimerDataTrain, i = 3L, j = "Process", value = "# Add CalendarDate holiday variables----")
    }

    # Save Timers to file
    SaveTimers(SaveModelObjectss = SaveModelObjects, procs = proc, TimerDataEvals = TimerDataEval, TimerDataTrains = TimerDataTrain, MetaDataPaths = MetaDataPath, ModelIDs = ModelID)

    # FE: CreateHolidayVariables() CohortDate----
    x <- system.time(gcFirst = FALSE, data <- RemixAutoML::CreateHolidayVariables(data, DateCols = eval(CohortDate), HolidayGroups = eval(HolidayGroups), Holidays = NULL, GroupingVars = NULL, Print = FALSE))
    data.table::setnames(data, old = "HolidayCounts", new = paste0(CohortDate, "HolidayCounts"))
    if(proc %chin% c("evaluate","eval","evaluation")) {
      data.table::set(TimerDataEval, i = 4L, j = "Time", value = x[[3L]])
      data.table::set(TimerDataEval, i = 4L, j = "Process", value = "# Add CohortDate holiday variables----")
    } else if(proc %chin% c("training","train")) {
      data.table::set(TimerDataTrain, i = 4L, j = "Time", value = x[[3L]])
      data.table::set(TimerDataTrain, i = 4L, j = "Process", value = "# Add CohortDate holiday variables----")
    }

    # AnomalyDetection for all CohortDates----
    if(!is.null(AnomalyDetection)) {
      temp <- data[get(CalendarDate) == get(CohortDate), list(ConversionCheck = sum(get(ConversionMeasure)), Leads = max(get(BaseFunnelMeasure[1]))), by = eval(CalendarDate)]
      temp <- temp[, ConversionRate := ConversionCheck / (Leads + 1)][, .SD, .SDcols = c(eval(CalendarDate), "ConversionRate")]
      temp <- RemixAutoML::CreateCalendarVariables(data = temp, DateCols = eval(CalendarDate), AsFactor = FALSE, TimeUnits = "wday")
      temp <- RemixAutoML::GenTSAnomVars(data = temp, ValueCol = "ConversionRate", GroupVars = paste0(CalendarDate,"_wday"), DateVar = eval(CalendarDate), HighThreshold = AnomalyDetection$tstat_high, LowThreshold = AnomalyDetection$tstat_low, KeepAllCols = TRUE, IsDataScaled = FALSE)
      temp <- temp[, .SD, .SDcols = c(eval(CalendarDate), "AnomHigh","AnomLow")]
      if(!is.null(x)) {
        data <- merge(data, temp, by.x = eval(CohortDate), by.y = eval(CalendarDate), all.x = TRUE)
        data[is.na(AnomHigh), AnomHigh := 0]
        data[is.na(AnomLow), AnomLow := 0]
      } else {
        ArgsList[["AnomalyDetection"]] <- NULL
      }
      rm(temp)
    }

    # DM: Type Casting CalendarDate to Character to be used as a Grouping Variable----
    x <- system.time(gcFirst = FALSE, data.table::set(data, j = eval(CalendarDate), value = as.character(data[[eval(CalendarDate)]])))
    if(proc %chin% c("evaluate","eval","evaluation")) {
      data.table::set(TimerDataEval, i = 6L, j = "Time", value = x[[3L]])
      data.table::set(TimerDataEval, i = 6L, j = "Process", value = "# Convert CalendarDate to Character to treat as Cohort Group----")
    } else if(proc %chin% c("training","train")) {
      data.table::set(TimerDataTrain, i = 6L, j = "Time", value = x[[3L]])
      data.table::set(TimerDataTrain, i = 6L, j = "Process", value = "# Convert CalendarDate to Character to treat as Cohort Group----")
    }

    # Save Timers to file
    SaveTimers(SaveModelObjectss = SaveModelObjects, procs = proc, TimerDataEvals = TimerDataEval, TimerDataTrains = TimerDataTrain, MetaDataPaths = MetaDataPath, ModelIDs = ModelID)

    # DM: Sort data by CalendarDate and then by CohortPeriodsVariable----
    x <- system.time(gcFirst = FALSE, data.table::setorderv(data, cols = c(eval(CalendarDate),eval(CohortPeriodsVariable)), c(1L, 1L)))
    if(proc %chin% c("evaluate","eval","evaluation")) {
      data.table::set(TimerDataEval, i = 5L, j = "Time", value = x[[3L]])
      data.table::set(TimerDataEval, i = 5L, j = "Process", value = "# Sort data by CalendarDate and then by CohortPeriodsVariable----")
    } else if(proc %chin% c("training","train")) {
      data.table::set(TimerDataTrain, i = 5L, j = "Time", value = x[[3L]])
      data.table::set(TimerDataTrain, i = 5L, j = "Process", value = "# Sort data by CalendarDate and then by CohortPeriodsVariable----")
    }

    # Save Timers to file
    SaveTimers(SaveModelObjectss = SaveModelObjects, procs = proc, TimerDataEvals = TimerDataEval, TimerDataTrains = TimerDataTrain, MetaDataPaths = MetaDataPath, ModelIDs = ModelID)

    #----

    #----

    # FE: AutoLagRollStats() ConversionMeasure with CalendarDate as a Grouping Variable----
    if(proc %in% c("evaluate","evaluation","eval","train","training")) {
      x <- system.time(gcFirst = FALSE, data <- RemixAutoML::AutoLagRollStats(

        # Data
        data                 = data,
        DateColumn           = eval(CohortDate),
        Targets              = c(eval(ConversionMeasure), "Rate"),
        HierarchyGroups      = NULL,
        IndependentGroups    = eval(CalendarDate),
        TimeUnit             = TimeUnit,
        TimeGroups           = CohortTimeGroups,
        TimeUnitAgg          = TimeUnit,

        # Services
        TimeBetween          = NULL,
        RollOnLag1           = TRUE,
        Type                 = "Lag",
        SimpleImpute         = FALSE,

        # Calculated Columns
        Lags                 = CohortLags,
        MA_RollWindows       = CohortMovingAverages,
        SD_RollWindows       = CohortStandardDeviations,
        Skew_RollWindows     = CohortSkews,
        Kurt_RollWindows     = CohortKurts,
        Quantile_RollWindows = CohortQuantiles,
        Quantiles_Selected   = CohortQuantilesSelected,
        Debug                = TRUE))
      if(proc %chin% c("evaluate","eval")) {
        data.table::set(TimerDataEval, i = 7L, j = "Time", value = x[[3L]])
        data.table::set(TimerDataEval, i = 7L, j = "Process", value = "# Rolling stats for CohortDate with CalendarDate as a Grouping Variable----")
      } else if(proc %chin% c("training","train")) {
        data.table::set(TimerDataTrain, i = 7L, j = "Time", value = x[[3L]])
        data.table::set(TimerDataTrain, i = 7L, j = "Process", value = "# Rolling stats for CohortDate with CalendarDate as a Grouping Variable----")
      }

      # Save Timers to file
      SaveTimers(SaveModelObjectss = SaveModelObjects, procs = proc, TimerDataEvals = TimerDataEval, TimerDataTrains = TimerDataTrain, MetaDataPaths = MetaDataPath, ModelIDs = ModelID)
    }

    # FE: AutoLagRollStats() CohortDate HolidayCounts with CalendarDate as a Grouping Variable----
    if(proc %in% c("evaluate","evaluation","eval","train","training") & !is.null(CohortHolidayLags)) {
      x <- system.time(gcFirst = FALSE, data <- RemixAutoML::AutoLagRollStats(

        # Data
        data                 = data,
        DateColumn           = eval(CohortDate),
        Targets              = paste0(CohortDate, "HolidayCounts"),
        HierarchyGroups      = NULL,
        IndependentGroups    = eval(CalendarDate),
        TimeUnit             = TimeUnit,
        TimeGroups           = TimeUnit,
        TimeUnitAgg          = TimeUnit,

        # Services
        TimeBetween          = NULL,
        RollOnLag1           = TRUE,
        Type                 = "Lag",
        SimpleImpute         = FALSE,

        # Calculated Columns
        Lags                 = CohortHolidayLags,
        MA_RollWindows       = CohortHolidayMovingAverages,
        SD_RollWindows       = NULL,
        Skew_RollWindows     = NULL,
        Kurt_RollWindows     = NULL,
        Quantile_RollWindows = NULL,
        Quantiles_Selected   = NULL,
        Debug                = TRUE))
      if(proc %chin% c("evaluate","eval")) {
        data.table::set(TimerDataEval, i = 7L, j = "Time", value = x[[3L]])
        data.table::set(TimerDataEval, i = 7L, j = "Process", value = "# Rolling stats for CohortDate with CalendarDate as a Grouping Variable----")
      } else if(proc %chin% c("training","train")) {
        data.table::set(TimerDataTrain, i = 7L, j = "Time", value = x[[3L]])
        data.table::set(TimerDataTrain, i = 7L, j = "Process", value = "# Rolling stats for CohortDate with CalendarDate as a Grouping Variable----")
      }

      # DM: Type Casting CalendarDate back to Date----
      x <- system.time(gcFirst = FALSE, data.table::set(data, j = eval(CalendarDate), value = as.Date(data[[eval(CalendarDate)]])))
      if(proc %chin% c("evaluate","eval")) {
        data.table::set(TimerDataEval, i = 8L, j = "Time", value = x[[3L]])
        data.table::set(TimerDataEval, i = 8L, j = "Process", value = "# Convert CalendarDate back to Date----")
      } else if(proc %chin% c("training","train")) {
        data.table::set(TimerDataTrain, i = 8L, j = "Time", value = x[[3L]])
        data.table::set(TimerDataTrain, i = 8L, j = "Process", value = "# Convert CalendarDate back to Date----")
      }

      # Save Timers to file----
      SaveTimers(SaveModelObjectss = SaveModelObjects, procs = proc, TimerDataEvals = TimerDataEval, TimerDataTrains = TimerDataTrain, MetaDataPaths = MetaDataPath, ModelIDs = ModelID)
    }

    # FE: AutoLagRollStats() BaseFunnelMeasure Over Calendar Time----
    if(proc %chin% c("evaluate","evaluation","eval","training","train")) {
      for(bfm in seq_len(length(BaseFunnelMeasure))) {
        temp <- data[, data.table::first(get(BaseFunnelMeasure[bfm])), by = eval(CalendarDate)]
        data.table::setnames(temp, "V1", eval(BaseFunnelMeasure[bfm]))
        x <- system.time(gcFirst = FALSE, temp <- RemixAutoML::AutoLagRollStats(

          # Data
          data                 = temp,
          DateColumn           = eval(CalendarDate),
          Targets              = eval(BaseFunnelMeasure[bfm]),
          HierarchyGroups      = NULL,
          IndependentGroups    = NULL,
          TimeGroups           = CalendarTimeGroups,
          TimeUnitAgg          = TimeUnit,
          TimeUnit             = TimeUnit,

          # Services
          TimeBetween          = NULL,
          RollOnLag1           = TRUE,
          Type                 = "Lag",
          SimpleImpute         = FALSE,

          # Calculated Columns
          Lags                 = CalendarLags,
          MA_RollWindows       = CalendarMovingAverages,
          SD_RollWindows       = CalendarStandardDeviations,
          Skew_RollWindows     = CalendarSkews,
          Kurt_RollWindows     = CalendarKurts,
          Quantile_RollWindows = CalendarQuantiles,
          Quantiles_Selected   = CalendarQuantilesSelected,
          Debug                = TRUE))
        if(proc %chin% c("evaluate","eval")) {
          data.table::set(TimerDataEval, i = 9L, j = "Time", value = x[[3L]])
          data.table::set(TimerDataEval, i = 9L, j = "Process", value = paste0("# Rolling stats for BaseFunnelMeasure ",bfm, "----"))
        } else if(proc %chin% c("training","train")) {
          data.table::set(TimerDataTrain, i = 9L, j = "Time", value = x[[3L]])
          data.table::set(TimerDataTrain, i = 9L, j = "Process", value = paste0("# Rolling stats for BaseFunnelMeasure ",bfm, "----"))
        }

        # Join back to data----
        data <- merge(data, temp[, .SD, .SDcols = c(eval(CalendarDate), setdiff(names(temp), names(data)))], by = eval(CalendarDate), all = FALSE)
      }
    }

    # FE: AutoLagRollStats() ConversionMeasure Over Calendar Time----
    if(proc %chin% c("evaluate","evaluation","eval","training","train")) {
      #temp <- data[get(CohortDate) == get(CalendarDate), sum(get(ConversionMeasure)), by = eval(CalendarDate)]
      temp <- data[, sum(get(ConversionMeasure)), by = eval(CohortDate)]
      data.table::setnames(temp, eval(CohortDate), eval(CalendarDate))
      data.table::setnames(temp, "V1", eval(ConversionMeasure))
      x <- system.time(gcFirst = FALSE, temp <- RemixAutoML::AutoLagRollStats(

        # Data
        data                 = temp,
        DateColumn           = eval(CalendarDate),
        Targets              = eval(ConversionMeasure),
        HierarchyGroups      = NULL,
        IndependentGroups    = NULL,
        TimeGroups           = CalendarTimeGroups,
        TimeUnitAgg          = TimeUnit,
        TimeUnit             = TimeUnit,

        # Services
        TimeBetween          = NULL,
        RollOnLag1           = TRUE,
        Type                 = "Lag",
        SimpleImpute         = FALSE,

        # Calculated Columns
        Lags                 = CalendarLags,
        MA_RollWindows       = CalendarMovingAverages,
        SD_RollWindows       = CalendarStandardDeviations,
        Skew_RollWindows     = CalendarSkews,
        Kurt_RollWindows     = CalendarKurts,
        Quantile_RollWindows = CalendarQuantiles,
        Quantiles_Selected   = CalendarQuantilesSelected,
        Debug                = TRUE))
      if(proc %chin% c("evaluate","eval")) {
        data.table::set(TimerDataEval, i = 9L, j = "Time", value = x[[3L]])
        data.table::set(TimerDataEval, i = 9L, j = "Process", value = "# Rolling stats for ConversionMeasure----")
      } else if(proc %chin% c("training","train")) {
        data.table::set(TimerDataTrain, i = 9L, j = "Time", value = x[[3L]])
        data.table::set(TimerDataTrain, i = 9L, j = "Process", value = "# Rolling stats for ConversionMeasure----")
      }

      # Join back to data----
      data <- merge(data, temp[, .SD, .SDcols = c(eval(CalendarDate), setdiff(names(temp), names(data)))], by = eval(CalendarDate), all = FALSE)
    }

    # FE: AutoLagRollStats() CalendarDateHolidayCounts Over Calendar Time----
    if(proc %chin% c("evaluate","evaluation","eval","training","train") & !is.null(CalendarHolidayLags)) {
      temp <- data[, max(get(paste0(CalendarDate, "HolidayCounts"))), by = eval(CalendarDate)]
      data.table::setnames(temp, "V1", paste0(CalendarDate, "HolidayCounts"))
      x <- system.time(gcFirst = FALSE, temp <- RemixAutoML::AutoLagRollStats(

        # Data
        data                 = temp,
        DateColumn           = eval(CalendarDate),
        Targets              = paste0(CalendarDate, "HolidayCounts"),
        HierarchyGroups      = NULL,
        IndependentGroups    = NULL,
        TimeGroups           = TimeUnit,
        TimeUnitAgg          = TimeUnit,
        TimeUnit             = TimeUnit,

        # Services
        TimeBetween          = NULL,
        RollOnLag1           = TRUE,
        Type                 = "Lag",
        SimpleImpute         = FALSE,

        # Calculated Columns
        Lags                 = CalendarHolidayLags,
        MA_RollWindows       = CalendarHolidayMovingAverages,
        SD_RollWindows       = NULL,
        Skew_RollWindows     = NULL,
        Kurt_RollWindows     = NULL,
        Quantile_RollWindows = NULL,
        Quantiles_Selected   = NULL,
        Debug                = TRUE))
      if(proc %chin% c("evaluate","eval")) {
        data.table::set(TimerDataEval, i = 9L, j = "Time", value = x[[3L]])
        data.table::set(TimerDataEval, i = 9L, j = "Process", value = "# Rolling stats for CalendarHolidayCounts----")
      } else if(proc %chin% c("training","train")) {
        data.table::set(TimerDataTrain, i = 9L, j = "Time", value = x[[3L]])
        data.table::set(TimerDataTrain, i = 9L, j = "Process", value = "# Rolling stats for CalendarHolidayCounts----")
      }

      # Join back to data
      data <- merge(data, temp[, .SD, .SDcols = c(eval(CalendarDate), setdiff(names(temp), names(data)))], by = eval(CalendarDate), all = FALSE)
    }

    # FE: ModelDataPrep() Impute Numeric Columns from AutoLagRollStats()----
    x <- system.time(gcFirst = FALSE, data <- RemixAutoML::ModelDataPrep(
      data         = data,
      Impute       = TRUE,
      CharToFactor = FALSE,
      FactorToChar = FALSE,
      IntToNumeric = TRUE,
      DateToChar   = FALSE,
      RemoveDates  = FALSE,
      MissFactor   = "0",
      MissNum      = ImputeRollStats,
      IgnoreCols   = NULL))
    if(proc %chin% c("evaluate","eval","evaluation")) {
      data.table::set(TimerDataEval, i = 10L, j = "Time", value = x[[3L]])
      data.table::set(TimerDataEval, i = 10L, j = "Process", value = "# Model data prep----")
    } else if(proc %chin% c("training","train")) {
      data.table::set(TimerDataTrain, i = 10L, j = "Time", value = x[[3L]])
      data.table::set(TimerDataTrain, i = 10L, j = "Process", value = "# Model data prep----")
    }

    # Save Timers to file
    SaveTimers(SaveModelObjectss = SaveModelObjects, procs = proc, TimerDataEvals = TimerDataEval, TimerDataTrains = TimerDataTrain, MetaDataPaths = MetaDataPath, ModelIDs = ModelID)

    #----

    #----

    # DM: Save data as csv----
    x <- system.time(gcFirst = FALSE, if(SaveModelObjects) data.table::fwrite(data, file = file.path(MetaDataPath, paste0(ModelID, "_ModelDataReady.csv"))))
    if(proc %chin% c("evaluate","eval")) {
      data.table::set(TimerDataEval, i = 12L, j = "Time", value = x[[3L]])
      data.table::set(TimerDataEval, i = 12L, j = "Process", value = "# Save data to file----")
    } else if(proc %chin% c("training","train")) {
      data.table::set(TimerDataTrain, i = 12L, j = "Time", value = x[[3L]])
      data.table::set(TimerDataTrain, i = 12L, j = "Process", value = "# Save data to file----")
    }

    # Save timers to file
    SaveTimers(SaveModelObjectss = SaveModelObjects, procs = proc, TimerDataEvals = TimerDataEval, TimerDataTrains = TimerDataTrain, MetaDataPaths = MetaDataPath, ModelIDs = ModelID)

    # DM: Load data if updating models without new data----
    if(!exists("data")) data <- data.table::fread(file = file.path(MetaDataPath, paste0(ModelID, "_ModelDataReady.csv")))
    if(proc %chin% c("evaluate","eval")) {
      data.table::set(TimerDataEval, i = 13L, j = "Time", value = x[[3L]])
      data.table::set(TimerDataEval, i = 13L, j = "Process", value = "# Load data if updating models without new data----")
    } else if(proc %chin% c("training","train")) {
      data.table::set(TimerDataTrain, i = 13L, j = "Time", value = x[[3L]])
      data.table::set(TimerDataTrain, i = 13L, j = "Process", value = "# Load data if updating models without new data----")
    }

    # Save timers to file
    SaveTimers(SaveModelObjectss = SaveModelObjects, procs = proc, TimerDataEvals = TimerDataEval, TimerDataTrains = TimerDataTrain, MetaDataPaths = MetaDataPath, ModelIDs = ModelID)

    # DM: Type Casting for CohortPeriodsVariable, CalendarDate, and CohortDate----
    if(!all(class(data[[eval(CohortPeriodsVariable)]]) %chin% "numeric")) data[, eval(CohortPeriodsVariable) := as.numeric(as.character(get(CohortPeriodsVariable)))]
    if(!all(class(data[[eval(CalendarDate)]]) %chin% "Date")) data[, eval(CalendarDate) := as.Date(get(CalendarDate))]
    if(!all(class(data[[eval(CohortDate)]]) %chin% "Date")) data[, eval(CohortDate) := as.Date(get(CohortDate))]

    # Filter out historical values via truncation----
    if(!is.null(TruncateDate)) data <- data[get(CalendarDate) >= eval(TruncateDate)]

    # DM: Partition Data----
    if(proc %chin% c("evaluate","eval","evaluation")) {
      x <- system.time(gcFirst = FALSE, DataSets <- RemixAutoML::AutoDataPartition(
        data = data,
        NumDataSets = 3L,
        Ratios = PartitionRatios,
        PartitionType = "random",
        StratifyColumnNames = NULL,
        StratifyNumericTarget = NULL,
        StratTargetPrecision = 1L,
        TimeColumnName = NULL))
      data.table::set(TimerDataEval, i = 15L, j = "Time", value = x[[3L]])
      data.table::set(TimerDataEval, i = 15L, j = "Process", value = "# Partition Data----")
      TrainData <- DataSets$TrainData
      ValidationData <- DataSets$ValidationData
      TestData <- DataSets$TestData
      rm(DataSets)
    }

    #----

    #----

    # ML: CatBoostRegression()----
    if(proc %chin% c("evaluate","eval","training","train")) {

      # Define features----
      if(proc %chin% c("evaluate","eval")) {
        Features <- names(TrainData)[!names(TrainData) %chin% c(eval(CalendarDate),eval(CohortDate),eval(BaseFunnelMeasure),eval(ConversionMeasure),eval(ConversionRateMeasure))]
        idcols <- names(TrainData)[!names(TrainData) %in% Features]
        if(ModelID %chin% names(TrainData)) Features <- Features[!Features %chin% ModelID]
      } else {
        Features <- names(data)[!names(data) %chin% c(eval(CalendarDate),eval(CohortDate),eval(BaseFunnelMeasure),eval(ConversionMeasure),eval(ConversionRateMeasure))]
        idcols <- names(data)[!names(data) %in% Features]
        if(ModelID %chin% names(data)) Features <- Features[!Features %chin% ModelID]
      }

      # Define number of trees----
      if(proc %chin% c("eval","evaluation","evaluate")) NTrees <- Trees

      # Build model----
      x <- system.time(gcFirst = FALSE, TestModel <- RemixAutoML::AutoCatBoostRegression(

        # GPU or CPU and the number of available GPUs----
        task_type = TaskType,
        NumGPUs = NumGPUs,

        # Metadata arguments----
        #   'ModelID' is used to create part of the file names generated when saving to file'
        #   'model_path' is where the minimal model objects for scoring will be stored
        #      'ModelID' will be the name of the saved model object
        #   'metadata_path' is where model evaluation and model interpretation files are saved
        #      objects saved to model_path if metadata_path is null
        #      Saved objects include:
        #         'ModelID_ValidationData.csv' is the supplied or generated TestData with predicted values
        #         'ModelID_VariableImportance.csv' is the variable importance.
        #            This won't be saved to file if GrowPolicy is either "Depthwise" or "Lossguide" was used
        #         'ModelID_ExperimentGrid.csv' if GridTune = TRUE.
        #            Results of all model builds including parameter settings, bandit probs, and grid IDs
        #         'ModelID_EvaluationMetrics.csv' which contains MSE, MAE, MAPE, R2
        ModelID = paste0(ModelID,"_", proc, "_"),
        model_path = ModelPath,
        metadata_path = MetaDataPath,
        SaveModelObjects = FALSE,
        ReturnModelObjects = TRUE,

        # Data arguments----
        #   'TrainOnFull' is to train a model with 100 percent of your data.
        #     That means no holdout data will be used for evaluation
        #   If ValidationData and TestData are NULL and TrainOnFull is FALSE then data will be split 70 20 10
        #   'PrimaryDateColumn' is a date column in data that is meaningful when sorted.
        #     CatBoost categorical treatment is enhanced when supplied
        #   'IDcols' are columns in your data that you don't use for modeling but get returned with ValidationData
        #   'TransformNumericColumns' is for transforming your target variable. Just supply the name of it
        data = if(proc %chin% c("eval", "evaluate")) TrainData else data,
        TrainOnFull = if(proc %chin% c("eval", "evaluate")) FALSE else TRUE,
        ValidationData = if(proc %chin% c("eval", "evaluate")) ValidationData else NULL,
        TestData = if(proc %chin% c("eval", "evaluate")) TestData else NULL,
        TargetColumnName = "Rate",
        FeatureColNames = Features,
        PrimaryDateColumn = CohortDate,
        IDcols = idcols,
        TransformNumericColumns = if(TransformTargetVariable) "Rate" else NULL,
        Methods = TransformMethods,

        # Model evaluation----
        #   'eval_metric' is the measure catboost uses when evaluting on holdout data during its bandit style process
        #   'loss_function' the loss function used in training optimization
        #   'NumOfParDepPlots' Number of partial dependence calibration plots generated.
        #     A value of 3 will return plots for the top 3 variables based on variable importance
        #     Won't be returned if GrowPolicy is either "Depthwise" or "Lossguide" is used
        #     Can run the RemixAutoML::ParDepCalPlots() with the outputted ValidationData
        eval_metric = EvaluationMetric,
        loss_function = LossFunction,
        MetricPeriods = MetricPeriods,
        NumOfParDepPlots = NumOfParDepPlots,

        # Grid tuning arguments----
        #   'PassInGrid' is for retraining using a previous grid winning args
        #   'MaxModelsInGrid' is a cap on the number of models that will run
        #   'MaxRunsWithoutNewWinner' number of runs without a new winner before exiting grid tuning
        #   'MaxRunMinutes' is a cap on the number of minutes that will run
        #   'Shuffles' is the number of times you want the random grid arguments shuffled
        #   'BaselineComparison' default means to compare each model build with a default built of catboost using max(Trees)
        #   'MetricPeriods' is the number of trees built before evaluting holdoutdata internally. Used in finding actual Trees used.
        PassInGrid = PassInGrid,
        GridTune = GridTune,
        MaxModelsInGrid = MaxModelsInGrid,
        MaxRunsWithoutNewWinner = MaxRunsWithoutNewWinner,
        MaxRunMinutes = MaxRunMinutes,
        Shuffles = 4L,
        BaselineComparison = BaselineComparison,

        # Tuning parameters----
        # Trees, Depth, and LearningRate used in the bandit grid tuning
        # Must set Trees to a single value if you are not grid tuning
        # The ones below can be set to NULL and the values in the example will be used
        # GrowPolicy is turned off for CPU runs
        # BootStrapType utilizes Poisson only for GPU and MVS only for CPU
        Trees = NTrees, #seq(100L, 2000L, 100L),
        Depth = Depth,
        LearningRate = LearningRate,
        L2_Leaf_Reg = L2_Leaf_Reg,
        RSM = RSM,
        BootStrapType = BootStrapType,
        GrowPolicy = GrowPolicy))

      # Define number of trees----
      if(proc %chin% c("eval","evaluate")) {
        TreeCount <- TestModel$Model$tree_count
        if(!is.null(Trees)) NTrees <- TreeCount else NTrees <- Trees
        ArgsList[["Trees"]] <- NTrees
      } else {
        NTrees <- Trees
      }

      # Store results----
      if(SaveModelObjects) {
        if(proc %chin% c("evaluate","eval","evaluation")) {
          data.table::set(TimerDataEval, i = 16L, j = "Time", value = x[[3L]])
          data.table::set(TimerDataEval, i = 16L, j = "Process", value = "# Build model using CatBoostRegression()----")
        } else if(proc %chin% c("training","train")) {
          data.table::set(TimerDataTrain, i = 16L, j = "Time", value = x[[3L]])
          data.table::set(TimerDataTrain, i = 16L, j = "Process", value = "# Build model using CatBoostRegression()----")
        }
      }

      # Save Timers to file
      SaveTimers(SaveModelObjectss = SaveModelObjects, procs = proc, TimerDataEvals = TimerDataEval, TimerDataTrains = TimerDataTrain, MetaDataPaths = MetaDataPath, ModelIDs = ModelID)

      # Save model objects----
      if(SaveModelObjects) {
        if(proc %chin% c("evaluate","eval","evaluation")) {
          save(TestModel, file =  file.path(normalizePath(ModelPath), paste0(ModelID, "_Evaluation.Rdata")))
        } else if(proc %chin% c("training","train")) {
          save(TestModel, file = file.path(normalizePath(ModelPath), paste0(ModelID, "_FinalTrain.Rdata")))
        }
      }

      # Remove objects before next run----
      if(proc %chin% c("evaluate","eval","evaluation")) {
        rm(TestModel)
      } else {
        rm(TestModel, data)
      }

      # Garbage collection----
      gc()
    }

    # Save timers to file----
    SaveTimers(SaveModelObjectss = SaveModelObjects, procs = proc, TimerDataEvals = TimerDataEval, TimerDataTrains = TimerDataTrain, MetaDataPaths = MetaDataPath, ModelIDs = ModelID)

    #----

    #----

  }

  # Return and Save----
  if(SaveModelObjects) save(ArgsList, file = file.path(normalizePath(ModelPath), paste0(ModelID, "_ArgsList.Rdata")))
  return(ArgsList)
}

#' CLForecast
#'
#' CLForecast for generating forecasts
#'
#' @author Adrian Antico
#' @family Automated Model Scoring
#' @param data N
#' @param FC_BaseFunnelMeasure d
#' @param OutputFilePath P
#' @param MaxDateForecasted S
#' @param MaxCalendarDate S
#' @param ArgsList A
#' @param MaxCohortPeriods T
#' @return S
#' @export
CLForecast <- function(data,
                       OutputFilePath = NULL,
                       FC_BaseFunnelMeasure = NULL,
                       SegmentName = NULL,
                       MaxDateForecasted = NULL,
                       MaxCalendarDate = NULL,
                       ArgsList = NULL,
                       MaxCohortPeriods = NULL) {

  # Forecasting start and end periods----
  if(parallel::detectCores() > 10) data.table::setDTthreads(threads = parallel::detectCores() - 2L) else data.table::setDTthreads(threads = max(1L, parallel::detectCores()))
  if(is.null(MaxDateForecasted)) MaxDateForecasted <- data[, max(get(ArgsList$CalendarDate), na.rm = TRUE)]
  if(is.null(MaxCalendarDate)) MaxCalendarDate <- FC_BaseFunnelMeasure[, max(get(ArgsList$CalendarDate), na.rm = TRUE)]
  if(is.null(SegmentName)) SegmentName <- ArgsList$ModelID

  # Loop through all periods to forecast----
  FC_Period <- 0L
  while(MaxDateForecasted < MaxCalendarDate) {

    # Increment FC_Period----
    FC_Period <- FC_Period + 1L
    for(bla in seq_len(20L)) print(paste0("Working on Forecast for period: ", FC_Period, " ::: Periods left to forecast: ", difftime(MaxCalendarDate, MaxDateForecasted)))

    # DE: Prepare data----
    print("# Prepare data----")

    # Convert to date----
    if(!all(class(data[[eval(ArgsList$CalendarDate)]]) %chin% "Date")) data[, eval(ArgsList$CalendarDate) := as.Date(get(ArgsList$CalendarDate))]
    if(!all(class(data[[eval(ArgsList$CohortDate)]]) != "Date")) data[, eval(ArgsList$CohortDate) := as.Date(get(ArgsList$CohortDate))]

    # Add indicator variable for AutoLagRollStatsScoring() so it knows what to score and what not to. A value of 1 indicates that the record should be scored. All others are not scored----
    data[, ScoreRecords := 2]

    # Type conversion----
    if(class(data[[eval(ArgsList$CohortPeriodsVariable)]]) == "factor") data[, eval(ArgsList$CohortPeriodsVariable) := as.numeric(as.character(get(ArgsList$CohortPeriodsVariable)))]

    # Create single future value for all cohorts----
    maxct <- data[, list(max(get(ArgsList$CohortPeriodsVariable)), data.table::first(ScoreRecords)), by = list(get(ArgsList$CalendarDate))]
    data.table::setnames(maxct, c("get","V1","V2"), c(ArgsList$CalendarDate, ArgsList$CohortPeriodsVariable, "ScoreRecords"))
    maxct[, eval(ArgsList$CohortPeriodsVariable) := get(ArgsList$CohortPeriodsVariable) + 1L]
    maxct[, eval(ArgsList$CohortDate) := as.Date(get(ArgsList$CalendarDate)) + lubridate::days(get(ArgsList$CohortPeriodsVariable))]
    maxct[, Segment := eval(ArgsList$ModelID)]
    data.table::setnames(maxct, "Segment", eval(SegmentName))

    # DE: Subset data and update data----
    print("# Subset data and update data----")
    FC_BaseFunnelMeasure <- FC_BaseFunnelMeasure[get(ArgsList$CalendarDate) > max(maxct[[eval(ArgsList$CalendarDate)]])]
    NextFCPeriod <- FC_BaseFunnelMeasure[1L]
    NextFCPeriod[, eval(ArgsList$CalendarDate) := as.Date(get(ArgsList$CalendarDate))]
    NextFCPeriod[, eval(ArgsList$CohortDate) := as.Date(get(ArgsList$CohortDate))]
    NextFCPeriod[, ScoreRecords := 1]
    FC_BaseFunnelMeasure <- FC_BaseFunnelMeasure[2L:.N]

    # DE: Merge on next date of BaseFunnelMeasure----
    print("# Merge on next date of BaseFunnelMeasure----")
    if(length(ArgsList$BaseFunnelMeasure) == 1) {
      temp <- data[, list(data.table::first(get(ArgsList$BaseFunnelMeasure[1]))), by = list(get(ArgsList$CalendarDate))]
      data.table::setnames(temp, c("get","V1"), c(ArgsList$CalendarDate, ArgsList$BaseFunnelMeasure[1]))
    } else if(length(ArgsList$BaseFunnelMeasure) == 2) {
      temp <- data[, list(
        data.table::first(get(ArgsList$BaseFunnelMeasure[1])),
        data.table::first(get(ArgsList$BaseFunnelMeasure[2]))), by = list(get(ArgsList$CalendarDate))]
      data.table::setnames(temp, c("get","V1","V2","V3"), c(ArgsList$CalendarDate, ArgsList$BaseFunnelMeasure[1],ArgsList$BaseFunnelMeasure[2]))
    } else if(length(ArgsList$BaseFunnelMeasure) == 3) {
      temp <- data[, list(
        data.table::first(get(ArgsList$BaseFunnelMeasure[1])),
        data.table::first(get(ArgsList$BaseFunnelMeasure[2])),
        data.table::first(get(ArgsList$BaseFunnelMeasure[3]))), by = list(get(ArgsList$CalendarDate))]
      data.table::setnames(temp, c("get","V1","V2","V3"), c(ArgsList$CalendarDate, ArgsList$BaseFunnelMeasure[1],ArgsList$BaseFunnelMeasure[2],ArgsList$BaseFunnelMeasure[3]))
    }

    # Merge data----
    temp[, eval(ArgsList$CalendarDate) := as.Date(get(ArgsList$CalendarDate))]
    maxct <- merge(maxct, temp, by = ArgsList$CalendarDate, all.x = TRUE)
    maxct[, eval(ArgsList$ConversionMeasure) := 0]
    maxct[, Rate := 0]
    maxct[, ScoreRecords := 1]
    maxct <- data.table::rbindlist(list(maxct, NextFCPeriod), use.names = TRUE, fill = TRUE)
    for(xxxx in seq_len(ncol(maxct))) data.table::set(maxct, i = which(is.na(maxct[[xxxx]])), j = xxxx, value = 0)

    # DE: Remove CohortPeriods beyond MaxCohortPeriods
    maxct <- maxct[get(ArgsList$CohortPeriodsVariable) <= MaxCohortPeriods]
    ScoreDate <- maxct[, max(get(ArgsList$CalendarDate))]

    # DE: Stack onto modeling data for ArgsList$ModelID
    print("# Stack onto modeling data for ArgsList$ModelID----")
    data <- data.table::rbindlist(list(data, maxct), fill = TRUE, use.names = TRUE)
    rm(maxct)

    # FE: Calendar & Holiday Variables----
    print("# Feature Engineering----")
    data <- RemixAutoML::CreateCalendarVariables(data, DateCols = c(ArgsList$CalendarDate, ArgsList$CohortDate), AsFactor = FALSE, TimeUnits = ArgsList$CalendarVariables)
    data <- RemixAutoML::CreateHolidayVariables(data, DateCols = c(ArgsList$CalendarDate), HolidayGroups = ArgsList$HolidayGroups, Holidays = NULL, GroupingVars = eval(SegmentName), Print = FALSE)
    data.table::setnames(data, old = "HolidayCounts", new = paste0(ArgsList$CalendarDate,"HolidayCounts"))
    data <- RemixAutoML::CreateHolidayVariables(data, DateCols = c(ArgsList$CohortDate), HolidayGroups = ArgsList$HolidayGroups, Holidays = NULL, GroupingVars = eval(SegmentName), Print = FALSE)
    data.table::setnames(data, old = "HolidayCounts", new = paste0(ArgsList$CohortDate,"HolidayCounts"))
    data.table::setorderv(data, cols = c(ArgsList$CalendarDate,eval(ArgsList$CohortPeriodsVariable)), c(1L, 1L))

    # Add Anomaly detection zeros----
    if(!is.null(ArgsList[["AnomalyDetection"]])) data[, ":=" (AnomHigh = 0, AnomLow = 0)]

    # FE: ConversionMeasure and Rate AutoLagRollStatsScoring----
    print("# AutoLagRollStatsScoring----")
    temp <- data.table::copy(data)
    data.table::set(temp, j = ArgsList$CalendarDate, value = as.character(temp[[ArgsList$CalendarDate]]))
    temp <- RemixAutoML::AutoLagRollStatsScoring(

      # Data
      data                 = temp,
      DateColumn           = ArgsList$CohortDate,
      Targets              = c(ArgsList$ConversionMeasure, "Rate"),
      RowNumsID            = "ScoreRecords",
      RowNumsKeep          = 1,
      HierarchyGroups      = NULL,
      IndependentGroups    = ArgsList$CalendarDate,
      TimeUnit             = ArgsList$TimeUnit,
      TimeGroups           = ArgsList$CohortTimeGroups,
      TimeUnitAgg          = ArgsList$TimeUnit,

      # Services
      TimeBetween          = NULL,
      RollOnLag1           = TRUE,
      Type                 = "Lag",
      SimpleImpute         = FALSE,

      # Calculated Columns
      Lags                 = ArgsList$CohortLags,
      MA_RollWindows       = ArgsList$CohortMovingAverages,
      SD_RollWindows       = ArgsList$CohortStandardDeviations,
      Skew_RollWindows     = ArgsList$CohortSkews,
      Kurt_RollWindows     = ArgsList$Kurts,
      Quantile_RollWindows = ArgsList$Quantiles,
      Quantiles_Selected   = ArgsList$CohortQuantilesSelected,
      Debug                = TRUE)

    # Join datasets
    print("# Combine datasets----")
    temp[, eval(ArgsList$CalendarDate) := as.Date(get(ArgsList$CalendarDate))]
    data <- merge(data, temp[, .SD, .SDcols = c(eval(ArgsList$CalendarDate),eval(ArgsList$CohortDate), setdiff(names(temp), names(data)))], by = c(eval(ArgsList$CalendarDate), eval(ArgsList$CohortDate)), all.x = TRUE)
    rm(temp)

    # FE: CohortDateHolidayCounts AutoLagRollStatsScoring----
    print("# TransferHolidayCounts AutoLagRollStatsScoring----")
    temp <- data.table::copy(data)
    data.table::set(temp, j = eval(ArgsList$CalendarDate), value = as.character(temp[[eval(ArgsList$CalendarDate)]]))
    temp <- RemixAutoML::AutoLagRollStatsScoring(

      # Data
      data                 = temp,
      DateColumn           = ArgsList$CohortDate,
      Targets              = paste0(ArgsList$CohortDate,"HolidayCounts"),
      RowNumsID            = "ScoreRecords",
      RowNumsKeep          = 1,
      HierarchyGroups      = NULL,
      IndependentGroups    = ArgsList$CalendarDate,
      TimeUnit             = ArgsList$TimeUnit,
      TimeGroups           = ArgsList$TimeUnit,
      TimeUnitAgg          = ArgsList$TimeUnit,

      # Services
      TimeBetween          = NULL,
      RollOnLag1           = TRUE,
      Type                 = "Lag",
      SimpleImpute         = FALSE,

      # Calculated Columns
      Lags                 = ArgsList$CohortHolidayLags,
      MA_RollWindows       = ArgsList$CohortHolidayMovingAverages,
      SD_RollWindows       = NULL,
      Skew_RollWindows     = NULL,
      Kurt_RollWindows     = NULL,
      Quantile_RollWindows = NULL,
      Quantiles_Selected   = NULL,
      Debug                = TRUE)

    # Join datasets
    print("# Combine datasets----")
    temp[, eval(ArgsList$CalendarDate) := as.Date(get(ArgsList$CalendarDate))]
    data <- merge(data, temp[, .SD, .SDcols = c(eval(ArgsList$CalendarDate), eval(ArgsList$CohortDate), setdiff(names(temp), names(data)))], by = c(eval(ArgsList$CalendarDate), eval(ArgsList$CohortDate)), all.x = TRUE)
    rm(temp)

    # FE: BaseFunnelMeasure AutoLagRollStatsScoring----
    print("# AutoLagRollStatsScoring----")
    for(bfm in seq_len(length(ArgsList$BaseFunnelMeasure))) {
      temp <- data.table::copy(data)
      temp <- temp[, list(data.table::first(get(ArgsList$BaseFunnelMeasure[bfm]))), by = list(get(ArgsList$CalendarDate))]
      data.table::setnames(temp, c("get","V1"), c(eval(ArgsList$CalendarDate), eval(ArgsList$BaseFunnelMeasure[bfm])))
      temp[, ScoreRecords := data.table::fifelse(get(ArgsList$CalendarDate) == ScoreDate, 1, 2)]
      data.table::set(temp, j = eval(ArgsList$CalendarDate), value = as.Date(temp[[eval(ArgsList$CalendarDate)]]))
      temp <- RemixAutoML::AutoLagRollStatsScoring(

        # Data
        data                 = temp,
        DateColumn           = ArgsList$CalendarDate,
        Targets              = ArgsList$BaseFunnelMeasure[bfm],
        HierarchyGroups      = NULL,
        IndependentGroups    = NULL,
        TimeGroups           = ArgsList$CalendarTimeGroups,
        TimeUnit             = ArgsList$TimeUnit,
        TimeUnitAgg          = ArgsList$TimeUnit,

        # Services
        RowNumsID            = "ScoreRecords",
        RowNumsKeep          = 1,
        TimeBetween          = NULL,
        RollOnLag1           = TRUE,
        Type                 = "Lag",
        SimpleImpute         = FALSE,

        # Calculated Columns
        Lags                 = ArgsList$CalendarLags,
        MA_RollWindows       = ArgsList$CalendarMovingAverages,
        SD_RollWindows       = ArgsList$CalendarStandardDeviations,
        Skew_RollWindows     = ArgsList$CalendarSkews,
        Kurt_RollWindows     = ArgsList$CalendarKurts,
        Quantile_RollWindows = ArgsList$CalendarQuantiles,
        Quantiles_Selected   = ArgsList$CalendarQuantilesSelected,
        Debug                = TRUE)

      # Join datasets
      print("# Combine datasets----")
      temp[, eval(ArgsList$CalendarDate) := as.Date(get(ArgsList$CalendarDate))]
      data <- merge(data, temp[, .SD, .SDcols = c(eval(ArgsList$CalendarDate), setdiff(names(temp), names(data)))], by = eval(ArgsList$CalendarDate), all.x = TRUE)
      rm(temp)
    }

    # FE: Total Transfers by CalendarDate AutoLagRollStatsScoring----
    print("# AutoLagRollStatsScoring----")
    temp <- data.table::copy(data)
    temp <- temp[get(ArgsList$CohortDate) == get(ArgsList$CalendarDate), list(sum(get(ArgsList$ConversionMeasure))), by = list(get(ArgsList$CalendarDate))]
    data.table::setnames(temp, c("get","V1"), c(eval(ArgsList$CalendarDate), eval(ArgsList$ConversionMeasure)))
    temp[, ScoreRecords := data.table::fifelse(get(ArgsList$CalendarDate) == ScoreDate, 1, 2)]
    data.table::set(temp, j = eval(ArgsList$CalendarDate), value = as.Date(temp[[eval(ArgsList$CalendarDate)]]))
    temp <- RemixAutoML::AutoLagRollStatsScoring(

      # Data
      data                 = temp,
      DateColumn           = ArgsList$CalendarDate,
      Targets              = ArgsList$ConversionMeasure,
      HierarchyGroups      = NULL,
      IndependentGroups    = NULL,
      TimeGroups           = ArgsList$CalendarTimeGroups,
      TimeUnit             = ArgsList$TimeUnit,
      TimeUnitAgg          = ArgsList$TimeUnit,

      # Services
      RowNumsID            = "ScoreRecords",
      RowNumsKeep          = 1,
      TimeBetween          = NULL,
      RollOnLag1           = TRUE,
      Type                 = "Lag",
      SimpleImpute         = FALSE,

      # Calculated Columns
      Lags                 = ArgsList$CalendarLags,
      MA_RollWindows       = ArgsList$CalendarMovingAverages,
      SD_RollWindows       = ArgsList$CalendarStandardDeviations,
      Skew_RollWindows     = ArgsList$CalendarSkews,
      Kurt_RollWindows     = ArgsList$CalendarKurts,
      Quantile_RollWindows = ArgsList$CalendarQuantiles,
      Quantiles_Selected   = ArgsList$CalendarQuantilesSelected,
      Debug                = TRUE)

    # Join datasets
    print("# Combine datasets----")
    temp[, eval(ArgsList$CalendarDate) := as.Date(get(ArgsList$CalendarDate))]
    data <- merge(data, temp[, .SD, .SDcols = c(eval(ArgsList$CalendarDate), setdiff(names(temp),names(data)))], by = eval(ArgsList$CalendarDate), all.x = TRUE)
    rm(temp)

    # FE: CalendarDateHolidayCounts AutoLagRollStatsScoring----
    print("# AutoLagRollStatsScoring----")
    temp <- data.table::copy(data)
    temp <- temp[, list(max(get(paste0(ArgsList$CalendarDate,"HolidayCounts")))), by = list(get(ArgsList$CalendarDate))]
    data.table::setnames(temp, c("get","V1"), c(eval(ArgsList$CalendarDate), paste0(ArgsList$CalendarDate,"HolidayCounts")))
    temp[, ScoreRecords := data.table::fifelse(get(ArgsList$CalendarDate) == ScoreDate, 1, 2)]
    data.table::set(temp, j = eval(ArgsList$CalendarDate), value = as.Date(temp[[eval(ArgsList$CalendarDate)]]))
    temp <- RemixAutoML::AutoLagRollStatsScoring(

      # Data
      data                 = temp,
      DateColumn           = eval(ArgsList$CalendarDate),
      Targets              = paste0(ArgsList$CalendarDate, "HolidayCounts"),
      HierarchyGroups      = NULL,
      IndependentGroups    = NULL,
      TimeGroups           = ArgsList$TimeUnit,
      TimeUnitAgg          = ArgsList$TimeUnit,
      TimeUnit             = ArgsList$TimeUnit,

      # Services
      RowNumsID            = "ScoreRecords",
      RowNumsKeep          = 1,
      TimeBetween          = NULL,
      RollOnLag1           = TRUE,
      Type                 = "Lag",
      SimpleImpute         = FALSE,

      # Calculated Columns
      Lags                 = ArgsList$CalendarHolidayLags,
      MA_RollWindows       = ArgsList$CalendarHolidayMovingAverages,
      SD_RollWindows       = NULL,
      Skew_RollWindows     = NULL,
      Kurt_RollWindows     = NULL,
      Quantile_RollWindows = NULL,
      Quantiles_Selected   = NULL,
      Debug                = TRUE)

    # Join datasets
    print("# Combine datasets----")
    temp[, eval(ArgsList$CalendarDate) := as.Date(get(ArgsList$CalendarDate))]
    data <- merge(data, temp[, .SD, .SDcols = c(eval(ArgsList$CalendarDate), setdiff(names(temp),names(data)))], by = c(eval(ArgsList$CalendarDate)), all.x = TRUE)
    rm(temp)

    # DE: Model data prep----
    print("# Model data prep----")
    data <- RemixAutoML::ModelDataPrep(
      data         = data,
      Impute       = TRUE,
      CharToFactor = FALSE,
      FactorToChar = FALSE,
      IntToNumeric = TRUE,
      DateToChar   = FALSE,
      RemoveDates  = FALSE,
      MissFactor   = "0",
      MissNum      = ArgsList$ImputeRollStats,
      IgnoreCols   = NULL)

    # DE: Type Change: CorhortDaysOut as numeric and the dates as Dates----
    print("# Convert features types to correct ones----")
    if(!all(class(data[[ArgsList$CohortPeriodsVariable]]) %chin% "numeric")) data[, eval(ArgsList$CohortPeriodsVariable) := as.numeric(as.character(get(ArgsList$CohortPeriodsVariable)))]
    if(!all(class(data[[ArgsList$CalendarDate]]) %chin% "Date")) data[, eval(ArgsList$CalendarDate) := as.Date(get(ArgsList$CalendarDate))]
    if(!all(class(data[[ArgsList$CohortDate]]) %chin% "Date")) data[, eval(ArgsList$CohortDate) := as.Date(get(ArgsList$CohortDate))]

    # DE: Load model artifacts----
    print("# Load model artifacts----")
    if(FC_Period == 1L) load(file = file.path(normalizePath(eval(ArgsList$ModelPath)), paste0(ArgsList$ModelID, "_FinalTrain.Rdata")))

    # ML: Score Model----
    print("# Score Model----")
    temp1 <- data.table::copy(data)
    temp <- temp1[ScoreRecords == 1]
    Features <- TestModel$ColNames[[1L]]
    temp <- RemixAutoML::AutoCatBoostScoring(
      TargetType = "regression",
      ScoringData = temp,
      FeatureColumnNames = Features,
      IDcols = names(temp)[!names(temp) %chin% Features],
      ModelObject = TestModel$Model,
      ModelPath = eval(ArgsList$ModelPath),
      ModelID = eval(ArgsList$ModelID),
      ReturnFeatures = FALSE,
      MultiClassTargetLevels = NULL,
      TransformNumeric = FALSE,
      BackTransNumeric = FALSE,
      TargetColumnName = "Rate",
      TransformationObject = TestModel$TransformationResults,
      TransID = NULL,
      TransPath = NULL,
      MDP_Impute = TRUE,
      MDP_CharToFactor = TRUE,
      MDP_RemoveDates = TRUE,
      MDP_MissFactor = "0",
      MDP_MissNum = -1,
      RemoveModel = FALSE)

    # DE: Update forecast data----
    print("# Update forecast data----")
    temp1[ScoreRecords == 1, Rate := temp[which(Predictions < 0), Predictions := 0][[1L]]]
    temp1[ScoreRecords == 1, eval(ArgsList$ConversionMeasure) := Rate * (get(ArgsList$BaseFunnelMeasure) + 1)]
    temp1 <- temp1[ScoreRecords == 1, .SD, .SDcols = c(eval(ArgsList$CalendarDate),eval(ArgsList$CohortDate),eval(ArgsList$CohortPeriodsVariable),SegmentName,eval(ArgsList$BaseFunnelMeasure),eval(ArgsList$ConversionMeasure),"Rate")]
    data <- data.table::rbindlist(list(data[ScoreRecords != 1, .SD, .SDcols = c(eval(ArgsList$CalendarDate),eval(ArgsList$CohortDate),eval(ArgsList$CohortPeriodsVariable),SegmentName,eval(ArgsList$BaseFunnelMeasure),eval(ArgsList$ConversionMeasure),"Rate")], temp1), fill = TRUE, use.names = TRUE)

    # DE: Save forecasts to file----
    data.table::fwrite(data, file = file.path(normalizePath(eval(OutputFilePath)), paste0(ArgsList$ModelID, "_Forecasts.csv")))

    # DE: Update MaxDateForecasted to know when to stop----
    MaxDateForecasted <- data[, max(get(ArgsList$CalendarDate))]
  }
}
