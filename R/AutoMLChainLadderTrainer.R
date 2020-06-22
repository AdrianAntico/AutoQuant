#' @title AutoCatBoostChainLadder
#' 
#' @description AutoCatBoostChainLadder is a forecasting model for chain ladder style forecasting
#' 
#' @author Adrian Antico
#' @family Automated Time Series
#' @param data data object
#' @param PartitionRatios Requires three values for train, validation, and test data sets
#' @param BaseFunnelMeasure E.g. "Leads". This value should be a forward looking variable. Say you want to forecast ConversionMeasure 2 months into the future. You should have two months into the future of values of BaseFunnelMeasure
#' @param ConversionMeasure E.g. "Conversions". Rate is derived as conversions over leads by cohort periods out
#' @param CohortPeriodsVariable Numeric. Numerical value of the the number of periods since cohort base date. 
#' @param TargetVariable Target Variable Name
#' @param CalendarDate The name of your date column that represents the calendar date
#' @param CohortDate The name of your date column that represents the cohort date
#' @param MaxCohortPeriods The maximum number of CohortPeriodsVariable out to include in modeling
#' @param TimeUnit Base time unit of data. "days", "weeks", "months", "quarters", "years"
#' @param TransformTargetVariable TRUE or FALSe
#' @param TransformMethods Choose from "Identity", "BoxCox", "Asinh", "Asin", "Log", "LogPlus1", "Logit", "YeoJohnson"
#' @param CalendarTimeGroups TimeUnit value must be included. If you want to generate lags and moving averages in several time based aggregations, choose from "days", "weeks", "months", "quarters", "years".
#' @param CohortTimeGroups TimeUnit value must be included. If you want to generate lags and moving averages in several time based aggregations, choose from "days", "weeks", "months", "quarters", "years". 
#' @param ModelPath Path to where you want your models saved
#' @param MetaDataPath Path to where you want your metadata saved. If NULL, function will try ModelPath if it is not NULL.
#' @param ModelID A character string to name your model and output
#' @param NumOfParDepPlots Tell the function the number of partial dependence calibration plots you want to create. Calibration boxplots will only be created for numerical features (not dummy variables)
#' @param ReturnModelObjects Set to TRUE to output all modeling objects (E.g. plots and evaluation metrics)
#' @param SaveModelObjects Set to TRUE to return all modeling objects to your environment
#' @param TaskType "GPU" or "CPU" for catboost training
#' @param NumGPUs Number of GPU's you would like to utilize
#' @param EvaluationMetric This is the metric used inside catboost to measure performance on validation data during a grid-tune. "RMSE" is the default, but other options include: "MAE", "MAPE", "Poisson", "Quantile", "LogLinQuantile", "Lq", "NumErrors", "SMAPE", "R2", "MSLE", "MedianAbsoluteError".
#' @param LossFunction Used in model training for model fitting. Select from 'RMSE', 'MAE', 'Quantile', 'LogLinQuantile', 'MAPE', 'Poisson', 'PairLogitPairwise', 'Tweedie', 'QueryRMSE' 
#' @param NumOfParDepPlots Number of partial dependence plots to return
#' @param MetricPeriods Number of trees to build before the internal catboost eval step happens
#' @param DT_Threads Number of threads to use for data.table. Default is Total - 2
#' @param ImputeRollStats Constant value to fill NA after running AutoLagRollStats()
#' @param CohortHolidayLags c(1L, 2L, 7L),
#' @param CohortHolidayMovingAverages c(3L, 7L),
#' @param CalendarHolidayLags c(1L, 2L, 7L),
#' @param CalendarHolidayMovingAverages = c(3L, 7L),
#' @param CalendarLags List of the form list("day" = c(1L, 7L, 21L), "week" = c(1L, 4L, 52L), "month" = c(1L, 6L, 12L))
#' @param CalendarMovingAverages List of the form list("day" = c(1L, 7L, 21L), "week" = c(1L, 4L, 52L), "month" = c(1L, 6L, 12L))
#' @param CalendarStandardDeviations List of the form list("day" = c(1L, 7L, 21L), "week" = c(1L, 4L, 52L), "month" = c(1L, 6L, 12L)) 
#' @param CalendarSkews List of the form list("day" = c(1L, 7L, 21L), "week" = c(1L, 4L, 52L), "month" = c(1L, 6L, 12L))
#' @param CalendarKurts List of the form list("day" = c(1L, 7L, 21L), "week" = c(1L, 4L, 52L), "month" = c(1L, 6L, 12L))
#' @param CalendarQuantiles List of the form list("day" = c(1L, 7L, 21L), "week" = c(1L, 4L, 52L), "month" = c(1L, 6L, 12L))
#' @param CalendarQuantilesSelected Supply a vector of "q5", "q10", "q15", "q20", "q25", "q30", "q35", "q40", "q45", "q50", "q55", "q60", "q65", "q70", "q75", "q80", "q85", "q90", "q95"
#' @param CohortLags List of the form list("day" = c(1L, 7L, 21L), "week" = c(1L, 4L, 52L), "month" = c(1L, 6L, 12L))
#' @param CohortMovingAverages List of the form list("day" = c(1L, 7L, 21L), "week" = c(1L, 4L, 52L), "month" = c(1L, 6L, 12L))
#' @param CohortStandardDeviations List of the form list("day" = c(1L, 7L, 21L), "week" = c(1L, 4L, 52L), "month" = c(1L, 6L, 12L)) 
#' @param CohortSkews List of the form list("day" = c(1L, 7L, 21L), "week" = c(1L, 4L, 52L), "month" = c(1L, 6L, 12L))
#' @param CohortKurts List of the form list("day" = c(1L, 7L, 21L), "week" = c(1L, 4L, 52L), "month" = c(1L, 6L, 12L))
#' @param CohortQuantiles List of the form list("day" = c(1L, 7L, 21L), "week" = c(1L, 4L, 52L), "month" = c(1L, 6L, 12L))
#' @param CohortQuantilesSelected Supply a vector of "q5", "q10", "q15", "q20", "q25", "q30", "q35", "q40", "q45", "q50", "q55", "q60", "q65", "q70", "q75", "q80", "q85", "q90", "q95"
#' @param CalendarVariables "wday", "mday", "yday", "week", "isoweek", "month", "quarter", "year"
#' @param HolidayGroups c("USPublicHolidays","EasterGroup","ChristmasGroup","OtherEcclesticalFeasts")
#' @param PassInGrid Defaults to NULL. Pass in a single row of grid from a previous output as a data.table (they are collected as data.tables)
#' @param GridTune Set to TRUE to run a grid tuning procedure. Set a number in MaxModelsInGrid to tell the procedure how many models you want to test.
#' @param BaselineComparison Set to either "default" or "best". Default is to compare each successive model build to the baseline model using max trees (from function args). Best makes the comparison to the current best model.
#' @param MaxModelsInGrid Number of models to test from grid options
#' @param MaxRunMinutes Maximum number of minutes to let this run
#' @param MaxRunsWithoutNewWinner Number of models built before calling it quits
#' @param Trees Bandit grid partitioned. The maximum number of trees you want in your models
#' @param Depth Bandit grid partitioned. Number, or vector for depth to test.  For running grid tuning, a NULL value supplied will mean these values are tested seq(4L, 16L, 2L)
#' @param LearningRate Bandit grid partitioned. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the LearningRate values to test. For running grid tuning, a NULL value supplied will mean these values are tested c(0.01,0.02,0.03,0.04)
#' @param L2_Leaf_Reg Random testing. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the L2_Leaf_Reg values to test. For running grid tuning, a NULL value supplied will mean these values are tested seq(1.0, 10.0, 1.0)
#' @param RSM CPU only. Random testing. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the RSM values to test. For running grid tuning, a NULL value supplied will mean these values are tested c(0.80, 0.85, 0.90, 0.95, 1.0)
#' @param BootStrapType Random testing. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the BootStrapType values to test. For running grid tuning, a NULL value supplied will mean these values are tested c("Bayesian", "Bernoulli", "Poisson", "MVS", "No")
#' @param GrowPolicy Random testing. NULL, character, or vector for GrowPolicy to test. For grid tuning, supply a vector of values. For running grid tuning, a NULL value supplied will mean these values are tested c("SymmetricTree", "Depthwise", "Lossguide")
#' @examples 
#' \donttest{
#' RemixAutoML::AutoCatBoostChainLadder(
#' 
#'    # Data Arguments----
#'    data = data,
#'    PartitionRatios = c(0.70,0.20,0.10),
#'    BaseFunnelMeasure = "Leads",
#'    ConversionMeasure = "Conversion",
#'    CohortPeriodsVariable = NULL,
#'    CalendarDate = "LeadDate",
#'    CohortDate = "ConversionDate",
#'    TimeUnit = "days",
#'    TransformTargetVariable = TRUE,
#'    TransformMethods = c("Identity","BoxCox", "Asinh", "Asin", "Log", "LogPlus1", "Logit", "YeoJohnson"),
#'    
#'    # MetaData Arguments----
#'    Jobs = c("eval","train"),
#'    SaveModelObjects = TRUE,
#'    ModelID = MarsSegment,
#'    ModelPath = Modeling,
#'    MetaDataPath = ModelData,
#'    TaskType = "CPU",
#'    NumGPUs = 1,
#'    DT_Threads = max(1L, parallel::detectCores() - 2L),
#'    EvaluationMetric = "RMSE",
#'    LossFunction = "RMSE",
#'    NumOfParDepPlots = 1L,
#'    MetricPeriods = 50L,
#'    
#'    # Feature Engineering Arguments----
#'    ImputeRollStats = -0.001,
#'    CalendarTimeGroups = c("days","weeks","months"),
#'    CohortTimeGroups = c("days", "weeks"),
#'    CalendarVariables = c("wday","mday","yday","week","month","quarter","year"),
#'    HolidayGroups = c("USPublicHolidays","EasterGroup","ChristmasGroup","OtherEcclesticalFeasts"),
#'    CohortHolidayLags = c(1L,2L,7L),
#'    CohortHolidayMovingAverages = c(3L,7L),
#'    CalendarHolidayLags = c(1L,2L,7L),
#'    CalendarHolidayMovingAverages = c(3L,7L),
#'    CalendarLags = list("day" = c(1L,2L,7L,35L,42L), "week" = c(5L,6L,10L,12L,25L,26L)),
#'    CalendarMovingAverages = list("day" = c(7L,14L,35L,42L), "week" = c(5L,6L,10L,12L,20L,24L), "month" = c(6L,12L)),
#'    CalendarStandardDeviations = NULL,
#'    CalendarSkews = NULL,
#'    CalendarKurts = NULL,
#'    CalendarQuantiles = NULL,
#'    CalendarQuantilesSelected = "q50",
#'    CohortLags = list("day" = c(1L,2L,7L,35L,42L), "week" = c(5L,6L)),
#'    CohortMovingAverages = list("day" = c(7L,14L,35L,42L), "week" = c(5L,6L), "month" = c(1L,2L)),
#'    CohortStandardDeviations = NULL,
#'    CohortSkews = NULL,
#'    CohortKurts = NULL,
#'    CohortQuantiles = NULL,
#'    CohortQuantilesSelected = "q50",
#'    
#'    # Grid Tuning
#'    PassInGrid = NULL,
#'    GridTune = FALSE,
#'    BaselineComparison = "default",
#'    MaxModelsInGrid = 25L,
#'    MaxRunMinutes = 180L,
#'    MaxRunsWithoutNewWinner = 10L,
#'    Trees = 4000L,
#'    Depth = seq(4L,8L,1L),
#'    LearningRate = seq(0.01,0.10,0.01),
#'    L2_Leaf_Reg = seq(1.0,10.0,1.0),
#'    RSM = c(0.80,0.85,0.90,0.95,1.0),
#'    BootStrapType = c("Bayesian","Bernoulli","Poisson","MVS","No"),
#'    GrowPolicy = c("SymmetricTree","Depthwise","Lossguide"))
#' }
#' @return Saves metadata and models to files of your choice. Also returns metadata and models from the function. User specifies both options.
#' @export
AutoCatBoostChainLadder <- function(data,
                                    PartitionRatios = c(0.70,0.20,0.10),
                                    BaseFunnelMeasure = NULL,
                                    ConversionMeasure = NULL,
                                    CohortPeriodsVariable = NULL,
                                    CalendarDate = NULL,
                                    CohortDate = NULL,
                                    TimeUnit = c("day"),
                                    CalendarTimeGroups = c("day","week","month"),
                                    CohortTimeGroups = c("day","week","month"),
                                    TransformTargetVariable = TRUE,
                                    TransformMethods = c("Identity","YeoJohnson"),
                                    Jobs = c("Evaluate","Train"),
                                    SaveModelObjects = TRUE,
                                    ModelID = "Segment_ID",
                                    ModelPath = NULL,
                                    MetaDataPath = NULL,
                                    TaskType = "CPU",
                                    NumGPUs = 1,
                                    DT_Threads = max(1L, parallel::detectCores() - 2L),
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
  if(is.null(DT_Threads)) data.table::setDTthreads(threads = max(1L, parallel::detectCores() - 2L)) else data.table::setDTthreads(threads = eval(DT_Threads))
  
  # Ensure ModelPath and MetaDataPath exists----
  if(!is.null(ModelPath)) if(!dir.exists(file.path(normalizePath(ModelPath)))) dir.create(normalizePath(ModelPath))
  if(!is.null(MetaDataPath)) if(!is.null(MetaDataPath)) if(!dir.exists(file.path(normalizePath(MetaDataPath)))) dir.create(normalizePath(MetaDataPath))
  
  # Args List----
  ArgsList <- list()
  if(!is.null(PartitionRatios)) ArgsList[["PartitionRatios"]] <- PartitionRatios else ArgsList[["PartitionRatios"]] <- c(0.70,0.20,0.10)
  ArgsList[["Algorithm"]] <- "catboost"
  ArgsList[["BaseFunnelMeasure"]] <- BaseFunnelMeasure
  ArgsList[["ConversionMeasure"]] <- ConversionMeasure
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
  }
  ArgsList[["CohortPeriodsVariable"]] <- CohortPeriodsVariable
  
  # ML Process: Train and Evaluate Models----
  for(proc in Jobs) {
    
    # Init: Function Similify----
    proc <- tolower(proc)
    
    # FE: CreateCalendarVariables() CalendarDate and CohortDate----
    x <- system.time(gcFirst = FALSE, data <- RemixAutoML::CreateCalendarVariables(data, DateCols = c(eval(CalendarDate), eval(CohortDate)), AsFactor = FALSE, TimeUnits = CalendarVariables))
    if(proc %chin% c("evaluate","eval")) {
      data.table::set(TimerDataEval, i = 2L, j = "Time", value = x[[3L]])
      data.table::set(TimerDataEval, i = 2L, j = "Process", value = "# Add CalendarDate and CohortDate calendar variables----")
    } else if(proc %chin% c("train")) {
      data.table::set(TimerDataTrain, i = 2L, j = "Time", value = x[[3L]])
      data.table::set(TimerDataTrain, i = 2L, j = "Process", value = "# Add CalendarDate and CohortDate calendar variables----")
    }
    
    # Save Timers to file
    SaveTimers(SaveModelObjectss = SaveModelObjects, procs = proc, TimerDataEvals = TimerDataEval, TimerDataTrains = TimerDataTrain, MetaDataPaths = MetaDataPath, ModelIDs = ModelID)
    
    # FE: CreateHolidayVariables() CalendarDate----
    x <- system.time(gcFirst = FALSE, data <- RemixAutoML::CreateHolidayVariables(data, DateCols = eval(CalendarDate), HolidayGroups = HolidayGroups, Holidays = NULL, GroupingVars = NULL, Print = FALSE))
    data.table::setnames(data, old = "HolidayCounts", new = paste0(BaseFunnelMeasure,"HolidayCounts"))
    if(proc %chin% c("evaluate","eval")) {
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
    data.table::setnames(data, old = "HolidayCounts", new = paste0(ConversionMeasure, "HolidayCounts"))
    if(proc %chin% c("evaluate","eval")) {
      data.table::set(TimerDataEval, i = 4L, j = "Time", value = x[[3L]])
      data.table::set(TimerDataEval, i = 4L, j = "Process", value = "# Add CohortDate holiday variables----")
    } else if(proc %chin% c("training","train")) {
      data.table::set(TimerDataTrain, i = 4L, j = "Time", value = x[[3L]])
      data.table::set(TimerDataTrain, i = 4L, j = "Process", value = "# Add CohortDate holiday variables----")
    }
    
    # DM: Type Casting CalendarDate to Character to be used as a Grouping Variable----
    x <- system.time(gcFirst = FALSE, data.table::set(data, j = eval(CalendarDate), value = as.character(data[[eval(CalendarDate)]])))
    if(proc %chin% c("evaluate","eval")) {
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
    if(proc %chin% c("evaluate","eval")) {
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
    if(proc %in% c("evaluate","eval","train","training")) {
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
    
    # FE: AutoLagRollStats() ConversionMeasure HolidayCounts with CalendarDate as a Grouping Variable----
    if(proc %in% c("evaluate","eval","train","training") & !is.null(CohortHolidayLags)) {
      x <- system.time(gcFirst = FALSE, data <- RemixAutoML::AutoLagRollStats(
        
        # Data
        data                 = data,
        DateColumn           = eval(CohortDate),
        Targets              = paste0(ConversionMeasure, "HolidayCounts"),
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
      
      # Save Timers to file
      SaveTimers(SaveModelObjectss = SaveModelObjects, procs = proc, TimerDataEvals = TimerDataEval, TimerDataTrains = TimerDataTrain, MetaDataPaths = MetaDataPath, ModelIDs = ModelID)
    }
    
    # FE: AutoLagRollStats() BaseFunnelMeasure Over Calendar Time----
    if(proc %chin% c("evaluate","eval","training","train")) {
      temp <- data[, data.table::first(get(BaseFunnelMeasure)), by = eval(CalendarDate)]
      data.table::setnames(temp, "V1", eval(BaseFunnelMeasure))
      x <- system.time(gcFirst = FALSE, temp <- RemixAutoML::AutoLagRollStats(
        
        # Data
        data                 = temp,
        DateColumn           = eval(CalendarDate),
        Targets              = eval(BaseFunnelMeasure),
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
        data.table::set(TimerDataEval, i = 9L, j = "Process", value = "# Rolling stats for BaseFunnelMeasure----")
      } else if(proc %chin% c("training","train")) {
        data.table::set(TimerDataTrain, i = 9L, j = "Time", value = x[[3L]])
        data.table::set(TimerDataTrain, i = 9L, j = "Process", value = "# Rolling stats for BaseFunnelMeasure----")
      }
    }
    
    # Join back to data
    data <- merge(data, temp[, .SD, .SDcols = c(eval(CalendarDate), setdiff(names(temp), names(data)))], by = eval(CalendarDate), all = FALSE)
    
    # FE: AutoLagRollStats() ConversionMeasure Over Calendar Time----
    if(proc %chin% c("evaluate","eval","training","train")) {
      temp <- data[get(CohortDate) == get(CalendarDate), sum(get(ConversionMeasure)), by = eval(CalendarDate)]
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
        data.table::set(TimerDataEval, i = 9L, j = "Process", value = "# Rolling stats for BaseFunnelMeasure----")
      } else if(proc %chin% c("training","train")) {
        data.table::set(TimerDataTrain, i = 9L, j = "Time", value = x[[3L]])
        data.table::set(TimerDataTrain, i = 9L, j = "Process", value = "# Rolling stats for BaseFunnelMeasure----")
      }
    }
    
    # Join back to data
    data <- merge(data, temp[, .SD, .SDcols = c(eval(CalendarDate), setdiff(names(temp), names(data)))], by = eval(CalendarDate), all = FALSE)
    
    # FE: AutoLagRollStats() CalendarHolidayCounts Over Calendar Time----
    if(proc %chin% c("evaluate","eval","training","train") & !is.null(CalendarHolidayLags)) {
      temp <- data[, max(get(paste0(BaseFunnelMeasure, "HolidayCounts"))), by = eval(CalendarDate)]
      data.table::setnames(temp, "V1", paste0(BaseFunnelMeasure, "HolidayCounts"))
      x <- system.time(gcFirst = FALSE, temp <- RemixAutoML::AutoLagRollStats(
        
        # Data
        data                 = temp,
        DateColumn           = eval(CalendarDate),
        Targets              = paste0(BaseFunnelMeasure, "HolidayCounts"),
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
    }
    
    # Join back to data
    data <- merge(data, temp[, .SD, .SDcols = c(eval(CalendarDate), setdiff(names(temp), names(data)))], by = eval(CalendarDate), all = FALSE)
    
    # FE: ModelDataPrep() Impute Numeric Columns from AutoLagRollStats()----
    x <- system.time(gcFirst = FALSE, data <- RemixAutoML::ModelDataPrep(
      data         = data,
      Impute       = TRUE,
      CharToFactor = FALSE,
      FactorToChar = FALSE,
      IntToNumeric = FALSE,
      DateToChar   = FALSE,
      RemoveDates  = FALSE,
      MissFactor   = "0",
      MissNum      = ImputeRollStats,
      IgnoreCols   = NULL))
    if(proc %chin% c("evaluate","eval")) {
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
    if(!all(class(data[[eval(CalendarDate)]]) %chin% "Date")) data[, eval(CalendarDate) := as.Date(CalendarDate)]
    if(!all(class(data[[eval(CohortDate)]]) %chin% "Date")) data[, eval(CohortDate) := as.Date(CohortDate)]
    
    # DM: Partition Data----
    if(proc %chin% c("evaluate","eval")) {
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
      rm(DataSets, data)
    }
    
    #----
    
    #----
    
    # ML: CatBoostRegression()----
    if(proc %chin% c("evaluate","eval","training","train")) {
      
      # Define features----
      Features <- names(TrainData)[c(4L, 5L, 8L:ncol(TrainData))]
      
      # Define number of trees----
      if(proc %chin% c("eval","evaluation")) NTrees <- Trees
      
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
        IDcols = names(TrainData)[!names(TrainData) %in% Features],
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
      } else {
        NTrees <- Trees
      }
      
      # Store results----
      if(SaveModelObjects) {
        if(proc %chin% c("evaluate","eval")) {
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
        if(proc %chin% c("evaluate","eval")) {
          save(TestModel, file = file.path(normalizePath(ModelPath), paste0(ModelID, "_Evaluation.Rdata")))
        } else if(proc %chin% c("training","train")) {
          save(TestModel, file = file.path(normalizePath(ModelPath), paste0(ModelID, "_FinalTrain.Rdata")))
        }
      }
      
      # Remove objects before next run----
      if(proc %chin% c("evaluate","eval")) {
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
