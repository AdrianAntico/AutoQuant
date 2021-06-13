#' @noRd
SaveTimers <- function(SaveModelObjectss = SaveModelObjects, procs = proc, TimerDataEvals = TimerDataEval, TimerDataTrains = TimerDataTrain, MetaDataPaths = MetaDataPath, ModelIDs = ModelID) {
  if(SaveModelObjectss) {
    if(procs %chin% c("evaluate","eval")) {
      data.table::fwrite(TimerDataEvals[Process != "a"], file = file.path(MetaDataPaths, paste0(ModelIDs, "_Eval_Timer.csv")))
    } else if(procs %chin% c("training","train")) {
      data.table::fwrite(TimerDataTrains[Process != "a"], file = file.path(MetaDataPaths, paste0(ModelIDs, "_Train_Timer.csv")))
    }
  }
}

#' @title AutoCatBoostFunnelCARMA
#'
#' @description AutoCatBoostFunnelCARMA is a forecasting model for cohort funnel forecasting for grouped data or non-grouped data
#'
#' @author Adrian Antico
#' @family Automated Funnel Data Forecasting
#'
#' @param data data object
#' @param PartitionRatios Requires three values for train, validation, and test data sets
#' @param BaseFunnelMeasure E.g. "Leads". This value should be a forward looking variable. Say you want to forecast ConversionMeasure 2 months into the future. You should have two months into the future of values of BaseFunnelMeasure
#' @param ConversionMeasure E.g. "Conversions". Rate is derived as conversions over leads by cohort periods out
#' @param ConversionRateMeasure Conversions over Leads for every cohort
#' @param CohortPeriodsVariable Numeric. Numerical value of the the number of periods since cohort base date.
#' @param CalendarDate The name of your date column that represents the calendar date
#' @param CohortDate The name of your date column that represents the cohort date
#' @param TruncateDate NULL. Supply a date to represent the earliest point in time you want in your data. Filtering takes place before partitioning data so feature engineering can include as many non null values as possible.
#' @param TimeUnit Base time unit of data. "days", "weeks", "months", "quarters", "years"
#' @param TransformTargetVariable TRUE or FALSe
#' @param TransformMethods Choose from "Identity", "BoxCox", "Asinh", "Asin", "Log", "LogPlus1", "Logit", "YeoJohnson"
#' @param AnomalyDetection Provide a named list. See examples
#' @param Jobs Default is "eval" and "train"
#' @param CalendarTimeGroups TimeUnit value must be included. If you want to generate lags and moving averages in several time based aggregations, choose from "days", "weeks", "months", "quarters", "years".
#' @param CohortTimeGroups TimeUnit value must be included. If you want to generate lags and moving averages in several time based aggregations, choose from "days", "weeks", "months", "quarters", "years".
#' @param ModelPath Path to where you want your models saved
#' @param MetaDataPath Path to where you want your metadata saved. If NULL, function will try ModelPath if it is not NULL.
#' @param DebugMode Internal use
#' @param ModelID A character string to name your model and output
#' @param NumOfParDepPlots Tell the function the number of partial dependence calibration plots you want to create. Calibration boxplots will only be created for numerical features (not dummy variables)
#' @param SaveModelObjects Set to TRUE to return all modeling objects to your environment
#' @param TaskType "GPU" or "CPU" for catboost training
#' @param NumGPUs Number of GPU's you would like to utilize
#' @param EvaluationMetric This is the metric used inside catboost to measure performance on validation data during a grid-tune. "RMSE" is the default, but other options include: "MAE", "MAPE", "Poisson", "Quantile", "LogLinQuantile", "Lq", "NumErrors", "SMAPE", "R2", "MSLE", "MedianAbsoluteError".
#' @param LossFunction Used in model training for model fitting. Select from 'RMSE', 'MAE', 'Quantile', 'LogLinQuantile', 'MAPE', 'Poisson', 'PairLogitPairwise', 'Tweedie', 'QueryRMSE'
#' @param NumOfParDepPlots Number of partial dependence plots to return
#' @param MetricPeriods Number of trees to build before the internal catboost eval step happens
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
#' @param HolidayLookback Number of days in range to compute number of holidays from a given date in the data. If NULL, the number of days are computed for you.
#' @param PassInGrid Defaults to NULL. Pass in a single row of grid from a previous output as a data.table (they are collected as data.tables)
#' @param GridTune Set to TRUE to run a grid tuning procedure. Set a number in MaxModelsInGrid to tell the procedure how many models you want to test.
#' @param BaselineComparison Set to either "default" or "best". Default is to compare each successive model build to the baseline model using max trees (from function args). Best makes the comparison to the current best model.
#' @param MaxModelsInGrid Number of models to test from grid options
#' @param MaxRunMinutes Maximum number of minutes to let this run
#' @param MaxRunsWithoutNewWinner Number of models built before calling it quits
#' @param Langevin Enables the Stochastic Gradient Langevin Boosting mode. If TRUE and TaskType == 'GPU' then TaskType will be converted to 'CPU'
#' @param DiffusionTemperature Default is 10000
#' @param Trees Select the number of trees you want to have built to train the model
#' @param Depth Depth of catboost model
#' @param L2_Leaf_Reg l2 reg parameter
#' @param LearningRate Defaults to NULL. Catboost will dynamically define this if L2_Leaf_Reg is NULL and RMSE is chosen (otherwise catboost will default it to 0.03). Then you can pull it out of the model object and pass it back in should you wish.
#' @param RandomStrength Default is 1
#' @param BorderCount Default is 254
#' @param RSM CPU only. If TaskType is GPU then RSM will not be used
#' @param BootStrapType If NULL, then if TaskType is GPU then Bayesian will be used. If CPU then MVS will be used. If MVS is selected when TaskType is GPU, then BootStrapType will be switched to Bayesian
#' @param GrowPolicy Default is SymmetricTree. Others include Lossguide and Depthwise
#' @param ModelSizeReg Defaults to 0.5. Set to 0 to allow for bigger models. This is for models with high cardinality categorical features. Valuues greater than 0 will shrink the model and quality will decline but models won't be huge.
#' @param FeatureBorderType Defaults to 'GreedyLogSum'. Other options include: Median, Uniform, UniformAndQuantiles, MaxLogSum, MinEntropy
#' @param SamplingUnit Default is Group. Other option is Object. if GPU is selected, this will be turned off unless the loss_function is YetiRankPairWise
#' @param SubSample Can use if BootStrapType is neither Bayesian nor No. Pass NULL to use Catboost default. Used for bagging.
#' @param ScoreFunction Default is Cosine. CPU options are Cosine and L2. GPU options are Cosine, L2, NewtonL2, and NewtomCosine (not available for Lossguide)
#' @param MinDataInLeaf Defaults to 1. Used if GrowPolicy is not SymmetricTree
#'
#' @examples
#' \dontrun{
#' # Create Fake Data
#' data <- RemixAutoML::FakeDataGenerator(ChainLadderData = TRUE)
#'
#' # Subset data for training
#' ModelDataBase <- data[CalendarDateColumn < '2020-01-01' & CohortDateColumn < '2020-01-01']
#' ModelData <- data.table::copy(ModelDataBase)
#'
#' # Train Funne Model
#' TestModel <- RemixAutoML::AutoCatBoostFunnelCARMA(
#'
#'   # Data Arguments
#'   data = ModelData,
#'   GroupVariables = NULL,
#'   BaseFunnelMeasure = "Leads", # if you have XREGS, supply vector such as c("Leads", "XREGS1", "XREGS2")
#'   ConversionMeasure = "Appointments",
#'   ConversionRateMeasure = NULL,
#'   CohortPeriodsVariable = "CohortDays",
#'   CalendarDate = "CalendarDateColumn",
#'   CohortDate = "CohortDateColumn",
#'   PartitionRatios = c(0.70,0.20,0.10),
#'   TruncateDate = NULL,
#'   TimeUnit = "days",
#'   TransformTargetVariable = TRUE,
#'   TransformMethods = c("Asinh","Asin","Log","LogPlus1","Sqrt","Logit"),
#'   AnomalyDetection = list(tstat_high = 3, tstat_low = -2),
#'
#'   # MetaData Arguments
#'   Jobs = c("eval","train"),
#'   SaveModelObjects = FALSE,
#'   ModelID = "ModelTest",
#'   ModelPath = getwd(),
#'   MetaDataPath = NULL,
#'   DebugMode = TRUE,
#'   NumOfParDepPlots = 1L,
#'
#'   # Feature Engineering Arguments
#'   CalendarTimeGroups = c("days","weeks","months"),
#'   CohortTimeGroups = c("days", "weeks"),
#'   CalendarVariables = c("wday","mday","yday","week","month","quarter","year"),
#'   HolidayGroups = c("USPublicHolidays","EasterGroup","ChristmasGroup","OtherEcclesticalFeasts"),
#'   HolidayLookback = NULL,
#'   CohortHolidayLags = c(1L,2L,7L),
#'   CohortHolidayMovingAverages = c(3L,7L),
#'   CalendarHolidayLags = c(1L,2L,7L),
#'   CalendarHolidayMovingAverages = c(3L,7L),
#'
#'   # Time Series Features
#'   ImputeRollStats = -0.001,
#'   CalendarLags = list("day" = c(1L,2L,7L,35L,42L), "week" = c(5L,6L,10L,12L,25L,26L)),
#'   CalendarMovingAverages = list("day" = c(7L,14L,35L,42L), "week" = c(5L,6L,10L,12L,20L,24L), "month" = c(6L,12L)),
#'   CalendarStandardDeviations = NULL,
#'   CalendarSkews = NULL,
#'   CalendarKurts = NULL,
#'   CalendarQuantiles = NULL,
#'   CalendarQuantilesSelected = "q50",
#'   CohortLags = list("day" = c(1L,2L,7L,35L,42L), "week" = c(5L,6L)),
#'   CohortMovingAverages = list("day" = c(7L,14L,35L,42L), "week" = c(5L,6L), "month" = c(1L,2L)),
#'   CohortStandardDeviations = NULL,
#'   CohortSkews = NULL,
#'   CohortKurts = NULL,
#'   CohortQuantiles = NULL,
#'   CohortQuantilesSelected = "q50",
#'
#'   # ML Grid Tuning
#'   PassInGrid = NULL,
#'   GridTune = FALSE,
#'   BaselineComparison = "default",
#'   MaxModelsInGrid = 25L,
#'   MaxRunMinutes = 180L,
#'   MaxRunsWithoutNewWinner = 10L,
#'
#'   # ML Setup Parameters
#'   MetricPeriods = 10,
#'   LossFunction = 'MAE',
#'   EvaluationMetric = 'MAE',
#'   TaskType = "CPU",
#'   NumGPUs = 1,
#'
#'   # ML Parameters
#'   Trees = 3000L,
#'   Depth = 8L,
#'   L2_Leaf_Reg = NULL,
#'   LearningRate = NULL,
#'   Langevin = FALSE,
#'   DiffusionTemperature = 10000,
#'   RandomStrength = 1,
#'   BorderCount = 254,
#'   RSM = NULL,
#'   GrowPolicy = "SymmetricTree",
#'   BootStrapType = "Bayesian",
#'   ModelSizeReg = 0.5,
#'   FeatureBorderType = "GreedyLogSum",
#'   SamplingUnit = "Group",
#'   SubSample = NULL,
#'   ScoreFunction = "Cosine",
#'   MinDataInLeaf = 1)
#'
#' # Separate out the Base Funnel Measures Data
#' LeadsData <- data[, lapply(.SD, data.table::first), .SDcols = c("Leads"), by = c("CalendarDateColumn")]
#' ModelData <- ModelDataBase[, Leads := NULL]
#'
#' # Forecast Funnel Model
#' Test <- RemixAutoML::AutoCatBoostFunnelCARMAScoring(
#'   TrainData = ModelData,
#'   ForwardLookingData = LeadsData,
#'   TrainEndDate = ModelData[, max(CalendarDateColumn)],
#'   ForecastEndDate = LeadsData[, max(CalendarDateColumn)],
#'   TrainOutput = TestModel$ModelOutput,
#'   ArgsList = TestModel$ArgsList,
#'   ModelPath = NULL,
#'   MaxCohortPeriod = 15,
#'   DebugMode = TRUE)
#' }
#' @export
AutoCatBoostFunnelCARMA <- function(data,
                                    GroupVariables = NULL,
                                    BaseFunnelMeasure = NULL,
                                    ConversionMeasure = NULL,
                                    ConversionRateMeasure = NULL,
                                    CohortPeriodsVariable = NULL,
                                    CalendarDate = NULL,
                                    CohortDate = NULL,

                                    # Additional Services
                                    TruncateDate = NULL,
                                    PartitionRatios = c(0.70,0.20,0.10),
                                    TimeUnit = c("day"),
                                    CalendarTimeGroups = c("day","week","month"),
                                    CohortTimeGroups = c("day","week","month"),
                                    TransformTargetVariable = TRUE,
                                    TransformMethods = c("Identity","YeoJohnson"),
                                    AnomalyDetection = list(tstat_high = 3, tstat_low = -2),

                                    # Production
                                    Jobs = c("Evaluate","Train"),
                                    SaveModelObjects = TRUE,
                                    ModelID = "Segment_ID",
                                    ModelPath = NULL,
                                    MetaDataPath = NULL,
                                    DebugMode = FALSE,

                                    # Date Features
                                    CalendarVariables = c("wday","mday","yday","week","isoweek","month","quarter","year"),
                                    HolidayGroups = c("USPublicHolidays","EasterGroup","ChristmasGroup","OtherEcclesticalFeasts"),
                                    HolidayLookback = NULL,

                                    # Time Series Holiday Features
                                    CohortHolidayLags = c(1L, 2L, 7L),
                                    CohortHolidayMovingAverages = c(3L, 7L),
                                    CalendarHolidayLags = c(1L, 2L, 7L),
                                    CalendarHolidayMovingAverages = c(3L, 7L),

                                    # Time Series Features
                                    ImputeRollStats = -0.001,
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

                                    # Grid Tuning
                                    PassInGrid = NULL,
                                    GridTune = FALSE,
                                    BaselineComparison = "default",
                                    MaxModelsInGrid = 25L,
                                    MaxRunMinutes = 180L,
                                    MaxRunsWithoutNewWinner = 10L,

                                    # Compute Parameters
                                    TaskType = "CPU",
                                    NumGPUs = 1,

                                    # ML Setup Parameters
                                    EvaluationMetric = "RMSE",
                                    LossFunction = "RMSE",
                                    MetricPeriods = 50L,
                                    NumOfParDepPlots = 1L,

                                    # ML Parameters
                                    Trees = 3000L,
                                    Depth = 8L,
                                    L2_Leaf_Reg = NULL,
                                    LearningRate = NULL,
                                    Langevin = FALSE,
                                    DiffusionTemperature = 10000,
                                    RandomStrength = 1,
                                    BorderCount = 254,
                                    RSM = NULL,
                                    GrowPolicy = "SymmetricTree",
                                    BootStrapType = "Bayesian",
                                    ModelSizeReg = 0.5,
                                    FeatureBorderType = "GreedyLogSum",
                                    SamplingUnit = "Group",
                                    SubSample = NULL,
                                    ScoreFunction = "Cosine",
                                    MinDataInLeaf = 1) {

  # Admin: Get catboost loaded ----
  loadNamespace(package = "catboost")

  # Admin: Ensure ModelPath and MetaDataPath exists ----
  if(!is.null(ModelPath)) if(!dir.exists(file.path(ModelPath))) dir.create(ModelPath)
  if(!is.null(MetaDataPath)) if(!is.null(MetaDataPath)) if(!dir.exists(file.path(MetaDataPath))) dir.create(MetaDataPath)

  # Admin: Args List ----
  ArgsList <- list()

  # Model Selection
  ArgsList[["Algorithm"]] <- "catboost"

  # Data Args
  ArgsList[["GroupVariables"]] <- GroupVariables
  ArgsList[["BaseFunnelMeasure"]] <- BaseFunnelMeasure
  ArgsList[["ConversionMeasure"]] <- ConversionMeasure
  ArgsList[["CohortPeriodsVariable"]] <- CohortPeriodsVariable
  ArgsList[["CalendarDate"]] <- CalendarDate
  ArgsList[["CohortDate"]] <- CohortDate
  ArgsList[["AnomalyDetection"]] <- AnomalyDetection
  ArgsList[["TimeUnit"]] <- TimeUnit
  ArgsList[["CalendarTimeGroups"]] <- CalendarTimeGroups
  ArgsList[["CohortTimeGroups"]] <- CohortTimeGroups

  # Metadata Args
  if(is.null(MetaDataPath)) if(!is.null(ModelPath)) MetaDataPath <- ModelPath
  ArgsList[["ModelID"]] <- ModelID
  ArgsList[["ModelPath"]] <- ModelPath
  ArgsList[["MetaDataPath"]] <- MetaDataPath
  ArgsList[["Jobs"]] <- Jobs

  # Feature Engineering Args
  ArgsList[["TransformTargetVariable"]] <- TransformTargetVariable
  ArgsList[["CalendarVariables"]] <- CalendarVariables
  ArgsList[["HolidayGroups"]] <- HolidayGroups
  ArgsList[["HolidayLookback"]] <- HolidayLookback
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
  ArgsList[["PartitionRatios"]] <- if(!is.null(PartitionRatios)) PartitionRatios else c(0.70,0.20,0.10)
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

  # Grid tuning args
  ArgsList[["PassInGrid"]] <- PassInGrid
  ArgsList[["GridTune"]] <- GridTune
  ArgsList[["BaselineComparison"]] <- BaselineComparison
  ArgsList[["MaxModelsInGrid"]] <- MaxModelsInGrid
  ArgsList[["MaxRunMinutes"]] <- MaxRunMinutes
  ArgsList[["MaxRunsWithoutNewWinner"]] <- MaxRunsWithoutNewWinner

  # CatBoost Args
  ArgsList[["LossFunction"]] <- LossFunction
  ArgsList[["EvaluationMetric"]] <- EvaluationMetric
  ArgsList[["TaskType"]] <- TaskType
  ArgsList[["NumGPUs"]] <- NumGPUs

  # ML Args
  ArgsList[["Trees"]] <- Trees
  ArgsList[["Depth"]] <- Depth
  ArgsList[["LearningRate"]] <- LearningRate
  ArgsList[["L2_Leaf_Reg"]] <- L2_Leaf_Reg
  ArgsList[["RSM"]] <- RSM
  ArgsList[["BootStrapType"]] <- BootStrapType
  ArgsList[["GrowPolicy"]] <- GrowPolicy
  ArgsList[["Langevin"]] <- Langevin
  ArgsList[["DiffusionTemperature"]] <- DiffusionTemperature
  ArgsList[["RandomStrength"]] <- RandomStrength
  ArgsList[["BorderCount"]] <- BorderCount
  ArgsList[["ModelSizeReg"]] <- ModelSizeReg
  ArgsList[["FeatureBorderType"]] <- FeatureBorderType
  ArgsList[["SamplingUnit"]] <- SamplingUnit
  ArgsList[["SubSample"]] <- SubSample
  ArgsList[["ScoreFunction"]] <- ScoreFunction
  ArgsList[["MinDataInLeaf"]] <- MinDataInLeaf

  # Admin: Special Args ----
  ArgsList[[paste0("Min","-", eval(CalendarDate))]] <- data[, min(get(CalendarDate))][[1L]]
  ArgsList[[paste0("Max","-", eval(CalendarDate))]] <- data[, max(get(CalendarDate))][[1L]]

  # Admin: Create timers ----
  TimerDataEval <- data.table::data.table(Process = rep("a", 25L), Time = rep(999, 25L))
  TimerDataTrain <- data.table::data.table(Process = rep("a", 25L), Time = rep(999, 25L))

  # DM: Convert data to data.table ----
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  # DM: Type Casting CalendarDate and CohortDate to Date or POSIXct ----
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

  # FE: Create CohortPeriodsVariable With GroupVariables ----
  if(is.null(CohortPeriodsVariable)) {
    data[, CohortPeriods := as.numeric(difftime(time1 = get(CohortDate), time2 = get(CalendarDate), units = eval(TimeUnit)))]
    CohortPeriodsVariable <- "CohortPeriods"
    ArgsList[["CohortPeriodsVariable"]] <- CohortPeriodsVariable
  }

  # DM: GroupVar ----
  if(!is.null(GroupVariables)) {
    data[, paste0("Temp_", CohortPeriodsVariable) := do.call(paste, c(.SD, sep = ' ')), .SDcols = c(GroupVariables, CohortPeriodsVariable)]
    data[, paste0("Temp_", CalendarDate) := do.call(paste, c(.SD, sep = ' ')), .SDcols = c(GroupVariables, CalendarDate)]
    data[, GroupVar := do.call(paste, c(.SD, sep = ' ')), .SDcols = c(GroupVariables)]
    ArgsList[[paste0("Temp_", CohortPeriodsVariable)]] <- paste0("Temp_", CohortPeriodsVariable)
    ArgsList[[paste0("Temp_", CalendarDate)]] <- paste0("Temp_", CalendarDate)
    ArgsList[["GroupVar"]] <- "GroupVar"
  } else {
    data[, paste0("Temp_", CohortPeriodsVariable) := do.call(paste, c(.SD, sep = ' ')), .SDcols = c(CohortPeriodsVariable)]
    data[, paste0("Temp_", CalendarDate) := do.call(paste, c(.SD, sep = ' ')), .SDcols = c(CalendarDate)]
    ArgsList[[paste0("Temp_", CohortPeriodsVariable)]] <- paste0("Temp_", CohortPeriodsVariable)
    ArgsList[[paste0("Temp_", CalendarDate)]] <- paste0("Temp_", CalendarDate)
  }

  # FE: Create ConversionRateMeasure if NULL ----
  if(is.null(ConversionRateMeasure)) {
    data[, ConversionRate := data.table::fifelse(get(BaseFunnelMeasure[1L]) == 0, 0, get(ConversionMeasure) / get(BaseFunnelMeasure[1L]))]
    ConversionRateMeasure <- "ConversionRate"
    ArgsList[["ConversionRateMeasure"]] <- ConversionRateMeasure
  } else {
    ArgsList[["ConversionRateMeasure"]] <- ConversionRateMeasure
  }

  # DM: Remove columns ----
  if("GroupVar" %chin% names(data)) {
    drop <- setdiff(names(data), c("GroupVar", GroupVariables, paste0("Temp_", CohortPeriodsVariable), paste0("Temp_", CalendarDate), BaseFunnelMeasure, ConversionMeasure, ConversionRateMeasure, CohortPeriodsVariable, CalendarDate, CohortDate))
  } else {
    drop <- setdiff(names(data), c(paste0("Temp_", CohortPeriodsVariable), paste0("Temp_", CalendarDate), BaseFunnelMeasure, ConversionMeasure, ConversionRateMeasure, CohortPeriodsVariable, CalendarDate, CohortDate))
  }
  if(!identical(drop, character(0))) data.table::set(data, j = c(drop), value = NULL)

  # ML Process: Train and Evaluate Models ----
  for(proc in Jobs) {

    # Admin: Function Similify ----
    proc <- tolower(proc)

    # DE: Copy data----
    if(proc %chin% c("evaluate","eval","evaluation")) {
      data1 <- data.table::copy(data)
    } else {
      data <- data1
      rm(data1)
    }

    # FE: CreateCalendarVariables() CalendarDate and CohortDate ----
    if(DebugMode) print("FE: CreateCalendarVariables() CalendarDate and CohortDate ----")
    x <- system.time(gcFirst = FALSE, data <- RemixAutoML::CreateCalendarVariables(data, DateCols = c(eval(CalendarDate)), AsFactor = FALSE, TimeUnits = CalendarVariables))
    data <- RemixAutoML::CreateCalendarVariables(data, DateCols = c(eval(CohortDate)), AsFactor = FALSE, TimeUnits = CalendarVariables)
    if(proc %chin% c("evaluate","eval","evaluation")) {
      data.table::set(TimerDataEval, i = 2L, j = "Time", value = x[[3L]])
      data.table::set(TimerDataEval, i = 2L, j = "Process", value = "# Add CalendarDate and CohortDate calendar variables----")
    } else if(proc %chin% c("training","train")) {
      data.table::set(TimerDataTrain, i = 2L, j = "Time", value = x[[3L]])
      data.table::set(TimerDataTrain, i = 2L, j = "Process", value = "# Add CalendarDate and CohortDate calendar variables----")
    }

    # Save Timers to file
    SaveTimers(SaveModelObjectss = SaveModelObjects, procs = proc, TimerDataEvals = TimerDataEval, TimerDataTrains = TimerDataTrain, MetaDataPaths = MetaDataPath, ModelIDs = ModelID)

    # FE: CreateHolidayVariables() CalendarDate ----
    if(DebugMode) print("FE: CreateHolidayVariables() CalendarDate ----")
    x <- system.time(gcFirst = FALSE, data <- RemixAutoML::CreateHolidayVariables(data, DateCols = eval(CalendarDate), LookbackDays = if(!is.null(HolidayLookback)) HolidayLookback else LB(TimeUnit), HolidayGroups = HolidayGroups, Holidays = NULL, Print = FALSE))
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

    # FE: CreateHolidayVariables() CohortDate ----
    if(DebugMode) print("FE: CreateHolidayVariables() CohortDate ----")
    x <- system.time(gcFirst = FALSE, data <- RemixAutoML::CreateHolidayVariables(data, DateCols = eval(CohortDate), LookbackDays = if(!is.null(HolidayLookback)) HolidayLookback else LB(TimeUnit), HolidayGroups = eval(HolidayGroups), Holidays = NULL, Print = FALSE))
    data.table::setnames(data, old = "HolidayCounts", new = paste0(CohortDate, "HolidayCounts"))
    if(proc %chin% c("evaluate","eval","evaluation")) {
      data.table::set(TimerDataEval, i = 4L, j = "Time", value = x[[3L]])
      data.table::set(TimerDataEval, i = 4L, j = "Process", value = "# Add CohortDate holiday variables----")
    } else if(proc %chin% c("training","train")) {
      data.table::set(TimerDataTrain, i = 4L, j = "Time", value = x[[3L]])
      data.table::set(TimerDataTrain, i = 4L, j = "Process", value = "# Add CohortDate holiday variables----")
    }

    # FE: AnomalyDetection for all CohortDates ----
    if(DebugMode) print("AnomalyDetection for all CohortDates ----")
    if(!is.null(AnomalyDetection)) {
      temp <- data[, list(ConversionCheck = sum(get(ConversionMeasure))), by = eval(CohortDate)]
      data.table::setnames(temp, eval(CohortDate), eval(CalendarDate))
      temp1 <- data[, list(Leads = max(get(BaseFunnelMeasure[1]))), by = eval(CalendarDate)]
      temp <- merge(temp, temp1, by = eval(CalendarDate), all = FALSE); rm(temp1)
      temp <- temp[, ConversionRate := ConversionCheck / (Leads + 1)][, .SD, .SDcols = c(eval(CalendarDate), "ConversionRate")]
      temp <- RemixAutoML::CreateCalendarVariables(data = temp, DateCols = eval(CalendarDate), AsFactor = FALSE, TimeUnits = "wday")
      temp <- RemixAutoML::GenTSAnomVars(data = temp, ValueCol = "ConversionRate", GroupVars = paste0(CalendarDate,"_wday"), DateVar = eval(CalendarDate), HighThreshold = AnomalyDetection$tstat_high, LowThreshold = AnomalyDetection$tstat_low, KeepAllCols = TRUE, IsDataScaled = FALSE)
      temp <- temp[, .SD, .SDcols = c(eval(CalendarDate), "AnomHigh","AnomLow")]
      if(!is.null(temp)) {
        data <- merge(data, temp, by.x = eval(CohortDate), by.y = eval(CalendarDate), all.x = TRUE)
        data[is.na(AnomHigh), AnomHigh := 0]
        data[is.na(AnomLow), AnomLow := 0]
      } else {
        ArgsList[["AnomalyDetection"]] <- NULL
      }
      rm(temp)
    }

    # DM: Type Casting CalendarDate to Character to be used as a Grouping Variable ----
    if(DebugMode) print("DM: Type Casting CalendarDate to Character to be used as a Grouping Variable ----")
    x <- system.time(gcFirst = FALSE, data.table::set(data, j = eval(CalendarDate), value = as.character(data[[eval(CalendarDate)]])))
    if(proc %chin% c("evaluate","eval","evaluation")) {
      data.table::set(TimerDataEval, i = 6L, j = "Time", value = x[[3L]])
      data.table::set(TimerDataEval, i = 6L, j = "Process", value = "# Convert CalendarDate to Character to treat as Cohort Group----")
    } else if(proc %chin% c("training","train")) {
      data.table::set(TimerDataTrain, i = 6L, j = "Time", value = x[[3L]])
      data.table::set(TimerDataTrain, i = 6L, j = "Process", value = "# Convert CalendarDate to Character to treat as Cohort Group----")
    }

    # FE: Target Transformation ----
    if(DebugMode) print('Feature Engineering: Add Target Transformation ----')
    if(TransformTargetVariable) {
      TransformResults <- AutoTransformationCreate(data, ColumnNames=c(BaseFunnelMeasure[1L], ConversionMeasure), Methods=TransformMethods, Path=NULL, TransID='Trans', SaveOutput=FALSE)
      data <- TransformResults$Data; TransformResults$Data <- NULL
      TransformObject <- TransformResults$FinalResults; rm(TransformResults)
      data[, eval(ConversionRateMeasure) := data.table::fifelse(get(BaseFunnelMeasure[1L]) == 0, 0, get(ConversionMeasure) / get(BaseFunnelMeasure[1L]))]
    } else {
      TransformObject <- NULL
    }

    # Save Timers to file
    SaveTimers(SaveModelObjectss = SaveModelObjects, procs = proc, TimerDataEvals = TimerDataEval, TimerDataTrains = TimerDataTrain, MetaDataPaths = MetaDataPath, ModelIDs = ModelID)

    # DM: Sort data by CalendarDate and then by CohortPeriodsVariable ----
    if(DebugMode) print("DM: Sort data by GroupVariables, CalendarDate and then by CohortPeriodsVariable ----")
    x <- system.time(gcFirst = FALSE, data.table::setorderv(data, cols = c(GroupVariables, CalendarDate, CohortPeriodsVariable), rep(1L, length(c(GroupVariables, CalendarDate, CohortPeriodsVariable)))))
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

    # FE: AutoLagRollStats() ConversionMeasure OVER CohortDate ----
    if(DebugMode) print("FE: AutoLagRollStats() ConversionMeasure with CalendarDate as a Grouping Variable ----")
    if(proc %in% c("evaluate","evaluation","eval","train","training")) {
      x <- system.time(gcFirst = FALSE, data <- RemixAutoML::AutoLagRollStats(

        # Data
        data                 = data,
        DateColumn           = CohortDate,
        Targets              = c(ConversionMeasure, ConversionRateMeasure),
        HierarchyGroups      = NULL,
        IndependentGroups    = paste0("Temp_", CalendarDate),
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
        Debug                = FALSE))
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

    # FE: AutoLagRollStats() HolidayCounts OVER CohortDate ----
    if(DebugMode) print("FE: AutoLagRollStats() CohortDate HolidayCounts with CalendarDate as a Grouping Variable ----")
    if(proc %in% c("evaluate","evaluation","eval","train","training") && !is.null(CohortHolidayLags)) {
      x <- system.time(gcFirst = FALSE, data <- RemixAutoML::AutoLagRollStats(

        # Data
        data                 = data,
        DateColumn           = CohortDate,
        Targets              = paste0(CohortDate, "HolidayCounts"),
        HierarchyGroups      = NULL,
        IndependentGroups    = paste0("Temp_", CalendarDate),
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
        Debug                = FALSE))
      if(proc %chin% c("evaluate","eval")) {
        data.table::set(TimerDataEval, i = 7L, j = "Time", value = x[[3L]])
        data.table::set(TimerDataEval, i = 7L, j = "Process", value = "# Rolling stats for CohortDate with CalendarDate as a Grouping Variable----")
      } else if(proc %chin% c("training","train")) {
        data.table::set(TimerDataTrain, i = 7L, j = "Time", value = x[[3L]])
        data.table::set(TimerDataTrain, i = 7L, j = "Process", value = "# Rolling stats for CohortDate with CalendarDate as a Grouping Variable----")
      }

      # DM: Type Casting CalendarDate back to Date ----
      x <- system.time(gcFirst = FALSE, data.table::set(data, j = eval(CalendarDate), value = as.Date(data[[eval(CalendarDate)]])))
      if(proc %chin% c("evaluate","eval")) {
        data.table::set(TimerDataEval, i = 8L, j = "Time", value = x[[3L]])
        data.table::set(TimerDataEval, i = 8L, j = "Process", value = "# Convert CalendarDate back to Date----")
      } else if(proc %chin% c("training","train")) {
        data.table::set(TimerDataTrain, i = 8L, j = "Time", value = x[[3L]])
        data.table::set(TimerDataTrain, i = 8L, j = "Process", value = "# Convert CalendarDate back to Date----")
      }

      # Save Timers to file ----
      SaveTimers(SaveModelObjectss = SaveModelObjects, procs = proc, TimerDataEvals = TimerDataEval, TimerDataTrains = TimerDataTrain, MetaDataPaths = MetaDataPath, ModelIDs = ModelID)
    }

    # FE: AutoLagRollStats() BaseFunnelMeasure OVER CalendarDate ----
    if(DebugMode) print("FE: AutoLagRollStats() BaseFunnelMeasure OVER CalendarDate ----")
    if(proc %chin% c("evaluate","evaluation","eval","training","train")) {
      for(bfm in seq_along(BaseFunnelMeasure)) {
        if("GroupVar" %chin% names(data)) {
          temp <- data[, lapply(.SD, data.table::first), .SDcols = c(BaseFunnelMeasure[bfm]), by = c("GroupVar", eval(CalendarDate))]
        } else {
          temp <- data[, lapply(.SD, data.table::first), .SDcols = c(BaseFunnelMeasure[bfm]), by = c(eval(CalendarDate))]
        }
        x <- system.time(gcFirst = FALSE, temp <- RemixAutoML::AutoLagRollStats(

          # Data
          data                 = temp,
          DateColumn           = CalendarDate,
          Targets              = BaseFunnelMeasure[bfm],
          HierarchyGroups      = NULL,
          IndependentGroups    = if(!"GroupVar" %chin% names(temp)) NULL else "GroupVar",
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
          Debug                = FALSE))
        if(proc %chin% c("evaluate","eval")) {
          data.table::set(TimerDataEval, i = 9L, j = "Time", value = x[[3L]])
          data.table::set(TimerDataEval, i = 9L, j = "Process", value = paste0("# Rolling stats for BaseFunnelMeasure ",bfm, "----"))
        } else if(proc %chin% c("training","train")) {
          data.table::set(TimerDataTrain, i = 9L, j = "Time", value = x[[3L]])
          data.table::set(TimerDataTrain, i = 9L, j = "Process", value = paste0("# Rolling stats for BaseFunnelMeasure ",bfm, "----"))
        }

        # Join back to data ----
        if("GroupVar" %chin% names(data)) {
          data.table::setkeyv(temp, c("GroupVar", CalendarDate))
          data.table::setkeyv(data, c("GroupVar", CalendarDate))
        } else {
          data.table::setkeyv(temp, c(CalendarDate))
          data.table::setkeyv(data, c(CalendarDate))
        }
        keep <- setdiff(names(temp), names(data))
        data[temp, paste0(keep) := mget(paste0("i.", keep))]
      }
    }

    # FE: ModelDataPrep() Impute Numeric Columns from AutoLagRollStats() ----
    if(DebugMode) print("FE: ModelDataPrep() Impute Numeric Columns from AutoLagRollStats() ----")
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

    # DM: Save data as csv ----
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

    # DM: Load data if updating models without new data ----
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

    # DM: Type Casting for CohortPeriodsVariable, CalendarDate, and CohortDate ----
    if(!all(class(data[[eval(CohortPeriodsVariable)]]) %chin% "numeric")) data[, eval(CohortPeriodsVariable) := as.numeric(as.character(get(CohortPeriodsVariable)))]
    if(!all(class(data[[eval(CalendarDate)]]) %chin% "Date")) data[, eval(CalendarDate) := as.Date(get(CalendarDate))]
    if(!all(class(data[[eval(CohortDate)]]) %chin% "Date")) data[, eval(CohortDate) := as.Date(get(CohortDate))]

    # Filter out historical values via truncation ---
    if(!is.null(TruncateDate)) data <- data[get(CalendarDate) >= eval(TruncateDate)]

    # DM: Partition Data ----
    if(DebugMode) print("DM: Partition Data ----")
    if(proc %chin% c("evaluate","eval","evaluation")) {
      x <- system.time(gcFirst = FALSE, DataSets <- RemixAutoML::AutoDataPartition(
        data = data,
        NumDataSets = 3L,
        Ratios = PartitionRatios,
        PartitionType = "random",
        StratifyColumnNames = if("GroupVar" %chin% names(data)) "GroupVar" else NULL,
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

    # ML: CatBoostRegression() ----
    if(DebugMode) print("ML: CatBoostRegression() ----")
    if(proc %chin% c("evaluate","eval","training","train")) {

      # Define features ----
      if(proc %chin% c("evaluate","eval")) {
        Features <- names(TrainData)[!names(TrainData) %chin% c(eval(CalendarDate),eval(CohortDate),eval(ConversionMeasure),eval(ConversionRateMeasure),paste0("Temp_", CalendarDate),paste0("Temp_", CohortPeriodsVariable))]
        idcols <- names(TrainData)[!names(TrainData) %in% c(Features, ConversionMeasure, ConversionRateMeasure)]
        if(ModelID %chin% names(TrainData)) Features <- Features[!Features %chin% ModelID]
      } else {
        Features <- names(data)[!names(data) %chin% c(eval(CalendarDate),eval(CohortDate),eval(ConversionMeasure),eval(ConversionRateMeasure),paste0("Temp_", CalendarDate),paste0("Temp_", CohortPeriodsVariable))]
        idcols <- names(data)[!names(data) %in% c(Features, ConversionMeasure, ConversionRateMeasure)]
        if(ModelID %chin% names(data)) Features <- Features[!Features %chin% ModelID]
      }

      # Define number of trees ----
      if(proc %chin% c("eval","evaluation","evaluate")) NTrees <- Trees

      # Build model ----
      x <- system.time(gcFirst = FALSE, TestModel <- RemixAutoML::AutoCatBoostRegression(

        # GPU or CPU and the number of available GPUs
        task_type = TaskType,
        NumGPUs = NumGPUs,

        # Metadata arguments
        ModelID = paste0(ModelID,"_", proc, "_"),
        model_path = ModelPath,
        metadata_path = MetaDataPath,
        SaveModelObjects = FALSE,
        ReturnModelObjects = TRUE,

        # Data arguments
        data = if(proc %chin% c("eval", "evaluate")) TrainData else data,
        TrainOnFull = if(proc %chin% c("eval", "evaluate")) FALSE else TRUE,
        ValidationData = if(proc %chin% c("eval", "evaluate")) ValidationData else NULL,
        TestData = if(proc %chin% c("eval", "evaluate")) TestData else NULL,
        TargetColumnName = ConversionRateMeasure,
        FeatureColNames = Features,
        PrimaryDateColumn = CohortDate,
        IDcols = idcols,
        TransformNumericColumns = NULL,
        Methods = TransformMethods,

        # Model evaluation
        eval_metric = EvaluationMetric,
        loss_function = LossFunction,
        MetricPeriods = MetricPeriods,
        NumOfParDepPlots = NumOfParDepPlots,

        # Grid tuning arguments
        PassInGrid = PassInGrid,
        GridTune = GridTune,
        MaxModelsInGrid = MaxModelsInGrid,
        MaxRunsWithoutNewWinner = MaxRunsWithoutNewWinner,
        MaxRunMinutes = MaxRunMinutes,
        BaselineComparison = BaselineComparison,

        # Tuning parameters
        Trees = NTrees,
        Depth = Depth,
        LearningRate = LearningRate,
        L2_Leaf_Reg = L2_Leaf_Reg,
        RSM = RSM,
        BootStrapType = BootStrapType,
        GrowPolicy = GrowPolicy,
        langevin = Langevin,
        diffusion_temperature = DiffusionTemperature,
        RandomStrength = RandomStrength,
        BorderCount = BorderCount,
        model_size_reg = ModelSizeReg,
        feature_border_type = FeatureBorderType,
        sampling_unit = SamplingUnit,
        subsample = SubSample,
        score_function = ScoreFunction,
        min_data_in_leaf = MinDataInLeaf))

      # Define number of trees ----
      if(proc %chin% c("eval","evaluate")) {
        TreeCount <- TestModel$Model$tree_count
        if(!is.null(Trees)) NTrees <- TreeCount else NTrees <- Trees
        ArgsList[["Trees"]] <- NTrees
      } else {
        NTrees <- Trees
      }

      # Store results ----
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

      # Add Transformation Objects to Output List ----
      TestModel[["TransformationResults"]] <- TransformObject

      # Save model objects ----
      if(SaveModelObjects) {
        if(proc %chin% c("evaluate","eval","evaluation")) {
          save(TestModel, file =  file.path(ModelPath, paste0(ModelID, "_Evaluation.Rdata")))
        } else if(proc %chin% c("training","train")) {
          save(TestModel, file = file.path(ModelPath, paste0(ModelID, "_FinalTrain.Rdata")))
        }
      }

      # Remove objects before next run ----
      if(proc %chin% c("evaluate","eval","evaluation")) {
        rm(TestModel)
      } else {
        for(zz in names(TestModel)) {
          if(!zz %chin% c("Model", "ColNames", "TransformationResults")) {
            TestModel[[zz]] <- NULL
          }
        }
      }

      # Garbage collection ----
      gc()
    }

    # Admin: Save timers to file ----
    SaveTimers(SaveModelObjectss = SaveModelObjects, procs = proc, TimerDataEvals = TimerDataEval, TimerDataTrains = TimerDataTrain, MetaDataPaths = MetaDataPath, ModelIDs = ModelID)

    #----

    #----

  }

  # Return and Save ----
  if(SaveModelObjects) save(ArgsList, file = file.path(ModelPath, paste0(ModelID, "_ArgsList.Rdata")))
  return(list(ModelOutput = TestModel, ArgsList = ArgsList))
}

#' @title AutoCatBoostFunnelCARMAScoring
#'
#' @description AutoCatBoostFunnelCARMAScoring for generating forecasts
#'
#' @author Adrian Antico
#' @family Automated Funnel Data Forecasting
#'
#' @param TrainData Data utilized in training. Do not put the BaseFunnelMeasure in this data set. Put it in the ForwardLookingData object
#' @param ForwardLookingData Base funnel measure data. Needs to cover the span of the forecast horizon
#' @param ModelPath Path to model location
#' @param TrainOutput Pass in the model object to speed up forecasting
#' @param TrainEndDate Max date from the training data
#' @param ForecastEndDate Max date to forecast out to
#' @param ArgsList Output list from AutoCatBoostFunnelCARMA
#' @param MaxCohortPeriod Max cohort periods to utilize when forecasting
#' @param DebugMode For debugging issues
#'
#' @examples
#' \dontrun{
#  # Create Fake Data
#' data <- RemixAutoML::FakeDataGenerator(ChainLadderData = TRUE)
#'
#' # Subset data for training
#' ModelDataBase <- data[CalendarDateColumn < '2020-01-01' & CohortDateColumn < '2020-01-01']
#' ModelData <- data.table::copy(ModelDataBase)
#'
#' # Train Funne Model
#' TestModel <- RemixAutoML::AutoCatBoostFunnelCARMA(
#'
#'   # Data Arguments
#'   data = ModelData,
#'   GroupVariables = NULL,
#'   BaseFunnelMeasure = "Leads", # if you have XREGS, supply vector such as c("Leads", "XREGS1", "XREGS2")
#'   ConversionMeasure = "Appointments",
#'   ConversionRateMeasure = NULL,
#'   CohortPeriodsVariable = "CohortDays",
#'   CalendarDate = "CalendarDateColumn",
#'   CohortDate = "CohortDateColumn",
#'   PartitionRatios = c(0.70,0.20,0.10),
#'   TruncateDate = NULL,
#'   TimeUnit = "days",
#'   TransformTargetVariable = TRUE,
#'   TransformMethods = c("Asinh","Asin","Log","LogPlus1","Sqrt","Logit"),
#'   AnomalyDetection = list(tstat_high = 3, tstat_low = -2),
#'
#'   # MetaData Arguments
#'   Jobs = c("eval","train"),
#'   SaveModelObjects = FALSE,
#'   ModelID = "ModelTest",
#'   ModelPath = getwd(),
#'   MetaDataPath = NULL,
#'   DebugMode = TRUE,
#'   NumOfParDepPlots = 1L,
#'
#'   # Feature Engineering Arguments
#'   CalendarTimeGroups = c("days","weeks","months"),
#'   CohortTimeGroups = c("days", "weeks"),
#'   CalendarVariables = c("wday","mday","yday","week","month","quarter","year"),
#'   HolidayGroups = c("USPublicHolidays","EasterGroup","ChristmasGroup","OtherEcclesticalFeasts"),
#'   HolidayLookback = NULL,
#'   CohortHolidayLags = c(1L,2L,7L),
#'   CohortHolidayMovingAverages = c(3L,7L),
#'   CalendarHolidayLags = c(1L,2L,7L),
#'   CalendarHolidayMovingAverages = c(3L,7L),
#'
#'   # Time Series Features
#'   ImputeRollStats = -0.001,
#'   CalendarLags = list("day" = c(1L,2L,7L,35L,42L), "week" = c(5L,6L,10L,12L,25L,26L)),
#'   CalendarMovingAverages = list("day" = c(7L,14L,35L,42L), "week" = c(5L,6L,10L,12L,20L,24L), "month" = c(6L,12L)),
#'   CalendarStandardDeviations = NULL,
#'   CalendarSkews = NULL,
#'   CalendarKurts = NULL,
#'   CalendarQuantiles = NULL,
#'   CalendarQuantilesSelected = "q50",
#'   CohortLags = list("day" = c(1L,2L,7L,35L,42L), "week" = c(5L,6L)),
#'   CohortMovingAverages = list("day" = c(7L,14L,35L,42L), "week" = c(5L,6L), "month" = c(1L,2L)),
#'   CohortStandardDeviations = NULL,
#'   CohortSkews = NULL,
#'   CohortKurts = NULL,
#'   CohortQuantiles = NULL,
#'   CohortQuantilesSelected = "q50",
#'
#'   # ML Grid Tuning
#'   PassInGrid = NULL,
#'   GridTune = FALSE,
#'   BaselineComparison = "default",
#'   MaxModelsInGrid = 25L,
#'   MaxRunMinutes = 180L,
#'   MaxRunsWithoutNewWinner = 10L,
#'
#'   # ML Setup Parameters
#'   MetricPeriods = 10,
#'   LossFunction = 'MAE',
#'   EvaluationMetric = 'MAE',
#'   TaskType = "CPU",
#'   NumGPUs = 1,
#'
#'   # ML Parameters
#'   Trees = 3000L,
#'   Depth = 8L,
#'   L2_Leaf_Reg = NULL,
#'   LearningRate = NULL,
#'   Langevin = FALSE,
#'   DiffusionTemperature = 10000,
#'   RandomStrength = 1,
#'   BorderCount = 254,
#'   RSM = NULL,
#'   GrowPolicy = "SymmetricTree",
#'   BootStrapType = "Bayesian",
#'   ModelSizeReg = 0.5,
#'   FeatureBorderType = "GreedyLogSum",
#'   SamplingUnit = "Group",
#'   SubSample = NULL,
#'   ScoreFunction = "Cosine",
#'   MinDataInLeaf = 1)
#'
#' # Separate out the Base Funnel Measures Data
#' LeadsData <- data[, lapply(.SD, data.table::first), .SDcols = c("Leads"), by = c("CalendarDateColumn")]
#' ModelData <- ModelDataBase[, Leads := NULL]
#'
#' # Forecast Funnel Model
#' Test <- RemixAutoML::AutoCatBoostFunnelCARMAScoring(
#'   TrainData = ModelData,
#'   ForwardLookingData = LeadsData,
#'   TrainEndDate = ModelData[, max(CalendarDateColumn)],
#'   ForecastEndDate = LeadsData[, max(CalendarDateColumn)],
#'   TrainOutput = TestModel$ModelOutput,
#'   ArgsList = TestModel$ArgsList,
#'   ModelPath = NULL,
#'   MaxCohortPeriod = 15,
#'   DebugMode = TRUE)
#' }
#'
#' @export
AutoCatBoostFunnelCARMAScoring <- function(TrainData,
                                           ForwardLookingData = NULL,
                                           TrainEndDate = NULL,
                                           ForecastEndDate = NULL,
                                           ArgsList = NULL,
                                           TrainOutput = NULL,
                                           ModelPath = NULL,
                                           MaxCohortPeriod = NULL,
                                           DebugMode = FALSE) {

  # Admin: Clean up TrainOutput ----
  if(!is.null(TrainOutput)) {
    for(zz in seq_along(TrainOutput)) if(!names(TrainOutput)[zz] %chin% c("Model", "ColNames", "TransformationResults")) TrainOutput[[zz]] <- NULL
  }

  # DE: ModelDataPrep ----
  TrainData <- ModelDataPrep(data=TrainData, Impute=FALSE, CharToFactor=FALSE, FactorToChar=FALSE, IntToNumeric=FALSE, LogicalToBinary=FALSE, DateToChar=FALSE, IDateConversion=TRUE, RemoveDates=FALSE, MissFactor="0", MissNum=-1, IgnoreCols=NULL)
  ForwardLookingData <- ModelDataPrep(data = ForwardLookingData, Impute=FALSE, CharToFactor=FALSE, FactorToChar=FALSE, IntToNumeric=FALSE, LogicalToBinary=FALSE, DateToChar=FALSE, IDateConversion=TRUE, RemoveDates=FALSE, MissFactor="0", MissNum=-1, IgnoreCols=NULL)

  # Admin: Forecasting start and end periods ----
  if(DebugMode) print("Forecasting start and end periods ----")
  if(is.null(TrainEndDate)) TrainEndDate <- TrainData[, max(get(ArgsList$CalendarDate), na.rm = TRUE)]
  if(is.null(ForecastEndDate)) ForecastEndDate <- ForwardLookingData[, max(get(ArgsList$CalendarDate), na.rm = TRUE)]

  # DE: Add GroupVar ----
  if(DebugMode) print("DE: Add GroupVar ----")
  if(!is.null(ArgsList[["GroupVar"]])) {
    TrainData[, paste0("Temp_", ArgsList$CohortPeriodsVariable) := do.call(paste, c(.SD, sep = ' ')), .SDcols = c(ArgsList$GroupVariables, ArgsList$CohortPeriodsVariable)]
    TrainData[, paste0("Temp_", ArgsList$CalendarDate) := do.call(paste, c(.SD, sep = ' ')), .SDcols = c(ArgsList$GroupVariables, ArgsList$CalendarDate)]
    TrainData[, GroupVar := do.call(paste, c(.SD, sep = ' ')), .SDcols = c(ArgsList$GroupVariables)]
  } else {
    TrainData[, paste0("Temp_", ArgsList$CohortPeriodsVariable) := do.call(paste, c(.SD, sep = ' ')), .SDcols = c(ArgsList$CohortPeriodsVariable)]
    TrainData[, paste0("Temp_", ArgsList$CalendarDate) := do.call(paste, c(.SD, sep = ' ')), .SDcols = c(ArgsList$CalendarDate)]
  }

  # FE: Target Transformation ----
  if(DebugMode) print('Feature Engineering: Add Target Transformation ----')
  if(ArgsList$TransformTargetVariable) {
    TrainData <- AutoTransformationScore(ScoringData = TrainData, FinalResults = TrainOutput$TransformationResults[ColumnName == eval(ArgsList$ConversionMeasure)], Type = "Apply")
    LeadsData <- AutoTransformationScore(ScoringData = LeadsData, FinalResults = TrainOutput$TransformationResults[ColumnName == eval(ArgsList$BaseFunnelMeasure[1L])], Type = "Apply")
  }

  # Admin: Initialize FC_Periods ----
  FC_Period <- 0L

  # ML: Loop through all periods to forecast ----
  while(TrainEndDate < ForecastEndDate) {

    # Admin: Iteration Start ----
    IterationStart <- Sys.time()

    # DE: Increment FC_Period ----
    if(DebugMode) print("Increment FC_Period ----")
    FC_Period <- FC_Period + 1L
    for(bla in seq_len(5L)) print(paste0("Working on Forecast for period: ", FC_Period, " ::: Periods left to forecast: ", difftime(ForecastEndDate, TrainEndDate)))

    # DE: Convert to date ----
    if(DebugMode) print("Convert to date ----")
    if(!all(class(TrainData[[eval(ArgsList$CalendarDate)]]) %chin% "Date")) TrainData[, eval(ArgsList$CalendarDate) := as.Date(get(ArgsList$CalendarDate))]
    if(!all(class(TrainData[[eval(ArgsList$CohortDate)]]) != "Date")) TrainData[, eval(ArgsList$CohortDate) := as.Date(get(ArgsList$CohortDate))]

    # DE: Add indicator variable for AutoLagRollStatsScoring() so it knows what to score and what not to. A value of 1 indicates that the record should be scored. All others are not scored----
    if(DebugMode) print("Add indicator variable for AutoLagRollStatsScoring() so it knows what to score and what not to. A value of 1 indicates that the record should be scored. All others are not scored----")
    TrainData[, ScoreRecords := 2]

    # DE: Type conversion ----
    if(DebugMode) print("Type conversion ----")
    if(class(TrainData[[eval(ArgsList$CohortPeriodsVariable)]]) == "factor") TrainData[, eval(ArgsList$CohortPeriodsVariable) := as.numeric(as.character(get(ArgsList$CohortPeriodsVariable)))]

    # DE: Create single future value for all cohorts ----
    if(DebugMode) print("Create single future value for all cohorts ----")
    maxct <- TrainData[, list(max(get(ArgsList$CohortPeriodsVariable)), data.table::first(ScoreRecords)), by = c(ArgsList$GroupVariables, ArgsList$CalendarDate)]
    data.table::setnames(maxct, c("V1","V2"), c(ArgsList$CohortPeriodsVariable, "ScoreRecords"))
    maxct[, eval(ArgsList$CohortPeriodsVariable) := get(ArgsList$CohortPeriodsVariable) + 1L]
    maxct[, eval(ArgsList$CohortDate) := as.Date(get(ArgsList$CalendarDate)) + lubridate::days(get(ArgsList$CohortPeriodsVariable))]
    data.table::setkeyv(maxct, cols = c(ArgsList$GroupVariables, ArgsList$CalendarDate))
    if(!is.null(ArgsList[["GroupVar"]])) {
      maxct[, paste0("Temp_", ArgsList$CohortPeriodsVariable) := do.call(paste, c(.SD, sep = ' ')), .SDcols = c(ArgsList$GroupVariables, ArgsList$CohortPeriodsVariable)]
      maxct[, paste0("Temp_", ArgsList$CalendarDate) := do.call(paste, c(.SD, sep = ' ')), .SDcols = c(ArgsList$GroupVariables, ArgsList$CalendarDate)]
      maxct[, GroupVar := do.call(paste, c(.SD, sep = ' ')), .SDcols = c(ArgsList$GroupVariables)]
    } else {
      maxct[, paste0("Temp_", ArgsList$CohortPeriodsVariable) := do.call(paste, c(.SD, sep = ' ')), .SDcols = c(ArgsList$CohortPeriodsVariable)]
      maxct[, paste0("Temp_", ArgsList$CalendarDate) := do.call(paste, c(.SD, sep = ' ')), .SDcols = c(ArgsList$CalendarDate)]
    }

    # DE: Subset TrainData and update TrainData ----
    if(DebugMode) print("DE: Subset TrainData and update TrainData ----")
    ForwardLookingData <- ForwardLookingData[get(ArgsList$CalendarDate) > max(maxct[[eval(ArgsList$CalendarDate)]])]
    if(!is.null(ArgsList$GroupVariables)) {
      NextFCPeriod <- ForwardLookingData[get(ArgsList$CalendarDate) == min(get(ArgsList$CalendarDate))]
    } else {
      NextFCPeriod <- ForwardLookingData[1L]
    }
    NextFCPeriod[, eval(ArgsList$CalendarDate) := as.Date(get(ArgsList$CalendarDate))]
    NextFCPeriod[, eval(ArgsList$CohortDate) := as.Date(get(ArgsList$CalendarDate))]
    NextFCPeriod[, ScoreRecords := 1]
    NextFCPeriod[, eval(ArgsList$CohortPeriodsVariable) := 0]
    NextFCPeriod[, eval(ArgsList$ConversionRateMeasure) := 0.0]
    if(!is.null(ArgsList[["GroupVar"]])) {
      NextFCPeriod[, paste0("Temp_", ArgsList$CohortPeriodsVariable) := do.call(paste, c(.SD, sep = ' ')), .SDcols = c(ArgsList$GroupVariables, ArgsList$CohortPeriodsVariable)]
      NextFCPeriod[, paste0("Temp_", ArgsList$CalendarDate) := do.call(paste, c(.SD, sep = ' ')), .SDcols = c(ArgsList$GroupVariables, ArgsList$CalendarDate)]
      NextFCPeriod[, GroupVar := do.call(paste, c(.SD, sep = ' ')), .SDcols = c(ArgsList$GroupVariables)]
    } else {
      NextFCPeriod[, paste0("Temp_", ArgsList$CohortPeriodsVariable) := do.call(paste, c(.SD, sep = ' ')), .SDcols = c(ArgsList$CohortPeriodsVariable)]
      NextFCPeriod[, paste0("Temp_", ArgsList$CalendarDate) := do.call(paste, c(.SD, sep = ' ')), .SDcols = c(ArgsList$CalendarDate)]
    }
    if(!is.null(ArgsList$GroupVariables)) {
      temp <- data.table::copy(NextFCPeriod)
      temp <- temp[, .SD, .SDcols = c(ArgsList$GroupVariables, ArgsList$CalendarDate, ArgsList$BaseFunnelMeasure)]
      ForwardLookingData <- data.table::fsetdiff(x = ForwardLookingData, y = temp)
    } else {
      ForwardLookingData <- ForwardLookingData[2L:.N]
    }

    # DE: Merge BaseFunnelMeasures to TrainData if they aren't in there ----
    if(DebugMode) print("DE: Merge BaseFunnelMeasures to TrainData if they aren't in there ----")
    if(!all(ArgsList$BaseFunnelMeasure %chin% names(TrainData))) {
      data.table::setkeyv(x = TrainData, c(ArgsList$GroupVariables, ArgsList$CalendarDate))
      data.table::setkeyv(x = LeadsData, c(ArgsList$GroupVariables, ArgsList$CalendarDate))
      TrainData[LeadsData, eval(ArgsList$BaseFunnelMeasure) := mget(paste0("i.", ArgsList$BaseFunnelMeasure))]
    }

    # DE: Aggregate Data ----
    if(DebugMode) print("DE: Aggregate Data ----")
    temp <- TrainData[, lapply(.SD, data.table::first), .SDcols = c(ArgsList$BaseFunnelMeasure), keyby = c(ArgsList$GroupVariables, ArgsList$CalendarDate)]

    # DE: Merge TrainData ----
    if(DebugMode) print("Merge TrainData ----")
    if(!any(class(temp[[ArgsList$CalendarDate]]) %chin% "Date")) temp[, eval(ArgsList$CalendarDate) := as.Date(get(ArgsList$CalendarDate))]
    maxct[temp, eval(ArgsList$BaseFunnelMeasure) := mget(paste0("i.", ArgsList$BaseFunnelMeasure))]
    maxct[, eval(ArgsList$ConversionMeasure) := 0]
    maxct[, eval(ArgsList$ConversionRateMeasure) := 0]
    maxct[, ScoreRecords := 1]
    if(!is.null(ArgsList[["GroupVar"]])) {
      maxct[, paste0("Temp_", ArgsList$CohortPeriodsVariable) := do.call(paste, c(.SD, sep = ' ')), .SDcols = c(ArgsList$GroupVariables, ArgsList$CohortPeriodsVariable)]
      maxct[, paste0("Temp_", ArgsList$CalendarDate) := do.call(paste, c(.SD, sep = ' ')), .SDcols = c(ArgsList$GroupVariables, ArgsList$CalendarDate)]
      maxct[, GroupVar := do.call(paste, c(.SD, sep = ' ')), .SDcols = c(ArgsList$GroupVariables)]
    } else {
      maxct[, paste0("Temp_", ArgsList$CohortPeriodsVariable) := do.call(paste, c(.SD, sep = ' ')), .SDcols = c(ArgsList$CohortPeriodsVariable)]
      maxct[, paste0("Temp_", ArgsList$CalendarDate) := do.call(paste, c(.SD, sep = ' ')), .SDcols = c(ArgsList$CalendarDate)]
    }
    maxct <- data.table::rbindlist(list(maxct, NextFCPeriod), use.names = TRUE, fill = TRUE)
    for(xxxx in seq_len(ncol(maxct))) data.table::set(maxct, i = which(is.na(maxct[[xxxx]])), j = xxxx, value = 0)

    # DE: Remove CohortPeriods beyond MaxCohortPeriod ----
    if(DebugMode) print("DE: Remove CohortPeriods beyond MaxCohortPeriod ----")
    maxct <- maxct[get(ArgsList$CohortPeriodsVariable) <= MaxCohortPeriod]
    ScoreDate <- maxct[, max(get(ArgsList$CalendarDate))]

    # DE: Stack onto modeling TrainData for ArgsList$ModelID ----
    if(DebugMode) print("DE: Stack onto modeling TrainData for ArgsList$ModelID ----")
    TrainData <- data.table::rbindlist(list(TrainData, maxct), fill = TRUE, use.names = TRUE)
    TrainData[, eval(ArgsList$ConversionRateMeasure) := data.table::fifelse(get(ArgsList$BaseFunnelMeasure[1L]) == 0, 0, get(ArgsList$ConversionMeasure) / get(ArgsList$BaseFunnelMeasure[1L]))]
    rm(maxct)

    # FE: Calendar & Holiday Variables ----
    if(DebugMode) print("FE: Calendar & Holiday Variables ----")
    TrainData <- RemixAutoML::CreateCalendarVariables(TrainData, DateCols = c(eval(ArgsList$CalendarDate)), AsFactor = FALSE, TimeUnits = ArgsList$CalendarVariables)
    TrainData <- RemixAutoML::CreateCalendarVariables(TrainData, DateCols = c(eval(ArgsList$CohortDate)), AsFactor = FALSE, TimeUnits = ArgsList$CalendarVariables)
    TrainData <- RemixAutoML::CreateHolidayVariables(TrainData, DateCols = c(ArgsList$CalendarDate), LookbackDays = if(!is.null(ArgsList$HolidayLookback)) ArgsList$HolidayLookback else LB(ArgsList$TimeUnit), HolidayGroups = ArgsList$HolidayGroups, Holidays = NULL, Print = FALSE)
    data.table::setnames(TrainData, old = "HolidayCounts", new = paste0(ArgsList$CalendarDate,"HolidayCounts"))
    TrainData <- RemixAutoML::CreateHolidayVariables(TrainData, DateCols = c(ArgsList$CohortDate), LookbackDays = if(!is.null(ArgsList$HolidayLookback)) ArgsList$HolidayLookback else LB(ArgsList$TimeUnit), HolidayGroups = ArgsList$HolidayGroups, Holidays = NULL, Print = FALSE)
    data.table::setnames(TrainData, old = "HolidayCounts", new = paste0(ArgsList$CohortDate,"HolidayCounts"))
    data.table::setorderv(TrainData, cols = c(ArgsList$CalendarDate,eval(ArgsList$CohortPeriodsVariable)), c(1L, 1L))

    # FE: Add Anomaly detection zeros ----
    if(DebugMode) print("Add Anomaly detection zeros ----")
    if(!is.null(ArgsList[["AnomalyDetection"]])) TrainData[, ":=" (AnomHigh = 0, AnomLow = 0)]

    # FE: ConversionMeasure OVER CohortDate ----
    if(DebugMode) print("FE: ConversionMeasure OVER CohortDate ----")
    temp <- data.table::copy(TrainData)
    data.table::set(temp, j = ArgsList$CalendarDate, value = as.character(temp[[ArgsList$CalendarDate]]))
    temp <- RemixAutoML::AutoLagRollStatsScoring(

      # Data
      data                 = temp,
      DateColumn           = ArgsList$CohortDate,
      Targets              = c(ArgsList$ConversionMeasure, ArgsList$ConversionRateMeasure),
      RowNumsID            = "ScoreRecords",
      RowNumsKeep          = 1,
      HierarchyGroups      = NULL,
      IndependentGroups    = paste0("Temp_", ArgsList$CalendarDate),
      TimeUnit             = ArgsList$TimeUnit,
      TimeGroups           = ArgsList$CohortTimeGroups,
      TimeUnitAgg          = ArgsList$TimeUnit,

      # Services
      TimeBetween          = NULL,
      RollOnLag1           = TRUE,
      Type                 = "Lag",
      SimpleImpute         = TRUE,

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
    temp[, eval(ArgsList$CalendarDate) := as.Date(get(ArgsList$CalendarDate))]
    data.table::setkeyv(temp, c(ArgsList$GroupVariables, ArgsList$CalendarDate))
    data.table::setkeyv(TrainData, c(ArgsList$GroupVariables, ArgsList$CalendarDate))
    TrainData[temp, paste0(setdiff(names(temp), names(TrainData))) := mget(paste0("i.", setdiff(names(temp), names(TrainData))))]
    rm(temp)

    # FE: CohortDateHolidayCounts OVER CohortDate ----
    if(DebugMode) print("FE: CohortDateHolidayCounts OVER CohortDate ----")
    temp <- data.table::copy(TrainData)
    data.table::set(temp, j = eval(ArgsList$CalendarDate), value = as.character(temp[[eval(ArgsList$CalendarDate)]]))
    temp <- RemixAutoML::AutoLagRollStatsScoring(

      # Data
      data                 = temp,
      DateColumn           = ArgsList$CohortDate,
      Targets              = paste0(ArgsList$CohortDate,"HolidayCounts"),
      RowNumsID            = "ScoreRecords",
      RowNumsKeep          = 1,
      HierarchyGroups      = NULL,
      IndependentGroups    = paste0("Temp_", ArgsList$CalendarDate),
      TimeUnit             = ArgsList$TimeUnit,
      TimeGroups           = ArgsList$TimeUnit,
      TimeUnitAgg          = ArgsList$TimeUnit,

      # Services
      TimeBetween          = NULL,
      RollOnLag1           = TRUE,
      Type                 = "Lag",
      SimpleImpute         = TRUE,

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
    temp[, eval(ArgsList$CalendarDate) := as.Date(get(ArgsList$CalendarDate))]
    data.table::setkeyv(temp, c(ArgsList$GroupVariables, ArgsList$CalendarDate))
    TrainData[temp, paste0(setdiff(names(temp), names(TrainData))) := mget(paste0("i.", setdiff(names(temp), names(TrainData))))]
    rm(temp)

    # FE: BaseFunnelMeasure OVER CalendarDate ----
    if(DebugMode) print("FE: BaseFunnelMeasure OVER CalendarDate ----")
    for(bfm in seq_along(ArgsList$BaseFunnelMeasure)) {
      if("GroupVar" %chin% names(TrainData)) {
        temp <- TrainData[, lapply(.SD, data.table::first), .SDcols = c(ArgsList$BaseFunnelMeasure[bfm]), by = c("GroupVar", eval(ArgsList$CalendarDate))]
      } else {
        temp <- TrainData[, lapply(.SD, data.table::first), .SDcols = c(ArgsList$BaseFunnelMeasure[bfm]), by = c(eval(ArgsList$CalendarDate))]
      }
      temp[, ScoreRecords := data.table::fifelse(get(ArgsList$CalendarDate) == eval(ScoreDate), 1, 2)]
      if(!any(class(temp[[ArgsList$CalendarDate]]) %chin% "Date")) data.table::set(temp, j = eval(ArgsList$CalendarDate), value = as.Date(temp[[eval(ArgsList$CalendarDate)]]))
      temp <- RemixAutoML::AutoLagRollStatsScoring(

        # Data
        data                 = temp,
        DateColumn           = ArgsList$CalendarDate,
        Targets              = ArgsList$BaseFunnelMeasure[bfm],
        HierarchyGroups      = NULL,
        IndependentGroups    = if(!"GroupVar" %chin% names(temp)) NULL else "GroupVar",
        TimeGroups           = ArgsList[["CalendarTimeGroups"]],
        TimeUnit             = ArgsList[["TimeUnit"]],
        TimeUnitAgg          = ArgsList[["TimeUnit"]],

        # Services
        RowNumsID            = "ScoreRecords",
        RowNumsKeep          = 1,
        TimeBetween          = NULL,
        RollOnLag1           = TRUE,
        Type                 = "Lag",
        SimpleImpute         = TRUE,

        # Calculated Columns
        Lags                 = ArgsList[["CalendarLags"]],
        MA_RollWindows       = ArgsList[["CalendarMovingAverages"]],
        SD_RollWindows       = ArgsList[["CalendarStandardDeviations"]],
        Skew_RollWindows     = ArgsList[["CalendarSkews"]],
        Kurt_RollWindows     = ArgsList[["CalendarKurts"]],
        Quantile_RollWindows = ArgsList[["CalendarQuantiles"]],
        Quantiles_Selected   = ArgsList[["CalendarQuantilesSelected"]],
        Debug                = TRUE)

      # Join datasets
      temp[, eval(ArgsList$CalendarDate) := as.Date(get(ArgsList$CalendarDate))]
      if("GroupVar" %chin% names(temp)) {
        data.table::setkeyv(temp, c("GroupVar", ArgsList$CalendarDate))
        data.table::setkeyv(TrainData, c("GroupVar", ArgsList$CalendarDate))
      } else {
        data.table::setkeyv(temp, ArgsList$CalendarDate)
        data.table::setkeyv(TrainData, ArgsList$CalendarDate)
      }
      TrainData[temp, paste0(setdiff(names(temp), names(TrainData))) := mget(paste0("i.", setdiff(names(temp), names(TrainData))))]
      rm(temp)
    }

    # DE: Model data prep ----
    if(DebugMode) print("DE: Model data prep ----")
    TrainData <- RemixAutoML::ModelDataPrep(
      data         = TrainData,
      Impute       = TRUE,
      CharToFactor = FALSE,
      FactorToChar = FALSE,
      IntToNumeric = TRUE,
      DateToChar   = FALSE,
      RemoveDates  = FALSE,
      MissFactor   = "0",
      MissNum      = ArgsList$ImputeRollStats,
      IgnoreCols   = NULL)

    # DE: Type Change: CorhortDaysOut as numeric and the dates as Dates ----
    if(DebugMode) print("DE: Type Change: CorhortDaysOut as numeric and the dates as Dates ----")
    if(!all(class(TrainData[[ArgsList$CohortPeriodsVariable]]) %chin% "numeric")) TrainData[, eval(ArgsList$CohortPeriodsVariable) := as.numeric(as.character(get(ArgsList$CohortPeriodsVariable)))]
    if(!all(class(TrainData[[ArgsList$CalendarDate]]) %chin% "Date")) TrainData[, eval(ArgsList$CalendarDate) := as.Date(get(ArgsList$CalendarDate))]
    if(!all(class(TrainData[[ArgsList$CohortDate]]) %chin% "Date")) TrainData[, eval(ArgsList$CohortDate) := as.Date(get(ArgsList$CohortDate))]

    # DE: Load model artifacts ----
    if(is.null(TrainOutput) && FC_Period == 1L) load(file = file.path(normalizePath(eval(ArgsList$ModelPath)), paste0(ArgsList$ModelID, "_FinalTrain.Rdata")))

    # ML: Score Model ----
    if(DebugMode) print("ML: Score Model ----")
    temp1 <- data.table::copy(TrainData)
    temp <- temp1[ScoreRecords == 1]
    Features <- TrainOutput$ColNames[[1L]]
    temp <- RemixAutoML::AutoCatBoostScoring(
      TargetType = "regression",
      ScoringData = temp,
      FeatureColumnNames = Features,
      IDcols = names(temp)[!names(temp) %chin% Features],
      ModelObject = TrainOutput$Model,
      ModelPath = if(is.null(TrainOutput)) ArgsList$ModelPath else NULL,
      ModelID = ArgsList$ModelID,
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

    # DE: Update forecast TrainData ----
    if(DebugMode) print("DE: Update forecast TrainData ----")
    temp1[ScoreRecords == 1, eval(ArgsList$ConversionRateMeasure) := temp[which(Predictions < 0), Predictions := 0][[1L]]]
    temp1[ScoreRecords == 1, eval(ArgsList$ConversionMeasure) := get(ArgsList$ConversionRateMeasure) * get(ArgsList$BaseFunnelMeasure)]
    if(!is.null(ArgsList$GroupVariables)) {
      temp1 <- temp1[ScoreRecords == 1, .SD, .SDcols = c("GroupVar", eval(ArgsList$GroupVariables), eval(ArgsList$CalendarDate),eval(ArgsList$CohortDate),eval(ArgsList$CohortPeriodsVariable),eval(ArgsList$BaseFunnelMeasure),eval(ArgsList$ConversionMeasure),eval(ArgsList$ConversionRateMeasure))]
      TrainData <- data.table::rbindlist(list(TrainData[ScoreRecords != 1, .SD, .SDcols = c("GroupVar", eval(ArgsList$GroupVariables), eval(ArgsList$CalendarDate),eval(ArgsList$CohortDate),eval(ArgsList$CohortPeriodsVariable),eval(ArgsList$BaseFunnelMeasure),eval(ArgsList$ConversionMeasure),eval(ArgsList$ConversionRateMeasure))], temp1), fill = TRUE, use.names = TRUE)
    } else {
      temp1 <- temp1[ScoreRecords == 1, .SD, .SDcols = c(eval(ArgsList$CalendarDate),eval(ArgsList$CohortDate),eval(ArgsList$CohortPeriodsVariable),eval(ArgsList$BaseFunnelMeasure),eval(ArgsList$ConversionMeasure),eval(ArgsList$ConversionRateMeasure))]
      TrainData <- data.table::rbindlist(list(TrainData[ScoreRecords != 1, .SD, .SDcols = c(eval(ArgsList$CalendarDate),eval(ArgsList$CohortDate),eval(ArgsList$CohortPeriodsVariable),eval(ArgsList$BaseFunnelMeasure),eval(ArgsList$ConversionMeasure),eval(ArgsList$ConversionRateMeasure))], temp1), fill = TRUE, use.names = TRUE)
    }

    # DE: Save forecasts to file ----
    if(!is.null(ModelPath)) data.table::fwrite(TrainData, file = file.path(ArgsList$ModelPath, paste0(ArgsList$ModelID, "_Forecasts.csv")))

    # DE: Update TrainEndDate to know when to stop ----
    TrainEndDate <- TrainData[, max(get(ArgsList$CalendarDate))]

    # Admin: Iteration Timing ----
    IterationEnd <- Sys.time()
    for(ggg in 1:5) print(difftime(time1 = IterationEnd, time2 = IterationStart, units = "secs"))
  }

  # DE: BackTransform ----
  if(ArgsList$TransformTargetVariable) {
    TrainData <- AutoTransformationScore(ScoringData=TrainData, FinalResults=TestModel$ModelOutput$TransformationResults, Type='Inverse', TransID=NULL, Path=NULL)
    TrainData[, eval(ArgsList$ConversionRateMeasure) := data.table::fifelse(get(ArgsList$BaseFunnelMeasure[1L]) == 0, 0, get(ArgsList$ConversionMeasure) / get(ArgsList$BaseFunnelMeasure[1L]))]
  }

  # Return Forecast Data With Features ----
  return(TrainData)
}
