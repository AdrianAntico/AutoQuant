#' @title AutoLightGBMFunnelCARMA
#'
#' @description AutoLightGBMFunnelCARMA is a forecasting model for cohort funnel forecasting for grouped data or non-grouped data
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
#' @param WeightsColumnName = NULL
#' @param TimeUnit Base time unit of data. "days", "weeks", "months", "quarters", "years"
#' @param OutputSelection = c('Importances', 'EvalPlots', 'EvalMetrics', 'Score_TrainData')
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
#' @param ImputeRollStats Constant value to fill NA after running AutoLagRollStats()
#' @param CalendarVariables "wday", "mday", "yday", "week", "isoweek", "month", "quarter", "year"
#' @param HolidayGroups c("USPublicHolidays","EasterGroup","ChristmasGroup","OtherEcclesticalFeasts")
#' @param HolidayLookback Number of days in range to compute number of holidays from a given date in the data. If NULL, the number of days are computed for you.
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
#'
#' # Grid tuning
#' @param PassInGrid Defaults to NULL. Pass in a single row of grid from a previous output as a data.table (they are collected as data.tables)
#' @param GridTune Set to TRUE to run a grid tuning procedure. Set a number in MaxModelsInGrid to tell the procedure how many models you want to test.
#' @param BaselineComparison Set to either "default" or "best". Default is to compare each successive model build to the baseline model using max trees (from function args). Best makes the comparison to the current best model.
#' @param MaxModelsInGrid Number of models to test from grid options
#' @param MaxRunMinutes Maximum number of minutes to let this run
#' @param MaxRunsWithoutNewWinner Number of models built before calling it quits
#'
#' # ML Args begin
#' @param NThreads = parallel::detectCores() / 2
#' @param Device_Type = 'CPU'
#' @param LossFunction = 'regression'
#' @param EvalMetric = 'mae'
#' @param Input_Model = NULL
#' @param Task = 'train'
#' @param Boosting = 'gbdt'
#' @param LinearTree = FALSE
#' @param Trees = 1000
#' @param ETA = 0.10
#' @param Num_Leaves = 31
#' @param Deterministic = TRUE
#'
#' # Learning Parameters
#' # https://lightgbm.readthedocs.io/en/latest/Parameters.html#learning-control-parameters
#' @param Force_Col_Wise = FALSE
#' @param Force_Row_Wise = FALSE
#' @param Max_Depth = 6
#' @param Min_Data_In_Leaf = 20
#' @param Min_Sum_Hessian_In_Leaf = 0.001
#' @param Bagging_Freq = 1.0
#' @param Bagging_Fraction = 1.0
#' @param Feature_Fraction = 1.0
#' @param Feature_Fraction_Bynode = 1.0
#' @param Lambda_L1 = 0.0
#' @param Lambda_L2 = 0.0
#' @param Extra_Trees = FALSE
#' @param Early_Stopping_Round = 10
#' @param First_Metric_Only = TRUE
#' @param Max_Delta_Step = 0.0
#' @param Linear_Lambda = 0.0
#' @param Min_Gain_To_Split = 0
#' @param Drop_Rate_Dart = 0.10
#' @param Max_Drop_Dart = 50
#' @param Skip_Drop_Dart = 0.50
#' @param Uniform_Drop_Dart = FALSE
#' @param Top_Rate_Goss = FALSE
#' @param Other_Rate_Goss = FALSE
#' @param Monotone_Constraints = NULL
#' @param Monotone_Constraints_method = 'advanced'
#' @param Monotone_Penalty = 0.0
#' @param Forcedsplits_Filename = NULL
#' @param Refit_Decay_Rate = 0.90
#' @param Path_Smooth = 0.0
#'
#' # IO Dataset Parameters
#' # https://lightgbm.readthedocs.io/en/latest/Parameters.html#io-parameters
#' @param Max_Bin = 255
#' @param Min_Data_In_Bin = 3
#' @param Data_Random_Seed = 1
#' @param Is_Enable_Sparse = TRUE
#' @param Enable_Bundle = TRUE
#' @param Use_Missing = TRUE
#' @param Zero_As_Missing = FALSE
#' @param Two_Round = FALSE
#'
#' # Convert Parameters
#' @param Convert_Model = NULL
#' @param Convert_Model_Language = 'cpp'
#'
#' # Objective Parameters
#' # https://lightgbm.readthedocs.io/en/latest/Parameters.html#objective-parameters
#' @param Boost_From_Average = TRUE
#' @param Alpha = 0.90
#' @param Fair_C = 1.0
#' @param Poisson_Max_Delta_Step = 0.70
#' @param Tweedie_Variance_Power = 1.5
#' @param Lambdarank_Truncation_Level = 30
#'
#' # Metric Parameters (metric is in Core)
#' # https://lightgbm.readthedocs.io/en/latest/Parameters.html#metric-parameters
#' @param Is_Provide_Training_Metric = TRUE,
#' @param Eval_At = c(1,2,3,4,5)
#'
#' # Network Parameters
#' # https://lightgbm.readthedocs.io/en/latest/Parameters.html#network-parameters
#' @param Num_Machines = 1
#'
#' # GPU Parameters
#' @param # https://lightgbm.readthedocs.io/en/latest/Parameters.html#gpu-parameters
#' @param Gpu_Platform_Id = -1
#' @param Gpu_Device_Id = -1
#' @param Gpu_Use_Dp = TRUE
#' @param Num_Gpu = 1
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
#' TestModel <- RemixAutoML::AutoLightGBMFunnelCARMA(
#'
#'   # Data Arguments
#'   data = ModelData,
#'   GroupVariables = NULL,
#'   BaseFunnelMeasure = "Leads", # if you have XREGS, supply vector such as c("Leads", "XREGS1", "XREGS2")
#'   ConversionMeasure = "Appointments",
#'   ConversionRateMeasure = NULL,
#'   CohortPeriodsVariable = "CohortDays",
#'   WeightsColumnName = NULL,
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
#'   EncodingMethod = "credibility",
#'   NThreads = parallel::detectCores(),
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
#'   LossFunction = 'regression',
#'   EvalMetric = 'mae',
#'   GridEvalMetric = 'mae',
#'
#'   # LightGBM Args
#'   Device_Type = 'CPU',
#'   Input_Model = NULL,
#'   Task = 'train',
#'   Boosting = 'gbdt',
#'   LinearTree = FALSE,
#'   Trees = 50,
#'   ETA = 0.10,
#'   Num_Leaves = 31,
#'   Deterministic = TRUE,
#'
#'   # Learning Parameters
#'   # https://lightgbm.readthedocs.io/en/latest/Parameters.html#learning-control-parameters
#'   Force_Col_Wise = FALSE,
#'   Force_Row_Wise = FALSE,
#'   Max_Depth = 6,
#'   Min_Data_In_Leaf = 20,
#'   Min_Sum_Hessian_In_Leaf = 0.001,
#'   Bagging_Freq = 1.0,
#'   Bagging_Fraction = 1.0,
#'   Feature_Fraction = 1.0,
#'   Feature_Fraction_Bynode = 1.0,
#'   Lambda_L1 = 0.0,
#'   Lambda_L2 = 0.0,
#'   Extra_Trees = FALSE,
#'   Early_Stopping_Round = 10,
#'   First_Metric_Only = TRUE,
#'   Max_Delta_Step = 0.0,
#'   Linear_Lambda = 0.0,
#'   Min_Gain_To_Split = 0,
#'   Drop_Rate_Dart = 0.10,
#'   Max_Drop_Dart = 50,
#'   Skip_Drop_Dart = 0.50,
#'   Uniform_Drop_Dart = FALSE,
#'   Top_Rate_Goss = FALSE,
#'   Other_Rate_Goss = FALSE,
#'   Monotone_Constraints = NULL,
#'   Monotone_Constraints_method = 'advanced',
#'   Monotone_Penalty = 0.0,
#'   Forcedsplits_Filename = NULL, # use for AutoStack option; .json file
#'   Refit_Decay_Rate = 0.90,
#'   Path_Smooth = 0.0,
#'
#'   # IO Dataset Parameters
#'   # https://lightgbm.readthedocs.io/en/latest/Parameters.html#io-parameters
#'   Max_Bin = 255,
#'   Min_Data_In_Bin = 3,
#'   Data_Random_Seed = 1,
#'   Is_Enable_Sparse = TRUE,
#'   Enable_Bundle = TRUE,
#'   Use_Missing = TRUE,
#'   Zero_As_Missing = FALSE,
#'   Two_Round = FALSE,
#'
#'   # Convert Parameters
#'   Convert_Model = NULL,
#'   Convert_Model_Language = 'cpp',
#'
#'   # Objective Parameters
#'   # https://lightgbm.readthedocs.io/en/latest/Parameters.html#objective-parameters
#'   Boost_From_Average = TRUE,
#'   Alpha = 0.90,
#'   Fair_C = 1.0,
#'   Poisson_Max_Delta_Step = 0.70,
#'   Tweedie_Variance_Power = 1.5,
#'   Lambdarank_Truncation_Level = 30,
#'
#'   # Metric Parameters (metric is in Core)
#'   # https://lightgbm.readthedocs.io/en/latest/Parameters.html#metric-parameters
#'   Is_Provide_Training_Metric = TRUE,
#'   Eval_At = c(1,2,3,4,5),
#'
#'   # Network Parameters
#'   # https://lightgbm.readthedocs.io/en/latest/Parameters.html#network-parameters
#'   Num_Machines = 1,
#'
#'   # GPU Parameters
#'   # https://lightgbm.readthedocs.io/en/latest/Parameters.html#gpu-parameters
#'   Gpu_Platform_Id = -1,
#'   Gpu_Device_Id = -1,
#'   Gpu_Use_Dp = TRUE,
#'   Num_Gpu = 1)
#'
#' # Separate out the Base Funnel Measures Data
#' LeadsData <- data[, lapply(.SD, data.table::first), .SDcols = c("Leads"), by = c("CalendarDateColumn")]
#' ModelData <- ModelDataBase[, Leads := NULL]
#'
#' # Forecast Funnel Model
#' Test <- RemixAutoML::AutoLightGBMFunnelCARMAScoring(
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
AutoLightGBMFunnelCARMA <- function(data,
                                    GroupVariables = NULL,
                                    BaseFunnelMeasure = NULL,
                                    ConversionMeasure = NULL,
                                    ConversionRateMeasure = NULL,
                                    CohortPeriodsVariable = NULL,
                                    CalendarDate = NULL,
                                    CohortDate = NULL,

                                    # Additional Services
                                    EncodingMethod = "credibility",
                                    OutputSelection = c('Importances', 'EvalPlots', 'EvalMetrics', 'Score_TrainData'),
                                    WeightsColumnName = NULL,
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

                                    # ML Setup Parameters
                                    LossFunction = 'regression',
                                    EvalMetric = 'mae',
                                    GridEvalMetric = 'mae',
                                    NumOfParDepPlots = 1L,

                                    # LightGBM Args
                                    Device_Type = 'CPU',
                                    Input_Model = NULL,
                                    Task = 'train',
                                    Boosting = 'gbdt',
                                    LinearTree = FALSE,
                                    Trees = 1000,
                                    ETA = 0.10,
                                    Num_Leaves = 31,
                                    Deterministic = TRUE,
                                    NThreads = parallel::detectCores() / 2,
                                    SaveInfoToPDF = FALSE,

                                    # Learning Parameters
                                    # https://lightgbm.readthedocs.io/en/latest/Parameters.html#learning-control-parameters
                                    Force_Col_Wise = FALSE,
                                    Force_Row_Wise = FALSE,
                                    Max_Depth = 6,
                                    Min_Data_In_Leaf = 20,
                                    Min_Sum_Hessian_In_Leaf = 0.001,
                                    Bagging_Freq = 1.0,
                                    Bagging_Fraction = 1.0,
                                    Feature_Fraction = 1.0,
                                    Feature_Fraction_Bynode = 1.0,
                                    Lambda_L1 = 0.0,
                                    Lambda_L2 = 0.0,
                                    Extra_Trees = FALSE,
                                    Early_Stopping_Round = 10,
                                    First_Metric_Only = TRUE,
                                    Max_Delta_Step = 0.0,
                                    Linear_Lambda = 0.0,
                                    Min_Gain_To_Split = 0,
                                    Drop_Rate_Dart = 0.10,
                                    Max_Drop_Dart = 50,
                                    Skip_Drop_Dart = 0.50,
                                    Uniform_Drop_Dart = FALSE,
                                    Top_Rate_Goss = FALSE,
                                    Other_Rate_Goss = FALSE,
                                    Monotone_Constraints = NULL,
                                    Monotone_Constraints_method = 'advanced',
                                    Monotone_Penalty = 0.0,
                                    Forcedsplits_Filename = NULL, # use for AutoStack option; .json file
                                    Refit_Decay_Rate = 0.90,
                                    Path_Smooth = 0.0,

                                    # IO Dataset Parameters
                                    # https://lightgbm.readthedocs.io/en/latest/Parameters.html#io-parameters
                                    Max_Bin = 255,
                                    Min_Data_In_Bin = 3,
                                    Data_Random_Seed = 1,
                                    Is_Enable_Sparse = TRUE,
                                    Enable_Bundle = TRUE,
                                    Use_Missing = TRUE,
                                    Zero_As_Missing = FALSE,
                                    Two_Round = FALSE,

                                    # Convert Parameters
                                    Convert_Model = NULL,
                                    Convert_Model_Language = 'cpp',

                                    # Objective Parameters
                                    # https://lightgbm.readthedocs.io/en/latest/Parameters.html#objective-parameters
                                    Boost_From_Average = TRUE,
                                    Alpha = 0.90,
                                    Fair_C = 1.0,
                                    Poisson_Max_Delta_Step = 0.70,
                                    Tweedie_Variance_Power = 1.5,
                                    Lambdarank_Truncation_Level = 30,

                                    # Metric Parameters (metric is in Core)
                                    # https://lightgbm.readthedocs.io/en/latest/Parameters.html#metric-parameters
                                    Is_Provide_Training_Metric = TRUE,
                                    Eval_At = c(1,2,3,4,5),

                                    # Network Parameters
                                    # https://lightgbm.readthedocs.io/en/latest/Parameters.html#network-parameters
                                    Num_Machines = 1,

                                    # GPU Parameters
                                    # https://lightgbm.readthedocs.io/en/latest/Parameters.html#gpu-parameters
                                    Gpu_Platform_Id = -1,
                                    Gpu_Device_Id = -1,
                                    Gpu_Use_Dp = TRUE,
                                    Num_Gpu = 1) {

  # Admin: Ensure ModelPath and MetaDataPath exists ----
  if(!is.null(ModelPath)) if(!dir.exists(file.path(ModelPath))) dir.create(ModelPath)
  if(!is.null(MetaDataPath)) if(!is.null(MetaDataPath)) if(!dir.exists(file.path(MetaDataPath))) dir.create(MetaDataPath)

  # Admin: Args List ----
  ArgsList <- list()

  # Model Selection
  ArgsList[["Algorithm"]] <- "LightGBM"

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
  ArgsList[["WeightsColumnName"]] <- WeightsColumnName

  # Metadata Args
  if(is.null(MetaDataPath)) if(!is.null(ModelPath)) MetaDataPath <- ModelPath
  ArgsList[["NThreads"]] <- NThreads
  ArgsList[["ModelID"]] <- ModelID
  ArgsList[["ModelPath"]] <- ModelPath
  ArgsList[["MetaDataPath"]] <- MetaDataPath
  ArgsList[["Jobs"]] <- Jobs

  # Feature Engineering Args
  ArgsList[["EncodingMethod"]] <- EncodingMethod
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
  ArgsList[["GridEvalMetric"]] <- GridEvalMetric
  ArgsList[["EvalMetric"]] <- EvalMetric

  # LightGBM Args
  ArgsList[["Device_Type"]] <- Device_Type
  ArgsList[["Input_Model"]] <- Input_Model
  ArgsList[["Task"]] <- Task
  ArgsList[["Boosting"]] <- Boosting
  ArgsList[["LinearTree"]] <- LinearTree
  ArgsList[["Trees"]] <- Trees
  ArgsList[["ETA"]] <- ETA
  ArgsList[["Num_Leaves"]] <- Num_Leaves
  ArgsList[["Deterministic"]] <- Deterministic

  # Learning Parameters
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#learning-control-parameters
  ArgsList[["Force_Col_Wise"]] <- Force_Col_Wise
  ArgsList[["Force_Row_Wise"]] <- Force_Row_Wise
  ArgsList[["Max_Depth"]] <- Max_Depth
  ArgsList[["Min_Data_In_Leaf"]] <- Min_Data_In_Leaf
  ArgsList[["Min_Sum_Hessian_In_Leaf"]] <- Min_Sum_Hessian_In_Leaf
  ArgsList[["Bagging_Freq"]] <- Bagging_Freq
  ArgsList[["Bagging_Fraction"]] <- Bagging_Fraction
  ArgsList[["Feature_Fraction"]] <- Feature_Fraction
  ArgsList[["Feature_Fraction_Bynode"]] <- Feature_Fraction_Bynode
  ArgsList[["Lambda_L1"]] <- Lambda_L1
  ArgsList[["Lambda_L2"]] <- Lambda_L2
  ArgsList[["Extra_Trees"]] <- Extra_Trees
  ArgsList[["Early_Stopping_Round"]] <- Early_Stopping_Round
  ArgsList[["First_Metric_Only"]] <- First_Metric_Only
  ArgsList[["Max_Delta_Step"]] <- Max_Delta_Step
  ArgsList[["Linear_Lambda"]] <- Linear_Lambda
  ArgsList[["Min_Gain_To_Split"]] <- Min_Gain_To_Split
  ArgsList[["Drop_Rate_Dart"]] <- Drop_Rate_Dart
  ArgsList[["Max_Drop_Dart"]] <- Max_Drop_Dart
  ArgsList[["Skip_Drop_Dart"]] <- Skip_Drop_Dart
  ArgsList[["Uniform_Drop_Dart"]] <- Uniform_Drop_Dart
  ArgsList[["Top_Rate_Goss"]] <- Top_Rate_Goss
  ArgsList[["Other_Rate_Goss"]] <- Other_Rate_Goss
  ArgsList[["Monotone_Constraints"]] <- Monotone_Constraints
  ArgsList[["Monotone_Constraints_method"]] <- Monotone_Constraints_method
  ArgsList[["Monotone_Penalty"]] <- Monotone_Penalty
  ArgsList[["Forcedsplits_Filename"]] <- Forcedsplits_Filename
  ArgsList[["Refit_Decay_Rate"]] <- Refit_Decay_Rate
  ArgsList[["Path_Smooth"]] <- Path_Smooth

  # IO Dataset Parameters
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#io-parameters
  ArgsList[["Max_Bin"]] <- Max_Bin
  ArgsList[["Min_Data_In_Bin"]] <- Min_Data_In_Bin
  ArgsList[["Data_Random_Seed"]] <- Data_Random_Seed
  ArgsList[["Is_Enable_Sparse"]] <- Is_Enable_Sparse
  ArgsList[["Enable_Bundle"]] <- Enable_Bundle
  ArgsList[["Use_Missing"]] <- Use_Missing
  ArgsList[["Zero_As_Missing"]] <- Zero_As_Missing
  ArgsList[["Two_Round"]] <- Two_Round

  # Convert Parameters
  ArgsList[["Convert_Model"]] <- Convert_Model
  ArgsList[["Convert_Model_Language"]] <- Convert_Model_Language

  # Objective Parameters
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#objective-parameters
  ArgsList[["Boost_From_Average"]] <- Boost_From_Average
  ArgsList[["Alpha"]] <- Alpha
  ArgsList[["Fair_C"]] <- Fair_C
  ArgsList[["Poisson_Max_Delta_Step"]] <- Poisson_Max_Delta_Step
  ArgsList[["Tweedie_Variance_Power"]] <- Tweedie_Variance_Power
  ArgsList[["Lambdarank_Truncation_Level"]] <- Lambdarank_Truncation_Level

  # Metric Parameters (metric is in Core)
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#metric-parameters
  ArgsList[["Is_Provide_Training_Metric"]] <- Is_Provide_Training_Metric
  ArgsList[["Eval_At"]] <- Eval_At

  # Network Parameters
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#network-parameters
  ArgsList[["Num_Machines"]] <- Num_Machines

  # GPU Parameters
  # https://lightgbm.readthedocs.io/en/latest/Parameters.html#gpu-parameters
  ArgsList[["Gpu_Platform_Id"]] <- Gpu_Platform_Id
  ArgsList[["Gpu_Device_Id"]] <- Gpu_Device_Id
  ArgsList[["Gpu_Use_Dp"]] <- Gpu_Use_Dp
  ArgsList[["Num_Gpu"]] <- Num_Gpu

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

    # FE: AutoLagRollStats() CohortDate HolidayCounts OVER CohortDate ----
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

    # ML: LightGBMRegression() ----
    if(DebugMode) print("ML: LightGBMRegression() ----")
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
      x <- system.time(gcFirst = FALSE, TestModel <- RemixAutoML::AutoLightGBMRegression(

        # Metadata arguments
        ModelID = paste0(ModelID,"_", proc, "_"),
        model_path = ModelPath,
        metadata_path = MetaDataPath,
        SaveModelObjects = FALSE,
        ReturnModelObjects = TRUE,
        NThreads = NThreads,
        WeightsColumnName = WeightsColumnName,
        OutputSelection = OutputSelection,
        DebugMode = DebugMode, SaveInfoToPDF = FALSE,
        ReturnFactorLevels = TRUE,
        EncodingMethod = EncodingMethod,

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
        NumOfParDepPlots = NumOfParDepPlots,

        # Grid tuning arguments
        PassInGrid = PassInGrid,
        GridTune = GridTune,
        MaxModelsInGrid = MaxModelsInGrid,
        MaxRunsWithoutNewWinner = MaxRunsWithoutNewWinner,
        MaxRunMinutes = MaxRunMinutes,
        BaselineComparison = BaselineComparison,
        grid_eval_metric = GridEvalMetric,

        # Core parameters
        # https://lightgbm.readthedocs.io/en/latest/Parameters.html#core-parameters
        device_type = tolower(Device_Type),
        objective = LossFunction,
        metric = EvalMetric,
        input_model = Input_Model,
        task = Task,
        boosting = Boosting,
        LinearTree = LinearTree,
        Trees = NTrees,
        eta = ETA,
        num_leaves = Num_Leaves,
        deterministic = Deterministic,

        # Learning Parameters
        # https://lightgbm.readthedocs.io/en/latest/Parameters.html#learning-control-parameters
        force_col_wise = Force_Col_Wise,
        force_row_wise = Force_Row_Wise,
        max_depth = Max_Depth,
        min_data_in_leaf = Min_Data_In_Leaf,
        min_sum_hessian_in_leaf = Min_Sum_Hessian_In_Leaf,
        bagging_freq = Bagging_Freq,
        bagging_fraction = Bagging_Fraction,
        feature_fraction = Feature_Fraction,
        feature_fraction_bynode = Feature_Fraction_Bynode,
        lambda_l1 = Lambda_L1,
        lambda_l2 = Lambda_L2,
        extra_trees = Extra_Trees,
        early_stopping_round = Early_Stopping_Round,
        first_metric_only = First_Metric_Only,
        max_delta_step = Max_Delta_Step,
        linear_lambda = Linear_Lambda,
        min_gain_to_split = Min_Gain_To_Split,
        drop_rate_dart = Drop_Rate_Dart,
        max_drop_dart = Max_Drop_Dart,
        skip_drop_dart = Skip_Drop_Dart,
        uniform_drop_dart = Uniform_Drop_Dart,
        top_rate_goss = Top_Rate_Goss,
        other_rate_goss = Other_Rate_Goss,
        monotone_constraints = Monotone_Constraints,
        monotone_constraints_method = Monotone_Constraints_Method,
        monotone_penalty = Monotone_Penalty,
        forcedsplits_filename = Forcedsplits_Filename, # use for AutoStack option; .json file
        refit_decay_rate = Refit_Decay_Rate,
        path_smooth = Path_Smooth,

        # IO Dataset Parameters
        # https://lightgbm.readthedocs.io/en/latest/Parameters.html#io-parameters
        max_bin = Max_Bin,
        min_data_in_bin = Min_Data_In_Bin,
        data_random_seed = Data_Random_Seed,
        is_enable_sparse = Is_Enable_Sparse,
        enable_bundle = Enable_Bundle,
        use_missing = Use_Missing,
        zero_as_missing = Zero_As_Missing,
        two_round = Two_Round,

        # Convert Parameters
        convert_model = Convert_Model,
        convert_model_language = Convert_Model_Language,

        # Objective Parameters
        # https://lightgbm.readthedocs.io/en/latest/Parameters.html#objective-parameters
        boost_from_average = Boost_From_Average,
        alpha = Alpha,
        fair_c = Fair_C,
        poisson_max_delta_step = Poisson_Max_Delta_Step,
        tweedie_variance_power = Tweedie_Variance_Power,
        lambdarank_truncation_level = Lambdarank_Truncation_Level,

        # Metric Parameters (metric is in Core)
        # https://lightgbm.readthedocs.io/en/latest/Parameters.html#metric-parameters
        is_provide_training_metric = TRUE,
        eval_at = Eval_At,

        # Network Parameters
        # https://lightgbm.readthedocs.io/en/latest/Parameters.html#network-parameters
        num_machines = Num_Machines,

        # GPU Parameters
        # https://lightgbm.readthedocs.io/en/latest/Parameters.html#gpu-parameters
        gpu_platform_id = Gpu_Platform_Id,
        gpu_device_id = Gpu_Device_Id,
        gpu_use_dp = Gpu_Use_Dp,
        num_gpu = Num_Gpu))

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

      # Define number of trees ----
      if(proc %chin% c("eval","evaluate")) {
        TreeCount <- TestModel$Model$best_iter
        if(!is.null(Trees)) NTrees <- TreeCount else NTrees <- Trees
        ArgsList[["Trees"]] <- NTrees
      } else {
        NTrees <- Trees
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
          if(!zz %chin% c("Model", "ColNames", "TransformationResults", "FactorLevelsList")) {
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

#' @title AutoLightGBMFunnelCARMAScoring
#'
#' @description AutoLightGBMFunnelCARMAScoring for generating forecasts
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
#' # Create Fake Data
#' data <- RemixAutoML::FakeDataGenerator(ChainLadderData = TRUE)
#'
#' # Subset data for training
#' ModelDataBase <- data[CalendarDateColumn < '2020-01-01' & CohortDateColumn < '2020-01-01']
#' ModelData <- data.table::copy(ModelDataBase)
#'
#' # Train Funne Model
#' TestModel <- RemixAutoML::AutoLightGBMFunnelCARMA(
#'
#'   # Data Arguments
#'   data = ModelData,
#'   GroupVariables = NULL,
#'   BaseFunnelMeasure = "Leads", # if you have XREGS, supply vector such as c("Leads", "XREGS1", "XREGS2")
#'   ConversionMeasure = "Appointments",
#'   ConversionRateMeasure = NULL,
#'   CohortPeriodsVariable = "CohortDays",
#'   WeightsColumnName = NULL,
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
#'   EncodingMethod = "credibility",
#'   NThreads = parallel::detectCores(),
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
#'   LossFunction = 'regression',
#'   EvalMetric = 'mae',
#'   GridEvalMetric = 'mae',
#'
#'   # LightGBM Args
#'   Device_Type = 'CPU',
#'   Input_Model = NULL,
#'   Task = 'train',
#'   Boosting = 'gbdt',
#'   LinearTree = FALSE,
#'   Trees = 50,
#'   ETA = 0.10,
#'   Num_Leaves = 31,
#'   Deterministic = TRUE,
#'
#'   # Learning Parameters
#'   # https://lightgbm.readthedocs.io/en/latest/Parameters.html#learning-control-parameters
#'   Force_Col_Wise = FALSE,
#'   Force_Row_Wise = FALSE,
#'   Max_Depth = 6,
#'   Min_Data_In_Leaf = 20,
#'   Min_Sum_Hessian_In_Leaf = 0.001,
#'   Bagging_Freq = 1.0,
#'   Bagging_Fraction = 1.0,
#'   Feature_Fraction = 1.0,
#'   Feature_Fraction_Bynode = 1.0,
#'   Lambda_L1 = 0.0,
#'   Lambda_L2 = 0.0,
#'   Extra_Trees = FALSE,
#'   Early_Stopping_Round = 10,
#'   First_Metric_Only = TRUE,
#'   Max_Delta_Step = 0.0,
#'   Linear_Lambda = 0.0,
#'   Min_Gain_To_Split = 0,
#'   Drop_Rate_Dart = 0.10,
#'   Max_Drop_Dart = 50,
#'   Skip_Drop_Dart = 0.50,
#'   Uniform_Drop_Dart = FALSE,
#'   Top_Rate_Goss = FALSE,
#'   Other_Rate_Goss = FALSE,
#'   Monotone_Constraints = NULL,
#'   Monotone_Constraints_method = 'advanced',
#'   Monotone_Penalty = 0.0,
#'   Forcedsplits_Filename = NULL, # use for AutoStack option; .json file
#'   Refit_Decay_Rate = 0.90,
#'   Path_Smooth = 0.0,
#'
#'   # IO Dataset Parameters
#'   # https://lightgbm.readthedocs.io/en/latest/Parameters.html#io-parameters
#'   Max_Bin = 255,
#'   Min_Data_In_Bin = 3,
#'   Data_Random_Seed = 1,
#'   Is_Enable_Sparse = TRUE,
#'   Enable_Bundle = TRUE,
#'   Use_Missing = TRUE,
#'   Zero_As_Missing = FALSE,
#'   Two_Round = FALSE,
#'
#'   # Convert Parameters
#'   Convert_Model = NULL,
#'   Convert_Model_Language = 'cpp',
#'
#'   # Objective Parameters
#'   # https://lightgbm.readthedocs.io/en/latest/Parameters.html#objective-parameters
#'   Boost_From_Average = TRUE,
#'   Alpha = 0.90,
#'   Fair_C = 1.0,
#'   Poisson_Max_Delta_Step = 0.70,
#'   Tweedie_Variance_Power = 1.5,
#'   Lambdarank_Truncation_Level = 30,
#'
#'   # Metric Parameters (metric is in Core)
#'   # https://lightgbm.readthedocs.io/en/latest/Parameters.html#metric-parameters
#'   Is_Provide_Training_Metric = TRUE,
#'   Eval_At = c(1,2,3,4,5),
#'
#'   # Network Parameters
#'   # https://lightgbm.readthedocs.io/en/latest/Parameters.html#network-parameters
#'   Num_Machines = 1,
#'
#'   # GPU Parameters
#'   # https://lightgbm.readthedocs.io/en/latest/Parameters.html#gpu-parameters
#'   Gpu_Platform_Id = -1,
#'   Gpu_Device_Id = -1,
#'   Gpu_Use_Dp = TRUE,
#'   Num_Gpu = 1)
#'
#' # Separate out the Base Funnel Measures Data
#' LeadsData <- data[, lapply(.SD, data.table::first), .SDcols = c("Leads"), by = c("CalendarDateColumn")]
#' ModelData <- ModelDataBase[, Leads := NULL]
#'
#' # Forecast Funnel Model
#' Test <- RemixAutoML::AutoLightGBMFunnelCARMAScoring(
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
AutoLightGBMFunnelCARMAScoring <- function(TrainData,
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
    for(zz in seq_along(TrainOutput)) if(!names(TrainOutput)[zz] %chin% c("Model", "ColNames", "TransformationResults", "FactorLevelsList")) TrainOutput[[zz]] <- NULL
  }

  # Admin: Turn off warnings ----
  options(warn = -1)

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
    temp <- RemixAutoML::AutoLightGBMScoring(
      FactorLevelsList = TrainOutput$FactorLevelsList,
      TargetType = "regression",
      ScoringData = temp,
      FeatureColumnNames = Features,
      IDcols = names(temp)[!names(temp) %chin% Features],
      EncodingMethod = ArgsList$EncodingMethod,
      ReturnShapValues = FALSE,
      ModelObject = TrainOutput$Model,
      ModelPath = if(is.null(TrainOutput)) ArgsList$ModelPath else NULL,
      ModelID = ArgsList$ModelID,
      ReturnFeatures = FALSE,
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
      MDP_MissNum = -1)

    # DE: Update forecast TrainData ----
    if(DebugMode) print("DE: Update forecast TrainData ----")
    temp1[ScoreRecords == 1, eval(ArgsList$ConversionRateMeasure) := temp[which(Predictions < 0), Predictions := 0][[1L]]]
    temp1[ScoreRecords == 1, eval(ArgsList$ConversionMeasure) := get(ArgsList$ConversionRateMeasure) * get(ArgsList$BaseFunnelMeasure[1L])]
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

  # Admin: Turn back to normal ----
  options(warn = 1)

  # Return Forecast Data With Features ----
  return(TrainData)
}
