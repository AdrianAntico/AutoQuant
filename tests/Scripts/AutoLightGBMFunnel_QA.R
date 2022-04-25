# Collection data.table
QA_Results <- data.table::CJ(
  Group = c(0,1,2,3),
  xregs = c(0,1,2,3),
  Trans = c(TRUE, FALSE),
  Training = "Failure",
  Forecast = "Failure")
QA_Results[, RunTimeTrain := 123.456]
QA_Results[, RunTimeScore := 123.456]
QA_Results[, DateTime := Sys.time()]

# run = 1
# run = 6
# run = 9
# run = 15
# run = 25
# run = 27
for(run in seq_len(QA_Results[,.N])) {

  # Get data ----
  if(QA_Results[run, Group] == 0) {
    groupvars <- NULL
    ModelData <- RemixAutoML:::Post_Query_Helper('"chainladdernogroupmodeldata.csv"')[['data']]
    data.table::setkeyv(x = ModelData, cols = c(groupvars, "CalendarDateColumn"))
    if(QA_Results[run, xregs] == 0) {
      LeadsData <- RemixAutoML:::Post_Query_Helper('"chainladdernogroupleadsdata.csv"')[['data']]
      data.table::setkeyv(x = ModelData, cols = c(groupvars, "CalendarDateColumn"))
    } else if(QA_Results[run, xregs] == 1) {
      LeadsData <- RemixAutoML:::Post_Query_Helper('"chainladdernogroupleadsdataxregs1.csv"')[['data']]
      data.table::setkeyv(x = ModelData, cols = c(groupvars, "CalendarDateColumn"))
    } else if(QA_Results[run, xregs] == 2) {
      LeadsData <- RemixAutoML:::Post_Query_Helper('"chainladdernogroupleadsdataxregs2.csv"')[['data']]
      data.table::setkeyv(x = ModelData, cols = c(groupvars, "CalendarDateColumn"))
    } else if(QA_Results[run, xregs] == 3) {
      LeadsData <- RemixAutoML:::Post_Query_Helper('"chainladdernogroupleadsdataxregs3.csv"')[['data']]
      data.table::setkeyv(x = ModelData, cols = c(groupvars, "CalendarDateColumn"))
    }
  } else if(QA_Results[run, Group] == 1) {
    groupvars <- "MarketingSegments"
    ModelData <- RemixAutoML:::Post_Query_Helper('"chainladderonegroupmodeldata.csv"')[['data']]
    data.table::setkeyv(x = ModelData, cols = c(groupvars, "CalendarDateColumn"))
    if(QA_Results[run, xregs] == 0) {
      LeadsData <- RemixAutoML:::Post_Query_Helper('"chainladderonegroupleadsdata.csv"')[['data']]
      data.table::setkeyv(x = ModelData, cols = c(groupvars, "CalendarDateColumn"))
    } else if(QA_Results[run, xregs] == 1) {
      LeadsData <- RemixAutoML:::Post_Query_Helper('"chainladderonegroupleadsdataxregs1.csv"')[['data']]
      data.table::setkeyv(x = ModelData, cols = c(groupvars, "CalendarDateColumn"))
    } else if(QA_Results[run, xregs] == 2) {
      LeadsData <- RemixAutoML:::Post_Query_Helper('"chainladderonegroupleadsdataxregs2.csv"')[['data']]
      data.table::setkeyv(x = ModelData, cols = c(groupvars, "CalendarDateColumn"))
    } else if(QA_Results[run, xregs] == 3) {
      LeadsData <- RemixAutoML:::Post_Query_Helper('"chainladderonegroupleadsdataxregs3.csv"')[['data']]
      data.table::setkeyv(x = ModelData, cols = c(groupvars, "CalendarDateColumn"))
    }
  } else if(QA_Results[run, Group] == 2) {
    groupvars <- c("MarketingSegments","MarketingSegments2")
    ModelData <- RemixAutoML:::Post_Query_Helper('"chainladdertwogroupmodeldata.csv"')[['data']]
    data.table::setkeyv(x = ModelData, cols = c(groupvars, "CalendarDateColumn"))
    if(QA_Results[run, xregs] == 0) {
      LeadsData <- RemixAutoML:::Post_Query_Helper('"chainladdertwogroupleadsdata.csv"')[['data']]
      data.table::setkeyv(x = ModelData, cols = c(groupvars, "CalendarDateColumn"))
    } else if(QA_Results[run, xregs] == 1) {
      LeadsData <- RemixAutoML:::Post_Query_Helper('"chainladdertwogroupleadsdataxregs1.csv"')[['data']]
      data.table::setkeyv(x = ModelData, cols = c(groupvars, "CalendarDateColumn"))
    } else if(QA_Results[run, xregs] == 2) {
      LeadsData <- RemixAutoML:::Post_Query_Helper('"chainladdertwogroupleadsdataxregs2.csv"')[['data']]
      data.table::setkeyv(x = ModelData, cols = c(groupvars, "CalendarDateColumn"))
    } else if(QA_Results[run, xregs] == 3) {
      LeadsData<- RemixAutoML:::Post_Query_Helper('"chainladdertwogroupleadsdataxregs3.csv"')[['data']]
      data.table::setkeyv(x = ModelData, cols = c(groupvars, "CalendarDateColumn"))
    }
  } else if(QA_Results[run, Group] == 3) {
    groupvars <- c("MarketingSegments","MarketingSegments2","MarketingSegments3")
    ModelData <- RemixAutoML:::Post_Query_Helper('"chainladderthreegroupmodeldata.csv"')[['data']]
    data.table::setkeyv(x = ModelData, cols = c(groupvars, "CalendarDateColumn"))
    if(QA_Results[run, xregs] == 0) {
      LeadsData <- RemixAutoML:::Post_Query_Helper('"chainladderthreegroupleadsdata.csv"')[['data']]
      data.table::setkeyv(x = ModelData, cols = c(groupvars, "CalendarDateColumn"))
    } else if(QA_Results[run, xregs] == 1) {
      LeadsData <- RemixAutoML:::Post_Query_Helper('"chainladderthreegroupleadsdataxregs1.csv"')[['data']]
      data.table::setkeyv(x = ModelData, cols = c(groupvars, "CalendarDateColumn"))
    } else if(QA_Results[run, xregs] == 2) {
      LeadsData <- RemixAutoML:::Post_Query_Helper('"chainladderthreegroupleadsdataxregs2.csv"')[['data']]
      data.table::setkeyv(x = ModelData, cols = c(groupvars, "CalendarDateColumn"))
    } else if(QA_Results[run, xregs] == 3) {
      LeadsData <- RemixAutoML:::Post_Query_Helper('"chainladderthreegroupleadsdataxregs3.csv"')[['data']]
      data.table::setkeyv(x = ModelData, cols = c(groupvars, "CalendarDateColumn"))
    }
  }

  # Join data
  keep <- names(LeadsData)
  keep <- keep[!keep %in% c(groupvars, "CalendarDateColumn")]
  ModelData[LeadsData, paste0(keep) := mget(paste0("i.", keep))]

  # Start Timer
  Start <- Sys.time()

  # Build model
  TestModel <- tryCatch({RemixAutoML::AutoLightGBMFunnelCARMA(

    # Data Arguments
    data = ModelData,
    GroupVariables = groupvars,
    BaseFunnelMeasure = keep,
    ConversionMeasure = "Appointments",
    ConversionRateMeasure = NULL,
    CohortPeriodsVariable = "CohortDays",
    CalendarDate = "CalendarDateColumn",
    CohortDate = "CohortDateColumn",
    PartitionRatios = c(0.70,0.20,0.10),
    TruncateDate = NULL,
    TimeUnit = "days",
    TransformTargetVariable = QA_Results[run, Trans],
    TransformMethods = c("Asinh","Asin","Log","LogPlus1","Sqrt","Logit"),
    AnomalyDetection = list(tstat_high = 3, tstat_low = -2),

    # MetaData Arguments
    Jobs = c("eval","train"),
    SaveModelObjects = FALSE,
    ModelID = "ModelTest",
    ModelPath = getwd(),
    MetaDataPath = NULL,
    DebugMode = TRUE,
    NumOfParDepPlots = 1L,
    EncodingMethod = "credibility",
    NThreads = parallel::detectCores() / 2,

    # Feature Engineering Arguments
    CalendarTimeGroups = c("days","weeks","months"),
    CohortTimeGroups = c("days", "weeks"),
    CalendarVariables = c("wday","mday","yday","week","month","quarter","year"),
    HolidayGroups = c("USPublicHolidays","EasterGroup","ChristmasGroup","OtherEcclesticalFeasts"),
    HolidayLookback = NULL,
    CohortHolidayLags = c(1L,2L,7L),
    CohortHolidayMovingAverages = c(3L,7L),
    CalendarHolidayLags = c(1L,2L,7L),
    CalendarHolidayMovingAverages = c(3L,7L),

    # Time Series Features
    ImputeRollStats = -0.001,
    CalendarLags = list("day" = c(1L,2L,7L,35L,42L), "week" = c(5L,6L,10L,12L,25L,26L)),
    CalendarMovingAverages = list("day" = c(7L,14L,35L,42L), "week" = c(5L,6L,10L,12L,20L,24L), "month" = c(6L,12L)),
    CalendarStandardDeviations = NULL,
    CalendarSkews = NULL,
    CalendarKurts = NULL,
    CalendarQuantiles = NULL,
    CalendarQuantilesSelected = "q50",
    CohortLags = list("day" = c(1L,2L,7L,35L,42L), "week" = c(5L,6L)),
    CohortMovingAverages = list("day" = c(7L,14L,35L,42L), "week" = c(5L,6L), "month" = c(1L,2L)),
    CohortStandardDeviations = NULL,
    CohortSkews = NULL,
    CohortKurts = NULL,
    CohortQuantiles = NULL,
    CohortQuantilesSelected = "q50",

    # ML Grid Tuning
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

    # LightGBM Args
    Device_Type = 'CPU',
    Input_Model = NULL,
    Task = 'train',
    Boosting = 'gbdt',
    LinearTree = FALSE,
    Trees = 50,
    ETA = 0.10,
    Num_Leaves = 31,
    Deterministic = TRUE,

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
    Num_Gpu = 1)}, error = function(x) NULL)

  # Timer
  End <- Sys.time()
  QA_Results[run, RunTimeTrain := as.numeric(difftime(time1 = End, Start))]

  # Outcome
  if(!is.null(TestModel)) QA_Results[run, Training := "Success"]
  RemixAutoML:::Post_Append_Helper(QA_Results,'AutoLightGBMFunnel_QA')

  # Forecast QA
  if(!is.null(TestModel)) {

    # Refresh data
    if(QA_Results[run, Group] == 0) {
      groupvars <- NULL
      ModelData <- RemixAutoML:::Post_Query_Helper('"chainladdernogroupmodeldata.csv"')[['data']]
      data.table::setkeyv(x = ModelData, cols = c(groupvars, "CalendarDateColumn"))
      if(QA_Results[run, xregs] == 0) {
        LeadsData <- RemixAutoML:::Post_Query_Helper('"chainladdernogroupleadsdata.csv"')[['data']]
        data.table::setkeyv(x = ModelData, cols = c(groupvars, "CalendarDateColumn"))
      } else if(QA_Results[run, xregs] == 1) {
        LeadsData <- RemixAutoML:::Post_Query_Helper('"chainladdernogroupleadsdataxregs1.csv"')[['data']]
        data.table::setkeyv(x = ModelData, cols = c(groupvars, "CalendarDateColumn"))
      } else if(QA_Results[run, xregs] == 2) {
        LeadsData <- RemixAutoML:::Post_Query_Helper('"chainladdernogroupleadsdataxregs2.csv"')[['data']]
        data.table::setkeyv(x = ModelData, cols = c(groupvars, "CalendarDateColumn"))
      } else if(QA_Results[run, xregs] == 3) {
        LeadsData <- RemixAutoML:::Post_Query_Helper('"chainladdernogroupleadsdataxregs3.csv"')[['data']]
        data.table::setkeyv(x = ModelData, cols = c(groupvars, "CalendarDateColumn"))
      }
    } else if(QA_Results[run, Group] == 1) {
      groupvars <- "MarketingSegments"
      ModelData <- RemixAutoML:::Post_Query_Helper('"chainladderonegroupmodeldata.csv"')[['data']]
      data.table::setkeyv(x = ModelData, cols = c(groupvars, "CalendarDateColumn"))
      if(QA_Results[run, xregs] == 0) {
        LeadsData <- RemixAutoML:::Post_Query_Helper('"chainladderonegroupleadsdata.csv"')[['data']]
        data.table::setkeyv(x = ModelData, cols = c(groupvars, "CalendarDateColumn"))
      } else if(QA_Results[run, xregs] == 1) {
        LeadsData <- RemixAutoML:::Post_Query_Helper('"chainladderonegroupleadsdataxregs1.csv"')[['data']]
        data.table::setkeyv(x = ModelData, cols = c(groupvars, "CalendarDateColumn"))
      } else if(QA_Results[run, xregs] == 2) {
        LeadsData <- RemixAutoML:::Post_Query_Helper('"chainladderonegroupleadsdataxregs2.csv"')[['data']]
        data.table::setkeyv(x = ModelData, cols = c(groupvars, "CalendarDateColumn"))
      } else if(QA_Results[run, xregs] == 3) {
        LeadsData <- RemixAutoML:::Post_Query_Helper('"chainladderonegroupleadsdataxregs3.csv"')[['data']]
        data.table::setkeyv(x = ModelData, cols = c(groupvars, "CalendarDateColumn"))
      }
    } else if(QA_Results[run, Group] == 2) {
      groupvars <- c("MarketingSegments","MarketingSegments2")
      ModelData <- RemixAutoML:::Post_Query_Helper('"chainladdertwogroupmodeldata.csv"')[['data']]
      data.table::setkeyv(x = ModelData, cols = c(groupvars, "CalendarDateColumn"))
      if(QA_Results[run, xregs] == 0) {
        LeadsData <- RemixAutoML:::Post_Query_Helper('"chainladdertwogroupleadsdata.csv"')[['data']]
        data.table::setkeyv(x = ModelData, cols = c(groupvars, "CalendarDateColumn"))
      } else if(QA_Results[run, xregs] == 1) {
        LeadsData <- RemixAutoML:::Post_Query_Helper('"chainladdertwogroupleadsdataxregs1.csv"')[['data']]
        data.table::setkeyv(x = ModelData, cols = c(groupvars, "CalendarDateColumn"))
      } else if(QA_Results[run, xregs] == 2) {
        LeadsData <- RemixAutoML:::Post_Query_Helper('"chainladdertwogroupleadsdataxregs2.csv"')[['data']]
        data.table::setkeyv(x = ModelData, cols = c(groupvars, "CalendarDateColumn"))
      } else if(QA_Results[run, xregs] == 3) {
        LeadsData<- RemixAutoML:::Post_Query_Helper('"chainladdertwogroupleadsdataxregs3.csv"')[['data']]
        data.table::setkeyv(x = ModelData, cols = c(groupvars, "CalendarDateColumn"))
      }
    } else if(QA_Results[run, Group] == 3) {
      groupvars <- c("MarketingSegments","MarketingSegments2","MarketingSegments3")
      ModelData <- RemixAutoML:::Post_Query_Helper('"chainladderthreegroupmodeldata.csv"')[['data']]
      data.table::setkeyv(x = ModelData, cols = c(groupvars, "CalendarDateColumn"))
      if(QA_Results[run, xregs] == 0) {
        LeadsData <- RemixAutoML:::Post_Query_Helper('"chainladderthreegroupleadsdata.csv"')[['data']]
        data.table::setkeyv(x = ModelData, cols = c(groupvars, "CalendarDateColumn"))
      } else if(QA_Results[run, xregs] == 1) {
        LeadsData <- RemixAutoML:::Post_Query_Helper('"chainladderthreegroupleadsdataxregs1.csv"')[['data']]
        data.table::setkeyv(x = ModelData, cols = c(groupvars, "CalendarDateColumn"))
      } else if(QA_Results[run, xregs] == 2) {
        LeadsData <- RemixAutoML:::Post_Query_Helper('"chainladderthreegroupleadsdataxregs2.csv"')[['data']]
        data.table::setkeyv(x = ModelData, cols = c(groupvars, "CalendarDateColumn"))
      } else if(QA_Results[run, xregs] == 3) {
        LeadsData <- RemixAutoML:::Post_Query_Helper('"chainladderthreegroupleadsdataxregs3.csv"')[['data']]
        data.table::setkeyv(x = ModelData, cols = c(groupvars, "CalendarDateColumn"))
      }
    }

    # Shrink Forecast Periods
    LeadsData <- LeadsData[CalendarDateColumn < '2020-01-05']

    # Start Timer
    Start <- Sys.time()

    # Scoring
    Test <- tryCatch({RemixAutoML::AutoLightGBMFunnelCARMAScoring(
      TrainData = ModelData,
      ForwardLookingData = LeadsData,
      TrainEndDate = ModelData[, max(CalendarDateColumn)],
      ForecastEndDate = LeadsData[, max(CalendarDateColumn)],
      TrainOutput = TestModel$ModelOutput,
      ArgsList = TestModel$ArgsList,
      ModelPath = NULL,
      MaxCohortPeriod = 15,
      DebugMode = TRUE)}, error = function(x) NULL)

    # Timer
    End <- Sys.time()
    QA_Results[run, RunTimeScore := as.numeric(difftime(time1 = End, Start))]

  } else {
    Test <- NULL
  }

  # Outcome
  if(!is.null(Test)) QA_Results[run, Forecast := "Success"]
  rm(TestModel, Test)
  RemixAutoML:::Post_Append_Helper(QA_Results,'AutoLightGBMFunnel_QA')
  Sys.sleep(5)
}

# Main Args ----

# run = 1
#
# library(RemixAutoML)
# library(data.table)
# library(lubridate)
#
# # Need to load NON-EXPORTED FUNCTIONS - I don't use RemixAutoML:::... inside exported functions.
# #  I just reference the function name. Thus, I need them predefined as well
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/FeatureEngineering_CalendarTypes.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/FeatureEngineering_CrossRowOperations.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/FeatureEngineering_NumericTypes.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/CARMA-HelperFunctions.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ReinforcementLearningFunctions.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/MiscFunctions.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ModelEvaluationPlots.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/CatBoostHelpers.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/LightGBMHelpers.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ModelMetrics.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/AutoCatBoostFunnel.R"))
#
#
#
# # Get data
# if(QA_Results[run, Group] == 0) {
#   groupvars <- NULL
#   ModelData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/ChainLadder-NoGroup-ModelData.csv"), key = c(groupvars, "CalendarDateColumn"))
#   if(QA_Results[run, xregs] == 0) {
#     LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/ChainLadder-NoGroup-LeadsData.csv"), key = c(groupvars, "CalendarDateColumn"))
#   } else if(QA_Results[run, xregs] == 1) {
#     LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/ChainLadder-NoGroup-LeadsData-XREGS1.csv"), key = c(groupvars, "CalendarDateColumn"))
#   } else if(QA_Results[run, xregs] == 2) {
#     LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/ChainLadder-NoGroup-LeadsData-XREGS2.csv"), key = c(groupvars, "CalendarDateColumn"))
#   } else if(QA_Results[run, xregs] == 3) {
#     LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/ChainLadder-NoGroup-LeadsData-XREGS3.csv"), key = c(groupvars, "CalendarDateColumn"))
#   }
# } else if(QA_Results[run, Group] == 1) {
#   groupvars <- "MarketingSegments"
#   ModelData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/ChainLadder-OneGroup-ModelData.csv"), key = c(groupvars, "CalendarDateColumn"))
#   if(QA_Results[run, xregs] == 0) {
#     LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/ChainLadder-OneGroup-LeadsData.csv"), key = c(groupvars, "CalendarDateColumn"))
#   } else if(QA_Results[run, xregs] == 1) {
#     LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/ChainLadder-OneGroup-LeadsData-XREGS1.csv"), key = c(groupvars, "CalendarDateColumn"))
#   } else if(QA_Results[run, xregs] == 2) {
#     LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/ChainLadder-OneGroup-LeadsData-XREGS2.csv"), key = c(groupvars, "CalendarDateColumn"))
#   } else if(QA_Results[run, xregs] == 3) {
#     LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/ChainLadder-OneGroup-LeadsData-XREGS3.csv"), key = c(groupvars, "CalendarDateColumn"))
#   }
# } else if(QA_Results[run, Group] == 2) {
#   groupvars <- c("MarketingSegments","MarketingSegments2")
#   ModelData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/ChainLadder-TwoGroup-ModelData.csv"), key = c(groupvars, "CalendarDateColumn"))
#   if(QA_Results[run, xregs] == 0) {
#     LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/ChainLadder-TwoGroup-LeadsData.csv"), key = c(groupvars, "CalendarDateColumn"))
#   } else if(QA_Results[run, xregs] == 1) {
#     LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/ChainLadder-TwoGroup-LeadsData-XREGS1.csv"), key = c(groupvars, "CalendarDateColumn"))
#   } else if(QA_Results[run, xregs] == 2) {
#     LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/ChainLadder-TwoGroup-LeadsData-XREGS2.csv"), key = c(groupvars, "CalendarDateColumn"))
#   } else if(QA_Results[run, xregs] == 3) {
#     LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/ChainLadder-TwoGroup-LeadsData-XREGS3.csv"), key = c(groupvars, "CalendarDateColumn"))
#   }
# } else if(QA_Results[run, Group] == 3) {
#   groupvars <- c("MarketingSegments","MarketingSegments2","MarketingSegments3")
#   ModelData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/ChainLadder-ThreeGroup-ModelData.csv"), key = c(groupvars, "CalendarDateColumn"))
#   if(QA_Results[run, xregs] == 0) {
#     LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/ChainLadder-ThreeGroup-LeadsData.csv"), key = c(groupvars, "CalendarDateColumn"))
#   } else if(QA_Results[run, xregs] == 1) {
#     LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/ChainLadder-ThreeGroup-LeadsData-XREGS1.csv"), key = c(groupvars, "CalendarDateColumn"))
#   } else if(QA_Results[run, xregs] == 2) {
#     LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/ChainLadder-ThreeGroup-LeadsData-XREGS2.csv"), key = c(groupvars, "CalendarDateColumn"))
#   } else if(QA_Results[run, xregs] == 3) {
#     LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/ChainLadder-ThreeGroup-LeadsData-XREGS3.csv"), key = c(groupvars, "CalendarDateColumn"))
#   }
# }
#
# # Scoring args
# TrainData = ModelData
# ForwardLookingData = LeadsData
# TrainEndDate = ModelData[, max(CalendarDateColumn)]
# ForecastEndDate = LeadsData[, max(CalendarDateColumn)]
# TrainOutput = TestModel$ModelOutput
# ArgsList = TestModel$ArgsList
# ModelPath = NULL
# MaxCohortPeriod = 15
# DebugMode = TRUE
#
# Join data
# keep <- names(LeadsData)
# keep <- keep[!keep %in% c(groupvars, "CalendarDateColumn")]
# ModelData[LeadsData, paste0(keep) := mget(paste0("i.", keep))]
#
# # Set working directory
# setwd("C:/Users/Bizon/Documents/GitHub")
#
# # Function Args
# data = ModelData
# GroupVariables = groupvars
# PartitionRatios = c(0.70,0.20,0.10)
# BaseFunnelMeasure = keep
# ConversionMeasure = "Appointments"
# ConversionRateMeasure = NULL
# CohortPeriodsVariable = "CohortDays"
# CalendarDate = "CalendarDateColumn"
# CohortDate = "CohortDateColumn"
# TruncateDate = NULL
# WeightsColumnName = NULL
# TimeUnit = "days"
# TransformTargetVariable = TRUE
# TransformMethods = c("Asinh","Asin","Log","LogPlus1","Sqrt","Logit")
# AnomalyDetection = list(tstat_high = 3, tstat_low = -2)
# # MetaData Arguments---
# Jobs = c("eval","train")
# SaveModelObjects = TRUE
# ModelID = "ModelTest"
# ModelPath = getwd()
# MetaDataPath = NULL
# TaskType = "GPU"
# NumGPUs = 1
# NThreads = max(1L, parallel::detectCores() - 2L)
# EvaluationMetric = "RMSE"
# LossFunction = "RMSE"
# NumOfParDepPlots = 1L
# MetricPeriods = 50L
# DebugMode = TRUE
# # Feature Engineering Arguments---
# ImputeRollStats = -0.001
# CalendarTimeGroups = c("days","weeks","months")
# CohortTimeGroups = c("days", "weeks")
# CalendarVariables = c("wday","mday","yday","week","month","quarter","year")
# HolidayGroups = c("USPublicHolidays","EasterGroup","ChristmasGroup","OtherEcclesticalFeasts")
# HolidayLookback = NULL
# CohortHolidayLags = c(1L,2L,7L)
# CohortHolidayMovingAverages = c(3L,7L)
# CalendarHolidayLags = c(1L,2L,7L)
# CalendarHolidayMovingAverages = c(3L,7L)
# CalendarLags = list("day" = c(1L,2L,7L,35L,42L), "week" = c(5L,6L,10L,12L,25L,26L))
# CalendarMovingAverages = list("day" = c(7L,14L,35L,42L), "week" = c(5L,6L,10L,12L,20L,24L), "month" = c(6L,12L))
# CalendarStandardDeviations = NULL
# CalendarSkews = NULL
# CalendarKurts = NULL
# CalendarQuantiles = NULL
# CalendarQuantilesSelected = "q50"
# CohortLags = list("day" = c(1L,2L,7L,35L,42L), "week" = c(5L,6L))
# CohortMovingAverages = list("day" = c(7L,14L,35L,42L), "week" = c(5L,6L), "month" = c(1L,2L))
# CohortStandardDeviations = NULL
# CohortSkews = NULL
# CohortKurts = NULL
# CohortQuantiles = NULL
# CohortQuantilesSelected = "q50"
# PassInGrid = NULL
# GridTune = FALSE
# BaselineComparison = "default"
# MaxModelsInGrid = 25L
# MaxRunMinutes = 180L
# MaxRunsWithoutNewWinner = 10L
# LossFunction = 'regression'
# EvalMetric = 'mae'
# MetricPeriods = 50L
# NumOfParDepPlots = 1L
# Device_Type = 'CPU'
# Input_Model = NULL
# Task = 'train'
# Boosting = 'gbdt'
# LinearTree = FALSE
# Trees = 1000
# ETA = 0.10
# Num_Leaves = 31
# Deterministic = TRUE
# Force_Col_Wise = FALSE
# Force_Row_Wise = FALSE
# Max_Depth = 6
# Min_Data_In_Leaf = 20
# Min_Sum_Hessian_In_Leaf = 0.001
# Bagging_Freq = 1.0
# Bagging_Fraction = 1.0
# Feature_Fraction = 1.0
# Feature_Fraction_Bynode = 1.0
# Lambda_L1 = 0.0
# Lambda_L2 = 0.0
# Extra_Trees = FALSE
# Early_Stopping_Round = 10
# First_Metric_Only = TRUE
# Max_Delta_Step = 0.0
# Linear_Lambda = 0.0
# Min_Gain_To_Split = 0
# Drop_Rate_Dart = 0.10
# Max_Drop_Dart = 50
# Skip_Drop_Dart = 0.50
# Uniform_Drop_Dart = FALSE
# Top_Rate_Goss = FALSE
# Other_Rate_Goss = FALSE
# Monotone_Constraints = NULL
# Monotone_Constraints_method = 'advanced'
# Monotone_Penalty = 0.0
# Forcedsplits_Filename = NULL
# Refit_Decay_Rate = 0.90
# Path_Smooth = 0.0
# Max_Bin = 255
# Min_Data_In_Bin = 3
# Data_Random_Seed = 1
# Is_Enable_Sparse = TRUE
# Enable_Bundle = TRUE
# Use_Missing = TRUE
# Zero_As_Missing = FALSE
# Two_Round = FALSE
# Convert_Model = NULL
# Convert_Model_Language = 'cpp'
# Boost_From_Average = TRUE
# Alpha = 0.90
# Fair_C = 1.0
# Poisson_Max_Delta_Step = 0.70
# Tweedie_Variance_Power = 1.5
# Lambdarank_Truncation_Level = 30
# Is_Provide_Training_Metric = TRUE
# Eval_At = c(1,2,3,4,5)
# Num_Machines = 1
# Gpu_Platform_Id = -1
# Gpu_Device_Id = -1
# Gpu_Use_Dp = TRUE
# Num_Gpu = 1
# EncodingMethod = 'credibility'
# proc = "eval"
# GridEvalMetric = 'mae'
# Monotone_Constraints_Method = 'advanced'
# OutputSelection = 'EvalMetrics'
#
# # AutoLagRollScoring
# data                 = temp
# DateColumn           = ArgsList$CalendarDate
# Targets              = ArgsList$BaseFunnelMeasure[bfm]
# HierarchyGroups      = NULL
# IndependentGroups    = NULL
# TimeGroups           = ArgsList$CalendarTimeGroups
# TimeUnit             = ArgsList$TimeUnit
# TimeUnitAgg          = ArgsList$TimeUnit
# RowNumsID            = "ScoreRecords"
# RowNumsKeep          = 1
# TimeBetween          = NULL
# RollOnLag1           = TRUE
# Type                 = "Lag"
# SimpleImpute         = FALSE
# Lags                 = ArgsList$CalendarLags
# MA_RollWindows       = ArgsList$CalendarMovingAverages
# SD_RollWindows       = ArgsList$CalendarStandardDeviations
# Skew_RollWindows     = ArgsList$CalendarSkews
# Kurt_RollWindows     = ArgsList$CalendarKurts
# Quantile_RollWindows = ArgsList$CalendarQuantiles
# Quantiles_Selected   = ArgsList$CalendarQuantilesSelected
# Debug                = TRUE
#
# # Partial DT Feature Engineering
# data
# lags            = if(is.list(Lags))                 Lags[[timeaggs]]                 else Lags
# periods         = if(is.list(MA_RollWindows))       MA_RollWindows[[timeaggs]]       else MA_RollWindows
# SDperiods       = if(is.list(SD_RollWindows))       SD_RollWindows[[timeaggs]]       else SD_RollWindows
# Skewperiods     = if(is.list(Skew_RollWindows))     Skew_RollWindows[[timeaggs]]     else Skew_RollWindows
# Kurtperiods     = if(is.list(Kurt_RollWindows))     Kurt_RollWindows[[timeaggs]]     else Kurt_RollWindows
# Quantileperiods = if(is.list(Quantile_RollWindows)) Quantile_RollWindows[[timeaggs]] else Quantile_RollWindows
# statsFUNs = RollFunctions
# targets = Targets
# groupingVars = NULL
# sortDateName = eval(DateColumn)
# timeDiffTarget = NULL
# timeAgg = timeaggs
# WindowingLag = RollOnLag1
# Type = Type
# Timer = FALSE
# SimpleImpute = SimpleImpute
# AscRowByGroup = RowNumsID
# RecordsKeep = RowNumsKeep
# AscRowRemove = TRUE

# AutoLightGBMRegression()

# Metadata arguments
# ModelID = paste0(ModelID,"_", proc, "_")
# model_path = ModelPath
# metadata_path = MetaDataPath
# SaveModelObjects = FALSE
# ReturnModelObjects = TRUE
# NThreads = NThreads
# WeightsColumnName = WeightsColumnName
# OutputSelection = OutputSelection
# DebugMode = DebugMode
# SaveInfoToPDF = FALSE
# ReturnFactorLevels = TRUE
# EncodingMethod = EncodingMethod
# data = if(proc %chin% c("eval", "evaluate")) TrainData else data
# TrainOnFull = if(proc %chin% c("eval", "evaluate")) FALSE else TRUE
# ValidationData = if(proc %chin% c("eval", "evaluate")) ValidationData else NULL
# TestData = if(proc %chin% c("eval", "evaluate")) TestData else NULL
# TargetColumnName = ConversionRateMeasure
# FeatureColNames = Features
# PrimaryDateColumn = CohortDate
# IDcols = idcols
# TransformNumericColumns = NULL
# Methods = TransformMethods
# NumOfParDepPlots = NumOfParDepPlots
# PassInGrid = PassInGrid
# GridTune = GridTune
# MaxModelsInGrid = MaxModelsInGrid
# MaxRunsWithoutNewWinner = MaxRunsWithoutNewWinner
# MaxRunMinutes = MaxRunMinutes
# BaselineComparison = BaselineComparison
# grid_eval_metric = GridEvalMetric
# device_type = tolower(Device_Type)
# objective = LossFunction
# metric = EvalMetric
# input_model = Input_Model
# task = Task
# boosting = Boosting
# LinearTree = LinearTree
# Trees = NTrees
# eta = ETA
# num_leaves = Num_Leaves
# deterministic = Deterministic
# force_col_wise = Force_Col_Wise
# force_row_wise = Force_Row_Wise
# max_depth = Max_Depth
# min_data_in_leaf = Min_Data_In_Leaf
# min_sum_hessian_in_leaf = Min_Sum_Hessian_In_Leaf
# bagging_freq = Bagging_Freq
# bagging_fraction = Bagging_Fraction
# feature_fraction = Feature_Fraction
# feature_fraction_bynode = Feature_Fraction_Bynode
# lambda_l1 = Lambda_L1
# lambda_l2 = Lambda_L2
# extra_trees = Extra_Trees
# early_stopping_round = Early_Stopping_Round
# first_metric_only = First_Metric_Only
# max_delta_step = Max_Delta_Step
# linear_lambda = Linear_Lambda
# min_gain_to_split = Min_Gain_To_Split
# drop_rate_dart = Drop_Rate_Dart
# max_drop_dart = Max_Drop_Dart
# skip_drop_dart = Skip_Drop_Dart
# uniform_drop_dart = Uniform_Drop_Dart
# top_rate_goss = Top_Rate_Goss
# other_rate_goss = Other_Rate_Goss
# monotone_constraints = Monotone_Constraints
# monotone_constraints_method = Monotone_Constraints_Method
# monotone_penalty = Monotone_Penalty
# forcedsplits_filename = Forcedsplits_Filename
# refit_decay_rate = Refit_Decay_Rate
# path_smooth = Path_Smooth
# max_bin = Max_Bin
# min_data_in_bin = Min_Data_In_Bin
# data_random_seed = Data_Random_Seed
# is_enable_sparse = Is_Enable_Sparse
# enable_bundle = Enable_Bundle
# use_missing = Use_Missing
# zero_as_missing = Zero_As_Missing
# two_round = Two_Round
# convert_model = Convert_Model
# convert_model_language = Convert_Model_Language
# boost_from_average = Boost_From_Average
# alpha = Alpha
# fair_c = Fair_C
# poisson_max_delta_step = Poisson_Max_Delta_Step
# tweedie_variance_power = Tweedie_Variance_Power
# lambdarank_truncation_level = Lambdarank_Truncation_Level
# is_provide_training_metric = TRUE
# eval_at = Eval_At
# num_machines = Num_Machines
# gpu_platform_id = Gpu_Platform_Id
# gpu_device_id = Gpu_Device_Id
# gpu_use_dp = Gpu_Use_Dp
# num_gpu = Num_Gpu
# Monotone_Constraints_method = 'advanced'
#
