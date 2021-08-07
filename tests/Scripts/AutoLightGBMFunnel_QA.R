# Collection data.table
QA_Results <- data.table::CJ(
  Group = c(0,1,2,3),
  xregs = c(0,1,2,3),
  Trans = c(TRUE, FALSE),
  Training = "Failure",
  Forecast = "Failure")

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
    ModelData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-NoGroup-ModelData.csv"), key = c(groupvars, "CalendarDateColumn"))
    if(QA_Results[run, xregs] == 0) {
      LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-NoGroup-LeadsData.csv"), key = c(groupvars, "CalendarDateColumn"))
    } else if(QA_Results[run, xregs] == 1) {
      LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-NoGroup-LeadsData-XREGS1.csv"), key = c(groupvars, "CalendarDateColumn"))
    } else if(QA_Results[run, xregs] == 2) {
      LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-NoGroup-LeadsData-XREGS2.csv"), key = c(groupvars, "CalendarDateColumn"))
    } else if(QA_Results[run, xregs] == 3) {
      LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-NoGroup-LeadsData-XREGS3.csv"), key = c(groupvars, "CalendarDateColumn"))
    }
  } else if(QA_Results[run, Group] == 1) {
    groupvars <- "MarketingSegments"
    ModelData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-OneGroup-ModelData.csv"), key = c(groupvars, "CalendarDateColumn"))
    if(QA_Results[run, xregs] == 0) {
      LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-OneGroup-LeadsData.csv"), key = c(groupvars, "CalendarDateColumn"))
    } else if(QA_Results[run, xregs] == 1) {
      LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-OneGroup-LeadsData-XREGS1.csv"), key = c(groupvars, "CalendarDateColumn"))
    } else if(QA_Results[run, xregs] == 2) {
      LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-OneGroup-LeadsData-XREGS2.csv"), key = c(groupvars, "CalendarDateColumn"))
    } else if(QA_Results[run, xregs] == 3) {
      LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-OneGroup-LeadsData-XREGS3.csv"), key = c(groupvars, "CalendarDateColumn"))
    }
  } else if(QA_Results[run, Group] == 2) {
    groupvars <- c("MarketingSegments","MarketingSegments2")
    ModelData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-TwoGroup-ModelData.csv"), key = c(groupvars, "CalendarDateColumn"))
    if(QA_Results[run, xregs] == 0) {
      LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-TwoGroup-LeadsData.csv"), key = c(groupvars, "CalendarDateColumn"))
    } else if(QA_Results[run, xregs] == 1) {
      LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-TwoGroup-LeadsData-XREGS1.csv"), key = c(groupvars, "CalendarDateColumn"))
    } else if(QA_Results[run, xregs] == 2) {
      LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-TwoGroup-LeadsData-XREGS2.csv"), key = c(groupvars, "CalendarDateColumn"))
    } else if(QA_Results[run, xregs] == 3) {
      LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-TwoGroup-LeadsData-XREGS3.csv"), key = c(groupvars, "CalendarDateColumn"))
    }
  } else if(QA_Results[run, Group] == 3) {
    groupvars <- c("MarketingSegments","MarketingSegments2","MarketingSegments3")
    ModelData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-ThreeGroup-ModelData.csv"), key = c(groupvars, "CalendarDateColumn"))
    if(QA_Results[run, xregs] == 0) {
      LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-ThreeGroup-LeadsData.csv"), key = c(groupvars, "CalendarDateColumn"))
    } else if(QA_Results[run, xregs] == 1) {
      LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-ThreeGroup-LeadsData-XREGS1.csv"), key = c(groupvars, "CalendarDateColumn"))
    } else if(QA_Results[run, xregs] == 2) {
      LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-ThreeGroup-LeadsData-XREGS2.csv"), key = c(groupvars, "CalendarDateColumn"))
    } else if(QA_Results[run, xregs] == 3) {
      LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-ThreeGroup-LeadsData-XREGS3.csv"), key = c(groupvars, "CalendarDateColumn"))
    }
  }

  # Join data
  keep <- names(LeadsData)
  keep <- keep[!keep %in% c(groupvars, "CalendarDateColumn")]
  ModelData[LeadsData, paste0(keep) := mget(paste0("i.", keep))]

  # Set working directory
  setwd("C:/Users/Bizon/Documents/GitHub")

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

  # Outcome
  if(!is.null(TestModel)) QA_Results[run, Training := "Success"]
  data.table::fwrite(QA_Results, file = "C:/Users/Bizon/Documents/GitHub/QA_Code/QA_CSV/AutoLightGBMFunnel_QA.csv")

  # Forecast QA
  if(!is.null(TestModel)) {

    # Refresh data
    if(QA_Results[run, Group] == 0) {
      groupvars <- NULL
      ModelData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-NoGroup-ModelData.csv"), key = c(groupvars, "CalendarDateColumn"))
      if(QA_Results[run, xregs] == 0) {
        LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-NoGroup-LeadsData.csv"), key = c(groupvars, "CalendarDateColumn"))
      } else if(QA_Results[run, xregs] == 1) {
        LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-NoGroup-LeadsData-XREGS1.csv"), key = c(groupvars, "CalendarDateColumn"))
      } else if(QA_Results[run, xregs] == 2) {
        LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-NoGroup-LeadsData-XREGS2.csv"), key = c(groupvars, "CalendarDateColumn"))
      } else if(QA_Results[run, xregs] == 3) {
        LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-NoGroup-LeadsData-XREGS3.csv"), key = c(groupvars, "CalendarDateColumn"))
      }
    } else if(QA_Results[run, Group] == 1) {
      groupvars <- "MarketingSegments"
      ModelData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-OneGroup-ModelData.csv"), key = c(groupvars, "CalendarDateColumn"))
      if(QA_Results[run, xregs] == 0) {
        LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-OneGroup-LeadsData.csv"), key = c(groupvars, "CalendarDateColumn"))
      } else if(QA_Results[run, xregs] == 1) {
        LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-OneGroup-LeadsData-XREGS1.csv"), key = c(groupvars, "CalendarDateColumn"))
      } else if(QA_Results[run, xregs] == 2) {
        LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-OneGroup-LeadsData-XREGS2.csv"), key = c(groupvars, "CalendarDateColumn"))
      } else if(QA_Results[run, xregs] == 3) {
        LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-OneGroup-LeadsData-XREGS3.csv"), key = c(groupvars, "CalendarDateColumn"))
      }
    } else if(QA_Results[run, Group] == 2) {
      groupvars <- c("MarketingSegments","MarketingSegments2")
      ModelData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-TwoGroup-ModelData.csv"), key = c(groupvars, "CalendarDateColumn"))
      if(QA_Results[run, xregs] == 0) {
        LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-TwoGroup-LeadsData.csv"), key = c(groupvars, "CalendarDateColumn"))
      } else if(QA_Results[run, xregs] == 1) {
        LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-TwoGroup-LeadsData-XREGS1.csv"), key = c(groupvars, "CalendarDateColumn"))
      } else if(QA_Results[run, xregs] == 2) {
        LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-TwoGroup-LeadsData-XREGS2.csv"), key = c(groupvars, "CalendarDateColumn"))
      } else if(QA_Results[run, xregs] == 3) {
        LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-TwoGroup-LeadsData-XREGS3.csv"), key = c(groupvars, "CalendarDateColumn"))
      }
    } else if(QA_Results[run, Group] == 3) {
      groupvars <- c("MarketingSegments","MarketingSegments2","MarketingSegments3")
      ModelData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-ThreeGroup-ModelData.csv"), key = c(groupvars, "CalendarDateColumn"))
      if(QA_Results[run, xregs] == 0) {
        LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-ThreeGroup-LeadsData.csv"), key = c(groupvars, "CalendarDateColumn"))
      } else if(QA_Results[run, xregs] == 1) {
        LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-ThreeGroup-LeadsData-XREGS1.csv"), key = c(groupvars, "CalendarDateColumn"))
      } else if(QA_Results[run, xregs] == 2) {
        LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-ThreeGroup-LeadsData-XREGS2.csv"), key = c(groupvars, "CalendarDateColumn"))
      } else if(QA_Results[run, xregs] == 3) {
        LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-ThreeGroup-LeadsData-XREGS3.csv"), key = c(groupvars, "CalendarDateColumn"))
      }
    }

    # Shrink Forecast Periods
    LeadsData <- LeadsData[CalendarDateColumn < '2020-01-05']

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
  } else {
    Test <- NULL
  }

  # Outcome
  if(!is.null(Test)) QA_Results[run, Forecast := "Success"]
  rm(TestModel, Test)
  data.table::fwrite(QA_Results, file = "C:/Users/Bizon/Documents/GitHub/QA_Code/QA_CSV/AutoLightGBMFunnel_QA.csv")
  Sys.sleep(5)
}

# Main Args ----

run = 32

library(RemixAutoML)
library(data.table)
library(lubridate)

source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/FeatureEngineering_CalendarTypes.R"))
source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/FeatureEngineering_CrossRowOperations.R"))
source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/CARMA-HelperFunctions.R"))
source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ReinforcementLearningFunctions.R"))
source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/MiscFunctions.R"))
source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ModelEvaluationPlots.R"))
source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/CatBoostHelpers.R"))
source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/LightGBMHelpers.R"))
source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ModelMetrics.R"))
source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/AutoCatBoostFunnel.R"))



# Get data
if(QA_Results[run, Group] == 0) {
  groupvars <- NULL
  ModelData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-NoGroup-ModelData.csv"), key = c(groupvars, "CalendarDateColumn"))
  if(QA_Results[run, xregs] == 0) {
    LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-NoGroup-LeadsData.csv"), key = c(groupvars, "CalendarDateColumn"))
  } else if(QA_Results[run, xregs] == 1) {
    LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-NoGroup-LeadsData-XREGS1.csv"), key = c(groupvars, "CalendarDateColumn"))
  } else if(QA_Results[run, xregs] == 2) {
    LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-NoGroup-LeadsData-XREGS2.csv"), key = c(groupvars, "CalendarDateColumn"))
  } else if(QA_Results[run, xregs] == 3) {
    LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-NoGroup-LeadsData-XREGS3.csv"), key = c(groupvars, "CalendarDateColumn"))
  }
} else if(QA_Results[run, Group] == 1) {
  groupvars <- "MarketingSegments"
  ModelData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-OneGroup-ModelData.csv"), key = c(groupvars, "CalendarDateColumn"))
  if(QA_Results[run, xregs] == 0) {
    LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-OneGroup-LeadsData.csv"), key = c(groupvars, "CalendarDateColumn"))
  } else if(QA_Results[run, xregs] == 1) {
    LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-OneGroup-LeadsData-XREGS1.csv"), key = c(groupvars, "CalendarDateColumn"))
  } else if(QA_Results[run, xregs] == 2) {
    LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-OneGroup-LeadsData-XREGS2.csv"), key = c(groupvars, "CalendarDateColumn"))
  } else if(QA_Results[run, xregs] == 3) {
    LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-OneGroup-LeadsData-XREGS3.csv"), key = c(groupvars, "CalendarDateColumn"))
  }
} else if(QA_Results[run, Group] == 2) {
  groupvars <- c("MarketingSegments","MarketingSegments2")
  ModelData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-TwoGroup-ModelData.csv"), key = c(groupvars, "CalendarDateColumn"))
  if(QA_Results[run, xregs] == 0) {
    LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-TwoGroup-LeadsData.csv"), key = c(groupvars, "CalendarDateColumn"))
  } else if(QA_Results[run, xregs] == 1) {
    LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-TwoGroup-LeadsData-XREGS1.csv"), key = c(groupvars, "CalendarDateColumn"))
  } else if(QA_Results[run, xregs] == 2) {
    LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-TwoGroup-LeadsData-XREGS2.csv"), key = c(groupvars, "CalendarDateColumn"))
  } else if(QA_Results[run, xregs] == 3) {
    LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-TwoGroup-LeadsData-XREGS3.csv"), key = c(groupvars, "CalendarDateColumn"))
  }
} else if(QA_Results[run, Group] == 3) {
  groupvars <- c("MarketingSegments","MarketingSegments2","MarketingSegments3")
  ModelData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-ThreeGroup-ModelData.csv"), key = c(groupvars, "CalendarDateColumn"))
  if(QA_Results[run, xregs] == 0) {
    LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-ThreeGroup-LeadsData.csv"), key = c(groupvars, "CalendarDateColumn"))
  } else if(QA_Results[run, xregs] == 1) {
    LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-ThreeGroup-LeadsData-XREGS1.csv"), key = c(groupvars, "CalendarDateColumn"))
  } else if(QA_Results[run, xregs] == 2) {
    LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-ThreeGroup-LeadsData-XREGS2.csv"), key = c(groupvars, "CalendarDateColumn"))
  } else if(QA_Results[run, xregs] == 3) {
    LeadsData <- data.table::fread(file = file.path("C:/Users/Bizon/Documents/GitHub/QA_DataSets/ChainLadder-ThreeGroup-LeadsData-XREGS3.csv"), key = c(groupvars, "CalendarDateColumn"))
  }
}

# Scoring args
TrainData = ModelData
ForwardLookingData = LeadsData
TrainEndDate = ModelData[, max(CalendarDateColumn)]
ForecastEndDate = LeadsData[, max(CalendarDateColumn)]
TrainOutput = TestModel$ModelOutput
ArgsList = TestModel$ArgsList
ModelPath = NULL
MaxCohortPeriod = 15
DebugMode = TRUE

# Join data
keep <- names(LeadsData)
keep <- keep[!keep %in% c(groupvars, "CalendarDateColumn")]
ModelData[LeadsData, paste0(keep) := mget(paste0("i.", keep))]

# Set working directory
setwd("C:/Users/Bizon/Documents/GitHub")

# Function Args
data = ModelData
GroupVariables = groupvars
PartitionRatios = c(0.70,0.20,0.10)
BaseFunnelMeasure = keep
ConversionMeasure = "Appointments"
ConversionRateMeasure = NULL
CohortPeriodsVariable = "CohortDays"
CalendarDate = "CalendarDateColumn"
CohortDate = "CohortDateColumn"
TruncateDate = NULL
WeightsColumnName = NULL
TimeUnit = "days"
TransformTargetVariable = TRUE
TransformMethods = c("Asinh","Asin","Log","LogPlus1","Sqrt","Logit")
AnomalyDetection = list(tstat_high = 3, tstat_low = -2)
# MetaData Arguments---
Jobs = c("eval","train")
SaveModelObjects = TRUE
ModelID = "ModelTest"
ModelPath = getwd()
MetaDataPath = NULL
TaskType = "GPU"
NumGPUs = 1
NThreads = max(1L, parallel::detectCores() - 2L)
EvaluationMetric = "RMSE"
LossFunction = "RMSE"
NumOfParDepPlots = 1L
MetricPeriods = 50L
DebugMode = TRUE
# Feature Engineering Arguments---
ImputeRollStats = -0.001
CalendarTimeGroups = c("days","weeks","months")
CohortTimeGroups = c("days", "weeks")
CalendarVariables = c("wday","mday","yday","week","month","quarter","year")
HolidayGroups = c("USPublicHolidays","EasterGroup","ChristmasGroup","OtherEcclesticalFeasts")
HolidayLookback = NULL
CohortHolidayLags = c(1L,2L,7L)
CohortHolidayMovingAverages = c(3L,7L)
CalendarHolidayLags = c(1L,2L,7L)
CalendarHolidayMovingAverages = c(3L,7L)
CalendarLags = list("day" = c(1L,2L,7L,35L,42L), "week" = c(5L,6L,10L,12L,25L,26L))
CalendarMovingAverages = list("day" = c(7L,14L,35L,42L), "week" = c(5L,6L,10L,12L,20L,24L), "month" = c(6L,12L))
CalendarStandardDeviations = NULL
CalendarSkews = NULL
CalendarKurts = NULL
CalendarQuantiles = NULL
CalendarQuantilesSelected = "q50"
CohortLags = list("day" = c(1L,2L,7L,35L,42L), "week" = c(5L,6L))
CohortMovingAverages = list("day" = c(7L,14L,35L,42L), "week" = c(5L,6L), "month" = c(1L,2L))
CohortStandardDeviations = NULL
CohortSkews = NULL
CohortKurts = NULL
CohortQuantiles = NULL
CohortQuantilesSelected = "q50"
PassInGrid = NULL
GridTune = FALSE
BaselineComparison = "default"
MaxModelsInGrid = 25L
MaxRunMinutes = 180L
MaxRunsWithoutNewWinner = 10L
LossFunction = 'regression'
EvalMetric = 'mae'
MetricPeriods = 50L
NumOfParDepPlots = 1L
Device_Type = 'CPU'
Input_Model = NULL
Task = 'train'
Boosting = 'gbdt'
LinearTree = FALSE
Trees = 1000
ETA = 0.10
Num_Leaves = 31
Deterministic = TRUE
Force_Col_Wise = FALSE
Force_Row_Wise = FALSE
Max_Depth = 6
Min_Data_In_Leaf = 20
Min_Sum_Hessian_In_Leaf = 0.001
Bagging_Freq = 1.0
Bagging_Fraction = 1.0
Feature_Fraction = 1.0
Feature_Fraction_Bynode = 1.0
Lambda_L1 = 0.0
Lambda_L2 = 0.0
Extra_Trees = FALSE
Early_Stopping_Round = 10
First_Metric_Only = TRUE
Max_Delta_Step = 0.0
Linear_Lambda = 0.0
Min_Gain_To_Split = 0
Drop_Rate_Dart = 0.10
Max_Drop_Dart = 50
Skip_Drop_Dart = 0.50
Uniform_Drop_Dart = FALSE
Top_Rate_Goss = FALSE
Other_Rate_Goss = FALSE
Monotone_Constraints = NULL
Monotone_Constraints_method = 'advanced'
Monotone_Penalty = 0.0
Forcedsplits_Filename = NULL
Refit_Decay_Rate = 0.90
Path_Smooth = 0.0
Max_Bin = 255
Min_Data_In_Bin = 3
Data_Random_Seed = 1
Is_Enable_Sparse = TRUE
Enable_Bundle = TRUE
Use_Missing = TRUE
Zero_As_Missing = FALSE
Two_Round = FALSE
Convert_Model = NULL
Convert_Model_Language = 'cpp'
Boost_From_Average = TRUE
Alpha = 0.90
Fair_C = 1.0
Poisson_Max_Delta_Step = 0.70
Tweedie_Variance_Power = 1.5
Lambdarank_Truncation_Level = 30
Is_Provide_Training_Metric = TRUE
Eval_At = c(1,2,3,4,5)
Num_Machines = 1
Gpu_Platform_Id = -1
Gpu_Device_Id = -1
Gpu_Use_Dp = TRUE
Num_Gpu = 1
proc = "eval"

# AutoLagRollScoring
data                 = temp
DateColumn           = ArgsList$CalendarDate
Targets              = ArgsList$BaseFunnelMeasure[bfm]
HierarchyGroups      = NULL
IndependentGroups    = NULL
TimeGroups           = ArgsList$CalendarTimeGroups
TimeUnit             = ArgsList$TimeUnit
TimeUnitAgg          = ArgsList$TimeUnit
RowNumsID            = "ScoreRecords"
RowNumsKeep          = 1
TimeBetween          = NULL
RollOnLag1           = TRUE
Type                 = "Lag"
SimpleImpute         = FALSE
Lags                 = ArgsList$CalendarLags
MA_RollWindows       = ArgsList$CalendarMovingAverages
SD_RollWindows       = ArgsList$CalendarStandardDeviations
Skew_RollWindows     = ArgsList$CalendarSkews
Kurt_RollWindows     = ArgsList$CalendarKurts
Quantile_RollWindows = ArgsList$CalendarQuantiles
Quantiles_Selected   = ArgsList$CalendarQuantilesSelected
Debug                = TRUE

# Partial DT Feature Engineering
data
lags            = if(is.list(Lags))                 Lags[[timeaggs]]                 else Lags
periods         = if(is.list(MA_RollWindows))       MA_RollWindows[[timeaggs]]       else MA_RollWindows
SDperiods       = if(is.list(SD_RollWindows))       SD_RollWindows[[timeaggs]]       else SD_RollWindows
Skewperiods     = if(is.list(Skew_RollWindows))     Skew_RollWindows[[timeaggs]]     else Skew_RollWindows
Kurtperiods     = if(is.list(Kurt_RollWindows))     Kurt_RollWindows[[timeaggs]]     else Kurt_RollWindows
Quantileperiods = if(is.list(Quantile_RollWindows)) Quantile_RollWindows[[timeaggs]] else Quantile_RollWindows
statsFUNs = RollFunctions
targets = Targets
groupingVars = NULL
sortDateName = eval(DateColumn)
timeDiffTarget = NULL
timeAgg = timeaggs
WindowingLag = RollOnLag1
Type = Type
Timer = FALSE
SimpleImpute = SimpleImpute
AscRowByGroup = RowNumsID
RecordsKeep = RowNumsKeep
AscRowRemove = TRUE
