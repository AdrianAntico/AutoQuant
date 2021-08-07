# Collection data.table
QA_Results <- data.table::CJ(
  Group = c(0,1,2,3),
  xregs = c(0,1,2,3),
  Trans = c(TRUE, FALSE),
  Training = "Failure",
  Forecast = "Failure")

# run = 25
for(run in seq_len(QA_Results[,.N])) {

  # Print run number
  for(lll in 1:100) print(paste0("Run Number: ", run))

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
  TestModel <- tryCatch({RemixAutoML::AutoXGBoostFunnelCARMA(

    # Data Arguments
    data = ModelData,
    GroupVariables = groupvars,
    BaseFunnelMeasure = keep,
    ConversionMeasure = "Appointments",
    ConversionRateMeasure = NULL,
    CohortPeriodsVariable = "CohortDays",
    WeightsColumnName = NULL,
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
    NThreads = parallel::detectCores(),

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
    GridEvalMetric = 'mae',

    # XGBoost arguments
    TreeMethod = 'hist',
    EvalMetric = 'MAE',
    LossFunction = 'reg:squarederror',
    Trees = 50L,
    LearningRate = 0.3,
    MaxDepth = 9L,
    MinChildWeight = 1.0,
    SubSample = 1.0,
    ColSampleByTree = 1.0)}, error = function(x) NULL)

  # Outcome
  if(!is.null(TestModel)) QA_Results[run, Training := "Success"]
  data.table::fwrite(QA_Results, file = "C:/Users/Bizon/Documents/GitHub/QA_Code/QA_CSV/AutoXGBoostFunnel_QA.csv")

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
    Test <- tryCatch({RemixAutoML::AutoXGBoostFunnelCARMAScoring(
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
  data.table::fwrite(QA_Results, file = "C:/Users/Bizon/Documents/GitHub/QA_Code/QA_CSV/AutoXGBoostFunnel_QA.csv")
  Sys.sleep(5)
}

# Main Args ----

run = 25

library(RemixAutoML)
library(data.table)
library(lubridate)

source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/FeatureEngineering_CalendarTypes.R"))
source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/FeatureEngineering_CrossRowOperations.R"))
source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/CARMA-HelperFunctions.R"))
source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ReinforcementLearningFunctions.R"))
source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/MiscFunctions.R"))
source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/XGBoostHelpers.R"))
source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/FeatureEngineering_CharacterTypes.R"))
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
OutputSelection = c("Importances", "EvalPlots", "EvalMetrics", "Score_TrainData")
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
EncodingMethod = "credibility"
MetricPeriods = 50L
GridEvalMetric = 'mae'
TreeMethod = 'hist'
EvalMetric = 'MAE'
LossFunction = 'reg:squarederror'
Trees = 1000L
LearningRate = 0.3
MaxDepth = 9L
MinChildWeight = 1.0
SubSample = 1.0
ColSampleByTree = 1.0
proc = "eval"

ModelID = paste0(ModelID,"_", proc, "_")
model_path = ModelPath
metadata_path = MetaDataPath
SaveModelObjects = FALSE
ReturnModelObjects = TRUE
NThreads = NThreads
WeightsColumnName = WeightsColumnName
OutputSelection = OutputSelection
DebugMode = DebugMode
SaveInfoToPDF = FALSE
ReturnFactorLevels = TRUE
EncodingMethod = EncodingMethod

# Data arguments
data = if(proc %chin% c("eval", "evaluate")) TrainData else data
TrainOnFull = if(proc %chin% c("eval", "evaluate")) FALSE else TRUE
ValidationData = if(proc %chin% c("eval", "evaluate")) ValidationData else NULL
TestData = if(proc %chin% c("eval", "evaluate")) TestData else NULL
TargetColumnName = ConversionRateMeasure
FeatureColNames = Features
PrimaryDateColumn = CohortDate
IDcols = idcols
TransformNumericColumns = NULL
Methods = TransformMethods

# Model evaluation
NumOfParDepPlots = NumOfParDepPlots

# Grid tuning arguments
PassInGrid = PassInGrid
GridTune = GridTune
MaxModelsInGrid = MaxModelsInGrid
MaxRunsWithoutNewWinner = MaxRunsWithoutNewWinner
MaxRunMinutes = MaxRunMinutes
BaselineComparison = BaselineComparison
grid_eval_metric = GridEvalMetric

# XGBoost arguments
eval_metric = EvalMetric
LossFunction = LossFunction
TreeMethod = TreeMethod
Trees = NTrees
eta = LearningRate
max_depth = MaxDepth
min_child_weight = MinChildWeight
subsample = SubSample
colsample_bytree = ColSampleByTree

# AutoLagRollScoring ----
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

# Partial DT Feature Engineering ----
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

# Scoring XGBoost ----
# FactorLevelsList = TrainOutput$FactorLevelsList
# TargetType = "regression"
# ScoringData = temp
# FeatureColumnNames = Features
# IDcols = names(temp)[!names(temp) %chin% Features]
# EncodingMethod = ArgsList$EncodingMethod
# ReturnShapValues = FALSE
# ModelObject = TrainOutput$Model
# ModelPath = if(is.null(TrainOutput)) ArgsList$ModelPath else NULL
# ModelID = ArgsList$ModelID
# ReturnFeatures = FALSE
# TransformNumeric = FALSE
# BackTransNumeric = FALSE
# TargetColumnName = "Rate"
# TransformationObject = TestModel$TransformationResults
# TransID = NULL
# TransPath = NULL
# MDP_Impute = FALSE
# MDP_CharToFactor = FALSE
# MDP_RemoveDates = TRUE
# MDP_MissFactor = "0"
# MDP_MissNum = -1
