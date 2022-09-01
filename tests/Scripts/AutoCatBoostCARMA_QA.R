# Collection data.table
QA_Results <- data.table::CJ(
  Group = c(0,1,2,3),
  xregs = c(0,1,2,3),
  TOF = c(TRUE, FALSE),
  Trans = c(TRUE, FALSE),
  Diff = c(TRUE, FALSE))

# Other tests
QA_Results[, TimeWeights := data.table::fifelse(runif(.N) < 0.5, 0.9999, 1)]
QA_Results[, TaskType := data.table::fifelse(runif(.N) < 0.5, "GPU", "CPU")]
QA_Results[, Success := "Failure"]
QA_Results[, RunTime := 123.456]
QA_Results[, DateTime := Sys.time()]
# QA_Results[, SaveModel := data.table::fifelse(runif(.N) < 0.5, TRUE, FALSE)]

# NOT Train On FULL TOF
# run = 37
# run = 45
# run = 53
# run = 61
# run = 69
# run = 101
# run = 109
# run = 117
# run = 125

# run = 101
for(run in  seq_len(QA_Results[,.N])) {


  # Unequal Start Dates
  # data[Region == 'A' & Date < "2010-05-05", ID := 'REMOVE']
  # data[is.na(ID), ID := 'KEEP']
  # data <- data[ID == 'KEEP'][, ID := NULL]


  # Data ----
  if(QA_Results[run, Group] == 0L) {
    data <- RemixAutoML:::Post_Query_Helper('"nogroupevalwalmart.csv"')[['data']]
  } else if(QA_Results[run, Group] == 1L) {
    data <- RemixAutoML:::Post_Query_Helper('"onegroupevalwalmart.csv"')[['data']]
  } else if(QA_Results[run, Group] == 2L) {
    data <- RemixAutoML:::Post_Query_Helper('"twogroupevalwalmart.csv"')[['data']]
  } else if(QA_Results[run, Group] == 3L) {
    data <- RemixAutoML:::Post_Query_Helper('"threegroupevalwalmart.csv"')[['data']]
  }

  # xregs
  if(QA_Results[run, xregs] == 0L) {
    xregs <- NULL
  } else if(QA_Results[run, xregs] == 1L) {
    if(QA_Results[run, Group] == 0L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("nogroupfcwalmartxreg1.csv"))[['data']]
    if(QA_Results[run, Group] == 1L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("onegroupfcwalmartxreg1.csv"))[['data']]
    if(QA_Results[run, Group] == 2L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("twogroupfcwalmartxreg1.csv"))[['data']]
    if(QA_Results[run, Group] == 3L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("threegroupfcwalmartxreg1.csv"))[['data']]
  } else if(QA_Results[run, xregs] == 2L) {
    if(QA_Results[run, Group] == 0L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("nogroupfcwalmartxreg2.csv"))[['data']]
    if(QA_Results[run, Group] == 1L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("onegroupfcwalmartxreg2.csv"))[['data']]
    if(QA_Results[run, Group] == 2L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("twogroupfcwalmartxreg2.csv"))[['data']]
    if(QA_Results[run, Group] == 3L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("threegroupfcwalmartxreg2.csv"))[['data']]
  } else if(QA_Results[run, xregs] == 3L) {
    if(QA_Results[run, Group] == 0L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("nogroupfcwalmartxreg3.csv"))[['data']]
    if(QA_Results[run, Group] == 1L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("onegroupfcwalmartxreg3.csv"))[['data']]
    if(QA_Results[run, Group] == 2L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("twogroupfcwalmartxreg3.csv"))[['data']]
    if(QA_Results[run, Group] == 3L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("threegroupfcwalmartxreg3.csv"))[['data']]
  }

  # Testing params
  TOF <- QA_Results[run, TOF]
  Trans <- QA_Results[run, Trans]
  Diff <- QA_Results[run, Diff]
  weights <- QA_Results[run, TimeWeights]
  tasktype <- QA_Results[run, TaskType]
  if(QA_Results[run, Group] == 0L) {
    groupvariables <- NULL
  } else if(QA_Results[run, Group] == 1L) {
    groupvariables <- "Dept"
  } else if(QA_Results[run, Group] == 2L) {
    groupvariables <- c("Store","Dept")
  } else if(QA_Results[run, Group] == 3L) {
    groupvariables <- c("Region","Store","Dept")
  }

  # Ensure series have no missing dates (also remove series with more than 25% missing values)
  data <- RemixAutoML::TimeSeriesFill(
    data,
    DateColumnName = "Date",
    GroupVariables = groupvariables,
    TimeUnit = "weeks",
    FillType = "maxmax",
    MaxMissingPercent = 0.25,
    SimpleImpute = TRUE)

  # Set negative numbers to 0
  data <- data[, Weekly_Sales := data.table::fifelse(Weekly_Sales < 0, 0, Weekly_Sales)]

  # Ensure series have no missing dates (also remove series with more than 25% missing values)
  if(QA_Results[run, xregs] != 0L) {
    xregs <- RemixAutoML::TimeSeriesFill(
      xregs,
      DateColumnName = "Date",
      GroupVariables = groupvariables,
      TimeUnit = "weeks",
      FillType = "maxmax",
      MaxMissingPercent = 0.25,
      SimpleImpute = TRUE)
  }

  # Copy data
  data1 <- data.table::copy(data)
  if(QA_Results[run, xregs] != 0L) xregs1 <- data.table::copy(xregs) else xregs1 <- NULL

  # Start Timer
  Start <- Sys.time()

  # Build forecast ----
  TestModel <- tryCatch({RemixAutoML::AutoCatBoostCARMA(

    SaveModel = FALSE,
    ArgsList = NULL, #TestModel$ArgsList, # NULL, #TestModel$ArgsList,

    # data args
    data = data1,
    XREGS = xregs1,
    TimeWeights = weights,
    TargetColumnName = "Weekly_Sales",
    DateColumnName = "Date",
    HierarchGroups = NULL,
    GroupVariables = groupvariables,
    TimeUnit = "weeks",
    TimeGroups = c("weeks","months"),

    # Production args
    TrainOnFull = TOF,
    SplitRatios = c(1 - 10 / 100, 10 / 100),
    PartitionType = "random",
    FC_Periods = 5,
    TaskType = tasktype,
    NumGPU = 1,
    EncodingMethod = 'target_encoding',
    Timer = TRUE,
    DebugMode = TRUE,

    # Target variable transformations
    TargetTransformation = Trans,
    Methods = c("Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit"),
    Difference = Diff,
    NonNegativePred = FALSE,
    RoundPreds = FALSE,

    # Calendar-related features
    CalendarVariables = c("week","wom","month","quarter"),
    HolidayVariable = c("USPublicHolidays"),
    HolidayLags = NULL, #c(1,2,3),
    HolidayMovingAverages = NULL, #c(2,3),

    # Lags, moving averages, and other rolling stats
    Lags = list("weeks" = c(1:5), "months" = c(1:3)),
    MA_Periods = list("weeks" = c(2:5), "months" = c(2,3)),
    SD_Periods = NULL,
    Skew_Periods = NULL,
    Kurt_Periods = NULL,
    Quantile_Periods = NULL,
    Quantiles_Selected = NULL,

    # Bonus features
    AnomalyDetection = NULL,
    FourierTerms = 0,
    TimeTrendVariable = TRUE,
    ZeroPadSeries = NULL,
    DataTruncate = FALSE,

    # ML grid tuning args
    GridTune = FALSE,
    PassInGrid = NULL,
    ModelCount = 5,
    MaxRunsWithoutNewWinner = 50,
    MaxRunMinutes = 60*60,

    # ML evaluation output
    PDFOutputPath = NULL,
    SaveDataPath = NULL,
    NumOfParDepPlots = 0L,

    # ML loss functions
    EvalMetric = "RMSE",
    EvalMetricValue = 1,
    LossFunction = "RMSE",
    LossFunctionValue = 1,

    # ML tuning args
    NTrees = 50L,
    Depth = 6L,
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
    MinDataInLeaf = 1)}, error = function(x) NULL)

  # Timer
  End <- Sys.time()
  QA_Results[run, RunTime := as.numeric(difftime(time1 = End, Start))]

  # Outcome
  if(!is.null(TestModel)) QA_Results[run, Success := "Success"]
  rm(TestModel)
  #data.table::fwrite(QA_Results, file = system.file('tests/Testing_Data/AutoCatBoostCARMA_QA.csv', package = 'RemixAutoML'))
  RemixAutoML:::Post_Append_Helper(QA_Results,'AutoCatBoostCARMA_QA')
  Sys.sleep(5)
}

# Defaults ----

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
source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ModelMetrics.R"))

# Collection data.table
QA_Results <- data.table::CJ(
  Group = c(0,1,2,3),
  xregs = c(0,1,2,3),
  TOF = c(TRUE, FALSE),
  Trans = c(TRUE, FALSE),
  Diff = c(TRUE, FALSE))

# Other tests
QA_Results[, TimeWeights := data.table::fifelse(runif(.N) < 0.5, 0.9999, 1)]
QA_Results[, TaskType := data.table::fifelse(runif(.N) < 0.5, "GPU", "CPU")]
QA_Results[, Success := "Failure"]

run = 101

# Data ----
if(QA_Results[run, Group] == 0L) {
  data <- RemixAutoML:::Post_Query_Helper('"nogroupevalwalmart.csv"')[['data']]
} else if(QA_Results[run, Group] == 1L) {
  data <- RemixAutoML:::Post_Query_Helper('"onegroupevalwalmart.csv"')[['data']]
} else if(QA_Results[run, Group] == 2L) {
  data <- RemixAutoML:::Post_Query_Helper('"twogroupevalwalmart.csv"')[['data']]
} else if(QA_Results[run, Group] == 3L) {
  data <- RemixAutoML:::Post_Query_Helper('"threegroupevalwalmart.csv"')[['data']]
}

# Unequal Start Dates
data[Region == 'A' & Date < "2010-05-05", ID := 'REMOVE']
data[is.na(ID), ID := 'KEEP']
data <- data[ID == 'KEEP'][, ID := NULL]

# xregs
if(QA_Results[run, xregs] == 0L) {
  xregs <- NULL
} else if(QA_Results[run, xregs] == 1L) {
  if(QA_Results[run, Group] == 0L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("nogroupfcwalmartxreg1.csv"))[['data']]
  if(QA_Results[run, Group] == 1L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("onegroupfcwalmartxreg1.csv"))[['data']]
  if(QA_Results[run, Group] == 2L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("twogroupfcwalmartxreg1.csv"))[['data']]
  if(QA_Results[run, Group] == 3L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("threegroupfcwalmartxreg1.csv"))[['data']]
} else if(QA_Results[run, xregs] == 2L) {
  if(QA_Results[run, Group] == 0L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("nogroupfcwalmartxreg2.csv"))[['data']]
  if(QA_Results[run, Group] == 1L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("onegroupfcwalmartxreg2.csv"))[['data']]
  if(QA_Results[run, Group] == 2L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("twogroupfcwalmartxreg2.csv"))[['data']]
  if(QA_Results[run, Group] == 3L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("threegroupfcwalmartxreg2.csv"))[['data']]
} else if(QA_Results[run, xregs] == 3L) {
  if(QA_Results[run, Group] == 0L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("nogroupfcwalmartxreg3.csv"))[['data']]
  if(QA_Results[run, Group] == 1L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("onegroupfcwalmartxreg3.csv"))[['data']]
  if(QA_Results[run, Group] == 2L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("twogroupfcwalmartxreg3.csv"))[['data']]
  if(QA_Results[run, Group] == 3L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("threegroupfcwalmartxreg3.csv"))[['data']]
}

# Testing params
TOF <- QA_Results[run, TOF]
Trans <- QA_Results[run, Trans]
Diff <- QA_Results[run, Diff]
weights <- QA_Results[run, TimeWeights]
tasktype <- QA_Results[run, TaskType]
if(QA_Results[run, Group] == 0L) {
  groupvariables <- NULL
} else if(QA_Results[run, Group] == 1L) {
  groupvariables <- "Dept"
} else if(QA_Results[run, Group] == 2L) {
  groupvariables <- c("Store","Dept")
} else if(QA_Results[run, Group] == 3L) {
  groupvariables <- c("Region","Store","Dept")
}

# Ensure series have no missing dates (also remove series with more than 25% missing values)
data <- RemixAutoML::TimeSeriesFill(
  data,
  DateColumnName = "Date",
  GroupVariables = groupvariables,
  TimeUnit = "weeks",
  FillType = "maxmax",
  MaxMissingPercent = 0.25,
  SimpleImpute = TRUE)

# Set negative numbers to 0
data <- data[, Weekly_Sales := data.table::fifelse(Weekly_Sales < 0, 0, Weekly_Sales)]

# Ensure series have no missing dates (also remove series with more than 25% missing values)
if(QA_Results[run, xregs] != 0L) {
  xregs <- RemixAutoML::TimeSeriesFill(
    xregs,
    DateColumnName = "Date",
    GroupVariables = groupvariables,
    TimeUnit = "weeks",
    FillType = "maxmax",
    MaxMissingPercent = 0.25,
    SimpleImpute = TRUE)
}

# Copy data
data1 <- data.table::copy(data)
if(QA_Results[run, xregs] != 0L) xregs1 <- data.table::copy(xregs) else xregs1 <- NULL

SaveModel = FALSE #FALSE
ArgsList = NULL #TestModel$ArgsList #ArgsList
data = data1
XREGS = xregs1
TimeWeights = weights
TargetColumnName = "Weekly_Sales"
DateColumnName = "Date"
HierarchGroups = NULL
GroupVariables = groupvariables
EncodingMethod = 'MEOW' #'target_encoding' #'credibility'
TimeUnit = "weeks"
TimeGroups = c("weeks","months")
TrainOnFull = TOF
SplitRatios = c(1 - 10 / 100, 10 / 100)
PartitionType = "random"
FC_Periods = 10
TaskType = tasktype
NumGPU = 1
Timer = TRUE
DebugMode = TRUE
TargetTransformation = Trans
Methods = c("Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit")
Difference = Diff
NonNegativePred = TRUE
RoundPreds = FALSE
CalendarVariables = c("week","wom","month","quarter")
HolidayVariable = c("USPublicHolidays")
HolidayLookback = 7
HolidayLags = c(1,2,3)
HolidayMovingAverages = c(2,3)
Lags = list("weeks" = c(1:5), "months" = c(1:3))
MA_Periods = list("weeks" = c(2:5), "months" = c(2,3))
SD_Periods = NULL
Skew_Periods = NULL
Kurt_Periods = NULL
Quantile_Periods = NULL
Quantiles_Selected = NULL
AnomalyDetection = NULL
FourierTerms = 0
TimeTrendVariable = TRUE
ZeroPadSeries = NULL
DataTruncate = FALSE
GridTune = FALSE
PassInGrid = NULL
ModelCount = 5
MaxRunsWithoutNewWinner = 50
MaxRunMinutes = 60*60
PDFOutputPath = NULL
SaveDataPath = NULL
NumOfParDepPlots = 0L
EvalMetric = "RMSE"
EvalMetricValue = 1
LossFunction = "RMSE"
LossFunctionValue = 1
NTrees = 50L
Depth = 6L
L2_Leaf_Reg = NULL
LearningRate = NULL
Langevin = FALSE
DiffusionTemperature = 10000
RandomStrength = 1
BorderCount = 254
RSM = NULL
GrowPolicy = "SymmetricTree"
BootStrapType = "Bayesian"
ModelSizeReg = 0.5
FeatureBorderType = "GreedyLogSum"
SamplingUnit = "Group"
SubSample = NULL
ScoreFunction = "Cosine"
MinDataInLeaf = 1

# CarmaTimeSeriesFeatures ----
# data.=data
# TargetColumnName.=TargetColumnName
# DateColumnName.=DateColumnName
# GroupVariables.=GroupVariables
# HierarchGroups.=HierarchGroups
# Difference.=Difference
# TimeGroups.=TimeGroups
# TimeUnit.=TimeUnit
# Lags.=Lags
# MA_Periods.=MA_Periods
# SD_Periods.=SD_Periods
# Skew_Periods.=Skew_Periods
# Kurt_Periods.=Kurt_Periods
# Quantile_Periods.=Quantile_Periods
# Quantiles_Selected.=Quantiles_Selected
# HolidayVariable.=HolidayVariable
# HolidayLags.=HolidayLags
# HolidayMovingAverages.=HolidayMovingAverages
# DebugMode.=DebugMode
#
# Update time series features ----
ModelType="catboost"
DebugMode.=DebugMode
UpdateData.=UpdateData
GroupVariables.=GroupVariables
Difference.=Difference
CalendarVariables.=CalendarVariables
HolidayVariable.=HolidayVariable
IndepVarPassTRUE.=IndepentVariablesPass
data.=data
CalendarFeatures.=CalendarFeatures
XREGS.=XREGS
HierarchGroups.=HierarchGroups
GroupVarVector.=GroupVarVector
TargetColumnName.=TargetColumnName
DateColumnName.=DateColumnName
Preds.=Preds
HierarchSupplyValue.=HierarchSupplyValue
IndependentSupplyValue.=IndependentSupplyValue
TimeUnit.=TimeUnit
FourierTerms. = NULL
TimeGroups.=TimeGroups
Lags.=Lags
MA_Periods.=MA_Periods
SD_Periods.=SD_Periods
Skew_Periods.=Skew_Periods
Kurt_Periods.=Kurt_Periods
Quantile_Periods.=Quantile_Periods
Quantiles_Selected.=Quantiles_Selected
HolidayLags.=HolidayLags
HolidayMovingAverages.=HolidayMovingAverages
HolidayLookback. = 7

# Data ----
# data                 = Temporary
# RowNumsID            = "ID"
# RowNumsKeep          = 1
# DateColumn           = eval(DateColumnName.)
# Targets              = eval(TargetColumnName.)
# HierarchyGroups      = NULL
# IndependentGroups    = NULL
# # Service
# TimeBetween          = NULL
# TimeUnit             = TimeUnit.
# TimeUnitAgg          = TimeGroups.[1]
# TimeGroups           = TimeGroups.
# RollOnLag1           = TRUE
# Type                 = "Lag"
# SimpleImpute         = TRUE
# # Calculated Column
# Lags                 = Lags.
# MA_RollWindows       = MA_Periods.
# SD_RollWindows       = SD_Periods.
# Skew_RollWindows     = Skew_Periods.
# Kurt_RollWindows     = Kurt_Periods.
# Quantile_RollWindows = Quantile_Periods.
# Quantiles_Selected   = Quantiles_Selected.
# Debug                = DebugMode.
#
# Keep GDL Features ----
# IndepVarPassTRUE = NULL
# data.
# UpdateData.
# CalendarFeatures.
# XREGS.
# Difference.
# HierarchGroups.
# GroupVariables.,
# GroupVarVector.
# CalendarVariables=CalVar
# HolidayVariable=HolVar
# TargetColumnName.,DateColumnName.
# Preds.

# AutoCatBoostRegression Args ----
# task_type = TaskType
# NumGPUs = NumGPU
# OutputSelection = c("Importances", "EvalPlots", "EvalMetrics", "Score_TrainData")
# # Metadata argument
# ModelID = "CatBoost"
# model_path = getwd()
# metadata_path = if(!is.null(PDFOutputPath)) PDFOutputPath else getwd()
# SaveModelObjects = FALSE
# ReturnModelObjects = TRUE
# SaveInfoToPDF = if(!is.null(PDFOutputPath)) TRUE else FALSE
# # Data argument
# data = train
# TrainOnFull = TOF
# ValidationData = valid
# TestData = test
# TargetColumnName = TargetVariable
# FeatureColNames = ModelFeatures
# PrimaryDateColumn = eval(DateColumnName)
# WeightsColumnName = if("Weights" %in% names(train)) "Weights" else NULL
# IDcols = IDcols
# TransformNumericColumns = NULL
# Methods = NULL
# # Model evaluatio
# eval_metric = EvalMetric
# eval_metric_value = EvalMetricValue
# loss_function = LossFunction
# loss_function_value = LossFunctionValue
# MetricPeriods = 10L
# NumOfParDepPlots = NumOfParDepPlots
# # Grid tuning argument
# PassInGrid = PassInGrid
# GridTune = GridTune
# MaxModelsInGrid = ModelCount
# MaxRunsWithoutNewWinner = MaxRunsWithoutNewWinner
# MaxRunMinutes = 60*60
# BaselineComparison = "default"
# # ML arg
# langevin = Langevin
# diffusion_temperature = DiffusionTemperature
# Trees = NTrees
# Depth = Depth
# LearningRate = LearningRate
# L2_Leaf_Reg = L2_Leaf_Reg
# RandomStrength = RandomStrength
# BorderCount = BorderCount
# RSM = if(TaskType == "GPU") NULL else RSM
# BootStrapType = BootStrapType
# GrowPolicy = GrowPolicy
# # New ML arg
# model_size_reg = ModelSizeReg
# feature_border_type = FeatureBorderType
# sampling_unit = SamplingUnit
# subsample = SubSample
# score_function = ScoreFunction
# min_data_in_leaf = MinDataInLeaf
# DebugMode = DebugMode

# Carma score ----
i.=i
N.=N
GroupVariables.=GroupVariables
ModelFeatures.=ModelFeatures
HierarchGroups.=HierarchGroups
DateColumnName.=DateColumnName
Difference.=Difference
TargetColumnName.=TargetColumnName
Step1SCore.=Step1SCore
Model.=Model
FutureDateData.=FutureDateData
NonNegativePred.=NonNegativePred
UpdateData.=UpdateData
FactorList.= TestModel$FactorLevelsList
EncodingMethod. = TestModel$FactorLevelsList$EncodingMethod

# Catboost scoring ----

# i == 1
# TargetType = 'regression'
# ScoringData = Step1SCore.
# FeatureColumnNames = ModelFeatures.
# FactorLevelsList = FactorList.
# IDcols = IDcols
# OneHot = FALSE
# ModelObject = Model.
# ModelPath = getwd()
# ReturnShapValues = FALSE
# MultiClassTargetLevels = NULL
# RemoveModel = FALSE
# ModelID = 'ModelTest'
# ReturnFeatures = TRUE
# TransformNumeric = FALSE
# BackTransNumeric = FALSE
# TransformationObject = NULL
# TransID = NULL
# TransPath = NULL
# TargetColumnName = NULL
# MDP_Impute = FALSE
# MDP_CharToFactor = FALSE
# MDP_RemoveDates = TRUE
# MDP_MissFactor = '0'
# MDP_MissNum = -1

# i == 2
# TargetType = 'regression'
# ScoringData = temp
# FeatureColumnNames = ModelFeatures.
# FactorLevelsList = FactorList.
# ReturnShapValues = FALSE
# IDcols = IDcols
# OneHot = FALSE
# ModelObject = Model.
# ModelPath = getwd()
# ModelID = 'ModelTest'
# ReturnFeatures = FALSE
# TransformNumeric = FALSE
# BackTransNumeric = FALSE
# TargetColumnName = NULL
# TransformationObject = NULL
# TransID = NULL
# TransPath = NULL
# MDP_Impute = FALSE
# MDP_CharToFactor = FALSE
# MDP_RemoveDates = TRUE
# MDP_MissFactor = '0'
# MDP_MissNum = -1

# Return data ----
# UpdateData.=UpdateData
# FutureDateData.=FutureDateData
# dataStart.=dataStart
# DateColumnName.=DateColumnName
# TargetColumnName.=TargetColumnName
# GroupVariables.=GroupVariables
# Difference.=Difference
# TargetTransformation.=TargetTransformation
# TransformObject.=TransformObject
# NonNegativePred.=NonNegativePred
# DiffTrainOutput.=DiffTrainOutput

# Difference data reverse single series


# ----

# ----
