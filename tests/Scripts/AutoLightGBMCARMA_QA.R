# Collection data.table
QA_Results <- data.table::CJ(
  Group = c(0,1,2,3),
  xregs = c(0,1,2,3),
  TOF = c(TRUE, FALSE),
  Trans = c(TRUE, FALSE),
  Diff = c(TRUE, FALSE))

# Other tests
QA_Results[, Success := "Failure"]

# run = 5
# run = 37
for(run in seq_len(QA_Results[,.N])) {

  # Data
  if(QA_Results[run, Group] == 0L) {
    data <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/NoGroup-Eval-Walmart.csv")
  } else if(QA_Results[run, Group] == 1L) {
    data <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/OneGroup-Eval-Walmart.csv")
  } else if(QA_Results[run, Group] == 2L) {
    data <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/TwoGroup-Eval-Walmart.csv")
  } else if(QA_Results[run, Group] == 3L) {
    data <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/ThreeGroup-Eval-Walmart.csv")
  }

  # xregs
  if(QA_Results[run, xregs] == 0L) {
    xregs <- NULL
  } else if(QA_Results[run, xregs] == 1L) {
    if(QA_Results[run, Group] == 0L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/NoGroup-FC-Walmart-XREG1.csv")
    if(QA_Results[run, Group] == 1L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/OneGroup-FC-Walmart-XREG1.csv")
    if(QA_Results[run, Group] == 2L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/TwoGroup-FC-Walmart-XREG1.csv")
    if(QA_Results[run, Group] == 3L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/ThreeGroup-FC-Walmart-XREG1.csv")
  } else if(QA_Results[run, xregs] == 2L) {
    if(QA_Results[run, Group] == 0L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/NoGroup-FC-Walmart-XREG2.csv")
    if(QA_Results[run, Group] == 1L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/OneGroup-FC-Walmart-XREG2.csv")
    if(QA_Results[run, Group] == 2L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/TwoGroup-FC-Walmart-XREG2.csv")
    if(QA_Results[run, Group] == 3L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/ThreeGroup-FC-Walmart-XREG2.csv")
  } else if(QA_Results[run, xregs] == 3L) {
    if(QA_Results[run, Group] == 0L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/NoGroup-FC-Walmart-XREG3.csv")
    if(QA_Results[run, Group] == 1L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/OneGroup-FC-Walmart-XREG3.csv")
    if(QA_Results[run, Group] == 2L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/TwoGroup-FC-Walmart-XREG3.csv")
    if(QA_Results[run, Group] == 3L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/ThreeGroup-FC-Walmart-XREG3.csv")
  }

  # Testing params
  TOF <- QA_Results[run, TOF]
  Trans <- QA_Results[run, Trans]
  Diff <- QA_Results[run, Diff]
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

  # Build forecast
  TestModel <- tryCatch({RemixAutoML::AutoLightGBMCARMA(

    # Data Artifacts
    data = data1,
    XREGS = xregs1,
    NonNegativePred = FALSE,
    RoundPreds = FALSE,
    TargetColumnName = "Weekly_Sales",
    DateColumnName = "Date",
    HierarchGroups = NULL,
    GroupVariables = groupvariables,
    TimeUnit = "weeks",
    TimeGroups = c("weeks","months"),
    TimeWeights = 0.9999,

    # Data Wrangling Features
    ZeroPadSeries = NULL,
    DataTruncate = FALSE,
    SplitRatios = c(1 - 10 / 100, 10 / 100),
    PartitionType = "timeseries",
    AnomalyDetection = NULL,
    EncodingMethod = "credibility",

    # Productionize
    FC_Periods = 4,
    TrainOnFull = TOF,
    NThreads = 64,
    Timer = TRUE,
    DebugMode = TRUE,
    SaveDataPath = getwd(),
    PDFOutputPath = getwd(),

    # Target Transformations
    TargetTransformation = Trans,
    Methods = c("BoxCox", "Asinh", "Asin", "Log", "LogPlus1", "Sqrt", "Logit"),
    Difference = Diff,

    # Features
    Lags = list("weeks" = c(1:5), "months" = c(1:3)),
    MA_Periods = list("weeks" = c(2:5), "months" = c(2,3)),
    SD_Periods = NULL,
    Skew_Periods = NULL,
    Kurt_Periods = NULL,
    Quantile_Periods = NULL,
    HolidayLags = 1,
    HolidayMovingAverages = 1:2,
    Quantiles_Selected = c("q5","q95"),
    FourierTerms = 0,
    CalendarVariables = c("week","wom","month","quarter"),
    HolidayVariable = c("USPublicHolidays","EasterGroup","ChristmasGroup","OtherEcclesticalFeasts"),
    HolidayLookback = 7,
    TimeTrendVariable = TRUE,

    # Grid tuning args
    GridTune = FALSE,
    GridEvalMetric = "mae",
    ModelCount = 30L,
    MaxRunsWithoutNewWinner = 20L,
    MaxRunMinutes = 24L*60L,

    # LightGBM Args
    Device_Type = 'CPU',
    LossFunction = "regression",
    EvalMetric = "MAE",
    Input_Model = NULL,
    Task = "train",
    Boosting = "gbdt",
    LinearTree = FALSE,
    Trees = 50L,
    ETA = .1,
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
    Monotone_Constraints_method = "advanced",
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
    Convert_Model_Language = "cpp",

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
  if(!is.null(TestModel)) QA_Results[run, Success := "Success"]
  rm(TestModel)
  data.table::fwrite(QA_Results, file = "C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/Testing_Data/AutoLightGBMCARMA_QA.csv")
  Sys.sleep(5)
}

# Defaults ----
# library(RemixAutoML)
# library(data.table)
# library(lubridate)
#
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/MiscFunctions.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/XGBoostHelpers.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/LightGBMHelpers.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ModelMetrics.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/CARMA-HelperFunctions.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/FeatureEngineering_CalendarTypes.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/FeatureEngineering_CharacterTypes.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/FeatureEngineering_CrossRowOperations.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/FeatureEngineering_NumericTypes.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/FeatureEngineering_DataSets.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/FeatureEngineering_ModelBased.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ModelEvaluationPlots.R"))
#
# run = 5
#
# # Data
# if(QA_Results[run, Group] == 0L) {
#   data <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/NoGroup-Eval-Walmart.csv")
# } else if(QA_Results[run, Group] == 1L) {
#   data <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/OneGroup-Eval-Walmart.csv")
# } else if(QA_Results[run, Group] == 2L) {
#   data <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/TwoGroup-Eval-Walmart.csv")
# } else if(QA_Results[run, Group] == 3L) {
#   data <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/ThreeGroup-Eval-Walmart.csv")
# }
#
# # xregs
# if(QA_Results[run, xregs] == 0L) {
#   xregs <- NULL
# } else if(QA_Results[run, xregs] == 1L) {
#   if(QA_Results[run, Group] == 0L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/NoGroup-FC-Walmart-XREG1.csv")
#   if(QA_Results[run, Group] == 1L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/OneGroup-FC-Walmart-XREG1.csv")
#   if(QA_Results[run, Group] == 2L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/TwoGroup-FC-Walmart-XREG1.csv")
#   if(QA_Results[run, Group] == 3L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/ThreeGroup-FC-Walmart-XREG1.csv")
# } else if(QA_Results[run, xregs] == 2L) {
#   if(QA_Results[run, Group] == 0L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/NoGroup-FC-Walmart-XREG2.csv")
#   if(QA_Results[run, Group] == 1L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/OneGroup-FC-Walmart-XREG2.csv")
#   if(QA_Results[run, Group] == 2L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/TwoGroup-FC-Walmart-XREG2.csv")
#   if(QA_Results[run, Group] == 3L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/ThreeGroup-FC-Walmart-XREG2.csv")
# } else if(QA_Results[run, xregs] == 3L) {
#   if(QA_Results[run, Group] == 0L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/NoGroup-FC-Walmart-XREG3.csv")
#   if(QA_Results[run, Group] == 1L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/OneGroup-FC-Walmart-XREG3.csv")
#   if(QA_Results[run, Group] == 2L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/TwoGroup-FC-Walmart-XREG3.csv")
#   if(QA_Results[run, Group] == 3L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/ThreeGroup-FC-Walmart-XREG3.csv")
# }
#
# # Testing params
# TOF <- QA_Results[run, TOF]
# Trans <- QA_Results[run, Trans]
# Diff <- QA_Results[run, Diff]
# if(QA_Results[run, Group] == 0L) {
#   groupvariables <- NULL
# } else if(QA_Results[run, Group] == 1L) {
#   groupvariables <- "Dept"
# } else if(QA_Results[run, Group] == 2L) {
#   groupvariables <- c("Store","Dept")
# } else if(QA_Results[run, Group] == 3L) {
#   groupvariables <- c("Region","Store","Dept")
# }
#
# # Ensure series have no missing dates (also remove series with more than 25% missing values)
# data <- RemixAutoML::TimeSeriesFill(
#   data,
#   DateColumnName = "Date",
#   GroupVariables = groupvariables,
#   TimeUnit = "weeks",
#   FillType = "maxmax",
#   MaxMissingPercent = 0.25,
#   SimpleImpute = TRUE)
#
# # Set negative numbers to 0
# data <- data[, Weekly_Sales := data.table::fifelse(Weekly_Sales < 0, 0, Weekly_Sales)]
#
# # Ensure series have no missing dates (also remove series with more than 25% missing values)
# if(QA_Results[run, xregs] != 0L) {
#   xregs <- RemixAutoML::TimeSeriesFill(
#     xregs,
#     DateColumnName = "Date",
#     GroupVariables = groupvariables,
#     TimeUnit = "weeks",
#     FillType = "maxmax",
#     MaxMissingPercent = 0.25,
#     SimpleImpute = TRUE)
# }
#
# # Copy data
# data1 <- data.table::copy(data)
# if(QA_Results[run, xregs] != 0L) xregs1 <- data.table::copy(xregs) else xregs1 <- NULL
#
# # Copy data
# data = data1
# XREGS = xregs1
# NonNegativePred = FALSE
# RoundPreds = FALSE
# TargetColumnName = "Weekly_Sales"
# DateColumnName = "Date"
# HierarchGroups = NULL
# GroupVariables = groupvariables
# TimeUnit = "weeks"
# TimeGroups = c("weeks","months")
# TimeWeights = 0.9999
#
# # Data Wrangling Features
# ZeroPadSeries = NULL
# DataTruncate = FALSE
# SplitRatios = c(1 - 10 / 100, 10 / 100)
# PartitionType = "timeseries"
# AnomalyDetection = NULL
# EncodingMethod = "credibility"
#
# # Productionize
# FC_Periods = 4
# TrainOnFull = TOF
# NThreads = 64
# Timer = TRUE
# DebugMode = TRUE
# SaveDataPath = getwd()
# PDFOutputPath = getwd()
#
# # Target Transformations
# TargetTransformation = Trans
# Methods = c("Asinh", "Asin", "Log", "LogPlus1", "Sqrt", "Logit")
# Difference = Diff
#
# # Features
# Lags = list("weeks" = c(1:5), "months" = c(1:3))
# MA_Periods = list("weeks" = c(2:5), "months" = c(2,3))
# SD_Periods = NULL
# Skew_Periods = NULL
# Kurt_Periods = NULL
# Quantile_Periods = NULL
# HolidayLags = 1
# HolidayMovingAverages = 1:2
# Quantiles_Selected = c("q5","q95")
# FourierTerms = 0
# CalendarVariables = c("week","wom","month","quarter")
# HolidayVariable = c("USPublicHolidays","EasterGroup","ChristmasGroup","OtherEcclesticalFeasts")
# HolidayLookback = 7
# TimeTrendVariable = TRUE
#
# # Grid tuning args
# GridTune = FALSE
# GridEvalMetric = "mae"
# ModelCount = 30L
# MaxRunsWithoutNewWinner = 20L
# MaxRunMinutes = 24L*60L
#
# # LightGBM Args
# Device_Type = 'cpu'
# LossFunction = "regression"
# EvalMetric = "MAE"
# Input_Model = NULL
# Task = "train"
# Boosting = "gbdt"
# LinearTree = FALSE
# Trees = 50L
# ETA = 0.1
# Num_Leaves = 31
# Deterministic = TRUE
#
# # Learning Parameters
# # https://lightgbm.readthedocs.io/en/latest/Parameters.html#learning-control-parameters
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
# Monotone_Constraints_Method = "advanced"
# Monotone_Penalty = 0.0
# Forcedsplits_Filename = NULL # use for AutoStack option; .json file
# Refit_Decay_Rate = 0.90
# Path_Smooth = 0.0
#
# # IO Dataset Parameters
# # https://lightgbm.readthedocs.io/en/latest/Parameters.html#io-parameters
# Max_Bin = 255
# Min_Data_In_Bin = 3
# Data_Random_Seed = 1
# Is_Enable_Sparse = TRUE
# Enable_Bundle = TRUE
# Use_Missing = TRUE
# Zero_As_Missing = FALSE
# Two_Round = FALSE
#
# # Convert Parameters
# Convert_Model = NULL
# Convert_Model_Language = "cpp"
#
# # Objective Parameters
# # https://lightgbm.readthedocs.io/en/latest/Parameters.html#objective-parameters
# Boost_From_Average = TRUE
# Alpha = 0.90
# Fair_C = 1.0
# Poisson_Max_Delta_Step = 0.70
# Tweedie_Variance_Power = 1.5
# Lambdarank_Truncation_Level = 30
#
# # Metric Parameters (metric is in Core)
# # https://lightgbm.readthedocs.io/en/latest/Parameters.html#metric-parameters
# Is_Provide_Training_Metric = TRUE
# Eval_At = c(1,2,3,4,5)
#
# # Network Parameters
# # https://lightgbm.readthedocs.io/en/latest/Parameters.html#network-parameters
# Num_Machines = 1
#
# # GPU Parameters
# # https://lightgbm.readthedocs.io/en/latest/Parameters.html#gpu-parameters
# Gpu_Device_Id = -1
# Gpu_Use_Dp = TRUE
# Num_Gpu = 1

# Differencing ----
# GroupVariables.=GroupVariables
# Difference.=Difference
# data.=data
# TargetColumnName.=TargetColumnName
# FC_Periods.=FC_Periods

# DifferenceData ----
# data = data.
# ColumnsToDiff = eval(TargetColumnName.)
# CARMA = TRUE
# TargetVariable = eval(TargetColumnName.)
# GroupingVariable = NULL

# Rolling Stats ----
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

# AutoLagRollStats ----
# data = data.
# DateColumn = eval(DateColumnName.)
# Targets = eval(TargetColumnName.)
# HierarchyGroups = NULL
# IndependentGroups = NULL
# TimeBetween = NULL
# TimeUnit = TimeUnit.
# TimeUnitAgg = TimeUnit.
# TimeGroups = TimeGroups.
# RollOnLag1 = TRUE
# Type = "Lag"
# SimpleImpute = TRUE
# Lags = Lags.
# MA_RollWindows = MA_Periods.
# SD_RollWindows = SD_Periods.
# Skew_RollWindows = Skew_Periods.
# Kurt_RollWindows = Kurt_Periods.
# Quantile_RollWindows = Quantile_Periods.
# Quantiles_Selected = Quantiles_Selected.
# Debug = DebugMode.

# Rolling stats update ----
# ModelType="xgboost"
# DebugMode.=DebugMode
# UpdateData.=UpdateData
# GroupVariables.=GroupVariables
# Difference.=Difference
# CalendarVariables.=CalendarVariables
# HolidayVariable.=HolidayVariable
# IndepVarPassTRUE.=IndepentVariablesPass
# data.=data
# CalendarFeatures.=CalendarFeatures
# XREGS.=XREGS
# HierarchGroups.=HierarchGroups
# GroupVarVector.=GroupVarVector
# TargetColumnName.=TargetColumnName
# DateColumnName.=DateColumnName
# Preds.=NULL
# HierarchSupplyValue.=HierarchSupplyValue
# IndependentSupplyValue.=IndependentSupplyValue
# TimeUnit.=TimeUnit
# TimeGroups.=TimeGroups
# Lags.=Lags
# MA_Periods.=MA_Periods
# SD_Periods.=SD_Periods
# Skew_Periods.=Skew_Periods
# Kurt_Periods.=Kurt_Periods
# Quantile_Periods.=Quantile_Periods
# Quantiles_Selected.=Quantiles_Selected
# HolidayLags.=HolidayLags
# HolidayMovingAverages.=HolidayMovingAverages

# CarmaUpdateVars ----
# IndepVarPassTRUE=NULL
# data.
# UpdateData.
# CalendarFeatures.
# XREGS.
# Difference.
# HierarchGroups.
# GroupVariables.
# GroupVarVector.
# CalendarVariables=CalVar
# HolidayVariable=HolVar
# TargetColumnName.
# DateColumnName.

# CarmaScore ----
# i = 1
# Type = "lightgbm"
# i.=i
# N.=N
# EncodingMethod. = EncodingMethod
# GroupVariables.=GroupVariables
# ModelFeatures.=ModelFeatures
# HierarchGroups.=HierarchGroups
# DateColumnName.=DateColumnName
# Difference.=Difference
# TargetColumnName.=TargetColumnName
# Step1SCore.=Step1SCore
# Model.=Model
# FutureDateData.=FutureDateData
# NonNegativePred.=NonNegativePred
# RoundPreds.=RoundPreds
# UpdateData.= if(i == 1) NULL else UpdateData
# FactorList.=FactorList

# LightGBM Scoring
# TargetType = 'regression'
# ScoringData = Step1SCore.
# FeatureColumnNames = ModelFeatures.
# ReturnShapValues = FALSE
# IDcols = IDcols
# ModelObject = Model.
# ModelPath = getwd()
# ModelID = 'ModelTest'
# EncodingMethod = EncodingMethod.
# ReturnFeatures = TRUE
# TransformNumeric = FALSE
# BackTransNumeric = FALSE
# TargetColumnName = NULL
# TransformationObject = NULL
# FactorLevelsList = FactorList.
# TransID = NULL
# TransPath = NULL
# MDP_Impute = TRUE
# MDP_CharToFactor = TRUE
# MDP_RemoveDates = TRUE
# MDP_MissFactor = '0'
# MDP_MissNum = -1

# Categorical encoding ----
# data=ScoringData
# ML_Type=TargetType
# GroupVariables=names(FactorLevelsList)
# TargetVariable=NULL
# Method=EncodingMethod
# SavePath=model_path.
# Scoring=TRUE
# ImputeValueScoring=0
# ReturnFactorLevelList=FALSE
# SupplyFactorLevelList=FactorLevelsList
# KeepOriginalFactors=FALSE

# Regression ----
# OutputSelection = c("Importances", "EvalPlots", "EvalMetrics", "Score_TrainData")
# WeightsColumnName = NULL
# TreeMethod = TreeMethod
# NThreads = NThreads
# model_path = getwd()
# metadata_path = if(!is.null(PDFOutputPath)) PDFOutputPath else getwd()
# ModelID = "XGBoost"
# ReturnFactorLevels = TRUE
# ReturnModelObjects = TRUE
# SaveModelObjects = FALSE
# SaveInfoToPDF = if(!is.null(PDFOutputPath)) TRUE else FALSE
# data = train
# TrainOnFull = TrainOnFull
# ValidationData = valid
# TestData = test
# TargetColumnName = TargetVariable
# FeatureColNames = ModelFeatures
# IDcols = IDcols
# TransformNumericColumns = NULL
# Methods = NULL
# LossFunction = LossFunction
# eval_metric = EvalMetric
# NumOfParDepPlots = 10
# PassInGrid = NULL
# GridTune = GridTune
# grid_eval_metric = GridEvalMetric
# BaselineComparison = "default"
# MaxModelsInGrid = ModelCount
# MaxRunsWithoutNewWinner = MaxRunsWithoutNewWinner
# MaxRunMinutes = MaxRunMinutes
# Verbose = 1L
# Trees = NTrees
# eta = LearningRate
# max_depth = MaxDepth
# min_child_weight = MinChildWeight
# subsample = SubSample
# colsample_bytree = ColSampleByTree

# Data Prep ----
# ModelType="regression"
# data.=data
# ValidationData.=ValidationData
# TestData.=TestData
# TargetColumnName.=TargetColumnName
# FeatureColNames.=FeatureColNames
# WeightsColumnName.=WeightsColumnName
# IDcols.=IDcols
# TransformNumericColumns.=TransformNumericColumns
# Methods.=Methods
# ModelID.=ModelID
# model_path.=model_path
# TrainOnFull.=TrainOnFull
# SaveModelObjects.=SaveModelObjects
# ReturnFactorLevels.=ReturnFactorLevels
# EncodingMethod.=EncodingMethod

# Train Validation Data ----
# model.=model
# TestData.=NULL
# ModelType="regression"
# TrainOnFull.=TRUE
# TestDataCheck=FALSE
# FinalTestTarget.=FinalTestTarget
# TestTarget.=TestTarget
# TrainTarget.=TrainTarget
# TrainMerge.=TrainMerge
# TestMerge.=TestMerge
# dataTest.=dataTest
# data.=dataTrain
# predict.=predict
# TargetColumnName.=TargetColumnName
# SaveModelObjects. = SaveModelObjects
# metadata_path.=metadata_path
# model_path.=model_path
# ModelID.=ModelID
# LossFunction.=NULL
# TransformNumericColumns.=TransformNumericColumns
# GridTune.=GridTune
# TransformationResults.=TransformationResults
# TargetLevels.=NULL

# Validation Data ----
# ModelType="regression"
# TestDataCheck=!is.null(TestData)
# TrainOnFull.=TrainOnFull
# model.=model
# TargetColumnName.=TargetColumnName
# SaveModelObjects.=SaveModelObjects
# metadata_path.=metadata_path
# model_path.=model_path
# ModelID.=ModelID
# TestData.=TestData
# TestTarget.=TestTarget
# FinalTestTarget.=FinalTestTarget
# TestMerge.=TestMerge
# dataTest.=dataTest
# TrainTarget.=TrainTarget
# predict.=predict
# TransformNumericColumns.=TransformNumericColumns
# TransformationResults.=TransformationResults
# GridTune.=GridTune
# data.=dataTrain

# Eval Plots Validation ----
# ModelType="regression"
# TrainOnFull.=TrainOnFull
# ValidationData.=ValidationData
# NumOfParDepPlots.=NumOfParDepPlots
# VariableImportance.=VariableImportance
# TargetColumnName.=TargetColumnName
# FeatureColNames.=FeatureColNames
# SaveModelObjects.=SaveModelObjects
# ModelID.=ModelID
# metadata_path.=metadata_path
# model_path.=model_path
# LossFunction.="RMSE"
# EvalMetric.=NULL
# EvaluationMetrics.=EvalMetricsList
# predict.=NULL

# Return Data Prep ----
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
# DiffTrainOutput. = NULL

# DiffernceDataReverse ----
# data = UpdateData.
# ScoreData = NULL
# CARMA = TRUE
# TargetCol = eval(TargetColumnName.)
# FirstRow = DiffTrainOutput.$FirstRow[[eval(TargetColumnName.)]]
# LastRow = NULL

# Update Features ----
# UpdateData.=UpdateData
# GroupVariables.=GroupVariables
# CalendarFeatures.=CalendarFeatures
# CalendarVariables.=CalendarVariables
# GroupVarVector.=GroupVarVector
# DateColumnName.=DateColumnName
# XREGS.=XREGS
# FourierTerms.=FourierTerms
# FourierFC.=FourierFC
# TimeGroups.=TimeGroups
# TimeTrendVariable.=TimeTrendVariable
# N.=N
# TargetColumnName.=TargetColumnName
# HolidayVariable.=HolidayVariable
# HolidayLookback.=HolidayLookback
# TimeUnit.=TimeUnit
# AnomalyDetection.=AnomalyDetection
# i.=i

