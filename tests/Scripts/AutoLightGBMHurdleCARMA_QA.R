# Collection data.table
QA_Results <- data.table::CJ(
  Group = c(0,1,2,3),
  xregs = c(0,1,2,3),
  TOF = c(TRUE, FALSE),
  Trans = c(TRUE, FALSE))

# Other tests
QA_Results[, TimeWeights := data.table::fifelse(runif(.N) < 0.5, 0.9999, 1)]
QA_Results[, Success := "Failure"]
QA_Results[, RunTime := 123.456]
QA_Results[, DateTime := Sys.time()]

# run = 16
for(run in seq_len(QA_Results[,.N])) {

  # Data
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
  weights <- QA_Results[run, TimeWeights]
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
  data[, Weekly_Sales := data.table::fifelse(runif(.N) < 0.35, 0, Weekly_Sales)]
  data1 <- data.table::copy(data)
  if(QA_Results[run, xregs] != 0L) xregs1 <- data.table::copy(xregs) else xregs1 <- NULL

  # Start Timer
  Start <- Sys.time()

  # Build forecast
  TestModel <- tryCatch({RemixAutoML::AutoLightGBMHurdleCARMA(

    # data args
    data = data1,
    XREGS = xregs1,
    TargetColumnName = "Weekly_Sales",
    DateColumnName = "Date",
    HierarchGroups = NULL,
    GroupVariables = groupvariables,
    EncodingMethod = 'credibility',
    TimeWeights = weights,
    TimeUnit = "weeks",
    TimeGroups = c("weeks","months"),

    # Production args
    TrainOnFull = TOF,
    SplitRatios = c(1 - 10 / 110, 10 / 110),
    PartitionType = "random",
    FC_Periods = 4,
    Timer = TRUE,
    DebugMode = TRUE,

    # Target transformations
    TargetTransformation = Trans,
    Methods = c("Asinh", "Asin", "Log", "LogPlus1", "Sqrt", "Logit"),
    Difference = FALSE,
    NonNegativePred = FALSE,
    Threshold = NULL,
    RoundPreds = FALSE,

    # Date features
    CalendarVariables = c("week", "wom", "month", "quarter"),
    HolidayVariable = c("USPublicHolidays", "EasterGroup", "ChristmasGroup","OtherEcclesticalFeasts"),
    HolidayLookback = NULL,
    HolidayLags = 1,
    HolidayMovingAverages = 1:2,

    # Time series features
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

    # ML Args
    NumOfParDepPlots = 3L,
    EvalMetric = "RMSE",
    GridTune = FALSE,
    PassInGrid = NULL,
    ModelCount = 5,
    MaxRunsWithoutNewWinner = 50,
    MaxRunMinutes = 60*60,

    # Core parameters https://lightgbm.readthedocs.io/en/latest/Parameters.html#core-parameters
    input_model = list('classifier' = NULL, 'regression' = NULL),
    task = list('classifier' = 'train', 'regression' = 'train'),
    device_type = list('classifier' = 'CPU', 'regression' = 'CPU'),
    objective = list('classifier' = 'binary', 'regression' = 'regression'),
    metric = list('classifier' = 'binary_logloss', 'regression' = 'rmse'),
    boosting = list('classifier' = 'gbdt', 'regression' = 'gbdt'),
    LinearTree = list('classifier' = FALSE, 'regression' = FALSE),
    Trees = list('classifier' = 1000L, 'regression' = 1000L),
    eta = list('classifier' = NULL, 'regression' = NULL),
    num_leaves = list('classifier' = 31, 'regression' = 31),
    deterministic = list('classifier' = TRUE, 'regression' = TRUE),

    # Learning Parameters https://lightgbm.readthedocs.io/en/latest/Parameters.html#learning-control-parameters
    force_col_wise = list('classifier' = FALSE, 'regression' = FALSE),
    force_row_wise = list('classifier' = FALSE, 'regression' = FALSE),
    max_depth = list('classifier' = NULL, 'regression' = NULL),
    min_data_in_leaf = list('classifier' = 20, 'regression' = 20),
    min_sum_hessian_in_leaf = list('classifier' = 0.001, 'regression' = 0.001),
    bagging_freq = list('classifier' = 0, 'regression' = 0),
    bagging_fraction = list('classifier' = 1.0, 'regression' = 1.0),
    feature_fraction = list('classifier' = 1.0, 'regression' = 1.0),
    feature_fraction_bynode = list('classifier' = 1.0, 'regression' = 1.0),
    extra_trees = list('classifier' = FALSE, 'regression' = FALSE),
    early_stopping_round = list('classifier' = 10, 'regression' = 10),
    first_metric_only = list('classifier' = TRUE, 'regression' = TRUE),
    max_delta_step = list('classifier' = 0.0, 'regression' = 0.0),
    lambda_l1 = list('classifier' = 0.0, 'regression' = 0.0),
    lambda_l2 = list('classifier' = 0.0, 'regression' = 0.0),
    linear_lambda = list('classifier' = 0.0, 'regression' = 0.0),
    min_gain_to_split = list('classifier' = 0, 'regression' = 0),
    drop_rate_dart = list('classifier' = 0.10, 'regression' = 0.10),
    max_drop_dart = list('classifier' = 50, 'regression' = 50),
    skip_drop_dart = list('classifier' = 0.50, 'regression' = 0.50),
    uniform_drop_dart = list('classifier' = FALSE, 'regression' = FALSE),
    top_rate_goss = list('classifier' = FALSE, 'regression' = FALSE),
    other_rate_goss = list('classifier' = FALSE, 'regression' = FALSE),
    monotone_constraints = list('classifier' = NULL, 'regression' = NULL),
    monotone_constraints_method = list('classifier' = 'advanced', 'regression' = 'advanced'),
    monotone_penalty = list('classifier' = 0.0, 'regression' = 0.0),
    forcedsplits_filename = list('classifier' = NULL, 'regression' = NULL),
    refit_decay_rate = list('classifier' = 0.90, 'regression' = 0.90),
    path_smooth = list('classifier' = 0.0, 'regression' = 0.0),

    # IO Dataset Parameters
    max_bin = list('classifier' = 255, 'regression' = 255),
    min_data_in_bin = list('classifier' = 3, 'regression' = 3),
    data_random_seed = list('classifier' = 1, 'regression' = 1),
    is_enable_sparse = list('classifier' = TRUE, 'regression' = TRUE),
    enable_bundle = list('classifier' = TRUE, 'regression' = TRUE),
    use_missing = list('classifier' = TRUE, 'regression' = TRUE),
    zero_as_missing = list('classifier' = FALSE, 'regression' = FALSE),
    two_round = list('classifier' = FALSE, 'regression' = FALSE),

    # Convert Parameters
    convert_model = list('classifier' = NULL, 'regression' = NULL),
    convert_model_language = list('classifier' = "cpp", 'regression' = "cpp"),

    # Objective Parameters
    boost_from_average = list('classifier' = TRUE, 'regression' = TRUE),
    is_unbalance = list('classifier' = FALSE, 'regression' = FALSE),
    scale_pos_weight = list('classifier' = 1.0, 'regression' = 1.0),

    # Metric Parameters (metric is in Core)
    is_provide_training_metric = list('classifier' = TRUE, 'regression' = TRUE),
    eval_at = list('classifier' = c(1,2,3,4,5), 'regression' = c(1,2,3,4,5)),

    # Network Parameters
    num_machines = list('classifier' = 1, 'regression' = 1),

    # GPU Parameters
    gpu_platform_id = list('classifier' = -1, 'regression' = -1),
    gpu_device_id = list('classifier' = -1, 'regression' = -1),
    gpu_use_dp = list('classifier' = TRUE, 'regression' = TRUE),
    num_gpu = list('classifier' = 1, 'regression' = 1))}, error = function(x) NULL)

  # Timer
  End <- Sys.time()
  QA_Results[run, RunTimeTrain := as.numeric(difftime(time1 = End, Start))]

  # Outcome
  if(!is.null(TestModel)) QA_Results[run, Success := "Success"]
  TestModel <- NULL
  RemixAutoML:::Post_Append_Helper(QA_Results,'AutoLightGBMHurdleCARMA_QA')
  Sys.sleep(5)
}

# Hurdle QA Defaults ----
# library(RemixAutoML)
# library(data.table)
# library(lubridate)
#
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/FeatureEngineering_CalendarTypes.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/FeatureEngineering_CrossRowOperations.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/FeatureEngineering_CharacterTypes.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/CARMA-HelperFunctions.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ReinforcementLearningFunctions.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/MiscFunctions.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ModelEvaluationPlots.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/LightGBMHelpers.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/XGBoostHelpers.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ModelMetrics.R"))
#
# # Collection data.table
# QA_Results <- data.table::CJ(
#   Group = c(0,1,2,3),
#   xregs = c(0,1,2,3),
#   TOF = c(TRUE, FALSE),
#   Trans = c(TRUE, FALSE))
#
# # Other tests
# QA_Results[, TimeWeights := data.table::fifelse(runif(.N) < 0.5, 0.9999, 1)]
# QA_Results[, Success := "Failure"]
# QA_Results[, RunTime := 123.456]
# QA_Results[, DateTime := Sys.time()]
#
# run = 17
#
# # Data
# if(QA_Results[run, Group] == 0L) {
#   data <- RemixAutoML:::Post_Query_Helper('"nogroupevalwalmart.csv"')[['data']]
# } else if(QA_Results[run, Group] == 1L) {
#   data <- RemixAutoML:::Post_Query_Helper('"onegroupevalwalmart.csv"')[['data']]
# } else if(QA_Results[run, Group] == 2L) {
#   data <- RemixAutoML:::Post_Query_Helper('"twogroupevalwalmart.csv"')[['data']]
# } else if(QA_Results[run, Group] == 3L) {
#   data <- RemixAutoML:::Post_Query_Helper('"threegroupevalwalmart.csv"')[['data']]
# }
#
# # xregs
# if(QA_Results[run, xregs] == 0L) {
#   xregs <- NULL
# } else if(QA_Results[run, xregs] == 1L) {
#   if(QA_Results[run, Group] == 0L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("nogroupfcwalmartxreg1.csv"))[['data']]
#   if(QA_Results[run, Group] == 1L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("onegroupfcwalmartxreg1.csv"))[['data']]
#   if(QA_Results[run, Group] == 2L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("twogroupfcwalmartxreg1.csv"))[['data']]
#   if(QA_Results[run, Group] == 3L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("threegroupfcwalmartxreg1.csv"))[['data']]
# } else if(QA_Results[run, xregs] == 2L) {
#   if(QA_Results[run, Group] == 0L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("nogroupfcwalmartxreg2.csv"))[['data']]
#   if(QA_Results[run, Group] == 1L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("onegroupfcwalmartxreg2.csv"))[['data']]
#   if(QA_Results[run, Group] == 2L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("twogroupfcwalmartxreg2.csv"))[['data']]
#   if(QA_Results[run, Group] == 3L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("threegroupfcwalmartxreg2.csv"))[['data']]
# } else if(QA_Results[run, xregs] == 3L) {
#   if(QA_Results[run, Group] == 0L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("nogroupfcwalmartxreg3.csv"))[['data']]
#   if(QA_Results[run, Group] == 1L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("onegroupfcwalmartxreg3.csv"))[['data']]
#   if(QA_Results[run, Group] == 2L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("twogroupfcwalmartxreg3.csv"))[['data']]
#   if(QA_Results[run, Group] == 3L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("threegroupfcwalmartxreg3.csv"))[['data']]
# }
#
# # Testing params
# TOF <- QA_Results[run, TOF]
# Trans <- QA_Results[run, Trans]
# weights <- QA_Results[run, TimeWeights]
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
# data[, Weekly_Sales := data.table::fifelse(runif(.N) < 0.35, 0, Weekly_Sales)]
# data1 <- data.table::copy(data)
# if(QA_Results[run, xregs] != 0L) xregs1 <- data.table::copy(xregs) else xregs1 <- NULL
#
# # Start Timer
# Start <- Sys.time()
#
# # data args
# data = data1
# XREGS = xregs1
# TargetColumnName = "Weekly_Sales"
# DateColumnName = "Date"
# HierarchGroups = NULL
# GroupVariables = groupvariables
# EncodingMethod = 'credibility'
# TimeWeights = weights
# TimeUnit = "weeks"
# TimeGroups = c("weeks","months")
# TrainOnFull = TOF
# SplitRatios = c(1 - 10 / 110, 10 / 110)
# PartitionType = "random"
# FC_Periods = 4
# Timer = TRUE
# DebugMode = TRUE
# TargetTransformation = Trans
# Methods = c("Asinh", "Asin", "Log", "LogPlus1", "Sqrt", "Logit")
# Difference = FALSE
# NonNegativePred = FALSE
# Threshold = NULL
# RoundPreds = FALSE
# CalendarVariables = c("week", "wom", "month", "quarter")
# HolidayVariable = c("USPublicHolidays", "EasterGroup", "ChristmasGroup","OtherEcclesticalFeasts")
# HolidayLookback = NULL
# HolidayLags = 1
# HolidayMovingAverages = 1:2
# Lags = list("weeks" = c(1:5), "months" = c(1:3))
# MA_Periods = list("weeks" = c(2:5), "months" = c(2,3))
# SD_Periods = NULL
# Skew_Periods = NULL
# Kurt_Periods = NULL
# Quantile_Periods = NULL
# Quantiles_Selected = NULL
# AnomalyDetection = NULL
# FourierTerms = 0
# TimeTrendVariable = TRUE
# ZeroPadSeries = NULL
# DataTruncate = FALSE
# NumOfParDepPlots = 3L
# EvalMetric = "RMSE"
# GridTune = FALSE
# PassInGrid = NULL
# ModelCount = 5
# MaxRunsWithoutNewWinner = 50
# MaxRunMinutes = 60*60
# input_model = list('classifier' = NULL, 'regression' = NULL)
# task = list('classifier' = 'train', 'regression' = 'train')
# device_type = list('classifier' = 'CPU', 'regression' = 'CPU')
# objective = list('classifier' = 'binary', 'regression' = 'regression')
# metric = list('classifier' = 'binary_logloss', 'regression' = 'rmse')
# boosting = list('classifier' = 'gbdt', 'regression' = 'gbdt')
# LinearTree = list('classifier' = FALSE, 'regression' = FALSE)
# Trees = list('classifier' = 1000L, 'regression' = 1000L)
# eta = list('classifier' = NULL, 'regression' = NULL)
# num_leaves = list('classifier' = 31, 'regression' = 31)
# deterministic = list('classifier' = TRUE, 'regression' = TRUE)
# force_col_wise = list('classifier' = FALSE, 'regression' = FALSE)
# force_row_wise = list('classifier' = FALSE, 'regression' = FALSE)
# max_depth = list('classifier' = NULL, 'regression' = NULL)
# min_data_in_leaf = list('classifier' = 20, 'regression' = 20)
# min_sum_hessian_in_leaf = list('classifier' = 0.001, 'regression' = 0.001)
# bagging_freq = list('classifier' = 0, 'regression' = 0)
# bagging_fraction = list('classifier' = 1.0, 'regression' = 1.0)
# feature_fraction = list('classifier' = 1.0, 'regression' = 1.0)
# feature_fraction_bynode = list('classifier' = 1.0, 'regression' = 1.0)
# extra_trees = list('classifier' = FALSE, 'regression' = FALSE)
# early_stopping_round = list('classifier' = 10, 'regression' = 10)
# first_metric_only = list('classifier' = TRUE, 'regression' = TRUE)
# max_delta_step = list('classifier' = 0.0, 'regression' = 0.0)
# lambda_l1 = list('classifier' = 0.0, 'regression' = 0.0)
# lambda_l2 = list('classifier' = 0.0, 'regression' = 0.0)
# linear_lambda = list('classifier' = 0.0, 'regression' = 0.0)
# min_gain_to_split = list('classifier' = 0, 'regression' = 0)
# drop_rate_dart = list('classifier' = 0.10, 'regression' = 0.10)
# max_drop_dart = list('classifier' = 50, 'regression' = 50)
# skip_drop_dart = list('classifier' = 0.50, 'regression' = 0.50)
# uniform_drop_dart = list('classifier' = FALSE, 'regression' = FALSE)
# top_rate_goss = list('classifier' = FALSE, 'regression' = FALSE)
# other_rate_goss = list('classifier' = FALSE, 'regression' = FALSE)
# monotone_constraints = list('classifier' = NULL, 'regression' = NULL)
# monotone_constraints_method = list('classifier' = 'advanced', 'regression' = 'advanced')
# monotone_penalty = list('classifier' = 0.0, 'regression' = 0.0)
# forcedsplits_filename = list('classifier' = NULL, 'regression' = NULL)
# refit_decay_rate = list('classifier' = 0.90, 'regression' = 0.90)
# path_smooth = list('classifier' = 0.0, 'regression' = 0.0)
# max_bin = list('classifier' = 255, 'regression' = 255)
# min_data_in_bin = list('classifier' = 3, 'regression' = 3)
# data_random_seed = list('classifier' = 1, 'regression' = 1)
# is_enable_sparse = list('classifier' = TRUE, 'regression' = TRUE)
# enable_bundle = list('classifier' = TRUE, 'regression' = TRUE)
# use_missing = list('classifier' = TRUE, 'regression' = TRUE)
# zero_as_missing = list('classifier' = FALSE, 'regression' = FALSE)
# two_round = list('classifier' = FALSE, 'regression' = FALSE)
# convert_model = list('classifier' = NULL, 'regression' = NULL)
# convert_model_language = list('classifier' = "cpp", 'regression' = "cpp")
# boost_from_average = list('classifier' = TRUE, 'regression' = TRUE)
# is_unbalance = list('classifier' = FALSE, 'regression' = FALSE)
# scale_pos_weight = list('classifier' = 1.0, 'regression' = 1.0)
# is_provide_training_metric = list('classifier' = TRUE, 'regression' = TRUE)
# eval_at = list('classifier' = c(1,2,3,4,5), 'regression' = c(1,2,3,4,5))
# num_machines = list('classifier' = 1, 'regression' = 1)
# gpu_platform_id = list('classifier' = -1, 'regression' = -1)
# gpu_device_id = list('classifier' = -1, 'regression' = -1)
# gpu_use_dp = list('classifier' = TRUE, 'regression' = TRUE)
# num_gpu = list('classifier' = 1, 'regression' = 1)

# Hurdle Model ----
# ModelID = 'ModelTest'
# SaveModelObjects = FALSE
# ReturnModelObjects = TRUE
# data = data.table::copy(train)
# TrainOnFull = TrainOnFull
# ValidationData = data.table::copy(valid)
# TestData = data.table::copy(test)
# Buckets = 0L
# TargetColumnName = TargetVariable
# FeatureColNames = ModelFeatures
# PrimaryDateColumn = eval(DateColumnName)
# WeightsColumnName = if('Weights' %chin% names(train)) 'Weights' else NULL
# IDcols = IDcols
# EncodingMethod = EncodingMethod
# DebugMode = DebugMode
# Paths = getwd()
# MetaDataPaths = NULL
# TransformNumericColumns = NULL
# Methods = NULL
# ClassWeights = c(1,1)
# SplitRatios = c(0.70, 0.20, 0.10)
# NumOfParDepPlots = NumOfParDepPlots
# PassInGrid = PassInGrid
# GridTune = GridTune
# BaselineComparison = 'default'
# MaxModelsInGrid = 500L
# MaxRunsWithoutNewWinner = 100L
# MaxRunMinutes = 60*60
# input_model = input_model
# task = task
# device_type = device_type
# objective = objective
# metric = metric
# boosting = boosting
# LinearTree = LinearTree
# Trees = Trees
# eta = eta
# num_leaves = num_leaves
# deterministic = deterministic
# force_col_wise = force_col_wise
# force_row_wise = force_row_wise
# max_depth = max_depth
# min_data_in_leaf = min_data_in_leaf
# min_sum_hessian_in_leaf = min_sum_hessian_in_leaf
# bagging_freq = bagging_freq
# bagging_fraction = bagging_fraction
# feature_fraction = feature_fraction
# feature_fraction_bynode = feature_fraction_bynode
# extra_trees = extra_trees
# early_stopping_round = early_stopping_round
# first_metric_only = first_metric_only
# max_delta_step = max_delta_step
# lambda_l1 = lambda_l1
# lambda_l2 = lambda_l2
# linear_lambda = linear_lambda
# min_gain_to_split = min_gain_to_split
# drop_rate_dart = drop_rate_dart
# max_drop_dart = max_drop_dart
# skip_drop_dart = skip_drop_dart
# uniform_drop_dart = uniform_drop_dart
# top_rate_goss = top_rate_goss
# other_rate_goss = other_rate_goss
# monotone_constraints = monotone_constraints
# monotone_constraints_method = monotone_constraints_method
# monotone_penalty = monotone_penalty
# forcedsplits_filename = forcedsplits_filename
# refit_decay_rate = refit_decay_rate
# path_smooth = path_smooth
# max_bin = max_bin
# min_data_in_bin = min_data_in_bin
# data_random_seed = data_random_seed
# is_enable_sparse = is_enable_sparse
# enable_bundle = enable_bundle
# use_missing = use_missing
# zero_as_missing = zero_as_missing
# two_round = two_round
# convert_model = convert_model
# convert_model_language = convert_model_language
# boost_from_average = boost_from_average
# is_unbalance = is_unbalance
# scale_pos_weight = scale_pos_weight
# is_provide_training_metric = is_provide_training_metric
# eval_at = eval_at
# num_machines = num_machines
# gpu_platform_id = gpu_platform_id
# gpu_device_id = gpu_device_id
# gpu_use_dp = gpu_use_dp
# num_gpu = num_gpu
# NThreads = 6


# Hurdle Regression
# OutputSelection = c("Importances","EvalMetrics")
# PrimaryDateColumn = PrimaryDateColumn
# WeightsColumnName = WeightsColumnName
# DebugMode = DebugMode
# SaveInfoToPDF = FALSE
# NThreads = NThreads
# model_path = Paths
# metadata_path = MetaDataPaths
# ModelID = ModelIDD
# ReturnFactorLevels = TRUE
# ReturnModelObjects = ReturnModelObjects
# SaveModelObjects = SaveModelObjects
# Verbose = 1L
# EncodingMethod = EncodingMethod
# data = data.table::copy(trainBucket)
# TrainOnFull = TrainOnFull
# ValidationData = data.table::copy(validBucket)
# TestData = data.table::copy(testBucket)
# TargetColumnName = TargetColumnName
# FeatureColNames = FeatureNames
# IDcols = IDcolsModified
# TransformNumericColumns = TransformNumericColumns
# Methods = Methods
# grid_eval_metric = "mse"
# NumOfParDepPlots = NumOfParDepPlots
# PassInGrid = PassInGrid
# GridTune = GridTune
# MaxModelsInGrid = MaxModelsInGrid
# BaselineComparison = "default"
# MaxRunsWithoutNewWinner = 20L
# MaxRunMinutes = 60*60
# input_model = Classifierinput_model
# task = Regressiontask
# device_type = Regressiondevice_type
# objective = Regressionobjective
# metric = Regressionmetric
# boosting = Regressionboosting
# LinearTree = RegressionLinearTree
# Trees = RegressionTrees
# eta = Regressioneta
# num_leaves = Regressionnum_leaves
# deterministic = Regressiondeterministic
# force_col_wise = Regressionforce_col_wise
# force_row_wise = Regressionforce_row_wise
# max_depth = Regressionmax_depth
# min_data_in_leaf = Regressionmin_data_in_leaf
# min_sum_hessian_in_leaf = Regressionmin_sum_hessian_in_leaf
# bagging_freq = Regressionbagging_freq
# bagging_fraction = Regressionbagging_fraction
# feature_fraction = Regressionfeature_fraction
# feature_fraction_bynode = Regressionfeature_fraction_bynode
# lambda_l1 = Regressionlambda_l1
# lambda_l2 = Regressionlambda_l2
# extra_trees = Regressionextra_trees
# early_stopping_round = Regressionearly_stopping_round
# first_metric_only = Regressionfirst_metric_only
# max_delta_step = Regressionmax_delta_step
# linear_lambda = Regressionlinear_lambda
# min_gain_to_split = Regressionmin_gain_to_split
# drop_rate_dart = Regressiondrop_rate_dart
# max_drop_dart = Regressionmax_drop_dart
# skip_drop_dart = Regressionskip_drop_dart
# uniform_drop_dart = Regressionuniform_drop_dart
# top_rate_goss = Regressiontop_rate_goss
# other_rate_goss = Regressionother_rate_goss
# monotone_constraints = Regressionmonotone_constraints
# monotone_constraints_method = Regressionmonotone_constraints_method
# monotone_penalty = Regressionmonotone_penalty
# forcedsplits_filename = Regressionforcedsplits_filename
# refit_decay_rate = Regressionrefit_decay_rate
# path_smooth = Regressionpath_smooth
# max_bin = Regressionmax_bin
# min_data_in_bin = Regressionmin_data_in_bin
# data_random_seed = Regressiondata_random_seed
# is_enable_sparse = Regressionis_enable_sparse
# enable_bundle = Regressionenable_bundle
# use_missing = Regressionuse_missing
# zero_as_missing = Regressionzero_as_missing
# two_round = Regressiontwo_round
# convert_model = Regressionconvert_model
# convert_model_language = Regressionconvert_model_language
# boost_from_average = Regressionboost_from_average
# alpha = NULL
# fair_c = NULL
# poisson_max_delta_step = NULL
# tweedie_variance_power = NULL
# lambdarank_truncation_level = NULL
# is_provide_training_metric = Regressionis_provide_training_metric
# eval_at = Regressioneval_at
# num_machines = Regressionnum_machines
# gpu_platform_id = Regressiongpu_platform_id
# gpu_device_id = Regressiongpu_device_id
# gpu_use_dp = Regressiongpu_use_dp
# num_gpu = Regressionnum_gpu


# Regression DataPrep inside AutoLightGBMRegression()
# Algo='lightgbm'
# ModelType='regression'
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
# DebugMode. = TRUE


# Regression Model first pass
# OutputSelection = c("Importances","EvalMetrics")
# PrimaryDateColumn = PrimaryDateColumn
# WeightsColumnName = WeightsColumnName
# DebugMode = DebugMode
# SaveInfoToPDF = FALSE
# NThreads = NThreads
# model_path = Paths
# metadata_path = MetaDataPaths
# ModelID = ModelIDD
# ReturnFactorLevels = TRUE
# ReturnModelObjects = ReturnModelObjects
# SaveModelObjects = SaveModelObjects
# Verbose = 1L
# EncodingMethod = EncodingMethod
# data = data.table::copy(trainBucket)
# TrainOnFull = TrainOnFull
# ValidationData = data.table::copy(validBucket)
# TestData = data.table::copy(testBucket)
# TargetColumnName = TargetColumnName
# FeatureColNames = FeatureNames
# IDcols = IDcolsModified
# TransformNumericColumns = TransformNumericColumns
# Methods = Methods
# grid_eval_metric = "mse"
# NumOfParDepPlots = NumOfParDepPlots
# PassInGrid = PassInGrid
# GridTune = GridTune
# MaxModelsInGrid = MaxModelsInGrid
# BaselineComparison = "default"
# MaxRunsWithoutNewWinner = 20L
# MaxRunMinutes = 60*60
# input_model = Classifierinput_model
# task = Regressiontask
# device_type = Regressiondevice_type
# objective = Regressionobjective
# metric = Regressionmetric
# boosting = Regressionboosting
# LinearTree = RegressionLinearTree
# Trees = RegressionTrees
# eta = Regressioneta
# num_leaves = Regressionnum_leaves
# deterministic = Regressiondeterministic
# force_col_wise = Regressionforce_col_wise
# force_row_wise = Regressionforce_row_wise
# max_depth = Regressionmax_depth
# min_data_in_leaf = Regressionmin_data_in_leaf
# min_sum_hessian_in_leaf = Regressionmin_sum_hessian_in_leaf
# bagging_freq = Regressionbagging_freq
# bagging_fraction = Regressionbagging_fraction
# feature_fraction = Regressionfeature_fraction
# feature_fraction_bynode = Regressionfeature_fraction_bynode
# lambda_l1 = Regressionlambda_l1
# lambda_l2 = Regressionlambda_l2
# extra_trees = Regressionextra_trees
# early_stopping_round = Regressionearly_stopping_round
# first_metric_only = Regressionfirst_metric_only
# max_delta_step = Regressionmax_delta_step
# linear_lambda = Regressionlinear_lambda
# min_gain_to_split = Regressionmin_gain_to_split
# drop_rate_dart = Regressiondrop_rate_dart
# max_drop_dart = Regressionmax_drop_dart
# skip_drop_dart = Regressionskip_drop_dart
# uniform_drop_dart = Regressionuniform_drop_dart
# top_rate_goss = Regressiontop_rate_goss
# other_rate_goss = Regressionother_rate_goss
# monotone_constraints = Regressionmonotone_constraints
# monotone_constraints_method = Regressionmonotone_constraints_method
# monotone_penalty = Regressionmonotone_penalty
# forcedsplits_filename = Regressionforcedsplits_filename
# refit_decay_rate = Regressionrefit_decay_rate
# path_smooth = Regressionpath_smooth
# max_bin = Regressionmax_bin
# min_data_in_bin = Regressionmin_data_in_bin
# data_random_seed = Regressiondata_random_seed
# is_enable_sparse = Regressionis_enable_sparse
# enable_bundle = Regressionenable_bundle
# use_missing = Regressionuse_missing
# zero_as_missing = Regressionzero_as_missing
# two_round = Regressiontwo_round
# convert_model = Regressionconvert_model
# convert_model_language = Regressionconvert_model_language
# boost_from_average = Regressionboost_from_average
# alpha = NULL
# fair_c = NULL
# poisson_max_delta_step = NULL
# tweedie_variance_power = NULL
# lambdarank_truncation_level = NULL
# is_provide_training_metric = Regressionis_provide_training_metric
# eval_at = Regressioneval_at
# num_machines = Regressionnum_machines
# gpu_platform_id = Regressiongpu_platform_id
# gpu_device_id = Regressiongpu_device_id
# gpu_use_dp = Regressiongpu_use_dp
# num_gpu = Regressionnum_gpu




# ----

# ----
