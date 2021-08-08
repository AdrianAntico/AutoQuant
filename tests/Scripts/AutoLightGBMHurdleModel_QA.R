# Test data.table
LightGBM_QA <- data.table::CJ(
  TOF = c(TRUE,FALSE),
  Classification = c(TRUE,FALSE),
  Success = "Failure",
  ScoreSuccess = "Failure",
  PartitionInFunction = c(TRUE,FALSE), sorted = FALSE
)

# Remove impossible combinations
LightGBM_QA <- LightGBM_QA[!(PartitionInFunction & TOF)]
LightGBM_QA[, RunNumber := seq_len(.N)]

# Path File
Path <- "C:/Users/Bizon/Documents/GitHub/QA_Code/QA_CSV"

#      TOF Classification Success PartitionInFunction RunNumber
# 1:  TRUE           TRUE Failure               FALSE         1
# 2:  TRUE          FALSE Failure               FALSE         2
# 3: FALSE           TRUE Failure                TRUE         3
# 4: FALSE           TRUE Failure               FALSE         4
# 5: FALSE          FALSE Failure                TRUE         5
# 6: FALSE          FALSE Failure               FALSE         6

# AutoCatBoostHurdleModel
# run = 1
# run = 6
for(run in seq_len(LightGBM_QA[,.N])) {

  # Define values
  tof <- LightGBM_QA[run, TOF]
  PartitionInFunction <- LightGBM_QA[run, PartitionInFunction]
  Classify <- LightGBM_QA[run, Classification]
  Tar <- "Adrian"

  # Get data
  if(Classify) {
    data <- RemixAutoML::FakeDataGenerator(N = 15000, ZIP = 1)
  } else {
    data <- RemixAutoML::FakeDataGenerator(N = 100000, ZIP = 2)
  }

  # Partition Data
  if(!tof && !PartitionInFunction) {
    Sets <- RemixAutoML::AutoDataPartition(
      data = data,
      NumDataSets = 3,
      Ratios = c(0.7,0.2,0.1),
      PartitionType = "random",
      StratifyColumnNames = "Adrian",
      TimeColumnName = NULL)
    TTrainData <- Sets$TrainData
    VValidationData <- Sets$ValidationData
    TTestData <- Sets$TestData
    rm(Sets)
  } else {
    TTrainData <- data.table::copy(data)
    VValidationData <- NULL
    TTestData <- NULL
  }

  # Run function
  TestModel <- tryCatch({RemixAutoML::AutoLightGBMHurdleModel(

    # Operationalization
    ModelID = 'ModelTest',
    SaveModelObjects = FALSE,
    ReturnModelObjects = TRUE,
    NThreads = parallel::detectCores(),

    # Data related args
    data = TTrainData,
    ValidationData = VValidationData,
    PrimaryDateColumn = "DateTime",
    TestData = TTestData,
    WeightsColumnName = NULL,
    TrainOnFull = tof,
    Buckets = if(Classify) 0L else c(0,2,3),
    TargetColumnName = "Adrian",
    FeatureColNames = names(TTrainData)[!names(data) %in% c("Adrian","IDcol_1","IDcol_2","IDcol_3","IDcol_4","IDcol_5","DateTime")],
    IDcols = c("IDcol_1","IDcol_2","IDcol_3","IDcol_4","IDcol_5","DateTime"),
    DebugMode = TRUE,

    # Metadata args
    EncodingMethod = "credibility",
    Paths = getwd(),
    MetaDataPaths = NULL,
    TransformNumericColumns = NULL,
    Methods = c('Asinh', 'Asin', 'Log', 'LogPlus1', 'Logit'),
    ClassWeights = c(1,1),
    SplitRatios = if(PartitionInFunction) c(0.70, 0.20, 0.10) else NULL,
    NumOfParDepPlots = 10L,

    # Grid tuning setup
    PassInGrid = NULL,
    GridTune = FALSE,
    BaselineComparison = 'default',
    MaxModelsInGrid = 1L,
    MaxRunsWithoutNewWinner = 20L,
    MaxRunMinutes = 60L*60L,

    # LightGBM parameters
    task = list('classifier' = 'train', 'regression' = 'train'),
    device_type = list('classifier' = 'CPU', 'regression' = 'CPU'),
    objective = if(Classify) list('classifier' = 'binary', 'regression' = 'regression') else list('classifier' = 'multiclass', 'regression' = 'regression'),
    metric = if(Classify) list('classifier' = 'binary_logloss', 'regression' = 'rmse') else list('classifier' = 'multi_logloss', 'regression' = 'rmse'),
    boosting = list('classifier' = 'gbdt', 'regression' = 'gbdt'),
    LinearTree = list('classifier' = FALSE, 'regression' = FALSE),
    Trees = list('classifier' = 50L, 'regression' = 50L),
    eta = list('classifier' = NULL, 'regression' = NULL),
    num_leaves = list('classifier' = 31, 'regression' = 31),
    deterministic = list('classifier' = TRUE, 'regression' = TRUE),

    # Learning Parameters
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

  # Outcome
  if(!is.null(TestModel)) LightGBM_QA[run, Success := "Success"]
  data.table::fwrite(LightGBM_QA, file = "C:/Users/Bizon/Documents/GitHub/QA_Code/QA_CSV/AutoLightGBMHurdleModel_QA.csv")

  # Remove Target Variable
  TTrainData[, c("Target_Buckets", "Adrian") := NULL]

  # Score CatBoost Hurdle Model
  Output <- tryCatch({RemixAutoML::AutoLightGBMHurdleModelScoring(
    TestData = TTrainData,
    Path = Path,
    ModelID = "ModelTest",
    ModelList = TestModel$ModelList,
    ArgsList = TestModel$ArgsList,
    Threshold = NULL)}, error = function(x) NULL)

  # Outcome
  if(!is.null(Output)) LightGBM_QA[run, Score := "Success"]
  TestModel <- NULL
  Output <- NULL
  TTrainData <- NULL
  VValidationData <- NULL
  TTestData <- NULL
  gc(); Sys.sleep(5)
  data.table::fwrite(LightGBM_QA, file = file.path(Path, "AutoLightGBMHurdleModel_QA.csv"))
}

# Defaults ----
# library(RemixAutoML)
# library(data.table)
#
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/FeatureEngineering_CharacterTypes.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/MiscFunctions.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/CatBoostHelpers.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/XGBoostHelpers.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/LightGBMHelpers.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ModelMetrics.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ModelEvaluationPlots.R"))

#
# run = 5
#
# tof <- LightGBM_QA[run, TOF]
# PartitionInFunction <- LightGBM_QA[run, PartitionInFunction]
# Classify <- LightGBM_QA[run, Classification]
# Tar <- "Adrian"
#
# # Get data
# if(Classify) {
#   data <- RemixAutoML::FakeDataGenerator(N = 15000, ZIP = 1)
# } else {
#   data <- RemixAutoML::FakeDataGenerator(N = 500000, ZIP = 2)
# }
#
# # Partition Data
# if(!tof && !PartitionInFunction) {
#   Sets <- RemixAutoML::AutoDataPartition(
#     data = data,
#     NumDataSets = 3,
#     Ratios = c(0.7,0.2,0.1),
#     PartitionType = "random",
#     StratifyColumnNames = "Adrian",
#     TimeColumnName = NULL)
#   TTrainData <- Sets$TrainData
#   VValidationData <- Sets$ValidationData
#   TTestData <- Sets$TestData
#   rm(Sets)
# } else {
#   TTrainData <- data.table::copy(data)
#   VValidationData <- NULL
#   TTestData <- NULL
# }
# LightGBM parameters
# task = list('classifier' = 'train', 'regression' = 'train')
# device_type = list('classifier' = 'CPU', 'regression' = 'CPU')
# objective = list('classifier' = 'binary', 'regression' = 'regression')
# metric = list('classifier' = 'binary_logloss', 'regression' = 'rmse')
# boosting = list('classifier' = 'gbdt', 'regression' = 'gbdt')
# LinearTree = list('classifier' = FALSE, 'regression' = FALSE)
# Trees = list('classifier' = 50L, 'regression' = 50L)
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





