# Test data.table
XGBoost_QA <- data.table::CJ(
  TOF = c(TRUE,FALSE),
  Classification = c(TRUE,FALSE),
  Success = "Failure",
  ScoreSuccess = "Failure",
  PartitionInFunction = c(TRUE,FALSE), sorted = FALSE
)

# Remove impossible combinations
XGBoost_QA <- XGBoost_QA[!(PartitionInFunction & TOF)]
XGBoost_QA[, RunNumber := seq_len(.N)]

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
# run = 5
# run = 6
for(run in seq_len(XGBoost_QA[,.N])) {

  # Define values
  tof <- XGBoost_QA[run, TOF]
  PartitionInFunction <- XGBoost_QA[run, PartitionInFunction]
  Classify <- XGBoost_QA[run, Classification]
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
  TestModel <- tryCatch({RemixAutoML::AutoXGBoostHurdleModel(

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
    Paths = normalizePath('./'),
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

    # XGBoost parameters
    TreeMethod = "hist",
    Trees = list("classifier" = 50, "regression" = 50),
    eta = list("classifier" = 0.05, "regression" = 0.05),
    max_depth = list("classifier" = 4L, "regression" = 4L),
    min_child_weight = list("classifier" = 1.0, "regression" = 1.0),
    subsample = list("classifier" = 0.55, "regression" = 0.55),
    colsample_bytree = list("classifier" = 0.55, "regression" = 0.55))}, error = function(x) NULL)

  # Outcome
  if(!is.null(TestModel)) XGBoost_QA[run, Success := "Success"]
  data.table::fwrite(XGBoost_QA, file = "C:/Users/Bizon/Documents/GitHub/QA_Code/QA_CSV/AutoXGBoostHurdleModel_QA.csv")

  # Remove Target Variable
  TTrainData[, c("Target_Buckets", "Adrian") := NULL]

  # Score CatBoost Hurdle Model
  Output <- tryCatch({RemixAutoML::AutoXGBoostHurdleModelScoring(
    TestData = TTrainData,
    Path = Path,
    ModelID = "ModelTest",
    ModelList = TestModel$ModelList,
    ArgsList = TestModel$ArgsList,
    Threshold = NULL)}, error = function(x) NULL)

  # Outcome
  if(!is.null(Output)) XGBoost_QA[run, Score := "Success"]
  TestModel <- NULL
  Output <- NULL
  TTrainData <- NULL
  VValidationData <- NULL
  TTestData <- NULL
  gc(); Sys.sleep(5)
  data.table::fwrite(XGBoost_QA, file = file.path(Path, "AutoXGBoostHurdleModel_QA.csv"))
}

# Defaults ----
# library(RemixAutoML)
# library(data.table)
#
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/FeatureEngineering_CharacterTypes.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/MiscFunctions.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/CatBoostHelpers.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/XGBoostHelpers.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ModelMetrics.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ModelEvaluationPlots.R"))

# Scoring Hurdle ----
# TTrainData[, c("Target_Buckets", "Adrian") := NULL]
# TestData = TTrainData
# Path = Path
# ModelID = "ModelTest"
# ModelList = TestModel$ModelList
# ArgsList = TestModel$ArgsList
# Threshold = NULL

# Scoring Classification / Multiclass Scoring ----
# ScoringData = TestData
# FeatureColumnNames = FeatureNames
# IDcols = IDcols
# ModelObject = ClassModel
# ModelPath = ArgsList$Paths
# ModelID = ArgsList$ModelID
# EncodingMethod = ArgsList$EncodingMethod
# ReturnShapValues = FALSE
# FactorLevelsList = ArgsList$FactorLevelsList
# TargetLevels = ArgsList$TargetLevels
# OneHot = FALSE
# TargetType = TargetType
# ReturnFeatures = TRUE
# TransformationObject = NULL
# TargetColumnName = NULL
# TransformNumeric = FALSE
# BackTransNumeric = FALSE
# TransID = NULL
# TransPath = NULL
# MDP_Impute = FALSE
# MDP_CharToFactor = TRUE
# MDP_RemoveDates = FALSE
# MDP_MissFactor = "0"
# MDP_MissNum = -1

# Scoring Regression Scoring ----
# ScoringData = TestData
# FeatureColumnNames = FeatureNames
# IDcols = IDcolsModified
# ModelObject = RegressionModel
# ModelPath = NULL
# ModelID = ArgsList$ModelID
# EncodingMethod = ArgsList$EncodingMethod
# ReturnShapValues = FALSE
# FactorLevelsList = ArgsList[[paste0(ModelIDD, "_FactorLevelsList")]]
# TargetLevels = NULL
# OneHot = FALSE
# TargetType = "regression"
# ReturnFeatures = TRUE
# TransformationObject = NULL
# TargetColumnName = NULL
# TransformNumeric = if(is.null(ArgsList[[paste0("TransformationResults_", ModelIDD)]])) FALSE else TRUE
# BackTransNumeric = if(is.null(ArgsList[[paste0("TransformationResults_", ModelIDD)]])) FALSE else TRUE
# TransID = NULL
# TransPath = NULL
# MDP_Impute = FALSE
# MDP_CharToFactor = TRUE
# MDP_RemoveDates = FALSE
# MDP_MissFactor = "0"
# MDP_MissNum = -1

# # Define values ----
#
# run = 5
#
# tof <- XGBoost_QA[run, TOF]
# PartitionInFunction <- XGBoost_QA[run, PartitionInFunction]
# Classify <- XGBoost_QA[run, Classification]
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

# hurdle ----
# NThreads = parallel::detectCores()
# PrimaryDateColumn = "DateTime"
# ModelID = 'ModelTest'
# SaveModelObjects = FALSE
# ReturnModelObjects = TRUE
# data = TTrainData
# ValidationData = VValidationData
# TestData = TTestData
# WeightsColumnName = NULL
# TrainOnFull = tof
# Buckets = if(Classify) 0L else c(0,2,3)
# TargetColumnName = "Adrian"
# FeatureColNames = names(TTrainData)[!names(data) %in% c("Adrian","IDcol_1","IDcol_2","IDcol_3","IDcol_4","IDcol_5","DateTime")]
# IDcols = c("IDcol_1","IDcol_2","IDcol_3","IDcol_4","IDcol_5","DateTime")
# DebugMode = TRUE
# EncodingMethod = "credibility"
# Paths = normalizePath('./')
# MetaDataPaths = NULL
# TransformNumericColumns = NULL
# Methods = c('Asinh', 'Asin', 'Log', 'LogPlus1', 'Logit')
# ClassWeights = c(1,1)
# SplitRatios = if(PartitionInFunction) c(0.70, 0.20, 0.10) else NULL
# NumOfParDepPlots = 10L
# PassInGrid = NULL
# GridTune = FALSE
# BaselineComparison = 'default'
# MaxModelsInGrid = 1L
# MaxRunsWithoutNewWinner = 20L
# MaxRunMinutes = 60L*60L
# TreeMethod = "hist"
# Trees = list("classifier" = 50, "regression" = 50)
# eta = list("classifier" = 0.05, "regression" = 0.05)
# max_depth = list("classifier" = 4L, "regression" = 4L)
# min_child_weight = list("classifier" = 1.0, "regression" = 1.0)
# subsample = list("classifier" = 0.55, "regression" = 0.55)
# colsample_bytree = list("classifier" = 0.55, "regression" = 0.55)

# Regression XGBoost ----
# OutputSelection = c("Importances", "EvalPlots", "EvalMetrics", "Score_TrainData")
# PrimaryDateColumn = PrimaryDateColumn
# WeightsColumnName = WeightsColumnName
# DebugMode = DebugMode
# SaveInfoToPDF = FALSE
# TreeMethod = TreeMethod
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
# IDcols = IDcols
# TransformNumericColumns = TransformNumericColumns
# Methods = Methods
# eval_metric = "rmse"
# grid_eval_metric = "mse"
# NumOfParDepPlots = NumOfParDepPlots
# PassInGrid = PassInGrid
# GridTune = GridTune
# MaxModelsInGrid = MaxModelsInGrid
# BaselineComparison = "default"
# MaxRunsWithoutNewWinner = 20L
# MaxRunMinutes = 60*60
# Trees = RegressionTrees
# eta = Regressioneta
# max_depth = Regressionmax_depth
# min_child_weight = Regressionmin_child_weight
# subsample = Regressionsubsample
# colsample_bytree = Regressioncolsample_bytree

# XGBoost Regression Data Prep ----
# Algo="xgboost"
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

# XGBoost Regression Data Prep Encode Character Variables ----
# RunMode='train'
# ModelType=ModelType
# TrainData=dataTrain
# ValidationData=dataTest
# TestData=TestData.
# TargetVariableName=TargetColumnName.
# CategoricalVariableNames=CatFeatures
# EncodeMethod=EncodingMethod.
# KeepCategoricalVariables=FALSE
# ReturnMetaData=TRUE
# MetaDataPath=model_path.
# MetaDataList=NULL
# ImputeMissingValue=0

# Categorical Encoding ----
# data=temp_train
# ML_Type=ModelType
# GroupVariables=CategoricalVariableNames
# TargetVariable=TargetVariableName
# Method=EncodeMethod
# SavePath=MetaDataPath
# Scoring=Score
# ImputeValueScoring=ImputeMissingValue
# ReturnFactorLevelList=TRUE
# SupplyFactorLevelList=MetaDataList
# KeepOriginalFactors=KeepCategoricalVariables

# Multiclass XGBoost Scoring ----
# ReturnShapValues = FALSE
# EncodingMethod = EncodingMethod
# TargetType = TargetType
# ScoringData = if(!is.null(TestData)) data.table::copy(TestData) else if(!is.null(ValidationData)) data.table::copy(ValidationData) else data.table::copy(data)
# FeatureColumnNames = FeatureNames
# IDcols = c(IDcols, "Target_Buckets")
# FactorLevelsList = FactorLevelsList
# TargetLevels = TargetLevels
# OneHot = FALSE
# ModelObject = ClassModel
# ModelPath = if(!is.null(ClassModel)) NULL else Paths
# ModelID = ModelID
# ReturnFeatures = TRUE
# TransformNumeric = FALSE
# BackTransNumeric = FALSE
# TargetColumnName = NULL
# TransformationObject = NULL
# TransID = NULL
# TransPath = NULL
# MDP_Impute = TRUE
# MDP_CharToFactor = TRUE
# MDP_RemoveDates = TRUE
# MDP_MissFactor = "0"
# MDP_MissNum = -1

# Multiclass Categorical Encoding Scoring ----
# data=ScoringData
# ML_Type=TargetType
# GroupVariables=names(FactorLevelsList)
# TargetVariable=NULL
# Method=EncodingMethod
# SavePath=NULL
# Scoring=TRUE
# ImputeValueScoring=0
# ReturnFactorLevelList=FALSE
# SupplyFactorLevelList=FactorLevelsList
# KeepOriginalFactors=FALSE

# Regression Scoring for Multiclass case ----
# TargetType = "regression"
# ScoringData = TestData
# FeatureColumnNames = FeatureNames
# IDcols = IDcolsModified
# FactorLevelsList = FactorLevelsList
# EncodingMethod = "credibility"
# OneHot = FALSE
# ModelObject = RegressionModel
# ModelPath = NULL #Paths
# ModelID = ModelIDD
# ReturnFeatures = TRUE
# TransformNumeric = if(is.null(ArgsList[[paste0("TransformationResults_", ModelIDD)]])) FALSE else TRUE
# BackTransNumeric = if(is.null(ArgsList[[paste0("TransformationResults_", ModelIDD)]])) FALSE else TRUE
# TargetColumnName = NULL #eval(TargetColumnName)
# TransformationObject = TransformationResults
# TransID = NULL
# TransPath = NULL
# MDP_Impute = TRUE
# MDP_CharToFactor = TRUE
# MDP_RemoveDates = FALSE
# MDP_MissFactor = "0"
# MDP_MissNum = -1




