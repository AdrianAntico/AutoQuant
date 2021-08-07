
# Collection data.table
QA_Results <- data.table::CJ(
  Class = c("DRF", "GAM", "GBM", "GLM", "AutoML"),
  Method = c("TrainOnFull","Not","Not"),
  Success = "Failure", sorted = FALSE)

# AutoH2oDRFClassifier ----
data <- RemixAutoML::FakeDataGenerator(
  Correlation = 0.85,
  N = 1000L,
  ID = 2L,
  ZIP = 0L,
  AddDate = FALSE,
  Classification = TRUE,
  MultiClass = FALSE)

# run = 1
for(run in 1L:3L) {

  # Set training mode
  if(run %in% c(2,3)) TOF <- FALSE else TOF <- TRUE

  # grid tune
  if(run == 3) gridtune <- TRUE else gridtune <- FALSE

  # Create copy
  data1 <- data.table::copy(data)

  # Model
  TestModel <- tryCatch({RemixAutoML::AutoH2oDRFClassifier(

    # Compute management
    DebugMode = TRUE,
    MaxMem = "28G",
    NThreads = max(1L, parallel::detectCores() - 2L),
    IfSaveModel = "mojo",
    H2OShutdown = TRUE,
    H2OStartUp = TRUE,

    # Metadata arguments
    OutputSelection = c("EvalMetrics", "PDFs", "Score_TrainData"),
    eval_metric = "auc",
    NumOfParDepPlots = 3L,

    # Data arguments
    model_path = normalizePath("./"),
    metadata_path = NULL,
    ModelID = "FirstModel",
    ReturnModelObjects = TRUE,
    SaveModelObjects = TRUE,
    SaveInfoToPDF = TRUE,

    # Model evaluation
    data = data1,
    TrainOnFull = TOF,
    ValidationData = NULL,
    TestData = NULL,
    TargetColumnName = "Adrian",
    FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2", "Adrian")],
    WeightsColumn = NULL,
    CostMatrixWeights = c(1,0,0,1),

    # Grid Tuning Args
    GridStrategy = "RandomDiscrete",
    GridTune = gridtune,
    MaxModelsInGrid = 10,
    MaxRunTimeSecs = 60*60*24,
    StoppingRounds = 10,

    # ML Args
    Trees = if(!gridtune) 50 else c(50,51,52,53),
    MaxDepth = if(!gridtune) 20 else c(10,15,20),
    SampleRate = if(!gridtune) 0.632 else c(0.5,0.6,0.7),
    MTries = -1,
    ColSampleRatePerTree = if(!gridtune) 1 else c(0.5,0.6,0.7),
    ColSampleRatePerTreeLevel = if(!gridtune) 1 else c(0.5,0.6,0.7),
    MinRows = if(!gridtune) 1 else c(1,2,5,10),
    NBins = if(!gridtune) 20 else c(20,25,30),
    NBinsCats = if(!gridtune) 1024 else c(32,64,128),
    NBinsTopLevel = 1024,
    HistogramType = "AUTO",
    CategoricalEncoding = "AUTO")}, error = function(x) NULL)


  # Outcome
  if(!is.null(TestModel)) QA_Results[run, Outcome := "Success"]
  rm(TestModel)
  data.table::fwrite(QA_Results, file = "C:/Users/Bizon/Documents/GitHub/QA_Code/QA_CSV/AutoH2OClassifier_QA.csv")
  Sys.sleep(5)
}

# Defaults ----
# data <- RemixAutoML::FakeDataGenerator(
#   Correlation = 0.85,
#   N = 1000L,
#   ID = 2L,
#   ZIP = 0L,
#   AddDate = FALSE,
#   Classification = TRUE,
#   MultiClass = FALSE)
#
# # AutoH2oDRFClassifier
# # run = 1
# run = 2
#
# # Set training mode
# if(run %% 2 == 0) TOF <- FALSE else TOF <- TRUE

# Create copy
# data1 <- data.table::copy(data)
#
# library(RemixAutoML)
# library(data.table)
# source("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/H2OHelpers.R")
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/MiscFunctions.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/CatBoostHelpers.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ModelMetrics.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ModelEvaluationPlots.R"))
#
# OutputSelection = c("EvalMetrics", "PDFs", "Score_TrainData")
# MaxMem = "28G"
# NThreads = max(1L, parallel::detectCores() - 2L)
# IfSaveModel = "mojo"
# H2OShutdown = TRUE
# H2OStartUp = TRUE
# eval_metric = "auc"
# CostMatrixWeights = c(1,0,0,1)
# NumOfParDepPlots = 3L
# model_path = normalizePath("./")
# metadata_path = NULL
# ModelID = "FirstModel"
# ReturnModelObjects = TRUE
# SaveModelObjects = FALSE
# SaveInfoToPDF = TRUE
# data = data1
# TrainOnFull = TOF
# ValidationData = NULL
# TestData = NULL
# TargetColumnName = "Adrian"
# FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2", "Adrian")]
# WeightsColumn = NULL
# GridStrategy = "RandomDiscrete"
# GridTune = FALSE
# MaxModelsInGrid = 10
# MaxRunTimeSecs = 60*60*24
# StoppingRounds = 10
# Trees = 50L
# MaxDepth = 20
# SampleRate = 0.632
# MTries = -1
# ColSampleRatePerTree = 1
# ColSampleRatePerTreeLevel = 1
# MinRows = 1
# NBins = 20
# NBinsCats = 1024
# NBinsTopLevel = 1024
# HistogramType = "AUTO"
# CategoricalEncoding = "AUTO"
# DebugMode = TRUE

# DataPrep ----
# ClassWeights.=NULL
# CostMatrixWeights.=CostMatrixWeights
# SaveModelObjects.=SaveModelObjects
# ValidationData.=ValidationData
# TrainOnFull.=TrainOnFull
# TargetColumnName.=TargetColumnName
# ModelID.=ModelID
# model_path.=model_path
# metadata_path.=metadata_path
#
# TargetType.="classifier"
# TargetColumnName.=TargetColumnName
# data.=data
# ValidationData.=ValidationData
# TestData.=TestData
# TrainOnFull.=TrainOnFull
# FeatureColNames.=FeatureColNames
# SaveModelObjects.=SaveModelObjects
# model_path.=model_path
# ModelID.=ModelID


# AutoH2oGAMClassifier ----

# Create some dummy correlated data
data <- RemixAutoML::FakeDataGenerator(
  Correlation = 0.85,
  N = 1000,
  ID = 2,
  ZIP = 0,
  AddDate = FALSE,
  Classification = TRUE,
  MultiClass = FALSE)

# Define GAM Columns to use - up to 9 are allowed
GamCols <- names(which(unlist(lapply(data, is.numeric))))
GamCols <- GamCols[!GamCols %in% c("Adrian","IDcol_1","IDcol_2")]
GamCols <- GamCols[1L:(min(9L,length(GamCols)))]

# run = 4
for(run in 4L:6L) {

  # Set training mode
  if(run %in% c(5,6)) TOF <- FALSE else TOF <- TRUE

  # grid tune
  if(run == 6) gridtune <- TRUE else gridtune <- FALSE

  # Create copy
  data1 <- data.table::copy(data)

  # Model
  TestModel <- tryCatch({RemixAutoML::AutoH2oGAMClassifier(

    # Compute management
    MaxMem = "28G",
    NThreads = max(1, parallel::detectCores()-2),
    H2OShutdown = TRUE,
    H2OStartUp = TRUE,
    IfSaveModel = "mojo",

    # Model evaluation:
    eval_metric = "auc",
    NumOfParDepPlots = 3,

    # Metadata arguments:
    model_path = NULL,
    metadata_path = NULL,
    ModelID = "FirstModel",
    ReturnModelObjects = TRUE,
    SaveModelObjects = FALSE,
    SaveInfoToPDF = TRUE,
    CostMatrixWeights = c(1,0,0,1),

    # Data arguments:
    data = data1,
    TrainOnFull = TOF,
    ValidationData = NULL,
    TestData = NULL,
    TargetColumnName = "Adrian",
    FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
    WeightsColumn = NULL,
    GamColNames = GamCols,

    # Model args
    num_knots = NULL,
    keep_gam_cols = TRUE,
    GridTune = gridtune,
    GridStrategy = "RandomDiscrete",
    StoppingRounds = 10,
    MaxRunTimeSecs = 3600 * 24 * 7,
    MaxModelsInGrid = 10,
    Distribution = "binomial",
    Link = "logit",
    Solver = "AUTO",
    Alpha = if(!gridtune) NULL else c(0.10,0.50,0.90),
    Lambda = if(!gridtune) NULL else c(0.10,0.50,0.90),
    LambdaSearch = FALSE,
    NLambdas = -1,
    Standardize = TRUE,
    RemoveCollinearColumns = FALSE,
    InterceptInclude = TRUE,
    NonNegativeCoefficients = FALSE)}, error = function(x) NULL)

  # Outcome
  if(!is.null(TestModel)) QA_Results[run, Outcome := "Success"]
  rm(TestModel)
  data.table::fwrite(QA_Results, file = "C:/Users/Bizon/Documents/GitHub/QA_Code/AutoH2OClassifier_QA.csv")
  Sys.sleep(5)
}

# Defaults ----

# library(RemixAutoML)
# library(data.table)
#
# # Create some dummy correlated data
# data <- RemixAutoML::FakeDataGenerator(
#   Correlation = 0.85,
#   N = 1000,
#   ID = 2,
#   ZIP = 0,
#   AddDate = FALSE,
#   Classification = TRUE,
#   MultiClass = FALSE)
#
# # Define GAM Columns to use - up to 9 are allowed
# GamCols <- names(which(unlist(lapply(data, is.numeric))))
# GamCols <- GamCols[!GamCols %in% c("Adrian","IDcol_1","IDcol_2")]
# GamCols <- GamCols[1L:(min(9L,length(GamCols)))]
#
# # AutoH2oGAMClassifier
# # run = 10
# run = 11
# # run = 12
#
# # Set training mode
# if(run %in% c(11,12))  TOF <- FALSE else TOF <- TRUE
#
# # grid tune
# if(run == 12) gridtune <- TRUE else gridtune <- FALSE
#
# # Create copy
# data1 <- data.table::copy(data)
#
# DebugMode = TRUE
# MaxMem = "28G"
# NThreads = max(1, parallel::detectCores()-2)
# H2OShutdown = TRUE
# H2OStartUp = TRUE
# IfSaveModel = "mojo"
# eval_metric = "auc"
# NumOfParDepPlots = 3
# model_path = NULL
# metadata_path = NULL
# ModelID = "FirstModel"
# ReturnModelObjects = TRUE
# SaveModelObjects = FALSE
# SaveInfoToPDF = TRUE
# CostMatrixWeights = c(1,0,0,1)
# data = data1
# TrainOnFull = TOF
# ValidationData = NULL
# TestData = NULL
# TargetColumnName = "Adrian"
# FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")]
# WeightsColumn = NULL
# GamColNames = GamCols
# num_knots = NULL
# keep_gam_cols = TRUE
# GridTune = gridtune
# GridStrategy = "RandomDiscrete"
# StoppingRounds = 10
# MaxRunTimeSecs = 3600 * 24 * 7
# MaxModelsInGrid = 10
# Distribution = "binomial"
# Link = "logit"
# Solver = "AUTO"
# Alpha = NULL
# Lambda = NULL
# LambdaSearch = FALSE
# NLambdas = -1
# Standardize = TRUE
# RemoveCollinearColumns = FALSE
# InterceptInclude = TRUE
# NonNegativeCoefficients = FALSE


# AutoH2oGBMClassifier ----
data <- RemixAutoML::FakeDataGenerator(
  Correlation = 0.85,
  N = 1000L,
  ID = 2L,
  ZIP = 0L,
  AddDate = FALSE,
  Classification = TRUE,
  MultiClass = FALSE)

# run = 7
for(run in 7L:9L) {

  # Set training mode
  if(run %in% c(8,9)) TOF <- FALSE else TOF <- TRUE

  # Create copy
  data1 <- data.table::copy(data)

  # grid tune
  if(run == 9) gridtune <- TRUE else gridtune <- FALSE

  # Model
  TestModel <- tryCatch({RemixAutoML::AutoH2oGBMClassifier(

    # Compute management
    MaxMem = "28G",
    NThreads = max(1, parallel::detectCores()-2),
    H2OShutdown = TRUE,
    H2OStartUp = TRUE,
    IfSaveModel = "mojo",

    # Model evaluation
    NumOfParDepPlots = 3,

    # Metadata arguments:
    model_path = normalizePath("./"),
    metadata_path = file.path(normalizePath("./")),
    ModelID = "FirstModel",
    ReturnModelObjects = TRUE,
    SaveModelObjects = FALSE,
    SaveInfoToPDF = TRUE,
    CostMatrixWeights = c(1,0,0,1),

    # Data arguments:
    data = data1,
    TrainOnFull = TOF,
    ValidationData = NULL,
    TestData = NULL,
    TargetColumnName = "Adrian",
    FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
    WeightsColumn = NULL,

    # ML grid tuning args
    GridTune = gridtune,
    GridStrategy = "RandomDiscrete",
    MaxRunTimeSecs = 60*60*24,
    StoppingRounds = 10,
    MaxModelsInGrid = 2,

    # Model args
    Trees = if(!gridtune) 50 else c(50,51,52),
    LearnRate = if(!gridtune) 0.10 else c(0.10,0.20,0.30),
    LearnRateAnnealing = 1,
    eval_metric = "auc",
    Distribution = "bernoulli",
    MaxDepth = 20,
    SampleRate = 0.632,
    ColSampleRate = 1,
    ColSampleRatePerTree = 1,
    ColSampleRatePerTreeLevel  = 1,
    MinRows = 1,
    NBins = 20,
    NBinsCats = 1024,
    NBinsTopLevel = 1024,
    HistogramType = "AUTO",
    CategoricalEncoding = "AUTO")}, error = function(x) NULL)

  # Outcome
  if(!is.null(TestModel)) QA_Results[run, Outcome := "Success"]
  rm(TestModel)
  data.table::fwrite(QA_Results, file = "C:/Users/Bizon/Documents/GitHub/QA_Code/AutoH2OClassifier_QA.csv")
  Sys.sleep(5)
}

# AutoH2oGLMClassifier ----
data <- RemixAutoML::FakeDataGenerator(
  Correlation = 0.85,
  N = 1000L,
  ID = 2L,
  ZIP = 0L,
  AddDate = FALSE,
  Classification = TRUE,
  MultiClass = FALSE)

# run = 10
for(run in 10L:12L) {

  # Set training mode
  if(run %in% c(11,12)) TOF <- FALSE else TOF <- TRUE

  # Create copy
  data1 <- data.table::copy(data)

  # grid tune
  if(run == 12) gridtune <- TRUE else gridtune <- FALSE

  # Model
  TestModel <- tryCatch({RemixAutoML::AutoH2oGLMClassifier(

    # Compute management
    MaxMem = "28G",
    NThreads = max(1, parallel::detectCores()-2),
    H2OShutdown = TRUE,
    H2OStartUp = TRUE,
    IfSaveModel = "mojo",

    # Model evaluation:
    eval_metric = "auc",
    NumOfParDepPlots = 3,

    # Metadata arguments:
    model_path = NULL,
    metadata_path = NULL,
    ModelID = "FirstModel",
    ReturnModelObjects = TRUE,
    SaveModelObjects = FALSE,
    SaveInfoToPDF = TRUE,
    CostMatrixWeights = c(1,0,0,1),

    # Data arguments:
    data = data1,
    TrainOnFull = TOF,
    ValidationData = NULL,
    TestData = NULL,
    TargetColumnName = "Adrian",
    FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
    RandomColNumbers = NULL,
    InteractionColNumbers = NULL,
    WeightsColumn = NULL,

    # Model args
    GridTune = gridtune,
    GridStrategy = "RandomDiscrete",
    StoppingRounds = 10,
    MaxRunTimeSecs = 3600 * 24 * 7,
    MaxModelsInGrid = 10,
    Distribution = "binomial",
    Link = "logit",
    RandomDistribution = NULL,
    RandomLink = NULL,
    Solver = "AUTO",
    Alpha = if(!gridtune) NULL else c(0.1,0.2,0.3),
    Lambda = if(!gridtune) NULL else c(0.1,0.2,0.3),
    LambdaSearch = FALSE,
    NLambdas = -1,
    Standardize = TRUE,
    RemoveCollinearColumns = FALSE,
    InterceptInclude = TRUE,
    NonNegativeCoefficients = FALSE)}, error = function(x) NULL)

  # Outcome
  if(!is.null(TestModel)) QA_Results[run, Outcome := "Success"]
  rm(TestModel)
  data.table::fwrite(QA_Results, file = "C:/Users/Bizon/Documents/GitHub/QA_Code/AutoH2OClassifier_QA.csv")
  Sys.sleep(5)
}

# Defaults ----
# rm(data)
#
# library(RemixAutoML)
# library(data.table)
# MaxMem = "28G"
# NThreads = max(1, parallel::detectCores()-2)
# H2OShutdown = TRUE
# H2OStartUp = TRUE
# IfSaveModel = "mojo"
# eval_metric = "auc"
# NumOfParDepPlots = 3
# model_path = NULL
# metadata_path = NULL
# ModelID = "FirstModel"
# ReturnModelObjects = TRUE
# SaveModelObjects = FALSE
# SaveInfoToPDF = TRUE
# CostMatrixWeights = c(1,0,0,1)
# data = data1
# TrainOnFull = TOF
# ValidationData = NULL
# TestData = NULL
# TargetColumnName = "Adrian"
# FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")]
# RandomColNumbers = NULL
# InteractionColNumbers = NULL
# WeightsColumn = NULL
# TransformNumericColumns = NULL
# Methods = c("BoxCox", "Asinh", "Asin", "Log", "LogPlus1", "Sqrt", "Logit", "YeoJohnson")
# GridTune = FALSE
# GridStrategy = "RandomDiscrete"
# StoppingRounds = 10
# MaxRunTimeSecs = 3600 * 24 * 7
# MaxModelsInGrid = 10
# Distribution = "binomial"
# Link = "logit"
# RandomDistribution = NULL
# RandomLink = NULL
# Solver = "AUTO"
# Alpha = NULL
# Lambda = NULL
# LambdaSearch = FALSE
# NLambdas = -1
# Standardize = TRUE
# RemoveCollinearColumns = FALSE
# InterceptInclude = TRUE
# NonNegativeCoefficients = FALSE
#
# RemixClassificationMetrics(
#   MLModels="h2oglm",
#   TargetVariable=eval(TargetColumnName),
#   Thresholds=seq(0.01,0.99,0.01),
#   CostMatrix=CostMatrixWeights,
#   ClassLabels=c(1,0),
#   H2oGLMTestData=ValidationData)

# AutoH2oMLClassifier ----

# Create some dummy correlated data with numeric and categorical features
data <- RemixAutoML::FakeDataGenerator(
  Correlation = 0.85,
  N = 1000L,
  ID = 2L,
  ZIP = 0L,
  AddDate = FALSE,
  Classification = TRUE,
  MultiClass = FALSE)

# run = 13
for(run in 13L:14L) {

  # Set training mode
  if(run %in% c(14)) TOF <- FALSE else TOF <- TRUE

  # Create copy
  data1 <- data.table::copy(data)

  # Model
  TestModel <- tryCatch({RemixAutoML::AutoH2oMLClassifier(
    data = data1,
    TrainOnFull = TOF,
    ValidationData = NULL,
    TestData = NULL,
    TargetColumnName = "Adrian",
    FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
    ExcludeAlgos = NULL,
    eval_metric = "auc",
    CostMatrixWeights = c(1,0,0,1),
    MaxMem = "28G",
    NThreads = max(1, parallel::detectCores()-2),
    MaxModelsInGrid = 10,
    model_path = normalizePath("./"),
    metadata_path = normalizePath("./"),
    ModelID = "FirstModel",
    NumOfParDepPlots = 3,
    ReturnModelObjects = TRUE,
    SaveModelObjects = FALSE,
    IfSaveModel = "mojo",
    H2OShutdown = TRUE,
    H2OStartUp = TRUE)}, error = function(x) NULL)

  # Outcome
  if(!is.null(TestModel)) QA_Results[run, Outcome := "Success"]
  rm(TestModel)
  data.table::fwrite(QA_Results, file = "C:/Users/Bizon/Documents/GitHub/QA_Code/AutoH2OClassifier_QA.csv")
  Sys.sleep(5)
}

# Defaults ----
# library(RemixAutoML)
# library(data.table)
#
# data = data1
# TrainOnFull = TOF
# ValidationData = NULL
# TestData = NULL
# TargetColumnName = "Adrian"
# FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")]
# ExcludeAlgos = NULL
# eval_metric = "auc"
# CostMatrixWeights = c(1,0,0,1)
# MaxMem = "28G"
# NThreads = max(1, parallel::detectCores()-2)
# MaxModelsInGrid = 10
# model_path = normalizePath("./")
# metadata_path = normalizePath("./")
# ModelID = "FirstModel"
# NumOfParDepPlots = 3
# ReturnModelObjects = TRUE
# SaveModelObjects = FALSE
# IfSaveModel = "mojo"
# H2OShutdown = TRUE
# H2OStartUp = TRUE
