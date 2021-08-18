
# Collection data.table
QA_Results <- data.table::CJ(
  Class = c("DRF", "GAM", "GBM", "GLM", "AutoML"),
  Method = c("TrainOnFull","Not","Not"),
  Success = "Failure", sorted = FALSE)

# AutoH2oDRFMultiClass ----
data <- RemixAutoML::FakeDataGenerator(
  Correlation = 0.85,
  N = 1000L,
  ID = 2L,
  ZIP = 0L,
  AddDate = FALSE,
  Classification = FALSE,
  MultiClass = TRUE)

# run = 2
for(run in 1L:3L) {

  # Set training mode
  if(run %in% c(2,3)) TOF <- FALSE else TOF <- TRUE

  # grid tune
  if(run == 3) gridtune <- TRUE else gridtune <- FALSE

  # Create copy
  data1 <- data.table::copy(data)

  # Model
  TestModel <- tryCatch({RemixAutoML::AutoH2oDRFMultiClass(
    OutputSelection = c("EvalMetrics", "PDFs", "Score_TrainData"),
    data = data1,
    TrainOnFull = TOF,
    ValidationData = NULL,
    TestData = NULL,
    TargetColumnName = "Adrian",
    FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
    WeightsColumn = NULL,
    eval_metric = "logloss",
    MaxMem = "28G",
    NThreads = max(1, parallel::detectCores()-2),
    model_path = normalizePath("./"),
    metadata_path = file.path(normalizePath("./")),
    ModelID = "FirstModel",
    ReturnModelObjects = TRUE,
    SaveModelObjects = FALSE,
    IfSaveModel = "mojo",
    H2OShutdown = TRUE,
    H2OStartUp = TRUE,
    DebugMode = TRUE,

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
  if(!is.null(TestModel)) QA_Results[run, Success := "Success"]
  rm(TestModel)
  data.table::fwrite(QA_Results, file = "C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/Testing_Data/QA_CSV/AutoH2OMultiClass_QA.csv")
  Sys.sleep(5)
}

# Defaults ----
# library(RemixAutoML)
# library(data.table)
# source("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/H2OHelpers.R")
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/MiscFunctions.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/CatBoostHelpers.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ModelMetrics.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ModelEvaluationPlots.R"))
#
# run = 1
#
# # Create some dummy correlated data
# data <- RemixAutoML::FakeDataGenerator(
#   Correlation = 0.85,
#   N = 1000L,
#   ID = 2L,
#   ZIP = 0L,
#   AddDate = FALSE,
#   Classification = FALSE,
#   MultiClass = TRUE)
#
# # Set training mode
# if(run %in% c(8,9)) TOF <- FALSE else TOF <- TRUE
#
# # grid tune
# if(run == 9) gridtune <- TRUE else gridtune <- FALSE
#
# # Create copy
# data1 <- data.table::copy(data)
#
# OutputSelection = c("EvalMetrics", "PDFs", "Score_TrainData")
# data = data1
# TrainOnFull = TOF
# ValidationData = NULL
# TestData = NULL
# TargetColumnName = "Adrian"
# FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")]
# WeightsColumn = NULL
# eval_metric = "logloss"
# MaxMem = "28G"
# NThreads = max(1, parallel::detectCores()-2)
# model_path = normalizePath("./")
# metadata_path = file.path(normalizePath("./"))
# ModelID = "FirstModel"
# ReturnModelObjects = TRUE
# SaveModelObjects = FALSE
# IfSaveModel = "mojo"
# H2OShutdown = TRUE
# H2OStartUp = TRUE
# GridStrategy = "RandomDiscrete"
# GridTune = FALSE
# MaxModelsInGrid = 10
# MaxRunTimeSecs = 60*60*24
# StoppingRounds = 10
# Trees = if(!gridtune) 50 else c(50,51,52,53)
# MaxDepth = if(!gridtune) 20 else c(10,15,20)
# SampleRate = if(!gridtune) 0.632 else c(0.5,0.6,0.7)
# MTries = -1
# ColSampleRatePerTree = if(!gridtune) 1 else c(0.5,0.6,0.7)
# ColSampleRatePerTreeLevel = if(!gridtune) 1 else c(0.5,0.6,0.7)
# MinRows = if(!gridtune) 1 else c(1,2,5,10)
# NBins = if(!gridtune) 20 else c(20,25,30)
# NBinsCats = if(!gridtune) 1024 else c(32,64,128)
# NBinsTopLevel = 1024
# HistogramType = "AUTO"
# CategoricalEncoding = "AUTO"
# DebugMode = TRUE

# Multinomial Metrics ----
# ModelClass="h2o"
# SaveModelObjects.=SaveModelObjects
# ValidationData.=ValidationData
# PredictData.=predict
# TrainOnFull.=TrainOnFull
# TargetColumnName.=TargetColumnName
# TargetLevels.=TargetLevels
# ModelID.=ModelID
# model_path.=model_path
# metadata_path.=metadata_path

# Evaluation metrics ----
# SaveModelObjects. = SaveModelObjects
# ValidationData. = ValidationData
# PredictData. = predict
# TrainOnFull. = TrainOnFull
# TargetColumnName. = TargetColumnName
# TargetLevels. = TargetLevels
# ModelID. = ModelID
# model_path. = model_path
# metadata_path. = metadata_path

# AutoH2oGAMMultiClass ----
data <- RemixAutoML::FakeDataGenerator(
  Correlation = 0.85,
  N = 1000L,
  ID = 2L,
  ZIP = 0L,
  AddDate = FALSE,
  Classification = FALSE,
  MultiClass = TRUE)

# Define GAM Columns to use - up to 9 are allowed
GamCols <- names(which(unlist(lapply(data, is.numeric))))
GamCols <- GamCols[!GamCols %in% c("Adrian","IDcol_1","IDcol_2")]
GamCols <- GamCols[1L:(min(9L,length(GamCols)))]

# AutoH2oGAMMultiClass
# run = 4
for(run in 4L:6L) {

  # Set training mode
  if(run %in% c(5,6)) TOF <- FALSE else TOF <- TRUE

  # Create copy
  data1 <- data.table::copy(data)

  # grid tune
  if(run == 6) gridtune <- TRUE else gridtune <- FALSE

  # Model
  TestModel <- tryCatch({RemixAutoML::AutoH2oGAMMultiClass(
    OutputSelection = c("EvalMetrics", "PDFs", "Score_TrainData"),
    data = data1,
    TrainOnFull = TOF,
    ValidationData = NULL,
    TestData = NULL,
    TargetColumnName = "Adrian",
    FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
    WeightsColumn = NULL,
    GamColNames = GamCols,
    eval_metric = "logloss",
    MaxMem = "28G",
    NThreads = max(1, parallel::detectCores()-2),
    model_path = normalizePath("./"),
    metadata_path = NULL,
    ModelID = "FirstModel",
    ReturnModelObjects = TRUE,
    SaveModelObjects = FALSE,
    IfSaveModel = "mojo",
    H2OShutdown = TRUE,
    H2OStartUp = TRUE,
    DebugMode = TRUE,

    # Model args
    num_knots = NULL,
    keep_gam_cols = TRUE,
    GridTune = gridtune,
    GridStrategy = "RandomDiscrete",
    StoppingRounds = 10,
    MaxRunTimeSecs = 3600 * 24 * 7,
    MaxModelsInGrid = 10,
    Distribution = "multinomial",
    Link = "Family_Default",
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
  if(!is.null(TestModel)) QA_Results[run, Success := "Success"]
  rm(TestModel)
  data.table::fwrite(QA_Results, file = "C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/Testing_Data/AutoH2OMultiClass_QA.csv")
  Sys.sleep(5)
}


# Defaults ----

# library(RemixAutoML)
# library(data.table)
# source("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/H2OHelpers.R")
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/MiscFunctions.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/CatBoostHelpers.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ModelMetrics.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ModelEvaluationPlots.R"))
#
# run = 4
#
# # Create some dummy correlated data
# data <- RemixAutoML::FakeDataGenerator(
#   Correlation = 0.85,
#   N = 1000L,
#   ID = 2L,
#   ZIP = 0L,
#   AddDate = FALSE,
#   Classification = FALSE,
#   MultiClass = TRUE)
#
# # Set training mode
# if(run %in% c(8,9)) TOF <- FALSE else TOF <- TRUE
#
# # grid tune
# if(run == 9) gridtune <- TRUE else gridtune <- FALSE
#
# # Create copy
# data1 <- data.table::copy(data)
#
# OutputSelection = c("EvalMetrics", "PDFs", "Score_TrainData")
# data = data1
# TrainOnFull = TOF
# ValidationData = NULL
# TestData = NULL
# TargetColumnName = "Adrian"
# FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")]
# WeightsColumn = NULL
# GamColNames = GamCols
# eval_metric = "logloss"
# MaxMem = "28G"
# NThreads = max(1, parallel::detectCores()-2)
# model_path = normalizePath("./")
# metadata_path = NULL
# ModelID = "FirstModel"
# ReturnModelObjects = TRUE
# SaveModelObjects = FALSE
# IfSaveModel = "mojo"
# H2OShutdown = TRUE
# H2OStartUp = TRUE
# DebugMode = TRUE
# num_knots = NULL
# keep_gam_cols = TRUE
# GridTune = gridtune
# GridStrategy = "RandomDiscrete"
# StoppingRounds = 10
# MaxRunTimeSecs = 3600 * 24 * 7
# MaxModelsInGrid = 10
# Distribution = "multinomial"
# Link = "Family_Default"
# Solver = "AUTO"
# Alpha = if(!gridtune) NULL else c(0.10,0.50,0.90)
# Lambda = if(!gridtune) NULL else c(0.10,0.50,0.90)
# LambdaSearch = FALSE
# NLambdas = -1
# Standardize = TRUE
# RemoveCollinearColumns = FALSE
# InterceptInclude = TRUE
# NonNegativeCoefficients = FALSE

# AutoH2oGBMMultiClass ----
data <- RemixAutoML::FakeDataGenerator(
  Correlation = 0.85,
  N = 1000,
  ID = 2,
  ZIP = 0,
  AddDate = FALSE,
  Classification = FALSE,
  MultiClass = TRUE)

# run = 7
for(run in 7L:9L) {

  # Set training mode
  if(run %in% c(8,9)) TOF <- FALSE else TOF <- TRUE

  # Create copy
  data1 <- data.table::copy(data)

  # grid tune
  if(run == 9) gridtune <- TRUE else gridtune <- FALSE

  # Model
  TestModel <- tryCatch({RemixAutoML::AutoH2oGBMMultiClass(
    OutputSelection = c("EvalMetrics", "PDFs", "Score_TrainData"),
    data = data1,
    TrainOnFull = TOF,
    ValidationData = NULL,
    TestData = NULL,
    TargetColumnName = "Adrian",
    FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
    WeightsColumn = NULL,
    eval_metric = "logloss",
    MaxMem = "28G",
    NThreads = max(1, parallel::detectCores()-2),
    model_path = normalizePath("./"),
    metadata_path = file.path(normalizePath("./")),
    ModelID = "FirstModel",
    ReturnModelObjects = TRUE,
    SaveModelObjects = FALSE,
    IfSaveModel = "mojo",
    H2OShutdown = TRUE,
    H2OStartUp = TRUE,
    DebugMode = TRUE,

    # Model args
    GridTune = gridtune,
    GridStrategy = "RandomDiscrete",
    MaxRunTimeSecs = 60*60*24,
    StoppingRounds = 10,
    MaxModelsInGrid = 2,
    Trees = if(!gridtune) 50 else c(50,51,52),
    LearnRate = if(!gridtune) 0.10 else c(0.10,0.20,0.30),
    LearnRateAnnealing = 1,
    Distribution = "multinomial",
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
  if(!is.null(TestModel)) QA_Results[run, Success := "Success"]
  rm(TestModel)
  data.table::fwrite(QA_Results, file = "C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/Testing_Data/AutoH2OMultiClass_QA.csv")
  Sys.sleep(5)
}

# AutoH2oGLMMultiClass ----
data <- RemixAutoML::FakeDataGenerator(
  Correlation = 0.85,
  N = 1000L,
  ID = 2L,
  ZIP = 0L,
  AddDate = FALSE,
  Classification = FALSE,
  MultiClass = TRUE)

# run = 10
for(run in 10L:12L) {

  # Set training mode
  if(run %in% c(11,12)) TOF <- FALSE else TOF <- TRUE

  # Create copy
  data1 <- data.table::copy(data)

  # grid tune
  if(run == 12) gridtune <- TRUE else gridtune <- FALSE

  # Model
  TestModel <- tryCatch({RemixAutoML::AutoH2oGLMMultiClass(

    # Compute management
    MaxMem = "28g",
    OutputSelection = c("EvalMetrics", "PDFs", "Score_TrainData"),
    NThreads = max(1, parallel::detectCores()-2),
    H2OShutdown = TRUE,
    H2OStartUp = TRUE,
    IfSaveModel = "mojo",

    # Model evaluation:
    eval_metric = "logloss",
    NumOfParDepPlots = 3,

    # Metadata arguments:
    model_path = NULL,
    metadata_path = NULL,
    ModelID = "FirstModel",
    ReturnModelObjects = TRUE,
    SaveModelObjects = FALSE,
    SaveInfoToPDF = FALSE,
    DebugMode = TRUE,

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
    Distribution = "multinomial",
    Link = "family_default",
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
  if(!is.null(TestModel)) QA_Results[run, Success := "Success"]
  rm(TestModel)
  data.table::fwrite(QA_Results, file = "C:/Users/Bizon/Documents/GitHub/QA_Code/AutoH2OMultiClass_QA.csv")
  Sys.sleep(5)
}

# AutoH2oMLMultiClass ----
data <- RemixAutoML::FakeDataGenerator(
  Correlation = 0.85,
  N = 1000,
  ID = 2,
  ZIP = 0,
  AddDate = FALSE,
  Classification = FALSE,
  MultiClass = TRUE)

# run = 13
for(run in 13L:14L) {

  # Set training mode
  if(run %in% c(14)) TOF <- FALSE else TOF <- TRUE

  # Create copy
  data1 <- data.table::copy(data)

  # Model
  TestModel <- tryCatch({RemixAutoML::AutoH2oMLMultiClass(
    OutputSelection = c("EvalMetrics", "PDFs", "Score_TrainData"),
    data = data1,
    TrainOnFull = TOF,
    ValidationData = NULL,
    TestData = NULL,
    TargetColumnName = "Adrian",
    FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
    ExcludeAlgos = NULL,
    eval_metric = "logloss",
    MaxMem = "28G",
    NThreads = max(1, parallel::detectCores()-2),
    MaxModelsInGrid = 10,
    model_path = normalizePath("./"),
    metadata_path = normalizePath("./"),
    ModelID = "FirstModel",
    ReturnModelObjects = TRUE,
    SaveModelObjects = FALSE,
    DebugMode = TRUE,
    IfSaveModel = "mojo",
    H2OShutdown = TRUE,
    H2OStartUp = TRUE)}, error = function(x) NULL)

  # Outcome
  if(!is.null(TestModel)) QA_Results[run, Success := "Success"]
  rm(TestModel)
  data.table::fwrite(QA_Results, file = "C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/Testing_Data/AutoH2OMultiClass_QA.csv")
  Sys.sleep(5)
}
