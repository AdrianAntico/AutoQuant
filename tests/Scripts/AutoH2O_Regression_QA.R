
# Collection data.table
QA_Results <- data.table::CJ(
  Class = c("DRF", "GAM", "GBM", "GLM", "AutoML"),
  Method = c("TrainOnFull","Not","Not"),
  Success = "Failure", sorted = FALSE)
QA_Results <- QA_Results[seq_len(.N-1)]

# AutoH2oDRFRegression ----
data <- RemixAutoML::FakeDataGenerator(
  Correlation = 0.85,
  N = 1000,
  ID = 2,
  ZIP = 0,
  AddDate = FALSE,
  Classification = FALSE,
  MultiClass = FALSE)

# run = 2
for(run in 1L:3L) {

  # Set training mode
  if(run %in% c(2,3)) TOF <- FALSE else TOF <- TRUE

  # grid tune
  if(run == 3) gridtune <- TRUE else gridtune <- FALSE

  # Create copy
  data1 <- data.table::copy(data)

  # Model
  TestModel <- tryCatch({RemixAutoML::AutoH2oDRFRegression(

    # Compute management
    MaxMem = "32g",
    NThreads = max(1L, parallel::detectCores() - 2L),
    H2OShutdown = TRUE,
    H2OStartUp = TRUE,
    IfSaveModel = "mojo",
    DebugMode = TRUE,

    # Model evaluation:
    eval_metric = "RMSE",
    NumOfParDepPlots = 3,

    # Metadata arguments:
    OutputSelection = c("EvalMetrics", "PDFs", "Score_TrainData"),
    model_path = normalizePath("./"),
    metadata_path = NULL,
    ModelID = "FirstModel",
    ReturnModelObjects = TRUE,
    SaveModelObjects = FALSE,
    SaveInfoToPDF = TRUE,

    # Data Args
    data = data1,
    TrainOnFull = TOF,
    ValidationData = NULL,
    TestData = NULL,
    TargetColumnName = "Adrian",
    FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
    WeightsColumn = NULL,
    TransformNumericColumns = NULL,
    Methods = c("BoxCox", "Asinh", "Asin", "Log", "LogPlus1", "Sqrt", "Logit"),

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
  data.table::fwrite(QA_Results, file = "C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/Testing_Data/AutoH2ORegression_QA.csv")
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
# MaxMem = "32g"
# NThreads = max(1L, parallel::detectCores() - 2L)
# H2OShutdown = TRUE
# H2OStartUp = TRUE
# IfSaveModel = "mojo"
# DebugMode = TRUE
# eval_metric = "RMSE"
# NumOfParDepPlots = 3
# OutputSelection = c("EvalMetrics", "PDFs", "Score_TrainData")
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
# FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")]
# WeightsColumn = NULL
# TransformNumericColumns = NULL
# Methods = c("BoxCox", "Asinh", "Asin", "Log", "LogPlus1", "Sqrt", "Logit")
# GridStrategy = "RandomDiscrete"
# GridTune = gridtune
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


# AutoH2oGAMRegression ----
data <- RemixAutoML::FakeDataGenerator(
  Correlation = 0.85,
  N = 1000,
  ID = 2,
  ZIP = 0,
  AddDate = FALSE,
  Classification = FALSE,
  MultiClass = FALSE)

# Define GAM Columns to use - up to 9 are allowed
GamCols <- names(which(unlist(lapply(data, is.numeric))))
GamCols <- GamCols[!GamCols %in% c("Adrian","IDcol_1","IDcol_2")]
GamCols <- GamCols[1L:(min(9L,length(GamCols)))]

# run = 4
for(run in 4L:6L) {

  # Set training mode
  if(run %in% c(5,6)) TOF <- FALSE else TOF <- TRUE

  # Create copy
  data1 <- data.table::copy(data)

  # grid tune
  if(run == 6) gridtune <- TRUE else gridtune <- FALSE

  # Model
  TestModel <- tryCatch({RemixAutoML::AutoH2oGAMRegression(

    # Compute management
    MaxMem = "28G",
    NThreads = max(1, parallel::detectCores()-2),
    H2OShutdown = TRUE,
    H2OStartUp = TRUE,
    IfSaveModel = "mojo",
    DebugMode = TRUE,

    # Model evaluation:
    eval_metric = "RMSE",
    NumOfParDepPlots = 3,

    # Metadata arguments:
    OutputSelection = c("EvalMetrics", "PDFs", "Score_TrainData"),
    model_path = NULL,
    metadata_path = NULL,
    ModelID = "FirstModel",
    ReturnModelObjects = TRUE,
    SaveModelObjects = FALSE,
    SaveInfoToPDF = TRUE,

    # Data arguments:
    data = data1,
    TrainOnFull = TOF,
    ValidationData = NULL,
    TestData = NULL,
    TargetColumnName = "Adrian",
    FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
    InteractionColNumbers = NULL,
    WeightsColumn = NULL,
    GamColNames = GamCols,
    TransformNumericColumns = NULL,
    Methods = c("BoxCox", "Asinh", "Asin", "Log", "LogPlus1", "Sqrt", "Logit"),

    # Model args
    num_knots = NULL,
    keep_gam_cols = TRUE,
    GridTune = gridtune,
    GridStrategy = "RandomDiscrete",
    StoppingRounds = 10,
    MaxRunTimeSecs = 3600 * 24 * 7,
    MaxModelsInGrid = 10,
    Distribution = "gaussian",
    Link = "Family_Default",
    TweedieLinkPower = NULL,
    TweedieVariancePower = NULL,
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
  data.table::fwrite(QA_Results, file = "C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/Testing_Data/AutoH2ORegression_QA.csv")
  Sys.sleep(5)
}

# AutoH2oGBMRegression ----
data <- RemixAutoML::FakeDataGenerator(
  Correlation = 0.85,
  N = 1000,
  ID = 2,
  ZIP = 0,
  AddDate = FALSE,
  Classification = FALSE,
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
  TestModel <- tryCatch({RemixAutoML::AutoH2oGBMRegression(

    # Compute management
    MaxMem = "28G",
    NThreads = max(1, parallel::detectCores()-2),
    H2OShutdown = TRUE,
    H2OStartUp = TRUE,
    IfSaveModel = "mojo",
    DebugMode = TRUE,

    # Model evaluation:
    eval_metric = "RMSE",
    NumOfParDepPlots = 3,

    # Metadata arguments:
    OutputSelection = c("EvalMetrics", "PDFs", "Score_TrainData"),

    # Metadata arguments:
    model_path = normalizePath("./"),
    metadata_path = file.path(normalizePath("./")),
    ModelID = "FirstModel",
    ReturnModelObjects = TRUE,
    SaveModelObjects = FALSE,
    SaveInfoToPDF = TRUE,

    # Data arguments
    data = data1,
    TrainOnFull = TOF,
    ValidationData = NULL,
    TestData = NULL,
    TargetColumnName = "Adrian",
    FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
    WeightsColumn = NULL,
    TransformNumericColumns = NULL,
    Methods = c("BoxCox", "Asinh", "Asin", "Log", "LogPlus1", "Sqrt", "Logit","YeoJohnson"),

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
    Alpha = NULL,
    Distribution = "poisson",
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
  data.table::fwrite(QA_Results, file = "C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/Testing_Data/AutoH2ORegression_QA.csv")
  Sys.sleep(5)
}

# AutoH2oGLMRegression ----
data <- RemixAutoML::FakeDataGenerator(
  Correlation = 0.85,
  N = 1000,
  ID = 2,
  ZIP = 0,
  AddDate = FALSE,
  Classification = FALSE,
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
  TestModel <- tryCatch({RemixAutoML::AutoH2oGLMRegression(

    # Compute management
    MaxMem = "28G",
    NThreads = max(1, parallel::detectCores()-2),
    H2OShutdown = TRUE,
    H2OStartUp = TRUE,
    IfSaveModel = "mojo",

    # Model evaluation:
    eval_metric = "RMSE",
    NumOfParDepPlots = 3,

    # Metadata arguments:
    model_path = NULL,
    metadata_path = NULL,
    ModelID = "FirstModel",
    ReturnModelObjects = TRUE,
    SaveModelObjects = FALSE,
    SaveInfoToPDF = TRUE,

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
    TransformNumericColumns = NULL,
    Methods = c("BoxCox", "Asinh", "Asin", "Log", "LogPlus1", "Sqrt", "Logit", "YeoJohnson"),

    # Model args
    GridTune = gridtune,
    GridStrategy = "RandomDiscrete",
    StoppingRounds = 10,
    MaxRunTimeSecs = 3600 * 24 * 7,
    MaxModelsInGrid = 10,
    Distribution = "gaussian",
    Link = "identity",
    TweedieLinkPower = NULL,
    TweedieVariancePower = NULL,
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
  data.table::fwrite(QA_Results, file = "C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/Testing_Data/AutoH2ORegression_QA.csv")
  Sys.sleep(5)
}

# AutoH2oMLRegression ----
data <- RemixAutoML::FakeDataGenerator(
  Correlation = 0.85,
  N = 1000,
  ID = 2,
  ZIP = 0,
  AddDate = FALSE,
  Classification = FALSE,
  MultiClass = FALSE)

# run = 13
for(run in 13L:14L) {

  # Set training mode
  if(run %in% 14) TOF <- FALSE else TOF <- TRUE

  # Create copy
  data1 <- data.table::copy(data)

  # Model
  TestModel <- tryCatch({RemixAutoML::AutoH2oMLRegression(

    # Compute management
    MaxMem = "28G",
    NThreads = max(1, parallel::detectCores()-2),
    H2OShutdown = TRUE,
    H2OStartUp = TRUE,
    IfSaveModel = "mojo",

    # Model evaluation
    eval_metric = "RMSE",
    NumOfParDepPlots = 3,

    # Metadata arguments
    model_path = NULL,
    metadata_path = NULL,
    ModelID = "FirstModel",
    ReturnModelObjects = TRUE,
    SaveModelObjects = FALSE,
    SaveInfoToPDF = FALSE,

    # Data arguments
    data = data1,
    TrainOnFull = TOF,
    ValidationData = NULL,
    TestData = NULL,
    TargetColumnName = "Adrian",
    FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
    TransformNumericColumns = NULL,
    Methods = c("BoxCox", "Asinh", "Asin", "Log", "LogPlus1", "Sqrt", "Logit"),

    # Model args
    ExcludeAlgos = NULL)}, error = function(x) NULL)

  # Outcome
  if(!is.null(TestModel)) QA_Results[run, Success := "Success"]
  rm(TestModel)
  data.table::fwrite(QA_Results, file = "C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/Testing_Data/AutoH2ORegression_QA.csv")
  Sys.sleep(5)
}
