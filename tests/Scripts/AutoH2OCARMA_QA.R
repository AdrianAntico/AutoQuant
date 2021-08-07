# Collection data.table
QA_Results <- data.table::CJ(
  Group = c(0,1,2,3),
  xregs = c(0,1,2,3),
  TOF = c(TRUE, FALSE),
  Trans = c(TRUE, FALSE),
  Diff = c(TRUE, FALSE))

# Other tests
QA_Results[, TimeWeights := data.table::fifelse(runif(.N) < 0.5, 0.9999, 1)]
QA_Results[, Success := "Failure"]

# NOT Train On FULL TOF
# run = 101
for(run in seq_len(QA_Results[,.N])) {# seq_len(QA_Results[,.N])) {

  # Data
  if(QA_Results[run, Group] == 0L) {
    data <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/NoGroup-Eval-Walmart.csv")
  } else if(QA_Results[run, Group] == 1L) {
    data <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/OneGroup-Eval-Walmart.csv")
  } else if(QA_Results[run, Group] == 2L) {
    data <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/TwoGroup-Eval-Walmart.csv")
  } else if(QA_Results[run, Group] == 3L) {
    data <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/ThreeGroup-Eval-Walmart.csv")
  }

  # xregs
  if(QA_Results[run, xregs] == 0L) {
    xregs <- NULL
  } else if(QA_Results[run, xregs] == 1L) {
    if(QA_Results[run, Group] == 0L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/NoGroup-FC-Walmart-XREG1.csv")
    if(QA_Results[run, Group] == 1L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/OneGroup-FC-Walmart-XREG1.csv")
    if(QA_Results[run, Group] == 2L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/TwoGroup-FC-Walmart-XREG1.csv")
    if(QA_Results[run, Group] == 3L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/ThreeGroup-FC-Walmart-XREG1.csv")
  } else if(QA_Results[run, xregs] == 2L) {
    if(QA_Results[run, Group] == 0L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/NoGroup-FC-Walmart-XREG2.csv")
    if(QA_Results[run, Group] == 1L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/OneGroup-FC-Walmart-XREG2.csv")
    if(QA_Results[run, Group] == 2L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/TwoGroup-FC-Walmart-XREG2.csv")
    if(QA_Results[run, Group] == 3L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/ThreeGroup-FC-Walmart-XREG2.csv")
  } else if(QA_Results[run, xregs] == 3L) {
    if(QA_Results[run, Group] == 0L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/NoGroup-FC-Walmart-XREG3.csv")
    if(QA_Results[run, Group] == 1L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/OneGroup-FC-Walmart-XREG3.csv")
    if(QA_Results[run, Group] == 2L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/TwoGroup-FC-Walmart-XREG3.csv")
    if(QA_Results[run, Group] == 3L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/ThreeGroup-FC-Walmart-XREG3.csv")
  }

  # Testing params
  TOF <- QA_Results[run, TOF]
  Trans <- QA_Results[run, Trans]
  Diff <- QA_Results[run, Diff]
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
  data1 <- data.table::copy(data)
  if(QA_Results[run, xregs] != 0L) xregs1 <- data.table::copy(xregs) else xregs1 <- NULL

  # Build forecast
  TestModel <- tryCatch({RemixAutoML::AutoH2OCARMA(

    # New args
    PDFOutputPath = NULL,
    SaveDataPath = NULL,
    TimeWeights = NULL,
    HolidayLookback = 7,
    LearnRate = 0.10,
    LearnRateAnnealing = 0.999,
    ColSampleRate = 0.615,

    # Data Artifacts
    AlgoType = "drf",
    ExcludeAlgos = NULL,
    data = data1,
    XREGS = xregs1,
    TargetColumnName = "Weekly_Sales",
    DateColumnName = "Date",
    HierarchGroups = NULL,
    GroupVariables = groupvariables,
    TimeUnit = "week",
    TimeGroups = c("weeks","months"),

    # Data Wrangling Features
    SplitRatios = c(1 - 10 / 110, 10 / 110),
    PartitionType = "random",

    # Production args
    FC_Periods = 4L,
    TrainOnFull = TOF,
    MaxMem = "28g",
    NThreads = parallel::detectCores(),
    Timer = TRUE,
    DebugMode = TRUE,

    # Target Transformations
    TargetTransformation = Trans,
    Methods = c("BoxCox", "Asinh", "Asin", "Log", "LogPlus1", "Sqrt", "Logit"),
    Difference = Diff,
    NonNegativePred = FALSE,
    RoundPreds = FALSE,

    # Calendar features
    CalendarVariables = c("week", "wom", "month", "quarter", "year"),
    HolidayVariable = c("USPublicHolidays","EasterGroup", "ChristmasGroup","OtherEcclesticalFeasts"),
    TimeTrendVariable = TRUE,
    HolidayLags = 1:7,
    HolidayMovingAverages = 2:7,

    # Time series features
    Lags = list("weeks" = c(1:4), "months" = c(6)),
    MA_Periods = list("weeks" = c(2:4), "months" = c(6)),
    SD_Periods = NULL,
    Skew_Periods = NULL,
    Kurt_Periods = NULL,
    Quantile_Periods = NULL,
    Quantiles_Selected = NULL,

    # Bonus Features
    FourierTerms = 0L,
    AnomalyDetection = NULL,
    ZeroPadSeries = NULL,
    DataTruncate = FALSE,

    # ML evaluation args
    EvalMetric = "RMSE",
    NumOfParDepPlots = 0L,

    # ML grid tuning args
    GridTune = FALSE,
    GridStrategy = "RandomDiscrete",
    ModelCount = 5,
    MaxRunTimeSecs = 60*60*24,
    StoppingRounds = 10,

    # ML Args
    NTrees = 50L,
    MaxDepth = 20,
    SampleRate = 0.632,
    MTries = -1,
    ColSampleRatePerTree = 1,
    ColSampleRatePerTreeLevel  = 1,
    MinRows = 1,
    NBins = 20,
    NBinsCats = 1024,
    NBinsTopLevel = 1024,
    HistogramType = "AUTO",
    CategoricalEncoding = "AUTO",
    RandomColNumbers = NULL,
    InteractionColNumbers = NULL,

    # GLM and GAM
    Distribution = "gaussian",
    Link = "identity",
    RandomDistribution = NULL,
    RandomLink = NULL,
    Solver = "AUTO",
    Alpha = NULL,
    Lambda = NULL,
    LambdaSearch = FALSE,
    NLambdas = -1,
    Standardize = TRUE,
    RemoveCollinearColumns = FALSE,
    InterceptInclude = TRUE,
    NonNegativeCoefficients = FALSE)}, error = function(x) NULL)

  # Outcome
  if(!is.null(TestModel)) QA_Results[run, Success := "Success"]
  rm(TestModel)
  data.table::fwrite(QA_Results, file = "C:/Users/Bizon/Documents/GitHub/QA_Code/QA_CSV/AutoH2OCARMA_QA.csv")
  Sys.sleep(5)
}

# Run Through H2O CARMA
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

run = 101

# Data
if(QA_Results[run, Group] == 0L) {
  data <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/NoGroup-Eval-Walmart.csv")
} else if(QA_Results[run, Group] == 1L) {
  data <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/OneGroup-Eval-Walmart.csv")
} else if(QA_Results[run, Group] == 2L) {
  data <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/TwoGroup-Eval-Walmart.csv")
} else if(QA_Results[run, Group] == 3L) {
  data <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/ThreeGroup-Eval-Walmart.csv")
}

# xregs
if(QA_Results[run, xregs] == 0L) {
  xregs <- NULL
} else if(QA_Results[run, xregs] == 1L) {
  if(QA_Results[run, Group] == 0L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/NoGroup-FC-Walmart-XREG1.csv")
  if(QA_Results[run, Group] == 1L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/OneGroup-FC-Walmart-XREG1.csv")
  if(QA_Results[run, Group] == 2L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/TwoGroup-FC-Walmart-XREG1.csv")
  if(QA_Results[run, Group] == 3L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/ThreeGroup-FC-Walmart-XREG1.csv")
} else if(QA_Results[run, xregs] == 2L) {
  if(QA_Results[run, Group] == 0L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/NoGroup-FC-Walmart-XREG2.csv")
  if(QA_Results[run, Group] == 1L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/OneGroup-FC-Walmart-XREG2.csv")
  if(QA_Results[run, Group] == 2L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/TwoGroup-FC-Walmart-XREG2.csv")
  if(QA_Results[run, Group] == 3L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/ThreeGroup-FC-Walmart-XREG2.csv")
} else if(QA_Results[run, xregs] == 3L) {
  if(QA_Results[run, Group] == 0L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/NoGroup-FC-Walmart-XREG3.csv")
  if(QA_Results[run, Group] == 1L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/OneGroup-FC-Walmart-XREG3.csv")
  if(QA_Results[run, Group] == 2L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/TwoGroup-FC-Walmart-XREG3.csv")
  if(QA_Results[run, Group] == 3L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/ThreeGroup-FC-Walmart-XREG3.csv")
}

# Testing params
TOF <- QA_Results[run, TOF]
Trans <- QA_Results[run, Trans]
Diff <- QA_Results[run, Diff]
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
data1 <- data.table::copy(data)
if(QA_Results[run, xregs] != 0L) xregs1 <- data.table::copy(xregs) else xregs1 <- NULL


# New args
AlgoType = "drf"
PDFOutputPath = NULL
SaveDataPath = NULL
TimeWeights = NULL
HolidayLookback = 7
LearnRate = 0.10
LearnRateAnnealing = 0.999
ColSampleRate = 0.615
ExcludeAlgos = NULL
data = data1
XREGS = xregs1
TargetColumnName = "Weekly_Sales"
DateColumnName = "Date"
HierarchGroups = NULL
GroupVariables = groupvariables
TimeUnit = "week"
TimeGroups = c("weeks","months")
SplitRatios = c(1 - 10 / 110, 10 / 110)
PartitionType = "random"
FC_Periods = 4L
TrainOnFull = TOF
MaxMem = "28g"
NThreads = parallel::detectCores()
Timer = TRUE
DebugMode = TRUE
TargetTransformation = Trans
Methods = c("BoxCox", "Asinh", "Asin", "Log", "LogPlus1", "Sqrt", "Logit")
Difference = Diff
NonNegativePred = FALSE
RoundPreds = FALSE
CalendarVariables = c("week", "wom", "month", "quarter", "year")
HolidayVariable = c("USPublicHolidays","EasterGroup", "ChristmasGroup","OtherEcclesticalFeasts")
TimeTrendVariable = TRUE
HolidayLags = 1:7
HolidayMovingAverages = 2:7
Lags = list("weeks" = c(1:4), "months" = c(6))
MA_Periods = list("weeks" = c(2:4), "months" = c(6))
SD_Periods = NULL
Skew_Periods = NULL
Kurt_Periods = NULL
Quantile_Periods = NULL
Quantiles_Selected = NULL
FourierTerms = 2L
AnomalyDetection = NULL
ZeroPadSeries = NULL
DataTruncate = FALSE
EvalMetric = "RMSE"
NumOfParDepPlots = 0L
GridTune = FALSE
GridStrategy = "RandomDiscrete"
ModelCount = 5
MaxRunTimeSecs = 60*60*24
StoppingRounds = 10
NTrees = 50L
MaxDepth = 20
SampleRate = 0.632
MTries = -1
ColSampleRatePerTree = 1
ColSampleRatePerTreeLevel  = 1
MinRows = 1
NBins = 20
NBinsCats = 1024
NBinsTopLevel = 1024
HistogramType = "AUTO"
CategoricalEncoding = "AUTO"
RandomColNumbers = NULL
InteractionColNumbers = NULL
WeightsColumn = NULL
Distribution = "gaussian"
Link = "identity"
RandomDistribution = NULL
RandomLink = NULL
Solver = "AUTO"
Alpha = NULL
Lambda = NULL
LambdaSearch = FALSE
NLambdas = -1
Standardize = TRUE
RemoveCollinearColumns = FALSE
InterceptInclude = TRUE
NonNegativeCoefficients = FALSE

# Model Scoring
ScoringData = Step1SCore
ModelObject = TestModel$Model
ModelType = "mojo"
H2OShutdown = FALSE
H2OStartUp = FALSE
MaxMem = MaxMem
JavaOptions = NULL
ModelPath = NULL
ModelID = paste0(AlgoType, "_Carma")
ReturnFeatures = TRUE
TransformNumeric = FALSE
BackTransNumeric = FALSE
TargetColumnName = NULL
TransformationObject = NULL
TransID = NULL
TransPath = NULL
MDP_Impute = TRUE
MDP_CharToFactor = TRUE
MDP_RemoveDates = FALSE
MDP_MissFactor = "0"
MDP_MissNum = -1

# Carma Update Features
UpdateData.=UpdateData
GroupVariables.=GroupVariables
CalendarFeatures.=CalendarFeatures
CalendarVariables.=CalendarVariables
GroupVarVector.=GroupVarVector
DateColumnName.=DateColumnName
XREGS.=XREGS
FourierTerms.=FourierTerms
FourierFC.=FourierFC
TimeGroups.=TimeGroups
TimeTrendVariable.=TimeTrendVariable
N.=N
TargetColumnName.=TargetColumnName
HolidayVariable.=HolidayVariable
HolidayLookback.=HolidayLookback
TimeUnit.=TimeUnit
AnomalyDetection.=AnomalyDetection
i.=i



