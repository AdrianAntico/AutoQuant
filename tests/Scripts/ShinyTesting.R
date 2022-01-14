data <- RemixAutoML::FakeDataGenerator(
  Correlation = 0.85,
  N = 10000,
  ID = 2,
  ZIP = 0,
  AddDate = FALSE,
  Classification = TRUE,
  MultiClass = FALSE)

# Run function
TestModel <- RemixAutoML::AutoCatBoostClassifier(

  # GPU or CPU and the number of available GPUs
  task_type = 'GPU',
  NumGPUs = 1,
  TrainOnFull = FALSE,
  DebugMode = FALSE,

  # Metadata args
  OutputSelection = c('Score_TrainData', 'Importances', 'EvalPlots', 'EvalMetrics'),
  ModelID = 'Test_Model_1',
  model_path = normalizePath('./'),
  metadata_path = normalizePath('./'),
  SaveModelObjects = FALSE,
  ReturnModelObjects = TRUE,
  SaveInfoToPDF = FALSE,

  # Data args
  data = data,
  ValidationData = NULL,
  TestData = NULL,
  TargetColumnName = 'Adrian',
  FeatureColNames = names(data)[!names(data) %in%
     c('IDcol_1','IDcol_2','Adrian')],
  PrimaryDateColumn = NULL,
  WeightsColumnName = NULL,
  IDcols = c('IDcol_1','IDcol_2'),

  # Evaluation args
  ClassWeights = c(1L,1L),
  CostMatrixWeights = c(1,0,0,1),
  EvalMetric = 'AUC',
  grid_eval_metric = 'MCC',
  LossFunction = 'Logloss',
  MetricPeriods = 10L,
  NumOfParDepPlots = ncol(data)-1L-2L,

  # Grid tuning args
  PassInGrid = NULL,
  GridTune = FALSE,
  MaxModelsInGrid = 30L,
  MaxRunsWithoutNewWinner = 20L,
  MaxRunMinutes = 24L*60L,
  BaselineComparison = 'default',

  # ML args
  Trees = 1000,
  Depth = 9,
  LearningRate = NULL,
  L2_Leaf_Reg = NULL,
  model_size_reg = 0.5,
  langevin = FALSE,
  diffusion_temperature = 10000,
  RandomStrength = 1,
  BorderCount = 128,
  RSM = 1,
  BootStrapType = 'Bayesian',
  GrowPolicy = 'SymmetricTree',
  feature_border_type = 'GreedyLogSum',
  sampling_unit = 'Object',
  subsample = NULL,
  score_function = 'Cosine',
  min_data_in_leaf = 1)

TestModel$Model <- NULL
save(TestModel, file = file.choose())


# Create some dummy correlated data
data <- RemixAutoML::FakeDataGenerator(
  Correlation = 0.85,
  N = 10000,
  ID = 2,
  ZIP = 0,
  AddDate = FALSE,
  Classification = FALSE,
  MultiClass = FALSE)

# Run function
TestModel <- RemixAutoML::AutoCatBoostRegression(

  # GPU or CPU and the number of available GPUs
  TrainOnFull = FALSE,
  task_type = 'GPU',
  NumGPUs = 1,
  DebugMode = FALSE,

  # Metadata args
  OutputSelection = c('Importances', 'EvalPlots', 'EvalMetrics', 'Score_TrainData'),
  ModelID = 'Test_Model_1',
  model_path = normalizePath('./'),
  metadata_path = normalizePath('./'),
  SaveModelObjects = FALSE,
  SaveInfoToPDF = FALSE,
  ReturnModelObjects = TRUE,

  # Data args
  data = data,
  ValidationData = NULL,
  TestData = NULL,
  TargetColumnName = 'Adrian',
  FeatureColNames = names(data)[!names(data) %in%
    c('IDcol_1', 'IDcol_2','Adrian')],
  PrimaryDateColumn = NULL,
  WeightsColumnName = NULL,
  IDcols = c('IDcol_1','IDcol_2'),
  TransformNumericColumns = 'Adrian',
  Methods = c('BoxCox', 'Asinh', 'Asin', 'Log',
    'LogPlus1', 'Sqrt', 'Logit'),

  # Model evaluation
  eval_metric = 'RMSE',
  eval_metric_value = 1.5,
  loss_function = 'RMSE',
  loss_function_value = 1.5,
  MetricPeriods = 10L,
  NumOfParDepPlots = ncol(data)-1L-2L,

  # Grid tuning args
  PassInGrid = NULL,
  GridTune = FALSE,
  MaxModelsInGrid = 30L,
  MaxRunsWithoutNewWinner = 20L,
  MaxRunMinutes = 60*60,
  BaselineComparison = 'default',

  # ML args
  langevin = FALSE,
  diffusion_temperature = 10000,
  Trees = 100,
  Depth = 9,
  L2_Leaf_Reg = NULL,
  RandomStrength = 1,
  BorderCount = 128,
  LearningRate = NULL,
  RSM = 1,
  BootStrapType = NULL,
  GrowPolicy = 'SymmetricTree',
  model_size_reg = 0.5,
  feature_border_type = 'GreedyLogSum',
  sampling_unit = 'Object',
  subsample = NULL,
  score_function = 'Cosine',
  min_data_in_leaf = 1)

data <- data.table::rbindlist(list(TestModel$TrainData, TestModel$TestData))
save(TestModel, file = file.path('C:/Users/Bizon/Documents/GitHub/ModelOutput.Rdata'))
data.table::fwrite(data, file = 'C:/Users/Bizon/Documents/GitHub/AppTesting.csv')
# Plotting App

# Pull Data
#data <- data.table::fread(system.file('tests/QA_DataSets/ThreeGroup-FC-Walmart-XREG3.csv', package = "RemixAutoML"))
#data[, Date := as.Date(Date)]

RemixAutoML::AppsPlotting(Debug = TRUE)

RemixAutoML::AppsPlotting(Debug = FALSE)

# Run App
RemixAutoML::AppsPlotting(
  data = NULL, #data, #data.table::fread(system.file('tests/QA_DataSets/ThreeGroup-FC-Walmart-XREG3.csv', package = "RemixAutoML")),
  XVariable = NULL,
  YVariable = NULL,
  DateName = NULL, #'Date',
  GroupVariables = NULL, #@names(data)[seq_len(3L)],
  FilterVariable = NULL,
  ModelOutputList = NULL, #TestModel,
  HeaderColor = 'black',
  AppWidth = 12L,
  LogoWidth = '1000px',
  LogoHeight = '100px',
  GroupVarsBoxColor = 'navy',
  VarsBoxColor = 'purple',
  FilterBoxColor = 'blue',
  PlotBoxColor = 'aqua',
  H3Color = 'darkblue',
  H4Color = 'darkblue',
  AppTextColor = 'blue',
  CreatePlotButtonColor = 'primary',
  UpdatePlotButtonColor = 'primary',
  ResetPlotButtonColor = 'primary',
  Browser = FALSE,
  Docker = FALSE,
  Debug = TRUE)


data <- RemixAutoML::FakeDataGenerator(N = 10000)
p1 <- ggplot2::ggplot(data = data, ggplot2::aes(x = Adrian)) + ggplot2::geom_histogram()
p2 <- ggplot2::ggplot(data = data, ggplot2::aes(x = Adrian)) + ggplot2::geom_histogram()
p3 <- ggplot2::ggplot(data = data, ggplot2::aes(x = Adrian)) + ggplot2::geom_histogram()
p4 <- ggplot2::ggplot(data = data, ggplot2::aes(x = Adrian)) + ggplot2::geom_histogram()

gridExtra::grid.arrange(p1,p2,p3,p4, ncol=2)


# install.packages("https://github.com/AdrianAntico/RemixAutoML/raw/master/RemixAutoML_0.6.0.tar.gz", repos = NULL, type = "source", INSTALL_opts = c("--no-multiarch", "--no-test-load"))

# shinyWidgets::dropdownButton(
#   tags$h3("List of Inputs"),
#   selectInput(inputId = 'xcol',
#               label = 'X Variable',
#               choices = names(iris)),
#   sliderInput(inputId = 'clusters',
#               label = 'Cluster count',
#               value = 3,
#               min = 1,
#               max = 9),
#   actionButton(inputId = "toggle2",
#                label = "Close dropdown"),
#   circle = TRUE, status = "danger",
#   inputId = "mydropdown",
#   icon = icon("gear"), width = "300px"
# )
#
#
#
#
# # RunRemixAutoML()
#
# # Create some dummy correlated data
# data <- RemixAutoML::FakeDataGenerator(
#   Correlation = 0.85,
#   N = 10000,
#   ID = 2,
#   ZIP = 0,
#   AddDate = FALSE,
#   Classification = FALSE,
#   MultiClass = FALSE)
#
# # Run function
# TestModel <- RemixAutoML::AutoCatBoostRegression(
#
#   # GPU or CPU and the number of available GPUs
#   TrainOnFull = FALSE,
#   task_type = 'GPU',
#   NumGPUs = 1,
#   DebugMode = FALSE,
#
#   # Metadata args
#   OutputSelection = c('Importances', 'EvalPlots', 'EvalMetrics', 'Score_TrainData'),
#   ModelID = 'Test_Model_1',
#   model_path = normalizePath('./'),
#   metadata_path = normalizePath('./'),
#   SaveModelObjects = FALSE,
#   SaveInfoToPDF = FALSE,
#   ReturnModelObjects = TRUE,
#
#   # Data args
#   data = data,
#   ValidationData = NULL,
#   TestData = NULL,
#   TargetColumnName = 'Adrian',
#   FeatureColNames = names(data)[!names(data) %in%
#     c('IDcol_1', 'IDcol_2','Adrian')],
#   PrimaryDateColumn = NULL,
#   WeightsColumnName = NULL,
#   IDcols = c('IDcol_1','IDcol_2'),
#   TransformNumericColumns = 'Adrian',
#   Methods = c('BoxCox', 'Asinh', 'Asin', 'Log',
#     'LogPlus1', 'Sqrt', 'Logit'),
#
#   # Model evaluation
#   eval_metric = 'RMSE',
#   eval_metric_value = 1.5,
#   loss_function = 'RMSE',
#   loss_function_value = 1.5,
#   MetricPeriods = 10L,
#   NumOfParDepPlots = ncol(data)-1L-2L,
#
#   # Grid tuning args
#   PassInGrid = NULL,
#   GridTune = FALSE,
#   MaxModelsInGrid = 30L,
#   MaxRunsWithoutNewWinner = 20L,
#   MaxRunMinutes = 60*60,
#   BaselineComparison = 'default',
#
#   # ML args
#   langevin = FALSE,
#   diffusion_temperature = 10000,
#   Trees = 100,
#   Depth = 9,
#   L2_Leaf_Reg = NULL,
#   RandomStrength = 1,
#   BorderCount = 128,
#   LearningRate = NULL,
#   RSM = 1,
#   BootStrapType = NULL,
#   GrowPolicy = 'SymmetricTree',
#   model_size_reg = 0.5,
#   feature_border_type = 'GreedyLogSum',
#   sampling_unit = 'Object',
#   subsample = NULL,
#   score_function = 'Cosine',
#   min_data_in_leaf = 1)
#
# # Combine Data
# data <- data.table::rbindlist(list(TestModel$TrainData, TestModel$TestData))
#
# RemixAutoML::RunRemixAutoML(data = data, ModelOutput = TestModel, TargetName = 'Adrian', PredictName = 'Predict', DateName = NULL, Debug = TRUE)
#
