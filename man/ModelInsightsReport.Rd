% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ModelInsights.R
\name{ModelInsightsReport}
\alias{ModelInsightsReport}
\title{ModelInsightsReport}
\usage{
ModelInsightsReport(
  KeepOutput = NULL,
  TrainData = NULL,
  ValidationData = NULL,
  TestData = NULL,
  TargetColumnName = NULL,
  PredictionColumnName = "Predict",
  FeatureColumnNames = NULL,
  DateColumnName = NULL,
  TargetType = "regression",
  ModelID = "ModelTest",
  Algo = "catboost",
  SourcePath = NULL,
  OutputPath = NULL,
  ModelObject = NULL,
  Test_Importance_dt = NULL,
  Validation_Importance_dt = NULL,
  Train_Importance_dt = NULL,
  Test_Interaction_dt = NULL,
  Validation_Interaction_dt = NULL,
  Train_Interaction_dt = NULL,
  GlobalVars = ls()
)
}
\arguments{
\item{KeepOutput}{NULL A list of output names to select. Pass in as a character vector. E.g. c('Test_VariableImportance', 'Train_VariableImportance')}

\item{TrainData}{data.table or something that converts to data.table via as.data.table}

\item{ValidationData}{data.table or something that converts to data.table via as.data.table}

\item{TestData}{data.table or something that converts to data.table via as.data.table}

\item{TargetColumnName}{NULL. Target variable column name as character}

\item{PredictionColumnName}{NULL. Predicted value column name as character. 'p1' for AutoQuant functions}

\item{FeatureColumnNames}{NULL. Feature column names as character vector.}

\item{DateColumnName}{NULL. Date column name as character}

\item{TargetType}{'regression', 'classification', or 'multiclass'}

\item{ModelID}{ModelID used in the AutoQuant supervised learning function}

\item{Algo}{'catboost' or 'other'. Use 'catboost' if using AutoQuant::AutoCatBoost_() functions. Otherwise, 'other'}

\item{SourcePath}{Path to directory with AutoQuant Model Output}

\item{OutputPath}{Path to directory where the html will be saved}

\item{ModelObject}{Returned output from regression, classificaiton, and multiclass Remix Auto_() models. Currenly supports CatBoost, XGBoost, and LightGBM models}

\item{Test_Importance_dt}{NULL.. Ignore if using AutoQuant Models. Otherwise, supply a two column data.table with colnames 'Variable' and 'Importance'}

\item{Validation_Importance_dt}{NULL.. Ignore if using AutoQuant Models. Otherwise, supply a two column data.table with colnames 'Variable' and 'Importance'}

\item{Train_Importance_dt}{NULL.. Ignore if using AutoQuant Models. Otherwise, supply a two column data.table with colnames 'Variable' and 'Importance'}

\item{Test_Interaction_dt}{NULL.. Ignore if using AutoQuant Models. Otherwise, supply a three column data.table with colnames 'Features1', 'Features2'  and 'score'}

\item{Validation_Interaction_dt}{NULL.. Ignore if using AutoQuant Models. Otherwise, supply a three column data.table with colnames 'Features1', 'Features2'  and 'score'}

\item{Train_Interaction_dt}{NULL.. Ignore if using AutoQuant Models. Otherwise, supply a three column data.table with colnames 'Features1', 'Features2'  and 'score'}

\item{GlobalVars}{ls() don't use}

\item{Path}{Path to Model Output if ModelObject is left NULL}
}
\description{
ModelInsightsReport is an Rmarkdown report for viewing the model insights generated by AutoQuant supervised learning functions
}
\examples{
\dontrun{

#####################################################
# CatBoost
#####################################################

# Create some dummy correlated data
data <- AutoQuant::FakeDataGenerator(
  Correlation = 0.85,
  N = 10000,
  ID = 2,
  ZIP = 0,
  AddDate = FALSE,
  Classification = FALSE,
  MultiClass = FALSE)

# Copy data
data1 <- data.table::copy(data)

# Run function
ModelObject <- AutoQuant::AutoCatBoostRegression(

  # GPU or CPU and the number of available GPUs
  TrainOnFull = FALSE,
  task_type = 'GPU',
  NumGPUs = 1,
  DebugMode = FALSE,

  # Metadata args
  OutputSelection = c('Importances','EvalPlots','EvalMetrics','Score_TrainData'),
  ModelID = 'Test_Model_1',
  model_path = getwd(),
  metadata_path = getwd(),
  SaveModelObjects = FALSE,
  SaveInfoToPDF = FALSE,
  ReturnModelObjects = TRUE,

  # Data args
  data = data1,
  ValidationData = NULL,
  TestData = NULL,
  TargetColumnName = 'Adrian',
  FeatureColNames = names(data1)[!names(data1) \%in\% c('IDcol_1','IDcol_2','Adrian')],
  PrimaryDateColumn = NULL,
  WeightsColumnName = NULL,
  IDcols = c('IDcol_1','IDcol_2'),
  TransformNumericColumns = 'Adrian',
  Methods = c('Asinh','Asin','Log','LogPlus1','Sqrt','Logit'),

  # Model evaluation
  eval_metric = 'RMSE',
  eval_metric_value = 1.5,
  loss_function = 'RMSE',
  loss_function_value = 1.5,
  MetricPeriods = 10L,
  NumOfParDepPlots = ncol(data1)-1L-2L,

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
  Trees = 500,
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

# Create Model Insights Report
AutoQuant::ModelInsightsReport(

  # Items to keep in global environment when
  #   function finishes execution
  KeepOutput = 'Test_VariableImportance',

  # DataSets
  TrainData = NULL,
  ValidationData = NULL,
  TestData = NULL,

  # Meta info
  TargetColumnName = NULL,
  PredictionColumnName = NULL,
  FeatureColumnNames = NULL,
  DateColumnName = NULL,

  # Variable Importance
  Test_Importance_dt = NULL,
  Validation_Importance_dt = NULL,
  Train_Importance_dt = NULL,
  Test_Interaction_dt = NULL,
  Validation_Interaction_dt = NULL,
  Train_Interaction_dt = NULL,

  # Control options
  TargetType = 'regression',
  ModelID = 'ModelTest',
  Algo = 'catboost',
  SourcePath = getwd(),
  OutputPath = getwd(),
  ModelObject = ModelObject)
}

}
\seealso{
Other Model Insights: 
\code{\link{ShapImportancePlot}()}
}
\author{
Adrian Antico
}
\concept{Model Insights}
