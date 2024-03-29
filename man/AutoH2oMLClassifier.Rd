% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AutoH2oMLClassifier.R
\name{AutoH2oMLClassifier}
\alias{AutoH2oMLClassifier}
\title{AutoH2oMLClassifier}
\usage{
AutoH2oMLClassifier(
  OutputSelection = c("EvalMetrics", "Score_TrainData"),
  data = NULL,
  TrainOnFull = FALSE,
  ValidationData = NULL,
  TestData = NULL,
  TargetColumnName = NULL,
  FeatureColNames = NULL,
  ExcludeAlgos = NULL,
  eval_metric = "auc",
  CostMatrixWeights = c(1, 0, 0, 1),
  MaxMem = {
     gc()
    
    paste0(as.character(floor(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",
    intern = TRUE))/1e+06)), "G")
 },
  NThreads = max(1, parallel::detectCores() - 2),
  MaxModelsInGrid = 2,
  model_path = NULL,
  metadata_path = NULL,
  ModelID = "FirstModel",
  NumOfParDepPlots = 3,
  ReturnModelObjects = TRUE,
  SaveModelObjects = FALSE,
  SaveInfoToPDF = TRUE,
  IfSaveModel = "mojo",
  H2OShutdown = TRUE,
  H2OStartUp = TRUE,
  DebugMode = FALSE
)
}
\arguments{
\item{OutputSelection}{You can select what type of output you want returned. Choose from c("EvalMetrics", "Score_TrainData")}

\item{data}{This is your data set for training and testing your model}

\item{TrainOnFull}{Set to TRUE to train on full data}

\item{ValidationData}{This is your holdout data set used in modeling either refine your hyperparameters.}

\item{TestData}{This is your holdout data set. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.}

\item{TargetColumnName}{Either supply the target column name OR the column number where the target is located (but not mixed types). Note that the target column needs to be a 0 | 1 numeric variable.}

\item{FeatureColNames}{Either supply the feature column names OR the column number where the target is located (but not mixed types)}

\item{ExcludeAlgos}{"DRF","GLM","XGBoost","GBM","DeepLearning" and "Stacke-dEnsemble"}

\item{eval_metric}{This is the metric used to identify best grid tuned model. Choose from "AUC" or "logloss"}

\item{CostMatrixWeights}{A vector with 4 elements c(True Positive Cost, False Negative Cost, False Positive Cost, True Negative Cost). Default c(1,0,0,1),}

\item{MaxMem}{Set the maximum amount of memory you'd like to dedicate to the model run. E.g. "32G"}

\item{NThreads}{Set the number of threads you want to dedicate to the model building}

\item{MaxModelsInGrid}{Number of models to test from grid options (1080 total possible options)}

\item{model_path}{A character string of your path file to where you want your output saved}

\item{metadata_path}{A character string of your path file to where you want your model evaluation output saved. If left NULL, all output will be saved to model_path.}

\item{ModelID}{A character string to name your model and output}

\item{NumOfParDepPlots}{Tell the function the number of partial dependence calibration plots you want to create.}

\item{ReturnModelObjects}{Set to TRUE to output all modeling objects (E.g. plots and evaluation metrics)}

\item{SaveModelObjects}{Set to TRUE to return all modeling objects to your environment}

\item{SaveInfoToPDF}{Set to TRUE to print model insights to PDF}

\item{IfSaveModel}{Set to "mojo" to save a mojo file, otherwise "standard" to save a regular H2O model object}

\item{H2OShutdown}{Set to TRUE to shutdown H2O after running the function}

\item{H2OStartUp}{Set to FALSE}

\item{DebugMode}{Set to TRUE to print out steps taken}
}
\value{
Saves to file and returned in list: VariableImportance.csv, Model, ValidationData.csv, EvalutionPlot.png, EvaluationMetrics.csv, ParDepPlots.R a named list of features with partial dependence calibration plots, GridCollect, and GridList
}
\description{
AutoH2oMLClassifier is an automated H2O modeling framework with grid-tuning and model evaluation that runs a variety of steps. First, a stratified sampling (by the target variable) is done to create train and validation sets. Then, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation plot, evaluation metrics, variable importance, partial dependence calibration plots, and column names used in model fitting.
}
\examples{
\donttest{
# Create some dummy correlated data with numeric and categorical features
data <- AutoQuant::FakeDataGenerator(
  Correlation = 0.85,
  N = 1000L,
  ID = 2L,
  ZIP = 0L,
  AddDate = FALSE,
  Classification = TRUE,
  MultiClass = FALSE)

TestModel <- AutoQuant::AutoH2oMLClassifier(
  OutputSelection = c("EvalMetrics", "Score_TrainData"),
  data,
  TrainOnFull = FALSE,
  ValidationData = NULL,
  TestData = NULL,
  TargetColumnName = "Adrian",
  FeatureColNames = names(data)[!names(data) \%in\% c("IDcol_1", "IDcol_2","Adrian")],
  ExcludeAlgos = NULL,
  eval_metric = "auc",
  CostMatrixWeights = c(1,0,0,1),
  MaxMem = {gc();paste0(as.character(floor(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE)) / 1000000)),"G")},
  NThreads = max(1, parallel::detectCores()-2),
  MaxModelsInGrid = 10,
  model_path = normalizePath("./"),
  metadata_path = normalizePath("./"),
  ModelID = "FirstModel",
  NumOfParDepPlots = 3,
  ReturnModelObjects = TRUE,
  SaveModelObjects = FALSE,
  SaveInfoToPDF = TRUE,
  IfSaveModel = "mojo",
  H2OShutdown = TRUE,
  H2OStartUp = TRUE,
  DebugMode = FALSE)
}
}
\seealso{
Other Automated Supervised Learning - Binary Classification: 
\code{\link{AutoCatBoostClassifier}()},
\code{\link{AutoH2oDRFClassifier}()},
\code{\link{AutoH2oGAMClassifier}()},
\code{\link{AutoH2oGBMClassifier}()},
\code{\link{AutoH2oGLMClassifier}()},
\code{\link{AutoLightGBMClassifier}()},
\code{\link{AutoXGBoostClassifier}()}
}
\author{
Adrian Antico
}
\concept{Automated Supervised Learning - Binary Classification}
