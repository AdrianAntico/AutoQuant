![RemixAutoML_Logo](https://user-images.githubusercontent.com/42076988/55656390-94dc4b00-57ab-11e9-9e3f-06b049b796d5.png)


# Install the package in R via:
```
# Depending on the development state (future versions, etc.) you can install by pasting the below into your R session:
devtools::install_github('AdrianAntico/RemixAutoML', upgrade = FALSE)
```
or
```
devtools::install_github('AdrianAntico/RemixAutoML', force = TRUE, dependencies = TRUE, upgrade = FALSE)
```

# RemixAutoML <img src="https://github.com/AdrianAntico/RemixAutoML/blob/master/RemixAutoML-hexSticker.png" align="right" width="120" />
> This is a collection of functions that I have made to speed up machine learning and to ensure high quality modeling output is generated. They are great at establishing solid baselines that are extremely challenging to beat using alternative methods. To see them in action, check out our free tutorials at <a href="http://www.remyxcourses.com/course?courseid=intro-to-remixautoml-in-r" target="_blank">RemyxCourses.com</a>.

Also, be sure to visit our blog at <a href="http://www.remixinstitute.com" target="_blank">RemixInstitute.ai</a> for data science, machine learning, and AI content.

You can also contact me via <a href="https://www.linkedin.com/in/adrian-antico/" target="_blank">LinkedIn</a> for any questions about the package. You can also go into the vignettes folder to see more detail. If you want to be a contributer, contact me via LinkedIn email.

Hex sticker rendered via the <code>hexSticker</code> package in R: https://github.com/GuangchuangYu/hexSticker

## Supervised Learning Functions: 
##### **AutoCatBoostRegression()**
AutoCatBoostRegression is an automated modeling function that runs a variety of steps for classical expected value regression along with quantile regression. First, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation plot, evaluation metrics, variable importance, partial dependence calibration plots, partial dependence calibration box plots, and column names used in model fitting.

##### **AutoXGBoostRegression()**
AutoXGBoostRegression is an automated XGBoost modeling framework with grid-tuning and model evaluation that runs a variety of steps. First, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation plot, evaluation boxplot, evaluation metrics, variable importance, partial dependence calibration plots, partial dependence calibration box plots, and column names used in model fitting.

##### **AutoH2oGBMRegression()**
AutoH2oGBMRegression is an automated H2O modeling framework with grid-tuning and model evaluation that runs a variety of steps. First, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation plot, evaluation boxplot, evaluation metrics, variable importance, partial dependence calibration plots, partial dependence calibration box plots, and column names used in model fitting.

##### **AutoH2oDRFRegression()**
AutoH2oDRFRegression is an automated H2O modeling framework with grid-tuning and model evaluation that runs a variety of steps. First, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation plot, evaluation boxplot, evaluation metrics, variable importance, partial dependence calibration plots, partial dependence calibration box plots, and column names used in model fitting.

##### **AutoCatBoostClassifier()**
AutoCatBoostClassifier is an automated modeling function that runs a variety of steps. First, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, ROC plot, evaluation plot, evaluation metrics, variable importance, partial dependence calibration plots, partial dependence calibration box plots, and column names used in model fitting.

##### **AutoH2oDRFClassifier()**
AutoH2oDRFRegression is an automated H2O modeling framework with grid-tuning and model evaluation that runs a variety of steps. First, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation plot, evaluation boxplot, evaluation metrics, variable importance, partial dependence calibration plots, partial dependence calibration box plots, and column names used in model fitting.

##### **AutoH2oGBMClassifier()**
AutoH2oDRFRegression is an automated H2O modeling framework with grid-tuning and model evaluation that runs a variety of steps. First, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation plot, evaluation boxplot, evaluation metrics, variable importance, partial dependence calibration plots, partial dependence calibration box plots, and column names used in model fitting.

##### **AutoCatBoostMultiClass()**
AutoCatBoostMultiClass is an automated modeling function that runs a variety of steps. First, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation metrics, variable importance, and column names used in model fitting.

##### **AutoXGBoostMultiClass()**
AutoCatBoostMultiClass is an automated modeling function that runs a variety of steps. First, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation metrics, variable importance, and column names used in model fitting.

##### **AutoH2oGBMMultiClass()**
AutoH2oGBMMultiClass is an automated H2O modeling framework with grid-tuning and model evaluation that runs a variety of steps. First, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation metrics, confusion matrix, and variable importance.

##### **AutoH2oDRFMultiClass()**
AutoH2oDRFMultiClass is an automated H2O modeling framework with grid-tuning and model evaluation that runs a variety of steps. First, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation metrics, confusion matrix, and variable importance.

##### **AutoH2OModeler()**
Automated machine learning. Automatically build any number of models along with generating partial dependence calibration plots, model evaluation calibration plots, grid tuning, and file storage for easy production implementation. Handles regression, quantile regression, time until event, and classification models (binary and multinomial) using numeric and factor variables without the need for monotonic transformations nor one-hot-encoding.
* Models include:
  * RandomForest (DRF)
  * GBM
  * Deeplearning
  * XGBoost (for Linux)
  * LightGBM (for Linux)
  * AutoML - medium debth grid tuning for Deeplearning, XGBoost (if available), DRF, GBM, GLM, and StackedEnsembles
  
##### **AutoH2OScoring()**
Scoring models that were built with the AutoH2OModeler, AutoKMeans, and AutoWord2VecModeler functions. Scores models either via mojo or the standard method by loading models into the H2O environment and scoring them. You can choose which output you wish to keep as well. 

##### **AutoTS()** <img src="https://github.com/AdrianAntico/RemixAutoML/blob/master/AutoTS.png" align="right" width="300" />

Automated time series modeling function. Automatically finds the most accurate time series model from the list of models below (using optimized Box-Cox transformations and tests both user-supplied time series frequency and model-based time series frequency). The best model is chosen by looking at the lowest out-of-sample error, and the output from <code>AutoTS()</code> includes forecasts, model evaluation metrics, and metadata on the competing models.

* Automated Time Series Models include:
  * DSHW: Double Seasonal Holt-Winters
  * ARFIMA: Auto Regressive Fractional Integrated Moving Average
  * ARIMA: Stepwise Auto Regressive Integrated Moving Average with specified max lags, seasonal lags, moving averages, and seasonal moving averages
  * ETS: Additive and Multiplicative Exponential Smoothing and Holt-Winters
  * NNetar: Auto Regressive Neural Network models automatically compares models with 1 lag or 1 seasonal lag compared to models with up to N lags and N seasonal lags
  * TBATS: Exponential smoothing state space model with Box-Cox transformation, ARMA errors, Trend and Seasonal components
  * TSLM: Time Series Linear Model - builds a linear model with trend and season components extracted from the data
 
##### **AutoNLS()**
Automated nonlinear regression modeling. Automatically finds the best model fit from the suite of models below and merges predictions to source data file. Great for forecasting growth over time or estimating single variable nonlinear functions.
* Models included:
  * Asymptotic
  * Asymptotic through origin
  * Asymptotic with offset
  * Bi-exponential
  * Four parameter logistic
  * Three parameter logistic
  * Gompertz
  * Michal Menton
  * Weibull
  * Polynomial regression or monotonic regression

##### **AutoRecommender()**
Automated collaborative filtering modeling where each model competes against each other
  * RandomItems
  * PopularItems
  * UserBasedCF  
  * ItemBasedCF
  * AssociationRules
  
##### **AutoRecommenderScoring()**
Automatically score a recommender model from AutoRecommender

## Unsupervised Learning Functions: 
##### **GenTSAnomVars()**
Generate time series anomaly variables. (Cross with Feature Engineering) Create indicator variables (high, low) along with cumulative anomaly rates (high, low) based on control limits methodology over a max of two grouping variables and a date variable (effectively a rolling GLM).

##### **ResidualOutliers()**
Residual outliers from time series modeling. (Cross with Feature Engineering) Utilize tsoutliers to indicate outliers within a time series data set

##### **AutoKMeans()** 
Generalized low rank model followed by KMeans. (Possible cross with Feature Engineering) Generate a column with a cluster identifier based on a grid tuned (optional) generalized low rank model and a grid tuned (optimal) K-Optimal searching K-Means algorithm

## Feature Engineering Functions: 
##### **FAST_GDL_Feature_Engineering()**
Fast generalized distributed lag feature engineering. Rapidly generate time between events, autoregressive, moving average / standard deviation / min / max / quantile 85 / quantile 95 for when you want to generate these features only for predicting events at the latest time interval of the data set. 100% data.table except for rolling statistics.

##### **GDL_Feature_Engineering()**
Generate a wider set of features (similar in structure to FAST_GDL) using any aggregation statistic for the rolling stats. 100% data.table except for rolling statistics.

##### **Scoring_GDL_Feature_Engineering()**
Generate the model features from FAST_GDL or GDL for scoring purposes when the scoring data is for forward looking predictions (not historical, which can be obtained from FAST_GDL or GDL). 100% data.table.

##### **DT_GDL_Feature_Engineering()**
Lags + Moving Averages, 100% data.table

##### **AutoWord2VecModeler()**
Generate a specified number of vectors for each column of text data in your data set and save the models for re-creating them later in the scoring process.

##### **ModelDataPrep()**
Rapidly convert "inf" values to NA, convert character columns to factor columns, and impute with specified values for factor and numeric columns (factors are necessary (no characters values) for H20).

##### **DummifyDT()** 
Rapidly dichotomize a list of columns in a data table (N+1 columns for N levels using one hot encoding or N columns for N levels otherwise)

## Model Evaluation, Interpretation, and Cost-Sensitive Functions: 
##### **ParDepCalPlots()**
Great for features effects estimation and reliability of model in predicting those effects. Build a partial dependence calibration plot on train, test, or all data

##### **EvalPlot()**
Great for assessing accuracy across range of predicted values. Build a calibration plot on test data

##### **threshOptim()**
Great for situations with asymmetric costs across the confusion matrix. Generate a cost-sensitive optimized threshold for classification models

##### **RedYellowGreen()**
Computes optimal thresholds for binary classification models when "don't classify" is an option

## Utilities and Misc. Functions:
##### **AutoH2OTextPrepScoring()** 
Prepares your data for scoring based on models built with Word2VecModel

##### **RecomDataCreate()** 
Turns your transactional data into a binary ratings matrix

##### **tokenizeH2O()** 
Tokenize and H20 string column.

##### **tempDatesFun()** 
Special case for character conversion to date when importing from Excel.

##### **RemixTheme()** 
Fonts, colors, style for plots.

##### **ChartTheme()** 
Fonts, colors, style for plots.

##### **SimpleCap()** 
Apply proper case to text.

##### **percRank()** 
Inner function for calibration plots and partial dependence plots. Computes PercentRank.

##### **multiplot()** 
Useful for displaying multiple plots in a single pane.

##### **PrintObjectsSize()** 
print out objects and their sizes that are in the envrionment

##### **AutoWordFreq()** 
creates a word frequency data.table and a word cloud

##### **ProblematicFeatures()**
Identify columns that have either little to no variance, extremely high cardinality, too many NA's, too many zeros, or too high of a skew
