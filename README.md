![RemixAutoML_Logo](https://user-images.githubusercontent.com/42076988/55656390-94dc4b00-57ab-11e9-9e3f-06b049b796d5.png)

# How to Install the Package for R:

#### 1. First, run the following R script to download dependencies
```
library(devtools)
to_install <- c("arules", "catboost", "caTools", "data.table", "doParallel", 
                "foreach", "forecast", "ggplot2", "h2o", "itertools", 
                "lubridate", "magick", "Matrix", "monreg", "nortest","pROC", "RColorBrewer", "recommenderlab", 
                "ROCR", "scatterplot3d", "stringr", "sde", "tm", "tsoutliers", "wordcloud", "xgboost", "zoo")
for (i in to_install) {
  message(paste("looking for ", i))
  if(i == "catboost" & !requireNamespace(i)) {
    devtools::install_github('catboost/catboost', subdir = 'catboost/R-package')
  } else if(i == "h2o" & !requireNamespace(i)) {
    if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
    if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }
    pkgs <- c("RCurl","jsonlite")
    for (pkg in pkgs) {
      if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
    }
    install.packages("h2o", type="source", repos="https://h2o-release.s3.amazonaws.com/h2o/rel-yates/3/R")
  } else if (!requireNamespace(i)) {
    message(paste("     installing", i))
    install.packages(i)
  }
}
```

#### 2. Next, Install RemixAutoML package from GitHub
```
# Install via:
devtools::install_github('AdrianAntico/RemixAutoML', upgrade = FALSE, dependencies = FALSE, force = TRUE)
```

#### 3. If you're having trouble installing, see if this issue helps you out.
![Issue #19](https://github.com/AdrianAntico/RemixAutoML/issues/19)

# RemixAutoML <img src="https://github.com/AdrianAntico/RemixAutoML/blob/master/RemixAutoML-hexSticker.png" align="right" width="120" />
> This is a collection of functions that I have made to speed up machine learning and to ensure high quality modeling output is generated. They are great at establishing solid baselines that are extremely challenging to beat using alternative methods. To see them in action, check out our free tutorials at <a href="http://www.remyxcourses.com/course?courseid=intro-to-remixautoml-in-r" target="_blank">RemyxCourses.com</a>.

Also, be sure to visit our blog at <a href="http://www.remixinstitute.com" target="_blank">RemixInstitute.ai</a> for data science, machine learning, and AI content.

You can contact me via <a href="https://www.linkedin.com/in/adrian-antico/" target="_blank">LinkedIn</a> for any questions about the package. You can also go into the vignettes folder to see the package reference manual and a vignette with some background and examples. If you want to be a contributer, contact me via LinkedIn email.

Hex sticker rendered via the <code>hexSticker</code> package in R: https://github.com/GuangchuangYu/hexSticker

## Supervised Learning Training Functions: 
<details><summary>EXPAND</summary>
<p>

#### Regression:
____________________________________________________________________________________________________________________________________________
<details><summary>expand</summary>
<p>
 
##### **AutoCatBoostRegression()** GPU + CPU
<code>AutoCatBoostRegression()</code> is an automated modeling function that runs a variety of steps. First, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation plot, evaluation boxplot, evaluation metrics, variable importance, partial dependence calibration plots, partial dependence calibration box plots, and column names used in model fitting. 

##### **AutoXGBoostRegression()** GPU + CPU
<code>AutoXGBoostRegression()</code> is an automated XGBoost modeling framework with grid-tuning and model evaluation that runs a variety of steps. First, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation plot, evaluation boxplot, evaluation metrics, variable importance, partial dependence calibration plots, partial dependence calibration box plots, and column names used in model fitting.

##### **AutoH2oGBMRegression()**
<code>AutoH2oGBMRegression()</code> is an automated H2O modeling framework with grid-tuning and model evaluation that runs a variety of steps. First, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation plot, evaluation boxplot, evaluation metrics, variable importance, partial dependence calibration plots, partial dependence calibration box plots, and column names used in model fitting.

##### **AutoH2oDRFRegression()**
<code>AutoH2oDRFRegression()</code> is an automated H2O modeling framework with grid-tuning and model evaluation that runs a variety of steps. First, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation plot, evaluation boxplot, evaluation metrics, variable importance, partial dependence calibration plots, partial dependence calibration box plots, and column names used in model fitting.
</p>
</details>

#### Binary Classification:
____________________________________________________________________________________________________________________________________________
<details><summary>expand</summary>
<p>

##### **AutoCatBoostClassifier()** GPU + CPU
<code>AutoCatBoostClassifier()</code> is an automated modeling function that runs a variety of steps. First, a stratified sampling (by the target variable) is done to create train and validation sets. Then, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, ROC plot, evaluation plot, evaluation metrics, variable importance, partial dependence calibration plots, partial dependence calibration box plots, and column names used in model fitting.

##### **AutoXGBoostClassifier()** GPU + CPU
<code>AutoXGBoostClassifier()</code> is an automated XGBoost modeling framework with grid-tuning and model evaluation that runs a variety of steps. First, a stratified sampling (by the target variable) is done to create train and validation sets. Then, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation plot, evaluation boxplot, evaluation metrics, variable importance, partial dependence calibration plots, partial dependence calibration box plots, and column names used in model fitting.

##### **AutoH2oGBMClassifier()**
<code>AutoH2oGBMClassifier()</code> is an automated H2O modeling framework with grid-tuning and model evaluation that runs a variety of steps. First, a stratified sampling (by the target variable) is done to create train and validation sets. Then, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation plot, evaluation metrics, variable importance, partial dependence calibration plots, and column names used in model fitting.

##### **AutoH2oDRFClassifier()**
<code>AutoH2oDRFClassifier()</code> is an automated H2O modeling framework with grid-tuning and model evaluation that runs a variety of steps. First, a stratified sampling (by the target variable) is done to create train and validation sets. Then, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation plot, evaluation metrics, variable importance, partial dependence calibration plots, and column names used in model fitting.
</p>
</details>

#### Multinomial Classification:
____________________________________________________________________________________________________________________________________________
<details><summary>expand</summary>
<p>
  
##### **AutoCatBoostMultiClass()** GPU + CPU
<code>AutoCatBoostMultiClass()</code> is an automated modeling function that runs a variety of steps. First, a stratified sampling (by the target variable) is done to create train and validation sets. Then, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation metrics, variable importance, and column names used in model fitting.

##### **AutoXGBoostMultiClass()** GPU + CPU
<code>AutoXGBoostMultiClass()</code> is an automated XGBoost modeling framework with grid-tuning and model evaluation that runs a variety of steps. First, a stratified sampling (by the target variable) is done to create train and validation sets. Then, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation metrics, variable importance, and column names used in model fitting.

##### **AutoH2oGBMMultiClass()**
<code>AutoH2oGBMMultiClass()</code> is an automated H2O modeling framework with grid-tuning and model evaluation that runs a variety of steps. First, a stratified sampling (by the target variable) is done to create train and validation sets. Then, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation metrics, confusion matrix, and variable importance.

##### **AutoH2oDRFMultiClass()**
<code>AutoH2oDRFMultiClass()</code> is an automated H2O modeling framework with grid-tuning and model evaluation that runs a variety of steps. First, a stratified sampling (by the target variable) is done to create train and validation sets. Then, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation metrics, confusion matrix, and variable importance.
</p>
</details>

#### Generalized Hurdle Models:
____________________________________________________________________________________________________________________________________________
<details><summary>expand</summary>
<p>
  
##### **AutoCatBoostHurdleModel()**
<code>AutoCatBoostHurdleModel()</code> is a modeling framework for building the necessary models for making predictions for hurdle modeling use-cases. It's generalized so that you can define any number of buckets. There are four cases that are handled with this function: 

###### Single Entry Bucket
* Pr(X = 0) * 0 + Pr(X > 0) * E(X | X > 0)  
* Pr(X < x1) * E(X | X < x1) + Pr(X >= x1) * E(X | X > x1)

###### Multiple Entry Bucket
* Pr(X = 0) * 0 + Pr(X < x2)  * E(X | X < x2) + ... + Pr(X < xn) * E(X | X < xn) + Pr(X >= xn) * E(X | X > xn)
* Pr(X < x1) * E(X | X < x1) + Pr(X < x2)  * E(X | X < x2) + ... + Pr(X < xn) * E(X | X < xn) + Pr(X >= xn) * E(X | X > xn)

First step is to build either a binary classification model (in the case of a single bucket value, such as zero) or a multiclass model (for the case of multiple bucket values, such as zero and 10). The next step is to subset the data for the cases of: less than the first bucket, in between the first and second, second and third, ..., second to last and last, along with greater than last. For each data subset, a regression model is built for predicting values in the bucket ranges. The final compilation is to multiply the probabilities of being in each bucket times the values supplied by the regression values for each buckets.
</p>
</details>

#### General Purpose H2O Automated Modeling:
____________________________________________________________________________________________________________________________________________
<details><summary>expand</summary>
<p>
  
##### **AutoH2OModeler()**
<code>AutoH2OModeler()</code> automatically build any number of models along with generating partial dependence calibration plots, model evaluation calibration plots, grid tuning, and file storage for easy production implementation. Handles regression, quantile regression, time until event, and classification models (binary and multinomial) using numeric and factor variables without the need for monotonic transformations nor one-hot-encoding.
* Models include:
  * RandomForest (DRF)
  * GBM
  * Deeplearning
  * XGBoost (for Linux)
  * LightGBM (for Linux)
  * AutoML - medium debth grid tuning for Deeplearning, XGBoost (if available), DRF, GBM, GLM, and StackedEnsembles
</p>
</details>

#### Nonlinear Regression Modeling:
____________________________________________________________________________________________________________________________________________
<details><summary>expand</summary>
<p>
  
##### **AutoNLS()**
<code>AutoNLS()</code> is an automated nonlinear regression modeling. This function automatically finds the best model fit from the suite of models below and merges predictions to source data file. Great for forecasting growth over time or estimating single variable nonlinear functions.
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
  
</p>
</details>

</p>
</details>

## Model Scoring Functions: 
<details><summary>EXPAND</summary>
<p>

##### **AutoCatBoostScoring()**
<code>AutoCatBoostScoring()</code> is an automated scoring function that compliments the AutoCatBoost() model training functions. This function requires you to supply features for scoring. It will run ModelDataPrep() to prepare your features for catboost data conversion and scoring. It will also handle and transformations and back-transformations if you utilized that feature in the regression training case.

##### **AutoXGBoostScoring()**
<code>AutoXGBoostScoring()</code> is an automated scoring function that compliments the AutoXGBoost() model training functions. This function requires you to supply features for scoring. It will run ModelDataPrep() and the DummifyDT() functions to prepare your features for xgboost data conversion and scoring. It will also handle and transformations and back-transformations if you utilized that feature in the regression training case.

##### **AutoH2OMLScoring()**
<code>AutoH2OMLScoring()</code> is an automated scoring function that compliments the AutoH2oGBM__() and AutoH2oDRF__() models training functions. This function requires you to supply features for scoring. It will run ModelDataPrep()to prepare your features for H2O data conversion and scoring. It will also handle and transformations and back-transformations if you utilized that feature in the regression training case.

##### **AutoH2OScoring()**
<code>AutoH2OScoring()</code> is for scoring models that were built with the AutoH2OModeler, AutoKMeans, and AutoWord2VecModeler functions. Scores models either via mojo or the standard method by loading models into the H2O environment and scoring them. You can choose which output you wish to keep as well. 
  
</p>
</details>

## Time Series Modeling Functions: 
<details><summary>EXPAND</summary>
<p>

##### **AutoTS()** <img src="https://github.com/AdrianAntico/RemixAutoML/blob/master/AutoTS.png" align="right" width="300" />
<code>AutoTS()</code> is an automated time series modeling function. The function automatically finds the most accurate time series model from the list of models below by utilizing optimal BoxCox transformations along with a stepwise procedue to test out possible values for lags and moving averages (user specifies upper bounds for lags and moving averages). All model parameters are optimally set to get the best possible performance out of each distinct model. There are also four different versions for each model that can be tested and internally compared by setting <code>ModelFreq = TRUE</code> and setting <code>TSClean = TRUE</code>, resulting in four tested combinations: 
  * user-specified time frequency + no historical series smoothing and imputation
  * model-based identified time frequency + no historical smoothing and imputation
  * user-specified time frequency + historical series smoothing and imputation
  * model-based identified time frequency + historical smoothing and imputation

The best model is chosen by looking at the lowest out-of-sample error (user sets the number of periods for testing along with the evaluation metric for evaluation), the winning model is rebuilt on all available data which is then used to generate the forecasts. The output from <code>AutoTS()</code> includes the forecast values, model evaluation metrics and metadata for all models tested, along with the model object.

* Automated Time Series Models include:
  * DSHW: Double Seasonal Holt-Winters
  * ARFIMA: Auto Regressive Fractional Integrated Moving Average
  * ARIMA: Auto Regressive Integrated Moving Average with specified max lags, seasonal lags, moving averages, and seasonal moving averages
  * ETS: Additive and Multiplicative Exponential Smoothing and Holt-Winters
  * NNetar: Auto Regressive Neural Network models automatically compares models with 1 lag or 1 seasonal lag compared to models with up to N lags and N seasonal lags
  * TBATS: Exponential smoothing state space model with Box-Cox transformation, ARMA errors, Trend and Seasonal components
  * TSLM: Time Series Linear Model - builds a linear model with trend and season components extracted from the data
 
##### **AutoCatBoostCARMA()**
<code>AutoCatBoostCARMA()</code> is an Automated Machine Learning Time Series Forecasting Function. Create hundreds of thousands of time series forecasts using this function. Internally, it utilizes the catboost algorithm and replicates the ARMA process of forecasting. What this means is that a one-step ahead forecast is made, then the model features are re-computed and the next one-step ahead forecast is made, etc. This process is done for every time period you wish to have forecasted. On top of that, you can include calendar variables, a time trend variable, and automatically have an optimal transformation made on your target variable, with competing transformations being: YeoJohnson, BoxCox, arcsinh, along with arcsin and logit for proportion data. Grid tuning is available along with several other arguments to customize your builds. You can also utilize GPU if you have one. Running with GPU, for example, allows me to forecast the entire Walmart store and department forecasts (2660 store & department combinations) in less than 15 minutes with a 30k-tree model (compared to 33 hours of run time to loop through all stores and departments using AutoTS). Note, the test was based on using a 1080ti.

##### **AutoXGBoostCARMA()**
<code>AutoXGBoostCARMA()</code> operates identically to the AutoCatBoostCARMA() function except that is utilizes XGBoost instead of CatBoost.

##### **AutoH2oDRFCARMA()**
<code>AutoH2oDRFCARMA()</code> operates identically to the AutoCatBoostCARMA() function except that is utilizes H2O Distributed Random Forest instead of CatBoost

##### **AutoH2oGBMCARMA()**
<code>AutoH2oGBMCARMA()</code> operates identically to the AutoCatBoostCARMA() function except that is utilizes H2O GBM instead of CatBoost
  
</p>
</details>

## Recommender System Functions: 
<details><summary>EXPAND</summary>
<p>
  
##### **AutoRecomDataCreate()**
<code>AutoRecomDataCreate()</code> automatically creates your binary ratings matix from transaction data

##### **AutoRecommender()**
<code>AutoRecommender()</code> automated collaborative filtering modeling where each model competes against each other
  * RandomItems
  * PopularItems
  * UserBasedCF  
  * ItemBasedCF
  * AssociationRules
  
##### **AutoRecommenderScoring()**
<code>AutoRecommenderScoring()</code> automatically score a recommender model from AutoRecommender

##### **AutoMarketBasketModel()**
<code>AutoMarketBasketModel()</code> is a function that runs a market basket analysis automatically. It will convert your data, run the algorithm, and add on additional significance values not provided by the source pacakge. 
  
</p>
</details>

## Unsupervised Learning Functions: 
<details><summary>EXPAND</summary>
<p>

##### **GenTSAnomVars()**
<code>GenTSAnomVars()</code> generates time series anomaly variables. (Cross with Feature Engineering) Create indicator variables (high, low) along with cumulative anomaly rates (high, low) based on control limits methodology over a max of two grouping variables and a date variable (effectively a rolling GLM).

##### **ResidualOutliers()**
<code>ResidualOutliers()</code> Generate residual outliers from time series modeling. (Cross with Feature Engineering) Utilize tsoutliers to indicate outliers within a time series data set

##### **AutoKMeans()** 
<code>AutoKMeans()</code> This function builds a generalized low rank model followed by KMeans. (Possible cross with Feature Engineering) Generate a column with a cluster identifier based on a grid tuned (optional) generalized low rank model and a grid tuned (optimal) K-Optimal searching K-Means algorithm
</p>
</details>

## Feature Engineering Functions: 
<details><summary>EXPAND</summary>
<p>

##### **DT_GDL_Feature_Engineering()**
<code>DT_GDL_Feature_Engineering()</code> builds autoregressive and moving average features from target columns and distributed lags and distributed moving average from independent features distributed across time. On top of that, you can also create time between instances along with their associated lags and moving averages. This function works for data with groups and without groups. 100% data.table built. It runs super fast and can handle big data.

##### **Partial_DT_GDL_Feature_Engineering()**
<code>Partial_DT_GDL_Feature_Engineering()</code> is for generating the equivalent features built from DT_GDL_Feature_Engineering() for a set of new records as rapidly as possible. I used this to create the feature vectors for scoring models in production. This function is for generating lags and moving averages (along with lags and moving averages off of time between records), for a partial set of records in your data set, typical new records that become available for model scoring. Column names and ordering will be identical to the output from the corresponding DT_GDL_Feature_Engineering() function, which most likely was used to create features for model training.

##### **Partial_DT_GDL_Feature_Engineering2()**
<code>Partial_DT_GDL_Feature_Engineering2()</code> is another way to compute the same features for a partial set of records as the Partial_DT_GDL_Feature_Engineering() function. This version can run quicker for data sets where moving average features have long windows and the lag list is short. You can benchmark both the original and this version to see which one runs faster for your data.

##### **Scoring_GDL_Feature_Engineering()**
<code>Scoring_GDL_Feature_Engineering()</code> is a function that runs internally inside the CARMA functions but might have use outside of it. It is for scoring a single record, for no grouping variables, or one record per group level when a single group is utilized. Generates identical column names as the DT_GDL_Feature_Engineering() function and the Partial_GDL_Feature_Engineering() function. 

##### **AutoWord2VecModeler()**
<code>AutoWord2VecModeler()</code> generates a specified number of vectors for each column of text data in your data set and save the models for re-creating them later in the scoring process. You can choose to build individual models for each columns or one model for all your columns.

##### **ModelDataPrep()**
<code>ModelDataPrep()</code> rapidly convert "inf" values to NA, convert character columns to factor columns, and impute with specified values for factor and numeric columns.

##### **DummifyDT()** 
<code>DummifyDT()</code> rapidly dichotomizes a list of columns in a data table (N+1 columns for N levels using one hot encoding or N columns for N levels otherwise). Several other arguments exist for outputting and saving factor levels for model scoring processes, which are used internally in the AutoXGBoost__() suite of modeling functions.

##### **AutoDataPartition()**
<code>AutoDataPartition()</code> is designed to achieve a few things that standard data partitioning processes or functions don't handle. First, you can choose to build any number of partitioned data sets beyond the standard train, validate, and test data sets. Second, you can choose between random sampling to split your data or you can choose a time-based partitioning. Third, for the random partitioning, you can specify stratification columns in your data to stratify by in order to ensure a proper split amongst your categorical features (E.g. think MultiClass targets). Lastly, it's 100% data.table so it will run fast and with low memory overhead.

##### **AutoTransformationCreate()**
<code>AutoTransformationCreate()</code> is a function for automatically identifying the optimal transformations for numeric features and transforming them once identified. This function will loop through your selected transformation options (YeoJohnson, BoxCox, Asinh, Asin, and Logit) and find the one that produces data that is the closest to normally distributed data. It then makes the transformation and collects the metadata information for use in the AutoTransformationScore() function, either by returning the objects (always) or saving them to file (optional).

##### **AutoTransformationScore()**
<code>AutoTransformationScore()</code> is a the compliment function to AutoTransformationCreate(). Automatically apply or inverse the transformations you identified in AutoTransformationCreate() to other data sets. This is useful for applying transformations to your validation and test data sets for modeling. It's also useful for back-transforming your target and prediction columns after you have build and score your models so you can obtain statistics on the original features.

##### **GDL_Feature_Engineering()**
<code>GDL_Feature_Engineering()</code> builds autoregressive and rolling stats from target columns and distributed lags and distributed rolling stats for independent features distributed across time. On top of that, you can also create time between instances along with their associated lags and rolling stats. This function works for data with groups and without groups. The rolling stats can be of any variety, such as rolling standard deviations, rolling quantiles, etc. but the function runs much slower than the DT_GDL_Feature_Engineering() counterpart so it might not be a good choice for scoring environments that require low latency.
</p>
</details>


## Model Evaluation, Interpretation, and Cost-Sensitive Functions: 
<details><summary>EXPAND</summary>
<p>

##### **ParDepCalPlots()**
<code>ParDepCalPlots()</code> is for visualizing the relationships of features and the reliability of the model in predicting those effects. Build a partial dependence calibration line plot, box plot or bar plot for the case of categorical variables.

##### **EvalPlot()**
<code>EvalPlot()</code> Has two plot versions: calibration line plot of predicted values and actual values across range of predicted value, and calibration boxplot for seeing the accuracy and variability of predictions against actuals. 

##### **threshOptim()**
<code>threshOptim()</code> is great for situations with asymmetric costs across the confusion matrix. Generate a cost-sensitive optimized threshold for classification models. Just supply the costs for false positives and false negatives (can supply costs for all four outcomes too) and the function will return the optimal threshold for maximizing "utility". 

##### **RedYellowGreen()**
<code>RedYellowGreen()</code> computes optimal thresholds for binary classification models where "don't classify" is an option. Consider a health care binary classification model that predicts whether or not a disease is present. This is certainly a case for threshOptim since the costs of false positives and false negatives can vary by a large margin. However, there is always the potential to run further analysis. The RedYellowGreen() function can compute two thresholds if you can supply a cost of "further analysis". Predicted values < the lower threshold are confidently classified as a negative case and predicted values > the upper threshold are confidently classified as a postive case. Predicted values in between the lower and upper thresholds are cases that should require further analysis.
</p>
</details>


## Utilities and Misc. Functions:
<details><summary>EXPAND</summary>
<p>
 
 ##### **AutoWordFreq()** 
<code>AutoWordFreq()</code> creates a word frequency data.table and a word cloud

##### **AutoH2OTextPrepScoring()** 
<code>AutoH2OTextPrepScoring()</code> prepares your data for scoring based on models built with AutoWord2VecModel and runs internally inside the AutoH2OScoring() function. It cleans and tokenizes your text data.

##### **ProblematicFeatures()**
<code>ProblematicFeatures()</code> identifies columns that have either little to no variance, categorical variables with extremely high cardinality, too many NA's, too many zeros, or too high of a skew.

##### **ProblematicRecords()**
<code>ProblematicRecords()</code> automatically identifies anomalous data records via Isolation Forests from H2O.

##### **RemixTheme()** 
<code>RemixTheme()</code> is a specific font, set of colors, and style for plots.

##### **ChartTheme()** 
<code>ChartTheme()</code> is a specific font, set of colors, and style for plots.

##### **multiplot()** 
<code>multiplot()</code> is useful for displaying multiple plots in a single pane. I've never had luck using grid so I just use this instead.

##### **tokenizeH2O()** 
<code>tokenizeH2O()</code> tokenizes an H2O string column.

##### **percRank()** 
<code>percRank()</code> is an inner function for calibration plots and partial dependence plots. It computes PercentRank for all numeric records in a column.

##### **SimpleCap()** 
<code>SimpleCap()</code> apply proper case to text.

##### **PrintObjectsSize()** 
<code>PrintObjectsSize()</code> prints out environment objects and their respective sizes. Useful for debugging programs.

##### **tempDatesFun()** 
<code>tempDatesFun()</code> is a special case for character conversion to date when importing from Excel.
</p>
</details>

