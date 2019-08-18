![RemixAutoML_Logo](https://user-images.githubusercontent.com/42076988/55656390-94dc4b00-57ab-11e9-9e3f-06b049b796d5.png)

# How to Install the Package for R:

#### 1. First, run the following R script to download dependencies (YOU MAY HAVE TO INSTALL A FEW OF THESE LINE BY LINE)
```
library(devtools)
to_install <- c("arules", "catboost", "caTools", "data.table", "doParallel", 
                "foreach", "forecast", "ggplot2", "h2o", "itertools", 
                "lubridate", "magick", "Matrix", "monreg", "nortest","pROC", "RColorBrewer", "recommenderlab", 
                "ROCR", "scatterplot3d", "stringr", "sde", "timeDate", "tm", "tsoutliers", "wordcloud", "xgboost", "zoo")
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
    install.packages("h2o")
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
> This is a collection of functions that I have made to speed up machine learning and to ensure high quality modeling results and output are generated. They are great at establishing solid baselines that are extremely challenging to beat using alternative methods (if at all). To see them in action, check out the free tutorials at <a href="http://www.remyxcourses.com/course?courseid=intro-to-remixautoml-in-r" target="_blank">RemyxCourses.com</a> or the blogs listed below.

Also, be sure to visit our blog at <a href="http://www.remixinstitute.com" target="_blank">RemixInstitute.ai</a> for data science, machine learning, and AI content.

You can contact me via <a href="https://www.linkedin.com/in/adrian-antico/" target="_blank">LinkedIn</a> for any questions about the package. You can also go into the vignettes folder to see the package reference manual and a vignette with some background and examples. If you want to be a contributer, contact me via LinkedIn email.

Hex sticker rendered via the <code>hexSticker</code> package in R: https://github.com/GuangchuangYu/hexSticker

## RemixAutoML Blogs: 
[Why Machine Learning is more Practical than Econometrics in the Real World](https://medium.com/@adrianantico/machine-learning-vs-econometrics-in-the-real-world-4058095b1013)

[Build Thousands of Automated Demand Forecasts in 15 Minutes Using AutoCatBoostCARMA in R](https://www.remixinstitute.com/blog/automated-demand-forecasts-using-autocatboostcarma-in-r/#.XUIO1ntlCDM)

[Automate Your KPI Forecasts With Only 1 Line of R Code Using AutoTS](https://www.remixinstitute.com/blog/automate-your-kpi-forecasts-with-only-1-line-of-r-code-using-autots/#.XUIOr3tlCDM)

[Companies Are Demanding Model Interpretability. Here’s How To Do It Right](https://www.remixinstitute.com/blog/companies-are-demanding-model-interpretability-heres-how-to-do-it-right/#.XUIN1HtlCDM)

[The Easiest Way to Create Thresholds And Improve Your Classification Model](https://www.remixinstitute.com/blog/the-easiest-way-to-create-thresholds-and-improve-your-classification-model/#.XUINVntlCDM)

## Supervised Learning Training Functions: 
<details><summary>EXPAND</summary>
<p>
  
#### Regression:
____________________________________________________________________________________________________________________________________________
<details><summary>expand</summary>
<p>
  
##### **AutoCatBoostRegression()** GPU + CPU
<code>AutoCatBoostRegression()</code> utilizes the CatBoost algorithm in the below steps

##### **AutoXGBoostRegression()** GPU + CPU
<code>AutoXGBoostRegression()</code> utilizes the XGBoost algorithm in the below steps 

##### **AutoH2oGBMRegression()**
<code>AutoH2oGBMRegression()</code> utilizes the H2O Gradient Boosting algorithm in the below steps 

##### **AutoH2oDRFRegression()**
<code>AutoH2oDRFRegression()</code> utilizes the H2o Distributed Random Forest algorithm in the below steps 
  
#### The Auto_Regression() models handle a multitude of tasks. In order:
1. Convert your data to data.table format for faster processing
2. Transform your target variable using the best normalization method based on the <code>AutoTransformationCreate()</code> function
3. Create train, validation, and test data if you didn't supply those directly to the function
4. Consoldate columns that are used for modeling and what is to be kept for data returned
5. Dichotomize categorical variables (for <code>AutoXGBoostRegression()</code>) and save the factor levels for scoring in a way that guarentees consistency across training, validation, and test data sets
6. Save the final modeling column names for later reference
7. Handles the data conversion to the appropriate type, based on model type (CatBoost, H2O, and XGBoost)
8. Build out a random hyperparameter set for a random grid search for model tuning (includes the default model hyperparameters) if you choose to grid tune
9. Loop through the grid-tuning process
10. Collect the evaluation metrics for each grid tune run
11. Identify the best model of the set of models built in the grid tuning search
12. Save the hyperparameters from the winning grid tuned model
13. Build the final model based on the best model from the grid tuning model search
14. Back-transform your predictions based on the best transformation used earlier in the process
15. Collect evaluation metrics based on performance on test data (based on back-transformed data)
16. Store the final predictions with the associated test data and other columns you want included in that set
17. Save your transformation metadata for recreating them in a scoring process
18. Build out and save an Evaluation Calibration Line Plot and Box-Plot
19. Generate and save Variable Importance
20. Generate and save Partital Dependence Calibration Line Plots and Box-Plots
21. Return all the objects generated in a named list for immediate use
 
</p>
</details>

#### Binary Classification:
____________________________________________________________________________________________________________________________________________
<details><summary>expand</summary>
<p>
  
##### **AutoCatBoostClassifier()** GPU + CPU
<code>AutoCatBoostClassifier()</code> utilizes the CatBoost algorithm in the below steps

##### **AutoXGBoostClassifier()** GPU + CPU
<code>AutoXGBoostClassifier()</code> utilizes the XGBoost algorithm in the below steps

##### **AutoH2oGBMClassifier()**
<code>AutoH2oGBMClassifier()</code> utilizes the H2O Gradient Boosting algorithm in the below steps

##### **AutoH2oDRFClassifier()**
<code>AutoH2oDRFClassifier()</code> utilizes the H2O Distributed Random Forest algorithm in the below steps
  
#### The Auto_Classifier() models handle a multitude of tasks. In order:
1. Convert your data to data.table format for faster processing
2. Create train, validation, and test data if you didn't supply those directly to the function
3. Consoldate columns that are used for modeling and what is to be kept for data returned
4. Dichotomize categorical variables (for AutoXGBoostRegression) and save the factor levels for scoring in a way that guarentees consistency across training, validation, and test data sets
5. Saves the final column names for modeling to a csv for later reference
6. Handles the data conversion to the appropriate type, based on model type (CatBoost, H2O, and XGBoost)
7. Build out a random hyperparameter set for a random grid search for model tuning (includes the default model hyperparameters) if you want to utilize that feature
8. Build the grid tuned models
9. Collect the evaluation metrics for each grid tune run
10. Identify the best model of the set of models built in the grid tuning setup
11. Save the hyperparameters from the winning grid tuned model
12. Build the final model based on the best model from the grid tuning model search
13. Collect evaluation metrics based on performance on test data
14. Store the final predictions with the associated test data and other columns you want included in that set
15. Build out and save an Evaluation Calibration Line Plot
16. Build out and save an ROC plot with the top 5 models used in grid-tuning (includes the winning model)
17. Generate and save Variable Importance data
18. Generate and save Partital Dependence Calibration Line Plots
19. Return all the objects generated in a named list for immediate use

</p>
</details>

#### Multinomial Classification:
____________________________________________________________________________________________________________________________________________
<details><summary>expand</summary>
<p>
  
##### **AutoCatBoostMultiClass()** GPU + CPU
<code>AutoCatBoostMultiClass()</code> utilizes the CatBoost algorithm in the below steps

##### **AutoXGBoostMultiClass()** GPU + CPU
<code>AutoXGBoostMultiClass()</code> utilizes the XGBoost algorithm in the below steps

##### **AutoH2oGBMMultiClass()**
<code>AutoH2oGBMMultiClass()</code> utilizes the H2O Gradient Boosting algorithm in the below steps

##### **AutoH2oDRFMultiClass()**
<code>AutoH2oDRFMultiClass()</code> utilizes the H2O Distributed Random Forest algorithm in the below steps
  
#### The Auto_MultiClass() models handle a multitude of tasks. In order:
1. Convert your data to data.table format for faster processing
2. Create train, validation, and test data if you didn't supply those directly to the function
3. Consoldate columns that are used for modeling and what is to be kept for data returned
4. Dichotomize categorical variables (for AutoXGBoostRegression) and save the factor levels for scoring in a way that guarentees consistency across training, validation, and test data sets
5. Saves the final column names for modeling to a csv for later reference
6. Ensures the target levels are consistent across train, validate, and test sets and save the levels to file
7. Handles the data conversion to the appropriate type, based on model type (CatBoost, H2O, and XGBoost)
8. Build out a random hyperparameter set for a random grid search for model tuning (includes the default model hyperparameters) if you want to utilize that feature
9. Build the grid tuned models
10. Collect the evaluation metrics for each grid tune run
11. Identify the best model of the set of models built in the grid tuning setup
12. Save the hyperparameters from the winning grid tuned model
13. Build the final model based on the best model from the grid tuning model search
14. Collect evaluation metrics based on performance on test data
15. Store the final predictions with the associated test data and other columns you want included in that set
16. Generate and save Variable Importance data
17. Return all the objects generated in a named list for immediate use

</p>
</details>

#### Generalized Hurdle Models:
____________________________________________________________________________________________________________________________________________
<details><summary>expand</summary>
<p>
  
First step is to build either a binary classification model (in the case of a single bucket value, such as zero) or a multiclass model (for the case of multiple bucket values, such as zero and 10). The next step is to subset the data for the cases of: less than the first split value, in between the first and second split value, second and third split value, ..., second to last and last split value, along with greater than last split value. For each data subset, a regression model is built for predicting values in the split value ranges. The final compilation is to multiply the probabilities of being in each group times the values supplied by the regression values for each group.

###### Single Partition
* E(y|x<sub>i</sub>) = Pr(X = 0) * 0 + Pr(X > 0) * E(X | X >= 0)  
* E(y|x<sub>i</sub>) = Pr(X < x<sub>1</sub>) * E(X | X < x<sub>1</sub>) + Pr(X >= x<sub>1</sub>) * E(X | X >= x<sub>1</sub>)

###### Multiple Partitions
* E(y|x<sub>i</sub>) = Pr(X = 0) * 0 + Pr(X < x<sub>2</sub>) * E(X | X < x<sub>2</sub>) + ... + Pr(X < x<sub>n</sub>) * E(X | X < x<sub>n</sub>) + Pr(X >= x<sub>n</sub>) * E(X | X >= x<sub>n</sub>)
* E(y|x<sub>i</sub>) = Pr(X < x<sub>1</sub>) * E(X | X < x<sub>1</sub>) + Pr(x<sub>1</sub> <= X < x<sub>2</sub>) * E(X | x<sub>1</sub> <= X < x<sub>2</sub>) + ... + Pr(x<sub>n-1</sub> <= X < x<sub>n</sub>) * E(X | x<sub>n-1</sub> <= X < x<sub>n</sub>) + Pr(X >= x<sub>n</sub>) * E(X | X >= x<sub>n</sub>)
  
##### **AutoCatBoostHurdleModel()**
<code>AutoCatBoostHurdleModel()</code> utilizes the CatBoost algorithm on the backend. 

##### **AutoXGBoostHurdleModel()**
<code>AutoXGBoostHurdleModel()</code> utilizes the XGBoost algorithm on the backend. 

##### **AutoH2oDRFHurdleModel()**
<code>AutoH2oDRFHurdleModel()</code> utilizes the H2O distributed random forest algorithm on the backend. 

##### **AutoH2oGBMHurdleModel()**
<code>AutoH2oGBMHurdleModel()</code> utilizes the H2O gradient boosting machine algorithm on the backend. 


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
<code>AutoH2OMLScoring()</code> is an automated scoring function that compliments the AutoH2oGBM__() and AutoH2oDRF__() model training functions. This function requires you to supply features for scoring. It will run ModelDataPrep()to prepare your features for H2O data conversion and scoring. It will also handle transformations and back-transformations if you utilized that feature in the regression training case and didn't do it yourself before hand.

##### **AutoH2OScoring()**
<code>AutoH2OScoring()</code> is for scoring models that were built with the AutoH2OModeler, AutoKMeans, and AutoWord2VecModeler functions. Scores mojo models or binary files by loading models into the H2O environment and scoring them. You can choose which output you wish to keep as well for classification and multinomial models. 
  
</p>
</details>

## Time Series Modeling Functions: 
<details><summary>EXPAND</summary>
<p>

### **AutoTS()** <img src="https://github.com/AdrianAntico/RemixAutoML/blob/master/AutoTS.png" align="right" width="300" />
<code>AutoTS()</code> 

* Returns a list containing 
  * A data.table object with a date column and the forecasted values
  * The model evaluation results
  * The champion model for later use if desired
  * The name of the champion model
  * A time series ggplot with historical values and forecasted values with optional 80% and 95% prediction intervals

* The models tested internally include:
  * DSHW: Double Seasonal Holt-Winters
  * ARFIMA: Auto Regressive Fractional Integrated Moving Average
  * ARIMA: Auto Regressive Integrated Moving Average with specified max lags, seasonal lags, moving averages, and seasonal moving averages
  * ETS: Additive and Multiplicative Exponential Smoothing and Holt-Winters
  * NNetar: Auto Regressive Neural Network models automatically compares models with 1 lag or 1 seasonal lag compared to models with up to N lags and N seasonal lags
  * TBATS: Exponential smoothing state space model with Box-Cox transformation, ARMA errors, Trend and Seasonal components
  * TSLM: Time Series Linear Model - builds a linear model with trend and season components extracted from the data

For each of the models tested internally, several aspects should be noted:
* Optimal Box-Cox transformations are used in every run where data is strictly positive. The optimal transformation could also be "no transformation". 
* Four different treatments are tested for each model:
  * user-specified time frequency + no historical series smoothing & imputation
  * model-based time frequency + no historical smoothing and imputation
  * user-specified time frequency + historical series smoothing & imputation
  * model-based time frequency + historical smoothing & imputation

* For ARIMA only (for releases >= 0.9.1) you can specify MaxFourierPairs to test out if adding Fourier term regressors can increase forecast accuracy.
* For the ARIMA, ARFIMA, and TBATS, any number of lags and moving averages along with up to 1 seasonal lags and seasonal moving averages can be used (selection based on a stepwise procedure)
* For the Double Seasonal Holt-Winters model, alpha, beta, gamma, omega, and phi are determined using least-squares and the forecasts are adjusted using an AR(1) model for the errors
* The Exponential Smoothing State-Space model runs through an automatic selection of the error type, trend type, and season type, with the options being "none", "additive", and "multiplicative", along with testing of damped vs. non-damped trend (either additive or multiplicative), and alpha, beta, and phi are estimated
* The neural network is setup to test out every combination of lags and seasonal lags and the model with the best holdout score is selected
* The TBATS model utilizes any number of lags and moving averages for the errors, damped trend vs. non-damped trend are tested, trend vs. non-trend are also tested, and the model utilizes parallel processing for efficient run times
* The TSLM model utilizes a simple time trend and season depending on the frequency of the data

### The **CARMA** Suite <img src="https://github.com/AdrianAntico/RemixAutoML/blob/master/CARMA_Logo.png" align="right" width="300" />
<code>AutoTS()</code>

##### **AutoCatBoostCARMA()**
<code>AutoCatBoostCARMA()</code> utilizes the CatBoost alorithm

##### **AutoXGBoostCARMA()**
<code>AutoXGBoostCARMA()</code> utilizes the XGBoost alorithm

##### **AutoH2oDRFCARMA()**
<code>AutoH2oDRFCARMA()</code> utilizes the H2O Distributed Random Forest alorithm

##### **AutoH2oGBMCARMA()**
<code>AutoH2oGBMCARMA()</code> utilizes the H2O Gradient Boosting Machine alorithm

##### The CARMA suite utilizes several features to ensure proper models are built to generate the best possible out-of-sample forecasts.

**Feature engineering:** I use a time trend, calendar variables, holiday counts, lags and moving averages. Internally, the CARMA functions utilize several RemixAutoML functions, all written using data.table for fast and memory efficient processing: 
  * <code>DT_GDL_Feature_Engineering()</code> - creates lags and moving average features (also creates lags and moving averages off of time between records)
  * <code>Scoring_GDL_Feature_Engineering()</code> - creates lags and moving average features for a single record (along with the time between vars)
  * <code>CreateCalendarVariables()</code> - creates numeric features identifying various time units based on date columns
  * <code>CreateHolidayVariables()</code> - creates count features based on the specified holiday groups you want to track and the date columns you supply

**Optimal transformations:** the target variable along with the associated lags and moving average features were transformed. This is really useful for regression models with categorical features that have associated target values that significantly differ from each other. The transformation options that are tested (using a Pearson test for normality) include: 
  * YeoJohnson
  * BoxCox
  * arcsinh
  * Identity
  * arcsin(sqrt(x)): proportion data only
  * logit(x): proportion data only

##### The functions used to create these and generate them for scoring models come from RemixAutoML:
* <code>AutoTransformationCreate()</code>
* <code>AutoTransformationScore()</code>

**Models:** there are four CARMA functions and each use a different algorithm for the model fitting. The models used to fit the time series data come from RemixAutoML and include: 
* <code>AutoCatBoostRegression()</code>
* <code>AutoXGBoostRegression()</code>
* <code>AutoH2oDRFRegression()</code>
* <code>AutoH2oGBMRegression()</code>

**GPU:** With the CatBoost and XGBoost functions, you can build the models utilizing GPU (I run them with a GeForce 1080ti) which results in an average 10x speedup in model training time (compared to running on CPU with 8 threads).

**Data partitioning:** for creating the training, validation, and test data, the CARMA functions utilize the <code>AutoDataPartition()</code> function and utilizes the "timeseries" option for the PartitionType argument which ensures that the train data reflects the furthest points back in time, followed by the validation data, and then the test data which is the most recent in time.

**Forecasting:** Once the regression model is built, the forecast process replicates the ARIMA process. Once a single step-ahead forecast is made, the lags and moving average features are updated based on the predicted values from scoring the model. Next, the rest of the other features are updated. Then the next forecast step is made, rinse and repeat for remaining forecasting steps. This process utilizes the RemixAutoML functions:
* <code>AutoCatBoostScoring()</code>
* <code>AutoXGBoostScoring()</code>
* <code>AutoH2oMLScoring()</code>
  
</p>
</details>

## Recommender System Functions: 
<details><summary>EXPAND</summary>
<p>
  
##### **AutoRecomDataCreate()**
<code>AutoRecomDataCreate()</code> automatically creates your binary ratings matix from transaction data

##### **AutoRecommender()**
<code>AutoRecommender()</code> automated collaborative filtering modeling where each model below competes against one another for top performance
  * RandomItems
  * PopularItems
  * UserBasedCF  
  * ItemBasedCF
  * AssociationRules
  
##### **AutoRecommenderScoring()**
<code>AutoRecommenderScoring()</code> automatically score a recommender model from AutoRecommender()

##### **AutoMarketBasketModel()**
<code>AutoMarketBasketModel()</code> is a function that runs a market basket analysis automatically. It will convert your data, run the algorithm, and generate the recommended items. On top of that, it includes additional significance values not provided by the source pacakge. 
  
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

##### **CreateCalendarVariables()**
<code>ModelDataPrep()</code> This functions creates new columns that extract the calendar information from date columns, such as second, minute, hour, week day, day of month, day of year, week, isoweek, month, quarter, and year.

##### **CreateHolidayVariable()**
<code>ModelDataPrep()</code> This function counts up the number of specified holidays between the current record time stamp and the previous record time stamp.

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

![ParDepCalPlots Blog](https://www.remixinstitute.com/blog/companies-are-demanding-model-interpretability-heres-how-to-do-it-right/#.XUIN1HtlCDM)

##### **EvalPlot()**
<code>EvalPlot()</code> Has two plot versions: calibration line plot of predicted values and actual values across range of predicted value, and calibration boxplot for seeing the accuracy and variability of predictions against actuals. 

##### **threshOptim()**
<code>threshOptim()</code> is great for situations with asymmetric costs across the confusion matrix. Generate a cost-sensitive optimized threshold for classification models. Just supply the costs for false positives and false negatives (can supply costs for all four outcomes too) and the function will return the optimal threshold for maximizing "utility". 

##### **RedYellowGreen()**
<code>RedYellowGreen()</code> computes optimal thresholds for binary classification models where "don't classify" is an option. Consider a health care binary classification model that predicts whether or not a disease is present. This is certainly a case for threshOptim since the costs of false positives and false negatives can vary by a large margin. However, there is always the potential to run further analysis. The RedYellowGreen() function can compute two thresholds if you can supply a cost of "further analysis". Predicted values < the lower threshold are confidently classified as a negative case and predicted values > the upper threshold are confidently classified as a postive case. Predicted values in between the lower and upper thresholds are cases that should require further analysis.

![RedYellowGreen Blog](https://www.remixinstitute.com/blog/the-easiest-way-to-create-thresholds-and-improve-your-classification-model/#.XUINVntlCDM)

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

