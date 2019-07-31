![RemixAutoML_Logo](https://user-images.githubusercontent.com/42076988/55656390-94dc4b00-57ab-11e9-9e3f-06b049b796d5.png)

# How to Install the Package for R:

#### 1. First, run the following R script to download dependencies (YOU MAY HAVE TO INSTALL A FEW OF THESE LINE BY LINE)
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
> This is a collection of functions that I have made to speed up machine learning and to ensure high quality modeling results and output are generated. They are great at establishing solid baselines that are extremely challenging to beat using alternative methods (if at all). To see them in action, check out the free tutorials at <a href="http://www.remyxcourses.com/course?courseid=intro-to-remixautoml-in-r" target="_blank">RemyxCourses.com</a>.

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
<code>AutoCatBoostRegression()</code> utilizes the CatBoost algorithm in the below steps

##### **AutoXGBoostRegression()** GPU + CPU
<code>AutoXGBoostRegression()</code> utilizes the XGBoost algorithm in the below steps 

##### **AutoH2oGBMRegression()**
<code>AutoH2oGBMRegression()</code> utilizes the H2O Gradient Boosting algorithm in the below steps 

##### **AutoH2oDRFRegression()**
<code>AutoH2oDRFRegression()</code> utilizes the H2o Distributed Random Forest algorithm in the below steps 
  
#### The Auto_Regression() models handle a multitude of tasks. In order:
1. Convert your data to data.table format for faster processing
2. Transform your target variable using the best normalization method based on the AutoTransformationCreate() function
3. Create train, validation, and test data if you didn't supply those directly to the function
4. Consoldate columns that are used for modeling and what is to be kept for data returned
5. Dichotomize categorical variables (for AutoXGBoostRegression) and save the factor levels for scoring in a way that guarentees consistency across training, validation, and test data sets
6. Saves the final column names for modeling to a csv for later reference
7. Handles the data conversion to the appropriate type, based on model type (CatBoost, H2O, and XGBoost)
8. Build out a random hyperparameter set for a random grid search for model tuning (includes the default model hyperparameters) if you want to utilize that feature
9. Build the grid tuned models
10. Collect the evaluation metrics for each grid tune run
11. Identify the best model of the set of models built in the grid tuning setup
12. Save the hyperparameters from the winning grid tuned model
13. Build the final model based on the best model from the grid tuning model search
14. Back-transform your predictions based on the best transformation used earlier in the process
15. Collect evaluation metrics based on performance on test data (based on back-transformed data)
16. Store the final predictions with the associated test data and other columns you want included in that set
17. Save your transformation metadata for recreating them in a scoring process
18. Build out and save an Evaluation Calibration Line Plot and Box-Plot
19. Generate and save Variable Importance data
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
* Pr(X = 0) * 0 + Pr(X > 0) * E(X | X >= 0)  
* Pr(X < x<sub>1</sub>) * E(X | X < x<sub>1</sub>) + Pr(X >= x<sub>1</sub>) * E(X | X >= x<sub>1</sub>)

###### Multiple Partitions
* Pr(X = 0) * 0 + Pr(X < x<sub>2</sub>) * E(X | X < x<sub>2</sub>) + ... + Pr(X < x<sub>n</sub>) * E(X | X < x<sub>n</sub>) + Pr(X >= x<sub>n</sub>) * E(X | X >= x<sub>n</sub>)
* Pr(X < x<sub>1</sub>) * E(X | X < x<sub>1</sub>) + Pr(x<sub>1</sub> <= X < x<sub>2</sub>) * E(X | x<sub>1</sub> <= X < x<sub>2</sub>) + ... + Pr(x<sub>n-1</sub> <= X < x<sub>n</sub>) * E(X | x<sub>n-1</sub> <= X < x<sub>n</sub>) + Pr(X >= x<sub>n</sub>) * E(X | X >= x<sub>n</sub>)
  
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
<code>AutoH2OMLScoring()</code> is an automated scoring function that compliments the AutoH2oGBM__() and AutoH2oDRF__() models training functions. This function requires you to supply features for scoring. It will run ModelDataPrep()to prepare your features for H2O data conversion and scoring. It will also handle and transformations and back-transformations if you utilized that feature in the regression training case.

##### **AutoH2OScoring()**
<code>AutoH2OScoring()</code> is for scoring models that were built with the AutoH2OModeler, AutoKMeans, and AutoWord2VecModeler functions. Scores mojo models or binary files by loading models into the H2O environment and scoring them. You can choose which output you wish to keep as well for classification and multinomial models. 
  
</p>
</details>

## Time Series Modeling Functions: 
<details><summary>EXPAND</summary>
<p>

##### **AutoTS()** <img src="https://github.com/AdrianAntico/RemixAutoML/blob/master/AutoTS.png" align="right" width="300" />
<code>AutoTS()</code> 

* Automated Time Series Models include:
  * DSHW: Double Seasonal Holt-Winters
  * ARFIMA: Auto Regressive Fractional Integrated Moving Average
  * ARIMA: Auto Regressive Integrated Moving Average with specified max lags, seasonal lags, moving averages, and seasonal moving averages
  * ETS: Additive and Multiplicative Exponential Smoothing and Holt-Winters
  * NNetar: Auto Regressive Neural Network models automatically compares models with 1 lag or 1 seasonal lag compared to models with up to N lags and N seasonal lags
  * TBATS: Exponential smoothing state space model with Box-Cox transformation, ARMA errors, Trend and Seasonal components
  * TSLM: Time Series Linear Model - builds a linear model with trend and season components extracted from the data

For each of the models tested internally, several aspects should be noted:
A) Optimal Box-Cox transformations were used in every run where data was strictly positive. The optimal transformation could also be "no transformation". 
B) Four different treatments are tested for each model:
user-specified time frequency + no historical series smoothing & imputation
model-based time frequency + no historical smoothing and imputation
user-specified time frequency + historical series smoothing & imputation
model-based time frequency + historical smoothing & imputation

C) For the ARIMA, ARFIMA, and TBATS, up to 25 lags and moving averages along with up to 1 seasonal lags and seasonal moving averages are used (determined via stepwise)
D) For the Double Seasonal Holt-Winters model, alpha, beta, gamma, omega, and phi are determined using least-squares and the forecasts are adjusted using an AR(1) model for the errors
E) The Exponential Smoothing State-Space model runs through an automatic selection of the error type, trend type, and season type, with the options being "none", "additive", and "multiplicative", along with testing of damped vs. non-damped trend (either additive or multiplicative), and alpha, beta, and phi are estimated
F) The neural network is setup to test out every combination of lags and seasonal lags up to 25 for each and the model with the best holdout score is selected
G) The TBATS model utilizes up to 25 lags and moving averages for the errors, damped trend vs. non-damped trend are tested, trend vs. non-trend are also tested, and the model utilizes parallel processing
H) The TSLM model utilizes a simple time trend and season depending on the frequency of the data

##### **AutoCatBoostCARMA()**
<code>AutoCatBoostCARMA()</code> is an automated machine learning time series forecasting function. The CARMA part of the name refers to Calendar and Auto-Regressive Moving-Average. Create hundreds of thousands of time series forecasts using this function. Internally, it utilizes the catboost algorithm and replicates an ARMA process. The features automatically created internally include calendar variables, lags, moving averages, and a time trend variable. The forecasts are generated by predicting one step ahead at a time and between forecasting steps the model features are updated before generating the next forecast. This process is done for every time step you wish to have forecasted. On top of that, you can automatically have an optimal transformation made on your target variable, with competing transformations being: YeoJohnson, BoxCox, arcsinh, along with arcsin(sqrt(x)) and logit for proportion data. Grid tuning is available along with several other arguments to customize your model builds. You can also utilize GPU if you have one. Running with GPU typically allows for a 10x speedup over CPU with the catboost algorithm. Note, this is based on utilizing a 1080ti.

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
<code>AutoRecommender()</code> automated collaborative filtering modeling where each model below competes against one another for top performance
  * RandomItems
  * PopularItems
  * UserBasedCF  
  * ItemBasedCF
  * AssociationRules
  
##### **AutoRecommenderScoring()**
<code>AutoRecommenderScoring()</code> automatically score a recommender model from AutoRecommender()

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

