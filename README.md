# RemixAutoML
> This is a collection of functions that I have made to speed up machine learning and to ensure high quality modeling output is generated. They are great at establishing solid baselines that are extremely challenging to beat using alternative methods. See them in action at https://www.remyxcourses.com alternatively, you can contact me via **LinkedIn** https://www.linkedin.com/in/adrian-antico/. If you want to be a contributer, contact me via email.
##### Supervised Learning Functions: 
1. **AutoH20Modeler**: Automated machine learning. Automatically build any number of models along with generating partial dependence calibration plots, model evaluation calibration plots, grid tuning, and file storage for easy production implementation. Handles regression, quantile regression, time until event, and classification models (binary and multinomial) using numeric and factor variables without the need for one-hot-encoding.
* Models include:
  * RandomForest (DRF)
  * GBM
  * Deeplearning
  * XGBoost (for Linux)
  * LightGBM (for Linux)
  * AutoML - medium debth grid tuning for Deeplearning, XGBoost (if available), DRF, GBM, GLM, and StackedEnsembles
2. **AutoH20Scoring** Scoring models that were built with the AutoH20Modeler function. Scores models either via mojo or the standard method by loading models into the H2O environment and scoring them. You can choose which output you wish to keep as well. 
3. **AutoTS**: Automated time series modeling function. Automatically finds the best model fit from the suite of models below (using optimized box-cox transformations), along with generating forecasts and evaluation metrics.
* Models include:
  * ARIFIMA: Auto Regressive Fractional Integrated Moving Average
  * ARIMIA: Stepwise Auto Regressive Integrated Moving Average with specified max lags, seasonal lags, moving averages, and seasonal moving averages
  * ETS: Additive and Multiplicitive Exponential Smoothing and Holt Winters
  * NNetar: Auto Regressive Neural Network models automatically compares models with 1 lag or 1 seasonal lag compared to models with up to N lags and N seasonal lags
  * TBATS: Exponential smoothing state space model with Box-Cox transformation, ARMA errors, Trend and Seasonal components
  * TSLM: Time Series Linear Model - builds a linear model with trend and season components extracted from the data
  * Prophet: Additive model where non-linear trends are fit with yearly, weekly, and daily seasonality, plus holiday effects
4. **AutoNLS**: Automated nonlinear regression modeling. Automatically finds the best model fit from the suite of models below and merges predictions to source data file. Great for forecasting growth over time or estimating single variable nonlinear functions.
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

##### Unsupervised Learning Functions: 
5. **GenTSAnomVars**: - Generate time series anomaly variables. (Cross with Feature Engineering) Create indicator variables (high, low) along with cumulative anomaly rates (high, low) based on control limits methodology over a max of two grouping variables and a date variable (effectively a rolling GLM).
6. **ResidualOutliers**: Residual outliers from time series modeling. (Cross with Feature Engineering) Utilize tsoutliers to indicate outliers within a time series data set
7. **AutoKMeans**: Generalized low rank model followed by KMeans. (Possible cross with Feature Engineering) Generate a column with a cluster identifier based on a grid tuned (optional) generalized low rank model and a grid tuned (optimal) K-Optimal searching K-Means algorithm

##### Feature Engineering Functions: 
8. **FAST_GDL_Feature_Engineering**: Fast generalized distributed lag feature engineering. Rapidly generate time between events, autoregressive, moving average / standard deviation / min / max / quantile 85 / quantile 95 for when you want to generate these features only for predicting events at the latest time interval of the data set. 100% data.table except for rolling statistics.
9. **GDL_Feature_Engineering**: Generate a wider set of features (similar in structure to FAST_GDL) using any aggregation statistic for the rolling stats. 100% data.table except for rolling statistics.
10. **Scoring_GDL_Feature_Engineering**: Generate the model features from FAST_GDL or GDL for scoring purposes when the scoring data is for forward looking predictions (not historical, which can be obtained from FAST_GDL or GDL). 100% data.table.
11. **DT_GDL_Feature_Engineering**: Lags + Moving Averages, 100% data.table
12. **AutoWord2VecModeler**: Generate a specified number of vectors for each column of text data in your data set and save the models for re-creating them later in the scoring process.
13. **ModelDataPrep**: Rapidly convert "inf" values to NA, convert character columns to factor columns, and impute with specified values for factor and numeric columns (factors are necessary (no characters values) for H20).
14. **DummifyDT** - rapidly dichotomize a list of columns in a data table (N+1 columns for N levels using one hot encoding or N columns for N levels otherwise)

##### Model Evaluation, Interpretation, and Cost-Sensitive Functions: 
15. **ParDepCalPlots**: Great for features effects estimation and reliability of model in predicting those effects. Build a partial dependence calibration plot on train, test, or all data
16. **EvalPlot**: Great for assessing accuracy across range of predicted values. Build a calibration plot on test data
17. **threshOptim**: Great for situations with asymmetric costs across the confusion matrix. Generate a cost-sensitive optimized threshold for classification models
18. **RedYellowGreen** - computes optimal thresholds for binary classification models when "don't classify" is an option

##### Utilities and Misc. Functions:
19. tokenizeH20 - Tokenize and H20 string column.
20. tempDatesFun - Special case for character conversion to date when importing from Excel.
21. RemixTheme - Fonts, colors, style for plots.
22. ChartTheme - Fonts, colors, style for plots.
23. SimpleCap - Apply proper case to text.
24. percRank - Inner function for calibration plots and partial dependence plots. Computes PercentRank.
25. multiplot - Useful for displaying multiple plots in a single pane.
26. PrintObjectsSize - print out objects and their sizes that are in the envrionment
27. AutoWordFreq - creates a word frequency data.table and a word cloud
28. AutoH20TextPrepScoring - prepares your data for scoring based on models built with Word2VecModel
