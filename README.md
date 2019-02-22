# R_Modeling_Tools
> This is a collection of functions that I have made to speed up machine learning. They are great at establishing solid baselines that are extremely challenging to beat using alternative methods. See them in action at https://www.remyxcourses.com alternatively, you can contact me via **LinkedIn** https://www.linkedin.com/in/adrian-antico/
##### Supervised Learning Functions: 
1. **AutoH20Modeler**: Great at structured data accuracy optimization. Automatically build any number of models along with generating partial dependence calibration plots, model evaluation calibration plots, grid tuning, and file storage for easy production implementation. Handles regression, quantile regression, time until event, and classification models (binary and multinomial) using numeric and factor variables without the need for one-hot-encoding.
* Models include:
  * RandomForest (DRF)
  * GBM
  * Deeplearning
  * XGBoost (for Linux)
  * lightGBM (for Linux)
  * AutoML - medium debth grid tuning for Deeplearning, XGBoost (if available), DRF, GBM, GLM, and StackedEnsembles
2. **AutoTS**: Great at extrapolating out of time. Automatically finds the best model fit from the suite of models below (using optimized box-cox transformations), generates forecasts and evaluation metrics.
* Models include:
  * ARIFIMA: Auto Regressive Fractional Integrated Moving Average
  * ARIMIA: Stepwise Auto Regressive Integrated Moving Average with specified max lags, seasonal lags, moving averages, and seasonal moving averages
  * ESM: Additive and Multiplicitive Exponential Smoothing and Holt Winters
  * ARNN: Auto Regressive Neural Network models automatically compares models with 1 lag or 1 seasonal lag compared to models with up to 50 lags and 50 seasonal lags
  * TBATS: Exponential smoothing state space model with Box-Cox transformation, ARMA errors, Trend and Seasonal components
  * TSLM: Time Series Linear Model - builds a linear model with trend and season components extracted from the data
  * CSS: Cubic Smoothing Spline regression on an ARIMA(0,2,2) model structure
  * Prophet: Additive model where non-linear trends are fit with yearly, weekly, and daily seasonality, plus holiday effects
3. **nlsModelFit**: Great at extrapolating from out of range from an independent variable. Automatically finds the best model fit from the suite of models below and merges predictions to source data file.
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
4. **GenTSAnomVars**: - (Cross with Feature Engineering) Create indicator variables (high, low) along with cumulative anomaly rates (high, low) based on control limits methodology over a max of two grouping variables and a date variable (effectively a rolling GLM).
5. **ResidualOutliers**: (Cross with Feature Engineering) Utilize tsoutliers to indicate outliers within a time series data set
6. **GLRM_KMeans_Col**: (Possible cross with Feature Engineering) Generate a column with a cluster identifier based on a grid tuned (optional) generalized low rank model and a K-Optimal searching K-Means algorithm

##### Feature Engineering Functions: 
7. **FAST_GDL_Feature_Engineering**: Rapidly generate time between events, autoregressive, moving average / standard deviation / min / max / quantile 85 / quantile 95 for when you want to generate these features only for predicting events at the latest time interval of the data set.
8. **GDL_Feature_Engineering**: Generate a wider set of features (similar in structure to FAST_GDL) using any aggregation statistic for the rolling stats.
9. **Scoring_GDL_Feature_Engineering**: Generate the model features from FAST_GDL or GDL for scoring purposes when the scoring data is for forward looking predictions (not historical, which can be obtained from FAST_GDL or GDL).
10. **DT_GDL_Feature_Engineering**: Lags + Moving Averages, all in data.table
11. **Word2VecModel**: Generate a specified number of vectors for each column of text data in your data set and save the models for re-creating them later in the scoring process.
12. **ModelDataPrep**: Rapidly convert "inf" values to NA, convert character columns to factor columns, and impute with specified values for factor and numeric columns (factors are necessary (no characters values) for H20).
13. **DummifyDT** - rapidly dichotomize a list of columns in a data table (N+1 columns for N levels)

##### Model Evaluation, Interpretation, and Cost-Sensitive Functions: 
14. **ParDepCalPlots**: Great for features effects estimation and reliability of model in predicting those effects. Build a partial dependence calibration plot on train, test, or all data
15. **EvalPlot**: Great for assessing accuracy across range of predicted values. Build a calibration plot on test data
16. **threshOptim**: Great for situations with asymmetric costs across the confusion matrix. Generate a cost-sensitive optimized threshold for classification models
17. **RedYellowGreen** - computes optimal thresholds for binary classification models when "don't classify" is an option

##### Utilities and Misc. Functions:
18. CountSingleDigits - From a project. Can probably delete.
19. tokenizeH20 - Tokenize and H20 string column.
20. tempDatesFun - Special case for character conversion to date when importing from Excel.
21. RemixTheme - Fonts, colors, style for plots.
22. ChartTheme - Fonts, colors, style for plots.
23. SimpleCap - Apply proper case to text.
24. percRank - Inner function for calibration plots and partial dependence plots. Computes PercentRank.
25. multiplot - Useful for displaying multiple plots in a single pane.
26. PrintObjectsSize - print out objects and their sizes that are in the envrionment
27. NumWeekdays - vectorized function to count up the number of weekdays in a range of dates
28. HolidayCounts - vectorized function to count up the number of holidays in a range of dates
