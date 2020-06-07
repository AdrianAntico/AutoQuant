![Version: 0.2.3](https://img.shields.io/static/v1?label=Version&message=0.2.3&color=blue&?style=plastic)
![Build: Passing](https://img.shields.io/static/v1?label=Build&message=passing&color=brightgreen)
[![License: MPL 2.0](https://img.shields.io/badge/License-MPL%202.0-brightgreen.svg)](https://opensource.org/licenses/MPL-2.0)
[![Maintenance](https://img.shields.io/badge/Maintained%3F-yes-green.svg)](https://GitHub.com/Naereen/StrapDown.js/graphs/commit-activity)
![Contributors: 4](https://img.shields.io/static/v1?label=Contributors&message=4&color=Orange)
[![GitHub issues](https://img.shields.io/github/issues/AdrianAntico/RemixAutoML)](https://GitHub.com/AdrianAntico/RemixAutoML/issues/)
[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg?style=default)](http://makeapullrequest.com)

<img src="https://github.com/AdrianAntico/RemixAutoML/blob/master/Images/NewPackageLogo.png" align="center" width="1000" />

## Install RemixAutoML:

<details><summary>Expand to view content</summary>
<p>

#### 1. First, install R package dependencies: 
XGBoost runs significantly faster with GPU (it's already pretty fast on CPU) but it can be tricky to get installed. The blog below has been shown to be reliable for getting it done.
 [Install XGBoost on Windows for R with GPU Capability](https://medium.com/@karthikdulam/installing-xgboost-gpu-for-r-on-windows-10-7927a65c0ca8)
 
```
# Install Dependencies----
if(!("remotes" %in% rownames(installed.packages()))) install.packages("remotes"); print("remotes")
if(!("arules" %in% rownames(installed.packages()))) install.packages("arules"); print("arules")
if(!("bit64" %in% rownames(installed.packages()))) install.packages("bit64"); print("bit64")
if(!("caTools" %in% rownames(installed.packages()))) install.packages("caTools"); print("caTools")
if(!("combinat" %in% rownames(install.packages()))) install.packages("combinat"); print("combinat")
if(!("data.table" %in% rownames(installed.packages()))) install.packages("data.table"); print("data.table")
if(!("doParallel" %in% rownames(installed.packages()))) install.packages("doParallel"); print("doParallel")
if(!("e1071" %in% rownames(installed.packages()))) install.packages("e1071"); print("e1071")
if(!("fBasics" %in% rownames(installed.packages()))) install.packages("fBasics"); print("fBasics")
if(!("foreach" %in% rownames(installed.packages()))) install.packages("foreach"); print("foreach")
if(!("forecast" %in% rownames(installed.packages()))) install.packages("forecast"); print("forecast")
if(!("fpp" %in% rownames(installed.packages()))) install.packages("fpp"); print("fpp")
if(!("ggplot2" %in% rownames(installed.packages()))) install.packages("ggplot2"); print("ggplot2")
if(!("gridExtra" %in% rownames(installed.packages()))) install.packages("gridExtra"); print("gridExtra")
if(!("here" %in% rownames(installed.packages()))) install.packages("here"); print("here")
if(!("itertools" %in% rownames(installed.packages()))) install.packages("itertools"); print("itertools")
if(!("lime" %in% rownames(installed.packages()))) install.packages("lime"); print("lime")
if(!("lubridate" %in% rownames(installed.packages()))) install.packages("lubridate"); print("lubridate")
if(!("Matrix" %in% rownames(installed.packages()))) install.packages("Matrix"); print("Matrix")
if(!("MLmetrics" %in% rownames(installed.packages()))) install.packages("MLmetrics"); print("MLmetrics")
if(!("monreg" %in% rownames(installed.packages()))) install.packages("monreg"); print("monreg")
if(!("nortest" %in% rownames(installed.packages()))) install.packages("nortest"); print("nortest")
if(!("RColorBrewer" %in% rownames(installed.packages()))) install.packages("RColorBrewer"); print("RColorBrewer")
if(!("recommenderlab" %in% rownames(installed.packages()))) install.packages("recommenderlab"); print("recommenderlab")
if(!("ROCR" %in% rownames(installed.packages()))) install.packages("ROCR"); print("ROCR")
if(!("pROC" %in% rownames(installed.packages()))) install.packages("pROC"); print("pROC")
if(!("Rcpp" %in% rownames(installed.packages()))) install.packages("Rcpp"); print("Rcpp")
if(!("scatterplot3d" %in% rownames(installed.packages()))) install.packages("scatterplot3d"); print("scatterplot3d")
if(!("stringr" %in% rownames(installed.packages()))) install.packages("stringr"); print("stringr")
if(!("sde" %in% rownames(installed.packages()))) install.packages("sde"); print("sde")
if(!("timeDate" %in% rownames(installed.packages()))) install.packages("timeDate"); print("timeDate")
if(!("tsoutliers" %in% rownames(installed.packages()))) install.packages("tsoutliers"); print("tsoutliers")
if(!("wordcloud" %in% rownames(installed.packages()))) install.packages("wordcloud"); print("wordcloud")
if(!("xgboost" %in% rownames(installed.packages()))) install.packages("xgboost"); print("xgboost")
for (pkg in c("RCurl","jsonlite")) if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
install.packages("h2o", type="source", repos="http://h2o-release.s3.amazonaws.com/h2o/rel-zahradnik/1/R")
remotes::install_github('catboost/catboost', subdir = 'catboost/R-package')
remotes::install_github('AdrianAntico/RemixAutoML', upgrade = FALSE, dependencies = FALSE, force = TRUE)
```

### Installation Troubleshooting 
The most common issue some users are having when trying to install <code>RemixAutoML</code> is the installation of the <code>catboost</code> package dependency. Since <code>catboost</code> is not on CRAN it can only be installed through GitHub. To install <code>catboost</code> without error (and consequently install <code>RemixAutoML</code> without error), try running this line of code first, then restart your R session, then re-run the 2-step installation process above. (<a href="https://github.com/catboost/catboost/issues/612" target="_blank">Reference</a>):
If you're still having trouble submit an issue and I'll work with you to get it installed.

```
# Be sure to use the version you want versus what is listed below
options(devtools.install.args = c("--no-multiarch", "--no-test-load"))
install.packages("https://github.com/catboost/catboost/releases/download/v0.17.3/catboost-R-Windows-0.17.3.tgz", repos = NULL, type = "source", INSTALL_opts = c("--no-multiarch", "--no-test-load"))
```
 
If you're having still having trouble installing see if the issue below helps out:

![Issue #19](https://github.com/AdrianAntico/RemixAutoML/issues/19)

</p>
</details>

## Intro to RemixAutoML:

<details><summary>Expand to view content</summary>
<p>

> Automated Machine Learning - In my view, AutoML should consist of functions to help make professional model development and operationalization more efficient. Most ML projects include at least one of the following: data wrangling, feature engineering, feature selection, model development, model evaluation, model interpretation, model optimization, and model operationalization. The functions in this package have been tested across a variety of industries and have consistently out-performd "state of the art" deep learning methods. I've watched coworkers spend months tuning and reconfiguring deep learning models just to have them lose to the functions here, in a matter of a day or two. My recommendation is to first utilize the functions here to establish a legit baseline performance. Then go and test out all the other methods. But please don't come to me and say that you built a deep learning model that outperforms a linear or logistic regression. Those comparisons are for research, when you are trying something completely new, where a better performance than a linear regression means you are heading in the right direction.

### Package Details
> Supervised Learning - Currently, I'm utilizing CatBoost, XGBoost, and H2O for all of the automated Machine Learning related functions. GPU's can be utilized with CatBoost and XGBoost. Multi-armed bandit grid tuning is available for CatBoost and XGBoost models, which utilize the concept of randomized probability matching, which is detailed in the R pacakge "bandit". 

> Time series forecasting - Automated functions for single series and panel data. The panel data models utilize the machine learning algos from above and the feature engineering functions below. There are extremely feature rich. The models for individual series are fully optimized versions from the R package "forecast". I utilize the multi-armed bandit grid tuning algo used in the supervised learning models and apply it to the SARIMA and NNETAR models from "forecast". I also measure performance on hold out data (and training data, or a blend of the two) which is not offered pretty much anywhere.

> Feature Engineering - Some of the feature engineering functions you can only find in this package, such as the <code>AutoLagRollStats()</code> and <code>AutoLagRollStatsScoring()</code> functions. You could classify the above functions into several buckets: categorical encoding, target encoding, distributed lag. You can generate any number of discontiguous lags and rolling statistics (mean, sd, skewness, kurtosis, and every 5th percentile) along with time between records and their associated lags and rolling statistics for transactional level data. The function runs extremely fast if you don't utilize rolling stats other than mean (I still use <code>data.table::frollapply()</code> but the data.table guys admit it isn't optimized like the <code>data.table::frollmean()</code> function). Furthermore, you can generate all these features by any number of categorical variables and their interactions PLUS you can request those sets of features to be generated for differnt levels of time aggregations such as transactional, hourly, daily, weekly, monthly, quarterly, and yearly, all in one shot (that is, you do not have to run the function repeated to deliver those features). Lastly, generating these kinds of time series features on the fly for only a subset of records in a data.table (typically for on-demand model scoring) is not an easy task to do correctly and quickly. However, I spent the time to make it run as fast as I could but I am open to suggestions for making it faster (that goes for any of the functions in RemixAutoML).

> Data Management - Every function here is written with fully-optimized data.table code so they run blazingly fast and are as memory efficient as possible. The current set of machine learning algorithms were chosen for their ability to work with big data and their ability to outperform other models, as demonstrated across a variety of real world use cases. The focus of the package is quality, not quantity.

> Documentation - For every exported function in the package you can pull up the help file, e.g. <code>?RemixAutoML::ModelDataPrep</code>. Many of them come with examples coded up in the help files that you can run to get a feel for how to set the parameters. There's also a full listing of all exported functions by category at the bottom of this readme and you can jump into the R folder here to dig into the code. 

> Contact - You can reach me via <a href="https://www.linkedin.com/in/adrian-antico/" target="_blank">LinkedIn</a> for any questions about the package. If you want to contribute shoot me an email on there.

</p>
</details>

## Common Workflows with RemixAutoML:

<details><summary>Expand to view content</summary>
<p>

### Supervised Learning
#### An example workflow with function references
<details><summary>Expand to view content</summary>
<p>
 
1. Pull in data from your data warehouse (or from wherever) and clean it up
2. Run all the applicable feature engineering functions, such as <code>AutoLagRollStats()</code>, <code>AutoWord2VecModeler()</code>, <code>CreateCalendarVariables()</code>, <code>CreateHolidayVariables()</code>, etc.
3. Partition your data with <code>AutoDataPartition()</code> if you want to go with a data split other than 70/20/10, which is automatically applied in the supervised learning functions if you don't supply the ValidationData and TestData (and TrainOnFull is set to FALSE).
4. Run <code>AutoCatBoostRegression()</code> or <code>AutoCatBoostClassifier()</code> or <code>AutoCatBoostMultiClass()</code> with GPU if you have access to one
5. Run <code>AutoXGBoostRegression()</code> or <code>AutoXGBoostClassifier()</code> or <code>AutoXGBoostMultiClass()</code> with GPU if you have access to one
6. Run <code>AutoH2oGBMRegression()</code> or <code>AutoH2oGBMClassifier()</code> or <code>AutoH2oGBMMultiClass()</code> if you have the patience to wait for a CPU build.
7. Run <code>AutoH2oGLMRegression()</code> or <code>AutoH2oGLMClassifier()</code> or <code>AutoH2oGLMMultiClass()</code> if you want to give a generalized linear model a shot.
8. Run <code>AutoH2oMLRegression()</code> or <code>AutoH2oMLClassifier()</code> or <code>AutoH2oMLMultiClass()</code> to run H2O's AutoML function inside the RemixAutoML framework.
9. Run <code>AutoH2oDRFRegression()</code> or <code>AutoH2oDRFClassifier()</code> or <code>AutoH2oDRFMultiClass()</code> H2O's Distributed Random Forest can take a really long time to build. H2O's documentation has a great explanation for the reason why it takes much longer compared to their GBM algo.
10. Investigate model performance contained in the output object returned by those functions. You will be able to look at model calibration plots or box plots, ROC plots, partial depence calibration plots or boxplots, model metrics, etc.
12. If you ran one of the <code>Auto__Classifer()</code> function supply the validation to the function <code>RemixClassificationMetrics()</code> for an exhaustive threshold analysis
12. Pick your model of choice and kick off an extended grid tuning and figure out something else to do that week (or run it over the weekend). 
13. Compare your results with your coworkers results and see what's working and what isn't. Then you can either move on or continue exploring. Bargain with your boss to get that time to explore so you can learn new things.

</p>
</details>

### Forecasting
#### Single series and panel data using Time Series models or Machine Learning models

<details><summary>Expand to view content</summary>
<p>
 
Supply a data.table to run the functions below:
1. For single series check out <code>AutoBanditSarima()</code>, <code>AutoBanditNNet()</code>, <code>AutoTBATS()</code>, or <code>AutoTS()</code> (older function; no longer developing)
2. For panel data OR single series check out <code>AutoCatBoostCARMA()</code>, <code>AutoXGBoostCARMA()</code>, <code>AutoH2oMLCARMA()</code>, <code>AutoH2oGBMCARMA()</code>, <code>AutoH2oGLMCARMA()</code>, or <code>AutoH2oDRFCARMA()</code> or build a loop and run functions from (a)

</p>
</details>

</p>
</details>

## RemixAutoML Blogs: 

<details><summary>Expand to view content</summary>
<p>

[AutoML Frameworks in R & Python](https://iamnagdev.com/2020/04/01/automl-frameworks-in-r-python/)

[AI for Small to Medium Size Businesses: A Management Take On The Challenges...](https://www.remixinstitute.com/blog/business-ai-for-small-to-medium-sized-businesses-with-remixautoml/#.XX-lD2ZlD8A)

[Why Machine Learning is more Practical than Econometrics in the Real World](https://medium.com/@adrianantico/machine-learning-vs-econometrics-in-the-real-world-4058095b1013)

[Build Thousands of Automated Demand Forecasts in 15 Minutes Using AutoCatBoostCARMA in R](https://www.remixinstitute.com/blog/automated-demand-forecasts-using-autocatboostcarma-in-r/#.XUIO1ntlCDM)

[Automate Your KPI Forecasts With Only 1 Line of R Code Using AutoTS](https://www.remixinstitute.com/blog/automate-your-kpi-forecasts-with-only-1-line-of-r-code-using-autots/#.XUIOr3tlCDM)

[Companies Are Demanding Model Interpretability. Here’s How To Do It Right](https://www.remixinstitute.com/blog/companies-are-demanding-model-interpretability-heres-how-to-do-it-right/#.XUIN1HtlCDM)

[The Easiest Way to Create Thresholds And Improve Your Classification Model](https://www.remixinstitute.com/blog/the-easiest-way-to-create-thresholds-and-improve-your-classification-model/#.XUINVntlCDM)

</p>
</details>

<img src="https://github.com/AdrianAntico/RemixAutoML/blob/master/Images/FunctionsReference.png" align="center" width="350" />

## Automated Feature Engineering Functions: <img src="Images/FeatureEngineeringImage2.png" align="right" width="80" />
<details><summary>Expand to view content</summary>
<p>

##### **AutoLagRollStats()** and **AutoLagRollStatsScoring()**

<details><summary>Code Example: AutoLagRollStats()</summary>
<p>

```
# Create fake data with a Date----
data <- RemixAutoML::FakeDataGenerator(
  Correlation = 0.75,
  N = 25000L,
  ID = 2L,
  ZIP = 0L,
  FactorCount = 2L,
  AddDate = TRUE,
  Classification = FALSE,
  MultiClass = FALSE)
data.table::setnames(x = data, old = c("Factor_1","Factor_2"), new = c("Factor1","Factor2"))
data.table::setorderv(x = data, cols = c("Factor1","Factor2","DateTime"))

# Add scoring records
data <- RemixAutoML::AutoLagRollStats(

  # Data
  data                 = data,
  DateColumn           = "DateTime",
  Targets              = "Adrian",
  HierarchyGroups      = c("Factor1","Factor2"),
  IndependentGroups    = NULL,
  TimeUnitAgg          = "days",
  TimeGroups           = c("days", "weeks"),
  TimeBetween          = NULL,
  TimeUnit             = "days",
  
  # Services
  RollOnLag1           = TRUE,
  Type                 = "Lag",
  SimpleImpute         = TRUE,

  # Calculated Columns
  Lags                  = list("days" = c(seq(1,5,1)), "weeks" = c(seq(1,3,1)), "months" = c(seq(1,2,1))),
  MA_RollWindows        = list("days" = c(seq(1,5,1)), "weeks" = c(seq(1,3,1)), "months" = c(seq(1,2,1))),
  SD_RollWindows        = NULL,
  Skew_RollWindows      = NULL,
  Kurt_RollWindows      = NULL,
  Quantile_RollWindows  = NULL,
  Quantiles_Selected   = c("q5","q10","q95"),
  Debug                = FALSE)
```

</p>
</details>

<details><summary>Code Example: AutoLagRollStatsScoring()</summary>
<p>

```
# Create fake data with a Date----
data <- RemixAutoML::FakeDataGenerator(
  Correlation = 0.75,
  N = 25000L,
  ID = 2L,
  ZIP = 0L,
  FactorCount = 2L,
  AddDate = TRUE,
  Classification = FALSE,
  MultiClass = FALSE)
data.table::setnames(x = data, old = c("Factor_1","Factor_2"), new = c("Factor1","Factor2"))
data.table::setorderv(x = data, cols = c("Factor1","Factor2","DateTime"))
data[1L:(.N - 100L), ScoreRecords := 2L]
data[(.N - 99L):.N, ScoreRecords := 1L]

# Add scoring records
data <- RemixAutoML::AutoLagRollStatsScoring(

  # Data
  data                 = data,
  RowNumsID            = "ScoreRecords",
  RowNumsKeep          = 1,
  DateColumn           = "DateTime",
  Targets              = "Adrian",
  HierarchyGroups      = c("Factor1","Factor2"),
  IndependentGroups    = NULL,
  TimeUnitAgg          = "days",
  TimeGroups           = c("days", "weeks"),
  TimeBetween          = NULL,
  TimeUnit             = "days",
  
  # Services
  RollOnLag1           = TRUE,
  Type                 = "Lag",
  SimpleImpute         = TRUE,

  # Calculated Columns
  Lags                  = list("days" = c(seq(1,5,1)), "weeks" = c(seq(1,3,1)), "months" = c(seq(1,2,1))),
  MA_RollWindows        = list("days" = c(seq(1,5,1)), "weeks" = c(seq(1,3,1)), "months" = c(seq(1,2,1))),
  SD_RollWindows        = NULL,
  Skew_RollWindows      = NULL,
  Kurt_RollWindows      = NULL,
  Quantile_RollWindows  = NULL,
  Quantiles_Selected   = c("q5","q10","q95"),
  Debug                = FALSE)
```

</p>
</details>

<code>AutoLagRollStats()</code> builds lags and rolling statistics by grouping variables and their interactions along with multiple different time aggregations if selected. Rolling stats include mean, sd, skewness, kurtosis, and the 5th - 95th percentiles. This function was inspired by the distributed lag modeling framework but I wanted to use it for time series analysis as well and really generalize it as much as possible. The beauty of this function is inspired by analyzing whether a baseball player will get a basehit or more in his next at bat. One easy way to get a better idea of the likelihood is to look at his batting average and his career batting average. However, players go into hot streaks and slumps. How do we account for that? Well, in comes the functions here. You look at the batting average over the last N to N+x at bats, for various N and x. I keep going though - I want the same windows for calculating the players standard deviation, skewness, kurtosis, and various quantiles over those time windows. I also want to look at all those measure but by using weekly data - as in, over the last N weeks, pull in those stats too. 

<code>AutoLagRollStatsScoring()</code> builds the above features for a partial set of records in a data set. The function is extremely useful as it can compute these feature vectors at a significantly faster rate than the non scoring version which comes in handy for scoring ML models. If you can find a way to make it faster, let me know.

##### **AutoWord2VecModeler()**
<code>AutoWord2VecModeler()</code> generates a specified number of vectors (word2vec) for each column of text data in your data set that you specify and it will save the models if you specify for re-creating them later in a model scoring process. You can choose to build individual models for each column or one model for all your columns. If you need to run several models for groups of text variables you can run the function several times. 

##### **CreateCalendarVariables()**
<code>ModelDataPrep()</code> This functions creates numerical columns based on the date columns you supply such as second, minute, hour, week day, day of month, day of year, week, isoweek, month, quarter, and year.

<details><summary>Code Example</summary>
<p>
 
```
# Create fake data with a Date----
data <- RemixAutoML::FakeDataGenerator(
  Correlation = 0.75, 
  N = 25000L, 
  ID = 2L, 
  ZIP = 0L, 
  FactorCount = 4L, 
  AddDate = TRUE, 
  Classification = FALSE, 
  MultiClass = FALSE)
for(i in seq_len(20L)) {
  print(i)
  data <- data.table::rbindlist(list(data, RemixAutoML::FakeDataGenerator(
    Correlation = 0.75, 
    N = 25000L, 
    ID = 2L, 
    ZIP = 0L, 
    FactorCount = 4L, 
    AddDate = TRUE, 
    Classification = FALSE, 
    MultiClass = FALSE)))
}

# Run function and time it
runtime <- system.time(
  data <- RemixAutoML::CreateCalendarVariables(
    data = data,
    DateCols = "DateTime",
    AsFactor = FALSE,
    TimeUnits = c("second", "minute", "hour", "wday", "mday", "yday", "week", "isoweek", "month", "quarter", "year")))
head(data)
print(runtime)
```

</p>
</details>

##### **CreateHolidayVariable()**
<code>CreateHolidayVariable()</code> This function counts up the number of specified holidays between the current record time stamp and the previous record time stamp, by group as well if specified.

<details><summary>Code Example</summary>
<p>

```
# Create fake data with a Date----
data <- RemixAutoML::FakeDataGenerator(
  Correlation = 0.75, 
  N = 25000L, 
  ID = 2L, 
  ZIP = 0L, 
  FactorCount = 4L, 
  AddDate = TRUE, 
  Classification = FALSE, 
  MultiClass = FALSE)
for(i in seq_len(20L)) {
  print(i)
  data <- data.table::rbindlist(list(data, RemixAutoML::FakeDataGenerator(
    Correlation = 0.75, 
    N = 25000L, 
    ID = 2L, 
    ZIP = 0L, 
    FactorCount = 4L, 
    AddDate = TRUE, 
    Classification = FALSE, 
    MultiClass = FALSE)))
}

# Run function and time it
runtime <- system.time(
  data <- CreateHolidayVariables(
    data,
    DateCols = "DateTime",
    HolidayGroups = c("USPublicHolidays","EasterGroup","ChristmasGroup","OtherEcclesticalFeasts"),
    Holidays = NULL,
    GroupingVars = c("Factor_1","Factor_2","Factor_3","Factor_4"),
    Print = FALSE))
head(data)
print(runtime)
```

</p>
</details>

##### **AutoHierarchicalFourier()**
<code>AutoHierarchicalFourier()</code> turns time series data into fourier series. This function can generate any number of fourier pairs the user wants (if they can actually build) and you can run it with grouped time series data. In the grouping case, fourier pairs can be created for each categorical variable along with the full interactions between specified categoricals. The process is parallelized as well to run as fast as possible.

##### **AutoTransformationCreate()** and **AutoTransformationScore()**
<code>AutoTransformationCreate()</code> is a function for automatically identifying the optimal transformations for numeric features and transforming them once identified. This function will loop through your selected transformation options (YeoJohnson, BoxCox, Asinh, Log, LogPlus1, along with Asin and Logit for proportion data) and find the one that produces the best fit to a normal distribution. It then generates the transformation and collects the metadata information for use in the AutoTransformationScore() function, either by returning the objects or saving them to file.

<code>AutoTransformationScore()</code> is a the compliment function to AutoTransformationCreate(). Automatically apply or inverse the transformations you identified in AutoTransformationCreate() to other data sets. This is useful for applying transformations to your validation and test data sets for modeling, which is done automatically for you if you specify.

##### **ModelDataPrep()**
<code>ModelDataPrep()</code> This function will loop through every column in your data and apply a variety of functions based on argument settings. For all columns not ignored, these tasks include:
* Character type to Factor type converstion
* Factor type to Character type conversion
* Constant value imputation for numeric and categorical columns
* Integer type to Numeric type conversion
* Date type to Character type conversion
* Remove date columns
* Ignore specified columns

##### **DummifyDT()** 
<code>DummifyDT()</code> This function is used in the AutoXGBoost__() suite of modeling functions to manage categorical variables in your training, validation, and test sets. This function rapidly dichotomizes categorical columns in a data.table (N+1 columns for N levels using one hot encoding or N columns for N levels otherwise). Several other arguments exist for outputting and saving factor levels. This is useful in model training, validating, and scoring processes.

##### **AutoDataPartition()**
<code>AutoDataPartition()</code> is designed to achieve a few things that standard data partitioning processes or functions don't handle. First, you can choose to build any number of partitioned data sets beyond the standard train, validate, and test data sets. Second, you can choose between random sampling to split your data or you can choose a time-based partitioning. Third, for the random partitioning, you can specify a stratification columns in your data to stratify by in order to ensure a proper split amongst your categorical features (E.g. think MultiClass targets). Lastly, it's 100% data.table so it will run fast and with low memory overhead.

##### **AutoDataDictionary()**
<code>AutoDataDictionary()</code> will pull back data dictionary data from a sql server data warehouse and run queries to pull in data to R. There are several data dictionary types returned, such as returning every table that exists along with every column with metadata information. Another good one is to pull back all tables and their counterparts that can be used in joins, along with the joining sql.

##### **DT_GDL_Feature_Engineering()** and **Partial_DT_GDL_Feature_Engineering()**
<code>DT_GDL_Feature_Engineering()</code> Runs in the background of AutoLagRollStats(). It builds autoregressive and moving average features from target columns and distributed lags and distributed moving average from independent features distributed across time. On top of that, you can also create time between instances along with their associated lags and moving averages. This function works for data with groups and without groups. 100% data.table built. It runs super fast and can handle big data.

<code>Partial_DT_GDL_Feature_Engineering()</code> Runs in the background of AutoLagRollStatsScoring().  is for generating the equivalent features built from DT_GDL_Feature_Engineering() for a set of new records as rapidly as possible. I used this to create the feature vectors for scoring models in production. This function is for generating lags and moving averages (along with lags and moving averages off of time between records), for a partial set of records in your data set, typical new records that become available for model scoring. Column names and ordering will be identical to the output from the corresponding DT_GDL_Feature_Engineering() function, which most likely was used to create features for model training. 

</p>
</details>

## Automated Supervised Learning Functions: <img src="Images/SupervisedLearningImage.png" align="right" width="80" />
<details><summary>Expand to view content</summary> 
<p>
  
#### Regression:
____________________________________________________________________________________________________________________________________________
<details><summary>click to expand</summary>
<p>
  
##### **AutoCatBoostRegression()** GPU Capable
<code>AutoCatBoostRegression()</code> utilizes the CatBoost algorithm in the below steps

<details><summary>Code Example</summary>
<p>
 
 ```
 # Create some dummy correlated data with numeric and categorical features
data <- RemixAutoML::FakeDataGenerator(Correlation = 0.85, N = 1000, ID = 2, ZIP = 0, AddDate = FALSE, Classification = FALSE, MultiClass = FALSE)

# Run function
TestModel <- RemixAutoML::AutoCatBoostRegression(

    # GPU or CPU and the number of available GPUs
    task_type = "GPU",
    NumGPUs = 1,

    # Metadata arguments:
    #   'ModelID' is used to create part of the file names generated when saving to file'
    #   'model_path' is where the minimal model objects for scoring will be stored
    #      'ModelID' will be the name of the saved model object
    #   'metadata_path' is where model evaluation and model interpretation files are saved
    #      objects saved to model_path if metadata_path is null
    #      Saved objects include:
    #         'ModelID_ValidationData.csv' is the supplied or generated TestData with predicted values
    #         'ModelID_VariableImportance.csv' is the variable importance.
    #            This won't be saved to file if GrowPolicy is either "Depthwise" or "Lossguide" was used
    #         'ModelID_ExperimentGrid.csv' if GridTune = TRUE.
    #            Results of all model builds including parameter settings, bandit probs, and grid IDs
    #         'ModelID_EvaluationMetrics.csv' which contains MSE, MAE, MAPE, R2
    ModelID = "Test_Model_1",
    model_path = normalizePath("./"),
    metadata_path = NULL,
    SaveModelObjects = FALSE,
    ReturnModelObjects = TRUE,

    # Data arguments:
    #   'TrainOnFull' is to train a model with 100 percent of your data.
    #     That means no holdout data will be used for evaluation
    #   If ValidationData and TestData are NULL and TrainOnFull is FALSE then data will be split 70 20 10
    #   'PrimaryDateColumn' is a date column in data that is meaningful when sorted.
    #     CatBoost categorical treatment is enhanced when supplied
    #   'IDcols' are columns in your data that you don't use for modeling but get returned with ValidationData
    #   'TransformNumericColumns' is for transforming your target variable. Just supply the name of it
    data = data,
    TrainOnFull = FALSE,
    ValidationData = NULL,
    TestData = NULL,
    TargetColumnName = "Adrian",
    FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
    PrimaryDateColumn = NULL,
    IDcols = c("IDcol_1","IDcol_2"),
    TransformNumericColumns = "Adrian",
    Methods = c("BoxCox", "Asinh", "Asin", "Log", "LogPlus1", "Logit", "YeoJohnson"),

    # Model evaluation:
    #   'eval_metric' is the measure catboost uses when evaluting on holdout data during its bandit style process
    #   'loss_function' the loss function used in training optimization
    #   'NumOfParDepPlots' Number of partial dependence calibration plots generated.
    #     A value of 3 will return plots for the top 3 variables based on variable importance
    #     Won't be returned if GrowPolicy is either "Depthwise" or "Lossguide" is used
    #     Can run the RemixAutoML::ParDepCalPlots() with the outputted ValidationData
    eval_metric = "RMSE",
    loss_function = "RMSE",
    MetricPeriods = 10L,
    NumOfParDepPlots = ncol(data)-1L-2L,

    # Grid tuning arguments:
    #   'PassInGrid' is for retraining using a previous grid winning args
    #   'MaxModelsInGrid' is a cap on the number of models that will run
    #   'MaxRunsWithoutNewWinner' number of runs without a new winner before exiting grid tuning
    #   'MaxRunMinutes' is a cap on the number of minutes that will run
    #   'Shuffles' is the number of times you want the random grid arguments shuffled
    #   'BaselineComparison' default means to compare each model build with a default built of catboost using max(Trees)
    #   'MetricPeriods' is the number of trees built before evaluting holdoutdata internally. Used in finding actual Trees used.
    PassInGrid = NULL,
    GridTune = FALSE,
    MaxModelsInGrid = 100L,
    MaxRunsWithoutNewWinner = 100L,
    MaxRunMinutes = 60*60,
    Shuffles = 4L,
    BaselineComparison = "default",

    # Trees, Depth, and LearningRate used in the bandit grid tuning
    # Must set Trees to a single value if you are not grid tuning
    # The ones below can be set to NULL and the values in the example will be used
    # GrowPolicy is turned off for CPU runs
    # BootStrapType utilizes Poisson only for GPU and MVS only for CPU
    Trees = seq(100L, 500L, 50L),
    Depth = seq(4L, 8L, 1L),
    LearningRate = seq(0.01,0.10,0.01),
    L2_Leaf_Reg = seq(1.0, 10.0, 1.0),
    RSM = c(0.80, 0.85, 0.90, 0.95, 1.0),
    BootStrapType = c("Bayesian", "Bernoulli", "Poisson", "MVS", "No"),
    GrowPolicy = c("SymmetricTree", "Depthwise", "Lossguide"))
 ```
 
</p>
</details> 

##### **AutoXGBoostRegression()** GPU Capable
<code>AutoXGBoostRegression()</code> utilizes the XGBoost algorithm in the below steps 

<details><summary>Code Example</summary>
<p>
 
```
# Create some dummy correlated data with numeric and categorical features
data <- RemixAutoML::FakeDataGenerator(Correlation = 0.85, N = 1000, ID = 2, ZIP = 0, AddDate = FALSE, Classification = FALSE, MultiClass = FALSE)

# Run function
TestModel <- RemixAutoML::AutoXGBoostRegression(

    # GPU or CPU
    TreeMethod = "hist",
    NThreads = 8L,

    # Metadata arguments:
    #   'ModelID' is used to create part of the file names generated when saving to file'
    #   'model_path' is where the minimal model objects for scoring will be stored
    #      'ModelID' will be the name of the saved model object
    #   'metadata_path' is where model evaluation and model interpretation files are saved
    #      objects saved to model_path if metadata_path is null
    #      Saved objects include:
    #         'ModelID_ValidationData.csv' is the supplied or generated TestData with predicted values
    #         'ModelID_VariableImportance.csv' is the variable importance.
    #            This won't be saved to file if GrowPolicy is either "Depthwise" or "Lossguide" was used
    #         'ModelID_ExperimentGrid.csv' if GridTune = TRUE.
    #            Results of all model builds including parameter settings, bandit probs, and grid IDs
    #         'ModelID_EvaluationMetrics.csv' which contains MSE, MAE, MAPE, R2
    model_path = normalizePath("./"),
    metadata_path = NULL,
    ModelID = "Test_Model_1",
    ReturnFactorLevels = TRUE,
    ReturnModelObjects = TRUE,
    SaveModelObjects = FALSE,

    # Data arguments:
    #   'TrainOnFull' is to train a model with 100 percent of your data.
    #     That means no holdout data will be used for evaluation
    #   If ValidationData and TestData are NULL and TrainOnFull is FALSE then data will be split 70 20 10
    #   'PrimaryDateColumn' is a date column in data that is meaningful when sorted.
    #     CatBoost categorical treatment is enhanced when supplied
    #   'IDcols' are columns in your data that you don't use for modeling but get returned with ValidationData
    #   'TransformNumericColumns' is for transforming your target variable. Just supply the name of it
    data = data,
    TrainOnFull = FALSE,
    ValidationData = NULL,
    TestData = NULL,
    TargetColumnName = "Adrian",
    FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
    IDcols = c("IDcols_1","IDcols_2"),
    TransformNumericColumns = NULL,
    Methods = c("BoxCox", "Asinh", "Asin", "Log", "LogPlus1", "Logit", "YeoJohnson"),

    # Model evaluation
    eval_metric = "rmse",
    NumOfParDepPlots = 3L,

    # Grid tuning arguments - PassInGrid is the best of GridMetrics
    PassInGrid = NULL,
    GridTune = TRUE,
    grid_eval_metric = "mse",
    BaselineComparison = "default",
    MaxModelsInGrid = 10L,
    MaxRunsWithoutNewWinner = 20L,
    MaxRunMinutes = 24L*60L,
    Verbose = 1L,

    # Trees, Depth, and LearningRate used in the bandit grid tuning
    # Must set Trees to a single value if you are not grid tuning
    # The ones below can be set to NULL and the values in the example will be used
    Shuffles = 1L,
    Trees = seq(50L, 500L, 50L),
    eta = seq(0.05,0.40,0.05),
    max_depth = seq(4L, 16L, 2L),
    min_child_weight = seq(1.0, 10.0, 1.0),
    subsample = seq(0.55, 1.0, 0.05),
    colsample_bytree = seq(0.55, 1.0, 0.05))
```
 
</p>
</details> 

##### **AutoH2oGBMRegression()**
<code>AutoH2oGBMRegression()</code> utilizes the H2O Gradient Boosting algorithm in the below steps 

<details><summary>Code Example</summary>
<p>
 
```
# Create some dummy correlated data with numeric and categorical features
data <- RemixAutoML::FakeDataGenerator(Correlation = 0.85, N = 1000, ID = 2, ZIP = 0, AddDate = FALSE, Classification = FALSE, MultiClass = FALSE)

# Run function
TestModel <- RemixAutoML::AutoH2oGBMRegression(

    # Compute management
    MaxMem = "32G",
    NThreads = max(1, parallel::detectCores()-2),
    H2OShutdown = TRUE,
    IfSaveModel = "mojo",
    Alpha = NULL,
    Distribution = "poisson",

    # Model evaluation:
    #   'eval_metric' is the measure catboost uses when evaluting on holdout data during its bandit style process
    #   'NumOfParDepPlots' Number of partial dependence calibration plots generated.
    #     A value of 3 will return plots for the top 3 variables based on variable importance
    #     Won't be returned if GrowPolicy is either "Depthwise" or "Lossguide" is used
    #     Can run the RemixAutoML::ParDepCalPlots() with the outputted ValidationData
    eval_metric = "RMSE",
    NumOfParDepPlots = 3,

    # Metadata arguments:
    #   'ModelID' is used to create part of the file names generated when saving to file'
    #   'model_path' is where the minimal model objects for scoring will be stored
    #      'ModelID' will be the name of the saved model object
    #   'metadata_path' is where model evaluation and model interpretation files are saved
    #      objects saved to model_path if metadata_path is null
    #      Saved objects include:
    #         'ModelID_ValidationData.csv' is the supplied or generated TestData with predicted values
    #         'ModelID_VariableImportance.csv' is the variable importance.
    #            This won't be saved to file if GrowPolicy is either "Depthwise" or "Lossguide" was used
    #         'ModelID_EvaluationMetrics.csv' which contains MSE, MAE, MAPE, R2
    model_path = NULL,
    metadata_path = NULL,
    ModelID = "FirstModel",
    ReturnModelObjects = TRUE,
    SaveModelObjects = FALSE,

    # Data arguments:
    #   'TrainOnFull' is to train a model with 100 percent of your data.
    #     That means no holdout data will be used for evaluation
    #   If ValidationData and TestData are NULL and TrainOnFull is FALSE then data will be split 70 20 10
    #   'PrimaryDateColumn' is a date column in data that is meaningful when sorted.
    #     CatBoost categorical treatment is enhanced when supplied
    #   'IDcols' are columns in your data that you don't use for modeling but get returned with ValidationData
    #   'TransformNumericColumns' is for transforming your target variable. Just supply the name of it
    TrainOnFull = FALSE,
    ValidationData = NULL,
    TestData = NULL,
    TargetColumnName = "Adrian",
    FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
    TransformNumericColumns = NULL,
    Methods = c("BoxCox", "Asinh", "Asin", "Log", "LogPlus1", "Logit", "YeoJohnson"),

    # Model args
    Trees = 50,
    GridTune = FALSE,
    MaxModelsInGrid = 10)
```
 
</p>
</details> 

##### **AutoH2oDRFRegression()**
<code>AutoH2oDRFRegression()</code> utilizes the H2o Distributed Random Forest algorithm in the below steps 

<details><summary>Code Example</summary>
<p>
 
```
 # Create some dummy correlated data with numeric and categorical features
data <- RemixAutoML::FakeDataGenerator(Correlation = 0.85, N = 1000, ID = 2, ZIP = 0, AddDate = FALSE, Classification = FALSE, MultiClass = FALSE)

# Run function
TestModel <- RemixAutoML::AutoH2oDRFRegression(

    # Compute management
    MaxMem = "32G",
    NThreads = max(1, parallel::detectCores()-2),
    H2OShutdown = TRUE,
    IfSaveModel = "mojo",

    # Model evaluation:
    #   'eval_metric' is the measure catboost uses when evaluting on holdout data during its bandit style process
    #   'NumOfParDepPlots' Number of partial dependence calibration plots generated.
    #     A value of 3 will return plots for the top 3 variables based on variable importance
    #     Won't be returned if GrowPolicy is either "Depthwise" or "Lossguide" is used
    #     Can run the RemixAutoML::ParDepCalPlots() with the outputted ValidationData
    eval_metric = "RMSE",
    NumOfParDepPlots = 3,

    # Metadata arguments:
    #   'ModelID' is used to create part of the file names generated when saving to file'
    #   'model_path' is where the minimal model objects for scoring will be stored
    #      'ModelID' will be the name of the saved model object
    #   'metadata_path' is where model evaluation and model interpretation files are saved
    #      objects saved to model_path if metadata_path is null
    #      Saved objects include:
    #         'ModelID_ValidationData.csv' is the supplied or generated TestData with predicted values
    #         'ModelID_VariableImportance.csv' is the variable importance.
    #            This won't be saved to file if GrowPolicy is either "Depthwise" or "Lossguide" was used
    #         'ModelID_ExperimentGrid.csv' if GridTune = TRUE.
    #            Results of all model builds including parameter settings, bandit probs, and grid IDs
    #         'ModelID_EvaluationMetrics.csv' which contains MSE, MAE, MAPE, R2
    model_path = NULL,
    metadata_path = NULL,
    ModelID = "FirstModel",
    ReturnModelObjects = TRUE,
    SaveModelObjects = FALSE,

    # Data arguments:
    #   'TrainOnFull' is to train a model with 100 percent of your data.
    #     That means no holdout data will be used for evaluation
    #   If ValidationData and TestData are NULL and TrainOnFull is FALSE then data will be split 70 20 10
    #   'PrimaryDateColumn' is a date column in data that is meaningful when sorted.
    #     CatBoost categorical treatment is enhanced when supplied
    #   'IDcols' are columns in your data that you don't use for modeling but get returned with ValidationData
    #   'TransformNumericColumns' is for transforming your target variable. Just supply the name of it
    TrainOnFull = FALSE,
    ValidationData = NULL,
    TestData = NULL,
    TargetColumnName = "Adrian",
    FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
    TransformNumericColumns = NULL,
    Methods = c("BoxCox", "Asinh", "Asin", "Log", "LogPlus1", "Logit", "YeoJohnson"),

    # Model args
    Trees = 50,
    GridTune = FALSE,
    MaxModelsInGrid = 10)
```
 
</p>
</details> 

##### **AutoH2oGLMRegression()**
<code>AutoH2oGLMRegression()</code> utilizes the H2o generalized linear model algorithm in the below steps 

<details><summary>Code Example</summary>
<p>
 
```
# Create some dummy correlated data with numeric and categorical features
data <- RemixAutoML::FakeDataGenerator(Correlation = 0.85, N = 1000, ID = 2, ZIP = 0, AddDate = FALSE, Classification = FALSE, MultiClass = FALSE)

# Run function
TestModel <- RemixAutoML::AutoH2oGLMRegression(

    # Compute management
    MaxMem = "32G",
    NThreads = max(1, parallel::detectCores()-2),
    H2OShutdown = TRUE,
    IfSaveModel = "mojo",

    # Model evaluation:
    #   'eval_metric' is the measure catboost uses when evaluting on holdout data during its bandit style process
    #   'NumOfParDepPlots' Number of partial dependence calibration plots generated.
    #     A value of 3 will return plots for the top 3 variables based on variable importance
    #     Won't be returned if GrowPolicy is either "Depthwise" or "Lossguide" is used
    #     Can run the RemixAutoML::ParDepCalPlots() with the outputted ValidationData
    eval_metric = "RMSE",
    NumOfParDepPlots = 3,

    # Metadata arguments:
    #   'ModelID' is used to create part of the file names generated when saving to file'
    #   'model_path' is where the minimal model objects for scoring will be stored
    #      'ModelID' will be the name of the saved model object
    #   'metadata_path' is where model evaluation and model interpretation files are saved
    #      objects saved to model_path if metadata_path is null
    #      Saved objects include:
    #         'ModelID_ValidationData.csv' is the supplied or generated TestData with predicted values
    #         'ModelID_VariableImportance.csv' is the variable importance.
    #            This won't be saved to file if GrowPolicy is either "Depthwise" or "Lossguide" was used
    #         'ModelID_ExperimentGrid.csv' if GridTune = TRUE.
    #            Results of all model builds including parameter settings, bandit probs, and grid IDs
    #         'ModelID_EvaluationMetrics.csv' which contains MSE, MAE, MAPE, R2
    model_path = NULL,
    metadata_path = NULL,
    ModelID = "FirstModel",
    ReturnModelObjects = TRUE,
    SaveModelObjects = FALSE,

    # Data arguments:
    #   'TrainOnFull' is to train a model with 100 percent of your data.
    #     That means no holdout data will be used for evaluation
    #   If ValidationData and TestData are NULL and TrainOnFull is FALSE then data will be split 70 20 10
    #   'PrimaryDateColumn' is a date column in data that is meaningful when sorted.
    #     CatBoost categorical treatment is enhanced when supplied
    #   'IDcols' are columns in your data that you don't use for modeling but get returned with ValidationData
    #   'TransformNumericColumns' is for transforming your target variable. Just supply the name of it
    TrainOnFull = FALSE,
    ValidationData = NULL,
    TestData = NULL,
    TargetColumnName = "Adrian",
    FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
    TransformNumericColumns = NULL,
    Methods = c("BoxCox", "Asinh", "Asin", "Log", "LogPlus1", "Logit", "YeoJohnson"),

    # Model args
    GridTune = FALSE,
    MaxModelsInGrid = 10,
    Distribution = "binomial",
    link = NULL)
```
 
</p>
</details> 

##### **AutoH2oMLRegression()**
<code>AutoH2oMLRegression()</code> utilizes the H2o AutoML algorithm in the below steps 

<details><summary>Code Example</summary>
<p>
 
```
# Create some dummy correlated data with numeric and categorical features
data <- RemixAutoML::FakeDataGenerator(Correlation = 0.85, N = 1000, ID = 2, ZIP = 0, AddDate = FALSE, Classification = FALSE, MultiClass = FALSE)

# Run function
TestModel <- RemixAutoML::AutoH2oMLRegression(

    # Compute management
    MaxMem = "32G",
    NThreads = max(1, parallel::detectCores()-2),
    H2OShutdown = TRUE,
    IfSaveModel = "mojo",

    # Model evaluation:
    #   'eval_metric' is the measure catboost uses when evaluting on holdout data during its bandit style process
    #   'NumOfParDepPlots' Number of partial dependence calibration plots generated.
    #     A value of 3 will return plots for the top 3 variables based on variable importance
    #     Won't be returned if GrowPolicy is either "Depthwise" or "Lossguide" is used
    #     Can run the RemixAutoML::ParDepCalPlots() with the outputted ValidationData
    eval_metric = "RMSE",
    NumOfParDepPlots = 3,

    # Metadata arguments:
    #   'ModelID' is used to create part of the file names generated when saving to file'
    #   'model_path' is where the minimal model objects for scoring will be stored
    #      'ModelID' will be the name of the saved model object
    #   'metadata_path' is where model evaluation and model interpretation files are saved
    #      objects saved to model_path if metadata_path is null
    #      Saved objects include:
    #         'ModelID_ValidationData.csv' is the supplied or generated TestData with predicted values
    #         'ModelID_VariableImportance.csv' is the variable importance.
    #            This won't be saved to file if GrowPolicy is either "Depthwise" or "Lossguide" was used
    #         'ModelID_ExperimentGrid.csv' if GridTune = TRUE.
    #            Results of all model builds including parameter settings, bandit probs, and grid IDs
    #         'ModelID_EvaluationMetrics.csv' which contains MSE, MAE, MAPE, R2
    model_path = NULL,
    metadata_path = NULL,
    ModelID = "FirstModel",
    ReturnModelObjects = TRUE,
    SaveModelObjects = FALSE,

    # Data arguments:
    #   'TrainOnFull' is to train a model with 100 percent of your data.
    #     That means no holdout data will be used for evaluation
    #   If ValidationData and TestData are NULL and TrainOnFull is FALSE then data will be split 70 20 10
    #   'PrimaryDateColumn' is a date column in data that is meaningful when sorted.
    #     CatBoost categorical treatment is enhanced when supplied
    #   'IDcols' are columns in your data that you don't use for modeling but get returned with ValidationData
    #   'TransformNumericColumns' is for transforming your target variable. Just supply the name of it
    TrainOnFull = FALSE,
    ValidationData = NULL,
    TestData = NULL,
    TargetColumnName = "Adrian",
    FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
    TransformNumericColumns = NULL,
    Methods = c("BoxCox", "Asinh", "Asin", "Log", "LogPlus1", "Logit", "YeoJohnson"),

    # Model args
    GridTune = FALSE,
    ExcludeAlgos = NULL,
    Trees = 50,
    MaxModelsInGrid = 10)
```
 
</p>
</details> 
  
#### The Auto_Regression() models handle a multitude of tasks. In order:
1. Convert your data to data.table format for faster processing
2. Transform your target variable using the best normalization method based on the <code>AutoTransformationCreate()</code> function
3. Create train, validation, and test data, utilizing the <code>AutoDataPartition()</code> function, if you didn't supply those directly to the function
4. Consoldate columns that are used for modeling and what metadata you want returned in your test data with predictions
5. Dichotomize categorical variables (for <code>AutoXGBoostRegression()</code>) and save the factor levels for scoring in a way that guarentees consistency across training, validation, and test data sets, utilizing the <code>DummifyDT()</code> function
6. Save the final modeling column names for reference
7. Handles the data conversion to the appropriate modeling type, such as CatBoost, H2O, and XGBoost
8. Multi-armed bandit hyperparameter tuning using randomized probability matching, if you choose to grid tune
9. Loop through the grid-tuning process, building N models
10. Collect the evaluation metrics for each grid tune run
11. Identify the best model of the set of models built in the grid tuning search
12. Save the hyperparameters from the winning grid tuned model
13. Build the final model based on the best model from the grid tuning model search (I remove each model after evaluation metrics are generated in the grid tune to avoid memory overflow)
14. Back-transform your predictions based on the best transformation used earlier in the process
15. Collect evaluation metrics based on performance on test data (based on back-transformed data)
16. Store the final predictions with the associated test data and other columns you want included in that set
17. Save your transformation metadata for recreating them in a scoring process
18. Build out and save an Evaluation Calibration Line Plot and Evaluation Calibration Box-Plot, using the <code>EvalPlot()</code> function
19. Generate and save Variable Importance
20. Generate and save Partital Dependence Calibration Line Plots and Partital Dependence Calibration Box-Plots, using the <code>ParDepPlots()</code> function
21. Return all the objects generated in a named list for immediate use and evaluation
 
</p>
</details>

#### Binary Classification:
____________________________________________________________________________________________________________________________________________
<details><summary>click to expand</summary>
<p>
  
##### **AutoCatBoostClassifier()** GPU Capable
<code>AutoCatBoostClassifier()</code> utilizes the CatBoost algorithm in the below steps

<details><summary>Code Example</summary>
<p>

```
# Create some dummy correlated data with numeric and categorical features
data <- RemixAutoML::FakeDataGenerator(Correlation = 0.85, N = 1000L, ID = 2L, ZIP = 0L, AddDate = FALSE, Classification = TRUE, MultiClass = FALSE)

# Run function
TestModel <- RemixAutoML::AutoCatBoostClassifier(

    # GPU or CPU and the number of available GPUs
    task_type = "GPU",
    NumGPUs = 1,

    # Metadata arguments:
    #   'ModelID' is used to create part of the file names generated when saving to file'
    #   'model_path' is where the minimal model objects for scoring will be stored
    #      'ModelID' will be the name of the saved model object
    #   'metadata_path' is where model evaluation and model interpretation files are saved
    #      objects saved to model_path if metadata_path is null
    #      Saved objects include:
    #         'ModelID_ValidationData.csv' is the supplied or generated TestData with predicted values
    #         'ModelID_ROC_Plot.png' and 'Model_ID_EvaluationPlot.png' calibration plot
    #         'ModelID_VariableImportance.csv' is the variable importance.
    #            This won't be saved to file if GrowPolicy is either "Depthwise" or "Lossguide" was used
    #         'ModelID_ExperimentGrid.csv' if GridTune = TRUE.
    #            Results of all model builds including parameter settings, bandit probs, and grid IDs
    #         'ModelID_EvaluationMetrics.csv' which contains all confusion matrix measures across all thresholds
    ModelID = "Test_Model_1",
    model_path = normalizePath("./"),
    metadata_path = file.path(normalizePath("./"),"R_Model_Testing"),
    SaveModelObjects = FALSE,
    ReturnModelObjects = TRUE,

    # Data arguments:
    #   'TrainOnFull' is to train a model with 100 percent of your data.
    #     That means no holdout data will be used for evaluation
    #   If ValidationData and TestData are NULL and TrainOnFull is FALSE then data will be split 70 20 10
    #   'PrimaryDateColumn' is a date column in data that is meaningful when sorted.
    #     CatBoost categorical treatment is enhanced when supplied
    #   'IDcols' are columns in your data that you don't use for modeling but get returned with ValidationData
    data = data,
    TrainOnFull = FALSE,
    ValidationData = NULL,
    TestData = NULL,
    TargetColumnName = "Adrian",
    FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
    PrimaryDateColumn = NULL,
    ClassWeights = c(1L,1L),
    IDcols = c("IDcols_1","IDcols_2"),

    # Model evaluation:
    #   'eval_metric' is the measure catboost uses when evaluting on holdout data during its bandit style process
    #   'loss_function' the loss function used in training optimization
    #   'NumOfParDepPlots' Number of partial dependence calibration plots generated.
    #     A value of 3 will return plots for the top 3 variables based on variable importance
    #     Won't be returned if GrowPolicy is either "Depthwise" or "Lossguide" is used
    #     Can run the RemixAutoML::ParDepCalPlots() with the outputted ValidationData
    eval_metric = "AUC",
    loss_function = "Logloss",
    MetricPeriods = 10L,
    NumOfParDepPlots = ncol(data)-1L-2L,

    # Grid tuning arguments:
    #   'PassInGrid' is for retraining using a previous grid winning args
    #   'MaxModelsInGrid' is a cap on the number of models that will run
    #   'MaxRunsWithoutNewWinner' number of runs without a new winner before exiting grid tuning
    #   'MaxRunMinutes' is a cap on the number of minutes that will run
    #   'Shuffles' is the number of times you want the random grid arguments shuffled
    #   'BaselineComparison' default means to compare each model build with a default built of catboost using max(Trees)
    #   'MetricPeriods' is the number of trees built before evaluting holdoutdata internally. Used in finding actual Trees used.
    PassInGrid = NULL,
    GridTune = FALSE,
    MaxModelsInGrid = 100L,
    MaxRunsWithoutNewWinner = 20L,
    MaxRunMinutes = 24L*60L,
    Shuffles = 4L,
    BaselineComparison = "default",

    # Trees, Depth, and LearningRate used in the bandit grid tuning
    # Must set Trees to a single value if you are not grid tuning
    # The ones below can be set to NULL and the values in the example will be used
    # GrowPolicy is turned off for CPU runs
    # BootStrapType utilizes Poisson only for GPU and MVS only for CPU
    Trees = seq(100L, 500L, 50L),
    Depth = seq(4L, 8L, 1L),
    LearningRate = seq(0.01,0.10,0.01),
    L2_Leaf_Reg = seq(1.0, 10.0, 1.0),
    RSM = c(0.80, 0.85, 0.90, 0.95, 1.0),
    BootStrapType = c("Bayesian", "Bernoulli", "Poisson", "MVS", "No"),
    GrowPolicy = c("SymmetricTree", "Depthwise", "Lossguide"))
```

</p>
</details>

##### **AutoXGBoostClassifier()** GPU Capable
<code>AutoXGBoostClassifier()</code> utilizes the XGBoost algorithm in the below steps

<details><summary>Code Example</summary>
<p>
 
```
# Create some dummy correlated data with numeric and categorical features
data <- RemixAutoML::FakeDataGenerator(Correlation = 0.85, N = 1000L, ID = 2L, ZIP = 0L, AddDate = FALSE, Classification = TRUE, MultiClass = FALSE)

# Run function
TestModel <- RemixAutoML::AutoXGBoostClassifier(

    # GPU or CPU
    TreeMethod = "hist",
    NThreads = 8L,

    # Metadata arguments
    model_path = normalizePath("./"),
    metadata_path = file.path(normalizePath("./"),"R_Model_Testing"),
    ModelID = "Test_Model_1",
    ReturnFactorLevels = TRUE,
    ReturnModelObjects = TRUE,
    SaveModelObjects = FALSE,

    # Data arguments
    data = data,
    TrainOnFull = FALSE,
    ValidationData = NULL,
    TestData = NULL,
    TargetColumnName = "Adrian",
    FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
    IDcols = c("IDcols_1","IDcols_2"),

    # Model evaluation
    eval_metric = "auc",
    NumOfParDepPlots = 3L,

    # Grid tuning arguments - PassInGrid is the best of GridMetrics
    PassInGrid = NULL,
    GridTune = TRUE,
    BaselineComparison = "default",
    MaxModelsInGrid = 10L,
    MaxRunsWithoutNewWinner = 20L,
    MaxRunMinutes = 24L*60L,
    Verbose = 1L,

    # Trees, Depth, and LearningRate used in the bandit grid tuning
    # Must set Trees to a single value if you are not grid tuning
    # The ones below can be set to NULL and the values in the example will be used
    Shuffles = 1L,
    Trees = seq(50L, 500L, 50L),
    eta = seq(0.05,0.40,0.05),
    max_depth = seq(4L, 16L, 2L),
    min_child_weight = seq(1.0, 10.0, 1.0),
    subsample = seq(0.55, 1.0, 0.05),
    colsample_bytree = seq(0.55, 1.0, 0.05))
```
 
</p>
</details> 

##### **AutoH2oGBMClassifier()**
<code>AutoH2oGBMClassifier()</code> utilizes the H2O Gradient Boosting algorithm in the below steps

<details><summary>Code Example</summary>
<p>
 
# Create some dummy correlated data with numeric and categorical features
data <- RemixAutoML::FakeDataGenerator(Correlation = 0.85, N = 1000L, ID = 2L, ZIP = 0L, AddDate = FALSE, Classification = TRUE, MultiClass = FALSE)

TestModel <- RemixAutoML::AutoH2oGBMClassifier(

    # Compute management
    MaxMem = "32G",
    NThreads = max(1, parallel::detectCores()-2),
    H2OShutdown = FALSE,
    IfSaveModel = "mojo",

    # Model evaluation:
    #   'eval_metric' is the measure catboost uses when evaluting on holdout data during its bandit style process
    #   'NumOfParDepPlots' Number of partial dependence calibration plots generated.
    #     A value of 3 will return plots for the top 3 variables based on variable importance
    #     Won't be returned if GrowPolicy is either "Depthwise" or "Lossguide" is used
    #     Can run the RemixAutoML::ParDepCalPlots() with the outputted ValidationData
    eval_metric = "auc",
    NumOfParDepPlots = 3L,

    # Metadata arguments:
    #   'ModelID' is used to create part of the file names generated when saving to file'
    #   'model_path' is where the minimal model objects for scoring will be stored
    #      'ModelID' will be the name of the saved model object
    #   'metadata_path' is where model evaluation and model interpretation files are saved
    #      objects saved to model_path if metadata_path is null
    #      Saved objects include:
    #         'ModelID_ValidationData.csv' is the supplied or generated TestData with predicted values
    #         'ModelID_VariableImportance.csv' is the variable importance.
    #         'ModelID_EvaluationMetrics.csv' which contains all confusion matrix metrics for all thresholds
    ModelID = "FirstModel",
    model_path = normalizePath("./"),
    metadata_path = file.path(normalizePath("./"), "MetaData"),
    ReturnModelObjects = TRUE,
    SaveModelObjects = FALSE,

    # Data arguments:
    #   'TrainOnFull' is to train a model with 100 percent of your data.
    #     That means no holdout data will be used for evaluation
    #   If ValidationData and TestData are NULL and TrainOnFull is FALSE then data will be split 70 20 10
    #   'PrimaryDateColumn' is a date column in data that is meaningful when sorted.
    #     CatBoost categorical treatment is enhanced when supplied
    #   'IDcols' are columns in your data that you don't use for modeling but get returned with ValidationData
    data,
    TrainOnFull = FALSE,
    ValidationData = NULL,
    TestData = NULL,
    TargetColumnName = "Target",
    FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],

    # Model args
    Trees = 50,
    GridTune = FALSE,
    MaxModelsInGrid = 10L)
 
</p>
</details> 

##### **AutoH2oDRFClassifier()**
<code>AutoH2oDRFClassifier()</code> utilizes the H2O Distributed Random Forest algorithm in the below steps

<details><summary>Code Example</summary>
<p>
 
```
# Create some dummy correlated data with numeric and categorical features
data <- RemixAutoML::FakeDataGenerator(Correlation = 0.85, N = 1000L, ID = 2L, ZIP = 0L, AddDate = FALSE, Classification = TRUE, MultiClass = FALSE)

TestModel <- RemixAutoML::AutoH2oDRFClassifier(

    # Compute management
    MaxMem = "32G",
    NThreads = max(1L, parallel::detectCores() - 2L),
    IfSaveModel = "mojo",
    H2OShutdown = FALSE,

    # Model evaluation:
    #   'eval_metric' is the measure catboost uses when evaluting on holdout data during its bandit style process
    #   'NumOfParDepPlots' Number of partial dependence calibration plots generated.
    #     A value of 3 will return plots for the top 3 variables based on variable importance
    #     Won't be returned if GrowPolicy is either "Depthwise" or "Lossguide" is used
    #     Can run the RemixAutoML::ParDepCalPlots() with the outputted ValidationData
    eval_metric = "auc",
    NumOfParDepPlots = 3L,

    #'     # Metadata arguments:
    #   'ModelID' is used to create part of the file names generated when saving to file'
    #   'model_path' is where the minimal model objects for scoring will be stored
    #      'ModelID' will be the name of the saved model object
    #   'metadata_path' is where model evaluation and model interpretation files are saved
    #      objects saved to model_path if metadata_path is null
    #      Saved objects include:
    #         'ModelID_ValidationData.csv' is the supplied or generated TestData with predicted values
    #         'ModelID_VariableImportance.csv' is the variable importance.
    #            This won't be saved to file if GrowPolicy is either "Depthwise" or "Lossguide" was used
    #         'ModelID_ExperimentGrid.csv' if GridTune = TRUE.
    #            Results of all model builds including parameter settings, bandit probs, and grid IDs
    #         'ModelID_EvaluationMetrics.csv' which contains MSE, MAE, MAPE, R2
    model_path = normalizePath("./"),
    metadata_path = NULL,
    ModelID = "FirstModel",
    ReturnModelObjects = TRUE,
    SaveModelObjects = FALSE,

    # Data arguments:
    #   'TrainOnFull' is to train a model with 100 percent of your data.
    #     That means no holdout data will be used for evaluation
    #   If ValidationData and TestData are NULL and TrainOnFull is FALSE then data will be split 70 20 10
    #   'PrimaryDateColumn' is a date column in data that is meaningful when sorted.
    #     CatBoost categorical treatment is enhanced when supplied
    #   'IDcols' are columns in your data that you don't use for modeling but get returned with ValidationData
    #   'TransformNumericColumns' is for transforming your target variable. Just supply the name of it
    data,
    TrainOnFull = FALSE,
    ValidationData = NULL,
    TestData = NULL,
    TargetColumnName = "Adrian",
    FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],

    # Model args
    Trees = 50L,
    GridTune = FALSE,
    MaxModelsInGrid = 10L)
```
 
</p>
</details> 

##### **AutoH2oGLMClassifier()**
<code>AutoH2oGLMClassifier()</code> utilizes the H2O generalized linear model algorithm in the below steps

<details><summary>Code Example</summary>
<p>
 
```
# Create some dummy correlated data with numeric and categorical features
data <- RemixAutoML::FakeDataGenerator(Correlation = 0.85, N = 1000L, ID = 2L, ZIP = 0L, AddDate = FALSE, Classification = TRUE, MultiClass = FALSE)

# Run function
TestModel <- RemixAutoML::AutoH2oGLMClassifier(
   data,
   TrainOnFull = FALSE,
   ValidationData = NULL,
   TestData = NULL,
   TargetColumnName = "Adrian",
   FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
   Distribution = "binomial",
   link = NULL,
   eval_metric = "auc",
   GridTune = FALSE,
   MaxMem = "32G",
   NThreads = max(1, parallel::detectCores()-2),
   MaxModelsInGrid = 10,
   model_path = NULL,
   metadata_path = NULL,
   ModelID = "FirstModel",
   NumOfParDepPlots = 3,
   ReturnModelObjects = TRUE,
   SaveModelObjects = FALSE,
   IfSaveModel = "mojo",
   H2OShutdown = FALSE,
   HurdleModel = FALSE)
```
 
</p>
</details> 

##### **AutoH2oMLClassifier()**
<code>AutoH2oMLClassifier()</code> utilizes the H2o AutoML algorithm in the below steps 

<details><summary>Code Example</summary>
<p>
 
```
# Create some dummy correlated data with numeric and categorical features
data <- RemixAutoML::FakeDataGenerator(Correlation = 0.85, N = 1000L, ID = 2L, ZIP = 0L, AddDate = FALSE, Classification = TRUE, MultiClass = FALSE)

TestModel <- RemixAutoML::AutoH2oMLClassifier(
   data,
   TrainOnFull = FALSE,
   ValidationData = NULL,
   TestData = NULL,
   TargetColumnName = "Adrian",
   FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
   ExcludeAlgos = NULL,
   eval_metric = "auc",
   Trees = 50,
   MaxMem = "32G",
   NThreads = max(1, parallel::detectCores()-2),
   MaxModelsInGrid = 10,
   model_path = normalizePath("./"),
   metadata_path = file.path(normalizePath("./"), "MetaData"),
   ModelID = "FirstModel",
   NumOfParDepPlots = 3,
   ReturnModelObjects = TRUE,
   SaveModelObjects = FALSE,
   IfSaveModel = "mojo",
   H2OShutdown = FALSE,
   HurdleModel = FALSE)
```
 
</p>
</details> 
  
#### The Auto_Classifier() models handle a multitude of tasks. In order:
1. Convert your data to data.table format for faster processing
2. Create train, validation, and test data if you didn't supply those directly to the function
3. Consoldate columns that are used for modeling and what is to be kept for data returned
4. Dichotomize categorical variables (for AutoXGBoostRegression) and save the factor levels for scoring in a way that guarentees consistency across training, validation, and test data sets
5. Saves the final column names for modeling to a csv for later reference
6. Handles the data conversion to the appropriate type, based on model type (CatBoost, H2O, and XGBoost)
7. Multi-armed bandit hyperparameter tuning using randomized probability matching, if you choose to grid tune
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
<details><summary>click to expand</summary>
<p>
  
##### **AutoCatBoostMultiClass()** GPU Capable
<code>AutoCatBoostMultiClass()</code> utilizes the CatBoost algorithm in the below steps

<details><summary>Code Example</summary>
<p>

```
# Create some dummy correlated data with numeric and categorical features
data <- RemixAutoML::FakeDataGenerator(Correlation = 0.85, N = 1000L, ID = 2L, ZIP = 0L, AddDate = FALSE, Classification = FALSE, MultiClass = TRUE)

# Run function
TestModel <- RemixAutoML::AutoCatBoostMultiClass(

    # GPU or CPU and the number of available GPUs
    task_type = "GPU",

    # Metadata arguments:
    #   'ModelID' is used to create part of the file names generated when saving to file'
    #   'model_path' is where the minimal model objects for scoring will be stored
    #      'ModelID' will be the name of the saved model object
    #   'metadata_path' is where model evaluation and model interpretation files are saved
    #      objects saved to model_path if metadata_path is null
    #      Saved objects include:
    #         'ModelID_ValidationData.csv' is the supplied or generated TestData with predicted values
    #         'ModelID_VariableImportance.csv' is the variable importance.
    #            This won't be saved to file if GrowPolicy is either "Depthwise" or "Lossguide" was used
    #         'ModelID_ExperimentGrid.csv' if GridTune = TRUE.
    #            Results of all model builds including parameter settings, bandit probs, and grid IDs
    #         'ModelID_EvaluationMetrics.csv' which contains all confusion matrix measures across all thresholds
    ModelID = "Test_Model_1",
    model_path = normalizePath("./"),
    metadata_path = file.path(normalizePath("./"),"R_Model_Testing"),
    SaveModelObjects = FALSE,
    ReturnModelObjects = TRUE,

    # Data arguments:
    #   'TrainOnFull' is to train a model with 100 percent of your data.
    #     That means no holdout data will be used for evaluation
    #   If ValidationData and TestData are NULL and TrainOnFull is FALSE then data will be split 70 20 10
    #   'PrimaryDateColumn' is a date column in data that is meaningful when sorted.
    #     CatBoost categorical treatment is enhanced when supplied
    #   'IDcols' are columns in your data that you don't use for modeling but get returned with ValidationData
    data = data,
    TrainOnFull = FALSE,
    ValidationData = NULL,
    TestData = NULL,
    TargetColumnName = "Adrian",
    FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
    PrimaryDateColumn = NULL,
    ClassWeights = c(1L,1L,1L,1L,1L),
    IDcols = c("IDcols_1","IDcols_2"),

    # Model evaluation:
    #   'eval_metric' is the measure catboost uses when evaluting on holdout data during its bandit style process
    #   'loss_function' the loss function used in training optimization
    eval_metric = "MultiClass",
    grid_eval_metric = "Accuracy",
    MetricPeriods = 10L,

    # Grid tuning arguments:
    #   'PassInGrid' is for retraining using a previous grid winning args
    #   'MaxModelsInGrid' is a cap on the number of models that will run
    #   'MaxRunsWithoutNewWinner' number of runs without a new winner before exiting grid tuning
    #   'MaxRunMinutes' is a cap on the number of minutes that will run
    #   'Shuffles' is the number of times you want the random grid arguments shuffled
    #   'BaselineComparison' default means to compare each model build with a default built of catboost using max(Trees)
    #   'MetricPeriods' is the number of trees built before evaluting holdoutdata internally. Used in finding actual Trees used.
    PassInGrid = NULL,
    GridTune = FALSE,
    MaxModelsInGrid = 100L,
    MaxRunsWithoutNewWinner = 20L,
    MaxRunMinutes = 24L*60L,
    Shuffles = 4L,
    BaselineComparison = "default",

    # Trees, Depth, and LearningRate used in the bandit grid tuning
    # Must set Trees to a single value if you are not grid tuning
    # The ones below can be set to NULL and the values in the example will be used
    # GrowPolicy is turned off for CPU runs
    # BootStrapType utilizes Poisson only for GPU and MVS only for CPU
    Trees = seq(100L, 500L, 50L),
    Depth = seq(4L, 8L, 1L),
    LearningRate = seq(0.01,0.10,0.01),
    L2_Leaf_Reg = seq(1.0, 10.0, 1.0),
    RSM = c(0.80, 0.85, 0.90, 0.95, 1.0),
    BootStrapType = c("Bayesian", "Bernoulli", "Poisson", "MVS", "No"),
    GrowPolicy = c("SymmetricTree", "Depthwise", "Lossguide"))
```

</p>
</details>

##### **AutoXGBoostMultiClass()** GPU Capable
<code>AutoXGBoostMultiClass()</code> utilizes the XGBoost algorithm in the below steps

<details><summary>Code Example</summary>
<p>

```
# Create some dummy correlated data with numeric and categorical features
data <- RemixAutoML::FakeDataGenerator(Correlation = 0.85, N = 1000L, ID = 2L, ZIP = 0L, AddDate = FALSE, Classification = FALSE, MultiClass = TRUE)

# Run function
TestModel <- RemixAutoML::AutoXGBoostMultiClass(

    # GPU or CPU
    TreeMethod = "hist",
    NThreads = 8L,

    # Metadata arguments
    model_path = normalizePath("./"),
    metadata_path = file.path(normalizePath("./"),"R_Model_Testing"),
    ModelID = "Test_Model_1",
    ReturnFactorLevels = TRUE,
    ReturnModelObjects = TRUE,
    SaveModelObjects = FALSE,

    # Data arguments
    data = data,
    TrainOnFull = FALSE,
    ValidationData = NULL,
    TestData = NULL,
    TargetColumnName = "Adrian",
    FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
    IDcols = c("IDcols_1","IDcols_2"),

    # Model evaluation
    eval_metric = "auc",
    Objective = 'multi:softmax',
    grid_eval_metric = "accuracy",
    NumOfParDepPlots = 3L,

    # Grid tuning arguments - PassInGrid is the best of GridMetrics
    PassInGrid = NULL,
    GridTune = TRUE,
    BaselineComparison = "default",
    MaxModelsInGrid = 10L,
    MaxRunsWithoutNewWinner = 20L,
    MaxRunMinutes = 24L*60L,
    Verbose = 1L,

    # Trees, Depth, and LearningRate used in the bandit grid tuning
    # Must set Trees to a single value if you are not grid tuning
    # The ones below can be set to NULL and the values in the example will be used
    Shuffles = 1L,
    Trees = seq(50L, 500L, 50L),
    eta = seq(0.05,0.40,0.05),
    max_depth = seq(4L, 16L, 2L),
    min_child_weight = seq(1.0, 10.0, 1.0),
    subsample = seq(0.55, 1.0, 0.05),
    colsample_bytree = seq(0.55, 1.0, 0.05))
```

</p>
</details>

##### **AutoH2oGBMMultiClass()**
<code>AutoH2oGBMMultiClass()</code> utilizes the H2O Gradient Boosting algorithm in the below steps

<details><summary>Code Example</summary>
<p>

```
# Create some dummy correlated data with numeric and categorical features
data <- RemixAutoML::FakeDataGenerator(Correlation = 0.85, N = 1000, ID = 2, ZIP = 0, AddDate = FALSE, Classification = FALSE, MultiClass = TRUE)

# Run function
TestModel <- RemixAutoML::AutoH2oGBMMultiClass(
   data,
   TrainOnFull = FALSE,
   ValidationData = NULL,
   TestData = NULL,
   TargetColumnName = "Target",
   FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
   eval_metric = "logloss",
   Trees = 50,
   GridTune = FALSE,
   MaxMem = "32G",
   NThreads = max(1, parallel::detectCores()-2),
   MaxModelsInGrid = 10,
   model_path = normalizePath("./"),
   metadata_path = file.path(normalizePath("./"), "MetaData"),
   ModelID = "FirstModel",
   ReturnModelObjects = TRUE,
   SaveModelObjects = FALSE,
   IfSaveModel = "mojo",
   H2OShutdown = FALSE,
   HurdleModel = FALSE)
```

</p>
</details>

##### **AutoH2oDRFMultiClass()**
<code>AutoH2oDRFMultiClass()</code> utilizes the H2O Distributed Random Forest algorithm in the below steps

<details><summary>Code Example</summary>
<p>

```
# Create some dummy correlated data with numeric and categorical features
data <- RemixAutoML::FakeDataGenerator(Correlation = 0.85, N = 1000L, ID = 2L, ZIP = 0L, AddDate = FALSE, Classification = FALSE, MultiClass = TRUE)

# Run function
TestModel <- RemixAutoML::AutoH2oDRFMultiClass(
   data,
   TrainOnFull = FALSE,
   ValidationData = NULL,
   TestData = NULL,
   TargetColumnName = "Adrian",
   FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
   eval_metric = "logloss",
   Trees = 50,
   GridTune = FALSE,
   MaxMem = "32G",
   NThreads = max(1, parallel::detectCores()-2),
   MaxModelsInGrid = 10,
   model_path = normalizePath("./"),
   metadata_path = file.path(normalizePath("./"), "MetaData"),
   ModelID = "FirstModel",
   ReturnModelObjects = TRUE,
   SaveModelObjects = FALSE,
   IfSaveModel = "mojo",
   H2OShutdown = FALSE,
   HurdleModel = FALSE)
```

</p>
</details>

##### **AutoH2oGLMMultiClass()**
<code>AutoH2oGLMMultiClass()</code> utilizes the H2O generalized linear model algorithm in the below steps

<details><summary>Code Example</summary>
<p>

```
# Create some dummy correlated data with numeric and categorical features
data <- RemixAutoML::FakeDataGenerator(Correlation = 0.85, N = 1000L, ID = 2L, ZIP = 0L, AddDate = FALSE, Classification = FALSE, MultiClass = TRUE)

# Run function
TestModel <- RemixAutoML::AutoH2oGLMMultiClass(
   data,
   TrainOnFull = FALSE,
   ValidationData = NULL,
   TestData = NULL,
   TargetColumnName = "Adrian",
   FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
   eval_metric = "logloss",
   GridTune = FALSE,
   MaxMem = "32G",
   NThreads = max(1, parallel::detectCores()-2),
   MaxModelsInGrid = 10,
   model_path = normalizePath("./"),
   metadata_path = file.path(normalizePath("./"), "MetaData"),
   ModelID = "FirstModel",
   ReturnModelObjects = TRUE,
   SaveModelObjects = FALSE,
   IfSaveModel = "mojo",
   H2OShutdown = FALSE,
   HurdleModel = FALSE)
```

</p>
</details>

##### **AutoH2oMLMultiClass()**
<code>AutoH2oMLMultiClass()</code> utilizes the H2o AutoML algorithm in the below steps

<details><summary>Code Example</summary>
<p>

```
# Create some dummy correlated data with numeric and categorical features
data <- RemixAutoML::FakeDataGenerator(Correlation = 0.85, N = 1000, ID = 2, ZIP = 0, AddDate = FALSE, Classification = FALSE, MultiClass = TRUE)

# Run function
TestModel <- RemixAutoML::AutoH2oMLMultiClass(
   data,
   TrainOnFull = FALSE,
   ValidationData = NULL,
   TestData = NULL,
   TargetColumnName = "Adrian",
   FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
   ExcludeAlgos = NULL,
   eval_metric = "logloss",
   Trees = 50,
   MaxMem = "32G",
   NThreads = max(1, parallel::detectCores()-2),
   MaxModelsInGrid = 10,
   model_path = normalizePath("./"),
   metadata_path = file.path(normalizePath("./"), "MetaData"),
   ModelID = "FirstModel",
   ReturnModelObjects = TRUE,
   SaveModelObjects = FALSE,
   IfSaveModel = "mojo",
   H2OShutdown = FALSE,
   HurdleModel = FALSE)
```

</p>
</details>
  
#### The Auto_MultiClass() models handle a multitude of tasks. In order:
1. Convert your data to data.table format for faster processing
2. Create train, validation, and test data if you didn't supply those directly to the function
3. Consoldate columns that are used for modeling and what is to be kept for data returned
4. Dichotomize categorical variables (for AutoXGBoostRegression) and save the factor levels for scoring in a way that guarentees consistency across training, validation, and test data sets
5. Saves the final column names for modeling to a csv for later reference
6. Ensures the target levels are consistent across train, validate, and test sets and save the levels to file
7. Handles the data conversion to the appropriate type, based on model type (CatBoost, H2O, and XGBoost)
8. Multi-armed bandit hyperparameter tuning using randomized probability matching, if you choose to grid tune
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
<details><summary>click to expand</summary>
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
<details><summary>click to expand</summary>
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
<details><summary>click to expand</summary>
<p>
  
##### **AutoNLS()**

<img src="Images/AutoNLS_Image.png" align="center" width="400" />

<code>AutoNLS()</code> is an automated nonlinear regression modeling function. This function automatically finds the best model fit from the set of models listed below and merges predictions to source data file. Great for forecasting growth (extrapolation) when domain knowledge can guide model selection.
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

## Automated Model Scoring Functions: <img src="Images/ModelScoringImage.png" align="right" width="80" />
<details><summary>Expand to view content</summary>
<p>
  
<img src="Images/ScoringCatBoost.png" align="center" width="400" />

##### **AutoCatBoostScoring()**
<code>AutoCatBoostScoring()</code> is an automated scoring function that compliments the AutoCatBoost() model training functions. This function requires you to supply features for scoring. It will run ModelDataPrep() to prepare your features for catboost data conversion and scoring. It will also handle and transformations and back-transformations if you utilized that feature in the regression training case.

##### **AutoXGBoostScoring()**
<code>AutoXGBoostScoring()</code> is an automated scoring function that compliments the AutoXGBoost() model training functions. This function requires you to supply features for scoring. It will run ModelDataPrep() and the DummifyDT() functions to prepare your features for xgboost data conversion and scoring. It will also handle and transformations and back-transformations if you utilized that feature in the regression training case.

##### **AutoH2OMLScoring()**
<code>AutoH2OMLScoring()</code> is an automated scoring function that compliments the AutoH2oGBM__() and AutoH2oDRF__() model training functions. This function requires you to supply features for scoring. It will run ModelDataPrep()to prepare your features for H2O data conversion and scoring. It will also handle transformations and back-transformations if you utilized that feature in the regression training case and didn't do it yourself before hand.

##### **AutoHurdleScoring()**
<code>AutoHurdleScoring()</code> will score the AutoCatBoostHurdleModel() function currently. Functionality for XGBoost hurdle models will be next, followed by the H2O version.

##### **AutoH2OScoring()**
<code>AutoH2OScoring()</code> is for scoring models that were built with the AutoH2OModeler, AutoKMeans, and AutoWord2VecModeler functions. Scores mojo models or binary files by loading models into the H2O environment and scoring them. You can choose which output you wish to keep as well for classification and multinomial models. 
  
</p>
</details>

## Automated Model Evaluation Functions: <img src="Images/ModelEvaluationImage.png" align="right" width="80" />
<details><summary>Expand to view content</summary>
<p>
  
##### **RemixClassificationMetrics()**
<code>RemixClassificationMetrics()</code> will return all confusion matrix metrics across all possible thresholds (seq(0.01,0.99,0.01) for any Remix Auto_Classification() model. Cost sensitive thresholds are also returned.

##### **ParDepCalPlots()**
<code>ParDepCalPlots()</code> is for visualizing the relationships of features and the reliability of the model in predicting those effects. Build a partial dependence calibration line plot, box plot or bar plot for the case of categorical variables.

<img src="Images/AutoCatBoostRegressionParDepMultiPlot.png" align="center" width="400" />

![ParDepCalPlots Blog](https://www.remixinstitute.com/blog/companies-are-demanding-model-interpretability-heres-how-to-do-it-right/#.XUIN1HtlCDM)

##### **EvalPlot()**
<code>EvalPlot()</code> Has two plot versions: calibration line plot of predicted values and actual values across range of predicted value, and calibration boxplot for seeing the accuracy and variability of predictions against actuals. 

<img src="Images/AutoCatBoostRegressionEvaluationMultiPlot.png" align="center" width="400" />

##### **threshOptim()**
<code>threshOptim()</code> is great for situations with asymmetric costs across the confusion matrix. Generate a cost-sensitive optimized threshold for classification models. Just supply the costs for false positives and false negatives (can supply costs for all four outcomes too) and the function will return the optimal threshold for maximizing "utility". 

##### **RedYellowGreen()**
<code>RedYellowGreen()</code> computes optimal thresholds for binary classification models where "don't classify" is an option. Consider a health care binary classification model that predicts whether or not a disease is present. This is certainly a case for threshOptim since the costs of false positives and false negatives can vary by a large margin. However, there is always the potential to run further analysis. The RedYellowGreen() function can compute two thresholds if you can supply a cost of "further analysis". Predicted values < the lower threshold are confidently classified as a negative case and predicted values > the upper threshold are confidently classified as a postive case. Predicted values in between the lower and upper thresholds are cases that should require further analysis.

<img src="Images/RedYellowGreenPic.png" align="center" width="400" />

![RedYellowGreen Blog](https://www.remixinstitute.com/blog/the-easiest-way-to-create-thresholds-and-improve-your-classification-model/#.XUINVntlCDM)

</p>
</details>

## Automated Time Series Modeling Functions: <img src="Images/AutoCARMA2.png" align="right" width="80" />
<details><summary>Expand to view content</summary>
<p>
  
<img src="Images/AutoTSPlot.png" align="center" width="300" />

### **AutoBanditSarima()**
<code>AutoBanditSarima()</code> is the newest weapon in the time series arsenal. This is the highest performing single series time series model in the package. The entire arima parameter space is divided up into blocks that are increasing in complexity of parameter settings. The multi-armed bandit will determine which parameter block to sample from more frequently based on which one is performing better than the others. The underlying bandit algorithm is the randomized probability matching algorithm found in the **bandit** package. I had to write a slight variation of it to allow for tweaking the number of intervals used in computing the integrals that result in the probabilities used for sampling. The evaluation is different from what exists today - you need to specify a weighting to use so that both the training metrics and validation metrics are used in calculating the best model. The user can specify 0% or 100% to go with just the one measure of their choice as well. The function returns a list with data.table of the forecasts and prediction inverals and the other item in the list is the Performance Grid results so you can see how every model tested performed.

```
# Pull in data
data <- data.table::as.data.table(fpp::cafe)
data.table::setnames(data, "x", "Weekly_Sales")
data.table::set(data, j = "Date", value = "1982-01-01")
data.table::setcolorder(data, c(2,1))
data[, Date := as.POSIXct(Date)]

# "1min"
data[, xx := 1:.N][, Date := Date + lubridate::minutes(1 * xx)][, xx := NULL]

# "5min"
#data[, xx := 1:.N][, Date := Date + lubridate::minutes(5 * xx)][, xx := NULL]

# "10min"
#data[, xx := 1:.N][, Date := Date + lubridate::minutes(10 * xx)][, xx := NULL]

# "15min"
#data[, xx := 1:.N][, Date := Date + lubridate::minutes(15 * xx)][, xx := NULL]

# "30min"
#data[, xx := 1:.N][, Date := Date + lubridate::minutes(30 * xx)][, xx := NULL]

# "hour"
#data[, xx := 1:.N][, Date := Date + lubridate::hours(xx)][, xx := NULL]

# Build model
Output <- RemixAutoML::AutoBanditSarima(
  data = data,
  TargetVariableName = "Weekly_Sales",
  DateColumnName = "Date",
  TimeAggLevel = "1min",
  EvaluationMetric = "MAE",
  NumHoldOutPeriods = 5L,
  NumFCPeriods = 5L,
  MaxLags = 5L,
  MaxSeasonalLags = 0L,
  MaxMovingAverages = 5L, 
  MaxSeasonalMovingAverages = 0L,
  MaxFourierPairs = 2L,
  TrainWeighting = 0.50,
  MaxConsecutiveFails = 50L,
  MaxNumberModels = 500L,
  MaxRunTimeMinutes = 30L)

# View output
Output$Forecast[ModelRank == min(ModelRank)]
View(Output$PerformanceGrid[DataSetName == "TSCleanModelFrequency"])
```

### **AutoBanditNNet()**
Same as AutoBanditArima except it uses the nnetar model behind the scenes.

### **AutoTBATS()**
Same as AutoBanditArima except for the bandit testing and it uses TBATS behind the scenes. It just runs through all the parameter settings and builds each model and returns the same list as the other two above.

### **AutoTS()** <img src="Images/AutoTS.png" align="right" width="300" />
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

* You can specify MaxFourierPairs to test out if adding Fourier term regressors can increase forecast accuracy. The Fourier terms will be applied to the ARIMA and NNetar models only.
* For the ARIMA, ARFIMA, and TBATS, any number of lags and moving averages along with up to 1 seasonal lags and seasonal moving averages can be used (selection based on a stepwise procedure)
* For the Double Seasonal Holt-Winters model, alpha, beta, gamma, omega, and phi are determined using least-squares and the forecasts are adjusted using an AR(1) model for the errors
* The Exponential Smoothing State-Space model runs through an automatic selection of the error type, trend type, and season type, with the options being "none", "additive", and "multiplicative", along with testing of damped vs. non-damped trend (either additive or multiplicative), and alpha, beta, and phi are estimated
* The neural network is setup to test out every combination of lags and seasonal lags and the model with the best holdout score is selected
* The TBATS model utilizes any number of lags and moving averages for the errors, damped trend vs. non-damped trend are tested, trend vs. non-trend are also tested, and the model utilizes parallel processing for efficient run times
* The TSLM model utilizes a simple time trend and season depending on the frequency of the data

### The **CARMA** Suite <img src="Images/AutoCARMA2.png" align="right" width="300" />
<code>AutoTS()</code>

##### **AutoCatBoostCARMA()**
<code>AutoCatBoostCARMA()</code> utilizes the CatBoost alorithm

##### **AutoXGBoostCARMA()**
<code>AutoXGBoostCARMA()</code> utilizes the XGBoost alorithm

##### **AutoH2oDRFCARMA()**
<code>AutoH2oDRFCARMA()</code> utilizes the H2O Distributed Random Forest alorithm

##### **AutoH2oGBMCARMA()**
<code>AutoH2oGBMCARMA()</code> utilizes the H2O Gradient Boosting Machine alorithm

##### **AutoH2oGLMCARMA()**
<code>AutoH2oGLMCARMA()</code> utilizes the H2O generalized linear model alorithm

##### **AutoH2oMLCARMA()**
<code>AutoH2oMLCARMA()</code> utilizes the H2O AutoML alorithm


<img src="Images/Catboost30.png" align="center" width="300" />


##### The CARMA suite utilizes several features to ensure proper models are built to generate the best possible out-of-sample forecasts.

**Feature engineering:** I use a time trend, calendar variables, holiday counts, differencing, along with fourier pairs and lags / rolling statistics (mean, sd, skewness, kurtosis, quantiles) and they can be generated by categorical variables and their interactions plus for various time-based aggregations. Internally, the CARMA functions utilize several RemixAutoML functions, all written using data.table for fast and memory efficient processing: 
  * <code>AutoLagRollStats()</code> - creates lags and moving average features (also creates lags and moving averages off of time between records)
  * <code>AutoLagRollStatsScoring()</code> - creates lags and moving average features for a single record (along with the time between vars)
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
  
### Intermittent Demand Forecasting Functions

##### **TimeSeriesFill()**
<code>TimeSeriesFill()</code> is a function that will zero pad (currently only zero pad) a time series data set (not transactional data). There are three ways to use this function:
* Grouped data 1 - find the minimum and maximum dates regardless of grouping variables and use those values to ensure all group levels have all the dates represented within the series bounds (if missing, fill with zeros)
* Grouped data 2 - find the minimum and maximum dates with respect to each unique grouping variable level (grouping variables must be hierarchical) and zero pads missing dates within in each group level.
* Single series - Zero pad any missing dates within series bounds
* Used internally with the CARMA suite of functions by specifying the argument to enable this functionality

##### **ContinuousTimeDataGenerator()**
<code>ContinuousTimeDataGenerator()</code> is for frequency and size data sets. This function generates count and size data sets for intermittent demand forecasting, using the methods in this package.

##### **AutoCatBoostSizeFreqDist()**
<code>AutoCatBoostSizeFreqDist()</code> is for building size and frequency predictive distributions via quantile regressions. Size (or severity) and frequency (or count) quantile regressions are build and you supply the actual percentiles you want predicted. Use this with the <code>ID_SingleLevelGibbsSampler()</code> function to simulate from the joint distribution.

##### **AutoH2oGBMSizeFreqDist()**
<code>AutoH2oGBMSizeFreqDist()</code> is for building size and frequency predictive distributions via quantile regressions. Size (or severity) and frequency (or count) quantile regressions are build and you supply the actual percentiles you want predicted. Use this with the <code>ID_SingleLevelGibbsSampler()</code> function to simulate from the joint distribution.

##### **AutoCatBoostFreqSizeScoring()**
<code>AutoCatBoostFreqSizeScoring()</code> is for scoring the models build with <code>AutoCatBoostFreqSizeScoring()</code>. It will return the predicted values for every quantile model for both distributions for 1 to the max forecast periods you provided to build the scoring data. 

##### **AutoH2oGBMFreqSizeScoring()**
<code>AutoH2oGBMFreqSizeScoring()</code> is for scoring the models build with <code>AutoH2oGBMSizeFreqDist()</code>. It will return the predicted values for every quantile model for both distributions for 1 to the max forecast periods you provided to build the scoring data. 

</p>
</details>

## Automated Recommender System Functions: <img src="Images/RecommenderSystemImage2.png" align="right" width="80" />
<details><summary>Expand to view content</summary>
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

## Automated Unsupervised Learning Functions: <img src="Images/UnsupervisedLearningImage.png" align="right" width="80" />
<details><summary>Expand to view content</summary>
<p>
 
##### **H2oAutoencoder()**
<code>H2oAutoencoder()</code> Use for dimension reduction and anomaly detection

##### **H2oIsolationForest()**
<code>H2oIsolationForest()</code> automatically identifies anomalous data records via Isolation Forests from H2O.

##### **AutoKMeans()** 
<code>AutoKMeans()</code> This function builds a generalized low rank model followed by KMeans. (Possible cross with Feature Engineering) Generate a column with a cluster identifier based on a grid tuned (optional) generalized low rank model and a grid tuned (optimal) K-Optimal searching K-Means algorithm

##### **ResidualOutliers()**
<code>ResidualOutliers()</code> Generate residual outliers from time series modeling. (Cross with Feature Engineering) Utilize tsoutliers to indicate outliers within a time series data set

##### **GenTSAnomVars()**
<code>GenTSAnomVars()</code> generates time series anomaly variables. (Cross with Feature Engineering) Create indicator variables (high, low) along with cumulative anomaly rates (high, low) based on control limits methodology over a max of two grouping variables and a date variable (effectively a rolling GLM).

<img src="Images/ResidualOutliers_MultiplotImage.png" align="center" width="400" />

</p>
</details>

## Utilities, EDA, and Misc. Functions: <img src="Images/MiscImage.png" align="right" width="80" />
<details><summary>Expand to view content</summary>
<p>
 
##### **AutoWordFreq()** 
<code>AutoWordFreq()</code> creates a word frequency data.table and a word cloud

<img src="Images/AutoWordFreq_WordCloudImage.png" align="center" width="400" />

##### **AutoH2OTextPrepScoring()** 
<code>AutoH2OTextPrepScoring()</code> prepares your data for scoring based on models built with AutoWord2VecModel and runs internally inside the AutoH2OScoring() function. It cleans and tokenizes your text data.

##### **ProblematicFeatures()**
<code>ProblematicFeatures()</code> identifies columns that have either little to no variance, categorical variables with extremely high cardinality, too many NA's, too many zeros, or too high of a skew.

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

