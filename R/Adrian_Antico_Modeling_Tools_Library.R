#' DummifyDT creates dummy variables for the selected columns
#'
#' @author Adrian Antico
#' @param data the data set to run the micro auc on
#' @param cols a vector with the names of the columns you wish to dichotomize
#' @param KeepBaseCols set to TRUE to keep the original columns used in the dichotomization process
#' @param OneHot Set to TRUE to run one hot encoding, FALSE to generate N columns for N levels
#' @examples
#' library(data.table)
#' library(AdrianModelingTools)
#' test <- data.table(Value = runif(100000),
#'                    FactorCol = sample(x = c(letters,
#'                                             LETTERS,
#'                                             paste0(letters,letters),
#'                                             paste0(LETTERS,LETTERS),
#'                                             paste0(letters,LETTERS),
#'                                             paste0(LETTERS,letters)),
#'                                       size = 100000,
#'                                       replace = TRUE))
#' test <- DummifyDT(data = test,
#'                   cols = "FactorCol",
#'                   KeepBaseCols = FALSE)
#' ncol(test)
#' test[, sum(FactorCol_gg)]
#' @return data table with new dummy variables columns and optionally removes base columns
#' @export
# Dummify meters
DummifyDT <- function(data,
                      cols,
                      KeepBaseCols = TRUE,
                      OneHot = TRUE) {
  library(data.table)
  if(!is.data.table(data)) data <- as.data.table(data)
  for (col in cols) {
    inds <- unique(data[[eval(col)]])
    alloc.col(data, ncol(data) + length(inds))
    set(data, j = paste0(col,"_",inds), value = 0L)
    data[, eval(col) := as.character(get(col))]
    for (ind in inds) {
      data.table::set(data,
                      i = which(data.table::chmatch(data[[col]],ind) == 1L),
                      j = paste0(col, "_",ind),
                      value = 1L)
    }
    if(OneHot) {
      data[, paste0(col,"_Base") := 0]
    }
    if(!KeepBaseCols) data[, eval(col) := NULL]
  }
  return(data)
}

#' H20MultinomialAUC computes the micro auc from a multinomial model
#'
#' @author Adrian Antico
#' @param validate the data set to run the micro auc on
#' @param best_model the model object you wish to test
#' @param targetColNum the column number of the target variable
#' @param targetName the name, in quotes, of the target variable
#' @examples
#' auc_val <- H20MultinomialAUC(validate, best_model, targetColNum = 1, targetName = "TargetVar")
#' @return Micro AUC
H20MultinomialAUC <- function(validate, best_model, targetColNum = 1, targetName = "TargetVar") {
  xx <- as.data.table(h2o.cbind(validate[,targetColNum],h2o.predict(best_model, newdata = validate)))
  xx[, predict := as.character(predict)]
  xx[, vals := 0.5]
  z <- ncol(xx)
  col <- targetName
  for (l in 1:nrow(xx)) {
    cols <- xx[l, get(col)][[1]]
    valss <- xx[l, ..cols][[1]]
    set(xx, l, j = z, value = valss)
  }
  return(round(as.numeric(noquote(stringr::str_extract(pROC::multiclass.roc(xx$target, xx$vals)$auc, "\\d+\\.*\\d*"))),4))
}

#' NumWeekdays is a vectorized function to count up the number of weekdays in a range of dates
#'
#' @author Adrian Antico
#' @param a The start date
#' @param b The end date
#' @examples
#' data[, Weekdaycounts := NumWeekdays(Date1, Date2)]
#' @return The counts either as a scalar or a column in your table
#' @export
NumWeekdays <- Vectorize(function(a, b) {
  sum(!weekdays(seq(a, b, "days")) %in% c("Saturday", "Sunday"))
})

#' HolidayCounts is a vectorized function to count up the number of holidays in a range of dates
#'
#' @author Adrian Antico
#' @param a The start date
#' @param b The end date
#' @examples
#' data[, holidays := HolidayCounts(Date1, Date2)]
#' @return The counts either as a scalar or a column in your table
#' @export
HolidayCounts <- Vectorize(function(a,b) {
  d <- seq(a, b, 1)
  sum(chron::is.holiday(d))
})

#' PrintObjectsSize prints out the top N objects and their associated sizes, sorted by size
#'
#' @author Adrian Antico
#' @param N The number of objects to display
#' @examples
#' PrintObjectsSize(N = 10)
#' @return The objects in your environment and their sizes
#' @export
PrintObjectsSize <- function(N = 10) {
  print(sort(-sapply(ls(),function(x){object.size(get(x))}))[1:N]/1024/1024)
}

#' CountSingleDigits counts the number of digits in a string
#'
#' @author Adrian Antico at RemixInstitute.com
#' @param data the source data.table
#' @param col the column number with the string to evaluate
#' @examples
#' VIMP[, variable2 := ifelse(CountSingleDigits(VIMP,1) == 2, substr(variable, 1,7), substr(variable, 1,6))]
#' @return The original data.table with the added columns merged in
#' @export
CountSingleDigits <- function(data, col) {
  unlist(lapply(substr(data[[col]],1,3), function(x) {
    nchar(gsub("[^0-9]+", "", x))
  }))
}

#' GenTSAnomVars is an automated z-score anomaly detection via GLM-like procedure
#'
#' GenTSAnomVars is an automated z-score anomaly detection via GLM-like procedure. Data is z-scaled and grouped by factors and time periods to determine which points are above and below the control limits in a cumulative time fashion. Then a cumulative rate is created as the final variable. Set KeepAllCols to FALSE to utilize the intermediate features to create rolling stats from them.
#'
#' @author Adrian Antico
#' @param data the source residuals data.table
#' @param ValueCol the numeric column to run anomaly detection over
#' @param GroupVar1 this is a group by variable
#' @param GroupVar2 this is another group by variable
#' @param DateVar this is a time variable for grouping
#' @param High this is the threshold on the high end
#' @param Low this is the threshold on the low end
#' @param KeepAllCols set to TRUE to remove the intermediate features
#' @param DataScaled set to TRUE if you already scaled your data
#' @examples
#' # Create data
#' data <- data.table(DateTime = as.Date(Sys.time()), Target = stats::filter(rnorm(10000,mean = 50, sd = 20), filter=rep(1,10), circular=TRUE))
#' data[, temp := seq(1:10000)][, DateTime := DateTime - temp][, temp := NULL]
#' data <- data[order(DateTime)]
#' x <- as.data.table(sde::GBM(N=10000)*1000)
#' data[, predicted := x[-1,]]
#' stuff    <- GenTSAnomVars(data,
#'                           ValueCol    = "Target",
#'                           GroupVar1   = NULL,
#'                           GroupVar2   = NULL,
#'                           DateVar     = "DateTime",
#'                           High        = 1.96,
#'                           Low         = -1.96,
#'                           KeepAllCols = TRUE,
#'                           DataScaled  = FALSE)
#' @return The original data.table with the added columns merged in
#' @export
GenTSAnomVars <- function(data,
                          ValueCol    = "Value",
                          GroupVar1   = "SKU",
                          GroupVar2   = NULL,
                          DateVar     = "DATE",
                          High        = 1.96,
                          Low         = -1.96,
                          KeepAllCols = FALSE,
                          DataScaled  = TRUE) {
  
  # Load data.table
  library(data.table)
  if(!is.data.table(data)) data <- as.data.table(data)

  # Scale data if not already
  if(!DataScaled) {
    data[, eval(ValueCol) := scale(get(ValueCol), center = TRUE, scale = TRUE)]
  }

  # Global check for date
  if(!is.null(DateVar)) {
    if(is.null(GroupVar1) & is.null(GroupVar2)) {
      data <- data[order(get(DateVar))]
      data[, RowNumAsc := 1:.N]
      data[, AnomHigh := as.numeric(ifelse(get(ValueCol) > High, 1, 0))]
      data[, AnomLow := as.numeric(ifelse(get(ValueCol) < Low, 1, 0))]
      data[, CumAnomHigh := cumsum(AnomHigh)]
      data[, CumAnomLow := cumsum(AnomLow)]
      data[, AnomHighRate := CumAnomHigh / RowNumAsc]
      data[, AnomLowRate := CumAnomLow / RowNumAsc]
      if(!KeepAllCols) {
        data[, ':=' (AnomHigh = NULL,
                     AnomLow = NULL,
                     CumAnomHigh = NULL,
                     CumAnomLow = NULL,
                     RowNumAsc = NULL)]
      }
    } else if(is.null(GroupVar2) & !is.null(GroupVar1)) {
      data <- data[order(get(GroupVar1), get(DateVar))]
      data[, RowNumAsc := 1:.N, by = get(GroupVar1)]
      data[, AnomHigh := as.numeric(ifelse(get(ValueCol) > High, 1, 0))]
      data[, AnomLow := as.numeric(ifelse(get(ValueCol) < Low, 1, 0))]
      data[, CumAnomHigh := cumsum(AnomHigh), by = get(GroupVar1)]
      data[, CumAnomLow := cumsum(AnomLow), by = get(GroupVar1)]
      data[, AnomHighRate := CumAnomHigh / RowNumAsc]
      data[, AnomLowRate := CumAnomLow / RowNumAsc]
      if(!KeepAllCols) {
        data[, ':=' (AnomHigh = NULL,
                     AnomLow = NULL,
                     CumAnomHigh = NULL,
                     CumAnomLow = NULL,
                     RowNumAsc = NULL)]
      }
    } else if (!is.null(GroupVar1) & !is.null(GroupVar2)) {
      data <- data[order(get(GroupVar1), get(GroupVar2), get(DateVar))]
      data[, RowNumAsc := 1:.N, by = list(get(GroupVar1), get(GroupVar2))]
      data[, AnomHigh := as.numeric(ifelse(get(ValueCol) > High, 1, 0))]
      data[, AnomLow := as.numeric(ifelse(get(ValueCol) < Low, 1, 0))]
      data[, CumAnomHigh := cumsum(AnomHigh), by = list(get(GroupVar1), get(GroupVar2))]
      data[, CumAnomLow := cumsum(AnomLow), by = list(get(GroupVar1), get(GroupVar2))]
      data[, paste0(GroupVar2, "AnomHighRate") := CumAnomHigh / RowNumAsc]
      data[, paste0(GroupVar2, "AnomLowRate") := CumAnomLow / RowNumAsc]
      if(!KeepAllCols) {
        data[, ':=' (AnomHigh = NULL,
                     AnomLow = NULL,
                     CumAnomHigh = NULL,
                     CumAnomLow = NULL,
                     RowNumAsc = NULL)]
      }
    }
    return(data)
  }
  return(NULL)
}

#' ResidualOutliers is an automated time series outlier detection function
#'
#' ResidualOutliers is an automated time series outlier detection function that utilizes tsoutliers and auto.arima.
#'
#' @author Adrian Antico
#' @param data the source residuals data.table
#' @param maxN the largest lag or moving average (seasonal too) values for the arima fit
#' @param cvar the t-stat value for tsoutliers
#' @examples
#' data <- data.table(a = seq(0,10000,1), predicted = sde::GBM(N=10000)*1000)[-1,]
#' data <- data.table(a = seq(1,10000,1), predicted = sde::rcCIR(n=10000, Dt=0.1, x0=1, theta=c(6,2,2)))
#' data <- data.table(a = seq(1,10000,1), predicted = sde::rsOU(n=10000, theta=c(0,2,1)))
#' stuff    <- ResidualOutliers(data = data, maxN = 5, cvar = 4)
#' data     <- stuff[[1]]
#' model    <- stuff[[2]]
#' resid    <- stuff[[3]]
#' outliers <- data[type != "<NA>"]
#' @return A data.table with outliers, the arima model, and residuals from the arima fit
#' @export
ResidualOutliers <- function(data, maxN = 5, cvar = 4) {

  # Load libraries
  library(data.table)
  library(forecast)
  library(tsoutliers)
  
  # Convert to time series object
  tsData <- ts(data, frequency = 1, start = 1, end = nrow(data))

  # Build the auto arimia
  fit <- forecast::auto.arima(
    tsData[,2],
    max.p   = maxN,
    max.q   = maxN,
    max.P   = maxN,
    max.Q   = maxN,
    start.p = maxN,
    start.q = maxN,
    start.P = maxN,
    start.Q = maxN)

  # Store the arima parameters
  pars  <- tsoutliers::coefs2poly(fit)

  # Store the arima residuals
  resid <- cbind(tsData,residuals(fit))

  # Find the outliers
  x <- as.data.table(
    tsoutliers::locate.outliers(
      resid=resid[,3],
      pars=pars,
      cval=cvar,
      types= c("AO","TC","LS","IO","SLS")))

  # Merge back to source data
  residDT <- as.data.table(resid)
  z <- merge(residDT, x, by.x = "tsData.a", by.y = "ind", all.x = TRUE)
  setnames(z,
           c("tsData.a", "tsData.predicted", "residuals(fit)"),
           c("ObsNum", "Preds", "Residuals"))

  # Reorder data, remove the coefhat column to send to database or stakeholder
  z[, coefhat := NULL]
  remove(tsData)
  return(list(z,fit,resid))
}

#' GLRM_KMeans_Col Automated row clustering for mixed column types
#'
#' GLRM_KMeans_Col adds a column to your original data with a cluster number identifier. Uses glrm (grid tune-able) and then k-means to find optimal k.
#'
#' @author Adrian Antico
#' @param data is the source time series data.table
#' @param GridTuneGLRM If you want to grid tune the glrm model, set to TRUE, FALSE otherwise
#' @param GridTuneKMeans If you want to grid tuen the KMeans model, set to TRUE, FALSE otherwise
#' @param nthreads set based on number of threads your machine has available
#' @param MaxMem set based on the amount of memory your machine has available
#' @param glrmCols the column numbers for the glrm
#' @param IgnoreConstCols tell H20 to ignore any columns that have zero variance
#' @param glrmFactors similar to the number of factors to return from PCA
#' @param Loss set to one of "Quadratic", "Absolute", "Huber", "Poisson", "Hinge", "Logistic", "Periodic"
#' @param glrmMaxIters max number of iterations
#' @param SVDMethod choose from "Randomized","GramSVD","Power"
#' @param MaxRunTimeSecs set the timeout for max run time
#' @param KMeansK number of factors to test out in k-means to find the optimal number
#' @param KMeansMetric pick the metric to identify top model in grid tune c("totss","betweenss","withinss")
#' @examples
#' library(datasets)
#' library(AdrianModelingTools)
#' library(h2o)
#' # Import data
#' data <- as.data.table(iris)
#' # Run algo, excluding Species column
#' data <- GLRM_KMeans_Col(data,
#'                         GridTuneGLRM = TRUE,
#'                         nthreads = 8,
#'                         MaxMem = "28G",
#'                         glrmCols = 1:(ncol(data)-1),
#'                         IgnoreConstCols = TRUE,
#'                         glrmFactors = 2,
#'                         Loss = "Absolute",
#'                         glrmMaxIters = 1000,
#'                         SVDMethod = "Randomized",
#'                         MaxRunTimeSecs = 3600,
#'                         KMeansK = 5)
#' # View unique levels
#' unique(data[["Species"]])
#' unique(data[["ClusterID"]])
#' # Store ClusterID associated with Species Level
#' temp <- data[, mean(ClusterID), by = "Species"]
#' Setosa <- round(temp[Species == "setosa", V1][[1]],0)
#' Versicolor <- round(temp[Species == "versicolor", V1][[1]],0)
#' Virginica <- round(temp[Species == "virginica", V1][[1]],0)
#' # Line up ClusterID with Species Type
#' data[, Check := "a"]
#' data[ClusterID == eval(Setosa), Check := "setosa"]
#' data[ClusterID == eval(Virginica), Check := "virginica"]
#' data[ClusterID == eval(Versicolor), Check := "versicolor"]
#' # Collect accuracy measures
#' data[, Acc := as.numeric(ifelse(Check == Species, 1, 0))]
#' data[, mean(Acc)][[1]]
#' @return Original data.table with added column with cluster number identifier
#' @export
GLRM_KMeans_Col <- function(data,
                            GridTuneGLRM    = TRUE,
                            GridTuneKMeans  = TRUE,
                            nthreads        = 4,
                            MaxMem          = "14G",
                            glrmCols        = 3:ncol(data),
                            IgnoreConstCols = TRUE,
                            glrmFactors     = 5,
                            Loss            = "Quadratic",
                            glrmMaxIters    = 1000,
                            SVDMethod       = "Randomized",
                            MaxRunTimeSecs  = 3600,
                            KMeansK         = 50,
                            KMeansMetric    = "totss") {

  # Load libraries
  library(data.table)
  library(h2o)
  if(!is.data.table(data)) data <- as.data.table(data)
  
  # Build glmr model
  h2o.init(nthreads = nthreads, max_mem_size = MaxMem)
  datax <- as.h2o(data)
  if(GridTuneGLRM) {

    # Define grid tune search scheme in a named list
    search_criteria  <- list(strategy             = "RandomDiscrete",
                             max_runtime_secs     = 3600,
                             max_models           = 30,
                             seed                 = 1234,
                             stopping_rounds      = 10,
                             stopping_metric      = "MSE",
                             stopping_tolerance   = 1e-3)

    # Define hyperparameters
    HyperParams <- list(transform        = c("NONE", "DEMEAN", "DESCALE", "STANDARDIZE"),
                        k                = 1:5,
                        regularization_x = c("None","Quadratic","L2","L1","NonNegative", "OneSparse", "UnitOneSparse", "Simplex"),
                        regularization_y = c("None","Quadratic","L2","L1","NonNegative", "OneSparse", "UnitOneSparse", "Simplex"),
                        gamma_x          = seq(0.01,0.10,0.01),
                        gamma_y          = seq(0.01,0.10,0.01),
                        svd_method       = c("Randomized","GramSVD","Power"))

    # Run grid tune
    grid <- h2o.grid("glrm",
                     search_criteria   = search_criteria,
                     training_frame    = datax,
                     grid_id           = "Temp",
                     ignore_const_cols = IgnoreConstCols,
                     loss              = Loss,
                     hyper_params      = HyperParams)

    # Get best performer
    Grid_Out <- h2o.getGrid(grid_id = "Temp", sort_by = search_criteria$stopping_metric, decreasing = FALSE)
    model <- h2o.getModel(model_id = Grid_Out@model_ids[[1]])

  } else {
    model <- h2o.glrm(training_frame    = datax,
                      cols              = glrmCols,
                      ignore_const_cols = IgnoreConstCols,
                      k                 = glrmFactors,
                      loss              = Loss,
                      max_iterations    = glrmMaxIters,
                      svd_method        = SVDMethod,
                      max_runtime_secs  = MaxRunTimeSecs)
  }

  # Run k-means
  if(GridTuneKMeans) {

    # GLRM output
    x_raw <- h2o.getFrame(model@model$representation_name)
    Nam <- colnames(x_raw)

    # Define grid tune search scheme in a named list
    search_criteria  <- list(strategy             = "RandomDiscrete",
                             max_runtime_secs     = 3600,
                             max_models           = 30,
                             seed                 = 1234,
                             stopping_rounds      = 10)

    # Define hyperparameters
    HyperParams <- list(max_iterations   = c(10,20,50,100),
                        init             = c("Random","PlusPlus","Furthest"))

    # Run grid tune
    grid <- h2o.grid("kmeans",
                     search_criteria   = search_criteria,
                     training_frame    = x_raw,
                     x                 = Nam,
                     k                 = KMeansK,
                     grid_id           = "grid",
                     estimate_k        = TRUE,
                     hyper_params      = HyperParams)

    # Get best performer
    Grid_Out <- h2o.getGrid(grid_id = "grid", sort_by = KMeansMetric, decreasing = FALSE)
    model <- h2o.getModel(model_id = Grid_Out@model_ids[[1]])


  } else {
    x_raw <- h2o.getFrame(model@model$representation_name)
    Nam <- colnames(x_raw)
    model <- h2o.kmeans(training_frame = x_raw,
                        x              = Nam,
                        k              = KMeansK,
                        estimate_k     = TRUE)
  }

  # Combine outputs
  preds <- as.data.table(h2o.predict(model, x_raw))
  h2o.shutdown(prompt = FALSE)
  data <- as.data.table(cbind(preds, data))
  setnames(data, "predict", "ClusterID")
  return(data)
}

#' AutoTS is an automated time series modeling function
#'
#' AutoTS builds the best time series models for each type, compares all types, selects the winner, and generate forecasts. Ensemble is also a feature where a randomForest model is build on the model outputs to utilize all in a more accuracte forecast.
#'
#' @author Adrian Antico
#' @param data is the source time series data.table
#' @param TargetName is the name of the dependent variable in your data.table
#' @param DateName is the name of the date column in your data.table
#' @param FCPeriods is the number of periods into the future you wish to forecast
#' @param HoldOutPeriods is the number of periods to use for validation testing
#' @param TimeUnit is the level of aggregation your dataset comes in
#' @param Lags is the number of lags you wish to test in various models (same with moving averages)
#' @param SLags is the number of seasonal lags you wish to test in various models (same with moving averages)
#' @param SkipModels Don't run specified models - e.g. exclude all models c("ARFIMA","ARIMA","ETS","SPLINE","NNET","TBATS","TSLM","PROPHET")
#' @param StepWise Set to TRUE to have ARIMA and ARFIMA run a stepwise selection process. Otherwise, all models will be generated in parallel execution, but still run much slower.
#' @examples
#' data <- data.table(DateTime = as.Date(Sys.time()), Target = stats::filter(rnorm(1000,mean = 50, sd = 20), filter=rep(1,10), circular=TRUE))
#' data[, temp := seq(1:1000)][, DateTime := DateTime - temp][, temp := NULL]
#' data <- data[order(DateTime)]
#' output <-   AutoTS(data,
#'                    TargetName     = "Target",
#'                    DateName       = "DateTime",
#'                    FCPeriods      = 30,
#'                    HoldOutPeriods = 30,
#'                    TimeUnit       = "day", # c("hour","day","week","month","quarter","year"),
#'                    Lags           = 25,
#'                    SLags          = 2,
#'                    NumCores       = 4,
#'                    SkipModels     = NULL,
#'                    StepWise       = TRUE)
#' @return If Ensemble is TRUE, return a data.table object with a date column and the forecasts, an evaluation data set, and an ensemble training data set (all in a list). If Ensemble is FALSE, then all items returned except the ensemble training set.
#' @export
AutoTS <- function(data,
                   TargetName     = "Targets",
                   DateName       = "DateTime",
                   FCPeriods      = 30,
                   HoldOutPeriods = 30,
                   TimeUnit       = "day", # c("hour","day","week","month","quarter","year"),
                   Lags           = 25,
                   SLags          = 2,
                   NumCores       = 4,
                   SkipModels     = NULL,
                   StepWise       = TRUE) {

  # Load libraries
  library(prophet)
  library(forecast)
  library(data.table)
  library(lubridate)

  # Initialize collection variables
  i <- 0
  EvalList <- list()

  # Convert to data.table if not already
  if (!is.data.table(data)) data <- as.data.table(data)

  # Convert to lubridate as_date() or POSIXct
  if(tolower(TimeUnit) != "hour") {
    data[, eval(DateName) := as_date(get(DateName))]
  } else {
    data[, eval(DateName) := as.POSIXct(get(DateName))]
    SkipModels <- c(SkipModels,"PROPHET")
    if(length(SkipModels) == 8) return("Prophet doesn't run on hourly data. Choose other models.")
  }

  # Ensure data is sorted
  data <- data[order(get(DateName))]

  # Change Target Name
  setnames(data, paste0(eval(TargetName)), "Target")
  TargetName <- "Target"

  # Create Training data
  data_train <- data[1:(nrow(data)-HoldOutPeriods)]

  # Create Test data
  data_test <- data[(nrow(data)-HoldOutPeriods+1):nrow(data)]

  # Check for different time aggregations
  MaxDate <- data_train[, max(get(DateName))]
  FC_Data <- data.table(Date = seq(1:FCPeriods))

  # Define TS Frequency
  if(tolower(TimeUnit) == "hour") {
    freq = 24
    FC_Data[, Date := MaxDate + lubridate::hours(Date)]
  } else if (tolower(TimeUnit) == "day") {
    freq = 365
    FC_Data[, Date := MaxDate + lubridate::days(Date)]
  } else if (tolower(TimeUnit) == "week") {
    freq = 52
    FC_Data[, Date := MaxDate + lubridate::weeks(Date)]
  } else if (tolower(TimeUnit) == "month") {
    freq = 12
    FC_Data[, Date := MaxDate + lubridate::months(Date)]
  } else if (tolower(TimeUnit) == "quarter") {
    freq = 4
    FC_Data[, Date := MaxDate + lubridate::months(4*Date)]
  } else if (tolower(TimeUnit) == "year") {
    freq = 1
    FC_Data[, Date := MaxDate + years(Date)]
  }

  # Convert data.tables to ts objects
  dataTSTrain <- ts(data = data_train, start = data_train[, min(get(DateName))][[1]], frequency = freq)

  # Begin model building
  if(!("ARFIMA" %in% toupper(SkipModels))) {
    # ARFIMA-------------
    # 1)
    print("ARFIMA FITTING")
    if(StepWise) {
      ARFIMA_model <- tryCatch({forecast::arfima(y = dataTSTrain[, TargetName],
                                                 lambda = TRUE,
                                                 biasadj = TRUE,
                                                 max.p = Lags,
                                                 max.q = Lags,
                                                 max.d = 1,
                                                 max.D = 1,
                                                 ic = "bic",
                                                 stepwise = StepWise,
                                                 num.cores = NumCores)},
                               error = function(x) "empty")
    } else {
      ARFIMA_model <- tryCatch({forecast::arfima(y = dataTSTrain[, TargetName],
                                                 lambda = TRUE,
                                                 biasadj = TRUE,
                                                 max.p = Lags,
                                                 max.q = Lags,
                                                 max.d = 1,
                                                 max.D = 1,
                                                 ic = "bic",
                                                 stepwise = StepWise,
                                                 parallel = TRUE,
                                                 num.cores = NumCores)},
                               error = function(x) "empty")
    }

    # Collect Test Data for Model Comparison
    # 2)
    if(tolower(class(ARFIMA_model)) == "fracdiff") {
      i <- i + 1
      data_test_ARF <- copy(data_test)
      data_test_ARF[, ':=' (Target = as.numeric(Target),
                            ModelName = rep("ARFIMA",HoldOutPeriods),
                            FC_Eval = as.numeric(forecast::forecast(ARFIMA_model, h = HoldOutPeriods)$mean))]

      # Add Evaluation Columns
      # 3)
      data_test_ARF[, ':=' (Resid = Target - FC_Eval,
                            PercentError = get(TargetName) / (FC_Eval+1) - 1,
                            AbsolutePercentError = abs(get(TargetName) / (FC_Eval+1) - 1))]

      # Collect model filename
      EvalList[[i]] <- data_test_ARF
    }
  }

  if(!("ARIMA" %in% toupper(SkipModels))) {
    # ARIMA-------------
    # 1)
    print("ARIMA FITTING")
    if(StepWise) {
      ARIMA_model <- tryCatch({forecast::auto.arima(y = dataTSTrain[, TargetName],
                                                    max.p = Lags,
                                                    max.q = Lags,
                                                    max.P = SLags,
                                                    max.Q = SLags,
                                                    max.d = 1,
                                                    max.D = 1,
                                                    ic = "bic",
                                                    lambda = TRUE,
                                                    biasadj = TRUE,
                                                    stepwise = StepWise,
                                                    num.cores = NumCores)},
                              error = function(x) "empty")
    } else {
      ARIMA_model <- tryCatch({forecast::auto.arima(y = dataTSTrain[, TargetName],
                                                    max.p = Lags,
                                                    max.q = Lags,
                                                    max.P = SLags,
                                                    max.Q = SLags,
                                                    max.d = 1,
                                                    max.D = 1,
                                                    ic = "bic",
                                                    lambda = TRUE,
                                                    biasadj = TRUE,
                                                    stepwise = StepWise,
                                                    parallel = TRUE,
                                                    num.cores = NumCores)},
                              error = function(x) "empty")
    }

    # Collect Test Data for Model Comparison
    # 2)
    if(tolower(class(ARIMA_model)[1]) == "arima") {
      i <- i + 1
      data_test_ARI <- copy(data_test)
      data_test_ARI[, ':=' (Target = as.numeric(Target),
                            ModelName = rep("ARIMA",HoldOutPeriods),
                            FC_Eval = as.numeric(forecast::forecast(ARIMA_model, h = HoldOutPeriods)$mean))]

      # Add Evaluation Columns
      # 3)
      data_test_ARI[, ':=' (Resid = get(TargetName) - FC_Eval,
                            PercentError = get(TargetName) / (FC_Eval+1) - 1,
                            AbsolutePercentError = abs(get(TargetName) / (FC_Eval+1) - 1))]

      # Collect model filename
      EvalList[[i]] <- data_test_ARI
    }
  }

  if(!("ETS" %in% toupper(SkipModels))) {
    # EXPONENTIAL SMOOTHING-------------
    # 1)
    print("ETS FITTING")
    if (freq > 24) { # when > 24, model's third letter has to be N for none (no seasonal estimation)
      EXPSMOOTH_model <- forecast::ets(y = dataTSTrain[, TargetName],
                                       model = "ZZN",
                                       allow.multiplicative.trend = TRUE,
                                       restrict = TRUE,
                                       lambda = TRUE,
                                       biasadj = TRUE)
    } else {
      EXPSMOOTH_model <- tryCatch({forecast::ets(y = dataTSTrain[, TargetName],
                                                 model = "ZZZ",
                                                 allow.multiplicative.trend = TRUE,
                                                 restrict = TRUE,
                                                 lambda = TRUE,
                                                 biasadj = TRUE)},
                                  error = function(x) "empty")
    }
    # Collect Test Data for Model Comparison
    # 2)
    if(tolower(class(EXPSMOOTH_model)) == "ets") {
      i <- i + 1
      data_test_ETS <- copy(data_test)
      data_test_ETS[, ':=' (Target = as.numeric(Target),
                            ModelName = rep("ETS",HoldOutPeriods),
                            FC_Eval = as.numeric(forecast::forecast(EXPSMOOTH_model, h = HoldOutPeriods)$mean))]

      # Add Evaluation Columns
      # 3)
      data_test_ETS[, ':=' (Resid = get(TargetName) - FC_Eval,
                            PercentError = get(TargetName) / (FC_Eval+1) - 1,
                            AbsolutePercentError = abs(get(TargetName) / (FC_Eval+1) - 1))]

      # Collect model filename
      EvalList[[i]] <- data_test_ETS
    }
  }

  if(!("SPLINE" %in% toupper(SkipModels))) {
    # CUBIC SMOOTHING SPLINE-------------
    # 1)
    print("SPLINE FITTING")
    splinef_model <- tryCatch({forecast::splinef(y = dataTSTrain[, TargetName],
                                                 lambda = TRUE,
                                                 biasadj = TRUE)},
                              error = function(x) "empty")

    if(tolower(class(splinef_model)) == "forecast") {
      i <- i + 1
      # Collect Test Data for Model Comparison
      # 2)
      data_test_CS <- copy(data_test)
      data_test_CS[, ':=' (Target = as.numeric(Target),
                           ModelName = rep("CS",HoldOutPeriods),
                           FC_Eval = as.numeric(forecast::forecast(splinef_model, h = HoldOutPeriods)$mean))]

      # Add Evaluation Columns
      # 3)
      data_test_CS[, ':=' (Resid = get(TargetName) - FC_Eval,
                           PercentError = get(TargetName) / (FC_Eval+1) - 1,
                           AbsolutePercentError = abs(get(TargetName) / (FC_Eval+1) - 1))]

      # Collect model filename
      EvalList[[i]] <- data_test_CS
    }
  }

  if(!("TBATS" %in% toupper(SkipModels))) {
    # TBATS-------------
    # 1)
    print("TBATS FITTING")
    TBATS_model <- tryCatch({forecast::tbats(y               = dataTSTrain[, TargetName],
                                             use.arma.errors = TRUE,
                                             lambda          = TRUE,
                                             biasadj         = TRUE,
                                             max.p           = Lags,
                                             max.q           = Lags,
                                             max.P           = SLags,
                                             max.Q           = SLags,
                                             max.d           = 1,
                                             max.D           = 1,
                                             num.cores       = NumCores)},
                            error = function(x) "empty")

    if(class(TBATS_model)[1] == "tbats" | class(TBATS_model)[1] == "bats") {
      i <- i + 1
      # Collect Test Data for Model Comparison
      # 2)
      data_test_TBATS <- copy(data_test)
      data_test_TBATS[, ':=' (Target = as.numeric(Target),
                              ModelName = rep("TBATS",HoldOutPeriods),
                              FC_Eval = as.numeric(forecast::forecast(TBATS_model, h = HoldOutPeriods)$mean))]

      # Add Evaluation Columns
      # 3)
      data_test_TBATS[, ':=' (Resid = get(TargetName) - FC_Eval,
                              PercentError = get(TargetName) / (FC_Eval+1) - 1,
                              AbsolutePercentError = abs(get(TargetName) / (FC_Eval+1) - 1))]

      # Collect model filename
      EvalList[[i]] <- data_test_TBATS
    }
  }

  if(!("TSLM" %in% toupper(SkipModels))) {
    # LINEAR MODEL WITH TIME SERIES COMPONENTS-------------
    # 1)
    print("TSLM FITTING")
    TSLM_model <- tryCatch({forecast::tslm(dataTSTrain[, TargetName] ~ trend + season,
                                           lambda = TRUE,
                                           biasadj = TRUE)},
                           error = function(x) "empty")

    if(tolower(class(TSLM_model)[1]) == "tslm") {
      i <- i + 1
      # Collect Test Data for Model Comparison
      # 2)
      data_test_TSLM <- copy(data_test)
      data_test_TSLM[, ':=' (Target = as.numeric(Target),
                             ModelName = rep("TSLM",HoldOutPeriods),
                             FC_Eval = as.numeric(forecast::forecast(TSLM_model, h = HoldOutPeriods)$mean))]

      # Add Evaluation Columns
      # 3)
      data_test_TSLM[, ':=' (Resid = get(TargetName) - FC_Eval,
                             PercentError = get(TargetName) / (FC_Eval+1) - 1,
                             AbsolutePercentError = abs(get(TargetName) / (FC_Eval+1) - 1))]

      # Collect model filename
      EvalList[[i]] <- data_test_TSLM
    }
  }

  if(!("NNET" %in% toupper(SkipModels))) {
    # Neural Network-------------
    # 1)
    print("NNet FITTING")
    k <- 0L
    temp <- data.table(Lag = rep(1L, Lags*SLags), Slag = rep(1L, Lags*SLags), meanResid = rnorm(Lags*SLags), sdResid = rnorm(Lags*SLags))
    for (lags in 1:Lags) {
      for (slags in 1:SLags) {
        k <- k + 1L
        print(k)
        NNETAR_model_temp <- tryCatch({forecast::nnetar(y = dataTSTrain[, TargetName],
                                                        p = lags,
                                                        P = slags,
                                                        lambda = "auto")}, error = function(x) "error")

        if(length(NNETAR_model_temp) == 1) {
          set(temp, i = k, j = 1L, value = lags)
          set(temp, i = k, j = 2L, value = slags)
          set(temp, i = k, j = 3L, value = 999999999)
          set(temp, i = k, j = 4L, value = 999999999)

        } else {
          set(temp, i = k, j = 1L, value = lags)
          set(temp, i = k, j = 2L, value = slags)
          set(temp, i = k, j = 3L, value = mean(abs(NNETAR_model_temp$residuals), na.rm = TRUE))
          set(temp, i = k, j = 4L, value = sd(NNETAR_model_temp$residuals, na.rm = TRUE))
        }
      }
    }

    # Identify best model and retrain it
    LagNN <- temp[order(meanResid)][1,][,1][[1]]
    SLagNN <- temp[order(meanResid)][1,][,2][[1]]
    NNETAR_model <- tryCatch({forecast::nnetar(y = dataTSTrain[, TargetName],
                                               p = LagNN,
                                               P = SLagNN,
                                               lambda = "auto")},
                             error = function(x) "empty")

    # Collect Test Data for Model Comparison
    # 2)
    if(tolower(class(NNETAR_model)) == "nnetar") {
      i <- i + 1
      data_test_NN <- copy(data_test)
      data_test_NN[, ':=' (Target = as.numeric(Target),
                           ModelName = rep("NN",HoldOutPeriods),
                           FC_Eval = as.numeric(forecast::forecast(NNETAR_model, h = HoldOutPeriods)$mean))]

      # Add Evaluation Columns
      # 3)
      data_test_NN[, ':=' (Resid = get(TargetName) - FC_Eval,
                           PercentError = get(TargetName) / (FC_Eval+1) - 1,
                           AbsolutePercentError = abs(get(TargetName) / (FC_Eval+1) - 1))]

      # Collect model filename
      EvalList[[i]] <- data_test_NN
    }
  }

  if(!("PROPHET" %in% toupper(SkipModels))) {
    # Prophet Model-------------
    print("PROPHET FITTING")
    if(TimeUnit == "hour") {
      ProphetTimeUnit <- 3600
    } else {
      ProphetTimeUnit <- TimeUnit
    }

    max_date <- data_train[, max(DateTime)]
    dataProphet <- copy(data_train)
    setnames(dataProphet, c("DateTime", "Target"), c("ds", "y"))

    # 1)
    # Define TS Frequency
    if(TimeUnit == "day") {
      PROPHET_model <- tryCatch({prophet(df = dataProphet, daily.seasonality = TRUE)},
                                error = function(x) "empty")
    } else if(TimeUnit == "week") {
      PROPHET_model <- tryCatch({prophet(df = dataProphet, weekly.seasonality = TRUE)},
                                error = function(x) "empty")
    } else if(TimeUnit == "year") {
      PROPHET_model <- tryCatch({prophet(df = dataProphet, yearly.seasonality = TRUE)},
                                error = function(x) "empty")
    } else {
      PROPHET_model <- tryCatch({prophet(df = dataProphet)},
                                error = function(x) "empty")
    }

    if(tolower(class(PROPHET_model)[1]) == "prophet") {
      i <- i + 1
      PROPHET_future <- as.data.table(prophet::make_future_dataframe(PROPHET_model, periods = HoldOutPeriods, freq = ProphetTimeUnit))[ds > max_date]

      # Collect Test Data for Model Comparison
      # 2)
      data_test_PROPHET <- copy(data_test)
      data_test_PROPHET[, ':=' (Target = as.numeric(Target),
                                ModelName = rep("PROPHET",HoldOutPeriods),
                                FC_Eval = as.data.table(predict(PROPHET_model, PROPHET_future))[["yhat"]])]
      # Add Evaluation Columns
      # 3)
      data_test_PROPHET[, ':=' (Resid = get(TargetName) - FC_Eval,
                                PercentError = get(TargetName) / (FC_Eval+1) - 1,
                                AbsolutePercentError = abs(get(TargetName) / (FC_Eval+1) - 1))]

      # Collect model filename
      EvalList[[i]] <- data_test_PROPHET
    }
  }

  # Model Collection-------------
  print("FIND WINNER")
  dataEval <- rbindlist(EvalList)

  # Model Evaluation
  Eval <- dataEval[, .(MeanResid = mean(Resid, na.rm = TRUE),
                       MeanPercError = mean(PercentError, na.rm = TRUE),
                       MAPE = mean(AbsolutePercentError, na.rm = TRUE)),
                   by = ModelName][order(MAPE)][, ID := 1:.N]

  # Grab Winning Model
  BestModel <- Eval[1,"ModelName"][[1]]

  # Generate Forecasts----
  print("GENERATE FORECASTS")

  # Create Training data
  data_train <- data[1:nrow(data)]

  # Convert data.tables to ts objects
  dataTSTrain <- ts(data = data_train, start = data_train[, min(get(DateName))][[1]], frequency = freq)

  # Retrain best model
  if(BestModel == "ARFIMA") {

    # Rebuild model on full data
    if(StepWise) {
      ARFIMA_model <- forecast::arfima(y = dataTSTrain[, TargetName],
                                       lambda = TRUE,
                                       biasadj = TRUE,
                                       max.p = Lags,
                                       max.q = Lags,
                                       max.d = 1,
                                       max.D = 1,
                                       ic = "bic",
                                       stepwise = StepWise,
                                       num.cores = NumCores)
    } else {
      ARFIMA_model <- forecast::arfima(y = dataTSTrain[, TargetName],
                                       lambda = TRUE,
                                       biasadj = TRUE,
                                       max.p = Lags,
                                       max.q = Lags,
                                       max.d = 1,
                                       max.D = 1,
                                       ic = "bic",
                                       stepwise = StepWise,
                                       parallel = TRUE,
                                       num.cores = NumCores)
    }

    # Forecast with new model
    FC_Data[, paste0("Forecast_",BestModel) := as.numeric(forecast::forecast(ARFIMA_model, h = FCPeriods)$mean)]

  } else if(BestModel == "ARIMA") {

    # Rebuild model on full data
    if(StepWise) {
      ARIMA_model <- forecast::auto.arima(y     = dataTSTrain[, TargetName],
                                          max.p = Lags,
                                          max.q = Lags,
                                          max.P = SLags,
                                          max.Q = SLags,
                                          max.d = 1,
                                          max.D = 1,
                                          ic = "bic",
                                          lambda = TRUE,
                                          biasadj = TRUE,
                                          stepwise = StepWise,
                                          num.cores = NumCores)
    } else {
      ARIMA_model <- forecast::auto.arima(y     = dataTSTrain[, TargetName],
                                          max.p = Lags,
                                          max.q = Lags,
                                          max.P = SLags,
                                          max.Q = SLags,
                                          max.d = 1,
                                          max.D = 1,
                                          ic = "bic",
                                          lambda = TRUE,
                                          biasadj = TRUE,
                                          stepwise = StepWise,
                                          parallel = TRUE,
                                          num.cores = NumCores)
    }

    # Forecast with new model
    FC_Data[, paste0("Forecast_",BestModel) := as.numeric(forecast::forecast(ARIMA_model, h = FCPeriods)$mean)]

  } else if(BestModel == "ETS") {

    # Rebuild model on full data
    if (freq > 24) { # when > 24, model's third letter has to be N for none (no seasonal estimation)
      EXPSMOOTH_model <- forecast::ets(y                          = dataTSTrain[, TargetName],
                                       model                      = "ZZN",
                                       allow.multiplicative.trend = TRUE,
                                       restrict                   = TRUE,
                                       lambda                     = TRUE,
                                       biasadj                    = TRUE)
    } else {
      EXPSMOOTH_model <- forecast::ets(y                          = dataTSTrain[, TargetName],
                                       model                      = "ZZZ",
                                       allow.multiplicative.trend = TRUE,
                                       restrict                   = TRUE,
                                       lambda                     = TRUE,
                                       biasadj                    = TRUE)
    }

    # Forecast with new model
    FC_Data[, paste0("Forecast_",BestModel) := as.numeric(forecast::forecast(EXPSMOOTH_model, h = FCPeriods)$mean)]

  } else if(BestModel == "CS") {

    # Rebuild model on full data
    splinef_model <- forecast::splinef(y = dataTSTrain[, TargetName], lambda = TRUE, biasadj = TRUE)

    # Forecast with new model
    FC_Data[, paste0("Forecast_",BestModel) := as.numeric(forecast::forecast(splinef_model, h = FCPeriods)$mean)]

  } else if(BestModel == "TBATS") {

    # Rebuild model on full data
    TBATS_model <- forecast::tbats(y = dataTSTrain[, TargetName],
                                   use.arma.errors = TRUE,
                                   lambda = TRUE,
                                   biasadj = TRUE,
                                   max.p = Lags,
                                   max.q = Lags,
                                   max.P = SLags,
                                   max.Q = SLags,
                                   max.d = 1,
                                   max.D = 1,
                                   num.cores = NumCores)

    # Forecast with new model
    FC_Data[, paste0("Forecast_",BestModel) := as.numeric(forecast::forecast(TBATS_model, h = FCPeriods)$mean)]

  } else if(BestModel == "TSLM") {

    # Rebuild model on full data
    TSLM_model <- forecast::tslm(dataTSTrain[, TargetName] ~ trend + season,
                                 lambda = TRUE,
                                 biasadj = TRUE)

    # Forecast with new model
    FC_Data[, paste0("Forecast_",BestModel) := as.numeric(forecast::forecast(TSLM_model, h = FCPeriods)$mean)]

  } else if(BestModel == "NN") {

    # Rebuild model on full data
    k <- 0L
    temp <- data.table(Lag = rep(1L, Lags*SLags), Slag = rep(1L, Lags*SLags), meanResid = rnorm(Lags*SLags), sdResid = rnorm(Lags*SLags))
    for (lags in 1:Lags) {
      for (slags in 1:SLags) {
        k <- k + 1L
        print(k)
        NNETAR_model_temp <- tryCatch({forecast::nnetar(y = dataTSTrain[, TargetName],
                                                        p = lags,
                                                        P = slags,
                                                        lambda = "auto")}, error = function(x) "error")

        if(length(NNETAR_model_temp) == 1) {
          set(temp, i = k, j = 1L, value = lags)
          set(temp, i = k, j = 2L, value = slags)
          set(temp, i = k, j = 3L, value = 999999999)
          set(temp, i = k, j = 4L, value = 999999999)

        } else {
          set(temp, i = k, j = 1L, value = lags)
          set(temp, i = k, j = 2L, value = slags)
          set(temp, i = k, j = 3L, value = mean(abs(NNETAR_model_temp$residuals), na.rm = TRUE))
          set(temp, i = k, j = 4L, value = sd(NNETAR_model_temp$residuals, na.rm = TRUE))
        }
      }
    }

    # Identify best model and retrain it
    LagNN <- temp[order(meanResid)][1,][,1][[1]]
    SLagNN <- temp[order(meanResid)][1,][,2][[1]]
    NNETAR_model <- tryCatch({forecast::nnetar(y = dataTSTrain[, TargetName],
                                               p = LagNN,
                                               P = SLagNN)},
                             error = function(x) "empty")

    # Forecast with new model
    FC_Data[, paste0("Forecast_",BestModel) := as.numeric(forecast::forecast(NNETAR_model, h = FCPeriods)$mean)]

  } else if(BestModel == "PROPHET") {

    # Rebuild model on full data
    print("PROPHET FITTING")
    if(TimeUnit == "hour") {
      ProphetTimeUnit <- 3600
    } else {
      ProphetTimeUnit <- TimeUnit
    }

    max_date <- data_train[, max(DateTime)]
    dataProphet <- copy(data_train)
    setnames(dataProphet, c("DateTime", "Target"), c("ds", "y"))

    # 1)
    # Define TS Frequency
    if(TimeUnit == "day") {
      PROPHET_model <- prophet(df = dataProphet, daily.seasonality = TRUE)
    } else if(TimeUnit == "week") {
      PROPHET_model <- prophet(df = dataProphet, weekly.seasonality = TRUE)
    } else if(TimeUnit == "year") {
      PROPHET_model <- prophet(df = dataProphet, yearly.seasonality = TRUE)
    } else {
      PROPHET_model <- prophet(df = dataProphet)
    }

    # Forecast with new model
    PROPHET_FC <- as.data.table(prophet::make_future_dataframe(PROPHET_model, periods = FCPeriods, freq = ProphetTimeUnit))[ds > MaxDate]
    FC_Data[, Forecast_PROPHET := as.data.table(predict(PROPHET_model, PROPHET_FC))[["yhat"]]]
  }

  # Return values
  return(list(FC_Data,Eval))
}

#' tempDatesFun Convert Excel datetime char columns to Date columns
#'
#' tempDatesFun takes the Excel datetime column, which imports as character, and converts it into a date type
#'
#' @author Adrian Antico
#' @examples
#' Cdata[, DAY_DATE := tempDatesFun(DAY_DATE)]
#' Cdata[, DAY_DATE := as.Date(DAY_DATE, "%m/%d/%Y")]
#' @return An object to pass along to ggplot objects following the "+" sign
#' @export
tempDatesFun <- Vectorize(function(x) {
  strsplit(x, " ")[[1]][1]
})

#' SimpleCap function is for capitalizing the first letter of words
#'
#' SimpleCap function is for capitalizing the first letter of words (need I say more?)
#'
#' @author Adrian Antico
#' @examples
#' x <- adrian
#' x <- SimpleCap(x)
#' @return An object to pass along to ggplot objects following the "+" sign
#' @export
SimpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

#' RemixTheme function is a ggplot theme generator for ggplots
#'
#' This function adds the Remix Theme to ggplots
#'
#' @author DougVegas at RemixInstitute.com
#' @examples
#' p <- ggplot(data, aes(x = weeks, y = quantity)) + geom_line()
#' p <- p + RemixTheme()
#' p
#' @return An object to pass along to ggplot objects following the "+" sign
#' @export
RemixTheme <- function(){

  theme(
    axis.title = element_blank(),
    axis.text = element_text(family = "A",
                             size = 11),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(family = "A",
                               color = "#1c1c1c", size = 11),
    legend.title = element_blank(),
    legend.justification = 0,
    legend.position = "top",
    plot.background = element_rect(fill = "#E7E7E7"),
    panel.background = element_rect(fill= "#E7E7E7"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "white"),
    panel.grid.minor.y = element_line(color = "white"),
    plot.title = element_text(family = "A",
                              color = "#1c1c1c", size = 20, hjust = 0),
    plot.subtitle = element_text(family = "C",
                                 color = "#1c1c1c", size = 12, hjust = 0),
    plot.caption = element_text(family = "B",
                                size = 9, hjust = 0, face = "italic")
  )
}

#' Final Data Preparation Function
#'
#' This function replaces inf values with NA, converts characters to factors, and imputes with constants
#'
#' @author Adrian Antico
#' @param Impute Defaults to TRUE which tells the function to impute the data
#' @param CharToFactor Defaults to TRUE which tells the function to convert characters to factors
#' @param data This is your source data you'd like to modify
#' @param MissFactor Supply the value to impute missing factor levels
#' @param MissNum Supply  the value to impute missing numeric values
#' @examples
#' data <- ModelDataPrep(data,
#'                       Impute = TRUE,
#'                       CharToFactor = TRUE,
#'                       MissFactor = "0",
#'                       MissNum    = -1)
#' @return Returns the original data table with corrected values
#' @export
ModelDataPrep <- function(data,
                          Impute     = TRUE,
                          CharToFactor = TRUE,
                          MissFactor = "0",
                          MissNum    = -1) {

  # Load libraries
  library(data.table)
  if(!is.data.table(data)) data <- as.data.table(data)

  # Replace any inf values with NA
  for (col in seq_along(data)) {
    set(data, j = col, value = replace(data[[col]], is.infinite(data[[col]]),NA))
  }

  # Turn character columns into factors
  if(CharToFactor) {
    for (col in seq_along(data)) {
      if(is.character(data[[col]])) {
        set(data, j = col, value = as.factor(data[[col]]))
      }
    }
  }

  # Impute missing values
  if(Impute) {
    for (j in seq_along(data)) {
      if(is.factor(data[[j]])) {
        set(data,which(!(data[[j]] %in% levels(data[[j]]))),j,MissFactor)
      } else {
        set(data,which(is.na(data[[j]])),j,MissNum)
      }
    }
  }
  return(data)
}

#' RedYellowGreen is for determining the optimal thresholds for binary classification when do-nothing is an option
#'
#' This function will find the optimial thresholds for applying the main label and for finding the optimial range for doing nothing when you can quantity the cost of doing nothing
#'
#' @author Adrian Antico
#' @param calibEval data is the data table with your predicted and actual values from a classification model
#' @param PredictColNumber The column number where the actual target variable is located (in binary form)
#' @param ActualColNumber The column number where the predicted values are located
#' @param TruePositiveCost This is the utility for generating a true positive prediction
#' @param TruePositiveCost This is the utility for generating a true negative prediction
#' @param FalsePositiveCost This is the cost of generating a false positive prediction
#' @param FalseNegativeCost This is the cost of generating a false negative prediction
#' @param MidTierCost This is the cost of doing nothing (or whatever it means to not classify in your case)
#' @param Cores Number of cores on your machine
#' @param Precision Set the decimal number to increment by between 0 and 1
#' @examples
#' library(h2o)
#' library(AdrianModelingTools)
#' library(data.table)
#' library(ggplot2)
#' Correl <- 0.85
#' aa <- data.table(target = runif(10000))
#' aa[, x1 := qnorm(target)]
#' aa[, x2 := runif(10000)]
#' aa[, Independent_Variable1 := log(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable2 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable3 := exp(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable4 := exp(exp(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2))))]
#' aa[, Independent_Variable5 := sqrt(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable6 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^0.10]
#' aa[, Independent_Variable7 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^0.25]
#' aa[, Independent_Variable8 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^0.75]
#' aa[, Independent_Variable9 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^2]
#' aa[, Independent_Variable10 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^4]
#' aa[, ':=' (x1 = NULL, x2 = NULL)]
#' aa[, target := as.factor(ifelse(target < 0.3333, 0,1))]
#' N = 1
#' Construct <- data.table(Targets         = "target",
#'                         Distribution    = "bernoulli",
#'                         Loss            = "auc",
#'                         Quantile        = 0.01,
#'                         ModelName       = "bla",
#'                         Algorithm       = "gbm",
#'                         dataName        = "aa",
#'                         TargetCol       = c("1"),
#'                         FeatureCols     = c("2:10"),
#'                         CreateDate      = Sys.time(),
#'                         GridTune        = FALSE,
#'                         ExportValidData = TRUE,
#'                         ParDep          = 10,
#'                         PD_Data         = "validate",
#'                         ThreshType      = "f1",
#'                         FSC             = 0.001,
#'                         tpProfit        = rep(0,N),
#'                         tnProfit        = rep(0,N),
#'                         fpProfit        = rep(-1,N),
#'                         fnProfit        = rep(-5,N),
#'                         SaveModel       = rep("FALSE",N),
#'                         SaveModelType   = rep("Mojo",N),
#'                         PredsAllData    = rep(TRUE,N),
#'                         TargetEncoding  = rep(NA,N))
#' AutoH20Modeler(Construct,
#'                max_memory = "28G",
#'                ratios = 0.75,
#'                BL_Trees = 500,
#'                nthreads = 5,
#'               model_path = getwd(),
#'                MaxRuntimeSeconds = 3600,
#'                MaxModels = 30)
#' load(paste0(getwd(), "/bla.Rdata"))
#' data <- RedYellowGreen(calibEval,
#'                        PredictColNumber  = 1,
#'                        ActualColNumber   = 2,
#'                        TruePositiveCost  = 0,
#'                        TrueNegativeCost  = 0,
#'                        FalsePositiveCost = -1,
#'                        FalseNegativeCost = -2,
#'                        MidTierCost       = -0.5)
#' @return A data table with all evaluated strategies, parameters, and utilities, along with a 3d scatterplot of the results
#' @export
RedYellowGreen <- function(calibEval,
                           PredictColNumber  = 2,
                           ActualColNumber   = 1,
                           TruePositiveCost  = 0,
                           TrueNegativeCost  = 0,
                           FalsePositiveCost = -10,
                           FalseNegativeCost = -50,
                           MidTierCost       = -2,
                           Cores             = 8,
                           Precision         = 0.01) {
  
  # Load libraries
  library(data.table)
  if(!is.data.table(calibEval)) data <- as.data.table(calibEval)

  # Set up evaluation table
  analysisTable <- data.table(TPP = rep(TruePositiveCost,1),
                              TNP = rep(TrueNegativeCost,1),
                              FPP = rep(FalsePositiveCost,1),
                              FNP = rep(FalseNegativeCost,1),
                              MTDN = rep(TRUE,1),
                              MTC = rep(MidTierCost,1),
                              Threshold = runif(1))

  # Do nothing possibilities
  temp     <- CJ(MTLT = seq(0.0,1.0,Precision), MTHT = seq(0.0,1.0,Precision))[MTHT > MTLT]
  new      <- cbind(analysisTable, temp)
  new[, Utility := runif(nrow(new))]

  # Parallel components
  suppressMessages(library(parallel))
  suppressMessages(library(snow))
  suppressMessages(library(doParallel))
  packages <- c("data.table")
  cores    <- Cores
  bat      <- ceiling(nrow(new)/cores)
  parts    <- floor(nrow(new) / bat)
  cl       <- makePSOCKcluster(cores)
  registerDoParallel(cl)

  # Kick off run
  results <- foreach(i            = itertools::isplitRows(new, chunks=parts),  # splits data and passes to each core
                     .combine      = function(...) rbindlist(list(...)),        # only way to get rbindlist to work
                     .multicombine = TRUE,                                      # required for rbindlist since list > 2 data sets
                     .packages     = packages                                   # need to feed in packages used
  ) %dopar% {
    # Inner function for parallel version
    RedYellowGreenParallel <- function(data,
                                       PredictColNumber  = 1,
                                       ActualColNumber   = 767,
                                       TruePositiveCost  = 0,
                                       TrueNegativeCost  = 0,
                                       FalsePositiveCost = -1,
                                       FalseNegativeCost = -10,
                                       MidTierCost       = -5,
                                       new = i) {

      # Loop through all combos
      for (k in as.integer(1:nrow(new))) {
        x <- threshOptim(data = data,
                         actTar = names(data)[ActualColNumber],
                         predTar = names(data)[PredictColNumber],
                         tpProfit = TruePositiveCost,
                         tnProfit = TrueNegativeCost,
                         fpProfit = FalsePositiveCost,
                         fnProfit = FalseNegativeCost,
                         MidTierDoNothing = TRUE,
                         MidTierCost = MidTierCost,
                         MidTierLowThresh = new[k,8][[1]],
                         MidTierHighThresh = new[k,9][[1]])
        set(new, i = k, j = 7L, value = x[[1]])
        temp <- x[[2]]
        set(new, i = k, j = 10L, value = temp[Thresholds == eval(x[[1]]), "Utilities"][[1]])
        #print(k/nrow(new))
        #print(k)
      }
      return(new)
    }

    # Inner function for threshold optimizataion
    threshOptim <- function(data,
                            actTar   = 1,
                            predTar  = 2,
                            tpProfit = 1,
                            tnProfit = 5,
                            fpProfit = -1,
                            fnProfit = -1,
                            MidTierDoNothing = FALSE,
                            MidTierCost = -100,
                            MidTierLowThresh = 0.25,
                            MidTierHighThresh = 0.75) {

      # Convert factor target to numeric
      data[, eval(actTar) := as.numeric(as.character(get(actTar)))]

      # Optimize each column's classification threshold ::
      popTrue <- mean(data[[(actTar)]])
      store   <- list()
      j <- 0
      options(warn = -1)
      if(MidTierDoNothing) {
        for (i in c(MidTierHighThresh)) {
          j <- j + 1
          tp      <- sum(ifelse(!(data[[predTar]] < MidTierHighThresh & data[[predTar]] > MidTierLowThresh) & data[[actTar]] == 1 & data[[predTar]] >= i, 1, 0))
          tn      <- sum(ifelse(!(data[[predTar]] < MidTierHighThresh & data[[predTar]] > MidTierLowThresh) & data[[actTar]] == 0 & data[[predTar]] <  i, 1, 0))
          fp      <- sum(ifelse(!(data[[predTar]] < MidTierHighThresh & data[[predTar]] > MidTierLowThresh) & data[[actTar]] == 0 & data[[predTar]] >= i, 1, 0))
          fn      <- sum(ifelse(!(data[[predTar]] < MidTierHighThresh & data[[predTar]] > MidTierLowThresh) & data[[actTar]] == 1 & data[[predTar]] <  i, 1, 0))
          none    <- sum(ifelse(data[[predTar]] <= MidTierHighThresh & data[[predTar]] >= MidTierLowThresh, 1, 0))
          tpr     <- ifelse((tp+fn) == 0, 0, tp / (tp + fn))
          fpr     <- ifelse((fp+tn) == 0, 0, fp / (fp + tn))
          noneRate <- none / nrow(data)
          utility <- (1-noneRate) * (popTrue * (tpProfit*tpr + fnProfit*(1-tpr)) + (1-popTrue) * (fpProfit * fpr + tnProfit * (1-fpr))) + noneRate * MidTierCost
          store[[j]] <- c(i, utility)
        }
        all <- rbindlist(list(store))
        utilities <- melt(all[2,])
        setnames(utilities, "value", "Utilities")
        thresholds <- melt(all[1,])
        setnames(thresholds, "value", "Thresholds")
        results <- cbind(utilities, thresholds)[,c(-1,-3)]
        thresh <- results[Thresholds <= eval(MidTierLowThresh) | Thresholds >= eval(MidTierHighThresh)][order(-Utilities)][1,2][[1]]
        options(warn = 1)
        return(list(thresh, results))
      } else {
        for (i in seq(from = 0.01, to = 0.99, by = 0.01)) {
          j <- j + 1
          tp      <- sum(ifelse(data[[actTar]] == 1 & data[[predTar]] >= i, 1, 0))
          tn      <- sum(ifelse(data[[actTar]] == 0 & data[[predTar]] <  i, 1, 0))
          fp      <- sum(ifelse(data[[actTar]] == 0 & data[[predTar]] >= i, 1, 0))
          fn      <- sum(ifelse(data[[actTar]] == 1 & data[[predTar]] <  i, 1, 0))
          tpr     <- ifelse((tp+fn) == 0, 0, tp / (tp + fn))
          fpr     <- ifelse((fp+tn) == 0, 0, fp / (fp + tn))
          utility <- popTrue * (tpProfit*tpr + fnProfit*(1-tpr)) + (1-popTrue) * (fpProfit * fpr + tnProfit * (1-fpr))
          store[[j]] <- c(i, utility)
        }
        all <- rbindlist(list(store))
        utilities <- melt(all[2,])
        setnames(utilities, "value", "Utilities")
        thresholds <- melt(all[1,])
        setnames(thresholds, "value", "Thresholds")
        results <- cbind(utilities, thresholds)[,c(-1,-3)]
        thresh <- results[order(-Utilities)][1,2][[1]]
        options(warn = 1)
        return(list(thresh, results))
      }
    }

    # Run core function
    data <- RedYellowGreenParallel(calibEval,
                                   PredictColNumber  = PredictColNumber, #1,
                                   ActualColNumber   = ActualColNumber, #767,
                                   TruePositiveCost  = TruePositiveCost, #0,
                                   TrueNegativeCost  = TrueNegativeCost, #0,
                                   FalsePositiveCost = FalsePositiveCost, #-1,
                                   FalseNegativeCost = FalseNegativeCost, #-10,
                                   MidTierCost       = MidTierCost, #-5,
                                   new = i)

    # Return data table
    data
  }

  # Shut down cluster
  stopCluster(cl)

  # 3D Scatterplot
  library("scatterplot3d")
  s3d <- scatterplot3d(x = results[["MTLT"]], y = results[["MTHT"]], z = results[["Utility"]],
                       type = "p",
                       color = "#401a50",
                       angle=45,
                       pch = 16,
                       main = paste0("Utility Maximizer - Main Threshold at ", results[order(-Utility)][1,"MTHT"][[1]]),
                       sub = paste0("Lower Thresh = ", results[order(-Utility)][1,"MTLT"][[1]], " and Upper Thresh = ", results[order(-Utility)][1,"MTHT"][[1]]),
                       xlab = "Mid Tier Lower Threshold",
                       ylab = "Mid Tier Higher Threshold",
                       zlab = "Utility")
  model <- lm(results[["Utility"]] ~ results[["MTLT"]] + results[["MTHT"]])
  s3d$plane3d(model)
  N <- nrow(results)
  s3d$points3d(x = results[order(-Utility)][1:(N/100),"MTLT"][[1]],
               y = results[order(-Utility)][1:(N/100),"MTHT"][[1]],
               z = results[order(-Utility)][1:(N/100),"Utility"][[1]],
               col = "#00aa9d", type = "h", pch = 1)
  s3d$points3d(x = results[order(-Utility)][1,"MTLT"][[1]],
               y = results[order(-Utility)][1,"MTHT"][[1]],
               z = results[order(-Utility)][1,"Utility"][[1]],
               col = "black", type = "h", pch = 10)
  return(results)
}

#' Utility maximizing thresholds for binary classification
#'
#' This function will return the utility maximizing threshold for future predictions along with the data generated to estimate the threshold
#'
#' @author Adrian Antico
#' @param data data is the data table you are building the modeling on
#' @param actTar The column name where the actual target variable is located (in binary form)
#' @param predTar The column name where the predicted values are located
#' @param tpProfit This is the utility for generating a true positive prediction
#' @param tnProfit This is the utility for generating a true negative prediction
#' @param fpProfit This is the cost of generating a false positive prediction
#' @param fnProfit This is the cost of generating a false negative prediction
#' @examples
#' library(h2o)
#' library(AdrianModelingTools)
#' library(data.table)
#' library(ggplot2)
#' Correl <- 0.85
#' aa <- data.table(target = runif(10000))
#' aa[, x1 := qnorm(target)]
#' aa[, x2 := runif(10000)]
#' aa[, Independent_Variable1 := log(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable2 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable3 := exp(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable4 := exp(exp(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2))))]
#' aa[, Independent_Variable5 := sqrt(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable6 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^0.10]
#' aa[, Independent_Variable7 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^0.25]
#' aa[, Independent_Variable8 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^0.75]
#' aa[, Independent_Variable9 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^2]
#' aa[, Independent_Variable10 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^4]
#' aa[, ':=' (x1 = NULL, x2 = NULL)]
#' aa[, target := as.factor(ifelse(target < 0.3333, 0,1))]
#' N = 1
#' Construct <- data.table(Targets         = "target",
#'                         Distribution    = "bernoulli",
#'                         Loss            = "auc",
#'                         Quantile        = 0.01,
#'                         ModelName       = "bla",
#'                         Algorithm       = "gbm",
#'                         dataName        = "aa",
#'                         TargetCol       = c("1"),
#'                         FeatureCols     = c("2:10"),
#'                         CreateDate      = Sys.time(),
#'                         GridTune        = FALSE,
#'                         ExportValidData = TRUE,
#'                         ParDep          = 10,
#'                         PD_Data         = "validate",
#'                         ThreshType      = "f1",
#'                         FSC             = 0.001,
#'                         tpProfit        = rep(0,N),
#'                         tnProfit        = rep(0,N),
#'                         fpProfit        = rep(-1,N),
#'                         fnProfit        = rep(-5,N),
#'                         SaveModel       = rep("FALSE",N),
#'                         SaveModelType   = rep("Mojo",N),
#'                         PredsAllData    = rep(TRUE,N),
#'                         TargetEncoding  = rep(NA,N))
#' AutoH20Modeler(Construct,
#'                max_memory = "28G",
#'                ratios = 0.75,
#'                BL_Trees = 500,
#'                nthreads = 5,
#'                model_path = getwd(),
#'                MaxRuntimeSeconds = 3600,
#'                MaxModels = 30)
#' load(paste0(getwd(), "/bla.Rdata"))
#' data <- threshOptim(data     = calibEval,
#'                     actTar   = "target",
#'                     predTar  = "p1",
#'                     tpProfit = 0,
#'                     tnProfit = 0,
#'                     fpProfit = -1,
#'                     fnProfit = -2)
#' optimalThreshold <- data[[1]]
#' allResults       <- data[[2]]
#' ggplot(allResults, aes(x = Thresholds)) +
#'   geom_line(aes(y = allResults[["Utilities"]], color = "red")) +
#'   ChartTheme(Size = 12) +
#'   ylab("Utility") + geom_vline(xintercept = optimalThreshold)
#' @return Optimal threshold and corresponding utilities for the range of thresholds tested
#' @export
threshOptim <- function(data,
                        actTar   = "target",
                        predTar  = "p1",
                        tpProfit = 0,
                        tnProfit = 0,
                        fpProfit = -1,
                        fnProfit = -2) {
  
  # Load libraries
  library(data.table)
  if(!is.data.table(data)) data <- as.data.table(data)

  # Convert factor target to numeric
  data[, eval(actTar) := as.numeric(as.character(get(actTar)))]

  # Optimize each column's classification threshold ::
  popTrue <- mean(data[[(actTar)]])
  store   <- list()
  j <- 0
  options(warn = -1)
  for (i in seq(from = 0.01, to = 0.99, by = 0.01)) {
    j <- j + 1
    tp      <- sum(ifelse(data[[actTar]] == 1 & data[[predTar]] >= i, 1, 0))
    tn      <- sum(ifelse(data[[actTar]] == 0 & data[[predTar]] <  i, 1, 0))
    fp      <- sum(ifelse(data[[actTar]] == 0 & data[[predTar]] >= i, 1, 0))
    fn      <- sum(ifelse(data[[actTar]] == 1 & data[[predTar]] <  i, 1, 0))
    tpr     <- ifelse((tp+fn) == 0, 0, tp / (tp + fn))
    fpr     <- ifelse((fp+tn) == 0, 0, fp / (fp + tn))
    utility <- popTrue * (tpProfit*tpr + fnProfit*(1-tpr)) + (1-popTrue) * (fpProfit * fpr + tnProfit * (1-fpr))
    store[[j]] <- c(i, utility)
  }
  all <- rbindlist(list(store))
  utilities <- melt(all[2,])
  setnames(utilities, "value", "Utilities")
  thresholds <- melt(all[1,])
  setnames(thresholds, "value", "Thresholds")
  results <- cbind(utilities, thresholds)[,c(-1,-3)]
  thresh <- results[order(-Utilities)][1,2][[1]]
  options(warn = 1)
  return(list(thresh, results))
}

#' nlsModelFit is a function for automatically building nls models
#'
#' This function will build models for 9 different nls models, along with a non-parametric monotonic regression and a polynomial regression. The models are evaluated, a winner is picked, and the predicted values are stored in your data table.
#'
#' @author Adrian Antico
#' @param data Data is the data table you are building the modeling on
#' @param y Y is the target variable name in quotes
#' @param x X is the independent variable name in quotes
#' @param monotonic This is a TRUE/FALSE indicator - choose TRUE if you want monotonic regression over polynomial regression
#' @examples
#' # Create Fake Annual Returns Data
#' library(data.table)
#' library(AdrianModelingTools)
#' library(monreg)
#' library(ggplot2)
#' data <- data.table(Variable = seq(1,500,1), Target = rep(1, 500))
#' for (i in as.integer(1:500)) {
#'   if(i == 1) {
#'     var <- data[i, "Variable"][[1]]
#'     set(data, i = i, j = 2L, value = var * (1 + runif(1)/100))
#' } else {
#'     var = data[i-1, "Target"][[1]]
#'     set(data, i = i, j = 2L, value = var * (1 + runif(1)/100))
#'   }
#' }
#'
#' # To keep original values
#' data1 <- copy(data)
#'
#' # Merge and Model data
#' data2 <- merge(data1,
#'                nlsModelFit(data = data, y = "Target", x = "Variable", monotonic = FALSE),
#'                by = "Variable",
#'                all = TRUE)
#'
#' # Plot graphs of predicted vs actual
#' ggplot(data2, aes(x = Variable)) +
#'   geom_line(aes(y = data2[["Target.x"]], color = "blue")) +
#'   geom_line(aes(y = data2[["Target.y"]], color = "red")) +
#'   ChartTheme(Size = 12) + ggtitle("Growth Models") +
#'   ylab("Target Variable") + xlab("Independent Variable")
#' @return A data table with your original column replaced by the nls model predictions
#' @export
nlsModelFit <- function(data, y, x, monotonic = TRUE) {
  
  # Load libraries
  library(data.table)
  
  DATA <- data
  nls_collection <- data.table(ModelName = c("Poly", "Asymp", "AsympOff", "AsympOrig",
                                             "Biexp", "FourParmLog","Gompertz", "Logistic",
                                             "Michal_Menton", "Weilbull"),
                               Accuracy = rep(999,10))
  
  # Convert to data.table if not already
  if(!is.data.table(data)) data <- as.data.table(data)

  xx <- x
  yy <- y
  z <- DATA[, get(xx)][[1]]
  zz <- DATA[, get(yy)][[1]]
  tryCatch({
    if (monotonic == TRUE) {
      tryCatch({
        baseline <- monreg(z,zz,hr = 0.5, hd = 0.5)
        preds    <- baseline$estimation
        preds[preds < 0] <- 0
        val0     <- mean(abs(zz - preds))
        set(nls_collection, 1, 2, value = val0)
      },error=function(x) {return("skip")})
    } else {
      tryCatch({
        baseline <- lm(as.formula(paste(y," ~ poly(",x,",5)", sep="")), data=DATA)
        preds    <- baseline$fitted.values
        preds[preds < 0] <- 0
        val0     <- mean(abs(zz - preds))
        set(nls_collection, 1L, 2L, value = val0)
      },error=function(x) {return("skip")})
    }
  },error=function(x) {return("skip")})
  tryCatch({
    model1 <- nls(as.formula(paste(y," ~ SSasymp(",x,", Asym, R0, lrc)", sep="")), data = DATA)
    preds1 <- fitted(model1, DATA)
    preds1[preds1 < 0] <- 0
    val    <- mean(abs(zz - preds1))
    set(nls_collection, 2L, 2L, value = val1)
  },error=function(x) {return("skip")})

  # Asymp offset model
  tryCatch({
    model2 <- nls(as.formula(paste(y," ~ SSasympOff(",x,", Asym, lrc, c0)", sep="")), data = DATA)
    preds2 <- fitted(model2, DATA)
    preds2[preds2 < 0] <- 0
    va2    <- mean(abs(zz - preds2))
    set(nls_collection, 3L, 2L, value = val2)
  },error=function(x) {return("skip")})

  # Asymp origin model
  tryCatch({
    model3 <- nls(as.formula(paste(y," ~ SSasympOrig(",x,", Asym, lrc)", sep="")), data = DATA)
    preds3 <- fitted(model3, DATA)
    preds3[preds3 < 0] <- 0
    va3    <- mean(abs(zz - preds3))
    set(nls_collection, 4L, 2L, value = val3)
  },error=function(x) {return("skip")})

  # Biexp model
  tryCatch({
    model4 <- nls(as.formula(paste(y," ~ SSbiexp(",x,", A1, lrc1, A2, lrc2)", sep="")), data = DATA)
    preds4 <- fitted(model4, DATA)
    preds4[preds4 < 0] <- 0
    val4   <- mean(abs(zz - preds4))
    set(nls_collection, 5L, 2L, value = val4)
  },error=function(x) {return("skip")})

  # Four parameter logistic model
  tryCatch({
    model5 <- nls(as.formula(paste(y," ~ SSfpl(",x,", A, B, xmid, scal)", sep="")), data = DATA)
    preds5 <- fitted(model5, DATA)
    preds5[preds5 < 0] <- 0
    val5   <- mean(abs(zz - preds5))
    set(nls_collection, 6L, 2L, value = val5)
  },error=function(x) {return("skip")})

  # Gompertz model
  tryCatch({
    model6 <- nls(as.formula(paste(y," ~ SSgompertz(",x,", Asym, b2, b3)", sep="")), data = DATA)
    preds6 <- fitted(model6, DATA)
    preds6[preds6 < 0] <- 0
    val6   <- mean(abs(zz - preds6))
    set(nls_collection, 7L, 2L, value = val6)
  },error=function(x) {return("skip")})

  # Logistic model
  tryCatch({
    model7 <- nls(as.formula(paste(y," ~ SSlogis(",x,", Asym, xmid, scal)", sep="")), data = DATA)
    preds7 <- fitted(model7, DATA)
    preds7[preds7 < 0] <- 0
    val7   <- mean(abs(zz - preds7))
    set(nls_collection, 8L, 2L, value = val7)
  },error=function(x) {return("skip")})

  # Michaelis-Menton model
  tryCatch({
    model8 <- nls(as.formula(paste(y," ~ SSmicmen(",x,", Vm, K)", sep="")), data = DATA)
    preds8 <- fitted(model8, DATA)
    preds8[preds8 < 0] <- 0
    val8   <- mean(abs(zz - preds8))
    set(nls_collection, 9L, 2L, value = val8)
  },error=function(x) {return("skip")})

  # Weibull Growth model
  tryCatch({
    model9 <- nls(as.formula(paste(y," ~ SSweibull(",x,", Asym, Drop, lrc, pwr)", sep="")), data = DATA)
    preds9 <- fitted(model9, DATA)
    preds9[preds9 < 0] <- 0
    val9   <- mean(abs(zz - preds9))
    set(nls_collection, 10L, 2L, value = val9)
  },error=function(x) {return("skip")})

  # Store best model name
  name <- nls_collection[Accuracy != 999][order(Accuracy)][1,1][[1]]

  # Create column using best model
  if(name == nls_collection[10,1][[1]]) {
    DATA[, eval(y) := preds9]
    return(DATA)
  } else if (name == nls_collection[2,1][[1]]) {
    DATA[, eval(y) := preds1]
    return(DATA)
  } else if (name == nls_collection[3,1][[1]]) {
    DATA[, eval(y) := preds2]
    return(DATA)
  } else if (name == nls_collection[4,1][[1]]) {
    DATA[, eval(y) := preds3]
    return(DATA)
  } else if (name == nls_collection[5,1][[1]]) {
    DATA[, eval(y) := preds4]
    return(DATA)
  } else if (name == nls_collection[6,1][[1]]) {
    DATA[, eval(y) := preds5]
    return(DATA)
  } else if (name == nls_collection[7,1][[1]]) {
    DATA[, eval(y) := preds6]
    return(DATA)
  } else if (name == nls_collection[8,1][[1]]) {
    DATA[, eval(y) := preds7]
    return(DATA)
  } else if (name == nls_collection[9,1][[1]]) {
    DATA[, eval(y) := preds8]
    return(DATA)
  } else {
    DATA[, eval(y) := preds]
    return(DATA)
  }
}

#' Multiplot is a function for combining multiple plots
#'
#' Sick of copying this one into your code? Well, not anymore.
#'
#' @author Adrian Antico
#' @param plotlist This is the list of your charts
#' @param rows This is the number of rows in your multiplot
#' @param cols This is the number of columns in your multiplot
#' @examples
#' library(h2o)
#' library(AdrianModelingTools)
#' library(data.table)
#' library(ggplot2)
#' library(grid)
#' Correl <- 0.85
#' aa <- data.table(target = runif(10000))
#' aa[, x1 := qnorm(target)]
#' aa[, x2 := runif(10000)]
#' aa[, Independent_Variable1 := log(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable2 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable3 := exp(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable4 := exp(exp(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2))))]
#' aa[, Independent_Variable5 := sqrt(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable6 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^0.10]
#' aa[, Independent_Variable7 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^0.25]
#' aa[, Independent_Variable8 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^0.75]
#' aa[, Independent_Variable9 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^2]
#' aa[, Independent_Variable10 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^4]
#' aa[, ':=' (x1 = NULL, x2 = NULL)]
#' aa[, target := as.factor(ifelse(target < 0.3333, 0,1))]
#' N = 1
#' Construct <- data.table(Targets         = "target",
#'                         Distribution    = "bernoulli",
#'                         Loss            = "auc",
#'                         Quantile        = 0.01,
#'                         ModelName       = "bla",
#'                         Algorithm       = "gbm",
#'                         dataName        = "aa",
#'                         TargetCol       = c("1"),
#'                         FeatureCols     = c("2:10"),
#'                         CreateDate      = Sys.time(),
#'                         GridTune        = FALSE,
#'                         ExportValidData = TRUE,
#'                         ParDep          = 10,
#'                         PD_Data         = "validate",
#'                         ThreshType      = "f1",
#'                         FSC             = 0.001,
#'                         tpProfit        = rep(0,N),
#'                         tnProfit        = rep(0,N),
#'                         fpProfit        = rep(-1,N),
#'                         fnProfit        = rep(-5,N),
#'                         SaveModel       = rep("FALSE",N),
#'                         SaveModelType   = rep("Mojo",N),
#'                         PredsAllData    = rep(TRUE,N),
#'                         TargetEncoding  = rep(NA,N))
#' AutoH20Modeler(Construct,
#'                max_memory = "28G",
#'                ratios = 0.75,
#'                BL_Trees = 500,
#'                nthreads = 5,
#'                model_path = getwd(),
#'                MaxRuntimeSeconds = 3600,
#'                MaxModels = 30)
#' load(paste0(getwd(), "/bla_ParDepCalPlots.Rdata"))
#' p1 <- calibr[[1]]
#' p2 <- calibr[[2]]
#' p3 <- calibr[[3]]
#' p4 <- calibr[[4]]
#' multiplot(plotlist = list(p1,p2,p3,p4), cols = 2)
#' @return An object to pass along to ggplot objects following the "+" sign
#' @export
multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  require(grid)

  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots == 1) {
    print(plots[[1]])

  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#' ChartTheme function is a ggplot theme generator for ggplots
#'
#' This function helps your ggplots look professional with the choice of the two main colors that will dominate the theme
#'
#' @author Adrian Antico
#' @param Size The size of the axis labels and title
#' @examples
#' p <- ggplot(data, aes(x = weeks, y = quantity)) + geom_line()
#' p <- p + ChartTheme(Size = 12)
#' p
#' @return An object to pass along to ggplot objects following the "+" sign
#' @export
ChartTheme <- function(Size = 12) {
  chart_theme <- theme(plot.background = element_rect(fill = "gray94"),
                       panel.background = element_rect(fill = "lightsteelblue1", colour = "darkblue", size = 0.25, color = "darkblue"),
                       panel.grid.major = element_line(colour = "darkblue", size=0.01, color = "white", linetype = 1),
                       panel.grid.minor = element_line(colour = "darkblue", size=0.01, color= "white", linetype = 1),
                       legend.position = "bottom",
                       legend.title = element_text(color = "darkblue", size=Size, face = "bold"),
                       legend.background = element_rect(fill = "gray95",size = 1, linetype = "solid", color = "darkblue"),
                       plot.title=element_text(color = "darkblue", size=Size, face = "bold"),
                       axis.title=element_text(color = "darkblue", size=Size, face = "bold"),
                       axis.text=element_text(colour="darkblue", face = "bold", angle = 90),
                       axis.title.x = element_text(margin = margin(t = 20, r = 20, b = 20, l = 20)),
                       axis.title.y = element_text(margin = margin(t = 20, r = 20, b = 20, l = 20)),
                       panel.border = element_rect(colour = "darkblue", fill = NA, size = 1.5))
  chart_theme
}

#' Percentile rank function
#'
#' This function computes percentile ranks for each row in your data like Excel's PERCENT_RANK
#'
#' @author Adrian Antico
#' @param x X is your variable of interest
#' @examples
#' percRank(x)
#' @return vector of percentile ranks
#' @export
percRank <- function(x) trunc(rank(x))/length(x)

#' Function automatically builds partial dependence calibration plots for model evaluation
#'
#' This function automatically builds partial dependence calibration plots and partial dependence calibration boxplots for model evaluation using regression, quantile regression, and binary and multinomial classification
#' @author Adrian Antico
#'
#' @param data Data containing predicted values and actual values for comparison
#' @param PredColName String representation of the column name with predicted values from model
#' @param ActColName String representation of the column name with actual values from model
#' @param IndepVar String representation of the column name with the independent variable of choice
#' @param type Calibration or boxplot - calibration aggregated data based on summary statistic; boxplot shows variation
#' @param bucket Number of buckets to partition the space on (0,1) for evaluation
#' @param FactLevels The number of levels to show on the chart (1. Levels are chosen based on frequency; 2. all other levels grouped and labeled as "Other")
#' @param Function Supply the function you wish to use for aggregation.
#' @return Partial dependence calibration plot or boxplot
#' @examples
#' library(h2o)
#' library(AdrianModelingTools)
#' library(data.table)
#' library(ggplot2)
#' Correl <- 0.85
#' aa <- data.table(target = runif(10000))
#' aa[, x1 := qnorm(target)]
#' aa[, x2 := runif(10000)]
#' aa[, Independent_Variable1 := log(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable2 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable3 := exp(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable4 := exp(exp(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2))))]
#' aa[, Independent_Variable5 := sqrt(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable6 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^0.10]
#' aa[, Independent_Variable7 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^0.25]
#' aa[, Independent_Variable8 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^0.75]
#' aa[, Independent_Variable9 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^2]
#' aa[, Independent_Variable10 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^4]
#' aa[, ':=' (x1 = NULL, x2 = NULL)]
#' N = 1
#' Construct <- data.table(Targets         = "target",
#'                         Distribution    = "gaussian",
#'                         Loss            = "MSE",
#'                         Quantile        = 0.01,
#'                         ModelName       = "bla",
#'                         Algorithm       = "gbm",
#'                         dataName        = "aa",
#'                         TargetCol       = c("1"),
#'                         FeatureCols     = c("2:10"),
#'                         CreateDate      = Sys.time(),
#'                         GridTune        = FALSE,
#'                         ExportValidData = TRUE,
#'                         ParDep          = 10,
#'                         PD_Data         = "validate",
#'                         ThreshType      = "f1",
#'                         FSC             = 0.001,
#'                         tpProfit        = rep(0,N),
#'                         tnProfit        = rep(0,N),
#'                         fpProfit        = rep(-1,N),
#'                         fnProfit        = rep(-5,N),
#'                         SaveModel       = rep("FALSE",N),
#'                         SaveModelType   = rep("Mojo",N),
#'                         PredsAllData    = rep(TRUE,N),
#'                         TargetEncoding  = rep(NA,N))
#' AutoH20Modeler(Construct,
#'                max_memory = "28G",
#'                ratios = 0.75,
#'                BL_Trees = 500,
#'                nthreads = 5,
#'                model_path = getwd(),
#'                MaxRuntimeSeconds = 3600,
#'                MaxModels = 30)
#' load(paste0(getwd(), "/bla.Rdata"))
#' ParDepCalPlots(data  = calibEval,
#'                PredColName = "predict",
#'                ActColName  = "target",
#'                IndepVar    = "Independent_Variable9",
#'                type        = "calibration",
#'                bucket      = 0.05,
#'                FactLevels  = 10,
#'                Function    = function(x) mean(x, na.rm = TRUE))
#' ParDepCalPlots(data  = calibEval,
#'                PredColName = "predict",
#'                ActColName  = "target",
#'                IndepVar    = "Independent_Variable9",
#'                type        = "boxplot",
#'                bucket      = 0.05,
#'                FactLevels  = 10,
#'                Function    = function(x) mean(x, na.rm = TRUE))
#' @export
ParDepCalPlots <- function(data,
                           PredColName = "PredictedValues",
                           ActColName  = "ActualValues",
                           IndepVar    = "Independent_Variable_Name",
                           type        = "calibration",
                           bucket      = 0.05,
                           FactLevels  = 10,
                           Function    = function(x) mean(x, na.rm = TRUE)) {
                             
  # Load libraries
  library(data.table)
  library(ggplot2)

  # Turn off ggplot2 warnings
  options(warn = -1)

  # Build buckets by independent variable of choice
  preds2 <- as.data.table(data)

  # Subset columns
  cols <- c(PredColName, ActColName, IndepVar)
  preds2 <- preds2[, ..cols]

  # Structure data
  cols <- c(PredColName, ActColName, IndepVar)
  data <- data[, ..cols]
  setcolorder(data, c(PredColName, ActColName, IndepVar))

  # If actual is in factor form, convert to numeric and coerce type to calibration
  if(!is.numeric(preds2[[ActColName]])) {
    preds2[, eval(ActColName) := as.numeric(as.character(get(ActColName)))]
    type <- "calibration"
  }

  # Prepare for both calibration and boxplot
  if (is.numeric(preds2[[IndepVar]]) || is.integer(preds2[[IndepVar]])) {
    preds2[, rank := 100*(round(percRank(preds2[[IndepVar]])/bucket)*bucket)]
  } else {
    type <- "FactorVar"
    preds2[, id := seq_len(.N), by = get(IndepVar)]
    #preds2 <- preds2[, ]
    preds2 <- preds2[, .(Function(get(ActColName)), Function(get(PredColName)), max(id)), by = get(IndepVar)][order(-V3)]
    if(nrow(preds2) > FactLevels) {
      temp1 <- preds2[1:FactLevels][, V3 := NULL]
      temp2 <- preds2[(FactLevels+1):nrow(preds2)]
      temp2[, ':=' (V1 = V1 * V3 / sum(V3),
                    V2 = V2 * V3 / sum(V3))]
      temp3 <- temp2[, .(sum(V1), sum(V2))]
      temp3[, get := "Other"]
      setcolorder(temp3, c(3,1,2))
    }
    preds2[, V3 := NULL]
    if(nrow(preds2) > FactLevels) {
      preds3 <- rbindlist(list(temp1, temp3))
    } else {
      preds3 <- preds2
    }
    setnames(preds3, old = c("get","V1","V2"), new = c(IndepVar, ActColName, PredColName))
    preds3 <- preds3[order(-get(PredColName))]
  }

  # Build plots
  if (type == "calibration") {
    # Aggregate by rank for calibration
    preds3 <- preds2[, lapply(.SD, noquote(Function)), by = rank][order(rank)]
    preds3[, eval(IndepVar) := as.numeric(get(IndepVar))]

    # Partial dependence calibration plot
    plot <- ggplot(preds3, aes(x = preds3[[IndepVar]])) +
      geom_line(aes(y = preds3[[PredColName]], color = "Predicted")) +
      geom_line(aes(y = preds3[[ActColName]], color = "Actuals")) +
      ylab("Actual / Predicted") + xlab(IndepVar) +
      scale_colour_manual("",
                          breaks = c("Actuals", "Predicted"),
                          values = c("blue", "red")) +
      ChartTheme(Size = 15) + ggtitle("Partial Dependence Calibration Plot")
    #plot <- plotly_build(ggplotly(plot))
  } else if (type == "boxplot"){
    # Partial dependence boxplot
    keep <- c("rank", ActColName, IndepVar)
    actual <- preds2[, ..keep]
    actual[, Type := "actual"]
    setnames(actual, ActColName, "Output")

    keep <- c("rank", PredColName, IndepVar)
    predicted <- preds2[, ..keep]
    predicted[, Type := "predicted"]
    setnames(predicted, PredColName, "Output")

    data <- rbindlist(list(actual, predicted))[order(rank)]
    data[, rank := as.factor(rank)]
    data <- data[ , eval(IndepVar) := as.numeric(get(IndepVar))]
    data <- data[ , eval(IndepVar) := round(Function(get(IndepVar)),3), by = rank]
    data[, eval(IndepVar) := as.factor(get(IndepVar))]
    data[, rank := NULL]
    plot <- ggplot(data, aes(x = data[[IndepVar]], y = Output)) +
      geom_boxplot(aes(fill = Type)) + scale_fill_manual(values = c("red", "blue")) +
      ggtitle("Partial Dependence Calibration Boxplot") +
      xlab(eval(IndepVar)) +
      ChartTheme(Size= 15)
    #plot <- plotly_build(ggplotly(plot))
  } else if (type == "FactorVar") {
    keep <- c(IndepVar, ActColName)
    actual <- preds3[, ..keep]
    actual[, Type := "actual"]
    setnames(actual, ActColName, "Output")

    keep <- c(IndepVar, PredColName)
    predicted <- preds3[, ..keep]
    predicted[, Type := "predicted"]
    setnames(predicted, PredColName, "Output")
    data <- rbindlist(list(actual, predicted))[order(-Output)]

    plot <- ggplot(data, aes(x = data[[IndepVar]], y = Output)) +
      geom_bar(stat="identity", position="dodge", aes(fill = Type)) + scale_fill_manual(values = c("red", "blue")) +
      ggtitle("Partial Dependence Calibration Barplot") +
      xlab(eval(IndepVar)) +
      ChartTheme(Size= 15)
    #plot <- plotly_build(ggplotly(plot))
  }
  return(plot)
}

#' Function automatically builds calibration plots for model evaluation
#'
#' This function automatically builds calibration plots and calibration boxplots for model evaluation using regression, quantile regression, and binary and multinomial classification
#' @author Adrian Antico
#' @param data Data containing predicted values and actual values for comparison
#' @param PredColName String representation of column name with predicted values from model
#' @param ActColName String representation of column name with actual values from model
#' @param type Calibration or boxplot - calibration aggregated data based on summary statistic; boxplot shows variation
#' @param bucket Number of buckets to partition the space on (0,1) for evaluation
#' @param aggrfun The statistics function used in aggregation, listed as a function
#' @return Calibration plot or boxplot
#' @examples
#' library(h2o)
#' library(AdrianModelingTools)
#' library(data.table)
#' library(ggplot2)
#' Correl <- 0.85
#' aa <- data.table(target = runif(10000))
#' aa[, x1 := qnorm(target)]
#' aa[, x2 := runif(10000)]
#' aa[, Independent_Variable1 := log(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable2 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable3 := exp(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable4 := exp(exp(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2))))]
#' aa[, Independent_Variable5 := sqrt(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable6 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^0.10]
#' aa[, Independent_Variable7 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^0.25]
#' aa[, Independent_Variable8 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^0.75]
#' aa[, Independent_Variable9 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^2]
#' aa[, Independent_Variable10 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^4]
#' aa[, ':=' (x1 = NULL, x2 = NULL)]
#' N = 1
#' Construct <- data.table(Targets         = "target",
#'                         Distribution    = "gaussian",
#'                         Loss            = "MSE",
#'                         Quantile        = 0.01,
#'                         ModelName       = "bla",
#'                         Algorithm       = "gbm",
#'                         dataName        = "aa",
#'                         TargetCol       = c("1"),
#'                         FeatureCols     = c("2:10"),
#'                         CreateDate      = Sys.time(),
#'                         GridTune        = FALSE,
#'                         ExportValidData = TRUE,
#'                         ParDep          = 10,
#'                         PD_Data         = "validate",
#'                         ThreshType      = "f1",
#'                         FSC             = 0.001,
#'                         tpProfit        = rep(0,N),
#'                         tnProfit        = rep(0,N),
#'                         fpProfit        = rep(-1,N),
#'                         fnProfit        = rep(-5,N),
#'                         SaveModel       = rep("FALSE",N),
#'                         SaveModelType   = rep("Mojo",N),
#'                         PredsAllData    = rep(TRUE,N),
#'                         TargetEncoding  = rep(NA,N))
#' AutoH20Modeler(Construct,
#'                max_memory = "28G",
#'                ratios = 0.75,
#'                BL_Trees = 500,
#'                nthreads = 5,
#'                model_path = getwd(),
#'                MaxRuntimeSeconds = 3600,
#'                MaxModels = 30)
#' load(paste0(getwd(), "/bla.Rdata"))
#' EvalPlot(calibEval,
#'          PredColName = "predict",
#'          ActColName  = "target",
#'          type        = "calibration",
#'          bucket      = 0.05,
#'          aggrfun     = function(x) quantile(x, probs = 0.50, na.rm = TRUE))
#' @export
EvalPlot <- function(data,
                     PredColName = "PredictedValues",
                     ActColName  = "ActualValues",
                     type        = c("calibration","boxplot"),
                     bucket      = 0.05,
                     aggrfun     = function(x) mean(x, na.rm = TRUE)) {

  # Load libraries
  library(data.table)
  library(ggplot2)

  # Turn data into data.table if not already
  if(!is.data.table(data)) data <- as.data.table(data)

  # Structure data
  cols <- c(eval(PredColName), eval(ActColName))
  data <- data[, ..cols]
  setcolorder(data, c(PredColName, ActColName))
  setnames(data, c(PredColName,ActColName), c("preds","acts"))

  # If actual is in factor form, convert to numeric and coerce type to calibration
  if(!is.numeric(data[["acts"]])) {
    data[, acts := as.numeric(as.character(acts))]
    type <- "calibration"
  }

  # Add a column that ranks predicted values
  data[, rank := 100*(round(percRank(data[[1]])/bucket)*bucket)]

  # Plot
  if(type == "boxplot") {
    # Remove classification and non-event predicted values
    data[, rank := as.factor(rank)]

    cols <- c("rank","preds")
    zz1 <- data[, ..cols]
    zz1[, Type := 'predicted']
    setnames(zz1, c("preds"), c("output"))

    cols <- c("rank","acts")
    zz2 <- data[, ..cols]
    zz2[, Type := 'actual']
    setnames(zz2, c("acts"), c("output"))

    data <- rbindlist(list(zz1,zz2))

    plot <- ggplot(data, aes(x=rank, y=output, fill=Type)) +
      geom_boxplot(outlier.color = "red", color = "black") +
      ggtitle("Calibration Evaluation Boxplot") +
      xlab("Predicted Percentile") + ylab("Observed Values") +
      ChartTheme(Size= 15)

  } else {
    # Aggregate all columns by rank, utilizing mean as the aggregator statistic
    data <- data[, lapply(.SD, noquote(aggrfun)), by=rank]

    # Build calibration plot
    plot  <- ggplot(data, aes(x=rank))  +
      geom_line(aes(y=data[[3]], colour="Actual")) +
      geom_line(aes(y=data[[2]], colour="Predicted")) +
      xlab("Predicted Percentile") + ylab("Observed Values") +
      scale_color_manual(values=c("red", "blue")) +
      theme(axis.text.x=element_text(angle=90, hjust=1)) +
      theme(legend.position="bottom") +
      ggtitle("Calibration Evaluation Plot") +
      ChartTheme(Size= 15)
  }
  return(plot)
}

#' An Automated Feature Engineering Function
#'
#' Builds autoregressive and rolling stats from target columns and distributed lags and distributed rolling stats for independent features distributed across time. On top of that, you can also create time between instances along with their associated lags and rolling stats. This function works for data with groups and without groups.
#' @author Adrian Antico
#' @param data The data source you want to run the function on
#' @param lags The list of specific lags you want to have generated
#' @param statsFuns List of functions for your rolling windows, such as mean, sd, min, max, quantile
#' @param statsNames The corresponding names to append to your colnames created associated with statsFuns
#' @param targets The column(s) in which you will build your lags and rolling stats
#' @param groupingVars Categorical variables you will build your lags and rolling stats by
#' @param sortDateName String name of your core date column in your transaction data
#' @param timeDiffTarget List a name in order to create time between events with assiciated lags and rolling features
#' @param timeAgg Unit of time to aggregate by
#' @param WindowingLag   Build moving stats off of target column(s) or one of their lags (1+)
#' @param Type input "Lag" if you want features built on historical values; use "Lead" if you want features built on future values
#' @param Timer Set to TRUE if you want a time run for the operation; useful when there is grouping
#' @param SkipCols Defaults to NULL; otherwise name the vector containing the names of columns to skip
#' @param SimpleImpute Set to TRUE for factor level imputation of "0" and numeric imputation of -1
#' @return data.table of original data plus newly created features
#' @examples
#' quick_model <- GDL_Feature_Engineering(quick_model,
#'                                        lags           = c(seq(1,5,1)),
#'                                        periods        = c(3,5,10,15,20,25),
#'                                        statsFUNs      = c(function(x) quantile(x, probs = 0.0, na.rm = TRUE),
#'                                                           function(x) quantile(x, probs = 1.0, na.rm = TRUE),
#'                                                           function(x) mean(x, na.rm = TRUE),
#'                                                           function(x) sd(x, na.rm = TRUE),
#'                                                           function(x) quantile(x, probs = 0.2, na.rm = TRUE),
#'                                                           function(x) quantile(x, probs = 0.8, na.rm = TRUE)),
#'                                        statsNames     = c("min","max","mean","sd","q20","q80"),
#'                                        targets        = c("qty","price"),
#'                                        groupingVars   = c("SKU","VENDOR_NAME"),
#'                                        sortDateName   = "RECEIPT_DATE",
#'                                        timeDiffTarget = c("ISSUE_GAP"),
#'                                        timeAgg        = c("auto","secs","mins","hours","days","weeks"),
#'                                        WindowingLag   = 0,
#'                                        Type           = "Lag",
#'                                        Timer          = TRUE,
#'                                        SkipCols       = FALSE,
#'                                        SimpleImpute   = TRUE)
#' @export
GDL_Feature_Engineering <- function(data,
                                    lags           = c(seq(1,5,1)),
                                    periods        = c(3,5,10,15,20,25),
                                    statsFUNs      = c(function(x) quantile(x, probs = 0.1, na.rm = TRUE),
                                                       function(x) quantile(x, probs = 0.9, na.rm = TRUE),
                                                       function(x) mean(x, na.rm = TRUE),
                                                       function(x) sd(x, na.rm = TRUE),
                                                       function(x) quantile(x, probs = 0.25, na.rm = TRUE),
                                                       function(x) quantile(x, probs = 0.75, na.rm = TRUE)),
                                    statsNames     = c("q10","q90","mean","sd","q25","q75"),
                                    targets        = c("qty"),
                                    groupingVars   = c("Group1","Group2"),
                                    sortDateName   = "date",
                                    timeDiffTarget = c("TimeDiffName"),
                                    timeAgg        = c("auto","secs","mins","hours","days","weeks"),
                                    WindowingLag   = 0,
                                    Type           = c("Lag","Lead"),
                                    Timer          = TRUE,
                                    SkipCols       = NULL,
                                    SimpleImpute   = TRUE) {

  # Load library
  library(data.table)
  library(zoo)
                                      
  # Convert to data.table if not already
  if(!is.data.table(data)) data <- as.data.table(data)

  # Set up counter for countdown
  CounterIndicator = 0
  if (!is.null(timeDiffTarget)) {
    tarNum <- length(targets) + 1
  } else {
    tarNum <- length(targets)
  }

  # Define total runs
  if (!is.null(groupingVars)) {
    runs <- length(groupingVars) * length(periods) * length(statsNames) * tarNum + length(lags)
  } else {
    runs <- length(periods) * length(statsNames) * tarNum
  }

  # Begin feature engineering
  if(!is.null(groupingVars)) {
    for (i in seq_along(groupingVars)) {

      # Sort data
      if(tolower(Type) == "lag") {
        colVar <- c(groupingVars[i],sortDateName[1])
        setorderv(data, colVar, order = 1)
      } else {
        colVar <- c(groupingVars[i],sortDateName[1])
        setorderv(data, colVar, order = -1)
      }

      # Lags
      for(l in seq_along(lags)) {
        for (t in targets) {
          if(!(paste0(groupingVars[i],"_LAG_",lags[l],"_",t) %in% SkipCols)) {
            data[, paste0(groupingVars[i],"_LAG_",lags[l],"_",t) := data.table::shift(get(t), n = lags[l], type = "lag"), by = get(groupingVars[i])]
          }
        }
      }

      # Time lags
      if(!is.null(timeDiffTarget)) {

        # Lag the dates first
        for(l in seq_along(lags)) {
          if(!(paste0(groupingVars[i],"TEMP",lags[l]) %in% SkipCols)) {
            data[, paste0(groupingVars[i],"TEMP",lags[l]) := data.table::shift(get(sortDateName), n = lags[l], type = "lag"), by = get(groupingVars[i])]
          }
        }

        # Difference the lag dates
        if(WindowingLag != 0) {
          for(l in seq_along(lags)) {
            if(!(paste0(timeDiffTarget,lags[l]) %in% SkipCols) || l == 1) {
              data[, paste0(timeDiffTarget,lags[l]) := as.numeric(
                difftime(
                  get(paste0(groupingVars[i],"TEMP",(lags[l-1]))),
                  get(paste0(groupingVars[i],"TEMP",lags[l])),
                  units = eval(timeAgg))), by = get(groupingVars[i])]
            }
          }
        } else {
          for(l in seq_along(lags)) {
            if (l == 1) {
              if(!(paste0(groupingVars[i],timeDiffTarget,lags[l]) %in% SkipCols)) {
                data[, paste0(groupingVars[i],timeDiffTarget,lags[l]) := as.numeric(
                  difftime(get(sortDateName),
                           get(paste0(groupingVars[i],"TEMP",lags[l])),
                           units = eval(timeAgg))), by = get(groupingVars[i])]
              }
            } else {
              if(!(paste0(groupingVars[i],timeDiffTarget,lags[l]) %in% SkipCols)) {
                data[, paste0(groupingVars[i],timeDiffTarget,lags[l]) := as.numeric(
                  difftime(
                    get(paste0(groupingVars[i],"TEMP",(lags[l-1]))),
                    get(paste0(groupingVars[i],"TEMP",lags[l])),
                    units = eval(timeAgg))), by = get(groupingVars[i])]
              }
            }
          }
        }

        # Remove temporary lagged dates
        for (l in seq_along(lags)) {
          data[, paste0(groupingVars[i],"TEMP",lags[l]) := NULL]
        }

        # Store new target
        timeTarget <- paste0(groupingVars[i],timeDiffTarget,"1")
      }

      # Define targets
      if(WindowingLag != 0) {
        if (!is.null(timeDiffTarget)) {
          targets <- c(paste0(groupingVars[i],"_LAG_",WindowingLag,"_",targets), timeTarget)
        } else {
          targets <- c(paste0(groupingVars[i],"_LAG_",WindowingLag,"_",targets))
        }
      } else {
        if (!is.null(timeDiffTarget)) {
          targets <- c(targets, timeTarget)
        } else {
          targets <- targets
        }
      }

      # Moving stats
      for (j in seq_along(periods)) {
        for (k in seq_along(statsNames)) {
          for (t in targets) {
            if(!(paste0(groupingVars[i],statsNames[k],"_",periods[j],"_",t) %in% SkipCols)) {
              data[, paste0(groupingVars[i],statsNames[k],"_",periods[j],"_",t) := zoo::rollapply(get(t), periods[j], statsFUNs[k][[1]], partial = TRUE),
                   by = get(groupingVars[i])]
              CounterIndicator = CounterIndicator + 1
              if(Timer) {
                print(CounterIndicator / runs)
              }
            }
          }
        }
      }
    }

    # Replace any inf values with NA
    for (col in seq_along(data)) {
      set(data, j = col, value = replace(data[[col]], is.infinite(data[[col]]),NA))
    }

    # Turn character columns into factors
    for (col in seq_along(data)) {
      if(is.character(data[[col]])) {
        set(data, j = col, value = as.factor(data[[col]]))
      }
    }

    # Impute missing values
    if(SimpleImpute) {
      for (j in seq_along(data)) {
        if(is.factor(data[[j]])) {
          set(data,which(!(data[[j]] %in% levels(data[[j]]))),j,"0")
        } else {
          set(data,which(is.na(data[[j]])),j,-1)
        }
      }
    }

    # Done!!
    return(data)

  } else {
    if (tolower(Type) == "lag") {
      colVar <- c(sortDateName[1])
      setorderv(data, colVar, order = 1)
    } else {
      colVar <- c(sortDateName[1])
      setorderv(data, colVar, order = -1)
    }

    # Lags
    for(l in seq_along(lags)) {
      for (t in targets) {
        if(!(paste0("LAG_",lags[l],"_",t) %in% SkipCols)) {
          data[, paste0("LAG_",lags[l],"_",t) := data.table::shift(get(t), n = lags[l], type = "lag")]
        }
      }
    }

    # Time lags
    if(!is.null(timeDiffTarget)) {

      # Lag the dates first
      for(l in seq_along(lags)) {
        if(!(paste0("TEMP",lags[l]) %in% SkipCols)) {
          data[, paste0("TEMP",lags[l]) := data.table::shift(get(sortDateName), n = lags[l], type = "lag")]
        }
      }

      # Difference the lag dates
      if(WindowingLag != 0) {
        for(l in seq_along(lags)) {
          if(!(paste0(timeDiffTarget,"_",lags[l]) %in% SkipCols)) {
            data[, paste0(timeDiffTarget,"_",lags[l]) := as.numeric(
              difftime(
                get(paste0("TEMP",(lags[l]-1))),
                get(paste0("TEMP",lags[l])),
                units = eval(timeAgg)))]
          }
        }
      } else {
        for(l in seq_along(lags)) {
          if (l == 1) {
            if(!(paste0(timeDiffTarget,"_",lags[l]) %in% SkipCols)) {
              data[, paste0(timeDiffTarget,"_",lags[l]) := as.numeric(
                difftime(get(sortDateName),
                         get(paste0("TEMP",lags[l])),
                         units = eval(timeAgg)))]
            }
          } else {
            if(!(paste0(timeDiffTarget,"_",lags[l]) %in% SkipCols)) {
              data[, paste0(timeDiffTarget,"_",lags[l]) := as.numeric(
                difftime(
                  get(paste0("TEMP",(lags[l-1]))),
                  get(paste0("TEMP",lags[l])),
                  units = eval(timeAgg)))]
            }
          }
        }
      }

      # Remove temporary lagged dates
      for (l in seq_along(lags)) {
        data[, paste0("TEMP",lags[l]) := NULL]
      }

      # Store new target
      timeTarget <- paste0(timeDiffTarget,"_1")
    }

    # Define targets
    if(WindowingLag !=0) {
      if (!is.null(timeDiffTarget)) {
        targets <- c(paste0("LAG_",WindowingLag,"_",targets), timeTarget)
      } else {
        targets <- c(paste0("LAG_",WindowingLag,"_",targets))
      }
    } else {
      if(!is.null(timeDiffTarget)) {
        targets <- c(targets, timeTarget)
      } else {
        targets <- targets
      }
    }

    # Define targets
    if(WindowingLag != 0) {
      if (!is.null(timeDiffTarget)) {
        targets <- c(paste0(groupingVars[i],"_LAG_",WindowingLag,"_",targets), timeTarget)
      } else {
        targets <- c(paste0(groupingVars[i],"_LAG_",WindowingLag,"_",targets))
      }
    } else {
      if (!is.null(timeDiffTarget)) {
        targets <- c(targets, timeTarget)
      } else {
        targets <- targets
      }
    }

    # Moving stats
    for (j in seq_along(periods)) {
      for (k in seq_along(statsNames)) {
        for (t in targets) {
          if(!(paste0(statsNames[k],"_",periods[j],"_",t) %in% SkipCols)) {
            data[, paste0(statsNames[k],"_",periods[j],"_",t) := zoo::rollapply(get(t), periods[j], statsFUNs[k][[1]], partial = TRUE)]
            CounterIndicator = CounterIndicator + 1
            if(Timer) {
              print(CounterIndicator / runs)
            }
          }
        }
      }
    }

    # Replace any inf values with NA
    for (col in seq_along(data)) {
      set(data, j = col, value = replace(data[[col]], is.infinite(data[[col]]),NA))
    }

    # Turn character columns into factors
    for (col in seq_along(data)) {
      if(is.character(data[[col]])) {
        set(data, j = col, value = as.factor(data[[col]]))
      }
    }

    # Impute missing values
    if(SimpleImpute) {
      for (j in seq_along(data)) {
        if(is.factor(data[[j]])) {
          set(data,which(!(data[[j]] %in% levels(data[[j]]))),j,"0")
        } else {
          set(data,which(is.na(data[[j]])),j,-1)
        }
      }
    }

    # Done!!
    return(data)
  }
}

#' An Automated Feature Engineering Function Using data.table frollmean
#'
#' Builds autoregressive and moving average from target columns and distributed lags and distributed moving average for independent features distributed across time. On top of that, you can also create time between instances along with their associated lags and moving averages. This function works for data with groups and without groups.
#' @author Adrian Antico
#' @param data The data source you want to run the function on
#' @param lags The list of specific lags you want to have generated
#' @param statsNames The corresponding names to append to your colnames created associated with statsFuns
#' @param targets The column(s) in which you will build your lags and rolling stats
#' @param groupingVars Categorical variables you will build your lags and rolling stats by
#' @param sortDateName String name of your core date column in your transaction data
#' @param timeDiffTarget List a name in order to create time between events with assiciated lags and rolling features
#' @param timeAgg Unit of time to aggregate by
#' @param WindowingLag   Build moving stats off of target column(s) or one of their lags (1+)
#' @param Type input "Lag" if you want features built on historical values; use "Lead" if you want features built on future values
#' @param Timer Set to TRUE if you want a time run for the operation; useful when there is grouping
#' @param SkipCols Defaults to NULL; otherwise name the vector containing the names of columns to skip
#' @param SimpleImpute Set to TRUE for factor level imputation of "0" and numeric imputation of -1
#' @return data.table of original data plus newly created features
#' @examples
#' quick_model <- DT_GDL_Feature_Engineering(quick_model,
#'                                           lags           = c(seq(1,5,1)),
#'                                           periods        = c(3,5,10,15,20,25),
#'                                           statsNames     = c("MA"),
#'                                           targets        = c("qty","price"),
#'                                           groupingVars   = c("SKU","VENDOR_NAME"),
#'                                           sortDateName   = "RECEIPT_DATE",
#'                                           timeDiffTarget = c("ISSUE_GAP"),
#'                                           timeAgg        = c("auto","secs","mins","hours","days","weeks"),
#'                                           WindowingLag   = 0,
#'                                           Type           = "Lag",
#'                                           Timer          = TRUE,
#'                                           SkipCols       = FALSE,
#'                                           SimpleImpute   = TRUE)
#' @export
DT_GDL_Feature_Engineering <- function(data,
                                       lags           = c(seq(1,50,1)),
                                       periods        = c(seq(5,95,5)),
                                       statsNames     = c("MA"),
                                       targets        = c("qty"),
                                       groupingVars   = c("Group1","Group2"),
                                       sortDateName   = "date",
                                       timeDiffTarget = c("TimeDiffName"),
                                       timeAgg        = c("auto","secs","mins","hours","days","weeks"),
                                       WindowingLag   = 0,
                                       Type           = c("Lag","Lead"),
                                       Timer          = TRUE,
                                       SkipCols       = NULL,
                                       SimpleImpute   = TRUE) {

  # Load libraries
  library(data.table)
  
  # Convert to data.table if not already
  if(!is.data.table(data)) data <- as.data.table(data)

  # Ensure target is numeric
  data[, eval(targets) := as.numeric(get(targets))]

  # Set up counter for countdown
  CounterIndicator = 0
  if (!is.null(timeDiffTarget)) {
    tarNum <- length(targets) + 1
  } else {
    tarNum <- length(targets)
  }

  # Define total runs
  if (!is.null(groupingVars)) {
    runs <- length(groupingVars) * tarNum * (length(periods) * length(statsNames) + length(lags))
  } else {
    runs <- tarNum * (length(periods) * length(statsNames) + length(lags))
  }

  # Begin feature engineering
  if(!is.null(groupingVars)) {
    for (i in seq_along(groupingVars)) {
      Targets <- targets

      # Sort data
      if(tolower(Type) == "lag") {
        colVar <- c(groupingVars[i],sortDateName[1])
        setorderv(data, colVar, order = 1)
      } else {
        colVar <- c(groupingVars[i],sortDateName[1])
        setorderv(data, colVar, order = -1)
      }

      # Lags
      for(l in seq_along(lags)) {
        for (t in Targets) {
          if(!(paste0(groupingVars[i],"_LAG_",lags[l],"_",t) %in% SkipCols)) {
            data[, paste0(groupingVars[i],"_LAG_",lags[l],"_",t) := data.table::shift(get(t), n = lags[l], type = "lag"), by = get(groupingVars[i])]
            CounterIndicator = CounterIndicator + 1
            if(Timer) {
              print(CounterIndicator / runs)
            }
          }
        }
      }

      # Time lags
      if(!is.null(timeDiffTarget)) {

        # Lag the dates first
        for(l in seq_along(lags)) {
          if(!(paste0(groupingVars[i],"TEMP",lags[l]) %in% SkipCols)) {
            data[, paste0(groupingVars[i],"TEMP",lags[l]) := data.table::shift(get(sortDateName), n = lags[l], type = "lag"), by = get(groupingVars[i])]
          }
        }

        # Difference the lag dates
        if(WindowingLag != 0) {
          for(l in seq_along(lags)) {
            if(!(paste0(timeDiffTarget,lags[l]) %in% SkipCols) & l == 1) {
              data[, paste0(groupingVars[i],timeDiffTarget,lags[l]) := as.numeric(
                difftime(get(sortDateName),
                         get(paste0(groupingVars[i],"TEMP",lags[l])),
                         units = eval(timeAgg))), by = get(groupingVars[i])]
              CounterIndicator = CounterIndicator + 1
              if(Timer) {
                print(CounterIndicator / runs)
              }
            } else {
              if(!(paste0(groupingVars[i],timeDiffTarget,lags[l]) %in% SkipCols)) {
                data[, paste0(groupingVars[i],timeDiffTarget,lags[l]) := as.numeric(
                  difftime(
                    get(paste0(groupingVars[i],"TEMP",(lags[l-1]))),
                    get(paste0(groupingVars[i],"TEMP",lags[l])),
                    units = eval(timeAgg))), by = get(groupingVars[i])]
                CounterIndicator = CounterIndicator + 1
                if(Timer) {
                  print(CounterIndicator / runs)
                }
              }
            }
          }
        } else {
          for(l in seq_along(lags)) {
            if (l == 1) {
              if(!(paste0(groupingVars[i],timeDiffTarget,lags[l]) %in% SkipCols)) {
                data[, paste0(groupingVars[i],timeDiffTarget,lags[l]) := as.numeric(
                  difftime(get(sortDateName),
                           get(paste0(groupingVars[i],"TEMP",lags[l])),
                           units = eval(timeAgg))), by = get(groupingVars[i])]
                CounterIndicator = CounterIndicator + 1
                if(Timer) {
                  print(CounterIndicator / runs)
                }
              }
            } else {
              if(!(paste0(groupingVars[i],timeDiffTarget,lags[l]) %in% SkipCols)) {
                data[, paste0(groupingVars[i],timeDiffTarget,lags[l]) := as.numeric(
                  difftime(
                    get(paste0(groupingVars[i],"TEMP",(lags[l-1]))),
                    get(paste0(groupingVars[i],"TEMP",lags[l])),
                    units = eval(timeAgg))), by = get(groupingVars[i])]
                CounterIndicator = CounterIndicator + 1
                if(Timer) {
                  print(CounterIndicator / runs)
                }
              }
            }
          }
        }

        # Remove temporary lagged dates
        for (l in seq_along(lags)) {
          data[, paste0(groupingVars[i],"TEMP",lags[l]) := NULL]
        }

        # Store new target
        timeTarget <- paste0(groupingVars[i],timeDiffTarget,"1")
      }

      # Define targets
      if(WindowingLag != 0) {
        if (!is.null(timeDiffTarget)) {
          Targets <- c(paste0(groupingVars[i],"_LAG_",WindowingLag,"_",Targets), timeTarget)
        } else {
          Targets <- c(paste0(groupingVars[i],"_LAG_",WindowingLag,"_",Targets))
        }
      } else {
        if (!is.null(timeDiffTarget)) {
          Targets <- c(Targets, timeTarget)
        } else {
          Targets <- Targets
        }
      }

      # Moving stats
      for (j in seq_along(periods)) {
        for (k in seq_along(statsNames)) {
          for (t in Targets) {
            if(!(paste0(groupingVars[i],statsNames[k],"_",periods[j],"_",t) %in% SkipCols)) {
              data[, paste0(groupingVars[i],statsNames[k],"_",periods[j],"_",t) := data.table::frollmean(x = get(t), n = periods[j], fill = NA, algo = "fast", align = "right", na.rm = TRUE, hasNA = TRUE, adaptive = FALSE),
                   by = get(groupingVars[i])]
              CounterIndicator = CounterIndicator + 1
              if(Timer) {
                print(CounterIndicator / runs)
              }
            }
          }
        }
      }
    }

    # Replace any inf values with NA
    for (col in seq_along(data)) {
      set(data, j = col, value = replace(data[[col]], is.infinite(data[[col]]),NA))
    }

    # Turn character columns into factors
    for (col in seq_along(data)) {
      if(is.character(data[[col]])) {
        set(data, j = col, value = as.factor(data[[col]]))
      }
    }

    # Impute missing values
    if(SimpleImpute) {
      for (j in seq_along(data)) {
        if(is.factor(data[[j]])) {
          set(data,which(!(data[[j]] %in% levels(data[[j]]))),j,"0")
        } else {
          set(data,which(is.na(data[[j]])),j,-1)
        }
      }
    }

    # Done!!
    print(CounterIndicator)
    return(data)

  } else {
    if (tolower(Type) == "lag") {
      colVar <- c(sortDateName[1])
      setorderv(data, colVar, order = 1)
    } else {
      colVar <- c(sortDateName[1])
      setorderv(data, colVar, order = -1)
    }

    # Lags
    for(l in seq_along(lags)) {
      for (t in Targets) {
        if(!(paste0("LAG_",lags[l],"_",t) %in% SkipCols)) {
          data[, paste0("LAG_",lags[l],"_",t) := data.table::shift(get(t), n = lags[l], type = "lag")]
          CounterIndicator = CounterIndicator + 1
          if(Timer) {
            print(CounterIndicator / runs)
          }
        }
      }
    }

    # Time lags
    if(!is.null(timeDiffTarget)) {

      # Lag the dates first
      for(l in seq_along(lags)) {
        if(!(paste0("TEMP",lags[l]) %in% SkipCols)) {
          data[, paste0("TEMP",lags[l]) := data.table::shift(get(sortDateName), n = lags[l], type = "lag")]
        }
      }

      # Difference the lag dates
      if(WindowingLag != 0) {
        for(l in seq_along(lags)) {
          if(!(paste0(timeDiffTarget,"_",lags[l]) %in% SkipCols)) {
            data[, paste0(timeDiffTarget,"_",lags[l]) := as.numeric(
              difftime(
                get(paste0("TEMP",(lags[l]-1))),
                get(paste0("TEMP",lags[l])),
                units = eval(timeAgg)))]
            CounterIndicator = CounterIndicator + 1
            if(Timer) {
              print(CounterIndicator / runs)
            }
          }
        }
      } else {
        for(l in seq_along(lags)) {
          if (l == 1) {
            if(!(paste0(timeDiffTarget,"_",lags[l]) %in% SkipCols)) {
              data[, paste0(timeDiffTarget,"_",lags[l]) := as.numeric(
                difftime(get(sortDateName),
                         get(paste0("TEMP",lags[l])),
                         units = eval(timeAgg)))]
              CounterIndicator = CounterIndicator + 1
              if(Timer) {
                print(CounterIndicator / runs)
              }
            }
          } else {
            if(!(paste0(timeDiffTarget,"_",lags[l]) %in% SkipCols)) {
              data[, paste0(timeDiffTarget,"_",lags[l]) := as.numeric(
                difftime(
                  get(paste0("TEMP",(lags[l-1]))),
                  get(paste0("TEMP",lags[l])),
                  units = eval(timeAgg)))]
              CounterIndicator = CounterIndicator + 1
              if(Timer) {
                print(CounterIndicator / runs)
              }
            }
          }
        }
      }

      # Remove temporary lagged dates
      for (l in seq_along(lags)) {
        data[, paste0("TEMP",lags[l]) := NULL]
      }

      # Store new target
      timeTarget <- paste0(timeDiffTarget,"_1")
    }

    # Define targets
    if(WindowingLag != 0) {
      if (!is.null(timeDiffTarget)) {
        Targets <- c(paste0(groupingVars[i],"_LAG_",WindowingLag,"_",Targets), timeTarget)
      } else {
        Targets <- c(paste0(groupingVars[i],"_LAG_",WindowingLag,"_",Targets))
      }
    } else {
      if (!is.null(timeDiffTarget)) {
        Targets <- c(Targets, timeTarget)
      } else {
        Targets <- Targets
      }
    }

    # Moving stats
    for (j in seq_along(periods)) {
      for (k in seq_along(statsNames)) {
        for (t in Targets) {
          if(!(paste0(statsNames[k],"_",periods[j],"_",t) %in% SkipCols)) {
            data[, paste0(statsNames[k],"_",periods[j],"_",t) := data.table::frollmean(x = get(t), n = periods[j], fill = NA, algo = "fast", align = "right", na.rm = TRUE, hasNA = TRUE, adaptive = FALSE)]
            CounterIndicator = CounterIndicator + 1
            if(Timer) {
              print(CounterIndicator / runs)
            }
          }
        }
      }
    }

    # Replace any inf values with NA
    for (col in seq_along(data)) {
      set(data, j = col, value = replace(data[[col]], is.infinite(data[[col]]),NA))
    }

    # Turn character columns into factors
    for (col in seq_along(data)) {
      if(is.character(data[[col]])) {
        set(data, j = col, value = as.factor(data[[col]]))
      }
    }

    # Impute missing values
    if(SimpleImpute) {
      for (j in seq_along(data)) {
        if(is.factor(data[[j]])) {
          set(data,which(!(data[[j]] %in% levels(data[[j]]))),j,"0")
        } else {
          set(data,which(is.na(data[[j]])),j,-1)
        }
      }
    }

    # Done!!
    return(data)
  }
}

#' An Automated Scoring Feature Engineering Function
#'
#' For scoring purposes (brings back a single row by group), this function creates autoregressive and rolling stats from target columns and distributed lags and distributed rolling stats for independent features distributed across time. On top of that, you can also create time between instances along with their associated lags and rolling stats. This function works for data with groups and without groups.
#' @author Adrian Antico
#' @param data The data source you want to run the function on
#' @param lags The list of specific lags you want to have generated
#' @param statsFuns List of functions for your rolling windows, such as mean, sd, min, max, quantile
#' @param statsNames The corresponding names to append to your colnames created associated with statsFuns
#' @param targets The column(s) in which you will build your lags and rolling stats
#' @param groupingVars Categorical variables you will build your lags and rolling stats by
#' @param sortDateName String name of your core date column in your transaction data
#' @param timeDiffTarget List a name in order to create time between events with assiciated lags and rolling features
#' @param timeAgg Unit of time to aggregate by
#' @param WindowingLag   Build moving stats off of target column(s) or one of their lags (1+)
#' @param Type input "Lag" if you want features built on historical values; use "Lead" if you want features built on future values
#' @param Timer Set to TRUE if you want a time run for the operation; useful when there is grouping
#' @param SkipCols Defaults to NULL; otherwise name the vector containing the names of columns to skip
#' @param SimpleImpute Set to TRUE for factor level imputation of "0" and numeric imputation of -1
#' @param AscRowByGroup Required to have a column with a Row Number by group (if grouping) with 1 being the record for scoring (typically the most current in time)
#' @param RecordsKeep List the number of records to retain (1 for last record, 2 for last 2 records, etc.)
#' @return data.table of original data plus newly created features
#' @examples
#' quick_model <- Scoring_GDL_Feature_Engineering(quick_model,
#'                                                lags           = c(1:6,12,seq(24,168,24)),
#'                                                periods        = c(6,12,24,72,168,720,4320,8640),
#'                                                statsFUNs      = c(function(x) mean(x, na.rm = TRUE),
#'                                                                   function(x) sd(x, na.rm = TRUE),
#'                                                                   function(x) quantile(x, probs = 0.85, na.rm = TRUE)),
#'                                                statsNames     = c("mean","sd","q85"),
#'                                                targets        = c("ScaledConsumption"),
#'                                                groupingVars   = c("BADGE_NBR"),
#'                                                sortDateName   = "DAY_DATE",
#'                                                timeDiffTarget = NULL,
#'                                                timeAgg        = "hours",
#'                                                WindowingLag   = 1,
#'                                                Type           = "Lag",
#'                                                Timer          = FALSE,
#'                                                SkipCols       = FALSE,
#'                                                SimpleImpute   = TRUE,
#'                                                AscRowByGroup  = "BadgeRowNum")
#' @export
Scoring_GDL_Feature_Engineering <- function(data,
                                            lags           = c(1:6,12,seq(24,168,24)),
                                            periods        = c(6,12,24,72,168,720,4320,8640),
                                            statsFUNs      = c(function(x) mean(x, na.rm = TRUE),
                                                               function(x) sd(x, na.rm = TRUE),
                                                               function(x) quantile(x, probs = 0.85, na.rm = TRUE)),
                                            statsNames     = c("mean","sd","q85"),
                                            targets        = c("ScaledConsumption"),
                                            groupingVars   = c("BADGE_NBR"),
                                            sortDateName   = "DAY_DATE",
                                            timeDiffTarget = NULL,
                                            timeAgg        = "hours",
                                            WindowingLag   = 1,
                                            Type           = "Lag",
                                            Timer          = FALSE,
                                            SkipCols       = FALSE,
                                            SimpleImpute   = TRUE,
                                            AscRowByGroup  = "BadgeRowNum",
                                            RecordsKeep    = 1) {

  # Load libraries
  library(data.table)

  # Convert to data.table if not already
  if(!is.data.table(data)) data <- as.data.table(data)

  # Max data to keep
  MAX_RECORDS_FULL <- max(max(lags+1),max(periods+1),RecordsKeep)
  MAX_RECORDS_LAGS <- max(max(lags+1),RecordsKeep)
  MAX_RECORDS_ROLL <- max(max(periods+1),RecordsKeep)

  # Set up counter for countdown
  CounterIndicator = 0
  if (!is.null(timeDiffTarget)) {
    tarNum <- length(targets) + 1
  } else {
    tarNum <- length(targets)
  }

  # Define total runs
  if (!is.null(groupingVars)) {
    runs <- length(groupingVars) * length(periods) * length(statsNames) * tarNum + length(lags)
  } else {
    runs <- length(periods) * length(statsNames) * tarNum
  }

  # Begin feature engineering
  if(!is.null(groupingVars)) {
    for (i in seq_along(groupingVars)) {
      # Sort data
      if(tolower(Type) == "lag") {
        colVar <- c(groupingVars[i],sortDateName[1])
        setorderv(data, colVar, order = 1)
      } else {
        colVar <- c(groupingVars[i],sortDateName[1])
        setorderv(data, colVar, order = -1)
      }

      # Remove records
      tempData <- data[get(AscRowByGroup) <= MAX_RECORDS_FULL]

      # Lags
      for(l in seq_along(lags)) {
        for (t in targets) {
          if(!(paste0(groupingVars[i],"_LAG_",lags[l],"_",t) %in% SkipCols)) {
            tempData[, paste0(groupingVars[i],"_LAG_",lags[l],"_",t) := data.table::shift(get(t), n = lags[l], type = "lag"), by = get(groupingVars[i])]
          }
        }
      }

      # Time lags
      if(!is.null(timeDiffTarget)) {

        # Lag the dates first
        for(l in seq_along(lags)) {
          if(!(paste0(groupingVars[i],"TEMP",lags[l]) %in% SkipCols)) {
            tempData[, paste0(groupingVars[i],"TEMP",lags[l]) := data.table::shift(get(sortDateName), n = lags[l], type = "lag"), by = get(groupingVars[i])]
          }
        }

        # Difference the lag dates
        if(WindowingLag != 0) {
          for(l in seq_along(lags)) {
            if(!(paste0(timeDiffTarget,lags[l]) %in% SkipCols) || l == 1) {
              tempData[, paste0(timeDiffTarget,lags[l]) := as.numeric(
                difftime(
                  get(paste0(groupingVars[i],"TEMP",(lags[l-1]))),
                  get(paste0(groupingVars[i],"TEMP",lags[l])),
                  units = eval(timeAgg))), by = get(groupingVars[i])]
            }
          }
        } else {
          for(l in seq_along(lags)) {
            if (l == 1) {
              if(!(paste0(groupingVars[i],timeDiffTarget,lags[l]) %in% SkipCols)) {
                tempData[, paste0(groupingVars[i],timeDiffTarget,lags[l]) := as.numeric(
                  difftime(get(sortDateName),
                           get(paste0(groupingVars[i],"TEMP",lags[l])),
                           units = eval(timeAgg))), by = get(groupingVars[i])]
              }
            } else {
              if(!(paste0(groupingVars[i],timeDiffTarget,lags[l]) %in% SkipCols)) {
                tempData[, paste0(groupingVars[i],timeDiffTarget,lags[l]) := as.numeric(
                  difftime(
                    get(paste0(groupingVars[i],"TEMP",(lags[l-1]))),
                    get(paste0(groupingVars[i],"TEMP",lags[l])),
                    units = eval(timeAgg))), by = get(groupingVars[i])]
              }
            }
          }
        }

        # Remove temporary lagged dates
        for (l in seq_along(lags)) {
          tempData[, paste0(groupingVars[i],"TEMP",lags[l]) := NULL]
        }

        # Store new target
        timeTarget <- paste0(groupingVars[i],timeDiffTarget,"1")
      }

      # Define targets
      if(WindowingLag != 0) {
        if (!is.null(timeDiffTarget)) {
          targets <- c(paste0(groupingVars[i],"_LAG_",WindowingLag,"_",targets), timeTarget)
        } else {
          targets <- c(paste0(groupingVars[i],"_LAG_",WindowingLag,"_",targets))
        }
      } else {
        if (!is.null(timeDiffTarget)) {
          targets <- c(targets, timeTarget)
        } else {
          targets <- targets
        }
      }

      # Keep final values
      tempData1 <- tempData[get(AscRowByGroup) <= eval(RecordsKeep)]

      # Moving stats
      for (j in seq_along(periods)) {
        for (k in seq_along(statsNames)) {
          for (t in targets) {
            if(!(paste0(groupingVars[i],statsNames[k],"_",periods[j],"_",t) %in% SkipCols)) {
              keep <- c(groupingVars[i],t,AscRowByGroup)
              temp2 <- tempData[get(AscRowByGroup) <= MAX_RECORDS_ROLL][, ..keep]
              temp3 <- temp2[, paste0(groupingVars[i],statsNames[k],"_",periods[j],"_",t) := lapply(.SD, statsFUNs[k][[1]]), by = get(groupingVars[i]), .SDcols = eval(t)]
              if(Timer) {
                CounterIndicator = CounterIndicator + 1
                print(CounterIndicator / runs)
              }
              # Merge files
              temp4 <- temp3[get(AscRowByGroup) <= eval(RecordsKeep)][, c(eval(t)) := NULL]
              tempData1 <- merge(tempData1, temp4, by = c(eval(groupingVars[i]),eval(AscRowByGroup)))
            }
          }
        }
      }
    }

    # Replace any inf values with NA
    for (col in seq_along(tempData1)) {
      set(tempData1, j = col, value = replace(tempData1[[col]], is.infinite(tempData1[[col]]),NA))
    }

    # Turn character columns into factors
    for (col in seq_along(tempData1)) {
      if(is.character(tempData1[[col]])) {
        set(tempData1, j = col, value = as.factor(tempData1[[col]]))
      }
    }

    # Impute missing values
    if(SimpleImpute) {
      for (j in seq_along(tempData1)) {
        if(is.factor(tempData1[[j]])) {
          set(tempData1,which(!(tempData1[[j]] %in% levels(tempData1[[j]]))),j,"0")
        } else {
          set(tempData1,which(is.na(tempData1[[j]])),j,-1)
        }
      }
    }

    # Done!!
    return(tempData1)

  } else {
    if (tolower(Type) == "lag") {
      colVar <- c(sortDateName[1])
      setorderv(data, colVar, order = 1)
    } else {
      colVar <- c(sortDateName[1])
      setorderv(data, colVar, order = -1)
    }

    # Remove records
    tempData <- data[get(AscRowByGroup) <= MAX_RECORDS_FULL]

    # Lags
    for(l in seq_along(lags)) {
      for (t in targets) {
        if(!(paste0("LAG_",lags[l],"_",t) %in% SkipCols)) {
          tempData[, paste0("LAG_",lags[l],"_",t) := data.table::shift(get(t), n = lags[l], type = "lag")]
        }
      }
    }

    # Time lags
    if(!is.null(timeDiffTarget)) {

      # Lag the dates first
      for(l in seq_along(lags)) {
        if(!(paste0("TEMP",lags[l]) %in% SkipCols)) {
          tempData[, paste0("TEMP",lags[l]) := data.table::shift(get(sortDateName), n = lags[l], type = "lag")]
        }
      }

      # Difference the lag dates
      if(WindowingLag != 0) {
        for(l in seq_along(lags)) {
          if(!(paste0(timeDiffTarget,"_",lags[l]) %in% SkipCols)) {
            tempData[, paste0(timeDiffTarget,"_",lags[l]) := as.numeric(
              difftime(
                get(paste0("TEMP",(lags[l]-1))),
                get(paste0("TEMP",lags[l])),
                units = eval(timeAgg)))]
          }
        }
      } else {
        for(l in seq_along(lags)) {
          if (l == 1) {
            if(!(paste0(timeDiffTarget,"_",lags[l]) %in% SkipCols)) {
              tempData[, paste0(timeDiffTarget,"_",lags[l]) := as.numeric(
                difftime(get(sortDateName),
                         get(paste0("TEMP",lags[l])),
                         units = eval(timeAgg)))]
            }
          } else {
            if(!(paste0(timeDiffTarget,"_",lags[l]) %in% SkipCols)) {
              tempData[, paste0(timeDiffTarget,"_",lags[l]) := as.numeric(
                difftime(
                  get(paste0("TEMP",(lags[l-1]))),
                  get(paste0("TEMP",lags[l])),
                  units = eval(timeAgg)))]
            }
          }
        }
      }

      # Remove temporary lagged dates
      for (l in seq_along(lags)) {
        tempData[, paste0("TEMP",lags[l]) := NULL]
      }

      # Store new target
      timeTarget <- paste0(timeDiffTarget,"_1")
    }

    # Define targets
    if(WindowingLag !=0) {
      if (!is.null(timeDiffTarget)) {
        targets <- c(paste0("LAG_",WindowingLag,"_",targets), timeTarget)
      } else {
        targets <- c(paste0("LAG_",WindowingLag,"_",targets))
      }
    } else {
      if(!is.null(timeDiffTarget)) {
        targets <- c(targets, timeTarget)
      } else {
        targets <- targets
      }
    }

    # Define targets
    if(WindowingLag != 0) {
      if (!is.null(timeDiffTarget)) {
        targets <- c(paste0(groupingVars[i],"_LAG_",WindowingLag,"_",targets), timeTarget)
      } else {
        targets <- c(paste0(groupingVars[i],"_LAG_",WindowingLag,"_",targets))
      }
    } else {
      if (!is.null(timeDiffTarget)) {
        targets <- c(targets, timeTarget)
      } else {
        targets <- targets
      }
    }

    # Keep final values
    tempData1 <- tempData[get(AscRowByGroup) <= eval(RecordsKeep)]

    # Moving stats
    for (j in seq_along(periods)) {
      for (k in seq_along(statsNames)) {
        for (t in targets) {
          if(!(paste0(statsNames[k],"_",periods[j],"_",t) %in% SkipCols)) {

            keep <- c(groupingVars[i],t,AscRowByGroup)
            temp2 <- tempData[get(AscRowByGroup) <= MAX_RECORDS_FULL][, ..keep]
            temp3 <- temp2[, paste0(statsNames[k],"_",periods[j],"_",t) := lapply(.SD, statsFUNs[k][[1]]), .SDcols = eval(t)]
            if(Timer) {
              CounterIndicator = CounterIndicator + 1
              print(CounterIndicator / runs)
            }
            # Merge files
            temp4 <- temp3[get(AscRowByGroup) <= eval(RecordsKeep)][, c(eval(AscRowByGroup), eval(t)) := NULL]
            tempData1 <- cbind(tempData1, temp4)
          }
        }
      }
    }

    # Replace any inf values with NA
    for (col in seq_along(data)) {
      set(data, j = col, value = replace(data[[col]], is.infinite(data[[col]]),NA))
    }

    # Turn character columns into factors
    for (col in seq_along(data)) {
      if(is.character(data[[col]])) {
        set(data, j = col, value = as.factor(data[[col]]))
      }
    }

    # Impute missing values
    if(SimpleImpute) {
      for (j in seq_along(data)) {
        if(is.factor(data[[j]])) {
          set(data,which(!(data[[j]] %in% levels(data[[j]]))),j,"0")
        } else {
          set(data,which(is.na(data[[j]])),j,-1)
        }
      }
    }

    # Done!!
    return(data)
  }
}

#' An Fast Automated Feature Engineering Function
#'
#' For models with target variables within the realm of the current time frame but not too far back in time, this function creates autoregressive and rolling stats from target columns and distributed lags and distributed rolling stats for independent features distributed across time. On top of that, you can also create time between instances along with their associated lags and rolling stats. This function works for data with groups and without groups.
#' @author Adrian Antico
#' @param data The data source you want to run the function on
#' @param lags The list of specific lags you want to have generated
#' @param statsFuns List of functions for your rolling windows, such as mean, sd, min, max, quantile
#' @param statsNames The corresponding names to append to your colnames created associated with statsFuns
#' @param targets The column(s) in which you will build your lags and rolling stats
#' @param groupingVars Categorical variables you will build your lags and rolling stats by
#' @param sortDateName String name of your core date column in your transaction data
#' @param timeDiffTarget List a name in order to create time between events with assiciated lags and rolling features
#' @param timeAgg Unit of time to aggregate by
#' @param WindowingLag   Build moving stats off of target column(s) or one of their lags (1+)
#' @param Type input "Lag" if you want features built on historical values; use "Lead" if you want features built on future values
#' @param Timer Set to TRUE if you want a time run for the operation; useful when there is grouping
#' @param SkipCols Defaults to NULL; otherwise name the vector containing the names of columns to skip
#' @param SimpleImpute Set to TRUE for factor level imputation of "0" and numeric imputation of -1
#' @param AscRowByGroup Required to have a column with a Row Number by group (if grouping) with 1 being the record for scoring (typically the most current in time)
#' @param RecordsKeep List the number of records to retain (1 for last record, 2 for last 2 records, etc.)
#' @return data.table of original data plus newly created features
#' @examples
#' data <- FAST_GDL_Feature_Engineering(data,
#'                                      lags           = c(1:6,12,seq(24,168,24)),
#'                                      periods        = c(6,12,24,72,168,720,4320,8640),
#'                                      statsFUNs      = c("mean","median","sd","quantile85","quantile95"),
#'                                      statsNames     = c("mean","median","sd","quantile85","quantile95"),
#'                                      targets        = c("timeseriesvalues"),
#'                                      groupingVars   = c("ID"),
#'                                      sortDateName   = "DATE",
#'                                      timeDiffTarget = NULL,
#'                                      timeAgg        = "hours",
#'                                      WindowingLag   = 1,
#'                                      Type           = "Lag",
#'                                      Timer          = FALSE,
#'                                      SkipCols       = FALSE,
#'                                      SimpleImpute   = TRUE,
#'                                      AscRowByGroup  = "RowNum")
#' @export
FAST_GDL_Feature_Engineering <- function(data,
                                         lags           = c(1:6,12,seq(24,168,24)),
                                         periods        = c(6,12,24,72,168,720,4320,8640),
                                         statsFUNs      = c("mean","median","sd","quantile85","quantile95"),
                                         statsNames     = c("mean","median","sd","quantile85","quantile95"),
                                         targets        = c("ScaledConsumption"),
                                         groupingVars   = c("BADGE_NBR"),
                                         sortDateName   = "DAY_DATE",
                                         timeDiffTarget = NULL,
                                         timeAgg        = "hours",
                                         WindowingLag   = 1,
                                         Type           = "Lag",
                                         Timer          = FALSE,
                                         SkipCols       = FALSE,
                                         SimpleImpute   = TRUE,
                                         AscRowByGroup  = "BadgeRowNum",
                                         RecordsKeep    = 1) {

  # Load libraries
  library(data.table)
  library(caTools)
  
  # Convert to data.table if not already
  if(!is.data.table(data)) data <- as.data.table(data)

  # Max data to keep
  MAX_RECORDS_FULL <- max(max(lags+1),max(periods*2),RecordsKeep)
  MAX_RECORDS_LAGS <- max(max(lags+1),RecordsKeep)
  MAX_RECORDS_ROLL <- max(max(periods*2),RecordsKeep)

  # Set up counter for countdown
  CounterIndicator = 0
  if (!is.null(timeDiffTarget)) {
    tarNum <- length(targets) + 1
  } else {
    tarNum <- length(targets)
  }

  # Define total runs
  if (!is.null(groupingVars)) {
    runs <- length(groupingVars) * length(periods) * length(statsNames) * tarNum + length(lags)
  } else {
    runs <- length(periods) * length(statsNames) * tarNum
  }

  # Begin feature engineering
  if(!is.null(groupingVars)) {
    for (i in seq_along(groupingVars)) {
      # Sort data
      if(tolower(Type) == "lag") {
        colVar <- c(groupingVars[i],sortDateName[1])
        setorderv(data, colVar, order = 1)
      } else {
        colVar <- c(groupingVars[i],sortDateName[1])
        setorderv(data, colVar, order = -1)
      }

      # Remove records
      tempData <- data[get(AscRowByGroup) <= MAX_RECORDS_FULL]

      # Lags
      for(l in seq_along(lags)) {
        for (t in targets) {
          if(!(paste0(groupingVars[i],"_LAG_",lags[l],"_",t) %in% SkipCols)) {
            tempData[, paste0(groupingVars[i],"_LAG_",lags[l],"_",t) := data.table::shift(get(t), n = lags[l], type = "lag"), by = get(groupingVars[i])]
          }
        }
      }

      # Time lags
      if(!is.null(timeDiffTarget)) {

        # Lag the dates first
        for(l in seq_along(lags)) {
          if(!(paste0(groupingVars[i],"TEMP",lags[l]) %in% SkipCols)) {
            tempData[, paste0(groupingVars[i],"TEMP",lags[l]) := data.table::shift(get(sortDateName), n = lags[l], type = "lag"), by = get(groupingVars[i])]
          }
        }

        # Difference the lag dates
        if(WindowingLag != 0) {
          for(l in seq_along(lags)) {
            if(!(paste0(timeDiffTarget,lags[l]) %in% SkipCols) || l == 1) {
              tempData[, paste0(timeDiffTarget,lags[l]) := as.numeric(
                difftime(
                  get(paste0(groupingVars[i],"TEMP",(lags[l-1]))),
                  get(paste0(groupingVars[i],"TEMP",lags[l])),
                  units = eval(timeAgg))), by = get(groupingVars[i])]
            }
          }
        } else {
          for(l in seq_along(lags)) {
            if (l == 1) {
              if(!(paste0(groupingVars[i],timeDiffTarget,lags[l]) %in% SkipCols)) {
                tempData[, paste0(groupingVars[i],timeDiffTarget,lags[l]) := as.numeric(
                  difftime(get(sortDateName),
                           get(paste0(groupingVars[i],"TEMP",lags[l])),
                           units = eval(timeAgg))), by = get(groupingVars[i])]
              }
            } else {
              if(!(paste0(groupingVars[i],timeDiffTarget,lags[l]) %in% SkipCols)) {
                tempData[, paste0(groupingVars[i],timeDiffTarget,lags[l]) := as.numeric(
                  difftime(
                    get(paste0(groupingVars[i],"TEMP",(lags[l-1]))),
                    get(paste0(groupingVars[i],"TEMP",lags[l])),
                    units = eval(timeAgg))), by = get(groupingVars[i])]
              }
            }
          }
        }

        # Remove temporary lagged dates
        for (l in seq_along(lags)) {
          tempData[, paste0(groupingVars[i],"TEMP",lags[l]) := NULL]
        }

        # Store new target
        timeTarget <- paste0(groupingVars[i],timeDiffTarget,"1")
      }

      # Define targets
      if(WindowingLag != 0) {
        if (!is.null(timeDiffTarget)) {
          targets <- c(paste0(groupingVars[i],"_LAG_",WindowingLag,"_",targets), timeTarget)
        } else {
          targets <- c(paste0(groupingVars[i],"_LAG_",WindowingLag,"_",targets))
        }
      } else {
        if (!is.null(timeDiffTarget)) {
          targets <- c(targets, timeTarget)
        } else {
          targets <- targets
        }
      }

      # Keep final values
      tempData1 <- tempData[get(AscRowByGroup) <= eval(RecordsKeep)]

      # Re-Sort
      if(tolower(Type) == "lag") {
        colVar <- c(groupingVars[i],sortDateName[1])
        setorderv(data, colVar, order = -1)
      } else {
        colVar <- c(groupingVars[i],sortDateName[1])
        setorderv(data, colVar, order = 1)
      }

      # Moving stats
      for (j in seq_along(periods)) {
        for (k in seq_along(statsNames)) {
          for (t in targets) {
            if(!(paste0(groupingVars[i],statsNames[k],"_",periods[j],"_",t) %in% SkipCols)) {
              keep <- c(groupingVars[i],t,AscRowByGroup)
              temp2 <- tempData[get(AscRowByGroup) <= MAX_RECORDS_ROLL][, ..keep]
              if(statsNames[k] == "mean") {
                temp3 <- temp2[, paste0(groupingVars[i],statsNames[k],"_",periods[j],"_",t) := caTools::runmean(get(t), k = periods[j], endrule = "trim", alg = "fast"), by = get(groupingVars[i])]
              } else if(statsNames[k] == "median") {
                temp3 <- temp2[, paste0(groupingVars[i],statsNames[k],"_",periods[j],"_",t) := caTools::runquantile(get(t), probs = 0.50, k = periods[j], endrule = "trim"), by = get(groupingVars[i])]
              } else if(statsNames[k] == "sd") {
                temp3 <- temp2[, paste0(groupingVars[i],statsNames[k],"_",periods[j],"_",t) := caTools::runsd(get(t), k = periods[j], endrule = "trim"), by = get(groupingVars[i])]
              } else if(statsNames[k] == "quantile85") {
                temp3 <- temp2[, paste0(groupingVars[i],statsNames[k],"_",periods[j],"_",t) := caTools::runquantile(get(t), probs = 0.85, k = periods[j], endrule = "trim"), by = get(groupingVars[i])]
              } else if(statsNames[k] == "quantile95") {
                temp3 <- temp2[, paste0(groupingVars[i],statsNames[k],"_",periods[j],"_",t) := caTools::runquantile(get(t), probs = 0.95, k = periods[j], endrule = "trim"), by = get(groupingVars[i])]
              }
              if(Timer) {
                CounterIndicator = CounterIndicator + 1
                print(CounterIndicator / runs)
              }
              # Merge files
              temp4 <- temp3[get(AscRowByGroup) <= eval(RecordsKeep)][, c(eval(t)) := NULL]
              tempData1 <- merge(tempData1, temp4, by = c(eval(groupingVars[i]),eval(AscRowByGroup)))
            }
          }
        }
      }
    }

    # Replace any inf values with NA
    for (col in seq_along(tempData1)) {
      set(tempData1, j = col, value = replace(tempData1[[col]], is.infinite(tempData1[[col]]),NA))
    }

    # Turn character columns into factors
    for (col in seq_along(tempData1)) {
      if(is.character(tempData1[[col]])) {
        set(tempData1, j = col, value = as.factor(tempData1[[col]]))
      }
    }

    # Impute missing values
    if(SimpleImpute) {
      for (j in seq_along(tempData1)) {
        if(is.factor(tempData1[[j]])) {
          set(tempData1,which(!(tempData1[[j]] %in% levels(tempData1[[j]]))),j,"0")
        } else {
          set(tempData1,which(is.na(tempData1[[j]])),j,-1)
        }
      }
    }

    # Done!!
    return(tempData1)

  } else {
    if (tolower(Type) == "lag") {
      colVar <- c(sortDateName[1])
      setorderv(data, colVar, order = 1)
    } else {
      colVar <- c(sortDateName[1])
      setorderv(data, colVar, order = -1)
    }

    # Remove records
    tempData <- data[get(AscRowByGroup) <= max(max(lags+1),max(periods+1),RecordsKeep)]

    # Lags
    for(l in seq_along(lags)) {
      for (t in targets) {
        if(!(paste0("LAG_",lags[l],"_",t) %in% SkipCols)) {
          tempData[, paste0("LAG_",lags[l],"_",t) := data.table::shift(get(t), n = lags[l], type = "lag")]
        }
      }
    }

    # Time lags
    if(!is.null(timeDiffTarget)) {

      # Lag the dates first
      for(l in seq_along(lags)) {
        if(!(paste0("TEMP",lags[l]) %in% SkipCols)) {
          tempData[, paste0("TEMP",lags[l]) := data.table::shift(get(sortDateName), n = lags[l], type = "lag")]
        }
      }

      # Difference the lag dates
      if(WindowingLag != 0) {
        for(l in seq_along(lags)) {
          if(!(paste0(timeDiffTarget,"_",lags[l]) %in% SkipCols)) {
            tempData[, paste0(timeDiffTarget,"_",lags[l]) := as.numeric(
              difftime(
                get(paste0("TEMP",(lags[l]-1))),
                get(paste0("TEMP",lags[l])),
                units = eval(timeAgg)))]
          }
        }
      } else {
        for(l in seq_along(lags)) {
          if (l == 1) {
            if(!(paste0(timeDiffTarget,"_",lags[l]) %in% SkipCols)) {
              tempData[, paste0(timeDiffTarget,"_",lags[l]) := as.numeric(
                difftime(get(sortDateName),
                         get(paste0("TEMP",lags[l])),
                         units = eval(timeAgg)))]
            }
          } else {
            if(!(paste0(timeDiffTarget,"_",lags[l]) %in% SkipCols)) {
              tempData[, paste0(timeDiffTarget,"_",lags[l]) := as.numeric(
                difftime(
                  get(paste0("TEMP",(lags[l-1]))),
                  get(paste0("TEMP",lags[l])),
                  units = eval(timeAgg)))]
            }
          }
        }
      }

      # Remove temporary lagged dates
      for (l in seq_along(lags)) {
        tempData[, paste0("TEMP",lags[l]) := NULL]
      }

      # Store new target
      timeTarget <- paste0(timeDiffTarget,"_1")
    }

    # Define targets
    if(WindowingLag !=0) {
      if (!is.null(timeDiffTarget)) {
        targets <- c(paste0("LAG_",WindowingLag,"_",targets), timeTarget)
      } else {
        targets <- c(paste0("LAG_",WindowingLag,"_",targets))
      }
    } else {
      if(!is.null(timeDiffTarget)) {
        targets <- c(targets, timeTarget)
      } else {
        targets <- targets
      }
    }

    # Define targets
    if(WindowingLag != 0) {
      if (!is.null(timeDiffTarget)) {
        targets <- c(paste0(groupingVars[i],"_LAG_",WindowingLag,"_",targets), timeTarget)
      } else {
        targets <- c(paste0(groupingVars[i],"_LAG_",WindowingLag,"_",targets))
      }
    } else {
      if (!is.null(timeDiffTarget)) {
        targets <- c(targets, timeTarget)
      } else {
        targets <- targets
      }
    }

    # Keep final values
    tempData1 <- tempData[get(AscRowByGroup) <= eval(RecordsKeep)]

    # Moving stats
    for (j in seq_along(periods)) {
      for (k in seq_along(statsNames)) {
        for (t in targets) {
          if(!(paste0(statsNames[k],"_",periods[j],"_",t) %in% SkipCols)) {
            keep <- c(groupingVars[i],t,AscRowByGroup)
            temp2 <- tempData[get(AscRowByGroup) <= MAX_RECORDS_ROLL][, ..keep]
            if(statsNames[k] == "mean") {
              temp3 <- temp2[, paste0(statsNames[k],"_",periods[j],"_",t) := caTools::runmean(get(t), k = periods[j], endrule = "trim", alg = "fast")]
            } else if(statsNames[k] == "median") {
              temp3 <- temp2[, paste0(statsNames[k],"_",periods[j],"_",t) := caTools::runquantile(get(t), probs = 0.50, k = periods[j], endrule = "trim")]
            } else if(statsNames[k] == "sd") {
              temp3 <- temp2[, paste0(statsNames[k],"_",periods[j],"_",t) := caTools::runsd(get(t), k = periods[j], endrule = "trim")]
            } else if(statsNames[k] == "quantile85") {
              temp3 <- temp2[, paste0(statsNames[k],"_",periods[j],"_",t) := caTools::runquantile(get(t), probs = 0.85, k = periods[j], endrule = "trim")]
            } else if(statsNames[k] == "quantile95") {
              temp3 <- temp2[, paste0(statsNames[k],"_",periods[j],"_",t) := caTools::runquantile(get(t), probs = 0.95, k = periods[j], endrule = "trim")]
            }
            if(Timer) {
              CounterIndicator = CounterIndicator + 1
              print(CounterIndicator / runs)
            }
            # Merge files
            temp4 <- temp3[get(AscRowByGroup) <= eval(RecordsKeep)][, c(eval(t)) := NULL]
            tempData1 <- merge(tempData1, temp4, by = c(eval(AscRowByGroup)))
          }
        }
      }
    }

    # Replace any inf values with NA
    for (col in seq_along(data)) {
      set(data, j = col, value = replace(data[[col]], is.infinite(data[[col]]),NA))
    }

    # Turn character columns into factors
    for (col in seq_along(data)) {
      if(is.character(data[[col]])) {
        set(data, j = col, value = as.factor(data[[col]]))
      }
    }

    # Impute missing values
    if(SimpleImpute) {
      for (j in seq_along(data)) {
        if(is.factor(data[[j]])) {
          set(data,which(!(data[[j]] %in% levels(data[[j]]))),j,"0")
        } else {
          set(data,which(is.na(data[[j]])),j,-1)
        }
      }
    }

    # Done!!
    return(data)
  }
}

#' An Automated Machine Learning Framework using H20
#'
#' 1. Logic: Error checking in the modeling arguments from your Construction file
#' 2. ML: Build grid-tuned models and baseline models for comparison and checks which one performs better on validation data
#' 3. Evaluation: Collects the performance metrics for both
#' 4. Evaluation: Generates calibration plots (and boxplots for regression) for the winning model
#' 5. Evaluation: Generates partial dependence calibration plots (and boxplots for regression) for the winning model
#' 6. Evaluation: Generates variable importance tables and a table of non-important features
#' 7. Production: Creates a storage file containing: model name, model path, grid tune performance, baseline performance, and threshold (if classification) and stores that file in your model_path location
#' @author Adrian Antico
#' @param Construct Core instruction file for automation
#' @param max_memory The ceiling amount of memory H20 will utilize
#' @param ratios The percentage of train samples from source data (remainder goes to validation set)
#' @param BL_Trees The number of trees to build in baseline GBM or RandomForest
#' @param nthreads Set the number of threads to run function
#' @param model_path Directory path for where you want your models saved
#' @param MaxRuntimeSeconds Number of seconds of run time for grid tuning
#' @param MaxModels Number of models you'd like to have returned
#' @param TrainData Set to NULL or supply a data.table for training data
#' @param TestData Set to NULL or supply  a data.table for validation data
#' @param Targets Names of target variables in source data
#' @param Distribution Distribution family, e.g. bernoulli
#' @param Loss Loss metric for model, e.g. AUC for binary classification
#' @param Quantile The numeric decimal representing the quantile you wish to model
#' @param ModelName The name for your model
#' @param Algorithm Name of algo, i.e. gbm, randomForest, deeplearning
#' @param dataName The name of the data used to build model
#' @param TargetCol The reference to the target variable
#' @param FeatureCols The reference to the feature variables
#' @param CreateDate Set the date of when this file was created
#' @param GridTune Set to TRUE / FALSE
#' @param ExportValidData Set to TRUE / FALSE to export the validation data with predictions
#' @param ParDep Set a number N to return the partial dependence plots for the top N features from variable importance
#' @param PD_Data Specify to use all, train, or validation data to build partial dependence plots
#' @param ThreshType For binary classification, choose from "f1", "f2", "f0point5", or "CS" (Cost Sensitive)
#' @param FSC Feature selection criteria: choose the variable importance percentage cutoff
#' @param tpProfit True Positive Profit amount
#' @param tnProfit True Negative Profit amount
#' @param fpProfit False Positive Profit amount
#' @param fnProfit False Negative Profit amount
#' @param SaveModel Set to TRUE to save model
#' @param SaveModelType Set to standard for h2o file, mojo for mojo file
#' @param PredsAllData Set to TRUE to export all data (train + validate) with predicted values
#' @param TargetEncoding Supply either NA or a vector of numeric column references in quotes "c(2:8)"
#' @param SupplyData Set to TRUE if you are supplying your own training and validation data
#' @return Returns saved models, corrected Construct file, variable importance tables, evaluation and partial dependence calibration plots, model performance measure, etc.
#' @examples
#'Correl <- 0.85
#'aa <- data.table(target = runif(10000))
#'aa[, x1 := qnorm(target)]
#'aa[, x2 := runif(10000)]
#'aa[, Independent_Variable1 := log(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
#'aa[, Independent_Variable2 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
#'aa[, Independent_Variable3 := exp(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
#'aa[, Independent_Variable4 := exp(exp(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2))))]
#'aa[, Independent_Variable5 := sqrt(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
#'aa[, Independent_Variable6 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^0.10]
#'aa[, Independent_Variable7 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^0.25]
#'aa[, Independent_Variable8 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^0.75]
#'aa[, Independent_Variable9 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^2]
#'aa[, Independent_Variable10 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^4]
#'aa[, ':=' (x1 = NULL, x2 = NULL)]
#'aa[, target := as.factor(ifelse(target > 0.5,1,0))]
#'N = 1
#'Construct <- data.table(Targets         = "target",
#'                        Distribution    = "bernoulli",
#'                        Loss            = "AUC",
#'                        Quantile        = 0.01,
#'                        ModelName       = "bla",
#'                        Algorithm       = "gbm",
#'                        dataName        = "aa",
#'                        TargetCol       = c("1"),
#'                        FeatureCols     = c("2:4"),
#'                        CreateDate      = Sys.time(),
#'                        GridTune        = FALSE,
#'                        ExportValidData = TRUE,
#'                        ParDep          = 10,
#'                        PD_Data         = "All",
#'                        ThreshType      = "f1",
#'                        FSC             = 0.001,
#'                        tpProfit        = rep(0,N),
#'                        tnProfit        = rep(0,N),
#'                        fpProfit        = rep(-1,N),
#'                        fnProfit        = rep(-5,N),
#'                        SaveModel       = rep("FALSE",N),
#'                        SaveModelType   = rep("Mojo",N),
#'                        PredsAllData    = rep(TRUE,N),
#'                        TargetEncoding  = rep(NA,N),
#'                        SupplyData      = rep(TRUE,N))
#'AutoH20Modeler(Construct,
#'               max_memory = "28G",
#'               ratios = 0.75,
#'               BL_Trees = 500,
#'               nthreads = 5,
#'               model_path = getwd(),
#'               MaxRuntimeSeconds = 3600,
#'               MaxModels = 30,
#'               TrainData = data,
#'               TestData  = test)
#' @export
AutoH20Modeler <- function(Construct,
                           max_memory,
                           ratios,
                           BL_Trees,
                           nthreads,
                           model_path,
                           MaxRuntimeSeconds = 3600,
                           MaxModels = 30,
                           TrainData = data,
                           TestData  = test) {

  library(data.table)
  library(h2o)
  
  ######################################
  # Error handling
  ######################################

  # 1. Check for errors
  # 2. Replace values with proper case values

  # ERROR PROCESS CHECKING
  # 1. Identify model type, record if not in supported model list
  # 2. Check to see if loss function is in supported loss function list for model types
  # 3. Check to see if distribution is in supported distribution list for model types
  # 4. For non-regression, check to see if distribution type corresponds to correct option set for loss functions
  # 5. For regression, check to see if distribution type corresponds to correct option set for loss functions
  # 6. For quantile regression, ensure the model is in the available model list for quantile regression
  # 7. For quantile regression, ensure chosen quantiles are within 0 and 1

  # REPLACING VALUES WITH PROPER CASE VALUES
  # 1. Store current value from Construct file
  # 2. Create data.table with current value repeated, lower case possible values, proper cased actual values
  # 3. Subset based on current value matching lower case value, and grabbing proper case value
  # 4. Replace current value for proper case value in Construct file

  ErrorCollection <- data.table(Row = rep(-720,10000), Msg = "I like modeling")
  j = 0
  for (i in 1:nrow(Construct)) {
    # Algorithm specific
    if (tolower(Construct[i,6][[1]]) %in% c("gbm","randomforest","automl","xgboost","lightgbm")) {

      # GBM and RF loss functions existence
      if (!(tolower(Construct[i,3][[1]]) %in% c("auto","deviance","mse", "rmse", "mae", "rmsle", "auc", "lift_top_group","misclassification", "mean_per_class_error","logloss"))) {
        j = j + 1
        set(ErrorCollection, i = j, j = 1L, value = i)
        set(ErrorCollection, i = i, j = 2L, value = c(paste0("Loss function ",Construct[i,3][[1]]," is not in list: AUTO | deviance | logloss | MSE | RMSE | MAE | RMSLE | AUC | lift_top_group | misclassification | mean_per_class_error")))
      } else {
        temp <- tolower(Construct[i,3][[1]])
        lower <- c("auto","deviance", "logloss", "mse", "rmse", "mae", "rmsle", "auc", "lift_top_group","misclassification", "mean_per_class_error")
        proper <- c("AUTO","deviance","logloss","MSE","RMSE","MAE","RMSLE","AUC","lift_top_group","misclassification","mean_per_class_error")
        distMatch <- data.table(act = rep(temp, 11), LCVals = lower, Proper = proper)
        ReplaceValue <- distMatch[act == LCVals][["Proper"]][[1]]
        set(Construct, i, 3L, value = ReplaceValue)
      }

      # GBM and RF distributions
      if (!(tolower(Construct[i,2][[1]]) %in% c("auto","bernoulli","quasibinomial","multinomial","gaussian","poisson","gamma","tweedie","laplace","quantile","huber"))) {
        set(ErrorCollection, i = j, j = 1L, value = i)
        set(ErrorCollection, i = j, j = 2L, value = c(paste0("Distribution ",Construct[i,2][[1]]," is not in list: AUTO | bernoulli | quasibinomial | multinomial | gaussian | poisson | gamma | tweedie | laplace | quantile | huber")))
      } else {
        temp <- tolower(Construct[i,2][[1]])
        lower <- c("auto","bernoulli","quasibinomial","multinomial","gaussian","poisson","gamma","tweedie","laplace","quantile","huber")
        proper <- c("AUTO","bernoulli","quasibinomial","multinomial","gaussian","poisson","gamma","tweedie","laplace","quantile","huber")
        distMatch <- data.table(act = rep(temp, 11), LCVals = lower, Proper = proper)
        ReplaceValue2 <- distMatch[act == LCVals][["Proper"]][[1]]
        set(Construct, i, 2L, value = ReplaceValue2)
      }

      # Distribution and loss combos for non-regression
      if(tolower(Construct[i,2][[1]]) %in% c("quasibinomial","binomial","bernoulli","multinomial") && !(tolower(Construct[i,3][[1]]) %in% c("auc","logloss","auto","lift_top_group","misclassification","mean_per_class_error"))) {
        j = j + 1
        set(ErrorCollection, i = j, j = 1L, value = i)
        set(ErrorCollection, i = j, j = 2L, value = c(paste0("Loss function ",Construct[i,3][[1]]," is not in list: AUC | logloss | AUTO | lift_top_group | misclassification | mean_per_class_error")))
      }

      # Distribution and loss combos for regression
      if(tolower(Construct[i,2][[1]]) %in% c("gaussian","poisson","gamma","tweedie","laplace","quantile","huber") && !(tolower(Construct[i,3][[1]]) %in% c("auto","mse", "rmse", "mae", "rmsle"))) {
        j = j + 1
        set(ErrorCollection, i = j, j = 1L, value = i)
        set(ErrorCollection, i = j, j = 2L, value = c(paste0("Loss function ",Construct[i,2][[1]]," is not in list: AUTO | MSE | RMSE | MAE | RMSLE")))
      }

      # Quantile Regression with GBM
      if(tolower(Construct[i,2][[1]]) %in% c("quantile") && (Construct[i,4][[1]] > 1 || Construct[i,4][[1]] < 0 || !is.numeric(Construct[i,4][[1]]))) {
        j = j + 1
        set(ErrorCollection, i = j, j = 1L, value = i)
        set(ErrorCollection, i = j, j = 2L, value = c(paste0("Quantiles using ",Construct[i,6][[1]]," must be a number less than or equal to 1 AND greater than or equal to 0")))
      }

      # RF Quantile regression fail
      if(tolower(Construct[i,6][[1]]) == "randomforest" && tolower(Construct[i,2][[1]]) == "quantile") {
        j = j + 1
        set(ErrorCollection, i = j, j = 1L, value = i)
        set(ErrorCollection, i = j, j = 2L, value = c(paste0("Quantile regression is only supported by GBM and Deeplearning models, not ",Construct[i,6][[1]]," models")))
      }

      # Quantile regression loss metrics
      if(tolower(Construct[i,2][[1]]) == "quantile" && tolower(Construct[i,3][[1]]) != "mae") {
        j = j + 1
        set(ErrorCollection, i = j, j = 1L, value = i)
        set(ErrorCollection, i = j, j = 2L, value = c(paste0("Quantile regression is best supported by MAE when using ",Construct[i,6][[1]]," models")))
      }

      if(tolower(Construct[i,6][[1]]) == "automl" & Construct[i,11][[1]] != TRUE) {
        j = j + 1
        set(ErrorCollection, i = j, j = 1L, value = i)
        set(ErrorCollection, i = j, j = 2L, value = c("using automl requires GridTune = TRUE"))
      }

    } else if (tolower(Construct[i,6][[1]]) == "deeplearning") {

      # Deeplearning loss functions
      if (!(tolower(Construct[i,3][[1]]) %in% c("automatic", "crossentropy", "quadratic","huber", "absolute", "quantile"))) {
        j = j + 1
        set(ErrorCollection, i = j, j = 1L, value = i)
        set(ErrorCollection, i = j, j = 2L, value = c(paste0("Loss function ",Construct[i,3][[1]]," is not in list: Automatic | CrossEntropy | Quadratic | Huber | Absolute | Quantile")))
      } else {
        temp <- tolower(Construct[i,2][[1]])
        lower <- c("auto","bernoulli","quasibinomial","multinomial","gaussian","poisson","gamma","tweedie","laplace","quantile","huber")
        proper <- c("Automatic", "CrossEntropy", "Quadratic","Huber", "Absolute", "Quantile")
        distMatch <- data.table(act = rep(temp, 11), LCVals = lower, Proper = proper)
        ReplaceVal <- distMatch[act == LCVals][["Proper"]][[1]]
        set(Construct, i, 3L, value = ReplaceVal)
      }

      # Deeplearning distributions
      if (!(tolower(Construct[i,2][[1]]) %in% c("auto", "bernoulli","multinomial", "gaussian", "poisson", "gamma", "tweedie", "laplace","quantile", "huber"))) {
        j = j + 1
        set(ErrorCollection, i = j, j = 1L, value = i)
        set(ErrorCollection, i = j, j = 2L, value = c(paste0("Distributions ",Construct[i,2][[1]]," is not in list: AUTO | bernoulli | multinomial | gaussian | poisson | gamma | tweedie | laplace | quantile | huber")))
      } else {
        temp <- tolower(Construct[i,2][[1]])
        lower <- c("auto", "bernoulli","multinomial", "gaussian", "poisson", "gamma", "tweedie", "laplace","quantile", "huber")
        proper <- c("AUTO", "bernoulli","multinomial", "gaussian", "poisson", "gamma", "tweedie", "laplace","quantile", "huber")
        distMatch <- data.table(act = rep(temp, 11), LCVals = lower, Proper = proper)
        ReplaceVal2 <- distMatch[act == LCVals][["Proper"]][[1]]
        set(Construct, i, 2L, value = ReplaceVal2)
      }

      # Distribution and loss combos for non-regression
      if(tolower(Construct[i,2][[1]]) %in% c("bernoulli","multinomial") && !(tolower(Construct[i,3][[1]]) %in% c("automatic", "crossentropy"))) {
        j = j + 1
        set(ErrorCollection, i = j, j = 1L, value = i)
        set(ErrorCollection, i = j, j = 2L, value = c(paste0("Loss function ",Construct[i,3][[1]]," is not in list: Automatic | CrossEntropy")))
      }

      # Distribution and loss combos for regression
      if(tolower(Construct[i,2][[1]]) %in% c("gaussian", "poisson", "gamma", "tweedie", "laplace","quantile", "huber") && !(tolower(Construct[i,3][[1]]) %in% c("automatic","quadratic","huber", "absolute", "quantile"))) {
        j = j + 1
        set(ErrorCollection, i = j, j = 1L, value = i)
        set(ErrorCollection, i = j, j = 2L, value = c(paste0("Loss function ",Construct[i,3][[1]]," is not in list: Automatic | Quadratic | Huber | Absolute | Quantile")))
      }

      # Quantile regression loss metrics
      if(tolower(Construct[i,2][[1]]) == "quantile" && tolower(Construct[i,3][[1]]) != "quantile") {
        j = j + 1
        set(ErrorCollection, i = j, j = 1L, value = i)
        set(ErrorCollection, i = j, j = 2L, value = c(paste0("Quantile regression needs to use Quantile for the loss function with ",Construct[i,6][[1]]," models")))
      }

      # Quantile Regression with DL
      if(tolower(Construct[i,2][[1]]) %in% c("quantile") && (Construct[i,4][[1]] > 1 || Construct[i,4][[1]] < 0 || !is.numeric(Construct[i,4][[1]]))) {
        j = j + 1
        set(ErrorCollection, i = j, j = 1L, value = i)
        set(ErrorCollection, i = j, j = 2L, value = c(paste0("Quantiles using ",Construct[i,6][[1]]," must be a number less than or equal to 1 AND greater than or equal to 0")))
      }

    } else {
      j = j + 1
      set(ErrorCollection, i = j, j = 1L, value = i)
      set(ErrorCollection, i = j, j = 2L, value = c(paste0("Models supported are: GBM, randomForest, and deeplearning, while ",Construct[i,6][[1]]," is not")))
    }
  }

  # Error stopping point and Construct file save
  ErrorCollection <- ErrorCollection[Row != -720]
  if(nrow(ErrorCollection) >= 1) {
    ErrorCollectionLog <<- ErrorCollection
    stop(print("Your model construction file has errors and an error log has been stored globally as 'ErrorCollectionLog'"))
  } else {
    save(Construct, file = paste0(model_path, "/Construct.Rdata"))
  }

  # Set up grid_tuned_paths.R file
  if (file.exists(paste0(model_path, "/grid_tuned_paths.Rdata"))) {
    load(paste0(model_path,"/grid_tuned_paths.Rdata"))
  } else {
    grid_tuned_paths <- data.table(Model     = rep("a", nrow(Construct)),
                                   Path      = rep("a", nrow(Construct)),
                                   GT_Metric = rep(1234.5678, nrow(Construct)),
                                   BL_Metric = rep(1234.5678, nrow(Construct)),
                                   BinThresh = rep(1234.5678, nrow(Construct)),
                                   PathJar   = rep("a", nrow(Construct)))
  }

  ######################################
  # Loop through model building
  ######################################

  for(i in 1:nrow(Construct)) {

    # No deeplearning loss functions as stopping metrics
    if(tolower(Construct[i,3][[1]]) == "crossentropy") {
      if(tolower(Construct[i,2][[1]]) == "multinomial") {
        StoppingMetric = "logloss"
      } else {
        StoppingMetric = "auc"
      }
    } else {
      if(tolower(Construct[i,3][[1]]) %in% c("quadratic", "huber")) {
        StoppingMetric = "mse"
      } else if (tolower(Construct[i,3][[1]]) %in% c("absolute", "quantile")) {
        StoppingMetric = "mae"
      } else {
        StoppingMetric = Construct[i,3][[1]]
      }
    }

    # Define grid tune search scheme in a named list
    search_criteria  <- list(strategy             = "RandomDiscrete",
                             max_runtime_secs     = MaxRuntimeSeconds,
                             max_models           = MaxModels,
                             seed                 = 1234,
                             stopping_rounds      = 10,
                             stopping_metric      = StoppingMetric,
                             stopping_tolerance   = 1e-3)

    # Set up H20 environment instance
    Sys.sleep(10)
    h2o.init(nthreads = nthreads, max_mem_size = max_memory, enable_assertions = FALSE)

    # Keep setting
    if(Construct[i,"SupplyData"][[1]]) {
      data_train   <- as.h2o(TrainData)
      validate     <- as.h2o(TestData)
    } else {
      data_h2o     <- eval(parse(text = paste0("as.h2o(",Construct[i,7][[1]],")")))
      data_train   <- h2o.splitFrame(data_h2o, ratios = ratios)
      train        <- data_train[[1]]
      validate     <- data_train[[2]]
    }

    # Define targets
    target         <- eval(parse(text = paste0(Construct[i,8][[1]])))
    features       <- eval(parse(text = paste0(Construct[i,9][[1]])))
    XGB            <- h2o.xgboost.available()
    if (XGB) {
      if (tolower(Construct[i,2][[1]]) != "quantile") {
        ModelExclude   <- NULL
      } else {
        ModelExclude   <- c("XGBoost","GLM","DRF")
      }
    } else {
      if (tolower(Construct[i,2][[1]]) != "quantile") {
        ModelExclude   <- c("XGBoost")
      } else {
        ModelExclude   <- c("XGBoost","GLM","DRF")
      }
    }

    N              <- length(features)
    P5             <- 2^(-1/5)
    P4             <- 2^(-1/4)
    P3             <- 2^(-1/3)
    set(grid_tuned_paths, i = i, j = 1L, value = Construct[i,5][[1]])

    ######################################
    # Target Encoding
    ######################################

    if(!is.na(Construct[i, "TargetEncoding"][[1]])) {
      TEncode <- eval(parse(text = Construct[i, "TargetEncoding"][[1]]))
      cols <- names(train)[TEncode]
      train[, Construct[i,"Targets"][[1]]] <- as.numeric(train[, Construct[i,"Targets"][[1]]])
      validate[, Construct[i,"Targets"][[1]]] <- as.numeric(validate[, Construct[i,"Targets"][[1]]])
      for (col in cols) {
        x     <- h2o.target_encode_create(data = train,
                                          x = list(col),
                                          y = Construct[i,"Targets"][[1]])
        # Apply to training data
        train <- h2o.target_encode_apply(train,
                                         x = list(col),
                                         y = Construct[i,"Targets"][[1]],
                                         target_encode_map = x,
                                         holdout_type = "None",
                                         blended_avg = TRUE,
                                         noise_level = 0)

        # Apply to validation data
        validate <- h2o.target_encode_apply(validate,
                                            x = list(col),
                                            y = Construct[i,"Targets"][[1]],
                                            target_encode_map = x,
                                            holdout_type = "None",
                                            blended_avg = TRUE,
                                            noise_level = 0)

        save(x, file = paste0(model_path, "/" , Construct[i, "Targets"][[1]], "_", col, ".Rdata"))
      }

      # Modify feature reference
      features <- c((min(features) + length(eval(parse(text = paste0(Construct[i,24][[1]]))))):max(features), (max(target) + 1):(max(target) + length(eval(parse(text = paste0(Construct[i,24][[1]]))))))

      # Turn target columns back to factor
      train[, Construct[i,"Targets"][[1]]] <- as.factor(train[, Construct[i,"Targets"][[1]]])
      validate[, Construct[i,"Targets"][[1]]] <- as.factor(validate[, Construct[i,"Targets"][[1]]])
      set(Construct, i = i, j = "PD_Data", value = "Validate")
    }

    ######################################
    # Hyperparameters
    ######################################

    # 1. Check if GridTune is true
    # 2. Check to see which model is chosen
    # 3. Check to see if this is classification / multinomial or not
    # 4. Select hyperparameter list

    if (Construct[i,11][[1]]) {
      if (tolower(Construct[i,6][[1]]) == "gbm") {
        if (tolower(Construct[i,3][[1]] %in% c("auc","logloss","auto","lift_top_group","misclassification","mean_per_class_error"))) {
          hyper_params <- list(max_depth                        = seq(5,8,1),
                               balance_classes                  = c(TRUE,FALSE),
                               ntrees                           = c(500,750,1000),
                               sample_rate                      = seq(0.5,1,0.01),
                               col_sample_rate                  = seq(0.2,1,0.01),
                               col_sample_rate_per_tree         = seq(0.2,1,0.01),
                               col_sample_rate_change_per_level = seq(0.9,1.1,0.01),
                               min_rows                         = 2^seq(0,log2(eval(parse(text=paste0("nrow(",Construct[i,7][[1]],")")))*ratios[1])-1,1),
                               nbins                            = 2^seq(4,10,1),
                               nbins_cats                       = 2^seq(4,12,1),
                               min_split_improvement            = c(0,1e-8,1e-6,1e-4),
                               histogram_type                   = c("UniformAdaptive","QuantilesGlobal","RoundRobin"))
        } else {
          hyper_params <- list(max_depth                        = seq(5,8,1),
                               ntrees                           = c(500,750,1000),
                               sample_rate                      = seq(0.5,1,0.01),
                               col_sample_rate                  = seq(0.2,1,0.01),
                               col_sample_rate_per_tree         = seq(0.2,1,0.01),
                               col_sample_rate_change_per_level = seq(0.9,1.1,0.01),
                               min_rows                         = 2^seq(0,log2(eval(parse(text=paste0("nrow(",Construct[i,7][[1]],")")))*ratios[1])-1,1),
                               nbins                            = 2^seq(4,10,1),
                               nbins_cats                       = 2^seq(4,12,1),
                               min_split_improvement            = c(0,1e-8,1e-6,1e-4),
                               histogram_type                   = c("UniformAdaptive","QuantilesGlobal","RoundRobin"))
        }

      } else if (tolower(Construct[i,6][[1]]) == "deeplearning") {
        if (tolower(Construct[i,3][[1]] %in% c("automatic", "crossentropy"))) {
          hyper_params <- list(activation = c("Rectifier", "Maxout", "Tanh", "RectifierWithDropout", "MaxoutWithDropout", "TanhWithDropout"),
                               hidden              = list(c(floor(N*P5), floor(N*P5*P5), floor(N*P5*P5*P5), floor(N*P5*P5*P5*P5), floor(N*P5*P5*P5*P5*P5)),
                                                          c(floor(N*P4), floor(N*P4*P4), floor(N*P4*P4*P4), floor(N*P4*P4*P4*P4)),
                                                          c(floor(N*P3), floor(N*P3*P3), floor(N*P3*P3*P3))),
                               balance_classes     = c(TRUE,FALSE),
                               epochs              = c(50, 100, 200),
                               l1                  = c(0, 0.00001, 0.0001),
                               l2                  = c(0, 0.00001, 0.0001),
                               rate                = c(0, 0.01, 0.005, 0.001),
                               rate_annealing      = c(1e-8, 1e-7, 1e-6),
                               rho                 = c(0.9, 0.95, 0.99, 0.999),
                               epsilon             = c(1e-10, 1e-8, 1e-6, 1e-4),
                               momentum_start      = c(0, 0.5),
                               momentum_stable     = c(0.99, 0.5, 0),
                               input_dropout_ratio = c(0, 0.1, 0.2),
                               max_w2              = c(10, 100, 1000, 3.4028235e+38))
        } else {
          hyper_params <- list(activation = c("Rectifier", "Maxout", "Tanh", "RectifierWithDropout", "MaxoutWithDropout", "TanhWithDropout"),
                               hidden              = list(c(floor(N*P5), floor(N*P5*P5), floor(N*P5*P5*P5), floor(N*P5*P5*P5*P5), floor(N*P5*P5*P5*P5*P5)),
                                                          c(floor(N*P4), floor(N*P4*P4), floor(N*P4*P4*P4), floor(N*P4*P4*P4*P4)),
                                                          c(floor(N*P3), floor(N*P3*P3), floor(N*P3*P3*P3))),
                               epochs              = c(50, 100, 200),
                               l1                  = c(0, 0.00001, 0.0001),
                               l2                  = c(0, 0.00001, 0.0001),
                               rate                = c(0, 0.01, 0.005, 0.001),
                               rate_annealing      = c(1e-8, 1e-7, 1e-6),
                               rho                 = c(0.9, 0.95, 0.99, 0.999),
                               epsilon             = c(1e-10, 1e-8, 1e-6, 1e-4),
                               momentum_start      = c(0, 0.5),
                               momentum_stable     = c(0.99, 0.5, 0),
                               input_dropout_ratio = c(0, 0.1, 0.2),
                               max_w2              = c(10, 100, 1000, 3.4028235e+38))
        }
      } else if (tolower(Construct[i,6][[1]]) == "randomforest") {
        if (tolower(Construct[i,3][[1]] %in% c("auc","logloss","auto","lift_top_group","misclassification","mean_per_class_error"))) {
          hyper_params <- list(max_depth                        = seq(5,8,1),
                               balance_classes                  = c(TRUE,FALSE),
                               ntrees                           = c(500,750,1000),
                               mtries                           = -1,
                               sample_rate                      = seq(0.2,1,0.05),
                               col_sample_rate_per_tree         = seq(0.2,1,0.05),
                               col_sample_rate_change_per_level = seq(0.9,1.1,0.01),
                               min_rows                         = 2^seq(0,log2(eval(parse(text=paste0("nrow(",Construct[i,7][[1]],")")))*ratios[1])-1,1),
                               nbins                            = 2^seq(4,10,1),
                               nbins_cats                       = 2^seq(4,12,1),
                               min_split_improvement            = c(0,1e-8,1e-6,1e-4),
                               histogram_type                   = c("UniformAdaptive","QuantilesGlobal","RoundRobin"))
        } else {
          hyper_params <- list(max_depth                        = seq(5,8,1),
                               ntrees                           = c(500,750,1000),
                               mtries                           = -1,
                               sample_rate                      = seq(0.2,1,0.05),
                               col_sample_rate_per_tree         = seq(0.2,1,0.05),
                               col_sample_rate_change_per_level = seq(0.9,1.1,0.01),
                               min_rows                         = 2^seq(0,log2(eval(parse(text=paste0("nrow(",Construct[i,7][[1]],")")))*ratios[1])-1,1),
                               nbins                            = 2^seq(4,10,1),
                               nbins_cats                       = 2^seq(4,12,1),
                               min_split_improvement            = c(0,1e-8,1e-6,1e-4),
                               histogram_type                   = c("UniformAdaptive","QuantilesGlobal","RoundRobin"))
        }
      } else if (tolower(Construct[i,6][[1]]) == "automl"){
        print("automl is preset with tuning parameters")
      } else if (tolower(Construct[i,6][[1]]) == "xgboost") {
        if (tolower(Construct[i,3][[1]] %in% c("auc","logloss","auto","lift_top_group","misclassification","mean_per_class_error"))) {
          hyper_params <- list(max_depth                        = seq(5,8,1),
                               tree_method                      = c("hist", "AUTO"),
                               grow_policy                      = c("lossguide", "depthwise"),
                               balance_classes                  = c(TRUE,FALSE),
                               ntrees                           = c(500,750,1000),
                               sample_rate                      = seq(0.5,1,0.01),
                               col_sample_rate                  = seq(0.2,1,0.01),
                               col_sample_rate_per_tree         = seq(0.2,1,0.01),
                               reg_lambda                       = c(0.001, 0.01,0.05),
                               min_rows                         = 2^seq(0,log2(eval(parse(text=paste0("nrow(",Construct[i,7][[1]],")")))*ratios[1])-1,1),
                               min_split_improvement            = c(0,1e-8,1e-6,1e-4))
        } else {
          hyper_params <- list(max_depth                        = seq(5,8,1),
                               ntrees                           = c(500,750,1000),
                               sample_rate                      = seq(0.5,1,0.01),
                               col_sample_rate                  = seq(0.2,1,0.01),
                               col_sample_rate_per_tree         = seq(0.2,1,0.01),
                               reg_lambda                       = c(0.001, 0.01,0.05),
                               min_rows                         = 2^seq(0,log2(eval(parse(text=paste0("nrow(",Construct[i,7][[1]],")")))*ratios[1])-1,1),
                               min_split_improvement            = c(0,1e-8,1e-6,1e-4))
        }
      } else if (tolower(Construct[i,6][[1]]) == "lightgbm") {
        if (tolower(Construct[i,3][[1]] %in% c("auc","logloss","auto","lift_top_group","misclassification","mean_per_class_error"))) {
          hyper_params <- list(max_depth                        = seq(5,8,1),
                               tree_method                      = c("hist"),
                               grow_policy                      = c("lossguide"),
                               balance_classes                  = c(TRUE,FALSE),
                               ntrees                           = c(500,750,1000),
                               sample_rate                      = seq(0.5,1,0.01),
                               col_sample_rate                  = seq(0.2,1,0.01),
                               col_sample_rate_per_tree         = seq(0.2,1,0.01),
                               reg_lambda                       = c(0.001, 0.01,0.05),
                               min_rows                         = 2^seq(0,log2(eval(parse(text=paste0("nrow(",Construct[i,7][[1]],")")))*ratios[1])-1,1),
                               min_split_improvement            = c(0,1e-8,1e-6,1e-4))
        } else {
          hyper_params <- list(max_depth                        = seq(5,8,1),
                               ntrees                           = c(500,750,1000),
                               sample_rate                      = seq(0.5,1,0.01),
                               col_sample_rate                  = seq(0.2,1,0.01),
                               col_sample_rate_per_tree         = seq(0.2,1,0.01),
                               reg_lambda                       = c(0.001, 0.01,0.05),
                               min_rows                         = 2^seq(0,log2(eval(parse(text=paste0("nrow(",Construct[i,7][[1]],")")))*ratios[1])-1,1),
                               min_split_improvement            = c(0,1e-8,1e-6,1e-4))
        }
      }
    }

    ######################################
    # Grid Tune Models
    ######################################

    # Check to see if GridTune is TRUE
    # Check to see if Distribution is quantile
    # Select model

    # Grid tuned model build
    if (Construct[i,11][[1]]) {
      if(tolower(Construct[i,2][[1]]) == "quantile") {
        if(tolower(Construct[i,6][[1]]) == "gbm") {
          grid <- h2o.grid(hyper_params         = hyper_params,
                           search_criteria      = search_criteria,
                           algorithm            = Construct[i,6][[1]],
                           grid_id              = Construct[i,5][[1]],
                           x                    = features,
                           y                    = target,
                           training_frame       = train,
                           validation_frame     = validate,
                           distribution         = Construct[i,2][[1]],
                           quantile_alpha       = Construct[i,4][[1]],
                           learn_rate           = 0.05,
                           learn_rate_annealing = 0.99,
                           max_runtime_secs     = MaxRuntimeSeconds,
                           stopping_rounds      = 5,
                           stopping_tolerance   = 1e-4,
                           stopping_metric      = StoppingMetric,
                           score_tree_interval  = 10,
                           seed                 = 1234)
        } else if (tolower(Construct[i,6][[1]]) == "deeplearning") {
          grid <- h2o.grid(hyper_params         = hyper_params,
                           search_criteria      = search_criteria,
                           algorithm            = Construct[i,6][[1]],
                           grid_id              = Construct[i,5][[1]],
                           x                    = features,
                           y                    = target,
                           training_frame       = train,
                           validation_frame     = validate,
                           distribution         = Construct[i,2][[1]],
                           quantile_alpha       = Construct[i,4][[1]],
                           seed                 = 42)
        }
      } else {
        if (tolower(Construct[i,6][[1]]) == "gbm") {
          grid <- h2o.grid(hyper_params         = hyper_params,
                           search_criteria      = search_criteria,
                           algorithm            = Construct[i,6][[1]],
                           grid_id              = Construct[i,5][[1]],
                           x                    = features,
                           y                    = target,
                           training_frame       = train,
                           validation_frame     = validate,
                           distribution         = Construct[i,2][[1]],
                           learn_rate           = 0.05,
                           learn_rate_annealing = 0.99,
                           max_runtime_secs     = MaxRuntimeSeconds,
                           stopping_rounds      = 5,
                           stopping_tolerance   = 1e-4,
                           stopping_metric      = StoppingMetric,
                           score_tree_interval  = 10,
                           seed                 = 1234)
        } else if (tolower(Construct[i,6][[1]]) == "deeplearning") {
          grid <- h2o.grid(hyper_params         = hyper_params,
                           search_criteria      = search_criteria,
                           algorithm            = Construct[i,6][[1]],
                           grid_id              = Construct[i,5][[1]],
                           x                    = features,
                           y                    = target,
                           training_frame       = train,
                           validation_frame     = validate,
                           distribution         = Construct[i,2][[1]],
                           seed                 = 42)
        } else if (tolower(Construct[i,6][[1]]) == "randomforest") {
          grid <- h2o.grid(hyper_params         = hyper_params,
                           search_criteria      = search_criteria,
                           algorithm            = Construct[i,6][[1]],
                           grid_id              = Construct[i,5][[1]],
                           x                    = features,
                           y                    = target,
                           training_frame       = train,
                           validation_frame     = validate,
                           max_runtime_secs     = MaxRuntimeSeconds,
                           stopping_rounds      = 5,
                           stopping_tolerance   = 1e-4,
                           stopping_metric      = StoppingMetric,
                           score_tree_interval  = 10,
                           seed                 = 1234)
        } else if (tolower(Construct[i,6][[1]]) == "automl") {
          aml <- h2o.automl(x                  = features,
                            y                  = target,
                            training_frame     = train,
                            validation_frame   = validate,
                            max_models         = MaxModels,
                            max_runtime_secs   = MaxRuntimeSeconds,
                            stopping_metric    = StoppingMetric,
                            stopping_tolerance = 1e-4,
                            stopping_rounds    = 10,
                            project_name       = "TestAML",
                            exclude_algos      = ModelExclude,
                            sort_metric        = StoppingMetric)
        } else if (tolower(Construct[i,6][[1]]) == "xgboost") {
          grid <- h2o.grid(hyper_params         = hyper_params,
                           search_criteria      = search_criteria,
                           algorithm            = Construct[i,6][[1]],
                           grid_id              = Construct[i,5][[1]],
                           x                    = features,
                           y                    = target,
                           training_frame       = train,
                           validation_frame     = validate,
                           categorical_encoding = "Enum",
                           distribution         = Construct[i,2][[1]],
                           learn_rate           = 0.05,
                           max_runtime_secs     = MaxRuntimeSeconds,
                           stopping_rounds      = 5,
                           stopping_tolerance   = 1e-4,
                           stopping_metric      = StoppingMetric,
                           score_tree_interval  = 10,
                           seed                 = 1234)
        } else if (tolower(Construct[i,6][[1]]) == "lightgbm") {
          grid <- h2o.grid(hyper_params         = hyper_params,
                           search_criteria      = search_criteria,
                           algorithm            = Construct[i,6][[1]],
                           grid_id              = Construct[i,5][[1]],
                           x                    = features,
                           y                    = target,
                           training_frame       = train,
                           validation_frame     = validate,
                           categorical_encoding = "Enum",
                           distribution         = Construct[i,2][[1]],
                           learn_rate           = 0.05,
                           max_runtime_secs     = MaxRuntimeSeconds,
                           stopping_rounds      = 5,
                           stopping_tolerance   = 1e-4,
                           stopping_metric      = StoppingMetric,
                           score_tree_interval  = 10,
                           seed                 = 1234)
        }
      }

      # Store all models built sorted by metric
      if (tolower(Construct[i,6][[1]]) == "automl") {
        Grid_Out <- h2o.getAutoML(project_name = "TestAML")
      } else if (tolower(Construct[i,2][[1]]) %in% c("quasibinomial","binomial","bernoulli","multinomial")) {
        Decreasing = TRUE
        Grid_Out   <- h2o.getGrid(grid_id = Construct[i,5][[1]], sort_by = StoppingMetric, decreasing = Decreasing)
      } else {
        Decreasing = FALSE
        Grid_Out   <- h2o.getGrid(grid_id = Construct[i,5][[1]], sort_by = StoppingMetric, decreasing = Decreasing)
      }

      # Store best model
      if (tolower(Construct[i,6][[1]]) == "automl") {
        best_model <- Grid_Out@leader
      } else {
        best_model <- h2o.getModel(Grid_Out@model_ids[[1]])
      }

      # Collect accuracy metric on validation data
      if(tolower(Construct[i,3][[1]]) == "crossentropy") {
        if(tolower(Construct[i,2][[1]]) == "multinomial") {
          cc <- h2o.logloss(h2o.performance(best_model, valid = TRUE))
        } else {
          cc <- h2o.auc(h2o.performance(best_model, valid = TRUE))
        }
      } else if(tolower(Construct[i,3][[1]]) == "absolute") {
        cc <- h2o.mae(h2o.performance(best_model, valid = TRUE))
      } else if(tolower(Construct[i,3][[1]]) %in% c("quadratic","huber")) {
        cc <- h2o.mse(h2o.performance(best_model, valid = TRUE))
      } else {
        cc <- eval(parse(text = paste0("h2o.", tolower(StoppingMetric), "(h2o.performance(best_model, valid = TRUE))")))
      }
      # Store results in metadata file
      set(grid_tuned_paths, i = i, j = 3L, value = cc)
    }

    ######################################
    # Baseline Models
    ######################################

    # Check to see if quantile is selected
    # Choose model
    if(tolower(Construct[i,6][[1]]) != "automl") {
      if(tolower(Construct[i,2][[1]]) == "quantile") {
        if(tolower(Construct[i,6][[1]]) == "gbm") {
          bl_model <- h2o.gbm(x                = features,
                              y                = target,
                              training_frame   = train,
                              validation_frame = validate,
                              distribution     = Construct[i,2][[1]],
                              quantile_alpha   = Construct[i,4][[1]],
                              model_id         = paste0("BL_GBM_",Construct[i,5][[1]]),
                              ntrees           = BL_Trees)
        } else if (tolower(Construct[i,6][[1]]) == "deeplearning") {
          bl_model <- h2o.deeplearning(x                = features,
                                       y                = target,
                                       hidden           = c(floor(N*P4), floor(N*P4*P4), floor(N*P4*P4*P4), floor(N*P4*P4*P4*P4)),
                                       training_frame   = train,
                                       validation_frame = validate,
                                       distribution     = Construct[i,2][[1]],
                                       model_id         = paste0("BL_DL_",Construct[i,5][[1]]),
                                       quantile_alpha   = Construct[i,4][[1]])
        }
      } else if (tolower(Construct[i,6][[1]]) == "gbm") {
        bl_model <- h2o.gbm(x                = features,
                            y                = target,
                            training_frame   = train,
                            validation_frame = validate,
                            distribution     = Construct[i,2][[1]],
                            model_id         = paste0("BL_GBM_",Construct[i,5][[1]]),
                            ntrees           = BL_Trees)
      } else if (tolower(Construct[i,6][[1]]) == "deeplearning") {
        bl_model <- h2o.deeplearning(x                = features,
                                     y                = target,
                                     hidden           = c(floor(N*P4), floor(N*P4*P4), floor(N*P4*P4*P4), floor(N*P4*P4*P4*P4)),
                                     training_frame   = train,
                                     validation_frame = validate,
                                     model_id         = paste0("BL_DL_",Construct[i,5][[1]]),
                                     distribution     = Construct[i,2][[1]])
      } else if (tolower(Construct[i,6][[1]]) == "randomforest") {
        bl_model <- h2o.randomForest(x                = features,
                                     y                = target,
                                     training_frame   = train,
                                     validation_frame = validate,
                                     model_id         = paste0("BL_RF_",Construct[i,5][[1]]),
                                     ntrees           = BL_Trees)
      } else if (tolower(Construct[i,6][[1]]) == "xgboost") {
        bl_model <- h2o.xgboost(x                = features,
                                y                = target,
                                training_frame   = train,
                                validation_frame = validate,
                                categorical_encoding = "Enum",
                                distribution     = Construct[i,2][[1]],
                                model_id         = paste0("BL_XG_",Construct[i,5][[1]]),
                                ntrees           = BL_Trees)
      } else if (tolower(Construct[i,6][[1]]) == "lightgbm") {
        bl_model <- h2o.xgboost(x                = features,
                                y                = target,
                                training_frame   = train,
                                validation_frame = validate,
                                categorical_encoding = "Enum",
                                distribution     = Construct[i,2][[1]],
                                tree_method      = "hist",
                                grow_policy      = "lossguide",
                                model_id         = paste0("BL_lgbm_",Construct[i,5][[1]]),
                                ntrees           = BL_Trees)
      }

      # Collect accuracy metric on validation data
      if(tolower(Construct[i,3][[1]]) == "crossentropy") {
        if(tolower(Construct[i,2][[1]]) == "multinomial") {
          dd <- h2o.logloss(h2o.performance(bl_model, valid = TRUE))
        } else {
          dd <- h2o.auc(h2o.performance(bl_model, valid = TRUE))
        }
      } else if(tolower(Construct[i,3][[1]]) == "absolute") {
          dd <- h2o.mae(h2o.performance(bl_model, valid = TRUE))
      } else if(tolower(Construct[i,3][[1]]) %in% c("quadratic","huber")) {
          dd <- h2o.mse(h2o.performance(bl_model, valid = TRUE))
      } else {
        dd <- eval(parse(text = paste0("h2o.", tolower(StoppingMetric), "(h2o.performance(bl_model, valid = TRUE))")))
      }

      # Store results in metadata file
      set(grid_tuned_paths, i = i, j = 4L, value = dd)
    }


    ######################################
    # Model Evaluation & Saving
    ######################################

    # Check to see if GridTune is TRUE
    # Check to see if Distribution is multinomial
    # Proceed

    if(tolower(Construct[i,6][[1]] == "automl")) {
      if(Construct[i,21][[1]] == TRUE) {
        if(grid_tuned_paths[i,2][[1]] != "a") file.remove(grid_tuned_paths[i,2][[1]])
        if(tolower(Construct[i,22][[1]]) == "standard") {
          save_model <- h2o.saveModel(object = best_model, path = model_path, force = TRUE)
          set(grid_tuned_paths, i = i, j = 2L, value = save_model)
          save(grid_tuned_paths, file = paste0(model_path, "/grid_tuned_paths.Rdata"))
        } else {
          save_model <- h2o.saveMojo(object = best_model, path = model_path, force = TRUE)
          h2o.download_mojo(model = best_model, path = model_path, get_genmodel_jar = TRUE, genmodel_path = model_path, genmodel_name = Construct[i,5][[1]])
          set(grid_tuned_paths, i = i, j = 2L, value = save_model)
          set(grid_tuned_paths, i = i, j = 6L, value = Construct[i,5][[1]])
          save(grid_tuned_paths, file = paste0(model_path, "/grid_tuned_paths.Rdata"))
        }
      }

      # Save VarImp and VarNOTImp
      if(best_model@algorithm != "stackedensemble") {
        VIMP <- as.data.table(h2o.varimp(best_model))
        save(VIMP, file = paste0(model_path, "/VarImp_", Construct[i,5][[1]],".Rdata"))
        if(tolower(best_model@algorithm) != "glm") {
          NIF <- VIMP[percentage < Construct[i,16][[1]], 1][[1]]
        } else {
          NIF <- NULL
        }
        if (length(NIF) > 0) {
          save(NIF, file = paste0(model_path, "/VarNOTImp_", Construct[i,5][[1]],".Rdata"))
        }
      } else {
        set(Construct, i = i, j = 13L, value = 0)
      }

      # Gather predicted values
      preds <- h2o.predict(best_model, newdata = validate)[,1]
      if(Construct[i,14][[1]] == "All") {
        predsPD <- h2o.predict(best_model, newdata = data_h2o)[,1]
        PredsPD <- as.data.table(predsPD)
        fwrite(PredsPD, file = paste0(model_path, "/",Construct[i,5][[1]],"_PredsAll.csv"))
      } else if (Construct[i,14][[1]] == "Train") {
        predsPD <- h2o.predict(best_model, newdata = train)[,1]
      } else if (Construct[i,14][[1]] == "Validate") {
        predsPD <- h2o.predict(best_model, newdata = validate)[,1]
      }
    }

    if (Construct[i,11][[1]] == TRUE & tolower(Construct[i,6][[1]]) != "automl") {
      if(!(tolower(Construct[i,2][[1]]) %in% c("quasibinomial","binomial","bernoulli")) | tolower(Construct[i,3][[1]]) == "logloss") {
        if(cc < dd) {
          # Save model
          if(Construct[i,21][[1]] == TRUE) {
            if(grid_tuned_paths[i,2][[1]] != "a") file.remove(grid_tuned_paths[i,2][[1]])
            if(tolower(Construct[i,22][[1]]) == "standard") {
              save_model <- h2o.saveModel(object = best_model, path = model_path, force = TRUE)
              set(grid_tuned_paths, i = i, j = 2L, value = save_model)
              save(grid_tuned_paths, file = paste0(model_path, "/grid_tuned_paths.Rdata"))
            } else {
              save_model <- h2o.saveMojo(object = best_model, path = model_path, force = TRUE)
              h2o.download_mojo(model = best_model, path = model_path, get_genmodel_jar = TRUE, genmodel_path = model_path, genmodel_name = Construct[i,5][[1]])
              set(grid_tuned_paths, i = i, j = 2L, value = save_model)
              set(grid_tuned_paths, i = i, j = 6L, value = Construct[i,5][[1]])
              save(grid_tuned_paths, file = paste0(model_path, "/grid_tuned_paths.Rdata"))
            }
          }

          # Save VarImp and VarNOTImp
          VIMP <- as.data.table(h2o.varimp(best_model))
          save(VIMP, file = paste0(model_path, "/VarImp_", Construct[i,5][[1]],".Rdata"))
          NIF <- VIMP[percentage < Construct[i,16][[1]], 1][[1]]
          if (length(NIF) > 0) {
            save(NIF, file = paste0(model_path, "/VarNOTImp_", Construct[i,5][[1]],".Rdata"))
          }

          # Gather predicted values
          preds <- h2o.predict(best_model, newdata = validate)[,1]
          if(Construct[i,14][[1]] == "All") {
            predsPD <- h2o.predict(best_model, newdata = data_h2o)[,1]
            PredsPD <- as.data.table(predsPD)
            fwrite(PredsPD, file = paste0(model_path, "/",Construct[i,5][[1]],"_PredsAll.csv"))
          } else if (Construct[i,14][[1]] == "Train") {
            predsPD <- h2o.predict(best_model, newdata = train)[,1]
          } else if (Construct[i,14][[1]] == "Validate") {
            predsPD <- h2o.predict(best_model, newdata = validate)[,1]
          }
        } else {
          # Save model
          if(Construct[i,21][[1]] == TRUE) {
            if(grid_tuned_paths[i,2][[1]] != "a") file.remove(grid_tuned_paths[i,2][[1]])
            if(tolower(Construct[i,22][[1]]) == "standard") {
              save_model <- h2o.saveModel(object = bl_model, path = model_path, force = TRUE)
              set(grid_tuned_paths, i = i, j = 2L, value = save_model)
              save(grid_tuned_paths, file = paste0(model_path, "/grid_tuned_paths.Rdata"))
            } else {
              save_model <- h2o.saveMojo(object = bl_model, path = model_path, force = TRUE)
              h2o.download_mojo(model = bl_model, path = model_path, get_genmodel_jar = TRUE, genmodel_path = model_path, genmodel_name = Construct[i,5][[1]])
              set(grid_tuned_paths, i = i, j = 2L, value = save_model)
              set(grid_tuned_paths, i = i, j = 6L, value = Construct[i,5][[1]])
              save(grid_tuned_paths, file = paste0(model_path, "/grid_tuned_paths.Rdata"))
            }
          }

          # Save VarImp
          VIMP <- as.data.table(h2o.varimp(bl_model))
          save(VIMP, file = paste0(model_path, "/VarImp_", Construct[i,5][[1]], ".Rdata"))
          NIF <- VIMP[percentage < Construct[i,16][[1]], 1][[1]]
          if (length(NIF) > 0) {
            save(NIF, file = paste0(model_path, "/VarNOTImp_", Construct[i,5][[1]],".Rdata"))
          }

          # Gather predicted values
          preds <- h2o.predict(bl_model, newdata = validate)[,1]
          if(Construct[i,14][[1]] == "All") {
            predsPD <- h2o.predict(bl_model, newdata = data_h2o)[,1]
            PredsPD <- as.data.table(predsPD)
            fwrite(PredsPD, file = paste0(model_path, "/",Construct[i,5][[1]],"_PredsAll.csv"))
          } else if (Construct[i,14][[1]] == "Train") {
            predsPD <- h2o.predict(bl_model, newdata = train)[,1]
          } else if (Construct[i,14][[1]] == "Validate") {
            predsPD <- h2o.predict(bl_model, newdata = validate)[,1]
          }
        }
      } else {
        if(cc > dd) {
          # Save model
          if(Construct[i,21][[1]] == TRUE) {
            if(grid_tuned_paths[i,2][[1]] != "a") file.remove(grid_tuned_paths[i,2][[1]])
            if(tolower(Construct[i,22][[1]]) == "standard") {
              save_model <- h2o.saveModel(object = best_model, path = model_path, force = TRUE)
              set(grid_tuned_paths, i = i, j = 2L, value = save_model)
              save(grid_tuned_paths, file = paste0(model_path, "/grid_tuned_paths.Rdata"))
            } else {
              save_model <- h2o.saveMojo(object = best_model, path = model_path, force = TRUE)
              h2o.download_mojo(model = best_model, path = model_path, get_genmodel_jar = TRUE, genmodel_path = model_path, genmodel_name = Construct[i,5][[1]])
              set(grid_tuned_paths, i = i, j = 2L, value = save_model)
              set(grid_tuned_paths, i = i, j = 6L, value = Construct[i,5][[1]])
              save(grid_tuned_paths, file = paste0(model_path, "/grid_tuned_paths.Rdata"))
            }
          }

          # Store threshold
          store_results <- data.table(best_model@model$training_metrics@metrics$thresholds_and_metric_scores)
          if (Construct[i,15][[1]] == "f1" || is.null(Construct[i,15][[1]])) {
            Thresh <<- tryCatch({store_results[order(-f1)][1,1][[1]]}, error = function(x) 1)
            Label  <<- "f1"
          } else if (Construct[i,15][[1]] == "f2") {
            Thresh <<- tryCatch({store_results[order(-f2)][1,1][[1]]}, error = function(x) 1)
            Label  <<- "f2"
          } else if (Construct[i,15][[1]] == "f0point5") {
            Thresh <<- tryCatch({store_results[order(-f0point5)][1,1][[1]]}, error = function(x) 1)
            Label <<- "f0point5"
          } else if (Construct[i,15][[1]] == "CS") {
            predsPDD <- h2o.predict(bl_model, newdata = data_h2o)[,3]
            data    <- as.data.table(h2o.cbind(data_h2o, predsPDD))
            data[, eval(Construct[i,1][[1]]) := as.numeric(as.character(get(Construct[i,1][[1]])))]
            temp  <- threshOptim(data     = data,
                                 actTar   = Construct[i,1][[1]],
                                 predTar  = 'p1',
                                 tpProfit = Construct[i,17][[1]],
                                 tnProfit = Construct[i,18][[1]],
                                 fpProfit = Construct[i,19][[1]],
                                 fnProfit = Construct[i,20][[1]])
            Thresh <<- temp[[1]]
            Label <<- "CS"
          }
          set(grid_tuned_paths, i = i, j = 5L, value = Thresh)

          # Save VarImp
          VIMP <- as.data.table(h2o.varimp(best_model))
          save(VIMP, file = paste0(model_path, "/VarImp_", Construct[i,5][[1]],".Rdata"))
          NIF <- VIMP[percentage < Construct[i,16][[1]], 1][[1]]
          if (length(NIF) > 0) {
            save(NIF, file = paste0(model_path, "/VarNOTImp_", Construct[i,5][[1]],".Rdata"))
          }

          # Gather predicted values
          preds <- h2o.predict(best_model, newdata = validate)[,3]
          if(Construct[i,14][[1]] == "All") {
            predsPD <- h2o.predict(best_model, newdata = data_h2o)[,3]
            PredsPD <- as.data.table(predsPD)
            fwrite(PredsPD, file = paste0(model_path, "/",Construct[i,5][[1]],"_PredsAll.csv"))
          } else if (Construct[i,14][[1]] == "Train") {
            predsPD <- h2o.predict(best_model, newdata = train)[,3]
          } else if (Construct[i,14][[1]] == "Validate") {
            predsPD <- h2o.predict(best_model, newdata = validate)[,3]
          }
        } else {
          # Save model
          if(Construct[i,21][[1]] == TRUE) {
            if(grid_tuned_paths[i,2][[1]] != "a") file.remove(grid_tuned_paths[i,2][[1]])
            if(tolower(Construct[i,22][[1]]) == "standard") {
              save_model <- h2o.saveModel(object = bl_model, path = model_path, force = TRUE)
              set(grid_tuned_paths, i = i, j = 2L, value = save_model)
              save(grid_tuned_paths, file = paste0(model_path, "/grid_tuned_paths.Rdata"))
            } else {
              save_model <- h2o.saveMojo(object = bl_model, path = model_path, force = TRUE)
              h2o.download_mojo(model = bl_model, path = model_path, get_genmodel_jar = TRUE, genmodel_path = model_path, genmodel_name = Construct[i,5][[1]])
              set(grid_tuned_paths, i = i, j = 2L, value = save_model)
              set(grid_tuned_paths, i = i, j = 6L, value = Construct[i,5][[1]])
              save(grid_tuned_paths, file = paste0(model_path, "/grid_tuned_paths.Rdata"))
            }
          }

          # Store threshold
          store_results <- data.table(bl_model@model$training_metrics@metrics$thresholds_and_metric_scores)
          if (Construct[i,15][[1]] == "f1" || is.null(Construct[i,15][[1]])) {
            Thresh <<- tryCatch({store_results[order(-f1)][1,1][[1]]}, error = function(x) 1)
            Label  <<- "f1"
          } else if (Construct[i,15][[1]] == "f2") {
            Thresh <<- tryCatch({store_results[order(-f2)][1,1][[1]]}, error = function(x) 1)
            Label  <<- "f2"
          } else if (Construct[i,15][[1]] == "f0point5") {
            Thresh <<- tryCatch({store_results[order(-f0point5)][1,1][[1]]}, error = function(x) 1)
            Label <<- "f0point5"
          } else if (Construct[i,15][[1]] == "CS") {
            predsPDD <- h2o.predict(bl_model, newdata = data_h2o)[,3]
            data    <- as.data.table(h2o.cbind(data_h2o, predsPDD))
            data[, eval(Construct[i,1][[1]]) := as.numeric(as.character(get(Construct[i,1][[1]])))]
            temp  <- threshOptim(data     = data,
                                 actTar   = Construct[i,1][[1]],
                                 predTar  = 'p1',
                                 tpProfit = Construct[i,17][[1]],
                                 tnProfit = Construct[i,18][[1]],
                                 fpProfit = Construct[i,19][[1]],
                                 fnProfit = Construct[i,20][[1]])
            Thresh <<- temp[[1]]
            Label <<- "CS"
          }
          set(grid_tuned_paths, i = i, j = 5L, value = Thresh)

          # Save VarImp
          VIMP <- as.data.table(h2o.varimp(bl_model))
          save(VIMP, file = paste0(model_path, "/VarImp_", Construct[i,5][[1]], ".Rdata"))
          NIF <- VIMP[percentage < Construct[i,16][[1]], 1][[1]]
          if (length(NIF) > 0) {
            save(NIF, file = paste0(model_path, "/VarNOTImp_", Construct[i,5][[1]],".Rdata"))
          }

          # Gather predicted values
          preds <- h2o.predict(bl_model, newdata = validate)[,3]
          if(Construct[i,14][[1]] == "All") {
            predsPD <- h2o.predict(bl_model, newdata = data_h2o)[,3]
            PredsPD <- as.data.table(predsPD)
            fwrite(PredsPD, file = paste0(model_path, "/",Construct[i,5][[1]],"_PredsAll.csv"))
          } else if (Construct[i,14][[1]] == "Train") {
            predsPD <- h2o.predict(bl_model, newdata = train)[,3]
          } else if (Construct[i,14][[1]] == "Validate") {
            predsPD <- h2o.predict(bl_model, newdata = validate)[,3]
          }
        }
      }
    } else if (tolower(Construct[i,6][[1]]) != "automl") {
      # Save model
      if(Construct[i,21][[1]] == TRUE) {
        if(grid_tuned_paths[i,2][[1]] != "a") file.remove(grid_tuned_paths[i,2][[1]])
        if(tolower(Construct[i,22][[1]]) == "standard") {
          save_model <- h2o.saveModel(object = bl_model, path = model_path, force = TRUE)
          set(grid_tuned_paths, i = i, j = 2L, value = save_model)
          save(grid_tuned_paths, file = paste0(model_path, "/grid_tuned_paths.Rdata"))
        } else {
          save_model <- h2o.saveMojo(object = bl_model, path = model_path, force = TRUE)
          h2o.download_mojo(model = bl_model, path = model_path, get_genmodel_jar = TRUE, genmodel_path = model_path, genmodel_name = Construct[i,5][[1]])
          set(grid_tuned_paths, i = i, j = 2L, value = save_model)
          set(grid_tuned_paths, i = i, j = 6L, value = Construct[i,5][[1]])
          save(grid_tuned_paths, file = paste0(model_path, "/grid_tuned_paths.Rdata"))
        }
      }

      # Store threshold for binary classification
      if(tolower(Construct[i,2][[1]]) %in% c("quasibinomial","binomial","bernoulli")) {
        store_results <- data.table(bl_model@model$training_metrics@metrics$thresholds_and_metric_scores)
        if (Construct[i,15][[1]] == "f1" || is.null(Construct[i,15][[1]])) {
          Thresh <<- tryCatch({store_results[order(-f1)][1,1][[1]]}, error = function(x) 1)
          Label  <<- "f1"
        } else if (Construct[i,15][[1]] == "f2") {
          Thresh <<- tryCatch({store_results[order(-f2)][1,1][[1]]}, error = function(x) 1)
          Label  <<- "f2"
        } else if (Construct[i,15][[1]] == "f0point5") {
          Thresh <<- tryCatch({store_results[order(-f0point5)][1,1][[1]]}, error = function(x) 1)
          Label <<- "f0point5"
        } else if (Construct[i,15][[1]] == "CS") {
          predsPDD <- h2o.predict(bl_model, newdata = data_h2o)[,3]
          data    <- as.data.table(h2o.cbind(data_h2o, predsPDD))
          data[, eval(Construct[i,1][[1]]) := as.numeric(as.character(get(Construct[i,1][[1]])))]
          temp  <- threshOptim(data     = data,
                               actTar   = Construct[i,1][[1]],
                               predTar  = 'p1',
                               tpProfit = Construct[i,17][[1]],
                               tnProfit = Construct[i,18][[1]],
                               fpProfit = Construct[i,19][[1]],
                               fnProfit = Construct[i,20][[1]])
          Thresh <<- temp[[1]]
          Label <<- "CS"
        }
        set(grid_tuned_paths, i = i, j = 5L, value = Thresh)
        preds <- h2o.predict(bl_model, newdata = validate)[,3]
        if(tolower(Construct[i,14][[1]]) == "all") {
          predsPD <- h2o.predict(bl_model, newdata = data_h2o)[,3]
          PredsPD <- as.data.table(predsPD)
          fwrite(PredsPD, file = paste0(model_path, "/",Construct[i,5][[1]],"_PredsAll.csv"))
        } else if (tolower(Construct[i,14][[1]]) == "train") {
          predsPD <- h2o.predict(bl_model, newdata = train)[,3]
        } else if (tolower(Construct[i,14][[1]]) == "validate") {
          predsPD <- h2o.predict(bl_model, newdata = validate)[,3]
        }
      } else {
        # Store predicted values against validate data for calibration plot
        preds <- h2o.predict(bl_model, newdata = validate)[,1]
        if(tolower(Construct[i,14][[1]]) == "all") {
          predsPD <- h2o.predict(bl_model, newdata = data_h2o)[,1]
          PredsPD <- as.data.table(predsPD)
          fwrite(PredsPD, file = paste0(model_path, "/",Construct[i,5][[1]],"_PredsAll.csv"))
        } else if (tolower(Construct[i,14][[1]]) == "train") {
          predsPD <- h2o.predict(bl_model, newdata = train)[,1]
        } else if (tolower(Construct[i,14][[1]]) == "validate") {
          predsPD <- h2o.predict(bl_model, newdata = validate)[,1]
        }
      }

      # Save VarImp
      VIMP <- as.data.table(h2o.varimp(bl_model))
      save(VIMP, file = paste0(model_path, "/VarImp_", Construct[i,5][[1]], ".Rdata"))
      NIF <- VIMP[percentage < Construct[i,16][[1]], 1][[1]]
      if (length(NIF) > 0) {
        save(NIF, file = paste0(model_path, "/VarNOTImp_", Construct[i,5][[1]],".Rdata"))
      }
    }

    ######################################
    # Model Evaluation Plots
    ######################################

    # Generate plots
    col <- Construct[i,1][[1]]
    calibration <- as.data.table(h2o.cbind(preds, validate[, col]))
    if (tolower(Construct[i,2][[1]]) %in% c("quasibinomial","binomial","bernoulli")) {
      calibration[, eval(col) := as.numeric(as.character(get(col)))]
    }
    if(Construct[i,13][[1]] >= 1) {
      if (tolower(Construct[i,14][[1]]) == "all") {
        calibEval <- as.data.table(h2o.cbind(preds, validate))
        calib <- as.data.table(h2o.cbind(predsPD, data_h2o))
      } else if (tolower(Construct[i,14][[1]]) == "train") {
        calibEval <- as.data.table(h2o.cbind(preds, validate))
        calib <- as.data.table(h2o.cbind(predsPD, train))
      } else if (tolower(Construct[i,14][[1]]) == "validate") {
        calibEval <- as.data.table(h2o.cbind(preds, validate))
        calib <- as.data.table(h2o.cbind(predsPD, validate))
      }
      if(Construct[i,12][[1]]) {
        save(calibEval, file = paste0(model_path,"/",Construct[i,5][[1]],".Rdata"))
      }
    } else {
      if(Construct[i,12][[1]]) {
        calibEval <- as.data.table(h2o.cbind(preds, validate))
        save(calibEval, file = paste0(model_path,"/",Construct[i,5][[1]],".Rdata"))
      }
    }
    predName <- names(calibration[,1])

    # Generate evaluation plots
    if (tolower(Construct[i,2][[1]]) != "multinomial") {
      if (tolower(Construct[i,2][[1]]) == "quantile") {

        # Calibration plot
        out1 <- EvalPlot(calibration,
                         PredColName = predName,
                         ActColName  = Construct[i,1][[1]],
                         type        = "calibration",
                         bucket      = 0.05,
                         aggrfun     = function(x) quantile(x, probs = Construct[i,4][[1]], na.rm = TRUE))
        ggsave(paste0(model_path, "/CalP_", Construct[i,5][[1]], ".png"))

        # Calibration boxplot
        out2 <- EvalPlot(calibration,
                         PredColName = predName,
                         ActColName  = Construct[i,1][[1]],
                         type        = "boxplot",
                         bucket      = 0.05)
        ggsave(paste0(model_path, "/CalBP_", Construct[i,5][[1]], ".png"))
      } else if (tolower(Construct[i,2][[1]]) %in% c("quasibinomial","binomial","bernoulli")) {

        # Calibration plot
        #interc <<- mean(calibration[[eval(predName)]], na.rm = TRUE)
        out1 <- EvalPlot(calibration,
                         PredColName = predName,
                         ActColName  = Construct[i,1][[1]],
                         type        = "calibration",
                         bucket      = 0.05,
                         aggrfun     = function(x) mean(x, na.rm = TRUE))

        if(exists("Thresh")) {
          out1 <- out1 + geom_hline(yintercept = Thresh)
        }
        ggsave(paste0(model_path, "/CalP_", Construct[i,5][[1]], ".png"))
      } else {
        # Calibration plot
        out1 <- EvalPlot(calibration,
                         PredColName = predName,
                         ActColName  = Construct[i,1][[1]],
                         type        = "calibration",
                         bucket      = 0.05,
                         aggrfun     = function(x) mean(x, na.rm = TRUE))
        ggsave(paste0(model_path, "/CalP_", Construct[i,5][[1]], ".png"))

        # Calibration boxplot
        out2 <- EvalPlot(calibration,
                         PredColName = predName,
                         ActColName  = Construct[i,1][[1]],
                         type        = "boxplot",
                         bucket      = 0.05)
        ggsave(paste0(model_path, "/CalBP_", Construct[i,5][[1]], ".png"))
      }
    } else {
      # Multinomial case
      # Stack each level's predicted values and actual values
      if (Construct[i,11][[1]] && cc <= dd) {
        predsMulti <- h2o.predict(best_model, newdata = validate)
        col <- Construct[i,1][[1]]
        xx <- as.data.table(h2o.cbind(validate[, col],predsMulti))
        if(Construct[i,12][[1]]) {
          calib <- as.data.table(h2o.cbind(validate, preds))
          save(calib, file = paste0(model_path,"/",Construct[i,5][[1]],".Rdata"))
        }
        N <- (ncol(xx) - 2)
        data <- eval(parse(text = Construct[i,7][[1]]))
        for (lev in levels(data[[Construct[i,1][[1]]]])) {
          xx[, paste0("V",lev) := ifelse(xx[[1]] %in% lev,1,0)]
        }
        RemoveCols <- names(xx)[1:2]
        KeepCols   <- names(xx)[3:length(names(xx))]
        xx[, (RemoveCols) := NULL]
        store <- list()
        for (k in 1:N) {
          j = k + N
          temp <- cbind(xx[,..k],xx[,..j])
          setnames(temp, KeepCols[k], "Preds")
          setnames(temp, KeepCols[j], "Act")
          store[[k]] <- temp
        }
        xxx <- rbindlist(store)

        # Calibration plot
        out1 <- EvalPlot(xxx,
                         PredColName = "Preds",
                         ActColName  = "Act",
                         type        = "calibration",
                         bucket      = 0.05,
                         aggrfun     = function(x) mean(x, na.rm = TRUE))
        ggsave(paste0(model_path, "/CalP_", Construct[i,5][[1]], ".png"))
      } else {
        predsMulti <- h2o.predict(bl_model, newdata = validate)
        col <- Construct[i,1][[1]]
        xx <- as.data.table(h2o.cbind(validate[, col],predsMulti))
        if(Construct[i,12][[1]]) {
          calib <- as.data.table(h2o.cbind(validate, preds))
          save(calib, file = paste0(model_path,"/",Construct[i,5][[1]],".Rdata"))
        }
        N <- (ncol(xx) - 2)
        data <- eval(parse(text = Construct[i,7][[1]]))
        for (lev in levels(data[[Construct[i,1][[1]]]])) {
          xx[, paste0("V",lev) := ifelse(xx[[1]] %in% lev,1,0)]
        }
        RemoveCols <- names(xx)[1:2]
        KeepCols   <- names(xx)[3:length(names(xx))]
        xx[, (RemoveCols) := NULL]
        store <- list()
        for (k in 1:N) {
          j = k + N
          temp <- cbind(xx[,..k],xx[,..j])
          setnames(temp, KeepCols[k], "Preds")
          setnames(temp, KeepCols[j], "Act")
          store[[k]] <- temp
        }
        xxx <- rbindlist(store)

        # Calibration plot
        out1 <- EvalPlot(xxx,
                         PredColName = "Preds",
                         ActColName  = "Act",
                         type        = "calibration",
                         bucket      = 0.05,
                         aggrfun     = function(x) mean(x, na.rm = TRUE))
        ggsave(paste0(model_path, "/CalP_", Construct[i,5][[1]], ".png"))
      }

      # Multinomial AUC function here::
      #val <- H20MultinomialAUC(validate, best_model, targetColNum = 1, targetName = "TargetVar")

    }

    #######################################
    # Partial dependence calibration plots
    #######################################

    if(Construct[i,13][[1]] >= 1) {
      VIMP <- VIMP[!is.na(VIMP[,2][[1]])]
      rows <- nrow(VIMP)
      cols <- VIMP[1:min(Construct[i,13][[1]],rows), 1][[1]]
      calibr <- list()
      boxplotr <- list()
      j <- 0
      if(!(tolower(Construct[i,2][[1]]) %in% c("multinomial"))) {
        for (col in cols) {
          j <- j + 1
          if(tolower(Construct[i,2][[1]]) == "quantile") {
            out1 <- tryCatch({ParDepCalPlots(calib,
                                   PredColName = predName,
                                   ActColName  = Construct[i,1][[1]],
                                   IndepVar    = col,
                                   type        = "calibration",
                                   bucket      = 0.05,
                                   FactLevels  = 10,
                                   Function    = function(x) quantile(x, probs = Construct[i,4][[1]], na.rm = TRUE))},
                             error = function(x) "skip")
          } else {
            out1 <- tryCatch({ParDepCalPlots(calib,
                                   PredColName = predName,
                                   ActColName  = Construct[i,1][[1]],
                                   IndepVar    = col,
                                   type        = "calibration",
                                   bucket      = 0.05,
                                   FactLevels  = 10,
                                   Function    = function(x) mean(x, na.rm = TRUE))},
                             error = function(x) "skip")
          }

          # Add threshold line to charts
          if (tolower(Construct[i,2][[1]]) %in% c("quasibinomial","binomial","bernoulli")) {
            if(exists("Thresh")) {
              out1 <- out1 + geom_hline(yintercept = Thresh)
            }
            calibr[[j]] <- out1
          } else {
            calibr[[j]] <- out1
          }

          # Expected value regression
          if (!(tolower(Construct[i,2][[1]]) %in% c("quasibinomial","binomial","bernoulli"))) {
            boxplotr[[j]] <- tryCatch({ParDepCalPlots(calib,
                                            PredColName = predName,
                                            ActColName  = Construct[i,1][[1]],
                                            IndepVar    = col,
                                            type        = "boxplot",
                                            bucket      = 0.05,
                                            FactLevels  = 10)},
                                      error = function(x) "skip")
          }
        }

        # Save output
        if (!(tolower(Construct[i,2][[1]]) %in% c("quasibinomial","binomial","bernoulli"))) {
          save(boxplotr, file = paste0(model_path,"/",Construct[i,5][[1]],"_ParDepCalBoxPlots.Rdata"))
        }
        save(calibr, file = paste0(model_path,"/",Construct[i,5][[1]],"_ParDepCalPlots.Rdata"))
      }
    }

    # Save grid_tuned_paths
    save(grid_tuned_paths, file = paste0(model_path, "/grid_tuned_paths.Rdata"))

    # Clear H20 environment between runs
    h2o.rm(data_h2o)
    h2o.rm(data_train)
    h2o.rm(train)
    h2o.rm(validate)
    if (Construct[i,11][[1]]) {
      h2o.rm(best_model)
    }
    if (Construct[i,6][[1]] != "automl") {
      h2o.rm(bl_model)
    }
    h2o.rm(preds)
    h2o.shutdown(prompt = FALSE)

    # Clear R environment between runs
    if (Construct[i,11][[1]]) {
      if (Construct[i,2][[1]] != "multinomial" & Construct[i,21][[1]] == TRUE) {
        rm(grid, Grid_Out, cc, dd, VIMP, calibration, calib, out2, out1, features, target, save_model)
      } else {
        rm(grid, Grid_Out, cc, dd, VIMP, features, target, predsMulti)
      }
    } else {
      if (Construct[i,2][[1]] != "multinomial") {
        rm(dd, VIMP, calibration, calib, out2, out1, features, target, save_model)
      } else {
        rm(dd, VIMP, features, target)
      }
    }

    # Remove data if no longer needed
    if (i > 1) {
      if (Construct[i,7][[1]] != Construct[(i-1),7][[1]]) {
        eval(parse(text = paste0("rm(",Construct[(i-1),7][[1]],")")))
      }
    }
  }
}

#' For NLP work
#'
#' This function tokenizes data
#' @author Adrian Antico at RemixInstitute.com
#' @param sentences Source data table to merge vects onto
#' @param stop.words A string name for the column to convert via word2vec
#' @export
tokenizeH20 <- function(data3) {
  library(h2o)
  data3 <- as.h2o(data3, col.types=c("String"))
  tokenized <- h2o.tokenize(data3, "\\\\W+")
  tokenized.lower <- h2o.tolower(tokenized)
  tokenized.words <- tokenized.lower[h2o.grep("[0-9]", tokenized.lower, invert = TRUE, output.logical = TRUE),]
  tokenized.words
}

#' Automated word2vec data generation via H20
#'
#' This function allows you to automatically build a word2vec model and merge the data onto your supplied dataset
#' @author Adrian Antico
#' @param data Source data table to merge vects onto
#' @param stringCol A string name for the column to convert via word2vec
#' @param KeepStringCol Set to TRUE if you want to keep the original string column that you convert via word2vec
#' @param model_path A string path to the location where you want the model and metadata stored
#' @param ModelID A vector of your model names
#' @param vects The number of vectors to retain from the word2vec model
#' @param SaveStopWords Set to TRUE to save the stop words used
#' @param MinWords For H20 word2vec model
#' @param WindowSize For H20 word2vec model
#' @param Epochs For H20 word2vec model
#' @param StopWords For H20 word2vec model
#' @param SaveModel Set to "standard" to save normally; set to "mojo" to save as mojo
#' @param Threads Number of available threads you want to dedicate to model building
#' @param MaxMemory Amount of memory you want to dedicate to model building
#' @examples
#'Word2VecModel(data,
#'              stringCol     = "Comment",
#'              KeepStringCol = FALSE,
#'              model_path    = getwd(),
#'              ModelID       = "bla",
#'              vects         = 50,
#'              SaveStopWords = FALSE,
#'              MinWords      = 1,
#'              WindowSize    = 1,
#'              Epochs        = 25,
#'              StopWords     = NULL)
#' @export
Word2VecModel <- function(datax,
                          stringCol     = c("Q65_Ans","Q66_Ans","Q67_Ans","Q69_Ans","Q70_Ans","Q71_Ans",
                                            "Q72_Ans","Q73_Ans","Q74_Ans","Q75_Ans","Q76_Ans","Q77_Ans",
                                            "Q78_Ans","Q79_Ans","Q80_Ans","Q81_Ans","Q82_Ans","Q83_Ans"),
                          KeepStringCol = FALSE,
                          model_path    = getwd(),
                          ModelID       = c("Q65_Ans","Q66_Ans","Q67_Ans","Q69_Ans","Q70_Ans","Q71_Ans",
                                            "Q72_Ans","Q73_Ans","Q74_Ans","Q75_Ans","Q76_Ans","Q77_Ans",
                                            "Q78_Ans","Q79_Ans","Q80_Ans","Q81_Ans","Q82_Ans","Q83_Ans"),
                          vects         = 5,
                          SaveStopWords = FALSE,
                          MinWords      = 1,
                          WindowSize    = 1,
                          Epochs        = 25,
                          StopWords     = NULL,
                          SaveModel     = "standard",
                          Threads       = 4,
                          MaxMemory     = "14G") {

  # Load libraries
  library(h2o)
  library(data.table)
  
  # Ensure data is a data.table
  data <- as.data.table(datax)

  # Create storage file
  N <- length(stringCol)
  StoreFile <- data.table(ModelName = rep("a", N), Path = c("aa",N))
  i <- 0

  # Loop through all the string columns
  for (string in stringCol) {
    i <- i + 1
    Sys.sleep(10)
    data[, eval(string) := as.character(get(string))]
    h2o.init(nthreads = Threads, max_mem_size = MaxMemory)

    # It is important to remove "\n" -- it appears to cause a parsing error when converting to an H2OFrame
    data[,':=' (TEMP=gsub("  ", " ", data[[string]]))]
    data[,':=' (TEMP=gsub("'|\"|'|"|"|\"|\n|,|\\.|.|\\?|\\+|\\-|\\/|\\=|\\(|\\)|'", "", TEMP))]
    data2 <- data[, "TEMP"]

    # Tokenize
    tokenized_words <- tokenizeH20(data2)

    # Build model
    w2v.model <- h2o.word2vec(tokenized_words,
                              model_id           = ModelID[i],
                              word_model         = "SkipGram",
                              norm_model         = "HSM",
                              vec_size           = vects,
                              min_word_freq      = MinWords,
                              window_size        = WindowSize,
                              init_learning_rate = 0.025,
                              sent_sample_rate   = 0.05,
                              epochs             = Epochs)

    # Save model
    if(tolower(SaveModel) == "standard") {
      w2vPath <- h2o.saveModel(w2v.model, path = model_path, force = TRUE)
      set(StoreFile, i = i, j = 1L, value = ModelID[i])
      set(StoreFile, i = i, j = 2L, value = w2vPath)
      save(StoreFile, file = paste0(model_path, "/StoreFile.Rdata"))
    } else {
      w2vPath <- h2o.saveMojo(w2v.model, path = model_path, force = TRUE)
      h2o.download_mojo(model = w2v.model,
                        path = model_path,
                        get_genmodel_jar = TRUE,
                        genmodel_path = model_path,
                        genmodel_name = ModelID[i])
      set(StoreFile, i = i, j = 1L, value = ModelID[i])
      set(StoreFile, i = i, j = 2L, value = w2vPath)
      save(StoreFile, file = paste0(model_path, "/StoreFile.Rdata"))
    }

    h2o.rm('data3')

    # Score model
    all_vecs <- h2o.transform(w2v.model, tokenized_words, aggregate_method = "AVERAGE")

    # Convert to data.table
    all_vecs <- as.data.table(all_vecs)
    data <- data.table(cbind(data, all_vecs))

    # Remove string cols
    data[, ':=' (TEMP = NULL)]
    if(!KeepStringCol) {
      data[, eval(string) := NULL]
    }

    # Replace Colnames
    cols <- names(data[, (ncol(data)-vects+1):ncol(data)])
    for (c in cols) {
      data[, paste0(string,"_",c) := get(c)]
      data[, eval(c) := NULL]
    }

    # Final Prep
    h2o.rm(w2v.model)
    h2o.shutdown(prompt = FALSE)
  }
  return(data)
}

#' Automated Word Frequency and Word Cloud Creation
#'
#' This function builds a word frequency table and a word cloud. It prepares data, cleans text, and generates output.
#' @author Adrian Antico
#' @param data Source data table
#' @param TextColName A string name for the column
#' @param ClusterCol Set to NULL to ignore, otherwise set to Cluster column name (or factor column name)
#' @param ClusterID Must be set if ClusterCol is defined. Set to cluster ID (or factor level)
#' @param RemoveEnglishStopwords Set to TRUE to remove English stop words, FALSE to ignore
#' @param Stemming Set to TRUE to run stemming on your text data
#' @param StopWords Add your own stopwords, in vector format
#' @examples
#'WordFreq(data,
#'         TextColName = "DESCR",
#'         ClusterCol = "ClusterAllNoTarget",
#'         ClusterID = 0,
#'         RemoveEnglishStopwords = TRUE,
#'         Stemming = TRUE,
#'         StopWords = c("blabla1", "blabla2")
#' @export
WordFreq <- function(data,
                     TextColName = "DESCR",
                     ClusterCol = "ClusterAllNoTarget",
                     ClusterID = 0,
                     RemoveEnglishStopwords = TRUE,
                     Stemming = TRUE,
                     StopWords = c("blabla1", "blabla2")) {

  # Load libraries
  library("tm")
  library("SnowballC")
  library("wordcloud")
  library("RColorBrewer")

  # Prepare data
  if(is.null(ClusterCol)) {
    desc <- Corpus(VectorSource(data[[eval(TextColName)]]))
  } else {
    desc <- Corpus(VectorSource(data[get(ClusterCol) == eval(ClusterID)][[eval(TextColName)]]))
  }

  # Clean text
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  text <- tm_map(text, toSpace, "/")
  text <- tm_map(text, toSpace, "@")
  text <- tm_map(text, toSpace, "\\|")

  # Convert the text to lower case
  text <- tm_map(text, content_transformer(tolower))

  # Remove numbers
  text <- tm_map(text, removeNumbers)

  # Remove english common stopwords
  if(RemoveEnglishStopwords) text <- tm_map(text, removeWords, stopwords("english"))

  # specify your stopwords as a character vector
  text <- tm_map(text, removeWords, StopWords)

  # Remove punctuations
  text <- tm_map(text, removePunctuation)

  # Eliminate extra white spaces
  text <- tm_map(text, stripWhitespace)

  # Text stemming
  if(Stemming) text <- tm_map(text, stemDocument)

  # Finalize
  dtm <- TermDocumentMatrix(text)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.table(word = names(v), freq=v)
  print(head(d, 10))

  # Word Cloud
  print(wordcloud(words = d$word, freq = d$freq, min.freq = 1,
                  max.words=200, random.order=FALSE, rot.per=0.35,
                  colors=brewer.pal(8, "Dark2")))

  # Return
  return(d)
}
