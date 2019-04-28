.datatable.aware <- TRUE

# Sys.setenv(R_GSCMD = "C:\\Program Files (x86)\\gs\\gs9.26\\bin\\gswin32c.exe")

"rbindlist" <- NULL
":=" <- NULL
"fwrite" <- NULL
"setnames" <- NULL
"is.data.table" <- NULL
"as.data.table" <- NULL
"set" <- NULL
"data.table" <- NULL
".N" <- NULL
"%chin%" <- NULL
"setcolorder" <- NULL
".SD" <- NULL
"setorderv" <- NULL

utils::globalVariables(
  names = c(
    "test",
    ":=",
    "fwrite",
    "setnames",
    "Metric1",
    "Metric2",
    "Metric3",
    "Predict",
    "Predicted",
    "setorderv",
    "rbindlist",
    "is.data.table",
    "as.data.table",
    ".N",
    "%chin%",
    "setcolorder",
    ".SD",
    "SFreq",
    "BinaryRatingsMatrix",
    "Residuals",
    "ind",
    "RANDOMNUMER",
    "p1",
    "MetricValue",
    "Threshold",
    "EvalStat",
    "ParamRow",
    "ModelNumber",
    "Specificity",
    "Importance",
    "Variable",
    "ObsNum",
    "type",
    "KMeansModelFile",
    "FilePath1",
    "FilePath2",
    "fitY",
    "Nam",
    "RatingMatrix",
    "ProductRank",
    "model",
    "n_products",
    "TPR",
    "item",
    "value",
    "act",
    "MeanAbsError",
    "SupplyData",
    "string",
    "..string",
    "NThreads",
    "MaxMem",
    "LCVals",
    "Row",
    "ErrorCollectionLog",
    "prophet",
    "pushViewport",
    "quantile",
    "removeNumbers",
    "removePunctuation",
    "removeWords",
    "residuals",
    "rnorm",
    "runif",
    "sd",
    "stemDocument",
    "stopCluster",
    "stopwords",
    "stripWhitespace",
    "theme",
    "tm_map",
    "ts",
    "val1",
    "val2",
    "val3",
    "vals",
    "viewport",
    "wordcloud",
    "xlab",
    "years",
    "ylab",
    "TermDocumentMatrix",
    "Thresholds",
    "Type",
    "Utilities",
    "Utility",
    "V1",
    "V2",
    "V3",
    "VectorSource",
    "acts",
    "aes",
    "as.formula",
    "as_date",
    "coefhat",
    "content_transformer",
    "ds",
    "f0point5",
    "f1",
    "f2",
    "fitted",
    "head",
    "i",
    "id",
    "lm",
    "margin",
    "meanResid",
    "monreg",
    "nls",
    "object.size",
    "output",
    "percentaR",
    "CMD",
    "check",
    "results",
    "Thresh",
    "Label",
    "..k",
    "..j",
    "Date",
    "Target",
    "RowNumAsc",
    "AnomHigh",
    "AnomLow",
    "CumAnomHigh",
    "CumAnomLow",
    "AnomHighRate",
    "AnomLowRate",
    "predict",
    "..cols",
    ".",
    "..keep",
    "Output",
    "MTHT",
    "MTLT",
    "TEMP",
    "Accuracy",
    "FC_Eval",
    "DateTime",
    "Resid",
    "PercentError",
    "AbsolutePercentError",
    "ModelName",
    "MAPE",
    "ID",
    "Forecast_PROPHET",
    "percentage",
    "data",
    "FilePath",
    "grid_tuned_paths",
    "StoreFile",
    "temp",
    "Path"
  )
)

#' DummifyDT creates dummy variables for the selected columns.
#'
#' DummifyDT creates dummy variables for the selected columns. Either one-hot encoding, N+1 columns for N levels, or N columns for N levels.
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @param data the data set to run the micro auc on
#' @param cols a vector with the names of the columns you wish to dichotomize
#' @param KeepFactorCols set to TRUE to keep the original columns used in the dichotomization process
#' @param OneHot Set to TRUE to run one hot encoding, FALSE to generate N columns for N levels
#' @param ClustScore This is for scoring AutoKMeans. Set to FALSE for all other applications.
#' @importFrom data.table %chin% set setcolorder := .N fwrite setnames is.data.table data.table .SD
#' @examples
#' test <- data.table::data.table(Value = runif(100000),
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
#'                   KeepFactorCols = FALSE)
#' ncol(test)
#' test[, sum(FactorCol_gg)]
#' @return data table with new dummy variables columns and optionally removes base columns
#' @export
DummifyDT <- function(data,
                      cols,
                      KeepFactorCols = FALSE,
                      OneHot         = TRUE,
                      ClustScore     = FALSE) {

  # Require data.table
  requireNamespace('data.table', quietly = FALSE)

  # Check data.table
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }

  # Ensure correct argument settings
  if(OneHot == TRUE & ClustScore == TRUE) {
    OneHot <- FALSE
    KeepFactorCols <- FALSE
  }

  for (col in rev(cols)) {
    # Store original column size
    size <- ncol(data)
    Names <- setdiff(names(data),col)
    inds <- sort(unique(data[[eval(col)]]))
    data.table::alloc.col(data,
                          ncol(data) + length(inds))
    if (is.factor(data[[eval(col)]])) {
      data[, eval(col) := as.character(get(col))]
    }
    if(!ClustScore) {
      data.table::set(data,
                      j = paste0(col, "_", inds),
                      value = 0L)
    } else {
      data.table::set(data,
                      j = paste0(col, inds),
                      value = 0L)
    }
    for (ind in inds) {
      if(!ClustScore) {
        data.table::set(
          data,
          i = which(data[[col]] %chin% ind),
          j = paste0(col, "_", ind),
          value = 1L
        )
      } else {
        data.table::set(
          data,
          i = which(data[[col]] %chin% ind),
          j = paste0(col, ind),
          value = 1L
        )
      }
    }
    if (!KeepFactorCols) {
      data[, eval(col) := NULL]
    }
    if(ClustScore) {
      setcolorder(data,
                  c(setdiff(names(data),
                            Names),
                    Names))
    }
    if (OneHot) {
      data[, paste0(col, "_Base") := 0]
    }
  }
  if(ClustScore) {
    setnames(data,names(data),
             tolower(
               gsub('[[:punct:] ]+',
                    replacement = "",
                    names(data))))
  }
  return(data)
}

#' H2OMultinomialAUC computes the micro auc from a multinomial model
#'
#' @author Adrian Antico
#' @family Model Evaluation and Interpretation
#' @noRd
#' @param validate the data set to run the micro auc on
#' @param best_model the model object you wish to test
#' @param targetColNum the column number of the target variable
#' @param targetName the name, in quotes, of the target variable
#' @examples
#' \donttest{
#' auc_val <- H2OMultinomialAUC(validate, best_model, targetColNum = 1, targetName = "TargetVar")
#' }
#' @return Micro AUC
H2OMultinomialAUC <-
  function(validate,
           best_model,
           targetColNum = 1,
           targetName = "TargetVar") {

  # Require data.table
  requireNamespace('data.table', quietly = FALSE)
  xx <-
    data.table::as.data.table(h2o::h2o.cbind(
      validate[, targetColNum],
      h2o::h2o.predict(best_model,
                       newdata = validate)))
  xx[, predict := as.character(predict)]
  xx[, vals := 0.5]
  z <- ncol(xx)
  col <- targetName
  for (l in seq_len(nrow(xx))) {
    cols <- xx[l, get(col)][[1]]
    valss <- xx[l, ..cols][[1]]
    data.table::set(xx, l, j = z, value = valss)
  }
  return(round(as.numeric(noquote(
    stringr::str_extract(
      pROC::multiclass.roc(xx$target, xx$vals)$auc,
      "\\d+\\.*\\d*"
    )
  )), 4))
}

#' PrintObjectsSize prints out the top N objects and their associated sizes, sorted by size
#'
#' @author Adrian Antico
#' @family Misc
#' @param N The number of objects to display
#' @examples
#' \donttest{
#' PrintObjectsSize(N = 10)
#' }
#' @return A print to your console of the sizes of the objects in your environment
#' @export
PrintObjectsSize <- function(N = 10) {
  m <- length(ls())
  z <- min(m,N)
  print(sort(-vapply(ls(), FUN.VALUE = 1.1, function(x) {
    object.size(get(x))}))[1:z] / 1024 / 1024)
}

#' GenTSAnomVars is an automated z-score anomaly detection via GLM-like procedure
#'
#' GenTSAnomVars is an automated z-score anomaly detection via GLM-like procedure. Data is z-scaled and grouped by factors and time periods to determine which points are above and below the control limits in a cumulative time fashion. Then a cumulative rate is created as the final variable. Set KeepAllCols to FALSE to utilize the intermediate features to create rolling stats from them. The anomalies are separated into those that are extreme on the positive end versus those that are on the negative end.
#'
#' @author Adrian Antico
#' @family Unsupervised Learning
#' @param data the source residuals data.table
#' @param ValueCol the numeric column to run anomaly detection over
#' @param GroupVar1 this is a group by variable
#' @param GroupVar2 this is another group by variable
#' @param DateVar this is a time variable for grouping
#' @param HighThreshold this is the threshold on the high end
#' @param LowThreshold this is the threshold on the low end
#' @param KeepAllCols set to TRUE to remove the intermediate features
#' @param IsDataScaled set to TRUE if you already scaled your data
#' @examples
#' data <- data.table::data.table(DateTime = as.Date(Sys.time()),
#'   Target = stats::filter(rnorm(10000,
#'                                mean = 50,
#'                                sd = 20),
#'                          filter=rep(1,10),
#'                          circular=TRUE))
#' data[, temp := seq(1:10000)][, DateTime := DateTime - temp][, temp := NULL]
#' data <- data[order(DateTime)]
#' x <- data.table::as.data.table(sde::GBM(N=10000)*1000)
#' data[, predicted := x[-1,]]
#' stuff <- GenTSAnomVars(data,
#'                        ValueCol = "Target",
#'                        GroupVar1 = NULL,
#'                        GroupVar2 = NULL,
#'                        DateVar = "DateTime",
#'                        HighThreshold = 1.96,
#'                        LowThreshold = -1.96,
#'                        KeepAllCols = TRUE,
#'                        IsDataScaled  = FALSE)
#' @return The original data.table with the added columns merged in. When KeepAllCols is set to FALSE, you will get back two columns: AnomHighRate and AnomLowRate - these are the cumulative anomaly rates over time for when you get anomalies from above the thresholds (e.g. 1.96) and below the thresholds.
#' @export
GenTSAnomVars <- function(data,
                          ValueCol    = "Value",
                          GroupVar1   = "SKU",
                          GroupVar2   = NULL,
                          DateVar     = "DATE",
                          HighThreshold = 1.96,
                          LowThreshold  = -1.96,
                          KeepAllCols = FALSE,
                          IsDataScaled  = TRUE) {

  # Ensure data.table is available
  requireNamespace('data.table', quietly = FALSE)

  # Check data.table
  if (!data.table::is.data.table(data))
    data <- data.table::as.data.table(data)

  # Scale data if not already
  if (!IsDataScaled) {
    data[, eval(ValueCol) := scale(get(ValueCol),
                                   center = TRUE,
                                   scale = TRUE)]
  }

  # Global check for date
  if (!is.null(DateVar)) {
    if (is.null(GroupVar1) & is.null(GroupVar2)) {
      data <- data[order(get(DateVar))]
      data[, RowNumAsc := 1:.N]
      data[, AnomHigh := as.numeric(ifelse(get(ValueCol) > HighThreshold,
                                           1, 0))]
      data[, AnomLow := as.numeric(ifelse(get(ValueCol) < LowThreshold,
                                          1, 0))]
      data[, CumAnomHigh := cumsum(AnomHigh)]
      data[, CumAnomLow := cumsum(AnomLow)]
      data[, AnomHighRate := CumAnomHigh / RowNumAsc]
      data[, AnomLowRate := CumAnomLow / RowNumAsc]
      if (!KeepAllCols) {
        data[, ':=' (
          AnomHigh = NULL,
          AnomLow = NULL,
          CumAnomHigh = NULL,
          CumAnomLow = NULL,
          RowNumAsc = NULL
        )]
      }
    } else if (is.null(GroupVar2) & !is.null(GroupVar1)) {
      data <- data[order(get(GroupVar1), get(DateVar))]
      data[, RowNumAsc := 1:.N, by = get(GroupVar1)]
      data[, AnomHigh := as.numeric(ifelse(get(ValueCol) > HighThreshold,
                                           1, 0))]
      data[, AnomLow := as.numeric(ifelse(get(ValueCol) < LowThreshold,
                                          1, 0))]
      data[, CumAnomHigh := cumsum(AnomHigh), by = get(GroupVar1)]
      data[, CumAnomLow := cumsum(AnomLow), by = get(GroupVar1)]
      data[, AnomHighRate := CumAnomHigh / RowNumAsc]
      data[, AnomLowRate := CumAnomLow / RowNumAsc]
      if (!KeepAllCols) {
        data[, ':=' (
          AnomHigh = NULL,
          AnomLow = NULL,
          CumAnomHigh = NULL,
          CumAnomLow = NULL,
          RowNumAsc = NULL
        )]
      }
    } else if (!is.null(GroupVar1) & !is.null(GroupVar2)) {
      data <- data[order(get(GroupVar1), get(GroupVar2),
                         get(DateVar))]
      data[, RowNumAsc := 1:.N, by = list(get(GroupVar1),
                                          get(GroupVar2))]
      data[, AnomHigh := as.numeric(ifelse(get(ValueCol) > HighThreshold,
                                           1, 0))]
      data[, AnomLow := as.numeric(ifelse(get(ValueCol) < LowThreshold,
                                          1, 0))]
      data[, CumAnomHigh := cumsum(AnomHigh),
           by = list(get(GroupVar1),
                     get(GroupVar2))]
      data[, CumAnomLow := cumsum(AnomLow),
           by = list(get(GroupVar1),
                     get(GroupVar2))]
      data[, paste0(GroupVar2,
                    "AnomHighRate") := CumAnomHigh / RowNumAsc]
      data[, paste0(GroupVar2,
                    "AnomLowRate") := CumAnomLow / RowNumAsc]
      if (!KeepAllCols) {
        data[, ':=' (
          AnomHigh = NULL,
          AnomLow = NULL,
          CumAnomHigh = NULL,
          CumAnomLow = NULL,
          RowNumAsc = NULL
        )]
      }
    }
    return(data)
  }
  return(NULL)
}

#' ResidualOutliers is an automated time series outlier detection function
#'
#' ResidualOutliers is an automated time series outlier detection function that utilizes tsoutliers and auto.arima. It looks for five types of outliers: "AO" Additive outliter - a singular extreme outlier that surrounding values aren't affected by; "IO" Innovational outlier - Initial outlier with subsequent anomalous values; "LS" Level shift - An initial outlier with subsequent observations being shifted by some constant on average; "TC" Transient change - initial outlier with lingering effects that dissapate exponentially over time; "SLS" Seasonal level shift - similar to level shift but on a seasonal scale.
#'
#' @author Adrian Antico
#' @family Unsupervised Learning
#' @param data the source residuals data.table
#' @param DateColName The name of your data column to use in reference to the target variable
#' @param TargetColName The name of your target variable column
#' @param PredictedColName The name of your predicted value column. If you supply this, you will run anomaly detection of the difference between the target variable and your predicted value. If you leave PredictedColName NULL then you will run anomaly detection over the target variable.
#' @param TimeUnit The time unit of your date column: hour, day, week, month, quarter, year
#' @param maxN the largest lag or moving average (seasonal too) values for the arima fit
#' @param tstat the t-stat value for tsoutliers
#' @examples
#' data <- data.table::data.table(DateTime = as.Date(Sys.time()),
#'                                Target = as.numeric(stats::filter(rnorm(1000,
#'                                                                        mean = 50,
#'                                                                        sd = 20),
#'                                                                  filter=rep(1,10),
#'                                                                  circular=TRUE)))
#' data[, temp := seq(1:1000)][, DateTime := DateTime - temp][, temp := NULL]
#' data <- data[order(DateTime)]
#' data[, Predicted := as.numeric(stats::filter(rnorm(1000,
#'                                                    mean = 50,
#'                                                    sd = 20),
#'                                              filter=rep(1,10),
#'                                              circular=TRUE))]
#' stuff <- ResidualOutliers(data = data,
#'                           DateColName = "DateTime",
#'                           TargetColName = "Target",
#'                           PredictedColName = NULL,
#'                           TimeUnit = "day",
#'                           maxN = 5,
#'                           tstat = 4)
#' data     <- stuff[[1]]
#' model    <- stuff[[2]]
#' outliers <- data[type != "<NA>"]
#' @return A named list containing FullData = original data.table with outliers data and ARIMA_MODEL = the arima model.
#' @export
ResidualOutliers <- function(data,
                             DateColName = "DateTime",
                             TargetColName = "Target",
                             PredictedColName = NULL,
                             TimeUnit = "day",
                             maxN = 5,
                             tstat = 2) {

  # Ensure data.table is available
  requireNamespace('data.table', quietly = FALSE)

  # Define TS Frequency
  if (tolower(TimeUnit) == "hour") {
    freq <- 24
  } else if (tolower(TimeUnit) == "day") {
    freq <- 365
  } else if (tolower(TimeUnit) == "week") {
    freq <- 52
  } else if (tolower(TimeUnit) == "month") {
    freq <- 12
  } else if (tolower(TimeUnit) == "quarter") {
    freq <- 4
  } else if (tolower(TimeUnit) == "year") {
    freq <- 1
  } else {
    warning("TimeUnit is not in hour, day, week, month,
    quarter, or year")
  }

  # Ensure data is a data.table
  if(!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }

  # Ensure data is sorted
  data.table::setorderv(x = data,
                        cols = eval(DateColName),
                        order = 1)

  # Keep columns
  if(!is.null(PredictedColName)) {
    data[, Residuals := get(TargetColName) - get(PredictedColName)]
  } else {
    data[, Residuals := get(TargetColName)]
  }
  keep <- c(DateColName, "Residuals")
  temp <- data[, ..keep]
  MinVal <- min(data[[eval(TargetColName)]], na.rm = TRUE)

  # Convert to time series object
  tsData <- stats::ts(temp,
                      start = temp[, min(get(DateColName))][[1]],
                      frequency = freq)

  # Build the auto arimia
  if(MinVal > 0) {
    fit <-
      tryCatch({
        forecast::auto.arima(
          y = tsData[, "Residuals"],
          max.p = maxN,
          max.q = maxN,
          max.P = maxN,
          max.Q = maxN,
          max.d = 1,
          max.D = 1,
          ic = "bic",
          lambda = TRUE,
          biasadj = TRUE,
          stepwise = TRUE
        )
      },
      error = function(x)
        "empty")
  } else {
    fit <-
      tryCatch({
        forecast::auto.arima(
          y = tsData[, "Residuals"],
          max.p = maxN,
          max.q = maxN,
          max.P = maxN,
          max.Q = maxN,
          max.d = 1,
          max.D = 1,
          ic = "bic",
          lambda = FALSE,
          biasadj = FALSE,
          stepwise = TRUE
        )
      },
      error = function(x)
        "empty")
  }

  # Store the arima parameters
  pars  <- tsoutliers::coefs2poly(fit)

  # Store the arima residuals
  resid <- cbind(tsData, stats::residuals(fit))

  # Find the outliers
  x <- data.table::as.data.table(tsoutliers::locate.outliers(
    resid = resid[, 3],
    pars = pars,
    cval = tstat,
    types = c("AO", "TC", "LS", "IO", "SLS")
  ))

  # Merge back to source data
  residDT <- data.table::as.data.table(resid)
  z <- cbind(data, residDT)
  z[, ind := 1:.N]
  data.table::setnames(
    z,
    names(z)[c((ncol(z)-3):(ncol(z)-1))],
    c("ObsNum", "Preds", "ARIMA_Residuals")
  )
  z[, ObsNum := NULL]
  data <- merge(z, x, by = "ind", all.x = TRUE)
  data[, ':=' (ind = NULL, coefhat = NULL)]
  data[type == "<NA>", type := NA]

  # Reorder data, remove the coefhat column to send to database or stakeholder
  return(list(FullData = data, ARIMA_MODEL = fit))
}

#' AutoKMeans Automated row clustering for mixed column types
#'
#' AutoKMeans adds a column to your original data with a cluster number identifier. Uses glrm (grid tune-able) and then k-means to find optimal k.
#'
#' @author Adrian Antico
#' @family Unsupervised Learning
#' @param data is the source time series data.table
#' @param GridTuneGLRM If you want to grid tune the glrm model, set to TRUE, FALSE otherwise
#' @param GridTuneKMeans If you want to grid tuen the KMeans model, set to TRUE, FALSE otherwise
#' @param nthreads set based on number of threads your machine has available
#' @param MaxMem set based on the amount of memory your machine has available
#' @param glrmCols the column numbers for the glrm
#' @param IgnoreConstCols tell H2O to ignore any columns that have zero variance
#' @param glrmFactors similar to the number of factors to return from PCA
#' @param Loss set to one of "Quadratic", "Absolute", "Huber", "Poisson", "Hinge", "Logistic", "Periodic"
#' @param glrmMaxIters max number of iterations
#' @param SVDMethod choose from "Randomized","GramSVD","Power"
#' @param MaxRunTimeSecs set the timeout for max run time
#' @param KMeansK number of factors to test out in k-means to find the optimal number
#' @param KMeansMetric pick the metric to identify top model in grid tune c("totss","betweenss","withinss")
#' @param SaveModels Set to "standard", "mojo", or NULL (default)
#' @param PathFile Set to folder where you will keep the models
#' @examples
#' \donttest{
#' data <- data.table::as.data.table(iris)
#' data <- AutoKMeans(data,
#'                    nthreads = 8,
#'                    MaxMem = "28G",
#'                    SaveModels = NULL,
#'                    PathFile = NULL,
#'                    GridTuneGLRM = TRUE,
#'                    GridTuneKMeans = TRUE,
#'                    glrmCols = 1:(ncol(data)-1),
#'                    IgnoreConstCols = TRUE,
#'                    glrmFactors = 2,
#'                    Loss = "Absolute",
#'                    glrmMaxIters = 1000,
#'                    SVDMethod = "Randomized",
#'                    MaxRunTimeSecs = 3600,
#'                    KMeansK = 5,
#'                    KMeansMetric = "totss")
#' unique(data[["Species"]])
#' unique(data[["ClusterID"]])
#' temp <- data[, mean(ClusterID), by = "Species"]
#' Setosa <- round(temp[Species == "setosa", V1][[1]],0)
#' Versicolor <- round(temp[Species == "versicolor", V1][[1]],0)
#' Virginica <- round(temp[Species == "virginica", V1][[1]],0)
#' data[, Check := "a"]
#' data[ClusterID == eval(Setosa), Check := "setosa"]
#' data[ClusterID == eval(Virginica), Check := "virginica"]
#' data[ClusterID == eval(Versicolor), Check := "versicolor"]
#' data[, Acc := as.numeric(ifelse(Check == Species, 1, 0))]
#' data[, mean(Acc)][[1]]
#' }
#' @return Original data.table with added column with cluster number identifier
#' @export
AutoKMeans <- function(data,
                       nthreads        = 4,
                       MaxMem          = "14G",
                       SaveModels      = NULL,
                       PathFile        = NULL,
                       GridTuneGLRM    = TRUE,
                       GridTuneKMeans  = TRUE,
                       glrmCols        = c(1:5),
                       IgnoreConstCols = TRUE,
                       glrmFactors     = 5,
                       Loss            = "Absolute",
                       glrmMaxIters    = 1000,
                       SVDMethod       = "Randomized",
                       MaxRunTimeSecs  = 3600,
                       KMeansK         = 50,
                       KMeansMetric    = "totss") {

  # Ensure data.table is available
  requireNamespace('data.table', quietly = FALSE)

  # Check data.table
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }

  # Set up Scoring File if SaveModels is not NULL
  if(!is.null(SaveModels)) {
    KMeansModelFile <- data.table::data.table(
      Name = c("GLMR","AutoKMeans"),
      FilePath1 = rep("bla",2),
      FilePath2 = rep("bla",2)
    )
  }

  # Build glmr model
  h2o::h2o.init(nthreads = nthreads, max_mem_size = MaxMem)
  datax <- h2o::as.h2o(data)
  if (GridTuneGLRM) {
    # Define grid tune search scheme in a named list
    search_criteria  <-
      list(
        strategy             = "RandomDiscrete",
        max_runtime_secs     = 3600,
        max_models           = 30,
        seed                 = 1234,
        stopping_rounds      = 10,
        stopping_metric      = "MSE",
        stopping_tolerance   = 1e-3
      )

    # Define hyperparameters
    HyperParams <-
      list(
        transform        = c("NONE",
                             "DEMEAN",
                             "DESCALE",
                             "STANDARDIZE"),
        k                = 1:5,
        regularization_x = c(
          "None",
          "Quadratic",
          "L2",
          "L1",
          "NonNegative",
          "OneSparse",
          "UnitOneSparse",
          "Simplex"
        ),
        regularization_y = c(
          "None",
          "Quadratic",
          "L2",
          "L1",
          "NonNegative",
          "OneSparse",
          "UnitOneSparse",
          "Simplex"
        ),
        gamma_x          = seq(0.01, 0.10, 0.01),
        gamma_y          = seq(0.01, 0.10, 0.01),
        svd_method       = c("Randomized",
                             "GramSVD",
                             "Power")
      )

    # Run grid tune
    grid <- h2o::h2o.grid(
      "glrm",
      search_criteria   = search_criteria,
      training_frame    = datax,
      grid_id           = "GLRM",
      ignore_const_cols = IgnoreConstCols,
      loss              = Loss,
      hyper_params      = HyperParams
    )

    # Get best performer
    Grid_Out <-
      h2o::h2o.getGrid(
        grid_id = "GLRM",
        sort_by = search_criteria$stopping_metric,
        decreasing = FALSE
      )
    model <- h2o::h2o.getModel(model_id = Grid_Out@model_ids[[1]])
  } else {
    model <- h2o::h2o.glrm(
      training_frame    = datax,
      cols              = glrmCols,
      ignore_const_cols = IgnoreConstCols,
      k                 = glrmFactors,
      loss              = Loss,
      max_iterations    = glrmMaxIters,
      svd_method        = SVDMethod,
      max_runtime_secs  = MaxRunTimeSecs
    )
  }

  # Save model if requested
  if(!is.null(SaveModels)) {
    # Save archetypes and colnames
    fitY <- model@model$archetypes
    save(fitY, file = paste0(PathFile, "/fitY"))
    set(KMeansModelFile,
        i = 1L,
        j = 2L,
        value = paste0(PathFile, "/fitY"))
  }

  # Run k-means
  if (GridTuneKMeans) {
    # GLRM output
    x_raw <- h2o::h2o.getFrame(model@model$representation_name)
    Names <- colnames(x_raw)
    if(!is.null(SaveModels)) {
      save(Names, file = paste0(PathFile, "/Names.Rdata"))
      set(KMeansModelFile,
          i = 1L,
          j = 3L,
          value = paste0(PathFile, "/Names.Rdata"))
      save(KMeansModelFile, file = paste0(PathFile, "/KMeansModelFile.Rdata"))
    }

    # Define grid tune search scheme in a named list
    search_criteria  <-
      list(
        strategy             = "RandomDiscrete",
        max_runtime_secs     = 3600,
        max_models           = 30,
        seed                 = 1234,
        stopping_rounds      = 10
      )

    # Define hyperparameters
    HyperParams <- list(
      max_iterations   = c(10, 20, 50, 100),
      init             = c("Random", "PlusPlus", "Furthest")
    )

    # Run grid tune
    grid <- h2o::h2o.grid(
      "kmeans",
      search_criteria   = search_criteria,
      training_frame    = x_raw,
      x                 = Names,
      k                 = KMeansK,
      grid_id           = "KMeans",
      estimate_k        = TRUE,
      hyper_params      = HyperParams
    )

    # Get best performer
    Grid_Out <-
      h2o::h2o.getGrid(grid_id = "KMeans",
                       sort_by = KMeansMetric,
                       decreasing = FALSE)
    model <- h2o::h2o.getModel(model_id = Grid_Out@model_ids[[1]])
  } else {
    # GLRM output
    x_raw <- h2o::h2o.getFrame(model@model$representation_name)
    Names <- colnames(x_raw)
    if(!is.null(SaveModels)) {
      save(Names, file = paste0(PathFile, "/Names.Rdata"))
      set(KMeansModelFile,
          i = 1L,
          j = 3L,
          value = paste0(PathFile, "/Names.Rdata"))
      save(KMeansModelFile, file = paste0(PathFile, "/KMeansModelFile.Rdata"))
    }

    # Train KMeans
    model <- h2o::h2o.kmeans(
      training_frame = x_raw,
      x              = Names,
      k              = KMeansK,
      estimate_k     = TRUE
    )
  }

  # Save model if requested
  if(!is.null(SaveModels)) {
    if(tolower(SaveModels) == "mojo") {
      save_model <-
        h2o::h2o.saveMojo(object = model,
                          path = PathFile,
                          force = TRUE)
      h2o::h2o.download_mojo(
        model = model,
        path = PathFile,
        get_genmodel_jar = TRUE,
        genmodel_path = PathFile,
        genmodel_name = "KMeans"
      )
      set(KMeansModelFile,
          i = 2L,
          j = 2L,
          value = save_model)
      set(KMeansModelFile,
          i = 2L,
          j = 3L,
          value = paste0(PathFile, "/KMeans"))
      save(KMeansModelFile, file = paste0(PathFile,
                                          "/KMeansModelFile.Rdata"))
    } else if(tolower(SaveModels) == "standard") {
      save_model <-
        h2o::h2o.saveModel(object = model,
                           path = PathFile,
                           force = TRUE)
      data.table::set(
        KMeansModelFile,
        i = 2L,
        j = 2L,
        value = save_model
      )
      save(KMeansModelFile,
           file = paste0(PathFile,
                         "/KMeansModelFile.Rdata"))
    } else {
      return("You need to specify mojo or standard if you want to save your models")
    }
  }

  # Combine outputs
  preds <- data.table::as.data.table(h2o::h2o.predict(model, x_raw))
  h2o::h2o.shutdown(prompt = FALSE)
  data <- data.table::as.data.table(cbind(preds, data))
  data.table::setnames(data, "predict", "ClusterID")
  return(data)
}

#' AutoTS is an automated time series modeling function
#'
#' Step 1 is to build all the models and evaluate them on the number of HoldOutPeriods periods you specify. Step 2 is to pick the winner and rebuild the winning model on the full data set. Step 3 is to generate forecasts with the final model for FCPeriods that you specify.
#' AutoTS builds the best time series models for each type, using optimized box-cox transformations and using a user-supplied frequency for the ts data conversion along with a model-based frequency for the ts data conversion, compares all types, selects the winner, and generates a forecast. Models include:
#'
#' DSHW: Double Seasonal Holt Winters
#'
#' ARFIMA: Auto Regressive Fractional Integrated Moving Average
#'
#' ARIMIA: Stepwise Auto Regressive Integrated Moving Average with specified max lags, seasonal lags, moving averages, and seasonal moving averages
#'
#' ETS: Additive and Multiplicitive Exponential Smoothing and Holt Winters
#'
#' NNetar: Auto Regressive Neural Network models automatically compares models with 1 lag or 1 seasonal lag compared to models with up to N lags and N seasonal lags
#'
#' TBATS: Exponential smoothing state space model with Box-Cox transformation, ARMA errors, Trend and Seasonal components
#'
#' TSLM: Time Series Linear Model - builds a linear model with trend and season components extracted from the data
#'
#' @author Adrian Antico and Douglas Pestana
#' @family Supervised Learning
#' @param data is the source time series data as a data.table - or a data structure that can be converted to a data.table
#' @param TargetName is the name of the target variable in your data.table
#' @param DateName is the name of the date column in your data.table
#' @param FCPeriods is the number of periods into the future you wish to forecast
#' @param HoldOutPeriods is the number of periods to use for validation testing
#' @param TimeUnit is the level of aggregation your dataset comes in
#' @param Lags is the number of lags you wish to test in various models (same as moving averages)
#' @param SLags is the number of seasonal lags you wish to test in various models (same as moving averages)
#' @param NumCores is the number of cores available on your computer
#' @param SkipModels Don't run specified models - e.g. exclude all models "DSHW" "ARFIMA" "ARIMA" "ETS" "NNET" "TBATS" "TSLM"
#' @param StepWise Set to TRUE to have ARIMA and ARFIMA run a stepwise selection process. Otherwise, all models will be generated in parallel execution, but still run much slower.
#' @param TSClean Set to TRUE to have missing values interpolated and outliers replaced with interpolated values: creates separate models for a larger comparison set
#' @param PrintUpdates Set to TRUE for a print to console of function progress
#' @examples
#' data <- data.table::data.table(DateTime = as.Date(Sys.time()),
#'   Target = stats::filter(rnorm(100,
#'                                mean = 50,
#'                                sd = 20),
#'                          filter=rep(1,10),
#'                          circular=TRUE))
#' data[, temp := seq(1:100)][, DateTime := DateTime - temp][, temp := NULL]
#' data <- data[order(DateTime)]
#' output <-   AutoTS(data,
#'                    TargetName     = "Target",
#'                    DateName       = "DateTime",
#'                    FCPeriods      = 1,
#'                    HoldOutPeriods = 1,
#'                    TimeUnit       = "day",
#'                    Lags           = 1,
#'                    SLags          = 1,
#'                    NumCores       = 4,
#'                    SkipModels     = c("NNET","TBATS","ETS","TSLM","ARFIMA","DSHW"),
#'                    StepWise       = TRUE,
#'                    TSClean        = FALSE)
#' ForecastData <- output$Forecast
#' ModelEval    <- output$EvaluationMetrics
#' WinningModel <- output$TimeSeriesModel
#' @return Returns a list containing 1: A data.table object with a date column and the forecasted values; 2: The model evaluation results; 3: The champion model for later use if desired; 4: The name of the champion model.
#' @export
AutoTS <- function(data,
                   TargetName     = "Target",
                   DateName       = "DateTime",
                   FCPeriods      = 30,
                   HoldOutPeriods = 30,
                   TimeUnit       = "day",
                   Lags           = 25,
                   SLags          = 2,
                   NumCores       = 4,
                   SkipModels     = NULL,
                   StepWise       = TRUE,
                   TSClean        = TRUE,
                   PrintUpdates   = FALSE) {

  # Ensure data.table is available
  requireNamespace('data.table', quietly = FALSE)

  # Turn off warnings
  options(warn = -1)

  # Initialize collection variables
  i <- 0
  EvalList <- list()

  # Convert to data.table if not already
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }

  # Check for min value of data
  MinVal <- data[, min(get(TargetName))]

  # Convert to lubridate as_date() or POSIXct
  if (tolower(TimeUnit) != "hour") {
    data[, eval(DateName) := lubridate::as_date(get(DateName))]
  } else {
    data[, eval(DateName) := as.POSIXct(get(DateName))]
    SkipModels <- c(SkipModels, "PROPHET")
    if (length(SkipModels) == 8)
      return("Prophet doesn't run on hourly data. Choose other models.")
  }

  # Ensure data is sorted
  data <- data[order(get(DateName))]

  # Change Target Name
  data.table::setnames(data, paste0(eval(TargetName)), "Target")
  TargetName <- "Target"

  # Create Training data
  data_train <- data[1:(nrow(data) - HoldOutPeriods)]

  # Create Test data
  data_test <- data[(nrow(data) - HoldOutPeriods + 1):nrow(data)]

  # Check for different time aggregations
  MaxDate <- data[, max(get(DateName))]
  FC_Data <- data.table::data.table(Date = seq(1:FCPeriods))

  # Define TS Frequency
  if (tolower(TimeUnit) == "hour") {
    freq <- 24
    FC_Data[, Date := MaxDate + lubridate::hours(Date)]
  } else if (tolower(TimeUnit) == "day") {
    freq <- 365
    FC_Data[, Date := MaxDate + lubridate::days(Date)]
  } else if (tolower(TimeUnit) == "week") {
    freq <- 52
    FC_Data[, Date := MaxDate + lubridate::weeks(Date)]
  } else if (tolower(TimeUnit) == "month") {
    freq <- 12
    FC_Data[, Date := MaxDate + lubridate::month(Date)]
  } else if (tolower(TimeUnit) == "quarter") {
    freq <- 4
    FC_Data[, Date := MaxDate + lubridate::month(4 * Date)]
  } else if (tolower(TimeUnit) == "year") {
    freq <- 1
    FC_Data[, Date := MaxDate + years(Date)]
  } else {
    return("TimeUnit is not in hour, day, week, month,
    quarter, or year")
  }

  # Coerce SLags if too large
  if(freq * SLags > nrow(data_train)) {
    SLags <- floor(nrow(data_train) / freq)
  }

  # Convert data.tables to stats::ts objects
  # User Supplied Frequency
  dataTSTrain <-
    stats::ts(data = data_train,
              start = data_train[, min(get(DateName))][[1]],
              frequency = freq)

  # TSClean Version
  if(TSClean) {
    if(MinVal > 0) {
      Target <- forecast::tsclean(x = dataTSTrain[, TargetName],
                                  replace.missing = TRUE,
                                  lambda = "auto")
    } else {
      Target <- forecast::tsclean(x = dataTSTrain[, TargetName],
                                  replace.missing = TRUE,
                                  lambda = NULL)
    }
  }

  # Model-Based Frequency
  SFreq <- forecast::findfrequency(as.matrix(data_train[,2]))
  dataTSTrain1 <-
    stats::ts(data = data_train,
              start = data_train[, min(get(DateName))][[1]],
              frequency = SFreq)

  # TSClean Version
  if(TSClean) {
    if(MinVal > 0) {
      TargetMB <- forecast::tsclean(x = dataTSTrain1[, TargetName],
                                    replace.missing = TRUE,
                                    lambda = "auto")
    } else {
      TargetMB <- forecast::tsclean(x = dataTSTrain1[, TargetName],
                                    replace.missing = TRUE,
                                    lambda = NULL)
    }
  }


  # Begin model building
  if (!("DSHW" %in% toupper(SkipModels))) {
    # ARFIMA-------------
    # 1)
    if(PrintUpdates) message("DSHW FITTING")
    if(MinVal > 0) {

      # User-Supplied-Freq
      DSHW_Model <-
        tryCatch({forecast::dshw(
          y = dataTSTrain[, TargetName],
          period1 = freq,
          period2 = freq*2,
          alpha = NULL,
          beta = NULL,
          gamma = NULL,
          omega = NULL,
          phi = NULL,
          lambda = "auto",
          biasadj = TRUE,
          armethod = TRUE,
          model = NULL
        )
        },
        error = function(x)
          "empty")

      # Model-Supplied-Freq
      DSHW_Model1 <-
        tryCatch({forecast::dshw(
          y = dataTSTrain1[, TargetName],
          period1 = SFreq,
          period2 = SFreq*2,
          alpha = NULL,
          beta = NULL,
          gamma = NULL,
          omega = NULL,
          phi = NULL,
          lambda = "auto",
          biasadj = TRUE,
          armethod = TRUE,
          model = NULL
        )
        },
        error = function(x)
          "empty")

      # Run for outlier removal and imputation
      if(TSClean) {

        # User-Supplied-Freq
        DSHW_Model2 <-
          tryCatch({forecast::dshw(
            y = Target,
            period1 = freq,
            period2 = freq*2,
            alpha = NULL,
            beta = NULL,
            gamma = NULL,
            omega = NULL,
            phi = NULL,
            lambda = "auto",
            biasadj = TRUE,
            armethod = TRUE,
            model = NULL
          )
          },
          error = function(x)
            "empty")

        # Model-Supplied-Freq
        DSHW_Model3 <-
          tryCatch({forecast::dshw(
            y = TargetMB,
            period1 = SFreq,
            period2 = SFreq*2,
            alpha = NULL,
            beta = NULL,
            gamma = NULL,
            omega = NULL,
            phi = NULL,
            lambda = "auto",
            biasadj = TRUE,
            armethod = TRUE,
            model = NULL
          )
          },
          error = function(x)
            "empty")
      }

    } else {

      # User-Supplied-Freq
      DSHW_Model <-
        tryCatch({forecast::dshw(
          y = dataTSTrain[, TargetName],
          period1 = freq,
          period2 = freq*2,
          alpha = NULL,
          beta = NULL,
          gamma = NULL,
          omega = NULL,
          phi = NULL,
          lambda = NULL,
          biasadj = FALSE,
          armethod = TRUE,
          model = NULL
        )
        },
        error = function(x)
          "empty")

      # Model-Supplied-Freq
      DSHW_Model1 <-
        tryCatch({forecast::dshw(
          y = dataTSTrain1[, TargetName],
          period1 = SFreq,
          period2 = SFreq*2,
          alpha = NULL,
          beta = NULL,
          gamma = NULL,
          omega = NULL,
          phi = NULL,
          lambda = NULL,
          biasadj = FALSE,
          armethod = TRUE,
          model = NULL
        )
        },
        error = function(x)
          "empty")

      # tsclean: impute and replace outliers
      if(TSClean) {

        # User-Supplied-Freq
        DSHW_Model2 <-
          tryCatch({forecast::dshw(
            y = Target,
            period1 = freq,
            period2 = freq*2,
            alpha = NULL,
            beta = NULL,
            gamma = NULL,
            omega = NULL,
            phi = NULL,
            lambda = NULL,
            biasadj = FALSE,
            armethod = TRUE,
            model = NULL
          )
          },
          error = function(x)
            "empty")

        # Model-Supplied-Freq
        DSHW_Model3 <-
          tryCatch({forecast::dshw(
            y = TargetMB,
            period1 = SFreq,
            period2 = SFreq*2,
            alpha = NULL,
            beta = NULL,
            gamma = NULL,
            omega = NULL,
            phi = NULL,
            lambda = NULL,
            biasadj = FALSE,
            armethod = TRUE,
            model = NULL
          )
          },
          error = function(x)
            "empty")
      }
    }

    # Collect Test Data for Model Comparison
    # 2: User-Supplied-Freq
    if (tolower(class(DSHW_Model)) == "forecast") {
      tryCatch({
        data_test_DSHW <- data.table::copy(data_test)
        data_test_DSHW[, ':=' (
          Target = as.numeric(Target),
          ModelName = rep("DSHW", HoldOutPeriods),
          FC_Eval = as.numeric(
            forecast::forecast(DSHW_Model, h = HoldOutPeriods)$mean
          )
        )]

        # Add Evaluation Columns
        # 3)
        data_test_DSHW[, ':=' (
          Resid = Target - FC_Eval,
          PercentError = get(TargetName) / (FC_Eval +
                                              1) - 1,
          AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                          1) - 1)
        )]

        # Increment
        i <- i + 1

        # Collect model filename
        EvalList[[i]] <- data_test_DSHW
      }, error = function(x) "skip")
    }

    # 2: Model-Supplied-Freq
    if (tolower(class(DSHW_Model1)) == "forecast") {
      tryCatch({
        data_test_DSHW1 <- data.table::copy(data_test)
        data_test_DSHW1[, ':=' (
          Target = as.numeric(Target),
          ModelName = rep("DSHW_ModelFreq", HoldOutPeriods),
          FC_Eval = as.numeric(
            forecast::forecast(DSHW_Model1, h = HoldOutPeriods)$mean
          )
        )]

        # Add Evaluation Columns
        # 3)
        data_test_DSHW1[, ':=' (
          Resid = Target - FC_Eval,
          PercentError = get(TargetName) / (FC_Eval +
                                              1) - 1,
          AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                          1) - 1)
        )]

        # Increment
        i <- i + 1

        # Collect model filename
        EvalList[[i]] <- data_test_DSHW1
      }, error = function(x) "skip")
    }

    # If TSClean is TRUE
    if(TSClean) {
      # 2: Model-Supplied-Freq
      if (tolower(class(DSHW_Model2)) == "forecast") {
        tryCatch({
          data_test_DSHW2 <- data.table::copy(data_test)
          data_test_DSHW2[, ':=' (
            Target = as.numeric(Target),
            ModelName = rep("DSHW_TSC", HoldOutPeriods),
            FC_Eval = as.numeric(
              forecast::forecast(DSHW_Model2, h = HoldOutPeriods)$mean
            )
          )]

          # Add Evaluation Columns
          # 3)
          data_test_DSHW2[, ':=' (
            Resid = Target - FC_Eval,
            PercentError = get(TargetName) / (FC_Eval +
                                                1) - 1,
            AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                            1) - 1)
          )]

          # Increment
          i <- i + 1

          # Collect model filename
          EvalList[[i]] <- data_test_DSHW2
        }, error = function(x) "skip")
      }

      # 2: Model-Supplied-Freq
      if (tolower(class(DSHW_Model3)) == "forecast") {
        tryCatch({
          data_test_DSHW3 <- data.table::copy(data_test)
          data_test_DSHW3[, ':=' (
            Target = as.numeric(Target),
            ModelName = rep("DSHW_ModelFreqTSC", HoldOutPeriods),
            FC_Eval = as.numeric(
              forecast::forecast(DSHW_Model3, h = HoldOutPeriods)$mean
            )
          )]

          # Add Evaluation Columns
          # 3)
          data_test_DSHW3[, ':=' (
            Resid = Target - FC_Eval,
            PercentError = get(TargetName) / (FC_Eval +
                                                1) - 1,
            AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                            1) - 1)
          )]

          # Increment
          i <- i + 1

          # Collect model filename
          EvalList[[i]] <- data_test_DSHW3
        }, error = function(x) "skip")
      }
    }
  }

  # ARFIMA Modeling
  if (!("ARFIMA" %in% toupper(SkipModels))) {
    # ARFIMA-------------
    # 1)
    if(PrintUpdates) message("ARFIMA FITTING")
    if (StepWise) {
      if(MinVal > 0) {

        # User-Supplied-Freq
        ARFIMA_model <-
          tryCatch({
            forecast::arfima(
              y = dataTSTrain[, TargetName],
              lambda = TRUE,
              biasadj = TRUE,
              max.p = Lags,
              max.q = Lags,
              max.d = 1,
              max.D = 1,
              ic = "bic",
              stepwise = StepWise,
              num.cores = NumCores
            )
          },
          error = function(x)
            "empty")

        # Model-Supplied-Freq
        ARFIMA_model1 <-
          tryCatch({
            forecast::arfima(
              y = dataTSTrain1[, TargetName],
              lambda = TRUE,
              biasadj = TRUE,
              max.p = Lags,
              max.q = Lags,
              max.d = 1,
              max.D = 1,
              ic = "bic",
              stepwise = StepWise,
              num.cores = NumCores
            )
          },
          error = function(x)
            "empty")

        # TSClean Version
        if(TSClean) {
          # User-Supplied-Freq
          ARFIMA_model2 <-
            tryCatch({
              forecast::arfima(
                y = Target,
                lambda = TRUE,
                biasadj = TRUE,
                max.p = Lags,
                max.q = Lags,
                max.d = 1,
                max.D = 1,
                ic = "bic",
                stepwise = StepWise,
                num.cores = NumCores
              )
            },
            error = function(x)
              "empty")

          # Model-Supplied-Freq
          ARFIMA_model3 <-
            tryCatch({
              forecast::arfima(
                y = TargetMB,
                lambda = TRUE,
                biasadj = TRUE,
                max.p = Lags,
                max.q = Lags,
                max.d = 1,
                max.D = 1,
                ic = "bic",
                stepwise = StepWise,
                num.cores = NumCores
              )
            },
            error = function(x)
              "empty")
        }

      } else {

        # User-Supplied-Freq
        ARFIMA_model <-
          tryCatch({
            forecast::arfima(
              y = dataTSTrain[, TargetName],
              lambda = FALSE,
              biasadj = FALSE,
              max.p = Lags,
              max.q = Lags,
              max.d = 1,
              max.D = 1,
              ic = "bic",
              stepwise = StepWise,
              num.cores = NumCores
            )
          },
          error = function(x)
            "empty")

        # Model-Supplied-Freq
        ARFIMA_model1 <-
          tryCatch({
            forecast::arfima(
              y = dataTSTrain1[, TargetName],
              lambda = FALSE,
              biasadj = FALSE,
              max.p = Lags,
              max.q = Lags,
              max.d = 1,
              max.D = 1,
              ic = "bic",
              stepwise = StepWise,
              num.cores = NumCores
            )
          },
          error = function(x)
            "empty")

        # TSClean Version
        if(TSClean) {
          # User-Supplied-Freq
          ARFIMA_model2 <-
            tryCatch({
              forecast::arfima(
                y = Target,
                lambda = FALSE,
                biasadj = FALSE,
                max.p = Lags,
                max.q = Lags,
                max.d = 1,
                max.D = 1,
                ic = "bic",
                stepwise = StepWise,
                num.cores = NumCores
              )
            },
            error = function(x)
              "empty")

          # Model-Supplied-Freq
          ARFIMA_model3 <-
            tryCatch({
              forecast::arfima(
                y = TargetMB,
                lambda = FALSE,
                biasadj = FALSE,
                max.p = Lags,
                max.q = Lags,
                max.d = 1,
                max.D = 1,
                ic = "bic",
                stepwise = StepWise,
                num.cores = NumCores
              )
            },
            error = function(x)
              "empty")
        }
      }
    } else {
      if(MinVal > 0) {

        # User-Supplied-Freq
        ARFIMA_model <-
          tryCatch({
            forecast::arfima(
              y = dataTSTrain[, TargetName],
              lambda = TRUE,
              biasadj = TRUE,
              max.p = Lags,
              max.q = Lags,
              max.d = 1,
              max.D = 1,
              ic = "bic",
              stepwise = StepWise,
              parallel = TRUE,
              num.cores = NumCores
            )
          },
          error = function(x)
            "empty")

        # Model-Supplied-Freq
        ARFIMA_model1 <-
          tryCatch({
            forecast::arfima(
              y = dataTSTrain1[, TargetName],
              lambda = TRUE,
              biasadj = TRUE,
              max.p = Lags,
              max.q = Lags,
              max.d = 1,
              max.D = 1,
              ic = "bic",
              stepwise = StepWise,
              parallel = TRUE,
              num.cores = NumCores
            )
          },
          error = function(x)
            "empty")

        # TSClean Version
        if(TSClean) {
          # User-Supplied-Freq
          ARFIMA_model2 <-
            tryCatch({
              forecast::arfima(
                y = Target,
                lambda = TRUE,
                biasadj = TRUE,
                max.p = Lags,
                max.q = Lags,
                max.d = 1,
                max.D = 1,
                ic = "bic",
                stepwise = StepWise,
                parallel = TRUE,
                num.cores = NumCores
              )
            },
            error = function(x)
              "empty")

          # Model-Supplied-Freq
          ARFIMA_model3 <-
            tryCatch({
              forecast::arfima(
                y = TargetMB,
                lambda = TRUE,
                biasadj = TRUE,
                max.p = Lags,
                max.q = Lags,
                max.d = 1,
                max.D = 1,
                ic = "bic",
                stepwise = StepWise,
                parallel = TRUE,
                num.cores = NumCores
              )
            },
            error = function(x)
              "empty")
        }

      } else {

        # User-Supplied-Freq
        ARFIMA_model <-
          tryCatch({
            forecast::arfima(
              y = dataTSTrain[, TargetName],
              lambda = FALSE,
              biasadj = FALSE,
              max.p = Lags,
              max.q = Lags,
              max.d = 1,
              max.D = 1,
              ic = "bic",
              stepwise = StepWise,
              parallel = TRUE,
              num.cores = NumCores
            )
          },
          error = function(x)
            "empty")

        # Model-Supplied-Freq
        ARFIMA_model1 <-
          tryCatch({
            forecast::arfima(
              y = dataTSTrain1[, TargetName],
              lambda = FALSE,
              biasadj = FALSE,
              max.p = Lags,
              max.q = Lags,
              max.d = 1,
              max.D = 1,
              ic = "bic",
              stepwise = StepWise,
              parallel = TRUE,
              num.cores = NumCores
            )
          },
          error = function(x)
            "empty")

        # TSClean Version
        if(TSClean) {
          # User-Supplied-Freq
          ARFIMA_model2 <-
            tryCatch({
              forecast::arfima(
                y = Target,
                lambda = FALSE,
                biasadj = FALSE,
                max.p = Lags,
                max.q = Lags,
                max.d = 1,
                max.D = 1,
                ic = "bic",
                stepwise = StepWise,
                parallel = TRUE,
                num.cores = NumCores
              )
            },
            error = function(x)
              "empty")

          # Model-Supplied-Freq
          ARFIMA_model3 <-
            tryCatch({
              forecast::arfima(
                y = TargetMB,
                lambda = FALSE,
                biasadj = FALSE,
                max.p = Lags,
                max.q = Lags,
                max.d = 1,
                max.D = 1,
                ic = "bic",
                stepwise = StepWise,
                parallel = TRUE,
                num.cores = NumCores
              )
            },
            error = function(x)
              "empty")
        }
      }
    }


    # Collect Test Data for Model Comparison
    # 2: User-Supplied-Freq
    if (tolower(class(ARFIMA_model)) == "fracdiff") {
      tryCatch({
        data_test_ARF <- data.table::copy(data_test)
        data_test_ARF[, ':=' (
          Target = as.numeric(Target),
          ModelName = rep("ARFIMA", HoldOutPeriods),
          FC_Eval = as.numeric(
            forecast::forecast(ARFIMA_model, h = HoldOutPeriods)$mean
          )
        )]

        # Add Evaluation Columns
        # 3)
        data_test_ARF[, ':=' (
          Resid = Target - FC_Eval,
          PercentError = get(TargetName) / (FC_Eval +
                                              1) - 1,
          AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                          1) - 1)
        )]

        # Increment
        i <- i + 1

        # Collect model filename
        EvalList[[i]] <- data_test_ARF
      }, error = function(x) "skip")
    }

    # 2: Model-Supplied-Freq
    if (tolower(class(ARFIMA_model1)) == "fracdiff") {
      tryCatch({
        data_test_ARF1 <- data.table::copy(data_test)
        data_test_ARF1[, ':=' (
          Target = as.numeric(Target),
          ModelName = rep("ARFIMA_ModelFreq", HoldOutPeriods),
          FC_Eval = as.numeric(
            forecast::forecast(ARFIMA_model1, h = HoldOutPeriods)$mean
          )
        )]

        # Add Evaluation Columns
        # 3)
        data_test_ARF1[, ':=' (
          Resid = Target - FC_Eval,
          PercentError = get(TargetName) / (FC_Eval +
                                              1) - 1,
          AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                          1) - 1)
        )]

        # Increment
        i <- i + 1

        # Collect model filename
        EvalList[[i]] <- data_test_ARF1
      }, error = function(x) "skip")
    }

    # TSClean Version
    if(TSClean) {
      # 2: User-Supplied-Freq
      if (tolower(class(ARFIMA_model2)) == "fracdiff") {
        tryCatch({
          data_test_ARF2 <- data.table::copy(data_test)
          data_test_ARF2[, ':=' (
            Target = as.numeric(Target),
            ModelName = rep("ARFIMA_TSC", HoldOutPeriods),
            FC_Eval = as.numeric(
              forecast::forecast(ARFIMA_model2, h = HoldOutPeriods)$mean
            )
          )]

          # Add Evaluation Columns
          # 3)
          data_test_ARF2[, ':=' (
            Resid = Target - FC_Eval,
            PercentError = get(TargetName) / (FC_Eval +
                                                1) - 1,
            AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                            1) - 1)
          )]

          # Increment
          i <- i + 1

          # Collect model filename
          EvalList[[i]] <- data_test_ARF2
        }, error = function(x) "skip")
      }

      # 2: Model-Supplied-Freq
      if (tolower(class(ARFIMA_model3)) == "fracdiff") {
        tryCatch({
          data_test_ARF3 <- data.table::copy(data_test)
          data_test_ARF3[, ':=' (
            Target = as.numeric(Target),
            ModelName = rep("ARFIMA_ModelFreqTSC", HoldOutPeriods),
            FC_Eval = as.numeric(
              forecast::forecast(ARFIMA_model3, h = HoldOutPeriods)$mean
            )
          )]

          # Add Evaluation Columns
          # 3)
          data_test_ARF3[, ':=' (
            Resid = Target - FC_Eval,
            PercentError = get(TargetName) / (FC_Eval +
                                                1) - 1,
            AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                            1) - 1)
          )]

          # Increment
          i <- i + 1

          # Collect model filename
          EvalList[[i]] <- data_test_ARF3
        }, error = function(x) "skip")
      }
    }
  }

  # Arima
  if (!("ARIMA" %in% toupper(SkipModels))) {
    # ARIMA-------------
    # 1)
    if(PrintUpdates) message("ARIMA FITTING")
    if (StepWise) {
      if(MinVal > 0) {

        # User-Supplied-Freq
        ARIMA_model <-
          tryCatch({
            forecast::auto.arima(
              y = dataTSTrain[, TargetName],
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
              num.cores = NumCores
            )
          },
          error = function(x)
            "empty")

        # Model-Supplied-Freq
        ARIMA_model1 <-
          tryCatch({
            forecast::auto.arima(
              y = dataTSTrain1[, TargetName],
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
              num.cores = NumCores
            )
          },
          error = function(x)
            "empty")

        # TSClean Verison
        if(TSClean) {
          # User-Supplied-Freq
          ARIMA_model2 <-
            tryCatch({
              forecast::auto.arima(
                y = Target,
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
                num.cores = NumCores
              )
            },
            error = function(x)
              "empty")

          # Model-Supplied-Freq
          ARIMA_model3 <-
            tryCatch({
              forecast::auto.arima(
                y = TargetMB,
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
                num.cores = NumCores
              )
            },
            error = function(x)
              "empty")
        }
      } else {

        # User-Supplied-Freq
        ARIMA_model <-
          tryCatch({
            forecast::auto.arima(
              y = dataTSTrain[, TargetName],
              max.p = Lags,
              max.q = Lags,
              max.P = SLags,
              max.Q = SLags,
              max.d = 1,
              max.D = 1,
              ic = "bic",
              lambda = FALSE,
              biasadj = FALSE,
              stepwise = StepWise,
              num.cores = NumCores
            )
          },
          error = function(x)
            "empty")

        # Model-Supplied-Freq
        ARIMA_model1 <-
          tryCatch({
            forecast::auto.arima(
              y = dataTSTrain1[, TargetName],
              max.p = Lags,
              max.q = Lags,
              max.P = SLags,
              max.Q = SLags,
              max.d = 1,
              max.D = 1,
              ic = "bic",
              lambda = FALSE,
              biasadj = FALSE,
              stepwise = StepWise,
              num.cores = NumCores
            )
          },
          error = function(x)
            "empty")

        # TSClean Version
        if(TSClean) {
          # User-Supplied-Freq
          ARIMA_model2 <-
            tryCatch({
              forecast::auto.arima(
                y = Target,
                max.p = Lags,
                max.q = Lags,
                max.P = SLags,
                max.Q = SLags,
                max.d = 1,
                max.D = 1,
                ic = "bic",
                lambda = FALSE,
                biasadj = FALSE,
                stepwise = StepWise,
                num.cores = NumCores
              )
            },
            error = function(x)
              "empty")

          # Model-Supplied-Freq
          ARIMA_model3 <-
            tryCatch({
              forecast::auto.arima(
                y = TargetMB,
                max.p = Lags,
                max.q = Lags,
                max.P = SLags,
                max.Q = SLags,
                max.d = 1,
                max.D = 1,
                ic = "bic",
                lambda = FALSE,
                biasadj = FALSE,
                stepwise = StepWise,
                num.cores = NumCores
              )
            },
            error = function(x)
              "empty")
        }
      }
    } else {
      if(MinVal > 0) {

        # User-Supplied-Freq
        ARIMA_model <-
          tryCatch({
            forecast::auto.arima(
              y = dataTSTrain[, TargetName],
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
              num.cores = NumCores
            )
          },
          error = function(x)
            "empty")

        # Model-Supplied-Freq
        ARIMA_model1 <-
          tryCatch({
            forecast::auto.arima(
              y = dataTSTrain1[, TargetName],
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
              num.cores = NumCores
            )
          },
          error = function(x)
            "empty")

        # TSClean Version
        if(TSClean) {
          # User-Supplied-Freq
          ARIMA_model2 <-
            tryCatch({
              forecast::auto.arima(
                y = Target,
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
                num.cores = NumCores
              )
            },
            error = function(x)
              "empty")

          # Model-Supplied-Freq
          ARIMA_model3 <-
            tryCatch({
              forecast::auto.arima(
                y = TargetMB,
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
                num.cores = NumCores
              )
            },
            error = function(x)
              "empty")
        }
      } else {

        # User-Supplied-Freq
        ARIMA_model <-
          tryCatch({
            forecast::auto.arima(
              y = dataTSTrain[, TargetName],
              max.p = Lags,
              max.q = Lags,
              max.P = SLags,
              max.Q = SLags,
              max.d = 1,
              max.D = 1,
              ic = "bic",
              lambda = FALSE,
              biasadj = FALSE,
              stepwise = StepWise,
              parallel = TRUE,
              num.cores = NumCores
            )
          },
          error = function(x)
            "empty")

        # Model-Supplied-Freq
        ARIMA_model1 <-
          tryCatch({
            forecast::auto.arima(
              y = dataTSTrain1[, TargetName],
              max.p = Lags,
              max.q = Lags,
              max.P = SLags,
              max.Q = SLags,
              max.d = 1,
              max.D = 1,
              ic = "bic",
              lambda = FALSE,
              biasadj = FALSE,
              stepwise = StepWise,
              parallel = TRUE,
              num.cores = NumCores
            )
          },
          error = function(x)
            "empty")

        # TSClean Version
        if(TSClean) {
          # User-Supplied-Freq
          ARIMA_model2 <-
            tryCatch({
              forecast::auto.arima(
                y = Target,
                max.p = Lags,
                max.q = Lags,
                max.P = SLags,
                max.Q = SLags,
                max.d = 1,
                max.D = 1,
                ic = "bic",
                lambda = FALSE,
                biasadj = FALSE,
                stepwise = StepWise,
                parallel = TRUE,
                num.cores = NumCores
              )
            },
            error = function(x)
              "empty")

          # Model-Supplied-Freq
          ARIMA_model3 <-
            tryCatch({
              forecast::auto.arima(
                y = TargetMB,
                max.p = Lags,
                max.q = Lags,
                max.P = SLags,
                max.Q = SLags,
                max.d = 1,
                max.D = 1,
                ic = "bic",
                lambda = FALSE,
                biasadj = FALSE,
                stepwise = StepWise,
                parallel = TRUE,
                num.cores = NumCores
              )
            },
            error = function(x)
              "empty")
        }
      }
    }

    # Collect Test Data for Model Comparison
    # 2: User-Supplied-Freq
    if (tolower(class(ARIMA_model)[1]) == "arima") {
      tryCatch({
        data_test_ARI <- data.table::copy(data_test)
        data_test_ARI[, ':=' (
          Target = as.numeric(Target),
          ModelName = rep("ARIMA", HoldOutPeriods),
          FC_Eval = as.numeric(
            forecast::forecast(ARIMA_model, h = HoldOutPeriods)$mean
          )
        )]

        # Add Evaluation Columns
        # 3)
        data_test_ARI[, ':=' (
          Resid = get(TargetName) - FC_Eval,
          PercentError = get(TargetName) / (FC_Eval +
                                              1) - 1,
          AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                          1) - 1)
        )]

        # Increment
        i <- i + 1

        # Collect model filename
        EvalList[[i]] <- data_test_ARI
      }, error = function(x) "skip")
    }

    # Model-Supplied-Freq
    if (tolower(class(ARIMA_model1)[1]) == "arima") {
      tryCatch({
        data_test_ARI1 <- data.table::copy(data_test)
        data_test_ARI1[, ':=' (
          Target = as.numeric(Target),
          ModelName = rep("ARIMA_ModelFreq", HoldOutPeriods),
          FC_Eval = as.numeric(
            forecast::forecast(ARIMA_model1, h = HoldOutPeriods)$mean
          )
        )]

        # Add Evaluation Columns
        # 3)
        data_test_ARI1[, ':=' (
          Resid = get(TargetName) - FC_Eval,
          PercentError = get(TargetName) / (FC_Eval +
                                              1) - 1,
          AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                          1) - 1)
        )]

        # Increment
        i <- i + 1

        # Collect model filename
        EvalList[[i]] <- data_test_ARI1
      }, error = function(x) "skip")
    }

    # TSClean Version
    if(TSClean) {
      # 2: User-Supplied-Freq
      if (tolower(class(ARIMA_model2)[1]) == "arima") {
        tryCatch({
          data_test_ARI2 <- data.table::copy(data_test)
          data_test_ARI2[, ':=' (
            Target = as.numeric(Target),
            ModelName = rep("ARIMA_TSC", HoldOutPeriods),
            FC_Eval = as.numeric(
              forecast::forecast(ARIMA_model2, h = HoldOutPeriods)$mean
            )
          )]

          # Add Evaluation Columns
          # 3)
          data_test_ARI2[, ':=' (
            Resid = get(TargetName) - FC_Eval,
            PercentError = get(TargetName) / (FC_Eval +
                                                1) - 1,
            AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                            1) - 1)
          )]

          # Increment
          i <- i + 1

          # Collect model filename
          EvalList[[i]] <- data_test_ARI2
        }, error = function(x) "skip")
      }

      # Model-Supplied-Freq
      if (tolower(class(ARIMA_model3)[1]) == "arima") {
        tryCatch({
          data_test_ARI3 <- data.table::copy(data_test)
          data_test_ARI3[, ':=' (
            Target = as.numeric(Target),
            ModelName = rep("ARIMA_ModelFreqTSC", HoldOutPeriods),
            FC_Eval = as.numeric(
              forecast::forecast(ARIMA_model3, h = HoldOutPeriods)$mean
            )
          )]

          # Add Evaluation Columns
          # 3)
          data_test_ARI3[, ':=' (
            Resid = get(TargetName) - FC_Eval,
            PercentError = get(TargetName) / (FC_Eval +
                                                1) - 1,
            AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                            1) - 1)
          )]

          # Increment
          i <- i + 1

          # Collect model filename
          EvalList[[i]] <- data_test_ARI3
        }, error = function(x) "skip")
      }
    }
  }

  if (!("ETS" %in% toupper(SkipModels))) {
    # EXPONENTIAL SMOOTHING-------------
    # 1)
    if(PrintUpdates) message("ETS FITTING")

    # User-Supplied-Freq
    if (freq > 24) {
      if(MinVal > 0) {
        # when > 24, model's third letter has to be N for none
        EXPSMOOTH_model <-
          tryCatch({
            forecast::ets(
              y = dataTSTrain[, TargetName],
              model = "ZZN",
              allow.multiplicative.trend = TRUE,
              restrict = TRUE,
              lambda = TRUE,
              biasadj = TRUE
            )}, error = function(x) "empty")

        # TSClean Version
        if(TSClean) {
          EXPSMOOTH_model2 <-
            tryCatch({
              forecast::ets(
                y = Target,
                model = "ZZN",
                allow.multiplicative.trend = TRUE,
                restrict = TRUE,
                lambda = TRUE,
                biasadj = TRUE
              )}, error = function(x) "empty")
        }
      } else {
        # when > 24, model's third letter has to be N for none
        EXPSMOOTH_model <-
          tryCatch({
            forecast::ets(
              y = dataTSTrain[, TargetName],
              model = "ZZN",
              allow.multiplicative.trend = TRUE,
              restrict = TRUE,
              lambda = FALSE,
              biasadj = FALSE
            )}, error = function(x) "empty")

        # TSClean Version
        if(TSClean) {
          EXPSMOOTH_model2 <-
            tryCatch({
              forecast::ets(
                y = Target,
                model = "ZZN",
                allow.multiplicative.trend = TRUE,
                restrict = TRUE,
                lambda = FALSE,
                biasadj = FALSE
              )}, error = function(x) "empty")
        }
      }
    } else {
      if(MinVal > 0) {
        EXPSMOOTH_model <-
          tryCatch({
            forecast::ets(
              y = dataTSTrain[, TargetName],
              model = "ZZZ",
              allow.multiplicative.trend = TRUE,
              restrict = TRUE,
              lambda = TRUE,
              biasadj = TRUE
            )
          },
          error = function(x)
            "empty")

        # TSClean Version
        if(TSClean) {
          EXPSMOOTH_model2 <-
            tryCatch({
              forecast::ets(
                y = Target,
                model = "ZZZ",
                allow.multiplicative.trend = TRUE,
                restrict = TRUE,
                lambda = TRUE,
                biasadj = TRUE
              )
            },
            error = function(x)
              "empty")
        }
      } else {
        EXPSMOOTH_model <-
          tryCatch({
            forecast::ets(
              y = dataTSTrain[, TargetName],
              model = "ZZZ",
              allow.multiplicative.trend = TRUE,
              restrict = TRUE,
              lambda = FALSE,
              biasadj = FALSE
            )
          },
          error = function(x)
            "empty")

        # TSClean Version
        if(TSClean) {
          EXPSMOOTH_model2 <-
            tryCatch({
              forecast::ets(
                y = Target,
                model = "ZZZ",
                allow.multiplicative.trend = TRUE,
                restrict = TRUE,
                lambda = FALSE,
                biasadj = FALSE
              )
            },
            error = function(x)
              "empty")
        }
      }
    }

    # Model-Supplied-Freq
    if (SFreq > 24) {
      if(MinVal > 0) {
        # when > 24, model's third letter has to be N for none
        EXPSMOOTH_model1 <-
          tryCatch({
            forecast::ets(
              y = dataTSTrain1[, TargetName],
              model = "ZZN",
              allow.multiplicative.trend = TRUE,
              restrict = TRUE,
              lambda = TRUE,
              biasadj = TRUE
            )}, error = function(x) "empty")

        # TSClean Version
        if(TSClean) {
          EXPSMOOTH_model3 <-
            tryCatch({
              forecast::ets(
                y = TargetMB,
                model = "ZZN",
                allow.multiplicative.trend = TRUE,
                restrict = TRUE,
                lambda = TRUE,
                biasadj = TRUE
              )}, error = function(x) "empty")
        }
      } else {
        # when > 24, model's third letter has to be N for none
        EXPSMOOTH_model1 <-
          tryCatch({
            forecast::ets(
              y = dataTSTrain1[, TargetName],
              model = "ZZN",
              allow.multiplicative.trend = TRUE,
              restrict = TRUE,
              lambda = FALSE,
              biasadj = FALSE
            )}, error = function(x) "empty")

        # TSClean Version
        if(TSClean) {
          EXPSMOOTH_model3 <-
            tryCatch({
              forecast::ets(
                y = TargetMB,
                model = "ZZN",
                allow.multiplicative.trend = TRUE,
                restrict = TRUE,
                lambda = FALSE,
                biasadj = FALSE
              )}, error = function(x) "empty")
        }
      }
    } else {
      if(MinVal > 0) {
        EXPSMOOTH_model1 <-
          tryCatch({
            forecast::ets(
              y = dataTSTrain1[, TargetName],
              model = "ZZZ",
              allow.multiplicative.trend = TRUE,
              restrict = TRUE,
              lambda = TRUE,
              biasadj = TRUE
            )
          },
          error = function(x)
            "empty")

        # TSClean Version
        if(TSClean) {
          EXPSMOOTH_model3 <-
            tryCatch({
              forecast::ets(
                y = TargetMB,
                model = "ZZZ",
                allow.multiplicative.trend = TRUE,
                restrict = TRUE,
                lambda = TRUE,
                biasadj = TRUE
              )
            },
            error = function(x)
              "empty")
        }
      } else {
        EXPSMOOTH_model1 <-
          tryCatch({
            forecast::ets(
              y = dataTSTrain1[, TargetName],
              model = "ZZZ",
              allow.multiplicative.trend = TRUE,
              restrict = TRUE,
              lambda = FALSE,
              biasadj = FALSE
            )
          },
          error = function(x)
            "empty")

        # TSClean Version
        if(TSClean) {
          EXPSMOOTH_model3 <-
            tryCatch({
              forecast::ets(
                y = TargetMB,
                model = "ZZZ",
                allow.multiplicative.trend = TRUE,
                restrict = TRUE,
                lambda = FALSE,
                biasadj = FALSE
              )
            },
            error = function(x)
              "empty")
        }
      }
    }

    # Collect Test Data for Model Comparison
    # 2: User-Supplied-Freq
    if (tolower(class(EXPSMOOTH_model)) == "ets") {
      tryCatch({
        data_test_ETS <- data.table::copy(data_test)
        data_test_ETS[, ':=' (
          Target = as.numeric(Target),
          ModelName = rep("ETS", HoldOutPeriods),
          FC_Eval = as.numeric(
            forecast::forecast(EXPSMOOTH_model, h = HoldOutPeriods)$mean
          )
        )]

        # Add Evaluation Columns
        # 3)
        data_test_ETS[, ':=' (
          Resid = get(TargetName) - FC_Eval,
          PercentError = get(TargetName) / (FC_Eval +
                                              1) - 1,
          AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                          1) - 1)
        )]

        # Increment
        i <- i + 1

        # Collect model filename
        EvalList[[i]] <- data_test_ETS
      }, error = function(x) "skip")
    }

    # 2: Model-Based-Freq
    if (tolower(class(EXPSMOOTH_model1)) == "ets") {
      tryCatch({
        data_test_ETS1 <- data.table::copy(data_test)
        data_test_ETS1[, ':=' (
          Target = as.numeric(Target),
          ModelName = rep("ETS_ModelFreq", HoldOutPeriods),
          FC_Eval = as.numeric(
            forecast::forecast(EXPSMOOTH_model1, h = HoldOutPeriods)$mean
          )
        )]

        # Add Evaluation Columns
        # 3)
        data_test_ETS1[, ':=' (
          Resid = get(TargetName) - FC_Eval,
          PercentError = get(TargetName) / (FC_Eval +
                                              1) - 1,
          AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                          1) - 1)
        )]

        # Increment
        i <- i + 1

        # Collect model filename
        EvalList[[i]] <- data_test_ETS1
      }, error = function(x) "skip")
    }

    # TSClean Version
    if(TSClean) {
      # 2: User-Supplied-Freq
      if (tolower(class(EXPSMOOTH_model2)) == "ets") {
        tryCatch({
          data_test_ETS2 <- data.table::copy(data_test)
          data_test_ETS2[, ':=' (
            Target = as.numeric(Target),
            ModelName = rep("ETS", HoldOutPeriods),
            FC_Eval = as.numeric(
              forecast::forecast(EXPSMOOTH_model2, h = HoldOutPeriods)$mean
            )
          )]

          # Add Evaluation Columns
          # 3)
          data_test_ETS2[, ':=' (
            Resid = get(TargetName) - FC_Eval,
            PercentError = get(TargetName) / (FC_Eval +
                                                1) - 1,
            AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                            1) - 1)
          )]

          # Increment
          i <- i + 1

          # Collect model filename
          EvalList[[i]] <- data_test_ETS2
        }, error = function(x) "skip")
      }

      # 2: Model-Based-Freq
      if (tolower(class(EXPSMOOTH_model3)) == "ets") {
        tryCatch({
          data_test_ETS3 <- data.table::copy(data_test)
          data_test_ETS3[, ':=' (
            Target = as.numeric(Target),
            ModelName = rep("ETS_ModelFreqTSC", HoldOutPeriods),
            FC_Eval = as.numeric(
              forecast::forecast(EXPSMOOTH_model3, h = HoldOutPeriods)$mean
            )
          )]

          # Add Evaluation Columns
          # 3)
          data_test_ETS3[, ':=' (
            Resid = get(TargetName) - FC_Eval,
            PercentError = get(TargetName) / (FC_Eval +
                                                1) - 1,
            AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                            1) - 1)
          )]

          # Increment
          i <- i + 1

          # Collect model filename
          EvalList[[i]] <- data_test_ETS3
        }, error = function(x) "skip")
      }
    }
  }

  if (!("TBATS" %in% toupper(SkipModels))) {
    # TBATS-------------
    # 1)
    if(PrintUpdates) message("TBATS FITTING")
    if(MinVal > 0) {

      # User-Supplied-Freq
      TBATS_model <-
        tryCatch({
          forecast::tbats(
            y               = dataTSTrain[, TargetName],
            use.arma.errors = TRUE,
            lambda          = TRUE,
            biasadj         = TRUE,
            max.p           = Lags,
            max.q           = Lags,
            max.P           = SLags,
            max.Q           = SLags,
            max.d           = 1,
            max.D           = 1,
            num.cores       = NumCores
          )
        },
        error = function(x)
          "empty")

      # Model-Supplied-Freq
      TBATS_model1 <-
        tryCatch({
          forecast::tbats(
            y               = dataTSTrain1[, TargetName],
            use.arma.errors = TRUE,
            lambda          = TRUE,
            biasadj         = TRUE,
            max.p           = Lags,
            max.q           = Lags,
            max.P           = SLags,
            max.Q           = SLags,
            max.d           = 1,
            max.D           = 1,
            num.cores       = NumCores
          )
        },
        error = function(x)
          "empty")

      # TSClean Version
      if(TSClean) {
        # User-Supplied-Freq
        TBATS_model2 <-
          tryCatch({
            forecast::tbats(
              y               = Target,
              use.arma.errors = TRUE,
              lambda          = TRUE,
              biasadj         = TRUE,
              max.p           = Lags,
              max.q           = Lags,
              max.P           = SLags,
              max.Q           = SLags,
              max.d           = 1,
              max.D           = 1,
              num.cores       = NumCores
            )
          },
          error = function(x)
            "empty")

        # Model-Supplied-Freq
        TBATS_model3 <-
          tryCatch({
            forecast::tbats(
              y               = TargetMB,
              use.arma.errors = TRUE,
              lambda          = TRUE,
              biasadj         = TRUE,
              max.p           = Lags,
              max.q           = Lags,
              max.P           = SLags,
              max.Q           = SLags,
              max.d           = 1,
              max.D           = 1,
              num.cores       = NumCores
            )
          },
          error = function(x)
            "empty")
      }
    } else {

      # User-Supplied-Freq
      TBATS_model <-
        tryCatch({
          forecast::tbats(
            y               = dataTSTrain[, TargetName],
            use.arma.errors = TRUE,
            lambda          = FALSE,
            biasadj         = FALSE,
            max.p           = Lags,
            max.q           = Lags,
            max.P           = SLags,
            max.Q           = SLags,
            max.d           = 1,
            max.D           = 1,
            num.cores       = NumCores
          )
        },
        error = function(x)
          "empty")

      # Model-Supplied-Freq
      TBATS_model1 <-
        tryCatch({
          forecast::tbats(
            y               = dataTSTrain1[, TargetName],
            use.arma.errors = TRUE,
            lambda          = FALSE,
            biasadj         = FALSE,
            max.p           = Lags,
            max.q           = Lags,
            max.P           = SLags,
            max.Q           = SLags,
            max.d           = 1,
            max.D           = 1,
            num.cores       = NumCores
          )
        },
        error = function(x)
          "empty")

      # TSClean Version
      if(TSClean) {
        # User-Supplied-Freq
        TBATS_model2 <-
          tryCatch({
            forecast::tbats(
              y               = Target,
              use.arma.errors = TRUE,
              lambda          = FALSE,
              biasadj         = FALSE,
              max.p           = Lags,
              max.q           = Lags,
              max.P           = SLags,
              max.Q           = SLags,
              max.d           = 1,
              max.D           = 1,
              num.cores       = NumCores
            )
          },
          error = function(x)
            "empty")

        # Model-Supplied-Freq
        TBATS_model3 <-
          tryCatch({
            forecast::tbats(
              y               = TargetMB,
              use.arma.errors = TRUE,
              lambda          = FALSE,
              biasadj         = FALSE,
              max.p           = Lags,
              max.q           = Lags,
              max.P           = SLags,
              max.Q           = SLags,
              max.d           = 1,
              max.D           = 1,
              num.cores       = NumCores
            )
          },
          error = function(x)
            "empty")
      }
    }

    # User-Supplied-Freq
    if (class(TBATS_model)[1] == "tbats" |
        class(TBATS_model)[1] == "bats") {
      tryCatch({
        # Collect Test Data for Model Comparison
        # 2)
        data_test_TBATS <- data.table::copy(data_test)
        data_test_TBATS[, ':=' (
          Target = as.numeric(Target),
          ModelName = rep("TBATS", HoldOutPeriods),
          FC_Eval = as.numeric(
            forecast::forecast(TBATS_model, h = HoldOutPeriods)$mean
          )
        )]

        # Add Evaluation Columns
        # 3)
        data_test_TBATS[, ':=' (
          Resid = get(TargetName) - FC_Eval,
          PercentError = get(TargetName) / (FC_Eval +
                                              1) - 1,
          AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                          1) - 1)
        )]

        # Increment
        i <- i + 1

        # Collect model filename
        EvalList[[i]] <- data_test_TBATS
      }, error = function(x) "skip")
    }

    # Model-Supplied-Freq
    if (class(TBATS_model1)[1] == "tbats" |
        class(TBATS_model1)[1] == "bats") {
      tryCatch({
        # Collect Test Data for Model Comparison
        # 2)
        data_test_TBATS1 <- data.table::copy(data_test)
        data_test_TBATS1[, ':=' (
          Target = as.numeric(Target),
          ModelName = rep("TBATS_ModelFreq", HoldOutPeriods),
          FC_Eval = as.numeric(
            forecast::forecast(TBATS_model1, h = HoldOutPeriods)$mean
          )
        )]

        # Add Evaluation Columns
        # 3)
        data_test_TBATS1[, ':=' (
          Resid = get(TargetName) - FC_Eval,
          PercentError = get(TargetName) / (FC_Eval +
                                              1) - 1,
          AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                          1) - 1)
        )]

        # Increment
        i <- i + 1

        # Collect model filename
        EvalList[[i]] <- data_test_TBATS1
      }, error = function(x) "skip")
    }

    # TSClean Version
    if(TSClean) {
      # User-Supplied-Freq
      if (class(TBATS_model2)[1] == "tbats" |
          class(TBATS_model2)[1] == "bats") {
        tryCatch({
          # Collect Test Data for Model Comparison
          # 2)
          data_test_TBATS2 <- data.table::copy(data_test)
          data_test_TBATS2[, ':=' (
            Target = as.numeric(Target),
            ModelName = rep("TBATS_TSC", HoldOutPeriods),
            FC_Eval = as.numeric(
              forecast::forecast(TBATS_model2, h = HoldOutPeriods)$mean
            )
          )]

          # Add Evaluation Columns
          # 3)
          data_test_TBATS2[, ':=' (
            Resid = get(TargetName) - FC_Eval,
            PercentError = get(TargetName) / (FC_Eval +
                                                1) - 1,
            AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                            1) - 1)
          )]

          # Increment
          i <- i + 1

          # Collect model filename
          EvalList[[i]] <- data_test_TBATS2
        }, error = function(x) "skip")
      }

      # Model-Supplied-Freq
      if (class(TBATS_model3)[1] == "tbats" |
          class(TBATS_model3)[1] == "bats") {
        tryCatch({
          # Collect Test Data for Model Comparison
          # 2)
          data_test_TBATS3 <- data.table::copy(data_test)
          data_test_TBATS3[, ':=' (
            Target = as.numeric(Target),
            ModelName = rep("TBATS_ModelFreqTSC", HoldOutPeriods),
            FC_Eval = as.numeric(
              forecast::forecast(TBATS_model3, h = HoldOutPeriods)$mean
            )
          )]

          # Add Evaluation Columns
          # 3)
          data_test_TBATS3[, ':=' (
            Resid = get(TargetName) - FC_Eval,
            PercentError = get(TargetName) / (FC_Eval +
                                                1) - 1,
            AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                            1) - 1)
          )]

          # Increment
          i <- i + 1

          # Collect model filename
          EvalList[[i]] <- data_test_TBATS3
        }, error = function(x) "skip")
      }
    }
  }

  if (!("TSLM" %in% toupper(SkipModels))) {
    # LINEAR MODEL WITH TIME SERIES COMPONENTS-------------
    # 1)
    if(PrintUpdates) message("TSLM FITTING")
    if(MinVal > 0) {

      # User-Supplied-Freq
      TSLM_model <-
        tryCatch({
          forecast::tslm(dataTSTrain[, TargetName] ~ trend + season,
                         lambda = TRUE,
                         biasadj = TRUE)
        },
        error = function(x)
          "empty")

      # Model-Supplied-Freq
      TSLM_model1 <-
        tryCatch({
          forecast::tslm(dataTSTrain1[, TargetName] ~ trend + season,
                         lambda = TRUE,
                         biasadj = TRUE)
        },
        error = function(x)
          "empty")

      # TSClean Version
      if(TSClean) {
        # User-Supplied-Freq
        TSLM_model2 <-
          tryCatch({
            forecast::tslm(Target ~ trend + season,
                           lambda = TRUE,
                           biasadj = TRUE)
          },
          error = function(x)
            "empty")

        # Model-Supplied-Freq
        TSLM_model3 <-
          tryCatch({
            forecast::tslm(TargetMB ~ trend + season,
                           lambda = TRUE,
                           biasadj = TRUE)
          },
          error = function(x)
            "empty")
      }
    } else {
      # User-Supplied-Freq
      TSLM_model <-
        tryCatch({
          forecast::tslm(dataTSTrain[, TargetName] ~ trend + season,
                         lambda = FALSE,
                         biasadj = FALSE)
        },
        error = function(x)
          "empty")

      # Model-Supplied-Freq
      TSLM_model1 <-
        tryCatch({
          forecast::tslm(dataTSTrain1[, TargetName] ~ trend + season,
                         lambda = TRUE,
                         biasadj = TRUE)
        },
        error = function(x)
          "empty")

      # TSClean Version
      if(TSClean) {
        TSLM_model2 <-
          tryCatch({
            forecast::tslm(Target ~ trend + season,
                           lambda = FALSE,
                           biasadj = FALSE)
          },
          error = function(x)
            "empty")

        # Model-Supplied-Freq
        TSLM_model3 <-
          tryCatch({
            forecast::tslm(TargetMB ~ trend + season,
                           lambda = TRUE,
                           biasadj = TRUE)
          },
          error = function(x)
            "empty")
      }
    }

    # User-Supplied-Freq
    if (tolower(class(TSLM_model)[1]) == "tslm") {
      tryCatch({
        # Collect Test Data for Model Comparison
        # 2)
        data_test_TSLM <- data.table::copy(data_test)
        data_test_TSLM[, ':=' (
          Target = as.numeric(Target),
          ModelName = rep("TSLM", HoldOutPeriods),
          FC_Eval = as.numeric(forecast::forecast(TSLM_model,
                                                  h = HoldOutPeriods)$mean)
        )]

        # Add Evaluation Columns
        # 3)
        data_test_TSLM[, ':=' (
          Resid = get(TargetName) - FC_Eval,
          PercentError = get(TargetName) / (FC_Eval +
                                              1) - 1,
          AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                          1) - 1)
        )]

        # Increment
        i <- i + 1

        # Collect model filename
        EvalList[[i]] <- data_test_TSLM
      }, error = function(x) "skip")
    }

    # Model-Supplied-Freq
    if (tolower(class(TSLM_model1)[1]) == "tslm") {
      tryCatch({
        # Collect Test Data for Model Comparison
        # 2)
        data_test_TSLM1 <- data.table::copy(data_test)
        data_test_TSLM1[, ':=' (
          Target = as.numeric(Target),
          ModelName = rep("TSLM_ModelFreq", HoldOutPeriods),
          FC_Eval = as.numeric(forecast::forecast(TSLM_model1,
                                                  h = HoldOutPeriods)$mean)
        )]

        # Add Evaluation Columns
        # 3)
        data_test_TSLM1[, ':=' (
          Resid = get(TargetName) - FC_Eval,
          PercentError = get(TargetName) / (FC_Eval +
                                              1) - 1,
          AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                          1) - 1)
        )]

        # Increment
        i <- i + 1

        # Collect model filename
        EvalList[[i]] <- data_test_TSLM1
      }, error = function(x) "skip")
    }

    # TSClean Version
    if(TSClean) {
      # User-Supplied-Freq
      if (tolower(class(TSLM_model2)[1]) == "tslm") {
        tryCatch({
          # Collect Test Data for Model Comparison
          # 2)
          data_test_TSLM2 <- data.table::copy(data_test)
          data_test_TSLM2[, ':=' (
            Target = as.numeric(Target),
            ModelName = rep("TSLM_TSC", HoldOutPeriods),
            FC_Eval = as.numeric(forecast::forecast(TSLM_model2,
                                                    h = HoldOutPeriods)$mean)
          )]

          # Add Evaluation Columns
          # 3)
          data_test_TSLM2[, ':=' (
            Resid = get(TargetName) - FC_Eval,
            PercentError = get(TargetName) / (FC_Eval +
                                                1) - 1,
            AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                            1) - 1)
          )]

          # Increment
          i <- i + 1

          # Collect model filename
          EvalList[[i]] <- data_test_TSLM2
        }, error = function(x) "skip")
      }

      # Model-Supplied-Freq
      if (tolower(class(TSLM_model3)[1]) == "tslm") {
        tryCatch({
          # Collect Test Data for Model Comparison
          # 2)
          data_test_TSLM3 <- data.table::copy(data_test)
          data_test_TSLM3[, ':=' (
            Target = as.numeric(Target),
            ModelName = rep("TSLM_ModelFreqTSC", HoldOutPeriods),
            FC_Eval = as.numeric(forecast::forecast(TSLM_model3,
                                                    h = HoldOutPeriods)$mean)
          )]

          # Add Evaluation Columns
          # 3)
          data_test_TSLM3[, ':=' (
            Resid = get(TargetName) - FC_Eval,
            PercentError = get(TargetName) / (FC_Eval +
                                                1) - 1,
            AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                            1) - 1)
          )]

          # Increment
          i <- i + 1

          # Collect model filename
          EvalList[[i]] <- data_test_TSLM3
        }, error = function(x) "skip")
      }
    }
  }

  if (!("NNET" %in% toupper(SkipModels))) {
    # Neural Network-------------
    # 1)
    if(PrintUpdates) message("NNet FITTING")
    k <- 0L
    temp <-
      data.table::data.table(
        Lag = rep(1L, Lags * SLags),
        Slag = rep(1L, Lags * SLags),
        meanResid = rnorm(Lags * SLags),
        sdResid = rnorm(Lags * SLags)
      )
    for (lags in seq_len(Lags)) {
      for (slags in seq_len(SLags)) {
        k <- k + 1L
        if(PrintUpdates) print(paste0("NNet Iteration: ",k))
        NNETAR_model_temp <-
          tryCatch({
            forecast::nnetar(
              y = dataTSTrain[, TargetName],
              p = lags,
              P = slags,
              lambda = "auto"
            )
          }, error = function(x)
            "error")

        if (length(NNETAR_model_temp) == 1) {
          data.table::set(temp,
                          i = k,
                          j = 1L,
                          value = lags)
          data.table::set(temp,
                          i = k,
                          j = 2L,
                          value = slags)
          data.table::set(temp,
                          i = k,
                          j = 3L,
                          value = 999999999)
          data.table::set(temp,
                          i = k,
                          j = 4L,
                          value = 999999999)

        } else {
          data.table::set(temp,
                          i = k,
                          j = 1L,
                          value = lags)
          data.table::set(temp,
                          i = k,
                          j = 2L,
                          value = slags)
          data.table::set(
            temp,
            i = k,
            j = 3L,
            value = base::mean(abs(NNETAR_model_temp$residuals),
                               na.rm = TRUE)
          )
          data.table::set(
            temp,
            i = k,
            j = 4L,
            value = sd(NNETAR_model_temp$residuals,
                       na.rm = TRUE)
          )
        }
      }
    }

    # Identify best model and retrain it
    LagNN <- temp[order(meanResid)][1, ][, 1][[1]]
    SLagNN <- temp[order(meanResid)][1, ][, 2][[1]]
    NNETAR_model <-
      tryCatch({
        forecast::nnetar(
          y = dataTSTrain[, TargetName],
          p = LagNN,
          P = SLagNN,
          lambda = "auto"
        )
      },
      error = function(x)
        "empty")

    # Collect Test Data for Model Comparison
    # 2)
    if (tolower(class(NNETAR_model)) == "nnetar") {
      tryCatch({
        data_test_NN <- data.table::copy(data_test)
        data_test_NN[, ':=' (
          Target = as.numeric(Target),
          ModelName = rep("NN", HoldOutPeriods),
          FC_Eval = as.numeric(
            forecast::forecast(NNETAR_model, h = HoldOutPeriods)$mean
          )
        )]

        # Add Evaluation Columns
        # 3)
        data_test_NN[, ':=' (
          Resid = get(TargetName) - FC_Eval,
          PercentError = get(TargetName) / (FC_Eval + 1) - 1,
          AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                          1) - 1)
        )]

        # Increment
        i <- i + 1

        # Collect model filename
        EvalList[[i]] <- data_test_NN
      }, error = function(x) "skip")
    }

    k <- 0L
    temp <-
      data.table::data.table(
        Lag = rep(1L, Lags * SLags),
        Slag = rep(1L, Lags * SLags),
        meanResid = rnorm(Lags * SLags),
        sdResid = rnorm(Lags * SLags)
      )
    for (lags in seq_len(Lags)) {
      for (slags in seq_len(SLags)) {
        k <- k + 1L
        if(PrintUpdates) print(paste0("NNet 2 Iteration: ",k))
        NNETAR_model_temp <-
          tryCatch({
            forecast::nnetar(
              y = dataTSTrain1[, TargetName],
              p = lags,
              P = slags,
              lambda = "auto"
            )
          }, error = function(x)
            "error")

        if (length(NNETAR_model_temp) == 1) {
          data.table::set(temp,
                          i = k,
                          j = 1L,
                          value = lags)
          data.table::set(temp,
                          i = k,
                          j = 2L,
                          value = slags)
          data.table::set(temp,
                          i = k,
                          j = 3L,
                          value = 999999999)
          data.table::set(temp,
                          i = k,
                          j = 4L,
                          value = 999999999)

        } else {
          data.table::set(temp,
                          i = k,
                          j = 1L,
                          value = lags)
          data.table::set(temp,
                          i = k,
                          j = 2L,
                          value = slags)
          data.table::set(
            temp,
            i = k,
            j = 3L,
            value = base::mean(abs(NNETAR_model_temp$residuals),
                               na.rm = TRUE)
          )
          data.table::set(
            temp,
            i = k,
            j = 4L,
            value = sd(NNETAR_model_temp$residuals,
                       na.rm = TRUE)
          )
        }
      }
    }

    # Identify best model and retrain it
    LagNN <- temp[order(meanResid)][1, ][, 1][[1]]
    SLagNN <- temp[order(meanResid)][1, ][, 2][[1]]
    NNETAR_model1 <-
      tryCatch({
        forecast::nnetar(
          y = dataTSTrain1[, TargetName],
          p = LagNN,
          P = SLagNN,
          lambda = "auto"
        )
      },
      error = function(x)
        "empty")

    # Collect Test Data for Model Comparison
    # 2)
    if (tolower(class(NNETAR_model1)) == "nnetar") {
      tryCatch({
        data_test_NN1 <- data.table::copy(data_test)
        data_test_NN1[, ':=' (
          Target = as.numeric(Target),
          ModelName = rep("NN_ModelFreq", HoldOutPeriods),
          FC_Eval = as.numeric(
            forecast::forecast(NNETAR_model1, h = HoldOutPeriods)$mean
          )
        )]

        # Add Evaluation Columns
        # 3)
        data_test_NN1[, ':=' (
          Resid = get(TargetName) - FC_Eval,
          PercentError = get(TargetName) / (FC_Eval + 1) - 1,
          AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                          1) - 1)
        )]

        # Increment
        i <- i + 1

        # Collect model filename
        EvalList[[i]] <- data_test_NN1
      }, error = function(x) "skip")
    }

    # TSClean Version
    if(TSClean) {
      k <- 0L
      temp <-
        data.table::data.table(
          Lag = rep(1L, Lags * SLags),
          Slag = rep(1L, Lags * SLags),
          meanResid = rnorm(Lags * SLags),
          sdResid = rnorm(Lags * SLags)
        )
      for (lags in seq_len(Lags)) {
        for (slags in seq_len(SLags)) {
          k <- k + 1L
          if(PrintUpdates) print(paste0("NNet 3 Iteration: ",k))
          NNETAR_model_temp <-
            tryCatch({
              forecast::nnetar(
                y = Target,
                p = lags,
                P = slags,
                lambda = "auto"
              )
            }, error = function(x)
              "error")

          if (length(NNETAR_model_temp) == 1) {
            data.table::set(temp,
                            i = k,
                            j = 1L,
                            value = lags)
            data.table::set(temp,
                            i = k,
                            j = 2L,
                            value = slags)
            data.table::set(temp,
                            i = k,
                            j = 3L,
                            value = 999999999)
            data.table::set(temp,
                            i = k,
                            j = 4L,
                            value = 999999999)

          } else {
            data.table::set(temp,
                            i = k,
                            j = 1L,
                            value = lags)
            data.table::set(temp,
                            i = k,
                            j = 2L,
                            value = slags)
            data.table::set(
              temp,
              i = k,
              j = 3L,
              value = base::mean(abs(NNETAR_model_temp$residuals),
                                 na.rm = TRUE)
            )
            data.table::set(
              temp,
              i = k,
              j = 4L,
              value = sd(NNETAR_model_temp$residuals,
                         na.rm = TRUE)
            )
          }
        }
      }

      # Identify best model and retrain it
      LagNN <- temp[order(meanResid)][1, ][, 1][[1]]
      SLagNN <- temp[order(meanResid)][1, ][, 2][[1]]
      NNETAR_model2 <-
        tryCatch({
          forecast::nnetar(
            y = Target,
            p = LagNN,
            P = SLagNN,
            lambda = "auto"
          )
        },
        error = function(x)
          "empty")

      # Collect Test Data for Model Comparison
      # 2)
      if (tolower(class(NNETAR_model2)) == "nnetar") {
        tryCatch({
          data_test_NN2 <- data.table::copy(data_test)
          data_test_NN2[, ':=' (
            Target = as.numeric(Target),
            ModelName = rep("NN_TSC", HoldOutPeriods),
            FC_Eval = as.numeric(
              forecast::forecast(NNETAR_model2, h = HoldOutPeriods)$mean
            )
          )]

          # Add Evaluation Columns
          # 3)
          data_test_NN2[, ':=' (
            Resid = get(TargetName) - FC_Eval,
            PercentError = get(TargetName) / (FC_Eval + 1) - 1,
            AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                            1) - 1)
          )]

          # Increment
          i <- i + 1

          # Collect model filename
          EvalList[[i]] <- data_test_NN2
        }, error = function(x) "skip")
      }

      k <- 0L
      temp <-
        data.table::data.table(
          Lag = rep(1L, Lags * SLags),
          Slag = rep(1L, Lags * SLags),
          meanResid = rnorm(Lags * SLags),
          sdResid = rnorm(Lags * SLags)
        )
      for (lags in seq_len(Lags)) {
        for (slags in seq_len(SLags)) {
          k <- k + 1L
          if(PrintUpdates) print(paste0("NNet 4 Iteration: ",k))
          NNETAR_model_temp <-
            tryCatch({
              forecast::nnetar(
                y = TargetMB,
                p = lags,
                P = slags,
                lambda = "auto"
              )
            }, error = function(x)
              "error")

          if (length(NNETAR_model_temp) == 1) {
            data.table::set(temp,
                            i = k,
                            j = 1L,
                            value = lags)
            data.table::set(temp,
                            i = k,
                            j = 2L,
                            value = slags)
            data.table::set(temp,
                            i = k,
                            j = 3L,
                            value = 999999999)
            data.table::set(temp,
                            i = k,
                            j = 4L,
                            value = 999999999)

          } else {
            data.table::set(temp,
                            i = k,
                            j = 1L,
                            value = lags)
            data.table::set(temp,
                            i = k,
                            j = 2L,
                            value = slags)
            data.table::set(
              temp,
              i = k,
              j = 3L,
              value = base::mean(abs(NNETAR_model_temp$residuals),
                                 na.rm = TRUE)
            )
            data.table::set(
              temp,
              i = k,
              j = 4L,
              value = sd(NNETAR_model_temp$residuals,
                         na.rm = TRUE)
            )
          }
        }
      }

      # Identify best model and retrain it
      LagNN <- temp[order(meanResid)][1, ][, 1][[1]]
      SLagNN <- temp[order(meanResid)][1, ][, 2][[1]]
      NNETAR_model3 <-
        tryCatch({
          forecast::nnetar(
            y = dataTSTrain1[, TargetName],
            p = LagNN,
            P = SLagNN,
            lambda = "auto"
          )
        },
        error = function(x)
          "empty")

      # Collect Test Data for Model Comparison
      # 2)
      if (tolower(class(NNETAR_model3)) == "nnetar") {
        tryCatch({
          data_test_NN3 <- data.table::copy(data_test)
          data_test_NN3[, ':=' (
            Target = as.numeric(Target),
            ModelName = rep("NN_ModelFreqTSC", HoldOutPeriods),
            FC_Eval = as.numeric(
              forecast::forecast(NNETAR_model3, h = HoldOutPeriods)$mean
            )
          )]

          # Add Evaluation Columns
          # 3)
          data_test_NN3[, ':=' (
            Resid = get(TargetName) - FC_Eval,
            PercentError = get(TargetName) / (FC_Eval + 1) - 1,
            AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                            1) - 1)
          )]

          # Increment
          i <- i + 1

          # Collect model filename
          EvalList[[i]] <- data_test_NN3
        }, error = function(x) "skip")
      }
    }
  }

  # Model Collection----
  if(PrintUpdates) message("FIND WINNER")
  dataEval <- data.table::rbindlist(EvalList)

  # Model Evaluation
  Eval <- dataEval[, .(
    MeanResid = round(base::mean(Resid, na.rm = TRUE),2),
    MeanPercError = round(base::mean(PercentError, na.rm = TRUE),5),
    MAPE = round(base::mean(AbsolutePercentError, na.rm = TRUE),5)
  ),
  by = "ModelName"][order(MAPE)][, ID := 1:.N]

  # Grab Winning Model
  BestModel <- Eval[1, "ModelName"][[1]]

  # Generate Forecasts----
  if(PrintUpdates) message("GENERATE FORECASTS")

  # Create Training data
  data_train <- data[seq_len(nrow(data))]

  # Create Full Training Data for Final Rebruild
  if(grepl("ModelFreq", BestModel)) {
    if(grepl("TSC", BestModel)) {
      if(MinVal > 0) {
        # Model-Supplied-Freq
        SFreq <- forecast::findfrequency(as.matrix(data_train[,2]))
        dataTSTrain <-
          stats::ts(data = data_train,
                    start = data_train[, min(get(DateName))][[1]],
                    frequency = SFreq)

        # TSClean Version
        dataTSTrain <- forecast::tsclean(x = dataTSTrain[, TargetName],
                                         replace.missing = TRUE,
                                         lambda = "auto")
      } else {
        # Model-Supplied-Freq
        SFreq <- forecast::findfrequency(as.matrix(data_train[,2]))
        dataTSTrain <-
          stats::ts(data = data_train,
                    start = data_train[, min(get(DateName))][[1]],
                    frequency = SFreq)

        # TSClean Version
        dataTSTrain <- forecast::tsclean(x = dataTSTrain[, TargetName],
                                         replace.missing = TRUE,
                                         lambda = NULL)
      }

    } else {
      # Model-Supplied-Freq
      SFreq <- forecast::findfrequency(as.matrix(data_train[,2]))
      dataTSTrain <-
        stats::ts(data = data_train,
                  start = data_train[, min(get(DateName))][[1]],
                  frequency = SFreq)

      # Only Target as Numeric Vector
      dataTSTrain <- dataTSTrain[, TargetName]
    }
  } else {
    if(grepl("TSC", BestModel)) {
      if(MinVal > 0) {
        # User-Supplied-Freq
        dataTSTrain <-
          stats::ts(data = data_train,
                    start = data_train[, min(get(DateName))][[1]],
                    frequency = freq)

        # TSClean Version
        dataTSTrain <- forecast::tsclean(x = dataTSTrain[, TargetName],
                                         replace.missing = TRUE,
                                         lambda = "auto")
      } else {
        # User-Supplied-Freq
        dataTSTrain <-
          stats::ts(data = data_train,
                    start = data_train[, min(get(DateName))][[1]],
                    frequency = freq)

        # TSClean Version
        dataTSTrain <- forecast::tsclean(x = dataTSTrain[, TargetName],
                                         replace.missing = TRUE,
                                         lambda = NULL)
      }
    } else {
      # User-Supplied-Freq
      dataTSTrain <-
        stats::ts(data = data_train,
                  start = data_train[, min(get(DateName))][[1]],
                  frequency = freq)

      # Only Target as Numeric Vector
      dataTSTrain <- dataTSTrain[, TargetName]
    }
  }

  # Retrain best model
  if (grepl(pattern = "DSHW",BestModel)) {
    if(PrintUpdates) message("FULL DATA DSHW FITTING")
    if(BestModel == "DSHW_ModelFreq") {
      freq <- SFreq
    }
    if(MinVal > 0) {
      DSHW_Model <-
        tryCatch({forecast::dshw(
          y = dataTSTrain,
          period1 = freq,
          period2 = freq*2,
          alpha = NULL,
          beta = NULL,
          gamma = NULL,
          omega = NULL,
          phi = NULL,
          lambda = "auto",
          biasadj = TRUE,
          armethod = TRUE,
          model = NULL
        )
        },
        error = function(x)
          "empty")
    } else {
      DSHW_Model <-
        tryCatch({forecast::dshw(
          y = dataTSTrain,
          period1 = freq,
          period2 = freq*2,
          alpha = NULL,
          beta = NULL,
          gamma = NULL,
          omega = NULL,
          phi = NULL,
          lambda = NULL,
          biasadj = FALSE,
          armethod = TRUE,
          model = NULL
        )
        },
        error = function(x)
          "empty")
    }

    # Forecast with new model
    FC_Data[, paste0("Forecast_", BestModel) := as.numeric(
      forecast::forecast(DSHW_Model,
                         h = FCPeriods)$mean)]

    # Store model
    model <- DSHW_Model

  } else if (grepl(pattern = "ARFIMA",BestModel)) {
    if(PrintUpdates) message("FULL DATA ARFIMA FITTING")
    # Rebuild model on full data
    if (StepWise) {
      if(MinVal > 0) {
        ARFIMA_model <- forecast::arfima(
          y = dataTSTrain,
          lambda = TRUE,
          biasadj = TRUE,
          max.p = Lags,
          max.q = Lags,
          max.d = 1,
          max.D = 1,
          ic = "bic",
          stepwise = StepWise,
          num.cores = NumCores
        )
      } else {
        ARFIMA_model <- forecast::arfima(
          y = dataTSTrain,
          lambda = FALSE,
          biasadj = FALSE,
          max.p = Lags,
          max.q = Lags,
          max.d = 1,
          max.D = 1,
          ic = "bic",
          stepwise = StepWise,
          num.cores = NumCores
        )
      }
    } else {
      if(MinVal > 0) {
        ARFIMA_model <- forecast::arfima(
          y = dataTSTrain,
          lambda = TRUE,
          biasadj = TRUE,
          max.p = Lags,
          max.q = Lags,
          max.d = 1,
          max.D = 1,
          ic = "bic",
          stepwise = StepWise,
          parallel = TRUE,
          num.cores = NumCores
        )
      } else {
        ARFIMA_model <- forecast::arfima(
          y = dataTSTrain,
          lambda = FALSE,
          biasadj = FALSE,
          max.p = Lags,
          max.q = Lags,
          max.d = 1,
          max.D = 1,
          ic = "bic",
          stepwise = StepWise,
          parallel = TRUE,
          num.cores = NumCores
        )
      }
    }

    # Forecast with new model
    FC_Data[, paste0("Forecast_", BestModel) := as.numeric(
      forecast::forecast(ARFIMA_model,
                         h = FCPeriods)$mean)]

    # Store model
    model <- ARFIMA_model

  } else if (grepl(pattern = "ARIMA",BestModel)) {
    if(PrintUpdates) message("FULL DATA ARIMA FITTING")
    # Rebuild model on full data
    if (StepWise) {
      if(MinVal > 0) {
        ARIMA_model <-
          forecast::auto.arima(
            y     = dataTSTrain,
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
            num.cores = NumCores
          )
      } else {
        ARIMA_model <-
          forecast::auto.arima(
            y     = dataTSTrain,
            max.p = Lags,
            max.q = Lags,
            max.P = SLags,
            max.Q = SLags,
            max.d = 1,
            max.D = 1,
            ic = "bic",
            lambda = FALSE,
            biasadj = FALSE,
            stepwise = StepWise,
            num.cores = NumCores
          )
      }
    } else {
      if(MinVal > 0) {
        ARIMA_model <-
          forecast::auto.arima(
            y     = dataTSTrain,
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
            num.cores = NumCores
          )
      } else {
        ARIMA_model <-
          forecast::auto.arima(
            y     = dataTSTrain,
            max.p = Lags,
            max.q = Lags,
            max.P = SLags,
            max.Q = SLags,
            max.d = 1,
            max.D = 1,
            ic = "bic",
            lambda = FALSE,
            biasadj = FALSE,
            stepwise = StepWise,
            parallel = TRUE,
            num.cores = NumCores
          )
      }
    }

    # Forecast with new model
    FC_Data[, paste0("Forecast_", BestModel) := as.numeric(
      forecast::forecast(ARIMA_model,
                         h = FCPeriods)$mean)]

    # Store model
    model <- ARIMA_model

  } else if (grepl(pattern = "ETS",BestModel)) {
    if(PrintUpdates) message("FULL DATA ETS FITTING")
    # Rebuild model on full data
    if (freq > 24) {
      if(MinVal > 0) {
        # when > 24, model's third letter has to be N for none
        EXPSMOOTH_model <-
          forecast::ets(
            y                          = dataTSTrain,
            model                      = "ZZN",
            allow.multiplicative.trend = TRUE,
            restrict                   = TRUE,
            lambda                     = TRUE,
            biasadj                    = TRUE
          )
      } else {
        # when > 24, model's third letter has to be N for none
        EXPSMOOTH_model <-
          forecast::ets(
            y                          = dataTSTrain,
            model                      = "ZZN",
            allow.multiplicative.trend = TRUE,
            restrict                   = TRUE,
            lambda                     = FALSE,
            biasadj                    = FALSE
          )
      }
    } else {
      if(MinVal > 0) {
        EXPSMOOTH_model <-
          forecast::ets(
            y                          = dataTSTrain,
            model                      = "ZZZ",
            allow.multiplicative.trend = TRUE,
            restrict                   = TRUE,
            lambda                     = TRUE,
            biasadj                    = TRUE
          )
      } else {
        EXPSMOOTH_model <-
          forecast::ets(
            y                          = dataTSTrain,
            model                      = "ZZZ",
            allow.multiplicative.trend = TRUE,
            restrict                   = TRUE,
            lambda                     = FALSE,
            biasadj                    = FALSE
          )
      }
    }

    # Forecast with new model
    FC_Data[, paste0("Forecast_", BestModel) := as.numeric(
      forecast::forecast(EXPSMOOTH_model,
                         h = FCPeriods)$mean)]

    # Store model
    model <- EXPSMOOTH_model

  } else if (grepl(pattern = "TBATS",BestModel)) {
    if(PrintUpdates) message("FULL DATA TBATS FITTING")
    if(MinVal > 0) {
      # Rebuild model on full data
      TBATS_model <- forecast::tbats(
        y = dataTSTrain,
        use.arma.errors = TRUE,
        lambda = TRUE,
        biasadj = TRUE,
        max.p = Lags,
        max.q = Lags,
        max.P = SLags,
        max.Q = SLags,
        max.d = 1,
        max.D = 1,
        num.cores = NumCores
      )
    } else {
      # Rebuild model on full data
      TBATS_model <- forecast::tbats(
        y = dataTSTrain,
        use.arma.errors = TRUE,
        lambda = FALSE,
        biasadj = FALSE,
        max.p = Lags,
        max.q = Lags,
        max.P = SLags,
        max.Q = SLags,
        max.d = 1,
        max.D = 1,
        num.cores = NumCores
      )
    }

    # Forecast with new model
    FC_Data[, paste0("Forecast_", BestModel) := as.numeric(
      forecast::forecast(TBATS_model,
                         h = FCPeriods)$mean)]

    # Store model
    model <- TBATS_model

  } else if (grepl(pattern = "TSLM",BestModel)) {
    if(PrintUpdates) message("FULL DATA TSLM FITTING")
    if(MinVal > 0) {
      # Rebuild model on full data
      TSLM_model <-
        forecast::tslm(dataTSTrain ~ trend + season,
                       lambda = TRUE,
                       biasadj = TRUE)
    } else {
      # Rebuild model on full data
      TSLM_model <-
        forecast::tslm(dataTSTrain ~ trend + season,
                       lambda = FALSE,
                       biasadj = FALSE)
    }

    # Forecast with new model
    FC_Data[, paste0("Forecast_", BestModel) := as.numeric(
      forecast::forecast(TSLM_model,h = FCPeriods)$mean)]

    # Store model
    model <- TSLM_model

  } else if (grepl(pattern = "NN",BestModel)) {
    if(PrintUpdates) message("FULL DATA NN FITTING")
    # Rebuild model on full data
    k <- 0L
    temp <-
      data.table::data.table(
        Lag = rep(1L, Lags * SLags),
        Slag = rep(1L, Lags * SLags),
        meanResid = rnorm(Lags * SLags),
        sdResid = rnorm(Lags * SLags)
      )
    for (lags in seq_len(Lags)) {
      for (slags in seq_len(SLags)) {
        k <- k + 1L
        if(PrintUpdates) print(k)
        NNETAR_model_temp <-
          tryCatch({
            forecast::nnetar(
              y = dataTSTrain,
              p = lags,
              P = slags,
              lambda = "auto"
            )
          }, error = function(x)
            "error")

        if (length(NNETAR_model_temp) == 1) {
          data.table::set(temp,
                          i = k,
                          j = 1L,
                          value = lags)
          data.table::set(temp,
                          i = k,
                          j = 2L,
                          value = slags)
          data.table::set(temp,
                          i = k,
                          j = 3L,
                          value = 999999999)
          data.table::set(temp,
                          i = k,
                          j = 4L,
                          value = 999999999)

        } else {
          data.table::set(temp,
                          i = k,
                          j = 1L,
                          value = lags)
          data.table::set(temp,
                          i = k,
                          j = 2L,
                          value = slags)
          data.table::set(
            temp,
            i = k,
            j = 3L,
            value = base::mean(abs(NNETAR_model_temp$residuals),
                               na.rm = TRUE)
          )
          data.table::set(
            temp,
            i = k,
            j = 4L,
            value = sd(NNETAR_model_temp$residuals,
                       na.rm = TRUE)
          )
        }
      }
    }

    # Identify best model and retrain it
    LagNN <- temp[order(meanResid)][1, ][, 1][[1]]
    SLagNN <- temp[order(meanResid)][1, ][, 2][[1]]
    NNETAR_model <-
      tryCatch({
        forecast::nnetar(y = dataTSTrain,
                         p = LagNN,
                         P = SLagNN)
      },
      error = function(x)
        "empty")

    # Forecast with new model
    FC_Data[, paste0("Forecast_", BestModel) := as.numeric(
      forecast::forecast(NNETAR_model, h = FCPeriods)$mean)]

    # Store model
    model <- NNETAR_model

  }

  # Create plot
  temp <- data.table::copy(FC_Data)
  data.table::setnames(data, c(eval(DateName)), "Date")
  Time <- data.table::rbindlist(list(data[, "Date"], temp[, "Date"]))
  z <- data.table::rbindlist(list(data[, Date := NULL], temp[, Date := NULL]), fill = TRUE)
  z <- cbind(Time, z)
  z[, eval(TargetName) := as.numeric(get(TargetName))]
  print(ggplot2::ggplot(z, ggplot2::aes(x = z[["Date"]])) +
          ggplot2::geom_line(ggplot2::aes(y = z[[eval(TargetName)]]), color = "#005B80") +
          ggplot2::geom_line(ggplot2::aes(y = z[[3]]), color = "#1c1c1c") +
          ggplot2::geom_vline(xintercept = max(data_test[[eval(DateName)]],
                                               na.rm = TRUE),
                              color = "red",
                              lty = "dotted",
                              lwd = 1) +
          RemixTheme() +
          ggplot2::labs(title = paste0(FCPeriods,"-", TimeUnit, " Forecast for ", TargetName),
                        subtitle = paste0("Champion Model: ", BestModel, " | Mean Absolute Percentage Error: ",
                                          paste(round(min(Eval$MAPE),3) * 100, "%", sep = "")),
                        caption = "Forecast generated by Remix Institute's RemixAutoML R package") +
          ggplot2::xlab(eval(DateName)) + ggplot2::ylab(eval(TargetName)))
  options(warn = 0)

  # Return values
  return(list(Forecast = FC_Data,
              EvaluationMetrics = Eval,
              TimeSeriesModel = model,
              ChampionModel = BestModel))
}

#' tempDatesFun Convert Excel datetime char columns to Date columns
#'
#' tempDatesFun takes the Excel datetime column, which imports as character, and converts it into a date type
#'
#' @author Adrian Antico
#' @family Misc
#' @param x The column of interest
#' @examples
#' Cdata <- data.table::data.table(DAY_DATE = "2018-01-01 8:53")
#' Cdata[, DAY_DATE := tempDatesFun(DAY_DATE)]
#' @return An object to pass along to ggplot objects following the "+" sign
#' @export
tempDatesFun <- base::Vectorize(function(x) {
  # Ensure data.table is available
  requireNamespace('data.table', quietly = FALSE)
  data.table::tstrsplit(x, " ")[[1]][1]
})

#' SimpleCap function is for capitalizing the first letter of words
#'
#' SimpleCap function is for capitalizing the first letter of words (need I say more?)
#'
#' @author Adrian Antico
#' @family Misc
#' @param x Column of interest
#' @examples
#' x <- "adrian"
#' x <- SimpleCap(x)
#' @return An object to pass along to ggplot objects following the "+" sign
#' @export
SimpleCap <- function(x) {
  # Ensure data.table is available
  requireNamespace('data.table', quietly = FALSE)
  s <- data.table::tstrsplit(x, " ")[[1]]
  paste(
    base::toupper(base::substring(s, 1, 1)),
    base::substring(s, 2),
    sep = "",
    collapse = " "
  )
}

#' RemixTheme function is a ggplot theme generator for ggplots
#'
#' This function adds the Remix Theme to ggplots
#'
#' @author Douglas Pestana
#' @family Misc
#' @examples
#' data <- data.table::data.table(DateTime = as.Date(Sys.time()),
#'   Target = stats::filter(rnorm(1000,
#'                                mean = 50,
#'                                sd = 20),
#'                          filter=rep(1,10),
#'                          circular=TRUE))
#' data[, temp := seq(1:1000)][, DateTime := DateTime - temp][, temp := NULL]
#' data <- data[order(DateTime)]
#' p <- ggplot2::ggplot(data, ggplot2::aes(x = DateTime, y = Target)) + ggplot2::geom_line()
#' p <- p + RemixTheme()
#' @return An object to pass along to ggplot objects following the "+" sign
#' @export
RemixTheme <- function() {
  ggplot2::theme(axis.title = ggplot2::element_text(size = 11),
        axis.text = ggplot2::element_text(size = 11),
        legend.background = ggplot2::element_blank(),
        legend.key = ggplot2::element_blank(),
        legend.text = ggplot2::element_text(color = "#1c1c1c",
                                   size = 11),
        legend.title = ggplot2::element_blank(),
        legend.justification = 0,
        legend.position = "top",
        plot.background = ggplot2::element_rect(fill = "#E7E7E7"),
        panel.background = ggplot2::element_rect(fill= "#E7E7E7"),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor.x = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_line(color = "white"),
        panel.grid.minor.y = ggplot2::element_line(color = "white"),
        plot.title = ggplot2::element_text(color = "#1c1c1c",
                                  size = 28,
                                  hjust = 0,
                                  face = "bold"),
        plot.subtitle = ggplot2::element_text(color = "#1c1c1c",
                                     size = 16,
                                     hjust = 0),
        plot.caption = ggplot2::element_text(size = 9,
                                    hjust = 0,
                                    face = "italic") )
}

#' Final Data Preparation Function
#'
#' This function replaces inf values with NA, converts characters to factors, and imputes with constants
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @param Impute Defaults to TRUE which tells the function to impute the data
#' @param CharToFactor Defaults to TRUE which tells the function to convert characters to factors
#' @param data This is your source data you'd like to modify
#' @param MissFactor Supply the value to impute missing factor levels
#' @param MissNum Supply  the value to impute missing numeric values
#' @examples
#' data <- data.table::data.table(Value = runif(100000),
#'                                FactorCol = as.character(sample(x = c(letters,
#'                                                                      LETTERS,
#'                                                                      paste0(letters,letters),
#'                                                                      paste0(LETTERS,LETTERS),
#'                                                                      paste0(letters,LETTERS),
#'                                                                      paste0(LETTERS,letters)),
#'                                                                size = 100000,
#'                                                                replace = TRUE)))
#' data <- ModelDataPrep(data,
#'                       Impute = TRUE,
#'                       CharToFactor = TRUE,
#'                       MissFactor = "0",
#'                       MissNum    = -1)
#' @return Returns the original data table with corrected values
#' @export
ModelDataPrep <- function(data,
                          Impute       = TRUE,
                          CharToFactor = TRUE,
                          MissFactor   = "0",
                          MissNum      = -1) {

  # Ensure data.table is available
  requireNamespace('data.table', quietly = FALSE)

  # Check data.table
  if (!data.table::is.data.table(data))
    data <- data.table::as.data.table(data)

  # Replace any inf values with NA
  for (col in base::seq_along(data)) {
    data.table::set(data,
                    j = col,
                    value = base::replace(data[[col]],
                                          base::is.infinite(data[[col]]), NA))
  }

  # Turn character columns into factors
  if (CharToFactor) {
    for (col in base::seq_along(data)) {
      if (is.character(data[[col]])) {
        data.table::set(data,
                        j = col,
                        value = base::as.factor(data[[col]]))
      }
    }
  }

  # Impute missing values
  if (Impute) {
    for (j in base::seq_along(data)) {
      if (base::is.factor(data[[j]])) {
        data.table::set(data,
                        base::which(!(data[[j]] %in% base::levels(data[[j]]))),
                        j,
                        MissFactor)
      } else {
        data.table::set(data,
                        base::which(base::is.na(data[[j]])),
                        j,
                        MissNum)
      }
    }
  }
  base::return(data)
}

#' RedYellowGreen is for determining the optimal thresholds for binary classification when do-nothing is an option
#'
#' This function will find the optimial thresholds for applying the main label and for finding the optimial range for doing nothing when you can quantity the cost of doing nothing
#'
#' @author Adrian Antico
#' @family Model Evaluation and Interpretation
#' @param data data is the data table with your predicted and actual values from a classification model
#' @param PredictColNumber The column number where the prediction variable is located (in binary form)
#' @param ActualColNumber The column number where the target variable is located
#' @param TruePositiveCost This is the utility for generating a true positive prediction
#' @param TrueNegativeCost This is the utility for generating a true negative prediction
#' @param FalsePositiveCost This is the cost of generating a false positive prediction
#' @param FalseNegativeCost This is the cost of generating a false negative prediction
#' @param MidTierCost This is the cost of doing nothing (or whatever it means to not classify in your case)
#' @param Cores Number of cores on your machine
#' @param Precision Set the decimal number to increment by between 0 and 1
#' @import foreach
#' @examples
#' data <- data.table::data.table(Target = runif(10))
#' data[, x1 := qnorm(Target)]
#' data[, x2 := runif(10)]
#' data[, Predict := log(pnorm(0.85 * x1 +
#'                               sqrt(1-0.85^2) * qnorm(x2)))]
#' data[, ':=' (x1 = NULL, x2 = NULL)]
#' data <- RedYellowGreen(data,
#'                        PredictColNumber  = 2,
#'                        ActualColNumber   = 1,
#'                        TruePositiveCost  = 0,
#'                        TrueNegativeCost  = 0,
#'                        FalsePositiveCost = -1,
#'                        FalseNegativeCost = -2,
#'                        MidTierCost = -0.5,
#'                        Precision = 0.5,
#'                        Cores = 1)
#' @return A data table with all evaluated strategies, parameters, and utilities, along with a 3d scatterplot of the results
#' @export
RedYellowGreen <- function(data,
                           PredictColNumber  = 2,
                           ActualColNumber   = 1,
                           TruePositiveCost  = 0,
                           TrueNegativeCost  = 0,
                           FalsePositiveCost = -10,
                           FalseNegativeCost = -50,
                           MidTierCost       = -2,
                           Cores             = 8,
                           Precision         = 0.01) {

  # Ensure packages are available
  requireNamespace('data.table', quietly = FALSE)
  requireNamespace('foreach', quietly = FALSE)
  requireNamespace('doParallel', quietly = FALSE)
  requireNamespace('parallel', quietly = FALSE)

  # Check data.table
  if (!data.table::is.data.table(data))
    data <- data.table::as.data.table(data)

  # Set up evaluation table
  analysisTable <- data.table::data.table(
    TPP = base::rep(TruePositiveCost, 1),
    TNP = base::rep(TrueNegativeCost, 1),
    FPP = base::rep(FalsePositiveCost, 1),
    FNP = base::rep(FalseNegativeCost, 1),
    MTDN = base::rep(TRUE, 1),
    MTC = base::rep(MidTierCost, 1),
    Threshold = runif(1)
  )

  # Do nothing possibilities
  temp     <-
    data.table::CJ(MTLT = seq(0.0, 1.0, Precision),
                   MTHT = seq(0.0, 1.0, Precision))[MTHT > MTLT]
  new      <- cbind(analysisTable, temp)
  new[, Utility := stats::runif(nrow(new))]

  # Parallel components
  requireNamespace(c("parallel", "doParallel", "foreach"))
  packages <- c("data.table")
  cores    <- Cores
  bat      <- base::ceiling(nrow(new) / cores)
  parts    <- base::floor(nrow(new) / bat)
  cl       <- parallel::makePSOCKcluster(cores)
  doParallel::registerDoParallel(cl)

  # Kick off run
  results <-
    foreach::foreach(
      i            = itertools::isplitRows(new, chunks = parts),
      .combine      = function(...)
        data.table::rbindlist(list(...)),
      .multicombine = TRUE,
      .packages     = packages
    ) %dopar% {
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
        for (k in base::as.integer(seq_len(nrow(new)))) {
          x <- threshOptim(
            data = data,
            actTar = base::names(data)[ActualColNumber],
            predTar = base::names(data)[PredictColNumber],
            tpProfit = TruePositiveCost,
            tnProfit = TrueNegativeCost,
            fpProfit = FalsePositiveCost,
            fnProfit = FalseNegativeCost,
            MidTierDoNothing = TRUE,
            MidTierCost = MidTierCost,
            MidTierLowThresh = new[k, 8][[1]],
            MidTierHighThresh = new[k, 9][[1]]
          )
          data.table::set(new,
                          i = k,
                          j = 7L,
                          value = x[[1]])
          temp <- x[[2]]
          data.table::set(new,
                          i = k,
                          j = 10L,
                          value = temp[Thresholds == eval(x[[1]]),
                                       "Utilities"][[1]])
        }
        base::return(new)
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
        data[, eval(actTar) := base::as.numeric(
          base::as.character(base::get(actTar)))]

        # Optimize each column's classification threshold ::
        popTrue <- base::mean(data[[(actTar)]])
        store   <- list()
        j <- 0
        base::options(warn = -1)
        for (i in base::c(MidTierHighThresh)) {
          j <- j + 1
          tp      <-
            base::sum(base::ifelse(
              !(data[[predTar]] < MidTierHighThresh &
                  data[[predTar]] > MidTierLowThresh) &
                data[[actTar]] == 1 & data[[predTar]] >= i,
              1,
              0
            ))
          tn      <-
            base::sum(base::ifelse(
              !(data[[predTar]] < MidTierHighThresh &
                  data[[predTar]] > MidTierLowThresh) &
                data[[actTar]] == 0 & data[[predTar]] <  i,
              1,
              0
            ))
          fp      <-
            base::sum(base::ifelse(
              !(data[[predTar]] < MidTierHighThresh &
                  data[[predTar]] > MidTierLowThresh) &
                data[[actTar]] == 0 & data[[predTar]] >= i,
              1,
              0
            ))
          fn      <-
            base::sum(base::ifelse(
              !(data[[predTar]] < MidTierHighThresh &
                  data[[predTar]] > MidTierLowThresh) &
                data[[actTar]] == 1 & data[[predTar]] <  i,
              1,
              0
            ))
          none    <-
            base::sum(base::ifelse(
              data[[predTar]] <= MidTierHighThresh &
                data[[predTar]] >= MidTierLowThresh,
              1,
              0
            ))
          tpr     <-
            base::ifelse((tp + fn) == 0, 0, tp / (tp + fn))
          fpr     <-
            base::ifelse((fp + tn) == 0, 0, fp / (fp + tn))
          noneRate <- none / base::nrow(data)
          utility <-
            (1 - noneRate) * (
              popTrue * (tpProfit * tpr + fnProfit * (1 - tpr)) +
                (1 - popTrue) * (fpProfit * fpr + tnProfit * (1 -fpr))
            ) + noneRate * MidTierCost
          store[[j]] <- base::c(i, utility)
        }
        all <- data.table::rbindlist(list(store))
        utilities <- data.table::melt(all[2, ])
        data.table::setnames(utilities, "value", "Utilities")
        thresholds <- data.table::melt(all[1, ])
        data.table::setnames(thresholds, "value", "Thresholds")
        results <- cbind(utilities, thresholds)[, c(-1, -3)]
        thresh <-
          results[Thresholds <= eval(MidTierLowThresh) |
                    Thresholds >= eval(
                      MidTierHighThresh)][order(-Utilities)][1,
                                                             2][[1]]
        options(warn = 1)
        return(list(thresh, results))
      }

      # Run core function
      data <- RedYellowGreenParallel(
        data,
        PredictColNumber  = PredictColNumber,
        ActualColNumber   = ActualColNumber,
        TruePositiveCost  = TruePositiveCost,
        TrueNegativeCost  = TrueNegativeCost,
        FalsePositiveCost = FalsePositiveCost,
        FalseNegativeCost = FalseNegativeCost,
        MidTierCost       = MidTierCost,
        new = i
      )

      # Return data table
      data
    }

  # Shut down cluster
  parallel::stopCluster(cl)

  # 3D Scatterplot
  s3d <-
    scatterplot3d::scatterplot3d(
      x = results[["MTLT"]],
      y = results[["MTHT"]],
      z = results[["Utility"]],
      type = "p",
      color = "#401a50",
      angle = 45,
      pch = 16,
      main = paste0("Utility Maximizer - Main Threshold at ",
                    results[order(-Utility)][1, "MTHT"][[1]]),
      sub = paste0("Lower Thresh = ",
                   results[order(-Utility)][1,
                                            "MTLT"][[1]],
                   " and Upper Thresh = ",
                   results[order(-Utility)][1, "MTHT"][[1]]),
      xlab = "Mid Tier Lower Threshold",
      ylab = "Mid Tier Higher Threshold",
      zlab = "Utility"
    )
  model <-
    stats::lm(results[["Utility"]] ~ results[["MTLT"]] + results[["MTHT"]])
  s3d$plane3d(model)
  N <- nrow(results)
  s3d$points3d(
    x = results[order(-Utility)][1:(N / 100), "MTLT"][[1]],
    y = results[order(-Utility)][1:(N / 100), "MTHT"][[1]],
    z = results[order(-Utility)][1:(N / 100), "Utility"][[1]],
    col = "#00aa9d",
    type = "h",
    pch = 1
  )
  s3d$points3d(
    x = results[order(-Utility)][1, "MTLT"][[1]],
    y = results[order(-Utility)][1, "MTHT"][[1]],
    z = results[order(-Utility)][1, "Utility"][[1]],
    col = "black",
    type = "h",
    pch = 10
  )
  return(results)
}

#' Utility maximizing thresholds for binary classification
#'
#' This function will return the utility maximizing threshold for future predictions along with the data generated to estimate the threshold
#'
#' @author Adrian Antico
#' @family Model Evaluation and Interpretation
#' @param data data is the data table you are building the modeling on
#' @param actTar The column name where the actual target variable is located (in binary form)
#' @param predTar The column name where the predicted values are located
#' @param tpProfit This is the utility for generating a true positive prediction
#' @param tnProfit This is the utility for generating a true negative prediction
#' @param fpProfit This is the cost of generating a false positive prediction
#' @param fnProfit This is the cost of generating a false negative prediction
#' @examples
#' data <- data.table::data.table(Target = runif(10))
#' data[, x1 := qnorm(Target)]
#' data[, x2 := runif(10)]
#' data[, Predict := log(pnorm(0.85 * x1 +
#'                               sqrt(1-0.85^2) * qnorm(x2)))]
#' data[, ':=' (x1 = NULL, x2 = NULL)]
#' data <- threshOptim(data     = data,
#'                     actTar   = "Target",
#'                     predTar  = "Predict",
#'                     tpProfit = 0,
#'                     tnProfit = 0,
#'                     fpProfit = -1,
#'                     fnProfit = -2)
#' optimalThreshold <- data$Thresholds
#' allResults <- data$EvaluationTable
#' @return Optimal threshold and corresponding utilities for the range of thresholds tested
#' @export
threshOptim <- function(data,
                        actTar   = "target",
                        predTar  = "p1",
                        tpProfit = 0,
                        tnProfit = 0,
                        fpProfit = -1,
                        fnProfit = -2) {

  # Ensure packages are available
  requireNamespace('data.table', quietly = FALSE)

  # Check data.table
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }

  # Convert factor target to numeric
  data[, eval(actTar) := as.numeric(as.character(get(actTar)))]

  # Optimize each column's classification threshold ::
  popTrue <- base::mean(data[[(actTar)]])
  store   <- list()
  j <- 0
  options(warn = -1)
  for (i in seq(from = 0.01, to = 0.99, by = 0.01)) {
    j <- j + 1
    tp      <-
      base::sum(ifelse(data[[actTar]] == 1 &
                         data[[predTar]] >= i, 1, 0))
    tn      <-
      base::sum(ifelse(data[[actTar]] == 0 &
                         data[[predTar]] <  i, 1, 0))
    fp      <-
      base::sum(ifelse(data[[actTar]] == 0 &
                         data[[predTar]] >= i, 1, 0))
    fn      <-
      base::sum(ifelse(data[[actTar]] == 1 &
                         data[[predTar]] <  i, 1, 0))
    tpr     <- ifelse((tp + fn) == 0, 0, tp / (tp + fn))
    fpr     <- ifelse((fp + tn) == 0, 0, fp / (fp + tn))
    utility <-
      popTrue * (tpProfit * tpr +
                   fnProfit * (1 - tpr)) +
      (1 - popTrue) * (fpProfit * fpr + tnProfit * (1 -fpr))
    store[[j]] <- c(i, utility)
  }
  all <- data.table::rbindlist(list(store))
  utilities <- data.table::melt(all[2, ])
  data.table::setnames(utilities, "value", "Utilities")
  thresholds <- data.table::melt(all[1, ])
  data.table::setnames(thresholds, "value", "Thresholds")
  results <- cbind(utilities, thresholds)[, c(-1, -3)]
  thresh <- results[order(-Utilities)][1, 2][[1]]
  options(warn = 1)
  return(list(Thresholds = thresh, EvaluationTable = results))
}

#' AutoNLS is a function for automatically building nls models
#'
#' This function will build models for 9 different nls models, along with a non-parametric monotonic regression and a polynomial regression. The models are evaluated, a winner is picked, and the predicted values are stored in your data table.
#'
#' @author Adrian Antico
#' @family Supervised Learning
#' @param data Data is the data table you are building the modeling on
#' @param y Y is the target variable name in quotes
#' @param x X is the independent variable name in quotes
#' @param monotonic This is a TRUE/FALSE indicator - choose TRUE if you want monotonic regression over polynomial regression
#' @examples
#' # Create Growth Data
#' data <-
#'   data.table::data.table(Target = seq(1, 500, 1),
#'                          Variable = rep(1, 500))
#' for (i in as.integer(1:500)) {
#'   if (i == 1) {
#'     var <- data[i, "Target"][[1]]
#'     data.table::set(data,
#'                     i = i,
#'                     j = 2L,
#'                     value = var * (1 + runif(1) / 100))
#'   } else {
#'     var <- data[i - 1, "Variable"][[1]]
#'     data.table::set(data,
#'                     i = i,
#'                     j = 2L,
#'                     value = var * (1 + runif(1) / 100))
#'   }
#' }
#'
#' # Add jitter to Target
#' data[, Target := jitter(Target,
#'                         factor = 0.25)]
#'
#' # To keep original values
#' data1 <- data.table::copy(data)
#'
#' # Merge and Model data
#' data11 <- AutoNLS(
#'   data = data,
#'   y = "Target",
#'   x = "Variable",
#'   monotonic = TRUE
#' )
#'
#' # Join predictions to source data
#' data2 <- merge(
#'   data1,
#'   data11$PredictionData,
#'   by = "Variable",
#'   all = FALSE
#' )
#'
#' # Plot output
#' ggplot2::ggplot(data2, ggplot2::aes(x = Variable)) +
#'   ggplot2::geom_line(ggplot2::aes(y = data2[["Target.x"]],
#'                                   color = "Target")) +
#'   ggplot2::geom_line(ggplot2::aes(y = data2[["Target.y"]],
#'                                   color = "Predicted")) +
#'  RemixAutoML::ChartTheme(Size = 12) +
#'   ggplot2::ggtitle(paste0("Growth Models AutoNLS: ",
#'                           data11$ModelName)) +
#'   ggplot2::ylab("Target Variable") +
#'   ggplot2::xlab("Independent Variable") +
#'   ggplot2::scale_colour_manual("Values",
#'                                breaks = c("Target",
#'                                           "Predicted"),
#'                                values = c("red",
#'                                           "blue"))
#' summary(data11$ModelObject)
#' data11$EvaluationMetrics
#' @return A list containing "PredictionData" which is a data table with your original column replaced by the nls model predictions; "ModelName" the model name; "ModelObject" The winning model to later use; "EvaluationMetrics" Model metrics for models with ability to build.
#' @export
AutoNLS <- function(data,
                    y,
                    x,
                    monotonic = TRUE) {

  # Ensure packages are available
  requireNamespace('data.table', quietly = FALSE)

  # Begin
  DATA <- data
  nls_collection <-
    data.table::data.table(
      ModelName = c(
        "Poly",
        "Asymp",
        "AsympOff",
        "AsympOrig",
        "Biexp",
        "FourParmLog",
        "Gompertz",
        "Logistic",
        "Michal_Menton",
        "Weilbull"
      ),
      MeanAbsError = rep(999, 10)
    )

  # Convert to data.table if not already
  if (!data.table::is.data.table(data))
    DATA <- data.table::as.data.table(data)

  data.table::setnames(DATA,
                       c(eval(y),eval(x)),
                       c("Target","Variable"))

  z <- DATA[["Variable"]]
  zz <- DATA[["Target"]]
  tryCatch({
    if (monotonic == TRUE) {
      tryCatch({
        baseline <- monreg::monreg(z, zz, hr = 0.5, hd = 0.5)
        preds    <- baseline$estimation
        preds[preds < 0] <- 0
        val0     <- base::mean(abs(zz - preds))
        data.table::set(nls_collection, 1L, 2L, value = val0)
      }, error = function(x) {
        return("skip")
      })
    } else {
      tryCatch({
        baseline <-
          stats::lm(as.formula(Target ~ poly(Variable,
                                             5)),
                    data = DATA)
        preds    <- baseline$fitted.values
        preds[preds < 0] <- 0
        val0     <- base::mean(abs(zz - preds))
        data.table::set(nls_collection, 1L, 2L, value = val0)
      }, error = function(x) {
        return("skip")
      })
    }
  }, error = function(x) {
    return("skip")
  })

  # Asymp model
  tryCatch({
    model1 <-
      stats::nls(Target ~ SSasymp(Variable, Asym, R0, lrc),
                 data = DATA)
    preds1 <- stats::fitted(model1, DATA)
    preds1[preds1 < 0] <- 0
    val    <- base::mean(abs(zz - preds1))
    data.table::set(nls_collection, 2L, 2L, value = val1)
  }, error = function(x) {
    return("skip")
  })

  # Asymp offset model
  tryCatch({
    model2 <-
      stats::nls(Target ~ SSasympOff(Variable, Asym, lrc, c0),
                 data = DATA)
    preds2 <- stats::fitted(model2, DATA)
    preds2[preds2 < 0] <- 0
    va2    <- base::mean(abs(zz - preds2))
    data.table::set(nls_collection, 3L, 2L, value = val2)
  }, error = function(x) {
    return("skip")
  })

  # Asymp origin model
  tryCatch({
    model3 <-
      stats::nls(Target ~ SSasympOrig(Variable,Asym,lrc),
                 data = DATA)
    preds3 <- stats::fitted(model3, DATA)
    preds3[preds3 < 0] <- 0
    va3    <- base::mean(abs(zz - preds3))
    data.table::set(nls_collection, 4L, 2L, value = val3)
  }, error = function(x) {
    return("skip")
  })

  # Biexp model
  tryCatch({
    model4 <-
      stats::nls(Target ~ SSbiexp(Variable,A1,lrc1,A2,lrc2),
                 data = DATA)
    preds4 <- stats::fitted(model4, DATA)
    preds4[preds4 < 0] <- 0
    val4   <- base::mean(abs(zz - preds4))
    data.table::set(nls_collection, 5L, 2L, value = val4)
  }, error = function(x) {
    return("skip")
  })

  # Four parameter logistic model
  tryCatch({
    model5 <-
      stats::nls(Target ~ SSfpl(Variable, A, B, xmid, scal),
                 data = DATA)
    preds5 <- stats::fitted(model5, DATA)
    preds5[preds5 < 0] <- 0
    val5   <- base::mean(abs(zz - preds5))
    data.table::set(nls_collection, 6L, 2L, value = val5)
  }, error = function(x) {
    return("skip")
  })

  # Gompertz model
  tryCatch({
    model6 <-
      stats::nls(Target ~ SSgompertz(Variable,Asym,b2,b3),
                 data = DATA)
    preds6 <- stats::fitted(model6, DATA)
    preds6[preds6 < 0] <- 0
    val6   <- base::mean(abs(zz - preds6))
    data.table::set(nls_collection, 7L, 2L, value = val6)
  }, error = function(x) {
    return("skip")
  })

  # Logistic model
  tryCatch({
    model7 <-
      stats::nls(Target ~ SSlogis(Variable,Asym,xmid,scal),
                 data = DATA)
    preds7 <- stats::fitted(model7, DATA)
    preds7[preds7 < 0] <- 0
    val7   <- base::mean(abs(zz - preds7))
    data.table::set(nls_collection, 8L, 2L, value = val7)
  }, error = function(x) {
    return("skip")
  })

  # Michaelis-Menton model
  tryCatch({
    model8 <-
      stats::nls(Target ~ SSmicmen(Variable,Vm,K),
                 data = DATA)
    preds8 <- stats::fitted(model8, DATA)
    preds8[preds8 < 0] <- 0
    val8   <- base::mean(abs(zz - preds8))
    data.table::set(nls_collection, 9L, 2L,
                    value = val8)
  }, error = function(x) {
    return("skip")
  })

  # Weibull Growth model
  tryCatch({
    model9 <-
      stats::nls(Target ~ SSweibull(Variable,Asym,Drop,lrc,pwr),
                 data = DATA)
    preds9 <- stats::fitted(model9, DATA)
    preds9[preds9 < 0] <- 0
    val9   <- base::mean(abs(zz - preds9))
    data.table::set(nls_collection, 10L, 2L, value = val9)
  }, error = function(x) {
    return("skip")
  })

  # Store best model name
  name <-
    nls_collection[MeanAbsError != 999][order(MeanAbsError)][1, 1][[1]]

  # Collect metrics for all models fitted
  temp <- nls_collection[MeanAbsError != 999][order(MeanAbsError)]

  # Create column using best model
  if (name == nls_collection[10, 1][[1]]) {
    DATA[, Target := preds9]
    data.table::setnames(DATA,
                         c("Target","Variable"),
                         c(eval(y),eval(x)))
    return(
      list(PredictionData = DATA,
           ModelName = name,
           ModelObject = model9,
           EvaluationMetrics = temp))
  } else if (name == nls_collection[2, 1][[1]]) {
    DATA[, Target := preds1]
    data.table::setnames(DATA,
                         c("Target","Variable"),
                         c(eval(y),eval(x)))
    return(
      list(PredictionData = DATA,
           ModelName = name,
           ModelObject = model1,
           EvaluationMetrics = temp))
  } else if (name == nls_collection[3, 1][[1]]) {
    DATA[, Target := preds2]
    data.table::setnames(DATA,
                         c("Target","Variable"),
                         c(eval(y),eval(x)))
    return(
      list(PredictionData = DATA,
           ModelName = name,
           ModelObject = model2,
           EvaluationMetrics = temp))
  } else if (name == nls_collection[4, 1][[1]]) {
    DATA[, Target := preds3]
    data.table::setnames(DATA,
                         c("Target","Variable"),
                         c(eval(y),eval(x)))
    return(
      list(PredictionData = DATA,
           ModelName = name,
           ModelObject = model3,
           EvaluationMetrics = temp))
  } else if (name == nls_collection[5, 1][[1]]) {
    DATA[, Target := preds4]
    data.table::setnames(DATA,
                         c("Target","Variable"),
                         c(eval(y),eval(x)))
    return(
      list(PredictionData = DATA,
           ModelName = name,
           ModelObject = model4,
           EvaluationMetrics = temp))
  } else if (name == nls_collection[6, 1][[1]]) {
    DATA[, Target := preds5]
    data.table::setnames(DATA,
                         c("Target","Variable"),
                         c(eval(y),eval(x)))
    return(
      list(PredictionData = DATA,
           ModelName = name,
           ModelObject = model5,
           EvaluationMetrics = temp))
  } else if (name == nls_collection[7, 1][[1]]) {
    DATA[, Target := preds6]
    data.table::setnames(DATA,
                         c("Target","Variable"),
                         c(eval(y),eval(x)))
    return(
      list(PredictionData = DATA,
           ModelName = name,
           ModelObject = model6,
           EvaluationMetrics = temp))
  } else if (name == nls_collection[8, 1][[1]]) {
    DATA[, Target := preds7]
    data.table::setnames(DATA,
                         c("Target","Variable"),
                         c(eval(y),eval(x)))
    return(
      list(PredictionData = DATA,
           ModelName = name,
           ModelObject = model7,
           EvaluationMetrics = temp))
  } else if (name == nls_collection[9, 1][[1]]) {
    DATA[, Target := preds8]
    data.table::setnames(DATA,
                         c("Target","Variable"),
                         c(eval(y),eval(x)))
    return(
      list(PredictionData = DATA,
           ModelName = name,
           ModelObject = model8,
           EvaluationMetrics = temp))
  } else {
    DATA[, Target := preds]
    data.table::setnames(DATA,
                         c("Target","Variable"),
                         c(eval(y),eval(x)))
    if(monotonic) {
      name <- "Monotonic Regression"
    } else {
      name <- "Polynomial Regression"
    }
    return(
      list(PredictionData = DATA,
           ModelName = name,
           ModelObject = baseline,
           EvaluationMetrics = temp))
  }
}

#' Multiplot is a function for combining multiple plots
#'
#' Sick of copying this one into your code? Well, not anymore.
#'
#' @author Adrian Antico
#' @family Misc
#' @param plotlist This is the list of your charts
#' @param cols This is the number of columns in your multiplot
#' @param layout Leave NULL
#' @param ... Passthrough arguments
#' @examples
#' Correl <- 0.85
#' data <- data.table::data.table(Target = runif(100))
#' data[, x1 := qnorm(Target)]
#' data[, x2 := runif(100)]
#' data[, Independent_Variable1 := log(pnorm(Correl * x1 +
#'                                             sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Predict := (pnorm(Correl * x1 +
#'                            sqrt(1-Correl^2) * qnorm(x2)))]
#' p1 <- RemixAutoML::ParDepCalPlots(data,
#'                                   PredictionColName = "Predict",
#'                                   TargetColName = "Target",
#'                                   IndepVar = "Independent_Variable1",
#'                                   GraphType = "calibration",
#'                                   PercentileBucket = 0.20,
#'                                   FactLevels = 10,
#'                                   Function = function(x) mean(x, na.rm = TRUE))
#' p2 <- RemixAutoML::ParDepCalPlots(data,
#'                                   PredictionColName = "Predict",
#'                                   TargetColName = "Target",
#'                                   IndepVar = "Independent_Variable1",
#'                                   GraphType = "boxplot",
#'                                   PercentileBucket = 0.20,
#'                                   FactLevels = 10,
#'                                   Function = function(x) mean(x, na.rm = TRUE))
#' RemixAutoML::multiplot(plotlist = list(p1,p2), cols = 2)
#' @return Multiple ggplots on a single image
#' @export
multiplot <-
  function(...,
           plotlist = NULL,
           cols     = 2,
           layout   = NULL) {

    # Ensure packages are available
    requireNamespace('data.table', quietly = FALSE)

    plots <- c(list(...), plotlist)

    numPlots <- length(plots)

    if (is.null(layout)) {
      layout <- matrix(seq(1, cols * ceiling(numPlots / cols)),
                       ncol = cols,
                       nrow = ceiling(numPlots / cols))
    }

    if (numPlots == 1) {
      print(plots[[1]])

    } else {
      grid::grid.newpage()
      grid::pushViewport(grid::viewport(layout = grid::grid.layout(
        nrow(layout),
        ncol(layout))))

      for (i in 1:numPlots) {
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

        print(
          plots[[i]],
          vp = grid::viewport(
            layout.pos.row = matchidx$row,
            layout.pos.col = matchidx$col
          )
        )
      }
    }
  }

#' ChartTheme function is a ggplot theme generator for ggplots
#'
#' This function helps your ggplots look professional with the choice of the two main colors that will dominate the theme
#'
#' @author Adrian Antico
#' @family Misc
#' @param Size The size of the axis labels and title
#' @examples
#' data <- data.table::data.table(DateTime = as.Date(Sys.time()),
#'   Target = stats::filter(rnorm(1000,
#'                                mean = 50,
#'                                sd = 20),
#'                          filter=rep(1,10),
#'                          circular=TRUE))
#' data[, temp := seq(1:1000)][, DateTime := DateTime - temp][, temp := NULL]
#' data <- data[order(DateTime)]
#' p <- ggplot2::ggplot(data, ggplot2::aes(x = DateTime, y = Target)) + ggplot2::geom_line()
#' p <- p + ChartTheme(Size = 12)
#' @return An object to pass along to ggplot objects following the "+" sign
#' @export
ChartTheme <- function(Size = 12) {
  chart_theme <-
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "gray94"),
      panel.background = ggplot2::element_rect(
        fill = "lightsteelblue1",
        colour = "darkblue",
        size = 0.25,
        color = "darkblue"
      ),
      panel.grid.major = ggplot2::element_line(
        colour = "darkblue",
        size = 0.01,
        color = "white",
        linetype = 1
      ),
      panel.grid.minor = ggplot2::element_line(
        colour = "darkblue",
        size = 0.01,
        color = "white",
        linetype = 1
      ),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(
        color = "darkblue",
        size = Size,
        face = "bold"
      ),
      legend.background = ggplot2::element_rect(
        fill = "gray95",
        size = 1,
        linetype = "solid",
        color = "darkblue"
      ),
      plot.title = ggplot2::element_text(
        color = "darkblue",
        size = Size,
        face = "bold"
      ),
      axis.title = ggplot2::element_text(
        color = "darkblue",
        size = Size,
        face = "bold"
      ),
      axis.text = ggplot2::element_text(
        colour = "darkblue",
        face = "bold",
        angle = 90
      ),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(
        t = 20,
        r = 20,
        b = 20,
        l = 20
      )),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(
        t = 20,
        r = 20,
        b = 20,
        l = 20
      )),
      panel.border = ggplot2::element_rect(
        colour = "darkblue",
        fill = NA,
        size = 1.5
      )
    )
  chart_theme
}

#' Percentile rank function
#'
#' This function computes percentile ranks for each row in your data like Excel's PERCENT_RANK
#'
#' @author Adrian Antico
#' @family Misc
#' @param x X is your variable of interest
#' @examples
#' data <- data.table::data.table(A = runif(100))
#' data[, Rank := percRank(A)]
#' @return vector of percentile ranks
#' @examples
#' data <- data.table::data.table(A = runif(100))
#' data[, Percentile := percRank(A)]
#' @export
percRank <- function(x) {
  trunc(rank(x)) / length(x)
}

#' ParDepCalPlots automatically builds partial dependence calibration plots for model evaluation
#'
#' This function automatically builds partial dependence calibration plots and partial dependence calibration boxplots for model evaluation using regression, quantile regression, and binary and multinomial classification
#' @author Adrian Antico
#' @family Model Evaluation and Interpretation
#' @param data Data containing predicted values and actual values for comparison
#' @param PredictionColName Predicted values column names
#' @param TargetColName Target value column names
#' @param IndepVar Independent variable column names
#' @param GraphType calibration or boxplot - calibration aggregated data based on summary statistic; boxplot shows variation
#' @param PercentileBucket Number of buckets to partition the space on (0,1) for evaluation
#' @param FactLevels The number of levels to show on the chart (1. Levels are chosen based on frequency; 2. all other levels grouped and labeled as "Other")
#' @param Function Supply the function you wish to use for aggregation.
#' @return Partial dependence calibration plot or boxplot
#' @examples
#' Correl <- 0.85
#' data <- data.table::data.table(Target = runif(100))
#' data[, x1 := qnorm(Target)]
#' data[, x2 := runif(100)]
#' data[, Independent_Variable1 := log(pnorm(Correl * x1 +
#'                                             sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Predict := (pnorm(Correl * x1 +
#'                            sqrt(1-Correl^2) * qnorm(x2)))]
#' p1 <- RemixAutoML::ParDepCalPlots(data,
#'                                   PredictionColName = "Predict",
#'                                   TargetColName = "Target",
#'                                   IndepVar = "Independent_Variable1",
#'                                   GraphType = "calibration",
#'                                   PercentileBucket = 0.20,
#'                                   FactLevels = 10,
#'                                   Function = function(x) mean(x, na.rm = TRUE))
#' p1
#' @export
ParDepCalPlots <- function(data,
                           PredictionColName = c("PredictedValues"),
                           TargetColName  = c("ActualValues"),
                           IndepVar    = c("Independent_Variable_Name"),
                           GraphType        = c("calibration"),
                           PercentileBucket = 0.05,
                           FactLevels  = 10,
                           Function    = function(x)
                             base::mean(x, na.rm = TRUE)) {

  # Ensure packages are available
  requireNamespace('data.table', quietly = FALSE)

  # Turn off ggplot2 warnings
  options(warn = -1)

  # Build buckets by independent variable of choice
  preds2 <- data.table::as.data.table(data)

  # Subset columns
  cols <- c(PredictionColName, TargetColName, IndepVar)
  preds2 <- preds2[, ..cols]

  # Structure data
  data <- data[, ..cols]
  data.table::setcolorder(data, c(PredictionColName, TargetColName, IndepVar))

  # If actual is in factor form, convert to numeric
  if (!is.numeric(preds2[[TargetColName]])) {
    preds2[, eval(TargetColName) := as.numeric(as.character(get(TargetColName)))]
    GraphType <- "calibration"
  }

  # Prepare for both calibration and boxplot
  if (is.numeric(preds2[[IndepVar]]) ||
      is.integer(preds2[[IndepVar]])) {
    preds2[, rank := 100 *
             (round(percRank(preds2[[IndepVar]]) / PercentileBucket) * PercentileBucket)]
  } else {
    GraphType <- "FactorVar"
    preds2[, id := seq_len(.N), by = get(IndepVar)]
    preds2 <-
      preds2[, .(Function(get(TargetColName)),
                 Function(get(PredictionColName)),
                 max(id)),
             by = get(IndepVar)][order(-V3)]
    if (nrow(preds2) > FactLevels) {
      temp1 <- preds2[1:FactLevels][, V3 := NULL]
      temp2 <- preds2[(FactLevels + 1):nrow(preds2)]
      temp2[, ':=' (V1 = V1 * V3 / base::sum(V3),
                    V2 = V2 * V3 / base::sum(V3))]
      temp3 <- temp2[, .(base::sum(V1), base::sum(V2))]
      temp3[, get := "Other"]
      data.table::setcolorder(temp3, c(3, 1, 2))
    }
    preds2[, V3 := NULL]
    if (nrow(preds2) > FactLevels) {
      preds3 <- data.table::rbindlist(list(temp1, temp3))
    } else {
      preds3 <- preds2
    }
    data.table::setnames(
      preds3,
      old = c("get", "V1", "V2"),
      new = c(IndepVar, TargetColName, PredictionColName)
    )
    preds3 <- preds3[order(-get(PredictionColName))]
  }

  # Build plots
  if (GraphType == "calibration") {
    # Aggregate by rank for calibration
    preds3 <-
      preds2[, lapply(.SD, noquote(Function)), by = rank][order(rank)]
    preds3[, eval(IndepVar) := as.numeric(get(IndepVar))]

    # Partial dependence calibration plot
    plot <-
      ggplot2::ggplot(preds3, ggplot2::aes(x = preds3[[IndepVar]])) +
      ggplot2::geom_line(ggplot2::aes(y = preds3[[PredictionColName]],
                                      color = "Predicted")) +
      ggplot2::geom_line(ggplot2::aes(y = preds3[[TargetColName]],
                                      color = "Actuals")) +
      ggplot2::ylab("Actual | Predicted") +
      ggplot2::xlab(IndepVar) +
      ggplot2::scale_colour_manual(
        "",
        breaks = c("Actuals", "Predicted"),
        values = c("blue", "red")
      ) +
      ChartTheme(Size = 15) +
      ggplot2::ggtitle("Partial Dependence Calibration Plot")
  } else if (GraphType == "boxplot") {
    # Partial dependence boxplot
    keep <- c("rank", TargetColName, IndepVar)
    actual <- preds2[, ..keep]
    actual[, Type := "actual"]
    data.table::setnames(actual, TargetColName, "Output")

    keep <- c("rank", PredictionColName, IndepVar)
    predicted <- preds2[, ..keep]
    predicted[, Type := "predicted"]
    data.table::setnames(predicted, PredictionColName, "Output")

    data <-
      data.table::rbindlist(list(actual, predicted))[order(rank)]
    data[, rank := as.factor(rank)]
    data <- data[, eval(IndepVar) := as.numeric(get(IndepVar))]
    data <-
      data[, eval(IndepVar) := round(Function(get(IndepVar)), 3),
           by = rank]
    data[, eval(IndepVar) := as.factor(get(IndepVar))]
    data[, rank := NULL]
    plot <-
      ggplot2::ggplot(data, ggplot2::aes(x = data[[IndepVar]],
                                         y = Output)) +
      ggplot2::geom_boxplot(ggplot2::aes(fill = Type)) +
      ggplot2::scale_fill_manual(values = c("red", "blue")) +
      ggplot2::ggtitle("Partial Dependence Calibration Boxplot") +
      ggplot2::xlab(eval(IndepVar)) +
      ggplot2::ylab("Actual | Predicted") +
      ChartTheme(Size = 15)
  } else if (GraphType == "FactorVar") {
    keep <- c(IndepVar, TargetColName)
    actual <- preds3[, ..keep]
    actual[, Type := "actual"]
    data.table::setnames(actual, TargetColName, "Output")

    keep <- c(IndepVar, PredictionColName)
    predicted <- preds3[, ..keep]
    predicted[, Type := "predicted"]
    data.table::setnames(predicted, PredictionColName, "Output")
    data <-
      data.table::rbindlist(list(actual,
                                 predicted))[order(-Output)]

    plot <-
      ggplot2::ggplot(data, ggplot2::aes(x = data[[IndepVar]],
                                         y = Output)) +
      ggplot2::geom_bar(stat = "identity",
                        position = "dodge",
                        ggplot2::aes(fill = Type)) +
      ggplot2::scale_fill_manual(values = c("red",
                                            "blue")) +
      ggplot2::ggtitle("Partial Dependence Calibration Barplot") +
      ggplot2::xlab(eval(IndepVar)) +
      ggplot2::ylab("Actual | Predicted") +
      ChartTheme(Size = 15)
  }
  return(plot)
}

#' EvalPlot automatically builds calibration plots for model evaluation
#'
#' This function automatically builds calibration plots and calibration boxplots for model evaluation using regression, quantile regression, and binary and multinomial classification
#' @author Adrian Antico
#' @family Model Evaluation and Interpretation
#' @param data Data containing predicted values and actual values for comparison
#' @param PredictionColName String representation of column name with predicted values from model
#' @param TargetColName String representation of column name with target values from model
#' @param GraphType Calibration or boxplot - calibration aggregated data based on summary statistic; boxplot shows variation
#' @param PercentileBucket Number of buckets to partition the space on (0,1) for evaluation
#' @param aggrfun The statistics function used in aggregation, listed as a function
#' @return Calibration plot or boxplot
#' @examples
#' Correl <- 0.85
#' data <- data.table::data.table(Target = runif(100))
#' data[, x1 := qnorm(Target)]
#' data[, x2 := runif(100)]
#' data[, Independent_Variable1 := log(pnorm(Correl * x1 +
#'                                             sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Predict := (pnorm(Correl * x1 +
#'                            sqrt(1-Correl^2) * qnorm(x2)))]
#' EvalPlot(data,
#'          PredictionColName = "Predict",
#'          TargetColName = "Target",
#'          GraphType = "calibration",
#'          PercentileBucket = 0.05,
#'          aggrfun = function(x) quantile(x, probs = 0.50, na.rm = TRUE))
#' @export
EvalPlot <- function(data,
                     PredictionColName = c("PredictedValues"),
                     TargetColName  = c("ActualValues"),
                     GraphType        = c("calibration"),
                     PercentileBucket = 0.05,
                     aggrfun     = function(x)
                       base::mean(x, na.rm = TRUE)) {

  # Ensure packages are available
  requireNamespace('data.table', quietly = FALSE)

  # Turn data into data.table if not already
  if (!data.table::is.data.table(data))
    data <- data.table::as.data.table(data)

  # Structure data
  cols <- c(eval(PredictionColName), eval(TargetColName))
  data <- data[, ..cols]
  data.table::setcolorder(data, c(PredictionColName, TargetColName))
  data.table::setnames(data,
                       c(PredictionColName, TargetColName),
                       c("preds", "acts"))

  # If actual is in factor form, convert to numeric
  if (!is.numeric(data[["acts"]])) {
    data[, acts := as.numeric(as.character(acts))]
    GraphType <- "calibration"
  }

  # Add a column that ranks predicted values
  data[, rank := 100 * (round(percRank(data[[1]]) / PercentileBucket) * PercentileBucket)]

  # Plot
  if (GraphType == "boxplot") {
    # Remove classification and non-event predicted values
    data[, rank := as.factor(rank)]

    cols <- c("rank", "preds")
    zz1 <- data[, ..cols]
    zz1[, Type := 'predicted']
    data.table::setnames(zz1, c("preds"), c("output"))

    cols <- c("rank", "acts")
    zz2 <- data[, ..cols]
    zz2[, Type := 'actual']
    data.table::setnames(zz2, c("acts"), c("output"))

    data <- data.table::rbindlist(list(zz1, zz2))

    plot <-
      ggplot2::ggplot(data, ggplot2::aes(x = rank,
                                         y = output,
                                         fill = Type)) +
      ggplot2::geom_boxplot(outlier.color = "red",
                            color = "black") +
      ggplot2::ggtitle("Calibration Evaluation Boxplot") +
      ggplot2::xlab("Predicted Percentile") +
      ggplot2::ylab("Observed Values") +
      ChartTheme(Size = 15) +
      ggplot2::scale_fill_manual(values=c("blue",
                                          "red"))

  } else {
    # Aggregate all columns by rank, utilizing mean as the aggregator statistic
    data <- data[, lapply(.SD, noquote(aggrfun)), by = rank]

    # Build calibration plot
    plot  <- ggplot2::ggplot(data, ggplot2::aes(x = rank))  +
      ggplot2::geom_line(ggplot2::aes(y = data[[3]],
                                      color = "Actual")) +
      ggplot2::geom_line(ggplot2::aes(y = data[[2]],
                                      color = "Predicted")) +
      ggplot2::xlab("Predicted Percentile") +
      ggplot2::ylab("Observed Values") +
      ggplot2::scale_color_manual(values = c("red", "blue")) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                         hjust = 1)) +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::ggtitle("Calibration Evaluation Plot") +
      ChartTheme(Size = 15) +
      ggplot2::scale_fill_manual(values=c("blue",
                                          "red"))
  }
  return(plot)
}

#' An Automated Feature Engineering Function
#'
#' Builds autoregressive and rolling stats from target columns and distributed lags and distributed rolling stats for independent features distributed across time. On top of that, you can also create time between instances along with their associated lags and rolling stats. This function works for data with groups and without groups.
#' @author Adrian Antico
#' @family Feature Engineering
#' @param data A data.table you want to run the function on
#' @param lags A numeric vector of the specific lags you want to have generated. You must include 1 if WindowingLag = 1.
#' @param periods A numeric vector of the specific rolling statistics window sizes you want to utilize in the calculations.
#' @param statsNames A character vector of the corresponding names to create for the rollings stats variables.
#' @param statsFUNs Vector that holds functions for your rolling stats, such as function(x) mean(x), function(x) sd(x), or function(x) quantile(x)
#' @param targets A character vector of the column names for the reference column in which you will build your lags and rolling stats
#' @param groupingVars A character vector of categorical variable names you will build your lags and rolling stats by
#' @param sortDateName The column name of your date column used to sort events over time
#' @param timeDiffTarget Specify a desired name for features created for time between events. Set to NULL if you don't want time between events features created.
#' @param timeAgg List the time aggregation level for the time between events features, such as "hour", "day", "week", "month", "quarter", or "year"
#' @param WindowingLag Set to 0 to build rolling stats off of target columns directly or set to 1 to build the rolling stats off of the lag-1 target
#' @param Type List either "Lag" if you want features built on historical values or "Lead" if you want features built on future values
#' @param Timer Set to TRUE if you percentage complete tracker printout
#' @param SkipCols Defaults to NULL; otherwise supply a character vector of the names of columns to skip
#' @param SimpleImpute Set to TRUE for factor level imputation of "0" and numeric imputation of -1
#' @return data.table of original data plus created lags, rolling stats, and time between event lags and rolling stats
#' @examples
#' N = 25116
#' data <- data.table::data.table(DateTime = as.Date(Sys.time()),
#'   Target = stats::filter(rnorm(N,
#'                                mean = 50,
#'                                sd = 20),
#'                          filter=rep(1,10),
#'                          circular=TRUE))
#' data[, temp := seq(1:N)][, DateTime := DateTime - temp][, temp := NULL]
#' data <- data[order(DateTime)]
#' data <- GDL_Feature_Engineering(data,
#'            lags           = c(seq(1,1,1)),
#'            periods        = c(3),
#'            statsFUNs      = c(function(x) quantile(x, probs = 0.20, na.rm = TRUE)),
#'            statsNames     = c("q20"),
#'            targets        = c("Target"),
#'            groupingVars   = NULL,
#'            sortDateName   = "DateTime",
#'            timeDiffTarget = NULL,
#'            timeAgg        = "days",
#'            WindowingLag   = 1,
#'            Type           = "Lag",
#'            Timer          = TRUE,
#'            SkipCols       = FALSE,
#'            SimpleImpute   = TRUE)
#' @export
GDL_Feature_Engineering <- function(data,
                                    lags           = c(seq(1,5,1)),
                                    periods        = c(3,5,10,15,20,25),
                                    statsFUNs      = c(function(x)
                                      quantile(x, probs = 0.1, na.rm = TRUE),
                                      function(x)
                                        quantile(x, probs = 0.9, na.rm = TRUE),
                                      function(x)
                                        base::mean(x, na.rm = TRUE),
                                      function(x)
                                        sd(x, na.rm = TRUE),
                                      function(x)
                                        quantile(x, probs = 0.25, na.rm = TRUE),
                                      function(x)
                                        quantile(x, probs = 0.75, na.rm = TRUE)),
                                    statsNames     = c("q10",
                                                       "q90",
                                                       "mean",
                                                       "sd",
                                                       "q25",
                                                       "q75"),
                                    targets        = c("qty"),
                                    groupingVars   = c("Group1",
                                                       "Group2"),
                                    sortDateName   = c("date"),
                                    timeDiffTarget = c("TimeDiffName"),
                                    timeAgg        = c("days"),
                                    WindowingLag   = 0,
                                    Type           = c("Lag"),
                                    Timer          = TRUE,
                                    SkipCols       = NULL,
                                    SimpleImpute   = TRUE) {

  # Ensure packages are available
  requireNamespace('data.table', quietly = FALSE)

  # Convert to data.table if not already
  if (!data.table::is.data.table(data))
    data <- data.table::as.data.table(data)

  # Ensure target is numeric
  data[, eval(targets) := as.numeric(get(targets))]

  # Set up counter for countdown
  CounterIndicator <- 0
  if (!is.null(timeDiffTarget)) {
    tarNum <- length(targets) + 1
  } else {
    tarNum <- length(targets)
  }

  # Define total runs
  if (!is.null(groupingVars)) {
    runs <-
      length(groupingVars) * tarNum * (length(periods) *
                                         length(statsNames) +
                                         length(lags))
  } else {
    runs <-
      tarNum * (length(periods) * length(statsNames) +
                  length(lags))
  }

  # Begin feature engineering
  if (!is.null(groupingVars)) {
    for (i in seq_along(groupingVars)) {
      Targets <- targets

      # Sort data
      if (tolower(Type) == "lag") {
        colVar <- c(groupingVars[i], sortDateName[1])
        data.table::setorderv(data, colVar, order = 1)
      } else {
        colVar <- c(groupingVars[i], sortDateName[1])
        data.table::setorderv(data, colVar, order = -1)
      }

      # Lags
      for (l in seq_along(lags)) {
        for (t in Targets) {
          if (!(paste0(groupingVars[i], "_LAG_",
                       lags[l], "_", t) %in% SkipCols)) {
            data[, paste0(groupingVars[i],
                          "_LAG_", lags[l], "_", t) := data.table::shift(
                            get(t), n = lags[l], type = "lag"),
                 by = get(groupingVars[i])]
            CounterIndicator <- CounterIndicator + 1
            if (Timer) {
              print(CounterIndicator / runs)
            }
          }
        }
      }

      # Time lags
      if (!is.null(timeDiffTarget)) {
        # Lag the dates first
        for (l in seq_along(lags)) {
          if (!(paste0(groupingVars[i], "TEMP", lags[l]) %in% SkipCols)) {
            data[, paste0(groupingVars[i], "TEMP",
                          lags[l]) := data.table::shift(get(sortDateName),
                                                        n = lags[l],
                                                        type = "lag"),
                 by = get(groupingVars[i])]
          }
        }

        # Difference the lag dates
        if (WindowingLag != 0) {
          for (l in seq_along(lags)) {
            if (!(paste0(timeDiffTarget, lags[l]) %in% SkipCols) & l == 1) {
              data[, paste0(groupingVars[i],
                            timeDiffTarget, lags[l]) := as.numeric(difftime(
                              get(sortDateName),
                              get(paste0(
                                groupingVars[i], "TEMP", lags[l]
                              )),
                              units = eval(timeAgg)
                            )), by = get(groupingVars[i])]
              CounterIndicator <- CounterIndicator + 1
              if (Timer) {
                print(CounterIndicator / runs)
              }
            } else {
              if (!(paste0(groupingVars[i], timeDiffTarget,
                           lags[l]) %in% SkipCols)) {
                data[, paste0(groupingVars[i],
                              timeDiffTarget, lags[l]) := as.numeric(
                                difftime(get(
                                  paste0(groupingVars[i], "TEMP", (lags[l - 1]))
                                ),
                                get(
                                  paste0(groupingVars[i], "TEMP", lags[l])
                                ),
                                units = eval(timeAgg))), by = get(groupingVars[i])]
                CounterIndicator <- CounterIndicator + 1
                if (Timer) {
                  print(CounterIndicator / runs)
                }
              }
            }
          }
        } else {
          for (l in seq_along(lags)) {
            if (l == 1) {
              if (!(paste0(groupingVars[i],
                           timeDiffTarget, lags[l]) %in% SkipCols)) {
                data[, paste0(groupingVars[i],
                              timeDiffTarget, lags[l]) := as.numeric(difftime(
                                get(sortDateName),
                                get(paste0(
                                  groupingVars[i], "TEMP", lags[l]
                                )),
                                units = eval(timeAgg)
                              )), by = get(groupingVars[i])]
                CounterIndicator <- CounterIndicator + 1
                if (Timer) {
                  print(CounterIndicator / runs)
                }
              }
            } else {
              if (!(paste0(groupingVars[i],
                           timeDiffTarget, lags[l]) %in% SkipCols)) {
                data[, paste0(groupingVars[i],
                              timeDiffTarget,
                              lags[l]) := as.numeric(difftime(get(
                                paste0(groupingVars[i], "TEMP", (lags[l - 1]))
                              ),
                              get(
                                paste0(groupingVars[i], "TEMP", lags[l])
                              ),
                              units = eval(timeAgg))), by = get(groupingVars[i])]
                CounterIndicator <- CounterIndicator + 1
                if (Timer) {
                  print(CounterIndicator / runs)
                }
              }
            }
          }
        }

        # Remove temporary lagged dates
        for (l in seq_along(lags)) {
          data[, paste0(groupingVars[i], "TEMP", lags[l]) := NULL]
        }

        # Store new target
        timeTarget <- paste0(groupingVars[i],
                             timeDiffTarget, "1")
      }

      # Define targets
      if (WindowingLag != 0) {
        if (!is.null(timeDiffTarget)) {
          Targets <-
            c(paste0(groupingVars[i], "_LAG_",
                     WindowingLag, "_", Targets),
              timeTarget)
        } else {
          Targets <-
            c(paste0(groupingVars[i], "_LAG_",
                     WindowingLag, "_", Targets))
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
            if (!(paste0(groupingVars[i], statsNames[k], "_",
                         periods[j], "_", t) %in% SkipCols)) {
              data[, paste0(groupingVars[i],
                            statsNames[k], "_",
                            periods[j], "_", t) := zoo::rollapply(
                              get(t), periods[j],
                              statsFUNs[k][[1]], partial = TRUE),
                   by = get(groupingVars[i])]
              CounterIndicator <- CounterIndicator + 1
              if (Timer) {
                print(CounterIndicator / runs)
              }
            }
          }
        }
      }
    }

    # Replace any inf values with NA
    for (col in seq_along(data)) {
      data.table::set(data,
                      j = col,
                      value = replace(data[[col]],
                                      is.infinite(data[[col]]), NA))
    }

    # Turn character columns into factors
    for (col in seq_along(data)) {
      if (is.character(data[[col]])) {
        data.table::set(data, j = col, value = as.factor(data[[col]]))
      }
    }

    # Impute missing values
    if (SimpleImpute) {
      for (j in seq_along(data)) {
        if (is.factor(data[[j]])) {
          data.table::set(data,
                          which(!(data[[j]] %in% levels(data[[j]]))),
                          j, "0")
        } else {
          data.table::set(data,
                          which(is.na(data[[j]])), j, -1)
        }
      }
    }

    # Done!!
    return(data)

  } else {
    if (tolower(Type) == "lag") {
      colVar <- c(sortDateName[1])
      data.table::setorderv(data, colVar, order = 1)
    } else {
      colVar <- c(sortDateName[1])
      data.table::setorderv(data, colVar, order = -1)
    }
    Targets <- targets

    # Lags
    for (l in seq_along(lags)) {
      for (t in Targets) {
        if (!(paste0("LAG_", lags[l], "_", t) %in% SkipCols)) {
          data[, paste0("LAG_",
                        lags[l],
                        "_",
                        t) := data.table::shift(get(t),
                                                n = lags[l],
                                                type = "lag")]
          CounterIndicator <- CounterIndicator + 1
          if (Timer) {
            print(CounterIndicator / runs)
          }
        }
      }
    }

    # Time lags
    if (!is.null(timeDiffTarget)) {
      # Lag the dates first
      for (l in seq_along(lags)) {
        if (!(paste0("TEMP", lags[l]) %in% SkipCols)) {
          data[, paste0("TEMP",
                        lags[l]) := data.table::shift(get(
                          sortDateName),
                          n = lags[l],
                          type = "lag")]
        }
      }

      # Difference the lag dates
      if (WindowingLag != 0) {
        for (l in seq_along(lags)) {
          if (!(paste0(timeDiffTarget, "_", lags[l]) %in% SkipCols) &
              l == 1) {
            data[, paste0(timeDiffTarget,
                          "_",
                          lags[l]) := as.numeric(
                            difftime(get(sortDateName),
                                     get(paste0(
                                       "TEMP", lags[l]
                                     )),
                                     units = eval(timeAgg)))]
            CounterIndicator <- CounterIndicator + 1
            if (Timer) {
              print(CounterIndicator / runs)
            }
          } else {
            data[, paste0(timeDiffTarget,
                          "_",
                          lags[l]) := as.numeric(
                            difftime(get(paste0(
                              "TEMP", lags[l] - 1
                            )),
                            get(paste0(
                              "TEMP", lags[l]
                            )),
                            units = eval(timeAgg)))]
            CounterIndicator <- CounterIndicator + 1
            if (Timer) {
              print(CounterIndicator / runs)
            }
          }
        }
      } else {
        for (l in seq_along(lags)) {
          if (l == 1) {
            if (!(paste0(timeDiffTarget,
                         "_",
                         lags[l]) %in% SkipCols)) {
              data[, paste0(timeDiffTarget,
                            "_",
                            lags[l]) := as.numeric(difftime(
                              get(sortDateName),
                              get(paste0("TEMP", lags[l])),
                              units = eval(timeAgg)
                            ))]
              CounterIndicator <- CounterIndicator + 1
              if (Timer) {
                print(CounterIndicator / runs)
              }
            }
          } else {
            if (!(paste0(timeDiffTarget, "_", lags[l]) %in% SkipCols)) {
              data[, paste0(timeDiffTarget,
                            "_",
                            lags[l]) := as.numeric(
                              difftime(get(paste0(
                                "TEMP", (lags[l - 1])
                              )),
                              get(paste0(
                                "TEMP", lags[l]
                              )),
                              units = eval(timeAgg)))]
              CounterIndicator <- CounterIndicator + 1
              if (Timer) {
                print(CounterIndicator / runs)
              }
            }
          }
        }
      }

      # Remove temporary lagged dates
      for (l in seq_along(lags)) {
        data[, paste0("TEMP", lags[l]) := NULL]
      }

      # Store new target
      timeTarget <- paste0(timeDiffTarget, "_1")
    }

    # Define targets
    if (WindowingLag != 0) {
      if (!is.null(timeDiffTarget)) {
        Targets <-
          c(paste0("LAG_", WindowingLag, "_", Targets),
            timeTarget)
      } else {
        Targets <-
          c(paste0("LAG_", WindowingLag, "_", Targets))
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
          if (!(paste0(statsNames[k],
                       "_",
                       periods[j],
                       "_", t) %in% SkipCols)) {
            data[, paste0(statsNames[k],
                          "_",
                          periods[j],
                          "_",
                          t) := zoo::rollapply(get(t),
                                               periods[j],
                                               statsFUNs[k][[1]],
                                               partial = TRUE)]
            CounterIndicator <- CounterIndicator + 1
            if (Timer) {
              print(CounterIndicator / runs)
            }
          }
        }
      }
    }

    # Replace any inf values with NA
    for (col in seq_along(data)) {
      data.table::set(data,
                      j = col,
                      value = replace(data[[col]],
                                      is.infinite(data[[col]]), NA))
    }

    # Turn character columns into factors
    for (col in seq_along(data)) {
      if (is.character(data[[col]])) {
        data.table::set(data,
                        j = col,
                        value = as.factor(data[[col]]))
      }
    }

    # Impute missing values
    if (SimpleImpute) {
      for (j in seq_along(data)) {
        if (is.factor(data[[j]])) {
          data.table::set(data,
                          which(!(data[[j]] %in% levels(data[[j]]))),
                          j, "0")
        } else {
          data.table::set(data,
                          which(is.na(data[[j]])), j, -1)
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
#' @family Feature Engineering
#' @param data A data.table you want to run the function on
#' @param lags A numeric vector of the specific lags you want to have generated. You must include 1 if WindowingLag = 1.
#' @param periods A numeric vector of the specific rolling statistics window sizes you want to utilize in the calculations.
#' @param statsNames A character vector of the corresponding names to create for the rollings stats variables.
#' @param targets A character vector of the column names for the reference column in which you will build your lags and rolling stats
#' @param groupingVars A character vector of categorical variable names you will build your lags and rolling stats by
#' @param sortDateName The column name of your date column used to sort events over time
#' @param timeDiffTarget Specify a desired name for features created for time between events. Set to NULL if you don't want time between events features created.
#' @param timeAgg List the time aggregation level for the time between events features, such as "hour", "day", "week", "month", "quarter", or "year"
#' @param WindowingLag Set to 0 to build rolling stats off of target columns directly or set to 1 to build the rolling stats off of the lag-1 target
#' @param Type List either "Lag" if you want features built on historical values or "Lead" if you want features built on future values
#' @param Timer Set to TRUE if you percentage complete tracker printout
#' @param SkipCols Defaults to NULL; otherwise supply a character vector of the names of columns to skip
#' @param SimpleImpute Set to TRUE for factor level imputation of "0" and numeric imputation of -1
#' @return data.table of original data plus created lags, rolling stats, and time between event lags and rolling stats
#' @examples
#' N = 25116
#' data <- data.table::data.table(DateTime = as.Date(Sys.time()),
#'                                Target = stats::filter(rnorm(N,
#'                                                             mean = 50,
#'                                                             sd = 20),
#'                                                       filter=rep(1,10),
#'                                                       circular=TRUE))
#' data[, temp := seq(1:N)][, DateTime := DateTime - temp][, temp := NULL]
#' data <- data[order(DateTime)]
#' data <- DT_GDL_Feature_Engineering(data,
#'                                    lags           = c(seq(1,5,1)),
#'                                    periods        = c(3,5,10,15,20,25),
#'                                    statsNames     = c("MA"),
#'                                    targets        = c("Target"),
#'                                    groupingVars   = NULL,
#'                                    sortDateName   = "DateTime",
#'                                    timeDiffTarget = c("Time_Gap"),
#'                                    timeAgg        = c("days"),
#'                                    WindowingLag   = 1,
#'                                    Type           = "Lag",
#'                                    Timer          = TRUE,
#'                                    SkipCols       = FALSE,
#'                                    SimpleImpute   = TRUE)
#' @export
DT_GDL_Feature_Engineering <- function(data,
                                       lags           = c(seq(1,50,1)),
                                       periods        = c(seq(5,95,5)),
                                       statsNames     = c("MA"),
                                       targets        = c("qty"),
                                       groupingVars   = c("Group1",
                                                          "Group2"),
                                       sortDateName   = c("date"),
                                       timeDiffTarget = c("TimeDiffName"),
                                       timeAgg        = c("days"),
                                       WindowingLag   = 0,
                                       Type           = c("Lag"),
                                       Timer          = TRUE,
                                       SkipCols       = NULL,
                                       SimpleImpute   = TRUE) {

  # Ensure packages are available
  requireNamespace('data.table', quietly = FALSE)

  # Convert to data.table if not already
  if (!data.table::is.data.table(data))
    data <- data.table::as.data.table(data)

  # Ensure target is numeric
  data[, eval(targets) := as.numeric(get(targets))]

  # Set up counter for countdown
  CounterIndicator <- 0
  if (!is.null(timeDiffTarget)) {
    tarNum <- length(targets) + 1
  } else {
    tarNum <- length(targets)
  }

  # Define total runs
  if (!is.null(groupingVars)) {
    runs <-
      length(groupingVars) * tarNum * (length(periods) *
                                         length(statsNames) +
                                         length(lags))
  } else {
    runs <-
      tarNum * (length(periods) * length(statsNames) +
                  length(lags))
  }

  # Begin feature engineering
  if (!is.null(groupingVars)) {
    for (i in seq_along(groupingVars)) {
      Targets <- targets

      # Sort data
      if (tolower(Type) == "lag") {
        colVar <- c(groupingVars[i], sortDateName[1])
        data.table::setorderv(data, colVar, order = 1)
      } else {
        colVar <- c(groupingVars[i], sortDateName[1])
        data.table::setorderv(data, colVar, order = -1)
      }

      # Lags
      for (l in seq_along(lags)) {
        for (t in Targets) {
          if (!(paste0(groupingVars[i],
                       "_LAG_",
                       lags[l],
                       "_",
                       t) %in% SkipCols)) {
            data[, paste0(groupingVars[i],
                          "_LAG_",
                          lags[l],
                          "_",
                          t) := data.table::shift(get(t),
                                                  n = lags[l],
                                                  type = "lag"),
                 by = get(groupingVars[i])]
            CounterIndicator <- CounterIndicator + 1
            if (Timer) {
              print(CounterIndicator / runs)
            }
          }
        }
      }

      # Time lags
      if (!is.null(timeDiffTarget)) {
        # Lag the dates first
        for (l in seq_along(lags)) {
          if (!(paste0(groupingVars[i], "TEMP", lags[l]) %in% SkipCols)) {
            data[, paste0(groupingVars[i],
                          "TEMP",
                          lags[l]) := data.table::shift(get(
                            sortDateName),
                            n = lags[l],
                            type = "lag"),
                 by = get(groupingVars[i])]
          }
        }

        # Difference the lag dates
        if (WindowingLag != 0) {
          for (l in seq_along(lags)) {
            if (!(paste0(timeDiffTarget,
                         lags[l]) %in% SkipCols) & l == 1) {
              data[, paste0(groupingVars[i],
                            timeDiffTarget,
                            lags[l]) := as.numeric(difftime(
                              get(sortDateName),
                              get(paste0(
                                groupingVars[i], "TEMP", lags[l]
                              )),
                              units = eval(timeAgg)
                            )), by = get(groupingVars[i])]
              CounterIndicator <- CounterIndicator + 1
              if (Timer) {
                print(CounterIndicator / runs)
              }
            } else {
              if (!(paste0(groupingVars[i],
                           timeDiffTarget,
                           lags[l]) %in% SkipCols)) {
                data[, paste0(groupingVars[i],
                              timeDiffTarget,
                              lags[l]) := as.numeric(difftime(get(
                                paste0(groupingVars[i], "TEMP", (lags[l - 1]))
                              ),
                              get(
                                paste0(groupingVars[i], "TEMP", lags[l])
                              ),
                              units = eval(timeAgg))), by = get(groupingVars[i])]
                CounterIndicator <- CounterIndicator + 1
                if (Timer) {
                  print(CounterIndicator / runs)
                }
              }
            }
          }
        } else {
          for (l in seq_along(lags)) {
            if (l == 1) {
              if (!(paste0(groupingVars[i],
                           timeDiffTarget,
                           lags[l]) %in% SkipCols)) {
                data[, paste0(groupingVars[i],
                              timeDiffTarget,
                              lags[l]) := as.numeric(difftime(
                                get(sortDateName),
                                get(paste0(
                                  groupingVars[i], "TEMP", lags[l]
                                )),
                                units = eval(timeAgg)
                              )), by = get(groupingVars[i])]
                CounterIndicator <- CounterIndicator + 1
                if (Timer) {
                  print(CounterIndicator / runs)
                }
              }
            } else {
              if (!(paste0(groupingVars[i],
                           timeDiffTarget,
                           lags[l]) %in% SkipCols)) {
                data[, paste0(groupingVars[i],
                              timeDiffTarget,
                              lags[l]) := as.numeric(difftime(get(
                                paste0(groupingVars[i], "TEMP", (lags[l - 1]))
                              ),
                              get(
                                paste0(groupingVars[i], "TEMP", lags[l])
                              ),
                              units = eval(timeAgg))), by = get(groupingVars[i])]
                CounterIndicator <- CounterIndicator + 1
                if (Timer) {
                  print(CounterIndicator / runs)
                }
              }
            }
          }
        }

        # Remove temporary lagged dates
        for (l in seq_along(lags)) {
          data[, paste0(groupingVars[i], "TEMP",
                        lags[l]) := NULL]
        }

        # Store new target
        timeTarget <- paste0(groupingVars[i],
                             timeDiffTarget, "1")
      }

      # Define targets
      if (WindowingLag != 0) {
        if (!is.null(timeDiffTarget)) {
          Targets <-
            c(paste0(groupingVars[i],
                     "_LAG_",
                     WindowingLag,
                     "_",
                     Targets),
              timeTarget)
        } else {
          Targets <-
            c(paste0(groupingVars[i],
                     "_LAG_",
                     WindowingLag,
                     "_",
                     Targets))
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
            if (!(paste0(groupingVars[i],
                         statsNames[k],
                         "_",
                         periods[j], "_", t) %in% SkipCols)) {
              data[, paste0(groupingVars[i],
                            statsNames[k],
                            "_",
                            periods[j],
                            "_",
                            t) := data.table::frollmean(
                              x = get(t),
                              n = periods[j],
                              fill = NA,
                              algo = "fast",
                              align = "right",
                              na.rm = TRUE,
                              hasNA = TRUE,
                              adaptive = FALSE
                            ),
                   by = get(groupingVars[i])]
              CounterIndicator <- CounterIndicator + 1
              if (Timer) {
                print(CounterIndicator / runs)
              }
            }
          }
        }
      }
    }

    # Replace any inf values with NA
    for (col in seq_along(data)) {
      data.table::set(data,
                      j = col,
                      value = replace(data[[col]],
                                      is.infinite(data[[col]]), NA))
    }

    # Turn character columns into factors
    for (col in seq_along(data)) {
      if (is.character(data[[col]])) {
        data.table::set(data, j = col, value = as.factor(data[[col]]))
      }
    }

    # Impute missing values
    if (SimpleImpute) {
      for (j in seq_along(data)) {
        if (is.factor(data[[j]])) {
          data.table::set(data,
                          which(!(data[[j]] %in% levels(data[[j]]))), j, "0")
        } else {
          data.table::set(data,
                          which(is.na(data[[j]])), j, -1)
        }
      }
    }

    # Done!!
    print(CounterIndicator)
    return(data)

  } else {
    if (tolower(Type) == "lag") {
      colVar <- c(sortDateName[1])
      data.table::setorderv(data, colVar, order = 1)
    } else {
      colVar <- c(sortDateName[1])
      data.table::setorderv(data, colVar, order = -1)
    }
    Targets <- targets

    # Lags
    for (l in seq_along(lags)) {
      for (t in Targets) {
        if (!(paste0("LAG_", lags[l], "_", t) %in% SkipCols)) {
          data[, paste0("LAG_", lags[l], "_", t) := data.table::shift(
            get(t), n = lags[l], type = "lag")]
          CounterIndicator <- CounterIndicator + 1
          if (Timer) {
            print(CounterIndicator / runs)
          }
        }
      }
    }

    # Time lags
    if (!is.null(timeDiffTarget)) {
      # Lag the dates first
      for (l in seq_along(lags)) {
        if (!(paste0("TEMP", lags[l]) %in% SkipCols)) {
          data[, paste0("TEMP",
                        lags[l]) := data.table::shift(get(
                          sortDateName), n = lags[l], type = "lag")]
        }
      }

      # Difference the lag dates
      if (WindowingLag != 0) {
        for (l in seq_along(lags)) {
          if (!(paste0(timeDiffTarget, "_", lags[l]) %in% SkipCols) &
              l == 1) {
            data[, paste0(timeDiffTarget,
                          "_",
                          lags[l]) := as.numeric(difftime(
                            get(sortDateName),
                            get(paste0(
                              "TEMP", lags[l]
                            )),
                            units = eval(timeAgg)))]
            CounterIndicator <- CounterIndicator + 1
            if (Timer) {
              print(CounterIndicator / runs)
            }
          } else {
            data[, paste0(timeDiffTarget,
                          "_", lags[l]) := as.numeric(difftime(
                            get(paste0(
                              "TEMP", lags[l] - 1
                            )),
                            get(paste0(
                              "TEMP", lags[l]
                            )),
                            units = eval(timeAgg)))]
            CounterIndicator <- CounterIndicator + 1
            if (Timer) {
              print(CounterIndicator / runs)
            }
          }
        }
      } else {
        for (l in seq_along(lags)) {
          if (l == 1) {
            if (!(paste0(timeDiffTarget,
                         "_", lags[l]) %in% SkipCols)) {
              data[, paste0(timeDiffTarget,
                            "_", lags[l]) := as.numeric(difftime(
                              get(sortDateName),
                              get(paste0("TEMP", lags[l])),
                              units = eval(timeAgg)
                            ))]
              CounterIndicator <- CounterIndicator + 1
              if (Timer) {
                print(CounterIndicator / runs)
              }
            }
          } else {
            if (!(paste0(timeDiffTarget,
                         "_",
                         lags[l]) %in% SkipCols)) {
              data[, paste0(timeDiffTarget,
                            "_",
                            lags[l]) := as.numeric(difftime(get(paste0(
                              "TEMP", (lags[l - 1])
                            )),
                            get(paste0(
                              "TEMP", lags[l]
                            )),
                            units = eval(timeAgg)))]
              CounterIndicator <- CounterIndicator + 1
              if (Timer) {
                print(CounterIndicator / runs)
              }
            }
          }
        }
      }

      # Remove temporary lagged dates
      for (l in seq_along(lags)) {
        data[, paste0("TEMP", lags[l]) := NULL]
      }

      # Store new target
      timeTarget <- paste0(timeDiffTarget, "_1")
    }

    # Define targets
    if (WindowingLag != 0) {
      if (!is.null(timeDiffTarget)) {
        Targets <-
          c(paste0("LAG_", WindowingLag, "_", Targets),
            timeTarget)
      } else {
        Targets <-
          c(paste0("LAG_", WindowingLag, "_", Targets))
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
          if (!(paste0(statsNames[k],
                       "_",
                       periods[j],
                       "_", t) %in% SkipCols)) {
            data[, paste0(statsNames[k],
                          "_",
                          periods[j],
                          "_", t) := data.table::frollmean(
                            x = get(t),
                            n = periods[j],
                            fill = NA,
                            algo = "fast",
                            align = "right",
                            na.rm = TRUE,
                            hasNA = TRUE,
                            adaptive = FALSE
                          )]
            CounterIndicator <- CounterIndicator + 1
            if (Timer) {
              print(CounterIndicator / runs)
            }
          }
        }
      }
    }

    # Replace any inf values with NA
    for (col in seq_along(data)) {
      data.table::set(data,
                      j = col,
                      value = replace(data[[col]],
                                      is.infinite(data[[col]]), NA))
    }

    # Turn character columns into factors
    for (col in seq_along(data)) {
      if (is.character(data[[col]])) {
        data.table::set(data,
                        j = col,
                        value = as.factor(data[[col]]))
      }
    }

    # Impute missing values
    if (SimpleImpute) {
      for (j in seq_along(data)) {
        if (is.factor(data[[j]])) {
          data.table::set(data,
                          which(!(data[[j]] %in% levels(data[[j]]))), j, "0")
        } else {
          data.table::set(data,
                          which(is.na(data[[j]])), j, -1)
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
#' @family Feature Engineering
#' @param data A data.table you want to run the function on
#' @param lags A numeric vector of the specific lags you want to have generated. You must include 1 if WindowingLag = 1.
#' @param periods A numeric vector of the specific rolling statistics window sizes you want to utilize in the calculations.
#' @param statsFUNs Vector of functions for your rolling windows, such as mean, sd, min, max, quantile
#' @param statsNames A character vector of the corresponding names to create for the rollings stats variables.
#' @param targets A character vector of the column names for the reference column in which you will build your lags and rolling stats
#' @param groupingVars A character vector of categorical variable names you will build your lags and rolling stats by
#' @param sortDateName The column name of your date column used to sort events over time
#' @param timeDiffTarget Specify a desired name for features created for time between events. Set to NULL if you don't want time between events features created.
#' @param timeAgg List the time aggregation level for the time between events features, such as "hour", "day", "week", "month", "quarter", or "year"
#' @param WindowingLag Set to 0 to build rolling stats off of target columns directly or set to 1 to build the rolling stats off of the lag-1 target
#' @param Type List either "Lag" if you want features built on historical values or "Lead" if you want features built on future values
#' @param Timer Set to TRUE if you percentage complete tracker printout
#' @param SkipCols Defaults to NULL; otherwise supply a character vector of the names of columns to skip
#' @param SimpleImpute Set to TRUE for factor level imputation of "0" and numeric imputation of -1
#' @param AscRowByGroup Required to have a column with a Row Number by group (if grouping) with 1 being the record for scoring (typically the most current in time)
#' @param RecordsKeep List the number of records to retain (1 for last record, 2 for last 2 records, etc.)
#' @return data.table of original data plus created lags, rolling stats, and time between event lags and rolling stats
#' @examples
#' N = 25116
#' data1 <- data.table::data.table(DateTime = as.Date(Sys.time()),
#'                                 Target = stats::filter(rnorm(N,
#'                                                              mean = 50,
#'                                                              sd = 20),
#'                                                        filter=rep(1,10),
#'                                                        circular=TRUE))
#' data1[, temp := seq(1:N)][, DateTime := DateTime - temp]
#' data1 <- data1[order(DateTime)]
#' data1 <- Scoring_GDL_Feature_Engineering(data1,
#'                                          lags           = c(seq(1,5,1)),
#'                                          periods        = c(3,5,10,15,20,25),
#'                                          statsFUNs      = c(function(x) mean(x,na.rm = TRUE)),
#'                                          statsNames     = c("MA"),
#'                                          targets        = c("Target"),
#'                                          groupingVars   = NULL,
#'                                          sortDateName   = c("DateTime"),
#'                                          timeDiffTarget = c("Time_Gap"),
#'                                          timeAgg        = "days",
#'                                          WindowingLag   = 1,
#'                                          Type           = "Lag",
#'                                          Timer          = TRUE,
#'                                          SkipCols       = FALSE,
#'                                          SimpleImpute   = TRUE,
#'                                          AscRowByGroup  = "temp",
#'                                          RecordsKeep    = 1)
#' @export
Scoring_GDL_Feature_Engineering <- function(data,
                                            lags           = c(seq(1,5,1)),
                                            periods        = c(3,5,10,15,20,25),
                                            statsFUNs      = c(function(x)
                                              mean(x,na.rm = TRUE)),
                                            statsNames     = c("MA"),
                                            targets        = c("Target"),
                                            groupingVars   = NULL,
                                            sortDateName   = c("DateTime"),
                                            timeDiffTarget = c("Time_Gap"),
                                            timeAgg        = "days",
                                            WindowingLag   = 1,
                                            Type           = "Lag",
                                            Timer          = TRUE,
                                            SkipCols       = FALSE,
                                            SimpleImpute   = TRUE,
                                            AscRowByGroup  = "temp",
                                            RecordsKeep    = 1) {

  # Ensure packages are available
  requireNamespace('data.table', quietly = FALSE)

  # Convert to data.table if not already
  if (!data.table::is.data.table(data))
    data <- data.table::as.data.table(data)

  # Max data to keep
  MAX_RECORDS_FULL <-
    max(max(lags + 1), max(periods + 1), RecordsKeep)
  MAX_RECORDS_LAGS <- max(max(lags + 1), RecordsKeep)
  MAX_RECORDS_ROLL <- max(max(periods + 1), RecordsKeep)

  # Set up counter for countdown
  CounterIndicator <- 0
  if (!is.null(timeDiffTarget)) {
    tarNum <- length(targets) + 1
  } else {
    tarNum <- length(targets)
  }

  # Define total runs
  if (!is.null(groupingVars)) {
    runs <-
      length(groupingVars) * tarNum * (length(periods) *
                                         length(statsNames) +
                                         length(lags))
  } else {
    runs <-
      tarNum * (length(periods) * length(statsNames) +
                  length(lags))
  }

  # Begin feature engineering
  if (!is.null(groupingVars)) {
    for (i in seq_along(groupingVars)) {
      Targets <- targets
      # Sort data
      if (tolower(Type) == "lag") {
        colVar <- c(groupingVars[i], sortDateName[1])
        data.table::setorderv(data, colVar, order = 1)
      } else {
        colVar <- c(groupingVars[i], sortDateName[1])
        data.table::setorderv(data, colVar, order = -1)
      }

      # Remove records
      tempData <- data[get(AscRowByGroup) <= MAX_RECORDS_FULL]

      # Lags
      for (l in seq_along(lags)) {
        for (t in Targets) {
          if (!(paste0(groupingVars[i],
                       "_LAG_", lags[l], "_", t) %in% SkipCols)) {
            tempData[, paste0(groupingVars[i],
                              "_LAG_",
                              lags[l], "_", t) := data.table::shift(get(
                                t), n = lags[l], type = "lag"),
                     by = get(groupingVars[i])]
            CounterIndicator <- CounterIndicator + 1
            if (Timer) {
              print(CounterIndicator / runs)
            }
          }
        }
      }

      # Time lags
      if (!is.null(timeDiffTarget)) {
        # Lag the dates first
        for (l in seq_along(lags)) {
          if (!(paste0(groupingVars[i], "TEMP", lags[l]) %in% SkipCols)) {
            tempData[, paste0(groupingVars[i],
                              "TEMP",
                              lags[l]) := data.table::shift(get(
                                sortDateName),
                                n = lags[l],
                                type = "lag"), by = get(groupingVars[i])]
          }
        }

        # Difference the lag dates
        if (WindowingLag != 0) {
          for (l in seq_along(lags)) {
            if (!(paste0(timeDiffTarget,
                         lags[l]) %in% SkipCols) & l == 1) {
              tempData[, paste0(groupingVars[i],
                                timeDiffTarget,
                                lags[l]) := as.numeric(difftime(
                                  get(sortDateName),
                                  get(paste0(
                                    groupingVars[i], "TEMP", lags[l]
                                  )),
                                  units = eval(timeAgg)
                                )), by = get(groupingVars[i])]
              CounterIndicator <- CounterIndicator + 1
              if (Timer) {
                print(CounterIndicator / runs)
              }
            } else {
              if (!(paste0(groupingVars[i],
                           timeDiffTarget, lags[l]) %in% SkipCols)) {
                tempData[, paste0(groupingVars[i],
                                  timeDiffTarget,
                                  lags[l]) := as.numeric(difftime(get(
                                    paste0(groupingVars[i], "TEMP", (lags[l - 1]))
                                  ),
                                  get(
                                    paste0(groupingVars[i], "TEMP", lags[l])
                                  ),
                                  units = eval(timeAgg))), by = get(groupingVars[i])]
                CounterIndicator <- CounterIndicator + 1
                if (Timer) {
                  print(CounterIndicator / runs)
                }
              }
            }
          }
        } else {
          for (l in seq_along(lags)) {
            if (l == 1) {
              if (!(paste0(groupingVars[i],
                           timeDiffTarget, lags[l]) %in% SkipCols)) {
                tempData[, paste0(groupingVars[i],
                                  timeDiffTarget,
                                  lags[l]) := as.numeric(difftime(
                                    get(sortDateName),
                                    get(paste0(
                                      groupingVars[i], "TEMP", lags[l]
                                    )),
                                    units = eval(timeAgg)
                                  )), by = get(groupingVars[i])]
                CounterIndicator <- CounterIndicator + 1
                if (Timer) {
                  print(CounterIndicator / runs)
                }
              }
            } else {
              if (!(paste0(groupingVars[i],
                           timeDiffTarget, lags[l]) %in% SkipCols)) {
                tempData[, paste0(groupingVars[i],
                                  timeDiffTarget,
                                  lags[l]) := as.numeric(difftime(get(
                                    paste0(groupingVars[i], "TEMP", (lags[l - 1]))
                                  ),
                                  get(
                                    paste0(groupingVars[i], "TEMP", lags[l])
                                  ),
                                  units = eval(timeAgg))), by = get(groupingVars[i])]
                CounterIndicator <- CounterIndicator + 1
                if (Timer) {
                  print(CounterIndicator / runs)
                }
              }
            }
          }
        }

        # Remove temporary lagged dates
        for (l in seq_along(lags)) {
          tempData[, paste0(groupingVars[i], "TEMP", lags[l]) := NULL]
        }

        # Store new target
        timeTarget <- paste0(groupingVars[i], timeDiffTarget, "1")
      }

      # Define targets
      if (WindowingLag != 0) {
        if (!is.null(timeDiffTarget)) {
          Targets <-
            c(paste0(groupingVars[i], "_LAG_", WindowingLag, "_", Targets),
              timeTarget)
        } else {
          Targets <-
            c(paste0(groupingVars[i], "_LAG_", WindowingLag, "_", Targets))
        }
      } else {
        if (!is.null(timeDiffTarget)) {
          Targets <- c(Targets, timeTarget)
        } else {
          Targets <- Targets
        }
      }

      # Keep final values
      tempData1 <- tempData[get(AscRowByGroup) <= eval(RecordsKeep)]

      # Moving stats
      for (j in seq_along(periods)) {
        for (k in seq_along(statsNames)) {
          for (t in Targets) {
            if (!(paste0(groupingVars[i],
                         statsNames[k],
                         "_",
                         periods[j], "_", t) %in% SkipCols)) {
              keep <- c(groupingVars[i], t, AscRowByGroup)
              temp2 <-
                tempData[get(AscRowByGroup) <= MAX_RECORDS_ROLL][, ..keep]
              temp3 <-
                temp2[, paste0(groupingVars[i],
                               statsNames[k],
                               "_",
                               periods[j],
                               "_",
                               t) := lapply(.SD,
                                            statsFUNs[k][[1]]),
                      by = get(groupingVars[i]), .SDcols = eval(t)]
              if (Timer) {
                CounterIndicator <- CounterIndicator + 1
                print(CounterIndicator / runs)
              }
              # Merge files
              temp4 <-
                temp3[get(AscRowByGroup) <=
                        eval(RecordsKeep)][, c(eval(t)) := NULL]
              tempData1 <-
                merge(tempData1,
                      temp4,
                      by = c(eval(groupingVars[i]), eval(AscRowByGroup)))
            }
          }
        }
      }
    }

    # Replace any inf values with NA
    for (col in seq_along(tempData1)) {
      data.table::set(tempData1,
                      j = col,
                      value = replace(tempData1[[col]],
                                      is.infinite(tempData1[[col]]), NA))
    }

    # Turn character columns into factors
    for (col in seq_along(tempData1)) {
      if (is.character(tempData1[[col]])) {
        data.table::set(tempData1,
                        j = col,
                        value = as.factor(tempData1[[col]]))
      }
    }

    # Impute missing values
    if (SimpleImpute) {
      for (j in seq_along(tempData1)) {
        if (is.factor(tempData1[[j]])) {
          data.table::set(tempData1, which(!(
            tempData1[[j]] %in% levels(tempData1[[j]])
          )), j, "0")
        } else {
          data.table::set(tempData1,
                          which(is.na(tempData1[[j]])),
                          j, -1)
        }
      }
    }

    # Done!!
    return(tempData1)

  } else {
    # Sort data
    if (tolower(Type) == "lag") {
      colVar <- c(sortDateName[1])
      data.table::setorderv(data, colVar, order = 1)
    } else {
      colVar <- c(sortDateName[1])
      data.table::setorderv(data, colVar, order = -1)
    }
    Targets <- targets

    # Remove records
    tempData <- data[get(AscRowByGroup) <= MAX_RECORDS_FULL]

    # Lags
    for (l in seq_along(lags)) {
      for (t in Targets) {
        if (!(paste0("LAG_", lags[l], "_", t) %in% SkipCols)) {
          tempData[, paste0("LAG_",
                            lags[l],
                            "_",
                            t) := data.table::shift(get(
                              t), n = lags[l], type = "lag")]
          CounterIndicator <- CounterIndicator + 1
          if (Timer) {
            print(CounterIndicator / runs)
          }
        }
      }
    }

    # Time lags
    if (!is.null(timeDiffTarget)) {
      # Lag the dates first
      for (l in seq_along(lags)) {
        if (!(paste0("TEMP", lags[l]) %in% SkipCols)) {
          tempData[, paste0("TEMP", lags[l]) := data.table::shift(get(
            sortDateName), n = lags[l], type = "lag")]
        }
      }

      # Difference the lag dates
      if (WindowingLag != 0) {
        for (l in seq_along(lags)) {
          if (!(paste0(timeDiffTarget, "_", lags[l]) %in% SkipCols) &
              l == 1) {
            tempData[, paste0(timeDiffTarget,
                              "_",
                              lags[l]) := as.numeric(difftime(get(
                                sortDateName),
                                get(paste0(
                                  "TEMP", lags[l]
                                )),
                                units = eval(timeAgg)))]
            CounterIndicator <- CounterIndicator + 1
            if (Timer) {
              print(CounterIndicator / runs)
            }
          } else {
            tempData[, paste0(timeDiffTarget,
                              "_",
                              lags[l]) := as.numeric(difftime(get(paste0(
                                "TEMP", lags[l] - 1
                              )),
                              get(paste0(
                                "TEMP", lags[l]
                              )),
                              units = eval(timeAgg)))]
            CounterIndicator <- CounterIndicator + 1
            if (Timer) {
              print(CounterIndicator / runs)
            }
          }
        }
      } else {
        for (l in seq_along(lags)) {
          if (l == 1) {
            if (!(paste0(timeDiffTarget, "_", lags[l]) %in% SkipCols)) {
              tempData[, paste0(timeDiffTarget,
                                "_",
                                lags[l]) := as.numeric(difftime(
                                  get(sortDateName),
                                  get(paste0("TEMP", lags[l])),
                                  units = eval(timeAgg)
                                ))]
              CounterIndicator <- CounterIndicator + 1
              if (Timer) {
                print(CounterIndicator / runs)
              }
            }
          } else {
            if (!(paste0(timeDiffTarget, "_", lags[l]) %in% SkipCols)) {
              tempData[, paste0(timeDiffTarget,
                                "_",
                                lags[l]) := as.numeric(difftime(get(paste0(
                                  "TEMP", (lags[l - 1])
                                )),
                                get(paste0(
                                  "TEMP", lags[l]
                                )),
                                units = eval(timeAgg)))]
              CounterIndicator <- CounterIndicator + 1
              if (Timer) {
                print(CounterIndicator / runs)
              }
            }
          }
        }
      }

      # Remove temporary lagged dates
      for (l in seq_along(lags)) {
        tempData[, paste0("TEMP", lags[l]) := NULL]
      }

      # Store new target
      timeTarget <- paste0(timeDiffTarget, "_1")
    }

    # Define targets
    if (WindowingLag != 0) {
      if (!is.null(timeDiffTarget)) {
        Targets <-
          c(paste0("LAG_", WindowingLag, "_", Targets),
            timeTarget)
      } else {
        Targets <-
          c(paste0("LAG_", WindowingLag, "_", Targets))
      }
    } else {
      if (!is.null(timeDiffTarget)) {
        Targets <- c(Targets, timeTarget)
      } else {
        Targets <- Targets
      }
    }

    # Keep final values
    tempData1 <- tempData[get(AscRowByGroup) <= eval(RecordsKeep)]

    # Moving stats
    for (j in seq_along(periods)) {
      for (k in seq_along(statsNames)) {
        for (t in Targets) {
          if (!(paste0(statsNames[k],
                       "_", periods[j],
                       "_", t) %in% SkipCols)) {
            keep <- c(t, AscRowByGroup)
            temp2 <-
              tempData[get(AscRowByGroup) <=
                         MAX_RECORDS_FULL][, ..keep]
            temp3 <-
              temp2[, paste0(statsNames[k],
                             "_", periods[j],
                             "_", t) := lapply(.SD,
                                               statsFUNs[k][[1]]),
                    .SDcols = eval(t)]
            if (Timer) {
              CounterIndicator <- CounterIndicator + 1
              print(CounterIndicator / runs)
            }
            # Merge files
            temp4 <-
              temp3[get(AscRowByGroup) <=
                      eval(RecordsKeep)][, c(eval(AscRowByGroup),
                                             eval(t)) := NULL]
            tempData1 <- cbind(tempData1, temp4)
          }
        }
      }
    }

    # Replace any inf values with NA
    for (col in seq_along(tempData1)) {
      data.table::set(tempData1,
                      j = col,
                      value = replace(tempData1[[col]],
                                      is.infinite(tempData1[[col]]), NA))
    }

    # Turn character columns into factors
    for (col in seq_along(tempData1)) {
      if (is.character(tempData1[[col]])) {
        data.table::set(tempData1,
                        j = col,
                        value = as.factor(tempData1[[col]]))
      }
    }

    # Impute missing values
    if (SimpleImpute) {
      for (j in seq_along(tempData1)) {
        if (is.factor(tempData1[[j]])) {
          data.table::set(tempData1, which(!(
            tempData1[[j]] %in% levels(tempData1[[j]])
          )), j, "0")
        } else {
          data.table::set(tempData1,
                          which(is.na(tempData1[[j]])), j, -1)
        }
      }
    }

    # Done!!
    return(tempData1)
  }
}

#' An Fast Automated Feature Engineering Function
#'
#' For models with target variables within the realm of the current time frame but not too far back in time, this function creates autoregressive and rolling stats from target columns and distributed lags and distributed rolling stats for independent features distributed across time. On top of that, you can also create time between instances along with their associated lags and rolling stats. This function works for data with groups and without groups.
#' @author Adrian Antico
#' @family Feature Engineering
#' @param data A data.table you want to run the function on
#' @param lags A numeric vector of the specific lags you want to have generated. You must include 1 if WindowingLag = 1.
#' @param periods A numeric vector of the specific rolling statistics window sizes you want to utilize in the calculations.
#' @param statsFUNs Vector of functions for your rolling windows, such as mean, sd, min, max, quantile
#' @param statsNames A character vector of the corresponding names to create for the rollings stats variables.
#' @param targets A character vector of the column names for the reference column in which you will build your lags and rolling stats
#' @param groupingVars A character vector of categorical variable names you will build your lags and rolling stats by
#' @param sortDateName The column name of your date column used to sort events over time
#' @param timeDiffTarget Specify a desired name for features created for time between events. Set to NULL if you don't want time between events features created.
#' @param timeAgg List the time aggregation level for the time between events features, such as "hour", "day", "week", "month", "quarter", or "year"
#' @param WindowingLag Set to 0 to build rolling stats off of target columns directly or set to 1 to build the rolling stats off of the lag-1 target
#' @param Type List either "Lag" if you want features built on historical values or "Lead" if you want features built on future values
#' @param Timer Set to TRUE if you percentage complete tracker printout
#' @param SkipCols Defaults to NULL; otherwise supply a character vector of the names of columns to skip
#' @param SimpleImpute Set to TRUE for factor level imputation of "0" and numeric imputation of -1
#' @param AscRowByGroup Required to have a column with a Row Number by group (if grouping) with 1 being the record for scoring (typically the most current in time)
#' @param RecordsKeep List the number of records to retain (1 for last record, 2 for last 2 records, etc.)
#' @return data.table of original data plus created lags, rolling stats, and time between event lags and rolling stats
#' @examples
#' N = 25116
#' data <- data.table::data.table(DateTime = as.Date(Sys.time()),
#'   Target = stats::filter(rnorm(N,
#'                                mean = 50,
#'                                sd = 20),
#'                          filter=rep(1,10),
#'                          circular=TRUE))
#' data[, temp := seq(1:N)][, DateTime := DateTime - temp]
#' data <- data[order(DateTime)]
#' data <- FAST_GDL_Feature_Engineering(data,
#'                                      lags           = c(1:5),
#'                                      periods        = c(seq(10,50,10)),
#'                                      statsFUNs      = c("mean",
#'                                                         "median",
#'                                                         "sd",
#'                                                         "quantile85",
#'                                                         "quantile95"),
#'                                      statsNames     = c("mean",
#'                                                         "median",
#'                                                         "sd",
#'                                                         "quantile85",
#'                                                         "quantile95"),
#'                                      targets        = c("Target"),
#'                                      groupingVars   = NULL,
#'                                      sortDateName   = "DateTime",
#'                                      timeDiffTarget = c("Time_Gap"),
#'                                      timeAgg        = "days",
#'                                      WindowingLag   = 1,
#'                                      Type           = "Lag",
#'                                      Timer          = TRUE,
#'                                      SkipCols       = FALSE,
#'                                      SimpleImpute   = TRUE,
#'                                      AscRowByGroup  = "temp")
#' @export
FAST_GDL_Feature_Engineering <- function(data,
                                         lags           = c(1:5),
                                         periods        = c(seq(10,50,10)),
                                         statsFUNs      = c("mean",
                                                            "median",
                                                            "sd",
                                                            "quantile85",
                                                            "quantile95"),
                                         statsNames     = c("mean",
                                                            "median",
                                                            "sd",
                                                            "quantile85",
                                                            "quantile95"),
                                         targets        = c("Target"),
                                         groupingVars   = c("GroupVariable"),
                                         sortDateName   = c("DateTime"),
                                         timeDiffTarget = NULL,
                                         timeAgg        = c("hours"),
                                         WindowingLag   = 1,
                                         Type           = c("Lag"),
                                         Timer          = FALSE,
                                         SkipCols       = FALSE,
                                         SimpleImpute   = TRUE,
                                         AscRowByGroup  = c("temp"),
                                         RecordsKeep    = 1) {

  # Ensure packages are available
  requireNamespace('data.table', quietly = FALSE)

  # Convert to data.table if not already
  if (!data.table::is.data.table(data))
    data <- data.table::as.data.table(data)

  # Max data to keep
  MAX_RECORDS_FULL <-
    max(max(lags + 1), max(periods + 1), RecordsKeep)
  MAX_RECORDS_LAGS <- max(max(lags + 1), RecordsKeep)
  MAX_RECORDS_ROLL <- max(max(periods + 1), RecordsKeep)

  # Set up counter for countdown
  CounterIndicator <- 0
  if (!is.null(timeDiffTarget)) {
    tarNum <- length(targets) + 1
  } else {
    tarNum <- length(targets)
  }

  # Define total runs
  if (!is.null(groupingVars)) {
    runs <-
      length(groupingVars) * tarNum * (length(periods) *
                                         length(statsNames) +
                                         length(lags))
  } else {
    runs <-
      tarNum * (length(periods) * length(statsNames) +
                  length(lags))
  }

  # Begin feature engineering
  if (!is.null(groupingVars)) {
    for (i in seq_along(groupingVars)) {
      Targets <- targets
      # Sort data
      if (tolower(Type) == "lag") {
        colVar <- c(groupingVars[i], sortDateName[1])
        data.table::setorderv(data, colVar, order = 1)
      } else {
        colVar <- c(groupingVars[i], sortDateName[1])
        data.table::setorderv(data, colVar, order = -1)
      }

      # Remove records
      tempData <- data[get(AscRowByGroup) <= MAX_RECORDS_FULL]

      # Lags
      for (l in seq_along(lags)) {
        for (t in Targets) {
          if (!(paste0(groupingVars[i],
                       "_LAG_", lags[l], "_", t) %in% SkipCols)) {
            tempData[, paste0(groupingVars[i],
                              "_LAG_",
                              lags[l], "_", t) := data.table::shift(get(
                                t), n = lags[l], type = "lag"),
                     by = get(groupingVars[i])]
            CounterIndicator <- CounterIndicator + 1
            if (Timer) {
              print(CounterIndicator / runs)
            }
          }
        }
      }

      # Time lags
      if (!is.null(timeDiffTarget)) {
        # Lag the dates first
        for (l in seq_along(lags)) {
          if (!(paste0(groupingVars[i], "TEMP", lags[l]) %in% SkipCols)) {
            tempData[, paste0(groupingVars[i],
                              "TEMP",
                              lags[l]) := data.table::shift(get(
                                sortDateName),
                                n = lags[l],
                                type = "lag"), by = get(groupingVars[i])]
          }
        }

        # Difference the lag dates
        if (WindowingLag != 0) {
          for (l in seq_along(lags)) {
            if (!(paste0(timeDiffTarget,
                         lags[l]) %in% SkipCols) & l == 1) {
              tempData[, paste0(groupingVars[i],
                                timeDiffTarget,
                                lags[l]) := as.numeric(difftime(
                                  get(sortDateName),
                                  get(paste0(
                                    groupingVars[i], "TEMP", lags[l]
                                  )),
                                  units = eval(timeAgg)
                                )), by = get(groupingVars[i])]
              CounterIndicator <- CounterIndicator + 1
              if (Timer) {
                print(CounterIndicator / runs)
              }
            } else {
              if (!(paste0(groupingVars[i],
                           timeDiffTarget, lags[l]) %in% SkipCols)) {
                tempData[, paste0(groupingVars[i],
                                  timeDiffTarget,
                                  lags[l]) := as.numeric(difftime(get(
                                    paste0(groupingVars[i],
                                           "TEMP",
                                           (lags[l - 1]))
                                  ),
                                  get(
                                    paste0(groupingVars[i],
                                           "TEMP",
                                           lags[l])
                                  ),
                                  units = eval(timeAgg))),
                         by = get(groupingVars[i])]
                CounterIndicator <- CounterIndicator + 1
                if (Timer) {
                  print(CounterIndicator / runs)
                }
              }
            }
          }
        } else {
          for (l in seq_along(lags)) {
            if (l == 1) {
              if (!(paste0(groupingVars[i],
                           timeDiffTarget, lags[l]) %in% SkipCols)) {
                tempData[, paste0(groupingVars[i],
                                  timeDiffTarget,
                                  lags[l]) := as.numeric(difftime(
                                    get(sortDateName),
                                    get(paste0(
                                      groupingVars[i], "TEMP", lags[l]
                                    )),
                                    units = eval(timeAgg)
                                  )), by = get(groupingVars[i])]
                CounterIndicator <- CounterIndicator + 1
                if (Timer) {
                  print(CounterIndicator / runs)
                }
              }
            } else {
              if (!(paste0(groupingVars[i],
                           timeDiffTarget, lags[l]) %in% SkipCols)) {
                tempData[, paste0(groupingVars[i],
                                  timeDiffTarget,
                                  lags[l]) := as.numeric(difftime(get(
                                    paste0(groupingVars[i],
                                           "TEMP",
                                           (lags[l - 1]))
                                  ),
                                  get(
                                    paste0(groupingVars[i],
                                           "TEMP",
                                           lags[l])
                                  ),
                                  units = eval(timeAgg))),
                         by = get(groupingVars[i])]
                CounterIndicator <- CounterIndicator + 1
                if (Timer) {
                  print(CounterIndicator / runs)
                }
              }
            }
          }
        }

        # Remove temporary lagged dates
        for (l in seq_along(lags)) {
          tempData[, paste0(groupingVars[i], "TEMP", lags[l]) := NULL]
        }

        # Store new target
        timeTarget <- paste0(groupingVars[i], timeDiffTarget, "1")
      }

      # Define targets
      if (WindowingLag != 0) {
        if (!is.null(timeDiffTarget)) {
          Targets <-
            c(paste0(groupingVars[i], "_LAG_", WindowingLag, "_", Targets),
              timeTarget)
        } else {
          Targets <-
            c(paste0(groupingVars[i], "_LAG_", WindowingLag, "_", Targets))
        }
      } else {
        if (!is.null(timeDiffTarget)) {
          Targets <- c(Targets, timeTarget)
        } else {
          Targets <- Targets
        }
      }

      # Keep final values
      tempData1 <- tempData[get(AscRowByGroup) <= eval(RecordsKeep)]

      # Moving stats
      for (j in seq_along(periods)) {
        for (k in seq_along(statsNames)) {
          for (t in Targets) {
            if (!(paste0(groupingVars[i],
                         statsNames[k],
                         "_",
                         periods[j],
                         "_", t) %in% SkipCols)) {
              keep <- c(groupingVars[i], t, AscRowByGroup)
              temp2 <-
                tempData[get(AscRowByGroup) <=
                           MAX_RECORDS_ROLL][, ..keep]
              if (statsFUNs[k] == "mean") {
                temp3 <-
                  temp2[, paste0(groupingVars[i],
                                 statsNames[k],
                                 "_",
                                 periods[j],
                                 "_",
                                 t) := caTools::runmean(
                                   get(t),
                                   k = periods[j],
                                   endrule = "mean",
                                   alg = "C"
                                 ), by = get(groupingVars[i])]
              } else if (statsFUNs[k] == "median") {
                temp3 <-
                  temp2[, paste0(groupingVars[i],
                                 statsNames[k],
                                 "_",
                                 periods[j],
                                 "_",
                                 t) := caTools::runquantile(
                                   get(t),
                                   probs = 0.50,
                                   k = periods[j],
                                   endrule = "quantile"
                                 ), by = get(groupingVars[i])]
              } else if (statsFUNs[k] == "sd") {
                temp3 <-
                  temp2[, paste0(groupingVars[i],
                                 statsNames[k],
                                 "_",
                                 periods[j],
                                 "_",
                                 t) := caTools::runsd(get(t),
                                                      k = periods[j],
                                                      endrule = "sd"),
                        by = get(groupingVars[i])]
              } else if (statsFUNs[k] == "quantile85") {
                temp3 <-
                  temp2[, paste0(groupingVars[i],
                                 statsNames[k],
                                 "_",
                                 periods[j],
                                 "_",
                                 t) := caTools::runquantile(
                                   get(t),
                                   probs = 0.85,
                                   k = periods[j],
                                   endrule = "quantile"
                                 ), by = get(groupingVars[i])]
              } else if (statsFUNs[k] == "quantile95") {
                temp3 <-
                  temp2[, paste0(groupingVars[i],
                                 statsNames[k],
                                 "_",
                                 periods[j],
                                 "_",
                                 t) := caTools::runquantile(
                                   get(t),
                                   probs = 0.95,
                                   k = periods[j],
                                   endrule = "quantile"
                                 ), by = get(groupingVars[i])]
              }
              if (Timer) {
                CounterIndicator <- CounterIndicator + 1
                print(CounterIndicator / runs)
              }
              # Merge files
              temp4 <-
                temp3[get(AscRowByGroup) <= eval(RecordsKeep)][, c(
                  eval(t)) := NULL]
              tempData1 <-
                merge(tempData1, temp4, by = c(eval(groupingVars[i]),
                                               eval(AscRowByGroup)))
            }
          }
        }
      }
    }

    # Replace any inf values with NA
    for (col in seq_along(tempData1)) {
      data.table::set(tempData1,
                      j = col,
                      value = replace(tempData1[[col]],
                                      is.infinite(tempData1[[col]]), NA))
    }

    # Turn character columns into factors
    for (col in seq_along(tempData1)) {
      if (is.character(tempData1[[col]])) {
        data.table::set(tempData1,
                        j = col,
                        value = as.factor(tempData1[[col]]))
      }
    }

    # Impute missing values
    if (SimpleImpute) {
      for (j in seq_along(tempData1)) {
        if (is.factor(tempData1[[j]])) {
          data.table::set(tempData1, which(!(
            tempData1[[j]] %in% levels(tempData1[[j]])
          )), j, "0")
        } else {
          data.table::set(tempData1, which(is.na(tempData1[[j]])), j, -1)
        }
      }
    }

    # Done!!
    return(tempData1)

  } else {
    # Sort data
    if (tolower(Type) == "lag") {
      colVar <- c(sortDateName[1])
      data.table::setorderv(data, colVar, order = 1)
    } else {
      colVar <- c(sortDateName[1])
      data.table::setorderv(data, colVar, order = -1)
    }
    Targets <- targets

    # Remove records
    tempData <- data[get(AscRowByGroup) <= MAX_RECORDS_FULL]

    # Lags
    for (l in seq_along(lags)) {
      for (t in Targets) {
        if (!(paste0("LAG_", lags[l], "_", t) %in% SkipCols)) {
          tempData[, paste0("LAG_", lags[l], "_", t) := data.table::shift(
            get(t), n = lags[l], type = "lag")]
          CounterIndicator <- CounterIndicator + 1
          if (Timer) {
            print(CounterIndicator / runs)
          }
        }
      }
    }

    # Time lags
    if (!is.null(timeDiffTarget)) {
      # Lag the dates first
      for (l in seq_along(lags)) {
        if (!(paste0("TEMP", lags[l]) %in% SkipCols)) {
          tempData[, paste0("TEMP",
                            lags[l]) := data.table::shift(get(
                              sortDateName), n = lags[l], type = "lag")]
        }
      }

      # Difference the lag dates
      if (WindowingLag != 0) {
        for (l in seq_along(lags)) {
          if (!(paste0(timeDiffTarget, "_", lags[l]) %in% SkipCols) &
              l == 1) {
            tempData[, paste0(timeDiffTarget,
                              "_",
                              lags[l]) := as.numeric(difftime(
                                get(sortDateName),
                                get(paste0(
                                  "TEMP", lags[l]
                                )),
                                units = eval(timeAgg)))]
            CounterIndicator <- CounterIndicator + 1
            if (Timer) {
              print(CounterIndicator / runs)
            }
          } else {
            tempData[, paste0(timeDiffTarget,
                              "_", lags[l]) := as.numeric(difftime(
                                get(paste0(
                                  "TEMP", lags[l] - 1
                                )),
                                get(paste0(
                                  "TEMP", lags[l]
                                )),
                                units = eval(timeAgg)))]
            CounterIndicator <- CounterIndicator + 1
            if (Timer) {
              print(CounterIndicator / runs)
            }
          }
        }
      } else {
        for (l in seq_along(lags)) {
          if (l == 1) {
            if (!(paste0(timeDiffTarget,
                         "_", lags[l]) %in% SkipCols)) {
              tempData[, paste0(timeDiffTarget,
                                "_", lags[l]) := as.numeric(difftime(
                                  get(sortDateName),
                                  get(paste0("TEMP", lags[l])),
                                  units = eval(timeAgg)
                                ))]
              CounterIndicator <- CounterIndicator + 1
              if (Timer) {
                print(CounterIndicator / runs)
              }
            }
          } else {
            if (!(paste0(timeDiffTarget,
                         "_",
                         lags[l]) %in% SkipCols)) {
              tempData[, paste0(timeDiffTarget,
                                "_",
                                lags[l]) := as.numeric(difftime(get(paste0(
                                  "TEMP", (lags[l - 1])
                                )),
                                get(paste0(
                                  "TEMP", lags[l]
                                )),
                                units = eval(timeAgg)))]
              CounterIndicator <- CounterIndicator + 1
              if (Timer) {
                print(CounterIndicator / runs)
              }
            }
          }
        }
      }

      # Remove temporary lagged dates
      for (l in seq_along(lags)) {
        tempData[, paste0("TEMP", lags[l]) := NULL]
      }

      # Store new target
      timeTarget <- paste0(timeDiffTarget, "_1")
    }

    # Define targets
    if (WindowingLag != 0) {
      if (!is.null(timeDiffTarget)) {
        Targets <-
          c(paste0("LAG_", WindowingLag, "_", Targets),
            timeTarget)
      } else {
        Targets <-
          c(paste0("LAG_", WindowingLag, "_", Targets))
      }
    } else {
      if (!is.null(timeDiffTarget)) {
        Targets <- c(Targets, timeTarget)
      } else {
        Targets <- Targets
      }
    }

    # Keep final values
    tempData1 <- tempData[get(AscRowByGroup) <= eval(RecordsKeep)]

    # Moving stats
    for (j in seq_along(periods)) {
      for (k in seq_along(statsNames)) {
        for (t in Targets) {
          if (!(paste0(statsNames[k],
                       "_",
                       periods[j],
                       "_",
                       t) %in% SkipCols)) {
            keep <- c(t, AscRowByGroup)
            temp2 <-
              tempData1[get(AscRowByGroup) <= MAX_RECORDS_ROLL][, ..keep]
            if (statsFUNs[k] == "mean") {
              temp3 <-
                temp2[, paste0(statsNames[k],
                               "_",
                               periods[j],
                               "_",
                               t) := caTools::runmean(get(t),
                                                      k = periods[j],
                                                      endrule = "mean",
                                                      alg = "C")]
            } else if (statsFUNs[k] == "median") {
              temp3 <-
                temp2[, paste0(statsNames[k],
                               "_",
                               periods[j],
                               "_",
                               t) := caTools::runquantile(
                                 get(t),
                                 probs = 0.50,
                                 k = periods[j],
                                 endrule = "quantile"
                               )]
            } else if (statsFUNs[k] == "sd") {
              temp3 <-
                temp2[, paste0(statsNames[k],
                               "_",
                               periods[j],
                               "_",
                               t) := caTools::runsd(get(t),
                                                    k = periods[j],
                                                    endrule = "sd")]
            } else if (statsFUNs[k] == "quantile85") {
              temp3 <-
                temp2[, paste0(statsNames[k],
                               "_",
                               periods[j],
                               "_",
                               t) := caTools::runquantile(
                                 get(t),
                                 probs = 0.85,
                                 k = periods[j],
                                 endrule = "quantile"
                               )]
            } else if (statsFUNs[k] == "quantile95") {
              temp3 <-
                temp2[, paste0(statsNames[k],
                               "_",
                               periods[j],
                               "_",
                               t) := caTools::runquantile(
                                 get(t),
                                 probs = 0.95,
                                 k = periods[j],
                                 endrule = "quantile"
                               )]
            }
            if (Timer) {
              CounterIndicator <- CounterIndicator + 1
              print(CounterIndicator / runs)
            }
            # Merge files
            temp4 <-
              temp3[get(AscRowByGroup) <=
                      eval(RecordsKeep)][, c(eval(t)) := NULL]
            tempData1 <-
              merge(tempData1, temp4, by = c(eval(AscRowByGroup)))
          }
        }
      }
    }

    # Replace any inf values with NA
    for (col in seq_along(tempData1)) {
      data.table::set(tempData1,
                      j = col,
                      value = replace(tempData1[[col]],
                                      is.infinite(tempData1[[col]]),
                                      NA))
    }

    # Turn character columns into factors
    for (col in seq_along(tempData1)) {
      if (is.character(tempData1[[col]])) {
        data.table::set(tempData1,
                        j = col,
                        value = as.factor(tempData1[[col]]))
      }
    }

    # Impute missing values
    if (SimpleImpute) {
      for (j in seq_along(tempData1)) {
        if (is.factor(tempData1[[j]])) {
          data.table::set(tempData1,
                          which(!(tempData1[[j]] %in% levels(tempData1[[j]]))),
                          j,
                          "0")
        } else {
          data.table::set(tempData1,
                          which(is.na(tempData1[[j]])),
                          j,
                          -1)
        }
      }
    }

    # Ensure correct order of columns
    setcolorder(tempData1, c(2,3,1,4:ncol(tempData1)))

    # Done!!
    return(tempData1)
  }
}

#' An Automated Machine Learning Framework using H2O
#'
#' Steps in the function include:
#' See details below for information on using this function.
#'
#' 1. Logic: Error checking in the modeling arguments from your Construction file
#'
#' 2. ML: Build grid-tuned models and baseline models for comparison and checks which one performs better on validation data
#'
#' 3. Evaluation: Collects the performance metrics for both
#'
#' 4. Evaluation: Generates calibration plots (and boxplots for regression) for the winning model
#'
#' 5. Evaluation: Generates partial dependence calibration plots (and boxplots for regression) for the winning model
#'
#' 6. Evaluation: Generates variable importance tables and a table of non-important features
#'
#' 7. Production: Creates a storage file containing: model name, model path, grid tune performance, baseline performance, and threshold (if classification) and stores that file in your model_path location
#'
#' The Construct file must be a data.table and the columns need to be in the correct order (see examples). Character columns must be converted to type "Factor". You must remove date columns or convert them to "Factor". For classification models, your target variable needs to be a (0,1) of type "Factor." See the examples below for help with setting up the Construct file for various modeling target variable types. There are examples for regression, classification, multinomial, and quantile regression. For help on which parameters to use, look up the r/h2o documentation. If you misspecify the construct file, it will produce an error and outputfile of what was wrong and suggestions for fixing the error.
#'
#' Let's go over the construct file, column by column. The Targets column is where you specify the column number of your target variable (in quotes, e.g. "c(1)").
#'
#' The Distribution column is where you specify the distribution type for the modeling task. For classification use bernoulli, for multilabel use multinomial, for quantile use quantile, and for regression, you can choose from the list available in the H2O docs, such as gaussian, poisson, gamma, etc. It's not set up to handle tweedie distributions currently but I can add support if there is demand.
#'
#' The Loss column tells H2O which metric to use for the loss metrics. For regression, I typically use "mse", quantile regression, "mae", classification "auc", and multinomial "logloss". For deeplearning models, you need to use "quadratic", "absolute", and "crossentropy".
#'
#' The Quantile column tells H2O which quantile to use for quantile regression (in decimal form).
#'
#' The ModelName column is the name you wish to give your model as a prefix.
#'
#' The Algorithm column is the model you wish to use: gbm, randomForest, deeplearning, AutoML, XGBoost, LightGBM.
#'
#' The dataName column is the name of your data.
#'
#' The TargetCol column is the column number of your target variable.
#'
#' The FeatureCols column is the column numbers of your features.
#'
#' The CreateDate column is for tracking your model build dates.
#'
#' The GridTune column is a TRUE / FALSE column for whether you want to run a grid tune model for comparison.
#'
#' The ExportValidData column is a TRUE / FALSE column indicating if you want to export the validation data.
#'
#' The ParDep column is where you put the number of partial dependence calibration plots you wish to generate.
#'
#' The PD_Data column is where you specify if you want to generate the partial dependence plots on "All" data, "Validate" data, or "Train" data.
#'
#' The ThreshType column is for classification models. You can specify "f1", "f2", "f0point5", or "CS" for cost sentitive.
#'
#' The FSC column is the feature selection column. Specify the percentage importance cutoff to create a table of "unimportant" features.
#'
#' The tpProfit column is for when you specify "CS" in the ThreshType column. This is your true positive profit.
#'
#' The tnProfit column is for when you specify "CS" in the ThreshType column. This is your true negative profit.
#'
#' The fpProfit column is for when you specify "CS" in the ThreshType column. This is your false positive profit.
#'
#' The fnProfit column is for when you specify "CS" in the ThreshType column. This is your false negative profit.
#'
#' The SaveModel column is a TRUE / FALSE indicator. If you are just testing out models, set this to FALSE.
#'
#' The SaveModelType column is where you specify if you want a "standard" model object saveed or a "mojo" model object saved.
#'
#' The PredsAllData column is a TRUE / FALSE column. Set to TRUE if you want all the predicted values returns (for all data).
#'
#' The TargetEncoding column let's you specify the column number of features you wish to run target encoding on. Set to NA to not run this feature.
#'
#' The SupplyData column lets you supply the data names for training and validation data. Set to NULL if you want the data partitioning to be done internally.
#' @author Adrian Antico
#' @family Supervised Learning
#' @param Construct Core instruction file for automation (see Details below for more information on this)
#' @param max_memory The ceiling amount of memory H2O will utilize
#' @param ratios The percentage of train samples from source data (remainder goes to validation set)
#' @param BL_Trees The number of trees to build in baseline GBM or RandomForest
#' @param nthreads Set the number of threads to run function
#' @param model_path Directory path for where you want your models saved
#' @param MaxRuntimeSeconds Number of seconds of run time for grid tuning
#' @param MaxModels Number of models you'd like to have returned
#' @param TrainData Set to NULL or supply a data.table for training data
#' @param TestData Set to NULL or supply  a data.table for validation data
#' @param SaveToFile Set to TRUE to save models and output to model_path
#' @param ReturnObjects Set to TRUE to return objects from functioin
#' @return Returns saved models, corrected Construct file, variable importance tables, evaluation and partial dependence calibration plots, model performance measure, and a file called grid_tuned_paths.Rdata which contains paths to your saved models for operationalization.
#' @examples
#' \donttest{
#' # Classification Example
#' Correl <- 0.85
#' aa <- data.table::data.table(target = runif(1000))
#' aa[, x1 := qnorm(target)]
#' aa[, x2 := runif(1000)]
#' aa[, Independent_Variable1 := log(pnorm(Correl * x1 +
#'                                           sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable2 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable3 := exp(pnorm(Correl * x1 +
#'                                           sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable4 := exp(exp(pnorm(Correl * x1 +
#'                                               sqrt(1-Correl^2) * qnorm(x2))))]
#' aa[, Independent_Variable5 := sqrt(pnorm(Correl * x1 +
#'                                            sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable6 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^0.10]
#' aa[, Independent_Variable7 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^0.25]
#' aa[, Independent_Variable8 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^0.75]
#' aa[, Independent_Variable9 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^2]
#' aa[, Independent_Variable10 := (pnorm(Correl * x1 +
#'                                         sqrt(1-Correl^2) * qnorm(x2)))^4]
#' aa[, ':=' (x1 = NULL, x2 = NULL)]
#' aa[, target := as.factor(ifelse(target > 0.5,1,0))]
#' Construct <- data.table::data.table(Targets = rep("target",3),
#'                                     Distribution    = c("bernoulli",
#'                                                         "bernoulli",
#'                                                         "bernoulli"),
#'                                     Loss            = c("AUC","AUC","CrossEntropy"),
#'                                     Quantile        = rep(NA,3),
#'                                     ModelName       = c("GBM","DRF","DL"),
#'                                     Algorithm       = c("gbm",
#'                                                         "randomForest",
#'                                                         "deeplearning"),
#'                                     dataName        = rep("aa",3),
#'                                     TargetCol       = rep(c("1"),3),
#'                                     FeatureCols     = rep(c("2:11"),3),
#'                                     CreateDate      = rep(Sys.time(),3),
#'                                     GridTune        = rep(FALSE,3),
#'                                     ExportValidData = rep(TRUE,3),
#'                                     ParDep          = rep(2,3),
#'                                     PD_Data         = rep("All",3),
#'                                     ThreshType      = rep("f1",3),
#'                                     FSC             = rep(0.001,3),
#'                                     tpProfit        = rep(NA,3),
#'                                     tnProfit        = rep(NA,3),
#'                                     fpProfit        = rep(NA,3),
#'                                     fnProfit        = rep(NA,3),
#'                                     SaveModel       = rep(FALSE,3),
#'                                     SaveModelType   = c("Mojo","standard","mojo"),
#'                                     PredsAllData    = rep(TRUE,3),
#'                                     TargetEncoding  = rep(NA,3),
#'                                     SupplyData      = rep(FALSE,3))
#' AutoH2OModeler(Construct,
#'                max_memory = "28G",
#'                ratios = 0.75,
#'                BL_Trees = 500,
#'                nthreads = 5,
#'                model_path = NULL,
#'                MaxRuntimeSeconds = 3600,
#'                MaxModels = 30,
#'                TrainData = NULL,
#'                TestData  = NULL,
#'                SaveToFile = FALSE,
#'                ReturnObjects = TRUE)
#'
#' # Multinomial Example
#' Correl <- 0.85
#' aa <- data.table::data.table(target = runif(1000))
#' aa[, x1 := qnorm(target)]
#' aa[, x2 := runif(1000)]
#' aa[, Independent_Variable1 := log(pnorm(Correl * x1 +
#'                                           sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable2 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable3 := exp(pnorm(Correl * x1 +
#'                                           sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable4 := exp(exp(pnorm(Correl * x1 +
#'                                               sqrt(1-Correl^2) * qnorm(x2))))]
#' aa[, Independent_Variable5 := sqrt(pnorm(Correl * x1 +
#'                                            sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable6 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^0.10]
#' aa[, Independent_Variable7 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^0.25]
#' aa[, Independent_Variable8 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^0.75]
#' aa[, Independent_Variable9 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^2]
#' aa[, Independent_Variable10 := (pnorm(Correl * x1 +
#'                                         sqrt(1-Correl^2) * qnorm(x2)))^4]
#' aa[, ':=' (x1 = NULL, x2 = NULL)]
#' aa[, target := as.factor(ifelse(target < 0.33,"A",ifelse(target < 0.66, "B","C")))]
#' Construct <- data.table::data.table(Targets = rep("target",3),
#'                                     Distribution    = c("multinomial",
#'                                                         "multinomial",
#'                                                         "multinomial"),
#'                                     Loss            = c("auc","logloss","accuracy"),
#'                                     Quantile        = rep(NA,3),
#'                                     ModelName       = c("GBM","DRF","DL"),
#'                                     Algorithm       = c("gbm",
#'                                                         "randomForest",
#'                                                         "deeplearning"),
#'                                     dataName        = rep("aa",3),
#'                                     TargetCol       = rep(c("1"),3),
#'                                     FeatureCols     = rep(c("2:11"),3),
#'                                     CreateDate      = rep(Sys.time(),3),
#'                                     GridTune        = rep(FALSE,3),
#'                                     ExportValidData = rep(TRUE,3),
#'                                     ParDep          = rep(NA,3),
#'                                     PD_Data         = rep("All",3),
#'                                     ThreshType      = rep("f1",3),
#'                                     FSC             = rep(0.001,3),
#'                                     tpProfit        = rep(NA,3),
#'                                     tnProfit        = rep(NA,3),
#'                                     fpProfit        = rep(NA,3),
#'                                     fnProfit        = rep(NA,3),
#'                                     SaveModel       = rep(FALSE,3),
#'                                     SaveModelType   = c("Mojo","standard","mojo"),
#'                                     PredsAllData    = rep(TRUE,3),
#'                                     TargetEncoding  = rep(NA,3),
#'                                     SupplyData      = rep(FALSE,3))
#'
#' AutoH2OModeler(Construct,
#'                max_memory = "28G",
#'                ratios = 0.75,
#'                BL_Trees = 500,
#'                nthreads = 5,
#'                model_path = NULL,
#'                MaxRuntimeSeconds = 3600,
#'                MaxModels = 30,
#'                TrainData = NULL,
#'                TestData  = NULL,
#'                SaveToFile = FALSE,
#'                ReturnObjects = TRUE)
#'
#' # Regression Example
#' Correl <- 0.85
#' aa <- data.table::data.table(target = runif(1000))
#' aa[, x1 := qnorm(target)]
#' aa[, x2 := runif(1000)]
#' aa[, Independent_Variable1 := log(pnorm(Correl * x1 +
#'                                           sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable2 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable3 := exp(pnorm(Correl * x1 +
#'                                           sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable4 := exp(exp(pnorm(Correl * x1 +
#'                                               sqrt(1-Correl^2) * qnorm(x2))))]
#' aa[, Independent_Variable5 := sqrt(pnorm(Correl * x1 +
#'                                            sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable6 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^0.10]
#' aa[, Independent_Variable7 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^0.25]
#' aa[, Independent_Variable8 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^0.75]
#' aa[, Independent_Variable9 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^2]
#' aa[, Independent_Variable10 := (pnorm(Correl * x1 +
#'                                         sqrt(1-Correl^2) * qnorm(x2)))^4]
#' aa[, ':=' (x1 = NULL, x2 = NULL)]
#' Construct <- data.table::data.table(Targets = rep("target",3),
#'                                     Distribution    = c("gaussian",
#'                                                         "gaussian",
#'                                                         "gaussian"),
#'                                     Loss            = c("MSE","MSE","Quadratic"),
#'                                     Quantile        = rep(NA,3),
#'                                     ModelName       = c("GBM","DRF","DL"),
#'                                     Algorithm       = c("gbm",
#'                                                         "randomForest",
#'                                                         "deeplearning"),
#'                                     dataName        = rep("aa",3),
#'                                     TargetCol       = rep(c("1"),3),
#'                                     FeatureCols     = rep(c("2:11"),3),
#'                                     CreateDate      = rep(Sys.time(),3),
#'                                     GridTune        = rep(FALSE,3),
#'                                     ExportValidData = rep(TRUE,3),
#'                                     ParDep          = rep(2,3),
#'                                     PD_Data         = rep("All",3),
#'                                     ThreshType      = rep("f1",3),
#'                                     FSC             = rep(0.001,3),
#'                                     tpProfit        = rep(NA,3),
#'                                     tnProfit        = rep(NA,3),
#'                                     fpProfit        = rep(NA,3),
#'                                     fnProfit        = rep(NA,3),
#'                                     SaveModel       = rep(FALSE,3),
#'                                     SaveModelType   = c("Mojo","standard","mojo"),
#'                                     PredsAllData    = rep(TRUE,3),
#'                                     TargetEncoding  = rep(NA,3),
#'                                     SupplyData      = rep(FALSE,3))
#' AutoH2OModeler(Construct,
#'                max_memory = "28G",
#'                ratios = 0.75,
#'                BL_Trees = 500,
#'                nthreads = 5,
#'                model_path = NULL,
#'                MaxRuntimeSeconds = 3600,
#'                MaxModels = 30,
#'                TrainData = NULL,
#'                TestData  = NULL,
#'                SaveToFile = FALSE,
#'                ReturnObjects = TRUE)
#'
#' # Quantile Regression Example
#' Correl <- 0.85
#' aa <- data.table::data.table(target = runif(1000))
#' aa[, x1 := qnorm(target)]
#' aa[, x2 := runif(1000)]
#' aa[, Independent_Variable1 := log(pnorm(Correl * x1 +
#'                                           sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable2 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable3 := exp(pnorm(Correl * x1 +
#'                                           sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable4 := exp(exp(pnorm(Correl * x1 +
#'                                               sqrt(1-Correl^2) * qnorm(x2))))]
#' aa[, Independent_Variable5 := sqrt(pnorm(Correl * x1 +
#'                                            sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable6 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^0.10]
#' aa[, Independent_Variable7 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^0.25]
#' aa[, Independent_Variable8 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^0.75]
#' aa[, Independent_Variable9 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^2]
#' aa[, Independent_Variable10 := (pnorm(Correl * x1 +
#'                                         sqrt(1-Correl^2) * qnorm(x2)))^4]
#' aa[, ':=' (x1 = NULL, x2 = NULL)]
#' Construct <- data.table::data.table(Targets = rep("target",3),
#'                                     Distribution    = c("quantile",
#'                                                         "quantile"),
#'                                     Loss            = c("MAE","Absolute"),
#'                                     Quantile        = rep(0.75,2),
#'                                     ModelName       = c("GBM","DL"),
#'                                     Algorithm       = c("gbm",
#'                                                         "deeplearning"),
#'                                     dataName        = rep("aa",2),
#'                                     TargetCol       = rep(c("1"),2),
#'                                     FeatureCols     = rep(c("2:11"),2),
#'                                     CreateDate      = rep(Sys.time(),2),
#'                                     GridTune        = rep(FALSE,2),
#'                                     ExportValidData = rep(TRUE,2),
#'                                     ParDep          = rep(4,2),
#'                                     PD_Data         = rep("All",2),
#'                                     ThreshType      = rep("f1",2),
#'                                     FSC             = rep(0.001,2),
#'                                     tpProfit        = rep(NA,2),
#'                                     tnProfit        = rep(NA,2),
#'                                     fpProfit        = rep(NA,2),
#'                                     fnProfit        = rep(NA,2),
#'                                     SaveModel       = rep(FALSE,2),
#'                                     SaveModelType   = c("Mojo","mojo"),
#'                                     PredsAllData    = rep(TRUE,2),
#'                                     TargetEncoding  = rep(NA,2),
#'                                     SupplyData      = rep(FALSE,2))
#' AutoH2OModeler(Construct,
#'                max_memory = "28G",
#'                ratios = 0.75,
#'                BL_Trees = 500,
#'                nthreads = 5,
#'                model_path = NULL,
#'                MaxRuntimeSeconds = 3600,
#'                MaxModels = 30,
#'                TrainData = NULL,
#'                TestData  = NULL,
#'                SaveToFile = FALSE,
#'                ReturnObjects = TRUE)
#'}
#' @export
AutoH2OModeler <- function(Construct,
                           max_memory        = "28G",
                           ratios            = 0.80,
                           BL_Trees          = 500,
                           nthreads          = 1,
                           model_path        = NULL,
                           MaxRuntimeSeconds = 3600,
                           MaxModels         = 30,
                           TrainData         = NULL,
                           TestData          = NULL,
                           SaveToFile        = FALSE,
                           ReturnObjects     = TRUE) {

  # Ensure packages are available
  requireNamespace('data.table', quietly = FALSE)

  ######################################
  # Error handling and prevention
  ######################################

  # Handle the multinomial case
  for (i in as.integer(seq_len(nrow(Construct)))) {
    if(tolower(Construct[i,2][[1]]) == "multinomial" &&
       tolower(Construct[i,3][[1]]) == "accuracy" &&
       tolower(Construct[i,6][[1]]) != "deeplearning") {
      multinomialMetric <- "accuracy"
      data.table::set(Construct,
                      i = i,
                      j = 3L,
                      value = "logloss")
    } else if(tolower(Construct[i,2][[1]]) == "multinomial" &&
              tolower(Construct[i,3][[1]]) == "auc" &&
              tolower(Construct[i,6][[1]]) != "deeplearning") {
      multinomialMetric <- "auc"
      data.table::set(Construct,
                      i = i,
                      j = 3L,
                      value = "logloss")
    } else if(tolower(Construct[i,2][[1]]) == "multinomial" &&
              tolower(Construct[i,3][[1]]) == "accuracy" &&
              tolower(Construct[i,6][[1]]) == "deeplearning") {
      multinomialMetric <- "accuracy"
      data.table::set(Construct,
                      i = i,
                      j = 3L,
                      value = "crossentropy")
    } else if (tolower(Construct[i,2][[1]]) == "multinomial" &&
               tolower(Construct[i,3][[1]]) == "auc" &&
               tolower(Construct[i,6][[1]]) == "deeplearning") {
      multinomialMetric <- "auc"
      data.table::set(Construct,
                      i = i,
                      j = 3L,
                      value = "crossentropy")
    }
  }

  ErrorCollection <-
    data.table::data.table(Row = rep(-720, 10000),
                           Msg = "I like modeling")
  j <- 0
  for (i in as.integer(seq_len(nrow(Construct)))) {
    # Algorithm specific
    if (tolower(
      Construct[i, 6][[1]]) %in% c("gbm",
                                   "randomforest",
                                   "automl",
                                   "xgboost",
                                   "lightgbm")) {
      # GBM and RF loss functions existence
      if (!(
        tolower(Construct[i, 3][[1]]) %in% c(
          "auto",
          "deviance",
          "mse",
          "rmse",
          "mae",
          "rmsle",
          "accuracy",
          "auc",
          "lift_top_group",
          "misclassification",
          "mean_per_class_error",
          "logloss"
        )
      )) {
        j <- j + 1
        data.table::set(ErrorCollection,
                        i = j,
                        j = 1L,
                        value = i)
        data.table::set(
          ErrorCollection,
          i = i,
          j = 2L,
          value = c(
            paste0(
              "Loss function ",
              Construct[i, 3][[1]],
              " is not in list: AUTO | deviance |
              logloss | MSE | RMSE |
              MAE | RMSLE | AUC | lift_top_group |
              misclassification | mean_per_class_error"
            )
          )
        )
      } else {
        temp <- tolower(Construct[i, 3][[1]])
        lower <-
          c(
            "auto",
            "deviance",
            "logloss",
            "mse",
            "rmse",
            "accuracy",
            "mae",
            "rmsle",
            "auc",
            "lift_top_group",
            "misclassification",
            "mean_per_class_error"
          )
        proper <-
          c(
            "AUTO",
            "deviance",
            "logloss",
            "MSE",
            "RMSE",
            "ACCURACY",
            "MAE",
            "RMSLE",
            "AUC",
            "lift_top_group",
            "misclassification",
            "mean_per_class_error"
          )
        distMatch <-
          data.table::data.table(act = rep(temp, 12),
                                 LCVals = lower,
                                 Proper = proper)
        ReplaceValue <- distMatch[act == LCVals][["Proper"]][[1]]
        data.table::set(Construct, i, 3L, value = ReplaceValue)
      }

      # GBM and RF distributions
      if (!(
        tolower(Construct[i, 2][[1]]) %in% c(
          "auto",
          "bernoulli",
          "quasibinomial",
          "multinomial",
          "gaussian",
          "poisson",
          "gamma",
          "tweedie",
          "laplace",
          "quantile",
          "huber"
        )
      )) {
        j <- j + 1
        data.table::set(ErrorCollection,
                        i = j,
                        j = 1L,
                        value = i)
        data.table::set(
          ErrorCollection,
          i = j,
          j = 2L,
          value = c(
            paste0(
              "Distribution ",
              Construct[i, 2][[1]],
              " is not in list: AUTO | bernoulli | quasibinomial |
              multinomial | gaussian | poisson | gamma | tweedie |
              laplace | quantile | huber"
            )
          )
        )
      } else {
        temp <- tolower(Construct[i, 2][[1]])
        lower <-
          c(
            "auto",
            "bernoulli",
            "quasibinomial",
            "multinomial",
            "gaussian",
            "poisson",
            "gamma",
            "tweedie",
            "laplace",
            "quantile",
            "huber"
          )
        proper <-
          c(
            "AUTO",
            "bernoulli",
            "quasibinomial",
            "multinomial",
            "gaussian",
            "poisson",
            "gamma",
            "tweedie",
            "laplace",
            "quantile",
            "huber"
          )
        distMatch <-
          data.table::data.table(act = rep(temp, 11),
                                 LCVals = lower,
                                 Proper = proper)
        ReplaceValue2 <- distMatch[act == LCVals][["Proper"]][[1]]
        data.table::set(Construct, i, 2L, value = ReplaceValue2)
      }

      # Distribution and loss combos for non-regression
      if (tolower(Construct[i, 2][[1]]) %in% c("quasibinomial", "binomial",
                                               "bernoulli", "multinomial") &&
          !(
            tolower(Construct[i, 3][[1]]) %in% c(
              "auc",
              "logloss",
              "accuracy",
              "auto",
              "lift_top_group",
              "misclassification",
              "mean_per_class_error"
            )
          )) {
        j <- j + 1
        data.table::set(ErrorCollection,
                        i = j,
                        j = 1L,
                        value = i)
        data.table::set(
          ErrorCollection,
          i = j,
          j = 2L,
          value = c(
            paste0(
              "Loss function ",
              Construct[i, 3][[1]],
              " is not in list: AUC | logloss | AUTO | lift_top_group |
              misclassification | mean_per_class_error | accuracy"
            )
          )
        )
      }

      # Distribution and loss combos for regression
      if (tolower(Construct[i, 2][[1]]) %in% c("gaussian",
                                               "poisson",
                                               "gamma",
                                               "tweedie",
                                               "laplace",
                                               "quantile",
                                               "huber") &&
          !(tolower(Construct[i, 3][[1]]) %in% c("auto", "mse", "rmse",
                                                 "mae", "rmsle"))) {
        j <- j + 1
        data.table::set(ErrorCollection,
                        i = j,
                        j = 1L,
                        value = i)
        data.table::set(
          ErrorCollection,
          i = j,
          j = 2L,
          value = c(
            paste0(
              "Loss function ",
              Construct[i, 2][[1]],
              " is not in list: AUTO | MSE | RMSE | MAE | RMSLE"
            )
          )
        )
      }

      # Quantile Regression with GBM
      if (tolower(Construct[i, 2][[1]]) %in% c("quantile") &&
          (Construct[i, 4][[1]] > 1 ||
           Construct[i, 4][[1]] < 0 ||
           !is.numeric(Construct[i, 4][[1]]))) {
        j <- j + 1
        data.table::set(ErrorCollection,
                        i = j,
                        j = 1L,
                        value = i)
        data.table::set(
          ErrorCollection,
          i = j,
          j = 2L,
          value = c(
            paste0(
              "Quantiles using ",
              Construct[i, 6][[1]],
              " must be a number less than or equal to 1 AND greater
              than or equal to 0"
            )
          )
        )
      }

      # RF Quantile regression fail
      if (tolower(Construct[i, 6][[1]]) == "randomforest" &&
          tolower(Construct[i, 2][[1]]) == "quantile") {
        j <- j + 1
        data.table::set(ErrorCollection,
                        i = j,
                        j = 1L,
                        value = i)
        data.table::set(
          ErrorCollection,
          i = j,
          j = 2L,
          value = c(
            paste0(
              "Quantile regression is only supported by GBM and
              Deeplearning models, not ",
              Construct[i, 6][[1]],
              " models"
            )
          )
        )
      }

      # Quantile regression loss metrics
      if (tolower(Construct[i, 2][[1]]) == "quantile" &&
          tolower(Construct[i, 3][[1]]) != "mae") {
        j <- j + 1
        data.table::set(ErrorCollection,
                        i = j,
                        j = 1L,
                        value = i)
        data.table::set(
          ErrorCollection,
          i = j,
          j = 2L,
          value = c(
            paste0(
              "Quantile regression is best supported by MAE when using ",
              Construct[i, 6][[1]],
              " models"
            )
          )
        )
      }

      if (tolower(Construct[i, 6][[1]]) == "automl" &
          Construct[i, 11][[1]] != TRUE) {
        j <- j + 1
        data.table::set(ErrorCollection,
                        i = j,
                        j = 1L,
                        value = i)
        data.table::set(
          ErrorCollection,
          i = j,
          j = 2L,
          value = c("using automl requires GridTune = TRUE")
        )
      }
    } else if (tolower(Construct[i, 6][[1]]) == "deeplearning") {
      # Deeplearning loss functions
      if (!(
        tolower(Construct[i, 3][[1]]) %in% c(
          "automatic",
          "crossentropy",
          "quadratic",
          "accuracy",
          "auc",
          "huber",
          "absolute",
          "quantile"
        )
      )) {
        j <- j + 1
        data.table::set(ErrorCollection,
                        i = j,
                        j = 1L,
                        value = i)
        data.table::set(
          ErrorCollection,
          i = j,
          j = 2L,
          value = c(
            paste0(
              "Loss function ",
              Construct[i, 3][[1]],
              " is not in list: Automatic | CrossEntropy | Quadratic |
              Huber | Absolute | Quantile | AUC | ACCURACY"
            )
          )
        )
      } else {
        temp <- tolower(Construct[i, 3][[1]])
        lower <-
          c(
            "automatic",
            "crossentropy",
            "quadratic",
            "auc",
            "accuracy",
            "huber",
            "absolute",
            "quantile"
          )
        proper <-
          c("Automatic",
            "CrossEntropy",
            "Quadratic",
            "AUC",
            "ACCURACY",
            "Huber",
            "Absolute",
            "Quantile")
        distMatch <-
          data.table::data.table(act = rep(temp, 8),
                                 LCVals = lower,
                                 Proper = proper)
        ReplaceVal <- distMatch[act == LCVals][["Proper"]][[1]]
        data.table::set(Construct, i, 3L, value = ReplaceVal)
      }

      # Deeplearning distributions
      if (!(
        tolower(Construct[i, 2][[1]]) %in% c(
          "auto",
          "bernoulli",
          "multinomial",
          "gaussian",
          "poisson",
          "gamma",
          "tweedie",
          "laplace",
          "quantile",
          "huber"
        )
      )) {
        j <- j + 1
        data.table::set(ErrorCollection,
                        i = j,
                        j = 1L,
                        value = i)
        data.table::set(
          ErrorCollection,
          i = j,
          j = 2L,
          value = c(
            paste0(
              "Distributions ",
              Construct[i, 2][[1]],
              " is not in list: AUTO | bernoulli | multinomial | gaussian |
              poisson | gamma | tweedie | laplace | quantile | huber"
            )
          )
        )
      } else {
        temp <- tolower(Construct[i, 2][[1]])
        lower <-
          c(
            "auto",
            "bernoulli",
            "multinomial",
            "gaussian",
            "poisson",
            "gamma",
            "tweedie",
            "laplace",
            "quantile",
            "huber"
          )
        proper <-
          c(
            "AUTO",
            "bernoulli",
            "multinomial",
            "gaussian",
            "poisson",
            "gamma",
            "tweedie",
            "laplace",
            "quantile",
            "huber"
          )
        distMatch <-
          data.table::data.table(act = rep(temp, 10),
                                 LCVals = lower,
                                 Proper = proper)
        ReplaceVal2 <- distMatch[act == LCVals][["Proper"]][[1]]
        data.table::set(Construct, i, 2L, value = ReplaceVal2)
      }

      # Distribution and loss combos for non-regression
      if (tolower(Construct[i,2][[1]]) %in% c("bernoulli",
                                              "multinomial") &&
          !(tolower(Construct[i,3][[1]]) %in% c("automatic",
                                                "crossentropy",
                                                "auc",
                                                "accuracy"))) {
        j <- j + 1
        data.table::set(ErrorCollection,
                        i = j,
                        j = 1L,
                        value = i)
        data.table::set(
          ErrorCollection,
          i = j,
          j = 2L,
          value = c(
            paste0(
              "Loss function ",
              Construct[i, 3][[1]],
              " is not in list: Automatic | CrossEntropy | AUC | ACCURACY"
            )
          )
        )
      }

      # Distribution and loss combos for regression
      if (tolower(Construct[i, 2][[1]]) %in% c("gaussian",
                                               "poisson",
                                               "gamma",
                                               "tweedie",
                                               "laplace",
                                               "quantile",
                                               "huber") &&
          !(
            tolower(Construct[i, 3][[1]]) %in% c(
              "automatic",
              "quadratic",
              "huber",
              "absolute",
              "quantile"
            )
          )) {
        j <- j + 1
        data.table::set(ErrorCollection,
                        i = j,
                        j = 1L,
                        value = i)
        data.table::set(
          ErrorCollection,
          i = j,
          j = 2L,
          value = c(
            paste0(
              "Loss function ",
              Construct[i, 3][[1]],
              " is not in list: Automatic | Quadratic
              | Huber | Absolute | Quantile"
            )
          )
        )
      }

      # Quantile regression loss metrics
      if (tolower(Construct[i, 2][[1]]) == "quantile" &&
          tolower(Construct[i, 3][[1]]) != "quantile") {
        j <- j + 1
        data.table::set(ErrorCollection,
                        i = j,
                        j = 1L,
                        value = i)
        data.table::set(
          ErrorCollection,
          i = j,
          j = 2L,
          value = c(
            paste0(
              "Quantile regression needs to use
              Quantile for the loss function with ",
              Construct[i, 6][[1]],
              " models"
            )
          )
        )
      }

      # Quantile Regression with DL
      if (tolower(Construct[i, 2][[1]]) %in% c("quantile") &&
          (Construct[i, 4][[1]] > 1 ||
           Construct[i, 4][[1]] < 0 ||
           !is.numeric(Construct[i, 4][[1]]))) {
        j <- j + 1
        data.table::set(ErrorCollection,
                        i = j,
                        j = 1L,
                        value = i)
        data.table::set(
          ErrorCollection,
          i = j,
          j = 2L,
          value = c(
            paste0(
              "Quantiles using ",
              Construct[i, 6][[1]],
              " must be a number less than or equal to
              1 AND greater than or equal to 0"
            )
          )
        )
      }

    } else {
      j <- j + 1
      data.table::set(ErrorCollection,
                      i = j,
                      j = 1L,
                      value = i)
      data.table::set(
        ErrorCollection,
        i = j,
        j = 2L,
        value = c(
          paste0(
            "Models supported are: GBM, randomForest,
            and deeplearning, while ",
            Construct[i, 6][[1]],
            " is not"
          )
        )
      )
    }
  }

  # Error stopping point and Construct file save
  ErrorCollection <- ErrorCollection[Row != -720]
  if (nrow(ErrorCollection) >= 1) {
    ErrorCollectionLog <- ErrorCollection
    if(SaveToFile == TRUE & ReturnObjects == TRUE) {
      message(
        "Your model construction file has errors and
        an error log has been returned and saved to model_path"
      )
      save(ErrorCollectionLog,
           file = paste0(model_path, "/ErrorCollectionLog.Rdata"))
      return(ErrorCollectionLog)
    } else if(SaveToFile == TRUE & ReturnObjects == FALSE) {
      save(ErrorCollectionLog,
           file = paste0(model_path, "/ErrorCollectionLog.Rdata"))
      warning("Your model construction file has errors and
            an error log has been saved to your model_path"
      )
    } else if(SaveToFile == FALSE & ReturnObjects == TRUE) {
      message(
        "Your model construction file has errors and
        an error log has
        been returned"
      )
      return(ErrorCollectionLog)
    } else {
      warning(
        "Your model construction file has errors and
        an error log errors. Set ReturnObjects to TRUE to see ErrorLog"
      )
    }
  }

  # Clear table
  rm(distMatch)

  # Set up grid_tuned_paths.R file
  grid_tuned_paths <-
    data.table::data.table(
      Model     = rep("a", nrow(Construct)),
      Path      = rep("a", nrow(Construct)),
      GT_Metric = rep(1234.5678, nrow(Construct)),
      BL_Metric = rep(1234.5678, nrow(Construct)),
      BinThresh = rep(1234.5678, nrow(Construct)),
      PathJar   = rep("a", nrow(Construct))
    )

  ######################################
  # Loop through model building
  ######################################

  tryCatch({
  for (i in as.integer(seq_len(nrow(Construct)))) {
    # No deeplearning loss functions as stopping metrics
    if (tolower(Construct[i, 3][[1]]) == "crossentropy") {
      if (tolower(Construct[i, 2][[1]]) == "multinomial") {
        StoppingMetric <- "logloss"
      } else {
        StoppingMetric <- "auc"
      }
    } else {
      if (tolower(Construct[i, 3][[1]]) %in% c("quadratic",
                                               "huber")) {
        StoppingMetric <- "mse"
      } else if (tolower(
        Construct[i, 3][[1]]) %in% c("absolute",
                                     "quantile")) {
        StoppingMetric <- "mae"
      } else {
        StoppingMetric <- Construct[i, 3][[1]]
      }
    }

    # Define grid tune search scheme in a named list
    search_criteria  <-
      list(
        strategy             = "RandomDiscrete",
        max_runtime_secs     = MaxRuntimeSeconds,
        max_models           = MaxModels,
        seed                 = 1234,
        stopping_rounds      = 10,
        stopping_metric      = StoppingMetric,
        stopping_tolerance   = 1e-3
      )

    # Set up H2O environment instance
    Sys.sleep(10)
    h2o::h2o.init(
      nthreads = nthreads,
      max_mem_size = max_memory,
      enable_assertions = FALSE
    )

    # Define data sets
    if (Construct[i, "SupplyData"][[1]]) {
      train        <- h2o::as.h2o(TrainData)
      validate     <- h2o::as.h2o(TestData)
      data_h2o     <- h2o::as.h2o(data.table::rbindlist(
        list(TrainData,
             TestData)))
    } else {
      data_h2o     <-
        eval(parse(text = paste0("h2o::as.h2o(",
                                 Construct[i, 7][[1]], ")")))
      data_train   <- h2o::h2o.splitFrame(data_h2o,
                                          ratios = ratios)
      train        <- data_train[[1]]
      validate     <- data_train[[2]]
    }

    # Define targets
    target         <-
      eval(parse(text = paste0(Construct[i, 8][[1]])))
    features       <-
      eval(parse(text = paste0(Construct[i, 9][[1]])))
    XGB            <- h2o::h2o.xgboost.available()
    if (XGB) {
      if (tolower(Construct[i, 2][[1]]) != "quantile") {
        ModelExclude   <- NULL
      } else {
        ModelExclude   <- c("XGBoost", "GLM", "DRF")
      }
    } else {
      if (tolower(Construct[i, 2][[1]]) != "quantile") {
        ModelExclude   <- c("XGBoost")
      } else {
        ModelExclude   <- c("XGBoost", "GLM", "DRF")
      }
    }

    if(tolower(Construct[i,6][[1]]) == "deeplearning") {
      N              <- length(features)
      P5             <- 2 ^ (-1 / 5)
      P4             <- 2 ^ (-1 / 4)
      P3             <- 2 ^ (-1 / 3)
    }
    data.table::set(grid_tuned_paths,
                    i = i,
                    j = 1L,
                    value = Construct[i, 5][[1]])

    ######################################
    # Target Encoding
    ######################################

    if (!is.na(Construct[i, "TargetEncoding"][[1]])) {
      TEncode <- eval(
        parse(
          text = Construct[i, "TargetEncoding"][[1]]))
      cols <- names(train)[TEncode]
      train[, Construct[i, "Targets"][[1]]] <-
        as.numeric(train[, Construct[i, "Targets"][[1]]])
      validate[, Construct[i, "Targets"][[1]]] <-
        as.numeric(validate[, Construct[i, "Targets"][[1]]])
      for (col in cols) {
        x     <- h2o::h2o.target_encode_create(
          data = train,
          x = list(col),
          y = Construct[i, "Targets"][[1]])
        # Apply to training data
        train <- h2o::h2o.target_encode_apply(
          train,
          x = list(col),
          y = Construct[i, "Targets"][[1]],
          target_encode_map = x,
          holdout_type = "None",
          blended_avg = TRUE,
          noise_level = 0
        )

        # Apply to validation data
        validate <- h2o::h2o.target_encode_apply(
          validate,
          x = list(col),
          y = Construct[i, "Targets"][[1]],
          target_encode_map = x,
          holdout_type = "None",
          blended_avg = TRUE,
          noise_level = 0
        )

        if(SaveToFile == TRUE) {
          save(x,
               file = paste0(model_path,
                             "/" ,
                             Construct[i, "Targets"][[1]],
                             "_", col,
                             ".Rdata"))
        }
      }

      # Modify feature reference
      features <-
        c((min(features) + length(eval(
          parse(text = paste0(Construct[i, 24][[1]]))
        ))):max(features), (max(target) + 1):(max(target) +
                                                length(eval(
                                                  parse(text = paste0(Construct[i, 24][[1]]))
                                                ))))

      # Turn target columns back to factor
      train[, Construct[i, "Targets"][[1]]] <-
        as.factor(train[, Construct[i, "Targets"][[1]]])
      validate[, Construct[i, "Targets"][[1]]] <-
        as.factor(validate[, Construct[i, "Targets"][[1]]])
      data.table::set(Construct,
                      i = i,
                      j = "PD_Data",
                      value = "Validate")
    }

    ######################################
    # Hyperparameters
    ######################################

    if (Construct[i, 11][[1]]) {
      if (tolower(Construct[i, 6][[1]]) == "gbm") {
        if (tolower(
          Construct[i, 3][[1]] %in% c(
            "auc",
            "logloss",
            "auto",
            "lift_top_group",
            "misclassification",
            "mean_per_class_error"
          )
        )) {
          hyper_params <- list(
            max_depth                        = seq(5, 8, 1),
            balance_classes                  = c(TRUE, FALSE),
            ntrees                           = c(500, 750, 1000),
            sample_rate                      = seq(0.5, 1, 0.01),
            col_sample_rate                  = seq(0.2, 1, 0.01),
            col_sample_rate_per_tree         = seq(0.2, 1, 0.01),
            col_sample_rate_change_per_level = seq(0.9, 1.1, 0.01),
            min_rows                         = 2 ^ seq(0, log2(eval(
              parse(text = paste0("nrow(", Construct[i, 7][[1]], ")"))
            ) * ratios[1]) - 1, 1),
            nbins                            = 2 ^ seq(4, 10, 1),
            nbins_cats                       = 2 ^ seq(4, 12, 1),
            min_split_improvement            = c(0, 1e-8, 1e-6, 1e-4),
            histogram_type                   = c("UniformAdaptive",
                                                 "QuantilesGlobal",
                                                 "RoundRobin")
          )
        } else {
          hyper_params <- list(
            max_depth                        = seq(5, 8, 1),
            ntrees                           = c(500, 750, 1000),
            sample_rate                      = seq(0.5, 1, 0.01),
            col_sample_rate                  = seq(0.2, 1, 0.01),
            col_sample_rate_per_tree         = seq(0.2, 1, 0.01),
            col_sample_rate_change_per_level = seq(0.9, 1.1, 0.01),
            min_rows                         = 2 ^ seq(0, log2(eval(
              parse(text = paste0("nrow(", Construct[i, 7][[1]], ")"))
            ) * ratios[1]) - 1, 1),
            nbins                            = 2 ^ seq(4, 10, 1),
            nbins_cats                       = 2 ^ seq(4, 12, 1),
            min_split_improvement            = c(0, 1e-8, 1e-6, 1e-4),
            histogram_type                   = c("UniformAdaptive",
                                                 "QuantilesGlobal",
                                                 "RoundRobin")
          )
        }

      } else if (tolower(Construct[i, 6][[1]]) == "deeplearning") {
        if (tolower(Construct[i, 3][[1]] %in% c("automatic",
                                                "crossentropy"))) {
          hyper_params <-
            list(
              activation = c(
                "Rectifier",
                "Maxout",
                "Tanh",
                "RectifierWithDropout",
                "MaxoutWithDropout",
                "TanhWithDropout"
              ),
              hidden              = list(
                c(
                  floor(N * P5),
                  floor(N * P5 * P5),
                  floor(N * P5 * P5 * P5),
                  floor(N * P5 * P5 * P5 * P5),
                  floor(N * P5 * P5 * P5 * P5 * P5)
                ),
                c(
                  floor(N * P4),
                  floor(N * P4 * P4),
                  floor(N * P4 * P4 * P4),
                  floor(N * P4 * P4 * P4 * P4)
                ),
                c(floor(N * P3), floor(N *
                                         P3 * P3),
                  floor(N * P3 * P3 * P3))
              ),
              balance_classes     = c(TRUE, FALSE),
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
              max_w2              = c(10, 100, 1000, 3.4028235e+38)
            )
        } else {
          hyper_params <-
            list(
              activation = c(
                "Rectifier",
                "Maxout",
                "Tanh",
                "RectifierWithDropout",
                "MaxoutWithDropout",
                "TanhWithDropout"
              ),
              hidden              = list(
                c(
                  floor(N * P5),
                  floor(N * P5 * P5),
                  floor(N * P5 * P5 * P5),
                  floor(N * P5 * P5 * P5 * P5),
                  floor(N * P5 * P5 * P5 * P5 * P5)
                ),
                c(
                  floor(N * P4),
                  floor(N * P4 * P4),
                  floor(N * P4 * P4 * P4),
                  floor(N * P4 * P4 * P4 * P4)
                ),
                c(floor(N * P3), floor(N *
                                         P3 * P3),
                  floor(N * P3 * P3 * P3))
              ),
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
              max_w2              = c(10, 100, 1000, 3.4028235e+38)
            )
        }
      } else if (tolower(Construct[i, 6][[1]]) == "randomforest") {
        if (tolower(
          Construct[i, 3][[1]] %in% c(
            "auc",
            "logloss",
            "auto",
            "lift_top_group",
            "misclassification",
            "mean_per_class_error"
          )
        )) {
          hyper_params <- list(
            max_depth                        = seq(5, 8, 1),
            balance_classes                  = c(TRUE, FALSE),
            ntrees                           = c(500, 750, 1000),
            mtries                           = -1,
            sample_rate                      = seq(0.2, 1, 0.05),
            col_sample_rate_per_tree         = seq(0.2, 1, 0.05),
            col_sample_rate_change_per_level = seq(0.9, 1.1, 0.01),
            min_rows                         = 2 ^ seq(0, log2(eval(
              parse(text = paste0("nrow(", Construct[i, 7][[1]], ")"))
            ) * ratios[1]) - 1, 1),
            nbins                            = 2 ^ seq(4, 10, 1),
            nbins_cats                       = 2 ^ seq(4, 12, 1),
            min_split_improvement            = c(0, 1e-8, 1e-6, 1e-4),
            histogram_type                   = c("UniformAdaptive",
                                                 "QuantilesGlobal",
                                                 "RoundRobin")
          )
        } else {
          hyper_params <- list(
            max_depth                        = seq(5, 8, 1),
            ntrees                           = c(500, 750, 1000),
            mtries                           = -1,
            sample_rate                      = seq(0.2, 1, 0.05),
            col_sample_rate_per_tree         = seq(0.2, 1, 0.05),
            col_sample_rate_change_per_level = seq(0.9, 1.1, 0.01),
            min_rows                         = 2 ^ seq(0, log2(eval(
              parse(text = paste0("nrow(", Construct[i, 7][[1]], ")"))
            ) * ratios[1]) - 1, 1),
            nbins                            = 2 ^ seq(4, 10, 1),
            nbins_cats                       = 2 ^ seq(4, 12, 1),
            min_split_improvement            = c(0, 1e-8, 1e-6, 1e-4),
            histogram_type                   = c("UniformAdaptive",
                                                 "QuantilesGlobal",
                                                 "RoundRobin")
          )
        }
      } else if (tolower(Construct[i, 6][[1]]) == "automl") {
        message("automl is preset with tuning parameters")
      } else if (tolower(Construct[i, 6][[1]]) == "xgboost") {
        if (tolower(
          Construct[i, 3][[1]] %in% c(
            "auc",
            "logloss",
            "auto",
            "lift_top_group",
            "misclassification",
            "mean_per_class_error"
          )
        )) {
          hyper_params <- list(
            max_depth                        = seq(5, 8, 1),
            tree_method                      = c("hist", "AUTO"),
            grow_policy                      = c("lossguide", "depthwise"),
            balance_classes                  = c(TRUE, FALSE),
            ntrees                           = c(500, 750, 1000),
            sample_rate                      = seq(0.5, 1, 0.01),
            col_sample_rate                  = seq(0.2, 1, 0.01),
            col_sample_rate_per_tree         = seq(0.2, 1, 0.01),
            reg_lambda                       = c(0.001, 0.01, 0.05),
            min_rows                         = 2 ^ seq(0, log2(eval(
              parse(text = paste0("nrow(", Construct[i, 7][[1]], ")"))
            ) * ratios[1]) - 1, 1),
            min_split_improvement            = c(0, 1e-8, 1e-6, 1e-4)
          )
        } else {
          hyper_params <- list(
            max_depth                        = seq(5, 8, 1),
            ntrees                           = c(500, 750, 1000),
            sample_rate                      = seq(0.5, 1, 0.01),
            col_sample_rate                  = seq(0.2, 1, 0.01),
            col_sample_rate_per_tree         = seq(0.2, 1, 0.01),
            reg_lambda                       = c(0.001, 0.01, 0.05),
            min_rows                         = 2 ^ seq(0, log2(eval(
              parse(text = paste0("nrow(", Construct[i, 7][[1]], ")"))
            ) * ratios[1]) - 1, 1),
            min_split_improvement            = c(0, 1e-8, 1e-6, 1e-4)
          )
        }
      } else if (tolower(Construct[i, 6][[1]]) == "lightgbm") {
        if (tolower(
          Construct[i, 3][[1]] %in% c(
            "auc",
            "logloss",
            "auto",
            "lift_top_group",
            "misclassification",
            "mean_per_class_error"
          )
        )) {
          hyper_params <- list(
            max_depth                        = seq(5, 8, 1),
            tree_method                      = c("hist"),
            grow_policy                      = c("lossguide"),
            balance_classes                  = c(TRUE, FALSE),
            ntrees                           = c(500, 750, 1000),
            sample_rate                      = seq(0.5, 1, 0.01),
            col_sample_rate                  = seq(0.2, 1, 0.01),
            col_sample_rate_per_tree         = seq(0.2, 1, 0.01),
            reg_lambda                       = c(0.001, 0.01, 0.05),
            min_rows                         = 2 ^ seq(0, log2(eval(
              parse(text = paste0("nrow(", Construct[i, 7][[1]], ")"))
            ) * ratios[1]) - 1, 1),
            min_split_improvement            = c(0, 1e-8, 1e-6, 1e-4)
          )
        } else {
          hyper_params <- list(
            max_depth                        = seq(5, 8, 1),
            ntrees                           = c(500, 750, 1000),
            sample_rate                      = seq(0.5, 1, 0.01),
            col_sample_rate                  = seq(0.2, 1, 0.01),
            col_sample_rate_per_tree         = seq(0.2, 1, 0.01),
            reg_lambda                       = c(0.001, 0.01, 0.05),
            min_rows                         = 2 ^ seq(0, log2(eval(
              parse(text = paste0("nrow(", Construct[i, 7][[1]], ")"))
            ) * ratios[1]) - 1, 1),
            min_split_improvement            = c(0, 1e-8, 1e-6, 1e-4)
          )
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
    if (Construct[i, 11][[1]]) {
      if (tolower(Construct[i, 2][[1]]) == "quantile") {
        if (tolower(Construct[i, 6][[1]]) == "gbm") {
          grid <- h2o::h2o.grid(
            hyper_params         = hyper_params,
            search_criteria      = search_criteria,
            algorithm            = Construct[i, 6][[1]],
            grid_id              = Construct[i, 5][[1]],
            x                    = features,
            y                    = target,
            training_frame       = train,
            validation_frame     = validate,
            distribution         = Construct[i, 2][[1]],
            quantile_alpha       = Construct[i, 4][[1]],
            learn_rate           = 0.05,
            learn_rate_annealing = 0.99,
            max_runtime_secs     = MaxRuntimeSeconds,
            stopping_rounds      = 5,
            stopping_tolerance   = 1e-4,
            stopping_metric      = StoppingMetric,
            score_tree_interval  = 10,
            seed                 = 1234
          )
        } else if (tolower(Construct[i, 6][[1]]) == "deeplearning") {
          grid <- h2o::h2o.grid(
            hyper_params         = hyper_params,
            search_criteria      = search_criteria,
            algorithm            = Construct[i, 6][[1]],
            grid_id              = Construct[i, 5][[1]],
            x                    = features,
            y                    = target,
            training_frame       = train,
            validation_frame     = validate,
            distribution         = Construct[i, 2][[1]],
            quantile_alpha       = Construct[i, 4][[1]],
            seed                 = 42
          )
        }
      } else {
        if (tolower(Construct[i, 6][[1]]) == "gbm") {
          grid <- h2o::h2o.grid(
            hyper_params         = hyper_params,
            search_criteria      = search_criteria,
            algorithm            = Construct[i, 6][[1]],
            grid_id              = Construct[i, 5][[1]],
            x                    = features,
            y                    = target,
            training_frame       = train,
            validation_frame     = validate,
            distribution         = Construct[i, 2][[1]],
            learn_rate           = 0.05,
            learn_rate_annealing = 0.99,
            max_runtime_secs     = MaxRuntimeSeconds,
            stopping_rounds      = 5,
            stopping_tolerance   = 1e-4,
            stopping_metric      = StoppingMetric,
            score_tree_interval  = 10,
            seed                 = 1234
          )
        } else if (tolower(Construct[i, 6][[1]]) == "deeplearning") {
          grid <- h2o::h2o.grid(
            hyper_params         = hyper_params,
            search_criteria      = search_criteria,
            algorithm            = Construct[i, 6][[1]],
            grid_id              = Construct[i, 5][[1]],
            x                    = features,
            y                    = target,
            training_frame       = train,
            validation_frame     = validate,
            distribution         = Construct[i, 2][[1]],
            seed                 = 42
          )
        } else if (tolower(Construct[i, 6][[1]]) == "randomforest") {
          grid <- h2o::h2o.grid(
            hyper_params         = hyper_params,
            search_criteria      = search_criteria,
            algorithm            = Construct[i, 6][[1]],
            grid_id              = Construct[i, 5][[1]],
            x                    = features,
            y                    = target,
            training_frame       = train,
            validation_frame     = validate,
            max_runtime_secs     = MaxRuntimeSeconds,
            stopping_rounds      = 5,
            stopping_tolerance   = 1e-4,
            stopping_metric      = StoppingMetric,
            score_tree_interval  = 10,
            seed                 = 1234
          )
        } else if (tolower(Construct[i, 6][[1]]) == "automl") {
          aml <- h2o::h2o.automl(
            x                  = features,
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
            sort_metric        = StoppingMetric
          )
        } else if (tolower(Construct[i, 6][[1]]) == "xgboost") {
          grid <- h2o::h2o.grid(
            hyper_params         = hyper_params,
            search_criteria      = search_criteria,
            algorithm            = Construct[i, 6][[1]],
            grid_id              = Construct[i, 5][[1]],
            x                    = features,
            y                    = target,
            training_frame       = train,
            validation_frame     = validate,
            categorical_encoding = "Enum",
            distribution         = Construct[i, 2][[1]],
            learn_rate           = 0.05,
            max_runtime_secs     = MaxRuntimeSeconds,
            stopping_rounds      = 5,
            stopping_tolerance   = 1e-4,
            stopping_metric      = StoppingMetric,
            score_tree_interval  = 10,
            seed                 = 1234
          )
        } else if (tolower(Construct[i, 6][[1]]) == "lightgbm") {
          grid <- h2o::h2o.grid(
            hyper_params         = hyper_params,
            search_criteria      = search_criteria,
            algorithm            = Construct[i, 6][[1]],
            grid_id              = Construct[i, 5][[1]],
            x                    = features,
            y                    = target,
            training_frame       = train,
            validation_frame     = validate,
            categorical_encoding = "Enum",
            distribution         = Construct[i, 2][[1]],
            learn_rate           = 0.05,
            max_runtime_secs     = MaxRuntimeSeconds,
            stopping_rounds      = 5,
            stopping_tolerance   = 1e-4,
            stopping_metric      = StoppingMetric,
            score_tree_interval  = 10,
            seed                 = 1234
          )
        }
      }

      # Store all models built sorted by metric
      if (tolower(Construct[i, 6][[1]]) == "automl") {
        Grid_Out <- h2o::h2o.getAutoML(project_name = "TestAML")
      } else if (tolower(Construct[i, 2][[1]]) %in% c("quasibinomial",
                                                      "binomial",
                                                      "bernoulli",
                                                      "multinomial")) {
        Decreasing <- TRUE
        Grid_Out   <-
          h2o::h2o.getGrid(
            grid_id = Construct[i, 5][[1]],
            sort_by = StoppingMetric,
            decreasing = Decreasing
          )
      } else {
        Decreasing <- FALSE
        Grid_Out   <-
          h2o::h2o.getGrid(
            grid_id = Construct[i, 5][[1]],
            sort_by = StoppingMetric,
            decreasing = Decreasing
          )
      }

      # Store best model
      if (tolower(Construct[i, 6][[1]]) == "automl") {
        best_model <- Grid_Out@leader
      } else {
        best_model <- h2o::h2o.getModel(Grid_Out@model_ids[[1]])
      }

      # Collect accuracy metric on validation data
      if (tolower(Construct[i, 3][[1]]) == "crossentropy") {
        if (tolower(Construct[i, 2][[1]]) == "multinomial") {
          cc <-
            h2o::h2o.logloss(h2o::h2o.performance(best_model, valid = TRUE))
        } else {
          cc <- h2o::h2o.auc(h2o::h2o.performance(best_model, valid = TRUE))
        }
      } else if (tolower(Construct[i, 3][[1]]) == "absolute") {
        cc <- h2o::h2o.mae(h2o::h2o.performance(best_model, valid = TRUE))
      } else if (tolower(Construct[i, 3][[1]]) %in% c("quadratic", "huber")) {
        cc <- h2o::h2o.mse(h2o::h2o.performance(best_model, valid = TRUE))
      } else {
        cc <-
          eval(parse(
            text = paste0(
              "h2o::h2o.",
              tolower(StoppingMetric),
              "(h2o::h2o.performance(best_model, valid = TRUE))"
            )
          ))
      }
      # Store results in metadata file
      data.table::set(grid_tuned_paths,
                      i = i,
                      j = 3L,
                      value = cc)
    }

    ######################################
    # Baseline Models
    ######################################

    # Check to see if quantile is selected
    # Choose model
    if (tolower(Construct[i, 6][[1]]) != "automl") {
      if (tolower(Construct[i, 2][[1]]) == "quantile") {
        if (tolower(Construct[i, 6][[1]]) == "gbm") {
          bl_model <- h2o::h2o.gbm(
            x                = features,
            y                = target,
            training_frame   = train,
            validation_frame = validate,
            distribution     = Construct[i, 2][[1]],
            quantile_alpha   = Construct[i, 4][[1]],
            model_id         = paste0("BL_GBM_", Construct[i, 5][[1]]),
            ntrees           = BL_Trees
          )
        } else if (tolower(Construct[i, 6][[1]]) == "deeplearning") {
          bl_model <- h2o::h2o.deeplearning(
            x                = features,
            y                = target,
            hidden           = c(
              floor(N * P4),
              floor(N * P4 * P4),
              floor(N * P4 * P4 * P4),
              floor(N * P4 * P4 * P4 * P4)
            ),
            training_frame   = train,
            validation_frame = validate,
            distribution     = Construct[i, 2][[1]],
            model_id         = paste0("BL_DL_", Construct[i, 5][[1]]),
            quantile_alpha   = Construct[i, 4][[1]]
          )
        }
      } else if (tolower(Construct[i, 6][[1]]) == "gbm") {
        bl_model <- h2o::h2o.gbm(
          x                = features,
          y                = target,
          training_frame   = train,
          validation_frame = validate,
          distribution     = Construct[i, 2][[1]],
          model_id         = paste0("BL_GBM_", Construct[i, 5][[1]]),
          ntrees           = BL_Trees
        )
      } else if (tolower(Construct[i, 6][[1]]) == "deeplearning") {
        bl_model <- h2o::h2o.deeplearning(
          x                = features,
          y                = target,
          hidden           = c(
            floor(N * P4),
            floor(N * P4 * P4),
            floor(N * P4 * P4 * P4),
            floor(N * P4 * P4 * P4 * P4)
          ),
          training_frame   = train,
          validation_frame = validate,
          model_id         = paste0("BL_DL_", Construct[i, 5][[1]]),
          distribution     = Construct[i, 2][[1]]
        )
      } else if (tolower(Construct[i, 6][[1]]) == "randomforest") {
        bl_model <- h2o::h2o.randomForest(
          x                = features,
          y                = target,
          training_frame   = train,
          validation_frame = validate,
          model_id         = paste0("BL_RF_", Construct[i, 5][[1]]),
          ntrees           = BL_Trees
        )
      } else if (tolower(Construct[i, 6][[1]]) == "xgboost") {
        bl_model <- h2o::h2o.xgboost(
          x                = features,
          y                = target,
          training_frame   = train,
          validation_frame = validate,
          categorical_encoding = "Enum",
          distribution     = Construct[i, 2][[1]],
          model_id         = paste0("BL_XG_", Construct[i, 5][[1]]),
          ntrees           = BL_Trees
        )
      } else if (tolower(Construct[i, 6][[1]]) == "lightgbm") {
        bl_model <- h2o::h2o.xgboost(
          x                = features,
          y                = target,
          training_frame   = train,
          validation_frame = validate,
          categorical_encoding = "Enum",
          distribution     = Construct[i, 2][[1]],
          tree_method      = "hist",
          grow_policy      = "lossguide",
          model_id         = paste0("BL_lgbm_", Construct[i, 5][[1]]),
          ntrees           = BL_Trees
        )
      }

      # Collect accuracy metric on validation data
      if (tolower(Construct[i, 3][[1]]) == "crossentropy") {
        if (tolower(Construct[i, 2][[1]]) == "multinomial") {
          dd <- h2o::h2o.logloss(h2o::h2o.performance(bl_model, valid = TRUE))
        } else {
          dd <- h2o::h2o.auc(h2o::h2o.performance(bl_model, valid = TRUE))
        }
      } else if (tolower(Construct[i, 3][[1]]) == "absolute") {
        dd <- h2o::h2o.mae(h2o::h2o.performance(bl_model, valid = TRUE))
      } else if (tolower(Construct[i, 3][[1]]) %in% c("quadratic", "huber")) {
        dd <- h2o::h2o.mse(h2o::h2o.performance(bl_model, valid = TRUE))
      } else {
        dd <-
          eval(parse(
            text = paste0(
              "h2o::h2o.",
              tolower(StoppingMetric),
              "(h2o::h2o.performance(bl_model, valid = TRUE))"
            )
          ))
      }

      # Store results in metadata file
      data.table::set(grid_tuned_paths,
                      i = i,
                      j = 4L,
                      value = dd)
    }

    ######################################
    # Model Evaluation & Saving
    ######################################

    # Check to see if GridTune is TRUE
    # Check to see if Distribution is multinomial
    # Proceed

    if (tolower(Construct[i, 6][[1]] == "automl")) {
      if (Construct[i, 21][[1]] == TRUE) {
        if (grid_tuned_paths[i, 2][[1]] != "a")
          file.remove(grid_tuned_paths[i, 2][[1]])
        if (tolower(Construct[i, 22][[1]]) == "standard") {
          save_model <-
            h2o::h2o.saveModel(object = best_model,
                               path = model_path,
                               force = TRUE)
          data.table::set(
            grid_tuned_paths,
            i = i,
            j = 2L,
            value = save_model
          )
          if(SaveToFile == TRUE) {
            save(grid_tuned_paths,
                 file = paste0(model_path, "/grid_tuned_paths.Rdata"))
          }
        } else {
          save_model <-
            h2o::h2o.saveMojo(object = best_model,
                              path = model_path,
                              force = TRUE)
          h2o::h2o.download_mojo(
            model = best_model,
            path = model_path,
            get_genmodel_jar = TRUE,
            genmodel_path = model_path,
            genmodel_name = Construct[i, 5][[1]]
          )
          data.table::set(
            grid_tuned_paths,
            i = i,
            j = 2L,
            value = save_model
          )
          data.table::set(
            grid_tuned_paths,
            i = i,
            j = 6L,
            value = paste0(model_path, "\\", Construct[i, 5][[1]])
          )
          if(SaveToFile == TRUE) {
            save(grid_tuned_paths,
                 file = paste0(model_path, "/grid_tuned_paths.Rdata"))
          }
        }
      }

      # Save VarImp and VarNOTImp
      if (best_model@algorithm != "stackedensemble") {
        VIMP <- data.table::as.data.table(h2o::h2o.varimp(best_model))
        if(SaveToFile == TRUE) {
          save(VIMP,
               file = paste0(model_path,
                             "/VarImp_",
                             Construct[i, 5][[1]],
                             ".Rdata"))
        }
        if (tolower(best_model@algorithm) != "glm") {
          NIF <- VIMP[percentage < Construct[i, 16][[1]], 1][[1]]
        } else {
          NIF <- NULL
        }
        if (length(NIF) > 0) {
          if(SaveToFile == TRUE) {
            save(NIF,
                 file = paste0(model_path,
                               "/VarNOTImp_",
                               Construct[i, 5][[1]],
                               ".Rdata"))
          }
        }
      } else {
        data.table::set(Construct,
                        i = i,
                        j = 13L,
                        value = 0)
      }

      # Gather predicted values
      preds <- h2o::h2o.predict(best_model, newdata = validate)[, 1]
      if (Construct[i, 14][[1]] == "All") {
        predsPD <- h2o::h2o.predict(best_model, newdata = data_h2o)[, 1]
        PredsPD <- data.table::as.data.table(predsPD)
        if(SaveToFile == TRUE) {
          data.table::fwrite(PredsPD,
                             file = paste0(model_path,
                                           "/",
                                           Construct[i, 5][[1]],
                                           "_PredsAll.csv"))
        }
      } else if (Construct[i, 14][[1]] == "Train") {
        predsPD <- h2o::h2o.predict(best_model, newdata = train)[, 1]
      } else if (Construct[i, 14][[1]] == "Validate") {
        predsPD <- h2o::h2o.predict(best_model, newdata = validate)[, 1]
      }
    }

    if (Construct[i, 11][[1]] == TRUE &
        tolower(Construct[i, 6][[1]]) != "automl") {
      if (!(tolower(Construct[i, 2][[1]]) %in% c("quasibinomial",
                                                 "binomial",
                                                 "bernoulli")) |
          tolower(Construct[i, 3][[1]]) == "logloss") {
        if (cc < dd) {
          # Save model
          if (Construct[i, 21][[1]] == TRUE) {
            if (grid_tuned_paths[i, 2][[1]] != "a")
              file.remove(grid_tuned_paths[i, 2][[1]])
            if (tolower(Construct[i, 22][[1]]) == "standard") {
              save_model <-
                h2o::h2o.saveModel(object = best_model,
                                   path = model_path,
                                   force = TRUE)
              data.table::set(
                grid_tuned_paths,
                i = i,
                j = 2L,
                value = save_model
              )
              if(SaveToFile == TRUE) {
                save(grid_tuned_paths,
                     file = paste0(model_path, "/grid_tuned_paths.Rdata"))
              }
            } else {
              save_model <-
                h2o::h2o.saveMojo(object = best_model,
                                  path = model_path,
                                  force = TRUE)
              h2o::h2o.download_mojo(
                model = best_model,
                path = model_path,
                get_genmodel_jar = TRUE,
                genmodel_path = model_path,
                genmodel_name = Construct[i, 5][[1]]
              )
              data.table::set(
                grid_tuned_paths,
                i = i,
                j = 2L,
                value = save_model
              )
              data.table::set(
                grid_tuned_paths,
                i = i,
                j = 6L,
                value = paste0(model_path, "\\", Construct[i, 5][[1]])
              )
              if(SaveToFile == TRUE) {
                save(grid_tuned_paths,
                     file = paste0(model_path, "/grid_tuned_paths.Rdata"))
              }
            }
          }

          # Save VarImp and VarNOTImp
          VIMP <-
            data.table::as.data.table(h2o::h2o.varimp(best_model))
          if(SaveToFile == TRUE) {
            save(VIMP,
                 file = paste0(model_path,
                               "/VarImp_",
                               Construct[i, 5][[1]],
                               ".Rdata"))
          }
          NIF <- VIMP[percentage < Construct[i, 16][[1]], 1][[1]]
          if (length(NIF) > 0) {
            if(SaveToFile == TRUE) {
              save(NIF,
                   file = paste0(model_path,
                                 "/VarNOTImp_",
                                 Construct[i, 5][[1]],
                                 ".Rdata"))
            }
          }

          # Gather predicted values
          preds <-
            h2o::h2o.predict(best_model, newdata = validate)[, 1]
          if (Construct[i, 14][[1]] == "All") {
            predsPD <- h2o::h2o.predict(best_model, newdata = data_h2o)[, 1]
            PredsPD <- as.data.table(predsPD)
            if(SaveToFile == TRUE) {
              data.table::fwrite(PredsPD,
                                 file = paste0(model_path,
                                               "/",
                                               Construct[i, 5][[1]],
                                               "_PredsAll.csv"))
            }
          } else if (Construct[i, 14][[1]] == "Train") {
            predsPD <- h2o::h2o.predict(best_model, newdata = train)[, 1]
          } else if (Construct[i, 14][[1]] == "Validate") {
            predsPD <- h2o::h2o.predict(best_model, newdata = validate)[, 1]
          }
        } else {
          # Save model
          if (Construct[i, 21][[1]] == TRUE) {
            if (grid_tuned_paths[i, 2][[1]] != "a")
              if(SaveToFile == TRUE) {
                file.remove(grid_tuned_paths[i, 2][[1]])
              }
            if (tolower(Construct[i, 22][[1]]) == "standard") {
              if(SaveToFile == TRUE) {
                save_model <-
                  h2o::h2o.saveModel(object = bl_model,
                                     path = model_path,
                                     force = TRUE)
                data.table::set(
                  grid_tuned_paths,
                  i = i,
                  j = 2L,
                  value = save_model
                )
                if(SaveToFile == TRUE) {
                  save(grid_tuned_paths,
                       file = paste0(model_path, "/grid_tuned_paths.Rdata"))
                }
              }
            } else {
              if(SaveToFile == TRUE) {
                save_model <-
                  h2o::h2o.saveMojo(object = bl_model,
                                    path = model_path,
                                    force = TRUE)
                h2o::h2o.download_mojo(
                  model = bl_model,
                  path = model_path,
                  get_genmodel_jar = TRUE,
                  genmodel_path = model_path,
                  genmodel_name = Construct[i, 5][[1]]
                )
                data.table::set(
                  grid_tuned_paths,
                  i = i,
                  j = 2L,
                  value = save_model
                )
                data.table::set(
                  grid_tuned_paths,
                  i = i,
                  j = 6L,
                  value = paste0(model_path, "\\", Construct[i, 5][[1]])
                )
                save(grid_tuned_paths,
                     file = paste0(model_path, "/grid_tuned_paths.Rdata"))
              }
            }
          }

          # Save VarImp
          VIMP <-
            data.table::as.data.table(h2o::h2o.varimp(bl_model))
          if(SaveToFile == TRUE) {
            save(VIMP,
                 file = paste0(model_path,
                               "/VarImp_",
                               Construct[i, 5][[1]],
                               ".Rdata"))
          }
          NIF <- VIMP[percentage < Construct[i, 16][[1]], 1][[1]]
          if (length(NIF) > 0) {
            if(SaveToFile == TRUE) {
              save(NIF,
                   file = paste0(model_path,
                                 "/VarNOTImp_",
                                 Construct[i, 5][[1]],
                                 ".Rdata"))
            }
          }

          # Gather predicted values
          preds <-
            h2o::h2o.predict(bl_model, newdata = validate)[, 1]
          if (Construct[i, 14][[1]] == "All") {
            predsPD <- h2o::h2o.predict(bl_model, newdata = data_h2o)[, 1]
            PredsPD <- data.table::as.data.table(predsPD)
            if(SaveToFile == TRUE) {
              data.table::fwrite(PredsPD,
                                 file = paste0(model_path,
                                               "/", Construct[i, 5][[1]],
                                               "_PredsAll.csv"))
            }
          } else if (Construct[i, 14][[1]] == "Train") {
            predsPD <- h2o::h2o.predict(bl_model, newdata = train)[, 1]
          } else if (Construct[i, 14][[1]] == "Validate") {
            predsPD <- h2o::h2o.predict(bl_model, newdata = validate)[, 1]
          }
        }
      } else {
        if (cc > dd) {
          # Save model
          if (Construct[i, 21][[1]] == TRUE) {
            if (grid_tuned_paths[i, 2][[1]] != "a")
              if(SaveToFile == TRUE) {
                file.remove(grid_tuned_paths[i, 2][[1]])
              }
            if (tolower(Construct[i, 22][[1]]) == "standard") {
              if(SaveToFile == TRUE) {
                save_model <-
                  h2o::h2o.saveModel(object = best_model,
                                     path = model_path,
                                     force = TRUE)
                data.table::set(
                  grid_tuned_paths,
                  i = i,
                  j = 2L,
                  value = save_model
                )
                save(grid_tuned_paths,
                     file = paste0(model_path, "/grid_tuned_paths.Rdata"))
              }
            } else {
              if(SaveToFile == TRUE) {
                save_model <-
                  h2o::h2o.saveMojo(object = best_model,
                                    path = model_path,
                                    force = TRUE)
                h2o::h2o.download_mojo(
                  model = best_model,
                  path = model_path,
                  get_genmodel_jar = TRUE,
                  genmodel_path = model_path,
                  genmodel_name = Construct[i, 5][[1]]
                )
                data.table::set(
                  grid_tuned_paths,
                  i = i,
                  j = 2L,
                  value = save_model
                )
                data.table::set(
                  grid_tuned_paths,
                  i = i,
                  j = 6L,
                  value = paste0(model_path, "\\", Construct[i, 5][[1]])
                )
                save(grid_tuned_paths,
                     file = paste0(model_path, "/grid_tuned_paths.Rdata"))
              }
            }
          }

          # Store threshold
          store_results <-
            data.table::data.table(
              best_model@model$training_metrics@metrics$thresholds_and_metric_scores
            )
          if (tolower(Construct[i, 15][[1]]) == "f1" ||
              is.null(Construct[i, 15][[1]])) {
            Thresh <-
              tryCatch({
                store_results[order(-f1)][1, 1][[1]]
              }, error = function(x)
                1)
            Label  <- "f1"
          } else if (tolower(Construct[i, 15][[1]]) == "f2") {
            Thresh <-
              tryCatch({
                store_results[order(-f2)][1, 1][[1]]
              }, error = function(x)
                1)
            Label  <- "f2"
          } else if (tolower(Construct[i, 15][[1]]) == "f0point5") {
            Thresh <-
              tryCatch({
                store_results[order(-f0point5)][1, 1][[1]]
              }, error = function(x)
                1)
            Label <- "f0point5"
          } else if (tolower(Construct[i, 15][[1]]) == "cs") {
            predsPDD <- h2o::h2o.predict(bl_model,
                                         newdata = data_h2o)[, 3]
            data    <-
              data.table::as.data.table(h2o::h2o.cbind(data_h2o,
                                                       predsPDD))
            data[, eval(Construct[i, 1][[1]]) := as.numeric(
              as.character(get(Construct[i, 1][[1]])))]
            temp  <- threshOptim(
              data     = data,
              actTar   = Construct[i, 1][[1]],
              predTar  = 'p1',
              tpProfit = Construct[i, 17][[1]],
              tnProfit = Construct[i, 18][[1]],
              fpProfit = Construct[i, 19][[1]],
              fnProfit = Construct[i, 20][[1]]
            )
            Thresh <- temp[[1]]
            Label <- "CS"
          }
          data.table::set(
            grid_tuned_paths,
            i = i,
            j = 5L,
            value = Thresh
          )

          # Save VarImp
          VIMP <-
            data.table::as.data.table(h2o::h2o.varimp(best_model))
          if(SaveToFile == TRUE) {
            save(VIMP,
                 file = paste0(model_path,
                               "/VarImp_", Construct[i, 5][[1]],
                               ".Rdata"))
          }
          NIF <- VIMP[percentage < Construct[i, 16][[1]], 1][[1]]
          if (length(NIF) > 0) {
            if(SaveToFile == TRUE) {
              save(NIF,
                   file = paste0(model_path,
                                 "/VarNOTImp_", Construct[i, 5][[1]],
                                 ".Rdata"))
            }
          }

          # Gather predicted values
          preds <-
            h2o::h2o.predict(best_model, newdata = validate)[, 3]
          if (Construct[i, 14][[1]] == "All") {
            predsPD <- h2o::h2o.predict(best_model, newdata = data_h2o)[, 3]
            PredsPD <- data.table::as.data.table(predsPD)
            if(SaveToFile == TRUE) {
              data.table::fwrite(PredsPD,
                                 file = paste0(model_path,
                                               "/", Construct[i, 5][[1]],
                                               "_PredsAll.csv"))
            }
          } else if (Construct[i, 14][[1]] == "Train") {
            predsPD <- h2o::h2o.predict(best_model, newdata = train)[, 3]
          } else if (Construct[i, 14][[1]] == "Validate") {
            predsPD <- h2o::h2o.predict(best_model, newdata = validate)[, 3]
          }
        } else {
          # Save model
          if (Construct[i, 21][[1]] == TRUE) {
            if (grid_tuned_paths[i, 2][[1]] != "a")
              if(SaveToFile == TRUE) {
                file.remove(grid_tuned_paths[i, 2][[1]])
              }
            if (tolower(Construct[i, 22][[1]]) == "standard") {
              if(SaveToFile == TRUE) {
                save_model <-
                  h2o::h2o.saveModel(object = bl_model,
                                     path = model_path,
                                     force = TRUE)
                data.table::set(
                  grid_tuned_paths,
                  i = i,
                  j = 2L,
                  value = save_model
                )
                save(grid_tuned_paths,
                     file = paste0(model_path, "/grid_tuned_paths.Rdata"))
              }
            } else {
              if(SaveToFile == TRUE) {
                save_model <-
                  h2o::h2o.saveMojo(object = bl_model,
                                    path = model_path,
                                    force = TRUE)
                h2o::h2o.download_mojo(
                  model = bl_model,
                  path = model_path,
                  get_genmodel_jar = TRUE,
                  genmodel_path = model_path,
                  genmodel_name = Construct[i, 5][[1]]
                )
                data.table::set(
                  grid_tuned_paths,
                  i = i,
                  j = 2L,
                  value = save_model
                )
                data.table::set(
                  grid_tuned_paths,
                  i = i,
                  j = 6L,
                  value = paste0(model_path, "\\", Construct[i, 5][[1]])
                )
                save(grid_tuned_paths,
                     file = paste0(model_path, "/grid_tuned_paths.Rdata"))
              }
            }
          }

          # Store threshold
          store_results <-
            data.table::data.table(
              bl_model@model$training_metrics@metrics$thresholds_and_metric_scores)
          if (tolower(Construct[i, 15][[1]]) == "f1" ||
              is.null(Construct[i, 15][[1]])) {
            Thresh <-
              tryCatch({
                store_results[order(-f1)][1, 1][[1]]
              }, error = function(x)
                1)
            Label  <- "f1"
          } else if (tolower(Construct[i, 15][[1]]) == "f2") {
            Thresh <-
              tryCatch({
                store_results[order(-f2)][1, 1][[1]]
              }, error = function(x)
                1)
            Label  <- "f2"
          } else if (tolower(Construct[i, 15][[1]]) == "f0point5") {
            Thresh <-
              tryCatch({
                store_results[order(-f0point5)][1, 1][[1]]
              }, error = function(x)
                1)
            Label <- "f0point5"
          } else if (tolower(Construct[i, 15][[1]]) == "CS") {
            predsPDD <- h2o::h2o.predict(bl_model, newdata = data_h2o)[, 3]
            data    <-
              data.table::as.data.table(h2o::h2o.cbind(data_h2o, predsPDD))
            data[, eval(Construct[i, 1][[1]]) := as.numeric(
              as.character(get(Construct[i, 1][[1]])))]
            temp  <- threshOptim(
              data     = data,
              actTar   = Construct[i, 1][[1]],
              predTar  = 'p1',
              tpProfit = Construct[i, 17][[1]],
              tnProfit = Construct[i, 18][[1]],
              fpProfit = Construct[i, 19][[1]],
              fnProfit = Construct[i, 20][[1]]
            )
            Thresh <- temp[[1]]
            Label <- "CS"
          }
          data.table::set(
            grid_tuned_paths,
            i = i,
            j = 5L,
            value = Thresh
          )

          # Save VarImp
          VIMP <-
            data.table::as.data.table(h2o::h2o.varimp(bl_model))
          if(SaveToFile == TRUE) {
            save(VIMP,
                 file = paste0(model_path,
                               "/VarImp_", Construct[i, 5][[1]],
                               ".Rdata"))
          }
          NIF <- VIMP[percentage < Construct[i, 16][[1]], 1][[1]]
          if (length(NIF) > 0) {
            if(SaveToFile == TRUE) {
              save(NIF,
                   file = paste0(model_path,
                                 "/VarNOTImp_", Construct[i, 5][[1]],
                                 ".Rdata"))
            }
          }

          # Gather predicted values
          preds <-
            h2o::h2o.predict(bl_model, newdata = validate)[, 3]
          if (Construct[i, 14][[1]] == "All") {
            predsPD <- h2o::h2o.predict(bl_model, newdata = data_h2o)[, 3]
            PredsPD <- data.table::as.data.table(predsPD)
            if(SaveToFile == TRUE) {
              data.table::fwrite(PredsPD,
                                 file = paste0(model_path,
                                               "/",
                                               Construct[i, 5][[1]],
                                               "_PredsAll.csv"))
            }
          } else if (Construct[i, 14][[1]] == "Train") {
            predsPD <- h2o::h2o.predict(bl_model, newdata = train)[, 3]
          } else if (Construct[i, 14][[1]] == "Validate") {
            predsPD <- h2o::h2o.predict(bl_model, newdata = validate)[, 3]
          }
        }
      }
    } else if (tolower(Construct[i, 6][[1]]) != "automl") {
      # Save model
      if (Construct[i, 21][[1]] == TRUE) {
        if (grid_tuned_paths[i, 2][[1]] != "a")
          if(SaveToFile == TRUE) {
            file.remove(grid_tuned_paths[i, 2][[1]])
          }
        if (tolower(Construct[i, 22][[1]]) == "standard") {
          if(SaveToFile == TRUE) {
            save_model <-
              h2o::h2o.saveModel(object = bl_model,
                                 path = model_path,
                                 force = TRUE)
            data.table::set(
              grid_tuned_paths,
              i = i,
              j = 2L,
              value = save_model
            )
            save(grid_tuned_paths,
                 file = paste0(model_path,
                               "/grid_tuned_paths.Rdata"))
          }
        } else {
          if(SaveToFile == TRUE) {
            save_model <-
              h2o::h2o.saveMojo(object = bl_model,
                                path = model_path,
                                force = TRUE)
            h2o::h2o.download_mojo(
              model = bl_model,
              path = model_path,
              get_genmodel_jar = TRUE,
              genmodel_path = model_path,
              genmodel_name = Construct[i, 5][[1]]
            )
            data.table::set(
              grid_tuned_paths,
              i = i,
              j = 2L,
              value = save_model
            )
            data.table::set(
              grid_tuned_paths,
              i = i,
              j = 6L,
              value = paste0(model_path, "\\", Construct[i, 5][[1]])
            )
            save(grid_tuned_paths,
                 file = paste0(model_path,
                               "/grid_tuned_paths.Rdata"))
          }
        }
      }

      # Store threshold for binary classification
      if (tolower(Construct[i, 2][[1]]) %in% c("quasibinomial",
                                               "binomial",
                                               "bernoulli")) {
        store_results <-
          data.table::data.table(
            bl_model@model$training_metrics@metrics$thresholds_and_metric_scores)
        if (tolower(Construct[i, 15][[1]]) == "f1" ||
            is.null(Construct[i, 15][[1]])) {
          Thresh <-
            tryCatch({
              store_results[order(-f1)][1, 1][[1]]
            }, error = function(x)
              1)
          Label  <- "f1"
        } else if (tolower(Construct[i, 15][[1]]) == "f2") {
          Thresh <-
            tryCatch({
              store_results[order(-f2)][1, 1][[1]]
            }, error = function(x)
              1)
          Label  <- "f2"
        } else if (tolower(Construct[i, 15][[1]]) == "f0point5") {
          Thresh <-
            tryCatch({
              store_results[order(-f0point5)][1, 1][[1]]
            }, error = function(x)
              1)
          Label <- "f0point5"
        } else if (tolower(Construct[i, 15][[1]]) == "cs") {
          predsPDD <- h2o::h2o.predict(bl_model, newdata = data_h2o)[, 3]
          data    <-
            data.table::as.data.table(h2o::h2o.cbind(data_h2o, predsPDD))
          data[, eval(Construct[i, 1][[1]]) := as.numeric(
            as.character(get(Construct[i, 1][[1]])))]
          temp  <- threshOptim(
            data     = data,
            actTar   = Construct[i, 1][[1]],
            predTar  = 'p1',
            tpProfit = Construct[i, 17][[1]],
            tnProfit = Construct[i, 18][[1]],
            fpProfit = Construct[i, 19][[1]],
            fnProfit = Construct[i, 20][[1]]
          )
          Thresh <- temp[[1]]
          Label <- "CS"
        }
        data.table::set(grid_tuned_paths,
                        i = i,
                        j = 5L,
                        value = Thresh)
        preds <- h2o::h2o.predict(bl_model, newdata = validate)[, 3]
        if (tolower(Construct[i, 14][[1]]) == "all") {
          predsPD <- h2o::h2o.predict(bl_model, newdata = data_h2o)[, 3]
          PredsPD <- data.table::as.data.table(predsPD)
          if(SaveToFile == TRUE) {
            fwrite(PredsPD,
                   file = paste0(model_path,
                                 "/", Construct[i, 5][[1]],
                                 "_PredsAll.csv"))
          }
        } else if (tolower(Construct[i, 14][[1]]) == "train") {
          predsPD <- h2o::h2o.predict(bl_model, newdata = train)[, 3]
        } else if (tolower(Construct[i, 14][[1]]) == "validate") {
          predsPD <- h2o::h2o.predict(bl_model, newdata = validate)[, 3]
        }
      } else {
        # Store predicted values against validate data for calibration plot
        preds <- h2o::h2o.predict(bl_model, newdata = validate)[, 1]
        if (tolower(Construct[i, 14][[1]]) == "all") {
          predsPD <- h2o::h2o.predict(bl_model, newdata = data_h2o)[, 1]
          PredsPD <- data.table::as.data.table(predsPD)
          if(SaveToFile == TRUE) {
            data.table::fwrite(PredsPD,
                               file = paste0(model_path,
                                             "/",
                                             Construct[i, 5][[1]],
                                             "_PredsAll.csv"))
          }
        } else if (tolower(Construct[i, 14][[1]]) == "train") {
          predsPD <- h2o::h2o.predict(bl_model, newdata = train)[, 1]
        } else if (tolower(Construct[i, 14][[1]]) == "validate") {
          predsPD <- h2o::h2o.predict(bl_model, newdata = validate)[, 1]
        }
      }

      # Save VarImp
      VIMP <- data.table::as.data.table(h2o::h2o.varimp(bl_model))
      if(SaveToFile == TRUE) {
        save(VIMP,
             file = paste0(model_path,
                           "/VarImp_",
                           Construct[i, 5][[1]],
                           ".Rdata"))
      }
      NIF <- VIMP[percentage < Construct[i, 16][[1]], 1][[1]]
      if (length(NIF) > 0) {
        if(SaveToFile == TRUE) {
          save(NIF,
               file = paste0(model_path,
                             "/VarNOTImp_",
                             Construct[i, 5][[1]],
                             ".Rdata"))
        }
      }
    }

    ######################################
    # Model Evaluation Plots
    ######################################

    # Generate plots
    col <- Construct[i, 1][[1]]
    calibration <-
      data.table::as.data.table(h2o::h2o.cbind(preds, validate[, col]))
    if (tolower(Construct[i, 2][[1]]) %in% c("quasibinomial",
                                             "binomial",
                                             "bernoulli")) {
      calibration[, eval(col) := as.numeric(as.character(get(col)))]
    }
    if (Construct[i, 13][[1]] >= 1) {
      if (tolower(Construct[i, 14][[1]]) == "all") {
        calibEval <-
          data.table::as.data.table(h2o::h2o.cbind(preds, validate))
        calib <-
          data.table::as.data.table(h2o::h2o.cbind(predsPD, data_h2o))
      } else if (tolower(Construct[i, 14][[1]]) == "train") {
        calibEval <-
          data.table::as.data.table(h2o::h2o.cbind(preds, validate))
        calib <- as.data.table(h2o::h2o.cbind(predsPD, train))
      } else if (tolower(Construct[i, 14][[1]]) == "validate") {
        calibEval <-
          data.table::as.data.table(h2o::h2o.cbind(preds, validate))
        calib <- as.data.table(h2o::h2o.cbind(predsPD, validate))
      }
      if (Construct[i, 12][[1]]) {
        if(SaveToFile == TRUE) {
          save(calibEval,
               file = paste0(model_path,
                             "/", Construct[i, 5][[1]],
                             "_Validation.Rdata"))
        }
      }
    } else {
      if (Construct[i, 12][[1]]) {
        calibEval <-
          data.table::as.data.table(h2o::h2o.cbind(preds, validate))
        if(SaveToFile == TRUE) {
          save(calibEval,
               file = paste0(model_path,
                             "/",
                             Construct[i, 5][[1]],
                             "_Validation.Rdata"))
        }
      }
    }
    predName <- names(calibration[, 1])

    # Generate evaluation plots
    if (tolower(Construct[i, 2][[1]]) != "multinomial") {
      if (tolower(Construct[i, 2][[1]]) == "quantile") {

        # Store best metric
        if(Construct[i,11][[1]]) {
          if(cc < dd) {
            val <- cc
          } else {
            val <- dd
          }
        } else {
          val <- dd
        }

        # Calibration plot
        out1 <- EvalPlot(
          calibration,
          PredictionColName = predName,
          TargetColName  = Construct[i, 1][[1]],
          GraphType        = "calibration",
          PercentileBucket      = 0.05,
          aggrfun     = function(x)
            quantile(x,
                     probs = Construct[i, 4][[1]],
                     na.rm = TRUE)
        )
        out1 <- out1 + ggplot2::ggtitle(
          paste0("Calibration Evaluation Plot ",
                 toupper(Construct[i,3][[1]]),
                 ": ",
                 round(val,4))
        )
        if(SaveToFile == TRUE) {
          ggplot2::ggsave(paste0(model_path,
                                 "/CalP_",
                                 Construct[i, 5][[1]],
                                 ".png"))
        }

        # Calibration boxplot
        out2 <- EvalPlot(
          calibration,
          PredictionColName = predName,
          TargetColName  = Construct[i, 1][[1]],
          GraphType        = "boxplot",
          PercentileBucket      = 0.05
        )
        out2 <- out2 + ggplot2::ggtitle(
          paste0("Calibration Evaluation Plot ",
                 toupper(Construct[i,3][[1]]),
                 ": ",
                 round(val,4))
        )
        if(SaveToFile == TRUE) {
          ggplot2::ggsave(paste0(model_path,
                                 "/CalBP_",
                                 Construct[i, 5][[1]],
                                 ".png"))
        }
      } else if (tolower(Construct[i, 2][[1]]) %in% c("quasibinomial",
                                                      "binomial",
                                                      "bernoulli")) {

        # Store best metric
        if(Construct[i,11][[1]]) {
          if(cc < dd) {
            val <- cc
          } else {
            val <- dd
          }
        } else {
          val <- dd
        }

        out1 <- EvalPlot(
          calibration,
          PredictionColName = predName,
          TargetColName  = Construct[i, 1][[1]],
          GraphType        = "calibration",
          PercentileBucket      = 0.05,
          aggrfun     = function(x)
            base::mean(x, na.rm = TRUE)
        )
        out1 <- out1 + ggplot2::ggtitle(
          paste0("Calibration Evaluation Plot ",
                 toupper(Construct[i,3][[1]]),
                 ": ",
                 round(val,4))
        )

        if (exists("Thresh")) {
          out1 <- out1 + ggplot2::geom_hline(yintercept = Thresh)
        }
        if(SaveToFile == TRUE) {
          ggplot2::ggsave(paste0(model_path,
                                 "/CalP_",
                                 Construct[i, 5][[1]],
                                 ".png"))
        }
      } else {

        # Store best metric
        if(Construct[i,11][[1]]) {
          if(cc < dd) {
            val <- cc
          } else {
            val <- dd
          }
        } else {
          val <- dd
        }

        # Calibration plot
        out1 <- EvalPlot(
          calibration,
          PredictionColName = predName,
          TargetColName  = Construct[i, 1][[1]],
          GraphType        = "calibration",
          PercentileBucket      = 0.05,
          aggrfun     = function(x)
            base::mean(x, na.rm = TRUE)
        )
        out1 <- out1 + ggplot2::ggtitle(
          paste0("Calibration Evaluation Plot ",
                 toupper(Construct[i,3][[1]]),
                 ": ",
                 round(val,4))
        )
        if(SaveToFile == TRUE) {
          ggplot2::ggsave(paste0(model_path,
                                 "/CalP_",
                                 Construct[i, 5][[1]],
                                 ".png"))
        }

        # Calibration boxplot
        out2 <- EvalPlot(
          calibration,
          PredictionColName = predName,
          TargetColName  = Construct[i, 1][[1]],
          GraphType        = "boxplot",
          PercentileBucket      = 0.05
        )
        out2 <- out2 + ggplot2::ggtitle(
          paste0("Calibration Evaluation Plot ",
                 toupper(Construct[i,3][[1]]),
                 ": ",
                 round(val,4))
        )
        if(SaveToFile == TRUE) {
          ggplot2::ggsave(paste0(model_path,
                                 "/CalBP_",
                                 Construct[i, 5][[1]],
                                 ".png"))
        }
      }
    } else {
      # Multinomial case
      # Stack each level's predicted values and actual values
      if (Construct[i, 11][[1]] && cc <= dd) {
        predsMulti <- h2o::h2o.predict(best_model, newdata = validate)
        col <- Construct[i, 1][[1]]
        xx <-
          data.table::as.data.table(h2o::h2o.cbind(validate[, col],
                                                   predsMulti))
        if (Construct[i, 12][[1]]) {
          calib <- data.table::as.data.table(h2o::h2o.cbind(validate,
                                                            preds))
          if(SaveToFile == TRUE) {
            save(calib, file = paste0(model_path,
                                      "/",
                                      Construct[i, 5][[1]],
                                      "_Validation.Rdata"))
          }
        }
        N <- (ncol(xx) - 2)
        data <- eval(parse(text = Construct[i, 7][[1]]))
        for (lev in levels(data[[Construct[i, 1][[1]]]])) {
          xx[, paste0("V", lev) := ifelse(xx[[1]] %in% lev, 1, 0)]
        }
        RemoveCols <- names(xx)[1:2]
        KeepCols   <- names(xx)[3:length(names(xx))]
        xx[, (RemoveCols) := NULL]
        store <- list()
        for (k in 1:N) {
          j <- k + N
          temp <- cbind(xx[, ..k], xx[, ..j])
          data.table::setnames(temp, KeepCols[k], "Preds")
          data.table::setnames(temp, KeepCols[j], "Act")
          store[[k]] <- temp
        }
        xxx <- data.table::rbindlist(store)

        # Multinomial metric
        if(multinomialMetric == "auc") {
          val <- H2OMultinomialAUC(validate,
                                   best_model,
                                   targetColNum = 1,
                                   targetName = Construct[i,1][[1]])
        } else {
          xx <- data.table::as.data.table(h2o::h2o.cbind(
            validate[, 1],
            h2o::h2o.predict(best_model,
                             newdata = validate)[,1]))
          names(xx)
          val <- mean(
            xx[, Accuracy := as.numeric(
              ifelse(get(Construct[i,1][[1]]) == predict, 1, 0))][["Accuracy"]],
            na.rm = TRUE)
        }

        # Store baseline val
        temp <- H2OMultinomialAUC(validate,
                                 best_model,
                                 targetColNum = 1,
                                 targetName = Construct[i,1][[1]])

        # Store micro auc
        data.table::set(grid_tuned_paths, i = i, j = 3L, value = val)
        data.table::set(grid_tuned_paths, i = i, j = 4L, value = temp)

        # Calibration plot
        out1 <- EvalPlot(
          xxx,
          PredictionColName = "Preds",
          TargetColName  = "Act",
          GraphType        = "calibration",
          PercentileBucket      = 0.05,
          aggrfun     = function(x)
            base::mean(x, na.rm = TRUE)
        )
        out1 <- out1 + ggplot2::ggtitle(
          paste0("Calibration Evaluation Plot ",
                 toupper(multinomialMetric),
                 ": ",
                 round(val,4))
        )
        if(SaveToFile == TRUE) {
          ggplot2::ggsave(paste0(model_path,
                                 "/CalP_",
                                 Construct[i, 5][[1]],
                                 ".png"))
        }

      } else {
        predsMulti <- h2o::h2o.predict(bl_model, newdata = validate)
        col <- Construct[i, 1][[1]]
        xx <-
          data.table::as.data.table(h2o::h2o.cbind(validate[, col],
                                                   predsMulti))
        if (Construct[i, 12][[1]]) {
          calib <- data.table::as.data.table(h2o::h2o.cbind(validate,
                                                            preds))
          if(SaveToFile == TRUE) {
            save(calib, file = paste0(model_path,
                                      "/",
                                      Construct[i, 5][[1]],
                                      "_Validation.Rdata"))
          }
        }
        N <- (ncol(xx) - 2)
        data <- eval(parse(text = Construct[i, 7][[1]]))
        for (lev in levels(data[[Construct[i, 1][[1]]]])) {
          xx[, paste0("V", lev) := ifelse(xx[[1]] %in% lev, 1, 0)]
        }
        RemoveCols <- names(xx)[1:2]
        KeepCols   <- names(xx)[3:length(names(xx))]
        xx[, (RemoveCols) := NULL]
        store <- list()
        for (k in seq_len(N)) {
          j <- k + N
          temp <- cbind(xx[, ..k], xx[, ..j])
          data.table::setnames(temp, KeepCols[k], "Preds")
          data.table::setnames(temp, KeepCols[j], "Act")
          store[[k]] <- temp
        }
        xxx <- data.table::rbindlist(store)

        # Multinomial metric
        if(multinomialMetric == "auc") {
          val <- H2OMultinomialAUC(validate,
                                   bl_model,
                                   targetColNum = 1,
                                   targetName = Construct[i,1][[1]])
        } else {
          xx <- data.table::as.data.table(h2o::h2o.cbind(
            validate[, 1],
            h2o::h2o.predict(bl_model,
                             newdata = validate)[,1]))
          names(xx)
          val <- mean(xx[, Accuracy := as.numeric(
            ifelse(
              get(
                Construct[i,1][[1]]) == predict, 1, 0))][["Accuracy"]],
            na.rm = TRUE)
        }

        # Calibration plot
        out1 <- EvalPlot(
          xxx,
          PredictionColName = "Preds",
          TargetColName  = "Act",
          GraphType        = "calibration",
          PercentileBucket      = 0.05,
          aggrfun     = function(x)
            base::mean(x, na.rm = TRUE)
        )
        out1 <- out1 + ggplot2::ggtitle(
          paste0("Calibration Evaluation Plot ",
                 toupper(multinomialMetric),
                 ": ",
                 round(val,4))
          )
        if(SaveToFile == TRUE) {
          ggplot2::ggsave(paste0(model_path,
                                 "/CalP_",
                                 Construct[i, 5][[1]], ".png"))
        }
      }

      # Store micro auc
      data.table::set(grid_tuned_paths, i = i, j = 4L, value = val)
    }

    #######################################
    # Partial dependence calibration plots
    #######################################

    if (Construct[i, 13][[1]] >= 1) {
      VIMP <- VIMP[!is.na(VIMP[, 2][[1]])]
      rows <- nrow(VIMP)
      cols <- VIMP[1:min(Construct[i, 13][[1]], rows), 1][[1]]
      calibr <- list()
      boxplotr <- list()
      j <- 0
      if (!(tolower(Construct[i, 2][[1]]) %in% c("multinomial"))) {
        for (col in cols) {
          j <- j + 1
          if (tolower(Construct[i, 2][[1]]) == "quantile") {
            out1 <- tryCatch({
              ParDepCalPlots(
                calib,
                PredictionColName = predName,
                TargetColName  = Construct[i, 1][[1]],
                IndepVar    = col,
                GraphType        = "calibration",
                PercentileBucket      = 0.05,
                FactLevels  = 10,
                Function    = function(x)
                  quantile(x,
                           probs = Construct[i, 4][[1]],
                           na.rm = TRUE)
              )
            },
            error = function(x)
              "skip")
          } else {
            out1 <- tryCatch({
              ParDepCalPlots(
                calib,
                PredictionColName = predName,
                TargetColName  = Construct[i, 1][[1]],
                IndepVar    = col,
                GraphType        = "calibration",
                PercentileBucket      = 0.05,
                FactLevels  = 10,
                Function    = function(x)
                  base::mean(x, na.rm = TRUE)
              )
            },
            error = function(x)
              "skip")
          }

          # Add threshold line to charts
          if (tolower(Construct[i, 2][[1]]) %in% c("quasibinomial",
                                                   "binomial",
                                                   "bernoulli")) {
            if (exists("Thresh")) {
              out1 <- out1 + ggplot2::geom_hline(yintercept = Thresh)
            }
            calibr[[paste0(col)]] <- out1
          } else {
            calibr[[paste0(col)]] <- out1
          }

          # Expected value regression
          if (!(tolower(Construct[i, 2][[1]]) %in% c("quasibinomial",
                                                     "binomial",
                                                     "bernoulli"))) {
            boxplotr[[paste0(col)]] <- tryCatch({
              ParDepCalPlots(
                calib,
                PredictionColName = predName,
                TargetColName  = Construct[i, 1][[1]],
                IndepVar    = col,
                GraphType        = "boxplot",
                PercentileBucket      = 0.05,
                FactLevels  = 10
              )
            },
            error = function(x)
              "skip")
          }
        }

        # Save output
        if (!(tolower(Construct[i, 2][[1]]) %in% c("quasibinomial",
                                                   "binomial",
                                                   "bernoulli"))) {
          if(SaveToFile == TRUE) {
            save(
              boxplotr,
              file = paste0(
                model_path,
                "/",
                Construct[i, 5][[1]],
                "_ParDepCalBoxPlots.Rdata"
              )
            )
          }
          save(calibr,
               file = paste0(model_path,
                             "/", Construct[i, 5][[1]],
                             "_ParDepCalPlots.Rdata"))
        }
      }
    }

    # Save grid_tuned_paths
    if(SaveToFile == TRUE) {
      save(grid_tuned_paths,
           file = paste0(model_path, "/grid_tuned_paths.Rdata"))
    }

    # Clear H2O environment between runs
    h2o::h2o.rm(data_h2o)
    if(!Construct[i,SupplyData][[1]]) {
      h2o::h2o.rm(data_train)
    }
    h2o::h2o.rm(train)
    h2o::h2o.rm(validate)
    if (Construct[i, 11][[1]]) {
      h2o::h2o.rm(best_model)
    }
    if (Construct[i, 6][[1]] != "automl") {
      h2o::h2o.rm(bl_model)
    }
    h2o::h2o.rm(preds)
    h2o::h2o.shutdown(prompt = FALSE)

    # Clear R environment between runs
    if (Construct[i, 11][[1]]) {
      if (Construct[i, 2][[1]] != "multinomial" &
          Construct[i, 21][[1]] == TRUE) {
        rm(
          grid,
          Grid_Out,
          cc,
          dd,
          VIMP,
          calibration,
          features,
          target,
          save_model
        )
      } else {
        rm(grid,
           Grid_Out,
           cc,
           dd,
           VIMP,
           features,
           target,
           predsMulti)
      }
    } else {
      if (Construct[i, 2][[1]] != "multinomial") {
        rm(dd,
           VIMP,
           calibration,
           features,
           target,
           save_model)
      } else {
        rm(dd, VIMP, features, target)
      }
    }

    # Remove data if no longer needed
    if (i > 1) {
      if (Construct[i, 7][[1]] != Construct[(i - 1), 7][[1]]) {
        eval(parse(text = paste0("rm(", Construct[(i - 1), 7][[1]], ")")))
      }
    }
  }}, error = function(x) h2o::h2o.shutdown(prompt = FALSE))
  if(ReturnObjects) {
    return(list(Construct = Construct, GridTunedPaths = grid_tuned_paths))
  }
}

#' AutoH2OScoring is the complement of AutoH20Modeler.
#'
#' AutoH2OScoring is the complement of AutoH20Modeler. Use this for scoring models. You can score regression, quantile regression, classification, multinomial, clustering, and text models (built with the Word2VecModel function). You can also use this to score multioutcome models so long as the there are two models: one for predicting the count of outcomes (a count outcome in character form) and a multinomial model on the label data. You will want to ensure you have a record for each label in your training data in (0,1) as factor form.
#'
#' @author Adrian Antico
#' @family Supervised Learning
#' @param Features This is a data.table of features for scoring.
#' @param GridTuneRow Numeric. The row numbers of grid_tuned_paths, KMeansModelFile, or StoreFile containing the model you wish to score
#' @param ScoreMethod "Standard" or "Mojo": Mojo is available for supervised models; use standard for all others
#' @param TargetType "Regression", "Classification", "Multinomial", "MultiOutcome", "Text", "Clustering". MultiOutcome must be two multinomial models, a count model (the count of outcomes, as a character value), and the multinomial model predicting the labels.
#' @param ClassVals Choose from "p1", "Probs", "Label", or "All" for classification and multinomial models.
#' @param NThreads Number of available threads for H2O
#' @param MaxMem Amount of memory to dedicate to H2O
#' @param JavaOptions Modify to your machine if the default doesn't work
#' @param SaveToFile Set to TRUE if you want your model scores saved to file.
#' @param FilesPath Set this to the folder where your models and model files are saved
#' @param H20ShutDown TRUE to shutdown H2O after the run. Use FALSE if you will be repeatedly scoring and shutdown somewhere else in your environment.
#' @return Returns a list of predicted values. Each list element contains the predicted values from a single model predict call.
#' @examples
#' \donttest{
#' # Multinomial Example
#' Correl <- 0.85
#' aa <- data.table::data.table(target = runif(1000))
#' aa[, x1 := qnorm(target)]
#' aa[, x2 := runif(1000)]
#' aa[, Independent_Variable1 := log(pnorm(Correl * x1 +
#'                                           sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable2 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable3 := exp(pnorm(Correl * x1 +
#'                                           sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable4 := exp(exp(pnorm(Correl * x1 +
#'                                               sqrt(1-Correl^2) * qnorm(x2))))]
#' aa[, Independent_Variable5 := sqrt(pnorm(Correl * x1 +
#'                                            sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable6 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^0.10]
#' aa[, Independent_Variable7 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^0.25]
#' aa[, Independent_Variable8 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^0.75]
#' aa[, Independent_Variable9 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^2]
#' aa[, Independent_Variable10 := (pnorm(Correl * x1 +
#'                                         sqrt(1-Correl^2) * qnorm(x2)))^4]
#' aa[, ':=' (x1 = NULL, x2 = NULL)]
#' aa[, target := as.factor(ifelse(target < 0.33,"A",ifelse(target < 0.66, "B","C")))]
#' Construct <- data.table::data.table(Targets = rep("target",3),
#'                                     Distribution    = c("multinomial",
#'                                                         "multinomial",
#'                                                         "multinomial"),
#'                                     Loss            = c("logloss","logloss","CrossEntropy"),
#'                                     Quantile        = rep(NA,3),
#'                                     ModelName       = c("GBM","DRF","DL"),
#'                                     Algorithm       = c("gbm",
#'                                                         "randomForest",
#'                                                         "deeplearning"),
#'                                     dataName        = rep("aa",3),
#'                                     TargetCol       = rep(c("1"),3),
#'                                     FeatureCols     = rep(c("2:11"),3),
#'                                     CreateDate      = rep(Sys.time(),3),
#'                                     GridTune        = rep(FALSE,3),
#'                                     ExportValidData = rep(TRUE,3),
#'                                     ParDep          = rep(NA,3),
#'                                     PD_Data         = rep("All",3),
#'                                     ThreshType      = rep("f1",3),
#'                                     FSC             = rep(0.001,3),
#'                                     tpProfit        = rep(NA,3),
#'                                     tnProfit        = rep(NA,3),
#'                                     fpProfit        = rep(NA,3),
#'                                     fnProfit        = rep(NA,3),
#'                                     SaveModel       = rep(FALSE,3),
#'                                     SaveModelType   = c("Mojo","mojo","mojo"),
#'                                     PredsAllData    = rep(TRUE,3),
#'                                     TargetEncoding  = rep(NA,3),
#'                                     SupplyData      = rep(FALSE,3))
#'
#' AutoH2OModeler(Construct,
#'                max_memory = "28G",
#'                ratios = 0.75,
#'                BL_Trees = 500,
#'                nthreads = 5,
#'                model_path = NULL,
#'                MaxRuntimeSeconds = 3600,
#'                MaxModels = 30,
#'                TrainData = NULL,
#'                TestData  = NULL,
#'                SaveToFile = FALSE,
#'                ReturnObjects = TRUE)
#'
#' N <- 3
#' data <- AutoH2OScoring(Features     = aa,
#'                        GridTuneRow  = c(1:N),
#'                        ScoreMethod  = "standard",
#'                        TargetType   = rep("multinomial",N),
#'                        ClassVals    = rep("Probs",N),
#'                        NThreads     = 6,
#'                        MaxMem       = "28G",
#'                        JavaOptions  = '-Xmx1g -XX:ReservedCodeCacheSize=256m',
#'                        SaveToFile   = FALSE,
#'                        FilesPath    = NULL,
#'                        H20ShutDown  = rep(FALSE,N))
#'}
#' @export
AutoH2OScoring <- function(Features     = data,
                           GridTuneRow  = c(1:3),
                           ScoreMethod  = "Standard",
                           TargetType   = rep("multinomial",3),
                           ClassVals    = rep("probs",3),
                           NThreads     = 6,
                           MaxMem       = "28G",
                           JavaOptions  = '-Xmx1g -XX:ReservedCodeCacheSize=256m',
                           SaveToFile   = FALSE,
                           FilesPath    = NULL,
                           H20ShutDown  = rep(FALSE,3)) {

  # If FilesPath is NULL, skip function
  if(is.null(FilesPath)) {
    # Ensure packages are available
    requireNamespace('data.table', quietly = FALSE)

    # Only run text or other models types
    if(any(tolower(TargetType) %in% "clustering") &
       any(tolower(TargetType) %in% "text") &
       any(tolower(TargetType) %in% c("regression",
                                      "classification",
                                      "multinomial",
                                      "multioutcome"))) {
      warning("Run either text models, supervised models,
         or unsupervised models, but only one")
    }

    # Import grid_tuned_paths or StoreFile
    if(any(tolower(TargetType) %in% c("regression",
                                      "classification",
                                      "multinomial",
                                      "multioutcome"))) {
      load(paste0(FilesPath, "/grid_tuned_paths.Rdata"))
    } else if (any(tolower(TargetType) %in% "text")) {
      load(paste0(FilesPath, "/StoreFile.Rdata"))
    } else if (any(tolower(TargetType) %in% "clustering")) {
      load(paste0(FilesPath, "/KMeansModelFile.Rdata"))
    } else {
      warning("TargetType not a valid option")
    }

    # Ensure GridTuneRow is not out of bounds
    if(any(tolower(TargetType) %in% c("regression",
                                      "classification",
                                      "multinomial",
                                      "multioutcome"))) {
      if(nrow(grid_tuned_paths) < max(GridTuneRow)) {
        warning("GridTuneRow is greater than
          the number of rows in grid_tuned_paths")
      }
    } else if (any(tolower(TargetType) %in% "text")) {
      if(nrow(StoreFile) < max(GridTuneRow)) {
        warning("GridTuneRow is greater than
          the number of rows in StoreFile")
      }
    } else if(any(tolower(TargetType) %in% "clustering")) {
      if(nrow(KMeansModelFile) < max(GridTuneRow)) {
        warning("GridTuneRow is greater than
            the number of rows in KMeansModelFile")
      }
    } else {
      warning("TargetType not a valid option")
    }

    ScoresList <- list()
    for(i in as.integer(seq_along(GridTuneRow))) {
      # Scoring
      if(tolower(ScoreMethod) == "mojo") {
        if(tolower(TargetType[i]) == "multinomial") {
          if(tolower(ClassVals[i]) == c("probs")) {
            if(SaveToFile) {
              data.table::fwrite(Features, file.path(FilesPath,'Features.csv'))
            }
            Scores <- data.table::as.data.table(
              h2o::h2o.mojo_predict_csv(
                input_csv_path = file.path(FilesPath,'Features.csv'),
                mojo_zip_path = grid_tuned_paths[i,2][[1]],
                java_options = JavaOptions,
                genmodel_jar_path = grid_tuned_paths[i,6][[1]],
                verbose = FALSE)[,-1])
          } else if(tolower(ClassVals[i]) == "label") {
            if(SaveToFile) {
              data.table::fwrite(Features, file.path(FilesPath,'Features.csv'))
            }
            Scores <- data.table::as.data.table(
              h2o::h2o.mojo_predict_csv(
                input_csv_path = file.path(FilesPath,'Features.csv'),
                mojo_zip_path = grid_tuned_paths[i,2][[1]],
                java_options = JavaOptions,
                genmodel_jar_path = grid_tuned_paths[i,6][[1]],
                verbose = FALSE)[,1])
            data.table::setnames(Scores, "predict","Class")
          } else if (tolower(ClassVals[i]) == "all") {
            if(SaveToFile) {
              data.table::fwrite(Features, file.path(FilesPath,'Features.csv'))
            }
            Scores <- data.table::as.data.table(
              h2o::h2o.mojo_predict_csv(
                input_csv_path = file.path(FilesPath,'Features.csv'),
                mojo_zip_path = grid_tuned_paths[i,2][[1]],
                java_options = JavaOptions,
                genmodel_jar_path = grid_tuned_paths[i,6][[1]],
                verbose = FALSE))
            data.table::setnames(Scores, "predict","Class")
          } else {
            warning("ClassVals can only be Probs, Label or All")
          }
        } else if(tolower(TargetType[i]) == "classification") {
          if(tolower(ClassVals[i]) == c("p1")) {
            if(SaveToFile) {
              data.table::fwrite(Features, file.path(FilesPath,'Features.csv'))
            }
            Scores <- data.table::as.data.table(
              h2o::h2o.mojo_predict_csv(
                input_csv_path = file.path(FilesPath,'Features.csv'),
                mojo_zip_path = grid_tuned_paths[i,2][[1]],
                java_options = JavaOptions,
                genmodel_jar_path = grid_tuned_paths[i,6][[1]],
                verbose = FALSE)[,3])
          } else if(tolower(ClassVals[i]) == c("probs")) {
            if(SaveToFile) {
              data.table::fwrite(Features, file.path(FilesPath,'Features.csv'))
            }
            Scores <- data.table::as.data.table(
              h2o::h2o.mojo_predict_csv(
                input_csv_path = file.path(FilesPath,'Features.csv'),
                mojo_zip_path = grid_tuned_paths[i,2][[1]],
                java_options = JavaOptions,
                genmodel_jar_path = grid_tuned_paths[i,6][[1]],
                verbose = FALSE)[,-1])
          } else if(tolower(ClassVals[i]) == "label") {
            data.table::fwrite(Features, file.path(FilesPath,'Features.csv'))
            Scores <- data.table::as.data.table(
              h2o::h2o.mojo_predict_csv(
                input_csv_path = file.path(FilesPath,'Features.csv'),
                mojo_zip_path = grid_tuned_paths[i,2][[1]],
                java_options = JavaOptions,
                genmodel_jar_path = grid_tuned_paths[i,6][[1]],
                verbose = FALSE)[,1])
            data.table::setnames(Scores, "predict","Class")
          } else if(tolower(ClassVals[i]) == "all") {
            if(SaveToFile) {
              data.table::fwrite(Features, file.path(FilesPath,'Features.csv'))
            }
            Scores <- data.table::as.data.table(
              h2o::h2o.mojo_predict_csv(
                input_csv_path = file.path(FilesPath,'Features.csv'),
                mojo_zip_path = grid_tuned_paths[i,2][[1]],
                java_options = JavaOptions,
                genmodel_jar_path = grid_tuned_paths[i,6][[1]],
                verbose = FALSE))
            data.table::setnames(Scores, "predict","Class")
          } else {
            warning("ClassVals can only be Probs, Label or All")
          }
        } else if(tolower(TargetType[i]) == "regression") {
          if(SaveToFile) {
            data.table::fwrite(Features, file.path(FilesPath,'Features.csv'))
          }
          Scores <- data.table::as.data.table(
            h2o::h2o.mojo_predict_csv(
              input_csv_path = file.path(FilesPath,'Features.csv'),
              mojo_zip_path = grid_tuned_paths[i,2][[1]],
              java_options = JavaOptions,
              genmodel_jar_path = grid_tuned_paths[i,6][[1]],
              verbose = FALSE))
        } else if(tolower(TargetType[i]) == "text") {
          keep <- StoreFile[i,1][[1]]
          temp <- AutoH2OTextPrepScoring(data = Features[, ..keep],
                                         string = StoreFile[i,1][[1]])
          if(SaveToFile) {
            data.table::fwrite(Features, file.path(FilesPath,'Features.csv'))
          }
          Scores <- data.table::as.data.table(
            h2o::h2o.mojo_predict_csv(
              input_csv_path = file.path(FilesPath,'Features.csv'),
              mojo_zip_path = StoreFile[i,2][[1]],
              java_options = JavaOptions,
              genmodel_jar_path = StoreFile[i,3][[1]],
              verbose = FALSE))
        } else if(tolower(TargetType[i]) == "multioutcome") {
          if(SaveToFile) {
            data.table::fwrite(Features, file.path(FilesPath,'Features.csv'))
          }
          Counts <- as.numeric(
            as.character(
              h2o::h2o.mojo_predict_csv(
                input_csv_path = file.path(FilesPath,'Features.csv'),
                mojo_zip_path = grid_tuned_paths[i,2][[1]],
                java_options = JavaOptions,
                genmodel_jar_path = grid_tuned_paths[i,6][[1]],
                verbose = FALSE)))
          if(SaveToFile) {
            data.table::fwrite(Features, paste0(FilesPath,"/Features.csv"))
          }
          Temp <- data.table::as.data.table(
            h2o::h2o.mojo_predict_csv(
              input_csv_path = file.path(FilesPath,'Features.csv'),
              mojo_zip_path = grid_tuned_paths[i,2][[1]],
              java_options = JavaOptions,
              genmodel_jar_path = grid_tuned_paths[i,6][[1]],
              verbose = FALSE))
          Vals <- names(sort(Temp[1,2:ncol(Temp)], decreasing = TRUE))
          Scores <- paste0(Vals, collapse = " ")
          preds$ModelName[i] <- grid_tuned_paths[i,1][[1]]
          preds$Scores[i] <- Scores
        } else {
          warning("TargetType is not Multinomial,
          Classification, Regression, or Text")
        }
      } else if(tolower(ScoreMethod) == "standard") {

        # H2O Startup function
        startH2o <- function(){
          h2o::h2o.init(nthreads     = NThreads,
                        max_mem_size = MaxMem)
        }
        # Check if H2O is running
        tryCatch(expr = {h2o::h2o.init(startH2O = FALSE)},
                 error = function(e){startH2o()})

        # Load model
        if(tolower(TargetType[i]) == "text") {
          model <- h2o::h2o.loadModel(path = StoreFile[i,Path])
        } else if (TargetType[i] != "clustering"){
          model <- h2o::h2o.loadModel(path = grid_tuned_paths[i,Path])
        } else {
          KMeans <- h2o::h2o.loadModel(path = KMeansModelFile[i+1,FilePath1])
        }
        # Load Features
        if(i == 1 && tolower(TargetType[i]) != "text") {
          if(tolower(TargetType[i]) == "clustering") {
            x <- c()
            z <- 0
            for(nam in names(Features)) {
              if(is.factor(Features[1,get(nam)]) |
                 is.character(Features[1,get(nam)])) {
                z <- z + 1
                x[z] <- nam
              }
            }
            features <- data.table::copy(Features)
            features <- DummifyDT(features,
                                  cols = x,
                                  KeepFactorCols = FALSE,
                                  OneHot = FALSE,
                                  ClustScore = TRUE)
            features <- h2o::as.h2o(features)
          } else {
            features <- h2o::as.h2o(Features)
          }
        }
        if(tolower(TargetType[i]) == "multinomial") {
          if(tolower(ClassVals[i]) == "probs") {
            Scores <- data.table::as.data.table(
              h2o::h2o.predict(model,
                               newdata = features)[,-1])
          } else if(tolower(ClassVals[i]) == "label") {
            Scores <- data.table::as.data.table(
              h2o::h2o.predict(model,
                               newdata = features)[,1])
            data.table::setnames(Scores, "predict","Class")
          } else if(tolower(ClassVals[i]) == "all") {
            Scores <- data.table::as.data.table(
              h2o::h2o.predict(model,
                               newdata = features))
            data.table::setnames(Scores, "predict","Class")
          } else {
            warning("ClassVals can only be Probs, Label, or All")
          }
        } else if(tolower(TargetType[i]) == "classification") {
          if(tolower(ClassVals[i]) == "p1") {
            Scores <- data.table::as.data.table(
              h2o::h2o.predict(model,
                               newdata = features)[,3])
          } else if(tolower(ClassVals[i]) == "probs") {
            Scores <- data.table::as.data.table(
              h2o::h2o.predict(model,
                               newdata = features)[,-1])
          } else if(tolower(ClassVals[i]) == "label") {
            Scores <- data.table::as.data.table(
              h2o::h2o.predict(model,
                               newdata = features)[,1])
            data.table::setnames(Scores, "predict","Class")
          } else if(tolower(ClassVals[i]) == "all") {
            Scores <- data.table::as.data.table(
              h2o::h2o.predict(model,
                               newdata = features))
            data.table::setnames(Scores, "predict","Class")
          } else {
            warning("ClassVals can only be Probs, Label, or All")
          }
        } else if(tolower(TargetType[i]) == "regression") {
          Scores <- data.table::as.data.table(
            h2o::h2o.predict(model,
                             newdata = features)[,1])
        } else if(tolower(TargetType[i]) == c("text")) {
          name <- StoreFile[i, ModelName][[1]]
          data <- AutoH2OTextPrepScoring(data = Features,
                                         string = name,
                                         NThreads = NThreads,
                                         MaxMem = MaxMem)
          Scores <- data.table::as.data.table(
            h2o::h2o.transform(model,
                               words = data,
                               aggregate_method = "AVERAGE"))
          setnames(Scores, names(Scores), paste0(name,
                                                 "_",
                                                 names(Scores)))
          Features <- cbind(Features[, paste0(name) := NULL], Scores)
        } else if(tolower(TargetType[i]) == "multioutcome") {
          Counts <- data.table::as.data.table(
            h2o::h2o.predict(model,
                             newdata = features)[1,1])
          Temp <- data.table::as.data.table(
            h2o::h2o.predict(model,
                             newdata = features))
          Vals <- names(sort(Temp[1,2:ncol(Temp)], decreasing = TRUE))
          Scores <- paste0(Vals, collapse = " ")
        } else if(tolower(TargetType[i]) == "clustering") {
          load(file = KMeansModelFile[i,FilePath1][[1]])
          load(file = KMeansModelFile[i,FilePath2][[1]])
          NewGLRM <- h2o::h2o.glrm(training_frame = features, init = "User", user_y = fitY)
          x_raw <- h2o::h2o.getFrame(NewGLRM@model$representation_name)
          Scores <- data.table::as.data.table(
            h2o::h2o.predict(object = KMeans,
                             newdata = x_raw))
          Scores <- cbind(data.table::as.data.table(
            Scores),
            Features)
        } else {
          warning("TargetType is not Multinomial,
          Classification, Regression, Text, Multioutcome,
          or Clustering.")
        }
      } else {
        warning("ScoreMethod must be Standard or Mojo")
      }
      if(H20ShutDown[i] && tolower(ScoreMethod) == "standard") {
        h2o::h2o.shutdown(prompt = FALSE)
      }
      if(any(tolower(TargetType) == "text")) {
        ScoresList <- Features
      } else {
        ScoresList[[i]] <- Scores
      }
    }
    return(ScoresList)
  }
}

#' For NLP work
#'
#' This function tokenizes text data
#' @author Adrian Antico
#' @family Misc
#' @param data The text data
#' @examples
#' \donttest{
#' data <- tokenizeH2O(data = data[["StringColumn"]])
#' }
#' @export
tokenizeH2O <- function(data) {
  # Ensure packages are available
  requireNamespace('data.table', quietly = FALSE)
  data <- h2o::as.h2o(data, col.types = c("String"))
  tokenized <- h2o::h2o.tokenize(data, "\\\\W+")
  tokenized.lower <- h2o::h2o.tolower(tokenized)
  tokenized.words <-
    tokenized.lower[h2o::h2o.grep("[0-9]",
                                  tokenized.lower,
                                  invert = TRUE,
                                  output.logical = TRUE), ]
  tokenized.words
}

#' Automated word2vec data generation via H2O
#'
#' This function allows you to automatically build a word2vec model and merge the data onto your supplied dataset
#' @author Adrian Antico
#' @family Feature Engineering
#' @param data Source data table to merge vects onto
#' @param stringCol A string name for the column to convert via word2vec
#' @param KeepStringCol Set to TRUE if you want to keep the original string column that you convert via word2vec
#' @param model_path A string path to the location where you want the model and metadata stored
#' @param vects The number of vectors to retain from the word2vec model
#' @param SaveStopWords Set to TRUE to save the stop words used
#' @param MinWords For H2O word2vec model
#' @param WindowSize For H2O word2vec model
#' @param Epochs For H2O word2vec model
#' @param StopWords For H2O word2vec model
#' @param SaveModel Set to "standard" to save normally; set to "mojo" to save as mojo. NOTE: while you can save a mojo, I haven't figured out how to score it in the AutoH20Scoring function.
#' @param Threads Number of available threads you want to dedicate to model building
#' @param MaxMemory Amount of memory you want to dedicate to model building
#' @param SaveOutput Set to TRUE to save your models to file
#' @examples
#' \donttest{
#' data <- AutoWord2VecModeler(data,
#'                             stringCol = c("Text_Col1",
#'                                           "Text_Col2"),
#'                             KeepStringCol = FALSE,
#'                             model_path = NULL,
#'                             vects = 100,
#'                             SaveStopWords = FALSE,
#'                             MinWords = 1,
#'                             WindowSize = 1,
#'                             Epochs = 25,
#'                             StopWords = NULL,
#'                             SaveModel = "standard",
#'                             Threads = 6,
#'                             MaxMemory = "28G",
#'                             SaveOutput = TRUE)
#'}
#' @export
AutoWord2VecModeler <- function(data,
                                stringCol     = c("Text_Col1",
                                                  "Text_Col2"),
                                KeepStringCol = FALSE,
                                model_path    = NULL,
                                vects         = 100,
                                SaveStopWords = FALSE,
                                MinWords      = 1,
                                WindowSize    = 12,
                                Epochs        = 25,
                                StopWords     = NULL,
                                SaveModel     = "standard",
                                Threads       = 6,
                                MaxMemory     = "28G",
                                SaveOutput    = FALSE) {

  # Ensure packages are available
  requireNamespace('data.table', quietly = FALSE)

  # Ensure data is a data.table
  if(!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }

  # Create storage file
  N <- length(stringCol)
  StoreFile <-
    data.table::data.table(ModelName = rep("a", N), Path = rep("a", N), Jar = rep("a", N))
  i <- 0

  # Loop through all the string columns
  for (string in stringCol) {

    # Ensure stringCol is character (not factor)
    if(!is.character(data[[eval(string)]])) {
      data[, eval(string) := as.character(get(string))]
    }

    # word2vec time
    i <- as.integer(i + 1)
    Sys.sleep(10)
    h2o::h2o.init(nthreads = Threads, max_mem_size = MaxMemory)

    # It is important to remove "\n" --
    data[, eval(string) := gsub("  ", " ", get(string))]
    data[, eval(string) := stringr::str_replace_all(get(string), "[[:punct:]]", "")]
    data2 <- data[, .(get(string))]

    # Tokenize
    tokenized_words <- tokenizeH2O(data2)
    rm(data2)

    # Build model
    w2v.model <- h2o::h2o.word2vec(
      tokenized_words,
      model_id           = string,
      word_model         = "SkipGram",
      norm_model         = "HSM",
      vec_size           = vects,
      min_word_freq      = MinWords,
      window_size        = WindowSize,
      init_learning_rate = 0.025,
      sent_sample_rate   = 0.05,
      epochs             = Epochs
    )

    # Save model
    if(SaveOutput) {
      if (tolower(SaveModel) == "standard") {
        w2vPath <-
          h2o::h2o.saveModel(w2v.model, path = model_path, force = TRUE)
        data.table::set(StoreFile,
                        i = i,
                        j = 1L,
                        value = string)
        data.table::set(StoreFile,
                        i = i,
                        j = 2L,
                        value = w2vPath)
        data.table::set(StoreFile,
                        i = i,
                        j = 3L,
                        value = "NA")
        save(StoreFile, file = paste0(model_path, "/StoreFile.Rdata"))
      } else {
        w2vPath <-
          h2o::h2o.saveMojo(w2v.model, path = model_path, force = TRUE)
        h2o::h2o.download_mojo(
          model = w2v.model,
          path = model_path,
          get_genmodel_jar = TRUE,
          genmodel_path = model_path,
          genmodel_name = string
        )
        data.table::set(StoreFile,
                        i = i,
                        j = 1L,
                        value = string)
        data.table::set(StoreFile,
                        i = i,
                        j = 2L,
                        value = w2vPath)
        data.table::set(StoreFile,
                        i = i,
                        j = 3L,
                        value = paste0(model_path, "/", string))
        save(StoreFile, file = paste0(model_path, "/StoreFile.Rdata"))
      }
    }

    # Score model
    all_vecs <-
      h2o::h2o.transform(w2v.model, tokenized_words,
                         aggregate_method = "AVERAGE")

    # Convert to data.table
    all_vecs <- data.table::as.data.table(all_vecs)
    data <- data.table::data.table(cbind(data, all_vecs))

    # Remove string cols
    if (!KeepStringCol) {
      data[, eval(string) := NULL]
    }

    # Replace Colnames
    cols <- names(data[, (ncol(data) - vects + 1):ncol(data)])
    for (c in cols) {
      data[, paste0(string, "_", c) := get(c)]
      data[, eval(c) := NULL]
    }

    # Final Prep
    h2o::h2o.rm(w2v.model)
    h2o::h2o.shutdown(prompt = FALSE)
  }
  return(data)
}

#' Automated Word Frequency and Word Cloud Creation
#'
#' This function builds a word frequency table and a word cloud. It prepares data, cleans text, and generates output.
#' @author Adrian Antico
#' @family Misc
#' @param data Source data table
#' @param TextColName A string name for the column
#' @param GroupColName Set to NULL to ignore, otherwise set to Cluster column name (or factor column name)
#' @param GroupLevel Must be set if GroupColName is defined. Set to cluster ID (or factor level)
#' @param RemoveEnglishStopwords Set to TRUE to remove English stop words, FALSE to ignore
#' @param Stemming Set to TRUE to run stemming on your text data
#' @param StopWords Add your own stopwords, in vector format
#' @examples
#' data <- data.table::data.table(
#' DESCR = c("Uwe, Uwe, Uwe, Uwe, Uwe, Uwe, Uwe, Uwe, Uwe, Uwe, Uwe,
#'            Ligges, Ligges, Ligges, Ligges, Ligges, Ligges, Ligges,
#'            sucks, sucks, sucks, sucks, sucks, sucks, smug, smug, smug, smug,
#'            smug, smug, smug, smug, smug, smug, smug, smug, smug, smug, smug,
#'            computer, computer, computer, computer, computer, computer,
#'            Data, Data, Data, Data, Data, Data, Data, science, science,
#'            science, science, science, science, science, science, science,
#'            science, science, science, science, science, science, science,
#'            science, science, science, science, science, games, games,
#'            games, games, games, games, games, games, games, games"))
#' data <- AutoWordFreq(data,
#'                      TextColName = "DESCR",
#'                      GroupColName = NULL,
#'                      GroupLevel = NULL,
#'                      RemoveEnglishStopwords = FALSE,
#'                      Stemming = FALSE,
#'                      StopWords = c("Bla"))
#' @export
AutoWordFreq <- function(data,
                         TextColName = "DESCR",
                         GroupColName = "ClusterAllNoTarget",
                         GroupLevel = 0,
                         RemoveEnglishStopwords = TRUE,
                         Stemming = TRUE,
                         StopWords = c("bla",
                                       "bla2")) {

  # Ensure packages are available
  requireNamespace('data.table', quietly = FALSE)

  # Check data.table
  if(!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }

  # Ensure stringCol is character (not factor)
  if(!is.character(data[[eval(TextColName)]])) data[, eval(TextColName) := as.character(get(TextColName))]

  # Prepare data
  if (is.null(GroupColName)) {
    desc <- tm::Corpus(tm::VectorSource(data[[eval(TextColName)]]))
  } else {
    if(!is.character(data[[GroupColName]])) {
      data[, eval(GroupColName) := as.character(get(GroupColName))]
      desc <- tm::Corpus(tm::VectorSource(
        data[get(GroupColName) == eval(GroupLevel)][[eval(TextColName)]])
      )
    }
  }

  # Clean text
  toSpace <-
    tm::content_transformer(function (x , pattern)
      gsub(pattern, " ", x))
  text <- tm::tm_map(desc, toSpace, "/")
  text <- tm::tm_map(text, toSpace, "@")
  text <- tm::tm_map(text, toSpace, "\\|")

  # Convert the text to lower case
  text <- tm::tm_map(text, tm::content_transformer(tolower))

  # Remove numbers
  text <- tm::tm_map(text, tm::removeNumbers)

  # Remove english common stopwords
  if (RemoveEnglishStopwords)
    text <-
    tm::tm_map(text, tm::removeWords, tm::stopwords("english"))

  # specify your stopwords as a character vector
  text <- tm::tm_map(text, tm::removeWords, StopWords)

  # Remove punctuations
  text <- tm::tm_map(text, tm::removePunctuation)

  # Eliminate extra white spaces
  text <- tm::tm_map(text, tm::stripWhitespace)

  # Text stemming
  if (Stemming)
    text <- tm::tm_map(text, tm::stemDocument)

  # Finalize
  dtm <- tm::TermDocumentMatrix(text)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m), decreasing = TRUE)
  d <- data.table::data.table(word = names(v), freq = v)
  print(head(d, 10))

  # Word Cloud
  print(
    wordcloud::wordcloud(
      words = d$word,
      freq = d$freq,
      min.freq = 1,
      max.words = 200,
      random.order = FALSE,
      rot.per = 0.35,
      colors = RColorBrewer::brewer.pal(8, "Dark2")
    )
  )

  # Return
  return(d)
}

#' AutoH2OTextPrepScoring is for NLP scoring
#'
#' This function returns prepared tokenized data for H2O Word2VecModeler scoring
#' @author Adrian Antico
#' @family Misc
#' @param data The text data
#' @param string The name of the string column to prepare
#' @param MaxMem Amount of memory you want to let H2O utilize
#' @param NThreads The number of threads you want to let H2O utilize
#' @examples
#' \donttest{
#' data <- AutoH2OTextPrepScoring(data = x,
#'                                string = "text_column",
#'                                MaxMem = "28G",
#'                                NThreads = 8)
#' }
#' @export
AutoH2OTextPrepScoring <- function(data,
                                   string,
                                   MaxMem,
                                   NThreads) {

  # Ensure packages are available
  requireNamespace('data.table', quietly = FALSE)

  if(!is.data.table(data)) {
    data <- data.table::as.data.table((data))
  }
  data[, eval(string) := as.character(get(string))]
  h2o::h2o.init(nthreads = NThreads, max_mem_size = MaxMem)

  # It is important to remove "\n" --
  data[, eval(string) := gsub("  ", " ", get(string))]
  data[, eval(string) := stringr::str_replace_all(get(string), "[[:punct:]]", "")]
  data2 <- data[, ..string]

  # Tokenize
  tokenized_words <- RemixAutoML::tokenizeH2O(data2)
  return(tokenized_words)
}

#' Convert transactional data.table to a binary ratings matrix
#'
#' @author Adrian Antico and Douglas Pestana
#' @family Misc
#' @param data This is your transactional data.table. Must include an Entity (typically customer), ProductCode (such as SKU), and a sales metric (such as total sales).
#' @param EntityColName This is the column name in quotes that represents the column name for the Entity, such as customer
#' @param ProductColName This is the column name in quotes that represents the column name for the product, such as SKU
#' @param MetricColName This is the column name in quotes that represents the column name for the metric, such as total sales
#' @param ReturnMatrix Set to FALSE to coerce the object (desired route) or TRUE to return a matrix
#' @return A BinaryRatingsMatrix
#' @examples
#' data <- data.table::data.table(CustomerID = c(1,1,2,2,3,3),
#'                                StockCode = c("A","B","A","A","B","A"),
#'                                TotalSales = c(2,3,4,5,1,2))
#' RatingsMatrix <- RecomDataCreate(data,
#'                                  EntityColName = "CustomerID",
#'                                  ProductColName = "StockCode",
#'                                  MetricColName = "TotalSales",
#'                                  ReturnMatrix = TRUE)
#' @export
RecomDataCreate <- function(data,
                            EntityColName  = "CustomerID",
                            ProductColName = "StockCode",
                            MetricColName  = "TotalSales",
                            ReturnMatrix   = FALSE) {

  # Ensure packages are available
  requireNamespace('data.table', quietly = FALSE)

  # Ensure data is data.table
  if(!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }

  # Ensure EntityColName is character type
  if(!is.character(data[1,get(EntityColName)])) {
    data[, eval(EntityColName) := as.character(get(EntityColName))]
  }

  # Ensure ProductColName is character type
  if(!is.character(data[1,get(ProductColName)])) {
    data[, eval(ProductColName) := as.character(get(ProductColName))]
  }

  # Ensure MtricColName is numeri
  if(!is.numeric(data[1,get(MetricColName)])) {
    data[, eval(MetricColName) := as.numeric(get(MetricColName))]
  }

  # Only keep the necessary columns
  keep <- c(EntityColName, ProductColName, MetricColName)
  data <- data[, ..keep]

  # CREATE BINARY RATING MATRIX-----
  train_data <- data.table::dcast(data,
                                  get(EntityColName) ~ get(ProductColName),
                                  value.var = eval(MetricColName),
                                  fun.aggregate = function(x) sum(!is.na(x)),
                                  fill = 0)

  # Change name back to original
  data.table::setnames(train_data,
                       "EntityColName",
                       eval(EntityColName))

  # Convert Sales data to Binary (60% faster than ifelse)
  for (j in 2:ncol(train_data)) {
    data.table::set(train_data,which(train_data[[j]] > 0), j, 1)
    data.table::set(train_data,which(train_data[[j]] <= 0), j, 0)
  }

  # Store customerID for rownames
  train_data_rownames <- train_data[[eval(EntityColName)]]

  # Remove CustomerID column
  train_data[, eval(EntityColName) := NULL]

  # Convert train to matrix
  train_data_matrix <- as.matrix(train_data)

  # Set rownames
  row.names(train_data_matrix) <- train_data_rownames

  # Return binary rating matrix
  if(ReturnMatrix) {
    return(recommenderlab::coerce(from = train_data_matrix,
                                  to = "BinaryRatingsMatrix"))
  } else {
    return(methods::as(object = train_data_matrix,
                       Class = "binaryRatingMatrix"))
  }
}

#' Automatically build the best recommendere model among models available.
#'
#' This function returns the winning model that you pass onto AutoRecommenderScoring
#' @author Adrian Antico and Douglas Pestana
#' @family Supervised Learning
#' @param data This is your BinaryRatingsMatrix. See function RecomDataCreate
#' @param Partition Choose from "split", "cross-validation", "bootstrap". See evaluationScheme in recommenderlab for details.
#' @param KFolds Choose 2 for traditional train and test. Choose greater than 2 for the number of cross validations
#' @param Ratio The ratio for train and test. E.g. 0.75 for 75 percent data allocated to training
#' @param RatingType Choose from "TopN", "ratings", "ratingMatrix"
#' @param RatingsKeep The total ratings you wish to return. Default is 20.
#' @param SkipModels AssociationRules runs the slowest and may crash your system. Choose from: "AssociationRules","ItemBasedCF","UserBasedCF","PopularItems","RandomItems"
#' @param ModelMetric Choose from "Precision", "Recall", "TPR", or "FPR"
#' @examples
#' \donttest{
#' WinningModel <- AutoRecommender(RatingsMatrix,
#'                                 Partition = "Split",
#'                                 KFolds = 2,
#'                                 Ratio = 0.75,
#'                                 RatingType = "TopN",
#'                                 RatingsKeep = 20,
#'                                 SkipModels = "AssociationRules",
#'                                 ModelMetric = "TPR")
#' }
#' @return The winning model used for scoring in the AutoRecommenderScoring function
#' @export
AutoRecommender <- function(data,
                            Partition   = "Split",
                            KFolds      = 2,
                            Ratio       = 0.75,
                            RatingType  = "TopN",
                            RatingsKeep = 20,
                            SkipModels  = "AssociationRules",
                            ModelMetric = "TPR") {

  # Ensure packages are available
  requireNamespace('data.table', quietly = FALSE)

  # Ensure data is proper
  if(class(data)[1] != "binaryRatingMatrix") {
    warning("data must be of class binaryRatingMatrix")
  }

  # Ensure KFolds is correct
  if(tolower(Partition) == "split") {
    KFolds <- 1
  }

  # Ensure Ratio is proper
  if(abs(Ratio) > 1 | Ratio == 0) {
    warning("Ratio must be a decimal between 0 and 1.
         Default is 0.75")
  }

  # Ensure RatingType is real
  if(tolower(RatingType) == "topn") {
    RatingType <- "topNList"
  } else if(tolower(RatingType) == "ratings") {
    RatingType <- "ratings"
  } else if(tolower(RatingType) == "ratingMatrix") {
    RatingType <- "ratingMatrix"
  }

  # Pick winning model based max TPR for 10th recommendation
  if(tolower(ModelMetric) == "precision") {
    ModelMetric <- "precision"
  } else if(tolower(ModelMetric) == "recall") {
    ModelMetric <- "recall"
  } else if(tolower(ModelMetric) == "tpr") {
    ModelMetric <- "TPR"
  } else if(tolower(ModelMetric) == "fpr") {
    ModelMetric <- "FPR"
  } else {
    warning("ModelMetric not in list of usable metrics")
  }

  # Evaluation setup
  scheme <- recommenderlab::evaluationScheme(
    data,
    method     = tolower(Partition),
    k          = KFolds,
    train      = Ratio,
    given      = 1,
    goodRating = 1)

  # Store algorithms in nested list
  algorithms <- list(
    "RandomItems"  = list(name = "RANDOM",  param = NULL),
    "PopularItems" = list(name = "POPULAR", param = NULL),
    "UserBasedCF" = list(name = "UBCF",    param = NULL),
    "ItemBasedCF" = list(name = "IBCF",    param = NULL),
    "AssociationRules" = list(name = "AR",      param = list(support=0.001, confidence=0.05))
  )

  # Remove all algos in SkipModels
  if(any(tolower(SkipModels) == "associationrules")) {
    algorithms[["AssociationRules"]] <- NULL
  }
  if(any(tolower(SkipModels) == "itembasedcf")) {
    algorithms[["ItemBasedCF"]] <- NULL
  }
  if(any(tolower(SkipModels) == "userbasedcf")) {
    algorithms[["UserBasedCF"]] <- NULL
  }
  if(any(tolower(SkipModels) == "popularitems")) {
    algorithms[["PopularItems"]] <- NULL
  }
  if(any(tolower(SkipModels) == "randomitems")) {
    algorithms[["RandomItems"]] <- NULL
  }
  if(length(algorithms) == 0) {
    warning("You must have at least one algorithm to run")
  }

  # evauluate predicted ratings from each algorithm
  results <- recommenderlab::evaluate(x      = scheme,
                                      method = algorithms,
                                      type   = RatingType,
                                      n      = 1:RatingsKeep)

  # determine winning model - highest TPR for next best 10 products
  # start by averaging Confusion Matrix for all k-fold runs
  n <- length(results)
  store <- list()
  for (i in 1:n) {
    temp <- data.table(recommenderlab::avg(results)[[i]])
    temp[, model := results[[i]]@method]
    temp[, n_products := seq(1:RatingsKeep)]
    store[[i]] <- temp
  }

  # Collect results in one data.table
  x <- data.table::rbindlist(store)

  WinningModel <- x[n_products == 10][
    order(-get(ModelMetric))][1, "model"][[1]]
  return(WinningModel)
}

#' The AutoRecomScoring function scores recommender models from AutoRecommender()
#'
#' This function will take your ratings matrix and model and score your data in parallel.
#' @author Adrian Antico and Douglas Pestana
#' @family Supervised Learning
#' @param data The binary ratings matrix from RecomDataCreate()
#' @param WinningModel The winning model returned from AutoRecommender()
#' @param EntityColName Typically your customer ID
#' @param ProductColName Something like "StockCode"
#' @return Returns the prediction data
#' @examples
#' \donttest{
#' # F(G(Z(x))): AutoRecommenderScoring(AutoRecommender(RecomDataCreate(TransactionData)))
#' Results <- AutoRecommenderScoring(
#'   data = RecomDataCreate(
#'       data,
#'       EntityColName = "CustomerID",
#'       ProductColName = "StockCode",
#'       MetricColName = "TotalSales"),
#'   WinningModel = AutoRecommender(
#'       RecomDataCreate(
#'         data,
#'         EntityColName = "CustomerID",
#'         ProductColName = "StockCode",
#'         MetricColName = "TotalSales"),
#'       Partition = "Split",
#'       KFolds = 2,
#'       Ratio = 0.75,
#'       RatingType = "TopN",
#'       RatingsKeep = 20,
#'       SkipModels = "AssociationRules",
#'       ModelMetric = "TPR"),
#'   EntityColName = "CustomerID",
#'   ProductColName = "StockCode")
#' }
#' @export
AutoRecommenderScoring <- function(data,
                                   WinningModel,
                                   EntityColName  = "CustomerID",
                                   ProductColName = "StockCode") {

  # Ensure packages are available
  requireNamespace('data.table', quietly = FALSE)
  requireNamespace('parallel', quietly = FALSE)
  requireNamespace('foreach', quietly = FALSE)
  requireNamespace('doParallel', quietly = FALSE)

  # Setup winning model and arguments
  if (WinningModel == "AR") {
    recommender <- recommenderlab::Recommender(
      data = data,
      method = "AR",
      parameter = list(
        support = 0.001,
        confidence = 0.05))
  } else {
    recommender <- recommenderlab::Recommender(
      data = data,
      method = WinningModel)
  }

  # Setup the parallel environment
  packages <- c("curl","reshape2", "recommenderlab", "data.table")
  cores    <- 8
  parts    <- floor(
    nrow(data) * ncol(data) / 250000)
  cl       <- parallel::makePSOCKcluster(cores)
  doParallel::registerDoParallel(cl)

  # Begin scoring
  results <- foreach::foreach(
    i = itertools::isplitRows(
      data,
      chunks = parts),
    .combine = function(...) data.table::rbindlist(list(...)),
    .multicombine = TRUE,
    .packages = packages
  ) %dopar% {
    data <- methods::as(
      recommenderlab::predict(
        recommender,
        i,
        type = "topNList",
        n = 10),
      "list")

    # Data transformations
    temp <- data.table::data.table(
      data.table::melt(data))
    data.table::setcolorder(temp,c(2,1))
    data.table::setnames(temp,
                         c("L1","value"),
                         c(EntityColName, ProductColName))
    temp
  }

  # shut down parallel objects
  parallel::stopCluster(cl)
  rm(cl)

  # Finalize data transformations: append list of data.tables, add ProductRank, gsub x 2, add ts
  results[, ProductRank := seq_len(.N), by = eval(EntityColName)]
  results[, ':=' (TimeStamp = as.character(Sys.time()))]

  return(results)
}

#' AutoCatBoostClassifier is an automated catboost model grid-tuning classifier and evaluation system
#'
#' AutoCatBoostClassifier is an automated modeling function that runs a variety of steps. First, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, ROC plot, evaluation plot, evaluation metrics, variable importance, partial dependence calibration plots, partial dependence calibration box plots, and column names used in model fitting.
#' @author Adrian Antico
#' @family Supervised Learning
#' @param data This is your data set for training and testing your model
#' @param TestData If you want to supply your own data for testing. Column names and column ordering must be the same as data.
#' @param TargetColumnName Either supply the target column name OR the column number where the target is located, but not mixed types. Note that the target column needs to be a 0 | 1 numeric variable.
#' @param FeatureColNames Either supply the feature column names OR the column number where the target is located, but not mixed types. Also, not zero-indexed.
#' @param CatFeatures A vector of column numbers of your categorical features, not zero indexed.
#' @param TrainSplitRatio A decimal between 0.01 and 0.99 that tells the function how much data to keep for training and validation.
#' @param task_type "GPU" Set to "GPU" to utilize your GPU for training. Default is "CPU".
#' @param eval_metric This is the metric used inside catboost to measure performance on validation data during a grid-tune. "AUC" is the default, but other options include "Logloss", "CrossEntropy", "Precision", "Recall", "F1", "BalancedAccuracy", "BalancedErrorRate", "MCC", "Accuracy", "CtrFactor", "AUC", "BrierScore", "HingeLoss", "HammingLoss", "ZeroOneLoss", "Kappa", "WKappa", "LogLikelihoodOfPrediction"
#' @param grid_eval_metric This is the metric used to find the threshold "f","auc","tpr","fnr","fpr","tnr","prbe","f","odds"
#' @param Trees The maximum number of trees you want in your models
#' @param GridTune Set to TRUE to run a grid tuning procedure. Set a number in MaxModelsInGrid to tell the procedure how many models you want to test.
#' @param MaxModelsInGrid Number of models to test from grid options. 1080 total possible options
#' @param model_path A character string of your path file to where you want your output saved
#' @param ModelID A character string to name your model and output
#' @param NumOfParDepPlots Tell the function the number of partial dependence calibration plots you want to create. Calibration boxplots will only be created for numerical features (not dummy variables)
#' @param ReturnModelObjects Set to TRUE to output all modeling objects. E.g. plots and evaluation metrics
#' @param SaveModelObjects Set to TRUE to return all modeling objects to your environment
#' @examples
#' \donttest{
#' Correl <- 0.85
#' N <- 1000
#' data <- data.table::data.table(Target = runif(N))
#' data[, x1 := qnorm(Target)]
#' data[, x2 := runif(N)]
#' data[, Independent_Variable1 := log(pnorm(Correl * x1 +
#'                                             sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable2 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable3 := exp(pnorm(Correl * x1 +
#'                                             sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable4 := exp(exp(pnorm(Correl * x1 +
#'                                                 sqrt(1-Correl^2) * qnorm(x2))))]
#' data[, Independent_Variable5 := sqrt(pnorm(Correl * x1 +
#'                                              sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable6 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^0.10]
#' data[, Independent_Variable7 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^0.25]
#' data[, Independent_Variable8 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^0.75]
#' data[, Independent_Variable9 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^2]
#' data[, Independent_Variable10 := (pnorm(Correl * x1 +
#'                                           sqrt(1-Correl^2) * qnorm(x2)))^4]
#' data[, Independent_Variable11 := as.factor(
#'   ifelse(Independent_Variable2 < 0.20, "A",
#'          ifelse(Independent_Variable2 < 0.40, "B",
#'                 ifelse(Independent_Variable2 < 0.6,  "C",
#'                        ifelse(Independent_Variable2 < 0.8,  "D", "E")))))]
#' # Dummify Categorical Variable
#' data <- RemixAutoML::DummifyDT(data = data,
#'                                cols = "Independent_Variable11",
#'                                KeepFactorCols = FALSE,
#'                                OneHot = TRUE,
#'                                ClustScore = FALSE)
#' data[, Predict := (pnorm(Correl * x1 +
#'                            sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, ':=' (x1 = NULL, x2 = NULL)]
#' data[, Target := ifelse(Target < 0.5, 1, 0)]
#' TestModel <- AutoCatBoostClassifier(data,
#'                                     TestData = NULL,
#'                                     TargetColumnName = "Target",
#'                                     FeatureColNames = c(2:11),
#'                                     CatFeatures = NULL,
#'                                     MaxModelsInGrid = 3,
#'                                     TrainSplitRatio = 0.80,
#'                                     task_type = "GPU",
#'                                     eval_metric = "AUC",
#'                                     grid_eval_metric = "auc",
#'                                     Trees = 50,
#'                                     GridTune = FALSE,
#'                                     model_path = NULL,
#'                                     ModelID = "ModelTest",
#'                                     NumOfParDepPlots = 15,
#'                                     ReturnModelObjects = TRUE,
#'                                     SaveModelObjects = FALSE)
#' }
#' @return Saves to file and returned in list: _ModelID_VariableImportance.csv, _ModelID_ (the model), _ModelID_ValidationData.csv, _ModelID_ROC_Plot.png, _ModelID_EvalutionPlot.png, _ModelID_EvaluationMetrics.csv, _ModelID_ParDepPlots.R a named list of features with partial dependence calibration plots, _ModelID_ParDepBoxPlots.R, _ModelID_GridCollect, and _ModelID_GridList
#' @export
AutoCatBoostClassifier <- function(data,
                                   TestData = NULL,
                                   TargetColumnName = NULL,
                                   FeatureColNames = NULL,
                                   CatFeatures = NULL,
                                   TrainSplitRatio = 0.80,
                                   task_type = "GPU",
                                   eval_metric = "AUC",
                                   Trees = 50,
                                   GridTune = FALSE,
                                   grid_eval_metric = "f",
                                   MaxModelsInGrid = 10,
                                   model_path = NULL,
                                   ModelID = "FirstModel",
                                   NumOfParDepPlots = 3,
                                   ReturnModelObjects = TRUE,
                                   SaveModelObjects = FALSE) {

  # Dont run if model_path is null
  if(is.null(model_path)) {
    warning("Need a model_path defined to run this function")
  } else {

    # Ensure packages are available
    requireNamespace('data.table', quietly = TRUE)
    if(!requireNamespace('catboost', quietly = TRUE)) {
      warning("catboost needs to be installed. See documentation")
    } else {

      # Binary Check Arguments----
      if(!(abs(TrainSplitRatio) <= 0.99)) warning("TrainSplitRatio needs to be less than or equal to 0.99")
      if(!(tolower(task_type) %chin% c("gpu","cpu"))) warning("task_type needs to be either 'GPU' or 'CPU'")
      if(!(tolower(eval_metric) %chin% c("logloss","crossentropy",
                                         "precision","recall",
                                         "f1","balancedaccuracy",
                                         "balancederrorrate","mcc",
                                         "accuracy","ctrfactor",
                                         "auc","brierscore",
                                         "hingeloss","hammingloss",
                                         "zerooneloss","kappa",
                                         "wkappa","loglikelihoodofprediction"))) {
        warning("eval_metric not in c('Logloss','CrossEntropy',
                              'Precision','Recall',
                              'F1','BalancedAccuracy',
                              'BalancedErrorRate','MCC',
                              'Accuracy','CtrFactor',
                              'AUC','BrierScore',
                              'HingeLoss','HammingLoss',
                              'ZeroOneLoss','Kappa',
                              'WKappa','LogLikelihoodOfPrediction')")

      }
      if(Trees < 1) warning("Trees must be greater than 1")
      if(!GridTune %in% c(TRUE,FALSE)) warning("GridTune needs to be TRUE or FALSE")
      if(!(tolower(grid_eval_metric) %chin% c("accuracy","auc","tpr","fnr","fpr","tnr","prbe","f","odds","chisq"))) {
        warning("grid_eval_metric not in c('accuracy','auc','tpr','fnr','fpr','tnr','prbe','f','odds','chisq')")
      }
      if(MaxModelsInGrid < 1 | MaxModelsInGrid > 1080 & GridTune == TRUE) {
        warning("MaxModelsInGrid needs to be at least 1 and less than 1080")
      }
      if(!is.character(model_path)) warning("model_path needs to be a character type")
      if(!is.character(ModelID)) warning("ModelID needs to be a character type")
      if(NumOfParDepPlots < 0) warning("NumOfParDepPlots needs to be a positive number")
      if(!(ReturnModelObjects %in% c(TRUE,FALSE))) warning("ReturnModelObjects needs to be TRUE or FALSE")
      if(!(SaveModelObjects %in% c(TRUE,FALSE))) warning("SaveModelObjects needs to be TRUE or FALSE")

      # Binary Ensure data is a data.table----
      if(!data.table::is.data.table(data)) {
        data <- data.table::as.data.table(data)
      }

      # Binary Convert CatFeatures to 1-indexed----
      if(!is.null(CatFeatures)) {
        CatFeatures <- c((CatFeatures[1]-1):(CatFeatures[length(CatFeatures)]-1))
      }

      # Binary Subset Columns Needed----
      if((is.numeric(TargetColumnName) | is.integer(TargetColumnName)) & (is.numeric(FeatureColNames) | is.integer(FeatureColNames))) {
        keep1 <- names(data)[c(FeatureColNames)]
        keep2 <- names(data)[c(TargetColumnName)]
        keep <- c(keep1, keep2)
        data <- data[, ..keep]
      } else if ((is.numeric(TargetColumnName) | is.integer(TargetColumnName)) & is.character(FeatureColNames)) {
        keep2 <- names(data)[c(TargetColumnName)]
        keep <- c(FeatureColNames, keep2)
        data <- data[, ..keep]
      } else if (is.character(TargetColumnName) & (is.numeric(FeatureColNames) | is.integer(FeatureColNames))) {
        keep1 <- names(data)[c(FeatureColNames)]
        keep <- c(TargetColumnName, keep1)
        data <- data[, ..keep]
      } else if (is.character(TargetColumnName) & is.character(FeatureColNames)) {
        keep <- c(FeatureColNames, TargetColumnName)
        data <- data[, ..keep]
      }

      # Binary Target Name Storage----
      if(is.character(TargetColumnName)) {
        Target <- TargetColumnName
      } else {
        Target <- names(data)[TargetColumnName]
      }

      # Binary Save Names of data----
      Names <- data.table::as.data.table(names(data))
      data.table::setnames(Names, "V1", "ColNames")
      if(SaveModelObjects) {
        data.table::fwrite(Names, paste0(model_path, "/",ModelID,"_ColNames.csv"))
      }

      # Binary Data Partition----
      if(is.null(TestData)) {
        dataTrain <- data[, RANDOMNUMER := runif(nrow(data))][order(RANDOMNUMER)][1:(nrow(data)*TrainSplitRatio)]
        dataTest <- data[(nrow(data) * TrainSplitRatio + 1):nrow(data)]
      } else {
        dataTrain <- data[, RANDOMNUMER := runif(nrow(data))][order(RANDOMNUMER)][1:(nrow(data)*TrainSplitRatio)]
        dataTest <- TestData[, RANDOMNUMER := runif(nrow(data))][order(RANDOMNUMER)][1:(nrow(data)*TrainSplitRatio)]
      }
      dataTrain[, RANDOMNUMER := NULL]
      dataTest[, RANDOMNUMER := NULL]

      # Binary Subset Target Variables----
      TrainTarget <- tryCatch({dataTrain[, get(Target)]}, error = function(x) dataTrain[, eval(Target)])
      TestTarget <- tryCatch({dataTest[, get(Target)]}, error = function(x) dataTest[, eval(Target)])

      # Binary Initialize Catboost Data Conversion----
      if(!is.null(CatFeatures)) {
        TrainPool <- catboost::catboost.load_pool(dataTrain[, eval(Target) := NULL], label = TrainTarget, cat_features = CatFeatures)
        TestPool <- catboost::catboost.load_pool(dataTest[, eval(Target) := NULL], label = TestTarget, cat_features = CatFeatures)
      } else {
        TrainPool <- catboost::catboost.load_pool(dataTrain[, eval(Target) := NULL], label = TrainTarget)
        TestPool <- catboost::catboost.load_pool(dataTest[, eval(Target) := NULL], label = TestTarget)
      }


      # Binary Grid Tune or Not Check----
      if(GridTune) {

        # Binary Grid Create data.table To Store Results----
        GridCollect <- data.table::data.table(ParamRow = 1:(MaxModelsInGrid + 1),
                                              EvalStat = rep(9999999, MaxModelsInGrid + 1))

        # Binary Grid Define Hyper Parameters----
        catboostGridList <- data.table::CJ(l2_leaf_reg = c(0,0.01,0.02,0.03,0.04,0.05),
                                           learning_rate = c(0.01,0.02,0.03,0.04,0.05),
                                           bootstrap_type = c("Poisson","Bayesian","Bernoulli","No"),
                                           depth = c(4:12))
        catboostGridList[, ID := runif(nrow(catboostGridList))]
        catboostGridList <- catboostGridList[order(ID)][1:(MaxModelsInGrid + 1)][, ID := NULL]

        # Binary AUC List----
        AUC_List <- list()

        # Binary Grid Tuning Main Loop----
        for(i in as.integer(seq_len(MaxModelsInGrid + 1))) {

          # Print i
          print(i)

          # Binary Grid Define Base Parameters----
          base_params <- list(iterations           = Trees,
                              loss_function        = 'CrossEntropy',
                              eval_metric          = eval_metric,
                              use_best_model       = TRUE,
                              best_model_min_trees = 10,
                              metric_period        = 10,
                              task_type            = task_type)

          # Binary Grid Merge Model Parameters----
          # Have first model be the baseline model
          if(i != 1) {
            base_params <- c(as.list(catboostGridList[i,]), base_params)
          }

          # Binary Grid Train Model----
          model <- catboost::catboost.train(learn_pool = TrainPool,
                                            test_pool  = TestPool,
                                            params     = base_params)

          # Binary Grid Score Model----
          predict <- catboost::catboost.predict(model = model,
                                                pool = TestPool,
                                                prediction_type = "Probability")

          # Binary Grid Validation Data----
          calibEval <- data.table::as.data.table(
            cbind(Target = TestTarget, p1 = predict))

          # Binary Grid Evaluation Metrics for Each Grid----
          if(tolower(grid_eval_metric) == "accuracy") {
            j <- 0
            x <- data.table::data.table(Metric = "Accuracy",
                                        MetricValue = 5.0,
                                        Threshold = seq(0.01,0.99,0.001))
            for (k in unique(x[["Threshold"]])) {
              j = as.integer(j + 1)
              Accuracy <- mean(calibEval[, ifelse(p1 > k & Target == 1 | p1 < k & Target == 0, 1, 0)])
              data.table::set(x, i = j, j = 2L, value = round(Accuracy,4))
            }
            data.table::setorderv(x, "MetricValue", order = -1, na.last = TRUE)
            Metric <- x[1,MetricValue]
          } else {
            x <- ROCR::prediction(predictions = calibEval[["p1"]], labels = calibEval[["Target"]])
            y <- ROCR::performance(prediction.obj = x, measure = grid_eval_metric)
            if(any(nrow(data.table::as.data.table(y@y.values)) <= 1 |
                   nrow(data.table::as.data.table(y@x.values)) <= 1)) {
              if(nrow(data.table::as.data.table(y@y.values)) <= 1 & nrow(data.table::as.data.table(y@x.values)) <= 1) {
                z <- data.table::as.data.table(cbind(Metric = y@y.values, Threshold = y@x.values))
                Metric <- z[[1]]
              } else if(nrow(data.table::as.data.table(y@y.values)) <= 1 & !(nrow(data.table::as.data.table(y@x.values) <= 1))) {
                z <- data.table::as.data.table(cbind(Metric = y@y.values, Threshold = y@x.values[[1]]))
                Metric <- z[!is.infinite(Threshold)][[1]]
              } else if(!(nrow(data.table::as.data.table(y@y.values)) <= 1) & nrow(data.table::as.data.table(y@x.values) <= 1)) {
                if(grid_eval_metric %chin% c("auc","tpr","tnr","prbe","f","odds")) {
                  z <- data.table::as.data.table(cbind(Metric = y@y.values[[1]], Threshold = y@x.values))
                  Metric <- z[order(-Metric)][!is.infinite(Metric)][[1]]
                } else {
                  z <- data.table::as.data.table(cbind(Metric = y@y.values[[1]], Threshold = y@x.values))
                  Metric <- z[order(Metric)][!is.infinite(Metric)][[1]]
                }
              }
            } else {
              if(metric %chin% c("auc","tpr","tnr","prbe","f","odds")) {
                z <- data.table::as.data.table(cbind(Metric = y@y.values[[1]], Threshold = y@x.values[[1]]))
                Metric <- z[order(-Metric)][!is.infinite(Threshold) & !is.infinite(Metric)][1,]
              } else {
                z <- data.table::as.data.table(cbind(Metric = y@y.values[[1]], Threshold = y@x.values[[1]]))
                Metric <- z[order(Metric)][!is.infinite(Threshold) & !is.infinite(Metric)][1,]
              }
            }
          }

          # Binary AUC Object Create----
          AUC_Metrics <- pROC::roc(response = calibEval[["Target"]],
                                   predictor = calibEval[["p1"]],
                                   na.rm = TRUE,
                                   algorithm = 3,
                                   auc = TRUE,
                                   ci = TRUE)

          # Binary AUC Conversion to data.table----
          AUC_List[[i]] <- data.table::data.table(
            ModelNumber = i,
            Sensitivity = as.numeric(AUC_Metrics$sensitivities+0.0001),
            Specificity = as.numeric(AUC_Metrics$specificities+0.0001))

          # Collect Metrics and Corresponding Grids
          # Store Output Information
          if(tolower(grid_eval_metric) == "accuracy") {
            data.table::set(GridCollect, i = i, j = 1L, value = i)
            data.table::set(GridCollect, i = i, j = 2L, value = Metric)
          } else if(any(nrow(data.table::as.data.table(y@y.values)) <= 1 |
                        nrow(data.table::as.data.table(y@x.values)) <= 1)) {
            data.table::set(GridCollect, i = i, j = 1L, value = i)
            data.table::set(GridCollect, i = i, j = 2L, value = Metric)
          } else {
            data.table::set(GridCollect, i = i, j = 1L, value = i)
            data.table::set(GridCollect, i = i, j = 2L, value = Metric[,1])
          }
        }
      }

      # Binary Define Final Model Parameters----
      if(GridTune) {
        if(grid_eval_metric %chin% c("accuracy","auc","tpr","tnr","prbe","f","odds")) {
          BestGrid <- GridCollect[order(-EvalStat)][1,ParamRow]
          BestThresh <- GridCollect[order(-EvalStat)][1,EvalStat]
        } else {
          BestGrid <- GridCollect[order(EvalStat)][1,ParamRow]
          BestThresh <- GridCollect[order(EvalStat)][1,EvalStat]
        }
        Base_params <- list(iterations           = Trees,
                            learning_rate        = 0.01,
                            depth                = 10,
                            loss_function        = "CrossEntropy",
                            eval_metric          = eval_metric,
                            use_best_model       = TRUE,
                            best_model_min_trees = 10,
                            metric_period        = 10,
                            task_type            = task_type)
        base_params <- c(as.list(catboostGridList[BestGrid,]), Base_params)
      } else {
        base_params <- list(iterations           = Trees,
                            learning_rate        = 0.01,
                            depth                = 10,
                            loss_function        = "CrossEntropy",
                            eval_metric          = eval_metric,
                            use_best_model       = TRUE,
                            best_model_min_trees = 10,
                            metric_period        = 10,
                            task_type            = task_type)
      }

      # Binary Train Final Model----
      model <- catboost::catboost.train(learn_pool = TrainPool,
                                        test_pool  = TestPool,
                                        params     = base_params)

      # Binary Save Model----
      if(SaveModelObjects) {
        setwd(model_path)
        catboost::catboost.save_model(model = model, model_path = paste0(ModelID))
      }

      # Binary Score Final Test Data----
      predict <- catboost::catboost.predict(model = model,
                                            pool = TestPool,
                                            prediction_type = "Probability")

      # Binary Validation Data----
      ValidationData <- data.table::as.data.table(
        cbind(Target = TestTarget, dataTest, p1 = predict))

      # Save Validation Data to File----
      if(SaveModelObjects) {
        data.table::fwrite(ValidationData,
                           file = paste0(model_path,"/", ModelID,"_ValidationData.csv"))
      }

      # Binary AUC Object Create----
      AUC_Metrics <- pROC::roc(response = ValidationData[["Target"]],
                               predictor = ValidationData[["p1"]],
                               na.rm = TRUE,
                               algorithm = 3,
                               auc = TRUE,
                               ci = TRUE)

      # Binary AUC Conversion to data.table----
      AUC_Data <- data.table::data.table(
        ModelNumber = 0,
        Sensitivity = AUC_Metrics$sensitivities,
        Specificity = AUC_Metrics$specificities)

      # Binary Rbind AUC
      if(GridTune == TRUE & MaxModelsInGrid <= 15) {
        temp <- data.table::rbindlist(AUC_List)
        AUC_Data <- data.table::rbindlist(list(temp,AUC_Data))
        AUC_Data[, ModelNumber := as.factor(ModelNumber)]

        # Binary Plot ROC Curve----
        ROC_Plot <- ggplot2::ggplot(AUC_Data, ggplot2::aes(x = 1 - Specificity,
                                                           group = ModelNumber,
                                                           color = ModelNumber)) +
          ggplot2::geom_line(ggplot2::aes(y = AUC_Data[["Sensitivity"]])) +
          ggplot2::geom_abline(slope = 1, color = "black") +
          ggplot2::ggtitle(paste0("Catboost Best Model AUC: ",
                                  100 * round(AUC_Metrics$auc,3),"%")) +
          ChartTheme() + ggplot2::xlab("Specificity") +
          ggplot2::ylab("Sensitivity")

      } else {
        ROC_Plot <- ggplot2::ggplot(AUC_Data, ggplot2::aes(x = 1 - Specificity)) +
          ggplot2::geom_line(ggplot2::aes(y = AUC_Data[["Sensitivity"]]), color = "blue") +
          ggplot2::geom_abline(slope = 1, color = "black") +
          ggplot2::ggtitle(paste0("Catboost AUC: ",
                                  100 * round(AUC_Metrics$auc,3),"%")) +
          ChartTheme() + ggplot2::xlab("Specificity") +
          ggplot2::ylab("Sensitivity")
      }

      # Save plot to file
      if(SaveModelObjects) {
        ggplot2::ggsave(paste0(model_path,"/", ModelID,"_ROC_Plot.png"))
      }

      # Binary Evaluation Calibration Plot----
      EvaluationPlot <- EvalPlot(data = ValidationData,
                                 PredictionColName = "p1",
                                 TargetColName = "Target",
                                 GraphType = "calibration",
                                 PercentileBucket = 0.05,
                                 aggrfun = function(x) mean(x, na.rm = TRUE))

      # Add Number of Trees to Title
      EvaluationPlot <- EvaluationPlot +
        ggplot2::ggtitle(
          paste0("Calibration Evaluation Plot: AUC = ",
                 round(AUC_Metrics$auc,3)))

      # Save plot to file
      if(SaveModelObjects) {
        ggplot2::ggsave(paste0(model_path,"/", ModelID,"_EvaluationPlot.png"))
      }

      # Evaluation Metrics at Optimial Threshold----
      x <- ROCR::prediction(predictions = ValidationData[["p1"]],
                            labels = ValidationData[["Target"]])
      EvaluationMetrics <- data.table::data.table(Metric = c("AUC","TruePositiveRate","FalseNegativeRate",
                                                             "FalsePositiveRate","TrueNegativeRate",
                                                             "PreceisionRecallBreakEven","F1_Score","Odds"),
                                                  MetricValue = rep(999999,8),
                                                  Threshold   = rep(999999,8))
      i <- 0
      for(metric in c("auc","tpr","fnr","fpr","tnr","prbe","f","odds")) {
        i <- as.integer(i + 1)
        tryCatch({
          y <- ROCR::performance(prediction.obj = x, measure = metric)
          if(any(nrow(data.table::as.data.table(y@y.values)) <= 1 |
                 nrow(data.table::as.data.table(y@x.values)) <= 1)) {
            if(nrow(data.table::as.data.table(y@y.values)) <= 1 & nrow(data.table::as.data.table(y@x.values)) <= 1) {
              z <- data.table::as.data.table(cbind(Metric = y@y.values, Threshold = y@x.values))
              Metric <- z[[1]]
            } else if(nrow(data.table::as.data.table(y@y.values)) <= 1 & !(nrow(data.table::as.data.table(y@x.values) <= 1))) {
              z <- data.table::as.data.table(cbind(Metric = y@y.values, Threshold = y@x.values[[1]]))
              Metric <- z[!is.infinite(Threshold)][[1]]
            } else if(!(nrow(data.table::as.data.table(y@y.values)) <= 1) & nrow(data.table::as.data.table(y@x.values) <= 1)) {
              if(metric %chin% c("auc","tpr","tnr","prbe","f","odds")) {
                z <- data.table::as.data.table(cbind(Metric = y@y.values[[1]], Threshold = y@x.values))
                Metric <- z[order(-Metric)][!is.infinite(Metric)][[1]]
              } else {
                z <- data.table::as.data.table(cbind(Metric = y@y.values[[1]], Threshold = y@x.values))
                Metric <- z[order(Metric)][!is.infinite(Metric)][[1]]
              }
            }
          } else {
            if(metric %chin% c("auc","tpr","tnr","prbe","f","odds")) {
              z <- data.table::as.data.table(cbind(Metric = y@y.values[[1]], Threshold = y@x.values[[1]]))
              Metric <- z[order(-Metric)][!is.infinite(Threshold) & !is.infinite(Metric)][1,]
            } else {
              z <- data.table::as.data.table(cbind(Metric = y@y.values[[1]], Threshold = y@x.values[[1]]))
              Metric <- z[order(Metric)][!is.infinite(Threshold) & !is.infinite(Metric)][1,]
            }
          }

          # Store Output Information
          if(any(nrow(data.table::as.data.table(y@y.values)) <= 1 |
                 nrow(data.table::as.data.table(y@x.values)) <= 1)) {
            data.table::set(EvaluationMetrics, i = i, j = 2L, value = round(Metric[[1]],4))
            data.table::set(EvaluationMetrics, i = i, j = 3L, value = NA)
          } else {
            data.table::set(EvaluationMetrics, i = i, j = 2L, value = round(Metric[[1]],4))
            data.table::set(EvaluationMetrics, i = i, j = 3L, value = Metric[[2]])
          }
        }, error = function(x) "skip")
      }

      # Binary Accuracy Threshold and Metric----
      j <- 0
      x <- data.table(Metric = "Accuracy", MetricValue = 5.0, Threshold = seq(0.01,0.99,0.001))
      for (i in unique(x[["Threshold"]])) {
        j = as.integer(j + 1)
        Accuracy <- mean(ValidationData[, ifelse(p1 > i & Target == 1 | p1 < i & Target == 0, 1, 0)])
        set(x, i = j, j = 2L, value = round(Accuracy,4))
      }
      data.table::setorderv(x, "MetricValue", order = -1, na.last = TRUE)
      x <- x[1,]
      EvaluationMetrics <- rbindlist(list(EvaluationMetrics,x))

      # Save EvaluationMetrics to File
      EvaluationMetrics <- EvaluationMetrics[MetricValue != 999999]
      if(SaveModelObjects) {
        data.table::fwrite(EvaluationMetrics,
                           file = paste0(model_path,"/", ModelID,"_EvaluationMetrics.csv"))
      }

      # Binary Variable Importance----
      temp <- catboost::catboost.get_feature_importance(model)
      VariableImportance <- data.table::data.table(cbind(Variable = rownames(temp), temp))
      data.table::setnames(VariableImportance, "V2", "Importance")
      VariableImportance[, Importance := round(as.numeric(Importance),4)]
      VariableImportance <- VariableImportance[order(-Importance)]
      if(SaveModelObjects) {
        data.table::fwrite(VariableImportance, file = paste0(model_path,"/", ModelID,"_VariableImportance.csv"))
      }

      # Binary Partial Dependence----
      ParDepPlots <- list()
      j <- 0
      ParDepBoxPlots <- list()
      k <- 0
      for(i in seq_len(min(length(FeatureColNames),NumOfParDepPlots))) {
        tryCatch({
          Out <- ParDepCalPlots(
            data = ValidationData,
            PredictionColName = "p1",
            TargetColName = "Target",
            IndepVar = VariableImportance[i, Variable],
            GraphType = "calibration",
            PercentileBucket = 0.05,
            FactLevels = 10,
            Function = function(x) mean(x, na.rm = TRUE))

          j <- j + 1
          ParDepPlots[[paste0(VariableImportance[j, Variable])]] <- Out
        }, error = function(x) "skip")
      }

      # Binary Save ParDepPlots to file----
      if(SaveModelObjects) {
        save(ParDepPlots, file = paste0(model_path,"/", ModelID,"_ParDepPlots.R"))
      }

      # Binary Save GridCollect and catboostGridList----
      if(SaveModelObjects & GridTune == TRUE) {
        data.table::fwrite(catboostGridList, file = paste0(model_path,"/",ModelID, "_/catboostGridList.csv"))
        data.table::fwrite(GridCollect, file = paste0(model_path,"/",ModelID, "_/GridCollect.csv"))
      }

      # Binary Return Model Objects----
      if(GridTune) {
        if(ReturnModelObjects) {
          return(
            list(Model = model,
                 ValidationData = ValidationData,
                 ROC_Plot = ROC_Plot,
                 EvaluationPlot = EvaluationPlot,
                 EvaluationMetrics = EvaluationMetrics,
                 VariableImportance = VariableImportance,
                 PartialDependencePlots = ParDepPlots,
                 PartialDependenceBoxPlots = ParDepBoxPlots,
                 GridList = catboostGridList,
                 GridMetrics = GridCollect,
                 ColNames = Names))
        }
      } else {
        if(ReturnModelObjects) {
          return(
            list(Model = model,
                 ValidationData = ValidationData,
                 ROC_Plot = ROC_Plot,
                 EvaluationPlot = EvaluationPlot,
                 EvaluationMetrics = EvaluationMetrics,
                 VariableImportance = VariableImportance,
                 PartialDependencePlots = ParDepPlots,
                 PartialDependenceBoxPlots = ParDepBoxPlots,
                 ColNames = Names))
        }
      }
    }
  }
}

#' AutoCatBoostRegression is an automated catboost model grid-tuning classifier and evaluation system
#'
#' AutoCatBoostRegression is an automated modeling function that runs a variety of steps. First, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation plot, evaluation boxplot, evaluation metrics, variable importance, partial dependence calibration plots, partial dependence calibration box plots, and column names used in model fitting.
#' @author Adrian Antico
#' @family Supervised Learning
#' @param data This is your data set for training and testing your model
#' @param TestData If you want to supply your own data for testing (column names and column ordering must be the same as data)
#' @param TargetColumnName Either supply the target column name OR the column number where the target is located (but not mixed types). Note that the target column needs to be a 0 | 1 numeric variable.
#' @param FeatureColNames Either supply the feature column names OR the column number where the target is located (but not mixed types)
#' @param CatFeatures A vector of column numbers of your categorical features.
#' @param TrainSplitRatio A decimal between 0.01 and 0.99 that tells the function how much data to keep for training and validation.
#' @param task_type = "GPU" Set to "GPU" to utilize your GPU for training. Default is "CPU".
#' @param eval_metric This is the metric used inside catboost to measure performance on validation data during a grid-tune. "RMSE" is the default, but other options include: "MAE", "MAPE", "Poisson", "Quantile", "LogLinQuantile", "Lq", "NumErrors", "SMAPE", "R2", "MSLE", "MedianAbsoluteError".
#' @param Alpha This is the quantile value you want to use for quantile regression. Must be a decimal between 0 and 1.
#' @param grid_eval_metric This is the metric used to find the threshold c('poisson','mae','mape','mse','msle','kl','cs','r2')
#' @param Trees The maximum number of trees you want in your models
#' @param GridTune Set to TRUE to run a grid tuning procedure. Set a number in MaxModelsInGrid to tell the procedure how many models you want to test.
#' @param MaxModelsInGrid Number of models to test from grid options (1080 total possible options)
#' @param model_path A character string of your path file to where you want your output saved
#' @param ModelID A character string to name your model and output
#' @param NumOfParDepPlots Tell the function the number of partial dependence calibration plots you want to create. Calibration boxplots will only be created for numerical features (not dummy variables)
#' @param ReturnModelObjects Set to TRUE to output all modeling objects (E.g. plots and evaluation metrics)
#' @param SaveModelObjects Set to TRUE to return all modeling objects to your environment
#' @examples
#' \donttest{
#' Correl <- 0.85
#' N <- 1000
#' data <- data.table::data.table(Target = runif(N))
#' data[, x1 := qnorm(Target)]
#' data[, x2 := runif(N)]
#' data[, Independent_Variable1 := log(pnorm(Correl * x1 +
#'                                             sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable2 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable3 := exp(pnorm(Correl * x1 +
#'                                             sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable4 := exp(exp(pnorm(Correl * x1 +
#'                                                 sqrt(1-Correl^2) * qnorm(x2))))]
#' data[, Independent_Variable5 := sqrt(pnorm(Correl * x1 +
#'                                              sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable6 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^0.10]
#' data[, Independent_Variable7 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^0.25]
#' data[, Independent_Variable8 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^0.75]
#' data[, Independent_Variable9 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^2]
#' data[, Independent_Variable10 := (pnorm(Correl * x1 +
#'                                           sqrt(1-Correl^2) * qnorm(x2)))^4]
#' data[, Independent_Variable11 := as.factor(
#'   ifelse(Independent_Variable2 < 0.20, "A",
#'          ifelse(Independent_Variable2 < 0.40, "B",
#'                 ifelse(Independent_Variable2 < 0.6,  "C",
#'                        ifelse(Independent_Variable2 < 0.8,  "D", "E")))))]
#' data[, ':=' (x1 = NULL, x2 = NULL)]
#' TestModel <- AutoCatBoostRegression(data,
#'                                     TestData = NULL,
#'                                     TargetColumnName = "Target",
#'                                     FeatureColNames = c(2:11),
#'                                     CatFeatures = c(12),
#'                                     MaxModelsInGrid = 1,
#'                                     TrainSplitRatio = 0.80,
#'                                     task_type = "GPU",
#'                                     eval_metric = "RMSE",
#'                                     grid_eval_metric = "r2",
#'                                     Trees = 50,
#'                                     GridTune = FALSE,
#'                                     model_path = NULL,
#'                                     ModelID = "ModelTest",
#'                                     NumOfParDepPlots = 3,
#'                                     ReturnModelObjects = TRUE,
#'                                     SaveModelObjects = FALSE)
#' }
#' @return Saves to file: _ModelID_VariableImportance.csv, _ModelID_, _ModelID_ValidationData.csv, _ModelID_EvalutionPlot.png, _ModelID_EvalutionBoxPlot.png, _ModelID_EvaluationMetrics.csv, _ModelID_ParDepPlots.R a named list of features with partial dependence calibration plots, _ModelID_ParDepBoxPlots.R, _ModelID_GridCollect, and _ModelID_catboostgrid
#' @export
AutoCatBoostRegression <- function(data,
                                   TestData = NULL,
                                   TargetColumnName = NULL,
                                   FeatureColNames = NULL,
                                   CatFeatures = NULL,
                                   TrainSplitRatio = 0.80,
                                   task_type = "GPU",
                                   eval_metric = "RMSE",
                                   Alpha = NULL,
                                   Trees = 50,
                                   GridTune = FALSE,
                                   grid_eval_metric = "mae",
                                   MaxModelsInGrid = 10,
                                   model_path = NULL,
                                   ModelID = "FirstModel",
                                   NumOfParDepPlots = 3,
                                   ReturnModelObjects = TRUE,
                                   SaveModelObjects = FALSE) {

  # If Model Path is null, dont run
  if(is.null(model_path)) {
    warning("Cannot run without a pathfile to drop notes")
  } else {

    # Ensure packages are available
    requireNamespace('data.table', quietly = TRUE)
    if(!requireNamespace('catboost', quietly = TRUE)) {
      return(warning("catboost needs to be installed. See documentation"))
    } else {

      # Regression Check Arguments----
      if(!(abs(TrainSplitRatio) <= 0.99)) warning("TrainSplitRatio needs to be less than or equal to 0.99")
      if(!(tolower(task_type) %chin% c("gpu","cpu"))) warning("task_type needs to be either 'GPU' or 'CPU'")
      if(!(tolower(eval_metric) %chin% c("rmse","mae","mape","poisson","quantile","loglinquantile",
                                         "lq","numerrors","smape","r2","msle","medianabsoluteerror"))) {
        warning("eval_metric not in c(RMSE,MAE,MAPE,Poisson,Quantile,
             LogLinQuantile,Lq,NumErrors,SMAPE,R2,MSLE,MedianAbsoluteError)")

      }
      if(Trees < 1) warning("Trees must be greater than 1")
      if(!GridTune %in% c(TRUE,FALSE)) warning("GridTune needs to be TRUE or FALSE")
      if(!(tolower(grid_eval_metric) %chin% c("poisson","mae","mape","mse","msle","kl","cs","r2"))) {
        warning("grid_eval_metric not in c('poisson','mae','mape','mse','msle','kl','cs','r2')")
      }
      if(MaxModelsInGrid < 1 | MaxModelsInGrid > 1080 & GridTune == TRUE) {
        warning("MaxModelsInGrid needs to be at least 1 and less than 1080")
      }
      if(!is.character(model_path)) warning("model_path needs to be a character type")
      if(!is.character(ModelID)) warning("ModelID needs to be a character type")
      if(NumOfParDepPlots < 0) warning("NumOfParDepPlots needs to be a positive number")
      if(!(ReturnModelObjects %in% c(TRUE,FALSE))) warning("ReturnModelObjects needs to be TRUE or FALSE")
      if(!(SaveModelObjects %in% c(TRUE,FALSE))) warning("SaveModelObjects needs to be TRUE or FALSE")

      # Regression Ensure data is a data.table----
      if(!data.table::is.data.table(data)) {
        data <- data.table::as.data.table(data)
      }

      # Regression Convert CatFeatures to 1-indexed----
      if(!is.null(CatFeatures)) {
        CatFeatures <- c((CatFeatures[1]-1):(CatFeatures[length(CatFeatures)]-1))
      }

      # Regression Subset Columns Needed----
      if((is.numeric(TargetColumnName) | is.integer(TargetColumnName)) &
         (is.numeric(FeatureColNames) | is.integer(FeatureColNames))) {
        keep1 <- names(data)[c(FeatureColNames)]
        keep2 <- names(data)[c(TargetColumnName)]
        keep <- c(keep1, keep2)
        data <- data[, ..keep]
      } else if ((is.numeric(TargetColumnName) | is.integer(TargetColumnName)) &
                 is.character(FeatureColNames)) {
        keep2 <- names(data)[c(TargetColumnName)]
        keep <- c(FeatureColNames, keep2)
        data <- data[, ..keep]
      } else if (is.character(TargetColumnName) & (is.numeric(FeatureColNames) |
                                                   is.integer(FeatureColNames))) {
        keep1 <- names(data)[c(FeatureColNames)]
        keep <- c(TargetColumnName, keep1)
        data <- data[, ..keep]
      } else if (is.character(TargetColumnName) & is.character(FeatureColNames)) {
        keep <- c(FeatureColNames, TargetColumnName)
        data <- data[, ..keep]
      }

      # Regression Target Name Storage----
      if(is.character(TargetColumnName)) {
        Target <- TargetColumnName
      } else {
        Target <- names(data)[TargetColumnName]
      }

      # Regression Save Names of data----
      Names <- data.table::as.data.table(names(data))
      data.table::setnames(Names, "V1", "ColNames")
      if(SaveModelObjects) {
        data.table::fwrite(Names, paste0(model_path,
                                         "/"
                                         ,ModelID,"_ColNames.csv"))
      }

      # Regression Data Partition----
      if(is.null(TestData)) {
        dataTrain <- data[, RANDOMNUMER := runif(nrow(data))][
          order(RANDOMNUMER)][1:(nrow(data)*TrainSplitRatio)]
        dataTest <- data[(nrow(data) * TrainSplitRatio + 1):nrow(data)]
      } else {
        dataTrain <- data[, RANDOMNUMER := runif(nrow(data))][
          order(RANDOMNUMER)][1:(nrow(data)*TrainSplitRatio)]
        dataTest <- TestData[, RANDOMNUMER := runif(nrow(data))][
          order(RANDOMNUMER)][1:(nrow(data)*TrainSplitRatio)]
      }
      dataTrain[, RANDOMNUMER := NULL]
      dataTest[, RANDOMNUMER := NULL]

      # Regression Subset Target Variables----
      TrainTarget <- tryCatch({dataTrain[, get(Target)]},
                              error = function(x) dataTrain[, eval(Target)])
      TestTarget <- tryCatch({dataTest[, get(Target)]},
                             error = function(x) dataTest[, eval(Target)])

      # Regression eval_metric checks
      if(tolower(eval_metric) == "poisson" & (min(TrainTarget) < 0 |
                                              min(TestTarget) < 0)) {
        warning("eval_metric Poisson requires positive values for Target")
      }

      # Regression Initialize Catboost Data Conversion----
      if(!is.null(CatFeatures)) {
        TrainPool <- catboost::catboost.load_pool(dataTrain[, eval(Target) := NULL],
                                                  label = TrainTarget,
                                                  cat_features = CatFeatures)
        TestPool <- catboost::catboost.load_pool(dataTest[, eval(Target) := NULL],
                                                 label = TestTarget,
                                                 cat_features = CatFeatures)
      } else {
        TrainPool <- catboost::catboost.load_pool(dataTrain[, eval(Target) := NULL],
                                                  label = TrainTarget)
        TestPool <- catboost::catboost.load_pool(dataTest[, eval(Target) := NULL],
                                                 label = TestTarget)
      }

      # Regression Grid Tune or Not Check----
      if(GridTune) {

        # Regression Grid Create data.table To Store Results----
        GridCollect <- data.table::data.table(ParamRow = 1:(MaxModelsInGrid + 1),
                                              EvalStat = rep(9999999, MaxModelsInGrid + 1))

        # Regression Grid Define Hyper Parameters----
        catboostGridList <- data.table::CJ(l2_leaf_reg = c(0,0.01,0.02,0.03,0.04,0.05),
                                           learning_rate = c(0.01,0.02,0.03,0.04,0.05),
                                           bootstrap_type = c("Poisson",
                                                              "Bayesian",
                                                              "Bernoulli",
                                                              "No"),
                                           depth = c(4:12))
        catboostGridList[, ID := runif(nrow(catboostGridList))]
        catboostGridList <- catboostGridList[order(ID)][1:(MaxModelsInGrid + 1)][, ID := NULL]

        # Regression Grid Tuning Main Loop----
        for(i in as.integer(seq_len(MaxModelsInGrid + 1))) {

          # Print i
          print(i)

          # Regression Grid Define Base Parameters----
          if(eval_metric != "Quantile" & eval_metric != "LogLinQuantile") {
            base_params <- list(iterations           = Trees,
                                loss_function        = 'RMSE',
                                eval_metric          = eval_metric,
                                use_best_model       = TRUE,
                                best_model_min_trees = 10,
                                metric_period        = 10,
                                task_type            = task_type)
          } else {
            base_params <- list(iterations           = Trees,
                                loss_function        = 'Quantile',
                                eval_metric          = eval_metric,
                                alpha                = Alpha,
                                use_best_model       = TRUE,
                                best_model_min_trees = 10,
                                metric_period        = 10,
                                task_type            = task_type)
          }

          # Regression Grid Merge Model Parameters----
          # Have first model be the baseline model
          if(i != 1) {
            base_params <- c(as.list(catboostGridList[i,]), base_params)
          }

          # Regression Grid Train Model----
          model <- catboost::catboost.train(learn_pool = TrainPool,
                                            test_pool  = TestPool,
                                            params     = base_params)

          # Regression Grid Score Model----
          predict <- catboost::catboost.predict(model = model,
                                                pool = TestPool,
                                                prediction_type = "RawFormulaVal")

          # Regression Grid Validation Data----
          calibEval <- data.table::as.data.table(
            cbind(Target = TestTarget, Predicted = predict))

          # Regression Metric Collection Table----
          GridCollect <- data.table::data.table(ParamRow = 1:(1 + MaxModelsInGrid),
                                                EvalStat = rep("a", MaxModelsInGrid + 1))

          # Regression Grid Evaluation Metrics----
          if(tolower(grid_eval_metric) == "poisson") {
            calibEval[, Metric := Predicted - Target * log(Predicted + 1)]
            Metric <- calibEval[, mean(Metric, na.rm = TRUE)]
          } else if(tolower(grid_eval_metric) == "mae") {
            calibEval[, Metric := abs(Target - Predicted)]
            Metric <- calibEval[, mean(Metric, na.rm = TRUE)]
          } else if(tolower(grid_eval_metric) == "mape") {
            calibEval[, Metric := abs((Target - Predicted) / (Target + 1))]
            Metric <- calibEval[, mean(Metric, na.rm = TRUE)]
          } else if(tolower(grid_eval_metric) == "mse") {
            calibEval[, Metric := (Target - Predicted)^2]
            Metric <- calibEval[, mean(Metric, na.rm = TRUE)]
          } else if(tolower(grid_eval_metric) == "msle") {
            calibEval[, Metric := (log(Target + 1) - log(Predicted + 1))^2]
            Metric <- calibEval[, mean(Metric, na.rm = TRUE)]
          } else if(tolower(grid_eval_metric) == "kl") {
            calibEval[, Metric := Target * log((Target + 1) / (Predicted + 1))]
            Metric <- calibEval[, mean(Metric, na.rm = TRUE)]
          } else if(tolower(grid_eval_metric) == "cs") {
            calibEval[, ':=' (Metric1 = Target * Predicted,
                              Metric2 = Target^2,
                              Metric3 = Predicted^2)]
            Metric <- calibEval[, sum(Metric1, na.rm = TRUE)] / (
              sqrt(calibEval[, sum(Metric2, na.rm = TRUE)]) *
                sqrt(calibEval[, sum(Metric3, na.rm = TRUE)])
            )
          } else if(tolower(grid_eval_metric) == "r2") {
            calibEval[, ':=' (Metric1 = (Target - mean(Target))^2,
                              Metric2 = (Target - Predicted)^2)]
            Metric <- 1 - calibEval[, sum(Metric2, na.rm = TRUE)] /
              calibEval[, sum(Metric1, na.rm = TRUE)]
          }

          # Regression Metrics Collection----
          data.table::set(GridCollect,
                          i = i,
                          j = 1L,
                          value = i)
          data.table::set(GridCollect,
                          i = i,
                          j = 2L,
                          value = round(Metric,4))
        }
      }

      # Regression Define Final Model Parameters----
      if(GridTune) {
        if(grid_eval_metric %chin% c("auc","tpr","tnr","prbe","f","odds")) {
          BestGrid <- GridCollect[order(-EvalStat)][1,ParamRow]
          BestThresh <- GridCollect[order(-EvalStat)][1,EvalStat]
        } else {
          BestGrid <- GridCollect[order(EvalStat)][1,ParamRow]
          BestThresh <- GridCollect[order(EvalStat)][1,EvalStat]
        }
        Base_params <- list(iterations           = Trees,
                            learning_rate        = 0.01,
                            depth                = 10,
                            loss_function        = "CrossEntropy",
                            eval_metric          = eval_metric,
                            use_best_model       = TRUE,
                            best_model_min_trees = 10,
                            metric_period        = 10,
                            task_type            = task_type)
        base_params <- c(as.list(catboostGridList[BestGrid,]),
                         Base_params)
      } else {
        base_params <- list(iterations           = Trees,
                            learning_rate        = 0.01,
                            depth                = 10,
                            loss_function        = "CrossEntropy",
                            eval_metric          = eval_metric,
                            use_best_model       = TRUE,
                            best_model_min_trees = 10,
                            metric_period        = 10,
                            task_type            = task_type)
      }

      # Regression Train Final Model----
      model <- catboost::catboost.train(learn_pool = TrainPool,
                                        test_pool  = TestPool,
                                        params     = base_params)

      # Regression Save Model----
      if(SaveModelObjects) {
        setwd(model_path)
        catboost::catboost.save_model(model = model,
                                      model_path = paste0(ModelID))
      }

      # Regression Score Final Test Data----
      predict <- catboost::catboost.predict(model = model,
                                            pool = TestPool,
                                            prediction_type = "RawFormulaVal")

      # Regression Validation Data----
      ValidationData <- data.table::as.data.table(
        cbind(Target = TestTarget, dataTest, Predict = predict))

      # Regression r2 via sqrt of correlation
      r_squared <- sqrt(ValidationData[, stats::cor(Target, Predict)])

      # Save Validation Data to File----
      if(SaveModelObjects) {
        data.table::fwrite(ValidationData, file = paste0(model_path,
                                                         "/",
                                                         ModelID,
                                                         "_ValidationData.csv"))
      }

      # Regression Evaluation Calibration Plot----
      EvaluationPlot <- EvalPlot(data = ValidationData,
                                 PredictionColName = "Predict",
                                 TargetColName = "Target",
                                 GraphType = "calibration",
                                 PercentileBucket = 0.05,
                                 aggrfun = function(x) mean(x, na.rm = TRUE))

      # Add Number of Trees to Title
      EvaluationPlot <- EvaluationPlot +
        ggplot2::ggtitle(
          paste0("Calibration Evaluation Plot: R2 = ",
                 round(r_squared,3)))

      # Save plot to file
      if(SaveModelObjects) {
        ggplot2::ggsave(paste0(model_path,
                               "/",
                               ModelID,"_EvaluationPlot.png"))
      }

      # Regression Evaluation Calibration Plot----
      EvaluationBoxPlot <- EvalPlot(data = ValidationData,
                                    PredictionColName = "Predict",
                                    TargetColName = "Target",
                                    GraphType = "boxplot",
                                    PercentileBucket = 0.05,
                                    aggrfun = function(x) mean(x, na.rm = TRUE))

      # Add Number of Trees to Title
      EvaluationBoxPlot <- EvaluationBoxPlot +
        ggplot2::ggtitle(
          paste0("Calibration Evaluation Plot: R2 = ",
                 round(r_squared,3)))

      # Save plot to file
      if(SaveModelObjects) {
        ggplot2::ggsave(paste0(model_path,
                               "/",
                               ModelID,
                               "_EvaluationBoxPlot.png"))
      }

      # Regression Evaluation Metrics----
      EvaluationMetrics <- data.table::data.table(Metric = c("Poisson","MAE",
                                                             "MAPE","MSE","MSLE",
                                                             "KL","CS","R2"),
                                                  MetricValue = rep(999999,8))
      i <- 0
      for(metric in c("poisson","mae","mape","mse","msle","kl","cs","r2")) {
        i <- as.integer(i + 1)
        tryCatch({
          # Regression Grid Evaluation Metrics----
          if(tolower(metric) == "poisson") {
            ValidationData[, Metric := Predict - Target * log(Predict + 1)]
            Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
          } else if(tolower(metric) == "mae") {
            ValidationData[, Metric := abs(Target - Predict)]
            Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
          } else if(tolower(metric) == "mape") {
            ValidationData[, Metric := abs((Target - Predict) / (Target + 1))]
            Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
          } else if(tolower(metric) == "mse") {
            ValidationData[, Metric := (Target - Predict)^2]
            Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
          } else if(tolower(metric) == "msle") {
            ValidationData[, Metric := (log(Target + 1) - log(Predict + 1))^2]
            Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
          } else if(tolower(metric) == "kl") {
            ValidationData[, Metric := Target * log((Target + 1) /
                                                      (Predict + 1))]
            Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
          } else if(tolower(metric) == "cs") {
            ValidationData[, ':=' (Metric1 = Target * Predict,
                                   Metric2 = Target^2,
                                   Metric3 = Predict^2)]
            Metric <- ValidationData[, sum(Metric1, na.rm = TRUE)] / (
              sqrt(ValidationData[, sum(Metric2, na.rm = TRUE)]) *
                sqrt(ValidationData[, sum(Metric3, na.rm = TRUE)])
            )
          } else if(tolower(metric) == "r2") {
            ValidationData[, ':=' (Metric1 = (Target - mean(Target))^2,
                                   Metric2 = (Target - Predict)^2)]
            Metric <- 1 - ValidationData[, sum(Metric2, na.rm = TRUE)] /
              ValidationData[, sum(Metric1, na.rm = TRUE)]
          }
          data.table::set(EvaluationMetrics, i = i, j = 2L, value = round(Metric,4))
          data.table::set(EvaluationMetrics, i = i, j = 3L, value = NA)
        }, error = function(x) "skip")
      }

      # Save EvaluationMetrics to File
      EvaluationMetrics <- EvaluationMetrics[MetricValue != 999999]
      if(SaveModelObjects) {
        data.table::fwrite(EvaluationMetrics,
                           file = paste0(model_path,
                                         "/",
                                         ModelID,"_EvaluationMetrics.csv"))
      }

      # Regression Variable Importance----
      temp <- catboost::catboost.get_feature_importance(model)
      VariableImportance <- data.table::data.table(cbind(Variable = rownames(temp), temp))
      data.table::setnames(VariableImportance, "V2", "Importance")
      VariableImportance[, Importance := round(as.numeric(Importance),4)]
      VariableImportance <- VariableImportance[order(-Importance)]
      if(SaveModelObjects) {
        data.table::fwrite(VariableImportance,
                           file = paste0(model_path,
                                         "/",
                                         ModelID,"_VariableImportance.csv"))
      }

      # Regression Partial Dependence----
      ParDepPlots <- list()
      j <- 0
      ParDepBoxPlots <- list()
      k <- 0
      for(i in seq_len(min(length(FeatureColNames),NumOfParDepPlots))) {
        tryCatch({
          Out <- ParDepCalPlots(
            data = ValidationData,
            PredictionColName = "Predict",
            TargetColName = "Target",
            IndepVar = VariableImportance[i, Variable],
            GraphType = "calibration",
            PercentileBucket = 0.05,
            FactLevels = 10,
            Function = function(x) mean(x, na.rm = TRUE))

          j <- j + 1
          ParDepPlots[[paste0(VariableImportance[j, Variable])]] <- Out
        }, error = function(x) "skip")

        if(length(unique(data[[VariableImportance[i, Variable]]])) > 2) {
          tryCatch({
            Out1 <- ParDepCalPlots(
              data = ValidationData,
              PredictionColName = "p1",
              TargetColName = "Target",
              IndepVar = VariableImportance[i, Variable],
              GraphType = "boxplot",
              PercentileBucket = 0.05,
              FactLevels = 10,
              Function = function(x) mean(x, na.rm = TRUE))

            k <- k + 1
            ParDepBoxPlots[[paste0(VariableImportance[k, Variable])]] <- Out1
          }, error = function(x) "skip")
        }
      }

      # Regression Save ParDepPlots to file----
      if(SaveModelObjects) {
        save(ParDepPlots, file = paste0(model_path,"/", ModelID,"_ParDepPlots.R"))
      }

      # Regression Save ParDepBoxPlots to file----
      if(SaveModelObjects) {
        save(ParDepBoxPlots, file = paste0(model_path,"/", ModelID,"_ParDepBoxPlots.R"))
      }

      # Regression Save GridCollect and catboostGridList----
      if(SaveModelObjects & GridTune == TRUE) {
        data.table::fwrite(catboostGridList, file = paste0(model_path,
                                                           "/catboostGridList.csv"))
        data.table::fwrite(GridCollect, file = paste0(model_path,
                                                      "/GridCollect.csv"))
      }

      # Regression Return Model Objects----
      if(GridTune) {
        if(ReturnModelObjects) {
          return(
            list(Model = model,
                 ValidationData = ValidationData,
                 EvaluationPlot = EvaluationPlot,
                 EvaluationBoxPlot = EvaluationBoxPlot,
                 EvaluationMetrics = EvaluationMetrics,
                 VariableImportance = VariableImportance,
                 PartialDependencePlots = ParDepPlots,
                 PartialDependenceBoxPlots = ParDepBoxPlots,
                 GridList = catboostGridList,
                 GridMetrics = GridCollect,
                 ColNames = Names))
        }
      } else {
        if(ReturnModelObjects) {
          return(
            list(Model = model,
                 ValidationData = ValidationData,
                 EvaluationPlot = EvaluationPlot,
                 EvaluationBoxPlot = EvaluationBoxPlot,
                 EvaluationMetrics = EvaluationMetrics,
                 VariableImportance = VariableImportance,
                 PartialDependencePlots = ParDepPlots,
                 PartialDependenceBoxPlots = ParDepBoxPlots,
                 ColNames = Names))
        }
      }
    }
  }
}
