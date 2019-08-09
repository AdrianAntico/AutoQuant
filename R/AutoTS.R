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
#' @family Automated Time Series
#' @param data is the source time series data as a data.table - or a data structure that can be converted to a data.table
#' @param TargetName is the name of the target variable in your data.table
#' @param DateName is the name of the date column in your data.table
#' @param FCPeriods is the number of periods into the future you wish to forecast
#' @param HoldOutPeriods is the number of periods to use for validation testing
#' @param EvaluationMetric Set this to either "MAPE", "MSE", or "MAE". Default is "MAPE"
#' @param TimeUnit is the level of aggregation your dataset comes in. Choices include: hour, day, week, month, quarter, year, 1Min, 5Min, 10Min, 15Min, and 30Min
#' @param Lags is the number of lags you wish to test in various models (same as moving averages)
#' @param SLags is the number of seasonal lags you wish to test in various models (same as moving averages)
#' @param NumCores is the number of cores available on your computer
#' @param SkipModels Don't run specified models - e.g. exclude all models "DSHW" "ARFIMA" "ARIMA" "ETS" "NNET" "TBATS" "TSLM"
#' @param StepWise Set to TRUE to have ARIMA and ARFIMA run a stepwise selection process. Otherwise, all models will be generated in parallel execution, but still run much slower.
#' @param TSClean Set to TRUE to have missing values interpolated and outliers replaced with interpolated values: creates separate models for a larger comparison set
#' @param ModelFreq Set to TRUE to run a separate version of all models where the time series frequency is chosen algorithmically
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
#'                    TargetName       = "Target",
#'                    DateName         = "DateTime",
#'                    FCPeriods        = 1,
#'                    HoldOutPeriods   = 1,
#'                    EvaluationMetric = "MAPE",
#'                    TimeUnit         = "day",
#'                    Lags             = 1,
#'                    SLags            = 1,
#'                    NumCores         = 4,
#'                    SkipModels       = c("NNET","TBATS","ETS","TSLM","ARFIMA","DSHW"),
#'                    StepWise         = TRUE,
#'                    TSClean          = FALSE,
#'                    ModelFreq        = TRUE,
#'                    PrintUpdates     = FALSE)
#' ForecastData <- output$Forecast
#' ModelEval    <- output$EvaluationMetrics
#' WinningModel <- output$TimeSeriesModel
#' @return Returns a list containing 1: A data.table object with a date column and the forecasted values; 2: The model evaluation results; 3: The champion model for later use if desired; 4: The name of the champion model; 5. A time series ggplot with historical values and forecasted values.
#' @export
AutoTS <- function(data,
                   TargetName     = "Target",
                   DateName       = "DateTime",
                   FCPeriods      = 30,
                   HoldOutPeriods = 30,
                   EvaluationMetric = "MAPE",
                   TimeUnit       = "day",
                   Lags           = 25,
                   SLags          = 2,
                   NumCores       = 4,
                   SkipModels     = NULL,
                   StepWise       = TRUE,
                   TSClean        = TRUE,
                   ModelFreq      = TRUE,
                   PrintUpdates   = FALSE) {
  # Check arguments----
  if (!is.character(TargetName)) {
    warning("TargetName needs to be a character value")
  }
  if (!is.character(DateName)) {
    warning("DateName needs to be a character value")
  }
  if (FCPeriods < 0) {
    warning("FCPeriods needs to be greater than 0")
  }
  if (HoldOutPeriods < 0) {
    warning("HoldOutPeriods needs to be greater than 0")
  }
  if (!is.character(TimeUnit)) {
    warning("TimeUnit needs to be a character value")
  }
  if (Lags < 0) {
    warning("Lags needs to be greater than 0")
  }
  if (!is.null(SkipModels)) {
    if (!any(
      toupper(SkipModels) %chin% c("DSHW", "ARFIMA", "ARIMA", "ETS", "NNET", "TBATS", "TSLM")
    )) {
      warning("SkipModels needs to be one of DSHW, ARFIMA, ARIMA, ETS, NNET, TBATS, TSLM")
    }
  }
  
  # Turn off warnings
  options(warn = -1)
  
  # Initialize collection variables
  i <- 0
  EvalList <- list()
  
  # Convert to data.table if not already
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Ensure correct ordering and subsetting of data
  keep <- c(DateName, TargetName)
  data <- data[, ..keep]
  
  # Check for min value of data
  MinVal <- data[, min(get(TargetName))]
  
  # Convert to lubridate as_date() or POSIXct
  if (tolower(TimeUnit) != "hour") {
    data[, eval(DateName) := lubridate::as_date(get(DateName))]
  } else {
    data[, eval(DateName) := as.POSIXct(get(DateName))]
  }
  
  # Correct ordering----
  if (is.numeric(data[[1]]) | is.integer(data[[1]])) {
    data.table::setcolorder(data, c(2, 1))
  }
  
  # Ensure data is sorted----
  data <- data[order(get(DateName))]
  
  # Change Target Name----
  TempTargetName <- TargetName
  data.table::setnames(data, paste0(eval(TargetName)), "Target")
  TargetName <- "Target"
  
  # Create Training data----
  data_train <- data[1:(nrow(data) - HoldOutPeriods)]
  
  # Create Test data----
  data_test <- data[(nrow(data) - HoldOutPeriods + 1):nrow(data)]
  
  # Check for different time aggregations
  MaxDate <- data[, max(get(DateName))]
  FC_Data <- data.table::data.table(Date = seq(1:FCPeriods))
  
  # Define TS Frequency----
  if (tolower(TimeUnit) == "hour") {
    freq <- 24
    FC_Data[, Date := MaxDate + lubridate::hours(Date)]
  } else if(tolower(TimeUnit) == "1min") {
    freq <- 60
    FC_Data[, Date := MaxDate + lubridate::minute(Date)]
  } else if(tolower(TimeUnit) == "5min") {
    freq <- 12
    FC_Data[, Date := MaxDate + lubridate::minute(5 * Date)]
  } else if(tolower(TimeUnit) == "10min") {
    freq <- 6
    FC_Data[, Date := MaxDate + lubridate::minute(10 * Date)]
  } else if(tolower(TimeUnit) == "15min") {
    freq <- 4
    FC_Data[, Date := MaxDate + lubridate::minute(15 * Date)]
  } else if(tolower(TimeUnit) == "30min") {
    freq <- 2
    FC_Data[, Date := MaxDate + lubridate::minute(30 * Date)]
  } else if (tolower(TimeUnit) == "day") {
    freq <- 365
    FC_Data[, Date := MaxDate + lubridate::days(Date)]
  } else if (tolower(TimeUnit) == "week") {
    freq <- 52
    FC_Data[, Date := MaxDate + lubridate::weeks(Date)]
  } else if (tolower(TimeUnit) == "month") {
    freq <- 12
    FC_Data[, Date := as.Date(MaxDate) %m+% months(Date)]
  } else if (tolower(TimeUnit) == "quarter") {
    freq <- 4
    FC_Data[, Date := as.Date(MaxDate)  %m+% months(4 * Date)]
  } else if (tolower(TimeUnit) == "year") {
    freq <- 1
    FC_Data[, Date := MaxDate + lubridate::years(Date)]
  } else {
    return("TimeUnit is not in hour, day, week, month,
    quarter, or year")
  }
  
  # Coerce SLags if too large----
  if (freq * SLags > nrow(data_train)) {
    SLags <- floor(nrow(data_train) / freq)
  }
  
  # Convert data.tables to stats::ts objects----
  # User Supplied Frequency
  dataTSTrain <-
    stats::ts(data = data_train,
              start = data_train[, min(get(DateName))][[1]],
              frequency = freq)
  
  # TSClean Version----
  if (TSClean) {
    if (MinVal > 0) {
      Target <- forecast::tsclean(x = dataTSTrain[, TargetName],
                                  replace.missing = TRUE,
                                  lambda = "auto")
    } else {
      Target <- forecast::tsclean(x = dataTSTrain[, TargetName],
                                  replace.missing = TRUE,
                                  lambda = NULL)
    }
  }
  
  # Model-Based Frequency----
  SFreq <- forecast::findfrequency(as.matrix(data_train[, 2]))
  dataTSTrain1 <-
    stats::ts(data = data_train,
              start = data_train[, min(get(DateName))][[1]],
              frequency = SFreq)
  
  # TSClean Version
  if (TSClean) {
    if (MinVal > 0) {
      TargetMB <- forecast::tsclean(x = dataTSTrain1[, TargetName],
                                    replace.missing = TRUE,
                                    lambda = "auto")
    } else {
      TargetMB <- forecast::tsclean(x = dataTSTrain1[, TargetName],
                                    replace.missing = TRUE,
                                    lambda = NULL)
    }
  }
  
  # Define differences----
  dTSClean <- forecast::ndiffs(x = TargetMB)
  DTSClean <- forecast::nsdiffs(x = TargetMB)
  ddataTSTrain1 <- forecast::ndiffs(x = dataTSTrain1)
  DdataTSTrain1 <- forecast::nsdiffs(x = dataTSTrain1)
  ddataTSTrain <- forecast::ndiffs(x = dataTSTrain) 
  DdataTSTrain <- forecast::nsdiffs(x = dataTSTrain)
  dTarget <- forecast::ndiffs(x = Target)
  DTarget <- forecast::nsdiffs(x = Target)
  
  # DSHW-------------
  if (!("DSHW" %in% toupper(SkipModels))) {
    # 1)
    if (PrintUpdates)
      message("DSHW FITTING")
    if (MinVal > 0) {
      # User-Supplied-Freq
      DSHW_Model <-
        tryCatch({
          forecast::dshw(
            y = dataTSTrain[, TargetName],
            period1 = freq,
            period2 = freq * 2,
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
      if (ModelFreq) {
        DSHW_Model1 <-
          tryCatch({
            forecast::dshw(
              y = dataTSTrain1[, TargetName],
              period1 = SFreq,
              period2 = SFreq * 2,
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
      
      # Run for outlier removal and imputation
      if (TSClean) {
        # User-Supplied-Freq
        DSHW_Model2 <-
          tryCatch({
            forecast::dshw(
              y = Target,
              period1 = freq,
              period2 = freq * 2,
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
        if (ModelFreq) {
          DSHW_Model3 <-
            tryCatch({
              forecast::dshw(
                y = TargetMB,
                period1 = SFreq,
                period2 = SFreq * 2,
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
      }
    } else {
      # User-Supplied-Freq
      DSHW_Model <-
        tryCatch({
          forecast::dshw(
            y = dataTSTrain[, TargetName],
            period1 = freq,
            period2 = freq * 2,
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
      if (ModelFreq) {
        DSHW_Model1 <-
          tryCatch({
            forecast::dshw(
              y = dataTSTrain1[, TargetName],
              period1 = SFreq,
              period2 = SFreq * 2,
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
      
      # tsclean: impute and replace outliers
      if (TSClean) {
        # User-Supplied-Freq
        DSHW_Model2 <-
          tryCatch({
            forecast::dshw(
              y = Target,
              period1 = freq,
              period2 = freq * 2,
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
        if (ModelFreq) {
          DSHW_Model3 <-
            tryCatch({
              forecast::dshw(
                y = TargetMB,
                period1 = SFreq,
                period2 = SFreq * 2,
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
                                                          1) - 1),
          AbsoluteError = abs(get(TargetName) - FC_Eval),
          SquaredError = (get(TargetName) - FC_Eval) ^ 2
        )]
        
        # Increment
        i <- i + 1
        
        # Collect model filename
        EvalList[[i]] <- data_test_DSHW
      }, error = function(x)
        "skip")
    }
    
    # 2: Model-Supplied-Freq
    if (ModelFreq) {
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
                                                            1) - 1),
            AbsoluteError = abs(get(TargetName) - FC_Eval),
            SquaredError = (get(TargetName) - FC_Eval) ^ 2
          )]
          
          # Increment
          i <- i + 1
          
          # Collect model filename
          EvalList[[i]] <- data_test_DSHW1
        }, error = function(x)
          "skip")
      }
    }
    
    # If TSClean is TRUE
    if (TSClean) {
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
                                                            1) - 1),
            AbsoluteError = abs(get(TargetName) - FC_Eval),
            SquaredError = (get(TargetName) - FC_Eval) ^ 2
          )]
          
          # Increment
          i <- i + 1
          
          # Collect model filename
          EvalList[[i]] <- data_test_DSHW2
        }, error = function(x)
          "skip")
      }
      
      # 2: Model-Supplied-Freq
      if (ModelFreq) {
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
                                                              1) - 1),
              AbsoluteError = abs(get(TargetName) - FC_Eval),
              SquaredError = (get(TargetName) - FC_Eval) ^ 2
            )]
            
            # Increment
            i <- i + 1
            
            # Collect model filename
            EvalList[[i]] <- data_test_DSHW3
          }, error = function(x)
            "skip")
        }
      }
    }
  }
  
  # ARFIMA Modeling----
  if (!("ARFIMA" %in% toupper(SkipModels))) {
    # ARFIMA-------------
    # 1)
    if (PrintUpdates)
      message("ARFIMA FITTING")
    if (StepWise) {
      if (MinVal > 0) {
        # User-Supplied-Freq
        ARFIMA_model <-
          tryCatch({
            forecast::arfima(
              y = dataTSTrain[, TargetName],
              lambda = TRUE,
              biasadj = TRUE,
              max.p = Lags,
              max.q = Lags,
              max.d = ddataTSTrain,
              max.D = DdataTSTrain,
              ic = "bic",
              stepwise = StepWise,
              num.cores = NumCores
            )
          },
          error = function(x)
            "empty")
        
        # Model-Supplied-Freq
        if (ModelFreq) {
          ARFIMA_model1 <-
            tryCatch({
              forecast::arfima(
                y = dataTSTrain1[, TargetName],
                lambda = TRUE,
                biasadj = TRUE,
                max.p = Lags,
                max.q = Lags,
                max.d = ddataTSTrain1,
                max.D = DdataTSTrain1,
                ic = "bic",
                stepwise = StepWise,
                num.cores = NumCores
              )
            },
            error = function(x)
              "empty")
        }
        
        # TSClean Version
        if (TSClean) {
          # User-Supplied-Freq
          ARFIMA_model2 <-
            tryCatch({
              forecast::arfima(
                y = Target,
                lambda = TRUE,
                biasadj = TRUE,
                max.p = Lags,
                max.q = Lags,
                max.d = dTarget,
                max.D = DTarget,
                ic = "bic",
                stepwise = StepWise,
                num.cores = NumCores
              )
            },
            error = function(x)
              "empty")
          
          # Model-Supplied-Freq
          if (ModelFreq) {
            ARFIMA_model3 <-
              tryCatch({
                forecast::arfima(
                  y = TargetMB,
                  lambda = TRUE,
                  biasadj = TRUE,
                  max.p = Lags,
                  max.q = Lags,
                  max.d = dTSClean,
                  max.D = DTSClean,
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
        # User-Supplied-Freq
        ARFIMA_model <-
          tryCatch({
            forecast::arfima(
              y = dataTSTrain[, TargetName],
              lambda = FALSE,
              biasadj = FALSE,
              max.p = Lags,
              max.q = Lags,
              max.d = ddataTSTrain,
              max.D = DdataTSTrain,
              ic = "bic",
              stepwise = StepWise,
              num.cores = NumCores
            )
          },
          error = function(x)
            "empty")
        
        # Model-Supplied-Freq
        if (ModelFreq) {
          ARFIMA_model1 <-
            tryCatch({
              forecast::arfima(
                y = dataTSTrain1[, TargetName],
                lambda = FALSE,
                biasadj = FALSE,
                max.p = Lags,
                max.q = Lags,
                max.d = ddataTSTrain1,
                max.D = DdataTSTrain1,
                ic = "bic",
                stepwise = StepWise,
                num.cores = NumCores
              )
            },
            error = function(x)
              "empty")
        }
        
        # TSClean Version
        if (TSClean) {
          # User-Supplied-Freq
          ARFIMA_model2 <-
            tryCatch({
              forecast::arfima(
                y = Target,
                lambda = FALSE,
                biasadj = FALSE,
                max.p = Lags,
                max.q = Lags,
                max.d = dTarget,
                max.D = DTarget,
                ic = "bic",
                stepwise = StepWise,
                num.cores = NumCores
              )
            },
            error = function(x)
              "empty")
          
          # Model-Supplied-Freq
          if (ModelFreq) {
            ARFIMA_model3 <-
              tryCatch({
                forecast::arfima(
                  y = TargetMB,
                  lambda = FALSE,
                  biasadj = FALSE,
                  max.p = Lags,
                  max.q = Lags,
                  max.d = dTSClean,
                  max.D = DTSClean,
                  ic = "bic",
                  stepwise = StepWise,
                  num.cores = NumCores
                )
              },
              error = function(x)
                "empty")
          }
        }
      }
    } else {
      if (MinVal > 0) {
        # User-Supplied-Freq
        ARFIMA_model <-
          tryCatch({
            forecast::arfima(
              y = dataTSTrain[, TargetName],
              lambda = TRUE,
              biasadj = TRUE,
              max.p = Lags,
              max.q = Lags,
              max.d = ddataTSTrain,
              max.D = DdataTSTrain,
              ic = "bic",
              stepwise = StepWise,
              parallel = TRUE,
              num.cores = NumCores
            )
          },
          error = function(x)
            "empty")
        
        # Model-Supplied-Freq
        if (ModelFreq) {
          ARFIMA_model1 <-
            tryCatch({
              forecast::arfima(
                y = dataTSTrain1[, TargetName],
                lambda = TRUE,
                biasadj = TRUE,
                max.p = Lags,
                max.q = Lags,
                max.d = ddataTSTrain1,
                max.D = DdataTSTrain1,
                ic = "bic",
                stepwise = StepWise,
                parallel = TRUE,
                num.cores = NumCores
              )
            },
            error = function(x)
              "empty")
        }
        
        # TSClean Version
        if (TSClean) {
          # User-Supplied-Freq
          ARFIMA_model2 <-
            tryCatch({
              forecast::arfima(
                y = Target,
                lambda = TRUE,
                biasadj = TRUE,
                max.p = Lags,
                max.q = Lags,
                max.d = dTarget,
                max.D = DTarget,
                ic = "bic",
                stepwise = StepWise,
                parallel = TRUE,
                num.cores = NumCores
              )
            },
            error = function(x)
              "empty")
          
          # Model-Supplied-Freq
          if (ModelFreq) {
            ARFIMA_model3 <-
              tryCatch({
                forecast::arfima(
                  y = TargetMB,
                  lambda = TRUE,
                  biasadj = TRUE,
                  max.p = Lags,
                  max.q = Lags,
                  max.d = dTSClean,
                  max.D = DTSClean,
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
              max.d = ddataTSTrain,
              max.D = DdataTSTrain,
              ic = "bic",
              stepwise = StepWise,
              parallel = TRUE,
              num.cores = NumCores
            )
          },
          error = function(x)
            "empty")
        
        # Model-Supplied-Freq
        if (ModelFreq) {
          ARFIMA_model1 <-
            tryCatch({
              forecast::arfima(
                y = dataTSTrain1[, TargetName],
                lambda = FALSE,
                biasadj = FALSE,
                max.p = Lags,
                max.q = Lags,
                max.d = ddataTSTrain1,
                max.D = DdataTSTrain1,
                ic = "bic",
                stepwise = StepWise,
                parallel = TRUE,
                num.cores = NumCores
              )
            },
            error = function(x)
              "empty")
        }
        
        # TSClean Version
        if (TSClean) {
          # User-Supplied-Freq
          ARFIMA_model2 <-
            tryCatch({
              forecast::arfima(
                y = Target,
                lambda = FALSE,
                biasadj = FALSE,
                max.p = Lags,
                max.q = Lags,
                max.d = dTarget,
                max.D = DTarget,
                ic = "bic",
                stepwise = StepWise,
                parallel = TRUE,
                num.cores = NumCores
              )
            },
            error = function(x)
              "empty")
          
          # Model-Supplied-Freq
          if (ModelFreq) {
            ARFIMA_model3 <-
              tryCatch({
                forecast::arfima(
                  y = TargetMB,
                  lambda = FALSE,
                  biasadj = FALSE,
                  max.p = Lags,
                  max.q = Lags,
                  max.d = dTSClean,
                  max.D = DTSClean,
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
                                                          1) - 1),
          AbsoluteError = abs(get(TargetName) - FC_Eval),
          SquaredError = (get(TargetName) - FC_Eval) ^ 2
        )]
        
        # Increment
        i <- i + 1
        
        # Collect model filename
        EvalList[[i]] <- data_test_ARF
      }, error = function(x)
        "skip")
    }
    
    # 2: Model-Supplied-Freq
    if (ModelFreq) {
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
                                                            1) - 1),
            AbsoluteError = abs(get(TargetName) - FC_Eval),
            SquaredError = (get(TargetName) - FC_Eval) ^ 2
          )]
          
          # Increment
          i <- i + 1
          
          # Collect model filename
          EvalList[[i]] <- data_test_ARF1
        }, error = function(x)
          "skip")
      }
    }
    
    # TSClean Version
    if (TSClean) {
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
                                                            1) - 1),
            AbsoluteError = abs(get(TargetName) - FC_Eval),
            SquaredError = (get(TargetName) - FC_Eval) ^ 2
          )]
          
          # Increment
          i <- i + 1
          
          # Collect model filename
          EvalList[[i]] <- data_test_ARF2
        }, error = function(x)
          "skip")
      }
      
      # 2: Model-Supplied-Freq
      if (ModelFreq) {
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
                                                              1) - 1),
              AbsoluteError = abs(get(TargetName) - FC_Eval),
              SquaredError = (get(TargetName) - FC_Eval) ^ 2
            )]
            
            # Increment
            i <- i + 1
            
            # Collect model filename
            EvalList[[i]] <- data_test_ARF3
          }, error = function(x)
            "skip")
        }
      }
    }
  }
  
  # Arima----
  if (!("ARIMA" %in% toupper(SkipModels))) {
    # ARIMA-------------
    # 1)
    if (PrintUpdates)
      message("ARIMA FITTING")
    if (StepWise) {
      if (MinVal > 0) {
        # User-Supplied-Freq
        ARIMA_model <-
          tryCatch({
            forecast::auto.arima(
              y = dataTSTrain[, TargetName],
              max.p = Lags,
              max.q = Lags,
              max.P = SLags,
              max.Q = SLags,
              max.d = ddataTSTrain,
              max.D = DdataTSTrain,
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
        if (ModelFreq) {
          ARIMA_model1 <-
            tryCatch({
              forecast::auto.arima(
                y = dataTSTrain1[, TargetName],
                max.p = Lags,
                max.q = Lags,
                max.P = SLags,
                max.Q = SLags,
                max.d = ddataTSTrain1,
                max.D = DdataTSTrain1,
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
        
        # TSClean Verison
        if (TSClean) {
          # User-Supplied-Freq
          ARIMA_model2 <-
            tryCatch({
              forecast::auto.arima(
                y = Target,
                max.p = Lags,
                max.q = Lags,
                max.P = SLags,
                max.Q = SLags,
                max.d = dTarget,
                max.D = DTarget,
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
          if (ModelFreq) {
            ARIMA_model3 <-
              tryCatch({
                forecast::auto.arima(
                  y = TargetMB,
                  max.p = Lags,
                  max.q = Lags,
                  max.P = SLags,
                  max.Q = SLags,
                  max.d = dTSClean,
                  max.D = DTSClean,
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
              max.d = ddataTSTrain,
              max.D = DdataTSTrain,
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
        if (ModelFreq) {
          ARIMA_model1 <-
            tryCatch({
              forecast::auto.arima(
                y = dataTSTrain1[, TargetName],
                max.p = Lags,
                max.q = Lags,
                max.P = SLags,
                max.Q = SLags,
                max.d = ddataTSTrain1,
                max.D = DdataTSTrain1,
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
        
        # TSClean Version
        if (TSClean) {
          # User-Supplied-Freq
          ARIMA_model2 <-
            tryCatch({
              forecast::auto.arima(
                y = Target,
                max.p = Lags,
                max.q = Lags,
                max.P = SLags,
                max.Q = SLags,
                max.d = dTarget,
                max.D = DTarget,
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
          if (ModelFreq) {
            ARIMA_model3 <-
              tryCatch({
                forecast::auto.arima(
                  y = TargetMB,
                  max.p = Lags,
                  max.q = Lags,
                  max.P = SLags,
                  max.Q = SLags,
                  max.d = dTSClean,
                  max.D = DTSClean,
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
      }
    } else {
      if (MinVal > 0) {
        # User-Supplied-Freq
        ARIMA_model <-
          tryCatch({
            forecast::auto.arima(
              y = dataTSTrain[, TargetName],
              max.p = Lags,
              max.q = Lags,
              max.P = SLags,
              max.Q = SLags,
              max.d = ddataTSTrain,
              max.D = DdataTSTrain,
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
        if (ModelFreq) {
          ARIMA_model1 <-
            tryCatch({
              forecast::auto.arima(
                y = dataTSTrain1[, TargetName],
                max.p = Lags,
                max.q = Lags,
                max.P = SLags,
                max.Q = SLags,
                max.d = ddataTSTrain1,
                max.D = DdataTSTrain1,
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
        
        # TSClean Version
        if (TSClean) {
          # User-Supplied-Freq
          ARIMA_model2 <-
            tryCatch({
              forecast::auto.arima(
                y = Target,
                max.p = Lags,
                max.q = Lags,
                max.P = SLags,
                max.Q = SLags,
                max.d = dTarget,
                max.D = DTarget,
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
          if (ModelFreq) {
            ARIMA_model3 <-
              tryCatch({
                forecast::auto.arima(
                  y = TargetMB,
                  max.p = Lags,
                  max.q = Lags,
                  max.P = SLags,
                  max.Q = SLags,
                  max.d = dTSClean,
                  max.D = DTSClean,
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
              max.d = dataTSTrain,
              max.D = DdataTSTrain,
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
        if (ModelFreq) {
          ARIMA_model1 <-
            tryCatch({
              forecast::auto.arima(
                y = dataTSTrain1[, TargetName],
                max.p = Lags,
                max.q = Lags,
                max.P = SLags,
                max.Q = SLags,
                max.d = ddataTSTrain1,
                max.D = DdataTSTrain1,
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
        
        # TSClean Version
        if (TSClean) {
          # User-Supplied-Freq
          ARIMA_model2 <-
            tryCatch({
              forecast::auto.arima(
                y = Target,
                max.p = Lags,
                max.q = Lags,
                max.P = SLags,
                max.Q = SLags,
                max.d = dTarget,
                max.D = DTarget,
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
          if (ModelFreq) {
            ARIMA_model3 <-
              tryCatch({
                forecast::auto.arima(
                  y = TargetMB,
                  max.p = Lags,
                  max.q = Lags,
                  max.P = SLags,
                  max.Q = SLags,
                  max.d = dTSClean,
                  max.D = DTSClean,
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
                                                          1) - 1),
          AbsoluteError = abs(get(TargetName) - FC_Eval),
          SquaredError = (get(TargetName) - FC_Eval) ^ 2
        )]
        
        # Increment
        i <- i + 1
        
        # Collect model filename
        EvalList[[i]] <- data_test_ARI
      }, error = function(x)
        "skip")
    }
    
    # Model-Supplied-Freq
    if (ModelFreq) {
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
                                                            1) - 1),
            AbsoluteError = abs(get(TargetName) - FC_Eval),
            SquaredError = (get(TargetName) - FC_Eval) ^ 2
          )]
          
          # Increment
          i <- i + 1
          
          # Collect model filename
          EvalList[[i]] <- data_test_ARI1
        }, error = function(x)
          "skip")
      }
    }
    
    # TSClean Version
    if (TSClean) {
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
                                                            1) - 1),
            AbsoluteError = abs(get(TargetName) - FC_Eval),
            SquaredError = (get(TargetName) - FC_Eval) ^ 2
          )]
          
          # Increment
          i <- i + 1
          
          # Collect model filename
          EvalList[[i]] <- data_test_ARI2
        }, error = function(x)
          "skip")
      }
      
      # Model-Supplied-Freq
      if (ModelFreq) {
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
                                                              1) - 1),
              AbsoluteError = abs(get(TargetName) - FC_Eval),
              SquaredError = (get(TargetName) - FC_Eval) ^ 2
            )]
            
            # Increment
            i <- i + 1
            
            # Collect model filename
            EvalList[[i]] <- data_test_ARI3
          }, error = function(x)
            "skip")
        }
      }
    }
  }
  
  # ETS----
  if (!("ETS" %in% toupper(SkipModels))) {
    # EXPONENTIAL SMOOTHING-------------
    # 1)
    if (PrintUpdates)
      message("ETS FITTING")
    
    # User-Supplied-Freq
    if (freq > 24) {
      if (MinVal > 0) {
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
            )
          }, error = function(x)
            "empty")
        
        # TSClean Version
        if (TSClean) {
          EXPSMOOTH_model2 <-
            tryCatch({
              forecast::ets(
                y = Target,
                model = "ZZN",
                allow.multiplicative.trend = TRUE,
                restrict = TRUE,
                lambda = TRUE,
                biasadj = TRUE
              )
            }, error = function(x)
              "empty")
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
            )
          }, error = function(x)
            "empty")
        
        # TSClean Version
        if (TSClean) {
          EXPSMOOTH_model2 <-
            tryCatch({
              forecast::ets(
                y = Target,
                model = "ZZN",
                allow.multiplicative.trend = TRUE,
                restrict = TRUE,
                lambda = FALSE,
                biasadj = FALSE
              )
            }, error = function(x)
              "empty")
        }
      }
    } else {
      if (MinVal > 0) {
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
        if (TSClean) {
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
        if (TSClean) {
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
    if (ModelFreq) {
      if (SFreq > 24) {
        if (MinVal > 0) {
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
              )
            }, error = function(x)
              "empty")
          
          # TSClean Version
          if (TSClean) {
            EXPSMOOTH_model3 <-
              tryCatch({
                forecast::ets(
                  y = TargetMB,
                  model = "ZZN",
                  allow.multiplicative.trend = TRUE,
                  restrict = TRUE,
                  lambda = TRUE,
                  biasadj = TRUE
                )
              }, error = function(x)
                "empty")
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
              )
            }, error = function(x)
              "empty")
          
          # TSClean Version
          if (TSClean) {
            EXPSMOOTH_model3 <-
              tryCatch({
                forecast::ets(
                  y = TargetMB,
                  model = "ZZN",
                  allow.multiplicative.trend = TRUE,
                  restrict = TRUE,
                  lambda = FALSE,
                  biasadj = FALSE
                )
              }, error = function(x)
                "empty")
          }
        }
      } else {
        if (MinVal > 0) {
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
          if (TSClean) {
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
          if (TSClean) {
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
                                                          1) - 1),
          AbsoluteError = abs(get(TargetName) - FC_Eval),
          SquaredError = (get(TargetName) - FC_Eval) ^ 2
        )]
        
        # Increment
        i <- i + 1
        
        # Collect model filename
        EvalList[[i]] <- data_test_ETS
      }, error = function(x)
        "skip")
    }
    
    # 2: Model-Based-Freq
    if (ModelFreq) {
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
                                                            1) - 1),
            AbsoluteError = abs(get(TargetName) - FC_Eval),
            SquaredError = (get(TargetName) - FC_Eval) ^ 2
          )]
          
          # Increment
          i <- i + 1
          
          # Collect model filename
          EvalList[[i]] <- data_test_ETS1
        }, error = function(x)
          "skip")
      }
    }
    
    # TSClean Version
    if (TSClean) {
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
                                                            1) - 1),
            AbsoluteError = abs(get(TargetName) - FC_Eval),
            SquaredError = (get(TargetName) - FC_Eval) ^ 2
          )]
          
          # Increment
          i <- i + 1
          
          # Collect model filename
          EvalList[[i]] <- data_test_ETS2
        }, error = function(x)
          "skip")
      }
      
      # 2: Model-Based-Freq
      if (ModelFreq) {
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
                                                              1) - 1),
              AbsoluteError = abs(get(TargetName) - FC_Eval),
              SquaredError = (get(TargetName) - FC_Eval) ^ 2
            )]
            
            # Increment
            i <- i + 1
            
            # Collect model filename
            EvalList[[i]] <- data_test_ETS3
          }, error = function(x)
            "skip")
        }
      }
    }
  }
  
  # TBATS----
  if (!("TBATS" %in% toupper(SkipModels))) {
    # TBATS-------------
    # 1)
    if (PrintUpdates)
      message("TBATS FITTING")
    if (MinVal > 0) {
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
            num.cores       = NumCores
          )
        },
        error = function(x)
          "empty")
      
      # Model-Supplied-Freq
      if (ModelFreq) {
        TBATS_model1 <-
          tryCatch({
            forecast::tbats(
              y               = dataTSTrain1[, TargetName],
              use.arma.errors = TRUE,
              lambda          = TRUE,
              biasadj         = TRUE,
              max.p           = Lags,
              max.q           = Lags,
              num.cores       = NumCores
            )
          },
          error = function(x)
            "empty")
      }
      
      # TSClean Version
      if (TSClean) {
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
              num.cores       = NumCores
            )
          },
          error = function(x)
            "empty")
        
        # Model-Supplied-Freq
        if (ModelFreq) {
          TBATS_model3 <-
            tryCatch({
              forecast::tbats(
                y               = TargetMB,
                use.arma.errors = TRUE,
                lambda          = TRUE,
                biasadj         = TRUE,
                max.p           = Lags,
                max.q           = Lags,
                num.cores       = NumCores
              )
            },
            error = function(x)
              "empty")
        }
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
            num.cores       = NumCores
          )
        },
        error = function(x)
          "empty")
      
      # Model-Supplied-Freq
      if (ModelFreq) {
        TBATS_model1 <-
          tryCatch({
            forecast::tbats(
              y               = dataTSTrain1[, TargetName],
              use.arma.errors = TRUE,
              lambda          = FALSE,
              biasadj         = FALSE,
              max.p           = Lags,
              max.q           = Lags,
              num.cores       = NumCores
            )
          },
          error = function(x)
            "empty")
      }
      
      # TSClean Version
      if (TSClean) {
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
              num.cores       = NumCores
            )
          },
          error = function(x)
            "empty")
        
        # Model-Supplied-Freq
        if (ModelFreq) {
          TBATS_model3 <-
            tryCatch({
              forecast::tbats(
                y               = TargetMB,
                use.arma.errors = TRUE,
                lambda          = FALSE,
                biasadj         = FALSE,
                max.p           = Lags,
                max.q           = Lags,
                num.cores       = NumCores
              )
            },
            error = function(x)
              "empty")
        }
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
                                                          1) - 1),
          AbsoluteError = abs(get(TargetName) - FC_Eval),
          SquaredError = (get(TargetName) - FC_Eval) ^ 2
        )]
        
        # Increment
        i <- i + 1
        
        # Collect model filename
        EvalList[[i]] <- data_test_TBATS
      }, error = function(x)
        "skip")
    }
    
    # Model-Supplied-Freq
    if (ModelFreq) {
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
                                                            1) - 1),
            AbsoluteError = abs(get(TargetName) - FC_Eval),
            SquaredError = (get(TargetName) - FC_Eval) ^ 2
          )]
          
          # Increment
          i <- i + 1
          
          # Collect model filename
          EvalList[[i]] <- data_test_TBATS1
        }, error = function(x)
          "skip")
      }
    }
    
    # TSClean Version
    if (TSClean) {
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
                                                            1) - 1),
            AbsoluteError = abs(get(TargetName) - FC_Eval),
            SquaredError = (get(TargetName) - FC_Eval) ^ 2
          )]
          
          # Increment
          i <- i + 1
          
          # Collect model filename
          EvalList[[i]] <- data_test_TBATS2
        }, error = function(x)
          "skip")
      }
      
      # Model-Supplied-Freq
      if (ModelFreq) {
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
                                                              1) - 1),
              AbsoluteError = abs(get(TargetName) - FC_Eval),
              SquaredError = (get(TargetName) - FC_Eval) ^ 2
            )]
            
            # Increment
            i <- i + 1
            
            # Collect model filename
            EvalList[[i]] <- data_test_TBATS3
          }, error = function(x)
            "skip")
        }
      }
    }
  }
  
  # TSLM----
  if (!("TSLM" %in% toupper(SkipModels))) {
    # LINEAR MODEL WITH TIME SERIES COMPONENTS-------------
    # 1)
    if (PrintUpdates)
      message("TSLM FITTING")
    if (MinVal > 0) {
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
      if (ModelFreq) {
        TSLM_model1 <-
          tryCatch({
            forecast::tslm(dataTSTrain1[, TargetName] ~ trend + season,
                           lambda = TRUE,
                           biasadj = TRUE)
          },
          error = function(x)
            "empty")
      }
      
      # TSClean Version
      if (TSClean) {
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
        if (ModelFreq) {
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
      if (ModelFreq) {
        TSLM_model1 <-
          tryCatch({
            forecast::tslm(dataTSTrain1[, TargetName] ~ trend + season,
                           lambda = TRUE,
                           biasadj = TRUE)
          },
          error = function(x)
            "empty")
      }
      
      # TSClean Version
      if (TSClean) {
        TSLM_model2 <-
          tryCatch({
            forecast::tslm(Target ~ trend + season,
                           lambda = FALSE,
                           biasadj = FALSE)
          },
          error = function(x)
            "empty")
        
        # Model-Supplied-Freq
        if (ModelFreq) {
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
          FC_Eval = as.numeric(
            forecast::forecast(TSLM_model,
                               h = HoldOutPeriods)$mean
          )
        )]
        
        # Add Evaluation Columns
        # 3)
        data_test_TSLM[, ':=' (
          Resid = get(TargetName) - FC_Eval,
          PercentError = get(TargetName) / (FC_Eval +
                                              1) - 1,
          AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                          1) - 1),
          AbsoluteError = abs(get(TargetName) - FC_Eval),
          SquaredError = (get(TargetName) - FC_Eval) ^ 2
        )]
        
        # Increment
        i <- i + 1
        
        # Collect model filename
        EvalList[[i]] <- data_test_TSLM
      }, error = function(x)
        "skip")
    }
    
    # Model-Supplied-Freq
    if (ModelFreq) {
      if (tolower(class(TSLM_model1)[1]) == "tslm") {
        tryCatch({
          # Collect Test Data for Model Comparison
          # 2)
          data_test_TSLM1 <- data.table::copy(data_test)
          data_test_TSLM1[, ':=' (
            Target = as.numeric(Target),
            ModelName = rep("TSLM_ModelFreq", HoldOutPeriods),
            FC_Eval = as.numeric(
              forecast::forecast(TSLM_model1,
                                 h = HoldOutPeriods)$mean
            )
          )]
          
          # Add Evaluation Columns
          # 3)
          data_test_TSLM1[, ':=' (
            Resid = get(TargetName) - FC_Eval,
            PercentError = get(TargetName) / (FC_Eval +
                                                1) - 1,
            AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                            1) - 1),
            AbsoluteError = abs(get(TargetName) - FC_Eval),
            SquaredError = (get(TargetName) - FC_Eval) ^ 2
          )]
          
          # Increment
          i <- i + 1
          
          # Collect model filename
          EvalList[[i]] <- data_test_TSLM1
        }, error = function(x)
          "skip")
      }
    }
    
    # TSClean Version
    if (TSClean) {
      # User-Supplied-Freq
      if (tolower(class(TSLM_model2)[1]) == "tslm") {
        tryCatch({
          # Collect Test Data for Model Comparison
          # 2)
          data_test_TSLM2 <- data.table::copy(data_test)
          data_test_TSLM2[, ':=' (
            Target = as.numeric(Target),
            ModelName = rep("TSLM_TSC", HoldOutPeriods),
            FC_Eval = as.numeric(
              forecast::forecast(TSLM_model2,
                                 h = HoldOutPeriods)$mean
            )
          )]
          
          # Add Evaluation Columns
          # 3)
          data_test_TSLM2[, ':=' (
            Resid = get(TargetName) - FC_Eval,
            PercentError = get(TargetName) / (FC_Eval +
                                                1) - 1,
            AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                            1) - 1),
            AbsoluteError = abs(get(TargetName) - FC_Eval),
            SquaredError = (get(TargetName) - FC_Eval) ^ 2
          )]
          
          # Increment
          i <- i + 1
          
          # Collect model filename
          EvalList[[i]] <- data_test_TSLM2
        }, error = function(x)
          "skip")
      }
      
      # Model-Supplied-Freq
      if (ModelFreq) {
        if (tolower(class(TSLM_model3)[1]) == "tslm") {
          tryCatch({
            # Collect Test Data for Model Comparison
            # 2)
            data_test_TSLM3 <- data.table::copy(data_test)
            data_test_TSLM3[, ':=' (
              Target = as.numeric(Target),
              ModelName = rep("TSLM_ModelFreqTSC", HoldOutPeriods),
              FC_Eval = as.numeric(
                forecast::forecast(TSLM_model3,
                                   h = HoldOutPeriods)$mean
              )
            )]
            
            # Add Evaluation Columns
            # 3)
            data_test_TSLM3[, ':=' (
              Resid = get(TargetName) - FC_Eval,
              PercentError = get(TargetName) / (FC_Eval +
                                                  1) - 1,
              AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                              1) - 1),
              AbsoluteError = abs(get(TargetName) - FC_Eval),
              SquaredError = (get(TargetName) - FC_Eval) ^ 2
            )]
            
            # Increment
            i <- i + 1
            
            # Collect model filename
            EvalList[[i]] <- data_test_TSLM3
          }, error = function(x)
            "skip")
        }
      }
    }
  }
  
  # NNET----
  if (!("NNET" %in% toupper(SkipModels))) {
    # Neural Network-------------
    # 1)
    if (PrintUpdates)
      message("NNet FITTING")
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
        if (PrintUpdates)
          print(paste0("NNet Iteration: ", k))
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
    LagNN <- temp[order(meanResid)][1,][, 1][[1]]
    SLagNN <- temp[order(meanResid)][1,][, 2][[1]]
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
                                                          1) - 1),
          AbsoluteError = abs(get(TargetName) - FC_Eval),
          SquaredError = (get(TargetName) - FC_Eval) ^ 2
        )]
        
        # Increment
        i <- i + 1
        
        # Collect model filename
        EvalList[[i]] <- data_test_NN
      }, error = function(x)
        "skip")
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
        if (PrintUpdates)
          print(paste0("NNet 2 Iteration: ", k))
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
    LagNN <- temp[order(meanResid)][1,][, 1][[1]]
    SLagNN <- temp[order(meanResid)][1,][, 2][[1]]
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
                                                          1) - 1),
          AbsoluteError = abs(get(TargetName) - FC_Eval),
          SquaredError = (get(TargetName) - FC_Eval) ^ 2
        )]
        
        # Increment
        i <- i + 1
        
        # Collect model filename
        EvalList[[i]] <- data_test_NN1
      }, error = function(x)
        "skip")
    }
    
    # TSClean Version
    if (TSClean) {
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
          if (PrintUpdates)
            print(paste0("NNet 3 Iteration: ", k))
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
              value = base::mean(abs(
                NNETAR_model_temp$residuals
              ),
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
      LagNN <- temp[order(meanResid)][1,][, 1][[1]]
      SLagNN <- temp[order(meanResid)][1,][, 2][[1]]
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
                                                            1) - 1),
            AbsoluteError = abs(get(TargetName) - FC_Eval),
            SquaredError = (get(TargetName) - FC_Eval) ^ 2
          )]
          
          # Increment
          i <- i + 1
          
          # Collect model filename
          EvalList[[i]] <- data_test_NN2
        }, error = function(x)
          "skip")
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
          if (PrintUpdates)
            print(paste0("NNet 4 Iteration: ", k))
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
              value = base::mean(abs(
                NNETAR_model_temp$residuals
              ),
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
      LagNN <- temp[order(meanResid)][1,][, 1][[1]]
      SLagNN <- temp[order(meanResid)][1,][, 2][[1]]
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
                                                            1) - 1),
            AbsoluteError = abs(get(TargetName) - FC_Eval),
            SquaredError = (get(TargetName) - FC_Eval) ^ 2
          )]
          
          # Increment
          i <- i + 1
          
          # Collect model filename
          EvalList[[i]] <- data_test_NN3
        }, error = function(x)
          "skip")
      }
    }
  }
  
  # Model Collection----
  if (PrintUpdates)
    message("FIND WINNER")
  dataEval <- data.table::rbindlist(EvalList)
  
  # Model Evaluation----
  if (tolower(EvaluationMetric) == "mae") {
    Eval <- dataEval[, .(
      MeanResid = round(base::mean(Resid, na.rm = TRUE), 2),
      MeanPercError = round(base::mean(PercentError, na.rm = TRUE), 5),
      MAPE = round(mean(AbsolutePercentError, na.rm = TRUE), 5),
      MAE = round(mean(AbsoluteError, na.rm = TRUE), 4),
      MSE = round(mean(SquaredError, na.rm = TRUE), 4)
    ),
    by = "ModelName"][order(MAE)][, ID := 1:.N]
  } else if (tolower(EvaluationMetric) == "mape") {
    Eval <- dataEval[, .(
      MeanResid = round(base::mean(Resid, na.rm = TRUE), 2),
      MeanPercError = round(base::mean(PercentError, na.rm = TRUE), 5),
      MAPE = round(mean(AbsolutePercentError, na.rm = TRUE), 5),
      MAE = round(mean(AbsoluteError, na.rm = TRUE), 4),
      MSE = round(mean(SquaredError, na.rm = TRUE), 4)
    ),
    by = "ModelName"][order(MAPE)][, ID := 1:.N]
  } else if (tolower(EvaluationMetric) == "mse") {
    Eval <- dataEval[, .(
      MeanResid = round(base::mean(Resid, na.rm = TRUE), 2),
      MeanPercError = round(base::mean(PercentError, na.rm = TRUE), 5),
      MAPE = round(mean(AbsolutePercentError, na.rm = TRUE), 5),
      MAE = round(mean(AbsoluteError, na.rm = TRUE), 4),
      MSE = round(mean(SquaredError, na.rm = TRUE), 4)
    ),
    by = "ModelName"][order(MSE)][, ID := 1:.N]
  }
  
  # Grab Winning Model----
  BestModel <- Eval[1, "ModelName"][[1]]
  
  # Generate Forecasts----
  if (PrintUpdates)
    message("GENERATE FORECASTS")
  
  # Create Training data----
  data_train <- data[seq_len(nrow(data))]
  
  # Create Full Training Data for Final Rebruild----
  if (grepl("ModelFreq", BestModel)) {
    if (grepl("TSC", BestModel)) {
      if (MinVal > 0) {
        # Model-Supplied-Freq
        SFreq <- forecast::findfrequency(as.matrix(data_train[, 2]))
        dataTSTrain <-
          stats::ts(data = data_train,
                    start = data_train[, min(get(DateName))][[1]],
                    frequency = SFreq)
        
        # TSClean Version
        dataTSTrain <-
          forecast::tsclean(x = dataTSTrain[, TargetName],
                            replace.missing = TRUE,
                            lambda = "auto")
      } else {
        # Model-Supplied-Freq
        SFreq <- forecast::findfrequency(as.matrix(data_train[, 2]))
        dataTSTrain <-
          stats::ts(data = data_train,
                    start = data_train[, min(get(DateName))][[1]],
                    frequency = SFreq)

        # TSClean Version
        dataTSTrain <-
          forecast::tsclean(x = dataTSTrain[, TargetName],
                            replace.missing = TRUE,
                            lambda = NULL)
      }
      
    } else {
      # Model-Supplied-Freq
      SFreq <- forecast::findfrequency(as.matrix(data_train[, 2]))
      dataTSTrain <-
        stats::ts(data = data_train,
                  start = data_train[, min(get(DateName))][[1]],
                  frequency = SFreq)
      
      # Only Target as Numeric Vector
      dataTSTrain <- dataTSTrain[, TargetName]
    }
  } else {
    if (grepl("TSC", BestModel)) {
      if (MinVal > 0) {
        # User-Supplied-Freq
        dataTSTrain <-
          stats::ts(data = data_train,
                    start = data_train[, min(get(DateName))][[1]],
                    frequency = freq)
        
        # TSClean Version
        dataTSTrain <-
          forecast::tsclean(x = dataTSTrain[, TargetName],
                            replace.missing = TRUE,
                            lambda = "auto")
      } else {
        # User-Supplied-Freq
        dataTSTrain <-
          stats::ts(data = data_train,
                    start = data_train[, min(get(DateName))][[1]],
                    frequency = freq)
        
        # TSClean Version
        dataTSTrain <-
          forecast::tsclean(x = dataTSTrain[, TargetName],
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
  
  # Diffs----
  d <- forecast::ndiffs(x = dataTSTrain)
  D <- forecast::nsdiffs(x = dataTSTrain)
  
  # Retrain best model
  if (grepl(pattern = "DSHW", BestModel)) {
    if (PrintUpdates)
      message("FULL DATA DSHW FITTING")
    if (BestModel == "DSHW_ModelFreq") {
      freq <- SFreq
    }
    if (MinVal > 0) {
      DSHW_Model <-
        tryCatch({
          forecast::dshw(
            y = dataTSTrain,
            period1 = freq,
            period2 = freq * 2,
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
        tryCatch({
          forecast::dshw(
            y = dataTSTrain,
            period1 = freq,
            period2 = freq * 2,
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
    FC_Data[, paste0("Forecast_",BestModel) := as.numeric(forecast::forecast(DSHW_Model, h = FCPeriods)$mean)]
    FC_Data[, paste0(BestModel, "_Low80") := as.numeric(forecast::forecast(DSHW_Model, h = FCPeriods)$lower)[1:FCPeriods]]
    FC_Data[, paste0(BestModel,"_Low95") := as.numeric(forecast::forecast(DSHW_Model, h = FCPeriods)$lower)[(FCPeriods+1):(2*FCPeriods)]]
    FC_Data[, paste0(BestModel,"_High80") := as.numeric(forecast::forecast(DSHW_Model, h = FCPeriods)$upper)[1:FCPeriods]]
    FC_Data[, paste0(BestModel,"_High95") := as.numeric(forecast::forecast(DSHW_Model, h = FCPeriods)$upper)[(FCPeriods+1):(2*FCPeriods)]]
    
    # Store model
    model <- DSHW_Model
    
  } else if (grepl(pattern = "ARFIMA", BestModel)) {
    if (PrintUpdates)
      message("FULL DATA ARFIMA FITTING")
    # Rebuild model on full data
    if (StepWise) {
      if (MinVal > 0) {
        ARFIMA_model <- forecast::arfima(
          y = dataTSTrain,
          lambda = TRUE,
          biasadj = TRUE,
          max.p = Lags,
          max.q = Lags,
          max.d = d,
          max.D = D,
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
          max.d = d,
          max.D = D,
          ic = "bic",
          stepwise = StepWise,
          num.cores = NumCores
        )
      }
    } else {
      if (MinVal > 0) {
        ARFIMA_model <- forecast::arfima(
          y = dataTSTrain,
          lambda = TRUE,
          biasadj = TRUE,
          max.p = Lags,
          max.q = Lags,
          max.d = d,
          max.D = D,
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
          max.d = d,
          max.D = D,
          ic = "bic",
          stepwise = StepWise,
          parallel = TRUE,
          num.cores = NumCores
        )
      }
    }
    
    # Forecast with new model
    FC_Data[, paste0("Forecast_",BestModel) := as.numeric(forecast::forecast(ARFIMA_model, h = FCPeriods)$mean)]
    FC_Data[, paste0(BestModel, "_Low80") := as.numeric(forecast::forecast(ARFIMA_model, h = FCPeriods)$lower)[1:FCPeriods]]
    FC_Data[, paste0(BestModel,"_Low95") := as.numeric(forecast::forecast(ARFIMA_model, h = FCPeriods)$lower)[(FCPeriods+1):(2*FCPeriods)]]
    FC_Data[, paste0(BestModel,"_High80") := as.numeric(forecast::forecast(ARFIMA_model, h = FCPeriods)$upper)[1:FCPeriods]]
    FC_Data[, paste0(BestModel,"_High95") := as.numeric(forecast::forecast(ARFIMA_model, h = FCPeriods)$upper)[(FCPeriods+1):(2*FCPeriods)]]
    
    # Store model
    model <- ARFIMA_model
    
  } else if (grepl(pattern = "ARIMA", BestModel)) {
    if (PrintUpdates)
      message("FULL DATA ARIMA FITTING")
    # Rebuild model on full data
    if (StepWise) {
      if (MinVal > 0) {
        ARIMA_model <-
          forecast::auto.arima(
            y     = dataTSTrain,
            max.p = Lags,
            max.q = Lags,
            max.P = SLags,
            max.Q = SLags,
            max.d = d,
            max.D = D,
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
            max.d = d,
            max.D = D,
            ic = "bic",
            lambda = FALSE,
            biasadj = FALSE,
            stepwise = StepWise,
            num.cores = NumCores
          )
      }
    } else {
      if (MinVal > 0) {
        ARIMA_model <-
          forecast::auto.arima(
            y     = dataTSTrain,
            max.p = Lags,
            max.q = Lags,
            max.P = SLags,
            max.Q = SLags,
            max.d = d,
            max.D = D,
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
            max.d = d,
            max.D = D,
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
    FC_Data[, paste0("Forecast_",BestModel) := as.numeric(forecast::forecast(ARIMA_model, h = FCPeriods)$mean)]
    FC_Data[, paste0(BestModel, "_Low80") := as.numeric(forecast::forecast(ARIMA_model, h = FCPeriods)$lower)[1:FCPeriods]]
    FC_Data[, paste0(BestModel,"_Low95") := as.numeric(forecast::forecast(ARIMA_model, h = FCPeriods)$lower)[(FCPeriods+1):(2*FCPeriods)]]
    FC_Data[, paste0(BestModel,"_High80") := as.numeric(forecast::forecast(ARIMA_model, h = FCPeriods)$upper)[1:FCPeriods]]
    FC_Data[, paste0(BestModel,"_High95") := as.numeric(forecast::forecast(ARIMA_model, h = FCPeriods)$upper)[(FCPeriods+1):(2*FCPeriods)]]
    
    # Store model
    model <- ARIMA_model
    
  } else if (grepl(pattern = "ETS", BestModel)) {
    if (PrintUpdates)
      message("FULL DATA ETS FITTING")
    # Rebuild model on full data
    if (freq > 24) {
      if (MinVal > 0) {
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
      if (MinVal > 0) {
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
    FC_Data[, paste0("Forecast_",BestModel) := as.numeric(forecast::forecast(EXPSMOOTH_model, h = FCPeriods)$mean)]
    FC_Data[, paste0(BestModel, "_Low80") := as.numeric(forecast::forecast(EXPSMOOTH_model, h = FCPeriods)$lower)[1:FCPeriods]]
    FC_Data[, paste0(BestModel,"_Low95") := as.numeric(forecast::forecast(EXPSMOOTH_model, h = FCPeriods)$lower)[(FCPeriods+1):(2*FCPeriods)]]
    FC_Data[, paste0(BestModel,"_High80") := as.numeric(forecast::forecast(EXPSMOOTH_model, h = FCPeriods)$upper)[1:FCPeriods]]
    FC_Data[, paste0(BestModel,"_High95") := as.numeric(forecast::forecast(EXPSMOOTH_model, h = FCPeriods)$upper)[(FCPeriods+1):(2*FCPeriods)]]
    
    # Store model
    model <- EXPSMOOTH_model
    
  } else if (grepl(pattern = "TBATS", BestModel)) {
    if (PrintUpdates)
      message("FULL DATA TBATS FITTING")
    if (MinVal > 0) {
      # Rebuild model on full data
      TBATS_model <- forecast::tbats(
        y = dataTSTrain,
        use.arma.errors = TRUE,
        lambda = TRUE,
        biasadj = TRUE,
        max.p = Lags,
        max.q = Lags,
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
        num.cores = NumCores
      )
    }
    
    # Forecast with new model
    FC_Data[, paste0("Forecast_",BestModel) := as.numeric(forecast::forecast(TBATS_model, h = FCPeriods)$mean)]
    FC_Data[, paste0(BestModel, "_Low80") := as.numeric(forecast::forecast(TBATS_model, h = FCPeriods)$lower)[1:FCPeriods]]
    FC_Data[, paste0(BestModel,"_Low95") := as.numeric(forecast::forecast(TBATS_model, h = FCPeriods)$lower)[(FCPeriods+1):(2*FCPeriods)]]
    FC_Data[, paste0(BestModel,"_High80") := as.numeric(forecast::forecast(TBATS_model, h = FCPeriods)$upper)[1:FCPeriods]]
    FC_Data[, paste0(BestModel,"_High95") := as.numeric(forecast::forecast(TBATS_model, h = FCPeriods)$upper)[(FCPeriods+1):(2*FCPeriods)]]
    
    # Store model
    model <- TBATS_model
    
  } else if (grepl(pattern = "TSLM", BestModel)) {
    if (PrintUpdates)
      message("FULL DATA TSLM FITTING")
    if (MinVal > 0) {
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
    FC_Data[, paste0("Forecast_",BestModel) := as.numeric(forecast::forecast(TSLM_model, h = FCPeriods)$mean)]
    FC_Data[, paste0(BestModel, "_Low80") := as.numeric(forecast::forecast(TSLM_model, h = FCPeriods)$lower)[1:FCPeriods]]
    FC_Data[, paste0(BestModel,"_Low95") := as.numeric(forecast::forecast(TSLM_model, h = FCPeriods)$lower)[(FCPeriods+1):(2*FCPeriods)]]
    FC_Data[, paste0(BestModel,"_High80") := as.numeric(forecast::forecast(TSLM_model, h = FCPeriods)$upper)[1:FCPeriods]]
    FC_Data[, paste0(BestModel,"_High95") := as.numeric(forecast::forecast(TSLM_model, h = FCPeriods)$upper)[(FCPeriods+1):(2*FCPeriods)]]
    
    # Store model
    model <- TSLM_model
    
  } else if (grepl(pattern = "NN", BestModel)) {
    if (PrintUpdates)
      message("FULL DATA NN FITTING")
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
        if (PrintUpdates)
          print(k)
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
    LagNN <- temp[order(meanResid)][1,][, 1][[1]]
    SLagNN <- temp[order(meanResid)][1,][, 2][[1]]
    NNETAR_model <-
      tryCatch({
        forecast::nnetar(y = dataTSTrain,
                         p = LagNN,
                         P = SLagNN)
      },
      error = function(x)
        "empty")
    
    # Forecast with new model
    xx <- forecast::forecast(NNETAR_model, PI=TRUE, h = FCPeriods)
    FC_Data[, paste0("Forecast_",BestModel) := as.numeric(forecast::forecast(TSLM_model, h = FCPeriods)$mean)]
    FC_Data[, paste0(BestModel, "_Low80") := as.numeric(xx$lower)[1:FCPeriods]]
    FC_Data[, paste0(BestModel,"_Low95") := as.numeric(xx$lower)[(FCPeriods+1):(2*FCPeriods)]]
    FC_Data[, paste0(BestModel,"_High80") := as.numeric(xx$upper)[1:FCPeriods]]
    FC_Data[, paste0(BestModel,"_High95") := as.numeric(x$upper)[(FCPeriods+1):(2*FCPeriods)]]
    
    # Store model
    model <- NNETAR_model
  }
  
  # Create plot
  temp <- data.table::copy(FC_Data)
  data.table::setnames(data, c(eval(DateName)), "Date")
  Time <-
    data.table::rbindlist(list(data[, "Date"], temp[, "Date"]))
  z <-
    data.table::rbindlist(list(data[, Date := NULL], temp[, Date := NULL]), fill = TRUE)
  z <- cbind(Time, z)
  z[, eval(TargetName) := as.numeric(get(TargetName))]
  logo = magick::image_read(
    "https://www.remixinstitute.com/wp-content/uploads/7b-Cheetah_Charcoal_Inline_No_Sub_No_BG.png"
  )
  TimeSeriesPlot <-
    ggplot2::ggplot(z, ggplot2::aes(x = z[["Date"]])) +
    ggplot2::geom_line(ggplot2::aes(y = z[[eval(TargetName)]]), color = "#005B80") +
    ggplot2::geom_vline(
      xintercept = max(data_test[[eval(DateName)]],
                       na.rm = TRUE),
      color = "#FF4F00",
      lty = "dotted",
      lwd = 1
    ) +
    RemixAutoML::RemixTheme() +
    ggplot2::labs(
      title = paste0(FCPeriods, "-", TimeUnit, " Forecast for ", TempTargetName),
      subtitle = paste0(
        "Champion Model: ",
        BestModel,
        " | Mean Absolute Percentage Error: ",
        paste(round(min(Eval$MAPE), 3) * 100, "%", sep = "")
      ),
      caption = "Forecast generated by Remix Institute's RemixAutoML R package"
    ) +
    ggplot2::xlab(eval(DateName)) + ggplot2::ylab(eval(TempTargetName))
  
  TimeSeriesPlot <- TimeSeriesPlot + ggplot2::geom_ribbon(
    ggplot2::aes(ymin = z[[5]], 
                 ymax = z[[7]]),
    fill = "blue", alpha = 1)
  TimeSeriesPlot <- TimeSeriesPlot + ggplot2::geom_ribbon(
    ggplot2::aes(ymin = z[[4]], 
                 ymax = z[[6]]),
    fill = "lightblue1", alpha = 1) + 
    ggplot2::geom_line(ggplot2::aes(y = z[[3]]), color = "black", lwd = 1) +
    ggplot2::geom_line(ggplot2::aes(y = z[[4]]), color = "black", lwd = 0.25) +
    ggplot2::geom_line(ggplot2::aes(y = z[[5]]), color = "black", lwd = 0.25) +
    ggplot2::geom_line(ggplot2::aes(y = z[[6]]), color = "black", lwd = 0.25) +
    ggplot2::geom_line(ggplot2::aes(y = z[[7]]), color = "black", lwd = 0.25)
  
  # Get back to adding image to plot----
  # TimeSeriesPlot
  # grid::grid.raster(logo, x = .73, y = 0.01, just = c('left', 'bottom'), width = 0.25)
  
  options(warn = 0)
  
  # Return values
  return(
    list(
      Forecast = FC_Data,
      EvaluationMetrics = Eval,
      TimeSeriesModel = model,
      ChampionModel = BestModel,
      TimeSeriesPlot = TimeSeriesPlot
    )
  )
}
