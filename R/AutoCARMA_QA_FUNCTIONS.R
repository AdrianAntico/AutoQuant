#' AutoCARMA_QA
#'
#' AutoCARMA_QA
#' @author Adrian Antico
#' @family QA Functions
#'
#' # Data Names Mapping
#' @param ModelName Choose from 'catboost', 'h2odrf', 'h2ogbm', 'h2oglm', 'h2oautoml', 'xgboost'
#' @param FeatureGridTune Set to TRUE to only run in evaluation model opposed to TrainOnFull model which does not return model performance measures
#' @param Nthreads_ = parallel::detectCores() - 2
#' @param MaxMem_ = "28G"
#' @param TreeMethod__ = "hist" or "gpu_hist" for xgboost carma
#' @param TestRows = "ALL" to run all tests (see example for all tests), or a numeric vector with the row numbers from the test list (see example)
#' @param DataPath In quotes, provide the file path to where your data is stored
#' @param dataX = "RawDataXREG.csv" Use quotes. # Be aware that grouped data and using XREGS_ requires that your joining group variables have the same name. MUST SUPPLY VALUE
#' @param dataForecastX = "RawDataXREG.csv" Use quotes. # Be aware that grouped data and using XREGS_ requires that your joining group variables have the same name. MUST SUPPLY VALUE
#' @param XREGSX = "XREG.csv" Use quotes. # data.table with ONLY 3 COLUMN TYPES: 1: - GroupVariables_ and DateColumnName_ join-by variables with matching join column names and data types compared to data_ and;  2 - features - needs to exist for all historical periods matching data_ along with a sufficient amount of data to cover the forecast period as defined by FC_Periods_. OR Supply NULL to arg.
#' @param TargetColumnName_ = "Weekly_Sales" # WalmartData target column name.
#' @param DateColumnName_ = "Date" # Name of data_ date column name.
#' @param HierarchGroups_ = c("Store","Dept") # NULL otherwise
#' @param GroupVariables_ = c("Store","Dept") #
#'
#' # Date Features
#' @param CalendarVariables_ = TRUE   # This TURNS ON procedure to create numeric calendar variables that your TimeUnit_ directs. FALSE otherwise.
#' @param HolidayVariable_ = TRUE   # This TURNS ON procedure to create a numeric holiday count variable. FALSE otherwise.
#' @param HolidayLags_ = c(1:2) # Supply a numeric vector of lag periods
#' @param HolidayMovingAverages_ = c(1:2) # Supply a numeric vector of Moving Average periods
#' @param TimeUnit_ = "week" # Choices include "1min", "5min", "10min", "15min", "30min", "hour", "day", "week", "month", "quarter", "year"
#' @param TimeGroups_ = c("weeks","months","quarter") # These will tell GDL to build gdl features along the time aggregation dimension 
#' # Data Wrangling Features Mapping
#' @param ZeroPadSeries_ = c('NULL', 'all', 'inner') ZeroPadSeries choose "all", "inner", or NULL. 'Outer' grows missing dates by group to the largest of all groups size. 'Inner' fills in series by using the group level's own max and min values (versus filling all group levels to the max value of the groups level with the widest time gap)
#' @param DataTruncate_ = FALSE # TRUE will truncate all rows where GDL columns produced a -1 (remove all rows where ID < max(rolling stats)). FALSE otherwise.
#' @param SplitRatios_ = c(1 - 10 / 143, 10 / 143) # If you have GroupVariables_ then base it on number of records in a group, like default
#' @param PartitionType_ = "timeseries" # always time series for this function. Place holder for other time series options down the road.
#'
#' # Productionize Features Mapping
#' @param TrainOnFull_ = FALSE # Set to TRUE put in Forecase mode. FALSE to put in Evaluation mode. Forecast mode generates forecasts based on a model built using all of data_, and no evaluation metrics are collected when set to TRUE. Evaluation mode will build a forecast for your validation periods and collect the holdout metrics and other evaluation objects, but no future forecast beyond max date of data_.  as specified in SplitRatios_.
#' @param FC_Periods_ = 4 # Self explanatory
#' @param EvalMetric_ = "RMSE" # "RMSE" only with catboost 17.5
#' @param GridTune_ = FALSE # NEEDS TO BE UPDATED ONCE BANDIT GRID TUNING WORKS.
#' @param GridEvalMetric_ = "mae" # 'poisson', 'mae', 'mape', 'mse', 'msle', 'kl', 'cs', 'r2'. If metric computation fails then no output is generated in final metric evaluation data.table
#' @param ModelCount_ = 5 # NEEDS TO BE UPDATED ONCE BANDIT GRID TUNING WORKS.
#' @param TaskType_ = "GPU" # Set to "CPU" to train on CPU versus GPU. Must supply a value.
#' @param Timer_ = TRUE # Print out the forecast step the function is currently working on. If it errors on the first run scoring the model then it is likely a very different error then if has printed "Forecasting 1:"
#'
#' # Target Transformations Feature Mapping
#' @param TargetTransformation_ = TRUE # Set to TRUE to have every available numeric transformation compete for best normalization fit to normal distribution
#' @param Difference_ = TRUE # The I in ARIMA. Works for single series and grouped series a.k.a. panel data.
#'
#' # Time Series Features
#' @param Lags_ = c(1:5) # Numeric vector of lag periods
#' @param MA_Periods_ = c(1:5) # Numeric vector of lag periods
#' @param SD_Periods_ = c(2:5) # Numeric vector of lag periods
#' @param Skew_Periods_ = c(3:5) # Numeric vector of lag periods
#' @param Kurt_Periods_ = c(4:5) # Numeric vector of lag periods
#' @param Quantile_Periods_ = c(3:5) # Numeric vector of lag periods
#' @param Quantiles_Selected_ = c("q5","q95") # Select the quantiles you want calculated. "q5", "q10", ..., "q95".
#'
#' # Bonus Features MAPPING
#' @param FourierTerms_ = 2 # (TECHINICALLY FOURIER PAIRS) Hierarchy grouping (full group variable interaction set) is ran by default (MAKE INTO OPTIOn). Uses parallelization to loop through the unique set of all GroupVariables levels and computes fourier terms as if the group level's are a single series; just for all groups and it's parallelized.
#' @param TimeTrendVariable_ = TRUE # Set to TRUE to have a sequence created from 1 to nrow by group or single series
#' @param NTrees_ = 150 # Number of trees to have trained. Can be 10000 or more depending on group level size.
#' @param DebugMode_ = TRUE # When TRUE it will print every comment section header line. When it crashes, you can get a print out of the last N steps that were ran, depending on the print max limit.
#' @param OptionsWarn Set to 1 to print warnings immediately to screen versus after a function finishes; 2 to kill processes if a warning occurs. See options(warn = )
#' @export
AutoCARMA_QA <- function(ModelName              = "catboost",
                         FeatureGridTune        = FALSE,
                         MaxMem_                = "28G",
                         NThreads_              = max(1,parallel::detectCores()-2),
                         TreeMethod__           = "hist",
                         TestRows               = "ALL",
                         
                         # Data Set
                         DataPath               = "C:/Users/aantico/Documents/Package/Data_For_Functions/CARMA_FUNCTIONS",
                         dataForecastX          = "CARMA-WALMART-2GroupVars_FC.csv",
                         dataX                  = "CARMA-WALMART-2GroupVars.csv",
                         XREGSX                 = "CARMA-WALMART-2GroupVars-XREGS_2Var.csv",
                         
                         # Data elements
                         TargetColumnName_      = "Weekly_Sales",
                         DateColumnName_        = "Date",
                         HierarchGroups_        = c("Store","Dept"),
                         GroupVariables_        = c("Store","Dept"),
                         TimeUnit_              = "week",
                         TimeGroups_            = c("week","month","quarter"),
                         
                         # Data Wrangling Features
                         ZeroPadSeries_         = NULL,
                         DataTruncate_          = FALSE,
                         SplitRatios_           = c(1 - 3 / 143, 3 / 143),
                         PartitionType_         = "timeseries",
                         
                         # Productionize
                         TrainOnFull_           = FALSE,
                         FC_Periods_            = 4,
                         EvalMetric_            = "RMSE",
                         GridTune_              = FALSE,
                         GridEvalMetric_        = "mae",
                         ModelCount_            = 5,
                         TaskType_              = "GPU",
                         Timer_                 = TRUE,
                         
                         # Target Transformations
                         TargetTransformation_  = TRUE,
                         Difference_            = TRUE,
                         
                         # Date Features
                         CalendarVariables_     = TRUE,
                         HolidayVariable_       = TRUE,
                         HolidayLags_           = 1,
                         HolidayMovingAverages_ = 1:2,
                         
                         # Time Series Features
                         Lags_                  = c(1:5),
                         MA_Periods_            = c(1:5),
                         SD_Periods_            = c(2:5),
                         Skew_Periods_          = c(3:5),
                         Kurt_Periods_          = c(4:5),
                         Quantile_Periods_      = c(3:5),
                         Quantiles_Selected_    = c("q5","q95"),
                         
                         # Bonus Features
                         FourierTerms_          = 4,
                         TimeTrendVariable_     = TRUE,
                         NTrees_                = 150,
                         DebugMode_             = TRUE,
                         OptionsWarn            = 1) {
  
  # Error handling mode----
  options(warn = OptionsWarn)
  
  # Turn on full speed ahead----
  data.table::setDTthreads(threads = max(1L, parallel::detectCores()-2))
  data.table::getDTthreads(verbose = TRUE)
  
  # Define arguments to test----
  AutoCARMA_ArgList <- data.table::CJ(
    TrainOnFull_Arg = c(TRUE,FALSE),
    XREGS_Include = c(TRUE,FALSE),
    Transformation = c(TRUE,FALSE),
    Differencing = c(TRUE,FALSE),
    FourierVars = c(TRUE,FALSE),
    CalendarVars = c(TRUE,FALSE),
    HolidaysVars = c(TRUE,FALSE),
    HolidayLags = c(TRUE,FALSE),
    HolidayMovingAverages = c(TRUE,FALSE),
    TimeTrend = c(TRUE,FALSE))
  
  # Remove impossible settings (total settings: 128)----
  if(FeatureGridTune) {
    AutoCARMA_ArgList <- AutoCARMA_ArgList[TrainOnFull_Arg == FALSE]
  }
  AutoCARMA_ArgList[, HolidayLags := ifelse(!HolidaysVars, FALSE, HolidayLags)]
  AutoCARMA_ArgList[, HolidayMovingAverages := ifelse(HolidaysVars, HolidayMovingAverages, FALSE)]
  AutoCARMA_ArgList <- unique(AutoCARMA_ArgList)
  
  # Notify user of number of cases----
  print("QA FUNCTION: Start: Number of test cases")
  print(AutoCARMA_ArgList[,.N])
  print(AutoCARMA_ArgList[,.N])
  print(AutoCARMA_ArgList[,.N])
  print(AutoCARMA_ArgList[,.N])
  print(AutoCARMA_ArgList[,.N])
  print(AutoCARMA_ArgList[,.N])
  print(AutoCARMA_ArgList[,.N])
  print("QA FUNCTION: End: Number of test cases")
  
  # MAE Metric of Whole Data Set----
  AutoCARMA_ArgList[, EvalMetric := -1]
  
  # MAE Metric of Whole Data Set----
  AutoCARMA_ArgList[, RUN_TIME := -1]
  
  # Setup column to collect results----
  AutoCARMA_ArgList[, RESULTS_TEST := "FAIL"]
  
  # Loop type----
  if(any(tolower(TestRows) == "all")) {
    RUNS <- seq_len(AutoCARMA_ArgList[,.N])  
  } else {
    RUNS <- as.integer(TestRows)
  }
  
  # Loop through tests----
  print("QA FUNCTION: Begin Model Building")
  print("QA FUNCTION: Begin Model Building")
  print("QA FUNCTION: Begin Model Building")
  print("QA FUNCTION: Begin Model Building")
  for(run in RUNS) {
    
    # Print Row Numbers of Test data.table----
    print("Test case: ")
    for(zzz in 1:20) {
      print(run)
    }
    
    # Pull in data so it is a fresh set everytime----
    print("pull in dataX now")
    Data <- data.table::fread(file.path(DataPath, dataX))
    print("pull in dataForecastX now")
    if(!is.null(dataForecastX)) {
      DataForecast <- data.table::fread(file.path(DataPath, dataForecastX))
      XREGSData <- data.table::fread(file.path(DataPath, XREGSX))
    } else {
      if(!is.null(GroupVariables_)) {
        Output <- RemixAutoAI::QA_WALMARTDATAGENERATOR(Data, Groups = length(GroupVariables_), TimeUnit__ = toupper(TimeUnit_))
      } else {
        Output <- RemixAutoAI::QA_WALMARTDATAGENERATOR(data = Data, Groups = 0L, TimeUnit__ = TimeUnit_)
      }
      Data <- Output$dataFull
      DataForecast <- Output$dataForecastX
      XREGSData <- Output$XREGS1
    }

    print("Testing Args 1")
    
    # Testing args----
    if(AutoCARMA_ArgList[run]$TrainOnFull_Arg) {
      TrainOnFull <- TRUE
      data__ <- Data
    } else {
      TrainOnFull <- FALSE
      data__ <- Data
    }
    
    print("Testing Args 2")
    
    if(AutoCARMA_ArgList[run]$XREGS_Include) {
      XREGS__ <- XREGSData
    } else {
      XREGS__ <- NULL
    }
    
    print("Testing Args 3")
    
    TargetTransformation__ <- AutoCARMA_ArgList[run]$Transformation
    
    print("Testing Args 4")
    
    Difference__ <- AutoCARMA_ArgList[run]$Differencing
    
    print("Testing Args 5")
    
    if(AutoCARMA_ArgList[run]$FourierVars) {
      FourierTerms__ <- 2
    } else {
      FourierTerms__ <- 0
    }
    
    print("Testing Args 6")
    
    CalendarVariables__ <- AutoCARMA_ArgList[run]$CalendarVars
    
    print("Testing Args 7")
    
    if(AutoCARMA_ArgList[run]$HolidaysVars) {
      HolidayVariable__ <- TRUE
      if(AutoCARMA_ArgList[run]$HolidayLags) {
        HolidayLags__ <- 1
        HolidayMovingAverages__ <- 1:2
      } else {
        HolidayLags__ <- 0
        HolidayMovingAverages__ <- 0
      }
    } else {
      HolidayVariable__ <- FALSE
      HolidayLags__ <- 0
      HolidayMovingAverages__ <- 0
    }
    
    print("Testing Args 8")
    
    TimeTrendVariable__ <- AutoCARMA_ArgList[run]$TimeTrend
    
    # Time runs start----
    StartTime <- Sys.time()
    
    # CatBoost CARMA FUNCTION----
    if(tolower(ModelName) == "catboost") {
      print("Build catboost models")
      PrintOut <- tryCatch({
        AutoCatBoostCARMA(
          
          # Testing Args----
          TrainOnFull = TrainOnFull,
          XREGS = XREGS__,
          TargetTransformation = TargetTransformation__,
          Difference = Difference__,
          FourierTerms = FourierTerms__,
          CalendarVariables = CalendarVariables__,
          TimeTrendVariable = TimeTrendVariable__,
          HolidayVariable = HolidayVariable__,
          HolidayLags = HolidayLags__,
          HolidayMovingAverages = HolidayMovingAverages__,
          
          # Data
          data = data__,
          
          # Data names----
          TargetColumnName = TargetColumnName_,
          DateColumnName = DateColumnName_,
          HierarchGroups = HierarchGroups_,
          GroupVariables = GroupVariables_,
          TimeUnit = TimeUnit_,
          TimeGroups = TimeGroups_,
          
          # Data Wrangling Features----
          ZeroPadSeries = ZeroPadSeries_,
          DataTruncate = DataTruncate_,
          SplitRatios = SplitRatios_,
          PartitionType = PartitionType_,
          
          # Productionize----
          
          FC_Periods = FC_Periods_,
          EvalMetric = EvalMetric_,
          GridTune = GridTune_,
          GridEvalMetric = GridEvalMetric_,
          ModelCount = ModelCount_,
          TaskType = TaskType_,
          Timer = Timer_,
          
          # Time Series Features----
          Lags = Lags_,
          MA_Periods = MA_Periods_,
          SD_Periods = SD_Periods_,
          Skew_Periods = Skew_Periods_,
          Kurt_Periods = Kurt_Periods_,
          Quantile_Periods = Quantile_Periods_,
          Quantiles_Selected = Quantiles_Selected_,
          
          # Bonus Features----
          NTrees = NTrees_,
          DebugMode = DebugMode_)
        
        # CREATE WARNING OUTPUT----
      }, error = function(x) {
        "error"
      })
      
      # Time runs end----
      print("QA Function: CARMA Function Run End----")
      print("QA Function: CARMA Function Run End----")
      print("QA Function: CARMA Function Run End----")
      print("QA Function: CARMA Function Run End----")
      EndTime <- Sys.time()
      
      # Fill in table----
      if(is.list(PrintOut)) {
        data.table::set(AutoCARMA_ArgList, i = run, j = "RESULTS_TEST", value = "success")
        data.table::set(AutoCARMA_ArgList, i = run, j = "RUN_TIME", value = EndTime - StartTime)
        if(!TrainOnFull) {
          # print(PrintOut$ModelInformation$EvaluationMetrics)
          # print(PrintOut$ModelInformation$EvaluationMetrics)
          # print(PrintOut$ModelInformation$EvaluationMetrics)
          # print(PrintOut$ModelInformation$EvaluationMetrics)
          # print(PrintOut$ModelInformation$EvaluationMetrics)
          # print(PrintOut$ModelInformation$EvaluationMetrics)
          # print(PrintOut$ModelInformation$EvaluationMetrics)
          print("SUCCESS SUCCESS SUCCESS")
          print("SUCCESS SUCCESS SUCCESS")
          print("SUCCESS SUCCESS SUCCESS")
          print("SUCCESS SUCCESS SUCCESS")
          print("SUCCESS SUCCESS SUCCESS")
          tryCatch({data.table::set(AutoCARMA_ArgList, i = run, j = "EvalMetric", value = PrintOut$ModelInformation$EvaluationMetrics[Metric == "MAE"][[2]])}, error = function(x) NULL)
        }
      } else {
        print("FAILURE FAILURE FAILURE")
        print("FAILURE FAILURE FAILURE")
        print("FAILURE FAILURE FAILURE")
        print("FAILURE FAILURE FAILURE")
        print("FAILURE FAILURE FAILURE")
        data.table::set(AutoCARMA_ArgList, i = run, j = "RESULTS_TEST", value = "failure")
        data.table::set(AutoCARMA_ArgList, i = run, j = "RUN_TIME", value = EndTime - StartTime)
      }
    }
    
    # H2O-DFR CARMA FUNCTION----
    if(tolower(ModelName) == "h2odrf") {
      PrintOut <- tryCatch({
        AutoH2oDRFCARMA(
          
          # Testing Args----
          TrainOnFull = TrainOnFull,
          XREGS = XREGS__,
          TargetTransformation = TargetTransformation__,
          Difference = Difference__,
          FourierTerms = FourierTerms__,
          CalendarVariables = CalendarVariables__,
          TimeTrendVariable = TimeTrendVariable__,
          HolidayVariable = HolidayVariable__,
          HolidayLags = HolidayLags__,
          HolidayMovingAverages = HolidayMovingAverages__,
          
          # Data
          data = data__,
          
          # Data names----
          TargetColumnName = TargetColumnName_,
          DateColumnName = DateColumnName_,
          HierarchGroups = HierarchGroups_,
          GroupVariables = GroupVariables_,
          TimeUnit = TimeUnit_,
          
          # Data Wrangling Features----
          ZeroPadSeries = ZeroPadSeries_,
          DataTruncate = DataTruncate_,
          SplitRatios = SplitRatios_,
          PartitionType = PartitionType_,
          
          # Productionize----
          FC_Periods = FC_Periods_,
          EvalMetric = EvalMetric_,
          GridTune = GridTune_,
          ModelCount = ModelCount_,
          Timer = Timer_,
          MaxMem = "28G",
          NThreads = NThreads_,
          
          # Time Series Features----
          Lags = Lags_,
          MA_Periods = MA_Periods_,
          SD_Periods = SD_Periods_,
          Skew_Periods = Skew_Periods_,
          Kurt_Periods = Kurt_Periods_,
          Quantile_Periods = Quantile_Periods_,
          Quantiles_Selected = Quantiles_Selected_,
          
          # Bonus Features----
          NTrees = NTrees_,
          DebugMode = DebugMode_)
        
        # CREATE WARNING OUTPUT----
      }, error = function(x) {
        "error"
      })
      
      # Time runs end----
      EndTime <- Sys.time()
      
      # Fill in table----
      if(is.list(PrintOut)) {
        data.table::set(AutoCARMA_ArgList, i = run, j = "RESULTS_TEST", value = "success")
        data.table::set(AutoCARMA_ArgList, i = run, j = "RUN_TIME", value = EndTime - StartTime)
        if(!TrainOnFull) {
          # print(PrintOut$ModelInformation$EvaluationMetrics)
          # print(PrintOut$ModelInformation$EvaluationMetrics)
          # print(PrintOut$ModelInformation$EvaluationMetrics)
          # print(PrintOut$ModelInformation$EvaluationMetrics)
          # print(PrintOut$ModelInformation$EvaluationMetrics)
          # print(PrintOut$ModelInformation$EvaluationMetrics)
          # print(PrintOut$ModelInformation$EvaluationMetrics)
          tryCatch({data.table::set(AutoCARMA_ArgList, i = run, j = "EvalMetric", value = PrintOut$ModelInformation$EvaluationMetrics[Metric == "MAE"][[2]])}, error = function(x) NULL)
        }
      } else {
        data.table::set(AutoCARMA_ArgList, i = run, j = "RESULTS_TEST", value = "failure")
        data.table::set(AutoCARMA_ArgList, i = run, j = "RUN_TIME", value = EndTime - StartTime)
      }
    }
    
    # H2O-GLM CARMA FUNCTION----
    if(tolower(ModelName) == "h2oglm") {
      PrintOut <- tryCatch({
        AutoH2oGLMCARMA(
          
          # Testing Args----
          TrainOnFull = TrainOnFull,
          XREGS = XREGS__,
          TargetTransformation = TargetTransformation__,
          Difference = Difference__,
          FourierTerms = FourierTerms__,
          CalendarVariables = CalendarVariables__,
          TimeTrendVariable = TimeTrendVariable__,
          HolidayVariable = HolidayVariable__,
          HolidayLags = HolidayLags__,
          HolidayMovingAverages = HolidayMovingAverages__,
          
          # Data
          data = data__,
          
          # Data names----
          TargetColumnName = TargetColumnName_,
          DateColumnName = DateColumnName_,
          HierarchGroups = HierarchGroups_,
          GroupVariables = GroupVariables_,
          TimeUnit = TimeUnit_,
          
          # Data Wrangling Features----
          ZeroPadSeries = ZeroPadSeries_,
          DataTruncate = DataTruncate_,
          SplitRatios = SplitRatios_,
          PartitionType = PartitionType_,
          
          # Productionize----
          FC_Periods = FC_Periods_,
          EvalMetric = EvalMetric_,
          GridTune = GridTune_,
          ModelCount = ModelCount_,
          Timer = Timer_,
          MaxMem = "28G",
          NThreads = NThreads_,
          
          # Time Series Features----
          Lags = Lags_,
          MA_Periods = MA_Periods_,
          SD_Periods = SD_Periods_,
          Skew_Periods = Skew_Periods_,
          Kurt_Periods = Kurt_Periods_,
          Quantile_Periods = Quantile_Periods_,
          Quantiles_Selected = Quantiles_Selected_,
          
          # Bonus Features----
          DebugMode = DebugMode_)
        
        # CREATE WARNING OUTPUT----
      }, error = function(x) {
        "error"
      })
      
      # Time runs end----
      EndTime <- Sys.time()
      
      # Fill in table----
      if(is.list(PrintOut)) {
        data.table::set(AutoCARMA_ArgList, i = run, j = "RESULTS_TEST", value = "success")
        data.table::set(AutoCARMA_ArgList, i = run, j = "RUN_TIME", value = EndTime - StartTime)
        if(!TrainOnFull) {
          # print(PrintOut$ModelInformation$EvaluationMetrics)
          # print(PrintOut$ModelInformation$EvaluationMetrics)
          # print(PrintOut$ModelInformation$EvaluationMetrics)
          # print(PrintOut$ModelInformation$EvaluationMetrics)
          # print(PrintOut$ModelInformation$EvaluationMetrics)
          # print(PrintOut$ModelInformation$EvaluationMetrics)
          # print(PrintOut$ModelInformation$EvaluationMetrics)
          tryCatch({data.table::set(AutoCARMA_ArgList, i = run, j = "EvalMetric", value = PrintOut$ModelInformation$EvaluationMetrics[Metric == "MAE"][[2]])}, error = function(x) NULL)
        }
      } else {
        data.table::set(AutoCARMA_ArgList, i = run, j = "RESULTS_TEST", value = "failure")
        data.table::set(AutoCARMA_ArgList, i = run, j = "RUN_TIME", value = EndTime - StartTime)
      }
    }
    
    # H2O-GLM CARMA FUNCTION----
    if(tolower(ModelName) == "h2oautoml") {
      PrintOut <- tryCatch({
        AutoH2oMLCARMA(
          
          # Testing Args----
          TrainOnFull = TrainOnFull,
          XREGS = XREGS__,
          TargetTransformation = TargetTransformation__,
          Difference = Difference__,
          FourierTerms = FourierTerms__,
          CalendarVariables = CalendarVariables__,
          TimeTrendVariable = TimeTrendVariable__,
          HolidayVariable = HolidayVariable__,
          HolidayLags = HolidayLags__,
          HolidayMovingAverages = HolidayMovingAverages__,
          
          # Data
          data = data__,
          
          # Data names----
          TargetColumnName = TargetColumnName_,
          DateColumnName = DateColumnName_,
          HierarchGroups = HierarchGroups_,
          GroupVariables = GroupVariables_,
          TimeUnit = TimeUnit_,
          
          # Data Wrangling Features----
          ZeroPadSeries = ZeroPadSeries_,
          DataTruncate = DataTruncate_,
          SplitRatios = SplitRatios_,
          PartitionType = PartitionType_,
          
          # Productionize----
          FC_Periods = FC_Periods_,
          EvalMetric = EvalMetric_,
          GridTune = GridTune_,
          ModelCount = ModelCount_,
          Timer = Timer_,
          MaxMem = "28G",
          NThreads = NThreads_,
          
          # Time Series Features----
          Lags = Lags_,
          MA_Periods = MA_Periods_,
          SD_Periods = SD_Periods_,
          Skew_Periods = Skew_Periods_,
          Kurt_Periods = Kurt_Periods_,
          Quantile_Periods = Quantile_Periods_,
          Quantiles_Selected = Quantiles_Selected_,
          
          # Bonus Features----
          DebugMode = DebugMode_)
        
        # CREATE WARNING OUTPUT----
      }, error = function(x) {
        "error"
      })
      
      # Time runs end----
      EndTime <- Sys.time()
      
      # Fill in table----
      if(is.list(PrintOut)) {
        data.table::set(AutoCARMA_ArgList, i = run, j = "RESULTS_TEST", value = "success")
        data.table::set(AutoCARMA_ArgList, i = run, j = "RUN_TIME", value = EndTime - StartTime)
        if(!TrainOnFull) {
          # print(PrintOut$ModelInformation$EvaluationMetrics)
          # print(PrintOut$ModelInformation$EvaluationMetrics)
          # print(PrintOut$ModelInformation$EvaluationMetrics)
          # print(PrintOut$ModelInformation$EvaluationMetrics)
          # print(PrintOut$ModelInformation$EvaluationMetrics)
          # print(PrintOut$ModelInformation$EvaluationMetrics)
          # print(PrintOut$ModelInformation$EvaluationMetrics)
          tryCatch({data.table::set(AutoCARMA_ArgList, i = run, j = "EvalMetric", value = PrintOut$ModelInformation$EvaluationMetrics[Metric == "MAE"][[2]])}, error = function(x) NULL)
        }
      } else {
        data.table::set(AutoCARMA_ArgList, i = run, j = "RESULTS_TEST", value = "failure")
        data.table::set(AutoCARMA_ArgList, i = run, j = "RUN_TIME", value = EndTime - StartTime)
      }
    }
    
    # H2O-GBM CARMA FUNCTION----
    if(tolower(ModelName) == "h2ogbm") {
      PrintOut <- tryCatch({
        AutoH2oGBMCARMA(
          
          # Testing Args----
          TrainOnFull = TrainOnFull,
          XREGS = XREGS__,
          TargetTransformation = TargetTransformation__,
          Difference = Difference__,
          FourierTerms = FourierTerms__,
          CalendarVariables = CalendarVariables__,
          TimeTrendVariable = TimeTrendVariable__,
          HolidayVariable = HolidayVariable__,
          HolidayLags = HolidayLags__,
          HolidayMovingAverages = HolidayMovingAverages__,
          
          # Data
          data = data__,
          
          # Data names----
          TargetColumnName = TargetColumnName_,
          DateColumnName = DateColumnName_,
          HierarchGroups = HierarchGroups_,
          GroupVariables = GroupVariables_,
          TimeUnit = TimeUnit_,
          
          # Data Wrangling Features----
          ZeroPadSeries = ZeroPadSeries_,
          DataTruncate = DataTruncate_,
          SplitRatios = SplitRatios_,
          PartitionType = PartitionType_,
          
          # Productionize----
          FC_Periods = FC_Periods_,
          EvalMetric = EvalMetric_,
          GridTune = GridTune_,
          ModelCount = ModelCount_,
          Timer = Timer_,
          MaxMem = MaxMem_,
          NThreads = max(1,parallel::detectCores()-2),
          
          # Time Series Features----
          Lags = Lags_,
          MA_Periods = MA_Periods_,
          SD_Periods = SD_Periods_,
          Skew_Periods = Skew_Periods_,
          Kurt_Periods = Kurt_Periods_,
          Quantile_Periods = Quantile_Periods_,
          Quantiles_Selected = Quantiles_Selected_,
          
          # Bonus Features----
          NTrees = NTrees_,
          DebugMode = DebugMode_)
        
        # CREATE WARNING OUTPUT----
      }, error = function(x) {
        "error"
      })
      
      # Time runs end----
      EndTime <- Sys.time()
      
      # Fill in table----
      if(is.list(PrintOut)) {
        data.table::set(AutoCARMA_ArgList, i = run, j = "RESULTS_TEST", value = "success")
        data.table::set(AutoCARMA_ArgList, i = run, j = "RUN_TIME", value = EndTime - StartTime)
        if(!TrainOnFull) {
          # print(PrintOut$ModelInformation$EvaluationMetrics)
          # print(PrintOut$ModelInformation$EvaluationMetrics)
          # print(PrintOut$ModelInformation$EvaluationMetrics)
          # print(PrintOut$ModelInformation$EvaluationMetrics)
          # print(PrintOut$ModelInformation$EvaluationMetrics)
          # print(PrintOut$ModelInformation$EvaluationMetrics)
          # print(PrintOut$ModelInformation$EvaluationMetrics)
          tryCatch({data.table::set(AutoCARMA_ArgList, i = run, j = "EvalMetric", value = PrintOut$ModelInformation$EvaluationMetrics[Metric == "MAE"][[2]])}, error = function(x) NULL)
        }
      } else {
        data.table::set(AutoCARMA_ArgList, i = run, j = "RESULTS_TEST", value = "failure")
        data.table::set(AutoCARMA_ArgList, i = run, j = "RUN_TIME", value = EndTime - StartTime)
      }
    }
    
    # XGBoost CARMA FUNCTION----
    if(tolower(ModelName) == "xgboost") {
      PrintOut <- tryCatch({
        AutoXGBoostCARMA(
          
          # Testing Args----
          TrainOnFull = TrainOnFull,
          TreeMethod = TreeMethod__,
          XREGS = XREGS__,
          TargetTransformation = TargetTransformation__,
          Difference = Difference__,
          FourierTerms = FourierTerms__,
          CalendarVariables = CalendarVariables__,
          TimeTrendVariable = TimeTrendVariable__,
          HolidayVariable = HolidayVariable__,
          HolidayLags = HolidayLags__,
          HolidayMovingAverages = HolidayMovingAverages__,
          
          # Data
          data = data__,
          
          # Data names----
          TargetColumnName = TargetColumnName_,
          DateColumnName = DateColumnName_,
          HierarchGroups = HierarchGroups_,
          GroupVariables = GroupVariables_,
          TimeUnit = TimeUnit_,
          
          # Data Wrangling Features----
          ZeroPadSeries = ZeroPadSeries_,
          DataTruncate = DataTruncate_,
          SplitRatios = SplitRatios_,
          PartitionType = PartitionType_,
          
          # Productionize----
          FC_Periods = FC_Periods_,
          EvalMetric = EvalMetric_,
          GridTune = GridTune_,
          ModelCount = ModelCount_,
          Timer = Timer_,
          NThreads = NThreads_,
          
          # Time Series Features----
          Lags = Lags_,
          MA_Periods = MA_Periods_,
          SD_Periods = SD_Periods_,
          Skew_Periods = Skew_Periods_,
          Kurt_Periods = Kurt_Periods_,
          Quantile_Periods = Quantile_Periods_,
          Quantiles_Selected = Quantiles_Selected_,
          
          # Bonus Features----
          NTrees = NTrees_,
          DebugMode = DebugMode_)
        
        # CREATE WARNING OUTPUT----
      }, error = function(x) {
        "error"
      })
      
      # Time runs end----
      EndTime <- Sys.time()
      
      # Fill in table----
      if(is.list(PrintOut)) {
        data.table::set(AutoCARMA_ArgList, i = run, j = "RESULTS_TEST", value = "success")
        data.table::set(AutoCARMA_ArgList, i = run, j = "RUN_TIME", value = EndTime - StartTime)
        if(!TrainOnFull) {
          # print(PrintOut$ModelInformation$EvaluationMetrics)
          # print(PrintOut$ModelInformation$EvaluationMetrics)
          # print(PrintOut$ModelInformation$EvaluationMetrics)
          # print(PrintOut$ModelInformation$EvaluationMetrics)
          # print(PrintOut$ModelInformation$EvaluationMetrics)
          # print(PrintOut$ModelInformation$EvaluationMetrics)
          # print(PrintOut$ModelInformation$EvaluationMetrics)
          tryCatch({data.table::set(AutoCARMA_ArgList, i = run, j = "EvalMetric", value = PrintOut$ModelInformation$EvaluationMetrics[Metric == "MAE"][[2]])}, error = function(x) NULL)
        }
      } else {
        data.table::set(AutoCARMA_ArgList, i = run, j = "RESULTS_TEST", value = "failure")
        data.table::set(AutoCARMA_ArgList, i = run, j = "RUN_TIME", value = EndTime - StartTime)
      }
    }
  }
  
  # Restore default settings----
  options(warn = 0)
  
  # Return Print Out----
  return(AutoCARMA_ArgList)
}

#' QA_WALMARTDATAGENERATOR
#' 
#' @family QA
#' @author Adrian Antico
#' @param data supply walmart data for either a single group or two group case. For no group, use XX
#' @param Groups Supply either 0L, 1L, or 2L to indicate the number of group variables to have tested
#' @param TimeUnit__ = TimeUnit_
#' @export
QA_WALMARTDATAGENERATOR <- function(data,
                                    Groups = 1L,
                                    TimeUnit__ = "WEEK") {
  
  # Quarterly DATA:----
  if(Groups == 0L & toupper(TimeUnit__) %chin% c("QUARTER","QUARTERS")) {
    dataFull <- data[, .SD, .SDcols = c("Date","Weekly_Sales")]
    XREGS <- data[, .SD, .SDcols = c("Date","Weekly_Sales2")]
    data.table::setnames(XREGS, "Weekly_Sales2","Xtra1")
    dataForecastX <- dataFull
    dataFull <- dataFull[1:110]
    dataFull <- dataFull[, .SD, .SDcols = c("Date","Weekly_Sales")]
    dataForecastX <- dataForecastX[, .SD, .SDcols = c("Date","Weekly_Sales")]
    
    # Save data
    # data.table::fwrite(dataFull, file = file.path(path, "NO_GROUP_QUARTER.csv"))
    # data.table::fwrite(XREGS, file = file.path(path, "NO_GROUP_QUARTER_XREGS-1.csv"))
    # data.table::fwrite(dataForecastX, file = file.path(path, "NO_GROUP_QUARTER_FC.csv"))  
  }
  
  # Monthly DATA:----
  if(Groups == 0L & TimeUnit__ %chin% c("MONTHS","MONTH")) {
    dataFull <- data[, .SD, .SDcols = c("Date","Weekly_Sales")]
    data.table::set(dataFull, j = "Date", value = as.Date(dataFull[["Date"]]))
    XREGS <- dataFull[, .SD, .SDcols = c("Date","Weekly_Sales2")]
    data.table::setnames(XREGS, "Weekly_Sales2","Xtra1")
    dataForecastX <- dataFull
    dataFull <- dataFull[1:110]
    dataFull <- dataFull[, .SD, .SDcols = c("Date","Weekly_Sales")]
    dataForecastX <- dataForecastX[, .SD, .SDcols = c("Date","Weekly_Sales")]
    
    dataFull[, Date := min(Date)]
    dataFull[, ID := 1:.N]
    data.table::set(dataFull, j = "Date", value = as.Date(dataFull[["Date"]]))
    data.table::set(dataFull, j = "Date", value = dataFull[["Date"]] %m+% months(dataFull[["ID"]]))
    dataFull <- dataFull[, .SD, .SDcols = c("Date","Weekly_Sales")]
    
    dataForecastX[, Date := min(dataForecastX$Date)]
    dataForecastX[, ID := 1:.N]
    data.table::set(dataForecastX, j = "Date", value = as.Date(dataForecastX[["Date"]]))
    data.table::set(dataForecastX, j = "Date", value = dataForecastX[["Date"]] %m+% months(dataForecastX[["ID"]]))
    dataFull <- dataFull[, .SD, .SDcols = c("Date","Weekly_Sales")]
    
    XREGS[, Date := min(Date)]
    XREGS[, ID := 1:.N]
    data.table::set(XREGS, j = "Date", value = as.Date(XREGS[["Date"]]))
    data.table::set(XREGS, j = "Date", value = XREGS[["Date"]] %m+% months(XREGS[["ID"]]))
    data.table::set(XREGS, j = "ID", value = NULL)
    
    # Save data
    # data.table::fwrite(dataFull, file = file.path(path, "NO_GROUP_MONTH.csv"))
    # data.table::fwrite(XREGS, file = file.path(path, "NO_GROUP_MONTH_XREGS-1.csv"))
    # data.table::fwrite(dataForecastX, file = file.path(path, "NO_GROUP_MONTH_FC.csv"))  
  }
  
  # Weekly DATA:----
  if(Groups == 0L & TimeUnit__ %chin% c("WEEKS","WEEK")) {
    dataFull <- data[, .SD, .SDcols = c("Date","Weekly_Sales")]
    data.table::set(dataFull, j = "Date", value = as.Date(dataFull[["Date"]]))
    XREGS <- dataFull[, .SD, .SDcols = c("Date","Weekly_Sales2")]
    data.table::setnames(XREGS, "Weekly_Sales2","Xtra1")
    dataForecastX <- dataFull
    dataFull <- dataFull[1:110]
    dataFull <- dataFull[, .SD, .SDcols = c("Date","Weekly_Sales")]
    dataForecastX <- dataForecastX[, .SD, .SDcols = c("Date","Weekly_Sales")]
    
    dataFull[, Date := min(Date)]
    dataFull[, ID := 1:.N]
    data.table::set(dataFull, j = "Date", value = as.Date(dataFull[["Date"]]))
    data.table::set(dataFull, j = "Date", value = dataFull[["Date"]] + lubridate::weeks(dataFull[["ID"]]))
    dataFull <- dataFull[, .SD, .SDcols = c("Date","Weekly_Sales")]
    
    dataForecastX[, Date := min(Date)]
    dataForecastX[, ID := 1:.N]
    data.table::set(dataForecastX, j = "Date", value = as.Date(dataForecastX[["Date"]]))
    data.table::set(dataForecastX, j = "Date", value = dataForecastX[["Date"]] + lubridate::weeks(dataForecastX[["ID"]]))
    dataForecastX <- dataForecastX[, .SD, .SDcols = c("Date","Weekly_Sales")]
    
    XREGS[, Date := min(Date)]
    XREGS[, ID := 1:.N]
    data.table::set(XREGS, j = "Date", value = as.Date(XREGS[["Date"]]))
    data.table::set(XREGS, j = "Date", value = XREGS[["Date"]] + lubridate::weeks(XREGS[["ID"]]))
    data.table::set(XREGS, j = "ID", value = NULL)
    
    # Save data
    # data.table::fwrite(dataFull, file = file.path(path, "NO_GROUP_WEEK.csv"))
    # data.table::fwrite(XREGS, file = file.path(path, "NO_GROUP_WEEK_XREGS-1.csv"))
    # data.table::fwrite(dataForecastX, file = file.path(path, "NO_GROUP_WEEK_FC.csv"))  
  }
  
  # Daily DATA:----
  if(Groups == 0L & TimeUnit__ %chin% c("DAYS","DAY")) {
    dataFull <- data[, .SD, .SDcols = c("Date","Weekly_Sales")]
    data.table::set(dataFull, j = "Date", value = as.Date(dataFull[["Date"]]))
    XREGS <- dataFull[, .SD, .SDcols = c("Date","Weekly_Sales2")]
    data.table::setnames(XREGS, "Weekly_Sales2","Xtra1")
    dataForecastX <- dataFull
    dataFull <- dataFull[1:110]
    dataFull <- dataFull[, .SD, .SDcols = c("Date","Weekly_Sales")]
    dataForecastX <- dataForecastX[, .SD, .SDcols = c("Date","Weekly_Sales")]
    
    dataFull[, Date := min(Date)]
    dataFull[, ID := 1:.N]
    data.table::set(dataFull, j = "Date", value = as.Date(dataFull[["Date"]]))
    data.table::set(dataFull, j = "Date", value = dataFull[["Date"]] + lubridate::days(dataFull[["ID"]]))
    dataFull <- dataFull[, .SD, .SDcols = c("Date","Weekly_Sales")]
    
    dataForecastX[, Date := min(Date)]
    dataForecastX[, ID := 1:.N]
    data.table::set(dataForecastX, j = "Date", value = as.Date(dataForecastX[["Date"]]))
    data.table::set(dataForecastX, j = "Date", value = dataForecastX[["Date"]] + lubridate::days(dataForecastX[["ID"]]))
    dataForecastX <- dataForecastX[, .SD, .SDcols = c("Date","Weekly_Sales")]
    
    XREGS[, Date := min(Date)]
    XREGS[, ID := 1:.N]
    data.table::set(XREGS, j = "Date", value = as.Date(XREGS[["Date"]]))
    data.table::set(XREGS, j = "Date", value = XREGS[["Date"]] + lubridate::days(XREGS[["ID"]]))
    data.table::set(XREGS, j = "ID", value = NULL)
    
    # Save data
    # data.table::fwrite(dataFull, file = file.path(path, "NO_GROUP_DAY.csv"))
    # data.table::fwrite(XREGS, file = file.path(path, "NO_GROUP_DAY_XREGS-1.csv"))
    # data.table::fwrite(dataForecastX, file = file.path(path, "NO_GROUP_DAY_FC.csv"))  
  }
  
  # Hourly DATA:----
  if(Groups == 0L & TimeUnit__ %chin% c("HOURS","HOUR")) {
    dataFull <- data[, .SD, .SDcols = c("Date","Weekly_Sales")]
    data.table::set(dataFull, j = "Date", value = as.Date(dataFull[["Date"]]))
    XREGS <- dataFull[, .SD, .SDcols = c("Date","Weekly_Sales2")]
    data.table::setnames(XREGS, "Weekly_Sales2","Xtra1")
    dataForecastX <- dataFull
    dataFull <- dataFull[1:110]
    dataFull <- dataFull[, .SD, .SDcols = c("Date","Weekly_Sales")]
    dataForecastX <- dataForecastX[, .SD, .SDcols = c("Date","Weekly_Sales")]
    
    dataFull[, Date := min(Date)]
    dataFull[, ID := 1:.N]
    data.table::set(dataFull, j = "Date", value = as.POSIXct(dataFull[["Date"]]))
    data.table::set(dataFull, j = "Date", value = dataFull[["Date"]] + lubridate::hours(dataFull[["ID"]]))
    dataFull <- dataFull[, .SD, .SDcols = c("Date","Weekly_Sales")]
    
    dataForecastX[, Date := min(dataForecastX$Date)]
    dataForecastX[, ID := 1:.N]
    data.table::set(dataForecastX, j = "Date", value = as.POSIXct(dataForecastX[["Date"]]))
    data.table::set(dataForecastX, j = "Date", value = dataForecastX[["Date"]] + lubridate::hours(dataForecastX[["ID"]]))
    dataForecastX <- dataForecastX[, .SD, .SDcols = c("Date","Weekly_Sales")]
    
    XREGS[, Date := min(Date)]
    XREGS[, ID := 1:.N]
    data.table::set(XREGS, j = "Date", value = as.POSIXct(XREGS[["Date"]]))
    data.table::set(XREGS, j = "Date", value = XREGS[["Date"]] + lubridate::hours(XREGS[["ID"]]))
    data.table::set(XREGS, j = "ID", value = NULL)
    
    # Save data
    # data.table::fwrite(dataFull, file = file.path(path, "NO_GROUP_HOUR.csv"))
    # data.table::fwrite(XREGS, file = file.path(path, "NO_GROUP_HOUR_XREGS-1.csv"))
    # data.table::fwrite(dataForecastX, file = file.path(path, "NO_GROUP_HOUR_FC.csv"))  
  }
  
  # 30min DATA:----
  if(Groups == 0L & TimeUnit__ == "30MIN") {
    dataFull <- data[, .SD, .SDcols = c("Date","Weekly_Sales")]
    data.table::set(dataFull, j = "Date", value = as.POSIXct(dataFull[["Date"]]))
    XREGS <- dataFull[, .SD, .SDcols = c("Date","Weekly_Sales2")]
    data.table::setnames(XREGS, "Weekly_Sales2","Xtra1")
    dataForecastX <- dataFull
    dataFull <- dataFull[1:110]
    dataFull <- dataFull[, .SD, .SDcols = c("Date","Weekly_Sales")]
    dataForecastX <- dataForecastX[, .SD, .SDcols = c("Date","Weekly_Sales")]
    
    dataFull[, Date := min(Date)]
    dataFull[, ID := 1:.N]
    data.table::set(dataFull, j = "Date", value = as.POSIXct(dataFull[["Date"]]))
    data.table::set(dataFull, j = "ID", value = dataFull[["ID"]]*30)
    data.table::set(dataFull, j = "Date", value = dataFull[["Date"]] + lubridate::minutes(dataFull[["ID"]]))
    dataFull <- dataFull[, .SD, .SDcols = c("Date","Weekly_Sales")]
    
    dataForecastX[, Date := min(dataForecastX$Date)]
    dataForecastX[, ID := 1:.N]
    data.table::set(dataForecastX, j = "ID", value = dataForecastX[["ID"]] * 5)
    data.table::set(dataForecastX, j = "Date", value = as.POSIXct(dataForecastX[["Date"]]))
    data.table::set(dataForecastX, j = "Date", value = dataForecastX[["Date"]] + lubridate::minutes(dataForecastX[["ID"]]))
    dataForecastX <- dataForecastX[, .SD, .SDcols = c("Date","Weekly_Sales")]
    
    XREGS[, Date := min(Date)]
    XREGS[, ID := 1:.N]
    data.table::set(XREGS, j = "ID", value = XREGS[["ID"]])
    data.table::set(XREGS, j = "ID", value = XREGS[["ID"]] * 30)
    data.table::set(XREGS, j = "Date", value = as.POSIXct(XREGS[["Date"]]))
    data.table::set(XREGS, j = "Date", value = XREGS[["Date"]] + lubridate::minutes(XREGS[["ID"]]))
    data.table::set(XREGS, j = "ID", value = NULL)
    
    # Save data
    # data.table::fwrite(dataFull, file = file.path(path, "NO_GROUP_30min.csv"))
    # data.table::fwrite(XREGS, file = file.path(path, "NO_GROUP_30min_XREGS-1.csv"))
    # data.table::fwrite(dataForecastX, file = file.path(path, "NO_GROUP_30min_FC.csv"))  
  }
  
  # 5min DATA:----
  if(Groups == 0L & TimeUnit__ == "5MIN") {
    dataFull <- data[, .SD, .SDcols = c("Date","Weekly_Sales")]
    data.table::set(dataFull, j = "Date", value = as.POSIXct(dataFull[["Date"]]))
    XREGS <- dataFull[, .SD, .SDcols = c("Date","Weekly_Sales2")]
    data.table::setnames(XREGS, "Weekly_Sales2","Xtra1")
    dataForecastX <- dataFull
    dataFull <- dataFull[1:110]
    dataFull <- dataFull[, .SD, .SDcols = c("Date","Weekly_Sales")]
    dataForecastX <- dataForecastX[, .SD, .SDcols = c("Date","Weekly_Sales")]
    
    dataFull[, Date := min(Date)]
    dataFull[, ID := 1:.N]
    data.table::set(dataFull, j = "ID", value = dataFull[["ID"]] * 5)
    data.table::set(dataFull, j = "Date", value = as.POSIXct(dataFull[["Date"]]))
    data.table::set(dataFull, j = "Date", value = dataFull[["Date"]] + lubridate::minutes(5 * dataFull[["ID"]]))
    dataFull <- dataFull[, .SD, .SDcols = c("Date","Weekly_Sales")]
    
    dataForecastX[, Date := min(Date)]
    dataForecastX[, ID := 1:.N]
    data.table::set(dataForecastX, j = "ID", value = dataForecastX[["ID"]] * 5)
    data.table::set(dataForecastX, j = "Date", value = as.POSIXct(dataForecastX[["Date"]]))
    data.table::set(dataForecastX, j = "Date", value = dataForecastX[["Date"]] + lubridate::minutes(5 * dataForecastX[["ID"]]))
    dataForecastX <- dataForecastX[, .SD, .SDcols = c("Date","Weekly_Sales")]
    
    XREGS[, Date := min(dataFull$Date)]
    XREGS[, ID := 1:.N]
    data.table::set(XREGS, j = "ID", value = XREGS[["ID"]] * 5)
    data.table::set(XREGS, j = "Date", value = as.POSIXct(XREGS[["Date"]]))
    data.table::set(XREGS, j = "Date", value = XREGS[["Date"]] + lubridate::minutes(XREGS[["ID"]]))
    data.table::set(XREGS, j = "ID", value = NULL)
  }
  
  #----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@----
  # 1 group time series data----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@----
  
  # Weekly DATA:----
  if(Groups == 1L & TimeUnit__ %chin% c("WEEKS","WEEK")) {
    
    # Import data and shrink it so the QA goes faster
    dataFull <- data[, .SD, .SDcols = c("GroupVar","Date","Weekly_Sales")]
    StoreDept <- unique(dataFull[["GroupVar"]])
    dataFull <- dataFull[GroupVar %in% StoreDept[1:10]]
    
    XREGS1 <- data.table::copy(dataFull)
    data.table::set(XREGS1, j = "Xtra1", value = jitter(XREGS1[["Weekly_Sales"]], factor = 2500))
    data.table::set(XREGS1, j = "Weekly_Sales", value = NULL)
    if("ID" %chin% names(XREGS1)) {
      data.table::set(XREGS1, j = "ID", value = NULL)
    }
    
    XREGS2 <- data.table::copy(dataFull)
    data.table::set(XREGS2, j = "Xtra1", value = jitter(XREGS2[["Weekly_Sales"]], factor = 2500))
    data.table::set(XREGS2, j = "Xtra2", value = jitter(XREGS2[["Xtra1"]], factor = 3500))
    data.table::set(XREGS2, j = "Weekly_Sales", value = NULL)
    if("ID" %chin% names(XREGS2)) {
      data.table::set(XREGS2, j = "ID", value = NULL)
    }
    
    dataForecastX <- dataFull
    dataFull <- dataFull[, ID := 1:.N, by = list(GroupVar)][ID < 139][, ID := NULL]
    if("ID" %chin% names(dataForecastX)) {
      data.table::set(dataForecastX, j = "ID", value = NULL)
    }
  }
  
  # Monthly DATA:----
  if(Groups == 1L & TimeUnit__ %chin% c("MONTHS","MONTH")) {
    
    # Import data and shrink it so the QA goes faster
    dataFull <- data[, .SD, .SDcols = c("GroupVar","Date","Weekly_Sales")]
    StoreDept <- unique(dataFull[["GroupVar"]])
    dataFull <- dataFull[GroupVar %in% StoreDept[1:10]]
    
    # Convert to monthly
    dataFull[, Date := min(Date)]
    dataFull[, ID := 1:.N, by = list(GroupVar)]
    data.table::set(dataFull, j = "Date", value = as.Date(dataFull[["Date"]]))
    data.table::set(dataFull, j = "Date", value = dataFull[["Date"]] %m+% months(dataFull[["ID"]]))
    
    XREGS1 <- data.table::copy(dataFull)
    data.table::set(XREGS1, j = "Xtra1", value = jitter(XREGS1[["Weekly_Sales"]], factor = 2500))
    data.table::set(XREGS1, j = "Weekly_Sales", value = NULL)
    if("ID" %chin% names(XREGS1)) data.table::set(XREGS1, j = "ID", value = NULL)
    
    XREGS2 <- data.table::copy(dataFull)
    data.table::set(XREGS2, j = "Xtra1", value = jitter(XREGS2[["Weekly_Sales"]], factor = 2500))
    data.table::set(XREGS2, j = "Xtra2", value = jitter(XREGS2[["Xtra1"]], factor = 3500))
    data.table::set(XREGS2, j = "Weekly_Sales", value = NULL)
    if("ID" %chin% names(XREGS2)) data.table::set(XREGS2, j = "ID", value = NULL)
    
    dataForecastX <- dataFull
    dataFull <- dataFull[, ID := 1:.N, by = list(GroupVar)][ID < 139][, ID := NULL]
    if("ID" %chin% names(dataForecastX)) data.table::set(dataForecastX, j = "ID", value = NULL) 
  }
  
  # Quarterly DATA:----
  if(Groups == 1L & TimeUnit__ %chin% c("QUARTER","QUARTERS")) {
    
    # Import data and shrink it so the QA goes faster
    dataFull <- data[, .SD, .SDcols = c("GroupVar","Date","Weekly_Sales")]
    StoreDept <- unique(dataFull[["GroupVar"]])
    dataFull <- dataFull[GroupVar %in% StoreDept[1:10]]
    
    # Convert to quarterly
    dataFull[, Date := min(Date)]
    dataFull[, ID := 1:.N, by = list(GroupVar)]
    data.table::set(dataFull, j = "Date", value = as.Date(dataFull[["Date"]]))
    data.table::set(dataFull, j = "Date", value = dataFull[["Date"]] + lubridate::quarter(dataFull[["ID"]]))
    
    XREGS1 <- data.table::copy(dataFull)
    data.table::set(XREGS1, j = "Xtra1", value = jitter(XREGS1[["Weekly_Sales"]], factor = 2500))
    data.table::set(XREGS1, j = "Weekly_Sales", value = NULL)
    if("ID" %chin% names(XREGS1)) data.table::set(XREGS1, j = "ID", value = NULL)
    
    XREGS2 <- data.table::copy(dataFull)
    data.table::set(XREGS2, j = "Xtra1", value = jitter(XREGS2[["Weekly_Sales"]], factor = 2500))
    data.table::set(XREGS2, j = "Xtra2", value = jitter(XREGS2[["Xtra1"]], factor = 3500))
    data.table::set(XREGS2, j = "Weekly_Sales", value = NULL)
    if("ID" %chin% names(XREGS2)) data.table::set(XREGS2, j = "ID", value = NULL)
    
    dataForecastX <- dataFull
    dataFull <- dataFull[, ID := 1:.N, by = list(GroupVar)][ID < 139][, ID := NULL]
    if("ID" %chin% names(dataForecastX)) data.table::set(dataForecastX, j = "ID", value = NULL)  
  }
  
  # Yearly DATA:----
  if(Groups == 1L & TimeUnit__ == "YEAR") {
    
    # Import data and shrink it so the QA goes faster
    dataFull <- data[, .SD, .SDcols = c("GroupVar","Date","Weekly_Sales")]
    StoreDept <- unique(dataFull[["GroupVar"]])
    dataFull <- dataFull[GroupVar %in% StoreDept[1:10]]
    
    # Convert to Yearly
    dataFull[, Date := min(Date)]
    dataFull[, ID := 1:.N, by = list(GroupVar)]
    data.table::set(dataFull, j = "Date", value = as.Date(dataFull[["Date"]]))
    data.table::set(dataFull, j = "Date", value = dataFull[["Date"]] + lubridate::years(dataFull[["ID"]]))
    
    XREGS1 <- data.table::copy(dataFull)
    data.table::set(XREGS1, j = "Xtra1", value = jitter(XREGS1[["Weekly_Sales"]], factor = 2500))
    data.table::set(XREGS1, j = "Weekly_Sales", value = NULL)
    if("ID" %chin% names(XREGS1)) data.table::set(XREGS1, j = "ID", value = NULL)
    
    XREGS2 <- data.table::copy(dataFull)
    data.table::set(XREGS2, j = "Xtra1", value = jitter(XREGS2[["Weekly_Sales"]], factor = 2500))
    data.table::set(XREGS2, j = "Xtra2", value = jitter(XREGS2[["Xtra1"]], factor = 3500))
    data.table::set(XREGS2, j = "Weekly_Sales", value = NULL)
    if("ID" %chin% names(XREGS2)) data.table::set(XREGS2, j = "ID", value = NULL)
    
    dataForecastX <- dataFull
    dataFull <- dataFull[, ID := 1:.N, by = list(GroupVar)][ID < 139][, ID := NULL]
    if("ID" %chin% names(dataForecastX)) data.table::set(dataForecastX, j = "ID", value = NULL) 
  }
  
  # Daily DATA:----
  if(Groups == 1L & TimeUnit__ %chin% c("DAYS","DAY")) {
    
    # Import data and shrink it so the QA goes faster
    dataFull <- data[, .SD, .SDcols = c("GroupVar","Date","Weekly_Sales")]
    StoreDept <- unique(dataFull[["GroupVar"]])
    dataFull <- dataFull[GroupVar %in% StoreDept[1:10]]
    
    # Convert to daily
    dataFull[, Date := min(Date)]
    dataFull[, ID := 1:.N, by = list(GroupVar)]
    data.table::set(dataFull, j = "Date", value = as.Date(dataFull[["Date"]]))
    data.table::set(dataFull, j = "Date", value = dataFull[["Date"]] + lubridate::days(dataFull[["ID"]]))
    
    XREGS1 <- data.table::copy(dataFull)
    data.table::set(XREGS1, j = "Xtra1", value = jitter(XREGS1[["Weekly_Sales"]], factor = 2500))
    data.table::set(XREGS1, j = "Weekly_Sales", value = NULL)
    if("ID" %chin% names(XREGS1)) data.table::set(XREGS1, j = "ID", value = NULL)
    
    XREGS2 <- data.table::copy(dataFull)
    data.table::set(XREGS2, j = "Xtra1", value = jitter(XREGS2[["Weekly_Sales"]], factor = 2500))
    data.table::set(XREGS2, j = "Xtra2", value = jitter(XREGS2[["Xtra1"]], factor = 3500))
    data.table::set(XREGS2, j = "Weekly_Sales", value = NULL)
    if("ID" %chin% names(XREGS2)) data.table::set(XREGS2, j = "ID", value = NULL)
    
    dataForecastX <- dataFull
    dataFull <- dataFull[, ID := 1:.N, by = list(GroupVar)][ID < 139][, ID := NULL]
    if("ID" %chin% names(dataForecastX)) data.table::set(dataForecastX, j = "ID", value = NULL) 
  }
  
  # Hourly DATA:----
  if(Groups == 1L & TimeUnit__ %chin% c("HOURS","HOUR")) {
    
    # Import data and shrink it so the QA goes faster
    dataFull <- data[, .SD, .SDcols = c("GroupVar","Date","Weekly_Sales")]
    StoreDept <- unique(dataFull[["GroupVar"]])
    dataFull <- dataFull[GroupVar %in% StoreDept[1:10]]
    
    # Convert to hourly
    dataFull[, Date := min(Date)]
    dataFull[, ID := 1:.N, by = list(GroupVar)]
    data.table::set(dataFull, j = "Date", value = as.POSIXct(dataFull[["Date"]]))
    data.table::set(dataFull, j = "Date", value = dataFull[["Date"]] + lubridate::hours(dataFull[["ID"]]))
    
    XREGS1 <- data.table::copy(dataFull)
    data.table::set(XREGS1, j = "Xtra1", value = jitter(XREGS1[["Weekly_Sales"]], factor = 2500))
    data.table::set(XREGS1, j = "Weekly_Sales", value = NULL)
    if("ID" %chin% names(XREGS1)) data.table::set(XREGS1, j = "ID", value = NULL)
    
    XREGS2 <- data.table::copy(dataFull)
    data.table::set(XREGS2, j = "Xtra1", value = jitter(XREGS2[["Weekly_Sales"]], factor = 2500))
    data.table::set(XREGS2, j = "Xtra2", value = jitter(XREGS2[["Xtra1"]], factor = 3500))
    data.table::set(XREGS2, j = "Weekly_Sales", value = NULL)
    if("ID" %chin% names(XREGS2)) data.table::set(XREGS2, j = "ID", value = NULL)
    
    dataForecastX <- dataFull
    dataFull <- dataFull[, ID := 1:.N, by = list(GroupVar)][ID < 139][, ID := NULL]
    if("ID" %chin% names(dataForecastX)) data.table::set(dataForecastX, j = "ID", value = NULL) 
  }
  
  # 30 min DATA:----
  if(Groups == 1L & TimeUnit__ == "30MIN") {
    
    # Import data and shrink it so the QA goes faster
    dataFull <- data[, .SD, .SDcols = c("GroupVar","Date","Weekly_Sales")]
    StoreDept <- unique(dataFull[["GroupVar"]])
    dataFull <- dataFull[GroupVar %in% StoreDept[1:10]]
    
    # Convert to hourly
    dataFull[, Date := min(Date)]
    dataFull[, ID := 1:.N, by = list(GroupVar)]
    data.table::set(dataFull, j = "ID", value = dataFull[["ID"]] * 30)
    data.table::set(dataFull, j = "Date", value = as.POSIXct(dataFull[["Date"]]))
    data.table::set(dataFull, j = "Date", value = dataFull[["Date"]] + lubridate::minutes(dataFull[["ID"]]))
    
    XREGS1 <- data.table::copy(dataFull)
    data.table::set(XREGS1, j = "Xtra1", value = jitter(XREGS1[["Weekly_Sales"]], factor = 2500))
    data.table::set(XREGS1, j = "Weekly_Sales", value = NULL)
    if("ID" %chin% names(XREGS1)) data.table::set(XREGS1, j = "ID", value = NULL)
    
    XREGS2 <- data.table::copy(dataFull)
    data.table::set(XREGS2, j = "Xtra1", value = jitter(XREGS2[["Weekly_Sales"]], factor = 2500))
    data.table::set(XREGS2, j = "Xtra2", value = jitter(XREGS2[["Xtra1"]], factor = 3500))
    data.table::set(XREGS2, j = "Weekly_Sales", value = NULL)
    if("ID" %chin% names(XREGS2)) data.table::set(XREGS2, j = "ID", value = NULL)
    
    dataForecastX <- dataFull
    dataFull <- dataFull[, ID := 1:.N, by = list(GroupVar)][ID < 139][, ID := NULL]
    if("ID" %chin% names(dataForecastX)) data.table::set(dataForecastX, j = "ID", value = NULL)
  }
  
  #----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@----
  # 2 group time series data----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@----
  
  # Weekly DATA:----
  if(Groups == 2L & TimeUnit__ %chin% c("WEEKS","WEEK")) {
    
    # Data
    dataFull <- data[, .SD, .SDcols = c("Store","Dept","Date","Weekly_Sales")]
    dataFull <- dataFull[Store %in% c(1,2)]
    
    XREGS1 <- data.table::copy(dataFull)
    data.table::set(XREGS1, j = "Xtra1", value = jitter(XREGS1[["Weekly_Sales"]], factor = 2500))
    data.table::set(XREGS1, j = "Weekly_Sales", value = NULL)
    if("ID" %chin% names(XREGS1)) data.table::set(XREGS1, j = "ID", value = NULL)
    
    XREGS2 <- data.table::copy(dataFull)
    data.table::set(XREGS2, j = "Xtra1", value = jitter(XREGS2[["Weekly_Sales"]], factor = 2500))
    data.table::set(XREGS2, j = "Xtra2", value = jitter(XREGS2[["Xtra1"]], factor = 3500))
    data.table::set(XREGS2, j = "Weekly_Sales", value = NULL)
    if("ID" %chin% names(XREGS2)) data.table::set(XREGS2, j = "ID", value = NULL)
    
    dataForecastX <- dataFull
    dataFull <- dataFull[, ID := 1:.N, by = list(Store,Dept)][ID < 139][, ID := NULL]
    if("ID" %chin% names(dataForecastX)) data.table::set(dataForecastX, j = "ID", value = NULL) 
  }
  
  # Monthly DATA:----
  if(Groups == 2L & TimeUnit__ %chin% c("MONTHS","MONTH")) {
    # Data
    dataFull <- data[, .SD, .SDcols = c("Store","Dept","Date","Weekly_Sales")]
    dataFull <- dataFull[Store %in% c(1,2)]
    
    # Convert to monthly
    dataFull[, Date := min(Date)]
    dataFull[, ID := 1:.N, by = list(Store,Dept)]
    data.table::set(dataFull, j = "Date", value = as.Date(dataFull[["Date"]]))
    data.table::set(dataFull, j = "Date", value = dataFull[["Date"]] %m+% months(dataFull[["ID"]]))
    
    XREGS1 <- data.table::copy(dataFull)
    data.table::set(XREGS1, j = "Xtra1", value = jitter(XREGS1[["Weekly_Sales"]], factor = 2500))
    data.table::set(XREGS1, j = "Weekly_Sales", value = NULL)
    if("ID" %chin% names(XREGS1)) data.table::set(XREGS1, j = "ID", value = NULL)
    
    XREGS2 <- data.table::copy(dataFull)
    data.table::set(XREGS2, j = "Xtra1", value = jitter(XREGS2[["Weekly_Sales"]], factor = 2500))
    data.table::set(XREGS2, j = "Xtra2", value = jitter(XREGS2[["Xtra1"]], factor = 3500))
    data.table::set(XREGS2, j = "Weekly_Sales", value = NULL)
    if("ID" %chin% names(XREGS2)) data.table::set(XREGS2, j = "ID", value = NULL)
    
    dataForecastX <- dataFull
    dataFull <- dataFull[, ID := 1:.N, by = list(Store,Dept)][ID < 139][, ID := NULL]
    if("ID" %chin% names(dataForecastX)) data.table::set(dataForecastX, j = "ID", value = NULL)
  }
  
  # Quarterly DATA:----
  if(Groups == 2L & TimeUnit__ %chin% c("QUARTER","QUARTERS")) {
    
    # Data
    dataFull <- data[, .SD, .SDcols = c("Store","Dept","Date","Weekly_Sales")]
    dataFull <- dataFull[Store %in% c(1,2)]
    
    # Convert to quarterly
    dataFull[, Date := min(Date)]
    dataFull[, ID := 1:.N, by = list(Store,Dept)]
    data.table::set(dataFull, j = "Date", value = as.Date(dataFull[["Date"]]))
    data.table::set(dataFull, j = "Date", value = dataFull[["Date"]] + lubridate::quarter(dataFull[["ID"]]))
    
    XREGS1 <- data.table::copy(dataFull)
    data.table::set(XREGS1, j = "Xtra1", value = jitter(XREGS1[["Weekly_Sales"]], factor = 2500))
    data.table::set(XREGS1, j = "Weekly_Sales", value = NULL)
    if("ID" %chin% names(XREGS1)) data.table::set(XREGS1, j = "ID", value = NULL)
    
    XREGS2 <- data.table::copy(dataFull)
    data.table::set(XREGS2, j = "Xtra1", value = jitter(XREGS2[["Weekly_Sales"]], factor = 2500))
    data.table::set(XREGS2, j = "Xtra2", value = jitter(XREGS2[["Xtra1"]], factor = 3500))
    data.table::set(XREGS2, j = "Weekly_Sales", value = NULL)
    if("ID" %chin% names(XREGS2)) data.table::set(XREGS2, j = "ID", value = NULL)
    
    dataForecastX <- dataFull
    dataFull <- dataFull[, ID := 1:.N, by = list(Store,Dept)][ID < 139][, ID := NULL]
    if("ID" %chin% names(dataForecastX)) data.table::set(dataForecastX, j = "ID", value = NULL) 
  }
  
  # Yearly DATA:----
  if(Groups == 2L & TimeUnit__ == "YEAR") {
    
    # Data
    dataFull <- data[, .SD, .SDcols = c("Store","Dept","Date","Weekly_Sales")]
    dataFull <- dataFull[Store %in% c(1,2)]
    
    # Convert to Yearly
    dataFull[, Date := min(Date)]
    dataFull[, ID := 1:.N, by = list(Store,Dept)]
    data.table::set(dataFull, j = "Date", value = lubridate::as_date(dataFull[["Date"]]))
    data.table::set(dataFull, j = "Date", value = dataFull[["Date"]] + lubridate::years(dataFull[["ID"]]))
    
    XREGS1 <- data.table::copy(dataFull)
    data.table::set(XREGS1, j = "Xtra1", value = jitter(XREGS1[["Weekly_Sales"]], factor = 2500))
    data.table::set(XREGS1, j = "Weekly_Sales", value = NULL)
    if("ID" %chin% names(XREGS1)) data.table::set(XREGS1, j = "ID", value = NULL)
    
    XREGS2 <- data.table::copy(dataFull)
    data.table::set(XREGS2, j = "Xtra1", value = jitter(XREGS2[["Weekly_Sales"]], factor = 2500))
    data.table::set(XREGS2, j = "Xtra2", value = jitter(XREGS2[["Xtra1"]], factor = 3500))
    data.table::set(XREGS2, j = "Weekly_Sales", value = NULL)
    if("ID" %chin% names(XREGS2)) data.table::set(XREGS2, j = "ID", value = NULL)
    
    dataForecastX <- dataFull
    dataFull <- dataFull[, ID := 1:.N, by = list(Store,Dept)][ID < 139][, ID := NULL]
    if("ID" %chin% names(dataForecastX)) data.table::set(dataForecastX, j = "ID", value = NULL)  
  }
  
  # Daily DATA:----
  if(Groups == 2L & TimeUnit__ %chin% c("DAYS","DAY")) {
    
    # Data
    dataFull <- data[, .SD, .SDcols = c("Store","Dept","Date","Weekly_Sales")]
    dataFull <- dataFull[Store %in% c(1,2)]
    
    # Convert to daily
    dataFull[, Date := min(Date)]
    dataFull[, ID := 1:.N, by = list(Store,Dept)]
    data.table::set(dataFull, j = "Date", value = as.Date(dataFull[["Date"]]))
    data.table::set(dataFull, j = "Date", value = dataFull[["Date"]] + lubridate::days(dataFull[["ID"]]))
    
    XREGS1 <- data.table::copy(dataFull)
    data.table::set(XREGS1, j = "Xtra1", value = jitter(XREGS1[["Weekly_Sales"]], factor = 2500))
    data.table::set(XREGS1, j = "Weekly_Sales", value = NULL)
    if("ID" %chin% names(XREGS1)) data.table::set(XREGS1, j = "ID", value = NULL)
    
    XREGS2 <- data.table::copy(dataFull)
    data.table::set(XREGS2, j = "Xtra1", value = jitter(XREGS2[["Weekly_Sales"]], factor = 2500))
    data.table::set(XREGS2, j = "Xtra2", value = jitter(XREGS2[["Xtra1"]], factor = 3500))
    if("ID" %chin% names(XREGS2)) data.table::set(XREGS2, j = "Weekly_Sales", value = NULL)
    
    dataForecastX <- dataFull
    dataFull <- dataFull[, ID := 1:.N, by = list(Store,Dept)][ID < 139][, ID := NULL]
    if("ID" %chin% names(dataForecastX)) data.table::set(dataForecastX, j = "ID", value = NULL)
  }
  
  # Hourly DATA:----
  if(Groups == 2L & TimeUnit__ %chin% c("HOURS","HOUR")) {
    
    # Data
    dataFull <- data[, .SD, .SDcols = c("Store","Dept","Date","Weekly_Sales")]
    dataFull <- dataFull[Store %in% c(1,2)]
    
    # Convert to hourly
    dataFull[, Date := min(Date)]
    dataFull[, ID := 1:.N, by = list(Store,Dept)]
    data.table::set(dataFull, j = "Date", value = as.POSIXct(dataFull[["Date"]]))
    data.table::set(dataFull, j = "Date", value = dataFull[["Date"]] + lubridate::hours(dataFull[["ID"]]))
    
    XREGS1 <- data.table::copy(dataFull)
    data.table::set(XREGS1, j = "Xtra1", value = jitter(XREGS1[["Weekly_Sales"]], factor = 2500))
    data.table::set(XREGS1, j = "Weekly_Sales", value = NULL)
    if("ID" %chin% names(XREGS1)) data.table::set(XREGS1, j = "ID", value = NULL)
    
    XREGS2 <- data.table::copy(dataFull)
    data.table::set(XREGS2, j = "Xtra1", value = jitter(XREGS2[["Weekly_Sales"]], factor = 2500))
    data.table::set(XREGS2, j = "Xtra2", value = jitter(XREGS2[["Xtra1"]], factor = 3500))
    data.table::set(XREGS2, j = "Weekly_Sales", value = NULL)
    if("ID" %chin% names(XREGS2)) data.table::set(XREGS2, j = "ID", value = NULL)
    
    dataForecastX <- dataFull
    dataFull <- dataFull[, ID := 1:.N, by = list(Store,Dept)][ID < 139][, ID := NULL]
    if("ID" %chin% names(dataForecastX)) data.table::set(dataForecastX, j = "ID", value = NULL)
  }
  
  # 30 min DATA:----
  if(Groups == 2L & TimeUnit__ == "30MIN") {
    
    # Data
    dataFull <- data[, .SD, .SDcols = c("Store","Dept","Date","Weekly_Sales")]
    dataFull <- dataFull[Store %in% c(1,2)]
    
    # Convert to 30 mins
    dataFull[, Date := min(Date)]
    dataFull[, ID := 1:.N, by = list(Store,Dept)]
    data.table::set(dataFull, j = "ID", value = dataFull[["ID"]] * 30)
    data.table::set(dataFull, j = "Date", value = as.POSIXct(dataFull[["Date"]]))
    data.table::set(dataFull, j = "Date", value = dataFull[["Date"]] + lubridate::minutes(dataFull[["ID"]]))
    
    XREGS1 <- data.table::copy(dataFull)
    data.table::set(XREGS1, j = "Xtra1", value = jitter(XREGS1[["Weekly_Sales"]], factor = 2500))
    data.table::set(XREGS1, j = "Weekly_Sales", value = NULL)
    if("ID" %chin% names(XREGS1)) data.table::set(XREGS1, j = "ID", value = NULL)
    
    XREGS2 <- data.table::copy(dataFull)
    data.table::set(XREGS2, j = "Xtra1", value = jitter(XREGS2[["Weekly_Sales"]], factor = 2500))
    data.table::set(XREGS2, j = "Xtra2", value = jitter(XREGS2[["Xtra1"]], factor = 3500))
    data.table::set(XREGS2, j = "Weekly_Sales", value = NULL)
    if("ID" %chin% names(XREGS2)) data.table::set(XREGS2, j = "ID", value = NULL)
    
    dataForecastX <- dataFull
    dataFull <- dataFull[, ID := 1:.N, by = list(Store,Dept)][ID < 139][, ID := NULL]
    if("ID" %chin% names(dataForecastX)) data.table::set(dataForecastX, j = "ID", value = NULL)
  }
  
  #----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@----
  # 2 group time series data----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@----
  
  # Weekly DATA:----
  if(Groups == 3L & TimeUnit__ %chin% c("WEEKS","WEEK")) {
    
    # Data
    dataFull <- data[, .SD, .SDcols = c("Store","Dept","Date","Weekly_Sales")]
    dataFull <- dataFull[Store %in% c(1,2,3,4)]
    data.table::set(dataFull, j = "ThirdGroup", value = data.table::fifelse(dataFull[["Store"]] %in% c(1,2), "A","B"))
    data.table::setcolorder(dataFull, neworder = c(5,1,2,3,4))
    
    XREGS1 <- data.table::copy(dataFull)
    data.table::set(XREGS1, j = "Xtra1", value = jitter(XREGS1[["Weekly_Sales"]], factor = 2500))
    data.table::set(XREGS1, j = "Weekly_Sales", value = NULL)
    if("ID" %chin% names(XREGS1)) data.table::set(XREGS1, j = "ID", value = NULL)
    
    XREGS2 <- data.table::copy(dataFull)
    data.table::set(XREGS2, j = "Xtra1", value = jitter(XREGS2[["Weekly_Sales"]], factor = 2500))
    data.table::set(XREGS2, j = "Xtra2", value = jitter(XREGS2[["Xtra1"]], factor = 3500))
    data.table::set(XREGS2, j = "Weekly_Sales", value = NULL)
    if("ID" %chin% names(XREGS2)) data.table::set(XREGS2, j = "ID", value = NULL)
    
    dataForecastX <- dataFull
    dataFull <- dataFull[, ID := 1:.N, by = list(Store,Dept)][ID < 139][, ID := NULL]
    if("ID" %chin% names(dataForecastX)) data.table::set(dataForecastX, j = "ID", value = NULL) 
  }
  
  # Monthly DATA:----
  if(Groups == 2L & TimeUnit__ %chin% c("MONTHS","MONTH")) {
    # Data
    dataFull <- data[, .SD, .SDcols = c("Store","Dept","Date","Weekly_Sales")]
    dataFull <- dataFull[Store %in% c(1,2)]
    
    # Convert to monthly
    dataFull[, Date := min(Date)]
    dataFull[, ID := 1:.N, by = list(Store,Dept)]
    data.table::set(dataFull, j = "Date", value = as.Date(dataFull[["Date"]]))
    data.table::set(dataFull, j = "Date", value = dataFull[["Date"]] %m+% months(dataFull[["ID"]]))
    
    XREGS1 <- data.table::copy(dataFull)
    data.table::set(XREGS1, j = "Xtra1", value = jitter(XREGS1[["Weekly_Sales"]], factor = 2500))
    data.table::set(XREGS1, j = "Weekly_Sales", value = NULL)
    if("ID" %chin% names(XREGS1)) data.table::set(XREGS1, j = "ID", value = NULL)
    
    XREGS2 <- data.table::copy(dataFull)
    data.table::set(XREGS2, j = "Xtra1", value = jitter(XREGS2[["Weekly_Sales"]], factor = 2500))
    data.table::set(XREGS2, j = "Xtra2", value = jitter(XREGS2[["Xtra1"]], factor = 3500))
    data.table::set(XREGS2, j = "Weekly_Sales", value = NULL)
    if("ID" %chin% names(XREGS2)) data.table::set(XREGS2, j = "ID", value = NULL)
    
    dataForecastX <- dataFull
    dataFull <- dataFull[, ID := 1:.N, by = list(Store,Dept)][ID < 139][, ID := NULL]
    if("ID" %chin% names(dataForecastX)) data.table::set(dataForecastX, j = "ID", value = NULL)
  }
  
  # Quarterly DATA:----
  if(Groups == 2L & TimeUnit__ %chin% c("QUARTER","QUARTERS")) {
    
    # Data
    dataFull <- data[, .SD, .SDcols = c("Store","Dept","Date","Weekly_Sales")]
    dataFull <- dataFull[Store %in% c(1,2)]
    
    # Convert to quarterly
    dataFull[, Date := min(Date)]
    dataFull[, ID := 1:.N, by = list(Store,Dept)]
    data.table::set(dataFull, j = "Date", value = as.Date(dataFull[["Date"]]))
    data.table::set(dataFull, j = "Date", value = dataFull[["Date"]] + lubridate::quarter(dataFull[["ID"]]))
    
    XREGS1 <- data.table::copy(dataFull)
    data.table::set(XREGS1, j = "Xtra1", value = jitter(XREGS1[["Weekly_Sales"]], factor = 2500))
    data.table::set(XREGS1, j = "Weekly_Sales", value = NULL)
    if("ID" %chin% names(XREGS1)) data.table::set(XREGS1, j = "ID", value = NULL)
    
    XREGS2 <- data.table::copy(dataFull)
    data.table::set(XREGS2, j = "Xtra1", value = jitter(XREGS2[["Weekly_Sales"]], factor = 2500))
    data.table::set(XREGS2, j = "Xtra2", value = jitter(XREGS2[["Xtra1"]], factor = 3500))
    data.table::set(XREGS2, j = "Weekly_Sales", value = NULL)
    if("ID" %chin% names(XREGS2)) data.table::set(XREGS2, j = "ID", value = NULL)
    
    dataForecastX <- dataFull
    dataFull <- dataFull[, ID := 1:.N, by = list(Store,Dept)][ID < 139][, ID := NULL]
    if("ID" %chin% names(dataForecastX)) data.table::set(dataForecastX, j = "ID", value = NULL) 
  }
  
  # Yearly DATA:----
  if(Groups == 2L & TimeUnit__ == "YEAR") {
    
    # Data
    dataFull <- data[, .SD, .SDcols = c("Store","Dept","Date","Weekly_Sales")]
    dataFull <- dataFull[Store %in% c(1,2)]
    
    # Convert to Yearly
    dataFull[, Date := min(Date)]
    dataFull[, ID := 1:.N, by = list(Store,Dept)]
    data.table::set(dataFull, j = "Date", value = lubridate::as_date(dataFull[["Date"]]))
    data.table::set(dataFull, j = "Date", value = dataFull[["Date"]] + lubridate::years(dataFull[["ID"]]))
    
    XREGS1 <- data.table::copy(dataFull)
    data.table::set(XREGS1, j = "Xtra1", value = jitter(XREGS1[["Weekly_Sales"]], factor = 2500))
    data.table::set(XREGS1, j = "Weekly_Sales", value = NULL)
    if("ID" %chin% names(XREGS1)) data.table::set(XREGS1, j = "ID", value = NULL)
    
    XREGS2 <- data.table::copy(dataFull)
    data.table::set(XREGS2, j = "Xtra1", value = jitter(XREGS2[["Weekly_Sales"]], factor = 2500))
    data.table::set(XREGS2, j = "Xtra2", value = jitter(XREGS2[["Xtra1"]], factor = 3500))
    data.table::set(XREGS2, j = "Weekly_Sales", value = NULL)
    if("ID" %chin% names(XREGS2)) data.table::set(XREGS2, j = "ID", value = NULL)
    
    dataForecastX <- dataFull
    dataFull <- dataFull[, ID := 1:.N, by = list(Store,Dept)][ID < 139][, ID := NULL]
    if("ID" %chin% names(dataForecastX)) data.table::set(dataForecastX, j = "ID", value = NULL)  
  }
  
  # Daily DATA:----
  if(Groups == 2L & TimeUnit__ %chin% c("DAYS","DAY")) {
    
    # Data
    dataFull <- data[, .SD, .SDcols = c("Store","Dept","Date","Weekly_Sales")]
    dataFull <- dataFull[Store %in% c(1,2)]
    
    # Convert to daily
    dataFull[, Date := min(Date)]
    dataFull[, ID := 1:.N, by = list(Store,Dept)]
    data.table::set(dataFull, j = "Date", value = as.Date(dataFull[["Date"]]))
    data.table::set(dataFull, j = "Date", value = dataFull[["Date"]] + lubridate::days(dataFull[["ID"]]))
    
    XREGS1 <- data.table::copy(dataFull)
    data.table::set(XREGS1, j = "Xtra1", value = jitter(XREGS1[["Weekly_Sales"]], factor = 2500))
    data.table::set(XREGS1, j = "Weekly_Sales", value = NULL)
    if("ID" %chin% names(XREGS1)) data.table::set(XREGS1, j = "ID", value = NULL)
    
    XREGS2 <- data.table::copy(dataFull)
    data.table::set(XREGS2, j = "Xtra1", value = jitter(XREGS2[["Weekly_Sales"]], factor = 2500))
    data.table::set(XREGS2, j = "Xtra2", value = jitter(XREGS2[["Xtra1"]], factor = 3500))
    if("ID" %chin% names(XREGS2)) data.table::set(XREGS2, j = "Weekly_Sales", value = NULL)
    
    dataForecastX <- dataFull
    dataFull <- dataFull[, ID := 1:.N, by = list(Store,Dept)][ID < 139][, ID := NULL]
    if("ID" %chin% names(dataForecastX)) data.table::set(dataForecastX, j = "ID", value = NULL)
  }
  
  # Hourly DATA:----
  if(Groups == 2L & TimeUnit__ %chin% c("HOURS","HOUR")) {
    
    # Data
    dataFull <- data[, .SD, .SDcols = c("Store","Dept","Date","Weekly_Sales")]
    dataFull <- dataFull[Store %in% c(1,2)]
    
    # Convert to hourly
    dataFull[, Date := min(Date)]
    dataFull[, ID := 1:.N, by = list(Store,Dept)]
    data.table::set(dataFull, j = "Date", value = as.POSIXct(dataFull[["Date"]]))
    data.table::set(dataFull, j = "Date", value = dataFull[["Date"]] + lubridate::hours(dataFull[["ID"]]))
    
    XREGS1 <- data.table::copy(dataFull)
    data.table::set(XREGS1, j = "Xtra1", value = jitter(XREGS1[["Weekly_Sales"]], factor = 2500))
    data.table::set(XREGS1, j = "Weekly_Sales", value = NULL)
    if("ID" %chin% names(XREGS1)) data.table::set(XREGS1, j = "ID", value = NULL)
    
    XREGS2 <- data.table::copy(dataFull)
    data.table::set(XREGS2, j = "Xtra1", value = jitter(XREGS2[["Weekly_Sales"]], factor = 2500))
    data.table::set(XREGS2, j = "Xtra2", value = jitter(XREGS2[["Xtra1"]], factor = 3500))
    data.table::set(XREGS2, j = "Weekly_Sales", value = NULL)
    if("ID" %chin% names(XREGS2)) data.table::set(XREGS2, j = "ID", value = NULL)
    
    dataForecastX <- dataFull
    dataFull <- dataFull[, ID := 1:.N, by = list(Store,Dept)][ID < 139][, ID := NULL]
    if("ID" %chin% names(dataForecastX)) data.table::set(dataForecastX, j = "ID", value = NULL)
  }
  
  # 30 min DATA:----
  if(Groups == 2L & TimeUnit__ == "30MIN") {
    
    # Data
    dataFull <- data[, .SD, .SDcols = c("Store","Dept","Date","Weekly_Sales")]
    dataFull <- dataFull[Store %in% c(1,2)]
    
    # Convert to 30 mins
    dataFull[, Date := min(Date)]
    dataFull[, ID := 1:.N, by = list(Store,Dept)]
    data.table::set(dataFull, j = "ID", value = dataFull[["ID"]] * 30)
    data.table::set(dataFull, j = "Date", value = as.POSIXct(dataFull[["Date"]]))
    data.table::set(dataFull, j = "Date", value = dataFull[["Date"]] + lubridate::minutes(dataFull[["ID"]]))
    
    XREGS1 <- data.table::copy(dataFull)
    data.table::set(XREGS1, j = "Xtra1", value = jitter(XREGS1[["Weekly_Sales"]], factor = 2500))
    data.table::set(XREGS1, j = "Weekly_Sales", value = NULL)
    if("ID" %chin% names(XREGS1)) data.table::set(XREGS1, j = "ID", value = NULL)
    
    XREGS2 <- data.table::copy(dataFull)
    data.table::set(XREGS2, j = "Xtra1", value = jitter(XREGS2[["Weekly_Sales"]], factor = 2500))
    data.table::set(XREGS2, j = "Xtra2", value = jitter(XREGS2[["Xtra1"]], factor = 3500))
    data.table::set(XREGS2, j = "Weekly_Sales", value = NULL)
    if("ID" %chin% names(XREGS2)) data.table::set(XREGS2, j = "ID", value = NULL)
    
    dataForecastX <- dataFull
    dataFull <- dataFull[, ID := 1:.N, by = list(Store,Dept)][ID < 139][, ID := NULL]
    if("ID" %chin% names(dataForecastX)) data.table::set(dataForecastX, j = "ID", value = NULL)
  }
  
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  if(Groups == 0L) {
    return(list(dataFull      = dataFull, 
                dataForecastX = dataForecastX, 
                XREGS1        = XREGS,
                XREGS2        = NULL))
  } else {
    return(list(dataFull      = dataFull, 
                dataForecastX = dataForecastX, 
                XREGS1        = XREGS1,
                XREGS2        = XREGS2))    
  }
}
