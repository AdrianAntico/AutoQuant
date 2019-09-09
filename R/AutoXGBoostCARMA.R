#' AutoXGBoostCARMA Automated XGBoost Calendar, Holiday, ARMA, and Trend Variables Forecasting
#'
#' AutoXGBoostCARMA Automated XGBoost Calendar, Holiday, ARMA, and Trend Variables Forecasting. Create hundreds of thousands of time series forecasts using this function.
#'
#' @author Adrian Antico
#' @family Automated Time Series
#' @param data Supply your full series data set here
#' @param TargetColumnName List the column name of your target variables column. E.g. "Target"
#' @param DateColumnName List the column name of your date column. E.g. "DateTime"
#' @param GroupVariables Defaults to NULL. Use NULL when you have a single series. Add in GroupVariables when you have a series for every level of a group or multiple groups.
#' @param FC_Periods Set the number of periods you want to have forecasts for. E.g. 52 for weekly data to forecast a year ahead
#' @param TimeUnit List the time unit your data is aggregated by. E.g. "1min", "5min", "10min", "15min", "30min", "hour", "day", "week", "year"
#' @param TargetTransformation Run AutoTransformationCreate() to find best transformation for the target variable. Tests YeoJohnson, BoxCox, and Asigh (also Asin and Logit for proportion target variables).
#' @param Lags Select the periods for all lag variables you want to create. E.g. c(1:5,52)
#' @param MA_Periods Select the periods for all moving average variables you want to create. E.g. c(1:5,52)
#' @param CalendarVariables Set to TRUE to have calendar variables created. The calendar variables are numeric representations of second, minute, hour, week day, month day, year day, week, isoweek, quarter, and year
#' @param HolidayVariable Set to TRUE to have a holiday counter variable created.
#' @param TimeTrendVariable Set to TRUE to have a time trend variable added to the model. Time trend is numeric variable indicating the numeric value of each record in the time series (by group). Time trend starts at 1 for the earliest point in time and increments by one for each success time point.
#' @param DataTruncate Set to TRUE to remove records with missing values from the lags and moving average features created
#' @param ZeroPadSeries Set to "all", "inner", or NULL. See TimeSeriesFill for explanation
#' @param SplitRatios E.g c(0.7,0.2,0.1) for train, validation, and test sets
#' @param TreeMethod Choose from "hist", "gpu_hist"
#' @param NThreads Set the maximum number of threads you'd like to dedicate to the model run. E.g. 8
#' @param EvalMetric Select from "r2", "RMSE", "MSE", "MAE"
#' @param GridTune Set to TRUE to run a grid tune
#' @param GridEvalMetric This is the metric used to find the threshold 'poisson', 'mae', 'mape', 'mse', 'msle', 'kl', 'cs', 'r2'
#' @param ModelCount Set the number of models to try in the grid tune
#' @param NTrees Select the number of trees you want to have built to train the model
#' @param PartitionType Select "random" for random data partitioning "time" for partitioning by time frames
#' @param Timer = TRUE
#' @examples
#' \donttest{
#' Results <- AutoXGBoostCARMA(data,
#'                             TargetColumnName = "Target",
#'                             DateColumnName = "DateTime",
#'                             GroupVariables = NULL,
#'                             FC_Periods = 30,
#'                             TimeUnit = "week",
#'                             TargetTransformation = FALSE,
#'                             Lags = c(1:5),
#'                             MA_Periods = c(1:5),
#'                             CalendarVariables = FALSE,
#'                             HolidayVariable = TRUE,
#'                             TimeTrendVariable = FALSE,
#'                             DataTruncate = FALSE,
#'                             ZeroPadSeries = "all",
#'                             SplitRatios = c(0.7, 0.2, 0.1),
#'                             TreeMethod = "hist",
#'                             NThreads = max(1, parallel::detectCores()-2),
#'                             EvalMetric = "MAE",
#'                             GridTune = FALSE,
#'                             GridEvalMetric = "mape",
#'                             ModelCount = 1,
#'                             NTrees = 1000,
#'                             PartitionType = "timeseries",
#'                             Timer = TRUE)
#' Results$TimeSeriesPlot
#' Results$Forecast
#' Results$ModelInformation$...
#' }
#' @return Returns a data.table of original series and forecasts, the catboost model objects (everything returned from AutoCatBoostRegression()), a time series forecast plot, and transformation info if you set TargetTransformation to TRUE. The time series forecast plot will plot your single series or aggregate your data to a single series and create a plot from that.
#' @export
AutoXGBoostCARMA <- function(data,
                             TargetColumnName = "Target",
                             DateColumnName = "DateTime",
                             GroupVariables = NULL,
                             FC_Periods = 30,
                             TimeUnit = "week",
                             TargetTransformation = FALSE,
                             Lags = c(1:5),
                             MA_Periods = c(1:5),
                             CalendarVariables = FALSE,
                             HolidayVariable = TRUE,
                             TimeTrendVariable = FALSE,
                             DataTruncate = FALSE,
                             ZeroPadSeries = NULL,
                             SplitRatios = c(0.7, 0.2, 0.1),
                             TreeMethod = "hist",
                             NThreads = max(1, parallel::detectCores()-2),
                             EvalMetric = "MAE",
                             GridTune = FALSE,
                             GridEvalMetric = "mae",
                             ModelCount = 1,
                             NTrees = 1000,
                             PartitionType = "timeseries",
                             Timer = TRUE) {
  
  # Check arguments----
  if (!(tolower(PartitionType) %chin% c("random", "time", "timeseries"))) {
    return("PartitionType needs to be one of 'random', 'time', or 'timeseries'")
  }
  if (tolower(PartitionType) == "timeseries" &
      is.null(GroupVariables)) {
    PartitionType <- "time"
  }
  if(FC_Periods <= 1) {
    FC_Periods <- 2
  } else {
    FC_Periods <- FC_Periods + 1
  }
  
  # Convert to data.table----
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Subset Columns----
  keep1 <- c(DateColumnName, TargetColumnName, GroupVariables)
  data <- data[, ..keep1]
  
  # Group Concatenation----
  if (!is.null(GroupVariables)) {
    data[, GroupVar := do.call(paste, c(.SD, sep = " ")), .SDcols = GroupVariables]
    data[, eval(GroupVariables) := NULL]
  }
  
  # Zero pad missing dates----
  if(!is.null(ZeroPadSeries)) {
    if (!is.null(GroupVariables)) {
      if(tolower(ZeroPadSeries) == "all") {
        data <- TimeSeriesFill(data,
                               DateColumnName = eval(DateColumnName),
                               GroupVariables = "GroupVar",
                               TimeUnit = TimeUnit,
                               FillType = "all")        
      } else {
        data <- TimeSeriesFill(data,
                               DateColumnName = eval(DateColumnName),
                               GroupVariables = "GroupVar",
                               TimeUnit = TimeUnit,
                               FillType = "inner")
      }
    } else {
      if(tolower(ZeroPadSeries) == "all") {
        data <- TimeSeriesFill(data,
                               DateColumnName = eval(DateColumnName),
                               GroupVariables = NULL,
                               TimeUnit = TimeUnit,
                               FillType = "all")        
      }
    }
    
    # Convert TimeUnit back if particular ones----
    if(TimeUnit == "weeks") {
      TimeUnit <- "week"
    } else if(TimeUnit == "secs") {
      TimeUnit <- "second"
    } else if(TimeUnit == "min") {
      TimeUnit <- "minute"
    }
  }
  
  # Get unique set of GroupVar----
  if (!is.null(GroupVariables)) {
    GroupVarVector <- unique(as.character(data[["GroupVar"]]))
  }
  
  # Change column ordering
  if (!is.null(GroupVariables)) {
    data.table::setcolorder(data,
                            c("GroupVar",
                              eval(DateColumnName),
                              eval(TargetColumnName)))
  } else {
    data.table::setcolorder(data,
                            c(eval(DateColumnName),
                              eval(TargetColumnName)))
  }
  
  # Convert to lubridate as_date() or POSIXct----
  if (!(tolower(TimeUnit) %chin% c("1min","5min","10min","15min","30min","hour"))) {
    data[, eval(DateColumnName) := lubridate::as_date(get(DateColumnName))]
  } else {
    data[, eval(DateColumnName) := as.POSIXct(get(DateColumnName))]
  }
  
  # Ensure Target is Numeric----
  data[, eval(TargetColumnName) := as.numeric(get(TargetColumnName))]
  
  # Define NumSets
  NumSets <- length(SplitRatios)
  
  # Set max vals----
  val <- max(Lags, MA_Periods)
  
  # Ensure data is sorted----
  if (!is.null(GroupVariables)) {
    data <- data[order(GroupVar, get(DateColumnName))]
  } else {
    data <- data[order(get(DateColumnName))]
  }
  
  # Create Calendar Variables----
  if (CalendarVariables) {
    data <- CreateCalendarVariables(
      data = data,
      DateCols = eval(DateColumnName),
      AsFactor = FALSE,
      TimeUnits = c(
        "second",
        "minute",
        "hour",
        "wday",
        "mday",
        "yday",
        "week",
        "isoweek",
        "month",
        "quarter",
        "year"
      )
    )
  }
  
  # Create Holiday Variables----
  if (HolidayVariable) {
    data <- CreateHolidayVariables(
      data,
      DateCols = eval(DateColumnName),
      HolidayGroups = c("USPublicHolidays","EasterGroup",
                        "ChristmasGroup","OtherEcclesticalFeasts"),
      Holidays = NULL)
  }
  
  # Target Transformation----
  if (TargetTransformation) {
    TransformResults <- AutoTransformationCreate(
      data,
      ColumnNames = TargetColumnName,
      Methods = c("BoxCox", "Asinh", "Asin", "Logit", "YeoJohnson"),
      Path = NULL,
      TransID = "Trans",
      SaveOutput = FALSE
    )
    data <- TransformResults$Data
    TransformObject <- TransformResults$FinalResults
  }
  
  # GDL Features----
  if (!is.null(GroupVariables)) {
    data <- DT_GDL_Feature_Engineering(
      data,
      lags           = c(Lags),
      periods        = c(MA_Periods),
      statsNames     = c("MA"),
      targets        = eval(TargetColumnName),
      groupingVars   = "GroupVar",
      sortDateName   = eval(DateColumnName),
      timeDiffTarget = NULL,
      timeAgg        = NULL,
      WindowingLag   = 1,
      Type           = "Lag",
      SimpleImpute   = TRUE
    )
  } else {
    data <- DT_GDL_Feature_Engineering(
      data,
      lags           = c(Lags),
      periods        = c(MA_Periods),
      statsNames     = c("MA"),
      targets        = eval(TargetColumnName),
      groupingVars   = NULL,
      sortDateName   = eval(DateColumnName),
      timeDiffTarget = NULL,
      timeAgg        = NULL,
      WindowingLag   = 1,
      Type           = "Lag",
      SimpleImpute   = TRUE
    )
  }
  
  # TimeTrend Variable----
  if (TimeTrendVariable) {
    if (!is.null(GroupVariables)) {
      data[, TimeTrend := 1:.N, by = "GroupVar"]
    } else {
      data[, TimeTrend := 1:.N]
    }
  }
  
  # Prepare data----
  data <- RemixAutoML::ModelDataPrep(
    data,
    Impute = TRUE,
    CharToFactor = FALSE,
    RemoveDates = FALSE,
    MissFactor = "0",
    MissNum    = -1
  )
  
  # Subset Data----
  if (DataTruncate) {
    data <- data[val:.N]
  }
  
  # Partition Data----
  if (tolower(PartitionType) == "timeseries") {
    DataSets <- RemixAutoML::AutoDataPartition(
      data,
      NumDataSets = NumSets,
      Ratios = SplitRatios,
      PartitionType = "timeseries",
      StratifyColumnNames = "GroupVar",
      TimeColumnName = eval(DateColumnName)
    )
  } else if (tolower(PartitionType) == "random") {
    if (!is.null(GroupVariables)) {
      DataSets <- RemixAutoML::AutoDataPartition(
        data,
        NumDataSets = NumSets,
        Ratios = SplitRatios,
        PartitionType = "random",
        StratifyColumnNames = "GroupVar",
        TimeColumnName = eval(DateColumnName)
      )
    } else {
      DataSets <- RemixAutoML::AutoDataPartition(
        data,
        NumDataSets = NumSets,
        Ratios = SplitRatios,
        PartitionType = "random",
        StratifyColumnNames = NULL,
        TimeColumnName = eval(DateColumnName)
      )
    }
  } else {
    DataSets <- RemixAutoML::AutoDataPartition(
      data,
      NumDataSets = NumSets,
      Ratios = SplitRatios,
      PartitionType = "time",
      StratifyColumnNames = NULL,
      TimeColumnName = eval(DateColumnName)
    )
  }
  
  # Remove ID Column----
  if (tolower(PartitionType) == "timeseries") {
    data.table::set(data, j = "ID", value = NULL) 
  }
  
  # Define data sets----
  if (NumSets == 2) {
    train <- DataSets$TrainData
    valid <- DataSets$ValidationData
    test  <- NULL
  } else if (NumSets == 3) {
    train <- DataSets$TrainData
    valid <- DataSets$ValidationData
    test  <- DataSets$TestData
  }
  
  # Pass along base data unperturbed----
  dataFuture <- data.table::copy(data)
  datax <- data
  rm(data)
  
  # IDcols----
  if(!is.null(GroupVariables)) {
    IDcols <- 2
  } else {
    IDcols <- 1
  }
  
  # Build Regression Model----
  TestModel <- RemixAutoML::AutoXGBoostRegression(
    data = train,
    ValidationData = valid,
    TestData = test,
    TargetColumnName = eval(TargetColumnName),
    FeatureColNames = setdiff(names(datax),
                              c(eval(TargetColumnName),
                                eval(DateColumnName))),
    IDcols = IDcols,
    ReturnFactorLevels = TRUE,
    TransformNumericColumns = NULL,
    eval_metric = EvalMetric,
    Trees = NTrees,
    GridTune = GridTune,
    grid_eval_metric = GridEvalMetric,
    TreeMethod = TreeMethod,
    MaxModelsInGrid = ModelCount,
    NThreads = NThreads,
    model_path = getwd(),
    ModelID = "ModelTest",
    NumOfParDepPlots = 1,
    Verbose = 1,
    ReturnModelObjects = TRUE,
    SaveModelObjects = FALSE,
    PassInGrid = NULL)
  
  # Store Model----
  Model <- TestModel$Model
  if(!is.null(GroupVariables)) {
    FactorList <- TestModel$FactorLevelsList    
  } else {
    FactorList <- NULL
  }
  
  # Update ValidationData and Create Metrics Data----
  TestDataEval <- TestModel$ValidationData
  TestDataEval[, Target := NULL]
  TestDataEval[, eval(DateColumnName) := NULL]
  if(TargetTransformation) {
    TransformObject <- data.table::rbindlist(list(
      TransformObject,
      data.table::data.table(
        ColumnName = "Predict",
        MethodName = TransformObject[ColumnName == eval(TargetColumnName), MethodName],
        Lambda = TransformObject[ColumnName == eval(TargetColumnName), Lambda],
        NormalizedStatistics = 0
      )
    ))
    TestDataEval <- AutoTransformationScore(
      ScoringData = TestDataEval,
      FinalResults = TransformObject,
      Type = "Inverse",
      TransID = NULL,
      Path = NULL
    )    
  } else {
    TransformObject <- NULL
  }
  
  MinVal <- TestDataEval[, min(get(TargetColumnName), na.rm = TRUE)]
  if (!is.null(GroupVariables)) {
    Metric <-
      TestDataEval[, .(GroupVar, get(TargetColumnName), Predict)]
    data.table::setnames(Metric, "V2", eval(TargetColumnName))
    MetricCollection <-
      Metric[, GroupVar, by = "GroupVar"][, GroupVar := NULL]
  }
  
  # poisson----
  if (MinVal > 0 &
      min(TestDataEval[["Predict"]], na.rm = TRUE) > 0) {
    if (!is.null(GroupVariables)) {
      TestDataEval[, Metric := Predict - get(TargetColumnName) * log(Predict + 1)]
      MetricCollection <-
        merge(MetricCollection,
              TestDataEval[, .(Poisson_Metric = mean(Metric, na.rm = TRUE)), by = "GroupVar"],
              by = "GroupVar",
              all = FALSE)
    } else {
      TestDataEval[, Metric := Predict - get(TargetColumnName) * log(Predict + 1)]
      Metric <-
        TestDataEval[, .(Poisson_Metric = mean(Metric, na.rm = TRUE))]
    }
  }
  
  # mae----
  if (!is.null(GroupVariables)) {
    TestDataEval[, Metric := abs(get(TargetColumnName) - Predict)]
    MetricCollection <-
      merge(MetricCollection,
            TestDataEval[, .(MAE_Metric = mean(Metric, na.rm = TRUE)), by = "GroupVar"],
            by = "GroupVar",
            all = FALSE)
  } else {
    TestDataEval[, Metric := abs(get(TargetColumnName) - Predict)]
    Metric <- TestDataEval[, mean(Metric, na.rm = TRUE)]
  }
  
  # mape----
  if (!is.null(GroupVariables)) {
    TestDataEval[, Metric := abs((get(TargetColumnName) - Predict) / (get(TargetColumnName) + 1))]
    MetricCollection <-
      merge(MetricCollection,
            TestDataEval[, .(MAPE_Metric = mean(Metric, na.rm = TRUE)), by = "GroupVar"],
            by = "GroupVar",
            all = FALSE)
  } else {
    TestDataEval[, Metric := abs((get(TargetColumnName) - Predict) / (get(TargetColumnName) + 1))]
    Metric <-
      TestDataEval[, .(MAPE_Metric = mean(Metric, na.rm = TRUE))]
  }
  
  # mse----
  if (!is.null(GroupVariables)) {
    TestDataEval[, Metric := (get(TargetColumnName) - Predict) ^ 2]
    MetricCollection <-
      merge(MetricCollection,
            TestDataEval[, .(MSE_Metric = mean(Metric, na.rm = TRUE)), by = "GroupVar"],
            by = "GroupVar",
            all = FALSE)
  } else {
    TestDataEval[, Metric := (get(TargetColumnName) - Predict) ^ 2]
    Metric <-
      TestDataEval[, .(MSE_Metric = mean(Metric, na.rm = TRUE))]
  }
  
  # msle----
  if (MinVal > 0 &
      min(TestDataEval[["Predict"]], na.rm = TRUE) > 0) {
    if (!is.null(GroupVariables)) {
      TestDataEval[, Metric := (log(get(TargetColumnName) + 1) - log(Predict + 1)) ^ 2]
      MetricCollection <-
        merge(MetricCollection,
              TestDataEval[, .(MSLE = mean(Metric, na.rm = TRUE)), by = "GroupVar"],
              by = "GroupVar",
              all = FALSE)
    } else {
      TestDataEval[, Metric := (log(get(TargetColumnName) + 1) - log(Predict + 1)) ^ 2]
      Metric <- TestDataEval[, .(MSLE = mean(Metric, na.rm = TRUE))]
    }
  }
  
  # kl----
  if (MinVal > 0 &
      min(TestDataEval[["Predict"]], na.rm = TRUE) > 0) {
    if (!is.null(GroupVariables)) {
      TestDataEval[, Metric := get(TargetColumnName) * log((get(TargetColumnName) + 1) / (Predict + 1))]
      MetricCollection <-
        merge(MetricCollection,
              TestDataEval[, .(KL_Metric = mean(Metric, na.rm = TRUE)), by = "GroupVar"],
              by = "GroupVar",
              all = FALSE)
    } else {
      TestDataEval[, Metric := get(TargetColumnName) * log((get(TargetColumnName) + 1) / (Predict + 1))]
      Metric <-
        TestDataEval[, .(KL_Metric = mean(Metric, na.rm = TRUE))]
    }
  }
  
  # r2----
  if (!is.null(GroupVariables)) {
    MetricCollection <-
      merge(MetricCollection,
            TestDataEval[, .(R2_Metric = stats::cor(get(TargetColumnName), Predict)), by = "GroupVar"],
            by = "GroupVar",
            all = FALSE)
    MetricCollection[, R2_Metric := R2_Metric ^ 2]
  } else {
    Metric <-
      (TestDataEval[, .(R2_Metric = stats::cor(get(TargetColumnName), Predict))]) ^ 2
  }
  
  # Update GroupVar with Original Columns, reorder columns, add to model objects----
  if (!is.null(GroupVariables)) {
    if(length(GroupVariables) > 1) {
      MetricCollection[, eval(GroupVariables) := data.table::tstrsplit(GroupVar, " ")][, GroupVar := NULL]
      NumGroupVars <- length(GroupVariables)
      data.table::setcolorder(MetricCollection,
                              c((ncol(MetricCollection) - NumGroupVars + 1):ncol(MetricCollection),
                                1:(ncol(MetricCollection) - NumGroupVars)
                              ))      
    }
    TestModel[["EvaluationMetricsByGroup"]] <- MetricCollection
    TestModel$EvaluationMetricsByGroup
  }
  
  # Store Date Info----
  if (!is.null(GroupVariables)) {
    FutureDateData <- unique(dataFuture[, get(DateColumnName)])
    for (i in  seq_len(FC_Periods)) {
      FutureDateData <- c(FutureDateData, max(FutureDateData) + 1)
    }
  } else {
    FutureDateData <- dataFuture[, get(DateColumnName)]
    for (i in  seq_len(FC_Periods)) {
      FutureDateData <- c(FutureDateData, max(FutureDateData) + 1)
    }
  }
  
  # Row Count----
  if (!is.null(GroupVariables)) {
    N <- datax[, .N, by = "GroupVar"][, max(N)]
  } else {
    N <- datax[, .N]
  }
  
  # Begin loop for generating forecasts----
  for (i in seq_len(FC_Periods)) {
    # Row counts----
    if (i != 1) {
      N <- N + 1
    }
    
    # Generate predictions----
    if (i == 1) {
      
      # Score model----
      if(!is.null(GroupVariables)) {
        Preds <- AutoXGBoostScoring(
          TargetType = "regression",
          ScoringData = datax,
          FeatureColumnNames = names(datax)[c(1,4:ncol(datax))],
          OneHot = FALSE,
          IDcols = NULL,
          ModelObject = Model,
          ModelPath = getwd(),
          ModelID = "ModelTest",
          ReturnFeatures = TRUE,
          TransformNumeric = FALSE,
          BackTransNumeric = FALSE,
          TargetColumnName = eval(TargetColumnName),
          TransformationObject = NULL,
          FactorLevelsList = FactorList,
          TransID = NULL,
          TransPath = NULL,
          MDP_Impute = TRUE,
          MDP_CharToFactor = TRUE,
          MDP_RemoveDates = TRUE,
          MDP_MissFactor = "0",
          MDP_MissNum = -1
        )  
      } else {
        Preds <- AutoXGBoostScoring(
          TargetType = "regression",
          ScoringData = datax,
          FeatureColumnNames = names(datax)[c(3:ncol(datax))],
          OneHot = FALSE,
          IDcols = NULL,
          ModelObject = Model,
          ModelPath = getwd(),
          ModelID = "ModelTest",
          ReturnFeatures = TRUE,
          TransformNumeric = FALSE,
          BackTransNumeric = FALSE,
          TargetColumnName = eval(TargetColumnName),
          TransformationObject = NULL,
          FactorLevelsList = FactorList,
          TransID = NULL,
          TransPath = NULL,
          MDP_Impute = TRUE,
          MDP_CharToFactor = TRUE,
          MDP_RemoveDates = TRUE,
          MDP_MissFactor = "0",
          MDP_MissNum = -1
        )
      }
      
      # Update data----
      UpdateData <- cbind(FutureDateData[1:N],
                          datax[, get(TargetColumnName)], Preds[, eval(DateColumnName) := NULL])
      data.table::setnames(UpdateData,
                           c("V1", "V2"),
                           c(eval(DateColumnName),
                             eval(TargetColumnName)))
    } else {
      if (!is.null(GroupVariables)) {
        temp <- data.table::copy(UpdateData[, ID := 1:.N, by = "GroupVar"])
        temp <- temp[ID == N][, ID := NULL]
        Preds <- AutoXGBoostScoring(
          TargetType = "regression",
          ScoringData = temp,
          FeatureColumnNames = c(4:ncol(temp)),
          OneHot = FALSE,
          IDcols = NULL,
          ModelObject = Model,
          ModelPath = getwd(),
          ModelID = "ModelTest",
          ReturnFeatures = FALSE,
          TransformNumeric = FALSE,
          BackTransNumeric = FALSE,
          TargetColumnName = eval(TargetColumnName),
          TransformationObject = NULL,
          FactorLevelsList = FactorList,
          TransID = NULL,
          TransPath = NULL,
          MDP_Impute = TRUE,
          MDP_CharToFactor = TRUE,
          MDP_RemoveDates = TRUE,
          MDP_MissFactor = "0",
          MDP_MissNum = -1
        )
        
        # Update data group case----
        data.table::setnames(Preds, "Predictions", "Preds")
        Preds <- cbind(UpdateData[ID == N], Preds)
        Preds[, eval(TargetColumnName) := Preds]
        Preds[, Predictions := Preds][, Preds := NULL]
        UpdateData <- UpdateData[ID != N]
        UpdateData <- data.table::rbindlist(list(UpdateData, Preds))
        UpdateData[, ID := NULL]
      } else {
        Preds <- AutoXGBoostScoring(
          TargetType = "regression",
          ScoringData = UpdateData[.N, ],
          FeatureColumnNames = setdiff(names(UpdateData),
                                       c("Predictions",
                                         eval(DateColumnName),
                                         eval(TargetColumnName))),
          OneHot = FALSE,
          IDcols = NULL,
          ModelObject = Model,
          ModelPath = getwd(),
          ModelID = "ModelTest",
          ReturnFeatures = FALSE,
          TransformNumeric = FALSE,
          BackTransNumeric = FALSE,
          TargetColumnName = eval(TargetColumnName),
          TransformationObject = NULL,
          FactorLevelsList = NULL,
          TransID = NULL,
          TransPath = NULL,
          MDP_Impute = TRUE,
          MDP_CharToFactor = TRUE,
          MDP_RemoveDates = TRUE,
          MDP_MissFactor = "0",
          MDP_MissNum = -1
        )
        
        # Update data non-group case----
        data.table::set(UpdateData,
                        i = N,
                        j = 2:3,
                        value = Preds[[1]])
      }
    }
    
    # Timer----
    if (Timer) {
      if(i != 1) print(paste("Forecast future step: ", i-1))
    }
    
    # Create single future record----
    d <- max(UpdateData[[eval(DateColumnName)]])
    if (tolower(TimeUnit) == "hour") {
      CalendarFeatures <-
        data.table::as.data.table(d + lubridate::hours(1))
    } else if (tolower(TimeUnit) == "1min") {
      CalendarFeatures <-
        data.table::as.data.table(d + lubridate::minute(1))
    } else if (tolower(TimeUnit) == "5min") {
      CalendarFeatures <-
        data.table::as.data.table(d + lubridate::minute(5))
    } else if (tolower(TimeUnit) == "10min") {
      CalendarFeatures <-
        data.table::as.data.table(d + lubridate::minute(10))
    } else if (tolower(TimeUnit) == "15min") {
      CalendarFeatures <-
        data.table::as.data.table(d + lubridate::minute(15))
    } else if (tolower(TimeUnit) == "30min") {
      CalendarFeatures <-
        data.table::as.data.table(d + lubridate::minute(30))
    } else if (tolower(TimeUnit) == "day") {
      CalendarFeatures <-
        data.table::as.data.table(d + lubridate::days(1))
    } else if (tolower(TimeUnit) == "week") {
      CalendarFeatures <-
        data.table::as.data.table(d + lubridate::weeks(1))
    } else if (tolower(TimeUnit) == "month") {
      CalendarFeatures <- data.table::as.data.table(d %m+% months(1))
    } else if (tolower(TimeUnit) == "quarter") {
      CalendarFeatures <- data.table::as.data.table(d %m+% months(3))
    } else if (tolower(TimeUnit) == "year") {
      CalendarFeatures <-
        data.table::as.data.table(d + lubridate::years(1))
    }
    
    # Prepare for more feature engineering----
    data.table::setnames(CalendarFeatures, "V1", eval(DateColumnName))
    CalendarFeatures[, eval(DateColumnName) := data.table::as.IDate(get(DateColumnName))]
    if (!is.null(GroupVariables)) {
      CalendarFeatures <- cbind(GroupVarVector, CalendarFeatures)
      data.table::setnames(CalendarFeatures, "GroupVarVector", "GroupVar")
    }
    
    # Add calendar variables----
    if (CalendarVariables) {
      CalendarFeatures <- RemixAutoML::CreateCalendarVariables(
        data = CalendarFeatures,
        DateCols = eval(DateColumnName),
        AsFactor = FALSE,
        TimeUnits = c(
          "second",
          "minute",
          "hour",
          "wday",
          "mday",
          "yday",
          "week",
          "isoweek",
          "month",
          "quarter",
          "year"
        )
      )
    }
    
    # Update holiday feature----
    if (HolidayVariable) {
      CalendarFeatures <- CreateHolidayVariables(
        CalendarFeatures,
        DateCols = eval(DateColumnName),
        HolidayGroups = c("USPublicHolidays","EasterGroup",
                          "ChristmasGroup","OtherEcclesticalFeasts"),
        Holidays = NULL) 
    }
    
    # Add TimeTrendVariable----
    if (TimeTrendVariable) {
      CalendarFeatures[, TimeTrend := N + 1]
    }
    
    # Update features for next run----
    if (i != max(FC_Periods)) {
      temp <- cbind(CalendarFeatures, 1)
      if (!(tolower(TimeUnit) %chin% c("1min","5min","10min","15min","30min","hour"))) {
        temp[, eval(DateColumnName) := lubridate::as_date(get(DateColumnName))]
      } else {
        temp[, eval(DateColumnName) := as.POSIXct(get(DateColumnName))]
      }
      data.table::setnames(temp, c("V2"), c(eval(TargetColumnName)))
      UpdateData <-
        data.table::rbindlist(list(UpdateData, temp), fill = TRUE)
      
      # Update Lags and MA's----
      if (!is.null(GroupVariables)) {
        UpdateData <- UpdateData[order(GroupVar, get(DateColumnName))]
        UpdateData[, ID := .N:1, by = "GroupVar"]
        keep <- unique(c(
          eval(DateColumnName),
          eval(TargetColumnName),
          "Predictions",
          "GroupVar",
          "ID",
          names(CalendarFeatures)
        ))
        Temporary <- data.table::copy(UpdateData[, ..keep])
        Temporary <- Partial_DT_GDL_Feature_Engineering(
          data           = Temporary,
          lags           = c(Lags),
          periods        = c(MA_Periods),
          statsNames     = c("MA"),
          targets        = eval(TargetColumnName),
          groupingVars   = "GroupVar",
          sortDateName   = eval(DateColumnName),
          timeDiffTarget = NULL,
          timeAgg        = NULL,
          WindowingLag   = 1,
          Type           = "Lag",
          SimpleImpute   = TRUE,
          AscRowByGroup  = "ID",
          RecordsKeep    = 1,
          AscRowRemove   = FALSE
        )
        
        # Not lining up - Updatedata and Temporary
        UpdateData <-
          data.table::rbindlist(list(UpdateData[ID != 1], Temporary), use.names = TRUE)
      } else {
        UpdateData <- UpdateData[order(get(DateColumnName))]
        UpdateData[, ID := .N:1]
        keep <- unique(c(
          eval(DateColumnName),
          eval(TargetColumnName),
          "Predictions",
          "ID",
          names(CalendarFeatures)
        ))
        Temporary <- data.table::copy(UpdateData[, ..keep])
        Temporary <- Partial_DT_GDL_Feature_Engineering(
          data           = Temporary,
          lags           = c(Lags),
          periods        = c(MA_Periods),
          statsNames     = c("MA"),
          targets        = eval(TargetColumnName),
          groupingVars   = NULL,
          sortDateName   = eval(DateColumnName),
          timeDiffTarget = NULL,
          timeAgg        = NULL,
          WindowingLag   = 1,
          Type           = "Lag",
          SimpleImpute   = TRUE,
          AscRowByGroup  = "ID",
          RecordsKeep    = 1,
          AscRowRemove   = FALSE
        )
        UpdateData <-
          data.table::rbindlist(list(UpdateData[ID != 1], Temporary), use.names = TRUE)
        UpdateData[, ID := NULL]
      }
    }
  }
  
  # BackTransform----
  if (TargetTransformation) {
    data.table::set(TransformObject,
                    i = 2L,
                    j = 1L,
                    value = "Predictions")
    UpdateData <- AutoTransformationScore(
      ScoringData = UpdateData,
      FinalResults = TransformObject,
      Type = "Inverse",
      TransID = NULL,
      Path = NULL
    )
  }
  
  # Metrics----
  EvalMetric <-
    TestModel$EvaluationMetrics[Metric == "MAPE", MetricValue]
  
  # Define plot theme----
  Temp <- function () {
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 11),
      axis.text = ggplot2::element_text(size = 11),
      legend.text = ggplot2::element_text(color = "#1c1c1c",
                                          size = 11),
      legend.background = ggplot2::element_rect(
        fill = "snow3",
        size = 0.25,
        colour = "darkblue"
      ),
      legend.justification = 0,
      legend.position = "bottom",
      plot.background = ggplot2::element_rect(fill = "#E7E7E7"),
      panel.background = ggplot2::element_rect(fill = "#E7E7E7"),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "white"),
      panel.grid.minor.y = ggplot2::element_line(color = "white"),
      plot.title = ggplot2::element_text(
        color = "#1c1c1c",
        size = 25,
        hjust = 0,
        face = "bold"
      ),
      plot.subtitle = ggplot2::element_text(
        color = "#1c1c1c",
        size = 14,
        hjust = 0
      ),
      plot.caption = ggplot2::element_text(
        size = 9,
        hjust = 0,
        face = "italic"
      )
    )
  }
  
  # Data Manipulation----
  if (!is.null(GroupVariables)) {
    PlotData <- data.table::copy(UpdateData)
    PlotData <- PlotData[, .(sum(get(TargetColumnName)),
                             sum(Predictions)),
                         by = eval(DateColumnName)]
    data.table::setnames(PlotData, c("V1", "V2"), c(eval(TargetColumnName), "Predictions"))
  } else {
    PlotData <- data.table::copy(UpdateData)
    data.table::set(
      PlotData,
      i = (datax[, .N] + 1):PlotData[, .N],
      j = 2,
      value = NA
    )
    data.table::set(PlotData,
                    i = 1:datax[, .N],
                    j = 3,
                    value = NA)
  }
  
  # Plot Time Series----
  TimeSeriesPlot <-
    ggplot2::ggplot(PlotData, ggplot2::aes(x = PlotData[[eval(DateColumnName)]])) +
    ggplot2::geom_line(ggplot2::aes(y = PlotData[[eval(TargetColumnName)]],
                                    color = "Actual")) +
    ggplot2::geom_line(ggplot2::aes(y = PlotData[["Predictions"]],
                                    color = "Forecast"))
  
  # Modify title----
  if (!is.null(GroupVariables)) {
    TimeSeriesPlot <- TimeSeriesPlot +
      ggplot2::geom_vline(
        xintercept = UpdateData[datax[, .N, by = "GroupVar"][1, 2][[1]],
                                max(get(DateColumnName), na.rm = TRUE)],
        color = "#FF4F00",
        lty = "dotted",
        lwd = 1
      ) +
      Temp()
    TimeSeriesPlot <- TimeSeriesPlot +
      ggplot2::labs(
        title = paste0(
          FC_Periods-1,
          " - Period Forecast for Aggregate ",
          eval(TargetColumnName)
        ),
        subtitle = paste0(
          "XGBoost Model: Mean Absolute Percentage Error = ",
          paste0(round(EvalMetric, 3) * 100, "%")
        ),
        caption = "Forecast generated by Remix Institute's RemixAutoML R package"
      ) +
      ggplot2::scale_colour_manual(
        "",
        breaks = c("Actual", "Forecast"),
        values = c("Actual" = "red", "Forecast" =
                     "blue")
      ) +
      ggplot2::xlab(eval(DateColumnName)) + ggplot2::ylab(eval(TargetColumnName))
  } else {
    TimeSeriesPlot <- TimeSeriesPlot +
      ggplot2::geom_vline(
        xintercept = UpdateData[datax[, .N][[1]],
                                max(get(DateColumnName), na.rm = TRUE)],
        color = "#FF4F00",
        lty = "dotted",
        lwd = 1
      ) +
      Temp() +
      ggplot2::labs(
        title = paste0(FC_Periods-1, " - Period Forecast for ", eval(TargetColumnName)),
        subtitle = paste0(
          "XGBoost Model: Mean Absolute Percentage Error = ",
          paste0(round(EvalMetric, 3) * 100, "%")
        ),
        caption = "Forecast generated by Remix Institute's RemixAutoML R package"
      ) +
      ggplot2::scale_colour_manual(
        "",
        breaks = c("Actual", "Forecast"),
        values = c("Actual" = "red", "Forecast" = "blue")) +
      ggplot2::xlab(eval(DateColumnName)) + ggplot2::ylab(eval(TargetColumnName))
  }
  
  # Return data----
  if (!is.null(GroupVariables)) {
    # Variables to keep----
    keep <-
      c("GroupVar",
        eval(DateColumnName),
        eval(TargetColumnName),
        "Predictions")
    UpdateData <- UpdateData[, ..keep]
    if(length(GroupVariables) > 1) {
      UpdateData[, eval(GroupVariables) := data.table::tstrsplit(GroupVar, " ")][, GroupVar := NULL]  
    }
    if(TargetTransformation) {
      return(
        list(
          Forecast = UpdateData,
          TimeSeriesPlot = TimeSeriesPlot,
          ModelInformation = TestModel,
          TransformationDetail = TransformObject
        )
      )
    } else {
      return(
        list(
          Forecast = UpdateData,
          TimeSeriesPlot = TimeSeriesPlot,
          ModelInformation = TestModel
        )
      )      
    }
  } else {
    # Variables to keep----
    keep <- c(eval(DateColumnName), "Predictions")
    if(TargetTransformation) {
      return(
        list(
          Forecast = PlotData[, ..keep],
          TimeSeriesPlot = TimeSeriesPlot,
          ModelInformation = TestModel,
          TransformationDetail = TransformObject
        )
      )
    } else {
      return(
        list(
          Forecast = PlotData[, ..keep],
          TimeSeriesPlot = TimeSeriesPlot,
          ModelInformation = TestModel
        )
      )      
    }
  }
}