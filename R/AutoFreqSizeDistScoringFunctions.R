#' IntermittentDemandScoringDataGenerator creates the scoring data for forecasting
#'
#' IntermittentDemandScoringDataGenerator creates the scoring data for forecasting. It will recreate the same features used for modeling, take the most recent record, and then duplicate those records for each forecast period specifed.
#' 
#' @author Adrian Antico
#' @family Automated Time Series
#' @param data This is your source data
#' @param FC_Periods The number of periods you set up to forecast
#' @param SaveData Set to TRUE to save the output data to file
#' @param FilePath Set a path file have the data saved there
#' @param TargetVariableName Name or column number of your target variable
#' @param DateVariableName Name or column number of your date variable
#' @param GroupingVariables Name or column number of your group variables
#' @param Lags The number of lags used in building the modeling data sets
#' @param MovingAverages The number of moving averages used in building the modeling data sets
#' @param TimeTrendVariable Set to TRUE if you did so in model data creation
#' @param TimeUnit Set to the same time unit used in modeling data creation
#' @param CurrentDate Set this to the current date or a date that you want. It is user specified in case you want to score historical data.
#' @param CalendarVariables Set this to the same setting you used in modeling data creation
#' @param HolidayGroups Set this to the same setting you used in modeling data creation
#' @examples 
#' \donttest{
#'  ScoringData <- IntermittentDemandScoringDataGenerator(
#'    data = data,
#'    SaveData = FALSE,
#'    FilePath = NULL,
#'    TargetVariableName = "qty",
#'    DateVariableName = "date",
#'    GroupingVariables = "sku",
#'    Lags = 1:7,
#'    MovingAverages = seq(7,28,7),
#'    TimeTrendVariable = TRUE,
#'    TimeUnit = "day",
#'    CurrentDate = NULL,
#'    CalendarVariables = c("wday",
#'                          "mday",
#'                          "yday",
#'                          "week",
#'                          "isoweek",
#'                          "month",
#'                          "quarter",
#'                          "year"),
#'    HolidayGroups = "USPublicHolidays")
#' }
#' @return Returns the most recent records for every level of your grouping variables with all the feature used in model building
#' @export
IntermittentDemandScoringDataGenerator <- function(data = NULL,
                                                   FC_Periods = 52,
                                                   SaveData = FALSE,
                                                   FilePath = NULL,
                                                   TargetVariableName = "qty",
                                                   DateVariableName = "date",
                                                   GroupingVariables = "sku",
                                                   Lags = 1:7,
                                                   MovingAverages = seq(7,28,7),
                                                   TimeTrendVariable = TRUE,
                                                   TimeUnit = "day",
                                                   CurrentDate = NULL,
                                                   CalendarVariables = c("wday",
                                                                         "mday",
                                                                         "yday",
                                                                         "week",
                                                                         "isoweek",
                                                                         "month",
                                                                         "quarter",
                                                                         "year"),
                                                   HolidayGroups = "USPublicHolidays") {
  
  # Copy data----
  datax <- data.table::copy(data)
  
  # Current date calculated like data gen process----
  if(is.null(CurrentDate)) {
    if(tolower(TimeUnit) == "day") {
      CurrentDate <- lubridate::floor_date(x = Sys.Date(), unit = TimeUnit)
    } else if(tolower(TimeUnit) == "week") {
      CurrentDate <- lubridate::floor_date(x = Sys.Date(), unit = "weeks")
    } else if(tolower(TimeUnit) == "month") {
      CurrentDate <- lubridate::floor_date(x = Sys.Date(), unit = "month")
    }
  }
  
  # Ensure is data.table----
  if(!data.table::is.data.table(datax)) {
    datax <- data.table::as.data.table(datax)
  }
  
  # Round up dates----
  datax[, paste0(eval(DateVariableName)) := lubridate::floor_date(
    get(DateVariableName),
    unit = TimeUnit)]
  
  # Group Concatenation----
  if (!is.null(GroupingVariables)) {
    if(length(GroupingVariables) > 1) {
      datax[, GroupVar := do.call(paste, c(.SD, sep = " ")), .SDcols = GroupingVariables]
      datax[, eval(GroupingVariables) := NULL]      
    } else {
      data.table::setnames(datax, eval(GroupingVariables), "GroupVar")
    }
    
    # Modify GroupingVariables argument
    ReverseGroupingVariables <- GroupingVariables
    GroupingVariables <- "GroupVar"
  }
  
  # Ensure data is aggregated to proper time unit----
  datax <- datax[, sum(get(TargetVariableName)), 
                 by = c(eval(GroupingVariables), eval(DateVariableName))]
  data.table::setnames(datax, "V1", eval(TargetVariableName))
  
  # Add Calendar Variables----
  if(!is.null(CalendarVariables)) {
    datax <- CreateCalendarVariables(
      datax, 
      DateCols = DateVariableName,
      AsFactor = FALSE, 
      TimeUnits = CalendarVariables)    
  }
  
  # Add Holiday Variables----
  if(!is.null(HolidayGroups)) {
    datax <- CreateHolidayVariables(
      datax, 
      DateCols = DateVariableName,
      HolidayGroups = HolidayGroups, 
      Holidays = NULL)    
  }
  
  # Add in the time varying features----
  datax <- DT_GDL_Feature_Engineering(
    datax,
    lags           = Lags,
    periods        = MovingAverages,
    statsNames     = c("MA"),
    targets        = TargetVariableName,
    groupingVars   = GroupingVariables,
    sortDateName   = DateVariableName,
    timeDiffTarget = "TimeGap",
    timeAgg        = TimeUnit,
    WindowingLag   = 0,
    Type           = "Lag",
    SimpleImpute   = TRUE)
  
  # Add Time Trend Variable----
  if(!is.null(GroupingVariables)) {
    data.table::setorderv(
      datax, 
      cols = c(eval(GroupingVariables), eval(DateVariableName)), 
      order = c(1,-1))
    datax[, TimeTrend := 1:.N, by = eval(GroupingVariables)]
  }
  
  # Add in the time since last demand instance from RandomStartDate----
  datax <- datax[order(-get(DateVariableName))][
    , TimeSinceLastDemand := as.numeric(difftime(CurrentDate, get(DateVariableName), units = TimeUnit))]
  
  # Subset data----
  datax <- datax[TimeTrend == 1]
  
  # Add FC_Window----
  temp <- data.table::CJ(GroupVar = as.character(datax[["GroupVar"]]), FC_Window = seq_len(FC_Periods))
  datax <- merge(datax, temp, by = "GroupVar", all = FALSE)
  
  # Save data----
  if(SaveData) {
    data.table::fwrite(datax, file = file.path(FilePath,"ScoringData.csv"))
  }
  
  # Back-transform GroupingVariables----
  if(length(ReverseGroupingVariables) > 1) {
    datax[, eval(ReverseGroupingVariables) := data.table::tstrsplit(GroupVar, " ")][
      , GroupVar := NULL]
  } else {
    data.table::setnames(datax, eval(GroupingVariables), eval(ReverseGroupingVariables))
  }
  
  # Return datax----
  return(datax)
}

#' AutoCatBoostFreqSizeScoring is for scoring the models build with AutoCatBoostSizeFreqDist()
#' 
#' AutoCatBoostFreqSizeScoring is for scoring the models build with AutoCatBoostSizeFreqDist(). It will return the predicted values for every quantile model for both distributions for 1 to the max forecast periods you provided to build the scoring data.
#' 
#' @author Adrian Antico
#' @family Automated Time Series
#' @param ScoringData The scoring data returned from IntermittentDemandScoringDataGenerator()
#' @param TargetColumnNames A character or numeric vector of the target names. E.g. c("Counts","TARGET_qty")
#' @param FeatureColumnNames A character vector of column names or column numbers
#' @param IDcols ID columns you want returned with the data that is not a model feature
#' @param CountQuantiles A numerical vector of the quantiles used in model building
#' @param SizeQuantiles A numerical vector of the quantiles used in model building
#' @param ModelPath The path file to where you models were saved
#' @param ModelIDs The ID's used in model building
#' @param KeepFeatures Set to TRUE to return the features with the predicted values
#' @examples 
#' \donttest{
#' FinalData <- AutoCatBoostFreqSizeScoring(
#'   ScoringData,
#'   TargetColumnNames = c("Counts","TARGET_qty"),
#'   FeatureColumnNames = 1:ncol(ScoringData),
#'   IDcols = NULL,
#'   CountQuantiles = seq(0.10,0.90,0.10), 
#'   SizeQuantiles = seq(0.10,0.90,0.10),
#'   ModelPath = getwd(),
#'   ModelIDs = c("CountModel","SizeModel"),
#'   KeepFeatures = TRUE)
#' }
#' @return Returns a list of CountData scores, SizeData scores, along with count and size prediction column names
#' @export
AutoCatBoostFreqSizeScoring <- function(ScoringData,
                                        TargetColumnNames = NULL,
                                        FeatureColumnNames = NULL,
                                        IDcols = NULL,
                                        CountQuantiles = seq(0.10,0.90,0.10), 
                                        SizeQuantiles = seq(0.10,0.90,0.10),
                                        ModelPath = NULL,
                                        ModelIDs = c("CountModel","SizeModel"),
                                        KeepFeatures = TRUE) {
  
  # Ensure data.table----
  if(!data.table::is.data.table(ScoringData)) {
    ScoringData <- data.table::as.data.table(ScoringData)
  }
  
  # Score count models----
  Counter <- 1
  for(Count in CountQuantiles) {
    data <- AutoCatBoostScoring(
      TargetType = "regression",
      ScoringData = ScoringData,
      FeatureColumnNames = FeatureColumnNames,
      IDcols = IDcols,
      ModelObject = NULL,
      ModelPath = ModelPath,
      ModelID = paste0(ModelIDs[1],"_",Count),
      ReturnFeatures = TRUE,
      MultiClassTargetLevels = NULL,
      TransformNumeric = FALSE,
      BackTransNumeric = TRUE,
      TargetColumnName = TargetColumnNames[1],
      TransformationObject = NULL,
      TransID = paste0(ModelIDs[1],"_",Count),
      TransPath = ModelPath,
      MDP_Impute = TRUE,
      MDP_CharToFactor = TRUE,
      MDP_RemoveDates = TRUE,
      MDP_MissFactor = "0",
      MDP_MissNum = -1
    )
    
    # Rearrange Column Ordering, change names, cbind----
    data.table::setcolorder(data, c(2:ncol(data),1))
    if(Count == min(CountQuantiles)) {
      data.table::setnames(data, "Predictions", paste0(ModelIDs[1],"_",Count))
      CountData <- data      
    } else {
      CountData <- cbind(CountData, data[[paste0("Predictions")]])
      data.table::setnames(CountData, "V2", paste0(ModelIDs[1],"_",Count))
    }
    
    # Update timer----
    print(paste0("Count model scoring is ",100*round(Counter/length(CountQuantiles),2),"% complete"))
    Counter <- Counter + 1
  }
  
  # Score size models----
  Counter <- 1
  for(Size in SizeQuantiles) {
    data <- AutoCatBoostScoring(
      TargetType = "regression",
      ScoringData = ScoringData,
      FeatureColumnNames = FeatureColumnNames,
      IDcols = IDcols,
      ModelObject = NULL,
      ModelPath = ModelPath,
      ModelID = paste0(ModelIDs[2],"_",Size),
      ReturnFeatures = TRUE,
      MultiClassTargetLevels = NULL,
      TransformNumeric = FALSE,
      BackTransNumeric = TRUE,
      TargetColumnName = TargetColumnNames[2],
      TransformationObject = NULL,
      TransID = paste0(ModelIDs[2],"_",Size),
      TransPath = ModelPath,
      MDP_Impute = TRUE,
      MDP_CharToFactor = TRUE,
      MDP_RemoveDates = TRUE,
      MDP_MissFactor = "0",
      MDP_MissNum = -1
    )
    
    # Rearrange Column Ordering, change names, cbind----
    data.table::setcolorder(data, c(2:ncol(data),1))
    if(Size == min(SizeQuantiles)) {
      data.table::setnames(data, "Predictions", paste0(ModelIDs[2],"_",Size))
      SizeData <- data      
    } else {
      SizeData <- cbind(SizeData, data[[paste0("Predictions")]])
      data.table::setnames(SizeData, "V2", paste0(ModelIDs[2],"_",Size))
    }
    
    # Update timer----
    print(paste0("Size model scoring is ",100*round(Counter/length(CountQuantiles),2),"% complete"))
    Counter <- Counter + 1
  }
  
  # Column names of predictions----
  CountPredNames <- c("FC_Window", names(CountData)[which(grepl(pattern = paste0(ModelIDs[1],"_"), x = names(CountData)))])
  SizePredNames <- c("FC_Window", names(SizeData)[which(grepl(pattern = paste0(ModelIDs[2],"_"), x = names(SizeData)))])
  
  # Return FinalData----
  return(list(CountData = CountData,
              SizeData = SizeData,
              CountPredNames = CountPredNames,
              SizePredNames = SizePredNames))
}

#' AutoH2oGBMFreqSizeScoring is for scoring the models build with AutoCatBoostSizeFreqDist()
#' 
#' AutoH2oGBMFreqSizeScoring is for scoring the models build with AutoCatBoostSizeFreqDist(). It will return the predicted values for every quantile model for both distributions for 1 to the max forecast periods you provided to build the scoring data.
#' 
#' @author Adrian Antico
#' @family Automated Time Series
#' @param ScoringData The scoring data returned from IntermittentDemandScoringDataGenerator()
#' @param TargetColumnNames A character or numeric vector of the target names. E.g. c("Counts","TARGET_qty")
#' @param CountQuantiles A numerical vector of the quantiles used in model building
#' @param SizeQuantiles A numerical vector of the quantiles used in model building
#' @param ModelPath The path file to where you models were saved
#' @param ModelIDs The ID's used in model building
#' @param KeepFeatures Set to TRUE to return the features with the predicted values
#' @param JavaOptions For mojo scoring '-Xmx1g -XX:ReservedCodeCacheSize=256m',
#' @examples 
#' \donttest{
#' FinalData <- AutoH2oGBMFreqSizeScoring(
#'   ScoringData,
#'   TargetColumnNames = c("Counts","TARGET_qty"),
#'   CountQuantiles = seq(0.10,0.90,0.10), 
#'   SizeQuantiles = seq(0.10,0.90,0.10),
#'   ModelPath = getwd(),
#'   ModelIDs = c("CountModel","SizeModel"),
#'   JavaOptions = '-Xmx1g -XX:ReservedCodeCacheSize=256m',
#'   KeepFeatures = TRUE)
#' }
#' @return Returns a list of CountData scores, SizeData scores, along with count and size prediction column names
#' @export
AutoH2oGBMFreqSizeScoring <- function(ScoringData,
                                      TargetColumnNames = NULL,
                                      CountQuantiles = seq(0.10,0.90,0.10), 
                                      SizeQuantiles = seq(0.10,0.90,0.10),
                                      ModelPath = NULL,
                                      ModelIDs = c("CountModel","SizeModel"),
                                      JavaOptions = '-Xmx1g -XX:ReservedCodeCacheSize=256m',
                                      KeepFeatures = TRUE) {
  
  # Ensure data.table----
  if(!data.table::is.data.table(ScoringData)) {
    ScoringData <- data.table::as.data.table(ScoringData)
  }
  
  # Score count models----
  Counter <- 1
  for(Count in CountQuantiles) {
    data <- AutoH2OMLScoring(
      ScoringData = ScoringData,
      ModelObject = NULL,
      ModelType = "mojo",
      H2OShutdown = FALSE,
      MaxMem = "28G",
      JavaOptions = '-Xmx1g -XX:ReservedCodeCacheSize=256m',
      ModelPath = ModelPath,
      ModelID = paste0(ModelIDs[1],"_",Count),
      ReturnFeatures = TRUE,
      TransformNumeric = FALSE,
      BackTransNumeric = TRUE,
      TargetColumnName = TargetColumnNames[1],
      TransformationObject = NULL,
      TransID = NULL,
      TransPath = paste0(ModelIDs[1],"_",Count),
      MDP_Impute = TRUE,
      MDP_CharToFactor = TRUE,
      MDP_RemoveDates = TRUE,
      MDP_MissFactor = "0",
      MDP_MissNum = -1)
    
    # Rearrange Column Ordering, change names, cbind----
    data.table::setcolorder(data, c(2:ncol(data),1))
    if(Count == min(CountQuantiles)) {
      data.table::setnames(data, "Predictions", paste0(ModelIDs[1],"_",Count))
      CountData <- data      
    } else {
      CountData <- cbind(CountData, data[[paste0("Predictions")]])
      data.table::setnames(CountData, "V2", paste0(ModelIDs[1],"_",Count))
    }
    
    # Update timer----
    print(paste0("Count model scoring is ",100*round(Counter/length(CountQuantiles),2),"% complete"))
    Counter <- Counter + 1
  }
  
  # Score size models----
  Counter <- 1
  for(Size in SizeQuantiles) {
    data <- AutoH2OMLScoring(
      ScoringData = ScoringData,
      ModelObject = NULL,
      ModelType = "mojo",
      H2OShutdown = FALSE,
      MaxMem = "28G",
      JavaOptions = '-Xmx1g -XX:ReservedCodeCacheSize=256m',
      ModelPath = ModelPath,
      ModelID = paste0(ModelIDs[2],"_",Size),
      ReturnFeatures = TRUE,
      TransformNumeric = FALSE,
      BackTransNumeric = TRUE,
      TargetColumnName = TargetColumnNames[2],
      TransformationObject = NULL,
      TransID = NULL,
      TransPath = paste0(ModelIDs[2],"_",Size),
      MDP_Impute = TRUE,
      MDP_CharToFactor = TRUE,
      MDP_RemoveDates = TRUE,
      MDP_MissFactor = "0",
      MDP_MissNum = -1)
    
    # Rearrange Column Ordering, change names, cbind----
    data.table::setcolorder(data, c(2:ncol(data),1))
    if(Size == min(SizeQuantiles)) {
      data.table::setnames(data, "Predictions", paste0(ModelIDs[2],"_",Size))
      SizeData <- data      
    } else {
      SizeData <- cbind(SizeData, data[[paste0("Predictions")]])
      data.table::setnames(SizeData, "V2", paste0(ModelIDs[2],"_",Size))
    }
    
    # Update timer----
    print(paste0("Size model scoring is ",100*round(Counter/length(CountQuantiles),2),"% complete"))
    Counter <- Counter + 1
  }
  
  # Column names of predictions----
  CountPredNames <- c("FC_Window", names(CountData)[which(grepl(pattern = paste0(ModelIDs[1],"_"), x = names(CountData)))])
  SizePredNames <- c("FC_Window", names(SizeData)[which(grepl(pattern = paste0(ModelIDs[2],"_"), x = names(SizeData)))])
  
  # Return FinalData----
  return(list(CountData = CountData,
              SizeData = SizeData,
              CountPredNames = CountPredNames,
              SizePredNames = SizePredNames))
}