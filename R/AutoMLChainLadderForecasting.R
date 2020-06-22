#' AutoChainLadderForecast()
#' 
#' AutoChainLadderForecast() for generating forecasts
#' 
#' @author Adrian Antico
#' @family Automated Model Scoring
#' @param data
#' @param FC_BaseFunnelMeasure Dataset containing the forward looking FC_BaseFunnelMeasure
#' @param MaxDateForecasted Supply a value if you want to backtest
#' @param MaxCalendarDate Supply a value if you want to backtest
#' @param ArgsList Argument list returned from AutoCatBoostChainLadder
#' @param MaxCohortPeriods The maximum amount of ArgsList$CohortPeriodsVariable to utilize for forecasting
#' @examples
#' \donttest{
#' AutoChainLadderForecast(
#'    data,
#'    FC_BaseFunnelMeasure,
#'    SegmentName = NULL,
#'    MaxDateForecasted = NULL,
#'    MaxCalendarDate = NULL,
#'    ArgsList = NULL,
#'    MaxCohortPeriods = NULL)
#' }
#' @return Saves metadata and models to files of your choice. Also returns metadata and models from the function. User specifies both options.
#' @export
AutoChainLadderForecast <- function(data,
                                    FC_BaseFunnelMeasure,
                                    SegmentName = NULL,
                                    MaxDateForecasted = NULL,
                                    MaxCalendarDate = NULL,
                                    ArgsList = NULL,
                                    MaxCohortPeriods = NULL) {
  
  # Forecasting start and end periods----
  if(is.null(MaxDateForecasted)) MaxDateForecasted <- data[, max(get(ArgsList$CalendarDate), na.rm = TRUE)]
  if(is.null(MaxCalendarDate)) MaxCalendarDate <- FC_BaseFunnelMeasure[, max(get(ArgsList$CalendarDate), na.rm = TRUE)]
  
  # Loop through all periods to forecast----
  FC_Period <- 0L
  while(MaxDateForecasted < MaxCalendarDate) {
    
    # Increment FC_Period----
    FC_Period <- FC_Period + 1L
    for(bla in seq_len(20L)) print(paste0("Working on Forecast for period: ", FC_Period, " ::: Periods left to forecast: ", difftime(MaxCalendarDate, MaxDateForecasted)))
    
    # DE: Prepare data----
    print("# Prepare data----")
    if(class(data[[eval(ArgsList$CalendarDate)]]) != "Date") data[, eval(ArgsList$CalendarDate) := as.Date(get(ArgsList$CalendarDate))]
    if(class(data[[ArgsList$CohortDate]]) != "Date") data[, eval(ArgsList$CohortDate) := as.Date(get(ArgsList$CohortDate))]
    data[, ScoreRecords := 2]
    if(class(data[[eval(ArgsList$CohortPeriodsVariable)]]) == "factor") data[, eval(ArgsList$CohortPeriodsVariable) := as.numeric(as.character(get(ArgsList$CohortPeriodsVariable)))]
    maxct <- data[, list(max(get(ArgsList$CohortPeriodsVariable)), data.table::first(ScoreRecords)), by = list(get(ArgsList$CalendarDate))]
    data.table::setnames(maxct, c("get","V1","V2"), c(ArgsList$CalendarDate, ArgsList$CohortPeriodsVariable, "ScoreRecords"))
    maxct[, eval(ArgsList$CohortPeriodsVariable) := get(ArgsList$CohortPeriodsVariable) + 1L]
    maxct[, eval(ArgsList$CohortDate) := as.Date(get(ArgsList$CalendarDate)) + lubridate::days(get(ArgsList$CohortPeriodsVariable))]
    maxct[, Segment := eval(ArgsList$ModelID)]
    data.table::setnames(maxct, "Segment", eval(SegmentName))
    
    # DE: Subset data and update data----
    print("# Subset data and update data----")
    FC_BaseFunnelMeasure <- FC_BaseFunnelMeasure[get(ArgsList$CalendarDate) > max(maxct[[eval(ArgsList$CalendarDate)]])]
    NextFCPeriod <- FC_BaseFunnelMeasure[1L]
    NextFCPeriod[, eval(ArgsList$CalendarDate) := as.Date(get(ArgsList$CalendarDate))]
    NextFCPeriod[, eval(ArgsList$CohortDate) := as.Date(get(ArgsList$CohortDate))]
    NextFCPeriod[, ScoreRecords := 1]
    FC_BaseFunnelMeasure <- FC_BaseFunnelMeasure[2L:.N]
    
    # DE: Merge on next date of Inquiries----
    print("# Merge on next date of Inquiries----")
    temp <- data[, list(data.table::first(get(ArgsList$BaseFunnelMeasure))), by = list(get(ArgsList$CalendarDate))]
    data.table::setnames(temp, c("get","V1"), c(ArgsList$CalendarDate, ArgsList$BaseFunnelMeasure))
    temp[, eval(ArgsList$CalendarDate) := as.Date(get(ArgsList$CalendarDate))]
    maxct <- merge(maxct, temp, by = ArgsList$CalendarDate, all.x = TRUE)
    maxct[, eval(ArgsList$ConversionMeasure) := 0]
    maxct[, Rate := 0]
    maxct[, ScoreRecords := 1]
    maxct <- data.table::rbindlist(list(maxct, NextFCPeriod), use.names = TRUE)
    
    # DE: Remove CohortPeriods beyond MaxCohortPeriods----
    maxct <- maxct[get(ArgsList$CohortPeriodsVariable) <= MaxCohortPeriods]
    ScoreDate <- maxct[, max(get(ArgsList$CohortDate))]
    
    # DE: Stack onto modeling data for seg----
    print("# Stack onto modeling data for seg----")
    data <- data.table::rbindlist(list(data, maxct), fill = TRUE, use.names = TRUE)
    rm(maxct)
    
    # FE: Calendar & Holiday Variables----
    print("# Feature Engineering----")
    data <- RemixAutoML::CreateCalendarVariables(data, DateCols = c(ArgsList$CalendarDate, ArgsList$CohortDate), AsFactor = FALSE, TimeUnits = ArgsList$CalendarVariables)
    data <- RemixAutoML::CreateHolidayVariables(data, DateCols = c(ArgsList$CalendarDate), HolidayGroups = ArgsList$HolidayGroups, Holidays = NULL, GroupingVars = eval(SegmentName), Print = FALSE)
    data.table::setnames(data, old = "HolidayCounts", new = paste0(ArgsList$CalendarDate,"DateHolidayCounts"))
    data <- RemixAutoML::CreateHolidayVariables(data, DateCols = c(ArgsList$CohortDate), HolidayGroups = ArgsList$HolidayGroups, Holidays = NULL, GroupingVars = eval(SegmentName), Print = FALSE)
    data.table::setnames(data, old = "HolidayCounts", new = paste0(ArgsList$CohortDate,"HolidayCounts"))
    data.table::setorderv(data, cols = c(ArgsList$CalendarDate,eval(ArgsList$CohortPeriodsVariable)), c(1L, 1L))
    
    # FE: Transfers and Rate AutoLagRollStatsScoring----
    print("# AutoLagRollStatsScoring----")
    temp <- data.table::copy(data)
    data.table::set(temp, j = ArgsList$CalendarDate, value = as.character(temp[[ArgsList$CalendarDate]]))
    temp <- RemixAutoML::AutoLagRollStatsScoring(
      
      # Data
      data                 = temp,
      DateColumn           = ArgsList$CohortDate,
      Targets              = c(ArgsList$ConversionMeasure, "Rate"),
      RowNumsID            = "ScoreRecords",
      RowNumsKeep          = 1,
      HierarchyGroups      = NULL,
      IndependentGroups    = ArgsList$CalendarDate,
      TimeUnit             = ArgsList$TimeUnit,
      TimeGroups           = ArgsList$CohortTimeGroups,
      TimeUnitAgg          = ArgsList$TimeUnit,
      
      # Services
      TimeBetween          = NULL,
      RollOnLag1           = TRUE,
      Type                 = "Lag",
      SimpleImpute         = FALSE,
      
      # Calculated Columns
      Lags                 = ArgsList$CohortLags,
      MA_RollWindows       = ArgsList$CohortMovingAverages,
      SD_RollWindows       = ArgsList$CohortStandardDeviations,
      Skew_RollWindows     = ArgsList$CohortSkews,
      Kurt_RollWindows     = ArgsList$Kurts,
      Quantile_RollWindows = ArgsList$Quantiles,
      Quantiles_Selected   = ArgsList$CohortQuantilesSelected,
      Debug                = TRUE)
    
    # Join datasets
    print("# Combine datasets----")
    temp[, eval(ArgsList$CalendarDate) := as.Date(get(ArgsList$CalendarDate))]
    data <- merge(data, temp[, .SD, .SDcols = c(eval(ArgsList$CalendarDate),eval(ArgsList$CohortDate), setdiff(names(temp), names(data)))], by = c(eval(ArgsList$CalendarDate), eval(ArgsList$CohortDate)), all.x = TRUE)
    rm(temp)
    
    # FE: CohortDateHolidayCounts AutoLagRollStatsScoring----
    print("# TransferHolidayCounts AutoLagRollStatsScoring----")
    temp <- data.table::copy(data)
    data.table::set(temp, j = eval(ArgsList$CalendarDate), value = as.character(temp[[eval(ArgsList$CalendarDate)]]))
    temp <- RemixAutoML::AutoLagRollStatsScoring(
      
      # Data
      data                 = temp,
      DateColumn           = ArgsList$CohortDate,
      Targets              = c(paste0(ArgsList$ConversionMeasure),"HolidayCounts"),
      RowNumsID            = "ScoreRecords",
      RowNumsKeep          = 1,
      HierarchyGroups      = NULL,
      IndependentGroups    = ArgsList$CalendarDate,
      TimeUnit             = ArgsList$TimeUnit,
      TimeGroups           = ArgsList$TimeUnit,
      TimeUnitAgg          = ArgsList$TimeUnit,
      
      # Services
      TimeBetween          = NULL,
      RollOnLag1           = TRUE,
      Type                 = "Lag",
      SimpleImpute         = FALSE,
      
      # Calculated Columns
      Lags                 = ArgsList$CohortHolidayLags,
      MA_RollWindows       = ArgsList$CohortMovingAverages,
      SD_RollWindows       = NULL,
      Skew_RollWindows     = NULL,
      Kurt_RollWindows     = NULL,
      Quantile_RollWindows = NULL,
      Quantiles_Selected   = NULL,
      Debug                = TRUE)
    
    # Join datasets
    print("# Combine datasets----")
    temp[, eval(ArgsList$CalendarDate) := get(as.Date(ArgsList$CalendarDate))]
    data <- merge(data, temp[, .SD, .SDcols = c(eval(ArgsList$CalendarDate), eval(ArgsList$CohortDate), setdiff(names(temp), names(data)))], by = c(eval(ArgsList$CalendarDate), eval(ArgsList$CohortDate)), all.x = TRUE)
    rm(temp)
    
    # FE: Inquiry AutoLagRollStatsScoring----
    print("# AutoLagRollStatsScoring----")
    temp <- data.table::copy(data)
    temp <- temp[, list(data.table::first(get(ArgsList$BaselineFunnelMeasure))), by = list(get(ArgsList$CalendarDate))]
    data.table::setnames(temp, "V1", eval(ArgsList$BaselineFunnelMeasure))
    temp[, ScoreRecords := data.table::fifelse(get(ArgsList$CalendarDate) == ScoreDate, 1, 2)]
    data.table::set(temp, j = eval(ArgsList$CalendarDate), value = as.Date(temp[[eval(ArgsList$CalendarDate)]]))
    temp <- RemixAutoML::AutoLagRollStatsScoring(
      
      # Data
      data                 = temp,
      DateColumn           = ArgsList$CalendarDate,
      Targets              = ArgsList$BaseFunnelMeasure,
      HierarchyGroups      = NULL,
      IndependentGroups    = NULL,
      TimeGroups           = ArgsList$CalendarTimeGroups,
      TimeUnit             = ArgsList$TimeUnit,
      TimeUnitAgg          = ArgsList$TimeUnit,
      
      # Services
      RowNumsID            = "ScoreRecords",
      RowNumsKeep          = 1,
      TimeBetween          = NULL,
      RollOnLag1           = TRUE,
      Type                 = "Lag",
      SimpleImpute         = FALSE,
      
      # Calculated Columns
      Lags                 = ArgsList$CalendarLags,
      MA_RollWindows       = ArgsList$CalendarMovingAverages,
      SD_RollWindows       = ArgsList$CalendarStandardDeviations,
      Skew_RollWindows     = ArgsList$CalendarSkews,
      Kurt_RollWindows     = ArgsList$CalendarKurts,
      Quantile_RollWindows = ArgsList$CalendarQuantiles,
      Quantiles_Selected   = ArgsList$CalendarQuantilesSelected,
      Debug                = TRUE)
    
    # Join datasets
    print("# Combine datasets----")
    temp[, eval(ArgsList$CalendarDate) := get(as.Date(ArgsList$CalendarDate))]
    data <- merge(data, temp[, .SD, .SDcols = c(eval(ArgsList$CalendarDate), setdiff(names(temp), names(data)))], by = eval(ArgsList$CalendarDate), all.x = TRUE)
    rm(temp)
    
    # FE: Total Transfers by CalendarDate AutoLagRollStatsScoring----
    print("# AutoLagRollStatsScoring----")
    temp <- data.table::copy(data)
    temp <- temp[get(ArgsList$CohortDate) == get(ArgsList$CalendarDate), list(sum(ArgsList$ConversionMeasure)), by = list(get(ArgsList$CalendarDate))]
    data.table::setnames(temp, "V1", eval(ArgsList$ConversionMeasure))
    temp[, ScoreRecords := data.table::fifelse(CalendarDate == ScoreDate, 1, 2)]
    data.table::set(temp, j = eval(ArgsList$CalendarDate), value = as.Date(temp[[eval(ArgsList$CalendarDate)]]))
    temp <- RemixAutoML::AutoLagRollStatsScoring(
      
      # Data
      data                 = temp,
      DateColumn           = ArgsList$CalendarDate,
      Targets              = ArgsList$ConversionMeasure,
      HierarchyGroups      = NULL,
      IndependentGroups    = NULL,
      TimeGroups           = ArgsList$CalendarTimeGroups,
      TimeUnit             = ArgsList$TimeUnit,
      TimeUnitAgg          = ArgsList$TimeUnit,
      
      # Services
      RowNumsID            = "ScoreRecords",
      RowNumsKeep          = 1,
      TimeBetween          = NULL,
      RollOnLag1           = TRUE,
      Type                 = "Lag",
      SimpleImpute         = FALSE,
      
      # Calculated Columns
      Lags                 = ArgsList$CalendarLags,
      MA_RollWindows       = ArgsList$CalendarMovingAverages,
      SD_RollWindows       = ArgsList$CalendarStandardDeviations,
      Skew_RollWindows     = ArgsList$CalendarSkews,
      Kurt_RollWindows     = ArgsList$CalendarKurts,
      Quantile_RollWindows = ArgsList$CalendarQuantiles,
      Quantiles_Selected   = ArgsList$CalendarQuantilesSelected,
      Debug                = TRUE)
    
    # Join datasets
    print("# Combine datasets----")
    temp[, eval(ArgsList$CalendarDate) := as.Date(ArgsList$CalendarDate)]
    data <- merge(data, temp[, .SD, .SDcols = c(eval(ArgsList$CalendarDate), setdiff(names(temp),names(data)))], by = eval(ArgsList$CalendarDate), all.x = TRUE)
    rm(temp)
    
    # FE: CalendarDateHolidayCounts AutoLagRollStatsScoring----
    print("# AutoLagRollStatsScoring----")
    temp <- data.table::copy(data)
    temp <- temp[, list(max(get(paste0(ArgsList$CalendarDate,"HolidayCounts")))), by = list(get(ArgsList$CalendarDate))]
    data.table::setnames(temp, "V1", paste0(ArgsList$CalendarDate,"HolidayCounts"))
    temp[, ScoreRecords := data.table::fifelse(get(ArgsList$CalendarDate) == ScoreDate, 1, 2)]
    data.table::set(temp, j = eval(ArgsList$CalendarDate), value = as.Date(temp[[eval(ArgsList$CalendarDate)]]))
    temp <- RemixAutoML::AutoLagRollStatsScoring(
      
      # Data
      data                 = temp,
      DateColumn           = eval(ArgsList$CalendarDate),
      Targets              = c(paste0(ArgsList$CalendarDate, "HolidayCounts")),
      HierarchyGroups      = NULL,
      IndependentGroups    = NULL,
      TimeGroups           = ArgsList$TimeUnit,
      TimeUnitAgg          = ArgsList$TimeUnit,
      TimeUnit             = ArgsList$TimeUnit,
      
      # Services
      RowNumsID            = "ScoreRecords",
      RowNumsKeep          = 1,
      TimeBetween          = NULL,
      RollOnLag1           = TRUE,
      Type                 = "Lag",
      SimpleImpute         = FALSE,
      
      # Calculated Columns
      Lags                 = ArgsList$CalendarHolidayLags,
      MA_RollWindows       = ArgsList$CalendarHolidayMovingAverages,
      SD_RollWindows       = NULL,
      Skew_RollWindows     = NULL,
      Kurt_RollWindows     = NULL,
      Quantile_RollWindows = NULL,
      Quantiles_Selected   = NULL,
      Debug                = TRUE)
    
    # Join datasets
    print("# Combine datasets----")
    temp[, eval(ArgsList$CalendarDate) := as.Date(get(ArgsList$CalendarDate))]
    data <- merge(data, temp[, .SD, .SDcols = c(eval(ArgsList$CalendarDate), setdiff(names(temp),names(data)))], by = c(eval(ArgsList$CalendarDate)), all.x = TRUE)
    rm(temp)
    
    # DE: Model data prep----
    print("# Model data prep----")
    data <- RemixAutoML::ModelDataPrep(
      data         = data,
      Impute       = TRUE,
      CharToFactor = FALSE,
      FactorToChar = FALSE,
      IntToNumeric = FALSE,
      DateToChar   = FALSE,
      RemoveDates  = FALSE,
      MissFactor   = "0",
      MissNum      = ArgsList$ImputeRollStats,
      IgnoreCols   = NULL)
    
    # DE: Type Change: CorhortDaysOut as numeric and the dates as Dates----
    print("# Convert features types to correct ones----")
    if(!all(class(data[[ArgsList$CohortPeriodsVariable]]) %chin% "numeric")) data[, eval(ArgsList$CohortPeriodsVariable) := as.numeric(as.character(get(ArgsList$CohortPeriodsVariable)))]
    if(!all(class(data[[ArgsList$CalendarDate]]) %chin% "Date")) data[, eval(ArgsList$CalendarDate) := as.Date(get(ArgsList$CalendarDate))]
    if(!all(class(data[[ArgsList$CohortDate]]) %chin% "Date")) data[, eval(ArgsList$CohortDate) := as.Date(get(ArgsList$CohortDate))]
    
    # DE: Load model artifacts----
    print("# Load model artifacts----")
    if(FC_Period == 1L) load(file = file.path(normalizePath(eval(ArgsList$ModelPath)), paste0(seg, "_FinalTrain.Rdata")))
    
    # ML: Score Model----
    print("# Score Model----")
    temp1 <- data.table::copy(data)
    temp <- temp1[ScoreRecords == 1]
    Features <- TestModel$ColNames[[1L]]
    temp <- RemixAutoML::AutoCatBoostScoring(
      TargetType = "regression",
      ScoringData = temp,
      FeatureColumnNames = Features,
      IDcols = names(temp)[!names(temp) %chin% Features],
      ModelObject = TestModel$Model,
      ModelPath = eval(ArgsList$ModelPath),
      ModelID = eval(ArgsList$ModelID),
      ReturnFeatures = FALSE,
      MultiClassTargetLevels = NULL,
      TransformNumeric = FALSE,
      BackTransNumeric = FALSE,
      TargetColumnName = "Rate",
      TransformationObject = TestModel$TransformationResults,
      TransID = NULL,
      TransPath = NULL,
      MDP_Impute = TRUE,
      MDP_CharToFactor = TRUE,
      MDP_RemoveDates = TRUE,
      MDP_MissFactor = "0",
      MDP_MissNum = -1,
      RemoveModel = FALSE)
    
    # DE: Update forecast data----
    print("# Update forecast data----")
    temp1[ScoreRecords == 1, Rate := temp[which(Predictions < 0), Predictions := 0][[1L]]]
    temp1[ScoreRecords == 1, eval(ArgsList$ConversionMeasure) := Rate * (get(ArgsList$BaselineFunnelMeasure) + 1)]
    temp1 <- temp1[ScoreRecords == 1, .SD, .SDcols = c(eval(ArgsList$CalendarDate),eval(ArgsList$CohortDate),eval(ArgsList$CohortPeriodsVariable),eval(ArgsList$ModelID),eval(ArgsList$BaselineFunnelMeasure),eval(ArgsList$ConversionMeasure),"Rate")]
    data <- data.table::rbindlist(list(data[ScoreRecords != 1, .SD, .SDcols = c(eval(ArgsList$CalendarDate),eval(ArgsList$CohortDate),eval(ArgsList$CohortPeriodsVariable),eval(ArgsList$ModelID),eval(ArgsList$BaselineFunnelMeasure),eval(ConversionMeasure),"Rate")], temp1), fill = TRUE, use.names = TRUE)
    
    # DE: Save forecasts to file----
    data.table::fwrite(data, file = file.path(normalizePath(ForecastOutput), paste0(ArgsList$ModelID, "_Forecasts.csv")))
    
    # DE: Update MaxDateForecasted to know when to stop----
    MaxDateForecasted <- data[, max(get(ArgsList$CalendarDate))]
  }
}
