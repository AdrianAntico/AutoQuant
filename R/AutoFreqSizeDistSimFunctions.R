#' QRGibbsSim for collapsed gibbs sampler simulations
#'
#' QRGibbsSim for collapsed gibbs sampler simulations. Use this to simulate from the Count and Size quantile regression output values.
#'
#' @family Automated Time Series
#' @author Adrian Antico
#' @param CountScore This is the set of predicted values from your Count target variables from AutoCatBoostFreqSizeScoring()
#' @param SizeScore This is the set of predicted values from your Size target variables from AutoCatBoostFreqSizeScoring()
#' @param CountList This is a numeric vector of the Count quantiles used in modeling
#' @param SizeList This is a numeric vector of the Size quantiles used in modeling
#' @param nSims This is the number of simulations you want to generate for each run. E.g. you want to forecast 10 days ahead by day. Then nSims is the number of sims you will run for each individual day of the forecast.
#' @noRd
#' @return A numerical vector of simulations
QRGibbsSim <- function(CountScore, 
                       SizeScore, 
                       CountList, 
                       SizeList, 
                       nSims) {
    .Call('_RemixAutoML_QRGibbsSim', 
          PACKAGE = 'RemixAutoML', 
          CountScore, 
          SizeScore, 
          CountList, 
          SizeList, 
          nSims)
}

#' ID_SingleLevelGibbsSampler for collapsed gibbs sampler 
#' 
#' ID_SingleLevelGibbsSampler for collapsed gibbs sampler from quantile regressions
#' 
#' @author Adrian Antico
#' @family Automated Time Series
#' @param CountDataLevel Single record of predicted values from Count quantile regressions in a numeric vector form
#' @param SizeDataLevel Single record of predicted values from Size quantile regressions in a numeric vector form
#' @param FC_Periods The number of periods you set up to forecast
#' @param nSims The number of simulations you want to have run for each period in 1 to FC_Periods
#' @param CountList A numeric vector of the quantiles used for Count modeling
#' @param SizeList A numeric vector of the quantiles used for Size modeling
#' @param CountVectorSize length of CountList
#' @param SizeVectorSize length of SizeList
#' @return The posterior predicted distribution of simulated values
#' @export
ID_SingleLevelGibbsSampler <- function(CountDataLevel,
                                       SizeDataLevel,
                                       FC_Periods = NULL, 
                                       nSims = 5000,
                                       CountList = NULL,
                                       SizeList = NULL,
                                       CountVectorSize = length(CountList),
                                       SizeVectorSize = length(SizeVectorList)) {
  
  # Loop through FC_Periods----
  SimResults <- list()
  
  for(fc in seq_len(FC_Periods)) {
    CountScoreSingle <- as.numeric(CountDataLevel[FC_Window == fc])[2:length(CountDataLevel)]
    SizeScoreSingle <- as.numeric(SizeDataLevel[FC_Window == fc])[2:length(SizeDataLevel)]
    SimResults[[fc]] <- QRGibbsSim(
      CountScore = CountScoreSingle,
      SizeScore = SizeScoreSingle,
      CountList = CountList,
      SizeList = SizeList,
      nSims = nSims,
      CountVectorSize = CountVectorSize,
      SizeVectorSize = SizeVectorSize)
  }
  return(
    data.table::rbindlist(
      list(
        SimResults)))
}

#' ID_Forecast for forecasting intermittent demand data
#' 
#' ID_Forecast for forecasting intermittent demand data. It runs collapsed gibbs sampler simulations, using the count and size quantile regression models, for every period for all group levels in your data.
#' 
#' @family Automated Time Series
#' @author Adrian Antico
#' @param CountData This is the count data returned from AutoCatBoostFreqSizeScoring() or AutoH2oGBMFreqSizeScoring()
#' @param SizeData This is the size data returned from AutoCatBoostFreqSizeScoring() or AutoH2oGBMFreqSizeScoring()
#' @param CountDataNames This is the count data names returned from AutoCatBoostFreqSizeScoring() or AutoH2oGBMFreqSizeScoring()
#' @param SizeDataNames This is the size data returned from AutoCatBoostFreqSizeScoring() or AutoH2oGBMFreqSizeScoring()
#' @param GroupVar This is your grouping variable. E.g. sku as being the name of the column that contains all skus
#' @param FC_Periods The max period of your forecast. E.g. if you want 52 weeks of forecasts, set to 52
#' @param NumSims Set the number of collapsed gibbs simulations to run for each time unit forecast. E.g. if you want 52 weeks of forecasts, the simulations will run NumSims for each period up to 52.
#' @param PredictionIntervals Set the prediction intervals you want returned. E.g. c(seq(0.05,0.95,0.05))
#' @examples 
#' \donttest{
#' Results <- ID_Forecast(
#'   CountData = FinalData$CountData,
#'   SizeData = FinalData$SizeData,
#'   CountDataNames = FinalData$CountPredNames,
#'   SizeDataNames = FinalData$SizePredNames,
#'   CountQuantiles = seq(0.05,0.95,0.05),
#'   SizeQuantiles = seq(0.05,0.95,0.05),
#'   GroupVar = "sku", 
#'   FC_Periods = 26, 
#'   NumSims = 1000, 
#'   PredictionIntervals = c(0.05,0.20,0.50,0.80,0.95))
#' }
#' @return Returns your entire set of groupvar forecasts in a single data.table
#' @export
ID_Forecast <- function(CountData = FinalData$CountData,
                        SizeData = FinalData$SizeData,
                        CountDataNames = FinalData$CountPredNames,
                        SizeDataNames = FinalData$SizePredNames,
                        CountQuantiles = seq(0.05,0.95,0.05),
                        SizeQuantiles = seq(0.05,0.95,0.05),
                        GroupVar = NULL, 
                        FC_Periods = 26, 
                        NumSims = 1000, 
                        PredictionIntervals = c(0.05,0.20,0.80,0.95)) {
  
  # Ensure data.tables----
  if(!data.table::is.data.table(CountData)) {
    CountData <- data.table::as.data.table(CountData)
  }
  if(!data.table::is.data.table(SizeData)) {
    SizeData <- data.table::as.data.table(SizeData)
  }
  
  # Forecast----
  Counter = 0L
  GroupVariable <- sort(as.character(unique(CountData[[eval(GroupVar)]])))
  CountDataNamesFinal <- CountDataNames[1:length(CountDataNames)]
  SizeDataNamesFinal <- SizeDataNames[1:length(SizeDataNames)]
  for(Level in GroupVariable) {
    
    # Increment counter----
    Counter <- Counter + 1L
    print(Counter)
    
    # Modify Count Data----
    CountDataSim <- CountData[get(GroupVar) == eval(Level)][, ..CountDataNamesFinal]
    for(col in as.integer(2:ncol(CountDataSim))) {
      data.table::set(CountDataSim, 
                      i = which(CountDataSim[[col]] < 0.50),
                      j = col,
                      value = 0)
    }
    
    # Modify size data----
    SizeDataSim <- SizeData[get(GroupVar) == eval(Level)][, ..SizeDataNamesFinal]
    for(col in as.integer(2:ncol(SizeDataSim))) {
      data.table::set(SizeDataSim, 
                      i = which(SizeDataSim[[col]] < 1),
                      j = col,
                      value = 1)
    }
    
    # Run ID_SIngleLevelGibbsSampler()----
    SingleLevelData <- ID_SingleLevelGibbsSampler(
      CountDataLevel = CountDataSim,
      SizeDataLevel = SizeDataSim,
      FC_Periods = FC_Periods, 
      nSims = NumSims, 
      CountList = CountQuantiles,
      SizeList = SizeQuantiles)
    
    # Replace all NaN with 0----
    for(miss in seq_len(FC_Periods)) {
      data.table::set(
        SingleLevelData, 
        i = which(is.na(SingleLevelData[[paste0("V",miss)]])), 
        j = paste0("V",miss), 
        value = 0)
    }

    # Create Final Data----
    ReturnData <- data.table::data.table(GroupVar = Level, Periods = 1:FC_Periods, Mean = 0)
    for(PI in PredictionIntervals) {
      data.table::set(ReturnData, j = paste0("PI_",PI*100), value = 0)
    }
    
    # Fill out data----
    for(period in as.integer(seq_len(FC_Periods))) {
      
      # Fill out mean value----
      data.table::set(
        x = ReturnData,
        i = period,
        j = "Mean",
        value = mean(SingleLevelData[[paste0("V",period)]]))
      
      # Fill out percentiles----
      for(PI in PredictionIntervals) {
        data.table::set(ReturnData, i = period, j = paste0("PI_",PI*100), value = quantile(x = SingleLevelData[[paste0("V",period)]],probs = PI))
      }      
    }
    
    # Combine data----
    if(Counter == 1L) {
      FinalDataReturn <- ReturnData
    } else {
      FinalDataReturn <- data.table::rbindlist(
        list(
          FinalDataReturn, 
          ReturnData))
    }
  }
  
  # Return results----
  return(FinalDataReturn)
}
