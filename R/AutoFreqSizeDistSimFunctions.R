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
    .Call('_RemixAutoML_QRGibbsSim', PACKAGE = 'RemixAutoML', CountScore, SizeScore, CountList, SizeList, nSims)
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
#' @return The posterior predicted distribution of simulated values
#' @export
ID_SingleLevelGibbsSampler <- function(CountDataLevel,
                                       SizeDataLevel,
                                       FC_Periods = NULL, 
                                       nSims = 5000,
                                       CountList = NULL,
                                       SizeList = NULL){
  
  # Loop through FC_Periods----
  SimResults <- list()
  for(fc in seq_len(FC_Periods)) {
    CountScoreSingle <- as.numeric(CountDataLevel[FC_Window == fc])[2:length(CountDataLevel)]
    SizeScoreSingle <- as.numeric(SizeDataLevel[FC_Window == fc])[2:length(SizeDataLevel)]
    SimResults[[fc]] <- QRGibbsSim(CountScore = CountScoreSingle,
               SizeScore = SizeScoreSingle,
               CountList = CountList,
               SizeList = SizeList,
               nSims = nSims)
  }
  return(
    data.table::rbindlist(
      list(
        simResults)))
}