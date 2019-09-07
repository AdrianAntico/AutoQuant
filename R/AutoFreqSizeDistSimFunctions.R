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

#' IntermittentDemandSimulator
#' 
#' IntermittentDemandSimulator
#' 
#' @author Adrian Antico
#' @family Automated Time Series
#' @param
#' @param
#' @param
#' @examples 
#' @return 
#' @export
Demand <- function(sku,
                   FC_Periods, 
                   nSims,
                   CountList,
                   SizeList){
  score <- singleSKU_UD(sku, TargetWindows = 1:weeksOut)
  simResults <- lapply(1:weeksOut, function(w){
    temp <- as.numeric(score[TargetWindow == w])
    QRGibbsSim(temp, 
               CountList = CountList,
               SizeList = SizeList,
               nSims = nSims)
  })
  return(
    data.table::rbindlist(
      list(
        simResults)))
}