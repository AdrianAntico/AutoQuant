#' @title AutoCorrAnalysis
#'
#' @description Generate correlation analysis over a data set
#'
#' @family EDA
#'
#' @author Adrian Antico
#'
#' @param data data.table
#' @param CorVars Can leave NULL or supply column names you want to analyze
#' @param SkipCorVars Can leave NULL or supply column names you want to skip
#' @param ByGroupVars Categorical variables to run correlation analysis by
#'
#' @examples
#' \dontrun{
#' data <- RemixAutoML::FakeDataGenerator(
#'   Correlation = 0.85,
#'   N = 10000L,
#'   ID = 2L,
#'   FactorCount = 2L,
#'   AddDate = TRUE,
#'   ZIP = 2L,
#'   TimeSeries = FALSE,
#'   ChainLadder = FALSE,
#'   Classification = TRUE,
#'   MultiClass = FALSE)
#'
#' # Run Analysis
#' data <- RemixAutoML::AutoCorrAnalysis(
#'   data = data,
#'   CorVars = NULL,
#'   SkipCorVars = c("IDcol_1","IDcol_2","DateTime"),
#'   ByGroupVars = "Factor_1",
#'   DataSampleRate = 0.50,
#'   MinRows = 30,
#'   KeepSignificantVars = TRUE,
#'   PValAdjMethod = "holm",
#'   RobustCalc = TRUE,
#'   PartialCorr = FALSE,
#'   BayesianCorr = FALSE)
#' }
#' @noRd
AutoCorrAnalysis <- function(data = NULL,
                             CorVars = NULL,
                             SkipCorVars = NULL,
                             ByGroupVars = NULL,
                             DataSampleRate = 0.50,
                             MinRows = 30,
                             KeepSignificantVars = TRUE,
                             PValAdjMethod = "holm",
                             RobustCalc = TRUE,
                             PartialCorr = FALSE,
                             BayesianCorr = FALSE) {
  if(!data.table::is.data.table(data)) data.table::setDT(data)
  if(!is.null(CorVars)) {
    CorVars <- CorVars[!CorVars %chin% SkipCorVars]
  } else {
    CorVars <- names(data)[!names(data) %chin% SkipCorVars]
  }

  # Subset data
  if(!all(names(data) %chin% CorVars)) data <- data[, .SD, .SDcols = c(CorVars)]
  if(DataSampleRate < 1.0) data <- data[order(runif(.N))][seq_len(floor(.N*DataSampleRate))]

  # Bayesian calc ----
  if(BayesianCorr && PartialCorr) {
    PartialBayesian <- TRUE
  } else {
    PartialBayesian <- FALSE
  }

  # Convert character to factor
  data <- RemixAutoML::ModelDataPrep(data = data, Impute = FALSE, CharToFactor = TRUE, FactorToChar = FALSE, IntToNumeric = FALSE, LogicalToBinary = TRUE, DateToChar = FALSE, RemoveDates = FALSE, MissFactor = "0", MissNum = -1, IgnoreCols = NULL)

  # Corr Analysis ----
  if(is.null(ByGroupVars)) {
    CorrAnalysis <- data.table::setDT(correlation::correlation(data = data, p_adjust = PvalAdjMethod, redundant = FALSE, include_factors = TRUE, robust = RobustCalc, partial = PartialCorr, bayesian = BayesianCorr, partial_bayesian = PartialBayesian))
    if(KeepSignificantVars) CorrAnalysis <- CorrAnalysis[p < 0.05]
    return(CorrAnalysis)
  } else {
    VarList <- list()
    VarList[["TotalData"]] <- data.table::setDT(correlation::correlation(data = data, p_adjust = PvalAdjMethod, redundant = FALSE, include_factors = TRUE, robust = RobustCalc, partial = PartialCorr, bayesian = BayesianCorr, partial_bayesian = PartialBayesian))
    for(group in ByGroupVars) {
      Levels <- as.character(data[, .N, by = eval(group)][order(-N)][N > MinRows][[eval(group)]])
      for(lev in Levels) {
        data1 <- data[get(group) == eval(lev)]
        temp <- data.table::setDT(correlation::correlation(data = data1, p_adjust = PvalAdjMethod, redundant = FALSE, include_factors = TRUE, robust = RobustCalc, partial = PartialCorr, bayesian = BayesianCorr, partial_bayesian = PartialBayesian))
        temp <- temp[!is.na(r)]
        if(KeepSignificantVars) temp <- temp[p < 0.05]
        VarList[[paste0(group,"_",lev)]] <- temp
        rm(temp)
      }
    }
    return(data.table::rbindlist(VarList))
  }
}

#' @title BNLearnArcStrength
#'
#' @description Utilize bnlearn to create a bayesian network and return the arc strengths for features and their edges
#'
#' @author Adrian Antico
#'
#' @family EDA
#'
#' @param data data.table
#' @param NetworkVars Names of the columns to utilize in the analysis
#' @param DataSampleRate Sample your data to reduce runtime
#' @param ByGroupVars Group variables that you want to have the analysis done by
#' @param MinRows Minimum number of rows to utilize in the ByGroupVars analysis
#'
#' @noRd
BNLearnArcStrength <- function(data = NULL,
                               NetworkVars = NULL,
                               DataSampleRate = 0.50,
                               ByGroupVars = NULL,
                               MinRows = 30) {

  if(!data.table::is.data.table(data)) data.table::setDT(data)
  if(DataSampleRate < 1.0) data <- data[order(runif(.N))][seq_len(floor(.N * DataSampleRate))]
  OutputList <- list()
  if(is.null(ByGroupVars)) {
    data1 <- data[, .SD, .SDcols = c(NetworkVars)]
    net <- bnlearn::hc(data1)
    fitted <- bnlearn::bn.fit(net, data1)
    ArcStrength <- data.table::setDT(bnlearn::arc.strength(net, data1))
    for(arc in ArcStrength$to) {
      Collect <- data.table::data.table()
      Collect[, ChildVar := fitted[[arc]][["node"]]]
      Collect <- cbind(Collect, ParentVar = fitted[[arc]][["parents"]])
      Collect[, ArcStrengths := ArcStrength[from %chin% fitted[[arc]][["parents"]] & to %chin% fittled[[arc]][["node"]]][, strength]]
      OutputList[[arc]] <- Collect
    }
    return(list(Data = data.table::rbindlist(OutputList)[!is.na(ArcStrengths)][order(-abs(ArcStrengths))], Structure = invisible(dbnR::plot_network(structure = fitted))))
  } else {
    data1 <- data[, .SD, .SDcols = c(NetworkVars)]
    net <- bnlearn::hc(data1)
    FittedOutput <- bnlearn::bn.fit(net, data1)
    for(group in ByGroupVars) {
      Levels <- as.character(data[, .N, by = eval(group)][N > MinRows][, get(group)])
      for(lev in Levels) {
        data1 <- data[get(group) == eval(lev)]
        data1 <- data1[, .SD, .SDcols = c(NetworkVars)]
        net <- bnlearn::hc(data1)
        fitted <- bnlearn::bn.fit(net, data1)
        ArcStrength <- data.table::setDT(bnlearn::arc.strength(net, data1))
        for(arc in names(fitted)) {
          Collect <- data.table::data.table()
          Collect[, GroupName := eval(group)]
          Collect[, GroupLevel := eval(lev)]
          Collect <- cbind(Collect, ChildVar = fitted[[arc]][["node"]])
          Collect <- cbind(Collect, ParentVar = fittled[[arc]][["parents"]])
          Collect[, ArcStrengths := ArcStrength[from %chin% fittled[[arc]][["parents"]] & to %chin% fitted[[arc]][["node"]]][, strength]]
          OutputList[[paste0(group,"-",lev,"-",arc)]] <- Collect
        }
      }
    }
    return(list(Data = data.table::rbindlist(OutputList)[!is.na(ArcStrengths)][order(-abs(ArcStrengths))], Structure = invisible(dbnR::plot_network(structure = FittedOutput))))
  }
}

#' @title PlotGUI
#'
#' @description Spin up the esquisse plotting gui
#'
#' @export
PlotGUI <- function() {
  if('esquisse' %in% installed.packages() && "rvg" %in% installed.packages()) esquisse::esquisser() else stop("You need to install 'esquisse' and / or 'rvg' to run the gui")
}
