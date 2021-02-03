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
#' @export
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

  # Convert to data.table ----
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  # Features ----
  if(!is.null(CorVars)) {
    CorVars <- CorVars[!CorVars %chin% SkipCorVars]
  } else {
    CorVars <- names(data)[!names(data) %chin% SkipCorVars]
  }

  # Subset data
  if(!all(names(data) %chin% CorVars)) data <- data[, .SD, .SDcols = c(CorVars)]

  # Sampling ----
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
#' @export
BNLearnArcStrength <- function(data = NULL,
                               NetworkVars = NULL,
                               DataSampleRate = 0.50,
                               ByGroupVars = NULL,
                               MinRows = 30) {

  # Convert to data.table ----
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  # Sample data ----
  if(DataSampleRate < 1.0) data <- data[order(runif(.N))][seq_len(floor(.N * DataSampleRate))]

  # Collection list ----
  OutputList <- list()

  # Network Analysis ----
  if(is.null(ByGroupVars)) {

    # Subset columns ----
    data1 <- data[, .SD, .SDcols = c(NetworkVars)]

    # Hill climbing algo ----
    net <- bnlearn::hc(data1)

    # Model fit ----
    fitted <- bnlearn::bn.fit(net, data1)

    # Arc Strength ----
    ArcStrength <- data.table::setDT(bnlearn::arc.strength(net, data1))

    # Loop through arc strengths ----
    for(arc in ArcStrength$to) {

      # Initialize data.table ----
      Collect <- data.table::data.table()

      # Add data ----
      Collect[, ChildVar := fitted[[arc]][["node"]]]
      Collect <- cbind(Collect, ParentVar = fitted[[arc]][["parents"]])

      # Add weights ----
      Collect[, ArcStrengths := ArcStrength[from %chin% fitted[[arc]][["parents"]] & to %chin% fittled[[arc]][["node"]]][, strength]]

      # Store data ----
      OutputList[[arc]] <- Collect
    }

    # Return data ----
    return(list(Data = data.table::rbindlist(OutputList)[!is.na(ArcStrengths)][order(-abs(ArcStrengths))],
                Structure = invisible(dbnR::plot_network(structure = fitted))))

  } else {

    # ByGroupVar case ----
    data1 <- data[, .SD, .SDcols = c(NetworkVars)]
    net <- bnlearn::hc(data1)
    FittedOutput <- bnlearn::bn.fit(net, data1)

    # ByGroup ----
    for(group in ByGroupVars) {
      Levels <- as.character(data[, .N, by = eval(group)][N > MinRows][, get(group)])
      for(lev in Levels) {
        data1 <- data[get(group) == eval(lev)]
        data1 <- data1[, .SD, .SDcols = c(NetworkVars)]

        # Hill climbing
        net <- bnlearn::hc(data1)

        # Model ----
        fitted <- bnlearn::bn.fit(net, data1)

        # Arc Strengths ----
        ArcStrength <- data.table::setDT(bnlearn::arc.strength(net, data1))

        # By level ----
        for(arc in names(fitted)) {

          # Initialize data.table ----
          Collect <- data.table::data.table()

          # Set Values ----
          Collect[, GroupName := eval(group)]
          Collect[, GroupLevel := eval(lev)]
          Collect <- cbind(Collect, ChildVar = fitted[[arc]][["node"]])
          Collect <- cbind(Collect, ParentVar = fittled[[arc]][["parents"]])

          # Add strengths ----
          Collect[, ArcStrengths := ArcStrength[from %chin% fittled[[arc]][["parents"]] & to %chin% fitted[[arc]][["node"]]][, strength]]

          # Add to list ----
          OutputList[[paste0(group,"-",lev,"-",arc)]] <- Collect
        }
      }
    }

    # Return data ----
    return(list(Data = data.table::rbindlist(OutputList)[!is.na(ArcStrengths)][order(-abs(ArcStrengths))],
                Structure = invisible(dbnR::plot_network(structure = FittedOutput))))
  }
}
