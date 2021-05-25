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

#' @title ScatterCopula
#'
#' @description Dual plot. One on original scale and one using empirical copula data
#'
#' @author Adrian Antico
#'
#' @family EDA
#'
#' @param data Source data.table
#' @param x_var Numeric variable
#' @param y_var Numeric variable
#' @param GroupVariable Color options
#' @param SampleCount Number of randomized rows to utilize. For speedup and memory purposes
#' @param FitGam Add gam fit to scatterplot and copula plot
#' @param color = "darkblue"
#' @param point_size = 0.50
#' @param text_size = 12
#' @param x_axis_text_angle = 35
#' @param y_axis_text_angle = 0
#' @param chart_color = "lightsteelblue1"
#' @param border_color = "darkblue"
#' @param text_color = "darkblue"
#' @param grid_color = "white"
#' @param background_color = "gray95"
#' @param legend_position = "bottom
#'
#' @export
ScatterCopula <- function(data = NULL,
                          x_var = NULL,
                          y_var = NULL,
                          GroupVariable = NULL,
                          SampleCount = 100000L,
                          FitGam = TRUE,
                          color = "darkblue",
                          point_size = 0.50,
                          text_size = 12,
                          x_axis_text_angle = 35,
                          y_axis_text_angle = 0,
                          chart_color = "lightsteelblue1",
                          border_color = "darkblue",
                          text_color = "darkblue",
                          grid_color = "white",
                          background_color = "gray95",
                          legend_position = "bottom") {

  # Subset data
  temp <- data[order(runif(.N))][seq_len(min(SampleCount, .N))][, .SD, .SDcols = c(y_var, x_var, GroupVariable)]
  temp[, Var1 := data.table::frank(get(y_var)) * (1 / 0.001) / .N * 0.001]
  temp[, Var2 := data.table::frank(get(x_var)) * (1 / 0.001) / .N * 0.001]

  # Correlations
  OS_Pearson <- cor(x = temp[[y_var]], y = temp[[x_var]], method = "pearson")
  OS_Spearman <- cor(x = temp[[y_var]], y = temp[[x_var]], method = "spearman")
  cop_Pearson <- cor(x = temp[["Var1"]], y = temp[["Var2"]], method = "pearson")
  cop_Spearman <- cor(x = temp[["Var1"]], y = temp[["Var2"]], method = "spearman")

  # Regular scatter plot
  if(is.null(GroupVariable)) {
    original_scale_plot <- suppressMessages(
      eval(
        ggplot2::ggplot(
          data = temp,
          ggplot2::aes_string(x = x_var, y = y_var)) +
          ggplot2::geom_point(size = point_size, color = color) +
          ggplot2::ggtitle(paste0("ScatterPlot: Pearson Corr: ", round(OS_Pearson, 3L), " :: Spearman Corr: ", round(OS_Spearman, 3L))) +
          ChartTheme(
            Size = text_size,
            AngleX = x_axis_text_angle,
            AngleY = y_axis_text_angle,
            ChartColor = chart_color,
            BorderColor = border_color,
            TextColor = text_color,
            GridColor = grid_color,
            BackGroundColor = background_color,
            LegendPosition = legend_position)))

    # Gam fit
    if(FitGam) original_scale_plot <- original_scale_plot + ggplot2::geom_smooth(method='gam')

  } else {
    original_scale_plot <- suppressMessages(
      eval(
        ggplot2::ggplot(
          data = temp,
          ggplot2::aes_string(x = x_var, y = y_var, color = GroupVariable)) +
          ggplot2::geom_point(size = point_size) +
          ggplot2::ggtitle(paste0("ScatterPlot: Pearson Corr: ", round(OS_Pearson, 3L), " :: Spearman Corr: ", round(OS_Spearman, 3L))) +
          ChartTheme(
            Size = text_size,
            AngleX = x_axis_text_angle,
            AngleY = y_axis_text_angle,
            ChartColor = chart_color,
            BorderColor = border_color,
            TextColor = text_color,
            GridColor = grid_color,
            BackGroundColor = background_color,
            LegendPosition = legend_position)))

    # Gam fit
    if(FitGam) original_scale_plot <- original_scale_plot + ggplot2::geom_smooth(method='gam')

  }

  # Empirical Copula Scatter
  if(is.null(GroupVariable)) {
    copula_plot <- suppressMessages(
      eval(
        ggplot2::ggplot(data = temp, ggplot2::aes_string(x = "Var2", y = "Var1")) +
          ggplot2::geom_point(size = point_size, color = color) +
          ggplot2::geom_smooth(method = "lm") +
          ggplot2::geom_segment(ggplot2::aes(x = 0, xend = 1, y = 0, yend = 1), colour = "darkviolet", size = 1) +
          ggplot2::ggtitle(paste0("Copula: Pearson Corr: ", round(cop_Pearson, 3L), " :: Spearman Corr: ", round(cop_Spearman, 3L))) +
          ggplot2::xlab(label = x_var) + ggplot2::ylab(label = y_var) +
          ChartTheme(
            Size = text_size,
            AngleX = x_axis_text_angle,
            AngleY = y_axis_text_angle,
            ChartColor = chart_color,
            BorderColor = border_color,
            TextColor = text_color,
            GridColor = grid_color,
            BackGroundColor = background_color,
            LegendPosition = legend_position)))

    # Gam fit
    if(FitGam) copula_plot <- copula_plot + ggplot2::geom_smooth(method='gam')

  } else {
    copula_plot <- suppressMessages(
      eval(
        ggplot2::ggplot(data = temp, ggplot2::aes_string(x = "Var2", y = "Var1", color = GroupVariable)) +
          ggplot2::geom_point(size = point_size) +
          ggplot2::geom_smooth(method = "lm") +
          ggplot2::geom_segment(ggplot2::aes(x = 0, xend = 1, y = 0, yend = 1), colour = "darkviolet", size = 1) +
          ggplot2::ggtitle(paste0("Copula: Pearson Corr: ", round(cop_Pearson, 3L), " :: Spearman Corr: ", round(cop_Spearman, 3L))) +
          ggplot2::xlab(label = x_var) + ggplot2::ylab(label = y_var) +
          ChartTheme(
            Size = text_size,
            AngleX = x_axis_text_angle,
            AngleY = y_axis_text_angle,
            ChartColor = chart_color,
            BorderColor = border_color,
            TextColor = text_color,
            GridColor = grid_color,
            BackGroundColor = background_color,
            LegendPosition = legend_position)))

    # Gam fit
    if(FitGam) copula_plot <- copula_plot + ggplot2::geom_smooth(method='gam')

  }

  # Return
  return(list(ScatterPlot = original_scale_plot, CopulaPlot = copula_plot))
}
