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
#' @param DataSampleRate = 0.50,
#' @param MinRows = 30,
#' @param KeepSignificantVars = TRUE,
#' @param PValAdjMethod = "holm",
#' @param RankTransform = TRUE,
#' @param PartialCorr = FALSE,
#' @param BayesianCorr = FALSE
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
#'   RankTransform = TRUE,
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
                             RankTransform = TRUE,
                             PartialCorr = FALSE,
                             BayesianCorr = FALSE) {
  if(!data.table::is.data.table(data)) data.table::setDT(data)
  if(!is.null(CorVars)) {
    CorVars <- CorVars[!CorVars %chin% SkipCorVars]
  } else {
    CorVars <- names(data)[!names(data) %chin% SkipCorVars]
  }

  # Subset data
  if(!all(names(data) %chin% CorVars)) data <- data[, .SD, .SDcols = c(CorVars, ByGroupVars)]
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
    CorrAnalysis <- data.table::setDT(correlation::correlation(data = data, p_adjust = PValAdjMethod, redundant = FALSE, include_factors = TRUE, ranktransform = RankTransform, partial = PartialCorr, bayesian = BayesianCorr, partial_bayesian = PartialBayesian))
    if(KeepSignificantVars) CorrAnalysis <- CorrAnalysis[p < 0.05]
    return(CorrAnalysis)
  } else {
    VarList <- list()
    VarList[["TotalData"]] <- data.table::setDT(correlation::correlation(data = data, p_adjust = PValAdjMethod, redundant = FALSE, include_factors = TRUE, ranktransform = RankTransform, partial = PartialCorr, bayesian = BayesianCorr, partial_bayesian = PartialBayesian))
    for(group in ByGroupVars) {
      print(group)
      Levels <- as.character(data[, .N, by = eval(group)][order(-N)][N > MinRows][[eval(group)]])
      for(lev in Levels) {
        print(lev)
        data1 <- data[get(group) == eval(lev)]
        temp <- data.table::setDT(correlation::correlation(data = data1, p_adjust = PValAdjMethod, redundant = FALSE, include_factors = TRUE,  ranktransform = RankTransform, partial = PartialCorr, bayesian = BayesianCorr, partial_bayesian = PartialBayesian))
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
      Collect[, ArcStrengths := ArcStrength[from %chin% fitted[[arc]][["parents"]] & to %chin% fitted[[arc]][["node"]]][, strength]]
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
          Collect <- cbind(Collect, ParentVar = fitted[[arc]][["parents"]])
          Collect[, ArcStrengths := ArcStrength[from %chin% fitted[[arc]][["parents"]] & to %chin% fitted[[arc]][["node"]]][, strength]]
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
#' @family EDA
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
          ggplot2::geom_segment(ggplot2::aes(x = 0, xend = 1, y = 0, yend = 1), colour = "darkviolet", size = 0.25) +
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
    if(FitGam) copula_plot <- copula_plot + ggplot2::geom_smooth(method = "lm") + ggplot2::geom_smooth(method='gam')
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
    if(FitGam) copula_plot <- copula_plot + ggplot2::geom_smooth(method = "lm") + ggplot2::geom_smooth(method='gam')
  }

  # Return
  return(list(ScatterPlot = original_scale_plot, CopulaPlot = copula_plot))
}

#' @title NameTypeDistinct
#'
#' @description Generates a data.table containing the column names, types, and distinct values from a source data.table
#'
#' @author Adrian Antico
#' @family EDA
#'
#' @param data Source data.table
#'
#' @noRd
NameTypeDistinct <- function(data) {
  x <- length(names(data))
  MetaData <- data.table::data.table(
    Variable = rep(NA_character_, x),
    Type = rep(NA_character_, x),
    Distinct = rep(NA_real_, x))
  x <- names(data)
  for(xx in seq_along(x)) {
    data.table::set(MetaData, i = xx, j = "Variable", value = x[xx])
    data.table::set(MetaData, i = xx, j = "Type", value = class(data[1L, get(x[xx])])[1L])
    data.table::set(MetaData, i = xx, j = "Distinct", value = length(unique(data[[x[xx]]])))
  }
  return(MetaData)
}

#' @title EDA_Histograms
#'
#' @description Creates histograms
#'
#' @author Adrian Antico
#' @family EDA
#'
#' @param data Input data.table
#' @param PlotColumns Default NULL. If NULL, all columns will be plotted (except date cols). Otherwise, supply a character vector of columns names to plot
#' @param SampleCount Number of random samples to use from data. data is first shuffled and then random samples taken
#' @param SavePath Output file path to where you can optionally save pdf
#' @param FactorCountPerPlot Default 10
#' @param AddDensityLine Set to TRUE to add a density line to the plots
#' @param PrintOutput Default FALSE. TRUE will print results upon running function
#' @param Size Default 12
#' @param AngleX Default 35
#' @param AngleY Default 0
#' @param ChartColor Default "lightsteelblue1"
#' @param BorderColor Default "darkblue"
#' @param TextColor Default "darkblue"
#' @param GridColor Default "white"
#' @param BackGroundColor Default "gray95"
#' @param LegendPosition Default "bottom"
#'
#' @export
EDA_Histograms <- function(data = NULL,
                           PlotColumns = NULL,
                           SampleCount = 100000,
                           SavePath = NULL,
                           FactorCountPerPlot = 10,
                           AddDensityLine = FALSE,
                           PrintOutput = FALSE,
                           Size = 12,
                           AngleX = 35,
                           AngleY = 0,
                           ChartColor = "lightsteelblue1",
                           BorderColor = "darkblue",
                           TextColor = "darkblue",
                           GridColor = "white",
                           BackGroundColor = "gray95",
                           LegendPosition = "bottom") {

  # Convert to dt
  if(!data.table::is.data.table(data)) data.table::setDT(data)
  if(data[,.N] > SampleCount) data <- data.table::copy(data[order(runif(.N))][seq_len(eval(SampleCount))])
  varMetadata <- NameTypeDistinct(data)
  if(!is.null(PlotColumns)) varMetadata[Variable %chin% eval(PlotColumns)]
  pb <- txtProgressBar(0, nrow(varMetadata))
  resVars <- c()
  results <- list()

  # Create plots
  for(i in seq_len(varMetadata[, .N])) {
    var <- varMetadata[i,]
    varName <- as.character(var$Variable)
    setTxtProgressBar(pb, i)
    if(var$Type %in% c("integer", "logical", "numeric", "factor", "character")) {
      resVars = unique(c(resVars, as.character(varName)))
      if(var$Type %in% c("integer", "numeric")) {
        varAnalyze = data.table::data.table(dat = as.double(data[[varName]]))
        range <- varAnalyze[, max(dat, na.rm = T) - min(varAnalyze$dat, na.rm = T)]
        if(var$Distinct > 10) {
          if(nrow(varAnalyze) > 1000 && var$Distinct > 50) {
            bins <- 20
          } else if(nrow(varAnalyze) > 5000 && var$Distinct > 30) {
            bins <- 15
          } else {
            bins <- 10
          }
          temp <- eval(
            ggplot2::ggplot(
              varAnalyze, ggplot2::aes(dat)) +
              ggplot2::geom_histogram(ggplot2::aes(y = ..density..), bins = bins, show.legend = FALSE, col = "grey", fill = "#5555ee") +
              ggplot2::scale_fill_discrete(h = c(180, 250), l = 50) +
              RemixAutoML::ChartTheme(
                Size = Size,
                AngleX = AngleX,
                AngleY = AngleY,
                ChartColor = ChartColor,
                BorderColor = BorderColor,
                TextColor = TextColor,
                GridColor = GridColor,
                BackGroundColor = BackGroundColor,
                LegendPosition = LegendPosition) +
              ggplot2::labs(x = varName, y = "Density") +
              ggplot2::ggtitle(paste("Histogram of", varName)))
          if(AddDensityLine) {
            temp <- temp + ggplot2::stat_function(fun = dnorm, args = list(mean = mean(varAnalyze$dat, na.rm = TRUE), sd = sd(varAnalyze$dat, na.rm = TRUE)), col = "red")
          }
          results[[varName]] <- temp
        } else {
          varAnalyze = data.table::data.table(dat = as.character(data[[varName]]))
          results[[varName]] <- eval(
            ggplot2::ggplot(
              varAnalyze, ggplot2::aes(dat, fill = dat)) +
              ggplot2::geom_bar(show.legend = FALSE) +
              ggplot2::scale_fill_discrete(h = c(180, 250), l = 50) +
              RemixAutoML::ChartTheme(
                Size = Size,
                AngleX = AngleX,
                AngleY = AngleY,
                ChartColor = ChartColor,
                BorderColor = BorderColor,
                TextColor = TextColor,
                GridColor = GridColor,
                BackGroundColor = BackGroundColor,
                LegendPosition = LegendPosition) +
              ggplot2::labs(x = varName, y = "Density") +
              ggplot2::ggtitle(paste("Bar Chart of", varName)))
        }
      } else {
        varAnalyze <- data.table::data.table(dat = as.character(data[[varName]]))
        grouped <- varAnalyze[, .N, by = "dat"][order(-N)]
        if(nrow(grouped) > FactorCountPerPlot) {
          top <- grouped[seq_len(min(.N, FactorCountPerPlot))]
          others <- data.table::fsetdiff(x = grouped, y = top)
          others <- data.table::data.table(dat = "other", N = others[, sum(N)])
        }
        if(nrow(grouped) > 10) grouped <- data.table::rbindlist(list(grouped, others))
        results[[varName]] <- eval(
          ggplot2::ggplot(data = grouped, ggplot2::aes(x = dat, y = N, fill = dat)) +
            ggplot2::geom_bar(stat = "identity", show.legend = FALSE) +
            ggplot2::coord_flip() +
            ggplot2::scale_fill_discrete(h = c(180, 250), l = 50) +
            RemixAutoML::ChartTheme(
              Size = Size,
              AngleX = AngleX,
              AngleY = AngleY,
              ChartColor = ChartColor,
              BorderColor = BorderColor,
              TextColor = TextColor,
              GridColor = GridColor,
              BackGroundColor = BackGroundColor,
              LegendPosition = LegendPosition) +
            ggplot2::labs(x = varName, y = "Counts") +
            ggplot2::ggtitle(paste0("Bar Chart of ", varName)))
      }
    }
  }

  # Combine plots
  close(pb)
  if(PrintOutput) multiplot(plotlist = results)

  # Save plots
  if(!is.null(SavePath)) {
    for(i in seq_along(results)) {
      ggplot2::ggsave(filename = file.path(SavePath, paste0(gsub("[^a-z0-9 ]", "_", tolower(resVars[[i]])), ".png")), plot = results[[i]])
    }
  }
  return(results)
}

#' @title UserBaseEvolution
#'
#' @description This function creates a table of user counts over time for accumulated unique users, active unique users, new unique users, retained unique users, churned unique users, and reactivated unique users. You can run this with several specifications. You can request monthly, weekly, or daily counts and you can specify a churn window for the computations. If you want to compare how many churned users also churned from another segment of sorts, provide a list in the Cross parameter.
#'
#' @author Adrian Antico
#' @family EDA
#'
#' @param data Source data.table
#' @param Cross Can be NULL. User base from non source. Must be a named list. Names of list are used to name columns in output table. Entity and DateColumnName must be identical across data sets.
#' @param Entity Column name of the entity / user
#' @param DateColumnName Name of the date column used for inclusion of users in time periods
#' @param TimeAgg Choose from 'Month', 'Week', or 'Day'. Do not lowercase
#' @param ChurnPeriods Defaults to 1. This means for TimeAgg = 'Month' a one month churn period is used. For TimeAgg = 'Week' you will have a one week churn period. If you set ChurnPeriods to 2 then it will be a 2 month churn or a 2 week churn. Same logic applies for daily.
#'
#' @export
UserBaseEvolution <- function(data, Cross = NULL, Entity = NULL, DateColumnName = NULL, TimeAgg = NULL, ChurnPeriods = 1) {

  # Set up time_agg column
  temp_func <- function(data1 = NULL, date_col = NULL, time_agg = NULL) {
    if(tolower(time_agg) == 'month') {
      data1[, paste0("Year", time_agg) := data.table::month(get(date_col))]
      data1[, paste0("Year", time_agg) := data.table::fifelse(get(paste0("Year", time_agg)) < 10, as.numeric(paste0(data.table::year(get(date_col)), 0, get(paste0("Year", time_agg)))), as.numeric(paste0(data.table::year(get(date_col)), get(paste0("Year", time_agg)))))]
    } else if(tolower(time_agg) == 'week') {
      data1[, paste0("Year", time_agg) := data.table::week(get(date_col))]
      data1[, paste0("Year", time_agg) := data.table::fifelse(get(paste0("Year", time_agg)) < 10, as.numeric(paste0(data.table::year(get(date_col)), 0, get(paste0("Year", time_agg)))), as.numeric(paste0(data.table::year(get(date_col)), get(paste0("Year", time_agg)))))]
    } else if(tolower(Time) == 'day') {
      data1[, paste0("Year", time_agg) := data.table::yday(get(date_col))]
      data1[, paste0("Year", time_agg) := data.table::fcase(
        get(paste0("Year", time_agg)) < 100, as.numeric(paste0(data.table::year(get(date_col)), 00, get(paste0("Year", time_agg)))),
        get(paste0("Year", time_agg)) < 10, as.numeric(paste0(data.table::year(get(date_col)), 0, get(paste0("Year", time_agg)))),
        get(paste0("Year", time_agg)) > 0, as.numeric(paste0(data.table::year(get(date_col)), get(paste0("Year", time_agg)))))]
    }
    return(data1)
  }

  # data sets
  for(g in seq_len((1 + length(Cross)))) {
    if(g == 1) {
      data <- temp_func(data=data, date_col = DateColumnName, time_agg = TimeAgg)
    } else {
      Cross[[names(Cross)[g-1]]] <- temp_func(data=Cross[[names(Cross)[g-1]]], date_col = DateColumnName, time_agg = TimeAgg)
    }
  }

  # Range
  LoopRange <- sort(data[, unique(get(paste0("Year", TimeAgg)))])

  # Set up Entity lists
  for(g in seq_len((1 + length(Cross)))) {
    if(g == 1) {
      EntityList <- list()
      for(i in seq_along(LoopRange)) {
        EntityList[[paste0("Entities", i)]] <- data[get(paste0("Year", TimeAgg)) == eval(LoopRange[i]), unique(get(Entity))]
        if(i != 1) {
          EntityList[[paste0("Accumulated_", i)]] <- unique(c(EntityList[[paste0("Accumulated_", i-1)]], EntityList[[paste0("Entities", i)]]))
        } else {
          EntityList[[paste0("Accumulated_", i)]] <- data[get(paste0("Year", TimeAgg)) == eval(LoopRange[i]), unique(get(Entity))]
        }
      }
    } else {
      for(i in seq_along(LoopRange)) {
        EntityList[[paste0(names(Cross)[g-1], "_Entities", i)]] <- Cross[[names(Cross)[g-1]]][get(paste0("Year", TimeAgg)) == eval(LoopRange[i]), unique(get(Entity))]
        if(i != 1) {
          EntityList[[paste0(names(Cross)[g-1], "_Accumulated_", i)]] <- unique(c(EntityList[[paste0(names(Cross)[g-1], "_Accumulated_", i-1)]], EntityList[[paste0(names(Cross)[g-1], "_Entities", i)]]))
        } else {
          EntityList[[paste0(names(Cross)[g-1], "_Accumulated_", i)]] <- Cross[[names(Cross)[g-1]]][get(paste0("Year", TimeAgg)) == eval(LoopRange[i]), unique(get(Entity))]
        }
      }
    }
  }


  # Create collection table
  Collection <- data.table::data.table(
    temp = LoopRange,
    Accumulated_Entities = 0,
    Active_Entities = 0,
    New_Entities = 0,
    Retained_Entities = 0,
    Churned_Entities = 0,
    Reactivated_Entities = 0)

  # Add columns for Cross
  if(!is.null(Cross)) {
    for(nam in names(Cross)) {
      Collection[, paste0(nam, "_Churned") := 0]
    }
  }

  # Update name
  data.table::setnames(Collection, "temp", paste0("Year_", TimeAgg))

  # Accumulated Entities
  for(i in seq_along(LoopRange)) {
    data.table::set(
      Collection,
      i = i,
      j = "Accumulated_Entities",
      value = unique(length(EntityList[[paste0("Accumulated_", i)]])))
  }

  # Active Entities
  for(i in seq_along(LoopRange)) {
    data.table::set(
      Collection,
      i = i,
      j = "Active_Entities",
      value = unique(length(EntityList[[paste0("Entities", i)]])))
  }

  # New Entities
  for(i in seq_along(LoopRange)) {
    data.table::set(
      Collection,
      i = i,
      j = "New_Entities",
      value =
        length(
          setdiff(
            unique(EntityList[[paste0("Entities", i)]]),
            unique(EntityList[[paste0("Accumulated_", i-1)]]))))

  }

  # Retained Entities
  if(ChurnPeriods == 1) {
    lrange <- seq_along(LoopRange)
  } else {
    lrange <- seq_along(LoopRange)[-(seq_len(ChurnPeriods-1L))]
  }
  for(i in seq_along(LoopRange)) {
    data.table::set(
      Collection,
      i = i,
      j = "Retained_Entities",
      value = length(
        intersect(
          unique(EntityList[[paste0("Entities", i-1)]]),
          unique(EntityList[[paste0("Entities", i)]]))))
  }

  # Churned Entities
  for(i in seq_along(LoopRange)[-(seq_len(ChurnPeriods))]) {
    data.table::set(
      Collection,
      i = i,
      j = "Churned_Entities",
      value = length(
        setdiff(
          unique(EntityList[[paste0("Entities", i-ChurnPeriods)]]),
          unique(EntityList[[paste0("Entities", i)]]))))

    # Cross
    if(!is.null(Cross)) {
      for(nam in names(Cross)) {
        data.table::set(
          Collection,
          i = i,
          j = paste0(nam, "_Churned"),
          value = length(

            # Setdiff = churn
            setdiff(

              # Previously Active in Both Segments
              intersect(
                unique(EntityList[[paste0(nam, "_Entities", i-ChurnPeriods)]]),
                unique(EntityList[[paste0("Entities", i-ChurnPeriods)]])),

              # Currently Active in Cross Segment
              unique(EntityList[[paste0(nam, "_Entities", i)]])
            )))
      }
    }
  }

  # Reactivated Entities
  for(i in seq_along(LoopRange)[-(seq_len(ChurnPeriods+1L))]) {
    data.table::set(
      Collection,
      i = i,
      j = "Reactivated_Entities",
      value = length(
        setdiff(
          setdiff(
            unique(EntityList[[paste0("Entities", i-ChurnPeriods-1)]]),
            unique(EntityList[[paste0("Entities", i-1)]])),
          unique(EntityList[[paste0("Entities", i)]]))))
  }

  # Return
  return(Collection)
}
