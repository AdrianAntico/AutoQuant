#' Multiplot is a function for combining multiple plots
#'
#' Sick of copying this one into your code? Well, not anymore.
#'
#' @author Adrian Antico
#' @family Graphics
#' @param plotlist This is the list of your charts
#' @param cols This is the number of columns in your multiplot
#' @param layout Leave NULL
#' @param ... Passthrough arguments
#' @examples
#' \dontrun{
#' Correl <- 0.85
#' data <- data.table::data.table(Target = runif(100))
#' data[, x1 := qnorm(Target)]
#' data[, x2 := runif(100)]
#' data[, Independent_Variable1 := log(
#'   pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Predict := (
#'   pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
#' p1 <- RemixAutoML::ParDepCalPlots(
#'   data,
#'   PredictionColName = "Predict",
#'   TargetColName = "Target",
#'   IndepVar = "Independent_Variable1",
#'   GraphType = "calibration",
#'   PercentileBucket = 0.20,
#'   FactLevels = 10,
#'   Function = function(x) mean(x, na.rm = TRUE))
#' p2 <- RemixAutoML::ParDepCalPlots(
#'   data,
#'   PredictionColName = "Predict",
#'   TargetColName = "Target",
#'   IndepVar = "Independent_Variable1",
#'   GraphType = "boxplot",
#'   PercentileBucket = 0.20,
#'   FactLevels = 10,
#'   Function = function(x) mean(x, na.rm = TRUE))
#' RemixAutoML::multiplot(plotlist = list(p1,p2), cols = 2)
#' }
#' @return Multiple ggplots on a single image
#' @export
multiplot <- function(...,
                      plotlist = NULL,
                      cols     = 2,
                      layout   = NULL) {
  plots <- c(list(...), plotlist)
  numPlots <- length(plots)
  if(is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots / cols)), ncol = cols, nrow = ceiling(numPlots / cols))
  }
  if(numPlots == 1) {
    print(plots[[1]])
  } else {
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow(layout), ncol(layout))))
    for(i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = grid::viewport(layout.pos.row = matchidx$row, layout.pos.col = matchidx$col))
    }
  }
}

#' RemixTheme function is a ggplot theme generator for ggplots
#'
#' This function adds the Remix Theme to ggplots
#'
#' @author Douglas Pestana
#' @family Graphics
#' @examples
#' \dontrun{
#' data <- data.table::data.table(
#'   DateTime = as.Date(Sys.time()),
#'   Target = stats::filter(rnorm(1000,
#'                                mean = 50,
#'                                sd = 20),
#'                          filter=rep(1,10),
#'                          circular=TRUE))
#' data[, temp := seq(1:1000)][, DateTime := DateTime - temp][
#'   , temp := NULL]
#' data <- data[order(DateTime)]
#' p <- ggplot2::ggplot(data, ggplot2::aes(x = DateTime, y = Target)) +
#'   ggplot2::geom_line()
#' p <- p + RemixTheme()
#' }
#' @return An object to pass along to ggplot objects following the "+" sign
#' @export
RemixTheme <- function() {
  ggplot2::theme(
    axis.title = ggplot2::element_text(size = 11),
    axis.text = ggplot2::element_text(size = 11),
    legend.background = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(color = "#1c1c1c", size = 11),
    legend.title = ggplot2::element_blank(),
    legend.justification = 0,
    legend.position = "top",
    plot.background = ggplot2::element_rect(fill = "#E7E7E7"),
    panel.background = ggplot2::element_rect(fill = "#E7E7E7"),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor.x = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color = "white"),
    panel.grid.minor.y = ggplot2::element_line(color = "white"),
    plot.title = ggplot2::element_text(color = "#1c1c1c", size = 28, hjust = 0, face = "bold"),
    plot.subtitle = ggplot2::element_text(color = "#1c1c1c", size = 16, hjust = 0),
    plot.caption = ggplot2::element_text(size = 9, hjust = 0, face = "italic"))
}

#' ChartTheme function is a ggplot theme generator for ggplots
#'
#' This function helps your ggplots look professional with the choice of the two main colors that will dominate the theme
#'
#' @author Adrian Antico
#' @family Misc
#' @param Size The size of the axis labels and title
#' @param AngleX The angle of the x axis labels
#' @param AngleY The angle of the Y axis labels
#' @param ChartColor "lightsteelblue1",
#' @param BorderColor "darkblue",
#' @param TextColor "darkblue",
#' @param GridColor "white",
#' @param BackGroundColor "gray95",
#' @param LegendPosition Where to place legend
#' @examples
#' \dontrun{
#' data <- data.table::data.table(DateTime = as.Date(Sys.time()),
#'   Target = stats::filter(rnorm(1000,
#'                                mean = 50,
#'                                sd = 20),
#'                          filter=rep(1,10),
#'                          circular=TRUE))
#' data[, temp := seq(1:1000)][, DateTime := DateTime - temp][
#'   , temp := NULL]
#' data <- data[order(DateTime)]
#' p <- ggplot2::ggplot(data, ggplot2::aes(x = DateTime, y = Target)) +
#'   ggplot2::geom_line()
#' p <- p + ChartTheme(Size = 12)
#' }
#' @return An object to pass along to ggplot objects following the "+" sign
#' @export
ChartTheme <- function(Size = 12,
                       AngleX = 35,
                       AngleY = 0,
                       ChartColor = "lightsteelblue1",
                       BorderColor = "darkblue",
                       TextColor = "darkblue",
                       GridColor = "white",
                       BackGroundColor = "gray95",
                       LegendPosition = "bottom") {
  chart_theme <- ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = BackGroundColor),
    panel.background = ggplot2::element_rect(fill = ChartColor, colour = BorderColor, size = 0.25, color = BorderColor),
    panel.grid.major = ggplot2::element_line(colour = BorderColor, size = 0.01, color = GridColor, linetype = 1),
    panel.grid.minor = ggplot2::element_line(colour = BorderColor, size = 0.01, color = GridColor, linetype = 1),
    legend.position = LegendPosition,
    legend.title = ggplot2::element_text(color = BorderColor, size = Size, face = "bold"),
    plot.subtitle = ggplot2::element_text(color = "darkred", size = max(1,floor(Size*5/6)), face = "bold"),
    legend.background = ggplot2::element_rect(fill = BackGroundColor, size = 1, linetype = "solid", color = BorderColor),
    plot.title = ggplot2::element_text(color = TextColor, size = Size, face = "bold"),
    axis.title = ggplot2::element_text(color = TextColor, size = Size, face = "bold"),
    axis.text.x = ggplot2::element_text(colour = TextColor, face = "bold", angle = AngleX),
    axis.text.y = ggplot2::element_text(colour = TextColor, face = "bold", angle = AngleY),
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 20, r = 20, b = 20, l = 20)),
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 20, r = 20, b = 20, l = 20)),
    panel.border = ggplot2::element_rect(colour = BorderColor, fill = NA, size = 1.5))
  chart_theme
}



#' VaporWaveTheme
#'
#' @author Adrian Antico
#' @family Misc
#'
#' @param Size1 Grid line size
#' @param Size2 Axis line sizes
#' @param Size3 Legend line sizes
#' @param LegendPosition "bottom" is default
#' @export
VaporWaveTheme <- function(Size1 = 1,
                           Size2 = 0.5,
                           Size3 = 12,
                           LegendPosition = "bottom") {
  theme(
    # Plot / Panel
    plot.background = element_rect(fill = clr_bg, colour = clr_bg),
    # plot.margin = margin(1.5, 2, 1.5, 1.5, "cm"),
    panel.background = element_rect(fill = clr_bg, color = clr_bg),
    # Grid
    panel.grid = element_line(colour = clr_grid, size = Size1),
    panel.grid.major = element_line(colour = clr_grid, size = Size1),
    panel.grid.minor = element_line(colour = clr_grid, size = Size1),
    axis.ticks.x = element_line(colour = clr_grid, size = Size1),
    axis.line.y = element_line(colour = clr_grid, size = Size2),
    axis.line.x = element_line(colour = clr_grid, size = Size2),
    # Text
    plot.title = element_text(colour = clr_text),
    plot.subtitle = element_text(colour = clr_text),
    axis.text = element_text(colour = clr_text),
    axis.title = element_text(colour = clr_text),
    # Legend
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.title = element_text(colour = clr_text),
    legend.text = element_text(colour = "gray80", size = Size3, face = "bold"),
    legend.position = LegendPosition,
    # Strip
    strip.background = element_rect(fill = clr_bg2, color = clr_bg2))
}

#' TimeSeriesPlotter
#'
#' TimeSeriesPlotter is a function to plot single or multiple lines on a single plot
#'
#' @family Graphics
#' @author Adrian Antico
#' @param data Source data
#' @param TargetVariable Target variable
#' @param DateVariable Date variable
#' @param GroupVariables Group variables
#' @param EvaluationMode TRUE means two lines are displayed for Actual and Forecast
#' @param VLineDate Date of last actual target value
#' @param Aggregate Choose from 'sum' or 'mean'
#' @param TextSize Default 12
#' @param NumberGroupsDisplay Number of lines to display
#' @param LevelsToDisplay Value
#' @param OtherGroupLabel Label to call all other group levels
#' @param DisplayOtherGroup If TRUE, a line will be shown with all levels that fall into 'other' otherwise no line will be shown
#' @param LineWidth Numeric value. Default is 1
#' @param Color Set to "blue", "red", etc
#' @param XTickMarks Number of tick marks on x-axis. "1 minute","15 minutes","30 minutes","1 hour","3 hour","6 hour","12 hour","1 day","3 day","1 week","2 week","1 month","3 month","6 month","1 year","2 year","5 year","10 year"
#' @param AngleX Angle of text on x axis
#' @param AngleY Angle of text on y axis
#' @param ChartColor Color of chart background
#' @param BorderColor Color of border
#' @param TextColor Text color
#' @param GridColor Grid color
#' @param BackGroundColor Background color
#' @param LegendPosition Legend position
#' @param LegendTextColor Text color
#' @param LegendTextSize Text size
#' @param ForecastLineColor Forecast line color
#' @param PredictionIntervals Set to TRUE to plot prediction intervals
#' @param TS_ModelID Select a model from the list for forecasting viewer
#' @param PredictionIntervalColorInner Fills 20th to 80th percentiles
#' @param PredictionIntervalColorOuter Fills 5th to 20th and 80th to 95th percentiles
#' @export
TimeSeriesPlotter <- function(data = data,
                              TargetVariable = "TargetVariableName",
                              DateVariable = "DateVariableName",
                              GroupVariables = "GroupVariableName",
                              EvaluationMode = FALSE,
                              VLineDate = NULL,
                              Aggregate = NULL,
                              NumberGroupsDisplay = 5,
                              LevelsToDisplay = NULL,
                              OtherGroupLabel = "Other",
                              DisplayOtherGroup = FALSE,
                              TextSize = 12,
                              LineWidth = 1,
                              Color = "blue",
                              XTickMarks = "1 year",
                              AngleX = 35,
                              AngleY = 0,
                              ChartColor = "lightsteelblue1",
                              BorderColor = "darkblue",
                              TextColor = "darkblue",
                              GridColor = "white",
                              BackGroundColor = "gray95",
                              LegendPosition = "bottom",
                              LegendTextColor = "darkblue",
                              LegendTextSize = 10,
                              ForecastLineColor = "black",
                              PredictionIntervals = FALSE,
                              TS_ModelID = NULL,
                              PredictionIntervalColorInner = "aquamarine1",
                              PredictionIntervalColorOuter = "peachpuff1") {

  # No scientific notation----
  options(scipen = FALSE)
  options(warn = -1)

  # Ensure data is data.table----
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  # Ensure arguments are correct----
  if(!is.null(TargetVariable)) if(!is.character(TargetVariable)) stop("TargetVariable did not pass through as string")
  if(!is.null(DateVariable)) if(!is.character(DateVariable)) stop("DateVariable did not pass through as string")
  if(!is.null(Aggregate)) if(!is.character(Aggregate)) stop("Aggregate did not pass through as string")
  if(!is.null(NumberGroupsDisplay)) if(is.character(NumberGroupsDisplay) | is.factor(NumberGroupsDisplay)) stop("NumberGroupsDisplay needs to be a number")
  if(!is.null(OtherGroupLabel)) if(!is.character(OtherGroupLabel)) stop("OtherGroupLabel did not pass through as string")

  # Melt if multiple targets----
  if("ModelID" %chin% names(data)) data <- data[ModelID == eval(TS_ModelID)]
  if(length(TargetVariable) > 1 & !EvaluationMode) {
    if(!is.null(GroupVariables)) {
      data <- TimeSeriesMelt(data = data, TargetVariable = TargetVariable, DateVariable = DateVariable, GroupVariables = c(GroupVariables))
      TargetVariable <- "TargetSeries"
      GroupVariables <- c("GroupVar", GroupVariables)
    } else {
      data <- TimeSeriesMelt(data = data, TargetVariable = TargetVariable, DateVariable = DateVariable)
      TargetVariable <- "TargetSeries"
      GroupVariables <- "GroupVar"
    }
  }

  # Ensure GroupVariables are character type----
  if(!is.null(GroupVariables)) data[, eval(GroupVariables) := lapply(.SD, as.character), .SDcols = c(eval(GroupVariables))]

  # Make copy of data----
  PlotData <- data.table::copy(data)

  # Subset columns for plotting----
  if(!is.null(GroupVariables)) {
    PlotData <- PlotData[, .SD, .SDcols = c(eval(TargetVariable), eval(DateVariable), eval(GroupVariables))]
  } else {
    PlotData <- PlotData[, .SD, .SDcols = c(eval(TargetVariable), eval(DateVariable))]
  }

  # Ensure DateVariable is date type----
  PlotData[, eval(DateVariable) := as.POSIXct(get(DateVariable))]

  # Evaluate mode ----
  if(EvaluationMode) {

    # Rename Target Variable
    data.table::setnames(PlotData, eval(TargetVariable[2L]), "Actual")

    # Legend definition
    Colors <- c("Actual" = "red", "Forecast" = "blue")

    # Eval Measures ----
    AvgError <- PlotData[, round(mean(Actual - Forecast),1L)]
    MAE <- PlotData[, round(mean(abs(Actual - Forecast)), 1L)]
    AvgPercError <- PlotData[, round(100*mean(Forecast / Actual - 1), 1L)]
    MAPE <- PlotData[, round(100*mean(abs(Forecast / Actual - 1)), 1L)]

    # Plot
    Plot <- ggplot2::ggplot(PlotData, ggplot2::aes(x = PlotData[, get(DateVariable)])) +
      ggplot2::geom_line(ggplot2::aes(y = PlotData[["Forecast"]], color = "Forecast"), lwd = LineWidth) +
      ggplot2::geom_line(ggplot2::aes(y = PlotData[["Actual"]], color = "Actual"), lwd = LineWidth) +
      ggplot2::xlab(DateVariable) + ggplot2::ylab("Forecast | Actual") +
      ggplot2::scale_color_manual(values = Colors) +
      ggplot2::labs(title = "Evaluation Plot", subtitle = paste0("MAPE = ",MAPE, "%  ::  Avg % Error = ",AvgPercError, "%  ::  MAE = ",MAE, "  ::  Avg Error = ",AvgError)) +
      ggplot2::theme(legend.position = LegendPosition) +
      ggplot2::theme(legend.title = ggplot2::element_blank()) +
      ChartTheme(
        Size = TextSize,
        AngleX = AngleX,
        AngleY = AngleY,
        ChartColor = ChartColor,
        BorderColor = BorderColor,
        TextColor = TextColor,
        GridColor = GridColor,
        BackGroundColor = BackGroundColor,
        LegendPosition = LegendPosition)

    # Update axis lables
    if(!is.null(XTickMarks)) {
      Plot <- Plot + ggplot2::scale_x_datetime(date_breaks = XTickMarks, labels = scales::date_format("%Y-%m-%d"))
    } else {
      Plot <- Plot + ggplot2::scale_x_datetime(labels = scales::date_format("%Y-%m-%d"))
    }

    # Return
    return(Plot)
  }

  # Plot data----
  if(!is.null(GroupVariables)) {

    # If more than 1 grouping variable----
    if(length(GroupVariables) > 1) {

      # Combine Group Variables----
      for (i in seq_len(length(GroupVariables))) PlotData[, eval(GroupVariables[i]) := paste0(eval(GroupVariables[i]),"_", get(GroupVariables[i]))]
      PlotData[, GroupVars := do.call(paste, c(.SD, sep = "_")), .SDcols = c(eval(GroupVariables))]
      PlotData[, paste0(eval(GroupVariables)) := NULL]

      # Collapse groups----
      SumTable <- PlotData[, sum(get(TargetVariable), na.rm = TRUE), by = "GroupVars"][order(-V1)]
      if(is.null(LevelsToDisplay)) {
        Levels <- as.character(SumTable[1:NumberGroupsDisplay, .SD, .SDcols = "GroupVars"][[1]])
        tempData <- PlotData[GroupVars %chin% Levels]
      } else {
        tempData <- PlotData[GroupVars %chin% LevelsToDisplay]
      }

      if(tolower(Aggregate) == "sum") {
        tempData <- tempData[, sum(get(TargetVariable), na.rm = TRUE), by = c("GroupVars", eval(DateVariable))]
        data.table::setnames(tempData, "V1", eval(TargetVariable))
      } else {
        tempData <- tempData[, mean(get(TargetVariable), na.rm = TRUE), by = c("GroupVars", eval(DateVariable))]
        data.table::setnames(tempData, "V1", eval(TargetVariable))
      }

      # Care to see all other groups as a single group level----
      if(DisplayOtherGroup) {
        tempData2 <- PlotData[!(GroupVars %chin% LevelsToDisplay)]
        tempData2[, GroupVars := eval(OtherGroupLabel)]
        if(tolower(Aggregate) == "sum") {
          tempData2 <- tempData2[, sum(get(TargetVariable), na.rm = TRUE), by = c("GroupVars", eval(DateVariable))]
          data.table::setnames(tempData2, "V1", eval(TargetVariable))
        } else if(tolower(Aggregate) == "mean") {
          tempData2 <- tempData2[, mean(get(TargetVariable), na.rm = TRUE), by = c("GroupVars", eval(DateVariable))]
          data.table::setnames(tempData2, "V1", eval(TargetVariable))
        }

        # Recombine data sets----
        tempData2 <- data.table::rbindlist(list(tempData, tempData2), use.names = TRUE)

      } else {

        # Recombine data sets----
        tempData2 <- tempData
      }

      # Grouping variables ----
      Plot <- ggplot2::ggplot(
        tempData2,
        ggplot2::aes(x = get(DateVariable), y = get(TargetVariable), color = GroupVars)) +
        ggplot2::geom_line() +
        ggplot2::theme(legend.title = ggplot2::element_blank()) +
        ggplot2::xlab(DateVariable) + ggplot2::ylab(TargetVariable) +
        ChartTheme(
          Size = TextSize,
          AngleX = AngleX,
          AngleY = AngleY,
          ChartColor = ChartColor,
          BorderColor = BorderColor,
          TextColor = TextColor,
          GridColor = GridColor,
          BackGroundColor = BackGroundColor,
          LegendPosition = LegendPosition) +
        ggplot2::theme(legend.title = ggplot2::element_blank()) +
        ggplot2::theme(legend.text = ggplot2::element_text(
          colour = LegendTextColor,
          size = LegendTextSize))
      if(!is.null(XTickMarks)) {
        Plot <- Plot + ggplot2::scale_x_datetime(date_breaks = XTickMarks, labels = scales::date_format("%Y-%m-%d"))
      } else {
        Plot <- Plot + ggplot2::scale_x_datetime(labels = scales::date_format("%Y-%m-%d"))
      }

    } else if(length(unique(PlotData[, get(GroupVariables)])) > 1) {

      # Collapse groups----
      SumTable <- PlotData[, sum(get(TargetVariable)), by = eval(GroupVariables)][order(-V1)]

      # Single group treatment----
      Levels <- as.character(SumTable[1:NumberGroupsDisplay][[1]])
      tempData <- PlotData[get(GroupVariables) %chin% Levels]

      # Other groups----
      if(DisplayOtherGroup) {
        tempData2 <- PlotData[!(get(GroupVariables) %chin% Levels)]
        tempData2 <- tempData2[, eval(GroupVariables) := eval(OtherGroupLabel)]
        if(tolower(Aggregate) == "sum") {
          tempData2 <- tempData2[, sum(get(TargetVariable), na.rm = TRUE), by = c(eval(GroupVariables), eval(DateVariable))]
          tempData <- tempData[, sum(get(TargetVariable), na.rm = TRUE), by = c(eval(GroupVariables), eval(DateVariable))]
          data.table::setnames(tempData2, "V1", eval(TargetVariable))
          data.table::setnames(tempData, "V1", eval(TargetVariable))
        } else if(tolower(Aggregate) == "mean") {
          tempData2 <- tempData2[, mean(get(TargetVariable), na.rm = TRUE), by = c(eval(GroupVariables), eval(DateVariable))]
          tempData <- tempData[, mean(get(TargetVariable), na.rm = TRUE), by = c(eval(GroupVariables), eval(DateVariable))]
          data.table::setnames(tempData2, "V1", eval(TargetVariable))
          data.table::setnames(tempData, "V1", eval(TargetVariable))
        }

        # Combine data
        tempData2 <- data.table::rbindlist(list(tempData,tempData2))
      } else {
        tempData2 <- tempData
      }

      # Grouping variables----
      Plot <- ggplot2::ggplot(
        tempData2,
        ggplot2::aes(x = get(DateVariable), y = get(TargetVariable), color = get(GroupVariables))) +
        ggplot2::geom_line() +
        ggplot2::theme(legend.title=ggplot2::element_blank()) +
        ggplot2::xlab(DateVariable) + ggplot2::ylab(TargetVariable) +
        ChartTheme(
          Size = TextSize,
          AngleX = AngleX,
          AngleY = AngleY,
          ChartColor = ChartColor,
          BorderColor = BorderColor,
          TextColor = TextColor,
          GridColor = GridColor,
          BackGroundColor = BackGroundColor,
          LegendPosition = LegendPosition) +
        ggplot2::theme(legend.title = ggplot2::element_blank()) +
        ggplot2::theme(legend.text = ggplot2::element_text(
          colour = LegendTextColor,
          size = LegendTextSize))
      if(!is.null(XTickMarks)) {
        Plot <- Plot + ggplot2::scale_x_datetime(date_breaks = XTickMarks, labels = scales::date_format("%Y-%m-%d"))
      } else {
        Plot <- Plot + ggplot2::scale_x_datetime(labels = scales::date_format("%Y-%m-%d"))
      }
    } else {
      Plot <- ggplot2::ggplot(
        PlotData,
        ggplot2::aes(x = PlotData[, get(DateVariable)])) +
        ggplot2::geom_line(ggplot2::aes(y = PlotData[[eval(TargetVariable)]]), color = Color, lwd = LineWidth) +
        ggplot2::xlab(DateVariable) + ggplot2::ylab(TargetVariable) +
        ChartTheme(
          Size = TextSize,
          AngleX = AngleX,
          AngleY = AngleY,
          ChartColor = ChartColor,
          BorderColor = BorderColor,
          TextColor = TextColor,
          GridColor = GridColor,
          BackGroundColor = BackGroundColor,
          LegendPosition = LegendPosition)
      if(!is.null(XTickMarks)) {
        Plot <- Plot + ggplot2::scale_x_datetime(date_breaks = XTickMarks, labels = scales::date_format("%Y-%m-%d"))
      } else {
        Plot <- Plot +
          ggplot2::scale_x_datetime(labels = scales::date_format("%Y-%m-%d"))
      }
    }
  } else {

    # No grouping variables----
    Plot <- ggplot2::ggplot(
      PlotData,
      ggplot2::aes(x = PlotData[, get(DateVariable)])) +
      ggplot2::geom_line(ggplot2::aes(y = PlotData[[eval(TargetVariable)]]), color = Color, lwd = LineWidth) +
      ggplot2::xlab(DateVariable) + ggplot2::ylab(TargetVariable) +
      ChartTheme(
        Size = TextSize,
        AngleX = AngleX,
        AngleY = AngleY,
        ChartColor = ChartColor,
        BorderColor = BorderColor,
        TextColor = TextColor,
        GridColor = GridColor,
        BackGroundColor = BackGroundColor,
        LegendPosition = LegendPosition)
    if(!is.null(XTickMarks)) {
      Plot <- Plot + ggplot2::scale_x_datetime(date_breaks = XTickMarks, labels = scales::date_format("%Y-%m-%d"))
    } else {
      Plot <- Plot + ggplot2::scale_x_datetime(labels = scales::date_format("%Y-%m-%d"))
    }
  }

  # Return plot object----
  return(Plot)

}

#' AutoBanditSarima2x2LagMA
#' @param Output asdf
#' @noRd
AutoBanditSarima2x2LagMA <- function(Output) {

  # Metric selection
  ErrorMetric <- Output$PerformanceGrid$Blended_MAE[!Output$PerformanceGrid$Lags %in% -7]

  # Built plot
  LagsMAPlot <- ggplot2::ggplot(
    data = Output$PerformanceGrid[!Output$PerformanceGrid$Lags %in% -7],
    ggplot2::aes(x = Output$PerformanceGrid$Lags[!Output$PerformanceGrid$Lags %in% -7],
                 y = Output$PerformanceGrid$MovingAverages[!Output$PerformanceGrid$Lags %in% -7],
                 fill = ErrorMetric)) +
    ggplot2::geom_tile() + ChartTheme(ChartColor = "gray25") +
    ggplot2::ylab("Moving AVerages") + ggplot2::xlab("Lags") +
    ggplot2::theme(legend.title = ggplot2::element_blank()) +
    ggplot2::labs(title = "MAE by Lags and Moving Average") +
    ggplot2::scale_fill_gradient(low = "darkblue", high = "green") +
    ggplot2::theme(legend.key.size = ggplot2::unit(1.75, "cm"))

  # Return
  return(LagsMAPlot)
}
