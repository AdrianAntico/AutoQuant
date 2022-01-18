#' @title AddFacet
#'
#' @description Add up to two facet variables for plots
#'
#' @author Adrian Antico
#' @family Graphics
#'
#' @param data Source data.table
#' @param ShapColNames Names of the columns that contain shap values you want included
#' @param FacetVar1 Column name
#' @param FacetVar2 Column name
#' @param AggMethod A string for aggregating shapely values for importances. Choices include, 'mean', 'absmean', 'meanabs', 'sd', 'median', 'absmedian', 'medianabs'
#' @param TopN The number of variables to plot
#' @param Debug = FALSE
#'
#' @export
AddFacet <- function(p, fv1=NULL, fv2=NULL, Exclude = 'None', Debug = FALSE) {
  if(length(fv1) != 0 && fv1 != Exclude && length(fv2) != 0 && fv2 != Exclude) {
    if(Debug) print('FacetVar1 and FacetVar2')
    p <- p + ggplot2::facet_grid(get(fv1) ~ get(fv2))
  } else if(length(fv1) != 0 && fv1 == Exclude) {
    if(Debug) print('FacetVar1')
    p <- p + ggplot2::facet_wrap(~ get(fv1))
  } else if(length(fv2) != 0 && fv2 == Exclude) {
    if(Debug) print('FacetVar2')
    p <- p + ggplot2::facet_wrap(~ get(fv2))
  }
  return(eval(p))
}

#' @title ShapImportancePlot
#'
#' @description Generate Variable Importance Plots using Shapely Values of given data set
#'
#' @author Adrian Antico
#' @family Model Insights
#'
#' @param data Source data.table
#' @param ShapColNames Names of the columns that contain shap values you want included
#' @param FacetVar1 Column name
#' @param FacetVar2 Column name
#' @param AggMethod A string for aggregating shapely values for importances. Choices include, 'mean', 'absmean', 'meanabs', 'sd', 'median', 'absmedian', 'medianabs'
#' @param TopN The number of variables to plot
#' @param Debug = FALSE
#'
#' @export
ShapImportancePlot <- function(data,
                               ShapColNames = NULL,
                               FacetVar1=NULL,
                               FacetVar2=NULL,
                               AggMethod = 'mean',
                               TopN = 25,
                               Debug = FALSE) {

  # Debug
  if(Debug) {
    print(paste0('AggMethod = ', AggMethod))
    print('Starting ShapImportancePlot')
    print(paste0('is data missing? ', missing(data)))
    if(!missing(data)) print(paste0(' and is null? ', is.null(data)))
    print('ShapColNames Next')
    print(ShapColNames)
  }

  # Arg checks
  if(length(ShapColNames) == 0) stop('ShapColNames cannot be NULL nor zero-length vectors')
  if(length(FacetVar1) != 0 && !FacetVar1 %in% names(data)) stop('FacetVar1 not in names(data)')
  if(length(FacetVar2) != 0 && !FacetVar2 %in% names(data)) stop('FacetVar2 not in names(data)')
  if(!AggMethod %in% c('mean', 'absmean', 'meanabs', 'sd', 'median', 'absmedian', 'medianabs')) stop("AggMethod must be in c('mean', 'absmean', 'meanabs', 'sd', 'median', 'absmedian', 'medianabs')")
  if(TopN < 1) stop('TopN cannot be less than 1')

  # Subset columns
  temp <- data[, .SD, .SDcols = c(ShapColNames, FacetVar1, FacetVar2)]

  # Aggregate; Melt; Subset to TopN count
  #
  #   TopN rows: half from highest shap values, remainder from lowest negative shap values
  if(AggMethod == 'mean') {
    temp <- temp[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c(ShapColNames)]
    temp1 <- data.table::melt.data.table(data = temp, measure.vars = names(temp), value.name = 'Importance', variable.name = 'Variable')
    gg <- temp1[, .N]
    if(gg > TopN) {
      temp1 <- temp1[c(1:13, (gg - 13L):gg)]
    }
  } else if(AggMethod == 'median') {
    temp <- temp[, lapply(.SD, median, na.rm = TRUE), .SDcols = c(ShapColNames)]
    temp1 <- data.table::melt.data.table(data = temp, measure.vars = names(temp), value.name = 'Importance', variable.name = 'Variable')
    gg <- temp1[, .N]
    if(gg > TopN) {
      temp1 <- temp1[c(1:13, (gg - 13L):gg)]
    }

  # TopN rows: since abs(), doesn't matter
  } else if(AggMethod == 'sd') {
    temp <- temp[, lapply(.SD, sd, na.rm = TRUE), .SDcols = c(ShapColNames)]
    temp1 <- data.table::melt.data.table(data = temp, measure.vars = names(temp), value.name = 'Importance', variable.name = 'Variable')
  } else if(AggMethod == 'absmean') {
    temp <- temp[, lapply(.SD, function(x) {
      return(abs(mean(x[which(!is.na(x))])))
    }), .SDcols = c(ShapColNames)]
    temp1 <- data.table::melt.data.table(data = temp, measure.vars = names(temp), value.name = 'Importance', variable.name = 'Variable')
  } else if(AggMethod == 'meanabs') {
    temp <- temp[, lapply(.SD, function(x) {
      return(mean(abs(x[which(!is.na(x))])))
    }), .SDcols = c(ShapColNames)]
    temp1 <- data.table::melt.data.table(data = temp, measure.vars = names(temp), value.name = 'Importance', variable.name = 'Variable')
  } else if(AggMethod == 'medianabs') {
    temp <- temp[, lapply(.SD, function(x) {
      return(median(abs(x[which(!is.na(x))])))
    }), .SDcols = c(ShapColNames)]
    temp1 <- data.table::melt.data.table(data = temp, measure.vars = names(temp), value.name = 'Importance', variable.name = 'Variable')
  } else if(AggMethod == 'absmedian') {
    temp <- temp[, lapply(.SD, function(x) {
      return(abs(median(x[which(!is.na(x))])))
    }), .SDcols = c(ShapColNames)]
    temp1 <- data.table::melt.data.table(data = temp, measure.vars = names(temp), value.name = 'Importance', variable.name = 'Variable')
  }

  # Build VI Plot
  p <- RemixAutoML:::VI_Plot(VI_Data = temp1, Type = 'catboost', TopN = TopN)
  p <- p + ggplot2::labs(title = 'Shapely Variable Importance', caption = 'RemixAutoML')


  # Y-Axis Label (its the Y-Axis because of coord_flip() in the VI_Plot() function
  if(AggMethod == 'mean') {
    p <- p + ggplot2::ylab(paste0('Mean(shapley values)'))
  } else if(AggMethod == 'absmean') {
    p <- p + ggplot2::ylab(paste0('Abs[mean(shapely values)]'))
  } else if(AggMethod == 'meanabs') {
    p <- p + ggplot2::ylab('Mean[abs(shapley values)]')
  } else if(AggMethod == 'sd') {
    p <- p + ggplot2::ylab('StDev(shapley values)')
  } else if(AggMethod == 'median') {
    p <- p + ggplot2::ylab('Median(shapley values)')
  } else if(AggMethod == 'absmedian') {
    p <- p + ggplot2::ylab('Abs[median(shapley values)]')
  } else if(AggMethod == 'medianabs') {
    p <- p + ggplot2::ylab('Median[abs(shapley values)]')
  }

  # TODO: Add faceting (returns no faceting in none was requested)
  # p <- AddFacet(p, fv1=FacetVar1, fv2=FacetVar2, Exclude = 'None', Debug = FALSE)

  # eval(p) to ensure it can save in list
  return(eval(p))
}

#' @noRd
LowerTriangle <- function(x, Diag = FALSE) {
  if(is.null(x)) {
    return(x)
  }
  x[upper.tri(x)] <- NA
  if(!Diag) {
    diag(x) <- NA
  }
  return(x)
}

#' @noRd
UpperTriangle <- function(x, Diag = FALSE) {
  if(is.null(x)) return(x)
  x[lower.tri(x)] <- NA
  if(!Diag) diag(x) <- NA
  return(x)
}

#' @noRd
CorrMatrixPlotBase <- function(x,
                               LegendName = 'Strength',
                               method = "square",
                               type = "upper",
                               Diag = FALSE,
                               colors = c("red", "green", "blue"),
                               outline.color = "gray",
                               lab = FALSE,
                               lab_col = "black",
                               lab_size = 4,
                               p.mat = NULL,
                               sig.level = 0.05,
                               insig = "pch",
                               digits = 2) {

  x <- round(x = x, digits = digits)
  if(type == "lower") {
    x <- LowerTriangle(x, Diag)
    p.mat <- LowerTriangle(p.mat, Diag)
  } else if(type == "upper") {
    x <- UpperTriangle(x, Diag)
    p.mat <- UpperTriangle(p.mat, Diag)
  }

  x <- reshape2::melt(x, na.rm = TRUE)
  colnames(x) <- c("Var1", "Var2", "value")
  x$pvalue <- rep(NA, nrow(x))
  x$signif <- rep(NA, nrow(x))

  if(!is.null(p.mat)) {
    p.mat <- reshape2::melt(p.mat, na.rm = TRUE)
    x$coef <- x$value
    x$pvalue <- p.mat$value
    x$signif <- as.numeric(p.mat$value <= sig.level)
    p.mat <- subset(p.mat, p.mat$value > sig.level)
    if(insig == "blank") {
      x$value <- x$value * x$signif
    }
  }

  x$abs_x <- abs(x$value) * 10
  p <- ggplot2::ggplot(data = x, mapping = ggplot2::aes_string(x = "Var1", y = "Var2", fill = "value"))

  if(method == "square") {
    p <- p + ggplot2::geom_tile(color = outline.color)
  } else if(method == "circle") {
    p <- p + ggplot2::geom_point(
      color = outline.color,
      shape = 21,
      ggplot2::aes_string(size = "abs_corr")) +
      ggplot2::scale_size(range = c(4,10)) +
      ggplot2::guides(size = FALSE)
  }

  p <- p + ggplot2::scale_fill_gradient2(
    low = colors[1],
    high = colors[3],
    mid = colors[2],
    midpoint = 0,
    limit = c(-1, 1),
    space = "Lab",
    name = LegendName)

  label <- round(x = x[, "value"], digits = digits)
  if(lab) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes_string(
        x = "Var1",
        y = "Var2"),
      color = lab_col,
      size = lab_size,
      label = label)
  }
  p
}

#' @title CorrMatrixPlot
#'
#' @description Build a violin plot by simply passing arguments to a single function. It will sample your data using SampleSize number of rows. Sampled data is randomized.
#'
#' @family Graphics
#'
#' @author Adrian Antico
#'
#' @param data Source data.table
#' @param CorrVars Column names of variables you want included in the correlation matrix
#' @param CorrMethod 'pearson', 'spearman', 'kendall'
#' @param FacetVar1 Column name of facet variable 1. If NULL then ignored
#' @param FacetVar2 Column name of facet variable 2. If NULL then ignored
#' @param SampleSize An integer for the number of rows to use. Sampled data is randomized. If NULL then ignored
#' @param DimnamesMaxNChar Default 20
#' @param CorrValueTextSize Default 5.5
#' @param TextSize 14
#' @param AngleX 90
#' @param AngleY 0
#' @param ChartColor 'lightsteelblue'
#' @param BorderColor 'darkblue'
#' @param TextColor 'darkblue'
#' @param GridColor 'white'
#' @param BackGroundColor 'gray95'
#' @param SubTitleColor 'darkblue'
#' @param LegendPosition 'bottom'
#' @param LegendBorderSize 0.50
#' @param LegendLineType 'solid'
#' @param Debug FALSE
#'
#' @export
CorrMatrixPlot <- function(data = NULL,
                           CorrVars = NULL,
                           CorrMethod = 'pearson',
                           FacetVar1 = NULL,
                           FacetVar2 = NULL,
                           SampleSize = 1000000L,
                           DimnamesMaxNChar = 20L,
                           CorrValueTextSize = 5.5,
                           TextSize = 12,
                           AngleX = 90,
                           AngleY = 0,
                           ChartColor = 'lightsteelblue1',
                           BorderColor = 'darkblue',
                           TextColor = 'darkblue',
                           GridColor = 'white',
                           BackGroundColor = 'gray95',
                           SubTitleColor = 'blue',
                           LegendPosition = 'top',
                           LegendBorderSize = 0.50,
                           LegendLineType = 'solid',
                           Debug = FALSE) {

  # Cap number of records
  if(!is.null(SampleSize)) if(data[,.N] > SampleSize) data <- data[order(runif(.N))][seq_len(SampleSize)]

  # Create correlation matrix
  CorrMatrix <- cor(data[, .SD, .SDcols = c(CorrVars)], use = 'pairwise.complete.obs')
  colnames(CorrMatrix) <- rownames(CorrMatrix) <- substr(rownames(CorrMatrix), 1L, DimnamesMaxNChar)

  # Create plot
  options(warn = -1)
  p1 <- suppressMessages(CorrMatrixPlotBase(
    x = CorrMatrix,
    method = 'square',
    type = 'upper',
    lab = TRUE,
    lab_col = 'white',
    lab_size = 5.5,
    Diag = FALSE,
    colors = c('darkred','white','darkblue'),
    outline.color = 'gray50'))
  options(warn = 1)

  # Add ChartTheme
  if(Debug) print('ChartTheme')
  p1 <- p1 + RemixAutoML::ChartTheme(
    Size = TextSize,
    AngleX = AngleX,
    AngleY = AngleY,
    ChartColor = ChartColor,
    BorderColor = BorderColor,
    TextColor = TextColor,
    GridColor = GridColor,
    BackGroundColor = BackGroundColor,
    SubTitleColor = SubTitleColor,
    LegendPosition = 'top',
    LegendBorderSize = LegendBorderSize,
    LegendLineType = LegendLineType)

  # Make legend thinnier and longer than default
  p1 <- p1 + ggplot2::theme(legend.key.width = ggplot2::unit(2, 'cm'))
  p1 <- p1 + ggplot2::theme(legend.key.height = ggplot2::unit(0.60, 'cm'))

  # Labels / Title / Caption
  p1 <- p1 + ggplot2::xlab(label = NULL) + ggplot2::ylab(NULL)
  p1 <- p1 + ggplot2::labs(title = paste0('Correlation Matrix with Correl Method: ', CorrMethod), caption = 'RemixAutoML')

  # Return plot
  return(eval(p1))
}

#' @title ViolinPlot
#'
#' @description Build a violin plot by simply passing arguments to a single function. It will sample your data using SampleSize number of rows. Sampled data is randomized.
#'
#' @family Graphics
#'
#' @author Adrian Antico
#'
#' @param data Source data.table
#' @param XVar Column name of X-Axis variable. If NULL then ignored
#' @param YVar Column name of Y-Axis variable. If NULL then ignored
#' @param FacetVar1 Column name of facet variable 1. If NULL then ignored
#' @param FacetVar2 Column name of facet variable 2. If NULL then ignored
#' @param SampleSize An integer for the number of rows to use. Sampled data is randomized. If NULL then ignored
#' @param FillColor 'gray'
#' @param YTicks Choose from 'Default', 'Percentiles', 'Every 5th percentile', 'Deciles', 'Quantiles', 'Quartiles'
#' @param XTicks Choose from 'Default', '1 year', '1 day', '3 day', '1 week', '2 week', '1 month', '3 month', '6 month', '2 year', '5 year', '10 year', '1 minute', '15 minutes', '30 minutes', '1 hour', '3 hour', '6 hour', '12 hour'
#' @param TextSize 14
#' @param AngleX 90
#' @param AngleY 0
#' @param ChartColor 'lightsteelblue'
#' @param BorderColor 'darkblue'
#' @param TextColor 'darkblue'
#' @param GridColor 'white'
#' @param BackGroundColor 'gray95'
#' @param SubTitleColor 'darkblue'
#' @param LegendPosition 'bottom'
#' @param LegendBorderSize 0.50
#' @param LegendLineType 'solid'
#' @param Debug FALSE
#'
#' @export
ViolinPlot <- function(data = NULL,
                       XVar = NULL,
                       YVar = NULL,
                       FacetVar1 = NULL,
                       FacetVar2 = NULL,
                       SampleSize = 1000000L,
                       FillColor = 'gray',
                       YTicks = 'Default',
                       XTicks = 'Default',
                       TextSize = 12,
                       AngleX = 90,
                       AngleY = 0,
                       ChartColor = 'lightsteelblue1',
                       BorderColor = 'darkblue',
                       TextColor = 'darkblue',
                       GridColor = 'white',
                       BackGroundColor = 'gray95',
                       SubTitleColor = 'blue',
                       LegendPosition = 'bottom',
                       LegendBorderSize = 0.50,
                       LegendLineType = 'solid',
                       Debug = FALSE) {

  # Cap number of records
  if(!is.null(SampleSize)) if(data[,.N] > SampleSize) data <- data[order(runif(.N))][seq_len(SampleSize)]

  # Used multiple times
  check1 <- length(XVar) != 0 && length(YVar) != 0 && (lubridate::is.Date(data[['XVar']]) || lubridate::is.POSIXct(data[['XVar']]))

  # Create base plot object
  if(Debug) print('Create Plot with only data')
  if(check1) {
    p1 <- ggplot2::ggplot(data = data, ggplot2::aes(x = get(XVar), y = get(YVar), group = get(XVar)))
  } else if(length(YVar) != 0) {
    p1 <- ggplot2::ggplot(data = data, ggplot2::aes(y = get(YVar), x = ""))
  } else if(length(XVar) != 0) {
    p1 <- ggplot2::ggplot(data = data, ggplot2::aes(y = get(XVar), x = ""))
  } else {
    stop('XVar and YVar cannot both be NULL')
  }

  # Violin Plot Call
  if(Debug) print('Create ViolinPlot')
  p1 <- p1 + ggplot2::geom_violin(fill = FillColor)

  # Add Horizontal Line for Mean Y
  if(Debug) print('Create Plot Horizontal Line')
  if(!is.null(YVar)) {
    p1 <- p1 + ggplot2::geom_hline(color = 'blue', yintercept = eval(mean(data[[eval(YVar)]], na.rm = TRUE)))
  } else {
    p1 <- p1 + ggplot2::geom_hline(color = 'blue', yintercept = eval(mean(data[[eval(XVar)]], na.rm = TRUE)))
  }

  # Create Plot labs
  if(Debug) print('Create Plot labs')
  if(check1) {
    p1 <- p1 + ggplot2::labs(title = 'Distribution over Time', subtitle = 'Blue line = mean(Y)', caption = 'RemixAutoML')
  } else {
    p1 <- p1 + ggplot2::labs(title = 'ViolinPlot', subtitle = 'Blue line = mean(Y)', caption = 'RemixAutoML')
  }

  # Labels
  if(check1) {
    p1 <- p1 + ggplot2::ylab(YVar)
    p1 <- p1 + ggplot2::xlab(XVar)
  } else if(length(YVar) != 0) {
    p1 <- p1 + ggplot2::ylab(NULL)
    p1 <- p1 + ggplot2::xlab(YVar)
  } else {
    p1 <- p1 + ggplot2::ylab(NULL)
    p1 <- p1 + ggplot2::xlab(XVar)
  }

  # Add faceting (returns no faceting in none was requested)
  p1 <- AddFacet(p1, fv1=FacetVar1, fv2=FacetVar2, Exclude = 'None', Debug = FALSE)

  # Add ChartTheme
  if(Debug) print('ChartTheme')
  p1 <- p1 + RemixAutoML::ChartTheme(
    Size = TextSize,
    AngleX = AngleX,
    AngleY = AngleY,
    ChartColor = ChartColor,
    BorderColor = BorderColor,
    TextColor = TextColor,
    GridColor = GridColor,
    BackGroundColor = BackGroundColor,
    SubTitleColor = SubTitleColor,
    LegendPosition = LegendPosition,
    LegendBorderSize = LegendBorderSize,
    LegendLineType = LegendLineType)

  # Define Tick Marks
  if(Debug) print('YTicks Update')
  if('Percentiles' %in% YTicks) {
    YTicks <- data[, quantile(round(get(YVar), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
  } else if('Every 5th percentile' %in% YTicks) {
    YTicks <- data[, quantile(round(get(YVar), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
    YTicks <- YTicks[c(seq(6L, length(YTicks)-1L, 5L))]
  } else if('Deciles' %in% YTicks) {
    YTicks <- data[, quantile(round(get(YVar), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
    YTicks <- YTicks[c(seq(11L, length(YTicks)-1L, 10L))]
  } else if('Quantiles' %in% YTicks) {
    YTicks <- data[, quantile(round(get(YVar), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
    YTicks <- YTicks[c(seq(21L, length(YTicks)-1L, 20L))]
  } else if('Quartiles' %in% YTicks) {
    YTicks <- data[, quantile(round(get(YVar), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
    YTicks <- YTicks[c(seq(26L, length(YTicks)-1L, 25L))]
  } else {
    YTicks <- NULL
  }

  # Add tick marks
  if(length(YTicks) != 0) p1 <- p1 + ggplot2::scale_y_continuous(breaks = as.numeric(YTicks))

  # Add XTicks for Date Case
  if(check1) {
    if(Debug) {print('XTicks'); print(XTicks)}
    date_check <- c("1 year", "1 day", "3 day", "1 week", "2 week", "1 month", "3 month", "6 month", "2 year", "5 year", "10 year", "1 minute", "15 minutes", "30 minutes", "1 hour", "3 hour", "6 hour", "12 hour")
    if(length(XTicks) > 1L && 'Default' %in% XTicks) XTicks <- XTicks[!XTicks %in% 'Default'][1L]
    if(length(XTicks) > 1L) XTicks <- XTicks[1L]
    if(XTicks %in% date_check) {
      p1 <- p1 + suppressMessages(ggplot2::scale_x_date(date_breaks = XTicks))
    }
  }

  # Return plot
  return(eval(p1))
}

#' @title BoxPlot
#'
#' @description Build a box plot by simply passing arguments to a single function. It will sample your data using SampleSize number of rows. Sampled data is randomized.
#'
#' @family Graphics
#'
#' @author Adrian Antico
#'
#' @param data Source data.table
#' @param XVar Column name of X-Axis variable. If NULL then ignored
#' @param YVar Column name of Y-Axis variable. If NULL then ignored
#' @param FacetVar1 Column name of facet variable 1. If NULL then ignored
#' @param FacetVar2 Column name of facet variable 2. If NULL then ignored
#' @param SampleSize An integer for the number of rows to use. Sampled data is randomized. If NULL then ignored
#' @param FillColor 'gray'
#' @param OutlierSize 0.10
#' @param OutlierColor 'blue'
#' @param YTicks Choose from 'Default', 'Percentiles', 'Every 5th percentile', 'Deciles', 'Quantiles', 'Quartiles'
#' @param XTicks Choose from 'Default', '1 year', '1 day', '3 day', '1 week', '2 week', '1 month', '3 month', '6 month', '2 year', '5 year', '10 year', '1 minute', '15 minutes', '30 minutes', '1 hour', '3 hour', '6 hour', '12 hour'
#' @param TextSize 14
#' @param AngleX 90
#' @param AngleY 0
#' @param ChartColor 'lightsteelblue'
#' @param BorderColor 'darkblue'
#' @param TextColor 'darkblue'
#' @param GridColor 'white'
#' @param BackGroundColor 'gray95'
#' @param SubTitleColor 'darkblue'
#' @param LegendPosition 'bottom'
#' @param LegendBorderSize 0.50
#' @param LegendLineType 'solid'
#' @param Debug FALSE
#'
#' @export
BoxPlot <- function(data = NULL,
                    XVar = NULL,
                    YVar = NULL,
                    FacetVar1 = NULL,
                    FacetVar2 = NULL,
                    SampleSize = 1000000L,
                    FillColor = 'gray',
                    OutlierSize = 0.10,
                    OutlierColor = 'blue',
                    YTicks = 'Default',
                    XTicks = 'Default',
                    TextSize = 12,
                    AngleX = 90,
                    AngleY = 0,
                    ChartColor = 'lightsteelblue1',
                    BorderColor = 'darkblue',
                    TextColor = 'darkblue',
                    GridColor = 'white',
                    BackGroundColor = 'gray95',
                    SubTitleColor = 'blue',
                    LegendPosition = 'bottom',
                    LegendBorderSize = 0.50,
                    LegendLineType = 'solid',
                    Debug = FALSE) {

  # Cap number of records
  if(!is.null(SampleSize)) if(data[,.N] > SampleSize) data <- data[order(runif(.N))][seq_len(SampleSize)]

  # Used multiple times
  check1 <- length(XVar) != 0 && length(YVar) != 0 && (lubridate::is.Date(data[['XVar']]) || lubridate::is.POSIXct(data[['XVar']]))

  # Create base plot object
  if(Debug) print('Create Plot with only data')
  if(check1) {
    p1 <- ggplot2::ggplot(data = data, ggplot2::aes(x = get(XVar), y = get(YVar), group = get(XVar)))
  } else if(length(YVar) != 0) {
    p1 <- ggplot2::ggplot(data = data, ggplot2::aes(y = get(YVar)))
  } else if(length(XVar) != 0) {
    p1 <- ggplot2::ggplot(data = data, ggplot2::aes(y = get(XVar)))
  } else {
    stop('XVar and YVar cannot both be NULL')
  }

  # Box Plot Call
  if(Debug) print('Create BoxPlot')
  p1 <- p1 + ggplot2::geom_boxplot(outlier.size = OutlierSize, outlier.colour = OutlierColor, fill = FillColor)

  # Add Horizontal Line for Mean Y
  if(Debug) print('Create Plot Horizontal Line')
  if(!is.null(YVar)) {
    p1 <- p1 + ggplot2::geom_hline(color = 'blue', yintercept = eval(mean(data[[eval(YVar)]], na.rm = TRUE)))
  } else {
    p1 <- p1 + ggplot2::geom_hline(color = 'blue', yintercept = eval(mean(data[[eval(XVar)]], na.rm = TRUE)))
  }

  # Create Plot labs
  if(Debug) print('Create Plot labs')
  if(check1) {
    p1 <- p1 + ggplot2::labs(title = 'Distribution over Time', subtitle = 'Blue line = mean(Y)', caption = 'RemixAutoML')
  } else {
    p1 <- p1 + ggplot2::labs(title = 'BoxPlot', subtitle = 'Blue line = mean(Y)', caption = 'RemixAutoML')
  }

  # Labels
  if(check1) {
    p1 <- p1 + ggplot2::ylab(YVar)
    p1 <- p1 + ggplot2::xlab(XVar)
  } else if(length(YVar) != 0) {
    p1 <- p1 + ggplot2::ylab(NULL)
    p1 <- p1 + ggplot2::xlab(YVar)
  } else {
    p1 <- p1 + ggplot2::ylab(NULL)
    p1 <- p1 + ggplot2::xlab(XVar)
  }

  # Add faceting (returns no faceting in none was requested)
  p1 <- AddFacet(p1, fv1=FacetVar1, fv2=FacetVar2, Exclude = 'None', Debug = FALSE)

  # Add ChartTheme
  if(Debug) print('ChartTheme')
  p1 <- p1 + RemixAutoML::ChartTheme(
    Size = TextSize,
    AngleX = AngleX,
    AngleY = AngleY,
    ChartColor = ChartColor,
    BorderColor = BorderColor,
    TextColor = TextColor,
    GridColor = GridColor,
    BackGroundColor = BackGroundColor,
    SubTitleColor = SubTitleColor,
    LegendPosition = LegendPosition,
    LegendBorderSize = LegendBorderSize,
    LegendLineType = LegendLineType)

  # Define Tick Marks
  if(Debug) print('YTicks Update')
  if('Percentiles' %in% YTicks) {
    YTicks <- data[, quantile(round(get(YVar), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
  } else if('Every 5th percentile' %in% YTicks) {
    YTicks <- data[, quantile(round(get(YVar), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
    YTicks <- YTicks[c(seq(6L, length(YTicks)-1L, 5L))]
  } else if('Deciles' %in% YTicks) {
    YTicks <- data[, quantile(round(get(YVar), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
    YTicks <- YTicks[c(seq(11L, length(YTicks)-1L, 10L))]
  } else if('Quantiles' %in% YTicks) {
    YTicks <- data[, quantile(round(get(YVar), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
    YTicks <- YTicks[c(seq(21L, length(YTicks)-1L, 20L))]
  } else if('Quartiles' %in% YTicks) {
    YTicks <- data[, quantile(round(get(YVar), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
    YTicks <- YTicks[c(seq(26L, length(YTicks)-1L, 25L))]
  } else {
    YTicks <- NULL
  }

  # Add tick marks
  if(length(YTicks) != 0) p1 <- p1 + ggplot2::scale_y_continuous(breaks = as.numeric(YTicks))

  # Add XTicks for Date Case
  if(check1) {
    if(Debug) {print('XTicks'); print(XTicks)}
    date_check <- c("1 year", "1 day", "3 day", "1 week", "2 week", "1 month", "3 month", "6 month", "2 year", "5 year", "10 year", "1 minute", "15 minutes", "30 minutes", "1 hour", "3 hour", "6 hour", "12 hour")
    if(length(XTicks) > 1L && 'Default' %in% XTicks) XTicks <- XTicks[!XTicks %in% 'Default'][1L]
    if(length(XTicks) > 1L) XTicks <- XTicks[1L]
    if(XTicks %in% date_check) {
      p1 <- p1 + suppressMessages(ggplot2::scale_x_date(date_breaks = XTicks))
    }
  }

  # Return plot
  return(eval(p1))
}

#' @title BarPlot
#'
#' @description Build a bar plot by simply passing arguments to a single function. It will sample your data using SampleSize number of rows. Sampled data is randomized.
#'
#' @family Graphics
#'
#' @author Adrian Antico
#'
#' @param data Source data.table
#' @param XVar Column name of X-Axis variable. If NULL then ignored
#' @param YVar Column name of Y-Axis variable. If NULL then ignored
#' @param FacetVar1 Column name of facet variable 1. If NULL then ignored
#' @param FacetVar2 Column name of facet variable 2. If NULL then ignored
#' @param SampleSize An integer for the number of rows to use. Sampled data is randomized. If NULL then ignored
#' @param FillColor 'gray'
#' @param OutlierSize 0.10
#' @param OutlierColor 'blue'
#' @param YTicks Choose from 'Default', 'Percentiles', 'Every 5th percentile', 'Deciles', 'Quantiles', 'Quartiles'
#' @param XTicks Choose from 'Default', '1 year', '1 day', '3 day', '1 week', '2 week', '1 month', '3 month', '6 month', '2 year', '5 year', '10 year', '1 minute', '15 minutes', '30 minutes', '1 hour', '3 hour', '6 hour', '12 hour'
#' @param TextSize 14
#' @param AngleX 90
#' @param AngleY 0
#' @param ChartColor 'lightsteelblue'
#' @param BorderColor 'darkblue'
#' @param TextColor 'darkblue'
#' @param GridColor 'white'
#' @param BackGroundColor 'gray95'
#' @param SubTitleColor 'darkblue'
#' @param LegendPosition 'bottom'
#' @param LegendBorderSize 0.50
#' @param LegendLineType 'solid'
#' @param Debug FALSE
#'
#' @export
BarPlot <- function(data = NULL,
                    XVar = NULL,
                    YVar = NULL,
                    ColorVar = NULL,
                    FacetVar1 = NULL,
                    FacetVar2 = NULL,
                    SampleSize = 1000000L,
                    FillColor = 'gray',
                    YTicks = 'Default',
                    XTicks = 'Default',
                    TextSize = 12,
                    AngleX = 90,
                    AngleY = 0,
                    ChartColor = 'lightsteelblue1',
                    BorderColor = 'darkblue',
                    TextColor = 'darkblue',
                    GridColor = 'white',
                    BackGroundColor = 'gray95',
                    SubTitleColor = 'blue',
                    LegendPosition = 'bottom',
                    LegendBorderSize = 0.50,
                    LegendLineType = 'solid',
                    Debug = FALSE) {

  # Cap number of records
  if(!is.null(SampleSize)) if(data[,.N] > SampleSize) data <- data[order(runif(.N))][seq_len(SampleSize)]

  # Used multiple times
  check1 <- length(XVar) != 0 && length(YVar) != 0
  check2 <- length(XVar) == 0 && length(YVar) != 0
  check3 <- length(XVar) != 0 && length(YVar) == 0

  # Create base plot object
  if(Debug) print('Create Plot with only data')
  if(check1) {
    if(length(ColorVar) != 0) {
      p1 <- ggplot2::ggplot(data = data, ggplot2::aes(x = get(XVar), y = get(YVar), fill = as.factor(get(ColorVar))))
      p1 <- p1 + ggplot2::geom_bar(stat = 'identity')
      p1 <- p1 + ggplot2::labs(fill = eval(ColorVar))
      p1 <- p1 + ggplot2::xlab(eval(YVar)) + ggplot2::ylab(eval(YVar))
    } else {
      p1 <- ggplot2::ggplot(data = data, ggplot2::aes(x = get(XVar), y = get(YVar)))
      p1 <- p1 + ggplot2::geom_bar(stat = 'identity', fill = FillColor)
      p1 <- p1 + ggplot2::xlab(eval(YVar)) + ggplot2::ylab(eval(YVar))
    }
  } else if(check2) {
    if(length(ColorVar) != 0) {
      p1 <- ggplot2::ggplot(data = data, ggplot2::aes(x = get(YVar), fill = as.factor(get(ColorVar))))
      if(length(unique(data[[eval(YVar)]])) <= 10L) {
        p1 <- p1 + ggplot2::geom_bar(stat = 'count')
      } else {
        p1 <- p1 + ggplot2::geom_bar(stat = 'bin')
      }
      p1 <- p1 + ggplot2::labs(fill = eval(ColorVar))
      p1 <- p1 + ggplot2::xlab(eval(YVar))
    } else {
      p1 <- ggplot2::ggplot(data = data, ggplot2::aes(x = get(YVar)))
      if(length(unique(data[[eval(YVar)]])) <= 10L) {
        p1 <- p1 + ggplot2::geom_bar(stat = 'count', fill = FillColor)
      } else {
        p1 <- p1 + ggplot2::geom_bar(stat = 'bin', fill = FillColor)
      }
      p1 <- p1 + ggplot2::xlab(eval(YVar))
    }
  } else if(check3) {
    if(length(ColorVar) != 0) {
      p1 <- ggplot2::ggplot(data = data, ggplot2::aes(x = get(XVar), fill = as.factor(get(ColorVar))))
      if(length(unique(data[[eval(XVar)]])) <= 10L) {
        p1 <- p1 + ggplot2::geom_bar(stat = 'count')
      } else {
        p1 <- p1 + ggplot2::geom_bar(stat = 'bin')
      }
      p1 <- p1 + ggplot2::labs(fill = eval(ColorVar))
      p1 <- p1 + ggplot2::xlab(eval(XVar))
    } else {
      p1 <- ggplot2::ggplot(data = data, ggplot2::aes(x = get(XVar)))
      if(length(unique(data[[eval(XVar)]])) <= 10L) {
        p1 <- p1 + ggplot2::geom_bar(stat = 'count', fill = FillColor)
      } else {
        p1 <- p1 + ggplot2::geom_bar(stat = 'bin', fill = FillColor)
      }
      p1 <- p1 + ggplot2::xlab(eval(XVar))
    }
  } else {
    stop('XVar and YVar cannot both be NULL')
  }

  # Create Plot labs
  if(Debug) print('Create Plot labs')
  p1 <- p1 + ggplot2::labs(title = 'Bar Plot', caption = 'RemixAutoML')

  # Add faceting (returns no faceting in none was requested)
  p1 <- AddFacet(p1, fv1=FacetVar1, fv2=FacetVar2, Exclude = 'None', Debug = FALSE)

  # Add ChartTheme
  if(Debug) print('ChartTheme')
  p1 <- p1 + RemixAutoML::ChartTheme(
    Size = TextSize,
    AngleX = AngleX,
    AngleY = AngleY,
    ChartColor = ChartColor,
    BorderColor = BorderColor,
    TextColor = TextColor,
    GridColor = GridColor,
    BackGroundColor = BackGroundColor,
    SubTitleColor = SubTitleColor,
    LegendPosition = LegendPosition,
    LegendBorderSize = LegendBorderSize,
    LegendLineType = LegendLineType)

  # Define Tick Marks
  if(Debug) print('YTicks Update')
  if('Percentiles' %in% YTicks) {
    YTicks <- data[, quantile(round(get(YVar), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
  } else if('Every 5th percentile' %in% YTicks) {
    YTicks <- data[, quantile(round(get(YVar), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
    YTicks <- YTicks[c(seq(6L, length(YTicks)-1L, 5L))]
  } else if('Deciles' %in% YTicks) {
    YTicks <- data[, quantile(round(get(YVar), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
    YTicks <- YTicks[c(seq(11L, length(YTicks)-1L, 10L))]
  } else if('Quantiles' %in% YTicks) {
    YTicks <- data[, quantile(round(get(YVar), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
    YTicks <- YTicks[c(seq(21L, length(YTicks)-1L, 20L))]
  } else if('Quartiles' %in% YTicks) {
    YTicks <- data[, quantile(round(get(YVar), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
    YTicks <- YTicks[c(seq(26L, length(YTicks)-1L, 25L))]
  } else {
    YTicks <- NULL
  }

  # Add tick marks
  if(length(YTicks) != 0) p1 <- p1 + ggplot2::scale_y_continuous(breaks = as.numeric(YTicks))

  # Add XTicks for Date Case
  if(check1) {
    if(Debug) {print('XTicks'); print(XTicks)}
    date_check <- c("1 year", "1 day", "3 day", "1 week", "2 week", "1 month", "3 month", "6 month", "2 year", "5 year", "10 year", "1 minute", "15 minutes", "30 minutes", "1 hour", "3 hour", "6 hour", "12 hour")
    if(length(XTicks) > 1L && 'Default' %in% XTicks) XTicks <- XTicks[!XTicks %in% 'Default'][1L]
    if(length(XTicks) > 1L) XTicks <- XTicks[1L]
    if(XTicks %in% date_check) {
      p1 <- p1 + suppressMessages(ggplot2::scale_x_date(date_breaks = XTicks))
    }
  }

  # Return plot
  return(eval(p1))
}

#' @title HistPlot
#'
#' @description Build a histogram plot by simply passing arguments to a single function. It will sample your data using SampleSize number of rows. Sampled data is randomized.
#'
#' @family Graphics
#'
#' @author Adrian Antico
#'
#' @param data Source data.table
#' @param XVar Column name of X-Axis variable. If NULL then ignored
#' @param YVar Column name of Y-Axis variable. If NULL then ignored
#' @param FacetVar1 Column name of facet variable 1. If NULL then ignored
#' @param FacetVar2 Column name of facet variable 2. If NULL then ignored
#' @param SampleSize An integer for the number of rows to use. Sampled data is randomized. If NULL then ignored
#' @param Bins = 30
#' @param FillColor 'gray'
#' @param OutlierSize 0.10
#' @param OutlierColor 'blue'
#' @param YTicks Choose from 'Default', 'Percentiles', 'Every 5th percentile', 'Deciles', 'Quantiles', 'Quartiles'
#' @param XTicks Choose from 'Default', '1 year', '1 day', '3 day', '1 week', '2 week', '1 month', '3 month', '6 month', '2 year', '5 year', '10 year', '1 minute', '15 minutes', '30 minutes', '1 hour', '3 hour', '6 hour', '12 hour'
#' @param TextSize 14
#' @param AngleX 90
#' @param AngleY 0
#' @param ChartColor 'lightsteelblue'
#' @param BorderColor 'darkblue'
#' @param TextColor 'darkblue'
#' @param GridColor 'white'
#' @param BackGroundColor 'gray95'
#' @param SubTitleColor 'darkblue'
#' @param LegendPosition 'bottom'
#' @param LegendBorderSize 0.50
#' @param LegendLineType 'solid'
#' @param Debug FALSE
#'
#' @export
HistPlot <- function(data = NULL,
                     XVar = NULL,
                     YVar = NULL,
                     FacetVar1 = NULL,
                     FacetVar2 = NULL,
                     SampleSize = 1000000L,
                     Bins = 30,
                     FillColor = 'gray',
                     OutlierSize = 0.10,
                     OutlierColor = 'blue',
                     YTicks = 'Default',
                     XTicks = 'Default',
                     TextSize = 12,
                     AngleX = 90,
                     AngleY = 0,
                     ChartColor = 'lightsteelblue1',
                     BorderColor = 'darkblue',
                     TextColor = 'darkblue',
                     GridColor = 'white',
                     BackGroundColor = 'gray95',
                     SubTitleColor = 'blue',
                     LegendPosition = 'bottom',
                     LegendBorderSize = 0.50,
                     LegendLineType = 'solid',
                     Debug = FALSE) {

  # Cap number of records
  if(!is.null(SampleSize)) if(data[,.N] > SampleSize) data <- data[order(runif(.N))][seq_len(SampleSize)]

  # Define Plotting Variable
  if(Debug) print('YTicks Update')
  if(length(YVar) == 0) YVar <- XVar
  if(length(YVar) == 0) stop('XVar and YVar cannot both be NULL')

  # Create base plot object
  if(Debug) print('Create Plot with only data')
  p1 <- ggplot2::ggplot(data = data, ggplot2::aes(x = get(YVar)))

  # Box Plot Call
  if(Debug) print('Create Histogram')
  p1 <- p1 + ggplot2::geom_histogram(bin = Bins)

  # Add Horizontal Line for Mean Y
  if(Debug) print('Create Plot Horizontal Line')
  p1 <- p1 + ggplot2::geom_vline(color = 'blue', xintercept = eval(mean(data[[eval(YVar)]], na.rm = TRUE)))

  # Create Plot labs
  if(Debug) print('Create Plot labs')
  p1 <- p1 + ggplot2::labs(title = 'Histogram', subtitle = 'Blue line = mean(X)', caption = 'RemixAutoML')

  # Labels
  p1 <- p1 + ggplot2::ylab(NULL)
  p1 <- p1 + ggplot2::xlab(YVar)

  # Add faceting (returns no faceting in none was requested)
  p1 <- AddFacet(p1, fv1=FacetVar1, fv2=FacetVar2, Exclude = 'None', Debug = FALSE)

  # Add ChartTheme
  if(Debug) print('ChartTheme')
  p1 <- p1 + RemixAutoML::ChartTheme(
    Size = TextSize,
    AngleX = AngleX,
    AngleY = AngleY,
    ChartColor = ChartColor,
    BorderColor = BorderColor,
    TextColor = TextColor,
    GridColor = GridColor,
    BackGroundColor = BackGroundColor,
    SubTitleColor = SubTitleColor,
    LegendPosition = LegendPosition,
    LegendBorderSize = LegendBorderSize,
    LegendLineType = LegendLineType)

  # Define Tick Marks
  if(Debug) print('YTicks Update')
  if(length(YTicks) == 0) YTicks <- XTicks
  if(length(YTicks) != 0) {
    if('Percentiles' %in% YTicks) {
      YTicks <- data[, quantile(round(get(YVar), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
    } else if('Every 5th percentile' %in% YTicks) {
      YTicks <- data[, quantile(round(get(YVar), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
      YTicks <- YTicks[c(seq(6L, length(YTicks)-1L, 5L))]
    } else if('Deciles' %in% YTicks) {
      YTicks <- data[, quantile(round(get(YVar), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
      YTicks <- YTicks[c(seq(11L, length(YTicks)-1L, 10L))]
    } else if('Quantiles' %in% YTicks) {
      YTicks <- data[, quantile(round(get(YVar), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
      YTicks <- YTicks[c(seq(21L, length(YTicks)-1L, 20L))]
    } else if('Quartiles' %in% YTicks) {
      YTicks <- data[, quantile(round(get(YVar), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
      YTicks <- YTicks[c(seq(26L, length(YTicks)-1L, 25L))]
    } else {
      YTicks <- NULL
    }
  }

  # Add tick marks
  if(length(YTicks) != 0) p1 <- p1 + ggplot2::scale_x_continuous(breaks = as.numeric(YTicks))

  # Return plot
  return(eval(p1))
}

#' @title AutoPlotter
#'
#' @description Create plots
#'
#' @author Adrian Antico
#' @family Graphics
#'
#' @param dt = NULL
#' @param PlotType = input[['PlotType']]
#' @param SampleSize = input[['SampleSize']]
#' @param YVar = shiny::isolate(YVar())
#' @param XVar = shiny::isolate(DateVar())
#' @param YMin = input[['YMin']]
#' @param YMax = input[['YMax']]
#' @param XMin = input[['XMin']]
#' @param XMax = input[['XMax']]
#' @param ColorVariables = shiny::isolate(SelectedGroups())
#' @param SizeVar1 = input[['SizeVar1']]
#' @param FacetVar1 = shiny::isolate(input[['FacetVar1']])
#' @param FacetVar2 = shiny::isolate(input[['FacetVar2']])
#' @param YTicks = input[['YTicks']]
#' @param XTicks = input[['XTicks']]
#' @param Bins = 30
#' @param OutlierSize = input[['OutlierSize']]
#' @param OutlierColor = input[['OutlierColor']]
#' @param FillColor = input[['BoxPlotFill']]
#' @param GamFitScatter = input[['GamFitScatter']]
#' @param TextSize = input[['TextSize']]
#' @param AngleX = input[['AngleX']]
#' @param AngleY = input[['AngleY']]
#' @param ChartColor = input[['ChartColor']]
#' @param BorderColor = input[['BorderColor']]
#' @param TextColor = input[['TextColor']]
#' @param GridColor = input[['GridColor']]
#' @param BackGroundColor = input[['BackGroundColor']]
#' @param SubTitleColor = input[['SubTitleColor']]
#' @param LegendPosition = input[['LegendPosition']]
#' @param LegendBorderSize = as.numeric(input[['LegendBorderSize']])
#' @param LegendLineType = input[['LegendLineType']]
#' @param Debug = FALSE
AutoPlotter <- function(dt = NULL,
                        PlotType = 'Scatter',
                        SampleSize = 100000L,
                        YVar = NULL,
                        XVar = NULL,
                        YMin = NULL,
                        YMax = NULL,
                        XMin = NULL,
                        XMax = NULL,
                        CorrVariables = NULL,
                        CorrelationMethod = 'pearson',
                        ColorVariables = NULL,
                        SizeVar1 = 'None',
                        FacetVar1 = 'None',
                        FacetVar2 = 'None',
                        YTicks = 'Default',
                        XTicks = 'Default',
                        Bins = 30,
                        OutlierSize = 0.10,
                        OutlierColor = 'blue',
                        FillColor = 'gray',
                        GamFitScatter = FALSE,
                        TextSize = 12,
                        AngleX = 90,
                        AngleY = 0,
                        ChartColor = 'lightsteelblue1',
                        BorderColor = 'darkblue',
                        TextColor = 'darkblue',
                        GridColor = 'white',
                        BackGroundColor = 'gray95',
                        SubTitleColor = 'blue',
                        LegendPosition = 'bottom',
                        LegendBorderSize = 0.50,
                        LegendLineType = 'solid',
                        Debug = FALSE) {

  # Debug
  if(Debug) print(paste0('AutoPlotter() begin, PlotType = ', PlotType))

  # Correlation Matrix Plot
  if(tolower(PlotType) == 'corrmatrix') {
    p1 <- RemixAutoML::CorrMatrixPlot(
      data = dt,
      CorrVars = CorrVariables,
      CorrMethod = CorrelationMethod,
      FacetVar1 = FacetVar1,
      FacetVar2 = FacetVar2,
      SampleSize = SampleSize,
      DimnamesMaxNChar = 20L,
      CorrValueTextSize = 5.5,
      TextSize = TextSize,
      AngleX = AngleX,
      AngleY = AngleY,
      ChartColor = ChartColor,
      BorderColor = BorderColor,
      TextColor = TextColor,
      GridColor = GridColor,
      BackGroundColor = BackGroundColor,
      SubTitleColor = SubTitleColor,
      LegendPosition = LegendPosition,
      LegendBorderSize = LegendBorderSize,
      LegendLineType = LegendLineType,
      Debug = Debug)

    # Modify x-axis scale
    if(Debug) {print('XTicks'); print(XTicks)}
    date_check <- c("1 year", "1 day", "3 day", "1 week", "2 week", "1 month", "3 month", "6 month", "2 year", "5 year", "10 year", "1 minute", "15 minutes", "30 minutes", "1 hour", "3 hour", "6 hour", "12 hour")
    if(length(XTicks) > 1L && 'Default' %in% XTicks) XTicks <- XTicks[!XTicks %in% 'Default'][1L]
    if(!'Default' %in% XTicks && length(XTicks) == 1 && any(XTicks %in% date_check) && class(dt[[XVar]])[1L] == 'Date') p1 <- p1 + suppressMessages(ggplot2::scale_x_date(date_breaks = XTicks))

    # Return plot
    return(eval(p1))
  }

  # Box Plot
  if(tolower(PlotType) == 'boxplot') {
    p1 <- RemixAutoML::BoxPlot(
      data = dt,
      XVar = XVar,
      YVar = YVar,
      FacetVar1 = FacetVar1,
      FacetVar2 = FacetVar2,
      SampleSize = SampleSize,
      FillColor = FillColor,
      OutlierSize = OutlierSize,
      OutlierColor = OutlierColor,
      YTicks = YTicks,
      XTicks = XTicks,
      TextSize = TextSize,
      AngleX = AngleX,
      AngleY = AngleY,
      ChartColor = ChartColor,
      BorderColor = BorderColor,
      TextColor = TextColor,
      GridColor = GridColor,
      BackGroundColor = BackGroundColor,
      SubTitleColor = SubTitleColor,
      LegendPosition = LegendPosition,
      LegendBorderSize = LegendBorderSize,
      LegendLineType = LegendLineType,
      Debug = Debug)

    # Modify x-axis scale
    if(Debug) {print('XTicks'); print(XTicks)}
    date_check <- c("1 year", "1 day", "3 day", "1 week", "2 week", "1 month", "3 month", "6 month", "2 year", "5 year", "10 year", "1 minute", "15 minutes", "30 minutes", "1 hour", "3 hour", "6 hour", "12 hour")
    if(length(XTicks) > 1L && 'Default' %in% XTicks) XTicks <- XTicks[!XTicks %in% 'Default'][1L]
    if(!'Default' %in% XTicks && length(XTicks) == 1 && any(XTicks %in% date_check) && class(dt[[XVar]])[1L] == 'Date') p1 <- p1 + suppressMessages(ggplot2::scale_x_date(date_breaks = XTicks))

    # Return plot
    return(eval(p1))
  }

  # Violin Plot
  if(tolower(PlotType) == 'violinplot') {
    p1 <- RemixAutoML::ViolinPlot(
      data = dt,
      XVar = XVar,
      YVar = YVar,
      FacetVar1 = FacetVar1,
      FacetVar2 = FacetVar2,
      SampleSize = SampleSize,
      FillColor = FillColor,
      YTicks = YTicks,
      XTicks = XTicks,
      TextSize = TextSize,
      AngleX = AngleX,
      AngleY = AngleY,
      ChartColor = ChartColor,
      BorderColor = BorderColor,
      TextColor = TextColor,
      GridColor = GridColor,
      BackGroundColor = BackGroundColor,
      SubTitleColor = SubTitleColor,
      LegendPosition = LegendPosition,
      LegendBorderSize = LegendBorderSize,
      LegendLineType = LegendLineType,
      Debug = Debug)

    # Modify x-axis scale
    if(Debug) {print('XTicks'); print(XTicks)}
    date_check <- c("1 year", "1 day", "3 day", "1 week", "2 week", "1 month", "3 month", "6 month", "2 year", "5 year", "10 year", "1 minute", "15 minutes", "30 minutes", "1 hour", "3 hour", "6 hour", "12 hour")
    if(length(XTicks) > 1L && 'Default' %in% XTicks) XTicks <- XTicks[!XTicks %in% 'Default'][1L]
    if(!'Default' %in% XTicks && length(XTicks) == 1 && any(XTicks %in% date_check) && class(dt[[XVar]])[1L] == 'Date') p1 <- p1 + suppressMessages(ggplot2::scale_x_date(date_breaks = XTicks))

    # Return plot
    return(eval(p1))
  }

  # Bar Plot
  if(tolower(PlotType) == 'bar') {
    p1 <- RemixAutoML::BarPlot(
      data = dt,
      XVar = XVar,
      YVar = YVar,
      FacetVar1 = FacetVar1,
      FacetVar2 = FacetVar2,
      ColorVar = ColorVariables,
      SampleSize = SampleSize,
      FillColor = FillColor,
      YTicks = YTicks,
      XTicks = XTicks,
      TextSize = TextSize,
      AngleX = AngleX,
      AngleY = AngleY,
      ChartColor = ChartColor,
      BorderColor = BorderColor,
      TextColor = TextColor,
      GridColor = GridColor,
      BackGroundColor = BackGroundColor,
      SubTitleColor = SubTitleColor,
      LegendPosition = LegendPosition,
      LegendBorderSize = LegendBorderSize,
      LegendLineType = LegendLineType,
      Debug = Debug)

    # Modify x-axis scale
    if(Debug) {print('XTicks'); print(XTicks)}
    date_check <- c("1 year", "1 day", "3 day", "1 week", "2 week", "1 month", "3 month", "6 month", "2 year", "5 year", "10 year", "1 minute", "15 minutes", "30 minutes", "1 hour", "3 hour", "6 hour", "12 hour")
    if(length(XTicks) > 1L && 'Default' %in% XTicks) XTicks <- XTicks[!XTicks %in% 'Default'][1L]
    if(!'Default' %in% XTicks && length(XTicks) == 1 && any(XTicks %in% date_check) && class(dt[[XVar]])[1L] == 'Date') p1 <- p1 + suppressMessages(ggplot2::scale_x_date(date_breaks = XTicks))

    # Return plot
    return(eval(p1))
  }

  # Histogram
  if(tolower(PlotType) %chin% 'histogram') {
    p1 <- RemixAutoML::HistPlot(
      data = dt,
      XVar = XVar,
      YVar = YVar,
      FacetVar1 = FacetVar1,
      FacetVar2 = FacetVar2,
      SampleSize = SampleSize,
      Bins = Bins,
      FillColor = FillColor,
      YTicks = YTicks,
      XTicks = XTicks,
      TextSize = TextSize,
      AngleX = AngleX,
      AngleY = AngleY,
      ChartColor = ChartColor,
      BorderColor = BorderColor,
      TextColor = TextColor,
      GridColor = GridColor,
      BackGroundColor = BackGroundColor,
      SubTitleColor = SubTitleColor,
      LegendPosition = LegendPosition,
      LegendBorderSize = LegendBorderSize,
      LegendLineType = LegendLineType,
      Debug = Debug)

    # Modify x-axis scale
    if(Debug) {print('XTicks'); print(XTicks)}
    date_check <- c("1 year", "1 day", "3 day", "1 week", "2 week", "1 month", "3 month", "6 month", "2 year", "5 year", "10 year", "1 minute", "15 minutes", "30 minutes", "1 hour", "3 hour", "6 hour", "12 hour")
    if(length(XTicks) > 1L && 'Default' %in% XTicks) XTicks <- XTicks[!XTicks %in% 'Default'][1L]
    if(!'Default' %in% XTicks && length(XTicks) == 1 && any(XTicks %in% date_check) && class(dt[[XVar]])[1L] == 'Date') p1 <- p1 + suppressMessages(ggplot2::scale_x_date(date_breaks = XTicks))

    # Return plot
    return(eval(p1))
  }

  # Line
  if(tolower(PlotType) == 'line') {

    if(Debug) print('Line Plot Here')
    if(Debug) print(paste0('ColorVariables: ', ColorVariables))
    if(Debug) print(paste0('XTicks: ', XTicks))
    if(Debug) print(paste0('XVar: ', XVar))
    if(Debug) print(paste0('names(dt): ', names(dt)))

    # TS Line Plot
    p1 <- RemixAutoML:::TimeSeriesPlotter(
      Debug = Debug,
      dt = dt,
      TargetVariable = YVar,
      DateVariable = XVar,
      GroupVariables = ColorVariables,
      Aggregate = 'mean',
      NumberGroupsDisplay = 5,
      LevelsToDisplay = NULL,
      OtherGroupLabel = "OtherGroups",
      DisplayOtherGroup = TRUE,
      TextSize = TextSize,
      LineWidth = 0.5,
      Color = 'blue',
      XTickMarks = if('Default' %in% XTicks) NULL else XTicks,
      AngleX = AngleX,
      AngleY = AngleY,
      ChartColor = ChartColor,
      BorderColor = BorderColor,
      TextColor = TextColor,
      GridColor = GridColor,
      BackGroundColor = BackGroundColor,
      LegendPosition = LegendPosition,
      LegendTextColor = 'darkblue',
      LegendTextSize = max(1, floor(TextSize * 2 / 3)))

    # Update labels
    p1 <- p1 + ggplot2::labs(
      title = 'Time Series Plot',
      caption = 'RemixAutoML') +
      #ggplot2::ylim(as.numeric(eval(YMin)), as.numeric(eval(YMax))) +
      ggplot2::ylab(eval(YVar)) + ggplot2::xlab(eval(XVar)) +
      ggplot2::theme(legend.title = ggplot2::element_blank())

    # Modify x-axis scale
    if(Debug) {print('XTicks'); print(XTicks)}
    date_check <- c("1 year", "1 day", "3 day", "1 week", "2 week", "1 month", "3 month", "6 month", "2 year", "5 year", "10 year", "1 minute", "15 minutes", "30 minutes", "1 hour", "3 hour", "6 hour", "12 hour")
    if(length(XTicks) > 1L && 'Default' %in% XTicks) XTicks <- XTicks[!XTicks %in% 'Default'][1L]
    if(!'Default' %in% XTicks && length(XTicks) == 1 && any(XTicks %in% date_check) && class(dt[[XVar]])[1L] == 'Date') p1 <- p1 + suppressMessages(ggplot2::scale_x_date(date_breaks = XTicks))

    # Add ChartTheme
    if(Debug) print('ChartTheme')
    p1 <- p1 + RemixAutoML::ChartTheme(Size = TextSize, AngleX = AngleX, AngleY = AngleY, ChartColor = ChartColor, BorderColor = BorderColor, TextColor = TextColor, GridColor = GridColor, BackGroundColor = BackGroundColor, SubTitleColor = SubTitleColor, LegendPosition = LegendPosition, LegendBorderSize = LegendBorderSize, LegendLineType = LegendLineType)

    # Return plot
    return(eval(p1))
  }

  # Scatter or Copula
  if(tolower(PlotType) %in% c('scatter', 'copula')) {

    N <- dt[,.N]

    # Cap number of records----
    if(Debug) print('Scatter or Copula Plot Here')
    if(N > 1000000) dt <- dt[order(runif(.N))][seq_len(1000000)]

    # Subset + Sample
    R2_Pearson <- c()
    R2_Spearman <- c()
    yyy <- eval(YVar)
    xxx <- eval(XVar)
    if(is.null(xxx) || is.null(yyy)) {
      print('XVar is NULL')
      return(NULL)
    }
    if(Debug) print(class(dt[[eval(xxx)]]))
    if(Debug) print(!any(class(dt[[eval(xxx)]]) %chin% c('numeric', 'integer')) && !any(class(dt[[eval(yyy)]]) %chin% c('numeric', 'integer')))
    if(!any(class(dt[[eval(xxx)]]) %chin% c('numeric', 'integer')) || !any(class(dt[[eval(yyy)]]) %chin% c('numeric', 'integer'))) {
      print('XVar and / or YVar is not numeric nor integer')
      return(NULL)
    }
    if(Debug) print(N)
    if(N < 100000L) {
      for(zz in seq_len(30L)) {
        temp <- dt[order(runif(.N))][seq_len(floor(0.50 * .N))]
        R2_Pearson <- c(R2_Pearson, (cor(x = temp[[yyy]], y = temp[[xxx]], method = "pearson")) ^ 2)
        R2_Spearman <- c(R2_Spearman, (cor(x = temp[[yyy]], y = temp[[xxx]], method = "spearman")) ^ 2)
      }
    } else {
      for(zz in seq_len(30L)) {
        temp <- dt[order(runif(.N))][seq_len(100000L)]
        R2_Pearson <- c(R2_Pearson, (cor(x = temp[[yyy]], y = temp[[xxx]], method = "pearson")) ^ 2)
        R2_Spearman <- c(R2_Spearman, (cor(x = temp[[yyy]], y = temp[[xxx]], method = "spearman")) ^ 2)
      }
    }
    rm(temp)

    # Build plot objects
    if(Debug) print('Build plot objects')
    Output <- RemixAutoML::ScatterCopula(
      data = dt,
      x_var = xxx,
      y_var = yyy,
      GroupVariable = ColorVariables,
      FacetCol = FacetVar2,
      FacetRow = FacetVar1,
      SizeVar1 = SizeVar1,
      SampleCount = 100000L,
      FitGam = as.logical(GamFitScatter))

    # Modify by plot type
    if(Debug) print('Modify by plot type')
    if(PlotType %chin% 'Scatter') {
      p1 <- Output[['ScatterPlot']]
      p1 <- p1 + ggplot2::labs(
        title = paste0('Scatter Plot'),
        subtitle = paste0("r-sq pearson xbar = ", round(mean(R2_Pearson),3L), " +/- ", round(sd(R2_Pearson) / sqrt(30L), 5L)," :: ",
                          "r-sq spearman xbar = ", round(mean(R2_Spearman),3L), " +/- ", round(sd(R2_Spearman) / sqrt(30L), 5L)),
        caption = 'RemixAutoML')
      p1 <- shiny::isolate( p1 + RemixAutoML::ChartTheme(Size = TextSize, AngleX = AngleX, AngleY = AngleY, ChartColor = ChartColor, BorderColor = BorderColor, TextColor = TextColor, GridColor = GridColor, BackGroundColor = BackGroundColor))
      p1 <- p1 + ggplot2::ylim(as.numeric(eval(YMin)), as.numeric(eval(YMax)))
      p1 <- p1 + ggplot2::xlim(as.numeric(eval(XMin)), as.numeric(eval(XMax)))

    } else if(PlotType %chin% 'Copula') {

      p1 <- Output[['CopulaPlot']]
      p1 <- p1 + ggplot2::labs(
        title = paste0('Empirical Copula Plot'),
        subtitle = paste0("r-sq pearson xbar = ", round(mean(R2_Pearson),3L), " +/- ", round(sd(R2_Pearson) / sqrt(30L), 5L)," :: ",
                          "r-sq spearman xbar = ", round(mean(R2_Spearman),3L), " +/- ", round(sd(R2_Spearman) / sqrt(30L), 5L)),
        caption = 'RemixAutoML')
      p1 <- shiny::isolate( p1 + RemixAutoML::ChartTheme(Size = TextSize, AngleX = AngleX, AngleY = AngleY, ChartColor = ChartColor, BorderColor = BorderColor, TextColor = TextColor, GridColor = GridColor, BackGroundColor = BackGroundColor))
    }

    # Tick Marks
    if(Debug) print('YTicks Update')
    if('Percentiles' %in% YTicks) {
      y_vals <- dt[, quantile(round(get(YVar), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
    } else if('Every 5th percentile' %in% YTicks) {
      y_vals <- dt[, quantile(round(get(YVar), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
      y_vals <- y_vals[c(seq(6L, length(y_vals)-1L, 5L))]
    } else if('Deciles' %in% YTicks) {
      y_vals <- dt[, quantile(round(get(YVar), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
      y_vals <- y_vals[c(seq(11L, length(y_vals)-1L, 10L))]
    } else if('Quantiles' %in% YTicks) {
      y_vals <- dt[, quantile(round(get(YVar), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
      y_vals <- y_vals[c(seq(21L, length(y_vals)-1L, 20L))]
    } else if('Quartiles' %in% YTicks) {
      y_vals <- dt[, quantile(round(get(YVar), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
      y_vals <- y_vals[c(seq(26L, length(y_vals)-1L, 25L))]
    } else {
      y_vals <- YTicks
    }

    if(Debug) print('XTicks Update')
    if('Percentiles' %in% XTicks) {
      x_vals <- dt[, quantile(round(get(XVar), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
    } else if('Every 5th percentile' %in% XTicks) {
      x_vals <- dt[, quantile(round(get(YVar), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
      x_vals <- x_vals[c(seq(6L, length(x_vals)-1L, 5L))]
    } else if('Deciles' %in% XTicks) {
      x_vals <- dt[, quantile(round(get(YVar), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
      x_vals <- x_vals[c(seq(11L, length(x_vals)-1L, 10L))]
    } else if('Quantiles' %in% XTicks) {
      x_vals <- dt[, quantile(round(get(YVar), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
      x_vals <- x_vals[c(seq(21L, length(x_vals)-1L, 20L))]
    } else if('Quartiles' %in% XTicks) {
      x_vals <- dt[, quantile(round(get(YVar), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]
      x_vals <- x_vals[c(seq(26L, length(x_vals)-1L, 25L))]
    } else {
      x_vals <- XTicks
    }

    if(Debug) print('Update X and Y Ticks')
    if(!'Default' %in% XTicks && PlotType == 'Scatter') p1 <- p1 + ggplot2::scale_x_continuous(breaks = as.numeric(x_vals))
    if(!'Default' %in% YTicks && PlotType == 'Scatter') p1 <- p1 + ggplot2::scale_y_continuous(breaks = as.numeric(y_vals))

    # Add ChartTheme
    if(Debug) print('ChartTheme')
    p1 <- p1 + RemixAutoML::ChartTheme(Size = TextSize, AngleX = AngleX, AngleY = AngleY, ChartColor = ChartColor, BorderColor = BorderColor, TextColor = TextColor, GridColor = GridColor, BackGroundColor = BackGroundColor, SubTitleColor = SubTitleColor, LegendPosition = LegendPosition, LegendBorderSize = LegendBorderSize, LegendLineType = LegendLineType)

    # Limit Y
    if(Debug) print('Limit Y'); print(PlotType)
    if(PlotType == 'Scatter' && !is.null(RemixAutoML:::CEPP(YMin, Default = NULL)) && !is.null(RemixAutoML:::CEPP(YMax, Default = NULL))) p1 <- p1 + ggplot2::ylim(as.numeric(eval(YMin)), as.numeric(eval(YMax)))

    # Return plot
    return(eval(p1))
  }
}

#' @title AppModelInsights
#'
#' @description Plot model insights in app
#'
#' @author Adrian Antico
#' @family Graphics
#'
#' @param ModelOutputList output list
#' @param dt = NULL,
#' @param PlotType = NULL,
#' @param TargetVar = isolate(YVar()),
#' @param PredictVar = isolate(ScoreVar()),
#' @param PDPVar = NULL,
#' @param DateVar = isolate(DateVar()),
#' @param FacetVar1 = NULL
#' @param FacetVar2 = NULL
#' @param GamFit = FALSE,
#' @param Buckets = 20,
#' @param ShapAgg A string for aggregating shapely values for importances. Choices include, 'mean', 'absmean', 'meanabs', 'geomean', 'harmmean', 'sd', 'median', 'absmedian', 'medianabs'
#' @param Rebuild = FALSE,
#' @param Check2 = FALSE,
#' @param Debug = FALSE
AppModelInsights <- function(ModelOutputList,
                             dt = NULL,
                             PlotType = NULL,
                             TargetVar = NULL,
                             PredictVar = NULL,
                             PDPVar = NULL,
                             DateVar = NULL,
                             FacetVar1 = NULL,
                             FacetVar2 = NULL,
                             GamFit = FALSE,
                             Buckets = 20,
                             ShapAgg = 'mean',
                             Rebuild = FALSE,
                             Check2 = FALSE,
                             Debug = FALSE) {

  # Debugging
  if(Debug) {print('Running AppModelInsights'); print(paste0('Rebuild = ', Rebuild))}

  # Shap VI
  if(tolower(PlotType) == 'shapelyvarimp') {

    # Debugging
    if(Debug) {print(dt); print(names(dt)); print(dt[, .N]); print(names(dt)[which(names(dt) %like% 'Shap_')]); print(length(names(dt)[which(names(dt) %like% 'Shap_')]))}

    # Prepare info
    vals <- names(dt)[which(names(dt) %like% 'Shap_')]
    if(length(vals) != 0) {
      p1 <- RemixAutoML::ShapImportancePlot(dt, ShapColNames = vals, FacetVar1 = FacetVar1, FacetVar2 = FacetVar2, AggMethod = ShapAgg, TopN = 25)
    } else {
      p1 <- NULL
    }

    # Set to NULL if not built for some reason
    if(!exists('p1')) p1 <- NULL

    # Debugging
    if(Debug) {print(exists('p1')); if(exists('p1')) print(is.null('p1'))}

    # return
    return(eval(p1))
  }

  # Test Evaluation Plot ----
  if(any(PlotType %chin% "Test_EvaluationPlot")) {
    if(Debug) print('Evaluation Plot')
    if(!Rebuild) {
      if(Debug) print('!Rebuild')
      p1 <- ModelOutputList$PlotList[['Test_EvaluationPlot']]
    } else {
      if(Debug) {
        print('! !Rebuild')
        print(paste0('Names in dt = ', names(dt)))
        print(paste0('PredictionColName = ', PredictVar))
        print(paste0('TargetVar = ', TargetVar))
        print(paste0('Buckets = ', Buckets))
      }
      p1 <- RemixAutoML::EvalPlot(
        data = dt,
        PredictionColName = PredictVar,
        TargetColName = TargetVar,
        GraphType = "calibration", PercentileBucket = 1/Buckets, aggrfun = function(x) mean(x, na.rm = TRUE))
    }
    if(!exists('p1')) p1 <- NULL
    return(eval(p1))
  }

  # ----

  # Train Evaluation Plot ----
  if(any(PlotType %chin% 'Train_EvaluationPlot')) {
    if(Debug) print('Evaluation Plot')
    if(Debug) print(Rebuild)
    if(!Rebuild) {
      if(Debug) print('!Rebuild')
      p1 <- ModelOutputList$PlotList[['Train_EvaluationPlot']]
    } else {
      if(Debug) {
        print('! !Rebuild')
        print(paste0('Names in dt = ', names(dt)))
        print(paste0('PredictionColName = ', PredictVar))
        print(paste0('TargetVar = ', TargetVar))
        print(paste0('Buckets = ', Buckets))
      }
      p1 <- RemixAutoML::EvalPlot(
        data = dt,
        PredictionColName = PredictVar,
        TargetColName = TargetVar,
        GraphType = "calibration", PercentileBucket = 1/Buckets, aggrfun = function(x) mean(x, na.rm = TRUE))
    }
    if(!exists('p1')) p1 <- NULL
    return(eval(p1))
  }

  # ----

  # Evaluation BoxPlot ----
  if(any(PlotType %chin% "Test_EvaluationBoxPlot")) {
    if(!Rebuild) {
      if(Debug) print('EvalBoxPlot !Rebuild')
      p1 <- ModelOutputList$PlotList[['Test_EvaluationBoxPlot']]
    } else {
      if(Debug) print('EvalBoxPlot ! !Rebuild')
      p1 <- RemixAutoML::EvalPlot(
        data = dt,
        PredictionColName = PredictVar,
        TargetColName = TargetVar,
        GraphType = "boxplot", PercentileBucket = 1/Buckets, aggrfun = function(x) mean(x, na.rm = TRUE))
    }
    if(!exists('p1')) p1 <- NULL
    return(eval(p1))
  }

  # ----

  # Evaluation BoxPlot Train ----
  if(any(PlotType %chin% "Train_EvaluationBoxPlot")) {
    if(!Rebuild) {
      if(Debug) print('EvalBoxPlot !Rebuild')
      p1 <- ModelOutputList$PlotList[['Train_EvaluationBoxPlot']]
    } else {
      if(Debug) {
        print('! !Rebuild')
        print(paste0('Names in dt = ', names(dt)))
        print(paste0('PredictionColName = ', PredictVar))
        print(paste0('TargetVar = ', TargetVar))
        print(paste0('Buckets = ', Buckets))
      }
      p1 <- RemixAutoML::EvalPlot(
        data = dt,
        PredictionColName = PredictVar,
        TargetColName = TargetVar,
        GraphType = "boxplot", PercentileBucket = 1/Buckets, aggrfun = function(x) mean(x, na.rm = TRUE))
    }
    if(!exists('p1')) p1 <- NULL
    return(eval(p1))
  }

  # ----

  # ROC Plot ----
  if(any(PlotType %chin% "Test_ROC_Plot")) {
    if(!Rebuild) {
      if(Debug) print('ROC !Rebuild')
      p1 <- ModelOutputList$PlotList[['Test_ROC_Plot']]
    } else {
      if(Debug) print('Test_ROC_Plot ! !Rebuild')
      p1 <- RemixAutoML::ROCPlot(
        data = dt,
        TargetName = TargetVar,
        SavePlot = FALSE, Name = NULL, metapath = NULL, modelpath = NULL)
    }
    if(!exists('p1')) p1 <- NULL
    return(eval(p1))
  }

  # ----

  # ROC Plot Train ----
  if(any(PlotType %chin% "Train_ROC_Plot")) {
    if(!Rebuild) {
      if(Debug) print('Train_ROC_Plot !Rebuild')
      p1 <- ModelOutputList$PlotList[['Train_ROC_Plot']]
    } else {
      if(Debug) print('Gains Plot ! !Rebuild')
      p1 <- RemixAutoML::CumGainsChart(
        data = dt,
        TargetColumnName = TargetVar,
        PredictedColumnName = PredictVar,
        SavePlot = FALSE, Name = NULL, metapath = NULL, modelpath = NULL)$GainsPlot
    }
  }

  # ----

  # Gains Plot ----
  if(any(PlotType %chin% "Test_GainsPlot")) {
    if(!Rebuild) {
      if(Debug) print('Test_GainsPlot !Rebuild')
      p1 <- ModelOutputList$PlotList[['Test_GainsPlot']]
    } else {
      if(Debug) print('Gains Plot ! !Rebuild')
      p1 <- RemixAutoML::CumGainsChart(
        data = dt,
        TargetColumnName = TargetVar,
        PredictedColumnName = PredictVar,
        SavePlot = FALSE, Name = NULL, metapath = NULL, modelpath = NULL)$GainsPlot
    }
    if(!exists('p1')) p1 <- NULL
    return(eval(p1))
  }

  # ----

  # Gains Plot Train ----
  if(any(PlotType %chin% "Train_GainsPlot")) {
    if(!Rebuild) {
      if(Debug) print('Gains Plot !Rebuild')
      p1 <- ModelOutputList$PlotList[['Train_GainsPlot']]
    } else {
      if(Debug) print('Train_LiftPlot ! !Rebuild')
      p1 <- RemixAutoML::CumGainsChart(
        data = dt,
        TargetColumnName = TargetVar,
        PredictedColumnName = PredictVar,
        SavePlot = FALSE, Name = NULL, metapath = NULL, modelpath = NULL)$GainsPlot
    }
    if(!exists('p1')) p1 <- NULL
    return(eval(p1))
  }

  # ----

  # Lift Plot Test ----
  if(any(PlotType %chin% "Test_LiftPlot")) {
    if(!Rebuild) {
      if(Debug) print('Test_LiftPlot !Rebuild')
      p1 <- ModelOutputList$PlotList[['Test_LiftPlot']]
      if(Debug) print('You Are Inside AppModelInsights 1')
    } else {
      if(Debug) print('Test_LiftPlot ! !Rebuild')
      p1 <- RemixAutoML::CumGainsChart(
        data = dt,
        TargetColumnName = TargetVar,
        PredictedColumnName = PredictVar,
        SavePlot = FALSE, Name = NULL, metapath = NULL, modelpath = NULL)$LiftPlot
    }
    if(!exists('p1')) p1 <- NULL
    return(eval(p1))
  }

  # ----

  # Lift Plot Train ----
  if(any(PlotType %chin% "Train_LiftPlot")) {
    if(!Rebuild) {
      if(Debug) print('Train_LiftPlot !Rebuild')
      p1 <- ModelOutputList$PlotList[['Train_LiftPlot']]
    } else {
      if(Debug) print('Test_LiftPlot ! !Rebuild')
      p1 <- RemixAutoML::CumGainsChart(
        data = dt,
        TargetColumnName = TargetVar,
        PredictedColumnName = PredictVar,
        SavePlot = FALSE, Name = NULL, metapath = NULL, modelpath = NULL)$LiftPlot
    }
    if(!exists('p1')) p1 <- NULL
    return(eval(p1))
  }

  # ----

  # Scatter Plot Test ----
  if(any(PlotType %chin% "Test_ScatterPlot")) {
    if(!Rebuild) {
      if(Debug) print('Test_ScatterPlot !Rebuild')
      p1 <- ModelOutputList$PlotList[['Test_ScatterPlot']]
      if(Debug) print(class(p1))
      if(Debug) print('Extracted the Test_ScatterPlot from ModelOutputList')
    } else {
      if(Debug) print('Test_ScatterPlot ! !Rebuild')
      p1 <- RemixAutoML::ResidualPlots(
        TestData = dt,
        Target = TargetVar, Predicted = PredictVar,
        DateColumnName = DateVar, Gam_Fit = GamFit)$ScatterPlot
    }
    if(!exists('p1')) p1 <- NULL
    return(eval(p1))
  }

  # ----

  # Scatter Plot Train ----
  if(any(PlotType %chin% "Train_ScatterPlot")) {
    if(!Rebuild) {
      if(Debug) print('Train_ScatterPlot !Rebuild')
      p1 <- ModelOutputList$PlotList[['Train_ScatterPlot']]
    } else {
      if(Debug) print('Train_ScatterPlot ! !Rebuild')
      p1 <- RemixAutoML::ResidualPlots(
        TestData = dt,
        Target = TargetVar, Predicted = PredictVar,
        DateColumnName = DateVar, Gam_Fit = GamFit)$ScatterPlot
    }
    if(!exists('p1')) p1 <- NULL
    return(eval(p1))
  }

  # ----

  # Copula Plot Test ----
  if(any(PlotType %chin% "Test_CopulaPlot")) {
    if(!Rebuild) {
      if(Debug) print('Test_CopulaPlot !Rebuild')
      p1 <- ModelOutputList$PlotList[['Test_CopulaPlot']]
    } else {
      if(Debug) print('Test_CopulaPlot ! !Rebuild')
      p1 <- RemixAutoML::ResidualPlots(
        TestData = dt,
        Target = TargetVar, Predicted = PredictVar,
        DateColumnName = DateVar, Gam_Fit = GamFit)$CopulaPlot
    }
    if(!exists('p1')) p1 <- NULL
    return(eval(p1))
  }

  # ----

  # Copula Plot Train ----
  if(any(PlotType %chin% "Train_CopulaPlot")) {
    if(!Rebuild) {
      if(Debug) print('Train_CopulaPlot !Rebuild')
      p1 <- ModelOutputList$PlotList[['Train_CopulaPlot']]
    } else {
      if(Debug) print('Train_CopulaPlot ! !Rebuild')
      p1 <- RemixAutoML::ResidualPlots(
        TestData = dt,
        Target = TargetVar, Predicted = PredictVar,
        DateColumnName = DateVar, Gam_Fit = GamFit)$CopulaPlot
    }
    if(!exists('p1')) p1 <- NULL
    return(eval(p1))
  }

  # ----

  # Residuals Histogram Plot Test ----
  if(any(PlotType %chin% "Test_ResidualsHistogram")) {
    if(!Rebuild) {
      if(Debug) print('Test_ResidualsHistogram !Rebuild')
      p1 <- ModelOutputList$PlotList[['Test_ResidualsHistogram']]
    } else {
      if(Debug) print('Test_ResidualsHistogram ! !Rebuild')
      p1 <- RemixAutoML::ResidualPlots(
        TestData = dt,
        Target = TargetVar, Predicted = PredictVar,
        DateColumnName = DateVar, Gam_Fit = GamFit)$ResidualsHistogram
    }
    if(!exists('p1')) p1 <- NULL
    return(eval(p1))
  }

  # ----

  # Residuals Histogram Plot Train ----
  if(any(PlotType %chin% "Train_ResidualsHistogram")) {
    if(!Rebuild) {
      if(Debug) print('Train_ResidualsHistogram !Rebuild')
      p1 <- ModelOutputList$PlotList[['Train_ResidualsHistogram']]
    } else {
      if(Debug) print('Train_ResidualsHistogram ! !Rebuild')
      p1 <- RemixAutoML::ResidualPlots(
        TestData = dt,
        Target = TargetVar, Predicted = PredictVar,
        DateColumnName = DateVar, Gam_Fit = GamFit)$ResidualsHistogram
    }
    if(!exists('p1')) p1 <- NULL
    return(eval(p1))
  }

  # ----

  # Variable Importance Plot Test ----
  if(any(PlotType %chin% "Test_VariableImportance")) {
    if(Debug) print('Test_Importance ! !Rebuild')
    if(Debug) print(ModelOutputList$VariableImportance[['Test_Importance']])
    p1 <- RemixAutoML:::VI_Plot(Type = "catboost", VI_Data = ModelOutputList$VariableImportance[['Test_Importance']], TopN = 25)
    if(!exists('p1')) p1 <- NULL
    if(!is.null(p1)) {
      p1 <- p1 + ggplot2::labs(title = 'Global Variable Importance: test data', caption = 'RemixAutoML')
      p1 <- p1 + ggplot2::ylab('ML-Algo-Generated Variable Importance')
    }
    return(eval(p1))

  }

  # ----

  # Variable Importance Plot Validation ----
  if(any(PlotType %chin% "Validation_VariableImportance")) {
    if(Debug) print('Validation_Importance ! !Rebuild')
    if(Debug) print(ModelOutputList$VariableImportance[['Validation_Importance']])
    p1 <- RemixAutoML:::VI_Plot(Type = "catboost", VI_Data = ModelOutputList$VariableImportance[['Validation_Importance']], TopN = 25)
    if(!exists('p1')) p1 <- NULL
    if(!is.null(p1)) {
      p1 <- p1 + ggplot2::labs(title = 'Global Variable Importance: Validation data', caption = 'RemixAutoML')
      p1 <- p1 + ggplot2::ylab('ML-Algo-Generated Variable Importance')
    }
    return(eval(p1))

  }

  # ----

  # Variable Importance Plot Train ----
  if(any(PlotType %chin% 'Train_VariableImportance')) {
    if(Debug) print('Train_Importance ! !Rebuild')
    if(Debug) print(ModelOutputList$VariableImportance[['Train_Importance']])
    p1 <- RemixAutoML:::VI_Plot(Type = 'catboost', VI_Data = ModelOutputList$VariableImportance[['Train_Importance']], TopN = 25)
    if(!exists('p1')) p1 <- NULL
    if(!is.null(p1)) {
      p1 <- p1 + ggplot2::labs(title = 'Global Variable Importance: training data', caption = 'RemixAutoML')
      p1 <- p1 + ggplot2::ylab('ML-Algo-Generated Variable Importance')
    }
    return(eval(p1))
  }

  # ----

  # Partial Dependence Plot Test ----
  if(any(PlotType %chin% 'Test_ParDepPlots') && !is.null(PDPVar)) {
    if(!Rebuild) {
      p1 <- p1 <- ModelOutputList$PlotList$Test_ParDepPlots[[eval(PDPVar)]]
      p1 <- p1 + ggplot2::labs(subtitle = NULL)
      p1$layers[[6L]] <- NULL
      p1$layers[[5L]] <- NULL
      p1$layers[[4L]] <- NULL
    } else {
      p1 <- RemixAutoML::ParDepCalPlots(
        data = dt,
        PredictionColName = PredictVar,
        TargetColName = TargetVar,
        IndepVar = PDPVar,
        GraphType = 'calibration', PercentileBucket = 1 / Buckets, FactLevels = 10, Function = function(x) mean(x, na.rm = TRUE))
      p1 <- p1 + ggplot2::labs(subtitle = NULL)
      p1$layers[[6L]] <- NULL
      p1$layers[[5L]] <- NULL
      p1$layers[[4L]] <- NULL
    }
    if(!exists('p1')) p1 <- NULL
    return(eval(p1))
  }

  # ----

  # Partial Dependence Plot Train ----
  if(any(PlotType %chin% 'Train_ParDepPlots') && !is.null(PDPVar)) {
    if(Debug) print('Partial Dependence Plot Train')
    if(!Rebuild && !is.null(ModelOutputList$PlotList$Train_ParDepPlots[[eval(PDPVar)]])) {
      p1 <- ModelOutputList$PlotList$Train_ParDepPlots[[eval(PDPVar)]]
      p1$layers[[6L]] <- NULL
      p1$layers[[5L]] <- NULL
      p1$layers[[4L]] <- NULL
    } else {
      if(Debug) print('Partial Dependence Plot Train else')
      p1 <- RemixAutoML::ParDepCalPlots(
        data = dt,
        PredictionColName = PredictVar,
        TargetColName = TargetVar,
        IndepVar = PDPVar,
        GraphType = 'calibration', PercentileBucket = 1 / Buckets, FactLevels = 10, Function = function(x) mean(x, na.rm = TRUE))
      p1$layers[[6L]] <- NULL
      p1$layers[[5L]] <- NULL
      p1$layers[[4L]] <- NULL
    }
    if(!exists('p1')) p1 <- NULL
    return(eval(p1))
  }

  # ----

  # Partial Dependence Box Plot Test ----
  if(any(PlotType %chin% 'Test_ParDepBoxPlots') && !is.null(PDPVar)) {
    if(!Rebuild && !is.null(ModelOutputList$PlotList$Test_ParDepBoxPlots[[eval(PDPVar)]])) {
      p1 <- ModelOutputList$PlotList$Test_ParDepBoxPlots[[eval(PDPVar)]]
      p1 <- p1 + ggplot2::labs(subtitle = NULL)
      p1$layers[[6L]] <- NULL
      p1$layers[[5L]] <- NULL
      p1$layers[[4L]] <- NULL
    } else {
      p1 <- RemixAutoML::ParDepCalPlots(
        data = dt,
        PredictionColName = PredictVar,
        TargetColName = TargetVar,
        IndepVar = PDPVar,
        GraphType = "boxplot", PercentileBucket = 1 / Buckets, FactLevels = 10, Function = function(x) mean(x, na.rm = TRUE))
      p1 <- p1 + ggplot2::labs(subtitle = NULL)
      p1$layers[[6L]] <- NULL
      p1$layers[[5L]] <- NULL
      p1$layers[[4L]] <- NULL
    }
    if(!exists('p1')) p1 <- NULL
    return(eval(p1))
  }

  # ----

  # Partial Dependence Box Plot Train ----
  if(any(PlotType %chin% 'Train_ParDepBoxPlots') && !is.null(PDPVar)) {
    if(!Rebuild && !is.null(ModelOutputList$PlotList$Train_ParDepBoxPlots[[eval(PDPVar)]])) {
      p1 <- ModelOutputList$PlotList$Train_ParDepBoxPlots[[eval(PDPVar)]]
      p1 <- p1 + ggplot2::labs(subtitle = NULL)
      p1$layers[[6L]] <- NULL
      p1$layers[[5L]] <- NULL
      p1$layers[[4L]] <- NULL
    } else {
      p1 <- RemixAutoML::ParDepCalPlots(
        data = dt,
        PredictionColName = PredictVar,
        TargetColName = TargetVar,
        IndepVar = PDPVar,
        GraphType = "boxplot", PercentileBucket = 1 / Buckets, FactLevels = 10, Function = function(x) mean(x, na.rm = TRUE))
      p1 <- p1 + ggplot2::labs(subtitle = NULL)
      p1$layers[[6L]] <- NULL
      p1$layers[[5L]] <- NULL
      p1$layers[[4L]] <- NULL
    }
    if(!exists('p1')) p1 <- NULL
    return(eval(p1))
  }

  # ----

  # Shap Table Variable Importance ----
  # if(any(PlotType %chin% "ShapPlot")) {
  #   if(!Rebuild && data.table::is.data.table(ML_ShapTable)) {
  #     ML_ShapTable2 <- ML_ShapTable[, list(Importance = mean(ShapValue, na.rm = TRUE)), by = "Variable"]
  #     p1 <- RemixAutoML:::VI_Plot(Type = "catboost", VI_Data = ML_ShapTable2, TopN = 25)
  #   } else {
  #     ML_ShapTable1 <- RemixAutoML::AutoShapeShap(ScoringData = dt, Threads = parallel::detectCores(), DateColumnName = DateVar, ByVariableName = NULL)
  #     ML_ShapTable2 <- ML_ShapTable1[, list(Importance = mean(ShapValue, na.rm = TRUE)), by = "Variable"]
  #     p1 <- RemixAutoML:::VI_Plot(Type = "catboost", VI_Data = ML_ShapTable2, TopN = 25)
  #   }
  # }

  if(!exists('p1')) p1 <- NULL
  return(eval(p1))
}

#' @title multiplot
#'
#' @description Sick of copying this one into your code? Well, not anymore.
#'
#' @author Adrian Antico
#' @family Graphics
#'
#' @param plotlist This is the list of your charts
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
#' RemixAutoML::multiplot(plotlist = list(p1,p2))
#' }
#' @return Multiple ggplots on a single image
#' @export
multiplot <- function(plotlist = NULL) {
  plotlist[vapply(plotlist, is.null, logical(1))] <- NULL
  batches <- ceiling(length(plotlist) / 4L)
  for(i in seq_len(batches)) {
    firstPlot <- ((i - 1L) * 4L) + 1L
    lastPlot <- min(firstPlot + 3L, length(plotlist), na.rm = TRUE)
    if(lastPlot == firstPlot) {
      plot(plotlist[[firstPlot]])
    } else {
      grid::grid.newpage()
      grid::pushViewport(grid::viewport(layout = grid::grid.layout(2L, 2L)))
      row <- 1L
      col <- 1L
      for(j in firstPlot:lastPlot) {
        print(plotlist[[j]], vp = grid::viewport(layout.pos.row = row, layout.pos.col = col))
        if(row == 2L) {
          row <- 1L
          col <- col + 1L
        } else {
          row = row + 1L
        }
      }
    }
  }
}

#' @title RemixTheme
#'
#' @description This function adds the Remix Theme to ggplots
#'
#' @author Douglas Pestana
#' @family Graphics
#'
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
#' @noRd
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

#' @title ChartTheme
#'
#' @description This function helps your ggplots look professional with the choice of the two main colors that will dominate the theme
#'
#' @author Adrian Antico
#' @family Graphics
#'
#' @param Size The size of the axis labels and title
#' @param AngleX The angle of the x axis labels
#' @param AngleY The angle of the Y axis labels
#' @param ChartColor "lightsteelblue1",
#' @param BorderColor "darkblue"
#' @param SubTitleColor 'blue'
#' @param TextColor "darkblue"
#' @param GridColor "white"
#' @param BackGroundColor "gray95"
#' @param LegendPosition Where to place legend
#' @param LegendBorderSize 0.50
#' @param LegendLineType 'solid'
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
                       AngleX = 90,
                       AngleY = 0,
                       ChartColor = 'lightsteelblue1',
                       BorderColor = 'darkblue',
                       TextColor = 'darkblue',
                       SubTitleColor = 'blue',
                       GridColor = 'white',
                       BackGroundColor = 'gray95',
                       LegendPosition = 'bottom',
                       LegendBorderSize = 0.01,
                       LegendLineType = 'solid') {
  chart_theme <- ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = BackGroundColor),
    panel.background = ggplot2::element_rect(fill = ChartColor, colour = BorderColor, size = 0.25, color = BorderColor),
    panel.grid.major = ggplot2::element_line(colour = BorderColor, size = 0.01, color = GridColor, linetype = 1),
    panel.grid.minor = ggplot2::element_line(colour = BorderColor, size = 0.01, color = GridColor, linetype = 1),
    legend.position = LegendPosition,
    legend.title = ggplot2::element_text(color = BorderColor, size = Size, face = 'bold'),
    plot.subtitle = ggplot2::element_text(color = SubTitleColor, size = max(1,floor(Size * 5 / 6)), face = 'bold'),
    legend.background = ggplot2::element_rect(fill = BackGroundColor, size = LegendBorderSize, linetype = LegendLineType, color = BorderColor),
    plot.title = ggplot2::element_text(color = TextColor, size = Size, face = 'bold'),
    axis.title = ggplot2::element_text(color = TextColor, size = Size, face = 'bold'),
    axis.text.x = ggplot2::element_text(colour = TextColor, face = "bold", angle = AngleX),
    axis.text.y = ggplot2::element_text(colour = TextColor, face = "bold", angle = AngleY),
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 20, r = 20, b = 20, l = 20)),
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 20, r = 20, b = 20, l = 20)),
    panel.border = ggplot2::element_rect(colour = BorderColor, fill = NA, size = 1.5))
  chart_theme
}

#' @title TimeSeriesPlotter
#'
#' @description TimeSeriesPlotter is a function to plot single or multiple lines on a single plot
#'
#' @family Graphics
#' @author Adrian Antico
#'
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
#' @param SSForecast Default FALSE. Set to TRUE for single series models
#' @param PredictionIntervalColorInner Fills 20th to 80th percentiles
#' @param PredictionIntervalColorOuter Fills 5th to 20th and 80th to 95th percentiles
#' @param Debug = FALSE
#' @noRd
TimeSeriesPlotter <- function(dt = NULL,
                              TargetVariable = NULL,
                              DateVariable = NULL,
                              GroupVariables = NULL,
                              EvaluationMode = FALSE,
                              VLineDate = NULL,
                              Aggregate = NULL,
                              NumberGroupsDisplay = 5,
                              LevelsToDisplay = NULL,
                              OtherGroupLabel = "Other",
                              DisplayOtherGroup = TRUE,
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
                              SSForecast = FALSE,
                              PredictionIntervalColorInner = "aquamarine1",
                              PredictionIntervalColorOuter = "peachpuff1",
                              Debug = FALSE) {

  # No scientific notation----
  options(scipen = FALSE)
  options(warn = -1)

  if(is.null(dt)) stop('dt is NULL')

  # Ensure dt is data.table----
  if(!data.table::is.data.table(dt)) data.table::setDT(dt)

  # Ensure arguments are correct----
  if(!is.null(TargetVariable)) if(!is.character(TargetVariable)) stop("TargetVariable did not pass through as string")
  if(!is.null(DateVariable)) if(!is.character(DateVariable)) stop("DateVariable did not pass through as string")
  if(!is.null(Aggregate)) if(!is.character(Aggregate)) stop("Aggregate did not pass through as string")
  if(!is.null(NumberGroupsDisplay)) if(is.character(NumberGroupsDisplay) || is.factor(NumberGroupsDisplay)) stop("NumberGroupsDisplay needs to be a number")
  if(!is.null(OtherGroupLabel)) if(!is.character(OtherGroupLabel)) stop("OtherGroupLabel did not pass through as string")

  # Forecast----
  if(SSForecast) {

    # Subset dt----
    if("ModelID" %chin% names(dt)) {
      dataSubset <- dt[ModelID == eval(TS_ModelID)]
    } else {
      dataSubset <- dt
    }

    # Groupvariables
    if(length(dataSubset[[eval(DateVariable)]]) != length(unique(dataSubset[[eval(DateVariable)]]))) {
      dataSubset <- dataSubset[, list(temp1 = mean(get(TargetVariable),na.rm = TRUE),
                                      Forecast = mean(Forecast,na.rm = TRUE)),
                               by = eval(DateVariable)]
      data.table::setnames(dataSubset, "temp1", eval(TargetVariable[1L]))
    }

    # Plot dt----
    Plot <- ggplot2::ggplot(dataSubset, ggplot2::aes(as.POSIXct(get(DateVariable)))) +
      ggplot2::geom_line(ggplot2::aes(y = get(TargetVariable[2L]), color = "Forecast")) +
      ggplot2::geom_line(ggplot2::aes(y = get(TargetVariable[1L]), color = "Actuals")) +
      ggplot2::scale_color_manual("", breaks = c("Forecast","Actuals"), values = c(ForecastLineColor, "blue")) +
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
    if(!is.null(XTickMarks) && class(dataSubset[[eval(DateVariable)]])[1L] == 'Date') {
      Plot <- Plot + ggplot2::scale_x_date(date_breaks = XTickMarks, labels = scales::date_format("%Y-%m-%d"))
    } else if(!is.null(XTickMarks) && class(dataSubset[[eval(DateVariable)]])[1L] == 'POSIXct') {
      Plot <- Plot + ggplot2::scale_x_datetime(date_breaks = XTickMarks, labels = scales::date_format("%Y-%m-%d HH:MM:SS"))
    }

    # Prediction Intervals
    if(PredictionIntervals) {
      Plot <- Plot + ggplot2::geom_ribbon(
        ggplot2::aes(ymin = dataSubset[[7L]], ymax = dataSubset[[6L]]),
        fill = PredictionIntervalColorOuter, alpha = 0.25)
      Plot <- Plot + ggplot2::geom_ribbon(
        ggplot2::aes(ymin = dataSubset[[6L]], ymax = dataSubset[[5L]]),
        fill = PredictionIntervalColorInner, alpha = 0.25)
      Plot <- Plot + ggplot2::geom_ribbon(
        ggplot2::aes(ymin = dataSubset[[4L]], ymax = dataSubset[[5L]]),
        fill = PredictionIntervalColorOuter, alpha = 0.25) +
        ggplot2::geom_line(ggplot2::aes(y = as.numeric(dataSubset$Low95)), color = ForecastLineColor, lwd = 0.25) +
        ggplot2::geom_line(ggplot2::aes(y = as.numeric(dataSubset$Low80)), color = ForecastLineColor, lwd = 0.25) +
        ggplot2::geom_line(ggplot2::aes(y = as.numeric(dataSubset$High80)), color = ForecastLineColor, lwd = 0.25) +
        ggplot2::geom_line(ggplot2::aes(y = as.numeric(dataSubset$High95)), color = ForecastLineColor, lwd = 0.25)
    }

    # Add labels----
    Plot <- Plot + ggplot2::xlab(eval(DateVariable)) + ggplot2::ylab(eval(TargetVariable[1]))

    # Return
    return(Plot)
  }

  # Melt if multiple targets----
  if("ModelID" %chin% names(dt)) dt <- dt[ModelID == eval(TS_ModelID)]
  if(length(TargetVariable) > 1 && !EvaluationMode) {
    if(!is.null(GroupVariables)) {
      dt <- TimeSeriesMelt(data = dt, TargetVariable = TargetVariable, DateVariable = DateVariable, GroupVariables = c(GroupVariables))
      TargetVariable <- "TargetSeries"
      GroupVariables <- c("GroupVar", GroupVariables)
    } else {
      dt <- TimeSeriesMelt(data = dt, TargetVariable = TargetVariable, DateVariable = DateVariable)
      TargetVariable <- "TargetSeries"
      GroupVariables <- "GroupVar"
    }
  }

  # Ensure GroupVariables are character type----
  if(!is.null(GroupVariables)) dt[, eval(GroupVariables) := lapply(.SD, as.character), .SDcols = c(eval(GroupVariables))]

  # Make copy of dt ----
  PlotData <- data.table::copy(dt)

  # Subset columns for plotting----
  if(!is.null(GroupVariables)) {
    if('Forecast' %chin% names(PlotData)) {
      PlotData <- PlotData[, .SD, .SDcols = c('Forecast', eval(TargetVariable), eval(DateVariable), eval(GroupVariables))]
    } else {
      PlotData <- PlotData[, .SD, .SDcols = c(eval(TargetVariable), eval(DateVariable), eval(GroupVariables))]
    }
  } else {
    PlotData <- PlotData[, .SD, .SDcols = c(eval(TargetVariable), eval(DateVariable))]
  }

  # Ensure DateVariable is date type----
  if(all(class(PlotData[[eval(DateVariable)]])[1L] != 'Date')) {
    PlotData[, eval(DateVariable) := as.POSIXct(get(DateVariable))]
  }

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
    if(!is.null(XTickMarks) && class(PlotData[[eval(DateVariable)]])[1L] == 'Date') {
      Plot <- Plot + ggplot2::scale_x_date(date_breaks = XTickMarks, labels = scales::date_format("%Y-%m-%d"))
    } else if(!is.null(XTickMarks) && class(PlotData[[eval(DateVariable)]])[1L] == 'POSIXct') {
      Plot <- Plot + ggplot2::scale_x_datetime(date_breaks = XTickMarks, labels = scales::date_format("%Y-%m-%d HH:MM:SS"))
    }

    # Return
    return(Plot)
  }

  # Plot dt----
  if(Debug) print(GroupVariables)
  if(!is.null(GroupVariables)) {

    # App issue here
    if(Debug) {

      print('TimeSeriesPlotter is here 0 :::::::  ')

      print(paste0('GroupVariables length > 1 = ', length(GroupVariables) > 1L))

      print(PlotData[, .SD, .SDcols = c(GroupVariables)])
      print(unique(PlotData[, .SD, .SDcols = c(GroupVariables)]))
      print(paste0('length of unique levels = ', length(unique(PlotData[, .SD, .SDcols = c(GroupVariables)][[1L]]))))

    }




    if(Debug) print('TimeSeriesPlotter is here 1 :::::::  ')

    # If more than 1 grouping variable----
    if(length(GroupVariables) > 1L) {

      if(Debug) print('TimeSeriesPlotter is here 2 :::::::  ')

      # Combine Group Variables ----
      for(i in seq_along(GroupVariables)) PlotData[, eval(GroupVariables[i]) := paste0(eval(GroupVariables[i]),"_", get(GroupVariables[i]))]
      PlotData[, GroupVars := do.call(paste, c(.SD, sep = "_")), .SDcols = c(eval(GroupVariables))]
      PlotData[, paste0(eval(GroupVariables)) := NULL]

      # Aggregate by groups ----
      SumTable <- PlotData[, sum(get(TargetVariable), na.rm = TRUE), by = "GroupVars"][order(-V1)]
      if(is.null(LevelsToDisplay)) {
        Levels <- as.character(SumTable[seq_len(min(SumTable[, .N], NumberGroupsDisplay)), .SD, .SDcols = "GroupVars"][[1L]])
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

      # Care to see all other groups as a single group level ----
      if(DisplayOtherGroup) {
        tempData2 <- tempData[!(GroupVars %chin% LevelsToDisplay)]
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
          size = LegendTextSize)) +
        ggplot2::labs(fill = 'GroupVars')
      if(!is.null(XTickMarks) && class(tempData2[[eval(DateVariable)]])[1L] == 'Date') {
        Plot <- Plot + ggplot2::scale_x_date(date_breaks = XTickMarks, labels = scales::date_format("%Y-%m-%d"))
      } else if(!is.null(XTickMarks) && class(tempData2[[eval(DateVariable)]])[1L] == 'POSIXct') {
        Plot <- Plot + ggplot2::scale_x_datetime(date_breaks = XTickMarks, labels = scales::date_format("%Y-%m-%d HH:MM:SS"))
      }

    } else if(length(unique(PlotData[, .SD, .SDcols = c(GroupVariables)][[1L]])) > 1L) {

      if(Debug) print('TimeSeriesPlotter is here 3 :::::::  ')

      # Collapse groups----
      SumTable <- PlotData[, sum(get(TargetVariable)), by = eval(GroupVariables)][order(-V1)]

      if(Debug) print(SumTable)

      # Single group treatment----
      Levels <- as.character(SumTable[seq_len(min(NumberGroupsDisplay, SumTable[, .N]))][, .SD, .SDcols = eval(GroupVariables)][[1L]])

      if(Debug) print(Levels)

      tempData <- PlotData[get(GroupVariables) %chin% Levels]

      if(Debug) print(tempData)

      # Other groups----
      if(DisplayOtherGroup && length(Levels) > NumberGroupsDisplay) {

        if(Debug) print('TimeSeriesPlotter is here 4 :::::::  ')

        tempData2 <- PlotData[!(get(GroupVariables) %chin% Levels)]

        if(Debug) print(tempData2)

        tempData2 <- tempData2[, eval(GroupVariables) := eval(OtherGroupLabel)]

        if(Debug) print(tempData2)

        if(tolower(Aggregate) == "sum") {
          tempData2 <- tempData2[, sum(get(TargetVariable), na.rm = TRUE), by = c(eval(GroupVariables), eval(DateVariable))]
          tempData <- tempData[, sum(get(TargetVariable), na.rm = TRUE), by = c(eval(GroupVariables), eval(DateVariable))]
          data.table::setnames(tempData2, "V1", eval(TargetVariable))
          data.table::setnames(tempData, "V1", eval(TargetVariable))
        } else if(tolower(Aggregate) == "mean") {

          if(Debug) print('TimeSeriesPlotter is here 5 :::::::  ')

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

      if(Debug) print('TimeSeriesPlotter is here 6 :::::::  ')

      # Grouping variables----
      Plot <- ggplot2::ggplot(
        tempData2,
        ggplot2::aes_string(x = eval(DateVariable), y = eval(TargetVariable), color = eval(GroupVariables))) +
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
          size = LegendTextSize)) +
        ggplot2::labs(fill = 'GroupVars')
      if(!is.null(XTickMarks) && class(tempData2[[eval(DateVariable)]])[1L] == 'Date') {
        Plot <- Plot + ggplot2::scale_x_date(date_breaks = XTickMarks, labels = scales::date_format("%Y-%m-%d"))
      } else if(!is.null(XTickMarks) && class(tempData2[[eval(DateVariable)]])[1L] == 'POSIXct') {
        Plot <- Plot + ggplot2::scale_x_datetime(date_breaks = XTickMarks, labels = scales::date_format("%Y-%m-%d HH:MM:SS"))
      }
    } else {

      if(Debug) print('TimeSeriesPlotter is here 7 :::::::  ')

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
      if(!is.null(XTickMarks) && class(PlotData[[eval(DateVariable)]])[1L] == 'Date') {
        Plot <- Plot + ggplot2::scale_x_date(date_breaks = XTickMarks, labels = scales::date_format("%Y-%m-%d"))
      } else if(!is.null(XTickMarks) && class(PlotData[[eval(DateVariable)]])[1L] == 'POSIXct') {
        Plot <- Plot + ggplot2::scale_x_datetime(labels = scales::date_format("%Y-%m-%d HH:MM:SS"))
      }
    }
  } else {

    if(Debug) print('TimeSeriesPlotter is here 8 :::::::  ')

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
    if(!is.null(XTickMarks) && class(PlotData[[eval(DateVariable)]])[1L] == 'Date') {
      Plot <- Plot + ggplot2::scale_x_date(date_breaks = XTickMarks, labels = scales::date_format("%Y-%m-%d"))
    } else if(!is.null(XTickMarks) && class(PlotData[[eval(DateVariable)]])[1L] == 'POSIXct') {
      Plot <- Plot + ggplot2::scale_x_datetime(labels = scales::date_format("%Y-%m-%d HH:MM:SS"))
    }
  }

  if(Debug) print('TimeSeriesPlotter is here 9 :::::::  ')

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
