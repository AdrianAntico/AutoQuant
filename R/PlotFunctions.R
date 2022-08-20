#' @noRd
holidayNYSE <- function(year = getRmetricsOptions("currentYear")) {
  # A function implemented by Diethelm Wuertz
  # improved speed and handling of time zone by Yohan Chalabi

  # Description:
  #   Returns 'timeDate' object for full-day NYSE holidays

  # Arguments:
  #   year - an integer variable or vector for the year(s)
  #       ISO-8601 formatted as "CCYY" where easter or
  #       easter related feasts should be computed.

  # Value:
  #   Returns the holiday calendar for the NYSE formatted as
  #   'timeDate' object.

  # Details:
  #   The "New York Stock Exchange" calendar starts from year 1885.
  #   The rules are listed at the web site http://www.nyse.com.

  # Example:
  #   > holiday.NYSE(2004)
  #   [1] "America/New_York"
  #   [1] [2004-01-01] [2004-01-19] [2004-02-16] [2004-04-09]
  #   [5] [2004-05-31] [2004-07-05] [2004-09-06] [2004-11-25]

  # FUNCTION:
  library(timeDate)
  #  Settings:
  holidays <- NULL

  # Iterate years:
  for (y in year ) {
    if (y >= 1885)
      holidays <- c(holidays, as.character(USNewYearsDay(y)))
    if (y >= 1885)
      holidays <- c(holidays, as.character(USIndependenceDay(y)))
    if (y >= 1885)
      holidays <- c(holidays, as.character(USThanksgivingDay(y)))
    if (y >= 1885)
      holidays <- c(holidays, as.character(USChristmasDay(y)))
    if (y >= 1887)
      holidays <- c(holidays, as.character(USLaborDay(y)))
    if (y != 1898 & y != 1906 & y != 1907)
      holidays <- c(holidays, as.character(USGoodFriday(y)))
    if (y >= 1909 & y <= 1953)
      holidays <- c(holidays, as.character(USColumbusDay(y)))
    if (y >= 1998)
      holidays <- c(holidays, as.character(USMLKingsBirthday(y)))
    if (y >= 1896 & y <= 1953)
      holidays <- c(holidays, as.character(USLincolnsBirthday(y)))
    if (y <= 1970)
      holidays <- c(holidays, as.character(USWashingtonsBirthday(y)))
    if (y > 1970)
      holidays <- c(holidays, as.character(USPresidentsDay(y)))
    if (y == 1918 | y == 1921 | (y >= 1934 & y <= 1953))
      holidays <- c(holidays, as.character(USVeteransDay(y)))
    if (y <= 1968 | y == 1972 | y == 1976 | y == 1980)
      holidays <- c(holidays, as.character(USElectionDay(y)))
    if (y <= 1970)
      holidays <- c(holidays, as.character(USDecorationMemorialDay(y)))
    if (y >= 1971)
      holidays <- c(holidays, as.character(USMemorialDay(y)))
  }

  # Sort and Convert to 'timeDate':
  holidays <- sort(holidays)
  ans <- timeDate(format(holidays), zone = "NewYork", FinCenter = "NewYork")

  # Move Sunday Holidays to Monday:
  posix1 <- as.POSIXlt(ans, tz = "GMT")
  ans <- ans + as.integer(posix1$wday==0) * 24 * 3600

  # After July 3, 1959, move Saturday holidays to Friday
  # ... except if at the end of monthly/yearly accounting period
  # this is the last business day of a month.
  posix2 <- as.POSIXlt(as.POSIXct(ans, tz = "GMT") - 24 * 3600)
  y <- posix2$year + 1900
  m <- posix2$mon + 1
  calendar <- timeCalendar(y = y+(m+1)%/%13,
                           m = m+1-(m+1)%/%13*12, d = 1,
                           zone = "GMT", FinCenter = "GMT")
  lastday <- as.POSIXlt(calendar - 24*3600, tz = "GMT")$mday
  lon <- .last.of.nday(year = y, month = m, lastday = lastday, nday = 5)
  ExceptOnLastFriday <- timeDate(format(lon), zone = "NewYork",
                                 FinCenter = "NewYork")
  ans <- ans - as.integer(ans >= timeDate("1959-07-03",
                                          zone ="GMT", FinCenter = "GMT") &
                            as.POSIXlt(ans, tz = "GMT")$wday == 6  &
                            (ans - 24*3600) != ExceptOnLastFriday ) * 24 * 3600

  # Remove Remaining Weekend Dates:
  posix3 <- as.POSIXlt(ans, tz = "GMT")
  ans <- ans[ !(posix3$wday == 0 | posix3$wday == 6)]

  # Return Value:
  ans
}

#' @noRd
StockSymbols <- function() {
  x <- jsonlite::fromJSON("https://api.polygon.io/v3/reference/tickers?active=true&sort=ticker&order=asc&limit=1000&apiKey=hvyL7ZOsKK_5PNplOmv55tBTRd8rdA20")
  xx <- data.table::setDT(x$results)
  return(xx[, .SD, .SDcols = c(names(xx)[c(1,2,5,6,12)])])
}

#' @noRd
GetAllTickers <- function() {
  x <- jsonlite::fromJSON("https://api.polygon.io/v3/reference/tickers?active=true&sort=ticker&order=asc&limit=1000&apiKey=hvyL7ZOsKK_5PNplOmv55tBTRd8rdA20")
  xx <- data.table::setDT(x$results)
  counter <- 1000L
  while(is.list(x)) {
    print(paste0('Working on first ', counter, ' ticker symbols'))
    x <- tryCatch({jsonlite::fromJSON(paste0(x$next_url, "&apiKey=hvyL7ZOsKK_5PNplOmv55tBTRd8rdA20"))}, error = function(x) 1)
    xx <- data.table::rbindlist(list(xx, data.table::setDT(x$results)), fill = TRUE, use.names = TRUE)
    counter <- counter + 1000L
    Sys.sleep(12L)
  }
  xx <- xx[, .SD, .SDcols = c(names(xx)[c(1,2,5,6,12)])]
  data.table::fwrite(xx, file = file.path('C:/Users/Bizon/Documents/GitHub/RemixAutoML/inst/shiny-apps/AutoInsights/ticker_data.csv'))
  RemixAutoML::PostGRE_RemoveCreateAppend(
    data = xx,
    TableName = "ticker_data",
    CloseConnection = TRUE,
    CreateSchema = NULL,
    Host = "localhost",
    DBName = "RemixAutoML",
    User = "postgres",
    Port = 5432,
    Password = "Aa1028#@",
    Temporary = FALSE,
    Connection = NULL,
    Append = TRUE)
  return(xx)
}

#' @noRd
OptionsSymbols <- function() {
  x <- jsonlite::fromJSON('https://api.polygon.io/v3/reference/tickers/types?asset_class=options&locale=us&apiKey=hvyL7ZOsKK_5PNplOmv55tBTRd8rdA20')
  xx <- data.table::setDT(x$results)
  return(xx[, .SD, .SDcols = c(names(xx)[c(1,2,5,6,12)])])
}

#' @noRd
CryptoSymbols <- function() {
  x <- jsonlite::fromJSON('https://api.polygon.io/v3/reference/tickers/types?asset_class=crypto&locale=us&apiKey=hvyL7ZOsKK_5PNplOmv55tBTRd8rdA20')
  xx <- data.table::setDT(x$results)
  return(xx[, .SD, .SDcols = c(names(xx)[c(1,2,5,6,12)])])
}

#' @noRd
Financials <- function() {
  x <- jsonlite::fromJSON("https://api.polygon.io/vX/reference/financials?apiKey=hvyL7ZOsKK_5PNplOmv55tBTRd8rdA20")
}

#' @title StockData
#'
#' @description  Create stock data for plotting using StockPlot()
#'
#' @family Graphics
#' @author Adrian Antico
#'
#' @param PolyOut NULL. If NULL, data is pulled. If supplied, data is not pulled.
#' @param Type 'candlestick', 'ohlc'
#' @param Metric Stock Price, Percent Returns (use symbol for percent), Percent Log Returns (use symbol for percent), Index, Quadratic Variation
#' @param TimeAgg = 'days', 'weeks', 'months'
#' @param Symbol ticker symbol string
#' @param CompanyName company name if you have it. ends up in title, that is all
#' @param StartDate Supply a start date. E.g. '2022-01-01'
#' @param EndDate Supply an end date. E.g. `Sys.Date()`
#' @param APIKey Supply your polygon API key
#'
#' @export
StockData <- function(PolyOut = NULL,
                      Symbol = 'TSLA',
                      CompanyName = 'Tesla Inc. Common Stock',
                      Metric = 'Stock Price',
                      TimeAgg = 'days',
                      StartDate = '2022-01-01',
                      EndDate = '2022-01-01',
                      APIKey = NULL) {
  StartDate <- as.Date(StartDate)
  EndDate <- as.Date(EndDate)
  PolyOut <- jsonlite::fromJSON(paste0("https://api.polygon.io/v2/aggs/ticker/",Symbol,"/range/1/day/",StartDate, "/", EndDate, "?adjusted=true&sort=asc&limit=10000&apiKey=", APIKey))
  data <- data.table::setDT(PolyOut$results)
  datas <- data.table::data.table(Date = seq(StartDate, EndDate, 'days'))
  datas <- RemixAutoML::CreateCalendarVariables(data = datas, DateCols = 'Date', AsFactor = FALSE, TimeUnits = 'wday')
  datas <- datas[Date_wday %in% c(2L:6L)]
  datas <- datas[!Date %in% as.Date(holidayNYSE(year = c(data.table::year(StartDate),data.table::year(EndDate))))]
  if(nrow(datas) == nrow(data) + 1L) datas <- datas[seq_len(.N-1L)]
  data <- cbind(data, datas)
  if(TimeAgg == 'weeks') {
    data[, Date := lubridate::floor_date(Date, unit = 'weeks')]
    data <- data[, lapply(.SD, mean, na.rm = TRUE), .SD = c('v','vw','o','c','h','l','t','n'), by = 'Date']
  } else if(TimeAgg == 'months') {
    data[, Date := lubridate::floor_date(Date, unit = 'months')]
    data <- data[, lapply(.SD, mean, na.rm = TRUE), .SD = c('v','vw','o','c','h','l','t','n'), by = 'Date']
  } else if(TimeAgg == 'quarters') {
    data[, Date := lubridate::floor_date(Date, unit = 'quarters')]
    data <- data[, lapply(.SD, mean, na.rm = TRUE), .SD = c('v','vw','o','c','h','l','t','n'), by = 'Date']
  } else if(TimeAgg == 'years') {
    data[, Date := lubridate::floor_date(Date, unit = 'years')]
    data <- data[, lapply(.SD, mean, na.rm = TRUE), .SD = c('v','vw','o','c','h','l','t','n'), by = 'Date']
  }
  if(Metric == '% Returns') {
    for(i in c('o','c','h','l')) data[, paste0(i) := get(i) / data.table::shift(x = get(i)) - 1]
  } else if(Metric  == '% Log Returns') {
    for(i in c('o','c','h','l')) data[, paste0(i) := log(get(i)) - log(data.table::shift(x = get(i)))]
  } else if(Metric  == 'Index') {
    for(i in c('o','c','h','l')) data[, paste0(i) := get(i) / data.table::first(get(i))]
  } else if(Metric  == 'Quadratic Variation') {
    for(i in c('o','c','h','l')) data[, temp_temp := data.table::shift(x = get(i), n = 1L, fill = NA, type = 'lag')][, paste0(i) := (get(i) - temp_temp)^2][, temp_temp := NULL]
  }
  return(list(data = data, PolyOut = PolyOut, CompanyName = CompanyName, Symbol = Symbol, Metric = Metric, StartDate = StartDate, EndDate = EndDate, APIKey = APIKey))
}

#' @title StockPlot
#'
#' @description  Create a candlestick plot for stocks. See https://plotly.com/r/figure-labels/
#'
#' @family Graphics
#' @author Adrian Antico
#'
#' @param Type 'candlestick', 'ohlc'
#' @param StockDataOutput PolyOut returned from StockData()
#'
#' @export
StockPlot <- function(StockDataOutput,
                      Type = 'candlestick') {
  if(missing(StockDataOutput)) stop('StockDataOutput cannot be missing')
  if(Type == 'CandlestickPlot') Type <- 'candlestick'
  if(Type == 'OHLCPlot') Type <- 'ohlc'
  p1 <- plotly::plot_ly(
    data = StockDataOutput$data,
    x = ~Date,
    type = Type,
    open = ~o,
    close = ~c,
    high = ~h,
    low = ~l,
    decreasing = list(line = list(color = '#ff0055')),
    increasing = list(line = list(color = '#66ff00')))
  p1 <- plotly::layout(
    p = p1,
    title = if(length(StockDataOutput$CompanyName) == 0L) list(text = paste0(StockDataOutput$Symbol, ": ", StockDataOutput$StartDate, " to ", StockDataOutput$EndDate), font = 'Segoe UI') else list(text = paste0(StockDataOutput$CompanyName, " - ", StockDataOutput$Symbol, ": ", StockDataOutput$StartDate, " to ", StockDataOutput$EndDate), font = 'Segoe UI'),
    plot_bgcolor = "#f0f8ff",
    yaxis = list(title = StockDataOutput$Metric),
    xaxis = list(title = 'Date'))
  return(p1)
}

#' @title DensityPlot
#'
#' @description Density plots, by groups, with transparent continuous plots
#'
#' @family Graphics
#'
#' @param data data.table
#' @param GroupVariables = NULL
#' @param MeasureVariables = NULL
#'
#' @export
DensityPlot <- function(data, GroupVariables, MeasureVars) {
  if(length(GroupVariables) == 0L) {
    plotly::ggplotly(
      eval(ggplot2::ggplot(data, ggplot2::aes(x = get(MeasureVars))) +
             ggplot2::geom_density(alpha = 0.3) +
             RemixAutoML::ChartTheme(BackGroundColor = 'gray75', ChartColor = RColorBrewer::brewer.pal(n = 3L, name = 'Dark2')[[3L]])))
  } else {
    xx <- data.table::melt.data.table(
      data = data,
      id.vars = c(GroupVariables), measure.vars= c(MeasureVars), variable.name='Method', value.name='Value')
    plotly::ggplotly(
      eval(ggplot2::ggplot(xx, ggplot2::aes(x = Value, fill = Method)) +
             ggplot2::geom_density(alpha = 0.3) +
             RemixAutoML::ChartTheme(BackGroundColor = 'gray75', ChartColor = RColorBrewer::brewer.pal(n = 3L, name = 'Dark2')[[3L]])))
  }
}


#' @title HeatMapPlot
#'
#' @description Create heat maps with numeric or categorical dt
#'
#' @family Graphics
#' @author Adrian Antico
#'
#' @param dt Source data.table
#' @param x X-Axis variable
#' @param y Y-Axis variable
#' @param z Z-Axis variable
#' @param AggMethod 'mean', 'median', 'sum', 'sd', 'count'
#' @param PercentileBuckets_X = 0.10
#' @param PercentileBuckets_Y = 0.10
#' @param NLevels_X = 20
#' @param NLevels_Y = 20
#' @param RankLevels_X = 'mean'
#'
#' @export
HeatMapPlot <- function(dt,
                        x = NULL,
                        y = NULL,
                        z = NULL,
                        AggMethod = 'mean',
                        PercentileBuckets_X = 0.10,
                        PercentileBuckets_Y = 0.10,
                        NLevels_X = 33,
                        NLevels_Y = 33) {


  # Subset cols
  dt <- dt[, .SD, .SDcols = c(x,y,z)]

  # Build pltos
  if(class(dt[[x]])[[1L]] %in% c('numeric','integer') && class(dt[[y]])[[1L]] %in% c('numeric','integer')) {

    # rank x and y
    dt[, eval(x) := round(data.table::frank(dt[[x]]) * (1/PercentileBuckets_X) /.N) * PercentileBuckets_X]
    dt[, eval(y) := round(data.table::frank(dt[[y]]) * (1/PercentileBuckets_X) /.N) * PercentileBuckets_X]
    data.table::setnames(dt, eval(z), 'Measure_Variable')

    # Formatting
    vals <- unique(scales::rescale(c(dt[['Measure_Variable']])))
    o <- order(vals, decreasing = FALSE)
    cols <- scales::col_numeric("Purples", domain = NULL)(vals)
    colz <- setNames(data.frame(vals[o], cols[o]), NULL)

    # Create final data for plot
    fig <- plotly::plot_ly(dt, x = ~get(x), y = ~get(y), z = ~Measure_Variable, colorscale = colz, type = "heatmap")
    fig <- plotly::layout(
      p = fig,
      title = eval(z),
      xaxis = list(title = eval(x)),
      yaxis = list(title = eval(y)))

  } else if(!class(dt[[x]])[[1L]] %in% c('numeric','integer') && class(dt[[y]])[[1L]] %in% c('numeric','integer')) {

    # rank y
    dt[, eval(y) := round(data.table::frank(dt[[y]]) * (1/PercentileBuckets_X) /.N) * PercentileBuckets_X]
    data.table::setnames(dt, eval(z), 'Measure_Variable')

    # Top Y Levels
    temp <- dt[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c('Measure_Variable'), by = c(y)][order(-Measure_Variable)]
    temp <- temp[seq_len(min(NLevels_Y, temp[, .N]))][[1L]]
    dt <- dt[get(y) %in% eval(temp)]

    # Formatting
    vals <- unique(scales::rescale(c(dt[['Measure_Variable']])))
    o <- order(vals, decreasing = FALSE)
    cols <- scales::col_numeric("Purples", domain = NULL)(vals)
    colz <- setNames(data.frame(vals[o], cols[o]), NULL)

    # Create final data for plot
    fig <- plotly::plot_ly(dt, x = ~get(x), y = ~get(y), z = ~Measure_Variable, colorscale = colz, type = "heatmap")
    fig <- plotly::layout(
      p = fig,
      title = eval(z),
      xaxis = list(title = eval(x)),
      yaxis = list(title = eval(y)))


  } else if(class(dt[[x]])[[1L]] %in% c('numeric','integer') && !class(dt[[y]])[[1L]] %in% c('numeric','integer')) {

    # rank x
    dt[, eval(x) := round(data.table::frank(dt[[x]]) * (1/PercentileBuckets_X) /.N) * PercentileBuckets_X]
    data.table::setnames(dt, eval(z), 'Measure_Variable')

    # Top Y Levels
    temp <- dt[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c('Measure_Variable'), by = c(y)][order(-Measure_Variable)]
    temp <- temp[seq_len(min(NLevels_X, temp[, .N]))][[1L]]

    dt <- dt[get(y) %in% eval(temp)]

    # Formatting
    vals <- unique(scales::rescale(c(dt[['Measure_Variable']])))
    o <- order(vals, decreasing = FALSE)
    cols <- scales::col_numeric("Purples", domain = NULL)(vals)
    colz <- setNames(data.frame(vals[o], cols[o]), NULL)

    # Create final dt for plot
    fig <- plotly::plot_ly(dt, x = ~get(x), y = ~get(y), z = ~Measure_Variable, colorscale = colz, type = "heatmap")
    fig <- plotly::layout(
      p = fig,
      title = eval(z),
      xaxis = list(title = eval(x)),
      yaxis = list(title = eval(y)))

  } else {

    print("HeatMapPlot AggMethod Here")
    print(AggMethod)

    # Starter pack
    if(AggMethod == 'mean') {
      temp_y <- dt[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c(z), by = c(y)][order(-get(z))]
      temp_x <- dt[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c(z), by = c(x)][order(-get(z))]
      temp_yy <- temp_y[seq_len(min(NLevels_Y, temp_y[, .N]))][[1L]]
      temp_xx <- temp_x[seq_len(min(NLevels_X, temp_x[, .N]))][[1L]]
      dt <- dt[get(y) %in% eval(temp_yy) & get(x) %in% eval(temp_xx)]
      dt <- dt[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c(z), by = c(x,y)]
    } else if(AggMethod == 'median') {
      temp_y <- dt[, lapply(.SD, median, na.rm = TRUE), .SDcols = c(z), by = c(y)][order(-get(z))]
      temp_x <- dt[, lapply(.SD, median, na.rm = TRUE), .SDcols = c(z), by = c(x)][order(-get(z))]
      temp_y <- temp_y[seq_len(min(NLevels_Y, temp_y[, .N]))][[1L]]
      temp_x <- temp_x[seq_len(min(NLevels_X, temp_x[, .N]))][[1L]]
      dt <- dt[get(y) %in% eval(temp_y) & get(x) %in% eval(temp_x)]
      dt <- dt[, lapply(.SD, median, na.rm = TRUE), .SDcols = c(z), by = c(x,y)]
    } else if(AggMethod == 'sum') {
      temp_y <- dt[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c(z), by = c(y)][order(-get(z))]
      temp_x <- dt[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c(z), by = c(x)][order(-get(z))]
      temp_y <- temp_y[seq_len(min(NLevels_Y, temp_y[, .N]))][[1L]]
      temp_x <- temp_x[seq_len(min(NLevels_X, temp_x[, .N]))][[1L]]
      dt <- dt[get(y) %in% eval(temp_y) & get(x) %in% eval(temp_x)]
      dt <- dt[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c(z), by = c(x,y)]
    } else if(AggMethod == 'sd') {
      temp_y <- dt[, lapply(.SD, sd, na.rm = TRUE), .SDcols = c(z), by = c(y)][order(-get(z))]
      temp_x <- dt[, lapply(.SD, sd, na.rm = TRUE), .SDcols = c(z), by = c(x)][order(-get(z))]
      temp_y <- temp_y[seq_len(min(NLevels_Y, temp_y[, .N]))][[1L]]
      temp_x <- temp_x[seq_len(min(NLevels_X, temp_x[, .N]))][[1L]]
      dt <- dt[get(y) %in% eval(temp_y) & get(x) %in% eval(temp_x)]
      dt <- dt[, lapply(.SD, sd, na.rm = TRUE), .SDcols = c(z), by = c(x,y)]
    } else if(AggMethod == 'count') {
      temp_y <- dt[, lapply(.SD, .N, na.rm = TRUE), .SDcols = c(z), by = c(y)][order(-get(z))]
      temp_x <- dt[, lapply(.SD, .N, na.rm = TRUE), .SDcols = c(z), by = c(x)][order(-get(z))]
      temp_y <- temp_y[seq_len(min(NLevels_Y, temp_y[, .N]))][[1L]]
      temp_x <- temp_x[seq_len(min(NLevels_X, temp_x[, .N]))][[1L]]
      dt <- dt[get(y) %in% eval(temp_y) & get(x) %in% eval(temp_x)]
      dt <- dt[, lapply(.SD, .N, na.rm = TRUE), .SDcols = c(z), by = c(x,y)]
    }

    # Update names
    data.table::setnames(dt, eval(z), 'Measure_Variable')

    # Formatting
    xform <- list(categoryarray = c(as.character(unique(dt[[x]]))), categoryorder = "array")
    yform <- list(categoryarray = c(as.character(unique(dt[[y]]))), categoryorder = "array")

    # Create final dt for plot
    # fig <- plotly::plot_ly(dt, x = ~get(x), y = ~get(y), z = ~Measure_Variable, colorscale = colz, type = "heatmap")
    fig <- plotly::plot_ly(
      dt,
      x = ~get(x),
      y = ~get(y),
      z = ~Measure_Variable,
      colors = grDevices::colorRamp(c('aliceblue','purple')),
      type = "heatmap")
    fig <- plotly::layout(
      p = fig,
      plot_bgcolor = '#F0F8FF',
      title = eval(z),
      xaxis = list(title = eval(x)),
      yaxis = list(title = eval(y)))
  }

  # Return plot
  return(eval(fig))
}

# Correlation Matrix Updates: https://okanbulut.github.io/bigdata/visualizing-big-data.html


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
    temp1[, Imp2 := sign(Importance) * sqrt(abs(Importance)) / sum(sqrt(abs(Importance)))]
    gg <- temp1[, .N]
    if(gg > TopN) {
      temp1 <- temp1[c(1:13, (gg - 13L):gg)]
    }
  } else if(AggMethod == 'median') {
    temp <- temp[, lapply(.SD, median, na.rm = TRUE), .SDcols = c(ShapColNames)]
    temp1 <- data.table::melt.data.table(data = temp, measure.vars = names(temp), value.name = 'Importance', variable.name = 'Variable')
    temp1[, Imp2 := sign(Importance) * sqrt(abs(Importance)) / sum(sqrt(abs(Importance)))]
    gg <- temp1[, .N]
    if(gg > TopN) {
      temp1 <- temp1[c(1:13, (gg - 13L):gg)]
    }

  # TopN rows: since abs(), doesn't matter
  } else if(AggMethod == 'sd') {
    temp <- temp[, lapply(.SD, sd, na.rm = TRUE), .SDcols = c(ShapColNames)]
    temp1 <- data.table::melt.data.table(data = temp, measure.vars = names(temp), value.name = 'Importance', variable.name = 'Variable')
    temp1[, Imp2 := sign(Importance) * sqrt(abs(Importance)) / sum(sqrt(abs(Importance)))]
  } else if(AggMethod == 'absmean') {
    temp <- temp[, lapply(.SD, function(x) {
      return(abs(mean(x[which(!is.na(x))])))
    }), .SDcols = c(ShapColNames)]
    temp1 <- data.table::melt.data.table(data = temp, measure.vars = names(temp), value.name = 'Importance', variable.name = 'Variable')
    temp1[, Imp2 := sign(Importance) * sqrt(abs(Importance)) / sum(sqrt(abs(Importance)))]
  } else if(AggMethod == 'meanabs') {
    temp <- temp[, lapply(.SD, function(x) {
      return(mean(abs(x[which(!is.na(x))])))
    }), .SDcols = c(ShapColNames)]
    temp1 <- data.table::melt.data.table(data = temp, measure.vars = names(temp), value.name = 'Importance', variable.name = 'Variable')
    temp1[, Imp2 := sign(Importance) * sqrt(abs(Importance)) / sum(sqrt(abs(Importance)))]
  } else if(AggMethod == 'medianabs') {
    temp <- temp[, lapply(.SD, function(x) {
      return(median(abs(x[which(!is.na(x))])))
    }), .SDcols = c(ShapColNames)]
    temp1 <- data.table::melt.data.table(data = temp, measure.vars = names(temp), value.name = 'Importance', variable.name = 'Variable')
    temp1[, Imp2 := sign(Importance) * sqrt(abs(Importance)) / sum(sqrt(abs(Importance)))]
  } else if(AggMethod == 'absmedian') {
    temp <- temp[, lapply(.SD, function(x) {
      return(abs(median(x[which(!is.na(x))])))
    }), .SDcols = c(ShapColNames)]
    temp1 <- data.table::melt.data.table(data = temp, measure.vars = names(temp), value.name = 'Importance', variable.name = 'Variable')
    temp1[, Imp2 := sign(Importance) * sqrt(abs(Importance)) / sum(sqrt(abs(Importance)))]
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
#' @param Method 'spearman' default, 'pearson' otherwise
#'
#' @examples
#' \dontrun{
#' data <- data.table::fread(file.choose())
#' CorrVars <- c('Weekly_Sales', 'XREG1', 'XREG2', 'XREG3')
#' p <- cor(data[, .SD, .SDcols = c(CorrVars)])
#' p1 <- heatmaply::heatmaply_cor(
#'   p,
#'   colors = c('red', 'white', 'blue'),
#'   xlab = "Features",
#'   ylab = "Features",
#'   k_col = 2,
#'   k_row = 2)
#' }
#'
#' @export
CorrMatrixPlot <- function(data = NULL,
                           CorrVars = NULL,
                           Method = 'spearman') {

  # Plot
  nafree <- na.omit(data[, .SD, .SDcols = c(CorrVars)])
  for(i in seq_along(names(nafree))) {
    yy <- names(nafree)[i]
    zz <- nchar(yy)
    print(yy)
    print(substr(x = yy, start = max(0L, zz - 40L), stop = nchar(yy)))
    data.table::setnames(nafree, yy, substr(x = yy, start = max(0L, zz - 40L), stop = nchar(yy)))
  }
  p <- cor(method = 'spearman', x = nafree)
  print(p)
  p1 <- heatmaply::heatmaply_cor(
    p,
    colors = c('darkred', 'pink', 'black', 'lightblue', 'darkblue'),
    xlab = NULL,
    ylab = NULL,
    k_col = 2,
    k_row = 2)

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
#' @examples
#' \dontrun{
#' # Load packages
#' library(RemixAutoML)
#' library(data.table)
#'
#' # Load data
#' data <- data.table::fread(file = file.path('C:/Users/Bizon/Documents/GitHub/BenchmarkData1.csv'))
#'
#' # Run function
#' RemixAutoML:::ViolinPlot(
#'   data = data,
#'   XVar = 'Region',
#'   YVar = 'Weekly_Sales',
#'   FacetVar1 = 'Store',
#'   FacetVar2 = NULL,
#'   SampleSize = 1000000L,
#'   FillColor = 'gray',
#'   YTicks = 'Default',
#'   XTicks = 'Default',
#'   TextSize = 12,
#'   AngleX = 90,
#'   AngleY = 0,
#'   ChartColor = 'lightsteelblue1',
#'   BorderColor = 'darkblue',
#'   TextColor = 'darkblue',
#'   GridColor = 'white',
#'   BackGroundColor = 'gray95',
#'   SubTitleColor = 'blue',
#'   LegendPosition = 'bottom',
#'   LegendBorderSize = 0.50,
#'   LegendLineType = 'solid',
#'   Debug = FALSE)
#'
#' # Step through function
#' # XVar = 'Region'
#' # YVar = 'Weekly_Sales'
#' # FacetVar1 = 'Store'
#' # FacetVar2 = NULL
#' # SampleSize = 1000000L
#' # FillColor = 'gray'
#' # YTicks = 'Default'
#' # XTicks = 'Default'
#' # TextSize = 12
#' # AngleX = 90
#' # AngleY = 0
#' # ChartColor = 'lightsteelblue1'
#' # BorderColor = 'darkblue'
#' # TextColor = 'darkblue'
#' # GridColor = 'white'
#' # BackGroundColor = 'gray95'
#' # SubTitleColor = 'blue'
#' # LegendPosition = 'bottom'
#' # LegendBorderSize = 0.50
#' # LegendLineType = 'solid'
#' # Debug = FALSE
#' }
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
  if(Debug) {
    print(paste0('BOX PLOT HERE: ,', data[,.N], ' > ', SampleSize, ' == ', data[,.N] > SampleSize))
    print(paste0('BOX PLOT HERE: data[,.N] == ', data[,.N]))
    print(paste0('BOX PLOT HERE: SampleSize == ', SampleSize))
  }
  c1 <- as.numeric(data[,.N])
  c2 <- as.numeric(SampleSize)
  if(!is.null(SampleSize)) if(c1 > c2) data <- data[order(runif(.N))][seq_len(SampleSize)]

  # Used multiple times
  X_and_Y <- length(XVar) != 0 && length(YVar) != 0

  # Check class of XVar
  if(X_and_Y) {
    if(any(c('Date','POSIXct','POSIXt') %in% tryCatch({class(data[[XVar]])}, error = function(x) NULL))) {
      class_x <- 'Date'
    } else {
      class_x <- NULL
      if(any(c('numeric','integer') %in% tryCatch({class(data[[XVar]])}, error = function(x) NULL))) {
        data[, eval(XVar) := as.character(get(XVar))]
      }
    }
  } else {
    class_x <- NULL
  }

  # Create base plot object
  if(Debug) print('Create Plot with only data')
  if(X_and_Y) {
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
  if(X_and_Y) {
    p1 <- p1 + ggplot2::labs(title = paste0('BoxPlot by ', stringr::str_to_title(gsub(pattern = '_', replacement = ' ', x = XVar))), subtitle = 'Blue line = mean(Y)', caption = 'RemixAutoML')
  } else {
    p1 <- p1 + ggplot2::labs(title = 'Violin Plot', subtitle = 'Blue line = mean(Y)', caption = 'RemixAutoML')
  }

  # Labels
  if(X_and_Y) {
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
  if('Date' %in% class_x) {
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
#' @examples
#' \dontrun{
#' # Load packages
#' library(RemixAutoML)
#' library(data.table)
#'
#' # Load data
#' data <- data.table::fread(file = file.path('C:/Users/Bizon/Documents/GitHub/BenchmarkData1.csv'))
#'
#' # Run function
#' RemixAutoML:::BoxPlot(
#'   data = data,
#'   XVar = 'Region',
#'   YVar = 'Weekly_Sales',
#'   FacetVar1 = 'Store',
#'   FacetVar2 = NULL,
#'   SampleSize = 1000000L,
#'   FillColor = 'gray',
#'   OutlierSize = 0.10,
#'   OutlierColor = 'blue',
#'   YTicks = 'Default',
#'   XTicks = 'Default',
#'   TextSize = 12,
#'   AngleX = 90,
#'   AngleY = 0,
#'   ChartColor = 'lightsteelblue1',
#'   BorderColor = 'darkblue',
#'   TextColor = 'darkblue',
#'   GridColor = 'white',
#'   BackGroundColor = 'gray95',
#'   SubTitleColor = 'blue',
#'   LegendPosition = 'bottom',
#'   LegendBorderSize = 0.50,
#'   LegendLineType = 'solid',
#'   Debug = FALSE)
#'
#' # Step through function
#' # XVar = 'Region'
#' # YVar = 'Weekly_Sales'
#' # FacetVar1 = 'Store'
#' # FacetVar2 = 'Dept'
#' # SampleSize = 1000000L
#' # FillColor = 'gray'
#' # OutlierSize = 0.10
#' # OutlierColor = 'blue'
#' # YTicks = 'Default'
#' # XTicks = 'Default'
#' # TextSize = 12
#' # AngleX = 90
#' # AngleY = 0
#' # ChartColor = 'lightsteelblue1'
#' # BorderColor = 'darkblue'
#' # TextColor = 'darkblue'
#' # GridColor = 'white'
#' # BackGroundColor = 'gray95'
#' # SubTitleColor = 'blue'
#' # LegendPosition = 'bottom'
#' # LegendBorderSize = 0.50
#' # LegendLineType = 'solid'
#' # Debug = FALSE
#' }
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
  if(Debug) {
    print(paste0('BOX PLOT HERE: ,', data[,.N], ' > ', SampleSize, ' == ', data[,.N] > SampleSize))
    print(paste0('BOX PLOT HERE: data[,.N] == ', data[,.N]))
    print(paste0('BOX PLOT HERE: SampleSize == ', SampleSize))
  }
  c1 <- as.numeric(data[,.N])
  c2 <- as.numeric(SampleSize)
  if(!is.null(SampleSize)) if(c1 > c2) data <- data[order(runif(.N))][seq_len(SampleSize)]

  # Used multiple times
  X_and_Y <- length(XVar) != 0 && length(YVar) != 0

  # Check class of XVar
  if(X_and_Y) {
    if(any(c('Date','POSIXct','POSIXt') %in% tryCatch({class(data[[XVar]])}, error = function(x) NULL))) {
      class_x <- 'Date'
    } else {
      class_x <- NULL
      if(any(c('numeric','integer') %in% tryCatch({class(data[[XVar]])}, error = function(x) NULL))) {
        data[, eval(XVar) := as.character(get(XVar))]
      }
    }
  } else {
    class_x <- NULL
  }

  # Create base plot object
  if(Debug) print('Create Plot with only data')
  if(X_and_Y) {
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
  if(X_and_Y) {
    p1 <- p1 + ggplot2::labs(title = paste0('Box Plot by ', stringr::str_to_title(gsub(pattern = '_', replacement = ' ', x = XVar))), subtitle = 'Blue line = mean(Y)', caption = 'RemixAutoML')
  } else {
    p1 <- p1 + ggplot2::labs(title = 'BoxPlot', subtitle = 'Blue line = mean(Y)', caption = 'RemixAutoML')
  }

  # Labels
  if(X_and_Y) {
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
  if('Date' %in% class_x) {
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
#' @param ColorVar Column name of Group Variable for distinct colored histograms by group levels
#' @param AggMethod Choose from 'mean', 'sum', 'sd', and 'median'
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
#' @examples
#' \dontrun{
#' # Load packages
#' library(RemixAutoML)
#' library(data.table)
#'
#' # Load data
#' data <- data.table::fread(file = file.path('C:/Users/Bizon/Documents/GitHub/BenchmarkData1.csv'))
#'
#' # Run function
#' RemixAutoML:::BarPlot(
#'   data = data,
#'   XVar = 'Region',
#'   YVar = 'Weekly_Sales',
#'   AggMethod = 'mean',
#'   ColorVar = NULL,
#'   FacetVar1 = 'Store',
#'   FacetVar2 = 'Dept',
#'   SampleSize = 1000000L,
#'   FillColor = 'gray',
#'   YTicks = 'Default',
#'   XTicks = 'Default',
#'   TextSize = 12,
#'   AngleX = 90,
#'   AngleY = 0,
#'   ChartColor = 'lightsteelblue1',
#'   BorderColor = 'darkblue',
#'   TextColor = 'darkblue',
#'   GridColor = 'white',
#'   BackGroundColor = 'gray95',
#'   SubTitleColor = 'blue',
#'   LegendPosition = 'bottom',
#'   LegendBorderSize = 0.50,
#'   LegendLineType = 'solid',
#'   Debug = FALSE)
#'
#' # Step through function
#' # XVar = 'Region'
#' # YVar = 'Weekly_Sales'
#' # AggMethod = 'mean'
#' # ColorVar = NULL
#' # FacetVar1 = NULL
#' # FacetVar2 = NULL
#' # SampleSize = 1000000L
#' # FillColor = 'gray'
#' # YTicks = 'Default'
#' # XTicks = 'Default'
#' # TextSize = 12
#' # AngleX = 90
#' # AngleY = 0
#' # ChartColor = 'lightsteelblue1'
#' # BorderColor = 'darkblue'
#' # TextColor = 'darkblue'
#' # GridColor = 'white'
#' # BackGroundColor = 'gray95'
#' # SubTitleColor = 'blue'
#' # LegendPosition = 'bottom'
#' # LegendBorderSize = 0.50
#' # LegendLineType = 'solid'
#' # Debug = FALSE
#' }
#'
#' @export
BarPlot <- function(data = NULL,
                    XVar = NULL,
                    YVar = NULL,
                    AggMethod = 'mean',
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
  if(Debug) {
    print(paste0('BOX PLOT HERE: ,', data[,.N], ' > ', SampleSize, ' == ', data[,.N] > SampleSize))
    print(paste0('BOX PLOT HERE: data[,.N] == ', data[,.N]))
    print(paste0('BOX PLOT HERE: SampleSize == ', SampleSize))
  }
  c1 <- as.numeric(data[,.N])
  c2 <- as.numeric(SampleSize)
  if(!is.null(SampleSize)) if(c1 > c2) data <- data[order(runif(.N))][seq_len(SampleSize)]

  # Used multiple times
  check1 <- length(XVar) != 0 && length(YVar) != 0
  check2 <- length(XVar) == 0 && length(YVar) != 0
  check3 <- length(XVar) != 0 && length(YVar) == 0

  # Create base plot object
  if(Debug) print('Create Plot with only data')
  numvars <- c()
  byvars <- c()
  if(check1) {
    if(length(ColorVar) != 0) {
      if(any(tryCatch({class(data[[eval(YVar)]])}, error = function(x) "bla") %in% c('numeric','integer'))) {
        numvars <- unique(c(numvars, YVar))
      } else {
        byvars <- unique(c(byvars, YVar))
      }
      if(any(tryCatch({class(data[[eval(XVar)]])}, error = function(x) "bla") %in% c('numeric','integer'))) {
        if(length(numvars) > 0) {
          x <- length(unique(data[[XVar]]))
          y <- length(unique(data[[YVar]]))
          if(x > y) {
            byvars <- unique(c(byvars, YVar))
            numvars[1L] <- XVar
          } else {
            byvars <- unique(c(byvars, XVar))
          }
        } else {
          numvars <- unique(c(numvars, XVar))
        }
      } else {
        byvars <- unique(c(byvars, XVar))
      }
      if(any(tryCatch({class(data[[eval(ColorVar)]])}, error = function(x) "bla") %in% c('numeric','integer'))) {
        numvars <- unique(c(numvars, ColorVar))
      } else {
        byvars <- unique(c(byvars, ColorVar))
      }
      if(any(tryCatch({class(data[[eval(FacetVar1)]])}, error = function(x) "bla") %in% c('numeric','integer'))) {
        numvars <- unique(c(numvars, FacetVar1))
      } else {
        byvars <- unique(c(byvars, FacetVar1))
      }
      if(any(tryCatch({class(data[[eval(FacetVar2)]])}, error = function(x) "bla") %in% c('numeric','integer'))) {
        numvars <- unique(c(numvars, FacetVar2))
      } else {
        byvars <- unique(c(byvars, FacetVar2))
      }
      if(!is.null(byvars)) {
        temp <- data[, lapply(.SD, function(x) eval(parse(text = paste0(AggMethod, '(x)')))), .SDcols = c(numvars), by = c(byvars)]
        for(i in byvars) {
          if(class(temp[[i]]) %in% c('numeric','integer')) {
            temp[, eval(i) := as.character(get(i))]
          }
        }
      } else {
        temp <- data[, lapply(.SD, function(x) eval(parse(text = paste0(AggMethod, '(x)')))), .SDcols = c(numvars)]
      }
      p1 <- ggplot2::ggplot(data = temp, ggplot2::aes(x = get(XVar), y = get(YVar), fill = as.factor(get(ColorVar))))
      p1 <- p1 + ggplot2::geom_bar(stat = 'summary', fun = 'sum')
      p1 <- p1 + ggplot2::labs(fill = eval(ColorVar))
      p1 <- p1 + ggplot2::xlab(eval(XVar)) + ggplot2::ylab(eval(YVar))
    } else {
      if(any(tryCatch({class(data[[eval(YVar)]])}, error = function(x) "bla") %in% c('numeric','integer'))) {
        numvars <- unique(c(numvars, YVar))
      } else {
        byvars <- unique(c(byvars, YVar))
      }
      if(any(tryCatch({class(data[[eval(XVar)]])}, error = function(x) "bla") %in% c('numeric','integer'))) {
        if(length(numvars) > 0) {
          x <- length(unique(data[[XVar]]))
          y <- length(unique(data[[YVar]]))
          if(x > y) {
            byvars <- unique(c(byvars, YVar))
            numvars[1L] <- XVar
          } else {
            byvars <- unique(c(byvars, XVar))
          }
        } else {
          numvars <- unique(c(numvars, XVar))
        }
      } else {
        byvars <- unique(c(byvars, XVar))
      }
      if(any(tryCatch({class(data[[eval(FacetVar1)]])}, error = function(x) "bla") %in% c('numeric','integer'))) {
        numvars <- unique(c(numvars, FacetVar1))
      } else {
        byvars <- unique(c(byvars, FacetVar1))
      }
      if(any(tryCatch({class(data[[eval(FacetVar2)]])}, error = function(x) "bla") %in% c('numeric','integer'))) {
        numvars <- unique(c(numvars, FacetVar2))
      } else {
        byvars <- unique(c(byvars, FacetVar2))
      }
      if(!is.null(byvars)) {
        temp <- data[, lapply(.SD, function(x) eval(parse(text = paste0(AggMethod, '(x)')))), .SDcols = c(numvars), by = c(byvars)]
        for(i in byvars) {
          if(class(temp[[i]]) %in% c('numeric','integer')) {
            temp[, eval(i) := as.character(get(i))]
          }
        }
      } else {
        temp <- data[, lapply(.SD, function(x) eval(parse(text = paste0(AggMethod, '(x)')))), .SDcols = c(numvars)]
      }
      p1 <- ggplot2::ggplot(data = temp, ggplot2::aes(x = get(XVar), y = get(YVar)))
      p1 <- p1 + ggplot2::geom_bar(stat = 'summary', fun = 'sum', fill = FillColor)
      p1 <- p1 + ggplot2::xlab(eval(XVar)) + ggplot2::ylab(eval(YVar))
    }
  } else if(check2) {
    if(length(ColorVar) != 0) {
      if(any(tryCatch({class(data[[eval(YVar)]])}, error = function(x) "bla") %in% c('numeric','integer'))) {
        numvars <- unique(c(numvars, YVar))
      } else {
        byvars <- unique(c(byvars, YVar))
      }
      if(any(tryCatch({class(data[[eval(ColorVar)]])}, error = function(x) "bla") %in% c('numeric','integer'))) {
        numvars <- unique(c(numvars, ColorVar))
      } else {
        byvars <- unique(c(byvars, ColorVar))
      }
      if(any(tryCatch({class(data[[eval(FacetVar1)]])}, error = function(x) "bla") %in% c('numeric','integer'))) {
        numvars <- unique(c(numvars, FacetVar1))
      } else {
        byvars <- unique(c(byvars, FacetVar1))
      }
      if(any(tryCatch({class(data[[eval(FacetVar2)]])}, error = function(x) "bla") %in% c('numeric','integer'))) {
        numvars <- unique(c(numvars, FacetVar2))
      } else {
        byvars <- unique(c(byvars, FacetVar2))
      }
      if(!is.null(byvars)) {
        temp <- data[, lapply(.SD, function(x) eval(parse(text = paste0(AggMethod, '(x)')))), .SDcols = c(numvars), by = c(byvars)]
      } else {
        temp <- data[, lapply(.SD, function(x) eval(parse(text = paste0(AggMethod, '(x)')))), .SDcols = c(numvars)]
      }
      p1 <- ggplot2::ggplot(data = temp, ggplot2::aes(x = get(YVar), fill = as.factor(get(ColorVar))))
      p1 <- p1 + ggplot2::geom_bar(stat = 'summary', fun = 'sum')
      p1 <- p1 + ggplot2::labs(fill = eval(ColorVar))
      p1 <- p1 + ggplot2::xlab(eval(YVar))
    } else {
      if(any(tryCatch({class(data[[eval(YVar)]])}, error = function(x) "bla") %in% c('numeric','integer'))) {
        numvars <- unique(c(numvars, YVar))
      } else {
        byvars <- unique(c(byvars, YVar))
      }
      if(any(tryCatch({class(data[[eval(ColorVar)]])}, error = function(x) "bla") %in% c('numeric','integer'))) {
        numvars <- unique(c(numvars, ColorVar))
      } else {
        byvars <- unique(c(byvars, ColorVar))
      }
      if(any(tryCatch({class(data[[eval(FacetVar1)]])}, error = function(x) "bla") %in% c('numeric','integer'))) {
        numvars <- unique(c(numvars, ColorVar))
      } else {
        byvars <- unique(c(byvars, ColorVar))
      }
      if(any(tryCatch({class(data[[eval(FacetVar2)]])}, error = function(x) "bla") %in% c('numeric','integer'))) {
        numvars <- unique(c(numvars, ColorVar))
      } else {
        byvars <- unique(c(byvars, ColorVar))
      }
      if(!is.null(byvars)) {
        temp <- data[, .N, by = c(byvars)]
        data.table::setnames(temp, 'N', byvars)
      } else {
        temp <- data[, lapply(.SD, function(x) eval(parse(text = paste0(AggMethod, '(x)')))), .SDcols = c(numvars)]
      }
      p1 <- ggplot2::ggplot(data = data, ggplot2::aes(x = get(YVar)))
      p1 <- p1 + ggplot2::geom_bar(stat = 'summary', fun = 'sum', fill = FillColor)
      p1 <- p1 + ggplot2::xlab(eval(YVar))
    }
  } else if(check3) {
    if(length(ColorVar) != 0) {
      if(any(tryCatch({class(data[[eval(XVar)]])}, error = function(x) "bla") %in% c('numeric','integer'))) {
        numvars <- unique(c(numvars, XVar))
      } else {
        byvars <- unique(c(byvars, XVar))
      }
      if(any(tryCatch({class(data[[eval(ColorVar)]])}, error = function(x) "bla") %in% c('numeric','integer'))) {
        numvars <- unique(c(numvars, ColorVar))
      } else {
        byvars <- unique(c(byvars, ColorVar))
      }
      if(any(tryCatch({class(data[[eval(FacetVar1)]])}, error = function(x) "bla") %in% c('numeric','integer'))) {
        numvars <- unique(c(numvars, FacetVar1))
      } else {
        byvars <- unique(c(byvars, FacetVar1))
      }
      if(any(tryCatch({class(data[[eval(FacetVar2)]])}, error = function(x) "bla") %in% c('numeric','integer'))) {
        numvars <- unique(c(numvars, FacetVar2))
      } else {
        byvars <- unique(c(byvars, FacetVar2))
      }
      if(!is.null(byvars)) {
        temp <- data[, lapply(.SD, function(x) eval(parse(text = paste0(AggMethod, '(x)')))), .SDcols = c(numvars), by = c(byvars)]
      } else {
        temp <- data[, lapply(.SD, function(x) eval(parse(text = paste0(AggMethod, '(x)')))), .SDcols = c(numvars)]
      }
      p1 <- ggplot2::ggplot(data = data, ggplot2::aes(x = get(XVar), fill = as.factor(get(ColorVar))))
      p1 <- p1 + ggplot2::geom_bar(stat = 'summary', fun = 'sum')
      p1 <- p1 + ggplot2::labs(fill = eval(ColorVar))
      p1 <- p1 + ggplot2::xlab(eval(XVar))
    } else {
      if(any(tryCatch({class(data[[eval(XVar)]])}, error = function(x) "bla") %in% c('numeric','integer'))) {
        numvars <- unique(c(numvars, XVar))
      } else {
        byvars <- unique(c(byvars, XVar))
      }
      if(any(tryCatch({class(data[[eval(ColorVar)]])}, error = function(x) "bla") %in% c('numeric','integer'))) {
        numvars <- unique(c(numvars, ColorVar))
      } else {
        byvars <- unique(c(byvars, ColorVar))
      }
      if(any(tryCatch({class(data[[eval(FacetVar1)]])}, error = function(x) "bla") %in% c('numeric','integer'))) {
        numvars <- unique(c(numvars, FacetVar1))
      } else {
        byvars <- unique(c(byvars, FacetVar1))
      }
      if(any(tryCatch({class(data[[eval(FacetVar2)]])}, error = function(x) "bla") %in% c('numeric','integer'))) {
        numvars <- unique(c(numvars, FacetVar2))
      } else {
        byvars <- unique(c(byvars, FacetVar2))
      }
      if(!is.null(byvars)) {
        temp <- data[, .N, by = c(byvars)]
        data.table::setnames(temp, 'N', byvars)
      } else {
        temp <- data[, lapply(.SD, function(x) eval(parse(text = paste0(AggMethod, '(x)')))), .SDcols = c(numvars)]
      }
      p1 <- ggplot2::ggplot(data = data, ggplot2::aes(x = get(XVar)))
      p1 <- p1 + ggplot2::geom_bar(stat = 'summary', fun = 'sum', fill = FillColor)
      p1 <- p1 + ggplot2::xlab(eval(XVar))
    }
  } else {
    stop('XVar and YVar cannot both be NULL')
  }

  # Create Plot labs
  if(Debug) print('Create Plot labs')
  p1 <- p1 + ggplot2::labs(title = 'Bar Plot', caption = 'RemixAutoML')

  # Add faceting (returns no faceting in none was requested)
  p1 <- AddFacet(p1, fv1=FacetVar1, fv2=FacetVar2, Exclude = 'None', Debug = Debug)

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
#' @param ColorVar Column name of Group Variable for distinct colored histograms by group levels
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
#' @examples
#' \dontrun{
#' # Load packages
#' library(RemixAutoML)
#' library(data.table)
#'
#' # Load data
#' data <- data.table::fread(file = file.path('C:/Users/Bizon/Documents/GitHub/BenchmarkData1.csv'))
#'
#' # Run function
#' p1 <- RemixAutoML:::HistPlot(
#'   data = data,
#'   XVar = NULL,
#'   YVar = 'Weekly_Sales',
#'   ColorVar = 'Region',
#'   FacetVar1 = 'Store',
#'   FacetVar2 = 'Dept',
#'   SampleSize = 1000000L,
#'   Bins = 20,
#'   FillColor = 'gray',
#'   YTicks = 'Default',
#'   XTicks = 'Default',
#'   TextSize = 12,
#'   AngleX = 90,
#'   AngleY = 0,
#'   ChartColor = 'lightsteelblue1',
#'   BorderColor = 'darkblue',
#'   TextColor = 'darkblue',
#'   GridColor = 'white',
#'   BackGroundColor = 'gray95',
#'   SubTitleColor = 'blue',
#'   LegendPosition = 'bottom',
#'   LegendBorderSize = 0.50,
#'   LegendLineType = 'solid',
#'   Debug = FALSE)
#'
#' # Step through function
#' # # plotly::ggplotly(p1)
#' # XVar = NULL
#' # YVar = 'Weekly_Sales'
#' # AggMethod = 'mean'
#' # ColorVar = 'Region'
#' # FacetVar1 = NULL
#' # FacetVar2 = NULL
#' # Bins = 20
#' # SampleSize = 1000000L
#' # FillColor = 'gray'
#' # YTicks = 'Default'
#' # XTicks = 'Default'
#' # TextSize = 12
#' # AngleX = 90
#' # AngleY = 0
#' # ChartColor = 'lightsteelblue1'
#' # BorderColor = 'darkblue'
#' # TextColor = 'darkblue'
#' # GridColor = 'white'
#' # BackGroundColor = 'gray95'
#' # SubTitleColor = 'blue'
#' # LegendPosition = 'bottom'
#' # LegendBorderSize = 0.50
#' # LegendLineType = 'solid'
#' # Debug = FALSE
#' # Bins
#' }
#'
#' @export
HistPlot <- function(data = NULL,
                     XVar = NULL,
                     YVar = NULL,
                     ColorVar = NULL,
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
                     ChartColor = 'aliceblue',
                     BorderColor = 'darkblue',
                     TextColor = 'darkblue',
                     GridColor = '#d3d3e0',
                     BackGroundColor = 'gray95',
                     SubTitleColor = 'blue',
                     LegendPosition = 'bottom',
                     LegendBorderSize = 0.50,
                     LegendLineType = 'solid',
                     Debug = FALSE) {

  # Cap number of records
  if(Debug) {
    print(paste0('BOX PLOT HERE: ,', data[,.N], ' > ', SampleSize, ' == ', data[,.N] > SampleSize))
    print(paste0('BOX PLOT HERE: data[,.N] == ', data[,.N]))
    print(paste0('BOX PLOT HERE: SampleSize == ', SampleSize))
  }
  c1 <- as.numeric(data[,.N])
  c2 <- as.numeric(SampleSize)
  if(!is.null(SampleSize)) if(c1 > c2) data <- data[order(runif(.N))][seq_len(SampleSize)]

  # Define Plotting Variable
  if(Debug) print('YTicks Update')
  if(length(YVar) == 0) YVar <- XVar
  if(length(YVar) == 0) stop('XVar and YVar cannot both be NULL')

  # Create base plot object
  if(Debug) print('Create Plot with only data')
  if(!is.null(ColorVar)) {
    p1 <- ggplot2::ggplot(data = data, ggplot2::aes(x = get(YVar), fill = get(ColorVar)))
  } else {
    p1 <- ggplot2::ggplot(data = data, ggplot2::aes(x = get(YVar)))
  }

  # Box Plot Call
  if(Debug) print('Create Histogram')
  p1 <- p1 + ggplot2::geom_histogram(bins = Bins)

  # Add Horizontal Line for Mean Y
  if(Debug) print('Create Plot Horizontal Line')
  p1 <- p1 + ggplot2::geom_vline(linetype="longdash", color = 'darkblue', xintercept = eval(mean(data[[eval(YVar)]], na.rm = TRUE)))
  p1 <- p1 + ggplot2::geom_vline(linetype="longdash", color = 'darkred', xintercept = eval(median(data[[eval(YVar)]], na.rm = TRUE)))
  p1 <- p1 + ggplot2::geom_vline(linetype="longdash", color = 'darkred', xintercept = eval(quantile(data[[eval(YVar)]], na.rm = TRUE, probs = 0.05)[[1L]]))
  p1 <- p1 + ggplot2::geom_vline(linetype="longdash", color = 'darkred', xintercept = eval(quantile(data[[eval(YVar)]], na.rm = TRUE, probs = 0.95)[[1L]]))

  # Create Plot labs
  if(Debug) print('Create Plot labs')
  p1 <- p1 + ggplot2::labs(title = 'Histogram', subtitle = 'Blue line: mean(X), Red lines: q5, q50, q95', caption = 'RemixAutoML')

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

  # Rename legend
  if(!is.null(ColorVar)) p1 <- p1 + ggplot2::scale_fill_discrete(name = ColorVar)

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


#' Automated Word Frequency and Word Cloud Creation
#'
#' This function builds a word frequency table and a word cloud. It prepares data, cleans text, and generates output.
#' @author Adrian Antico
#' @family EDA
#' @param data Source data table
#' @param TextColName A string name for the column
#' @param GroupColName Set to NULL to ignore, otherwise set to Cluster column name (or factor column name)
#' @param GroupLevel Must be set if GroupColName is defined. Set to cluster ID (or factor level)
#' @param RemoveEnglishStopwords Set to TRUE to remove English stop words, FALSE to ignore
#' @param Stemming Set to TRUE to run stemming on your text data
#' @param StopWords Add your own stopwords, in vector format
#' @examples
#' \dontrun{
#' data <- data.table::data.table(
#' DESCR = c(
#'   "Gru", "Gru", "Gru", "Gru", "Gru", "Gru", "Gru",
#'   "Gru", "Gru", "Gru", "Gru", "Gru", "Gru", "Urkle",
#'   "Urkle", "Urkle", "Urkle", "Urkle", "Urkle", "Urkle",
#'   "Gru", "Gru", "Gru", "bears", "bears", "bears",
#'   "bears", "bears", "bears", "smug", "smug", "smug", "smug",
#'   "smug", "smug", "smug", "smug", "smug", "smug",
#'   "smug", "smug", "smug", "smug", "smug", "eats", "eats",
#'   "eats", "eats", "eats", "eats", "beats", "beats", "beats", "beats",
#'   "beats", "beats", "beats", "beats", "beats", "beats",
#'   "beats", "science", "science", "Dwigt", "Dwigt", "Dwigt", "Dwigt",
#'   "Dwigt", "Dwigt", "Dwigt", "Dwigt", "Dwigt", "Dwigt",
#'   "Schrute", "Schrute", "Schrute", "Schrute", "Schrute",
#'   "Schrute", "Schrute", "James", "James", "James", "James",
#'   "James", "James", "James", "James", "James", "James",
#'   "Halpert", "Halpert", "Halpert", "Halpert",
#'   "Halpert", "Halpert", "Halpert", "Halpert"))
#' data <- AutoWordFreq(
#'   data,
#'   TextColName = "DESCR",
#'   GroupColName = NULL,
#'   GroupLevel = NULL,
#'   RemoveEnglishStopwords = FALSE,
#'   Stemming = FALSE,
#'   StopWords = c("Bla"))
#' }
#' @export
AutoWordFreq <- function(data,
                         TextColName = "DESCR",
                         GroupColName = "ClusterAllNoTarget",
                         GroupLevel = 0,
                         RemoveEnglishStopwords = TRUE,
                         Stemming = TRUE,
                         StopWords = c("bla",
                                       "bla2")) {
  # Check data.table
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  # Ensure stringCol is character (not factor)
  if(!is.character(data[[eval(TextColName)]])) data[, eval(TextColName) := as.character(get(TextColName))]

  # Prepare data
  if(is.null(GroupColName)) {
    desc <- tm::Corpus(tm::VectorSource(data[[eval(TextColName)]]))
  } else {
    if(!is.character(data[[GroupColName]])) {
      data[, eval(GroupColName) := as.character(get(GroupColName))]
      desc <- tm::Corpus(tm::VectorSource(data[get(GroupColName) == eval(GroupLevel)][[eval(TextColName)]]))
    }
  }

  # Clean text
  toSpace <- tm::content_transformer(function(x , pattern) gsub(pattern, " ", x))
  text <- tm::tm_map(desc, toSpace, "/")
  text <- tm::tm_map(text, toSpace, "@")
  text <- tm::tm_map(text, toSpace, "\\|")

  # Convert the text to lower case
  text <- tm::tm_map(text, tm::content_transformer(tolower))

  # Remove numbers
  text <- tm::tm_map(text, tm::removeNumbers)

  # Remove english common stopwords
  if(RemoveEnglishStopwords)
    text <- tm::tm_map(text, tm::removeWords, tm::stopwords("english"))

  # specify your stopwords as a character vector
  text <- tm::tm_map(text, tm::removeWords, StopWords)

  # Remove punctuations
  text <- tm::tm_map(text, tm::removePunctuation)

  # Eliminate extra white spaces
  text <- tm::tm_map(text, tm::stripWhitespace)

  # Text stemming
  if(Stemming) text <- tm::tm_map(text, tm::stemDocument)

  # Finalize
  dtm <- tm::TermDocumentMatrix(text)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m), decreasing = TRUE)
  d <- data.table::data.table(word = names(v), freq = v)
  print(head(d, 10))

  # Word Cloud
  xx <- wordcloud::wordcloud(
    words = d$word,
    freq = d$freq,
    min.freq = 1,
    max.words = 200,
    random.order = FALSE,
    rot.per = 0.35,
    colors = RColorBrewer::brewer.pal(8, "Dark2"))

  # Return
  return(d)
}


#' @title PlotlyConversion
#'
#' @author Adrian Antico
#' @family Graphics
#'
#' @param p1 ggplot2 object
#'
#' @keywords internal
PlotlyConversion <- function(p1) {
  if(class(p1)[[1L]] != 'plotly') {
    if(!is.null(p1$plot_env$ForecastLineColor) || any(p1$layers[[1]]$mapping %like% 'PredictionColName')) {
      p1 <- plotly::rangeslider(plotly::ggplotly(p1, dynamicTicks = TRUE))
    } else {
      p1 <- plotly::ggplotly(p1)
    }
  }
  return(p1)
}

#' @noRd
BlankPlot <- function() {
  `Y Axis` <- rnorm(20)
  `X Axis` <- rnorm(20,1,0.5)
  df <- data.frame(`X Axis`,`Y Axis`)
  return(
    eval(ggplot2::ggplot(df, ggplot2::aes(`X Axis`,`Y Axis`)) +
           ggplot2::geom_blank() +
           RemixAutoML::ChartTheme(ChartColor = 'aliceblue', GridColor = 'lightsteelblue1', BorderColor = 'lightsteelblue2') +
           ggplot2::labs(title = 'Title', subtitle = 'Subtitle', caption = 'RemixAutoML'))

  )
}

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
#' @keywords internal
AddFacet <- function(p, fv1=NULL, fv2=NULL, Exclude = 'None', Debug = FALSE) {
  if(length(fv1) != 0 && fv1 != Exclude && length(fv2) != 0 && fv2 != Exclude) {
    if(Debug) print('FacetVar1 and FacetVar2')
    p <- p + ggplot2::facet_grid(get(fv1) ~ get(fv2))
  } else if(length(fv1) != 0 && fv1 != Exclude) {
    if(Debug) print('FacetVar1')
    p <- p + ggplot2::facet_wrap(~ get(fv1))
  } else if(length(fv2) != 0 && fv2 != Exclude) {
    if(Debug) print('FacetVar2')
    p <- p + ggplot2::facet_wrap(~ get(fv2))
  }
  return(eval(p))
}
