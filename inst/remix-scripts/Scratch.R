# polygon My Home: https://polygon.io/dashboard/api-keys/45dfcdb6-8e9f-4fdf-8a69-b995d49c99d7

# API Key
key <- 'hvyL7ZOsKK_5PNplOmv55tBTRd8rdA20'


RemixAutoML::StockPlot(Type = 'ohlc', Symbol = 'TSLA', StartDate = '2022-01-01', EndDate = Sys.Date(), APIKey = key)


x <- RemixAutoML:::StockSymbols()

x$symbol


sort(unique(x$sector))






























