# polygon My Home: https://polygon.io/dashboard/api-keys/45dfcdb6-8e9f-4fdf-8a69-b995d49c99d7

# API Key
APIKey <- 'hvyL7ZOsKK_5PNplOmv55tBTRd8rdA20'


RemixAutoML::StockPlot(Type = 'ohlc', Symbol = 'TSLA', StartDate = '2022-01-01', EndDate = Sys.Date(), APIKey = APIKey)


x <- RemixAutoML:::StockSymbols()

x$symbol


sort(unique(x$sector))



RemixAutoML:::StockPlot(
  Type = 'candlestick',
  Metric = 'Stock Price',
  TimeAgg = 'days',
  Symbol = 'TSLA',
  StartDate = '2021-05-24',
  EndDate = '2022-05-24',
  APIKey = APIKey
  )


Type = 'candlestick'
Metric = 'Stock Price'
TimeAgg = 'days'
Symbol = 'TSLA'
StartDate = '2021-05-24'
EndDate = '2022-05-24'
APIKey = APIKey








y <- jsonlite::fromJSON("https://api.polygon.io/v1/marketstatus/upcoming?apiKey=hvyL7ZOsKK_5PNplOmv55tBTRd8rdA20")

y










