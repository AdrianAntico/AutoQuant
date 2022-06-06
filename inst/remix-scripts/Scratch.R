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



x <- data.table::fread(file.choose())
y<- data.table::data.table(A = 1:10)
temp <- function(x, y, h = 'a') {
  if(h == 'a') {
    print(x)
    if(!missing(y)) print(y)
  }
}
temp(x)

gg <- call('temp', x)
gg

data <- data.table::fread('C:/Users/Bizon/Documents/GitHub/BenchmarkData1.csv')


data <- data.table::data.table(A = 1:10, B = 1:10)
sdcols <- c('A','B')
lsdcols <- length(sdcols)
data[, list(sum(.SD), sum(.SD) / lsdcols), .SDcols = sdcols, by = .I]
data[, list(sum(.SD)^2, mean(.SD)), .SDcols = sdcols, by = .I]
