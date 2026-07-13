\name{aq_discover_forecasting_capabilities}
\alias{aq_discover_forecasting_capabilities}
\title{Discover vNext Forecasting Capabilities}
\usage{
aq_discover_forecasting_capabilities(
  data,
  target = NULL,
  date = NULL,
  entity = NULL,
  hierarchy = NULL,
  stages = NULL,
  stage = NULL,
  value = NULL,
  known_future_variables = character(),
  horizon = 1L,
  forecast_origin = NULL,
  dataset_id = NULL
)
}
\arguments{
\item{data}{Input data.}
\item{target}{Optional target column or columns.}
\item{date}{Optional date column.}
\item{entity}{Optional entity/group column for panel forecasting.}
\item{hierarchy}{Optional hierarchy column names or hierarchy metadata.}
\item{stages}{Optional ordered funnel stage names.}
\item{stage}{Optional funnel stage column.}
\item{value}{Optional funnel value column.}
\item{known_future_variables}{Optional variables known at forecast time.}
\item{horizon}{Forecast horizon used for planning diagnostics.}
\item{forecast_origin}{Optional forecast origin.}
\item{dataset_id}{Optional dataset id.}
}
\value{
An \code{aq_forecasting_capability_discovery} object.
}
\description{
Inspects a forecasting problem and records which forecasting families are
supported, unsupported, or uncertain. Discovery is deterministic and does not
fit models.
}
\examples{
\dontrun{
discovery <- aq_discover_forecasting_capabilities(
  data = history,
  target = c("leads", "applications"),
  date = "date",
  horizon = 3
)
}
}
