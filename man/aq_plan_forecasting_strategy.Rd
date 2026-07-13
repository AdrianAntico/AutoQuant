\name{aq_plan_forecasting_strategy}
\alias{aq_plan_forecasting_strategy}
\title{Plan vNext Forecasting Strategy}
\usage{
aq_plan_forecasting_strategy(
  discovery = NULL,
  data = NULL,
  max_experiments = 5L,
  ...
)
}
\arguments{
\item{discovery}{Optional \code{aq_forecasting_capability_discovery}.}
\item{data}{Optional data used when discovery is not supplied.}
\item{max_experiments}{Maximum number of recommended experiments.}
\item{...}{Arguments passed to \code{aq_discover_forecasting_capabilities()} when
\code{discovery} is not supplied.}
}
\value{
An \code{aq_forecasting_strategy_plan}.
}
\description{
Converts capability discovery into a deterministic analytical plan. The planner
recommends what to test; it does not execute models.
}
\examples{
\dontrun{
plan <- aq_plan_forecasting_strategy(
  data = history,
  target = "sales",
  date = "date",
  horizon = 7
)
}
}
