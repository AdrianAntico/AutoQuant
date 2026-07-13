\name{aq_validate_decision_context}
\alias{aq_validate_decision_context}
\title{Validate a Decision Context}
\usage{
aq_validate_decision_context(decision_context)
}
\arguments{
\item{decision_context}{An \code{aq_decision_context} object.}
}
\value{
A data.table of deterministic validation diagnostics.
}
\description{
Validates the structure, baseline alternative, supported alternative types, and
core references in a decision context.
}
