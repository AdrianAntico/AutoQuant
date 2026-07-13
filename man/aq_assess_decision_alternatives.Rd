\name{aq_assess_decision_alternatives}
\alias{aq_assess_decision_alternatives}
\title{Assess Decision Alternatives}
\usage{
aq_assess_decision_alternatives(decision_context)
}
\arguments{
\item{decision_context}{An \code{aq_decision_context} object.}
}
\value{
A data.table of alternative-level assessment evidence.
}
\description{
Produces structured tradeoff evidence for alternatives. It does not force all
dimensions into one opaque score.
}
