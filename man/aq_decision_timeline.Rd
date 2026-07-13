\name{aq_decision_timeline}
\alias{aq_decision_timeline}
\title{Build a Decision Timeline}
\usage{
aq_decision_timeline(decision_context, reviews = NULL)
}
\arguments{
\item{decision_context}{An \code{aq_decision_context} object.}
\item{reviews}{Optional decision review rows.}
}
\value{A data.table timeline.}
\description{Reconstructs decision context, alternatives, recommendations, decisions, outcomes, and learning events.}
