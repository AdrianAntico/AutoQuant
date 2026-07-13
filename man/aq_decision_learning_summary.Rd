\name{aq_decision_learning_summary}
\alias{aq_decision_learning_summary}
\title{Summarize Decision Learning}
\usage{
aq_decision_learning_summary(decision_context, reviews)
}
\arguments{
\item{decision_context}{An \code{aq_decision_context} object.}
\item{reviews}{Decision review rows.}
}
\value{A data.table learning summary.}
\description{Summarizes validated, partial, failed-assumption, and negative evidence from decision reviews.}
