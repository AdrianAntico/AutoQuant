\name{aq_decision_memory_artifact}
\alias{aq_decision_memory_artifact}
\title{Create a Decision Memory Artifact}
\usage{
aq_decision_memory_artifact(decision_context, reviews = NULL, artifact_id = NULL)
}
\arguments{
\item{decision_context}{An \code{aq_decision_context} object.}
\item{reviews}{Decision review rows.}
\item{artifact_id}{Optional artifact id.}
}
\value{A canonical decision-memory artifact.}
\description{Packages a decision lifecycle timeline and learning summary as a canonical AutoQuant artifact.}
