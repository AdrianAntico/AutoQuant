\name{aq_review_decision}
\alias{aq_review_decision}
\title{Review a Decision Outcome}
\usage{
aq_review_decision(
  decision_context,
  decision_id = NULL,
  realized_outcome = NA_character_,
  actual_value = NA_real_,
  actual_execution_state = "completed",
  lessons_learned = NA_character_,
  strategy_implications = NA_character_,
  lever_implications = NA_character_,
  assumption_updates = NA_character_,
  future_recommendations = NA_character_,
  review_id = NULL,
  review_date = Sys.Date(),
  assumption_status = NA_character_
)
}
\arguments{
\item{decision_context}{An \code{aq_decision_context} object.}
\item{decision_id}{Optional decision id to review.}
\item{realized_outcome}{Observed outcome text.}
\item{actual_value}{Observed numeric value.}
\item{actual_execution_state}{Observed execution state.}
\item{lessons_learned}{Lessons learned text.}
\item{strategy_implications}{Strategy implications.}
\item{lever_implications}{Lever implications.}
\item{assumption_updates}{Assumption updates.}
\item{future_recommendations}{Future recommendations.}
\item{review_id}{Optional review id.}
\item{review_date}{Review date.}
\item{assumption_status}{Assumption status.}
}
\value{A data.table decision review row.}
\description{Attaches observed outcome evidence to a governed decision context.}
