\name{aq_decision_context}
\alias{aq_decision_context}
\title{Create a Canonical Decision Context}
\usage{
aq_decision_context(
  context,
  alternatives = NULL,
  criteria = NULL,
  financial_impacts = NULL,
  uncertainties = NULL,
  optionality = NULL,
  recommendations = NULL,
  decisions = NULL,
  outcomes = NULL,
  business_intent = NULL,
  variable_semantics = NULL,
  decision_context_id = NULL,
  supported_downstream_actions = aq_decision_supported_actions()
)
}
\arguments{
\item{context}{Decision context record.}
\item{alternatives}{Alternative records.}
\item{criteria}{Evaluation criteria records.}
\item{financial_impacts}{Financial impact records.}
\item{uncertainties}{Uncertainty records.}
\item{optionality}{Optionality records.}
\item{recommendations}{Recommendation records.}
\item{decisions}{Decision records.}
\item{outcomes}{Outcome-review records.}
\item{business_intent}{Optional \code{aq_business_intent} object.}
\item{variable_semantics}{Optional \code{aq_variable_semantics} object.}
\item{decision_context_id}{Optional context id.}
\item{supported_downstream_actions}{Supported downstream actions.}
}
\value{
An \code{aq_decision_context} object.
}
\description{
Creates a typed decision-management object containing a decision question,
alternatives, criteria, financial impact, uncertainty, optionality, authority,
coverage, recommendations, decisions, and outcome follow-up.
}
