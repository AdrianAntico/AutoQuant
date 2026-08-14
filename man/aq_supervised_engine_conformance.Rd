\name{aq_supervised_engine_conformance}
\alias{aq_supervised_engine_comparison}
\alias{aq_supervised_engine_params}
\alias{aq_supervised_engine_tuning_space}
\alias{aq_supervised_implementation_descriptor}
\alias{aq_supervised_native_explainability}
\alias{qa_supervised_engine_conformance}
\title{Governed LightGBM and XGBoost supervised-engine contracts}
\description{
Constructs engine-specific parameters, descriptors, tuning spaces, native
explainability results, and a comparative capability envelope for conforming
AutoQuant supervised implementations. The QA entry point exercises the shared
reference contract while preserving declared engine-specific differences.
}
\usage{
aq_supervised_engine_params(engine, ...)

aq_supervised_engine_tuning_space(engine, task = c("regression", "binary"))

aq_supervised_implementation_descriptor(engine, task, params = NULL)

aq_supervised_native_explainability(fit, data, interactions = FALSE)

aq_supervised_engine_comparison()

qa_supervised_engine_conformance()
}
\arguments{
  \item{engine}{One of \code{"catboost"}, \code{"lightgbm"}, or \code{"xgboost"}, as supported by the called function.}
  \item{...}{Named engine-specific parameter overrides.}
  \item{task}{The supervised task, \code{"regression"} or \code{"binary"}.}
  \item{params}{A governed engine-parameter list.}
  \item{fit}{A fitted \code{aq_fit_result}.}
  \item{data}{Compatible scoring data used for on-demand native evidence.}
  \item{interactions}{Whether native interaction contributions are requested.}
}
\value{
Parameter and descriptor functions return named lists; tuning and comparison
functions return machine-readable tables or lists; explainability returns an
\code{aq_evidence_result}; and QA returns a data table of normalized checks.
}
