# AutoQuant is a package for quickly creating high quality visualizations under a common and easy api.
# Copyright (C) <year>  <name of author>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

#' @title CausalMediation
#'
#' @description CausalMediation utilizes models from regmedint package
#'
#' @author Adrian Antico
#' @family Statistical Modeling
#'
#' @param data Data frame containing the following relevant variables.
#' @param OutcomeTargetVariable yvar in underlying model. A character vector of length 1. Outcome variable name. It should be the time variable for the survival outcome.
#' @param TreatmentVariable avar in underlying model. A character vector of length 1. Treatment variable name.
#' @param MediatorVariable mvar in underlying model. A character vector of length 1. Mediator variable name.
#' @param Covariates For main model
#' @param ConfoundingVariables cvar in underlying model. A character vector of length > 0. Covariate names. Use \code{NULL} if there is no covariate. However, this is a highly suspicious situation. Even if \code{avar} is randomized, \code{mvar} is not. Thus, there are usually some confounder(s) to account for the common cause structure (confounding) between \code{mvar} and \code{yvar}.
#' @param MM_TreatmentCovariates emm_ac_mreg in underlying model. A character vector of length > 0. Effect modifiers names. The covariate vector in treatment-covariate product term in the mediator model.
#' @param OM_TreatmentCovariates emm_ac_yreg in underlying model. A character vector of length > 0. Effect modifiers names. The covariate vector in treatment-covariate product term in the outcome model.
#' @param OM_MediatorCovariates emm_mc_yreg in underlying model. A character vector of length > 0. Effect modifiers names. The covariate vector in mediator-covariate product term in outcome model.
#' @param SurvivalEventVariable eventvar in underlying model. An character vector of length 1. Only required for survival outcome regression models. Note that the coding is 1 for event and 0 for censoring, following the R survival package convention.
#' @param UnTreated_ReferenceIndicator a0 in underlying model. A numeric vector of length 1. The reference level of treatment variable that is considered "untreated" or "unexposed".
#' @param Treated_ReferenceIndicator a1 in underlying model. A numeric vector of length 1.
#' @param Mediator_ControlDirectEffectLevel m_cde in underlying model. A numeric vector of length 1. Mediator level at which controlled direct effect is evaluated at.
#' @param Covariate_NaturalDirectIndirect c_cond in underlying model. A numeric vector of the same length as \code{cvar}. Covariate levels at which natural direct and indirect effects are evaluated at.
#' @param MediatorTargetType mreg in underlying model. A character vector of length 1. Mediator regression type: \code{"linear"} or \code{"logistic"}.
#' @param OutcomeTargetType yreg in underlying model. A character vector of length 1. Outcome regression type: \code{"linear"}, \code{"logistic"}, \code{"loglinear"}, \code{"poisson"}, \code{"negbin"}, \code{"survCox"}, \code{"survAFT_exp"}, or \code{"survAFT_weibull"}.
#' @param TreatmentMediatorInteraction interaction in underlying model. A logical vector of length 1. The presence of treatment-mediator interaction in the outcome model. Default to TRUE.
#' @param CaseControlSourceData casecontrol in underlying model. A logical vector of length 1. Default to FALSE. Whether data comes from a case-control study.
#' @param RemoveNA na_omit in underlying model. A logical vector of length 1. Default to FALSE. Whether to remove NAs in the columns of interest before fitting the models.
#'
#' @examples
#' \dontrun{
#' library(regmedint) # to load vv2015
#' data(vv2015)
#' Output <- AutoQuant::CausalMediation(
#'   data = vv2015,
#'   OutcomeTargetVariable = 'y',            # yvar char length = 0
#'   TreatmentVariable = "x",                # avar char length = 0 (binary)
#'   MediatorVariable = "m",                 # mvar char length = 0 (binary)
#'   Covariates = "c",                       # cvar char length > 0
#'   MM_TreatmentCovariates = NULL,          # emm_ac_mreg = NULL char length > 0
#'   OM_TreatmentCovariates = NULL,          # emm_ac_yreg = NULL char length > 0
#'   OM_MediatorCovariates = NULL,           # emm_mc_yreg = NULL char length > 0
#'   SurvivalEventVariable = "event",        # eventvar char length = 0
#'   UnTreated_ReferenceIndicator = 0,       # ao num length = 1
#'   Treated_ReferenceIndicator = 1,         # a1 num length = 1
#'   Mediator_ControlDirectEffectLevel = 1,  # m_cde num length = 1
#'   Covariate_NaturalDirectIndirect = 3,    # c_cond; same length as Covariates num length = length(Covariates)
#'   MediatorTargetType = 'logistic',        # mreg "linear" or "logistic",
#'   OutcomeTargetType = 'survAFT_weibull',  # yreg "linear", "logistic", "loglinear", "poisson", "negbin", "survCox", "survAFT_exp", or "survAFT_weibull"
#'   TreatmentMediatorInteraction = TRUE,    # interaction = TRUE,
#'   CaseControlSourceData = FALSE,          # casecontrol = FALSE,
#'   RemoveNA = FALSE)
#'
#' # data = vv2015
#' # OutcomeTargetVariable = 'y'
#' # TreatmentVariable = "x"
#' # MediatorVariable = "m"
#' # Covariates = "c"
#' # MM_TreatmentCovariates = NULL
#' # OM_TreatmentCovariates = NULL
#' # OM_MediatorCovariates = NULL
#' # SurvivalEventVariable = "event"
#' # UnTreated_ReferenceIndicator = 0
#' # Treated_ReferenceIndicator = 1
#' # Mediator_ControlDirectEffectLevel = 1
#' # Covariate_NaturalDirectIndirect = 3
#' # MediatorTargetType = 'logistic'
#' # OutcomeTargetType = 'survAFT_weibull'
#' # TreatmentMediatorInteraction = TRUE
#' # CaseControlSourceData = FALSE
#' # RemoveNA = FALSE
#' }
#'
#' @return list with model output object, summary output, effects output, and an effects plot
#'
#' @export
CausalMediation <- function(data,
                            OutcomeTargetVariable = NULL,              # yvar char length = 0
                            TreatmentVariable = NULL,                  # avar char length = 0 (binary)
                            MediatorVariable = NULL,                   # mvar char length = 0 (binary)
                            Covariates = NULL,                         # cvar char length > 0
                            MM_TreatmentCovariates = NULL,             # emm_ac_mreg = NULL char length > 0
                            OM_TreatmentCovariates = NULL,             # emm_ac_yreg = NULL char length > 0
                            OM_MediatorCovariates = NULL,              # emm_mc_yreg = NULL char length > 0
                            SurvivalEventVariable = NULL,              # eventvar char length = 0
                            UnTreated_ReferenceIndicator = NULL,       # ao num length = 1
                            Treated_ReferenceIndicator = NULL,         # a1 num length = 1
                            Mediator_ControlDirectEffectLevel = NULL,  # m_cde num length = 1
                            Covariate_NaturalDirectIndirect = 0,       # c_cond; same length as Covariates num length = length(Covariates)
                            MediatorTargetType = 'linear',             # mreg "linear" or "logistic"
                            OutcomeTargetType = 'linear',              # yreg "linear", "logistic", "loglinear", "poisson", "negbin", "survCox", "survAFT_exp", or "survAFT_weibull"
                            TreatmentMediatorInteraction = TRUE,       # interaction = TRUE,
                            CaseControlSourceData = FALSE,             # casecontrol = FALSE,
                            RemoveNA = FALSE) {                        # na_omit = FALSE

  print("Causaul Mediation 1")

  print(data)
  print(OutcomeTargetVariable)
  print(TreatmentVariable)
  print(MediatorVariable)
  print(Covariates)
  print(MM_TreatmentCovariates)
  print(OM_TreatmentCovariates)
  print(OM_MediatorCovariates)
  print(SurvivalEventVariable)
  print(UnTreated_ReferenceIndicator)
  print(Treated_ReferenceIndicator)
  print(Mediator_ControlDirectEffectLevel)
  print(Covariate_NaturalDirectIndirect)
  print(MediatorTargetType)
  print(OutcomeTargetType)
  print(TreatmentMediatorInteraction)
  print(CaseControlSourceData)
  print(RemoveNA)

  # Model
  Output <- regmedint::regmedint(
    data = data,
    yvar = OutcomeTargetVariable,
    avar = TreatmentVariable,
    mvar = MediatorVariable,
    cvar = Covariates,
    emm_ac_mreg = MM_TreatmentCovariates,
    emm_ac_yreg = OM_TreatmentCovariates,
    emm_mc_yreg = OM_MediatorCovariates,
    eventvar = SurvivalEventVariable,
    a0 = UnTreated_ReferenceIndicator,
    a1 = Treated_ReferenceIndicator,
    m_cde = Mediator_ControlDirectEffectLevel,
    c_cond = Covariate_NaturalDirectIndirect,
    mreg = MediatorTargetType,
    yreg = OutcomeTargetType,
    interaction = TreatmentMediatorInteraction,
    casecontrol = CaseControlSourceData,
    na_omit = RemoveNA)

  print("Causaul Mediation 2")

  # Summary Output
  SummaryOutput <- summary(Output)

  print("Causaul Mediation 3")

  Effects <- data.table::as.data.table(SummaryOutput$summary_myreg, keep.rownames = TRUE)

  print("Causaul Mediation 4")

  data.table::setnames(Effects, c('est','rn','se','Z','p'), c('Estimate','Measure','Standard Error','Z-Value','P-Value'))

  print("Causaul Mediation 5")

  Effects[, Description := 'a']
  Effects[, Definition := 'a']

  data.table::set(Effects, i = 1L, j = 'Description', value = 'Conditional Direct Effect')
  data.table::set(Effects, i = 1L, j = 'Definition', value = 'E[Y_treat,mediator | C = c] − E[Y_notreat,mediator | C = c]')

  data.table::set(Effects, i = 2L, j = 'Description', value = 'Pure Natural Direct Effect')
  data.table::set(Effects, i = 2L, j = 'Definition', value = 'E[Y_treat,no_mediation | C = c] − E[Y_no_treat,no_mediation | C = c]')

  data.table::set(Effects, i = 3L, j = 'Description', value = 'Total Natural Indirect Effect')
  data.table::set(Effects, i = 3L, j = 'Definition', value = 'E[Y_treat, mediation |C = c] − E[Y_treat,no_mediation | C = c]')

  data.table::set(Effects, i = 4L, j = 'Description', value = 'Total Natural Direct Effect')
  data.table::set(Effects, i = 4L, j = 'Definition', value = 'E[Y_treat,mediation | C = c] − E[Y_no_treat, mediation | C = c]')

  data.table::set(Effects, i = 5L, j = 'Description', value = 'Pure Natural Indirect Effect')
  data.table::set(Effects, i = 5L, j = 'Definition', value = 'E[Y_no_treat,mediation | C = c] − E[Y_no_treat,no_mediation | C = c]')

  data.table::set(Effects, i = 6L, j = 'Description', value = 'Total Effect')
  data.table::set(Effects, i = 6L, j = 'Definition', value = 'E[Y_treat | C = c] − E[Y_no_treat | C = c]')

  if(MediatorTargetType == 'linear') {
    data.table::set(Effects, i = 7L, j = 'Description', value = 'Proportion Mediated')
    data.table::set(Effects, i = 7L, j = 'Definition', value = 'TNIE / (TNIE + PNDE)')
  } else {
    data.table::set(Effects, i = 7L, j = 'Description', value = 'Proportion Mediated')
    data.table::set(Effects, i = 7L, j = 'Definition', value = 'exp(PNDE) * (exp(TNIE) - 1) / (exp(PNDE) * exp(TNIE) - 1)')
  }

  # Significance stars
  Effects[, Significance := data.table::fcase(
    `P-Value` < 0.001, '***',
    `P-Value` < 0.01, '**',
    `P-Value` < 0.05, '*',
    default = ''
  )]

  data.table::setcolorder(Effects, c(8L,1L:7L,9L))
  p1 <- AutoQuant::BarPlot(data = Effects, XVar = 'Description', YVar = 'Estimate',  AggMethod = 'sum', ColorVar = 'Significance', FillColor = '#66ff10')

  MainEffects <- data.table::as.data.table(x = SummaryOutput$summary_yreg_fit$table, keep.rownames = TRUE)
  data.table::setnames(x = MainEffects, old = c('rn','z','p'), c('Variable', 'Z-Value', 'P-Value'))
  MainEffects[, Significance := data.table::fcase(
    `P-Value` < 0.001, '***',
    `P-Value` < 0.01, '**',
    `P-Value` < 0.05, '*',
    default = ''
  )]

  # Return
  return(list(
    Output = Output,
    SummaryOutput = SummaryOutput,
    MainEffects = MainEffects,
    Effects = Effects,
    EffectsPlot = p1
  ))
}
