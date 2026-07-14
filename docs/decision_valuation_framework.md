# Decision Valuation Framework

Decision Valuation Phase 1 adds deterministic contracts for alternative economics, uncertainty translation, action thresholds, information value, optionality, effort, risk, multi-criteria assessment, and canonical valuation artifacts.

## Core APIs

- `aq_decision_valuation_context()`
- `aq_validate_decision_valuation_context()`
- `aq_alternative_cash_flows()`
- `aq_evidence_impact_mapping()`
- `aq_validate_evidence_impact_mapping()`
- `aq_decision_valuation_scenarios()`
- `aq_assess_alternative_economics()`
- `aq_decision_valuation_sensitivity()`
- `aq_decision_action_thresholds()`
- `aq_assess_decision_thresholds()`
- `aq_assess_decision_information_value()`
- `aq_assess_decision_option_value()`
- `aq_assess_decision_effort()`
- `aq_assess_decision_risk()`
- `aq_multi_criteria_assessment()`
- `aq_alternative_dominance()`
- `aq_governed_decision_valuation_recommendation()`
- `aq_decision_valuation_artifact()`
- `aq_decision_valuation_campaign_seeds()`
- `aq_decision_valuation_calibration_record()`
- `qa_decision_valuation_framework()`

## Evidence Status

Every valuation input should retain source status:

- `directly_observed`
- `experimentally_estimated`
- `causally_estimated`
- `predictively_modeled`
- `forecast`
- `scenario_assumption`
- `expert_judgment`
- `imported_financial_input`
- `llm_suggestion`
- `missing`
- `unsupported`

Missing and unsupported inputs are preserved as missing. They produce diagnostics and can influence recommendations, but they are not silently fabricated.

## Economics

`aq_assess_alternative_economics()` computes transparent fields by alternative and scenario:

- gross benefit
- total cost
- net benefit
- ROI
- payback period
- NPV
- incremental net benefit
- incremental NPV
- missing-input status
- source-status summary

Scenarios do not require probabilities. Probabilities may be supplied later, but Phase 1 does not invent them.

## Evidence Translation

`aq_evidence_impact_mapping()` translates causal, predictive, forecast, or assumed effects through scale, duration, and unit economics. Validation checks for missing translation inputs, capacity limits, guardrail blockers, and validated operating range warnings.

## Recommendation

`aq_governed_decision_valuation_recommendation()` combines economics, thresholds, optionality, effort, risk, multi-criteria status, and information value. It can recommend action, staged action, evidence collection, authority escalation, rejection, or retaining the baseline.

It does not approve or execute the decision.

## Calibration

`aq_decision_valuation_calibration_record()` records expected versus realized value after an outcome review. The record is intended for future learning and assumption calibration.

