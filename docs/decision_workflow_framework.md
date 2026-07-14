# Decision Workflow Framework

Decision Workflow Intelligence is the governed lifecycle layer after decision valuation. It links explicit alternatives, valuation evidence, recommendation, authority, approval, implementation, monitoring, realized value, and organizational learning.

## Public APIs

- `aq_decision_workflow()`
- `aq_validate_decision_workflow()`
- `aq_assess_decision_review_readiness()`
- `aq_decision_evidence_package()`
- `aq_decision_review_request()`
- `aq_decision_review()`
- `aq_decision_approval()`
- `aq_validate_decision_approval()`
- `aq_decision_condition()`
- `aq_decision_implementation_plan()`
- `aq_record_decision_implementation()`
- `aq_reconcile_decision_implementation()`
- `aq_decision_monitoring_plan()`
- `aq_assess_decision_quality()`
- `aq_realized_value_review()`
- `aq_decision_followup_candidates()`
- `aq_decision_workflow_staleness()`
- `aq_decision_workflow_campaign_seeds()`
- `aq_decision_workflow_artifact()`
- `qa_decision_workflow_framework()`

## Lifecycle

The supported workflow path is:

```text
valued decision
-> review readiness
-> frozen evidence package
-> human review
-> governed approval
-> implementation plan
-> realized implementation
-> monitoring
-> outcome and realized-value review
-> decision-quality assessment
-> follow-up candidate
-> organizational learning
```

The state model is deterministic. Impossible transitions are rejected by `aq_decision_workflow_transition_allowed()`.

## Review Readiness

`aq_assess_decision_review_readiness()` determines whether a decision package is ready for governed review. It checks structural completeness, selected alternative, valuation linkage, authority, blockers, and deadlines. It returns states such as `ready_for_review`, `valuation_stale`, `authority_incomplete`, `structurally_incomplete`, `blocked`, and `expired`.

## Evidence Package Snapshots

`aq_decision_evidence_package()` creates a canonical `decision_evidence_package_artifact`. It preserves references to the decision context, valuation artifact, evidence references, recommendation, artifact versions, and evidence cutoff so reviewers can later reconstruct what was known.

## Reviews And Approvals

Review requests and review records remain distinct. A review may endorse, endorse with conditions, request more evidence, object, escalate, abstain, or be out of scope. A review is not automatically an approval.

Approvals preserve approver, authority basis, approved alternative, budget, timing, conditions, guardrails, monitoring requirements, expiration, and status. `aq_validate_decision_approval()` validates explicit authority fields such as magnitude and selected alternative. It does not infer permission from a job title string.

## Conditions

`aq_decision_condition()` treats approval and review conditions as durable obligations. Conditions have owners, due dates, evidence, status, breach severity, and resolution. Conditions remain visible after approval.

## Implementation And Monitoring

`aq_decision_implementation_plan()` translates an approved alternative into inspectable operational intent. `aq_record_decision_implementation()` records realized implementation. `aq_reconcile_decision_implementation()` classifies deviations as immaterial, material, outcome-relevant, authority breach, or guardrail breach.

`aq_decision_monitoring_plan()` attaches KPI, guardrail, cost, timing, fidelity, and review monitoring to the decision.

## Outcome, Quality, And Learning

`aq_assess_decision_quality()` assesses the decision process rather than employees. It distinguishes readiness gaps, missing review, missing approval, and implementation deviations.

`aq_realized_value_review()` compares expected and realized value, cost, and timing while preserving maturation status.

`aq_decision_followup_candidates()` proposes bounded follow-up candidates such as retain, expand, modify, contract, escalate, or gather more evidence. It does not create or approve follow-up decisions.

## Staleness

`aq_decision_workflow_staleness()` maps material changes to proportional actions such as reviewer acknowledgement, partial rereview, reapproval, escalation, or no action.

## Artifacts

`aq_decision_workflow_artifact()` creates a canonical `decision_workflow_artifact` containing workflow, readiness, reviews, approvals, implementation plan, reconciliation, monitoring, decision-quality assessment, realized-value review, and follow-up candidates.

## Limitations

This phase intentionally avoids electronic signature infrastructure, identity management, enterprise workflow engines, autonomous approvals, ERP integration, resource allocation optimizers, and automatic execution.
