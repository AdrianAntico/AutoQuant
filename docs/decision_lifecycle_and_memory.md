# Decision Lifecycle and Organizational Memory

Semantic Intelligence Phase 4 extends decision management from decision framing
into durable decision memory.

This gives AutoQuant an organizational memory layer for decisions: expected
outcomes, realized outcomes, learning, and future reuse guidance are preserved
as evidence rather than disappearing into one-time reports.

The framework remains deterministic and human-governed. It does not optimize,
execute, or autonomously approve decisions. It records how recommendations,
approvals, outcomes, and learning evolve so future analysis can reuse evidence
without pretending past recommendations were final truth.

## Lifecycle

The canonical lifecycle is:

1. `aq_decision_context()` records the decision question, alternatives,
   authority, coverage, criteria, expected financial impact, uncertainty,
   optionality, recommendation, selected decision, and outcome-review intent.
2. `aq_review_decision()` attaches an observed outcome to a selected decision.
3. `aq_decision_timeline()` reconstructs the lifecycle in deterministic order.
4. `aq_decision_learning_summary()` summarizes whether outcome evidence
   validated, partially supported, or contradicted the decision pattern.
5. `aq_decision_memory_artifact()` emits a canonical artifact that can be used
   by collectors, reports, bounded GenAI context, and future knowledge
   promotion.

## Review Contract

`aq_review_decision()` records:

- decision context id
- selected decision id
- selected alternative id
- expected outcome
- expected net benefit
- realized outcome
- actual value
- variance
- execution state
- review status
- assumption status
- lessons learned
- strategy implications
- lever implications
- assumption updates
- future recommendations
- review date

The review status is deterministic:

- `outcome_missing`: no actual value was supplied.
- `validated`: actual value meets or exceeds expected net benefit and
  assumptions did not fail.
- `partial`: actual value is positive but below expected net benefit.
- `assumption_failed`: the review explicitly marks assumptions as failed.
- `negative_evidence`: actual value is negative or otherwise contradicts the
  expected value story.

## Organizational Memory Artifact

`aq_decision_memory_artifact()` creates a canonical table artifact over the
decision timeline. Its metadata includes the raw reviews, timeline, learning
summary, supported actions, and lineage back to the decision context and
business intent.

Supported downstream actions include:

- decision review
- outcome review
- campaign review
- report
- GenAI bounded context
- knowledge promotion

## Relationship to Campaigns

Forecasting and analytical campaigns can consume decision memory as evidence.
Validated decisions become reusable patterns with applicability limits. Partial
learning can guide additional evidence collection. Negative evidence should
trigger strategy, tactic, lever, or assumption review before the pattern is
reused.

Campaigns should not treat decision memory as a permanent rule. It is evidence
with context, authority, coverage, and review history.

## Relationship to GenAI

Decision memory is designed for bounded context. A GenAI layer should receive
the decision question, selected alternative, expected versus actual outcome,
variance, review status, lessons learned, recommendations, and artifact
references rather than raw operational data by default.

## Limitations

Phase 4 does not implement:

- optimization
- causal estimators
- autonomous decisions
- reinforcement learning
- portfolio optimization
- execution engines
- enterprise workflow integration

Those may use decision memory later, but the Phase 4 contract is evidence
recording and deterministic replay.
