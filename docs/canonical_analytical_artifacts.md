# Canonical Analytical Artifacts

Status: Phase 7 implemented for AutoQuant vNext supervised CatBoost regression and binary classification paths.

AutoQuant vNext treats analytical outputs as evidence-bearing artifacts rather than disconnected return objects. Models remain important, but they are not the primary language exchanged with downstream consumers. The shared language is the canonical analytical artifact envelope.

## Philosophy

Every analytical artifact should answer:

- what was done
- why it was done
- how it was produced
- what evidence was produced
- what assumptions or warnings matter
- what can happen next

The goal is consistency without forced uniformity. A model fit, scoring run, monitoring summary, forecast, MMM estimate, and causal effect estimate will not have identical payloads. They should, however, expose identity, lineage, supported actions, and consumer expectations in the same way.

## Canonical Envelope

The implemented envelope version is:

```text
aq_artifact_envelope_v1
```

Implemented fields:

- `artifact_id`
- `artifact_type`
- `artifact_version`
- `envelope_version`
- `parent_artifact_ids`
- `lineage`
- `task`
- `operator`
- `engine`
- `specification_id`
- `dataset_id`
- `prepared_dataset_id`
- `transformation_id`
- `model_id`
- `campaign_references`
- `warnings`
- `supported_actions`
- `producer`
- `consumer_expectations`
- `created_at`

Use:

```r
envelope <- aq_artifact_envelope(artifact)
relationships <- aq_artifact_relationships(artifact)
actions <- aq_supported_actions(artifact)
diagnostics <- aq_validate_artifact(artifact)
```

Older AutoQuant table/plot artifacts can be normalized into this shape through inference. vNext artifacts carry explicit envelopes.

## Implemented Artifact Family

Phase 7 covers:

- supervised fit artifacts
- prediction artifacts
- scoring artifacts
- outcome attachment artifacts
- realized assessment artifacts
- scoring monitoring artifacts
- portable model bundles

The current relationship pattern is:

```text
model specification
-> fit
-> prediction
-> assessment

fit
-> scoring
-> outcome attachment
-> realized assessment
-> monitoring

fit
-> model bundle
```

Relationships are represented as deterministic parent artifact ids. AutoQuant does not implement a graph database.

## Supported Actions

Artifacts expose downstream actions such as:

- `predict`
- `score`
- `attach_outcomes`
- `assess`
- `assess_realized`
- `monitor`
- `compare`
- `report`
- `campaign_review`
- `knowledge_promotion`
- `validate_bundle`

These actions are metadata, not automatic execution. They allow AnalyticsShinyApp, campaigns, and future operators to reason about what an artifact can support without inspecting engine-specific classes.

## Consumer Expectations

The default vNext consumer expectations record that artifacts are intended for:

- AnalyticsShinyApp
- campaign systems
- future operators

They also state that stable identity, lineage, and the envelope contract are required. Missing optional payloads should become diagnostics rather than hard failures.

## Extension Principles

Future operators should:

- use the canonical envelope
- preserve stable artifact ids
- record parent artifact ids
- keep operator-specific payloads explicit
- expose supported downstream actions
- validate artifacts deterministically
- avoid hiding scoring, assessment, monitoring, or reporting inside fitting

Future operators should not:

- invent a parallel result philosophy
- require a graph database for basic lineage
- use generic schemas where explicit contracts are clearer
- force all artifact payloads into one inheritance hierarchy

## Future Operators

Forecasting, panel forecasting, MMM, and causal modeling should inherit this artifact language.

Examples:

- A forecast fit artifact can use `operator = "forecast_fit"` and expose `forecast`, `assess`, `monitor`, and `report`.
- An MMM artifact can use `operator = "mmm_fit"` and expose `simulate`, `optimize`, `compare`, `report`, and `campaign_review`.
- A causal effect artifact can use `operator = "causal_effect"` and expose `validate_assumptions`, `sensitivity_analysis`, `compare`, `report`, and `knowledge_promotion`.

The payloads will differ. The artifact language should not.

## Validation

`aq_validate_artifact()` verifies:

- required envelope metadata
- envelope version
- lineage shape
- supported action normalization
- consumer expectation availability

It does not validate every task-specific payload. Operator-specific validators should build on top of the canonical artifact validator.
