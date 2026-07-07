# AutoQuant Typed Artifact Schema Contract

## Purpose

AutoQuant generators should return reusable analytical artifacts, not report-specific fragments.

The artifact schema framework defines a lightweight, typed object contract that can be consumed by:

- static reports
- dashboards
- Shiny applications
- APIs
- automated pipelines
- LLM agents
- future AutoQuant modules

This contract does not change existing analytical computations. It defines the target shape for future generator outputs and adapters.

## Core Principle

Every artifact should answer:

- What is this?
- Where did it come from?
- What does it depend on?
- How should consumers interpret it?
- Can another renderer or workflow reuse it without recomputing analysis?

## Artifact Types

The first supported artifact types are:

- `table`
- `plot`
- `diagnostic`
- `finding`
- `warning`
- `metadata`
- `computation_graph`
- `display_plan`
- `quality_gate`

These types are intentionally broad. Future generators should add type-specific metadata or roles before creating new top-level artifact types.

## Common Metadata

Every artifact must contain:

- `id`
- `type`
- `title`
- `subtitle`
- `description`
- `tags`
- `dependencies`
- `source_generator`
- `creation_time`
- `version`

Rules:

- `id` must be stable and unique inside an artifact collection.
- `type` must be one of the supported artifact types.
- `title` must be human-readable.
- `tags` must be character values usable by reports, Shiny apps, APIs, and agents.
- `dependencies` must reference other artifact IDs when possible.
- `source_generator` should identify the generator or adapter that created the artifact.
- `version` is the artifact schema version, not the package version.

## Type-Specific Payloads

### Table Artifact

Required fields:

- common metadata
- `data`
- `schema`

Use for authoritative computed tables. Plots, findings, and text should derive from table artifacts where practical.

### Plot Artifact

Required fields:

- common metadata
- `object`
- `config`

Use for AutoPlots/htmlwidget/plot objects. Plot artifacts should reference their source table artifact through `dependencies`.

### Diagnostic Artifact

Required fields:

- common metadata
- `status`
- `severity`
- `details`

Use for checks, diagnostics, and validation results that are not necessarily user-facing warnings.

### Finding Artifact

Required fields:

- common metadata
- `claim`
- `evidence_ids`
- `confidence`
- `severity`
- `caveats`
- `recommended_follow_up`

Use for compressed analytical interpretation. Findings must remain traceable to evidence artifacts.

### Warning Artifact

Required fields:

- common metadata
- `message`
- `severity`
- `affected_artifacts`

Use for user-visible caveats or risks.

### Metadata Artifact

Required fields:

- common metadata
- `values`

Use for model context, run context, data context, problem type, source package, source function, and other reusable metadata.

### Computation Graph Artifact

Required fields:

- common metadata
- `nodes`
- `edges`
- `cache_keys`

Use to preserve dependency and cache structure. Reports must consume graph outputs; they must not recompute analytical quantities.

### Display Plan Artifact

Required fields:

- common metadata
- `sections`
- `artifact_ids`
- `audience`
- `layout_type`

Use to organize artifacts for reports, dashboards, or Shiny pages. Display plans reference artifacts by ID.

### Quality Gate Artifact

Required fields:

- common metadata
- `gate_type`
- `status`
- `severity`
- `message`
- `diagnostic_value`
- `threshold`
- `recommended_action`

Use for enforceable trust checks.

## Validation

The framework provides:

- `validate_artifact()`
- `validate_artifact_collection()`

Validation must check:

- S3 class inheritance
- required common fields
- supported type
- required type-specific fields
- scalar identity fields
- tag and dependency shape
- collection-level unique IDs
- dependency references

Validation should return structured tables so QA, reports, and apps can display failures without parsing errors.

## Constructor Rules

Constructors should be boring and predictable:

- `new_table_artifact()`
- `new_plot_artifact()`
- `new_diagnostic_artifact()`
- `new_finding_artifact()`
- `new_warning_artifact()`
- `new_metadata_artifact()`
- `new_computation_graph_artifact()`
- `new_display_plan_artifact()`
- `new_quality_gate_artifact()`

They should:

- create typed S3/list objects
- preserve common metadata
- avoid analytical computation
- avoid renderer-specific assumptions
- accept optional metadata for future extension

## Consumer Contract

Reports:

- render artifacts
- do not recompute artifacts
- use display plans for organization

Dashboards and Shiny apps:

- read artifact IDs, types, tags, and dependencies
- preview tables/plots/findings/warnings consistently
- allow filtering by artifact type and tag

APIs:

- serialize artifact metadata and payloads where safe
- expose validation status
- preserve dependency graph

LLM agents:

- consume findings, warnings, diagnostics, and quality gates before raw plots
- cite evidence IDs
- preserve caveats and counter-evidence

## Migration Strategy

Existing generators do not need to be rewritten immediately.

Recommended phases:

1. Add typed artifact constructors and validators.
2. Add adapters that wrap existing generator outputs into typed artifacts.
3. Add QA that validates typed collections.
4. Migrate modern generators one at a time.
5. Update reports and apps to prefer typed collections.

## QA Contract

The package should expose `qa_artifact_schema_framework()`.

It must verify:

- every required artifact type can be constructed
- every constructor returns `aq_artifact`
- individual artifacts validate
- collections validate
- dependencies resolve
- finding evidence IDs are preserved
- display plans reference existing artifacts

## Non-Goals

This framework does not:

- change analytical computations
- force existing generators to migrate immediately
- define every future analytical schema
- replace report API contracts
- replace model behavior or prediction surface contracts

It defines the reusable object layer those systems should share.
