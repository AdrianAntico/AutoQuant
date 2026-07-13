library(AutoQuant)

decision <- AutoQuant:::aq_decision_context_fixture()

review <- aq_review_decision(
  decision,
  decision_id = "decision_budget_pending",
  realized_outcome = "Validated paid-search response inside delegated range.",
  actual_value = 36000,
  lessons_learned = "Validated range remains a usable decision boundary.",
  strategy_implications = "Continue scaling qualified-demand strategy inside validated range.",
  lever_implications = "Paid-search budget can be adjusted within current guardrails.",
  assumption_updates = "Response curve held for the next-quarter campaign window.",
  future_recommendations = "Monitor saturation before testing outside range.",
  assumption_status = "held"
)

timeline <- aq_decision_timeline(decision, review)
learning <- aq_decision_learning_summary(decision, review)
memory_artifact <- aq_decision_memory_artifact(decision, review)

stopifnot(nrow(review) == 1)
stopifnot(any(timeline$event_type == "learning"))
stopifnot(learning$validated_decisions[[1]] == 1)
stopifnot(inherits(memory_artifact, "aq_artifact"))
