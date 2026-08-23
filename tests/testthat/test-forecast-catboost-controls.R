testthat::test_that("forecast CatBoost retains expert engine controls", {
  out <- qa_forecast_catboost_controls()
  testthat::expect_true(isTRUE(attr(out, "passed")))
  testthat::expect_true(all(out$passed))
})

testthat::test_that("advanced forecast controls execute in native CatBoost", {
  out <- qa_forecast_catboost_engine_execution()
  if (identical(attr(out, "status"), "dependency_missing")) {
    testthat::skip("catboost is unavailable in this qualification runtime.")
  }
  testthat::expect_true(isTRUE(attr(out, "passed")))
  testthat::expect_true(all(out$passed))
})
