testthat::test_that("compact forecasting API reaches native implementations", {
  out <- qa_forecast_public_api()
  testthat::expect_true(isTRUE(attr(out, "passed")))
  testthat::expect_true(all(out$passed))
})
