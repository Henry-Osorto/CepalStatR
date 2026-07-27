test_that("call.data returns a tabular object for a valid indicator", {
  skip_on_cran()

  out <- tryCatch(
    call.data(
      id.indicator = "4788",
      language.en = TRUE,
      notes = TRUE,
      progress = FALSE
    ),
    error = function(e) {
      skip(paste("CEPALSTAT API was not available:", conditionMessage(e)))
    }
  )

  expect_s3_class(out, "data.frame")
  expect_gt(nrow(out), 0)
})
