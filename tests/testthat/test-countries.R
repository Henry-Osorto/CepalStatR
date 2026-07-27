test_that("countries returns a non-empty data frame", {
  skip_on_cran()

  out <- tryCatch(
    countries(language.en = TRUE),
    error = function(e) {
      skip(paste("CEPALSTAT API was not available:", conditionMessage(e)))
    }
  )

  expect_s3_class(out, "data.frame")
  expect_gt(nrow(out), 0)
})
