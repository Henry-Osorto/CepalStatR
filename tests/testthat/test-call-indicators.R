test_that("call.indicators returns a non-empty data frame", {
  skip_on_cran()

  indicators <- call.indicators(language.en = TRUE, progress = FALSE)

  expect_s3_class(indicators, "data.frame")
  expect_gt(nrow(indicators), 0)
  expect_true(any(grepl("Indicator", names(indicators), ignore.case = TRUE)))
  expect_true(any(grepl("ID", names(indicators), ignore.case = TRUE)))
})
