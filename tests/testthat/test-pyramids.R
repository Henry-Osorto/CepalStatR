test_that("pyramids runs for Honduras without error", {
  skip_on_cran()

  expect_no_error(
    pyramids(
      country = "Honduras",
      years = c(11, 16),
      language.en = TRUE,
      caption = TRUE,
      progress = FALSE
    )
  )
})
