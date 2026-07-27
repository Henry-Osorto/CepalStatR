test_that("ranking.sdg runs for a valid indicator without error", {
  skip_on_cran()

  expect_no_error(
    ranking.sdg(
      id.indicator = 3682,
      language.en = TRUE,
      title = TRUE,
      caption = TRUE,
      progress = FALSE
    )
  )
})
