test_that("ranking.sdg validates arguments without using the API", {
  expect_error(
    ranking.sdg(id.indicator = c(1, 2)),
    "id.indicator must be a single numeric or character value",
    fixed = TRUE
  )
  expect_error(
    ranking.sdg(id.indicator = 3682, height = 0),
    "height must be a positive number",
    fixed = TRUE
  )
  expect_error(
    ranking.sdg(id.indicator = 3682, color = NA_character_),
    "color must be a single character string",
    fixed = TRUE
  )
})

test_that("ranking.sdg returns a ggplot from mocked catalogue and data", {
  skip_if_not_installed("ggplot2")

  testthat::local_mocked_bindings(
    call.indicators = function(...) fixture_indicator_catalogue(),
    call.data = function(...) fixture_sdg_data(),
    .package = "CepalStatR"
  )

  plot_file <- tempfile(fileext = ".pdf")
  grDevices::pdf(plot_file)
  on.exit(grDevices::dev.off(), add = TRUE)

  out <- ranking.sdg(
    id.indicator = "3682",
    language.en = TRUE,
    save = FALSE,
    title = TRUE,
    caption = TRUE,
    progress = FALSE
  )

  expect_true(inherits(out, "ggplot"))
  expect_equal(out$labels$x, "Country")
  expect_equal(out$labels$y, "Indicator achievement")
})
