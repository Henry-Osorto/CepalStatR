test_that("pyramids validates arguments without using the API", {
  expect_error(
    pyramids(country = NA_character_),
    "country must be a non-missing character vector",
    fixed = TRUE
  )
  expect_error(
    pyramids(country = "Honduras", years = 0),
    "years must be numeric values between 1 and 31",
    fixed = TRUE
  )
  expect_error(
    pyramids(country = "Honduras", color = "black"),
    "color must be a character vector of length 2",
    fixed = TRUE
  )
})

test_that("pyramids returns a grob from mocked population data", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("gridExtra")

  testthat::local_mocked_bindings(
    call.data = function(...) fixture_population_data(),
    .package = "CepalStatR"
  )

  plot_file <- tempfile(fileext = ".pdf")
  grDevices::pdf(plot_file)
  on.exit(grDevices::dev.off(), add = TRUE)

  out <- pyramids(
    country = "Honduras",
    years = 16,
    language.en = TRUE,
    save = FALSE,
    caption = FALSE,
    progress = FALSE
  )

  expect_true(inherits(out, "grob"))
  expect_true(inherits(out, "gtable"))
})
