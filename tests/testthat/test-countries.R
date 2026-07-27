test_that("countries validates the language argument", {
  expect_error(
    countries(language.en = NA),
    "language.en must be TRUE or FALSE",
    fixed = TRUE
  )
})

test_that("countries returns sorted unique country labels from mocked dimensions", {
  testthat::local_mocked_bindings(
    get_cepal_dimensions = function(...) fixture_dimensions_response(),
    .package = "CepalStatR"
  )

  out <- countries(language.en = TRUE)

  expect_s3_class(out, "data.frame")
  expect_identical(names(out), "Countries")
  expect_identical(out$Countries, c("Guatemala", "Honduras"))
})
