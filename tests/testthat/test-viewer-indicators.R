test_that("viewer.indicators validates display arguments before data retrieval", {
  expect_error(
    viewer.indicators(page_size = 0, progress = FALSE),
    "page_size must be a positive number",
    fixed = TRUE
  )
  expect_error(
    viewer.indicators(open.browser = NA, progress = FALSE),
    "open.browser must be TRUE or FALSE",
    fixed = TRUE
  )
})

test_that("viewer.indicators creates a browsable object from mocked indicators", {
  testthat::local_mocked_bindings(
    call.indicators = function(...) fixture_indicator_catalogue(),
    .package = "CepalStatR"
  )

  out <- viewer.indicators(
    language.en = TRUE,
    progress = FALSE,
    page_size = 10,
    open.browser = FALSE
  )

  expect_true(inherits(out, "html_browsable"))
  expect_true(inherits(out, "shiny.tag"))
  expect_match(as.character(out), "Available indicators", fixed = TRUE)
  expect_match(as.character(out), "Generated with CepalStatR", fixed = TRUE)
})
