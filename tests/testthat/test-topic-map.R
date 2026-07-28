test_that("topic_map validates its public arguments", {
  expect_error(
    topic_map(language.en = NA, progress = FALSE),
    "language.en must be TRUE or FALSE",
    fixed = TRUE
  )
  expect_error(
    topic_map(progress = NA),
    "progress must be TRUE or FALSE",
    fixed = TRUE
  )
  expect_error(
    topic_map(open.browser = NA, progress = FALSE),
    "open.browser must be TRUE or FALSE",
    fixed = TRUE
  )
})

test_that("topic_map creates a browsable tree from mocked indicators", {
  skip_if_not_installed("collapsibleTree")
  skip_if_not_installed("htmlwidgets")

  testthat::local_mocked_bindings(
    call.indicators = function(...) fixture_indicator_catalogue(),
    .package = "CepalStatR"
  )

  out <- topic_map(
    language.en = TRUE,
    progress = FALSE,
    open.browser = FALSE
  )

  expect_true(htmltools::is.browsable(out))
  expect_true(inherits(out, "shiny.tag"))
  expect_match(as.character(out), "Interactive thematic map", fixed = TRUE)
  expect_match(as.character(out), "Generated with CepalStatR", fixed = TRUE)
})
