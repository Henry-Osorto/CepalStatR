test_that("call.data returns provenance metadata", {

  testthat::local_mocked_bindings(
    cepal_get = function(...) fixture_indicator_response(),
    .package = "CepalStatR"
  )

  data <- call.data(
    id.indicator = "4788",
    language.en = TRUE,
    progress = FALSE
  )

  expect_s3_class(data, "data.frame")
  expect_s3_class(attr(data, "retrieved_at"), "POSIXct")

  expect_type(
    attr(data, "package_version"),
    "character"
  )

  expect_identical(
    attr(data, "indicator_id"),
    "4788"
  )

  expect_identical(
    attr(data, "language"),
    "en"
  )

  expect_true(
    grepl(
      "indicator/4788/data",
      attr(data, "api_endpoint"),
      fixed = TRUE
    )
  )
})


test_that("call.indicators preserves provenance metadata", {

  testthat::local_mocked_bindings(
    cepal_get = function(...) fixture_thematic_tree(),
    .package = "CepalStatR"
  )

  indicators <- call.indicators(
    language.en = TRUE,
    progress = FALSE
  )

  expect_s3_class(indicators, "data.frame")
  expect_s3_class(
    attr(indicators, "retrieved_at"),
    "POSIXct"
  )

  expect_type(
    attr(indicators, "package_version"),
    "character"
  )

  expect_identical(
    attr(indicators, "language"),
    "en"
  )

  expect_true(
    grepl(
      "thematic-tree",
      attr(indicators, "api_endpoint"),
      fixed = TRUE
    )
  )
})
