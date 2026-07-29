test_that("call.data returns provenance metadata", {

  data <- call.data(
    id.indicator = "4788",
    language.en = TRUE,
    progress = FALSE
  )

  expect_s3_class(data, "data.frame")

  expect_true(
    "Value" %in% names(data)
  )

  expect_false(
    is.null(attr(data, "retrieved_at"))
  )

  expect_false(
    is.null(attr(data, "package_version"))
  )

  expect_false(
    is.null(attr(data, "indicator_id"))
  )

  expect_equal(
    attr(data, "indicator_id"),
    "4788"
  )

  expect_type(
    attr(data, "package_version"),
    "character"
  )

})




test_that("call.indicators preserves provenance metadata", {

  indicators <- call.indicators(
    language.en = TRUE,
    progress = FALSE
  )

  expect_s3_class(indicators, "data.frame")

  expect_false(
    is.null(attr(indicators, "retrieved_at"))
  )

  expect_false(
    is.null(attr(indicators, "package_version"))
  )

})
