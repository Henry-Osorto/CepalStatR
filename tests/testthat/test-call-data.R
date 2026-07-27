test_that("call.data validates its public arguments without using the network", {
  expect_error(
    call.data(),
    "id.indicator must be a single indicator ID",
    fixed = TRUE
  )
  expect_error(
    call.data(c(1, 2)),
    "id.indicator must be a single indicator ID",
    fixed = TRUE
  )
  expect_error(
    call.data(1, language.en = NA),
    "language.en must be TRUE or FALSE",
    fixed = TRUE
  )
  expect_error(
    call.data(1, notes = NA),
    "notes must be TRUE or FALSE",
    fixed = TRUE
  )
  expect_error(
    call.data(1, progress = NA),
    "progress must be TRUE or FALSE",
    fixed = TRUE
  )
  expect_error(
    call.data(1, add.indicator.name = NA),
    "add.indicator.name must be TRUE or FALSE",
    fixed = TRUE
  )
})

test_that("call.data structures a mocked API response and preserves metadata", {
  testthat::local_mocked_bindings(
    cepal_get = function(...) fixture_indicator_response(),
    .package = "CepalStatR"
  )

  out <- call.data(
    id.indicator = "4788",
    language.en = TRUE,
    notes = TRUE,
    progress = FALSE
  )

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2L)
  expect_true(all(c("Value", "Country", "Years") %in% names(out)))
  expect_true(all(c("unit", "definition", "last_update") %in% names(out)))
  expect_true("Notes" %in% names(out))
  expect_equal(out$Country, c("Honduras", "Guatemala"))
  expect_equal(out$Years, c("2020", "2021"))
  expect_equal(out$Value, c(10.5, 11.0))
  expect_equal(out$Notes[1], "Provisional value")
  expect_equal(as.character(out$indicator_id), c("4788", "4788"))
  expect_false(any(grepl("^dim_", names(out))))
})

