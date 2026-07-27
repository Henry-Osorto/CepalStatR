test_that("call.indicators validates scalar logical arguments", {
  expect_error(
    call.indicators(language.en = NA, progress = FALSE),
    "language.en must be TRUE or FALSE",
    fixed = TRUE
  )
  expect_error(
    call.indicators(language.en = TRUE, progress = NA),
    "progress must be TRUE or FALSE",
    fixed = TRUE
  )
})

test_that("call.indicators flattens a mocked thematic tree", {
  testthat::local_mocked_bindings(
    cepal_get = function(...) fixture_thematic_tree(),
    .package = "CepalStatR"
  )

  out <- call.indicators(language.en = TRUE, progress = FALSE)

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 1L)
  expect_identical(
    names(out),
    c(
      "Area",
      "Dimension",
      "Subdimension",
      "Group",
      "Sub Group Level 1",
      "Sub Group Level 2",
      "Indicator Name",
      "Indicator ID"
    )
  )
  expect_equal(out$Area, "Social statistics")
  expect_equal(out$`Indicator Name`, "Total population, by sex")
  expect_equal(as.character(out$`Indicator ID`), "4788")
})

