test_that("live CEPALSTAT API returns a catalogue when explicitly enabled", {
  skip_if_not(
    identical(Sys.getenv("CEPALSTAT_LIVE_TESTS"), "true"),
    paste(
      "Live API tests are opt-in.",
      "Set CEPALSTAT_LIVE_TESTS=true to run them."
    )
  )

  out <- call.indicators(language.en = TRUE, progress = FALSE)

  expect_s3_class(out, "data.frame")
  expect_gt(nrow(out), 0L)
  expect_true(all(c("Indicator Name", "Indicator ID") %in% names(out)))
})

