test_that("Summary prints without disease threshold", {
  skip_if_not_installed("withr")
  withr::local_seed(123)
  # Generate seasonal data
  tsd_data <- generate_seasonal_data(
    years = 1,
    start_date = as.Date("2021-01-01")
  )

  # Estimate seasonal_onset with a 3-day window
  tsd_onset <- seasonal_onset(
    tsd = tsd_data,
    k = 3,
    level = 0.95,
    family = "quasipoisson"
  )

  # Capture the output of the summary function
  tmp <- capture_output(summary(tsd_onset))

  # Verify that the summary printed without errors
  expect_true(grepl(pattern = "Summary of tsd_onset object without disease_threshold", x = tmp))
})

test_that("Summary prints with disease threshold", {
  skip_if_not_installed("withr")
  withr::local_seed(123)
  # Generate seasonal data
  tsd_data <- generate_seasonal_data(
    years = 1,
    start_date = as.Date("2021-01-01")
  )

  # Estimate seasonal_onset with a 3-day window and disease threshold
  tsd_onset <- seasonal_onset(
    tsd = tsd_data,
    k = 3,
    level = 0.95,
    family = "quasipoisson",
    disease_threshold = 100
  )

  # Capture the output of the summary function
  tmp <- capture_output(summary(tsd_onset))

  # Verify that the summary printed without errors
  expect_true(grepl(pattern = "Summary of tsd_onset object with disease_threshold", x = tmp))
})

test_that("Summary prints with disease threshold", {
  skip_if_not_installed("withr")
  withr::local_seed(123)
  # Generate seasonal data
  tsd_data <- generate_seasonal_data(
    years = 1,
    start_date = as.Date("2021-01-01")
  )

  # Estimate seasonal burden levels with disease threshold
  tsd_burden_levels <- seasonal_burden_levels(
    tsd = tsd_data,
    disease_threshold = 100
  )

  # Capture the output of the summary function
  tmp <- capture_output(summary(tsd_burden_levels))

  # Verify that the summary printed without errors
  expect_true(grepl(pattern = "Summary of tsd_burden_levels object", x = tmp))
})
