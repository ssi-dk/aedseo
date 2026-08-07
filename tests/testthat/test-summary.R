test_that("Summary prints without disease threshold (tsd_onset object)", {
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

test_that("Summary prints with disease threshold (tsd_onset object)", {
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

test_that("Summary prints proportional output when no onset is detected", {
  tsd_data <- generate_seasonal_data(
    years = 1,
    mean = 0.3,
    amplitude = 0,
    noise_overdispersion = 0,
    samples = 100
  )
  tsd_onset <- seasonal_onset(
    tsd = tsd_data,
    family = "quasibinomial",
    disease_threshold = 0.9
  )

  tmp <- capture_output(summary(tsd_onset))

  expect_match(tmp, "Summary of tsd_onset object with disease_threshold", fixed = TRUE)
  expect_match(tmp, "Observations at reference time point: NA", fixed = TRUE)
  expect_match(tmp, "Disease specific threshold: 0.9", fixed = TRUE)
})

test_that("Summary prints of burden_levels object", {
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

test_that("Summary prints fractional settings for proportional burden levels", {
  skip_if_not_installed("withr")
  withr::local_seed(123)

  tsd_data <- generate_seasonal_data(
    years = 3,
    mean = 0.3,
    amplitude = 0.2,
    noise_overdispersion = 0,
    samples = 100
  )

  tsd_burden_levels <- seasonal_burden_levels(
    tsd = tsd_data,
    disease_threshold = 0.1,
    family = "beta"
  )

  tmp <- capture_output(summary(tsd_burden_levels))

  expect_match(tmp, "Disease specific threshold: 0.1", fixed = TRUE)
  expect_match(tmp, "Incidence denominator: 1", fixed = TRUE)
})
