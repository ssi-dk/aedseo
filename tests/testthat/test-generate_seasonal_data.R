# test-generate_seasonal_data.R
test_that("generate_seasonal_data() - input argument checks", {
  expect_error(
    generate_seasonal_data(years = 0),
    "Variable 'years': Element 1 is not >= 1."
  )

  expect_error(
    generate_seasonal_data(start_date = "Not a date"),
    "Variable 'start_date': Must be of class 'Date', not 'character'."
  )

  expect_error(
    generate_seasonal_data(amplitude = 0),
    "Variable 'amplitude': Element 1 is not >= 1."
  )

  expect_error(
    generate_seasonal_data(phase = -1),
    "Variable 'phase': Element 1 is not >= 0."
  )

  expect_error(
    generate_seasonal_data(trend_rate = -0.5),
    "Variable 'trend_rate': Element 1 is not >= 0."
  )

  expect_error(
    generate_seasonal_data(time_interval = "year"),
    "time_interval.*must be one of"
  )
})

test_that("generate_seasonal_data() - output structure and defaults", {

  sim_data <- generate_seasonal_data()

  # Check that 'sim_data' inherits from 'tsd'
  expect_s3_class(sim_data, "tsd")

  # Check that the expected number of rows matches years * period
  expect_equal(nrow(sim_data), 3 * 52)

  # Check that 'time' and 'observation' columns exist
  expect_true(all(c("time", "observation") %in% names(sim_data)))

  # Check that the time_interval is stored correctly
  expect_equal(attr(sim_data, "time_interval"), "week")
})

test_that("generate_seasonal_data() - noise works as expected", {
  skip_if_not_installed("withr")
  withr::local_seed(123)
  noisy_result <- generate_seasonal_data(
    years         = 1,
    start_date    = as.Date("2021-05-26"),
    amplitude     = 1000,
    mean          = 1000,
    phase         = 0,
    trend_rate    = 1.001,
    noise_overdispersion      = 10,
    time_interval = "week"
  )

  # Compare to a non-noisy version
  no_noise_result <- generate_seasonal_data(
    years         = 1,
    start_date    = as.Date("2021-05-26"),
    amplitude     = 1000,
    mean          = 1000,
    phase         = 0,
    trend_rate    = 1.001,
    noise_overdispersion      = NULL,
    time_interval = "week"
  )

  # The two should differ (in most or all rows) due to random noise
  expect_false(isTRUE(all.equal(no_noise_result$observation,
                                noisy_result$observation)))
})

test_that("generate_seasonal_data() - trend_rate = NULL implies no trend", {
  skip_if_not_installed("withr")
  withr::local_seed(123)
  no_trend <- generate_seasonal_data(
    years         = 1,
    start_date    = as.Date("2021-05-26"),
    amplitude     = 1000,
    mean          = 1000,
    phase         = 0,
    trend_rate    = NULL,     # No trend
    noise_overdispersion      = NULL,
    time_interval = "week"
  )

  # With a non-zero trend
  with_trend <- generate_seasonal_data(
    years         = 1,
    start_date    = as.Date("2021-05-26"),
    amplitude     = 1000,
    mean          = 1000,
    phase         = 0,
    trend_rate    = 1.01,
    noise_overdispersion      = NULL,
    time_interval = "week"
  )

  # Check difference in last vs. first observation for each
  no_trend_diff   <- no_trend$observation[length(no_trend$observation)] - no_trend$observation[1]
  with_trend_diff <- with_trend$observation[length(with_trend$observation)] - with_trend$observation[1]

  # With trend should have larger difference than no trend scenario
  expect_true(with_trend_diff > no_trend_diff)
})

test_that("generate_seasonal_data() - mean must be greater than amplitude", {
  skip_if_not_installed("withr")
  withr::local_seed(123)
  expect_error(
    generate_seasonal_data(
      years         = 1,
      start_date    = as.Date("2021-05-26"),
      amplitude     = 1000,
      mean          = 100,
      phase         = 0,
      trend_rate    = NULL,     # No trend
      noise_overdispersion      = NULL,
      time_interval = "week"
    )
  )
})

test_that("generate_seasonal_data() - no variance", {
  skip_if_not_installed("withr")
  withr::local_seed(123)
  no_variance <- generate_seasonal_data(
    years         = 1,
    start_date    = as.Date("2021-05-26"),
    amplitude     = 0,
    mean          = 100,
    phase         = 0,
    trend_rate    = NULL,     # No trend
    noise_overdispersion      = NULL,
    time_interval = "week"
  )

  # Variance should be zero when no dispersion is set
  expect_true(var(no_variance$observation) == 0)
})

test_that("generate_seasonal_data() - test increasing overdispersion", {
  skip_if_not_installed("withr")
  withr::local_seed(123)
  # With poisson noise
  noise_poisson <- generate_seasonal_data(
    years         = 1,
    start_date    = as.Date("2021-05-26"),
    amplitude     = 0,
    mean          = 100,
    phase         = 0,
    trend_rate    = NULL,
    noise_overdispersion      = 1,
    time_interval = "week"
  )

  # With negative binomial noise
  noise_nbinom <- generate_seasonal_data(
    years         = 1,
    start_date    = as.Date("2021-05-26"),
    amplitude     = 0,
    mean          = 100,
    phase         = 0,
    trend_rate    = NULL,
    noise_overdispersion      = 4,
    time_interval = "week"
  )

  expect_true(var(noise_poisson$observation) < var(noise_nbinom$observation))
})
