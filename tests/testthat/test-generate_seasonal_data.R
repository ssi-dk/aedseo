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
    generate_seasonal_data(amplitude = -1),
    "Variable 'amplitude': Element 1 is not >= 0."
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

  expect_error(
    generate_seasonal_data(noise_overdispersion = 0.5),
    "noise_overdispersion .*Must be.*FALSE"
  )
})

test_that("generate_seasonal_data() - output structure and defaults", {

  sim_data <- generate_seasonal_data()

  # Check that 'sim_data' inherits from 'tsd'
  expect_s3_class(sim_data, "tsd")

  # Check that the expected number of rows matches years * period
  expect_equal(nrow(sim_data), 3 * 52)

  # Check that 'time' and 'cases' columns exist
  expect_true(all(c("time", "cases") %in% names(sim_data)))

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
  expect_false(isTRUE(all.equal(no_noise_result$cases,
                                noisy_result$cases)))
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

  # Check difference in last vs. first case for each
  no_trend_diff   <- no_trend$cases[length(no_trend$cases)] - no_trend$cases[1]
  with_trend_diff <- with_trend$cases[length(with_trend$cases)] - with_trend$cases[1]

  # With trend should have larger difference than no trend scenario
  expect_true(with_trend_diff > no_trend_diff)
})

test_that("generate_seasonal_data() - lower_bound must be non-negative", {
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
      time_interval = "week",
      lower_bound = -1
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
  expect_true(var(no_variance$cases) == 0)
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

  expect_true(var(noise_poisson$cases) < var(noise_nbinom$cases))
})

test_that("generate_seasonal_data() - test increasing relative epidemic concentration", {
  skip_if_not_installed("withr")
  withr::local_seed(123)
  # Pure sinusoidal
  sinusoidal <- generate_seasonal_data(
    years         = 1,
    start_date    = as.Date("2021-05-26"),
    amplitude     = 100,
    mean          = 100,
    phase         = 0,
    trend_rate    = NULL,
    noise_overdispersion = 1,
    relative_epidemic_concentration = 1,
    time_interval = "week"
  )

  # Concentrated
  concentrated <- generate_seasonal_data(
    years         = 1,
    start_date    = as.Date("2021-05-26"),
    amplitude     = 100,
    mean          = 100,
    phase         = 0,
    trend_rate    = NULL,
    noise_overdispersion = 1,
    relative_epidemic_concentration = 3,
    time_interval = "week"
  )

  sinusoidal_0 <- nrow(sinusoidal[sinusoidal$cases == 0, ])
  concentrated_0 <- nrow(concentrated[concentrated$cases == 0, ])

  expect_gt(concentrated_0, sinusoidal_0)
})
