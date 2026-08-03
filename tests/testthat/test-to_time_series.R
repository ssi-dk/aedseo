test_that("Can correctly make an 'tsd' class object", {
  tsd_week <- to_time_series(
    cases = c(10, 15, 20, 18),
    time = seq(from = as.Date("2023-01-01"), by = "1 week", length.out = 4)
  )

  expect_s3_class(object = tsd_week, class = "tsd")
  expect_equal(attr(tsd_week, "time_interval"), "weeks")
})

test_that("Test that input argument checks work", {
  # Expect no error
  expect_no_error(to_time_series(
    cases = c(100, 120, 150, 180, 220, 270),
    time = seq(from = as.Date("2023-01-01"), by = "1 day", length.out = 6),
  ))

  expect_no_error(to_time_series(
    cases = c(100, 120, 150, 180, 220, 270),
    population = c(100000, 100050, 100000, 100000, 100002, 100100),
    time = seq(from = as.Date("2023-01-01"), by = "1 day", length.out = 6),
    time_interval = "days"
  ))

  #  Expect error for observation not being numeric
  expect_error(to_time_series(
    cases = c("100", "120", "150", "180", "220", "270"),
    time = seq(from = as.Date("2023-01-01"), by = "1 day", length.out = 6),
    time_interval = "days"
  ))

  #  Expect error for time not being dates
  expect_error(to_time_series(
    cases = c(100, 120, 150, 180, 220, 270),
    time = c(
      "2023-01-01",
      "2023-01-02",
      "2023-01-03",
      "2023-01-04",
      "2023-01-05",
      "2023-01-06"
    ),
    time_interval = "days"
  ))

  #  Expect error for wrong time_interval
  expect_error(to_time_series(
    cases = c(100, 120, 150, 180, 220, 270),
    time = seq(from = as.Date("2023-01-01"), by = "1 day", length.out = 6),
    time_interval = "years"
  ))

  #  Accept names that match time_interval
  expect_no_error(to_time_series(
    cases = c(100, 120, 150, 180, 220, 270),
    time = seq(from = as.Date("2023-01-01"), by = "1 day", length.out = 6),
    time_interval = "w"
  ))
})

test_that("cases vs. incidence input/conversion works as expected", {
  tsd_cases <- to_time_series(
    cases = c(10, 15, 20, 18),
    time = seq(from = as.Date("2023-01-01"), by = "1 week", length.out = 4)
  )
  expect_named(tsd_cases, c("time", "cases"))
  expect_identical(attr(tsd_cases, "outcome_type"), "cases")

  tsd_cal_incidence <- to_time_series(
    cases = c(10, 15, 20, 18),
    population = c(1e+06, 1e+06, 1e+06, 1e+06),
    time = seq(from = as.Date("2023-01-01"), by = "1 week", length.out = 4)
  )
  expect_named(tsd_cal_incidence, c("time", "cases", "incidence", "population"))
  expect_identical(attr(tsd_cal_incidence, "outcome_type"), "incidence")

  expect_error(to_time_series(
    incidence = c(1.0, 1.5, 2.0, 1.8),
    time = seq(from = as.Date("2023-01-01"), by = "1 week", length.out = 4)
  ),
  "assumes integer counts",
  fixed = TRUE)

  expect_error(to_time_series(
    population = c(1e+06, 1e+06, 1e+06, 1e+06),
    time = seq(from = as.Date("2023-01-01"), by = "1 week", length.out = 4)
  ),
  "Either cases or incidence must be given")

  tsd_miss_cases <- to_time_series(
    incidence = c(1.0, 1.5, 2.0, 1.8),
    population = c(1e+06, 1e+06, 1e+06, 1e+06),
    time = seq(from = as.Date("2023-01-01"), by = "1 week", length.out = 4)
  )
  expect_named(tsd_miss_cases, c("time", "cases", "incidence", "population"))
})

test_that("binomial input is converted to proportion-scale tsd", {
  time <- seq(from = as.Date("2023-01-01"), by = "1 week", length.out = 3)

  tsd_successes <- to_time_series(
    cases = c(10L, 12L, 30L),
    samples = c(100L, 120L, 150L),
    time = time
  )

  expect_s3_class(tsd_successes, "tsd")
  expect_named(tsd_successes, c("time", "cases", "proportion", "samples"))
  expect_equal(tsd_successes$cases, c(10L, 12L, 30L))
  expect_equal(tsd_successes$samples, c(100L, 120L, 150L))
  expect_equal(tsd_successes$proportion, c(0.1, 0.1, 0.2))
  expect_equal(attr(tsd_successes, "incidence_denominator"), 1)
  expect_identical(attr(tsd_successes, "outcome_type"), "proportion")

  tsd_proportion <- to_time_series(
    proportion = c(10, 25, 0.5),
    samples = c(100L, 200L, 20L),
    time = time
  )

  expect_named(tsd_proportion, c("time", "cases", "proportion", "samples"))
  expect_equal(tsd_proportion$cases, c(10, 50, 0))
  expect_equal(tsd_proportion$proportion, c(0.1, 0.25, 0.005))
  expect_equal(tsd_proportion$samples, c(100L, 200L, 20L))
})

test_that("binomial input validation catches invalid combinations", {
  time <- seq(from = as.Date("2023-01-01"), by = "1 week", length.out = 2)

  expect_error(
    to_time_series(cases = c(3L, 5L), samples = c(2L, 4L), time = time),
    "less than or equal"
  )
  expect_error(
    to_time_series(proportion = c(0.5, 101), samples = c(10L, 10L), time = time),
    "between 0 and 1"
  )
  expect_error(
    to_time_series(
      incidence = c(1, 2),
      population = c(100, 100),
      samples = c(10L, 10L),
      time = time
    ),
    "Count inputs.*cannot be combined with binomial inputs"
  )
})
