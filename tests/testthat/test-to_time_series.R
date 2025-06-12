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
})

test_that("cases vs. incidence input/conversion works as expected", {
  tsd_cases <- to_time_series(
    cases = c(10, 15, 20, 18),
    time = seq(from = as.Date("2023-01-01"), by = "1 week", length.out = 4)
  )
  expect_named(tsd_cases, c("time", "cases"))

  tsd_cal_incidence <- to_time_series(
    cases = c(10, 15, 20, 18),
    population = c(1e+06, 1e+06, 1e+06, 1e+06),
    time = seq(from = as.Date("2023-01-01"), by = "1 week", length.out = 4)
  )
  expect_named(tsd_cal_incidence, c("time", "cases", "population", "incidence"))

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
  "Either cases or incidence must be assigned")

  tsd_miss_cases <- to_time_series(
    incidence = c(1.0, 1.5, 2.0, 1.8),
    population = c(1e+06, 1e+06, 1e+06, 1e+06),
    time = seq(from = as.Date("2023-01-01"), by = "1 week", length.out = 4)
  )
  expect_named(tsd_miss_cases, c("time", "incidence", "population", "cases"))
})
