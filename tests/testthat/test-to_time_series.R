test_that("Can correctly make an 'tsd' class object", {
  tsd_day <- to_time_series(
    observation = c(10, 15, 20, 18),
    time = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03", "2023-01-05")),
    time_interval = "day"
  )

  expect_s3_class(object = tsd_day, class = "tsd")
})

test_that("Test that input argument checks work", {
  # Expect no error
  expect_no_error(to_time_series(
    observation = c(100, 120, 150, 180, 220, 270),
    time = as.Date(c(
      "2023-01-01",
      "2023-01-02",
      "2023-01-03",
      "2023-01-04",
      "2023-01-05",
      "2023-01-06"
    )),
    time_interval = "day"
  ))

  expect_no_error(to_time_series(
    observation = c(100, 120, 150, 180, 220, 270),
    population = c(100000, 100050, 100000, 100000, 100002, 100100),
    time = as.Date(c(
      "2023-01-01",
      "2023-01-02",
      "2023-01-03",
      "2023-01-04",
      "2023-01-05",
      "2023-01-06"
    )),
    time_interval = "day"
  ))

  #  Expect error for observation not being numeric
  expect_error(to_time_series(
    observation = c("100", "120", "150", "180", "220", "270"),
    time = as.Date(c(
      "2023-01-01",
      "2023-01-02",
      "2023-01-03",
      "2023-01-04",
      "2023-01-05",
      "2023-01-06"
    )),
    time_interval = "day"
  ))

  #  Expect error for time not being dates
  expect_error(to_time_series(
    observation = c(100, 120, 150, 180, 220, 270),
    time = c(
      "2023-01-01",
      "2023-01-02",
      "2023-01-03",
      "2023-01-04",
      "2023-01-05",
      "2023-01-06"
    ),
    time_interval = "day"
  ))

  #  Expect error for wrong time_interval
  expect_error(to_time_series(
    observation = c(100, 120, 150, 180, 220, 270),
    time = as.Date(c(
      "2023-01-01",
      "2023-01-02",
      "2023-01-03",
      "2023-01-04",
      "2023-01-05",
      "2023-01-06"
    )),
    time_interval = "year"
  ))
})
