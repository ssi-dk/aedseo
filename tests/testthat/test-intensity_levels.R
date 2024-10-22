test_that("Test that input argument checks work", {

  start_date <- as.Date("2021-01-04")
  end_date <- as.Date("2023-12-31")

  weekly_dates <- seq.Date(from = start_date,
                           to = end_date,
                           by = "week")

  obs <- rpois(length(weekly_dates),1000)

  tsd_data <- tsd(
    observed = obs,
    time = as.Date(weekly_dates),
    time_interval = "week"
  )

  expect_no_error(intensity_levels(tsd_data))

})
