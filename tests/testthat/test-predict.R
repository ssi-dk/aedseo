test_that("Returns the desired length", {
  skip_if_not_installed("withr")
  withr::local_seed(222)
  # Generate some sample data
  tsd_data <- generate_seasonal_data(
    years = 1,
    start_date = as.Date("2023-01-01"),
    time_interval = "day"
  )

  # Employ the seasonal_onset function
  tsd_results <- seasonal_onset(
    tsd = tsd_data,
    k = 7,
    level = 0.95,
    family = "poisson"
  )

  # Predict observations for the next 7 time steps
  prediction <- predict(object = tsd_results, n_step = 7)

  # Return the number of prediction + the initial observation
  expect_length(prediction$estimate, 8)
})

test_that("Can correctly make an 'tsd_predict' class object", {
  skip_if_not_installed("withr")
  withr::local_seed(222)
  # Generate some sample data
  tsd_data <- generate_seasonal_data(
    years = 1,
    start_date = as.Date("2023-01-01"),
    time_interval = "day"
  )

  # Employ the seasonal_onset function
  tsd_results <- seasonal_onset(
    tsd = tsd_data,
    k = 7,
    level = 0.95,
    family = "poisson"
  )

  # Predict observations for the next 7 time steps
  prediction <- predict(object = tsd_results, n_step = 7)

  # Return the number of prediction + the initial observation
  expect_s3_class(object = prediction, class = "tsd_predict")
})

test_that("Can correctly use weekly time_interval classification", {
  skip_if_not_installed("withr")
  withr::local_seed(222)
  # Generate some sample data
  tsd_data <- generate_seasonal_data(
    years = 1,
    start_date = as.Date("2023-01-01"),
    time_interval = "week"
  )

  # Employ the seasonal_onset function
  tsd_results <- seasonal_onset(
    tsd = tsd_data,
    k = 7,
    level = 0.95,
    family = "poisson"
  )

  # Get subsequent 5 weeks of data
  weeks <- seq.Date(from = dplyr::last(tsd_results$reference_time), by = "week", length.out = 6)

  # Make prediction of next 5 time steps
  prediction <- predict(object = tsd_results, n_step = 5)

  expect_equal(weeks, prediction$reference_time)
})

test_that("Can correctly use daily time_interval classification", {
  skip_if_not_installed("withr")
  withr::local_seed(222)
  # Generate some sample data
  tsd_data <- generate_seasonal_data(
    years = 1,
    start_date = as.Date("2023-01-01"),
    time_interval = "day"
  )

  # Employ the seasonal_onset function
  tsd_results <- seasonal_onset(
    tsd = tsd_data,
    k = 7,
    level = 0.95,
    family = "poisson"
  )

  # Get subsequent 5 days of data
  days <- seq.Date(from = dplyr::last(tsd_results$reference_time), by = "day", length.out = 6)

  # Make prediction of next 5 time steps
  prediction <- predict(object = tsd_results, n_step = 5)

  expect_equal(days, prediction$reference_time)
})

test_that("Can correctly use monthly time_interval classification", {
  skip_if_not_installed("withr")
  withr::local_seed(222)
  # Generate some sample data
  tsd_data <- generate_seasonal_data(
    years = 1,
    start_date = as.Date("2023-01-01"),
    time_interval = "month"
  )

  # Employ the seasonal_onset function
  tsd_results <- seasonal_onset(
    tsd = tsd_data,
    k = 7,
    level = 0.95,
    family = "poisson"
  )

  # Get subsequent 5 days of data
  months <- seq.Date(from = dplyr::last(tsd_results$reference_time), by = "30 days", length.out = 6)

  # Make prediction of next 5 time steps
  prediction <- predict(object = tsd_results, n_step = 5)

  expect_equal(months, prediction$reference_time)
})
