test_that("Test that selection of current and all seasons work as expected", {
  skip_if_not_installed("withr")
  withr::local_seed(123)
  tsd_data <- generate_seasonal_data(
    years = 3,
    start_date = as.Date("2021-01-04")
  )

  current_season <- epi_calendar(dplyr::last(tsd_data$time))

  current_season_output <- combined_seasonal_output(tsd_data, only_current_season = TRUE)
  all_seasons_output <- combined_seasonal_output(tsd_data, only_current_season = FALSE)

  expect_equal(unique(current_season_output$onset_output$season), current_season)
  expect_equal(unique(current_season_output$burden_output$season), current_season)

  expect_gt(length(unique(all_seasons_output$onset_output$season)), 1)
  expect_gt(length(all_seasons_output$burden_output), 1)
})

test_that("Test that onset_output has one more season than burden_output", {
  skip_if_not_installed("withr")
  withr::local_seed(123)
  tsd_data <- generate_seasonal_data(
    years = 3,
    start_date = as.Date("2021-01-04")
  )

  all_seasons_output <- combined_seasonal_output(tsd_data, only_current_season = FALSE)

  expect_length(unique(all_seasons_output$onset_output$season), 4)
  expect_length(all_seasons_output$burden_output, 3)
})

test_that("Test that default arguments can be overwritten", {
  skip_if_not_installed("withr")
  withr::local_seed(123)
  tsd_data <- generate_seasonal_data(
    years = 5,
    amplitude = 100,
    mean = 100,
    start_date = as.Date("2021-01-04"),
    noise_overdispersion = 5,
    trend_rate = 1.006
  )

  default_args <- combined_seasonal_output(tsd_data)
  changed_conf_levels <- combined_seasonal_output(
    tsd_data,
    conf_levels = 0.975
  )

  expect_false(default_args$burden_output$values[["high"]] == changed_conf_levels$burden_output$values[["high"]])

  changed_n_peak <- combined_seasonal_output(
    tsd_data,
    n_peak = 10
  )

  expect_false(default_args$burden_output$values[["high"]] == changed_n_peak$burden_output$values[["high"]])

  changed_decay_factor <- combined_seasonal_output(
    tsd_data,
    decay_factor = 0.6
  )

  expect_false(default_args$burden_output$values[["high"]] == changed_decay_factor$burden_output$values[["high"]])

  changed_dt <- combined_seasonal_output(
    tsd_data,
    disease_threshold = 500
  )

  expect_false(default_args$burden_output$values[["medium"]] == changed_dt$burden_output$values[["medium"]])

  expect_false(identical(default_args$onset_output$seasonal_onset, changed_dt$onset_output$seasonal_onset))

  changed_window <- combined_seasonal_output(
    tsd_data,
    k = 10
  )

  expect_false(
    identical(
      default_args$onset_output$average_observations_window,
      changed_window$onset_output$average_observations_window
    )
  )
})

test_that("Test that family argument works as expected", {
  skip_if_not_installed("withr")
  withr::local_seed(123)
  # Generate seasonal data
  tsd_data <- generate_seasonal_data(
    years = 3,
    start_date = as.Date("2021-01-04")
  )

  expect_no_error(combined_seasonal_output(
    tsd = tsd_data,
    family = "poisson",
  ))

  expect_no_error(combined_seasonal_output(
    tsd = tsd_data,
    family = stats::poisson(),
  ))

  expect_no_error(combined_seasonal_output(
    tsd = tsd_data,
    family = stats::poisson(link = "log"),
  ))

  expect_error(combined_seasonal_output(
    tsd = tsd_data,
    family = "stats::poisson(link = log)",
  ))

  expect_error(combined_seasonal_output(
    tsd = tsd_data,
    family = "hello",
  ))

})

test_that("Test that multiple waves feature works as expected", {
  skip_if_not_installed("withr")
  withr::local_seed(123)
  tsd_data <- generate_seasonal_data(
    years = 3,
    start_date = as.Date("2021-01-04"),
    noise_overdispersion = 100,
    phase = 4
  )

  expect_no_error(combined_seasonal_output(
    tsd_data,
    disease_threshold = 2,
    only_current_season = TRUE,
    multiple_waves = TRUE,
    burden_level_decrease = "medium",
    steps_with_decrease = 1
  ))

})
