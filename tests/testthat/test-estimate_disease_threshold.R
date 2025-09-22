test_that("Test that only one season can be used", {
  skip_if_not_installed("withr")
  withr::local_seed(123)
  # Generate seasonal data
  tsd_data <- generate_seasonal_data(
    years = 1,
    start_date = as.Date("2021-01-01")
  )

  onset_data <- seasonal_onset(
    tsd = tsd_data,
    season_start = 21,
    only_current_season = FALSE
  )

  disease_threshold <- estimate_disease_threshold(onset_data)

  expect_equal(
    disease_threshold$note,
    "Only one season is used to determine the threshold."
  )
})

test_that("Test output of correct note for no seasons meeting input criteria", {
  skip_if_not_installed("withr")
  withr::local_seed(123)
  # Generate seasonal data
  tsd_data <- generate_seasonal_data(
    years = 1,
    start_date = as.Date("2021-01-01")
  )

  tsd_data <- tsd_data |>
    dplyr::mutate(
      cases = 100
    )

  onset_data <- seasonal_onset(
    tsd = tsd_data,
    season_start = 21,
    only_current_season = FALSE
  )

  disease_threshold <- estimate_disease_threshold(onset_data)

  expect_equal(
    disease_threshold$note,
    "No seasons met the criteria."
  )
})

test_that("Test changes in input", {
  skip_if_not_installed("withr")
  withr::local_seed(123)
  # Generate seasonal data
  tsd_data <- generate_seasonal_data(
    years = 6,
    start_date = as.Date("2021-01-01")
  )

  onset_data <- seasonal_onset(
    tsd = tsd_data,
    season_start = 21,
    only_current_season = FALSE
  )

  disease_threshold_five_seasons <- estimate_disease_threshold(
    onset_data,
    use_prev_seasons_num = 5
  )

  disease_threshold_not_skip_cur <- estimate_disease_threshold(
    onset_data,
    skip_current_season = FALSE
  )

  expect_length(disease_threshold_five_seasons$seasons, 5)

  expect_contains(disease_threshold_not_skip_cur$seasons, "2026/2027")

  expect_gt(
    length(disease_threshold_five_seasons$seasons),
    length(disease_threshold_not_skip_cur$seasons)
  )
})
