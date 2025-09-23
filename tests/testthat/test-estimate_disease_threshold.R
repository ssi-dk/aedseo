test_that("Test that only one season can be used", {
  skip_if_not_installed("withr")
  withr::local_seed(123)
  # Generate seasonal data
  tsd_data <- generate_seasonal_data(
    years = 1,
    start_date = as.Date("2021-01-01")
  )

  disease_threshold <- estimate_disease_threshold(tsd_data)

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

  disease_threshold <- estimate_disease_threshold(tsd_data)

  expect_equal(
    disease_threshold$note,
    "No seasons met the criteria."
  )
})

test_that("Test changes in input", {
  skip_if_not_installed("withr")
  withr::local_seed(111)
  # Generate seasonal data
  tsd_data <- generate_seasonal_data(
    years = 6,
    start_date = as.Date("2021-01-01"),
    noise_overdispersion = 3,
    phase = 2
  )

  last_season <- tsd_data |>
    dplyr::mutate(season = epi_calendar(time)) |>
    dplyr::pull(season) |>
    unique() |>
    dplyr::last()

  disease_threshold_five_seasons <- estimate_disease_threshold(
    tsd_data,
    use_prev_seasons_num = 5
  )

  disease_threshold_not_skip_cur <- estimate_disease_threshold(
    tsd_data,
    skip_current_season = FALSE
  )

  expect_false(last_season %in% disease_threshold_five_seasons$seasons)

  expect_contains(disease_threshold_not_skip_cur$seasons, "2026/2027")

  expect_gt(
    length(disease_threshold_five_seasons$seasons),
    length(disease_threshold_not_skip_cur$seasons)
  )
})

test_that("Test that selection and merging of sequences works as expected", {
  skip_if_not_installed("withr")
  withr::local_seed(111)
  # Generate seasonal data
  tsd_data <- generate_seasonal_data(
    years = 5,
    start_date = as.Date("2021-01-01"),
    noise_overdispersion = 5,
    relative_epidemic_concentration = 3,
    phase = 2
  )

  onset <- seasonal_onset(tsd_data, only_current_season = FALSE, season_start = 21)
  sign_warnings <- consecutive_growth_warnings(onset)
  sign_warnings <- sign_warnings |>
    dplyr::arrange(.data$reference_time) |>
    dplyr::filter(.data$growth_warning == TRUE) |>
    dplyr::reframe(
      significant_observations_window = dplyr::n(),
      start_window_time = dplyr::first(.data$reference_time),
      end_window_time = dplyr::last(.data$reference_time),
      start_average_observations_window = dplyr::first(.data$average_observations_window),
      .by = c("season", "groupID")
    )

  dt_min_seven <- estimate_disease_threshold(
    tsd_data,
    use_prev_seasons_num = 5,
    min_significant_time = 7,
    skip_current_season = FALSE
  )

  min_seven_seq <- sign_warnings |>
    dplyr::filter(.data$significant_observations_window >= 7)

  expect_equal(dt_min_seven$seasons, unique(min_seven_seq$season))

  dt_default_gap <- estimate_disease_threshold(
    tsd_data,
    use_prev_seasons_num = 5
  )

  dt_change_gap <- estimate_disease_threshold(
    tsd_data,
    max_gap_time = 2,
    use_prev_seasons_num = 5
  )

  expect_false(dt_default_gap$disease_threshold == dt_change_gap$disease_threshold)
  expect_gt(dt_default_gap$disease_threshold, dt_change_gap$disease_threshold)
})
