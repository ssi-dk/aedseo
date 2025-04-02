test_that("Historical summary prints a row for each input season", {
  skip_if_not_installed("withr")
  withr::local_seed(123)
  # Generate seasonal data
  tsd_data <- generate_seasonal_data(
    years = 5,
    trend_rate = 1.001,
    noise_overdispersion = 3,
    start_date = as.Date("2021-01-01")
  )

  # Extract seasons
  org_seasons <- tsd_data |>
    dplyr::mutate(season = epi_calendar(time)) |>
    dplyr::distinct(season) |>
    dplyr::pull()

  # Estimate seasonal_onset
  tsd_onset <- seasonal_onset(
    tsd = tsd_data,
    disease_threshold = 10,
    family = "quasipoisson",
    season_start = 21,
    season_end = 20,
    only_current_season = FALSE
  )

  # Get historical summary seasons
  hs_seasons <- historical_summary(tsd_onset) |>
    dplyr::distinct(season) |>
    dplyr::pull()

  # Verify that the summary printed without errors
  expect_identical(org_seasons, hs_seasons)
})

test_that("Historical summary checks tsd_object correctly", {
  skip_if_not_installed("withr")
  withr::local_seed(123)
  # Generate seasonal data
  tsd_data <- generate_seasonal_data(
    years = 5,
    trend_rate = 1.001,
    noise_overdispersion = 3,
    start_date = as.Date("2021-01-01")
  )

  # Estimate seasonal_onset with threshold
  tsd_onset <- seasonal_onset(
    tsd = tsd_data,
    disease_threshold = 10,
    family = "quasipoisson"
  )
  expect_error(
    historical_summary(tsd_onset),
    "The tsd_onset object is not stratified by season"
  )

  # Estimate seasonal_onset with no needed arguments
  tsd_onset_2 <- seasonal_onset(
    tsd = tsd_data,
    family = "quasipoisson"
  )
  expect_error(
    historical_summary(tsd_onset_2),
    "Column 'seasonal_onset' not found in tsd_onset object."
  )
})
