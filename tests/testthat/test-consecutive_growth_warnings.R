test_that("Consecutive growth warnings includes all seasons and output columns", {
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
    family = "quasipoisson",
    season_start = 21,
    season_end = 20,
    only_current_season = FALSE
  )

  # Get significant obs seasons
  cgr_data <- consecutive_growth_warnings(tsd_onset)

  # Verify that all seasons are included
  expect_identical(
    org_seasons,
    cgr_data |>
      dplyr::distinct(season) |>
      dplyr::pull()
  )

  # Verify that columns exist
  expect_true(
    all(c("counter", "changeFlag", "groupID", "significant_counter") %in% names(cgr_data))
  )
})
