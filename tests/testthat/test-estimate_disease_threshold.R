
test_that("Test that input argument checks work", {
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

  estimate_disease_threshold(onset_data)

})
