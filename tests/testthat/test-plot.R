test_that("Test that plot works for cases and incidence for tsd, tsd_onset, tsd_onset_and_burden objects", {
  skip_if_not_installed("withr")
  withr::local_seed(123)

  ## Cases

  # tsd
  tsd_data_cases <- generate_seasonal_data(
    years = 3,
    start_date = as.Date("2023-10-18")
  )

  cases_plot_1 <- plot(tsd_data_cases)

  expect_contains(names(cases_plot_1$data), "cases")

  # tsd_onset
  tsd_onset_cases <- seasonal_onset(
    tsd = tsd_data_cases
  )

  cases_plot_2 <- plot(tsd_onset_cases)

  expect_equal(all(cases_plot_2$data$incidence), NA)

  # tsd_onset_and_burden
  combined_cases <- combined_seasonal_output(tsd_data_cases)

  cases_plot_3 <- plot(combined_cases)

  expect_equal(all(cases_plot_3$data$incidence), NA)


  ## Incidence

  # tsd
  tsd_data_incidence <- to_time_series(
    time = tsd_data_cases$time,
    cases = tsd_data_cases$cases,
    population = 1e+06
  )

  incidence_plot_1 <- plot(tsd_data_incidence)

  expect_contains(names(incidence_plot_1$data), "incidence")

  # tsd_onset
  tsd_onset_incidence <- seasonal_onset(
    tsd = tsd_data_incidence
  )

  incidence_plot_2 <- plot(tsd_onset_incidence)

  expect_false(all(is.na(incidence_plot_2$data$incidence)))

  # tsd_onset_and_burden
  combined_incidence <- combined_seasonal_output(tsd_data_incidence, disease_threshold = 2)

  incidence_plot_3 <- plot(combined_incidence, y_lower_bound = 1)

  expect_false(all(is.na(incidence_plot_3$data$incidence)))
})

test_that("Test that plot works for cases and incidence in `tsd_growth_warning` objects", {
  skip_if_not_installed("withr")
  withr::local_seed(123)

  ## Cases
  tsd_data_cases <- generate_seasonal_data(
    years = 3,
    trend_rate = 0.997,
    start_date = as.Date("2023-10-18")
  )

  tsd_onset_cases <- seasonal_onset(
    tsd = tsd_data_cases,
    season_start = 21,
    only_current_season = FALSE
  )

  tsd_growth_w_cases <- consecutive_growth_warnings(tsd_onset_cases)

  cases_plot <- plot(tsd_growth_w_cases)

  expect_equal(all(cases_plot$data$incidence), NA)

  ## Incidence
  tsd_data_incidence <- to_time_series(
    time = tsd_data_cases$time,
    cases = tsd_data_cases$cases,
    population = 1e+06
  )

  tsd_onset_incidence <- seasonal_onset(
    tsd = tsd_data_incidence,
    season_start = 21,
    only_current_season = FALSE
  )

  tsd_growth_w_incidence <- consecutive_growth_warnings(tsd_onset_incidence)

  incidence_plot <- plot(tsd_growth_w_incidence)

  expect_false(all(is.na(incidence_plot$data$incidence)))
})
