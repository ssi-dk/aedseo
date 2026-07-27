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




  ## Proportion

  tsd_data_proportion <- generate_seasonal_data(
    years = 3,
    mean = 0.3,
    amplitude = 0.2,
    samples = 100
  )

  proportion_plot_1 <- autoplot(tsd_data_proportion)

  expect_equal(proportion_plot_1$labels$y, "Proportion")
  expect_silent(ggplot2::ggplot_build(proportion_plot_1))

  tsd_onset_proportion <- seasonal_onset(
    tsd = tsd_data_proportion,
    family = "quasibinomial"
  )

  proportion_plot_2 <- autoplot(tsd_onset_proportion)

  expect_equal(proportion_plot_2$observed$labels$y, "Proportion")
  expect_silent(ggplot2::ggplot_build(proportion_plot_2$observed))

  combined_proportion <- combined_seasonal_output(
    tsd_data_proportion,
    disease_threshold = 0.1,
    family = "quasibinomial",
    family_quant = "beta"
  )

  proportion_plot_3 <- autoplot(combined_proportion, y_lower_bound = 0.01)

  expect_equal(proportion_plot_3$labels$y, "Proportion")
  expect_silent(ggplot2::ggplot_build(proportion_plot_3))
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
