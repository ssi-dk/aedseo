test_that("Test that plot works for cases and incidence for tsd, tsd_onset, tsd_onset_and_burden objects", {
  skip_if_not_installed("withr")
  withr::local_seed(123)
  grDevices::pdf(tempfile(fileext = ".pdf"))
  withr::defer(grDevices::dev.off())

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

  expect_true(all(is.na(cases_plot_2$data$incidence)))

  # tsd_onset_and_burden
  combined_cases <- combined_seasonal_output(tsd_data_cases)

  cases_plot_3 <- plot(combined_cases)

  expect_true(all(is.na(cases_plot_3$data$incidence)))


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
  current_season <- max(combined_proportion$onset_output$season)
  first_current <- which(combined_proportion$onset_output$season == current_season)[1]
  combined_proportion$onset_output$proportion[first_current] <- 0

  proportion_plot_3 <- autoplot(combined_proportion)

  expect_equal(proportion_plot_3$labels$y, "Proportion")
  expect_equal(attr(combined_proportion$burden_output, "burden_outcome"), "proportion")
  expect_false(all(is.na(proportion_plot_3$data$proportion)))
  expect_true(any(proportion_plot_3$data$proportion == 0))
  expect_true(any(vapply(proportion_plot_3$layers, \(layer) inherits(layer$geom, "GeomLine"), logical(1))))
  proportion_y_scale <- proportion_plot_3$scales$get_scales("y")
  expect_equal(proportion_y_scale$trans$name, "identity")
  expect_equal(proportion_y_scale$limits[1], 0)
  built_proportion_plot <- ggplot2::ggplot_build(proportion_plot_3)
  expect_true(any(vapply(
    built_proportion_plot$data,
    \(layer) any(layer$y == 0, na.rm = TRUE),
    logical(1)
  )))
})

test_that("Test that plot works for cases and incidence in `tsd_growth_warning` objects", {
  skip_if_not_installed("withr")
  withr::local_seed(123)
  grDevices::pdf(tempfile(fileext = ".pdf"))
  withr::defer(grDevices::dev.off())

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

  expect_true(all(is.na(cases_plot$data$incidence)))

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

  ## Proportion
  tsd_data_proportion <- generate_seasonal_data(years = 3, samples = 100)
  tsd_onset_proportion <- seasonal_onset(
    tsd = tsd_data_proportion,
    family = "quasibinomial",
    only_current_season = FALSE
  )
  tsd_growth_w_proportion <- consecutive_growth_warnings(tsd_onset_proportion)
  proportion_plot <- autoplot(tsd_growth_w_proportion)

  expect_equal(proportion_plot$scales$get_scales("x")$trans$name, "identity")
})
