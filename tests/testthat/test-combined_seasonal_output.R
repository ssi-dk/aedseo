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
    years = 4,
    start_date = as.Date("2021-05-24")
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

test_that("Test that family and data type arguments works as expected", {
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

  combined_count <- combined_seasonal_output(
    tsd = tsd_data,
    disease_threshold = 0.1
  )
  expect_equal(attr(combined_count$burden_output, "burden_outcome"), "cases")
  expect_equal(combined_count$burden_output$optim$family, "lnorm")
  expect_equal(attr(combined_count$onset_output, "model_outcome"), "cases")
  expect_equal(attr(combined_count$onset_output, "family"), "quasipoisson")

  tsd_binomial <- generate_seasonal_data(
    years = 3,
    mean = 0.3,
    amplitude = 0.2,
    samples = 100
  )

  combined_binomial <- combined_seasonal_output(
    tsd = tsd_binomial,
    disease_threshold = 0.1,
    family = "binomial"
  )
  expect_equal(attr(combined_binomial$burden_output, "burden_outcome"), "proportion")
  expect_equal(combined_binomial$burden_output$optim$family, "beta")
  expect_equal(attr(combined_binomial$onset_output, "model_outcome"), "proportion")
  expect_equal(attr(combined_binomial$onset_output, "family"), "binomial")

  combined_quasibinomial <- combined_seasonal_output(
    tsd = tsd_binomial,
    disease_threshold = 0.1,
    family = "quasibinomial"
  )
  expect_equal(attr(combined_quasibinomial$burden_output, "burden_outcome"), "proportion")
  expect_equal(combined_quasibinomial$burden_output$optim$family, "beta")
  expect_equal(attr(combined_quasibinomial$onset_output, "model_outcome"), "proportion")
  expect_equal(attr(combined_quasibinomial$onset_output, "family"), "quasibinomial")

  tsd_incidence <- to_time_series(
    cases = tsd_binomial$cases,
    population = tsd_binomial$samples,
    time = tsd_binomial$time
  )

  combined_incidence <- combined_seasonal_output(
    tsd = tsd_incidence,
    disease_threshold = 0.1,
    family = "poisson",
    family_quant = "weibull"
  )
  expect_equal(attr(combined_incidence$burden_output, "burden_outcome"), "incidence")
  expect_equal(combined_incidence$burden_output$optim$family, "weibull")
  expect_equal(attr(combined_incidence$onset_output, "model_outcome"), "incidence")
  expect_equal(attr(combined_incidence$onset_output, "family"), "poisson")
})

test_that("Test that multiple waves feature works for only current season", {
  skip_if_not_installed("withr")
  withr::local_seed(123)
  tsd_data <- generate_seasonal_data(
    years = 3,
    start_date = as.Date("2021-01-04"),
    noise_overdispersion = 100,
    phase = 4
  )

  # Settings
  steps_with_decrease <- 2
  level <- "medium"

  mult_waves <- combined_seasonal_output(
    tsd_data,
    disease_threshold = 2,
    only_current_season = TRUE,
    multiple_waves = TRUE,
    burden_level_decrease = level,
    steps_with_decrease = steps_with_decrease
  )

  end_wave <- mult_waves$onset_output |>
    dplyr::filter(wave_ends == TRUE)

  expect_equal(end_wave$decrease_counter, steps_with_decrease)

  expect_equal(
    as.numeric(mult_waves$burden_output$values[level]),
    end_wave$decrease_value
  )
})

test_that("Test that multiple waves feature works for all seasons in tsd", {
  skip_if_not_installed("withr")
  withr::local_seed(123)
  tsd_data <- generate_seasonal_data(
    years = 5,
    start_date = as.Date("2021-01-04"),
    noise_overdispersion = 100,
    phase = 4
  )

  # Settings
  steps_with_decrease <- 2
  level <- "low"

  mult_waves <- combined_seasonal_output(
    tsd_data,
    disease_threshold = 5,
    only_current_season = FALSE,
    multiple_waves = TRUE,
    burden_level_decrease = level,
    steps_with_decrease = steps_with_decrease
  )

  end_wave <- mult_waves$onset_output |>
    dplyr::filter(wave_ends == TRUE)

  expect_equal(unique(end_wave$decrease_counter), steps_with_decrease)

  burden_levels <- purrr::map_dfr(mult_waves$burden_output, ~ {
    tibble::tibble(
      season = .x$season,
      values = .x$values
    ) |>
      tidyr::unnest_longer(
        col = values,
        indices_to = "decrease_level",
        values_to = "decrease_value"
      )
  }) |>
    dplyr::filter(decrease_level == level)

  compare_burden_values <- end_wave |>
    dplyr::left_join(burden_levels, by = "season")

  expect_equal(
    compare_burden_values$decrease_value.x,
    compare_burden_values$decrease_value.y
  )
})

test_that("Test that seasonal end feature works as expected", {
  skip_if_not_installed("withr")
  withr::local_seed(123)
  tsd_data <- generate_seasonal_data(
    years = 5,
    start_date = as.Date("2021-01-04"),
    noise_overdispersion = 10,
    trend_rate = 1.001,
    phase = 4
  )

  # Settings
  steps_with_decrease <- 2
  level <- "medium"

  seasonal_end <- combined_seasonal_output(
    tsd_data,
    disease_threshold = 10,
    only_current_season = TRUE,
    burden_level_decrease = level,
    steps_with_decrease = steps_with_decrease
  )

  row_nr <- seasonal_end$onset_output |>
    dplyr::mutate(row = dplyr::row_number()) |>
    dplyr::filter(seasonal_offset == "TRUE") |>
    dplyr::select(row) |>
    dplyr::pull()

  # Expect one seasonal end value
  expect_length(row_nr, 1)

  # Expect two steps with decrease
  end_row <- seasonal_end$onset_output[row_nr, ]$cases
  expect_lt(end_row, seasonal_end$onset_output[row_nr - 1, ]$cases)
  expect_lt(end_row, seasonal_end$onset_output[row_nr - 2, ]$cases)

  mult_waves <- combined_seasonal_output(
    tsd_data,
    disease_threshold = 10,
    only_current_season = TRUE,
    burden_level_decrease = level,
    steps_with_decrease = steps_with_decrease,
    multiple_waves = TRUE
  )

  # Expect first wave equal to seasonal end
  row_nr_mult <- mult_waves$onset_output |>
    dplyr::mutate(row = dplyr::row_number()) |>
    dplyr::filter(wave_ends == "TRUE") |>
    dplyr::filter(dplyr::row_number() == 1) |>
    dplyr::select(row) |>
    dplyr::pull()

  expect_equal(row_nr, row_nr_mult)

  # Expect new attributes
  expect_match(attributes(seasonal_end)$burden_level_decrease, "medium")
  expect_equal(attributes(seasonal_end)$steps_with_decrease, 2)
  expect_equal(attributes(seasonal_end)$multiple_waves, FALSE)

  expect_equal(attributes(mult_waves)$multiple_waves, TRUE)
})

test_that("Test that seasonal end feature works as expected when there are multiple waves", {
  set.seed(123)
  tsd_data_monthly <- generate_seasonal_data(
    years = 14,
    phase = 3,
    start_date = as.Date("2020-05-18"),
    noise_overdispersion = 5,
    time_interval = "months"
  )

  tsd_data <- to_time_series(
    cases = tsd_data_monthly$cases,
    time = seq.Date(
      from = as.Date("2020-05-18"),
      by = "week",
      length.out = length(tsd_data_monthly$cases)
    )
  ) |>
    dplyr::filter(time < as.Date("2023-05-22"))

  # Settings
  steps_with_decrease <- 2
  level <- "low"

  seasonal_end <- combined_seasonal_output(
    tsd_data,
    disease_threshold = 10,
    only_current_season = TRUE,
    burden_level_decrease = level,
    steps_with_decrease = steps_with_decrease
  )

  mult_waves <- combined_seasonal_output(
    tsd_data,
    disease_threshold = 10,
    only_current_season = TRUE,
    burden_level_decrease = level,
    steps_with_decrease = steps_with_decrease,
    multiple_waves = TRUE
  )

  n_ends_single <- seasonal_end$onset_output |>
    dplyr::filter(seasonal_offset == "TRUE") |>
    dplyr::count() |>
    dplyr::pull()

  n_ends_mult <- mult_waves$onset_output |>
    dplyr::filter(wave_ends == "TRUE") |>
    dplyr::count() |>
    dplyr::pull()

  # Expect more ends in multiple_waves
  expect_gt(n_ends_mult, n_ends_single)

  row_nr <- seasonal_end$onset_output |>
    dplyr::mutate(row = dplyr::row_number()) |>
    dplyr::filter(seasonal_offset == "TRUE") |>
    dplyr::select(row) |>
    dplyr::pull()

  # Expect first wave equal to seasonal end
  row_nr_mult <- mult_waves$onset_output |>
    dplyr::mutate(row = dplyr::row_number()) |>
    dplyr::filter(wave_ends == "TRUE") |>
    dplyr::filter(dplyr::row_number() == 1) |>
    dplyr::select(row) |>
    dplyr::pull()

  expect_equal(row_nr, row_nr_mult)
})
