test_that("The growth rate models converge", {
  skip_if_not_installed("withr")
  withr::local_seed(123)
  # Generate seasonal data
  tsd_data <- generate_seasonal_data(
    years = 1,
    start_date = as.Date("2021-01-01"),
    mean = 200
  )

  # Calculate seasonal_onset with a 3-day window
  tsd_poisson <- seasonal_onset(
    tsd = tsd_data,
    k = 3,
    level = 0.95,
    family = "poisson",
    disease_threshold = 20,
    na_fraction_allowed = 0.2
  )
  tsd_quasipoisson <- seasonal_onset(
    tsd = tsd_data,
    k = 3,
    level = 0.95,
    family = "quasipoisson",
    disease_threshold = 20,
    na_fraction_allowed = 0.2
  )

  # Check if they all converge
  expect_true(object = all(tsd_poisson$converged))
  expect_true(object = all(tsd_quasipoisson$converged))
})

test_that("Test if it works with weeks with NA values", {
  skip_if_not_installed("withr")
  withr::local_seed(123)
  # Generate seasonal data
  tsd_data <- generate_seasonal_data(
    years = 1,
    start_date = as.Date("2021-01-01")
  )

  # Count the number of cases
  n <- length(tsd_data$time)

  # Add NA values to cases
  na_count <- 15

  # Randomly select indices to replace with NA
  na_indices <- sample(1:n, na_count, replace = FALSE)

  # Add NA values
  tsd_data$cases[na_indices] <- NA

  # Calculate seasonal_onset with a 3-day window
  tsd_na <- seasonal_onset(
    tsd = tsd_data,
    k = 5,
    level = 0.95,
    disease_threshold = 20,
    na_fraction_allowed = 0.4
  )

  # Test if correct amount of windows with NA are skipped
  k <- 5
  na_fraction_allowed <- 0.4
  n <- base::nrow(tsd_data)
  skipped_window_count <- 0

  for (i in k:n) {
    obs_iter <- tsd_data[(i - k + 1):i, ]
    if (sum(is.na(obs_iter$cases) | obs_iter$cases == 0) > k * na_fraction_allowed) {
      skipped_window_count <- skipped_window_count + 1
    }
  }

  # Not all will be converged due to NA injections
  expect_false(all(tsd_na$converged))
  # Count if the skipped windows are = ones in output
  expect_equal(skipped_window_count, sum(tsd_na$skipped_window))
})

test_that("Test that input argument checks work", {
  skip_if_not_installed("withr")
  withr::local_seed(123)
  # Generate seasonal data
  tsd_data <- generate_seasonal_data(
    years = 1,
    start_date = as.Date("2023-01-01")
  )

  expect_no_error(seasonal_onset(tsd_data))

  # Expect error when not matching family
  expect_error(seasonal_onset(tsd_data, family = "ttt"))

  # Expect errors from wrong input arguments
  expect_error(seasonal_onset(tsd_data, k = 1.4))
  expect_error(seasonal_onset(tsd_data, level = 2))
  expect_error(seasonal_onset(tsd_data, na_fraction_allowed = 2))

  # Expect error with random data frame
  r_df <- data.frame(
    cases = c(100, 120, 150, 180, 220, 270),
    time = as.Date(c(
      "2023-01-01",
      "2023-01-02",
      "2023-01-03",
      "2023-01-04",
      "2023-01-05",
      "2023-01-06"
    )),
    time_interval = "days"
  )

  expect_error(seasonal_onset(r_df))

  # Expect error with wrong column names
  colnames(tsd_data) <- c("hey", "test")
  expect_error(seasonal_onset(tsd_data))
})

test_that("Test that selection of current and all seasons work as expected", {
  skip_if_not_installed("withr")
  withr::local_seed(123)
  # Generate seasonal data
  tsd_data <- generate_seasonal_data(
    years = 3,
    start_date = as.Date("2021-01-04")
  )

  current_season <- epi_calendar(dplyr::last(tsd_data$time))

  current_onset <- seasonal_onset(tsd_data, season_start = 21, only_current_season = TRUE)
  all_onsets <- seasonal_onset(tsd_data, season_start = 21, only_current_season = FALSE)

  # It actually returns one season or all seasons
  expect_equal(current_season, unique(current_onset$season))
  expect_gt(length(unique(all_onsets$season)), 1)

  # It adds k-1 rows from previous season if available, if not expect 4 less cases
  tsd_seasons <- tsd_data |>
    dplyr::mutate(season = epi_calendar(.data$time))
  tsd_last_season <- tsd_seasons |>
    dplyr::filter(season == current_season) |>
    dplyr::select(-season)

  tsd_na_rows <- seasonal_onset(tsd_last_season, season_start = 21, only_current_season = TRUE)
  expect_length(tsd_na_rows$cases, length(current_onset$cases[-(1:4)]))
})

test_that("Test that adding population works as expected", {
  skip_if_not_installed("withr")
  withr::local_seed(123)
  # Generate seasonal data
  cases <- c(100, 120, 150, 180, 220, 270, 300, 500, 320, 234, 100, 5)
  tsd_data <- to_time_series(
    cases = cases,
    time = seq(as.Date("2020-01-01"), by = "week", length.out = length(cases))
  )

  tsd_data_pop <- to_time_series(
    cases = cases,
    time = seq(as.Date("2020-01-01"), by = "week", length.out = length(cases)),
    population = rep(100000, length(cases))
  )

  # Calculate growth rates with stable population - should be identical
  no_pop <- seasonal_onset(
    tsd = tsd_data,
    k = 3
  )

  with_pop_stable <- seasonal_onset(
    tsd = tsd_data_pop,
    k = 3
  )

  with_pop_stable <- with_pop_stable |>
    dplyr::select(-c("population", "incidence"))

  no_pop <- no_pop |>
    dplyr::select(-c("population", "incidence"))

  expect_equal(no_pop, with_pop_stable, ignore_attr = TRUE)
  expect_false(identical(attr(no_pop, "incidence_denominator"), attr(with_pop_stable, "incidence_denominator")))

  # Change population size during period
  with_pop <- seasonal_onset(
    tsd = tsd_data_pop |>
      dplyr::mutate(population = population + seq(from = 1000, by = 100, length.out = dplyr::n())),
    k = 3
  )

  with_pop <- with_pop |>
    dplyr::select(-c("population", "incidence"))

  expect_false(isTRUE(all.equal(no_pop, with_pop_stable, ignore_attr = TRUE)))
})

test_that("family works the same via name, generator or object", {
  skip_if_not_installed("withr")
  withr::local_seed(123)
  # Generate seasonal data
  tsd_data <- generate_seasonal_data(
    years = 3,
    start_date = as.Date("2021-01-04")
  )

  # Apply methods
  fam_inputs <- list(
    character = "poisson",
    generator = stats::poisson,
    object = stats::poisson(),
    object_with_link = stats::poisson(link = "log")
  )

  # Run seasonal_onset on all methods
  onset_outputs <- lapply(fam_inputs, function(fam) {
    seasonal_onset(tsd = tsd_data, family = fam)
  })

  # Check all results are equal
  purrr::walk(
    onset_outputs[-1],
    ~ expect_equal(.x, (onset_outputs[[1]]), ignore_attr = TRUE)
  )

  expect_error(seasonal_onset(
    tsd = tsd_data,
    family = 4,
  ))

  expect_error(seasonal_onset(
    tsd = tsd_data,
    family = "hello",
  ))

  expect_error(seasonal_onset(
    tsd = tsd_data,
    family = 4,
  ))
})

test_that("binomial onset output keeps one row per fitted window", {
  skip_if_not_installed("withr")
  withr::local_seed(123)
  tsd_data <- generate_seasonal_data(
    years = 1,
    mean = 0.3,
    amplitude = 0.2,
    samples = 100
  )

  onset_output <- seasonal_onset(tsd = tsd_data, family = "binomial")

  expected_rows <- nrow(tsd_data) - attr(onset_output, "k") + 1
  expected_indices <- attr(onset_output, "k"):nrow(tsd_data)
  expect_equal(nrow(onset_output), expected_rows)
  expect_equal(onset_output$proportion, tsd_data$proportion[expected_indices])
  expect_equal(onset_output$samples, tsd_data$samples[expected_indices])
})

test_that("Test that seasonal onset correctly creates NA for significant growth in output", {
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
    season_end = 20,
    only_current_season = FALSE,
    disease_threshold = NA_real_
  )

  expect_true(all(is.na(onset_data$upper_growth_rate)))
})


test_that("Average observations in window are correctly calculated", {
  skip_if_not_installed("withr")
  withr::local_seed(123)

  # Generate seasonal data
  tsd_data <- generate_seasonal_data(
    years = 1,
    start_date = as.Date("2021-01-04"),
    amplitude = 10000
  )

  tsd_data_inc <-
    to_time_series(time = tsd_data$time,
                   cases = tsd_data$cases,
                   population = 1e5,
                   incidence_denominator = 100)

  onset_output_inc <- seasonal_onset(
    tsd = tsd_data_inc
  )

  # Use k = 5
  tsd_data_inc_avg_obs <- tsd_data_inc |>
    dplyr::mutate(
      inc_rolling_avg = pracma::movavg(incidence, n = 5)
    ) |>
    dplyr::slice_max(order_by = time, n = (nrow(tsd_data) - 4)) |>
    dplyr::arrange(time) |>
    dplyr::pull(inc_rolling_avg)

  average_observations_window_output <- onset_output_inc$average_observations_window

  purrr::walk2(average_observations_window_output, tsd_data_inc_avg_obs, ~ expect_equal(.x, .y))
})

test_that("test that incidence disease threshold is used correctly in seasonal onset", {
  skip_if_not_installed("withr")
  withr::local_seed(111)
  tsd_data <- generate_seasonal_data(
    years = 6,
    start_date = as.Date("2021-01-01"),
    noise_overdispersion = 3,
    phase = 2
  )

  tsd_data_pop <- to_time_series(
    cases = tsd_data$cases,
    population = 10000000,
    incidence_denominator = 1000,
    time = tsd_data$time
  )

  disease_threshold <- estimate_disease_threshold(
    tsd_data_pop,
    family = "poisson",
    burden_family = "weibull",
    use_prev_seasons_num = 5,
    skip_current_season = FALSE
  )

  onset_data <- seasonal_onset(
    tsd = tsd_data_pop,
    disease_threshold = disease_threshold$disease_threshold,
    season_start = 21,
    only_current_season = TRUE
  )

  expect_equal(attr(onset_data, "model_outcome"), "incidence")
  expect_lt(disease_threshold$disease_threshold, 1)
})

test_that("test that seasonal onset works with disease threshold set to 0", {
  skip_if_not_installed("withr")
  withr::local_seed(111)
  tsd_data <- generate_seasonal_data()

  expect_no_message(seasonal_onset(
    tsd = tsd_data,
    disease_threshold = 0,
    season_start = 21,
    only_current_season = TRUE
  ))
})
