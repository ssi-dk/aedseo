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

  # Count the number of observations
  n <- length(tsd_data$time)

  # Add NA values to observation
  na_count <- 15

  # Randomly select indices to replace with NA
  na_indices <- sample(1:n, na_count, replace = FALSE)

  # Add NA values
  tsd_data$observation[na_indices] <- NA

  # Calculate seasonal_onset with a 3-day window
  tsd_poisson_na <- seasonal_onset(
    tsd = tsd_data,
    k = 5,
    level = 0.95,
    family = "poisson",
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
    if (sum(is.na(obs_iter) | obs_iter == 0) > k * na_fraction_allowed) {
      skipped_window_count <- skipped_window_count + 1
    }
  }

  # Not all will be converged due to NA injections
  expect_false(all(tsd_poisson_na$converged))
  # Count if the skipped windows are = ones in output
  expect_equal(skipped_window_count, sum(tsd_poisson_na$skipped_window))
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
  expect_error(seasonal_onset(tsd_data, k = 1.4, disease_threshold = 1.5))
  expect_error(seasonal_onset(tsd_data, disease_threshold = 1.5))
  expect_error(seasonal_onset(tsd_data, level = 2))
  expect_error(seasonal_onset(tsd_data, na_fraction_allowed = 2))

  # Expect error with random data frame
  r_df <- data.frame(
    observation = c(100, 120, 150, 180, 220, 270),
    time = as.Date(c(
      "2023-01-01",
      "2023-01-02",
      "2023-01-03",
      "2023-01-04",
      "2023-01-05",
      "2023-01-06"
    )),
    time_interval = "day"
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

  # It adds k-1 rows from previous season if available, if not expect 4 less observations
  tsd_seasons <- tsd_data |>
    dplyr::mutate(season = epi_calendar(.data$time))
  tsd_last_season <- tsd_seasons |>
    dplyr::filter(season == current_season) |>
    dplyr::select(-season)

  tsd_na_rows <- seasonal_onset(tsd_last_season, season_start = 21, only_current_season = TRUE)
  expect_length(tsd_na_rows$observation, length(current_onset$observation[-(1:4)]))
})

test_that("Test that adding population works as expected", {
  skip_if_not_installed("withr")
  withr::local_seed(123)
  # Generate seasonal data
  observation <- c(100, 120, 150, 180, 220, 270, 300, 500, 320, 234, 100, 5)
  tsd_data <- to_time_series(
    observation = observation,
    time = seq(as.Date("2020-01-01"), by = "week", length.out = length(observation)),
    time_interval = "week"
  )

  tsd_data_pop <- to_time_series(
    observation = observation,
    time = seq(as.Date("2020-01-01"), by = "week", length.out = length(observation)),
    population = rep(100000, length(observation)),
    time_interval = "week"
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
  expect_false(identical(attr(no_pop, "incidence_rate"), attr(with_pop_stable, "incidence_rate")))

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
    family = stats::binomial,
  ))
})
