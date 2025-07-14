test_that("Test that input argument checks work", {
  skip_if_not_installed("withr")
  withr::local_seed(123)
  # Generate seasonal data
  tsd_data <- generate_seasonal_data(years = 3, start_date = as.Date("2021-01-04"))

  expect_error(
    checkmate_err_msg(seasonal_burden_levels(tsd_data, family = "lnorm", decay_factor = 3),
      "Variable 'decay_factor': Element 1 is not <= 1."
    )
  )

  expect_error(
    checkmate_err_msg(seasonal_burden_levels(tsd_data, family = "lnorm", n_peak = c(1, 2)),
      "Variable 'n_peak': Must have length 1, but has length 2."
    )
  )

  expect_error(
    checkmate_err_msg(seasonal_burden_levels(tsd_data, family = "lnorm", disease_threshold = "hey"),
      "'disease_threshold': Must be of type 'integerish', not 'character'."
    )
  )

  expect_error(
    checkmate_err_msg(seasonal_burden_levels(tsd_data, family = "lnorm", season_start = 2, season_end = 10),
      "`start` must be greater than `end`!"
    )
  )

  expect_error(
    checkmate_err_msg(seasonal_burden_levels(tsd_data, family = "lnorm", method = "peak_levels"),
      "Variable 'conf_levels': Must have length 3, but has length 1."
    )
  )

  expect_error(
    checkmate_err_msg(seasonal_burden_levels(tsd_data, family = "lnorm", conf_levels = c(0.2, 0.5)),
      "Variable 'conf_levels': Must have length 1, but has length 2."
    )
  )

  tsd_fail <- tsd_data |> dplyr::rename(week = time)

  expect_error(
    checkmate_err_msg(seasonal_burden_levels(tsd_fail),
      paste("Variable 'colnames(tsd)': Names must be a identical to set {'time','observation'},
          but is {'week','observation'}."),
      fixed = TRUE
    )
  )

  #Finally check that dots arguments work
  model_output <- seasonal_burden_levels(
    tsd_data,
    family = "exp",
    optim_method = "Brent",
    lower_optim = 1, upper_optim = 1000
  )
  expect_equal(model_output$optim$family, "exp")
})

test_that("Test that we get correct season output for newest season", {
  skip_if_not_installed("withr")
  withr::local_seed(123)
  # Generate seasonal data
  tsd_data <- generate_seasonal_data(years = 3, start_date = as.Date("2021-01-04"))

  seasonal_tsd <- tsd_data |>
    dplyr::mutate(season = epi_calendar(.data$time, start = 21, end = 20))

  newest_season <- max(seasonal_tsd$season)

  intensity_levels <- seasonal_burden_levels(
    tsd_data, family = "lnorm",
    method = "intensity_levels"
  )

  expect_equal(newest_season, intensity_levels$season)
})

test_that("Test that we have same numbers of outputs for both methods", {
  skip_if_not_installed("withr")
  withr::local_seed(123)
  # Generate seasonal data
  tsd_data <- generate_seasonal_data(years = 3, start_date = as.Date("2021-01-04"))

  intensity_levels <- seasonal_burden_levels(
    tsd_data, family = "lnorm",
    method = "intensity_levels"
  )

  peak_levels <- seasonal_burden_levels(
    tsd_data,
    family = "lnorm",
    method = "peak_levels",
    conf_levels = c(0.4, 0.9, 0.99)
  )

  expect_equal(length(intensity_levels), length(peak_levels))
})

test_that("Test that function fail with less than two seasons", {
  skip_if_not_installed("withr")
  withr::local_seed(123)
  # Generate seasonal data
  tsd_one_season <- generate_seasonal_data(
    years = 1,
    start_date = as.Date("2021-05-24")
  )

  expect_error(
    checkmate_err_msg(seasonal_burden_levels(tsd_one_season, family = "lnorm"),
      "There must be at least two unique seasons in the data."
    )
  )
})

test_that("Test that selection of current and all seasons work as expected", {
  skip_if_not_installed("withr")
  withr::local_seed(123)
  # Generate seasonal data
  tsd_data <- generate_seasonal_data(years = 3, start_date = as.Date("2021-01-04"))

  current_season <- epi_calendar(dplyr::last(tsd_data$time))

  current_level <- seasonal_burden_levels(
    tsd_data, family = "lnorm",
    only_current_season = TRUE
  )
  all_levels <- seasonal_burden_levels(
    tsd_data, family = "lnorm",
    only_current_season = FALSE
  )

  expect_equal(current_season, unique(current_level$season))
  expect_gt(length(all_levels), 1)
})


test_that("Test that function does not fail if there are no observations surpassing the disease specific threshold
          in the current season.", {
    skip_if_not_installed("withr")
    withr::local_seed(123)
    # Generate seasonal data
    tsd_data_one <- generate_seasonal_data(
      years = 1,
      start_date = as.Date("2022-05-23")
    )
    tsd_data_two <- generate_seasonal_data(
      years = 1, amplitude = 10,
      start_date = as.Date("2023-05-22")
    )
    tsd_data_combined <- dplyr::bind_rows(tsd_data_one, tsd_data_two)

    burden_list <- seasonal_burden_levels(
      tsd_data_combined,
      disease_threshold = 150
    )

    expect_equal(burden_list$season, "2023/2024")
  }
)

test_that("Test that seasons are correctly increasing when only_current_season = FALSE", {
  skip_if_not_installed("withr")
  withr::local_seed(123)
  # Generate seasonal data
  tsd_data <- generate_seasonal_data(
    years = 3,
    start_date = as.Date("2021-01-04")
  )

  unique_seasons <- tsd_data |>
    dplyr::mutate(season = epi_calendar(time)) |>
    dplyr::pull(season) |>
    base::unique()

  all_levels <- seasonal_burden_levels(
    tsd_data, family = "lnorm",
    only_current_season = FALSE
  )

  level_seasons <- sapply(all_levels, function(x) x$season)

  expect_equal(unique_seasons[2:4], level_seasons)
})

test_that("Test that when no obs above disease_threshold return NA and warning", {
  skip_if_not_installed("withr")
  withr::local_seed(123)
  # Generate seasonal data
  tsd_data <- generate_seasonal_data(
    years = 3,
  )

  expect_warning(
    obs_under_dt <- seasonal_burden_levels(
      tsd_data,
      disease_threshold = max(tsd_data$cases) + 50
    ),
    "There are no observations above `disease_threshold`. Returning NA values."
  )

  expect_equal(
    unname(obs_under_dt$values),
    c(max(tsd_data$cases) + 50, rep(NA, 3))
  )
})

test_that("Test that when only current season has an obs above disease_threshold return NA and warning", {
  skip_if_not_installed("withr")
  withr::local_seed(123)
  # Generate seasonal data
  tsd_data <- generate_seasonal_data(
    years = 2,
    trend_rate = 1.1
  )

  max_obs <- tsd_data |>
    dplyr::mutate(season = epi_calendar(time)) |>
    dplyr::filter(season == dplyr::first(season)) |>
    dplyr::slice_max(cases, n = 1) |>
    dplyr::pull(cases)

  expect_warning(
    obs_under_dt <- seasonal_burden_levels(
      tsd_data,
      disease_threshold = max_obs + 50
    ),
    "There are no observations above `disease_threshold`. Returning NA values."
  )

  expect_equal(
    unname(obs_under_dt$values),
    c(max_obs + 50, rep(NA, 3))
  )
})

test_that("Test that only_current_season = FALSE/TRUE estimate same results", {
  skip_if_not_installed("withr")
  withr::local_seed(123)
  # Generate seasonal data
  tsd_data <- generate_seasonal_data(years = 4, start_date = as.Date("2021-01-04"))

  current_level <- seasonal_burden_levels(
    tsd_data, family = "lnorm",
    only_current_season = TRUE
  )
  all_levels <- seasonal_burden_levels(
    tsd_data, family = "lnorm",
    only_current_season = FALSE
  )

  expect_equal(current_level$values, all_levels[[4]]$values)
})

test_that("Test that levels are always included in output", {
  skip_if_not_installed("withr")
  withr::local_seed(123)
  # Generate seasonal data
  tsd_data <- generate_seasonal_data(
    years = 6,
    start_date = as.Date("2021-01-04"),
    trend_rate = 1.02,
    relative_epidemic_concentration = 2
  ) |>
    dplyr::mutate(season = epi_calendar(time))

  nr_seasons <- dplyr::n_distinct(tsd_data$season)

  new_tsd <- tsd_data |>
    dplyr::mutate(cases = dplyr::if_else(season == "2022/2023", 1, cases)
    ) |>
    dplyr::select(-season)

  current_level <- seasonal_burden_levels(
    tsd = new_tsd,
    only_current_season = FALSE
  )

  expect_equal(length(current_level), nr_seasons - 1) # exclude first season
  expect_equal(current_level[[2]]$values, current_level[[3]]$values)
})

test_that("Convert to incidence work as expected", {
  skip_if_not_installed("withr")
  withr::local_seed(123)

  # Generate seasonal data
  tsd_data <- generate_seasonal_data(
    years = 1,
    start_date = as.Date("2021-01-04"),
    amplitude = 10000
  )
  tsd_data_inc <- tsd_data |>
    dplyr::mutate(population = 100000,
                  incidence = (cases / population) * 100)
  attr(tsd_data_inc, "incidence_denominator") <- 100

  with_inc <- seasonal_burden_levels(
    tsd = tsd_data_inc,
    disease_threshold = 5
  )

  no_inc <- seasonal_burden_levels(
    tsd = tsd_data,
    disease_threshold = (5 / 100) * 100000
  )

  expect_gt(no_inc$values["medium"], with_inc$values["medium"])
  expect_equal(round(((with_inc$values / 100) * 100000))["medium"], round(no_inc$values["medium"]))
})
