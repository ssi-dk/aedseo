test_that("Test if checkmate checks work", {
  skip_if_not_installed("withr")
  withr::local_seed(42)
  peak_input <- generate_seasonal_data(years = 5, noise_overdispersion = 5, trend_rate = 1.005) |>
    dplyr::mutate(season = epi_calendar(time)) |>
    dplyr::group_by(season) |>
    dplyr::slice_max(n = 6, order_by = observation, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::mutate(season_count = rev(dplyr::dense_rank(season)) - 1,
                  weight = 0.8^season_count) |>
    dplyr::select(observation, weight)

  fit_percentiles(weighted_observations = peak_input, conf_levels = c(0.1, 0.2, 0.4, 0.8, 0.9))

  # Exp fit
  expect_no_error(fit_percentiles(weighted_observations = peak_input,
                                  family = "exp",
                                  optim_method = "Brent",
                                  lower_optim = 0,
                                  upper_optim = 1000))
  # lnorm fit
  expect_no_error(fit_percentiles(weighted_observations = peak_input,
                                  family = "lnorm"))

  expect_error(
    checkmate_err_msg(
      fit_percentiles(weighted_observations = peak_input, conf_levels = c(0.2, 0.9, 0.9)),
      "Variable 'conf_levels': Contains duplicated values, position 3."
    )
  )

  expect_error(
    checkmate_err_msg(
      fit_percentiles(weighted_observations = peak_input, conf_levels = c(0.9, 0.7, 0.2)),
      "Variable 'conf_levels': Must be sorted."
    )
  )
})

test_that("Test that changing weights work", {
  skip_if_not_installed("withr")
  withr::local_seed(123)

  generate_data <- generate_seasonal_data(years = 5, noise_overdispersion = 5, trend_rate = 1.005) |>
    dplyr::mutate(season = epi_calendar(time)) |>
    dplyr::group_by(season) |>
    dplyr::slice_max(n = 6, order_by = observation, with_ties = FALSE) |>
    dplyr::ungroup()

  peak_input <- generate_data |>
    dplyr::mutate(season_count = rev(dplyr::dense_rank(season)) - 1,
                  weight = 0.8^season_count) |>
    dplyr::select(observation, weight)

  peak_input_2 <- generate_data |>
    dplyr::mutate(season_count = rev(dplyr::dense_rank(season)) - 1,
                  weight = 0.5^season_count) |>
    dplyr::select(observation, weight)

  small_diff <- fit_percentiles(peak_input)
  big_diff <- fit_percentiles(peak_input_2)

  expect_gt(big_diff$values[[3]], small_diff$values[[3]])
})

test_that("Test that when there is only one unique observation return NA and warning", {
  skip_if_not_installed("withr")
  withr::local_seed(123)
  # Generate observations
  unique_data <- tibble::tibble(
    observation = c(100, 100),
    weight = c(1, 1)
  )

  expect_warning(
    one_unique_res <- fit_percentiles(
      weighted_observations = unique_data
    ),
    "Cannot optimise, there is only one unique observation. Returning NA values."
  )

  expect_equal(
    one_unique_res$values,
    rep(NA, 3)
  )
})
