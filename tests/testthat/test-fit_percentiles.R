test_that("Test if checkmate checks work", {
  skip_if_not_installed("withr")
  withr::local_seed(42)
  obs <- 10
  season <- c("2018/2019", "2019/2020", "2020/2021")
  season_num_rev <- rev(seq_along(season))
  observations <- rep(stats::rnorm(10, obs), length(season))

  peak_input <- tibble::tibble(
    observation = observations,
    weight = 0.8^rep(season_num_rev, each = obs)
  )

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
