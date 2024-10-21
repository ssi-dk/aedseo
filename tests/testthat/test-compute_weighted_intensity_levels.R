test_that("Can run the compute_weighted_intensity_levels fun without error", {

  # Weibull fit
  obs <- 10
  season <- c("2018/2019", "2019/2020", "2020/2021")
  season_num_rev <- rev(seq(from = 1, to = length(season)))
  observations <- rep(stats::rnorm(10, obs), length(season))

  peak_input <- tibble::tibble(
    observation = observations,
    weight = 0.8^rep(season_num_rev, each = obs)
  )

  expect_no_error(compute_weighted_intensity_levels(weighted_observations = peak_input))

  # Exp fit
  expect_no_error(compute_weighted_intensity_levels(weighted_observations = peak_input,
                                                    family = "exp",
                                                    optim_method = "Brent",
                                                    lower_optim = 0,
                                                    upper_optim = 1000))
})
