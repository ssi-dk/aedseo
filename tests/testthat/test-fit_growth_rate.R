test_that("The growth rate models converge", {
  skip_if_not_installed("withr")
  withr::local_seed(42)
  # Number of random data points to generate
  n <- 1e3

  # The simulated data
  data_poisson <- stats::rpois(n = n, lambda = 5)
  data_nbinom <- stats::rnbinom(n = n, mu = 5, size = 1)

  # Fit with poisson family
  fit_poisson <- fit_growth_rate(
    cases = data_poisson,
    level = 0.95,
    family = "poisson"
  )
  # Fit with quassipoisson family
  fit_quasipoisson <- fit_growth_rate(
    cases = data_nbinom,
    level = 0.95,
    family = "quasipoisson"
  )

  # Check if they all converge
  expect_true(object = fit_poisson$fit$converged)
  expect_true(object = fit_quasipoisson$fit$converged)
})
