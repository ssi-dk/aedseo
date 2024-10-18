#' Fit a peak algorithm to time series observations in a set of seasons.
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This function calculates the intensity levels of peak time series
#' observations to the next comming season based on previouse seasons.
#' Peak observations are the n highest observations from each season.
#' It provides low, medium, high intensity levels, that reflect the
#' current intensity of infection based on previous seasons. Hence it
#' uses the peak observations from each previous season it can only
#' explain the current intensity in relation to the peak of the season.
#'
#' @param weighted_observations A tibble containing three columns of size n;
#' `observation`, which represents the data points, and `weight`, which is the
#' importance assigned to an observation. Higher weights indicate that an
#' observation has more influence on the model outcome, while lower weights
#' reduce its impact. The structure is:
#' `weighted_observations = tibble(observation = c(...), weight = c(...))`
#' @param conf_levels The confidence levels for parameter estimates, a numeric
#' vector of length 3. Default is c(0.50, 0.90, 0.95).
#' @param family A character string specifying the family for
#' modeling. Choose between "weibull", "lnorm" or "exp".
#' @param optim_method A character string specifying the method to be used in
#' the optimisation. Lookup ?optim::stats for details about methods.
#' Default is Nelder-Mead. If using the exp family it is recomended to use
#' Brent as it is a one-dimensional optimisation.
#' @param lower_optim A numeric value for the optimisation. Default is -Inf.
#' @param upper_optim A numeric value for the optimisation. Default is Inf.
#'
#' @return A tibble containing:
#'   - 'high_level': The highest peak intensity level
#'   - 'medium_level': The medium peak intensity level
#'   - 'low_level': The lowest intensity level
#'   - 'optim_fit_par_1': The first fit parameter for the chosen family.
#'       - For 'weibull': Shape parameter (k).
#'       - For 'lnorm': Mean of the log-transformed observations.
#'       - For 'exp': Rate parameter (λ).
#'   - 'optim_fit_par_2': The second fit parameter for the chosen family.
#'      - For 'weibull': Scale parameter (λ).
#'      - For 'lnorm': Standard deviation of the log-transformed observations.
#'      - For 'exp': Not applicable (set to NA).
#'   - 'obj_value': The value of the objective function
#'     (negative log-likelihood), which represent the minimized objective
#'      function value from the optimisation. Smaller value equals better
#'      optimisation.
#'   - 'converged': Logical. TRUE if the optimisation converged.
#'   - 'family': The distribution family used for the optimization.
#'      - 'weibull': Uses the Weibull distribution for fitting.
#'      - 'lnorm': Uses the Log-normal distribution for fitting.
#'      - 'exp': Uses the Exponential distribution for fitting.
#'
#' @export
#'
#' @examples
#' # Create three seasons with random observations
#' obs = 10
#' season = c("2018/2019", "2019/2020", "2020/2021")
#' season_num_rev <- rev(seq(from = 1, to = length(season)))
#' observations = rep(rnorm(10,obs), length(season))
#'
#' # Add into a tibble with weight decreasing going one season step back
#' peak_input <- tibble(
#'   observation = observations,
#'   weight = 0.8^rep(season_num_rev, each = obs),
#'   season = season[rep(seq(from = 1, to = length(season)), each = obs)]
#' )
#'
#' # Use the peak model
#' fit_peak(weighted_observations = peak_input,
#'          conf_levels = c(0.50, 0.90, 0.95),
#'          family = "weibull")

fit_peak <- function(
  weighted_observations,
  conf_levels = c(0.50, 0.90, 0.95),
  family = c("weibull",
             "lnorm",
             "exp"),
  optim_method = c("Nelder-Mead",
                   "BFGS",
                   "CG",
                   "L-BFGS-B",
                   "SANN",
                   "Brent"),
  lower_optim = -Inf,
  upper_optim = Inf
) {
  # Match the arguements.
  family <- rlang::arg_match(family)
  optim_method <- rlang::arg_match(optim_method)

  # Initialising parameters based on family
  init_par_fun <- function(family, observations) {
    init_params <- switch(family,
      weibull = log(c(1.5, mean(observations))),
      lnorm = c(mean(log(observations)),
                sd(log(observations))),
      exp = log(1.5)
    )
    return(init_params)
  }

  # The weighted negative loglikelihood function
  nll <- function(par, weighted_observations, family = "weibull") {
    switch(family,
      weibull = -sum(dweibull(weighted_observations$observation,
                              shape = exp(par[1]), scale = exp(par[2]),
                              log = TRUE) * weighted_observations$weight),
      lnorm = -sum(dlnorm(weighted_observations$observation,
                          meanlog =  par[1], sdlog = par[2],
                          log = TRUE) * weighted_observations$weight),
      exp =  -sum(dexp(weighted_observations$observation, rate = exp(par[1]),
                       log = TRUE) * weighted_observations$weight)
    )
  }

  # Run optimisation for weighted observations
  optim_obj <- optim(par = init_par_fun(family = family,
                                        observations =
                                          weighted_observations$observation),
                     fn = nll,
                     weighted_observations = weighted_observations,
                     family = family,
                     method = optim_method,
                     lower = lower_optim,
                     upper = upper_optim)

  # Back-transform optimized parameters to their original scale if needed.
  # This is done by exponentiating the parameters, as they were
  # log-transformed during the optimisation process
  par_fit <- switch(family,
    weibull = exp(optim_obj$par),
    lnorm = optim_obj$par,
    exp = c(exp(optim_obj$par), NA)
  )

  # Calculate the low, medium, high intensity levels based on input `conf_level`
  quantiles <- switch(family,
    weibull = stats::qweibull(p = c(0.50, 0.90, 0.95),
                              shape = par_fit[1],
                              scale = par_fit[2]),
    lnorm = stats::qlnorm(p = c(0.50, 0.90, 0.95),
                          meanlog = par_fit[1],
                          sdlog = par_fit[2]),
    exp = stats::qexp(p = c(0.50, 0.90, 0.95),
                      rate = par_fit[1])
  )

  # Create a tibble with the fit parameters
  optim_results <- dplyr::tibble(
    high_level = quantiles[3],
    medium_level = quantiles[2],
    low_level = quantiles[1],
    optim_fit_par_1 = par_fit[1],
    optim_fit_par_2 = ifelse(length(par_fit) == 2, par_fit[2], NA),
    obj_value = optim_obj$value,
    converged = ifelse(optim_obj$convergence == 0, TRUE, FALSE),
    family = family
  )

  return(optim_results)
}
