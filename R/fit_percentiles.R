#' Fits weighted observations to distribution and returns percentiles
#'
#' @description
#'
#' This function estimates the percentiles of weighted time series observations. The output contains the percentiles
#' from the fitted distribution.
#'
#' @param weighted_observations A tibble containing two columns of length n; `observation`, which contains the data
#' points, and `weight`, which is the importance assigned to the observation. Higher weights indicate that an
#' observation has more influence on the model outcome, while lower weights reduce its impact.
#' @param conf_levels A numeric vector specifying the confidence levels for parameter estimates. The values have
#' to be unique and in ascending order, that is the lowest level is first and highest level is last.
#' @param family `r rd_family()`
#' @param optim_method A character string specifying the method to be used in the optimisation. Lookup `?optim::stats`
#' for details about methods.
#' If using the exp family it is recommended to use Brent as it is a one-dimensional optimisation.
#' @param lower_optim A numeric value for the optimisation.
#' @param upper_optim A numeric value for the optimisation.
#'
#' @return A list containing:
#'   - 'conf_levels': The conf_levels chosen to fit the percentiles.
#'   - 'percentiles': The percentile results from the fit.
#'   - 'par': The fit parameters for the chosen family.
#'       - par_1:
#'          - For 'weibull': Shape parameter (k).
#'          - For 'lnorm': Mean of the log-transformed observations.
#'          - For 'exp': Rate parameter (λ).
#'       - 'par_2':
#'          - For 'weibull': Scale parameter (λ).
#'          - For 'lnorm': Standard deviation of the log-transformed observations.
#'          - For 'exp': Not applicable (set to NA).
#'   - 'obj_value': The value of the objective function - (negative log-likelihood), which represent the minimized
#'                  objective function value from the optimisation. Smaller value equals better optimisation.
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
#' obs <- 10
#' season <- c("2018/2019", "2019/2020", "2020/2021")
#' season_num_rev <- rev(seq(from = 1, to = length(season)))
#' observations <- rep(stats::rnorm(10, obs), length(season))
#'
#' # Add into a tibble with decreasing weight for older seasons
#' data_input <- tibble::tibble(
#'   observation = observations,
#'   weight = 0.8^rep(season_num_rev, each = obs)
#' )
#'
#' # Use the model
#' fit_percentiles(
#'   weighted_observations = data_input,
#'   conf_levels = c(0.50, 0.90, 0.95),
#'   family= "weibull"
#' )
fit_percentiles <- function(
  weighted_observations,
  conf_levels = c(0.50, 0.90, 0.95),
  family = c("lnorm",
             "weibull",
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
  # Check input arguments
  coll <- checkmate::makeAssertCollection()
  checkmate::assert_tibble(weighted_observations, add = coll)
  checkmate::assert_numeric(conf_levels, lower = 0, upper = 1,
                            unique = TRUE, sorted = TRUE, add = coll)
  checkmate::assert_names(colnames(weighted_observations),
                          identical.to = c("observation", "weight"), add = coll)
  checkmate::assert_numeric(lower_optim, add = coll)
  checkmate::assert_numeric(upper_optim, add = coll)
  checkmate::reportAssertions(coll)
  # Match the arguments.
  family <- rlang::arg_match(family)
  optim_method <- rlang::arg_match(optim_method)

  # If there is only one unique observation we cannot optimise -> return NA
  if (length(unique(weighted_observations$observation)) == 1) {
    warning("Cannot optimise, all `n_peak` observations are identical. Returning NA values")
    return(list(
      conf_levels = conf_levels,
      values      = rep(NA, length(conf_levels)),
      par         = c(NA, NA),
      obj_value   = NA,
      converged   = FALSE,
      family      = family
    ))
  }

  # Initialising parameters based on family
  init_par_fun <- function(family, observations) {
    init_params <- switch(family,
      weibull = log(c(1.5, mean(observations))),
      lnorm = c(mean(log(observations)), log(stats::sd(log(observations)))),
      exp = log(1.5)
    )
    init_params
  }

  # The weighted negative loglikelihood function
  nll <- function(par, weighted_observations, family = family) {
    log_probability <- switch(family,
      weibull = stats::dweibull(weighted_observations$observation, shape = exp(par[1]), scale = exp(par[2]),
                                log = TRUE),
      lnorm = stats::dlnorm(weighted_observations$observation, meanlog =  par[1], sdlog = exp(par[2]), log = TRUE),
      exp = stats::dexp(weighted_observations$observation, rate = exp(par[1]), log = TRUE)
    )
    return(-sum(log_probability * weighted_observations$weight))
  }

  # Run optimisation for weighted observations
  optim_obj <- stats::optim(par = init_par_fun(family = family,
                                               observations = weighted_observations$observation),
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
    lnorm = c(optim_obj$par[1], exp(optim_obj$par[2])),
    exp = c(exp(optim_obj$par), NA)
  )

  # Estimate the low, medium, high intensity levels based on input `conf_levels`
  percentiles <- switch(family,
    weibull = stats::qweibull(p = conf_levels, shape = par_fit[1], scale = par_fit[2]),
    lnorm = stats::qlnorm(p = conf_levels, meanlog = par_fit[1], sdlog = par_fit[2]),
    exp = stats::qexp(p = conf_levels, rate = par_fit[1])
  )

  # Create return fit parameters
  return(list(
    conf_levels = conf_levels,
    values = as.numeric(percentiles),
    par = par_fit[1:2], # Returns NA in second position if optim_method = "exp"
    obj_value = optim_obj$value,
    converged = optim_obj$convergence == 0,
    family = family
  ))
}
