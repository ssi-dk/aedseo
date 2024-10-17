#' Fit a peak algorithm to time series observations.
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This function calculates the intensity levels of peak time series
#' observations to each available season based on previouse seasons.
#' Peak observations are the n highest observations from each season.
#' It provides low, medium, high intensity levels, that reflect the
#' current intensity of infection based on previous seasons. Hence it
#' uses the peak observations from each previous season it can only
#' explain the current intensity in relation to the peak of the season.
#'
#' @param weighted_observations A tibble containing two columns of size n;
#' `observation`, which represents the data points, and `weight`.
#' The weight is the importance assigned to an observation. Higher weights
#' indicate that an observation has more influence on the model outcome, while
#' lower weights reduce its impact. The structure is:
#' `weighted_observations = tibble(observation = c(...), weight = c(...))`
#' @param conf_levels The confidence levels for parameter estimates, a numeric
#' vector of length 3. Default is c(0.95, 0.90, 0.50).
#' @param family A character string specifying the family for
#' modeling. Choose between "weibull", "lnorm" or "exp".

#'
#' @return A list containing:
#'   - 'season':
#'   - 'high_level':
#'   - 'medium_level':
#'   - 'low_level':
#'   - 'obj_value':
#'   - 'family':

fit_peak <- function(
  weighted_observations,
  conf_levels = c(0.95, 0.90, 0.5),
  family = c("weibull",
             "lnorm",
             "qexp")
) {
  # Match the arguements
  family <- rlang::arg_match(family)

  # Initialising parameters based on family
  init_par_fun <- function(family, weighted_observations) {
    init_params <- switch(family,
      weibull = log(c(1.5, mean(weighted_observations$observation))),
      lnorm = c(mean(log(weighted_observations$observation)),
                sd(log(weighted_observations$observation))),
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

  # Run optimization for weighted observations
  optim_obj <- weighted_observations |>
    dplyr::group_by(weights) |>
    dplyr::group_map(~ {
      optim(par = init_par_fun(family = family,
                               weighted_observations = .x$observation),
            fn = nll,
            data = .x,
            family = family)
    })
}
