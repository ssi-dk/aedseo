#' Fit a growth rate model to time series observations.
#'
#' @description
#'
#' This function fits a growth rate model to time series observations and provides parameter estimates along with
#' confidence intervals.
#'
#' @param observation `r rd_observation`.
#' @param pop `r rd_pop`
#' @param level The confidence level for parameter estimates, a numeric value between 0 and 1.
#' @param family A character string specifying the family for modeling. Choose between "poisson," or "quasipoisson".
#'
#' @return A list containing:
#'   - 'fit': The fitted growth rate model.
#'   - 'estimate': A numeric vector with parameter estimates, including
#'   the growth rate and its confidence interval.
#'   - 'level': The confidence level used for estimating parameter
#'   confidence intervals.
#' @export
#'
#' @examples
#' # Fit a growth rate model to a time series of counts
#' # (e.g., population growth)
#' data <- c(100, 120, 150, 180, 220, 270)
#' fit_growth_rate(
#'   observation = data,
#'   level = 0.95,
#'   family = "poisson"
#' )
fit_growth_rate <- function(
    observation,
    pop = NULL,
    level = 0.95,
    family = c(
      "poisson",
      "quasipoisson" # TODO #10 Include negative.binomial regressions. @telkamp7
    )) {
  safe_confint <- purrr::safely(stats::confint)

  # Match the selected model
  family <- switch(
    family,
    poisson = stats::poisson(link = "log"),
    quasipoisson = stats::quasipoisson(link = "log")
  )

  # Construct the data with growth rates for the glm model
  growth_data <- purrr::compact(list(
    growth_rate = seq_along(observation),
    x = observation,
    pop = pop
  )) |>
    tibble::as_tibble()

  # Construct formula terms
  terms <- if (is.null(pop)) {
    "growth_rate"
  } else {
    c("growth_rate", "offset(log(pop))")
  }

  # Fit the model
  growth_fit <- stats::glm(
    formula = stats::reformulate(response = "observation", termlabels = terms),
    data = growth_data,
    family = family
  )

  # Calculate the 'safe' confidence intervals
  growth_confint <- suppressMessages(
    safe_confint(
      object = growth_fit,
      parm = "growth_rate",
      level = level
    )
  )

  # Collect the estimates
  ans <- c(
    stats::coef(object = growth_fit)["growth_rate"],
    growth_confint$result
  )

  return(list(
    fit = growth_fit,
    estimate = ans,
    level = level
  ))
}
