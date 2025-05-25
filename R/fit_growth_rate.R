#' Fit a growth rate model to time series observations.
#'
#' @description
#'
#' This function fits a growth rate model to time series observations and provides parameter estimates along with
#' confidence intervals.
#'
#' @param observation `r rd_observation`.
#' @param population `r rd_population`
#' @param level The confidence level for parameter estimates, a numeric value between 0 and 1.
#' @param family `r rd_family()`
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
  population = NULL,
  level = 0.95,
  family = c(
    "quasipoisson",
    "poisson"
  )
) {
  safe_confint <- purrr::safely(stats::confint)

  # Check input arguments
  coll <- checkmate::makeAssertCollection()
  checkmate::assert_numeric(observation, add = coll)
  checkmate::assert_numeric(level, lower = 0, upper = 1, add = coll)
  checkmate::assert_numeric(population, null.ok = TRUE, add = coll)
  # Match the selected model
  if (is.character(family)) { # If character
    fam_name <- match.arg(family)
    family_fun <- get(fam_name, mode = "function", envir = parent.frame())
    fam_obj <- family_fun()
  } else if (is.function(family)) { # If family-generator e.g. stats::poisson
    fam_obj <- family()
  } else if (inherits(family, "family")) { # If family object e.g. stats::poisson()
    fam_obj <- family
  } else {
    coll$push("`family` must be one of: character, family-generator, or family object")
  }
  checkmate::reportAssertions(coll) # Assert that we have an object before going further
  checkmate::assert_names(names(fam_obj), must.include = c("family", "link"), add = coll)
  checkmate::assert_choice(fam_obj$family, choices = c("poisson", "quasipoisson"), add = coll)
  checkmate::reportAssertions(coll)

  # Construct the data with growth rates for the glm model
  growth_data <- purrr::compact(list(
    growth_rate = seq_along(observation),
    x = observation,
    population = population
  )) |>
    tibble::as_tibble()

  # Construct formula terms
  terms <- if (is.null(population)) {
    "growth_rate"
  } else {
    c("growth_rate", "offset(log(population))")
  }

  # Fit the model
  growth_fit <- stats::glm(
    formula = stats::reformulate(response = "observation", termlabels = terms),
    data = growth_data,
    family = fam_obj
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
