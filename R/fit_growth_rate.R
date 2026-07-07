#' Fit a growth rate model to time series observations.
#'
#' @description
#'
#' This function fits a growth rate model to time series observations and provides parameter estimates along with
#' confidence intervals. For binomial data, supply successes and trials and use `family = "binomial"` or
#' `family = "quasibinomial"`.
#'
#' @param cases `r rd_cases`
#' @param successes An integer vector containing binomial successes. This is an alias for `cases` for
#' binomial/quasibinomial models.
#' @param population `r rd_population`
#' @param trials An integer vector containing binomial trials. This is an alias for `population` for
#' binomial/quasibinomial models.
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
#'   cases = data,
#'   level = 0.95,
#'   family = "poisson"
#' )
#'
#' # Fit a binomial growth rate model to successes out of trials
#' fit_growth_rate(
#'   successes = c(1, 2, 3, 4),
#'   trials = c(10, 10, 10, 10),
#'   family = "binomial"
#' )
fit_growth_rate <- function(
  cases = NULL,
  successes = NULL,
  population = NULL,
  trials = NULL,
  level = 0.95,
  family = c(
    "quasipoisson",
    "poisson",
    "quasibinomial",
    "binomial"
  )
) {
  safe_confint <- purrr::safely(stats::confint)

  # Check input arguments
  coll <- checkmate::makeAssertCollection()
  if (!is.null(successes)) {
    if (!is.null(cases)) {
      coll$push("Supply only one of `cases` or `successes`")
    }
    cases <- successes
  }
  if (!is.null(trials)) {
    if (!is.null(population)) {
      coll$push("Supply only one of `population` or `trials`")
    }
    population <- trials
  }
  checkmate::assert_numeric(cases, null.ok = FALSE, add = coll)
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
  checkmate::assert_choice(
    fam_obj$family,
    choices = c("poisson", "quasipoisson", "binomial", "quasibinomial"),
    add = coll
  )
  if (fam_obj$family %in% c("binomial", "quasibinomial")) {
    if (is.null(population)) {
      coll$push("`trials` (or `population`) must be supplied for binomial and quasibinomial models")
    } else {
      checkmate::assert_true(all(cases >= 0, na.rm = TRUE), add = coll)
      checkmate::assert_true(all(population > 0, na.rm = TRUE), add = coll)
      checkmate::assert_true(all(cases <= population, na.rm = TRUE), add = coll)
    }
  }
  checkmate::reportAssertions(coll)

  # Construct the data with growth rates for the glm model
  growth_data <- purrr::compact(list(
    growth_rate = seq_along(cases),
    cases = cases,
    population = population,
    successes = cases,
    trials = population
  )) |>
    tibble::as_tibble()

  # Construct formula terms
  terms <- if (is.null(population)) {
    "growth_rate"
  } else if (fam_obj$family %in% c("binomial", "quasibinomial")) {
    "growth_rate"
  } else {
    c("growth_rate", "offset(log(population))")
  }
  response <- if (fam_obj$family %in% c("binomial", "quasibinomial")) {
    "cbind(successes, trials - successes)"
  } else {
    "cases"
  }

  # Fit the model
  growth_fit <- stats::glm(
    formula = stats::as.formula(paste(response, "~", paste(terms, collapse = " + "))),
    data = growth_data,
    family = fam_obj
  )

  # Calculate the 'safe' confidence intervals
  growth_confint <- suppressMessages(
    safe_confint(
      object = growth_fit,
      parm = "growth_rate",
      level = level
    )$result
  )

  # Ensuring that confint is also returned when `safe_confint` returns an error
  if (length(growth_confint) < 2) {
    growth_confint <- c(NA_real_, NA_real_)
  } else if (fam_obj$family == "quasipoisson") {
    # Returning NA as confidence interval if fit converted to extreme underdispersion
    dispersion <- sum(
      (growth_fit$weights * growth_fit$residuals^2)[growth_fit$weights > 0]
    ) / growth_fit$df.residual
    if (dispersion < growth_fit$control$epsilon) {
      growth_confint <- c(NA_real_, NA_real_)
    }
  }

  # Collect the estimates
  ans <- c(
    stats::coef(object = growth_fit)["growth_rate"],
    growth_confint
  )

  list(
    fit = growth_fit,
    estimate = ans,
    level = level
  )
}
