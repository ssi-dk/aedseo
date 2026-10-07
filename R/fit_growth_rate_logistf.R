#' Fit a growth rate model to time series observations using the logistf function in the logistf package.
#' logistf function: Implements Firth's bias-Reduced penalized-likelihood logistic regression.
#'
#' @description
#'
#' This function fits a growth rate model to time series observations and provides parameter estimates along with
#' confidence intervals.
#'
#' @param cases An integer vector containing cases.
#' @param denominator An integer vector containing binomial sample size.
#' @param level The confidence level for parameter estimates, a numeric value between 0 and 1.
#'
#' @return A list containing:
#'   - 'fit': The fitted growth rate model.
#'   - 'estimate': A numeric vector with parameter estimates, including
#'   the growth rate and its confidence interval.
#'   - 'level': The confidence level used for estimating parameter
#'   confidence intervals.
#' @export
#'
#'
fit_growth_rate_logistf <- function(
  cases,
  denominator,
  level = 0.95
) {
  # Construct data frame
  growth_data <- tibble::tibble(
    time_step = seq_along(cases),
    cases = cases,
    denominator = denominator
  )

  # logistf requires a binary response. Each time point is represented by:
  #   outcome = 1, weight = number of positive samples
  #   outcome = 0, weight = number of negative samples
  firth_data <- dplyr::bind_rows(
    growth_data |>
      dplyr::mutate(
        time_step = .data$time_step,
        outcome = 1L,
        weight = .data$cases,
        .keep = "none"
      ),
    growth_data |>
      dplyr::mutate(
        time_step = .data$time_step,
        outcome = 0L,
        weight = .data$denominator - .data$cases,
        .keep = "none"
      )
  ) |>
    dplyr::filter(.data$weight > 0)

  # Fit growth model
  growth_fit <- tryCatch(
    logistf::logistf(
      outcome ~ time_step,
      data = firth_data,
      weights = weight,
      alpha = 1 - level
    ),
    error = function(e) e
  )

  if (inherits(growth_fit, "error")) {
    return(
      list(
        fit = list(
          converged = FALSE,
          message = conditionMessage(growth_fit)
        ),
        estimate = c(
          estimate = NA_real_,
          lower = NA_real_,
          upper = NA_real_
        ),
        level = level
      )
    )
  }

  estimates <- c(
    estimate = unname(
      growth_fit$coefficients[["time_step"]]
    ),
    lower = unname(
      growth_fit$ci.lower[["time_step"]]
    ),
    upper = unname(
      growth_fit$ci.upper[["time_step"]]
    )
  )

  # logistf returns a three-element convergence diagnostic
  convergence_limits <- unlist(growth_fit$control[c("lconv", "gconv", "xconv")], use.names = FALSE)

  # Check if the model converged
  growth_fit$converged <-
    length(growth_fit$conv) == 3L &&
    all(is.finite(growth_fit$conv)) &&
    all(abs(growth_fit$conv) <= convergence_limits) &&
    all(is.finite(estimates))

  list(
    fit = growth_fit,
    estimate = estimates,
    level = level
  )
}
