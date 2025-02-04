#' Predict Growth Rates for Future Time Steps
#'
#' @description
#'
#' This function is used to predict future growth rates based on a model object created using the 'aedseo' package.
#' It takes the model object and the number of future time steps (`n_step`) for which you want to make predictions and
#' returns a prediction tibble.
#'
#' @param object A `tsd_onset` object created using the `seasonal_onset()` function.
#' @param n_step An integer specifying the number of future time steps for which you want to predict growth rates.
#' @param ... Additional arguments (not used).
#'
#' @return  A tibble-like object called `tsd_onset` containing the predicted growth rates, including time,
#' estimated growth rate, lower confidence interval, and upper confidence interval for the specified number of
#' future time steps.
#'
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' # Analyze the data using the aedseo package
#' time_series <- to_time_series(
#'   observation = c(100, 120, 150, 180, 220, 270),
#'   time = as.Date(c(
#'     "2023-01-01",
#'     "2023-01-02",
#'     "2023-01-03",
#'     "2023-01-04",
#'     "2023-01-05",
#'     "2023-01-06"
#'   )),
#'   time_interval = "day"
#' )
#'
#' time_series_with_onset <- seasonal_onset(
#'   tsd = time_series,
#'   k = 3,
#'   level = 0.95,
#'   family = "poisson"
#' )
#'
#' # Predict growth rates for the next 5 time steps
#' predict(object = time_series_with_onset, n_step = 5)
predict.tsd_onset <- function(object, n_step = 3, ...) {
  # Calculate the prediction
  res <- dplyr::last(object) |>
    dplyr::reframe(
      t = 0:n_step,
      time = .data$reference_time + t,
      estimate = exp(log(.data$observation) + .data$growth_rate * t),
      lower = exp(log(.data$observation) + .data$lower_growth_rate * t),
      upper = exp(log(.data$observation) + .data$upper_growth_rate * t)
    )

  # Extract the attributes from the object
  attributes_object <- attributes(object)

  # Extract the object k, level, and family
  k <- attributes_object$k
  level <- attributes_object$level
  family <- attributes_object$family

  # Turn the results into a class
  ans <- tibble::new_tibble(
    x = res,
    class = "tsd_predict",
    k = k,
    level = level,
    family = family
  )

  # Return
  return(ans)
}
