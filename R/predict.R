#' Predict Observations for Future Time Steps
#'
#' @description
#'
#' This function is used to predict future observations based on a `tsd_onset` object.
#' It takes the `tsd_onset` object and the number of future time steps (`n_step`) for which you want to
#' make predictions and returns a prediction tibble.
#'
#' @param object A `tsd_onset` object created using the `seasonal_onset()` function.
#' @param n_step An integer specifying the number of future time steps for which you want to predict observations.
#' @param ... Additional arguments (not used).
#'
#' @return  A tibble-like object called `tsd_predict` containing the predicted observations, including reference time,
#' lower confidence interval, and upper confidence interval for the specified number of future time steps.
#' NOTE: If `time_interval` is month then subsequent `reference_time` will be every 30 days.
#'
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' # Generate predictions of time series data
#' set.seed(123)
#' time_series <- generate_seasonal_data(
#'   years = 1,
#'   time_interval = "day"
#' )
#' # Apply `seasonal_onset` analysis
#' time_series_with_onset <- seasonal_onset(
#'   tsd = time_series,
#'   k = 7
#' )
#' # Predict observations for the next 7 time steps
#' predict(object = time_series_with_onset, n_step = 7)
predict.tsd_onset <- function(object, n_step = 3, ...) {
  checkmate::assert_class(object, "tsd_onset")

  # Determine time_interval
  time_interval <- attr(object, "time_interval")
  if (time_interval == "day") {
    t_int <- 1
  } else if (time_interval == "week") {
    t_int <- 7
  } else if (time_interval == "month") {
    t_int <- 30
  }

  # Calculate the prediction
  res <- dplyr::last(object) |>
    dplyr::reframe(
      t = 0:n_step,
      reference_time = .data$reference_time + t * t_int,
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
