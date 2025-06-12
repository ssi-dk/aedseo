#' Predict Cases for Future Time Steps
#'
#' @description
#'
#' This function is used to predict future cases based on a `tsd_onset` object.
#' It uses the `time_interval` attribute from the `tsd_onset` object to make predictions.
#'
#' @param object A `tsd_onset` object created using the `seasonal_onset()` function.
#' @param n_step An integer specifying the number of future time steps for which you want to predict cases.
#' @param ... Additional arguments (not used).
#'
#' @return  A tibble-like object called `tsd_predict` containing the predicted cases, including reference time,
#' lower confidence interval, and upper confidence interval for the specified number of future time steps.
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
#'   time_interval = "days"
#' )
#' # Apply `seasonal_onset` analysis
#' time_series_with_onset <- seasonal_onset(
#'   tsd = time_series,
#'   k = 7
#' )
#' # Predict cases for the next 7 time steps
#' predict(object = time_series_with_onset, n_step = 7)
predict.tsd_onset <- function(object, n_step = 3, ...) {
  checkmate::assert_class(object, "tsd_onset")

  # Determine time_interval
  steps <- seq_len(n_step)
  t_int <- switch(
    attr(object, "time_interval"),
    "days" = c(0, rep(1, n_step) * steps),
    "weeks" = c(0, rep(7, n_step) * steps),
    "months" = {
      last_month <- dplyr::last(object$reference_time)
      months <- c(last_month, purrr::map(steps, ~ lubridate::add_with_rollback(last_month, months(.x))))
      month_days <- as.numeric(diff(months))
      c(0, cumsum(month_days))
    }
  )

  # Calculate the prediction
  res <- dplyr::last(object) |>
    dplyr::reframe(
      t = 0:n_step,
      reference_time = .data$reference_time + t_int[t + 1],
      estimate = exp(log(.data$cases) + .data$growth_rate * t),
      lower = exp(log(.data$cases) + .data$lower_growth_rate * t),
      upper = exp(log(.data$cases) + .data$upper_growth_rate * t)
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
