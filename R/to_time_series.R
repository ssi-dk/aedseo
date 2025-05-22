#' Create a tibble-like `tsd` (time-series data) object from observed data and corresponding dates.
#'
#' @description
#'
#' This function takes observations and the corresponding date vector and converts it into a `tsd` object, which is
#' a time series data structure that can be used for time series analysis.
#' Optionally the background population can be added.
#'
#' @param observation `r rd_observation`
#' @param time A date vector containing the corresponding dates.
#' @param population `r rd_population`
#' @param time_interval `r rd_time_interval`
#'
#' @return A `tsd` object containing:
#'   - 'time': The time point for for when the observation is observed.
#'   - 'observation': The observed value at the time point.
#'   - 'population': The background population for the observed value (optional)
#'
#' @export
#'
#' @examples
#' # Create a `tsd` object from daily data
#' daily_tsd <- to_time_series(
#'   observation = c(10, 15, 20, 18),
#'   time = as.Date(
#'     c("2023-01-01", "2023-01-02", "2023-01-03", "2023-01-04")
#'   ),
#'   time_interval = "day"
#' )
#'
#' # Create a `tsd` object from weekly data
#' weekly_tsd <- to_time_series(
#'   observation = c(100, 120, 130),
#'   time = as.Date(
#'     c("2023-01-01", "2023-01-08", "2023-01-15")
#'   ),
#'   population = c(100000, 100000, 100000),
#'   time_interval = "week"
#' )
#'
#' # Create a `tsd` object from monthly data
#' monthly_tsd <- to_time_series(
#'   observation = c(500, 520, 540),
#'   time = as.Date(
#'     c("2023-01-01", "2023-02-01", "2023-03-01")
#'   ),
#'   time_interval = "month"
#' )
#'
to_time_series <- function(
  observation,
  time,
  population = NULL,
  time_interval = c("day", "week", "month")
) {
  # Check input arguments
  coll <- checkmate::makeAssertCollection()
  checkmate::assert_date(time, add = coll)
  checkmate::assert_numeric(observation, add = coll)
  checkmate::assert_numeric(population, null.ok = TRUE, add = coll)
  checkmate::reportAssertions(coll)

  # Throw an error if any of the inputs are not supported
  time_interval <- rlang::arg_match(time_interval)

  # Collect the input in a tibble
  tbl <- purrr::compact(list( # compact discards empty vectors
    time = time,
    observation = observation,
    population = population
  )) |>
    tibble::as_tibble()

  # Create the time series data object
  tsd <- tibble::new_tibble(
    x = tbl,
    class = "tsd",
    time_interval = time_interval
  )

  return(tsd)
}

#' Deprecated tsd function
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function has been renamed to better reflect its purpose.
#' Please use `to_time_series()` instead.
#' @param ... Arguments passed to `to_time_series()`
#' @keywords internal
#' @export
tsd <- function(...) {
  lifecycle::deprecate_warn("0.1.2", "tsd()", "to_time_series()")
  to_time_series(...)
}
