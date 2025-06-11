#' Create a tibble-like `tsd` (time-series data) object from time series data and corresponding dates.
#'
#' @description
#'
#' This function takes `cases` and the corresponding date vector (`time`) and converts it into a `tsd` object, which is
#' a time series data structure that can be used for time series analysis. If incidence is added, it will be used as
#' observation in all future use of the `aedseo` package on the defined `tsd` object.
#'
#' Options:
#'  - `incidence` can be calculated if also supplying `cases`, `population`, and `incidence_denominator`.
#'  - `cases` can be calculated if also supplying `incidence`, `population` and `incidence_denominator`.
#'  - If background population changes during the time series,
#' it is used to adjust the growth rate in `seasonal_onset()`.
#'
#' @param cases `r rd_cases`
#' @param incidence A numeric vector containing the time series incidences.
#' @param population `r rd_population`
#' @param incidence_denominator An integer >= 1, specifying the cases per incidence_denominator.
#' @param time A date vector containing the corresponding dates.
#' @param time_interval `r rd_time_interval`
#'
#' @return A `tsd` object containing:
#'   - 'time': The time point for the corresponding data.
#'   - 'cases': The number of cases at the time point.
#'   - 'incidence': The incidence per `incidence_denominator` at the time point. (optional)
#'   - 'population': The background population for the cases at the time point. (optional)
#'
#' @export
#'
#' @examples
#' # Create a `tsd` object with only cases
#' tsd_cases <- to_time_series(
#'   cases = c(10, 15, 20, 18),
#'   time = seq(from = as.Date("2023-01-01"), by = "1 week", length.out = 4)
#' )
#'
#' # Create a `tsd` object with incidence from cases, population and incidence_denominator
#' tsd_calculate_incidence <- to_time_series(
#'   cases = c(100, 120, 130, 150),
#'   time = seq(from = as.Date("2023-01-01"), by = "1 week", length.out = 4),
#'   population = c(3000000, 3000000, 3000000, 3000000)
#' )
#'
#' # Create a `tsd` object with cases from incidence, population and incidence_denominator
#' tsd_calculate_cases <- to_time_series(
#'   incidence = c(5, 7.8, 8, 8.5),
#'   time = seq(from = as.Date("2023-01-01"), by = "1 week", length.out = 4),
#'   population = c(3000000, 3000000, 3000000, 3000000)
#' )
#'
to_time_series <- function(                                     # nolint: cyclocomp_linter.
  cases = NULL,
  incidence = NULL,
  population = NULL,
  incidence_denominator = if (is.null(population)) NA else 1e5,
  time,
  time_interval = c("week", "day", "month")
) {
  # Check input arguments
  coll <- checkmate::makeAssertCollection()
  checkmate::assert_date(time, add = coll)
  checkmate::assert_integerish(cases, null.ok = TRUE, add = coll)
  checkmate::assert_numeric(incidence, null.ok = TRUE, add = coll)
  checkmate::assert_integerish(population, null.ok = TRUE, add = coll)
  checkmate::assert_integerish(incidence_denominator, lower = 1, len = 1, null.ok = TRUE, add = coll)
  checkmate::reportAssertions(coll)
  if (is.null(cases) && is.null(incidence)) {
    coll$push("Either cases or incidence must be assigned")
  }
  if (is.null(cases) && is.null(population) && !is.null(incidence)) {
    coll$push("seasonal_onset() assumes integer counts, please supply population and incidence_denominator")
  }
  if (is.null(population) && !is.na(incidence_denominator)) {
    coll$push("If incidence_denominator is assigned population should also be assigned")
  }
  checkmate::reportAssertions(coll)

  # Throw an error if any of the inputs are not supported
  time_interval <- rlang::arg_match(time_interval)

  # Collect the input in a tibble
  tbl <- purrr::compact(list( # compact discards empty vectors
    time = time,
    cases = cases,
    incidence = incidence,
    population = population
  )) |>
    tibble::as_tibble()
  # Calculate incidence from input
  if (!is.null(cases) && is.null(incidence) && !is.null(population)) {
    tbl <- tbl |>
      dplyr::mutate(incidence = (.data$cases / .data$population) * incidence_denominator)
  }
  # Calculate cases from input
  if (is.null(cases) && !is.null(population) && !is.null(incidence) && !is.null(incidence_denominator)) {
    tbl <- tbl |>
      dplyr::mutate(cases = (.data$incidence * .data$population) / incidence_denominator)
  }

  # Create the time series data object
  tsd <- tibble::new_tibble(
    x = tbl,
    class = "tsd",
    time_interval = time_interval,
    incidence_denominator = incidence_denominator
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
