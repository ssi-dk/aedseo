#' Create a tibble-like `tsd` (time-series data) object from time series data and corresponding dates.
#'
#' @description
#'
#' This function takes observations and the corresponding date vector (`time`) and converts them into a `tsd` object,
#' which is a time series data structure that can be used for time series analysis. For count data, supply `cases`
#' or `incidence`/`population`. For binomial data, supply `successes`/`trials` or `proportion`/`trials`;
#' downstream functions then keep observations on the proportion scale.
#'
#' Options:
#'  - `incidence` can be calculated if also supplying `cases`, `population`, and `incidence_denominator`.
#'  - `cases` can be calculated if also supplying `incidence`, `population` and `incidence_denominator`.
#'  - If background population changes during the time series,
#' it is used to adjust the growth rate in `seasonal_onset()`.
#'  - `proportion` can be calculated if also supplying `successes` and `trials`.
#'  - `successes` can be calculated if also supplying `proportion` and `trials`.
#'
#' @param cases `r rd_cases`
#' @param successes An integer vector containing binomial successes. Use with `trials` for binomial data.
#' @param incidence A numeric vector containing the time series incidences.
#' With the given incidence_denominator.
#' @param proportion A numeric vector containing binomial proportions in `[0, 1]` or percentages in `(1, 100]`.
#' Use with `trials` for proportional/binomial data.
#' @param population `r rd_population`
#' @param trials An integer vector containing binomial trials. Use with `successes` or `proportion` for binomial data.
#' @param incidence_denominator An integer >= 1, specifying the observations per incidence-denominator.
#' @param time A date vector containing the corresponding dates.
#' @param time_interval `r rd_time_interval`
#'
#' @return A `tsd` object containing:
#'   - 'time': The time point for the corresponding data.
#'   - 'cases': The number of cases at the time point.
#'   - 'incidence': The incidence per `incidence_denominator` at the time point. (optional)
#'   - 'population': The background population for the cases at the time point. (optional)
#'   - 'successes': The number of successes at the time point for binomial input. (optional)
#'   - 'proportion': The proportion of successes per trial at the time point for binomial input. (optional)
#'   - 'trials': The number of binomial trials at the time point for binomial input. (optional)
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
#' # Create a `tsd` object with incidence from cases, population and default incidence_denominator
#' tsd_calculate_incidence <- to_time_series(
#'   cases = c(100, 120, 130, 150),
#'   time = seq(from = as.Date("2023-01-01"), by = "1 week", length.out = 4),
#'   population = c(3000000, 3000000, 3000000, 3000000)
#' )
#'
#' # Create a `tsd` object with cases from incidence, population and default incidence_denominator
#' tsd_calculate_cases <- to_time_series(
#'   incidence = c(5, 7.8, 8, 8.5),
#'   time = seq(from = as.Date("2023-01-01"), by = "1 week", length.out = 4),
#'   population = c(3000000, 3000000, 3000000, 3000000)
#' )
#'
#' # Create a `tsd` object with binomial data
#' tsd_binomial <- to_time_series(
#'   successes = c(10, 12, 18, 25),
#'   trials = c(100, 100, 120, 140),
#'   time = seq(from = as.Date("2023-01-01"), by = "1 week", length.out = 4)
#' )
#'
to_time_series <- function(                                     # nolint: cyclocomp_linter.
  cases = NULL,
  successes = NULL,
  incidence = NULL,
  proportion = NULL,
  population = NULL,
  trials = NULL,
  incidence_denominator = if (is.null(population)) NA_real_ else 1e5,
  time,
  time_interval = c("weeks", "days", "months")
) {
  if (!is.null(successes)) {
    if (!is.null(cases)) {
      stop("Use only one of `cases` or `successes`.")
    }
    cases <- successes
  }
  if (!is.null(trials)) {
    if (!is.null(population)) {
      stop("Use only one of `population` or `trials`.")
    }
    population <- trials
  }
  if (!is.null(proportion)) {
    if (!is.null(incidence)) {
      stop("Use only one of `incidence` or `proportion`.")
    }
    proportion <- dplyr::if_else(
      proportion > 1 & proportion <= 100, proportion / 100, proportion
    )
    incidence <- proportion
    incidence_denominator <- 1
  }
  binomial_input <- !is.null(successes) || !is.null(proportion) || !is.null(trials)
  if (binomial_input) {
    incidence_denominator <- 1
  }

  # Check input arguments
  coll <- checkmate::makeAssertCollection()
  checkmate::assert_date(time, add = coll)
  checkmate::assert_integerish(cases, null.ok = TRUE, add = coll)
  checkmate::assert_numeric(incidence, null.ok = TRUE, add = coll)
  checkmate::assert_integerish(population, null.ok = TRUE, add = coll)
  checkmate::assert_integerish(incidence_denominator, lower = 1, len = 1, null.ok = TRUE, add = coll)
  checkmate::reportAssertions(coll)
  if (is.null(cases) && is.null(incidence)) {
    coll$push("Either cases or incidence must be given")
  }
  if (is.null(cases) && is.null(population) && !is.null(incidence)) {
    coll$push("seasonal_onset() assumes integer counts, please supply population and incidence_denominator")
  }
  if (!is.null(cases) && !is.null(population) && any(cases > population, na.rm = TRUE)) {
    coll$push("`cases` must be less than or equal to `population` when both are supplied")
  }
  if (is.null(population) && !is.na(incidence_denominator)) {
    coll$push("If incidence_denominator is assigned population should also be assigned")
  }
  if (!is.null(proportion) && any(incidence < 0 | incidence > 1, na.rm = TRUE)) {
    coll$push("`proportion` must be between 0 and 1 or a percentage between 1 and 100")
  }
  if (binomial_input) {
    if (is.null(population)) {
      coll$push("`trials` (or `population`) must be supplied for binomial input")
    } else {
      checkmate::assert_true(all(population > 0, na.rm = TRUE), add = coll)
    }
    if (!is.null(cases)) {
      checkmate::assert_true(all(cases >= 0, na.rm = TRUE), add = coll)
    }
  }
  checkmate::reportAssertions(coll)

  # Throw an error if any of the inputs are not supported
  time_interval <- match.arg(time_interval)

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
  if (is.null(cases) && !is.null(population) && !is.null(incidence) && !is.na(incidence_denominator)) {
    tbl <- tbl |>
      dplyr::mutate(cases = round((.data$incidence * .data$population) / incidence_denominator))
  }

  if (binomial_input && !is.null(cases) && !is.null(population)) {
    tbl <- tbl |>
      dplyr::mutate(incidence = .data$cases / .data$population)
  }
  if (binomial_input) {
    tbl <- tibble::tibble(
      time = time,
      successes = tbl$cases,
      proportion = tbl$incidence,
      trials = tbl$population
    )
  }

  # Create the time series data object
  tibble::new_tibble(
    x = tbl,
    class = "tsd",
    time_interval = time_interval,
    incidence_denominator = incidence_denominator
  )
}
