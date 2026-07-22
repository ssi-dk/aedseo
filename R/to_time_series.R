#' Create a tibble-like `tsd` (time-series data) object from time series data and corresponding dates.
#'
#' @description
#'
#' This function takes observations and the corresponding date vector (`time`) and converts them into a `tsd` object,
#' which is a time series data structure that can be used for time series analysis. For count data, supply `cases`
#' or `incidence` with given `population`. For binomial data, supply `samples` and `cases` or `proportion`.
#'
#' Options:
#'  - `incidence` can be calculated if also supplying `cases`, `population`, and `incidence_denominator`.
#'  - `cases` can be calculated if also supplying `incidence`, `population` and `incidence_denominator`.
#'  - If background population changes during the time series,
#' it is used to adjust the growth rate in `seasonal_onset()`.
#'  - `proportion` will be calculated if supplying `cases` and `samples`.
#'  - `cases` will be calculated if supplying `proportion` and `samples`.
#'
#' @param cases `r rd_cases`
#' @param incidence A numeric vector containing the time series incidences.
#' With the given incidence_denominator.
#' @param population `r rd_population`
#' @param samples An integer vector containing number of samples tested. Use with `cases` or
#' `proportion` for binomial data.
#' @param proportion A numeric vector containing binomial proportions in `[0, 1]` (Will be rescaled to `[0, 1]`
#' if percentages in `(1, 100]` are provided).
#' Use with `trials` for proportional/binomial data.
#' @param incidence_denominator An integer >= 1, specifying the observations per incidence-denominator.
#' @param time A date vector containing the corresponding dates.
#' @param time_interval `r rd_time_interval`
#'
#' @return A `tsd` object containing:
#'   - 'time': The time point for the corresponding data.
#'   - 'cases': The number of cases at the time point.
#'   - 'incidence': The incidence per `incidence_denominator` at the time point. (optional)
#'   - 'population': The background population for the cases at the time point. (optional)
#'   - 'proportion': The proportion of cases in the tested samples at the time point for binomial input. (optional)
#'   - 'samples': The number of samples tested at the time point for binomial input. (optional)
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
#'   cases = c(10, 12, 18, 25),
#'   samples = c(100, 100, 120, 140),
#'   time = seq(from = as.Date("2023-01-01"), by = "1 week", length.out = 4)
#' )
#'
to_time_series <- function(                                     # nolint: cyclocomp_linter.
  cases = NULL,
  incidence = NULL,
  population = NULL,
  proportion = NULL,
  samples = NULL,
  incidence_denominator = if (is.null(population)) NA_real_ else 1e5,
  time,
  time_interval = c("weeks", "days", "months")
) {
  # Check input arguments
  coll <- checkmate::makeAssertCollection()
  checkmate::assert_date(time, add = coll)
  checkmate::assert_integerish(cases, null.ok = TRUE, add = coll)
  checkmate::assert_numeric(population, null.ok = TRUE, add = coll)
  checkmate::assert_numeric(incidence, null.ok = TRUE, add = coll)
  checkmate::assert_numeric(proportion, null.ok = TRUE, add = coll)
  checkmate::assert_integerish(samples, null.ok = TRUE, add = coll)

  # Defining output types to be used for asserting and completing columns
  outcome_type <- NULL
  if (!is.null(population) || !is.null(incidence)) {
    outcome_type <- c(outcome_type, "incidence")
  }
  if (!is.null(samples) || !is.null(proportion)) {
    outcome_type <- c(outcome_type, "proportion")
  }
  # Defaulting to cases
  outcome_type <- ifelse(is.null(outcome_type), "cases", outcome_type)

  # Enforcing incidence denominator as 1 when proportion is the only outcome_type
  if (outcome_type == "proportion") {
    incidence_denominator <- 1
  }

  if ("proportion" %in% outcome_type) {
    if (is.null(samples))
      coll$push("Assuming binomial data as 'proportion' is provided. In this case 'samples' has to be provided.")
    checkmate::assert_true(all(samples > 0, na.rm = TRUE), add = coll)

    # Rescale percentages to proportions if needed
    if (!is.null(proportion)) {
      if (any(proportion > 1 & proportion <= 100)) {
        proportion <- proportion / 100
      }
    }
    if (!is.null(proportion) && any(proportion < 0 | proportion > 1, na.rm = TRUE)) {
      coll$push("`proportion` must be between 0 and 1 or a percentage between 1 and 100")
    }

    # Calculate cases if needed
    if (is.null(cases) & !is.null(proportion)) {
      cases <- round(proportion * samples)
    }

    # Calculate proportion if needed
    if (is.null(proportion)) {
      proportion <- cases / samples
    }

    if (!is.null(cases) && !is.null(samples) && any(cases > samples, na.rm = TRUE)) {
      coll$push("`cases` must be less than or equal to `samples` when both are supplied (or can be calculated).")
    }

  } else {

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
      coll$push("If incidence_denominator is assigned then population should also be assigned")
    }

    # Calculate cases if possible
    if (is.null(cases) & !is.null(population)) {
      cases <- round(incidence / incidence_denominator * population)
    }
    # Calculate incidence if possible
    if (is.null(incidence)) {
      incidence <- cases / population * incidence_denominator
    }
  }
  checkmate::assert_true(all(cases >= 0, na.rm = TRUE), add = coll)
  checkmate::assert_integerish(incidence_denominator, lower = 1, len = 1, null.ok = TRUE, add = coll)
  checkmate::reportAssertions(coll)

  # Throw an error if any of the inputs are not supported
  time_interval <- match.arg(time_interval)

  # Collect the input in a tibble
  tbl <- purrr::compact(list( # compact discards empty vectors
    time = time,
    cases = cases,
    incidence = incidence,
    population = population,
    proportion = proportion,
    samples = samples
  )) |>
    tibble::as_tibble()


  # Create the time series data object
  tibble::new_tibble(
    x = tbl,
    class = "tsd",
    time_interval = time_interval,
    incidence_denominator = incidence_denominator,
    outcome_type = outcome_type
  )
}
