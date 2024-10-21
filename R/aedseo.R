#' Automated and Early Detection of Seasonal Epidemic Onset
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This function performs automated and early detection of seasonal epidemic
#' onsets (aedseo) on a time series dataset. It estimates growth rates for
#' consecutive time intervals and calculates the sum of cases (sum_of_cases).
#'
#' @param tsd A `aedseo_tsd` object containing time series data with 'time' and
#' 'observed.'
#' @param k An integer specifying the window size for modeling growth rates.
#' @param level The confidence level for parameter estimates, a numeric value
#' between 0 and 1.
#' @param disease_threshold An integer specifying the threshold for considering
#' a disease outbreak. It defines the per time-step disease threshold that has
#' to be surpassed to possibly trigger a seasonal onset alarm. If the total
#' number of cases in a window of size k exceeds  `disease_threshold * k`,
#' a seasonal onset alarm can be triggered.
#' @param family A character string specifying the family for modeling.
#' Choose between "poisson," or "quasipoisson".
#' @param na_fraction_allowed Numeric value between 0 and 1 specifying the
#' fraction of observables in the window of size k that are allowed to be NA.
#' @param season_weeks A numeric vector of length 2, `c(start,end)`, with the
#' start and end weeks of the seasons to stratify the observations by. Must
#' span the new year; ex: `season_weeks = c(21,20)`. Default, `NULL`, is no
#' stratification by season.
#'
#' @return A `aedseo` object containing:
#'   - 'reference_time': The time point for which the growth rate is estimated.
#'   - 'observed': The observed value in the reference time point.
#'   - 'season': The stratification of observables in corresponding seasons.
#'   - 'growth_rate': The estimated growth rate.
#'   - 'lower_growth_rate': The lower bound of the growth rate's confidence
#'   interval.
#'   - 'upper_growth_rate': The upper bound of the growth rate's confidence
#'   interval.
#'   - 'growth_warning': Logical. Is the growth rate significantly higher than
#'   zero?
#'   - 'sum_of_cases': The sum of cases within the time window.
#'   - 'sum_of_cases_warning': Logical. Does the Sum of Cases exceed the
#'   disease threshold?
#'   - 'seasonal_onset_alarm': Logical. Is there a seasonal onset alarm?
#'   - 'skipped_window': Logical. Was the window skipped due to missing?
#'   - 'converged': Logical. Was the IWLS judged to have converged?
#'
#' @export
#'
#' @examples
#' # Create a tibble object from sample data
#' tsd_data <- tsd(
#'   observed = c(100, 120, 150, 180, 220, 270),
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
#' # Calculate AEDSEO with a 3-day window and a Poisson family model
#' aedseo_results <- aedseo(
#'   tsd = tsd_data,
#'   k = 3,
#'   level = 0.95,
#'   disease_threshold = 200,
#'   family = "poisson",
#'   na_fraction_allowed = 0.4,
#'   season_weeks = NULL
#' )
#'
#' # Print the AEDSEO results
#' print(aedseo_results)
aedseo <- function(
    tsd,
    k = 5,
    level = 0.95,
    disease_threshold = NA_integer_,
    family = c(
      "poisson",
      "quasipoisson"
      # TODO: #10 Include negative.binomial regressions. @telkamp7
    ),
    na_fraction_allowed = 0.4,
    season_weeks = NULL) {
  # Check input arguments
  coll <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(tsd)
  checkmate::assert_class(tsd, "aedseo_tsd")
  checkmate::assert_names(colnames(tsd), identical.to = c("time", "observed"))
  checkmate::assert_numeric(level, lower = 0, upper = 1, add = coll)
  checkmate::assert_numeric(na_fraction_allowed, lower = 0, upper = 1,
                            add = coll)
  checkmate::assert_integerish(k, add = coll)
  checkmate::assert_integerish(disease_threshold, add = coll)
  checkmate::assert_integerish(season_weeks, len = 2, lower = 1, upper = 53,
                               null.ok = TRUE, add = coll)
  checkmate::reportAssertions(coll)

  # Throw an error if any of the inputs are not supported
  family <- rlang::arg_match(family)

  # Extract the length of the series
  n <- base::nrow(tsd)

  # Allocate space for growth rate estimates
  res <- tibble::tibble()
  skipped_window <- base::rep(FALSE, base::nrow(tsd))

  # Add the seasons to tsd if available
  if (!is.null(season_weeks)) {
    tsd <- tsd |> dplyr::mutate(season = epi_calendar(.data$time))
  } else {
    tsd <- tsd |> dplyr::mutate(season = "not_defined")
  }

  for (i in k:n) {
    # Index observations for this iteration
    obs_iter <- tsd[(i - k + 1):i, ]

    # Evaluate NA values in windows
    if (sum(is.na(obs_iter)) >= k * na_fraction_allowed) {
      skipped_window[i] <- TRUE
      # Set fields to NA since the window is skipped
      growth_rates <- list(estimate = c(NA, NA, NA),
                           fit = list(converged = FALSE))
    } else {
      # Calculate growth rates
      growth_rates <- fit_growth_rate(
        observations = obs_iter$observed,
        level = level,
        family = family
      )
    }

    # See if the growth rate is significantly higher than zero
    growth_warning <- growth_rates$estimate[2] > 0

    # Calculate Sum of Cases (sum_of_cases)
    sum_of_cases <- base::sum(obs_iter$observed, na.rm = TRUE)

    # Evaluate if sum_of_cases exceeds disease_threshold
    sum_of_cases_warning <- sum_of_cases > (disease_threshold * k)

    # Give an seasonal_onset_alarm if both criteria are met
    seasonal_onset_alarm <- growth_warning & sum_of_cases_warning

    # Collect the results
    res <- dplyr::bind_rows(
      res,
      tibble::tibble(
        reference_time = tsd$time[i],
        observed = tsd$observed[i],
        season = tsd$season[i],
        growth_rate = growth_rates$estimate[1],
        lower_growth_rate = growth_rates$estimate[2],
        upper_growth_rate = growth_rates$estimate[3],
        growth_warning = growth_warning,
        sum_of_cases = sum_of_cases,
        sum_of_cases_warning = sum_of_cases_warning,
        seasonal_onset_alarm = seasonal_onset_alarm,
        skipped_window = skipped_window[i],
        converged = growth_rates$fit$converged
      )
    )
  }

  # Turn the results into an `aedseo` class
  ans <- tibble::new_tibble(
    x = res,
    class = "aedseo",
    k = k,
    level = level,
    disease_threshold = disease_threshold,
    family = family
  )

  # Keep attributes from aedseo_tsd
  attr(ans, "time_interval") <- attr(tsd, "time_interval")

  return(ans)
}
