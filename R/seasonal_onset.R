#' Automated and Early Detection of Seasonal Epidemic Onset
#'
#' @description
#'
#' This function performs automated and early detection of seasonal epidemic onsets on a time series dataset.
#' It estimates growth rates for consecutive time intervals and calculates the sum of cases (sum_of_cases).
#'
#' @param tsd `r rd_tsd`
#' @param k An integer specifying the window size for modeling growth rates for the onset.
#' @param level The confidence level for onset parameter estimates, a numeric value between 0 and 1.
#' @param disease_threshold `r rd_disease_threshold(usage = "onset")`
#' @param family `r rd_family()`
#' @param use_offset "Should the growth rates be adjusted for background population?"
#' @param na_fraction_allowed Numeric value between 0 and 1 specifying the fraction of observables in the window
#' of size k that are allowed to be NA or zero, i.e. without cases, in onset calculations.
#' @param season_start,season_end `r rd_season_start_end(usage = "onset")`
#' @param only_current_season `r rd_only_current_season`
#'
#' @return `r rd_seasonal_onset_return`
#'
#' @export
#'
#' @examples
#' # Create a tibble object from sample data
#' tsd_data <- tsd(
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
#' # Estimate seasonal onset with a 3-day window and a Poisson family model
#' seasonal_onset(
#'   tsd = tsd_data,
#'   k = 3,
#'   level = 0.95,
#'   disease_threshold = 20,
#'   family = "poisson",
#'   na_fraction_allowed = 0.4,
#'   season_start = NULL,
#'   season_end = NULL,
#'   only_current_season = NULL
#' )
seasonal_onset <- function(                                     # nolint: cyclocomp_linter.
    tsd,
    k = 5,
    level = 0.95,
    disease_threshold = NA_integer_,
    family = c(
      "poisson",
      "quasipoisson"
      # TODO: #10 Include negative.binomial regressions. @telkamp7
    ),
    use_offset = FALSE,
    na_fraction_allowed = 0.4,
    season_start = NULL,
    season_end = season_start - 1,
    only_current_season = NULL) {
  # Check input arguments
  coll <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(tsd, add = coll)
  checkmate::assert_class(tsd, "tsd", add = coll)
  checkmate::assert_names(
    colnames(tsd),
    must.include = c("time", "observation"),
    subset.of = c("time", "observation", "pop"),
    add = coll
  )
  checkmate::assert_logical(use_offset, add = coll)
  if (isTRUE(use_offset) && !"pop" %in% names(tsd)) {
    coll$push("If use_offset is TRUE, tsd must contain a 'pop' column")
  }
  checkmate::assert_numeric(level, lower = 0, upper = 1, add = coll)
  checkmate::assert_numeric(na_fraction_allowed, lower = 0, upper = 1,
                            add = coll)
  checkmate::assert_integerish(k, add = coll)
  checkmate::assert_integerish(disease_threshold, add = coll)
  checkmate::assert_integerish(season_start, lower = 1, upper = 53,
                               null.ok = TRUE, add = coll)
  checkmate::assert_integerish(season_end, lower = 1, upper = 53,
                               null.ok = TRUE, add = coll)
  checkmate::assert_logical(only_current_season, null.ok = TRUE, add = coll)
  if (!is.null(season_start) && is.null(season_end)) {
    coll$push("If season_start is assigned season_end must also be assigned.")
  }
  if (is.null(season_start) && !is.null(only_current_season)) {
    coll$push("If season_start is NULL only_current_season must also be NULL")
  }
  if (!is.null(season_start) && is.null(only_current_season)) {
    coll$push("If season_start is assigned only_current_season must also be assigned")
  }
  checkmate::reportAssertions(coll)

  # Throw an error if any of the inputs are not supported
  family <- rlang::arg_match(family)

  # Add the seasons to tsd if available
  if (!is.null(season_start)) {
    tsd <- tsd |> dplyr::mutate(season = epi_calendar(.data$time, start = season_start, end = season_end))
  } else {
    tsd <- tsd |> dplyr::mutate(season = "not_defined")
  }

  # Extract only current season if assigned
  if (!is.null(season_start) && only_current_season == TRUE) {
    seasons <- tsd |>
      dplyr::distinct(.data$season) |>
      dplyr::pull(.data$season)

    # If two or more seasons exist, take the last two
    if (length(seasons) >= 2) {
      seasons <- utils::tail(seasons, n = 2)
      prev_season <- seasons[1]
      current_season <- seasons[2]
    } else {
      prev_season <- NA_character_
      current_season <- seasons[1]
    }

    # Create the combined data frame:
    tsd <- dplyr::bind_rows(
      # If a previous season exists, use its last k-1 rows
      if (!is.na(prev_season)) {
        tsd |>
          dplyr::filter(.data$season == prev_season) |>
          dplyr::slice_tail(n = k - 1)
      } else {
        tibble::tibble()
      },
      # Bind all rows from the current season
      tsd |>
        dplyr::filter(.data$season == current_season)
    )
  }

  # Extract the length of the series
  n <- base::nrow(tsd)

  # Allocate space for growth rate estimates
  res <- tibble::tibble()
  skipped_window <- base::rep(FALSE, base::nrow(tsd))

  for (i in k:n) {
    # Index observations for this iteration
    obs_iter <- tsd[(i - k + 1):i, ]

    # Evaluate NA and zero values in windows
    if (sum(is.na(obs_iter) | obs_iter == 0) > k * na_fraction_allowed) {
      skipped_window[i] <- TRUE
      # Set fields to NA since the window is skipped
      growth_rates <- list(estimate = c(NA, NA, NA),
                           fit = list(converged = FALSE))
    } else {
      # Estimate growth rates
      growth_rates <- fit_growth_rate(
        observations = obs_iter$observation,
        pop = if (isTRUE(use_offset) && "pop" %in% names(tsd)) obs_iter$pop else NULL,
        level = level,
        family = family
      )
    }

    # See if the growth rate is significantly higher than zero
    growth_warning <- growth_rates$estimate[2] > 0

    # Calculate Sum of Cases (sum_of_cases)
    sum_of_cases <- base::sum(obs_iter$observation, na.rm = TRUE)

    # Evaluate if sum_of_cases exceeds disease_threshold
    sum_of_cases_warning <- sum_of_cases > (disease_threshold * k)

    # Give an seasonal_onset_alarm if both criteria are met
    seasonal_onset_alarm <- growth_warning & sum_of_cases_warning

    # Collect the results
    res <- dplyr::bind_rows(
      res,
      tibble::tibble(
        reference_time = tsd$time[i],
        observation = tsd$observation[i],
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

  if (!is.na(disease_threshold)) {
    # Extract seasons from onset_output and create seasonal_onset
    res <- res |>
      dplyr::mutate(
        onset_flag = cumsum(.data$seasonal_onset_alarm),
        seasonal_onset = .data$onset_flag == 1 & !duplicated(.data$onset_flag),
        .by = "season"
      ) |>
      dplyr::select(!"onset_flag")
  }

  # Turn the results into an `seasonal_onset` class
  ans <- tibble::new_tibble(
    x = res,
    class = "tsd_onset",
    k = k,
    level = level,
    disease_threshold = disease_threshold,
    family = family
  )

  # Keep attributes from the `tsd` class
  attr(ans, "time_interval") <- attr(tsd, "time_interval")

  return(ans)
}

#' Deprecated aedseo function
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function has been renamed to better reflect its purpose.
#' Please use `seasonal_onset()` instead.
#' @param ... Arguments passed to `seasonal_onset()`
#' @keywords internal
#' @export
aedseo <- function(...) {
  lifecycle::deprecate_warn("0.1.2", "aedseo()", "seasonal_onset()")
  seasonal_onset(...)
}
