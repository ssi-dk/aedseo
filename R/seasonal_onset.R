#' Automated and Early Detection of Seasonal Epidemic Onset
#'
#' @description
#'
#' This function performs automated and early detection of seasonal epidemic onsets on a `tsd` object.
#' It estimates growth rates and calculates the average sum of cases in consecutive time intervals (`k`).
#' If the time series data includes `population` it will be used as offset to adjust the growth rate in the glm,
#' additionally the output will include incidence, population and average sum of incidence.
#'
#' @param tsd `r rd_tsd`
#' @param k An integer specifying the window size for modeling growth rates and average sum of cases.
#' @param level The confidence level for onset parameter estimates, a numeric value between 0 and 1.
#' @param disease_threshold `r rd_disease_threshold(usage = "onset")`
#' @param family `r rd_family()`
#' @param na_fraction_allowed Numeric value between 0 and 1 specifying the fraction of observations in the window
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
#'   cases = c(100, 120, 150, 180, 220, 270),
#'   time = seq(from = as.Date("2023-01-01"), by = "1 week", length.out = 6)
#' )
#'
#' # Estimate seasonal onset with a 3-day window
#' seasonal_onset(
#'   tsd = tsd_data,
#'   k = 3,
#'   level = 0.975,
#'   disease_threshold = 5,
#'   na_fraction_allowed = 0.4,
#'   season_start = 21,
#'   season_end = 20,
#'   only_current_season = FALSE
#' )
seasonal_onset <- function(                                     # nolint: cyclocomp_linter.
  tsd,
  k = 5,
  level = 0.95,
  disease_threshold = NA_real_,
  family = c(
    "quasipoisson",
    "poisson"
    # TODO: #10 Include negative.binomial regressions. @telkamp7
  ),
  na_fraction_allowed = 0.4,
  season_start = NULL,
  season_end = season_start - 1,
  only_current_season = NULL
) {
  # Check input arguments
  coll <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(tsd, add = coll)
  checkmate::assert_class(tsd, "tsd", add = coll)
  checkmate::assert_names(
    colnames(tsd),
    must.include = c("time", "cases"),
    subset.of = c("time", "cases", "incidence", "population"),
    add = coll
  )
  checkmate::assert_numeric(level, lower = 0, upper = 1, add = coll)
  checkmate::assert_numeric(na_fraction_allowed, lower = 0, upper = 1,
                            add = coll)
  checkmate::assert_integerish(k, add = coll)
  checkmate::assert_numeric(disease_threshold, add = coll)
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

  # Add the seasons to tsd if available
  if (!is.null(season_start)) {
    tsd <- tsd |> dplyr::mutate(season = epi_calendar(.data$time, start = season_start, end = season_end))
  } else {
    tsd <- tsd |> dplyr::mutate(season = "not_defined")
  }

  # Use incidence if in tsd else use cases
  use_incidence <- FALSE
  if ("population" %in% names(tsd) && "incidence" %in% names(tsd)) {
    use_incidence <- TRUE
  }

  # Define observation as cases in `tsd`.
  tsd <- tsd |>
    dplyr::mutate(observation = .data$cases)

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
    if (sum(is.na(obs_iter$observation) | obs_iter$observation == 0) > k * na_fraction_allowed) {
      skipped_window[i] <- TRUE
      # Set fields to NA since the window is skipped
      growth_rates <- list(estimate = c(NA, NA, NA),
                           fit = list(converged = FALSE))
    } else {
      # Estimate growth rates
      growth_rates <- fit_growth_rate(
        cases = obs_iter$observation,
        population = if ("population" %in% names(tsd)) obs_iter$population else NULL,
        level = level,
        family = family
      )
    }

    # See if the growth rate is significantly higher than zero
    growth_warning <- growth_rates$estimate[2] > 0

    if (use_incidence) {
      # Calculate average incidence in window (k)
      average_observations_window <- base::sum(obs_iter$incidence, na.rm = TRUE) / k
    } else {
      # Calculate average cases in window (k)
      average_observations_window <- base::sum(obs_iter$cases, na.rm = TRUE) / k
    }
    # Evaluate if average_incidence_window exceeds disease_threshold
    average_observations_warning <- average_observations_window > disease_threshold

    # Give a seasonal_onset_alarm if both criteria are met
    seasonal_onset_alarm <- growth_warning & average_observations_warning

    # Collect the results
    res <- dplyr::bind_rows(
      res,
      tibble::tibble(
        reference_time = tsd$time[i],
        cases = tsd$cases[i],
        season = tsd$season[i],
        population = if ("population" %in% names(tsd)) tsd$population[i] else NA,
        incidence = if ("incidence" %in% names(tsd)) tsd$incidence[i] else NA,
        growth_rate = growth_rates$estimate[1],
        lower_growth_rate = growth_rates$estimate[2],
        upper_growth_rate = growth_rates$estimate[3],
        growth_warning = growth_warning,
        average_observations_window = average_observations_window,
        average_observations_warning = average_observations_warning,
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
    family = family,
    incidence_denominator = attr(tsd, "incidence_denominator")
  )

  # Keep attributes from the `tsd` class
  attr(ans, "time_interval") <- attr(tsd, "time_interval")
  attr(ans, "incidence_denominator") <- attr(tsd, "incidence_denominator")

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
