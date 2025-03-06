#' Summary method for `tsd_onset` objects
#'
#' @description
#' Summarize key results from a seasonal onset analysis.
#'
#' @param object An object of class 'tsd_onset'
#' containing the results of a `seasonal_onset` analysis.
#' @param ... Additional arguments (not used).
#'
#' @return This function is used for its side effect, which is printing a summary message to the console.
#'
#' @export
#'
#' @examples
#' # Create a `tsd` object
#' tsd_data <- generate_seasonal_data()
#'
#' # Create a `tsd_onset` object
#' tsd_onset <- seasonal_onset(
#'   tsd = tsd_data,
#'   k = 3,
#'   disease_threshold = 100,
#'   season_start = 21,
#'   season_end = 20,
#'   level = 0.95,
#'   family = "poisson",
#'   only_current_season = TRUE
#' )
#' # Print the summary
#' summary(tsd_onset)
summary.tsd_onset <- function(object, ...) {
  checkmate::assert_class(object, "tsd_onset")

  # Extract the last observation
  last_observation <- dplyr::last(object)

  # Extract the reference time
  reference_time <- last_observation$reference_time

  # Extract the time_interval
  time_interval <- attr(object, "time_interval")

  # Extract the season
  last_season <- last_observation$season

  # Latest sum of cases
  latest_sum_of_cases <- last_observation |>
    dplyr::pull(.data$sum_of_cases)

  # Latest cases
  latest_cases <- last_observation |>
    dplyr::pull(.data$observation)

  # Latest sum of cases warning
  latest_sum_of_cases_warning <- object |>
    dplyr::filter(.data$sum_of_cases_warning == TRUE) |>
    dplyr::summarise(
      latest_sum_of_cases_warning = dplyr::last(reference_time)
    ) |>
    dplyr::pull(latest_sum_of_cases_warning)

  # Latest growth warning
  latest_growth_warning <- object |>
    dplyr::filter(.data$growth_warning == TRUE) |>
    dplyr::summarise(latest_growth_warning = dplyr::last(reference_time)) |>
    dplyr::pull(latest_growth_warning)

  # Latest growth warning
  latest_seasonal_onset_alarm <- object |>
    dplyr::filter(.data$seasonal_onset_alarm == TRUE) |>
    dplyr::summarise(
      latest_seasonal_onset_alarm = dplyr::last(reference_time)
    ) |>
    dplyr::pull(latest_seasonal_onset_alarm)

  # Calculate the total number of growth warnings
  sum_of_growth_warnings <- object |>
    dplyr::filter(.data$growth_warning == TRUE) |>
    dplyr::summarise(sum_of_growth_warnings = sum(.data$growth_warning)) |>
    dplyr::pull(sum_of_growth_warnings)

  # Extract the attributes from the object
  attributes_object <- attributes(object)

  # Extract the object k, level, and family
  k <- attributes_object$k
  level <- attributes_object$level
  disease_threshold <- attributes_object$disease_threshold
  family <- attributes_object$family

  # Extract the lower and upper confidence intervals
  lower_confidence_interval <- (1 - level) / 2
  upper_confidence_interval <- level + lower_confidence_interval

  # Extract first seasonal onset if threshold was given
  if (!is.na(disease_threshold)) {
    seasonal_onset_ref_obs <- object |>
      dplyr::filter(.data$season == last_season) |>
      dplyr::filter(.data$seasonal_onset == TRUE)

    seasonal_onset_ref_time <- as.character(
      seasonal_onset_ref_obs |>
        dplyr::pull(.data$reference_time)
    )
    seasonal_onset_obs <- as.character(
      seasonal_onset_ref_obs |>
        dplyr::pull(.data$observation)
    )
    seasonal_onset_sum_obs <- as.character(
      seasonal_onset_ref_obs |>
        dplyr::pull(.data$sum_of_cases)
    )
    seasonal_onset_gr <- seasonal_onset_ref_obs |>
      dplyr::pull(.data$growth_rate)

    seasonal_onset_upper_gr <- seasonal_onset_ref_obs |>
      dplyr::pull(.data$upper_growth_rate)

    seasonal_onset_lower_gr <- seasonal_onset_ref_obs |>
      dplyr::pull(.data$lower_growth_rate)
  }

  # Generate the summary message
  if (is.na(disease_threshold)) {
    summary_message <- sprintf(
      "Summary of tsd_onset object without disease_threshold

      Reference time point (last observation in series): %s

      Observations at reference time point: %s

      Sum of cases at reference time point: %d

      Total number of growth warnings in the series: %d

      Latest growth warning: %s

      Growth rate estimate at reference time point:
        Estimate   Lower (%.1f%%)   Upper (%.1f%%)
          %.3f     %.3f          %.3f

      The last season in the series: %s

      Called using distributional family: %s

      Window size for growth rate estimation and
      calculation of sum of cases: %d

      The time interval for the observations: %s

      Disease specific threshold: %d",
      as.character(reference_time),
      as.character(latest_cases),
      as.integer(latest_sum_of_cases),
      sum_of_growth_warnings,
      as.character(latest_growth_warning),
      lower_confidence_interval * 100,
      upper_confidence_interval * 100,
      last_observation$growth_rate,
      last_observation$lower_growth_rate,
      last_observation$upper_growth_rate,
      last_season,
      family,
      k,
      time_interval,
      disease_threshold
    )
  } else {
    # Generate the summary message
    summary_message <- sprintf(
      "Summary of tsd_onset object with disease_threshold

      Reference time point (first seasonal onset alarm in season): %s

      Observations at reference time point: %s

      Sum of cases at reference time point: %s

      Growth rate estimate at reference time point:
        Estimate   Lower (%.1f%%)   Upper (%.1f%%)
          %.3f     %.3f          %.3f

      Season for reference time point: %s

      Called using distributional family: %s

      Window size for growth rate estimation and
      calculation of sum of cases: %d

      The time interval for the observations: %s

      Disease specific threshold: %d

      Total number of growth warnings in the series: %d

      Latest growth warning: %s

      Latest sum of cases warning: %s

      Latest seasonal onset alarm: %s",
      seasonal_onset_ref_time,
      seasonal_onset_obs,
      seasonal_onset_sum_obs,
      lower_confidence_interval * 100,
      upper_confidence_interval * 100,
      seasonal_onset_gr,
      seasonal_onset_upper_gr,
      seasonal_onset_lower_gr,
      last_season,
      family,
      k,
      time_interval,
      disease_threshold,
      sum_of_growth_warnings,
      as.character(latest_growth_warning),
      as.character(latest_sum_of_cases_warning),
      as.character(latest_seasonal_onset_alarm)
    )
  }

  # Print the summary message
  cat(summary_message)
}
#' Summary method for `tsd_burden_levels` objects
#'
#' @description
#' Summarize key results from a seasonal burden levels analysis.
#'
#' @param object An object of class 'tsd_burden_levels'
#' containing the results of a `seasonal_burden_levels` analysis.
#' @param ... Additional arguments (not used).
#'
#' @return This function is used for its side effect, which is printing the burden levels.
#'
#' @export
#'
#' @examples
#' # Create a `tsd` object
#' tsd_data <- generate_seasonal_data()
#'
#' # Create a `tsd_burden_levels` object
#' tsd_burden_levels <- seasonal_burden_levels(
#'   tsd = tsd_data
#' )
#' # Print the summary
#' summary(tsd_burden_levels)
summary.tsd_burden_levels <- function(object, ...) {
  checkmate::assert_class(object, "tsd_burden_levels")

  # Extract data
  if (all(sapply(object, is.list))) {
    object <- dplyr::last(unclass(object))
  }

  # Generate the summary message
  summary_message <- sprintf(
    "Summary of tsd_burden_levels object

    Breakpoint (very low): %f

    Breakpoint (low): %f

    Breakpoint (medium): %f

    Breakpoint (high): %f

    The season for the burden levels: %s

    Disease specific threshold:%d

    Called using distributional family: %s",

    object$values["very low"],
    object$values["low"],
    object$values["medium"],
    object$values["high"],
    object$season,
    object$disease_threshold,
    object$optim$family
  )

  cat(summary_message)
}
