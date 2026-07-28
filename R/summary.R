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
#'   only_current_season = TRUE
#' )
#' # Print the summary
#' summary(tsd_onset)
summary.tsd_onset <- function(object, ...) {
  checkmate::assert_class(object, "tsd_onset")

  # Report observations on the scale used by the fitted model.
  model_outcome <- attr(object, "model_outcome")
  observation_column <- if (identical(model_outcome, "proportion")) {
    "proportion"
  } else if (identical(model_outcome, "incidence")) {
    "incidence"
  } else {
    "cases"
  }

  # Extract the last observation
  last_observation <- dplyr::last(object)

  # Extract the reference time
  reference_time <- last_observation$reference_time

  # Extract attributes
  time_interval <- attr(object, "time_interval")
  incidence_denominator <- attr(object, "incidence_denominator")

  # Extract the season
  last_season <- last_observation$season

  # Latest observation
  latest_observation <- as.numeric(last_observation[[observation_column]])

  # Latest average of observations in window
  latest_average_observations_window <- last_observation |>
    dplyr::pull(.data$average_observations_window)

  # Latest average of observations warning
  latest_average_observations_warning <- object |>
    dplyr::filter(.data$average_observations_warning == TRUE) |>
    dplyr::summarise(
      latest_average_observations_warning = dplyr::last(reference_time)
    ) |>
    dplyr::pull(latest_average_observations_warning)

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

    if (nrow(seasonal_onset_ref_obs) == 0) {
      seasonal_onset_ref_time <- NA_character_
      seasonal_onset_obs <- NA_real_
      seasonal_onset_sum_obs <- NA_character_
      seasonal_onset_gr <- NA_real_
      seasonal_onset_upper_gr <- NA_real_
      seasonal_onset_lower_gr <- NA_real_
    } else {
      seasonal_onset_ref_time <- as.character(seasonal_onset_ref_obs$reference_time)
      seasonal_onset_obs <- as.numeric(seasonal_onset_ref_obs[[observation_column]])
      seasonal_onset_sum_obs <- as.character(seasonal_onset_ref_obs$average_observations_window)
      seasonal_onset_gr <- seasonal_onset_ref_obs$growth_rate
      seasonal_onset_upper_gr <- seasonal_onset_ref_obs$upper_growth_rate
      seasonal_onset_lower_gr <- seasonal_onset_ref_obs$lower_growth_rate
    }
  }

  offset_block <- ""
  if ("seasonal_offset" %in% names(object) && !is.na(disease_threshold)) {
    # Extract first seasonal offset if threshold was given
    seasonal_offset_ref_obs <- object |>
      dplyr::filter(.data$season == last_season) |>
      dplyr::filter(.data$seasonal_offset == TRUE)

    if (nrow(seasonal_offset_ref_obs) > 0) {
      offset_block <- sprintf(
        "Reference-offset time point (first seasonal offset alarm in season): %s
      Observations at reference-offset time point: %g
      Average observations (in k window) at reference-offset time point: %s ",
        as.character(seasonal_offset_ref_obs$reference_time),
        as.numeric(seasonal_offset_ref_obs[[observation_column]]),
        as.character(seasonal_offset_ref_obs$average_observations_window)
      )
    }
  }

  # Generate the summary message
  if (is.na(disease_threshold)) {
    summary_message <- sprintf(
      "Summary of tsd_onset object without disease_threshold

      Model output:
        Reference time point (last case in series): %s
        Observations at reference time point: %g
        Average observations (in k window) at reference time point: %g
        Total number of growth warnings in the series: %d
        Latest growth warning: %s
        Growth rate estimate at reference time point:
          Estimate   Lower (%.1f%%)   Upper (%.1f%%)
            %.3f     %.3f          %.3f

      The season for reference time point:
        %s

      Model settings:
        Called using distributional family: %s
        Window size: %d
        The time interval for the observations: %s
        Disease specific threshold: %g
        Incidence denominator: %g",
      as.character(reference_time),
      latest_observation,
      as.numeric(latest_average_observations_window),
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
      disease_threshold,
      incidence_denominator
    )
  } else {
    # Generate the summary message
    summary_message <- sprintf(
      "Summary of tsd_onset object with disease_threshold

      Model output:
        Reference time point (first seasonal onset alarm in season): %s
        Observations at reference time point: %g
        Average observations (in k window) at reference time point: %s
        Growth rate estimate at reference time point:
          Estimate   Lower (%.1f%%)   Upper (%.1f%%)
            %.3f     %.3f          %.3f
        %s
        Total number of growth warnings in the series: %d
        Latest growth warning: %s
        Latest average observations warning: %s
        Latest seasonal onset alarm: %s

      The season for reference time point:
        %s

      Model settings:
        Called using distributional family: %s
        Window size: %d
        The time interval for the observations: %s
        Disease specific threshold: %g
        Incidence denominator: %g",
      seasonal_onset_ref_time,
      seasonal_onset_obs,
      seasonal_onset_sum_obs,
      lower_confidence_interval * 100,
      upper_confidence_interval * 100,
      seasonal_onset_gr,
      seasonal_onset_lower_gr,
      seasonal_onset_upper_gr,
      offset_block,
      sum_of_growth_warnings,
      as.character(latest_growth_warning),
      as.character(latest_average_observations_warning),
      as.character(latest_seasonal_onset_alarm),
      last_season,
      family,
      k,
      time_interval,
      disease_threshold,
      incidence_denominator
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

    Breakpoint estimates:
      very low : %f
      low: %f
      medium: %f
      high: %f

    The season for the burden levels:
      %s

    Model settings:
      Disease specific threshold: %g
      Incidence denominator: %g
      Called using distributional family: %s",
    object$values["very low"],
    object$values["low"],
    object$values["medium"],
    object$values["high"],
    object$season,
    object$disease_threshold,
    object$incidence_denominator,
    object$optim$family
  )

  cat(summary_message)
}
