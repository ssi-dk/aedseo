#' Compute seasonal onset and burden levels from seasonal time series observations.
#'
#' @description
#'
#' This function performs automated and early detection of seasonal epidemic onsets and estimates the burden
#' levels from time series dataset stratified by season. The seasonal onset estimates growth rates for consecutive
#' time intervals and calculates the average sum of cases/incidence in consecutive time intervals (`k`).
#' The burden levels use the previous seasons to estimate the levels of the current season.
#' Output will be in incidence if `population` and `incidence` are assigned in input.
#'
#' @inheritParams seasonal_burden_levels
#' @inheritParams seasonal_onset
#' @param disease_threshold `r rd_disease_threshold(usage = "combined")`
#' @param family `r rd_family(usage = "combined")`
#' @param family_quant A character string specifying the family for modeling burden levels.
#' @param multiple_waves A logical. Should the output contain multiple waves?
#' @param burden_level_decrease A character specifying the burden breakpoint the observations should decrease under
#' before a new increase in observations can call a new wave onset if seasonal onset criteria are met.
#' Choose between; "very low", "low", "medium", or "high".
#' @param steps_with_decrease An integer specifying in how many time steps (days, weeks, months) the decrease
#' should be observed (if there is a sudden decrease followed by an increase it could e.g. be due to testing).
#' If multiple_waves are assigned steps_with_decrease defaults to 2.
#' @param ... Arguments passed to `seasonal_burden_levels()`, `fit_percentiles()` and `seasonal_onset()` functions.
#'
#' @return An object containing two lists: onset_output and burden_output:
#'
#' onset_output:
#' `r rd_seasonal_onset_return`
#'
#' If multiple waves is selected the `tsd_onset` object will also contain:
#' - 'wave_number': The wave number in the time series data.
#' - 'wave_starts': Logical. Did a new wave start?
#' - 'wave_ends': Logical. Did the wave end?
#' - 'decrease_counter': How many consecutive time intervals have decreased below the selected burden breakpoint.
#' - 'decrease_level': A character specifying the selected burden breakpoint to fall below for ending the wave.
#' - 'decrease_value': A numeric specifying the selected burden breakpoint to fall below for ending the wave.
#'
#' burden_output:
#' `r rd_seasonal_burden_levels_return`
#'
#' @export
#'
#' @examples
#' # Generate random flu season
#' generate_flu_season <- function(start = 1, end = 1000) {
#'   random_increasing_obs <- round(sort(runif(24, min = start, max = end)))
#'   random_decreasing_obs <- round(rev(random_increasing_obs))
#'
#'   # Generate peak numbers
#'   add_to_max <- c(50, 100, 200, 100)
#'   peak <- add_to_max + max(random_increasing_obs)
#'
#'   # Combine into a single observations sequence
#'   observations <- c(random_increasing_obs, peak, random_decreasing_obs)
#'
#'  return(observations)
#' }
#'
#' season_1 <- generate_flu_season()
#' season_2 <- generate_flu_season()
#'
#' start_date <- as.Date("2022-05-29")
#' end_date <- as.Date("2024-05-20")
#'
#' weekly_dates <- seq.Date(from = start_date,
#'                          to = end_date,
#'                          by = "week")
#'
#' tsd_data <- tsd(
#'   cases = c(season_1, season_2),
#'   time = as.Date(weekly_dates)
#' )
#'
#' # Run the main function
#' combined_data <- combined_seasonal_output(tsd_data)
#' # Print seasonal onset results
#' print(combined_data$onset_output)
#' # Print burden level results
#' print(combined_data$burden_output)
combined_seasonal_output <- function(         # nolint: cyclocomp_linter.
  tsd,
  disease_threshold = 20,
  family = c(
    "quasipoisson",
    "poisson"
  ),
  family_quant = c(
    "lnorm",
    "weibull",
    "exp"
  ),
  season_start = 21,
  season_end = season_start - 1,
  only_current_season = TRUE,
  multiple_waves = FALSE,
  burden_level_decrease = NULL,
  steps_with_decrease = NULL,
  ...
) {
  coll <- checkmate::makeAssertCollection()
  checkmate::assert_logical(multiple_waves, add = coll)
  checkmate::assert_character(burden_level_decrease, null.ok = TRUE, add = coll)
  checkmate::assert_choice(burden_level_decrease,
                           choices = c("very low", "low", "medium", "high"),
                           null.ok = TRUE,
                           add = coll)
  checkmate::assert_integerish(steps_with_decrease, lower = 1, null.ok = TRUE, add = coll)
  if (!multiple_waves && !is.null(burden_level_decrease)) {
    coll$push("burden_level_decrease is not used unless multiple_waves is TRUE.")
  }
  if (!multiple_waves && !is.null(steps_with_decrease)) {
    coll$push("steps_with_decrease is not used unless multiple_waves is TRUE.")
  }
  if (multiple_waves && is.null(burden_level_decrease)) {
    coll$push("burden_level_decrease must be assigned if multiple_waves is TRUE")
  }
  if (multiple_waves && is.null(steps_with_decrease)) {
    warning("steps_with_decrease is by default set to 2")
    steps_with_decrease <- 2L
  }
  checkmate::reportAssertions(coll)

  # Capture all extra arguments
  extra_args <- list(...)

  # Get the allowed arguments for seasonal_burden_levels() and/or fit_percentiles()
  burden_allowed <- union(names(formals(seasonal_burden_levels)), names(formals(fit_percentiles)))
  burden_args <- extra_args[names(extra_args) %in% burden_allowed]

  # Get the allowed arguments for seasonal_onset()
  onset_allowed <- names(formals(seasonal_onset))
  onset_args <- extra_args[names(extra_args) %in% onset_allowed]

  # Run the models
  onset_output <- do.call(
    seasonal_onset,
    c(list(tsd = tsd, disease_threshold = disease_threshold, family = family,
           season_start = season_start, season_end = season_end, only_current_season = only_current_season),
      onset_args)
  )   # nolint: object_usage_linter.

  burden_output <- do.call(
    seasonal_burden_levels,
    c(list(tsd = tsd, season_start = season_start, season_end = season_end,
           disease_threshold = disease_threshold, family = family_quant, only_current_season = only_current_season),
      burden_args)
  )

  # Add multiple waves if assigned in input
  if (multiple_waves) {

    # Iterate over onset_output
    wave_fun <- function(onset_data, steps_with_decrease) {

      # Define observation based on input data
      if (!is.na(all(onset_data$incidence))) {
        onset_data <- onset_data |>
          dplyr::mutate(observation = .data$incidence)
      } else {
        onset_data <- onset_data |>
          dplyr::mutate(observation = .data$cases)
      }

      # Initialise
      in_wave <- FALSE
      wave_count <- 0

      for (i in seq_len(nrow(onset_data))) {
        # Not currently in a wave, look for a wave start signal:
        if (!in_wave) {
          if (isTRUE(onset_data$seasonal_onset_alarm[i])) {
            onset_data$wave_number[i] <- wave_count + 1 # Assign which wave
            onset_data$wave_starts[i] <- TRUE  # Mark the beginning of a wave
            in_wave <- TRUE
            wave_count <- wave_count + 1
          }
        } else {
          onset_data$wave_number[i] <- wave_count
          onset_data$wave_starts[i] <- FALSE
        }
        # Check if the current observation is decreasing compared to the previous row
        # and falls below the decrease_below threshold.
        prev_obs <- if (i > 1) onset_data$observation[i - 1] else NA_real_
        if (!is.na(prev_obs) && onset_data$observation[i] < prev_obs &&
              onset_data$observation[i] < onset_data$decrease_value[i]) {
          onset_data$decrease_counter[i] <- onset_data$decrease_counter[i - 1] + 1
        }
        # If the number of consecutive decreasing steps reaches `steps_with_decrease`, end the current wave.
        if (in_wave && onset_data$decrease_counter[i] >= steps_with_decrease) {
          onset_data$wave_ends[i] <- TRUE
          in_wave <- FALSE
        }
      }
      onset_data <- onset_data |>
        dplyr::select(-"observation")
    }

    # Add new columns for wave_number, wave_starts and decrease_counter
    onset_output <- onset_output |>
      dplyr::mutate(
        wave_number = NA_real_,
        wave_starts = FALSE,
        wave_ends = FALSE,
        decrease_counter = 0
      )

    if (only_current_season) {
      decrease_below <- burden_output$values[[burden_level_decrease]]
      onset_and_decrease_level <- onset_output |>
        dplyr::mutate(
          decrease_level = burden_level_decrease,
          decrease_value = decrease_below
        )

      onset_output <- wave_fun(
        onset_data = onset_and_decrease_level,
        steps_with_decrease = steps_with_decrease
      )
    } else {
      burden_levels <- purrr::map_dfr(burden_output, ~ {
        tibble::tibble(
          season = .x$season,
          values = .x$values
        ) |>
          tidyr::unnest_longer(
            col = values,
            indices_to = "decrease_level",
            values_to = "value"
          ) |>
          dplyr::mutate(
            decrease_value = as.numeric(value)
          ) |>
          dplyr::filter(.data$decrease_level == burden_level_decrease)
      })
      onset_and_decrease_level <- onset_output |>
        dplyr::right_join(burden_levels, by = "season")

      onset_output <- wave_fun(
        onset_data = onset_and_decrease_level,
        steps_with_decrease = steps_with_decrease
      )
    }
  }

  # Combine both results in lists
  seasonal_output <- list(
    onset_output = onset_output,
    burden_output = burden_output
  )

  # Assign a class for the combined results
  class(seasonal_output) <- "tsd_onset_and_burden"

  return(seasonal_output)
}
