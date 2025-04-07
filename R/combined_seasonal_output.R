#' Compute seasonal onset and burden levels from seasonal time series observations.
#'
#' @description
#'
#' This function performs automated and early detection of seasonal epidemic onsets and estimates the burden
#' levels from time series dataset stratified by season. The seasonal onset estimates growth rates for consecutive
#' time intervals and calculates the sum of cases. The burden levels use the previous seasons to estimate the levels
#' of the current season.
#' @inheritParams seasonal_burden_levels
#' @inheritParams seasonal_onset
#' @param disease_threshold `r rd_disease_threshold(usage = "combined")`
#' @param family `r rd_family(usage = "combined")`
#' @param family_quant A character string specifying the family for modeling burden levels.
#' @param multiple_waves A logical. Should the output contain multiple seasonal onsets?
#' @param burden_level_decrease A character specifying the burden breakpoint the observations should decrease under
#' before a new increase in observations can call a new seasonal onset if seasonal onset criteria are met.
#' Choose between; "very low", "low", "medium", or "high".
#' @param steps_with_decrease An integer specifying in how many time steps (days, weeks, months) the decrease
#' should be observed (if there is a sudden decrease followed by an increase it could e.g. be due to testing).
#' If multiple_waves are assigned steps_with_decrease defaults to 1.
#' @param ... Arguments passed to `seasonal_burden_levels()`, `fit_percentiles()` and `seasonal_onset()` functions.
#'
#' @return An object containing two lists: onset_output and burden_output:
#'
#' onset_output:
#' `r rd_seasonal_onset_return`
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
#'   observation = c(season_1, season_2),
#'   time = as.Date(weekly_dates),
#'   time_interval = "week"
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
    "poisson",
    "quasipoisson"
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
    warning("steps_with_decrease is by default set to 1")
    steps_with_decrease <- 1
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
  burden_output <- do.call(
    seasonal_burden_levels,
    c(list(tsd = tsd, season_start = season_start, season_end = season_end,
           disease_threshold = disease_threshold, family = family_quant, only_current_season = only_current_season),
      burden_args)
  )

  onset_output <- do.call(
    seasonal_onset,
    c(list(tsd = tsd, disease_threshold = disease_threshold, family = family,
           season_start = season_start, season_end = season_end, only_current_season = only_current_season),
      onset_args)
  )   # nolint: object_usage_linter.

  # Add multiple waves if assigned in input
  if (multiple_waves && isTRUE(only_current_season)) {
    decrease_below <- burden_output$values[[burden_level_decrease]]

    # Add new columns for wave_number, wave_starts and decrease_counter
    onset_output <- onset_output |>
      dplyr::mutate(
        wave_number = NA_integer_,
        wave_starts = FALSE,
        decrease_counter = NA_integer_
      )

    in_wave <- FALSE      # Are we currently in a wave?
    wave_counter <- 0     # Wave counter
    decrease_counter <- 0 # Counter for consecutive steps where decrease is observed

    # Iterate over onset_output
    for (i in seq_len(nrow(onset_output))) {
      if (!in_wave) {
        # Not currently in a wave, look for a wave start signal:
        if (isTRUE(onset_output$seasonal_onset_alarm[i])) {
          wave_counter <- wave_counter + 1
          in_wave <- TRUE
          onset_output$wave_number[i] <- wave_counter
          onset_output$wave_starts[i] <- TRUE  # Mark the beginning of a wave
          decrease_counter <- 0  # Reset the decrease counter at wave start
          onset_output$decrease_counter[i] <- decrease_counter
        }
      } else {
        # If already in a wave, assign the current wave number to this row
        onset_output$wave_number[i] <- wave_counter

        # Check if the current observation is decreasing compared to the previous row
        # and falls below the decrease_below threshold.
        if (onset_output$observation[i] < onset_output$observation[i - 1] &&
              onset_output$observation[i] < decrease_below) {
          decrease_counter <- decrease_counter + 1
          onset_output$decrease_counter[i] <- decrease_counter
        } else {
          # Reset the counter if decrease does not persist
          decrease_counter <- 0
          onset_output$decrease_counter[i] <- decrease_counter
        }

        # If the number of consecutive decreasing steps reaches the specified threshold end the current wave.
        if (decrease_counter == steps_with_decrease) {
          in_wave <- FALSE
          onset_output$decrease_counter[i] <- decrease_counter
          decrease_counter <- 0  # Reset counter for next wave detection
        }
      }
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
