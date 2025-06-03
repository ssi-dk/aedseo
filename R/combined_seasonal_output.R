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
combined_seasonal_output <- function(
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
  ...
) {

  # Capture all extra arguments
  extra_args <- list(...)

  # Get the allowed arguments for seasonal_burden_levels() and/or fit_percentiles()
  burden_allowed <- union(names(formals(seasonal_burden_levels)), names(formals(fit_percentiles)))
  burden_args <- extra_args[names(extra_args) %in% burden_allowed]

  # Get the allowed arguments for seasonal_onset()
  onset_allowed <- names(formals(seasonal_onset))
  onset_args <- extra_args[names(extra_args) %in% onset_allowed]

  onset_output <- do.call(
    seasonal_onset,
    c(list(tsd = tsd, disease_threshold = disease_threshold, family = family,
           season_start = season_start, season_end = season_end, only_current_season = only_current_season),
      onset_args)
  )   # nolint: object_usage_linter.

  # Run the models
  burden_output <- do.call(
    seasonal_burden_levels,
    c(list(tsd = tsd, season_start = season_start, season_end = season_end,
           disease_threshold = disease_threshold, family = family_quant, only_current_season = only_current_season),
      burden_args)
  )

  # Combine both results in lists
  seasonal_output <- list(
    onset_output = onset_output,
    burden_output = burden_output
  )

  # Assign a class for the combined results
  class(seasonal_output) <- "tsd_onset_and_burden"

  return(seasonal_output)
}
