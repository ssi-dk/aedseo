#' Summarises estimates like seasonal peak and onset from all available seasons
#'
#' @description
#'
#' This function summarises peak timing and seasonal onset from estimates in a `tsd_onset` object.
#' This can be useful for investigating if the current season falls within estimates from previous seasons
#' or if it is very distinct from previous seasons.
#'
#' Uses data from a `tsd_onset` object (output from `seasonal_onset()`).
#'
#' `seasonal_onset()` has to be run with arguments;
#'  - disease_threshold
#'  - season_start
#'  - season_end
#'  - only_current_season = FALSE
#'
#' @param onset_output A `tsd_onset` object returned from `seasonal_onset()`.
#'
#' @return An object of class `historical_summary`, containing:
#'  - Usual time to seasonal peak (weeks after onset)
#'  - The week in which the peak usually falls
#'  - Usual peak intensity
#'  - The week in which the onset usually falls
#'  - Usual onset intensity and growth rate estimates
#'  If the season does not have an onset, it will not be included in the summary.
#'
#' @export
#'
#' @examples
#' # Generate simulated data of seasonal waves
#' sim_data <- generate_seasonal_data(
#'   years = 5,
#'   start_date = as.Date("2022-05-26"),
#'   trend_rate = 1.002,
#'   noise_overdispersion = 1.1
#' )
#'
#' # Estimate seasonal onset
#' tsd_onset <- seasonal_onset(
#'   tsd = sim_data,
#'   disease_threshold = 20,
#'   family = "quasipoisson",
#'   season_start = 21,
#'   season_end = 20,
#'   only_current_season = FALSE
#' )
#'
#' # Get historical summary
#' historical_summary(tsd_onset)
historical_summary <- function(
  onset_output
) {
  # Check input arguments
  coll <- checkmate::makeAssertCollection()
  checkmate::assert_class(onset_output, "tsd_onset", add = coll)
  if (!"seasonal_onset" %in% names(onset_output)) {
    coll$push("Column 'seasonal_onset' not found in tsd_onset object.")
  }
  if ("seasonal_onset" %in% names(onset_output) && all(onset_output$season == "not_defined")) {
    coll$push("The tsd_onset object is not stratified by season")
  }
  checkmate::reportAssertions(coll)

  # Get seasonal onset dates
  onset_df <- onset_output |>
    dplyr::filter(.data$seasonal_onset) |>
    dplyr::select("season", onset_time = "reference_time")

  # Add onset info back to full output
  peak_df <- onset_output |>
    dplyr::left_join(onset_df, by = "season") |>
    dplyr::filter(!is.na(.data$onset_time), .data$reference_time >= .data$onset_time)

  # Use incidence if in onset_output else use cases
  use_incidence <- FALSE
  if (all(!is.na(onset_output$incidence))) {
    use_incidence <- TRUE
  }

  # Identify peak per season after onset
  peak_summary <- peak_df |>
    dplyr::group_by(.data$season) |>
    dplyr::reframe(
      onset_time = dplyr::first(.data$onset_time),
      peak_time = if (use_incidence) {
        .data$reference_time[which.max(.data$incidence)]
      } else {
        .data$reference_time[which.max(.data$cases)]
      },
      peak_intensity = if (use_incidence) {
        max(.data$incidence)
      } else {
        max(.data$cases, na.rm = TRUE)
      },
      lower_growth_rate_onset = .data$lower_growth_rate[which(.data$reference_time == .data$onset_time)],
      growth_rate_onset = .data$growth_rate[which(.data$reference_time == .data$onset_time)],
      upper_growth_rate_onset = .data$upper_growth_rate[which(.data$reference_time == .data$onset_time)]
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      # Week number of the actual onset and peak date
      onset_week = lubridate::isoweek(.data$onset_time),
      peak_week = lubridate::isoweek(.data$peak_time),
      # Weeks from onset to peak
      weeks_to_peak = as.numeric(.data$peak_time - .data$onset_time) / 7
    )

  class(peak_summary) <- c("historical_summary", class(peak_summary))

  org_seasons <- unique(onset_output$season)
  seasons_with_onset <- unique(peak_summary$season)
  seasons_not_included <- setdiff(org_seasons, seasons_with_onset)

  if (length(seasons_not_included) != 0) {
    seasons <- paste(seasons_not_included, collapse = ", ")
    message(paste("Following seasons do not have an onset,", seasons))
  }

  return(peak_summary)  # nolint: return_linter
}
