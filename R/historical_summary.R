#' Summarises historical estimates like peak and onset information
#'
#' @description
#'
#' This function summarises peak timing and seasonal onset from estimates from previous seasons.
#' This can be useful for investigating if the current season falls within estimates from previous seasons
#' or if it is very distinct from previous seasons.
#'
#' Uses data from previous seasons from a `tsd_onset` object (output from `seasonal_onset()`).
#'
#' @param onset_output A `tsd_onset` object returned from `seasonal_onset()`.
#'
#' @return An object of class `historical_summary`, containing:
#'  - Usual time to seasonal peak (weeks after onset)
#'  - The week in which the peak usually falls
#'  - Usual peak intensity
#'  - The week in which the onset usually falls
#'  - Usual onset intensity and lower growth rate (How significant)
#'
#' @export
historical_summary <- function(
  onset_output
) {
  checkmate::assert_class(onset_output, "tsd_onset")

  # Get seasonal onset dates
  onset_df <- onset_output |>
    dplyr::filter(.data$seasonal_onset) |>
    dplyr::select("season", onset_time = .data$reference_time)

  # Add onset info back to full output
  peak_df <- onset_output |>
    dplyr::left_join(onset_df, by = "season") |>
    dplyr::filter(!is.na(.data$onset_time), .data$reference_time >= .data$onset_time)

  # Identify peak per season after onset
  peak_summary <- peak_df |>
    dplyr::group_by(.data$season) |>
    dplyr::summarise(
      onset_time = dplyr::first(.data$onset_time),
      peak_time = .data$reference_time[which.max(.data$observation)],
      peak_intensity = max(.data$observation, na.rm = TRUE),
      lower_growth_rate_onset = .data$lower_growth_rate[which(.data$reference_time == .data$onset_time)],
      .groups = "drop"
    ) |>
    dplyr::mutate(
      # Weeks from onset to peak
      weeks_to_peak = as.numeric(.data$peak_time - .data$onset_time) / 7,
      # Week number of the actual peak date
      peak_week = lubridate::isoweek(.data$peak_time),
      onset_week = lubridate::isoweek(.data$onset_time)
    )

  class(peak_summary) <- c("peak_summary", class(peak_summary))
}
