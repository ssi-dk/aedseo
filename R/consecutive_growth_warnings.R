#' Create a tsd_growth_warning object to count consecutive significant observations
#'
#' @description
#'
#' This function calculates the number of consecutive significant ("growth_warning") observations,
#' grouping them accordingly. The result is stored in an S3 object of class `tsd_threshold`.
#'
#' Uses data from a `tsd_onset` object (output from `seasonal_onset()`).
#'
#' `seasonal_onset()` has to be run with arguments;
#'  - season_start
#'  - season_end
#'  - only_current_season = FALSE
#'
#' @param onset_output A `tsd_onset` object returned from `seasonal_onset()`.
#'
#' @return An object of class `tsd_growth_warning`, containing;
#' A tibble of processed observations, the significant_counter column specifies when a sequence of
#' significant observation starts and ends. The first number is how many subsequent observations will be significant.
#'
#' @export
#'
#' @examples
#' # Generate simulated data of seasonal waves
#' sim_data <- generate_seasonal_data(
#'   years = 5,
#'   start_date = as.Date("2022-05-26"),
#'   trend_rate = 1.002,
#'   noise_overdispersion = 2,
#'   relative_epidemic_concentration = 3
#' )
#'
#' # Estimate seasonal onset
#' tsd_onset <- seasonal_onset(
#'   tsd = sim_data,
#'   family = "quasipoisson",
#'   season_start = 21,
#'   season_end = 20,
#'   only_current_season = FALSE
#' )
#'
#' # Get consecutive significant observations
#' consecutive_growth_warnings(tsd_onset)
consecutive_growth_warnings <- function(
  onset_output
) {
  # Check input arguments
  coll <- checkmate::makeAssertCollection()
  checkmate::assert_class(onset_output, "tsd_onset", add = coll)
  if (all(onset_output$season == "not_defined")) {
    coll$push("The tsd_onset object is not stratified by season")
  }
  checkmate::reportAssertions(coll)

  # Create counters for consecutive "growth_warning == TRUE"
  significant_counter <- onset_output |>
    dplyr::mutate(
      counter = cumsum(.data$growth_warning == TRUE & !is.na(.data$growth_warning)) * .data$growth_warning
    ) |>
    dplyr::mutate(
      counter = dplyr::if_else(.data$growth_warning, .data$counter, NA_real_)
    ) |>
    dplyr::mutate(
      changeFlag = is.na(.data$counter),
      groupID = cumsum(.data$changeFlag)
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(.data$groupID) |>
    dplyr::mutate(
      significant_counter = dplyr::if_else(
        .data$growth_warning == TRUE,
        rev(cumsum(rev(!is.na(.data$counter)))),
        NA_real_
      )
    ) |>
    dplyr::ungroup()

  class(significant_counter) <- c("tsd_growth_warning", class(significant_counter))

  return(significant_counter)  # nolint: return_linter
}
