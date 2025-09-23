#' Estimate the disease specific threshold
#'
#' @description
#'
#' This function estimates the disease specific threshold, based on previous seasons.
#' If the disease threshold is estimated between ]0:1] it will be set to 1.
#' Uses data from a `tsd` object.
#'
#' @param tsd `r rd_tsd`
#' @param season_start,season_end `r rd_season_start_end()`
#' @param skip_current_season A logical. Do you want to skip your current season?
#' @param min_significant_time An integer specifying how many time steps that have to be significant to the sequence
#' to be considered in estimation.
#' @param max_gap_time A numeric value specifying how many time steps there is allowed to be non-significant between two
#' significant sequences for maybe considering them as the same sequence.
#' Sometimes e.g. vacations or less testing can lead to false decreases.
#' @param use_prev_seasons_num An integer specifying how many previous seasons you want to include in estimation.
#' @param pick_significant_sequence A character string specifying which significant sequence to pick from each season.
#'  - `longest`: The longest sequence of size `min_significant_time` closest to the peak.
#'  - `earliest`: The earliest sequence of size `min_significant_time` of the season.
#' @param season_importance_decay A numeric value between 0 and 1, that specifies the weight applied to previous
#' seasons. It is used as `season_importance_decay`^(number of seasons back), whereby the weight for the most recent
#' season will be `season_importance_decay`^0 = 1. This parameter allows for a decreasing weight assigned to prior
#' seasons, such that the influence of older seasons diminishes exponentially.
#' @param conf_levels A numeric vector specifying the confidence levels for parameter estimates. The values have
#' to be unique and in ascending order, the first percentile is the disease specific threshold.
#' Specify one or three confidence levels e.g.: `c(0.25)` `c(0.25, 0.5, 0.75)`.
#' @param ... Arguments passed to the `seasonal_onset()` or `fit_percentiles()` function.
#' `only_current_season = FALSE` and `disease_threshold = NA_real_` cannot be changed in `seasonal_onset()`.
#'
#' @return An object of class `tsd_disease_threshold`, containing;
#' ....
#'
#' @export
#'
#' @examples
#' # Generate seasonal data
#' tsd_data <- generate_seasonal_data(
#'  years = 3,
#'  start_date = as.Date("2021-01-01"),
#'  noise_overdispersion = 3
#' )
#'
#' # Estimate disease threshold
#' estimate_disease_threshold(tsd_data)
#'
estimate_disease_threshold <- function(
  tsd,
  season_start = 21,
  season_end = season_start - 1,
  skip_current_season = TRUE,
  min_significant_time = 3,
  max_gap_time = 1,
  use_prev_seasons_num = 3,
  pick_significant_sequence = c("longest", "earliest"),
  season_importance_decay = 0.8,
  conf_levels = c(0.25, 0.5, 0.75),
  ...
) {
  # Check input arguments
  coll <- checkmate::makeAssertCollection()
  checkmate::assert_integerish(season_start, lower = 1, upper = 53,
                               null.ok = FALSE, add = coll)
  checkmate::assert_integerish(season_end, lower = 1, upper = 53,
                               null.ok = FALSE, add = coll)
  checkmate::assert_logical(skip_current_season, add = coll)
  checkmate::assert_integerish(min_significant_time, lower = 1, add = coll)
  checkmate::assert_integerish(use_prev_seasons_num, lower = 1, add = coll)
  checkmate::assert_numeric(season_importance_decay, lower = 0, upper = 1, len = 1, add = coll)
  checkmate::assert_numeric(conf_levels, lower = 0, upper = 1,
                            unique = TRUE, sorted = TRUE, add = coll)
  checkmate::reportAssertions(coll)

  # Capture all extra arguments
  extra_args <- list(...)

  # Get the allowed arguments for seasonal_burden_levels() and/or fit_percentiles()
  percentile_allowed <- names(formals(fit_percentiles))
  percentile_args <- extra_args[names(extra_args) %in% percentile_allowed]

  # Get the allowed arguments for seasonal_onset()
  onset_allowed <- names(formals(seasonal_onset))
  onset_args <- extra_args[names(extra_args) %in% onset_allowed]

  # Throw an error if any of the inputs are not supported
  pick_significant_sequence <- match.arg(pick_significant_sequence)

  # Estimate growth rates
  onset_output <- do.call(
    seasonal_onset,
    c(list(tsd = tsd, season_start = season_start, season_end = season_end,
           only_current_season = FALSE, disease_threshold = NA_real_),
      onset_args)
  )   # nolint: object_usage_linter.

  # Check if skip season
  if (skip_current_season) {
    onset_output <- onset_output |>
      dplyr::filter(.data$season != max(onset_output$season))
  }

  # Count consecutive significant observations
  sign_warnings <- consecutive_growth_warnings(onset_output)

  # Peak time per season
  peaks <- onset_output |>
    dplyr::arrange(.data$season) |>
    dplyr::group_by(.data$season) |>
    dplyr::slice_max(order_by = .data$cases, n = 1, with_ties = FALSE, na_rm = TRUE) |>
    dplyr::ungroup() |>
    dplyr::select("season", peak_time = "reference_time") |>
    dplyr::slice_tail(n = use_prev_seasons_num)

  # Select candidate sequences
  all_sign_seq <- sign_warnings |>
    dplyr::arrange(.data$reference_time) |>
    dplyr::filter(.data$growth_warning == TRUE) |>
    dplyr::reframe(
      significant_observations_window = dplyr::n(),
      start_window_time = dplyr::first(.data$reference_time),
      end_window_time = dplyr::last(.data$reference_time),
      start_average_observations_window = dplyr::first(.data$average_observations_window),
      .by = c("season", "groupID")
    ) |>
    dplyr::filter(.data$significant_observations_window > 0)

  # Merge sequences that fulfill input arguments and select candidate sequences
  cand_seq <- all_sign_seq |>
    dplyr::group_by(.data$season) |>
    dplyr::mutate(
      next_window = dplyr::lead(.data$significant_observations_window, default = NULL),
      next_start = dplyr::lead(.data$start_window_time, default = NULL),
      gap_time = as.numeric(
        difftime(
          .data$next_start,
          .data$end_window_time,
          units = attr(onset_output, "time_interval")
        )
      ),
      do_merge = dplyr::if_else(
        .data$significant_observations_window >= min_significant_time & .data$gap_time <= max_gap_time,
        TRUE, FALSE
      ),
      do_merge = tidyr::replace_na(.data$do_merge, FALSE),
      merge_block = cumsum(dplyr::if_else(dplyr::lag(.data$do_merge, default = FALSE), 0L, 1L))
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(.data$season, .data$merge_block) |>
    dplyr::summarise(
      significant_observations_window = sum(.data$significant_observations_window),
      start_window_time = dplyr::first(.data$start_window_time),
      end_window_time = dplyr::last(.data$end_window_time),
      start_average_observations_window = dplyr::first(.data$start_average_observations_window)
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(.data$significant_observations_window >= min_significant_time) |>
    dplyr::right_join(peaks, by = "season") |>
    dplyr::mutate(peak_time = dplyr::first(.data$peak_time), .by = "season") |>
    dplyr::mutate(
      end_to_peak_gap = as.numeric(
        difftime(
          .data$peak_time,
          .data$end_window_time,
          units = attr(onset_output, "time_interval")
        )
      )
    ) |>
    dplyr::filter(!is.na(.data$significant_observations_window))

  # If no seasons have significant weeks
  if (nrow(cand_seq) == 0) {
    no_results <- list(
      note = "No seasons met the criteria.",
      seasons = unique(peaks$season),
      disease_threshold = NA_real_,
      optim = NA,
      settings = list(skip_current_season = skip_current_season,
                      min_significant_time = min_significant_time,
                      use_prev_seasons_num = use_prev_seasons_num,
                      pick_significant_sequence = pick_significant_sequence,
                      season_importance_decay = season_importance_decay,
                      percentiles = conf_levels),
      incidence_denominator = attr(onset_output, "incidence_denominator"),
      time_interval = attr(onset_output, "time_interval")
    )
    class(no_results) <- "tsd_disease_threshold"
    return(no_results)
  }

  # Select one consecutive significant sequence per season
  if (pick_significant_sequence == "earliest") {
    per_season_sequence <- cand_seq |>
      dplyr::group_by(.data$season) |>
      dplyr::arrange(.data$start_window_time) |>
      dplyr::slice_head(n = 1) |>
      dplyr::ungroup()
  } else {
    per_season_sequence <- cand_seq |>
      dplyr::group_by(.data$season) |>
      dplyr::mutate(end_to_peak_gap_abs = abs(.data$end_to_peak_gap)) |>
      dplyr::arrange(dplyr::desc(.data$significant_observations_window), .data$end_to_peak_gap_abs) |>
      dplyr::slice_head(n = 1) |>
      dplyr::ungroup()
  }

  # If average observations in the start of the window is 0 it will be converted to 1
  if (any(per_season_sequence$start_average_observations_window <= 0)) {
    per_season_sequence <- per_season_sequence |>
      dplyr::mutate(
        start_average_observations_window = dplyr::if_else(
          .data$start_average_observations_window <= 0, 1,
          .data$start_average_observations_window
        )
      )
  }

  # If there is only one season with observation that will be the threshold
  # If all observations are 1, the disease threshold will be 1
  if (nrow(per_season_sequence) == 1 ||
        length(unique(per_season_sequence$start_average_observations_window)) == 1 ||
        all(unique(per_season_sequence$start_average_observations_window) == 1)) {

    disease_threshold <- unique(per_season_sequence$start_average_observations_window)

    same_result <- list(
      note = "Only one season is used to determine the threshold.",
      seasons = unique(per_season_sequence$season),
      disease_threshold = dplyr::if_else(dplyr::between(disease_threshold, 0, 1), 1, disease_threshold),
      optim = NA,
      settings = list(skip_current_season = skip_current_season,
                      min_significant_time = min_significant_time,
                      use_prev_seasons_num = use_prev_seasons_num,
                      pick_significant_sequence = pick_significant_sequence,
                      season_importance_decay = season_importance_decay,
                      percentiles = conf_levels),
      incidence_denominator = attr(onset_output, "incidence_denominator"),
      time_interval = attr(onset_output, "time_interval")
    )

    class(same_result) <- "tsd_disease_threshold"
    return(same_result)
  }

  # Add weights and remove current season to get predictions for this season
  weighted_significant_sequences <- per_season_sequence |>
    dplyr::mutate(year = purrr::map_chr(.data$season, ~ stringr::str_extract(.x, "[0-9]+")) |>
                    as.numeric()) |>
    dplyr::mutate(weight = season_importance_decay^(max(.data$year) - .data$year)) |>
    dplyr::select(-"year") |>
    dplyr::rename(observation = "start_average_observations_window")

  # Run percentiles_fit function
  percentiles_fit <- do.call(
    fit_percentiles,
    c(list(
           weighted_observations = weighted_significant_sequences |>
             dplyr::select("observation", "weight"),
           conf_levels = conf_levels),
    percentile_args)
  )

  fit_results <- list(
    note = "Input settings were successfully used in the estimation.",
    seasons = unique(weighted_significant_sequences$season),
    disease_threshold = dplyr::if_else(dplyr::between(percentiles_fit$values[1], 0, 1), 1, percentiles_fit$values[1]),
    optim = percentiles_fit,
    settings = list(skip_current_season = skip_current_season,
                    min_significant_time = min_significant_time,
                    use_prev_seasons_num = use_prev_seasons_num,
                    pick_significant_sequence = pick_significant_sequence,
                    season_importance_decay = season_importance_decay,
                    percentiles = conf_levels),
    incidence_denominator = attr(onset_output, "incidence_denominator"),
    time_interval = attr(onset_output, "time_interval")
  )

  class(fit_results) <- "tsd_disease_threshold"

  return(fit_results)
}
