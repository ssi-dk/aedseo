#' Estimate the disease specific threshold of your time series data
#'
#' @description
#'
#' This function estimates the disease specific threshold, based on previous seasons.
#' For count/incidence data, thresholds estimated between ]0:1] are set to 1.
#' For binomial/proportional data, thresholds remain on the proportion scale and beta percentiles are used by default.
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
#' @param family `r rd_family()` Passed to `seasonal_onset()` and then to `fit_growth_rate()`.
#' @param burden_family `r rd_burden_level_family` Passed to `fit_percentiles()` as its `family` argument.
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
  family = c(
    "quasipoisson",
    "poisson",
    "quasibinomial",
    "binomial"
  ),
  burden_family = c(
    "lnorm",
    "weibull",
    "exp",
    "beta"
  ),
  ...
) {
  # Check input arguments
  family <- rlang::arg_match(family)
  burden_family <- rlang::arg_match(burden_family)

  coll <- checkmate::makeAssertCollection()
  checkmate::assert_names(
    colnames(tsd),
    must.include = c("time", "cases"),
    subset.of = c("time", "cases", "incidence", "population", "proportion", "samples"),
    add = coll
  )
  if ("proportion" %in% attr(tsd, "outcome_type")) {
    checkmate::assert_names(colnames(tsd), must.include = c("proportion", "samples"), add = coll)
  }
  if ("incidence" %in% attr(tsd, "outcome_type")) {
    checkmate::assert_names(colnames(tsd), must.include = c("incidence", "population"), add = coll)
  }
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
  percentile_allowed <- setdiff(names(formals(fit_percentiles)), "family")
  percentile_args <- extra_args[names(extra_args) %in% percentile_allowed]

  # Get the allowed arguments for seasonal_onset()
  onset_allowed <- setdiff(names(formals(seasonal_onset)), "family")
  onset_args <- extra_args[names(extra_args) %in% onset_allowed]

  # Throw an error if any of the inputs are not supported
  pick_significant_sequence <- match.arg(pick_significant_sequence)
  # Estimate growth rates
  onset_output <- do.call(
    seasonal_onset,
    c(
      list(
        tsd = tsd, season_start = season_start, season_end = season_end,
        only_current_season = FALSE, disease_threshold = NA_real_, family = family
      ),
      onset_args
    )
  )   # nolint: object_usage_linter.

  # Checking which model_outcome was used
  model_outcome <- attr(onset_output, "model_outcome")

  # Check if skip season
  if (skip_current_season) {
    onset_output <- onset_output |>
      dplyr::filter(.data$season != max(onset_output$season))
  }

  # If no seasons have onset output
  if (all(is.na(onset_output$average_observations_window))) {
    no_tsd_onset <- tsd |> dplyr::mutate(season = epi_calendar(.data$time, start = season_start, end = season_end))
    no_results <- list(
      note = "No seasons met the `seasonal_onset()` criteria.",
      seasons = unique(no_tsd_onset$season),
      disease_threshold = NA_real_,
      optim = NA,
      settings = list(skip_current_season = skip_current_season,
                      min_significant_time = min_significant_time,
                      use_prev_seasons_num = use_prev_seasons_num,
                      pick_significant_sequence = pick_significant_sequence,
                      season_importance_decay = season_importance_decay,
                      family = family,
                      percentiles = conf_levels),
      incidence_denominator = attr(onset_output, "incidence_denominator"),
      time_interval = attr(onset_output, "time_interval"),
      onset_output = onset_output
    )
    class(no_results) <- "tsd_disease_threshold"
    return(no_results)
  }

  # Count consecutive significant observations
  sign_warnings <- consecutive_growth_warnings(onset_output)

  # Peak time per season. Prefer incidence when it is available, because it
  # accounts for population/trial denominators; otherwise use raw cases.
  peak_observation <- if (model_outcome == "incidence") {
    "incidence"
  } else {
    "cases"
  }
  peaks <- onset_output |>
    dplyr::arrange(.data$season) |>
    dplyr::group_by(.data$season) |>
    dplyr::slice_max(order_by = .data[[peak_observation]], n = 1, with_ties = FALSE, na_rm = TRUE) |>
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

  # Merge sequences
  merged_seq <- all_sign_seq |>
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
    dplyr::ungroup()

  # Remove sequences that do not start before the peak and select candidate sequences
  cand_seq <- merged_seq |>
    dplyr::filter(.data$significant_observations_window >= min_significant_time) |>
    dplyr::right_join(peaks, by = "season") |>
    dplyr::mutate(peak_time = dplyr::first(.data$peak_time), .by = "season") |>
    dplyr::mutate(
      start_to_peak_gap = as.numeric(
        difftime(
          .data$peak_time,
          .data$start_window_time,
          units = attr(onset_output, "time_interval")
        )
      ),
      end_to_peak_gap = as.numeric(
        difftime(
          .data$peak_time,
          .data$end_window_time,
          units = attr(onset_output, "time_interval")
        )
      )
    ) |>
    dplyr::filter(.data$start_to_peak_gap >= 0) |>
    dplyr::filter(!is.na(.data$significant_observations_window))

  # If no seasons have significant weeks in sequences
  if (nrow(cand_seq) == 0) {
    no_results <- list(
      note = "No seasons met the `estimate_disease_threshold()` criteria.",
      seasons = unique(peaks$season),
      disease_threshold = NA_real_,
      optim = NA,
      settings = list(skip_current_season = skip_current_season,
                      min_significant_time = min_significant_time,
                      use_prev_seasons_num = use_prev_seasons_num,
                      pick_significant_sequence = pick_significant_sequence,
                      season_importance_decay = season_importance_decay,
                      family = family,
                      percentiles = conf_levels),
      incidence_denominator = attr(onset_output, "incidence_denominator"),
      time_interval = attr(onset_output, "time_interval"),
      onset_output = onset_output
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
          .data$start_average_observations_window <= 0,
          1,
          .data$start_average_observations_window
        )
      )
  }

  # Function to normalize the threshold based on count or binomial data
  normalize_threshold <- function(x, is_binomial) {
    if (is_binomial) {
      pmin(pmax(x, 0), 1)
    } else {
      dplyr::if_else(dplyr::between(x, 0, 1), 1, x)
    }
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
      disease_threshold = normalize_threshold(disease_threshold, model_outcome == "proportion"),
      optim = NA,
      settings = list(skip_current_season = skip_current_season,
                      min_significant_time = min_significant_time,
                      use_prev_seasons_num = use_prev_seasons_num,
                      pick_significant_sequence = pick_significant_sequence,
                      season_importance_decay = season_importance_decay,
                      family = family,
                      percentiles = conf_levels),
      incidence_denominator = attr(onset_output, "incidence_denominator"),
      time_interval = attr(onset_output, "time_interval"),
      onset_output = onset_output
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

  # For proportion-based data, account for binomial precision by up-weighting
  # observations from larger trial counts.
  if (model_outcome == "proportion") {
    k_window <- attr(onset_output, "k")
    if (is.null(k_window) || !is.numeric(k_window) || length(k_window) != 1) {
      k_window <- 5
    }
    onset_proportion <- onset_output |>
      dplyr::arrange(.data$reference_time) |>
      dplyr::pull("proportion")
    onset_with_window <- onset_output |>
      dplyr::arrange(.data$reference_time) |>
      dplyr::mutate(
        idx = dplyr::row_number(),
        population_window = purrr::map_dbl(
          .data$idx,
          function(idx) {
            sum(onset_proportion[max(1, idx - k_window + 1):idx], na.rm = TRUE)
          }
        )
      )
    pop_weights <- onset_with_window |>
      dplyr::filter(.data$reference_time %in% weighted_significant_sequences$start_window_time) |>
      dplyr::select("season", "reference_time", "population_window") |>
      dplyr::rename(start_window_time = "reference_time", population_weight = "population_window")
    weighted_significant_sequences <- weighted_significant_sequences |>
      dplyr::left_join(pop_weights, by = c("season", "start_window_time")) |>
      dplyr::mutate(
        population_weight = dplyr::coalesce(.data$population_weight, 1),
        weight = .data$weight * .data$population_weight
      ) |>
      dplyr::select(-"population_weight")
  }

  # Run percentiles_fit function
  percentiles_fit <- do.call(
    fit_percentiles,
    c(
      list(
        weighted_observations = weighted_significant_sequences |>
          dplyr::select("observation", "weight"),
        conf_levels = conf_levels, family = burden_family
      ),
      percentile_args
    )
  )

  fit_results <- list(
    note = "Sufficient information to estimate percentiles.",
    seasons = unique(weighted_significant_sequences$season),
    disease_threshold = normalize_threshold(percentiles_fit$values[1], model_outcome == "proportion"),
    optim = percentiles_fit,
    settings = list(skip_current_season = skip_current_season,
                    min_significant_time = min_significant_time,
                    use_prev_seasons_num = use_prev_seasons_num,
                    pick_significant_sequence = pick_significant_sequence,
                    season_importance_decay = season_importance_decay,
                    family = family,
                    percentiles = conf_levels),
    incidence_denominator = attr(onset_output, "incidence_denominator"),
    time_interval = attr(onset_output, "time_interval"),
    onset_output = onset_output
  )

  # Add class, and keep attributes from the `tsd` class
  structure(
    fit_results,
    time_interval = attr(tsd, "time_interval"),
    incidence_denominator = attr(tsd, "incidence_denominator"),
    class = c("tsd_disease_threshold", class(fit_results)),
    model_outcome = model_outcome
  )
}
