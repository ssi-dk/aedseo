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
#' @param family_quant `r rd_burden_level_family`
#' @param burden_level_decrease A character string specifying the burden breakpoint the observations should decrease
#' under to reach `seasonal_offset` or before a new increase in observations can call a new wave onset if
#' `multiple_waves` are TRUE. Choose between; "very low", "low", "medium", or "high".
#' @param steps_with_decrease An integer specifying in how many time steps (days, weeks, months) the decrease
#' should be observed under the `burden_level_decrease` (if there is a sudden decrease followed by an
#' increase it could e.g. be due to testing).
#' @param multiple_waves A logical. Should the output contain multiple waves?
#' @param ... Arguments passed to `seasonal_burden_levels()`, `fit_percentiles()` and `seasonal_onset()` functions.
#'
#' @return An `tsd_onset_and_burden` object containing two lists:
#'
#' onset_output:
#' `r rd_seasonal_onset_return`
#'
#' As extra the `tsd_onset` object will for each season contain a `seasonal_offset` variable:
#' - 'seasonal_offset': Logical. The first detected seasonal offset in the season.
#'
#' If multiple waves is selected the `tsd_onset` object will also contain:
#' - 'wave_number': The wave number in the time series data.
#' - 'wave_starts': Logical. Did a new wave start?
#' - 'wave_ends': Logical. Did the wave end?
#' - 'decrease_counter': How many consecutive time intervals have decreased below the selected burden breakpoint.
#' - 'decrease_value': A numeric specifying the selected burden breakpoint value to fall below for ending the wave.
#'
#' burden_output:
#' `r rd_seasonal_burden_levels_return`
#'
#' #' Attributes in the `tsd_onset_and_burden` object are:
#' `burden_level_decrease`, `steps_with_decrease` and `multiple_waves`.
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
#' tsd_data <- to_time_series(
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
    "poisson",
    "quasibinomial",
    "binomial"
  ),
  family_quant = NULL,
  season_start = 21,
  season_end = season_start - 1,
  only_current_season = TRUE,
  multiple_waves = FALSE,
  burden_level_decrease = c(
    "low",
    "very low",
    "medium",
    "high"
  ),
  steps_with_decrease = 2,
  ...
) {
  coll <- checkmate::makeAssertCollection()
  checkmate::assert_logical(multiple_waves, add = coll)
  checkmate::assert_integerish(steps_with_decrease, lower = 1, add = coll)
  checkmate::reportAssertions(coll)
  burden_level_decrease <- rlang::arg_match(burden_level_decrease)
  if (is.character(family)) {
    family <- match.arg(family)
  }
  if (is.null(family_quant)) {
    family_name <- if (is.character(family)) {
      family
    } else if (inherits(family, "family")) {
      family$family
    } else {
      family()$family
    }
    family_quant <- if (family_name %in% c("binomial", "quasibinomial")) {
      "beta"
    } else {
      "lnorm"
    }
  } else if (is.character(family_quant)) {
    family_quant <- match.arg(family_quant, c("lnorm", "weibull", "exp", "beta"))
  }

  # Capture all extra arguments
  extra_args <- list(...)

  # Get the allowed arguments for seasonal_burden_levels() and/or fit_percentiles()
  burden_allowed <- union(names(formals(seasonal_burden_levels)), names(formals(fit_percentiles)))
  burden_args <- extra_args[names(extra_args) %in% burden_allowed]

  # Get the allowed arguments for seasonal_onset()
  onset_allowed <- names(formals(seasonal_onset))
  onset_args <- extra_args[names(extra_args) %in% onset_allowed]

  # Run the models
  onset_output_raw <- do.call(
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

  if (only_current_season) {
    decrease_below <- burden_output$values[[burden_level_decrease]]
    onset_and_decrease_level <- onset_output_raw |>
      dplyr::mutate(
        decrease_level = burden_level_decrease,
        decrease_value = decrease_below
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
          values_to = "decrease_value"
        ) |>
        dplyr::filter(.data$decrease_level == burden_level_decrease)
    })
    onset_and_decrease_level <- onset_output_raw |>
      dplyr::left_join(burden_levels, by = "season")
  }

  # Keep offset comparisons on the same scale as the fitted burden levels.
  model_outcome <- attr(onset_output_raw, "model_outcome")
  if (identical(model_outcome, "proportion")) {
    onset_and_decrease_level <- onset_and_decrease_level |>
      dplyr::mutate(observation = .data$proportion)
  } else if (identical(model_outcome, "incidence")) {
    onset_and_decrease_level <- onset_and_decrease_level |>
      dplyr::mutate(observation = .data$incidence)
  } else {
    onset_and_decrease_level <- onset_and_decrease_level |>
      dplyr::mutate(observation = .data$cases)
  }

  # Add seasonal end variable
  lag_fns <- stats::setNames(
    lapply(seq_len(steps_with_decrease), \(i) \(x) dplyr::lag(x, n = i)),
    paste0("lag", seq_len(steps_with_decrease))
  )
  onset_output <- onset_and_decrease_level |>
    dplyr::mutate(
      season_id = cumsum(tidyr::replace_na(.data$seasonal_onset, FALSE))
    ) |>
    dplyr::mutate(
      dplyr::across(tidyr::all_of("observation"), .fns = lag_fns, .names = "{.col}_{.fn}")
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      vals = list(c(.data$observation, dplyr::c_across(dplyr::starts_with("observation_lag")))),
      # obs < lag1 < lag2 < ... < lag_steps  (continuous decrease)
      dec_run = !anyNA(.data$vals) && all(diff(.data$vals) > 0),
      # under decrease_value for the "decreased" obs: obs..lag_{steps-1}
      below_thr = !is.na(.data$decrease_value) &&
        !anyNA(.data$vals[seq_len(steps_with_decrease)]) &&
        all(.data$vals[seq_len(steps_with_decrease)] < .data$decrease_value),
      end_candidate = (.data$season_id > 0) && .data$dec_run && .data$below_thr
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      # A zero-case observation is below thresholds on both the count and
      # proportion scales. For binomial data, label the preceding positive
      # observation in the confirmed below-threshold run so the offset remains
      # specifically attributable to the proportion-scale comparison.
      offset_candidate = if (identical(model_outcome, "proportion")) {
        (.data$end_candidate & .data$cases > .data$decrease_value) |
          dplyr::lead(
            .data$end_candidate & .data$cases <= .data$decrease_value,
            default = FALSE
          )
      } else {
        .data$end_candidate
      },
      seasonal_offset = .data$offset_candidate & (cumsum(.data$offset_candidate) == 1),
      .by = "season_id"
    ) |>
    dplyr::select(
      -c("season_id", "vals", "dec_run", "below_thr", "end_candidate", "offset_candidate",
         "decrease_level"),
      -dplyr::starts_with("observation")
    )

  # Add multiple waves if assigned in input
  if (multiple_waves) {

    # Iterate over onset_output
    wave_fun <- function(onset_data, steps_with_decrease) {

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
        if (
          !is.na(prev_obs) &&
            !is.na(onset_data$decrease_value[i]) &&
            onset_data$observation[i] < prev_obs &&
            onset_data$observation[i] < onset_data$decrease_value[i]
        ) {
          onset_data$decrease_counter[i] <- onset_data$decrease_counter[i - 1] + 1
        } else {
          onset_data$decrease_counter[i] <- 0
        }
        # If the number of consecutive decreasing steps reaches `steps_with_decrease`, end the current wave.
        if (
          in_wave &&
            onset_data$decrease_counter[i] >= steps_with_decrease
        ) {
          onset_data$wave_ends[i] <- TRUE
          in_wave <- FALSE
        }
      }
      onset_data <- onset_data |>
        dplyr::select(-c("observation", "decrease_level"))
    }

    # Add new columns for wave_number, wave_starts and decrease_counter
    onset_and_decrease_level <- onset_and_decrease_level |>
      dplyr::mutate(
        wave_number = NA_real_,
        wave_starts = FALSE,
        wave_ends = FALSE,
        decrease_counter = 0
      ) |>
      dplyr::left_join(
        onset_output |>
          dplyr::select(c("reference_time", "seasonal_offset")),
        by = "reference_time"
      )

    onset_output <- wave_fun(
      onset_data = onset_and_decrease_level,
      steps_with_decrease = steps_with_decrease
    )
  }

  # Add attributes and class to `tsd_onset` object again
  onset_output <- structure(
    onset_output,
    k = attr(onset_output_raw, "k"),
    level = attr(onset_output_raw, "level"),
    disease_threshold = attr(onset_output_raw, "disease_threshold"),
    family = attr(onset_output_raw, "family"),
    time_interval = attr(onset_output_raw, "time_interval"),
    incidence_denominator = attr(onset_output_raw, "incidence_denominator"),
    model_outcome = attr(onset_output_raw, "model_outcome"),
    class = c("tsd_onset", class(onset_output))
  )

  # Combine both results in lists and assign a class for the combined results
  structure(
    list(
      onset_output  = onset_output,
      burden_output = burden_output
    ),
    multiple_waves = multiple_waves,
    burden_level_decrease = burden_level_decrease,
    steps_with_decrease = steps_with_decrease,
    class = "tsd_onset_and_burden"
  )
}
