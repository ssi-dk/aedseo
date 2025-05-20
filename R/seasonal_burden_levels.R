#' Compute burden levels from seasonal time series observations of current season.
#'
#' @description
#'
#' This function estimates the burden levels of time series observations that are stratified by season.
#' It uses the previous seasons to estimate the levels of the current season.
#' The output is results regarding the current season in the time series observations.
#' NOTE: The data must include data for a complete previous season to make predictions for the current season.
#'
#' @param tsd `r rd_tsd`
#' @param family `r rd_family()`
#' @param season_start,season_end `r rd_season_start_end()`
#' @param incidence_conversion Should the burden levels be converted to per incidence?
#' @param incidence_rate An integer >= 1, specifying the incidence rate (e.g. observations per 1,000 population).
#' @param method A character string specifying the model to be used in the level calculations.
#' Both model predict the levels of the current series of
#' observations.
#'  - `intensity_levels`: models the risk compared to what has been observed in previous seasons.
#'  - `peak_levels`: models the risk compared to what has been observed in the `n_peak` observations each season.
#' @param conf_levels A numeric vector specifying the confidence levels for parameter estimates. The values have
#' to be unique and in ascending order, (i.e. the lowest level is first and highest level is last).
#' The `conf_levels` are specific for each method:
#'   - for `intensity_levels` only specify the highest confidence level e.g.: `0.95`, which is the highest intensity
#'     that has been observed in previous seasons.
#'   - for `peak_levels` specify three confidence levels e.g.: `c(0.4, 0.9, 0.975)`, which are the three confidence
#'     levels low, medium and high that reflect the peak severity relative to those observed in previous seasons.
#' @param decay_factor A numeric value between 0 and 1, that specifies the weight applied to previous seasons in level
#' calculations. It is used as `decay_factor`^(number of seasons back), whereby the weight for the most recent season
#' will be `decay_factor`^0 = 1. This parameter allows for a decreasing weight assigned to prior seasons, such that
#' the influence of older seasons diminishes exponentially.
#' @param disease_threshold `r rd_disease_threshold(usage = "levels")`
#' @param n_peak A numeric value specifying the number of peak observations to be selected from each season in the
#' level calculations. The `n_peak` observations have to surpass the `disease_threshold` to be included.
#' @param only_current_season `r rd_only_current_season`
#' @param ... Arguments passed to the `fit_percentiles()` function.
#'
#' @return `r rd_seasonal_burden_levels_return`
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
#' # Print seasonal burden results
#' seasonal_burden_levels(tsd_data, family = "lnorm")
#' @importFrom rlang .data
seasonal_burden_levels <- function(
  tsd,
  family = c("lnorm",
             "weibull",
             "exp"),
  season_start = 21,
  season_end = season_start - 1,
  incidence_conversion = FALSE,
  incidence_rate = NULL,
  method = c("intensity_levels", "peak_levels"),
  conf_levels = 0.95,
  decay_factor = 0.8,
  disease_threshold = 20,
  n_peak = 6,
  only_current_season = TRUE,
  ...
) {
  # Check input arguments
  method <- rlang::arg_match(method)
  coll <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(tsd, add = coll)
  checkmate::assert_class(tsd, "tsd", add = coll)
  checkmate::assert_names(
    colnames(tsd),
    must.include = c("time", "observation"),
    subset.of = c("time", "observation", "pop"),
    add = coll
  )
  checkmate::assert_integerish(season_start, lower = 1, upper = 53,
                               null.ok = FALSE, add = coll)
  checkmate::assert_integerish(season_end, lower = 1, upper = 53,
                               null.ok = FALSE, add = coll)
  checkmate::assert_logical(incidence_conversion, add = coll)
  checkmate::assert_integerish(incidence_rate, lower = 1, len = 1, null.ok = TRUE, add = coll)
  checkmate::assert_numeric(decay_factor, lower = 0, upper = 1, len = 1, add = coll)
  checkmate::assert_numeric(n_peak, lower = 1, len = 1, add = coll)
  checkmate::assert_integerish(disease_threshold, len = 1, add = coll)
  checkmate::assert_logical(only_current_season, add = coll)
  # Assert conf_levels based on the method chosen
  if (method == "intensity_levels") {
    checkmate::assert_numeric(conf_levels, lower = 0, upper = 1, len = 1,
                              unique = TRUE, sorted = TRUE, add = coll)
  } else if (method == "peak_levels") {
    checkmate::assert_numeric(conf_levels, lower = 0, upper = 1, len = 3,
                              unique = TRUE, sorted = TRUE, add = coll)
  }

  if (incidence_conversion) {
    if (is.null(incidence_rate)) {
      coll$push("If incidence_conversion is assigned incidence_rate must also be assigned")
    }
    if (!"pop" %in% names(tsd)) {
      coll$push("If incidence_conversion is assigned, tsd must contain a 'pop' column")
    }
    tsd <- tsd |>
      dplyr::mutate(observation = (.data$observation / .data$pop) * incidence_rate) |>
      dplyr::select(-"pop")
  }

  # Add the seasons to data
  seasonal_tsd <- tsd |>
    dplyr::mutate(season = epi_calendar(.data$time, start = season_start, end = season_end)) |>
    dplyr::arrange(dplyr::desc(.data$season))

  # Check that there is at least two seasons of data
  if (length(unique(seasonal_tsd$season)) <= 1) coll$push("There must be at least two unique seasons in the data.")
  checkmate::reportAssertions(coll)

  # Select n_peak highest observations and filter observations >= disease_threshold
  peak_seasonal_tsd <- seasonal_tsd |>
    dplyr::filter(.data$observation >= disease_threshold) |>
    dplyr::slice_max(.data$observation, n = n_peak, with_ties = FALSE, by = "season")

  # main level function
  main_level_fun <- function(seasonal_data, current_season) {
    # If there is no previous observations > disease_threshold -> return NA
    if (nrow(seasonal_data) == 0 || all(unique(seasonal_data$season) == current_season)) {
      warning("There are no observations above `disease_threshold`. Returning NA values.", call. = FALSE)
      percentiles_fit <- list(
        conf_levels = conf_levels,
        values      =  rep(NA, length(conf_levels)),
        par         = c(NA, NA),
        obj_value   = NA,
        converged   = FALSE,
        family      = family
      )
    } else {
      # Add weights and remove current season to get predictions for this season
      weighted_seasonal_tsd <- seasonal_data |>
        dplyr::filter(.data$season != current_season) |>
        dplyr::mutate(year = purrr::map_chr(.data$season, ~ stringr::str_extract(.x, "[0-9]+")) |>
                        as.numeric()) |>
        dplyr::mutate(weight = decay_factor^(max(.data$year) - .data$year)) |>
        dplyr::select(-c("year", "time"))

      # Run percentiles_fit function
      percentiles_fit <- weighted_seasonal_tsd |>
        dplyr::select("observation", "weight") |>
        fit_percentiles(weighted_observations = _, conf_levels = conf_levels, family = family, ...)
    }

    # If method intensity_levels was chosen; use the high level from the `fit_percentiles` function as the high
    # level and the disease_threshold as the very low level. The low and medium levels are defined as the relative
    # increase between the very low level and high level.
    results <- switch(method,
      peak_levels = {
        model_output <- append(percentiles_fit, list(season = max(seasonal_tsd$season)), after = 0)
        model_output <- list(
          season = max(seasonal_tsd$season),
          values = stats::setNames(c(disease_threshold, model_output$values),
                                   c("very low", "low", "medium", "high")),
          optim = percentiles_fit,
          disease_threshold = disease_threshold
        )
      },
      intensity_levels = {
        if (all(is.na(percentiles_fit$values))) {
          level_step_log <- c(disease_threshold, rep(NA, 3))
        } else {
          level_step_log <- pracma::logseq(disease_threshold, percentiles_fit$values, n = 4)
        }
        model_output <- list(
          season = current_season,
          values = stats::setNames(level_step_log, c("very low", "low", "medium", "high")),
          optim = list(
            par = percentiles_fit$par,
            obj_value = percentiles_fit$obj_value,
            converged = percentiles_fit$converged,
            high_conf_level = percentiles_fit$conf_levels,
            family = percentiles_fit$family
          ),
          disease_threshold = disease_threshold
        )
      }
    )
    results
  }
  # Select seasons for output based on only_current_season input argument
  if (only_current_season == FALSE) {
    # Group seasons to get results for all seasons available
    unique_seasons <- unique(rev(peak_seasonal_tsd$season))
    season_groups <- purrr::accumulate(unique_seasons, `c`)
    season_groups_data <- purrr::map(season_groups[-1], ~ peak_seasonal_tsd |> dplyr::filter(.data$season %in% .x))

    level_results <- purrr::map(season_groups_data, ~ main_level_fun(.x, current_season = max(.x$season)))

  } else {
    level_results <- main_level_fun(peak_seasonal_tsd, current_season = max(seasonal_tsd$season))
  }

  class(level_results) <- "tsd_burden_levels"

  return(level_results)
}
