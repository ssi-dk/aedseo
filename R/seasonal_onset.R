#' Automated and Early Detection of Seasonal Epidemic Onset
#'
#' @description
#'
#' This function performs automated and early detection of seasonal epidemic onsets on a `tsd` object.
#' It estimates growth rates and calculates the average observation in consecutive time intervals (`k`).
#' For count/incidence data, Poisson/quasi-Poisson models use `population` as an offset when available.
#' For binomial data created with `samples` and `cases` or `proportion`, use `family = "binomial"`
#' or `family = "quasibinomial"`; `samples` is used as the denominator and the rolling window is reported
#' as a pooled proportion.
#'
#' @param tsd `r rd_tsd`
#' @param k An integer specifying the window size for modeling growth rates and average sum of cases.
#' @param level The confidence level for onset parameter estimates, a numeric value between 0 and 1.
#' @param disease_threshold `r rd_disease_threshold(usage = "onset")`
#' @param family `r rd_family()`
#' @param na_fraction_allowed Numeric value between 0 and 1 specifying the fraction of observations in the window
#' of size k that are allowed to be NA or zero, i.e. without cases, in onset calculations.
#' @param season_start,season_end `r rd_season_start_end(usage = "onset")`
#' @param only_current_season `r rd_only_current_season`
#'
#' @return `r rd_seasonal_onset_return`
#'
#' @export
#'
#' @examples
#' # Create a tibble object from sample data
#' tsd_data <- to_time_series(
#'   cases = c(100, 120, 150, 180, 220, 270),
#'   time = seq(from = as.Date("2023-01-01"), by = "1 week", length.out = 6)
#' )
#'
#' # Estimate seasonal onset with a 3-day window
#' seasonal_onset(
#'   tsd = tsd_data,
#'   k = 3,
#'   level = 0.975,
#'   disease_threshold = 5,
#'   na_fraction_allowed = 0.4,
#'   season_start = 21,
#'   season_end = 20,
#'   only_current_season = FALSE
#' )
seasonal_onset <- function(
  tsd,
  k = 5,
  level = 0.95,
  disease_threshold = NA_real_,
  family = c(
    "quasipoisson",
    "poisson",
    "quasibinomial",
    "binomial"
    # TODO: #10 Include negative.binomial regressions. @telkamp7
  ),
  na_fraction_allowed = 0.4,
  season_start = NULL,
  season_end = season_start - 1,
  only_current_season = NULL
) {
  # Check input arguments
  coll <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(tsd, add = coll)
  checkmate::assert_class(tsd, "tsd", add = coll)

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
  checkmate::assert_numeric(level, lower = 0, upper = 1, add = coll)
  checkmate::assert_numeric(na_fraction_allowed, lower = 0, upper = 1,
                            add = coll)
  checkmate::assert_integerish(k, lower = 1, len = 1, add = coll)
  checkmate::assert_numeric(disease_threshold, add = coll)
  checkmate::assert_integerish(season_start, lower = 1, upper = 53,
                               null.ok = TRUE, add = coll)
  checkmate::assert_integerish(season_end, lower = 1, upper = 53,
                               null.ok = TRUE, add = coll)
  checkmate::assert_logical(only_current_season, null.ok = TRUE, add = coll)
  checkmate::assert_multi_class(x = family, classes = c("character", "function", "family"), add = coll)
  if (is.character(family)) {
    family <- match.arg(family)
    family_char <- family
  } else if (inherits(family, "family")) {
    family_char <- family$family
  } else if (inherits(family, "function")) {
    tmp <- eval(as.call(list(family)))
    if (inherits(tmp, "family")) {
      family_char <- tmp$family
    } else {
      coll$push(
        "The family argument was a function that did not return an object with class 'family'."
      )
    }
  }
  checkmate::reportAssertions(coll)

  # Deciding which model outcome to use
  incidence_denominator <- attr(tsd, "incidence_denominator")
  model_outcome <- NULL
  if ("proportion" %in% attr(tsd, "outcome_type") && family_char %in% c("binomial", "quasibinomial")) {
    model_outcome <- "proportion"
    incidence_denominator <- 1 # Enforcing unity for proportions
  } else if ("incidence" %in% attr(tsd, "outcome_type") && family_char %in% c("poisson", "quasipoisson")) {
    model_outcome <- "incidence"
  } else if ("cases" %in% attr(tsd, "outcome_type") && family_char %in% c("poisson", "quasipoisson")) {
    model_outcome <- "cases"
  }

  if (is.null(model_outcome)) {
    coll$push(
      "Mismatch between variables in the input data and the desired family for the glm. Unable to decide model outcome."
    )
  }

  if (!is.null(season_start) && is.null(season_end)) {
    coll$push("If season_start is assigned season_end must also be assigned.")
  }
  if (is.null(season_start) && !is.null(only_current_season)) {
    coll$push("If season_start is NULL only_current_season must also be NULL")
  }
  if (!is.null(season_start) && is.null(only_current_season)) {
    coll$push("If season_start is assigned only_current_season must also be assigned")
  }
  checkmate::reportAssertions(coll)

  # Save the time_interval from the original tsd object
  time_interval <- attr(tsd, "time_interval")

  # Add the seasons to tsd if available
  if (!is.null(season_start)) {
    tsd <- tsd |> dplyr::mutate(season = epi_calendar(.data$time, start = season_start, end = season_end))
  } else {
    tsd <- tsd |> dplyr::mutate(season = "not_defined")
  }

  # Define observation as cases in `tsd`.
  tsd <- tsd |>
    dplyr::mutate(observation = .data$cases)

  # Extract only current season if assigned
  if (!is.null(season_start) && only_current_season == TRUE) {
    seasons <- tsd |>
      dplyr::distinct(.data$season) |>
      dplyr::pull(.data$season)

    # If two or more seasons exist, take the last two
    if (length(seasons) >= 2) {
      seasons <- utils::tail(seasons, n = 2)
      prev_season <- seasons[1]
      current_season <- seasons[2]
    } else {
      prev_season <- NA_character_
      current_season <- seasons[1]
    }

    # Create the combined data frame:
    # If a previous season exists, use its last k-1 rows
    # or else use the current season
    if (!is.na(prev_season)) {
      tsd <- dplyr::bind_rows(
        tsd |>
          dplyr::filter(.data$season == prev_season) |>
          dplyr::slice_tail(n = k - 1),
        tsd |>
          dplyr::filter(.data$season == current_season)
      )
    } else {
      tsd <- tsd |>
        dplyr::filter(.data$season == current_season)
    }
  }

  # Extract the length of the series
  n <- base::nrow(tsd)

  # Allocate space for growth rate estimates
  res <- tibble::tibble()
  skipped_window <- base::rep(FALSE, base::nrow(tsd))

  # Return NA if the tsd is too short for the window size
  if (n < k) {
    res <- tibble::tibble(
      reference_time = tsd$time,
      cases = tsd$cases,
      season = tsd$season,
      population = if ("population" %in% names(tsd)) tsd$population else NA_real_,
      incidence = if ("incidence"  %in% names(tsd)) tsd$incidence  else NA_real_,
      proportion = if ("proportion" %in% names(tsd)) tsd$proportion else NA_real_,
      samples = if ("samples"  %in% names(tsd)) tsd$samples  else NA_real_,
      growth_rate = NA_real_,
      lower_growth_rate = NA_real_,
      upper_growth_rate = NA_real_,
      growth_warning = FALSE,
      average_observations_window = NA_real_,
      average_observations_warning = FALSE,
      seasonal_onset_alarm = FALSE,
      seasonal_onset = FALSE,
      skipped_window = TRUE,
      converged = FALSE
    )

    # Turn the results into an `tsd_onset` class
    ans <- tibble::new_tibble(
      x = res,
      class = "tsd_onset",
      k = k,
      level = level,
      disease_threshold = disease_threshold,
      family = family,
      incidence_denominator = incidence_denominator,
      model_outcome = model_outcome
    )

    # Keep attributes from the `tsd` class
    attr(ans, "time_interval") <- attr(tsd, "time_interval")

    return(ans)
  }

  # Estimate growth rates for all possible intervals
  for (i in k:n) {

    # Ensure continuous time steps within the k window with maximum of na_fraction_allowed
    current_time <- tsd$time[i]

    # Define expected time points within the k-window
    expected_time <- switch(
      time_interval,
      days = current_time - lubridate::days((k - 1):0),
      weeks = current_time - lubridate::weeks((k - 1):0),
      months = lubridate::`%m-%`(current_time, lubridate::period(months = (k - 1):0))
    )

    # Create complete k-window
    # Use match instead of left_join to reduce computation time
    # Missing time points are represented by NA
    idx <- match(expected_time, tsd$time)
    obs_iter <- tsd[idx, ]
    obs_iter$time <- expected_time

    # Evaluate NA and zero values in windows
    if (sum(is.na(obs_iter$observation) | obs_iter$observation == 0) > k * na_fraction_allowed) {
      skipped_window[i] <- TRUE
      # Set fields to NA since the window is skipped
      growth_rates <- list(estimate = c(NA, NA, NA), fit = list(converged = FALSE))
    } else {
      # Estimate growth rates
      growth_rates <- fit_growth_rate(
        cases = obs_iter$observation,
        denominator = if (model_outcome == "proportion") {
          obs_iter$samples
        } else if (model_outcome == "incidence") {
          obs_iter$population
        } else {
          NULL
        },
        level = level,
        family = family
      )
    }

    # See if the growth rate is significantly higher than zero
    growth_warning <- growth_rates$estimate[2] > 0

    if (model_outcome == "cases") {
      # Calculate average cases in window (k)
      average_observations_window <- base::sum(obs_iter$cases, na.rm = TRUE) / k
    } else {
      # Calculate pooled incidence/proportion in window (k), weighted by samples
      # and expressed on the original incidence denominator scale.
      total_cases <- base::sum(obs_iter$observation, na.rm = TRUE)
      if (model_outcome == "proportion") {
        total_population <- base::sum(obs_iter$samples, na.rm = TRUE)
      } else {
        total_population <- base::sum(obs_iter$population, na.rm = TRUE)
      }
      average_observations_window <- ifelse(total_population > 0,
        (total_cases / total_population) * incidence_denominator,
        NA_real_
      )
    }
    # Evaluate if average_incidence_window exceeds disease_threshold.
    # If no threshold is supplied, no threshold-based onset can be detected,
    # but keep the output schema consistent for downstream methods.
    average_observations_warning <- ifelse(is.na(disease_threshold),
      FALSE,
      average_observations_window > disease_threshold
    )

    # Give a seasonal_onset_alarm if both criteria are met
    seasonal_onset_alarm <- growth_warning & average_observations_warning

    # Collect the results
    res <- dplyr::bind_rows(
      res,
      tibble::tibble(
        reference_time = tsd$time[i],
        cases = tsd$cases[i],
        season = tsd$season[i],
        population = if ("population" %in% names(tsd)) tsd$population[i] else NA_real_,
        incidence = if ("incidence" %in% names(tsd)) tsd$incidence[i] else NA_real_,
        proportion = if ("proportion" %in% names(tsd)) tsd$proportion[i] else NA_real_,
        samples = if ("samples"  %in% names(tsd)) tsd$samples[i]  else NA_real_,
        growth_rate = growth_rates$estimate[1],
        lower_growth_rate = growth_rates$estimate[2],
        upper_growth_rate = growth_rates$estimate[3],
        growth_warning = growth_warning,
        average_observations_window = average_observations_window,
        average_observations_warning = average_observations_warning,
        seasonal_onset_alarm = seasonal_onset_alarm,
        skipped_window = skipped_window[i],
        converged = growth_rates$fit$converged
      )
    )
  }

  # Extract seasons from onset_output and create seasonal_onset. When no
  # disease_threshold is supplied, seasonal_onset_alarm is FALSE for every row
  # and seasonal_onset is therefore consistently present but always FALSE.
  res <- res |>
    dplyr::mutate(
      seasonal_onset_alarm = tidyr::replace_na(.data$seasonal_onset_alarm, FALSE),
      onset_flag = cumsum(.data$seasonal_onset_alarm),
      seasonal_onset = .data$onset_flag == 1 & !duplicated(.data$onset_flag),
      .by = "season"
    ) |>
    dplyr::select(-"onset_flag")

  # Create as tibble with an`tsd_onset` class
  # Add attributes, and keep attributes from the `tsd` class
  ans <- tibble::new_tibble(
    x = res,
    k = k,
    level = level,
    disease_threshold = disease_threshold,
    family = family,
    time_interval = attr(tsd, "time_interval"),
    incidence_denominator = incidence_denominator,
    model_outcome = model_outcome
  )

  structure(
    ans,
    class = c("tsd_onset", class(ans))
  )
}
