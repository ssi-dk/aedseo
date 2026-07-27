#' Generate Simulated Data of Seasonal Waves as a `tsd` object
#'
#' @description
#'
#' This function generates a simulated dataset of seasonal waves with trend and noise.
#' This function assumes 365 days, 52 weeks, and 12 months per year. Leap years are not included in the calculation.
#'
#' @param years An integer specifying the number of years of data to simulate.
#' @param start_date A date representing the start date of the simulated data.
#' @param amplitude A number specifying the amplitude of the seasonal wave.
#' The output will fluctuate within the range `[mean - amplitude, mean + amplitude]`.
#' @param mean A number specifying the mean of the seasonal wave.
#' @param phase A numeric value (in radians) representing the horizontal shift
#' of the sine wave, hence the phase shift of the seasonal wave. The phase must be between zero and 2*pi.
#' @param trend_rate A numeric value specifying the exponential growth/decay rate.
#' @param noise_overdispersion A numeric value specifying the overdispersion of the generated data.
#' 0 means deterministic, 1 gives Poisson or binomial noise, and values greater than one use a negative
#' binomial distribution for count data or a beta-binomial distribution for binomial data.
#' @param relative_epidemic_concentration A numeric that transforms the reference sinusoidal season.
#' A value of 1 gives the pure sinusoidal curve, and greater values concentrate the epidemic around the peak.
#' @param time_interval `r rd_time_interval`
#' @param lower_bound A numeric value that can be used to ensure that intensities are always greater than zero,
#' which is needed when `noise_overdispersion` is different from zero.
#' @param samples An optional positive integer specifying the number of samples tested at each time point.
#' When supplied, `mean`, `amplitude`, and the resulting seasonal wave are interpreted as proportions,
#' and cases are drawn from a binomial distribution. In this mode, `noise_overdispersion = 1` (or `NULL`)
#' gives binomial variation, values greater than one give quasi-binomial variation using a beta-binomial
#' distribution, and zero gives deterministic rounded counts.
#'
#' @return A `tsd` object with simulated data containing:
#'   - 'time': The time point for the corresponding data.
#'   - 'cases': The number of cases at the time point.
#'   - 'proportion': The proportion of positive samples for binomial data. (optional)
#'   - 'samples': The number of samples tested for binomial data. (optional)
#'
#' @export
#'
#' @examples
#' # Generate simulated data of seasonal waves
#'
#' #With default arguments
#' default_sim <- generate_seasonal_data()
#' plot(default_sim)
#'
#' #With an exponential growth rate trend
#' trend_sim <- generate_seasonal_data(trend_rate = 1.001)
#' plot(trend_sim)
#'
#' #With noise
#' noise_sim <- generate_seasonal_data(noise_overdispersion = 2)
#' plot(noise_sim)
#'
#' # With binomial data (positive samples out of samples tested)
#' binomial_sim <- generate_seasonal_data(mean = 0.3, amplitude = 0.2, samples = 100)
#' plot(binomial_sim)
#'
#' #With distinct parameters, trend and noise
#' sim_data <- generate_seasonal_data(
#'   years = 2,
#'   start_date = as.Date("2022-05-26"),
#'   amplitude = 2000,
#'   mean = 3000,
#'   trend_rate = 1.002,
#'   noise_overdispersion = 1.1,
#'   time_interval = c("weeks")
#' )
#' plot(sim_data, time_interval = "2 months")
generate_seasonal_data <- function(
  years = 3,
  start_date = as.Date("2021-05-26"),
  amplitude = 100,
  mean = 100,
  phase = 0,
  trend_rate = NULL,
  noise_overdispersion = NULL,
  relative_epidemic_concentration = 1,
  time_interval = c("weeks", "days", "months"),
  lower_bound = 1e-6,
  samples = NULL
) {
  # Check input arguments
  coll <- checkmate::makeAssertCollection()
  checkmate::assert_integerish(years, len = 1, lower = 1, add = coll)
  checkmate::assert_date(start_date, add = coll)
  checkmate::assert_numeric(amplitude, len = 1, lower = 0, add = coll)
  checkmate::assert_numeric(
    mean,
    len = 1,
    lower = if (is.null(samples)) 1 else 0,
    add = coll
  )
  checkmate::assert_numeric(phase, len = 1, lower = 0, upper = 2 * pi, add = coll)
  checkmate::assert_numeric(trend_rate, len = 1, lower = 0, null.ok = TRUE, add = coll)
  checkmate::assert_numeric(noise_overdispersion, len = 1, lower = 0, null.ok = TRUE, add = coll)
  checkmate::assert_false(ifelse(is.null(noise_overdispersion),
                                 FALSE,
                                 noise_overdispersion > 0 & noise_overdispersion < 1), add = coll)
  checkmate::assert_numeric(relative_epidemic_concentration, len = 1, lower = 0)
  checkmate::assert_numeric(lower_bound, len = 1, lower = 0, add = coll)
  checkmate::assert_integerish(samples, len = 1, lower = 1, null.ok = TRUE, add = coll)
  if (!is.null(samples) && !is.null(noise_overdispersion) && noise_overdispersion >= samples) {
    coll$push("`noise_overdispersion` must be less than `samples` when generating binomial data")
  }
  checkmate::reportAssertions(coll)

  # Throw an error if any of the inputs are not supported
  time_interval <- match.arg(time_interval)

  if (grepl(time_interval, "weeks")) {
    period <- 52
  } else if (grepl(time_interval, "days")) {
    period <- 365
  } else if (grepl(time_interval, "months")) {
    period <- 12
  }

  # Define time sequence
  t <- 1:(years * period)

  # Generate the seasonal component
  seasonal_component <- mean + amplitude *
    (((sin(2 * pi * t / period + phase) + 1)^relative_epidemic_concentration) /
       2^(relative_epidemic_concentration - 1) - 1)

  # Add the trend component
  if (!is.null(trend_rate)) {
    trend_component <- exp(log(trend_rate) * t)

    # Combine trend and seasonal components
    seasonal_component <- seasonal_component * trend_component
  }

  # Applying lower bound
  seasonal_component <- pmax(seasonal_component, lower_bound)

  if (!is.null(samples) && any(seasonal_component > 1)) {
    stop("The generated proportion must be between zero and one when `samples` is supplied.", call. = FALSE)
  }

  # Add random noise if specified
  if (!is.null(samples)) {
    samples <- rep_len(samples, length(t))
    if (!is.null(noise_overdispersion) && noise_overdispersion == 0) {
      seasonal_component <- round(samples * seasonal_component)
    } else {
      binomial_probability <- seasonal_component

      if (!is.null(noise_overdispersion) && noise_overdispersion > 1) {
        # A beta-binomial has variance phi * n * p * (1 - p), where
        # phi = 1 + (n - 1) * rho and rho is the intra-class correlation.
        beta_concentration <- (samples - noise_overdispersion) / (noise_overdispersion - 1)
        variable_probability <- seasonal_component > 0 & seasonal_component < 1
        binomial_probability[variable_probability] <- stats::rbeta(
          n = sum(variable_probability),
          shape1 = seasonal_component[variable_probability] *
            beta_concentration[variable_probability],
          shape2 = (1 - seasonal_component[variable_probability]) *
            beta_concentration[variable_probability]
        )
      }

      seasonal_component <- stats::rbinom(
        n = length(t),
        size = samples,
        prob = binomial_probability
      )
    }
  } else if (!is.null(noise_overdispersion) && noise_overdispersion != 0) {
    if (noise_overdispersion == 1) {
      seasonal_component <- stats::rpois(n = length(t), lambda = seasonal_component)
    } else {
      # p = 1/dispersion, n = mu * p / (1-p)
      seasonal_component <-
        stats::rnbinom(
          n = length(t),
          size = seasonal_component * (1 / noise_overdispersion) / (1 - 1 / noise_overdispersion),
          prob = 1 / noise_overdispersion
        )
    }
  }

  # Create arbitrary dates
  dates <- seq.Date(from = start_date, by = time_interval, length.out = length(t))

  # Ensure no negative values
  seasonal_component[seasonal_component < 0] <- NA

  # Construct a 'tsd' object with the time series data
  to_time_series(
    cases = round(seasonal_component),
    samples = samples,
    time = dates,
    time_interval = time_interval
  )
}
