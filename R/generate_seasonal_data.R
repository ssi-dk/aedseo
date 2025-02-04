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
#' @param mean A number specifying the mean of the seasonal wave. Must be greater than or equal to the amplitude.
#' @param phase A numeric value (in radians) representing the horizontal shift
#' of the sine wave, hence the phase shift of the seasonal wave. The phase must be between zero and 2*pi.
#' @param trend_rate A numeric value specifying the exponential growth/decay rate.
#' @param noise_sd A numeric value specifying the standard deviation of random noise.
#' @param time_interval `r rd_time_interval`
#'
#' @return A `tsd` object with simulated data containing:
#'   - 'time': The time point for for when the observation is observed.
#'   - 'observation': The observed value at the time point.
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
#' noise_sim <- generate_seasonal_data(noise_sd = 100)
#' plot(noise_sim)
#'
#' #With distinct parameters, trend and noise
#' sim_data <- generate_seasonal_data(
#'   years = 2,
#'   start_date = as.Date("2022-05-26"),
#'   amplitude = 2000,
#'   mean = 3000,
#'   trend_rate = 1.002,
#'   noise_sd = 110,
#'   time_interval = c("week")
#' )
#' plot(sim_data, time_interval = "2 months")
generate_seasonal_data <- function(
  years = 3,
  start_date = as.Date("2021-05-26"),
  amplitude = 1000,
  mean = 1000,
  phase = 0,
  trend_rate = NULL,
  noise_sd = NULL,
  time_interval = c("week", "day", "month")
) {
  # Check input arguments
  time_interval <- rlang::arg_match(time_interval)
  coll <- checkmate::makeAssertCollection()
  checkmate::assert_integerish(years, len = 1, lower = 1, add = coll)
  checkmate::assert_date(start_date, add = coll)
  checkmate::assert_numeric(amplitude, len = 1, lower = 1, add = coll)
  checkmate::assert_numeric(mean, len = 1, lower = 1, add = coll)
  checkmate::assert_true(mean >= amplitude, add = coll)
  checkmate::assert_numeric(phase, len = 1, lower = 0, upper = 2 * pi, add = coll)
  checkmate::assert_numeric(trend_rate, len = 1, lower = 0, null.ok = TRUE, add = coll)
  checkmate::assert_numeric(noise_sd, len = 1, lower = 0, null.ok = TRUE, add = coll)
  checkmate::reportAssertions(coll)

  if (time_interval == "week") {
    period <- 52
  } else if (time_interval == "day") {
    period <- 365
  } else if (time_interval == "month") {
    period <- 12
  }

  # Define time sequence
  t <- 1:(years * period)

  # Generate the seasonal component
  seasonal_component <- mean + amplitude * sin(2 * pi * t / period + phase)

  # Add the trend component
  if (!is.null(trend_rate)) {
    trend_component <- exp(log(trend_rate) * t)

    # Combine trend and seasonal components
    seasonal_component <- seasonal_component * trend_component
  }

  # Add random noise if specified
  if (!is.null(noise_sd)) {
    seasonal_component <- seasonal_component + stats::rnorm(length(t), mean = 0, sd = noise_sd)
  }

  # Create arbitrary dates
  dates <- seq.Date(from = start_date, by = time_interval, length.out = length(t))

  # Ensure no negative values
  seasonal_component[seasonal_component < 0] <- NA

  # Construct a 'tsd' object with the time series data
  sim_tsd_data <- to_time_series(
    observation = round(seasonal_component),
    time = dates,
    time_interval = time_interval
  )
  return(sim_tsd_data)
}
