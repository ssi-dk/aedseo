% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/combined_seasonal_output.R
\name{combined_seasonal_output}
\alias{combined_seasonal_output}
\title{Compute seasonal onset and burden levels from seasonal time series observations.}
\usage{
combined_seasonal_output(
  tsd,
  disease_threshold = 20,
  family = c("quasipoisson", "poisson"),
  family_quant = c("lnorm", "weibull", "exp"),
  season_start = 21,
  season_end = season_start - 1,
  only_current_season = TRUE,
  ...
)
}
\arguments{
\item{tsd}{An object containing time series data with 'time' and 'observation.'}

\item{disease_threshold}{An integer specifying the threshold for considering a disease outbreak. For seasonal onset it defines the per time-step disease threshold that has to be surpassed to possibly
trigger a seasonal onset alarm. If the total number of cases in a window of size k exceeds
\code{disease_threshold * k}, a seasonal onset alarm can be triggered. For burden levels it defines the per
time-step disease threshold that has to be surpassed for the observation to be included in the level
calculations.}

\item{family}{A character string specifying the family for modeling. Choose between 'poisson', or 'quasipoisson'.
Must be one of: character, family-generator, or family object.  This is passed to 'seasonal_onset()'.}

\item{family_quant}{A character string specifying the family for modeling burden levels.}

\item{season_start, season_end}{Integers giving the start and end weeks of the seasons to
stratify the observations by.}

\item{only_current_season}{Should the output only include results for the current season?}

\item{...}{Arguments passed to \code{seasonal_burden_levels()}, \code{fit_percentiles()} and \code{seasonal_onset()} functions.}
}
\value{
An object containing two lists: onset_output and burden_output:

onset_output:

A \code{seasonal_onset} object containing:
\itemize{
\item 'reference_time': The time point for which the growth rate is estimated.
\item 'observation': The observation at reference time point.
\item 'population': The population at reference time point.
\item 'season': The stratification of observables in corresponding seasons.
\item 'growth_rate': The estimated growth rate.
\item 'lower_growth_rate': The lower bound of the growth rate's confidence interval.
\item 'upper_growth_rate': The upper bound of the growth rate's confidence interval.
\item 'growth_warning': Logical. Is the growth rate significantly higher than zero?
\item 'sum_of_cases': The sum of cases within the time window.
\item 'sum_of_cases_warning': Logical. Does the Sum of Cases exceed the disease threshold?
\item 'seasonal_onset_alarm': Logical. Is there a seasonal onset alarm?
\item 'skipped_window': Logical. Was the window skipped due to missing?
\item 'converged': Logical. Was the IWLS judged to have converged? - 'seasonal_onset': Logical. The first detected seasonal onset in the season?
}

burden_output:

A list containing:
\itemize{
\item 'season': The season that burden levels are calculated for.
\item 'high_conf_level': (only for intensity_level method) The conf_level chosen for the high level.
\item 'conf_levels': (only for peak_level method) The conf_levels chosen to fit the 'low', 'medium', 'high' levels.
\item 'values': A named vector with values for 'very low', 'low', 'medium', 'high' levels.
\item 'par': The fit parameters for the chosen family.
\itemize{
\item par_1:
\itemize{
\item For 'weibull': Shape parameter.
\item For 'lnorm': Mean of the log-transformed observations.
\item For 'exp': Rate parameter.
}
\item 'par_2':
\itemize{
\item For 'weibull': Scale parameter.
\item For 'lnorm': Standard deviation of the log-transformed observations.
\item For 'exp': Not applicable (set to NA).
}
}
\item 'obj_value': The value of the objective function - (negative log-likelihood), which represent the minimized
objective function value from the optimisation. Smaller value equals better optimisation.
\item 'converged': Logical. TRUE if the optimisation converged.
\item 'family': The distribution family used for the optimization.
\itemize{
\item 'weibull': Uses the Weibull distribution for fitting.
\item 'lnorm': Uses the Log-normal distribution for fitting.
\item 'exp': Uses the Exponential distribution for fitting.
\item 'disease_threshold': The input disease threshold, which is also the very low level.
}
}
}
\description{
This function performs automated and early detection of seasonal epidemic onsets and estimates the burden
levels from time series dataset stratified by season. The seasonal onset estimates growth rates for consecutive
time intervals and calculates the sum of cases. The burden levels use the previous seasons to estimate the levels
of the current season.
}
\examples{
# Generate random flu season
generate_flu_season <- function(start = 1, end = 1000) {
  random_increasing_obs <- round(sort(runif(24, min = start, max = end)))
  random_decreasing_obs <- round(rev(random_increasing_obs))

  # Generate peak numbers
  add_to_max <- c(50, 100, 200, 100)
  peak <- add_to_max + max(random_increasing_obs)

  # Combine into a single observations sequence
  observations <- c(random_increasing_obs, peak, random_decreasing_obs)

 return(observations)
}

season_1 <- generate_flu_season()
season_2 <- generate_flu_season()

start_date <- as.Date("2022-05-29")
end_date <- as.Date("2024-05-20")

weekly_dates <- seq.Date(from = start_date,
                         to = end_date,
                         by = "week")

tsd_data <- tsd(
  observation = c(season_1, season_2),
  time = as.Date(weekly_dates),
  time_interval = "week"
)

# Run the main function
combined_data <- combined_seasonal_output(tsd_data)
# Print seasonal onset results
print(combined_data$onset_output)
# Print burden level results
print(combined_data$burden_output)
}
