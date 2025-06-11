# Model documentation
rd_cases <- "An integer vector containing the time series cases."
rd_disease_threshold <- function(usage = NULL) {
  paste("An integer specifying the threshold for considering a disease outbreak. Should be given as incidence if
        `population` and `incidence_denominator` are in the `tsd` object else as cases.",
        if (usage == "onset") {
          paste("It defines the per time-step disease threshold that has to be surpassed to possibly trigger a seasonal
          onset alarm. If the total number of cases in a window of size k exceeds `disease_threshold * k`, a seasonal
          onset alarm can be triggered.")
        } else if (usage == "levels") {
          paste("It defines the per time-step disease threshold that has to be surpassed for the observation to be
          included in the level calculations.")
        } else if (usage == "combined") {
          paste("For seasonal onset it defines the per time-step disease threshold that has to be surpassed to possibly
          trigger a seasonal onset alarm. If the total number of cases in a window of size k exceeds
          `disease_threshold * k`, a seasonal onset alarm can be triggered. For burden levels it defines the per
          time-step disease threshold that has to be surpassed for the observation to be included in the level
          calculations.")
        })
}
rd_family <- function(usage = NULL) {
  paste("A character string specifying the family for modeling. Choose between 'poisson', or 'quasipoisson'.
        Must be one of: character, family-generator, or family object.",
        ifelse(usage == "combined", paste(" This is passed to 'seasonal_onset()'.")))
}
rd_only_current_season <- "Should the output only include results for the current season?"
rd_population <- "An integer vector containing the time series background population."
rd_season_start_end <- function(usage = NULL) {
  paste("Integers giving the start and end weeks of the seasons to
  stratify the observations by.",
        ifelse(usage == "onset", paste("If set to `NULL`, it means no stratification by season.")))
}
rd_seasonal_onset_return <- paste(
  "\nA `seasonal_onset` object containing:\n",
  "- 'reference_time': The time point for which the growth rate is estimated.\n",
  "- 'cases': The cases at reference time point.\n",
  "- 'population': The population at reference time point.\n",
  "- 'incidence': The incidence at reference time point.\n",
  "- 'season': The stratification of observables in corresponding seasons.\n",
  "- 'growth_rate': The estimated growth rate.\n",
  "- 'lower_growth_rate': The lower bound of the growth rate's confidence interval.\n",
  "- 'upper_growth_rate': The upper bound of the growth rate's confidence interval.\n",
  "- 'growth_warning': Logical. Is the growth rate significantly higher than zero?\n",
  "- 'average_sum_of_cases': The sum of cases within the time window.\n",
  "- 'average_sum_of_cases_warning': Logical. Does the Average Sum of Cases exceed the disease threshold?\n",
  "- 'seasonal_onset_alarm': Logical. Is there a seasonal onset alarm?\n",
  "- 'skipped_window': Logical. Was the window skipped due to missing?\n",
  "- 'converged': Logical. Was the IWLS judged to have converged?",
  "- 'seasonal_onset': Logical. The first detected seasonal onset in the season?"
)
rd_seasonal_burden_levels_return <- paste(
  "\nA list containing:\n",
  "- 'season': The season that burden levels are calculated for.\n",
  "- 'high_conf_level': (only for intensity_level method) The conf_level chosen for the high level.\n",
  "- 'conf_levels': (only for peak_level method) The conf_levels chosen to fit the 'low', 'medium', 'high' levels.\n",
  "- 'values': A named vector with values for 'very low', 'low', 'medium', 'high' levels.\n",
  "- 'par': The fit parameters for the chosen family.\n",
  "    - par_1:\n",
  "       - For 'weibull': Shape parameter.\n",
  "       - For 'lnorm': Mean of the log-transformed observations.\n",
  "       - For 'exp': Rate parameter.\n",
  "    - 'par_2':\n",
  "       - For 'weibull': Scale parameter.\n",
  "       - For 'lnorm': Standard deviation of the log-transformed observations.\n",
  "       - For 'exp': Not applicable (set to NA).\n",
  "- 'obj_value': The value of the objective function - (negative log-likelihood), which represent the minimized\n",
  "objective function value from the optimisation. Smaller value equals better optimisation.\n",
  "- 'converged': Logical. TRUE if the optimisation converged.\n",
  "- 'family': The distribution family used for the optimization.\n",
  "   - 'weibull': Uses the Weibull distribution for fitting.\n",
  "   - 'lnorm': Uses the Log-normal distribution for fitting.\n",
  "   - 'exp': Uses the Exponential distribution for fitting.\n",
  "   - 'disease_threshold': The input disease threshold, which is also the very low level."
)
rd_tsd <- "A `tsd` object containing time series data"

# Autoplot and generate data documentation
rd_disease_color <- "A character specifying the base color of the disease."
rd_text_family <- "A character specifying the font family for the text labels."
rd_legend_position <- "A character specifying the position of the legend on the plot."
rd_line_width <- "A numeric specifying the width of line connecting observations."
rd_obs_size <- "A numeric, specifying the size of observational points."
rd_time_interval <- "A character vector specifying the time interval. Choose between 'day', 'week', or 'month'."
rd_time_interval_step <- "A character vector specifying the time interval and how many time steps are desired
 on the x-axis, e.g. '10 days', '4 weeks', or '3 months'."
rd_y_label <- "A character vector specifying the y label text."
