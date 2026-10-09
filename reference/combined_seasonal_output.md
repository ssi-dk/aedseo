# Compute seasonal onset and burden levels from seasonal time series observations.

This function performs automated and early detection of seasonal
epidemic onsets and estimates the burden levels from time series dataset
stratified by season. The seasonal onset estimates growth rates for
consecutive time intervals and calculates the average sum of
cases/incidence in consecutive time intervals (`k`). The burden levels
use the previous seasons to estimate the levels of the current season.
Output will be in incidence if `population` and `incidence` are assigned
in input.

## Usage

``` r
combined_seasonal_output(
  tsd,
  disease_threshold = 20,
  family = c("quasipoisson", "poisson", "quasibinomial", "binomial"),
  family_quant = NULL,
  season_start = 21,
  season_end = season_start - 1,
  only_current_season = TRUE,
  multiple_waves = FALSE,
  burden_level_decrease = c("low", "very low", "medium", "high"),
  steps_with_decrease = 2,
  ...
)
```

## Arguments

- tsd:

  A `tsd` object containing time series data

- disease_threshold:

  A number specifying the threshold for considering a disease outbreak.
  Should be given as incidence if `population` and
  `incidence_denominator` are in the `tsd` object else as cases. For
  seasonal onset it defines the per time-step disease threshold that has
  to be surpassed to possibly trigger a seasonal onset alarm. If the
  average observation count in a window of size k exceeds
  `disease_threshold`, a seasonal onset alarm can be triggered. For
  burden levels it defines the per time-step disease threshold that has
  to be surpassed for the observation to be included in the level
  calculations.

- family:

  A character string, family-generator, or family object specifying the
  distribution family for growth-rate modeling. Choose between
  'poisson', 'quasipoisson', 'binomial', or 'quasibinomial'. Use
  'poisson' or 'quasipoisson' for cases/incidences, and use 'binomial'
  or 'quasibinomial' for binomial data supplied as `cases` and
  `samples`. This is passed to 'seasonal_onset()'.

- family_quant:

  A character string specifying the family for modeling burden levels.
  Choose between 'lnorm', 'weibull', 'exp', or 'beta'. Use 'lnorm',
  'weibull' or 'exp' for count data, or use 'beta' for
  proportional/binomial data.

- season_start, season_end:

  Integers giving the start and end weeks of the seasons to stratify the
  observations by.

- only_current_season:

  Should the output only include results for the current season?

- multiple_waves:

  A logical. Should the output contain multiple waves?

- burden_level_decrease:

  A character string specifying the burden breakpoint the observations
  should decrease under to reach `seasonal_offset` or before a new
  increase in observations can call a new wave onset if `multiple_waves`
  are TRUE. Choose between; "very low", "low", "medium", or "high".

- steps_with_decrease:

  An integer specifying in how many time steps (days, weeks, months) the
  decrease should be observed under the `burden_level_decrease` (if
  there is a sudden decrease followed by an increase it could e.g. be
  due to testing).

- ...:

  Arguments passed to
  [`seasonal_burden_levels()`](https://ssi-dk.github.io/aedseo/reference/seasonal_burden_levels.md),
  [`fit_percentiles()`](https://ssi-dk.github.io/aedseo/reference/fit_percentiles.md)
  and
  [`seasonal_onset()`](https://ssi-dk.github.io/aedseo/reference/seasonal_onset.md)
  functions.

## Value

An `tsd_onset_and_burden` object containing two lists:

onset_output:

A `tsd_onset` object containing:

- 'reference_time': The time point for which the growth rate is
  estimated.

- 'cases': The cases at reference time point.

- 'population': The population at reference time point.

- 'incidence': The incidence at reference time point.

- 'season': The stratification of observables in corresponding seasons.

- 'growth_rate': The estimated growth rate.

- 'lower_growth_rate': The lower bound of the growth rate's confidence
  interval.

- 'upper_growth_rate': The upper bound of the growth rate's confidence
  interval.

- 'growth_warning': Logical. Is the growth rate significantly higher
  than zero?

- 'average_observation_window': The average of cases/incidence, or
  pooled proportion, within the time window.

- 'average_observation_warning': Logical. Does the average observations
  exceed the disease threshold?

- 'seasonal_onset_alarm': Logical. Is there a seasonal onset alarm?

- 'skipped_window': Logical. Was the window skipped due to missing
  observations?

- 'converged': Logical. Was the IWLS judged to have converged?

- 'seasonal_onset': Logical. The first detected seasonal onset in the
  season.

- Attributes: `time_interval`, `incidence_denominator` and
  `model_outcome`.

As extra the `tsd_onset` object will for each season contain a
`seasonal_offset` variable:

- 'seasonal_offset': Logical. The first detected seasonal offset in the
  season.

If multiple waves is selected the `tsd_onset` object will also contain:

- 'wave_number': The wave number in the time series data.

- 'wave_starts': Logical. Did a new wave start?

- 'wave_ends': Logical. Did the wave end?

- 'decrease_counter': How many consecutive time intervals have decreased
  below the selected burden breakpoint.

- 'decrease_value': A numeric specifying the selected burden breakpoint
  value to fall below for ending the wave.

burden_output:

A `tsd_burden_levels` object containing:

- 'season': The season that burden levels are calculated for.

- 'high_conf_level': (only for intensity_level method) The conf_level
  chosen for the high level.

- 'conf_levels': (only for peak_level method) The conf_levels chosen to
  fit the 'low', 'medium', 'high' levels.

- 'values': A named vector with values for 'very low', 'low', 'medium',
  'high' levels.

- 'optim' A list containing:

  - 'par': The fit parameters for the chosen family.

    - par_1:

      - For 'weibull': Shape parameter.

      - For 'lnorm': Mean of the log-transformed observations.

      - For 'exp': Rate parameter.

      - For 'beta': First shape parameter.

    - 'par_2':

      - For 'weibull': Scale parameter.

      - For 'lnorm': Standard deviation of the log-transformed
        observations.

      - For 'exp': Not applicable (set to NA).

      - For 'beta': Second shape parameter.

  - 'obj_value': The value of the objective function - (negative
    log-likelihood), which represent the minimised objective function
    value from the optimisation. Smaller value equals better
    optimisation.

  - 'converged': Logical. TRUE if the optimisation converged.

  - 'family': The distribution family used for the optimization.

    - 'weibull': Uses the Weibull distribution for fitting.

    - 'lnorm': Uses the Log-normal distribution for fitting.

    - 'exp': Uses the Exponential distribution for fitting.

    - 'beta': Uses the Beta distribution for proportional/binomial
      observations.

- 'disease_threshold': The input disease threshold, which is also the
  very low level.

- 'incidence_denominator': only used for count data with population
  given.

- Attributes: `time_interval`, `incidence_denominator` and
  `burden_outcome`.

\#' Attributes in the `tsd_onset_and_burden` object are:
`burden_level_decrease`, `steps_with_decrease` and `multiple_waves`.

## Examples

``` r
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

tsd_data <- to_time_series(
  cases = c(season_1, season_2),
  time = as.Date(weekly_dates)
)

# Run the main function
combined_data <- combined_seasonal_output(tsd_data)
# Print seasonal onset results
print(combined_data$onset_output)
#> # A tibble: 52 × 19
#>    reference_time cases season    population incidence proportion samples
#>  * <date>         <dbl> <chr>          <dbl>     <dbl>      <dbl>   <dbl>
#>  1 2023-05-28         6 2023/2024         NA        NA         NA      NA
#>  2 2023-06-04        81 2023/2024         NA        NA         NA      NA
#>  3 2023-06-11       121 2023/2024         NA        NA         NA      NA
#>  4 2023-06-18       145 2023/2024         NA        NA         NA      NA
#>  5 2023-06-25       157 2023/2024         NA        NA         NA      NA
#>  6 2023-07-02       199 2023/2024         NA        NA         NA      NA
#>  7 2023-07-09       237 2023/2024         NA        NA         NA      NA
#>  8 2023-07-16       385 2023/2024         NA        NA         NA      NA
#>  9 2023-07-23       392 2023/2024         NA        NA         NA      NA
#> 10 2023-07-30       430 2023/2024         NA        NA         NA      NA
#> # ℹ 42 more rows
#> # ℹ 12 more variables: growth_rate <dbl>, lower_growth_rate <dbl>,
#> #   upper_growth_rate <dbl>, growth_warning <lgl>,
#> #   average_observations_window <dbl>, average_observations_warning <lgl>,
#> #   seasonal_onset_alarm <lgl>, skipped_window <lgl>, converged <lgl>,
#> #   seasonal_onset <lgl>, decrease_value <dbl>, seasonal_offset <lgl>
# Print burden level results
print(combined_data$burden_output)
#> $season
#> [1] "2023/2024"
#> 
#> $values
#>   very low        low     medium       high 
#>   20.00000   78.21533  305.88189 1196.23264 
#> 
#> $optim
#> $optim$par
#> [1] 7.00429030 0.04216513
#> 
#> $optim$obj_value
#> [1] 15.7705
#> 
#> $optim$converged
#> [1] TRUE
#> 
#> $optim$high_conf_level
#> [1] 0.975
#> 
#> $optim$family
#> [1] "lnorm"
#> 
#> 
#> $disease_threshold
#> [1] 20
#> 
#> $incidence_denominator
#> [1] NA
#> 
#> $burden_outcome
#> [1] "cases"
#> 
#> attr(,"time_interval")
#> [1] "weeks"
#> attr(,"incidence_denominator")
#> [1] NA
#> attr(,"burden_outcome")
#> [1] "cases"
#> attr(,"class")
#> [1] "tsd_burden_levels" "list"             
```
