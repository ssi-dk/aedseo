# Compute burden levels from seasonal time series observations of current season.

This function estimates the burden levels of time series observations
that are stratified by season. It uses the previous seasons to estimate
the levels of the current season. The output is results regarding the
current season in the time series observations. NOTE: The data must
include data for a complete previous season to make predictions for the
current season. Observations will be incidence if `population` and
`incidence` are available in the `tsd` object.

## Usage

``` r
seasonal_burden_levels(
  tsd,
  family = c("lnorm", "weibull", "exp"),
  season_start = 21,
  season_end = season_start - 1,
  method = c("intensity_levels", "peak_levels"),
  conf_levels = 0.95,
  decay_factor = 0.8,
  disease_threshold = 20,
  n_peak = 6,
  only_current_season = TRUE,
  ...
)
```

## Arguments

- tsd:

  A `tsd` object containing time series data

- family:

  A character string specifying the family for modeling. Choose between
  'poisson', or 'quasipoisson'. Must be one of: character,
  family-generator, or family object.

- season_start, season_end:

  Integers giving the start and end weeks of the seasons to stratify the
  observations by.

- method:

  A character string specifying the model to be used in the level
  calculations. Both model predict the levels of the current series of
  observations.

  - `intensity_levels`: models the risk compared to what has been
    observed in previous seasons.

  - `peak_levels`: models the risk compared to what has been observed in
    the `n_peak` observations each season.

- conf_levels:

  A numeric vector specifying the confidence levels for parameter
  estimates. The values have to be unique and in ascending order, (i.e.
  the lowest level is first and highest level is last). The
  `conf_levels` are specific for each method:

  - for `intensity_levels` only specify the highest confidence level
    e.g.: `0.95`, which is the highest intensity that has been observed
    in previous seasons.

  - for `peak_levels` specify three confidence levels e.g.:
    `c(0.4, 0.9, 0.975)`, which are the three confidence levels low,
    medium and high that reflect the peak severity relative to those
    observed in previous seasons.

- decay_factor:

  A numeric value between 0 and 1, that specifies the weight applied to
  previous seasons in level calculations. It is used as
  `decay_factor`^(number of seasons back), whereby the weight for the
  most recent season will be `decay_factor`^0 = 1. This parameter allows
  for a decreasing weight assigned to prior seasons, such that the
  influence of older seasons diminishes exponentially.

- disease_threshold:

  A number specifying the threshold for considering a disease outbreak.
  Should be given as incidence if `population` and
  `incidence_denominator` are in the `tsd` object else as cases. It
  defines the per time-step disease threshold that has to be surpassed
  for the observation to be included in the level calculations.

- n_peak:

  A numeric value specifying the number of peak observations to be
  selected from each season in the level calculations. The `n_peak`
  observations have to surpass the `disease_threshold` to be included.

- only_current_season:

  Should the output only include results for the current season?

- ...:

  Arguments passed to the
  [`fit_percentiles()`](https://ssi-dk.github.io/aedseo/reference/fit_percentiles.md)
  function.

## Value

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

    - 'par_2':

      - For 'weibull': Scale parameter.

      - For 'lnorm': Standard deviation of the log-transformed
        observations.

      - For 'exp': Not applicable (set to NA).

  - 'obj_value': The value of the objective function - (negative
    log-likelihood), which represent the minimised objective function
    value from the optimisation. Smaller value equals better
    optimisation.

  - 'converged': Logical. TRUE if the optimisation converged.

  - 'family': The distribution family used for the optimization.

    - 'weibull': Uses the Weibull distribution for fitting.

    - 'lnorm': Uses the Log-normal distribution for fitting.

    - 'exp': Uses the Exponential distribution for fitting.

- 'disease_threshold': The input disease threshold, which is also the
  very low level.

- 'incidence_denominator': The observations per incidence-denominator.

- Attributes: `time_interval` and `incidence_denominator`.

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

# Print seasonal burden results
seasonal_burden_levels(tsd_data, family = "lnorm")
#> $season
#> [1] "2023/2024"
#> 
#> $values
#>   very low        low     medium       high 
#>   20.00000   77.96614  303.93594 1184.83559 
#> 
#> $optim
#> $optim$par
#> [1] 6.97243417 0.06378995
#> 
#> $optim$obj_value
#> [1] 33.83526
#> 
#> $optim$converged
#> [1] TRUE
#> 
#> $optim$high_conf_level
#> [1] 0.95
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
#> attr(,"class")
#> [1] "tsd_burden_levels"
#> attr(,"time_interval")
#> [1] "weeks"
#> attr(,"incidence_denominator")
#> [1] NA
```
