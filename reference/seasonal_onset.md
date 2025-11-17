# Automated and Early Detection of Seasonal Epidemic Onset

This function performs automated and early detection of seasonal
epidemic onsets on a `tsd` object. It estimates growth rates and
calculates the average sum of cases in consecutive time intervals (`k`).
If the time series data includes `population` it will be used as offset
to adjust the growth rate in the glm, additionally the output will
include incidence, population and average sum of incidence.

## Usage

``` r
seasonal_onset(
  tsd,
  k = 5,
  level = 0.95,
  disease_threshold = NA_real_,
  family = c("quasipoisson", "poisson"),
  na_fraction_allowed = 0.4,
  season_start = NULL,
  season_end = season_start - 1,
  only_current_season = NULL
)
```

## Arguments

- tsd:

  A `tsd` object containing time series data

- k:

  An integer specifying the window size for modeling growth rates and
  average sum of cases.

- level:

  The confidence level for onset parameter estimates, a numeric value
  between 0 and 1.

- disease_threshold:

  A number specifying the threshold for considering a disease outbreak.
  Should be given as incidence if `population` and
  `incidence_denominator` are in the `tsd` object else as cases. It
  defines the per time-step disease threshold that has to be surpassed
  to possibly trigger a seasonal onset alarm. If the average observation
  count in a window of size k exceeds `disease_threshold`, a seasonal
  onset alarm can be triggered.

- family:

  A character string specifying the family for modeling. Choose between
  'poisson', or 'quasipoisson'. Must be one of: character,
  family-generator, or family object.

- na_fraction_allowed:

  Numeric value between 0 and 1 specifying the fraction of observations
  in the window of size k that are allowed to be NA or zero, i.e.
  without cases, in onset calculations.

- season_start, season_end:

  Integers giving the start and end weeks of the seasons to stratify the
  observations by. If set to `NULL`, it means no stratification by
  season.

- only_current_season:

  Should the output only include results for the current season?

## Value

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

- 'average_observation_window': The average of cases or incidence within
  the time window.

- 'average_observation_warning': Logical. Does the average observations
  exceed the disease threshold?

- 'seasonal_onset_alarm': Logical. Is there a seasonal onset alarm?

- 'skipped_window': Logical. Was the window skipped due to missing
  observations?

- 'converged': Logical. Was the IWLS judged to have converged?

- 'seasonal_onset': Logical. The first detected seasonal onset in the
  season.

- Attributes: `time_interval` and `incidence_denominator`.

## Examples

``` r
# Create a tibble object from sample data
tsd_data <- to_time_series(
  cases = c(100, 120, 150, 180, 220, 270),
  time = seq(from = as.Date("2023-01-01"), by = "1 week", length.out = 6)
)

# Estimate seasonal onset with a 3-day window
seasonal_onset(
  tsd = tsd_data,
  k = 3,
  level = 0.975,
  disease_threshold = 5,
  na_fraction_allowed = 0.4,
  season_start = 21,
  season_end = 20,
  only_current_season = FALSE
)
#> # A tibble: 4 × 15
#>   reference_time cases season population incidence growth_rate lower_growth_rate
#>   <date>         <dbl> <chr>  <lgl>      <lgl>           <dbl>             <dbl>
#> 1 2023-01-15       150 2022/… NA         NA              0.204             0.178
#> 2 2023-01-22       180 2022/… NA         NA              0.201             0.175
#> 3 2023-01-29       220 2022/… NA         NA              0.192             0.180
#> 4 2023-02-05       270 2022/… NA         NA              0.203             0.200
#> # ℹ 8 more variables: upper_growth_rate <dbl>, growth_warning <lgl>,
#> #   average_observations_window <dbl>, average_observations_warning <lgl>,
#> #   seasonal_onset_alarm <lgl>, skipped_window <lgl>, converged <lgl>,
#> #   seasonal_onset <lgl>
```
