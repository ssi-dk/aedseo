# Estimate the disease specific threshold of your time series data

This function estimates the disease specific threshold, based on
previous seasons. For count/incidence data, thresholds estimated below 0
are set to 0. For binomial/proportional data, thresholds remain on the
proportion scale and beta percentiles are used by default.

## Usage

``` r
estimate_disease_threshold(
  tsd,
  season_start = 21,
  season_end = season_start - 1,
  skip_current_season = TRUE,
  min_significant_time = 3,
  max_gap_time = 1,
  use_prev_seasons_num = 3,
  pick_significant_sequence = c("longest", "earliest"),
  season_importance_decay = 0.8,
  conf_levels = c(0.25, 0.5, 0.75),
  family = c("quasipoisson", "poisson", "quasibinomial", "binomial"),
  burden_family = c("lnorm", "weibull", "exp", "beta"),
  ...
)
```

## Arguments

- tsd:

  A `tsd` object containing time series data

- season_start, season_end:

  Integers giving the start and end weeks of the seasons to stratify the
  observations by.

- skip_current_season:

  A logical. Do you want to skip your current season?

- min_significant_time:

  An integer specifying how many time steps that have to be significant
  to the sequence to be considered in estimation.

- max_gap_time:

  A numeric value specifying how many time steps there is allowed to be
  non-significant between two significant sequences for maybe
  considering them as the same sequence. Sometimes e.g. vacations or
  less testing can lead to false decreases.

- use_prev_seasons_num:

  An integer specifying how many previous seasons you want to include in
  estimation.

- pick_significant_sequence:

  A character string specifying which significant sequence to pick from
  each season.

  - `longest`: The longest sequence of size `min_significant_time`
    closest to the peak.

  - `earliest`: The earliest sequence of size `min_significant_time` of
    the season.

- season_importance_decay:

  A numeric value between 0 and 1, that specifies the weight applied to
  previous seasons. It is used as `season_importance_decay`^(number of
  seasons back), whereby the weight for the most recent season will be
  `season_importance_decay`^0 = 1. This parameter allows for a
  decreasing weight assigned to prior seasons, such that the influence
  of older seasons diminishes exponentially.

- conf_levels:

  A numeric vector specifying the confidence levels for parameter
  estimates. The values have to be unique and in ascending order, the
  first percentile is the disease specific threshold. Specify one or
  three confidence levels e.g.: `c(0.25)` `c(0.25, 0.5, 0.75)`.

- family:

  A character string, family-generator, or family object specifying the
  distribution family for growth-rate modeling. Choose between
  'poisson', 'quasipoisson', 'binomial', or 'quasibinomial'. Use
  'poisson' or 'quasipoisson' for cases/incidences, and use 'binomial'
  or 'quasibinomial' for binomial data supplied as `cases` and
  `samples`. Passed to
  [`seasonal_onset()`](https://ssi-dk.github.io/aedseo/reference/seasonal_onset.md)
  and then to
  [`fit_growth_rate()`](https://ssi-dk.github.io/aedseo/reference/fit_growth_rate.md).

- burden_family:

  A character string specifying the family for modeling burden levels.
  Choose between 'lnorm', 'weibull', 'exp', or 'beta'. Use 'lnorm',
  'weibull' or 'exp' for count data, or use 'beta' for
  proportional/binomial data. Passed to
  [`fit_percentiles()`](https://ssi-dk.github.io/aedseo/reference/fit_percentiles.md)
  as its `family` argument.

- ...:

  Arguments passed to the
  [`seasonal_onset()`](https://ssi-dk.github.io/aedseo/reference/seasonal_onset.md)
  or
  [`fit_percentiles()`](https://ssi-dk.github.io/aedseo/reference/fit_percentiles.md)
  function. `only_current_season = FALSE` and
  `disease_threshold = NA_real_` cannot be changed in
  [`seasonal_onset()`](https://ssi-dk.github.io/aedseo/reference/seasonal_onset.md).
  ....

## Value

A `tsd_disease_threshold` object containing:

- 'note': Information about the percentiles fit.

- 'season': The season that the disease threshold is estimated with.

- 'disease_threshold': The disease threshold value.

- 'optim': The arguments used in the
  [`seasonal_burden_levels()`](https://ssi-dk.github.io/aedseo/reference/seasonal_burden_levels.md)
  calculation of the percentiles.

- 'settings': Settings used to estimate the disease specific threshold.

- 'incidence-denominator': only used for count data with population
  given.

- 'time_interval': time interval of the time series data in days, weeks,
  or months.

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

## Examples

``` r
# Generate seasonal data
tsd_data <- generate_seasonal_data(
 years = 3,
 start_date = as.Date("2021-01-01"),
 noise_overdispersion = 3
)

# Estimate disease threshold
estimate_disease_threshold(tsd_data)
#> $note
#> [1] "Sufficient information to estimate percentiles."
#> 
#> $seasons
#> [1] "2021/2022" "2022/2023"
#> 
#> $disease_threshold
#> [1] 4.54741
#> 
#> $optim
#> $optim$conf_levels
#> [1] 0.25 0.50 0.75
#> 
#> $optim$values
#> [1]  4.54741 12.01884 31.76586
#> 
#> $optim$par
#> [1] 2.486475 1.440967
#> 
#> $optim$obj_value
#> [1] 7.6869
#> 
#> $optim$converged
#> [1] TRUE
#> 
#> $optim$family
#> [1] "lnorm"
#> 
#> 
#> $settings
#> $settings$skip_current_season
#> [1] TRUE
#> 
#> $settings$min_significant_time
#> [1] 3
#> 
#> $settings$use_prev_seasons_num
#> [1] 3
#> 
#> $settings$pick_significant_sequence
#> [1] "longest"
#> 
#> $settings$season_importance_decay
#> [1] 0.8
#> 
#> $settings$family
#> [1] "quasipoisson"
#> 
#> $settings$percentiles
#> [1] 0.25 0.50 0.75
#> 
#> 
#> $incidence_denominator
#> [1] NA
#> 
#> $time_interval
#> [1] "weeks"
#> 
#> $onset_output
#> # A tibble: 121 × 17
#>    reference_time cases season    population incidence proportion samples
#>    <date>         <dbl> <chr>          <dbl>     <dbl>      <dbl>   <dbl>
#>  1 2021-01-29       208 2020/2021         NA        NA         NA      NA
#>  2 2021-02-05       155 2020/2021         NA        NA         NA      NA
#>  3 2021-02-12       190 2020/2021         NA        NA         NA      NA
#>  4 2021-02-19       200 2020/2021         NA        NA         NA      NA
#>  5 2021-02-26       241 2020/2021         NA        NA         NA      NA
#>  6 2021-03-05       179 2020/2021         NA        NA         NA      NA
#>  7 2021-03-12       181 2020/2021         NA        NA         NA      NA
#>  8 2021-03-19       230 2020/2021         NA        NA         NA      NA
#>  9 2021-03-26       204 2020/2021         NA        NA         NA      NA
#> 10 2021-04-02       185 2020/2021         NA        NA         NA      NA
#> # ℹ 111 more rows
#> # ℹ 10 more variables: growth_rate <dbl>, lower_growth_rate <dbl>,
#> #   upper_growth_rate <dbl>, growth_warning <lgl>,
#> #   average_observations_window <dbl>, average_observations_warning <lgl>,
#> #   seasonal_onset_alarm <lgl>, skipped_window <lgl>, converged <lgl>,
#> #   seasonal_onset <lgl>
#> 
#> attr(,"time_interval")
#> [1] "weeks"
#> attr(,"incidence_denominator")
#> [1] NA
#> attr(,"class")
#> [1] "tsd_disease_threshold" "list"                 
#> attr(,"model_outcome")
#> [1] "cases"
```
