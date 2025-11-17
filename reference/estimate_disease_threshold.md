# Estimate the disease specific threshold of your time series data

This function estimates the disease specific threshold, based on
previous seasons. If the disease threshold is estimated between \]0:1\]
it will be set to 1.

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

- ...:

  Arguments passed to the
  [`seasonal_onset()`](https://ssi-dk.github.io/aedseo/reference/seasonal_onset.md)
  or
  [`fit_percentiles()`](https://ssi-dk.github.io/aedseo/reference/fit_percentiles.md)
  function. `only_current_season = FALSE` and
  `disease_threshold = NA_real_` cannot be changed in
  [`seasonal_onset()`](https://ssi-dk.github.io/aedseo/reference/seasonal_onset.md).

## Value

An object of class `tsd_disease_threshold`, containing; ....

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
#> [1] "2020/2021" "2021/2022" "2022/2023"
#> 
#> $disease_threshold
#> [1] 4.525448
#> 
#> $optim
#> $optim$conf_levels
#> [1] 0.25 0.50 0.75
#> 
#> $optim$values
#> [1]  4.525448 17.260926 65.836479
#> 
#> $optim$par
#> [1] 2.848445 1.984802
#> 
#> $optim$obj_value
#> [1] 12.08436
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
#> # A tibble: 121 × 14
#>    reference_time cases season    population incidence growth_rate
#>    <date>         <dbl> <chr>     <lgl>      <lgl>           <dbl>
#>  1 2021-01-29       175 2020/2021 NA         NA            0.111  
#>  2 2021-02-05       173 2020/2021 NA         NA            0.00175
#>  3 2021-02-12       191 2020/2021 NA         NA            0.0263 
#>  4 2021-02-19       203 2020/2021 NA         NA            0.0554 
#>  5 2021-02-26       222 2020/2021 NA         NA            0.0644 
#>  6 2021-03-05       217 2020/2021 NA         NA            0.0592 
#>  7 2021-03-12       202 2020/2021 NA         NA            0.0174 
#>  8 2021-03-19       183 2020/2021 NA         NA           -0.0292 
#>  9 2021-03-26       254 2020/2021 NA         NA            0.0139 
#> 10 2021-04-02       185 2020/2021 NA         NA           -0.00576
#> # ℹ 111 more rows
#> # ℹ 8 more variables: lower_growth_rate <dbl>, upper_growth_rate <dbl>,
#> #   growth_warning <lgl>, average_observations_window <dbl>,
#> #   average_observations_warning <lgl>, seasonal_onset_alarm <lgl>,
#> #   skipped_window <lgl>, converged <lgl>
#> 
#> attr(,"class")
#> [1] "tsd_disease_threshold"
```
