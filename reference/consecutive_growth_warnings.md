# Create a tsd_growth_warning object to count consecutive significant observations

This function calculates the number of consecutive significant
("growth_warning") observations, grouping them accordingly. The result
is stored in an S3 object of class `tsd_growth_warning`.

Uses data from a `tsd_onset` object (output from
[`seasonal_onset()`](https://ssi-dk.github.io/aedseo/reference/seasonal_onset.md)).

[`seasonal_onset()`](https://ssi-dk.github.io/aedseo/reference/seasonal_onset.md)
has to be run with arguments;

- season_start

- season_end

- only_current_season = FALSE

## Usage

``` r
consecutive_growth_warnings(onset_output)
```

## Arguments

- onset_output:

  A `tsd_onset` object returned from
  [`seasonal_onset()`](https://ssi-dk.github.io/aedseo/reference/seasonal_onset.md).

## Value

An object of class `tsd_growth_warning`, containing; A tibble of
processed observations, the significant_counter column specifies when a
sequence of significant observation starts and ends. The first number is
how many subsequent observations will be significant.

## Examples

``` r
# Generate simulated data of seasonal waves
sim_data <- generate_seasonal_data(
  years = 5,
  start_date = as.Date("2022-05-26"),
  trend_rate = 1.002,
  noise_overdispersion = 2,
  relative_epidemic_concentration = 3
)

# Estimate seasonal onset
tsd_onset <- seasonal_onset(
  tsd = sim_data,
  season_start = 21,
  season_end = 20,
  only_current_season = FALSE
)

# Get consecutive significant observations
consecutive_growth_warnings(tsd_onset)
#> # A tibble: 256 × 21
#>    reference_time cases season    population incidence proportion samples
#>    <date>         <dbl> <chr>          <dbl>     <dbl>      <dbl>   <dbl>
#>  1 2022-06-23       101 2022/2023         NA        NA         NA      NA
#>  2 2022-06-30       113 2022/2023         NA        NA         NA      NA
#>  3 2022-07-07       159 2022/2023         NA        NA         NA      NA
#>  4 2022-07-14       152 2022/2023         NA        NA         NA      NA
#>  5 2022-07-21       155 2022/2023         NA        NA         NA      NA
#>  6 2022-07-28       201 2022/2023         NA        NA         NA      NA
#>  7 2022-08-04       197 2022/2023         NA        NA         NA      NA
#>  8 2022-08-11       237 2022/2023         NA        NA         NA      NA
#>  9 2022-08-18       196 2022/2023         NA        NA         NA      NA
#> 10 2022-08-25       235 2022/2023         NA        NA         NA      NA
#> # ℹ 246 more rows
#> # ℹ 14 more variables: growth_rate <dbl>, lower_growth_rate <dbl>,
#> #   upper_growth_rate <dbl>, growth_warning <lgl>,
#> #   average_observations_window <dbl>, average_observations_warning <lgl>,
#> #   seasonal_onset_alarm <lgl>, skipped_window <lgl>, converged <lgl>,
#> #   seasonal_onset <lgl>, counter <dbl>, changeFlag <lgl>, groupID <int>,
#> #   significant_counter <dbl>
```
