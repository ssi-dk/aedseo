# Summarises estimates like seasonal peak and onset from all available seasons

This function summarises peak timing and seasonal onset from estimates
in a `tsd_onset` object. This can be useful for investigating if the
current season falls within estimates from previous seasons or if it is
very distinct from previous seasons.

Uses data from a `tsd_onset` object (output from
[`seasonal_onset()`](https://ssi-dk.github.io/aedseo/reference/seasonal_onset.md)).

[`seasonal_onset()`](https://ssi-dk.github.io/aedseo/reference/seasonal_onset.md)
has to be run with arguments;

- disease_threshold

- season_start

- season_end

- only_current_season = FALSE

## Usage

``` r
historical_summary(onset_output)
```

## Arguments

- onset_output:

  A `tsd_onset` object returned from
  [`seasonal_onset()`](https://ssi-dk.github.io/aedseo/reference/seasonal_onset.md).

## Value

An object of class `historical_summary`, containing:

- Usual time to seasonal peak (weeks after onset)

- The week in which the peak usually falls

- Usual peak intensity

- The week in which the onset usually falls

- Usual onset intensity and growth rate estimates If the season does not
  have an onset, it will not be included in the summary.

## Examples

``` r
# Generate simulated data of seasonal waves
sim_data <- generate_seasonal_data(
  years = 5,
  start_date = as.Date("2022-05-26"),
  trend_rate = 1.002,
  noise_overdispersion = 1.1
)

# Estimate seasonal onset
tsd_onset <- seasonal_onset(
  tsd = sim_data,
  disease_threshold = 20,
  family = "quasipoisson",
  season_start = 21,
  season_end = 20,
  only_current_season = FALSE
)

# Get historical summary
historical_summary(tsd_onset)
#> # A tibble: 5 × 10
#>   season    onset_time peak_time  peak_intensity lower_growth_rate_onset
#>   <chr>     <date>     <date>              <dbl>                   <dbl>
#> 1 2022/2023 2022-06-23 2022-08-04            219                  0.0656
#> 2 2023/2024 2023-05-25 2023-08-10            239                  0.0407
#> 3 2024/2025 2024-05-23 2024-08-08            281                  0.0470
#> 4 2025/2026 2025-05-22 2025-08-14            292                  0.0566
#> 5 2026/2027 2026-05-21 2026-07-16            321                  0.0886
#> # ℹ 5 more variables: growth_rate_onset <dbl>, upper_growth_rate_onset <dbl>,
#> #   onset_week <dbl>, peak_week <dbl>, weeks_to_peak <dbl>
```
