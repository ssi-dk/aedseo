# Summary method for `tsd_onset` objects

Summarize key results from a seasonal onset analysis.

## Usage

``` r
# S3 method for class 'tsd_onset'
summary(object, ...)
```

## Arguments

- object:

  An object of class 'tsd_onset' containing the results of a
  `seasonal_onset` analysis.

- ...:

  Additional arguments (not used).

## Value

This function is used for its side effect, which is printing a summary
message to the console.

## Examples

``` r
# Create a `tsd` object
tsd_data <- generate_seasonal_data()

# Create a `tsd_onset` object
tsd_onset <- seasonal_onset(
  tsd = tsd_data,
  k = 3,
  disease_threshold = 100,
  season_start = 21,
  season_end = 20,
  level = 0.95,
  only_current_season = TRUE
)
# Print the summary
summary(tsd_onset)
#> Summary of tsd_onset object with disease_threshold
#> 
#>       Model output:
#>         Reference time point (first seasonal onset alarm in season): 2023-05-31
#>         Observations at reference time point: 124
#>         Average observations (in k window) at reference time point: 112
#>         Growth rate estimate at reference time point:
#>           Estimate   Lower (2.5%)   Upper (97.5%)
#>             0.107     0.114          0.101
#>         Total number of growth warnings in the series: 25
#>         Latest growth warning: 2024-05-15
#>         Latest average observations warning: 2023-11-15
#>         Latest seasonal onset alarm: 2023-08-16
#> 
#>       The season for reference time point:
#>         2023/2024
#> 
#>       Model settings:
#>         Called using distributional family: quasipoisson
#>         Window size: 3
#>         The time interval for the observations: weeks
#>         Disease specific threshold: 100
#>         Incidence denominator: NA Summary of tsd_onset object with disease_threshold
#> 
#>       Model output:
#>         Reference time point (first seasonal onset alarm in season): 2023-05-31
#>         Observations at reference time point: 124
#>         Average observations (in k window) at reference time point: 112
#>         Growth rate estimate at reference time point:
#>           Estimate   Lower (2.5%)   Upper (97.5%)
#>             0.107     0.114          0.101
#>         Total number of growth warnings in the series: 25
#>         Latest growth warning: 2024-05-15
#>         Latest average observations warning: 2023-11-15
#>         Latest seasonal onset alarm: 2023-08-16
#> 
#>       The season for reference time point:
#>         2023/2024
#> 
#>       Model settings:
#>         Called using distributional family: poisson
#>         Window size: 3
#>         The time interval for the observations: weeks
#>         Disease specific threshold: 100
#>         Incidence denominator: NA
```
