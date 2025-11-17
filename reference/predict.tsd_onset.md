# Predict Cases for Future Time Steps

This function is used to predict future cases based on a `tsd_onset`
object. It uses the `time_interval` attribute from the `tsd_onset`
object to make predictions.

## Usage

``` r
# S3 method for class 'tsd_onset'
predict(object, n_step = 3, ...)
```

## Arguments

- object:

  A `tsd_onset` object created using the
  [`seasonal_onset()`](https://ssi-dk.github.io/aedseo/reference/seasonal_onset.md)
  function.

- n_step:

  An integer specifying the number of future time steps for which you
  want to predict cases.

- ...:

  Additional arguments (not used).

## Value

A tibble-like object called `tsd_predict` containing the predicted
cases, including reference time, lower confidence interval, and upper
confidence interval for the specified number of future time steps.

## Examples

``` r
# Generate predictions of time series data
set.seed(123)
time_series <- generate_seasonal_data(
  years = 1,
  time_interval = "days"
)
# Apply `seasonal_onset` analysis
time_series_with_onset <- seasonal_onset(
  tsd = time_series,
  k = 7
)
# Predict cases for the next 7 time steps
predict(object = time_series_with_onset, n_step = 7)
#> # A tibble: 8 × 5
#>       t reference_time estimate lower upper
#>   <int> <date>            <dbl> <dbl> <dbl>
#> 1     0 2022-05-25         100   100   100 
#> 2     1 2022-05-26         102.  102.  102.
#> 3     2 2022-05-27         104.  103.  104.
#> 4     3 2022-05-28         106.  105.  106.
#> 5     4 2022-05-29         107.  107.  108.
#> 6     5 2022-05-30         109.  109.  110.
#> 7     6 2022-05-31         111.  111.  112.
#> 8     7 2022-06-01         113.  112.  115.
```
