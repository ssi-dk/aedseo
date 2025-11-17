# Summary method for `tsd_burden_levels` objects

Summarize key results from a seasonal burden levels analysis.

## Usage

``` r
# S3 method for class 'tsd_burden_levels'
summary(object, ...)
```

## Arguments

- object:

  An object of class 'tsd_burden_levels' containing the results of a
  `seasonal_burden_levels` analysis.

- ...:

  Additional arguments (not used).

## Value

This function is used for its side effect, which is printing the burden
levels.

## Examples

``` r
# Create a `tsd` object
tsd_data <- generate_seasonal_data()

# Create a `tsd_burden_levels` object
tsd_burden_levels <- seasonal_burden_levels(
  tsd = tsd_data
)
# Print the summary
summary(tsd_burden_levels)
#> Summary of tsd_burden_levels object
#> 
#>     Breakpoint estimates:
#>       very low : 20.000000
#>       low: 43.155941
#>       medium: 93.121762
#>       high: 200.937862
#> 
#>     The season for the burden levels:
#>       2023/2024
#> 
#>     Model settings:
#>       Disease specific threshold: 20
#>       Incidence denominator: NA
#>       Called using distributional family: lnorm
```
