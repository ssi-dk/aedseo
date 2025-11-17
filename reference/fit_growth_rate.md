# Fit a growth rate model to time series cases.

This function fits a growth rate model to time series cases and provides
parameter estimates along with confidence intervals.

## Usage

``` r
fit_growth_rate(
  cases,
  population = NULL,
  level = 0.95,
  family = c("quasipoisson", "poisson")
)
```

## Arguments

- cases:

  An integer vector containing the time series cases.

- population:

  An integer vector containing the time series background population.

- level:

  The confidence level for parameter estimates, a numeric value between
  0 and 1.

- family:

  A character string specifying the family for modeling. Choose between
  'poisson', or 'quasipoisson'. Must be one of: character,
  family-generator, or family object.

## Value

A list containing:

- 'fit': The fitted growth rate model.

- 'estimate': A numeric vector with parameter estimates, including the
  growth rate and its confidence interval.

- 'level': The confidence level used for estimating parameter confidence
  intervals.

## Examples

``` r
# Fit a growth rate model to a time series of counts
# (e.g., population growth)
data <- c(100, 120, 150, 180, 220, 270)
fit_growth_rate(
  cases = data,
  level = 0.95,
  family = "poisson"
)
#> $fit
#> 
#> Call:  stats::glm(formula = stats::reformulate(response = "cases", termlabels = terms), 
#>     family = fam_obj, data = growth_data)
#> 
#> Coefficients:
#> (Intercept)  growth_rate  
#>      4.4008       0.1992  
#> 
#> Degrees of Freedom: 5 Total (i.e. Null);  4 Residual
#> Null Deviance:       116.2 
#> Residual Deviance: 0.04923   AIC: 45.67
#> 
#> $estimate
#> growth_rate       2.5 %      97.5 % 
#>   0.1992211   0.1624836   0.2362807 
#> 
#> $level
#> [1] 0.95
#> 
```
