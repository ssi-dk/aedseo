# Fit a growth rate model to time series observations.

This function fits a growth rate model to time series observations and
provides parameter estimates along with confidence intervals. For
binomial data use `family = "binomial"` or `family = "quasibinomial"`.

## Usage

``` r
fit_growth_rate(
  cases,
  denominator = NULL,
  level = 0.95,
  family = c("quasipoisson", "poisson", "quasibinomial", "binomial")
)
```

## Arguments

- cases:

  An integer vector containing cases.

- denominator:

  An integer vector containing population or binomial sample size. This
  is mandatory for binomial models

- level:

  The confidence level for parameter estimates, a numeric value between
  0 and 1.

- family:

  A character string, family-generator, or family object specifying the
  distribution family for growth-rate modeling. Choose between
  'poisson', 'quasipoisson', 'binomial', or 'quasibinomial'. Use
  'poisson' or 'quasipoisson' for cases/incidences, and use 'binomial'
  or 'quasibinomial' for binomial data supplied as `cases` and
  `samples`.

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
#> Call:  stats::glm(formula = stats::as.formula(paste(response, "~", paste(terms, 
#>     collapse = " + "))), family = fam_obj, data = growth_data)
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

# Fit a binomial growth rate model to successes out of trials
fit_growth_rate(
  cases = c(1, 2, 3, 4),
  denominator = c(10, 10, 10, 10),
  family = "binomial"
)
#> $fit
#> 
#> Call:  stats::glm(formula = stats::as.formula(paste(response, "~", paste(terms, 
#>     collapse = " + "))), family = fam_obj, data = growth_data)
#> 
#> Coefficients:
#> (Intercept)  growth_rate  
#>     -2.6137       0.5666  
#> 
#> Degrees of Freedom: 3 Total (i.e. Null);  2 Residual
#> Null Deviance:       2.8 
#> Residual Deviance: 0.05243   AIC: 13.75
#> 
#> $estimate
#> growth_rate       2.5 %      97.5 % 
#>   0.5666205  -0.1000528   1.3345240 
#> 
#> $level
#> [1] 0.95
#> 
```
