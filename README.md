
<!-- README.md is generated from README.Rmd. Please edit that file -->

# aedseo <a href="https://telkamp7.github.io/aedseo/"><img src="man/figures/logo.png" align="right" height="139" alt="aedseo website" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/telkamp7/aedseo/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/telkamp7/aedseo/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/telkamp7/aedseo/branch/master/graph/badge.svg)](https://app.codecov.io/gh/telkamp7/aedseo?branch=master)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Description

The Automated and Early Detection of Seasonal Epidemic Onset (`aedseo`)
Package provides a powerful tool for automating the early detection of
seasonal epidemic onsets in time series data. It offers the ability to
estimate growth rates for consecutive time intervals and calculate the
Sum of Cases (SoC) within those intervals. This package is particularly
useful for epidemiologists, public health professionals, and researchers
seeking to identify and respond to seasonal epidemics in a timely
fashion.

## Installation

``` r
# Install aedseo from CRAN
install.packages("aedseo")
```

### Development vestion

You can install the development version of `aedseo` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("telkamp7/aedseo")
```

## Getting started

To quickly get started with `aedseo`, follow these steps:

1.  Install the package using the code provided above.
2.  Load the package with `library(aedseo)`.
3.  Create a time series data object (`tsibble`) from your data using
    the `aedseo::tsd` function.
4.  Apply the `aedseo::aedseo` function to estimate growth rates and
    detect seasonal epidemic onsets.

``` r
# Load the package
library(aedseo)

# Create a tsibble object from your data
tsd_data <- tsd(
  observed = c(100, 120, 150, 180, 220, 270),
  time = as.Date(c(
    "2023-01-01",
    "2023-01-02",
    "2023-01-03",
    "2023-01-04",
    "2023-01-05",
    "2023-01-06")
    ),
    time_interval = "day"
  )

# Detect seasonal epidemic onsets
aedseo_results <- aedseo(tsd = tsd_data, k = 3, level = 0.95, family = "poisson")
```

## Vignette

For a more detailed introduction to the workflow of this package, see
the introductory vignette.

``` r
# After installing the package
vignette("aedseo_introduction", package = "aedseo")
```

## Contributing

We welcome contributions to the `aedseo` package. Feel free to open
issues, submit pull requests, or provide feedback to help us improve.
