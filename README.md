
<!-- README.md is generated from README.Rmd. Please edit that file -->

# aedseo <a href="https://ssi-dk.github.io/aedseo/"><img src="man/figures/logo.png" align="right" height="139" alt="aedseo website" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/ssi-dk/aedseo/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ssi-dk/aedseo/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/ssi-dk/aedseo/branch/master/graph/badge.svg)](https://app.codecov.io/gh/ssi-dk/aedseo?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/aedseo)](https://CRAN.R-project.org/package=aedseo)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

## Description

The Automated and Early Detection of Seasonal Epidemic Onset and Burden
Levels (`aedseo`) package provides a powerful tool for automating the
early detection of seasonal epidemic onsets in time series data. It
offers the ability to estimate growth rates for consecutive time
intervals and calculate the Sum of Cases (SoC) within those intervals.
With use of a disease-specific threshold it also offers the possibility
to estimate seasonal onset of epidemics. Additionally it offers the
ability to estimate burden levels for seasons based on historical data.
It is aimed towards epidemiologists, public health professionals, and
researchers seeking to identify and respond to seasonal epidemics in a
timely fashion.

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
devtools::install_github("ssi-dk/aedseo")
```

## Getting started

To quickly get started with `aedseo`, follow these steps:

1.  Install the package using the code provided above.
2.  Load the package with `library(aedseo)`.
3.  Create a time series data object (`tsd`) from your data using the
    `to_time_series()` function or `generate_seasonal_data()` functions.
4.  Apply the `combined_seasonal_output()` function to get a
    comprehensive seasonal analysis with seasonal onset and burden
    levels.

## Vignette

For a more detailed introduction to the workflow of this package, see
the `Get Started` vignette or run; `vignette("aedseo")`.

## Contributing

We welcome contributions to the `aedseo` package. Feel free to open
issues, submit pull requests, or provide feedback to help us improve.
