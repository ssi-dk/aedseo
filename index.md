# aedseo

## Description

The Automated and Early Detection of Seasonal Epidemic Onset and Burden
Levels (`aedseo`) package provides a powerful tool for automating the
early detection of seasonal epidemic onsets in time series data. It
offers the ability to estimate growth rates for consecutive time
intervals. With use of the average observations within those intervals
and an estimated disease-specific threshold it also offers the
possibility to estimate seasonal onset of epidemics. Additionally it
offers the ability to estimate burden levels for seasons based on
historical data. It is aimed towards epidemiologists, public health
professionals, and researchers seeking to identify and respond to
seasonal epidemics in a timely fashion.

## Installation

``` r
# Install aedseo from CRAN
install.packages("aedseo")
```

### Development version

You can install the development version of `aedseo` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ssi-dk/aedseo")
```

## Getting started

To quickly get started with `aedseo`, follow these steps:

1.  Install the package using the code provided above.
2.  Load the package with
    [`library(aedseo)`](https://github.com/ssi-dk/aedseo).
3.  Create a time series data object (`tsd`) from your data using the
    [`to_time_series()`](https://ssi-dk.github.io/aedseo/reference/to_time_series.md)
    function or
    [`generate_seasonal_data()`](https://ssi-dk.github.io/aedseo/reference/generate_seasonal_data.md)
    functions.
4.  Apply the
    [`combined_seasonal_output()`](https://ssi-dk.github.io/aedseo/reference/combined_seasonal_output.md)
    function to get a comprehensive seasonal analysis with seasonal
    onset and burden levels.

## Vignette

For a more detailed introduction to the workflow of this package, see
the `Get Started` vignette or run;
[`vignette("aedseo")`](https://ssi-dk.github.io/aedseo/articles/aedseo.md).

## Contributing

We welcome contributions to the `aedseo` package. Feel free to open
issues, submit pull requests, or provide feedback to help us improve.
