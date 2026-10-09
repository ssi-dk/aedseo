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

``` r

vignette("aedseo")
```

## Citation

The AEDSEO method is described and externally evaluated in:

Otero Sofia Myrup, Emborg Hanne-Dorthe, Telkamp Kasper Schou,
Moustsen-Helms Ida Rask, Søborg Bolette, Christiansen Lasse Engbo.
**\[Evaluation of the Automated and Early Detection of Seasonal Epidemic
Onset and Burden Levels (AEDSEO) method for respiratory surveillance
using data from 21 European countries\]**. *Eurosurveillance*.
2026;31(30):2500896.
<https://doi.org/10.2807/1560-7917.ES.2026.31.30.2500896>

To cite the `aedseo` package and method, run:

``` r

citation("aedseo")
```

## Contributing

We welcome contributions to the `aedseo` package. Feel free to open
issues, submit pull requests, or provide feedback to help us improve.
