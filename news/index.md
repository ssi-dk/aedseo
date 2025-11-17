# Changelog

## aedseo 1.0.0

### Deprecations

### Features

- Added new arguments `incidence` and `incidence_denominator` to
  [`to_time_series()`](https://ssi-dk.github.io/aedseo/reference/to_time_series.md)
  that allows the user to get output as incidence
  ([\#84](https://github.com/ssi-dk/aedseo/issues/84)).

- Added new argument `population` to
  [`to_time_series()`](https://ssi-dk.github.io/aedseo/reference/to_time_series.md)
  and
  [`fit_growth_rate()`](https://ssi-dk.github.io/aedseo/reference/fit_growth_rate.md)
  that allows the user to add the background population connected to
  each observation ([\#83](https://github.com/ssi-dk/aedseo/issues/83)).

- Added new argument `use_offset` to
  [`seasonal_onset()`](https://ssi-dk.github.io/aedseo/reference/seasonal_onset.md)
  that allows the user to add the background population to adjust the
  growth rate estimations
  ([\#83](https://github.com/ssi-dk/aedseo/issues/83)).

- Added new feature to estimate multiple waves in
  `combined_seasonal_output`
  ([\#77](https://github.com/ssi-dk/aedseo/issues/77)).

- Added
  [`estimate_disease_threshold()`](https://ssi-dk.github.io/aedseo/reference/estimate_disease_threshold.md)
  for users to easier estimate the disease specific threshold
  ([\#85](https://github.com/ssi-dk/aedseo/issues/85)).

### Improvements

- Observations are now divided into `cases` and `incidence`, which is
  implemented into all functions in the package. Cases are used as
  default, but if the user additionally inputs `population` the output
  will be incidence
  ([\#84](https://github.com/ssi-dk/aedseo/issues/84)).

### Minor changes

## aedseo 0.3.0

CRAN release: 2025-04-09

### Deprecations

- [`aedseo()`](https://ssi-dk.github.io/aedseo/reference/aedseo-package.md)
  is now deprecated. Please use
  [`seasonal_onset()`](https://ssi-dk.github.io/aedseo/reference/seasonal_onset.md)
  instead. A warning is shown when using
  [`aedseo()`](https://ssi-dk.github.io/aedseo/reference/aedseo-package.md)
  ([\#41](https://github.com/ssi-dk/aedseo/issues/41)).

- `tsd()` is now deprecated. Please use
  [`to_time_series()`](https://ssi-dk.github.io/aedseo/reference/to_time_series.md)
  instead. A warning is shown when using `tsd()`
  ([\#41](https://github.com/ssi-dk/aedseo/issues/41)).

### Features

- Added the
  [`seasonal_burden_levels()`](https://ssi-dk.github.io/aedseo/reference/seasonal_burden_levels.md)
  function, which calculates burden levels based on data from previous
  seasons with two different methods; “peak_levels” or
  “intensity_levels”
  ([\#37](https://github.com/ssi-dk/aedseo/issues/37)).

- Added the
  [`fit_percentiles()`](https://ssi-dk.github.io/aedseo/reference/fit_percentiles.md)
  function, which optimises a user selected distribution and calculates
  the percentiles based on observations and weights. It is meant to be
  used within the
  [`seasonal_burden_levels()`](https://ssi-dk.github.io/aedseo/reference/seasonal_burden_levels.md)
  function ([\#35](https://github.com/ssi-dk/aedseo/issues/35),
  [\#37](https://github.com/ssi-dk/aedseo/issues/37)) - Renamed
  `fit_quantiles()` to
  [`fit_percentiles()`](https://ssi-dk.github.io/aedseo/reference/fit_percentiles.md)
  ([\#60](https://github.com/ssi-dk/aedseo/issues/60)).

- Added
  [`combined_seasonal_output()`](https://ssi-dk.github.io/aedseo/reference/combined_seasonal_output.md)
  as the main function to run both
  [`seasonal_onset()`](https://ssi-dk.github.io/aedseo/reference/seasonal_onset.md)
  and
  [`seasonal_burden_levels()`](https://ssi-dk.github.io/aedseo/reference/seasonal_burden_levels.md)
  to get a combined result for the newest season
  ([\#44](https://github.com/ssi-dk/aedseo/issues/44)).

- Added
  [`consecutive_growth_warnings()`](https://ssi-dk.github.io/aedseo/reference/consecutive_growth_warnings.md)
  function to help the user with a method to define the disease-specific
  threshold ([\#80](https://github.com/ssi-dk/aedseo/issues/80)).

- Added a new argument `only_current_season` to
  [`seasonal_onset()`](https://ssi-dk.github.io/aedseo/reference/seasonal_onset.md),
  [`seasonal_burden_levels()`](https://ssi-dk.github.io/aedseo/reference/seasonal_burden_levels.md)
  and
  [`combined_seasonal_output()`](https://ssi-dk.github.io/aedseo/reference/combined_seasonal_output.md)
  which gives the possibility to either get output from only the current
  season or for all available seasons
  ([\#45](https://github.com/ssi-dk/aedseo/issues/45)).

- Added
  [`historical_summary()`](https://ssi-dk.github.io/aedseo/reference/historical_summary.md)
  which uses a `tsd_onset` object to summarise historical estimations
  ([\#75](https://github.com/ssi-dk/aedseo/issues/75)).

- [`summary()`](https://rdrr.io/r/base/summary.html) can now summarise
  `tsd_burden_level` objects
  ([\#60](https://github.com/ssi-dk/aedseo/issues/60)).

- [`plot()`](https://ssi-dk.github.io/aedseo/reference/plot.md) and
  [`autoplot()`](https://ssi-dk.github.io/aedseo/reference/autoplot.md)
  can now plot `tsd_combined_seasonal_output` and
  `tsd_consecutive_growth_warning` objects
  ([\#57](https://github.com/ssi-dk/aedseo/issues/57),
  [\#80](https://github.com/ssi-dk/aedseo/issues/80)).

- Added
  [`generate_seasonal_data()`](https://ssi-dk.github.io/aedseo/reference/generate_seasonal_data.md)
  to generate synthetic data for testing and documentation purposes
  ([\#56](https://github.com/ssi-dk/aedseo/issues/56)).

- Added
  [`seasonal_onset()`](https://ssi-dk.github.io/aedseo/reference/seasonal_onset.md)
  as a replacement for the deprecated
  [`aedseo()`](https://ssi-dk.github.io/aedseo/reference/aedseo-package.md)
  function ([\#41](https://github.com/ssi-dk/aedseo/issues/41)).

- Added
  [`to_time_series()`](https://ssi-dk.github.io/aedseo/reference/to_time_series.md)
  as a replacement for the deprecated `tsd()` function
  ([\#41](https://github.com/ssi-dk/aedseo/issues/41)).

### Improvements

- Enhanced clarity and user guidance in the vignettes:

  - [`vignette("generate_seasonal_wave")`](https://ssi-dk.github.io/aedseo/articles/generate_seasonal_wave.md),
  - [`vignette("aedseo")`](https://ssi-dk.github.io/aedseo/articles/aedseo.md),
  - [`vignette("seasonal_onset")`](https://ssi-dk.github.io/aedseo/articles/seasonal_onset.md)
  - [`vignette("burden_levels")`](https://ssi-dk.github.io/aedseo/articles/burden_levels.md)
    providing a comprehensive walkthrough of the application of the
    functions provided by the `aedseo` package with detailed
    explanations and illustrative examples
    ([\#56](https://github.com/ssi-dk/aedseo/issues/56),
    [\#57](https://github.com/ssi-dk/aedseo/issues/57),
    [\#58](https://github.com/ssi-dk/aedseo/issues/58),
    [\#59](https://github.com/ssi-dk/aedseo/issues/59),
    [\#60](https://github.com/ssi-dk/aedseo/issues/60),
    [\#61](https://github.com/ssi-dk/aedseo/issues/61)).

- Improved the
  [`autoplot()`](https://ssi-dk.github.io/aedseo/reference/autoplot.md)
  function which can now visualise dates as days, weeks and months on
  the x-axis with the `time_interval` argument
  ([\#56](https://github.com/ssi-dk/aedseo/issues/56)).

- Improved the
  [`epi_calendar()`](https://ssi-dk.github.io/aedseo/reference/epi_calendar.md)
  function to work for a season spanning new year
  ([\#34](https://github.com/ssi-dk/aedseo/issues/34)).

- Using [`predict()`](https://rdrr.io/r/stats/predict.html) on
  `tsd_onset` objects now uses the same time-scale as the given object
  ([\#61](https://github.com/ssi-dk/aedseo/issues/61)). That is, the
  `time_interval` attribute controls if predictions are by “days”,
  “weeks” or “months”.

- The
  [`aedseo()`](https://ssi-dk.github.io/aedseo/reference/aedseo-package.md)
  function now allows for the choice of adding season as an input
  argument ([\#34](https://github.com/ssi-dk/aedseo/issues/34)).

- [checkmate](https://mllg.github.io/checkmate/) assertions have been
  added to enhance user feedback with clearer error messages and to
  ensure functions operate correctly by validating inputs
  ([\#33](https://github.com/ssi-dk/aedseo/issues/33)).

- Improved the
  [`aedseo()`](https://ssi-dk.github.io/aedseo/reference/aedseo-package.md)
  function to work with `NA` values. The user now defines how many `NA`
  values the function should allow in each window
  ([\#32](https://github.com/ssi-dk/aedseo/issues/32)).

### Minor changes

- Added Sofia Myrup Otero as an author of the R package
  ([\#55](https://github.com/ssi-dk/aedseo/issues/55)).

- Added Rasmus Skytte Randløv as a reviewer of the R package
  ([\#55](https://github.com/ssi-dk/aedseo/issues/55)).

- The `disease_threshold` argument now reflects the disease threshold in
  one time step. If the total number of cases in a window of size `k`
  exceeds `disease_threshold * k`, a seasonal onset alarm can be
  triggered ([\#32](https://github.com/ssi-dk/aedseo/issues/32)).

## aedseo 0.1.2

CRAN release: 2023-11-27

### Minor changes

- Transferring maintainership of the R package to Lasse Engbo
  Christiansen.

## aedseo 0.1.1

CRAN release: 2023-11-16

### Improvements

- Enhanced clarity and user guidance in the introductory vignette,
  providing a more comprehensive walkthrough of the application of the
  ‘aeddo’ algorithm on time series data with detailed explanations and
  illustrative examples.

### Minor changes

- Updated LICENSE.md to have Statens Serum Institut as a copyright
  holder.

- Fixed installation guide for the development version in the README.Rmd
  and README.md

- Added Lasse Engbo Christiansen as an author of the R package.

- Added a new function
  [`epi_calendar()`](https://ssi-dk.github.io/aedseo/reference/epi_calendar.md)
  that determines the epidemiological season based on a given date,
  allowing users to easily categorize dates within or outside specified
  seasons.

- Introduced additional visualizations in the
  [`autoplot()`](https://ssi-dk.github.io/aedseo/reference/autoplot.md)
  method, enhancing the capabilities of the
  [`plot()`](https://ssi-dk.github.io/aedseo/reference/plot.md) method
  with new displays of observed cases and growth rates.

## aedseo 0.1.0

CRAN release: 2023-11-07

### Features

- Added the `aedseo` function, which automates the early detection of
  seasonal epidemic onsets by estimating growth rates for consecutive
  time intervals and calculating the Sum of Cases (sum_of_cases).

- Introduced `autoplot` and `plot` methods for visualizing `aedseo` and
  `aedseo_tsd` objects. These functions allow you to create insightful
  ggplot2 plots for your data.

- Included the `fit_growth_rate` function, enabling users to fit growth
  rate models to time series observations.

- Introduced the `predict` method for `aedseo` objects, which allows you
  to predict observations for future time steps given the growth rates.

- Added the `summary` method for `aedseo` objects, providing a
  comprehensive summary of the results.

- Introduced the `tsd` function, allowing users to create S3
  `aedseo_tsd` (time-series data) objects from observed data and
  corresponding dates.
