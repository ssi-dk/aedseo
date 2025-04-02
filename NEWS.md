# aedseo (development version)

## Deprecations

* `aedseo()` is now deprecated. Please use `seasonal_onset()` instead. A warning is shown when using `aedseo()` (#41).

* `tsd()` is now deprecated. Please use `to_time_series()` instead. A warning is shown when using `tsd()` (#41).

## Features

* Added the `seasonal_burden_levels()` function, which calculates burden levels based on data from previous seasons with two different methods; "peak_levels" or "intensity_levels" (#37).

* Added the `fit_percentiles()` function, which optimises a user selected distribution and calculates the percentiles based on observations and weights. It is meant to be used within the `seasonal_burden_levels()` function (#35, #37) - Renamed `fit_quantiles()` to `fit_percentiles()` (#60).

* Added `combined_seasonal_output()` as the main function to run both `seasonal_onset()` and `seasonal_burden_levels()` to get a combined result for the newest season (#44).

* Added a new argument `only_current_season` to `seasonal_onset()`, `seasonal_burden_levels()` and `combined_seasonal_output()` which gives the possibility to either get output from only the current season or for all available seasons (#45).

* Added `historical_summary` which uses a `tsd_onset` object to summarise historical estimations (#75).

* `summary()` can now summarise `tsd_burden_level` objects (#60).

* `plot()` and `autoplot()` can now plot `tsd_combined_seasonal_output` objects (#57).

* Added `generate_seasonal_data()` to generate synthetic data for testing and documentation purposes (#56).

* Added `seasonal_onset()` as a replacement for the deprecated `aedseo()` function (#41).

* Added `to_time_series()` as a replacement for the deprecated `tsd()` function (#41).

## Improvements

* Enhanced clarity and user guidance in the vignettes:
  - `vignette("generate_seasonal_wave")`,
  - `vignette("aedseo")`,
  - `vignette("seasonal_onset")`
  - `vignette("burden_levels")`
  providing a comprehensive walkthrough of the application of the functions provided by the `aedseo` package with detailed explanations and illustrative examples (#56, #57, #58, #59, #60, #61).

* Improved the `autoplot()` function which can now visualise dates as days, weeks and months on the x-axis with the `time_interval` argument (#56).

* Improved the `epi_calendar()` function to work for a season spanning new year (#34).

* Using `predict()` on `tsd_onset` objects now uses the same time-scale as the given object (#61).
That is, the `time_interval` attribute controls if predictions are by "days", "weeks" or "months".

* The `aedseo()` function now allows for the choice of adding season as an input argument (#34).

* `{checkmate}` assertions have been added to enhance user feedback with clearer error messages and to ensure functions operate correctly by validating inputs (#33).

* Improved the `aedseo()` function to work with `NA` values. The user now defines how many `NA` values the function should allow in each window (#32).

## Minor changes

* Added Sofia Myrup Otero as an author of the R package (#55).

* Added Rasmus Skytte Randløv as a reviewer of the R package (#55).

* The `disease_threshold` argument now reflects the disease threshold in one time step. If the total number of cases in a window of size `k` exceeds  `disease_threshold * k`, a seasonal onset alarm can be triggered (#32).

# aedseo 0.1.2

## Minor changes

* Transferring maintainership of the R package to Lasse Engbo Christiansen.

# aedseo 0.1.1

## Improvements

* Enhanced clarity and user guidance in the introductory vignette, providing a more comprehensive walkthrough of the application of the 'aeddo' algorithm on time series data with detailed explanations and illustrative examples.

## Minor changes

* Updated LICENSE.md to have Statens Serum Institut as a copyright holder.

* Fixed installation guide for the development version in the README.Rmd and README.md

* Added Lasse Engbo Christiansen as an author of the R package.

* Added a new function `epi_calendar()` that determines the epidemiological season based on a given date, allowing users to easily categorize dates within or outside specified seasons.

* Introduced additional visualizations in the `autoplot()` method, enhancing the capabilities of the `plot()` method with new displays of observed cases and growth rates.

# aedseo 0.1.0

## Features

- Added the `aedseo` function, which automates the early detection of seasonal epidemic onsets by estimating growth rates for consecutive time intervals and calculating the Sum of Cases (sum_of_cases).

- Introduced `autoplot` and `plot` methods for visualizing `aedseo` and `aedseo_tsd` objects. These functions allow you to create insightful ggplot2 plots for your data.

- Included the `fit_growth_rate` function, enabling users to fit growth rate models to time series observations.

- Introduced the `predict` method for `aedseo` objects, which allows you to predict observations for future time steps given
the growth rates.

- Added the `summary` method for `aedseo` objects, providing a comprehensive summary of the results.

- Introduced the `tsd` function, allowing users to create S3 `aedseo_tsd` (time-series data) objects from observed data and corresponding dates.
