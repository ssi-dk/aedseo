# Create a tibble-like `tsd` (time-series data) object from time series data and corresponding dates.

This function takes `cases` and the corresponding date vector (`time`)
and converts it into a `tsd` object, which is a time series data
structure that can be used for time series analysis. If incidence is
added, it will be used as observation in all future use of the `aedseo`
package on the defined `tsd` object.

Options:

- `incidence` can be calculated if also supplying `cases`, `population`,
  and `incidence_denominator`.

- `cases` can be calculated if also supplying `incidence`, `population`
  and `incidence_denominator`.

- If background population changes during the time series, it is used to
  adjust the growth rate in
  [`seasonal_onset()`](https://ssi-dk.github.io/aedseo/reference/seasonal_onset.md).

## Usage

``` r
to_time_series(
  cases = NULL,
  incidence = NULL,
  population = NULL,
  incidence_denominator = if (is.null(population)) NA_real_ else 1e+05,
  time,
  time_interval = c("weeks", "days", "months")
)
```

## Arguments

- cases:

  An integer vector containing the time series cases.

- incidence:

  A numeric vector containing the time series incidences. With the given
  incidence_denominator.

- population:

  An integer vector containing the time series background population.

- incidence_denominator:

  An integer \>= 1, specifying the observations per
  incidence-denominator.

- time:

  A date vector containing the corresponding dates.

- time_interval:

  A character vector specifying the time interval. Choose between
  'days', 'weeks', or 'months'.

## Value

A `tsd` object containing:

- 'time': The time point for the corresponding data.

- 'cases': The number of cases at the time point.

- 'incidence': The incidence per `incidence_denominator` at the time
  point. (optional)

- 'population': The background population for the cases at the time
  point. (optional)

## Examples

``` r
# Create a `tsd` object with only cases
tsd_cases <- to_time_series(
  cases = c(10, 15, 20, 18),
  time = seq(from = as.Date("2023-01-01"), by = "1 week", length.out = 4)
)

# Create a `tsd` object with incidence from cases, population and default incidence_denominator
tsd_calculate_incidence <- to_time_series(
  cases = c(100, 120, 130, 150),
  time = seq(from = as.Date("2023-01-01"), by = "1 week", length.out = 4),
  population = c(3000000, 3000000, 3000000, 3000000)
)

# Create a `tsd` object with cases from incidence, population and default incidence_denominator
tsd_calculate_cases <- to_time_series(
  incidence = c(5, 7.8, 8, 8.5),
  time = seq(from = as.Date("2023-01-01"), by = "1 week", length.out = 4),
  population = c(3000000, 3000000, 3000000, 3000000)
)
```
