# Create a tibble-like `tsd` (time-series data) object from time series data and corresponding dates.

This function takes observations and the corresponding date vector
(`time`) and converts them into a `tsd` object, which is a time series
data structure that can be used for time series analysis. For count
data, supply `cases` or `incidence` with given `population`. For
binomial data, supply `samples` and `cases` or `proportion`. Count and
binomial inputs are mutually exclusive.

Options:

- `incidence` can be calculated if also supplying `cases`, `population`,
  and `incidence_denominator`.

- `cases` can be calculated if also supplying `incidence`, `population`
  and `incidence_denominator`.

- If background population changes during the time series, it is used to
  adjust the growth rate in
  [`seasonal_onset()`](https://ssi-dk.github.io/aedseo/reference/seasonal_onset.md).

- `proportion` will be calculated if supplying `cases` and `samples`.

- `cases` will be calculated if supplying `proportion` and `samples`.

## Usage

``` r
to_time_series(
  cases = NULL,
  incidence = NULL,
  population = NULL,
  proportion = NULL,
  samples = NULL,
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
  For binomial data, use `samples` instead.

- proportion:

  A numeric vector containing binomial proportions in `[0, 1]` (Will be
  rescaled to `[0, 1]` if percentages in `(1, 100]` are provided). Use
  with `samples` for proportional/binomial data.

- samples:

  An integer vector containing number of samples tested. Use with
  `cases` or `proportion` for binomial data.

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

- 'proportion': The proportion of cases in the tested samples at the
  time point for binomial input. (optional)

- 'samples': The number of samples tested at the time point for binomial
  input. (optional)

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

# Create a `tsd` object with binomial data
tsd_binomial <- to_time_series(
  cases = c(10, 12, 18, 25),
  samples = c(100, 100, 120, 140),
  time = seq(from = as.Date("2023-01-01"), by = "1 week", length.out = 4)
)
```
