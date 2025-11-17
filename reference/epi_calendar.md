# Determine Epidemiological Season

This function identifies the epidemiological season, (must span new
year) to which a given date belongs. The epidemiological season is
defined by a start and end week, where weeks are numbered according to
the ISO week date system.

## Usage

``` r
epi_calendar(date, start = 21, end = 20)
```

## Arguments

- date:

  A date object representing the date to check.

- start:

  An integer specifying the start week of the epidemiological season.

- end:

  An integer specifying the end week of the epidemiological season.

## Value

A character vector indicating the season:

- "out_of_season" if the date is outside the specified season,

- If within the season, the function returns a character string
  indicating the epidemiological season.

## Examples

``` r
# Check if a date is within the epidemiological season
epi_calendar(as.Date("2023-09-15"), start = 21, end = 20)
#> [1] "2023/2024"
# Expected output: "2023/2024"

epi_calendar(as.Date("2023-05-30"), start = 40, end = 20)
#> [1] "out_of_season"
# Expected output: "out_of_season"

try(epi_calendar(as.Date("2023-01-15"), start = 1, end = 40))
#> Error in pkgdown::build_site_github_pages(new_process = FALSE, install = FALSE) : 
#>   1 assertions failed:
#>  * `start` must be greater than `end`!
# Expected error: "`start` must be greater than `end`!"

epi_calendar(as.Date("2023-10-06"), start = 40, end = 11)
#> [1] "2023/2024"
# Expected output: "2023/2024"
```
