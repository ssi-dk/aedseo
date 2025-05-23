% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/epi_calendar.R
\name{epi_calendar}
\alias{epi_calendar}
\title{Determine Epidemiological Season}
\usage{
epi_calendar(date, start = 21, end = 20)
}
\arguments{
\item{date}{A date object representing the date to check.}

\item{start}{An integer specifying the start week of the epidemiological season.}

\item{end}{An integer specifying the end week of the epidemiological season.}
}
\value{
A character vector indicating the season:
\itemize{
\item "out_of_season" if the date is outside the specified season,
\item If within the season, the function returns a character string indicating
the epidemiological season.
}
}
\description{
This function identifies the epidemiological season, (must span new year) to which a given date belongs.
The epidemiological season is defined by a start and end week, where weeks are numbered according to the
ISO week date system.
}
\examples{
# Check if a date is within the epidemiological season
epi_calendar(as.Date("2023-09-15"), start = 21, end = 20)
# Expected output: "2023/2024"

epi_calendar(as.Date("2023-05-30"), start = 40, end = 20)
# Expected output: "out_of_season"

try(epi_calendar(as.Date("2023-01-15"), start = 1, end = 40))
# Expected error: "`start` must be greater than `end`!"

epi_calendar(as.Date("2023-10-06"), start = 40, end = 11)
# Expected output: "2023/2024"
}
