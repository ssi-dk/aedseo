% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/historical_summary.R
\name{historical_summary}
\alias{historical_summary}
\title{Summarises estimates like seasonal peak and onset from all available seasons}
\usage{
historical_summary(onset_output)
}
\arguments{
\item{onset_output}{A \code{tsd_onset} object returned from \code{seasonal_onset()}.}
}
\value{
An object of class \code{historical_summary}, containing:
\itemize{
\item Usual time to seasonal peak (weeks after onset)
\item The week in which the peak usually falls
\item Usual peak intensity
\item The week in which the onset usually falls
\item Usual onset intensity and growth rate estimates
}
}
\description{
This function summarises peak timing and seasonal onset from estimates in a \code{tsd_onset} object.
This can be useful for investigating if the current season falls within estimates from previous seasons
or if it is very distinct from previous seasons.

Uses data from a \code{tsd_onset} object (output from \code{seasonal_onset()}).

\code{seasonal_onset()} has to be run with arguments;
\itemize{
\item disease_threshold
\item season_start
\item season_end
\item only_current_season = FALSE
}
}
\examples{
# Generate simulated data of seasonal waves
sim_data <- generate_seasonal_data(
  years = 5,
  start_date = as.Date("2022-05-26"),
  trend_rate = 1.002,
  noise_overdispersion = 1.1
)

# Estimate seasonal onset
tsd_onset <- seasonal_onset(
  tsd = sim_data,
  disease_threshold = 20,
  family = "quasipoisson",
  season_start = 21,
  season_end = 20,
  only_current_season = FALSE
)

# Get historical summary
historical_summary(tsd_onset)
}
