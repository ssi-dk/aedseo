% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_percentiles.R
\name{fit_percentiles}
\alias{fit_percentiles}
\title{Fits weighted observations to distribution and returns percentiles}
\usage{
fit_percentiles(
  weighted_observations,
  conf_levels = c(0.5, 0.9, 0.95),
  family = c("lnorm", "weibull", "exp"),
  optim_method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent"),
  lower_optim = -Inf,
  upper_optim = Inf
)
}
\arguments{
\item{weighted_observations}{A tibble containing two columns of length n; \code{observation}, which contains the data
points, and \code{weight}, which is the importance assigned to the observation. Higher weights indicate that an
observation has more influence on the model outcome, while lower weights reduce its impact.}

\item{conf_levels}{A numeric vector specifying the confidence levels for parameter estimates. The values have
to be unique and in ascending order, that is the lowest level is first and highest level is last.}

\item{family}{A character string specifying the family for modeling. Choose between 'poisson', or 'quasipoisson'.
Must be one of: character, family-generator, or family object.}

\item{optim_method}{A character string specifying the method to be used in the optimisation. Lookup \code{?optim::stats}
for details about methods.
If using the exp family it is recommended to use Brent as it is a one-dimensional optimisation.}

\item{lower_optim}{A numeric value for the optimisation.}

\item{upper_optim}{A numeric value for the optimisation.}
}
\value{
A list containing:
\itemize{
\item 'conf_levels': The conf_levels chosen to fit the percentiles.
\item 'percentiles': The percentile results from the fit.
\item 'par': The fit parameters for the chosen family.
\itemize{
\item par_1:
\itemize{
\item For 'weibull': Shape parameter (k).
\item For 'lnorm': Mean of the log-transformed observations.
\item For 'exp': Rate parameter (rate).
}
\item 'par_2':
\itemize{
\item For 'weibull': Scale parameter (scale).
\item For 'lnorm': Standard deviation of the log-transformed observations.
\item For 'exp': Not applicable (set to NA).
}
}
\item 'obj_value': The value of the objective function - (negative log-likelihood), which represent the minimized
objective function value from the optimisation. Smaller value equals better optimisation.
\item 'converged': Logical. TRUE if the optimisation converged.
\item 'family': The distribution family used for the optimization.
\itemize{
\item 'weibull': Uses the Weibull distribution for fitting.
\item 'lnorm': Uses the Log-normal distribution for fitting.
\item 'exp': Uses the Exponential distribution for fitting.
}
}
}
\description{
This function estimates the percentiles of weighted time series observations. The output contains the percentiles
from the fitted distribution.
}
\examples{
# Create three seasons with random observations
obs <- 10
season <- c("2018/2019", "2019/2020", "2020/2021")
season_num_rev <- rev(seq(from = 1, to = length(season)))
observations <- rep(stats::rnorm(10, obs), length(season))

# Add into a tibble with decreasing weight for older seasons
data_input <- tibble::tibble(
  observation = observations,
  weight = 0.8^rep(season_num_rev, each = obs)
)

# Use the model
fit_percentiles(
  weighted_observations = data_input,
  conf_levels = c(0.50, 0.90, 0.95),
  family= "weibull"
)
}
