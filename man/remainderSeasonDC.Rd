% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dixon-coles.R
\name{remainderSeasonDC}
\alias{remainderSeasonDC}
\title{DC remainder of season}
\usage{
remainderSeasonDC(
  nsims = 10000,
  cores = NULL,
  params = NULL,
  scores = HockeyModel::scores,
  schedule = HockeyModel::schedule,
  odds = FALSE,
  regress = TRUE,
  mu_lambda = FALSE
)
}
\arguments{
\item{nsims}{Number of simulations}

\item{cores}{The number of cores to use if using parallel processing, or 1 for single-core, NULL defaults to all cores or 1 if \code{parallel} package not installed.}

\item{params}{The named list containing m, rho, beta, eta, and k. See \link{updateDC} for information on the params list}

\item{scores}{the historical scores}

\item{schedule}{un-played future games}

\item{odds}{whether to return odds table or simulate season}

\item{regress}{whether to apply a regression to the mean for team strength on future predictions}

\item{mu_lambda}{whether to return team xG values. Can't be set true if odds is true}
}
\value{
data frame of Team, playoff odds.
}
\description{
Odds for each team to get to playoffs.
}
