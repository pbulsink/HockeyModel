% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graphics.R
\name{plot_playoff_series_odds}
\alias{plot_playoff_series_odds}
\title{Plot Today's Playoff Series Odds}
\usage{
plot_playoff_series_odds(
  series = getAPISeries(),
  params = NULL,
  teamColours = HockeyModel::teamColours
)
}
\arguments{
\item{series}{A data frame of home team, away team, home wins, away wins}

\item{params}{The named list containing m, rho, beta, eta, and k. See \link{updateDC} for information on the params list}

\item{teamColours}{HockeyModel::teamColours or a custom value}
}
\value{
a ggplot image of odds
}
\description{
Plot Today's Playoff Series Odds
}
