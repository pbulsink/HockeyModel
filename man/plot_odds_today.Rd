% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graphics.R
\name{plot_odds_today}
\alias{plot_odds_today}
\title{Plot Today's Odds}
\usage{
plot_odds_today(
  today = Sys.Date(),
  params = NULL,
  schedule = HockeyModel::schedule,
  teamColours = HockeyModel::teamColours
)
}
\arguments{
\item{today}{The day's odds to plot. Default today.}

\item{params}{The named list containing m, rho, beta, eta, and k. See \link{updateDC} for information on the params list}

\item{schedule}{HockeyModel::schedule or a custom value}

\item{teamColours}{HockeyModel::teamColours or a custom value}
}
\value{
a ggplot image of odds
}
\description{
Plot Today's Odds
}
