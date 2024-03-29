% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dixon-coles.R
\name{todayDC}
\alias{todayDC}
\title{DC Predictions Today}
\usage{
todayDC(
  params = NULL,
  today = Sys.Date(),
  schedule = HockeyModel::schedule,
  expected_mean = NULL,
  season_percent = NULL,
  include_xG = FALSE,
  draws = TRUE
)
}
\arguments{
\item{params}{The named list containing m, rho, beta, eta, and k. See \link{updateDC} for information on the params list}

\item{today}{Generate predictions for this date. Defaults to today}

\item{schedule}{schedule to use, if not the built-in}

\item{expected_mean}{the mean lambda & mu, used only for regression}

\item{season_percent}{the percent complete of the season, used for regression}

\item{include_xG}{Whether to include team expected goals. default FALSE}

\item{draws}{whether to report draws in odds (AwayWin/HomeWin/Draw) or not (AwayWin/HomeWin). Default True}
}
\value{
a data frame of HomeTeam, AwayTeam, HomeWin, AwayWin, Draw, GameID; or NULL if no games today
}
\description{
DC Predictions Today
}
