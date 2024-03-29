% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api-interface.R
\name{games_today}
\alias{games_today}
\title{Games Today}
\usage{
games_today(
  schedule = HockeyModel::schedule,
  date = Sys.Date(),
  all_games = FALSE
)
}
\arguments{
\item{schedule}{the schedule within which to look for games}

\item{date}{the date to look for games, as a date}

\item{all_games}{whether to return all games scheduled for a date (True) or exclude postponed, rescheduled, in-progress, or completed games (False, default)}
}
\value{
Scheduled games (in the format of the schedule) for the requested date, or NULL if none
}
\description{
given a schedule, it returns todays (or another date's) scheduled games (excluding postponements), or NULL if there are none
}
