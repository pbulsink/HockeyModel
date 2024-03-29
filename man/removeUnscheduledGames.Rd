% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api-interface.R
\name{removeUnscheduledGames}
\alias{removeUnscheduledGames}
\title{Remove Unscheduled Games}
\usage{
removeUnscheduledGames(schedule = HockeyModel::schedule, save_data = FALSE)
}
\arguments{
\item{schedule}{the schedule to check for unscheduled games}

\item{save_data}{whether to save the cleaned schedule to package or not. Default False}
}
\value{
a schedule with unscheduled games removed
}
\description{
Sometimes games are scheduled then not played (e.g. unneeded games in playoff series, etc.)
}
