% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{updatePredictions}
\alias{updatePredictions}
\title{Update predictions}
\usage{
updatePredictions(
  data_dir = getOption("HockeyModel.prediction.path"),
  scores = HockeyModel::scores,
  schedule = HockeyModel::schedule,
  params = NULL
)
}
\arguments{
\item{data_dir}{directory of predictions}

\item{scores}{HockeyModel::scores or a custom value}

\item{schedule}{HockeyModel::schedule or a custom value}

\item{params}{The named list containing m, rho, beta, eta, and k. See \link{updateDC} for information on the params list}
}
\description{
Update predictions
}
