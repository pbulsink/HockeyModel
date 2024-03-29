% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dixon-coles.R
\name{getM}
\alias{getM}
\title{Fast Dixon-Coles model fitting 'm'.}
\usage{
getM(scores = HockeyModel::scores, currentDate = Sys.Date(), xi = 0.00426)
}
\arguments{
\item{scores}{the historical scores to evaluate}

\item{currentDate}{(for date weight adjustment)}

\item{xi}{aggressiveness of date weighting}
}
\value{
a model 'm' of Dixon-Coles' type parameters.
}
\description{
Produces a DC model
}
