% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{auc}
\alias{auc}
\title{AUC}
\usage{
auc(predicted, actual)
}
\arguments{
\item{predicted}{Predicted odds of an event occuring. needen't be of set {0,1}}

\item{actual}{If the event occured (0 or 1), or model results in 0, 0.25, 0.4, 0.6, 0.75, 1.0}
}
\value{
a single value for auc
}
\description{
calculate the AUC metrics. From MLMetrics
}
