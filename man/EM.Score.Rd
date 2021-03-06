% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Estimation.R
\name{EM.Score}
\alias{EM.Score}
\title{EM Score Equation for Tau2}
\usage{
EM.Score(theta, se, tau2, gamma)
}
\arguments{
\item{theta}{Parameter estimates.}

\item{se}{Parameter standard errors.}

\item{tau2}{Variance component.}

\item{gamma}{Responsibilities.}
}
\value{
Numeric EM objective.
}
\description{
EM Score Equation for Tau2
}
