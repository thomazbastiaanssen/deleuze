% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scaling.R
\name{g_mean}
\alias{g_mean}
\title{Compute the geometric mean of a vector.}
\usage{
g_mean(x)
}
\arguments{
\item{x}{a numeric vector that does not contain zeroes.}
}
\description{
Simply shorthand for exp(mean(log(x))).
}
