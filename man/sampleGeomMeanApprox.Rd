% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/resample.R
\name{sampleGeomMeanApprox}
\alias{sampleGeomMeanApprox}
\title{samples from the approximated probability density function of the log-transformed geometric mean of a count sample.}
\usage{
sampleGeomMeanApprox(samples, count_sample, log_transformed = T)
}
\arguments{
\item{samples}{An integer. How many samples should be taken}

\item{count_sample}{A vector of count data.}

\item{log_transformed}{A boolean, whether to return log-transformed values.}
}
\description{
samples from the approximated probability density function of the log-transformed geometric mean of a count sample.
}
