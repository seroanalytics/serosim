% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc_functions.R
\name{normal_to_lognormal_mean}
\alias{normal_to_lognormal_mean}
\title{Convert normal distributions to log-normal distributions and return mean}
\usage{
normal_to_lognormal_mean(normmean, normsd)
}
\arguments{
\item{normmean}{Mean of the normal distribution}

\item{normsd}{Standard deviation of the normal distribution}
}
\value{
Mean of the log-normal distribution
}
\description{
Convert normal distributions to log-normal distributions and return mean
}
\examples{
normal_to_lognormal_mean(50,10)
}
