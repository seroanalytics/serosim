% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/immunity_models.R
\name{immunity_model_all_successful}
\alias{immunity_model_all_successful}
\title{Null immunity model}
\usage{
immunity_model_all_successful(
  i,
  t,
  x,
  immune_histories,
  biomarker_states,
  demography,
  biomarker_map,
  model_pars,
  ...
)
}
\arguments{
\item{i}{integer for the individual ID}

\item{t}{integer for the time period}

\item{x}{integer for the exposure ID}

\item{immune_histories}{a 3D array of immune histories for all individuals, time steps and exposure IDs}

\item{biomarker_states}{an 3D array of biomarker states (biomarker quantities) for all individuals, time steps and biomarker IDs}

\item{demography}{a tibble of demographic information for each individual in the simulation}

\item{biomarker_map}{a table specifying the relationship between exposure IDs and biomarker IDs}

\item{model_pars}{a tibble of parameters needed for the immunity model}

\item{...}{Additional arguments}
}
\value{
The probability of successful exposure
}
\description{
All exposure trials are successful (probability of success is 1).
}
\examples{
immunity_model_all_successful(1,1,1,NULL,NULL,NULL,NULL,NULL)
}
