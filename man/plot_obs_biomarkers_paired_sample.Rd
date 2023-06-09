% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_plots.R
\name{plot_obs_biomarkers_paired_sample}
\alias{plot_obs_biomarkers_paired_sample}
\title{Plot biomarker quantities for multiple observation times and paired samples}
\usage{
plot_obs_biomarkers_paired_sample(
  observed_biomarker_states,
  discretize_times = TRUE
)
}
\arguments{
\item{observed_biomarker_states}{The reshaped data set containing observed biomarker quantities for individuals at all time steps for each biomarker}

\item{discretize_times}{if TRUE, plot sampling times as discrete factors. Otherwise, plots sampling time on the x-axis as its raw value.}
}
\value{
A plot of observed biomarker quantities for all individuals and biomarkers is returned
}
\description{
This function should be used when there were multiple time step in which biomarker quantities were observed
}
\examples{
library(dplyr)
example_biomarker_states$observed <- example_biomarker_states$value
example_biomarker_states_subset <- example_biomarker_states \%>\% dplyr::filter(t \%in\% c(1,120))
plot_obs_biomarkers_paired_sample(example_biomarker_states_subset)
}
