% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc_functions.R
\name{precomputation_checks}
\alias{precomputation_checks}
\title{Pre-compute exposure probabilities}
\usage{
precomputation_checks(
  N,
  times,
  exposure_ids,
  groups,
  exposure_model,
  foe_pars,
  demography,
  VERBOSE,
  check_correct = FALSE,
  ...
)
}
\arguments{
\item{N}{integer number of individuals in the simulation}

\item{times}{vector of time periods in the simulation}

\item{exposure_ids}{integer vector of unique exposure IDs matching the \code{biomarker_map}}

\item{groups}{integer vector of unique groups matching entries in \code{demography}}

\item{exposure_model}{function pointer to the desired exposure model}

\item{foe_pars}{object (usually an array, but may be a list or other class) corresponding to the \code{exposure_model} inputs}

\item{demography}{tibble of demography variables used for the simulation}

\item{VERBOSE}{(optional) if specified, an integer giving the frequency at which updates should be printed, as well as dictating whether progress messages are printed. Defaults to \code{NULL}}

\item{check_correct}{if TRUE, computes the entire exposure probability array as would be done in \code{runserosim} with no pre-computation. This is usually quite slow, but can be used to check that the pre-computed exposure probability array is correct.}

\item{...}{other inputs to the \code{exposure_model}}
}
\value{
a list containing: 1) a boolean set to \code{TRUE} if precomputation was successful; 2) a 3D array matching the dimensions of \code{N}, \code{times} and \code{exposure_ids} giving the individual probability of exposure in each time period
}
\description{
Checks if the provided exposure model can be solved in advance of the main simulation in fewer function calls than would be expected in the main simulation. Note that there is some overhead to checking for pre-computation, so this may not be quicker and may actually slow down the simulation. However, in some cases where many individuals share identical exposure probabilities, running this function can lead to massive speed ups.
}
\examples{
times <- seq(1,100,by=1)
N <- 100
n_exposure_ids <- 2
n_groups <- 2
demography <- generate_pop_demography(N=N, times=times,prob_removal=0,
aux=list(Group=list(name="group",options=c(1,2),proportion=c(0.5,0.5))))
foe_pars <- array(runif(n_exposure_ids*length(times)*n_groups),dim=c(n_groups,
length(times),n_exposure_ids))
res <- precomputation_checks(N, times=times, exposure_ids=1:2,groups=1:2,
exposure_model_simple_FOE,foe_pars=foe_pars, demography=demography,VERBOSE=10,check_correct=TRUE)
}
