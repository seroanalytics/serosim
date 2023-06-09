% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{example_model_pars_biphasic}
\alias{example_model_pars_biphasic}
\title{Example model parameters for biphasic waning model}
\format{
A data frame with 5 rows and 6 variables:
\describe{
\item{exposure_id}{name of each exposure type present in example biomarker_map}
\item{biomarker_id}{name of each biomarker present within each exposure type present in example biomarker_map}
\item{name}{names of model parameters}
\item{mean}{numeric values for the true paramter means}
\item{sd}{numeric values for the true paramter standard deviation}
\item{distribution}{distribution type from which the draw_paramaters function will be sampling a parameter }
}
}
\usage{
data(example_model_pars_biphasic)
}
\description{
This example \code{model_pars} is for a simulation three different exposure events (one vaccination, two infection types) corresponding to two biomarkers. \code{model_pars} argument is responsible for storing parameter information needed for the antibody model, observation model and immunity model. In this example, \code{model_pars} has parameters for a biphasic antibody model and for an observational model with noise. This \code{model_pars} input must be converted to the numeric version (\code{reformat_biomarker_map}) before being input into \code{runserosim}.
}
\seealso{
Other example_data: 
\code{\link{example_biomarker_map_numeric}},
\code{\link{example_biomarker_map}},
\code{\link{example_biomarker_states_wide}},
\code{\link{example_biomarker_states}},
\code{\link{example_demography}},
\code{\link{example_exposure_force}},
\code{\link{example_exposure_probabilities}},
\code{\link{example_foe_pars}},
\code{\link{example_immune_histories_wide}},
\code{\link{example_immune_histories}},
\code{\link{example_model_pars_numeric}},
\code{\link{example_model_pars_typhoid}},
\code{\link{example_model_pars}},
\code{\link{example_observed_biomarker_states}}
}
\concept{example_data}
\keyword{datasets}
