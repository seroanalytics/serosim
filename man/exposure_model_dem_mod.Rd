% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exposure_models.R
\name{exposure_model_dem_mod}
\alias{exposure_model_dem_mod}
\title{Exposure Model Modified By Relevant Demographic Elements}
\usage{
exposure_model_dem_mod(
  i,
  t,
  x,
  g,
  foe_pars,
  demography,
  dem_mod,
  t_in_year = 1,
  ...
)
}
\arguments{
\item{i}{Individual}

\item{t}{time}

\item{x}{exposure}

\item{g}{group}

\item{foe_pars}{A 3D array providing the force of exposure for each exposure ID, group and time.}

\item{demography}{A tibble of relevant demographic information for each individual in the simulation.}

\item{dem_mod}{A tibble specifying the modifier (how much each input affects probability of exposure) for each demographic elements; column names are column, value, modifier. Entries in column and value must match format in demography table. All column and value combinations in demography must have a modifier value within this tibble. Users can also add age modifier (how much each age affects probability of exposure). The column name will be "age" with the entry being individual's ages.}

\item{t_in_year}{The number of time steps in a year; defaults to 1}

\item{...}{Additional arguments}
}
\value{
A probability of exposure is returned
}
\description{
Probability of exposure depends on the force of exposure at the current time t for group g modulated by relevant demographic elements specified within the dem_mod. Within dem_mod, users can select which demographic elements affect the probability of exposure and by how much.
}
\examples{
times <- seq(1,365,by=1)
## Create fixed FOI (force of infection) for two exposure types in one group
n_groups <- 1
n_exposures <- 2
n_times <- length(times)
n_indiv <- 2
foe_pars <- array(NA, dim=c(n_groups,length(times),n_exposures))
foe_pars[1,,1] <- 0.01
foe_pars[1,,2] <- 0.005

## Create demography modifiers
## Example with two individuals, one in low SES -and one in high SES
demography <- dplyr::tibble(i = rep(1:n_indiv, each=n_times), times=rep(times,2),
SES=rep(c("low","high"),each=n_times))

## Create example where for exposure ID 1, high SES gives 25\% reduction in FOE.
## high SES gives 50\% reduction in FOE for exposure ID 2
dem_mod <- dplyr::tibble(exposure_id=c(1,1,2,2),column=c("SES","SES","SES","SES"),
                 value=c("low","high","low","high"),modifier=c(1,0.75,1,0.5))

## Solve the model for each time point for each group.
## but is kept for compatibility with other functions.
foe <- array(NA, dim=c(n_indiv, n_times, n_exposures))
for(i in 1:n_indiv){
    for(x in 1:n_exposures){
       foe[i,,x] <- unlist(sapply(times, function(t) exposure_model_dem_mod(i, 
       t, x, 1, foe_pars, demography, dem_mod)))
    }
}
}
