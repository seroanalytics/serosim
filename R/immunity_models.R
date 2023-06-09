#' Null immunity model
#'
#' @description All exposure trials are successful (probability of success is 1).
#'
#' @param i integer for the individual ID
#' @param t integer for the time period
#' @param x integer for the exposure ID
#' @param immune_histories a 3D array of immune histories for all individuals, time steps and exposure IDs
#' @param biomarker_states an 3D array of biomarker states (biomarker quantities) for all individuals, time steps and biomarker IDs
#' @param demography a tibble of demographic information for each individual in the simulation
#' @param biomarker_map a table specifying the relationship between exposure IDs and biomarker IDs
#' @param model_pars a tibble of parameters needed for the immunity model
#' @param ... Additional arguments
#'
#' @return The probability of successful exposure
#' @export
#'
#' @examples
#' immunity_model_all_successful(1,1,1,NULL,NULL,NULL,NULL,NULL)
immunity_model_all_successful <- function(i, t, x, immune_histories, 
                           biomarker_states, demography, biomarker_map, model_pars, ...){
  return(1)
}

#' Vaccine-mediated immunity model
#' 
#' @description This immunity model should only be used if all exposures are vaccination events. The probability of successful exposure (vaccination event) depends on the number of vaccines an individual has received prior to time t. If the individual is under the maximum vaccinations allotted then the probability of successful exposure event is 1.
#'
#' @inheritParams immunity_model_all_successful
#' @param max_vacc_events a vector of the maximum number of vaccination events possible for each exposure type; If an exposure type is not a vaccination event then input `NA`
#' @param vacc_age a vector of the minimum age at which an individual is eligible for vaccination for each exposure event; If an exposure event is not a vaccination event then input `NA`
#' @param ... Additional arguments
#'
#' @return The probability of successful exposure
#' @export
#'
#' @examples
#' tmp_immune_history <- array(0, dim=c(1, 10, 1))
#' tmp_immune_history[1,1,1] <- 1
#' tmp_demography <- dplyr::tibble(i=1, birth=1)
#' immunity_model_vacc_only(1,8,1,tmp_immune_history,NULL, tmp_demography,NULL, 
#' NULL,max_vacc_events=2,vacc_age=5)
immunity_model_vacc_only <- function(i, t, x, immune_histories, 
                              biomarker_states, demography, biomarker_map, 
                              model_pars, max_vacc_events, vacc_age,...){
  ## Calculate the individual's current age
  birth_time<-unique(demography$birth[demography$i==i])
  curr_age<- t-birth_time
  
  ## If the individual is above the minimum age of vaccination 
  if(curr_age>=vacc_age[x]){
  ## Count the total number of successful exposures to e thus far 
    curr_vacc_events<-sum(immune_histories[i,1:t-1,x], na.rm=TRUE)
    ## If number of successful exposures is less than the max number of vaccination events then vaccine exposure is successful 
    if(curr_vacc_events<max_vacc_events[x]){
      return(1)
    }else{
      return(0)
    }
  }else{
    return(0)
  }
}
 

#' Simple immunity model for vaccination and infection events
#' 
#' @description This immunity model should only be used with vaccines and infection. The probability of successful exposure for vaccination events depends on the number of vaccines an individual has received prior to time `t` and their current age. If the individual is under the maximum vaccinations allotted and is of an age eligible for vaccination then the probability of a successful exposure event is 1. The probability of a successful infection event is dependent on the total number of infections that individual has experienced thus far. If the individual is under the maximum number of infections allotted then the probability of a successful exposure event is 1. 
#'
#' @inheritParams immunity_model_all_successful
#' @param max_events a vector of the maximum number of successful exposure events possible for each exposure ID
#' @param vacc_exposures a vector of exposure IDs (x)
#' @param vacc_age a vector of the minimum age at which an individual is eligible for vaccination for each exposure event; If an exposure event is not a vaccination event then input `NA`
#' @param ... Additional arguments
#'
#' @return  A probability of successful exposure is returned
#' @export
#'
#' @examples
#' tmp_immune_history <- array(0, dim=c(1, 10, 2))
#' ## Toy example: individual has 3 early exposures with exposure ID 1
#' tmp_immune_history[1,1:3,1] <- 1
#' tmp_demography <- dplyr::tibble(i=1, birth=1)
#' ## Probability of successful exposure ID 1 at time 8 is 0, as all 3 allowable 
#' ## exposure events have occurred
#' immunity_model_vacc_ifxn_simple(1,8,1,tmp_immune_history,NULL, tmp_demography,
#' NULL, NULL,max_events=c(3,5),vacc_exposures=c(1,2),vacc_age=c(1,1))
#' ## Probability of successful exposure ID 2 at time 8 is 1, as <5 of the 
#' ## allowable exposure events have occurred
#' immunity_model_vacc_ifxn_simple(1,8,2,tmp_immune_history,NULL, tmp_demography,
#' NULL, NULL,max_events=c(3,5),vacc_exposures=c(1,2),vacc_age=c(1,1))
immunity_model_vacc_ifxn_simple <- function(i, t, x, immune_histories, 
                                     biomarker_states, demography, biomarker_map, model_pars, 
                                     max_events=rep(Inf, dim(immune_histories)[3]), vacc_exposures=rep(Inf, dim(immune_histories)[3]), 
                                     vacc_age, ...){
  ## If an exposure event is a vaccination event, then guaranteed exposure unless the individual has already been vaccinated
  if(x %in% c(vacc_exposures)){  	
    ## Calculate the individual's current age
    birth_time<-unique(demography$birth[demography$i==i])
    curr_age<- t-birth_time
    
    ## If the individual is above the minimum age of vaccination 
    if(curr_age>=vacc_age[x]){
         ## Count the total number of successful exposures to e thus far 
          curr_vacc_events<-sum(immune_histories[i,1:t-1,x], na.rm=TRUE)
        ## If number of successful exposures is less than the max number of vaccination events then vaccine exposure is successful 
        if(curr_vacc_events<max_events[x]){
           return(1)
             }else{
             return(0)
              }
    }
    if(curr_age<vacc_age[x]){
      return(0)
    }
  }
  else{ ## If the exposure event is an infection event
    ## Count the total number of successful exposures to x thus far 
    curr_ifx_events<-sum(immune_histories[i,1:t-1,x], na.rm=TRUE)
    if(curr_ifx_events<max_events[x]){
      return(1)
    }else{
      return(0)
  }
  }
}



#' Immunity model for infection with biomarker-quantity-mediated protection
#' 
#' @description This immunity model should only be used for infection events. The probability of a successful exposure event is dependent on the individual’s biomarker quantity at the time of exposure. User specified `biomarker_prot_midpoint` and `biomarker_prot_width` within `model_pars` is used to calculate biomarker-mediated protection.   
#'    
#' @inheritParams immunity_model_all_successful
#' @param max_events a vector of the maximum number of successful exposure events possible for each exposure ID
#' @param cross_reactivity_table an optional table which indicates cross-reactivity between exposure and biomarker quantities. Here users can specify whether other biomarker quantities are also protective against successful exposure.  Defaults to NULL.
#' @param sum_biomarkers if no `cross_reactivity_table` is provided, this flag determines if protection is calculated as the sum of all cross-reactive biomarkers from the `biomarker_map`, otherwise only uses biomarker for `biomarker_id==exposure_id`
#' @param ... Additional arguments
#'
#' @return A probability of successful exposure is returned
#' @export
#'
#' @examples
#' tmp_immune_history <- array(0, dim=c(1, 10, 1))
#' ## Toy example: individual has 1 prior exposure
#' tmp_immune_history[1,1,1] <- 1
#' ## Set all biomarker states to 3 for sake of example
#' tmp_biomarker_states <- array(0, dim=c(1,10,1))
#' tmp_biomarker_states[1,,1] <- 3
#' tmp_pars <- reformat_biomarker_map(example_model_pars_biphasic)
#' ## Probability of successful exposure (i.e., infection) depends on the biomarker quantity
#' immunity_model_ifxn_biomarker_prot(1,8,1,immune_histories=tmp_immune_history, 
#' biomarker_states=tmp_biomarker_states, demography=NULL, 
#' biomarker_map=example_biomarker_map_numeric, model_pars=tmp_pars, max_events=c(3,5))
immunity_model_ifxn_biomarker_prot <- function(i, t, x, immune_histories, 
                              biomarker_states, demography, biomarker_map, 
                              model_pars, max_events=rep(Inf, dim(immune_histories)[3]), cross_reactivity_table=NULL,
                              sum_biomarkers = FALSE,
                              ...){
  
  ## Count the total number of successful exposures to x thus far 
  curr_ifx_events<-sum(immune_histories[i,1:t-1,x], na.rm=TRUE)
  
  ## If the current number of successful exposures to x is less than the maximum number of successful exposures
  if(curr_ifx_events<max_events[x]){
    
    ## If a cross reactivity table is specify, calculate the current the individuals current biomarker quantity for all protective biomarker types.
    if(!is.null(cross_reactivity_table)){
      cr <- cross_reactivity_table[exposure_id == x,]
      curr_b <- biomarker_states[i,t,cr$biomarker_id]*cr$cross_reactivity
      curr_b <- sum(curr_b)
    } else{
    ## If no cross reactivity table is specified, the model assumes that all biomarkers boosted by the exposure as defined in the biomarker_map are protective to the same extent.
    if(sum_biomarkers){
      b<-biomarker_map$biomarker_id[biomarker_map$exposure_id==x]
    } else {
      b <- x
    }
    ## Find current biomarker quantity to all relevant biomarkers
    curr_b <- biomarker_states[i,t,b] 
    curr_b <- sum(curr_b)
    }
    
    ## Pull out necessary variables 
    biomarker_prot_midpoint <- model_pars$mean[model_pars$exposure_id==x & model_pars$name=="biomarker_prot_midpoint"] 
    biomarker_prot_width <- model_pars$mean[model_pars$exposure_id==x  & model_pars$name=="biomarker_prot_width"]
    
    
    prob_success<- (1-biomarker_protection(curr_b, biomarker_prot_midpoint, biomarker_prot_width))
    
    return(prob_success)
  }
  else{
    return(0)
  }
}


#' Immunity model for vaccination and infection events with biomarker-quantity-mediated protection
#' 
#' @description This immunity model should be used if exposures represent vaccination and infection events. The probability of a successful vaccination exposure event depends on the number of vaccines received prior to time t while the probability of successful infection is dependent on the biomarker quantity at the time of exposure and the total number of successful infections prior to that point.
#' 
#' @inheritParams immunity_model_vacc_ifxn_simple
#' @param cross_reactivity_table an optional table which indicates cross-reactivity between exposure and biomarker quantities. Here users can specify whether other biomarker quantities are also protective against successful exposure.  Defaults to NULL.

#' @param ... Additional arguments
#'
#' @return A probability of successful exposure is returned
#' @export
#'
#' @examples
#' tmp_immune_history <- array(0, dim=c(1, 10, 2))
#' ## Toy example: individual has 3 prior exposures to exposure ID 1, and none to exposure ID 2
#' tmp_immune_history[1,1:3,1] <- 1
#' ## Set all biomarker states to 3 for sake of example
#' tmp_biomarker_states <- array(0, dim=c(1,10,1))
#' tmp_biomarker_states[1,,1] <- 3
#' tmp_demography <- dplyr::tibble(i=1, birth=1)
#' tmp_pars <- reformat_biomarker_map(example_model_pars_biphasic)
#' ## Successful exposure probability for exposure ID 1 (representing vaccination) 
#' ## is 1 or 0 depending on exposure history
#' immunity_model_vacc_ifxn_biomarker_prot(1,8,1,immune_histories=tmp_immune_history, 
#' biomarker_states=tmp_biomarker_states, demography=tmp_demography, 
#' biomarker_map=example_biomarker_map_numeric, model_pars=tmp_pars,
#' max_events=c(3),vacc_exposures=c(1),vacc_age=c(1))
#' 
#' ## Successful exposure probability for exposure ID 2 (representing infection) 
#' ## is conditional on titer
#' immunity_model_vacc_ifxn_biomarker_prot(1,8,2,immune_histories=tmp_immune_history, 
#' biomarker_states=tmp_biomarker_states, demography=tmp_demography, 
#' biomarker_map=example_biomarker_map_numeric, model_pars=tmp_pars,max_events=c(3,10),
#' vacc_exposures=c(1),vacc_age=c(1))
immunity_model_vacc_ifxn_biomarker_prot <- function(i, t, x, immune_histories,
                           biomarker_states, demography, biomarker_map, model_pars, 
                           max_events=rep(Inf, dim(immune_histories)[3]), vacc_exposures=rep(Inf, dim(immune_histories)[3]), 
                           vacc_age=1, cross_reactivity_table=NULL, ...){
  ## If an exposure event is a vaccination event, then guaranteed exposure unless the individual has already been vaccinated
  if(x %in% c(vacc_exposures)){  
    ## Calculate the individual's current age
    birth_time<-unique(demography$birth[demography$i==i])
    curr_age<- t-birth_time
    
    ## If the individual is above the minimum age of vaccination 
    if(curr_age>=vacc_age[x]){
    ## Count the total number of successful exposures to e thus far 
    curr_vacc_events<-sum(immune_histories[i,1:t-1,x], na.rm=TRUE)
    ## If number of successful exposures is less than the max number of vaccination events then vaccine exposure is successful 
    if(curr_vacc_events<max_events[x]){
      return(1)
    }else{
      return(0)
    }
    }
    if(curr_age<vacc_age[x]){
      return(0)
    }
  } 
  else {
    ## If the exposure event is an infection event
    ## Count the total number of successful exposures to x thus far 
    curr_ifx_events<-sum(immune_histories[i,1:t-1,x], na.rm=TRUE)
    
    ## If the current number of successful exposures to x is less than the maximum number of successful exposures
    if(curr_ifx_events<max_events[x]){
      
      ## If a cross reactivity table is specify, calculate the current the individuals current biomarker quantity for all protective biomarker types.
      if(!is.null(cross_reactivity_table)){
        cr<- cross_reactivity_table %>% filter(exposure_id==x)
        cr$curr_biom_quant <- biomarker_states[i,t,cr$biomarker_id]
        cr<- cr %>% mutate(prot_b=cross_reactivity*curr_biom_quant)
        curr_b<- sum(cr$prot_b)
        
      } else{
      
        ## If no cross reactivity table is specified, the model assumes that all biomarkers boosted by the exposure as defined in the biomarker_map are protective to the same extent.
         b<-biomarker_map$biomarker_id[biomarker_map$exposure_id==x]
        ## Find current biomarker quantity to all relevant biomarkers
        curr_b <- biomarker_states[i,t,b]
        curr_b <- sum(curr_b)
      }
      
        ## Pull out necessary variables 
       biomarker_prot_midpoint <- model_pars$mean[model_pars$exposure_id==x & model_pars$name=="biomarker_prot_midpoint"]
       biomarker_prot_width <- model_pars$mean[model_pars$exposure_id==x & model_pars$name=="biomarker_prot_width"]
     
       prob_success<- (1-biomarker_protection(curr_b, biomarker_prot_midpoint, biomarker_prot_width))
    
       return(prob_success)
    }
    else{
      return(0)
    }
  }
}

