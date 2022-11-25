#' Monophasic antibody boosting-waning model
#' 
#' @description This monophasic antibody boosting-waning model model assumes that for each exposure there is a boost and boost waning parameter
#'
#' @param i Individual
#' @param t1 time
#' @param b biomarker
#' @param exposure_histories An array of exposure histories across all individuals, time steps and exposure IDs
#' @param antibody_states An array of antibody states across all individuals, time steps and biomarker IDs
#' @param kinetics_parameters A tibble of parameters needed for the antibody kinetics model for all biomarkers 
#' @param biomarker_map A table specifying the relationship between exposure IDs and biomarker IDs
#' @param ... 
#'
#' @return A titer value is returned 
#' @export
#'
#' @examples
antibody_model_monophasic <-  function(i, t1, b, exposure_histories, antibody_states, kinetics_parameters, biomarker_map, ...){
  ## Find which successful exposures correspond to this biomarker 
  exposure_id_tmp<-biomarker_map$exposure_id[biomarker_map$biomarker_id==b]
  
  ## Find all exposures up until current time for this individual and exposure type
  exp_history <- exposure_histories[i,1:t1,exposure_id_tmp]
  
  ## Set starting titer to 0
  titer<-0
  
  ## Calculate current titer if there has been an exposure 
  if(sum(exp_history,na.rm = TRUE)==0){
    return(0)
  }
  if(sum(exp_history,na.rm = TRUE)>0){
    ## Extract all kinetics_parameters for biomarker 
    b_tmp<-b
    
    tmp_kinetics_parameters <- data.table(kinetics_parameters[[i]])
    tmp_kinetics_parameters<-tmp_kinetics_parameters[tmp_kinetics_parameters$b==b_tmp,] ## Since you are going through time, all parameters will only be from the current or previous times?
    
    # setkey(tmp_kinetics_parameters, cols="i","t","e","b","name","value", "realized_value")
    tmp_boost <- tmp_kinetics_parameters[tmp_kinetics_parameters$name == "boost",] 
    tmp_wane <- tmp_kinetics_parameters[tmp_kinetics_parameters$name == "wane",] 
    
    
    
    for(j in seq_along(tmp_boost$realized_value)){
      titer<- titer + tmp_boost$realized_value[j]*max(0,1-tmp_wane$realized_value[j]*(t1-tmp_wane$t[j]))
    }
    titer
  }
}

#' Biphasic antibody boosting-waning model
#' 
#' @description Biphasic antibody boosting-waning model. This model assumes that for each exposure there is a set of long-term boost, long-term boost waning, short-term boost, and short-term boost waning parameters
#'
#' @param i Individual
#' @param t1 time
#' @param b biomarker
#' @param exposure_histories An array of exposure histories across all individuals, time steps and exposure IDs
#' @param antibody_states An array of antibody states across all individuals, time steps and biomarker IDs
#' @param kinetics_parameters An object of all kinetics parameters for all exposures
#' @param biomarker_map A table specifying the relationship between exposure IDs and biomarker IDs
#' @param ... 
#'
#' @return A titer value is returned 
#' @export
#'
#' @examples
antibody_model_biphasic <-  function(i, t1, b, exposure_histories, antibody_states, kinetics_parameters, biomarker_map, ...){
  ## Find which successful exposures correspond to this biomarker 
  exposure_id_tmp<-biomarker_map$exposure_id[biomarker_map$biomarker_id==b]
  
  ## Find all exposures up until current time for this individual and exposure type
  exp_history <- exposure_histories[i,1:t1,exposure_id_tmp]
  
  ## Set starting titer to 0
  titer<-0
  
  ## Calculate current titer if there has been an exposure 
  if(sum(exp_history,na.rm = TRUE)==0){
    return(0)
  }
  if(sum(exp_history,na.rm = TRUE)>0){
    ## Extract all kinetics_parameters for biomarker 
    b_tmp<-b
    
    tmp_kinetics_parameters <- data.table(kinetics_parameters[[i]])
    tmp_kinetics_parameters<-tmp_kinetics_parameters[tmp_kinetics_parameters$b==b_tmp,] ## Since you are going through time, all parameters will only be from the current or previous times?
    
    # setkey(tmp_kinetics_parameters, cols="i","t","e","b","name","value", "realized_value")
    tmp_boost_long <- tmp_kinetics_parameters[tmp_kinetics_parameters$name == "boost_long",] 
    tmp_boost_short <- tmp_kinetics_parameters[tmp_kinetics_parameters$name == "boost_short",] 
    
    tmp_wane_long <- tmp_kinetics_parameters[tmp_kinetics_parameters$name == "wane_long",] 
    tmp_wane_short <- tmp_kinetics_parameters[tmp_kinetics_parameters$name == "wane_short",] 
    
    
    for(j in seq_along(tmp_boost_long$realized_value)){
      titer<- titer + tmp_boost_long$realized_value[j]*max(0,1-tmp_wane_long$realized_value[j]*(t1-tmp_wane_long$t[j])) + tmp_boost_short$realized_value[j]*max(0,1-tmp_wane_short$realized_value[j]*(t1-tmp_wane_short$t[j]))
    }
    titer
    
  }
}
