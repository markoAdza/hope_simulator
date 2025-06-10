
#' @title Get simulation parameters
#' @author Marina Papadopoulou
#' @description Extract parameters of interest from the config file
#' @param config the config file of a simulation
#' @return a named list with all parameters of interest
#' @export
get_parameters <- function(config, verbose = FALSE)
{
  params <- list()
  
  params$N <-config$Starling$N
  params$cs_sd <- config$Starling$aero$cruiseSpeedSd
  params$speed <- config$Starling$aero$cruiseSpeed
  #  params$aero_w <- config$Starling$aero$w
  
  for (m in 1:length(config$Simulation$Analysis$Observers))
  {
    obs <- config$Simulation$Analysis$Observers[[m]]
    
    if (obs$output_name == 'roosting')
    {
      params$roost_s_freq <- config$Simulation$Analysis$Observers[[m]]$sample_freq 
    }
  }
  
  for (stt in 1:length(config$Starling$states))
  {
    if ( config$Starling$states[[stt]]$description == 'normal flocking' )
    {
      params$react_time <- config$Starling$states[[stt]]$tr
      params$aero_w <- config$Starling$states[[stt]]$aeroState$w
      
      for (m in 1:length(config$Starling$states[[stt]]$actions))
      {
        act <- config$Starling$states[[stt]]$actions[[m]]
        if (act$name == 'copy_escape')
        {
          params$copy_fov <- config$Starling$states[[stt]]$actions[[m]]$fov 
          params$copy_topo <- config$Starling$states[[stt]]$actions[[m]]$topo 
        }
        
        if (act$name == 'cohere_accel_forwards')
        {
          params$coh_sp_w <- config$Starling$states[[stt]]$actions[[m]]$w 
          params$coh_sp_topo<- config$Starling$states[[stt]]$actions[[m]]$topo 
          
        }
        
        if (act$name == 'cohere_centroid_distance')
        {
          params$coh_t_topo<- config$Starling$states[[stt]]$actions[[m]]$topo 
          params$coh_t_w<- config$Starling$states[[stt]]$actions[[m]]$w 
          params$coh_fov <- config$Starling$states[[stt]]$actions[[m]]$fov
        }
        
        if (act$name == 'align_n')
        {
          params$ali_topo<- config$Starling$states[[stt]]$actions[[m]]$topo 
          params$ali_w<- config$Starling$states[[stt]]$actions[[m]]$w 
          params$ali_fov <- config$Starling$states[[stt]]$actions[[m]]$fov
        }
        
        if (act$name == 'avoid_n_position')
        {
          params$sep_w<- config$Starling$states[[stt]]$actions[[m]]$w 
          params$sep_min<- config$Starling$states[[stt]]$actions[[m]]$minsep 
          params$sep_topo<- config$Starling$states[[stt]]$actions[[m]]$topo 
        }
        
        if (act$name == 'wiggle')
        {
          params$wig_w<- config$Starling$states[[1]]$actions[[m]]$w
        }
      }
    }
    
    if ( config$Starling$states[[stt]]$description == 'roosting')
    {
      
      for (m in 1:length(config$Starling$states[[stt]]$actions))
      {
        act <- config$Starling$states[[stt]]$actions[[m]]
        
        if (act$name == 'relative_roosting_persistant' | act$name == 'turn_tendency')
        {
          params$turn_type <- 'roosting'
          params$roost_w <- config$Starling$states[[stt]]$actions[[m]]$w 
          params$esc_turn <- as.numeric(NA)
        }
      }
    }
    if ( config$Starling$states[[stt]]$description == 'escape turn' )
    {
      
      for (m in 1:length(config$Starling$states[[stt]]$actions))
      {
        act <- config$Starling$states[[stt]]$actions[[m]]
        
        if (act$name == 'random_t_turn_gamma_pred')
        {
          params$turn_type <- 'escape'
          params$esc_turn <- config$Starling$states[[stt]]$actions[[m]]$turn_mean 
          params$roost_w <- as.numeric(NA)
        }
        
        if (act$name == 'relative_roosting_persistant' | act$name == 'turn_tendency')
        {
          params$turn_type <- 'roosting'
          params$roost_w <- config$Starling$states[[stt]]$actions[[m]]$w 
          params$esc_turn <- as.numeric(NA)
        }
      }
    }
  }
  
  if (verbose)
  {
    print('Extracted parameters:')
    print(names(params))
  }
  return(params)
}



#' @title Get parameters vector
#' @author Marina Papadopoulou
#' @description Export ordered vector from list of parameters based on input of parameter names
#' @param params_list named list with all parameters of interest, result of get_parameters
#' @param par_order named list with all parameters of interest
#' @return a vector with the parameter values in the same order as the par_order
#' @export
get_params_vector <- function(params_list, par_order)
{
  if (all(par_order %in% names(params_list)))
  {
    p_values <- c()
    for (p in par_order)
    {
      p_values <- c(p_values, params_list[[p]])
    }
    return(p_values)
  }
  else
  {
    stop("Input parameter names not in the parameter list")
  }
}

