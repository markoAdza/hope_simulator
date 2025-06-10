# Create config files - changing parameters

#' @title Change the config of a Bird-type agent
#' @author Marina Papadopoulou (email = "marina.vp@outlook.com", ORCID = "0000-0002-6478-8365")
#' @description Replaces the existing values of the parameters in the config
#' with the ones from the input vector
#' @param conf_prey a list of lists, the config file of a bird-type agent
#' @param new_params a named vector of all values to be replaced, (a line
#' of the output dataframe of the create_params function
#' @return the config file (list of lists) with the changed parameters
#' @export
prey_config_change <- function(
    conf_prey,
    new_params
)
{
  if (!(is.na( new_params[1, 'N']))){ conf_prey$N <- new_params[1, 'N'] }
  if (!(is.na( new_params[1, 'aer_cs_sd']))){ conf_prey$aero$cruiseSpeedSd <- new_params[1, 'aer_cs_sd'] }
  if (!(is.na( new_params[1, 'fl_aer_w']))){ conf_prey$aero$w <- new_params[1, 'fl_aer_w'] }
  if (!(is.na( new_params[1, 'fl_cs']))){ conf_prey$aero$cruiseSpeed <- new_params[1, 'fl_cs'] }
  
  
  for (k in 1:states_size(conf_prey))
  {
    skchange <- get_state(conf_prey, k)
    if (!(is.na( new_params[1, 'react_time']))) { skchange$tr <- new_params[1, 'react_time']}
    if (!(is.na( new_params[1, 'fl_cs']))) { skchange$aeroState$cruiseSpeed <- new_params[1, 'fl_cs']}
    
    # Duration of turn
    if ( skchange$description == 'roosting' | skchange$description == 'escape turn' )
    {
      if (!(is.na( new_params[1, 'turn_dur']))){ skchange$duration <- new_params[1, 'turn_dur'] }
    }
    
    
    # ACTIONS
    for (m in 1:length(skchange$actions))
    {
      act <- skchange$actions[[m]]
      
      if (!(is.na( new_params[1, 'fov'])) & act$name != 'copy_escape'){
        skchange$actions[[m]]$fov <- new_params[1, 'fov'] 
      }
      
      if (act$name == 'relative_roosting_persistant' | act$name == 'turn_tendency')
      {
        if (!(is.na( new_params[1, 'roost_w']))) {  skchange$actions[[m]]$w <- new_params[1, 'roost_w'] }
        if (!(is.na( new_params[1, 'turn_chars']))) {  skchange$actions[[m]]$w <- new_params[1, 'turn_chars'] }
        if (!(is.na( new_params[1, 'dist2home']))) {  skchange$actions[[m]]$home_dist <- new_params[1, 'dist2home'] }
        if (!(is.na( new_params[1, 'deg2home']))) {  skchange$actions[[m]]$home_direction <- new_params[1, 'deg2home'] }
      }
      
      if (act$name == 'random_t_turn_gamma_pred')
      {
        if (!(is.na( new_params[1, 'turn_chars']))) {  skchange$actions[[m]]$turn_mean <- new_params[1, 'turn_chars'] }
      }
      
      if (act$name == 'copy_escape')
      {
        if (!(is.na( new_params[1, 'copy_fov']))) { skchange$actions[[m]]$fov <- new_params[1, 'copy_fov'] }
        if (!(is.na( new_params[1, 'copy_topo']))) { skchange$actions[[m]]$topo <- new_params[1, 'copy_topo'] }
      }
      
      if ( act$name == 'turn_tendency')
      {
        if (!(is.na( new_params[1, 'roost_w']))) {  skchange$actions[[m]]$w <- new_params[1, 'roost_w'] }
      }
      
      if (act$name == 'cohere_accel_forwards')
      {
        if (!(is.na( new_params[1, 'coh_sp_w']))) { skchange$actions[[m]]$w <- new_params[1, 'coh_sp_w'] }
        if (!(is.na( new_params[1, 'coh_sp_topo']))) { skchange$actions[[m]]$topo <- new_params[1, 'coh_sp_topo'] }
      }
      
      if (act$name == 'cohere_centroid_distance')
      {
        if (!(is.na( new_params[1, 'coh_t_topo']))) { skchange$actions[[m]]$topo <- new_params[1, 'coh_t_topo'] }
        if (!(is.na( new_params[1, 'coh_t_w']))) { skchange$actions[[m]]$w <- new_params[1, 'coh_t_w'] }
      }
      
      if (act$name == 'align_n')
      {
        if (!(is.na( new_params[1, 'ali_topo']))) { skchange$actions[[m]]$topo <- new_params[1, 'ali_topo'] }
        if (!(is.na( new_params[1, 'ali_w']))) { skchange$actions[[m]]$w <- new_params[1, 'ali_w'] }
      }
      
      if (act$name == 'avoid_n_position')
      {
        if (!(is.na( new_params[1, 'sep_w']))) { skchange$actions[[m]]$w <- new_params[1, 'sep_w'] }
        if (!(is.na( new_params[1, 'sep_topo']))) { skchange$actions[[m]]$topo <- new_params[1, 'sep_topo'] }
        if (!(is.na( new_params[1, 'sep_min']))) { skchange$actions[[m]]$minsep <- new_params[1, 'sep_min'] }
      }
      
      if (act$name == 'wiggle')
      {
        if (!(is.na( new_params[1, 'wig_w']))) { skchange$actions[[m]]$w <- new_params[1, 'wig_w'] }
      }
      
      
      conf_prey$states[[k]] <- skchange
    }
    
  }
  return(conf_prey)
}


#' @title Change the config of the main simulation settings
#' @author Marina Papadopoulou (email = "marina.vp@outlook.com", ORCID = "0000-0002-6478-8365")
#' @description Replaces the existing values of the parameters in the config
#' with the ones from the input vector
#' @param conf_sim a list of lists, the config file of the simulation settings
#' @param new_params a named vector of all values to be replaced, (a line
#' of the output dataframe of the create_params function
#' @return the config file (list of lists) with the changed parameters
#' @export
sim_config_change <- function(
    conf_sim,
    new_params
)
{
  if (!(is.na( new_params[1, 'output_folder'])))
  {
    conf_sim$Analysis$data_folder <- new_params[1, 'output_folder']
  }
  if (!(is.na( new_params[1, 'Tmax']))) { conf_sim$Tmax <- new_params[1, 'Tmax'] }
  
  if (!(is.na( new_params[1, 'roost_s_freq'])))
  {
    for (m in 1:length(conf_sim$Analysis$Observers))
    {
      obs <- conf_sim$Analysis$Observers[[m]]
      
      if (obs$type == 'RoostingData')
      {
        conf_sim$Analysis$Observers[[m]]$sample_freq <- new_params[1, 'roost_s_freq']
      }
    }
    
  }
  
  return(conf_sim)
}




#' @title Change a config file
#' @author Marina Papadopoulou (email = "marina.vp@outlook.com", ORCID = "0000-0002-6478-8365")
#' @description Replaces the existing values of the parameters in the config
#' with the ones from the input vector
#' @param config_temp a list of lists, the full config file of a simulation
#' @param df_param a named vector of all values to be replaced, (a line
#' of the output dataframe of the create_params function
#' @param exp_name the name with which to export the changed config
#' @return none, exports a json file with the changed config
#' @export
config_change <- function(
    config_temp,
    df_param,
    exp_name,
    bunch_id
)
{
  conf_prey <- agent_config(config_temp, 'Pigeon')
  
  for (i in 1:length(df_param[,1]))
  {
    ## Prey config
    config_temp$Prey <- prey_config_change(conf_prey, df_param[i,])
    
    ## Simulation change
    config_temp$Simulation <- sim_config_change(config_temp$Simulation, df_param[i,])
    
    config_temp$Simulation$esc_states <- list(2)
    config_temp$Pred$transitions$edges <- list(0)
    ## Pred change
    exportJson <- rjson::toJSON(config_temp)
    write(exportJson, paste0(exp_name, i, bunch_id,  ".json"))
  }
}



