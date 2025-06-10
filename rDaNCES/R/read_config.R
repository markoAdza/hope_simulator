# Reading configs

#' @title Load config.json file
#' @author Marina Papadopoulou
#' @description Loads config file from json format used to generate simulated data, turning it into a list
#' @param path2config path to json config file
#' @return list of all model parameters
#' @export
load_config <- function(path2config)
{
  d <- rjson::fromJSON(file = path2config)
  return(d)
}

#' @title Get agent type config
#' @author Marina Papadopoulou
#' @description Extracts the config file of a specific agent type
#' @param config_list a list, the config file in a named list format
#' @param agent_type a string, the type of agent as named in the config file, e.g. 'Pigeon'
#' @return list of all agent type specific parameters
#' @export
agent_config <- function(config_list, agent_type)
{
  if (agent_type %in% names(config_list))
  {
    return(config_list[[agent_type]])
  }
  print("Agent name not matching config file.")
}

#' @title Get the simulation config
#' @author Marina Papadopoulou
#' @description Extracts the simulation part of the config file.
#' @param config_list a list, the config file in a named list format
#' @return list of all simulation parameters
#' @export
sim_config <- function(config_list)
{
  return(config_list$Simulation)
}

#' @title Number of states
#' @author Marina Papadopoulou
#' @description Get the number of states of a specific agent-type.
#' @param config_list a list, the config file in a named list format
#' @param agent_type a string, the type of agent as named in the config file, e.g. 'Pigeon'
#' @return numeric, the number of potential states of an agent
#' @export
states_size <- function(config_list, agent_type)
{
  if (agent_type %in% names(config_list))
  {
    acfg <- agent_config(config_list, agent_type)
    return(length(acfg$states))
  }
  print("Agent type not in config file.")
}

#' @title Number of states
#' @author Marina Papadopoulou
#' @description Get the number of states of a given agent type config.
#' @param agent_config a list, the config of a specific agent-type in a named list format
#' @return numeric, the number of potential states of the agent
#' @export
states_size <- function(agent_config)
{
  return(length(agent_config$states))
}


#' @title Actions of a state
#' @author Marina Papadopoulou
#' @description Get the actions included in a specific state
#' @param agent_config a list, the config of a specific agent-type in a named list format
#' @param state_n the number of a state
#' @return a vector with the names of all actions included in the state
#' @export
state_actions <- function(agent_config, state_n)
{
  st_act <- c()
  if (!(state_n > states_size(agent_config)))
  {
    cf <- get_state(agent_config, state_n)
    for (i in 1:length(cf$actions))
    {
      st_act <- c(st_act, cf$actions[[i]]$name)
    }
    return(st_act)
  }
  print(paste0("Input config has less than ", state_n," states"))
}

#' @title Actions of a state
#' @author Marina Papadopoulou
#' @description Get the actions included in a specific state
#' @param state_config a list, the config of a specific state of a specific agent-type in a named list format
#' @return a vector with the names of all actions included in the state
#' @export
state_actions_names <- function(state_config)
{
  st_act <- c()
  for (i in state_config$actions)
  {
    st_act <- c(st_act, i$name)
  }
  return(st_act)
}

#' @title State description
#' @author Marina Papadopoulou
#' @description Get the description of all actions included in a specific state
#' @param agent_config a list, the config of a specific agent-type in a named list format
#' @param state_n the number of a state
#' @return a string with the description of the state
#' @export
state_descr <- function(agent_config, state_n)
{
  if (!(state_n > states_size(agent_config)))
  {
    return(agent_config$states[[state_n]]$description)
  }
  print(paste0("Input config has less than ", state_n," states"))
}

#' @title Get state config
#' @author Marina Papadopoulou
#' @description Get a specific state of an agent
#' @param agent_config a list, the config of a specific agent-type in a named list format
#' @param state_n the number of a state
#' @return list of all parameters (the config) of a specific state of an agent type
#' @export
get_state <- function(agent_config, state_n)
{
  if (!(state_n > states_size(agent_config)))
  {
    return(agent_config$states[[state_n]])
  }
  print(paste0("Input config has less than ", state_n," states"))
}


#' @title Get state config
#' @author Marina Papadopoulou
#' @description Get a specific state of an agent
#' @param agent_config a list, the config of a specific agent-type in a named list format
#' @param state_n the number of a state
#' @return list of all parameters (the config) of a specific state of an agent type
#' @export
get_state_fromdescr <- function(agent_config, state_descr)
{
  for (k in 1:states_size(agent_config))
  {
    if (state_descr(agent_config, k) == 'normal flocking')
    {
      return(get_state(agent_config, k))
    }
  }
  print(paste0("No state with this name found"))
}


#' @title Get state config
#' @author Marina Papadopoulou
#' @description Get a specific state of an agent
#' @param agent_config a list, the config of a specific agent-type in a named list format
#' @param state_n the number of a state
#' @return list of all parameters (the config) of a specific state of an agent type
#' @export
get_action_fromdescr <- function(state_config, action_descr)
{
  for (m in 1:length(state_config$actions))
  {
    act <- state_config$actions[[m]]
    if (act$name == action_descr)
    {
      return(act)
    }
  }
  print(paste0("No action with this name found in this state"))
}



#' @title State composition
#' @author Marina Papadopoulou
#' @description Get the composition of actions of a specific state of an agent type
#' @param agent_config a list, the config of a specific agent-type in a named list format
#' @return list of all states with their actions composition
#' @export
states_composition <- function(agent_config)
{
  states_comp <- list()
  for (i in 1:states_size(agent_config))
  {
    states_comp[[i]] <- state_actions(agent_config, i)
    names(states_comp)[i] <- agent_config$states[[i]]$description
  }
  return(states_comp)
}
