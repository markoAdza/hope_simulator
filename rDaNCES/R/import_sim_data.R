# Importing simulated data:

#' @title Import flock data
#' @author Marina Papadopoulou
#' @description Imports all the data either produced by the model simulation.
#' @param folder_path path to data folder
#' @param types which data to load, from: timeseries, neighbours, flocks, roosting, curvature, transition matrix and forces.
#' @return list of all imported dataframes
#' @export
import_base_data <- function(folder_path,
                             types = c('self',
                                       'neighb',
                                       'flocks'
                                       ))
{
  out <- list()
  
  if ('neighb' %in% types) out$neighb <- data.table::fread(paste0(folder_path, '\\', 'all_neighbors.csv'))
  if ('self' %in% types) out$self <- data.table::fread(paste0(folder_path, '\\', 'time_series.csv'))
  if ('flocks' %in% types)out$flock <- data.table::fread(paste0(folder_path, '\\', 'flocks.csv'))
  if ('roost' %in% types)out$roost <- data.table::fread(paste0(folder_path, '\\', 'roosting.csv'))
  if ('curv' %in% types)out$curv <- data.table::fread(paste0(folder_path, '\\', 'curvature.csv'))
  if ('tm' %in% types)out$TM <- read.csv(paste0(folder_path, '\\', 'trans_matrix.csv'))
  if ('forces' %in% types)out$forces <- read.csv(paste0(folder_path, '\\', 'forces.csv'))
  
  
  cfg <- load_config(paste0(folder_path, '\\', 'composed_config.json'))
  out$config <- cfg
  out$sim_info$sim_id <- basename(dirname(paste0(folder_path, '\\', 'time_series.csv')))
  out$sim_info$N <- 1
  
  return(out)
}


#' @title Import multiple datasets
#' @author Marina Papadopoulou
#' @description Imports all the data saved from multiple model simulations. Calls import_base_data for each folder
#' @param head_folder_path path to result folder that contains all the single simulation folder results.
#' @param types which data to load, from: timeseries, neighbours, flocks, roosting, curvature, transition matrix and forces.
#' @return list of all simulation lists
#' @export
import_multi_data <- function(head_folder_path, 
                              types = c('self'))
{
  sim_data <- list()
  sim_id <- 0
  for (i in list.dirs(head_folder_path))
  {
    if (sim_id == 0 ) { sim_id <- sim_id + 1; next;}
    
    sim_data[[sim_id]] <- import_base_data(i, types)
    #print(sim_id)
    sim_id <- sim_id + 1
  }
  return(sim_data)
}


#' @title Discard initial timesteps
#' @author Marina Papadopoulou
#' @description Discards timesteps from the beginning of the simulation until a cut point from all the dataframes exported by the model.
#' @param data_list list of base model data, export of import_base_data
#' @param cut_time the time point in seconds from which onwards to keep in the dataframes
#' @return the input dataframe cut after the init cut point
#' @export
discard_init_timesteps <- function(data_list, cut_time = 1)
{
  for (i in 1:length(data_list))
  {
    if (is.data.frame(data_list[[i]]) & 'time' %in% colnames(data_list[[i]]))
    {
      data_list[[i]] <- data_list[[i]][data_list[[i]]$time > cut_time,]
    }
  }
  return(data_list)
}

