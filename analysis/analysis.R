#!/usr/bin/env Rscript
source('rDaNCES/R/rDaNCES_proxy.R')

####################################################
## 3. Load data to start analysis

## replace  DATA_OUT_PATH 
folder_paths <- list.dirs(DATA_OUT_PATH)[-1]

## to store all data
sim_datas <- list()

## Loop through all files
k <- 1
pb <- txtProgressBar(min = 0, max = length(folder_paths), style = 3)
for (fpath in folder_paths)
{
  sim_data <- import_base_data(fpath, 
                               types = c('self', 'neighb'))
  
  ## discard initialization data
  sim_data <- discard_init_timesteps(sim_data, cut_time = 5) ## cut time in seconds
  
  sim_datas[[k]] <- sim_data
  k <- k + 1
  setTxtProgressBar(pb, k)
}

## OR import directly all the data without filtering
sim_datas <- import_multi_data(DATA_OUT_PATH,  types = c('self', 'neighb'))

## Save loaded files in R object
save(sim_datas, 
     file = 'data/sim_data_050625_lists.RData')
