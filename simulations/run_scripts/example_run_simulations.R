####################################################
## Run Simulations

## Load functions, soon to be packaged
source('rDaNCES/R/rDaNCES_proxy.R')

####################################################
## 1. SET UP

### A. Simulation info

# Template of config file, example flocking_final.json
config_tmp_name <- c('simulations\\config_templates\\config_temp_chase_close',
                     'simulations\\config_templates\\config_temp_chase_centr',
                     'simulations\\config_templates\\config_temp_chase_iso',
                     'simulations\\config_templates\\config_temp_chase_peripheral',
                     'simulations\\config_templates\\config_temp_chase_stoop'
                     )

gen_config_path <- 'simulations\\generated_configs\\' 

# Name of the folder that contains the model exe (should be in working directory)
model_exe_name <- 'pigeon_model.exe' 
model_fname <- c('simulations\\models\\chase_closest\\',
                 'simulations\\models\\chase_centroid\\',
                 'simulations\\models\\chase_most_isolated\\',
                 'simulations\\models\\chase_most_peripheral\\',
                 'simulations\\models\\chase_stooping\\')

# Output folder
data_out_path  <- 'data\\simulated\\' 
logs_path <- 'simulations\\sim_logs\\'
## Name of sets
## In this examnple we will run sets for varying group sizes and number of
## interacting neighbours of the prey in alignment and attraction rules
sets <- c('test_closest', ## N = 10, topological range = 5
          'test_centroid',
          'test_most_isolated',
          'test_most_peripheral',
          'test_stooping'
         )

# Number of repetitions of each parameter set:
reps <- 1


### B. Parameter values
# Change some of the default parameter values from config template (NA to keep)

## Bi. General
# N <- c(10, 30) # Flock size per set
#chase_w <- list(c(1), c(5,10)) # will vary at each set
#hunt_sp_sc <- c(1.2, 1.5)
#TUKI dodas variabilne vrednosti

## Bii. Coordination
# 
# # Topological interactions
# topo <- list(c(4), c(7), c(4), c(7))
# fov <- c(210, 270, 330)
# 
# # Relative weights
# coh_turn_w <- c(0.1) ## change from default but not deviate across runs
# sep_w <- c(1, 2)
# align_w <- c(2, 5)
# 
# ## Biii. Collective turning
# roost_w <- c(2, 5)
# 
# ## Can set all others to NA and add them at the construct param set below for
# ## tidyness, eg:
# # aer_cs_sd <- NA
# # fl_tr <- NA
# # pred_shadow_bangl <- NA
# 


####################################################
## 2. MAIN LOOP - Run sims

result_folds <- c()

for (i in 1:length(sets))
{
  param_set <- construct_ParamSets( # TUKI uporabs variabilne vrednosti
    #N = N, ## i if varying per set
    output_folder = sets[i],
    #chase_w = chase_w[[i]],
    #prey_speed_scale = hunt_sp_sc[i]
  )
  
  dirs <- set_up_directories(out_folder = sets[i],
                             config_name = config_tmp_name[i],
                             model_exe = model_exe_name,
                             model_path = model_fname[i],
                             config_exp_path = gen_config_path,
                             results_path = data_out_path, 
                             logs_path = logs_path)
  
  set_up_simulations(param_set, dirs)
  
  ## Runs simulations
  run_simulations(dirs, reps)
  result_folds <- c(result_folds, dirs$DATA_OUT_PATH)
  
  ## Move data to external drive if too heavy
  #rt <- file.copy(dirs$DATA_OUT_PATH, 'D:/Projects/CollectiveTurns', recursive = TRUE) 
  #if (rt) {  fs::dir_delete(dirs$DATA_OUT_PATH) }
  print(paste0('Simulations set done: ', i))
}


