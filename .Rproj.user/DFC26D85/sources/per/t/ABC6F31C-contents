
#' @export
set_up_simulations <- function(param_set_class,
                               dirs_list)
{
  ## CONFIG
  conf <- load_config(dirs_list$CONFIG_PATH)
  new_par <- create_param_set(param_set_class)
  
  write.csv(new_par, 
            paste0(dirs_list$RESULTS_SPEC_TMP_PATH,
                   dirs_list$OUT_FOLD, 
                   '_config_params.csv'))
  
  # create configs
  config_change(conf, 
                new_par, 
                paste0(dirs_list$NEW_CONFIGS_PATH,
                       '\\',
                       dirs_list$CONFIGS_NAME), 
                '')
}