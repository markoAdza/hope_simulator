
#' @export
run_simulations <- function(dirs_list,
                            reps)
{
  ####################################################
  ## RUN MODEL
  if (length(list.files(dirs_list$NEW_CONFIGS_PATH)) < 1)
  {
    stop("Config folder is empty! Make sure to run the set up simulations function first.")
  }
  
  cat(paste0('Simulations sets: ', dirs_list$OUT_FOLD, '\n'), file = dirs_list$LOG_FULL_PATH)
  k <- 1
  pb <- txtProgressBar(min = 0, max = reps*length(list.files(dirs_list$NEW_CONFIGS_PATH)), style = 3)
  for (i in list.files( dirs_list$NEW_CONFIGS_PATH))
  {
    repets <- reps
    while (repets > 0)
    {
      cat(paste0('Simulation with config: ', i, ', repetition: ', reps - repets + 1, ' \n'), file =  dirs_list$LOG_FULL_PATH, append = TRUE)
      #shell(paste0(dirs_list$MODEL_RUN_DIR,' config=',  dirs_list$NEW_CONFIGS_PATH, '/', i, ' exp_files=false --headless',' >> ',  dirs_list$LOG_FULL_PATH))
      
      system2(
        normalizePath(dirs_list$MODEL_RUN_DIR),
        args = c(
          paste0("config=", dirs_list$NEW_CONFIGS_PATH, '/', i),
          "exp_files=false",
          "--headless"
        ),
        stdout = "", stderr = ""
      )
      
      repets <- repets - 1
      k <- k + 1
      setTxtProgressBar(pb, k)
    }
  }
}
