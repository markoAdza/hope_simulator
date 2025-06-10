# Multisim analysis

#' @export
accum_sim_data <- function(data_lists)
{
  accum_data <- list(vector(mode = 'list', length = 9))
  k <- 1
  for (i in data_lists)
  {
    print(paste0('Adding output ', k, '...'))
    
    if (length(accum_data[[1]][[1]]) == 0 )
    {
      accum_data[[1]] <- i
      k <- k+1
      next
    }
    
    toadd <- 1
    dts <- 1
    for (lst in accum_data)
    {
      clean_i_config <- i$config
      clean_lst_config <- lst$config
      clean_i_config$Simulation <- clean_i_config$Simulation[names(clean_i_config$Simulation) != 'Analysis']
      clean_lst_config$Simulation <- clean_lst_config$Simulation[names(clean_lst_config$Simulation) != 'Analysis']
      
      if (all(useful::compare.list(clean_i_config, clean_lst_config))) # if same parameterization will merge them
      {
        i <- i[names(i) != 'config']
        for (nm in names(i))
        {
          if (nm == 'sim_info')
          {
            accum_data[[dts]][[nm]]$sim_id <- c(accum_data[[dts]][[nm]]$sim_id,  i[[nm]]$sim_id)
            accum_data[[dts]][[nm]]$N <- accum_data[[dts]][[nm]]$N + i[[nm]]$N
            next
          }
          accum_data[[dts]][[nm]] <- rbind(accum_data[[dts]][[nm]], i[[nm]])
        }
        toadd <- 0
        break
      }
      dts<- dts + 1
    }
    if (toadd) { accum_data[[length(accum_data) + 1]] <- i }
    k <- k+1
  }
  return(accum_data)
}

