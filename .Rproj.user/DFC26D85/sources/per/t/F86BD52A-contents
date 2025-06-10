#' @title Create parameter set
#' @author Marina Papadopoulou 
#' @description Creates a data frame with all the combinations of each parameter vector
#' @param ps Parameters set class object
#' @return a column-named dataframe with each conbination of the input parameters (as rows)
#' @export
create_param_set <- function(
    ps
)
{
  # CAUTION: ORDER MATTERS
  param_order <-  c('N', 
                    'Tmax', 
                    'aer_cs_sd',
                    'fl_aer_w', 
                    'fl_cs', 
                    'coh_sp_w',
                    'coh_sp_topo',  
                    'coh_sp_ffov', 
                    'coh_sp_mindist', 
                    'coh_sp_maxdist',
                    'coh_t_topo', 
                    'coh_t_w', 
                    'ali_topo', 
                    'ali_w', 
                    'sep_w', 
                    'sep_min',
                    'sep_topo',
                    'roost_w',
                    'react_time',
                    'output_folder',
                    'wig_w', 
                    'roost_s_freq', 
                    'turn_dur', 
                    'dist2home',
                    'deg2home',
                    'fov', 
                    'turn_chars',
                    'copy_fov', 
                    'copy_topo');
  
  prey_param <- expand.grid(ps@N, 
                            ps@Tmax,
                            ps@aer_cs_sd, 
                            ps@fl_aer_w, 
                            ps@fl_aer_cs, 
                            ps@coh_speed_w,
                            ps@coh_speed_topo,
                            ps@coh_speed_ffov,
                            ps@coh_speed_mindist,
                            ps@coh_speed_maxdist,
                            ps@coh_turn_topo, 
                            ps@coh_turn_w, 
                            ps@ali_topo,
                            ps@ali_w, 
                            ps@sep_w,
                            ps@sep_mindist, 
                            ps@sep_topo, 
                            ps@roost_w,
                            ps@fl_tr, 
                            ps@output_folder,
                            ps@wig_w,
                            ps@roost_s_freq, 
                            ps@turn_dur,
                            ps@dist2home, 
                            ps@deg2home, 
                            ps@fov,
                            ps@turn_chars, 
                            ps@copy_fov, 
                            ps@copy_topo);
  
  colnames(prey_param) <- param_order;
  return(prey_param);
}
