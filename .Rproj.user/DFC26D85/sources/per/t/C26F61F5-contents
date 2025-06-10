
#' @title Initialize the class with all parameters that can be changed.
#' @description Class for the parameters of the computational model
#' @name ParamSets
#' @keywords class
#' @export
ParamSets <-  methods::setClass(
  "ParamSets",
  representation=list(
    output_folder = "character",
    N = "numeric",
    Tmax ="numeric",
    aer_cs_sd = "numeric",
    fl_aer_cs ="numeric",
    fl_aer_w = "numeric",
    coh_speed_w = "numeric",
    coh_speed_topo = "numeric",
    coh_speed_ffov = "numeric",
    coh_speed_mindist = "numeric",
    coh_speed_maxdist = "numeric",
    roost_w = "numeric",
    fl_tr = "numeric",
    coh_turn_topo = "numeric",
    coh_turn_w = "numeric",
    sep_w = "numeric",
    sep_mindist = "numeric",
    sep_topo =  "numeric",
    ali_topo = "numeric",
    ali_w = "numeric",
    wig_w = "numeric",
    roost_s_freq = "numeric",
    turn_dur = "numeric",
    deg2home = "numeric",
    dist2home = "numeric",
    fov = "numeric",
    turn_chars = "numeric",
    copy_fov = "numeric",
    copy_topo = "numeric"
  )
)

#' @title Construct param set
#' @description Construct a new ParamSets object
#' @name construct_ParamSets
#' @export
construct_ParamSets <- function(
    N = NA_integer_,
    Tmax = NA_integer_,
    aer_cs_sd = NA_integer_,
    fl_aer_cs = NA_integer_,
    fl_aer_w = NA_integer_,
    coh_speed_w = NA_integer_,
    coh_speed_topo = NA_integer_,
    coh_speed_ffov = NA_integer_,
    coh_speed_mindist = NA_integer_,
    coh_speed_maxdist = NA_integer_,
    roost_w = NA_integer_,
    fl_tr = NA_integer_,
    coh_turn_topo = NA_integer_,
    coh_turn_w = NA_integer_,
    sep_w = NA_integer_,
    sep_mindist = NA_integer_,
    sep_topo = NA_integer_,
    ali_topo = NA_integer_,
    ali_w = NA_integer_,
    output_folder = NA_character_,
    wig_w = NA_integer_,
    roost_s_freq = NA_integer_,
    turn_dur = NA_integer_,
    deg2home = NA_integer_,
    dist2home = NA_integer_,
    fov = NA_integer_,
    turn_chars = NA_integer_,
    copy_fov = NA_integer_,
    copy_topo = NA_integer_
)
{
  
  param_set  <- methods::new("ParamSets",
                             output_folder = output_folder,
                             N = N,
                             Tmax = Tmax,
                             aer_cs_sd = aer_cs_sd,
                             fl_aer_cs = fl_aer_cs,
                             fl_aer_w = fl_aer_w,
                             coh_speed_w = coh_speed_w,
                             coh_speed_topo = coh_speed_topo,
                             coh_speed_ffov = coh_speed_ffov,
                             coh_speed_mindist = coh_speed_mindist,
                             coh_speed_maxdist = coh_speed_maxdist,
                             roost_w = roost_w,
                             fl_tr = fl_tr,
                             coh_turn_topo = coh_turn_topo,
                             coh_turn_w = coh_turn_w,
                             sep_w = sep_w,
                             sep_mindist = sep_mindist,
                             sep_topo = sep_topo,
                             ali_topo = ali_topo,
                             ali_w = ali_w,
                             wig_w = wig_w, 
                             roost_s_freq = roost_s_freq,
                             turn_dur = turn_dur,
                             deg2home = deg2home,
                             dist2home = dist2home,
                             fov = fov,
                             turn_chars = turn_chars,
                             copy_fov = copy_fov,
                             copy_topo = copy_topo
  )
  
  return(param_set)
}
