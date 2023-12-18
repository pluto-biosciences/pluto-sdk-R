# EXPERIMENTS ENDPOINTS

#' List all experiments available in Pluto
#'
#' @description
#' Fetches metadata for all experiments the requester has permission to view
#'
#' @param limit Max number of experiments to return, default 1000
#' @returns API response object containing `count`, a count of the total experiments
#' and `items`, an array of experiment objects
#' @export
pluto_get_experiments <- function(limit = 1000){

  url_path <- paste0('lab/experiments/?limit=', limit)
  return(pluto_GET(url_path))

}


#' Get experiment details
#'
#' @description
#' Fetches metadata for an experiment in Pluto
#'
#' @param experiment_id Pluto experiment ID (e.g. PLX239843)
#' @returns API response object containing metadata fields for the experiment:\tabular{ll}{
#'    \code{uuid} \tab Unique identifier for the experiment \cr
#'    \tab \cr
#'    \code{pluto_id} \tab Pluto experiment ID (e.g. PLX239843) \cr
#'    \tab \cr
#'    \code{name} \tab Experiment name \cr
#'    \tab \cr
#'    \code{description} \tab Experiment description \cr
#'    \tab \cr
#'    \code{organism} \tab Object containing organism (e.g. human) and genome build, if relevant \cr
#'    \tab \cr
#'    \code{share_level} \tab Shortname for share level (private, shareable, public) \cr
#'    \tab \cr
#'    \code{type} \tab Object containing assay information \cr
#'    \tab \cr
#'    \code{experiment_owner} \tab User who currently owns the experiment \cr
#'    \tab \cr
#'    \code{is_archived} \tab Boolean, whether the experiment is archived \cr
#'    \tab \cr
#'    \code{created_by} \tab User who created the experiment \cr
#'    \tab \cr
#'    \code{created_at} \tab Date the experiment was created \cr
#' }
#' @export
pluto_get_experiment <- function(experiment_id){

  url_path <- paste0('lab/experiments/', experiment_id, '/')
  return(pluto_GET(url_path))

}
