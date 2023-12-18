# PROJECTS ENDPOINTS


#' Get all projects available in Pluto
#'
#' @description
#' Fetches metadata for all projects the requester has permission to view
#'
#' @param limit Max number of projects to return, default 1000
#' @returns API response object containing `count`, a count of the total projects,
#' and `items`, an array of project objects
#' @export
pluto_get_projects <- function(limit = 1000){

  url_path <- paste0('lab/projects/?limit=', limit)
  return(pluto_GET(url_path))

}


#' Get project details
#'
#' @description
#' Fetches metadata for a project in Pluto
#'
#' @param project_id Pluto project ID (e.g. PLXP02355)
#' @returns API response object containing metadata fields for the project:\tabular{ll}{
#'    \code{uuid} \tab Unique identifier for the project \cr
#'    \tab \cr
#'    \code{pluto_id} \tab Pluto project ID (e.g. PLXP02355) \cr
#'    \tab \cr
#'    \code{name} \tab Project name \cr
#'    \tab \cr
#'    \code{share_level} \tab Shortname for share level (private, shareable, public) \cr
#'    \tab \cr
#'    \code{owner} \tab User who currently owns the project \cr
#'    \tab \cr
#'    \code{experiments_count} \tab Integer, number of (non-archived) experiments in project \cr
#'    \tab \cr
#'    \code{is_archived} \tab Boolean, whether the project is archived \cr
#'    \tab \cr
#'    \code{created_at} \tab Date the project was created \cr
#' }
#' @export
pluto_get_project <- function(project_id){

  url_path <- paste0('lab/projects/', project_id, '/')
  return(pluto_GET(url_path))

}


#' List all experiments in a project in Pluto
#'
#' @description
#' Fetches metadata for all experiments in a given project in Pluto
#'
#' @param project_id Pluto project ID (e.g. PLXP02355)
#' @returns API response object containing `count`, a count of the total experiments
#' in the project, and `items`, an array of experiments
#' @export
pluto_get_project_experiments <- function(project_id){

  url_path <- paste0('lab/projects/', project_id, '/experiments')
  return(pluto_GET(url_path))

}


#' Read all experiments in a Pluto project into a data frame
#'
#' @description
#' Fetches metadata for all experiments in a given project in Pluto and stores it
#' in a data.frame
#'
#' @param project_id Pluto project ID (e.g. PLXP02355)
#' @returns A data.frame containing columns:\tabular{ll}{
#'    \code{experiment_id} \tab Pluto ID for the experiment (e.g. PLX123456) \cr
#'    \tab \cr
#'    \code{name} \tab Experiment name \cr
#'    \tab \cr
#'    \code{organism} \tab Computer-friendly shortname for organism (e.g. human, mouse) \cr
#'    \tab \cr
#'    \code{experiment_type} \tab Shortname for experiment type (e.g. rnaseq, atacseq) \cr
#'    \tab \cr
#'    \code{share_level} \tab Shortname for share level (private, shareable, public) \cr
#'    \tab \cr
#'    \code{status} \tab Shortname for experiment status (draft, pending, pending_complete, complete, failed) \cr
#'    \tab \cr
#'    \code{assay_data_units} \tab Shortname for assay data units (e.g. raw_counts) \cr
#'    \tab \cr
#'    \code{plot_count} \tab Integer, total plots created for the experiment \cr
#'    \tab \cr
#'    \code{owner} \tab First and last name of the user who currently owns the experiment \cr
#'    \tab \cr
#'    \code{created_by} \tab First and last name of the user who created the experiment \cr
#'    \tab \cr
#'    \code{created_at} \tab Date the experiment was created \cr
#' }
#' @export
pluto_read_project_experiments <- function(project_id){

  project_response <- pluto_get_project_experiments(project_id)
  experiment_count <- project_response$count
  experiment_list <- project_response$items

  final_df = data.frame()

  if (experiment_count > 0){
    for (i in 1:experiment_count){

      # Subset to a few summary columns
      expt <- experiment_list[[i]]
      df <- data.frame(
        experiment_id = expt$pluto_id,
        name = expt$name,
        organism = expt$organism$shortname,
        experiment_type = expt$type$shortname,
        share_level = expt$share_level,
        status = expt$status,
        assay_data_units = null_to_na(expt$assay_data_units$units$shortname),
        plot_count = expt$plot_count,
        owner = paste(expt$experiment_owner$first_name, expt$experiment_owner$last_name),
        created_by = paste(expt$created_by$first_name, expt$created_by$last_name),
        created_at = format(as.Date(expt$created_at), format="%Y-%m-%d")
      )

      # Append cleaned row to the final data frame
      final_df <- rbind(final_df, df)
    }
  }

  return(final_df)

}
