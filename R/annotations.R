# Cluster annotation sets for single-cell experiments.
#
# Note: the existing function `pluto_get_cluster_annotation_sets` is
# preserved in experiments_data.R. These helpers are additive and give
# symmetric access to the annotation-set resource plus its nested
# annotations.


#' List cluster annotation sets for a single-cell experiment
#' @param experiment_id Pluto experiment ID.
#' @returns A list of annotation-set records.
#' @export
pluto_list_annotation_sets <- function(experiment_id){
  resp <- pluto_GET(paste0(
    "lab/experiments/", experiment_id, "/annotation-sets/"
  ))
  # Endpoint returns a bare list. Strip response_status_code.
  if (is.null(resp$items) && is.null(resp$count)){
    status <- resp$response_status_code
    resp$response_status_code <- NULL
    return(unname(resp))
  }
  resp$items %||% resp$results %||% list()
}


#' Get the detail record for a single annotation set
#' @param experiment_id Pluto experiment ID.
#' @param set_uuid Annotation set UUID.
#' @returns The annotation set record.
#' @export
pluto_get_annotation_set <- function(experiment_id, set_uuid){
  pluto_GET(paste0(
    "lab/experiments/", experiment_id,
    "/annotation-sets/", set_uuid, "/"
  ))
}


#' List annotations (clusters) within a set
#' @param experiment_id Pluto experiment ID.
#' @param set_uuid Annotation set UUID.
#' @returns A list of annotation records.
#' @export
pluto_list_annotations <- function(experiment_id, set_uuid){
  resp <- pluto_GET(paste0(
    "lab/experiments/", experiment_id,
    "/annotation-sets/", set_uuid, "/annotations/"
  ))
  if (is.null(resp$items) && is.null(resp$count)){
    status <- resp$response_status_code
    resp$response_status_code <- NULL
    return(unname(resp))
  }
  resp$items %||% resp$results %||% list()
}
