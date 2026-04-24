# Automated bulk-pipeline helpers.
#
# Unlike single-cell Workflows (which are JWT-only and therefore not
# reachable from the SDK), these endpoints accept token auth and are safe
# to wrap.


#' List automated pipelines available for an experiment
#'
#' @param experiment_id Pluto experiment ID.
#' @returns The API response (a list of pipeline records).
#' @export
pluto_list_pipelines <- function(experiment_id){
  resp <- pluto_GET(paste0("lab/experiments/", experiment_id, "/pipelines/"))
  # Endpoint may return a bare list — strip response_status_code so callers
  # receive a clean list they can iterate.
  if (is.null(resp$items) && is.null(resp$count) && is.null(resp$results)){
    resp$response_status_code <- NULL
    return(unname(resp))
  }
  resp$items %||% resp$results %||% resp
}


#' List past pipeline runs for an experiment
#'
#' @param experiment_id Pluto experiment ID.
#' @returns The API response (list of pipeline run records).
#' @export
pluto_list_pipeline_runs <- function(experiment_id){
  resp <- pluto_GET(paste0("lab/experiments/", experiment_id, "/pipeline-runs/"))
  if (is.null(resp$items) && is.null(resp$count) && is.null(resp$results)){
    resp$response_status_code <- NULL
    return(unname(resp))
  }
  resp$items %||% resp$results %||% resp
}
