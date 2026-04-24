# Single-cell preprocessing Workflows.
#
# Mirrors the Python SDK's Workflow class. Each workflow moves through
# the states in_progress -> completed -> accepted (see pluto-api:
# lab/models/workflows.py).
#
# Note: these endpoints previously required JWT auth only. pluto-api
# PR #3454 adds AccessTokenAuthentication to the workflow + preprocess
# views; these helpers assume that fix is deployed. If you see 401
# responses, the backend may not have the patch yet.


# Terminal states. in_progress = non-terminal; the rest end the wait.
.pluto_workflow_terminal <- c("completed", "accepted", "failed")
.pluto_workflow_success  <- c("completed", "accepted")


#' List workflows for an experiment
#'
#' @param experiment_id Pluto experiment ID.
#' @returns A list of workflow records.
#' @export
pluto_list_workflows <- function(experiment_id){
  resp <- pluto_GET(
    paste0("lab/experiments/", experiment_id, "/workflows/"),
    strict = TRUE
  )
  .pluto_workflow_response_to_list(resp)
}


#' Get a workflow's detail record
#'
#' @param experiment_id Pluto experiment ID.
#' @param workflow_uuid Workflow UUID.
#' @returns The workflow record.
#' @export
pluto_get_workflow <- function(experiment_id, workflow_uuid){
  pluto_GET(paste0(
    "lab/experiments/", experiment_id,
    "/workflows/", workflow_uuid, "/"
  ))
}


#' Wait for a workflow to reach a terminal status
#'
#' @description
#' Polls the workflow detail endpoint until `status` is one of
#' `completed`, `accepted`, or `failed`. Does not raise on workflow
#' failure — check `$status` after return. Raises only on timeout
#' expiry.
#'
#' @param experiment_id Pluto experiment ID.
#' @param workflow_uuid Workflow UUID.
#' @param poll_interval Seconds between polls (default 10).
#' @param timeout Maximum seconds to wait. Defaults to 3 hours (Nextflow
#'   pipelines are slow); pass `Inf` to wait indefinitely.
#' @param progress If TRUE, emit a message on each status transition.
#' @returns The workflow record at the last poll.
#' @export
pluto_wait_for_workflow <- function(experiment_id, workflow_uuid,
                                    poll_interval = 10,
                                    timeout = 3 * 60 * 60,
                                    progress = FALSE){
  start <- Sys.time()
  last_status <- NA_character_

  repeat {
    wf <- pluto_get_workflow(experiment_id, workflow_uuid)
    current <- wf$status %||% ""

    if (progress && !identical(current, last_status)){
      message(sprintf("Workflow %s status: %s", workflow_uuid,
                      if (nzchar(current)) current else "unknown"))
    }
    last_status <- current

    if (tolower(current) %in% .pluto_workflow_terminal){
      return(wf)
    }

    elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))
    if (is.finite(timeout) && elapsed >= timeout){
      stop(sprintf(
        "Timed out after %ss waiting for workflow %s (last status: %s)",
        timeout, workflow_uuid, if (nzchar(current)) current else "unknown"
      ))
    }
    Sys.sleep(poll_interval)
  }
}


#' Accept a workflow (promotes the selected preprocess to "accepted")
#' @param experiment_id Pluto experiment ID.
#' @param workflow_uuid Workflow UUID.
#' @returns The API response.
#' @export
pluto_accept_workflow <- function(experiment_id, workflow_uuid){
  pluto_POST(
    paste0("lab/experiments/", experiment_id,
           "/workflows/", workflow_uuid, "/accept/"),
    list()
  )
}


#' Archive a workflow (soft-delete)
#' @param experiment_id Pluto experiment ID.
#' @param workflow_uuid Workflow UUID.
#' @returns The API response.
#' @export
pluto_archive_workflow <- function(experiment_id, workflow_uuid){
  pluto_POST(
    paste0("lab/experiments/", experiment_id,
           "/workflows/", workflow_uuid, "/archive/"),
    list()
  )
}


#' Copy a workflow
#' @param experiment_id Pluto experiment ID.
#' @param workflow_uuid Workflow UUID.
#' @param data Optional list of fields to pass in the POST body (e.g. a
#'   new name and/or `copy_to_preprocess_id`).
#' @returns The newly-created workflow record.
#' @export
pluto_copy_workflow <- function(experiment_id, workflow_uuid, data = list()){
  pluto_POST(
    paste0("lab/experiments/", experiment_id,
           "/workflows/", workflow_uuid, "/copy/"),
    data
  )
}


#' List preprocesses attached to a workflow
#' @param experiment_id Pluto experiment ID.
#' @param workflow_uuid Workflow UUID.
#' @returns A list of preprocess records.
#' @export
pluto_list_workflow_preprocesses <- function(experiment_id, workflow_uuid){
  resp <- pluto_GET(
    paste0("lab/experiments/", experiment_id,
           "/workflows/", workflow_uuid, "/preprocesses/"),
    strict = TRUE
  )
  .pluto_workflow_response_to_list(resp)
}


# Coerce a successful workflow-style response into a list of records.
# The workflow endpoints return a bare JSON array; pluto_GET attaches a
# `response_status_code` named element. Strict mode guarantees we only
# enter this function on a 2xx response, so it's safe to treat all
# named elements as envelope metadata and return only the unnamed
# records. If the response somehow looks like an envelope
# ({items}/{results}), handle that too.
.pluto_workflow_response_to_list <- function(resp){
  if (!is.list(resp)) return(list())
  if (!is.null(resp$results) && is.list(resp$results)) return(resp$results)
  if (!is.null(resp$items) && is.list(resp$items))   return(resp$items)

  named <- !is.null(names(resp)) & nzchar(names(resp))
  if (any(named)){
    unname(resp[!named])
  } else {
    resp
  }
}
