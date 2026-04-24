# Programs and program frameworks (translational research).
#
# Mirrors the Python SDK. Programs are top-level containers for
# translational research work; each has a framework (dimensions / next
# steps / coverage gaps / evidence sources) and zero or more analysis
# spaces. Backend endpoints documented in pluto-api: programs/urls.py.
#
# Endpoints not wrapped here (broken on the backend at time of writing —
# see SDK_BACKEND_ISSUES.md):
#   - GET /programs/{uuid}/events/        500 (missing lab_activityevent)
#   - GET /programs/{uuid}/members/       403 with token auth
#   - GET /lab/evidence/...                500 (Django ORM bug)


# ------------------------------------------------------------------ list/get


#' List programs accessible to the current token
#' @param offset Pagination offset, default 0.
#' @param limit Pagination limit, default 100.
#' @param search Optional search term.
#' @returns The DRF paginated response (`count`, `next`, `previous`, `results`).
#' @export
pluto_list_programs <- function(offset = 0, limit = 100, search = NULL){
  qs <- sprintf("?offset=%d&limit=%d", offset, limit)
  if (!is.null(search)){
    qs <- paste0(qs, "&search=", utils::URLencode(search, reserved = TRUE))
  }
  pluto_GET(paste0("programs/", qs))
}


#' Get the detail record for a program
#' @param program_uuid Program UUID.
#' @returns The program record.
#' @export
pluto_get_program <- function(program_uuid){
  pluto_GET(paste0("programs/", program_uuid, "/"))
}


# ----------------------------------------------------------- sub-resources


#' List assets attached to a program
#' @param program_uuid Program UUID.
#' @returns A list of asset records.
#' @export
pluto_list_program_assets <- function(program_uuid){
  .pluto_as_list(pluto_GET(paste0("programs/", program_uuid, "/assets/")))
}


#' List analysis spaces attached to a program
#' @param program_uuid Program UUID.
#' @returns A list of analysis space records.
#' @export
pluto_list_program_analysis_spaces <- function(program_uuid){
  .pluto_as_list(pluto_GET(paste0("programs/", program_uuid, "/analysis-spaces/")))
}


# ----------------------------------------------------------- frameworks


#' Get a program's current framework metadata
#' @param program_uuid Program UUID.
#' @returns The framework record.
#' @export
pluto_get_program_framework <- function(program_uuid){
  pluto_GET(paste0("programs/", program_uuid, "/framework/"))
}


#' Get the program's framework summary (e.g. `{dimensions: [...]}`)
#' @param program_uuid Program UUID.
#' @returns The summary record.
#' @export
pluto_get_program_framework_summary <- function(program_uuid){
  pluto_GET(paste0("programs/", program_uuid, "/framework/summary/"))
}


#' List the framework's version history
#' @param program_uuid Program UUID.
#' @returns A list of version records.
#' @export
pluto_list_program_framework_versions <- function(program_uuid){
  .pluto_as_list(pluto_GET(paste0("programs/", program_uuid, "/framework/versions/")))
}


#' List the framework's coverage gaps
#' @param program_uuid Program UUID.
#' @returns A list of coverage gap records.
#' @export
pluto_list_program_coverage_gaps <- function(program_uuid){
  .pluto_as_list(pluto_GET(paste0("programs/", program_uuid, "/framework/coverage-gaps/")))
}


#' List the framework's recommended next steps
#' @param program_uuid Program UUID.
#' @returns A list of next-step records.
#' @export
pluto_list_program_next_steps <- function(program_uuid){
  .pluto_as_list(pluto_GET(paste0("programs/", program_uuid, "/framework/next-steps/")))
}


#' List the framework's linked evidence sources
#' @param program_uuid Program UUID.
#' @returns A list of evidence source records.
#' @export
pluto_list_program_evidence_sources <- function(program_uuid){
  .pluto_as_list(pluto_GET(paste0("programs/", program_uuid, "/framework/evidence-sources/")))
}


# ------------------------------------------------------------ lookups


#' List the program type taxonomy
#' @returns A list of program-type records.
#' @export
pluto_list_program_types <- function(){
  .pluto_as_list(pluto_GET("programs/program-types/"))
}


#' List available framework templates programs can be created from
#' @returns A list of template records.
#' @export
pluto_list_framework_templates <- function(){
  .pluto_as_list(pluto_GET("programs/framework-templates/"))
}


#' List program labels (categorized tags applied to programs)
#' @returns A list of label records.
#' @export
pluto_list_program_labels <- function(){
  .pluto_as_list(pluto_GET("programs/labels/"))
}


#' Get aggregate program counts (total / active) for the labspace
#' @returns A list with `total` and `active` integer counts.
#' @export
pluto_get_program_stats <- function(){
  pluto_GET("programs/stats/")
}


# ------------------------------------------------------------ history


#' Fetch the activity history for an experiment (cursor-paginated)
#'
#' @description
#' Returns the full envelope (`items`, `next`, `previous`, `next_cursor`,
#' `previous_cursor`). Pass the previous response's `next_cursor` value
#' to walk forward through pages. Page size is fixed at 100 by the
#' backend (no per-call override available).
#'
#' @param experiment_id Pluto experiment ID.
#' @param cursor Optional cursor from a previous response.
#' @returns The paginated envelope.
#' @export
pluto_get_experiment_history <- function(experiment_id, cursor = NULL){
  qs <- ""
  if (!is.null(cursor) && nzchar(cursor)){
    qs <- paste0("?cursor=", utils::URLencode(cursor, reserved = TRUE))
  }
  pluto_GET(paste0("lab/experiments/", experiment_id, "/history/", qs))
}


# ------------------------------------------------------------ helper


# Coerce a response that might be a bare list, a DRF paginated dict, or
# an items envelope into a list of records. Strips the
# `response_status_code` field that pluto_GET attaches when the underlying
# JSON is a bare array (in which case the parsed list ends up as a mix of
# numbered + named elements).
.pluto_as_list <- function(response){
  if (!is.list(response)) return(list())

  # DRF paginated envelope
  if (!is.null(response$results) && is.list(response$results)){
    return(response$results)
  }
  # {items: [...]} envelope
  if (!is.null(response$items) && is.list(response$items)){
    return(response$items)
  }

  # Bare JSON array — pluto_GET appends response_status_code so we get a
  # list whose names are c("", "", ..., "response_status_code"). Strip
  # the status field and any other named elements, return the unnamed
  # records.
  has_named <- !is.null(names(response)) && any(nzchar(names(response)))
  if (has_named){
    keep <- nzchar(names(response)) == FALSE
    return(unname(response[keep]))
  }

  # Truly unnamed list — already in the right shape.
  response
}
