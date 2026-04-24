# ANALYSES FUNCTIONS

#' List all analyses performed on an experiment in Pluto
#'
#' @description
#' Fetches metadata for all analyses performed on the given experiment
#'
#' @param experiment_id Pluto experiment ID
#' @param limit Max number of analyses to return, default 1000
#' @returns API response object containing `count`, a count of the total analyses,
#' and `items`, an array of analysis objects
#' @export
pluto_get_experiment_analyses <- function(experiment_id, limit = 1000) {
  # Note: this previously hit /plots by mistake; /analyses is the correct
  # endpoint for experiment analyses. /analyses returns a bare JSON array,
  # so we normalize to the {count, items, response_status_code} shape that
  # pluto_read_experiment_analyses and other callers expect.
  url_path <- paste0("lab/experiments/", experiment_id, "/analyses/?limit=", format(limit, scientific = FALSE))
  resp <- pluto_GET(url_path)

  if (is.null(resp$items) && is.null(resp$count)){
    status <- resp$response_status_code
    resp$response_status_code <- NULL
    items <- unname(resp)
    return(list(
      count = length(items),
      items = items,
      response_status_code = status
    ))
  }
  resp
}


#' Get the details of a specific analysis on an experiment
#'
#' @param experiment_id Pluto experiment ID
#' @param analysis_uuid UUID of the analysis
#' @returns The analysis object as a list.
#' @export
pluto_get_analysis <- function(experiment_id, analysis_uuid){
  url_path <- paste0("lab/experiments/", experiment_id, "/analyses/", analysis_uuid, "/")
  pluto_GET(url_path)
}


#' Get the summary results table for an analysis
#'
#' @description
#' Returns the paginated summary data (headers + items) for an analysis.
#' For differential expression, this is the gene-level results table.
#'
#' @param experiment_id Pluto experiment ID
#' @param analysis_uuid UUID of the analysis
#' @param offset Pagination offset, default 0
#' @param limit Pagination limit, default 100
#' @param search Optional search term
#' @param sort_by Optional sort column (prefix with `-` for descending)
#' @returns The full response envelope (`headers`, `count`, `items`, ...).
#' @export
pluto_get_analysis_summary <- function(experiment_id, analysis_uuid,
                                       offset = 0, limit = 100,
                                       search = NULL, sort_by = NULL){
  params <- list(offset = offset, limit = limit)
  if (!is.null(search))  params$search  <- search
  if (!is.null(sort_by)) params$sort_by <- sort_by
  query <- paste0(sapply(seq_along(params), function(i){
    paste0(names(params)[i], "=", utils::URLencode(as.character(params[[i]]), reserved = TRUE))
  }), collapse = "&")
  url_path <- paste0(
    "lab/experiments/", experiment_id,
    "/analyses/", analysis_uuid, "/summary/?", query
  )
  pluto_GET(url_path)
}


#' Get the statistics for an analysis
#' @param experiment_id Pluto experiment ID
#' @param analysis_uuid UUID of the analysis
#' @returns The stats response (typically a list).
#' @export
pluto_get_analysis_stats <- function(experiment_id, analysis_uuid){
  url_path <- paste0(
    "lab/experiments/", experiment_id,
    "/analyses/", analysis_uuid, "/stats/"
  )
  pluto_GET(url_path)
}


#' Get a signed URL for downloading an analysis artifact
#'
#' @param experiment_id Pluto experiment ID
#' @param analysis_uuid UUID of the analysis
#' @param kind One of "spreadsheet" or "image".
#' @returns A short-lived signed URL string.
#' @export
pluto_get_analysis_signed_url <- function(experiment_id, analysis_uuid, kind = c("spreadsheet", "image")){
  kind <- match.arg(kind)
  suffix <- switch(kind,
    spreadsheet = "spreadsheet-analysis-signed-url",
    image = "image-analysis-signed-url"
  )
  url_path <- paste0(
    "lab/experiments/", experiment_id,
    "/analyses/", analysis_uuid, "/", suffix, "/"
  )
  resp <- pluto_GET(url_path)
  if (is.null(resp$url)){
    stop("No signed URL in response")
  }
  resp$url
}


# Terminal states for an analysis's pipeline_status field. Values come from
# pluto-api: lab/analyses/models.py. Note the backend model also declares a
# `status` field but it is not yet populated; `pipeline_status` is the one
# the API actually fills in (observed values on dev: completed, failed, NULL).
.pluto_analysis_terminal <- c("completed", "failed", "unprocessable", "canceled", "cancelled")
.pluto_analysis_success  <- c("completed")


#' Wait for an analysis to reach a terminal state
#'
#' @description
#' Polls the analysis detail endpoint until the `pipeline_status` is one of
#' `completed`, `failed`, `unprocessable`, or `canceled`. Returns the
#' refreshed analysis; check `$pipeline_status == "completed"` to
#' distinguish success from failure. Raises an error only on timeout.
#'
#' @param experiment_id Pluto experiment ID.
#' @param analysis_uuid UUID of the analysis.
#' @param poll_interval Seconds between polls (default 5).
#' @param timeout Give up after this many seconds and raise. Default is one
#'   hour; pass `Inf` to wait indefinitely.
#' @param progress If TRUE, emit a message on each status transition.
#' @returns The latest analysis response object.
#' @export
pluto_wait_for_analysis <- function(experiment_id, analysis_uuid,
                                    poll_interval = 5, timeout = 3600,
                                    progress = FALSE){

  start <- Sys.time()
  last_status <- NA_character_

  repeat {
    analysis <- pluto_get_analysis(experiment_id, analysis_uuid)
    current <- analysis$pipeline_status %||% analysis$status %||% ""
    display <- if (nzchar(current)) current else "unknown"

    if (progress && !identical(current, last_status)){
      message(sprintf("Analysis %s status: %s", analysis_uuid, display))
    }
    last_status <- current

    if (tolower(current) %in% .pluto_analysis_terminal){
      return(analysis)
    }

    elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))
    if (is.finite(timeout) && elapsed >= timeout){
      stop(sprintf(
        "Timed out after %ss waiting for analysis %s (last status: %s)",
        timeout, analysis_uuid, if (nzchar(current)) current else "unknown"
      ))
    }

    Sys.sleep(poll_interval)
  }
}


#' Read Pluto analyses on an experiment into a data frame
#'
#' @description
#' Fetches metadata for all analyses in a given experiment in Pluto and stores them
#' in a data.frame
#'
#' @param experiment_id Pluto experiment ID
#' @returns A data.frame containing columns:\tabular{ll}{
#'    \code{analysis_type} \tab Type of analysis (e.g. `differential_expression` or `gene_set_enrichment`) \cr
#'    \tab \cr
#'    \code{analysis_name} \tab Name of the analysis \cr
#'    \tab \cr
#'    \code{display_type} \tab Active display type (e.g. `volcano_plot` or `score_bar_plot`) \cr
#'    \tab \cr
#'    \code{share_level} \tab Shortname for share level (private, shareable, public) \cr
#'    \tab \cr
#'    \code{status} \tab Shortname for analysis status (draft, pending, pending_complete, complete, failed) \cr
#'    \tab \cr
#'    \code{is_archived} \tab Boolean, whether the analysis was archived and is no longer active \cr
#' }
#' @export
pluto_read_experiment_analyses <- function(experiment_id) {
  analyses_response <- pluto_get_experiment_analyses(experiment_id)
  analyses_count <- analyses_response$count
  analyses_list <- analyses_response$items

  final_df <- data.frame()

  # The /analyses endpoint returns analyses with a flat field layout
  # (uuid, name, pipeline_status, analysis_type, ...). Older callers that
  # were written against the /plots response (nested $analysis, $display)
  # are supported by falling back through %||% below.
  for (i in seq_len(analyses_count)) {
    analysis_display <- analyses_list[[i]]

    # analysis_type may be a bare string or a nested list(shortname=...)
    at <- analysis_display$analysis_type
    if (is.list(at)) at <- at$shortname %||% at$name %||% NA

    analysis_name <- analysis_display$name %||%
      (if (!is.null(analysis_display$analysis)) analysis_display$analysis$name else NA)

    display_type <- if (!is.null(analysis_display$display)) {
      analysis_display$display$display_type %||% NA
    } else NA

    is_archived <- analysis_display$is_archived %||%
      (if (!is.null(analysis_display$display)) analysis_display$display$is_archived else NA)

    df <- data.frame(
      uuid = analysis_display$uuid %||% NA,
      analysis_type = null_to_na(at),
      analysis_name = null_to_na(analysis_name),
      display_type = null_to_na(display_type),
      share_level = null_to_na(analysis_display$share_level),
      status = null_to_na(analysis_display$pipeline_status %||% analysis_display$status),
      is_archived = null_to_na(is_archived),
      stringsAsFactors = FALSE
    )

    final_df <- rbind(final_df, df)
  }

  return(final_df)
}
