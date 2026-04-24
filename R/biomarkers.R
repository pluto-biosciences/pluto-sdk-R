# Biomarker sets and individual biomarkers.
#
# Mirrors the Python SDK's biomarkers module. Backend endpoints:
# - GET /lab/biomarker-sets/                               list
# - POST /lab/biomarker-sets/                              create
# - GET/PATCH /lab/biomarker-sets/{uuid}/                  detail/update
# - POST /lab/biomarker-sets/{uuid}/archive/               archive
# - POST /lab/biomarker-sets/{uuid}/copy/                  copy
# - GET /lab/biomarker-sets/{uuid}/biomarkers/             list biomarkers
# - POST /lab/biomarker-sets/{uuid}/biomarkers/            add one
# - GET/PATCH/DELETE /lab/biomarker-sets/{uuid}/biomarkers/{b_uuid}/
# - POST /lab/biomarker-sets/{uuid}/upload-csv/            bulk upload
# - GET /lab/biomarker-sets/{uuid}/download/               export as CSV


#' List biomarker sets accessible to the current token
#'
#' @param offset Pagination offset, default 0.
#' @param limit Pagination limit, default 100.
#' @param search Optional search term.
#' @returns API response with `count` + `items`.
#' @export
pluto_list_biomarker_sets <- function(offset = 0, limit = 100, search = NULL){
  qs <- sprintf("?offset=%d&limit=%d", offset, limit)
  if (!is.null(search)){
    qs <- paste0(qs, "&search=", utils::URLencode(search, reserved = TRUE))
  }
  pluto_GET(paste0("lab/biomarker-sets/", qs))
}


#' Get the detail record for a biomarker set
#' @param set_uuid Biomarker set UUID.
#' @returns The biomarker set record.
#' @export
pluto_get_biomarker_set <- function(set_uuid){
  pluto_GET(paste0("lab/biomarker-sets/", set_uuid, "/"))
}


#' Create a new biomarker set
#'
#' @description
#' Required `data` keys: `name`, `target_type` (one of `gene`, `protein`,
#' `metabolite`, `other`), `organism_shortname` (e.g. `human`, `mouse`).
#' Optional: `description`, `set_type`.
#'
#' @param data A list of fields for the new set.
#' @returns The newly-created biomarker set record.
#' @export
pluto_create_biomarker_set <- function(data){
  pluto_POST("lab/biomarker-sets/", data)
}


#' Archive a biomarker set
#' @param set_uuid Biomarker set UUID.
#' @returns The API response.
#' @export
pluto_archive_biomarker_set <- function(set_uuid){
  pluto_POST(paste0("lab/biomarker-sets/", set_uuid, "/archive/"), list())
}


#' List biomarkers contained in a set (paginated)
#' @param set_uuid Biomarker set UUID.
#' @param offset Pagination offset, default 0.
#' @param limit Pagination limit, default 100.
#' @param search Optional search term.
#' @returns API response with `count` + `items`.
#' @export
pluto_list_biomarkers <- function(set_uuid, offset = 0, limit = 100, search = NULL){
  qs <- sprintf("?offset=%d&limit=%d", offset, limit)
  if (!is.null(search)){
    qs <- paste0(qs, "&search=", utils::URLencode(search, reserved = TRUE))
  }
  pluto_GET(paste0("lab/biomarker-sets/", set_uuid, "/biomarkers/", qs))
}


#' Add a single biomarker to a set
#' @param set_uuid Biomarker set UUID.
#' @param data Biomarker fields.
#' @returns The created biomarker record.
#' @export
pluto_add_biomarker <- function(set_uuid, data){
  pluto_POST(paste0("lab/biomarker-sets/", set_uuid, "/biomarkers/"), data)
}


#' Download a biomarker set as CSV to a local path
#' @param set_uuid Biomarker set UUID.
#' @param dest_filename Destination path on disk.
#' @returns The destination path invisibly.
#' @export
pluto_download_biomarker_set <- function(set_uuid, dest_filename){
  resp <- pluto_GET(paste0("lab/biomarker-sets/", set_uuid, "/download/"))
  if (is.null(resp$url)){
    rlang::abort(
      sprintf("Expected signed URL for biomarker set %s", set_uuid),
      class = c("pluto_api_error", "pluto_error")
    )
  }
  utils::download.file(resp$url, destfile = dest_filename, quiet = TRUE, mode = "wb")
  invisible(dest_filename)
}
