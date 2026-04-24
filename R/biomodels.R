# Biomodel sets (structured reference datasets with dynamic columns).


#' List biomodel sets accessible to the current token
#' @returns A list of biomodel set records (bare list).
#' @export
pluto_list_biomodel_sets <- function(){
  resp <- pluto_GET("lab/biomodel-sets/")
  # /biomodel-sets/ returns a bare JSON array. Strip response_status_code so
  # callers get a clean list.
  if (is.null(resp$items) && is.null(resp$count)){
    status <- resp$response_status_code
    resp$response_status_code <- NULL
    return(unname(resp))
  }
  resp$items %||% resp$results %||% list()
}


#' Get the detail record for a biomodel set
#' @param set_uuid Biomodel set UUID.
#' @returns The biomodel set record.
#' @export
pluto_get_biomodel_set <- function(set_uuid){
  pluto_GET(paste0("lab/biomodel-sets/", set_uuid, "/"))
}


#' Create a new biomodel set
#' @param data Set fields — typically `name` plus `schema_definition`.
#' @returns The newly-created biomodel set record.
#' @export
pluto_create_biomodel_set <- function(data){
  pluto_POST("lab/biomodel-sets/", data)
}


#' Archive a biomodel set
#' @param set_uuid Biomodel set UUID.
#' @returns The API response.
#' @export
pluto_archive_biomodel_set <- function(set_uuid){
  pluto_GET(paste0("lab/biomodel-sets/", set_uuid, "/archive/"))
}


#' List rows in a biomodel set
#' @description
#' Returns the full `{headers, count, items}` envelope so callers can use the
#' schema `headers` to drive data.frame construction.
#' @param set_uuid Biomodel set UUID.
#' @param offset Pagination offset, default 0.
#' @param limit Pagination limit, default 100.
#' @param search Optional search term.
#' @returns The full response envelope.
#' @export
pluto_list_biomodels <- function(set_uuid, offset = 0, limit = 100, search = NULL){
  qs <- sprintf("?offset=%d&limit=%d", offset, limit)
  if (!is.null(search)){
    qs <- paste0(qs, "&search=", utils::URLencode(search, reserved = TRUE))
  }
  pluto_GET(paste0("lab/biomodel-sets/", set_uuid, "/biomodels/", qs))
}


#' Get the column schema for a biomodel set
#' @param set_uuid Biomodel set UUID.
#' @returns The column definitions (list).
#' @export
pluto_get_biomodel_columns <- function(set_uuid){
  pluto_GET(paste0("lab/biomodel-sets/", set_uuid, "/columns/"))
}
