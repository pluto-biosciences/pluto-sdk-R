# Base functions for interfacing with the Pluto API
#
# Historically these functions returned the parsed JSON body plus an extra
# `$response_status_code` field and never threw on non-2xx. That behavior is
# preserved (existing callers continue to work unchanged), but each HTTP
# helper now also accepts `strict = TRUE` to route failures through
# `pluto_check_response()` and raise a typed `rlang` condition. Retries on
# 429/5xx and explicit timeouts apply automatically.


# Default (connect + read) timeout in seconds. Researchers can override with
# the PLUTO_TIMEOUT env var or the `timeout` argument on any HTTP helper.
.pluto_default_timeout <- 60

# Default number of retries on 429/5xx responses. Override via
# PLUTO_MAX_RETRIES or the `max_retries` arg.
.pluto_default_max_retries <- 3

# HTTP status codes that should trigger an httr2 retry.
.pluto_retry_on_status <- c(429L, 500L, 502L, 503L, 504L)


base_url <- function(){
  if(Sys.getenv("PLUTO_ENV") == "staging"){
    return("https://staging-api.pluto.bio/")
  } else if(Sys.getenv("PLUTO_ENV") == "development"){
    return("https://dev-api.pluto.bio/")
  } else{
    return("https://api.pluto.bio/")
  }
}


.resolve_timeout <- function(timeout){
  if (!is.null(timeout)) return(timeout)
  env <- Sys.getenv("PLUTO_TIMEOUT", unset = "")
  if (!identical(env, "")){
    parsed <- suppressWarnings(as.numeric(env))
    if (!is.na(parsed) && parsed > 0) return(parsed)
  }
  .pluto_default_timeout
}


.resolve_max_retries <- function(max_retries){
  if (!is.null(max_retries)) return(max(0L, as.integer(max_retries)))
  env <- Sys.getenv("PLUTO_MAX_RETRIES", unset = "")
  if (!identical(env, "")){
    parsed <- suppressWarnings(as.integer(env))
    if (!is.na(parsed) && parsed >= 0) return(parsed)
  }
  .pluto_default_max_retries
}


# Build a pre-configured httr2 request that already has auth, timeout, and
# retry behavior applied. Shared by every HTTP helper below so we only
# maintain one place.
.pluto_request <- function(url_path, api_token, timeout, max_retries){

  if (is.null(api_token)){
    api_token <- Sys.getenv('PLUTO_API_TOKEN')
  }
  validate_auth(api_token)

  timeout <- .resolve_timeout(timeout)
  max_retries <- .resolve_max_retries(max_retries)

  req <- httr2::request(paste0(base_url(), url_path)) %>%
    httr2::req_headers(Authorization = paste0('Token ', api_token)) %>%
    httr2::req_timeout(timeout) %>%
    httr2::req_error(is_error = function(resp) FALSE)

  if (max_retries > 0){
    req <- req %>%
      httr2::req_retry(
        max_tries = max_retries + 1L,  # max_tries counts the initial attempt
        backoff = function(attempt) min(30, 2^(attempt - 1)),
        is_transient = function(resp) {
          isTRUE(httr2::resp_status(resp) %in% .pluto_retry_on_status)
        }
      )
  }

  req
}


# Turn a finished response into the legacy "parsed body + status_code" list,
# tolerating empty bodies (e.g. 204 No Content).
.pluto_finish <- function(resp, strict = FALSE){

  status <- httr2::resp_status(resp)
  body_size <- tryCatch(
    length(httr2::resp_body_raw(resp)),
    error = function(e) 0
  )

  if (body_size == 0){
    resp_obj <- list()
  } else {
    resp_obj <- tryCatch(
      httr2::resp_body_json(resp),
      error = function(e) list()
    )
  }

  resp_obj$response_status_code <- status

  if (isTRUE(strict)){
    pluto_check_response(resp_obj)
  }

  resp_obj
}


#' Pluto internal GET function
#'
#' @description
#' Makes a GET request to the Pluto API. Applies automatic retries on
#' 429/5xx and a default 60s timeout.
#'
#' @param url_path URL path (e.g. "lab/projects/?limit=10")
#' @param api_token Optional API token, otherwise the PLUTO_API_TOKEN environment variable will be used
#' @param strict If TRUE, raise a classed `rlang` condition on non-2xx instead of
#'   returning the response list. See [pluto_check_response()].
#' @param timeout Request timeout in seconds. Defaults to 60; override via
#'   the `PLUTO_TIMEOUT` env var.
#' @param max_retries Max retries on 429/5xx. Defaults to 3; override via
#'   the `PLUTO_MAX_RETRIES` env var.
#' @returns API response object containing `count`, a count of the total experiments
#' in the project, and `items`, an array of experiments
pluto_GET <- function(url_path, api_token = NULL, strict = FALSE, timeout = NULL, max_retries = NULL) {

  req <- .pluto_request(url_path, api_token, timeout, max_retries) %>%
    httr2::req_method("GET")

  resp <- req %>% httr2::req_perform()
  .pluto_finish(resp, strict = strict)
}


#' Pluto internal POST function
#'
#' @description
#' Makes a POST request to the Pluto API. POST is NOT retried automatically
#' (to avoid accidentally creating resources twice) — only 429 triggers a
#' retry because the server is explicitly asking us to back off.
#'
#' @param url_path URL path (e.g. "lab/projects/?limit=10")
#' @param body_data Data to be included in body
#' @param api_token Optional API token, otherwise the PLUTO_API_TOKEN environment variable will be used
#' @param strict If TRUE, raise a classed `rlang` condition on non-2xx.
#' @param timeout Request timeout in seconds (default 60).
#' @param max_retries Max retries on 429 only (default 3).
#' @returns API response object
#' @keywords internal
pluto_POST <- function(url_path, body_data, api_token = NULL, strict = FALSE, timeout = NULL, max_retries = NULL) {

  req <- .pluto_request(url_path, api_token, timeout, max_retries) %>%
    httr2::req_method("POST") %>%
    httr2::req_body_json(body_data)

  # For POST we only retry on 429 (the server is asking us to back off); other
  # failures may have created a resource and should not be retried blindly.
  req$policies$retry_is_transient <- function(resp) {
    isTRUE(httr2::resp_status(resp) == 429L)
  }

  resp <- req %>% httr2::req_perform()
  .pluto_finish(resp, strict = strict)
}


#' Pluto internal PUT function
#' @param url_path URL path
#' @param body_data Data to be included in body
#' @param api_token Optional API token
#' @param strict If TRUE, raise a classed condition on non-2xx.
#' @param timeout Request timeout in seconds.
#' @param max_retries Max retries on 429/5xx.
#' @returns API response object
#' @keywords internal
pluto_PUT <- function(url_path, body_data, api_token = NULL, strict = FALSE, timeout = NULL, max_retries = NULL) {

  req <- .pluto_request(url_path, api_token, timeout, max_retries) %>%
    httr2::req_method("PUT") %>%
    httr2::req_body_json(body_data)

  resp <- req %>% httr2::req_perform()
  .pluto_finish(resp, strict = strict)
}


#' Pluto internal PATCH function
#' @param url_path URL path
#' @param body_data Data to be included in body
#' @param api_token Optional API token
#' @param strict If TRUE, raise a classed condition on non-2xx.
#' @param timeout Request timeout in seconds.
#' @param max_retries Max retries on 429/5xx.
#' @returns API response object
#' @keywords internal
pluto_PATCH <- function(url_path, body_data, api_token = NULL, strict = FALSE, timeout = NULL, max_retries = NULL) {

  req <- .pluto_request(url_path, api_token, timeout, max_retries) %>%
    httr2::req_method("PATCH") %>%
    httr2::req_body_json(body_data)

  resp <- req %>% httr2::req_perform()
  .pluto_finish(resp, strict = strict)
}


#' Pluto internal DELETE function
#' @param url_path URL path
#' @param api_token Optional API token
#' @param strict If TRUE, raise a classed condition on non-2xx.
#' @param timeout Request timeout in seconds.
#' @param max_retries Max retries on 429/5xx.
#' @returns API response object (may be empty for 204 No Content)
#' @keywords internal
pluto_DELETE <- function(url_path, api_token = NULL, strict = FALSE, timeout = NULL, max_retries = NULL) {

  req <- .pluto_request(url_path, api_token, timeout, max_retries) %>%
    httr2::req_method("DELETE")

  resp <- req %>% httr2::req_perform()
  .pluto_finish(resp, strict = strict)
}


#' Pluto internal download handler
#'
#' @description
#' Makes a GET request to the Pluto API and downloads a file via signed url
#'
#' @param url_path URL path (e.g. "lab/projects/?limit=10")
#' @param dest_filename Filename for the downloaded file
#' @param api_token Optional API token, otherwise the PLUTO_API_TOKEN environment variable will be used
#' @importFrom utils download.file
pluto_download <- function(url_path, dest_filename, api_token=NULL) {

  # Attempt to fetch signed url
  resp_obj <- pluto_GET(url_path, api_token)

  if (!is.null(resp_obj$url)){

    utils::download.file(resp_obj$url, destfile = dest_filename, quiet = T, mode = "wb")

  } else{
    stop('Download response did not contain a valid signed URL')
  }
}


#' Pluto internal upload handler
#'
#' @description
#' Makes a POST request to the Pluto API to begin an upload session then makes a
#' PUT request to upload a file
#'
#' @param experiment_id URL path (e.g. "lab/projects/?limit=10")
#' @param file_path Filename of the uploaded file
#' @returns API response object containing `count`, a count of the total experiments
#' in the project, and `items`, an array of experiments
pluto_upload <- function(experiment_id, file_path) {

  url_path <- paste0("lab/experiments/", experiment_id, "/upload-sessions/")

  file_name <- gsub('\\s', '_', basename(file_path))
  file_ext <- file_ext(file_path)
  file_size <- file.info(file_path)$size

  body_data <- list(
    analysis_type = "external",
    origin = "R",
    filename = paste0(experiment_id, "--", file_name),
    data_type = "external",
    file_type = file_ext,
    file_size = file_size
  )

  resp_obj <- pluto_POST(url_path, body_data)
  session_uri <- resp_obj$session_url
  session_uuid <- resp_obj$uuid
  experiment_file <- resp_obj$file

  # Initial PUT request to get uploaded range
  put_req1 <- httr2::request(session_uri) %>% httr2::req_method("PUT") %>%
    httr2::req_headers("Content-Length" = "0") %>%
    httr2::req_headers("Content-Range" = "bytes */*")

  resp1 <- put_req1 %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  start_byte <- 0

  # Read file data starting from start_byte
  con <- file(file_path, "rb")
  seek(con, start_byte)
  file_data <- readBin(con, "raw", file_size - start_byte)
  close(con)

  # Final PUT request to upload the file
  total_size <- file_size
  put_req2 <- httr2::request(session_uri) %>% httr2::req_method("PUT") %>%
    httr2::req_headers("Content-Length" = as.character(length(file_data))) %>%
    httr2::req_headers("Content-Range" = paste0("bytes ", start_byte, "-", total_size-1, "/", total_size)) %>%
    httr2::req_body_raw(file_data)

  resp2 <- put_req2 %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  upload_resp_obj <- httr2::resp_body_json(resp2)

  if (resp2$status_code %in% c(200, 201)) {
    message("Upload successful!")
    return(list(
      session_uri = session_uri,
      session_uuid = session_uuid,
      experiment_file = experiment_file,
      resp_obj = resp_obj
    ))
  } else {
    stop(paste0("Upload failed with status code: ", resp2$status_code))
  }
}
