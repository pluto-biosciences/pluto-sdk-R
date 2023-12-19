# Base functions for interfacing with the Pluto API

base_url <- function(){
  if (!Sys.getenv("PLUTO_ENV") == "staging"){
    Sys.setenv(PLUTO_ENV="production")
  }

  if (Sys.getenv("PLUTO_ENV") == "production"){
    return(Sys.getenv("PLUTO_API_PROD"))
  } else{
    return(Sys.getenv("PLUTO_API_STAGING"))
  }
}


#' Pluto internal GET function
#'
#' @description
#' Makes a GET request to the Pluto API
#'
#' @param url_path URL path (e.g. "lab/projects/?limit=10")
#' @param api_token Optional API token, otherwise the PLUTO_API_TOKEN environment variable will be used
#' @returns API response object containing `count`, a count of the total experiments
#' in the project, and `items`, an array of experiments
pluto_GET <- function(url_path, api_token = NULL) {

  # Check API token
  if (is.null(api_token)){
    api_token <- Sys.getenv('PLUTO_API_TOKEN')
  }
  validate_auth(api_token)

  # GET request
  req <- httr2::request(paste0(base_url(), url_path)) %>%
    httr2::req_method("GET") %>%
    httr2::req_headers(Authorization = paste0('Token ', api_token)) %>%
    httr2::req_error(is_error = function(resp) FALSE)

  # Response
  resp <- req %>% httr2::req_perform()
  resp_obj <- httr2::resp_body_json(resp)
  resp_obj$response_status_code <- resp$status_code

  return(resp_obj)
}


#' Pluto internal POST function
#'
#' @description
#' Makes a POST request to the Pluto API
#'
#' @param url_path URL path (e.g. "lab/projects/?limit=10")
#' @param body_data Data to be included in body
#' @param api_token Optional API token, otherwise the PLUTO_API_TOKEN environment variable will be used
#' @returns API response object containing `count`, a count of the total experiments
#' in the project, and `items`, an array of experiments
#' @keywords internal
pluto_POST <- function(url_path, body_data, api_token = NULL) {

  # Check API token
  if (is.null(api_token)){
    api_token <- Sys.getenv('PLUTO_API_TOKEN')
  }
  validate_auth(api_token)

  # POST request
  req <- httr2::request(paste0(base_url(), url_path)) %>%
    httr2::req_method("POST") %>%
    httr2::req_headers(Authorization = paste0('Token ', api_token)) %>%
    httr2::req_body_json(body_data) %>%
    httr2::req_error(is_error = function(resp) FALSE)

  # Response
  resp <- req %>% httr2::req_perform()
  resp_obj <- httr2::resp_body_json(resp)
  resp_obj$response_status_code <- resp$status_code

  return(resp_obj)
}


#' Pluto internal PUT function
#'
#' @description
#' Makes a PUT request to the Pluto API
#'
#' @param url_path URL path (e.g. "lab/projects/?limit=10")
#' @param body_data Data to be included in body
#' @param api_token Optional API token, otherwise the PLUTO_API_TOKEN environment variable will be used
#' @returns API response object containing `count`, a count of the total experiments
#' in the project, and `items`, an array of experiments
#' @keywords internal
pluto_PUT <- function(url_path, body_data, api_token = NULL) {

  # Check API token
  if (is.null(api_token)){
    api_token <- Sys.getenv('PLUTO_API_TOKEN')
  }
  validate_auth(api_token)

  # PUT request
  req <- httr2::request(paste0(base_url(), url_path)) %>%
    httr2::req_method("PUT") %>%
    httr2::req_headers(Authorization = paste0('Token ', api_token)) %>%
    httr2::req_body_json(body_data) %>%
    httr2::req_error(is_error = function(resp) FALSE)

  # Response
  resp <- req %>% httr2::req_perform()
  resp_obj <- httr2::resp_body_json(resp)
  resp_obj$response_status_code <- resp$status_code

  return(resp_obj)
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

    utils::download.file(resp_obj$url, destfile = dest_filename, quiet = T)

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

  # How I debugged:
  # resp_obj <- httr2::resp_body_string(resp2)
  # Invalid request.  According to the Content-Range header, the upload offset is 1 byte(s), which exceeds already uploaded size of 0 byte(s).

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
