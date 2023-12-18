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


#' Pluto internal download handler
#'
#' @description
#' Fetches metadata for all experiments in a given project in Pluto
#'
#' @param url_path URL path (e.g. "lab/projects/?limit=10")
#' @param dest_filename Filename for the downloaded file
#' @param api_token Optional API token, otherwise the PLUTO_API_TOKEN environment variable will be used
#' @returns API response object containing `count`, a count of the total experiments
#' in the project, and `items`, an array of experiments
pluto_GET_download <- function(url_path, dest_filename, api_token=NULL) {

  # Attempt to fetch signed url
  resp_obj <- pluto_GET(url_path, api_token)

  if (!is.null(resp_obj$url)){

    utils::download.file(resp_obj$url, destfile = dest_filename, quiet = T)

  } else{
    stop('Download response did not contain a valid signed URL')
  }
}
