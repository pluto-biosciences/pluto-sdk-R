# EXPERIMENTS ENDPOINTS

#' List all experiments available in Pluto
#'
#' @description
#' Fetches metadata for all experiments the requester has permission to view
#'
#' @param limit Max number of experiments to return, default 1000
#' @param api_token Optional, otherwise the PLUTO_API_TOKEN environment variable will be used
#' @returns API response object containing `count`, a count of the total experiments
#' and `items`, an array of experiment objects
#' @export
pluto_list_experiments <- function(limit = 1000, api_token = NULL){

  if (is.null(api_token)){
    api_token <- Sys.getenv('PLUTO_API_TOKEN')
  }
  validate_auth(api_token)

  url_path <- paste0('https://api.pluto.bio/lab/experiments/?limit=', limit)

  req <- httr2::request(url_path)
  resp <- req %>%
    httr2::req_headers(Authorization = paste0('Token ', api_token)) %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  resp_obj <- httr2::resp_body_json(resp)

  return(resp_obj)
}
