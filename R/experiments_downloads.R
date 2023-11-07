#' @importFrom utils download.file
pluto_download_data <- function(experiment_id, table_type, dest_filename = NULL){

  api_token <- Sys.getenv('PLUTO_API_TOKEN')
  if (!is_valid_api_key(api_token)){
    stop("Invalid API token. Check your PLUTO_API_TOKEN environment variable.")
  }

  if (table_type == 'sample'){
    endpoint <- '/sample-data/'

  } else if (table_type == 'assay'){
    endpoint <- '/assay-data/'

  } else{
    stop("Unsupported table_type Supported data table types are 'sample', 'assay'.")
  }

  if (is.null(dest_filename)){
    dest_filename <- paste0(experiment_id, "_", table_type, "_data.csv")
  }

  url_path <- paste0("https://api.pluto.bio/lab/experiments/", experiment_id,
                     endpoint, "download/?filename=", dest_filename)

  req <- httr2::request(url_path)
  resp <- req %>%
    httr2::req_headers(Authorization = paste0('Token ', api_token)) %>%
    # The line below is required to override httr2's default behavior of
    # automatically converting HTTP errors into R errors
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  if (resp$status_code == 200){

    resp_obj <- httr2::resp_body_json(resp)
    utils::download.file(resp_obj$url, destfile = dest_filename, quiet = T)

  } else{
    stop(paste0('Response: ', resp$status_code))
  }

}

pluto_download_sample_data <- function(experiment_id, dest_filename = NULL){
  pluto_download_data(experiment_id, table_type = "sample", dest_filename)
}

pluto_download_assay_data <- function(experiment_id, dest_filename = NULL){
  pluto_download_data(experiment_id, table_type = "assay", dest_filename)
}

#' Read Pluto sample data table into a data frame
#'
#' @description
#' Fetches the sample data table for a given experiment in Pluto and stores it
#' in a data.frame
#'
#' @param experiment_id Pluto experiment ID
#' @returns Data.frame containing the requested data
#' @importFrom utils read.csv
#' @export
pluto_read_sample <- function(experiment_id){
  tmpfile <- tempfile()
  pluto_download_sample_data(experiment_id, dest_filename = tmpfile)
  df <- read.csv(tmpfile, stringsAsFactors = F)
  file.remove(tmpfile)
  return(df)
}

#' Read Pluto assay data table into a data frame
#'
#' @description
#' Fetches the assay data table for a given experiment in Pluto and stores it
#' in a data.frame
#'
#' @param experiment_id Pluto experiment ID
#' @returns Data.frame containing the requested data
#' @importFrom utils read.csv
#' @export
pluto_read_assay <- function(experiment_id){
  tmpfile <- tempfile()
  pluto_download_assay_data(experiment_id, dest_filename = tmpfile)
  df <- read.csv(tmpfile, stringsAsFactors = F)
  file.remove(tmpfile)
  return(df)
}
