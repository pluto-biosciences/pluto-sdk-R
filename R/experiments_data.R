# EXPERIMENTS DATA ENDPOINTS

pluto_get_experiment_data_paginated <- function(experiment_id, table_type, limit = NULL,
                                                silent = FALSE){

  api_token <- Sys.getenv('PLUTO_API_TOKEN')
  validate_auth(api_token)

  page_size <- 10000

  if (table_type == 'sample'){
    endpoint <- '/sample-data/'

  } else if (table_type == 'assay'){
    endpoint <- '/assay-data/'

  } else{
    stop("Unsupported table_type Supported data table types are 'sample', 'assay'.")
  }

  if (!is.null(limit)){
    url_path <- paste0('https://api.pluto.bio/lab/experiments/',
                       experiment_id, endpoint, '?limit=', limit)
  } else{
    url_path <- paste0('https://api.pluto.bio/lab/experiments/',
                       experiment_id, endpoint, '?limit=', page_size)
  }

  req <- httr2::request(url_path)
  resp <- req %>%
    httr2::req_headers(Authorization = paste0('Token ', api_token)) %>%
    # The line below is required to override httr2's default behavior of
    # automatically converting HTTP errors into R errors
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  # Preview JSON response
  #resp %>% resp_raw()

  resp_obj <- httr2::resp_body_json(resp)

  if (resp$status_code == 200){

    # Calculate whether we need to paginate
    total_count <- resp_obj$count

    if (!is.null(limit)){

      if (total_count <= limit){
        quiet_message(silent,
                      message = c('All ', total_count, ' rows of the ', table_type, ' data were fetched.'))
      } else{
        quiet_message(silent,
                      message = c('Note: Data has ', total_count, ' rows but only ', limit,
                                  ' will be fetched due to the provided "limit" parameter.',
                                  '\nIncrease or remove the "limit" parameter if more rows are desired.'))
      }

      final_df <- data_response_to_df(resp_obj)

    } else{

      final_df <- data_response_to_df(resp_obj)

      if (total_count <= page_size){
        quiet_message(silent,
                      message = c('All ', total_count, ' rows of the ', table_type, ' data were fetched.'))

      } else{
        quiet_message(silent,
                      message = c('Paginating API calls to retrieve all ', total_count,
                                  ' rows in the ', table_type, ' data in batches of ',
                                  page_size, ' rows...'))

        # Parallelized requests
        offsets <- seq(page_size, total_count, by = page_size)

        paginated_url_paths <- paste0('https://api.pluto.bio/lab/experiments/',
                                      experiment_id, endpoint,
                                      '?limit=', page_size,
                                      '&offset=', offsets)
        reqs <- lapply(paginated_url_paths, function(u){
          httr2::request(u) %>%
            httr2::req_headers(Authorization = paste0('Token ', api_token))
        })

        resps <- httr2::multi_req_perform(reqs)

        paginated_resp_objs <- c()

        for (paginated_resp in resps){

          if (paginated_resp$status_code == 200){
            paginated_resp_objs <- c(paginated_resp_objs, httr2::resp_body_json(paginated_resp))

          } else{
            stop(paste0('Response: ', paginated_resp$status_code))
          }
        }

        final_df <- rbind(final_df,
                          do.call(data_response_to_df, list(paginated_resp_objs)))

      }
    }

    if (table_type == 'sample'){
      names(final_df) <- tolower(names(final_df))
    }
    if (table_type == 'assay'){
      names(final_df)[1] <- tolower(names(final_df)[1])
    }
    return(
      list(
        status = list(
          status_code = resp$status_code,
          code = resp_obj$code,
          message = resp_obj$message),
        df = final_df)
    )

  } else {
    return(
      list(
        status = list(
          status_code = resp$status_code,
          code = resp_obj$code,
          message = resp_obj$message),
        df = NULL)
    )
  }

}

#' Get sample or assay data tables from Pluto
#'
#' @description
#' Fetches the sample or assay data table for a given experiment in Pluto and
#' returns metadata from the API request as well as the data itself
#'
#' @param experiment_id Pluto experiment ID
#' @param table_type Table type, choices are "sample" or "assay"
#' @param limit Integer for max rows to fetch, or NULL to fetch all rows
#' @param silent Boolean, whether to suppress console messages
#' @param paginated Boolean, whether to make paginated request instead of downloading whole table, default FALSE
#' @returns A list containing `df`, the data.frame of the requested data if it was successfully fetched, and `status`, a list containing information from the Pluto API request:\tabular{ll}{
#'    \code{status_code} \tab HTTP status code (e.g. 200, 400, 401) \cr
#'    \tab \cr
#'    \code{code} \tab String, computer-friendly code for response (e.g. `authentication_failed`) \cr
#'    \tab \cr
#'    \code{message} \tab Additional details \cr
#' }
#' @importFrom utils read.csv
#' @export
pluto_get_experiment_data <- function(experiment_id, table_type, limit = NULL,
                                      silent = FALSE, paginated = FALSE){

  api_token <- Sys.getenv('PLUTO_API_TOKEN')
  validate_auth(api_token)

  if (!is.null(limit)){
    paginated <- TRUE
  }

  if (!paginated){

    # If not paginating, download the whole file and read the CSV into a data.frame
    temp_filename <- tempfile()
    pluto_download_data(experiment_id, table_type, dest_filename = temp_filename)
    final_df <- read.csv(temp_filename, stringsAsFactors = F)
    file.remove(temp_filename)

    return(
      list(
        status = list(
          status_code = 200,
          code = NULL,
          message = ''),
        df = final_df)
    )

  } else{
    pluto_get_experiment_data_paginated(experiment_id, table_type, limit, silent)
  }
}


#' Read Pluto sample or assay data table into a data frame
#'
#' @description
#' Fetches the sample or assay data table for a given experiment in Pluto and
#' stores it in a data.frame
#'
#' @param experiment_id Pluto experiment ID
#' @param table_type Table type, choices are "sample" or "assay"
#' @param limit Integer for max rows to fetch, or NULL to fetch all rows
#' @param silent Boolean, whether to suppress console messages
#' @returns Data.frame containing the requested data
#' @export
pluto_read_data <- function(experiment_id, table_type, limit = NULL, silent = FALSE){
  data_obj <- pluto_get_experiment_data(experiment_id, table_type, limit, silent)
  return(data_obj$df)
}


#' Read Pluto sample data table into a data frame
#'
#' @description
#' Fetches the sample data table for a given experiment in Pluto and stores it
#' in a data.frame
#'
#' @param experiment_id Pluto experiment ID
#' @param limit Integer for max rows to fetch, or NULL to fetch all rows
#' @param silent Boolean, whether to suppress console messages
#' @returns Data.frame containing the requested data
#' @export
pluto_read_sample_data <- function(experiment_id, limit = NULL, silent = FALSE){
  return(pluto_read_data(experiment_id, table_type = "sample", limit, silent))
}


#' Read Pluto assay data table into a data frame
#'
#' @description
#' Fetches the assay data table for a given experiment in Pluto and stores it
#' in a data.frame
#'
#' @param experiment_id Pluto experiment ID
#' @param limit Integer for max rows to fetch, or NULL to fetch all rows
#' @param silent Boolean, whether to suppress console messages
#' @returns Data.frame containing the requested data
#' @export
pluto_read_assay_data <- function(experiment_id, limit = NULL, silent = FALSE){
  return(pluto_read_data(experiment_id, table_type = "assay", limit, silent))
}
