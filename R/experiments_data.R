# EXPERIMENTS DATA ENDPOINTS

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
#' @returns A list containing `df`, the data.frame of the requested data if it was successfully fetched, and `status`, a list containing information from the Pluto API request:\tabular{ll}{
#'    \code{status_code} \tab HTTP status code (e.g. 200, 400, 401) \cr
#'    \tab \cr
#'    \code{code} \tab String, computer-friendly code for response (e.g. `authentication_failed`) \cr
#'    \tab \cr
#'    \code{message} \tab Additional details \cr
#' }
#' @export
pluto_get_experiment_data <- function(experiment_id, table_type, limit = NULL, silent = FALSE){

  page_size <- 10000

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

      if (total_count <= page_size){
        quiet_message(silent,
                      message = c('All ', total_count, ' rows of the ', table_type, ' data were fetched.'))
        final_df <- data_response_to_df(resp_obj)

      } else{
        quiet_message(silent,
                      message = c('Paginating API calls to retrieve all ', total_count,
                      ' rows in the ', table_type, ' data in batches of ',
                      page_size, ' rows...'))

        final_df <- data_response_to_df(resp_obj)


        while(nrow(final_df) < total_count){

          cat('nrow(final_df)\n')
          cat(nrow(final_df))

          quiet_message(silent,
                        message = c('Fetching rows ', nrow(final_df)+1, '...'))

          paginated_url_path <- paste0('https://api.pluto.bio/lab/experiments/',
                        experiment_id, endpoint,
                        '?limit=', page_size,
                        '&offset=', nrow(final_df))
          paginated_req <- httr2::request(paginated_url_path)
          paginated_resp <- paginated_req %>% httr2::req_headers(Authorization = paste0('Token ', api_token)) %>% httr2::req_perform()
          paginated_resp_obj <- httr2::resp_body_json(paginated_resp)

          if (paginated_resp$status_code == 200){
            final_df <- rbind(final_df,
                             data_response_to_df(paginated_resp_obj))

          } else{
            stop(paste0('Response: ', paginated_resp$status_code))
          }

          paginated_resp_obj <- NULL

        }

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
