# EXPERIMENTS DATA ENDPOINTS

pluto_get_experiment_data_paginated <- function(experiment_id, table_type,
                                                limit = NULL, silent = FALSE){

  page_size <- 10000

  if (table_type == 'sample'){
    endpoint <- '/sample-data/'

  } else if (table_type == 'assay'){
    endpoint <- '/assay-data/'

  } else{
    stop("Unsupported table_type Supported data table types are 'sample', 'assay'.")
  }

  if (!is.null(limit)){
    url_path <- paste0('lab/experiments/',
                       experiment_id, endpoint, '?limit=', format(limit, scientific=F))
  } else{
    url_path <- paste0('lab/experiments/',
                       experiment_id, endpoint, '?limit=', page_size)
  }

  resp_obj <- pluto_GET(url_path)

  if (resp_obj$response_status_code == 200){

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
        if (is.null(api_token)){
          api_token <- Sys.getenv('PLUTO_API_TOKEN')
        }
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
        status_code = resp_obj$response_status_code,
        df = final_df)
    )

  } else {
    return(
      list(
        status_code = resp_obj$response_status_code,
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
    final_df <- read.csv(temp_filename, stringsAsFactors = FALSE)
    file.remove(temp_filename)

    return(
      list(
        status_code = 200,
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


pluto_download_data <- function(experiment_id, table_type, dest_filename = NULL){

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

  url_path <- paste0("lab/experiments/", experiment_id,
                     endpoint, "download/?filename=", dest_filename)

  pluto_download(url_path, dest_filename)

}


#' Download Pluto sample table
#'
#' @description
#' Fetches the sample table for a given experiment and plot in Pluto and saves
#' it as a CSV file
#'
#' @param experiment_id Pluto experiment ID
#' @param dest_filename Destination filename for CSV file (e.g. "PLX12345_sample_data.csv")
#' @returns Saves the downloaded data to `dest_filename`
#' @export
pluto_download_sample_data <- function(experiment_id, dest_filename = NULL){
  pluto_download_data(experiment_id, table_type = "sample", dest_filename)
}


#' Download Pluto assay table
#'
#' @description
#' Fetches the assay table for a given experiment and plot in Pluto and saves
#' it as a CSV file
#'
#' @param experiment_id Pluto experiment ID
#' @param dest_filename Destination filename for CSV file (e.g. "PLX12345_assay_data.csv")
#' @returns Saves the downloaded data to `dest_filename`
#' @export
pluto_download_assay_data <- function(experiment_id, dest_filename = NULL){
  pluto_download_data(experiment_id, table_type = "assay", dest_filename)
}


#' Get Seurat objects from Pluto
#'
#' @description
#' Fetches the raw or final Seurat object for a given experiment in Pluto and
#' returns metadata from the API request as well as the object itself
#'
#' @param experiment_id Pluto experiment ID
#' @param seurat_type Seurat type, choices are "raw" or "final"
#' @param silent Boolean, whether to suppress console messages
#' @returns A list containing `obj`, the requested Seurat object if it was successfully fetched, and `status`, a list containing information from the Pluto API request:\tabular{ll}{
#'    \code{status_code} \tab HTTP status code (e.g. 200, 400, 401) \cr
#'    \tab \cr
#'    \code{code} \tab String, computer-friendly code for response (e.g. `authentication_failed`) \cr
#'    \tab \cr
#'    \code{message} \tab Additional details \cr
#' }
#' @importFrom utils read.csv
#' @export
pluto_get_seurat_object <- function(experiment_id, seurat_type, silent = FALSE){

  api_token <- Sys.getenv('PLUTO_API_TOKEN')
  validate_auth(api_token)

  temp_filename <- tempfile()
  pluto_download_seurat_object(experiment_id, seurat_type, dest_filename = temp_filename)
  closeAllConnections()
  final_obj <- readRDS(temp_filename)
  file.remove(temp_filename)

  return(
    list(
      status_code = 200,
      obj = final_obj)
  )

}


#' Get cluster annotation sets from Pluto
#'
#' @description
#' Fetches the cluster annotation sets for a given experiment in Pluto and
#' returns metadata from the API request as well as the data itself
#'
#' @param experiment_id Pluto experiment ID
#' @param silent Boolean, whether to suppress console messages
#' @returns A list containing `obj`, the requested data if it was successfully fetched, and `status`, a list containing information from the Pluto API request:\tabular{ll}{
#'    \code{status_code} \tab HTTP status code (e.g. 200, 400, 401) \cr
#'    \tab \cr
#'    \code{code} \tab String, computer-friendly code for response (e.g. `authentication_failed`) \cr
#'    \tab \cr
#'    \code{message} \tab Additional details \cr
#' }
#' @importFrom utils read.csv
#' @export
pluto_get_cluster_annotation_sets <- function(experiment_id, silent = FALSE){

  endpoint <- "/annotation-sets"
  url_path <- paste0("lab/experiments/", experiment_id, endpoint)
  final_obj <- pluto_GET(url_path)

  return(
    list(
      status_code = 200,
      obj = final_obj[1:length(final_obj)-1])
  )

}


#' Add cluster annotations to Seurat object
#'
#' @description
#' Adds cluster annotations from JSON-like response object to Seurat object `meta.data` and
#' also creates a list of custom cluster color palettes
#'
#' @param seurat_obj Seurat object
#' @param cluster_obj Cluster annotation set JSON-like response object
#' @param silent Boolean, whether to suppress console messages
#' @returns A list containing `obj`, the Seurat object with added cluster annotations, and `colors`, a list containing custom cluster color palettes
#' @export
add_cluster_annotations <- function(seurat_obj, cluster_obj, silent = FALSE){

  # Establish list for custom cluster color palettes
  custom_colors <- list()

  # Convert Seurat meta.data rownames to column
  seurat_obj@meta.data <- tibble::rownames_to_column(seurat_obj@meta.data, "cells")

  # Loop through all annotation sets
  for (i in 1:length(cluster_obj)) {

    # Get target resolution for annotation set
    target_res <- paste0("res_", cluster_obj[[i]]$resolution)
    if (!grepl("\\.", target_res)) {
      target_res <- paste0(target_res, ".0")
    }

    # Get display name for annotation set
    display_name <- cluster_obj[[i]]$display_name

    # Clean up annotation set display name
    clean_display_name <- tolower(gsub(" ", "_", display_name))
    clean_display_name <- gsub("[[:punct:]]+", '_', clean_display_name)

    # Get map to add custom annotation set to Seurat meta.data
    cluster_number <- c()
    cluster_display_name <- c()
    cluster_color <- c()
    for (c in 1:length(cluster_obj[[i]]$clusters)) {
      cluster_number <- c(cluster_number, cluster_obj[[i]]$clusters[[c]]$number)
      cluster_display_name <- c(cluster_display_name, cluster_obj[[i]]$clusters[[c]]$display_name)
      cluster_color <- c(cluster_color, cluster_obj[[i]]$clusters[[c]]$color)
    }
    annotation_set <- data.frame(
      factor(cluster_number),
      factor(cluster_display_name),
      cluster_color
    )
    colnames(annotation_set) <- c(target_res, clean_display_name, "cluster_color")
    annotation_map <- annotation_set[c(target_res, clean_display_name)]

    # Add custom annotation set to Seurat meta.data
    seurat_obj@meta.data <- suppressMessages(
      seurat_obj@meta.data %>%
      dplyr::left_join(annotation_map)
    )

    # Add custom cluster colors to custom_colors object
    custom_colors[[clean_display_name]] <- annotation_set[[3]] # cluster_color
    names(custom_colors[[clean_display_name]]) <- annotation_set[[2]] # cluster_display_name

  }

  # Convert Seurat meta.data "cells" column back to rownames
  seurat_obj@meta.data <- seurat_obj@meta.data %>% tibble::column_to_rownames("cells")

  return(
    list(
      obj = seurat_obj,
      colors = custom_colors
    )
  )

}


#' Read Seurat object from Pluto
#'
#' @description
#' Fetches the raw or final Seurat object for a given experiment in Pluto and
#' automatically adds cluster annotation sets to Seurat `meta.data` if `seurat_type=final`
#'
#' @param experiment_id Pluto experiment ID
#' @param seurat_type Seurat type, choices are "raw" or "final"
#' @param silent Boolean, whether to suppress console messages
#' @returns Requested Seurat object; if `seurat_type=final`, also returns custom cluster color palettes
#' @export
pluto_read_seurat_object <- function(experiment_id, seurat_type, silent = FALSE){

  seurat_obj <- pluto_get_seurat_object(experiment_id, seurat_type, silent)

  if (seurat_type == "final"){
    cluster_obj <- pluto_get_cluster_annotation_sets(experiment_id, silent)
    final_obj <- add_cluster_annotations(seurat_obj$obj, cluster_obj$obj, silent)
    return(final_obj)
  } else {
    return(seurat_obj$obj)
  }

}


#' Download Seurat object from Pluto
#'
#' @description
#' Fetches the raw or final Seurat object for a given experiment in Pluto and saves
#' it as an RDS file
#'
#' @param experiment_id Pluto experiment ID
#' @param seurat_type Seurat type, choices are "raw" or "final"
#' @param silent Boolean, whether to suppress console messages
#' @param dest_filename Destination filename for RDS file (e.g. "PLX12345_final_seurat_object.rds")
#' @returns Saves the downloaded Seurat object to `dest_filename`
#' @export
pluto_download_seurat_object <- function(experiment_id, seurat_type,
                                         silent = FALSE, dest_filename = NULL){

  if (is.null(dest_filename)){
    dest_filename <- paste0(experiment_id, "_", seurat_type, "_seurat_object.rds")
  }

  endpoint <- '/files/?data_type=seurat'
  url_path_get <- paste0("lab/experiments/", experiment_id, endpoint)

  seurat_resp <- pluto_GET(url_path_get)
  if (seurat_type == "raw"){
    seurat_uuid <- seurat_resp$seurat$items[[1]]$uuid
  } else if (seurat_type == "final"){
    seurat_uuid <- seurat_resp$seurat$items[[2]]$uuid
  } else{
    stop("Unsupported seurat_type Supported Seurat object types are 'raw', 'final'.")
  }
  url_path_post <- paste0("lab/experiments/", experiment_id,
                          "/files/", seurat_uuid, "/download/")

  pluto_download(url_path_post, dest_filename)

}

