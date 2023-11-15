# ANALYSES FUNCTIONS

#' List all analyses performed on an experiment in Pluto
#'
#' @description
#' Fetches metadata for all analyses performed on the given experiment
#'
#' @param experiment_id Pluto experiment ID
#' @param limit Max number of analyses to return, default 1000
#' @param api_token Optional, otherwise the PLUTO_API_TOKEN environment variable will be used
#' @returns API response object containing `count`, a count of the total analyses,
#' and `items`, an array of analysis objects
#' @export
pluto_list_experiment_analyses <- function(experiment_id, limit = 1000, api_token = NULL){

  if (is.null(api_token)){
    api_token <- Sys.getenv('PLUTO_API_TOKEN')
  }
  validate_auth(api_token)

  url_path <- paste0('https://api.pluto.bio/lab/experiments/', experiment_id,
                      '/plots/?limit=', limit)

  req <- httr2::request(url_path)
  resp <- req %>%
    httr2::req_headers(Authorization = paste0('Token ', api_token)) %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  resp_obj <- httr2::resp_body_json(resp)

  return(resp_obj)

}


#' Read Pluto analyses on an experiment into a data frame
#'
#' @description
#' Fetches metadata for all analyses in a given experiment in Pluto and stores them
#' in a data.frame
#'
#' @param experiment_id Pluto experiment ID
#' @returns A data.frame containing columns:\tabular{ll}{
#'    \code{analysis_type} \tab Type of analysis (e.g. `differential_expression` or `gene_set_enrichment`) \cr
#'    \tab \cr
#'    \code{analysis_name} \tab Name of the analysis \cr
#'    \tab \cr
#'    \code{display_type} \tab Active display type (e.g. `volcano_plot` or `score_bar_plot`) \cr
#'    \tab \cr
#'    \code{share_level} \tab Shortname for share level (private, shareable, public) \cr
#'    \tab \cr
#'    \code{status} \tab Shortname for analysis status (draft, pending, pending_complete, complete, failed) \cr
#'    \tab \cr
#'    \code{is_archived} \tab Boolean, whether the analysis was archived and is no longer active \cr
#' }
#' @export
pluto_read_experiment_analyses <- function(experiment_id){

  analyses_response <- pluto_list_experiment_analyses(experiment_id)
  analyses_count <- analyses_response$count
  analyses_list <- analyses_response$items

  final_df = data.frame()

  for (i in 1:analyses_count){

    # Subset to a few summary columns
    analysis_display <- analyses_list[[i]]

    analysis_name <- ifelse(!is.null(analysis_display$analysis),
                            analysis_display$analysis$name, "")
    display_type <- ifelse(!is.null(analysis_display$display),
                            analysis_display$display$display_type, "")
    is_archived <- ifelse(!is.null(analysis_display$display),
                           analysis_display$display$is_archived, "")

    df <- data.frame(
      analysis_type = analysis_display$analysis_type,
      analysis_name = analysis_name,
      display_type = display_type,
      share_level = analysis_display$share_level,
      status = analysis_display$status,
      is_archived = is_archived
    )

    # Append cleaned row to the final data frame
    final_df <- rbind(final_df, df)
  }

  return(final_df)

}


# Upload a file to Pluto
upload_file <- function(experiment_id, analysis_id, file_path, api_token = NULL) {

  if (is.null(api_token)){
    api_token <- Sys.getenv('PLUTO_API_TOKEN')
  }
  validate_auth(api_token)

  url_path <- paste0("https://api.pluto.bio/lab/experiments/",
                     experiment_id, "/upload-sessions/")

  file_name <- basename(file_path)
  file_ext <- file_ext(file_path)
  file_size <- file.info(file_path)$size

  upload_data <- list(
    analysis_type = "external",
    origin = "R",
    filename = paste0(analysis_id, "--", file_name),
    data_type = "external",
    file_type = file_ext,
    file_size = file_size
  )

  req <- httr2::request(url_path) %>% httr2::req_method("POST") %>%
    httr2::req_headers(Authorization = paste0("Token ", api_token)) %>%
    httr2::req_body_json(upload_data)

  resp <- req %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  resp_obj <- httr2::resp_body_json(resp)
  session_uri <- resp_obj$session_url
  session_uuid <- resp_obj$uuid
  experiment_file <- resp_obj$file

  # Initial PUT request to get uploaded range
  req <- httr2::request(session_uri) %>% httr2::req_method("PUT") %>%
    httr2::req_headers("Content-Length" = "0") %>%
    httr2::req_headers("Content-Range" = "bytes */*")

  resp <- req %>%
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
  req <- httr2::request(session_uri) %>% httr2::req_method("PUT") %>%
    httr2::req_headers("Content-Length" = as.character(length(file_data))) %>%
    httr2::req_headers("Content-Range" = paste0("bytes ", start_byte, "-", total_size-1, "/", total_size)) %>%
    httr2::req_body_raw(file_data)

  resp <- req %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  resp_obj <- httr2::resp_body_json(resp)

  # How I debugged:
  # resp_obj <- httr2::resp_body_string(resp)
  # Invalid request.  According to the Content-Range header, the upload offset is 1 byte(s), which exceeds already uploaded size of 0 byte(s).

  if (resp$status_code %in% c(200, 201)) {
    message("Upload successful!")
    return(list(
      session_uri = session_uri,
      session_uuid = session_uuid,
      experiment_file = experiment_file,
      resp_obj = resp_obj
    ))
  } else {
    stop(paste0("Upload failed with status code: ", resp$status_code))
  }

}


create_analysis <- function(experiment_id, file_path, api_token = NULL){

  if (is.null(api_token)){
    api_token <- Sys.getenv('PLUTO_API_TOKEN')
  }
  validate_auth(api_token)

  url_path <- paste0('https://api.pluto.bio/lab/experiments/',
                     experiment_id, '/analyses/')

  req <- httr2::request(url_path) %>% httr2::req_method("POST") %>%
    httr2::req_headers(Authorization = paste0("Token ", api_token)) %>%
    httr2::req_body_json(list(
      analysis_type = "external",
      origin = "R",
      filename = file_path,
      data_type = "external"
    ))

  resp <- req %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  resp_obj <- httr2::resp_body_json(resp)

  if (resp$status_code == 200){
    return(list(analysis_id = resp_obj$uuid,
                resp_obj = resp_obj))
  } else{
    stop(paste0('Response: ', resp$status_code))
  }

}


create_plot_display <- function(experiment_id, plot_methods, api_token = NULL){

  if (is.null(api_token)){
    api_token <- Sys.getenv('PLUTO_API_TOKEN')
  }
  validate_auth(api_token)

  url_path <- paste0('https://api.pluto.bio/lab/experiments/',
                     experiment_id, '/plots/')

  req <- httr2::request(url_path) %>% httr2::req_method("POST") %>%
    httr2::req_headers(Authorization = paste0("Token ", api_token)) %>%
    httr2::req_body_json(list(
      analysis_type = "external",
      display_type = "html",
      status = "published",
      methods = plot_methods
    ))

  resp <- req %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  resp_obj <- httr2::resp_body_json(resp)

  if (resp$status_code == 200 | resp$status_code == 201){
    return(list(plot_id = resp_obj$uuid,
                display_id = resp_obj$display$uuid,
                resp_obj = resp_obj))
  } else{
    stop(paste0('Response: ', resp$status_code))
  }

}

link_analysis <- function(experiment_id, analysis_id, plot_id, display_id, api_token = NULL){

  if (is.null(api_token)){
    api_token <- Sys.getenv('PLUTO_API_TOKEN')
  }
  validate_auth(api_token)

  url_path <- paste0('https://api.pluto.bio/lab/experiments/',
                     experiment_id, '/plots/', plot_id, '/link-analysis/')

  req <- httr2::request(url_path) %>% httr2::req_method("POST") %>%
    httr2::req_headers(Authorization = paste0("Token ", api_token)) %>%
    httr2::req_body_json(list(
      analysis_id = analysis_id,
      display_id = display_id
    ))

  resp <- req %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  resp_obj <- httr2::resp_body_json(resp)

  if (resp$status_code == 200){

    return(list(analysis_id = resp_obj$analysis$uuid,
                plot_id = resp_obj$plot$uuid,
                display_id = resp_obj$display$uuid,
                resp_obj = resp_obj))
  } else{
    stop(paste0('Response: ', resp$status_code))
  }
}


update_plot_display <- function(experiment_id, analysis_id, display_id, plot_id,
                                plot_methods, file_id, api_token = NULL){

  if (is.null(api_token)){
    api_token <- Sys.getenv('PLUTO_API_TOKEN')
  }
  validate_auth(api_token)

  # Update the display
  url_path <- paste0('https://api.pluto.bio/lab/experiments/',
                     experiment_id, '/plots/', plot_id, '/displays/', display_id, '/')

  req <- httr2::request(url_path) %>% httr2::req_method("PUT") %>%
    httr2::req_headers(Authorization = paste0("Token ", api_token)) %>%
    httr2::req_body_json(list(
      analysis_id = analysis_id,
      display_id = display_id,
      methods = plot_methods,
      display_type = 'external',
      figure_file = file_id
    ))

  resp <- req %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  resp_obj <- httr2::resp_body_json(resp)

  if (resp$status_code == 200){

    # Update the plot
    url_path <- paste0('https://api.pluto.bio/lab/experiments/',
                       experiment_id, '/plots/', plot_id, '/')

    req <- httr2::request(url_path) %>% httr2::req_method("PUT") %>%
      httr2::req_headers(Authorization = paste0("Token ", api_token)) %>%
      httr2::req_body_json(list(
        analysis_id = analysis_id,
        display_id = display_id
      ))

    resp <- req %>%
      httr2::req_error(is_error = function(resp) FALSE) %>%
      httr2::req_perform()

    resp_obj <- httr2::resp_body_json(resp)

    if (resp$status_code == 200){
      return(resp_obj)

    } else{
      stop(paste0('Response: ', resp$status_code))
    }

  } else{
    stop(paste0('Response: ', resp$status_code))
  }

}


update_analysis <- function(experiment_id, analysis_id, analysis_name, api_token = NULL){

  if (is.null(api_token)){
    api_token <- Sys.getenv('PLUTO_API_TOKEN')
  }
  validate_auth(api_token)

  url_path <- paste0('https://api.pluto.bio/lab/experiments/',
                     experiment_id, '/analyses/', analysis_id, '/')

  req <- httr2::request(url_path) %>% httr2::req_method("PUT") %>%
    httr2::req_headers(Authorization = paste0("Token ", api_token)) %>%
    httr2::req_body_json(list(
      name = analysis_name
    ))

  resp <- req %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  resp_obj <- httr2::resp_body_json(resp)

  if (resp$status_code == 200){
    return(resp_obj)
  } else{
    stop(paste0('Response: ', resp$status_code))
  }

}


#' Add an analysis + plot to an experiment in Pluto
#'
#' @description
#' Uploads an HTML plot with optional details to a Pluto experiment
#'
#' @param experiment_id Pluto experiment ID
#' @param file_path Path to the .html file containing a plot to push to Pluto
#' @param analysis_name Character, a title for the new analysis
#' @param plot_methods Character, a description of the methods used for making the plot
#' @export
pluto_add_experiment_plot <- function(experiment_id, file_path, analysis_name = "", plot_methods = ""){

  api_token <- Sys.getenv('PLUTO_API_TOKEN')
  validate_auth(api_token)

  # Create plot & display
  plot_display <- create_plot_display(experiment_id, plot_methods, api_token)

  # Create analysis
  analysis <- create_analysis(experiment_id, file_path, api_token)

  analysis_uuid <- analysis$analysis_id
  plot_uuid <- plot_display$plot_id
  display_uuid <- plot_display$display_id

  # Link analysis & plot
  linked_analysis_plot_display <- link_analysis(experiment_id = experiment_id,
                                                analysis_id = analysis_uuid,
                                                plot_id = plot_uuid,
                                                display_id = display_uuid,
                                                api_token = api_token)

  # Upload plot file to Pluto
  uploaded_plot <- upload_file(experiment_id, analysis_uuid, file_path, api_token)
  file_uuid <- uploaded_plot$experiment_file$uuid

  # Update plot & display
  plot_display2 <- update_plot_display(experiment_id, analysis_uuid, display_uuid, plot_uuid,
                                       plot_methods, file_uuid, api_token)

  # Update analysis
  final_obj <- update_analysis(experiment_id, analysis_uuid, analysis_name, api_token)

}
