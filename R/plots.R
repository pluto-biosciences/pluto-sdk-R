#' Export a plotly figure to an HTML file
#'
#' @description
#' Exports a plotly figure to an HTML file
#'
#' @param plotly_obj Plotly object
#' @param file_path Path to the .html file to save
#' @export
export_plotly_to_html <- function(plotly_obj, file_path) {

  tryCatch({
    # Check if the plotly object is valid
    if (!inherits(plotly_obj, "plotly")) {
      stop("Invalid plotly object")
    }

    # Save the plotly object to an html file
    htmlwidgets::saveWidget(plotly_obj, file_path, selfcontained = TRUE)

  }, error = function(e) {
    cat(paste0("Error: ", e$message, "\n"))
  })
}


#' Get information about Pluto plot
#'
#' @description
#' Fetches the details for a given plot in Pluto
#'
#' @param experiment_id Pluto experiment ID
#' @param plot_id Pluto uuid for a plot
#' @param silent Boolean, whether to suppress console messages
#' @returns A list containing `plot_details`, a list of information about the analysis and plot:\tabular{ll}{
#'    \code{analysis_type} \tab Analysis type \cr
#'    \tab \cr
#'    \code{analysis_status} \tab Analysis status \cr
#'    \tab \cr
#'    \code{analysis_version} \tab Analysis version \cr
#'    \tab \cr
#'    \code{plot_type} \tab Plot type \cr
#'    \tab \cr
#'    \code{plot_title} \tab Plot title \cr
#'  }
#'  and `status`, a list containing information from the Pluto API request:\tabular{ll}{
#'    \code{status_code} \tab HTTP status code (e.g. 200, 400, 401) \cr
#'    \tab \cr
#'    \code{code} \tab String, computer-friendly code for response (e.g. `authentication_failed`) \cr
#'    \tab \cr
#'    \code{message} \tab Additional details \cr
#' }
#' @export
pluto_get_plot_details <- function(experiment_id, plot_id, silent = FALSE){

  api_token <- Sys.getenv('PLUTO_API_TOKEN')
  validate_auth(api_token)

  if (is.null(plot_id)){
    stop("plot_id param must be provided to fetch results")

  } else{
    endpoint = paste0('/plots/', plot_id)
  }

  url_path <- paste0('https://api.pluto.bio/lab/experiments/',
                     experiment_id, endpoint)

  req <- httr2::request(url_path)
  resp <- req %>%
    httr2::req_headers(Authorization = paste0('Token ', api_token)) %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  resp_obj <- httr2::resp_body_json(resp)

  if (resp$status_code == 200){

    plot_title <- ifelse(resp_obj$display$plot_title == "",
                         resp_obj$analysis$name,
                         resp_obj$display$plot_title)

    return(
      list(
        status = list(
          status_code = resp$status_code,
          code = resp_obj$code,
          message = resp_obj$message),
        plot_details = list(
          analysis_type = resp_obj$analysis_type,
          analysis_status = resp_obj$analysis$pipeline_status,
          analysis_version = resp_obj$analysis$pipeline_version,
          plot_type = resp_obj$display$display_type,
          plot_title = plot_title
        ))
    )
  } else {
    return(
      list(
        status = list(
          status_code = resp$status_code,
          code = resp_obj$code,
          message = resp_obj$message),
        plot_details = NULL)
    )
  }
}
