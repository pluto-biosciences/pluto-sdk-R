# EXPERIMENTS RESULTS ENDPOINTS

#' Fetch results for a plot
#'
#' @description
#' Fetches the results for a given experiment plot in Pluto
#'
#' @param experiment_id Pluto experiment ID
#' @param plot_id Pluto uuid for a plot
#' @param limit Max number of rows to return, default 100k
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
#' and `response_status_code`, the http response code for the API response
#' @export
pluto_get_results <- function(experiment_id, plot_id, limit = 100000, silent = FALSE){

  url_path <- paste0('lab/experiments/', experiment_id,
                     '/plots/', plot_id, '/data/?limit=',
                     format(limit, scientific=F))

  # Get plot details to determine analysis and plot type
  plot_details <- pluto_get_plot(experiment_id, plot_id, silent = T)
  transformer_type <- ifelse(plot_details$plot_details$plot_type %in% PLOT_DATA_FORMAT_ARROW,
                             "arrow", "data")

  if (!plot_details$response_status_code == 200){
    stop(paste0('Error fetching plot data. Response: ', plot_details$response_status_code))
  }

  resp_obj <- pluto_GET(url_path)

  if (resp_obj$response_status_code == 200){

    final_df <- json_to_df_transfomer(resp_obj, transformer_type)
    final_df <- as.data.frame(lapply(final_df, unlist))

    return(
      list(
        response_status_code = resp_obj$response_status_code,
        df = final_df)
    )

  } else {
    return(
      list(
        response_status_code = resp_obj$response_status_code,
        df = NULL)
    )
  }
}

#' Read Pluto results data table into a data frame
#'
#' @description
#' Fetches the results table for a given analysis in Pluto and stores it
#' in a data.frame
#'
#' @param experiment_id Pluto experiment ID
#' @param plot_id Pluto uuid for a plot
#' @param silent Boolean, whether to suppress console messages
#' @returns Data.frame containing the requested results
#' @export
pluto_read_results <- function(experiment_id, plot_id, silent = FALSE){
  return(pluto_get_results(experiment_id = experiment_id,
                           plot_id = plot_id, silent = silent)$df)
}

#' Download Pluto results table
#'
#' @description
#' Fetches the results table for a given experiment and plot in Pluto and saves
#' it as a CSV file
#'
#' @param experiment_id Pluto experiment ID
#' @param plot_id Pluto plot ID
#' @param dest_filename Destination filename for CSV file (e.g. "PLX12345_deg_table.csv")
#' @returns Saves the downloaded data to `dest_filename`
#' @export
pluto_download_results <- function(experiment_id, plot_id, dest_filename = NULL){

  plot_details <- pluto_get_plot(experiment_id, plot_id)

  if (is.null(dest_filename)){
    dest_filename <- paste0(experiment_id, "_", plot_details$plot_details$plot_type, ".csv")
  }

  url_path <- paste0("lab/experiments/", experiment_id, "/plots/",
                     plot_id, "/download/?filename=", dest_filename)

  pluto_download(url_path, dest_filename)

}
