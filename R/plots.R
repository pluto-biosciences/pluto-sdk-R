#' Export a plotly figure to an HTML file
#'
#' @description
#' Exports a plotly figure to an HTML file
#'
#' @param plotly_obj Plotly object
#' @param file_path Path to the .html file to save
#' @export
export_plotly_to_html <- function(plotly_obj, file_path) {
  tryCatch(
    {
      # Check if the plotly object is valid
      if (!inherits(plotly_obj, "plotly")) {
        stop("Invalid plotly object")
      }

      # Save the plotly object to an html file
      htmlwidgets::saveWidget(plotly_obj, file_path, selfcontained = TRUE)
    },
    error = function(e) {
      cat(paste0("Error: ", e$message, "\n"))
    }
  )
}


#' List all plots in an experiment in Pluto
#'
#' @description
#' Fetches metadata for all plots in a given experiment in Pluto
#'
#' @param experiment_id Pluto experiment ID (e.g. PLXP02355)
#' @returns API response object containing `count`, a count of the total plots
#' in the experiment, and `items`, an array of plots
#' @export
pluto_get_experiment_plots <- function(experiment_id) {
  url_path <- paste0("lab/experiments/", experiment_id, "/plots")
  return(pluto_GET(url_path))
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
#' and `response_status_code`, the http response code for the API response
#' @export
pluto_get_plot <- function(experiment_id, plot_id, silent = FALSE) {
  if (is.null(plot_id)) {
    stop("plot_id param must be provided to fetch results")
  } else {
    endpoint <- paste0("/plots/", plot_id)
  }

  url_path <- paste0(
    "lab/experiments/",
    experiment_id, endpoint, "/"
  )

  resp_obj <- pluto_GET(url_path)

  if (resp_obj$response_status_code == 200) {
    plot_title <- ifelse(resp_obj$display$plot_title == "",
      resp_obj$analysis$name,
      resp_obj$display$plot_title
    )

    return(
      list(
        response_status_code = resp_obj$response_status_code,
        plot_details = list(
          analysis_type = resp_obj$analysis_type,
          analysis_status = resp_obj$analysis$pipeline_status,
          analysis_version = resp_obj$analysis$pipeline_version,
          plot_type = resp_obj$display$display_type,
          plot_title = plot_title
        )
      )
    )
  } else {
    return(
      list(
        response_status_code = resp_obj$response_status_code,
        plot_details = NULL
      )
    )
  }
}

#' Create an external plot in Pluto
#'
#' @description
#' Creates a new external plot in a Pluto experiment with optional display, results, and script files
#'
#' @param experiment_id Pluto experiment ID
#' @param analysis_name Name for the plot
#' @param analysis_methods Optional, text methods describing the plot
#' @param display_file_path Optional, path to the display file to upload
#' @param results_file_path Optional, path to the results file to upload
#' @param script_file_path Optional, path to the script file to upload
#' @returns API response for the created plot object
#' @export
pluto_create_external_plot <- function(experiment_id,
                                       analysis_name,
                                       analysis_methods = NULL,
                                       display_file_path = NULL,
                                       results_file_path = NULL,
                                       script_file_path = NULL) {
  # Prepare the request body
  body_data <- list(
    name = analysis_name,
    origin = "R"
  )

  # Add optional methods if provided
  if (!is.null(analysis_methods)) {
    body_data$methods <- analysis_methods
  }

  # Upload files if paths are provided and add file IDs to the request
  if (!is.null(display_file_path)) {
    display_file <- pluto_upload(experiment_id, display_file_path)
    body_data$display_file_id <- display_file$experiment_file$uuid
  }

  if (!is.null(results_file_path)) {
    results_file <- pluto_upload(experiment_id, results_file_path)
    body_data$results_file_id <- results_file$experiment_file$uuid
  }

  if (!is.null(script_file_path)) {
    script_file <- pluto_upload(experiment_id, script_file_path)
    body_data$script_file_id <- script_file$experiment_file$uuid
  }

  # Make the API request
  url_path <- paste0("lab/experiments/", experiment_id, "/external/plots/")
  resp_obj <- pluto_POST(url_path, body_data)

  if (resp_obj$response_status_code %in% c(200, 201)) {
    message("External plot created successfully!")
    return(resp_obj)
  } else {
    stop(paste0("Failed to create external plot. Response: ", resp_obj$response_status_code))
  }
}

#' Update an external plot in Pluto
#'
#' @description
#' Updates an existing external plot in a Pluto experiment with optional name, methods, display, results, and script files
#'
#' @param experiment_id Pluto experiment ID
#' @param plot_id Pluto plot UUID
#' @param name Optional, new name for the plot (can also use analysis_name for backward compatibility)
#' @param methods Optional, text methods describing the plot (can also use plot_methods for backward compatibility)
#' @param display_file_path Optional, path to the new display file to upload
#' @param results_file_path Optional, path to the new results file to upload
#' @param script_file_path Optional, path to the new script file to upload
#' @param analysis_name Deprecated, use name instead. Optional, new name for the plot
#' @param plot_methods Deprecated, use methods instead. Optional, text methods describing the plot
#' @returns API response for the updated plot object
#' @export
pluto_update_external_plot <- function(experiment_id, plot_id, name = NULL, methods = NULL,
                                       display_file_path = NULL,
                                       results_file_path = NULL,
                                       script_file_path = NULL,
                                       analysis_name = NULL,
                                       plot_methods = NULL) {
  # For backward compatibility, use analysis_name if name is NULL
  if (is.null(name) && !is.null(analysis_name)) {
    name <- analysis_name
  }

  # For backward compatibility, use plot_methods if methods is NULL
  if (is.null(methods) && !is.null(plot_methods)) {
    methods <- plot_methods
  }

  # Prepare the request body
  body_data <- list(
    origin = "R"
  )

  # Add optional fields if provided
  if (!is.null(name)) {
    body_data$name <- name
  }

  if (!is.null(methods)) {
    body_data$methods <- methods
  }

  # Upload files if paths are provided and add file IDs to the request
  if (!is.null(display_file_path)) {
    display_file <- pluto_upload(experiment_id, display_file_path)
    body_data$display_file_id <- display_file$experiment_file$uuid
  }

  if (!is.null(results_file_path)) {
    results_file <- pluto_upload(experiment_id, results_file_path)
    body_data$results_file_id <- results_file$experiment_file$uuid
  }

  if (!is.null(script_file_path)) {
    script_file <- pluto_upload(experiment_id, script_file_path)
    body_data$script_file_id <- script_file$experiment_file$uuid
  }

  # Make the API request
  url_path <- paste0("lab/experiments/", experiment_id, "/external/plots/", plot_id, "/")
  resp_obj <- pluto_PUT(url_path, body_data)

  if (resp_obj$response_status_code == 200) {
    message("External plot updated successfully!")
    return(resp_obj)
  } else {
    stop(paste0("Failed to update external plot. Response: ", resp_obj$response_status_code))
  }
}

#' Add an analysis + plot to an experiment in Pluto
#'
#' @description
#' Uploads an HTML or image plot with optional information to a Pluto experiment.
#' Optionally upload tabular results as well.
#'
#' @param experiment_id Pluto experiment ID
#' @param display_file_path Path to the .html or image file containing a plot to push to Pluto
#' @param results_file_path Optional, path to the .csv file containing results to push to Pluto
#' @param script_file_path Optional, path to the script file used to generate the plot
#' @param analysis_name Deprecated, use name instead. String, a title for the new plot
#' @param plot_methods Deprecated, use methods instead. String, a description of the methods used for making the plot
#' @export
pluto_add_experiment_plot <- function(experiment_id,
                                      display_file_path,
                                      results_file_path = NULL,
                                      script_file_path = NULL,
                                      analysis_name = NULL,
                                      analysis_methods = NULL) {

  # Use the new simplified endpoint
  pluto_create_external_plot(
    experiment_id = experiment_id,
    analysis_name = analysis_name,
    analysis_methods = analysis_methods,
    display_file_path = display_file_path,
    results_file_path = results_file_path,
    script_file_path = script_file_path
  )
}
