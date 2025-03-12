# ANALYSES FUNCTIONS

#' List all analyses performed on an experiment in Pluto
#'
#' @description
#' Fetches metadata for all analyses performed on the given experiment
#'
#' @param experiment_id Pluto experiment ID
#' @param limit Max number of analyses to return, default 1000
#' @returns API response object containing `count`, a count of the total analyses,
#' and `items`, an array of analysis objects
#' @export
pluto_get_experiment_analyses <- function(experiment_id, limit = 1000) {
  url_path <- paste0("lab/experiments/", experiment_id, "/plots/?limit=", format(limit, scientific = F))
  return(pluto_GET(url_path))
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
pluto_read_experiment_analyses <- function(experiment_id) {
  analyses_response <- pluto_get_experiment_analyses(experiment_id)
  analyses_count <- analyses_response$count
  analyses_list <- analyses_response$items

  final_df <- data.frame()

  for (i in 1:analyses_count) {
    # Subset to a few summary columns
    analysis_display <- analyses_list[[i]]

    analysis_name <- ifelse(!is.null(analysis_display$analysis),
      analysis_display$analysis$name, ""
    )
    display_type <- ifelse(!is.null(analysis_display$display),
      analysis_display$display$display_type, ""
    )
    is_archived <- ifelse(!is.null(analysis_display$display),
      analysis_display$display$is_archived, ""
    )

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


#' Internal create new external analysis
#'
#' @description
#' Creates a new external analysis on an experiment using the upload session UUID.
#'
#' @param experiment_id Pluto experiment ID
#' @param upload_session_uuid The upload session UUID to use as the display_file_id
#' @param analysis_name Name for the analysis (default "External")
#' @param methods Optional methods describing the analysis
#' @returns API response for the created analysis object containing `uuid`,
#' `analysis_type`, `name`, `results`, and `response_status_code`
create_analysis <- function(experiment_id, upload_session_uuid, analysis_name = "External", methods = "") {
  url_path <- paste0("lab/experiments/", experiment_id, "/analyses/")

  body_data <- list(
    analysis_type = "external",
    name = analysis_name,
    origin = "web",
    display_file_id = upload_session_uuid,
    results_file_id = "",
    script_file_id = "",
    methods = methods
  )

  resp_obj <- pluto_POST(url_path, body_data)

  if (resp_obj$response_status_code == 200) {
    return(resp_obj)
  } else {
    stop(paste0("Response: ", resp_obj$response_status_code))
  }
}


#' Internal create new external plot display
#'
#' @description
#' Creates a new external plot display on an experiment
#'
#' @param experiment_id Pluto experiment ID
#' @returns API response for the created plot object containing plot shell `uuid`,
#' `display$uuid`, and `response_status_code`
create_plot_shell <- function(experiment_id) {
  url_path <- paste0("lab/experiments/", experiment_id, "/plots/")

  body_data <- list(
    analysis_type = "external",
    status = "published"
  )

  resp_obj <- pluto_POST(url_path, body_data)

  if (resp_obj$response_status_code == 201) {
    return(resp_obj)
  } else {
    stop(paste0("Response: ", resp_obj$response_status_code))
  }
}


#' Internal link plot shell with an analysis & display
#'
#' @description
#' Links a plot shell to a created analysis object and display object
#'
#' @param experiment_id Pluto experiment ID
#' @param analysis_id Pluto analysis uuid
#' @param plot_id Pluto plot shell uuid
#' @param display_id Pluto display uuid
#' @returns API response for the linked plot object containing the plot shell,
#' analysis, and display fields
link_analysis <- function(experiment_id, analysis_id, plot_id, display_id) {
  url_path <- paste0(
    "lab/experiments/", experiment_id,
    "/plots/", plot_id, "/link-analysis/"
  )

  body_data <- list(
    analysis_id = analysis_id,
    display_id = display_id
  )

  resp_obj <- pluto_POST(url_path, body_data)

  if (resp_obj$response_status_code == 200 | resp_obj$response_status_code == 201) {
    return(list(
      analysis_id = resp_obj$analysis$uuid,
      plot_id = resp_obj$plot$uuid,
      display_id = resp_obj$display$uuid,
      plot_obj = resp_obj
    ))
  } else {
    stop(paste0("Response: ", resp_obj$response_status_code))
  }
}


#' Internal update display
#'
#' @description
#' Updates a display with an HTML/image file and corresponding methods
#'
#' @param experiment_id Pluto experiment ID
#' @param analysis_id Pluto analysis uuid
#' @param plot_id Pluto plot shell uuid
#' @param display_id Pluto display uuid
#' @param display_methods Optional, text methods describing the plot
#' @param uploaded_file Optional, experiment file object to upload to the display
#' @returns API response for the linked plot object containing the plot shell,
#' analysis, and display fields
update_plot_display <- function(experiment_id, analysis_id, plot_id, display_id,
                                display_methods = NULL, uploaded_file = NULL) {
  # Add the methods to the display
  url_path <- paste0(
    "lab/experiments/", experiment_id,
    "/plots/", plot_id, "/displays/", display_id, "/"
  )

  body_data <- list(
    analysis_id = analysis_id,
    display_id = display_id,
    display_type = "external"
  )

  # Add optional fields
  if (!is.null(display_methods)) {
    body_data$methods <- display_methods
  }
  if (!is.null(uploaded_file)) {
    body_data$figure_file <- uploaded_file
  }

  display_resp_obj <- pluto_PUT(url_path, body_data)

  if (display_resp_obj$response_status_code == 200) {
    # Add the analysis to the plot shell
    url_path <- paste0(
      "lab/experiments/", experiment_id,
      "/plots/", plot_id, "/"
    )

    body_data <- list(
      analysis_id = analysis_id,
      display_id = display_id
    )

    plot_resp_obj <- pluto_PUT(url_path, body_data)

    if (plot_resp_obj$response_status_code == 200) {
      return(plot_resp_obj)
    } else {
      stop(paste0("Response: ", plot_resp_obj$response_status_code))
    }
  } else {
    stop(paste0("Response: ", display_resp_obj$response_status_code))
  }
}


#' Internal update analysis
#'
#' @description
#' Updates an analysis with name
#'
#' @param experiment_id Pluto experiment ID
#' @param analysis_id Pluto analysis uuid
#' @param analysis_name Optional, text methods describing the plot
#' @param analysis_methods TODO accept methods on analysis
#' @param results TODO upload results file
#' @returns API response for the created analysis object containing `uuid`,
#' `analysis_type`, `name`, `results`, and `response_status_code`
update_analysis <- function(experiment_id, analysis_id, analysis_name = NULL,
                            analysis_methods = NULL, results = NULL) {
  url_path <- paste0(
    "lab/experiments/", experiment_id,
    "/analyses/", analysis_id, "/"
  )

  body_data <- list()

  # Add optional fields
  if (!is.null(analysis_name)) {
    body_data$name <- analysis_name
  }
  if (!is.null(analysis_methods)) {
    body_data$methods <- analysis_methods
  }

  resp_obj <- pluto_PUT(url_path, body_data)

  if (resp_obj$response_status_code == 200) {
    return(resp_obj)
  } else {
    stop(paste0("Response: ", resp_obj$response_status_code))
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
#' @param analysis_name String, a title for the new analysis
#' @param plot_methods String, a description of the methods used for making the plot
#' @export
pluto_add_experiment_plot <- function(experiment_id, display_file_path, results_file_path = NULL,
                                      analysis_name = NULL, plot_methods = NULL) {
  # Upload plot file to Pluto and obtain the upload session UUID
  uploaded_plot <- pluto_upload(experiment_id, display_file_path)
  upload_session_uuid <- uploaded_plot$upload_session_uuid

  # Create external analysis using the upload session UUID
  analysis <- create_analysis(
    experiment_id,
    upload_session_uuid,
    analysis_name = ifelse(is.null(analysis_name), "External", analysis_name),
    methods = plot_methods
  )

  return(analysis)
}
