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
create_analysis <- function(experiment_id, uploaded_display_uuid, uploaded_results_uuid = NULL, uploaded_script_uuid = NULL, analysis_name = "External", methods = NULL) {

  # create the plot shell
  plot_shell <- create_plot_shell(experiment_id)
  plot_id = plot_shell$uuid
  display_id = plot_shell$display$uuid
  print("plot_shell created")

  # create plot shell // https://api.pluto.bio/lab/experiments/c627c313-5e8c-46a3-812f-3241da7816f4/v2/plots/
  # analysis_type: "external"
  # display_type: "external"
  #status: "published"

  # already updload - uuid's passed in

  # upload files (or do that ahead of time)
  # ensure it completes
  # data_type: "external"
  # https://api.pluto.bio/lab/experiments/c627c313-5e8c-46a3-812f-3241da7816f4/upload-sessions/80f7bbab-7692-4de7-bf68-01f6660a991b/complete/

  url_path <- paste0("lab/experiments/", experiment_id, "/analyses/")

  body_data <- list(
    analysis_type = "external",
    name = analysis_name,
    origin = "R",
    display_file_id = uploaded_display_uuid,
    results_file_id = uploaded_results_uuid,
    script_file_id = uploaded_script_uuid,
    methods=methods
  )


  # Log the request details
  cat("Creating analysis with the following parameters:\n")
  cat("URL: ", paste0(base_url(), url_path), "\n")

  resp_obj <- pluto_POST(url_path, body_data)
  print(resp_obj)

  # POST analysis
  # https://api.pluto.bio/lab/experiments/c627c313-5e8c-46a3-812f-3241da7816f4/analyses/
  # analysis_type: external
  # name: External
  # origin: web
  # display_file_id: ee59ce8f-7761-4fd4-a3e5-ddb79d6f42b5
  # results_file_id:
  # script_file_id:
  # methods:

  analysis_id = resp_obj$uuid

  # update the plot with the analysis
  updated_plot_response = update_plot(experiment_id, plot_id, analysis_id)

  # Update Plot
  # https://api.pluto.bio/lab/experiments/c627c313-5e8c-46a3-812f-3241da7816f4/plots/b34dab0d-478c-4225-819b-1b1aebd32744/
  # analysis_id: "e7e4b5b3-5860-48ac-aa48-87209be72133"

  linked_analysis_response <- link_analysis(experiment_id, analysis_id, plot_id, display_id)
  print(linked_analysis_response)

  # Link analysis
  # https://api.pluto.bio/lab/experiments/c627c313-5e8c-46a3-812f-3241da7816f4/plots/b34dab0d-478c-4225-819b-1b1aebd32744/link-analysis/
  # analysis_id: "e7e4b5b3-5860-48ac-aa48-87209be72133"
  # display_id: "a3e62729-73ff-4b43-bce1-bd5a58041ee6"


  # Log the response
  cat("\nAPI Response:\n")
  print(resp_obj)

  # Log specific fields that might be useful for debugging
  cat("\nResponse Status Code: ", resp_obj$response_status_code, "\n")

  if (!is.null(resp_obj$detail)) {
    cat("Error Detail: ", resp_obj$detail, "\n")
  }

  if (!is.null(resp_obj$message)) {
    cat("Error Message: ", resp_obj$message, "\n")
  }

  if (!is.null(resp_obj$code)) {
    cat("Error Code: ", resp_obj$code, "\n")
  }

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



#' Internal update external plot display
#'
#' @description
#' Updates a external plot display on an experiment
#'
#' @param experiment_id Pluto experiment ID
#' @param plot_id Pluto plot ID
#' @param analysis_id Pluto analysis ID
#' @returns API response for the updated plot object containing
update_plot <- function(experiment_id, plot_id, analysis_id) {
  url_path <- paste0("lab/experiments/", experiment_id, "/plots/", plot_id)

  body_data <- list(
    analysis_id = analysis_id
  )

  resp_obj <- pluto_PUT(url_path, body_data)

  if (resp_obj$response_status_code == 200) {
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
pluto_add_experiment_plot <- function(experiment_id, display_file_path, results_file_path = NULL, script_file_path = NULL,
                                      analysis_name = NULL, plot_methods = NULL) {

  # Upload plot file to Pluto and obtain the upload session UUID
  uploaded_display <- pluto_upload(experiment_id, display_file_path)
  uploaded_display_uuid <- uploaded_display$experiment_file_uuid

  uploaded_results_uuid = NULL
  if (!is.null(results_file_path)) {
    uploaded_results <- pluto_upload(experiment_id, results_file_path)
    uploaded_results_uuid <- uploaded_results$experiment_file_uuid
  }

  uploaded_script_uuid = NULL
  if (!is.null(script_file_path)) {
    uploaded_script <- pluto_upload(experiment_id, script_file_path)
    uploaded_script_uuid <- uploaded_script$experiment_file_uuid
  }

  analysis_name_updated = analysis_name
  if (is.null(analysis_name)) {
    analysis_name_updated = "External"
  }

  # Create external analysis using the upload session UUID
  analysis <- create_analysis(
    experiment_id = experiment_id,
    uploaded_display_uuid = uploaded_display_uuid,
    uploaded_results_uuid = uploaded_results_uuid,
    uploaded_script_uuid = uploaded_script_uuid,
    analysis_name = analysis_name_updated,
    methods = plot_methods
  )

  return(analysis)
}
