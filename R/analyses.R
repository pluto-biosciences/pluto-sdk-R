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
