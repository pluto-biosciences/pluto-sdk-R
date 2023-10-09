# PLOTS ENDPOINTS

library(httr)
library(jsonlite)

# Validate API key
is_valid_api_key <- function(api_key) {
    # Implement your validation logic here
    return(TRUE)
}

# Upload a file to Pluto
upload_file <- function(experiment_id, analysis_id, file_path) {
    api_token <- Sys.getenv("PLUTO_API_TOKEN")
    if (!is_valid_api_key(api_token)) {
        stop("Invalid API token. Check your PLUTO_API_TOKEN environment variable.")
    }

    base_url <- "https://api.pluto.bio/lab/experiments"
    upload_session_url <- paste0(base_url, "/", experiment_id, "/upload-sessions")

    file_name <- basename(file_path)
    file_ext <- tools::file_ext(file_path)
    file_size <- file.info(file_path)$size

    upload_data <- list(
        analysis_type = "external",
        origin = "R",
        filename = paste0(analysis_id, "--", file_name),
        data_type = "external",
        file_type = file_ext,
        file_size = file_size
    )

    response <- POST(upload_session_url, add_headers(Authorization = paste0("Token ", api_token)), body = upload_data, encode = "json")
    upload_session <- fromJSON(content(response, "text", encoding = "UTF-8"))
    session_uri <- upload_session$session_url

    # Initial PUT request to get uploaded range
    headers <- add_headers("Content-Length" = "0", "Content-Range" = "bytes */*")
    response <- PUT(session_uri, headers)

    start_byte <- 0
    if (http_status(response)$status_code == 308) {
        if ("Range" %in% names(headers(response))) {
            uploaded_range <- as.integer(unlist(strsplit(gsub("bytes=", "", headers(response)[["Range"]]), "-")))
            start_byte <- uploaded_range[2] + 1
        }
    }

    # Read file data starting from start_byte
    con <- file(file_path, "rb")
    seek(con, start_byte)
    file_data <- readBin(con, "raw", file_size - start_byte)
    close(con)

    # Final PUT request to upload the file
    total_size <- file_size
    headers <- add_headers("Content-Length" = as.character(length(file_data)), "Content-Range" = paste0("bytes ", start_byte, "-", total_size - 1, "/", total_size))
    response <- PUT(session_uri, headers, body = file_data)

    if (http_status(response)$status_code %in% c(200, 201)) {
        message("Upload successful!")
    } else {
        message(paste0("Upload failed with status code: ", http_status(response)$status_code, ". Response: ", content(response, "text", encoding = "UTF-8")))
    }

    return(response)
}

# Add an external analysis plot in Pluto
post_plot <- function(client, experiment_id, plot_id = NULL, file_path, plot_data = NULL, methods = NULL) {
    api_token <- Sys.getenv("PLUTO_API_TOKEN")
    if (!is_valid_api_key(api_token)) {
        stop("Invalid API token. Check your PLUTO_API_TOKEN environment variable.")
    }

    base_url <- "https://api.pluto.bio/lab/experiments"
    experiment_url <- paste0(base_url, "/", experiment_id)
    plots_url <- paste0(experiment_url, "/plots")

    plot_uuid <- ""
    analysis_uuid <- ""

    if (!is.null(plot_id)) {
        # Get existing analysis details
        response <- GET(paste0(plots_url, "/", plot_id), add_headers(Authorization = paste0("Token ", api_token)))
        analysis_details <- fromJSON(content(response, "text", encoding = "UTF-8"))
        analysis_uuid <- analysis_details$analysis$uuid
        plot_uuid <- plot_id
    } else {
        # Create a new plot and analysis
        new_plot_data <- list(
            analysis_type = "external",
            display_type = "html",
            status = "published"
        )
        response <- POST(plots_url, add_headers(Authorization = paste0("Token ", api_token)), body = new_plot_data, encode = "json")
        new_plot <- fromJSON(content(response, "text", encoding = "UTF-8"))
        plot_uuid <- new_plot$uuid

        analysis_data <- list(
            analysis_type = "external",
            name = basename(file_path),
            methods = methods
        )

        if (!is.null(plot_data)) {
            analysis_data$results <- basename(plot_data)
        }

        response <- POST(paste0(experiment_url, "/analyses"), add_headers(Authorization = paste0("Token ", api_token)), body = analysis_data, encode = "json")
        new_analysis <- fromJSON(content(response, "text", encoding = "UTF-8"))
        analysis_uuid <- new_analysis$uuid
    }

    # File Upload
    upload_response <- upload_file(experiment_id, analysis_uuid, file_path)

    if (http_status(upload_response)$status_code %in% c(200, 201)) {
        message("File upload successful!")
    } else {
        stop("File upload failed!")
        # Additional error handling can go here
    }

    # Update the plot with the analysis ID
    update_data <- list(analysis_id = analysis_uuid)
    response <- PUT(paste0(plots_url, "/", plot_uuid), add_headers(Authorization = paste0("Token ", api_token)), body = update_data, encode = "json")

    updated_plot <- fromJSON(content(response, "text", encoding = "UTF-8"))
    return(updated_plot)
}
