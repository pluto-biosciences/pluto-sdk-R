#' Upload a file to Pluto
#'
#' @description
#' Uploads a file to a given experiment and analysis in Pluto using a signed URL.
#'
#' @param experiment_id Pluto experiment ID
#' @param analysis_id Pluto analysis ID
#' @param file_path Path to the file to be uploaded
#' @returns HTTP response status indicating the success or failure of the upload
#' @export
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
