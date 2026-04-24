# File discovery + download helpers.
#
# Mirrors the Python SDK's _files + bulk/epigenetics helpers. Lets callers
# list experiment files filtered by data_type, grab signed URLs, and stream
# downloads without reinventing the pattern in each caller.


# Internal: list experiment files filtered by data_type. Returns a list of
# file records ({uuid, filename, file_size, ...}). Tolerates the three
# response shapes the API uses:
#   - {data_type: {items: [...]}}  (single-type serializer)
#   - {items: [...]}               (bare envelope)
#   - bare list                    (raw DRF)
.pluto_list_files_by_type <- function(experiment_id, data_type){
  url_path <- paste0(
    "lab/experiments/", experiment_id,
    "/files/?data_type=", utils::URLencode(data_type, reserved = TRUE)
  )
  resp <- pluto_GET(url_path)

  bucket <- resp[[data_type]]
  if (is.list(bucket) && !is.null(bucket$items)){
    return(bucket$items)
  }
  if (!is.null(resp$items) && is.list(resp$items)){
    return(resp$items)
  }
  # Fall back: strip response_status_code and return whatever remains if it
  # looks like a bare list.
  status <- resp$response_status_code
  resp$response_status_code <- NULL
  if (length(resp) > 0 && is.null(names(resp))){
    return(unname(resp))
  }
  list()
}


# Internal: fetch a short-lived signed URL for a specific file.
.pluto_signed_url <- function(experiment_id, file_uuid, filename = NULL){
  query <- ""
  if (!is.null(filename)){
    query <- paste0("?filename=", utils::URLencode(filename, reserved = TRUE))
  }
  url_path <- paste0(
    "lab/experiments/", experiment_id,
    "/files/", file_uuid, "/download/", query
  )
  resp <- pluto_GET(url_path)
  if (is.null(resp$url)){
    rlang::abort(
      sprintf("Expected signed URL for file %s, got nothing", file_uuid),
      class = c("pluto_api_error", "pluto_error")
    )
  }
  resp$url
}


# Internal: download a single file record (must have $uuid and $filename) to
# `dest_dir`, returning the local path.
.pluto_download_file_record <- function(experiment_id, file_rec, dest_dir){
  if (is.null(file_rec$uuid) || is.null(file_rec$filename)){
    stop("file record missing uuid or filename")
  }
  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)
  url <- .pluto_signed_url(experiment_id, file_rec$uuid, file_rec$filename)
  dest <- file.path(dest_dir, file_rec$filename)
  utils::download.file(url, destfile = dest, quiet = TRUE, mode = "wb")
  dest
}


# Counts kind -> data_type mapping. Matches pluto-api:
# lab/models/data.py ExperimentFile.DataType.
.pluto_counts_kinds <- list(
  raw            = "assay_data",
  cpm            = "assay_data_cpm",
  tpm_gene       = "tpm_gene",
  tpm_transcript = "tpm_transcript"
)


#' Get counts / normalized expression as a data frame
#'
#' @description
#' Unified accessor for the bulk expression matrices Pluto stores:
#'
#' - `kind = "raw"` — raw assay data (counts). Equivalent to
#'   `pluto_read_assay_data()`.
#' - `kind = "cpm"` — CPM-normalized assay data.
#' - `kind = "tpm_gene"` — TPM by gene.
#' - `kind = "tpm_transcript"` — TPM by transcript.
#'
#' For `"raw"` this delegates to the existing assay-data flow. Other kinds
#' discover the right file through `/files/?data_type=<X>`, download via a
#' signed URL, and parse the CSV.
#'
#' @param experiment_id Pluto experiment ID.
#' @param kind One of "raw", "cpm", "tpm_gene", "tpm_transcript" (default "raw").
#' @param dest_dir Where to save intermediate downloads. Defaults to `tempdir()`.
#' @returns A data.frame matching the file as stored on Pluto (no transpose).
#' @export
pluto_get_counts <- function(experiment_id, kind = "raw", dest_dir = tempdir()){

  if (!(kind %in% names(.pluto_counts_kinds))){
    stop(sprintf(
      "Invalid kind=%s; expected one of %s",
      shQuote(kind),
      paste(shQuote(names(.pluto_counts_kinds)), collapse = ", ")
    ))
  }

  if (kind == "raw"){
    return(pluto_read_assay_data(experiment_id))
  }

  data_type <- .pluto_counts_kinds[[kind]]
  files <- .pluto_list_files_by_type(experiment_id, data_type)
  if (length(files) == 0){
    rlang::abort(
      sprintf("No %s files are available for this experiment. Check the Pluto UI to confirm processing has completed.", kind),
      class = c("pluto_not_found_error", "pluto_api_error", "pluto_error")
    )
  }
  if (length(files) > 1){
    message(sprintf(
      "Multiple %s files available; using the first (%s). Filter by file_uuid if you need a specific one.",
      kind, files[[1]]$filename
    ))
  }

  local <- .pluto_download_file_record(experiment_id, files[[1]], dest_dir)
  utils::read.csv(local)
}


#' List BigWig coverage files for an experiment
#' @param experiment_id Pluto experiment ID.
#' @returns A list of file records, each with `uuid`, `filename`, `file_size`
#'   and related metadata.
#' @export
pluto_list_bigwigs <- function(experiment_id){
  .pluto_list_files_by_type(experiment_id, "bigwig")
}


#' Download BigWig files for an experiment
#' @param experiment_id Pluto experiment ID.
#' @param dest_dir Destination directory. Defaults to `tempdir()`.
#' @param file_uuids Optional vector of file UUIDs to filter to.
#' @returns A character vector of local paths.
#' @export
pluto_download_bigwigs <- function(experiment_id, dest_dir = tempdir(), file_uuids = NULL){
  .pluto_download_files_filtered(experiment_id, "bigwig", dest_dir, file_uuids)
}


#' List consensus BED (peaks) files for an experiment
#' @param experiment_id Pluto experiment ID.
#' @returns A list of file records.
#' @export
pluto_list_peaks <- function(experiment_id){
  .pluto_list_files_by_type(experiment_id, "consensus_bed")
}


#' Download consensus BED (peaks) files for an experiment
#' @param experiment_id Pluto experiment ID.
#' @param dest_dir Destination directory. Defaults to `tempdir()`.
#' @param file_uuids Optional vector of file UUIDs to filter to.
#' @returns A character vector of local paths.
#' @export
pluto_download_peaks <- function(experiment_id, dest_dir = tempdir(), file_uuids = NULL){
  .pluto_download_files_filtered(experiment_id, "consensus_bed", dest_dir, file_uuids)
}


#' List BAM files for an experiment (BAI files come alongside)
#' @param experiment_id Pluto experiment ID.
#' @returns A list of file records.
#' @export
pluto_list_bam_files <- function(experiment_id){
  .pluto_list_files_by_type(experiment_id, "bam")
}


#' List Seurat (.rds) files for an experiment
#' @param experiment_id Pluto experiment ID.
#' @returns A list of file records.
#' @export
pluto_list_seurat_objects <- function(experiment_id){
  .pluto_list_files_by_type(experiment_id, "seurat")
}


# Shared helper: filter and download files of a given data_type.
.pluto_download_files_filtered <- function(experiment_id, data_type, dest_dir, file_uuids){
  files <- .pluto_list_files_by_type(experiment_id, data_type)
  if (!is.null(file_uuids)){
    wanted <- as.character(file_uuids)
    files <- Filter(function(f) isTRUE(f$uuid %in% wanted), files)
    if (length(files) != length(wanted)){
      missing <- setdiff(wanted, vapply(files, function(f) f$uuid, character(1)))
      if (length(missing) > 0){
        rlang::abort(
          sprintf("Could not find %s files with uuids: %s",
                  data_type, paste(missing, collapse = ", ")),
          class = c("pluto_not_found_error", "pluto_api_error", "pluto_error")
        )
      }
    }
  }
  vapply(
    files,
    function(f) .pluto_download_file_record(experiment_id, f, dest_dir),
    character(1)
  )
}
