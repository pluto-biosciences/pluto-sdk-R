# Full-text search across projects and experiments.


#' Search projects or experiments
#'
#' @description
#' Thin wrapper around `GET /lab/search/`. Returns the `{count, items}`
#' envelope so callers can drive pagination.
#'
#' @param query Free-text search query.
#' @param type One of `"experiments"` (default) or `"projects"`.
#' @param access One of `"shareable"` (default; requires an authenticated
#'   token) or `"public"`.
#' @param offset Pagination offset, default 0.
#' @param limit Pagination limit, default 24.
#' @returns The full response envelope (`count`, `items`).
#' @export
pluto_search <- function(query, type = c("experiments", "projects"),
                         access = c("shareable", "public"),
                         offset = 0, limit = 24){
  type <- match.arg(type)
  access <- match.arg(access)

  if (is.null(query) || !nzchar(query)){
    stop("query is required and must be non-empty.")
  }

  qs <- paste0(
    "?query=", utils::URLencode(query, reserved = TRUE),
    "&type=", type,
    "&access=", access,
    "&offset=", offset,
    "&limit=", limit
  )
  pluto_GET(paste0("lab/search/", qs))
}


#' Paginate through every matching search result
#'
#' @param query Free-text search query.
#' @param type One of `"experiments"` (default) or `"projects"`.
#' @param access One of `"shareable"` (default) or `"public"`.
#' @param page_size Number of results per request, default 100.
#' @param max_results Hard cap on total results returned, default 10000.
#' @returns A list of matching items (flat; no envelope).
#' @export
pluto_search_all <- function(query, type = c("experiments", "projects"),
                             access = c("shareable", "public"),
                             page_size = 100, max_results = 10000){
  type <- match.arg(type)
  access <- match.arg(access)
  results <- list()
  offset <- 0

  repeat {
    page <- pluto_search(
      query,
      type = type, access = access,
      offset = offset, limit = page_size
    )
    items <- page$items %||% list()
    total <- page$count %||% 0L
    if (length(items) == 0) break
    results <- c(results, items)
    if (length(results) >= min(total, max_results)) break
    offset <- offset + page_size
  }
  utils::head(results, max_results)
}
