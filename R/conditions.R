# Typed error conditions for the Pluto R SDK.
#
# These mirror the Python SDK's PlutoError / PlutoAPIError / PlutoNotFoundError /
# PlutoAuthError hierarchy. We use classed `rlang::abort` conditions so callers
# can write readable `tryCatch(..., pluto_not_found = handler)` blocks.
#
# Historically the base HTTP functions returned the parsed JSON response with
# an extra `response_status_code` field and never threw on non-2xx. That API
# is preserved (existing callers continue to work) — these helpers are additive
# and only throw when callers opt in by calling `pluto_check_response()` or
# setting `strict = TRUE` on a high-level function.


# Backend error codes that indicate "not found" even when HTTP status is 400.
# See pluto-api: pluto/exceptions.py (ObjectDoesNotExistException +
# ObjectDoesNotExist404Exception).
.pluto_not_found_codes <- c(
  "invalid_object_id",
  "object_not_found",
  "resource_not_found"
)


#' Check a Pluto API response and throw a typed condition on failure
#'
#' @description
#' Inspects a response object returned by `pluto_GET`/`pluto_POST`/etc. and
#' throws a classed `rlang` condition when the HTTP status is non-2xx or when
#' the backend error code indicates "not found". Returns the response
#' unchanged on success.
#'
#' Condition classes (in order of increasing specificity):
#'
#' - `pluto_error`                — base class for every SDK-raised error
#' - `pluto_api_error`            — an HTTP response was received but failed
#' - `pluto_auth_error`           — 401
#' - `pluto_permission_error`     — 403
#' - `pluto_not_found_error`      — 404 or 400+`invalid_object_id`
#' - `pluto_validation_error`     — 400 / 409 / 422 (other)
#' - `pluto_rate_limit_error`     — 429
#' - `pluto_server_error`         — 5xx
#'
#' @param response Response object returned from `pluto_GET`/`pluto_POST`/`pluto_PUT`/`pluto_PATCH`/`pluto_DELETE`.
#' @returns The response, unchanged, on success.
#' @export
pluto_check_response <- function(response){

  status <- response$response_status_code
  if (is.null(status) || (status >= 200 && status < 300)){
    return(invisible(response))
  }

  body_code <- response$code %||% NULL
  body_message <- response$message %||% response$detail %||% "Pluto API error"
  body_details <- response$details %||% NULL

  parts <- c(sprintf("HTTP %s", status))
  if (!is.null(body_message)) parts <- c(parts, body_message)
  if (!is.null(body_code))    parts <- c(parts, sprintf("code=%s", body_code))
  message <- paste(parts, collapse = " | ")

  classes <- .pluto_condition_classes(status, body_code)

  rlang::abort(
    message = message,
    class   = c(classes, "pluto_error"),
    status_code = status,
    code    = body_code,
    details = body_details,
    response = response
  )
}


.pluto_condition_classes <- function(status, code){

  # The `code` field beats the status — a 400 with invalid_object_id is a
  # not-found for our purposes.
  if (!is.null(code) && code %in% .pluto_not_found_codes){
    return(c("pluto_not_found_error", "pluto_api_error"))
  }

  if (is.null(status)){
    return(c("pluto_connection_error"))
  }

  by_status <- switch(
    as.character(status),
    "400" = c("pluto_validation_error", "pluto_api_error"),
    "401" = c("pluto_auth_error", "pluto_api_error"),
    "403" = c("pluto_permission_error", "pluto_api_error"),
    "404" = c("pluto_not_found_error", "pluto_api_error"),
    "409" = c("pluto_validation_error", "pluto_api_error"),
    "422" = c("pluto_validation_error", "pluto_api_error"),
    "429" = c("pluto_rate_limit_error", "pluto_api_error"),
    NULL
  )

  if (!is.null(by_status)) return(by_status)

  if (status >= 500 && status < 600){
    return(c("pluto_server_error", "pluto_api_error"))
  }

  c("pluto_api_error")
}


# Small helper — mirrors rlang's %||%. Avoids taking a hard dep on rlang for
# just this one operator, since older httr2 setups may not have it exported.
`%||%` <- function(a, b) if (is.null(a)) b else a


#' Return TRUE if a caught condition is a Pluto API "not found" error
#'
#' @description
#' Convenience predicate so callers can write
#' ```
#' tryCatch(pluto_get_experiment("BOGUS"),
#'   pluto_not_found_error = function(e) NULL)
#' ```
#' without remembering the exact class name.
#'
#' @param e A condition object caught by `tryCatch`.
#' @returns `TRUE` if the condition inherits from `pluto_not_found_error`.
#' @export
pluto_is_not_found <- function(e){
  inherits(e, "pluto_not_found_error")
}


#' Return TRUE if a caught condition is any Pluto API error
#' @param e A condition object caught by `tryCatch`.
#' @returns `TRUE` if the condition inherits from `pluto_api_error`.
#' @export
pluto_is_api_error <- function(e){
  inherits(e, "pluto_api_error")
}
