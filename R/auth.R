#' Validate and store Pluto API token
#'
#' @description
#' Validates the provided Pluto API token and, if valid, stores it to the
#' `PLUTO_API_TOKEN` environment variable to be provided in subsequent API requests.
#'
#' @param api_key String, API token
#' @param save_Renviron Boolean, save the API token to .Renviron
#' @param silent Boolean, suppress login message
#' @param organization Optional organization UUID to scope subsequent requests
#'   to. Sent as the `Organization` header on every request. If omitted, the
#'   backend resolves to the user's `default_organization`. Required for users
#'   with access to multiple organizations who want to target a specific one.
#'   Stored in the `PLUTO_ORGANIZATION` env var. Use [pluto_list_organizations()]
#'   to discover available orgs and [pluto_use_organization()] to switch later.
#' @returns `TRUE` if API key is valid, otherwise returns an error message.
#' @export
pluto_login <- function(api_key, save_Renviron = FALSE, silent = FALSE,
                        organization = NULL){

  if (!is_valid_api_key(api_key)){
    stop("Invalid API key")

  } else{
    Sys.setenv(PLUTO_API_TOKEN = api_key)
    if (!is.null(organization) && nzchar(organization)){
      Sys.setenv(PLUTO_ORGANIZATION = organization)
    }
    quiet_message(silent,
                  message = "Welcome! You're logged in.")

    if (save_Renviron){
      if (!file.exists('.Renviron')){
        file.create('.Renviron')
      }
      line <- paste0("PLUTO_API_TOKEN=", api_key)
      write(line, file = ".Renviron", append = TRUE)
      if (!is.null(organization) && nzchar(organization)){
        write(paste0("PLUTO_ORGANIZATION=", organization),
              file = ".Renviron", append = TRUE)
      }
    }

    return(TRUE)
  }

}


#' List organizations the current user belongs to
#'
#' @description
#' Returns the list of organizations visible to the authenticated user.
#' Use the `uuid` of a result with [pluto_use_organization()] or the
#' `organization` argument on the base HTTP helpers to scope subsequent
#' requests.
#'
#' @returns A list of organization records.
#' @export
pluto_list_organizations <- function(){
  resp <- pluto_GET("user/organizations/")
  # Endpoint returns a bare JSON array. Strip the status code field.
  if (is.null(resp$items) && is.null(resp$count)){
    resp$response_status_code <- NULL
    return(unname(resp))
  }
  resp$items %||% resp$results %||% list()
}


#' Set the organization used for subsequent requests
#'
#' @description
#' Stores the organization UUID in the `PLUTO_ORGANIZATION` env var so every
#' subsequent SDK call includes the `Organization` header. Pass `NULL` to
#' clear the env var and let the backend fall back to the user's
#' `default_organization`.
#'
#' @param organization Organization UUID, or `NULL` to clear.
#' @returns The organization value that was set (invisibly).
#' @export
pluto_use_organization <- function(organization){
  if (is.null(organization) || !nzchar(organization)){
    Sys.unsetenv("PLUTO_ORGANIZATION")
    invisible(NULL)
  } else {
    Sys.setenv(PLUTO_ORGANIZATION = organization)
    invisible(organization)
  }
}


#' Return the organization UUID currently being sent, or NULL
#' @returns The organization UUID or NULL if unset.
#' @export
pluto_current_organization <- function(){
  env <- Sys.getenv("PLUTO_ORGANIZATION", unset = "")
  if (!nzchar(env)) NULL else env
}


#' Clear Pluto API token
#'
#' @description
#' Sets the `PLUTO_API_TOKEN` environment variable back to an empty value,
#' thus removing any previous token value it held.
#'
#' @param silent Boolean, suppress logout message
#' @returns `TRUE` when environment variable has been cleared.
#' @export
pluto_logout <- function(silent = FALSE){
  Sys.setenv(PLUTO_API_TOKEN = "")
  if (!silent){
    message("You're logged out")
  }
  return(TRUE)
}


#' Check for valid Pluto API token
#'
#' @description
#' Checks whether a valid value is stored in the `PLUTO_API_TOKEN` environment
#' variable.
#'
#' @param return_key Boolean, print the API key
#' @returns
#' `TRUE` if a valid API key is in the environment. If `return_key=T`, the API
#' token will be printed to the console.
#'
#' `FALSE` if a valid API key is not in the environment.
#' @export
pluto_is_logged_in <- function(return_key = FALSE){

  local_api_key <- Sys.getenv("PLUTO_API_TOKEN")

  if (is_valid_api_key(local_api_key)){
    if (return_key){
      cat(local_api_key, '\n')
    }
    return(TRUE)
  } else{
    return(FALSE)
  }
}
