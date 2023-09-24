#' Validate and store Pluto API token
#'
#' @description
#' Validates the provided Pluto API token and, if valid, stores it to the
#' `PLUTO_API_TOKEN` environment variable to be provided in subsequent API requests.
#'
#' @param api_key String, API token
#' @param save_Renviron Boolean, save the API token to .Renviron
#' @param silent Boolean, suppress login message
#' @returns `TRUE` if API key is valid, otherwise returns an error message.
#' @export
pluto_login <- function(api_key, save_Renviron = FALSE, silent = FALSE){

  if (!is_valid_api_key(api_key)){
    stop("Invalid API key")

  } else{
    Sys.setenv(PLUTO_API_TOKEN = api_key)
    quiet_message(silent,
                  message = "Welcome! You're logged in.")

    if (save_Renviron){
      if (!file.exists('.Renviron')){
        file.create('.Renviron')
      }
      line <- paste0("PLUTO_API_TOKEN=", api_key)
      write(line, file = ".Renviron", append = TRUE)
    }

    return(TRUE)
  }

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
