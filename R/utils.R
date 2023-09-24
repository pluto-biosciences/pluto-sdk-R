is_alphanumeric <- function(input_string){
  return(grepl("^[[:alnum:]]+$", input_string))
}

quiet_message <- function(silent, message){
  if (!silent){
    return(message(message))
  }
}

is_valid_api_key <- function(api_key){

  # Check that API key is alphanumeric
  if (!is_alphanumeric(api_key)){
    return(FALSE)
  }

  # Check that length of API key is correct
  if (length(strsplit(api_key, '')[[1]]) != 40){
    return(FALSE)
  }

  # TODO add API call to validate

  return(TRUE)
}
