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

data_response_to_df <- function(response_json){

  column_names <- response_json$headers
  nrows <- length(response_json$items)
  df <- data.frame()

  for (i in 1:nrows){

    # Convert any NULLs in the row to NA
    row <- response_json$items[[i]]
    is_null_vals <- sapply(row, is.null)
    if (any(is_null_vals)){
      null_vals <- row[which(is_null_vals)] <- NA
    }

    # Append cleaned row to the final data frame
    df <- rbind(df, as.data.frame(row))
  }

  return(df)

}
