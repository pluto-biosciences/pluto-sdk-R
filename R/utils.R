PLOT_DATA_FORMAT_ARROW <- c('volcano_plot_v2')

is_alphanumeric <- function(input_string){
  return(grepl("^[[:alnum:]]+$", input_string))
}

validate_auth <- function(api_token){
  if (!is_valid_api_key(api_token)){
    stop("Invalid API token. Check your PLUTO_API_TOKEN environment variable.")
  }
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

clean_row <- function(row){

  # Convert any NULLs in the row to NA
  is_null_vals <- sapply(row, is.null)
  if (any(is_null_vals)){
    null_vals <- row[which(is_null_vals)] <- NA
  }
  return(row)
}

data_response_to_df <- function(response_json){

  column_names <- response_json$headers
  nrows <- length(response_json$items)
  df <- data.frame()

  all_rows <- do.call(clean_row, list(response_json$items))
  df <- as.data.frame(do.call(rbind, all_rows))

  return(df)

}

arrow_response_to_df <- function(response_json){

  column_names <- response_json$headers
  df <- NULL

  for (i in 1:length(column_names)){

    # Convert any NULLs in the row to NA
    y <- response_json$items[[i]]
    is_null_vals <- sapply(y, is.null)
    if (any(is_null_vals)){
      null_vals <- row[which(is_null_vals)] <- NA
    }

    # Append cleaned column to the final data frame
    new_column <- as.data.frame(matrix(y))
    if (is.null(df)){
      df <- new_column
    } else{
      df <- cbind(df, new_column)
    }
  }

  names(df) <- column_names
  return(df)

}

json_to_df_transfomer <- function(response_json, type){

  switch(type,
         data = data_response_to_df(response_json),
         arrow = arrow_response_to_df(response_json))

}

file_ext <- function(x){
  pos <- regexpr("\\.([[:alnum:]]+)$", x)
  ifelse(pos > -1L, substring(x, pos + 1L), "")
}
