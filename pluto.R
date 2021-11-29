library(httr)
library(jsonlite)
library(rjson)

# Function to read sample and assay data
pluto_read <- function(experiment_uuid, data_type, limit=1000){
  
  access_token = Sys.getenv('PLUTO_API_TOKEN')
  if (is.null(access_token)){
    stop("Missing access token. Have you set the PLUTO_API_TOKEN environment variable?")
  }
  
  if (data_type == 'sample'){
    endpoint = '/sample-data/'
  } else if (data_type == 'assay'){
    endpoint = '/assay-data/'
  } else{
    stop("Unsupported data_type. Supported types are 'sample' and 'assay'.")
  }
  
  path = paste0('https://api.pluto.bio/lab/experiments/',
                experiment_uuid, endpoint, '?limit=', limit)
  
  response = GET(path, 
                 add_headers(Authorization = paste0('Token ',
                                                    access_token)))
  if (response$status_code == 200){
    json_obj = prettify(content(response, as = 'text', encoding = 'UTF-8'))
    json_list = fromJSON(json_obj)
    
    column_headers = unlist(json_list$headers)
    max_row = length(json_list$items)
    df = as.data.frame(matrix(nrow = max_row, ncol = length(column_headers)))
    names(df) = column_headers
    
    for (i in 1:length(json_list$items)){
      row = json_list$items[[i]]
      for (ii in 1:length(row)){
        if (is.character(unlist(row[ii])) | is.numeric(unlist(row[ii]))){
          df[i, ii] = row[ii]
        }
      }
    }
    
    return(df)
  }
  else{
    stop(paste0('Response: ', response$status_code))
  }
}
