library(httr)
library(jsonlite)
library(rjson)

# Function to read sample and assay data
pluto_read <- function(experiment_id, data_type, plot_id=NULL, limit=1000){
  
  access_token = Sys.getenv('PLUTO_API_TOKEN')
  if (is.null(access_token)){
    stop("Missing access token. Have you set the PLUTO_API_TOKEN environment variable?")
  }
  
  if (data_type == 'sample'){
    endpoint = paste0('/sample-data/?limit=', limit)
    
  } else if (data_type == 'assay'){
    endpoint = paste0('/assay-data/?limit=', limit)
    
  } else if (data_type == 'deg'){
    
    if (is.null(plot_id)){
      stop("plot_id param must be provided to fetch DEG data")
    } else{
      endpoint = paste0('/plots/', plot_id, '/data/?limit=', limit)
    }
    
  }else{
    stop("Unsupported data_type. Supported types are 'sample' and 'assay'.")
  }
  
  path = paste0('https://api.pluto.bio/lab/experiments/',
                experiment_id, endpoint)
  
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
    
  } else if (response$status_code == 401){
    stop('Unauthorized: User does not have permission to view experiment')
    
  } else{
    stop(paste0('Response: ', response$status_code))
  }
}
