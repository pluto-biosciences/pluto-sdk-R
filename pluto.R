library(httr)
library(jsonlite)
library(rjson)

# Helper function to format JSON -> df
pluto_api_response_to_df <- function(response){
  
  json_obj = prettify(httr::content(response, as = 'text', encoding = 'UTF-8'))
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

# Function to read sample and assay data
pluto_read <- function(experiment_id, data_type, limit=NULL, plot_id=NULL){
  
  pagination_step_size = 10000
  
  access_token = Sys.getenv('PLUTO_API_TOKEN')
  if (is.null(access_token)){
    stop("Missing access token. Have you set the PLUTO_API_TOKEN environment variable?")
  }
  
  if (data_type == 'sample'){
    endpoint = '/sample-data/'
    
  } else if (data_type == 'assay'){
    endpoint = '/assay-data/'
    
  } else if (data_type == 'results'){
    
    if (is.null(plot_id)){
      stop("plot_id param must be provided to fetch results")
      
    } else{
      endpoint = paste0('/plots/', plot_id, '/data/')
    }
    
  } else{
    stop("Unsupported data_type. Supported types are 'sample', 'assay', and 'results'.")
  }
  
  if (!is.null(limit)){
    path = paste0('https://api.pluto.bio/lab/experiments/',
                  experiment_id, endpoint, '?limit=', limit)
  } else{
    path = paste0('https://api.pluto.bio/lab/experiments/',
                  experiment_id, endpoint, '?limit=', pagination_step_size)
  }
  
  response = GET(path,
                 add_headers(Authorization = paste0('Token ',
                                                    access_token)))
  
  if (response$status_code == 200){
    
    # Calculate whether we need to paginate
    response_obj = fromJSON(prettify(httr::content(response, as = 'text', encoding = 'UTF-8')))
    total_count = response_obj$count
    
    # Temp until count is added to the data/ endpoint
    if (is.null(total_count)){
      total_count = length(response_obj$items)
    }
    
    if (!is.null(limit)){
      
      if (total_count <= limit){
        message('All ', total_count, ' rows of the assay data were fetched.')
      } else{
        message('Note: Assay data has ', total_count, ' rows but only ', limit, 
                ' will be fetched due to the "limit" parameter.', 
                '\nIncrease or remove the "limit" parameter if more rows are desired.')
      }
      
      final_df = pluto_api_response_to_df(response)
      
    } else{
      
      if (total_count <= pagination_step_size){
        message('All ', total_count, ' rows of the assay data were fetched.')
        final_df = pluto_api_response_to_df(response)
        
      } else{
        message('Paginating API calls to retrieve all ', total_count, 
                ' rows in the assay data in batches of ', 
                pagination_step_size, ' rows...')
        
        final_df = pluto_api_response_to_df(response)
        
        offsets = as.numeric(unlist(lapply(split(1:total_count, 
                                                 ceiling(seq_along(1:total_count)/pagination_step_size))[-1], 
                                           function (l){
                                             l[[1]]
                                           })))
        
        for (offset_num in offsets){
          
          message('Fetching rows ', offset_num, ' to ', offset_num + pagination_step_size, '...')
          
          path = paste0('https://api.pluto.bio/lab/experiments/',
                        experiment_id, endpoint, 
                        '?limit=', min(c(pagination_step_size, 
                                         total_count - nrow(final_df))),
                        '&offset=', offset_num)
          response = GET(path,
                         add_headers(Authorization = paste0('Token ',
                                                            access_token)))
          if (response$status_code == 200){
            final_df = rbind(final_df,
                             pluto_api_response_to_df(response))
            
          } else{
            stop(paste0('Response: ', response$status_code))
          }
          
        }
        
      }
    }
    
    
    if (data_type == 'sample'){
      names(final_df) = tolower(names(final_df))
    }
    if (data_type == 'assay'){
      names(final_df)[1] = tolower(names(final_df)[1])
    }
    return(final_df)
    
  } else if (response$status_code == 401){
    stop('Unauthorized: User does not have permission to view experiment')
    
  } else {
    stop(paste0('Response: ', response$status_code))
    
  }
}
