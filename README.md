# Pluto R package

A simple R interface for interacting with data and results stored in [Pluto](https://pluto.bio), the collaborative life sciences platform. 

## Create your API token

If you are accessing data programmatically for the first time, you'll need to generate a new API token. Navigate to your Account page in Pluto and click the "+ API token" button.

![account_api_1](https://user-images.githubusercontent.com/4951228/200467418-beaab6f8-f905-4915-8572-08e527604a7e.png)

A new API token will be generated, and can then be copied or viewed.

![account_api_2](https://user-images.githubusercontent.com/4951228/200467423-abee2baf-1f9b-41bb-8628-e7ee2d105603.png)

This API token can be used to fetch data programmatically using this R package.

## Set up R environment

### 1. Set the `PLUTO_API_TOKEN` environment variable

Copy the API token that you created on your Account page and store it in an environment variable called PLUTO_API_TOKEN. To do this, create an `.Renviron` file containing:

```
PLUTO_API_TOKEN="<YOUR API TOKEN>"
```

### 2. Source the latest version of the R package

```
# Load required dependencies
library(httr)
library(jsonlite)
library(rjson)

# Print PLUTO_API_TOKEN to confirm that it's in your environment
Sys.getenv('PLUTO_API_TOKEN')

# If PLUTO_API_TOKEN is blank, make sure the API key is in .Renviron
# and restart your R session, then rerun Sys.getenv('PLUTO_API_TOKEN')

# Import the pluto_read() function
devtools::source_url("https://github.com/pluto-biosciences/pluto-sdk-R/blob/main/pluto.R?raw=TRUE")
```

## Fetch data from Pluto

### Sample data

```
sample_data <- pluto_read(experiment_id = 'PLX140206', 
                          data_type = 'sample')

# All 12 rows of the sample data were fetched
```

<img width="757" alt="sample_data_api" src="https://user-images.githubusercontent.com/4951228/200468390-ca5377e7-72f2-4fea-a1b7-88e6a2c3942a.png">

### Assay data
```
assay_data <- pluto_read(experiment_id = 'PLX140206', 
                         data_type = 'assay')

# Paginating API calls to retrieve all 32544 rows in the assay data in batches of 10000 rows... 
# Fetching rows 10001 to 20001... 
# Fetching rows 20001 to 30001... 
# Fetching rows 30001 to 32544...
```

<img width="758" alt="assay_data_api" src="https://user-images.githubusercontent.com/4951228/200468369-058ef989-c880-4a8e-adac-9ca76f8bf5fc.png">

### Plot data (results)

Visit our website to [learn more about the assays and analyses that Pluto supports](https://pluto.bio/product/experiments). To fetch data from a specific analysis and plot, obtain the plot ID from the Methods Modal on any plot:

![plot_id_methods_gif](https://user-images.githubusercontent.com/4951228/200468480-2bcc18f0-eb14-486f-9385-b263aec4372d.gif)

Use the plot ID in the pluto_read() function to fetch the underlying results:

```
deg_table <- pluto_read(experiment_id = "PLX140206", 
                        plot_id = "386b3bbf-fc86-4071-88c2-6340634cdc62",
                        data_type = "results")

# Paginating API calls to retrieve all 16954 rows in the assay data in batches of 10000 rows... 
# Fetching rows 10001 to 20001...
```
<img width="758" alt="deg_api" src="https://user-images.githubusercontent.com/4951228/200468566-36b2fd20-ee6c-429c-8d37-08a799927910.png">


## Available endpoints

For more information about all available endpoints, visit the [Pluto API docs](https://docs.pluto.bio)
