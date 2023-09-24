# Pluto R package

A simple R interface for interacting with data and results stored in [Pluto](https://pluto.bio), the collaborative life sciences platform. 

## Create your API token

If you are accessing data programmatically for the first time, you'll need to generate a new API token. Navigate to your Account page in Pluto and click the "+ API token" button.

![account_api_1](https://user-images.githubusercontent.com/4951228/200467418-beaab6f8-f905-4915-8572-08e527604a7e.png)

A new API token will be generated, and can then be copied or viewed.

![account_api_2](https://user-images.githubusercontent.com/4951228/200467423-abee2baf-1f9b-41bb-8628-e7ee2d105603.png)

This API token can be used to fetch data programmatically using this R package.

## Install the `pluto` R package

```
# Install devtools if you don't already have it
install.packages("devtools")

# Install the pluto R package
remotes::install_github("pluto-biosciences/pluto-sdk-r")
```

Once installed, you can load the Pluto R package into your scripts with `library(pluto)`.

## Provide Pluto credentials

### Option 1: Provide your API token interactively / at run-time

You can "log in" to Pluto interactively by providing your API token to the `pluto_login` helper function.

To save your API key to the current environment so it can be used in subsequent API calls _during the current session_, simply provide the API key:

```
library(pluto)

pluto_login("<Your API key>")
```

You can also use `save_Renviron = T` to save your API key to the current environment AND save it to your `.Renviron` file so that it will be referenced in future sessions. This is a shortcut that achieves the same outcome as the manual Option #2 described below.

```
library(pluto)

pluto_login("<Your API key>", save_Renviron = T)

```


### Option 2: Manually add the `PLUTO_API_TOKEN` environment variable to your `.Renviron`

When using Pluto in one of your R projects, it is convenient to save your API token to an environment variable so that it can be easily referenced when running scripts in that project.

Copy the API token that you created on your Account page and store it in an environment variable called PLUTO_API_TOKEN. To do this, create an `.Renviron` file containing:

```
PLUTO_API_TOKEN="<YOUR API TOKEN>"
```

Save the file, and restart your R session to apply changes. To confirm that your API token is in your active R environment at any time, you can run `Sys.getenv("PLUTO_API_TOKEN")`.


## Fetch data from Pluto

### Sample data

```
library(pluto)

sample_data <- pluto_read_data(experiment_id = 'PLX140206', 
                               table_type = 'sample')

# All 12 rows of the sample data were fetched
```

<img width="757" alt="sample_data_api" src="https://user-images.githubusercontent.com/4951228/200468390-ca5377e7-72f2-4fea-a1b7-88e6a2c3942a.png">

### Assay data
```
assay_data <- pluto_read(experiment_id = 'PLX140206', 
                         table_type = 'assay')

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


## More resources

For more information about all available endpoints, visit the [Pluto API docs](https://docs.pluto.bio)
