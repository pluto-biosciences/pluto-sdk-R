% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/auth.R
\name{pluto_login}
\alias{pluto_login}
\title{Validate and store Pluto API token}
\usage{
pluto_login(api_key, save_Renviron = FALSE, silent = FALSE)
}
\arguments{
\item{api_key}{String, API token}

\item{save_Renviron}{Boolean, save the API token to .Renviron}

\item{silent}{Boolean, suppress login message}
}
\value{
\code{TRUE} if API key is valid, otherwise returns an error message.
}
\description{
Validates the provided Pluto API token and, if valid, stores it to the
\code{PLUTO_API_TOKEN} environment variable to be provided in subsequent API requests.
}
