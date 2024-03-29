% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/experiments_data.R
\name{pluto_read_assay_data}
\alias{pluto_read_assay_data}
\title{Read Pluto assay data table into a data frame}
\usage{
pluto_read_assay_data(experiment_id, limit = NULL, silent = FALSE)
}
\arguments{
\item{experiment_id}{Pluto experiment ID}

\item{limit}{Integer for max rows to fetch, or NULL to fetch all rows}

\item{silent}{Boolean, whether to suppress console messages}
}
\value{
Data.frame containing the requested data
}
\description{
Fetches the assay data table for a given experiment in Pluto and stores it
in a data.frame
}
