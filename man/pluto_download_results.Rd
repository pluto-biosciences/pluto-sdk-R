% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/results.R
\name{pluto_download_results}
\alias{pluto_download_results}
\title{Download Pluto results table}
\usage{
pluto_download_results(experiment_id, plot_id, dest_filename = NULL)
}
\arguments{
\item{experiment_id}{Pluto experiment ID}

\item{plot_id}{Pluto plot ID}

\item{dest_filename}{Destination filename for CSV file (e.g. "PLX12345_deg_table.csv")}
}
\value{
Saves the downloaded data to \code{dest_filename}
}
\description{
Fetches the results table for a given experiment and plot in Pluto and saves
it as a CSV file
}
