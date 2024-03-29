% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{pluto_get_experiment_plots}
\alias{pluto_get_experiment_plots}
\title{List all plots in an experiment in Pluto}
\usage{
pluto_get_experiment_plots(experiment_id)
}
\arguments{
\item{experiment_id}{Pluto experiment ID (e.g. PLXP02355)}
}
\value{
API response object containing \code{count}, a count of the total plots
in the experiment, and \code{items}, an array of plots
}
\description{
Fetches metadata for all plots in a given experiment in Pluto
}
