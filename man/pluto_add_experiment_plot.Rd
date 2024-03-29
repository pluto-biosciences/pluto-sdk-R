% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analyses.R
\name{pluto_add_experiment_plot}
\alias{pluto_add_experiment_plot}
\title{Add an analysis + plot to an experiment in Pluto}
\usage{
pluto_add_experiment_plot(
  experiment_id,
  display_file_path,
  results_file_path = NULL,
  analysis_name = NULL,
  plot_methods = NULL
)
}
\arguments{
\item{experiment_id}{Pluto experiment ID}

\item{display_file_path}{Path to the .html or image file containing a plot to push to Pluto}

\item{results_file_path}{Optional, path to the .csv file containing results to push to Pluto}

\item{analysis_name}{String, a title for the new analysis}

\item{plot_methods}{String, a description of the methods used for making the plot}
}
\description{
Uploads an HTML or image plot with optional information to a Pluto experiment.
Optionally upload tabular results as well.
}
