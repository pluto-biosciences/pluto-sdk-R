% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analyses.R
\name{create_analysis}
\alias{create_analysis}
\title{Internal create new external analysis}
\usage{
create_analysis(experiment_id, analysis_name = "")
}
\arguments{
\item{experiment_id}{Pluto experiment ID}

\item{analysis_name}{Name for the analysis}
}
\value{
API response for the created analysis object containing \code{uuid},
\code{analysis_type}, \code{category}, \code{results}, and \code{response_status_code}
}
\description{
Creates a new external analysis on an experiment
}
