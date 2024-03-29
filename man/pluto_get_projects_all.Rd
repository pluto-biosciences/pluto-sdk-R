% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/projects.R
\name{pluto_get_projects_all}
\alias{pluto_get_projects_all}
\title{Get all projects available in Pluto}
\usage{
pluto_get_projects_all(limit = 1000)
}
\arguments{
\item{limit}{Max number of projects to return, default 1000}
}
\value{
API response object containing \code{count}, a count of the total projects,
and \code{items}, an array of project objects
}
\description{
Fetches metadata for all projects the requester has permission to view
}
