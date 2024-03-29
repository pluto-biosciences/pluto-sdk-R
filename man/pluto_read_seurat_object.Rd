% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/experiments_data.R
\name{pluto_read_seurat_object}
\alias{pluto_read_seurat_object}
\title{Read Seurat object from Pluto}
\usage{
pluto_read_seurat_object(experiment_id, seurat_type, silent = FALSE)
}
\arguments{
\item{experiment_id}{Pluto experiment ID}

\item{seurat_type}{Seurat type, choices are "raw" or "final"}

\item{silent}{Boolean, whether to suppress console messages}
}
\value{
Requested Seurat object; if \code{seurat_type=final}, also returns custom cluster color palettes
}
\description{
Fetches the raw or final Seurat object for a given experiment in Pluto and
automatically adds cluster annotation sets to Seurat \code{meta.data} if \code{seurat_type=final}
}
