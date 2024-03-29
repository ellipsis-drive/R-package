% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/path.R
\name{path.search}
\alias{path.search}
\title{Search for a path to a folder}
\usage{
path.search(
  pathTypes = list("raster", "vector", "file", "folder"),
  root = NULL,
  text = NULL,
  active = NULL,
  userId = NULL,
  pageStart = NULL,
  hashtag = NULL,
  extent = NULL,
  resolution = NULL,
  date = NULL,
  listAll = FALSE,
  token = NULL
)
}
\arguments{
\item{pathTypes}{a list with strings used to indicate the file types included in the search}

\item{root}{a list of strings indicating the rootname of the folder you would like to search}

\item{text}{a string indicating ...}

\item{active}{a boolean indicating ...}

\item{userId}{a uuid referencing the ...}
}
\value{
A name value list containing information about the folder
}
\description{
Search for a path to a folder
}
