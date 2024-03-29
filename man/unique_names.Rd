% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/unique_names.R
\name{unique_names}
\alias{unique_names}
\title{Get unique names}
\usage{
unique_names(full_names, breaks = "/", remove_ext = TRUE)
}
\arguments{
\item{full_names}{a list of full names}

\item{breaks}{regex for breaking up the names into parts. If "", then each character will be assessed.}

\item{remove_ext}{whether to remove the extension before comparing}
}
\value{
a list or vector of names with the common beginnings removed
}
\description{
Get unique names
}
\keyword{internal}
