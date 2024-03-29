% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/image_func.R
\name{image_func_types}
\alias{image_func_types}
\title{Possible functions}
\usage{
image_func_types()
}
\value{
list of compatible function names
}
\description{
\code{\link{image_func}} can take a named function from the magick package, but only functions that return an image that is compatible with the current template (e.g., doesn't change size or shape).
}
\examples{
image_func_types()
}
