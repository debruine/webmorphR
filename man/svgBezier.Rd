% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/svg.R
\name{svgBezier}
\alias{svgBezier}
\title{Construct Bezier Curves from Template Points}
\usage{
svgBezier(v, idx = 1)
}
\arguments{
\item{v}{matrix of template points with x and y as rows and point as columns}

\item{idx}{index of line segment within the path}
}
\value{
string with path component
}
\description{
Construct Bezier Curves from Template Points
}
\keyword{internal}
