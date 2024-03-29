% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/info.R
\name{add_info}
\alias{add_info}
\title{Add Information}
\usage{
add_info(stimuli, ..., .by = NULL)
}
\arguments{
\item{stimuli}{list of stimuli}

\item{...}{data table or named vectors of info to add}

\item{.by}{the column to use to match info to stimuli names; leave NULL if the data are to be matched by order}
}
\value{
list of stimuli with info added
}
\description{
Add info with a data table that contains the info in either the same order as the stimulus list, or matching the stimuli item name with the column specified by \code{.by}.
}
\details{
You can also add data as named vectors.
}
\examples{
stimuli <- demo_stim() |>
  add_info(project = "XXX", gender = c("F", "M"))

stimuli$f_multi$info |> str()

}
\seealso{
Other info: 
\code{\link{compare}()},
\code{\link{get_info}()},
\code{\link{get_point}()},
\code{\link{height}()},
\code{\link{metrics}()},
\code{\link{rename_stim}()},
\code{\link{width}()}
}
\concept{info}
