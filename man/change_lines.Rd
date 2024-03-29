% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tem.R
\name{change_lines}
\alias{change_lines}
\title{Change template lines}
\usage{
change_lines(stimuli, line_id = 1, pts = NULL)
}
\arguments{
\item{stimuli}{list of stimuli}

\item{line_id}{index of the line to change}

\item{pts}{vector of points to change the line_idx to (deletes line if NULL)}
}
\value{
stimlist with altered templates
}
\description{
Alter, add or remove lines in a template
}
\examples{
# get image with dlib70 template and view lines
s <- demo_tems("dlib70")
s[[1]]$lines

# remove all lines
s2 <- change_lines(s, line_id = 1:13, pts = NULL)
s2[[1]]$lines

# visualise point indices
draw_tem(s2, pt.shape = "index", pt.size = 15)

# add a new line
s3 <- change_lines(s2, line_id = "face_outline", 
                   pts = c(2:18, 28:19, 2))
s3[[1]]$lines
draw_tem(s3)
}
\seealso{
Template functions
\code{\link{auto_delin}()},
\code{\link{average_tem}()},
\code{\link{centroid}()},
\code{\link{delin}()},
\code{\link{draw_tem}()},
\code{\link{features}()},
\code{\link{get_point}()},
\code{\link{remove_tem}()},
\code{\link{require_tems}()},
\code{\link{same_tems}()},
\code{\link{squash_tem}()},
\code{\link{subset_tem}()},
\code{\link{tem_def}()},
\code{\link{viz_tem_def}()}
}
\concept{tem}
