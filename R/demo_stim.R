#' Demo Stimuli
#'
#' A convenience function to get demo stimuli
#'
#' @param dir the directory in extdata to get files from
#' @param pattern defaults to all files
#' @param ... Other arguments to pass on to `read_tem()`
#'
#' @return stimlist
#' @export
#'
#' @examples
#' demo_stim() |> plot()
#'
demo_stim <- function(dir = c("test", "tem_examples", "composite", "london", "smiling", "lisa", "zoom", "rainbow"),
                     pattern = NULL, ...) {
  dir <- match.arg(dir)
  
  if (dir %in% c("test", "tem_examples")) { # included in webmorphR
    path <- system.file(file.path("extdata", dir), package = "webmorphR")
  } else if (requireNamespace("webmorphR.stim", quietly = TRUE)) {
    path <- system.file(file.path(dir), package = "webmorphR.stim")
  } else {
    stop("You  need to install the package webmorphR.stim to access these demo images\nremotes::install_github(\"debruine/webmorphR.stim\")")
  }

  stimuli <- read_stim(path, pattern, ...)

  stimuli
}
