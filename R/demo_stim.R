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
#' demo_stim() %>% plot()
#'
demo_stim <- function(dir = c("test", "composite", "london", "smiling", "lisa", "zoom", "rainbow"),
                     pattern = NULL, ...) {
  dir <- match.arg(dir)
  
  if (dir == "test") { # included in webmorphR
    path <- system.file(file.path("extdata", dir), package = "webmorphR")
  } else if (requireNamespace("stimsets", quietly = TRUE)) {
    path <- system.file(file.path(dir), package = "stimsets")
  } else {
    stop("You  need to install the package stimsets to access these demo images\nremotes::install_github(\"debruine/stimsets\")")
  }

  stimuli <- read_stim(path, pattern, ...)

  stimuli
}
