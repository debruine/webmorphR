#' Demo Stimuli
#'
#' A convenience function to get demo stimuli
#'
#' @param set the images set ("test" or "tem_examples")
#' @param pattern defaults to all files
#'
#' @return stimlist
#' @export
#'
#' @examples
#' demo_stim() |> plot()
#'
demo_stim <- function(set = c("test", "tem_examples"),
                     pattern = NULL) {
  set <- match.arg(set)
  path <- file.path("extdata", set) |>
    system.file(package = "webmorphR")
  stimuli <- read_stim(path, pattern)

  stimuli
}
