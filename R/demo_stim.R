#' Demo Stimuli
#'
#' A convenience function to get demo stimuli
#'
#' @param pattern defaults to all files
#'
#' @return stimlist
#' @export
#'
#' @examples
#' demo_stim() |> plot()
#'
demo_stim <- function(pattern = NULL) {
  path <- file.path("extdata", "test") |>
    system.file(package = "webmorphR")
  stimuli <- read_stim(path, pattern)

  stimuli
}

#' @export
#' @rdname demo_stim
demo_tems <- function(pattern = NULL) {
  path <- file.path("extdata", "tem_examples") |>
    system.file(package = "webmorphR")
  stimuli <- read_stim(path, pattern)
  
  stimuli
}
