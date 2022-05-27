#' Demo Stimuli
#'
#' A convenience function to get demo stimuli
#' 
#' [demo_stim()] is two composite faces with frl delineations; CC-BY https://doi.org/10.6084/m9.figshare.4055130.v1
#' 
#' [demo_tems()] is an image of the author, provided on a CC0 license, with 5 different delineations
#' 
#' [demo_unstandard()] is a set of 10 composite faces with frl delineations, rotated, resized, and cropped so face position is not standard and each image is a different size ; CC-BY https://doi.org/10.6084/m9.figshare.4055130.v1
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
#' 
#' @examples
#' demo_tems() |> draw_tem() |> plot(maxwidth = 1000)
demo_tems <- function(pattern = NULL) {
  path <- file.path("extdata", "tem_examples") |>
    system.file(package = "webmorphR")
  stimuli <- read_stim(path, pattern)
  
  stimuli
}

#' @export
#' @rdname demo_stim
#' 
#' @examples
#' demo_unstandard() |> plot(maxwidth = 1000)
demo_unstandard <- function(pattern = NULL) {
  path <- file.path("extdata", "unstandard") |>
    system.file(package = "webmorphR")
  stimuli <- read_stim(path, pattern)
  
  stimuli
}
