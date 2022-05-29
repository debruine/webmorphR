#' Demo Stimuli
#'
#' A convenience function to get demo stimuli. See the Details below for citation and license info.
#' 
#' * [demo_stim()]: two composite faces with frl delineations; 500x500 pixels
#' 
#' * [demo_tems()]: an image with 5 different delineations; 675x900 pixels
#' 
#' * [demo_unstandard()]: a set of 10 composite faces with frl delineations; rotated, resized, and cropped so face position is not standard and each image is a different size (444 to 645 pixels)
#' 
#' ### Citation
#' 
#' The images from `demo_stim()` and `demo_unstandard()` are usable on a CC-BY license, citing: 
#' 
#' DeBruine, L. (2016).Young adult composite faces (Version1). figshare. doi: [10.6084/m9.figshare.4055130.v1](https://doi.org/10.6084/m9.figshare.4055130.v1). 
#' 
#' The image from `demo_tems()` is Lisa DeBruine (the author of webmorphR) and available on a CC-O license (no attribution needed). 
#'
#' @param pattern Vector of patterns to use to search for files, or a vector of image indices (e.g., 1:4 selects the first 4 images and their templates)
#'
#' @return list of stimuli
#' @export
#'
#' @examples
#' demo_stim() |> label()
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
#' # visualise templates
#' demo_tems() |> 
#'   draw_tem(pt.size = 10) |> 
#'   label() |>
#'   plot(maxwidth = 1000)
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
#' # visualise keeping relative sizes
#' demo_unstandard() |> 
#'   to_size(keep_rels = TRUE) |>
#'   pad(80, 0, 0, 0) |>
#'   label() |>
#'   plot(nrow = 2, maxwidth = 1000)
demo_unstandard <- function(pattern = NULL) {
  path <- file.path("extdata", "unstandard") |>
    system.file(package = "webmorphR")
  stimuli <- read_stim(path, pattern)
  
  stimuli
}
