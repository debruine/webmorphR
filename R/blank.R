#' Make blank images
#'
#' @param n the number of images to return
#' @param width width of the images
#' @param height height of the images
#' @param color background colour of the images
#' @param names names of the images
#'
#' @return stimlist with labelled images
#' @export
#'
#' @examples
#' blank(5, 100, 300, color = rainbow(5))
blank <- function(n = 1, width = 100, height = 100, color = "white", names = "img") {
  # fix name length
  if (length(names) < n && n > 1) {
    names <- rep_len(names, n)|> paste0("_", 1:n)
  }

  # fix color length
  color <- sapply(color, color_conv) |> rep_len(n)
  names(color) <- names

  # make stim
  stimuli <- lapply(color, function(color) {
    new_stim(magick::image_blank(width, height, color))
  })

  class(stimuli) <- c("stimlist", "list")

  stimuli
}
