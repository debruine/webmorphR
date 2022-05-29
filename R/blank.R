#' Make blank images
#'
#' @param n the number of images to return
#' @param width,height image dimensions
#' @param color background color, see [color_conv()]
#' @param names names of the images (appended with index if < n)
#'
#' @return list of stimuli
#' @export
#' @family stim
#'
#' @examples
#' stimuli <- blank(5, 100, 250, color = rainbow(5))
#' 
#' label(stimuli, size = 20)
blank <- function(n = 1, width = 100, height = 100, color = "white", names = "img") {
  # fix name length
  if (length(names) < n && n > 1) {
    idx <- as.character(n) |> nchar() |>
      formatC(x = 1:n, digits = 0, flag = "0")
    names <- rep_len(names, n)|> paste0("_", idx)
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
