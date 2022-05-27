#' Create an animated gif from a list of stimuli
#'
#' @param stimuli list of stimuli
#' @param fps frames per second
#' @param loop how many times to loop the animation (0 = infinite)
#' @param rev whether to loop back and forth (TRUE) or in one direction (FALSE)
#'
#' @return magick image
#' @export
#'
#' @examples
#' gif <- demo_stim() |> animate()
animate <- function(stimuli, fps = 1, loop = 0, rev = FALSE) {
  if (length(stimuli) < 2) {
    stop("You need at least two images in the list to make an animated gif")
  }

  img <- get_imgs(stimuli)
  # if looping back and forth
  if (isTRUE(rev)) img <- c(img, rev(img))

  x <- magick::image_animate(
    img,
    fps = fps,
    dispose = "previous",
    loop = loop,
    optimize = TRUE)

  new_stim(x, "animation.gif") |>
    new_stimlist()
}
