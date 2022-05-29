#' Convert stimuli to a ggplot
#' 
#' Convert a stimulus or list of stimuli into a ggplot, which can be further used with ggplot functions.
#'
#' @param stimuli list of stimuli 
#' @param ... Additional arguments to pass to [plot_stim()] if stimuli contains more than 1 image
#'
#' @return a ggplot object
#' @export
#' @family viz
#'
#' @examples
#' stimuli <- demo_stim()
#' gg <- as_ggplot(stimuli)
#' 
#' # add to ggplot object; coordinates are pixels
#' # (images are 500x500 each, plus 10px padding)
#' gg + 
#'   ggplot2::geom_vline(xintercept = 0, color = "red") +
#'   ggplot2::geom_vline(xintercept = 1030, color = "blue") +
#'   ggplot2::geom_hline(yintercept = 0, color = "green") +
#'   ggplot2::geom_hline(yintercept = 520, color = "purple") +
#'   ggplot2::annotate("point", x = 515, y = 260, size = 10) +
#'   ggplot2::labs(
#'     title = "This is a ggplot!",
#'     caption = "Made with webmorphR"
#'   )
as_ggplot <- function(stimuli, ...) {
  stimuli <- as_stimlist(stimuli)
  
  if (length(stimuli) > 1) {
    stimuli <- plot(stimuli, ...)
  }
  
  img <- stimuli[[1]]$img
  
  magick::image_ggplot(img)
}