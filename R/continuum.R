#' Morph between two images
#' 
#' Morph from one image to another in the specified steps.
#' 
#' @param from_img image to start at
#' @param to_img image to end at
#' @param from starting percentage
#' @param to ending percentage
#' @param by step size
#' @param ... arguments to pass to [trans()]
#'
#' @return a list of stimuli containing each step of the continuum
#' @export
#' @family webmorph
#'
#' @examples
#' \dontrun{
#' stimuli <- demo_stim()
#' cont <- continuum(stimuli$f_multi, stimuli$m_multi)
#' 
#' # create an animated gif
#' animate(cont, fps = 10, rev = TRUE)
#' }
continuum <- function(from_img, to_img, from = 0, to = 1, by = 0.1, ...) {
  steps <- seq(from, to, by)
  trans(from_img, from_img, to_img, steps, steps, steps, ...)
}
