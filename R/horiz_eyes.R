#' Make eyes horizontal
#' 
#' Rotate each stimulus so the eye points are horizontal. 
#'
#' @param stimuli list of stimuli
#' @param left_eye The first point to align (defaults to 0)
#' @param right_eye The second point to align (defaults to 1)
#' @param fill background color to pass to rotate, see [color_conv()]
#'
#' @return list of stimuli with rotated tems and/or images
#' @export
#' @family manipulators
#'
#' @examples
#' stimuli <- demo_unstandard(1:3)
#' horiz_eyes(stimuli, fill = "red")
#'
horiz_eyes <- function(stimuli, left_eye = 0, right_eye = 1, fill = wm_opts("fill")) {
  stimuli <- require_tems(stimuli)
  
  degrees <- lapply(stimuli, `[[`, "points") |>
    lapply(function(pt) {
      x1 = pt[[1, left_eye+1]]
      y1 = pt[[2, left_eye+1]]
      x2 = pt[[1, right_eye+1]]
      y2 = pt[[2, right_eye+1]]
      rad <- atan2(y1 - y2, x1 - x2) %% (2*pi)
      180 - (rad / (pi/180))
    })
  
  stimuli |>
    rotate(degrees = degrees, fill = fill, keep_size = TRUE)
}
