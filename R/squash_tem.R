#' Squash Template Points
#' 
#' Move template points that are outside the image boundaries (e.g., negative values or larger than image width or height) to the borders of the image.
#'
#' @param stimuli list of stimuli
#'
#' @return list of stimuli
#' @export
#' @family tem
#'
#' @examples
#' nosquash <- demo_stim(1) |> 
#'   crop(0.4, 0.5)
#' 
#' squash <- demo_stim(1) |> 
#'   crop(0.4, 0.5) |> 
#'   squash_tem()
#'
#' # add padding and visualise templates
#' c(nosquash, squash) |> 
#'   pad(50) |>
#'   draw_tem(pt.size = 5) 
squash_tem <- function(stimuli) {
  stimuli <- as_stimlist(stimuli)
  
  for (i in seq_along(stimuli)) {
    if (!is.null(stimuli[[i]]$points)) {
      w <- stimuli[[i]]$width
      h <- stimuli[[i]]$height
      
      stimuli[[i]]$points <- apply(stimuli[[i]]$points, 2, function(pt) {
        # move points into image boundaries
        pt |>
          pmax(c(0, 0)) |>
          pmin(c(w-1, h-1)) # subtract 1 for 0-vs 1-based origin
      })
    }
  }
  
  stimuli
}