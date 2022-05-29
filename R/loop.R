#' Loop
#' 
#' Morph between each image in a list of stimuli, looping back to the start. 
#'
#' @param stimuli list of stimuli to morph between
#' @param steps number of steps from one image to the next
#' @param ... arguments to pass to [trans()]
#'
#' @return list of stimuli containing each step of the loop
#' @export
#' @family webmorph
#'
#' @examples
#' \dontrun{
#' # align and crop images
#' stimuli <- demo_unstandard(1:5) |> 
#'   align() |> crop_tem()
#' 
#' loop <- loop(stimuli, 5)
#' 
#' # create an animated gif
#' animate(loop, fps = 10)
#' }
loop <- function(stimuli, steps = 10, ...) {
  stimuli <- require_tems(stimuli, TRUE)
  if (length(stimuli) < 2) {
    stop("You need at least 2 stimuli")
  }
  if (steps < 2) {
    stop("You need at least 2 steps")
  }
  
  n_unique_names <- names(stimuli) |> unique() |> length()
  if (n_unique_names < length(stimuli)) {
    names(stimuli) <- paste0(seq_along(stimuli), "_", names(stimuli))
  }
  
  from_img <- stimuli
  to_img <- c(stimuli[2:length(stimuli)], stimuli[1])
  p <- seq(0, 1, length.out = steps+1)
  p <- p[1:(length(p)-1)]
  loop <- trans(from_img, from_img, to_img, p, p, p)
  
  # get the right order
  order <- paste0(names(from_img), "_", names(to_img)) |>
    lapply(grepl, names(loop)) |>
    lapply(which) |>
    unlist()
  
  loop[order]
}
