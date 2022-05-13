#' Loop
#'
#' @param stimuli list of images to morph between
#' @param steps number of steps from one image to the next
#' @param ... arguments to pass to \code{\link{trans}}
#'
#' @return stimlist
#' @export
#'
#' @examples
#' \dontrun{
#'   stimuli <- demo_stim("zoom") |>
#'     align(procrustes = TRUE) |>
#'     crop_tem(250, 50, 50, 50) |>
#'     resize(300)
#'   loop <- loop(stimuli, 5)
#'   animate(loop, 10)
#' }
loop <- function(stimuli, steps = 10, ...) {
  stimuli <- validate_stimlist(stimuli, TRUE)
  if (length(stimuli) < 2) {
    stop("You need at least 2 stimuli")
  }
  if (steps < 2) {
    stop("You need at least 2 steps")
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
