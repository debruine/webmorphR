#' Average templates
#'
#' This function just averages the templates. An average image is returned, but it is just all the images superimposed. To create a template-aware average, see \code{\link{avg}}.
#'
#' @param stimuli list of class stimlist
#' @param name Name for the average
#'
#' @return list with class stimlist
#' @export
#'
#' @examples
#' demo_stim() |> average_tem()
#'
average_tem <- function(stimuli, name = "average") {
  stimuli <- validate_stimlist(stimuli, TRUE)

  # dim is coord (x/y), pt_i, tem_n
  pt <- sapply(stimuli, `[[`, "points", simplify = "array")

  avg <- apply(pt, c(1, 2), mean)

  w <- width(stimuli) |> mean()
  h <- height(stimuli) |> mean()
  img <- crop(stimuli, w, h) |> get_imgs()

  stim <- new_stim(
    magick::image_average(img),
    name,
    points = avg,
    lines = stimuli[[1]]$lines,
    closed = stimuli[[1]]$closed
  )

  validate_stimlist(stim)
}
