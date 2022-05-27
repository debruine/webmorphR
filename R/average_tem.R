#' Average templates
#'
#' This function just averages the templates. An average image is returned, but it is just all the images superimposed. To create a template-aware average, see [avg()].
#'
#' @param stimuli list of stimuli
#' @param name Name for the average
#'
#' @return list of stimuli consisting of just the average
#' @export
#' @family tem
#'
#' @examples
#' tem_only_avg <- demo_stim() |> average_tem()
#' 
#' # view the average template
#' draw_tem(tem_only_avg, bg = "white")
#' 
#' # view the superimposed image
#' plot(tem_only_avg)
average_tem <- function(stimuli, name = "average") {
  stimuli <- require_tems(stimuli, TRUE)

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

  as_stimlist(stim)
}
