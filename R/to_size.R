#' Resize and crop/pad images to a specified size
#'
#' @param stimuli of class stimlist
#' @param width the target width (or a vector of width and height)
#' @param height the target height (or null if width is dimensions)
#' @param fill background color if cropping goes outside the original image
#' @param patch whether to use the patch function to set the background color
#' @param crop whether to crop or pad images to make them the specified size
#' @param keep_rels whether to keep the size relationships between images in the set, or make all the maximum size
#'
#' @return stimlist with cropped tems and/or images
#' @export
#'
#' @examples
#'
#' # make images with different aspect ratios and sizes
#' stimuli <- demo_stim() |> crop(c(0.8, 1.0)) |> resize(c(1.0, 0.5))
#'
#' to_size(stimuli, 300, 400, fill = "dodgerblue") |> plot()
#'
to_size <- function(stimuli, width, height = NULL,
                    fill = wm_opts("fill"), patch = FALSE,
                    crop = FALSE, keep_rels = FALSE) {
  # process width and height
  if (length(width) == 2 && is.null(height)) {
    dim <- width
    width <- xget(dim, "width", "w", 1)
    height <- xget(dim, "height", "h", 2)
  }

  if (!is.numeric(width) || !is.numeric(height)) {
    stop("width and height must be numeric")
  } else if (any(width < 1) || any(height < 1)) {
    stop("width and height must be positive numbers")
  }

  w <- width(stimuli)
  h <- height(stimuli)

  if (keep_rels) {
    # resize all the same %
    if (isTRUE(crop)) {
      w_pcnt <- width / min(w)
      h_pcnt <- height / min(h)
      pcnt <- max(c(w_pcnt, h_pcnt))
    } else {
      w_pcnt <- width / max(w)
      h_pcnt <- height / max(h)
      pcnt <- min(c(w_pcnt, h_pcnt))
    }
  } else {
    # resize each to fit
    w_pcnt <- width / w
    h_pcnt <- height / h
    if (isTRUE(crop)) {
      pcnt <- mapply(max, w_pcnt, h_pcnt)
    } else {
      pcnt <- mapply(min, w_pcnt, h_pcnt)
    }
  }

  resized <- resize(stimuli, pcnt)

  crop(resized, width, height, fill = fill, patch = patch)
}
