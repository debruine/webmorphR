#' Resize and crop/pad images to a specified size
#'
#' @param stimuli list of stimuli
#' @param width the target width (if null, the maximum stimulus width is used)
#' @param height the target height (if null, the maximum stimulus height is used)
#' @param fill background color if cropping goes outside the original image, see [color_conv()]
#' @param crop whether to crop or pad images to make them the specified size
#' @param keep_rels whether to keep the size relationships between images in the set, or make all the maximum size
#'
#' @return list of stimuli with cropped tems and/or images
#' @export
#' @family manipulators
#'
#' @examples
#'
#' # images with different aspect ratios and sizes
#' stimuli <- demo_unstandard(c(1:4, 6:9))
#'
#' to_size(stimuli, 200, 200, fill = "dodgerblue")
#'
to_size <- function(stimuli, width = NULL, height = NULL,
                    fill = wm_opts("fill"),
                    crop = FALSE, keep_rels = FALSE) {
  stimuli <- as_stimlist(stimuli)
  
  # process width and height
  # set to maximum width and height in set
  if (is.null(width)) width <- width(stimuli, "max")
  if (is.null(height)) height <- height(stimuli, "max")

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

  crop(resized, width, height, fill = fill)
}
