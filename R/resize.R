#' Resize templates and images
#'
#' @param stimuli list of class stimlist
#' @param width new width (in pixels or percent if < 10)
#' @param height new height (in pixels or percent if < 10)
#'
#' @return stimlist with resized tems and/or images
#' @export
#'
#' @examples
#' resized <- demo_stim() |>
#'   resize(.5, .75) |>
#'   draw_tem()
#'
resize <- function(stimuli, width = NULL, height = NULL) {
  stimuli <- validate_stimlist(stimuli)

  if (is.null(width)) width <- 0
  if (is.null(height)) height <- 0

  if (all(width == 0) && all(height == 0)) {
    return(stimuli)
  } else if (any(width < 0)) {
    stop("width must be a positive number")
  } else if (any(height < 0)) {
    stop("height must be a positive number")
  }

  n <- length(stimuli)
  width <- rep_len(width, n)
  height <- rep_len(height, n)

  for (i in seq_along(stimuli)) {
    # express height and/or width as % and fill empty value

    if (width[i] == 0) {
      # check height first
    } else if (width[i] <= 10) { # percentage
      w <-   width[i]
    } else if (!is.null(width[i])) { # pixels
      w <- width[i]/stimuli[[i]]$width
    }

    if (height[i] == 0) {
      h <- w
    } else if (height[i] <= 10) { # percentage
      h <-  height[i]
    } else { # pixels
      h <- height[i]/stimuli[[i]]$height
    }

    if (width[i] == 0) w <- h

    # resize template
    if (!is.null(stimuli[[i]]$points)) {
      stimuli[[i]]$points <- stimuli[[i]]$points * c(w, h)
    }

    # calculate new dimensions
    stimuli[[i]]$width <- round(stimuli[[i]]$width*w)
    stimuli[[i]]$height <- round(stimuli[[i]]$height*h)

    if ("magick-image" %in% class(stimuli[[i]]$img)) {
      # resize image
      stimuli[[i]]$img <- magick::image_resize(
        stimuli[[i]]$img,
        #magick::geometry_size_percent(w*100, h*100)
        magick::geometry_size_pixels(stimuli[[i]]$width,
                                     stimuli[[i]]$height,
                                     preserve_aspect = FALSE)
      )

      # make sure dimensions are consistent with image
      info <- magick::image_info(stimuli[[i]]$img)
      stimuli[[i]]$width <- info$width
      stimuli[[i]]$height <- info$height
    }
  }

  stimuli
}
