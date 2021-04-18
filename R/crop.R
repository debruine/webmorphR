#' Crop images and templates
#'
#' Remove or add margins to images and templates. Width, height, x_off and y_off can be set in pixels (> 2.0) or proportions (between 0.0 and 2.0). Cropping is anchored at the image center (or calculated template centroid if there is no image) unless x_off or y_off are set.
#'
#' Fill can be set to R color names (see `colors()`) or valid hex or rgb values. Patch can be set to TRUE (defaults to median color of top left 10-pixel square) or a list of arguments to the function `patch()` to set background from a patch of the image.
#'
#' @param stimuli list of class stimlist
#' @param width width of cropped image in pixels or % (<5)
#' @param height height of cropped image in pixels or % (<5)
#' @param x_off x-offset in pixels or % (<1) (NULL horizontally centers cropped image)
#' @param y_off y-offset in pixels or % (<1) (NULL vertically centers cropped image)
#' @param fill background color if cropping goes outside the original image
#' @param patch whether to use the patch function to set the background color
#'
#' @return stimlist with cropped tems and/or images
#' @export
#'
#' @examples
#' stimuli <- demo_stim()
#'
#' # crop to 60% width and 80% height (centered)
#' stim_centered <- crop(stimuli, width = .60, height = .80)
#'
#' # crop to upper right quadrant
#' urq <- crop(stimuli, .5, .5, x_off = .5, y_off = 0)
#'
#' # take median color from a patch
#' bigger <- crop(stimuli, 1.2, 1.2, patch = c(100, 200, 1, 10))
#'
crop <- function(stimuli,
                 width = 1.0, height = 1.0,
                 x_off = NULL, y_off = NULL,
                 fill = wm_opts("fill"), patch = FALSE) {
  stimuli <- validate_stimlist(stimuli)

  suppressWarnings({
    width <- rep(width, length.out = length(stimuli))
    height <- rep(height, length.out = length(stimuli))
    x_off <- rep(x_off, length.out = length(stimuli))
    y_off <- rep(y_off, length.out = length(stimuli))
    fill <- rep(fill, length.out = length(stimuli))
    #patch <- rep(patch, length.out = length(stimuli))
  })

  for (i in seq_along(stimuli)) {
    origw <- stimuli[[i]]$width
    origh <- stimuli[[i]]$height
    w <- width[i] %||% origw
    h <- height[i] %||% origh

    # handle percentages
    if (w <= 5) w <- w * origw
    if (h <= 5) h <- h * origh

    # handle percentage offsets
    if (!is.null(x_off[i]) && !is.na(x_off[i]) && abs(x_off[i]) <= 1)
      x_off[i] <- x_off[i] * origw
    if (!is.null(y_off[i]) && !is.na(y_off[i]) && abs(y_off[i]) <= 1)
      y_off[i] <- y_off[i] * origh

    # null offsets split the remainder between orig and new dimensions
    if (is.null(x_off[i]) || is.na(x_off[i])) x_off[i] <- (origw - w)/2
    if (is.null(y_off[i]) || is.na(y_off[i])) y_off[i] <- (origh - h)/2

    stimuli[[i]]$width <- w
    stimuli[[i]]$height <- h

    if (!is.null(stimuli[[i]]$img) &&
        class(stimuli[[i]]$img) == "magick-image") {
      # crop doesn't handle negative offsets well
      ga <- magick::geometry_area(
        width = min(w, origw),
        height = min(h, origh),
        x_off = max(0, x_off[i]),
        y_off = max(0, y_off[i])
      )

      newimg <- magick::image_crop(
        image = stimuli[[i]]$img,
        geometry = ga,
        gravity = "NorthWest"
      )

      # set fill from patch
      if (isTRUE(patch)) {
        fill[i] <- patch(stimuli[[i]]$img)
      } else if (!isFALSE(patch)) {
        plist <- c(list(img = stimuli[[i]]$img), patch)
        fill[i] <- do.call("patch", plist)
      }

      # make background image with fill
      bg <- magick::image_blank(w, h, color = fill[i])
      offset <- magick::geometry_point(
        x = max(0, -x_off[i]),
        y = max(0, -y_off[i])
      )

      stimuli[[i]]$img <- magick::image_composite(
        image = bg,
        composite_image = newimg,
        offset = offset
      )
    }

    if (!is.null(stimuli[[i]]$points)) {
      stimuli[[i]]$points <- apply(stimuli[[i]]$points, 2, function(pt) {
        pt - c(x_off[i], y_off[i])
      })
    }
  }

  stimuli
}

#' Pad images
#'
#' Convenience function to calcuate offsets for crop
#'
#' @param stimuli list of class stimlist
#' @param top number of pixels to pad the top
#' @param right number of pixels to pad the right side
#' @param bottom number of pixels to pad the bottom
#' @param left number of pixels to pad the left side
#' @param ... additional arguments to pass to \code{\link{crop}}
#'
#' @return list of class stimlist
#' @export
#'
#' @examples
#' padded <- demo_stim() %>% pad(10, fill = "red")
#' plot(padded)
pad <- function(stimuli, top = 10, right = top, bottom = top, left = right, ...) {
  stimuli <- validate_stimlist(stimuli)

  # convert if percents
  top    <- ifelse(abs(top) < 1,    top * height(stimuli),    top)
  bottom <- ifelse(abs(bottom) < 1, bottom * height(stimuli), bottom)
  left   <- ifelse(abs(left) < 1,   left * width(stimuli),    left)
  right  <- ifelse(abs(right) < 1,  right * width(stimuli),   right)


  stimuli %>%
    crop(width = width(stimuli) + left + right,
         height = height(stimuli) + top + bottom,
         x_off = - left, y_off = - top, ...
    )
}


#' Crop to template boundaries and pad
#'
#' Calculate the max and min x and y coordinates across the stimuli and crop all image to this plus padding
#'
#' @param stimuli list of class stimlist
#' @param top number of pixels to pad the top
#' @param right number of pixels to pad the right side
#' @param bottom number of pixels to pad the bottom
#' @param left number of pixels to pad the left side
#' @param each Whether to calculate bounds for the full set (default) or each image separately
#' @param ... additional arguments to pass to \code{\link{crop}}
#'
#' @return list of class stimlist
#' @export
#'
#' @examples
#' ctem <- demo_stim() %>% crop_tem(20) %>% draw_tem()
#' plot(ctem)
crop_tem <- function(stimuli, top = 10, right = top, bottom = top, left = right, each = FALSE, ...) {
  stimuli <- validate_stimlist(stimuli, TRUE)

  b <- bounds(stimuli, each = each)

  stimuli %>%
    crop(width = ceiling(b$max_x - b$min_x + left + right),
         height = ceiling(b$max_y - b$min_y + top + bottom),
         x_off = floor(b$min_x - left),
         y_off = floor(b$min_y - top), ...
    )
}


#' Get template bounds
#'
#' @param stimuli A stimlist
#' @param each Whether to calculate max and min for the full set (default) or each image separately
#'
#' @return A list of min and max x and y values
#' @export
#'
#' @examples
#' demo_stim() %>% bounds() %>% str()
#'
#' demo_stim() %>% bounds(each = TRUE)
bounds <- function(stimuli, each = FALSE) {
  stimuli <- validate_stimlist(stimuli, TRUE)

  if (isTRUE(each)) {
    # get separate bounds for each stimulus
    b <- sapply(stimuli, bounds) %>%
      t() %>%
      as.data.frame() %>%
      tidyr::unnest(cols = c("min_x", "max_x", "min_y", "max_y")) %>%
      as.data.frame()
    rownames(b) <- names(stimuli)
    return(b)
  }

  A <- tems_to_array(stimuli)
  x <- (A[, "X", ])
  y <- (A[, "Y", ]) * -1

  list(min_x = min(x),
       max_x = max(x),
       min_y = min(y),
       max_y = max(y))
}


#' Squash Template Points
#' 
#' Move template points that are outside the image boundaries (e.g., negative values or larger than image width or height) to the borders of the image.
#'
#' @param stimuli list of class stimlist
#'
#' @return list of class stimlist
#' @export
#'
#' @examples
#' nosquash <- demo_stim()[1] %>% 
#'   crop(0.4, 0.5) %>% pad(50) %>% 
#'   draw_tem(line.size = 3)
#' 
#' squashed <- demo_stim()[1] %>% 
#'   crop(0.4, 0.5) %>% squash_tem() %>% pad(50) %>% 
#'   draw_tem(line.size = 3)
#'   
#' # c(nosquash, squashed) %>% plot()
squash_tem <- function(stimuli) {
  stimuli <- validate_stimlist(stimuli, TRUE)
  
  for (i in seq_along(stimuli)) {
    if (!is.null(stimuli[[i]]$points)) {
      w <- stimuli[[i]]$width
      h <- stimuli[[i]]$height
      
      stimuli[[i]]$points <- apply(stimuli[[i]]$points, 2, function(pt) {
        # move points into image boundaries
        pt %>%
          pmax(c(0, 0)) %>%
          pmin(c(w-1, h-1)) # subtract 1 for 0-vs 1-based origin
      })
    }
  }
  
  stimuli
}
