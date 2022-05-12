#' Rotate templates and images
#'
#' @param stimuli list of class stimlist
#' @param degrees degrees to rotate
#' @param fill background color
#' @param patch whether to use the patch function to set the background color
#' @param keep_size whether to keep the original size or expand images to the new rotated size
#'
#' @return stimlist with rotated tems and/or images
#'
#' @export
#'
#' @examples
#' rotated <- demo_stim() %>%
#'   rotate(45, fill = "dodgerblue") %>%
#'   draw_tem()
#'
#' rotate_patch <- demo_stim() %>%
#'   rotate(45, patch = TRUE, keep_size = FALSE)
#'
rotate <- function(stimuli, degrees = 0,
                   fill = wm_opts("fill"), patch = FALSE,
                   keep_size = TRUE) {
  stimuli <- validate_stimlist(stimuli)
  n <- length(stimuli)

  degrees <- degrees %>%
    rep(length.out = length(stimuli)) %>%
    sapply(`%%`, 360)
  radians <- degrees * (pi/180)

  suppressWarnings({
    fill <- rep_len(fill, n)
    #patch <- rep_len(patch, n)
  })

  for (i in seq_along(stimuli)) {
    w <- stimuli[[i]]$width
    h <- stimuli[[i]]$height

    # rotate image ----
    if ("magick-image" %in% class(stimuli[[i]]$img)) {
      xm1 <- w/2
      ym1 <- h/2

      # set fill from patch
      if (isTRUE(patch)) {
        fill[i] <- patch(stimuli[[i]]$img)
      } else if (!isFALSE(patch)) {
        plist <- c(list(img = stimuli[[i]]$img), patch)
        fill[i] <- do.call("patch", plist)
      }

      rotimg <- stimuli[[i]]$img %>%
        magick::image_background(color = fill[i]) %>%
        magick::image_rotate(degrees[i])
      if (keep_size) {
        rotimg <- magick::image_crop(rotimg, magick::geometry_area(w, h))
      }
      stimuli[[i]]$img <- magick::image_repage(rotimg)
      info <- magick::image_info(stimuli[[i]]$img)
      xm2 <- info$width/2
      ym2 <- info$height/2
    } else if (!is.null(w) && !is.null(h)) {
      xm1 <- w/2
      ym1 <- h/2

      if (keep_size) {
        xm2 <- xm1
        ym2 <- ym1
      } else {
        rotsize <- rotated_size(w, h, degrees[i])
        xm2 <- rotsize$width/2
        ym2 <- rotsize$height/2
      }
    } else if (!is.null(stimuli[[i]]$points)) {
      # rotate around the centre of the points
      centre <- apply(stimuli[[i]]$points, 1, mean)
      xm1 <- centre[[1]]
      ym1 <- centre[[2]]

      if (keep_size) {
        xm2 <- xm1
        ym2 <- ym1
      } else {
        rotsize <- rotated_size(xm1*2, ym1*2, degrees[i])
        xm2 <- rotsize$width/2
        ym2 <- rotsize$height/2
      }
    }

    stimuli[[i]]$width = round(xm2*2)
    stimuli[[i]]$height = round(ym2*2)

    # rotate points ----
    if (!is.null(stimuli[[i]]$points)) {
      # Subtract original midpoints, rotate,
      # and add the new midpoints in the end again
      stimuli[[i]]$points <- apply(stimuli[[i]]$points, 2, function(pt) {
        crad <- cos(radians[i])
        srad <- sin(radians[i])
        x_offset <- pt[[1]] - xm1
        y_offset <- pt[[2]] - ym1
        xr = x_offset * crad - y_offset * srad + xm2
        yr = x_offset * srad + y_offset * crad + ym2

        c(x = xr, y = yr)
      })
    }
  }

  stimuli
}

#' Image size after rotation
#'
#' @param width Width of the original image
#' @param height Height of the original image
#' @param degrees Rotation in degreed
#'
#' @return list of rotated width and height
#' @export
#'
#' @examples
#' rotated_size(100, 100, 45)
rotated_size <- function(width, height, degrees) {
  degrees <- degrees %% 180

  if (degrees < 0) {
    degrees <- 180 + degrees
  }
  if (degrees >= 90) {
    tmpw <- width
    width <- height
    height <- tmpw
    degrees <- degrees - 90
  }

  radians <- degrees * pi / 180;
  w <- (width * cos(radians)) + (height * sin(radians))
  h <- (width * sin(radians)) + (height * cos(radians))

  list(
    width = w,
    height = h
  )
}


#' Make eyes horizontal
#'
#' @param stimuli list of class stimlist
#' @param left_eye The first point to align (defaults to 0)
#' @param right_eye The second point to align (defaults to 1)
#' @param fill background color to pass to rotate
#' @param patch whether to use the patch function to set the background color
#'
#' @return stimlist with rotated tems and/or images
#' @export
#'
#' @examples
#' demo_stim() %>% horiz_eyes()
#'
horiz_eyes <- function(stimuli, left_eye = 0, right_eye = 1, fill = wm_opts("fill"), patch = FALSE) {
  stimuli <- validate_stimlist(stimuli, TRUE)

  degrees <- lapply(stimuli, `[[`, "points") %>%
    lapply(function(pt) {
      x1 = pt[[1, left_eye+1]]
      y1 = pt[[2, left_eye+1]]
      x2 = pt[[1, right_eye+1]]
      y2 = pt[[2, right_eye+1]]
      rad <- atan2(y1 - y2, x1 - x2) %% (2*pi)
      180 - (rad / (pi/180))
  })

  stimuli %>%
    rotate(degrees = degrees, fill = fill,
           patch = patch, keep_size = TRUE)
}
