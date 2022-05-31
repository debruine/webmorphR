#' Rotate templates and images
#'
#' @param stimuli list of stimuli
#' @param degrees degrees to rotate
#' @param fill background color, see [color_conv()]
#' @param keep_size whether to keep the original size or expand images to the new rotated size
#' @param origin The origin of the rotation. Options are:
#'    `"image"` will rotate around the image center. 
#'    
#'    `"tem"` will rotate around the average of all template coordinates.
#'    
#'    A vector of 1 or more point indices (0-based) will rotate around their average position. 
#'
#' @return list of stimuli with rotated tems and/or images
#'
#' @export
#' @family manipulators
#'
#' @examples
#' stimuli <- demo_stim() |> resize(0.5)
#' 
#' rotate(stimuli, 45, fill = "dodgerblue")
#' rotate(stimuli, 45, fill = "dodgerblue", keep_size = FALSE)
#'   
#' \donttest{
#' # if images are not in the centre of the image,
#' # try setting the origin to tem or specific point(s)
#' offset <- stimuli[1] |> 
#'   draw_tem() |> 
#'   pad(0, 250, 0, 0, fill = "dodgerblue")
#'   
#' rotate(offset, 45, origin = "image", fill = "pink")
#' rotate(offset, 45, origin = "tem", fill = "pink")
#' 
#' # rotate around point 0 (left eye)
#' offset |> crop_tem() |> rep(8) |>
#'   rotate(seq(0, 325, 45), origin = 0, fill = "pink") |>
#'   animate(fps = 5)
#' }
#'
rotate <- function(stimuli, degrees = 0,
                   fill = wm_opts("fill"),
                   keep_size = TRUE,
                   origin = "image") {
  stimuli <- as_stimlist(stimuli)
  orig_w <- width(stimuli)
  orig_h <- height(stimuli)

  degrees <- degrees |>
    rep(length.out = length(stimuli)) |>
    sapply(`%%`, 360)
  radians <- degrees * (pi/180)

  fill <- sapply(fill, color_conv)
  suppressWarnings({
    l <- length(stimuli)
    fill <- rep_len(fill, l)
  })

  for (i in seq_along(stimuli)) {
    w <- stimuli[[i]]$width
    h <- stimuli[[i]]$height
    
    # calculate xm1, ym1 ----
    if ((keep_size && origin == "tem") || 
        is.null(w) || is.null(h)) {
      ct <- centroid(stimuli[[i]])
      xm1 <- ct[1, 'x']
      ym1 <- ct[1, 'y']
    } else if (!keep_size || origin == "image") {
      xm1 <- w/2
      ym1 <- h/2
    } else if (is.numeric(origin)) {
      ct <- centroid(stimuli[[i]], points = origin)
      xm1 <- ct[1, 'x']
      ym1 <- ct[1, 'y']
    }
    
    # calculate rotsize ----
    if (keep_size) {
      rotsize <- list(width = w, height = h)
      xm2 <- xm1
      ym2 <- ym1
    } else {
      w <- w %||% (2*xm1)
      h <- h %||% (2*ym1)
      rotsize <- rotated_size(w, h, degrees[i]) |> lapply(ceiling)
      xm2 <- rotsize$width/2
      ym2 <- rotsize$height/2
    }

    # rotate image ----
    if ("magick-image" %in% class(stimuli[[i]]$img)) {
      # centre image on xm1, ym1
      if ((xm1 != w/2 || ym1 != h/2) && keep_size) {
        new_w <- ceiling(max(xm1, w-xm1) * 2)
        new_h <- ceiling(max(ym1, h-ym1) * 2)
        x_off <- ifelse(xm1 < w-xm1, w - new_w, 0) |> round()
        y_off <- ifelse(ym1 < h-ym1, h - new_h, 0) |> round()
        centred_img <- crop(stimuli[[i]], 
                            new_w, new_h, 
                            x_off, y_off, 
                            fill = fill[i])
      } else {
        centred_img <- stimuli[i]
      }
      
      # rotate on center
      rotimg <- centred_img[[1]]$img |>
        magick::image_background(color = "none") |>
        magick::image_rotate(degrees[i])
      stimuli[[i]]$img <- magick::image_repage(rotimg)
      
      # crop back to right size
      # get center of new img to xm2,ym2 at rotsize
      info <- magick::image_info(stimuli[[i]]$img)
      stimuli[[i]]$width <- info$width
      stimuli[[i]]$height <- info$height
      if (!keep_size) {
        x_off <- NULL
        y_off <- NULL
      } else {
        rot_xm <- info$width/2
        rot_ym <- info$height/2
        x_off <- round(rot_xm - xm2)
        y_off <- round(rot_ym - ym2)
      }
      
      uncropimg <- crop(stimuli[[i]], 
                        rotsize$width, 
                        rotsize$height, 
                        x_off, y_off, 
                        fill = fill[i])
      
      stimuli[[i]]$img <- uncropimg[[1]]$img
      
      stimuli[[i]]$width = rotsize$width
      stimuli[[i]]$height = rotsize$height
    }
    
    

    # rotate points ----
    if (!is.null(stimuli[[i]]$points)) {
      pt <- stimuli[[i]]$points
      offset <- pt - c(xm1, ym1)
      crad <- cos(radians[i]) * offset
      srad <- sin(radians[i]) * offset
      xr <- crad[1,] - srad[2,] + xm2
      yr <- srad[1,] + crad[2,] + ym2
      stimuli[[i]]$points <- matrix(c(xr, yr), 2, 
                                    byrow = TRUE, 
                                    dimnames = dimnames(pt))
    }
  }
  
  # if (keep_size) {
  #   stimuli <- crop(stimuli, orig_w, orig_h, fill = fill)
  # }

  stimuli
}

#' Image size after rotation
#'
#' @param width Width of the original image
#' @param height Height of the original image
#' @param degrees Rotation in degreed
#'
#' @return list of rotated width and height
#' @keywords internal
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


