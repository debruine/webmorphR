#' Patch colour
#'
#' Get the median (or mean or user-defined function) colour value of a specified patch of pixels on an image. This is useful for matching background colours.
#' 
#' @details The colour values of each pixel in the patch are converted to CIE-Lab values before using the func to calculate the central tendency of the L (lightness), a (red-green axis) and b (blue-yellow axis); see [col2lab()] and [lab2rgb()] for more details.
#' 
#' This excludes transparent pixels, and returns "transparent" if all pixels in the patch are transparent.
#'
#' @param stimuli list of stimuli
#' @param width,height dimensions of the patch in pixels, if <=1, interpreted as proportions of width or height
#' @param x_off,y_off offset in pixels or proportion (<1)
#' @param color The type of color to return (hex, rgb)
#' @param func The function to apply to an array of L*ab color values to determine the central colour (defaults to median, but mean, min, or max can also be useful)
#'
#' @return a vector of hex or rgba color values
#' @export
#'
#' @examples
#' stimuli <- demo_stim()
#' 
#' # get colour from the upper left corder
#' patch(stimuli)
#' 
#' # get median colour from centre .1 width pixels
#' patch(stimuli, width = .1, height = .1, 
#'       x_off = .45, y_off = .45)
#' 
#' # get mean rgb colour from full image
#' patch(stimuli, width = 1, height = 1, 
#'       color = "rgb", func = mean)
#'
patch <- function(stimuli, width = 10, height = 10, x_off = 0, y_off = 0,
                  color = c("hex", "rgb"), func = stats::median) {
  stimuli <- as_stimlist(stimuli)
  
  color <- match.arg(color)
  
  l <- length(stimuli)
  w <- rep_len(width, l)
  h <- rep_len(height, l)
  x <- rep_len(x_off, l)
  y <- rep_len(y_off, l)
  
  # handle proportions
  w <- ifelse(w <= 1, w * width(stimuli), w) |> round()
  h <- ifelse(h <= 1, h * width(stimuli), h) |> round()
  x <- ifelse(x < 1, x * height(stimuli), x) |> round()
  y <- ifelse(y < 1, y * height(stimuli), y) |> round()
  
  patches <- lapply(seq_along(stimuli), function(i) {
    # crop image and get pixels
    ga <- magick::geometry_area(w[i], h[i], x[i], y[i])
    cropped <- magick::image_crop(stimuli[[i]]$img, ga, repage = TRUE)
    pixels <- magick::image_raster(cropped)
    
    # remove transparent pixels
    pixels <- pixels[pixels$col != "transparent", ]
    if (nrow(pixels) == 0) {
      return("transparent")
    }
    
    # convert to Lab and get func
    # prevent calling col2lab more than necessary
    unique_col <- dplyr::count(pixels, col)
    lab <- sapply(unique_col$col, col2lab)
    mult <- apply(lab, 1, rep, times = unique_col$n, 
                  simplify = FALSE)
    avg_lab <- sapply(mult, func)
    central_col <- lab2rgb(avg_lab)
    central_col[['alpha']] <- paste0('0x', substr(pixels$col, 8, 9))|> strtoi() |> func()

    if (color == "rgb") {
      central_col
    } else {
      # get hex value
      grDevices::rgb(
        central_col[['red']],
        central_col[['green']],
        central_col[['blue']],
        central_col[['alpha']],
        maxColorValue = 255
      )
    }
  })
  
  names(patches) <- names(stimuli)
  
  if (color == "hex") { patches <- unlist(patches) }
  patches
}
