#' Patch colour
#'
#' Get the median (or mean or user-defined function) colour value of a specified patch of pixels on an image. This is useful for matching background colours.
#'
#' @param stimuli A stim or list of class stimlist
#' @param x1,x2,y1,y2 start and end pixels of the patch, if <=1, interpreted as proportions of width or height
#' @param color The type of color to return (hex, rgb)
#' @param func The function to apply to an array of rgba values to determine the central colour (defaults to median, but mean, min, or max are also useful)
#'
#' @return a vector of hex or rgba color values
#' @export
#'
#' @examples
#' # get colour from the upper left corder
#' demo_stim() |> patch()
#' 
#' # get median colour from centre .1 width pixels
#' demo_stim() |> patch(x1 = .45, x2 = .55, y1 = .45, y2 = .55)
#' 
#' # get mean rgb colour from a 10-pixel strip across the top of the image
#' demo_stim() |> patch(0, 1, 0, 10, color = "rgb", func = mean)
#'
patch <- function(stimuli, x1 = 0, x2 = 10, y1 = 0, y2 = 10,
                  color = c("hex", "rgb"), func = stats::median) {
  stimuli <- validate_stimlist(stimuli)
  
  color <- match.arg(color)
  
  l <- length(stimuli)
  x1 <- rep_len(x1, l)
  x2 <- rep_len(x2, l)
  y1 <- rep_len(y1, l)
  y2 <- rep_len(y2, l)
  
  # handle proportions
  x1 <- ifelse(x1 <= 1, x1 * width(stimuli), x1)
  x2 <- ifelse(x2 <= 1, x2 * width(stimuli), x2)
  y1 <- ifelse(y1 <= 1, y1 * height(stimuli), y1)
  y2 <- ifelse(y2 <= 1, y2 * height(stimuli), y2)
  
  
  patches <- lapply(seq_along(stimuli), function(i) {
    all_pixels <- magick::image_raster(stimuli[[i]]$img)
    selected_pixels <- (
      all_pixels$x >= min(x1[i], x2[i]) &
      all_pixels$x <= max(x1[i], x2[i]) &
      all_pixels$y >= min(y1[i], y2[i]) &
      all_pixels$y <= max(y1[i], y2[i])
    )
    pixels <- all_pixels[selected_pixels, ]
    
    central_col <- grDevices::col2rgb(pixels$col, alpha = TRUE) |>
      apply(1, func)
    
    if (color == "rgb") {
      return(central_col)
    }
    
    # return hex value
    grDevices::rgb(
      central_col[['red']],
      central_col[['green']],
      central_col[['blue']],
      central_col[['alpha']],
      maxColorValue = 255
    )
  })
  
  names(patches) <- names(stimuli)
  
  if (color == "hex") { patches <- unlist(patches) }
  patches
}
