#' Patch colour
#'
#' Get the median (or mean or user-defined function) colour value of a specified patch of pixels on an image. This is useful for matching background colours.
#'
#' @param img The image
#' @param x1 starting x pixel of patch
#' @param x2 ending x pixel of patch
#' @param y1 starting y pixel of patch
#' @param y2 ending y pixel of patch
#' @param color The type of color to return (hex, rgb)
#' @param func The function to apply to an array of rgba values to determine the central colour (defaults to median)
#'
#' @return hex or rgba color value
#' @export
#'
#' @examples
#' patch(demo_stim()[[1]]$img)
#'
patch <- function(img, x1 = 1, x2 = 10, y1 = 1, y2 = 10,
                  color = c("hex", "rgb"), func = stats::median) {
  if (!"magick-image" %in% class(img)) {
    stop("img must be 'magick-image'")
  }
  
  color <- match.arg(color)
  
  all_pixels <- magick::image_raster(img)
  selected_pixels <- (
    all_pixels$x >= min(x1, x2) &
    all_pixels$x <= max(x1, x2) &
    all_pixels$y >= min(y1, y2) &
    all_pixels$y <= max(y1, y2)
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
}
