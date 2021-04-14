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

  pixels <- magick::image_raster(img) %>%
    dplyr::filter(.data$x >= x1,
                  .data$x <= x2,
                  .data$y >= y1,
                  .data$y <= y2)

  central_col <- grDevices::col2rgb(pixels$col, alpha = TRUE) %>%
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


#' Color to Lab Conversion
#'
#' R color to LAB colourspace conversion.
#'
#' @param col vector of hex or color names
#'
#' @return list of L, a and b values
#' @export
#'
#' @examples
#' col2lab(c("red", "green", "blue"))
#'
col2lab <- function(col) {
  # checked at http://www.brucelindbloom.com/index.html?ColorCheckerCalcHelp.html

  # RGB to XYZ via http://www.easyrgb.com/index.php?X=MATH&H=02#text2

  #change to 0-1
  rgb <- grDevices::col2rgb(col)/255

  # inverse sRGB companding
  rgb2 <- apply(rgb, 1, function(v) {
    100 * ifelse( v > 0.04045,
                  `^`( (( v + 0.055 ) / 1.055 ), 2.4),
                  v / 12.92
    )
  }) %>% matrix(nrow = ncol(rgb)) # for 1-pixel images

  # Observer. = 2°, Illuminant = D65
  X = rgb2[, 1] * 0.4124 + rgb2[, 2] * 0.3576 + rgb2[, 3] * 0.1805
  Y = rgb2[, 1] * 0.2126 + rgb2[, 2] * 0.7152 + rgb2[, 3] * 0.0722
  Z = rgb2[, 1] * 0.0193 + rgb2[, 2] * 0.1192 + rgb2[, 3] * 0.9505

  # XYZ to CieL*ab via http://www.easyrgb.com/index.php?X=MATH&H=07#text7


  # Observer= 2°, Illuminant= D65
  #see http://www.brucelindbloom.com/index.html?ColorCheckerCalcHelp.html for other values
  ref_X =  95.047
  ref_Y = 100.000
  ref_Z = 108.883

  xyz <- list(
    'x' = X / ref_X,
    'y' = Y / ref_Y,
    'z' = Z / ref_Z
  ) %>% lapply(function(v) {
    ifelse( v > 0.008856,
            `^`(v, 1/3 ),
            (7.787 * v) + (16 / 116))
  })

  list(
    L = ( 116 * xyz$y ) - 16,
    a = 500 * ( xyz$x - xyz$y ),
    b = 200 * ( xyz$y - xyz$z )
  )
}



#' Convert colors
#'
#' Convert from common color inputs to specified output type, adding alpha transparency.
#'
#' color: one of the R colours listed in c\code{colors()}, e.g., "red"
#' hex: hexadecimal string, e.g., "#FF0000"
#' hexa: hexadecimal string with alpha, e.g., "#FF0000FF"
#' hex3: abbreviated hexadecimal string, e.g., "#F00"
#' rgb: vector of red, green and blue values 0-255, e.g., c(255, 0, 0)
#' rgb: vector of red, green, blue and alpha values 0-255, e.g., c(255, 0, 0, 255)
#' hsv: vector of hue, saturation and value values (0-1), e.g., c(h=0, s = 1, v = 1)
#'
#' @param color A color in one of the input formats (see Details)
#' @param alpha Alpha transparency (values <=1 converted to 0-255); ignored if color has alpha already
#' @param from,to Input and output color spaces, see `Details` below.
#'
#' @return color in \code{to} format
#' @export
#'
#' @examples
#' color_conv("red")
#' color_conv("#FF0000")
#' color_conv("#FF0000FF")
#' color_conv(c(255,0,0))
#' color_conv(c(255,0,0,255))
#'
#' color_conv("dodgerblue", 0.5, to = "rgba")
#'
color_conv <- function(color, alpha = 1,
                       from = c("guess", "col", "hex", "hexa", "hex3", "rgb", "rgba"),
                       to = c("hexa", "hex", "rgba", "rgb", "hsv")) {
  from <- match.arg(from)
  to <- match.arg(to)

  # guess colour type ----
  if (from == "guess") {
    from <- dplyr::case_when(
      length(color) == 3 && all(color <= 255) && all(color>=0) ~ "rgb",
      length(color) == 4 ~ "rgba",
      all(grepl("^rgb\\(.*\\)$", color)) ~ "rgb",
      all(grepl("^rgba\\(.*\\)$", color)) ~ "rgba",
      all(color %in% colors()) ~ "col",
      all(grepl("^#[0-9A-Fa-f]{6}$", color)) ~ "hex",
      all(grepl("^#[0-9A-Fa-f]{8}$", color)) ~ "hexa",
      all(grepl("^#[0-9A-Fa-f]{3}$", color)) ~ "hex3",
      NULL
    )
  }

  if (from %in% c("rgb", "rgba") && is.character(color)) {
    from <- paste0(from, "_text")
  }

  if (is.null(from)) {
    stop("The color format could not be guessed.")
  }

  # convert alpha to integer between 0 and 255
  alpha255 <- ifelse(alpha > 1, alpha, alpha * 255) %>%
    round() %>% pmax(0) %>% pmin(255) %>%
    `[[`(1) # remove if vectorised, currently only 1 alpha

  # convert to rgba ----
  rgba <- switch(
    from,
    rgba = color,
    rgb = c(color, alpha255),
    rgb_text = gsub("(rgb|\\(|\\)|\\s)", "", color) %>%
      strsplit(",") %>% `[[`(1) %>%
      as.numeric() %>% c(., alpha255),
    rgba_text = gsub("(rgba|\\(|\\)|\\s)", "", color) %>%
      strsplit(",") %>% `[[`(1) %>%
      as.numeric(),
    col = grDevices::col2rgb(color) %>% c(., alpha255),
    hex = grDevices::col2rgb(color) %>% c(., alpha255),
    hex3 = strsplit(color, "")[[1]] %>%
      rep(each = 2) %>%
      paste(collapse = "") %>%
      substr(2,8) %>%
      grDevices::col2rgb() %>%
      c(., alpha255),
    hexa = grDevices::col2rgb(color, alpha = TRUE)
  )

  # convert to output
  if (to == "rgba") {
    rgba[1:4]
  } else if (to == "rgb") {
    rgba[1:3]
  } else if (to == "hsv") {
    (grDevices::rgb2hsv(rgba[1:3]) %>% t())[1,]
  } else if (to == "hex") {
    c(rgba[1:3], 255) %>% # add 255 to make sure <10 is padded
      as.hexmode() %>% as.character() %>% `[`(1:3) %>%
      paste(collapse = "") %>% paste0("#", .)
  } else if (to == "hexa") {
    c(rgba[1:4], 255)  %>% # add 255 to make sure <10 is padded
      as.hexmode() %>% as.character() %>% `[`(1:4) %>%
      paste(collapse = "") %>% paste0("#", .)
  }
}
