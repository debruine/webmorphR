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
  # handle NULL and NA
  if (is.null(color) || any(is.na(color))) {
    return(NULL)
  }
  
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

  if (is.null(from) || is.na(from)) {
    stop("The color format could not be guessed.")
  }

  # convert alpha to integer between 0 and 255
  alpha255 <- ifelse(alpha > 1, alpha, alpha * 255) |>
    round() |> pmax(0) |> pmin(255) |>
    .bb(1) # remove if vectorised, currently only 1 alpha

  # convert to rgba ----
  rgba <- switch(
    from,
    rgba = color,
    rgb = c(color, alpha255),
    rgb_text = gsub("(rgb|\\(|\\)|\\s)", "", color) |>
      strsplit(",") |> .bb(1) |>
      as.numeric() |> c(alpha255),
    rgba_text = gsub("(rgba|\\(|\\)|\\s)", "", color) |>
      strsplit(",") |> .bb(1) |>
      as.numeric(),
    col = grDevices::col2rgb(color) |> c(alpha255),
    hex = grDevices::col2rgb(color) |> c(alpha255),
    hex3 = strsplit(color, "")[[1]] |>
      rep(each = 2) |>
      paste(collapse = "") |>
      substr(2,8) |> 
      grDevices::col2rgb() |>
      c(alpha255),
    hexa = grDevices::col2rgb(color, alpha = TRUE)
  )

  # convert to output
  if (to == "rgba") {
    rgba[1:4]
  } else if (to == "rgb") {
    rgba[1:3]
  } else if (to == "hsv") {
    (grDevices::rgb2hsv(rgba[1:3]) |> t())[1,]
  } else if (to == "hex") {
    hex <- rgba[1:3] |>
      format.hexmode(width = 2) |>
      paste(collapse = "")
    
    paste0("#", hex)
  } else if (to == "hexa") {
    hex <- rgba[1:4] |> 
      format.hexmode(width = 2) |> 
      paste(collapse = "")
    
    paste0("#", hex)
  }
}
