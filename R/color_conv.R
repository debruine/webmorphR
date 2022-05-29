#' Convert colors
#'
#' Convert from common color inputs to specified output type, adding alpha transparency for output formats that support it (hexa, rgba).
#'
#' @details
#' * color: one of the R colours listed in [grDevices::colors()], e.g., "red"
#' * hex: hexadecimal string, e.g., "#FF0000"
#' * hexa: hexadecimal string with alpha, e.g., "#FF0000FF"
#' * hex3: abbreviated hexadecimal string, e.g., "#F00"
#' * rgb: vector of red, green and blue values 0-255, e.g., c(255, 0, 0)
#' * rgba: vector of red, green, blue and alpha values 0-255, e.g., c(255, 0, 0, 255)
#' * lab: CIE-Lab color 
#' * hsv: vector of hue, saturation and value values (0-1), e.g., c(h=0, s = 1, v = 1)
#'
#' @param color A color in one of the input formats (see Details)
#' @param alpha Alpha transparency (values <=1 converted to 0-255); ignored if color has alpha already
#' @param from,to Input and output color spaces, see `Details` below.
#'
#' @return color in \code{to} format
#' @export
#' @family color
#'
#' @examples
#' # different ways to input red
#' color_conv("red")
#' color_conv("#FF0000")
#' color_conv("#FF0000FF")
#' color_conv(c(255,0,0))
#' color_conv("rgb(255,0,0)") # you can use CSS-style text
#' color_conv(c(255,0,0,255))
#' 
#' # Lab must have names or use text format to be guessed
#' color_conv(c(l = 53.2, a = 80.1, b = 67.2)) 
#' color_conv("lab(53.2,80.1,67.2)")
#' 
#' # else, it will be guessed as rgb; fix by setting from explicitly
#' color_conv(c(53.2, 80.1, 67.2))
#' color_conv(c(53.2, 80.1, 67.2), from = "lab")
#'
#' # add 50% alpha transparency to dodgerblue
#' color_conv("dodgerblue", alpha = 0.5, to = "rgba")
#'
color_conv <- function(color, alpha = 1,
                       from = c("guess", "col", "hex", "hexa", "hex3", "rgb", "rgba", "lab"),
                       to = c("hexa", "hex", "rgba", "rgb", "lab", "hsv")) {
  # handle NULL and NA
  if (is.null(color) || any(is.na(color))) {
    return(NULL)
  } else if (any(color == "none")) {
    return("none")
  }
  
  from <- match.arg(from)
  to <- match.arg(to)

  # guess colour type ----
  if (from == "guess") {
    from <- dplyr::case_when(
      length(color) == 3 && !is.null(names(color)) && 
        all(tolower(names(color)) %in% c("l", "a", "b")) ~ "lab",
      length(color) == 3 && all(color <= 255) && all(color>=0) ~ "rgb",
      length(color) == 4 ~ "rgba",
      all(grepl("^rgb\\(.*\\)$", color)) ~ "rgb",
      all(grepl("^rgba\\(.*\\)$", color)) ~ "rgba",
      all(color %in% colors()) ~ "col",
      all(grepl("^#[0-9A-Fa-f]{6}$", color)) ~ "hex",
      all(grepl("^#[0-9A-Fa-f]{8}$", color)) ~ "hexa",
      all(grepl("^#[0-9A-Fa-f]{3}$", color)) ~ "hex3",
      all(grepl("^lab\\(.*\\)$", color, ignore.case = TRUE)) ~ "lab",
      NULL
    )
  }

  if (from %in% c("rgb", "rgba", "lab") && is.character(color)) {
    from <- paste0(from, "_text")
  }

  if (is.null(from) || is.na(from)) {
    stop("The color format could not be guessed.")
  }

  # convert alpha to integer between 0 and 255
  alpha255 <- ifelse(alpha > 1, alpha, alpha * 255) |>
    round() |> pmax(0) |> pmin(255) |>
    .subset2(1) # remove if vectorised, currently only 1 alpha

  # convert to rgba ----
  rgba <- switch(
    from,
    rgba = color,
    rgb = c(color, alpha255),
    lab = lab2rgb(color) |> c(alpha = 255),
    rgb_text = gsub("(rgb|\\(|\\)|\\s)", "", color) |>
      strsplit(",") |> .subset2(1) |>
      as.numeric() |> c(alpha255),
    lab_text = gsub("(lab|\\(|\\)|\\s)", "", color, ignore.case = TRUE) |>
      strsplit(",") |> .subset2(1) |>
      as.numeric() |> lab2rgb() |> c(alpha255),
    rgba_text = gsub("(rgba|\\(|\\)|\\s)", "", color) |>
      strsplit(",") |> .subset2(1) |>
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
  } else if (to == "lab") {
    hex <- rgba[1:3] |> format.hexmode(width = 2) |> paste(collapse = "")
    col2lab(paste0("#", hex))
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
