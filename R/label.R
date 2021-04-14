#' Add a label
#'
#' This is just a wrapper function for magick::image_annotate that allows more flexibility in color input.
#'
#' @param stimuli list of class stimlist
#' @param text a vector of the label text(s) or TRUE to use stimlist names
#' @param color a vector of the label colour(s)
#' @param gravity string with gravity value from \code{magick::gravity_types}.
#' @param location geometry string with location relative to gravity
#' @param degrees rotates text around center point
#' @param size font size in pixels (if NULL, scales to 5% image height)
#' @param font string with font family such as "sans", "mono", "serif", "Times", "Helvetica", "Trebuchet", "Georgia", "Palatino" or "Comic Sans".
#' @param style value of style_types for example "italic"
#' @param weight thickness of the font, 400 is normal and 700 is bold.
#' @param kerning increases or decreases whitespace between letters
#' @param decoration value of decoration_types for example "underline"
#' @param strokecolor adds a stroke (border around the text)
#' @param boxcolor adds a background color

#'
#' @return stimlist with labelled images
#' @export
#'
#' @examples
#'
#' demo_stim("test") %>%
#'   label(c("CHINWE", "GEORGE"), color = "red") %>%
#'   plot()
label <- function(stimuli,
                  text = TRUE,
                  gravity = "north",
                  location = "+0+10",
                  degrees = 0,
                  size = NULL,
                  font = "sans",
                  style = "normal",
                  weight = 400,
                  kerning = 0,
                  decoration = NULL,
                  color = "black",
                  strokecolor = NULL,
                  boxcolor = NULL) {
  stimuli <- validate_stimlist(stimuli)

  if (isTRUE(text)) text <- names(stimuli)
  color <- sapply(color, color_conv)
  if (!is.null(strokecolor)) strokecolor <- sapply(strokecolor, color_conv)
  if (!is.null(boxcolor)) boxcolor <- sapply(boxcolor, color_conv)
  # scale size to image height
  if (is.null(size)) size <- height(stimuli)*0.05

  # allows for arguments to be vectors of any length
  ith <- function(v, i) {
    v[[(i-1)%%length(v)+1]]
  }

  for (i in seq_along(stimuli)) {
    stimuli[[i]]$img <- magick::image_annotate(
      stimuli[[i]]$img,
      ith(text, i),
      gravity = ith(gravity, i),
      location = ith(location, i),
      degrees = ith(degrees, i),
      size = ith(size, i),
      font = ith(font, i),
      style = ith(style, i),
      weight = ith(weight, i),
      kerning = ith(kerning, i),
      decoration = ith(decoration, i),
      color = ith(color, i),
      strokecolor = ith(strokecolor, i),
      boxcolor = ith(boxcolor, i)
    )
  }

  stimuli
}


