
#' Label images
#' 
#' Defaults to [mlabel] unless you use arguments specific to [gglabel].
#'
#' @param stimuli list of class stimlist
#' @param ... arguments to pass on to \code{mlabel} or \code{gglabel}
#'
#' @return
#' @export
#'
#' @examples
#' stimuli <- demo_stim("test")
#' m_stimuli <- stimuli |>
#'   label(text = c("CHINWE", "GEORGE"), 
#'         gravity = c("north", "south"),
#'         color = "red")
#'         
#' gg_stimuli <- stimuli |>
#'   label(label = c("CHINWE", "GEORGE"), 
#'         x = 0.5, 
#'         y = c(1, 0.02),
#'         vjust = c(1, 0), 
#'         size = 10,
#'         color = "red")
#'         
#' # c(stimuli, gg_stimuli, m_stimuli) |> plot(nrow = 3)
label <- function(stimuli, ...) {
  args <- list(...) |> names()
  
  # list unique args
  magick_args <- c("text", "gravity", "location", "degrees", 
                   "font", "style", "weight", "kerning", 
                   "decoration", "strokecolor", "boxcolor")
  
  gg_args <- c("label", "x", "y", "geom", "hjust", "vjust", 
              "xintercept", "yintercept", "xmax", "xmin", "ymax", "ymin",
              "stat", "label.padding", "label.r", "label.size", "alpha",
              "family", "fontface", "angle")
  
  has_magic_args <- any(args %in% magick_args)
  has_gg_args <- any(args %in% gg_args)
  
  if (has_magic_args & has_gg_args) {
    stop("You're using arguments for both mlabel() and gglabel(). Fix this or use one of those functions.")
  } else if (has_magic_args) {
    mlabel(stimuli, ...)
  } else if (has_gg_args) {
    gglabel(stimuli, ...)
  } else { # default to mlabel
    mlabel(stimuli, ...)
  }
}


#' Label with magick annotations
#'
#' This is just a wrapper function for \code{magick::\link[magick]{image_annotate}} that allows more flexibility in color input. Setting a font, weight, style only works if your imagemagick is compiled with fontconfig support.
#'
#' @param stimuli list of class stimlist
#' @param text a vector of the label text(s) or TRUE to use stimlist names
#' @param color a vector of the label colour(s)
#' @param gravity string with gravity value from \code{magick::gravity_types}.
#' @param location geometry string with location relative to gravity
#' @param degrees rotates text around center point
#' @param size font size in pixels or proportion of image width (if < 1.0)
#' @param font string with font family such as "sans", "mono", "serif", "Times", "Helvetica", "Trebuchet", "Georgia", "Palatino" or "Comic Sans".
#' @param style value of style_types for example "italic"
#' @param weight thickness of the font, 400 is normal and 700 is bold.
#' @param kerning increases or decreases whitespace between letters
#' @param decoration value of decoration_types: "LineThrough" "None", "Overline", "Underline" 
#' @param strokecolor adds a stroke (border around the text)
#' @param boxcolor adds a background color
#' 
#' @seealso [gglabel()] for a labeller using syntax like \code{ggplot2::\link[ggplot2]{annotate}}
#' @return stimlist with labelled images
#' @export
#'
#' @examples
#' stimuli <- demo_stim("test")
#' labelled_stimuli <- stimuli |>
#'   mlabel(text = c("CHINWE", "GEORGE"), 
#'         gravity = c("north", "south"),
#'         color = "red")
mlabel <- function(stimuli,
                  text = TRUE,
                  gravity = "north",
                  location = "+0+0",
                  degrees = 0,
                  size = 0.1,
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
  
  tryCatch({
    color <- sapply(color, color_conv)
  }, error = function(e) {
    stop("Invalid color: ", e$message, call. = FALSE)
  })
  if (!is.null(strokecolor)) {
    tryCatch({
      strokecolor <- sapply(strokecolor, color_conv)
    }, error = function(e) {
      stop("Invalid strokecolor: ", e$message, call. = FALSE)
    })
  }
  
  if (!is.null(boxcolor)) {
    tryCatch({
      boxcolor <- sapply(boxcolor, color_conv)
    }, error = function(e) {
      stop("Invalid boxcolor: ", e$message, call. = FALSE)
    })
  }
  # font size
  if (!is.numeric(size)) {
    stop("size must be a number")
  } else if (any(size < 1.0)) {
    # sizes are proportions of image width
    size <- rep_len(size, length(stimuli)) * width(stimuli)
  }

  # allows for arguments to be vectors of any length
  ith <- function(v, i) {
    v[[(i-1)%%length(v)+1]]
  }

  for (i in seq_along(stimuli)) {
    tryCatch({
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
    }, error = function(e) {
      stop("Error in label(): ", e$message, call. = FALSE)
    })
  }

  stimuli
}


#' Label with ggplot annotations
#' 
#' Pass arguments to \code{ggplot2::\link[ggplot2]{annotate}} to label each image in a stimulus list.
#' 
#' @param stimuli list of class stimlist
#' @param label a vector of the label text(s) or TRUE to use stimlist names
#' @param x x-coordinate for label anchor (left is 0); values <= 1 are interpreted as proportions of width
#' @param y y-coordinate for label anchor (bottom is 0); values <= 1 are interpreted as proportions of height
#' @param geom the geom to use
#' @param ... further arguments to pass to \code{ggplot2::\link[ggplot2]{annotate}}
#'
#' @return stimlist with labelled images
#' @seealso [label()] for a labeller using syntax like \code{magick::\link[magick]{image_annotate}}
#' @export
#'
#' @examples
#' stimuli <- demo_stim()
#' 
#' # label with image names
#' labeled_stim <- gglabel(stimuli)
#' 
#' # add a watermark
#' watermark <- gglabel(
#'   stimuli,
#'   label = "watermark",
#'   x = 0.5, 
#'   y = 0.5,
#'   geom = "text",
#'   size = 20,
#'   color = "black",
#'   angle = -30,
#'   alpha = 0.5
#' )
gglabel <- function(stimuli, label = TRUE, x = 0.5, y = 0.95, geom = "text", ...) {
  stimuli <- validate_stimlist(stimuli)
  
  if (isTRUE(label)) label <- names(stimuli)
  dots <- list(...)
  dots$geom = geom
  dots$label = label
  dots$x = x
  dots$y = y
  
  # fix arguments in units
  if (!is.null(dots$label.padding)) {
    dots$label.padding <- list(dots$label.padding)
  }
  if (!is.null(dots$label.r)) {
    dots$label.r <- list(dots$label.r)
  }
  
  suppressWarnings({
    l <- length(stimuli)
    dots <- lapply(dots, rep, length.out = l)
  })
  
  # convert x and y to pixels
  w <- width(stimuli)
  h <- height(stimuli)
  dots$x <- ifelse(dots$x <= 1, dots$x*w, dots$x)
  dots$y <- ifelse(dots$y <= 1, dots$y*h, dots$y)
  
  for (i in seq_along(stimuli)) {
    args <- lapply(dots, `[[`, i)
    info <- magick::image_info(stimuli[[i]]$img)
    res <- gsub("x.*$", "", info$density) |> as.integer()
    
    # TODO: only suppress warnings that start with "Ignoring unknown"
    suppressWarnings({
      gg <- stimuli[[i]]$img |> 
        magick::image_ggplot() +
        do.call(ggplot2::annotate, args)
    })
    
    img <- magick::image_graph(width = w[i], 
                               height = h[i], 
                               bg = "white",
                               res = res,
                               clip = FALSE, # TODO: check if this makes a difference
                               antialias = TRUE)
    print(gg)
    grDevices::dev.off()
    stimuli[[i]]$img <- img
  }
  
  stimuli
}

