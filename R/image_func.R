#' Apply function to each image
#'
#' This is a convenience function for applying {magick} functions that take an image as the first argument and return an image. It's fully vectorised, so you can set separate argument values for each image.
#'
#' @param stimuli list of class stimlist
#' @param func the function or a string with the short name of the magick function (e.g., "blur" for \code{image_blur()}
#' @param ... arguments to pass to the function
#'
#' @return stimlist with new images
#' @export
#'
#' @examples
#' stimuli <- demo_stim()
#'
#' # use magick::image_* functions
#' image_func(stimuli, "fill", 
#'            color = "black", 
#'            fuzz = 10, 
#'            point = "+0+10")
#'
#' blur <- image_func(stimuli, "blur", 5, 3)
#' oilpaint <- image_func(stimuli, "oilpaint", radius = 5)
#' negate <- image_func(stimuli, "negate")
#' greenscreen <- image_func(stimuli, "transparent", 
#'                           color = "green", fuzz = 5)
#' colorize <- image_func(stimuli, "colorize", opacity = 50,
#'                        color = c("hotpink", "dodgerblue"))
#' sharpen <- image_func(stimuli, "contrast", sharpen = 1)
#'
#' # load a logo image and superimpose it on each image
#' logo <- system.file("extdata/logo.png", package = "webmorphR") |>
#'   magick::image_read() |>
#'   magick::image_resize(100)
#' image_func(stimuli, "composite", logo, offset = "+10+10")
#'
#' # use a self-defined function
#' testfunc <- function(image) {
#'   image # just return the image unprocessed
#' }
#' test <- image_func(stimuli, testfunc)
#'
image_func <- function(stimuli, func, ...) {
  stimuli <- validate_stimlist(stimuli)

  # make sure func is a function or a magick image function
  if (is.character(func)) {
    if (!func %in% image_func_types()) {
      stop("That named function is not possible. See image_func_type() for a full list")
    }

    func <- parse(text = paste0("magick::image_", func)) |>
      eval()
  }

  if (!is.function(func)) {
    stop("func must be a function or the short name of an image function in the magick package (e.g., \"blur\" for the function `image_blur`")
  }

  # if an argument has the same length as the stimuli
  # match argument to stimuli, otherwise pass to the function unaltered
  n <- length(stimuli)
  dots <- lapply(list(...), function(x) {
    if (length(x) == n) {
      rep_len(x, n)
    } else {
      rep_len(list(x), n)
    }
  })

  for (i in seq_along(stimuli)) {
    subdots <- lapply(dots, `[[`, i)
    args <- c(list(stimuli[[i]]$img), subdots)
    stimuli[[i]]$img <- do.call(func, args)
  }

  stimuli
}


#' Possible functions
#'
#' \code{\link{image_func}} can take a named function from the magick package, but only functions that return an image that is compatible with the current template (e.g., doesn't change size or shape).
#'
#' @return list of compatible function names
#' @export
#'
#' @examples
#' image_func_types()
image_func_types <- function() {
  c("annotate", "apply", "average", "background", "blur",
    "canny", "channel", "charcoal", "colorize", "combine",
    "composite", "contrast", "convert", "convolve", "despeckle",
    "edge", "emboss", "enhance", "equalize", "fill", "flatten",
    "fuzzycmeans", "fx", "fx_sequence", "implode", "lat", "level",
    "map", "median", "modulate", "morphology", "motion_blur",
    "negate", "noise", "normalize", "oilpaint", "ordered_dither",
    "page", "quantize", "reducenoise", "repage", "separate",
    "set_defines", "shade", "strip", "threshold", "transparent")
}


#' Make images greyscale
#'
#' @param stimuli list of class stimuli
#'
#' @return stimlist with new images
#' @export
#'
#' @examples 
#' demo_stim() |> greyscale()
greyscale <- function(stimuli) {
  image_func(stimuli, "modulate", saturation = 0)
}

#' @rdname greyscale
#' @export
#'
#' @examples
#' demo_stim() |> grayscale()
grayscale <- greyscale

