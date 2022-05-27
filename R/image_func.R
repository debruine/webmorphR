#' Apply a magick function to each image
#'
#' This is a convenience function for applying {magick} functions that take an image as the first argument and return an image. It's fully vectorised, so you can set separate argument values for each image.
#' 
#' @details 
#' These functions only affect the image, not the template. If a function changes the morphology of the image (e.g., "implode"), the template will not alter in the same way.
#'
#' @param stimuli list of stimuli
#' @param func the function or a string with the short name of the magick function (see [image_func_types()])
#' @param ... arguments to pass to the function
#'
#' @return list of stimuli with new images
#' @export
#' @family manipulators
#'
#' @examples
#' stimuli <- demo_stim()
#'
#' # make a photographic negative version
#' image_func(stimuli, "negate") |> 
#'   plot(maxwidth = 500)
#' 
#' # set different argument values for each image
#' image_func(stimuli, "implode", factor = c(0.2, -0.2)) |> 
#'   plot(maxwidth = 500)
#' 
#' \donttest{
#' # other image functions
#' image_func(stimuli, "blur", 5, 3) |> 
#'   plot(maxwidth = 500)
#' image_func(stimuli, "contrast", sharpen = 1) |> 
#'   plot(maxwidth = 500)
#' image_func(stimuli, "oilpaint", radius = 5) |> 
#'   plot(maxwidth = 500)
#' image_func(stimuli, "colorize", opacity = 50,
#'           color = c("hotpink", "dodgerblue")) |> 
#'   plot(maxwidth = 500)
#'
#' # load a logo image and superimpose it on each image
#' logo <- system.file("extdata/logo.png", package = "webmorphR") |>
#'   magick::image_read() |>
#'   magick::image_resize(100)
#' plus_logo <- image_func(stimuli, "composite", logo, 
#'                         offset = "+10+10")
#' plot(plus_logo, maxwidth = 500)
#'
#' # use a self-defined function
#' testfunc <- function(image) {
#'   image # just return the image unprocessed
#' }
#' test <- image_func(stimuli, testfunc)
#' }
image_func <- function(stimuli, func, ...) {
  stimuli <- as_stimlist(stimuli)

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
#' @family manipulators
#'
#' @examples 
#' stimuli <- demo_stim()
#' grey_stim <- greyscale(stimuli)
#' plot(grey_stim)
greyscale <- function(stimuli) {
  image_func(stimuli, "modulate", saturation = 0)
}

#' @rdname greyscale
#' @export
#' @family manipulators
#'
#' @examples
#' stimuli <- demo_stim()
#' gray_stim <- grayscale(stimuli)
#' plot(gray_stim)
grayscale <- greyscale

