#' Print stim
#'
#' @param x a list of class stim
#' @param ... arguments passed to or from other methods
#'
#' @return prints summary info and returns x
#' @export
#' @keywords internal
#'
print.stim <- function(x, ...) {
  as_stimlist(x) |> print()
}
# print.stim <- function(x, ...) {
#   # uses magick:::print.magick-image
#   print(x$img, FALSE)
# }

#' Print stimlist
#'
#' @param x list of stimuli
#' @param ... arguments passed to or from other methods
#'
#' @return prints summary info and returns x
#' @export
#' @keywords internal
#'
print.stimlist <- function(x, ...) {
  if (!length(x)) return(invisible(x))
  
  if (length(x) == 1) {
    if (length(x[[1]]$img) > 1) {
      # animated gif
      print(x[[1]]$img, info = FALSE)
      return(invisible(x))
    }
  } else {
    x <- plot(x)
  }
  
  grid::grid.newpage()
  grid::grid.raster(x[[1]]$img)
                    # width = x[[1]]$width, 
                    # height = x[[1]]$height,
                    # default.units = "points")
  
  invisible(x)
}

# This is registered as an S3 method in .onLoad()
knit_print.stimlist <- function(x, ...) {
  if (!length(x)) return(invisible())
  
  if (length(x) == 1) {
    print(x[[1]]$img, info = FALSE)
  } else {
    plot(x) |> get_imgs() |> print(info = FALSE)
  }
}
# print.stimlist <- function(x, ...) {
#   img <- get_imgs(x)
# 
#   # print image inline if option set and not knitting
#   # TODO: only run this if in an Rmd chunk
#   #rmd_inline <- rstudioapi::readRStudioPreference("rmd_chunk_output_inline", NA)
#   if (length(img) == 1 &&
#       interactive() &&
#       wm_opts("plot") == "inline" &&
#       #rmd_inline &&
#       !isTRUE(getOption("knitr.in.progress"))) {
#     tmp <- tempfile(fileext = ".png")
#     magick::image_write(img, tmp)
#     suppressWarnings({
#       # suppress warning about absolute paths
#       knitr::include_graphics(tmp) |> print()
#     })
#   }
#   
#   # prints in the viewer using magick:::print.magick-image
#   # if multiple images or not in an Rmd document
#   suppressWarnings({
#     # suppress warning about absolute paths
#     print(img, FALSE)
#   })
# }

#' Subset Stimulus Lists
#'
#' Returns a subset of the stimulus list meeting the condition.
#'
#' @param x list of stimuli
#' @param subset a character string to use as a pattern for searching stimulus IDs, or a logical expression indicating elements or rows to keep: missing values are taken as false.
#' @param ... further arguments to be passed to or from other methods.
#'
#' @return list of stimuli
#' @export
#' @keywords internal
subset.stimlist <- function (x, subset, ...) {
  e <- substitute(subset)
  info <- get_info(x)
  r <- eval(e, info, parent.frame())

  if (is.logical(r)) {
    selected <- r & !is.na(r)
  } else if (is.numeric(subset)) {
    selected <- subset
  } else {
    selected <- grepl(subset, names(x))
  }

  x[selected]
}

#' Repeat stim in a list
#'
#' @param x A list of class stim
#' @param ... Additional arguments to pass on to `base::rep()`
#'
#' @return A stimlist
#' @export
#' @keywords internal
rep.stim <- function (x, ...) {
  # turn into a list and handle below
  x <- as_stimlist(x)
  rep.stimlist(x, ...)
}

#' Repeat stim in a list
#'
#' @param x A stimlist
#' @param ... Additional arguments to pass on to `base::rep()`
#'
#' @return A stimlist
#' @export
#' @keywords internal
rep.stimlist <- function(x, ...) {
  nm <- names(x)
  newnm <- rep(nm, ...)
  newx <- x[newnm]
  class(newx) <- c("stimlist", "list")
  newx
}

#' Combine stim
#'
#' @param ... stim to be concatenated
#'
#' @return stimlist
#' @export
#' @keywords internal
#'
c.stim <- function(...) {
  # turn into a stimlist and handle below
  list(...) |>
    lapply(as_stimlist) |>
    do.call(what = c.stimlist)
}


#' Combine stimlists
#'
#' @param ... stimlists to be concatenated
#'
#' @return stimlist
#' @export
#' @keywords internal
#'
c.stimlist <- function(...) {
  dots <- lapply(list(...), as_stimlist) |>
    lapply(unclass) # prevent infinite recursion
  x <- do.call(c, dots)
  class(x) <- c("stimlist", "list")
  x
}


#' Extract stimlist elements
#'
#' @param x stimlist from which to extract elements
#' @param i indices to be selected
#'
#' @return stimlist
#' @export
#' @keywords internal
#'
`[.stimlist` <- function(x, i) {
  x <- NextMethod()
  class(x) <- c("stimlist", "list")
  x
}

#' Replace stimlist element
#'
#' @param x stimlist from which to extract elements
#' @param i index to be replaced
#' @param value stim element to replace with
#'
#' @return stimlist
#' @export
#' @keywords internal
#'
`[[<-.stimlist` <- function(x, i, value) {
  stopifnot("stim" %in% class(value))
  NextMethod()
}

#' WebmorphR Message
#'
#' @param ... arguments to pass to base::message()
#'
#' @return NULL
#' @keywords internal
message <- function(...) {
  if (isTRUE(wm_opts("verbose"))) {
    base::message(...)
  }
}



#' Format file size
#'
#' @param x the file size in bytes
#'
#' @return human-readable file size
#' @keywords internal
format_size <- function (x) {
  digits = 1L
  base <- 1024
  units_map <- c("b", "Kb", "Mb", "Gb", "Tb", "Pb")
  power <- if (x <= 0) 0L else min(as.integer(log(x, base = base)), length(units_map) - 1L)
  unit <- units_map[power + 1L]
  if (power == 0) unit <- "bytes"
  paste(round(x/base^power, digits = digits), unit)
}

#' Get Images into List
#'
#' @param stimuli list of stimuli
#'
#' @return list of magick images
#' @keywords internal
get_imgs <- function(stimuli) {
  args <- as_stimlist(stimuli) |>
    lapply(`[[`, "img")
  
  do.call(c, args)
}
