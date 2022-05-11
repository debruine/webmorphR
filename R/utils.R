#' Print stim
#'
#' @param x a list of class stim
#' @param ... arguments passed to or from other methods
#'
#' @return prints summary info and returns x
#' @export
#'
print.stim <- function(x, ...) {
  # attr <- list()
  # if ("points" %in% names(x)) {
  #   attr$points <- sprintf("%i points", ncol(x$points))
  # }
  # if ("lines" %in% names(x)) {
  #   attr$lines <- sprintf("%i lines", length(x$lines))
  # }
  #
  # if ("img" %in% names(x)) {
  #   info <- tryCatch({
  #     magick::image_info(x$img)
  #   }, error = function(e) {
  #     x$img <- magick::image_read(x$imgpath)
  #     magick::image_info(x$img)
  #   })
  #   attr$img <- sprintf("%i x %i %s",
  #                       info$width,
  #                       info$height,
  #                       info$format)
  # }
  #
  # paste(attr, collapse = ", ") %>% cat()
  #
  # invisible(x)
  print(x$img, FALSE)
}

#' Print stimlist
#'
#' @param x a list of class stimlist
#' @param ... arguments passed to or from other methods
#'
#' @return prints summary info and returns x
#' @export
#'
print.stimlist <- function(x, ...) {
  # mapply(function(xi, nm) {
  #   sprintf("* %s: ", nm) %>% cat()
  #   print(xi, ...)
  #   cat("\n")
  # }, x, names(x) %||% seq_along(x)) # in case names are null

  img <- get_imgs(x)

  # print image inline if option set and not knitting
  # TODO: only run this if in an Rmd chunk
  #rmd_inline <- rstudioapi::readRStudioPreference("rmd_chunk_output_inline", NA)
  if (length(img) == 1 &&
      interactive() &&
      wm_opts("plot") == "inline" &&
      #rmd_inline &&
      !isTRUE(getOption("knitr.in.progress"))) {
    tmp <- tempfile(fileext = ".png")
    magick::image_write(img, tmp)
    suppressWarnings({
      # suppress warning about absolute paths
      knitr::include_graphics(tmp) %>% print()
    })
  }
  
  # prints in the viewer if not in an Rmd document
  print(img, FALSE)
}

#' Subset Stimulus Lists
#'
#' Returns a subset of the stimulus list meeting the condition.
#'
#' @param x a list of class stimlist
#' @param subset a character string to use as a pattern for searching stimulus IDs, or a logical expression indicating elements or rows to keep: missing values are taken as false.
#' @param ... further arguments to be passed to or from other methods.
#'
#' @return a list of class stimlist
#' @export
#'
#' @examples
#' f <- demo_stim() %>% subset("f_")
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
#'
#' @examples
#' a <- demo_stim()
#' rep(a[[1]], 3)
rep.stim <- function (x, ...) {
  # turn into a list and handle below
  x <- validate_stimlist(x)
  rep.stimlist(x, ...)
}

#' Repeat stim in a list
#'
#' @param x A stimlist
#' @param ... Additional arguments to pass on to `base::rep()`
#'
#' @return A stimlist
#' @export
#'
#' @examples
#' demo_stim() %>%
#'   rep(3) %>%
#'   rotate(seq(10, 60, 10), fill = rainbow(6)) %>%
#'   plot()
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
#'
c.stim <- function(...) {
  # turn into a stimlist and handle below
  dots <- lapply(list(...), validate_stimlist)
  do.call("c", dots)
}


#' Combine stimlists
#'
#' @param ... stimlists to be concatenated
#'
#' @return stimlist
#' @export
#'
c.stimlist <- function(...) {
  dots <- lapply(list(...), validate_stimlist) %>%
    lapply(unclass) # prevent infinite recursion
  x <- do.call("c", dots)
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


#' Get named item when unsure of name
#'
#' If a user gives you a named vector or list and there are a few possibilities for the item you want to get, list the possible names in order, using integers for indices.
#'
#' @param x named vector or list
#' @param ... possible names in x in priority order
#' @param .default default value if no names found
#'
#' @return list or vector item
#' @keywords internal
#'
#' @examples
#' x <- c(h = 100, w = 200)
#'
#' width <- xget(x, "width", "w", 1)
#' height <- xget(x, "height", "h", 2)
xget <- function(x, ..., .default = NULL) {
  list(...) %>%           # get possible names
    lapply(function(y) {  # retrieve from x
      z <- x[y][[1]]      # missing from vectors = NA, from list = NULL
      if (is.null(z)) NA else z
    }) %>%
    c(list(.default)) %>% # add default to the end
    `[`(., !is.na(.)) %>% # get rid of NAs
    `[[`(1)               # return first item
}


#' Format file size
#'
#' @param x the file size in bytes
#'
#' @return human-readable file size
#' @export
#'
#' @examples
#' format_size(1024*1024)
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
#' @param stimuli list of class stimlist
#'
#' @return list of magick images
#' @export
#'
#' @examples
#' demo_stim() %>% get_imgs
get_imgs <- function(stimuli) {
  validate_stimlist(stimuli) %>%
    lapply(`[[`, "img") %>%
    do.call(c, .)
}
