#' Set/get global webmorph options
#'
#' @param ... One of four: (1) nothing, then returns all options as a list; (2) a name of an option element, then returns its value; (3) a name-value pair which sets the corresponding option to the new value (and returns nothing), (4) a list with option-value pairs which sets all the corresponding arguments.
#'
#' @return a list of options, values of an option, or nothing
#' @export
#'
#' @examples
#'
#' wm_opts() # see all options
#'
#' wm_opts("verbose") # see value of webmorph.verbose
#'
wm_opts <- function (...) {
  # code from afex::afex_options
  dots <- list(...)
  if (length(dots) == 0) {
    # get all webmorph options
    op <- options()
    webmorph_op <- op[grepl("^webmorph.", names(op))]
    names(webmorph_op) <- sub("^webmorph.", "", names(webmorph_op))
    return(webmorph_op)
  } else if (is.list(dots[[1]])) {
    # first item is a list, set from list if named
    newop <- dots[[1]]
    if (is.null(names(newop)))
      stop("Format lists with names like list(verbose = FALSE)")
    names(newop) <- paste0("webmorph.", names(newop))
    options(newop)
  } else if (!is.null(names(dots))) {
    # dots have names, so set webmorph options
    newop <- dots
    names(newop) <- paste0("webmorph.", names(newop))
    options(newop)
  } else if (is.null(names(dots))) {
    # dots don't have names, so get webmorph options
    opnames <- paste0("webmorph.", unlist(dots))
    getop <- lapply(opnames, getOption)
    if (length(opnames) == 1) {
      getop <- getop[[1]]
    } else {
      names(getop) <- unlist(dots)
    }
    return(getop)
  } else {
    warning("Unsupported command to wm_opts(), nothing done.",
            call. = FALSE)
  }
}
