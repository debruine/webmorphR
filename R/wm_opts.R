#' Set/get global webmorph options
#' 
#' See [wm_opts_defaults()] for explanations of the default options.
#'
#' @param ... One of four: (1) nothing, then returns all options as a list; (2) a name of an option element, then returns its value; (3) a name-value pair which sets the corresponding option to the new value (and returns nothing), (4) a list with option-value pairs which sets all the corresponding arguments.
#'
#' @return a list of options, values of an option, or nothing
#' @export
#' 
#' @seealso [wm_opts_defaults()]
#'
#' @examples
#'
#' wm_opts() # see all options
#'
#' wm_opts("verbose") # see value of webmorph.verbose
#' 
#' \dontrun{
#' # set value of webmorph.verbose
#' wm_opts(verbose = FALSE) 
#' 
#' # set multiple options
#' opts <- list(fill = "black",
#'              pt.color = "white", 
#'              line.color = "red")
#' wm_opts(opts)
#' }
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

#' WebmorphR default options
#' 
#' @description
#' Options set on load (unless they were already set by .Renviron)
#' 
#' * overwrite ("ask"): Whether to overwrite images saved with [write_stim()] when in interactive mode; possible values are "ask" (ask if filenames exist), TRUE (always overwrite), and FALSE (never overwrite)
#' * fill ("white"): the colour to use to fill image backgrounds
#' * pt.color ("green") : the colour to use for points in [draw_tem()]
#' * line.color ("blue"): the colour to use for lines in [draw_tem()]
#' * plot ("inline"): whether to plot images inline in R markdown documents (set to any other value to just view them in the viewer)
#' * plot.maxwidth (2400): The maximum width of images created by [plot()]
#' * plot.maxheight (2400): The maximum height of images created by [plot()]
#' * verbose (TRUE): Whether to produce verbose output and progress bars for long functions like [auto_delin()], [avg()] or [trans()]
#' * server ("https://webmorph.org"): The server to use for webmorph functions like [avg()] and [trans()]; do not change unless you've set up a local server
#' * connection (stdin()): use internally for testing interactive functions; do not change
#'
#' @return a list of default options 
#' @export
#' 
#' @seealso [wm_opts()]
#'
#' @examples
#' wm_opts_defaults() |> str() # view defaults
#' 
#' \dontrun{
#' # reset all options to default
#' wm_opts_defaults() |> wm_opts()
#' }
wm_opts_defaults <- function() {
  list(
    connection = stdin(),
    fill = "white",
    line.color = "blue",
    overwrite = "ask",
    plot = "inline",
    plot.maxheight = 2400,
    plot.maxwidth = 2400,
    pt.color = "green",
    server = "https://webmorph.org",
    verbose = TRUE
  )
}
