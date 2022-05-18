#' webmorphR: Reproducible Face Stimuli
#'
#' Create reproducible image stimuli, specialised for images with webmorph.org templates. See https://debruine.github.io/webmorphR/ for detailed tutorials. 
#'
#' @docType package
#' @name webmorphR
#' @keywords internal
#' 
#' @importFrom stats cor sd
#' @importFrom dplyr .data
#' @importFrom rsvg rsvg_raw
#'
"_PACKAGE"


.onLoad <- function(libname, pkgname) {
  ## set default options for wm_opts:
  op <- options()
  op.webmorph <- list(
    webmorph.overwrite = "ask",
    webmorph.connection = stdin(),
    webmorph.verbose = TRUE,
    webmorph.line.color = "blue",
    webmorph.pt.color = "green",
    webmorph.fill = "white",
    webmorph.server = "https://webmorph.org",
    webmorph.plot = "inline",
    webmorph.plot.maxwidth = 10000,
    webmorph.plot.maxheight = 10000
  )
  toset <- !(names(op.webmorph) %in% names(op))
  if(any(toset)) options(op.webmorph[toset])
  
  invisible()
}

.onAttach <- function(libname, pkgname) {
  paste(
    "\n************",
    "Welcome to webmorphR. For support and examples visit:",
    "https://debruine.github.io/webmorphR/",
    # "If this package is useful, please cite it:",
    # paste0("http://doi.org/", utils::citation("webmorphR")$doi),
    "************\n",
    sep = "\n"
  ) |> packageStartupMessage()
}
