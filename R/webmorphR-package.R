#' webmorphR: Reproducible Face Stimuli
#'
#' Create reproducible image stimuli, specialised for images with webmorph.org templates.
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
    webmorph.plot.maxwidth = 2400,
    webmorph.plot.maxheight = 2400
  )
  toset <- !(names(op.webmorph) %in% names(op))
  if(any(toset)) options(op.webmorph[toset])
  
  register_s3_method("knitr", "knit_print", "stimlist")
  
  invisible()
}

register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg), length(pkg) == 1)
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)
  
  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }
  
  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }
  
  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
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
