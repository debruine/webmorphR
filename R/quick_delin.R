#' Quickly delineate images
#'
#' Best used for just a few points, like delineating the eyes, or top, right, bottom and left boundaries. This will overwrite existing templates.
#'
#' @param stimuli list of class stimlist
#' @param n_points Number of points after which the template automatically saves and moves on to the next image.
#'
#' @return stimlist with new templates
#' @export
#'
quick_delin <- function(stimuli, n_points = 0) {
  if (!is.numeric(n_points) || n_points < 0) {
    stop("n_points must be an integer >= 0")
  }
  stimuli <- validate_stimlist(stimuli) |>
    remove_tem()

  # save images to temp dir
  imgdir <- tempfile()
  write_stim(stimuli, imgdir, format = "jpg")

  # start shiny app to delineate
  message("Running shiny app...")
  shiny::shinyOptions(imgdir = imgdir)
  shiny::shinyOptions(n_points = n_points)
  shiny::runApp(appDir = system.file("app", package = "webmorphR"))

  # return contents of temp dir
  read_stim(imgdir)
}

