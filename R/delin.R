#' Manually delineate images
#'
#' Adjust the templates in a shiny interface. This will overwrite existing templates.
#'
#' @param stimuli list of stimuli
#'
#' @return list of stimuli with new templates
#' @export
#' @family tem
#' 
#' @examples 
#' \dontrun{
#' # adjust existing delineations
#' stimuli <- demo_stim() |> delin()
#' 
#' # create new delineations from scratch
#' stimuli <- demo_stim() |> remove_tems() |> delin()
#' }
delin <- function(stimuli) {
  # check for required packages in shiny app
  req_packages <- c("shiny", "shinyjs", "shinydashboard", "shinyWidgets", "DT")
  pkg_available <- sapply(req_packages, requireNamespace, quietly = TRUE)
  
  if (!all(pkg_available)) {
    pkg_txt <- pkg_available[pkg_available == FALSE] |> paste(collapse = ", ")
    stop("You need to install the following packages to use the shiny delineator: ",
         pkg_txt)
  }
  
  stimuli <- as_stimlist(stimuli)

  # save images to temp dir
  imgdir <- tempfile()
  write_stim(stimuli, imgdir)

  # start shiny app to delineate
  message("Running shiny app...")
  shiny::shinyOptions(imgdir = imgdir)
  shiny::runApp(appDir = system.file("app", package = "webmorphR"))

  # return contents of temp dir
  read_stim(imgdir)
}

