#' Manually delineate images
#'
#' Adjust the templates in a shiny interface. This will overwrite existing templates.
#'
#' @param stimuli list of class stimlist
#'
#' @return stimlist with new templates
#' @export
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
  stimuli <- validate_stimlist(stimuli)

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

