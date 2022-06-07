DEBUG <- FALSE # show debug messages in console

# display debugging messages in R if local,
# or in the console log if remote
debug_msg <- function(...) {
  is_local <- Sys.getenv('SHINY_PORT') == ""
  txt <- paste(...)
  if (is_local & DEBUG) {
    message(txt)
  } else {
    shinyjs::logjs(txt)
  }
}

## datatable constants ----
dt_options <- function() {
  list(
    info = FALSE,
    lengthChange = FALSE,
    paging = FALSE,
    ordering = FALSE,
    searching = FALSE,
    pageLength = 500,
    keys = TRUE
  )
}