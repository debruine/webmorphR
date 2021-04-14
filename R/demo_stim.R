#' Demo Stimuli
#'
#' A convenience function to get demo stimuli
#'
#' @param dir the directory in extdata to get files from
#' @param pattern defaults to all files
#' @param ... Other arguments to pass on to `read_tem()`
#'
#' @return stimlist
#' @export
#'
#' @examples
#' demo_stim() %>% plot()
#'
demo_stim <- function(dir = c("test", "composite", "london", "smiling", "lisa", "zoom", "rainbow"),
                     pattern = NULL, ...) {
  dir <- match.arg(dir)
  path <- system.file(file.path("extdata", dir), package = "webmorphR")
  
  # download missing images
  if (path == "") {
    message(dir, " is not yet installed. Downloading...")
    remote_zip_dir <- "https://raw.githubusercontent.com/debruine/webmorphR/master/data-raw/"
    url <- list(
      test = paste0(remote_zip_dir, "test.zip"),
      lisa = paste0(remote_zip_dir, "lisa.zip"),
      zoom = paste0(remote_zip_dir, "zoom.zip"),
      rainbow = paste0(remote_zip_dir, "rainbow.zip"),
      london = "https://ndownloader.figshare.com/files/8541961",
      smiling = "https://ndownloader.figshare.com/files/8541964",
      composite = "https://ndownloader.figshare.com/articles/4055130/versions/1"
    )
    basedir <- system.file("extdata", package = "webmorphR")
    newdir <- file.path(basedir, dir)
    ziptmp <- file.path(tempdir(), "zip.zip")
    download.file(url[[dir]], ziptmp)
    utils::unzip(ziptmp, exdir = newdir, junkpaths = TRUE)
    
    path <- system.file(file.path("extdata", dir), package = "webmorphR")
    message("...Installed")
  }

  stimuli <- read_stim(path, pattern, ...)

  stimuli
}
